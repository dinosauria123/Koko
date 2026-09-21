#!/usr/bin/env python3
"""Behavior-preserving splitter for gui_py/mainwindow.py.

Extracts the god-object KokoMainWindow into responsibility mixins in separate
modules, and moves standalone dialogs / plot-view windows into helper modules.
All method/class bodies are copied VERBATIM (AST segments of the original
source) so runtime behaviour cannot change. Only *placement* changes.

NOTE: this generator reads the *un-split* god-object `gui_py/mainwindow.py`
as its input. It must be run before the split is committed (i.e. while
`gui_py/mainwindow.py` still contains the full KokoMainWindow + all dialog
and plot-view class definitions). After the split is committed, `mainwindow.py`
only holds the controller mixin and re-running will not regenerate correctly.
"""
import ast
import os

SRC = os.path.join(os.path.dirname(__file__), 'gui_py', 'mainwindow.py')
OUT = os.path.join(os.path.dirname(__file__), 'gui_py_stage')
os.makedirs(OUT, exist_ok=True)

src = open(SRC).read()
lines = src.splitlines(keepends=True)
tree = ast.parse(src)

km = [n for n in tree.body if isinstance(n, ast.ClassDef) and n.name == 'KokoMainWindow'][0]
km_methods = [n for n in km.body if isinstance(n, ast.FunctionDef)]
km_by_name = {}
for m in km_methods:
    km_by_name.setdefault(m.name, m)   # first wins (no dups expected in this class)

# ---- responsibility mapping -------------------------------------------------
PROCESS = {  # koko-cli process lifecycle / command in-out / RTG / msg / history
    'find_koko_cli', '_launch_koko_process', 'start_koko_cli', '_reap_koko',
    '_kill_koko', 'send_koko', 'execute_command', '_poll_koko_pty',
    '_capture_rtg', '_check_prompt_idle', '_parse_rtg_meta',
    '_parse_rtg_surface_details', '_on_rtg_complete', 'on_finished', 'on_error',
    'append_msg',
}
TABLE = {  # lens table / glass / header / context menu
    '_on_cell_changed', '_send_table_current_cell', '_set_cell', '_surface_type_str',
    '_build_rtg_rows', 'populate_table', '_refresh_radius_display',
    '_on_radius_curvature_changed', '_build_header_row', '_sync_header_geometry',
    '_on_material_cell_double_clicked', '_highlight_rows', '_show_surface_panel',
    'slot_lensInfo', '_load_glass_catalogs', '_calc_glass_nv', '_read_glass_data',
    'slot_show_context_menu', '_ctx_insert_surface', '_ctx_delete_surface',
    '_ctx_material', '_send_surface_cmd',
}
MENUS = {'_wire_menus'}
PLOTS = {  # render / windows / banner / image
    'slot_text', 'slot_plot', '_is_point_plot', '_schedule_plot_render',
    '_poll_plot_render', 'slot_export', 'show_startup_banner', '_center_banner',
    '_synthesize_psf_plot_lines', 'render_plots', '_render_plots_inner',
    'show_plot', '_on_plot_window_closed', 'slot_quit2', '_wait_for_psf_gri',
    '_schedule_image_render', '_poll_image_render', '_show_image_result',
}

# top-level classes: dialogs vs plot-view windows
DIALOG_CLASSES = set()
PLOT_WINDOW_CLASSES = {'BannerWindow', 'PlotWindow', '_WinMouseEvent',
                       'GlassMapWindow', 'GlassMapDialog'}
for n in tree.body:
    if isinstance(n, ast.ClassDef) and n.name not in ('KokoMainWindow',) \
            and n.name not in PLOT_WINDOW_CLASSES:
        DIALOG_CLASSES.add(n.name)

# ui_* class names referenced from the module (needed by common + windows)
UI_NAMES = []
UI_IMPORT_NODES = []
for n in tree.body:
    if isinstance(n, ast.ImportFrom) and (n.module or '').startswith('gui_py.ui_'):
        UI_IMPORT_NODES.append(n)              # collect; build UI_IMPORTS after segment()
        for a in n.names:
            UI_NAMES.append(a.asname or a.name)

# stdlib + PyQt import block (verbatim), everything before PLOT_TRIGGER_PREFIXES
import_end = None
for n in tree.body:
    if isinstance(n, ast.Assign) and isinstance(n.targets[0], ast.Name) \
            and n.targets[0].id == 'PLOT_TRIGGER_PREFIXES':
        import_end = n.lineno - 1
        break
# The stdlib + PyQt import block (verbatim), everything before the first
# PLOT_TRIGGER_PREFIXES assignment, but WITHOUT the module docstring.
import_start = next(i for i, ln in enumerate(lines) if ln.strip().startswith('import os'))
IMPORTS = ''.join(lines[import_start:import_end])

COMMON_CONSTS = ['PLOT_TRIGGER_PREFIXES', 'CenterComboDelegate']


def segment(node):
    """Verbatim source slice for `node` (lines + absolute indent preserved)."""
    if node.lineno is None or node.end_lineno is None:
        raise SystemExit("no line info for %r" % getattr(node, 'name', node))
    seg = ''.join(lines[node.lineno - 1:node.end_lineno])
    return seg.rstrip('\n')


UI_IMPORTS = [segment(n) for n in UI_IMPORT_NODES]


def common_import_names(names):
    """Build `from gui_py._koko_gui_common import (...)` for `names`."""
    names = sorted(set(names))
    if not names:
        return ''
    block = 'from gui_py._koko_gui_common import (\n'
    line = '    '
    for nm in names:
        tok = nm + ', '
        if line == '    ' and len(line + tok) <= 76:
            line += tok
            continue
        if len(line) + len(tok) > 76:
            block += line.rstrip(', ') + ',\n'
            line = '    ' + tok
        else:
            line += tok
    block += line.rstrip(', ') + '\n)'
    return block


def collect_bare_names(nodes):
    used = set()
    for nd in (nodes if isinstance(nodes, (list, tuple)) else [nodes]):
        for sub in ast.walk(nd):
            if isinstance(sub, ast.Name):
                used.add(sub.id)
    return used


def build_mixin(module, class_name, method_names, extra_common=(),
                exclude_common=()):
    segs = []
    used_names = set()
    for nm in method_names:
        m = km_by_name[nm]
        segs.append(segment(m))
        used_names |= collect_bare_names(m.body)
        used_names |= collect_bare_names(m.args.args + m.args.posonlyargs +
                                         m.args.kwonlyargs)
        used_names.add(m.name)
    # common names referenced (that live in common). Optionally exclude some
    # (e.g. plot windows that live IN the same module) so the mixin does not
    # self-import names defined later in the same file.
    common_set = (DIALOG_CLASSES | set(UI_NAMES) | set(COMMON_CONSTS) |
                  set(extra_common))
    needed_common = sorted((used_names & common_set) - set(exclude_common))
    header = ('"""%s: %s mixin (see gui_py/mainwindow.py).\n"""\n' %
              (class_name, module)) + IMPORTS + '\n\n' + \
             common_import_names(needed_common) + '\n'
    body = '\n\n\n'.join(segs)
    return header, 'class %s:\n%s\n' % (class_name, body)


def build_common():
    segs = [segment(n) for n in tree.body
            if isinstance(n, ast.Assign) and isinstance(n.targets[0], ast.Name)
            and n.targets[0].id == 'PLOT_TRIGGER_PREFIXES']
    segs += [segment(n) for n in tree.body
             if isinstance(n, ast.ClassDef) and n.name == 'CenterComboDelegate']
    # dialog classes in ORIGINAL source order (some subclass earlier ones,
    # e.g. LIDialog(StringDialog)); sorting alphabetically would break it.
    cls_nodes = [n for n in tree.body
                 if isinstance(n, ast.ClassDef) and n.name in DIALOG_CLASSES]
    cls_nodes.sort(key=lambda n: n.lineno)
    for n in cls_nodes:
        segs.append(segment(n))
    header = ('"""Shared GUI data / dialogs for the Koko GUI split.\n"""\n')
    header += IMPORTS
    # ALL gui_py.ui_* module imports (verbatim). Every dialog class (and the
    # controller) subclasses one of these Ui_* widgets, so they must all be
    # importable here, not just Ui_MainWindow.
    if UI_IMPORTS:
        header += '\n\n' + '\n'.join(UI_IMPORTS) + '\n\n\n'
    header += '# Shared GUI helpers / data / dialogs for the Koko GUI split.\n'
    return header, '\n\n'.join(segs) + '\n'


# ---- plot view windows ------------------------------------------------------
def build_plot_windows():
    # plot-view classes in ORIGINAL source order (GlassMapWindow subclasses
    # PlotWindow), so sorting would break subclassing.
    pw_nodes = [n for n in tree.body
                if isinstance(n, ast.ClassDef) and n.name in PLOT_WINDOW_CLASSES]
    pw_nodes.sort(key=lambda n: n.lineno)
    segs = []
    used = set()
    for node in pw_nodes:
        segs.append(segment(node))
        for item in node.body:
            used |= collect_bare_names(item)
    needed_common = sorted(used & (DIALOG_CLASSES | set(UI_NAMES) | set(COMMON_CONSTS)))
    header = ('"""Plot viewer windows (banner / plot image / glass map).\n"""\n') + IMPORTS + '\n\n' + \
             common_import_names(needed_common) + '\n'
    header += "# Plot viewer windows (banner / plot image / glass map).\n"
    body = '\n\n'.join(segs) + '\n'
    return header, body


# ---- assemble each mixin ----------------------------------------------------
proc_h, proc_b = build_mixin('koko_process.py', 'KokoProcessMixin',
                             sorted(PROCESS))
tab_h, tab_b = build_mixin('koko_table.py', 'KokoTableMixin', sorted(TABLE))
men_h, men_b = build_mixin('koko_menus.py', 'KokoMenuMixin', sorted(MENUS))
plot_h, plot_b = build_mixin('koko_plots.py', 'KokoPlotMixin', sorted(PLOTS),
                             exclude_common=PLOT_WINDOW_CLASSES)
ph, pw = build_plot_windows()
ch, cb = build_common()

# ---- new mainwindow.py (controller keeps: __init__/closeEvent/eventFilter +
#      all slot_action*/IO; imports mixins + dialog classes) ------------------
controller = []
for m in km_methods:
    if isinstance(m, ast.FunctionDef) and m.name not in (PROCESS | TABLE | MENUS | PLOTS):
        controller.append(m.name)

ctrl_segs = []
ctrl_used = set()
for nm in controller:
    m = km_by_name[nm]
    ctrl_segs.append(segment(m))
    ctrl_used |= collect_bare_names(m.body)
    ctrl_used |= collect_bare_names(m.args.args + m.args.posonlyargs + m.args.kwonlyargs)
    ctrl_used.add(m.name)
needed_common = sorted(ctrl_used & (DIALOG_CLASSES | set(UI_NAMES) |
                                     set(PLOT_WINDOW_CLASSES)))
# base class + any plot windows referenced from controller methods must be
# importable in the controller module itself.
needed_common.append('Ui_MainWindow')
dlg_imp = sorted(n for n in needed_common if n in DIALOG_CLASSES)
plot_imp = sorted(n for n in needed_common if n in PLOT_WINDOW_CLASSES)
# dialog/class names that must come from the shared-common module
common_import_lines = '\n'.join('    ' + n for n in dlg_imp)
plot_import_lines = '\n'.join('    ' + n for n in plot_imp)

mainwin_src = (
    '"""KokoMainWindow controller: command dispatch + lens IO.\n'
    '   Method bodies live in gui_py/koko_*.py mixins.\n'
    '"""\n'
    + IMPORTS
    + '\n\n'
    + 'from gui_py.koko_process import KokoProcessMixin\n'
    + 'from gui_py.koko_table import KokoTableMixin\n'
    + 'from gui_py.koko_menus import KokoMenuMixin\n'
    + 'from gui_py.koko_plots import KokoPlotMixin\n'
    + '\n'
    + 'from gui_py.ui_mainwindow import Ui_MainWindow\n'
    + '\n'
    + common_import_names(sorted(
        n for n in needed_common if n not in PLOT_WINDOW_CLASSES))
    + ('\nfrom gui_py.koko_plots import (\n' + plot_import_lines + ')\n' if plot_imp else '')
    + '\n\n'
    + 'class KokoMainWindow(KokoProcessMixin, KokoTableMixin,\n'
    + '                     KokoMenuMixin, KokoPlotMixin,\n'
    + '                     QMainWindow, Ui_MainWindow):\n'
    + '\n\n'.join(ctrl_segs) + '\n')


def writef(name, header, body):
    text = header.rstrip('\n') + '\n\n' + body.lstrip('\n') + '\n'
    p = os.path.join(OUT, name)
    open(p, 'w').write(text)
    print("wrote", p, len(text), "bytes")


writef('_koko_gui_common.py', ch, cb)
writef('koko_process.py', proc_h, proc_b)
writef('koko_table.py', tab_h, tab_b)
writef('koko_menus.py', men_h, men_b)
writef('koko_plots.py', plot_h, plot_b + '\n\n' + pw)
writef('mainwindow.py', mainwin_src, '')

# sanity: every KokoMainWindow method assigned
assigned = (PROCESS | TABLE | MENUS | PLOTS | set(controller))
missing = {m.name for m in km_methods} - assigned
extra = (PROCESS | TABLE | MENUS | PLOTS) & set(controller)
assert not missing, "unassigned: %s" % missing
assert not extra, "double-assigned: %s" % extra
print("OK: controller=%d, process=%d, table=%d, menus=%d, plot=%d, dialogs=%d, winclasses=%d"
      % (len(controller), len(PROCESS), len(TABLE), len(MENUS), len(PLOTS),
         len(DIALOG_CLASSES), len(PLOT_WINDOW_CLASSES)))
