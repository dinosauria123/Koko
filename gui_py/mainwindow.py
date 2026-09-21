"""KokoMainWindow controller: command dispatch + lens IO.
   Method bodies live in gui_py/koko_*.py mixins.
"""
import os
import re
import math
import subprocess
import struct
import sys

from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QMessageBox, QFileDialog, QTableWidgetItem,
    QDialog, QLabel, QVBoxLayout, QHBoxLayout, QLineEdit, QPushButton,
    QComboBox, QDialogButtonBox, QInputDialog, QMenu, QWidget, QFrame,
    QSizePolicy, QStyledItemDelegate, QCheckBox, QListWidget,
    QListWidgetItem,
)
from PyQt6.QtCore import QProcess, Qt, QTimer, QByteArray, QSize, QEvent, QPointF
from PyQt6.QtGui import QFont, QPixmap, QImage, QPalette, QColor, QBrush


# Commands that make koko write a plot script (drawcmd.gpl). Any of these,
# whether launched from the Plot menu or typed in the command line, should
# trigger an automatic render of the graph in the GUI.
# NOTE: PLTIMG is intentionally excluded - it writes PLOTBMP.BMP directly
# via IGrSaveImageData, not via gnuplot. The ImageBlur dialog handles it
# by polling for PLOTBMP.BMP in _schedule_image_render().


from gui_py.koko_process import KokoProcessMixin
from gui_py.koko_table import KokoTableMixin
from gui_py.koko_menus import KokoMenuMixin
from gui_py.koko_plots import KokoPlotMixin

from gui_py.ui_mainwindow import Ui_MainWindow

from gui_py._koko_gui_common import (
    ApertureDialog, ApodDialog, AsphDialog, AstDialog, BbDialog,
    CapfnDialog, CoatingDialog, DecDialog, DifsetDialog, DistDialog,
    DotfDialog, FldcvDialog, GlassLibDialog, GotfDialog, GrtArrayDialog,
    ImageBlurDialog, LIDialog, LensOpsDialog, MacroDialog,
    MultiApertureDialog, NKDialog, NewDialog, NssDialog, ObscurationDialog,
    OptimizeRunDialog, PikupDialog, PivaxisDialog, PlotDetailDialog,
    PsfDialog, RayAuxDialog, RayDialog, RayInputDialog, RefDialog,
    SolveDialog, SpotDialog, SpsrfDialog, StopDialog, SurtypeDialog,
    TiltDialog, ToperDialog, Ui_MainWindow, VieDialog
)
from gui_py.koko_plots import (
    GlassMapDialog)


class KokoMainWindow(KokoProcessMixin, KokoTableMixin,
                     KokoMenuMixin, KokoPlotMixin,
                     QMainWindow, Ui_MainWindow):
    def closeEvent(self, event):
        self._kill_koko()
        super().closeEvent(event)

    def __init__(self):
        super().__init__()
        self.setupUi(self)
        self.setWindowTitle("KOKO GUI")

        # koko-cli process (run inside a real PTY; see start_koko_cli)
        self._koko_pid = None
        self._koko_fd = None
        self._koko_notifier = None
        self.koko_path = self.find_koko_cli()

        if not self.koko_path:
            self.append_msg("** koko-cli not found **")

        # directories
        self.HOME = os.path.expanduser('~/KODS')
        self.TMPDIR = '/tmp'
        self.current_lens = None
        self._pending_vie = False

        # font
        self.msgView.setFont(QFont("Noto Mono", 10, QFont.Weight.Bold))
        self.cmdLine.setFocus()

        # command line
        self.cmdLine.returnPressed.connect(self.execute_command)

        # menu actions -> koko command map
        self._wire_menus()

        # table headers (matching the C++ GUI columns)
        self.table.setHorizontalHeaderLabels(
            ['Surf', 'Surface Type', 'Radius', 'Thickness',
             'Glass', 'Index n', 'Abbe V'])
        self.table.verticalHeader().setVisible(True)
        # Let the last column stretch to fill the table's content area so no
        # grey gap is left on the right of the grid (and the header band /
        # separator below the titles end exactly on the last column).
        self.table.horizontalHeader().setStretchLastSection(True)
        # Build a custom header row so the Radius/Curvature combo box is
        # embedded in the Radius column title (mirrors original RDM GUI).
        self._build_header_row()
        # lens data table: row click => lensPara detail (mirrors C++)
        self.table.cellClicked.connect(self.slot_lensInfo)
        # Double-clicking Material/Index n/Abbe V opens the Material (nk)
        # dialog. These columns are not directly editable (see _set_cell), so
        # the only way to edit them is this dialog -- matching how koko's glass
        # data is edited (via MODEL name,nd,vd).
        self.table.cellDoubleClicked.connect(self._on_material_cell_double_clicked)
        # Forward an edited cell to koko when the edit is committed
        # (Enter, or moving focus to another cell). Mirrors the C++ GUI
        # slot_action_value_entered. We connect cellChanged (guarded by
        # self._table_updating so populate_table's own writes never
        # trigger a send) instead of only the Return-key path, so an edit
        # committed by clicking away is also sent. Material/Index/Abbe
        # columns are not directly editable, so they can never fire here;
        # the nkDialog double-click path handles those.
        self.table.cellChanged.connect(self._on_cell_changed)
        self._table_updating = False
        # Radius/Curvature display mode (mirrors original RDM flag).
        # False = Radius mode (default), True = Curvature mode (shows 1/R).
        self._curvature_mode = False
        # Cache of raw radius values per row so we can toggle display mode
        # without re-querying koko. row -> radius (float or None).
        self._radius_values = {}
        # cache of glass catalog names, lazily loaded
        self._glass_catalogs = None
        # Last material command sent per surface row (e.g. "RADHARD BK7G18").
        # koko sometimes echoes the RTG ALL line with an EMPTY material field
        # but valid n/V for certain catalog glasses (e.g. RADHARD); we use this
        # to repair the Material column after populate_table parses RTG ALL.
        self._pending_material = {}

        # plot image window
        self.plot_window = None
        # Glass-map window (kept alive while open so it isn't GC'd)
        self.glass_map_window = None
        # Path of the PNG currently shown in plot_window (so we can delete it
        # when the window is closed). None when no plot is shown.
        self._plot_png_path = None
        # Render serialization: a plot command may fire while a previous
        # render is still polling or running. Without guards, VIE XZ / DIST /
        # PLTDIST issued in quick succession spawn several concurrent
        # poll+render chains that write the same fixed PNG path and the
        # stale image gets shown on top of the new one (the "overprint"
        # the user sees). These flags force a single in-flight render.
        self._plot_poll_active = False   # a poll chain is currently running
        self._rendering = False          # render_plots() is executing
        self._render_pending = False     # a render was requested during a run

        # surface detail storage (mirrors C++ ccv/asphv/asph2v/tiltv vectors)
        self._ccv = {}
        self._asphv = {}
        self._asph2v = {}
        self._tiltv = {}
        self._row0 = 0

        # command history (Up/Down arrow navigation, mirrors C++ history)
        self._history = []
        self._hist_cur = 0

        # lens metadata extracted from RTG ALL / LENSTEXT.DAT
        self._li = ""
        self._lF = 0.0
        self._lD = 0.0
        self._lC = 0.0

        # command line key filter for history navigation
        self.cmdLine.installEventFilter(self)

        # start koko-cli (no lens yet)
        self.start_koko_cli()

        # Launch the banner window only after the message view + koko
        # process are up, so banner.set_content() can safely touch them.
        QTimer.singleShot(100, self.show_startup_banner)

    def eventFilter(self, obj, event):
        """Route table/cmdLine key events (mirrors C++ eventFilter)."""
        if obj is self.cmdLine:
            if event.type() == QEvent.Type.KeyPress:
                if event.key() in (Qt.Key.Key_Up, Qt.Key.Key_Down):
                    if not self._history:
                        return False
                    if event.key() == Qt.Key.Key_Up:
                        self._hist_cur -= 1
                        if self._hist_cur < 0:
                            self._hist_cur = len(self._history) - 1
                    else:
                        self._hist_cur += 1
                        if self._hist_cur >= len(self._history):
                            self._hist_cur = 0
                    self.cmdLine.setText(self._history[self._hist_cur])
                    return True
                elif event.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
                    cmd = self.cmdLine.text().strip()
                    if cmd:
                        self._history.append(cmd)
                    self._hist_cur = len(self._history)

        if obj is self.table:
            if event.type() == QEvent.Type.Resize:
                # Window resize / scrollbar appear-disappear changes the
                # table's width or viewport; re-align the custom header row.
                self._sync_header_geometry()
            if event.type() == QEvent.Type.KeyPress:
                if event.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
                    # Let the default handler commit the edit; cellChanged
                    # will then fire and forward the value to koko. Returning
                    # False (instead of consuming the key) is what allows the
                    # edit to be committed on Enter.
                    return False

        return super().eventFilter(obj, event)

    def slot_actionNew(self):
        """Create new lens -- mirrors C++ slot_actionNew fully."""
        dlg = NewDialog(self)
        name = dlg.get_value()
        if not name:
            return
        # Full new-lens sequence per C++: LENS, LI, UNITS, SAY, SCY FANG
        #   TH 1E20, AIR, REFS, AIR, TH 1E20, AIR, EOS, RTG ALL, LENSSAVE
        self.send_koko("LENS")
        self.send_koko("LI " + name + " ")
        self.send_koko("UNITS mm ")
        self.send_koko("SAY ")
        self.send_koko("SCY FANG 0")
        self.send_koko("TH 1.0E20")
        self.send_koko("AIR")
        self.send_koko("REFS")
        self.send_koko("AIR")
        self.send_koko("TH 1.0E20")
        self.send_koko("AIR")
        self.send_koko("EOS")
        self.send_koko("RTG ALL")
        self.send_koko("LENSSAVE")

    def load_lens(self, file_path):
        """Restore a lens file via koko's LENSREST command.

        Pass the BASE NAME only (no directory), exactly like the C++ GUI.
        koko looks the name up under its lens directory (DIRLEN, i.e.
        ~/KODS/LENSES) with .PRG/.koko extensions, so the lens is actually
        loaded and RTG ALL reflects it.

        We deliberately do NOT pass a full path here. koko's LENSREST has
        a FULLPATH branch for directory separators, but it is currently
        broken (it still prepends DIRLEN to the path, yielding
        "~/KODS/LENSES//home/.../FILE.PRG"). Sending the base name avoids
        that bug entirely and is what the C++ GUI does.

        Previously the GUI used ``IN FILE``, but IN FILE only switches the
        input device and does not load a lens in interactive mode, so every
        open showed the default lens (Cooke Triplet).
        """
        self.current_lens = file_path
        base = os.path.splitext(os.path.basename(file_path))[0]
        # Read lens metadata (LI, WV) straight from the .PRG file, mirroring
        # the C++ GUI's ReadFileToTable(). RTG ALL does NOT echo the WV line,
        # so parsing it from koko's terminal output (the old approach) left
        # _lF/_lD/_lC at 0.0 and printed "Wavelength (um): 0.0000, ...".
        self._read_lens_file_meta(file_path)
        # Defer the actual LENSREST until koko is idle. koko rejects a
        # LENSREST as INVALID CMD LEVEL if it arrives while a prior command
        # (notably VIE XZ / PNG generation) is still running, which would
        # leave the table stuck on the previous lens. If koko is busy we
        # poll briefly until it returns to its prompt.
        self._pending_lens_base = base
        self._pending_vie = True
        self._try_send_lensrest()

    def _read_lens_file_meta(self, file_path):
        """Extract LI (lens identifier) and WV (wavelengths) from the lens
        .PRG file, exactly like the C++ MainWindow::ReadFileToTable().

        koko's RTG ALL output omits the WV line, so the only reliable
        source for wavelengths is the lens file itself. Set self._li and
        self._lF/_lD/_lC (matching C++: lambda[0]=lD, [1]=lF, [2]=lC).
        """
        try:
            with open(file_path, 'r', errors='replace') as fh:
                lines = fh.readlines()
        except OSError:
            return
        li = None
        wv = None
        for raw in lines:
            line = raw.strip()
            # LI line (lens identifier) -- same pattern the C++ uses
            m_li = re.match(r'(?i)^LI\s*,?\s*(.+)$', line)
            if m_li:
                li = m_li.group(1).strip()
            # WV line: "WV d f c [ND VD ...]"  (C++ scans for the three
            # wavelength numbers around the dots)
            if re.match(r'(?i)^WV\s', line):
                nums = re.findall(r'[\d.]+', line)
                if len(nums) >= 3:
                    try:
                        wv = (float(nums[0]), float(nums[1]), float(nums[2]))
                    except ValueError:
                        wv = None
        if li is not None:
            self._li = li
        if wv is not None:
            self._lD, self._lF, self._lC = wv

    def _try_send_lensrest(self):
        """Send the pending LENSREST once koko is idle, else retry shortly."""
        if getattr(self, '_koko_idle', True):
            base = getattr(self, '_pending_lens_base', None)
            if base is None:
                return
            self._pending_lens_base = None
            self.send_koko("LENSREST " + base)
            # _capture_rtg fires RTG ALL automatically on "LENS SAVED AS",
            # and VIE XZ after RTG ALL's LAST SURFACE -- so VIE XZ always
            # follows the table update and never races a running command.
        else:
            QTimer.singleShot(150, self._try_send_lensrest)

    def _flush_pending_vie(self):
        """Send VIE XZ if RTG ALL did not emit a LAST SURFACE line yet."""
        if self._pending_vie:
            self._pending_vie = False
            self.send_koko("VIE XZ")
            self._schedule_plot_render()

    def slot_actionOpen(self):
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Open Lens File", os.path.expanduser("~/KODS/LENSES"), "Lens Files (*.PRG *.prg)")
        if file_path:
            if self._koko_pid is not None:
                self.load_lens(file_path)
            else:
                self.start_koko_cli(lens_path=file_path)

    def slot_actionSave(self):
        file_path, _ = QFileDialog.getSaveFileName(
            self, "Save Lens File", os.path.expanduser("~/KODS/LENSES"), "Lens Files (*.PRG)")
        if file_path:
            base = os.path.splitext(os.path.basename(file_path))[0]
            self.send_koko("LENSSAVE " + base)

    def slot_actionExport_Zemax(self):
        file_path, _ = QFileDialog.getSaveFileName(
            self, "Export ZEMAX File", "", "Zemax Files (*.ZMX)")
        if file_path:
            # Mirror C++: "OUT FILE\nLENO ZMX\nOUT TP"
            self.send_koko("OUT FILE " + file_path)
            self.send_koko("LENO ZMX")
            self.send_koko("OUT TP")

    def slot_actionExport_CODE_V(self):
        file_path, _ = QFileDialog.getSaveFileName(
            self, "Export Code-V File", "", "Code-V Files (*.SEQ)")
        if file_path:
            self.send_koko("OUT FILE " + file_path)
            self.send_koko("LENO CV")
            self.send_koko("OUT TP")

    def slot_actionExport_Leno_AC(self):
        """Export the current lens as a KDP2 ASCII (LENO AC) file
        (mirrors KDP2 IDD_LENOACC: OUT FILE + LENO AC + OUT TP)."""
        file_path, _ = QFileDialog.getSaveFileName(
            self, "Export Lens (LENO AC)", "", "Lens Files (*.PRG *.TXT)")
        if file_path:
            self.send_koko("OUT FILE " + file_path)
            self.send_koko("LENO AC")
            self.send_koko("OUT TP")

    def _read_buildstr(self):
        """Read the Fortran core's build string (see Src/buildinfo.inc).

        Returns something like "[GCC 15.2.0][build date: 2026-08-18]
        [git commit: 375]" or "" if it can't be found, so the banner
        stays usable even before a fresh build.
        """
        src_inc = os.path.join(
            os.path.expanduser("~/Koko"), "Src", "buildinfo.inc")
        try:
            with open(src_inc, "r", encoding="utf-8", errors="ignore") as fh:
                text = fh.read()
        except OSError:
            return ""
        m = re.search(r'buildstr\s*=\s*[\'"](.*?)[\'"]', text, re.S)
        if not m:
            return ""
        return m.group(1).strip()

    def slot_actionAbout(self):
        """About box (mirrors KDP2 IDD_ABOUT)."""
        QMessageBox.about(
            self, "About Koko",
            "Koko Optical Design Software (KODS)\n\n"
            "Free software — no warranty, not even for\n"
            "merchantability or fitness for a particular purpose.\n"
            "See COPYING, LICENSE and AUTHORS in the source\n"
            "distribution for details.\n\n"
            "PyQt6 GUI front-end over the koko-cli Fortran core.")

    def slot_actionImport_Zemax(self):
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Import Zemax File", os.path.expanduser("~/KODS/LENSES"), "Zemax Files (*.ZMX)")
        if file_path:
            self.send_koko("ZMX2PRG " + file_path)
            self.send_koko("LENSSAVE")
            self.send_koko("RTG ALL")
            QTimer.singleShot(1000, lambda: self.send_koko("VIE XZ"))

    def slot_actionImport_CODE_V(self):
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Import Code-V File", os.path.expanduser("~/KODS/LENSES"), "Code-V Files (*.SEQ)")
        if file_path:
            self.send_koko("CV2PRG " + file_path)
            self.send_koko("LENSSAVE")
            self.send_koko("RTG ALL")
            QTimer.singleShot(1000, lambda: self.send_koko("VIE XZ"))

    def slot_actionModeldialog(self):
        """Edit menu: Input Model Glass -- mirrors C++ slot_actionModeldialog.

        Now routes through the unified Material dialog so AIR / REFLECTOR /
        catalog choices are also reachable from the Edit menu.
        """
        row = self.table.currentRow()
        if row < 0:
            row = 0
        dlg = NKDialog(self, catalogs=self._load_glass_catalogs())
        if dlg.exec() == QDialog.DialogCode.Accepted:
            cmd = dlg.material_command()
            if not cmd:
                return
            self._send_surface_cmd(row, cmd)
            # FINDGLASS only for explicit MODEL input (see _ctx_material).
            if dlg.material_type() == 'MODEL':
                self.send_koko("FINDGLASS %d" % row)

    def slot_actionInput_LensIdentifier(self):
        dlg = LIDialog(self)
        val = dlg.get_value()
        if val:
            self.send_koko("U L")
            self.send_koko("LI " + val + " ")
            self.send_koko("EOS")
            self.send_koko("LI")

    def slot_actionRay_input_angle(self):
        dlg = RayInputDialog(self)
        val = dlg.get_value()
        if val:
            self.send_koko("U L")
            self.send_koko("SCY FANG " + val)
            self.send_koko("EOS")
            self.send_koko("VIE")

    def slot_actionPikup(self):
        """Parameter pickup: prompt for surface/type/value and send koko's
        PIKUP command inside UPDATE LENS mode. Mirrors KDP2 IDD_PIKSLV."""
        dlg = PikupDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        surf, ptype, val = vals
        self.send_koko("U L")
        self.send_koko("PIKUP %s,%d,%s" % (ptype, surf, repr(val)))
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionSolve(self):
        """Solve editor: prompt for surface/solve family/plane/target and
        send koko's solve command inside UPDATE LENS mode. Mirrors KDP2
        IDD_SLVED. Utility buttons: PIKD (delete all pickups on the
        surface) and SLV ALL (list all solves)."""
        dlg = SolveDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        if vals["action"] == "slvall":
            self.send_koko("SLV ALL")
            return
        surf = vals["surf"]
        self.send_koko("U L")
        self.send_koko("CHG %d" % surf)
        if vals["action"] == "pikd":
            self.send_koko("PIKD")
        else:
            self.send_koko("%s %s" % (vals["cmd"], repr(vals["val"])))
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionAsph(self):
        """Aspheric / toric: prompt for coefficients and send koko's ASPH
        command sequence inside UPDATE LENS mode. Mirrors KDP2 IDD_ASPH."""
        dlg = AsphDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        surf = vals["surf"]
        if vals["mode"] == "asph":
            coeffs: dict = vals["coeffs"]
            self.send_koko("U L")
            self.send_koko("CHG %d" % surf)
            self.send_koko("ASPH")
            self.send_koko("CHG %d" % surf)
            self.send_koko("CC %s" % repr(vals["cc"]))
            for cmd in ("AC", "AD", "AE", "AF", "AG",
                        "AH", "AI", "AJ", "AK", "AL"):
                self.send_koko("%s %s" % (cmd, repr(coeffs[cmd])))
            self.send_koko("EOS")
        else:
            self.send_koko("U L")
            self.send_koko("CHG %d" % surf)
            self.send_koko(vals["toric"])
            self.send_koko("%s %s" % (vals["tormode"], repr(vals["torval"])))
            self.send_koko("EOS")
            if vals["cctor"] != 0.0:
                self.send_koko("U L")
                self.send_koko("CHG %d" % surf)
                self.send_koko("CCTOR %s" % repr(vals["cctor"]))
                self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionGrtArray(self):
        """Grating / array lens: prompt for grating/array params and send
        koko's GRT/ARRAY commands inside UPDATE LENS mode. Mirrors KDP2
        IDD_GRTARRAY."""
        dlg = GrtArrayDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        surf = vals["surf"]
        sent = False
        if vals["grating"] == "assign":
            self.send_koko("U L")
            self.send_koko("CHG %d" % surf)
            self.send_koko("GRT")
            self.send_koko("GRO,%s" % repr(vals["gro"]))
            self.send_koko("GRS,%s" % repr(vals["grs"]))
            self.send_koko("GRX,%s" % repr(vals["grx"]))
            self.send_koko("GRY,%s" % repr(vals["gry"]))
            self.send_koko("GRZ,%s" % repr(vals["grz"]))
            self.send_koko("EOS")
            sent = True
        elif vals["grating"] == "delete":
            self.send_koko("U L")
            self.send_koko("CHG %d" % surf)
            self.send_koko("GRTD")
            self.send_koko("EOS")
            sent = True
        if vals["array"] == "assign":
            self.send_koko("U L")
            self.send_koko("CHG %d" % surf)
            self.send_koko("ARRAY %s,%s,%s" % (
                vals["arraytype"], repr(vals["dx"]), repr(vals["dy"])))
            self.send_koko("EOS")
            sent = True
        elif vals["array"] == "delete":
            self.send_koko("U L")
            self.send_koko("CHG %d" % surf)
            self.send_koko("ARRAYD")
            self.send_koko("EOS")
            sent = True
        if sent:
            self.send_koko("RTG ALL")
        else:
            self.append_msg("Grating/Array: nothing selected")

    def slot_actionSpsrf(self):
        """Special surface: prompt for surface/type and send koko's SPECIAL
        command inside UPDATE SPSRF mode. Mirrors KDP2 IDD_SPSRF."""
        dlg = SpsrfDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        surf, stype = vals
        self.send_koko("U SP")
        self.send_koko("SPECIAL,%d,%d" % (surf, stype))
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionBb(self):
        """Blackbody radiation: open the BB dialog (mirrors KDP2 IDD_BB).
        WIEN / STEFBOLT / PLANK are CMD-level commands; koko prints the
        result as text in the message view. Dialog stays open."""
        dlg = BbDialog(self)
        dlg.exec()

    def slot_actionRayAux(self):
        """Ray settings / analysis aux: open the dialog bundling KDP2
        IDD_RAYSETTINGS / IDD_FIRD / IDD_ISTAT / IDD_FAIL. All are
        CMD-level text commands; results print in the message view."""
        dlg = RayAuxDialog(self)
        dlg.exec()

    def slot_actionFldcv(self):
        """Field curvature: open the FLDCV settings dialog, then on OK
        send FLDCV,<orient>,,<n> (+ PLTFLDCV,,1 if ticked) and render."""
        dlg = FldcvDialog(self)
        if dlg.exec():
            self.slot_plot(*dlg.commands())

    def slot_actionAst(self):
        """Astigmatism: open the AST settings dialog, then on OK send
        AST,<orient>,,<n> (+ PLTAST,,1 if ticked) and render."""
        dlg = AstDialog(self)
        if dlg.exec():
            self.slot_plot(*dlg.commands())

    def slot_actionDist(self):
        """Distortion: open the DIST settings dialog, then on OK send
        DIST/FISHDIST,<orient>,,<n> (+ PLTDIST/PLTFDIST,,1 if ticked)
        and render."""
        dlg = DistDialog(self)
        if dlg.exec():
            self.slot_plot(*dlg.commands())

    def slot_actionCapfn(self):
        """Complex pupil function settings (mirrors KDP2 IDD_CAPFN).
        Dialog stays open; compute / analysis / plot buttons act."""
        dlg = CapfnDialog(self)
        dlg.exec()

    def slot_actionSpot(self):
        """Spot diagram: open the SPD settings dialog, then on OK send
        the ray-pattern / statistics setup + SPD[ ACC][,<wav>]
        (+ PLTSPD if ticked) and render."""
        dlg = SpotDialog(self)
        if dlg.exec():
            self.slot_plot(*dlg.commands())

    def slot_actionPsf(self):
        """PSF: open the PSF settings dialog, then on OK send NRD /
        PSFWRITE / PSFPLOT / <mode>,<wav> / CAPFNOUT and render."""
        dlg = PsfDialog(self)
        if dlg.exec():
            self.slot_plot(*dlg.commands())

    def slot_actionDotf(self):
        """Diffraction MTF: open the DOTF settings dialog, then on OK
        send SPACE I/O + FAR/NEAR + DOTF + PLTDOTF[ LEICA],,1 + DRAW.
        The Leica variant is selected inside the dialog."""
        dlg = DotfDialog(self)
        if dlg.exec():
            self.slot_plot(*dlg.commands())

    def slot_actionGotf(self):
        """Geometric MTF: open the GOTF settings dialog, then on OK
        send SPACE I/O + FAR/NEAR + GOTF + PLTGOTF[ LEICA],1 + DRAW.
        The Leica variant is selected inside the dialog."""
        dlg = GotfDialog(self)
        if dlg.exec():
            self.slot_plot(*dlg.commands())

    def slot_actionLensOps(self):
        """Lens operations (KDP2 item 7): FLIP / SCALE / ZERO / OTHER.
        Dialog stays open; each operation button sends its command."""
        dlg = LensOpsDialog(self)
        dlg.exec()

    def slot_actionMultiAperture(self):
        """Multiple apertures/obscurations (KDP2 IDD_CLAPS / IDD_MCLAP /
        IDD_MCOBS): MULTCLAP / MULTCOBS instances. Dialog stays open."""
        dlg = MultiApertureDialog(self)
        dlg.exec()

    def slot_actionAperture(self):
        """Clear-aperture (CLAP): prompt for shape/params and send koko's CLAP
        command(s) inside UPDATE LENS mode. Mirrors KDP2 IDD_APECIRC /
        IDD_APERECT / IDD_APEELIP / IDD_APERCTK."""
        dlg = ApertureDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        surf = vals["surf"]
        self.send_koko("U L")
        self.send_koko("CHG %d" % surf)
        if vals["shape"] == "Circular":
            self.send_koko("CLAP %s %s %s 0 0" % (
                repr(vals["rad"]), repr(vals["xdec"]), repr(vals["ydec"])))
        elif vals["shape"] == "Rectangular":
            self.send_koko("CLAP RECT %s %s %s %s" % (
                repr(vals["hx"]), repr(vals["hy"]),
                repr(vals["xdec"]), repr(vals["ydec"])))
            self.send_koko("CLAP TILT %s" % repr(vals["tilt"]))
        elif vals["shape"] == "Elliptical":
            self.send_koko("CLAP ELIP %s %s %s %s" % (
                repr(vals["hx"]), repr(vals["hy"]),
                repr(vals["xdec"]), repr(vals["ydec"])))
            self.send_koko("CLAP TILT %s" % repr(vals["tilt"]))
        elif vals["shape"] == "Rectangular + Frame":
            self.send_koko("CLAP RCTK %s %s %s %s %s" % (
                repr(vals["hx"]), repr(vals["hy"]),
                repr(vals["xdec"]), repr(vals["ydec"]), repr(vals["fr"])))
            self.send_koko("CLAP TILT %s" % repr(vals["tilt"]))
        elif vals["shape"] == "Polygonal":
            self.send_koko("CLAP POLY %s %d %s %s" % (
                repr(vals["rad"]), vals["nsides"],
                repr(vals["xdec"]), repr(vals["ydec"])))
            self.send_koko("CLAP TILT %s" % repr(vals["tilt"]))
        elif vals["shape"] == "Erase region":
            self.send_koko("CLAP ERASE %s %s %s" % (
                repr(vals["rad"]), repr(vals["xdec"]), repr(vals["ydec"])))
        elif vals["shape"] == "Delete all (CLAPD)":
            self.send_koko("CLAPD")
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionObscuration(self):
        """Clear-obscuration (COBS): prompt for shape/params and send koko's
        COBS command(s) inside UPDATE LENS mode. Mirrors KDP2 IDD_APECIRC2 /
        IDD_APERECT2 / IDD_APEELIP2."""
        dlg = ObscurationDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        surf = vals["surf"]
        self.send_koko("U L")
        self.send_koko("CHG %d" % surf)
        if vals["shape"] == "Circular":
            # koko: COBS <R> <YDEC> <XDEC>
            self.send_koko("COBS %s %s %s" % (
                repr(vals["rad"]), repr(vals["ydec"]), repr(vals["xdec"])))
        elif vals["shape"] == "Rectangular":
            self.send_koko("COBS RECT %s %s %s %s" % (
                repr(vals["hx"]), repr(vals["hy"]),
                repr(vals["xdec"]), repr(vals["ydec"])))
            self.send_koko("COBS TILT %s" % repr(vals["tilt"]))
        elif vals["shape"] == "Elliptical":
            self.send_koko("COBS ELIP %s %s %s %s" % (
                repr(vals["hx"]), repr(vals["hy"]),
                repr(vals["xdec"]), repr(vals["ydec"])))
            self.send_koko("COBS TILT %s" % repr(vals["tilt"]))
        elif vals["shape"] == "Rectangular + Frame":
            self.send_koko("COBS RCTK %s %s %s %s %s" % (
                repr(vals["hx"]), repr(vals["hy"]),
                repr(vals["xdec"]), repr(vals["ydec"]), repr(vals["fr"])))
            self.send_koko("COBS TILT %s" % repr(vals["tilt"]))
        elif vals["shape"] == "Polygonal":
            self.send_koko("COBS POLY %s %d %s %s" % (
                repr(vals["rad"]), vals["nsides"],
                repr(vals["xdec"]), repr(vals["ydec"])))
            self.send_koko("COBS TILT %s" % repr(vals["tilt"]))
        elif vals["shape"] == "Erase region":
            self.send_koko("COBS ERASE %s %s %s" % (
                repr(vals["rad"]), repr(vals["xdec"]), repr(vals["ydec"])))
        elif vals["shape"] == "Delete all (COBSD)":
            self.send_koko("COBSD")
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionTilt(self):
        """Surface tilt: prompt for tilt type/angles and send koko's TILT
        command inside UPDATE LENS mode. Mirrors KDP2 IDD_TILTS family."""
        dlg = TiltDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        surf = vals["surf"]
        self.send_koko("U L")
        self.send_koko("CHG %d" % surf)
        ttype = str(vals["ttype"])
        if ttype.startswith("Basic"):
            self.send_koko("TILT %s %s %s" % (
                repr(vals["alpha"]), repr(vals["beta"]), repr(vals["gamma"])))
        elif ttype == "Auto":
            self.send_koko("TILT AUTO")
        elif ttype == "DARD":
            self.send_koko("TILT DARD")
        elif ttype == "BEND":
            self.send_koko("TILT BEND")
        elif ttype == "REV":
            self.send_koko("TILT REV")
        elif ttype.startswith("RTILT"):
            self.send_koko("RTILT")
        elif ttype.startswith("TILTD"):
            self.send_koko("TILTD")
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionVie(self):
        """View control: prompt for view type/factor/toggles and send koko's
        VIE command sequence. Mirrors KDP2 IDD_VIE / LENSED.INC."""
        dlg = VieDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        if vals["vig"]:
            self.send_koko("VIEVIG ON")
        else:
            self.send_koko("VIEVIG OFF")
        if vals["sym"]:
            self.send_koko("VIESYM ON")
        else:
            self.send_koko("VIESYM OFF")
        self.send_koko("VIE %s,%s" % (vals["vtype"], repr(vals["factor"])))

    def slot_actionPlotDetail(self):
        """Plot overlay controls: send PLOT FRAME / AXIS / NOTE / PEN /
        UPLOT overlay commands after the current plot, then DRAW so
        drawcmd.gpl is regenerated with the overlays. Mirrors KDP2
        PLOTCAD1-5.FOR command handlers."""
        dlg = PlotDetailDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        cmds = []
        # Frame / Axis
        if vals["frame"]:
            if vals["frame_coords"]:
                cmds.append("PLOT FRAME " + vals["frame_coords"])
            else:
                cmds.append("PLOT FRAME")
        if vals["axis"]:
            cmds.append("PLOT AXIS")
        # Note: PNOTE sets the text, PLOT NOTE x y draws it
        if vals["pnote"]:
            cmds.append("PNOTE " + vals["pnote"])
        if vals["note"]:
            cmds.append("PLOT NOTE %d %d" % (vals["note_x"], vals["note_y"]))
        # Pen
        if vals["pen"]:
            cmds.append("PLOT PEN %d %d %d"
                        % (vals["pen_x"], vals["pen_y"], vals["pen_state"]))
        # User plot
        if vals["uplot"]:
            cmds.append("PLOT UPLOT %d %d %d %d"
                        % (vals["uxr1"], vals["uxr2"],
                           vals["uyr1"], vals["uyr2"]))
        if not cmds:
            self.append_msg("Plot Detail: nothing selected")
            return
        for c in cmds:
            self.send_koko(c)
        # Regenerate drawcmd.gpl with the overlays and render
        self.send_koko("DRAW")
        self._last_plot_cmd = "DRAW"
        self._schedule_plot_render()

    def slot_actionSurtype(self):
        """Surface type: prompt for surface (or all) and send koko's SURTYPE
        display command. koko's SURTYPE prints REAL/PARAXIAL per surface."""
        dlg = SurtypeDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        if vals["all_surfs"]:
            self.send_koko("SURTYPE ALL")
        else:
            self.send_koko("SURTYPE %d" % vals["surf"])

    def slot_actionCoating(self):
        """Surface coating: prompt for surface/coating index and send koko's
        COATING command inside UPDATE LENS mode. Mirrors KDP2 COATING."""
        dlg = CoatingDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        surf = vals["surf"]
        self.send_koko("U L")
        self.send_koko("CHG %d" % surf)
        if vals["show_only"]:
            self.send_koko("COATING ?")
        else:
            self.send_koko("COATING %d" % vals["index"])
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionPivaxis(self):
        """Pivot axis: prompt for mode/coords and send koko's PIVAXIS command
        inside UPDATE LENS mode. Mirrors KDP2 IDD_PIVAX."""
        dlg = PivaxisDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        surf = vals["surf"]
        if vals["show_only"]:
            self.send_koko("U L")
            self.send_koko("CHG %d" % surf)
            self.send_koko("PIVAXIS ?")
            self.send_koko("EOS")
            self.send_koko("RTG ALL")
            return
        self.send_koko("U L")
        self.send_koko("CHG %d" % surf)
        if vals["mode"] == "NORMAL":
            self.send_koko("PIVAXIS NORMAL")
        else:  # VERTEX
            self.send_koko("PIVAXIS VERTEX")
            self.send_koko("PIVOT,%s,%s,%s" % (
                repr(vals["x"]), repr(vals["y"]), repr(vals["z"])))
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionGlassLib(self):
        """Lens library: prompt for operation/slot and send koko's LIB command.
        Mirrors KDP2 IDD_LLIB (GET/PUT/DEL subset koko supports)."""
        dlg = GlassLibDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        slot = vals["slot"]
        if vals["op"] == "GET":
            self.send_koko("LIB GET %d" % slot)
        elif vals["op"] == "PUT":
            self.send_koko("LIB PUT %d" % slot)
        elif vals["op"] == "DEL":
            self.send_koko("LIB DEL %d" % slot)

    def slot_actionStop(self):
        """Aperture stop: prompt for surface/pupil-adjust and send koko's
        ASTOP command inside UPDATE LENS mode. Mirrors KDP2 IDD_STOPSURF."""
        dlg = StopDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        self.send_koko("U L")
        self.send_koko("CHG %d" % vals["surf"])
        self.send_koko("ASTOP%s" % vals["qual"])
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionRef(self):
        """Reference surface: prompt for surface/rotation and send koko's
        REFS command inside UPDATE LENS mode. Mirrors KDP2 IDD_REFSSURF."""
        dlg = RefDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        self.send_koko("U L")
        self.send_koko("CHG %d" % vals["surf"])
        self.send_koko("REFS %s" % repr(vals["rot"]))
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionDec(self):
        """Decenter: prompt for surface/X/Y/Z and send koko's DEC command
        inside UPDATE LENS mode. Mirrors KDP2 IDD_DEC."""
        dlg = DecDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        self.send_koko("U L")
        self.send_koko("CHG %d" % vals["surf"])
        self.send_koko("DEC %s %s %s" % (
            repr(vals["x"]), repr(vals["y"]), repr(vals["z"])))
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def slot_actionMacro(self):
        """Macro library: init (once), run, delete, or edit a macro.
        Mirrors KDP2 IDD_MACRO intent for the subset koko supports."""
        dlg = MacroDialog(self)
        # Keep the init button in sync with actual library state.
        import os
        libmac = os.path.join(os.path.expanduser("~"), "KODS", "LIBMAC")
        dlg._ui.btn_init.setEnabled(
            not os.path.isdir(libmac) or
            not os.path.exists(os.path.join(libmac, "MAC.DAT")))
        vals = dlg.get_values()
        if not vals:
            return
        name = vals["name"]
        if vals["op"] == "EDIT":
            # Enter mac> mode; user types commands then MACSAVE.
            self.send_koko("MACED %s" % name)
            return
        if vals["op"] == "DEL":
            self.send_koko("MDEL %s" % name)
            return
        # RUN
        self.send_koko("MACRO %s" % name)

    def slot_actionNss(self):
        """Non-sequential database: open the NSS dialog (mirrors KDP2
        NSS-menu intent). koko implements NSS fully via NSSCALL."""
        dlg = NssDialog(self)
        dlg.exec()

    def slot_actionToper(self):
        """Tolerancing: open the tolerancing dialog (mirrors KDP2 tolerance
        editor intent). koko implements TVAR/TOPER/SENSI/MONTE fully."""
        dlg = ToperDialog(self)
        dlg.exec()

    def slot_actionGlassMap(self):
        """Glass map (n vs v): open the catalog picker and render the map."""
        dlg = GlassMapDialog(self)
        dlg.exec()

    def slot_actionRay_single(self):
        """Single-ray trace: prompt for normalized field (X,Y) and either
        trace the ray (text output) or plot its transverse-aberration fan.
        Mirrors KDP2 IDD_RAY / RAYS.INC."""
        dlg = RayDialog(self)
        vals = dlg.get_values()
        if not vals:
            return
        mode, x, y = vals
        if mode == "fan":
            # transverse-aberration fan for this field point
            self.send_koko("FANS XFAN")
        else:
            # trace the single ray and list its coordinates per surface
            self.send_koko("FOB %s %s" % (x, y))
            self.send_koko("RAY")
            self.send_koko("PRXYZ ALL")

    def slot_actionFocus(self):
        """Set focus: adjust last surface PY to bring best focus (mirrors C++ slot_focus)."""
        # Get total number of surfaces
        try:
            last_surf = self.table.rowCount() - 2
            if last_surf < 0:
                last_surf = 0
        except Exception:
            last_surf = 0
        self.send_koko("U L")
        self.send_koko("CHG %d" % last_surf)
        self.send_koko("PY")
        self.send_koko("EOS")
        self.send_koko("RTG ALL")
        self.send_koko("VIE")

    def slot_actionApod_Settings(self):
        """Image Evaluation -> Aperture Apodization Settings (mirrors IDD_APOD)."""
        dlg = ApodDialog(self)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            self.send_koko(dlg.apply_command())

    def slot_actionImageBlur(self):
        """Image Evaluation -> Image Blur: load a BMP, blur it with the
        lens PSF, and display the result.

        KDP2 parity (IMAGE1.FOR FULLIMAGING): the PSF grid spacing (GRI)
        must equal the image-plane pixel size (IDELX) so the PSF lands
        on a single image pixel. KDP2 requires the user to run PSF first
        to obtain GRI, then set IIMAGEN/IOBJECTD with IDELX = GRI.
        We replicate this: run FOB+PSF, read GRIIMG from PSFGRI.DAT,
        then build IIMAGEN/IOBJECTD extents = GRI * (NX-1).
        """
        dlg = ImageBlurDialog(self)
        if dlg.exec() != QDialog.DialogCode.Accepted:
            return
        # Copy the BMP to ~/KODS/KOBJ.BMP (koko reads $HOME/<name>.BMP)
        n = dlg.get_bmp_path()
        if not n:
            self.append_msg("** Image Blur: no BMP selected **")
            return
        import os
        home = os.path.join(os.path.expanduser("~"), "KODS")
        os.makedirs(home, exist_ok=True)
        dest = os.path.join(home, "KOBJ.BMP")
        try:
            with open(n, "rb") as src, open(dest, "wb") as dst:
                dst.write(src.read())
        except OSError:
            self.append_msg("** Image Blur: BMP copy failed **")
            return
        # Send the verified macro-equivalent command chain. This mirrors the
        # working IMTESTx.MAC exactly: COLOR RGB, IIMAGEN, OFROMBMP, PLTOBJ,
        # TGR/NRD/PGR, IMTRACE2/3, PLTIMG. No separate PSF step is needed
        # (IMTRACE2/3 build the PSF internally; TGR/NRD/PGR size it).
        cmds = dlg.commands()
        if not cmds:
            self.append_msg("** Image Blur: command build failed **")
            return
        for cmd in cmds:
            self.send_koko(cmd)
        self._pending_image = os.path.join(
            os.path.expanduser("~"), "KODS", "PLOTBMP.BMP")
        self.append_msg("** Image Blur: running (this can take a while) **")
        self._schedule_image_render()

    def slot_actionDifset_Settings(self):
        """Image Evaluation -> General Diffraction Calculation Settings."""
        dlg = DifsetDialog(self)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            for cmd in dlg.apply_commands():
                self.send_koko(cmd)

    def slot_actionOptimizer(self):
        """Optimize menu -> Optimizer (mirrors original IDD_OPTIM)."""
        dlg = OptimizeRunDialog(self)
        dlg.exec()

    def slot_actionLensData_FIELD(self):
        """Lens Data (Non-surface) -> Field of View Data.

        Mirrors the original Windows GUI (ID_FIELD in GUICODE.FOR), which
        prints the reference-object spec and (when field points exist)
        the field-of-view area layout. We send the same sequence of koko
        commands. SYSTEM(51..54,94,95,98,99) conditions are not exposed to
        the GUI, so we just emit all of the relevant display commands; koko
        ignores the ones that do not apply to the current lens.
        """
        for cmd in ('SCY', 'SCY FANG', 'SCX', 'SCX FANG',
                    'PYIM', 'PYIM FANG', 'PXIM', 'PXIM FANG',
                    'FLDSARE'):
            self.send_koko(cmd)

    def slot_text_insert_surface(self):
        """Edit menu -> Insert Surface (mirrors C++ slot_actionInsert_surface)."""
        row = self.table.currentRow()
        if row < 0:
            row = 1
        if row != 0:
            # Insert row in table
            self.table.insertRow(row)
            self._set_cell(row, 0, "")
            self._set_cell(row, 1, "inf")
            self._set_cell(row, 2, "0")
            self._set_cell(row, 3, "AIR")
            self._ccv[row] = " "
            self._asphv[row] = " "
            self._asph2v[row] = " "
            self._tiltv[row] = " "
            self.send_koko("U L")
            self.send_koko("INS %d" % row)
            self.send_koko("EOS")
            self.send_koko("RTG ALL")

    def slot_text_delete_surface(self):
        """Edit menu -> Delete Surface (mirrors C++ slot_actionDelete_surface)."""
        row = self.table.currentRow()
        if row < 0:
            return
        if row != 0:
            self.table.removeRow(row)
            self.send_koko("U L")
            self.send_koko("DEL %d" % row)
            self.send_koko("EOS")
            self.send_koko("RTG ALL")


