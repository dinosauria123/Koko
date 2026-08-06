"""
Koko Optical Design Software GUI - main window controller

This module wires the generated Ui_MainWindow (from gui_py/mainwindow.py)
to the koko-cli back end. koko-cli is launched as a child process; commands
typed in the command line or selected from the menus are written to its
standard input and the textual output is displayed in msgView.

Lens files (.PRG) are loaded with "koko-cli -b <file>", after which koko
drops into interactive mode so further commands can be piped in.

Plotting commands make koko write gnuplot script files into
$HOME/gnuplot/*.gpl; this module then calls the system gnuplot to render a
PNG and shows it in a separate window.
"""

import os
import re
import math
import shutil
import subprocess
import struct

from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QMessageBox, QFileDialog, QTableWidgetItem,
    QDialog, QLabel, QVBoxLayout, QHBoxLayout, QLineEdit, QPushButton,
    QComboBox, QDialogButtonBox, QInputDialog, QMenu,
)
from PyQt6.QtCore import QProcess, Qt, QTimer, QByteArray, QSize, QEvent
from PyQt6.QtGui import QFont, QPixmap, QImage, QPalette


# Commands that make koko write a plot script (drawcmd.gpl). Any of these,
# whether launched from the Plot menu or typed in the command line, should
# trigger an automatic render of the graph in the GUI.
PLOT_TRIGGER_PREFIXES = (
    'VIE', 'SPD', 'CAPFN', 'PSF', 'DIST', 'FLDCV', 'AST', 'CHRSHIFT',
    'FANS', 'DRAW', 'DRAWFAN', 'GRAOUT', 'PLT', 'PLOT ',
)

from gui_py.ui_mainwindow import Ui_MainWindow
from gui_py.ui_lidialog import Ui_LIDialog
from gui_py.ui_newdialog import Ui_NewDialog
from gui_py.ui_nkdialog import Ui_nkDialog
from gui_py.ui_rayinputdialog import Ui_rayinputDialog
from gui_py.ui_optimize import Ui_Optimize


# --------------------------------------------------------------------------
# Dialogs
# --------------------------------------------------------------------------

class LIDialog(QDialog, Ui_LIDialog):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)

    def get_value(self):
        if self.exec() == QDialog.DialogCode.Accepted:
            return self.lineEdit.text().strip()
        return None


class NewDialog(QDialog, Ui_NewDialog):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)

    def get_value(self):
        if self.exec() == QDialog.DialogCode.Accepted:
            return self.lineEdit.text().strip()
        return None


class NKDialog(QDialog, Ui_nkDialog):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)

    def values(self):
        """Return the entered (name, n, v) without (re)showing the dialog.

        Call this AFTER exec() has already returned. get_value() below
        shows the dialog itself; calling get_value() after a prior exec()
        would pop the dialog a second time (the "Cancel/OK re-opens it"
        bug), so the callers that do `if dlg.exec() == Accepted:` must use
        values() instead of get_value().
        """
        return (self.lineEdit.text().strip(),
                self.lineEdit_2.text().strip(),
                self.lineEdit_3.text().strip())

    def get_value(self):
        if self.exec() == QDialog.DialogCode.Accepted:
            return self.values()
        return None


class RayInputDialog(QDialog, Ui_rayinputDialog):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)

    def get_value(self):
        if self.exec() == QDialog.DialogCode.Accepted:
            return self.lineEdit.text().strip()
        return None


class OptimizeDialog(QDialog, Ui_Optimize):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)

    def get_value(self):
        if self.exec() == QDialog.DialogCode.Accepted:
            return self.lineEdit.text().strip()
        return None


# --------------------------------------------------------------------------
# Main window
# --------------------------------------------------------------------------

class KokoMainWindow(QMainWindow, Ui_MainWindow):
    def closeEvent(self, event):
        self._kill_koko()
        super().closeEvent(event)

    def __init__(self):
        super().__init__()
        self.setupUi(self)

        # koko-cli process (run inside a real PTY; see start_koko_cli)
        self._koko_pid = None
        self._koko_fd = None
        self._koko_notifier = None
        self.koko_path = self.find_koko_cli()

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
             'Material', 'Index n', 'Abbe V', 'Aperture'])
        self.table.verticalHeader().setVisible(True)
        # lens data table: row click => lensPara detail (mirrors C++)
        self.table.cellClicked.connect(self.slot_lensInfo)
        # Double-clicking Material/Index n/Abbe V opens the Material (nk)
        # dialog. These columns are not directly editable (see _set_cell), so
        # the only way to edit them is this dialog -- matching how koko's glass
        # data is edited (via MODEL name,nd,vd).
        self.table.cellDoubleClicked.connect(self._on_material_cell_double_clicked)

        # Editing is forwarded to koko only via the Return/Enter key handler
        # (eventFilter -> _send_table_current_cell), mirroring the C++ GUI
        # which uses slot_action_value_entered (returnPressed) and NOT a
        # cellChanged signal. Connecting cellChanged here too caused a double
        # dialog: editing a cell fired BOTH the Return-key path and this
        # handler, so the nkDialog (Material edit) popped up twice -- and on
        # Cancel it re-appeared as well. So we intentionally do NOT connect
        # cellChanged.
        self._table_updating = False
        # right-click context menu (mirrors the C++ GUI)
        self.table.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.table.customContextMenuRequested.connect(
            self.slot_show_context_menu)
        # cache of glass catalog names, lazily loaded
        self._glass_catalogs = None

        # plot image window
        self.plot_window = None
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

    # ----- process management --------------------------------------------

    def find_koko_cli(self):
        paths = [
            '/usr/local/bin/koko-cli',
            '/usr/bin/koko-cli',
            os.path.expanduser('~/bin/koko-cli'),
            os.path.expanduser('~/Koko/Src/koko-cli'),
            '/tmp/Koko/Src/koko-cli',
        ]
        for p in paths:
            if os.path.exists(p):
                return p
        return None

    def start_koko_cli(self, lens_path=None):
        """Launch koko-cli inside a real pseudo-terminal.

        koko is a Fortran program that uses linenoise for its interactive
        prompt. It only reads commands and only flushes its text output
        when its stdin/stdout are a real tty, so driving it through a
        QProcess pipe (or a `script` wrapper) leaves it stuck at the
        "1:cmd> " prompt with zero bytes of output. We therefore fork a
        child connected to a pty pair: the child execs koko with the slave
        ends as its std streams, and the GUI watches the master fd via
        a QTimer poll and writes commands with os.write.
        """
        self._kill_koko()

        if not self.koko_path:
            QMessageBox.critical(
                self, "Error",
                "koko-cli not found. Please build Koko first "
                "(see Src/Makefile).")
            return False

        import subprocess, pty, os, fcntl, signal, termios, struct
        master, slave = pty.openpty()
        # non-blocking master so the GUI event loop is never stalled
        fl = fcntl.fcntl(master, fcntl.F_GETFL)
        fcntl.fcntl(master, fcntl.F_SETFL, fl | os.O_NONBLOCK)
        # CRITICAL: set a window size on the slave pty. Without this koko
        # (Fortran + linenoise) stays silent and emits nothing -- `script
        # -qec` sets this internally, which is why it worked and a bare
        # pty+execv did not.
        winsize = struct.pack("HHHH", 24, 80, 0, 0)
        fcntl.ioctl(slave, termios.TIOCSWINSZ, winsize)
        # Launch koko via subprocess (NOT os.fork directly): forking inside
        # a live QApplication corrupts the child and can leave the parent's
        # post-fork code (timer setup) unreached. subprocess.Popen does a
        # safe fork+exec and is the reliable path here.
        try:
            # Flags for the embedded Qt GUI (see Src/koko.f:237-251):
            #   -G  -> GENERATE_PLOT_PNG: koko writes the gnuplot data files
            #         (black/yellow/red.gpl under ~/KODS/gnuplot) on VIE XZ.
            #         It used to also call render_plot_png (an internal gnuplot
            #         that hung the prompt with no X display); that call is now
            #         skipped in PDRAW (hardwar1.f), so koko stays responsive
            #         and the GUI renders the PNG itself via render_plots.
            #         -G is REQUIRED: only GENERATE_PLOT_PNG makes koko emit
            #         the actual trace data into the .gpl files (otherwise they
            #         stay empty and the plot shows no lens lines).
            #   -n  -> NOLAUNCH_GNUPLOT: keeps koko from spawning the native
            #         gnuplot wxt window; set implicitly by -G as well.
            proc = subprocess.Popen(
                [self.koko_path, '-G'],
                stdin=slave, stdout=slave, stderr=slave,
                start_new_session=True, close_fds=True,
            )
        except Exception as _e:
            QMessageBox.critical(self, "Error", "Failed to start koko-cli: %s" % _e)
            os.close(master)
            os.close(slave)
            return False
        os.close(slave)
        # parent
        self._koko_pid = proc.pid
        self._koko_fd = master
        # Commands we have written to koko. koko (linenoise raw mode) echoes
        # the typed command back into its pty output even with ECHO OFF, so
        # the GUI strips these command strings from koko's raw output before
        # displaying it -- the GUI already prints "> CMD" itself in send_koko,
        # and we never want the command shown more than once.
        self._sent_cmds = []
        # Poll the pty master on a timer. (QSocketNotifier on the master fd
        # proved unreliable here, whereas a periodic non-blocking os.read
        # reliably drains koko's output -- matching the standalone pty test.)
        self._koko_poll = QTimer(self)
        self._koko_poll.timeout.connect(self._poll_koko_pty)
        self._koko_poll.start(80)
        # small safety: reap the child if it dies
        self._koko_watch = QTimer(self)
        self._koko_watch.timeout.connect(lambda: self._reap_koko())
        self._koko_watch.start(1000)

        # If a lens was requested, restore it the interactive way. Defer
        # slightly so koko has finished its startup banner/initialization
        # and is ready to accept the LENSREST command.
        if lens_path:
            QTimer.singleShot(600, lambda: self.load_lens(lens_path))
        else:
            # No explicit lens: koko auto-loads its default (Cooke Triplet)
            # into memory but emits NO "LENS SAVED AS" message and NO LI/WV
            # line, so read the default lens file's metadata directly here
            # (mirrors C++ ReadFileToTable). Without this, _lF/_lD/_lC stay
            # 0.0 and the row-click panel shows "Wavelength (um): 0.0000".
            default_lens = os.path.join(
                self.HOME, 'LENSES', 'COOCK.PRG')
            if os.path.exists(default_lens):
                self.current_lens = default_lens
                self._read_lens_file_meta(default_lens)

        # Disable koko's command echo-back. The GUI already echoes every
        # command it sends (" > CMD" via send_koko/execute_command), so
        # koko's own echo would just duplicate that output in the pane.
        # Send ECHO OFF once koko has finished startup and is ready for
        # input (it accepts commands after the initial prompt appears).
        QTimer.singleShot(250, lambda: self.send_koko("ECHO OFF"))
        # give koko a moment, then ask for surface listing
        QTimer.singleShot(400, lambda: self.send_koko("RTG ALL"))
        return True

    def _reap_koko(self):
        if self._koko_pid is None:
            return
        import os
        try:
            pid, _ = os.waitpid(self._koko_pid, os.WNOHANG)
        except ChildProcessError:
            pid = self._koko_pid
        if pid:
            self._koko_pid = None
            if getattr(self, "_koko_poll", None) is not None:
                self._koko_poll.stop()
            if self._koko_notifier is not None:
                self._koko_notifier.setEnabled(False)
            self._koko_watch.stop()
            self.append_msg("** koko-cli exited **")

    def _kill_koko(self):
        import os, signal
        if getattr(self, "_koko_poll", None) is not None:
            self._koko_poll.stop()
            self._koko_poll = None
        if self._koko_notifier is not None:
            self._koko_notifier.setEnabled(False)
            self._koko_notifier = None
        if self._koko_fd is not None:
            try:
                os.close(self._koko_fd)
            except OSError:
                pass
            self._koko_fd = None
        if self._koko_pid is not None:
            try:
                os.kill(self._koko_pid, signal.SIGTERM)
                os.waitpid(self._koko_pid, 0)
            except (OSError, ChildProcessError):
                pass
            self._koko_pid = None

    # ----- command I/O ---------------------------------------------------

    def send_koko(self, command):
        """Write a single command line to the koko-cli process."""
        if self._koko_fd is None or self._koko_pid is None:
            self.append_msg("** koko-cli is not running **")
            return
        self.append_msg("> " + command.strip())
        self._koko_idle = False  # we just issued a command; koko is busy
        # Remember what we sent so _poll_koko_pty can strip the echo koko
        # writes back. Skip the bare "ECHO" query so its reply ("ECHO IS
        # OFF/ON") is not accidentally truncated by the removal below.
        cmd = command.strip()
        if cmd.upper() != "ECHO":
            self._sent_cmds.append(cmd)
            if len(self._sent_cmds) > 8:
                self._sent_cmds.pop(0)
        try:
            os.write(self._koko_fd, (command + "\n").encode('utf-8'))
        except OSError:
            self.append_msg("** failed to write to koko-cli **")
            return
        # If this is a plotting command, automatically render the graph.
        # Check every line (a menu command may be "GOTF\nPLTGOTF" etc.),
        # not just the first, so PLTGOTF/PLTDOTF/PLTSPD embedded after a
        # setup line still trigger the render.
        for line in command.strip().splitlines():
            tok = line.strip().upper()
            if not tok:
                continue
            first = tok.split()[0]
            if any(tok.startswith(p) for p in PLOT_TRIGGER_PREFIXES) \
                    or first.startswith('PLT') or first.startswith('VIE') \
                    or first.startswith('FANS'):
                self._schedule_plot_render()
                break

    def execute_command(self):
        command = self.cmdLine.text().strip()
        if not command:
            return
        # Check for "lib get" command -> refresh table after lenssave
        if command.lower().startswith("lib get"):
            self.send_koko(command)
            self.send_koko("LENSSAVE\n")
            self.send_koko("RTG ALL\n")
            return
        self.send_koko(command)
        self.cmdLine.clear()

    def _poll_koko_pty(self):
        """Read whatever koko wrote to the PTY master and feed it to the
        RTG parser. Called on a timer so we never depend on QSocketNotifier
        quirks. Non-blocking read so we never stall the event loop.
        Accumulates partial lines so a BASIC LENS DATA block split across
        chunks is reassembled before _capture_rtg parses it."""
        if self._koko_fd is None:
            return
        try:
            data = os.read(self._koko_fd, 65536)
        except (OSError, BlockingIOError):
            return
        if not data:
            return
        text = data.decode('utf-8', errors='replace')
        # Strip ANSI escape sequences (color codes AND linenoise cursor
        # movements like ESC[9G / ESC[J) so the GUI terminal shows plain
        # text and the RTG parser never sees raw escape bytes.
        text = re.sub(r'\x1b\[[0-9;?]*[A-Za-z]', '', text)
        # Remove the command echo that koko (linenoise raw mode) writes back
        # into its output: the typed command appears twice (once as the
        # keystroke is drawn, once on enter) even with ECHO OFF. The GUI
        # already prints "> CMD" in send_koko, so drop these echoes here to
        # avoid the command being shown 2-3 times.
        for c in getattr(self, '_sent_cmds', []):
            if c:
                text = text.replace(c, '')
        self.append_msg(text)
        # koko echoes a prompt like " 4:cmd> " after each command finishes.
        # Mark it idle here (on the cleaned stream) because the prompt has
        # no trailing newline, so it would otherwise stay stuck in _line_buf
        # and never reach _capture_rtg's parser.
        if re.search(r'\d+:\s*cmd>', text):
            self._koko_idle = True
        if not hasattr(self, '_line_buf'):
            self._line_buf = ""
        self._line_buf += text
        # split into complete lines (those ending in \n); keep the trailing
        # incomplete fragment buffered for the next poll
        parts = self._line_buf.split("\n")
        self._line_buf = parts.pop()
        for raw in parts:
            self._capture_rtg(raw + "\n")

    def _capture_rtg(self, text):
        """Buffer koko's 'BASIC LENS DATA' output and parse it when complete.

        Extracts lens metadata (LI, WV, UNITS) and per-surface detail
        markers (CC, ASPH, ASPH2, TILT, CLAP) so slot_lensInfo can show
        them when the user clicks a table row.
        """
        if not hasattr(self, '_rtg_buf'):
            self._rtg_buf = None
        if not hasattr(self, '_koko_idle'):
            self._koko_idle = True
        # koko echoes a prompt like " 4:cmd> " (with ANSI color escapes)
        # after each command finishes. Track idleness so load_lens() can
        # avoid sending LENSREST while a prior command (esp. VIE XZ / PNG
        # generation) is still running -- otherwise koko rejects it as
        # INVALID CMD LEVEL and the table never refreshes on a lens switch.
        # Strip ANSI escapes first; the raw prompt is "N:cmd>" at line end.
        _clean = re.sub(r'\x1b\[[0-9;]*[A-Za-z]', '', text)
        if re.search(r'\d+:\s*cmd>\s*$', _clean.rstrip()):
            self._koko_idle = True
        # Always scan for the lens identifier, even outside a BASIC LENS
        # DATA block: koko emits "LENS SAVED AS: <NAME>.PRG HAS BEEN
        # RESTORED" right after LENSREST, BEFORE the RTG ALL table.
        for line in text.splitlines():
            stripped = line.strip()
            m_li = re.match(r'(?i)^LI\s*,?\s*(.+)$', stripped)
            if m_li:
                self._li = m_li.group(1).strip()
            else:
                m_restored = re.search(
                    r'LENS SAVED AS:\s*(\S+?)\.PRG\s+HAS BEEN RESTORED',
                    stripped, re.IGNORECASE)
                if m_restored:
                    name = m_restored.group(1)
                    self._li = name
                    # C++ reads LI/WV straight from the lens file; koko's
                    # RTG ALL does NOT echo the WV line, so parse the lens
                    # file here (mirrors C++ ReadFileToTable). Covers every
                    # load path -- startup default, explicit Open, and any
                    # LENSREST -- because they all emit this message.
                    for cand in (
                        os.path.join(self.HOME, 'LENSES',
                                     name + '.PRG'),
                        os.path.join(self.HOME, 'LENSES',
                                     name + '.prg'),
                        os.path.join(self.HOME, 'LENSES',
                                     name + '.koko'),
                        name + '.PRG',
                        name + '.prg',
                    ):
                        if os.path.exists(cand):
                            self._read_lens_file_meta(cand)
                            break
                    # A new lens is being restored: discard any BASIC LENS
                    # DATA block we were accumulating for the previous lens
                    # so the upcoming RTG ALL is captured into a fresh buffer.
                    self._rtg_buf = None
                    # koko has confirmed the restore; now request the surface
                    # listing. Doing RTG ALL here (instead of in load_lens)
                    # guarantees it is sent only AFTER koko finished the
                    # LENSREST, never while a prior VIE XZ is still running
                    # (which would make koko reject the LENSREST as INVALID
                    # CMD LEVEL and the table would never refresh).
                    self.send_koko("RTG ALL")
        if 'BASIC LENS DATA' in text:
            self._rtg_buf = ''
        if self._rtg_buf is not None:
            self._rtg_buf += text
            for line in text.splitlines():
                stripped = line.strip()
                # Wavelengths: WV d.f.c [ND VD ...]
                if stripped.startswith('WV') or re.match(r'(?i)^WV\s', stripped):
                    nums = re.findall(r'[\d.]+', stripped)
                    if len(nums) >= 3:
                        try:
                            self._lD = float(nums[0])
                            self._lF = float(nums[1])
                            self._lC = float(nums[2])
                        except ValueError:
                            pass
                # Units
                m_units = re.match(r'(?i)^UNITS\s+(.+)$', stripped)
                if m_units:
                    self._units = m_units.group(1).strip().lower()
                # Surface-numbered markers: "N*CC ...", "N*ASPH ...", etc.
                m_surf = re.match(r'^(\d+)\s*\*\s*(CC|ASPH2?|TILT|REFS|ASTOP)\b(.*)', stripped, re.IGNORECASE)
                if m_surf:
                    s = int(m_surf.group(1))
                    kind = m_surf.group(2).upper()
                    rest = m_surf.group(3).strip()
                    if kind == 'CC':
                        self._ccv[s] = rest if rest else line.strip()
                    elif kind == 'ASPH2':
                        self._asph2v[s] = rest if rest else line.strip()
                    elif kind == 'ASPH':
                        self._asphv[s] = rest if rest else line.strip()
                    elif kind == 'TILT':
                        self._tiltv[s] = rest if rest else line.strip()
                # Standalone CC / ASPH / TILT without "*N" prefix
                if not m_surf:
                    m_cc = re.match(r'(?i)^CC\s+(.+)$', stripped)
                    if m_cc:
                        for s in (self._ccv.keys()):
                            if s not in self._ccv or not self._ccv[s]:
                                self._ccv[s] = m_cc.group(1).strip()
                                break
                    m_asph = re.match(r'(?i)^ASPH\s+(.+)$', stripped)
                    if m_asph:
                        for s in (self._asphv.keys() if hasattr(self, '_asphv') else []):
                            if s not in self._asphv or not self._asphv[s]:
                                self._asphv[s] = m_asph.group(1).strip()
                                break
                    m_tilt = re.match(r'(?i)^TILT\s+(.+)$', stripped)
                    if m_tilt:
                        for s in (self._tiltv.keys() if hasattr(self, '_tiltv') else []):
                            if s not in self._tiltv or not self._tiltv[s]:
                                self._tiltv[s] = m_tilt.group(1).strip()
                                break
                # Standalone CLAP value
                m_clap = re.match(r'(?i)^CLAP\s+([\d.eE+-]+)', stripped)
                if m_clap:
                    try:
                        ap_val = float(m_clap.group(1))
                        # find a plausible row to attach this to
                        for r in range(self.table.rowCount()):
                            itm = self.table.item(r, 7)
                            if itm is None or not itm.text().strip():
                                self._set_cell(r, 7, str(ap_val))
                                break
                    except ValueError:
                        pass
            if 'LAST SURFACE' in text or 'NO SURFACES' in text:
                buf = self._rtg_buf
                self._rtg_buf = None
                self.populate_table(buf)
                # If a lens load asked us to issue VIE XZ after RTG ALL
                # finished, do it now that the table is fully populated.
                if self._pending_vie:
                    self._pending_vie = False
                    self.send_koko("VIE XZ")
                    # koko now writes drawcmd.gpl; render the PNG once it does.
                    self._schedule_plot_render()

    # ----- eventFilter (cmdLine Up/Down arrow history) -------------------

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
            if event.type() == QEvent.Type.KeyPress:
                if event.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
                    self._send_table_current_cell()
                    return True

        return super().eventFilter(obj, event)

    def _send_table_current_cell(self):
        """Mirror C++ slot_action_value_entered for the current table cell."""
        row = self.table.currentRow()
        col = self.table.currentColumn()
        if row == 0:
            return
        item = self.table.item(row, col)
        if item is None:
            return
        val = item.text().strip()
        if not val:
            return

        self.send_koko("U L")
        self.send_koko("CHG %d" % row)
        command = None
        # NOTE: Python's table has a leading "Surf" column, so its column
        # indices are +1 vs the C++ table the original switch() was written
        # for. These must match on_cell_changed() below.
        if col == 2:          # Radius
            command = "RD " + val
        elif col == 3:        # Thickness
            command = "TH " + val
        elif col == 4:        # Material -> use the nk dialog
            dlg = NKDialog(self)
            dlg.lineEdit.setText(val)
            if dlg.exec() == QDialog.DialogCode.Accepted:
                name, n, v = dlg.values()
                command = "MODEL " + name
                if n:
                    command += "," + n
                if v:
                    command += "," + v
        elif col == 7:        # Aperture (CLAP)
            command = "CLAP " + val
        # col 1 (Surface Type), col 5 (Index n) and col 6 (Abbe V) are not
        # directly editable in koko (matches C++ case 0/4/5 which do nothing).
        if command:
            self.send_koko(command)
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    # ----- lens data table ----------------------------------------------

    def _set_cell(self, row, col, value):
        item = self.table.item(row, col)
        if item is None:
            item = QTableWidgetItem("")
            self.table.setItem(row, col, item)
        item.setText(str(value))
        # Material / Index n / Abbe V columns (4/5/6) are NOT directly
        # editable from the table: they are edited only through the Material
        # (nk) dialog opened on double-click. Make them selectable but not
        # editable so a normal cell edit can never fire for these columns.
        # The Surface column (0) is koko's identifier and is likewise
        # read-only (mirrors the C++ GUI, where surface number is never an
        # editable field); leaving it editable made a click enter edit mode
        # and momentarily hide the row number. Everything else is editable,
        # matching the C++ case 0/1/2/3/7 which forward Radius/Thickness/
        # Material/Aperture edits to koko.
        if col in (0, 4, 5, 6):
            item.setFlags(Qt.ItemFlag.ItemIsSelectable
                          | Qt.ItemFlag.ItemIsEnabled)
        else:
            item.setFlags(Qt.ItemFlag.ItemIsSelectable
                          | Qt.ItemFlag.ItemIsEnabled
                          | Qt.ItemFlag.ItemIsEditable)

    def _surface_type_str(self, surf):
        """Compose the surface-type label for a row, mirroring the C++
        surftypeCheck() accumulation in ReadFileToTable().

        C++ maps each marker to a text token (independent checks on the
        line content):
          CC *    -> "Conic "
          ASPH *  -> "Asphare "
          TILT *  -> "Tilt "
          REFS*   -> "REFS "
          ASTOP*  -> "STOP "
        The resulting string is what the C++ table puts in its column 0
        (here column 1, "Surface Type"); the surface number itself is the
        row index / vertical header, never part of this label.

        NOTE: in the Python port both conic constants and the REFS/STOP
        markers are recorded in self._ccv, so we must disambiguate by
        content: a conic entry is a numeric value, while REFS/STOP entries
        contain the literal tokens.
        """
        parts = []
        ccv_val = self._ccv.get(surf, "") if hasattr(self, '_ccv') else ""
        asph_val = self._asphv.get(surf, "") if hasattr(self, '_asphv') else ""
        tilt_val = self._tiltv.get(surf, "") if hasattr(self, '_tiltv') else ""
        # conic constant (numeric) -- only when not a REFS/STOP entry
        if ccv_val and 'REFS' not in ccv_val and 'STOP' not in ccv_val \
                and 'ASTOP' not in ccv_val:
            parts.append("Conic")
        if asph_val:
            parts.append("Asphare")
        if tilt_val:
            parts.append("Tilt")
        if 'REFS' in ccv_val:
            parts.append("REFS")
        if 'STOP' in ccv_val or 'ASTOP' in ccv_val:
            parts.append("STOP")
        return " ".join(parts)

    def populate_table(self, text):
        """Parse koko's 'BASIC LENS DATA' (RTG ALL) output into the table."""
        self._table_updating = True
        self.table.setUpdatesEnabled(False)
        rows = []
        cur_type = ""
        nxt_index = ""
        nxt_abbe = ""
        for line in text.splitlines():
            line = line.rstrip()
            if line.startswith("(MODEL DATA:"):
                # Describes the glass surface emitted on the previous line.
                m = re.search(r"Nd=\s*([\d.]+)", line)
                v = re.search(r"Vd=\s*([\d.]+)", line)
                mi = m.group(1) if m else ""
                ma = v.group(1) if v else ""
                if rows and rows[-1][4].startswith(("MODEL", "SCHOTT")):
                    prev = rows[-1]
                    rows[-1] = (prev[0], prev[1], prev[2], prev[3], prev[4],
                                prev[5] or mi, prev[6] or ma, prev[7])
                else:
                    nxt_index = mi
                    nxt_abbe = ma
                continue
            if "SURF" in line and "RADIUS" in line:
                continue
            if not line.strip():
                continue
            # Surface-type / property markers (e.g. "6*REFS,STOP")
            if "*" in line and not line.split()[0].replace("*", "").replace(
                    "-", "").isdigit():
                marker = line.split("*", 1)[1].strip().rstrip(",")
                if rows:
                    rows[-1] = (rows[-1][0], (rows[-1][1] + " " + marker).strip(),
                                rows[-1][2], rows[-1][3], rows[-1][4],
                                rows[-1][5], rows[-1][6], rows[-1][7])
                # Also store surface detail markers (mirrors C++ ccv/asphv/etc.)
                surf_m = re.match(r'(\d+)\*', line)
                surf_num = int(surf_m.group(1)) if surf_m else None
                parts_detail = marker.split(',')
                for pd in parts_detail:
                    pd = pd.strip()
                    if pd.startswith('REFS') or pd == 'REFS':
                        if surf_num is not None:
                            self._ccv[surf_num] = pd
                    elif pd.startswith('ASTOP') or pd == 'ASTOP':
                        if surf_num is not None:
                            self._ccv[surf_num] = pd
                    elif pd.startswith('TILT'):
                        if surf_num is not None:
                            self._tiltv[surf_num] = pd
                    elif pd.startswith('ASPH'):
                        if surf_num is not None:
                            self._asphv[surf_num] = pd
                    elif pd.startswith('ASPH2'):
                        if surf_num is not None:
                            self._asph2v[surf_num] = pd
                continue
            # Try to parse a CC * line (conic constant)
            cc_match = re.match(r'^\s*(\d+?)\s*\*?\s*CC\s+(.*)', line)
            if cc_match:
                s = int(cc_match.group(1))
                self._ccv[s] = cc_match.group(2).strip()
                continue
            # Try to parse ASPH lines
            asph_match = re.match(r'^\s*(\d+?)\s*\*?\s*ASPH\s+(.*)', line)
            if asph_match:
                s = int(asph_match.group(1))
                self._asphv[s] = asph_match.group(2).strip()
                continue
            # Try to parse ASPH2 lines
            asph2_match = re.match(r'^\s*(\d+?)\s*\*?\s*ASPH2\s+(.*)', line)
            if asph2_match:
                s = int(asph2_match.group(1))
                self._asph2v[s] = asph2_match.group(2).strip()
                continue
            # Try to parse TILT lines
            tilt_match = re.match(r'^\s*(\d+?)\s*\*?\s*TILT\s+(.*)', line)
            if tilt_match:
                s = int(tilt_match.group(1))
                self._tiltv[s] = tilt_match.group(2).strip()
                continue
            parts = line.split()
            if not parts:
                continue
            surf = parts[0].replace("*", "").strip()
            try:
                int(surf)
            except ValueError:
                continue
            radius = parts[1] if len(parts) > 1 else ""
            thickness = parts[2] if len(parts) > 2 else ""
            material = ""
            if len(parts) > 3:
                kind = parts[3]
                if kind in ("MODEL", "SCHOTT", "HIKARI", "OHARA",
                            "OHARA-O", "HOYA", "CHANCE", "CORNIN", "RADHARD",
                            "SCH2000"):
                    material = (kind + " " + (parts[4] if len(parts) > 4
                                              else "")).strip()
                else:
                    material = kind
            is_glass = material.startswith("MODEL") or material.startswith(
                "SCHOTT") or material.startswith("HIKARI") or \
                material.startswith("OHARA") or material.startswith("HOYA") or \
                material.startswith("CHANCE") or material.startswith("CORNIN") \
                or material.startswith("RADHARD") or material.startswith(
                "SCH2000")
            # Prefer inline INDEX/V-NUM (real koko RTG ALL); fall back to a
            # pending (MODEL DATA:) line that described a non-glass surface.
            index = parts[5] if (is_glass and len(parts) > 5) else (
                nxt_index if is_glass else "")
            abbe = parts[6] if (is_glass and len(parts) > 6) else (
                nxt_abbe if is_glass else "")
            nxt_index = ""
            nxt_abbe = ""
            rows.append((surf, self._surface_type_str(int(surf)), radius,
                         thickness, material, index, abbe, ""))

            # For catalog glasses (SCHOTT, OHARA, ...) koko's RTG ALL does NOT
            # print n/V, so read the binary catalog here (mirrors the C++
            # DataRead call inside ShowContextMenu*). This guarantees the
            # Index n / Abbe V columns are filled right after the table is
            # built, regardless of async RTG ordering.
            if is_glass and not material.startswith("MODEL"):
                gcat, _, gname = material.partition(" ")
                if gcat and gname:
                    gi, ga = self._calc_glass_nv(gcat, gname)
                    if gi is not None:
                        rows[-1] = (rows[-1][0], rows[-1][1], rows[-1][2],
                                    rows[-1][3], rows[-1][4], gi, ga,
                                    rows[-1][7])

        # When the new row count is *smaller* than the existing one, Qt may
        # emit a dataChanged() with an invalid (-1,-1) index range while it
        # tears down the surplus rows. Guard that by explicitly removing the
        # extra items before shrinking the row count (Qt 6.10 surfaces this as
        # a "dataChanged() called with an invalid index range" warning).
        if self.table.rowCount() > len(rows):
            for r in range(len(rows), self.table.rowCount()):
                for c in range(self.table.columnCount()):
                    it = self.table.takeItem(r, c)
                    if it is not None:
                        del it
        self.table.setRowCount(len(rows))
        # Avoid passing an empty label list to setVerticalHeaderLabels: an
        # empty list drives QHeaderView to emit dataChanged(QModelIndex(-1,-1),
        # QModelIndex(-1,-1)) which Qt 6.10 reports as the same warning. Only
        # set labels when there is at least one row.
        if len(rows) > 0:
            self.table.setVerticalHeaderLabels([r[0] for r in rows])
        # Re-enable updates BEFORE populating the cells. If updates are still
        # disabled here, Qt 6.10.2 defers the dataChanged() emission and flushes
        # it when updates are re-enabled, emitting dataChanged() with an invalid
        # (-1,-1) index range -- which the new Qt 6.10 validation reports as the
        # "dataChanged() called with an invalid index range" warning. Enabling
        # first makes every setItem() emit with a valid index.
        self.table.setUpdatesEnabled(True)
        for i, (surf, stype, radius, thickness, material, index,
                abbe, ap) in enumerate(rows):
            self._set_cell(i, 0, surf)
            self._set_cell(i, 1, stype)
            self._set_cell(i, 2, radius)
            self._set_cell(i, 3, thickness)
            self._set_cell(i, 4, material)
            self._set_cell(i, 5, index)
            self._set_cell(i, 6, abbe)
            self._set_cell(i, 7, ap)
        self._table_updating = False

    # ----- material cell double-click -------------------------------------

    def _on_material_cell_double_clicked(self, row, col):
        """Open the Material (nk) dialog when Material/Index n/Abbe V are
        double-clicked. These columns are not directly editable, so this is the
        only way to edit glass data. Mirrors the C++ Input Model flow."""
        if self._koko_pid is None or row <= 0:
            return
        if col not in (4, 5, 6):   # Material / Index n / Abbe V only
            return
        dlg = NKDialog(self)
        # Pre-fill from the current table values where available.
        mat = self.table.item(row, 4)
        idx = self.table.item(row, 5)
        abb = self.table.item(row, 6)
        name = mat.text().strip() if mat else ""
        n = idx.text().strip() if idx else ""
        v = abb.text().strip() if abb else ""
        dlg.lineEdit.setText(name)
        dlg.lineEdit_2.setText(n)
        dlg.lineEdit_3.setText(v)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            name, n, v = dlg.values()
            self.send_koko("U L")
            self.send_koko("CHG %d" % row)
            cmd = "MODEL " + name
            if n:
                cmd += "," + n
            if v:
                cmd += "," + v
            self.send_koko(cmd)
            self.send_koko("EOS")
            self.send_koko("RTG ALL")

    # ----- lens info (row click) ------------------------------------------

    def slot_lensInfo(self, row, col):
        """Show surface detail info when clicking a table row (mirrors C++)."""
        # Fill any empty cells in the clicked/previous row so background
        # highlighting works. Do this under the _table_updating guard so the
        # programmatic setText does NOT fire cellChanged -> on_cell_changed
        # (which would otherwise emit a spurious RD/TH command and rewrite
        # the Radius when a cell happens to be empty).
        self._table_updating = True
        try:
            for i in range(8):
                if self.table.item(row, i) is None:
                    self.table.setItem(row, i, QTableWidgetItem(" "))
                if self.table.item(self._row0, i) is None:
                    self.table.setItem(self._row0, i, QTableWidgetItem(" "))
        finally:
            self._table_updating = False
        # Highlight current row cyan; the previously selected row returns to
        # the table's base color. NOTE: do NOT use QPalette.ColorRole.AlternateBase
        # for the "restored" color -- on a stock Qt6/Linux palette AlternateBase
        # is often unset and falls back to the Window color (a heavy gray), so
        # the deselected row turned gray instead of returning to the table base.
        # ColorRole.Base is the actual table viewport background, which is what
        # we want the row to blend back into.
        base_color = QApplication.palette().color(
            QPalette.ColorRole.Base)
        for i in range(8):
            if self._row0 == row:
                continue
            if self.table.item(row, i):
                self.table.item(row, i).setBackground(
                    Qt.GlobalColor.cyan)
            if self.table.item(self._row0, i):
                self.table.item(self._row0, i).setBackground(
                    base_color)
        self._row0 = row

        # Defensive fallback: if wavelengths are still unset (e.g. the lens
        # was loaded without a file read), try to recover them from the
        # current lens file. Mirrors C++ ReadFileToTable reading the .PRG.
        cur = getattr(self, 'current_lens', None)
        if self._lF == 0.0 and isinstance(cur, str) and cur and \
                os.path.exists(cur):
            self._read_lens_file_meta(cur)

        self.lensPara.clear()
        self.lensPara.append(self._li)
        self.lensPara.append("Wavelength (um): %.4f, %.4f, %.4f" % (
            self._lF, self._lD, self._lC))

        # Surface type lives in column 1 ("Surface Type"), mirroring the
        # C++ table where column 0 holds the type text (here the surface
        # number is the row's vertical header / the table row index).
        surf_item = self.table.item(row, 1)
        surf_text = surf_item.text().strip() if surf_item else ""
        surf_type = "Surface type:"
        if not surf_text:
            surf_type = "Surface type: Spherical"
        self.lensPara.append("Surface No. %d" % row)
        self.lensPara.append(surf_type + " " + surf_text)

        if row in self._ccv and self._ccv[row]:
            self.lensPara.append(str(self._ccv[row]))
        if row in self._asphv and self._asphv[row]:
            self.lensPara.append(str(self._asphv[row]))
        if row in self._asph2v and self._asph2v[row]:
            self.lensPara.append(str(self._asph2v[row]))
        if row in self._tiltv and self._tiltv[row]:
            self.lensPara.append(str(self._tiltv[row]))

    # ----- table context menu (right-click) ------------------------------

    def _load_glass_catalogs(self):
        """Lazily read glass names from Libs/LIBGLA/*.BIN (mirrors C++ GN1..9)."""
        if self._glass_catalogs is not None:
            return self._glass_catalogs
        mapping = [
            ('CHANCE', 'CHANCE.BIN'),
            ('CORNIN', 'CORNIN.BIN'),
            ('HIKARI', 'HIKARI.BIN'),
            ('HOYA', 'HOYA.BIN'),
            ('OHARA', 'OHARA.BIN'),
            ('OHARA-O', 'OHARA-O.BIN'),
            ('RADHARD', 'RADHARD.BIN'),
            ('SCH2000', 'SCH2000.BIN'),
            ('SCHOTT', 'SCHOTT.BIN'),
        ]
        base = os.path.join(os.path.dirname(os.path.dirname(
            os.path.abspath(__file__))), 'Libs', 'LIBGLA')
        cats = []
        for cat_name, fname in mapping:
            path = os.path.join(base, fname)
            if not os.path.exists(path):
                continue
            with open(path, 'rb') as fh:
                data = fh.read()
            names = []
            i = 132  # skip the leading 'CA' header words
            while i + 8 <= len(data):
                # skip NUL padding
                while i < len(data) and data[i] == 0x00:
                    i += 1
                if i + 8 > len(data):
                    break
                name = data[i:i + 10].split(b'\x00')[0].decode(
                    'ascii', 'replace').strip()
                if name:
                    names.append(name)
                i += 74  # stride between glass records (matches C++ i += 74)
            # the C++ reader drops the last two records
            if len(names) > 2:
                names = names[:-2]
            cats.append((cat_name, names))
        self._glass_catalogs = cats
        return cats

    def _calc_glass_nv(self, catalog, name):
        """Look up a glass in its binary catalog and return (nD, AbbeV).

        Pure calculation helper mirroring the C++ ``DataRead`` routine.
        Returns (None, None) if the glass cannot be found. Used both by
        ``populate_table`` (to fill Index n / Abbe V right after RTG ALL)
        and by ``_read_glass_data``.
        """
        # Wavelengths in um (from C++ code)
        lF = 0.4861327  # F line
        lD = 0.5875618  # d line
        lC = 0.6562725  # C line

        # Try both possible locations for LIBGLA directory
        fname = os.path.join(self.HOME, 'Libs', 'LIBGLA', catalog + '.BIN')
        if not os.path.exists(fname):
            fname = os.path.join(os.path.dirname(os.path.dirname(
                os.path.abspath(__file__))), 'Libs', 'LIBGLA', catalog + '.BIN')
        if not os.path.exists(fname):
            return (None, None)

        with open(fname, 'rb') as fh:
            data = fh.read()

        target = name.strip()
        i = 2
        found = False
        n = len(data)
        while i + 10 <= n:
            while i < n and data[i] == 0x00:
                i += 1
            if i + 10 > n:
                break
            glass_name = data[i:i + 10].split(b'\x00')[0].decode(
                'ascii', 'replace').strip()
            if glass_name == target:
                found = True
                break
            i += 74
        if not found:
            return (None, None)

        i += 10  # skip catalog number (10 bytes after name)
        while i < n and data[i] == 0x20:
            i += 1
        while i < n and data[i] != 0x20:
            i += 1
        while i < n and data[i] == 0x20:
            i += 1

        if i + 48 > n:
            return (None, None)
        try:
            A = [struct.unpack('<d', data[i + j * 8:i + j * 8 + 8])[0]
                 for j in range(6)]
        except struct.error:
            return (None, None)

        if catalog in ('SCHOTT', 'SCH2000', 'OHARA', 'OHARA-O'):
            nF = math.sqrt(1 + (A[0]*lF*lF)/(lF*lF-A[3]) + (A[1]*lF*lF)/(lF*lF-A[4]) + (A[2]*lF*lF)/(lF*lF-A[5]))
            nD = math.sqrt(1 + (A[0]*lD*lD)/(lD*lD-A[3]) + (A[1]*lD*lD)/(lD*lD-A[4]) + (A[2]*lD*lD)/(lD*lD-A[5]))
            nC = math.sqrt(1 + (A[0]*lC*lC)/(lC*lC-A[3]) + (A[1]*lC*lC)/(lC*lC-A[4]) + (A[2]*lC*lC)/(lC*lC-A[5]))
        else:
            nF = math.sqrt(A[0] + A[1]*lF*lF + A[2]/(lF*lF) + A[3]/(lF**4) + A[4]/(lF**6) + A[5]/(lF**8))
            nD = math.sqrt(A[0] + A[1]*lD*lD + A[2]/(lD*lD) + A[3]/(lD**4) + A[4]/(lD**6) + A[5]/(lD**8))
            nC = math.sqrt(A[0] + A[1]*lC*lC + A[2]/(lC*lC) + A[3]/(lC**4) + A[4]/(lC**6) + A[5]/(lC**8))

        abbe = (nD - 1) / (nF - nC) if (nF - nC) != 0 else 0
        return (f"{nD:.4f}", f"{abbe:.1f}")

    def _read_glass_data(self, catalog, name, row):
        """Read glass data from binary catalog and calculate nD, Abbe V.

        Updates table columns 5 (Index n) and 6 (Abbe V). Mirrors the C++
        ``DataRead`` routine (which writes the same two columns)."""

        gi, ga = self._calc_glass_nv(catalog, name)
        if gi is None:
            return False
        prev = self._table_updating
        self._table_updating = True
        self._set_cell(row, 5, gi)
        self._set_cell(row, 6, ga)
        self._table_updating = prev
        return True


    def slot_show_context_menu(self, pos):
        """Right-click menu on the lens table (mirrors the C++ GUI)."""
        row = self.table.currentRow()
        if row < 0:
            return
        menu = QMenu(self)
        a_ins = menu.addAction("Insert Surface")
        a_del = menu.addAction("Delete Surface")
        menu.addSeparator()
        a_model = menu.addAction("Model")
        a_air = menu.addAction("AIR")
        a_refl = menu.addAction("REFLECTOR")
        for cat_name, names in self._load_glass_catalogs():
            if not names:
                continue
            sub = QMenu(cat_name, menu)
            for gname in names:
                sub.addAction(gname)
            menu.addMenu(sub)

        action = menu.exec(self.table.mapToGlobal(pos))
        if action is None:
            return
        if action == a_ins:
            self._ctx_insert_surface(row)
        elif action == a_del:
            self._ctx_delete_surface(row)
        elif action == a_model:
            self._ctx_model(row)
        elif action == a_air:
            self._send_surface_cmd(row, "AIR")
        elif action == a_refl:
            self._send_surface_cmd(row, "REFL")
        else:
            parent = action.parent()
            if isinstance(parent, QMenu) and parent.title():
                self._ctx_glass(row, parent.title(), action.text())

    def _ctx_insert_surface(self, row):
        """Mirror C++ slot_actionInsert_surface: INS <row>, update table."""
        self.send_koko("U L")
        self.send_koko("INS %d" % row)
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def _ctx_delete_surface(self, row):
        """Mirror C++ slot_actionDelete_surface: DEL <row>, update table."""
        if row == 0:
            return  # protect object surface
        self.send_koko("U L")
        self.send_koko("DEL %d" % row)
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def _ctx_model(self, row):
        """Mirror C++ slot_actionModeldialog: open nkDialog, set MODEL."""
        dlg = NKDialog(self)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            name, n, v = dlg.values()
            cmd = "MODEL " + name
            if n:
                cmd += "," + n
            if v:
                cmd += "," + v
            self._send_surface_cmd(row, cmd)
            # After MODEL, also call FINDGLASS to compute n,V (mirrors C++)
            self.send_koko("FINDGLASS %d" % row)

    def _send_surface_cmd(self, row, cmd):
        """CHG <row> then <cmd> (AIR/REFL/MODEL.../CATALOG name), EOS, RTG."""
        self.send_koko("U L")
        self.send_koko("CHG %d" % row)
        self.send_koko(cmd)
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    def _ctx_glass(self, row, catalog, name):
        self._send_surface_cmd(row, "%s %s" % (catalog, name))
        # n and V are now filled by populate_table() once RTG ALL returns
        # (mirrors C++ ShowContextMenu*, which calls DataRead immediately
        # after sending the glass command). Calling _read_glass_data here is
        # redundant and would race with the async RTG ALL reparse, so we let
        # populate_table handle it.

    # ----- finished / error ---------------------------------------------

    def on_finished(self, exit_code, exit_status):
        self.append_msg("** koko-cli exited (code %d) **" % exit_code)

    def on_error(self, error):
        QMessageBox.critical(self, "koko-cli error", str(error))

    def append_msg(self, text):
        for line in text.split('\n'):
            if line.strip():
                self.msgView.append(line.rstrip())

    # ----- menu wiring ----------------------------------------------------

    def _wire_menus(self):
        # File
        self.actionNew.triggered.connect(self.slot_actionNew)
        self.actionOpen.triggered.connect(self.slot_actionOpen)
        self.actionSave.triggered.connect(self.slot_actionSave)
        self.actionQuit.triggered.connect(self.slot_quit2)
        self.actionExport_JPEG.triggered.connect(
            lambda: self.slot_export("jpeg"))
        self.actionExport_EPS.triggered.connect(
            lambda: self.slot_export("eps"))
        self.actionExport_PDF.triggered.connect(
            lambda: self.slot_export("pdf"))
        self.actionImport_Zemax.triggered.connect(self.slot_actionImport_Zemax)
        self.actionImport_Code_V.triggered.connect(self.slot_actionImport_CODE_V)
        self.actionExport_Zemax.triggered.connect(self.slot_actionExport_Zemax)
        self.actionExport_Code_V.triggered.connect(self.slot_actionExport_CODE_V)

        # Lens View (plots)
        self.actionXZ.triggered.connect(lambda: self.slot_plot("VIE XZ"))
        self.actionOrtho.triggered.connect(lambda: self.slot_plot("VIE ORTHO"))

        # Analyze -> spot / wavefront / PSF
        self.actionSpot_Diagram.triggered.connect(
            lambda: self.slot_plot("SPD", "PLTSPD"))
        self.actionWavefront_Phase.triggered.connect(
            lambda: self.slot_plot("CAPFN", "PLOT CAPFNOPD"))
        self.actionWavefront_Intensity.triggered.connect(
            lambda: self.slot_plot("CAPFN", "PLOT CAPFNAPD"))
        self.actionPoint_Spread_Function.triggered.connect(
            lambda: self.slot_plot("PSF"))
        self.actionDistortion.triggered.connect(
            lambda: self.slot_plot("DIST", "PLTDIST"))
        self.actionField_Curvature.triggered.connect(
            lambda: self.slot_plot("FLDCV", "PLTFLDCV"))
        self.actionAstigmatism.triggered.connect(
            lambda: self.slot_plot("AST", "PLTAST"))
        self.actionGeometical.triggered.connect(
            lambda: self.slot_text("GOTF\nPLTGOTF"))
        self.actionGeometical_Leica.triggered.connect(
            lambda: self.slot_text("GOTF\nPLTGOTF LEICA"))
        self.actionDiffraction.triggered.connect(
            lambda: self.slot_text("DOTF\nPLTDOTF"))
        self.actionDiffraction_Leica.triggered.connect(
            lambda: self.slot_text("DOTF\nPLTDOTF LEICA"))
        self.actionParaxial_Chromatic_Focus_Shift.triggered.connect(
            lambda: self.slot_plot("CHRSHIFT", "PLTCHRSH"))

        # Aberration fans (FANS <qualifier>)
        self.actionXYFAN.triggered.connect(
            lambda: self.slot_plot("FANS XYFAN"))
        self.actionYXFAN.triggered.connect(
            lambda: self.slot_plot("FANS YXFAN"))
        self.actionXFAN.triggered.connect(
            lambda: self.slot_plot("FANS XFAN"))
        self.actionYFAN.triggered.connect(
            lambda: self.slot_plot("FANS YFAN"))
        self.actionNFAN.triggered.connect(
            lambda: self.slot_plot("FANS NFAN"))
        self.actionPFAN.triggered.connect(
            lambda: self.slot_plot("FANS PFAN"))
        self.actionXYOPD.triggered.connect(
            lambda: self.slot_plot("FANS XYOPD"))
        self.actionYOPD.triggered.connect(
            lambda: self.slot_plot("FANS YOPD"))
        self.actionNOPD.triggered.connect(
            lambda: self.slot_plot("FANS NOPD"))
        self.actionPOPD.triggered.connect(
            lambda: self.slot_plot("FANS POPD"))
        self.actionXCD.triggered.connect(
            lambda: self.slot_plot("FANS XCD"))
        self.actionYCD.triggered.connect(
            lambda: self.slot_plot("FANS YCD"))
        self.actionXYCD.triggered.connect(
            lambda: self.slot_plot("FANS XYCD"))
        self.actionYXCD.triggered.connect(
            lambda: self.slot_plot("FANS YXCD"))
        self.actionNCD.triggered.connect(
            lambda: self.slot_plot("FANS NCD"))
        self.actionPCD.triggered.connect(
            lambda: self.slot_plot("FANS PCD"))
        self.actionXLA.triggered.connect(
            lambda: self.slot_plot("FANS XLA"))
        self.actionYLA.triggered.connect(
            lambda: self.slot_plot("FANS YLA"))
        self.actionXYLA.triggered.connect(
            lambda: self.slot_plot("FANS XYLA"))
        self.actionYXLA.triggered.connect(
            lambda: self.slot_plot("FANS YXLA"))
        self.actionNLA.triggered.connect(
            lambda: self.slot_plot("FANS NLA"))
        self.actionPLA.triggered.connect(
            lambda: self.slot_plot("FANS PLA"))

        # Edit
        self.actionInsert_Surface.triggered.connect(
            self.slot_text_insert_surface)
        self.actionDelete_Surface.triggered.connect(
            self.slot_text_delete_surface)
        self.actionInput_Glass_Model.triggered.connect(
            self.slot_actionModeldialog)
        self.actionInput_Lens_Idenfier.triggered.connect(
            self.slot_actionInput_LensIdentifier)
        self.actionAll_Lens_Data.triggered.connect(
            lambda: self.slot_text("RTG ALL"))

        # Lens view
        self.actionSet_ray_input_angle.triggered.connect(
            self.slot_actionRay_input_angle)
        self.actionSet_Focus.triggered.connect(
            self.slot_actionFocus)

        # Optimize
        self.actionInput_Variables.triggered.connect(
            self.slot_actionInput_Variables)

    # ----- command helpers ------------------------------------------------

    def slot_text(self, command):
        """Send a command and let its textual output appear in msgView."""
        self.send_koko(command)

    def slot_plot(self, *commands):
        """Send one or more plotting commands (setup then draw)."""
        # Lazily establish a default field of view the first time any
        # plot is requested, so spot/wavefront/PSF plots have data.
        if not getattr(self, '_fob_sent', False):
            self.send_koko("FOB")
            self._fob_sent = True
        for cmd in commands:
            self.send_koko(cmd)
        # Remember which plot family this was so render_plots can choose
        # the right gnuplot style.
        self._last_plot_cmd = " ".join(commands)
        # koko writes drawcmd.gpl on the plotting command; render the PNG
        # once it updates.
        self._schedule_plot_render()

    @staticmethod
    def _is_point_plot(cmd_str):
        """Return True if the plot family is a spot/point diagram."""
        up = cmd_str.upper()
        return ('SPD' in up) or ('PSF' in up) or ('CAPFN' in up)

    def _schedule_plot_render(self):
        """Wait until koko updates drawcmd.gpl, then render the graph.

        Serialized: if a poll chain is already in flight, just mark a
        pending request so the running chain re-renders once more at the
        end instead of spawning a second concurrent chain (which would
        write the same fixed PNG path and overprint the image).
        """
        if self._plot_poll_active:
            self._render_pending = True
            return
        self._plot_poll_active = True
        self._render_pending = False
        gpl = os.path.join(self.HOME, 'gnuplot', 'drawcmd.gpl')
        try:
            base = os.path.getmtime(gpl)
        except OSError:
            base = 0.0
        self._plot_poll = {'base': base, 'tries': 0}
        QTimer.singleShot(600, self._poll_plot_render)

    def _poll_plot_render(self):
        self._plot_poll['tries'] += 1
        gpl = os.path.join(self.HOME, 'gnuplot', 'drawcmd.gpl')
        try:
            m = os.path.getmtime(gpl)
        except OSError:
            m = 0.0
        if m > self._plot_poll['base'] or self._plot_poll['tries'] >= 12:
            # Stop polling; hand off to render_plots (which is itself
            # serialized so two chains can never render at once).
            self._plot_poll_active = False
            self.render_plots()
        else:
            QTimer.singleShot(400, self._poll_plot_render)

    def slot_export(self, fmt):
        self.append_msg("Export %s: generating plot..." % fmt)
        self.send_koko("PLOT")
        QTimer.singleShot(800, lambda: self.render_plots(fmt))

    # ----- dialog slots ---------------------------------------------------

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
        """Edit menu: Input Model Glass -- mirrors C++ slot_actionModeldialog."""
        row = self.table.currentRow()
        if row < 0:
            row = 0
        dlg = NKDialog(self)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            name, n, v = dlg.values()
            cmd = "MODEL " + name
            if n:
                cmd += "," + n
            if v:
                cmd += "," + v
            self._send_surface_cmd(row, cmd)
            # C++ also calls FINDGLASS after setting model
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

    def slot_actionInput_Variables(self):
        dlg = OptimizeDialog(self)
        val = dlg.get_value()
        if val is None or not val:
            return
        # Parse variables from dialog (e.g. "R1 1 10 R2 2 20 ...")
        var_str = val
        self.send_koko("MERIT")
        self.send_koko("FLCLTH %s 1 0 0" % var_str.strip())
        self.send_koko("EOS")
        self.send_koko("VARIABLES")
        for part in var_str.split():
            self.send_koko(part)
        self.send_koko("EOS")
        self.send_koko("VB")
        self.send_koko("OPRD")
        self.send_koko("ITER FULL")
        self.send_koko("RTG ALL")

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

    # ----- plotting -------------------------------------------------------
    def render_plots(self, fmt=None):
        """Render koko's plot to a PNG and display it.

        koko is launched with ``-n`` (NOLAUNCH_GNUPLOT) so it writes the
        plot script into ``$HOME/KODS/gnuplot/drawcmd.gpl`` on VIE XZ but
        does NOT spawn gnuplot itself (launching gnuplot from koko hangs in
        this environment). Instead we run gnuplot here to produce the PNG
        and show it -- a fresh image on every lens switch.

        Serialized: only one render runs at a time. If a new plot request
        arrives while we are mid-render, it is queued (at most one) and
        replayed on exit, so rapid VIE XZ / DIST / PLTDIST clicks can never
        write the same PNG path concurrently and overprint the image.
        """
        # If already rendering, queue a single replay and bail.
        if self._rendering:
            self._render_pending = True
            return
        self._rendering = True
        try:
            self._render_plots_inner(fmt)
        finally:
            self._rendering = False
            # If another plot arrived during this render, replay once.
            if self._render_pending:
                self._render_pending = False
                self.render_plots(fmt)

    def _render_plots_inner(self, fmt=None):
        import subprocess as _sp
        gpl = os.path.join(os.path.expanduser('~'), 'KODS', 'gnuplot',
                           'drawcmd.gpl')
        if not os.path.isfile(gpl) or os.path.getsize(gpl) == 0:
            self.append_msg(
                "** %s not found or empty -- did the plot command run? **"
                % os.path.basename(gpl))
            return
        # drawcmd.gpl is a concatenation of MULTIPLE independent plot
        # blocks (e.g. X-Z layout, field curvature, spot diagram), each
        # terminated by "pause -1". koko appends every block for the
        # current draw into one file, so loading the whole thing makes
        # gnuplot overplot all blocks on a single canvas (the "graph
        # overprint" bug). We render ONLY the LAST block -- that is the
        # figure the user actually requested. We split on "pause" lines
        # (the block separators) and keep the trailing fragment.
        # Also strip any "set terminal" line so our pngcairo terminal wins.
        with open(gpl, 'r') as src:
            raw_lines = src.readlines()
        # Split into blocks at every line starting with "pause" (case-
        # insensitive); keep the last non-empty block.
        blocks = []
        cur = []
        for line in raw_lines:
            if line.strip().lower().startswith('pause'):
                if cur:
                    blocks.append(cur)
                cur = []
            else:
                cur.append(line)
        if cur:
            blocks.append(cur)
        # Fallback: if splitting yielded nothing (no pause markers), use
        # the whole file.
        last_block = blocks[-1] if blocks else raw_lines
        clean_gpl = os.path.join(self.TMPDIR, 'koko_gui_drawcmd.gpl')
        with open(clean_gpl, 'w') as dst:
            for line in last_block:
                low = line.strip().lower()
                # Drop any terminal line so our pngcairo terminal wins.
                if low.startswith('set terminal'):
                    continue
                dst.write(line)
        # Unique PNG path per render so two renders can never clobber the
        # same file and show a half-written / stale image.
        self._png_seq = getattr(self, '_png_seq', 0) + 1
        png_path = os.path.join(self.TMPDIR,
                                'koko_gnuplot_plot_%d.png' % self._png_seq)
        try:
            os.remove(png_path)
        except OSError:
            pass
        # Build a self-contained gnuplot script that loads drawcmd.gpl and
        # renders to PNG. Use pngcairo (no X display needed).
        script = os.path.join(self.TMPDIR, 'koko_gui_render.gpl')
        with open(script, 'w') as f:
            f.write('set terminal pngcairo size 1000,700 font "DejaVu Sans,9"\n')
            f.write('set output "%s"\n' % png_path)
            f.write('load "%s"\n' % clean_gpl)
        try:
            env = dict(os.environ)
            env['DISPLAY'] = ''
            _sp.run(['gnuplot', script], env=env, check=True,
                    stdout=_sp.DEVNULL, stderr=_sp.DEVNULL, timeout=30)
        except Exception as e:  # noqa: BLE001
            self.append_msg("** gnuplot failed: %s **" % e)
            return
        if not os.path.isfile(png_path) or os.path.getsize(png_path) == 0:
            self.append_msg("** plot PNG not produced **")
            return
        self.show_plot(png_path)



    def show_plot(self, path):
        pix = QPixmap(path)
        if pix.isNull():
            self.append_msg("** plot image could not be loaded: %s **" % path)
            return
        # Make the plot window a top-level window (no parent) so it can
        # never be hidden behind the main window.
        if self.plot_window is None:
            from PyQt6.QtWidgets import QLabel
            self.plot_window = PlotWindow(self)
            self.plot_window.setWindowTitle("Koko Plot")
            self._plot_label = QLabel()
            self._plot_label.setScaledContents(True)
            self.plot_window.setCentralWidget(self._plot_label)
        self._plot_png_path = path
        self._plot_label.setPixmap(pix)
        # Enforce a sensible minimum size so a degenerate pixmap can't
        # collapse the window to 0x0 (which looks like "nothing appears").
        size = pix.size()
        if size.width() < 200 or size.height() < 200:
            size = size.expandedTo(QSize(400, 300))
        self._plot_label.resize(size)
        self.plot_window.resize(size)
        self.plot_window.show()
        self.plot_window.raise_()

    def _on_plot_window_closed(self):
        """Plot window closed: drop its PNG and reset so the next plot is clean."""
        if self._plot_png_path:
            try:
                os.remove(self._plot_png_path)
            except OSError:
                pass
            self._plot_png_path = None
        self.plot_window = None

    # ----- shutdown -------------------------------------------------------

    def slot_quit2(self):
        self._kill_koko()
        self.close()


class PlotWindow(QMainWindow):
    """Top-level plot viewer window.

    On close it notifies its owner (the KokoMainWindow) so the PNG it was
    displaying can be deleted and the window reference reset -- the next
    plot then builds a fresh window + fresh PNG instead of reusing a stale
    one (which caused the "overprint" artifact).
    """

    def __init__(self, owner):
        super().__init__()
        self._owner = owner

    def closeEvent(self, event):
        # Notify the owner BEFORE the default close (which may tear the
        # widget down) so it can delete the PNG and reset its reference
        # while the window is still fully alive.
        if self._owner is not None:
            self._owner._on_plot_window_closed()
            self._owner = None
        event.accept()
        super().closeEvent(event)


def main():
    import sys
    app = QApplication(sys.argv)
    window = KokoMainWindow()
    window.show()
    sys.exit(app.exec())


if __name__ == '__main__':
    main()
