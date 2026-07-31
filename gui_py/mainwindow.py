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
import shutil
import subprocess

from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QMessageBox, QFileDialog, QTableWidgetItem,
    QDialog, QLabel, QVBoxLayout, QHBoxLayout, QLineEdit, QPushButton,
    QComboBox, QDialogButtonBox, QInputDialog, QMenu,
)
from PyQt6.QtCore import QProcess, Qt, QTimer, QByteArray, QSize, QEvent
from PyQt6.QtGui import QFont, QPixmap, QImage


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

    def get_value(self):
        if self.exec() == QDialog.DialogCode.Accepted:
            return (self.lineEdit.text().strip(),
                    self.lineEdit_2.text().strip(),
                    self.lineEdit_3.text().strip())
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
    def __init__(self):
        super().__init__()
        self.setupUi(self)

        # koko-cli process
        self.process = QProcess(self)
        self.koko_path = self.find_koko_cli()

        # directories
        self.HOME = os.path.expanduser('~/KODS')
        self.TMPDIR = '/tmp'
        self.current_lens = None

        # font
        self.msgView.setFont(QFont("Noto Mono", 10, QFont.Weight.Bold))
        self.cmdLine.setFocus()

        # process signals
        self.process.readyReadStandardOutput.connect(self.on_stdout)
        self.process.readyReadStandardError.connect(self.on_stderr)
        self.process.finished.connect(self.on_finished)
        self.process.errorOccurred.connect(self.on_error)

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

        # editing: forward cell edits to koko
        self._table_updating = False
        self.table.cellChanged.connect(self.on_cell_changed)
        # right-click context menu (mirrors the C++ GUI)
        self.table.setContextMenuPolicy(
            Qt.ContextMenuPolicy.CustomContextMenu)
        self.table.customContextMenuRequested.connect(
            self.slot_show_context_menu)
        # cache of glass catalog names, lazily loaded
        self._glass_catalogs = None

        # plot image window
        self.plot_window = None

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
            './koko-cli',
            '/tmp/Koko/Src/koko-cli',
        ]
        for p in paths:
            if os.path.exists(p):
                return p
        return None

    def start_koko_cli(self, lens_path=None):
        """Launch koko-cli in interactive mode (no -b batch)."""
        if self.process.state() == QProcess.ProcessState.Running:
            self.process.write(b"EXIT\n")
            self.process.waitForFinished(2000)

        if not self.koko_path:
            QMessageBox.critical(
                self, "Error",
                "koko-cli not found. Please build Koko first "
                "(see Src/Makefile).")
            return False

        # Always start interactively; lens loading is done via LENSREST.
        # Pass -n so koko does NOT auto-launch gnuplot: the GUI renders
        # the plot itself (this is what distinguishes the GUI build from
        # the cli build, which plots with the native gnuplot window).
        self.process.setWorkingDirectory(os.path.dirname(self.koko_path))
        self.process.start(self.koko_path, ['-n'])
        if not self.process.waitForStarted(5000):
            QMessageBox.critical(self, "Error", "Failed to start koko-cli")
            return False

        # If a lens was requested, restore it the interactive way.
        if lens_path:
            self.load_lens(lens_path)

        # give koko a moment, then ask for surface listing
        QTimer.singleShot(300, lambda: self.send_koko("RTG ALL"))
        return True

    # ----- command I/O ---------------------------------------------------

    def send_koko(self, command):
        """Write a single command line to the koko-cli process."""
        if self.process.state() != QProcess.ProcessState.Running:
            self.append_msg("** koko-cli is not running **")
            return
        self.append_msg("> " + command.strip())
        self.process.write((command + "\n").encode('utf-8'))
        # If this is a plotting command, automatically render the graph
        tok = command.strip().upper()
        if tok:
            first = tok.split()[0]
            if any(tok.startswith(p) for p in PLOT_TRIGGER_PREFIXES) \
                    or first.startswith('PLT') or first.startswith('VIE') \
                    or first.startswith('FANS'):
                self._schedule_plot_render()

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
        self.append_msg("> " + command)
        self.send_koko(command)
        self.cmdLine.clear()

    def on_stdout(self):
        data = self.process.readAllStandardOutput()
        text = bytes(data.data()).decode('utf-8', errors='replace')
        self.append_msg(text)
        # Capture RTG ALL ("BASIC LENS DATA") blocks and populate the table.
        self._capture_rtg(text)

    def _capture_rtg(self, text):
        """Buffer koko's 'BASIC LENS DATA' output and parse it when complete.

        Extracts lens metadata (LI, WV, UNITS) and per-surface detail
        markers (CC, ASPH, ASPH2, TILT, CLAP) so slot_lensInfo can show
        them when the user clicks a table row.
        """
        if not hasattr(self, '_rtg_buf'):
            self._rtg_buf = None
        if 'BASIC LENS DATA' in text:
            self._rtg_buf = ''
        if self._rtg_buf is not None:
            self._rtg_buf += text
            for line in text.splitlines():
                stripped = line.strip()
                # Lens Identifier
                m_li = re.match(r'(?i)^LI\s*,?\s*(.+)$', stripped)
                if m_li:
                    self._li = m_li.group(1).strip()
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

    def on_stderr(self):
        data = self.process.readAllStandardError()
        text = bytes(data.data()).decode('utf-8', errors='replace')
        self.append_msg(text)

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
        if col == 1:
            command = "RD " + val
        elif col == 2:
            command = "TH " + val
        elif col == 3:
            dlg = NKDialog(self)
            dlg.lineEdit.setText(val)
            if dlg.exec() == QDialog.DialogCode.Accepted:
                name, n, v = dlg.get_value()
                command = "MODEL " + name
                if n:
                    command += "," + n
                if v:
                    command += "," + v
        elif col == 6:
            command = "CLAP " + val
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
                if kind in ("MODEL", "SCHOTT", "HIKARI", "OHARA", "HOYA",
                            "CHANCE", "CORNIN", "RADHARD", "SCH2000"):
                    material = (kind + " " + (parts[4] if len(parts) > 4
                                               else "")).strip()
                else:
                    material = kind
            is_glass = material.startswith("MODEL") or material.startswith(
                "SCHOTT")
            # Prefer inline INDEX/V-NUM (real koko RTG ALL); fall back to a
            # pending (MODEL DATA:) line that described a non-glass surface.
            index = parts[5] if (is_glass and len(parts) > 5) else (
                nxt_index if is_glass else "")
            abbe = parts[6] if (is_glass and len(parts) > 6) else (
                nxt_abbe if is_glass else "")
            nxt_index = ""
            nxt_abbe = ""
            rows.append((surf, cur_type, radius, thickness, material,
                         index, abbe, ""))

        self.table.setRowCount(len(rows))
        self.table.setVerticalHeaderLabels([r[0] for r in rows])
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
        self.table.setUpdatesEnabled(True)

    def on_cell_changed(self, row, col):
        """Forward an edited table cell to koko (mirrors the C++ GUI)."""
        if self._table_updating:
            return
        if row < 0 or self.process.state() != QProcess.ProcessState.Running:
            return
        item = self.table.item(row, col)
        if item is None:
            return
        val = item.text().strip()

        # Surface 0 (object) is not editable in koko
        if row == 0:
            return

        self.send_koko("U L")
        self.send_koko("CHG %d" % row)
        if col == 2:          # Radius
            self.send_koko("RD " + val)
        elif col == 3:        # Thickness
            self.send_koko("TH " + val)
        elif col == 4:        # Material -> use the nk dialog
            dlg = NKDialog(self)
            dlg.lineEdit.setText(val)
            res = dlg.exec()
            if res == QDialog.DialogCode.Accepted:
                name, n, v = dlg.get_value()
                cmd = "MODEL " + name
                if n:
                    cmd += "," + n
                if v:
                    cmd += "," + v
                self.send_koko(cmd)
        elif col == 7:        # Aperture (CLAP) - C++ case 6 (col 6 in 0-based C++ table)
            self.send_koko("CLAP " + val)
        else:
            self.send_koko("RD " + val)  # fallback: treat unknown col as radius
        self.send_koko("EOS")
        self.send_koko("RTG ALL")

    # ----- lens info (row click) ------------------------------------------

    def slot_lensInfo(self, row, col):
        """Show surface detail info when clicking a table row (mirrors C++)."""
        # Highlight current row cyan, un-highlight previous white
        for i in range(8):
            item_row = self.table.item(row, i)
            item_prev = self.table.item(self._row0, i)
            if item_row is None:
                self._set_cell(row, i, " ")
            if item_prev is None:
                self._set_cell(self._row0, i, " ")
            if self._row0 == row:
                continue
            if self.table.item(row, i):
                self.table.item(row, i).setBackground(
                    Qt.GlobalColor.cyan)
            if self.table.item(self._row0, i):
                self.table.item(self._row0, i).setBackground(
                    Qt.GlobalColor.white)
        self._row0 = row

        self.lensPara.clear()
        self.lensPara.append(self._li)
        self.lensPara.append("Wavelength (um): %.4f, %.4f, %.4f" % (
            self._lF, self._lD, self._lC))

        surf_item = self.table.item(row, 0)
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
                name = data[i:i + 8].split(b'\x00')[0].decode(
                    'ascii', 'replace').strip()
                if name:
                    names.append(name)
                i += 100  # stride between glass records
            # the C++ reader drops the last two records
            if len(names) > 2:
                names = names[:-2]
            cats.append((cat_name, names))
        self._glass_catalogs = cats
        return cats

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
            name, n, v = dlg.get_value()
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

    @staticmethod
    def _is_point_plot(cmd_str):
        """Return True if the plot family is a spot/point diagram."""
        up = cmd_str.upper()
        return ('SPD' in up) or ('PSF' in up) or ('CAPFN' in up)

    def _schedule_plot_render(self):
        """Wait until koko updates drawcmd.gpl, then render the graph."""
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
        """Restore a lens file interactively via koko's LENSREST command."""
        base = os.path.splitext(os.path.basename(file_path))[0]
        self.current_lens = file_path
        self.send_koko("LENSREST " + base)
        self.send_koko("RTG ALL")
        QTimer.singleShot(500, lambda: self.send_koko("VIE XZ"))

    def slot_actionOpen(self):
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Open Lens File", "", "Lens Files (*.PRG *.prg)")
        if file_path:
            if self.process.state() == QProcess.ProcessState.Running:
                self.load_lens(file_path)
            else:
                self.start_koko_cli(lens_path=file_path)

    def slot_actionSave(self):
        file_path, _ = QFileDialog.getSaveFileName(
            self, "Save Lens File", "", "Lens Files (*.PRG)")
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
            self, "Import Zemax File", "", "Zemax Files (*.ZMX)")
        if file_path:
            self.send_koko("ZMX2PRG " + file_path)
            self.send_koko("LENSSAVE")
            self.send_koko("RTG ALL")
            QTimer.singleShot(1000, lambda: self.send_koko("VIE XZ"))

    def slot_actionImport_CODE_V(self):
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Import Code-V File", "", "Code-V Files (*.SEQ)")
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
            name, n, v = dlg.get_value()
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
        """Render koko's gnuplot script into a raster image.

        The Fortran backend writes drawcmd.gpl and runs gnuplot itself
        (pngcairo terminal), producing PNG at $TMPDIR/koko_gnuplot_plot.png.
        This method simply reads that PNG and displays it in the Koko Plot
        window -- no more spawning our own gnuplot subprocess.
        """
        # Path where Fortran write() redirects the gnuplot PNG output
        png_path = os.path.join(self.TMPDIR, 'koko_gnuplot_plot.png')

        if not os.path.isfile(png_path) or os.path.getsize(png_path) == 0:
            self.append_msg(
                "** %s not found or empty — did the plot command run? **"
                % os.path.basename(png_path))
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
            self.plot_window = QMainWindow()
            self.plot_window.setWindowTitle("Koko Plot")
            self._plot_label = QLabel()
            self._plot_label.setScaledContents(True)
            self.plot_window.setCentralWidget(self._plot_label)
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

    # ----- shutdown -------------------------------------------------------

    def slot_quit2(self):
        if self.process.state() == QProcess.ProcessState.Running:
            self.process.write(b"EXIT\n")
            self.process.waitForFinished(3000)
        self.close()


def main():
    import sys
    app = QApplication(sys.argv)
    window = KokoMainWindow()
    window.show()
    sys.exit(app.exec())


if __name__ == '__main__':
    main()
