"""KokoTableMixin: koko_table.py mixin (see gui_py/mainwindow.py).
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


from gui_py._koko_gui_common import (
    CenterComboDelegate, NKDialog
)

class KokoTableMixin:
    def _build_header_row(self):
        """Build a custom header row above the table so the Radius/Curvature
        combo box sits inside the Radius column's title (mirrors the original
        Windows GUI where the column header toggles the RDM flag).

        The built-in QTableWidget horizontal header can only show text, so we
        hide it and replace it with a QWidget row whose children line up with
        the table columns. The Radius column (col 2) hosts the combo box.
        """
        # Hide the default text header; our custom row replaces it.
        self.table.horizontalHeader().setVisible(False)

        # Build the header band as a light-grey stripe so it reads as a
        # column-header band directly above the grid (the default Qt header
        # is hidden; this replaces it).
        self._header_widget = QWidget(self.centralWidget)
        self._header_widget.setAutoFillBackground(True)
        # No layout manager positions the children (see _sync_header_geometry),
        # so the widget has no implicit sizeHint -- give the band a fixed
        # height matching a normal header row.
        self._header_widget.setFixedHeight(26)
        hp = self._header_widget.palette()
        hp.setColor(QPalette.ColorRole.Window, QColor('#eef0f2'))
        self._header_widget.setPalette(hp)

        # Children are positioned manually in _sync_header_geometry (no
        # layout manager): a QHBoxLayout would compress or overflow the
        # children whenever the table's total column width differs from the
        # available width, which is exactly what misaligns the separators.
        self._header_children = []

        headers = ['Surf', 'Surface Type', 'Radius', 'Thickness',
                   'Glass', 'Index n', 'Abbe V']
        for i, h in enumerate(headers):
            if i == 2:
                # Radius column: the combo box IS the header label
                # ("Radius" / "Curvature"). Style it to sit inside the
                # header band rather than as a detached floating widget.
                # Center the displayed text without making the box editable
                # (an editable+read-only box stops the drop-down from opening
                # on mouse click). A center-aligning delegate handles both the
                # current item and the popup items.
                self.comboRadiusCurvature.setParent(self._header_widget)
                self.comboRadiusCurvature.setItemDelegate(
                    CenterComboDelegate(self.comboRadiusCurvature))
                self.comboRadiusCurvature.setStyleSheet(
                    "QComboBox {"
                    "  background-color: #eef0f2;"
                    "  border: 1px solid #999999;"
                    "  border-radius: 2px;"
                    "  padding: 2px 4px;"
                    "  font: 9pt \"Noto Sans\";"
                    "  color: #222;"
                    "}"
                    "QComboBox::drop-down {"
                    "  border: none;"
                    "  width: 12px;"
                    "}"
                )
                self.comboRadiusCurvature.setCurrentIndex(0)
                self._header_children.append(self.comboRadiusCurvature)
            else:
                lbl = QLabel(h, parent=self._header_widget)
                lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
                lbl.setFont(QFont("Noto Sans", 9, QFont.Weight.Bold))
                lbl.setStyleSheet(
                    "QLabel {"
                    "  color: #333333;"
                    "  background-color: #eef0f2;"
                    "  padding: 3px 2px;"
                    "  border-right: 1px solid #d8dadc;"
                    "}"
                )
                self._header_children.append(lbl)

        # ---- bottom separator line (table-like header rule) ----
        self._header_line = QFrame(self.centralWidget)
        self._header_line.setFrameShape(QFrame.Shape.HLine)
        self._header_line.setFrameShadow(QFrame.Shadow.Plain)
        self._header_line.setLineWidth(1)
        self._header_line.setMidLineWidth(0)
        self._header_line.setStyleSheet("color: #9a9da2;")

        # ---- insert into the table's parent layout:
        #        0: header_widget, 1: separator line, 2: table ----
        self.verticalLayout_2.insertWidget(0, self._header_widget)
        self.verticalLayout_2.insertWidget(1, self._header_line)

        # Keep the custom header row aligned with the table's column grid.
        # The table's columns start frameWidth + verticalHeader-width pixels
        # right of the table's left edge, so a plain 0-margin row above it
        # is always misaligned; _sync_header_geometry mirrors the table's
        # exact geometry (including the horizontal scroll offset) onto the
        # header row whenever anything about it changes.
        self.table.horizontalHeader().sectionResized.connect(
            self._sync_header_geometry)
        self.table.verticalHeader().geometriesChanged.connect(
            self._sync_header_geometry)
        self.table.horizontalScrollBar().valueChanged.connect(
            self._sync_header_geometry)
        # Table resizes (window resize, scrollbar appear/disappear) arrive
        # as Resize events via the event filter installed here.
        self.table.installEventFilter(self)
        # First sync after the layout pass settles.
        QTimer.singleShot(0, self._sync_header_geometry)


    def _build_rtg_rows(self, text):
        """Parse koko's 'BASIC LENS DATA' output into a list of row tuples."""
        rows = []
        nxt_index = ""
        nxt_abbe = ""

        for line in text.splitlines():
            stripped = line.rstrip()
            
            # MODEL DATA block -> provides n/V for preceding non-glass surface
            if stripped.startswith("(MODEL DATA:"):
                m_nd = re.search(r"Nd=\s*([\d.]+)", stripped)
                m_vd = re.search(r"Vd=\s*([\d.]+)", stripped)
                mi = m_nd.group(1) if m_nd else ""
                ma = m_vd.group(1) if m_vd else ""
                if rows and rows[-1][4].startswith(("MODEL", "SCHOTT")):
                    prev = rows[-1]
                    rows[-1] = (prev[0], prev[1], prev[2], prev[3], prev[4],
                                prev[5] or mi, prev[6] or ma)
                else:
                    nxt_index = mi
                    nxt_abbe = ma
                continue
            
            if "SURF" in stripped and "RADIUS" in stripped:
                continue
            if not stripped.strip():
                continue
            
            # Surface-type markers ("6*REFS,STOP" etc.)
            if "*" in stripped:
                first_word = stripped.split()[0].replace("*", "").replace("-", "")
                if not first_word.isdigit():
                    marker = stripped.split("*", 1)[1].strip().rstrip(",")
                    if rows:
                        rows[-1] = (rows[-1][0], (rows[-1][1] + " " + marker).strip(),
                                    rows[-1][2], rows[-1][3], rows[-1][4],
                                    rows[-1][5], rows[-1][6])
                    surf_m = re.match(r"(\d+)\*", stripped)
                    surf_num = int(surf_m.group(1)) if surf_m else None
                    for pd in marker.split(","):
                        pd = pd.strip()
                        if pd == "REFS" or pd.startswith("REFS"):
                            if surf_num is not None: self._ccv[surf_num] = pd
                        elif pd == "ASTOP" or pd.startswith("ASTOP"):
                            if surf_num is not None: self._ccv[surf_num] = pd
                        elif pd.startswith("TILT"):
                            if surf_num is not None: self._tiltv[surf_num] = pd
                        elif pd.startswith("ASPH"):
                            if surf_num is not None: self._asphv[surf_num] = pd
                        elif pd.startswith("ASPH2"):
                            if surf_num is not None: self._asph2v[surf_num] = pd
                    continue
            
            # Standalone CC / ASPH / TILT lines (no *N prefix)
            cc_match = re.match(r"^\s*(\d+?)\s*\*?\s*CC\s+(.*)", stripped)
            if cc_match:
                self._ccv[int(cc_match.group(1))] = cc_match.group(2).strip()
                continue
            asph_match = re.match(r"^\s*(\d+?)\s*\*?\s*ASPH\s+(.*)", stripped)
            if asph_match:
                self._asphv[int(asph_match.group(1))] = asph_match.group(2).strip()
                continue
            asph2_match = re.match(r"^\s*(\d+?)\s*\*?\s*ASPH2\s+(.*)", stripped)
            if asph2_match:
                self._asph2v[int(asph2_match.group(1))] = asph2_match.group(2).strip()
                continue
            tilt_match = re.match(r"^\s*(\d+?)\s*\*?\s*TILT\s+(.*)", stripped)
            if tilt_match:
                self._tiltv[int(tilt_match.group(1))] = tilt_match.group(2).strip()
                continue
            
            # Regular surface data line
            parts = stripped.split()
            if not parts:
                continue
            try:
                int(parts[0].replace("*", ""))
            except ValueError:
                continue
            
            surf = parts[0].replace("*", "").strip()
            radius = parts[1] if len(parts) > 1 else ""
            thickness = parts[2] if len(parts) > 2 else ""
            
            material = ""
            if len(parts) > 3:
                kind = parts[3]
                valid_glasses = ("MODEL", "SCHOTT", "HIKARI", "OHARA", "OHARA-O",
                                 "HOYA", "CHANCE", "CORNIN", "RADHARD", "SCH2000")
                if kind in valid_glasses:
                    material = (kind + " " + (parts[4] if len(parts) > 4 else "")).strip()
                else:
                    material = kind
            
            is_glass = any(material.startswith(p) for p in 
                          ("MODEL", "SCHOTT", "HIKARI", "OHARA", "HOYA",
                           "CHANCE", "CORNIN", "RADHARD", "SCH2000"))
            
            index = parts[5] if (is_glass and len(parts) > 5) else (
                nxt_index if is_glass else "")
            abbe = parts[6] if (is_glass and len(parts) > 6) else (
                nxt_abbe if is_glass else "")
            nxt_index = ""
            nxt_abbe = ""
            
            row = (surf, self._surface_type_str(int(surf)), radius,
                   thickness, material, index, abbe)
            rows.append(row)
            
            if is_glass and not material.startswith("MODEL"):
                gcat, _, gname = material.partition(" ")
                if gcat and gname:
                    gi, ga = self._calc_glass_nv(gcat, gname)
                    if gi is not None:
                        rows[-1] = (rows[-1][0], rows[-1][1], rows[-1][2],
                                    rows[-1][3], rows[-1][4], gi, ga)
        
        return rows


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


    def _ctx_delete_surface(self, row):
        """Mirror C++ slot_actionDelete_surface: DEL <row>, update table."""
        if row == 0:
            return  # protect object surface
        self.send_koko("U L")
        self.send_koko("DEL %d" % row)
        self.send_koko("EOS")
        self.send_koko("RTG ALL")


    def _ctx_insert_surface(self, row):
        """Mirror C++ slot_actionInsert_surface: INS <row>, update table."""
        self.send_koko("U L")
        self.send_koko("INS %d" % row)
        self.send_koko("EOS")
        self.send_koko("RTG ALL")


    def _ctx_material(self, row):
        """Open the Material dialog (nkDialog); set the chosen material.

        Folds the former right-click "Model / AIR / REFLECTOR / Glass
        catalog" choices into the single Material dialog.
        """
        dlg = NKDialog(self, catalogs=self._load_glass_catalogs())
        if dlg.exec() == QDialog.DialogCode.Accepted:
            cmd = dlg.material_command()
            if not cmd:
                return
            self._send_surface_cmd(row, cmd)
            # After a MODEL assignment, recompute n,V (mirrors C++). For
            # catalog glasses koko already knows n/V, and sending FINDGLASS
            # would convert the catalog glass to a MODEL and DROP the
            # material-name field in the RTG ALL echo (e.g. RADHARD showed
            # only the index). So only FINDGLASS for explicit MODEL input.
            if dlg.material_type() == 'MODEL':
                self.send_koko("FINDGLASS %d" % row)


    def _highlight_rows(self, clicked, prev):
        """Swap background colours between the clicked row and the previous selection.
        
        Mirrors C++ MainWindow::slot_lensInfo: only modify cells on rows
        'clicked' and 'prev'. Skip entirely when they are equal.
        """
        # Save col-0 texts BEFORE any modification to protect against
        # Qt-side text mutation from setBackground/setItem calls.
        max_r = max(clicked, prev)
        saved_texts = {}
        for r in range(max_r + 1):
            it = self.table.item(r, 0)
            if it is not None:
                saved_texts[r] = it.text()
        # Rows beyond max_r
        for r in range(len(saved_texts), self.table.rowCount()):
            it = self.table.item(r, 0)
            if it is not None:
                saved_texts[r] = it.text()
        
        base_color = QApplication.palette().color(QPalette.ColorRole.Base)
        sel_color = QColor('cyan')
        
        self._table_updating = True
        try:
            for i in range(8):
                if i != 0:
                    if self.table.item(clicked, i) is None:
                        self.table.setItem(clicked, i, QTableWidgetItem(" "))
                    if self.table.item(prev, i) is None:
                        self.table.setItem(prev, i, QTableWidgetItem(" "))
                
                if clicked == prev:
                    continue
                
                cr = self.table.item(clicked, i)
                cp = self.table.item(prev, i)
                if cr is not None:
                    cr.setBackground(sel_color)
                if cp is not None:
                    cp.setBackground(base_color)
        finally:
            self._table_updating = False
        
        # Restore col-0 texts after all modifications
        for r, txt in saved_texts.items():
            it = self.table.item(r, 0)
            if it is not None:
                it.setText(txt)


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


    def _on_cell_changed(self, row, col):
        """Forward a committed cell edit to koko (mirrors C++ slot_action_
        value_entered). Fires on Enter and on focus-loss commit. Guarded by
        self._table_updating so populate_table's own writes are ignored.
        Only Radius/Curvature (2) and Thickness (3) are
        directly editable; other columns are read-only and never reach here.
        """
        if self._table_updating:
            return
        if row == 0:          # OBJ row is never edited
            return
        if col not in (2, 3):
            return
        self._send_table_current_cell()


    def _on_material_cell_double_clicked(self, row, col):
        """Open the Material (nk) dialog when Material/Index n/Abbe V are
        double-clicked. These columns are not directly editable, so this is the
        only way to edit glass data. Mirrors the C++ Input Model flow."""
        if self._koko_pid is None or row <= 0:
            return
        if col not in (4, 5, 6):   # Material / Index n / Abbe V only
            return
        dlg = NKDialog(self, catalogs=self._load_glass_catalogs())
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
            cmd = dlg.material_command()
            if not cmd:
                return
            self._send_surface_cmd(row, cmd)
            if dlg.material_type() == 'MODEL':
                self.send_koko("FINDGLASS %d" % row)


    def _on_radius_curvature_changed(self, text):
        """Combo box handler: switch Radius/Curvature display mode."""
        self._curvature_mode = (text == "Curvature")
        self._refresh_radius_display()


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


    def _refresh_radius_display(self):
        """Re-draw the Radius column according to the Radius/Curvature mode.

        Mirrors the original LOADSHEET.INC behaviour: in Radius mode the
        raw radius is shown; in Curvature mode 1/R (curvature) is shown.
        The cached self._radius_values holds the raw radius per row.
        """
        prev = self._table_updating
        self._table_updating = True
        for row, radius in self._radius_values.items():
            if row < 0 or row >= self.table.rowCount():
                continue
            if radius is None:
                self._set_cell(row, 2, "")
                continue
            if self._curvature_mode:
                # Curvature = 1 / Radius. Guard against infinite radius
                # (plane surface, R=0 in koko convention -> blank).
                if radius == 0.0:
                    display = ""
                else:
                    display = "%.6g" % (1.0 / radius)
            else:
                display = "%.6g" % radius
            self._set_cell(row, 2, display)
        self._table_updating = prev


    def _send_surface_cmd(self, row, cmd):
        """CHG <row> then <cmd> (AIR/REFL/MODEL.../CATALOG name), EOS, RTG.

        Remembers the command in _pending_material[row] so populate_table
        can repair the Material column when koko echoes an empty material
        field for certain catalog glasses (e.g. RADHARD).
        """
        self._pending_material[row] = cmd
        self.send_koko("U L")
        self.send_koko("CHG %d" % row)
        self.send_koko(cmd)
        self.send_koko("EOS")
        self.send_koko("RTG ALL")


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
        if col == 2:          # Radius / Curvature (toggle via combo box)
            # In Curvature mode the cell shows 1/R, so send CV (curvature);
            # in Radius mode send RD (radius). Mirrors original RDM flag.
            # Validate numeric input (C++ uses WDIALOGGETDOUBLE).
            try:
                new_val = float(val)
            except ValueError:
                return
            if self._curvature_mode:
                command = "CV " + val
                # Curvature input -> cache the equivalent radius (1/CV).
                self._radius_values[row] = (1.0 / new_val) if new_val != 0.0 else 0.0
            else:
                command = "RD " + val
                # Radius input -> cache the radius directly.
                self._radius_values[row] = new_val
        elif col == 3:        # Thickness
            command = "TH " + val
        elif col == 4:        # Material -> use the nk dialog
            dlg = NKDialog(self, catalogs=self._load_glass_catalogs())
            dlg.lineEdit.setText(val)
            if dlg.exec() == QDialog.DialogCode.Accepted:
                command = dlg.material_command()
                if command:
                    self._send_surface_cmd(row, command)
                    if dlg.material_type() == 'MODEL':
                        self.send_koko("FINDGLASS %d" % row)
                    return
        # col 1 (Surface Type), col 5 (Index n) and col 6 (Abbe V) are not
        # directly editable in koko (matches C++ case 0/4/5 which do nothing).
        if command:
            self.send_koko(command)
        self.send_koko("EOS")
        self.send_koko("RTG ALL")


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
        # Explicitly give every cell a real background + foreground. Under
        # PyQt6.10 an item with no explicit background renders with an
        # invalid QColor (observed as black), and combined with the default
        # (black) foreground text becomes invisible -- this is what made the
        # Surface (col 0) number look "gone". White base + black text keeps
        # every cell legible from the moment the table is populated, before
        # any click. (slot_lensInfo later re-applies highlight colors.)
        item.setBackground(QBrush(QApplication.palette().color(
            QPalette.ColorRole.Base)))
        item.setForeground(QBrush(QColor('black')))


    def _show_surface_panel(self, row):
        """Populate lensPara list box with metadata and surface detail for row."""
        # Defensive fallback: recover wavelengths from current lens file
        cur = getattr(self, 'current_lens', None)
        if self._lF == 0.0 and isinstance(cur, str) and cur and os.path.exists(cur):
            self._read_lens_file_meta(cur)
        
        self.lensPara.clear()
        self.lensPara.append(self._li)
        self.lensPara.append(
            "Wavelength (um): %.4f, %.4f, %.4f" % (self._lF, self._lD, self._lC))
        
        surf_item = self.table.item(row, 1)
        surf_text = surf_item.text().strip() if surf_item else "Spherical"
        self.lensPara.append("Surface No. %d" % row)
        self.lensPara.append("Surface type: " + surf_text)
        
        for attr in ('_ccv', '_asphv', '_asph2v', '_tiltv'):
            d = getattr(self, attr, {})
            if row in d and d[row]:
                self.lensPara.append(str(d[row]))


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


    def _sync_header_geometry(self, *args):
        """Align the custom header row exactly with the table's column grid.

        Mirrors the table's own geometry: its columns begin at
        frameWidth + verticalHeader().width() from the table's left edge,
        each header child is moved/resized to its column's exact x/width,
        shifted by the horizontal scroll offset -- so the title separators
        always sit on the table's vertical grid lines. Children are placed
        manually (no layout manager) so nothing can compress or reflow them.
        """
        if not hasattr(self, '_header_widget'):
            return
        t = self.table
        vh = t.verticalHeader()
        # All columns share one fixed width so the grid (e.g. Index n / Abbe
        # V / Radius) reads as evenly spaced. Compute the width from the
        # current content area and hand the leftover 1px rows to the leading
        # columns so sum(columnWidths) equals the content width exactly --
        # leaving no grey gap on the right of the grid or the header band.
        n = t.columnCount()
        if n:
            left = t.frameWidth() + (vh.width() if vh.isVisible() else 0)
            target = (t.width() - (left + t.horizontalScrollBar().value())) // n
            target = max(target, t.horizontalHeader().minimumSectionSize())
            rem = (t.width() - (left + t.horizontalScrollBar().value())) - target * n
            t.horizontalHeader().setStretchLastSection(False)
            for c in range(n):
                t.setColumnWidth(c, target + (1 if c < rem else 0))
            left = t.frameWidth() + (vh.width() if vh.isVisible() else 0)
            left -= t.horizontalScrollBar().value()
        hh = self._header_widget.height()
        x = left
        for i, wgt in enumerate(self._header_children):
            if i >= t.columnCount():
                break
            w = t.columnWidth(i)
            wgt.setGeometry(x, 0, w, hh)
            x += w
        # The columns end at x (left + sum of column widths); the header
        # band and the separator under the titles must match that right edge
        # exactly, not the full table frame width. Sizing them to t.width()
        # left a grey gap beyond the last column whenever the columns did not
        # fill the table width.
        self._header_widget.setFixedWidth(x)
        self._header_line.setFixedWidth(x)


    def populate_table(self, text):
        """Render parsed RTG ALL output in the surface table."""
        self._table_updating = True
        self.table.setUpdatesEnabled(False)
        
        rows = self._build_rtg_rows(text)
        
        if self.table.rowCount() > len(rows):
            for r in range(len(rows), self.table.rowCount()):
                for c in range(self.table.columnCount()):
                    it = self.table.takeItem(r, c)
                    if it is not None:
                        del it
        
        self.table.setRowCount(len(rows))
        if len(rows) > 0:
            self.table.setVerticalHeaderLabels([r[0] for r in rows])

        self.table.setUpdatesEnabled(True)
        for i, (surf, stype, radius, thickness, material, index, abbe) in enumerate(rows):
            # koko emits the RTG ALL line for certain catalog glasses
            # (e.g. RADHARD) with the MATERIAL column BLANK and the index
            # value shifted into the material position, because the name was
            # dropped (model/FINDGLASS conversion). str.split() then yields
            # material = the index number. Repair the Material column from
            # the command we last sent for this row whenever the parsed
            # material is not a known glass/catalog token, and recompute
            # n/V from the catalog so those columns are also filled.
            _valid_glasses = ("MODEL", "SCHOTT", "HIKARI", "OHARA", "OHARA-O",
                              "HOYA", "CHANCE", "CORNIN", "RADHARD", "SCH2000")
            if not material.startswith(_valid_glasses):
                pending = self._pending_material.get(i)
                if pending:
                    material = pending
                    # pending is e.g. "RADHARD BK7G18" or "MODEL NAME,n,v"
                    _pc = pending.split()
                    if len(_pc) >= 2 and _pc[0] in _valid_glasses:
                        gi, ga = self._calc_glass_nv(_pc[0], _pc[1])
                        if gi is not None:
                            index = gi
                            abbe = ga
                        elif len(_pc) >= 3:
                            # MODEL name,n,v form
                            index = _pc[1]
                            abbe = _pc[2] if len(_pc) > 2 else ""
            self._set_cell(i, 0, surf)
            self._set_cell(i, 1, stype)
            self._set_cell(i, 2, radius)
            self._set_cell(i, 3, thickness)
            self._set_cell(i, 4, material)
            self._set_cell(i, 5, index)
            self._set_cell(i, 6, abbe)
            # Cache raw radius (col 2 in _build_rtg_rows output) so the
            # Radius/Curvature display toggle can update without koko.
            try:
                self._radius_values[i] = float(radius)
            except (ValueError, TypeError):
                self._radius_values[i] = None
        self._table_updating = False
        # Apply the current Radius/Curvature display mode to the Radius column.
        self._refresh_radius_display()


    def slot_lensInfo(self, row, col):
        """Show surface detail info when clicking a table row (mirrors C++)."""
        self._highlight_rows(row, self._row0)
        self._row0 = row
        self._show_surface_panel(row)


    def slot_show_context_menu(self, pos):
        """Right-click menu on the lens table (mirrors the C++ GUI)."""
        row = self.table.currentRow()
        if row < 0:
            return
        menu = QMenu(self)
        a_ins = menu.addAction("Insert Surface")
        a_del = menu.addAction("Delete Surface")
        menu.addSeparator()
        a_material = menu.addAction("Material...")

        action = menu.exec(self.table.mapToGlobal(pos))
        if action is None:
            return
        if action == a_ins:
            self._ctx_insert_surface(row)
        elif action == a_del:
            self._ctx_delete_surface(row)
        elif action == a_material:
            self._ctx_material(row)

