"""Shared GUI data / dialogs for the Koko GUI split.
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


from gui_py.ui_mainwindow import Ui_MainWindow
from gui_py.ui_apoddialog import Ui_ApodDialog
from gui_py.ui_difsetdialog import Ui_DifsetDialog
from gui_py.ui_imageblurdialog import Ui_ImageBlurDialog
from gui_py.ui_lidialog import Ui_LIDialog
from gui_py.ui_newdialog import Ui_NewDialog
from gui_py.ui_nkdialog import Ui_nkDialog
from gui_py.ui_rayinputdialog import Ui_rayinputDialog
from gui_py.ui_optimize import Ui_Optimize
from gui_py.ui_optimdialog import Ui_OptimizeDialog
from gui_py.ui_raydialog import Ui_RayDialog
from gui_py.ui_pikupdialog import Ui_PikupDialog
from gui_py.ui_solvedialog import Ui_SolveDialog, SOLVE_TYPES
from gui_py.ui_asphdialog import Ui_AsphDialog, ASPH_COEFFS
from gui_py.ui_grtarraydialog import Ui_GrtArrayDialog
from gui_py.ui_spsrfdialog import Ui_SpsrfDialog
from gui_py.ui_bbdialog import Ui_BbDialog
from gui_py.ui_rayauxdialog import Ui_RayAuxDialog
from gui_py.ui_fldcvdialog import Ui_FldcvDialog
from gui_py.ui_astdialog import Ui_AstDialog
from gui_py.ui_distdialog import Ui_DistDialog
from gui_py.ui_capfndialog import Ui_CapfnDialog
from gui_py.ui_spotdialog import Ui_SpotDialog
from gui_py.ui_psfdialog import Ui_PsfDialog
from gui_py.ui_dotfdialog import Ui_DotfDialog
from gui_py.ui_gotfdialog import Ui_GotfDialog
from gui_py.ui_lensopsdialog import Ui_LensOpsDialog
from gui_py.ui_multiaperturedialog import Ui_MultiApertureDialog
from gui_py.ui_aperturedialog import Ui_ApertureDialog
from gui_py.ui_obsdialog import Ui_ObscurationDialog
from gui_py.ui_tiltdialog import Ui_TiltDialog
from gui_py.ui_viedialog import Ui_VieDialog
from gui_py.ui_plotdetaildialog import Ui_PlotDetailDialog
from gui_py.ui_surtypedialog import Ui_SurtypeDialog
from gui_py.ui_coatingdialog import Ui_CoatingDialog
from gui_py.ui_pivaxisdialog import Ui_PivaxisDialog
from gui_py.ui_glasslibdialog import Ui_GlassLibDialog
from gui_py.ui_stopdialog import Ui_StopDialog
from gui_py.ui_refdialog import Ui_RefDialog
from gui_py.ui_decdialog import Ui_DecDialog
from gui_py.ui_macrodialog import Ui_MacroDialog
from gui_py.ui_nssdialog import Ui_NssDialog
from gui_py.ui_toperdialog import Ui_ToperDialog


# Shared GUI helpers / data / dialogs for the Koko GUI split.

PLOT_TRIGGER_PREFIXES = (
    'VIE', 'SPD', 'CAPFN', 'PSF', 'DIST', 'FLDCV', 'AST', 'CHRSHIFT',
    'FANS', 'DRAW', 'DRAWFAN', 'GRAOUT', 'PLT', 'PLOT ', 'SPOT', 'DOTF',
    'GOTF', 'FAN', 'RAY', 'PARAX',
)

class CenterComboDelegate(QStyledItemDelegate):
    """Center-aligns the text of a (non-editable) QComboBox, both the
    current item shown in the box and the items in its drop-down popup.
    A non-editable QComboBox left-aligns its text, and making it editable
    just to center it breaks mouse interaction with the drop-down, so we
    use a delegate instead.
    """

    def paint(self, painter, option, index):
        option.displayAlignment = Qt.AlignmentFlag.AlignCenter
        super().paint(painter, option, index)

class CenterComboDelegate(QStyledItemDelegate):
    """Center-aligns the text of a (non-editable) QComboBox, both the
    current item shown in the box and the items in its drop-down popup.
    A non-editable QComboBox left-aligns its text, and making it editable
    just to center it breaks mouse interaction with the drop-down, so we
    use a delegate instead.
    """

    def paint(self, painter, option, index):
        option.displayAlignment = Qt.AlignmentFlag.AlignCenter
        super().paint(painter, option, index)

class StringDialog(QDialog):
    """Generic single-line string-input dialog. Subclasses set _ui_cls."""
    _ui_cls = None

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = self._ui_cls()
        self._ui.setupUi(self)

    def get_value(self):
        """Show dialog; return trimmed text on OK, or None."""
        if self.exec() == QDialog.DialogCode.Accepted:
            return self._ui.lineEdit.text().strip()
        return None

class RayDialog(QDialog, Ui_RayDialog):
    """Single-ray trace dialog (mirrors KDP2 IDD_RAY / RAYS.INC).

    The user enters normalized field (X,Y) coordinates. Two actions are
    offered:
      * "Trace"    -> FOB X Y + RAY + PRXYZ ALL (text output in msgView)
      * "Plot Fan" -> FANS XFAN (transverse-aberration fan graph)
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_RayDialog()
        self._ui.setupUi(self)
        self._mode = None
        self._ui.pushButton_trace.clicked.connect(
            lambda: self._accept("trace"))
        self._ui.pushButton_fan.clicked.connect(
            lambda: self._accept("fan"))

    def _accept(self, mode):
        self._mode = mode
        # validate inputs before accepting
        try:
            float(self._ui.lineEdit_x.text().strip() or "0.0")
            float(self._ui.lineEdit_y.text().strip() or "0.0")
        except ValueError:
            return
        self.accept()

    def get_values(self):
        """Show dialog; return (mode, x, y) or None on cancel."""
        if self.exec() == QDialog.DialogCode.Accepted:
            try:
                x = float(self._ui.lineEdit_x.text().strip() or "0.0")
                y = float(self._ui.lineEdit_y.text().strip() or "0.0")
            except ValueError:
                return None
            return (self._mode, x, y)
        return None

class PikupDialog(QDialog, Ui_PikupDialog):
    """Parameter-pickup dialog (mirrors KDP2 IDD_PIKSLV / IDD_PIKED1-3).

    The user enters a surface number, a pickup type (CV/RD/CC/TH/...), and a
    value; on accept we send, inside UPDATE LENS mode:
        U L
        PIKUP <TYPE>,<surface>,<value>
        EOS
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_PikupDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return (surface, type, value) on OK, or None."""
        if self.exec() == QDialog.DialogCode.Accepted:
            surf = self._ui.spin_surf.value()
            ptype = self._ui.combo_type.currentText()
            try:
                val = float(self._ui.lineEdit_val.text().strip() or "0.0")
            except ValueError:
                return None
            return (surf, ptype, val)
        return None

class SolveDialog(QDialog, Ui_SolveDialog):
    """Solve editor dialog (mirrors KDP2 IDD_SLVED).

    The user picks a surface, a solve family (PY/PX, PCY/PCX, ...), a
    plane (Y/X) and a target value; on accept we send, inside UPDATE
    LENS mode:
        U L
        CHG <surface>
        <SOLVE> <target>
        EOS
    The two utility buttons act immediately:
        PIKD    -> delete all pickups on the surface
        SLV ALL -> list all solves (text output)
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_SolveDialog()
        self._ui.setupUi(self)
        self._action = None
        self._ui.btn_pikd.clicked.connect(self._on_pikd)
        self._ui.btn_slvall.clicked.connect(self._on_slvall)

    def _on_pikd(self):
        self._action = "pikd"
        self.accept()

    def _on_slvall(self):
        self._action = "slvall"
        self.accept()

    def get_values(self):
        """Show dialog; return dict describing the action, or None."""
        if self.exec() != QDialog.DialogCode.Accepted:
            return None
        surf = self._ui.spin_surf.value()
        if self._action == "pikd":
            return dict(action="pikd", surf=surf)
        if self._action == "slvall":
            return dict(action="slvall")
        idx = self._ui.combo_type.currentIndex()
        label, ycmd, xcmd = SOLVE_TYPES[idx]
        plane_y = self._ui.radio_y.isChecked()
        cmd = ycmd if plane_y else xcmd
        try:
            val = float(self._ui.lineEdit_val.text().strip() or "0.0")
        except ValueError:
            return None
        return dict(action="solve", surf=surf, cmd=cmd, val=val)

class AsphDialog(QDialog, Ui_AsphDialog):
    """Aspheric / toric dialog (mirrors KDP2 IDD_ASPH / ASPH.INC).

    Aspheric mode sends, inside UPDATE LENS mode:
        U L -> CHG <surf> -> ASPH -> CHG <surf>
            -> CC <conic> -> AC/AD/AE/AF/AG/AH/AI/AJ/AK/AL <coeff> -> EOS
    Toric mode sends:
        U L -> CHG <surf> -> YTORIC|XTORIC -> RDTOR|CVTOR <val> -> EOS
        (plus CCTOR when the toric conic is non-zero)
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_AsphDialog()
        self._ui.setupUi(self)

    def get_values(self) -> dict | None:
        """Show dialog; return dict of values on OK, or None."""
        if self.exec() != QDialog.DialogCode.Accepted:
            return None
        surf = self._ui.spin_surf.value()
        mode = self._ui.combo_mode.currentIndex()
        try:
            if mode == 0:
                cc = float(self._ui.lineEdit_cc.text().strip() or "0.0")
                coeffs: dict = {}
                for cmd, (lab, edit) in self._ui.coeff_edits.items():
                    coeffs[cmd] = float(edit.text().strip() or "0.0")
                return dict(mode="asph", surf=surf, cc=cc, coeffs=coeffs)
            torval = float(self._ui.lineEdit_torval.text().strip() or "0.0")
            cctor = float(self._ui.lineEdit_cctor.text().strip() or "0.0")
            return dict(mode="toric", surf=surf,
                        toric="YTORIC" if mode == 1 else "XTORIC",
                        tormode="RDTOR"
                        if self._ui.combo_tormode.currentIndex() == 0
                        else "CVTOR",
                        torval=torval, cctor=cctor)
        except ValueError:
            return None
        return None

class GrtArrayDialog(QDialog, Ui_GrtArrayDialog):
    """Grating / array-lens dialog (mirrors KDP2 IDD_GRTARRAY / ARRAYGRT.INC).

    Grating assign sends:
        U L -> CHG <surf> -> GRT -> GRO,<v> -> GRS,<v> -> GRX,<v>
            -> GRY,<v> -> GRZ,<v> -> EOS
    Grating delete sends: U L -> CHG <surf> -> GRTD -> EOS
    Array assign sends: U L -> CHG <surf> -> ARRAY ODD|EVEN,<dx>,<dy> -> EOS
    Array delete sends: U L -> CHG <surf> -> ARRAYD -> EOS
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_GrtArrayDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return dict of values on OK, or None."""
        if self.exec() != QDialog.DialogCode.Accepted:
            return None
        surf = self._ui.spin_surf.value()
        gidx = self._ui.combo_grating.currentIndex()
        aidx = self._ui.combo_array.currentIndex()
        try:
            vals = {}
            vals["surf"] = surf
            if gidx == 1:
                vals["grating"] = "assign"
                vals["gro"] = float(self._ui.lineEdit_gro.text().strip() or "0.0")
                vals["grs"] = float(self._ui.lineEdit_grs.text().strip() or "0.0")
                vals["grx"] = float(self._ui.lineEdit_grx.text().strip() or "0.0")
                vals["gry"] = float(self._ui.lineEdit_gry.text().strip() or "0.0")
                vals["grz"] = float(self._ui.lineEdit_grz.text().strip() or "0.0")
            elif gidx == 2:
                vals["grating"] = "delete"
            else:
                vals["grating"] = None
            if aidx == 1:
                vals["array"] = "assign"
                vals["arraytype"] = self._ui.combo_arraytype.currentText()
                vals["dx"] = float(self._ui.lineEdit_dx.text().strip() or "0.0")
                vals["dy"] = float(self._ui.lineEdit_dy.text().strip() or "0.0")
            elif aidx == 2:
                vals["array"] = "delete"
            else:
                vals["array"] = None
            return vals
        except ValueError:
            return None

class SpsrfDialog(QDialog, Ui_SpsrfDialog):
    """Special-surface dialog (mirrors KDP2 IDD_SPSRF / SPSRF.INC).

    Sends: U SP -> SPECIAL,<surface>,<type> -> EOS
    koko supports special surface types 1 through 24.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_SpsrfDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return (surface, type) on OK, or None."""
        if self.exec() == QDialog.DialogCode.Accepted:
            return (self._ui.spin_surf.value(), self._ui.spin_type.value())
        return None

class BbDialog(QDialog, Ui_BbDialog):
    """Blackbody radiation dialog (mirrors KDP2 IDD_BB / GUICODE.FOR).

    Three independent computations share a units radio (WATTS/PHOTONS).
    Each Compute button first sends RADUNITS <unit>, then the matching
    CMD-level command; koko prints the result as text in the message
    view. The dialog stays open so several computations can be run.

      Wien             : RADUNITS <u> -> WIEN P,<T>
      Stefan-Boltzmann : RADUNITS <u> -> STEFBOLT P,<T>,<lam_up>,<lam_lo>
      Planck           : RADUNITS <u> -> PLANK P,<T>,<lambda>
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_BbDialog()
        self._ui.setupUi(self)
        ui = self._ui
        ui.btn_wien.clicked.connect(self._compute_wien)
        ui.btn_stef.clicked.connect(self._compute_stef)
        ui.btn_plank.clicked.connect(self._compute_plank)

    def _send(self, cmd):
        main = self.parent()
        send = getattr(main, "send_koko", None)
        if callable(send):
            send(cmd)

    def _units_cmd(self):
        return ("RADUNITS WATTS" if self._ui.radio_watts.isChecked()
                else "RADUNITS PHOTONS")

    def _compute_wien(self):
        ui = self._ui
        try:
            t = float(ui.lineEdit_wien_t.text().strip() or "0.0")
        except ValueError:
            return
        if t <= 0.0:
            return
        self._send(self._units_cmd())
        self._send("WIEN P,%s" % repr(t))

    def _compute_stef(self):
        ui = self._ui
        try:
            t = float(ui.lineEdit_stef_t.text().strip() or "0.0")
            lu = float(ui.lineEdit_stef_lu.text().strip() or "0.0")
            ll = float(ui.lineEdit_stef_ll.text().strip() or "0.0")
        except ValueError:
            return
        if t <= 0.0 or lu <= 0.0 or ll < 0.0 or ll >= lu:
            return
        self._send(self._units_cmd())
        self._send("STEFBOLT P,%s,%s,%s" % (repr(t), repr(lu), repr(ll)))

    def _compute_plank(self):
        ui = self._ui
        try:
            t = float(ui.lineEdit_plank_t.text().strip() or "0.0")
            lam = float(ui.lineEdit_plank_l.text().strip() or "0.0")
        except ValueError:
            return
        if t <= 0.0 or lam <= 0.0:
            return
        self._send(self._units_cmd())
        self._send("PLANK P,%s,%s" % (repr(t), repr(lam)))

class RayAuxDialog(QDialog, Ui_RayAuxDialog):
    """Ray-settings / analysis-aux dialog.

    Bundles four KDP2 dialogs that koko exposes as CMD-level text
    commands; results print in the message view and the dialog stays
    open for repeat runs.

      RAYSETTINGS : SURTOL / AIMTOL / CAIMTOL / NRAITR  (PM get/set)
      FIRD        : FIRD,NW1,NW2                        (paraxial EFL/BFL/FFL)
      ISTAT       : FOB -> SPD ISTAT|IPSTAT,<J>,<start>,<end>,<del>
      FAIL        : FOB -> SPD <grid> -> FAIL|FAILACC,<s1>,<s2>
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_RayAuxDialog()
        self._ui.setupUi(self)
        ui = self._ui
        ui.btn_rayset.clicked.connect(self._apply_rayset)
        ui.btn_fird.clicked.connect(self._compute_fird)
        ui.btn_istat.clicked.connect(self._compute_istat)
        ui.btn_fail.clicked.connect(self._compute_fail)

    def _send(self, cmd):
        main = self.parent()
        send = getattr(main, "send_koko", None)
        if callable(send):
            send(cmd)

    def _apply_rayset(self):
        ui = self._ui
        pairs = (
            ("SURTOL", ui.lineEdit_surtol.text().strip()),
            ("AIMTOL", ui.lineEdit_aimtol.text().strip()),
            ("CAIMTOL", ui.lineEdit_caimtol.text().strip()),
            ("NRAITR", ui.lineEdit_nraitr.text().strip()),
        )
        sent = False
        for name, val in pairs:
            if val:
                try:
                    float(val)
                except ValueError:
                    continue
                self._send("%s %s" % (name, val))
                sent = True
            else:
                # blank -> print current value
                self._send(name)
                sent = True
        if not sent:
            for name, _ in pairs:
                self._send(name)

    def _compute_fird(self):
        ui = self._ui
        s1 = ui.lineEdit_fird_s1.text().strip()
        s2 = ui.lineEdit_fird_s2.text().strip()
        if s1 and s2:
            self._send("FIRD,%s,%s" % (s1, s2))
        elif s1:
            self._send("FIRD,%s" % s1)
        else:
            self._send("FIRD")

    def _compute_istat(self):
        ui = self._ui
        try:
            start = float(ui.lineEdit_istat_start.text().strip() or "0.0")
            end = float(ui.lineEdit_istat_end.text().strip() or "90.0")
            step = float(ui.lineEdit_istat_step.text().strip() or "10.0")
        except ValueError:
            return
        if step <= 0.0 or end <= start:
            return
        j = 1 if ui.combo_istat_type.currentIndex() == 0 else 2
        qual = "ISTAT" if j == 1 else "IPSTAT"
        self._send("FOB")
        self._send("SPD %s,%d,%s,%s,%s" % (
            qual, j, repr(start), repr(end), repr(step)))

    def _compute_fail(self):
        ui = self._ui
        s1 = ui.lineEdit_fail_s1.text().strip()
        s2 = ui.lineEdit_fail_s2.text().strip()
        cmd = "FAILACC" if ui.check_failacc.isChecked() else "FAIL"
        self._send("FOB")
        self._send("SPOT RING")
        self._send("SPD")
        if s1 and s2:
            self._send("%s,%s,%s" % (cmd, s1, s2))
        elif s1:
            self._send("%s,%s" % (cmd, s1))
        else:
            self._send(cmd)

class FldcvDialog(QDialog, Ui_FldcvDialog):
    """Field curvature settings (split from KDP2 IDD_DISAST). OK sends
    FLDCV,<orient>,1,<n> and, if the plot box is ticked, PLTFLDCV
    through the main window's slot_plot so the graph renders."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_FldcvDialog()
        self._ui.setupUi(self)

    @staticmethod
    def _orient(idx):
        return "0" if idx == 0 else "90"

    def commands(self):
        ui = self._ui
        # word #2 is the field factor; send it explicitly (1.0) because an
        # empty word parses as 0.0, not the 1.0 default, and a 0 factor
        # yields a degenerate plot with no labels.
        cmds = ["FLDCV,%s,1,%d" % (self._orient(ui.combo_orient.currentIndex()),
                                   ui.spin_n.value())]
        if ui.check_plot.isChecked():
            cmds.append("PLTFLDCV")
        return cmds

class AstDialog(QDialog, Ui_AstDialog):
    """Astigmatism settings (split from KDP2 IDD_DISAST). OK sends
    AST,<orient>,1,<n> and, if the plot box is ticked, PLTAST."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_AstDialog()
        self._ui.setupUi(self)

    @staticmethod
    def _orient(idx):
        return "0" if idx == 0 else "90"

    def commands(self):
        ui = self._ui
        # word #2 is the field factor; send it explicitly (1.0) -- see
        # FldcvDialog.commands for why the empty word is unsafe.
        cmds = ["AST,%s,1,%d" % (self._orient(ui.combo_orient.currentIndex()),
                                 ui.spin_n.value())]
        if ui.check_plot.isChecked():
            cmds.append("PLTAST")
        return cmds

class DistDialog(QDialog, Ui_DistDialog):
    """Distortion settings (split from KDP2 IDD_DISAST). OK sends
    DIST/FISHDIST,<orient>,,<n> and, if the plot box is ticked,
    PLTDIST,,1 / PLTFDIST,,1."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_DistDialog()
        self._ui.setupUi(self)

    @staticmethod
    def _orient(idx):
        return "0" if idx == 0 else "90"

    def commands(self):
        ui = self._ui
        fish = ui.combo_type.currentIndex() == 1
        base = "FISHDIST" if fish else "DIST"
        # word #2 is the field factor; send it explicitly (1.0) -- an
        # empty word parses as 0.0 and DIST then plots with no labels.
        cmds = ["%s,%s,1,%d" % (base,
                                self._orient(ui.combo_orient.currentIndex()),
                                ui.spin_n.value())]
        if ui.check_plot.isChecked():
            cmds.append("PLTFDIST" if fish else "PLTDIST")
        return cmds

class CapfnDialog(QDialog, Ui_CapfnDialog):
    """Complex pupil function settings (KDP2 IDD_CAPFN). Compute /
    analysis / plot buttons route through the main window's slot_plot
    (text listings go to the message view). Dialog stays open."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_CapfnDialog()
        self._ui.setupUi(self)
        ui = self._ui
        ui.btn_compute.clicked.connect(self._compute)
        ui.btn_capgrid.clicked.connect(lambda: self._wav_cmd("CAPGRID"))
        ui.btn_wamap.clicked.connect(lambda: self._wav_cmd("WAMAP"))
        ui.btn_amap.clicked.connect(lambda: self._wav_cmd("AMAP"))
        ui.btn_fitzern.clicked.connect(lambda: self._wav_cmd("FITZERN"))
        ui.btn_listopd.clicked.connect(lambda: self._send("LISTOPD"))
        ui.btn_listzern.clicked.connect(lambda: self._send("LISTZERN"))
        ui.btn_listrept.clicked.connect(lambda: self._send("LISTREPT"))
        ui.btn_plotopd.clicked.connect(lambda: self._plot_capfn("CAPFNOPD"))
        ui.btn_plotapd.clicked.connect(lambda: self._plot_capfn("CAPFNAPD"))
        ui.btn_contopd.clicked.connect(lambda: self._plot_con_capfn("CAPFNOPD"))
        ui.btn_contapd.clicked.connect(lambda: self._plot_con_capfn("CAPFNAPD"))
        ui.btn_out.clicked.connect(lambda: self._send("CAPFNOUT"))
        ui.btn_in.clicked.connect(lambda: self._send("CAPFNIN"))
        ui.btn_add.clicked.connect(lambda: self._send("CAPFNADD"))
        ui.btn_clr.clicked.connect(lambda: self._send("CAPFNCLR"))

    def _send(self, cmd):
        main = self.parent()
        fn = getattr(main, "send_koko", None)
        if callable(fn):
            fn(cmd)

    def _plot(self, *commands):
        main = self.parent()
        fn = getattr(main, "slot_plot", None)
        if callable(fn):
            fn(*commands)

    def _mode_cmd(self):
        return self._ui.combo_mode.currentText()

    def _compute(self):
        ui = self._ui
        nrd = ui.spin_nrd.value()
        if nrd % 2:
            nrd += 1
        self._send("CAPFNNRD,%d" % nrd)
        self._send(self._mode_cmd())

    def _wav_cmd(self, base):
        self._compute()
        self._send("%s,%d" % (base, self._ui.spin_wav.value()))

    def _plot_capfn(self, kind):
        ui = self._ui
        self._compute()
        self._send("CAPFNROT %s" % ("YES" if ui.check_rot.isChecked() else "NO"))
        wav = ui.spin_pltwav.value()
        lo = ui.lineEdit_min.text().strip()
        hi = ui.lineEdit_max.text().strip()
        if lo and hi:
            self._plot("PLOT %s,%d,1,%s,%s" % (kind, wav, lo, hi))
        else:
            self._plot("PLOT %s,%d,1" % (kind, wav))

    def _plot_con_capfn(self, kind):
        """Contour plot (PLOTCON CAPFNOPD/CAPFNAPD). KDP2 parity: PLOTCON
        takes no min/max (auto z-scale), only an optional numeric word #1
        = wavelength for the OPD variant. Sequence mirrors the CLI:
        compute -> PLOT NEW (plot mode) -> PLOTCON <kind>[,<wav>] -- the
        DRAW that PLTCAPCO queues is replayed by koko's command loop and
        writes drawcmd.gpl, which the main window polls and renders."""
        ui = self._ui
        self._compute()
        self._send("CAPFNROT %s" % ("YES" if ui.check_rot.isChecked() else "NO"))
        wav = ui.spin_pltwav.value()
        if kind == "CAPFNOPD":
            self._plot("PLOT NEW", "PLOTCON %s,%d" % (kind, wav))
        else:
            self._plot("PLOT NEW", "PLOTCON %s" % kind)

class SpotDialog(QDialog, Ui_SpotDialog):
    """Geometric spot-diagram settings (split from KDP2 IDD_SPOT /
    SPOTGUI.FOR). OK sends the ray-pattern / statistics setup, computes
    SPD[ ACC][,<wav>] and, if the plot box is ticked, PLTSPD through
    the main window's slot_plot so the graph renders. The spot-file
    buttons (SPDSAVE / SPDADD / SPDSTATS) act immediately."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_SpotDialog()
        self._ui.setupUi(self)
        ui = self._ui
        ui.btn_save.clicked.connect(lambda: self._send("SPDSAVE"))
        ui.btn_add.clicked.connect(lambda: self._send("SPDADD"))
        ui.btn_stats.clicked.connect(lambda: self._send("SPDSTATS"))

    def _send(self, cmd):
        main = self.parent()
        fn = getattr(main, "send_koko", None)
        if callable(fn):
            fn(cmd)

    def commands(self):
        ui = self._ui
        pidx = ui.combo_pattern.currentIndex()
        n = ui.spin_count.value()
        cmds = []
        if pidx == 0:
            cmds.append("SPOT RING")
            cmds.append("RINGS,%d" % n)
        elif pidx == 1:
            cmds.append("SPOT RECT")
            cmds.append("RECT,%d" % n)
        else:
            cmds.append("SPOT RAND")
            cmds.append("RANNUM,%d" % n)
        cmds.append("STATS FULL" if ui.combo_stats.currentIndex() == 0
                    else "STATS MIN")
        wav = ui.spin_wav.value()
        if ui.check_acc.isChecked():
            cmds.append("SPD ACC,%d" % wav if wav else "SPD ACC")
        else:
            cmds.append("SPD,%d" % wav if wav else "SPD")
        if ui.check_plot.isChecked():
            cmds.append("PLTSPD")
        return cmds

class PsfDialog(QDialog, Ui_PsfDialog):
    """PSF settings (split from KDP2 IDD_PSF). OK sends NRD,<n>,
    PSFWRITE/PSFPLOT toggles, then the bare mode command (PSF /
    PSF PERFECT / PSF PERFNOOB) + CAPFNOUT through the main window's
    slot_plot so the graph renders. No wavelength qualifier: it would
    land in the dflag slot and suppress the internal DRAW (no labels)."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_PsfDialog()
        self._ui.setupUi(self)

    def commands(self):
        ui = self._ui
        cmds = ["NRD,%d" % ui.spin_nrd.value(),
                "PSFWRITE %s" % ("YES" if ui.check_write.isChecked() else "NO"),
                "PSFPLOT %s" % ("YES" if ui.check_plot.isChecked() else "NO"),
                ui.combo_mode.currentText(),
                "CAPFNOUT"]
        return cmds

class DotfDialog(QDialog, Ui_DotfDialog):
    """Diffraction MTF settings (split from KDP2 IDD_DOTF). OK sends
    SPACE I/O, FAR/NEAR, DOTF, PLTDOTF[ LEICA],,1, DRAW."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_DotfDialog()
        self._ui.setupUi(self)

    def commands(self):
        ui = self._ui
        cmds = ["SPACE I" if ui.combo_space.currentIndex() == 0 else "SPACE O",
                "FAR" if ui.combo_range.currentIndex() == 0 else "NEAR",
                "DOTF",
                "PLTDOTF LEICA,,1" if ui.check_leica.isChecked() else "PLTDOTF,,1",
                "DRAW"]
        return cmds

class GotfDialog(QDialog, Ui_GotfDialog):
    """Geometric MTF settings (split from KDP2 IDD_GOTF). OK sends
    SPACE I/O, FAR/NEAR, GOTF, PLTGOTF[ LEICA],1, DRAW."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_GotfDialog()
        self._ui.setupUi(self)

    def commands(self):
        ui = self._ui
        cmds = ["SPACE I" if ui.combo_space.currentIndex() == 0 else "SPACE O",
                "FAR" if ui.combo_range.currentIndex() == 0 else "NEAR",
                "GOTF",
                "PLTGOTF LEICA,1" if ui.check_leica.isChecked() else "PLTGOTF,1",
                "DRAW"]
        return cmds

class LensOpsDialog(QDialog, Ui_LensOpsDialog):
    """Lens operations dialog (KDP2 item 7): FLIP / SCALE / ZERO /
    OTHER. All are CMD-level commands sent through the main window's
    send_koko. Dialog stays open so several operations can be run."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_LensOpsDialog()
        self._ui.setupUi(self)
        ui = self._ui
        ui.btn_flip.clicked.connect(self._do_flip)
        ui.btn_scale.clicked.connect(self._do_scale)
        ui.btn_zero.clicked.connect(self._do_zero)
        ui.btn_other.clicked.connect(self._do_other)

    def _send(self, cmd):
        main = self.parent()
        fn = getattr(main, "send_koko", None)
        if callable(fn):
            fn(cmd)

    def _do_flip(self):
        ui = self._ui
        s = ui.spin_flip_s.value()
        e = ui.spin_flip_e.value()
        if s >= e:
            return
        self._send("FLIP,%d,%d" % (s, e))

    def _do_scale(self):
        ui = self._ui
        tidx = ui.combo_scale_type.currentIndex()
        try:
            factor = float(ui.lineEdit_scale_f.text().strip() or "1.0")
        except ValueError:
            return
        if factor == 0.0:
            return
        s = ui.spin_scale_s.value()
        e = ui.spin_scale_e.value()
        if s >= e:
            return
        base = ["SC", "WSC", "SC FY", "WSC FY"][tidx]
        self._send("%s,%s,%d,%d" % (base, repr(factor), s, e))

    def _do_zero(self):
        ui = self._ui
        surf = ui.spin_zero_surf.value()
        self._send("U L")
        self._send("CHG %d" % surf)
        self._send("ZERO")
        self._send("EOS")
        self._send("RTG ALL")

    def _do_other(self):
        ui = self._ui
        surf = ui.spin_other_surf.value()
        self._send("U L")
        self._send("CHG %d" % surf)
        self._send(ui.combo_other_trace.currentText())
        self._send("FOOTBLOK %s" % ("YES" if ui.check_footblok.isChecked() else "NO"))
        self._send("NODUM %s" % ("YES" if ui.check_nodum.isChecked() else "NO"))
        spgr = ui.lineEdit_spgr.text().strip()
        if spgr:
            self._send("SPGR,%s" % spgr)
        price = ui.lineEdit_price.text().strip()
        if price:
            self._send("PRICE,%s" % price)
        inr = ui.lineEdit_inr.text().strip()
        if inr:
            self._send("INR,%s" % inr)
        rayerr = ui.lineEdit_rayerr.text().strip()
        if rayerr:
            self._send("RAYERROR,%s" % rayerr)
        lbl = ui.lineEdit_lbl.text().strip()
        if lbl:
            self._send("LBL,%s" % lbl)
        coat = ui.spin_coat.value()
        if coat > 0:
            self._send("COATING,%d" % coat)
        self._send("EOS")
        self._send("RTG ALL")

class MultiApertureDialog(QDialog, Ui_MultiApertureDialog):
    """Multiple apertures/obscurations dialog (KDP2 IDD_CLAPS /
    IDD_MCLAP / IDD_MCOBS). koko commands:
        MULTCLAP,<n>,<x>,<y>[,<gam>]   add aperture instance
        MULTCLAP DELETE                remove all on surface
        MULTCOBS,<n>,<x>,<y>[,<gam>]   add obscuration instance
        MULTCOBS DELETE                remove all on surface
    MULTCLAP requires a pre-existing CLAP; MULTCOBS requires COBS.
    Dialog stays open so several instances can be added."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_MultiApertureDialog()
        self._ui.setupUi(self)
        ui = self._ui
        ui.btn_clap_add.clicked.connect(lambda: self._add("MULTCLAP"))
        ui.btn_clap_del.clicked.connect(lambda: self._delete("MULTCLAP"))
        ui.btn_cobs_add.clicked.connect(lambda: self._add("MULTCOBS"))
        ui.btn_cobs_del.clicked.connect(lambda: self._delete("MULTCOBS"))

    def _send(self, cmd):
        main = self.parent()
        fn = getattr(main, "send_koko", None)
        if callable(fn):
            fn(cmd)

    def _add(self, kind):
        ui = self._ui
        surf = ui.spin_surf.value()
        if kind == "MULTCLAP":
            n = ui.spin_clap_n.value()
            xs, ys, gs = (ui.lineEdit_clap_x.text().strip(),
                          ui.lineEdit_clap_y.text().strip(),
                          ui.lineEdit_clap_gam.text().strip())
        else:
            n = ui.spin_cobs_n.value()
            xs, ys, gs = (ui.lineEdit_cobs_x.text().strip(),
                          ui.lineEdit_cobs_y.text().strip(),
                          ui.lineEdit_cobs_gam.text().strip())
        try:
            x = float(xs or "0.0")
            y = float(ys or "0.0")
        except ValueError:
            return
        self._send("U L")
        self._send("CHG %d" % surf)
        if gs:
            self._send("%s,%d,%s,%s,%s" % (kind, n, repr(x), repr(y), gs))
        else:
            self._send("%s,%d,%s,%s" % (kind, n, repr(x), repr(y)))
        self._send("EOS")
        self._send("RTG ALL")

    def _delete(self, kind):
        ui = self._ui
        surf = ui.spin_surf.value()
        self._send("U L")
        self._send("CHG %d" % surf)
        self._send("%s DELETE" % kind)
        self._send("EOS")
        self._send("RTG ALL")

class ApertureDialog(QDialog, Ui_ApertureDialog):
    """Clear-aperture (CLAP) dialog (mirrors KDP2 IDD_APECIRC / IDD_APERECT /
    IDD_APEELIP / IDD_APERCTK).

    The user picks a shape (circular / rectangular / elliptical / rectangular
    with frame) and enters the corresponding parameters; on accept we send,
    inside UPDATE LENS mode:
        U L
        CHG <surface>
        CLAP <shape> <params...>      (circular: CLAP R XDEC YDEC 0 0)
        [CLAP TILT <angle>]           (rect/elip/rctk only)
        EOS
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_ApertureDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return dict of values on OK, or None."""
        if self.exec() == QDialog.DialogCode.Accepted:
            shape = self._ui.combo_shape.currentText()
            try:
                surf = self._ui.spin_surf.value()
                if shape == "Delete all (CLAPD)":
                    return dict(shape=shape, surf=surf)
                xdec = float(self._ui.lineEdit_xdec.text().strip() or "0.0")
                ydec = float(self._ui.lineEdit_ydec.text().strip() or "0.0")
                if shape in ("Circular", "Erase region"):
                    rad = float(self._ui.lineEdit_rad.text().strip() or "0.0")
                    return dict(shape=shape, surf=surf, rad=rad,
                                xdec=xdec, ydec=ydec)
                if shape == "Polygonal":
                    rad = float(self._ui.lineEdit_rad.text().strip() or "0.0")
                    nsides = self._ui.spin_nsides.value()
                    tilt = float(self._ui.lineEdit_tilt.text().strip() or "0.0")
                    return dict(shape=shape, surf=surf, rad=rad,
                                nsides=nsides, xdec=xdec, ydec=ydec,
                                tilt=tilt)
                tilt = float(self._ui.lineEdit_tilt.text().strip() or "0.0")
                hx = float(self._ui.lineEdit_hx.text().strip() or "0.0")
                hy = float(self._ui.lineEdit_hy.text().strip() or "0.0")
                if shape == "Rectangular + Frame":
                    fr = float(self._ui.lineEdit_fr.text().strip() or "0.0")
                    return dict(shape=shape, surf=surf, hx=hx, hy=hy,
                                xdec=xdec, ydec=ydec, tilt=tilt, fr=fr)
                return dict(shape=shape, surf=surf, hx=hx, hy=hy,
                            xdec=xdec, ydec=ydec, tilt=tilt)
            except ValueError:
                return None
        return None

class ObscurationDialog(QDialog, Ui_ObscurationDialog):
    """Clear-obscuration (COBS) dialog (mirrors KDP2 IDD_APECIRC2 /
    IDD_APERECT2 / IDD_APEELIP2).

    The user picks a shape (circular / rectangular / elliptical) and enters
    the corresponding parameters; on accept we send, inside UPDATE LENS mode:
        U L
        CHG <surface>
        COBS <shape> <params...>      (circular: COBS R YDEC XDEC)
        [COBS TILT <angle>]           (rect/elip only)
        EOS
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_ObscurationDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return dict of values on OK, or None."""
        if self.exec() == QDialog.DialogCode.Accepted:
            shape = self._ui.combo_shape.currentText()
            try:
                surf = self._ui.spin_surf.value()
                if shape == "Delete all (COBSD)":
                    return dict(shape=shape, surf=surf)
                xdec = float(self._ui.lineEdit_xdec.text().strip() or "0.0")
                ydec = float(self._ui.lineEdit_ydec.text().strip() or "0.0")
                if shape in ("Circular", "Erase region"):
                    rad = float(self._ui.lineEdit_rad.text().strip() or "0.0")
                    return dict(shape=shape, surf=surf, rad=rad,
                                xdec=xdec, ydec=ydec)
                if shape == "Polygonal":
                    rad = float(self._ui.lineEdit_rad.text().strip() or "0.0")
                    nsides = self._ui.spin_nsides.value()
                    tilt = float(self._ui.lineEdit_tilt.text().strip() or "0.0")
                    return dict(shape=shape, surf=surf, rad=rad,
                                nsides=nsides, xdec=xdec, ydec=ydec,
                                tilt=tilt)
                tilt = float(self._ui.lineEdit_tilt.text().strip() or "0.0")
                hx = float(self._ui.lineEdit_hx.text().strip() or "0.0")
                hy = float(self._ui.lineEdit_hy.text().strip() or "0.0")
                if shape == "Rectangular + Frame":
                    fr = float(self._ui.lineEdit_fr.text().strip() or "0.0")
                    return dict(shape=shape, surf=surf, hx=hx, hy=hy,
                                xdec=xdec, ydec=ydec, tilt=tilt, fr=fr)
                return dict(shape=shape, surf=surf, hx=hx, hy=hy,
                            xdec=xdec, ydec=ydec, tilt=tilt)
            except ValueError:
                return None
        return None

class TiltDialog(QDialog, Ui_TiltDialog):
    """Surface-tilt (TILT) dialog (mirrors KDP2 IDD_TILTS / IDD_TILT /
    IDD_TILTAUTO / IDD_TILTBEN / IDD_TILTRET / IDD_TILTDAR / IDD_TILTREV).

    The user picks a tilt type and (for basic tilt) the alpha/beta/gamma
    angles; on accept we send, inside UPDATE LENS mode:
        U L
        CHG <surface>
        TILT <a> <b> <g>          (basic)
        TILT AUTO | TILT DARD | TILT BEND | TILT REV   (special)
        RTILT                     (reverse)
        TILTD                     (delete)
        EOS
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_TiltDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return dict of values on OK, or None."""
        if self.exec() == QDialog.DialogCode.Accepted:
            ttype = self._ui.combo_type.currentText()
            try:
                surf = self._ui.spin_surf.value()
                if ttype.startswith("Basic"):
                    a = float(self._ui.lineEdit_alpha.text().strip() or "0.0")
                    b = float(self._ui.lineEdit_beta.text().strip() or "0.0")
                    g = float(self._ui.lineEdit_gamma.text().strip() or "0.0")
                    return dict(ttype=ttype, surf=surf,
                                alpha=a, beta=b, gamma=g)
                return dict(ttype=ttype, surf=surf)
            except ValueError:
                return None
        return None

class VieDialog(QDialog, Ui_VieDialog):
    """View-control (VIE) dialog (mirrors KDP2 IDD_VIE / LENSED.INC).

    The user picks a view (XZ/XY/ORTHO) and an optional scale factor, plus
    vignetting/symmetric display toggles; on accept we send:
        [VIEVIG ON|OFF]
        [VIESYM ON|OFF]
        VIE <type>,<factor>
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_VieDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return dict of values on OK, or None."""
        if self.exec() == QDialog.DialogCode.Accepted:
            try:
                vtype = self._ui.combo_type.currentText()
                factor = float(self._ui.lineEdit_factor.text().strip() or "0.10")
                vig = self._ui.check_vig.isChecked()
                sym = self._ui.check_sym.isChecked()
                return dict(vtype=vtype, factor=factor, vig=vig, sym=sym)
            except ValueError:
                return None
        return None

class PlotDetailDialog(QDialog, Ui_PlotDetailDialog):
    """Plot overlay control dialog (PLOT FRAME / AXIS / NOTE / PEN / UPLOT).

    These KDP2 PLOTCAD commands modify the current plot buffer; they are
    sent after a base plot (e.g. VIE XZ) and the slot finishes with DRAW
    so drawcmd.gpl is regenerated with the overlays included.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_PlotDetailDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return dict of values on OK, or None."""
        if self.exec() != QDialog.DialogCode.Accepted:
            return None
        vals = {}
        # Frame / Axis tab
        vals["frame"] = self._ui.check_frame.isChecked()
        if vals["frame"] and not self._ui.check_use_default_frame.isChecked():
            vals["frame_coords"] = self._ui.lineEdit_fcoords.text().strip()
        else:
            vals["frame_coords"] = None
        vals["axis"] = self._ui.check_axis.isChecked()
        # Note tab
        vals["pnote"] = self._ui.lineEdit_pnote.text().strip()
        vals["note"] = self._ui.check_note.isChecked()
        vals["note_x"] = self._ui.spin_note_x.value()
        vals["note_y"] = self._ui.spin_note_y.value()
        # Pen tab
        vals["pen"] = self._ui.check_pen.isChecked()
        vals["pen_x"] = self._ui.spin_pen_x.value()
        vals["pen_y"] = self._ui.spin_pen_y.value()
        vals["pen_state"] = self._ui.combo_pen_state.currentIndex() + 1
        # User plot tab
        vals["uplot"] = self._ui.check_uplot.isChecked()
        vals["uxr1"] = self._ui.spin_uxr1.value()
        vals["uxr2"] = self._ui.spin_uxr2.value()
        vals["uyr1"] = self._ui.spin_uyr1.value()
        vals["uyr2"] = self._ui.spin_uyr2.value()
        return vals

class SurtypeDialog(QDialog, Ui_SurtypeDialog):
    """Surface-type (SURTYPE) query dialog.

    koko's SURTYPE is a display command (KDP2 has no SURTYPE *setting*
    dialog either):
        SURTYPE <surface>   -> prints REAL / PARAXIAL for that surface
        SURTYPE ALL         -> prints the whole surface-type table
    The output is shown in the message view.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_SurtypeDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return dict of values on OK, or None."""
        if self.exec() == QDialog.DialogCode.Accepted:
            all_surfs = self._ui.check_all.isChecked()
            if all_surfs:
                return dict(all_surfs=True, surf=None)
            return dict(all_surfs=False,
                        surf=self._ui.spin_surf.value())
        return None

class CoatingDialog(QDialog, Ui_CoatingDialog):
    """Surface-coating (COATING) dialog.

    koko's COATING command (inside UPDATE LENS mode):
        CHG <surface>
        COATING <n>      (set coating index; 0 = no coating)
        COATING ?        (display current coating number)
    Mirrors the original KDP2 COATING command.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_CoatingDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return dict of values on OK, or None."""
        if self.exec() == QDialog.DialogCode.Accepted:
            show_only = self._ui.check_show.isChecked()
            if show_only:
                return dict(show_only=True,
                            surf=self._ui.spin_surf.value())
            return dict(show_only=False,
                        surf=self._ui.spin_surf.value(),
                        index=self._ui.spin_index.value())
        return None

class PivaxisDialog(QDialog, Ui_PivaxisDialog):
    """Pivot-axis (PIVAXIS) dialog (mirrors KDP2 IDD_PIVAX).

    The user picks a mode (NORMAL or VERTEX with explicit coordinates);
    on accept we send, inside UPDATE LENS mode:
        U L
        CHG <surface>
        PIVAXIS NORMAL                      (NORMAL mode)
        PIVAXIS VERTEX + PIVOT,X,Y,Z       (VERTEX mode)
        EOS
    koko also supports "PIVAXIS ?" to display the current setting.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_PivaxisDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return dict of values on OK, or None."""
        if self.exec() == QDialog.DialogCode.Accepted:
            show_only = self._ui.check_show.isChecked()
            if show_only:
                return dict(show_only=True,
                            surf=self._ui.spin_surf.value())
            mode = self._ui.combo_mode.currentText()
            try:
                surf = self._ui.spin_surf.value()
                if mode.startswith("NORMAL"):
                    return dict(show_only=False, surf=surf, mode="NORMAL")
                x = float(self._ui.lineEdit_x.text().strip() or "0.0")
                y = float(self._ui.lineEdit_y.text().strip() or "0.0")
                z = float(self._ui.lineEdit_z.text().strip() or "0.0")
                return dict(show_only=False, surf=surf, mode="VERTEX",
                            x=x, y=y, z=z)
            except ValueError:
                return None
        return None

class GlassLibDialog(QDialog, Ui_GlassLibDialog):
    """Lens-library (LIB) dialog (mirrors KDP2 IDD_LLIB subset koko supports):
        LIB GET <n>   -> restore library lens n
        LIB PUT <n>   -> store current lens into slot n
        LIB DEL <n>   -> delete library slot n
    koko does not support LIB REST / LIB SAVE / LIB LIST.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_GlassLibDialog()
        self._ui.setupUi(self)

    def get_values(self):
        """Show dialog; return dict of values on OK, or None."""
        if self.exec() == QDialog.DialogCode.Accepted:
            op = self._ui.combo_op.currentText()
            slot = self._ui.spin_slot.value()
            if op.startswith("Get"):
                return dict(op="GET", slot=slot)
            if op.startswith("Put"):
                return dict(op="PUT", slot=slot)
            if op.startswith("Delete"):
                return dict(op="DEL", slot=slot)
        return None

class StopDialog(QDialog, Ui_StopDialog):
    """Aperture-stop (ASTOP) dialog (mirrors KDP2 IDD_STOPSURF).

    koko sets the stop on the currently-CHG'd surface:
        U L -> CHG <surf> -> ASTOP[ EN|EX|ENEX] -> EOS
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_StopDialog()
        self._ui.setupUi(self)

    def get_values(self):
        if self.exec() == QDialog.DialogCode.Accepted:
            surf = self._ui.spin_surf.value()
            adj = self._ui.combo_adj.currentText()
            if adj.startswith("None"):
                qual = ""
            elif adj.startswith("Entrance"):
                qual = " EN"
            elif adj.startswith("Exit"):
                qual = " EX"
            else:
                qual = " ENEX"
            return dict(surf=surf, qual=qual)
        return None

class RefDialog(QDialog, Ui_RefDialog):
    """Reference-surface (REFS) dialog (mirrors KDP2 IDD_REFSSURF).

    koko: U L -> CHG <surf> -> REFS <rotation> -> EOS
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_RefDialog()
        self._ui.setupUi(self)

    def get_values(self):
        if self.exec() == QDialog.DialogCode.Accepted:
            try:
                surf = self._ui.spin_surf.value()
                rot = float(self._ui.lineEdit_rot.text().strip() or "0.0")
                return dict(surf=surf, rot=rot)
            except ValueError:
                return None
        return None

class DecDialog(QDialog, Ui_DecDialog):
    """Decenter (DEC) dialog (mirrors KDP2 IDD_DEC).

    koko: U L -> CHG <surf> -> DEC <x> <y> <z> -> EOS
    (KDP2 uses DEC,Y,X,Z order; we expose X/Y/Z to the user.)
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_DecDialog()
        self._ui.setupUi(self)

    def get_values(self):
        if self.exec() == QDialog.DialogCode.Accepted:
            try:
                surf = self._ui.spin_surf.value()
                x = float(self._ui.lineEdit_x.text().strip() or "0.0")
                y = float(self._ui.lineEdit_y.text().strip() or "0.0")
                z = float(self._ui.lineEdit_z.text().strip() or "0.0")
                return dict(surf=surf, x=x, y=y, z=z)
            except ValueError:
                return None
        return None

class MacroDialog(QDialog, Ui_MacroDialog):
    """Macro-library (MACRO) dialog.

    koko's macro library lives in $HOME/KODS/LIBMAC/MAC.DAT and must be
    initialized once with IMF + PROCEED. Then:
        MACRO  <name>  -> run macro
        MDEL   <name>  -> delete macro
        MACED  <name>  -> enter mac> edit mode (MACSAVE to store)
    The init button is enabled only when the library file is missing.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_MacroDialog()
        self._ui.setupUi(self)

    def get_values(self):
        if self.exec() == QDialog.DialogCode.Accepted:
            name = self._ui.lineEdit_name.text().strip()
            op = self._ui.combo_op.currentText()
            if not name:
                return None
            if op.startswith("Run"):
                return dict(op="RUN", name=name)
            if op.startswith("Delete"):
                return dict(op="DEL", name=name)
            if op.startswith("Edit"):
                return dict(op="EDIT", name=name)
        return None

class NssDialog(QDialog, Ui_NssDialog):
    """Non-sequential (NSS) database dialog.

    koko implements NSS fully: NSSNEW creates the in-memory database,
    after which NSSUNITS/NSSWV/UNIVERSE/OBJECT/ONAME/NSSSAVE/NSSREST/
    NSSTRACE/NSSLIST/NSSDEL all work. This dialog wires those commands
    to buttons (mirrors KDP2 NSS-menu intent).
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_NssDialog()
        self._ui.setupUi(self)
        ui = self._ui
        ui.btn_new.clicked.connect(lambda: self._send("NSSNEW"))
        ui.btn_apply.clicked.connect(self._apply_settings)
        ui.btn_object.clicked.connect(self._define_object)
        ui.btn_trace.clicked.connect(lambda: self._send("NSSTRACE"))
        ui.btn_list.clicked.connect(lambda: self._send("NSSLIST"))
        ui.btn_save.clicked.connect(self._save)
        ui.btn_rest.clicked.connect(self._restore)
        ui.btn_del.clicked.connect(lambda: self._send("NSSDEL"))

    def _send(self, cmd):
        main = self.parent()
        if main is not None and hasattr(main, "send_koko"):
            main.send_koko(cmd)

    def _apply_settings(self):
        ui = self._ui
        units = ui.combo_units.currentText()
        try:
            wv = float(ui.lineEdit_wv.text().strip() or "0.55")
            uni = float(ui.lineEdit_uni.text().strip() or "100.0")
        except ValueError:
            return
        self._send("NSSUNITS %s" % units)
        self._send("NSSWV %s" % repr(wv))
        self._send("UNIVERSE %s" % repr(uni))

    def _define_object(self):
        ui = self._ui
        name = ui.lineEdit_oname.text().strip() or "OBJ1"
        self._send("OBJECT")
        self._send("ONAME %s" % name)

    def _save(self):
        fname = self._ui.lineEdit_file.text().strip()
        if fname:
            self._send("NSSSAVE %s" % fname)

    def _restore(self):
        fname = self._ui.lineEdit_file.text().strip()
        if fname:
            self._send("NSSREST %s" % fname)

class ToperDialog(QDialog, Ui_ToperDialog):
    """Tolerancing (TOPER/TVAR) dialog.

    koko implements tolerancing with a multi-mode flow:
      TVAR  -> tvb> mode -> define tolerance VARIABLES (TH/RD_FR/CV_FR/...)
      TOPER -> top> mode -> define tolerance OPERANDS (FUNCxx / built-ins)
      SENSI / MONTE -> run sensitivity / Monte-Carlo analysis
    Verified via PTY: TVAR + TOPER + SENSI produces a full report.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self._ui = Ui_ToperDialog()
        self._ui.setupUi(self)
        ui = self._ui
        ui.btn_addvar.clicked.connect(self._add_var)
        ui.btn_delvar.clicked.connect(
            lambda: ui.table_vars.removeRow(ui.table_vars.currentRow())
            if ui.table_vars.currentRow() >= 0 else None)
        ui.btn_addop.clicked.connect(self._add_op)
        ui.btn_delop.clicked.connect(
            lambda: ui.table_ops.removeRow(ui.table_ops.currentRow())
            if ui.table_ops.currentRow() >= 0 else None)
        ui.btn_setup.clicked.connect(self._setup)
        ui.btn_sensi.clicked.connect(lambda: self._send("SENSI"))
        ui.btn_monte.clicked.connect(lambda: self._send("MONTE"))

    def _send(self, cmd):
        main = self.parent()
        if main is not None and hasattr(main, "send_koko"):
            main.send_koko(cmd)

    def _add_var(self):
        ui = self._ui
        vtype = ui.combo_vtype.currentText()
        surf = ui.spin_vsurf.value()
        try:
            delta = float(ui.line_vdelta.text().strip() or "0.01")
        except ValueError:
            return
        row = ui.table_vars.rowCount()
        ui.table_vars.insertRow(row)
        ui.table_vars.setItem(row, 0, QTableWidgetItem(vtype))
        ui.table_vars.setItem(row, 1, QTableWidgetItem(str(surf)))
        ui.table_vars.setItem(row, 2, QTableWidgetItem(repr(delta)))

    def _add_op(self):
        ui = self._ui
        op = ui.combo_op.currentText()
        args = ui.line_opargs.text().strip() or "1 1"
        row = ui.table_ops.rowCount()
        ui.table_ops.insertRow(row)
        ui.table_ops.setItem(row, 0, QTableWidgetItem(op))
        ui.table_ops.setItem(row, 1, QTableWidgetItem(args))

    def _setup(self):
        ui = self._ui
        grid = ui.spin_grid.value()
        self._send("TOLNRD %d" % grid)
        self._send("TVAR")
        for r in range(ui.table_vars.rowCount()):
            vtype = ui.table_vars.item(r, 0).text()
            surf = ui.table_vars.item(r, 1).text()
            delta = ui.table_vars.item(r, 2).text()
            self._send("%s %s %s" % (vtype, surf, delta))
        self._send("EOS")
        self._send("TOPER")
        for r in range(ui.table_ops.rowCount()):
            args = ui.table_ops.item(r, 1).text()
            self._send("FUNC00 %s" % args)
        self._send("EOS")

class LIDialog(StringDialog):
    _ui_cls = Ui_LIDialog

class NewDialog(StringDialog):
    _ui_cls = Ui_NewDialog

class RayInputDialog(StringDialog):
    _ui_cls = Ui_rayinputDialog

class OptimizeDialog(StringDialog):
    _ui_cls = Ui_Optimize

    def apply_commands(self):
        """Return the koko command sequence that defines the optimization
        variables and the default merit function (EFL target), mirroring the
        original IDD_VARED / FLCLTH / VARIABLES flow.

        Verified command sequence (tested against koko-cli):
          MERIT            -> enter merit-creation level (F27=1), resets OPCNT
          FLCLTH <target>  -> add focal-length operand (target = EFL in mm);
                               surface range defaults to the whole lens
          EOS              -> leave merit-creation level (operand is committed)
          VARIABLES        -> enter variable-definition level
          <CV 1> etc.      -> one variable spec per line
          EOS              -> leave variable level
          VB               -> turn the variable block ON
        The ITER run is left to the Optimizer dialog so the user controls
        when/how many cycles to execute (avoids an uncontrolled ITER FULL
        that can crash koko when variables/operands are still empty).
        """
        efl = self._ui.lineEdit_efl.text().strip()
        var_text = self._ui.plainEdit_var.toPlainText().strip()
        # Normalize the EFL target: a bare number is the target; if the user
        # typed something else, just pass it through.
        try:
            float(efl)
        except ValueError:
            efl = "0.0"
        cmds = [
            "MERIT",
            "FLCLTH %s" % efl,
            "EOS",
            "VARIABLES",
        ]
        # Each non-empty line is one variable spec (e.g. "CV 1", "TH 3").
        for line in var_text.splitlines():
            line = line.strip()
            if line:
                cmds.append(line)
        cmds.extend([
            "EOS",
            "VB",
        ])
        return cmds

class OptimizeRunDialog(QDialog, Ui_OptimizeDialog):
    """Optimization run dialog (mirrors original IDD_OPTIM).

    Each button forwards the corresponding koko command:
      SET DAMPING FACTOR -> PFAC (Meiron damping factor, local var)
      FIND BEST DAMPING  -> PFIND,<CY>,<CF>
      RESTORE / RESTORE MIN / RESTORE ORIG -> RESTORE / RESTORE MIN / RESTORE ORIG
      ITER     -> ITER,<NITER>
      ITER FULL -> ITER FULL,<NITERFULL>
      ITER POWELL -> IT P,<NITERP>
      PERFORM ROBB -> ROBB,<BETA>,<DELTA>,<NROBB>
    The Verbose checkbox toggles OVERBOSE YES/NO before the command.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self._wire()

    def _wire(self):
        self.pushButton_setPfac.clicked.connect(self._on_set_pfac)
        self.pushButton_pfind.clicked.connect(self._on_pfind)
        self.pushButton_rest1.clicked.connect(
            lambda: self._send_verbose("RESTORE"))
        self.pushButton_rest2.clicked.connect(
            lambda: self._send_verbose("RESTORE MIN"))
        self.pushButton_rest3.clicked.connect(
            lambda: self._send_verbose("RESTORE ORIG"))
        self.pushButton_iter.clicked.connect(self._on_iter)
        self.pushButton_iterfull.clicked.connect(self._on_iterfull)
        self.pushButton_iterp.clicked.connect(self._on_iterp)
        self.pushButton_robb.clicked.connect(self._on_robb)
        self.pushButton_exit.clicked.connect(self.reject)
        self.pushButton_varEditor.clicked.connect(self._on_open_var_editor)

    def _on_open_var_editor(self):
        """Open the variable/operand editor (original IDD_VARED flow) from
        within the Optimizer dialog, so variables and the default merit
        function can be defined before running ITER / PFIND / etc."""
        mw = self.parent()
        if not hasattr(mw, "send_koko"):
            return
        dlg = OptimizeDialog(self)
        if dlg.exec() == QDialog.DialogCode.Accepted:
            for cmd in dlg.apply_commands():
                mw.send_koko(cmd)

    def _verbose_prefix(self):
        """Return the OVERBOSE command for the current checkbox state."""
        if self.checkBox_verbose.isChecked():
            return "OVERBOSE YES"
        return "OVERBOSE NO"

    def _send_verbose(self, cmd):
        mw = self.parent()
        if hasattr(mw, "send_koko"):
            mw.send_koko(self._verbose_prefix())
            mw.send_koko(cmd)

    def _on_set_pfac(self):
        mw = self.parent()
        if not hasattr(mw, "send_koko"):
            return
        txt = self.lineEdit_pfac.text().strip()
        try:
            val = float(txt)
        except ValueError:
            return
        # Mirror original IDD_OPTIM / IDF_MEIRON: PFAC is a local damping
        # factor held by the dialog, NOT a koko command. The original only
        # does WRITE(OUTLYNE,*) 'PFAC RESET TO: <val>' + SHOWIT and then
        # PFAC=<val>; it never sends anything to koko. So we just echo it
        # to the message view, not to the engine.
        mw.append_msg("PFAC RESET TO: %s" % repr(val))
        mw.append_msg("PFAC = %s" % repr(val))

    def _on_pfind(self):
        mw = self.parent()
        if not hasattr(mw, "send_koko"):
            return
        cy = self.spinBox_cy.value()
        cf = self.lineEdit_cf.text().strip()
        try:
            cfv = float(cf)
        except ValueError:
            cfv = 0.6
        mw.send_koko(self._verbose_prefix())
        mw.send_koko("PFIND,%d,%s" % (cy, repr(cfv)))

    def _on_iter(self):
        mw = self.parent()
        if not hasattr(mw, "send_koko"):
            return
        n = self.spinBox_niter.value()
        mw.send_koko(self._verbose_prefix())
        mw.send_koko("ITER,%d" % n)

    def _on_iterfull(self):
        mw = self.parent()
        if not hasattr(mw, "send_koko"):
            return
        n = self.spinBox_niterfull.value()
        mw.send_koko(self._verbose_prefix())
        mw.send_koko("ITER FULL,%d" % n)

    def _on_iterp(self):
        mw = self.parent()
        if not hasattr(mw, "send_koko"):
            return
        n = self.spinBox_niterp.value()
        mw.send_koko(self._verbose_prefix())
        mw.send_koko("IT P,%d" % n)

    def _on_robb(self):
        mw = self.parent()
        if not hasattr(mw, "send_koko"):
            return
        try:
            beta = float(self.lineEdit_beta.text().strip())
            delta = float(self.lineEdit_delta.text().strip())
        except ValueError:
            return
        n = self.spinBox_nrobb.value()
        mw.send_koko(self._verbose_prefix())
        mw.send_koko("ROBB,%s,%s,%d" % (repr(beta), repr(delta), n))

class NKDialog(QDialog, Ui_nkDialog):
    """Material dialog.

    Mirrors the C++ nkDialog but folds the right-click context-menu
    "Model / AIR / REFLECTOR / Glass catalog" choices into one dialog: the
    user picks a material type (radio buttons) and the relevant inputs
    appear in a stacked widget. The chosen material is returned as a ready
    koko command fragment via material_command().

    The Model page also carries a FINDGLASS GUI: enter n (and optionally V)
    and click "Find Glass" to list the 5 nearest real glasses across the
    catalogs; double-clicking a candidate fills it in as a catalog glass.
    """

    def __init__(self, parent=None, catalogs=None):
        super().__init__(parent)
        self.setupUi(self)
        # catalogs: list of (catalog_name, [glass_names])
        self._catalogs = catalogs or []
        self._populate_catalogs()
        # Radio -> stacked page. Order must match the .ui radio creation.
        self._radio_pages = {
            self.radioModel: 0,    # pageModel
            self.radioAir: 1,      # pageAir
            self.radioRefl: 2,     # pageRefl
            self.radioCatalog: 3,  # pageCatalog
        }
        for radio, idx in self._radio_pages.items():
            radio.toggled.connect(
                lambda _checked, i=idx: self.stackedWidget.setCurrentIndex(i))
        self.comboCatalog.currentIndexChanged.connect(self._on_catalog_changed)
        self._on_catalog_changed(0)
        self._build_findglass_ui()
        # Ensure the Model page (with the FINDGLASS GUI) is the only visible
        # stacked page. On some Qt builds setCurrentIndex alone leaves the
        # later-added pages shown, so hide the others explicitly.
        self.stackedWidget.setCurrentIndex(0)
        self.pageAir.hide()
        self.pageRefl.hide()
        self.pageCatalog.hide()

    def _populate_catalogs(self):
        self.comboCatalog.clear()
        for cat_name, names in self._catalogs:
            if names:
                self.comboCatalog.addItem(cat_name)

    def _on_catalog_changed(self, _index):
        self.comboGlass.clear()
        cat = self.comboCatalog.currentText()
        for cat_name, names in self._catalogs:
            if cat_name == cat:
                self.comboGlass.addItems(names)
                break

    def material_type(self):
        """Return one of 'MODEL', 'AIR', 'REFL', 'CATALOG'."""
        if self.radioAir.isChecked():
            return 'AIR'
        if self.radioRefl.isChecked():
            return 'REFL'
        if self.radioCatalog.isChecked():
            return 'CATALOG'
        return 'MODEL'

    def material_command(self):
        """Return the koko command fragment for the chosen material.

        MODEL  -> "MODEL name[,n[,v]]"
        AIR    -> "AIR"
        REFL   -> "REFL"
        CATALOG-> "<catalog> <glass>"
        """
        mtype = self.material_type()
        if mtype == 'AIR':
            return 'AIR'
        if mtype == 'REFL':
            return 'REFL'
        if mtype == 'CATALOG':
            cat = self.comboCatalog.currentText().strip()
            glass = self.comboGlass.currentText().strip()
            if not cat or not glass:
                return None
            return '%s %s' % (cat, glass)
        # MODEL
        name = self.lineEdit.text().strip()
        n = self.lineEdit_2.text().strip()
        v = self.lineEdit_3.text().strip()
        if not name:
            return None
        cmd = 'MODEL ' + name
        if n:
            cmd += ',' + n
        if v:
            cmd += ',' + v
        return cmd

    def _build_findglass_ui(self):
        """Add the FINDGLASS GUI to the Model page (pageModel).

        A horizontal separator + a header band, an n/V input row, a "Find
        Glass" button, and a 5-row candidate list (QListWidget).
        Double-clicking a candidate copies it into the catalog combo + glass
        combo and switches to the Glass catalog page so it becomes the
        selected material on OK.
        """
        # Replace pageModel's existing layout (a QGridLayout holding the n/V
        # inputs) with a single QVBoxLayout so we can stack the FINDGLASS
        # controls beneath the inputs. Moving the existing children into the
        # new layout (instead of nesting layouts) avoids Qt's "already has a
        # layout/parent" errors.
        old = self.pageModel.layout()
        vbox = QVBoxLayout()
        vbox.setContentsMargins(8, 8, 8, 8)
        vbox.setSpacing(6)
        if old is not None:
            while old.count():
                item = old.takeAt(0)
                w = item.widget()
                l = item.layout()
                if w is not None:
                    vbox.addWidget(w)
                elif l is not None:
                    vbox.addLayout(l)
            import PyQt6.sip as sip
            sip.delete(old)
        self.pageModel.setLayout(vbox)

        # separator
        sep = QFrame(self.pageModel)
        sep.setFrameShape(QFrame.Shape.HLine)
        sep.setFrameShadow(QFrame.Shadow.Sunken)
        sep.setLineWidth(1)
        sep.setStyleSheet(
            "QFrame { color: #c0c4c8; background-color: #c0c4c8; }")
        vbox.addWidget(sep)

        # header band
        hdr = QLabel("FINDGLASS  (find real glasses by n, V)", self.pageModel)
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        hdr.setStyleSheet(
            "QLabel { background-color: #eef0f2; border: 1px solid #c8ccd0;"
            " border-radius: 3px; padding: 5px; font-weight: bold; }")
        vbox.addWidget(hdr)

        # Search row: reads n / V from the Model n/V fields above and lists
        # the 5 nearest real glasses. Double-click a candidate to use it.
        inp = QHBoxLayout()
        self._btn_find = QPushButton("Find Glass", self.pageModel)
        self._btn_find.setMinimumWidth(110)
        hint = QLabel("Uses the Index n / Abbe V above to list the 5 "
                      "nearest catalog glasses.", self.pageModel)
        hint.setStyleSheet("QLabel { color: #5a6066; font-size: 10px; }")
        inp.addWidget(self._btn_find)
        inp.addWidget(hint, 1)
        vbox.addLayout(inp)

        # candidate list (up to 5)
        self._fg_list = QListWidget(self.pageModel)
        self._fg_list.setAlternatingRowColors(True)
        self._fg_list.setMinimumHeight(130)
        self._fg_list.setFont(QFont("Noto Mono", 9))
        vbox.addWidget(self._fg_list)

        note = QLabel("Double-click a candidate to use it as a catalog glass.",
                      self.pageModel)
        note.setStyleSheet("QLabel { color: #5a6066; font-size: 10px; }")
        vbox.addWidget(note)

        self._btn_find.clicked.connect(self._fg_search)
        self._fg_list.itemDoubleClicked.connect(self._fg_choose)

    def _fg_search(self):
        """Run the FINDGLASS search over all catalogs and list up to 5.

        Reads the target refractive index (n) and Abbe number (V) from the
        Model page's Index n / Abbe V fields -- matching the KDP2 FINDGLASS
        flow, which searches for glasses near the current MODEL glass.
        """
        import gui_py.glassmap as gm
        try:
            n = float(self.lineEdit_2.text().strip())
        except ValueError:
            QMessageBox.information(
                self, "FINDGLASS",
                "Enter a numeric refractive index in 'Index n' above.")
            return
        vtxt = self.lineEdit_3.text().strip()
        v = float(vtxt) if vtxt else 50.0
        glasses = gm.load_all_glasses()
        if not glasses:
            QMessageBox.information(self, "FINDGLASS",
                                    "No glass catalogs found.")
            return
        hits = gm.find_nearest_glasses(n, v, glasses=glasses, limit=5)
        self._fg_list.clear()
        for h in hits:
            item = QListWidgetItem(
                "%-22s %-9s n=%.4f  V=%.2f"
                % (h["name"], h["catalog"], h["nd"], h["vd"]))
            item.setData(Qt.ItemDataRole.UserRole, h)
            self._fg_list.addItem(item)
        if not hits:
            self._fg_list.addItem("(no matches)")

    def _fg_choose(self, item):
        """Double-click a candidate: switch to Glass catalog page with it."""
        h = item.data(Qt.ItemDataRole.UserRole)
        if not isinstance(h, dict):
            return
        self.radioCatalog.setChecked(True)
        # make sure the candidate's catalog is present in the combo
        cat = h["catalog"]
        idx = self.comboCatalog.findText(cat)
        if idx < 0:
            # catalog not in the name-only combo list; add it
            self.comboCatalog.addItem(cat)
            # populate its glass names lazily from glassmap
            import gui_py.glassmap as gm
            names = [g["name"] for g in gm.load_all_glasses(catalogs=[cat])]
            self._catalogs.append((cat, names))
            idx = self.comboCatalog.findText(cat)
        self.comboCatalog.setCurrentIndex(idx)
        gidx = self.comboGlass.findText(h["name"])
        if gidx < 0:
            self.comboGlass.addItem(h["name"])
            gidx = self.comboGlass.findText(h["name"])
        self.comboGlass.setCurrentIndex(gidx)

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

class ApodDialog(QDialog, Ui_ApodDialog):
    """Aperture Apodization Settings (mirrors original IDD_APOD / APODGUI)."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)

    def apply_command(self):
        """Return the koko command string for the current settings."""
        if self.radioGaussian.isChecked():
            val = self.doubleApod.value()
            return "APOD GAUSS,%s" % repr(val)
        return "APOD NONE"

class ImageBlurDialog(QDialog, Ui_ImageBlurDialog):
    """Image Blur: load a 24-bit BMP, trace it through the lens, convolve
    with the lens PSF, and show the resulting blurred image.

    Mirrors Koko's OFROMBMP / IOBJECTD / IMTRACE / PSF / PSFTOIMG / PLTIMG
    command chain. The BMP is copied into $HOME so koko (which reads
    $HOME/<name>.BMP) can find it.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)
        self._bmp_path = None
        self.btnBrowse.clicked.connect(self._browse)
        self.btnAuto.clicked.connect(self._use_bmp_dims)
        # Default the source 24-bit BMP to ~/KODS/PORT.BMP (shipped via
        # `make install-data`, which copies ./Libs into ~/KODS). The user can
        # still override it with the Browse button.
        import os
        default_bmp = os.path.join(os.path.expanduser("~"), "KODS", "PORT.BMP")
        if os.path.exists(default_bmp):
            self._bmp_path = default_bmp
            self.lineFile.setText(default_bmp)
            self._probe_dims()

    def _browse(self):
        from PyQt6.QtWidgets import QFileDialog
        import os
        start = os.path.dirname(self._bmp_path) if self._bmp_path \
            else os.path.join(os.path.expanduser("~"), "KODS")
        path, _ = QFileDialog.getOpenFileName(
            self, "Select 24-bit BMP", start,
            "BMP images (*.bmp *.BMP);;All files (*)")
        if path:
            self._bmp_path = path
            self.lineFile.setText(path)
            self._probe_dims()

    def _probe_dims(self):
        """Read BMP header to default the array size to the image size."""
        try:
            with open(self._bmp_path, "rb") as fh:
                data = fh.read(30)
            # BITMAPINFOHEADER: width @ offset 18 (int32 LE),
            # height @ offset 22 (int32 LE)
            import struct
            w = struct.unpack_from("<i", data, 18)[0]
            h = struct.unpack_from("<i", data, 22)[0]
            w = abs(w)
            h = abs(h)
            if w > 1 and h > 1:
                self.spinNX.setValue(min(w, 1024))
                self.spinNY.setValue(min(h, 1024))
        except OSError:
            pass

    def _use_bmp_dims(self):
        if self._bmp_path:
            self._probe_dims()

    def get_bmp_path(self):
        return self._bmp_path

    def commands(self):
        """Build the koko command sequence for the current settings.

        Mirrors KDP2's FULLIMAGING command chain (IMAGE1.FOR):
          - COLOR RGB            : select 24-bit RGB imagery
          - IIMAGEN xext yext nx ny : define the IMAGE-plane array
          - IOBJECTD xext yext nx ny : define the OBJECT-plane array
          - OFROMBMP <name>      : load the 24-bit BMP into the object array
          - IMTRACE2  (Single PSF)  : one on-axis PSF convolved over every
                                      object point (KDP2 "Single PSF
                                      convolution")
          - IMTRACE3  (Full)         : a fresh PSF is recomputed at every
                                      object point (KDP2 "PSF per object
                                      point")
          - PLTIMG <trim>        : write the blurred image BMP

        KDP2's IMTRACE2/3 build the PSF internally, so no separate PSF /
        PSFTOIMG step is needed. The chosen BMP is copied into $HOME/KODS/
        under a fixed short name ("KOBJ") because koko reads $HOME/<name>.BMP
        and uppercases/truncates bare names to 8.3; a constant name also
        prevents koko's write-back from ever touching the user's original.
        """
        n = self._bmp_path
        if not n:
            return None
        import os
        # koko's HOME (from .kokorc) is ~/KODS/; OFROMBMP reads
        # $HOME/<name>.BMP i.e. ~/KODS/<name>.BMP
        home = os.path.join(os.path.expanduser("~"), "KODS")
        os.makedirs(home, exist_ok=True)
        objname = "KOBJ"
        dest = os.path.join(home, objname + ".BMP")
        try:
            with open(n, "rb") as src, open(dest, "wb") as dst:
                dst.write(src.read())
        except OSError:
            return None
        # Determine the BMP's real pixel size and override NX/NY.
        nx = ny = 0
        try:
            from PIL import Image
            with Image.open(dest) as im:
                nx, ny = im.size
        except Exception:
            nx = self.spinNX.value()
            ny = self.spinNY.value()
        if nx <= 0 or ny <= 0:
            nx = self.spinNX.value()
            ny = self.spinNY.value()
        dx = self.doubleDX.value()
        dy = self.doubleDY.value()
        trim = self.spinTrim.value()
        # This MUST mirror the verified IMTESTx.MAC macro chain, which is the
        # only sequence koko's IMTRACE2/3 accepts. The macro (which works) is:
        #   COLOR RGB
        #   IIMAGEN 0.44794 0.335624 320 240      (image-plane EXTENT, NX, NY)
        #   OFROMBMP 0.40E+19 PORT                (WRD1=object size, name=PORT)
        #   PLTOBJ                                 (aim PSF at object points)
        #   TGR 512 / NRD 64 / PGR 91             (PSF grid)
        #   IMTRACE2 | IMTRACE3
        #   PLTIMG <trim>
        # Key facts from koko's OFROMBMP handler (image.f): the filename arrives
        # as the STRING word (WS/WQ) and the object size as W1 (WRD1). So the
        # order is "<size> <name>", NOT "<name> <size>". There is NO IOBJECTD
        # (OFROMBMP alone defines the object plane) and PLTOBJ + TGR/NRD/PGR
        # are required for IMTRACE2/3 to aim and size the PSF correctly.
        # The object size WRD1 is the macro's 0.40E+19 ("object at infinity");
        # IIMAGEN's extent is the image-plane extent (here dx*(nx-1), dy*(ny-1)).
        obj_extent_x = dx * (nx - 1)
        obj_extent_y = dy * (ny - 1)
        cmds = [
            "COLOR RGB",
            # IIMAGEN image-plane EXTENT (xext, yext) and grid (NX, NY).
            "IIMAGEN %s %s %d %d" % (repr(obj_extent_x), repr(obj_extent_y), nx, ny),
            # OFROMBMP: WRD1 = object size (macro uses 0.40E+19 = infinity),
            # then the object BMP name (koko reads $HOME/<name>.BMP).
            "OFROMBMP 0.40E+19 %s" % objname,
            # NOTE: PLTOBJ is intentionally omitted. In koko it is
            # "PLOT THE OBJECT ARRAY" (image.f -> PLOTIMAGEARRAY), a gnuplot
            # plot command, NOT an IMTRACE setup step. Sending it makes koko
            # block on a gnuplot plot and IMTRACE/PLTIMG never completes, so
            # PLOTBMP.BMP is never written. IMTRACE2/3 build the PSF internally
            # (they issue PSF themselves), so PLTOBJ is not needed.
            # PSF grid size (matches the verified macro).
            "TGR 512",
            "NRD 64",
            "PGR 91",
        ]
        if self.radioSimple.isChecked():
            # Single on-axis PSF convolution (KDP2 IMTRACE2).
            cmds.append("IMTRACE2")
        else:
            # PSF recomputed at each object point (KDP2 IMTRACE3).
            cmds.append("IMTRACE3")
        cmds.append("PLTIMG %d" % trim)
        return cmds

    def set_psf_grid_spacing(self, griimg):
        """Override the image-plane pixel size with the PSF grid spacing.

        Called by the main window after it has run PSF and parsed GRIIMG, so
        the IIMAGEN grid lines up 1:1 with the PSF grid (KDP2 parity).
        """
        try:
            self._griimg = float(griimg)
        except (TypeError, ValueError):
            self._griimg = 0.0

class DifsetDialog(QDialog, Ui_DifsetDialog):
    """General Diffraction Calculation Settings (mirrors original IDD_DIFSET)."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setupUi(self)

    def apply_commands(self):
        """Return the koko command strings for the current settings.

        Mirrors rays.inc IDD_DIFSET handler: exit-pupil choice then
        reference-sphere choice.
        """
        cmds = []
        if self.radioEx1.isChecked():
            cmds.append("EXPUP AUTO")
        elif self.radioEx2.isChecked():
            cmds.append("EXPUP NOAUTO")
        if self.radioRef1.isChecked():
            cmds.append("RSPH NOTILT")
        elif self.radioRef2.isChecked():
            cmds.append("RSPH BEST")
        elif self.radioRef3.isChecked():
            cmds.append("RSPH CHIEF")
        return cmds

