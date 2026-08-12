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
import subprocess
import struct

from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QMessageBox, QFileDialog, QTableWidgetItem,
    QDialog, QLabel, QVBoxLayout, QHBoxLayout, QLineEdit, QPushButton,
    QComboBox, QDialogButtonBox, QInputDialog, QMenu, QWidget, QFrame,
    QSizePolicy, QStyledItemDelegate,
)
from PyQt6.QtCore import QProcess, Qt, QTimer, QByteArray, QSize, QEvent
from PyQt6.QtGui import QFont, QPixmap, QImage, QPalette, QColor, QBrush


# Commands that make koko write a plot script (drawcmd.gpl). Any of these,
# whether launched from the Plot menu or typed in the command line, should
# trigger an automatic render of the graph in the GUI.
PLOT_TRIGGER_PREFIXES = (
    'VIE', 'SPD', 'CAPFN', 'PSF', 'DIST', 'FLDCV', 'AST', 'CHRSHIFT',
    'FANS', 'DRAW', 'DRAWFAN', 'GRAOUT', 'PLT', 'PLOT ', 'SPOT', 'DOTF',
    'GOTF', 'FAN', 'RAY', 'PARAX',
)

from gui_py.ui_mainwindow import Ui_MainWindow
from gui_py.ui_apoddialog import Ui_ApodDialog
from gui_py.ui_difsetdialog import Ui_DifsetDialog
from gui_py.ui_lidialog import Ui_LIDialog
from gui_py.ui_newdialog import Ui_NewDialog
from gui_py.ui_nkdialog import Ui_nkDialog
from gui_py.ui_rayinputdialog import Ui_rayinputDialog
from gui_py.ui_optimize import Ui_Optimize
from gui_py.ui_optimdialog import Ui_OptimizeDialog
from gui_py.ui_raydialog import Ui_RayDialog
from gui_py.ui_pikupdialog import Ui_PikupDialog
from gui_py.ui_aperturedialog import Ui_ApertureDialog
from gui_py.ui_obsdialog import Ui_ObscurationDialog
from gui_py.ui_tiltdialog import Ui_TiltDialog
from gui_py.ui_viedialog import Ui_VieDialog


# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

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


# --------------------------------------------------------------------------
# Dialogs
# --------------------------------------------------------------------------

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
                xdec = float(self._ui.lineEdit_xdec.text().strip() or "0.0")
                ydec = float(self._ui.lineEdit_ydec.text().strip() or "0.0")
                if shape == "Circular":
                    rad = float(self._ui.lineEdit_rad.text().strip() or "0.0")
                    return dict(shape=shape, surf=surf, rad=rad,
                                xdec=xdec, ydec=ydec)
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
                xdec = float(self._ui.lineEdit_xdec.text().strip() or "0.0")
                ydec = float(self._ui.lineEdit_ydec.text().strip() or "0.0")
                if shape == "Circular":
                    rad = float(self._ui.lineEdit_rad.text().strip() or "0.0")
                    return dict(shape=shape, surf=surf, rad=rad,
                                xdec=xdec, ydec=ydec)
                tilt = float(self._ui.lineEdit_tilt.text().strip() or "0.0")
                hx = float(self._ui.lineEdit_hx.text().strip() or "0.0")
                hy = float(self._ui.lineEdit_hy.text().strip() or "0.0")
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
             'Glass', 'Index n', 'Abbe V', 'Aperture'])
        self.table.verticalHeader().setVisible(True)
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

    def _launch_koko_process(self):
        """Start koko-cli inside a real PTY and return (pid, fd).
        
        Returns None on failure. On success sets self._koko_pid and
        self._koko_fd. The child is launched in its own session so it
        can be killed by group later if needed.
        """
        import pty, os, fcntl, struct, signal, termios

        master, slave = pty.openpty()
        # non-blocking master so the GUI event loop is never stalled
        fl = fcntl.fcntl(master, fcntl.F_GETFL)
        fcntl.fcntl(master, fcntl.F_SETFL, fl | os.O_NONBLOCK)
        # CRITICAL: set a window size on the slave pty. Without this koko
        # (Fortran + linenoise) stays silent and emits nothing.
        winsize = struct.pack("HHHH", 24, 80, 0, 0)
        fcntl.ioctl(slave, termios.TIOCSWINSZ, winsize)

        try:
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

        self._koko_pid = proc.pid
        self._koko_fd = master
        self._sent_cmds = []  # strip echo-back from koko output
        
        # Poll the pty master on a timer
        self._koko_poll = QTimer(self)
        self._koko_poll.timeout.connect(self._poll_koko_pty)
        self._koko_poll.start(80)
        
        # Reap the child if it dies
        self._koko_watch = QTimer(self)
        self._koko_watch.timeout.connect(lambda: self._reap_koko())
        self._koko_watch.start(1000)
        
        return True

    def start_koko_cli(self, lens_path=None):
        """Launch koko-cli and set up initial command schedule."""
        self._kill_koko()
        
        if not self.koko_path:
            QMessageBox.critical(
                self, "Error",
                "koko-cli not found. Please build Koko first "
                "(see Src/Makefile).")
            return False
        
        if not self._launch_koko_process():
            return False
        
        # If a lens was requested, restore it after startup banner
        if lens_path:
            QTimer.singleShot(600, lambda: self.load_lens(lens_path))
        else:
            default_lens = os.path.join(
                self.HOME, 'LENSES', 'COOCK.PRG')
            if os.path.exists(default_lens):
                self.current_lens = default_lens
                self._read_lens_file_meta(default_lens)
        
        # Disable command echo-back, then request surface listing
        QTimer.singleShot(250, lambda: self.send_koko("ECHO OFF"))
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
        # Suppress the "> command" echo in GUI mode only when explicitly
        # disabled. Set KOKO_GUI_ECHO=0 to hide the echo (koko is launched
        # with -G, GUI mode). The default is echo ON so command flow is
        # visible during normal use.
        if os.environ.get("KOKO_GUI_ECHO", "1") == "1":
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
        """Orchestrate RTG output parsing."""
        if not hasattr(self, '_rtg_buf'):
            self._rtg_buf = None
        if not hasattr(self, '_koko_idle'):
            self._koko_idle = True
        
        self._check_prompt_idle(text)
        self._parse_rtg_meta(text)
        
        if 'BASIC LENS DATA' in text:
            self._rtg_buf = ''
        if self._rtg_buf is not None:
            self._rtg_buf += text
            self._parse_rtg_surface_details(text)
            if 'LAST SURFACE' in text or 'NO SURFACES' in text:
                self._on_rtg_complete(self._rtg_buf)
                self._rtg_buf = None

    def _check_prompt_idle(self, text):
        """Detect koko's 'N:cmd>' prompt after stripping ANSI escapes."""
        _clean = re.sub(r'\x1b\[[0-9;]*[A-Za-z]', '', text)
        if re.search(r'\d+:\s*cmd>\s*$', _clean.rstrip()):
            self._koko_idle = True

    def _parse_rtg_meta(self, text):
        """Scan for lens metadata: LI, WV, UNITS, LENS RESTORED."""
        for line in text.splitlines():
            stripped = line.strip()
            
            m_li = re.match(r'(?i)^LI\s*,?\s*(.+)$', stripped)
            if m_li:
                self._li = m_li.group(1).strip()
                continue
            
            m_restored = re.search(
                r'LENS SAVED AS:\s*(\S+?)\.PRG\s+HAS BEEN RESTORED',
                stripped, re.IGNORECASE)
            if m_restored:
                name = m_restored.group(1)
                self._li = name
                for cand in (
                    os.path.join(self.HOME, 'LENSES', name + '.PRG'),
                    os.path.join(self.HOME, 'LENSES', name + '.prg'),
                    os.path.join(self.HOME, 'LENSES', name + '.koko'),
                    name + '.PRG', name + '.prg',
                ):
                    if os.path.exists(cand):
                        self._read_lens_file_meta(cand)
                        break
                self._rtg_buf = None
                self.send_koko("RTG ALL")
                continue
            
            if self._rtg_buf is not None and (stripped.startswith('WV')
                   or re.match(r'(?i)^WV\s', stripped)):
                nums = re.findall(r'[\d.]+', stripped)
                if len(nums) >= 3:
                    try:
                        self._lD = float(nums[0])
                        self._lF = float(nums[1])
                        self._lC = float(nums[2])
                    except ValueError:
                        pass
                continue
            
            m_units = re.match(r'(?i)^UNITS\s+(.+)$', stripped)
            if m_units:
                self._units = m_units.group(1).strip().lower()

    def _parse_rtg_surface_details(self, text):
        """Extract per-surface markers: CC, ASPH, TILT, CLAP."""
        for line in text.splitlines():
            stripped = line.strip()
            
            m_surf = re.match(r'^(\d+)\s*\*\s*(CC|ASPH2?|TILT|REFS|ASTOP)\b(.*)',
                              stripped, re.IGNORECASE)
            if m_surf:
                s = int(m_surf.group(1))
                kind = m_surf.group(2).upper()
                rest = m_surf.group(3).strip()
                store_map = {'CC': self._ccv, 'ASPH2': self._asph2v,
                             'ASPH': self._asphv, 'TILT': self._tiltv}
                if kind in store_map:
                    store_map[kind][s] = rest if rest else line.strip()
                elif kind == 'REFS':
                    self._ccv[s] = 'REFS'
                elif kind == 'ASTOP':
                    self._ccv[s] = 'ASTOP'
                continue
            
            for pat, attr in [(r'(?i)^CC\s+(.+)$', '_ccv'),
                              (r'(?i)^ASPH\s+(.+)$', '_asphv'),
                              (r'(?i)^TILT\s+(.+)$', '_tiltv')]:
                m = re.match(pat, stripped)
                if m:
                    src = getattr(self, attr, {})
                    for s in list(src.keys()):
                        if s not in src or not src[s]:
                            src[s] = m.group(1).strip()
                            break
                    break
            
            m_clap = re.match(r'(?i)^CLAP\s+([\d.eE+-]+)', stripped)
            if m_clap:
                try:
                    ap_val = float(m_clap.group(1))
                    for r in range(self.table.rowCount()):
                        itm = self.table.item(r, 7)
                        if itm is None or not itm.text().strip():
                            self._set_cell(r, 7, str(ap_val))
                            break
                except ValueError:
                    pass

    def _on_rtg_complete(self, buf):
        """Dispatch populated buffer to table and schedule plot render."""
        self.populate_table(buf)
        if self._pending_vie:
            self._pending_vie = False
            self.send_koko("VIE XZ")
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
                    # Let the default handler commit the edit; cellChanged
                    # will then fire and forward the value to koko. Returning
                    # False (instead of consuming the key) is what allows the
                    # edit to be committed on Enter.
                    return False

        return super().eventFilter(obj, event)

    def _on_cell_changed(self, row, col):
        """Forward a committed cell edit to koko (mirrors C++ slot_action_
        value_entered). Fires on Enter and on focus-loss commit. Guarded by
        self._table_updating so populate_table's own writes are ignored.
        Only Radius/Curvature (2), Thickness (3) and Aperture (7) are
        directly editable; other columns are read-only and never reach here.
        """
        if self._table_updating:
            return
        if row == 0:          # OBJ row is never edited
            return
        if col not in (2, 3, 7):
            return
        self._send_table_current_cell()

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
                                prev[5] or mi, prev[6] or ma, prev[7])
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
                                    rows[-1][5], rows[-1][6], rows[-1][7])
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
                   thickness, material, index, abbe, "")
            rows.append(row)
            
            if is_glass and not material.startswith("MODEL"):
                gcat, _, gname = material.partition(" ")
                if gcat and gname:
                    gi, ga = self._calc_glass_nv(gcat, gname)
                    if gi is not None:
                        rows[-1] = (rows[-1][0], rows[-1][1], rows[-1][2],
                                    rows[-1][3], rows[-1][4], gi, ga,
                                    rows[-1][7])
        
        return rows

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
        for i, (surf, stype, radius, thickness, material, index, abbe, ap) in enumerate(rows):
            self._set_cell(i, 0, surf)
            self._set_cell(i, 1, stype)
            self._set_cell(i, 2, radius)
            self._set_cell(i, 3, thickness)
            self._set_cell(i, 4, material)
            self._set_cell(i, 5, index)
            self._set_cell(i, 6, abbe)
            self._set_cell(i, 7, ap)
            # Cache raw radius (col 2 in _build_rtg_rows output) so the
            # Radius/Curvature display toggle can update without koko.
            try:
                self._radius_values[i] = float(radius)
            except (ValueError, TypeError):
                self._radius_values[i] = None
        self._table_updating = False
        # Apply the current Radius/Curvature display mode to the Radius column.
        self._refresh_radius_display()

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

    def _on_radius_curvature_changed(self, text):
        """Combo box handler: switch Radius/Curvature display mode."""
        self._curvature_mode = (text == "Curvature")
        self._refresh_radius_display()

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
        hp = self._header_widget.palette()
        hp.setColor(QPalette.ColorRole.Window, QColor('#eef0f2'))
        self._header_widget.setPalette(hp)

        hbox = QHBoxLayout(self._header_widget)
        hbox.setContentsMargins(0, 0, 0, 0)
        hbox.setSpacing(0)

        headers = ['Surf', 'Surface Type', 'Radius', 'Thickness',
                   'Glass', 'Index n', 'Abbe V', 'Aperture']
        for i, h in enumerate(headers):
            if i == 2:
                # Radius column: the combo box IS the header label
                # ("Radius" / "Curvature"). Style it to sit inside the
                # header band rather than as a detached floating widget.
                self.comboRadiusCurvature.setFixedWidth(110)
                # Center the displayed text without making the box editable
                # (an editable+read-only box stops the drop-down from opening
                # on mouse click). A center-aligning delegate handles both the
                # current item and the popup items.
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
                hbox.addWidget(self.comboRadiusCurvature)
            else:
                lbl = QLabel(h, parent=self._header_widget)
                lbl.setAlignment(Qt.AlignmentFlag.AlignCenter)
                lbl.setSizePolicy(
                    QSizePolicy.Policy.Expanding,
                    QSizePolicy.Policy.Preferred)
                lbl.setFont(QFont("Noto Sans", 9, QFont.Weight.Bold))
                lbl.setStyleSheet(
                    "QLabel {"
                    "  color: #333333;"
                    "  background-color: #eef0f2;"
                    "  padding: 3px 2px;"
                    "  border-right: 1px solid #d8dadc;"
                    "}"
                )
                hbox.addWidget(lbl)

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

        # Keep the custom header column widths in sync with the table.
        self.table.horizontalHeader().sectionResized.connect(
            self._sync_header_widths)
        # First sync after the layout/layout-pass settles.
        QTimer.singleShot(0, self._sync_initial_header_widths)

    def _sync_initial_header_widths(self):
        """Initial column-width sync once the table has settled on its
        default sizes (e.g. AdjustToContents has run)."""
        if not hasattr(self, '_header_widget'):
            return
        hbox = self._header_widget.layout()
        for i in range(self.table.columnCount()):
            if i >= hbox.count():
                break
            item = hbox.itemAt(i)
            if item is None or item.widget() is None:
                continue
            w = max(40, self.table.columnWidth(i))
            if i == 2:      # combo box: fixed width, don't overwrite
                continue
            item.widget().setMinimumWidth(w)
            item.widget().setMaximumWidth(16777215)

    def _sync_header_widths(self, logicalIndex, oldSize, newSize):
        """Mirror table column width changes onto the custom header row."""
        if not hasattr(self, '_header_widget'):
            return
        hbox = self._header_widget.layout()
        if logicalIndex < 0 or logicalIndex >= hbox.count():
            return
        item = hbox.itemAt(logicalIndex)
        if item is None or item.widget() is None:
            return
        if logicalIndex == 2:     # combo box: fixed width
            return
        w = max(40, newSize)
        item.widget().setMinimumWidth(w)
        item.widget().setMaximumWidth(16777215)

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

    def slot_lensInfo(self, row, col):
        """Show surface detail info when clicking a table row (mirrors C++)."""
        self._highlight_rows(row, self._row0)
        self._row0 = row
        self._show_surface_panel(row)
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
        """Wire all menu actions using a data-driven dispatch table."""
        # Helper to connect an action by its generated slot name attribute
        def connect(attr_name, handler, *handler_args):
            action = getattr(self, attr_name)
            if handler_args:
                action.triggered.connect(
                    lambda _checked=False, h=handler, a=handler_args: h(*a))
            else:
                action.triggered.connect(handler)

        # Action dispatch table: [(action_attr, target_method_or_lambda, *args)]
        actions = [
            # File
            ('actionNew', self.slot_actionNew),
            ('actionOpen', self.slot_actionOpen),
            ('actionSave', self.slot_actionSave),
            ('actionQuit', self.slot_quit2),
            ('actionExport_JPEG', 'slot_export', 'jpeg'),
            ('actionExport_EPS', 'slot_export', 'eps'),
            ('actionExport_PDF', 'slot_export', 'pdf'),
            ('actionImport_Zemax', self.slot_actionImport_Zemax),
            ('actionImport_Code_V', self.slot_actionImport_CODE_V),
            ('actionExport_Zemax', self.slot_actionExport_Zemax),
            ('actionExport_Code_V', self.slot_actionExport_CODE_V),
            # Lens View (plots)
            ('actionXZ', self.slot_plot, 'VIE XZ'),
            ('actionOrtho', self.slot_plot, 'VIE ORTHO'),
            # Analyze -> spot / wavefront / PSF
            ('actionSpot_Diagram', self.slot_plot, 'SPOT RING', 'SPD',
             'PLTSPD'),
            ('actionWavefront_Phase', self.slot_plot, 'CAPFN', 'PLOT CAPFNOPD'),
            ('actionWavefront_Intensity', self.slot_plot, 'CAPFN', 'PLOT CAPFNAPD'),
            ('actionPoint_Spread_Function', self.slot_plot, 'PSFWRITE YES',
             'PSFLOG 0', 'PSFPLOT YES', 'PSF,1', 'CAPFNOUT'),
            ('actionDistortion', self.slot_plot, 'DIST', 'PLTDIST'),
            ('actionField_Curvature', self.slot_plot, 'FLDCV', 'PLTFLDCV'),
            ('actionAstigmatism', self.slot_plot, 'AST', 'PLTAST'),
            ('actionGeometical', self.slot_plot, 'SPACE I', 'SPACE O', 'FAR',
             'GOTF', 'PLTGOTF,1'),
            ('actionGeometical_Leica', self.slot_plot, 'SPACE I', 'SPACE O',
             'FAR', 'GOTF', 'PLTGOTF LEICA,1'),
            ('actionDiffraction', self.slot_plot, 'SPACE I', 'SPACE O', 'FAR',
             'DOTF', 'PLTDOTF,,1'),
            ('actionDiffraction_Leica', self.slot_plot, 'SPACE I', 'SPACE O',
             'FAR', 'DOTF', 'PLTDOTF LEICA,,1'),
            ('actionParaxial_Chromatic_Focus_Shift', self.slot_plot, 'CHRSHIFT', 'PLTCHRSH'),
            # Ray (single ray trace) and Paraxial data displays
            ('actionRay_Single', self.slot_actionRay_single),
            ('actionPikup', self.slot_actionPikup),
            ('actionAperture', self.slot_actionAperture),
            ('actionObscuration', self.slot_actionObscuration),
            ('actionTilt', self.slot_actionTilt),
            ('actionVie', self.slot_actionVie),
            ('actionParaxial_FCHY', self.slot_text, 'FCHY ALL'),
            ('actionParaxial_FCHX', self.slot_text, 'FCHX ALL'),
            ('actionParaxial_PCD3', self.slot_text, 'PCD3 ALL'),
            ('actionParaxial_SCD3', self.slot_text, 'SCD3 ALL'),
            ('actionParaxial_PRXYZ', self.slot_text, 'PRXYZ ALL'),
            ('actionParaxial_PRR', self.slot_text, 'PRR ALL'),
            # Aberration fans (koko 'FANS <qualifier>' command, per KDP2 RIMS)
            ('actionXYFAN', self.slot_plot, 'FANS XYFAN'),
            ('actionYXFAN', self.slot_plot, 'FANS YXFAN'),
            ('actionXFAN', self.slot_plot, 'FANS XFAN'),
            ('actionYFAN', self.slot_plot, 'FANS YFAN'),
            ('actionNFAN', self.slot_plot, 'FANS NFAN'),
            ('actionPFAN', self.slot_plot, 'FANS PFAN'),
            ('actionXOPD', self.slot_plot, 'FANS XOPD'),
            ('actionYOPD', self.slot_plot, 'FANS YOPD'),
            ('actionXYOPD', self.slot_plot, 'FANS XYOPD'),
            ('actionNOPD', self.slot_plot, 'FANS NOPD'),
            ('actionPOPD', self.slot_plot, 'FANS POPD'),
            ('actionXCD', self.slot_plot, 'FANS XCD'),
            ('actionYCD', self.slot_plot, 'FANS YCD'),
            ('actionXYCD', self.slot_plot, 'FANS XYCD'),
            ('actionYXCD', self.slot_plot, 'FANS YXCD'),
            ('actionNCD', self.slot_plot, 'FANS NCD'),
            ('actionPCD', self.slot_plot, 'FANS PCD'),
            ('actionXLA', self.slot_plot, 'FANS XLA'),
            ('actionYLA', self.slot_plot, 'FANS YLA'),
            ('actionXYLA', self.slot_plot, 'FANS XYLA'),
            ('actionYXLA', self.slot_plot, 'FANS YXLA'),
            ('actionNLA', self.slot_plot, 'FANS NLA'),
            ('actionPLA', self.slot_plot, 'FANS PLA'),
            # Edit
            ('actionInsert_Surface', self.slot_text_insert_surface),
            ('actionDelete_Surface', self.slot_text_delete_surface),
            ('actionInput_Glass_Model', self.slot_actionModeldialog),
            ('actionInput_Lens_Idenfier', self.slot_actionInput_LensIdentifier),
            ('actionAll_Lens_Data', self.slot_text, 'RTG ALL'),
            # Lens view
            ('actionSet_ray_input_angle', self.slot_actionRay_input_angle),
            ('actionSet_Focus', self.slot_actionFocus),
            # Lens Data (Non-surface)
            ('actionLensData_LI', self.slot_text, 'LI\nLIC'),
            ('actionLensData_UNITS', self.slot_text, 'UNITS'),
            ('actionLensData_INI', self.slot_text, 'INI'),
            ('actionLensData_LTYPE', self.slot_text, 'LTYPE'),
            ('actionLensData_SPTWT', self.slot_text, 'SPTWT\nCW\nPCW\nSCW'),
            ('actionLensData_MODE', self.slot_text, 'MODE'),
            ('actionLensData_STOP', self.slot_text, 'ASTOP\nREFS'),
            ('actionLensData_FIELD', self.slot_actionLensData_FIELD),
            ('actionLensData_APS', self.slot_text, 'CAOB ALL'),
            # Lens Data (Surface)
            ('actionLensData_RTG', self.slot_text, 'RTG ALL'),
            ('actionLensData_RTGLBL', self.slot_text, 'RTGLBL ALL'),
            ('actionLensData_CTG', self.slot_text, 'CTG ALL'),
            ('actionLensData_CTGLBL', self.slot_text, 'CTGLBL ALL'),
            ('actionLensData_DUMOUT', self.slot_text, 'DUMOUT ALL'),
            ('actionLensData_CAOB', self.slot_text, 'CAOB ALL'),
            ('actionLensData_INR', self.slot_text, 'INR ALL\nINR2 ALL'),
            ('actionLensData_SPIDER', self.slot_text, 'SPIDER ALL'),
            ('actionLensData_TAD', self.slot_text, 'TAD ALL'),
            ('actionLensData_PIVOT', self.slot_text, 'PIVOT ALL'),
            ('actionLensData_ASPH', self.slot_text, 'ASPH ALL\nASPH2 ALL'),
            ('actionLensData_ARRAY', self.slot_text, 'ARRAY ALL'),
            ('actionLensData_DEFORM', self.slot_text, 'DEFORM ALL'),
            ('actionLensData_THM', self.slot_text, 'THM ALL'),
            ('actionLensData_TR', self.slot_text, 'TR ALL'),
            ('actionLensData_TC', self.slot_text, 'TC ALL'),
            ('actionLensData_TASPH', self.slot_text, 'TASPH ALL'),
            ('actionLensData_SLV', self.slot_text, 'SLV ALL'),
            ('actionLensData_PIK', self.slot_text, 'PIK ALL'),
            ('actionLensData_RIN', self.slot_text, 'RIN ALL\nRIN2 ALL'),
            ('actionLensData_NDEX', self.slot_text, 'NDEX ALL\nNDEX2 ALL'),
            ('actionLensData_FOOTBLOK', self.slot_text, 'FOOTBLOK ALL'),
            ('actionLensData_SPGR', self.slot_text, 'SPGR ALL'),
            ('actionLensData_PRICE', self.slot_text, 'PRICE ALL'),
            ('actionLensData_GRT', self.slot_text, 'GRT ALL'),
            ('actionLensData_PRSPR', self.slot_text, 'PRSPR ALL'),
            ('actionLensData_CONFIGS', self.slot_text, 'CONFIGS ALL'),
            # Image Evaluation
            ('actionApod_Settings', self.slot_actionApod_Settings),
            ('actionDifset_Settings', self.slot_actionDifset_Settings),
            # Optimize
            ('actionOptimizer', self.slot_actionOptimizer),
        ]

        for entry in actions:
            attr_name = entry[0]
            handler = entry[1]
            args = entry[2:]

            # If handler is a string attribute name, resolve it
            if isinstance(handler, str):
                handler = getattr(self, handler)

            action = getattr(self, attr_name)
            if args:
                action.triggered.connect(
                    lambda _checked=False, h=handler, a=args: h(*a))
            else:
                action.triggered.connect(handler)

        # Radius/Curvature display-mode combo box (mirrors original RDM flag).
        # Connect both signals: currentTextChanged (programmatic / editable
        # line-edit updates) and activated (user picks from the drop-down,
        # which does NOT reliably fire currentTextChanged when the box is
        # editable + read-only).
        self.comboRadiusCurvature.currentTextChanged.connect(
            self._on_radius_curvature_changed)
        self.comboRadiusCurvature.activated.connect(
            lambda idx: self._on_radius_curvature_changed(
                self.comboRadiusCurvature.itemText(idx)))

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
