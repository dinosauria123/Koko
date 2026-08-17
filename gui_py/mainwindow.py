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
PLOT_TRIGGER_PREFIXES = (
    'VIE', 'SPD', 'CAPFN', 'PSF', 'DIST', 'FLDCV', 'AST', 'CHRSHIFT',
    'FANS', 'DRAW', 'DRAWFAN', 'GRAOUT', 'PLT', 'PLOT ', 'SPOT', 'DOTF',
    'GOTF', 'FAN', 'RAY', 'PARAX',
)

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
            # PLTOBJ is an internal IMTRACE瞄准 command (no plot output) and
            # PLTIMG writes PLOTBMP.BMP directly (handled by the ImageBlur
            # dialog's _schedule_image_render, NOT the gnuplot plot renderer).
            # Exclude both from the auto plot-render trigger so they don't
            # pop an intermediate/empty plot window mid Image-Blur run.
            if first in ('PLTOBJ', 'PLTIMG'):
                continue
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
        #
        # IMPORTANT: never strip a sent command from an RTG ALL data row.
        # A data row such as " 2 ... RADHARD BK7G18 ..." (or a marker row like
        # " 1* ... RADHARD BK7G18 ...") contains the exact string we just sent
        # ("RADHARD BK7G18"); a blanket replace() would delete the glass name
        # from the built-in terminal, leaving the index and V-number orphaned.
        # We therefore keep every RTG surface-data row verbatim (any line that
        # starts with a surface number, with or without a "*" marker) and only
        # strip echoes that sit on a koko prompt line (e.g. " 3:uln> RADHARD
        # BK7G18"), so the glass name stays visible.
        out_lines = []
        for line in text.split('\n'):
            if ((re.match(r'^\s*\d+', line)
                 and not re.search(r':(?:cmd|uln)>', line))
                    or 'BASIC LENS DATA' in line
                    or (line.strip().startswith('SURF') and 'RADIUS' in line)):
                out_lines.append(line)
                continue
            for c in getattr(self, '_sent_cmds', []):
                if c:
                    line = line.replace(c, '')
            out_lines.append(line)
        text = '\n'.join(out_lines)
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
            dlg = NKDialog(self, catalogs=self._load_glass_catalogs())
            dlg.lineEdit.setText(val)
            if dlg.exec() == QDialog.DialogCode.Accepted:
                command = dlg.material_command()
                if command:
                    self._send_surface_cmd(row, command)
                    if dlg.material_type() == 'MODEL':
                        self.send_koko("FINDGLASS %d" % row)
                    return
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
             'PSFPLOT YES', 'PSF', 'CAPFNOUT'),
            ('actionDistortion', self.slot_plot, 'DIST', 'PLTDIST'),
            ('actionField_Curvature', self.slot_plot, 'FLDCV', 'PLTFLDCV'),
            ('actionAstigmatism', self.slot_plot, 'AST', 'PLTAST'),
            ('actionGeometical', self.slot_plot, 'SPACE I', 'SPACE O', 'FAR',
             'GOTF', 'PLTGOTF,1', 'DRAW'),
            ('actionGeometical_Leica', self.slot_plot, 'SPACE I', 'SPACE O',
             'FAR', 'GOTF', 'PLTGOTF LEICA,1', 'DRAW'),
            ('actionDiffraction', self.slot_plot, 'SPACE I', 'SPACE O', 'FAR',
             'DOTF', 'PLTDOTF,,1', 'DRAW'),
            ('actionDiffraction_Leica', self.slot_plot, 'SPACE I', 'SPACE O',
             'FAR', 'DOTF', 'PLTDOTF LEICA,,1', 'DRAW'),
            ('actionParaxial_Chromatic_Focus_Shift', self.slot_plot, 'CHRSHIFT', 'PLTCHRSH'),
            # Ray (single ray trace) and Paraxial data displays
            ('actionRay_Single', self.slot_actionRay_single),
            ('actionPikup', self.slot_actionPikup),
            ('actionAperture', self.slot_actionAperture),
            ('actionObscuration', self.slot_actionObscuration),
            ('actionTilt', self.slot_actionTilt),
            ('actionVie', self.slot_actionVie),
            ('actionPlotDetail', self.slot_actionPlotDetail),
            ('actionSurtype', self.slot_actionSurtype),
            ('actionCoating', self.slot_actionCoating),
            ('actionPivaxis', self.slot_actionPivaxis),
            ('actionGlassLib', self.slot_actionGlassLib),
            ('actionStop', self.slot_actionStop),
            ('actionRef', self.slot_actionRef),
            ('actionDec', self.slot_actionDec),
            ('actionMacro', self.slot_actionMacro),
            ('actionNss', self.slot_actionNss),
            ('actionToper', self.slot_actionToper),
            ('actionGlassMap', self.slot_actionGlassMap),
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
            ('actionImage_Blur', self.slot_actionImageBlur),
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

    def _wait_for_psf_gri(self, path, timeout_s=30):
        """Poll ~/KODS/PSFGRI.DAT until PSF has written GRIIMG, return it."""
        import time
        base = 0.0
        try:
            base = os.path.getmtime(path)
        except OSError:
            base = 0.0
        deadline = time.time() + timeout_s
        while time.time() < deadline:
            try:
                m = os.path.getmtime(path)
            except OSError:
                m = 0.0
            if m > base:
                try:
                    with open(path) as fh:
                        val = float(fh.read().strip())
                    return val
                except (OSError, ValueError):
                    return 0.0
            time.sleep(0.3)
        return 0.0

    def _schedule_image_render(self):
        """Wait for koko to write ~/PLOTBMP.BMP, then display it."""
        if getattr(self, '_img_poll_active', False):
            self._img_pending = True
            return
        self._img_poll_active = True
        self._img_poll_pending = False
        try:
            base = os.path.getmtime(self._pending_image)
        except OSError:
            base = 0.0
        self._img_poll = {'base': base, 'tries': 0, 'max': 600}
        QTimer.singleShot(1000, self._poll_image_render)

    def _poll_image_render(self):
        self._img_poll['tries'] += 1
        try:
            m = os.path.getmtime(self._pending_image)
        except OSError:
            m = 0.0
        done = m > self._img_poll['base']
        if done or self._img_poll['tries'] >= self._img_poll['max']:
            self._img_poll_active = False
            self._show_image_result()
        else:
            # Report progress roughly every 10s.
            if self._img_poll['tries'] % 10 == 0:
                self.append_msg(
                    "** Image Blur: tracing... (%d s) **"
                    % (self._img_poll['tries'] // 10 * 4))
            QTimer.singleShot(400, self._poll_image_render)

    def _show_image_result(self):
        path = self._pending_image
        if not os.path.isfile(path) or os.path.getsize(path) == 0:
            self.append_msg("** Image Blur: no output BMP produced **")
            return
        # Show via the existing plot viewer (handles BMP via QPixmap).
        self.show_plot(path)


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
    def _synthesize_psf_plot_lines(self, gpl_dir):
        """Build gnuplot 'plot' lines for a PSF/plot-family figure.

        Some plot families (PSF in particular) rewrite the per-colour data
        files (black.gpl, red.gpl, ...) but never emit a "plot [...]" command
        into drawcmd3.gpl (their DRAW pass re-clears the body before
        drawcmdsave concatenates it, wiping the plot line). Without a plot
        line the rebuilt drawcmd.gpl shows labels but no curve.

        This reconstructs the plot line(s) from whichever data files koko
        actually wrote and that contain real (non-empty) point data. Mirrors
        the colour routing in koko's gnuplot.f (black=130, yellow=115,
        magenta=116, red=117, cyan=118).
        """
        candidates = [
            ('black.gpl',  'black',      '0.70'),
            ('yellow.gpl', 'dark-yellow', '0.70'),
            ('magenta.gpl', 'magenta',   '0.70'),
            ('red.gpl',    'red',        '0.70'),
            ('cyan.gpl',   'cyan',       '0.70'),
        ]
        lines = []
        for fname, colour, lw in candidates:
            path = os.path.join(gpl_dir, fname)
            if not os.path.isfile(path) or os.path.getsize(path) == 0:
                continue
            try:
                with open(path) as fh:
                    content = fh.read().strip()
            except OSError:
                continue
            if not content:
                continue
            has_point = False
            for ln in content.splitlines():
                s = ln.strip()
                if not s:
                    continue
                parts = s.split()
                if len(parts) >= 2 and parts[0].lstrip('-').isdigit() \
                        and parts[1].lstrip('-').isdigit():
                    has_point = True
                    break
            if not has_point:
                continue
            lines.append(
                'plot [0:10000] [0:7000] "%s" lc rgb "%s" lw %s w l'
                % (path, colour, lw))
        return '\n'.join(lines)
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
        # Some plot families (notably PSF, via PLOTPSF -> PLTDEV ->
        # drawcmd3_clear) rewrite the body file drawcmd3.gpl and the per-
        # plot colour data files (black/red/yellow.gpl ...), but koko does
        # NOT call drawcmdsave for them, so the concatenated drawcmd.gpl is
        # left stale (it still carries the PREVIOUS figure's header/labels).
        # In that case the labels from the old plot overprint the new one.
        # Detect the staleness by comparing drawcmd.gpl's mtime against the
        # body file drawcmd3.gpl; if the body is newer, rebuild drawcmd.gpl
        # from its header (drawcmd0.gpl) + body (drawcmd3.gpl), exactly the
        # way koko's drawcmdsave would have.
        gpl_dir = os.path.dirname(gpl)
        body = os.path.join(gpl_dir, 'drawcmd3.gpl')
        header = os.path.join(gpl_dir, 'drawcmd0.gpl')
        need_rebuild = False
        if os.path.isfile(body):
            try:
                if os.path.getmtime(body) > os.path.getmtime(gpl) + 0.001:
                    need_rebuild = True
            except OSError:
                need_rebuild = False
        if need_rebuild and os.path.isfile(header) \
                and os.path.getsize(header) > 0 and os.path.getsize(body) > 0:
            try:
                with open(header, 'r') as fh:
                    htxt = fh.read()
                with open(body, 'r') as fb:
                    btxt = fb.read()
                # The body (drawcmd3.gpl) written by PSF/plot families may
                # NOT contain a "plot [...] black.gpl ..." line: those
                # families route the draw through setonecolors -> DRAW, and
                # the DRAW's own PLTDEV re-clears the body (unit 150) BEFORE
                # drawcmdsave concatenates it, wiping the plot line. So after
                # rebuild the figure would show the new labels but no curve.
                # Synthesize the missing plot line(s) from the data files
                # koko definitely wrote (black/red/yellow/magenta/cyan.gpl).
                if 'plot [' not in btxt and 'plot[' not in btxt:
                    plot_lines = self._synthesize_psf_plot_lines(gpl_dir)
                    if plot_lines:
                        btxt = btxt.rstrip('\n') + '\n' + plot_lines + '\n'
                with open(gpl, 'w') as fg:
                    fg.write(htxt)
                    # The body may itself start with an "unset label" line
                    # generated by PLTDEV; keep it (it clears stale labels).
                    fg.write(btxt)
                self.append_msg(
                    "** rebuilt stale drawcmd.gpl from header+body (PSF/plot "
                    "family did not call drawcmdsave) **")
            except OSError as _e:
                self.append_msg(
                    "** could not rebuild drawcmd.gpl: %s **" % _e)

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
            from PyQt6.QtWidgets import QLabel, QVBoxLayout
            self.plot_window = PlotWindow(self)
            self.plot_window.setWindowTitle("Koko Plot")
            self._plot_label = QLabel()
            self._plot_label.setScaledContents(True)
            lay = QVBoxLayout(self.plot_window)
            lay.setContentsMargins(0, 0, 0, 0)
            lay.addWidget(self._plot_label)
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


class PlotWindow(QWidget):
    """Top-level plot viewer window.

    On close it notifies its owner (the KokoMainWindow) so the PNG it was
    displaying can be deleted and the window reference reset -- the next
    plot then builds a fresh window + fresh PNG instead of reusing a stale
    one (which caused the "overprint" artifact).
    """

    def __init__(self, owner):
        # No parent -> real top-level window with normal window decoration
        # (title bar + close button). A parented QWidget is rendered by the
        # WM as a frameless child window that cannot be closed.
        super().__init__(None)
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


class _WinMouseEvent:
    """Adapter that wraps a QLabel-originated mouse event and reports its
    position in the *window* (GlassMapWindow) coordinate system, so the
    window's mousePressEvent can treat label clicks identically to direct
    window clicks. The ``_is_adapter`` flag tells the window handler NOT to
    re-dispatch to QWidget.mousePressEvent (which only accepts real events)."""

    _is_adapter = True

    def __init__(self, window, src_event):
        self._window = window
        self._src = src_event
        # Convert the source (label-local) position into window-global then
        # window-local coordinates.
        self._pos = QPointF(window.mapFromGlobal(src_event.globalPosition().toPoint()))

    def position(self):
        return self._pos

    def globalPosition(self):
        return self._src.globalPosition()

    def button(self):
        return self._src.button()

    def buttons(self):
        return self._src.buttons()

    def modifiers(self):
        return self._src.modifiers()


class GlassMapWindow(PlotWindow):
    """n-v glass-map viewer. Clicking the plot maps the pixel coordinate
    back to (n, v) data space (using the fixed gnuplot margins/ranges the
    PNG was rendered with) and reports the nearest glass."""

    def __init__(self, owner, glasses, geom):
        super().__init__(owner)
        self._glasses = glasses
        # geom: dict with vmin,vmax,nmin,nmax,width,height,lmargin,rmargin,
        #       tmargin,bmargin
        self._geom = geom
        # Build the plot label up front so _plot in the dialog can set the
        # pixmap directly.
        self._plot_label = QLabel()
        self._plot_label.setScaledContents(True)
        self._plot_label.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        # The QLabel does NOT forward mouse events to its parent by default,
        # so clicks land on the label and never reach this window's
        # mousePressEvent. Install a small filter that re-routes label clicks
        # to self.mousePressEvent so clicking anywhere on the plot works.
        self._plot_label.mousePressEvent = (
            lambda ev: self.mousePressEvent(
                _WinMouseEvent(self, ev)))
        # Result bar: shows the glass identified on the last click, directly
        # inside the map window so the user sees feedback without hunting
        # through the main message log.
        self._click_label = QLabel("Click a point on the map to identify the glass")
        self._click_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._click_label.setStyleSheet(
            "QLabel { background-color: #eef0f2; border-top: 1px solid #c8ccd0; "
            "padding: 6px; font-weight: bold; }")
        self._click_label.setMinimumHeight(28)
        # Lay the plot (filling) above the result bar.
        lay = QVBoxLayout(self)
        lay.setContentsMargins(0, 0, 0, 0)
        lay.setSpacing(0)
        lay.addWidget(self._plot_label, 1)
        lay.addWidget(self._click_label, 0)
        # Keep this window above the main window / catalog picker so the map
        # is always visible in front (the WM would otherwise let it sink
        # behind the main window).
        self.setWindowFlag(Qt.WindowType.WindowStaysOnTopHint, True)
        # The window is sized to the rendered PNG in _plot() — the client
        # area grows by the result-bar height there.

    def mousePressEvent(self, event):
        if self._glasses and self._geom:
            # A click may arrive either on this window (event position is in
            # window coords) or on the child plot QLabel (event position is in
            # label coords, re-routed through a _WinMouseEvent adapter). Both
            # are normalized to window coordinates here.
            wp = event.position().toPoint()
            lp = self._plot_label.mapFrom(self, wp)
            self._report_click(lp.x(), lp.y(), self._plot_label.size())
        if getattr(event, "_is_adapter", False):
            # Already handled; do not re-dispatch to QWidget (it rejects the
            # adapter as a non-QMouseEvent).
            return
        super().mousePressEvent(event)

    def closeEvent(self, event):
        super().closeEvent(event)
        if self._owner is not None and hasattr(self._owner, "glass_map_window"):
            self._owner.glass_map_window = None
        self._owner = None

    def _report_click(self, px, py, label_size):
        g = self._geom
        # The label may be scaled to fit the window; scale the click back to
        # the logical 640x480 plot coordinate system.
        sx = g["width"] / label_size.width() if label_size.width() else 1.0
        sy = g["height"] / label_size.height() if label_size.height() else 1.0
        px *= sx
        py *= sy
        # px, py are now in the logical 640x480 plot coordinate system but in
        # Qt's TOP-left origin. gnuplot's term_* values use a BOTTOM-left
        # (PNG) origin, so flip Y to match before mapping.
        py = g["height"] - py
        # Prefer gnuplot's ACTUAL rendered plot rectangle (term_*) so the
        # click maps exactly to what was drawn. Fall back to the margin-based
        # rectangle if gnuplot didn't report one.
        if all(k in g for k in ("term_xmin", "term_xmax",
                                "term_ymin", "term_ymax")):
            x0, x1 = g["term_xmin"], g["term_xmax"]
            y0, y1 = g["term_ymin"], g["term_ymax"]
        else:
            x0 = g["lmargin"]
            x1 = g["width"] - g["rmargin"]
            y0 = g["tmargin"]
            y1 = g["height"] - g["bmargin"]
        plot_w = x1 - x0
        plot_h = y1 - y0
        if plot_w <= 0 or plot_h <= 0:
            return
        # Clamp to plot area.
        x = min(max(px, x0), x1)
        y = min(max(py, y0), y1)
        # Map pixel -> data.
        #   x axis = Abbe number v (Vd), LARGE at LEFT  -> xrange [vmax, vmin]
        #   y axis = refractive index n (Nd), small at bottom -> [nmin, nmax]
        # gnuplot's Y origin is the LOWER edge of the PNG (py already flipped
        # to bottom-left above).
        frac_x = (x - x0) / plot_w
        frac_y = (y - y0) / plot_h
        v = g["vmax"] - frac_x * (g["vmax"] - g["vmin"])
        n = g["nmin"] + frac_y * (g["nmax"] - g["nmin"])
        # Nearest glass in (n, v) space. We also measure the click-to-glass
        # distance in PLOT PIXELS so we can reject clicks that landed in empty
        # space (no dot near the cursor). The plotted dots are ~ps 1.1, so we
        # accept a hit within a small radius (a few dot-widths) of a point.
        best = None
        best_d = None
        best_px = None
        for gl in self._glasses:
            dn = gl["nd"] - n
            dv = gl["vd"] - v
            d = dn * dn + dv * dv
            if best_d is None or d < best_d:
                best_d = d
                best = gl
                # pixel position of this glass: x from v (left=large), y from n
                gfx = (g["vmax"] - gl["vd"]) / (g["vmax"] - g["vmin"]) * plot_w
                gfy = (gl["nd"] - g["nmin"]) / (g["nmax"] - g["nmin"]) * plot_h
                best_px = ((x - (x0 + gfx)) ** 2 + (y - (y0 + gfy)) ** 2) ** 0.5
        # Hit radius in plot pixels: dots are ~5px radius (ps 1.1), so a
        # generous radius keeps clicks that land just off-center from being
        # rejected as empty space.
        HIT_RADIUS_PX = 20.0
        if best is not None and best_px is not None and best_px <= HIT_RADIUS_PX:
            msg = ("Glass: %s  (catalog %s)\n"
                   "  n (Nd) = %.5f\n"
                   "  v (Vd) = %.3f" % (best["name"], best["catalog"],
                                       best["nd"], best["vd"]))
            if self._owner is not None and hasattr(self._owner, "append_msg"):
                self._owner.append_msg(">> Glass map click: " + msg.replace("\n", "  "))
            self.setWindowTitle("Glass Map — " + best["name"])
            # Show the result in the window's own result bar so the user
            # gets immediate, visible feedback on click.
            if self._click_label is not None:
                self._click_label.setText(
                    "Glass: %s  (catalog %s)    n = %.5f    v = %.3f"
                    % (best["name"], best["catalog"], best["nd"], best["vd"]))
        else:
            # No dot near the click: clicking empty space -> report none.
            self.setWindowTitle("Glass Map — (no glass here)")
            if self._click_label is not None:
                self._click_label.setText("No glass — click nearer to a data point")


class GlassMapDialog(QDialog):
    """Dialog to choose glass catalogs and render the n-v (index vs Abbe)
    glass map. Renders via gnuplot (pngcairo) and opens a GlassMapWindow
    where clicking reports the nearest glass."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Glass Map (n vs v)")
        self.resize(360, 200)
        self._glasses = []
        self._glass_map_window = None

        vbox = QVBoxLayout(self)
        hdr = QLabel("Glass catalog n–v map")
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        hdr.setStyleSheet(
            "QLabel { background-color: #eef0f2; border-bottom: 1px solid "
            "#c8ccd0; padding: 6px; font-weight: bold; }")
        vbox.addWidget(hdr)

        vbox.addWidget(QLabel("Select catalogs to plot:"))

        self._cat_checks = {}
        import gui_py.glassmap as gm
        for cat in gm.list_catalogs():
            cb = QCheckBox(cat)
            cb.setChecked(True)
            self._cat_checks[cat] = cb
            vbox.addWidget(cb)

        hbox = QHBoxLayout()
        self.btn_plot = QPushButton("Plot")
        self.btn_close = QPushButton("Close")
        hbox.addWidget(self.btn_plot)
        hbox.addWidget(self.btn_close)
        vbox.addLayout(hbox)

        self.btn_plot.clicked.connect(self._plot)
        self.btn_close.clicked.connect(self.reject)

    def _plot(self):
        import os
        import tempfile
        import subprocess
        import shutil
        import gui_py.glassmap as gm

        gnuplot_bin = shutil.which("gnuplot") or "gnuplot"
        env = dict(os.environ)
        env["DISPLAY"] = ""

        cats = [c for c, cb in self._cat_checks.items() if cb.isChecked()]
        if not cats:
            QMessageBox.information(self, "Glass Map",
                                    "Select at least one catalog.")
            return
        glasses = gm.load_all_glasses(catalogs=cats)
        if not glasses:
            QMessageBox.information(self, "Glass Map", "No glasses found.")
            return
        self._glasses = glasses

        vmin, vmax, nmin, nmax = gm.compute_ranges(glasses)
        # Margins are PIXELS (relative to the 640x480 render). They are used
        # both by gnuplot (converted to screen fractions in glassmap.py) and
        # by the click-to-glass mapping in GlassMapWindow._report_click, so
        # the two coordinate systems stay exactly in sync.
        geom = dict(vmin=vmin, vmax=vmax, nmin=nmin, nmax=nmax,
                    width=640, height=480,
                    lmargin=70, rmargin=20, tmargin=50, bmargin=60)

        tmp = tempfile.mkdtemp(prefix="koko_glassmap_")
        data_path = os.path.join(tmp, "glassmap.dat")
        script_path = os.path.join(tmp, "glassmap.gpl")
        png_path = os.path.join(tmp, "glassmap.png")
        gm.write_gnuplot_data(glasses, data_path)
        gm.build_gnuplot_script(data_path, script_path, png_path,
                                "Glass Map (v vs n)", vmax, vmin, nmin, nmax,
                                width=geom["width"], height=geom["height"],
                                lmargin=geom["lmargin"], rmargin=geom["rmargin"],
                                tmargin=geom["tmargin"], bmargin=geom["bmargin"])

        r = subprocess.run([gnuplot_bin, script_path], env=env,
                           capture_output=True, text=True, timeout=30)
        if r.returncode != 0 or not os.path.exists(png_path):
            QMessageBox.critical(self, "Glass Map",
                                 "gnuplot failed:\n" + (r.stderr or r.stdout))
            return

        # Capture the ACTUAL rendered plot rectangle (PNG pixel coords) so
        # click-to-glass mapping matches gnuplot's real layout exactly.
        # gnuplot's `print` writes to stderr, so read it from there.
        rect = gm.parse_plot_rect(r.stderr)
        if rect is not None:
            geom["term_xmin"], geom["term_xmax"], \
                geom["term_ymin"], geom["term_ymax"] = rect
        # Pass the main window as owner so click reports can be written to
        # its message log via _owner.append_msg(). The GlassMapWindow itself
        # is a top-level window (no parent) so it never hides behind others.
        owner = self.parent()
        win = GlassMapWindow(owner, glasses, geom)
        win.setWindowTitle("Glass Map (n vs v) — %d glasses" % len(glasses))
        pix = QPixmap(png_path)
        win._plot_label.setPixmap(pix)
        win._plot_label.setScaledContents(True)
        if win.layout() is not None:
            win.layout().setContentsMargins(0, 0, 0, 0)
        win.show()
        QApplication.processEvents()
        fw = win.frameGeometry().width() - win.geometry().width()
        fh = win.frameGeometry().height() - win.geometry().height()
        # Include the result bar (its current height) so it is not clipped by
        # the fixed-size window.
        bar_h = win._click_label.height() if win._click_label is not None else 0
        win.setFixedSize(pix.width() + fw, pix.height() + bar_h + fh)
        # Keep reference so it's not GC'd
        self._glass_map_window = win
        win.raise_()
        win.activateWindow()
        # Close the catalog picker now that the map is on screen. The map
        # window is a top-level window with its own lifecycle, so it stays
        # alive for click-to-identify after the picker closes.
        self.accept()


def main():
    import sys
    app = QApplication(sys.argv)
    window = KokoMainWindow()
    window.show()
    sys.exit(app.exec())


if __name__ == '__main__':
    main()
