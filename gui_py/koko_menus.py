"""KokoMenuMixin: koko_menus.py mixin (see gui_py/mainwindow.py).
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

class KokoMenuMixin:
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
            ('actionExport_Leno_AC', self.slot_actionExport_Leno_AC),
            ('actionAbout', self.slot_actionAbout),
            # Lens View (plots)
            ('actionXZ', self.slot_plot, 'VIE XZ'),
            ('actionOrtho', self.slot_plot, 'VIE ORTHO'),
            # Analyze -> spot / wavefront / PSF
            ('actionSpot_Diagram', self.slot_actionSpot),
            ('actionWavefront_Phase', self.slot_plot, 'CAPFN', 'PLOT CAPFNOPD'),
            ('actionWavefront_Intensity', self.slot_plot, 'CAPFN', 'PLOT CAPFNAPD'),
            ('actionPoint_Spread_Function', self.slot_actionPsf),
            ('actionDistortion', self.slot_actionDist),
            ('actionField_Curvature', self.slot_actionFldcv),
            ('actionAstigmatism', self.slot_actionAst),
            ('actionGeometical', self.slot_actionGotf),
            ('actionDiffraction', self.slot_actionDotf),
            ('actionParaxial_Chromatic_Focus_Shift', self.slot_plot, 'CHRSHIFT', 'PLTCHRSH'),
            # Ray (single ray trace) and Paraxial data displays
            ('actionRay_Single', self.slot_actionRay_single),
            ('actionPikup', self.slot_actionPikup),
            ('actionSolve', self.slot_actionSolve),
            ('actionAsph', self.slot_actionAsph),
            ('actionGrtArray', self.slot_actionGrtArray),
            ('actionSpsrf', self.slot_actionSpsrf),
            ('actionBb', self.slot_actionBb),
            ('actionRayAux', self.slot_actionRayAux),
            ('actionCapfn', self.slot_actionCapfn),
            ('actionLensOps', self.slot_actionLensOps),
            ('actionMultiAperture', self.slot_actionMultiAperture),
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

