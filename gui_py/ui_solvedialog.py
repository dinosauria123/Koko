# Form implementation for the solve editor (SLVED) dialog.
# Mirrors the original IDD_SLVED (KDP2 GUICODE.FOR) flow:
#   U L -> CHG <surf> -> <SOLVE> <target> -> EOS
# where <SOLVE> is one of the 10 solve families, each with a Y/X plane:
#   marginal ray height : PY / PX
#   paraxial chief      : PCY / PCX
#   paraxial upper      : PUY / PUX
#   paraxial image      : PIY / PIX
#   paraxial upper chief: PUCY / PUCX
#   paraxial image chief: PICY / PICX
#   concentric          : COCY / COCX
#   clear aperture      : CAY / CAX
#   aperture            : APY / APX
#   aperture chief      : APCY / APCX
# Also offers PIKD (delete all pickups on the surface) and SLV ALL
# (list all solves) as utility buttons.

from PyQt6 import QtCore, QtGui, QtWidgets

# (display label, Y command, X command)
SOLVE_TYPES = [
    ("Marginal ray height (PY/PX)", "PY", "PX"),
    ("Paraxial chief ray (PCY/PCX)", "PCY", "PCX"),
    ("Paraxial upper ray (PUY/PUX)", "PUY", "PUX"),
    ("Paraxial image (PIY/PIX)", "PIY", "PIX"),
    ("Paraxial upper chief (PUCY/PUCX)", "PUCY", "PUCX"),
    ("Paraxial image chief (PICY/PICX)", "PICY", "PICX"),
    ("Concentric (COCY/COCX)", "COCY", "COCX"),
    ("Clear aperture (CAY/CAX)", "CAY", "CAX"),
    ("Aperture (APY/APX)", "APY", "APX"),
    ("Aperture chief (APCY/APCX)", "APCY", "APCX"),
]


class Ui_SolveDialog(object):
    def setupUi(self, SolveDialog):
        SolveDialog.setObjectName("SolveDialog")
        SolveDialog.resize(360, 300)
        SolveDialog.setWindowTitle("Solve Editor (SLVED)")

        self.verticalLayout = QtWidgets.QVBoxLayout(SolveDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(SolveDialog)
        self.header.setObjectName("header")
        self.header.setText("Set a solve on a surface")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setObjectName("formLayout")
        self.formLayout.setContentsMargins(12, 12, 12, 12)

        # Surface #
        self.label_surf = QtWidgets.QLabel(SolveDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(SolveDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # Solve type
        self.label_type = QtWidgets.QLabel(SolveDialog)
        self.label_type.setText("Solve type")
        self.combo_type = QtWidgets.QComboBox(SolveDialog)
        self.combo_type.setObjectName("combo_type")
        self.combo_type.addItems([s[0] for s in SOLVE_TYPES])
        self.formLayout.addRow(self.label_type, self.combo_type)

        # Plane (Y / X)
        self.label_plane = QtWidgets.QLabel(SolveDialog)
        self.label_plane.setText("Plane")
        self.plane_widget = QtWidgets.QWidget(SolveDialog)
        ph = QtWidgets.QHBoxLayout(self.plane_widget)
        ph.setContentsMargins(0, 0, 0, 0)
        self.radio_y = QtWidgets.QRadioButton(self.plane_widget)
        self.radio_y.setObjectName("radio_y")
        self.radio_y.setText("Y (tangential)")
        self.radio_y.setChecked(True)
        self.radio_x = QtWidgets.QRadioButton(self.plane_widget)
        self.radio_x.setObjectName("radio_x")
        self.radio_x.setText("X (sagittal)")
        ph.addWidget(self.radio_y)
        ph.addWidget(self.radio_x)
        self.formLayout.addRow(self.label_plane, self.plane_widget)

        # Target value
        self.label_val = QtWidgets.QLabel(SolveDialog)
        self.label_val.setText("Target value")
        self.lineEdit_val = QtWidgets.QLineEdit(SolveDialog)
        self.lineEdit_val.setObjectName("lineEdit_val")
        self.lineEdit_val.setText("0.0")
        self.formLayout.addRow(self.label_val, self.lineEdit_val)

        self.verticalLayout.addLayout(self.formLayout)

        # Utility buttons (PIKD / SLV ALL)
        self.util_layout = QtWidgets.QHBoxLayout()
        self.util_layout.setContentsMargins(12, 0, 12, 0)
        self.btn_pikd = QtWidgets.QPushButton(SolveDialog)
        self.btn_pikd.setObjectName("btn_pikd")
        self.btn_pikd.setText("Delete all pickups (PIKD)")
        self.btn_slvall = QtWidgets.QPushButton(SolveDialog)
        self.btn_slvall.setObjectName("btn_slvall")
        self.btn_slvall.setText("List all solves (SLV ALL)")
        self.util_layout.addWidget(self.btn_pikd)
        self.util_layout.addWidget(self.btn_slvall)
        self.verticalLayout.addLayout(self.util_layout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(SolveDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(SolveDialog)
        self.buttonBox.accepted.connect(SolveDialog.accept)
        self.buttonBox.rejected.connect(SolveDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(SolveDialog)

    def retranslateUi(self, SolveDialog):
        pass
