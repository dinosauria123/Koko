# Form implementation for the PIKUP (parameter pickup) dialog.
# Mirrors the original IDD_PIKSLV / IDD_PIKED1-3 (KDP2) flow:
#   U L -> PIKUP <TYPE>,<surface>,<value> -> EOS
# where <TYPE> is one of the 44 koko pickup types (CV, RD, CC, TH, ...).

from PyQt6 import QtCore, QtGui, QtWidgets

# koko pickup type keywords (from LDM11.FOR / utility1.f format strings)
PIKUP_TYPES = [
    "CV", "RD", "CC", "TH", "AD", "AE", "AF", "AG",
    "RDTOR", "CVTOR", "CCTOR", "ADTOR", "AETOR", "AFTOR", "AGTOR",
    "ALPHA", "BETA", "GAMMA", "XD", "YD", "GLASS", "PRO", "NPRO",
    "ZD", "PIVX", "PIVY", "PIVZ", "GDX", "GDY", "GDZ",
    "GALPHA", "GBETA", "GGAMMA", "GRT",
    "AX", "AY", "AZ", "BX", "BY", "BZ",
    "PIVX2", "PIVY2", "PIVZ2", "CV2", "RD2", "CC2", "TH2",
]


class Ui_PikupDialog(object):
    def setupUi(self, PikupDialog):
        PikupDialog.setObjectName("PikupDialog")
        PikupDialog.resize(340, 200)
        PikupDialog.setWindowTitle("Parameter Pickup (PIKUP)")

        self.verticalLayout = QtWidgets.QVBoxLayout(PikupDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(PikupDialog)
        self.header.setObjectName("header")
        self.header.setText("Pick up a parameter value")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # inputs
        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setObjectName("formLayout")
        self.formLayout.setContentsMargins(12, 12, 12, 12)

        self.label_surf = QtWidgets.QLabel(PikupDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(PikupDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        self.label_type = QtWidgets.QLabel(PikupDialog)
        self.label_type.setText("Pickup type")
        self.combo_type = QtWidgets.QComboBox(PikupDialog)
        self.combo_type.setObjectName("combo_type")
        self.combo_type.addItems(PIKUP_TYPES)
        self.formLayout.addRow(self.label_type, self.combo_type)

        self.label_val = QtWidgets.QLabel(PikupDialog)
        self.label_val.setText("Value")
        self.lineEdit_val = QtWidgets.QLineEdit(PikupDialog)
        self.lineEdit_val.setObjectName("lineEdit_val")
        self.lineEdit_val.setText("0.0")
        self.formLayout.addRow(self.label_val, self.lineEdit_val)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(PikupDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(PikupDialog)
        self.buttonBox.accepted.connect(PikupDialog.accept)
        self.buttonBox.rejected.connect(PikupDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(PikupDialog)

    def retranslateUi(self, PikupDialog):
        pass
