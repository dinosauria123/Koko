# Form implementation for the view-control (VIE) dialog.
# Mirrors the original IDD_VIE (KDP2 LENSED.INC) flow:
#   VIE XZ|XY|ORTHO,<factor>     (lens layout view)
#   VIEVIG ON|OFF                 (vignetting display)
#   VIESYM ON|OFF                 (symmetric display)
# The factor scales the layout; the toggles are independent modifiers.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_VieDialog(object):
    def setupUi(self, VieDialog):
        VieDialog.setObjectName("VieDialog")
        VieDialog.resize(320, 260)
        VieDialog.setWindowTitle("View Control (VIE)")

        self.verticalLayout = QtWidgets.QVBoxLayout(VieDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(VieDialog)
        self.header.setObjectName("header")
        self.header.setText("Lens layout view")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setObjectName("formLayout")
        self.formLayout.setContentsMargins(12, 12, 12, 12)

        # View type
        self.label_type = QtWidgets.QLabel(VieDialog)
        self.label_type.setText("View")
        self.combo_type = QtWidgets.QComboBox(VieDialog)
        self.combo_type.setObjectName("combo_type")
        self.combo_type.addItems(["XZ", "XY", "ORTHO"])
        self.formLayout.addRow(self.label_type, self.combo_type)

        # Scale factor
        self.label_factor = QtWidgets.QLabel(VieDialog)
        self.label_factor.setText("Scale factor")
        self.lineEdit_factor = QtWidgets.QLineEdit(VieDialog)
        self.lineEdit_factor.setObjectName("lineEdit_factor")
        self.lineEdit_factor.setText("0.10")
        self.formLayout.addRow(self.label_factor, self.lineEdit_factor)

        # Vignetting display
        self.check_vig = QtWidgets.QCheckBox(VieDialog)
        self.check_vig.setObjectName("check_vig")
        self.check_vig.setText("Show vignetting (VIEVIG)")
        self.formLayout.addRow(self.check_vig)

        # Symmetric display
        self.check_sym = QtWidgets.QCheckBox(VieDialog)
        self.check_sym.setObjectName("check_sym")
        self.check_sym.setText("Symmetric display (VIESYM)")
        self.formLayout.addRow(self.check_sym)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(VieDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(VieDialog)
        self.buttonBox.accepted.connect(VieDialog.accept)
        self.buttonBox.rejected.connect(VieDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(VieDialog)

    def retranslateUi(self, VieDialog):
        pass
