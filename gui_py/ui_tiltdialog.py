# Form implementation for the tilt (TILT) dialog.
# Mirrors the original IDD_TILTS / IDD_TILT / IDD_TILTAUTO / IDD_TILTBEN /
# IDD_TILTRET / IDD_TILTDAR / IDD_TILTREV (KDP2 GUICODE.FOR) flows:
#   basic   : U L -> CHG <surf> -> TILT <alpha> <beta> <gamma> -> EOS
#   auto    : U L -> CHG <surf> -> TILT AUTO   -> EOS
#   dard    : U L -> CHG <surf> -> TILT DARD   -> EOS
#   bend    : U L -> CHG <surf> -> TILT BEND   -> EOS
#   rev     : U L -> CHG <surf> -> TILT REV    -> EOS
#   rtilt   : U L -> CHG <surf> -> RTILT        -> EOS
#   tildd   : U L -> CHG <surf> -> TILTD        -> EOS  (delete tilt)

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_TiltDialog(object):
    def setupUi(self, TiltDialog):
        TiltDialog.setObjectName("TiltDialog")
        TiltDialog.resize(340, 300)
        TiltDialog.setWindowTitle("Surface Tilt (TILT)")

        self.verticalLayout = QtWidgets.QVBoxLayout(TiltDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(TiltDialog)
        self.header.setObjectName("header")
        self.header.setText("Set surface tilt")
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
        self.label_surf = QtWidgets.QLabel(TiltDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(TiltDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # Type
        self.label_type = QtWidgets.QLabel(TiltDialog)
        self.label_type.setText("Tilt type")
        self.combo_type = QtWidgets.QComboBox(TiltDialog)
        self.combo_type.setObjectName("combo_type")
        self.combo_type.addItems(
            ["Basic (alpha/beta/gamma)", "Auto", "DARD", "BEND",
             "REV", "RTILT (reverse)", "TILTD (delete)"])
        self.formLayout.addRow(self.label_type, self.combo_type)

        # Alpha / Beta / Gamma (basic only)
        self.label_alpha = QtWidgets.QLabel(TiltDialog)
        self.label_alpha.setText("Alpha (deg)")
        self.lineEdit_alpha = QtWidgets.QLineEdit(TiltDialog)
        self.lineEdit_alpha.setObjectName("lineEdit_alpha")
        self.lineEdit_alpha.setText("0.0")
        self.formLayout.addRow(self.label_alpha, self.lineEdit_alpha)

        self.label_beta = QtWidgets.QLabel(TiltDialog)
        self.label_beta.setText("Beta (deg)")
        self.lineEdit_beta = QtWidgets.QLineEdit(TiltDialog)
        self.lineEdit_beta.setObjectName("lineEdit_beta")
        self.lineEdit_beta.setText("0.0")
        self.formLayout.addRow(self.label_beta, self.lineEdit_beta)

        self.label_gamma = QtWidgets.QLabel(TiltDialog)
        self.label_gamma.setText("Gamma (deg)")
        self.lineEdit_gamma = QtWidgets.QLineEdit(TiltDialog)
        self.lineEdit_gamma.setObjectName("lineEdit_gamma")
        self.lineEdit_gamma.setText("0.0")
        self.formLayout.addRow(self.label_gamma, self.lineEdit_gamma)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(TiltDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(TiltDialog)
        self.buttonBox.accepted.connect(TiltDialog.accept)
        self.buttonBox.rejected.connect(TiltDialog.reject)
        self.combo_type.currentTextChanged.connect(self._on_type_changed)
        QtCore.QMetaObject.connectSlotsByName(TiltDialog)
        self._on_type_changed(self.combo_type.currentText())

    def _on_type_changed(self, text):
        is_basic = text.startswith("Basic")
        self.label_alpha.setVisible(is_basic)
        self.lineEdit_alpha.setVisible(is_basic)
        self.label_beta.setVisible(is_basic)
        self.lineEdit_beta.setVisible(is_basic)
        self.label_gamma.setVisible(is_basic)
        self.lineEdit_gamma.setVisible(is_basic)

    def retranslateUi(self, TiltDialog):
        pass
