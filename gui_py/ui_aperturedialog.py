# Form implementation for the circular clear-aperture (CLAP) dialog.
# Mirrors the original IDD_APECIRC (KDP2 GUICODE.FOR) flow:
#   U L -> CHG <surface> -> CLAP <rad> <xdecenter> <ydecenter> 0 0 -> EOS
# Sets a circular clear aperture on the given surface.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_ApertureDialog(object):
    def setupUi(self, ApertureDialog):
        ApertureDialog.setObjectName("ApertureDialog")
        ApertureDialog.resize(340, 220)
        ApertureDialog.setWindowTitle("Circular Aperture (CLAP)")

        self.verticalLayout = QtWidgets.QVBoxLayout(ApertureDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(ApertureDialog)
        self.header.setObjectName("header")
        self.header.setText("Set circular clear aperture")
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

        self.label_surf = QtWidgets.QLabel(ApertureDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(ApertureDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        self.label_rad = QtWidgets.QLabel(ApertureDialog)
        self.label_rad.setText("Radius")
        self.lineEdit_rad = QtWidgets.QLineEdit(ApertureDialog)
        self.lineEdit_rad.setObjectName("lineEdit_rad")
        self.lineEdit_rad.setText("5.0")
        self.formLayout.addRow(self.label_rad, self.lineEdit_rad)

        self.label_xdec = QtWidgets.QLabel(ApertureDialog)
        self.label_xdec.setText("X decenter")
        self.lineEdit_xdec = QtWidgets.QLineEdit(ApertureDialog)
        self.lineEdit_xdec.setObjectName("lineEdit_xdec")
        self.lineEdit_xdec.setText("0.0")
        self.formLayout.addRow(self.label_xdec, self.lineEdit_xdec)

        self.label_ydec = QtWidgets.QLabel(ApertureDialog)
        self.label_ydec.setText("Y decenter")
        self.lineEdit_ydec = QtWidgets.QLineEdit(ApertureDialog)
        self.lineEdit_ydec.setObjectName("lineEdit_ydec")
        self.lineEdit_ydec.setText("0.0")
        self.formLayout.addRow(self.label_ydec, self.lineEdit_ydec)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(ApertureDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(ApertureDialog)
        self.buttonBox.accepted.connect(ApertureDialog.accept)
        self.buttonBox.rejected.connect(ApertureDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(ApertureDialog)

    def retranslateUi(self, ApertureDialog):
        pass
