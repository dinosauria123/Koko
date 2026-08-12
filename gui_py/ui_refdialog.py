# Form implementation for the reference-surface (REFS) dialog.
# Mirrors KDP2 IDD_REFSSURF (GUICODE.FOR) flow:
#   U L -> CHG <surf> -> REFS <rotation> -> EOS

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_RefDialog(object):
    def setupUi(self, RefDialog):
        RefDialog.setObjectName("RefDialog")
        RefDialog.resize(320, 180)
        RefDialog.setWindowTitle("Reference Surface (REFS)")

        self.verticalLayout = QtWidgets.QVBoxLayout(RefDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(RefDialog)
        self.header.setObjectName("header")
        self.header.setText("Set reference surface")
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
        self.label_surf = QtWidgets.QLabel(RefDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(RefDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(1)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # Rotation
        self.label_rot = QtWidgets.QLabel(RefDialog)
        self.label_rot.setText("Rotation (deg)")
        self.lineEdit_rot = QtWidgets.QLineEdit(RefDialog)
        self.lineEdit_rot.setObjectName("lineEdit_rot")
        self.lineEdit_rot.setText("0.0")
        self.formLayout.addRow(self.label_rot, self.lineEdit_rot)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(RefDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(RefDialog)
        self.buttonBox.accepted.connect(RefDialog.accept)
        self.buttonBox.rejected.connect(RefDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(RefDialog)

    def retranslateUi(self, RefDialog):
        pass
