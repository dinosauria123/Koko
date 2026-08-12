# Form implementation for the decenter (DEC) dialog.
# Mirrors KDP2 IDD_DEC (GUICODE.FOR) flow:
#   U L -> CHG <surf> -> DEC <y>,<x>,<z> -> EOS
# (koko accepts DEC X Y Z; we map to the KDP2 Y,X,Z order)

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_DecDialog(object):
    def setupUi(self, DecDialog):
        DecDialog.setObjectName("DecDialog")
        DecDialog.resize(320, 240)
        DecDialog.setWindowTitle("Decenter (DEC)")

        self.verticalLayout = QtWidgets.QVBoxLayout(DecDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(DecDialog)
        self.header.setObjectName("header")
        self.header.setText("Set surface decenter")
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
        self.label_surf = QtWidgets.QLabel(DecDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(DecDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # Decenter X / Y / Z
        self.label_x = QtWidgets.QLabel(DecDialog)
        self.label_x.setText("Decenter X")
        self.lineEdit_x = QtWidgets.QLineEdit(DecDialog)
        self.lineEdit_x.setObjectName("lineEdit_x")
        self.lineEdit_x.setText("0.0")
        self.formLayout.addRow(self.label_x, self.lineEdit_x)

        self.label_y = QtWidgets.QLabel(DecDialog)
        self.label_y.setText("Decenter Y")
        self.lineEdit_y = QtWidgets.QLineEdit(DecDialog)
        self.lineEdit_y.setObjectName("lineEdit_y")
        self.lineEdit_y.setText("0.0")
        self.formLayout.addRow(self.label_y, self.lineEdit_y)

        self.label_z = QtWidgets.QLabel(DecDialog)
        self.label_z.setText("Decenter Z")
        self.lineEdit_z = QtWidgets.QLineEdit(DecDialog)
        self.lineEdit_z.setObjectName("lineEdit_z")
        self.lineEdit_z.setText("0.0")
        self.formLayout.addRow(self.label_z, self.lineEdit_z)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(DecDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(DecDialog)
        self.buttonBox.accepted.connect(DecDialog.accept)
        self.buttonBox.rejected.connect(DecDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(DecDialog)

    def retranslateUi(self, DecDialog):
        pass
