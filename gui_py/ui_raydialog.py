# Form implementation for the single-ray trace dialog.
# Mirrors the original IDD_RAY (KDP2 RAYS.INC / GUICODE.FOR) flow:
# the user supplies normalized field (X,Y) coordinates and koko traces
# that single ray with the RAY command.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_RayDialog(object):
    def setupUi(self, RayDialog):
        RayDialog.setObjectName("RayDialog")
        RayDialog.resize(320, 180)
        RayDialog.setWindowTitle("Single Ray Trace")

        self.verticalLayout = QtWidgets.QVBoxLayout(RayDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(RayDialog)
        self.header.setObjectName("header")
        self.header.setText("Trace a single ray")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # X / Y inputs
        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setObjectName("formLayout")
        self.formLayout.setContentsMargins(12, 12, 12, 12)

        self.label_x = QtWidgets.QLabel(RayDialog)
        self.label_x.setText("Field X (normalized)")
        self.lineEdit_x = QtWidgets.QLineEdit(RayDialog)
        self.lineEdit_x.setText("0.0")
        self.lineEdit_x.setObjectName("lineEdit_x")
        self.formLayout.addRow(self.label_x, self.lineEdit_x)

        self.label_y = QtWidgets.QLabel(RayDialog)
        self.label_y.setText("Field Y (normalized)")
        self.lineEdit_y = QtWidgets.QLineEdit(RayDialog)
        self.lineEdit_y.setText("0.0")
        self.lineEdit_y.setObjectName("lineEdit_y")
        self.formLayout.addRow(self.label_y, self.lineEdit_y)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(RayDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(RayDialog)
        self.buttonBox.accepted.connect(RayDialog.accept)
        self.buttonBox.rejected.connect(RayDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(RayDialog)

    def retranslateUi(self, RayDialog):
        pass
