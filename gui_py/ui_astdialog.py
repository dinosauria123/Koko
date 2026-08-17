# Form implementation for the astigmatism (AST) settings dialog
# (split out of the former combined IDD_DISAST dialog). koko commands:
#   AST,<orient>,,<n>   compute astigmatism (orient: 0 / 90 deg)
#   PLTAST,,1           plot the result
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_AstDialog(object):
    def setupUi(self, AstDialog):
        AstDialog.setObjectName("AstDialog")
        AstDialog.resize(380, 250)
        self.verticalLayout = QtWidgets.QVBoxLayout(AstDialog)
        self.verticalLayout.setContentsMargins(0, 0, 0, 10)
        self.verticalLayout.setSpacing(8)

        # Header band
        self.header = QtWidgets.QLabel(AstDialog)
        self.header.setObjectName("header")
        self.header.setText("Astigmatism (AST)")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # Settings form
        self.body = QtWidgets.QWidget(AstDialog)
        alf = QtWidgets.QFormLayout(self.body)
        alf.setContentsMargins(16, 12, 16, 4)
        alf.setHorizontalSpacing(16)
        alf.setVerticalSpacing(8)
        self.label_orient = QtWidgets.QLabel(self.body)
        self.label_orient.setText("Orientation")
        self.combo_orient = QtWidgets.QComboBox(self.body)
        self.combo_orient.addItems(["0 deg (tangential)", "90 deg (sagittal)"])
        alf.addRow(self.label_orient, self.combo_orient)
        self.label_n = QtWidgets.QLabel(self.body)
        self.label_n.setText("Field points")
        self.spin_n = QtWidgets.QSpinBox(self.body)
        self.spin_n.setRange(10, 50)
        self.spin_n.setValue(20)
        alf.addRow(self.label_n, self.spin_n)
        self.check_plot = QtWidgets.QCheckBox(self.body)
        self.check_plot.setText("Plot after compute (PLTAST)")
        self.check_plot.setChecked(True)
        alf.addRow(self.check_plot)
        self.verticalLayout.addWidget(self.body)

        self.verticalLayout.addStretch()

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(AstDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.buttonBox.button(
            QtWidgets.QDialogButtonBox.StandardButton.Ok).setText("Compute")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(AstDialog)
        self.buttonBox.accepted.connect(AstDialog.accept)
        self.buttonBox.rejected.connect(AstDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(AstDialog)

    def retranslateUi(self, AstDialog):
        _translate = QtCore.QCoreApplication.translate
        AstDialog.setWindowTitle(_translate("AstDialog", "Astigmatism (AST)"))
