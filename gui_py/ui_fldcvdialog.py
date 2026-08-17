# Form implementation for the field curvature (FLDCV) settings dialog
# (split out of the former combined IDD_DISAST dialog). koko commands:
#   FLDCV,<orient>,,<n>   compute field curvature (orient: 0 / 90 deg)
#   PLTFLDCV,,1           plot the result
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_FldcvDialog(object):
    def setupUi(self, FldcvDialog):
        FldcvDialog.setObjectName("FldcvDialog")
        FldcvDialog.resize(380, 250)
        self.verticalLayout = QtWidgets.QVBoxLayout(FldcvDialog)
        self.verticalLayout.setContentsMargins(0, 0, 0, 10)
        self.verticalLayout.setSpacing(8)

        # Header band
        self.header = QtWidgets.QLabel(FldcvDialog)
        self.header.setObjectName("header")
        self.header.setText("Field Curvature (FLDCV)")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # Settings form
        self.body = QtWidgets.QWidget(FldcvDialog)
        flf = QtWidgets.QFormLayout(self.body)
        flf.setContentsMargins(16, 12, 16, 4)
        flf.setHorizontalSpacing(16)
        flf.setVerticalSpacing(8)
        self.label_orient = QtWidgets.QLabel(self.body)
        self.label_orient.setText("Orientation")
        self.combo_orient = QtWidgets.QComboBox(self.body)
        self.combo_orient.addItems(["0 deg (tangential)", "90 deg (sagittal)"])
        flf.addRow(self.label_orient, self.combo_orient)
        self.label_n = QtWidgets.QLabel(self.body)
        self.label_n.setText("Field points")
        self.spin_n = QtWidgets.QSpinBox(self.body)
        self.spin_n.setRange(10, 50)
        self.spin_n.setValue(20)
        flf.addRow(self.label_n, self.spin_n)
        self.check_plot = QtWidgets.QCheckBox(self.body)
        self.check_plot.setText("Plot after compute (PLTFLDCV)")
        self.check_plot.setChecked(True)
        flf.addRow(self.check_plot)
        self.verticalLayout.addWidget(self.body)

        self.verticalLayout.addStretch()

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(FldcvDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.buttonBox.button(
            QtWidgets.QDialogButtonBox.StandardButton.Ok).setText("Compute")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(FldcvDialog)
        self.buttonBox.accepted.connect(FldcvDialog.accept)
        self.buttonBox.rejected.connect(FldcvDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(FldcvDialog)

    def retranslateUi(self, FldcvDialog):
        _translate = QtCore.QCoreApplication.translate
        FldcvDialog.setWindowTitle(
            _translate("FldcvDialog", "Field Curvature (FLDCV)"))
