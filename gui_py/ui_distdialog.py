# Form implementation for the distortion (DIST / FISHDIST) settings dialog
# (split out of the former combined IDD_DISAST dialog). koko commands:
#   DIST,<orient>,,<n>       normal distortion
#   FISHDIST,<orient>,,<n>   fisheye distortion
#   PLTDIST,,1 / PLTFDIST,,1 plot the result
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_DistDialog(object):
    def setupUi(self, DistDialog):
        DistDialog.setObjectName("DistDialog")
        DistDialog.resize(380, 290)
        self.verticalLayout = QtWidgets.QVBoxLayout(DistDialog)
        self.verticalLayout.setContentsMargins(0, 0, 0, 10)
        self.verticalLayout.setSpacing(8)

        # Header band
        self.header = QtWidgets.QLabel(DistDialog)
        self.header.setObjectName("header")
        self.header.setText("Distortion (DIST)")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # Settings form
        self.body = QtWidgets.QWidget(DistDialog)
        dlf = QtWidgets.QFormLayout(self.body)
        dlf.setContentsMargins(16, 12, 16, 4)
        dlf.setHorizontalSpacing(16)
        dlf.setVerticalSpacing(8)
        self.label_type = QtWidgets.QLabel(self.body)
        self.label_type.setText("Projection")
        self.combo_type = QtWidgets.QComboBox(self.body)
        self.combo_type.addItems(["Normal (DIST)", "Fisheye (FISHDIST)"])
        dlf.addRow(self.label_type, self.combo_type)
        self.label_orient = QtWidgets.QLabel(self.body)
        self.label_orient.setText("Orientation")
        self.combo_orient = QtWidgets.QComboBox(self.body)
        self.combo_orient.addItems(["0 deg (tangential)", "90 deg (sagittal)"])
        dlf.addRow(self.label_orient, self.combo_orient)
        self.label_n = QtWidgets.QLabel(self.body)
        self.label_n.setText("Field points")
        self.spin_n = QtWidgets.QSpinBox(self.body)
        self.spin_n.setRange(10, 50)
        self.spin_n.setValue(20)
        dlf.addRow(self.label_n, self.spin_n)
        self.check_plot = QtWidgets.QCheckBox(self.body)
        self.check_plot.setText("Plot after compute (PLTDIST / PLTFDIST)")
        self.check_plot.setChecked(True)
        dlf.addRow(self.check_plot)
        self.verticalLayout.addWidget(self.body)

        self.verticalLayout.addStretch()

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(DistDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.buttonBox.button(
            QtWidgets.QDialogButtonBox.StandardButton.Ok).setText("Compute")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(DistDialog)
        self.buttonBox.accepted.connect(DistDialog.accept)
        self.buttonBox.rejected.connect(DistDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(DistDialog)

    def retranslateUi(self, DistDialog):
        _translate = QtCore.QCoreApplication.translate
        DistDialog.setWindowTitle(_translate("DistDialog", "Distortion (DIST)"))
