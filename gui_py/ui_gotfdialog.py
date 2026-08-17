# Form implementation for the geometric MTF (GOTF) settings dialog
# (split out of the former combined PSF/MTF dialog, KDP2 IDD_GOTF /
# GOTFGUI.FOR). koko commands:
#   SPACE I / SPACE O    image / object space
#   FAR / NEAR           frequency range
#   GOTF                 compute geometric MTF
#   PLTGOTF,1 / PLTGOTF LEICA,1   plot
#   DRAW                 regenerate the plot file
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_GotfDialog(object):
    def setupUi(self, GotfDialog):
        GotfDialog.setObjectName("GotfDialog")
        GotfDialog.resize(380, 280)
        self.verticalLayout = QtWidgets.QVBoxLayout(GotfDialog)
        self.verticalLayout.setContentsMargins(0, 0, 0, 10)
        self.verticalLayout.setSpacing(8)

        # Header band
        self.header = QtWidgets.QLabel(GotfDialog)
        self.header.setObjectName("header")
        self.header.setText("Geometric MTF (GOTF)")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # Settings form
        self.body = QtWidgets.QWidget(GotfDialog)
        glf = QtWidgets.QFormLayout(self.body)
        glf.setContentsMargins(16, 12, 16, 4)
        glf.setHorizontalSpacing(16)
        glf.setVerticalSpacing(8)
        self.label_space = QtWidgets.QLabel(self.body)
        self.label_space.setText("Space")
        self.combo_space = QtWidgets.QComboBox(self.body)
        self.combo_space.addItems(["Image (SPACE I)", "Object (SPACE O)"])
        glf.addRow(self.label_space, self.combo_space)
        self.label_range = QtWidgets.QLabel(self.body)
        self.label_range.setText("Range")
        self.combo_range = QtWidgets.QComboBox(self.body)
        self.combo_range.addItems(["Far (FAR)", "Near (NEAR)"])
        glf.addRow(self.label_range, self.combo_range)
        self.check_leica = QtWidgets.QCheckBox(self.body)
        self.check_leica.setText("Leica plot (PLTGOTF LEICA)")
        self.check_leica.setChecked(False)
        glf.addRow(self.check_leica)
        self.verticalLayout.addWidget(self.body)

        self.verticalLayout.addStretch()

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(GotfDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.buttonBox.button(
            QtWidgets.QDialogButtonBox.StandardButton.Ok).setText("Compute")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(GotfDialog)
        self.buttonBox.accepted.connect(GotfDialog.accept)
        self.buttonBox.rejected.connect(GotfDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(GotfDialog)

    def retranslateUi(self, GotfDialog):
        _translate = QtCore.QCoreApplication.translate
        GotfDialog.setWindowTitle(
            _translate("GotfDialog", "Geometric MTF (GOTF)"))
