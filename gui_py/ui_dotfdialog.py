# Form implementation for the diffraction MTF (DOTF) settings dialog
# (split out of the former combined PSF/MTF dialog, KDP2 IDD_DOTF /
# DOTFGUI.FOR). koko commands:
#   SPACE I / SPACE O    image / object space
#   FAR / NEAR           frequency range
#   DOTF                 compute diffraction MTF
#   PLTDOTF,,1 / PLTDOTF LEICA,,1   plot
#   DRAW                 regenerate the plot file
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_DotfDialog(object):
    def setupUi(self, DotfDialog):
        DotfDialog.setObjectName("DotfDialog")
        DotfDialog.resize(380, 280)
        self.verticalLayout = QtWidgets.QVBoxLayout(DotfDialog)
        self.verticalLayout.setContentsMargins(0, 0, 0, 10)
        self.verticalLayout.setSpacing(8)

        # Header band
        self.header = QtWidgets.QLabel(DotfDialog)
        self.header.setObjectName("header")
        self.header.setText("Diffraction MTF (DOTF)")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # Settings form
        self.body = QtWidgets.QWidget(DotfDialog)
        dlf = QtWidgets.QFormLayout(self.body)
        dlf.setContentsMargins(16, 12, 16, 4)
        dlf.setHorizontalSpacing(16)
        dlf.setVerticalSpacing(8)
        self.label_space = QtWidgets.QLabel(self.body)
        self.label_space.setText("Space")
        self.combo_space = QtWidgets.QComboBox(self.body)
        self.combo_space.addItems(["Image (SPACE I)", "Object (SPACE O)"])
        dlf.addRow(self.label_space, self.combo_space)
        self.label_range = QtWidgets.QLabel(self.body)
        self.label_range.setText("Range")
        self.combo_range = QtWidgets.QComboBox(self.body)
        self.combo_range.addItems(["Far (FAR)", "Near (NEAR)"])
        dlf.addRow(self.label_range, self.combo_range)
        self.check_leica = QtWidgets.QCheckBox(self.body)
        self.check_leica.setText("Leica plot (PLTDOTF LEICA)")
        self.check_leica.setChecked(False)
        dlf.addRow(self.check_leica)
        self.verticalLayout.addWidget(self.body)

        self.verticalLayout.addStretch()

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(DotfDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.buttonBox.button(
            QtWidgets.QDialogButtonBox.StandardButton.Ok).setText("Compute")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(DotfDialog)
        self.buttonBox.accepted.connect(DotfDialog.accept)
        self.buttonBox.rejected.connect(DotfDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(DotfDialog)

    def retranslateUi(self, DotfDialog):
        _translate = QtCore.QCoreApplication.translate
        DotfDialog.setWindowTitle(
            _translate("DotfDialog", "Diffraction MTF (DOTF)"))
