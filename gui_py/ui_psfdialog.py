# Form implementation for the point spread function (PSF) settings dialog
# (split out of the former combined PSF/MTF dialog, KDP2 IDD_PSF /
# PSFGUI.FOR). koko commands:
#   NRD,<n>              radial samples
#   PSFWRITE YES/NO      write PSF data file
#   PSFPLOT YES/NO       plot the PSF
#   PSF,<wav> / PSF PERFECT,<wav> / PSF PERFNOOB,<wav>   compute
#   CAPFNOUT             write pupil file
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_PsfDialog(object):
    def setupUi(self, PsfDialog):
        PsfDialog.setObjectName("PsfDialog")
        PsfDialog.resize(380, 330)
        self.verticalLayout = QtWidgets.QVBoxLayout(PsfDialog)
        self.verticalLayout.setContentsMargins(0, 0, 0, 10)
        self.verticalLayout.setSpacing(8)

        # Header band
        self.header = QtWidgets.QLabel(PsfDialog)
        self.header.setObjectName("header")
        self.header.setText("Point Spread Function (PSF)")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # Settings form
        self.body = QtWidgets.QWidget(PsfDialog)
        plf = QtWidgets.QFormLayout(self.body)
        plf.setContentsMargins(16, 12, 16, 4)
        plf.setHorizontalSpacing(16)
        plf.setVerticalSpacing(8)
        self.label_nrd = QtWidgets.QLabel(self.body)
        self.label_nrd.setText("Radial samples (NRD)")
        self.spin_nrd = QtWidgets.QSpinBox(self.body)
        self.spin_nrd.setRange(2, 256)
        self.spin_nrd.setValue(32)
        plf.addRow(self.label_nrd, self.spin_nrd)
        self.label_mode = QtWidgets.QLabel(self.body)
        self.label_mode.setText("Mode")
        self.combo_mode = QtWidgets.QComboBox(self.body)
        self.combo_mode.addItems(["PSF", "PSF PERFECT", "PSF PERFNOOB"])
        plf.addRow(self.label_mode, self.combo_mode)
        self.label_wav = QtWidgets.QLabel(self.body)
        self.label_wav.setText("Wavelength #")
        self.spin_wav = QtWidgets.QSpinBox(self.body)
        self.spin_wav.setRange(1, 20)
        self.spin_wav.setValue(1)
        plf.addRow(self.label_wav, self.spin_wav)
        self.check_write = QtWidgets.QCheckBox(self.body)
        self.check_write.setText("Write data file (PSFWRITE YES)")
        self.check_write.setChecked(True)
        plf.addRow(self.check_write)
        self.check_plot = QtWidgets.QCheckBox(self.body)
        self.check_plot.setText("Plot (PSFPLOT YES)")
        self.check_plot.setChecked(True)
        plf.addRow(self.check_plot)
        self.verticalLayout.addWidget(self.body)

        self.verticalLayout.addStretch()

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(PsfDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.buttonBox.button(
            QtWidgets.QDialogButtonBox.StandardButton.Ok).setText("Compute")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(PsfDialog)
        self.buttonBox.accepted.connect(PsfDialog.accept)
        self.buttonBox.rejected.connect(PsfDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(PsfDialog)

    def retranslateUi(self, PsfDialog):
        _translate = QtCore.QCoreApplication.translate
        PsfDialog.setWindowTitle(
            _translate("PsfDialog", "Point Spread Function (PSF)"))
