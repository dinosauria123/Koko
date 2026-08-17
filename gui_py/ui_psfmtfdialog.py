# Form implementation for the PSF / MTF (DOTF/GOTF) settings dialog.
# Bundles KDP2 IDD_PSF (PSFGUI.FOR), IDD_DOTF (DOTFGUI.FOR) and
# IDD_GOTF (GOTFGUI.FOR). koko commands:
#   PSF:  NRD,<n> / TGR,<n> / PGR,<n> / GRI,<dx>  sampling
#         PSFWRITE YES/NO, PSFPLOT YES/NO, PSFROT YES/NO
#         PSFLIN / PSFLOG,<n>
#         PSF,<wav> / PSF PERFECT,<wav> / PSF PERFNOOB,<wav>
#         CAPFNOUT (write pupil file)
#   DOTF: SPACE I / SPACE O, FAR / NEAR, DOTF, DIFLEICA NO,<n>,
#         PLTDOTF[,<scale>],1 / PLTDOTF LEICA,,1
#   GOTF: SPACE I / SPACE O, FAR / NEAR, GOTF[,<scale>],
#         GEOLEICA NO,<n>, PLTGOTF,1 / PLTGOTF LEICA,1
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_PsfMtfDialog(object):
    def setupUi(self, PsfMtfDialog):
        PsfMtfDialog.setObjectName("PsfMtfDialog")
        PsfMtfDialog.resize(470, 600)
        self.verticalLayout = QtWidgets.QVBoxLayout(PsfMtfDialog)
        self.verticalLayout.setContentsMargins(12, 12, 12, 12)
        self.verticalLayout.setSpacing(8)

        # --- PSF sampling ---
        self.grp_psf = QtWidgets.QGroupBox(PsfMtfDialog)
        self.grp_psf.setTitle("Point spread function (PSF)")
        plf = QtWidgets.QFormLayout(self.grp_psf)
        plf.setContentsMargins(10, 10, 10, 10)
        self.label_nrd = QtWidgets.QLabel(self.grp_psf)
        self.label_nrd.setText("Radial samples (NRD)")
        self.spin_nrd = QtWidgets.QSpinBox(self.grp_psf)
        self.spin_nrd.setRange(2, 256)
        self.spin_nrd.setValue(32)
        plf.addRow(self.label_nrd, self.spin_nrd)
        self.label_mode = QtWidgets.QLabel(self.grp_psf)
        self.label_mode.setText("Mode")
        self.combo_mode = QtWidgets.QComboBox(self.grp_psf)
        self.combo_mode.addItems(["PSF", "PSF PERFECT", "PSF PERFNOOB"])
        plf.addRow(self.label_mode, self.combo_mode)
        self.label_wav = QtWidgets.QLabel(self.grp_psf)
        self.label_wav.setText("Wavelength #")
        self.spin_wav = QtWidgets.QSpinBox(self.grp_psf)
        self.spin_wav.setRange(1, 20)
        self.spin_wav.setValue(1)
        plf.addRow(self.label_wav, self.spin_wav)
        self.check_write = QtWidgets.QCheckBox(self.grp_psf)
        self.check_write.setText("PSFWRITE YES")
        self.check_write.setChecked(True)
        plf.addRow(self.check_write)
        self.check_plot = QtWidgets.QCheckBox(self.grp_psf)
        self.check_plot.setText("PSFPLOT YES")
        self.check_plot.setChecked(True)
        plf.addRow(self.check_plot)
        self.btn_psf = QtWidgets.QPushButton(self.grp_psf)
        self.btn_psf.setText("Compute PSF")
        plf.addRow(self.btn_psf)
        self.verticalLayout.addWidget(self.grp_psf)

        # --- DOTF (diffraction MTF) ---
        self.grp_dotf = QtWidgets.QGroupBox(PsfMtfDialog)
        self.grp_dotf.setTitle("Diffraction MTF (DOTF)")
        dlf = QtWidgets.QFormLayout(self.grp_dotf)
        dlf.setContentsMargins(10, 10, 10, 10)
        self.label_dotf_space = QtWidgets.QLabel(self.grp_dotf)
        self.label_dotf_space.setText("Space")
        self.combo_dotf_space = QtWidgets.QComboBox(self.grp_dotf)
        self.combo_dotf_space.addItems(["Image (SPACE I)", "Object (SPACE O)"])
        dlf.addRow(self.label_dotf_space, self.combo_dotf_space)
        self.label_dotf_range = QtWidgets.QLabel(self.grp_dotf)
        self.label_dotf_range.setText("Range")
        self.combo_dotf_range = QtWidgets.QComboBox(self.grp_dotf)
        self.combo_dotf_range.addItems(["Far (FAR)", "Near (NEAR)"])
        dlf.addRow(self.label_dotf_range, self.combo_dotf_range)
        self.check_dotf_leica = QtWidgets.QCheckBox(self.grp_dotf)
        self.check_dotf_leica.setText("Leica plot (PLTDOTF LEICA)")
        self.check_dotf_leica.setChecked(False)
        dlf.addRow(self.check_dotf_leica)
        self.btn_dotf = QtWidgets.QPushButton(self.grp_dotf)
        self.btn_dotf.setText("Compute & plot DOTF")
        dlf.addRow(self.btn_dotf)
        self.verticalLayout.addWidget(self.grp_dotf)

        # --- GOTF (geometric MTF) ---
        self.grp_gotf = QtWidgets.QGroupBox(PsfMtfDialog)
        self.grp_gotf.setTitle("Geometric MTF (GOTF)")
        glf = QtWidgets.QFormLayout(self.grp_gotf)
        glf.setContentsMargins(10, 10, 10, 10)
        self.label_gotf_space = QtWidgets.QLabel(self.grp_gotf)
        self.label_gotf_space.setText("Space")
        self.combo_gotf_space = QtWidgets.QComboBox(self.grp_gotf)
        self.combo_gotf_space.addItems(["Image (SPACE I)", "Object (SPACE O)"])
        glf.addRow(self.label_gotf_space, self.combo_gotf_space)
        self.label_gotf_range = QtWidgets.QLabel(self.grp_gotf)
        self.label_gotf_range.setText("Range")
        self.combo_gotf_range = QtWidgets.QComboBox(self.grp_gotf)
        self.combo_gotf_range.addItems(["Far (FAR)", "Near (NEAR)"])
        glf.addRow(self.label_gotf_range, self.combo_gotf_range)
        self.check_gotf_leica = QtWidgets.QCheckBox(self.grp_gotf)
        self.check_gotf_leica.setText("Leica plot (PLTGOTF LEICA)")
        self.check_gotf_leica.setChecked(False)
        glf.addRow(self.check_gotf_leica)
        self.btn_gotf = QtWidgets.QPushButton(self.grp_gotf)
        self.btn_gotf.setText("Compute & plot GOTF")
        glf.addRow(self.btn_gotf)
        self.verticalLayout.addWidget(self.grp_gotf)

        # --- Close ---
        self.buttonBox = QtWidgets.QDialogButtonBox(PsfMtfDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Close)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(PsfMtfDialog)
        self.buttonBox.rejected.connect(PsfMtfDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(PsfMtfDialog)

    def retranslateUi(self, PsfMtfDialog):
        _translate = QtCore.QCoreApplication.translate
        PsfMtfDialog.setWindowTitle(
            _translate("PsfMtfDialog", "PSF / MTF Settings"))
