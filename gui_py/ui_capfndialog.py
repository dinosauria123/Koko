# Form implementation for the complex pupil function (CAPFN) settings
# dialog (KDP2 IDD_CAPFN, RAYS.INC). koko commands:
#   CAPFNNRD,<n>          set radial sampling (even)
#   CAPFN / CAPFN PERFECT / CAPFN SILENT   compute pupil function
#   CAPGRID,<wav>         grid display for a wavelength
#   WAMAP,<wav> / AMAP,<wav>   wave/amplitude map
#   FITZERN,<wav>         Zernike fit
#   LISTOPD / LISTZERN / LISTREPT   text listings
#   CAPFNROT YES/NO       rotation flag
#   PLOT CAPFNOPD,<wav>,1[,min,max]   OPD plot
#   PLOT CAPFNAPD,<wav>,1[,min,max]   amplitude plot
#   CAPFNOUT / CAPFNIN / CAPFNADD / CAPFNCLR   pupil file ops
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_CapfnDialog(object):
    def setupUi(self, CapfnDialog):
        CapfnDialog.setObjectName("CapfnDialog")
        CapfnDialog.resize(460, 560)
        self.verticalLayout = QtWidgets.QVBoxLayout(CapfnDialog)
        self.verticalLayout.setContentsMargins(12, 12, 12, 12)
        self.verticalLayout.setSpacing(8)

        # --- Sampling & mode ---
        self.grp_setup = QtWidgets.QGroupBox(CapfnDialog)
        self.grp_setup.setTitle("Pupil sampling & mode")
        slf = QtWidgets.QFormLayout(self.grp_setup)
        slf.setContentsMargins(10, 10, 10, 10)
        self.label_nrd = QtWidgets.QLabel(self.grp_setup)
        self.label_nrd.setText("Radial samples (NRD, even)")
        self.spin_nrd = QtWidgets.QSpinBox(self.grp_setup)
        self.spin_nrd.setRange(2, 256)
        self.spin_nrd.setValue(32)
        self.spin_nrd.setSingleStep(2)
        slf.addRow(self.label_nrd, self.spin_nrd)
        self.label_mode = QtWidgets.QLabel(self.grp_setup)
        self.label_mode.setText("Mode")
        self.combo_mode = QtWidgets.QComboBox(self.grp_setup)
        self.combo_mode.addItems(["CAPFN", "CAPFN PERFECT", "CAPFN SILENT"])
        slf.addRow(self.label_mode, self.combo_mode)
        self.btn_compute = QtWidgets.QPushButton(self.grp_setup)
        self.btn_compute.setText("Compute pupil function")
        slf.addRow(self.btn_compute)
        self.verticalLayout.addWidget(self.grp_setup)

        # --- Wavelength-dependent analyses ---
        self.grp_wav = QtWidgets.QGroupBox(CapfnDialog)
        self.grp_wav.setTitle("Wavelength analyses")
        wlf = QtWidgets.QFormLayout(self.grp_wav)
        wlf.setContentsMargins(10, 10, 10, 10)
        self.label_wav = QtWidgets.QLabel(self.grp_wav)
        self.label_wav.setText("Wavelength #")
        self.spin_wav = QtWidgets.QSpinBox(self.grp_wav)
        self.spin_wav.setRange(1, 20)
        self.spin_wav.setValue(1)
        wlf.addRow(self.label_wav, self.spin_wav)
        self.btn_capgrid = QtWidgets.QPushButton(self.grp_wav)
        self.btn_capgrid.setText("Grid (CAPGRID)")
        wlf.addRow(self.btn_capgrid)
        self.btn_wamap = QtWidgets.QPushButton(self.grp_wav)
        self.btn_wamap.setText("Wave map (WAMAP)")
        wlf.addRow(self.btn_wamap)
        self.btn_amap = QtWidgets.QPushButton(self.grp_wav)
        self.btn_amap.setText("Amplitude map (AMAP)")
        wlf.addRow(self.btn_amap)
        self.btn_fitzern = QtWidgets.QPushButton(self.grp_wav)
        self.btn_fitzern.setText("Zernike fit (FITZERN)")
        wlf.addRow(self.btn_fitzern)
        self.verticalLayout.addWidget(self.grp_wav)

        # --- Listings ---
        self.grp_list = QtWidgets.QGroupBox(CapfnDialog)
        self.grp_list.setTitle("Listings")
        llf = QtWidgets.QHBoxLayout(self.grp_list)
        llf.setContentsMargins(10, 10, 10, 10)
        self.btn_listopd = QtWidgets.QPushButton(self.grp_list)
        self.btn_listopd.setText("LISTOPD")
        llf.addWidget(self.btn_listopd)
        self.btn_listzern = QtWidgets.QPushButton(self.grp_list)
        self.btn_listzern.setText("LISTZERN")
        llf.addWidget(self.btn_listzern)
        self.btn_listrept = QtWidgets.QPushButton(self.grp_list)
        self.btn_listrept.setText("LISTREPT")
        llf.addWidget(self.btn_listrept)
        self.verticalLayout.addWidget(self.grp_list)

        # --- OPD / amplitude plots ---
        self.grp_plot = QtWidgets.QGroupBox(CapfnDialog)
        self.grp_plot.setTitle("OPD / amplitude plot")
        plf = QtWidgets.QFormLayout(self.grp_plot)
        plf.setContentsMargins(10, 10, 10, 10)
        self.label_pltwav = QtWidgets.QLabel(self.grp_plot)
        self.label_pltwav.setText("Wavelength #")
        self.spin_pltwav = QtWidgets.QSpinBox(self.grp_plot)
        self.spin_pltwav.setRange(1, 20)
        self.spin_pltwav.setValue(1)
        plf.addRow(self.label_pltwav, self.spin_pltwav)
        self.check_rot = QtWidgets.QCheckBox(self.grp_plot)
        self.check_rot.setText("Rotate (CAPFNROT YES)")
        self.check_rot.setChecked(False)
        plf.addRow(self.check_rot)
        self.label_min = QtWidgets.QLabel(self.grp_plot)
        self.label_min.setText("Min (blank=auto)")
        self.lineEdit_min = QtWidgets.QLineEdit(self.grp_plot)
        plf.addRow(self.label_min, self.lineEdit_min)
        self.label_max = QtWidgets.QLabel(self.grp_plot)
        self.label_max.setText("Max (blank=auto)")
        self.lineEdit_max = QtWidgets.QLineEdit(self.grp_plot)
        plf.addRow(self.label_max, self.lineEdit_max)
        hbtn = QtWidgets.QHBoxLayout()
        self.btn_plotopd = QtWidgets.QPushButton(self.grp_plot)
        self.btn_plotopd.setText("Plot OPD")
        hbtn.addWidget(self.btn_plotopd)
        self.btn_plotapd = QtWidgets.QPushButton(self.grp_plot)
        self.btn_plotapd.setText("Plot amplitude")
        hbtn.addWidget(self.btn_plotapd)
        plf.addRow(hbtn)
        self.verticalLayout.addWidget(self.grp_plot)

        # --- Pupil file ops ---
        self.grp_file = QtWidgets.QGroupBox(CapfnDialog)
        self.grp_file.setTitle("Pupil file")
        flf2 = QtWidgets.QHBoxLayout(self.grp_file)
        flf2.setContentsMargins(10, 10, 10, 10)
        self.btn_out = QtWidgets.QPushButton(self.grp_file)
        self.btn_out.setText("CAPFNOUT")
        flf2.addWidget(self.btn_out)
        self.btn_in = QtWidgets.QPushButton(self.grp_file)
        self.btn_in.setText("CAPFNIN")
        flf2.addWidget(self.btn_in)
        self.btn_add = QtWidgets.QPushButton(self.grp_file)
        self.btn_add.setText("CAPFNADD")
        flf2.addWidget(self.btn_add)
        self.btn_clr = QtWidgets.QPushButton(self.grp_file)
        self.btn_clr.setText("CAPFNCLR")
        flf2.addWidget(self.btn_clr)
        self.verticalLayout.addWidget(self.grp_file)

        # --- Close ---
        self.buttonBox = QtWidgets.QDialogButtonBox(CapfnDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Close)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(CapfnDialog)
        self.buttonBox.rejected.connect(CapfnDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(CapfnDialog)

    def retranslateUi(self, CapfnDialog):
        _translate = QtCore.QCoreApplication.translate
        CapfnDialog.setWindowTitle(
            _translate("CapfnDialog", "Complex Pupil Function (CAPFN)"))
