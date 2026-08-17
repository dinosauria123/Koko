# Form implementation for the multiple aperture/obscuration dialog
# (KDP2 IDD_CLAPS / IDD_MCLAP / IDD_MCOBS). koko commands:
#   MULTCLAP,<n>,<x>,<y>[,<gam>]   add aperture instance n at (x,y) rot gam
#   MULTCLAP DELETE                remove all multiple apertures on surface
#   MULTCOBS,<n>,<x>,<y>[,<gam>]   add obscuration instance n at (x,y) rot gam
#   MULTCOBS DELETE                remove all multiple obscurations on surface
# Requires a pre-existing CLAP (for MULTCLAP) or COBS (for MULTCOBS).
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_MultiApertureDialog(object):
    def setupUi(self, MultiApertureDialog):
        MultiApertureDialog.setObjectName("MultiApertureDialog")
        MultiApertureDialog.resize(420, 480)
        self.verticalLayout = QtWidgets.QVBoxLayout(MultiApertureDialog)
        self.verticalLayout.setContentsMargins(12, 12, 12, 12)
        self.verticalLayout.setSpacing(8)

        # --- Surface selection ---
        self.grp_surf = QtWidgets.QGroupBox(MultiApertureDialog)
        self.grp_surf.setTitle("Surface")
        slf = QtWidgets.QFormLayout(self.grp_surf)
        slf.setContentsMargins(10, 10, 10, 10)
        self.label_surf = QtWidgets.QLabel(self.grp_surf)
        self.label_surf.setText("Surface number")
        self.spin_surf = QtWidgets.QSpinBox(self.grp_surf)
        self.spin_surf.setRange(1, 200)
        self.spin_surf.setValue(2)
        slf.addRow(self.label_surf, self.spin_surf)
        self.verticalLayout.addWidget(self.grp_surf)

        # --- MULTCLAP ---
        self.grp_clap = QtWidgets.QGroupBox(MultiApertureDialog)
        self.grp_clap.setTitle("Multiple apertures (MULTCLAP)")
        clf = QtWidgets.QFormLayout(self.grp_clap)
        clf.setContentsMargins(10, 10, 10, 10)
        self.label_clap_n = QtWidgets.QLabel(self.grp_clap)
        self.label_clap_n.setText("Instance # (1-1000)")
        self.spin_clap_n = QtWidgets.QSpinBox(self.grp_clap)
        self.spin_clap_n.setRange(1, 1000)
        self.spin_clap_n.setValue(1)
        clf.addRow(self.label_clap_n, self.spin_clap_n)
        self.label_clap_x = QtWidgets.QLabel(self.grp_clap)
        self.label_clap_x.setText("X offset")
        self.lineEdit_clap_x = QtWidgets.QLineEdit(self.grp_clap)
        self.lineEdit_clap_x.setText("0.0")
        clf.addRow(self.label_clap_x, self.lineEdit_clap_x)
        self.label_clap_y = QtWidgets.QLabel(self.grp_clap)
        self.label_clap_y.setText("Y offset")
        self.lineEdit_clap_y = QtWidgets.QLineEdit(self.grp_clap)
        self.lineEdit_clap_y.setText("0.0")
        clf.addRow(self.label_clap_y, self.lineEdit_clap_y)
        self.label_clap_gam = QtWidgets.QLabel(self.grp_clap)
        self.label_clap_gam.setText("Rotation (deg, optional)")
        self.lineEdit_clap_gam = QtWidgets.QLineEdit(self.grp_clap)
        clf.addRow(self.label_clap_gam, self.lineEdit_clap_gam)
        self.btn_clap_add = QtWidgets.QPushButton(self.grp_clap)
        self.btn_clap_add.setText("Add aperture instance")
        clf.addRow(self.btn_clap_add)
        self.btn_clap_del = QtWidgets.QPushButton(self.grp_clap)
        self.btn_clap_del.setText("Delete all (MULTCLAP DELETE)")
        clf.addRow(self.btn_clap_del)
        self.verticalLayout.addWidget(self.grp_clap)

        # --- MULTCOBS ---
        self.grp_cobs = QtWidgets.QGroupBox(MultiApertureDialog)
        self.grp_cobs.setTitle("Multiple obscurations (MULTCOBS)")
        obf = QtWidgets.QFormLayout(self.grp_cobs)
        obf.setContentsMargins(10, 10, 10, 10)
        self.label_cobs_n = QtWidgets.QLabel(self.grp_cobs)
        self.label_cobs_n.setText("Instance # (1-1000)")
        self.spin_cobs_n = QtWidgets.QSpinBox(self.grp_cobs)
        self.spin_cobs_n.setRange(1, 1000)
        self.spin_cobs_n.setValue(1)
        obf.addRow(self.label_cobs_n, self.spin_cobs_n)
        self.label_cobs_x = QtWidgets.QLabel(self.grp_cobs)
        self.label_cobs_x.setText("X offset")
        self.lineEdit_cobs_x = QtWidgets.QLineEdit(self.grp_cobs)
        self.lineEdit_cobs_x.setText("0.0")
        obf.addRow(self.label_cobs_x, self.lineEdit_cobs_x)
        self.label_cobs_y = QtWidgets.QLabel(self.grp_cobs)
        self.label_cobs_y.setText("Y offset")
        self.lineEdit_cobs_y = QtWidgets.QLineEdit(self.grp_cobs)
        self.lineEdit_cobs_y.setText("0.0")
        obf.addRow(self.label_cobs_y, self.lineEdit_cobs_y)
        self.label_cobs_gam = QtWidgets.QLabel(self.grp_cobs)
        self.label_cobs_gam.setText("Rotation (deg, optional)")
        self.lineEdit_cobs_gam = QtWidgets.QLineEdit(self.grp_cobs)
        obf.addRow(self.label_cobs_gam, self.lineEdit_cobs_gam)
        self.btn_cobs_add = QtWidgets.QPushButton(self.grp_cobs)
        self.btn_cobs_add.setText("Add obscuration instance")
        obf.addRow(self.btn_cobs_add)
        self.btn_cobs_del = QtWidgets.QPushButton(self.grp_cobs)
        self.btn_cobs_del.setText("Delete all (MULTCOBS DELETE)")
        obf.addRow(self.btn_cobs_del)
        self.verticalLayout.addWidget(self.grp_cobs)

        # --- Close ---
        self.buttonBox = QtWidgets.QDialogButtonBox(MultiApertureDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Close)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(MultiApertureDialog)
        self.buttonBox.rejected.connect(MultiApertureDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(MultiApertureDialog)

    def retranslateUi(self, MultiApertureDialog):
        _translate = QtCore.QCoreApplication.translate
        MultiApertureDialog.setWindowTitle(
            _translate("MultiApertureDialog",
                       "Multiple Apertures / Obscurations (MULTCLAP / MULTCOBS)"))
