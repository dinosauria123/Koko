# Form implementation for the geometric spot-diagram settings dialog
# (KDP2 IDD_SPOT / SPOTGUI.FOR). koko commands:
#   SPOT RING / SPOT RECT / SPOT RAND   ray distribution pattern
#   RINGS,<n>          number of rings (ring pattern)
#   RECT,<n>           number across (rect pattern)
#   RANNUM,<n>         number of random rays
#   STATS FULL / STATS MIN   statistics mode
#   SPD / SPD,<wav>    compute spot data (optionally one wavelength)
#   SPD ACC / SPD MOVE,<dx>  accumulate / decenter
#   SPDSAVE / SPDADD / SPDSTATS   spot-file operations
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_SpotDialog(object):
    def setupUi(self, SpotDialog):
        SpotDialog.setObjectName("SpotDialog")
        SpotDialog.resize(430, 470)
        self.verticalLayout = QtWidgets.QVBoxLayout(SpotDialog)
        self.verticalLayout.setContentsMargins(12, 12, 12, 12)
        self.verticalLayout.setSpacing(8)

        # --- Ray pattern ---
        self.grp_pat = QtWidgets.QGroupBox(SpotDialog)
        self.grp_pat.setTitle("Ray distribution")
        plf = QtWidgets.QFormLayout(self.grp_pat)
        plf.setContentsMargins(10, 10, 10, 10)
        self.label_pattern = QtWidgets.QLabel(self.grp_pat)
        self.label_pattern.setText("Pattern")
        self.combo_pattern = QtWidgets.QComboBox(self.grp_pat)
        self.combo_pattern.addItems(["Ring (SPOT RING)", "Rectangular (SPOT RECT)", "Random (SPOT RAND)"])
        plf.addRow(self.label_pattern, self.combo_pattern)
        self.label_count = QtWidgets.QLabel(self.grp_pat)
        self.label_count.setText("Rings / across / rays")
        self.spin_count = QtWidgets.QSpinBox(self.grp_pat)
        self.spin_count.setRange(1, 200)
        self.spin_count.setValue(6)
        plf.addRow(self.label_count, self.spin_count)
        self.label_stats = QtWidgets.QLabel(self.grp_pat)
        self.label_stats.setText("Statistics")
        self.combo_stats = QtWidgets.QComboBox(self.grp_pat)
        self.combo_stats.addItems(["Full (STATS FULL)", "Minimum (STATS MIN)"])
        plf.addRow(self.label_stats, self.combo_stats)
        self.verticalLayout.addWidget(self.grp_pat)

        # --- Compute ---
        self.grp_comp = QtWidgets.QGroupBox(SpotDialog)
        self.grp_comp.setTitle("Compute spot data")
        clf = QtWidgets.QFormLayout(self.grp_comp)
        clf.setContentsMargins(10, 10, 10, 10)
        self.label_wav = QtWidgets.QLabel(self.grp_comp)
        self.label_wav.setText("Wavelength # (0=all)")
        self.spin_wav = QtWidgets.QSpinBox(self.grp_comp)
        self.spin_wav.setRange(0, 20)
        self.spin_wav.setValue(0)
        clf.addRow(self.label_wav, self.spin_wav)
        hbtn = QtWidgets.QHBoxLayout()
        self.btn_spd = QtWidgets.QPushButton(self.grp_comp)
        self.btn_spd.setText("SPD")
        hbtn.addWidget(self.btn_spd)
        self.btn_spdacc = QtWidgets.QPushButton(self.grp_comp)
        self.btn_spdacc.setText("SPD ACC")
        hbtn.addWidget(self.btn_spdacc)
        clf.addRow(hbtn)
        self.verticalLayout.addWidget(self.grp_comp)

        # --- Plot ---
        self.grp_plot = QtWidgets.QGroupBox(SpotDialog)
        self.grp_plot.setTitle("Plot")
        glf = QtWidgets.QHBoxLayout(self.grp_plot)
        glf.setContentsMargins(10, 10, 10, 10)
        self.btn_plot = QtWidgets.QPushButton(self.grp_plot)
        self.btn_plot.setText("Plot spot diagram (PLTSPD)")
        glf.addWidget(self.btn_plot)
        self.verticalLayout.addWidget(self.grp_plot)

        # --- Spot file ops ---
        self.grp_file = QtWidgets.QGroupBox(SpotDialog)
        self.grp_file.setTitle("Spot file")
        flf = QtWidgets.QHBoxLayout(self.grp_file)
        flf.setContentsMargins(10, 10, 10, 10)
        self.btn_save = QtWidgets.QPushButton(self.grp_file)
        self.btn_save.setText("SPDSAVE")
        flf.addWidget(self.btn_save)
        self.btn_add = QtWidgets.QPushButton(self.grp_file)
        self.btn_add.setText("SPDADD")
        flf.addWidget(self.btn_add)
        self.btn_stats = QtWidgets.QPushButton(self.grp_file)
        self.btn_stats.setText("SPDSTATS")
        flf.addWidget(self.btn_stats)
        self.verticalLayout.addWidget(self.grp_file)

        # --- Close ---
        self.buttonBox = QtWidgets.QDialogButtonBox(SpotDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Close)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(SpotDialog)
        self.buttonBox.rejected.connect(SpotDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(SpotDialog)

    def retranslateUi(self, SpotDialog):
        _translate = QtCore.QCoreApplication.translate
        SpotDialog.setWindowTitle(
            _translate("SpotDialog", "Spot Diagram Settings"))
