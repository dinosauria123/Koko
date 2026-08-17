# Form implementation for the geometric spot-diagram (SPD) settings
# dialog (KDP2 IDD_SPOT / SPOTGUI.FOR). koko commands:
#   SPOT RING / SPOT RECT / SPOT RAND   ray distribution pattern
#   RINGS,<n>          number of rings (ring pattern)
#   RECT,<n>           number across (rect pattern)
#   RANNUM,<n>         number of random rays
#   STATS FULL / STATS MIN   statistics mode
#   SPD / SPD,<wav>    compute spot data (optionally one wavelength)
#   SPD ACC / SPD ACC,<wav>   accumulate spot data
#   PLTSPD             plot the spot diagram
#   SPDSAVE / SPDADD / SPDSTATS   spot-file operations
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_SpotDialog(object):
    def setupUi(self, SpotDialog):
        SpotDialog.setObjectName("SpotDialog")
        SpotDialog.resize(400, 420)
        self.verticalLayout = QtWidgets.QVBoxLayout(SpotDialog)
        self.verticalLayout.setContentsMargins(0, 0, 0, 10)
        self.verticalLayout.setSpacing(8)

        # Header band
        self.header = QtWidgets.QLabel(SpotDialog)
        self.header.setObjectName("header")
        self.header.setText("Spot Diagram (SPD)")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # Settings form
        self.body = QtWidgets.QWidget(SpotDialog)
        plf = QtWidgets.QFormLayout(self.body)
        plf.setContentsMargins(16, 12, 16, 4)
        plf.setHorizontalSpacing(16)
        plf.setVerticalSpacing(8)
        self.label_pattern = QtWidgets.QLabel(self.body)
        self.label_pattern.setText("Ray pattern")
        self.combo_pattern = QtWidgets.QComboBox(self.body)
        self.combo_pattern.addItems(
            ["Ring (SPOT RING)", "Rectangular (SPOT RECT)",
             "Random (SPOT RAND)"])
        plf.addRow(self.label_pattern, self.combo_pattern)
        self.label_count = QtWidgets.QLabel(self.body)
        self.label_count.setText("Rings / across / rays")
        self.spin_count = QtWidgets.QSpinBox(self.body)
        self.spin_count.setRange(1, 200)
        self.spin_count.setValue(6)
        plf.addRow(self.label_count, self.spin_count)
        self.label_stats = QtWidgets.QLabel(self.body)
        self.label_stats.setText("Statistics")
        self.combo_stats = QtWidgets.QComboBox(self.body)
        self.combo_stats.addItems(["Full (STATS FULL)", "Minimum (STATS MIN)"])
        plf.addRow(self.label_stats, self.combo_stats)
        self.label_wav = QtWidgets.QLabel(self.body)
        self.label_wav.setText("Wavelength # (0=all)")
        self.spin_wav = QtWidgets.QSpinBox(self.body)
        self.spin_wav.setRange(0, 20)
        self.spin_wav.setValue(0)
        plf.addRow(self.label_wav, self.spin_wav)
        self.check_acc = QtWidgets.QCheckBox(self.body)
        self.check_acc.setText("Accumulate (SPD ACC)")
        self.check_acc.setChecked(False)
        plf.addRow(self.check_acc)
        self.check_plot = QtWidgets.QCheckBox(self.body)
        self.check_plot.setText("Plot (PLTSPD)")
        self.check_plot.setChecked(True)
        plf.addRow(self.check_plot)
        self.verticalLayout.addWidget(self.body)

        # Spot file operations (act immediately, dialog stays open)
        self.grp_file = QtWidgets.QGroupBox(SpotDialog)
        self.grp_file.setTitle("Spot file")
        flf = QtWidgets.QHBoxLayout(self.grp_file)
        flf.setContentsMargins(16, 10, 16, 10)
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

        self.verticalLayout.addStretch()

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(SpotDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.buttonBox.button(
            QtWidgets.QDialogButtonBox.StandardButton.Ok).setText("Compute")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(SpotDialog)
        self.buttonBox.accepted.connect(SpotDialog.accept)
        self.buttonBox.rejected.connect(SpotDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(SpotDialog)

    def retranslateUi(self, SpotDialog):
        _translate = QtCore.QCoreApplication.translate
        SpotDialog.setWindowTitle(
            _translate("SpotDialog", "Spot Diagram (SPD)"))
