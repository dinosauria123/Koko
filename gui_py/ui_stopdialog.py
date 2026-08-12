# Form implementation for the aperture-stop (ASTOP) dialog.
# Mirrors KDP2 IDD_STOPSURF (GUICODE.FOR) flow.
# koko sets the stop on the currently-CHG'd surface:
#   U L -> CHG <surf> -> ASTOP        (no pupil adjustment)
#                       ASTOP EN     (entrance pupil)
#                       ASTOP EX     (exit pupil)
#                       ASTOP ENEX   (both)

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_StopDialog(object):
    def setupUi(self, StopDialog):
        StopDialog.setObjectName("StopDialog")
        StopDialog.resize(340, 220)
        StopDialog.setWindowTitle("Aperture Stop (ASTOP)")

        self.verticalLayout = QtWidgets.QVBoxLayout(StopDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(StopDialog)
        self.header.setObjectName("header")
        self.header.setText("Set aperture stop")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setObjectName("formLayout")
        self.formLayout.setContentsMargins(12, 12, 12, 12)

        # Surface #
        self.label_surf = QtWidgets.QLabel(StopDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(StopDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # Pupil adjustment
        self.label_adj = QtWidgets.QLabel(StopDialog)
        self.label_adj.setText("Pupil adjustment")
        self.combo_adj = QtWidgets.QComboBox(StopDialog)
        self.combo_adj.setObjectName("combo_adj")
        self.combo_adj.addItems(
            ["None", "Entrance pupil (EN)", "Exit pupil (EX)",
             "Both (ENEX)"])
        self.formLayout.addRow(self.label_adj, self.combo_adj)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(StopDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(StopDialog)
        self.buttonBox.accepted.connect(StopDialog.accept)
        self.buttonBox.rejected.connect(StopDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(StopDialog)

    def retranslateUi(self, StopDialog):
        pass
