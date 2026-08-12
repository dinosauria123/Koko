# Form implementation for the surface-coating (COATING) dialog.
# Mirrors koko's COATING command (U L -> CHG <surf> -> COATING <n>):
#   sets the coating index for a given surface. A value of 0 means no
#   coating. The coating index maps into the active COATFILE library.
# koko also supports "COATING ?" to display the current coating number.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_CoatingDialog(object):
    def setupUi(self, CoatingDialog):
        CoatingDialog.setObjectName("CoatingDialog")
        CoatingDialog.resize(320, 200)
        CoatingDialog.setWindowTitle("Surface Coating (COATING)")

        self.verticalLayout = QtWidgets.QVBoxLayout(CoatingDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(CoatingDialog)
        self.header.setObjectName("header")
        self.header.setText("Set surface coating")
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
        self.label_surf = QtWidgets.QLabel(CoatingDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(CoatingDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # Coating index
        self.label_index = QtWidgets.QLabel(CoatingDialog)
        self.label_index.setText("Coating # (0=none)")
        self.spin_index = QtWidgets.QSpinBox(CoatingDialog)
        self.spin_index.setObjectName("spin_index")
        self.spin_index.setMinimum(0)
        self.spin_index.setMaximum(999)
        self.spin_index.setValue(0)
        self.formLayout.addRow(self.label_index, self.spin_index)

        # Show current coating
        self.check_show = QtWidgets.QCheckBox(CoatingDialog)
        self.check_show.setObjectName("check_show")
        self.check_show.setText("Show current coating number (COATING ?)")
        self.formLayout.addRow(self.check_show)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(CoatingDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(CoatingDialog)
        self.buttonBox.accepted.connect(CoatingDialog.accept)
        self.buttonBox.rejected.connect(CoatingDialog.reject)
        self.check_show.toggled.connect(self._on_show_toggled)
        QtCore.QMetaObject.connectSlotsByName(CoatingDialog)
        self._on_show_toggled(self.check_show.isChecked())

    def _on_show_toggled(self, checked):
        self.label_index.setEnabled(not checked)
        self.spin_index.setEnabled(not checked)

    def retranslateUi(self, CoatingDialog):
        pass
