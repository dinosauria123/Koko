# Form implementation for the surface-type (SURTYPE) query dialog.
# koko's SURTYPE command is a display command (mirrors KDP2, which has no
# SURTYPE *setting* dialog either):
#   SURTYPE <surface>   -> prints REAL / PARAXIAL for that surface
#   SURTYPE ALL         -> prints surface-type table for the whole lens
# The result is shown in the message view (text output).

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_SurtypeDialog(object):
    def setupUi(self, SurtypeDialog):
        SurtypeDialog.setObjectName("SurtypeDialog")
        SurtypeDialog.resize(320, 180)
        SurtypeDialog.setWindowTitle("Surface Type (SURTYPE)")

        self.verticalLayout = QtWidgets.QVBoxLayout(SurtypeDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(SurtypeDialog)
        self.header.setObjectName("header")
        self.header.setText("Show surface type")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setObjectName("formLayout")
        self.formLayout.setContentsMargins(12, 12, 12, 12)

        # Surface # (disabled when "All" is checked)
        self.label_surf = QtWidgets.QLabel(SurtypeDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(SurtypeDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(0)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(1)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # All surfaces
        self.check_all = QtWidgets.QCheckBox(SurtypeDialog)
        self.check_all.setObjectName("check_all")
        self.check_all.setText("All surfaces (SURTYPE ALL)")
        self.formLayout.addRow(self.check_all)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(SurtypeDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(SurtypeDialog)
        self.buttonBox.accepted.connect(SurtypeDialog.accept)
        self.buttonBox.rejected.connect(SurtypeDialog.reject)
        self.check_all.toggled.connect(self._on_all_toggled)
        QtCore.QMetaObject.connectSlotsByName(SurtypeDialog)

    def _on_all_toggled(self, checked):
        self.label_surf.setEnabled(not checked)
        self.spin_surf.setEnabled(not checked)

    def retranslateUi(self, SurtypeDialog):
        pass
