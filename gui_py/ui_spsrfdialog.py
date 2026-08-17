# Form implementation for the special-surface (SPSRF) dialog.
# Mirrors the original IDD_SPSRF (KDP2 SPSRF.INC) flow:
#   U SP -> SPECIAL,<surf>,<type> -> EOS
# koko supports special surface types 1 through 24.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_SpsrfDialog(object):
    def setupUi(self, SpsrfDialog):
        SpsrfDialog.setObjectName("SpsrfDialog")
        SpsrfDialog.resize(340, 220)
        SpsrfDialog.setWindowTitle("Special Surface (SPSRF)")

        self.verticalLayout = QtWidgets.QVBoxLayout(SpsrfDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(SpsrfDialog)
        self.header.setObjectName("header")
        self.header.setText("Assign a special surface type")
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
        self.label_surf = QtWidgets.QLabel(SpsrfDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(SpsrfDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # Special surface type
        self.label_type = QtWidgets.QLabel(SpsrfDialog)
        self.label_type.setText("Special type (1-24)")
        self.spin_type = QtWidgets.QSpinBox(SpsrfDialog)
        self.spin_type.setObjectName("spin_type")
        self.spin_type.setMinimum(1)
        self.spin_type.setMaximum(24)
        self.spin_type.setValue(1)
        self.formLayout.addRow(self.label_type, self.spin_type)

        self.verticalLayout.addLayout(self.formLayout)

        # Note
        self.note = QtWidgets.QLabel(SpsrfDialog)
        self.note.setObjectName("note")
        self.note.setWordWrap(True)
        self.note.setText(
            "Sends: U SP -> SPECIAL,<surface>,<type> -> EOS. "
            "Type 18 requires a simple spherical/conic mirror surface.")
        self.note.setStyleSheet("QLabel#note { color: #666; font-size: 11px; }")
        self.verticalLayout.addWidget(self.note)

        self.verticalLayout.addStretch()

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(SpsrfDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(SpsrfDialog)
        self.buttonBox.accepted.connect(SpsrfDialog.accept)
        self.buttonBox.rejected.connect(SpsrfDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(SpsrfDialog)

    def retranslateUi(self, SpsrfDialog):
        pass
