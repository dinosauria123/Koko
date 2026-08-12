# Form implementation for the lens-library (LIB) dialog.
# Mirrors KDP2 IDD_LLIB (GUICODE.FOR) flow for the subset koko supports:
#   LIB GET <n>   -> restore library lens n
#   LIB PUT <n>   -> store current lens into library slot n
#   LIB DEL <n>   -> delete library slot n
# (koko does not support LIB REST / LIB SAVE / LIB LIST.)

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_GlassLibDialog(object):
    def setupUi(self, GlassLibDialog):
        GlassLibDialog.setObjectName("GlassLibDialog")
        GlassLibDialog.resize(320, 200)
        GlassLibDialog.setWindowTitle("Lens Library (LIB)")

        self.verticalLayout = QtWidgets.QVBoxLayout(GlassLibDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(GlassLibDialog)
        self.header.setObjectName("header")
        self.header.setText("Lens library operation")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setObjectName("formLayout")
        self.formLayout.setContentsMargins(12, 12, 12, 12)

        # Operation
        self.label_op = QtWidgets.QLabel(GlassLibDialog)
        self.label_op.setText("Operation")
        self.combo_op = QtWidgets.QComboBox(GlassLibDialog)
        self.combo_op.setObjectName("combo_op")
        self.combo_op.addItems(
            ["Get (restore lens)", "Put (store current lens)",
             "Delete (remove slot)"])
        self.formLayout.addRow(self.label_op, self.combo_op)

        # Slot #
        self.label_slot = QtWidgets.QLabel(GlassLibDialog)
        self.label_slot.setText("Library slot #")
        self.spin_slot = QtWidgets.QSpinBox(GlassLibDialog)
        self.spin_slot.setObjectName("spin_slot")
        self.spin_slot.setMinimum(1)
        self.spin_slot.setMaximum(999)
        self.spin_slot.setValue(1)
        self.formLayout.addRow(self.label_slot, self.spin_slot)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(GlassLibDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(GlassLibDialog)
        self.buttonBox.accepted.connect(GlassLibDialog.accept)
        self.buttonBox.rejected.connect(GlassLibDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(GlassLibDialog)

    def retranslateUi(self, GlassLibDialog):
        pass
