# Form implementation for the macro-library (MACRO) dialog.
# koko's macro library lives in $HOME/KODS/LIBMAC/MAC.DAT and must be
# initialized once with "IMF" + "PROCEED" before any macro command works.
# After that, individual macros are run/deleted/edited with:
#   MACRO  <name>   -> run macro
#   MDEL   <name>   -> delete macro
#   MACED  <name>   -> enter mac> mode to edit (MACSAVE to store)
# Mirrors KDP2 IDD_MACRO intent (run/delete/edit) for the subset koko supports.

import os
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_MacroDialog(object):
    def setupUi(self, MacroDialog):
        MacroDialog.setObjectName("MacroDialog")
        MacroDialog.resize(360, 240)
        MacroDialog.setWindowTitle("Macro Library (MACRO)")

        self.verticalLayout = QtWidgets.QVBoxLayout(MacroDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(MacroDialog)
        self.header.setObjectName("header")
        self.header.setText("Macro library operation")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setObjectName("formLayout")
        self.formLayout.setContentsMargins(12, 12, 12, 12)

        # Macro name
        self.label_name = QtWidgets.QLabel(MacroDialog)
        self.label_name.setText("Macro name")
        self.lineEdit_name = QtWidgets.QLineEdit(MacroDialog)
        self.lineEdit_name.setObjectName("lineEdit_name")
        self.lineEdit_name.setText("")
        self.formLayout.addRow(self.label_name, self.lineEdit_name)

        # Operation
        self.label_op = QtWidgets.QLabel(MacroDialog)
        self.label_op.setText("Operation")
        self.combo_op = QtWidgets.QComboBox(MacroDialog)
        self.combo_op.setObjectName("combo_op")
        self.combo_op.addItems(
            ["Run (MACRO)", "Delete (MDEL)", "Edit (MACED)"])
        self.formLayout.addRow(self.label_op, self.combo_op)

        self.verticalLayout.addLayout(self.formLayout)

        # Initialize button (only needed once)
        self.btn_init = QtWidgets.QPushButton(MacroDialog)
        self.btn_init.setObjectName("btn_init")
        self.btn_init.setText("Initialize library (IMF + PROCEED)")
        libmac = os.path.join(os.path.expanduser("~"), "KODS", "LIBMAC")
        self.btn_init.setEnabled(not os.path.isdir(libmac) or
                                not os.path.exists(os.path.join(libmac, "MAC.DAT")))
        self.verticalLayout.addWidget(self.btn_init)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(MacroDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(MacroDialog)
        self.buttonBox.accepted.connect(MacroDialog.accept)
        self.buttonBox.rejected.connect(MacroDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(MacroDialog)

    def retranslateUi(self, MacroDialog):
        pass
