# Form implementation for the pivot-axis (PIVAXIS) dialog.
# Mirrors KDP2 IDD_PIVAX (GUICODE.FOR) flow:
#   NORMAL mode : U L -> CHG <surf> -> PIVAXIS NORMAL -> EOS
#   VERTEX mode : U L -> CHG <surf> -> PIVAXIS VERTEX -> PIVOT,X,Y,Z -> EOS
# koko also supports "PIVAXIS ?" to display the current setting.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_PivaxisDialog(object):
    def setupUi(self, PivaxisDialog):
        PivaxisDialog.setObjectName("PivaxisDialog")
        PivaxisDialog.resize(340, 280)
        PivaxisDialog.setWindowTitle("Pivot Axis (PIVAXIS)")

        self.verticalLayout = QtWidgets.QVBoxLayout(PivaxisDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(PivaxisDialog)
        self.header.setObjectName("header")
        self.header.setText("Set pivot axis")
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
        self.label_surf = QtWidgets.QLabel(PivaxisDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(PivaxisDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # Mode
        self.label_mode = QtWidgets.QLabel(PivaxisDialog)
        self.label_mode.setText("Mode")
        self.combo_mode = QtWidgets.QComboBox(PivaxisDialog)
        self.combo_mode.setObjectName("combo_mode")
        self.combo_mode.addItems(["NORMAL (surface normal)",
                                  "VERTEX (explicit coords)"])
        self.formLayout.addRow(self.label_mode, self.combo_mode)

        # Pivot coordinates (VERTEX only)
        self.label_x = QtWidgets.QLabel(PivaxisDialog)
        self.label_x.setText("Pivot X")
        self.lineEdit_x = QtWidgets.QLineEdit(PivaxisDialog)
        self.lineEdit_x.setObjectName("lineEdit_x")
        self.lineEdit_x.setText("0.0")
        self.formLayout.addRow(self.label_x, self.lineEdit_x)

        self.label_y = QtWidgets.QLabel(PivaxisDialog)
        self.label_y.setText("Pivot Y")
        self.lineEdit_y = QtWidgets.QLineEdit(PivaxisDialog)
        self.lineEdit_y.setObjectName("lineEdit_y")
        self.lineEdit_y.setText("0.0")
        self.formLayout.addRow(self.label_y, self.lineEdit_y)

        self.label_z = QtWidgets.QLabel(PivaxisDialog)
        self.label_z.setText("Pivot Z")
        self.lineEdit_z = QtWidgets.QLineEdit(PivaxisDialog)
        self.lineEdit_z.setObjectName("lineEdit_z")
        self.lineEdit_z.setText("0.0")
        self.formLayout.addRow(self.label_z, self.lineEdit_z)

        # Show current
        self.check_show = QtWidgets.QCheckBox(PivaxisDialog)
        self.check_show.setObjectName("check_show")
        self.check_show.setText("Show current setting (PIVAXIS ?)")
        self.formLayout.addRow(self.check_show)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(PivaxisDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(PivaxisDialog)
        self.buttonBox.accepted.connect(PivaxisDialog.accept)
        self.buttonBox.rejected.connect(PivaxisDialog.reject)
        self.combo_mode.currentTextChanged.connect(self._on_mode_changed)
        self.check_show.toggled.connect(self._on_show_toggled)
        QtCore.QMetaObject.connectSlotsByName(PivaxisDialog)
        self._on_mode_changed(self.combo_mode.currentText())
        self._on_show_toggled(self.check_show.isChecked())

    def _on_mode_changed(self, text):
        is_vertex = text.startswith("VERTEX")
        for w in (self.label_x, self.lineEdit_x,
                  self.label_y, self.lineEdit_y,
                  self.label_z, self.lineEdit_z):
            w.setEnabled(is_vertex)

    def _on_show_toggled(self, checked):
        self.label_mode.setEnabled(not checked)
        self.combo_mode.setEnabled(not checked)
        self._on_mode_changed(self.combo_mode.currentText())

    def retranslateUi(self, PivaxisDialog):
        pass
