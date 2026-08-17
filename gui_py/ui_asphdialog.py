# Form implementation for the aspheric / toric (ASPH) dialog.
# Mirrors the original IDD_ASPH (KDP2 ASPH.INC) flow. Non-toric branch:
#   U L -> CHG <surf> -> ASPH -> CHG <surf>
#       -> CC <conic> -> AC <2nd> -> AD <4th> -> AE <6th> -> AF <8th>
#       -> AG <10th> -> AH <12th> -> AI <14th> -> AJ <16th>
#       -> AK <18th> -> AL <20th> -> EOS
# Toric branch:
#   U L -> CHG <surf> -> YTORIC|XTORIC -> RDTOR|CVTOR <val> -> EOS
#   then CCTOR / TASPH + ADTOR/AETOR/AFTOR/AGTOR as needed.

from PyQt6 import QtCore, QtGui, QtWidgets

# (label, command) for the even-order aspheric coefficients
ASPH_COEFFS = [
    ("2nd (AC)", "AC"),
    ("4th (AD)", "AD"),
    ("6th (AE)", "AE"),
    ("8th (AF)", "AF"),
    ("10th (AG)", "AG"),
    ("12th (AH)", "AH"),
    ("14th (AI)", "AI"),
    ("16th (AJ)", "AJ"),
    ("18th (AK)", "AK"),
    ("20th (AL)", "AL"),
]


class Ui_AsphDialog(object):
    def setupUi(self, AsphDialog):
        AsphDialog.setObjectName("AsphDialog")
        AsphDialog.resize(380, 460)
        AsphDialog.setWindowTitle("Aspheric / Toric (ASPH)")

        self.verticalLayout = QtWidgets.QVBoxLayout(AsphDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(AsphDialog)
        self.header.setObjectName("header")
        self.header.setText("Aspheric and toric surface coefficients")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # Scrollable body (many coefficient fields)
        self.scroll = QtWidgets.QScrollArea(AsphDialog)
        self.scroll.setObjectName("scroll")
        self.scroll.setWidgetResizable(True)
        self.scroll.setFrameShape(QtWidgets.QFrame.Shape.NoFrame)
        self.body = QtWidgets.QWidget()
        self.body.setObjectName("body")
        self.formLayout = QtWidgets.QFormLayout(self.body)
        self.formLayout.setObjectName("formLayout")
        self.formLayout.setContentsMargins(12, 12, 12, 12)

        # Surface #
        self.label_surf = QtWidgets.QLabel(self.body)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(self.body)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # Mode: aspheric vs toric
        self.label_mode = QtWidgets.QLabel(self.body)
        self.label_mode.setText("Surface mode")
        self.combo_mode = QtWidgets.QComboBox(self.body)
        self.combo_mode.setObjectName("combo_mode")
        self.combo_mode.addItems(["Aspheric (conic + even terms)",
                                  "Toric (Y-toric)", "Toric (X-toric)"])
        self.formLayout.addRow(self.label_mode, self.combo_mode)

        # Conic constant (aspheric mode)
        self.label_cc = QtWidgets.QLabel(self.body)
        self.label_cc.setText("Conic constant (CC)")
        self.lineEdit_cc = QtWidgets.QLineEdit(self.body)
        self.lineEdit_cc.setObjectName("lineEdit_cc")
        self.lineEdit_cc.setText("0.0")
        self.formLayout.addRow(self.label_cc, self.lineEdit_cc)

        # Even-order aspheric coefficients
        self.coeff_edits = {}
        for label, cmd in ASPH_COEFFS:
            lab = QtWidgets.QLabel(self.body)
            lab.setText(label)
            edit = QtWidgets.QLineEdit(self.body)
            edit.setObjectName("lineEdit_" + cmd)
            edit.setText("0.0")
            self.formLayout.addRow(lab, edit)
            self.coeff_edits[cmd] = (lab, edit)

        # Toric value (toric mode)
        self.label_torval = QtWidgets.QLabel(self.body)
        self.label_torval.setText("Toric value")
        self.lineEdit_torval = QtWidgets.QLineEdit(self.body)
        self.lineEdit_torval.setObjectName("lineEdit_torval")
        self.lineEdit_torval.setText("0.0")
        self.formLayout.addRow(self.label_torval, self.lineEdit_torval)

        # Toric value mode (radius vs curvature)
        self.label_tormode = QtWidgets.QLabel(self.body)
        self.label_tormode.setText("Toric value is")
        self.combo_tormode = QtWidgets.QComboBox(self.body)
        self.combo_tormode.setObjectName("combo_tormode")
        self.combo_tormode.addItems(["Radius (RDTOR)", "Curvature (CVTOR)"])
        self.formLayout.addRow(self.label_tormode, self.combo_tormode)

        # Toric conic (CCTOR)
        self.label_cctor = QtWidgets.QLabel(self.body)
        self.label_cctor.setText("Toric conic (CCTOR)")
        self.lineEdit_cctor = QtWidgets.QLineEdit(self.body)
        self.lineEdit_cctor.setObjectName("lineEdit_cctor")
        self.lineEdit_cctor.setText("0.0")
        self.formLayout.addRow(self.label_cctor, self.lineEdit_cctor)

        self.scroll.setWidget(self.body)
        self.verticalLayout.addWidget(self.scroll)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(AsphDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(AsphDialog)
        self.buttonBox.accepted.connect(AsphDialog.accept)
        self.buttonBox.rejected.connect(AsphDialog.reject)
        self.combo_mode.currentIndexChanged.connect(self._on_mode_changed)
        QtCore.QMetaObject.connectSlotsByName(AsphDialog)
        self._on_mode_changed(0)

    def _on_mode_changed(self, idx):
        is_asph = (idx == 0)
        self.label_cc.setVisible(is_asph)
        self.lineEdit_cc.setVisible(is_asph)
        for cmd, (lab, edit) in self.coeff_edits.items():
            lab.setVisible(is_asph)
            edit.setVisible(is_asph)
        self.label_torval.setVisible(not is_asph)
        self.lineEdit_torval.setVisible(not is_asph)
        self.label_tormode.setVisible(not is_asph)
        self.combo_tormode.setVisible(not is_asph)
        self.label_cctor.setVisible(not is_asph)
        self.lineEdit_cctor.setVisible(not is_asph)

    def retranslateUi(self, AsphDialog):
        pass
