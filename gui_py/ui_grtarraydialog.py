# Form implementation for the grating / array-lens (GRTARRAY) dialog.
# Mirrors the original IDD_GRTARRAY (KDP2 ARRAYGRT.INC) flow:
#   grating assign : U L -> CHG <surf> -> GRT -> GRO,<v> -> GRS,<v>
#                    -> GRX,<v> -> GRY,<v> -> GRZ,<v> -> EOS
#   grating delete : U L -> CHG <surf> -> GRTD -> EOS
#   array assign   : U L -> CHG <surf> -> ARRAY ODD|EVEN,<dx>,<dy> -> EOS
#   array delete   : U L -> CHG <surf> -> ARRAYD -> EOS

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_GrtArrayDialog(object):
    def setupUi(self, GrtArrayDialog):
        GrtArrayDialog.setObjectName("GrtArrayDialog")
        GrtArrayDialog.resize(380, 420)
        GrtArrayDialog.setWindowTitle("Grating / Array Lens (GRTARRAY)")

        self.verticalLayout = QtWidgets.QVBoxLayout(GrtArrayDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(GrtArrayDialog)
        self.header.setObjectName("header")
        self.header.setText("Diffraction grating and array lens")
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
        self.label_surf = QtWidgets.QLabel(GrtArrayDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(GrtArrayDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        self.verticalLayout.addLayout(self.formLayout)

        # ---- Grating group ----
        self.grp_grating = QtWidgets.QGroupBox(GrtArrayDialog)
        self.grp_grating.setObjectName("grp_grating")
        self.grp_grating.setTitle("Diffraction grating")
        gl = QtWidgets.QVBoxLayout(self.grp_grating)

        self.combo_grating = QtWidgets.QComboBox(self.grp_grating)
        self.combo_grating.setObjectName("combo_grating")
        self.combo_grating.addItems(
            ["No change", "Assign grating (GRT)", "Delete grating (GRTD)"])
        gl.addWidget(self.combo_grating)

        self.grating_widget = QtWidgets.QWidget(self.grp_grating)
        gf = QtWidgets.QFormLayout(self.grating_widget)
        gf.setContentsMargins(8, 8, 8, 8)

        self.label_gro = QtWidgets.QLabel(self.grating_widget)
        self.label_gro.setText("Groove density (GRO)")
        self.lineEdit_gro = QtWidgets.QLineEdit(self.grating_widget)
        self.lineEdit_gro.setObjectName("lineEdit_gro")
        self.lineEdit_gro.setText("1200.0")
        gf.addRow(self.label_gro, self.lineEdit_gro)

        self.label_grs = QtWidgets.QLabel(self.grating_widget)
        self.label_grs.setText("Groove spacing (GRS)")
        self.lineEdit_grs = QtWidgets.QLineEdit(self.grating_widget)
        self.lineEdit_grs.setObjectName("lineEdit_grs")
        self.lineEdit_grs.setText("0.0")
        gf.addRow(self.label_grs, self.lineEdit_grs)

        self.label_grx = QtWidgets.QLabel(self.grating_widget)
        self.label_grx.setText("Grating vector X (GRX)")
        self.lineEdit_grx = QtWidgets.QLineEdit(self.grating_widget)
        self.lineEdit_grx.setObjectName("lineEdit_grx")
        self.lineEdit_grx.setText("1.0")
        gf.addRow(self.label_grx, self.lineEdit_grx)

        self.label_gry = QtWidgets.QLabel(self.grating_widget)
        self.label_gry.setText("Grating vector Y (GRY)")
        self.lineEdit_gry = QtWidgets.QLineEdit(self.grating_widget)
        self.lineEdit_gry.setObjectName("lineEdit_gry")
        self.lineEdit_gry.setText("0.0")
        gf.addRow(self.label_gry, self.lineEdit_gry)

        self.label_grz = QtWidgets.QLabel(self.grating_widget)
        self.label_grz.setText("Grating vector Z (GRZ)")
        self.lineEdit_grz = QtWidgets.QLineEdit(self.grating_widget)
        self.lineEdit_grz.setObjectName("lineEdit_grz")
        self.lineEdit_grz.setText("0.0")
        gf.addRow(self.label_grz, self.lineEdit_grz)

        gl.addWidget(self.grating_widget)
        self.verticalLayout.addWidget(self.grp_grating)

        # ---- Array lens group ----
        self.grp_array = QtWidgets.QGroupBox(GrtArrayDialog)
        self.grp_array.setObjectName("grp_array")
        self.grp_array.setTitle("Array lens")
        al = QtWidgets.QVBoxLayout(self.grp_array)

        self.combo_array = QtWidgets.QComboBox(self.grp_array)
        self.combo_array.setObjectName("combo_array")
        self.combo_array.addItems(
            ["No change", "Assign array (ARRAY)", "Delete array (ARRAYD)"])
        al.addWidget(self.combo_array)

        self.array_widget = QtWidgets.QWidget(self.grp_array)
        af = QtWidgets.QFormLayout(self.array_widget)
        af.setContentsMargins(8, 8, 8, 8)

        self.label_arraytype = QtWidgets.QLabel(self.array_widget)
        self.label_arraytype.setText("Array type")
        self.combo_arraytype = QtWidgets.QComboBox(self.array_widget)
        self.combo_arraytype.setObjectName("combo_arraytype")
        self.combo_arraytype.addItems(["ODD", "EVEN"])
        af.addRow(self.label_arraytype, self.combo_arraytype)

        self.label_dx = QtWidgets.QLabel(self.array_widget)
        self.label_dx.setText("Element pitch X (DX)")
        self.lineEdit_dx = QtWidgets.QLineEdit(self.array_widget)
        self.lineEdit_dx.setObjectName("lineEdit_dx")
        self.lineEdit_dx.setText("1.0")
        af.addRow(self.label_dx, self.lineEdit_dx)

        self.label_dy = QtWidgets.QLabel(self.array_widget)
        self.label_dy.setText("Element pitch Y (DY)")
        self.lineEdit_dy = QtWidgets.QLineEdit(self.array_widget)
        self.lineEdit_dy.setObjectName("lineEdit_dy")
        self.lineEdit_dy.setText("1.0")
        af.addRow(self.label_dy, self.lineEdit_dy)

        al.addWidget(self.array_widget)
        self.verticalLayout.addWidget(self.grp_array)

        self.verticalLayout.addStretch()

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(GrtArrayDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(GrtArrayDialog)
        self.buttonBox.accepted.connect(GrtArrayDialog.accept)
        self.buttonBox.rejected.connect(GrtArrayDialog.reject)
        self.combo_grating.currentIndexChanged.connect(self._on_grating_changed)
        self.combo_array.currentIndexChanged.connect(self._on_array_changed)
        QtCore.QMetaObject.connectSlotsByName(GrtArrayDialog)
        self._on_grating_changed(0)
        self._on_array_changed(0)

    def _on_grating_changed(self, idx):
        self.grating_widget.setVisible(idx == 1)

    def _on_array_changed(self, idx):
        self.array_widget.setVisible(idx == 1)

    def retranslateUi(self, GrtArrayDialog):
        pass
