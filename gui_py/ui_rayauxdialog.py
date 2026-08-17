# Form implementation for the ray-settings / analysis-aux dialog.
# Bundles four KDP2 dialogs that koko exposes as CMD-level text commands:
#   IDD_RAYSETTINGS -> SURTOL / AIMTOL / CAIMTOL / NRAITR  (PM get/set)
#   IDD_FIRD        -> FIRD,NW1,NW2                        (paraxial EFL/BFL/FFL)
#   IDD_ISTAT       -> SPD ISTAT|IPSTAT,<J>,<start>,<end>,<del>
#   IDD_FAIL        -> FAIL|FAILACC,<surf1>,<surf2>
# All results are printed by koko as text in the message view. The dialog
# stays open so several commands can be run in a row. ISTAT needs a field
# of view (FOB is sent automatically); FAIL needs spot-diagram data, so a
# grid size is provided and FOB + SPD <grid> are sent before FAIL.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_RayAuxDialog(object):
    def setupUi(self, RayAuxDialog):
        RayAuxDialog.setObjectName("RayAuxDialog")
        RayAuxDialog.resize(430, 620)
        RayAuxDialog.setWindowTitle("Ray Settings / Analysis Aux")

        self.verticalLayout = QtWidgets.QVBoxLayout(RayAuxDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(RayAuxDialog)
        self.header.setObjectName("header")
        self.header.setText("Ray tolerances, paraxial first order and spot stats")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # ---- Ray settings (SURTOL/AIMTOL/CAIMTOL/NRAITR) ----
        self.grp_rayset = QtWidgets.QGroupBox(RayAuxDialog)
        self.grp_rayset.setObjectName("grp_rayset")
        self.grp_rayset.setTitle("Ray tolerances (RAYSETTINGS)")
        rf = QtWidgets.QFormLayout(self.grp_rayset)
        rf.setContentsMargins(10, 10, 10, 10)

        self.label_surtol = QtWidgets.QLabel(self.grp_rayset)
        self.label_surtol.setText("Surface tolerance (SURTOL)")
        self.lineEdit_surtol = QtWidgets.QLineEdit(self.grp_rayset)
        self.lineEdit_surtol.setObjectName("lineEdit_surtol")
        self.lineEdit_surtol.setPlaceholderText("e.g. 1.0E-9 (blank = show current)")
        rf.addRow(self.label_surtol, self.lineEdit_surtol)

        self.label_aimtol = QtWidgets.QLabel(self.grp_rayset)
        self.label_aimtol.setText("Aim tolerance (AIMTOL)")
        self.lineEdit_aimtol = QtWidgets.QLineEdit(self.grp_rayset)
        self.lineEdit_aimtol.setObjectName("lineEdit_aimtol")
        self.lineEdit_aimtol.setPlaceholderText("e.g. 1.0E-9 (blank = show current)")
        rf.addRow(self.label_aimtol, self.lineEdit_aimtol)

        self.label_caimtol = QtWidgets.QLabel(self.grp_rayset)
        self.label_caimtol.setText("Chief-aim tolerance (CAIMTOL)")
        self.lineEdit_caimtol = QtWidgets.QLineEdit(self.grp_rayset)
        self.lineEdit_caimtol.setObjectName("lineEdit_caimtol")
        self.lineEdit_caimtol.setPlaceholderText("e.g. 1.0E-2 (blank = show current)")
        rf.addRow(self.label_caimtol, self.lineEdit_caimtol)

        self.label_nraitr = QtWidgets.QLabel(self.grp_rayset)
        self.label_nraitr.setText("Max aim iterations (NRAITR)")
        self.lineEdit_nraitr = QtWidgets.QLineEdit(self.grp_rayset)
        self.lineEdit_nraitr.setObjectName("lineEdit_nraitr")
        self.lineEdit_nraitr.setPlaceholderText("e.g. 100 (blank = show current)")
        rf.addRow(self.label_nraitr, self.lineEdit_nraitr)

        self.btn_rayset = QtWidgets.QPushButton(self.grp_rayset)
        self.btn_rayset.setObjectName("btn_rayset")
        self.btn_rayset.setText("Apply / Show tolerances")
        rf.addRow(self.btn_rayset)
        self.verticalLayout.addWidget(self.grp_rayset)

        # ---- FIRD ----
        self.grp_fird = QtWidgets.QGroupBox(RayAuxDialog)
        self.grp_fird.setObjectName("grp_fird")
        self.grp_fird.setTitle("Paraxial first order (FIRD)")
        ff = QtWidgets.QFormLayout(self.grp_fird)
        ff.setContentsMargins(10, 10, 10, 10)
        self.label_fird_s1 = QtWidgets.QLabel(self.grp_fird)
        self.label_fird_s1.setText("First surface (blank = 1)")
        self.lineEdit_fird_s1 = QtWidgets.QLineEdit(self.grp_fird)
        self.lineEdit_fird_s1.setObjectName("lineEdit_fird_s1")
        ff.addRow(self.label_fird_s1, self.lineEdit_fird_s1)
        self.label_fird_s2 = QtWidgets.QLabel(self.grp_fird)
        self.label_fird_s2.setText("Last surface (blank = image)")
        self.lineEdit_fird_s2 = QtWidgets.QLineEdit(self.grp_fird)
        self.lineEdit_fird_s2.setObjectName("lineEdit_fird_s2")
        ff.addRow(self.label_fird_s2, self.lineEdit_fird_s2)
        self.btn_fird = QtWidgets.QPushButton(self.grp_fird)
        self.btn_fird.setObjectName("btn_fird")
        self.btn_fird.setText("Compute FIRD (EFL/BFL/FFL)")
        ff.addRow(self.btn_fird)
        self.verticalLayout.addWidget(self.grp_fird)

        # ---- ISTAT ----
        self.grp_istat = QtWidgets.QGroupBox(RayAuxDialog)
        self.grp_istat.setObjectName("grp_istat")
        self.grp_istat.setTitle("Spot angle statistics (ISTAT)")
        itf = QtWidgets.QFormLayout(self.grp_istat)
        itf.setContentsMargins(10, 10, 10, 10)
        self.label_istat_type = QtWidgets.QLabel(self.grp_istat)
        self.label_istat_type.setText("Statistic")
        self.combo_istat_type = QtWidgets.QComboBox(self.grp_istat)
        self.combo_istat_type.setObjectName("combo_istat_type")
        self.combo_istat_type.addItems(
            ["Angle of incidence (ISTAT)",
             "Angle of refraction/reflection (IPSTAT)"])
        itf.addRow(self.label_istat_type, self.combo_istat_type)
        self.label_istat_start = QtWidgets.QLabel(self.grp_istat)
        self.label_istat_start.setText("Start angle (deg)")
        self.lineEdit_istat_start = QtWidgets.QLineEdit(self.grp_istat)
        self.lineEdit_istat_start.setObjectName("lineEdit_istat_start")
        self.lineEdit_istat_start.setText("0.0")
        itf.addRow(self.label_istat_start, self.lineEdit_istat_start)
        self.label_istat_end = QtWidgets.QLabel(self.grp_istat)
        self.label_istat_end.setText("End angle (deg)")
        self.lineEdit_istat_end = QtWidgets.QLineEdit(self.grp_istat)
        self.lineEdit_istat_end.setObjectName("lineEdit_istat_end")
        self.lineEdit_istat_end.setText("90.0")
        itf.addRow(self.label_istat_end, self.lineEdit_istat_end)
        self.label_istat_step = QtWidgets.QLabel(self.grp_istat)
        self.label_istat_step.setText("Step (deg)")
        self.lineEdit_istat_step = QtWidgets.QLineEdit(self.grp_istat)
        self.lineEdit_istat_step.setObjectName("lineEdit_istat_step")
        self.lineEdit_istat_step.setText("10.0")
        itf.addRow(self.label_istat_step, self.lineEdit_istat_step)
        self.btn_istat = QtWidgets.QPushButton(self.grp_istat)
        self.btn_istat.setObjectName("btn_istat")
        self.btn_istat.setText("Compute angle statistics")
        itf.addRow(self.btn_istat)
        self.verticalLayout.addWidget(self.grp_istat)

        # ---- FAIL ----
        self.grp_fail = QtWidgets.QGroupBox(RayAuxDialog)
        self.grp_fail.setObjectName("grp_fail")
        self.grp_fail.setTitle("Failed rays (FAIL)")
        flf = QtWidgets.QFormLayout(self.grp_fail)
        flf.setContentsMargins(10, 10, 10, 10)
        self.label_fail_s1 = QtWidgets.QLabel(self.grp_fail)
        self.label_fail_s1.setText("From surface (blank = object)")
        self.lineEdit_fail_s1 = QtWidgets.QLineEdit(self.grp_fail)
        self.lineEdit_fail_s1.setObjectName("lineEdit_fail_s1")
        flf.addRow(self.label_fail_s1, self.lineEdit_fail_s1)
        self.label_fail_s2 = QtWidgets.QLabel(self.grp_fail)
        self.label_fail_s2.setText("To surface (blank = image)")
        self.lineEdit_fail_s2 = QtWidgets.QLineEdit(self.grp_fail)
        self.lineEdit_fail_s2.setObjectName("lineEdit_fail_s2")
        flf.addRow(self.label_fail_s2, self.lineEdit_fail_s2)
        self.check_failacc = QtWidgets.QCheckBox(self.grp_fail)
        self.check_failacc.setObjectName("check_failacc")
        self.check_failacc.setText("Store count in GPREG (FAILACC, no print)")
        flf.addRow(self.check_failacc)
        self.btn_fail = QtWidgets.QPushButton(self.grp_fail)
        self.btn_fail.setObjectName("btn_fail")
        self.btn_fail.setText("Count failed rays")
        flf.addRow(self.btn_fail)
        self.verticalLayout.addWidget(self.grp_fail)

        self.verticalLayout.addStretch()

        # Note
        self.note = QtWidgets.QLabel(RayAuxDialog)
        self.note.setObjectName("note")
        self.note.setWordWrap(True)
        self.note.setText(
            "Results are printed by koko as text in the message view. "
            "ISTAT sends FOB first; FAIL sends FOB + SPD <grid> first so "
            "spot-diagram data exists. Leave a tolerance field blank to "
            "print its current value instead of setting it.")
        self.note.setStyleSheet("QLabel#note { color: #666; font-size: 11px; }")
        self.verticalLayout.addWidget(self.note)

        # Close button
        self.buttonBox = QtWidgets.QDialogButtonBox(RayAuxDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Close)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(RayAuxDialog)
        self.buttonBox.rejected.connect(RayAuxDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(RayAuxDialog)

    def retranslateUi(self, RayAuxDialog):
        pass
