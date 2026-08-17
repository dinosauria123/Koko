# Form implementation for the blackbody radiation (IDD_BB) dialog.
# Mirrors the original KDP2 IDD_BB (GUICODE.FOR) flow. Three independent
# computations share a units radio (WATTS / PHOTONS):
#   Wien            : RADUNITS <unit> -> WIEN P,<T>
#   Stefan-Boltzmann: RADUNITS <unit> -> STEFBOLT P,<T>,<lam_upper>,<lam_lower>
#   Planck          : RADUNITS <unit> -> PLANK P,<T>,<lambda>
# All are CMD-level commands; koko prints the result as text (shown in
# the message view). Each section has its own Compute button and the
# dialog stays open so several computations can be run in a row.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_BbDialog(object):
    def setupUi(self, BbDialog):
        BbDialog.setObjectName("BbDialog")
        BbDialog.resize(400, 470)
        BbDialog.setWindowTitle("Blackbody Radiation")

        self.verticalLayout = QtWidgets.QVBoxLayout(BbDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(BbDialog)
        self.header.setObjectName("header")
        self.header.setText("Blackbody radiation calculations")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # ---- Units radio (shared) ----
        self.grp_units = QtWidgets.QGroupBox(BbDialog)
        self.grp_units.setObjectName("grp_units")
        self.grp_units.setTitle("Radiant units (RADUNITS)")
        uh = QtWidgets.QHBoxLayout(self.grp_units)
        self.radio_watts = QtWidgets.QRadioButton(self.grp_units)
        self.radio_watts.setObjectName("radio_watts")
        self.radio_watts.setText("Watts")
        self.radio_watts.setChecked(True)
        self.radio_photons = QtWidgets.QRadioButton(self.grp_units)
        self.radio_photons.setObjectName("radio_photons")
        self.radio_photons.setText("Photons")
        uh.addWidget(self.radio_watts)
        uh.addWidget(self.radio_photons)
        uh.addStretch()
        self.verticalLayout.addWidget(self.grp_units)

        # ---- Wien section ----
        self.grp_wien = QtWidgets.QGroupBox(BbDialog)
        self.grp_wien.setObjectName("grp_wien")
        self.grp_wien.setTitle("Wien — peak wavelength")
        wf = QtWidgets.QFormLayout(self.grp_wien)
        wf.setContentsMargins(10, 10, 10, 10)
        self.label_wien_t = QtWidgets.QLabel(self.grp_wien)
        self.label_wien_t.setText("Temperature T (K)")
        self.lineEdit_wien_t = QtWidgets.QLineEdit(self.grp_wien)
        self.lineEdit_wien_t.setObjectName("lineEdit_wien_t")
        self.lineEdit_wien_t.setText("5000.0")
        wf.addRow(self.label_wien_t, self.lineEdit_wien_t)
        self.btn_wien = QtWidgets.QPushButton(self.grp_wien)
        self.btn_wien.setObjectName("btn_wien")
        self.btn_wien.setText("Compute WIEN")
        wf.addRow(self.btn_wien)
        self.verticalLayout.addWidget(self.grp_wien)

        # ---- Stefan-Boltzmann section ----
        self.grp_stef = QtWidgets.QGroupBox(BbDialog)
        self.grp_stef.setObjectName("grp_stef")
        self.grp_stef.setTitle("Stefan-Boltzmann — integrated radiance")
        sf = QtWidgets.QFormLayout(self.grp_stef)
        sf.setContentsMargins(10, 10, 10, 10)
        self.label_stef_t = QtWidgets.QLabel(self.grp_stef)
        self.label_stef_t.setText("Temperature T (K)")
        self.lineEdit_stef_t = QtWidgets.QLineEdit(self.grp_stef)
        self.lineEdit_stef_t.setObjectName("lineEdit_stef_t")
        self.lineEdit_stef_t.setText("5000.0")
        sf.addRow(self.label_stef_t, self.lineEdit_stef_t)
        self.label_stef_lu = QtWidgets.QLabel(self.grp_stef)
        self.label_stef_lu.setText("Wavelength upper (\u00b5m)")
        self.lineEdit_stef_lu = QtWidgets.QLineEdit(self.grp_stef)
        self.lineEdit_stef_lu.setObjectName("lineEdit_stef_lu")
        self.lineEdit_stef_lu.setText("1.0")
        sf.addRow(self.label_stef_lu, self.lineEdit_stef_lu)
        self.label_stef_ll = QtWidgets.QLabel(self.grp_stef)
        self.label_stef_ll.setText("Wavelength lower (\u00b5m)")
        self.lineEdit_stef_ll = QtWidgets.QLineEdit(self.grp_stef)
        self.lineEdit_stef_ll.setObjectName("lineEdit_stef_ll")
        self.lineEdit_stef_ll.setText("0.0")
        sf.addRow(self.label_stef_ll, self.lineEdit_stef_ll)
        self.btn_stef = QtWidgets.QPushButton(self.grp_stef)
        self.btn_stef.setObjectName("btn_stef")
        self.btn_stef.setText("Compute STEFBOLT")
        sf.addRow(self.btn_stef)
        self.verticalLayout.addWidget(self.grp_stef)

        # ---- Planck section ----
        self.grp_plank = QtWidgets.QGroupBox(BbDialog)
        self.grp_plank.setObjectName("grp_plank")
        self.grp_plank.setTitle("Planck — spectral radiance")
        pf = QtWidgets.QFormLayout(self.grp_plank)
        pf.setContentsMargins(10, 10, 10, 10)
        self.label_plank_t = QtWidgets.QLabel(self.grp_plank)
        self.label_plank_t.setText("Temperature T (K)")
        self.lineEdit_plank_t = QtWidgets.QLineEdit(self.grp_plank)
        self.lineEdit_plank_t.setObjectName("lineEdit_plank_t")
        self.lineEdit_plank_t.setText("5000.0")
        pf.addRow(self.label_plank_t, self.lineEdit_plank_t)
        self.label_plank_l = QtWidgets.QLabel(self.grp_plank)
        self.label_plank_l.setText("Wavelength \u03bb (\u00b5m)")
        self.lineEdit_plank_l = QtWidgets.QLineEdit(self.grp_plank)
        self.lineEdit_plank_l.setObjectName("lineEdit_plank_l")
        self.lineEdit_plank_l.setText("0.55")
        pf.addRow(self.label_plank_l, self.lineEdit_plank_l)
        self.btn_plank = QtWidgets.QPushButton(self.grp_plank)
        self.btn_plank.setObjectName("btn_plank")
        self.btn_plank.setText("Compute PLANK")
        pf.addRow(self.btn_plank)
        self.verticalLayout.addWidget(self.grp_plank)

        self.verticalLayout.addStretch()

        # Note
        self.note = QtWidgets.QLabel(BbDialog)
        self.note.setObjectName("note")
        self.note.setWordWrap(True)
        self.note.setText(
            "Results are printed by koko as text in the message view. "
            "For STEFBOLT leave the upper wavelength very large (e.g. 1e20) "
            "and the lower at 0 to get the total radiance.")
        self.note.setStyleSheet("QLabel#note { color: #666; font-size: 11px; }")
        self.verticalLayout.addWidget(self.note)

        # Close button
        self.buttonBox = QtWidgets.QDialogButtonBox(BbDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Close)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(BbDialog)
        self.buttonBox.rejected.connect(BbDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(BbDialog)

    def retranslateUi(self, BbDialog):
        pass
