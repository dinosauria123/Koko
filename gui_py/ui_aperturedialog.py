# Form implementation for the clear-aperture (CLAP) dialog.
# Mirrors the original IDD_APECIRC / IDD_APERECT / IDD_APEELIP / IDD_APERCTK
# / IDD_APEPOLY / IDD_APECIRC3 (KDP2 GUICODE.FOR) flows:
#   circular : U L -> CHG <surf> -> CLAP <R> <XDEC> <YDEC> 0 0 -> EOS
#   rect     : U L -> CHG <surf> -> CLAP RECT <HX> <HY> <XDEC> <YDEC>
#                              -> CLAP TILT <ANG> -> EOS
#   ellipse  : U L -> CHG <surf> -> CLAP ELIP <HX> <HY> <XDEC> <YDEC>
#                              -> CLAP TILT <ANG> -> EOS
#   rect+frame: U L -> CHG <surf> -> CLAP RCTK <HX> <HY> <XDEC> <YDEC> <FR>
#                              -> CLAP TILT <ANG> -> EOS
#   polygon  : U L -> CHG <surf> -> CLAP POLY <R> <NSIDES> <XDEC> <YDEC>
#                              -> CLAP TILT <ANG> -> EOS
#   erase    : U L -> CHG <surf> -> CLAP ERASE <R> <XDEC> <YDEC> -> EOS
#   delete   : U L -> CHG <surf> -> CLAPD -> EOS

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_ApertureDialog(object):
    def setupUi(self, ApertureDialog):
        ApertureDialog.setObjectName("ApertureDialog")
        ApertureDialog.resize(360, 320)
        ApertureDialog.setWindowTitle("Clear Aperture (CLAP)")

        self.verticalLayout = QtWidgets.QVBoxLayout(ApertureDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(ApertureDialog)
        self.header.setObjectName("header")
        self.header.setText("Set clear aperture")
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
        self.label_surf = QtWidgets.QLabel(ApertureDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(ApertureDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # Shape
        self.label_shape = QtWidgets.QLabel(ApertureDialog)
        self.label_shape.setText("Shape")
        self.combo_shape = QtWidgets.QComboBox(ApertureDialog)
        self.combo_shape.setObjectName("combo_shape")
        self.combo_shape.addItems(
            ["Circular", "Rectangular", "Elliptical", "Rectangular + Frame",
             "Polygonal", "Erase region", "Delete all (CLAPD)"])
        self.formLayout.addRow(self.label_shape, self.combo_shape)

        # Radius (circular / polygonal / erase)
        self.label_rad = QtWidgets.QLabel(ApertureDialog)
        self.label_rad.setText("Radius")
        self.lineEdit_rad = QtWidgets.QLineEdit(ApertureDialog)
        self.lineEdit_rad.setObjectName("lineEdit_rad")
        self.lineEdit_rad.setText("5.0")
        self.formLayout.addRow(self.label_rad, self.lineEdit_rad)

        # Number of sides (polygonal only)
        self.label_nsides = QtWidgets.QLabel(ApertureDialog)
        self.label_nsides.setText("Number of sides")
        self.spin_nsides = QtWidgets.QSpinBox(ApertureDialog)
        self.spin_nsides.setObjectName("spin_nsides")
        self.spin_nsides.setMinimum(3)
        self.spin_nsides.setMaximum(100)
        self.spin_nsides.setValue(6)
        self.formLayout.addRow(self.label_nsides, self.spin_nsides)

        # Half-width X / Half-width Y (rect/elip/rctk)
        self.label_hx = QtWidgets.QLabel(ApertureDialog)
        self.label_hx.setText("Half-width X")
        self.lineEdit_hx = QtWidgets.QLineEdit(ApertureDialog)
        self.lineEdit_hx.setObjectName("lineEdit_hx")
        self.lineEdit_hx.setText("5.0")
        self.formLayout.addRow(self.label_hx, self.lineEdit_hx)

        self.label_hy = QtWidgets.QLabel(ApertureDialog)
        self.label_hy.setText("Half-width Y")
        self.lineEdit_hy = QtWidgets.QLineEdit(ApertureDialog)
        self.lineEdit_hy.setObjectName("lineEdit_hy")
        self.lineEdit_hy.setText("3.0")
        self.formLayout.addRow(self.label_hy, self.lineEdit_hy)

        # Frame width (rctk only)
        self.label_fr = QtWidgets.QLabel(ApertureDialog)
        self.label_fr.setText("Frame width")
        self.lineEdit_fr = QtWidgets.QLineEdit(ApertureDialog)
        self.lineEdit_fr.setObjectName("lineEdit_fr")
        self.lineEdit_fr.setText("1.0")
        self.formLayout.addRow(self.label_fr, self.lineEdit_fr)

        # X decenter / Y decenter
        self.label_xdec = QtWidgets.QLabel(ApertureDialog)
        self.label_xdec.setText("X decenter")
        self.lineEdit_xdec = QtWidgets.QLineEdit(ApertureDialog)
        self.lineEdit_xdec.setObjectName("lineEdit_xdec")
        self.lineEdit_xdec.setText("0.0")
        self.formLayout.addRow(self.label_xdec, self.lineEdit_xdec)

        self.label_ydec = QtWidgets.QLabel(ApertureDialog)
        self.label_ydec.setText("Y decenter")
        self.lineEdit_ydec = QtWidgets.QLineEdit(ApertureDialog)
        self.lineEdit_ydec.setObjectName("lineEdit_ydec")
        self.lineEdit_ydec.setText("0.0")
        self.formLayout.addRow(self.label_ydec, self.lineEdit_ydec)

        # Tilt angle (rect/elip/rctk)
        self.label_tilt = QtWidgets.QLabel(ApertureDialog)
        self.label_tilt.setText("Tilt angle")
        self.lineEdit_tilt = QtWidgets.QLineEdit(ApertureDialog)
        self.lineEdit_tilt.setObjectName("lineEdit_tilt")
        self.lineEdit_tilt.setText("0.0")
        self.formLayout.addRow(self.label_tilt, self.lineEdit_tilt)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(ApertureDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(ApertureDialog)
        self.buttonBox.accepted.connect(ApertureDialog.accept)
        self.buttonBox.rejected.connect(ApertureDialog.reject)
        self.combo_shape.currentTextChanged.connect(self._on_shape_changed)
        QtCore.QMetaObject.connectSlotsByName(ApertureDialog)
        self._on_shape_changed(self.combo_shape.currentText())

    def _on_shape_changed(self, text):
        is_circ = (text == "Circular")
        is_rctk = (text == "Rectangular + Frame")
        is_poly = (text == "Polygonal")
        is_erase = (text == "Erase region")
        is_delete = (text == "Delete all (CLAPD)")
        # radius: circular / polygonal / erase
        self.label_rad.setVisible(is_circ or is_poly or is_erase)
        self.lineEdit_rad.setVisible(is_circ or is_poly or is_erase)
        # number of sides: polygonal only
        self.label_nsides.setVisible(is_poly)
        self.spin_nsides.setVisible(is_poly)
        # half-widths: rect / elip / rctk
        show_hw = text in ("Rectangular", "Elliptical", "Rectangular + Frame")
        self.label_hx.setVisible(show_hw)
        self.lineEdit_hx.setVisible(show_hw)
        self.label_hy.setVisible(show_hw)
        self.lineEdit_hy.setVisible(show_hw)
        # frame width: rctk only
        self.label_fr.setVisible(is_rctk)
        self.lineEdit_fr.setVisible(is_rctk)
        # decenters: everything except delete-all
        self.label_xdec.setVisible(not is_delete)
        self.lineEdit_xdec.setVisible(not is_delete)
        self.label_ydec.setVisible(not is_delete)
        self.lineEdit_ydec.setVisible(not is_delete)
        # tilt: rect / elip / rctk / poly (not circular, erase, delete)
        show_tilt = text in ("Rectangular", "Elliptical",
                             "Rectangular + Frame", "Polygonal")
        self.label_tilt.setVisible(show_tilt)
        self.lineEdit_tilt.setVisible(show_tilt)

    def retranslateUi(self, ApertureDialog):
        pass
