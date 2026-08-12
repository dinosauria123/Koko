# Form implementation for the clear-obscuration (COBS) dialog.
# Mirrors the original IDD_APECIRC2 / IDD_APERECT2 / IDD_APEELIP2
# (KDP2 GUICODE.FOR) flows:
#   circular : U L -> CHG <surf> -> COBS <R> <YDEC> <XDEC> -> EOS
#   rect     : U L -> CHG <surf> -> COBS RECT <HX> <HY> <XDEC> <YDEC>
#                              -> COBS TILT <ANG> -> EOS
#   ellipse  : U L -> CHG <surf> -> COBS ELIP <HX> <HY> <XDEC> <YDEC>
#                              -> COBS TILT <ANG> -> EOS

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_ObscurationDialog(object):
    def setupUi(self, ObscurationDialog):
        ObscurationDialog.setObjectName("ObscurationDialog")
        ObscurationDialog.resize(360, 320)
        ObscurationDialog.setWindowTitle("Clear Obscuration (COBS)")

        self.verticalLayout = QtWidgets.QVBoxLayout(ObscurationDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(ObscurationDialog)
        self.header.setObjectName("header")
        self.header.setText("Set clear obscuration")
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
        self.label_surf = QtWidgets.QLabel(ObscurationDialog)
        self.label_surf.setText("Surface #")
        self.spin_surf = QtWidgets.QSpinBox(ObscurationDialog)
        self.spin_surf.setObjectName("spin_surf")
        self.spin_surf.setMinimum(1)
        self.spin_surf.setMaximum(499)
        self.spin_surf.setValue(2)
        self.formLayout.addRow(self.label_surf, self.spin_surf)

        # Shape
        self.label_shape = QtWidgets.QLabel(ObscurationDialog)
        self.label_shape.setText("Shape")
        self.combo_shape = QtWidgets.QComboBox(ObscurationDialog)
        self.combo_shape.setObjectName("combo_shape")
        self.combo_shape.addItems(
            ["Circular", "Rectangular", "Elliptical"])
        self.formLayout.addRow(self.label_shape, self.combo_shape)

        # Radius (circular only)
        self.label_rad = QtWidgets.QLabel(ObscurationDialog)
        self.label_rad.setText("Radius")
        self.lineEdit_rad = QtWidgets.QLineEdit(ObscurationDialog)
        self.lineEdit_rad.setObjectName("lineEdit_rad")
        self.lineEdit_rad.setText("3.0")
        self.formLayout.addRow(self.label_rad, self.lineEdit_rad)

        # Half-width X / Half-width Y (rect/elip)
        self.label_hx = QtWidgets.QLabel(ObscurationDialog)
        self.label_hx.setText("Half-width X")
        self.lineEdit_hx = QtWidgets.QLineEdit(ObscurationDialog)
        self.lineEdit_hx.setObjectName("lineEdit_hx")
        self.lineEdit_hx.setText("2.0")
        self.formLayout.addRow(self.label_hx, self.lineEdit_hx)

        self.label_hy = QtWidgets.QLabel(ObscurationDialog)
        self.label_hy.setText("Half-width Y")
        self.lineEdit_hy = QtWidgets.QLineEdit(ObscurationDialog)
        self.lineEdit_hy.setObjectName("lineEdit_hy")
        self.lineEdit_hy.setText("1.0")
        self.formLayout.addRow(self.label_hy, self.lineEdit_hy)

        # X decenter / Y decenter
        self.label_xdec = QtWidgets.QLabel(ObscurationDialog)
        self.label_xdec.setText("X decenter")
        self.lineEdit_xdec = QtWidgets.QLineEdit(ObscurationDialog)
        self.lineEdit_xdec.setObjectName("lineEdit_xdec")
        self.lineEdit_xdec.setText("0.0")
        self.formLayout.addRow(self.label_xdec, self.lineEdit_xdec)

        self.label_ydec = QtWidgets.QLabel(ObscurationDialog)
        self.label_ydec.setText("Y decenter")
        self.lineEdit_ydec = QtWidgets.QLineEdit(ObscurationDialog)
        self.lineEdit_ydec.setObjectName("lineEdit_ydec")
        self.lineEdit_ydec.setText("0.0")
        self.formLayout.addRow(self.label_ydec, self.lineEdit_ydec)

        # Tilt angle (rect/elip)
        self.label_tilt = QtWidgets.QLabel(ObscurationDialog)
        self.label_tilt.setText("Tilt angle")
        self.lineEdit_tilt = QtWidgets.QLineEdit(ObscurationDialog)
        self.lineEdit_tilt.setObjectName("lineEdit_tilt")
        self.lineEdit_tilt.setText("0.0")
        self.formLayout.addRow(self.label_tilt, self.lineEdit_tilt)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(ObscurationDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(ObscurationDialog)
        self.buttonBox.accepted.connect(ObscurationDialog.accept)
        self.buttonBox.rejected.connect(ObscurationDialog.reject)
        self.combo_shape.currentTextChanged.connect(self._on_shape_changed)
        QtCore.QMetaObject.connectSlotsByName(ObscurationDialog)
        self._on_shape_changed(self.combo_shape.currentText())

    def _on_shape_changed(self, text):
        is_circ = (text == "Circular")
        self.label_rad.setVisible(is_circ)
        self.lineEdit_rad.setVisible(is_circ)
        self.label_hx.setVisible(not is_circ)
        self.lineEdit_hx.setVisible(not is_circ)
        self.label_hy.setVisible(not is_circ)
        self.lineEdit_hy.setVisible(not is_circ)
        self.label_tilt.setVisible(not is_circ)
        self.lineEdit_tilt.setVisible(not is_circ)

    def retranslateUi(self, ObscurationDialog):
        pass
