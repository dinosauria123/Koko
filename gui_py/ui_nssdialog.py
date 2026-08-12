# Form implementation for the non-sequential (NSS) database dialog.
# koko implements NSS fully: NSSNEW creates the in-memory database
# (NEXISTN=.TRUE.), after which NSSUNITS/NSSWV/UNIVERSE/OBJECT/ONAME/
# NSSSAVE/NSSREST/NSSTRACE/NSSLIST/NSSDEL all work.
# Mirrors the KDP2 NSS-menu intent for the command subset koko supports.
#
# koko command notes (verified via PTY):
#   NSSNEW                 -> create new NSS database
#   NSSUNITS IN|CM|MM|M    -> set linear units
#   NSSWV <wavelength>     -> set wavelength (micrometers)
#   UNIVERSE <radius>      -> set universe radius
#   OBJECT                 -> define object
#   ONAME <name>           -> object name
#   NSSSAVE <file>         -> save to $HOME/KODS/NSSDIR/<file>.NSS
#   NSSREST <file>         -> restore from <file>.NSS
#   NSSTRACE               -> perform NSS ray trace
#   NSSLIST                -> list current NSS database
#   NSSDEL                 -> delete current NSS database

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_NssDialog(object):
    def setupUi(self, NssDialog):
        NssDialog.setObjectName("NssDialog")
        NssDialog.resize(420, 360)
        NssDialog.setWindowTitle("Non-Sequential (NSS) Database")

        self.verticalLayout = QtWidgets.QVBoxLayout(NssDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(NssDialog)
        self.header.setObjectName("header")
        self.header.setText("Non-sequential database")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        self.formLayout = QtWidgets.QFormLayout()
        self.formLayout.setObjectName("formLayout")
        self.formLayout.setContentsMargins(12, 12, 12, 12)
        self.formLayout.setVerticalSpacing(8)

        # Units
        self.label_units = QtWidgets.QLabel(NssDialog)
        self.label_units.setText("Units")
        self.combo_units = QtWidgets.QComboBox(NssDialog)
        self.combo_units.setObjectName("combo_units")
        self.combo_units.addItems(["IN", "CM", "MM", "M"])
        self.combo_units.setCurrentText("MM")
        self.formLayout.addRow(self.label_units, self.combo_units)

        # Wavelength
        self.label_wv = QtWidgets.QLabel(NssDialog)
        self.label_wv.setText("Wavelength (µm)")
        self.lineEdit_wv = QtWidgets.QLineEdit(NssDialog)
        self.lineEdit_wv.setObjectName("lineEdit_wv")
        self.lineEdit_wv.setText("0.55")
        self.formLayout.addRow(self.label_wv, self.lineEdit_wv)

        # Universe radius
        self.label_uni = QtWidgets.QLabel(NssDialog)
        self.label_uni.setText("Universe radius")
        self.lineEdit_uni = QtWidgets.QLineEdit(NssDialog)
        self.lineEdit_uni.setObjectName("lineEdit_uni")
        self.lineEdit_uni.setText("100.0")
        self.formLayout.addRow(self.label_uni, self.lineEdit_uni)

        # Object name
        self.label_oname = QtWidgets.QLabel(NssDialog)
        self.label_oname.setText("Object name")
        self.lineEdit_oname = QtWidgets.QLineEdit(NssDialog)
        self.lineEdit_oname.setObjectName("lineEdit_oname")
        self.lineEdit_oname.setText("OBJ1")
        self.formLayout.addRow(self.label_oname, self.lineEdit_oname)

        # Save/restore file
        self.label_file = QtWidgets.QLabel(NssDialog)
        self.label_file.setText("Save/restore file")
        self.lineEdit_file = QtWidgets.QLineEdit(NssDialog)
        self.lineEdit_file.setObjectName("lineEdit_file")
        self.lineEdit_file.setText("MYNSS")
        self.formLayout.addRow(self.label_file, self.lineEdit_file)

        self.verticalLayout.addLayout(self.formLayout)

        # Buttons grid
        self.gridLayout = QtWidgets.QGridLayout()
        self.gridLayout.setObjectName("gridLayout")
        self.gridLayout.setSpacing(6)

        self.btn_new = QtWidgets.QPushButton(NssDialog)
        self.btn_new.setText("New (NSSNEW)")
        self.gridLayout.addWidget(self.btn_new, 0, 0)
        self.btn_apply = QtWidgets.QPushButton(NssDialog)
        self.btn_apply.setText("Apply Settings")
        self.gridLayout.addWidget(self.btn_apply, 0, 1)
        self.btn_object = QtWidgets.QPushButton(NssDialog)
        self.btn_object.setText("Define Object")
        self.gridLayout.addWidget(self.btn_object, 1, 0)
        self.btn_trace = QtWidgets.QPushButton(NssDialog)
        self.btn_trace.setText("Trace (NSSTRACE)")
        self.gridLayout.addWidget(self.btn_trace, 1, 1)
        self.btn_list = QtWidgets.QPushButton(NssDialog)
        self.btn_list.setText("List (NSSLIST)")
        self.gridLayout.addWidget(self.btn_list, 2, 0)
        self.btn_save = QtWidgets.QPushButton(NssDialog)
        self.btn_save.setText("Save (NSSSAVE)")
        self.gridLayout.addWidget(self.btn_save, 2, 1)
        self.btn_rest = QtWidgets.QPushButton(NssDialog)
        self.btn_rest.setText("Restore (NSSREST)")
        self.gridLayout.addWidget(self.btn_rest, 3, 0)
        self.btn_del = QtWidgets.QPushButton(NssDialog)
        self.btn_del.setText("Delete (NSSDEL)")
        self.gridLayout.addWidget(self.btn_del, 3, 1)

        self.verticalLayout.addLayout(self.gridLayout)
        self.verticalLayout.addStretch()

        self.retranslateUi(NssDialog)
        QtCore.QMetaObject.connectSlotsByName(NssDialog)

    def retranslateUi(self, NssDialog):
        pass
