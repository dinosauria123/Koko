# Form implementation for the Variable/Operand editor dialog.
# Simplified to: EFL target (merit operand) + variable list.
# Mirrors the original IDD_VARED / FLCLTH / VARIABLES flow.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_Optimize(object):
    def setupUi(self, Optimize):
        Optimize.setObjectName("Optimize")
        Optimize.resize(360, 220)
        Optimize.setWindowTitle("Variables & Operands")

        self.verticalLayout = QtWidgets.QVBoxLayout(Optimize)
        self.verticalLayout.setContentsMargins(11, 11, 11, 11)
        self.verticalLayout.setSpacing(8)
        self.verticalLayout.setObjectName("verticalLayout")

        # EFL target (merit operand FLCLTH)
        self.groupBox_efl = QtWidgets.QGroupBox("Merit Function (EFL Target)")
        self.groupBox_efl.setObjectName("groupBox_efl")
        self.formLayout_efl = QtWidgets.QFormLayout(self.groupBox_efl)
        self.formLayout_efl.setContentsMargins(9, 16, 9, 9)
        self.formLayout_efl.setSpacing(6)

        self.label_efl = QtWidgets.QLabel("EFL Target (mm) :")
        self.label_efl.setObjectName("label_efl")
        self.formLayout_efl.setWidget(
            0, QtWidgets.QFormLayout.ItemRole.LabelRole, self.label_efl)

        self.lineEdit_efl = QtWidgets.QLineEdit("50.0")
        self.lineEdit_efl.setObjectName("lineEdit_efl")
        self.formLayout_efl.setWidget(
            0, QtWidgets.QFormLayout.ItemRole.FieldRole, self.lineEdit_efl)

        self.verticalLayout.addWidget(self.groupBox_efl)

        # Variables
        self.groupBox_var = QtWidgets.QGroupBox("Variables")
        self.groupBox_var.setObjectName("groupBox_var")
        self.vBoxLayout_var = QtWidgets.QVBoxLayout(self.groupBox_var)
        self.vBoxLayout_var.setContentsMargins(9, 16, 9, 9)
        self.vBoxLayout_var.setSpacing(6)

        self.label_var = QtWidgets.QLabel(
            "One variable per line, e.g.  CV 1   TH 3   RD 2")
        self.label_var.setObjectName("label_var")
        self.label_var.setWordWrap(True)
        self.vBoxLayout_var.addWidget(self.label_var)

        self.plainEdit_var = QtWidgets.QPlainTextEdit()
        self.plainEdit_var.setObjectName("plainEdit_var")
        self.plainEdit_var.setMinimumHeight(70)
        self.plainEdit_var.setPlainText("CV 1")
        self.vBoxLayout_var.addWidget(self.plainEdit_var)

        self.verticalLayout.addWidget(self.groupBox_var)

        self.buttonBox = QtWidgets.QDialogButtonBox(Optimize)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Cancel |
            QtWidgets.QDialogButtonBox.StandardButton.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.verticalLayout.addWidget(self.buttonBox)

        self.buttonBox.rejected.connect(Optimize.reject)  # type: ignore
        self.buttonBox.accepted.connect(Optimize.accept)  # type: ignore
        QtCore.QMetaObject.connectSlotsByName(Optimize)

    def retranslateUi(self, Optimize):
        _translate = QtCore.QCoreApplication.translate
        Optimize.setWindowTitle(_translate("Optimize", "Variables & Operands"))
        self.groupBox_efl.setTitle(
            _translate("Optimize", "Merit Function (EFL Target)"))
        self.label_efl.setText(_translate("Optimize", "EFL Target (mm) :"))
        self.groupBox_var.setTitle(_translate("Optimize", "Variables"))
        self.label_var.setText(
            _translate("Optimize",
                       "One variable per line, e.g.  CV 1   TH 3   RD 2"))
