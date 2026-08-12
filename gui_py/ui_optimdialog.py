# Form implementation generated for the Optimization dialog
# Mirrors the original Windows/Winteracter IDD_OPTIM dialog (kdpres.rc).

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_OptimizeDialog(object):
    def setupUi(self, OptimizeDialog):
        OptimizeDialog.setObjectName("OptimizeDialog")
        OptimizeDialog.resize(420, 380)
        OptimizeDialog.setWindowTitle("Optimization")

        self.verticalLayout = QtWidgets.QVBoxLayout(OptimizeDialog)
        self.verticalLayout.setContentsMargins(11, 11, 11, 11)
        self.verticalLayout.setSpacing(8)
        self.verticalLayout.setObjectName("verticalLayout")

        # ------------------------------------------------------------------
        # Group 1: Variables & Operands
        # Integrated here (mirrors the original IDD_VARED variable/operand
        # setup) so the user can define variables and the default merit
        # function in the same dialog before running ITER / PFIND / etc.
        # Placed at the top because variables/operands must be defined
        # before any optimization run.
        # ------------------------------------------------------------------
        self.groupBox_Var = QtWidgets.QGroupBox("Variables & Operands")
        self.groupBox_Var.setObjectName("groupBox_Var")
        self.gridLayout_Var = QtWidgets.QGridLayout(self.groupBox_Var)
        self.gridLayout_Var.setContentsMargins(9, 16, 9, 9)
        self.gridLayout_Var.setSpacing(6)
        self.gridLayout_Var.setObjectName("gridLayout_Var")

        self.pushButton_varEditor = QtWidgets.QPushButton(
            "OPEN VARIABLE EDITOR")
        self.pushButton_varEditor.setObjectName("pushButton_varEditor")
        self.gridLayout_Var.addWidget(self.pushButton_varEditor, 0, 0, 1, 1)

        self.label_varHint = QtWidgets.QLabel(
            "Define optimization variables and the default merit function "
            "(EFL target) before running ITER.")
        self.label_varHint.setObjectName("label_varHint")
        self.label_varHint.setWordWrap(True)
        self.gridLayout_Var.addWidget(self.label_varHint, 1, 0, 1, 1)

        self.verticalLayout.addWidget(self.groupBox_Var)

        # ------------------------------------------------------------------
        # Group 2: Damped Least Squares Controls
        # ------------------------------------------------------------------
        self.groupBox_DLS = QtWidgets.QGroupBox("Damped Least Squares Controls")
        self.groupBox_DLS.setObjectName("groupBox_DLS")
        self.gridLayout_DLS = QtWidgets.QGridLayout(self.groupBox_DLS)
        self.gridLayout_DLS.setContentsMargins(9, 16, 9, 9)
        self.gridLayout_DLS.setSpacing(6)
        self.gridLayout_DLS.setObjectName("gridLayout_DLS")

        # Meiron Damping Factor
        self.label_pfac = QtWidgets.QLabel("Meiron Damping Factor :")
        self.label_pfac.setObjectName("label_pfac")
        self.label_pfac.setAlignment(QtCore.Qt.AlignmentFlag.AlignRight
                                     | QtCore.Qt.AlignmentFlag.AlignVCenter)
        self.gridLayout_DLS.addWidget(self.label_pfac, 0, 0, 1, 1)

        self.lineEdit_pfac = QtWidgets.QLineEdit("0.0000")
        self.lineEdit_pfac.setObjectName("lineEdit_pfac")
        self.gridLayout_DLS.addWidget(self.lineEdit_pfac, 0, 1, 1, 1)

        self.pushButton_setPfac = QtWidgets.QPushButton(
            "SET DAMPING FACTOR TO ABOVE VALUE")
        self.pushButton_setPfac.setObjectName("pushButton_setPfac")
        self.gridLayout_DLS.addWidget(self.pushButton_setPfac, 1, 0, 1, 2)

        # FIND BEST DAMPING FACTOR
        self.pushButton_pfind = QtWidgets.QPushButton(
            "FIND BEST DAMPING FACTOR")
        self.pushButton_pfind.setObjectName("pushButton_pfind")
        self.gridLayout_DLS.addWidget(self.pushButton_pfind, 2, 0, 1, 2)

        # Number of Search Cycles
        self.label_cy = QtWidgets.QLabel("Number of Search Cycles :")
        self.label_cy.setObjectName("label_cy")
        self.label_cy.setAlignment(QtCore.Qt.AlignmentFlag.AlignRight
                                   | QtCore.Qt.AlignmentFlag.AlignVCenter)
        self.gridLayout_DLS.addWidget(self.label_cy, 3, 0, 1, 1)

        self.spinBox_cy = QtWidgets.QSpinBox()
        self.spinBox_cy.setObjectName("spinBox_cy")
        self.spinBox_cy.setMinimum(1)
        self.spinBox_cy.setMaximum(2147483647)
        self.spinBox_cy.setValue(25)
        self.gridLayout_DLS.addWidget(self.spinBox_cy, 3, 1, 1, 1)

        # Search Increment
        self.label_cf = QtWidgets.QLabel("Search Increment :")
        self.label_cf.setObjectName("label_cf")
        self.label_cf.setAlignment(QtCore.Qt.AlignmentFlag.AlignRight
                                   | QtCore.Qt.AlignmentFlag.AlignVCenter)
        self.gridLayout_DLS.addWidget(self.label_cf, 4, 0, 1, 1)

        self.lineEdit_cf = QtWidgets.QLineEdit("0.60000")
        self.lineEdit_cf.setObjectName("lineEdit_cf")
        self.gridLayout_DLS.addWidget(self.lineEdit_cf, 4, 1, 1, 1)

        self.verticalLayout.addWidget(self.groupBox_DLS)

        # ------------------------------------------------------------------
        # Group 3: Optimize
        # ------------------------------------------------------------------
        self.groupBox_Opt = QtWidgets.QGroupBox("Optimize")
        self.groupBox_Opt.setObjectName("groupBox_Opt")
        self.gridLayout_Opt = QtWidgets.QGridLayout(self.groupBox_Opt)
        self.gridLayout_Opt.setContentsMargins(9, 16, 9, 9)
        self.gridLayout_Opt.setSpacing(6)
        self.gridLayout_Opt.setObjectName("gridLayout_Opt")

        # ITER
        self.pushButton_iter = QtWidgets.QPushButton("ITER")
        self.pushButton_iter.setObjectName("pushButton_iter")
        self.gridLayout_Opt.addWidget(self.pushButton_iter, 0, 0, 1, 1)

        self.label_niter = QtWidgets.QLabel("Number of ITER cycles :")
        self.label_niter.setObjectName("label_niter")
        self.label_niter.setAlignment(QtCore.Qt.AlignmentFlag.AlignRight
                                      | QtCore.Qt.AlignmentFlag.AlignVCenter)
        self.gridLayout_Opt.addWidget(self.label_niter, 0, 1, 1, 1)

        self.spinBox_niter = QtWidgets.QSpinBox()
        self.spinBox_niter.setObjectName("spinBox_niter")
        self.spinBox_niter.setMinimum(1)
        self.spinBox_niter.setMaximum(2147483647)
        self.spinBox_niter.setValue(1)
        self.gridLayout_Opt.addWidget(self.spinBox_niter, 0, 2, 1, 1)

        # ITER FULL
        self.pushButton_iterfull = QtWidgets.QPushButton("ITER FULL")
        self.pushButton_iterfull.setObjectName("pushButton_iterfull")
        self.gridLayout_Opt.addWidget(self.pushButton_iterfull, 1, 0, 1, 1)

        self.label_niterfull = QtWidgets.QLabel("Number of ITER cycles :")
        self.label_niterfull.setObjectName("label_niterfull")
        self.label_niterfull.setAlignment(
            QtCore.Qt.AlignmentFlag.AlignRight
            | QtCore.Qt.AlignmentFlag.AlignVCenter)
        self.gridLayout_Opt.addWidget(self.label_niterfull, 1, 1, 1, 1)

        self.spinBox_niterfull = QtWidgets.QSpinBox()
        self.spinBox_niterfull.setObjectName("spinBox_niterfull")
        self.spinBox_niterfull.setMinimum(1)
        self.spinBox_niterfull.setMaximum(2147483647)
        self.spinBox_niterfull.setValue(1)
        self.gridLayout_Opt.addWidget(self.spinBox_niterfull, 1, 2, 1, 1)

        # ITER POWELL
        self.pushButton_iterp = QtWidgets.QPushButton("ITER POWELL")
        self.pushButton_iterp.setObjectName("pushButton_iterp")
        self.gridLayout_Opt.addWidget(self.pushButton_iterp, 2, 0, 1, 1)

        self.label_niterp = QtWidgets.QLabel(
            "Number of ITER POWELL cycles :")
        self.label_niterp.setObjectName("label_niterp")
        self.label_niterp.setAlignment(
            QtCore.Qt.AlignmentFlag.AlignRight
            | QtCore.Qt.AlignmentFlag.AlignVCenter)
        self.gridLayout_Opt.addWidget(self.label_niterp, 2, 1, 1, 1)

        self.spinBox_niterp = QtWidgets.QSpinBox()
        self.spinBox_niterp.setObjectName("spinBox_niterp")
        self.spinBox_niterp.setMinimum(1)
        self.spinBox_niterp.setMaximum(2147483647)
        self.spinBox_niterp.setValue(1)
        self.gridLayout_Opt.addWidget(self.spinBox_niterp, 2, 2, 1, 1)

        # RESTORE buttons
        self.pushButton_rest1 = QtWidgets.QPushButton("RESTORE")
        self.pushButton_rest1.setObjectName("pushButton_rest1")
        self.gridLayout_Opt.addWidget(self.pushButton_rest1, 3, 0, 1, 1)

        self.pushButton_rest2 = QtWidgets.QPushButton("RESTORE MIN")
        self.pushButton_rest2.setObjectName("pushButton_rest2")
        self.gridLayout_Opt.addWidget(self.pushButton_rest2, 3, 1, 1, 1)

        self.pushButton_rest3 = QtWidgets.QPushButton("RESTORE ORIG.")
        self.pushButton_rest3.setObjectName("pushButton_rest3")
        self.gridLayout_Opt.addWidget(self.pushButton_rest3, 3, 2, 1, 1)

        # ROBB
        self.pushButton_robb = QtWidgets.QPushButton(
            "PERFORM ROBB ACELLERATION")
        self.pushButton_robb.setObjectName("pushButton_robb")
        self.gridLayout_Opt.addWidget(self.pushButton_robb, 4, 0, 1, 3)

        self.label_beta = QtWidgets.QLabel("Beta (ROBB) :")
        self.label_beta.setObjectName("label_beta")
        self.label_beta.setAlignment(QtCore.Qt.AlignmentFlag.AlignRight
                                     | QtCore.Qt.AlignmentFlag.AlignVCenter)
        self.gridLayout_Opt.addWidget(self.label_beta, 5, 0, 1, 1)

        self.lineEdit_beta = QtWidgets.QLineEdit("1.00000000000000")
        self.lineEdit_beta.setObjectName("lineEdit_beta")
        self.gridLayout_Opt.addWidget(self.lineEdit_beta, 5, 1, 1, 2)

        self.label_delta = QtWidgets.QLabel("Delta (ROBB) :")
        self.label_delta.setObjectName("label_delta")
        self.label_delta.setAlignment(QtCore.Qt.AlignmentFlag.AlignRight
                                      | QtCore.Qt.AlignmentFlag.AlignVCenter)
        self.gridLayout_Opt.addWidget(self.label_delta, 6, 0, 1, 1)

        self.lineEdit_delta = QtWidgets.QLineEdit("1.10000000000000")
        self.lineEdit_delta.setObjectName("lineEdit_delta")
        self.gridLayout_Opt.addWidget(self.lineEdit_delta, 6, 1, 1, 2)

        self.label_nrobb = QtWidgets.QLabel("MAX Cycles (ROBB) :")
        self.label_nrobb.setObjectName("label_nrobb")
        self.label_nrobb.setAlignment(QtCore.Qt.AlignmentFlag.AlignLeft
                                      | QtCore.Qt.AlignmentFlag.AlignVCenter)
        self.gridLayout_Opt.addWidget(self.label_nrobb, 7, 0, 1, 1)

        self.spinBox_nrobb = QtWidgets.QSpinBox()
        self.spinBox_nrobb.setObjectName("spinBox_nrobb")
        self.spinBox_nrobb.setMinimum(1)
        self.spinBox_nrobb.setMaximum(2147483647)
        self.spinBox_nrobb.setValue(50)
        self.gridLayout_Opt.addWidget(self.spinBox_nrobb, 7, 1, 1, 2)

        self.verticalLayout.addWidget(self.groupBox_Opt)

        # ------------------------------------------------------------------
        # Verbose + Exit row
        # ------------------------------------------------------------------
        self.horizontalLayout_bottom = QtWidgets.QHBoxLayout()
        self.horizontalLayout_bottom.setSpacing(6)
        self.horizontalLayout_bottom.setObjectName("horizontalLayout_bottom")

        self.checkBox_verbose = QtWidgets.QCheckBox(
            "Verbose Mode Optimization")
        self.checkBox_verbose.setObjectName("checkBox_verbose")
        self.horizontalLayout_bottom.addWidget(self.checkBox_verbose)

        self.horizontalLayout_bottom.addStretch(1)

        self.pushButton_exit = QtWidgets.QPushButton("EXIT DIALOG")
        self.pushButton_exit.setObjectName("pushButton_exit")
        self.pushButton_exit.setDefault(False)
        self.pushButton_exit.setAutoDefault(False)
        self.horizontalLayout_bottom.addWidget(self.pushButton_exit)

        self.verticalLayout.addLayout(self.horizontalLayout_bottom)

        QtCore.QMetaObject.connectSlotsByName(OptimizeDialog)
