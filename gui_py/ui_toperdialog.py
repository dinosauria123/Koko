# Form implementation for the tolerancing (TOLERANCING) dialog.
# koko implements tolerancing fully with a multi-mode flow:
#   1) TVAR  -> tvb> mode -> define tolerance VARIABLES (TH/RD_FR/CV_FR/CC/...)
#   2) TOPER -> top> mode -> define tolerance OPERANDS (FUNCxx or built-ins)
#   3) SENSI / MONTE  -> run sensitivity / Monte-Carlo analysis
# Verified via PTY: TVAR+TOPER+SENSI produces a full sensitivity report.
#
# koko command notes:
#   TVAR                 -> enter tvb> tolerance-variable mode
#   TH  <s> <delta>      -> thickness tolerance on surface s
#   RD_FR <s> <delta>    -> radius (fringe) tolerance on surface s
#   CV_FR <s> <delta>    -> curvature (fringe) tolerance on surface s
#   CC <s> <delta>       -> conic tolerance
#   AD <s> <delta>       -> aspheric ADEG tolerance
#   AE <s> <delta> ...   -> higher aspheric terms
#   EOS                  -> exit tvb>/top> mode back to cmd>
#   TOPER                -> enter top> tolerance-operand mode
#   FUNC01 <n> <n> ...   -> user-defined operand (requires macro FUNxx)
#   SENSI                -> sensitivity analysis
#   MONTE                -> Monte-Carlo analysis
#   TOLNRD <n>           -> set tolerance grid (even int 16..512)

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_ToperDialog(object):
    def setupUi(self, ToperDialog):
        ToperDialog.setObjectName("ToperDialog")
        ToperDialog.resize(460, 420)
        ToperDialog.setWindowTitle("Tolerancing (TOPER/TVAR)")

        self.verticalLayout = QtWidgets.QVBoxLayout(ToperDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(ToperDialog)
        self.header.setObjectName("header")
        self.header.setText("Tolerancing setup & analysis")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # Tolerance grid
        self.hlayout_grid = QtWidgets.QHBoxLayout()
        self.label_grid = QtWidgets.QLabel("Tolerance grid (TOLNRD)")
        self.spin_grid = QtWidgets.QSpinBox()
        self.spin_grid.setRange(16, 512)
        self.spin_grid.setSingleStep(2)
        self.spin_grid.setValue(16)
        self.hlayout_grid.addWidget(self.label_grid)
        self.hlayout_grid.addWidget(self.spin_grid)
        self.hlayout_grid.addStretch()
        self.verticalLayout.addLayout(self.hlayout_grid)

        # Variables table
        self.label_vars = QtWidgets.QLabel("Tolerance variables (TVAR)")
        self.label_vars.setStyleSheet("font-weight: bold; padding-top: 4px;")
        self.verticalLayout.addWidget(self.label_vars)
        self.table_vars = QtWidgets.QTableWidget()
        self.table_vars.setColumnCount(3)
        self.table_vars.setHorizontalHeaderLabels(
            ["Type", "Surface #", "Delta"])
        self.table_vars.horizontalHeader().setStretchLastSection(True)
        self.verticalLayout.addWidget(self.table_vars)

        self.hlayout_vbtns = QtWidgets.QHBoxLayout()
        self.combo_vtype = QtWidgets.QComboBox()
        self.combo_vtype.addItems(
            ["TH", "RD_FR", "CV_FR", "CC", "AD", "AE", "AF", "AG",
             "XD", "YD", "PIVX", "PIVY", "PIVZ"])
        self.spin_vsurf = QtWidgets.QSpinBox()
        self.spin_vsurf.setRange(1, 499)
        self.spin_vsurf.setValue(1)
        self.line_vdelta = QtWidgets.QLineEdit("0.01")
        self.btn_addvar = QtWidgets.QPushButton("Add Variable")
        self.btn_delvar = QtWidgets.QPushButton("Delete Selected")
        self.hlayout_vbtns.addWidget(self.combo_vtype)
        self.hlayout_vbtns.addWidget(self.spin_vsurf)
        self.hlayout_vbtns.addWidget(self.line_vdelta)
        self.hlayout_vbtns.addWidget(self.btn_addvar)
        self.hlayout_vbtns.addWidget(self.btn_delvar)
        self.verticalLayout.addLayout(self.hlayout_vbtns)

        # Operands
        self.label_ops = QtWidgets.QLabel("Tolerance operands (TOPER)")
        self.label_ops.setStyleSheet("font-weight: bold; padding-top: 4px;")
        self.verticalLayout.addWidget(self.label_ops)
        self.hlayout_ops = QtWidgets.QHBoxLayout()
        self.combo_op = QtWidgets.QComboBox()
        self.combo_op.addItems(
            ["SPOT", "OPD", "ELEV", "DIST", "GBR", "POWR", "FOV", "WAV",
             "REA", "IMA", "LUM", "USR1", "USR2", "USR3"])
        self.line_opargs = QtWidgets.QLineEdit("1 1")
        self.btn_addop = QtWidgets.QPushButton("Add Operand")
        self.btn_delop = QtWidgets.QPushButton("Delete Selected")
        self.hlayout_ops.addWidget(self.combo_op)
        self.hlayout_ops.addWidget(self.line_opargs)
        self.hlayout_ops.addWidget(self.btn_addop)
        self.hlayout_ops.addWidget(self.btn_delop)
        self.verticalLayout.addLayout(self.hlayout_ops)
        self.table_ops = QtWidgets.QTableWidget()
        self.table_ops.setColumnCount(2)
        self.table_ops.setHorizontalHeaderLabels(
            ["Operand", "Args"])
        self.table_ops.horizontalHeader().setStretchLastSection(True)
        self.verticalLayout.addWidget(self.table_ops)

        # Action buttons
        self.hlayout_actions = QtWidgets.QHBoxLayout()
        self.btn_setup = QtWidgets.QPushButton("Setup (TVAR+TOPER)")
        self.btn_sensi = QtWidgets.QPushButton("Sensitivity (SENSI)")
        self.btn_monte = QtWidgets.QPushButton("Monte-Carlo (MONTE)")
        self.hlayout_actions.addWidget(self.btn_setup)
        self.hlayout_actions.addWidget(self.btn_sensi)
        self.hlayout_actions.addWidget(self.btn_monte)
        self.verticalLayout.addLayout(self.hlayout_actions)
        self.verticalLayout.addStretch()

        self.retranslateUi(ToperDialog)
        QtCore.QMetaObject.connectSlotsByName(ToperDialog)

    def retranslateUi(self, ToperDialog):
        pass
