# Form implementation for the field-curvature / astigmatism / distortion
# settings dialog (KDP2 IDD_DISAST, RAYS.INC). Each section picks an
# orientation (0 / 90 deg), a number of field points (10-50), and whether
# to plot. koko commands:
#   field curvature : FLDCV,<orient>,,<n>      then PLTFLDCV,,1
#   astigmatism     : AST,<orient>,,<n>        then PLTAST,,1
#   distortion      : DIST,<orient>,,<n>       then PLTDIST,,1
#   fisheye dist    : FISHDIST,<orient>,,<n>   then PLTFDIST,,1
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_DisastDialog(object):
    def setupUi(self, DisastDialog):
        DisastDialog.setObjectName("DisastDialog")
        DisastDialog.resize(430, 470)
        self.verticalLayout = QtWidgets.QVBoxLayout(DisastDialog)
        self.verticalLayout.setContentsMargins(12, 12, 12, 12)
        self.verticalLayout.setSpacing(8)

        # --- Field curvature ---
        self.grp_fld = QtWidgets.QGroupBox(DisastDialog)
        self.grp_fld.setTitle("Field curvature (FLDCV)")
        flf = QtWidgets.QFormLayout(self.grp_fld)
        flf.setContentsMargins(10, 10, 10, 10)
        self.label_fld_orient = QtWidgets.QLabel(self.grp_fld)
        self.label_fld_orient.setText("Orientation")
        self.combo_fld_orient = QtWidgets.QComboBox(self.grp_fld)
        self.combo_fld_orient.addItems(["0 deg (tangential)", "90 deg (sagittal)"])
        flf.addRow(self.label_fld_orient, self.combo_fld_orient)
        self.label_fld_n = QtWidgets.QLabel(self.grp_fld)
        self.label_fld_n.setText("Field points")
        self.spin_fld_n = QtWidgets.QSpinBox(self.grp_fld)
        self.spin_fld_n.setRange(10, 50)
        self.spin_fld_n.setValue(20)
        flf.addRow(self.label_fld_n, self.spin_fld_n)
        self.check_fld_plot = QtWidgets.QCheckBox(self.grp_fld)
        self.check_fld_plot.setText("Plot (PLTFLDCV)")
        self.check_fld_plot.setChecked(True)
        flf.addRow(self.check_fld_plot)
        self.btn_fld = QtWidgets.QPushButton(self.grp_fld)
        self.btn_fld.setText("Compute field curvature")
        flf.addRow(self.btn_fld)
        self.verticalLayout.addWidget(self.grp_fld)

        # --- Astigmatism ---
        self.grp_ast = QtWidgets.QGroupBox(DisastDialog)
        self.grp_ast.setTitle("Astigmatism (AST)")
        alf = QtWidgets.QFormLayout(self.grp_ast)
        alf.setContentsMargins(10, 10, 10, 10)
        self.label_ast_orient = QtWidgets.QLabel(self.grp_ast)
        self.label_ast_orient.setText("Orientation")
        self.combo_ast_orient = QtWidgets.QComboBox(self.grp_ast)
        self.combo_ast_orient.addItems(["0 deg (tangential)", "90 deg (sagittal)"])
        alf.addRow(self.label_ast_orient, self.combo_ast_orient)
        self.label_ast_n = QtWidgets.QLabel(self.grp_ast)
        self.label_ast_n.setText("Field points")
        self.spin_ast_n = QtWidgets.QSpinBox(self.grp_ast)
        self.spin_ast_n.setRange(10, 50)
        self.spin_ast_n.setValue(20)
        alf.addRow(self.label_ast_n, self.spin_ast_n)
        self.check_ast_plot = QtWidgets.QCheckBox(self.grp_ast)
        self.check_ast_plot.setText("Plot (PLTAST)")
        self.check_ast_plot.setChecked(True)
        alf.addRow(self.check_ast_plot)
        self.btn_ast = QtWidgets.QPushButton(self.grp_ast)
        self.btn_ast.setText("Compute astigmatism")
        alf.addRow(self.btn_ast)
        self.verticalLayout.addWidget(self.grp_ast)

        # --- Distortion ---
        self.grp_dist = QtWidgets.QGroupBox(DisastDialog)
        self.grp_dist.setTitle("Distortion (DIST)")
        dlf = QtWidgets.QFormLayout(self.grp_dist)
        dlf.setContentsMargins(10, 10, 10, 10)
        self.label_dist_orient = QtWidgets.QLabel(self.grp_dist)
        self.label_dist_orient.setText("Orientation")
        self.combo_dist_orient = QtWidgets.QComboBox(self.grp_dist)
        self.combo_dist_orient.addItems(["0 deg (tangential)", "90 deg (sagittal)"])
        dlf.addRow(self.label_dist_orient, self.combo_dist_orient)
        self.label_dist_n = QtWidgets.QLabel(self.grp_dist)
        self.label_dist_n.setText("Field points")
        self.spin_dist_n = QtWidgets.QSpinBox(self.grp_dist)
        self.spin_dist_n.setRange(10, 50)
        self.spin_dist_n.setValue(20)
        dlf.addRow(self.label_dist_n, self.spin_dist_n)
        self.label_dist_type = QtWidgets.QLabel(self.grp_dist)
        self.label_dist_type.setText("Projection")
        self.combo_dist_type = QtWidgets.QComboBox(self.grp_dist)
        self.combo_dist_type.addItems(["Normal (DIST)", "Fisheye (FISHDIST)"])
        dlf.addRow(self.label_dist_type, self.combo_dist_type)
        self.check_dist_plot = QtWidgets.QCheckBox(self.grp_dist)
        self.check_dist_plot.setText("Plot (PLTDIST / PLTFDIST)")
        self.check_dist_plot.setChecked(True)
        dlf.addRow(self.check_dist_plot)
        self.btn_dist = QtWidgets.QPushButton(self.grp_dist)
        self.btn_dist.setText("Compute distortion")
        dlf.addRow(self.btn_dist)
        self.verticalLayout.addWidget(self.grp_dist)

        # --- Close ---
        self.buttonBox = QtWidgets.QDialogButtonBox(DisastDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Close)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(DisastDialog)
        self.buttonBox.rejected.connect(DisastDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(DisastDialog)

    def retranslateUi(self, DisastDialog):
        _translate = QtCore.QCoreApplication.translate
        DisastDialog.setWindowTitle(
            _translate("DisastDialog",
                       "Field Curvature / Astigmatism / Distortion"))
