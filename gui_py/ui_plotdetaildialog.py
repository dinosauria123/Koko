# Form implementation for the Plot Detail (PLOT FRAME/AXIS/NOTE/PEN/UPLOT)
# dialog. These are overlay commands that modify the current plot buffer;
# they must be sent after a base plot (e.g. VIE XZ) and followed by DRAW
# to regenerate drawcmd.gpl.
#
# Mirrors KDP2 PLOTCAD1-5.FOR:
#   PLOT FRAME [x1 y1 x2 y2]   - draw rectangle (default 0 0 10000 7000)
#   PLOT AXIS                   - draw axis
#   PNOTE <text>                - set plot note text
#   PLOT NOTE x y               - draw note at position
#   PLOT PEN x y state          - move pen (state: 1=none 2=lower 3=lift)
#   PLOT UPLOT xr1 xr2 yr1 yr2  - plot user function from GPREG registers
#
from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_PlotDetailDialog(object):
    def setupUi(self, PlotDetailDialog):
        PlotDetailDialog.setObjectName("PlotDetailDialog")
        PlotDetailDialog.resize(420, 380)
        PlotDetailDialog.setWindowTitle("Plot Detail Control")

        self.verticalLayout = QtWidgets.QVBoxLayout(PlotDetailDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # Header band
        self.header = QtWidgets.QLabel(PlotDetailDialog)
        self.header.setObjectName("header")
        self.header.setText("Plot overlay controls (FRAME / AXIS / NOTE / PEN / UPLOT)")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.header.setStyleSheet(
            "QLabel#header { background-color: #eef0f2; "
            "border-bottom: 1px solid #c8ccd0; padding: 6px; "
            "font-weight: bold; }")
        self.verticalLayout.addWidget(self.header)

        # Tab widget
        self.tabs = QtWidgets.QTabWidget(PlotDetailDialog)
        self.tabs.setObjectName("tabs")

        # ---- Tab 1: Frame & Axis ----
        self.tab_frame = QtWidgets.QWidget()
        self.tab_frame.setObjectName("tab_frame")
        fl = QtWidgets.QVBoxLayout(self.tab_frame)
        fl.setContentsMargins(12, 12, 12, 12)

        self.check_frame = QtWidgets.QCheckBox(self.tab_frame)
        self.check_frame.setObjectName("check_frame")
        self.check_frame.setText("Draw frame rectangle (PLOT FRAME)")
        fl.addWidget(self.check_frame)

        self.frame_coords_widget = QtWidgets.QWidget(self.tab_frame)
        fcl = QtWidgets.QFormLayout(self.frame_coords_widget)
        fcl.setContentsMargins(20, 4, 0, 4)
        self.label_fcoords = QtWidgets.QLabel(self.frame_coords_widget)
        self.label_fcoords.setText("Coords (x1 y1 x2 y2)")
        self.lineEdit_fcoords = QtWidgets.QLineEdit(self.frame_coords_widget)
        self.lineEdit_fcoords.setObjectName("lineEdit_fcoords")
        self.lineEdit_fcoords.setText("0 0 10000 7000")
        self.lineEdit_fcoords.setToolTip(
            "Device-independent coordinates. Leave default for full frame.")
        fcl.addRow(self.label_fcoords, self.lineEdit_fcoords)
        fl.addWidget(self.frame_coords_widget)

        self.check_use_default_frame = QtWidgets.QCheckBox(self.tab_frame)
        self.check_use_default_frame.setObjectName("check_use_default_frame")
        self.check_use_default_frame.setText("Use default (0 0 10000 7000)")
        self.check_use_default_frame.setChecked(True)
        fl.addWidget(self.check_use_default_frame)

        self.check_axis = QtWidgets.QCheckBox(self.tab_frame)
        self.check_axis.setObjectName("check_axis")
        self.check_axis.setText("Draw axis (PLOT AXIS)")
        fl.addWidget(self.check_axis)

        fl.addStretch()
        self.tabs.addTab(self.tab_frame, "Frame / Axis")

        # ---- Tab 2: Note ----
        self.tab_note = QtWidgets.QWidget()
        self.tab_note.setObjectName("tab_note")
        nl = QtWidgets.QVBoxLayout(self.tab_note)
        nl.setContentsMargins(12, 12, 12, 12)

        nform = QtWidgets.QFormLayout()
        self.label_pnote = QtWidgets.QLabel(self.tab_note)
        self.label_pnote.setText("Note text (PNOTE)")
        self.lineEdit_pnote = QtWidgets.QLineEdit(self.tab_note)
        self.lineEdit_pnote.setObjectName("lineEdit_pnote")
        self.lineEdit_pnote.setText("")
        self.lineEdit_pnote.setToolTip(
            "Sets the plot note text via PNOTE command. "
            "Leave empty to skip.")
        nform.addRow(self.label_pnote, self.lineEdit_pnote)

        self.label_note_x = QtWidgets.QLabel(self.tab_note)
        self.label_note_x.setText("Note X position")
        self.spin_note_x = QtWidgets.QSpinBox(self.tab_note)
        self.spin_note_x.setObjectName("spin_note_x")
        self.spin_note_x.setMinimum(0)
        self.spin_note_x.setMaximum(10000)
        self.spin_note_x.setValue(500)
        nform.addRow(self.label_note_x, self.spin_note_x)

        self.label_note_y = QtWidgets.QLabel(self.tab_note)
        self.label_note_y.setText("Note Y position")
        self.spin_note_y = QtWidgets.QSpinBox(self.tab_note)
        self.spin_note_y.setObjectName("spin_note_y")
        self.spin_note_y.setMinimum(0)
        self.spin_note_y.setMaximum(7000)
        self.spin_note_y.setValue(6500)
        nform.addRow(self.label_note_y, self.spin_note_y)
        nl.addLayout(nform)

        self.check_note = QtWidgets.QCheckBox(self.tab_note)
        self.check_note.setObjectName("check_note")
        self.check_note.setText("Draw note at position (PLOT NOTE x y)")
        self.check_note.setToolTip(
            "Draws the PNOTE text at the specified position. "
            "Requires note text to be set.")
        nl.addWidget(self.check_note)

        nl.addStretch()
        self.tabs.addTab(self.tab_note, "Note")

        # ---- Tab 3: Pen ----
        self.tab_pen = QtWidgets.QWidget()
        self.tab_pen.setObjectName("tab_pen")
        pl = QtWidgets.QVBoxLayout(self.tab_pen)
        pl.setContentsMargins(12, 12, 12, 12)

        pform = QtWidgets.QFormLayout()
        self.label_pen_x = QtWidgets.QLabel(self.tab_pen)
        self.label_pen_x.setText("Pen X")
        self.spin_pen_x = QtWidgets.QSpinBox(self.tab_pen)
        self.spin_pen_x.setObjectName("spin_pen_x")
        self.spin_pen_x.setMinimum(0)
        self.spin_pen_x.setMaximum(10000)
        self.spin_pen_x.setValue(5000)
        pform.addRow(self.label_pen_x, self.spin_pen_x)

        self.label_pen_y = QtWidgets.QLabel(self.tab_pen)
        self.label_pen_y.setText("Pen Y")
        self.spin_pen_y = QtWidgets.QSpinBox(self.tab_pen)
        self.spin_pen_y.setObjectName("spin_pen_y")
        self.spin_pen_y.setMinimum(0)
        self.spin_pen_y.setMaximum(7000)
        self.spin_pen_y.setValue(3500)
        pform.addRow(self.label_pen_y, self.spin_pen_y)

        self.label_pen_state = QtWidgets.QLabel(self.tab_pen)
        self.label_pen_state.setText("Pen state")
        self.combo_pen_state = QtWidgets.QComboBox(self.tab_pen)
        self.combo_pen_state.setObjectName("combo_pen_state")
        self.combo_pen_state.addItems([
            "1 - No change",
            "2 - Lower pen before move",
            "3 - Lift pen before move",
        ])
        pform.addRow(self.label_pen_state, self.combo_pen_state)
        pl.addLayout(pform)

        self.check_pen = QtWidgets.QCheckBox(self.tab_pen)
        self.check_pen.setObjectName("check_pen")
        self.check_pen.setText("Move pen (PLOT PEN x y state)")
        pl.addWidget(self.check_pen)

        pl.addStretch()
        self.tabs.addTab(self.tab_pen, "Pen")

        # ---- Tab 4: User Plot ----
        self.tab_uplot = QtWidgets.QWidget()
        self.tab_uplot.setObjectName("tab_uplot")
        ul = QtWidgets.QVBoxLayout(self.tab_uplot)
        ul.setContentsMargins(12, 12, 12, 12)

        uform = QtWidgets.QFormLayout()
        self.label_uxr1 = QtWidgets.QLabel(self.tab_uplot)
        self.label_uxr1.setText("X reg start")
        self.spin_uxr1 = QtWidgets.QSpinBox(self.tab_uplot)
        self.spin_uxr1.setObjectName("spin_uxr1")
        self.spin_uxr1.setMinimum(1)
        self.spin_uxr1.setMaximum(50000)
        self.spin_uxr1.setValue(1)
        uform.addRow(self.label_uxr1, self.spin_uxr1)

        self.label_uxr2 = QtWidgets.QLabel(self.tab_uplot)
        self.label_uxr2.setText("X reg end")
        self.spin_uxr2 = QtWidgets.QSpinBox(self.tab_uplot)
        self.spin_uxr2.setObjectName("spin_uxr2")
        self.spin_uxr2.setMinimum(1)
        self.spin_uxr2.setMaximum(50000)
        self.spin_uxr2.setValue(100)
        uform.addRow(self.label_uxr2, self.spin_uxr2)

        self.label_uyr1 = QtWidgets.QLabel(self.tab_uplot)
        self.label_uyr1.setText("Y reg start")
        self.spin_uyr1 = QtWidgets.QSpinBox(self.tab_uplot)
        self.spin_uyr1.setObjectName("spin_uyr1")
        self.spin_uyr1.setMinimum(1)
        self.spin_uyr1.setMaximum(50000)
        self.spin_uyr1.setValue(101)
        uform.addRow(self.label_uyr1, self.spin_uyr1)

        self.label_uyr2 = QtWidgets.QLabel(self.tab_uplot)
        self.label_uyr2.setText("Y reg end")
        self.spin_uyr2 = QtWidgets.QSpinBox(self.tab_uplot)
        self.spin_uyr2.setObjectName("spin_uyr2")
        self.spin_uyr2.setMinimum(1)
        self.spin_uyr2.setMaximum(50000)
        self.spin_uyr2.setValue(200)
        uform.addRow(self.label_uyr2, self.spin_uyr2)
        ul.addLayout(uform)

        self.check_uplot = QtWidgets.QCheckBox(self.tab_uplot)
        self.check_uplot.setObjectName("check_uplot")
        self.check_uplot.setText("Plot user function (PLOT UPLOT)")
        self.check_uplot.setToolTip(
            "Plots data from GPREG storage registers. "
            "Requires X and Y axes to be plotted first.")
        ul.addWidget(self.check_uplot)

        ul.addStretch()
        self.tabs.addTab(self.tab_uplot, "User Plot")

        self.verticalLayout.addWidget(self.tabs)

        # Buttons
        self.buttonBox = QtWidgets.QDialogButtonBox(PlotDetailDialog)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Ok |
            QtWidgets.QDialogButtonBox.StandardButton.Cancel)
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(PlotDetailDialog)
        self.buttonBox.accepted.connect(PlotDetailDialog.accept)
        self.buttonBox.rejected.connect(PlotDetailDialog.reject)
        self.check_frame.toggled.connect(self._on_frame_toggled)
        self.check_use_default_frame.toggled.connect(self._on_default_frame_toggled)
        QtCore.QMetaObject.connectSlotsByName(PlotDetailDialog)
        self._on_frame_toggled(self.check_frame.isChecked())
        self._on_default_frame_toggled(self.check_use_default_frame.isChecked())

    def _on_frame_toggled(self, checked):
        self.check_use_default_frame.setVisible(checked)
        self.frame_coords_widget.setVisible(checked and not self.check_use_default_frame.isChecked())

    def _on_default_frame_toggled(self, checked):
        self.frame_coords_widget.setVisible(
            self.check_frame.isChecked() and not checked)

    def retranslateUi(self, PlotDetailDialog):
        pass
