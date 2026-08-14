# Form implementation for the Image Blur (BMP -> PSF convolution) dialog.
# Loads a 24-bit BMP "object" image, traces it through the current lens
# (optionally applying the lens PSF per object point), and displays the
# resulting blurred image. Mirrors Koko's OFROMBMP / IMTRACE / PSFTOIMG
# / PLTIMG command chain.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_ImageBlurDialog(object):
    def setupUi(self, ImageBlurDialog):
        ImageBlurDialog.setObjectName("ImageBlurDialog")
        ImageBlurDialog.resize(460, 520)
        self.verticalLayout = QtWidgets.QVBoxLayout(ImageBlurDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        # ---- Source BMP ----
        self.groupSource = QtWidgets.QGroupBox(parent=ImageBlurDialog)
        self.groupSource.setObjectName("groupSource")
        self.groupSource.setTitle("Source Image (BMP)")
        self.formSource = QtWidgets.QFormLayout(self.groupSource)

        self.labelFile = QtWidgets.QLabel(parent=self.groupSource)
        self.labelFile.setText("24-bit BMP file:")
        self.formSource.setWidget(0, QtWidgets.QFormLayout.ItemRole.LabelRole,
                                   self.labelFile)
        self.horizontalFile = QtWidgets.QHBoxLayout()
        self.lineFile = QtWidgets.QLineEdit(parent=self.groupSource)
        self.lineFile.setObjectName("lineFile")
        self.lineFile.setReadOnly(True)
        self.horizontalFile.addWidget(self.lineFile)
        self.btnBrowse = QtWidgets.QPushButton(parent=self.groupSource)
        self.btnBrowse.setObjectName("btnBrowse")
        self.btnBrowse.setText("Browse...")
        self.horizontalFile.addWidget(self.btnBrowse)
        self.formSource.setLayout(0, QtWidgets.QFormLayout.ItemRole.FieldRole,
                                   self.horizontalFile)

        self.verticalLayout.addWidget(self.groupSource)

        # ---- Object array size ----
        self.groupArray = QtWidgets.QGroupBox(parent=ImageBlurDialog)
        self.groupArray.setObjectName("groupArray")
        self.groupArray.setTitle("Object Plane Array")
        self.formArray = QtWidgets.QFormLayout(self.groupArray)

        self.labelNX = QtWidgets.QLabel(parent=self.groupArray)
        self.labelNX.setText("NX (columns):")
        self.spinNX = QtWidgets.QSpinBox(parent=self.groupArray)
        self.spinNX.setObjectName("spinNX")
        self.spinNX.setRange(2, 1024)
        self.spinNX.setValue(128)
        self.formArray.addRow(self.labelNX, self.spinNX)

        self.labelNY = QtWidgets.QLabel(parent=self.groupArray)
        self.labelNY.setText("NY (rows):")
        self.spinNY = QtWidgets.QSpinBox(parent=self.groupArray)
        self.spinNY.setObjectName("spinNY")
        self.spinNY.setRange(2, 1024)
        self.spinNY.setValue(128)
        self.formArray.addRow(self.labelNY, self.spinNY)

        self.labelDX = QtWidgets.QLabel(parent=self.groupArray)
        self.labelDX.setText("Pixel X-length (DX):")
        self.doubleDX = QtWidgets.QDoubleSpinBox(parent=self.groupArray)
        self.doubleDX.setObjectName("doubleDX")
        self.doubleDX.setDecimals(6)
        self.doubleDX.setRange(1e-9, 1e9)
        self.doubleDX.setValue(0.01)
        self.formArray.addRow(self.labelDX, self.doubleDX)

        self.labelDY = QtWidgets.QLabel(parent=self.groupArray)
        self.labelDY.setText("Pixel Y-length (DY):")
        self.doubleDY = QtWidgets.QDoubleSpinBox(parent=self.groupArray)
        self.doubleDY.setObjectName("doubleDY")
        self.doubleDY.setDecimals(6)
        self.doubleDY.setRange(1e-9, 1e9)
        self.doubleDY.setValue(0.01)
        self.formArray.addRow(self.labelDY, self.doubleDY)

        self.btnAuto = QtWidgets.QPushButton(parent=self.groupArray)
        self.btnAuto.setObjectName("btnAuto")
        self.btnAuto.setText("Use BMP dimensions")
        self.formArray.addRow(self.btnAuto)

        self.verticalLayout.addWidget(self.groupArray)

        # ---- Blur mode ----
        self.groupMode = QtWidgets.QGroupBox(parent=ImageBlurDialog)
        self.groupMode.setObjectName("groupMode")
        self.groupMode.setTitle("Blur Mode")
        self.vboxMode = QtWidgets.QVBoxLayout(self.groupMode)

        self.radioSimple = QtWidgets.QRadioButton(parent=self.groupMode)
        self.radioSimple.setObjectName("radioSimple")
        self.radioSimple.setText("Single PSF convolution (PSFTOIMG)")
        self.radioSimple.setChecked(True)
        self.vboxMode.addWidget(self.radioSimple)

        self.radioFull = QtWidgets.QRadioButton(parent=self.groupMode)
        self.radioFull.setObjectName("radioFull")
        self.radioFull.setText("Full imaging (PSF per object point)")
        self.vboxMode.addWidget(self.radioFull)

        self.hboxChannel = QtWidgets.QHBoxLayout()
        self.labelChannel = QtWidgets.QLabel(parent=self.groupMode)
        self.labelChannel.setText("Channel:")
        self.comboChannel = QtWidgets.QComboBox(parent=self.groupMode)
        self.comboChannel.setObjectName("comboChannel")
        self.comboChannel.addItems(["Luminance (1)", "Red (2)",
                                     "Green (3)", "Blue (4)"])
        self.hboxChannel.addWidget(self.labelChannel)
        self.hboxChannel.addWidget(self.comboChannel)
        self.vboxMode.addLayout(self.hboxChannel)

        self.hboxTrim = QtWidgets.QHBoxLayout()
        self.labelTrim = QtWidgets.QLabel(parent=self.groupMode)
        self.labelTrim.setText("Trim pixels (PLTIMG):")
        self.spinTrim = QtWidgets.QSpinBox(parent=self.groupMode)
        self.spinTrim.setObjectName("spinTrim")
        self.spinTrim.setRange(0, 500)
        self.spinTrim.setValue(0)
        self.hboxTrim.addWidget(self.labelTrim)
        self.hboxTrim.addWidget(self.spinTrim)
        self.vboxMode.addLayout(self.hboxTrim)

        self.verticalLayout.addWidget(self.groupMode)

        # ---- Defaults header band ----
        self.header = QtWidgets.QLabel(parent=ImageBlurDialog)
        self.header.setObjectName("header")
        self.header.setText("Image Blur — convolve a BMP with the lens PSF")
        self.header.setStyleSheet(
            "background-color:#eef0f2; padding:6px; border:1px solid #c8ccd0;")
        self.header.setAlignment(QtCore.Qt.AlignmentFlag.AlignCenter)
        self.verticalLayout.addWidget(self.header)

        self.buttonBox = QtWidgets.QDialogButtonBox(parent=ImageBlurDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Cancel
            | QtWidgets.QDialogButtonBox.StandardButton.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.button(
            QtWidgets.QDialogButtonBox.StandardButton.Ok).setText(
            "RUN BLUR")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(ImageBlurDialog)
        self.buttonBox.accepted.connect(ImageBlurDialog.accept)
        self.buttonBox.rejected.connect(ImageBlurDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(ImageBlurDialog)

    def retranslateUi(self, ImageBlurDialog):
        pass
