# Form implementation generated for the APOD (Aperture Apodization) dialog.
# Mirrors the original Windows/Winteracter IDD_APOD dialog.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_ApodDialog(object):
    def setupUi(self, ApodDialog):
        ApodDialog.setObjectName("ApodDialog")
        ApodDialog.resize(320, 180)
        self.verticalLayout = QtWidgets.QVBoxLayout(ApodDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        self.groupBox = QtWidgets.QGroupBox(parent=ApodDialog)
        self.groupBox.setObjectName("groupBox")
        self.groupBox.setTitle("Apodization")
        self.formLayout = QtWidgets.QFormLayout(self.groupBox)

        self.radioGaussian = QtWidgets.QRadioButton(parent=self.groupBox)
        self.radioGaussian.setObjectName("radioGaussian")
        self.radioGaussian.setText("Gaussian")
        self.formLayout.addRow(self.radioGaussian)

        self.radioUniform = QtWidgets.QRadioButton(parent=self.groupBox)
        self.radioUniform.setObjectName("radioUniform")
        self.radioUniform.setText("Uniform (default)")
        self.radioUniform.setChecked(True)
        self.formLayout.addRow(self.radioUniform)

        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.labelApod = QtWidgets.QLabel(parent=self.groupBox)
        self.labelApod.setText("Ray Intensity Reduction at Pupil Edge :")
        self.horizontalLayout.addWidget(self.labelApod)
        self.doubleApod = QtWidgets.QDoubleSpinBox(parent=self.groupBox)
        self.doubleApod.setObjectName("doubleApod")
        self.doubleApod.setDecimals(6)
        self.doubleApod.setRange(0.0, 1e300)
        self.doubleApod.setValue(0.0)
        self.horizontalLayout.addWidget(self.doubleApod)
        self.labelDb = QtWidgets.QLabel(parent=self.groupBox)
        self.labelDb.setText("Decibels")
        self.horizontalLayout.addWidget(self.labelDb)
        self.formLayout.addRow(self.horizontalLayout)

        self.verticalLayout.addWidget(self.groupBox)

        self.buttonBox = QtWidgets.QDialogButtonBox(parent=ApodDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Cancel
            | QtWidgets.QDialogButtonBox.StandardButton.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.button(
            QtWidgets.QDialogButtonBox.StandardButton.Ok).setText("SET APODIZATION")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(ApodDialog)
        self.buttonBox.accepted.connect(ApodDialog.accept)
        self.buttonBox.rejected.connect(ApodDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(ApodDialog)

    def retranslateUi(self, ApodDialog):
        _translate = QtCore.QCoreApplication.translate
        ApodDialog.setWindowTitle(
            _translate("ApodDialog", "SPOT, CAPFN and PSF Apodization Settings"))
