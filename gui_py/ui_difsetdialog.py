# Form implementation generated for the DIFSET
# (General Diffraction Calculation Settings) dialog.
# Mirrors the original Windows/Winteracter IDD_DIFSET dialog.

from PyQt6 import QtCore, QtGui, QtWidgets


class Ui_DifsetDialog(object):
    def setupUi(self, DifsetDialog):
        DifsetDialog.setObjectName("DifsetDialog")
        DifsetDialog.resize(300, 230)
        self.verticalLayout = QtWidgets.QVBoxLayout(DifsetDialog)
        self.verticalLayout.setObjectName("verticalLayout")

        self.groupRef = QtWidgets.QGroupBox(parent=DifsetDialog)
        self.groupRef.setObjectName("groupRef")
        self.groupRef.setTitle("Reference Sphere Settings")
        self.vboxRef = QtWidgets.QVBoxLayout(self.groupRef)
        self.radioRef1 = QtWidgets.QRadioButton(parent=self.groupRef)
        self.radioRef1.setObjectName("radioRef1")
        self.radioRef1.setText("Remove Wave Front Tilt")
        self.vboxRef.addWidget(self.radioRef1)
        self.radioRef2 = QtWidgets.QRadioButton(parent=self.groupRef)
        self.radioRef2.setObjectName("radioRef2")
        self.radioRef2.setText("Remove Wave Front Tilt and Focus")
        self.vboxRef.addWidget(self.radioRef2)
        self.radioRef3 = QtWidgets.QRadioButton(parent=self.groupRef)
        self.radioRef3.setObjectName("radioRef3")
        self.radioRef3.setText("No Reference Sphere Adjustments")
        self.radioRef3.setChecked(True)
        self.vboxRef.addWidget(self.radioRef3)
        self.verticalLayout.addWidget(self.groupRef)

        self.groupExit = QtWidgets.QGroupBox(parent=DifsetDialog)
        self.groupExit.setObjectName("groupExit")
        self.groupExit.setTitle("Exit Pupil Determination")
        self.vboxExit = QtWidgets.QVBoxLayout(self.groupExit)
        self.radioEx1 = QtWidgets.QRadioButton(parent=self.groupExit)
        self.radioEx1.setObjectName("radioEx1")
        self.radioEx1.setText(
            "Automatically Find Best Exit Pupil Position Using Differential Rays")
        self.radioEx1.setChecked(True)
        self.vboxExit.addWidget(self.radioEx1)
        self.radioEx2 = QtWidgets.QRadioButton(parent=self.groupExit)
        self.radioEx2.setObjectName("radioEx2")
        self.radioEx2.setText("User Set or Paraxial Exit Pupil")
        self.vboxExit.addWidget(self.radioEx2)
        self.verticalLayout.addWidget(self.groupExit)

        self.buttonBox = QtWidgets.QDialogButtonBox(parent=DifsetDialog)
        self.buttonBox.setOrientation(QtCore.Qt.Orientation.Horizontal)
        self.buttonBox.setStandardButtons(
            QtWidgets.QDialogButtonBox.StandardButton.Cancel
            | QtWidgets.QDialogButtonBox.StandardButton.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.buttonBox.button(
            QtWidgets.QDialogButtonBox.StandardButton.Ok).setText("APPLY SETTINGS")
        self.verticalLayout.addWidget(self.buttonBox)

        self.retranslateUi(DifsetDialog)
        self.buttonBox.accepted.connect(DifsetDialog.accept)
        self.buttonBox.rejected.connect(DifsetDialog.reject)
        QtCore.QMetaObject.connectSlotsByName(DifsetDialog)

    def retranslateUi(self, DifsetDialog):
        _translate = QtCore.QCoreApplication.translate
        DifsetDialog.setWindowTitle(
            _translate("DifsetDialog", "General Diffraction Calculation Settings"))
