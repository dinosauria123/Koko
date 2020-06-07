/********************************************************************************
** Form generated from reading UI file 'mainwindow.ui'
**
** Created by: Qt User Interface Compiler version 5.12.8
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWINDOW_H
#define UI_MAINWINDOW_H

#include <QtCore/QLocale>
#include <QtCore/QVariant>
#include <QtGui/QIcon>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QTableWidget>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MainWindow
{
public:
    QAction *actionInput_Variables;
    QAction *actionOpen;
    QAction *actionSave;
    QAction *actionImport_Zemax;
    QAction *actionImport_Code_V;
    QAction *actionExport_Zemax;
    QAction *actionExport_Code_V;
    QAction *actionQuit;
    QAction *actionInsert_Surface;
    QAction *actionDelete_Surface;
    QAction *actionInput_Glass_Model;
    QAction *actionXZ;
    QAction *actionOrtho;
    QAction *actionDistortion;
    QAction *actionField_Curvature;
    QAction *actionAstigmatism;
    QAction *actionGeometical;
    QAction *actionGeometical_Leica;
    QAction *actionDiffraction;
    QAction *actionDiffraction_Leica;
    QAction *actionSpot_Diagram;
    QAction *actionWavefront_Phase;
    QAction *actionWavefront_Intensity;
    QAction *actionPoint_Spread_Function;
    QAction *actionAll_Lens_Data;
    QAction *actionInput_Lens_Idenfier;
    QAction *actionSet_Focus;
    QAction *actionSet_ray_input_angle;
    QAction *actionParaxial_Chromatic_Focus_Shift;
    QAction *actionExport_JPEG;
    QAction *actionExport_EPS;
    QAction *actionTransverce_Sagital_Aberrations;
    QAction *actionXFAN;
    QAction *actionYFAN;
    QAction *actionXOPD;
    QAction *actionYOPD;
    QAction *actionXYOPD;
    QAction *actionYray_fans;
    QAction *actionNOPD;
    QAction *actionPOPD;
    QAction *actionXCD;
    QAction *actionYCD;
    QAction *actionXray_fans_2;
    QAction *actionYray_fans_2;
    QAction *actionXYCD;
    QAction *actionYXCD;
    QAction *actionNCD;
    QAction *actionPCD;
    QAction *actionXLA;
    QAction *actionYLA;
    QAction *actionXYLA;
    QAction *actionYXLA;
    QAction *actionNLA;
    QAction *actionPLA;
    QAction *actionNFAN;
    QAction *actionPFAN;
    QAction *actionXYFAN;
    QAction *actionYXFAN;
    QAction *actionExport_PDF;
    QAction *actionNew;
    QWidget *centralWidget;
    QGridLayout *gridLayout;
    QVBoxLayout *verticalLayout;
    QTextEdit *msgView;
    QHBoxLayout *horizontalLayout;
    QLabel *label;
    QLineEdit *cmdLine;
    QVBoxLayout *verticalLayout_2;
    QTableWidget *table;
    QTextEdit *lensPara;
    QMenuBar *menuBar;
    QMenu *menuFile;
    QMenu *menuEdit;
    QMenu *menuLens_View;
    QMenu *menuGraphs;
    QMenu *menuAberration_Fans;
    QMenu *menuTransverce_Aberrations;
    QMenu *menuComponets;
    QMenu *menuOPD;
    QMenu *menuChromatic_Differences;
    QMenu *menuLongityudinal_Abberations;
    QMenu *menuOptical_Transfar_Function;
    QMenu *menuComplex_Aperture_Function;
    QMenu *menuOptimize;
    QMenu *menuTolerancing;
    QMenu *menuMacros;
    QToolBar *mainToolBar;
    QStatusBar *statusBar;

    void setupUi(QMainWindow *MainWindow)
    {
        if (MainWindow->objectName().isEmpty())
            MainWindow->setObjectName(QString::fromUtf8("MainWindow"));
        MainWindow->resize(1200, 670);
        QIcon icon;
        icon.addFile(QString::fromUtf8("images/koko.png"), QSize(), QIcon::Normal, QIcon::Off);
        MainWindow->setWindowIcon(icon);
        actionInput_Variables = new QAction(MainWindow);
        actionInput_Variables->setObjectName(QString::fromUtf8("actionInput_Variables"));
        actionOpen = new QAction(MainWindow);
        actionOpen->setObjectName(QString::fromUtf8("actionOpen"));
        actionSave = new QAction(MainWindow);
        actionSave->setObjectName(QString::fromUtf8("actionSave"));
        actionImport_Zemax = new QAction(MainWindow);
        actionImport_Zemax->setObjectName(QString::fromUtf8("actionImport_Zemax"));
        actionImport_Code_V = new QAction(MainWindow);
        actionImport_Code_V->setObjectName(QString::fromUtf8("actionImport_Code_V"));
        actionExport_Zemax = new QAction(MainWindow);
        actionExport_Zemax->setObjectName(QString::fromUtf8("actionExport_Zemax"));
        actionExport_Code_V = new QAction(MainWindow);
        actionExport_Code_V->setObjectName(QString::fromUtf8("actionExport_Code_V"));
        actionQuit = new QAction(MainWindow);
        actionQuit->setObjectName(QString::fromUtf8("actionQuit"));
        actionInsert_Surface = new QAction(MainWindow);
        actionInsert_Surface->setObjectName(QString::fromUtf8("actionInsert_Surface"));
        actionDelete_Surface = new QAction(MainWindow);
        actionDelete_Surface->setObjectName(QString::fromUtf8("actionDelete_Surface"));
        actionInput_Glass_Model = new QAction(MainWindow);
        actionInput_Glass_Model->setObjectName(QString::fromUtf8("actionInput_Glass_Model"));
        actionXZ = new QAction(MainWindow);
        actionXZ->setObjectName(QString::fromUtf8("actionXZ"));
        actionOrtho = new QAction(MainWindow);
        actionOrtho->setObjectName(QString::fromUtf8("actionOrtho"));
        actionDistortion = new QAction(MainWindow);
        actionDistortion->setObjectName(QString::fromUtf8("actionDistortion"));
        actionField_Curvature = new QAction(MainWindow);
        actionField_Curvature->setObjectName(QString::fromUtf8("actionField_Curvature"));
        actionAstigmatism = new QAction(MainWindow);
        actionAstigmatism->setObjectName(QString::fromUtf8("actionAstigmatism"));
        actionGeometical = new QAction(MainWindow);
        actionGeometical->setObjectName(QString::fromUtf8("actionGeometical"));
        actionGeometical_Leica = new QAction(MainWindow);
        actionGeometical_Leica->setObjectName(QString::fromUtf8("actionGeometical_Leica"));
        actionDiffraction = new QAction(MainWindow);
        actionDiffraction->setObjectName(QString::fromUtf8("actionDiffraction"));
        actionDiffraction_Leica = new QAction(MainWindow);
        actionDiffraction_Leica->setObjectName(QString::fromUtf8("actionDiffraction_Leica"));
        actionSpot_Diagram = new QAction(MainWindow);
        actionSpot_Diagram->setObjectName(QString::fromUtf8("actionSpot_Diagram"));
        actionWavefront_Phase = new QAction(MainWindow);
        actionWavefront_Phase->setObjectName(QString::fromUtf8("actionWavefront_Phase"));
        actionWavefront_Intensity = new QAction(MainWindow);
        actionWavefront_Intensity->setObjectName(QString::fromUtf8("actionWavefront_Intensity"));
        actionPoint_Spread_Function = new QAction(MainWindow);
        actionPoint_Spread_Function->setObjectName(QString::fromUtf8("actionPoint_Spread_Function"));
        actionAll_Lens_Data = new QAction(MainWindow);
        actionAll_Lens_Data->setObjectName(QString::fromUtf8("actionAll_Lens_Data"));
        actionInput_Lens_Idenfier = new QAction(MainWindow);
        actionInput_Lens_Idenfier->setObjectName(QString::fromUtf8("actionInput_Lens_Idenfier"));
        actionSet_Focus = new QAction(MainWindow);
        actionSet_Focus->setObjectName(QString::fromUtf8("actionSet_Focus"));
        actionSet_ray_input_angle = new QAction(MainWindow);
        actionSet_ray_input_angle->setObjectName(QString::fromUtf8("actionSet_ray_input_angle"));
        actionParaxial_Chromatic_Focus_Shift = new QAction(MainWindow);
        actionParaxial_Chromatic_Focus_Shift->setObjectName(QString::fromUtf8("actionParaxial_Chromatic_Focus_Shift"));
        actionExport_JPEG = new QAction(MainWindow);
        actionExport_JPEG->setObjectName(QString::fromUtf8("actionExport_JPEG"));
        actionExport_EPS = new QAction(MainWindow);
        actionExport_EPS->setObjectName(QString::fromUtf8("actionExport_EPS"));
        actionTransverce_Sagital_Aberrations = new QAction(MainWindow);
        actionTransverce_Sagital_Aberrations->setObjectName(QString::fromUtf8("actionTransverce_Sagital_Aberrations"));
        actionXFAN = new QAction(MainWindow);
        actionXFAN->setObjectName(QString::fromUtf8("actionXFAN"));
        actionYFAN = new QAction(MainWindow);
        actionYFAN->setObjectName(QString::fromUtf8("actionYFAN"));
        actionXOPD = new QAction(MainWindow);
        actionXOPD->setObjectName(QString::fromUtf8("actionXOPD"));
        actionYOPD = new QAction(MainWindow);
        actionYOPD->setObjectName(QString::fromUtf8("actionYOPD"));
        actionXYOPD = new QAction(MainWindow);
        actionXYOPD->setObjectName(QString::fromUtf8("actionXYOPD"));
        actionYray_fans = new QAction(MainWindow);
        actionYray_fans->setObjectName(QString::fromUtf8("actionYray_fans"));
        actionNOPD = new QAction(MainWindow);
        actionNOPD->setObjectName(QString::fromUtf8("actionNOPD"));
        actionPOPD = new QAction(MainWindow);
        actionPOPD->setObjectName(QString::fromUtf8("actionPOPD"));
        actionXCD = new QAction(MainWindow);
        actionXCD->setObjectName(QString::fromUtf8("actionXCD"));
        actionYCD = new QAction(MainWindow);
        actionYCD->setObjectName(QString::fromUtf8("actionYCD"));
        actionXray_fans_2 = new QAction(MainWindow);
        actionXray_fans_2->setObjectName(QString::fromUtf8("actionXray_fans_2"));
        actionYray_fans_2 = new QAction(MainWindow);
        actionYray_fans_2->setObjectName(QString::fromUtf8("actionYray_fans_2"));
        actionXYCD = new QAction(MainWindow);
        actionXYCD->setObjectName(QString::fromUtf8("actionXYCD"));
        actionYXCD = new QAction(MainWindow);
        actionYXCD->setObjectName(QString::fromUtf8("actionYXCD"));
        actionNCD = new QAction(MainWindow);
        actionNCD->setObjectName(QString::fromUtf8("actionNCD"));
        actionPCD = new QAction(MainWindow);
        actionPCD->setObjectName(QString::fromUtf8("actionPCD"));
        actionXLA = new QAction(MainWindow);
        actionXLA->setObjectName(QString::fromUtf8("actionXLA"));
        actionYLA = new QAction(MainWindow);
        actionYLA->setObjectName(QString::fromUtf8("actionYLA"));
        actionXYLA = new QAction(MainWindow);
        actionXYLA->setObjectName(QString::fromUtf8("actionXYLA"));
        actionYXLA = new QAction(MainWindow);
        actionYXLA->setObjectName(QString::fromUtf8("actionYXLA"));
        actionNLA = new QAction(MainWindow);
        actionNLA->setObjectName(QString::fromUtf8("actionNLA"));
        actionPLA = new QAction(MainWindow);
        actionPLA->setObjectName(QString::fromUtf8("actionPLA"));
        actionNFAN = new QAction(MainWindow);
        actionNFAN->setObjectName(QString::fromUtf8("actionNFAN"));
        actionPFAN = new QAction(MainWindow);
        actionPFAN->setObjectName(QString::fromUtf8("actionPFAN"));
        actionXYFAN = new QAction(MainWindow);
        actionXYFAN->setObjectName(QString::fromUtf8("actionXYFAN"));
        actionYXFAN = new QAction(MainWindow);
        actionYXFAN->setObjectName(QString::fromUtf8("actionYXFAN"));
        actionExport_PDF = new QAction(MainWindow);
        actionExport_PDF->setObjectName(QString::fromUtf8("actionExport_PDF"));
        actionNew = new QAction(MainWindow);
        actionNew->setObjectName(QString::fromUtf8("actionNew"));
        centralWidget = new QWidget(MainWindow);
        centralWidget->setObjectName(QString::fromUtf8("centralWidget"));
        gridLayout = new QGridLayout(centralWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        verticalLayout = new QVBoxLayout();
        verticalLayout->setSpacing(6);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        msgView = new QTextEdit(centralWidget);
        msgView->setObjectName(QString::fromUtf8("msgView"));
        msgView->setMaximumSize(QSize(800, 2000));
        QFont font;
        font.setFamily(QString::fromUtf8("Takao P\343\202\264\343\202\267\343\203\203\343\202\257"));
        msgView->setFont(font);
        msgView->setFocusPolicy(Qt::NoFocus);
        msgView->setReadOnly(true);
        msgView->setAcceptRichText(false);

        verticalLayout->addWidget(msgView);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        label = new QLabel(centralWidget);
        label->setObjectName(QString::fromUtf8("label"));

        horizontalLayout->addWidget(label);

        cmdLine = new QLineEdit(centralWidget);
        cmdLine->setObjectName(QString::fromUtf8("cmdLine"));
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(cmdLine->sizePolicy().hasHeightForWidth());
        cmdLine->setSizePolicy(sizePolicy);
        cmdLine->setMinimumSize(QSize(700, 0));
        cmdLine->setMaximumSize(QSize(700, 16777215));

        horizontalLayout->addWidget(cmdLine);


        verticalLayout->addLayout(horizontalLayout);


        gridLayout->addLayout(verticalLayout, 1, 0, 1, 1);

        verticalLayout_2 = new QVBoxLayout();
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        table = new QTableWidget(centralWidget);
        if (table->columnCount() < 7)
            table->setColumnCount(7);
        QTableWidgetItem *__qtablewidgetitem = new QTableWidgetItem();
        table->setHorizontalHeaderItem(0, __qtablewidgetitem);
        QTableWidgetItem *__qtablewidgetitem1 = new QTableWidgetItem();
        table->setHorizontalHeaderItem(1, __qtablewidgetitem1);
        QTableWidgetItem *__qtablewidgetitem2 = new QTableWidgetItem();
        table->setHorizontalHeaderItem(2, __qtablewidgetitem2);
        QTableWidgetItem *__qtablewidgetitem3 = new QTableWidgetItem();
        table->setHorizontalHeaderItem(3, __qtablewidgetitem3);
        QTableWidgetItem *__qtablewidgetitem4 = new QTableWidgetItem();
        table->setHorizontalHeaderItem(4, __qtablewidgetitem4);
        QTableWidgetItem *__qtablewidgetitem5 = new QTableWidgetItem();
        table->setHorizontalHeaderItem(5, __qtablewidgetitem5);
        QTableWidgetItem *__qtablewidgetitem6 = new QTableWidgetItem();
        table->setHorizontalHeaderItem(6, __qtablewidgetitem6);
        if (table->rowCount() < 11)
            table->setRowCount(11);
        QTableWidgetItem *__qtablewidgetitem7 = new QTableWidgetItem();
        table->setVerticalHeaderItem(0, __qtablewidgetitem7);
        QTableWidgetItem *__qtablewidgetitem8 = new QTableWidgetItem();
        table->setVerticalHeaderItem(1, __qtablewidgetitem8);
        QTableWidgetItem *__qtablewidgetitem9 = new QTableWidgetItem();
        table->setVerticalHeaderItem(2, __qtablewidgetitem9);
        QTableWidgetItem *__qtablewidgetitem10 = new QTableWidgetItem();
        table->setVerticalHeaderItem(3, __qtablewidgetitem10);
        QTableWidgetItem *__qtablewidgetitem11 = new QTableWidgetItem();
        table->setVerticalHeaderItem(4, __qtablewidgetitem11);
        QTableWidgetItem *__qtablewidgetitem12 = new QTableWidgetItem();
        table->setVerticalHeaderItem(5, __qtablewidgetitem12);
        QTableWidgetItem *__qtablewidgetitem13 = new QTableWidgetItem();
        table->setVerticalHeaderItem(6, __qtablewidgetitem13);
        QTableWidgetItem *__qtablewidgetitem14 = new QTableWidgetItem();
        table->setVerticalHeaderItem(7, __qtablewidgetitem14);
        QTableWidgetItem *__qtablewidgetitem15 = new QTableWidgetItem();
        table->setVerticalHeaderItem(8, __qtablewidgetitem15);
        QTableWidgetItem *__qtablewidgetitem16 = new QTableWidgetItem();
        table->setVerticalHeaderItem(9, __qtablewidgetitem16);
        QTableWidgetItem *__qtablewidgetitem17 = new QTableWidgetItem();
        table->setVerticalHeaderItem(10, __qtablewidgetitem17);
        table->setObjectName(QString::fromUtf8("table"));
        table->setEnabled(true);
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(table->sizePolicy().hasHeightForWidth());
        table->setSizePolicy(sizePolicy1);
        table->setMaximumSize(QSize(800, 400));
        table->setFocusPolicy(Qt::ClickFocus);
        table->setContextMenuPolicy(Qt::CustomContextMenu);
        table->setAutoFillBackground(false);
        table->setLocale(QLocale(QLocale::English, QLocale::UnitedStates));
        table->setInputMethodHints(Qt::ImhDigitsOnly);
        table->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
        table->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
        table->setSizeAdjustPolicy(QAbstractScrollArea::AdjustToContents);

        verticalLayout_2->addWidget(table);


        gridLayout->addLayout(verticalLayout_2, 0, 0, 1, 1);

        lensPara = new QTextEdit(centralWidget);
        lensPara->setObjectName(QString::fromUtf8("lensPara"));
        lensPara->setMinimumSize(QSize(400, 590));
        lensPara->setMaximumSize(QSize(300, 590));

        gridLayout->addWidget(lensPara, 0, 1, 2, 1);

        MainWindow->setCentralWidget(centralWidget);
        menuBar = new QMenuBar(MainWindow);
        menuBar->setObjectName(QString::fromUtf8("menuBar"));
        menuBar->setGeometry(QRect(0, 0, 1200, 23));
        menuFile = new QMenu(menuBar);
        menuFile->setObjectName(QString::fromUtf8("menuFile"));
        menuEdit = new QMenu(menuBar);
        menuEdit->setObjectName(QString::fromUtf8("menuEdit"));
        menuLens_View = new QMenu(menuBar);
        menuLens_View->setObjectName(QString::fromUtf8("menuLens_View"));
        menuGraphs = new QMenu(menuBar);
        menuGraphs->setObjectName(QString::fromUtf8("menuGraphs"));
        menuAberration_Fans = new QMenu(menuGraphs);
        menuAberration_Fans->setObjectName(QString::fromUtf8("menuAberration_Fans"));
        menuTransverce_Aberrations = new QMenu(menuAberration_Fans);
        menuTransverce_Aberrations->setObjectName(QString::fromUtf8("menuTransverce_Aberrations"));
        menuComponets = new QMenu(menuAberration_Fans);
        menuComponets->setObjectName(QString::fromUtf8("menuComponets"));
        menuOPD = new QMenu(menuAberration_Fans);
        menuOPD->setObjectName(QString::fromUtf8("menuOPD"));
        menuChromatic_Differences = new QMenu(menuAberration_Fans);
        menuChromatic_Differences->setObjectName(QString::fromUtf8("menuChromatic_Differences"));
        menuLongityudinal_Abberations = new QMenu(menuAberration_Fans);
        menuLongityudinal_Abberations->setObjectName(QString::fromUtf8("menuLongityudinal_Abberations"));
        menuOptical_Transfar_Function = new QMenu(menuGraphs);
        menuOptical_Transfar_Function->setObjectName(QString::fromUtf8("menuOptical_Transfar_Function"));
        menuComplex_Aperture_Function = new QMenu(menuGraphs);
        menuComplex_Aperture_Function->setObjectName(QString::fromUtf8("menuComplex_Aperture_Function"));
        menuOptimize = new QMenu(menuBar);
        menuOptimize->setObjectName(QString::fromUtf8("menuOptimize"));
        menuTolerancing = new QMenu(menuBar);
        menuTolerancing->setObjectName(QString::fromUtf8("menuTolerancing"));
        menuMacros = new QMenu(menuBar);
        menuMacros->setObjectName(QString::fromUtf8("menuMacros"));
        MainWindow->setMenuBar(menuBar);
        mainToolBar = new QToolBar(MainWindow);
        mainToolBar->setObjectName(QString::fromUtf8("mainToolBar"));
        MainWindow->addToolBar(Qt::TopToolBarArea, mainToolBar);
        statusBar = new QStatusBar(MainWindow);
        statusBar->setObjectName(QString::fromUtf8("statusBar"));
        MainWindow->setStatusBar(statusBar);

        menuBar->addAction(menuFile->menuAction());
        menuBar->addAction(menuEdit->menuAction());
        menuBar->addAction(menuLens_View->menuAction());
        menuBar->addAction(menuGraphs->menuAction());
        menuBar->addAction(menuOptimize->menuAction());
        menuBar->addAction(menuTolerancing->menuAction());
        menuBar->addAction(menuMacros->menuAction());
        menuFile->addAction(actionNew);
        menuFile->addAction(actionOpen);
        menuFile->addAction(actionSave);
        menuFile->addSeparator();
        menuFile->addAction(actionImport_Zemax);
        menuFile->addAction(actionImport_Code_V);
        menuFile->addAction(actionExport_Zemax);
        menuFile->addAction(actionExport_Code_V);
        menuFile->addSeparator();
        menuFile->addAction(actionExport_JPEG);
        menuFile->addAction(actionExport_EPS);
        menuFile->addAction(actionExport_PDF);
        menuFile->addSeparator();
        menuFile->addAction(actionQuit);
        menuEdit->addAction(actionInsert_Surface);
        menuEdit->addAction(actionDelete_Surface);
        menuEdit->addAction(actionInput_Glass_Model);
        menuEdit->addAction(actionInput_Lens_Idenfier);
        menuEdit->addSeparator();
        menuEdit->addAction(actionAll_Lens_Data);
        menuLens_View->addAction(actionXZ);
        menuLens_View->addAction(actionOrtho);
        menuLens_View->addSeparator();
        menuLens_View->addAction(actionSet_ray_input_angle);
        menuLens_View->addAction(actionSet_Focus);
        menuGraphs->addAction(menuAberration_Fans->menuAction());
        menuGraphs->addAction(actionDistortion);
        menuGraphs->addAction(actionField_Curvature);
        menuGraphs->addAction(actionAstigmatism);
        menuGraphs->addAction(actionParaxial_Chromatic_Focus_Shift);
        menuGraphs->addAction(menuOptical_Transfar_Function->menuAction());
        menuGraphs->addAction(actionSpot_Diagram);
        menuGraphs->addAction(menuComplex_Aperture_Function->menuAction());
        menuGraphs->addAction(actionPoint_Spread_Function);
        menuAberration_Fans->addAction(menuTransverce_Aberrations->menuAction());
        menuAberration_Fans->addAction(menuComponets->menuAction());
        menuAberration_Fans->addAction(menuOPD->menuAction());
        menuAberration_Fans->addAction(menuChromatic_Differences->menuAction());
        menuAberration_Fans->addAction(menuLongityudinal_Abberations->menuAction());
        menuTransverce_Aberrations->addAction(actionXYFAN);
        menuTransverce_Aberrations->addAction(actionYXFAN);
        menuComponets->addAction(actionXFAN);
        menuComponets->addAction(actionYFAN);
        menuComponets->addSeparator();
        menuComponets->addAction(actionNFAN);
        menuComponets->addAction(actionPFAN);
        menuOPD->addAction(actionXOPD);
        menuOPD->addAction(actionYOPD);
        menuOPD->addSeparator();
        menuOPD->addAction(actionXYOPD);
        menuOPD->addSeparator();
        menuOPD->addAction(actionNOPD);
        menuOPD->addAction(actionPOPD);
        menuChromatic_Differences->addAction(actionXCD);
        menuChromatic_Differences->addAction(actionYCD);
        menuChromatic_Differences->addSeparator();
        menuChromatic_Differences->addAction(actionXYCD);
        menuChromatic_Differences->addAction(actionYXCD);
        menuChromatic_Differences->addSeparator();
        menuChromatic_Differences->addAction(actionNCD);
        menuChromatic_Differences->addAction(actionPCD);
        menuLongityudinal_Abberations->addAction(actionXLA);
        menuLongityudinal_Abberations->addAction(actionYLA);
        menuLongityudinal_Abberations->addSeparator();
        menuLongityudinal_Abberations->addAction(actionXYLA);
        menuLongityudinal_Abberations->addAction(actionYXLA);
        menuLongityudinal_Abberations->addSeparator();
        menuLongityudinal_Abberations->addAction(actionNLA);
        menuLongityudinal_Abberations->addAction(actionPLA);
        menuOptical_Transfar_Function->addAction(actionGeometical);
        menuOptical_Transfar_Function->addAction(actionGeometical_Leica);
        menuOptical_Transfar_Function->addAction(actionDiffraction);
        menuOptical_Transfar_Function->addAction(actionDiffraction_Leica);
        menuComplex_Aperture_Function->addAction(actionWavefront_Phase);
        menuComplex_Aperture_Function->addAction(actionWavefront_Intensity);
        menuOptimize->addAction(actionInput_Variables);

        retranslateUi(MainWindow);
        QObject::connect(actionInput_Variables, SIGNAL(triggered()), MainWindow, SLOT(slot_actionInput_Variables()));
        QObject::connect(actionOpen, SIGNAL(triggered()), MainWindow, SLOT(slot_actionOpen()));
        QObject::connect(actionQuit, SIGNAL(triggered()), MainWindow, SLOT(slot_quit2()));
        QObject::connect(actionSave, SIGNAL(triggered()), MainWindow, SLOT(slot_actionSave()));
        QObject::connect(actionImport_Zemax, SIGNAL(triggered()), MainWindow, SLOT(slot_actionImport_Zemax()));
        QObject::connect(actionImport_Code_V, SIGNAL(triggered()), MainWindow, SLOT(slot_actionImport_CODE_V()));
        QObject::connect(actionExport_Zemax, SIGNAL(triggered()), MainWindow, SLOT(slot_actionExport_Zemax()));
        QObject::connect(actionExport_Code_V, SIGNAL(triggered()), MainWindow, SLOT(slot_actionExport_CODE_V()));
        QObject::connect(actionInsert_Surface, SIGNAL(triggered()), MainWindow, SLOT(slot_actionInsert_surface()));
        QObject::connect(actionDelete_Surface, SIGNAL(triggered()), MainWindow, SLOT(slot_actionDelete_surface()));
        QObject::connect(actionInput_Glass_Model, SIGNAL(triggered()), MainWindow, SLOT(slot_actionModeldialog()));
        QObject::connect(actionXZ, SIGNAL(triggered()), MainWindow, SLOT(slot_actionXZ()));
        QObject::connect(actionOrtho, SIGNAL(triggered()), MainWindow, SLOT(slot_actionOrtho()));
        QObject::connect(actionDistortion, SIGNAL(triggered()), MainWindow, SLOT(slot_actionDistortion()));
        QObject::connect(actionField_Curvature, SIGNAL(triggered()), MainWindow, SLOT(slot_actionField_Curvature()));
        QObject::connect(actionAstigmatism, SIGNAL(triggered()), MainWindow, SLOT(slot_actionAstigmatism()));
        QObject::connect(actionGeometical, SIGNAL(triggered()), MainWindow, SLOT(slot_actionGeometical()));
        QObject::connect(actionGeometical_Leica, SIGNAL(triggered()), MainWindow, SLOT(slot_actionGeometical_Leica()));
        QObject::connect(actionDiffraction, SIGNAL(triggered()), MainWindow, SLOT(slot_actionDiffraction()));
        QObject::connect(actionDiffraction_Leica, SIGNAL(triggered()), MainWindow, SLOT(slot_actionDiffraction_Leica()));
        QObject::connect(actionSpot_Diagram, SIGNAL(triggered()), MainWindow, SLOT(slot_actionSpot_Diagram()));
        QObject::connect(actionWavefront_Phase, SIGNAL(triggered()), MainWindow, SLOT(slot_actionWavefront_Phase()));
        QObject::connect(actionWavefront_Intensity, SIGNAL(triggered()), MainWindow, SLOT(slot_actionWavefront_Intensity()));
        QObject::connect(actionPoint_Spread_Function, SIGNAL(triggered()), MainWindow, SLOT(slot_actionPoint_Spread_Function()));
        QObject::connect(cmdLine, SIGNAL(returnPressed()), MainWindow, SLOT(slot_commandExec()));
        QObject::connect(actionAll_Lens_Data, SIGNAL(triggered()), MainWindow, SLOT(slot_actionDisplayLensData()));
        QObject::connect(actionInput_Lens_Idenfier, SIGNAL(triggered()), MainWindow, SLOT(slot_actionInput_LensIdentifier()));
        QObject::connect(actionSet_Focus, SIGNAL(triggered()), MainWindow, SLOT(slot_focus()));
        QObject::connect(actionSet_ray_input_angle, SIGNAL(triggered()), MainWindow, SLOT(slot_actionRay_input_angle()));
        QObject::connect(actionParaxial_Chromatic_Focus_Shift, SIGNAL(triggered()), MainWindow, SLOT(slot_actionPltchrsh()));
        QObject::connect(actionExport_JPEG, SIGNAL(triggered()), MainWindow, SLOT(slot_actionExport_JPEG()));
        QObject::connect(actionExport_EPS, SIGNAL(triggered()), MainWindow, SLOT(slot_actionExport_EPS()));
        QObject::connect(actionXYFAN, SIGNAL(triggered()), MainWindow, SLOT(slot_actionXYFAN()));
        QObject::connect(actionYFAN, SIGNAL(triggered()), MainWindow, SLOT(slot_actionYFAN()));
        QObject::connect(actionXFAN, SIGNAL(triggered()), MainWindow, SLOT(slot_actionXFAN()));
        QObject::connect(actionYXFAN, SIGNAL(triggered()), MainWindow, SLOT(slot_actionYXFAN()));
        QObject::connect(actionNFAN, SIGNAL(triggered()), MainWindow, SLOT(slot_actionNFAN()));
        QObject::connect(actionPFAN, SIGNAL(triggered()), MainWindow, SLOT(slot_actionPFAN()));
        QObject::connect(actionXOPD, SIGNAL(triggered()), MainWindow, SLOT(slot_actionYOPD()));
        QObject::connect(actionYCD, SIGNAL(triggered()), MainWindow, SLOT(slot_actionYCD()));
        QObject::connect(actionXYCD, SIGNAL(triggered()), MainWindow, SLOT(slot_actionXYCD()));
        QObject::connect(actionYXCD, SIGNAL(triggered()), MainWindow, SLOT(slot_actionYXCD()));
        QObject::connect(actionNCD, SIGNAL(triggered()), MainWindow, SLOT(slot_actionNCD()));
        QObject::connect(actionPCD, SIGNAL(triggered()), MainWindow, SLOT(slot_actionPCD()));
        QObject::connect(actionXLA, SIGNAL(triggered()), MainWindow, SLOT(slot_actionXLA()));
        QObject::connect(actionYLA, SIGNAL(triggered()), MainWindow, SLOT(slot_actionYLA()));
        QObject::connect(actionNLA, SIGNAL(triggered()), MainWindow, SLOT(slot_actionNLA()));
        QObject::connect(actionXYLA, SIGNAL(triggered()), MainWindow, SLOT(slot_actionXYLA()));
        QObject::connect(actionYXLA, SIGNAL(triggered(bool)), MainWindow, SLOT(slot_actionYXLA()));
        QObject::connect(actionPLA, SIGNAL(triggered()), MainWindow, SLOT(slot_actionPLA()));
        QObject::connect(actionYOPD, SIGNAL(triggered()), MainWindow, SLOT(slot_actionYOPD()));
        QObject::connect(actionXYOPD, SIGNAL(triggered()), MainWindow, SLOT(slot_actionXYOPD()));
        QObject::connect(actionNOPD, SIGNAL(triggered()), MainWindow, SLOT(slot_actionNOPD()));
        QObject::connect(actionPOPD, SIGNAL(triggered()), MainWindow, SLOT(slot_actionPOPD()));
        QObject::connect(actionXCD, SIGNAL(triggered()), MainWindow, SLOT(slot_actionXCD()));
        QObject::connect(actionExport_PDF, SIGNAL(triggered()), MainWindow, SLOT(slot_actionExport_PDF()));
        QObject::connect(actionNew, SIGNAL(triggered()), MainWindow, SLOT(slot_actionNew()));

        QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
        MainWindow->setWindowTitle(QApplication::translate("MainWindow", "KODS GUI", nullptr));
        actionInput_Variables->setText(QApplication::translate("MainWindow", "Input Variables", nullptr));
        actionOpen->setText(QApplication::translate("MainWindow", "Open", nullptr));
        actionSave->setText(QApplication::translate("MainWindow", "Save", nullptr));
        actionImport_Zemax->setText(QApplication::translate("MainWindow", "Import Zemax", nullptr));
        actionImport_Code_V->setText(QApplication::translate("MainWindow", "Import Code-V", nullptr));
        actionExport_Zemax->setText(QApplication::translate("MainWindow", "Export Zemax", nullptr));
        actionExport_Code_V->setText(QApplication::translate("MainWindow", "Export Code-V", nullptr));
        actionQuit->setText(QApplication::translate("MainWindow", "Quit", nullptr));
        actionInsert_Surface->setText(QApplication::translate("MainWindow", "Insert Surface", nullptr));
        actionDelete_Surface->setText(QApplication::translate("MainWindow", "Delete Surface", nullptr));
        actionInput_Glass_Model->setText(QApplication::translate("MainWindow", "Input Model Glass", nullptr));
        actionXZ->setText(QApplication::translate("MainWindow", "XZ", nullptr));
        actionOrtho->setText(QApplication::translate("MainWindow", "Ortho", nullptr));
        actionDistortion->setText(QApplication::translate("MainWindow", "Distortion", nullptr));
        actionField_Curvature->setText(QApplication::translate("MainWindow", "Field Curvature", nullptr));
        actionAstigmatism->setText(QApplication::translate("MainWindow", "Astigmatism", nullptr));
        actionGeometical->setText(QApplication::translate("MainWindow", "Geometical", nullptr));
        actionGeometical_Leica->setText(QApplication::translate("MainWindow", "Geometical (Leica)", nullptr));
        actionDiffraction->setText(QApplication::translate("MainWindow", "Diffraction", nullptr));
        actionDiffraction_Leica->setText(QApplication::translate("MainWindow", "Diffraction (Leica)", nullptr));
        actionSpot_Diagram->setText(QApplication::translate("MainWindow", "Spot Diagram", nullptr));
        actionWavefront_Phase->setText(QApplication::translate("MainWindow", "Wavefront Phase", nullptr));
        actionWavefront_Intensity->setText(QApplication::translate("MainWindow", "Wavefront Intensity", nullptr));
        actionPoint_Spread_Function->setText(QApplication::translate("MainWindow", "Point Spread Function", nullptr));
        actionAll_Lens_Data->setText(QApplication::translate("MainWindow", "Display Lens Data", nullptr));
        actionInput_Lens_Idenfier->setText(QApplication::translate("MainWindow", "Input Lens Idenfier", nullptr));
        actionSet_Focus->setText(QApplication::translate("MainWindow", "Set Focus", nullptr));
        actionSet_ray_input_angle->setText(QApplication::translate("MainWindow", "Set ray input angle", nullptr));
        actionParaxial_Chromatic_Focus_Shift->setText(QApplication::translate("MainWindow", "Paraxial Chromatic Focus Shift", nullptr));
        actionExport_JPEG->setText(QApplication::translate("MainWindow", "Export JPEG", nullptr));
        actionExport_EPS->setText(QApplication::translate("MainWindow", "Export EPS", nullptr));
        actionTransverce_Sagital_Aberrations->setText(QApplication::translate("MainWindow", "Transverce Sagital Aberrations", nullptr));
        actionXFAN->setText(QApplication::translate("MainWindow", "X-fan", nullptr));
        actionYFAN->setText(QApplication::translate("MainWindow", "Y-fan", nullptr));
        actionXOPD->setText(QApplication::translate("MainWindow", "X-fan", nullptr));
        actionYOPD->setText(QApplication::translate("MainWindow", "Y-fan", nullptr));
        actionXYOPD->setText(QApplication::translate("MainWindow", "X and Yray fans", nullptr));
        actionYray_fans->setText(QApplication::translate("MainWindow", "Yray fans", nullptr));
        actionNOPD->setText(QApplication::translate("MainWindow", "N-fan", nullptr));
        actionPOPD->setText(QApplication::translate("MainWindow", "P-fan", nullptr));
        actionXCD->setText(QApplication::translate("MainWindow", "X-fans", nullptr));
        actionYCD->setText(QApplication::translate("MainWindow", "Y-fans", nullptr));
        actionXray_fans_2->setText(QApplication::translate("MainWindow", "Xray fans", nullptr));
        actionYray_fans_2->setText(QApplication::translate("MainWindow", "Yray fans", nullptr));
        actionXYCD->setText(QApplication::translate("MainWindow", "Tangental", nullptr));
        actionYXCD->setText(QApplication::translate("MainWindow", "Sagital", nullptr));
        actionNCD->setText(QApplication::translate("MainWindow", "N-fans", nullptr));
        actionPCD->setText(QApplication::translate("MainWindow", "P-fans", nullptr));
        actionXLA->setText(QApplication::translate("MainWindow", "X-componets", nullptr));
        actionYLA->setText(QApplication::translate("MainWindow", "Y-components", nullptr));
        actionXYLA->setText(QApplication::translate("MainWindow", "Tangential", nullptr));
        actionYXLA->setText(QApplication::translate("MainWindow", "sagital", nullptr));
        actionNLA->setText(QApplication::translate("MainWindow", "N-fan", nullptr));
        actionPLA->setText(QApplication::translate("MainWindow", "P-fan", nullptr));
        actionNFAN->setText(QApplication::translate("MainWindow", "N-fan", nullptr));
        actionPFAN->setText(QApplication::translate("MainWindow", "P-fan", nullptr));
        actionXYFAN->setText(QApplication::translate("MainWindow", "Tangential", nullptr));
        actionYXFAN->setText(QApplication::translate("MainWindow", "Sagital", nullptr));
        actionExport_PDF->setText(QApplication::translate("MainWindow", "Export PDF", nullptr));
        actionNew->setText(QApplication::translate("MainWindow", "New", nullptr));
        label->setText(QApplication::translate("MainWindow", "Command >", nullptr));
        QTableWidgetItem *___qtablewidgetitem = table->horizontalHeaderItem(0);
        ___qtablewidgetitem->setText(QApplication::translate("MainWindow", "Surf. property", nullptr));
        QTableWidgetItem *___qtablewidgetitem1 = table->horizontalHeaderItem(1);
        ___qtablewidgetitem1->setText(QApplication::translate("MainWindow", "Thickness", nullptr));
        QTableWidgetItem *___qtablewidgetitem2 = table->horizontalHeaderItem(2);
        ___qtablewidgetitem2->setText(QApplication::translate("MainWindow", "Material", nullptr));
        QTableWidgetItem *___qtablewidgetitem3 = table->horizontalHeaderItem(3);
        ___qtablewidgetitem3->setText(QApplication::translate("MainWindow", "Index n", nullptr));
        QTableWidgetItem *___qtablewidgetitem4 = table->horizontalHeaderItem(4);
        ___qtablewidgetitem4->setText(QApplication::translate("MainWindow", "Abbe VD", nullptr));
        QTableWidgetItem *___qtablewidgetitem5 = table->horizontalHeaderItem(5);
        ___qtablewidgetitem5->setText(QApplication::translate("MainWindow", "Radius", nullptr));
        QTableWidgetItem *___qtablewidgetitem6 = table->horizontalHeaderItem(6);
        ___qtablewidgetitem6->setText(QApplication::translate("MainWindow", "Aperture", nullptr));
        QTableWidgetItem *___qtablewidgetitem7 = table->verticalHeaderItem(0);
        ___qtablewidgetitem7->setText(QApplication::translate("MainWindow", "0", nullptr));
        QTableWidgetItem *___qtablewidgetitem8 = table->verticalHeaderItem(1);
        ___qtablewidgetitem8->setText(QApplication::translate("MainWindow", "1", nullptr));
        QTableWidgetItem *___qtablewidgetitem9 = table->verticalHeaderItem(2);
        ___qtablewidgetitem9->setText(QApplication::translate("MainWindow", "2", nullptr));
        QTableWidgetItem *___qtablewidgetitem10 = table->verticalHeaderItem(3);
        ___qtablewidgetitem10->setText(QApplication::translate("MainWindow", "3", nullptr));
        QTableWidgetItem *___qtablewidgetitem11 = table->verticalHeaderItem(4);
        ___qtablewidgetitem11->setText(QApplication::translate("MainWindow", "4", nullptr));
        QTableWidgetItem *___qtablewidgetitem12 = table->verticalHeaderItem(5);
        ___qtablewidgetitem12->setText(QApplication::translate("MainWindow", "5", nullptr));
        QTableWidgetItem *___qtablewidgetitem13 = table->verticalHeaderItem(6);
        ___qtablewidgetitem13->setText(QApplication::translate("MainWindow", "6", nullptr));
        QTableWidgetItem *___qtablewidgetitem14 = table->verticalHeaderItem(7);
        ___qtablewidgetitem14->setText(QApplication::translate("MainWindow", "7", nullptr));
        QTableWidgetItem *___qtablewidgetitem15 = table->verticalHeaderItem(8);
        ___qtablewidgetitem15->setText(QApplication::translate("MainWindow", "8", nullptr));
        QTableWidgetItem *___qtablewidgetitem16 = table->verticalHeaderItem(9);
        ___qtablewidgetitem16->setText(QApplication::translate("MainWindow", "9", nullptr));
        QTableWidgetItem *___qtablewidgetitem17 = table->verticalHeaderItem(10);
        ___qtablewidgetitem17->setText(QApplication::translate("MainWindow", "10", nullptr));
        menuFile->setTitle(QApplication::translate("MainWindow", "File", nullptr));
        menuEdit->setTitle(QApplication::translate("MainWindow", "Edit", nullptr));
        menuLens_View->setTitle(QApplication::translate("MainWindow", "Lens View", nullptr));
        menuGraphs->setTitle(QApplication::translate("MainWindow", "Analyze", nullptr));
        menuAberration_Fans->setTitle(QApplication::translate("MainWindow", "Aberration Fans", nullptr));
        menuTransverce_Aberrations->setTitle(QApplication::translate("MainWindow", "Transverce Aberrations", nullptr));
        menuComponets->setTitle(QApplication::translate("MainWindow", "Componets", nullptr));
        menuOPD->setTitle(QApplication::translate("MainWindow", "OPD", nullptr));
        menuChromatic_Differences->setTitle(QApplication::translate("MainWindow", "Chromatic Differences", nullptr));
        menuLongityudinal_Abberations->setTitle(QApplication::translate("MainWindow", "Longityudinal Abberations", nullptr));
        menuOptical_Transfar_Function->setTitle(QApplication::translate("MainWindow", "Optical Transfar Function", nullptr));
        menuComplex_Aperture_Function->setTitle(QApplication::translate("MainWindow", "Complex Aperture Function", nullptr));
        menuOptimize->setTitle(QApplication::translate("MainWindow", "Optimize", nullptr));
        menuTolerancing->setTitle(QApplication::translate("MainWindow", "Tolerancing", nullptr));
        menuMacros->setTitle(QApplication::translate("MainWindow", "Macros", nullptr));
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINDOW_H
