////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020 The Koko Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution
//
// This file is part of Koko.
//
// Koko is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Koko is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Koko; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#ifndef __MAIN_WINDOW_H
#define __MAIN_WINDOW_H

#include <QWidget>
#include <QProcess>
#include "ui_mainwindow.h"
#include "nkdialog.h"
#include "optimize.h"
#include "lidialog.h"
#include "rayinputdialog.h"

class MainWindow : public QMainWindow, public Ui::MainWindow
{
    Q_OBJECT

public:
    MainWindow(QMainWindow *parnet=0);
    QProcess *proc;
    QString hdir,ldir,li;
    double lF,lD,lC;

    QMenu *MaterialMenu;
    QMenu *menuAir;
    QMenu *SubMenu1;
    QMenu *SubMenu2;
    QMenu *SubMenu3;
    QMenu *SubMenu4;
    QMenu *SubMenu5;
    QMenu *SubMenu6;
    QMenu *SubMenu7;
    QMenu *SubMenu8;
    QMenu *SubMenu9;

    QAction *Modelaction;
    QAction *Airaction;
    QAction *Reflaction;
    QAction *Makeraction;
    QString String;
    QAction* selectedItem;

    QTableWidgetItem *tableitem;

private:

    int nol;
    int NoGlass,NoGlass1,NoGlass2,NoGlass3,NoGlass4,NoGlass5,NoGlass6,NoGlass7,NoGlass8,NoGlass9;
    int row;
    int column;
    int LF;
    int histnum;
    int curhist;

    double numconv(int,QByteArray);

    void ndex();
    void Glass(QString);
    void DataRead(QString, QString, int);
    void ReadFileToTable(QString);
    void addcontextmenu();
    void menu();
    void delay();
    void history(int);

    nkDialog *dialog;
    Optimize *dialog2;
    LIDialog *dialog3;
    rayinputDialog *dialog4;
    QStringList GN,GN1,GN2,GN3,GN4,GN5,GN6,GN7,GN8,GN9;
    QString DispHistory;
    bool eventFilter(QObject* object, QEvent* event);
    void leaveEvent(QEvent * event);
    void enterEvent(QEvent * event);
    QString surftypeCheck(QString, QString, QString);

private slots:

    void slot_commandExec();
    void slot_appendView();

    void slot_actionOpen();
    void slot_actionSave();
    void slot_actionImport_Zemax();
    void slot_actionImport_CODE_V();
    void slot_actionExport_Zemax();
    void slot_actionExport_CODE_V();
    void slot_actionExport_JPEG();
    void slot_actionExport_EPS();
    void slot_actionExport_PDF();
    void slot_quit2();

    void slot_actionInsert_surface();
    void slot_actionDelete_surface();
    void slot_actionDisplayLensData();

    void slot_actionXZ();
    void slot_actionOrtho();
    void slot_focus();

    void slot_actionXFAN();
    void slot_actionYFAN();
    void slot_actionXYFAN();
    void slot_actionYXFAN();
    void slot_actionNFAN();
    void slot_actionPFAN();
    void slot_actionXOPD();
    void slot_actionYOPD();
    void slot_actionXYOPD();
    void slot_actionNOPD();
    void slot_actionPOPD();
    void slot_actionXCD();
    void slot_actionYCD();
    void slot_actionXYCD();
    void slot_actionYXCD();
    void slot_actionNCD();
    void slot_actionPCD();
    void slot_actionXLA();
    void slot_actionYLA();
    void slot_actionXYLA();
    void slot_actionYXLA();
    void slot_actionNLA();
    void slot_actionPLA();

    void slot_actionDistortion();
    void slot_actionField_Curvature();
    void slot_actionAstigmatism();
    void slot_actionGeometical();
    void slot_actionPltchrsh();
    void slot_actionGeometical_Leica();
    void slot_actionDiffraction();
    void slot_actionDiffraction_Leica();
    void slot_actionSpot_Diagram();
    void slot_actionWavefront_Phase();
    void slot_actionWavefront_Intensity();
    void slot_actionPoint_Spread_Function();
    void slot_action_value_entered();

    void slot_actionModeldialog();

    void slot_actionInput_Variables();

    void slot_actionInput_LensIdentifier();

    void slot_actionRay_input_angle();

    void slot_ShowContextMenu(const QPoint& Pos);
    void slot_lensInfo(int,int);


    void ShowContextMenu(QAction *Action);
    void ShowContextMenu2(QAction *Action);
    void ShowContextMenu3(QAction *Action);
    void ShowContextMenu4(QAction *Action);
    void ShowContextMenu5(QAction *Action);
    void ShowContextMenu6(QAction *Action);
    void ShowContextMenu7(QAction *Action);
    void ShowContextMenu8(QAction *Action);
    void ShowContextMenu9(QAction *Action);


    void InputAir();
    void InputReflector();
};


#endif /* __MAIN_WINDOW_H */

