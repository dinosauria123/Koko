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

#include "mainwindow.h"
#include "nkdialog.h"
#include "optimize.h"
#include "lidialog.h"
#include "ui_mainwindow.h"
#include <QtWidgets>
#include <QWidget>
#include <QDebug>
#include <QLineEdit>
#include <QFont>
#include <QMenu>

MainWindow::MainWindow(QMainWindow *parent)
    : QMainWindow( parent )
{

    histnum=0;
    curhist=histnum;

    ldir="/usr/local/KODS";
    hdir="/usr/local/bin";   //For Linux, MacOSX

//    hdir="/sdcard/KODS";     //For Android



    setupUi(this);

    msgView->setFont(QFont("courier",9,QFont::Bold));
    cmdLine->setFocus();

    proc = new QProcess(this);
    QObject::connect( proc, SIGNAL(readyReadStandardOutput()), this, SLOT(slot_appendView()) ); //display stdout
    QObject::connect( proc, SIGNAL(readyReadStandardError()), this, SLOT(slot_appendView()) ); //display stderr

    table->installEventFilter(this);  //install event filter
    cmdLine->installEventFilter(this);  //install event filter

    table->setColumnCount( 7 );
    table->setRowCount( 20 );

    table ->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(table, SIGNAL(customContextMenuRequested(QPoint)),this,SLOT(slot_ShowContextMenu(QPoint)));    //context menu

    addcontextmenu();

    connect(table, SIGNAL(cellClicked(int,int)), this, SLOT(slot_lensInfo(int,int)));

    proc->start(hdir+"/koko-cli"); //For Linux, MacOSXcmdline
//    proc->start("/data/data/com.install.kods/lib/libkods.so");   //For Android
    proc->write("RTG ALL\n");
    ReadFileToTable(ldir+"/CURLENS/LENSTEXT.DAT");

    for(int i=0; i<=nol-1; i++){
            tableitem=table->item(i,3);
//            tableitem->setFlags(Qt::ItemIsEnabled);    // material is not editable
    }

    for(int i=0; i<=6; i++){
            if (table->item(0,i) == NULL){
                 continue;
            }
            tableitem=table->item(0,i);
            tableitem->setFlags(Qt::ItemIsEnabled);    // infinity distance is not editable
    }

    for(int i=0; i<=6; i++){
            if (table->item(0,i) == NULL){
                 continue;
            }
            tableitem=table->item(nol-1,i);
            tableitem->setFlags(Qt::ItemIsEnabled);    // last surface is not editable
    }

}

void MainWindow::slot_lensInfo(int row,int col)
{
   QString tabletext, surftype;
   qDebug() << row;
   qDebug() << col;

   table->selectRow(row);
   lensPara -> setText(NULL);
   lensPara -> append(li);
   lensPara -> append("Wavelength (um): "+QString::number(lF)+", "+QString::number(lD)+", "+QString::number(lC));
   tabletext = QString("Surface type:");
   tableitem = table->item(row,0);
   lensPara -> append("Surface No." + QString::number(row));
   qDebug()<<tableitem->text();
   if (tableitem -> text() == ""){
        tabletext = QString("Surface type:Spherical");
   }
   tabletext = tabletext + tableitem->text().trimmed();
   lensPara->append(tabletext);
   if (ccv[row] != " "){
        lensPara->append(ccv[row].toLatin1());
   }
   if (asphv[row] != " "){
        lensPara->append(asphv[row].toLatin1());
   }
   if (asph2v[row] != " "){
        lensPara->append(asph2v[row].toLatin1());
   }
   if (tiltv[row] != " "){
        lensPara->append(tiltv[row].toLatin1());
   }
}

void MainWindow::addcontextmenu()
{

    Glass("CHANCE.BIN");
    GN1 << GN;
    GN1.sort();
    NoGlass1=NoGlass;

    Glass("CORNIN.BIN");
    GN2 << GN;
    GN2.sort();
    NoGlass2=NoGlass;

    Glass("HIKARI.BIN");
    GN3 << GN;
    GN3.sort();
    NoGlass3=NoGlass;

    Glass("HOYA.BIN");
    GN4 << GN;
    GN4.sort();
    NoGlass4=NoGlass;

    Glass("OHARA.BIN");
    GN5 << GN;
    GN5.sort();
    NoGlass5=NoGlass;

    Glass("OHARA-O.BIN");
    GN6 << GN;
    GN6.sort();
    NoGlass6=NoGlass;

    Glass("RADHARD.BIN");
    GN7 << GN;
    GN7.sort();
    NoGlass7=NoGlass;

    Glass("SCH2000.BIN");
    GN8 << GN;
    GN8.sort();
    NoGlass8=NoGlass;

    Glass("SCHOTT.BIN");
    GN9 << GN;
    GN9.sort();
    NoGlass9=NoGlass;

    MaterialMenu=menuEdit->addMenu("Input Material");

    Airaction=new QAction("Air",this);
    MaterialMenu->addAction(Airaction);
    connect(Airaction, SIGNAL(triggered()), this, SLOT(InputAir()));

    Reflaction=new QAction("Reflector",this);
    MaterialMenu->addAction(Reflaction);
    connect(Reflaction, SIGNAL(triggered()), this, SLOT(InputReflector()));

    SubMenu1=MaterialMenu->addMenu("CHANCE");
    SubMenu2=MaterialMenu->addMenu("CORNIN");
    SubMenu3=MaterialMenu->addMenu("HIKARI");
    SubMenu4=MaterialMenu->addMenu("HOYA");
    SubMenu5=MaterialMenu->addMenu("OHARA");
    SubMenu6=MaterialMenu->addMenu("OHARA-O");
    SubMenu7=MaterialMenu->addMenu("RADHARD");
    SubMenu8=MaterialMenu->addMenu("SCH2000");
    SubMenu9=MaterialMenu->addMenu("SCHOTT");

    menuEdit->insertMenu(actionInput_Lens_Idenfier, MaterialMenu);

    for (int i=0;i<=NoGlass1;i++){

        Makeraction=new QAction(QString(GN1.at(i)),this);
        Makeraction->setData(QString(GN1.at(i)));
        this->SubMenu1->addAction(Makeraction);

    }

    connect(SubMenu1, SIGNAL(triggered(QAction*)),this,SLOT(ShowContextMenu(QAction*)));

    SubMenu1->setStyleSheet("QMenu {menu-scrollable: 1;}");


    for (int i=0;i<=NoGlass2;i++){

        Makeraction=new QAction(QString(GN2.at(i)),this);
        Makeraction->setData(QString(GN2.at(i)));
        this->SubMenu2->addAction(Makeraction);

    }

    connect(SubMenu2, SIGNAL(triggered(QAction*)),this,SLOT(ShowContextMenu2(QAction*)));

    SubMenu2->setStyleSheet("QMenu {menu-scrollable: 1;}");


    for (int i=0;i<=NoGlass3;i++){

        Makeraction=new QAction(QString(GN3.at(i)),this);
        Makeraction->setData(QString(GN3.at(i)));
        this->SubMenu3->addAction(Makeraction);

    }

    connect(SubMenu3, SIGNAL(triggered(QAction*)),this,SLOT(ShowContextMenu3(QAction*)));

    SubMenu3->setStyleSheet("QMenu {menu-scrollable: 1;}");


    for (int i=0;i<=NoGlass4;i++){

        Makeraction=new QAction(QString(GN4.at(i)),this);
        Makeraction->setData(QString(GN4.at(i)));
        this->SubMenu4->addAction(Makeraction);

    }

    connect(SubMenu4, SIGNAL(triggered(QAction*)),this,SLOT(ShowContextMenu4(QAction*)));

    SubMenu4->setStyleSheet("QMenu {menu-scrollable: 1;}");


    for (int i=0;i<=NoGlass5;i++){

        Makeraction=new QAction(QString(GN5.at(i)),this);
        Makeraction->setData(QString(GN5.at(i)));
        this->SubMenu5->addAction(Makeraction);

    }

    connect(SubMenu5, SIGNAL(triggered(QAction*)),this,SLOT(ShowContextMenu5(QAction*)));

    SubMenu5->setStyleSheet("QMenu {menu-scrollable: 1;}");


    for (int i=0;i<=NoGlass6;i++){

        Makeraction=new QAction(QString(GN6.at(i)),this);
        Makeraction->setData(QString(GN6.at(i)));
        this->SubMenu6->addAction(Makeraction);

    }

    connect(SubMenu6, SIGNAL(triggered(QAction*)),this,SLOT(ShowContextMenu6(QAction*)));

    SubMenu6->setStyleSheet("QMenu {menu-scrollable: 1;}");


    for (int i=0;i<=NoGlass7;i++){

        Makeraction=new QAction(QString(GN7.at(i)),this);
        Makeraction->setData(QString(GN7.at(i)));
        this->SubMenu7->addAction(Makeraction);

    }

    connect(SubMenu7, SIGNAL(triggered(QAction*)),this,SLOT(ShowContextMenu7(QAction*)));

    SubMenu7->setStyleSheet("QMenu {menu-scrollable: 1;}");


    for (int i=0;i<=NoGlass8;i++){

        Makeraction=new QAction(QString(GN8.at(i)),this);
        Makeraction->setData(QString(GN8.at(i)));
        this->SubMenu8->addAction(Makeraction);

    }

    connect(SubMenu8, SIGNAL(triggered(QAction*)),this,SLOT(ShowContextMenu8(QAction*)));

    SubMenu8->setStyleSheet("QMenu {menu-scrollable: 1;}");



    for (int i=0;i<=NoGlass9;i++){

        Makeraction=new QAction(QString(GN9.at(i)),this);
        Makeraction->setData(QString(GN9.at(i)));
        this->SubMenu9->addAction(Makeraction);

    }

    connect(SubMenu9, SIGNAL(triggered(QAction*)),this,SLOT(ShowContextMenu9(QAction*)));

    SubMenu9->setStyleSheet("QMenu {menu-scrollable: 1;}");

}


void MainWindow::slot_commandExec()
{
    QString cmd;

    cmd = cmdLine->text();

    QByteArray input(cmd.toLatin1()+"\n");

    msgView->append(cmd);
    proc->write(input);
    delay();

    if (input == "exit\n")
    {
        slot_quit2();
    }

    if (cmd.trimmed().startsWith("lib get"))
    {
        proc->write("LENSSAVE\n");
        proc->write("RTG ALL\n");
        delay();
        ReadFileToTable(ldir+"/LENSES/LENS.PRG");
    }

    cmdLine->clear();
    cmdLine->setFocus();

}


void MainWindow::slot_appendView()
{
    QTextCodec *codec = QTextCodec::codecForName( "UTF8" );
    QString str( codec->toUnicode(proc->readAllStandardOutput()) );
    QString str2( codec->toUnicode(proc->readAllStandardError()) );
    msgView->append(str);
    msgView->append(str2);
}


void MainWindow::ShowContextMenu(QAction *Action)

{
    QString str = Action->text();
    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);
//    qDebug() << Action->parent()->objectName();

    LF=18;
    DataRead("CHANCE.BIN",str,row);
    table->setItem(row, 3, new QTableWidgetItem("CHANCE "+str));  //Change table value to MODEL
    table->resizeColumnToContents(3);
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    proc->write("U L\n");
    proc->write("CHG "+Qrow.toLatin1()+"\n");
    proc->write("CHANCE "+str.toLatin1()+"\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
}

void MainWindow::ShowContextMenu2(QAction *Action)

{
    QString str = Action->text();
    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);
 //   qDebug() << Action->parent()->objectName();

    LF=18;
    DataRead("CORNIN.BIN",str,row);
    table->setItem(row, 3, new QTableWidgetItem("CORNIN "+str));  //Change table value to MODEL
    table->resizeColumnToContents(3);
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    proc->write("U L\n");
    proc->write("CHG "+Qrow.toLatin1()+"\n");
    proc->write("CORNIN "+str.toLatin1()+"\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
}

void MainWindow::ShowContextMenu3(QAction *Action)

{
    QString str = Action->text();
    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);
//    qDebug() << Action->parent()->objectName();

    LF=18;
    DataRead("HIKARI.BIN",str,row);
    table->setItem(row, 3, new QTableWidgetItem("HIKARI "+str));  //Change table value to MODEL
    table->resizeColumnToContents(3);
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    proc->write("U L\n");
    proc->write("CHG "+Qrow.toLatin1()+"\n");
    proc->write("HIKARI "+str.toLatin1()+"\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
}

void MainWindow::ShowContextMenu4(QAction *Action)

{
    QString str = Action->text();
    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);
//    qDebug() << Action->parent()->objectName();

    LF=18;
    DataRead("HOYA.BIN",str,row);
    table->setItem(row, 3, new QTableWidgetItem("HOYA "+str));  //Change table value to MODEL
    table->resizeColumnToContents(3);
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    proc->write("U L\n");
    proc->write("CHG "+Qrow.toLatin1()+"\n");
    proc->write("HOYA "+str.toLatin1()+"\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
}

void MainWindow::ShowContextMenu5(QAction *Action)

{
    QString str = Action->text();
    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);
//    qDebug() << Action->parent()->objectName();

    LF=18;
    DataRead("OHARA.BIN",str,row);
    table->setItem(row, 3, new QTableWidgetItem("OHARA "+str));  //Change table value to MODEL
    table->resizeColumnToContents(3);
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    proc->write("U L\n");
    proc->write("CHG "+Qrow.toLatin1()+"\n");
    proc->write("OHARA "+str.toLatin1()+"\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
}

void MainWindow::ShowContextMenu6(QAction *Action)

{
    QString str = Action->text();
    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);
//    qDebug() << Action->parent()->objectName();

    LF=18;
    DataRead("OHARA-O.BIN",str,row);
    table->setItem(row, 3, new QTableWidgetItem("OHARA-O "+str));  //Change table value to MODEL
    table->resizeColumnToContents(3);
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    proc->write("U L\n");
    proc->write("CHG "+Qrow.toLatin1()+"\n");
    proc->write("OHARA-O "+str.toLatin1()+"\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
}

void MainWindow::ShowContextMenu7(QAction *Action)

{
    QString str = Action->text();
    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);
//    qDebug() << Action->parent()->objectName();

    LF=18;
    DataRead("RADHARD.BIN",str,row);
    table->setItem(row, 3, new QTableWidgetItem("RADHARD "+str));  //Change table value to MODEL
    table->resizeColumnToContents(3);
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    proc->write("U L\n");
    proc->write("CHG "+Qrow.toLatin1()+"\n");
    proc->write("RADHARD "+str.toLatin1()+"\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
}

void MainWindow::ShowContextMenu8(QAction *Action)

{
    QString str = Action->text();
    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);
//    qDebug() << Action->parent()->objectName();

    LF=18;
    DataRead("SCH2000.BIN",str,row);
    table->setItem(row, 3, new QTableWidgetItem("SCH2000 "+str));  //Change table value to MODEL
    table->resizeColumnToContents(3);
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    proc->write("U L\n");
    proc->write("CHG "+Qrow.toLatin1()+"\n");
    proc->write("SCH2000 "+str.toLatin1()+"\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
}

void MainWindow::ShowContextMenu9(QAction *Action)

{
    QString str = Action->text();
    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);
//    qDebug() << Action->parent()->objectName();

    LF=18;
    DataRead("SCHOTT.BIN",str,row);
    table->setItem(row, 3, new QTableWidgetItem("SCHOTT "+str));  //Change table value to MODEL
    table->resizeColumnToContents(3);
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    proc->write("U L\n");
    proc->write("CHG "+Qrow.toLatin1()+"\n");
    proc->write("SCHOTT "+str.toLatin1()+"\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
}



void MainWindow::InputAir()
{
    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);

    table->setItem(row, 3, new QTableWidgetItem("AIR"));
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    table->setItem(row, 4, new QTableWidgetItem(""));
    table->setItem(row, 5, new QTableWidgetItem(""));
    proc->write("U L\n");
    proc->write("CHG "+QString(Qrow).toLatin1()+"\n");
    proc->write("AIR\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
}

void MainWindow::InputReflector()
{
    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);

    table->setItem(row, 3, new QTableWidgetItem("REFL"));
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    table->setItem(row, 4, new QTableWidgetItem(""));
    table->setItem(row, 5, new QTableWidgetItem(""));
    proc->write("U L\n");
    proc->write("CHG "+QString(Qrow).toLatin1()+"\n");
    proc->write("REFL\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
}


void MainWindow::slot_ShowContextMenu(const QPoint& Pos){

    row = table->currentRow();          //get number of row
    QString Qrow = QString::number(row);
    column = table->currentColumn();    //get number of column

    QMenu ContextMenu(this);
    QMenu SubMenu1("CHANCE");
    QMenu SubMenu2("CORNIN");
    QMenu SubMenu3("HIKARI");
    QMenu SubMenu4("HOYA");
    QMenu SubMenu5("OHARA");
    QMenu SubMenu6("OHARA-O");
    QMenu SubMenu7("RADHARD");
    QMenu SubMenu8("SCH2000");
    QMenu SubMenu9("SCHOTT");

    ContextMenu.addAction("Insert Surface");
    ContextMenu.addAction("Delete Surface");
    ContextMenu.addSeparator();
    ContextMenu.addAction("Model");
    ContextMenu.addAction("AIR");
    ContextMenu.addAction("REFLECTOR");

    ContextMenu.addMenu(&SubMenu1);
    for (int i=0;i<=NoGlass1;i++){
        SubMenu1.addAction(QString(GN1.at(i)).toLatin1());
        SubMenu1.setStyleSheet("QMenu {menu-scrollable: 1;}");
    }

    ContextMenu.addMenu(&SubMenu2);
    for (int i=0;i<=NoGlass2;i++){
        SubMenu2.addAction(QString(GN2.at(i)).toLatin1());
        SubMenu2.setStyleSheet("QMenu {menu-scrollable: 1;}");
    }

    ContextMenu.addMenu(&SubMenu3);
    for (int i=0;i<=NoGlass3;i++){
        SubMenu3.addAction(QString(GN3.at(i)).toLatin1());
        SubMenu3.setStyleSheet("QMenu {menu-scrollable: 1;}");
    }

    ContextMenu.addMenu(&SubMenu4);
    for (int i=0;i<=NoGlass4;i++){
        SubMenu4.addAction(QString(GN4.at(i)).toLatin1());
        SubMenu4.setStyleSheet("QMenu {menu-scrollable: 1;}");
    }

    ContextMenu.addMenu(&SubMenu5);
    for (int i=0;i<=NoGlass5;i++){
        SubMenu5.addAction(QString(GN5.at(i)).toLatin1());
        SubMenu5.setStyleSheet("QMenu {menu-scrollable: 1;}");
    }

    ContextMenu.addMenu(&SubMenu6);
      for (int i=0;i<=NoGlass6;i++){
        SubMenu6.addAction(QString(GN6.at(i)).toLatin1());
        SubMenu6.setStyleSheet("QMenu {menu-scrollable: 1;}");
    }

    ContextMenu.addMenu(&SubMenu7);
    for (int i=0;i<=NoGlass7;i++){
        SubMenu7.addAction(QString(GN7.at(i)).toLatin1());
        SubMenu7.setStyleSheet("QMenu {menu-scrollable: 1;}");
    }

    ContextMenu.addMenu(&SubMenu8);
    for (int i=0;i<=NoGlass8;i++){
        SubMenu8.addAction(QString(GN8.at(i)).toLatin1());
        SubMenu8.setStyleSheet("QMenu {menu-scrollable: 1;}");
    }

    ContextMenu.addMenu(&SubMenu9);
    for (int i=0;i<=NoGlass9;i++){
        SubMenu9.addAction(QString(GN9.at(i)).toLatin1());
        SubMenu9.setStyleSheet("QMenu {menu-scrollable: 1;}");
    }


    QAction* selectedItem = ContextMenu.exec(table->mapToGlobal(Pos));

    if (selectedItem==NULL){
        return;
    }


    if (selectedItem->text()==QString("Insert Surface")){
        slot_actionInsert_surface();
    }

    if (selectedItem->text()==QString("Delete Surface")){
        slot_actionDelete_surface();
    }

    if (selectedItem->text()==QString("Model")){
        slot_actionModeldialog();
        table->resizeColumnToContents(3);
    }

    if (selectedItem->text()==QString("AIR")){
        InputAir();
    }

    if (selectedItem->text()==QString("REFLECTOR")){
        InputReflector();
    }


    if (selectedItem->parent()==&SubMenu1){
        LF=18;
        DataRead("CHANCE.BIN",selectedItem->text(),row);
        table->setItem(row, 3, new QTableWidgetItem("CHANCE "+selectedItem->text()));  //Change table value to MODEL
        table->resizeColumnToContents(3);
        tableitem=table->item(row,3);
//        tableitem->setFlags(Qt::ItemIsEnabled);
        proc->write("U L\n");
        proc->write("CHG "+Qrow.toLatin1()+"\n");
        proc->write("CHANCE "+selectedItem->text().toLatin1()+"\n");
        proc->write("EOS\n");
        proc->write("RTG ALL\n");
        return;
    }

    if (selectedItem->parent()==&SubMenu2){
        LF=18;
        DataRead("CORNIN.BIN",selectedItem->text(),row);
        table->setItem(row, 3, new QTableWidgetItem("CORNIN "+selectedItem->text()));  //Change table value to MODEL
        table->resizeColumnToContents(3);
        tableitem=table->item(row,3);
//        tableitem->setFlags(Qt::ItemIsEnabled);
        proc->write("U L\n");
        proc->write("CHG "+Qrow.toLatin1()+"\n");
        proc->write("CORNIN "+selectedItem->text().toLatin1()+"\n");
        proc->write("EOS\n");
        proc->write("RTG ALL\n");
        return;
    }

    if (selectedItem->parent()==&SubMenu3){
        LF=18;
        DataRead("HIKARI.BIN",selectedItem->text(),row);
        table->setItem(row, 3, new QTableWidgetItem("HIKARI "+selectedItem->text()));  //Change table value to MODEL
        table->resizeColumnToContents(3);
        tableitem=table->item(row,3);
//        tableitem->setFlags(Qt::ItemIsEnabled);
        proc->write("U L\n");
        proc->write("CHG "+Qrow.toLatin1()+"\n");
        proc->write("HIKARI "+selectedItem->text().toLatin1()+"\n");
        proc->write("EOS\n");
        proc->write("RTG ALL\n");
        return;
    }

    if (selectedItem->parent()==&SubMenu4){
        LF=18;
        DataRead("HOYA.BIN",selectedItem->text(),row);
        table->setItem(row, 3, new QTableWidgetItem("HOYA "+selectedItem->text()));  //Change table value to MODEL
        table->resizeColumnToContents(3);
        tableitem=table->item(row,3);
//        tableitem->setFlags(Qt::ItemIsEnabled);
        proc->write("U L\n");
        proc->write("CHG "+Qrow.toLatin1()+"\n");
        proc->write("HOYA "+selectedItem->text().toLatin1()+"\n");
        proc->write("EOS\n");
        proc->write("RTG ALL\n");
        return;
    }

    if (selectedItem->parent()==&SubMenu5){
        LF=18;
        DataRead("OHARA.BIN",selectedItem->text(),row);
        table->setItem(row, 3, new QTableWidgetItem("OHARA "+selectedItem->text()));  //Change table value to MODEL
        table->resizeColumnToContents(3);
        tableitem=table->item(row,3);
//        tableitem->setFlags(Qt::ItemIsEnabled);
        proc->write("U L\n");
        proc->write("CHG "+Qrow.toLatin1()+"\n");
        proc->write("OHARA "+selectedItem->text().toLatin1()+"\n");
        proc->write("EOS\n");
        proc->write("RTG ALL\n");
        return;
    }

    if (selectedItem->parent()==&SubMenu6){
        LF=18;
        DataRead("OHARA-O.BIN",selectedItem->text(),row);
        table->setItem(row, 3, new QTableWidgetItem("OHARA-O "+selectedItem->text()));  //Change table value to MODEL
        table->resizeColumnToContents(3);
        tableitem=table->item(row,3);
//        tableitem->setFlags(Qt::ItemIsEnabled);
        proc->write("U L\n");
        proc->write("CHG "+Qrow.toLatin1()+"\n");
        proc->write("OHARA "+selectedItem->text().toLatin1()+"\n");
        proc->write("EOS\n");
        proc->write("RTG ALL\n");
        return;
    }

    if (selectedItem->parent()==&SubMenu7){
        LF=18;
        DataRead("RADHARD.BIN",selectedItem->text(),row);
        table->setItem(row, 3, new QTableWidgetItem("RADHARD "+selectedItem->text()));  //Change table value to MODEL
        table->resizeColumnToContents(3);
        tableitem=table->item(row,3);
//        tableitem->setFlags(Qt::ItemIsEnabled);
        proc->write("U L\n");
        proc->write("CHG "+Qrow.toLatin1()+"\n");
        proc->write("RADHARD "+selectedItem->text().toLatin1()+"\n");
        proc->write("EOS\n");
        proc->write("RTG ALL\n");
        return;
    }

    if (selectedItem->parent()==&SubMenu8){
        LF=18;
        DataRead("SCH2000.BIN",selectedItem->text(),row);
        table->setItem(row, 3, new QTableWidgetItem("SCH2000 "+selectedItem->text()));  //Change table value to MODEL
        table->resizeColumnToContents(3);
        tableitem=table->item(row,3);
//        tableitem->setFlags(Qt::ItemIsEnabled);
        proc->write("U L\n");
        proc->write("CHG "+Qrow.toLatin1()+"\n");
        proc->write("SCH2000 "+selectedItem->text().toLatin1()+"\n");
        proc->write("EOS\n");
        proc->write("RTG ALL\n");
        return;
    }

    if (selectedItem->parent()==&SubMenu9){
        LF=18;
        DataRead("SCHOTT.BIN",selectedItem->text(),row);
        table->setItem(row, 3, new QTableWidgetItem("SCHOTT "+selectedItem->text()));  //Change table value to MODEL
        table->resizeColumnToContents(3);
        tableitem=table->item(row,3);
//        tableitem->setFlags(Qt::ItemIsEnabled);
        proc->write("U L\n");
        proc->write("CHG "+Qrow.toLatin1()+"\n");
        proc->write("SCHOTT "+selectedItem->text().toLatin1()+"\n");
        proc->write("EOS\n");
        proc->write("RTG ALL\n");
        return;
    }

    else return;

}


void MainWindow::Glass(QString BinName){

    QByteArray HexData;
    GN.clear();

    QFile GlassData(ldir+"/LIBGLA/" + BinName);                 //set data file

    GlassData.open(QIODevice::ReadOnly);                    //file open
    HexData = GlassData.readAll();                          //Read all data to Hex
    GlassData.close();

    int DataSize = int(GlassData.size());
    int j=0;                                                //skip first 2 words ('CA')

    QByteArray GlassName;

    int i=132;

    while (i<=DataSize){


        while (HexData[i]==char(0x00)){                                   //skip '00'
            i++;
        }

        for (int k=i;k<=i+8;k++){
            GlassName= GlassName+HexData[k];
        }
        i=i+8;

        GN << GlassName;
        j++;
        i+=100;

        GlassName.clear();
    }

    NoGlass=j-2;

    return;

}


void MainWindow::DataRead(QString BinName, QString name, int k){

    QByteArray HexData;
    QByteArray GlassName;
    GlassName.clear();
    GN.clear();

    double nF,nD,nC;


    QFile GlassData(ldir+"/LIBGLA/"+ BinName);                 //set data file
    GlassData.open(QIODevice::ReadOnly);                    //file open
    HexData = GlassData.readAll();                          //Read all data to Hex
    GlassData.close();

    int i=2;                                                 //skip first 2 word

    while (GlassName!=name.trimmed()){

        while (HexData[i]==char(0x00)){                                   //skip '00'
            i++;
        }

        GlassName.clear();

            for(int j=i;j<=i+10;j++){                                       //Read GlassName (10 letters)
                GlassName=(GlassName+HexData[j]).trimmed();
            }
//                qDebug()<<GlassName;
            if (GlassName==name.trimmed()) {
                break;
             }

            i=i+74;
    }

      i=i+10;
      GN << GlassName;
//    qDebug() << "GlassName= "+GlassName;

      while (HexData[i]==char(0x20)){                                 //skip '0x20'
          i++;
      }


      while (HexData[i]!=char(0x20)){                                 //read catalog No. until '0x20'
//              qDebug() << char(HexData[i]);
        i++;
      }


      while (HexData[i]==char(0x20)){                                   //skip '0x20'
        i++;
      }


    double A0,A1,A2,A3,A4,A5;
                                                                       // Read binary data
        A0=numconv(i,HexData);
        A1=numconv(i+8,HexData);
        A2=numconv(i+16,HexData);
        A3=numconv(i+24,HexData);
        A4=numconv(i+32,HexData);
        A5=numconv(i+40,HexData);

    if (BinName=="SCHOTT.BIN"||BinName=="SCH2000.BIN"||BinName=="OHARA.BIN"){
//        qDebug() << BinName;
        nF=sqrt(1+((A0*lF*lF)/(lF*lF-A3))+((A1*lF*lF)/(lF*lF-A4))+((A2*lF*lF)/(lF*lF-A5)));
        nD=sqrt(1+((A0*lD*lD)/(lD*lD-A3))+((A1*lD*lD)/(lD*lD-A4))+((A2*lD*lD)/(lD*lD-A5)));
        nC=sqrt(1+((A0*lC*lC)/(lC*lC-A3))+((A1*lC*lC)/(lC*lC-A4))+((A2*lC*lC)/(lC*lC-A5)));
        }

    else{
//        qDebug() << BinName;
        nF=sqrt(A0+A1*pow(lF,2)+A2*1/pow(lF,2)+A3*1/pow(lF,4)+A4*1/pow(lF,6)+A5*1/pow(lF,8));
        nD=sqrt(A0+A1*pow(lD,2)+A2*1/pow(lD,2)+A3*1/pow(lD,4)+A4*1/pow(lD,6)+A5*1/pow(lD,8));
        nC=sqrt(A0+A1*pow(lC,2)+A2*1/pow(lC,2)+A3*1/pow(lC,4)+A4*1/pow(lC,6)+A5*1/pow(lC,8));
    }

    double Abbe=(nD-1)/(nF-nC);

    qDebug() << nD;
    qDebug() << Abbe;

    table->setItem(k,4,new QTableWidgetItem(QString::number(nD,'f',4)));
    table->setItem(k,5,new QTableWidgetItem(QString::number(Abbe,'f',1)));

//    table->item(k,4)->setFlags(Qt::ItemIsEnabled); //Index & Abbe are not editable.
//    table->item(k,5)->setFlags(Qt::ItemIsEnabled);

    return;

}


double MainWindow::numconv(int k,QByteArray HexData){

    QByteArray HEXarray0="";
    QByteArray HEXarray="";

    for (int j=k;j<=k+7;j++){
        HEXarray0=HEXarray0+HexData[j];                         //read numerical data
    }

    HEXarray0=HEXarray0.toHex();

    bool ok;
    int sign = 1;

    for (int i=15;i>0;i-=2){
        HEXarray[15-i] = HEXarray0[i-1];
        HEXarray[15-i+1] = HEXarray0[i];
    }


    QByteArray array = QByteArray::number(HEXarray.toULongLong(&ok,16),2); //convert hex to binary -you don't need this since your incoming data is binary

    if(array.length()==64) {
        if(array.at(0)=='1') sign =-1; // if bit 0 is 1 number is negative
        array.remove(0,1); // remove sign bit
    }
    QByteArray fraction0 =array.right(52); //get the fractional part
    QByteArray fraction = fraction0;

    double mantissa = 1;

    for(int i=0;i<fraction.length();i++){ // iterate through the array to calculate the fraction as a decimal.
        if(fraction.at(i)=='1') mantissa += 1.0/(pow(2,i+1));
    }

    int exponent = array.left(array.length()-52).toULongLong(&ok,2); //calculate the exponent
    QString Ax = QString::number( sign*pow(2,exponent-1023)*(mantissa),'e', 10 );
    double num = Ax.toDouble();

    return num;

}


void MainWindow::ReadFileToTable(QString pathname)
{
    ccv.clear();
    asphv.clear();
    asph2v.clear();
    tiltv.clear();

    QFile textFile(pathname);
    QString buffer = "";
    textFile.open(QIODevice::ReadOnly); // file open

    QTextStream input(&textFile);
    buffer = input.readAll();    // read data
    textFile.close();

    QStringList lines;
    nol = buffer.count("CV"); //number of lines of lens data file
    int nof = buffer.count("\n"); //total number of lines in lens data file
    lines << buffer.split("\n");  //separate single lines

    int k=0;
    QString item;

    int comma;
    int comma2;
    QString separeter;

    QStringList label;
    for (int i=0; i<=nol-1; i++){
        label << QString::number(i);    //set table vetical label start to 0
    }

    for(int i=0; i<=nof; i++){
        item = lines[i];

        QRegExp RegExp10("LI,*");
        RegExp10.setPatternSyntax(QRegExp::Wildcard); //pick up Lens Identifier
        if (RegExp10.exactMatch(lines[i])){
           li = lines[i];
	   qDebug() << li;
        }

        QStringList lambda;
        QRegExp RegExp("WV *");
        RegExp.setPatternSyntax(QRegExp::Wildcard); //pick up Wavelength

        if (RegExp.exactMatch(item)){

            for(int j=0; j<=100; j++){
                separeter = item.mid(j,1);

                if (separeter == "."){
                lambda << item.mid(j-1,7);

                }

            }

            lD=lambda[0].toDouble();
            lF=lambda[1].toDouble();
            lC=lambda[2].toDouble();

            qDebug() << lD;
            qDebug() << lF;
            qDebug() << lC;

        }
    }

    QString units;
    QRegExp RegExp0("UNITS *");
    RegExp0.setPatternSyntax(QRegExp::Wildcard); //pick up Units

    for(int i=0; i<=nof; i++){

        item = lines[i];

        if (RegExp0.exactMatch(item)){
           units = item.trimmed().right(2).toLower();
           qDebug() << units;
        }
    }

    QString DpR;
    QString DpT;
    QString DpA;

    DpR = "Radius ("+units+")";
    QByteArray ba1 = DpR.toLocal8Bit();
    const char *DispRadius = ba1.data();

    DpT = "Thickness ("+units+")";
    QByteArray ba2 = DpT.toLocal8Bit();
    const char *DispThickness = ba2.data();

    DpA = "Aperture ("+units+")";
    QByteArray ba3 = DpA.toLocal8Bit();
    const char *DispAperture = ba3.data();

    table->clear();
    table->setRowCount( nol );          //number of row = nol
    table->setVerticalHeaderLabels(label);
    table->setHorizontalHeaderLabels( QStringList() << tr("Surface Type")<< tr(DispRadius) << tr(DispThickness ) << tr("Material") << tr("Index n")<< tr("Abbe V%1").arg(QChar(0x1D05))<< tr(DispAperture));

    k=0;
    QString surftype;

    for(int i=0; i<=nof; i++){        
        table->setItem( k, 0, new QTableWidgetItem(""));

        surftype = surftype + surftypeCheck("CC *", "Conic ", lines[i]);
        surftype = surftype + surftypeCheck("ASPH *", "Asphare ", lines[i]);
        surftype = surftype + surftypeCheck("TILT *", "Tilt ", lines[i]);
        surftype = surftype + surftypeCheck("REFS*", "REFS ", lines[i]);
        surftype = surftype + surftypeCheck("ASTOP*", "STOP ", lines[i]);

        QRegExp RegExp3("C THE FOLLOWING DATA REFERS TO SURFACE*");
        RegExp3.setPatternSyntax(QRegExp::Wildcard);
        if (RegExp3.exactMatch(lines[i])){
            table->setItem(k-1,0,new QTableWidgetItem(surftype));
            table->resizeColumnToContents(0);
            surftype = "";
            ccv << QString(" ");
            asphv << QString(" ");
            asph2v << QString(" ");
            tiltv << QString(" ");
            k++;
        }


    QString radius;
    QString curveture;
    QRegExp RegExp1("CV *");
    RegExp1.setPatternSyntax(QRegExp::Wildcard); //pick up lens curveture

        comma = item.indexOf(",")+1; // pick up numenical value
        curveture = item.trimmed().mid(comma,25);
        radius = radius.setNum(1.0E0/curveture.toDouble(),'g',6); //convert curveture to radius

        if (RegExp1.exactMatch(item)){
        table->setItem( k-1, 1, new QTableWidgetItem(radius));
        }

    QString thickness;
    QRegExp RegExp2("TH *");
    RegExp2.setPatternSyntax(QRegExp::Wildcard);  //pick up lens thickness


        item = lines[i];
        comma = item.indexOf(",")+1;

        if (RegExp2.exactMatch(item)){
        thickness = item.trimmed().mid(comma,25);
        thickness = thickness.setNum(thickness.toDouble(),'g',6);
        table->setItem( k-1, 2, new QTableWidgetItem(thickness));
        }

    QString aperture;
    QRegExp RegExp4("CLAP *");
    RegExp4.setPatternSyntax(QRegExp::Wildcard);  //pick up lens aperture radius

        item = lines[i];
        comma = item.indexOf(",")+1;
        comma2 =comma+item.indexOf(",");

        if (RegExp4.exactMatch(item)){
            aperture = item.trimmed().mid(comma,comma2-comma);
            aperture = aperture.setNum(aperture.toDouble(),'g',6);
            table->setItem( k-1, 6, new QTableWidgetItem(aperture));
        }

        QRegExp RegExp11("CC *");
        RegExp11.setPatternSyntax(QRegExp::Wildcard); //pick up Conic constant
        if (RegExp11.exactMatch(item)){
           ccv[k-1] = item;
        }

        QRegExp RegExp12("ASPH2 *");
        RegExp12.setPatternSyntax(QRegExp::Wildcard); //pick up aspheric constant
        if (RegExp12.exactMatch(item)){
           asph2v[k-1] = item;
           continue;
        }

        QRegExp RegExp13("ASPH *");
        RegExp13.setPatternSyntax(QRegExp::Wildcard); //pick up aspheric constant
        if (RegExp13.exactMatch(item)){
           asphv[k-1] = item;
        }

        QRegExp RegExp14("TILT *");
        RegExp14.setPatternSyntax(QRegExp::Wildcard); //pick up aspheric constant
        if (RegExp14.exactMatch(item)){
           tiltv[k-1] = item;
        }
    }

    QString material;
    QString name;
    QString index;
    QString Abbe;
    k=0;

    for(int i=0; i<nof; i++){
        item = lines[i];
        comma = lines[i+1].indexOf(",");
        material = lines[i+1].trimmed();
        LF = lines[i+1].length();
        name = lines[i+1].mid(7,LF).trimmed();

        if (k==nol){
            table->setItem(k-1,3,new QTableWidgetItem("LAST SURFACE"));
            table->resizeColumnToContents(3);
            return;
        }

        if (material.left(3)=="AIR"){
            table->setItem(k,3,new QTableWidgetItem("AIR"));
            k++;
            continue;
        }

        if (material.left(5)=="NODUM"){
            material = lines[i+2].trimmed();
            continue;
        }

        if (material.left(5)=="IDEAL"){
            table->setItem(k,3,new QTableWidgetItem("IDEAL"));
            table->setItem(k,4,new QTableWidgetItem("1"));
            table->setItem(k,5,new QTableWidgetItem("0"));
            k++;
            continue;
        }


        if (material.left(4)=="REFL"){
            table->setItem(k,3,new QTableWidgetItem("REFL"));
            k++;
            continue;
        }


        if (material.left(3)=="GLA"){
            table->setItem(k,3,new QTableWidgetItem(material));
            table->setItem(k,4,new QTableWidgetItem("1.0000"));
            table->setItem(k,5,new QTableWidgetItem("0.0"));
            k++;
            continue;
        }

        if (material.left(4)=="MATL"){
            table->setItem(k,3,new QTableWidgetItem(material));
            table->setItem(k,4,new QTableWidgetItem(index.setNum(index.toDouble(),'f',4)));
            table->setItem(k,5,new QTableWidgetItem(Abbe.setNum(Abbe.toDouble(),'f',1)));
            k++;
            continue;
        }


        if (material.left(5)=="MODEL"){
            index = lines[i+1].mid(comma+4,20);
            Abbe =  lines[i+1].mid(comma+28,20);
            table->setItem(k,3,new QTableWidgetItem(material.left(15)));
            table->setItem(k,4,new QTableWidgetItem(index.setNum(index.toDouble(),'f',4)));
            table->setItem(k,5,new QTableWidgetItem(Abbe.setNum(Abbe.toDouble(),'f',1)));
            table->resizeColumnToContents(3);
            k++;
            continue;
        }

        if (material.left(3)=="SCH"){
            DataRead("SCHOTT.BIN",name,k);
            table->setItem(k,3,new QTableWidgetItem(material));
            table->resizeColumnToContents(3);
            k++;
            continue;
        }


        if (material.left(7)=="SCH2000"){
            DataRead("SCH2000.BIN",name,k);
            table->setItem(k,3,new QTableWidgetItem(material));
            table->resizeColumnToContents(3);
            k++;
            continue;
        }


        if (material.left(6)=="CHANCE"){
            DataRead("CHANCE.BIN",name,k);
            table->setItem(k,3,new QTableWidgetItem(material));
            table->resizeColumnToContents(3);
            k++;
            continue;
        }

        if (material.left(6)=="CORNIN"){
            DataRead("CORNIN.BIN",name,k);
            table->setItem(k,3,new QTableWidgetItem(material));
            table->resizeColumnToContents(3);
            k++;
            continue;
        }

        if (material.left(6)=="HIKARI"){
            DataRead("HIKARI.BIN",name,k);
            table->setItem(k,3,new QTableWidgetItem(material));
            table->resizeColumnToContents(3);
            k++;
            continue;
        }

        if (material.left(4)=="HOYA"){
            DataRead("HOYA.BIN",name,k);
            table->setItem(k,3,new QTableWidgetItem(material));
            table->resizeColumnToContents(3);
            k++;
            continue;
        }

        if (material.left(5)=="OHARA"){
            qDebug()<<material;
            DataRead("OHARA-O.BIN",name,k);
            table->setItem(k,3,new QTableWidgetItem(material));
            table->resizeColumnToContents(3);
            k++;
            continue;
        }


        if (material.left(5)=="OHARA"){
 //                       qDebug()<<material;
 //                       qDebug()<<name;
            DataRead("OHARA.BIN",name,k);
            table->setItem(k,3,new QTableWidgetItem(material));
            table->resizeColumnToContents(3);
            k++;
            continue;
        }


        if (material.left(7)=="RADHARD"){
            DataRead("RADHARD.BIN",name,k);
            table->setItem(k,3,new QTableWidgetItem(material));
            table->resizeColumnToContents(3);
            k++;
            continue;
        }

    }
}



QString MainWindow::surftypeCheck(QString iden, QString type, QString lines)
{
        QRegExp RegExp(iden);
        QString surftype;
        RegExp.setPatternSyntax(QRegExp::Wildcard); 
        if (RegExp.exactMatch(lines)){
            surftype = type;
        qDebug()<<surftype;
        } 
        return surftype;
}

// File Menu

void MainWindow::slot_actionNew()
{

    dialog5 = new NewDialog();
    dialog5->exec();
    if (dialog5->Button == 1024){

         qDebug()<< dialog5->LensIdentifier.toLatin1();
         qDebug()<< dialog5->units.toLatin1();
         qDebug()<< dialog5->say.toLatin1();
         qDebug()<< dialog5->scyfang.toLatin1();

         proc->write("\n");
         proc->write("LENS\n");
         proc->write("LI "+dialog5->LensIdentifier.toLatin1()+" \n");
         proc->write("UNITS "+dialog5->units.toLatin1()+" \n");
         proc->write("SAY "+dialog5->say.toLatin1()+" \n");
         proc->write("SCY FANG "+dialog5->scyfang.toLatin1()+" \n");
         proc->write("TH 1.0E20\n");
         proc->write("AIR\n");
         proc->write("REFS\n");
         proc->write("AIR\n");
         proc->write("TH 1.0E20\n");
         proc->write("AIR\n");
         proc->write("EOS\n");
         proc->write("RTG ALL\n");
         proc->write("LENSSAVE\n");

         delay();
         ReadFileToTable(ldir+"/LENSES/LENS.PRG");
    }
}

void MainWindow::slot_actionOpen()
{

    QFileDialog dlg;
    QString pathname = dlg.getOpenFileName(this,tr("Open Lens File"), ldir+"/LENSES",tr("LENS (*.PRG);;All Files (*)"));

        QFileInfo info1 (pathname);
        QString filename = info1.baseName();

    if (filename!=""){                                             // OPEN Button
        proc->write("\n");
        proc->write("LENSREST "+filename.toUtf8()+"\n");
        proc->write("RTG ALL\n");

        ReadFileToTable(pathname);

        proc->write("\n");
        proc->write("VIE XZ\n");
        return;
    }

        if (filename==""){                                              //CANCEL Button
        return;
    }
}



void MainWindow::slot_actionSave()
{

    QString pathname = QFileDialog::getSaveFileName(this,tr("Save Lens File"), ldir+"/LENSES",tr("LENS (*.PRG);;All Files (*)"));
    QFileInfo info1 (pathname);
    QString filename = info1.baseName();

    if (filename!=""){
        proc->write("\n");
        proc->write("LENSSAVE "+filename.toUtf8()+"\n");
        return;
    }

    if (filename==""){                                              //CANCEL Button
        return;
    }

}

void MainWindow::slot_actionImport_Zemax()
{

    QString pathname = QFileDialog::getOpenFileName(this,tr("Import ZEMAX File"), ldir+"/LENSES",tr("ZEMAX (*.ZMX);;All Files (*)"));
    QFileInfo info1 (pathname);
    QString filename = info1.fileName();

    if (filename!=""){
        proc->write("\n");
        proc->write("ZMX2PRG "+filename.toUtf8()+"\n");
        proc->write("LENSSAVE\n");
        proc->write("RTG ALL\n");
        delay();
        ReadFileToTable(ldir+"/LENSES/LENS.PRG");
        proc->write("VIE XZ\n");
        return;
    }

    if (filename==""){                                              //CANCEL Button
        return;
    }

}


void MainWindow::slot_actionImport_CODE_V()
{

    QString pathname = QFileDialog::getOpenFileName(this,tr("Import Code-V File"), ldir+"/LENSES",tr("Code-V (*.SEQ);;All Files (*)"));
    QFileInfo info1 (pathname);
    QString filename = info1.fileName();

    if (filename!=""){
        proc->write("\n");
        proc->write("CV2PRG "+filename.toUtf8()+"\n");
        proc->write("LENSSAVE\n");
        proc->write("RTG ALL\n");
        delay();
        ReadFileToTable(ldir+"/LENSES/LENS.PRG");
        proc->write("VIE XZ\n");
        return;
    }

    if (filename==""){                                              //CANCEL Button
        return;
    }

}


void MainWindow::slot_actionExport_Zemax()
{

    QString pathname = QFileDialog::getSaveFileName(this,tr("Export ZEMAX File"), ldir+"/LENSES",tr("ZEMAX (*.ZMX);;All Files (*)"));
    QFileInfo info1 (pathname);
    QString filename = info1.fileName();

    if (filename!=""){
        proc->write("\n");
        proc->write("OUT FILE "+filename.toUtf8()+"\nLENO ZMX\nOUT TP\n");
    return;
    }

    if (filename==""){                                              //CANCEL Button
        return;
    }

}

void MainWindow::slot_actionExport_CODE_V()
{

    QString pathname = QFileDialog::getSaveFileName(this,tr("Export Code-V File"), ldir+"/LENSES",tr("Code-V (*.SEQ);;All Files (*)"));
    QFileInfo info1 (pathname);
    QString filename = info1.fileName();

    if (filename!=""){
        proc->write("\n");
        proc->write("OUT FILE "+filename.toUtf8()+"\nLENO CV\nOUT TP\n");
    return;
    }

    if (filename==""){                                              //CANCEL Button
        return;
    }

}

void MainWindow::slot_actionExport_JPEG()
{

    proc->write("GRAOUT JPG\n");
    return;

}

void MainWindow::slot_actionExport_EPS()
{

    proc->write("GRAOUT COLEPS\n");
    return;

}

void MainWindow::slot_actionExport_PDF()
{

    proc->write("GRAOUT PDF\n");
    return;

}


//Edit Menu

void MainWindow::slot_actionInsert_surface()
{
    proc->write("\n");

    int row;

    row = table->currentRow();              //get number of row

    if (row < 0){
        row=1;
    }

    if (row != 0){
        table->insertRow(row);              //insert row to GUI table
        table->setItem(row,0,new QTableWidgetItem(""));
        table->setItem(row,1,new QTableWidgetItem("inf"));
        table->setItem(row,2,new QTableWidgetItem("0"));
        table->setItem(row,3,new QTableWidgetItem("AIR"));
        tableitem=table->item(row,3);
        ccv.insert(row,1," ");
        asphv.insert(row,1," ");
        asph2v.insert(row,1," ");
        tiltv.insert(row,1," ");
//        tableitem->setFlags(Qt::ItemIsEnabled);
        nol++;
    }

    QStringList label;
    for (int i=0; i<=nol-1; i++){
        label << QString::number(i);        //set table vetical label start to 0
    }

    table->setRowCount( nol );              //number of row = nol
    table->setVerticalHeaderLabels(label);

    QString Qrow = QString::number(row);    //get number of insert surface
    QString cmd = "INS "+Qrow+"\n";
    proc->write("\n");
    proc->write("U L\n");                   //move to ULN mode
    proc->write(cmd.toLatin1());
    proc->write("EOS\n");                   //return to CMD mode
    proc->write("RTG ALL\n");
}


void MainWindow::slot_actionDelete_surface()
{
    int row;
    row = table->currentRow();              //get number of row

    if (row < 0) return;

    proc->write("\n");

    if ((row != 0)&&(row != nol-1)){
        table->removeRow(row);              //delete row from GUI table
        nol--;
        ccv.remove(row,1);
        asphv.remove(row,1);
        asph2v.remove(row,1);
        tiltv.remove(row,1);
    }
    QStringList label;
    for (int i=0; i<=nol-1; i++){
        label << QString::number(i);        //set table vetical label start to 0
    }

    table->setRowCount( nol );              //number of row = nol
    table->setVerticalHeaderLabels(label);


    QString Qrow = QString::number(row);    //get number of delete surface
    QString cmd = "DEL "+Qrow+"\n";
    proc->write("\n");
    proc->write("U L\n");                   //move to ULN mode
    proc->write(cmd.toLatin1());
    proc->write("EOS\n");                   //return to CMD mode
    proc->write("RTG ALL\n");
}


void MainWindow::slot_actionDisplayLensData()
{
    proc->write("\n");
    proc->write("LIS\n");
}


//View Menu

void MainWindow::slot_actionXZ()
{
    proc->write("\n");
    proc->write("VIE XZ\n");
}

void MainWindow::slot_actionOrtho()
{
    proc->write("\n");
    proc->write("VIE ORTHO\n");
}

void MainWindow::slot_focus()
{
    QString cmd;
    QString opr = "CHG ";               //set edit surface
    QString num = QString::number(nol-2);
    QString lf = "\n";
    const char *input_cmd;
    qDebug()<< num;
    cmd = "";
    cmd = opr + num + lf;               //generate command
    input_cmd = cmd.toLatin1();

    proc->write("U L\n");
    proc->write(input_cmd);
    proc->write("PY\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
    proc->write("VIE\n");

}


//Graph Menu

void MainWindow::slot_actionXFAN()
{
    proc->write("\n");
    proc->write("FANS XFAN\n");
}

void MainWindow::slot_actionYFAN()
{
    proc->write("\n");
    proc->write("FANS YFAN\n");
}

void MainWindow::slot_actionXYFAN()
{
    proc->write("\n");
    proc->write("FANS XYFAN\n");
}

void MainWindow::slot_actionYXFAN()
{
    proc->write("\n");
    proc->write("FANS YXFAN\n");
}

void MainWindow::slot_actionNFAN()
{
    proc->write("\n");
    proc->write("FANS NFAN\n");
}

void MainWindow::slot_actionPFAN()
{
    proc->write("\n");
    proc->write("FANS PFAN\n");
}

void MainWindow::slot_actionXOPD()
{
    proc->write("\n");
    proc->write("FANS XOPD\n");
}

void MainWindow::slot_actionYOPD()
{
    proc->write("\n");
    proc->write("FANS YOPD\n");
}

void MainWindow::slot_actionXYOPD()
{
    proc->write("\n");
    proc->write("FANS XYOPD\n");
}

void MainWindow::slot_actionNOPD()
{
    proc->write("\n");
    proc->write("FANS NOPD\n");
}

void MainWindow::slot_actionPOPD()
{
    proc->write("\n");
    proc->write("FANS POPD\n");
}

void MainWindow::slot_actionXCD()
{
    proc->write("\n");
    proc->write("FANS XCD\n");
}

void MainWindow::slot_actionYCD()
{
    proc->write("\n");
    proc->write("FANS YCD\n");
}

void MainWindow::slot_actionXYCD()
{
    proc->write("\n");
    proc->write("FANS XYCD\n");
}

void MainWindow::slot_actionYXCD()
{
    proc->write("\n");
    proc->write("FANS YXCD\n");
}

void MainWindow::slot_actionNCD()
{
    proc->write("\n");
    proc->write("FANS NCD\n");
}

void MainWindow::slot_actionPCD()
{
    proc->write("\n");
    proc->write("FANS PCD\n");
}

void MainWindow::slot_actionXLA()
{
    proc->write("\n");
    proc->write("FANS XLA\n");
}

void MainWindow::slot_actionYLA()
{
    proc->write("\n");
    proc->write("FANS YLA\n");
}

void MainWindow::slot_actionXYLA()
{
    proc->write("\n");
    proc->write("FANS XYLA\n");
}

void MainWindow::slot_actionYXLA()
{
    proc->write("\n");
    proc->write("FANS YXLA\n");
}

void MainWindow::slot_actionNLA()
{
    proc->write("\n");
    proc->write("FANS NLA\n");
}

void MainWindow::slot_actionPLA()
{
    proc->write("\n");
    proc->write("FANS PLA\n");
}

void MainWindow::slot_actionDistortion()
{
    proc->write("\n");
    proc->write("DIST\nPLTDIST\n");
}

void MainWindow::slot_actionField_Curvature()
{
    proc->write("\n");
    proc->write("FLDCV\nPLTFLDCV\n");
}

void MainWindow::slot_actionAstigmatism()
{
    proc->write("\n");
    proc->write("AST\nPLTAST\n");
}

void MainWindow::slot_actionPltchrsh()
{
    proc->write("\n");
    proc->write("CHRSHIFT\nPLTCHRSH\n");
}


void MainWindow::slot_actionGeometical()
{
    proc->write("\n");
    proc->write("GOTF\nPLTGOTF\n");
}

void MainWindow::slot_actionGeometical_Leica()
{
    proc->write("\n");
    proc->write("GOTF\nPLTGOTF LEICA\n");
}

void MainWindow::slot_actionDiffraction()
{
    proc->write("\n");
    proc->write("DOTF\nPLTDOTF\n");
}

void MainWindow::slot_actionDiffraction_Leica()
{
    proc->write("\n");
    proc->write("DOTF\nPLTDOTF LEICA\n");
}

void MainWindow::slot_actionSpot_Diagram()
{
    proc->write("\n");
    proc->write("SPD\nPLTSPD\n");
}

void MainWindow::slot_actionWavefront_Phase()
{
    proc->write("\n");
    proc->write("CAPFN\nPLOT CAPFNOPD\n");
}

void MainWindow::slot_actionWavefront_Intensity()
{
    proc->write("\n");
    proc->write("CAPFN\nPLOT CAPFNAPD\n");
}

void MainWindow::slot_actionPoint_Spread_Function()
{
    proc->write("\n");
    proc->write("PSF\n");
}

// Input table value

void MainWindow::slot_action_value_entered()
{
    proc->write("\n");

    row = table->currentRow();          //get number of row
    column = table->currentColumn();    //get number of column

    if (row==0) return;

    QString cmd;
    QString opr = "CHG ";               //set edit surface
    QString num = QString::number(row);
    QString lf = "\n";

    const char *input_cmd;


    cmd = "";
    cmd = opr + num + lf;               //generate command
    input_cmd = cmd.toLatin1();

    proc->write("U L\n");
    proc->write(input_cmd);             //input command (edit surface)

    switch (column){
        case 1:
            opr="RD ";                  //edit radius
            break;
        case 2:
            opr="TH ";                  //edit thickness
            break;
        case 3:                         //edit glass data
            dialog = new nkDialog();
            dialog->exec();

            if (dialog->Button1 == 1024){

            proc->write("U L\n");
            proc->write("CHG 2\n");
            proc->write("MODEL "+dialog->GlassName.toLatin1()+","+dialog->Index.toLatin1()+","+dialog->Abbe.toLatin1()+",\n");
            proc->write("EOS\n");
            proc->write("RTG ALL\n");

            table->setItem(row, 3, new QTableWidgetItem(dialog->GlassName.toUpper()));  //Change table value to MODEL
            table->setItem(row, 4, new QTableWidgetItem(dialog->Index.trimmed()));
            table->setItem(row, 5, new QTableWidgetItem(dialog->Abbe.trimmed()));
            break;
            }
        case 4:
            break;
        case 5:
            break;
        case 6:
            opr="CLAP ";                  //edit aperture radius
        break;

    }

    num = table->item(row,column)->text();  //get input value

    cmd = "";
    cmd = opr + num + lf;
    input_cmd = cmd.toLatin1();

    proc->write(input_cmd);
    proc->write("EOS\n");                   //exit LENS INPUT mode (Return to CMD mode)
    proc->write("RTG ALL\n");

}


//Input Model menu

void MainWindow::slot_actionModeldialog()
{

    int row;
    row = table->currentRow();              //get number of row
    QString Qrow = QString::number(row);    //get number of modify surface

    dialog = new nkDialog();
//    dialog->setWindowState(dialog->windowState() | Qt::WindowMaximized); //for android
    dialog->exec();

    if (dialog->Button1 == 1024){

    proc->write("\n");
    proc->write("U L\n");
    proc->write("CHG "+Qrow.toLatin1()+"\n");
    proc->write("MODEL "+dialog->GlassName.toLatin1()+","+dialog->Index.toLatin1()+","+dialog->Abbe.toLatin1()+",\n");
    proc->write("EOS\n");
    proc->write("RTG ALL\n");
    proc->write("FINDGLASS "+Qrow.toLatin1()+"\n");

    table->setItem(Qrow.toInt(), 3, new QTableWidgetItem("MODEL "+dialog->GlassName.toUpper()));  //Change table value to MODEL
    tableitem=table->item(row,3);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    table->setItem(Qrow.toInt(), 4, new QTableWidgetItem(dialog->Index.trimmed()));
    tableitem=table->item(row,4);
//    tableitem->setFlags(Qt::ItemIsEnabled);
    table->setItem(Qrow.toInt(), 5, new QTableWidgetItem(dialog->Abbe.trimmed()));
    tableitem=table->item(row,5);
//    tableitem->setFlags(Qt::ItemIsEnabled);

    }

}


void MainWindow::slot_actionInput_Variables()
{

    dialog2 = new Optimize();
 //   dialog2->setWindowState(dialog2->windowState() | Qt::WindowMaximized); //for android
    dialog2->exec();

    if (dialog2->Button2 == 1024){

    proc->write("\n");
    proc->write("MERIT\n");
    proc->write("FLCLTH "+dialog2->FocalLength.toLatin1()+" 1 0 "+QString::number(nol-1).toLatin1()+"\n");
    proc->write(dialog2->Target.toLatin1()+" 0.0 1\n");
    proc->write("EOS\n");

    proc->write("VARIABLES\n");
    proc->write(dialog2->Parameter1.toLatin1()+" "+dialog2->ParamValue1.toLatin1()+"\n");
    proc->write(dialog2->Parameter2.toLatin1()+" "+dialog2->ParamValue2.toLatin1()+"\n");
    proc->write(dialog2->Parameter3.toLatin1()+" "+dialog2->ParamValue3.toLatin1()+"\n");
    proc->write(dialog2->Parameter4.toLatin1()+" "+dialog2->ParamValue4.toLatin1()+"\n");
    proc->write(dialog2->Parameter5.toLatin1()+" "+dialog2->ParamValue5.toLatin1()+"\n");
    proc->write("EOS\n");

    proc->write("VB\n");
    proc->write("OPRD\n");

    proc->write("ITER FULL\n");
    proc->write("RTG ALL\n");

    }
}


void MainWindow::slot_actionInput_LensIdentifier()
{

    dialog3 = new LIDialog();
//    dialog3->setWindowState(dialog3->windowState() | Qt::WindowMaximized); //for android
    dialog3->exec();

    if (dialog3->Button3 == 1024){

    proc->write("\n");
    proc->write("U L\n");
    proc->write("LI "+dialog3->LensIdentifier.toLatin1()+" \n");
    proc->write("EOS\n");

    proc->write("LI\n");

    }
}

void MainWindow::slot_actionRay_input_angle()
{

    dialog4 = new rayinputDialog();
//    dialog4->setWindowState(dialog4->windowState() | Qt::WindowMaximized); //for android
    dialog4->exec();

    if (dialog4->Button4 == 1024){

    proc->write("\n");
    proc->write("U L\n");
    proc->write("SCY FANG "+dialog4->inputangle.toLatin1()+"\n");
    proc->write("EOS\n");

    proc->write("VIE\n");

    }
}

void MainWindow::leaveEvent(QEvent * event){    //Release input when event happend
//       qDebug() << "Leave event";
       cmdLine->releaseKeyboard();
       QWidget::leaveEvent(event);
}

void MainWindow::enterEvent(QEvent * event){    //Grab input when event finished
//       qDebug() << "Enter event";
//       cmdLine->grabKeyboard();
       QWidget::enterEvent(event);
}


bool MainWindow::eventFilter(QObject* object, QEvent* event)
{

    if (object==table)
    {
        if (event->type()==QEvent::KeyRelease)
        {
        // key pressed
        // transforms QEvent into QKeyEvent
        QKeyEvent* pKeyEvent=static_cast<QKeyEvent*>(event);
        switch(pKeyEvent->key())
        {
            case Qt::Key_Return:
            {
                // Return key pressed - record update

//                qDebug() << "Return key pressed";
                slot_action_value_entered();
                break;
            }
            case Qt::Key_Enter:
            {
               // Enter key pressed - record update

//                qDebug() << "Enter key pressed";
                slot_action_value_entered();
                break;
            }
        }
    }
    }

    if (object == cmdLine){
        if (event->type()==QEvent::KeyRelease)
        {
            // key pressed
            // transforms QEvent into QKeyEvent
            QKeyEvent* pKeyEvent=static_cast<QKeyEvent*>(event);
            switch(pKeyEvent->key())
            {
                case Qt::Key_Up:
                {
                    // Up arrow key pressed - record update

//                    qDebug() << "Up arrow key pressed";
                    curhist--;
                    if (curhist < 0) curhist=histnum-1;
                    history(curhist);
                    cmdLine->setText(DispHistory);
                    break;
                }
                case Qt::Key_Down:
                {
                   // Down arrow key pressed - record update

//                    qDebug() << "Down arrow key pressed";
                    curhist++;
                    if (curhist >= histnum) curhist=0;
                    history(curhist);
                    cmdLine->setText(DispHistory);
                    break;

                }

                case Qt::Key_Return:
                {
                   // Enter key pressed - record update

 //                   qDebug() << "Return key pressed";

                QFile HistData(ldir+"/HISTORY.DAT");                 //set data file
                HistData.open(QIODevice::WriteOnly|QIODevice::Append);

                QTextStream out(&HistData);
                if ((cmdLine->text().toUtf8()) == "") break;
                out << cmdLine->text()+"\n";
                HistData.close();

                    histnum++;
                    curhist++;
                    break;
                 }

            }
        }

    }
        return QWidget::eventFilter(object, event);
}

void MainWindow::delay()  //Code from StackOverFlow
{
    QTime dieTime= QTime::currentTime().addMSecs(500);
    while( QTime::currentTime() < dieTime )
    QCoreApplication::processEvents(QEventLoop::AllEvents, 100);
}

void MainWindow::history(int curhist)
{

    QFile HistData(ldir+"/HISTORY.DAT");                 //set data file

    HistData.open(QIODevice::ReadOnly);                    //file open
    QTextStream input(&HistData);

    for (int i=0; i<=curhist; i++)
    {
        DispHistory = input.readLine(0);    // read data
//        qDebug() << DispHistory;
//        qDebug() << histnum;
    }

    HistData.close();

}

void MainWindow::slot_quit2()
{
    proc->write("EXIT\n");
    delay();
    QFile::remove(ldir+"/HISTORY.DAT");
    QApplication::quit();
}
