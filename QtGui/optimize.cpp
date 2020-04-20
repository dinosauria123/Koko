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

#include "optimize.h"
#include "ui_optimize.h"

Optimize::Optimize(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::Optimize)
{
    ui->setupUi(this);
    connect( ui->buttonBox, SIGNAL(accepted()), this, SLOT(onAccept()) );
    connect( ui->buttonBox, SIGNAL(rejected()), this, SLOT(onReject()) );
}

void Optimize::onAccept()
{

    FocalLength = ui->lineEdit->text();
    Target = ui->comboBox_6->currentText();

    Parameter1 = ui->comboBox->currentText();
    Parameter2 = ui->comboBox_2->currentText();
    Parameter3 = ui->comboBox_3->currentText();
    Parameter4 = ui->comboBox_4->currentText();
    Parameter5 = ui->comboBox_5->currentText();

    ParamValue1 = ui->lineEdit_2->text();
    ParamValue2 = ui->lineEdit_3->text();
    ParamValue3 = ui->lineEdit_4->text();
    ParamValue4 = ui->lineEdit_5->text();
    ParamValue5 = ui->lineEdit_6->text();

    Button2 = ui->buttonBox->Ok;

}

void Optimize::onReject()
{
  reject();
}


Optimize::~Optimize()
{
    delete ui;
}
