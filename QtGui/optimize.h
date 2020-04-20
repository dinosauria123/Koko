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

#ifndef OPTIMIZE_H
#define OPTIMIZE_H

#include <QDialog>
#include "ui_optimize.h"

namespace Ui {
class Optimize;
}

class Optimize : public QDialog
{
    Q_OBJECT

public:
    explicit Optimize(QWidget *parent = 0);
    ~Optimize();
    QString FocalLength;
    QString Target;
    QString Parameter1;
    QString Parameter2;
    QString Parameter3;
    QString Parameter4;
    QString Parameter5;
    QString ParamValue1;
    QString ParamValue2;
    QString ParamValue3;
    QString ParamValue4;
    QString ParamValue5;

    int Button2;

private:
    Ui::Optimize *ui;

private slots:

    void onAccept();
    void onReject();

};

#endif // OPTIMIZE_H
