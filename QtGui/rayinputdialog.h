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

#ifndef RAYINPUTDIALOG_H
#define RAYINPUTDIALOG_H

#include <QDialog>

namespace Ui {
class rayinputDialog;
}

class rayinputDialog : public QDialog
{
    Q_OBJECT

public:
    explicit rayinputDialog(QWidget *parent = 0);
    ~rayinputDialog();

    Ui::rayinputDialog *ui;

    QString inputangle;

    int Button4;


private:


private slots:
    void onAccept();
    void onReject();

};

#endif // RAYINPUTDIALOG_H
