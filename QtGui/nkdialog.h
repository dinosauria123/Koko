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

#ifndef NKDIALOG_H
#define NKDIALOG_H

#include <QDialog>
#include <QProcess>
#include "ui_nkdialog.h"


class nkDialog : public QDialog, public Ui::nkDialog
{
    Q_OBJECT

public:
    explicit nkDialog(QWidget *parent = 0);

    QString GlassName;
    QString Index;
    QString Abbe;

    int Button1;

    Ui::nkDialog *ui;

    ~nkDialog();

private:

private slots:
    void onAccept();
    void onReject();

};

#endif // NKDIALOG_H
