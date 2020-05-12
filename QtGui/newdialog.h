#ifndef NEWDIALOG_H
#define NEWDIALOG_H

#include <QDialog>

namespace Ui {
class NewDialog;
}

class NewDialog : public QDialog
{
    Q_OBJECT

public:
    explicit NewDialog(QWidget *parent = nullptr);
    ~NewDialog();
    QString LensIdentifier,units,say,scyfang;
    int Button;

private:
    Ui::NewDialog *ui;

private slots:
    void onAccept();
    void onReject();
    void on_radioButton_clicked();
    void on_radioButton_2_clicked();
};

#endif // NEWDIALOG_H
