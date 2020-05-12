#include "newdialog.h"
#include "ui_newdialog.h"

NewDialog::NewDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::NewDialog)
{
    ui->setupUi(this);
    connect( ui->buttonBox, SIGNAL(accepted()), this, SLOT(onAccept()) );
    connect( ui->buttonBox, SIGNAL(rejected()), this, SLOT(onReject()) );
}

void NewDialog::onAccept()
{

    LensIdentifier = ui->lineEdit->text();
    say = ui->lineEdit_2->text();
    scyfang = ui->lineEdit_3->text();
    Button = ui->buttonBox->Ok;

}

void NewDialog::onReject()
{
    reject();
}

NewDialog::~NewDialog()
{
    delete ui;
}

void NewDialog::on_radioButton_clicked()
{
    units = "MM";
}

void NewDialog::on_radioButton_2_clicked()
{
    units = "IN";
}
