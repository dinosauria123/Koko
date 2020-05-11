#-------------------------------------------------
#
# Project created by QtCreator 2014-08-07T16:07:35
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = KODS_GUI
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    newdialog.cpp \
    nkdialog.cpp \
    optimize.cpp \
    lidialog.cpp \
    rayinputdialog.cpp

HEADERS  += mainwindow.h \
    newdialog.h \
    nkdialog.h \
    optimize.h \
    lidialog.h \
    rayinputdialog.h

FORMS    += mainwindow.ui \
    newdialog.ui \
    nkdialog.ui \
    optimize.ui \
    lidialog.ui \
    rayinputdialog.ui
