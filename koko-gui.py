#!/usr/bin/env python3
"""
Koko Optical Design Software GUI launcher.

This launches the PyQt6 front end which drives the koko-cli back end.
"""

import os
import sys

# make sure the gui_py package (next to this script) is importable
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from gui_py.mainwindow import KokoMainWindow
from PyQt6.QtWidgets import QApplication


def main():
    app = QApplication(sys.argv)
    window = KokoMainWindow()
    window.show()
    sys.exit(app.exec())


if __name__ == '__main__':
    main()
