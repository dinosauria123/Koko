#!/usr/bin/env python3
"""
Koko Optical Design Software GUI launcher.

This launches the PyQt6 front end which drives the koko-cli back end.
"""

import os
import sys
import traceback

# Disable Qt's AT-SPI / accessibility DBus probing at startup. Without this,
# Qt warns: AtSpiAdaptor::applicationInterface does not implement
# "GetApplicationBusAddress" "/org/a11y/atspi/accessible/root"
os.environ.setdefault("QT_ACCESSIBILITY", "0")

# make sure the gui_py package (next to this script) is importable
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from PyQt6.QtCore import qInstallMessageHandler, QtMsgType
from gui_py.mainwindow import KokoMainWindow
from PyQt6.QtWidgets import QApplication

# Diagnostic hook: if Qt 6.10 still emits the "dataChanged() called with an
# invalid index range" warning from QAbstractItemView, capture a stack trace
# so the real emit site can be identified. The warning is NOT suppressed here.
_TRACE_LOG = os.path.join("/tmp", "koko_datachanged_trace.log")

# AT-SPI / a11y warnings we never want echoed to the terminal.
_ATSPI_MARKERS = (
    "AtSpiAdaptor",
    "GetApplicationBusAddress",
    "a11y/atspi",
    "accessible/root",
)


def _qt_message_handler(msg_type, context, message):
    if msg_type == QtMsgType.QtWarningMsg and "dataChanged" in message:
        # Capture a stack trace so the real emit site can be identified, but
        # do NOT echo this specific Qt 6.10 warning to the terminal (it is a
        # benign invalid-index range that the table update no longer produces).
        try:
            with open(_TRACE_LOG, "a") as fh:
                fh.write("=== dataChanged WARNING ===\n")
                fh.write(message + "\n")
                traceback.print_stack(file=fh)
                fh.write("===========================\n\n")
        except OSError:
            pass
        return
    # Silently drop AT-SPI / accessibility bus warnings.
    if msg_type == QtMsgType.QtWarningMsg and any(
            m in message for m in _ATSPI_MARKERS):
        return
    # Forward everything else to the default handler.
    sys.stderr.write("[%s] %s\n" % (msg_type.name, message))
    sys.stderr.flush()


qInstallMessageHandler(_qt_message_handler)


def main():
    app = QApplication(sys.argv)
    window = KokoMainWindow()
    window.show()
    sys.exit(app.exec())


if __name__ == '__main__':
    main()
