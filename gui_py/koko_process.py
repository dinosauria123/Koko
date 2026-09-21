"""KokoProcessMixin: koko_process.py mixin (see gui_py/mainwindow.py).
"""
import os
import re
import math
import subprocess
import struct
import sys

from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QMessageBox, QFileDialog, QTableWidgetItem,
    QDialog, QLabel, QVBoxLayout, QHBoxLayout, QLineEdit, QPushButton,
    QComboBox, QDialogButtonBox, QInputDialog, QMenu, QWidget, QFrame,
    QSizePolicy, QStyledItemDelegate, QCheckBox, QListWidget,
    QListWidgetItem,
)
from PyQt6.QtCore import QProcess, Qt, QTimer, QByteArray, QSize, QEvent, QPointF
from PyQt6.QtGui import QFont, QPixmap, QImage, QPalette, QColor, QBrush


# Commands that make koko write a plot script (drawcmd.gpl). Any of these,
# whether launched from the Plot menu or typed in the command line, should
# trigger an automatic render of the graph in the GUI.
# NOTE: PLTIMG is intentionally excluded - it writes PLOTBMP.BMP directly
# via IGrSaveImageData, not via gnuplot. The ImageBlur dialog handles it
# by polling for PLOTBMP.BMP in _schedule_image_render().


from gui_py._koko_gui_common import (
    PLOT_TRIGGER_PREFIXES
)

class KokoProcessMixin:
    def _capture_rtg(self, text):
        """Orchestrate RTG output parsing."""
        if not hasattr(self, '_rtg_buf'):
            self._rtg_buf = None
        if not hasattr(self, '_koko_idle'):
            self._koko_idle = True
        
        self._check_prompt_idle(text)
        self._parse_rtg_meta(text)
        
        if 'BASIC LENS DATA' in text:
            self._rtg_buf = ''
        if self._rtg_buf is not None:
            self._rtg_buf += text
            self._parse_rtg_surface_details(text)
            if 'LAST SURFACE' in text or 'NO SURFACES' in text:
                self._on_rtg_complete(self._rtg_buf)
                self._rtg_buf = None


    def _check_prompt_idle(self, text):
        """Detect koko's 'N:cmd>' prompt after stripping ANSI escapes."""
        _clean = re.sub(r'\x1b\[[0-9;]*[A-Za-z]', '', text)
        if re.search(r'\d+:\s*cmd>\s*$', _clean.rstrip()):
            self._koko_idle = True


    def _kill_koko(self):
        import os, signal
        if getattr(self, "_koko_poll", None) is not None:
            self._koko_poll.stop()
            self._koko_poll = None
        if self._koko_notifier is not None:
            self._koko_notifier.setEnabled(False)
            self._koko_notifier = None
        if self._koko_fd is not None:
            try:
                os.close(self._koko_fd)
            except OSError:
                pass
            self._koko_fd = None
        if self._koko_pid is not None:
            try:
                os.kill(self._koko_pid, signal.SIGTERM)
                os.waitpid(self._koko_pid, 0)
            except (OSError, ChildProcessError):
                pass
            self._koko_pid = None


    def _launch_koko_process(self):
        """Start koko-cli inside a real PTY and return (pid, fd).
        
        Returns None on failure. On success sets self._koko_pid and
        self._koko_fd. The child is launched in its own session so it
        can be killed by group later if needed.
        """
        import pty, os, fcntl, struct, signal, termios

        master, slave = pty.openpty()
        # non-blocking master so the GUI event loop is never stalled
        fl = fcntl.fcntl(master, fcntl.F_GETFL)
        fcntl.fcntl(master, fcntl.F_SETFL, fl | os.O_NONBLOCK)
        # CRITICAL: set a window size on the slave pty. Without this koko
        # (Fortran + linenoise) stays silent and emits nothing.
        winsize = struct.pack("HHHH", 24, 80, 0, 0)
        fcntl.ioctl(slave, termios.TIOCSWINSZ, winsize)

        try:
            proc = subprocess.Popen(
                [self.koko_path, '-G'],
                stdin=slave, stdout=slave, stderr=slave,
                start_new_session=True, close_fds=True,
            )
        except Exception as _e:
            QMessageBox.critical(self, "Error", "Failed to start koko-cli: %s" % _e)
            os.close(master)
            os.close(slave)
            return False
        os.close(slave)

        self._koko_pid = proc.pid
        self._koko_fd = master
        self._sent_cmds = []  # strip echo-back from koko output
        
        # Poll the pty master on a timer
        self._koko_poll = QTimer(self)
        self._koko_poll.timeout.connect(self._poll_koko_pty)
        self._koko_poll.start(80)
        
        # Reap the child if it dies
        self._koko_watch = QTimer(self)
        self._koko_watch.timeout.connect(lambda: self._reap_koko())
        self._koko_watch.start(1000)
        
        return True


    def _on_rtg_complete(self, buf):
        """Dispatch populated buffer to table and schedule plot render."""
        self.populate_table(buf)
        if self._pending_vie:
            self._pending_vie = False
            self.send_koko("VIE XZ")
            self._schedule_plot_render()


    def _parse_rtg_meta(self, text):
        """Scan for lens metadata: LI, WV, UNITS, LENS RESTORED."""
        for line in text.splitlines():
            stripped = line.strip()
            
            m_li = re.match(r'(?i)^LI\s*,?\s*(.+)$', stripped)
            if m_li:
                self._li = m_li.group(1).strip()
                continue
            
            m_restored = re.search(
                r'LENS SAVED AS:\s*(\S+?)\.PRG\s+HAS BEEN RESTORED',
                stripped, re.IGNORECASE)
            if m_restored:
                name = m_restored.group(1)
                self._li = name
                for cand in (
                    os.path.join(self.HOME, 'LENSES', name + '.PRG'),
                    os.path.join(self.HOME, 'LENSES', name + '.prg'),
                    os.path.join(self.HOME, 'LENSES', name + '.koko'),
                    name + '.PRG', name + '.prg',
                ):
                    if os.path.exists(cand):
                        self._read_lens_file_meta(cand)
                        break
                self._rtg_buf = None
                self.send_koko("RTG ALL")
                continue
            
            if self._rtg_buf is not None and (stripped.startswith('WV')
                   or re.match(r'(?i)^WV\s', stripped)):
                nums = re.findall(r'[\d.]+', stripped)
                if len(nums) >= 3:
                    try:
                        self._lD = float(nums[0])
                        self._lF = float(nums[1])
                        self._lC = float(nums[2])
                    except ValueError:
                        pass
                continue
            
            m_units = re.match(r'(?i)^UNITS\s+(.+)$', stripped)
            if m_units:
                self._units = m_units.group(1).strip().lower()


    def _parse_rtg_surface_details(self, text):
        """Extract per-surface markers: CC, ASPH, TILT, CLAP."""
        for line in text.splitlines():
            stripped = line.strip()
            
            m_surf = re.match(r'^(\d+)\s*\*\s*(CC|ASPH2?|TILT|REFS|ASTOP)\b(.*)',
                              stripped, re.IGNORECASE)
            if m_surf:
                s = int(m_surf.group(1))
                kind = m_surf.group(2).upper()
                rest = m_surf.group(3).strip()
                store_map = {'CC': self._ccv, 'ASPH2': self._asph2v,
                             'ASPH': self._asphv, 'TILT': self._tiltv}
                if kind in store_map:
                    store_map[kind][s] = rest if rest else line.strip()
                elif kind == 'REFS':
                    self._ccv[s] = 'REFS'
                elif kind == 'ASTOP':
                    self._ccv[s] = 'ASTOP'
                continue
            
            for pat, attr in [(r'(?i)^CC\s+(.+)$', '_ccv'),
                              (r'(?i)^ASPH\s+(.+)$', '_asphv'),
                              (r'(?i)^TILT\s+(.+)$', '_tiltv')]:
                m = re.match(pat, stripped)
                if m:
                    src = getattr(self, attr, {})
                    for s in list(src.keys()):
                        if s not in src or not src[s]:
                            src[s] = m.group(1).strip()
                            break
                    break


    def _poll_koko_pty(self):
        """Read whatever koko wrote to the PTY master and feed it to the
        RTG parser. Called on a timer so we never depend on QSocketNotifier
        quirks. Non-blocking read so we never stall the event loop.
        Accumulates partial lines so a BASIC LENS DATA block split across
        chunks is reassembled before _capture_rtg parses it."""
        if self._koko_fd is None:
            return
        try:
            data = os.read(self._koko_fd, 65536)
        except (OSError, BlockingIOError):
            return
        if not data:
            return
        text = data.decode('utf-8', errors='replace')
        # Strip ANSI escape sequences (color codes AND linenoise cursor
        # movements like ESC[9G / ESC[J) so the GUI terminal shows plain
        # text and the RTG parser never sees raw escape bytes.
        text = re.sub(r'\x1b\[[0-9;?]*[A-Za-z]', '', text)
        # Remove the command echo that koko (linenoise raw mode) writes back
        # into its output: the typed command appears twice (once as the
        # keystroke is drawn, once on enter) even with ECHO OFF. The GUI
        # already prints "> CMD" in send_koko, so drop these echoes here to
        # avoid the command being shown 2-3 times.
        #
        # IMPORTANT: never strip a sent command from an RTG ALL data row.
        # A data row such as " 2 ... RADHARD BK7G18 ..." (or a marker row like
        # " 1* ... RADHARD BK7G18 ...") contains the exact string we just sent
        # ("RADHARD BK7G18"); a blanket replace() would delete the glass name
        # from the built-in terminal, leaving the index and V-number orphaned.
        # We therefore keep every RTG surface-data row verbatim (any line that
        # starts with a surface number, with or without a "*" marker) and only
        # strip echoes that sit on a koko prompt line (e.g. " 3:uln> RADHARD
        # BK7G18"), so the glass name stays visible.
        out_lines = []
        for line in text.split('\n'):
            if ((re.match(r'^\s*\d+', line)
                 and not re.search(r':(?:cmd|uln)>', line))
                    or 'BASIC LENS DATA' in line
                    or (line.strip().startswith('SURF') and 'RADIUS' in line)):
                out_lines.append(line)
                continue
            for c in getattr(self, '_sent_cmds', []):
                if c:
                    line = line.replace(c, '')
            out_lines.append(line)
        text = '\n'.join(out_lines)
        self.append_msg(text)
        # koko echoes a prompt like " 4:cmd> " after each command finishes.
        # Mark it idle here (on the cleaned stream) because the prompt has
        # no trailing newline, so it would otherwise stay stuck in _line_buf
        # and never reach _capture_rtg's parser.
        if re.search(r'\d+:\s*cmd>', text):
            self._koko_idle = True
        if not hasattr(self, '_line_buf'):
            self._line_buf = ""
        self._line_buf += text
        # split into complete lines (those ending in \n); keep the trailing
        # incomplete fragment buffered for the next poll
        parts = self._line_buf.split("\n")
        self._line_buf = parts.pop()
        for raw in parts:
            self._capture_rtg(raw + "\n")


    def _reap_koko(self):
        if self._koko_pid is None:
            return
        import os
        try:
            pid, _ = os.waitpid(self._koko_pid, os.WNOHANG)
        except ChildProcessError:
            pid = self._koko_pid
        if pid:
            self._koko_pid = None
            if getattr(self, "_koko_poll", None) is not None:
                self._koko_poll.stop()
            if self._koko_notifier is not None:
                self._koko_notifier.setEnabled(False)
            self._koko_watch.stop()
            self.append_msg("** koko-cli exited **")


    def append_msg(self, text):
        for line in text.split('\n'):
            if line.strip():
                self.msgView.append(line.rstrip())


    def execute_command(self):
        command = self.cmdLine.text().strip()
        if not command:
            return
        # Check for "lib get" command -> refresh table after lenssave
        if command.lower().startswith("lib get"):
            self.send_koko(command)
            self.send_koko("LENSSAVE\n")
            self.send_koko("RTG ALL\n")
            return
        self.send_koko(command)
        self.cmdLine.clear()


    def find_koko_cli(self):
        paths = [
            '/usr/local/bin/koko-cli',
            '/usr/bin/koko-cli',
            os.path.expanduser('~/bin/koko-cli'),
            os.path.expanduser('~/Koko/Src/koko-cli'),
            '/tmp/Koko/Src/koko-cli',
        ]
        for p in paths:
            if os.path.exists(p):
                return p
        return None


    def on_error(self, error):
        QMessageBox.critical(self, "koko-cli error", str(error))


    def on_finished(self, exit_code, exit_status):
        self.append_msg("** koko-cli exited (code %d) **" % exit_code)


    def send_koko(self, command):
        """Write a single command line to the koko-cli process."""
        if self._koko_fd is None or self._koko_pid is None:
            self.append_msg("** koko-cli is not running **")
            return
        # Suppress the "> command" echo in GUI mode only when explicitly
        # disabled. Set KOKO_GUI_ECHO=0 to hide the echo (koko is launched
        # with -G, GUI mode). The default is echo ON so command flow is
        # visible during normal use.
        if os.environ.get("KOKO_GUI_ECHO", "1") == "1":
            self.append_msg("> " + command.strip())
        self._koko_idle = False  # we just issued a command; koko is busy

        # Remember what we sent so _poll_koko_pty can strip the echo koko
        # writes back. Skip the bare "ECHO" query so its reply ("ECHO IS
        # OFF/ON") is not accidentally truncated by the removal below.
        cmd = command.strip()
        if cmd.upper() != "ECHO":
            self._sent_cmds.append(cmd)
            if len(self._sent_cmds) > 8:
                self._sent_cmds.pop(0)
        try:
            os.write(self._koko_fd, (command + "\n").encode('utf-8'))
        except OSError:
            self.append_msg("** failed to write to koko-cli **")
            return
        # If this is a plotting command, automatically render the graph.
        # Check every line (a menu command may be "GOTF\nPLTGOTF" etc.),
        # not just the first, so PLTGOTF/PLTDOTF/PLTSPD embedded after a
        # setup line still trigger the render.
        for line in command.strip().splitlines():
            tok = line.strip().upper()
            if not tok:
                continue
            first = tok.split()[0]
            # PLTOBJ is an internal IMTRACE瞄准 command (no plot output) and
            # PLTIMG writes PLOTBMP.BMP directly (handled by the ImageBlur
            # dialog's _schedule_image_render, NOT the gnuplot plot renderer).
            # Exclude both from the auto plot-render trigger so they don't
            # pop an intermediate/empty plot window mid Image-Blur run.
            if first in ('PLTOBJ', 'PLTIMG'):
                continue
            if any(tok.startswith(p) for p in PLOT_TRIGGER_PREFIXES) \
                    or first.startswith('PLT') or first.startswith('VIE') \
                    or first.startswith('FANS'):
                self._schedule_plot_render()
                break


    def start_koko_cli(self, lens_path=None):
        """Launch koko-cli and set up initial command schedule."""
        self._kill_koko()
        
        if not self.koko_path:
            QMessageBox.critical(
                self, "Error",
                "koko-cli not found. Please build Koko first "
                "(see Src/Makefile).")
            return False
        
        if not self._launch_koko_process():
            return False
        
        # If a lens was requested, restore it after startup banner
        if lens_path:
            QTimer.singleShot(600, lambda: self.load_lens(lens_path))
        else:
            default_lens = os.path.join(
                self.HOME, 'LENSES', 'COOCK.PRG')
            if os.path.exists(default_lens):
                self.current_lens = default_lens
                self._read_lens_file_meta(default_lens)
        
        # Disable command echo-back, then request surface listing
        QTimer.singleShot(250, lambda: self.send_koko("ECHO OFF"))
        QTimer.singleShot(400, lambda: self.send_koko("RTG ALL"))
        return True

