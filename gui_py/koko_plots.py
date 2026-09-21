"""KokoPlotMixin: koko_plots.py mixin (see gui_py/mainwindow.py).
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

class KokoPlotMixin:
    def _center_banner(self, win):
        """Center ``win`` on the client area of the main window (also works
        before the main window is fully shown)."""
        center = self.geometry().center()
        win.move(center.x() - win.width() // 2,
                 center.y() - win.height() // 2)


    def _is_point_plot(cmd_str):
        """Return True if the plot family is a spot/point diagram."""
        up = cmd_str.upper()
        return ('SPD' in up) or ('PSF' in up) or ('CAPFN' in up)


    def _on_plot_window_closed(self):
        """Plot window closed: drop its PNG and reset so the next plot is clean."""
        if self._plot_png_path:
            try:
                os.remove(self._plot_png_path)
            except OSError:
                pass
            self._plot_png_path = None
        self.plot_window = None


    def _poll_image_render(self):
        self._img_poll['tries'] += 1
        try:
            m = os.path.getmtime(self._pending_image)
        except OSError:
            m = 0.0
        done = m > self._img_poll['base']
        if done or self._img_poll['tries'] >= self._img_poll['max']:
            self._img_poll_active = False
            self._show_image_result()
        else:
            # Report progress roughly every 10s.
            if self._img_poll['tries'] % 10 == 0:
                self.append_msg(
                    "** Image Blur: tracing... (%d s) **"
                    % (self._img_poll['tries'] // 10 * 4))
            QTimer.singleShot(400, self._poll_image_render)


    def _poll_plot_render(self):
        self._plot_poll['tries'] += 1
        gpl = os.path.join(self.HOME, 'gnuplot', 'drawcmd.gpl')
        try:
            m = os.path.getmtime(gpl)
        except OSError:
            m = 0.0
        if m > self._plot_poll['base'] or self._plot_poll['tries'] >= 12:
            # Stop polling; hand off to render_plots (which is itself
            # serialized so two chains can never render at once).
            self._plot_poll_active = False
            self.render_plots()
        else:
            QTimer.singleShot(400, self._poll_plot_render)


    def _render_plots_inner(self, fmt=None):
        import subprocess as _sp
        gpl = os.path.join(os.path.expanduser('~'), 'KODS', 'gnuplot',
                           'drawcmd.gpl')
        if not os.path.isfile(gpl) or os.path.getsize(gpl) == 0:
            self.append_msg(
                "** %s not found or empty -- did the plot command run? **"
                % os.path.basename(gpl))
            return
        # Some plot families (notably PSF, via PLOTPSF -> PLTDEV ->
        # drawcmd3_clear) rewrite the body file drawcmd3.gpl and the per-
        # plot colour data files (black/red/yellow.gpl ...), but koko does
        # NOT call drawcmdsave for them, so the concatenated drawcmd.gpl is
        # left stale (it still carries the PREVIOUS figure's header/labels).
        # In that case the labels from the old plot overprint the new one.
        # Detect the staleness by comparing drawcmd.gpl's mtime against the
        # body file drawcmd3.gpl; if the body is newer, rebuild drawcmd.gpl
        # from its header (drawcmd0.gpl) + body (drawcmd3.gpl), exactly the
        # way koko's drawcmdsave would have.
        gpl_dir = os.path.dirname(gpl)
        body = os.path.join(gpl_dir, 'drawcmd3.gpl')
        header = os.path.join(gpl_dir, 'drawcmd0.gpl')
        need_rebuild = False
        if os.path.isfile(body):
            try:
                if os.path.getmtime(body) > os.path.getmtime(gpl) + 0.001:
                    need_rebuild = True
            except OSError:
                need_rebuild = False
        if need_rebuild and os.path.isfile(header) \
                and os.path.getsize(header) > 0 and os.path.getsize(body) > 0:
            try:
                with open(header, 'r') as fh:
                    htxt = fh.read()
                with open(body, 'r') as fb:
                    btxt = fb.read()
                # The body (drawcmd3.gpl) written by PSF/plot families may
                # NOT contain a "plot [...] black.gpl ..." line: those
                # families route the draw through setonecolors -> DRAW, and
                # the DRAW's own PLTDEV re-clears the body (unit 150) BEFORE
                # drawcmdsave concatenates it, wiping the plot line. So after
                # rebuild the figure would show the new labels but no curve.
                # Synthesize the missing plot line(s) from the data files
                # koko definitely wrote (black/red/yellow/magenta/cyan.gpl).
                if 'plot [' not in btxt and 'plot[' not in btxt:
                    plot_lines = self._synthesize_psf_plot_lines(gpl_dir)
                    if plot_lines:
                        btxt = btxt.rstrip('\n') + '\n' + plot_lines + '\n'
                with open(gpl, 'w') as fg:
                    fg.write(htxt)
                    # The body may itself start with an "unset label" line
                    # generated by PLTDEV; keep it (it clears stale labels).
                    fg.write(btxt)
                self.append_msg(
                    "** rebuilt stale drawcmd.gpl from header+body (PSF/plot "
                    "family did not call drawcmdsave) **")
            except OSError as _e:
                self.append_msg(
                    "** could not rebuild drawcmd.gpl: %s **" % _e)

        # drawcmd.gpl is a concatenation of MULTIPLE independent plot
        # blocks (e.g. X-Z layout, field curvature, spot diagram), each
        # terminated by "pause -1". koko appends every block for the
        # current draw into one file, so loading the whole thing makes
        # gnuplot overplot all blocks on a single canvas (the "graph
        # overprint" bug). We render ONLY the LAST block -- that is the
        # figure the user actually requested. We split on "pause" lines
        # (the block separators) and keep the trailing fragment.
        # Also strip any "set terminal" line so our pngcairo terminal wins.
        with open(gpl, 'r') as src:
            raw_lines = src.readlines()
        # Split into blocks at every line starting with "pause" (case-
        # insensitive); keep the last non-empty block.
        blocks = []
        cur = []
        for line in raw_lines:
            if line.strip().lower().startswith('pause'):
                if cur:
                    blocks.append(cur)
                cur = []
            else:
                cur.append(line)
        if cur:
            blocks.append(cur)
        # Fallback: if splitting yielded nothing (no pause markers), use
        # the whole file.
        last_block = blocks[-1] if blocks else raw_lines
        clean_gpl = os.path.join(self.TMPDIR, 'koko_gui_drawcmd.gpl')
        with open(clean_gpl, 'w') as dst:
            for line in last_block:
                low = line.strip().lower()
                # Drop any terminal line so our pngcairo terminal wins.
                if low.startswith('set terminal'):
                    continue
                dst.write(line)
        # Unique PNG path per render so two renders can never clobber the
        # same file and show a half-written / stale image.
        self._png_seq = getattr(self, '_png_seq', 0) + 1
        png_path = os.path.join(self.TMPDIR,
                                'koko_gnuplot_plot_%d.png' % self._png_seq)
        try:
            os.remove(png_path)
        except OSError:
            pass
        # Build a self-contained gnuplot script that loads drawcmd.gpl and
        # renders to PNG. Use pngcairo (no X display needed).
        script = os.path.join(self.TMPDIR, 'koko_gui_render.gpl')
        with open(script, 'w') as f:
            f.write('set terminal pngcairo size 1000,700 font "DejaVu Sans,9"\n')
            f.write('set output "%s"\n' % png_path)
            f.write('load "%s"\n' % clean_gpl)
        try:
            env = dict(os.environ)
            env['DISPLAY'] = ''
            _sp.run(['gnuplot', script], env=env, check=True,
                    stdout=_sp.DEVNULL, stderr=_sp.DEVNULL, timeout=30)
        except Exception as e:  # noqa: BLE001
            self.append_msg("** gnuplot failed: %s **" % e)
            return
        if not os.path.isfile(png_path) or os.path.getsize(png_path) == 0:
            self.append_msg("** plot PNG not produced **")
            return
        self.show_plot(png_path)


    def _schedule_image_render(self):
        """Wait for koko to write ~/PLOTBMP.BMP, then display it."""
        if getattr(self, '_img_poll_active', False):
            self._img_pending = True
            return
        self._img_poll_active = True
        self._img_poll_pending = False
        try:
            base = os.path.getmtime(self._pending_image)
        except OSError:
            base = 0.0
        self._img_poll = {'base': base, 'tries': 0, 'max': 600}
        QTimer.singleShot(1000, self._poll_image_render)


    def _schedule_plot_render(self):
        """Wait until koko updates drawcmd.gpl, then render the graph.

        Serialized: if a poll chain is already in flight, just mark a
        pending request so the running chain re-renders once more at the
        end instead of spawning a second concurrent chain (which would
        write the same fixed PNG path and overprint the image).
        """
        if self._plot_poll_active:
            self._render_pending = True
            return
        self._plot_poll_active = True
        self._render_pending = False
        gpl = os.path.join(self.HOME, 'gnuplot', 'drawcmd.gpl')
        try:
            base = os.path.getmtime(gpl)
        except OSError:
            base = 0.0
        self._plot_poll = {'base': base, 'tries': 0}
        QTimer.singleShot(600, self._poll_plot_render)


    def _show_image_result(self):
        path = self._pending_image
        if not os.path.isfile(path) or os.path.getsize(path) == 0:
            self.append_msg("** Image Blur: no output BMP produced **")
            return
        # Show via the existing plot viewer (handles BMP via QPixmap).
        self.show_plot(path)


    def _synthesize_psf_plot_lines(self, gpl_dir):
        """Build gnuplot 'plot' lines for a PSF/plot-family figure.

        Some plot families (PSF in particular) rewrite the per-colour data
        files (black.gpl, red.gpl, ...) but never emit a "plot [...]" command
        into drawcmd3.gpl (their DRAW pass re-clears the body before
        drawcmdsave concatenates it, wiping the plot line). Without a plot
        line the rebuilt drawcmd.gpl shows labels but no curve.

        This reconstructs the plot line(s) from whichever data files koko
        actually wrote and that contain real (non-empty) point data. Mirrors
        the colour routing in koko's gnuplot.f (black=130, yellow=115,
        magenta=116, red=117, cyan=118).
        """
        candidates = [
            ('black.gpl',  'black',      '0.70'),
            ('yellow.gpl', 'dark-yellow', '0.70'),
            ('magenta.gpl', 'magenta',   '0.70'),
            ('red.gpl',    'red',        '0.70'),
            ('cyan.gpl',   'cyan',       '0.70'),
        ]
        lines = []
        for fname, colour, lw in candidates:
            path = os.path.join(gpl_dir, fname)
            if not os.path.isfile(path) or os.path.getsize(path) == 0:
                continue
            try:
                with open(path) as fh:
                    content = fh.read().strip()
            except OSError:
                continue
            if not content:
                continue
            has_point = False
            for ln in content.splitlines():
                s = ln.strip()
                if not s:
                    continue
                parts = s.split()
                if len(parts) >= 2 and parts[0].lstrip('-').isdigit() \
                        and parts[1].lstrip('-').isdigit():
                    has_point = True
                    break
            if not has_point:
                continue
            lines.append(
                'plot [0:10000] [0:7000] "%s" lc rgb "%s" lw %s w l'
                % (path, colour, lw))
        return '\n'.join(lines)


    def _wait_for_psf_gri(self, path, timeout_s=30):
        """Poll ~/KODS/PSFGRI.DAT until PSF has written GRIIMG, return it."""
        import time
        base = 0.0
        try:
            base = os.path.getmtime(path)
        except OSError:
            base = 0.0
        deadline = time.time() + timeout_s
        while time.time() < deadline:
            try:
                m = os.path.getmtime(path)
            except OSError:
                m = 0.0
            if m > base:
                try:
                    with open(path) as fh:
                        val = float(fh.read().strip())
                    return val
                except (OSError, ValueError):
                    return 0.0
            time.sleep(0.3)
        return 0.0


    def render_plots(self, fmt=None):
        """Render koko's plot to a PNG and display it.

        koko is launched with ``-n`` (NOLAUNCH_GNUPLOT) so it writes the
        plot script into ``$HOME/KODS/gnuplot/drawcmd.gpl`` on VIE XZ but
        does NOT spawn gnuplot itself (launching gnuplot from koko hangs in
        this environment). Instead we run gnuplot here to produce the PNG
        and show it -- a fresh image on every lens switch.

        Serialized: only one render runs at a time. If a new plot request
        arrives while we are mid-render, it is queued (at most one) and
        replayed on exit, so rapid VIE XZ / DIST / PLTDIST clicks can never
        write the same PNG path concurrently and overprint the image.
        """
        # If already rendering, queue a single replay and bail.
        if self._rendering:
            self._render_pending = True
            return
        self._rendering = True
        try:
            self._render_plots_inner(fmt)
        finally:
            self._rendering = False
            # If another plot arrived during this render, replay once.
            if self._render_pending:
                self._render_pending = False
                self.render_plots(fmt)


    def show_plot(self, path):
        pix = QPixmap(path)
        if pix.isNull():
            self.append_msg("** plot image could not be loaded: %s **" % path)
            return
        # Make the plot window a top-level window (no parent) so it can
        # never be hidden behind the main window.
        if self.plot_window is None:
            from PyQt6.QtWidgets import QLabel, QVBoxLayout
            self.plot_window = PlotWindow(self)
            self.plot_window.setWindowTitle("Koko Plot")
            self._plot_label = QLabel()
            self._plot_label.setScaledContents(True)
            lay = QVBoxLayout(self.plot_window)
            lay.setContentsMargins(0, 0, 0, 0)
            lay.addWidget(self._plot_label)
        self._plot_png_path = path
        self._plot_label.setPixmap(pix)
        # Enforce a sensible minimum size so a degenerate pixmap can't
        # collapse the window to 0x0 (which looks like "nothing appears").
        size = pix.size()
        if size.width() < 200 or size.height() < 200:
            size = size.expandedTo(QSize(400, 300))
        self._plot_label.resize(size)
        self.plot_window.resize(size)
        self.plot_window.show()
        self.plot_window.raise_()


    def show_startup_banner(self):
        """Show a branded startup banner in its own top-level window for a
        few seconds after launch (then it auto-closes), mirroring the
        koko-cli greeting without cluttering the message view.
        """
        if getattr(self, "_banner_shown", False):
            return
        self._banner_shown = True

        version = self._read_buildstr()
        win = BannerWindow()
        win.set_content("Koko Optical Design Software (KODS)", version)
        win.show()
        win.raise_()
        win.activateWindow()
        QApplication.processEvents()
        # keep a reference so the window is not GC'd before it can close
        self._banner_window = win
        # Auto-dismiss after a short delay.
        QTimer.singleShot(3000, win.close)
        # Center on the main window.
        self._center_banner(win)


    def slot_export(self, fmt):
        self.append_msg("Export %s: generating plot..." % fmt)
        self.send_koko("PLOT")
        QTimer.singleShot(800, lambda: self.render_plots(fmt))


    def slot_plot(self, *commands):
        """Send one or more plotting commands (setup then draw)."""
        # Lazily establish a default field of view the first time any
        # plot is requested, so spot/wavefront/PSF plots have data.
        if not getattr(self, '_fob_sent', False):
            self.send_koko("FOB")
            self._fob_sent = True
        for cmd in commands:
            self.send_koko(cmd)
        # Remember which plot family this was so render_plots can choose
        # the right gnuplot style.
        self._last_plot_cmd = " ".join(commands)
        # koko writes drawcmd.gpl on the plotting command; render the PNG
        # once it updates.
        self._schedule_plot_render()


    def slot_quit2(self):
        self._kill_koko()
        self.close()


    def slot_text(self, command):
        """Send a command and let its textual output appear in msgView."""
        self.send_koko(command)


class BannerWindow(QWidget):
    """Branded startup banner shown in its own top-level window for a
    few seconds after launch, mirroring the koko-cli greeting without
    cluttering the message view. Auto-closes on its own.

    Follow the existing KOKO palette: light-grey (#eef0f2) header band,
    1px separators, centered alignment, native-looking widgets.
    """

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Koko")
        # A normal, decorated top-level window, raised above the main
        # window so the startup banner is actually visible (not hidden
        # behind the main window, which shares the same top-left corner).
        self.setWindowFlags(
            self.windowFlags()
            | Qt.WindowType.WindowStaysOnTopHint
            | Qt.WindowType.FramelessWindowHint)
        self.resize(640, 360)

        vbox = QVBoxLayout(self)
        vbox.setContentsMargins(0, 0, 0, 0)
        vbox.setSpacing(0)

        # Header band
        self.header = QLabel()
        self.header.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.header.setText("KOKO")
        self.header.setStyleSheet(
            "QLabel { "
            "background-color: #2f6f4f; "
            "color: #ffffff; "
            "padding: 12px; "
            "font-size: 24px; "
            "font-weight: bold; "
            "letter-spacing: 4px; }")
        self.header.setMinimumHeight(56)
        vbox.addWidget(self.header)

        # Mascot image
        art_dir = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
            "Artwork")
        mascot_path = os.path.join(art_dir, "koko_mascot.png")
        if os.path.exists(mascot_path):
            self.mascot = QLabel()
            self.mascot.setAlignment(Qt.AlignmentFlag.AlignCenter)
            self.mascot.setStyleSheet("QLabel { background-color: #f7f8f9; }")
            pix = QPixmap(mascot_path)
            if not pix.isNull():
                scaled = pix.scaled(
                    320, 240, Qt.AspectRatioMode.KeepAspectRatio,
                    Qt.TransformationMode.SmoothTransformation)
                self.mascot.setPixmap(scaled)
                self.mascot.setMinimumHeight(scaled.height())
                vbox.addWidget(self.mascot)

        # Separator
        sep = QFrame()
        sep.setFrameShape(QFrame.Shape.HLine)
        sep.setFrameShadow(QFrame.Shadow.Sunken)
        sep.setStyleSheet("QFrame { background-color: #c8ccd0; "
                          "border: none; }")
        sep.setMaximumHeight(2)
        vbox.addWidget(sep)

        # Body
        self.body = QLabel()
        self.body.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.body.setTextInteractionFlags(
            Qt.TextInteractionFlag.TextSelectableByMouse)
        self.body.setStyleSheet("QLabel { background-color: #f7f8f9; color: #333333; }")
        self.body.setMinimumHeight(180)
        vbox.addWidget(self.body)

    def set_content(self, brand, version):
        lines = []
        lines.append("")
        lines.append("%s" % brand)
        lines.append("")
        lines.append("Optical Design Software (KODS)")
        lines.append("")
        lines.append("Free software -- no warranty.")
        lines.append("See COPYING, LICENSE and AUTHORS in the source.")
        if version:
            lines.append("")
            lines.append(version)
        self.body.setText("\n".join(lines))

    def showEvent(self, event):
        super().showEvent(event)

class PlotWindow(QWidget):
    """Top-level plot viewer window.

    On close it notifies its owner (the KokoMainWindow) so the PNG it was
    displaying can be deleted and the window reference reset -- the next
    plot then builds a fresh window + fresh PNG instead of reusing a stale
    one (which caused the "overprint" artifact).
    """

    def __init__(self, owner):
        # No parent -> real top-level window with normal window decoration
        # (title bar + close button). A parented QWidget is rendered by the
        # WM as a frameless child window that cannot be closed.
        super().__init__(None)
        self._owner = owner

    def closeEvent(self, event):
        # Notify the owner BEFORE the default close (which may tear the
        # widget down) so it can delete the PNG and reset its reference
        # while the window is still fully alive.
        if self._owner is not None:
            self._owner._on_plot_window_closed()
            self._owner = None
        event.accept()
        super().closeEvent(event)

class _WinMouseEvent:
    """Adapter that wraps a QLabel-originated mouse event and reports its
    position in the *window* (GlassMapWindow) coordinate system, so the
    window's mousePressEvent can treat label clicks identically to direct
    window clicks. The ``_is_adapter`` flag tells the window handler NOT to
    re-dispatch to QWidget.mousePressEvent (which only accepts real events)."""

    _is_adapter = True

    def __init__(self, window, src_event):
        self._window = window
        self._src = src_event
        # Convert the source (label-local) position into window-global then
        # window-local coordinates.
        self._pos = QPointF(window.mapFromGlobal(src_event.globalPosition().toPoint()))

    def position(self):
        return self._pos

    def globalPosition(self):
        return self._src.globalPosition()

    def button(self):
        return self._src.button()

    def buttons(self):
        return self._src.buttons()

    def modifiers(self):
        return self._src.modifiers()

class GlassMapWindow(PlotWindow):
    """n-v glass-map viewer. Clicking the plot maps the pixel coordinate
    back to (n, v) data space (using the fixed gnuplot margins/ranges the
    PNG was rendered with) and reports the nearest glass."""

    def __init__(self, owner, glasses, geom):
        super().__init__(owner)
        self._glasses = glasses
        # geom: dict with vmin,vmax,nmin,nmax,width,height,lmargin,rmargin,
        #       tmargin,bmargin
        self._geom = geom
        # Build the plot label up front so _plot in the dialog can set the
        # pixmap directly.
        self._plot_label = QLabel()
        self._plot_label.setScaledContents(True)
        self._plot_label.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        # The QLabel does NOT forward mouse events to its parent by default,
        # so clicks land on the label and never reach this window's
        # mousePressEvent. Install a small filter that re-routes label clicks
        # to self.mousePressEvent so clicking anywhere on the plot works.
        self._plot_label.mousePressEvent = (
            lambda ev: self.mousePressEvent(
                _WinMouseEvent(self, ev)))
        # Result bar: shows the glass identified on the last click, directly
        # inside the map window so the user sees feedback without hunting
        # through the main message log.
        self._click_label = QLabel("Click a point on the map to identify the glass")
        self._click_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self._click_label.setStyleSheet(
            "QLabel { background-color: #eef0f2; border-top: 1px solid #c8ccd0; "
            "padding: 6px; font-weight: bold; }")
        self._click_label.setMinimumHeight(28)
        # Lay the plot (filling) above the result bar.
        lay = QVBoxLayout(self)
        lay.setContentsMargins(0, 0, 0, 0)
        lay.setSpacing(0)
        lay.addWidget(self._plot_label, 1)
        lay.addWidget(self._click_label, 0)
        # Keep this window above the main window / catalog picker so the map
        # is always visible in front (the WM would otherwise let it sink
        # behind the main window).
        self.setWindowFlag(Qt.WindowType.WindowStaysOnTopHint, True)
        # The window is sized to the rendered PNG in _plot() — the client
        # area grows by the result-bar height there.

    def mousePressEvent(self, event):
        if self._glasses and self._geom:
            # A click may arrive either on this window (event position is in
            # window coords) or on the child plot QLabel (event position is in
            # label coords, re-routed through a _WinMouseEvent adapter). Both
            # are normalized to window coordinates here.
            wp = event.position().toPoint()
            lp = self._plot_label.mapFrom(self, wp)
            self._report_click(lp.x(), lp.y(), self._plot_label.size())
        if getattr(event, "_is_adapter", False):
            # Already handled; do not re-dispatch to QWidget (it rejects the
            # adapter as a non-QMouseEvent).
            return
        super().mousePressEvent(event)

    def closeEvent(self, event):
        super().closeEvent(event)
        if self._owner is not None and hasattr(self._owner, "glass_map_window"):
            self._owner.glass_map_window = None
        self._owner = None

    def _report_click(self, px, py, label_size):
        g = self._geom
        # The label may be scaled to fit the window; scale the click back to
        # the logical 640x480 plot coordinate system.
        sx = g["width"] / label_size.width() if label_size.width() else 1.0
        sy = g["height"] / label_size.height() if label_size.height() else 1.0
        px *= sx
        py *= sy
        # px, py are now in the logical 640x480 plot coordinate system but in
        # Qt's TOP-left origin. gnuplot's term_* values use a BOTTOM-left
        # (PNG) origin, so flip Y to match before mapping.
        py = g["height"] - py
        # Prefer gnuplot's ACTUAL rendered plot rectangle (term_*) so the
        # click maps exactly to what was drawn. Fall back to the margin-based
        # rectangle if gnuplot didn't report one.
        if all(k in g for k in ("term_xmin", "term_xmax",
                                "term_ymin", "term_ymax")):
            x0, x1 = g["term_xmin"], g["term_xmax"]
            y0, y1 = g["term_ymin"], g["term_ymax"]
        else:
            x0 = g["lmargin"]
            x1 = g["width"] - g["rmargin"]
            y0 = g["tmargin"]
            y1 = g["height"] - g["bmargin"]
        plot_w = x1 - x0
        plot_h = y1 - y0
        if plot_w <= 0 or plot_h <= 0:
            return
        # Clamp to plot area.
        x = min(max(px, x0), x1)
        y = min(max(py, y0), y1)
        # Map pixel -> data.
        #   x axis = Abbe number v (Vd), LARGE at LEFT  -> xrange [vmax, vmin]
        #   y axis = refractive index n (Nd), small at bottom -> [nmin, nmax]
        # gnuplot's Y origin is the LOWER edge of the PNG (py already flipped
        # to bottom-left above).
        frac_x = (x - x0) / plot_w
        frac_y = (y - y0) / plot_h
        v = g["vmax"] - frac_x * (g["vmax"] - g["vmin"])
        n = g["nmin"] + frac_y * (g["nmax"] - g["nmin"])
        # Nearest glass in (n, v) space. We also measure the click-to-glass
        # distance in PLOT PIXELS so we can reject clicks that landed in empty
        # space (no dot near the cursor). The plotted dots are ~ps 1.1, so we
        # accept a hit within a small radius (a few dot-widths) of a point.
        best = None
        best_d = None
        best_px = None
        for gl in self._glasses:
            dn = gl["nd"] - n
            dv = gl["vd"] - v
            d = dn * dn + dv * dv
            if best_d is None or d < best_d:
                best_d = d
                best = gl
                # pixel position of this glass: x from v (left=large), y from n
                gfx = (g["vmax"] - gl["vd"]) / (g["vmax"] - g["vmin"]) * plot_w
                gfy = (gl["nd"] - g["nmin"]) / (g["nmax"] - g["nmin"]) * plot_h
                best_px = ((x - (x0 + gfx)) ** 2 + (y - (y0 + gfy)) ** 2) ** 0.5
        # Hit radius in plot pixels: dots are ~5px radius (ps 1.1), so a
        # generous radius keeps clicks that land just off-center from being
        # rejected as empty space.
        HIT_RADIUS_PX = 20.0
        if best is not None and best_px is not None and best_px <= HIT_RADIUS_PX:
            msg = ("Glass: %s  (catalog %s)\n"
                   "  n (Nd) = %.5f\n"
                   "  v (Vd) = %.3f" % (best["name"], best["catalog"],
                                       best["nd"], best["vd"]))
            if self._owner is not None and hasattr(self._owner, "append_msg"):
                self._owner.append_msg(">> Glass map click: " + msg.replace("\n", "  "))
            self.setWindowTitle("Glass Map — " + best["name"])
            # Show the result in the window's own result bar so the user
            # gets immediate, visible feedback on click.
            if self._click_label is not None:
                self._click_label.setText(
                    "Glass: %s  (catalog %s)    n = %.5f    v = %.3f"
                    % (best["name"], best["catalog"], best["nd"], best["vd"]))
        else:
            # No dot near the click: clicking empty space -> report none.
            self.setWindowTitle("Glass Map — (no glass here)")
            if self._click_label is not None:
                self._click_label.setText("No glass — click nearer to a data point")

class GlassMapDialog(QDialog):
    """Dialog to choose glass catalogs and render the n-v (index vs Abbe)
    glass map. Renders via gnuplot (pngcairo) and opens a GlassMapWindow
    where clicking reports the nearest glass."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Glass Map (n vs v)")
        self.resize(360, 200)
        self._glasses = []
        self._glass_map_window = None

        vbox = QVBoxLayout(self)
        hdr = QLabel("Glass catalog n–v map")
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        hdr.setStyleSheet(
            "QLabel { background-color: #eef0f2; border-bottom: 1px solid "
            "#c8ccd0; padding: 6px; font-weight: bold; }")
        vbox.addWidget(hdr)

        vbox.addWidget(QLabel("Select catalogs to plot:"))

        self._cat_checks = {}
        import gui_py.glassmap as gm
        for cat in gm.list_catalogs():
            cb = QCheckBox(cat)
            cb.setChecked(True)
            self._cat_checks[cat] = cb
            vbox.addWidget(cb)

        hbox = QHBoxLayout()
        self.btn_plot = QPushButton("Plot")
        self.btn_close = QPushButton("Close")
        hbox.addWidget(self.btn_plot)
        hbox.addWidget(self.btn_close)
        vbox.addLayout(hbox)

        self.btn_plot.clicked.connect(self._plot)
        self.btn_close.clicked.connect(self.reject)

    def _plot(self):
        import os
        import tempfile
        import subprocess
        import shutil
        import gui_py.glassmap as gm

        gnuplot_bin = shutil.which("gnuplot") or "gnuplot"
        env = dict(os.environ)
        env["DISPLAY"] = ""

        cats = [c for c, cb in self._cat_checks.items() if cb.isChecked()]
        if not cats:
            QMessageBox.information(self, "Glass Map",
                                    "Select at least one catalog.")
            return
        glasses = gm.load_all_glasses(catalogs=cats)
        if not glasses:
            QMessageBox.information(self, "Glass Map", "No glasses found.")
            return
        self._glasses = glasses

        vmin, vmax, nmin, nmax = gm.compute_ranges(glasses)
        # Margins are PIXELS (relative to the 640x480 render). They are used
        # both by gnuplot (converted to screen fractions in glassmap.py) and
        # by the click-to-glass mapping in GlassMapWindow._report_click, so
        # the two coordinate systems stay exactly in sync.
        geom = dict(vmin=vmin, vmax=vmax, nmin=nmin, nmax=nmax,
                    width=640, height=480,
                    lmargin=70, rmargin=20, tmargin=50, bmargin=60)

        tmp = tempfile.mkdtemp(prefix="koko_glassmap_")
        data_path = os.path.join(tmp, "glassmap.dat")
        script_path = os.path.join(tmp, "glassmap.gpl")
        png_path = os.path.join(tmp, "glassmap.png")
        gm.write_gnuplot_data(glasses, data_path)
        gm.build_gnuplot_script(data_path, script_path, png_path,
                                "Glass Map (v vs n)", vmax, vmin, nmin, nmax,
                                width=geom["width"], height=geom["height"],
                                lmargin=geom["lmargin"], rmargin=geom["rmargin"],
                                tmargin=geom["tmargin"], bmargin=geom["bmargin"])

        r = subprocess.run([gnuplot_bin, script_path], env=env,
                           capture_output=True, text=True, timeout=30)
        if r.returncode != 0 or not os.path.exists(png_path):
            QMessageBox.critical(self, "Glass Map",
                                 "gnuplot failed:\n" + (r.stderr or r.stdout))
            return

        # Capture the ACTUAL rendered plot rectangle (PNG pixel coords) so
        # click-to-glass mapping matches gnuplot's real layout exactly.
        # gnuplot's `print` writes to stderr, so read it from there.
        rect = gm.parse_plot_rect(r.stderr)
        if rect is not None:
            geom["term_xmin"], geom["term_xmax"], \
                geom["term_ymin"], geom["term_ymax"] = rect
        # Pass the main window as owner so click reports can be written to
        # its message log via _owner.append_msg(). The GlassMapWindow itself
        # is a top-level window (no parent) so it never hides behind others.
        owner = self.parent()
        win = GlassMapWindow(owner, glasses, geom)
        win.setWindowTitle("Glass Map (n vs v) — %d glasses" % len(glasses))
        pix = QPixmap(png_path)
        win._plot_label.setPixmap(pix)
        win._plot_label.setScaledContents(True)
        if win.layout() is not None:
            win.layout().setContentsMargins(0, 0, 0, 0)
        win.show()
        QApplication.processEvents()
        fw = win.frameGeometry().width() - win.geometry().width()
        fh = win.frameGeometry().height() - win.geometry().height()
        # Include the result bar (its current height) so it is not clipped by
        # the fixed-size window.
        bar_h = win._click_label.height() if win._click_label is not None else 0
        win.setFixedSize(pix.width() + fw, pix.height() + bar_h + fh)
        # Keep reference so it's not GC'd
        self._glass_map_window = win
        win.raise_()
        win.activateWindow()
        # Close the catalog picker now that the map is on screen. The map
        # window is a top-level window with its own lifecycle, so it stays
        # alive for click-to-identify after the picker closes.
        self.accept()

