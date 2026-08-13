"""Glass-map (n vs v) plotting for the Koko PyQt6 GUI.

koko ships glass catalogs in AGF (ASCII Glass Format) under
``Libs/Glass_Data/<Catalog>/<name>.agf``. Each glass is introduced by an
``NM`` record whose fields are::

    NM <name> <group> <TCE> <Nd> <Vd> <status> <mfg>

so Nd (the d-line refractive index, plotted as ``n``) and Vd (the Abbe
number, plotted as ``v``) are available directly without evaluating the
dispersion polynomial.

This module parses every catalog the GUI can find and builds a list of
(name, Nd, Vd, catalog) tuples, then drives gnuplot (pngcairo) to render
an n-v scatter plot -- reusing the same PNG-to-window path as the rest of
the GUI. A click on the plot maps pixel coordinates back to (n, v) data
space and reports the nearest glass.
"""

import os
import re
import csv

_KOKO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
_GLASS_ROOT = os.path.join(_KOKO_ROOT, "Libs", "Glass_Data")

# NM <name> <grp> <TCE> <Nd> <Vd> <status> <mfg>
_NM_RE = re.compile(
    r"^NM\s+(\S+)\s+\S+\s+\S+\s+([-\d.E+]+)\s+([-\d.E+]+)")


def find_glass_files():
    """Return a list of (catalog_name, path) for every glass file found
    under Libs/Glass_Data (both .agf and .csv)."""
    out = []
    if not os.path.isdir(_GLASS_ROOT):
        return out
    for cat in sorted(os.listdir(_GLASS_ROOT)):
        cat_dir = os.path.join(_GLASS_ROOT, cat)
        if not os.path.isdir(cat_dir):
            continue
        for fn in sorted(os.listdir(cat_dir)):
            low = fn.lower()
            if low.endswith(".agf") or low.endswith(".csv"):
                out.append((cat, os.path.join(cat_dir, fn)))
    return out


def parse_glass_file(agf_path, catalog):
    """Parse one glass file (AGF or CSV) into a list of dicts with keys
    name, nd, vd, catalog."""
    if agf_path.lower().endswith(".csv"):
        return _parse_csv(agf_path, catalog)
    return _parse_agf(agf_path, catalog)


def _parse_agf(agf_path, catalog):
    glasses = []
    try:
        with open(agf_path, "r", errors="ignore") as fh:
            for line in fh:
                if not line.startswith("NM "):
                    continue
                m = _NM_RE.match(line)
                if not m:
                    continue
                name = m.group(1)
                try:
                    nd = float(m.group(2))
                    vd = float(m.group(3))
                except ValueError:
                    continue
                if not (1.0 < nd < 5.0 and 1.0 < vd < 200.0):
                    continue
                glasses.append(
                    {"name": name, "nd": nd, "vd": vd, "catalog": catalog})
    except OSError:
        return []
    return glasses


def _parse_csv(csv_path, catalog):
    """Parse a manufacturer CSV (Hoya/Ohara/Hikari/Sumita style).

    The header spans two rows: row 2 is a category band, row 3 is the
    column-name row. We locate the columns whose names contain 'Glass
    Type', an 'nd'/'nD' refractive index, and a 'vd'/'νd' Abbe number.
    """
    glasses = []
    try:
        with open(csv_path, "r", errors="ignore", newline="") as fh:
            reader = csv.reader(fh)
            rows = list(reader)
    except OSError:
        return []

    name_idx = nd_idx = vd_idx = None
    header_row = -1
    # Search the header rows (Hoya uses rows 2-3; Ohara/others use row 2).
    # Track the last row where a column was found so data starts just after.
    for ridx, row in enumerate(rows[:4]):
        for i, cell in enumerate(row):
            c = (cell or "").strip()
            cl = c.lower()
            if name_idx is None and ("glass type" in cl or cl == "glass"
                                      or cl == "glass " or "gtype" in cl
                                      or cl == "gtype"):
                name_idx = i
                header_row = max(header_row, ridx)
            # Prefer the bare lowercase 'nd' header (the d-line index). Fall
            # back to 'nD' only if 'nd' was never seen.
            if cl == "nd" or cl == "nd ":
                nd_idx = i
                header_row = max(header_row, ridx)
            elif nd_idx is None and (cl == "nd" or cl == "nd " or cl == "nD"
                                     or cl == "nD "):
                nd_idx = i
                header_row = max(header_row, ridx)
            if vd_idx is None and ("νd" in c or cl == "vd" or cl == "vd "
                                   or "abbe" in cl):
                vd_idx = i
                header_row = max(header_row, ridx)
    if name_idx is None or nd_idx is None or vd_idx is None:
        return glasses

    for row in rows[header_row + 1:]:
        if len(row) <= max(name_idx, nd_idx, vd_idx):
            continue
        name = (row[name_idx] or "").strip()
        if not name or name.upper() == "GLASS TYPE":
            continue
        try:
            nd = float(row[nd_idx])
            vd = float(row[vd_idx])
        except (ValueError, IndexError):
            continue
        if not (1.0 < nd < 5.0 and 1.0 < vd < 200.0):
            continue
        glasses.append(
            {"name": name, "nd": nd, "vd": vd, "catalog": catalog})
    return glasses


def load_all_glasses(catalogs=None):
    """Load glasses from all catalogs (or a subset if ``catalogs`` is given
    as an iterable of catalog directory names). Returns a list of dicts."""
    out = []
    files = find_glass_files()
    for cat, path in files:
        if catalogs is not None and cat not in set(catalogs):
            continue
        out.extend(parse_glass_file(path, cat))
    return out


def list_catalogs():
    """Return the sorted list of available catalog directory names."""
    if not os.path.isdir(_GLASS_ROOT):
        return []
    return sorted(
        d for d in os.listdir(_GLASS_ROOT)
        if os.path.isdir(os.path.join(_GLASS_ROOT, d))
    )


def write_gnuplot_data(glasses, data_path):
    """Write ``n v name`` rows (space separated) for gnuplot.

    Column 1 is the refractive index n (Nd), plotted on the x axis; column
    2 is the Abbe number v (Vd), plotted on the y axis. This matches the
    conventional glass-map orientation (x = n, y = v).
    """
    with open(data_path, "w") as fh:
        for g in glasses:
            fh.write("%r %r %s\n" % (g["nd"], g["vd"], g["name"]))


def build_gnuplot_script(data_path, script_path, png_path, title,
                         xmin, xmax, ymin, ymax,
                         width=1400, height=1010,
                         lmargin=70, rmargin=20, tmargin=50, bmargin=60):
    """Write a gnuplot script that renders an n-v scatter plot.

    Glass-name labels are NOT drawn on the plot (with ~800+ glasses they
    would overlap into an unreadable mess); clicking a point reports the
    nearest glass name instead.

    Margins are supplied in PIXELS (relative to the requested width/height)
    and converted to gnuplot ``at screen`` fractions so the rendered plot
    area exactly matches the pixel coordinates used by the GUI's
    click-to-glass mapping (see GlassMapWindow._report_click).
    """
    # Convert pixel margins to gnuplot screen fractions (0..1, origin at the
    # lower-left). lmargin/rmargin are measured from the left/right edges,
    # tmargin/bmargin from the top/bottom edges.
    lf = lmargin / width
    rf = 1.0 - rmargin / width
    tf = 1.0 - tmargin / height
    bf = bmargin / height
    script = (
        "set terminal pngcairo size {w},{h} enhanced font 'DejaVuSans,10'\n"
        "set output '{png}'\n"
        "set title '{title}'\n"
        "set xlabel 'Refractive index  n  (Nd)'\n"
        "set ylabel 'Abbe number  v  (Vd)'\n"
        "set grid\n"
        "set key off\n"
        "set clip points\n"
        "set ytics 10\n"
        "set xtics 0.1\n"
        "set mytics 2\n"
        "set mxtics 2\n"
        "set lmargin at screen {lm}\n"
        "set rmargin at screen {rm}\n"
        "set tmargin at screen {tm}\n"
        "set bmargin at screen {bm}\n"
        "set xrange [{xmin}:{xmax}]\n"
        "set yrange [{ymin}:{ymax}]\n"
        "plot '{dat}' using 1:2 with points pt 7 ps 1.1 lc rgb '#1f5fa8' notitle\n"
    ).format(w=width, h=height, png=png_path, title=title,
             lm=lf, rm=rf, tm=tf, bm=bf,
             xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, dat=data_path)
    with open(script_path, "w") as fh:
        fh.write(script)


def compute_ranges(glasses, vpad=3.0, npad=0.02):
    """Return (vmin, vmax, nmin, nmax) covering all glasses with padding."""
    vs = [g["vd"] for g in glasses]
    ns = [g["nd"] for g in glasses]
    vmin, vmax = min(vs), max(vs)
    nmin, nmax = min(ns), max(ns)
    vmin -= vpad
    vmax += vpad
    nmin -= npad
    nmax += npad
    return vmin, vmax, nmin, nmax
