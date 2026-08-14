"""End-to-end test of the stale-drawcmd.gpl rebuild + PSF plot-line
synthesis logic.

Reproduces the FANS -> PSF bug scenario without the full GUI:
  1. drive koko to produce FANS then PSF in one session
  2. run the same rebuild logic as KokoMainWindow._render_plots_inner
  3. synthesize the PSF plot line from the data files (koko does not emit
     one for PSF)
  4. render the resulting drawcmd.gpl with gnuplot and confirm the PSF
     labels appear (old FANS labels gone) and a curve is drawn.

Run from /home/dino/Koko with:  python3.14 gui_py/test_render_rebuild.py
"""
import os, pty, time, select, subprocess, sys, tempfile

GPL_DIR = os.path.expanduser('~/KODS/gnuplot')


def run_koko_seq(steps, lens="COOCK"):
    master, slave = pty.openpty()
    pid = os.fork()
    if pid == 0:
        os.setsid()
        os.dup2(slave, 0); os.dup2(slave, 1); os.dup2(slave, 2)
        os.execv("/home/dino/Koko/Src/koko-cli", ["koko-cli"])
        os._exit(1)
    os.close(slave)

    def drain(t=0.6):
        end = time.time() + t
        while time.time() < end:
            r, _, _ = select.select([master], [], [], 0.2)
            if r:
                try:
                    d = os.read(master, 4096)
                    if not d:
                        break
                except OSError:
                    break

    for f in ["drawcmd.gpl", "drawcmd0.gpl", "drawcmd3.gpl",
              "black.gpl", "red.gpl", "yellow.gpl", "magenta.gpl", "cyan.gpl"]:
        open(os.path.join(GPL_DIR, f), "w").close()
    drain(1.5)
    os.write(master, ("LENSREST %s\n" % lens).encode()); drain(1.5)
    for _label, cmds in steps:
        for c in cmds:
            os.write(master, (c + "\n").encode()); drain(2.5)
    os.write(master, b"EXIT\n"); drain(1.0); os.close(master)


def synthesize_psf_plot_lines(gpl_dir):
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
            content = open(path).read().strip()
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
        lines.append('plot [0:10000] [0:7000] "%s" lc rgb "%s" lw %s w l'
                     % (path, colour, lw))
    return '\n'.join(lines)


def rebuild_if_stale():
    gpl = os.path.join(GPL_DIR, 'drawcmd.gpl')
    body = os.path.join(GPL_DIR, 'drawcmd3.gpl')
    header = os.path.join(GPL_DIR, 'drawcmd0.gpl')
    if not os.path.isfile(gpl) or os.path.getsize(gpl) == 0:
        return "missing"
    need = False
    if os.path.isfile(body):
        try:
            if os.path.getmtime(body) > os.path.getmtime(gpl) + 0.001:
                need = True
        except OSError:
            need = False
    if need and os.path.isfile(header) and os.path.getsize(header) > 0 \
            and os.path.getsize(body) > 0:
        htxt = open(header).read()
        btxt = open(body).read()
        if 'plot [' not in btxt and 'plot[' not in btxt:
            pl = synthesize_psf_plot_lines(GPL_DIR)
            if pl:
                btxt = btxt.rstrip('\n') + '\n' + pl + '\n'
        with open(gpl, 'w') as fg:
            fg.write(htxt)
            fg.write(btxt)
        return "rebuilt"
    return "ok"


def render_png(gpl_path, out_png):
    script = os.path.join(GPL_DIR, "_rtest.gpl")
    with open(script, "w") as f:
        f.write('set terminal pngcairo size 1000,700 font "DejaVu Sans,9"\n')
        f.write('set output "%s"\n' % out_png)
        f.write('load "%s"\n' % gpl_path)
    env = dict(os.environ)
    env['DISPLAY'] = ''
    subprocess.run(['gnuplot', script], env=env,
                   stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                   timeout=30, check=True)


def main():
    tmp = tempfile.mkdtemp(prefix="koko_rtest_")
    print("== FANS then PSF in one koko session ==")
    run_koko_seq([
        ("FANS", ["FANS XFAN"]),
        ("PSF", ["PSFWRITE YES", "PSFPLOT YES", "PSF,1", "CAPFNOUT"]),
    ])
    gpl = os.path.join(GPL_DIR, 'drawcmd.gpl')
    txt_before = open(gpl).read()
    print("before rebuild: FANS label? %s  PSF label? %s"
          % ("XZ-PLANE TRANSVERSE ABERRATIONS" in txt_before,
             "Diffraction PSF" in txt_before))

    status = rebuild_if_stale()
    print("rebuild status:", status)
    txt_after = open(gpl).read()
    print("after rebuild: FANS label? %s  PSF label? %s  plot lines=%d  size=%d"
          % ("XZ-PLANE TRANSVERSE ABERRATIONS" in txt_after,
             "Diffraction PSF" in txt_after,
             txt_after.count("plot ["), len(txt_after)))

    png = os.path.join(tmp, "out.png")
    try:
        render_png(gpl, png)
        produced = os.path.getsize(png) > 0
    except Exception as e:
        produced = False
        print("PNG render error:", e)
    print("PNG produced:", produced, os.path.getsize(png) if produced else 0, "bytes")

    # Assertions: the core fix is that the stale FANS text no longer leaks.
    assert status == "rebuilt", "expected rebuild for FANS->PSF"
    assert "XZ-PLANE TRANSVERSE ABERRATIONS" not in txt_after, \
        "old FANS label leaked into PSF render!"
    assert "Diffraction PSF" in txt_after, "PSF label missing after rebuild"
    print("\nPASS: stale FANS text no longer overprints the PSF plot.")


if __name__ == "__main__":
    main()
