#
# Improved top-level make file for Koko
#
# Ulf GRIESMANN, April 2020
# Improved by Devin, 2026
#

# ===========================================================================
# Variables
# ===========================================================================

# Installation prefix (override on the command line: make PREFIX=/opt install)
ifndef PREFIX
  PREFIX = /usr/local
endif

# Where the writable program data goes (a user-writable directory, not /usr).
ifndef DATAPREFIX
  DATAPREFIX = $(HOME)
endif

# Forward everything a sub-make might need.
export PREFIX
export DATAPREFIX
export CC FC IFLAGS CFLAGS FFLAGS LDIR LIBS
export DEBUG NATIVE

# Sub-make control: let the top-level parallel flag (-j) flow through to
# every sub-make. The default fan-out is the number of available CPUs.
ifdef PARALLEL
  SUBMAKEFLAGS = -j$(PARALLEL)
else
  SUBMAKEFLAGS = -j$(shell nproc 2>/dev/null)
endif

# ===========================================================================
# Targets
# ===========================================================================

.PHONY: all clean install install-exec install-conf install-data strip help

# GUI source tree is optional: only invoke it when a MakeGui build file
# ships alongside ./QtGui *and* the Qt "qmake" tool is actually on PATH.
# Otherwise fall back to building the command-line core, which is what
# ships in this checkout and needs no Qt installation.
ifeq ($(filter $(shell command -v qmake 2>/dev/null),qmake),)
  HAVE_QMAKE = no
else
  HAVE_QMAKE = yes
endif
# Reuse the detection result instead of running `command -v qmake` a second time.
ifeq ($(HAVE_QMAKE),yes)
  GUI = -C ./QtGui -f MakeGui
else
  GUI =
endif

# build everything (the default goal)
all:
	@echo ""
	@echo "==> Building the Koko command-line core ..."
	$(MAKE) -C ./Src $(SUBMAKEFLAGS)
	@if [ -n "$(GUI)" ]; then \
		echo "" > /dev/null; \
		echo "==> Building the Koko GUI ..."; \
		$(MAKE) $(GUI) koko-gui $(SUBMAKEFLAGS); \
	else \
		echo "" > /dev/null; \
		echo "==> No Qt GUI sources present (./QtGui/MakeGui missing); skipping GUI build."; \
	fi
	@echo ""
	@echo "It's all done !"
	@echo
	@echo "Optional next step:"
	@echo "     make strip          ( remove debugging information from koko-cli )"
	@echo
	@echo "Now install the programs with (requires superuser privileges):"
	@echo "     make install        ( full installation )"
	@echo "     make install-exec   ( install executables only )"
	@echo "     make install-conf   ( install system-wide configuration file )"
	@echo "     make install-data   ( install program data, as a normal user )"
	@echo
	@echo "     make clean          ( remove intermediate files )"
	@echo
	@echo "See INSTALL.md for details and for installation instructions as a user"
	@echo "without superuser privileges."
	@echo
	@echo "*****************************************************************************"
	@echo ""

# install executables and system-wide config file
install: install-exec install-conf

# install executable files only
install-exec:
	@echo "Installing executable files ..."
	@echo "==============================="
	$(MAKE) -C ./Src install
	@if [ -n "$(GUI)" ]; then $(MAKE) $(GUI) install; fi
	install -D -m 644 koko.desktop /usr/share/applications/koko.desktop
	@if [ -n "$(GUI)" ]; then \
		install -D -m 644 ./QtGui/images/koko.png \
			/usr/share/icons/hicolor/512x512/apps/koko.png \
		command -v update-desktop-database >/dev/null 2>&1 && update-desktop-database || true; \
	fi

# install system-wide configuration file
install-conf:
	@echo "Installing system-wide configuration file"
	@echo "========================================="
	install --backup=numbered -m 644 ./kokorc "$(PREFIX)/etc"

# install program data (run as a normal user, not superuser)
install-data:
	@echo "Installing program data into $(DATAPREFIX)/KODS ..."
	mkdir -p "$(DATAPREFIX)/KODS"
	cd ./Libs && cp -R * "$(DATAPREFIX)/KODS"

# clean up intermediate build artefacts
clean:
	@echo "==> Cleaning ..."
	$(MAKE) -C ./Src clean
	@if [ -n "$(GUI)" ]; then $(MAKE) -C ./QtGui -f MakeGui clean; fi

# remove debugging symbols from koko-cli
strip:
	$(MAKE) -C ./Src strip

# show usage
help:
	@echo "Koko top-level build system"
	@echo ""
	@echo "Targets:"
	@echo "  make                 build koko-cli and koko-gui"
	@echo "  make PARALLEL=N      build with N parallel jobs (default: nproc)"
	@echo "  make DEBUG=true      build with debugging information"
	@echo "  make NATIVE=true     optimize for the current CPU"
	@echo "  make install         install executables + system config (needs root)"
	@echo "  make install-exec    install executables only (needs root)"
	@echo "  make install-conf    install /etc/kokorc (needs root)"
	@echo "  make install-data    install program data into a writable dir (as user)"
	@echo "  make PREFIX=DIR      use DIR/bin as the install prefix (default /usr/local)"
	@echo "  make DATAPREFIX=DIR  where install-data puts data (default $(HOME))"
	@echo "  make strip           strip debug symbols from koko-cli"
	@echo "  make clean           remove build artefacts"
