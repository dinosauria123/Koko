#
# Basic Makefile for compiling Koko ODS with gfortran
#
# Ulf GRIESMANN, April 2020
#

SUBDIRS = kmath ldm config hardware macro optimize paraxial pickup \
          raytrace specsurf nonseq utility wavespot glass plotcad

# default platform is Linux
#ifndef OS
  OS = LINUX
#endif

# default installation directory prefix
ifndef PREFIX
  PREFIX=/usr/local
endif

# compiler flags
CC = gcc         # for now, only support GCC
FC = gfortran
ifdef DEBUG
  FFLAGS = -g -cpp -Wall -ffixed-line-length-none -malign-double -fno-align-commons -fallow-argument-mismatch -D$(OS)
  CFLAGS = -Wall -g -malign-double
else
  ifdef NATIVE
    FFLAGS = -g -cpp -O2 -ffixed-line-length-none -march=native -mtune=native -malign-double -fno-align-commons -fallow-argument-mismatch -D$(OS)
  else
    FFLAGS = -g -cpp -O2 -ffixed-line-length-none -malign-double -fno-align-commons -fallow-argument-mismatch -D$(OS)
  endif  
  CFLAGS = -g -O2 -malign-double
endif
IFLAGS = -I../include -I../modules

# needed in sub-make processes
export CC FC IFLAGS CFLAGS FFLAGS

LDIR = -L./lib
LIBS = -static -Wl,--start-group $(addprefix -l,$(SUBDIRS)) -llinenoise -lkmod -Wl,--end-group -lm -lstdc++

fSRC = koko.f cmder.f names.f libload.f saverest.f

FOBJ = $(fSRC:.f=.o)

# Objects and sublibraries that consume the shipped Fortran module files must
# wait for `modules` to finish, otherwise under `make -j` they compile before
# the .mod files (globals.mod, opsys.mod, getsoptions.mod, ...) exist.
# `modules` is used as an *order-only* prerequisite (|), so it is guaranteed
# to be built first but does not force an unconditional rebuild of consumers.

# targets
# These goals never correspond to files named after them, so declare them
# phony to avoid a same-named file silently disabling the rule.
.PHONY: all clean install strip
all: buildinfo.inc libdir modules libraries koko-cli

.PHONY: modules
modules:
	make -C modules

# linenoise is a plain C++ library (no Fortran .mod files), so it does NOT
# belong in SUBDIRS (that would make `liblinenoise` link against `-llinenoise`
# before it exists). But `koko-cli` links it via `-llinenoise`, so it must be
# built before link time; `libraries` depends on it explicitly.
.PHONY: linenoise
linenoise:
	make -C linenoise

.PHONY: libraries $(SUBDIRS)
libraries: $(SUBDIRS) linenoise
# Sublibraries that consume the shipped .mod files must wait for `modules`.
# `modules` itself is excluded from this rule to avoid a self-dependency
# (modules is a member of SUBDIRS), which would otherwise break the build.
$(filter-out modules,$(SUBDIRS)): | modules
	make -C $@

koko-cli: buildinfo.inc $(FOBJ) libraries
	$(FC) -o koko-cli $(FOBJ) $(LDIR) $(LIBS)

# koko.o embeds buildinfo.inc (git commit number). Make sure it is
# recompiled whenever buildinfo.inc changes, otherwise the running
# binary can display a newer commit number than the code it was
# actually built from (stale-build confusion).
koko.o: koko.f buildinfo.inc | modules
	$(FC) -c $(FFLAGS) -I./include -I./modules $<

# buildinfo.inc is a real file generated from the buildinfo script. A file
# rule (not a phony target) lets make track it and only regenerate it when the
# script changes, so koko-cli (which embeds it) recompiles in the right order.
buildinfo.inc: buildinfo
	./buildinfo

%.o: %.f | modules
	$(FC) -c $(FFLAGS) -I./include -I./modules $<

libdir:
	mkdir -p ./lib

clean:
	rm -f *.o *~ koko-cli buildinfo.inc
	rm -rf ./lib
	make -C modules clean
	make -C linenoise clean
	for d in $(SUBDIRS) ; do make -C $$d clean ; done

strip:
	strip -S ./koko-cli

install:
	mkdir -p $(PREFIX)/bin
	install -m 755 koko-cli $(PREFIX)/bin
