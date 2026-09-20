#
# Makefile for Fortran modules
#
# Ulf GRIESMANN, April 2020
#

IFLAGS = -I../include

# module sources
fSRC   = globals.f nssmod.f gplblk.f
f90SRC = configfile.f90 kokoconfig.f90 getoptions.f90 commandline.f90 strings.f90 \
         rgb.f90 posix_regex.f90
F90SRC = opsys.F90 treap.F90 dictionary.F90
CSRC   = strings_f90.c rgb_f90.c

MODS = $(fSRC:.f=.mod) $(f90SRC:.f90=.mod) $(F90SRC:.F90=.mod)
FOBJ = $(fSRC:.f=.o) $(f90SRC:.f90=.o) $(F90SRC:.F90=.o)
COBJ = $(CSRC:.c=.o)

# Module compilation must run serially: several .f90 sources `USE` the module
# produced by a *sibling* .f90 file (e.g. kokoconfig.f90 USEs configfile), so
# under `make -j` make has no ordering guarantee between independent `%.mod`
# rules. Serialize here so each .mod is fully written before the next runs.
.NOTPARALLEL:

# targets
all: regex_f90.inc libkmod

libkmod: $(MODS) $(COBJ)
	ar rs libkmod.a $(FOBJ) $(COBJ)
	mv libkmod.a ../lib

%.mod: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.mod: %.f90
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.mod: %.F90
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.o : %.c
	$(CC) -c $(CFLAGS) $<

regex_f90.inc:
	$(CC) -o regex_t_size regex_t_size.c
	./regex_t_size > regex_f90.inc

clean:
	rm -f *.mod *.o *~ regex_f90.inc regex_t_size
