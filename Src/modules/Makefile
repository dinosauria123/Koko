#
# Makefile for Fortran modules
#
# Ulf GRIESMANN, April 2020
#

IFLAGS = -I../include

# module sources
fSRC   = globals.f nssmod.f
f90SRC = configfile.f90 getoptions.f90
F90SRC = opsys.F90

MODS = $(fSRC:.f=.mod) $(f90SRC:.f90=.mod) $(F90SRC:.F90=.mod)
FOBJ = $(fSRC:.f=.o) $(f90SRC:.f90=.o) $(F90SRC:.F90=.o)

# targets
all: libkmod

libkmod: $(MODS)
	ar r libkmod.a $(FOBJ)
	ranlib libkmod.a
	mv libkmod.a ../lib

%.mod: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.mod: %.f90
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.mod: %.F90
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.mod *.o *~
