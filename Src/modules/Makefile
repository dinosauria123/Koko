#
# Makefile for Fortran modules
#
# Ulf GRIESMANN, April 2020
#

IFLAGS = -I../include

# module sources
MSRC= globals.f nssmod.f

MODS = $(MSRC:.f=.mod)
FOBJ = $(MSRC:.f=.o)

# targets
all: libkmod

libkmod: $(MODS)
	ar r libkmod.a $(FOBJ)
	ranlib libkmod.a
	mv libkmod.a ../lib

%.mod: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.mod *.o *~
