#
# Makefile for math and optical engineering functions and subroutines
#
# Ulf GRIESMANN, April 2020
#

fSRC = bicubic.f  math.f  opteng.f  tilter.f dpsort.f splerror.f
f90SRC = sortdmat.f90 splineint.f90 cubicspline.f90 svd.f90 svdsub.f90 random.f90
mSRC = cubicspline.f90 svd.f90 svdsub.f90 random.f90

# modules
FMOD = $(mSRC:.f90=.mod)

FOBJ = $(fSRC:.f=.o) $(f90SRC:.f90=.o)

# targets
all: modules libkmath

modules: $(FMOD)
	 mv *.mod ../modules

libkmath: $(FOBJ)
	ar rs libkmath.a $?
	mv libkmath.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.o: %.f90
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.mod: %.f90
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
