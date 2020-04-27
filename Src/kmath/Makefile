#
# Makefile for math and optical engineering functions and subroutines
#
# Ulf GRIESMANN, April 2020
#

fSRC = bicubic.f  math.f  opteng.f  svd.f  tilter.f dpsort.f
f90SRC = sortdmat.f90

FOBJ = $(fSRC:.f=.o) $(f90SRC:.f90=.o)

# targets
all: libkmath

libkmath: $(FOBJ)
	ar r libkmath.a $?
	ranlib libkmath.a
	mv libkmath.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.o: %.f90
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
