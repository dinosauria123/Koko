#
# Makefile for special surfaces library
#
# Ulf GRIESMANN, April 2020
#

fSRC = specsur1.f specsur2.f specsur3.f user.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libspecsurf

libspecsurf: $(FOBJ)
	ar r libspecsurf.a $?
	ranlib libspecsurf.a
	mv libspecsurf.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
