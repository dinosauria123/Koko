#
# Makefile for paraxial raytracing library
#
# Ulf GRIESMANN, April 2020
#

fSRC = parax1.f parax2.f parax3.f parax4.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libparaxial

libparaxial: $(FOBJ)
	ar r libparaxial.a $?
	ranlib libparaxial.a
	mv libparaxial.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
