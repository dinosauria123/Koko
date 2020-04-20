#
# Makefile for functions that provide macro functionality
#
# Ulf GRIESMANN, April 2020
#

fSRC = macro1.f macro2.f macro3.f macro4.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libmacro

libmacro: $(FOBJ)
	ar r libmacro.a $?
	ranlib libmacro.a
	mv libmacro.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
