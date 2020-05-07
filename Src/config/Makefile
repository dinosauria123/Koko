#
# Makefile for the Configuration Library
#
# Ulf GRIESMANN, April 2020
#

fSRC = config1.f config2.f config3.f config4.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libconfig

libconfig: $(FOBJ)
	ar r libconfig.a $?
	ranlib libconfig.a
	mv libconfig.a ../../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
