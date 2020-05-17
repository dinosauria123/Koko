#
# Makefile for Input / Output Function Library
#
# Ulf GRIESMANN, April 2020
#

FSRC = hardwar1.F
fSRC = hardwar2.f

FOBJ = $(FSRC:.F=.o) $(fSRC:.f=.o)

# targets
all: libhardware

libhardware: $(FOBJ)
	ar rs libhardware.a $?
	mv libhardware.a ../lib

%.o: %.F
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) -I.. $<

clean:
	rm -f *.o *~
