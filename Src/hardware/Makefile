#
# Makefile for Input / Output Function Library
#
# Ulf GRIESMANN, April 2020
#

FSRC = hardwar1.f hardwar2.f

FOBJ = $(FSRC:.f=.o)

# targets
all: libhardware

libhardware: $(FOBJ)
	ar rs libhardware.a $?
	mv libhardware.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) -I.. $<

clean:
	rm -f *.o *~
