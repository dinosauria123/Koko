#
# Makefile for pickup handling library
#
# Ulf GRIESMANN, April 2020
#

fSRC = pikups1.f pikups2.f pikfresh.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libpickup

libpickup: $(FOBJ)
	ar rs libpickup.a $?
	mv libpickup.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
