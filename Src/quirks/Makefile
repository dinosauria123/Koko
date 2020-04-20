#
# Makefile for some quirky funcionality (astronomical and financial)
# Slated for removal
#
# Ulf GRIESMANN, April 2020
#

fSRC = astro.f profit.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libquirks

libquirks: $(FOBJ)
	ar r libquirks.a $?
	ranlib libquirks.a
	mv libquirks.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
