#
# Makefile for utility function/subroutine library
#
# Ulf GRIESMANN, April 2020
#

fSRC = utility1.f utility2.f utility3.f utility4.f utility5.f utility6.f utility7.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libutility

libutility: $(FOBJ)
	ar rs libutility.a $?
	mv libutility.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
