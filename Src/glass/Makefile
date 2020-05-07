#
# Makefile for the glass data handling iibrary
#
# Ulf GRIESMANN, April 2020
#

fSRC = fglassload.f  gcatload2.f  gcatload.f  glassfresh.f  model.f  myglass.f  thickness_fresh.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libglass

libglass: $(FOBJ)
	ar r libglass.a $?
	ranlib libglass.a
	mv libglass.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
