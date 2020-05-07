#
# Makefile for wavefront and spot diagram analysis library
#
# Ulf GRIESMANN, April 2020
#

fSRC = wavspot1.f wavspot2.f wavspot3.f wavspot4.f wavspot5.f wavspot6.f wavspot7.f \
       wavspot8.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libwavespot

libwavespot: $(FOBJ)
	ar r libwavespot.a $?
	ranlib libwavespot.a
	mv libwavespot.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
