#
# Makefile for non-sequential raytracing library
#
# Ulf GRIESMANN, April 2020
#

fSRC = nssfile1.f nssfile2.f nssfile3.f nssfile4.f nssfile5.f nssdxf.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libnonseq

libnonseq: $(FOBJ)
	ar r libnonseq.a $?
	ranlib libnonseq.a
	mv libnonseq.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
