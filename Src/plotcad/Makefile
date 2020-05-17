#
# Makefile for plotting and CAD library
#
# Ulf GRIESMANN, April 2020
#

fSRC = plotcad1.f plotcad2.f plotcad3.f plotcad4.f plotcad5.f plotcad6.f plotcad7.f \
       plotcad8.f plotcad9.f plotcad10.f dxf.f bmp.f

FSRC = gnuplot.F

FOBJ = $(FSRC:.F=.o) $(fSRC:.f=.o)

# targets
all: libplotcad

libplotcad: $(FOBJ)
	ar rs libplotcad.a $?
	mv libplotcad.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.o: %.F
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
