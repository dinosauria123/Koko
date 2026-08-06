#
# Makefile for plotting and CAD library
#
# Ulf GRIESMANN, April 2020
#

fSRC = plotcad1.f plotcad2.f plotcad3.f plotcad4.f plotcad5.f plotcad6.f plotcad7.f \
       plotcad8.f plotcad9.f plotcad10.f dxf.f bmp.f gnuplot.f gplblk.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libplotcad

libplotcad: $(FOBJ)
	ar rs libplotcad.a $?
	mv libplotcad.a ../lib

%.o: %.f
	gfortran -c -g -cpp -O2 -ffixed-line-length-none -malign-double -fno-align-commons -fallow-argument-mismatch -DLINUX -I../include -I../modules $<

# gnuplot.f uses the gplblk_mod module; ensure it is compiled first so
# gplblk_mod.mod exists when gnuplot.f is compiled.
gnuplot.o: gplblk.o

clean:
	rm -f *.o *~
