#
# Makefile for raytrace library
#
# Ulf GRIESMANN, April 2020
#

fSRC = raytra1.f raytra2.f raytra3.f raytra4.f raytra5.f raytra6.f raytra7.f \
       raytra8.f raytra9.f raytra10.f raytra11.f raytra12.f raytra13.f \
       aimdifrys.f difrays.f insider.f image.f spect.f fobbs.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libraytrace

libraytrace: $(FOBJ)
	ar r libraytrace.a $?
	ranlib libraytrace.a
	mv libraytrace.a ../../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
