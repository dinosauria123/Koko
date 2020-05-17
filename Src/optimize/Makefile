#
# Makefile for Optimization Library
#
# Ulf GRIESMANN, April 2020
#

fSRC = optim1.f optim2.f optim3.f optim4.f optim5.f optim6.f optim7.f optim8.f \
       optim9.f optim10.f optim11.f 

FOBJ = $(fSRC:.f=.o)

# targets
all: liboptimize

liboptimize: $(FOBJ)
	ar rs liboptimize.a $?
	mv liboptimize.a ../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
