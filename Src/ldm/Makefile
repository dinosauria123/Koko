#
# Makefile for Lens Database Manager Library
#
# Ulf GRIESMANN, April 2020
#

fSRC = ldm1.f ldm2.f ldm3.f ldm4.f ldm5.f ldm6.f ldm7.f ldm8.f ldm9.f ldm10.f \
       ldm11.f ldm12.f ldm12.f ldm13.f ldm14.f ldm15.f ldm16.f ldm17.f ldm18.f

FOBJ = $(fSRC:.f=.o)

# targets
all: libldm

libldm: $(FOBJ)
	ar r libldm.a $?
	ranlib libldm.a
	mv libldm.a ../../lib

%.o: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

clean:
	rm -f *.o *~
