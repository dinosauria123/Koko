#
# Basic Makefile for compiling Koko ODS with gfortran
#
# Ulf GRIESMANN, April 2020
#

SUBDIRS = ldm config hardware macro optimize paraxial pickup raytrace \
          specsurf nonseq utility wavespot glass plotcad kmath quirks

# default platform is Linux
ifndef OS
  OS = LINUX
endif

# compiler flags
CC = gcc
FC = gfortran
CFLAGS = -Wall -g -malign-double
ifdef DEBUG
  FFLAGS = -g -Wall -ffixed-line-length-none -malign-double -fno-align-commons -D$(OS)
else
  ifdef NATIVE
    FFLAGS = -O2 -ffixed-line-length-none -march=native -mtune=native -malign-double -fno-align-commons -D$(OS)
  else
    FFLAGS = -O2 -ffixed-line-length-none -malign-double -fno-align-commons -D$(OS)
  endif  
endif
IFLAGS = -I../include -I../modules

# needed in sub-make processes
export IFLAGS FC FFLAGS

LDIR = -L./lib
LIBS = -Wl,--start-group $(addprefix -l,$(SUBDIRS)) -Wl,--end-group -lkmod -lm

CSRC = sys_keyin.c

fSRC = koko.f cmder.f names.f libload.f loadgrida.f saverest.f
FSRC = kods.F

COBJ = $(CSRC:.c=.o)
FOBJ = $(FSRC:.F=.o) $(fSRC:.f=.o) 

# targets
all: buildinfo modules libraries koko

.PHONY: modules
modules:
	make -C modules

.PHONY: libraries $(SUBDIRS)
libraries: $(SUBDIRS)
$(SUBDIRS):
	make -C $@

koko: buildinfo.inc $(COBJ) $(FOBJ)
	$(FC) -o koko $(FOBJ) $(COBJ) $(LDIR) $(LIBS)

.PHONY: buildinfo
buildinfo:
	./buildinfo

%.o: %.f
	$(FC) -c $(FFLAGS) -I./include -I./modules $<
	
%.o: %.F
	$(FC) -c $(FFLAGS) -I./include -I./modules $<
	
%.o: %.c
	$(CC) -c $(CFLAGS) $<


clean:
	rm -f *.o *~ koko buildinfo.inc
	rm -f ./lib/lib*.a
	make -C modules clean
	for d in $(SUBDIRS) ; do make -C $$d clean ; done
	
install:
	install -m 755 koko /usr/local/bin

