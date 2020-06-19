#
# Makefile for Fortran modules
#
# Ulf GRIESMANN, April 2020
#

IFLAGS = -I../include

# old modules from KDP
fSRC   = globals.f nssmod.f kokodata.f

# new modules
f90SRC = configfile.f90 getoptions.f90 commandline.f90 strings.f90 \
         rgb.f90 kokoconfig.f90 posix_regex.f90 gnuplot.f90

# modules that need preprocessing
F90SRC = opsys.F90 treap.F90 dictionary.F90

CSRC   = strings_f90.c rgb_f90.c

MODS = $(fSRC:.f=.mod) $(F90SRC:.F90=.mod) $(f90SRC:.f90=.mod)
FOBJ = $(fSRC:.f=.o) $(F90SRC:.F90=.o) $(f90SRC:.f90=.o)
COBJ = $(CSRC:.c=.o)

# targets
all: regex_f90.inc libkmod

libkmod: $(MODS) $(COBJ)
	ar rs libkmod.a $(FOBJ) $(COBJ)
	mv libkmod.a ../lib

%.mod: %.f
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.mod: %.f90
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.mod: %.F90
	$(FC) -c $(FFLAGS) $(IFLAGS) $<

%.o : %.c
	$(CC) -c $(CFLAGS) $<

regex_f90.inc:
	$(CC) -o regex_t_size regex_t_size.c
	./regex_t_size > regex_f90.inc

clean:
	rm -f *.mod *.o *~ regex_f90.inc regex_t_size
