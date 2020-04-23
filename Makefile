#
# Simple top-level make file for Koko
#

# check installation prefix
ifndef PREFIX
  PREFIX = /usr/local
endif
export PREFIX

# check variables for sub-makes
ifdef JOBS
  export JOBS
endif
ifdef NATIVE
  export NATIVE
endif
ifdef DEBUG
  export DEBUG
endif
ifdef OS
  export OS
endif

.PHONY: all clean install

# build all the components
all:
	make -C ./Src
	make -C ./QtGui -f MakeGui koko-gui

# install everything
install:
	make -C ./Src install
	make -C ./QtGui -f MakeGui install
	mkdir -p $(PREFIX)/KODS
	cd ./Libs && cp -R * $(PREFIX)/KODS

# clean up
clean:
	make -C ./Src clean
	make -C ./QtGui -f MakeGui clean
