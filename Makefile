#
# Simple top-level make file for Koko
#

# check installation prefix
ifndef PREFIX
  PREFIX = /usr/local
endif

ifndef DATAPREFIX
  DATAPREFIX = ${HOME}
endif	     

export PREFIX

# check variables for sub-makes
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

# install executables and system-wide config file
install:
	make -C ./Src install
	make -C ./QtGui -f MakeGui install
	install -m 644 ./kokorc /etc

# install data
install-data:
	mkdir -p $(DATAPREFIX)/KODS
	cd ./Libs && cp -R * $(DATAPREFIX)/KODS

# clean up
clean:
	make -C ./Src clean
	make -C ./QtGui -f MakeGui clean
