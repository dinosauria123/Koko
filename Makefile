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
	@echo
	@echo "*************************************************************"
	@echo
	@echo "It's all done !"
	@echo
	@echo "Now install the programs with: make install  (superuser only)"
	@echo "or see INSTALL.m for installation alternatives."
	@echo "Remove object files etc. with : make clean"
	@echo
	@echo "*************************************************************"
	@echo

# install executables and system-wide config file
install: install-exec install-conf

# install executable files only
install-exec:
	@echo "Installing executable files ..."
	@echo "==============================="
	make -C ./Src install
	make -C ./QtGui -f MakeGui install
	cp koko.desktop /usr/share/applications/koko.desktop
	cp ./QtGui/images/koko.png /usr/share/icons/hicolor/512x512/apps/koko.png
	update-desktop-database

# install system-wide configuration file
install-conf:
	@echo "Installing system-wide configuration file"
	@echo "========================================="
	install --backup=numbered -m 644 ./kokorc /etc

# install data
install-data:
	mkdir -p $(DATAPREFIX)/KODS
	cd ./Libs && cp -R * $(DATAPREFIX)/KODS

# clean up
clean:
	make -C ./Src clean
	make -C ./QtGui -f MakeGui clean
