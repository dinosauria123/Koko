Koko ODS Installation from Source
=================================

While it is our goal to make KODS platform independent, these
installation instructions currently apply to Linux only.

Prerequisites
-------------

* A __Fortran 90__ compiler such as gfortran.
* An __ANSI C__ compiler such as gcc.
* The Make tool, typically gmake on 
* The __editline__ library (<https://www.thrysoee.dk/editline>).
* The __gnuplot__ graphing software.
  (<https://sourceforge.net/projects/gnuplot>), which is included with
  all Linux distributions.
* For the graphical user interface an installation of the
  cross-platform development framework Qt5 (<https://www.qt.io), with
  development libraries, is required. The Qt libraries are included
  with all Linux distributions.


Building Koko with Command Line Interface
-----------------------------------------

In the top level directory ./Koko that holds the source code, at
the command shell prompt, type

    make

This will build a version of Koko without debugging information. The
executable file is called 'koko-cli', the GUI is started with
'koko-gui'. Debugging information is included in the executable file
when make is called as follows:

    make DEBUG=true

Finally, Koko can also be compiled with optimizations for the specific
of the computer the software is being compiled on using the make
command

    make NATIVE=true

In all cases the compile process can be speeded up by running multiple
compile jobs in parallel. The number of jobs can be specified on the
make command line as shown in the following example:

    make NATIVE=true JOBS=8
    
Koko must be installed as super-user (or using sudo) with the command

    make install

which installs the executables 'koko-cli' and 'koko-gui' in
'/usr/local/bin' and the program data in the directory
'/usr/local/KODS'. When a directory prefix other than '/usr/local' is
desired, e.g. '/opt', then the installation directory can be changed
by specifying the directory prefix during installation:

    make PREFIX=/opt install

The program data (Glass data, test plate data, macros, etc) are
installed with

    make installdata

or

    make PREFIX=/data_directory installdata

When an installation directory prefix other than '/usr/local' was
used, the location of the KODS data directory can be specified by
setting the value of the environment variable __KODS_HOME__ to the
data directory. For example, when the bash shell is used, the command

    export KODS_HOME=/home/user

can be added either to the system-wide file '/etc/bash.bashrc' or to
the file '.bashrc' in a user's home directory.  This not only allows
the installation of the program data in a non-default directory, but
it also allows every user to maintain and change their own data
directory.


Building the Graphical User Interface
-------------------------------------

Change to the __./QtGui__ directory and create the build files by
issuing the

    qmake

command at the command prompt, followed by

    make

and, which creates the executable file 'koko-gui'. Finally,

    make install

installs the executable file in '/usr/local/bin'.


Happy Designing !
-----------------
