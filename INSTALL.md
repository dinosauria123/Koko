Koko ODS Installation from Source
=================================

While it is our goal to make KODS platform independent, these
installation instructions currently apply to Linux only.

Prerequisites
-------------

* A  **Fortran 95** compiler such as gfortran.
* An **ANSI C** compiler such as gcc.
* The **Make** tool, typically gmake on Linux
* The **gnuplot** plotting software.
  (<https://sourceforge.net/projects/gnuplot>), which is included with
  all Linux distributions.
* For the graphical user interface an installation of the
  cross-platform development framework **Qt5** (<https://www.qt.io), with
  development libraries, is required. The Qt libraries are included
  with all Linux distributions.


Building Koko
-------------

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

Koko must be installed as super-user (or using sudo) with the command

    make install

which installs the executables 'koko-cli' and 'koko-gui' in
'/usr/local/bin' and the program data in the directory
'/usr/local/KODS'. When a directory prefix other than '/usr/local' is
desired, e.g. '/opt', then the installation directory can be changed
by specifying the directory prefix during installation:

    make PREFIX=/opt install


Setting up the KODS Data Directories
------------------------------------

Koko was originally developed for single-user MS-DOS and Windows
computers, and it (still) sits uneasily with modern operating
systems. For example, the directory tree containing program data,
e.g. glass data, also contains directories that must be
writable. Program data for Koko must be installed by every user into a
user-writable directory. This can be done even without compiling
Koko simply by executing

    make install-data

in the Koko top level directory. This will create a directory named
"KODS" in the home directory (if it does not exist) and then the
program data will be copied into this directory. Should a different
location for the program data be desired, the data can be installed
with

    make DATAPREFIX=~/some/directory install-data


Configuring Koko
----------------

When the program data for Koko are stored in a non-standard location,
this location must be defined in the Koko configuration file. When
Koko is started, the program reads a configuration file named
".kokorc" in a user's home directory, which is defined in the
environment variable HOME in unix, and in the environment variables
HOMEDRIVE and HOMEDIR in Windows. An example configuration file can be
found in the top level directory of the Koko source distribution. The
configuration file has the following form:

<pre>
; Configuration file example for Koko
;
[directories]
    home = /HOME/ulfg/KODS
    temp = /tmp

[graphics]
    viewer = /usr/bin/gnuplot
</pre>

In the configuration file the location of the Koko data directory
("home" in the configuration file), and a directory for temporary data
can be defined. The viewer for graphical output from Koko can also be
specified in the configuration file. Comment lines beginning with ;
are ignored in the configuration file.

The configuration can also be changed temporarily by specifiying the
home directory on the Koko command line. For more details type:

<pre>
koko-cli --help
</pre>

Building the Graphical User Interface Separately
------------------------------------------------

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
