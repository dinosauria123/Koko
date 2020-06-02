Koko ODS Installation from Source
=================================

While it is our goal to make KODS platform independent, these
installation instructions currently apply to Linux only.

Prerequisites
-------------

* A  modern **Fortran** compiler (gfortran).
* A **C++** compiler (gcc).
* The **Make** tool, typically gmake on Linux
* The **gnuplot** cross-platform plotting software version 5 or later.
  (<https://sourceforge.net/projects/gnuplot>)
* For the graphical user interface an installation of the
  cross-platform development framework **Qt5** (<https://www.qt.io), with
  development libraries, is required.
* A text editor (vi, jed, nano, ...)


Building Koko
-------------

First download the sourcecode and unpack the archive or clone the
github repository.  In the top level directory ./Koko that holds the
source code (or ./Koko-master if the latest version from github was
downloaded), type

    make

at the shell prompt. This will build a version of Koko without
debugging information. The executable file is called **koko-cli**, the
**koko-gui** is the executable of the GUI. Debugging information is
included in the executable files when make is called as follows:

    make DEBUG=true

Finally, Koko can also be compiled with optimizations specific to
the computer the software is being compiled on using the make
command

    make NATIVE=true

Koko can be installed as super-user (or using sudo) with the command

    make install

which installs the executables **koko-cli** and **koko-gui** in
'/usr/local/bin' and copies the system-wide configuration file
**kokorc** to **/etc**. When the system-wide configuration file exists
it will be backed up.  Should a directory prefix other than
**/usr/local** be desired, e.g. **/opt**, or even a user's home
directory, the installation directory can be changed by specifying the
directory prefix during installation:

    make PREFIX=~/bin install

The installation command

    make install-exec

only installs the executable files but does not overwrite an existing
system-wide configuration file. The installation directory PREFIX/bin
should be in the search path.


Setting up the KODS Data Directories
------------------------------------

Koko was originally developed for single-user MS-DOS and Windows
computers, and it (still) sits uneasily with modern operating
systems. For example, the directory tree containing program data,
e.g. glass data, also contains directories that must be
writable. Program data for Koko must therefore be installed by every
user into a user-writable directory. This can be done even without
compiling Koko simply by executing

    make install-data

**as a normal user, not superuser** in the Koko top level
directory. This will create a directory named "KODS" in a user's home
directory (if it does not exist) and the program data will be
copied into this directory. Should a different location for the
program data be desired, the data can be installed with

    make DATAPREFIX=~/some/directory install-data


Configuring Koko
----------------

When the program data for Koko are stored in a non-standard location,
this location must be defined in the Koko configuration file. When
Koko is started, the program first reads the system-wide configuration
file **kokorc**. The location of this file depends on the operating
system. On Unix-like systems, the system-wide configuration file is
located in **/etc**. On Windows it will be in the application data
folder. The system-wide configuration file can be installed with

    make install-conf

after the Koko executables have been built. **Remember to edit the
configuration file to match the system setup before installing it.**
Once the system-wide configuration is parsed, a configuration file
named **.kokorc** (dot kokorc) in a user's home directory is
read. This enables every user to temporarily or permanently override
system-wide defaults. The system-wide configuration file can also be
absent. In this case only the per-user configuration file is parsed.
An example configuration file can be found in the top level directory
of the Koko source distribution. The configuration file has the
following form:

<pre>
; Minimal configuration file example for Koko
;
[directories]
    home = /HOME/ulfg/KODS
    temp = /tmp

[graphics]
    viewer = /usr/bin/gnuplot
</pre>

In the configuration file the location of the Koko data directory
(**home** in the configuration file), and a directory for temporary data
can be defined. The viewer for graphical output from Koko can also be
specified in the configuration file. Comment lines beginning with ;
are ignored in the configuration file. The example configuration in
the source distribution contains all available configuration options.

The configuration can also be changed temporarily by specifiying home
and temporary directories on the Koko command line. For more details
type:

<pre>
koko-cli  -h
</pre>


Building the Graphical User Interface Separately
------------------------------------------------

Change to the **./QtGui** directory and create the build files by
issuing the

    qmake

command at the command prompt, followed by

    make

and, which creates the executable file **koko-gui**. Finally,

    make install

installs the executable file in **/usr/local/bin** (default).


Happy Designing !
-----------------
