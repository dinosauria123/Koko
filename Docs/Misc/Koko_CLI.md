
The Koko Command Line Interface (CLI)
=====================================

Koko's command line interface is based on a modified
[linenoise-ng](https://github.com/arangodb/linenoise-ng) library,
which provides command line editing and command history recall
and command completion functionality for Koko.


Command line editing
--------------------
linenoise emulates the
[GNU Readline](https://en.wikipedia.org/wiki/GNU_Readline) library. In
addition to the Emacs-style commands, the *End*, *Home*, *Left arrow*,
and *Right arrow* keys may be used to navigate within the command line.


Command history
---------------
On exit, Koko stores the last 250 commands in the file
*(user home directory)/.koko_history*. At the beginning of the next Koko
session, the command history file is read and users can recall
previously typed commands using the *Arrow up* key (or CTRL-p). The
*Arrow down* key (or CTRL-n) scrolls towards more recent commands in the
history. 


Command completion
------------------
Previously typed commands don't have to re-typed in full. When the
*Tab* key is pressed after the beginning of a command string is typed,
the command history is searched for a previous command that begins
with the same partial command. When the command history contains more
than one command with the same initial string, the *Tab* key matches
them one-by-one, going back in the command history.


Command case protection
-----------------------
KOKO LIKES TO BARK. Most program output is in upper case, and all user
input is internally converted to upper case before it is processed.
This can lead to problems when the user input is a file name and the
computer Koko is running on uses a case sensitive file system. An
example is the **lensloc** command, which can be used to define the
directory from which lens files (.koko or .prg) are to read. Single
quotes can be used to protect parts or all of a command argument from
conversion to upper case. For example, the command
<pre>
    lensloc '/HOME/ulfg/Raytrace/Examples/Newtonian'
</pre>
can be used to change the lens directory on Linux or macOS systems.


Command prompt color
--------------------
The color of the Koko command prompt can be configured, which can make
it easier to visually associate commands and their output in the
terminal window. The default prompt color is black. The prompt color
can be changed either in the system wide configuration file *kokorc*
or the user specific configuration file
*(user home directory)/.kokorc*. The configuration file must contain
a section
<pre>
[cli]
    promptcolor = 4
</pre>
which sets the [8-bit color code](https://en.wikipedia.org/wiki/ANSI_escape_code)
for the prompt color. The 8-bit color codes permit 256 colors, the
standard colors are given in the following table. 


 | Color          | 8-bit ANSI code |
 | :------------- | :-------------- |
 | Black          | 0               |
 | Red            | 1               |
 | Green          | 2               |
 | Yellow         | 3               |
 | Blue           | 4               |
 | Magenta        | 5               |
 | Cyan           | 6               |
 | Light grey     | 7               |
 | Dark grey      | 8               |
 | Bright red     | 9               |
 | Bright green   | 10              |
 | Bright yellow  | 11              |
 | Bright blue    | 12              |
 | Bright magenta | 13              |
 | Bright cyan    | 14              |
 | White          | 15              |
