/* sys_keyin.c  This version works on _most_ Unix platforms
Fortran calls:
CALL SYS_KEYSET(1)   to set single-keystroke mode
CALL SYS_KEYIN(KEY)  to get integer ASCII code for next key-stroke
e.g. 32 for space, 97 for "a" etc.
(Integer rather than character to cope with
control characters etc.)
CALL SYS_KEYSET(0)   to restore normal input mode
Author:  Clive Page,  cgp@le.ac.uk, 1994-JUL-13
*/

#include <stdio.h>
#include <termios.h>

int sys_keyset_(int *mode)
{
static struct termios termattr,saveattr;
//  static tcflag_t save_flags;

if(*mode != 0)
{
tcgetattr(0,&termattr);
saveattr=termattr;
termattr.c_lflag&=~(ICANON);
termattr.c_lflag&=~(ECHO);
termattr.c_cc[VMIN] = 1;
termattr.c_cc[VTIME] = 0;
tcsetattr(0,TCSADRAIN,&termattr);
}
else
{
tcsetattr(0,TCSADRAIN,&saveattr);
}
return 0;
}


int sys_keyin_(int *nextch)
{
*nextch = getchar();
return 0;
}

