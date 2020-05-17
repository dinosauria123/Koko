////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020 The Koko Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution
//
// This file is part of Koko.
//
// Koko is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Koko is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Koko; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

// Provides a Fortran 90 - callable interface to the functions of the
// linenoise library for command line editing in a VT100 terminal.
//
// Ulf GRIESMANN, May 2020

#include <linenoise.h>
#include <stdlib.h>
#include <string.h>

#define PROMPT_LEN 32  // max number of characters in prompt
#define FNAME_LEN 256  // max file name length

//--- Prototypes ---------------------------------------------------------

void nextline( const char*, const int, char*, int );
void loadhistory( const char *, int );
void savehistory( const char *, int );

//--- Interface functions ------------------------------------------------

// Prompts a user and returns the user response
//
// INPUT
// prompt :   pointer to character string with prompt
// ncprs :    number of characters in prompt string
// lenrs :    (Fortran) length of the response string
//
// OUTPUT
// response : character string with user response
//
void
nextline( const char* prompt, const int ncprs, char *response, int lenrs ) {

   char tmp_prompt[PROMPT_LEN];
   int nc;
   char *tmp_response;

   // copy Fortran prompt string to C string
   if ( ncprs > PROMPT_LEN - 1 ) {
      nc = PROMPT_LEN - 1;
   }
   else {
      nc = ncprs;
   }
   strncpy(tmp_prompt, prompt, nc);
   tmp_prompt[nc] = '\0';

   // prompt the user and return the answer
   tmp_response = linenoise(tmp_prompt);

   // copy response to calling subroutine
   nc = strlen(tmp_response);
   strncpy(response, tmp_response, nc);
   if (nc < lenrs) {
      memset(response+nc, ' ', lenrs-nc);
   }

   // add response to command history
   linenoiseHistoryAdd(tmp_response);

   // free tmp result buffer
   free(tmp_response);
}


//------------------------------------------------------------------------

void
loadhistory( const char *fname, int ncs ) {

   char cfname[FNAME_LEN];
   
   // make C-style file name string
   strncpy(cfname, fname, ncs);
   cfname[ncs] = '\0';

   linenoiseHistoryLoad(cfname);  
}


//------------------------------------------------------------------------

void
savehistory( const char *fname, int ncs ) {

   char cfname[FNAME_LEN];
   
   // make C-style file name string
   strncpy(cfname, fname, ncs);
   cfname[ncs] = '\0';

   linenoiseHistorySave(cfname);
}

