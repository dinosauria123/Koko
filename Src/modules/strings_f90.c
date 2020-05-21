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

// Provides access to string functions from the C standard library that
// provide functionality not available in Fortran 90+
//
// Ulf GRIESMANN, May 2020

#include <ctype.h>


//--- Prototypes ---------------------------------------------------------

char c_toupper( char );  // case conversion
char c_tolower( char );


//--- Case conversion functions ------------------------------------------
//
// these functions perform the implicit char <--> int typecasting that
// cannot be done in Fortran 90

char
c_toupper( char ch )
{
   return toupper(ch);
}


char
c_tolower( char ch )
{
   return tolower(ch);
}
