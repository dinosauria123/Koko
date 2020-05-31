! Copyright (C) 2020 The Koko Project Developers
!
! See the file COPYRIGHT.md in the top-level directory of this
! distribution
!
! This file is part of Koko.
!
! Koko is free software: you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Koko is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Koko; see the file COPYING.  If not, see
! <https://www.gnu.org/licenses/>.

MODULE commandline

  ! Provides command line input functions such as command line editing,
  ! command recall, and command history through an interface to the
  ! platform-independent linenoise-ng library:
  ! https://github.com/arangodb/linenoise-ng
  !
  ! The subroutines defined in this module call C-functions in the
  ! file "linenoise_interface.c", which make it easier to access the
  ! linenoise library from Fortran.
  !
  ! Ulf GRIESMANN, May 2020

  IMPLICIT NONE

  INTERFACE
     
     !----------------------------------------------------------
     ! Display a prompt and return the user response
     !
     ! INPUT
     ! prompt :   a character string with a prompt (<= 32 characters)
     ! ncprs :    the number of characters in the prompt string
     ! lenrs :    the length of the character array for the response
     !
     ! OUTPUT
     ! response : a character string with the user response
     !
     SUBROUTINE nextline( prompt, ncprs, response, lenrs ) BIND(C)
       USE, INTRINSIC                      :: ISO_C_BINDING
       CHARACTER(KIND=c_char), INTENT(IN)  :: prompt(*)
       INTEGER(c_int), VALUE, INTENT(IN)   :: ncprs
       CHARACTER(KIND=c_char), INTENT(OUT) :: response(*)
       INTEGER(c_int), VALUE, INTENT(IN)   :: lenrs
     END SUBROUTINE nextline


     !----------------------------------------------------------
     ! Set the color of the command prompt
     ! See: https://en.wikipedia.org/wiki/ANSI_escape_code
     !
     ! INPUT
     ! acc :  an integer with the ANSI color code.
     !          30 : black
     !          31 : red
     !          32 : green
     !          34 : blue
     !
     SUBROUTINE promptcolor( acc ) BIND(C)
       USE, INTRINSIC                     :: ISO_C_BINDING
       INTEGER(c_int), VALUE, INTENT(IN)  :: acc
     END SUBROUTINE promptcolor


     !----------------------------------------------------------
     ! Load the command history from a file
     !
     ! INPUT
     ! fname :  a character string with the history file name
     ! ncs :    number of characters in the history file name
     !
     SUBROUTINE loadhistory( fname, ncs ) BIND(C)
       USE, INTRINSIC                     :: ISO_C_BINDING
       CHARACTER(KIND=c_char), INTENT(IN) :: fname(*)
       INTEGER(c_int), VALUE, INTENT(IN)  :: ncs
     END SUBROUTINE loadhistory

     
     !----------------------------------------------------------
     ! Save the command history to a file
     !
     ! INPUT
     ! fname :  a character string with the history file name
     ! ncs :    number of characters in the history file name
     !
     SUBROUTINE savehistory( fname, ncs ) BIND(C)
       USE, INTRINSIC                     :: ISO_C_BINDING
       CHARACTER(KIND=c_char), INTENT(IN) :: fname(*)
       INTEGER(c_int), VALUE, INTENT(IN)  :: ncs
     END SUBROUTINE savehistory

  END INTERFACE

END MODULE commandline
