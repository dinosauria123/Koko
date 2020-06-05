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

MODULE rgb

  ! Subroutines for RGB color value lookup. The lookup is performed
  ! with a static gperf hash function.
  
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: rgbval, rgbhex

  INTERFACE

     !----------------------------------------------------------
     ! Returns the RGB values corresponding to a color name
     !
     ! INPUT
     ! colorname :  string with a color name
     ! lcn :        number of characters in the color name
     !
     ! OUTPUT
     ! R,G,B :      values of the color channels in the range 0 .. 255
     !
     SUBROUTINE c_rgbval( colorname, lcn, R, G, B ) BIND(C)
       USE, INTRINSIC                     :: ISO_C_BINDING
       CHARACTER(KIND=c_char), INTENT(IN) :: colorname(*)
       INTEGER(c_int), VALUE, INTENT(IN)  :: lcn
       INTEGER(c_int), INTENT(OUT)        :: R, G, B
     END SUBROUTINE c_rgbval

     
     !----------------------------------------------------------
     ! Returns the RGB values corresponding to a color name
     ! in the form of a RGB hex string
     !
     ! INPUT
     ! colorname : string with a color name
     ! lcn :       length of the color name (NOT Fortran string)
     !
     ! OUTPUT
     ! hexstring : string with the RGB hex values
     ! lhs :       length of the Fortran string
     !
     SUBROUTINE c_rgbhex( colorname, lcn, hexstring, lhs ) BIND(C)
       USE, INTRINSIC                      :: ISO_C_BINDING
       CHARACTER(KIND=c_char), INTENT(IN)  :: colorname(*)
       INTEGER(c_int), VALUE, INTENT(IN)   :: lcn
       CHARACTER(KIND=c_char), INTENT(OUT) :: hexstring(*)
       INTEGER(c_int), VALUE, INTENT(IN)   :: lhs
     END SUBROUTINE c_rgbhex

  END INTERFACE


CONTAINS

  !----------------------------------------------------------
  ! Returns the RGB values corresponding to a color name
  !
  ! INPUT
  ! colorname :  string with a color name (see Koko_colors.txt)
  !
  ! OUTPUT
  ! R,G,B :      values of RGB color channels in the range 0 .. 255
  !
  SUBROUTINE rgbval(colorname, R, G, B)

    CHARACTER(LEN=*), INTENT(IN) :: colorname
    INTEGER, INTENT(OUT)         :: R, G, B
    
    CALL c_rgbval(colorname, LEN_TRIM(colorname), R, G, B)
    
  END SUBROUTINE rgbval


  !----------------------------------------------------------
  ! Returns the RGB values corresponding to a color name
  ! encoded as a RGB hex string
  !
  ! INPUT
  ! colorname : string with a color name
  ! lcn :       length of the color name (NOT Fortran string)
  !
  ! OUTPUT
  ! hexstring : string with the RGB hex values
  ! lhs :       length of the Fortran string
  SUBROUTINE rgbhex(colorname, hexstring)

    CHARACTER(LEN=*), INTENT(IN)  :: colorname
    CHARACTER(LEN=*), INTENT(OUT) :: hexstring

    CALL c_rgbhex(colorname, LEN_TRIM(colorname), hexstring, LEN(hexstring))
    
  END SUBROUTINE rgbhex
  
END MODULE rgb
