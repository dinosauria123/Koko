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
  PUBLIC :: rgbval, rgbhex, rgbint, rgbint_to_hex

  INTERFACE

     !----------------------------------------------------------
     ! Returns the RGB values corresponding to a color name
     !
     ! INPUT
     ! colorname :  string with a color name, or RGB hex string
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

     
     !----------------------------------------------------------
     ! Returns the RGB values corresponding to a color name
     ! encoded in a 24-bit integer
     !
     ! INPUT
     ! colorname : string with a color name, or RGB hex string
     ! lcn :       length of the color name (NOT Fortran string)
     !
     ! OUTPUT
     ! colint :    RGB values encoded as 24-bit integer
     !
     SUBROUTINE c_rgbint( colorname, lcn, colint ) BIND(C)
       USE, INTRINSIC                      :: ISO_C_BINDING
       CHARACTER(KIND=c_char), INTENT(IN)  :: colorname(*)
       INTEGER(c_int), VALUE, INTENT(IN)   :: lcn
       INTEGER(c_int), INTENT(OUT)         :: colint
     END SUBROUTINE c_rgbint     

  END INTERFACE


CONTAINS

  !----------------------------------------------------------
  ! Returns the RGB values corresponding to a color name
  !
  ! INPUT
  ! colorname :  string with a color name (see Koko_colors.txt)
  !              or a #RRGGBB hex string
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
  !
  ! OUTPUT
  ! hexstring : string with the RGB hex values
  !
  SUBROUTINE rgbhex(colorname, hexstring)

    CHARACTER(LEN=*), INTENT(IN)  :: colorname
    CHARACTER(LEN=*), INTENT(OUT) :: hexstring

    CALL c_rgbhex(colorname, LEN_TRIM(colorname), hexstring, LEN(hexstring))
    
  END SUBROUTINE rgbhex

  
  !----------------------------------------------------------
  ! Returns the RGB values corresponding to a color name
  ! encoded as a 24-bit integer number where the first
  ! byte contains the 8-bit value of R, the second byte
  ! the value of G, and the third byte the value of B
  !
  ! INPUT
  ! colorname : string with a color name,
  !             or a #RRGGBB hex string
  !
  ! OUTPUT
  ! colint :    24 bit integer 
  !
  SUBROUTINE rgbint(colorname, colint)

    CHARACTER(LEN=*), INTENT(IN)  :: colorname
    INTEGER, INTENT(OUT)          :: colint

    CALL c_rgbint(colorname, LEN_TRIM(colorname), colint)
    
  END SUBROUTINE rgbint

  
  !----------------------------------------------------------
  ! Converts a RGB triplet encoded as a 24-bit integer number
  ! into a hex string representation of the form #RRGGBB
  !
  ! INPUT
  ! colint :  24-bit integer encoding an 8-bit RGB triplet
  !
  ! OUTPUT
  ! hexstr :  RGB triplet encoded as a 7-character string #RRGGBB
  !
  SUBROUTINE rgbint_to_hex(colint, hexstr)

    INTEGER, INTENT(IN)           :: colint
    CHARACTER(LEN=*), INTENT(OUT) :: hexstr

    INTEGER :: r, g, b

    r =        IAND(colint,      255)
    g = ISHFT( IAND(colint,    65280),  -8)
    b = ISHFT( IAND(colint, 16711680), -16)

    WRITE (hexstr, "(A1,3Z2.2)") '#',r,g,b
    
  END SUBROUTINE rgbint_to_hex

END MODULE rgb
