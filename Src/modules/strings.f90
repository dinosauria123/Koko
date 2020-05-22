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

MODULE strings

  ! Some C-style string handling functions for Fortran 90
  
  IMPLICIT NONE

  ! Interfaces to C library string functions
  
  INTERFACE

     !----------------------------------------------------------
     ! Change the input character into upper case using the
     ! default locale using the C library function "toupper"
     !
     ! INPUT
     ! ch :  a character
     !
     ! FUNCTION OUTPUT
     !       the corresponding upper case character
     !
     FUNCTION c_toupper( ch ) BIND(C)
       USE, INTRINSIC                       :: ISO_C_BINDING
       CHARACTER(c_char), INTENT(IN), VALUE :: ch
       CHARACTER(c_char)                    :: c_toupper
     END FUNCTION c_toupper
     
     !----------------------------------------------------------
     ! Change the input character into lower case using the
     ! default locale using the C library function "tolower"
     !
     ! INPUT
     ! ch :  a character
     !
     ! FUNCTION OUTPUT
     !       the corresponding lower case character
     !
     FUNCTION c_tolower( ch ) BIND(C)
       USE, INTRINSIC                       :: ISO_C_BINDING
       CHARACTER(c_char), INTENT(IN), VALUE :: ch
       CHARACTER(c_char)                    :: c_tolower
     END FUNCTION c_tolower

  END INTERFACE

CONTAINS
  
  !------------------------------------------------------
  ! Changes all characters in a string to upper case
  ! without assuming a particular character set or a
  ! particular locale. Protects parts of the input string
  ! enclosed in single quotes.
  !
  ! INPUT/OUTPUT
  ! str     :  a character string
  !
  SUBROUTINE to_upper(str)

    CHARACTER(LEN=*), INTENT(INOUT) :: str
    LOGICAL                         :: protect
    INTEGER                         :: k, n
    CHARACTER                       :: ch

    n = 1
    protect = .FALSE.
    
    DO k = 1, LEN_TRIM(str)
       
       ch = str(k:k)

       ! turn on/off protection when a quote is met
       IF (ch == "'") THEN
          protect = .NOT. protect
          CYCLE
       END IF
       
       IF ( protect ) THEN
          str(n:n) = ch
       ELSE
          str(n:n) = c_toupper( ch )
       END IF

       n = n + 1
       
    END DO

    str(n:LEN(str)) = ' '  ! clear string tail
    
  END SUBROUTINE to_upper


  !------------------------------------------------------
  ! Changes all characters in a string to lower case
  ! without assuming a particular character set or a
  ! particular locale. Protects parts of the input string
  ! enclosed in single quotes.
  !
  ! INPUT/OUTPUT
  ! str     :  a character string
  !
  SUBROUTINE to_lower(str)

    CHARACTER(LEN=*), INTENT(INOUT) :: str
    LOGICAL                         :: protect
    INTEGER                         :: k, n
    CHARACTER                       :: ch

    n = 1
    protect = .FALSE.
    
    DO k = 1, LEN_TRIM(str)
       
       ch = str(k:k)

       ! turn on/off protection when a quote is met
       IF (ch == "'") THEN
          protect = .NOT. protect
          CYCLE
       END IF
       
       IF ( protect ) THEN
          str(n:n) = ch
       ELSE
          str(n:n) = c_tolower( ch )
       END IF

       n = n + 1
       
    END DO

    str(n:LEN(str)) = ' '  ! clear string tail
    
  END SUBROUTINE to_lower
  
END MODULE strings
