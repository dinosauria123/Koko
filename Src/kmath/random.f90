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

MODULE random

  ! Provides (pseudo-) random number generators with
  ! uniform and normal probability distribution functions.
  ! These functions are based on the built-in random number
  ! generator of Fortran 90.
  !
  ! Ulf GRIESMANN, May 2020
  
    IMPLICIT NONE
    INTEGER, PARAMETER :: sp = KIND(1.0)
    INTEGER, PARAMETER :: dp = KIND(1.0d0)

    REAL (sp), PARAMETER :: pi_sp = 4.0_sp*ATAN2(1.0_sp,1.0_sp)
    REAL (dp), PARAMETER :: pi_dp = 4.0_sp*ATAN2(1.0_dp,1.0_dp)

CONTAINS
  
  FUNCTION srand_uniform()
    !
    ! Returns a single precision pseudo-random number
    ! in the interval [0,1) using the standard
    ! random number generator

    REAL (sp) :: srand_uniform
    REAL (sp) :: rn

    CALL random_number( rn )
    srand_uniform = rn

    RETURN

  END FUNCTION srand_uniform


  FUNCTION srand_normal()
    !
    ! srand_normal returns a unit pseudonormal
    !
    ! Discussion:
    !
    !   The standard normal probability distribution function (PDF) has
    !   mean 0 and standard deviation 1.
    !
    ! Licensing:
    !
    !   This code is distributed under the GNU LGPL license.
    !
    ! Modified:
    !
    !   06 August 2013
    !
    ! Author:
    !
    !   John Burkardt
    !
    ! Parameters:
    !
    !   Output, real (sp) srand_normal, a sample of the standard
    !   normal PDF.

    REAL (sp) :: srand_normal
    REAL (sp) :: r1, r2
    
    CALL random_number(r1)
    CALL random_number(r2)
    
    srand_normal = &
         SQRT ( - 2.0_sp * LOG ( r1 ) ) * COS ( 2.0_sp * pi_sp * r2 )
    
    RETURN

  END FUNCTION srand_normal
  

  FUNCTION drand_uniform()
    !
    ! Returns a double precision pseudo-random number
    ! in the interval [0,1) using the standard
    ! random number generator

    REAL (dp) :: drand_uniform
    REAL (dp) :: rn

    CALL random_number( rn )
    drand_uniform = rn

    RETURN

  END FUNCTION drand_uniform
  

  FUNCTION drand_normal()
    !
    ! drand_normal returns a double precision unit pseudo-normal.
    !
    ! Discussion:
    !
    !   The standard normal probability distribution function (PDF) has
    !   mean 0 and standard deviation 1.
    !
    ! Licensing:
    !
    !   This code is distributed under the GNU LGPL license.
    !
    ! Modified:
    !
    !   06 August 2013
    !
    ! Author:
    !
    !   John Burkardt
    !
    ! Parameters:
    !
    !   Output, real (dp) a normally distributed
    !   random value.

    REAL (dp) :: drand_normal
    REAL (dp) :: r1, r2

    CALL random_number(r1)
    CALL random_number(r2)
    
    drand_normal = &
         SQRT( - 2.0_dp * LOG ( r1 ) ) * COS ( 2.0_dp * pi_dp * r2 )
    
    RETURN

  END FUNCTION drand_normal
  
END MODULE random
