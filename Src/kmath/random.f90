module random

  ! Provides (pseudo-) random number generators with
  ! uniform and normal probability distribution functions.
  ! These functions are based on the built-in random number
  ! generator of Fortran 90.
  !
  ! Ulf GRIESMANN, May 2020
  
    implicit none
    integer, parameter :: sp = kind(1.0)
    integer, parameter :: dp = kind(1.0d0)

    real (sp), parameter :: pi_sp = 4.0_sp*atan2(1.0_sp,1.0_sp)
    real (dp), parameter :: pi_dp = 4.0_sp*atan2(1.0_dp,1.0_dp)

contains

  
  function srand_uniform()
    !
    ! Returns a single precision pseudo-random number
    ! in the interval [0,1) using the standard
    ! random number generator

    real (sp) :: srand_uniform
    real (sp) :: rn

    call random_number( rn )
    srand_uniform = rn

    return

  end function srand_uniform


  function srand_normal()
    !
    !! srand_normal returns a unit pseudonormal
    !
    !  Discussion:
    !
    !    The standard normal probability distribution function (PDF) has
    !    mean 0 and standard deviation 1.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    06 August 2013
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Output, real (sp) srand_normal, a sample of the standard
    !    normal PDF.

    real (sp) :: srand_normal
    real (sp) :: r1, r2
    
    call random_number(r1)
    call random_number(r2)
    
    srand_normal = &
         sqrt ( - 2.0_sp * log ( r1 ) ) * cos ( 2.0_sp * pi_sp * r2 )
    
    return

  end function srand_normal
  

  function drand_uniform()
    !
    ! Returns a double precision pseudo-random number
    ! in the interval [0,1) using the standard
    ! random number generator

    real (dp) :: drand_uniform
    real (dp) :: rn

    call random_number( rn )
    drand_uniform = rn

    return

  end function drand_uniform
  

  function drand_normal()
    !
    !! drand_normal returns a double precision unit pseudo-normal.
    !
    !  Discussion:
    !
    !    The standard normal probability distribution function (PDF) has
    !    mean 0 and standard deviation 1.
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    06 August 2013
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    Output, real (dp) a normally distributed
    !    random value.

    real (dp) :: drand_normal
    real (dp) :: r1, r2

    call random_number(r1)
    call random_number(r2)
    
    drand_normal = &
         sqrt ( - 2.0_dp * log ( r1 ) ) * cos ( 2.0_dp * pi_dp * r2 )
    
    return

  end function drand_normal
  
end module random
