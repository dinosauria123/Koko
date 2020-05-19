      subroutine splerror( ier )

c     prints an error message when spline interpolation has
c     failed and returns to the command prompt

      implicit none
      include 'datmai.inc'

      integer :: ier   ! error number

      select case ( ier )
         case ( 1 )
            outlyne = "spline error (1): number of knots is smaller than 2"
         case ( 2 )
            outlyne = "spline error (2): knots are not monotonic"
         case ( 3 )
            outlyne = "spline error (3): invalid boundary condition arguments"
         case default
            outlyne = "spline error: unknown error number"
      end select
      
      call showit(1)
      call macfal
  
      end subroutine
