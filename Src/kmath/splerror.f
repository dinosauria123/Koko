      subroutine splerror

c     prints an error message when spline interpolation has
c     failed and returns to the command prompt

      implicit none
      include 'datmai.inc'
  
      outlyne = 'spline: error calculating interpolation spline'
      call showit(1)
      call macfal
  
      end

