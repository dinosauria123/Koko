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

MODULE gnuplot

  ! Interface module to the gnuplot package. Provides data visualization
  ! functionality and related data structures for Koko.
  !
  ! The Koko plotter plots data on a canvas with 10000 x 7000 pixels
  ! X coordinate: ix in [0,10000]
  ! Y coordinate: iy in [0,7000]

  USE kokodata
  USE opsys
  USE rgb
  USE dictionary

  IMPLICIT NONE
  
  PRIVATE
  PUBLIC plot_line, reset_pens
  

  ! The plotter is a dictionary of pens with different colors. Pens
  ! are added to the plotter as needed when a plot is created. The
  ! only characteristic of the pen that needs to be stored is the
  ! i/o unit of the file associated with it.
  TYPE(dict) :: plotter          ! dictionary of plotter pens

  
CONTAINS

  !----------------------------------------------------------  
  ! Plots a line segment from the current pen position to a
  ! new pen position in the global canvas coordinate system.
  !
  ! INPUT
  ! ix, iy :  coordinates of the next point on the canvas
  ! cont :    if cont == 1, the point (ix,iy) is the next
  !           point of a multi-segment line. Otherwise,
  !           (ix,iy) is the start of a new line
  ! pencolor: 24-bit color of the plotting pen
  !
  SUBROUTINE plot_line(ix,iy, cont, pencolor)

    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: ix,iy
    INTEGER, INTENT(IN) :: cont
    INTEGER, INTENT(IN) :: pencolor
    
    CHARACTER(LEN=256)  :: kods_home, pen_fullname
    CHARACTER(LEN=16)   :: pen_id
    CHARACTER(LEN=32)   :: pen_name
    INTEGER             :: iounit
    
    ! convert 24-bit color to hex string #RRGGBB
    CALL rgbint_to_hex(pencolor, pen_id)

    ! check if a pen with this color is already in use
    IF ( .NOT. exists(plotter, TRIM(pen_id)) ) THEN

       ! new pen data file with name kokoRRGGBB.gpl
       pen_name = "koko"//pen_id(2:7)//".gpl" ! don't use '#'
       CALL get_kods_home(kods_home)
       CALL dir_path_append(kods_home,"gnuplot",pen_fullname)
       CALL dir_path_append(pen_fullname, pen_name, pen_fullname)

       ! create the plot data file for this pen
       OPEN(NEWUNIT=iounit,FILE=TRIM(pen_fullname),STATUS='new')

       ! save the io unit of this pen for future use
       CALL insert_or_assign(plotter, TRIM(pen_id), iounit)
    ELSE
       iounit = get_val(plotter, TRIM(pen_id)) ! use an existing pen
    END IF

    ! now use the pen to plot the line
    IF (cont /= 1) THEN
       WRITE (iounit, *)
    END IF
    WRITE (iounit, "(2I5)") ix, iy
    
  END SUBROUTINE plot_line


  !----------------------------------------------------------  
  ! Remove all active plotter pens from the plotter
  !
  SUBROUTINE reset_pens()

    CHARACTER(LEN=16)  :: pen_id

    DO
       ! exit when no pens are left in the plotter
       IF (get_size(plotter) == 0) EXIT

       ! get the next pen and remove it from the plotter
       pen_id = get_kth_key(plotter, 1)
       CALL remove(plotter, TRIM(pen_id))

    END DO
    
  END SUBROUTINE reset_pens
  
END MODULE gnuplot
