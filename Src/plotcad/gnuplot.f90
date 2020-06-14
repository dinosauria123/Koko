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

  USE dictionary
  USE rgb
  USE opsys
  

  ! Defines a pen object for the plotter
  TYPE, ALLOCATABLE     :: plot_pen
     INTEGER            :: color ! 24-bit color of the pen
     CHARACTER(LEN=256) :: file  ! full name of plotter data file
     INTEGER            :: unit  ! io unit associated with the file
  END TYPE plot_pen

  
  ! The plotter is a dictionary of pens with different colors. Pens get
  ! added to the plotter as needed when a plot is created.
  dict :: gnu_plotter            ! dictionary of plotter pens

  
CONTAINS

  !----------------------------------------------------------  
  ! Plots a line segment from the current pen position to a
  ! new pen position
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

    INCLUDE 'datmai.inc' ! needed for the KODS HOME directory :-(
    
    ! plotter pen to use for the line
    plot_pen          :: pen
    CHARACTER(LEN=16) :: pen_id, pen_name
    CHARACTER(LEN=256):: pen_fullname
    INTEGER           :: iounit
    
    ! convert 24-bit color to hex string #RRGGBB
    CALL rgbint_to_hex(pencolor, pen_id)

    ! check if a pen with this color is already in use
    IF ( .NOT. exists(gnu_plotter, TRIM(pen_id)) ) THEN
       ALLOCATE(pen)  ! create a new pen

       ! new pen data file with name kokoRRGGBB.gpl
       pen_name = "koko"//pen_id(2:7)//".gpl" ! don't use '#'
       CALL dir_path_append(HOME,"gnuplot",pen_fullname)
       CALL dir_path_append(pen_fullname, pen_name, pen_fullname)

       ! create the plot data file for this pen
       OPEN(NEWUNIT=iounit,FILE=TRIM(pen_fullname),STATUS='new')

       ! save the pen for future use
       pen%color = pencolor
       pen%file  = pen_fullname
       pen%unit  = iounit
       CALL insert_or_assign(gnu_plotter, TRIM(pen_id), pen)
    ELSE
       pen = get_val(gnu_plotter, TRIM(pen_id)) ! use existing pen
    END IF

    ! now use the pen to plot the line
    IF (cont /= 1) THEN
       WRITE (pen%unit, *)
    END IF
    WRITE (pen%unit, "(2I5)") ix, iy
    
  END SUBROUTINE plot_line

END MODULE gnuplot
