C///////////////////////////////////////////////////////////////////////
C/
C/ Copyright (C) 2020 The Koko Project Developers
C/
C/ See the file COPYRIGHT.md in the top-level directory of this
C/ distribution
C/
C/ This file is part of Koko.
C/
C/ Koko is free software: you can redistribute it and/or modify it
C/ under the terms of the GNU General Public License as published by
C/ the Free Software Foundation, either version 3 of the License, or
C/ (at your option) any later version.
C/
C/ Koko is distributed in the hope that it will be useful, but
C/ WITHOUT ANY WARRANTY; without even the implied warranty of
C/ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C/ GNU General Public License for more details.
C/
C/ You should have received a copy of the GNU General Public License
C/ along with Koko; see the file COPYING.  If not, see
C/ <https://www.gnu.org/licenses/>.
C/
C///////////////////////////////////////////////////////////////////////

!
! Plotting interface to gnuplot
!

      SUBROUTINE datacolorssave(IX,IY,I3,IC)

          IMPLICIT NONE
          INTEGER, INTENT(IN) :: IX,IY ! plotter coordinates
          INTEGER, INTENT(IN) :: I3    ! pen state (1=no blank, 0=pen-up)
          INTEGER, INTENT(IN) :: IC    ! plot color (0=black,1=yellow,2=magenta,3=red,4=cyan)

          ! canvas size: x in [0,10000], y in [0,7000]
          ! color→unit mapping: 0→130(black), 1→115(yellow), 2→116(magenta),
          !                      3→117(red),   4→118(cyan)
          INTEGER :: i, u
          INTEGER, PARAMETER :: units(0:4) = (/130,115,116,117,118/)

          IF(IC.GE.0.AND.IC.LE.4) THEN
              ! Write data point: once for pen-up blank line if I3=0, once for coords
              DO i = 1, 2
                  IF(I3.EQ.0) WRITE(units(IC),'(A)') ' '
                  IF(I3.EQ.1) THEN
                      WRITE(units(IC),'(2I5)') IX,IY
                  ELSE
                      WRITE(units(IC),'(2I5)') IX,IY
                  END IF
              END DO
              FLUSH(units(IC))
          END IF

      END SUBROUTINE datacolorssave


      SUBROUTINE drawdatasave(I1,I2,I3,I4)

          implicit none
          integer I1,I2,I3,I4
          INTEGER :: u
C     unit lookup table for color routing: black=130, yellow=115, magenta=116, red=117, cyan=118
          INTEGER, PARAMETER :: units(0:4) = (/130,115,116,117,118/)

          IF(I4.LT.0.OR.I4.GT.4) RETURN
          ! Write data point: once for pen-up blank line if I3=0, once for coords
          IF(I3.EQ.0) WRITE(units(I4),'(A)') ' '
          WRITE(units(I4),'(2I5)') I1,I2
          FLUSH(units(I4))

            END SUBROUTINE drawdatasave


      SUBROUTINE drawdatasave2(I1,I2,I3,I4,I5,I6,I7,I8)

          USE GLOBALS
          IMPLICIT NONE
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8

          INCLUDE 'datmai.inc'
C     color->unit mapping: 0=black/130, 1=yellow/115, 2=magenta/116, 3=red/117, 4=cyan/118
          INTEGER, PARAMETER :: units(0:4) = (/130,115,116,117,118/)

          IF(I4.LT.0.OR.I4.GT.4) RETURN
C     Pen state I3: 0 = pen-up (move only, break the line), 1 = pen-down
C     (draw). Write a blank line on pen-up so gnuplot 'with lines' starts a
C     new segment instead of connecting from the previous point. We must
C     write EVERY point unconditionally (no range gating): gating drops
C     pen-up coordinates at the plot-frame edge, which makes gnuplot draw a
C     spurious connecting line across the gap ("doesn't bend down" artifact).
C     The gnuplot 'plot [0:10000] [0:7000]' range clips off-canvas points.
          IF(I3.EQ.0) WRITE(units(I4),'(A)') ' '
          WRITE(units(I4),'(2I5)') I1,I2
          FLUSH(units(I4))

          if (I4.eq.2) then
              if (I3.eq.1) then
                  write(131,'(2I5)') I1,I2    !breakblack
              else
                  write(131,*)
                  write(131,'(2I5)') I1,I2
              end if
          end if

      END SUBROUTINE drawdatasave2


      SUBROUTINE gnuplotlabel(IX,IY,label,I3,I4)

          USE kokoconfig
          USE gplblk_mod
        
          IMPLICIT NONE
          
          CHARACTER(LEN=*), INTENT(IN) :: label
          INTEGER, INTENT(IN)          :: IX,IY,I3,I4
          
          REAL              :: X,Y
          CHARACTER(LEN=32) :: font

          IF (GPL_BLK_START) THEN
              CALL drawcmd3_clear
              GPL_BLK_START = .FALSE.
          ELSE
          END IF

          ! font to be used
          CALL CFG_get(koko_cfg, "graphics%fontlrg", font)
          
          ! position of label
          X=REAL(IX)/10500.0+0.02
          Y=REAL(IY)/7350.0+0.03

          IF ((I4.EQ.2).AND.(I3.EQ.90)) THEN
              WRITE(150,'(A,A,A,F6.3,A1,F6.3,A2,A,A)') 'set label font "',TRIM(font),
     1'" center at screen ',REAL(IX)/10000.0,",",REAL(IY/7000.0)+0.01,
     2        ' "',TRIM(label),'" rotate by 90'

          ELSE IF (I4.EQ.2) THEN
              WRITE(150,'(A,A,A,F6.3,A1,F6.3,A2,A,A)') 'set label font "',TRIM(font),
     1'" center at screen ',REAL(IX)/10000.0,",",REAL(IY/7000.0)+0.01,
     2        ' "',TRIM(label),'"'

          ELSE
              WRITE(150,'(A,F6.3,A1,F6.3,A2,A,A)') 'set label at screen '
     1        ,X,",",Y,' "',TRIM(label),'"'

          END IF

      END SUBROUTINE gnuplotlabel


      SUBROUTINE contlabel(X0,Y0,label)

          USE gplblk_mod
          IMPLICIT NONE

          REAL, INTENT(IN)             :: X0,Y0
          CHARACTER(LEN=*), INTENT(IN) :: label

          REAL :: X,Y

          IF (GPL_BLK_START) THEN
              CALL drawcmd3_clear
              GPL_BLK_START = .FALSE.
          END IF

          X=REAL(300.0/10000.0*X0)
          Y=REAL(210.0/7000.0*Y0)

          WRITE(150,'(A,F6.3,A1,F6.3,A2,A,A)') 'set label at screen ',
     1    X,',',Y,' "',label,'"'

      END SUBROUTINE contlabel


      SUBROUTINE drawcmdsave

                USE opsys
                USE kokoconfig
                USE globals
                USE gplblk_mod
       
                INCLUDE 'datmai.inc'
                CHARACTER(LEN=256) :: file_a, file_b, file_out
                CHARACTER(LEN=32)  :: gpterm, gpfont
                LOGICAL :: gen_png
                LOGICAL :: OPEN150

                ! Reset the per-plot-block clear flag so the NEXT plot
                ! command starts from a clean body (overprint fix).
                GPL_BLK_START = .TRUE.

                ! name of script header file
                file_a = TRIM(HOME)
                CALL dir_path_append(file_a, "gnuplot", file_a)
                CALL dir_path_append(file_a, "drawcmd0.gpl", file_a)

                ! create new script header
                CALL CFG_get(koko_cfg, "graphics%fontsml",  gpfont)
                CALL CFG_get(koko_cfg, "graphics%terminal", gpterm)
                gen_png = GENERATE_PLOT_PNG
            
                OPEN (213, STATUS='replace', FILE=TRIM(file_a))
                IF (gen_png) THEN
                    WRITE(213,*) 'set terminal png size 1000,700 font "'//TRIM(gpfont)//'"'
                ELSE
                    WRITE(213,*) 'set terminal '//TRIM(gpterm)//' font "'//TRIM(gpfont)//'"'
                END IF
                WRITE(213,*) 'set noborder'
                WRITE(213,*) 'set nokey'
                WRITE(213,*) 'set notics'
C     Clear ALL previously-set labels. Koko writes labels with
C     number-less "set label ..." commands (see gnuplotlabel), which
C     gnuplot auto-numbers and ACCUMULATES across repeated "load"
C     calls inside a single (persistent) gnuplot session. Without this
C     reset, re-drawing a PSF/plot leaves the previous figure's text
C     overprinted on the new one. "unset label" (no number) removes
C     every label; drawcmd3.gpl (the body) is re-written from scratch
C     on every plot command, so this only drops the stale labels.
                WRITE(213,*) 'unset label'
                CLOSE(213)

          ! script body file
          file_b = TRIM(HOME)
          CALL dir_path_append(file_b, "gnuplot", file_b)
          CALL dir_path_append(file_b, "drawcmd3.gpl", file_b)

          ! name of complete script file
          file_out = TRIM(HOME)
          CALL dir_path_append(file_out, "gnuplot", file_out)
          CALL dir_path_append(file_out, "drawcmd.gpl", file_out)

          ! Flush any buffered gnuplot body (unit 150 / drawcmd3.gpl) so the
          ! cat-based append_files below sees the complete content.
          INQUIRE(UNIT=150, OPENED=OPEN150)
          IF(OPEN150) FLUSH(150)
          ! create full gnuplot script
          CALL append_files(file_a, file_b, file_out)

      END SUBROUTINE drawcmdsave


      SUBROUTINE drawcmdsave2

          USE opsys
          USE gplblk_mod
        
          INCLUDE 'datmai.inc'
          CHARACTER(LEN=256) :: file_a, file_b, file_out
          LOGICAL :: OPEN150

          ! Reset the per-plot-block clear flag so the NEXT plot command
          ! starts from a clean body (overprint fix).
          GPL_BLK_START = .TRUE.

          ! name of first script file
          file_a = TRIM(HOME)
          CALL dir_path_append(file_a, "gnuplot", file_a)
          CALL dir_path_append(file_a, "drawcmd3.gpl", file_a)

          ! name of second script file
          file_b = TRIM(HOME)
          CALL dir_path_append(file_b, "gnuplot", file_b)
          CALL dir_path_append(file_b, "plotcont.gpl", file_b)

          ! name of complete script file
          file_out = TRIM(HOME)
          CALL dir_path_append(file_out, "gnuplot", file_out)
          CALL dir_path_append(file_out, "drawcmd.gpl", file_out)

          ! Flush any buffered gnuplot body (unit 150 / drawcmd3.gpl) so the
          ! cat-based append_files below sees the complete content.
          INQUIRE(UNIT=150, OPENED=OPEN150)
          IF(OPEN150) FLUSH(150)
          ! create full gnuplot script
          CALL append_files(file_a, file_b, file_out)

      END SUBROUTINE drawcmdsave2


      SUBROUTINE setonecolors

          USE opsys
          USE gplblk_mod
          
          INCLUDE 'datmai.inc'

          CHARACTER(LEN=256) :: script
          CHARACTER(LEN=16)  :: lwstr



          CALL retrieve_linewidth(lwstr)

          ! name of plot scripts
          CALL dir_path_append(TRIM(HOME), 'gnuplot', script)
          CALL dir_path_append(script, "black.gpl", script)

#if defined(WINDOWS)
          call replace_slash(script)
#endif

          WRITE(150,*)'plot [0:10000] [0:7000] "'//
     &TRIM(script)//'" lc rgb "black" lw '//TRIM(lwstr)//' w l'
          WRITE(150,*) 'pause -1'

      END SUBROUTINE setonecolors

      
      SUBROUTINE setonecolors2

          USE opsys
          USE gplblk_mod
        
          INCLUDE 'datmai.inc'

          CHARACTER(LEN=256) :: script1, script2
          CHARACTER(LEN=16)  :: lwstr



          CALL retrieve_linewidth(lwstr)

          ! names of plot scripts
          CALL dir_path_append(TRIM(HOME), "gnuplot", script1)
          CALL dir_path_append(script1, "black.gpl", script1)

          CALL dir_path_append(TRIM(HOME), "gnuplot", script2)
          CALL dir_path_append(script2, "breakblack.gpl", script2)

#if defined(WINDOWS)
          call replace_slash(script1)
          call replace_slash(script2)
#endif						   
          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     &TRIM(script1)//'" lc rgb "black" lw '//TRIM(lwstr)//' w l,"'//
     &TRIM(script2)//'" with lines lt 0 lc rgb "black" lw 2'
          WRITE(150,*) 'pause -1'

      END SUBROUTINE setonecolors2


      SUBROUTINE settwocolors

          USE opsys
          USE gplblk_mod
        
          INCLUDE 'datmai.inc'

          CHARACTER(LEN=256) :: script1, script2
          CHARACTER(LEN=16)  :: lwstr



          CALL retrieve_linewidth(lwstr)

          ! names of plot scripts
          CALL dir_path_append(TRIM(HOME), "gnuplot", script1)
          CALL dir_path_append(script1, "black.gpl", script1)

          CALL dir_path_append(TRIM(HOME), "gnuplot", script2)
          CALL dir_path_append(script2, "yellow.gpl", script2)

#if defined(WINDOWS)
          call replace_slash(script1)
          call replace_slash(script2)
#endif						   							   

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     &TRIM(script1)//'" lc rgb "black" lw '//TRIM(lwstr)//' w l,"'//
     &TRIM(script2)//'" lc rgb "dark-yellow" lw '//TRIM(lwstr)//' w l'
          WRITE(150,*) 'pause -1'

      END SUBROUTINE settwocolors


      SUBROUTINE settwocolors2

          USE opsys
          USE gplblk_mod
        
          INCLUDE 'datmai.inc'

          CHARACTER(LEN=256) :: script1, script2, script3
          CHARACTER(LEN=16)  :: lwstr



          CALL retrieve_linewidth(lwstr)

          ! names of plot scripts
          CALL dir_path_append(TRIM(HOME), "gnuplot", script1)
          CALL dir_path_append(script1, "black.gpl", script1)

          CALL dir_path_append(TRIM(HOME), "gnuplot", script2)
          CALL dir_path_append(script2, "red.gpl", script2)

#if defined(WINDOWS)
          call replace_slash(script1)
          call replace_slash(script2)
#endif							   

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     &TRIM(script1)//'" lc rgb "black" lw '//TRIM(lwstr)//' w l,"'//
     &TRIM(script2)//'" lc rgb "red" lw '//TRIM(lwstr)//' w l'
          WRITE(150,*) 'pause -1'

      END SUBROUTINE settwocolors2


      SUBROUTINE settwocolors3

          USE opsys
          USE gplblk_mod
        
          INCLUDE 'datmai.inc'

          CHARACTER(LEN=256) :: script1, script2
          CHARACTER(LEN=16)  :: lwstr



          CALL retrieve_linewidth(lwstr)

          ! names of plot scripts
          CALL dir_path_append(TRIM(HOME), "gnuplot", script1)
          CALL dir_path_append(script1, "black.gpl", script1)

          CALL dir_path_append(TRIM(HOME), "gnuplot", script2)
          CALL dir_path_append(script2, "red.gpl", script2)

#if defined(WINDOWS)
          call replace_slash(script1)
          call replace_slash(script2)
#endif						   

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     &TRIM(script1)//'" lc rgb "black" lw '//TRIM(lwstr)//' w l,"'//
     &TRIM(script2)//'" lc rgb "red" w points'
          WRITE(150,*) 'pause -1'

      END SUBROUTINE settwocolors3


      SUBROUTINE setthreecolors

          USE opsys
          USE gplblk_mod
        
          INCLUDE 'datmai.inc'

          CHARACTER(LEN=256) :: script1, script2, script3
          CHARACTER(LEN=16)  :: lwstr



          CALL retrieve_linewidth(lwstr)

          ! names of plot scripts
          CALL dir_path_append(TRIM(HOME), "gnuplot", script1)
          CALL dir_path_append(script1, "black.gpl", script1)

          CALL dir_path_append(TRIM(HOME), "gnuplot", script2)
          CALL dir_path_append(script2, "yellow.gpl", script2)

          CALL dir_path_append(TRIM(HOME), "gnuplot", script3)
          CALL dir_path_append(script3, "magenta.gpl", script3)

#if defined(WINDOWS)
          call replace_slash(script1)
          call replace_slash(script2)
		  call replace_slash(script3)
#endif						   

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     &TRIM(script1)//'" lc rgb "black" lw '//TRIM(lwstr)//' w l,"'//
     &TRIM(script2)//'" lc rgb "dark-yellow" lw '//TRIM(lwstr)//' w l,"'//
     &TRIM(script3)//'" lc rgb "magenta" lw '//TRIM(lwstr)//' w l'
          WRITE(150,*) 'pause -1'

      END SUBROUTINE setthreecolors

      
      SUBROUTINE setthreecolors2

          USE opsys
          USE gplblk_mod
        
          INCLUDE 'datmai.inc'

          CHARACTER(LEN=256) :: script1, script2, script3
          CHARACTER(LEN=16)  :: lwstr



          CALL retrieve_linewidth(lwstr)

          ! names of plot scripts
          CALL dir_path_append(TRIM(HOME), "gnuplot", script1)
          CALL dir_path_append(script1, "black.gpl", script1)

          CALL dir_path_append(TRIM(HOME), "gnuplot", script2)
          CALL dir_path_append(script2, "yellow.gpl", script2)

          CALL dir_path_append(TRIM(HOME), "gnuplot", script3)
          CALL dir_path_append(script3, "red.gpl", script3)

#if defined(WINDOWS)
          call replace_slash(script1)
          call replace_slash(script2)
		  call replace_slash(script3)
#endif	

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     &TRIM(script1)//'" lc rgb "black" lw '//TRIM(lwstr)//' w l,"'//
     &TRIM(script2)//'" lc rgb "dark-yellow" lw '//TRIM(lwstr)//' w l,"'//
     &TRIM(script3)//'" lc rgb "red" lw '//TRIM(lwstr)//' w l'
          WRITE(150,*) 'pause -1'

      END SUBROUTINE setthreecolors2


      SUBROUTINE setfourcolors

          USE opsys
          USE gplblk_mod
        
          INCLUDE 'datmai.inc'

          CHARACTER(LEN=256) :: script1, script2, script3, script4
          CHARACTER(LEN=16)  :: lwstr



          CALL retrieve_linewidth(lwstr)

          ! names of plot scripts
          CALL dir_path_append(TRIM(HOME), "gnuplot", script1)
          CALL dir_path_append(script1, "black.gpl", script1)

          CALL dir_path_append(TRIM(HOME), "gnuplot", script2)
          CALL dir_path_append(script2, "yellow.gpl", script2)

          CALL dir_path_append(TRIM(HOME), "gnuplot", script3)
          CALL dir_path_append(script3, "magenta.gpl", script3)

          CALL dir_path_append(TRIM(HOME), "gnuplot", script4)
          CALL dir_path_append(script4, "red.gpl", script4)

#if defined(WINDOWS)
          call replace_slash(script1)
          call replace_slash(script2)
		  call replace_slash(script3)
		  call replace_slash(script4)
#endif

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     &TRIM(script1)//'" lc rgb "black" lw '//TRIM(lwstr)//' w l,"'//
     &TRIM(script2)//'" lc rgb "dark-yellow" lw '//TRIM(lwstr)//' w l,"'//
     &TRIM(script3)//'" lc rgb "magenta" lw '//TRIM(lwstr)//' w l,"'//
     &TRIM(script4)//'" lc rgb "red" lw '//TRIM(lwstr)//' w l'
          WRITE(150,*) 'pause -1'

      END SUBROUTINE setfourcolors

      
      SUBROUTINE MAC_EDITOR
        
          USE opsys
          USE globals

          IMPLICIT NONE
          
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'

          CALL shell_command(TRIM(TXTEDITOR)//" "//"MAC_EDIT.DAT")

      END SUBROUTINE MAC_EDITOR


      SUBROUTINE plotbmp(BMPFILE)
        !
        ! plots a BMP file using gnuplot in a specified
        ! graphics format. The file is plotted in a graphics
        ! window.
        !
        ! INPUT
        ! BMPFILE :  name of BMP file
        !
          USE globals
          USE opsys
          USE kokoconfig
          
          IMPLICIT NONE

          INCLUDE 'datmai.inc'

          CHARACTER(LEN=*),INTENT(IN) :: BMPFILE
          
          CHARACTER(LEN=32) :: gpterm, gpfont
          LOGICAL           :: BMPEXIST

          INQUIRE(file=TRIM(BMPFILE),exist=BMPEXIST)
          IF (.NOT.BMPEXIST) THEN
              WRITE(OUTLYNE,*) 'ERROR OPENING BMP FILE ',TRIM(BMPFILE)
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

          ! retrieve font and terminal info
          CALL CFG_get(koko_cfg, "graphics%fontsml",  gpfont)
          CALL CFG_get(koko_cfg, "graphics%terminal", gpterm)
             
          ! create gnuplot script
          OPEN (113, STATUS='replace', file=TRIM(HOME)//'plotbmp.gpl')
          WRITE(113,*) 'set terminal '//TRIM(gpterm)//' font "'//TRIM(gpfont)//'"'
          WRITE(113,*) 'set noborder'
          WRITE(113,*) 'set nokey'
          WRITE(113,*) 'set notics'
          WRITE(113,*) 'plot "'//TRIM(BMPFILE)//'" binary array=(320,240)
     & skip=54 format="%uint8" using 3:2:1 with rgbimage'
          WRITE(113,*) 'pause -1'
          CLOSE(113)

          ! dispatch gnuplot script
          ! -n flag set by the embedded GUI: skip launching the native
          ! gnuplot window. The GUI will read plotbmp.gpl itself and
          ! render the BMP image in its "Koko Plot" window.
          IF (.NOT. NOLAUNCH_GNUPLOT) THEN
              CALL shell_command(TRIM(BMPREADR)//' '//TRIM(HOME)//'plotbmp.gpl')
          END IF

      END SUBROUTINE plotbmp


      SUBROUTINE saveplot(BMPFILE,GRFILN, GFMT)
        !
        ! plots a BMP file using gnuplot in a specified
        ! graphics format.
        !
        ! INPUT
        ! BMPFILE :  name of BMP file to be printed
        ! GRFILN :   name of the graphics file to be created
        ! GFMT :     graphics format. "eps", "pdf", or "jpg"
        !
          USE globals
          USE opsys
          USE kokoconfig
          USE strings

          IMPLICIT NONE
          
          INCLUDE 'datmai.inc'

          CHARACTER(LEN=*), INTENT(IN) :: BMPFILE, GRFILN, GFMT
          
          CHARACTER(LEN=32) :: gpterm, gpfont
          CHARACTER(LEN=16) :: loc_gfmt
          LOGICAL           :: BMPEXIST

          INQUIRE(file=TRIM(BMPFILE),exist=BMPEXIST)
          IF (.NOT.BMPEXIST) THEN
              WRITE(OUTLYNE,*) 'ERROR OPENING BMP FILE ',TRIM(BMPFILE)
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

          ! retrieve font info
          CALL CFG_get(koko_cfg, "graphics%fontsml",  gpfont)

          loc_gfmt = GFMT  ! local copy
          CALL to_lower( loc_gfmt )
             
          SELECT CASE ( loc_gfmt )
          CASE ("eps")
             gpterm = "eps"
          CASE ("jpg")
             gpterm = "jpeg"
          CASE ("pdf")
             gpterm = "pdfcairo"
          CASE DEFAULT
             WRITE(OUTLYNE,*) 'UNRECOGNIZED GRAPHICS FORMAT'
             CALL SHOWIT(1)
             CALL MACFAL
             RETURN
          END SELECT
          
          ! created gnuplot script
          OPEN (113,file=TRIM(HOME)//'plotbmp.gpl')
          WRITE(113,*) 'set terminal '//TRIM(gpterm)//' font "'//TRIM(gpfont)//'"'
          WRITE(113,*) 'set noborder'
          WRITE(113,*) 'set nokey'
          WRITE(113,*) 'set notics'
          WRITE(113,*) 'plot "'//TRIM(BMPFILE)//'" binary array=(320,240)
     & skip=54 format="%uint8" using 3:2:1 with rgbimage'
          CLOSE(113)

          ! dispatch gnuplot
          CALL shell_command(TRIM(BMPREADR)//' '//TRIM(HOME)//'plotbmp.gpl > '
     &                       //TRIM(USERHOME)//'/'//TRIM(GRFILN))             

      END SUBROUTINE saveplot


      SUBROUTINE retrieve_linewidth(lw_str)

        USE kokoconfig
        
        CHARACTER(LEN=*),INTENT(OUT)   :: lw_str
        REAL (KIND=dp)                 :: linewidth
          
        ! retrieve configured linewidth
        CALL CFG_get(koko_cfg, "graphics%linewidth", linewidth)

        ! write it into string
        WRITE(lw_str, "(F6.2)") linewidth
        lw_str = ADJUSTL(lw_str)

      END SUBROUTINE retrieve_linewidth


      SUBROUTINE drawcmd3_clear
C       Truncate (empty) the plot-body script drawcmd3.gpl (unit 150) so the
C       NEXT plot command starts from a clean body instead of accumulating on
C       top of the previous figure (the overprint bug). koko opens unit 150
C       once at startup (koko.f), so without this every VIE/DIST/SPD/PLTDIST
C       block piles up for the life of the process. Labels (set label) are
C       written AFTER this clear during each plot command, so they are
C       preserved -- only the stale previous block is dropped.
C
C       We close unit 150 (if open) WITHOUT deleting the file, then re-open it
C       with STATUS='REPLACE'. REPLACE truncates the existing file to empty (or
C       creates it if absent) in a single atomic step, so there is no
C       delete-then-open race. The earlier code used CLOSE(STATUS='DELETE')
C       followed by OPEN(STATUS='NEW'); under gfortran the DELETE was not
C       always visible before the NEW open, so OPEN(NEW) failed with "file
C       exists" on the 2nd+ plot -- leaving unit 150 closed and the stale body
C       intact, which made PLTDIST (the 2nd plot in a session) accumulate
C       labels on top of the previous figure.
          USE opsys
          INCLUDE 'datmai.inc'
          CHARACTER(LEN=256) :: gp
          LOGICAL            :: OPEN150
          INQUIRE(UNIT=150, OPENED=OPEN150)
          IF (OPEN150) CLOSE(UNIT=150)
          CALL dir_path_append(TRIM(HOME), "gnuplot", gp)
          CALL dir_path_append(gp, "drawcmd3.gpl", gp)
          OPEN(UNIT=150, STATUS='REPLACE', FILE=TRIM(gp))
C     Also clear ALL labels at the top of the (now-empty) body file.
C     Koko writes labels with number-less "set label ..." (see
C     gnuplotlabel), which gnuplot auto-numbers and ACCUMULATES across
C     repeated "load" calls inside one (persistent) gnuplot session.
C     drawcmd.gpl may be split into blocks at "pause -1" lines and only
C     the LAST block rendered (the embedded GUI does exactly this), so
C     the header "unset label" in drawcmd0.gpl is NOT always present in
C     the rendered block. Putting "unset label" here -- at the very top
C     of the body that every plot command writes into -- guarantees the
C     stale labels from the previous figure are dropped no matter which
C     block the renderer loads. drawcmd3.gpl is rewritten from scratch on
C     each plot, so this only removes the previous figure's labels.
          WRITE(150,*) 'unset label'
          FLUSH(UNIT=150)
      END SUBROUTINE drawcmd3_clear
