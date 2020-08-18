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
          INTEGER, INTENT(IN) :: I3    ! ???
          INTEGER, INTENT(IN) :: IC    ! plot color

          ! canvas size: x in [0,10000], y in [0,7000]

          IF (IC.EQ.0) THEN
              IF (I3.EQ.1) THEN
                  WRITE(130,'(2I5)') IX,IY
              ELSE
                  WRITE(130,*)
                  WRITE(130,'(2I5)') IX,IY
              END IF
          END IF

          IF (IC.EQ.1) THEN
              IF (I3.EQ.1) THEN
                  WRITE(115,'(2I5)') IX,IY
              ELSE
                  WRITE(115,*)
                  WRITE(115,'(2I5)') IX,IY

              END IF
          END IF

          IF (IC.EQ.2) THEN
              IF (I3.EQ.1) THEN
                  WRITE(116,'(2I5)') IX,IY
              ELSE
                  WRITE(116,*)
                  WRITE(116,'(2I5)') IX,IY
              END IF
          END IF

          IF (IC.EQ.3) THEN
              IF (I3.EQ.1) THEN
                  WRITE(117,'(2I5)') IX,IY
              ELSE
                  WRITE(117,*)
                  WRITE(117,'(2I5)') IX,IY
              END IF
          END IF

          IF (IC.EQ.4) THEN
              IF (I3.EQ.1) THEN
                  WRITE(118,'(2I5)') IX,IY
              ELSE
                  WRITE(118,*)
                  WRITE(118,'(2I5)') IX,IY
              END IF
          END IF

      END SUBROUTINE datacolorssave


      SUBROUTINE drawdatasave(I1,I2,I3,I4)

          implicit none
          integer I1,I2,I3,I4            

            if (I4.eq.0) then
              if (I3.eq.1) then
                  write(130,'(2I5)') I1,I2    !black
              else
                  write(130,*)
                  write(130,'(2I5)') I1,I2
              end if
            end if

           if (I4.eq.1) then
              if (I3.eq.1) then
                  write(115,'(2I5)') I1,I2    !yellow
              else
                  write(115,*)
                  write(115,'(2I5)') I1,I2
              end if
            end if

           if (I4.eq.2) then
              if (I3.eq.1) then
                  write(116,'(2I5)') I1,I2    !magenta
              else
                  write(116,*)
                  write(116,'(2I5)') I1,I2
              end if
           end if

           if (I4.eq.3) then
              if (I3.eq.1) then
                  write(117,'(2I5)') I1,I2    !red
              else
                  write(117,*)
                  write(117,'(2I5)') I1,I2
              end if        
           end if

           if (I4.eq.4) then
              if (I3.eq.1) then
                  write(118,'(2I5)') I1,I2    !cyan
              else
                  write(118,*)
                  write(118,'(2I5)') I1,I2
              end if
            end if

          if (I4.eq.2) then
              if (I3.eq.1) then
                  write(131,'(2I5)') I1,I2    !break black (dashed ???)
              else
                  write(131,*)
                  write(131,'(2I5)') I1,I2
              end if
          end if

      END SUBROUTINE drawdatasave


      SUBROUTINE drawdatasave2(I1,I2,I3,I4,I5,I6,I7,I8)

          implicit none
          integer I1,I2,I3,I4,I5,I6,I7,I8

          if ((I1.eq.-1).and.(I2.eq.-1)) then
              write(130,*)
              write(115,*)
              write(116,*)
              write(117,*)
              write(118,*)
              return
          end if

          if ((I1.eq.416).or.(I1.eq.3750).or.(I1.eq.7082)) then
              write(130,*)
              write(115,*)
              write(116,*)
              write(117,*)
              write(118,*)
              return
          end if

          if (((I1.ge.I5).and.(I1.le.I6)).and.((I2.ge.I7).and.(I2.le.I8))) then
            if ((I4.eq.0).and.(I3.eq.1)) then
                  write(130,'(2I5)') I1,I2    !black
            else if ((I4.eq.0).and.(I3.eq.0)) then
                  write(130,*)
                  write(130,'(2I5)') I1,I2
            end if
          end if

          if (((I1.ge.I5).and.(I1.le.I6)).and.((I2.ge.I7).and.(I2.le.I8))) then
            if ((I4.eq.1).and.(I3.eq.1)) then
                  write(115,'(2I5)') I1,I2    !yellow
            else if ((I4.eq.1).and.(I3.eq.0)) then
                  write(115,*)
                  write(115,'(2I5)') I1,I2
            end if
          end if

          if (((I1.ge.I5).and.(I1.le.I6)).and.((I2.ge.I7).and.(I2.le.I8))) then
            if ((I4.eq.2).and.(I3.eq.1)) then
                  write(116,'(2I5)') I1,I2    !yellow
            else if ((I4.eq.2).and.(I3.eq.0)) then
                  write(116,*)
                  write(116,'(2I5)') I1,I2
            end if
          end if

          if (((I1.ge.I5).and.(I1.le.I6)).and.((I2.ge.I7).and.(I2.le.I8))) then
            if ((I4.eq.3).and.(I3.eq.1)) then
                  write(117,'(2I5)') I1,I2    !yellow
            else if ((I4.eq.3).and.(I3.eq.0)) then
                  write(117,*)
                  write(117,'(2I5)') I1,I2
            end if
          end if

          if (((I1.ge.I5).and.(I1.le.I6)).and.((I2.ge.I7).and.(I2.le.I8))) then
            if ((I4.eq.4).and.(I3.eq.1)) then
                  write(118,'(2I5)') I1,I2    !yellow
            else if ((I4.eq.4).and.(I3.eq.0)) then
                  write(118,*)
                  write(118,'(2I5)') I1,I2
            end if
          end if

          if (I4.eq.2) then
              if (I3.eq.1) then
                  write(131,'(2I5)') I1,I2    !break black
              else
                  write(131,*)
                  write(131,'(2I5)') I1,I2
              end if
          end if

      END SUBROUTINE drawdatasave2


      SUBROUTINE gnuplotlabel(IX,IY,label,I3,I4)

          USE kokoconfig
        
          IMPLICIT NONE
          
          CHARACTER(LEN=*), INTENT(IN) :: label
          INTEGER, INTENT(IN)          :: IX,IY,I3,I4
          
          REAL              :: X,Y
          CHARACTER(LEN=32) :: font

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

          IMPLICIT NONE

          REAL, INTENT(IN)             :: X0,Y0
          CHARACTER(LEN=*), INTENT(IN) :: label

          REAL :: X,Y

          X=REAL(300.0/10000.0*X0)
          Y=REAL(210.0/7000.0*Y0)

          WRITE(150,'(A,F6.3,A1,F6.3,A2,A,A)') 'set label at screen ',
     1    X,',',Y,' "',label,'"'

      END SUBROUTINE contlabel


      SUBROUTINE drawcmdsave

          USE opsys
          USE kokoconfig
        
          INCLUDE 'datmai.inc'
          CHARACTER(LEN=256) :: file_a, file_b, file_out
          CHARACTER(LEN=32)  :: gpterm, gpfont

          ! name of script header file
          file_a = TRIM(HOME)
          CALL dir_path_append(file_a, "gnuplot", file_a)
          CALL dir_path_append(file_a, "drawcmd0.gpl", file_a)

          ! create new script header
          CALL CFG_get(koko_cfg, "graphics%fontsml",  gpfont)
          CALL CFG_get(koko_cfg, "graphics%terminal", gpterm)
             
          OPEN (213, STATUS='replace', FILE=TRIM(file_a))
          WRITE(213,*) 'set terminal '//TRIM(gpterm)//' font "'//TRIM(gpfont)//'"'
          WRITE(213,*) 'set noborder'
          WRITE(213,*) 'set nokey'
          WRITE(213,*) 'set notics'
          CLOSE(213)
          
          ! script body file
          file_b = TRIM(HOME)
          CALL dir_path_append(file_b, "gnuplot", file_b)
          CALL dir_path_append(file_b, "drawcmd3.gpl", file_b)

          ! name of complete script file
          file_out = TRIM(HOME)
          CALL dir_path_append(file_out, "gnuplot", file_out)
          CALL dir_path_append(file_out, "drawcmd.gpl", file_out)

          ! create full gnuplot script
          CALL append_files(file_a, file_b, file_out)

      END SUBROUTINE drawcmdsave


      SUBROUTINE drawcmdsave2

          USE opsys
        
          INCLUDE 'datmai.inc'
          CHARACTER(LEN=256) :: file_a, file_b, file_out

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

          ! create full gnuplot script
          CALL append_files(file_a, file_b, file_out)

      END SUBROUTINE drawcmdsave2


      SUBROUTINE setonecolors

          USE opsys
          
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
     &TRIM(script2)//'" lc rgb "red" lw '//TRIM(lwstr)//' w l'
          WRITE(150,*) 'pause -1'

      END SUBROUTINE settwocolors2


      SUBROUTINE settwocolors3

          USE opsys
        
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
     &TRIM(script2)//'" lc rgb "red" pt 7 ps 0.3'
          WRITE(150,*) 'pause -1'

      END SUBROUTINE settwocolors3


      SUBROUTINE setthreecolors

          USE opsys
        
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
          CALL dir_path_append(script3, "red.gpl", script4)

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
          CALL shell_command(TRIM(BMPREADR)//' '//TRIM(HOME)//'plotbmp.gpl')

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
