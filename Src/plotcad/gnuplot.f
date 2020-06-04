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

      SUBROUTINE datacolorssave(I1,I2,I3,I4)

          IMPLICIT NONE
          INTEGER I1,I2,I3,I4

          IF (I1.LT.0.OR.I2.LT.0.OR.I1.GT.10000.OR.I2.GT.7000) THEN
              RETURN
          END IF

          IF (I4.EQ.0) THEN
              IF (I3.EQ.1) THEN
                  WRITE(130,'(2I5)') I1,I2
              ELSE
                  WRITE(130,*)
                  WRITE(130,'(2I5)') I1,I2
              END IF
          END IF

          IF (I4.EQ.1) THEN
              IF (I3.EQ.1) THEN
                  WRITE(115,'(2I5)') I1,I2
              ELSE
                  WRITE(115,*)
                  WRITE(115,'(2I5)') I1,I2

              END IF
          END IF

          IF (I4.EQ.2) THEN
              IF (I3.EQ.1) THEN
                  WRITE(116,'(2I5)') I1,I2
              ELSE
                  WRITE(116,*)
                  WRITE(116,'(2I5)') I1,I2
              END IF
          END IF

          IF (I4.EQ.3) THEN
              IF (I3.EQ.1) THEN
                  WRITE(117,'(2I5)') I1,I2
              ELSE
                  WRITE(117,*)
                  WRITE(117,'(2I5)') I1,I2
              END IF
          END IF

          IF (I4.EQ.4) THEN
              IF (I3.EQ.1) THEN
                  WRITE(118,'(2I5)') I1,I2
              ELSE
                  WRITE(118,*)
                  WRITE(118,'(2I5)') I1,I2
              END IF
          END IF

          RETURN
      END SUBROUTINE datacolorssave


      SUBROUTINE drawdatasave(I1,I2,I3,I4)

          implicit none
          integer I1,I2,I3,I4            

          if (I1.LT.0.OR.I2.LT.0.OR.I1.GT.10000.OR.I2.GT.7000) then
              return
          end if

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
                  write(131,'(2I5)') I1,I2    !break black
              else
                  write(131,*)
                  write(131,'(2I5)') I1,I2

              end if
          end if
          return
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
          return
      END SUBROUTINE drawdatasave2


      SUBROUTINE gnuplotlabel(I1,I2,label,I3,I4)

          IMPLICIT NONE
          INTEGER I1,I2,I3,I4
          REAL X,Y
          CHARACTER label*80

          X=REAL(I1)/10500.0+0.02
          Y=REAL(I2)/7350.0+0.03

          IF ((I4.EQ.2).AND.(I3.EQ.90)) THEN
              WRITE(150,'(A,F6.3,A1,F6.3,A2,A,A)') 'set label font "Courier,12"
     1center at screen ',REAL(I1)/10000.0,",",REAL(I2/7000.0)+0.01,
     2        ' "',TRIM(label),'" rotate by 90'
              GOTO 10

          ELSE IF (I4.EQ.2) THEN
              WRITE(150,'(A,F6.3,A1,F6.3,A2,A,A)') 'set label font "Courier,12"
     1center at screen ',REAL(I1)/10000.0,",",REAL(I2/7000.0)+0.01,
     2        ' "',TRIM(label),'"'
              GOTO 10

          ELSE
              WRITE(150,'(A,F6.3,A1,F6.3,A2,A,A)') 'set label at screen '
     1        ,X,",",Y,' "',TRIM(label),'"'

          END IF

   10     RETURN
      END SUBROUTINE gnuplotlabel


      SUBROUTINE contlabel(X0,Y0,label)

          IMPLICIT NONE

          REAL X0,Y0,X,Y
          CHARACTER label*80

          X=REAL(300.0/10000.0*X0)
          Y=REAL(210.0/7000.0*Y0)

          WRITE(150,'(A,F6.3,A1,F6.3,A2,A,A)') 'set label at screen ',
     1    X,",",Y,' "',label,'"'

          RETURN
      END SUBROUTINE contlabel


      SUBROUTINE drawcmdsave

          INCLUDE 'datmai.inc'
          CHARACTER*150 catcommand

          catcommand="cat "//TRIM(HOME)//"drawcmd0.txt "
     1    //TRIM(HOME)//"drawcmd3.txt "//"> "//TRIM(HOME)//"drawcmd.txt"

          CALL system(catcommand)

          RETURN
      END SUBROUTINE drawcmdsave


      SUBROUTINE drawcmdsave2

          INCLUDE 'datmai.inc'
          CHARACTER*150 catcommand2

          catcommand2="cat "//TRIM(HOME)//"drawcmd3.txt "
     1    //TRIM(HOME)//"gnuplot/plotcont.txt"//"> "
     2    //TRIM(HOME)//"drawcmd.txt"

          CALL system(catcommand2)

          RETURN
      END SUBROUTINE drawcmdsave2


      SUBROUTINE setonecolors

          INCLUDE 'datmai.inc'

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     1     TRIM(HOME)//'gnuplot/black.txt" lc rgb "black" lw 0.5 w l'
          WRITE(150,*) 'pause -1'

          RETURN
      END SUBROUTINE setonecolors

      
      SUBROUTINE setonecolors2

          INCLUDE 'datmai.inc'

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     1     TRIM(HOME)//'gnuplot/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     TRIM(HOME)//'gnuplot/breakblack.txt" with lines lt 0
     1 lc rgb "black" lw 2'

          WRITE(150,*) 'pause -1'

          RETURN
      END SUBROUTINE setonecolors2


      SUBROUTINE settwocolors

          INCLUDE 'datmai.inc'

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     1     TRIM(HOME)//'gnuplot/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     TRIM(HOME)//'gnuplot/yellow.txt" lc rgb "dark-yellow"
     1 lw 0.5 w l'
          WRITE(150,*) 'pause -1'

          RETURN
      END SUBROUTINE settwocolors

        
      SUBROUTINE settwocolors2

          INCLUDE 'datmai.inc'

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     1     TRIM(HOME)//'gnuplot/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     TRIM(HOME)//'gnuplot/red.txt" lc rgb "red"
     1 lw 0.5 w l'
          WRITE(150,*) 'pause -1'

          RETURN
      END SUBROUTINE settwocolors2

      
      SUBROUTINE settwocolors3

          INCLUDE 'datmai.inc'

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     1     TRIM(HOME)//'gnuplot/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     TRIM(HOME)//'gnuplot/red.txt" lc rgb "red"
     1 pt 7 ps 0.3'
          WRITE(150,*) 'pause -1'

          RETURN
      END SUBROUTINE settwocolors3

      
      SUBROUTINE setthreecolors

          INCLUDE 'datmai.inc'

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     1     TRIM(HOME)//'gnuplot/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     TRIM(HOME)//'gnuplot/yellow.txt" lc rgb "dark-yellow"
     1 lw 0.5 w l,"'//
     1     TRIM(HOME)//'gnuplot/magenta.txt" lc rgb "magenta"
     1 lw 0.5 w l'
          WRITE(150,*) 'pause -1'

          RETURN

      END SUBROUTINE setthreecolors

      
      SUBROUTINE setthreecolors2

          INCLUDE 'datmai.inc'

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     1     TRIM(HOME)//'gnuplot/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     TRIM(HOME)//'gnuplot/yellow.txt" lc rgb "dark-yellow"
     1 lw 0.5 w l,"'//
     1     TRIM(HOME)//'gnuplot/red.txt" lc rgb "red" lw 0.5 w l'
          WRITE(150,*) 'pause -1'

          RETURN
      END SUBROUTINE setthreecolors2


      SUBROUTINE setfourcolors

          INCLUDE 'datmai.inc'

          WRITE(150,*) 'plot [0:10000] [0:7000] "'//
     1     TRIM(HOME)//'gnuplot/black.txt" lc rgb "black"
     1 lw 0.5 w l,"'//
     1     TRIM(HOME)//'gnuplot/yellow.txt" lc rgb "dark-yellow"
     1 lw 0.5 w l,"'//
     1     TRIM(HOME)//'gnuplot/magenta.txt" lc rgb "magenta"
     1 lw 0.5 w l,"'//
     1     TRIM(HOME)//'gnuplot/red.txt" lc rgb "red" lw 0.5 w l'
          WRITE(150,*) 'pause -1'

          RETURN
      END SUBROUTINE setfourcolors

      
      SUBROUTINE MAC_EDITOR
        
          USE opsys
          USE GLOBALS

          IMPLICIT NONE
          
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'

          CALL shell_command(TRIM(TXTEDITOR)//" "//"MAC_EDIT.DAT")

          RETURN
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

          ! retrieve font info
          CALL CFG_get(koko_cfg, "graphics%fontsml",  gpfont)
          CALL CFG_get(koko_cfg, "graphics%terminal", gpterm)
             
          ! create gnuplot script
          OPEN (113,file=TRIM(HOME)//'plotbmp.gpl')
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
