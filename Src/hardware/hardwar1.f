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

      SUBROUTINE FORCE_BATCH(BFLAG)
          LOGICAL BFLAG
          BFLAG=.TRUE.
      END SUBROUTINE FORCE_BATCH


      SUBROUTINE MYDATE(DDATE)

          IMPLICIT NONE
          CHARACTER DDATE*10
          CHARACTER FDATE*8,FTIME*10,FZONE*5
          INTEGER FVALUE(8)

          CALL MY_DATE_AND_TIME(FDATE,FTIME,FZONE,FVALUE)
          DDATE=FDATE(5:6)//'-'//FDATE(7:8)//'-'//FDATE(1:4)

      END SUBROUTINE MYDATE


      SUBROUTINE MYTIME(TTIME)

          IMPLICIT NONE
          INTEGER FVALUE(8)
          CHARACTER TTIME*8,FTIME*10,FZONE*5,FDATE*8

          CALL MY_DATE_AND_TIME(FDATE,FTIME,FZONE,FVALUE)
          TTIME=FTIME(1:2)//':'//FTIME(3:4)//':'//FTIME(5:6)

      END SUBROUTINE MYTIME


      SUBROUTINE NOBLANK(OLYNE)
          IMPLICIT NONE
          CHARACTER OLYNE*140
          INTEGER J,LLL,LL
          INCLUDE 'datmai.inc'
          LL=LEN_TRIM(OLYNE)
          LLL=LL
          DO J=LL-1,1,-1
              IF(OLYNE(J:J).EQ.' '.AND.OLYNE(J+1:J+1).EQ.',') THEN
                  OLYNE(1:140)=OLYNE(1:J-1)//OLYNE(J+1:140)//' '
                  LLL=LLL-1
              END IF
          END DO
      END SUBROUTINE NOBLANK


C SUB MY_DIR.INC
      SUBROUTINE MY_DIR(STRINGER)
          USE opsys
          IMPLICIT NONE
          CHARACTER STRINGER*80
          CALL shell_command( STRINGER )
      END SUBROUTINE MY_DIR


      SUBROUTINE MACPAUSE
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          CALL MY_IGRPAUSE
      END SUBROUTINE MACPAUSE


      SUBROUTINE MY_IGRPAUSE
          IMPLICIT NONE
          CHARACTER KKDP*3

          CALL SELECTKOKO(KKDP)
      END SUBROUTINE MY_IGRPAUSE


      SUBROUTINE CBREAK
C     CONTROL BREAK
          LOGICAL LBLHT
          INTEGER IICODE

          COMMON/JKA/LBLHT

          INCLUDE 'datmai.inc'

          IICODE=-999
          LBLHT=.FALSE.
          CALL MY_INKEYEVENTIMM(IICODE)
          IF(IICODE.EQ.3) THEN
              LBLHT=.TRUE.
          END IF
          IICODE=-999
      END SUBROUTINE CBREAK


      SUBROUTINE CLOSE_FILE(UNITT,STAT)
C
C     THIS SUBROUTINE IS CALLED TO CLOSE OPEN FILES
C
          IMPLICIT NONE
          INTEGER UNITT,STAT
          LOGICAL UNITOPEN
          CHARACTER*80 FILE_NAME

          INCLUDE 'datmai.inc'

C     STAT = 1 MEANS 'KEEP'
C     STAT = 0 MEANS 'DELETE'

          ALLSTOP=.FALSE.

          FILE_NAME='UNNAMED'
          INQUIRE(UNIT=UNITT,NAME=FILE_NAME)

          UNITOPEN=.FALSE.
          INQUIRE(UNIT=UNITT,OPENED=UNITOPEN)
          IF(UNITOPEN) THEN
              IF(STAT.EQ.0) THEN
                  CLOSE(UNITT,STATUS='DELETE',ERR=666)
              END IF
              IF(STAT.EQ.1) THEN
                  CLOSE(UNITT,STATUS='KEEP',ERR=777)
              END IF
          END IF
          RETURN
 666      CONTINUE
          WRITE(OUTLYNE,*)
     1    'An error was encountered in closing the file:',
     2    TRIM(FILE_NAME),' with status = "DELETE"'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)
     1    'Try deleting the file from the operating system level'
     1    ,' before proceeding.'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
 777      CONTINUE
          WRITE(OUTLYNE,*)
     1    'An error was encountered in closing the file:',
     2    TRIM(FILE_NAME),' with status = "KEEP"'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)
     1    'Try deleting the file from the operating system level'
     1    ,' before proceeding.'
          CALL SHOWIT(1)
          CALL MACFAL
      END SUBROUTINE CLOSE_FILE


      SUBROUTINE EXIST_FILE(FILE_NAME,FEXIST)
C
C     THIS SUBROUTINE IS CALLED TO TEST IF A FILE EXISTS
C     IN THE HOME DIRECTORY
C
          IMPLICIT NONE
          LOGICAL FEXIST
          CHARACTER*80 FILE_NAME

          INCLUDE 'datmai.inc'

          FEXIST=.FALSE.
          INQUIRE(FILE=trim(HOME)//FILE_NAME,EXIST=FEXIST)
      END SUBROUTINE EXIST_FILE


      SUBROUTINE OPEN_FILE(FILE_NAME,FOPEN)
C
C     THIS SUBROUTINE IS CALLED TO TEST IF A FILE IS OPEN OR CLOSED
C
          IMPLICIT NONE
          LOGICAL FOPEN
          CHARACTER*80 FILE_NAME

          INCLUDE 'datmai.inc'

          FOPEN=.FALSE.
          INQUIRE(FILE=trim(HOME)//FILE_NAME,OPENED=FOPEN)
      END SUBROUTINE OPEN_FILE


      SUBROUTINE SEETIM

          IMPLICIT NONE
          INTEGER BASE,IVAL
          REAL RESULT

          COMMON/TIMMER/BASE

          INCLUDE 'datmai.inc'

          IVAL=10
          CALL MY_TIMER(IVAL)
          RESULT=IVAL-BASE
          WRITE(OUTLYNE,100) RESULT
 100      FORMAT(
     1    'ELAPSED TIME SINCE TIMER RESET = (',G11.4,') SECONDS')
          CALL SHOWIT(0)
      END SUBROUTINE SEETIM


      SUBROUTINE SETTIM
          IMPLICIT NONE
          INTEGER BASE,I
          COMMON/TIMMER/BASE
          I=0
          CALL MY_TIMER(I)
      END SUBROUTINE SETTIM


C SUB EDITOR.INC
      SUBROUTINE EDITOR

          USE GLOBALS
          USE opsys

          IMPLICIT NONE
          INCLUDE 'datmai.inc'

          LOGICAL FEXIST
          CHARACTER AS*80
          INTEGER I,N2,ITEM

          IF(SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"EDIT" ONLY TAKES STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(SST.EQ.0) THEN
              OUTLYNE='NO FILE NAME WAS ISSUED WITH "EDIT"'
              CALL SHOWIT(1)
              OUTLYNE='EDITING THE DEFAULT FILE "text.dat"'
              CALL SHOWIT(1)
              WS(1:12)='text.dat'
          END IF

          CALL CLOSE_FILE(7,1)
          CALL CLOSE_FILE(8,1)
          CALL CLOSE_FILE(9,1)
          CALL CLOSE_FILE(10,1)
          CALL CLOSE_FILE(30,1)
C
C       EDIT FILE DESIGNATED BY WS
C
          AS=WS
          N2=0
          DO I=80,1,-1
              IF(AS(I:I).NE.' ') THEN
                  N2=I
                  GO TO 20
              ELSE
              END IF
          END DO
 20       CONTINUE

          FEXIST=.FALSE.
          INQUIRE(FILE=AS(1:N2),EXIST=FEXIST)
          IF(.NOT.FEXIST) THEN
              OPEN(UNIT=47,ACCESS='SEQUENTIAL',BLANK='NULL'
     1          ,FORM='FORMATTED',FILE=AS(1:N2)
     2          ,STATUS='UNKNOWN')
              CLOSE(47 )
          END IF
          ITEM=ID_SYSTEM
          call shell_command(trim(TXTEDITOR)//" "//AS(1:N2))
      END SUBROUTINE EDITOR


C SUB OPENTSTP.INC
      SUBROUTINE OPENTSTP(FILENAME)

          USE GLOBALS
          USE opsys
      
          IMPLICIT NONE

          LOGICAL FEXIST
          CHARACTER AS*19,FILENAME*19
          INTEGER I,N2

          INCLUDE 'datmai.inc'

          CALL CLOSE_FILE(7,1)
          CALL CLOSE_FILE(8,1)
          CALL CLOSE_FILE(9,1)
          CALL CLOSE_FILE(10,1)
          CALL CLOSE_FILE(30,1)
C
C       EDIT FILE DESIGNATED BY FILENAME
C
          AS(1:19)=FILENAME(1:19)
          N2=0
          DO I=19,1,-1
              IF(AS(I:I).NE.' ') THEN
                  N2=I
                  GO TO 20
              ELSE
              END IF
          END DO
 20       CONTINUE

          FEXIST=.FALSE.
          INQUIRE(FILE=AS(1:N2),EXIST=FEXIST)
          IF(.NOT.FEXIST) THEN
              OPEN(UNIT=47,ACCESS='SEQUENTIAL',BLANK='NULL'
     1          ,FORM='FORMATTED',FILE=AS(1:N2)
     2          ,STATUS='UNKNOWN')
              CLOSE(47 )
          END IF
          call shell_command(trim(TXTEDITOR)//" "//AS(1:N2))
      END SUBROUTINE OPENTSTP


      SUBROUTINE DRAWIT
          USE GLOBALS
          IMPLICIT NONE
          CALL GRADRAW
      END SUBROUTINE DRAWIT


      SUBROUTINE GRADRAW
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          CALL RUN_WDRAW
          OPEN(UNIT=28
     1    ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2    ,RECL=(NRECL*42),STATUS='UNKNOWN')
      END SUBROUTINE GRADRAW


      SUBROUTINE MY_SETCHARASPECT(J_X,J_Y)
          USE GLOBALS
          IMPLICIT NONE
          CHARACTER STRINGER*1,NEUTLINE*42
          REAL J_X,J_Y
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8
          INCLUDE 'datmai.inc'
          INCLUDE 'dathgr.inc'
          I1=0
          I2=0
          I3=0
          I4=0
          I5=0
          I6=0
          I7=0
          I8=0
          STRINGER='F'
          NEUTTOTAL=NEUTTOTAL+1
          WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
 1000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
          NEUTTOTAL=NEUTTOTAL+1
          WRITE(NEUTLINE,1500) J_X,J_Y
          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2)
     1    CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
 1500     FORMAT(E15.7,E15.7,11X)
      END SUBROUTINE MY_SETCHARASPECT


      SUBROUTINE PDRAW

          USE globals
          USE opsys
        
          IMPLICIT NONE
C
C       THIS SUB DOES THE DRAW COMMAND
C
          CHARACTER(LEN=11)  :: DRWNAM
          CHARACTER(LEN=256) :: plotcommand
          COMMON/DRWTAG/DRWNAM

          INCLUDE 'datmai.inc'
          INCLUDE 'dathgr.inc'

C       CHECK SYNTAX
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"DRAW" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          OPEN28=.FALSE.
          INQUIRE(UNIT=28,OPENED=OPEN28)
          IF(OPEN28) CALL MY_ENDPLT
          CALL CLOSE_FILE(28,1)
          OPEN(UNIT=28
     1    ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2    ,RECL=(NRECL*42),STATUS='UNKNOWN')
          IF(.NOT.NODRAW) CALL DRAWIT
          IF(.NOT.NOWMF) THEN
              SAVE_KDP(22)=SAVEINPT(22)
              INPUT='GRAOUT COLWMF PRG'
              CALL PROCES
              REST_KDP(22)=RESTINPT(22)
          END IF
          IF(NODRAW) THEN
              SAVE_KDP(22)=SAVEINPT(22)
              INPUT='GRAOUT REPLAY'
              CALL PROCES
              REST_KDP(22)=RESTINPT(22)
          END IF

          CALL drawcmdsave

#if defined(WINDOWS)
          plotcommand = TRIM(BMPREADR)//" -persist "//TRIM(HOME)
#endif

#if defined(LINUX)
          plotcommand = TRIM(BMPREADR)//" "//TRIM(HOME)
#endif

          CALL dir_path_append(plotcommand, "gnuplot", plotcommand)
          CALL dir_path_append(plotcommand, "drawcmd.gpl", plotcommand)

          CALL shell_command( plotcommand )
          
      END SUBROUTINE PDRAW

      
      SUBROUTINE LDRAW
        
          USE GLOBALS
          IMPLICIT NONE
C
C       THIS SUB DOES THE LISTDRAW COMMAND
C
          INTEGER I

          INCLUDE 'datmai.inc'
          INCLUDE 'dathgr.inc'

          OPEN28=.FALSE.
          INQUIRE(UNIT=28,OPENED=OPEN28)
          IF(OPEN28) CALL MY_ENDPLT
          CALL CLOSE_FILE(28,1)
          DO I=1,NEUTTOTAL+1
              WRITE(OUTLYNE,10) NEUTARRAY(I)
              CALL SHOWIT(1)
 10           FORMAT(A42)
          END DO
      END SUBROUTINE LDRAW


      SUBROUTINE NODRAWW

          IMPLICIT NONE

          INCLUDE 'datmai.inc'
          INCLUDE 'dathgr.inc'
          
C       CHECK SYNTAX
          IF(STI.EQ.1) THEN
              IF(NODRAW)
     1             OUTLYNE='DRAWING TO THE SCREEN IS CURRENTLY TURNED "OFF"'
              IF(.NOT.NODRAW)
     1             OUTLYNE='DRAWING TO THE SCREEN IS CURRENTLY TURNED "ON"'
              CALL SHOWIT(1)
              RETURN
          ELSE
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              IF(WC.EQ.'YESDRAW') OUTLYNE='"YESDRAW" TAKES NO ADDITIONAL INPUT'
              IF(WC.EQ.'NODRAW') OUTLYNE='"NODRAW" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'YESDRAW') NODRAW=.FALSE.
          IF(WC.EQ.'NODRAW') NODRAW=.TRUE.
          RETURN
      END SUBROUTINE NODRAWW


      SUBROUTINE NOWMFF

          IMPLICIT NONE

          INCLUDE 'datmai.inc'
          INCLUDE 'dathgr.inc'

C       CHECK SYNTAX
          IF(STI.EQ.1) THEN
              IF(NOWMF)
     1             OUTLYNE='DRAWING TO PRG.WMF IS CURENTLY TURNED "OFF"'
              IF(.NOT.NOWMF)
     1             OUTLYNE='DRAWING TO PRG.WMF IS CURRENTLY TURNED "ON"'
              CALL SHOWIT(1)
              RETURN
          ELSE
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              IF(WC.EQ.'YESWMF') OUTLYNE='"YESWMF" TAKES NO ADDITIONAL INPUT'
              IF(WC.EQ.'NOWMF') OUTLYNE='"NOWMF" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'YESWMF') NOWMF=.FALSE.
          IF(WC.EQ.'NOWMF') NOWMF=.TRUE.
          RETURN
      END SUBROUTINE NOWMFF


      SUBROUTINE GRAOUT
          USE GLOBALS
          IMPLICIT NONE
C
C       THIS SUB DOES THE GRAOUT COMMAND
C
          CHARACTER GRFILN*12,CEELINE*14,DRWNAM*11,JK_TAG*2

          COMMON/DRWTAG/DRWNAM

          INTEGER I,J
          LOGICAL DEVOK

          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'

          OPEN28=.FALSE.
          INQUIRE(UNIT=28,OPENED=OPEN28)
          IF(OPEN28) CALL MY_ENDPLT
          CALL CLOSE_FILE(28,1)
          OPEN(UNIT=28
     1    ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2    ,RECL=(NRECL*42),STATUS='UNKNOWN')

C       CHECK SYNTAX
          DEVOK=.FALSE.
          IF(SQ.EQ.0) SST=0
          IF(WQ.EQ.'COLOR   ') SST=0
          IF(SQ.EQ.0) DEVOK=.TRUE.
          IF(WQ.EQ.'COLEPS  ') DEVOK=.TRUE.
          if(WQ.EQ.'JPG     ') DEVOK=.TRUE.
          if(WQ.EQ.'PDF     ') DEVOK=.TRUE.
          IF(.NOT.DEVOK) THEN
              OUTLYNE='INVALID QUALIFIER WORD USED WITH "GRAOUT"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(WQ.EQ.'        '.AND.SST.EQ.0) THEN
              GRFILN='            '
          END IF
          IF(WQ.EQ.'WMF     '.AND.SST.EQ.0) THEN
              GRFILN='WMF.WMF'
          END IF
          IF(WQ.EQ.'EMF     '.AND.SST.EQ.0) THEN
              GRFILN='EMF.EMF'
          END IF
          IF(WQ.EQ.'AMF     '.AND.SST.EQ.0) THEN
              GRFILN='AMF.AMF'
          END IF
          IF(WQ.EQ.'COLWMF  '.AND.SST.EQ.0) THEN
              GRFILN='COLWMF.WMF'
          END IF
          IF(WQ.EQ.'PCX     '.AND.SST.EQ.0) THEN
              GRFILN='PCX.PCX'
          END IF
          IF(WQ.EQ.'COLPCX  '.AND.SST.EQ.0) THEN
              GRFILN='COLPCX.PCX'
          END IF
          IF(WQ.EQ.'BMP     '.AND.SST.EQ.0) THEN
              GRFILN='BMP.BMP'
          END IF
          IF(WQ.EQ.'COLBMP  '.AND.SST.EQ.0) THEN
              GRFILN='COLBMP.BMP'
          END IF
          IF(WQ.EQ.'CBMP    '.AND.SST.EQ.0) THEN
              GRFILN='CBMP.BMP'
          END IF
          IF(WQ.EQ.'COLCBMP '.AND.SST.EQ.0) THEN
              GRFILN='COLCBMP.BMP'
          END IF
          IF(WQ.EQ.'EPS     '.AND.SST.EQ.0) THEN
              GRFILN='EPS.EPS'
          END IF
          IF(WQ.EQ.'COLEPS  '.AND.SST.EQ.0) THEN
              GRFILN='COLEPS.EPS'
          END IF
          IF(WQ.EQ.'REPLAY  ') THEN
              GRFILN='REPLAY.WMF'
          END IF
          if(WQ.EQ.'JPG  ') then
              GRFILN='JPEG.JPG'
          end if
          if(WQ.EQ.'PDF  ') then
              GRFILN='PDF.PDF'
          end if
          IF(SST.EQ.1) THEN
              WS = ADJUSTR(WS)
          END IF
          IF(SST.EQ.1) THEN
              IF(WQ.EQ.'        ') GRFILN='            '
              IF(WQ.EQ.'WMF     ') GRFILN=WS(1:8)//'.WMF'
              IF(WQ.EQ.'EMF     ') GRFILN=WS(1:8)//'.EMF'
              IF(WQ.EQ.'AMF     ') GRFILN=WS(1:8)//'.AMF'
              IF(WQ.EQ.'COLWMF  ') GRFILN=WS(1:8)//'.WMF'
              IF(WQ.EQ.'COLEMF  ') GRFILN=WS(1:8)//'.EMF'
              IF(WQ.EQ.'COLAMF  ') GRFILN=WS(1:8)//'.AMF'
              IF(WQ.EQ.'PCX     ') GRFILN=WS(1:8)//'.PCX'
              IF(WQ.EQ.'COLPCX  ') GRFILN=WS(1:8)//'.PCX'
              IF(WQ.EQ.'BMP     ') GRFILN=WS(1:8)//'.BMP'
              IF(WQ.EQ.'COLBMP  ') GRFILN=WS(1:8)//'.BMP'
              IF(WQ.EQ.'CBMP    ') GRFILN=WS(1:8)//'.BMP'
              IF(WQ.EQ.'COLCBMP ') GRFILN=WS(1:8)//'.BMP'
              IF(WQ.EQ.'EPS     ') GRFILN=WS(1:8)//'.EPS'
              IF(WQ.EQ.'COLEPS  ') GRFILN=WS(1:8)//'.EPS'
              if(WQ.EQ.'JPG     ') GRFILN=WS(1:8)//'.JPG'
              if(WQ.EQ.'PDF     ') GRFILN=WS(1:8)//'.PDF'
              J=1
 90           CONTINUE
              DO I=2,12
                  IF(GRFILN(I:I).EQ.' ') THEN
                      GRFILN(1:12)=GRFILN(1:I-1)//GRFILN(I+1:12)//' '
                      J=J+1
                      IF(J.GE.12) GO TO 91
                      GO TO 90
                  END IF
              END DO
 91           CONTINUE
          END IF
          SST=1
          IF(SN.EQ.1) THEN
              OUTLYNE='"GRAOUT"'
              CALL SHOWIT(1)
              OUTLYNE='TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C
          IF(DRWNAM.EQ.'           ') THEN
              DRWNAM='NEUTRAL.DAT'
          ELSE
          END IF
C
          IF(DRWNAM(1:11).EQ.'NEUTRAL.DAT') THEN
              EXIS28=.FALSE.
              INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',EXIST=EXIS28)
              IF(.NOT.EXIS28.OR..NOT.PLEXIS) THEN
                  OUTLYNE='NO PLOTFILE EXISTS TO BE PLOTTED'
                  CALL SHOWIT(1)
                  OUTLYNE='NO PLOT WILL BE MADE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
          ELSE
C     PROCEED
          END IF
          JK_TAG='00'

          IF(WQ.EQ.'COLEPS  ')  JK_TAG='32'
          if(WQ.EQ.'JPG     ')  JK_TAG='33'
          if(WQ.EQ.'PDF     ')  JK_TAG='34'
          CLOSE(28)

          INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',EXIST=EXIS28)
          IF(.NOT.EXIS28) THEN
C     MESSAGE THAT NO FILE EXISTS TO DRAW
              OUTLYNE='NO PLOTFILE EXISTS TO BE PLOTTED'
              CALL SHOWIT(1)
              OUTLYNE='NO PLOT WILL BE MADE'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(JK_TAG.EQ.'32') OUTLYNE=
     1    'COLOR GRAPHICS BEING SENT TO EPS FILE '
     2    //trim(USERHOME)//'/'//trim(GRFILN)
          if(JK_TAG.EQ.'33') OUTLYNE=
     1    'COLOR GRAPHICS BEING SENT TO JPG FILE '
     2    //trim(USERHOME)//'/'//trim(GRFILN)
          if(JK_TAG.EQ.'34') OUTLYNE=
     1    'COLOR GRAPHICS BEING SENT TO PDF FILE '
     2    //trim(USERHOME)//'/'//trim(GRFILN)

          CALL SHOWIT(1)
          IF(OPEN28) CALL MY_ENDPLT
          CALL CLOSE_FILE(28,1)

          GRFILN = ADJUSTL( GRFILN )

          CEELINE=JK_TAG//(GRFILN(1:12))
          OPEN(UNIT=28
     1    ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2    ,RECL=(NRECL*42),STATUS='UNKNOWN')
          CALL CLOSE_FILE(28,0)
          OPEN(UNIT=28
     1    ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2    ,RECL=(NRECL*42),STATUS='UNKNOWN')
          CALL CLOSE_FILE(28,0)
          OPEN(UNIT=28
     1    ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2    ,RECL=(NRECL*42),STATUS='UNKNOWN')
          IF(NEUTFILE) THEN
              DO I=1,NEUTTOTAL+1
                  WRITE(UNIT=28,REC=I) NEUTARRAY(I)
              END DO
          END IF
          CALL CLOSE_FILE(28,1)
 5        CONTINUE
          OPEN28=.FALSE.
          EXIS28=.FALSE.
          INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',EXIST=EXIS28)
          INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',OPENED=OPEN28)
          IF(EXIS28.AND.OPEN28) GO TO 5
          IF(EXIS28.AND..NOT.OPEN28)
     1     CALL RUN_WPLOT(CEELINE)
          OPEN(UNIT=28
     1    ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2    ,RECL=(NRECL*42),STATUS='UNKNOWN')
          RETURN
      END SUBROUTINE GRAOUT


      SUBROUTINE PLSTAT

          IMPLICIT NONE

C       RETURNS THE NUMBER OF PLOTS CURRENTLY
C       ON FILE IN THE PLOT LIBRARAY

          INTEGER I,J,K,II
          LOGICAL EXISJK
          CHARACTER DATA*80

          COMMON/DATA/I,J,K,II

          INCLUDE 'datmai.inc'

C         OPEN UNIT 20 FOR I/O
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBPLO)//'PLIB.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"PLOT LIBRARY DOES NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='TO INITIALIZE IT, USE "IPF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C       PROCEED
          OPEN(UNIT=24,ACCESS='DIRECT',FILE=trim(LIBPLO)//'PLIB.DAT',
     1    FORM='UNFORMATTED',RECL=(90*NRECL),STATUS='UNKNOWN')
          J=0
          K=0
          DO I=1,999
              READ(UNIT=24,REC=I) II,DATA
              IF(II.NE.0) THEN
C       FOUND A STORED LENS
                  J=J+1
              END IF
          END DO

          K=999-J
C       CLOSE UNIT 24 TO I/O
          CLOSE(24)

          WRITE(OUTLYNE,450) J
          CALL SHOWIT(1)
 450      FORMAT(I3,' PLOTS ON FILE.')
          WRITE(OUTLYNE,460) K
          CALL SHOWIT(1)
 460      FORMAT('ROOM FOR ',I3,' MORE PLOTS IN PLOT LIBRARY')

          RETURN
      END SUBROUTINE PLSTAT


C FUNCTION WVWT.INC

      FUNCTION WVWT(IIX,IIY,ERROR)

          IMPLICIT NONE

C       PLACES A WAVELENGTH/SPECTRAL WEIGHT
C       BOXED CAPTION ON A PLOT WITH THE UPPER LEFT HAND CORNER
C       OF THE BOX AT VDC IIX AND IIY

          CHARACTER NNTT1*80,NNTT2*80,NNTT3*80,B*80
          REAL WAVER,WAITR
          LOGICAL ERROR
          INTEGER COLPAS,WVWT,I,IIX,IIY,IIIY,NT1ANG,NT1SIZ
          INTEGER NB1,NB2
          integer IIX2,IIY2

          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'

          WVWT=0

          ERROR=.FALSE.
          IF(DEVTYP.NE.1.OR..NOT.PLEXIS.OR..NOT.GRASET) THEN
              ERROR=.TRUE.
              RETURN
          END IF

          CALL MY_SETFONT(1,0)
C     SET LETTER SIZE AND ANGLE
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
C     LIFT PEN, MOVE TO FRAME START
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          CALL MY_PLOT(IIX,IIY,0,0,0,10000,0,7000)
C     DROP PEN, DRAW BOX FOR LEGEND OF PLTDOTF PLTGOTF
          CALL MY_PLOT(IIX,IIY-1600,1,0,0,10000,0,7000)
          CALL MY_PLOT(IIX+2850,IIY-1600,1,0,0,10000,0,7000)
          CALL MY_PLOT(IIX+2850,IIY,1,0,0,10000,0,7000)
          CALL MY_PLOT(IIX,IIY,1,0,0,10000,0,7000)

          IIX2=IIX+150
          IIY2=IIY-150
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)

          DO I=1,10
              IF(I.EQ.1) THEN
                  WAVER=REAL(SYSTEM1(1))
                  WAITR=REAL(SYSTEM1(31))
              END IF
              IF(I.EQ.2) THEN
                  WAVER=REAL(SYSTEM1(2))
                  WAITR=REAL(SYSTEM1(32))
              END IF
              IF(I.EQ.3) THEN
                  WAVER=REAL(SYSTEM1(3))
                  WAITR=REAL(SYSTEM1(33))
              END IF
              IF(I.EQ.4) THEN
                  WAVER=REAL(SYSTEM1(4))
                  WAITR=REAL(SYSTEM1(34))
              END IF
              IF(I.EQ.5) THEN
                  WAVER=REAL(SYSTEM1(5))
                  WAITR=REAL(SYSTEM1(35))
              END IF
              IF(I.EQ.6) THEN
                  WAVER=REAL(SYSTEM1(71))
                  WAITR=REAL(SYSTEM1(76))
              END IF
              IF(I.EQ.7) THEN
                  WAVER=REAL(SYSTEM1(72))
                  WAITR=REAL(SYSTEM1(77))
              END IF
              IF(I.EQ.8) THEN
                  WAVER=REAL(SYSTEM1(73))
                  WAITR=REAL(SYSTEM1(78))
              END IF
              IF(I.EQ.9) THEN
                  WAVER=REAL(SYSTEM1(74))
                  WAITR=REAL(SYSTEM1(79))
              END IF
              IF(I.EQ.10) THEN
                  WAVER=REAL(SYSTEM1(75))
                  WAITR=REAL(SYSTEM1(80))
              END IF

C     CREATE CHARACTER VALUES
              IF(WAVER.GT.99.9) THEN
                  WRITE(B,180) WAVER
                  READ(B,200) NNTT1
                  NB1=10
              ELSE
                  WRITE(B,181) WAVER
                  READ(B,201) NNTT1
                  NB1=9
              END IF

              IF(WAITR.GT.99.9) THEN
                  WRITE(B,180) WAITR
                  READ(B,200) NNTT2
                  NB2=10
              ELSE
                  WRITE(B,181) WAITR
                  READ(B,201) NNTT2
                  NB2=8
              END IF

C     CREATE STRINGS TO WRITE
              IF(I.EQ.1) THEN
                  NNTT1='WV1  = '//NNTT1(1:NB1)
                  IF(WAVER.EQ.0.0)  NNTT1='WV1  =  UNDEF.'
                  NNTT2='WT. = '//NNTT2(1:NB2)
                  IF(WAVER.EQ.0.0)  NNTT2='WT. =  0.0'
              END IF
C     CREATE STRINGS TO WRITE
              IF(I.EQ.2) THEN
                  NNTT1='WV2  = '//NNTT1(1:NB1)
                  IF(WAVER.EQ.0.0)  NNTT1='WV2  =  UNDEF.'
                  NNTT2='WT. = '//NNTT2(1:NB2)
                  IF(WAVER.EQ.0.0)  NNTT2='WT. =  0.0'
              END IF
C     CREATE STRINGS TO WRITE
              IF(I.EQ.3) THEN
                  NNTT1='WV3  = '//NNTT1(1:NB1)
                  IF(WAVER.EQ.0.0)  NNTT1='WV3  =  UNDEF.'
                  NNTT2='WT. = '//NNTT2(1:NB2)
                  IF(WAVER.EQ.0.0)  NNTT2='WT. =  0.0'
              END IF
C     CREATE STRINGS TO WRITE
              IF(I.EQ.4) THEN
                  NNTT1='WV4  = '//NNTT1(1:NB1)
                  IF(WAVER.EQ.0.0)  NNTT1='WV4  =  UNDEF.'
                  NNTT2='WT. = '//NNTT2(1:NB2)
                  IF(WAVER.EQ.0.0)  NNTT2='WT. =  0.0'
              END IF
C     CREATE STRINGS TO WRITE
              IF(I.EQ.5) THEN
                  NNTT1='WV5  = '//NNTT1(1:NB1)
                  IF(WAVER.EQ.0.0)  NNTT1='WV5  =  UNDEF.'
                  NNTT2='WT. = '//NNTT2(1:NB2)
                  IF(WAVER.EQ.0.0)  NNTT2='WT. =  0.0'
              END IF
C     CREATE STRINGS TO WRITE
              IF(I.EQ.6) THEN
                  NNTT1='WV6  = '//NNTT1(1:NB1)
                  IF(WAVER.EQ.0.0)  NNTT1='WV6  =  UNDEF.'
                  NNTT2='WT. = '//NNTT2(1:NB2)
                  IF(WAVER.EQ.0.0)  NNTT2='WT. =  0.0'
              END IF
C     CREATE STRINGS TO WRITE
              IF(I.EQ.7) THEN
                  NNTT1='WV7  = '//NNTT1(1:NB1)
                  IF(WAVER.EQ.0.0)  NNTT1='WV7  =  UNDEF.'
                  NNTT2='WT. = '//NNTT2(1:NB2)
                  IF(WAVER.EQ.0.0)  NNTT2='WT. =  0.0'
              END IF
C     CREATE STRINGS TO WRITE
              IF(I.EQ.8) THEN
                  NNTT1='WV8  = '//NNTT1(1:NB1)
                  IF(WAVER.EQ.0.0)  NNTT1='WV8  =  UNDEF.'
                  NNTT2='WT. = '//NNTT2(1:NB2)
                  IF(WAVER.EQ.0.0)  NNTT2='WT. =  0.0'
              END IF
C     CREATE STRINGS TO WRITE
              IF(I.EQ.9) THEN
                  NNTT1='WV9  = '//NNTT1(1:NB1)
                  IF(WAVER.EQ.0.0)  NNTT1='WV9  =  UNDEF.'
                  NNTT2='WT. = '//NNTT2(1:NB2)
                  IF(WAVER.EQ.0.0)  NNTT2='WT. =  0.0'
              END IF
C     CREATE STRINGS TO WRITE
              IF(I.EQ.10) THEN
                  NNTT1='WV10 = '//NNTT1(1:NB1)
                  IF(WAVER.EQ.0.0)  NNTT1='WV10 =  UNDEF.'
                  NNTT2='WT. = '//NNTT2(1:NB2)
                  IF(WAVER.EQ.0.0)  NNTT2='WT. =  0.0'
              END IF
C
C     NOW MOVE TO CORRECT POSITION
C     AND WRITE THE STRING
              IF(I.EQ.1)  IIIY=0
              IF(I.EQ.2)  IIIY=150
              IF(I.EQ.3)  IIIY=300
              IF(I.EQ.4)  IIIY=450
              IF(I.EQ.5)  IIIY=600
              IF(I.EQ.6)  IIIY=750
              IF(I.EQ.7)  IIIY=900
              IF(I.EQ.8)  IIIY=1050
              IF(I.EQ.9)  IIIY=1200
              IF(I.EQ.10) IIIY=1350
              CALL MY_SETCHARASPECT(1.25,1.25)
              CALL MY_PLOT(IIX2,IIY2-IIIY,0,0,0,10000,0,7000)
              CALL MY_JUSTSTRING(IIX2,IIY2-IIIY,NNTT1(1:17),NT1ANG,NT1SIZ,3)
              IF(WAVER.NE.0.0) THEN
                  IF(WAVER.GT.99.9) THEN
                      CALL MY_SETCHARASPECT(1.5,1.5)
                      CALL MY_SETFONT(2,0)
                      NNTT3='m'
                      CALL MY_PLOT(IIX2+1300,IIY2-IIIY,0,0,0,10000,0,7000)
                      CALL MY_JUSTSTRING(IIX2+1300,IIY2-IIIY,NNTT3(1:1),NT1ANG,NT1SIZ,3)
                      CALL MY_SETFONT(1,0)
                      CALL MY_SETCHARASPECT(1.25,1.25)
                  END IF
                  IF(WAVER.LE.99.9) THEN
                      CALL MY_SETCHARASPECT(1.5,1.5)
                      CALL MY_SETFONT(2,0)
                      NNTT3='m'
                      CALL MY_PLOT(IIX2+1200,IIY2-IIIY,0,0,0,10000,0,7000)
                      CALL MY_JUSTSTRING(IIX2+1200,IIY2-IIIY,NNTT3(1:1),NT1ANG,NT1SIZ,3)
                      CALL MY_SETFONT(1,0)
                      CALL MY_SETCHARASPECT(1.25,1.25)
                  END IF
              END IF
              CALL MY_PLOT(IIX2+1500,IIY2-IIIY,0,0,0,10000,0,7000)
              CALL MY_JUSTSTRING(IIX2+1500,IIY2-IIIY,NNTT2(1:16),NT1ANG,NT1SIZ,3)
              CALL MY_SETCHARASPECT(1.5,1.5)

          END DO
          RETURN
180       FORMAT(G10.4)
200       FORMAT(A10)
181       FORMAT(F8.5)
201       FORMAT(A8)
      END FUNCTION WVWT


      SUBROUTINE IPF

          IMPLICIT NONE
C
C       INITIALIZES OR BLANKS OUT THE CURRENT
C       PLOT LIBRARY DIRECTORY.
C
          INTEGER I,II,N,III
          LOGICAL EXISJK,OPENJK
          CHARACTER BLANK*80,FN*10,AN*3

          INCLUDE 'datmai.inc'

          BLANK=AA//AA//AA//AA

          OPEN(UNIT=24,ACCESS='DIRECT',FILE=trim(LIBPLO)//'PLIB.DAT',FORM=
     1    'UNFORMATTED',RECL=(90*NRECL),STATUS='UNKNOWN')
          CLOSE(24,STATUS='DELETE')
          DO N=1,999
              CALL CCOONN(N,AN)
              IF(N.GT.0.AND.N.LE.9) FN='PLT00'//AN(3:3)//'.DAT'
              IF(N.GT.9.AND.N.LE.99) FN='PLT0'//AN(2:3)//'.DAT'
              IF(N.GT.999.AND.N.LE.999) FN='PLT'//AN(1:3)//'.DAT'
              EXISJK=.FALSE.
              OPENJK=.FALSE.
              INQUIRE(FILE=trim(LIBPLO)//FN,EXIST=EXISJK)
              INQUIRE(FILE=trim(LIBPLO)//FN,OPENED=OPENJK)
              IF(EXISJK) THEN
                  IF(.NOT.OPENJK) OPEN(UNIT=77
     1            ,FILE=trim(LIBPLO)//FN,FORM='UNFORMATTED',ACCESS='DIRECT'
     2            ,RECL=(NRECL*42),STATUS='UNKNOWN')
                  CLOSE(77,STATUS='DELETE')
              END IF
          END DO
C
C       OPEN UNIT 24 FOR I/O
C
          OPEN(UNIT=24,ACCESS='DIRECT',FILE=trim(LIBPLO)//'PLIB.DAT',FORM=
     1    'UNFORMATTED',RECL=(90*NRECL),STATUS='UNKNOWN')
C
          II=0
          III=0
          DO I=1,999
              WRITE(UNIT=24,REC=I)II,BLANK,III
          END DO

          CLOSE(24 )

          OUTLYNE='PLOT LIBRARY INITIALIZED'
          CALL SHOWIT(1)
          RETURN
      END SUBROUTINE IPF


      SUBROUTINE MY_INIPLT
          USE GLOBALS
          IMPLICIT NONE
          INTEGER ALLOERR
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8
          CHARACTER STRINGER*1,NEUTLINE*42

          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'

          CALL CLOSE_FILE(28,0)
          DEALLOCATE(NEUTARRAY,STAT=ALLOERR)

          CALL MY_DEL_NEUT
          ALLOCATE(NEUTARRAY(1:MAXNEUTRAL),STAT=ALLOERR)

C     NOW OPEN NEW FILE NEUTRAL.DAT FOR OUTPUT, IT IS CURRENTLY EMPTY
C
          OPEN(UNIT=28
     1    ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2    ,RECL=(NRECL*42),STATUS='REPLACE')
          I1=0
          I2=0
          I3=0
          I4=0
          I5=0
          I6=0
          I7=0
          I8=0
          NEUTTOTAL=1
          STRINGER='A'
          WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE

 1000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
          RETURN
      END SUBROUTINE MY_INIPLT


      SUBROUTINE PLOT_CONTOUR_OPD(PCOUNT,CON_ARRAY,NSTEP,IU,OPDPEAK,OPDPIT)
          USE GLOBALS
          IMPLICIT NONE
          INTEGER I,J,IU
          INTEGER NSTEP,PCOUNT,CON_ARRAY
          DIMENSION CON_ARRAY(1:PCOUNT,1:PCOUNT)
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8
          CHARACTER STRINGER*1,NEUTLINE*42
          REAL OPDPEAK,OPDPIT

          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'

          I1=PCOUNT
          I2=NSTEP
          I3=0
          I4=0
          I5=0
          I6=0
          I7=0
          I8=0
          CALL check_bounds(I1,I2,I3,I4,I5,I6,I7,I8)
          NEUTTOTAL=NEUTTOTAL+1
          STRINGER='K'
          WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE

          NEUTTOTAL=NEUTTOTAL+1
          STRINGER='O'
          WRITE(NEUTLINE,1011) STRINGER,OPDPEAK,OPDPIT
 1011     FORMAT(A1,E15.7,E15.7,10X)
          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2)
     1    CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  I1=CON_ARRAY(I,J)
                  I2=0
                  I3=0
                  I4=0
                  I5=0
                  I6=0
                  I7=0
                  I8=0
                  CALL check_bounds(I1,I2,I3,I4,I5,I6,I7,I8)
                  NEUTTOTAL=NEUTTOTAL+1
                  STRINGER='L'
                  WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

                  IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
                  NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
              END DO
          END DO
 1000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)

C     NOW WRITE THE ARRAY VALUES
          IU=1
          RETURN
      END SUBROUTINE PLOT_CONTOUR_OPD


      SUBROUTINE PLOT_CONTOUR_APD(PCOUNT,CON_ARRAY,NSTEP,IU)
          USE GLOBALS
          IMPLICIT NONE
          INTEGER I,J,IU
          INTEGER NSTEP,PCOUNT,CON_ARRAY
          DIMENSION CON_ARRAY(1:PCOUNT,1:PCOUNT)
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8
          CHARACTER STRINGER*1,NEUTLINE*42
          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'

          I1=PCOUNT
          I2=NSTEP
          I3=0
          I4=0
          I5=0
          I6=0
          I7=0
          I8=0
          CALL check_bounds(I1,I2,I3,I4,I5,I6,I7,I8)
          NEUTTOTAL=NEUTTOTAL+1
          STRINGER='M'
          WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  I1=CON_ARRAY(I,J)
                  I2=0
                  I3=0
                  I4=0
                  I5=0
                  I6=0
                  I7=0
                  I8=0
                  CALL check_bounds(I1,I2,I3,I4,I5,I6,I7,I8)
                  NEUTTOTAL=NEUTTOTAL+1
                  STRINGER='N'
                  WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

                  IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
                  NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
              END DO
          END DO
 1000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)

C     NOW WRITE THE ARRAY VALUES
          IU=2
          RETURN
      END SUBROUTINE PLOT_CONTOUR_APD


      SUBROUTINE MY_ENDPLT
          USE GLOBALS
          IMPLICIT NONE
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8
          CHARACTER STRINGER*1,NEUTLINE*80

          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          
          EXIS28=.FALSE.
          OPEN28=.FALSE.
          INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',EXIST=EXIS28)
          INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',OPENED=OPEN28)
          IF(EXIS28.AND.OPEN28) THEN
              NEUTLINE=NEUTARRAY(NEUTTOTAL+1)
              IF(NEUTLINE(1:1).NE.'B') THEN
                  NEUTTOTAL=NEUTTOTAL+1
                  I1=0
                  I2=0
                  I3=0
                  I4=0
                  I5=0
                  I6=0
                  I7=0
                  I8=0
                  STRINGER='B'
                  WRITE(NEUTLINE,2000) NEUTTOTAL
                  NEUTARRAY(1)=NEUTLINE
 2000             FORMAT(I9,32X)
                  IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
                  NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
              ELSE
                  WRITE(NEUTLINE,2000) NEUTTOTAL
                  NEUTARRAY(1)=NEUTLINE
              END IF
              CLOSE(28 )
          END IF
          F34=0
          MSG=.TRUE.
          RETURN
      END SUBROUTINE MY_ENDPLT

      SUBROUTINE MY_MARKER(II1,II2,II3,II4)
          USE GLOBALS
          IMPLICIT NONE
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8
          INTEGER II1,II2,II3,II4
          CHARACTER STRINGER*1,NEUTLINE*42
          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'
          I1=II1
          I2=II2
          I3=II3
          I4=II4
          I5=0
          I6=0
          I7=0
          I8=0
          CALL check_bounds(I1,I2,I3,I4,I5,I6,I7,I8)
          STRINGER='C'
          NEUTTOTAL=NEUTTOTAL+1
          WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
 1000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
          RETURN
      END SUBROUTINE MY_MARKER

        
      SUBROUTINE MY_JUSTSTRING(II1,II2,C1,II3,II4,II5)
          USE GLOBALS
          IMPLICIT NONE
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8
          INTEGER LENN,II1,II2,II3,II4,II5
          CHARACTER STRINGER*1
          CHARACTER C1A*20,C1B*20,C1C*20,C1D*20,NEUTLINE*42
          CHARACTER*(*) C1
          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'
          I1=II1
          I2=II2
          I3=II3
          I4=II4
          I5=II5
          I6=LEN_TRIM(C1)
          LENN=LEN_TRIM(C1)
          I7=0
          I8=0
          CALL check_bounds(I1,I2,I3,I4,I5,I6,I7,I8)
          STRINGER='D'
          NEUTTOTAL=NEUTTOTAL+1
          WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1) = NEUTLINE
 1000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
          C1A='                    '
          C1B='                    '
          C1C='                    '
          C1D='                    '
          IF(LENN.GE.0.AND.LENN.LE.20) THEN
              C1A(1:LENN)=C1(1:LENN)
          END IF
          IF(LENN.GT.20.AND.LENN.LE.40) THEN
              C1A(1:20)=C1(1:20)
              C1B(1:(LENN-20))=C1(21:LENN)
          END IF
          IF(LENN.GT.40.AND.LENN.LE.60) THEN
              C1A(1:20)=C1(1:20)
              C1B(1:20)=C1(21:40)
              C1C(1:(LENN-40))=C1(41:LENN)
          END IF
          IF(LENN.GT.60.AND.LENN.LE.80) THEN
              C1A(1:20)=C1(1:20)
              C1B(1:20)=C1(21:40)
              C1C(1:20)=C1(41:60)
              C1D(1:(LENN-60))=C1(61:LENN)
          END IF

          NEUTTOTAL=NEUTTOTAL+1

          WRITE(NEUTLINE,5000) C1A

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2)
     1    CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
          NEUTTOTAL=NEUTTOTAL+1

          WRITE(NEUTLINE,5000) C1B
          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2)
     1    CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
          NEUTTOTAL=NEUTTOTAL+1

          WRITE(NEUTLINE,5000) C1C
          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2)
     1    CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
          NEUTTOTAL=NEUTTOTAL+1

          WRITE(NEUTLINE,5000) C1D

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2)
     1    CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE

          call gnuplotlabel(I1,I2,C1A//C1B//C1C//C1D,I3,I4)

 5000     FORMAT(A20,22X)
          RETURN
      END SUBROUTINE MY_JUSTSTRING


      SUBROUTINE MY_COLTYP(COLPAS)
          USE GLOBALS
          IMPLICIT NONE
          CHARACTER STRINGER*1,NEUTLINE*42
          INTEGER COLPAS
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8

          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'

          I1=COLPAS
          I2=0
          I3=0
          I4=0
          I5=0
          I6=0
          I7=0
          I8=0
          CALL check_bounds(I1,I2,I3,I4,I5,I6,I7,I8)
          STRINGER='E'
          NEUTTOTAL=NEUTTOTAL+1
          WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
 1000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
          RETURN
      END SUBROUTINE MY_COLTYP


      SUBROUTINE MY_SETPAL(II1)
          USE GLOBALS
          IMPLICIT NONE
          CHARACTER STRINGER*1,NEUTLINE*42
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8
          INTEGER II1

          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'
          
          I1=II1
          I2=0
          I3=0
          I4=0
          I5=0
          I6=0
          I7=0
          I8=0
          CALL check_bounds(I1,I2,I3,I4,I5,I6,I7,I8)
          STRINGER='G'
          NEUTTOTAL=NEUTTOTAL+1
          WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
 1000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
          RETURN
      END SUBROUTINE MY_SETPAL

      
      SUBROUTINE MY_SETFONT(II1,II2)
          USE GLOBALS
          IMPLICIT NONE
          CHARACTER STRINGER*1,NEUTLINE*42
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8
          INTEGER II1,II2
          
          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'
          
          I1=II1
          I2=II2
          I3=0
          I4=0
          I5=0
          I6=0
          I7=0
          I8=0
          CALL check_bounds(I1,I2,I3,I4,I5,I6,I7,I8)
          STRINGER='H'
          NEUTTOTAL=NEUTTOTAL+1
          WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
 1000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
          RETURN
      END SUBROUTINE MY_SETFONT
        

      SUBROUTINE MY_PLOT(II1,II2,II3,II4,II5,II6,II7,II8)
          USE GLOBALS
          IMPLICIT NONE
          CHARACTER STRINGER*1,NEUTLINE*42
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8
          INTEGER II1,II2,II3,II4,II5,II6,II7,II8

          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'
          
          I1=II1
          I2=II2
          I3=II3
          I4=II4
          I5=II5
          I6=II6
          I7=II7
          I8=II8
          CALL check_bounds(I1,I2,I3,I4,I5,I6,I7,I8)
          PENPOSX=II1
          PENPOSY=II2
          STRINGER='I'
          NEUTTOTAL=NEUTTOTAL+1
          WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

          call drawdatasave(I1,I2,I3,I4)

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
 1000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
          RETURN
      END SUBROUTINE MY_PLOT

      
      SUBROUTINE MY_PLOTC(II1,II2,II3,II4,II5,II6,II7,II8)
          USE GLOBALS
          IMPLICIT NONE
          CHARACTER STRINGER*1,NEUTLINE*42
          INTEGER I1,I2,I3,I4,I5,I6,I7,I8
          INTEGER II1,II2,II3,II4,II5,II6,II7,II8
          
          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'
          
          I1=II1
          I2=II2
          I3=II3
          I4=II4
          I5=II5
          I6=II6
          I7=II7
          I8=II8
          CALL check_bounds(I1,I2,I3,I4,I5,I6,I7,I8)
          PENPOSX=II1
          PENPOSY=II2
          STRINGER='J'
          NEUTTOTAL=NEUTTOTAL+1
          WRITE(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8

          call drawdatasave2(I1,I2,I3,I4,I5,I6,I7,I8)

          IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
          NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
 1000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)

      END SUBROUTINE MY_PLOTC

        
      SUBROUTINE PLTSYM

          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT SYMBOL COMMAND AT THE CMD LEVEL

          CHARACTER SYM*31
          INTEGER COLPAS,IXPPEE,IYPPEE

          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       PLOT SYMBOL
C
          IF(WQ.EQ.'SYMBOL') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT SYMBOL" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT SYMBOL" TAKES NO NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  IF(SYMB.EQ.1)  SYM='+'
                  IF(SYMB.EQ.2)  SYM='x'
                  IF(SYMB.EQ.3)  SYM='SQUARE'
                  IF(SYMB.EQ.4)  SYM='TRIANGLE'
                  IF(SYMB.EQ.5)  SYM='INVERTED TRIANGLE'
                  IF(SYMB.EQ.6)  SYM='QUARTERED SQUARE'
                  IF(SYMB.EQ.7)  SYM='CROSSED TRIANGLE'
                  IF(SYMB.EQ.8)  SYM='CROSSED INVERTED TRIANGLE'
                  IF(SYMB.EQ.9)  SYM='SQUARE WITH X'
                  IF(SYMB.EQ.10) SYM='TRIANGLE PLUS INVERTED TRIANGLE'
                  WRITE(OUTLYNE,874) SYM,SYMB
                  CALL SHOWIT(1)
 874              FORMAT('CURRENT SYMBOL IS ',A15,1X,'SYMBOL NUMBER = ',I1)
                  RETURN
              ELSE
              END IF
              IF(INT(W1).LT.1.OR.INT(W1).GT.10) THEN
                  OUTLYNE=
     1            'VALID SYMBOL NUMBERS RANGE FROM 1 TO 10'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W2).LT.1.OR.INT(W2).GT.9) THEN
                  OUTLYNE=
     1            'VALID SYMBOL SIZES RANGE FROM 1 TO 9'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF3.EQ.1.OR.DF4.EQ.1) THEN
                  OUTLYNE=
     1            'X AND Y-COORDINATES MUST BE ENTERED IN NUMERIC WORDS 3 AND 4'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              IF(DF1.EQ.1) W1=9.0D0
              IF(DF2.EQ.1) W2=1.0D0
              SYMB=INT(W1)
              SYMSIZ=INT(W2)
              IXPPEE=INT(W3)
              IYPPEE=INT(W4)
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              CALL MY_MARKER(SYMB,SYMSIZ,IXPPEE,IYPPEE)
              RETURN
          END IF
          RETURN
      END SUBROUTINE PLTSYM


      SUBROUTINE PSTART

          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE ALL PLOTTING INITIALIZATION
C
          CHARACTER BLANK*80

          INTEGER LLX,LLY,URX,URY,COLBACC
          COMMON/VIEWER/LLX,LLY,URX,URY

          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'

          BLANK=AA//AA//AA//AA

          PLEXIS=.FALSE.
          IF(DEVTYP.EQ.1) THEN
C       PROCEED
          PPLI(1:60)=LI(1:60)
          CALL MY_INIPLT

          LLX=-10
          LLY=-10
          URX=10010
          URY=7010

C     LIGHT BLUE (DEFAULT) BACKGROUND
C     DOES IT NEED TO CHANGE ?
C     WHITE BACKGROUND         COLBAC=0
C     LIGHT YELLOW BACKGROUND  COLBAC=1
C     LIGHT MAGENTA BACKGROUND COLBAC=2
C     LIGHT RED BACKGROUND     COLBAC=3
C     LIGHT CYAN BACKGROUND    COLBAC=4
C     LIGHT GREEN BACKGROUND   COLBAC=5
C     LIGHT BLUE BACKGROUND    COLBAC=6
C     DARK GREY BACKGROUND     COLBAC=7
C     LIGHT GREY BACKGROUND    COLBAC=8
C     DARK YELLOW BACKGROUND   COLBAC=9
C     DARK MAGENTA BACKGROUND  COLBAC=10
C     DARK RED BACKGROUND      COLBAC=11
C     DARK CYAN BACKGROUND     COLBAC=12
C     DARK GREEN BACKGROUND    COLBAC=13
C     DARK BLUE BACKGROUND     COLBAC=14
C     BLACK BACKGROUND         COLBAC=15
              COLBACC=COLBAC
C     RESET THE BACKGROUND COLOR AND THE FONT STYLE
              CALL MY_SETPAL(COLBACC)
              CALL MY_SETFONT(1,0)
              RETURN
          ELSE
C       PROCEED
          END IF
C       NOT A VALID DEVICE TYPE
          OUTLYNE=
     1    '"PLOT NEW" MUST BE ISSUED BEFORE PLOTTING CAN PROCEED'
          CALL SHOWIT(1)
          OUTLYNE='RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
      END SUBROUTINE PSTART


      SUBROUTINE PSTOP

          IMPLICIT NONE
C
C       THIS PROGRAM STOPS GRAPHIC OUTPUT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'

C       STOP PLOTTING
          F34=0
          MSG=.FALSE.
          DEVTYP=0
          GRASET=.FALSE.
          PLEXIS=.FALSE.
          MSG=.TRUE.
          RETURN
      END SUBROUTINE PSTOP

      
      SUBROUTINE COLORS
C
C               COLORS AND THEIR INTEGER CODES
C
C               0 = WHITE
C               1 = LIGHT YELLOW
C               2 = LIGHT MAGENTA
C               3 = LIGHT RED
C               4 = LIGHT CYAN
C               5 = LIGHT GREEN
C               6 = LIGHT BLUE
C               7 = DARK GREY
C               8 = LIGHT GREY
C               9 = DARK YELLOW
C              10 = DARK MAGENTA
C              11 = DARK RED
C              12 = DARK CYAN
C              13 = DARK GREEN
C              14 = DARK BLUE
C              15 = BLACK
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE COLORS. THIS IS THE SUBROUTINE WHICH
C       SETS THE PROGRAM COLORS.

          INTEGER COLANS

          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'

          IF(STI.EQ.0) THEN
C     NO QUERY
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE=
     1            '"COLORSET" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(WQ.NE.'RESET') THEN
                  IF(S1.EQ.0.OR.SQ.EQ.0) THEN
                      OUTLYNE=
     1                '"COLORSET" REQUIRES EXPLICIT QUALIFIER AND'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     WQ WAS RESET
              END IF
              IF(WQ.EQ.'RESET') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE=
     1                '"COLORSET RESET" TAKES NO ADDITIONAL INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C     WQ WAS NOT RESET
              END IF
              IF(WQ.EQ.'RESET') THEN

                  CALL MY_INIPLT

C     RESETTING PROGRAM COLORS TO DEFAULTS

                  COLDEF=15
                  COLRAY=15
                  COLCLP=3
                  COLCOB=9
                  COLEDG=1
                  COLPRO=1
                  COLAXS=15
                  COLBAC=0

                  COLR1=15
                  COLR2=12
                  COLR3=2
                  COLR4=3
                  COLR5=4
                  COLR6=5
                  COLR7=6
                  COLR8=7
                  COLR9=8
                  COLR10=9

                  COLFRM=15
                  COLLBL=15
                  COLSPE=15
                  COLPEN=15
                  RETURN
              END IF
              IF(WQ.NE.'RAYS'.AND.WQ.NE.'CLAP'.AND.WQ.NE.'COBS'.AND.
     1        WQ.NE.'EDGE'.AND.WQ.NE.'PROF'.AND.WQ.NE.'AXIS'.AND.
     1        WQ.NE.'GBAC'.AND.WQ.NE.'WAV1'.AND.WQ.NE.'WAV2'.AND.
     1        WQ.NE.'WAV3'.AND.WQ.NE.'WAV4'.AND.WQ.NE.'WAV5'.AND.
     1        WQ.NE.'FRAM'.AND.WQ.NE.'LABL'.AND.
     1        WQ.NE.'SPEC'.AND.WQ.NE.'PEN'.AND.
     1        WQ.NE.'WAV6'.AND.WQ.NE.'WAV7'.AND.WQ.NE.'WAV8'.AND.
     1        WQ.NE.'WAV9'.AND.WQ.NE.'WAV10')THEN
                  OUTLYNE=
     1            'INVALID QUALIFIER USED WITH "COLORSET"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.-1.OR.INT(W1).GT.16) THEN
                  OUTLYNE=
     1            'NUMERIC COLOR VALUES MUST RANGE FROM -1 TO 15'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C     PROCEED WITH SETTING OF COLORS
              IF(WQ.EQ.'RAYS') COLRAY=INT(W1)
              IF(WQ.EQ.'CLAP') COLCLP=INT(W1)
              IF(WQ.EQ.'COBS') COLCOB=INT(W1)
              IF(WQ.EQ.'EDGE') COLEDG=INT(W1)
              IF(WQ.EQ.'PROF') COLPRO=INT(W1)
              IF(WQ.EQ.'AXIS') COLAXS=INT(W1)
              IF(WQ.EQ.'GBAC') COLBAC=INT(W1)
              IF(WQ.EQ.'WAV1') COLR1=INT(W1)
              IF(WQ.EQ.'WAV2') COLR2=INT(W1)
              IF(WQ.EQ.'WAV3') COLR3=INT(W1)
              IF(WQ.EQ.'WAV4') COLR4=INT(W1)
              IF(WQ.EQ.'WAV5') COLR5=INT(W1)
              IF(WQ.EQ.'WAV6') COLR6=INT(W1)
              IF(WQ.EQ.'WAV7') COLR7=INT(W1)
              IF(WQ.EQ.'WAV8') COLR8=INT(W1)
              IF(WQ.EQ.'WAV9') COLR9=INT(W1)
              IF(WQ.EQ.'WAV10') COLR10=INT(W1)
              IF(WQ.EQ.'FRAM') COLFRM=INT(W1)
              IF(WQ.EQ.'LABL') COLLBL=INT(W1)

              IF(WQ.EQ.'GBAC')  THEN
                  IF(W1.LT.-1.0D0.OR.W1.GT.15.0D0) THEN
                      OUTLYNE=
     1                '"COLORSET GBAC" ONLY USES COLOR NUMBERS -1 TO 15'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C     INPUT OK
                  END IF
              ELSE
C     NOT GBAC
              END IF
              IF(WQ.EQ.'SPEC') COLSPE=INT(W1)
              IF(WQ.EQ.'PEN') COLPEN=INT(W1)

              RETURN

          ELSE
C     QUERY IS IMPLEMENTED HERE
C     CASE OF "COLORSET ?"
              IF(SQ.EQ.0) THEN
                  OUTLYNE='"COLORSET" QUERRY REQUIRES A VALID QALIFIER WORD'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     IS SQ=1 ?
              IF(SQ.EQ.1) THEN
                  IF(WQ.NE.'RAYS'.AND.WQ.NE.'CLAP'.AND.WQ.NE.'COBS'.AND.
     1            WQ.NE.'EDGE'.AND.WQ.NE.'PROF'.AND.WQ.NE.'AXIS'.AND.
     1            WQ.NE.'GBAC'.AND.WQ.NE.'WAV1'.AND.WQ.NE.'WAV2'.AND.
     1            WQ.NE.'WAV3'.AND.WQ.NE.'WAV4'.AND.WQ.NE.'WAV5'.AND.
     1            WQ.NE.'FRAM'.AND.WQ.NE.'LABL'.AND.WQ.NE.'AIRY'.AND.
     1            WQ.NE.'WAV6'.AND.WQ.NE.'WAV7'.AND.WQ.NE.'WAV8'.AND.
     1            WQ.NE.'WAV9'.AND.WQ.NE.'WAV10'.AND.WQ.NE.'MARK'.AND.
     1            WQ.NE.'SPEC'.AND.WQ.NE.'PEN')THEN
                      OUTLYNE=
     1                'INVALID QUALIFIER USED WITH "COLORSET" QUERRY'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C     QUALIFIER OK
                  IF(WQ.EQ.'RAYS')  COLANS=COLRAY
                  IF(WQ.EQ.'CLAP')  COLANS=COLCLP
                  IF(WQ.EQ.'COBS')  COLANS=COLCOB
                  IF(WQ.EQ.'EDGE')  COLANS=COLEDG
                  IF(WQ.EQ.'PROF')  COLANS=COLPRO
                  IF(WQ.EQ.'AXIS')  COLANS=COLAXS
                  IF(WQ.EQ.'GBAC')  COLANS=COLBAC
                  IF(WQ.EQ.'WAV1')  COLANS=COLR1
                  IF(WQ.EQ.'WAV2')  COLANS=COLR2
                  IF(WQ.EQ.'WAV3')  COLANS=COLR3
                  IF(WQ.EQ.'WAV4')  COLANS=COLR4
                  IF(WQ.EQ.'WAV5')  COLANS=COLR5
                  IF(WQ.EQ.'WAV6')  COLANS=COLR6
                  IF(WQ.EQ.'WAV7')  COLANS=COLR7
                  IF(WQ.EQ.'WAV8')  COLANS=COLR8
                  IF(WQ.EQ.'WAV9')  COLANS=COLR9
                  IF(WQ.EQ.'WAV10') COLANS=COLR10
                  IF(WQ.EQ.'FRAM')  COLANS=COLFRM
                  IF(WQ.EQ.'LABL')  COLANS=COLLBL
                  IF(WQ.EQ.'SPEC')  COLANS=COLSPE
                  IF(WQ.EQ.'AIRY')  COLANS=COLAIR
                  IF(WQ.EQ.'MARK')  COLANS=COLMRK
                  IF(WQ.EQ.'PEN')   COLANS=COLPEN
100               FORMAT('"',A4,'"',' IS CURRENTLY SET TO COLOR NUMBER ',I3)
                  WRITE(OUTLYNE,100)WQ(1:4),COLANS
                  CALL SHOWIT(1)
              END IF
          END IF

      END SUBROUTINE COLORS


      SUBROUTINE MY_DEL_NEUT
          USE GLOBALS
          IMPLICIT NONE
          LOGICAL EXISNEUT,OPENNEUT
          INTEGER ALLOERR
          INCLUDE 'datmai.inc'
          INQUIRE(UNIT=28,EXIST=EXISNEUT)
          INQUIRE(UNIT=28,OPENED=OPENNEUT)
          IF(EXISNEUT.AND..NOT.OPENNEUT) THEN
              OPEN(UNIT=28
     1        ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2        ,RECL=(NRECL*42),STATUS='UNKNOWN')
              CALL CLOSE_FILE(28,0)
          END IF
          IF(EXISNEUT.AND.OPENNEUT) THEN
              CALL CLOSE_FILE(28,0)
          END IF
          DEALLOCATE(NEUTARRAY,STAT=ALLOERR)
          RETURN
      END SUBROUTINE MY_DEL_NEUT


      SUBROUTINE RESIZE_NEUT
C     THIS ROUTINE DOUBLES THE SIZE OF NEUTARRAY IF NEEDED
          USE GLOBALS
          IMPLICIT NONE
          CHARACTER*80 NEUTTEMP
          INTEGER ALLOERR,I
          DIMENSION NEUTTEMP(:)
          ALLOCATABLE :: NEUTTEMP
          INCLUDE 'dathgr.inc'
          MAXNEUTRAL=MAXNEUTRAL*2
          ALLOCATE (NEUTTEMP(MAXNEUTRAL),STAT=ALLOERR)
          DO I=1,MAXNEUTRAL/2
              NEUTTEMP(I)=NEUTARRAY(I)
          END DO
          DEALLOCATE(NEUTARRAY,STAT=ALLOERR)
          ALLOCATE(NEUTARRAY(MAXNEUTRAL),STAT=ALLOERR)
          DO I=1,MAXNEUTRAL/2
              NEUTARRAY(I)=NEUTTEMP(I)
          END DO
          DEALLOCATE(NEUTTEMP,STAT=ALLOERR)
          RETURN
      END SUBROUTINE RESIZE_NEUT


      SUBROUTINE PLIBRY
          USE opsys
          USE GLOBALS
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS PLOT LIBRARY COMMANDS
C       PLIB P, PLIB PUT, PLIB DEL AND PLIB GET.
C                       DEFINE VARIABLES
C
          CHARACTER DATA*80,STRINGER*1,
     1    LLIP*80,DDATE*10,TTIME*8,AN*3,NEUTLINE*42
     2    ,FN*10,NM*8,TTTIM*8,DDDAT*10,DRWNAM*11

          COMMON/DRWTAG/DRWNAM

          CHARACTER PFILENAME*90,C1A*20,C1B*20,C1C*20,C1D*20

          COMMON/STRNGR/DDDAT,TTTIM,NM,FN

          LOGICAL EXISJK
          REAL JJ1,JJ2
          INTEGER SING,LCNT,I,II,J,N,PFCOUNTER,I1,I2,I3,I4,I5,I6,I7,
     1    I8,ALLOERR,JJ
          INTEGER DUMTOT,FLG(0:20),ALL

          COMMON/FFL/FLG

          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'dathgr.inc'

          ALL=0
          SING=0
C
C*************************************************************
C       SPECIFIC CHECKS FOR PLIB P
C
C       LIB P CAN TAKE NUMERIC INPUT FROM NW1 AND NW2
C       NW1 CAN BE 1 TO 999 OR BLANK
C       NW2 CAN BE 2 TO 999 OR BLANK
C       NW1 MUST BE LESS THAN NW2
C
          IF(WQ.EQ.'P') THEN
              IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1              '"PLIB P" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       INPUT OK
              END IF
              IF(W1.LT.1.0.AND.DF1.NE.1.OR.
     1        W1.GT.999.0.AND.DF1.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 1 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(W2.LT.2.0.AND.DF2.NE.1.OR.
     1        W2.GT.999.0.AND.DF2.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 2 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(S2.EQ.1) THEN

                  IF(W1.GE.W2) THEN
                      OUTLYNE=
     1                  'NUMERIC WORD 1 MUST BE LESS THAN NUMERIC WORD 2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C       NW2 IS BLANK
                  SING=1
              END IF
C       IS NW1 AND NW2 DEFAULT
              IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
C       NO NUMERIC INPUT, SET THE ALL VARIABLE TO 1
                  ALL=1
              END IF
          END IF
C**********************************************************
C       SPECIFIC CHECKS FOR PLIB PUT
C
C       LIB PUT CAN TAKE NUMERIC INPUT FROM NW1 ONLY
C       NW1 CAN BE 1 TO 999 OR BLANK
C
C       PLIB PUT NEEDS THE FILE NEUTRAL.DAT TO EXIST AND FOR
C       IT TO BE CLOSED
          IF(WQ.EQ.'PUT') THEN
              EXIS28=.FALSE.
              OPEN28=.FALSE.
              INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',EXIST=EXIS28)
              INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',OPENED=OPEN28)
              IF(.NOT.OPEN28.AND.EXIS28) THEN
C       PROCEED WITH PLIB PUT, ELSE RETURN WITH A MESSAGE
                  GO TO 89
              ELSE
              END IF
C
C     FILE DOES NOT EXIST
              IF(.NOT.EXIS28) THEN
                  OUTLYNE='NO PLOTFILE EXISTS TO BE FILED'
                  CALL SHOWIT(1)
                  OUTLYNE='NO "LIB PUT" CAN BE DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
 89           CONTINUE
          ELSE
C     WQ.NOT.'PUT'
          END IF
          IF(WQ.EQ.'PUT') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1              '"PLIB PUT" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       INPUT OK
              END IF
              IF(W1.LT.1.0.AND.DF1.NE.1.OR.
     1        W1.GT.999.0.AND.DF1.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 1 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
C       IS NW1 DEFAULT
              IF(DF1.EQ.1) THEN
C       NO NUMERIC INPUT, SET THE ALL VARIABLE TO 1
                  ALL=1
              ELSE
              END IF
          ELSE
C       NOT PLIB PUT
          END IF

C**********************************************************
C       SPECIFIC CHECKS FOR PLIB GET
C
C       LIB GET CAN TAKE NUMERIC INPUT FROM NW1 ONLY
C       NW1 CAN BE 1 TO 999 OR BLANK
C
          IF(WQ.EQ.'GET') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1              '"PLIB GET" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       INPUT OK
              END IF
              IF(W1.LT.1.0.AND.DF1.NE.1.OR.
     1        W1.GT.999.0.AND.DF1.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 1 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
C       IS NW1 DEFAULT
              IF(DF1.EQ.1) THEN
                  OUTLYNE='"PLIB GET" REQUIRES EXPLICIT NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
          ELSE
C       NOT PLIB GET
          END IF
C*************************************************************
C       SPECIFIC CHECKS FOR PLIB DEL
C
C       PLIB DEL CAN TAKE NUMERIC INPUT FROM NW1 AND NW2
C       NW1 CAN BE 1 TO 999
C       NW2 CAN BE 2 TO 999
C       NW1 MUST BE LESS THAN NW2 IF NW2 NOT DEFAULT
C
          IF(WQ.EQ.'DEL') THEN
              IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1              '"PLIB DEL" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       INPUT OK
              END IF
              IF(W1.LT.1.0.AND.DF1.NE.1.OR.
     1        W1.GT.999.0.AND.DF1.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 1 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.2.0.AND.DF2.NE.1.OR.
     1        W2.GT.999.0.AND.DF2.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 2 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1) THEN
C
                  IF(W1.GE.W2) THEN
                      OUTLYNE=
     1                  'NUMERIC WORD 1 MUST BE LESS THAN NUMERIC WORD 2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              ELSE
C       NW2 IS BLANK
                  SING=1
              END IF
C       IS NW1 AND NW2 DEFAULT
              IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
                  OUTLYNE='"PLIB DEL" REQUIRES EXPLICIT NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
C       NOT PLIB DEL
          END IF
C***********************************************************
C       IF YOU GOT HERE, INPUT WAS OK
C       PROCEED WITH PROCESSING
C
          IF(WQ.EQ.'PUT') THEN
C       FIRST PLIB PUT
C       WHEN PLIB PUT IS USED, AN ENTRY IS MADE INTO AN AVALIABLE
C       SLOT IN PLIB.DAT (THE PLOT LIBRARY DIRECTORY)
C       DEPENDING ON WHICH SLOT WAS USED, A FILE
C       PLIB001.DAT TO PLIB999.DAT IS USED TO THEN STORE THE
C       PLOT DATA IN BINARY FORMAT. PLIB.DAT IS UNIT 24
C
C       IF ALL=1 THEN WE MUST SEARCH FOR AN AVALIABLE SLOT
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=trim(LIBPLO)//'PLIB.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  OUTLYNE='"PLOT LIBRARY DOES NOT YET EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='TO INITIALIZE IT, USE "IPF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       PROCEED
              END IF
C       ***************************************************************
              OPEN(UNIT=24,ACCESS='DIRECT',FILE=trim(LIBPLO)//'PLIB.DAT',
     1        FORM='UNFORMATTED',RECL=(90*NRECL),STATUS='UNKNOWN')
              IF(ALL.EQ.1) THEN
                  DO 10 I=1,999
                      READ(UNIT=24,REC=I) II,DATA,DUMTOT
                      IF(II.EQ.0) THEN
C       FOUND AN EMPTY SLOT
                          II=I
                          GO TO 11
                      ELSE
C       CONTINUE SEARCH
                      END IF
 10               CONTINUE
C       IF YOU GOT HERE, LIBRARY WAS FULL
                  OUTLYNE='PLOT LIBRARY FULL, PLOT NOT FILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
 11               CONTINUE
C       PROCEED WITH STORAGE
              ELSE
C       ALL NOT 1 SET II=INT(W1)
                  II=INT(W1)
                  READ(UNIT=24,REC=II) J,DATA,DUMTOT
                  IF(J.NE.0) THEN
C       SLOT OCCUPIED
                      WRITE(OUTLYNE,100) J
 100                  FORMAT('LIBRARY FILE NO. ',I3,' OCCUPIED')
                      CALL SHOWIT(1)
                      OUTLYNE='PLOT NOT FILED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C       PROCEED, SPOT EMPTY
                  END IF
              END IF
C       FILE THE NUMBER AND NAME IN THE LIBRARY
              CALL MYDATE(DDATE)
              CALL MYTIME(TTIME)
              TTTIM=TTIME
              DDDAT=DDATE
              IF(PPLI(1:30).NE.'                              ') THEN
                  LLIP=DDATE//' '//TTIME//' '//PPLI(1:59)
              ELSE
                  LLIP=DDATE//' '//TTIME//' '//LI(1:59)
              END IF
              WRITE(UNIT=24,REC=II) II,LLIP,NEUTTOTAL+1
              CLOSE(24 )
C
C     OPEN, CLOSE AND REMOVE FILE 28
              OPEN(UNIT=28
     1        ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2        ,RECL=(NRECL*42),STATUS='UNKNOWN')
              CALL CLOSE_FILE(28,0)
C     OPEN A NEW ONE
              OPEN(UNIT=28
     1        ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2        ,RECL=(NRECL*42),STATUS='UNKNOWN')
              IF(NEUTFILE) THEN
                  DO I=1,NEUTTOTAL+1
                      WRITE(UNIT=28,REC=I) NEUTARRAY(I)
                  END DO
              END IF
C     CLOSE AND SAVE FILE 28
              CALL CLOSE_FILE(28,1)
C       NOW DETERMINE WHICH OF THE 999 PLOT LIBRARY FILES
C       TO USE
C
              N=II
              CALL CCOONN(N,AN)
              IF(N.GT.0.AND.N.LE.9) FN='PLT00'//AN(3:3)//'.DAT'
              IF(N.GT.9.AND.N.LE.99) FN='PLT0'//AN(2:3)//'.DAT'
              IF(N.GT.99.AND.N.LE.999) FN='PLT'//AN(1:3)//'.DAT'
              CLOSE(28 )
              PFILENAME=LIBPLO//FN
              PFCOUNTER=NEUTTOTAL
              CALL os_copy('NEUTRAL.DAT',PFILENAME)
C
              WRITE(OUTLYNE,470) II
 470          FORMAT('PLOT STORED IN LIBRARY FILE NO. ',I3)
              CALL SHOWIT(1)
              RETURN
          END IF
C
C       NOW PLIB GET
C
C
          IF(WQ.EQ.'GET') THEN
              SAVE_KDP(19)=SAVEINPT(19)
              INPUT='PLOT NEW'
              CALL PROCES
              REST_KDP(19)=RESTINPT(19)
C
C       WHEN PLIB GET IS USED, A CHECK TO SEE IF THE SPECIFIC
C       PLOT LIBRARY ENTRY IN VACANT IS MADE. IF NOT VACANT,
C       PLOT DATA IS DRAWN.
C       IF IT IS VACANT, PRINT MESAGE AND STOP
C
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=trim(LIBPLO)//'PLIB.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  OUTLYNE='"PLOT LIBRARY DOES NOT YET EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='TO INITIALIZE IT, USE "IPF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       PROCEED
              END IF
C       ***************************************************************
              OPEN(UNIT=24,ACCESS='DIRECT',FILE=trim(LIBPLO)//'PLIB.DAT',
     1        FORM='UNFORMATTED',RECL=(90*NRECL),STATUS='UNKNOWN')
              READ(UNIT=24,REC=INT(W1)) II,DATA,DUMTOT
              IF(II.EQ.0) THEN
C       FOUND AN EMPTY SLOT
                  WRITE(OUTLYNE,480) INT(W1)
 480              FORMAT('PLOT LIBRARY FILE NO. ',I3,' IS EMPTY')
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       NOT EMPTY, READ DATA
                  II=INT(W1)
              END IF
C
C       NOW DETERMINE WHICH OF THE 999 LENS LIBRARY FILES
C       TO USE
              N=II
              CALL CCOONN(N,AN)
              IF(N.GT.0.AND.N.LE.9) FN='PLT00'//AN(3:3)//'.DAT'
              IF(N.GT.9.AND.N.LE.99) FN='PLT0'//AN(2:3)//'.DAT'
              IF(N.GT.99.AND.N.LE.999) FN='PLT'//AN(1:3)//'.DAT'
C**********************************************************
C       NOW INPUT PLOT FROM LIBXXX.DAT
C
C       READ IN THE PLI FROM THE DIRECTORY FILE
              READ(UNIT=24,REC=N) II,PPLI,NEUTTOTAL
              PPLI(1:60)=PPLI(20:80)
              PLEXIS=.TRUE.
              GRASET=.TRUE.
C
C       GET THE PLOT FROM THE PLOT FILE
C
              CLOSE(28 )
              PFILENAME=LIBPLO//FN
              PFCOUNTER=NEUTTOTAL
              CALL MY_DEL_NEUT
              CALL os_copy(PFILENAME,'NEUTRAL.DAT')
 5            CONTINUE
              OPEN28=.FALSE.
              EXIS28=.FALSE.
              INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',EXIST=EXIS28)
              INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',OPENED=OPEN28)
              IF(EXIS28.AND..NOT.OPEN28) THEN
C     PROCEED
              ELSE
                  GO TO 5
              END IF
              OPEN(UNIT=28
     1        ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2        ,RECL=(NRECL*42),STATUS='UNKNOWN')
              READ(UNIT=28,REC=1) NEUTLINE
              READ(NEUTLINE,1003) NEUTTOTAL
              PFCOUNTER=NEUTTOTAL
C     NOW LOAD UP THE NEUTARRAY
              DEALLOCATE(NEUTARRAY,STAT=ALLOERR)
              MAXNEUTRAL=50000
              ALLOCATE(NEUTARRAY(1:MAXNEUTRAL),STAT=ALLOERR)
              WRITE(NEUTLINE,1003) NEUTTOTAL
 1003         FORMAT(I9,32X)
              NEUTARRAY(1)=NEUTLINE
              J=2
 98           CONTINUE
              READ(UNIT=28,REC=J) NEUTLINE
              READ(NEUTLINE,1000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8
              IF(J.GE.MAXNEUTRAL/2)
     1        CALL RESIZE_NEUT
              NEUTARRAY(J)=NEUTLINE
 1000         FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
              J=J+1
              IF(STRINGER.EQ.'D') THEN
                  READ(UNIT=28,REC=J) NEUTLINE
                  READ(NEUTLINE,1001) C1A
                  IF(J.GE.MAXNEUTRAL/2)
     1            CALL RESIZE_NEUT
                  WRITE(NEUTARRAY(J),1001) C1A
 1001             FORMAT(A20,22X)
                  J=J+1
                  READ(UNIT=28,REC=J) NEUTLINE
                  READ(NEUTLINE,1001) C1B
                  IF(J.GE.MAXNEUTRAL/2)
     1            CALL RESIZE_NEUT
                  WRITE(NEUTARRAY(J),1001) C1B
                  J=J+1
                  READ(UNIT=28,REC=J) NEUTLINE
                  READ(NEUTLINE,1001) C1C
                  IF(J.GE.MAXNEUTRAL/2)
     1            CALL RESIZE_NEUT
                  WRITE(NEUTARRAY(J),1001) C1C
                  J=J+1
                  READ(UNIT=28,REC=J) NEUTLINE
                  READ(NEUTLINE,1001) C1D
                  IF(J.GE.MAXNEUTRAL/2)
     1            CALL RESIZE_NEUT
                  WRITE(NEUTARRAY(J),1001) C1D
                  J=J+1
              END IF
              IF(STRINGER.EQ.'F') THEN
                  READ(UNIT=28,REC=J) NEUTLINE
                  READ(NEUTLINE,1002) JJ1,JJ2
                  IF(J.GE.MAXNEUTRAL/2)
     1            CALL RESIZE_NEUT
                  WRITE(NEUTARRAY(J),1002) JJ1,JJ2
 1002             FORMAT(E15.7,E15.7,11X)
                  J=J+1
              END IF
              IF(STRINGER.EQ.'K') THEN
 2011             FORMAT(A1,E15.7,E15.7,10X)
                  DO II=1,I1
                      DO JJ=1,I1
                          READ(UNIT=28,REC=J) NEUTLINE
                          READ(NEUTLINE,2011) STRINGER,JJ1,JJ2
                          IF(J.GE.MAXNEUTRAL/2)
     1                    CALL RESIZE_NEUT
                          WRITE(NEUTARRAY(J),2011) STRINGER,JJ1,JJ2
                          J=J+1
                      END DO
                  END DO
              END IF
              IF(STRINGER.EQ.'M') THEN
                  DO II=1,I1
                      DO JJ=1,I1
                          READ(UNIT=28,REC=J) NEUTLINE
                          READ(NEUTLINE,2011) STRINGER,JJ1,JJ2
                          IF(J.GE.MAXNEUTRAL/2)
     1                    CALL RESIZE_NEUT
                          WRITE(NEUTARRAY(J),2011) STRINGER,JJ1,JJ2
                          J=J+1
                      END DO
                  END DO
              END IF
              IF(J.LE.NEUTTOTAL+1) GO TO 98
C
C     CURRENT PLOT HAS BEEN RETRIEVED AND NEUTARRAY LOADED
C
              WRITE(OUTLYNE,490) INT(W1)
 490          FORMAT('PLOT LIBRARY FILE NO. ',I3,' RETRIEVED')
              CALL SHOWIT(1)
          ELSE
C       NOT LIB DRAW
          END IF
C
C       NOW LIB P
          IF(WQ.EQ.'P') THEN
C
C       WHEN LIB P IS USED, PRINT OUT A PORTION
C       OF THE LENS LIBRARY DIRECTORY LIB.DAT
C
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=trim(LIBPLO)//'PLIB.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  OUTLYNE='"PLOT LIBRARY DOES NOT YET EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='TO INITIALIZE IT, USE "IPF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       PROCEED
              END IF
C       ***************************************************************
              OPEN(UNIT=24,ACCESS='DIRECT',FILE=trim(LIBPLO)//'PLIB.DAT',
     1        FORM='UNFORMATTED',RECL=(90*NRECL),STATUS='UNKNOWN')
              IF(ALL.EQ.1) THEN
                  LCNT=0
 123              FORMAT(1X)
                  WRITE(OUTLYNE,123)
                  CALL SHOWIT(1)
                  DO 817 I=1,999
                      READ(UNIT=24,REC=I) II,DATA,DUMTOT
                      IF(II.EQ.0) THEN
C       FOUND AN EMPTY SLOT
C       NO OUTPUT
                          GO TO 817
                      ELSE
C       NOT EMPTY, WRITE OUTPUT
                          LCNT=LCNT+1
                          WRITE(OUTLYNE,3994)II,DATA
 3994                     FORMAT(I3,1X,A75)
                          CALL SHOWIT(1)
                      END IF
 817              CONTINUE
                  IF(LCNT.EQ.0) THEN
                      WRITE(OUTLYNE,3995)
                      CALL SHOWIT(1)
 3995                 FORMAT('PLOT LIBRARY EMPTY')
                      RETURN
                  ELSE
C       NOT ALL EMPTY
                  END IF
                  RETURN
              ELSE
C       ALL NOT 1
              END IF
              IF(SING.EQ.1) THEN
                  READ(UNIT=24,REC=INT(W1))II,DATA,DUMTOT
                  IF(II.NE.0) THEN
                      WRITE(OUTLYNE,3994)II,DATA
                      CALL SHOWIT(1)
                      RETURN
                  ELSE
                      WRITE(OUTLYNE,9934) INT(W1)
                      CALL SHOWIT(1)
 9934                 FORMAT('PLOT LIBRARY ENTRY ',I3,' IS CURRENTLY EMPTY')
                  END IF
                  RETURN
              ELSE
C       SING NOT SET
              END IF
C
C       PRINT SEVERAL ENTRIES
              DO 819 I=INT(W1),INT(W2)
                  READ(UNIT=24,REC=I) II,DATA,DUMTOT
                  IF(II.EQ.0) THEN
C       FOUND AN EMPTY SLOT
C       NO OUTPUT
                      GO TO 819
                  ELSE
C       NOT EMPTY, WRITE OUTPUT
                      WRITE(OUTLYNE,3994)II,DATA
                      CALL SHOWIT(1)
                  END IF
 819          CONTINUE
              RETURN
C
          ELSE
C       NOT LIB P
          END IF
C***********************************************************
C       NOW LIB DEL
          IF(WQ.EQ.'DEL') THEN
C
C       WHEN LIB DEL IS USED,DELETE SPECIFIC ENTRIES IN THE
C       DIRECTORY AND DELETE SPECIFIC FILES
C
              IF(SING.EQ.1) THEN
C       ***************************************************************
                  EXISJK=.FALSE.
                  INQUIRE(FILE=trim(LIBPLO)//'PLIB.DAT',EXIST=EXISJK)
                  IF(.NOT.EXISJK) THEN
                      OUTLYNE='"PLOT LIBRARY DOES NOT YET EXIST'
                      CALL SHOWIT(1)
                      OUTLYNE='TO INITIALIZE IT, USE "IPF" AND "PROCEED"'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C       PROCEED
                  END IF
C       ***************************************************************
                  OPEN(UNIT=24,ACCESS='DIRECT',FILE=trim(LIBPLO)//'PLIB.DAT',
     1            FORM='UNFORMATTED',RECL=(90*NRECL),STATUS='UNKNOWN')
                  READ(UNIT=24,REC=INT(W1))II,DATA,DUMTOT
                  IF(II.EQ.0) THEN
                      WRITE(OUTLYNE,510) II
 510                  FORMAT('PLOT LIBRARY FILE NO. ',I3,' ALREADY EMPTY')
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C       PROCEED WITH DELETION
                      II=0
                      DATA=AA//AA//AA//AA
                      DUMTOT=0
                      WRITE(UNIT=24,REC=INT(W1))II,DATA,DUMTOT
                      CLOSE(24 )
                      II=INT(W1)
C       DUMP THE LIBRARY FILE
                      N=II
                      CALL CCOONN(N,AN)
                      IF(N.GT.0.AND.N.LE.9) FN='PLT00'//AN(3:3)//'.DAT'
                      IF(N.GT.9.AND.N.LE.99) FN='PLT0'//AN(2:3)//'.DAT'
                      IF(N.GT.99.AND.N.LE.999) FN='PLT'//AN(1:3)//'.DAT'
                      EXISJK=.FALSE.
                      INQUIRE(FILE=trim(LIBPLO)//FN,EXIST=EXISJK)
                      EXISJK=.FALSE.
                      INQUIRE(FILE=trim(LIBPLO)//FN,EXIST=EXISJK)
                      IF(EXISJK) THEN
                          OPEN(UNIT=77
     1                    ,FILE=trim(LIBPLO)//FN,FORM='UNFORMATTED',ACCESS='DIRECT'
     2                    ,RECL=(NRECL*42),STATUS='UNKNOWN')
                          CLOSE(77,STATUS='DELETE')
                      ELSE
                      END IF
                      WRITE(OUTLYNE,200)INT(W1)
                      CALL SHOWIT(1)
 200                  FORMAT('PLOT LIBRARY FILE NO. ',I3,' DELETED')
C
                  END IF
                  RETURN
              ELSE
C       SING NOT SET
              END IF
C
C       DELETE SEVERAL LIBRARY FILES
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=trim(LIBPLO)//'PLIB.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  OUTLYNE='"PLOT LIBRARY DOES NOT YET EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='TO INITIALIZE IT, USE "IPF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       PROCEED
              END IF
C       ***************************************************************
              DO 8191 I=INT(W1),INT(W2)
                  OPEN(UNIT=24,ACCESS='DIRECT',FILE=trim(LIBPLO)//'PLIB.DAT',
     1            FORM='UNFORMATTED',RECL=(90*NRECL),STATUS='UNKNOWN')
                  READ(UNIT=24,REC=I) II,DATA,DUMTOT
                  IF(II.EQ.0) THEN
C       FOUND AN EMPTY SLOT
C       NO DELETION
                      WRITE(OUTLYNE,300) I
                      CALL SHOWIT(1)
 300                  FORMAT('PLOT LIBRARY FILE NO. ',I3,' ALREADY EMPTY')
                      CLOSE(24 )
                      CALL MACFAL
                      GO TO 8191
                  ELSE
C       NOT EMPTY, DELETE ENTRY
                      II=0
                      DATA=AA//AA//AA//AA
                      DUMTOT=0
                      WRITE(UNIT=24,REC=I)II,DATA,DUMTOT
                      CLOSE(24 )
                      II=I
C       DUMP THE LIBRARY FILE
                      N=II
                      CALL CCOONN(N,AN)
                      IF(N.GT.0.AND.N.LE.9) FN='PLT00'//AN(3:3)//'.DAT'
                      IF(N.GT.9.AND.N.LE.99) FN='PLT0'//AN(2:3)//'.DAT'
                      IF(N.GT.99.AND.N.LE.999) FN='PLT'//AN(1:3)//'.DAT'
                      EXISJK=.FALSE.
                      INQUIRE(FILE=trim(LIBPLO)//FN,EXIST=EXISJK)
                      EXISJK=.FALSE.
                      INQUIRE(FILE=trim(LIBPLO)//FN,EXIST=EXISJK)
                      IF(EXISJK) THEN
                          OPEN(UNIT=76
     1                    ,FILE=trim(LIBPLO)//FN,FORM='UNFORMATTED',ACCESS='DIRECT'
     2                    ,RECL=(NRECL*42),STATUS='UNKNOWN')
                          CLOSE(76,STATUS='DELETE')
                      ELSE
                      END IF
                      WRITE(OUTLYNE,400)I
                      CALL SHOWIT(1)
 400                  FORMAT('PLOT LIBRARY FILE NO. ',I3,' DELETED')
                  END IF
 8191         CONTINUE
              RETURN

          END IF
       END SUBROUTINE PLIBRY


      SUBROUTINE MY_DATE_AND_TIME(FDATE,FTIME,FZONE,FVALUE)
          IMPLICIT NONE
          CHARACTER FDATE*8,FTIME*10,FZONE*5
          INTEGER FVALUE(8)
          CALL DATE_AND_TIME(FDATE,FTIME,FZONE,FVALUE)
          RETURN
      END SUBROUTINE MY_DATE_AND_TIME


      SUBROUTINE MY_TIMER(I)
          IMPLICIT NONE
C
          CHARACTER FDATE*8,FTIME*10,FZONE*5
          INTEGER FVALUE(8),BASE

          COMMON/TIMMER/BASE
          INTEGER I

          call MY_DATE_AND_TIME(FDATE,FTIME,FZONE,FVALUE)

          IF(I.EQ.0) THEN
C       RESET BASE TIME
              BASE=FVALUE(5)*3600+FVALUE(6)*60+FVALUE(7)
              I=BASE
          ELSE
C       RETURN CURRENT VALUE OF ITEM15 IN MILLISECONDS
              I=FVALUE(5)*3600+FVALUE(6)*60+FVALUE(7)

          END IF

          RETURN
      END SUBROUTINE MY_TIMER


      SUBROUTINE SHOWIT(II)
C     THIS REPLACES CALLES TO "WRITE" EVERYWHERE IN THE PROGRAM
C     II=0 MEANS THE OUTPUT IS TO UNIT=OUT (79 COL OUTPUT)
C     II=2 MEANS THE OUTPUT IS TO UNIT=OUT (139 COL OUTPUT)
C     II=1 MEANS THE OUTPUT IS A MESSAGE TO 6 UNLESS OUT IS NULL
C     II=4 MEANS USE $ EDITING
C     II=5 MEANS OUTPUT TO UNIT 6 ONLY
C     II=10 MEANS II=0 BUT STRIP ALL BLANKS INFRONT OF COMMAS AND OTHER STUFF
C     OPERATIONS, USED ONLY IN LENO
C     II=11 MEANS II=10 BUT NO STRIP OPERATIONS, USED ONLY IN LENO
C
          IMPLICIT NONE

          CHARACTER JK_J1*20,BFRM*3,JK_BLLIN*140,OLYNE*140,A1*1,A2*2,A3*3
          CHARACTER FFORM*10
          CHARACTER*153 STRIPOUT
          INTEGER II,I,OUTY,LL,IPASS1
          LOGICAL OPENOUT

          COMMON/SHOWME/BFRM

          INCLUDE 'datmai.inc'

          I=II
C       ADDED 1/20/2004
          IF(OUTLYNE(1:1).EQ.' ') OUTLYNE(1:139)=OUTLYNE(2:139)//' '
          OLYNE=TRIM(OUTLYNE)
          LL=LEN_TRIM(OLYNE)

          if (LL.eq.0) return

          IF(LL.LT.10) THEN
              WRITE(A1,FMT='(I1)') LL
              FFORM=TRIM('A'//A1)
          END IF
          IF(LL.GT.9.AND.LL.LT.100) THEN
              WRITE(A2,FMT='(I2)') LL
              FFORM=TRIM('A'//A2)
          END IF
          IF(LL.GT.99.AND.LL.LT.1000) THEN
              WRITE(A3,'(I3)') LL
              FFORM=TRIM('A'//A3)
          END IF
          FFORM='('//TRIM(FFORM)//')'

C     NO OUTPUT CAN EVER BE DONE TO 0 OR 1 WHEN OUTPUT NULL IS IN EFFECT
          IF(I.EQ.1.AND.OUT.EQ.98.OR.I.EQ.0.AND.OUT.EQ.98) RETURN

          IF(I.EQ.3) I=1

          IF(F15.EQ.1) THEN
C     CHANGING CONFIGS, NO SCREEN OUTPUT
              IF(I.EQ.0.AND.OUT.EQ.6.OR.
     1           I.EQ.2.AND.OUT.EQ.6.OR.
     2           I.EQ.0.AND.OUT.EQ.69.OR.
     3           I.EQ.2.AND.OUT.EQ.69.OR.
     4           I.EQ.1.OR.
     5           I.EQ.4.OR.
     6           I.EQ.5.OR.I.EQ.10.AND.OUT.EQ.6
     7           .OR.I.EQ.10.AND.OUT.EQ.6) RETURN
          END IF

          IF(I.EQ.20) THEN
C       OUTPUT INTENDED FOR UNIT=OUT (256 COL), UNIT 97
C       USED ONLY IN COMMA DELIMITED TABLE OUTPUT
              WRITE(97,2000) OUTLYNE(1:256)
 2000         FORMAT(A256)
          END IF

          IF(I.EQ.10) THEN
              I=0
              STRIPOUT = OUTLYNE(1:LEN(STRIPOUT))
              IPASS1=139
              CALL NO_ZEROS(STRIPOUT,IPASS1)
              OUTLYNE=STRIPOUT   ! ??? wtf ...
              STRIPOUT = OUTLYNE(1:LEN(STRIPOUT))  
              IPASS1=139
              CALL ONE_BLANK(STRIPOUT,IPASS1)
              OUTLYNE=STRIPOUT
          END IF

          IF(I.EQ.11) THEN
              I=0
          END IF
C
C     IF SHOWIT GETS CALLED WHILE READING IN A LENS, DO NOT DO
C     ANYTHING, JUST RETURN
          IF(IN.EQ.99.AND.OUT.EQ.99) RETURN

          JK_J1='                    '
          JK_BLLIN=JK_J1//JK_J1//JK_J1//JK_J1//JK_J1//JK_J1//JK_J1

          IF(OUT.EQ.69) WRITE(97,FMT=FFORM) OLYNE

          IF(I.EQ.3) OUTY=6
          IF(I.NE.3) OUTY=OUT
          OPENOUT=.FALSE.
          IF(OUTY.NE.6) INQUIRE(UNIT=OUTY,OPENED=OPENOUT)
          IF(.NOT.OPENOUT) OUTY=6
C     RESETTING I TO 0 OR 2
          IF(I.NE.1.AND.I.NE.4.AND.I.NE.5) THEN
C     NOT A SCREEN MESSAGE
              IF(OUTY.EQ.6.OR.OUTY.EQ.7) THEN
C     79 COL OUTPUT
                  I=0
              ELSE
C     139 COL OUTPUT
                  I=2
              END IF
          END IF

          IF(I.EQ.0) THEN
C       OUTPUT INTENDED FOR UNIT=OUT (79 COL)
              IF(OUT.EQ.6.OR.OUT.EQ.69.OR.OUT.EQ.7) THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.69) THEN
                      IPASS1=79
                      WRITE(OUT,FMT=FFORM) OUTLYNE
                  ELSE
C     OUT IS NOT 6
                      WRITE(OUTY,FMT=FFORM) OLYNE
                      IF(OUTY.EQ.6) THEN
                          IPASS1=79
                          WRITE(OUT,*) OUTLYNE
                      END IF
                  END IF
              ELSE
                  IF(I.NE.5) THEN
                      WRITE(OUTY,FMT=FFORM) OLYNE
                      IF(OUTY.EQ.6) THEN
                          IPASS1=79
                      END IF
                  END IF
              END IF
              OUTLYNE=JK_BLLIN
              RETURN
          END IF

          IF(I.EQ.2) THEN
C       OUTPUT INTENDED FOR UNIT=OUT (139 COL)
              IF(OUT.EQ.6.OR.OUT.EQ.69.OR.OUT.EQ.7) THEN
                  LL=LEN_TRIM(OUTLYNE)
                  IF(OUT.EQ.6.OR.OUT.EQ.69) THEN
                      WRITE(OUT,*) OUTLYNE
                  ELSE
                      LL=LEN_TRIM(OUTLYNE)
                      WRITE(OUTY,FFORM) OLYNE
                      IF(OUTY.EQ.6) THEN
                          IPASS1=79
                          WRITE(OUT,*) OUTLYNE
                      END IF
                  END IF
              ELSE
                  IF(I.NE.5) THEN
                      LL=LEN_TRIM(OUTLYNE)
                      WRITE(OUTY,FMT=FFORM) OLYNE
                      IF(OUTY.EQ.6) THEN
                          IPASS1=79
                          WRITE(OUT,*) OUTLYNE
                      END IF
                      LL=LEN_TRIM(OUTLYNE)
                  END IF
              END IF
              OUTLYNE=JK_BLLIN
          END IF

          IF(I.EQ.5) THEN
C       A MESSAGE INTENDED FOR THE SCREEN ONLY (UNIT 6 EXCLUSIVELY)
              IPASS1=79
              WRITE(OUT,*) TRIM(OUTLYNE)
              OUTLYNE=JK_BLLIN
              RETURN
          END IF

          IF(I.EQ.1) THEN
C       A MESSAGE INTENDED FOR THE SCREEN ONLY
              IPASS1=79
              WRITE(OUT,*) TRIM(OUTLYNE)
              OUTLYNE=JK_BLLIN
              RETURN
          END IF

          IF(I.EQ.4) THEN
C       A MESSAGE INTENDED FOR THE SCREEN WITHOUT A CARRIAGE CONTROL/LF
              WRITE(OUT,*) TRIM(OUTLYNE)
              OUTLYNE=JK_BLLIN
              RETURN
          END IF
          IF(I.NE.5) OUTLYNE=JK_BLLIN

      END SUBROUTINE SHOWIT


      SUBROUTINE check_bounds(I1, I2, I3, I4, I5, I6, I7, I8)
      !
      ! ensure that the plot variables I1 .. I8 are within bounds
      !   -9999 < Ik < 99999

         IMPLICIT NONE
         INTEGER, INTENT(INOUT) :: I1,I2,I3,I4,I5,I6,I7,I8

         IF (I1 > 99999) THEN
            I1 = 99999
         ELSE IF (I1 < -9999) THEN
            I1 = -9999
         END IF

         IF (I2 > 99999) THEN
            I2 = 99999
         ELSE IF (I2 < -9999) THEN
            I2 = -9999
         END IF

         IF (I3 > 99999) THEN
            I3 = 99999
         ELSE IF (I3 < -9999) THEN
            I3 = -9999
         END IF

         IF (I4 > 99999) THEN
            I4 = 99999
         ELSE IF (I4 < -9999) THEN
            I4 = -9999
         END IF

         IF (I5 > 99999) THEN
            I5 = 99999
         ELSE IF (I5 < -9999) THEN
            I5 = -9999
         END IF

         IF (I6 > 99999) THEN
            I6 = 99999
         ELSE IF (I6 < -9999) THEN
            I6 = -9999
         END IF

         IF (I7 > 99999) THEN
            I7 = 99999
         ELSE IF (I7 < -9999) THEN
            I7 = -9999
         END IF

         IF (I8 > 99999) THEN
            I8 = 99999
         ELSE IF (I8 < -9999) THEN
            I8 = -9999
         END IF
          
      END SUBROUTINE check_bounds
