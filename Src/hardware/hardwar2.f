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

      SUBROUTINE REPLAYFILE

          USE GLOBALS
          IMPLICIT NONE
          LOGICAL GROPEN(1:10)
          COMMON/OPENGR/GROPEN
          INTEGER I,J
          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'resource.inc'

C       GET FIRST AVALABLE WINDOW NUMBER OR IF ALL USED,
C       USE WINDOW # 10
          I=0
          J=0
          DO I=1,10
              IF(GROPEN(I)) THEN
                  J=I
                  EXIT
              END IF
          END DO
          IF(J.EQ.0) J=10
C
          J=1
          RETURN
      END

      SUBROUTINE RUN_WPLOT(CEELINE)
C     THIS IS THE DRIVER ROUTINE FOR SENDING GRAPHICS TO
C     HARDCOPY DEVICES
          IMPLICIT NONE
          CHARACTER CEELINE*(*)
          INCLUDE 'datmai.inc'
          CALL WPLOT(CEELINE)
          RETURN
      END

      SUBROUTINE WPLOT(CEELINE)
C     PLOTTING ROUTINE FOR HARDCOPY
          IMPLICIT NONE
          LOGICAL OPEN28,EXIS28
          CHARACTER CEELINE*(*),FILNAME*12
          INTEGER I
          INCLUDE 'datmai.inc'
          DO I=1,10
              IF(CEELINE(1:1).EQ.' ') CEELINE(1:80)=CEELINE(2:80)//' '
          END DO
          FILNAME(1:12)=CEELINE(3:14)
          EXIS28=.FALSE.
          OPEN28=.FALSE.
          INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',EXIST=EXIS28)
          INQUIRE(FILE=trim(HOME)//'NEUTRAL.DAT',OPENED=OPEN28)
          IF(OPEN28) CALL CLOSE_FILE(28,1)
          IF(EXIS28) THEN
!       IF(CEELINE(1:2).EQ.'31') CALL GRAOUTPRT(31,FILNAME)
              IF(CEELINE(1:2).EQ.'32') CALL GRAOUTPRT(32,FILNAME)
              IF(CEELINE(1:2).EQ.'33') CALL GRAOUTPRT(33,FILNAME)
              IF(CEELINE(1:2).EQ.'34') CALL GRAOUTPRT(34,FILNAME)
              CALL CLOSE_FILE(28,1)
              RETURN
          ELSE
          END IF
          RETURN
      END


      SUBROUTINE GRAOUTPRT(JK_TAG,GRFILN)
C     CALL BY HARDCOPY GRAPHICS ROUTINE
          USE GLOBALS

          IMPLICIT NONE
!      COMMON/PENPEN2/PENPOSX,PENPOSY
          INCLUDE 'datmai.inc'
          character GRFILN*12
          integer JK_TAG
          
          CHARACTER*80 BMPFILE
          COMMON /FILEBMP/ BMPFILE
C
C     "PLOT NEW" STRINGER = 'A'
C
          if(JK_TAG.eq.33) then
              call saveplot(BMPFILE,GRFILN,"jpg")
              return
          end if

          if(JK_TAG.eq.34) then
              call saveplot(BMPFILE,GRFILN,"pdf")
              return
          end if

          IF(JK_TAG.EQ.31.OR.JK_TAG.EQ.32) THEN
C     INITIALIZE THE EPS AND COLEPS DRIVER
              call saveplot(BMPFILE,GRFILN,"eps")
              RETURN
          end if
      END


      SUBROUTINE RUN_WDRAW
C     THIS IS THE DRIVER ROUTINE FOR SENDING GRAPHICS TO
C     A GRAPHIC WINDOW
!      USE WINTERACTER
          IMPLICIT NONE
          !     LOGICAL EXISD
          INCLUDE 'datmai.inc'
          CALL WDRAW
          RETURN
      END


      SUBROUTINE WDRAW
          USE GLOBALS

          IMPLICIT NONE
          LOGICAL FIRST
          INTEGER IDRAW1,ISKEY
          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'resource.inc'
          COMMON/DRAWI1/IDRAW1
          FIRST=.TRUE.
          ISKEY=-999
          READ(NEUTARRAY(1),1000) NEUTTOTAL
 1000     FORMAT(I9,32X)
          IF(NEUTTOTAL.EQ.0) GO TO 10
C     INITIALIZE SCREEN
C     IDRAW1 IS THE WINDOW HANDLE
C     DRW IS THE STRUCTURE WHICH PASSES CHILD WINDOW CHARACTERISTICS
C     TO THE WINDOW
          CALL DRAW(1,FIRST,ISKEY)
 10       CONTINUE
          RETURN
      END

      SUBROUTINE NEWDRAWSCREEN

          IMPLICIT NONE
          RETURN
      END

      SUBROUTINE REDRAWSCREEN(FIRST,ISKEY)

          LOGICAL FIRST
          INTEGER ISKEY
          INTEGER IDRAW1
          COMMON/DRAWI1/IDRAW1
C     RE-INITIALIZE SCREEN
          CALL DRAW(2,FIRST,ISKEY)
          RETURN
      END


      SUBROUTINE DRAW(ITYPER,FIRST,ISKEY)
          USE GLOBALS

          IMPLICIT NONE
          LOGICAL FIRST
          INTEGER CON_ARRAY,NX,NY,ZSTEP,ALLOERR
          REAL OPDPEAK,OPDPIT
          DIMENSION CON_ARRAY(:,:)
          ALLOCATABLE :: CON_ARRAY
          CHARACTER AB*80,STRINGER*1,C1A*20,C1B*20,C1C*20,C1D*20
          CHARACTER C1*320
          CHARACTER CC1*1
          CHARACTER CC2*2
          CHARACTER CC3*3
          CHARACTER CC4*4
          CHARACTER CC5*5
          CHARACTER CC6*6
          CHARACTER CC7*7
          CHARACTER CC8*8
          CHARACTER CC9*9
          CHARACTER CC10*10
          CHARACTER CC11*11
          CHARACTER CC12*12
          CHARACTER CC13*13
          CHARACTER CC14*14
          CHARACTER CC15*15
          CHARACTER CC16*16
          CHARACTER CC17*17
          CHARACTER CC18*18
          CHARACTER CC19*19
          CHARACTER CC20*20
          CHARACTER CC21*21
          CHARACTER CC22*22
          CHARACTER CC23*23
          CHARACTER CC24*24
          CHARACTER CC25*25
          CHARACTER CC26*26
          CHARACTER CC27*27
          CHARACTER CC28*28
          CHARACTER CC29*29
          CHARACTER CC30*30
          CHARACTER CC31*31
          CHARACTER CC32*32
          CHARACTER CC33*33
          CHARACTER CC34*34
          CHARACTER CC35*35
          CHARACTER CC36*36
          CHARACTER CC37*37
          CHARACTER CC38*38
          CHARACTER CC39*39
          CHARACTER CC40*40
          INTEGER I,I1,I2,I3,I4,I5,I6,I7,I8,JIMLEN
          INTEGER ISKEY,II1,II2,II3,II4,II5,II6,II7,II8
     1    ,COLPASS,COLTRUE,CHARSTYPE,ITYPER
          INTEGER J,II,JJ
          REAL CL1,CL2,CL3,CL4
          COMMON/OLDCLIP/CL1,CL2,CL3,CL4
          REAL IA,IB,RRR1,RRR2
          COMMON/TEXTCOM/CHARSTYPE
          COMMON/COLCOM/COLPASS,COLTRUE
          REAL JJ_X,JJ_Y
          COMMON/ASPECTER/JJ_X,JJ_Y
          INCLUDE 'datmai.inc'
          INCLUDE 'dathgr.inc'
C     INITIALIZE CHARACTER ASPECT RATIO
          JJ_X=1.0
          JJ_Y=1.0
          J=1

          READ(NEUTARRAY(1),1000) NEUTTOTAL
 1000     FORMAT(I9,32X)

 300      J=J+1
          READ(NEUTARRAY(J),2000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8
 2000     FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
          II1=(I1)
          II2=(I2)
          II3=(I3)
          II4=(I4)
          II5=(I5)
          II6=(I6)
          II7=(I7)
          II8=(I8)

C
C     "PLOT NEW" STRINGER = A
C
          IF(STRINGER.EQ.'A') THEN
C     INITIALIZE THE DRAW SCREEN
              CALL NEWDRAWSCREEN
C
C     DEFINE DARK GREY AS BLACK
C     THIS LETS COLOR 0 BE RE-DEFINED AS ANY BACKGROUND COLOR DESIRED
C     DEFINE COLOR 208 AS WHITE
C     THIS LETS COLOR 0 BE RE-DEFINED AS ANY BACKGROUND COLOR DESIRED
C     NOW READ ANOTHER COMMAND
              GO TO 300
          END IF
C
C     "PLOT COLTYP"
C
          IF(STRINGER.EQ.'E') THEN
C     SETTING THE FOREGROUND COLOR
              COLPASS=II1
              IF(COLPASS.EQ.0)  COLTRUE=208
              IF(COLPASS.EQ.1)  COLTRUE=48
              IF(COLPASS.EQ.2)  COLTRUE=176
              IF(COLPASS.EQ.3)  COLTRUE=16
              IF(COLPASS.EQ.4)  COLTRUE=112
              IF(COLPASS.EQ.5)  COLTRUE=80
              IF(COLPASS.EQ.6)  COLTRUE=144
              IF(COLPASS.EQ.7)  COLTRUE=240
              IF(COLPASS.EQ.8)  COLTRUE=224
              IF(COLPASS.EQ.9)  COLTRUE=64
              IF(COLPASS.EQ.10) COLTRUE=192
              IF(COLPASS.EQ.11) COLTRUE=32
              IF(COLPASS.EQ.12) COLTRUE=128
              IF(COLPASS.EQ.13) COLTRUE=96
              IF(COLPASS.EQ.14) COLTRUE=160
              IF(COLPASS.EQ.15) COLTRUE=240

              GO TO 300
          END IF
C
C
C     "PLOT SETCHARACTERASPECT" = F
C
          IF(STRINGER.EQ.'F') THEN
C     SETTING THE CHARACTER ASPECT
              J=J+1
 3000         FORMAT(E15.7,E15.7,11X)
              READ(NEUTARRAY(J),3000) RRR1,RRR2
              JJ_X=RRR1
              JJ_Y=RRR2
C     NOW READ ANOTHER COMMAND
              GO TO 300
          END IF
C
C     "PLOT SETPAL" = G
C
          IF(STRINGER.EQ.'G') THEN
C     SETTING THE BACKGROUND COLOR TO A NEW VALUE
C     FORCES A SCREEN CLEAR AND A PLOT NEW EFFECT
C     NOW READ ANOTHER COMMAND
              GO TO 300
          END IF
C
C     "PLOT MARKER" = C
C
          IF(STRINGER.EQ.'C') THEN
C     PLOTTING A SYMBOL
C     NOW READ ANOTHER COMMAND
              GO TO 300
          END IF
C
C     "PLOT SETFONT" = H
C
          IF(STRINGER.EQ.'H') THEN
C     CHANGING FONTS
              CHARSTYPE=II1
C     NOW READ ANOTHER COMMAND
              GO TO 300
          END IF
C
C     "PLOT JUSTIFYSTRING" = D
C
          IF(STRINGER.EQ.'D') THEN
C     PLOTTING A STRING
              J=J+1
 4000         FORMAT(A20,22X)
              READ(NEUTARRAY(J),4000) C1A
              J=J+1
              READ(NEUTARRAY(J),4000) C1B
              J=J+1
              READ(NEUTARRAY(J),4000) C1C
              J=J+1
              READ(NEUTARRAY(J),4000) C1D

              AB='                    '
              C1=AB//AB//AB//AB
              IF(II6.LE.20) THEN
                  C1(1:II6)=C1A(1:II6)
              END IF
              IF(II6.GT.20.AND.II6.LE.40) THEN
                  C1(1:II6)=C1A(1:20)//C1B(1:II6-20)
              END IF
              IF(II6.GT.40.AND.II6.LE.60) THEN
                  C1(1:II6)=C1A(1:20)//C1B(1:20)//C1C(1:II6-40)
              END IF
              IF(II6.GT.60.AND.II6.LE.80) THEN
                  C1(1:II6)=C1A(1:20)//C1B(1:20)//C1C(1:20)//C1D(1:II6-60)
              END IF
              IF(CHARSTYPE.EQ.1) CALL SETSTANDARD
              IF(CHARSTYPE.EQ.2) CALL SETSYMBOL
              IA=0.0
              IB=35.0
              IF(II5.EQ.2) THEN
                  DO I=1,80
                      IF(C1(1:1).EQ.' ') THEN
                          C1(1:80)=C1(2:80)//' '
                      ELSE
                          GO TO 88
                      END IF
                  END DO
 88               CONTINUE
                  JIMLEN=0
                  DO I=80,1,-1
                      IF(C1(I:I).NE.' ') THEN
                          JIMLEN=I
                          GO TO 99
                      END IF
                  END DO
 99               CONTINUE
                  IF(JIMLEN.EQ.1)   CC1=C1(1:1)
                  IF(JIMLEN.EQ.2)   CC2=C1(1:2)
                  IF(JIMLEN.EQ.3)   CC3=C1(1:3)
                  IF(JIMLEN.EQ.4)   CC4=C1(1:4)
                  IF(JIMLEN.EQ.5)   CC5=C1(1:5)
                  IF(JIMLEN.EQ.6)   CC6=C1(1:6)
                  IF(JIMLEN.EQ.7)   CC7=C1(1:7)
                  IF(JIMLEN.EQ.8)   CC8=C1(1:8)
                  IF(JIMLEN.EQ.9)   CC9=C1(1:9)
                  IF(JIMLEN.EQ.10) CC10=C1(1:10)
                  IF(JIMLEN.EQ.11) CC11=C1(1:11)
                  IF(JIMLEN.EQ.12) CC12=C1(1:12)
                  IF(JIMLEN.EQ.13) CC13=C1(1:13)
                  IF(JIMLEN.EQ.14) CC14=C1(1:14)
                  IF(JIMLEN.EQ.15) CC15=C1(1:15)
                  IF(JIMLEN.EQ.16) CC16=C1(1:16)
                  IF(JIMLEN.EQ.17) CC17=C1(1:17)
                  IF(JIMLEN.EQ.18) CC18=C1(1:18)
                  IF(JIMLEN.EQ.19) CC19=C1(1:19)
                  IF(JIMLEN.EQ.20) CC20=C1(1:20)
                  IF(JIMLEN.EQ.21) CC21=C1(1:21)
                  IF(JIMLEN.EQ.22) CC22=C1(1:22)
                  IF(JIMLEN.EQ.23) CC23=C1(1:23)
                  IF(JIMLEN.EQ.24) CC24=C1(1:24)
                  IF(JIMLEN.EQ.25) CC25=C1(1:25)
                  IF(JIMLEN.EQ.26) CC26=C1(1:26)
                  IF(JIMLEN.EQ.27) CC27=C1(1:27)
                  IF(JIMLEN.EQ.28) CC28=C1(1:28)
                  IF(JIMLEN.EQ.29) CC29=C1(1:29)
                  IF(JIMLEN.EQ.30) CC30=C1(1:30)
                  IF(JIMLEN.EQ.31) CC31=C1(1:31)
                  IF(JIMLEN.EQ.32) CC32=C1(1:32)
                  IF(JIMLEN.EQ.33) CC33=C1(1:33)
                  IF(JIMLEN.EQ.34) CC34=C1(1:34)
                  IF(JIMLEN.EQ.35) CC35=C1(1:35)
                  IF(JIMLEN.EQ.36) CC36=C1(1:36)
                  IF(JIMLEN.EQ.37) CC37=C1(1:37)
                  IF(JIMLEN.EQ.38) CC38=C1(1:38)
                  IF(JIMLEN.EQ.39) CC39=C1(1:39)
                  IF(JIMLEN.EQ.40) CC40=C1(1:40)
              END IF
              IF(II5.EQ.2) THEN
              END IF
C     NOW READ ANOTHER COMMAND
              GO TO 300
          END IF

C     "PLOT PLOTTO" = I
C
          IF(STRINGER.EQ.'I') THEN
              GO TO 300
          END IF
C
C     "PLOT PLOTTOC" = J
C
          IF(STRINGER.EQ.'J') THEN

              GO TO 300
          END IF
C
C     CAPFN_OPD CONTOUR PLOTTING
C
          IF(STRINGER.EQ.'K') THEN
              NX=I1
              NY=I1
              ZSTEP=II2
              ALLOCATE(CON_ARRAY(1:NX,1:NY),STAT=ALLOERR)
              J=J+1
              READ(NEUTARRAY(J),2011) STRINGER,OPDPEAK,OPDPIT
 2011         FORMAT(A1,E15.7,E15.7,10X)
              DO II=1,NX
                  DO JJ=1,NY
                      J=J+1
                      READ(NEUTARRAY(J),2000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8
                      CON_ARRAY(II,JJ)=I1
                  END DO
              END DO
              CALL DrawContour_OPD(NX,NY,ZSTEP,CON_ARRAY,OPDPEAK,OPDPIT)
              DEALLOCATE(CON_ARRAY,STAT=ALLOERR)
              GO TO 300
          END IF
          IF(STRINGER.EQ.'M') THEN
              NX=I1
              NY=I1
              ZSTEP=II2
              ALLOCATE(CON_ARRAY(1:NX,1:NY),STAT=ALLOERR)

              DO II=1,NX
                  DO JJ=1,NY
                      J=J+1
                      READ(NEUTARRAY(J),2000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8
                      CON_ARRAY(II,JJ)=I1
                  END DO
              END DO
              CALL DrawContour_APD(NX,NY,CON_ARRAY)
              DEALLOCATE(CON_ARRAY,STAT=ALLOERR)
              GO TO 300
          END IF
C
C     "PLOT END" = B
          IF(STRINGER.EQ.'B'.AND.J.LT.NEUTTOTAL+1) GO TO 300
C
 8        CONTINUE
          IF(ISKEY.EQ.259.OR.ISKEY.EQ.260) ISKEY=-999
C      IF(J.EQ.NEUTTOTAL+1) THEN
          IF(STRINGER.EQ.'B'.OR.J.GE.NEUTTOTAL+1) THEN
              IF(ITYPER.EQ.2) RETURN
 5            CALL MY_INFOINPUT(ISKEY)
              IF(ISKEY.EQ.259.AND..NOT.FIRST) GO TO 6
              IF(ISKEY.EQ.259.AND.FIRST) FIRST=.FALSE.
              IF(ISKEY.EQ.259.AND..NOT.FIRST) GO TO 6
              IF(ISKEY.EQ.260) THEN
                  RETURN
              END IF
              IF(ISKEY.NE.259.AND.ISKEY.NE.260) GO TO 5
          END IF
          GO TO 7
 6        CALL REDRAWSCREEN(FIRST,ISKEY)
          GO TO 8
 7        CONTINUE
          GO TO 300
          RETURN
      END


      SUBROUTINE DrawContour_OPD(NX,NY,NZSTEP,CON_ARRAY,OPDPEAK,OPDPIT)
c
c     Draw 2D OPD CONTOUR PLOT
c
          USE opsys
          USE GLOBALS
c
          IMPLICIT NONE

          include 'datmai.inc'

          INTEGER ICC,NX,NY,CON_ARRAY,ZSTEP,NZSTEP,ALLOERR
          REAL ZCONT,RSTEP,STEP_VAL,OPDPIT,OPDPEAK
          CHARACTER KEYSTR*80
          CHARACTER*8 ASTEP_VAL,B
          DIMENSION ZCONT(:),KEYSTR(:)
          ALLOCATABLE :: ZCONT,KEYSTR
          DIMENSION CON_ARRAY(1:NX,1:NY)
c
          REAL            , DIMENSION(NX,NY)       :: ZDATA
          INTEGER         , DIMENSION(2)           :: NVAL
          INTEGER                                  :: IX,IY,IC
          CHARACTER(LEN=256)                       :: plotcommand
          
c  generate some data
          ZSTEP=INT(5000.0/REAL(NZSTEP))
          IF(NZSTEP.LT.250) NZSTEP=250
          IF(ZSTEP.GT.20) ZSTEP=20
          ALLOCATE(ZCONT(ZSTEP),KEYSTR(ZSTEP),STAT=ALLOERR)
          DO IX = 1,NX
              DO IY = 1,NY
                  ZDATA(IX,IY) = REAL(CON_ARRAY(IX,IY))/REAL(NZSTEP)

                  write(119,*) IX,IY,ZDATA(IX,IY)

              END DO
          END DO
c
c
          DO IC = 1,ZSTEP
              ZCONT(IC) = REAL(IC)
          END DO
c
c
c  In this example, declared size of data array and
c  size of plot are the same.
c
          NVAL(1) = NX
          NVAL(2) = NY
c
c
c  Set up simple strings for the key and assign
c  varying line styles for each contour
c
          ICC=0
C     PLOT ROUTINE DOES THE LOWEST LEVEL FIRST. FIRST CONTOUR IS PIT,
C     LAST ONE IS PEAK WITH LEVEL SPACING OF (PEAK-PIT)/(ZSTEP-1)
C
          RSTEP=(OPDPEAK-OPDPIT)/(REAL(ZSTEP)-1.0)
C
          STEP_VAL=OPDPIT
          DO IC = 1,ZSTEP
              ICC=ICC+1
              IF(ICC.GT.16) ICC=ICC-17
              IF((ICC*16).GE.0.AND.(ICC*16).LE.15.OR.
     1        (ICC*16).GE.208.AND.(ICC*16).LE.223) ICC=ICC+1
C
C     CONVERT CURRENT STEP_VAL TO F8.3
C
              WRITE(B,180)STEP_VAL
              READ(B,200) ASTEP_VAL
180           FORMAT(F8.3)
200           FORMAT(A8)
              KEYSTR(IC) =TRIM(ASTEP_VAL)//' = '//CHAR(64+IC)
              STEP_VAL=STEP_VAL+RSTEP

              CALL contlabel(26.0,real(10.0+IC),KEYSTR(IC))
              CALL drawcmdsave2

          END DO
c
c  generate contour plot
c
          plotcommand = TRIM(BMPREADR)//" "//TRIM(HOME)
          CALL dir_path_append(plotcommand, "gnuplot", plotcommand)
          CALL dir_path_append(plotcommand, "drawcmd.gpl", plotcommand)

          CALL shell_command( plotcommand )

      END SUBROUTINE DrawContour_OPD


      SUBROUTINE DrawContour_APD(NX,NY,CON_ARRAY)
c
c     Draw 2D APD CONTOUR CONTOUR
c
          USE opsys
          USE GLOBALS
c
          IMPLICIT NONE

          include 'datmai.inc'

          INTEGER ICC,NX,NY,CON_ARRAY,ZSTEP,ALLOERR
          REAL ZCONT
          CHARACTER*7 KEYSTR*80
          DIMENSION ZCONT(:),KEYSTR(:)
          ALLOCATABLE :: ZCONT,KEYSTR
          DIMENSION CON_ARRAY(1:NX,1:NY)

          REAL            , DIMENSION(NX,NY)       :: ZDATA
          INTEGER         , DIMENSION(2)           :: NVAL
          INTEGER                                  :: IX,IY,IC
          CHARACTER(LEN=256)                       :: plotcommand

          ZSTEP=11

c  generate some data
          ALLOCATE(ZCONT(ZSTEP),KEYSTR(ZSTEP),STAT=ALLOERR)
          DO IX = 1,NX
              DO IY = 1,NY

                  ZDATA(IX,IY) = REAL(CON_ARRAY(IX,IY))/454.5454

                  write(119,*) IX,IY,ZDATA(IX,IY)/real(ZSTEP)

              END DO
          END DO
c
          DO IC = 1,ZSTEP
              ZCONT(IC) = REAL(IC)
          END DO
c
c
c  In this example, declared size of data array and
c  size of plot are the same.
c
          NVAL(1) = NX
          NVAL(2) = NY
c
c
c  Set up simple strings for the key and assign
c  varying line styles for each contour
c
          ICC=0
          DO IC = 1,ZSTEP
              ICC=ICC+1
              IF((ICC*16).GE.0.AND.(ICC*16).LE.15.OR.
     1        (ICC*16).GE.208.AND.(ICC*16).LE.223) ICC=ICC+1
c          IF((ICC*16).GE.0.AND.(ICC*16).LE.15) icc=icc+1
c     1(ICC*16).GE.208.AND.(ICC*16).LE.223) ICC=ICC+1
              WRITE(KEYSTR(IC),100) REAL(IC-1)/10.0
 100          FORMAT('I = ',F3.1)

              CALL contlabel(26.0,real(9.0+IC),KEYSTR(IC))
              CALL drawcmdsave2

          END DO
c
c  generate contour plot
c
          plotcommand = TRIM(BMPREADR)//" "//TRIM(HOME)
          CALL dir_path_append(plotcommand, "gnuplot", plotcommand)
          CALL dir_path_append(plotcommand, "drawcmd.gpl", plotcommand)

          CALL shell_command( plotcommand )

      END SUBROUTINE DrawContour_APD


      SUBROUTINE SETSYMBOL
!      USE WINTERACTER
          IMPLICIT NONE
!      CALL IGRCHARSET('fonts\symbol.chr')
          RETURN
      END

      SUBROUTINE SETSTANDARD
!      USE WINTERACTER
          IMPLICIT NONE
!      CALL IGRCHARSET('fonts\standard.chr')
          RETURN
      END

      SUBROUTINE GETOPSYS(ID)
!      USE WINTERACTER
          IMPLICIT NONE
          INTEGER ID
!      ID=INFOOPSYSTEM1(1)
          IF(ID.EQ.6) THEN
!      ID=INFOOPSYSTEM1(16)
          ELSE
             ID=1000
          END IF
      END


      SUBROUTINE MY_INKEYEVENTIMM(N)
          !     USE WINTERACTER
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INTEGER N
          !     TYPE(WIN_MESSAGE)   :: MESSAGE
          !     INTEGER             :: ITYPE
          N=-999
!      CALL WMessagePeek(ITYPE, MESSAGE)
!          SELECT CASE (ITYPE)
!          CASE ('CloseRequest')
          N=3
!          END SELECT
          RETURN
      END


      SUBROUTINE MY_INFOINPUT(I)
!      USE WINTERACTER
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'resource.inc'
          INTEGER I
!      TYPE (WIN_MESSAGE) MESG
!      Call WMessage(ITYPE,MESG)
!      SELECT CASE (ITYPE)
!      CASE (CloseRequest)
          I=260
!      CASE (KeyDown)
          I=260
!      CASE (Resize)
          I=259
!      CASE (Expose)
          I=259
C     CASE (EditorCommand)
C               I=260
!      CASE (MenuSelect)
!                        SELECT CASE (MESG%VALUE1)
!                        CASE(ID_EXIT)
          I=260
!                        END SELECT
!      END SELECT
          RETURN
      END

