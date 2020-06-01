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

C       FOURTH FILE OF PLOT/CAD ROUTINES

C SUB PLTCHRSH.FOR
      SUBROUTINE PLTCHRSH
          IMPLICIT NONE
          CHARACTER*11 AXMIN,AXMAX,AYMIN,AYMAX,YHIGHH,XHIGHH
          LOGICAL TABEXIST
          REAL*8 SHIFT_TABLE(1:10,1:6),MINSHIFT,MAXSHIFT,MINWAVE,MAXWAVE
          REAL*8 DNUMWAVE
          COMMON/TABLESHIFT/SHIFT_TABLE,TABEXIST
          INTEGER NUMWAVE,I,PLV
          COMMON/WAVEMAX/NUMWAVE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
          PLV=1
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='CHRSHIFT ACC'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          DNUMWAVE=DBLE(NUMWAVE)
          IF(WQ(1:1).EQ.'X') PLV=2
C
C
C     THIS PLOTS CHROMATIC FOCUS SHIFT DATA
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"PLTCHRSH" PLOTS XZ OR YZ-PLANE CHROMATIC FOCAL SHIFT DATA'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        '"PLTCHRSH" TAKES NO STRING OR NUMERIC WORD INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'X') THEN
              OUTLYNE=
     1        '"PLTCHRSH" ONLY TAKES "X" AS OPTIONAL QUALIFIER INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.TABEXIST) THEN
              OUTLYNE=
     1          'A CHROMATIC FOCUS SHIFT TABLE COULD NOT BE GENERATED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO PLOT WILL BE PRODUCED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          MINWAVE=SHIFT_TABLE(1,2)
          MAXWAVE=SHIFT_TABLE(NUMWAVE,2)
          MINSHIFT=1.0D20
          MAXSHIFT=-1.0D20
          DO I=1,NUMWAVE
              IF(SHIFT_TABLE(I,3).LT.MINSHIFT)MINSHIFT=SHIFT_TABLE(I,3)
              IF(SHIFT_TABLE(I,4).LT.MINSHIFT)MINSHIFT=SHIFT_TABLE(I,4)
              IF(SHIFT_TABLE(I,5).LT.MINSHIFT)MINSHIFT=SHIFT_TABLE(I,5)
              IF(SHIFT_TABLE(I,6).LT.MINSHIFT)MINSHIFT=SHIFT_TABLE(I,6)
              IF(SHIFT_TABLE(I,3).GT.MAXSHIFT)MAXSHIFT=SHIFT_TABLE(I,3)
              IF(SHIFT_TABLE(I,4).GT.MAXSHIFT)MAXSHIFT=SHIFT_TABLE(I,4)
              IF(SHIFT_TABLE(I,5).GT.MAXSHIFT)MAXSHIFT=SHIFT_TABLE(I,5)
              IF(SHIFT_TABLE(I,6).GT.MAXSHIFT)MAXSHIFT=SHIFT_TABLE(I,6)
          END DO
          IF(DABS(MINSHIFT).GT.DABS(MAXSHIFT)) THEN
              MAXSHIFT=DABS(MINSHIFT)
          ELSE
              MINSHIFT=-DABS(MAXSHIFT)
          END IF

          WRITE(AXMIN,20) MINSHIFT
          WRITE(AXMAX,20) MAXSHIFT
          WRITE(AYMIN,20) MINWAVE
          WRITE(AYMAX,20) MAXWAVE
          WRITE(XHIGHH,20) DNUMWAVE
          WRITE(YHIGHH,20) DNUMWAVE+10.0D0
20        FORMAT(G11.4)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT NEW'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT UXAXIS '//AXMIN //AXMAX
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT UYAXIS '//AYMIN //AYMAX
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT UYLINE 0'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT UYAXISLB WAVELENGTH IN MICROMETER'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SAVE_KDP(1)=SAVEINPT(1)
          IF(SYSTEM1(30).LE.2.0D0) THEN
C     FOCAL
              INPUT='PLOT UXAXISLB FOCAL SHIFT IN MICROMETER'
              CALL PROCES
          ELSE
C     AFOCAL
              INPUT='PLOT UXAXISLB FOCAL SHIFT IN MICRO-RADIANS'
              CALL PROCES
          END IF
          REST_KDP(1)=RESTINPT(1)

          IF(PLV.EQ.2) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C
C     REGISTERS USED ARE 1 THROUGH NUMWAVE FOR THE FOCUS SHIFTS
C     AND                11 THROU  10+NUMWAVE FOR THE WAVELENGTHS
C     WE DO THIS FIRST FOR THE XZ-PLANE, THEN THE YZ-PLANE
C
C     XZ-PLAME MARGINAL
              DO I=1,NUMWAVE
                  GPREG(I)=SHIFT_TABLE(I,5)
                  GPREG(I+10)=SHIFT_TABLE(I,2)
              END DO
              INPUT='PLOT UPLOT 1,'//XHIGHH//',11,'//YHIGHH//',0'
              CALL PROCES
C
              REST_KDP(1)=RESTINPT(1)
              SAVE_KDP(1)=SAVEINPT(1)
C
C     REGISTERS USED ARE 1 THROUGH NUMWAVE FOR THE FOCUS SHIFTS
C     AND                11 THROU  10+NUMWAVE FOR THE WAVELENGTHS
C     WE DO THIS FIRST FOR THE XZ-PLANE, THEN THE YZ-PLANE
C
C     XZ-PLAME CHIEF
              DO I=1,NUMWAVE
                  GPREG(I)=SHIFT_TABLE(I,6)
                  GPREG(I+10)=SHIFT_TABLE(I,2)
              END DO
              INPUT='PLOT UPLOT 1,'//XHIGHH//',11,'//YHIGHH//',2'
              CALL PROCES
C
              REST_KDP(1)=RESTINPT(1)
          END IF
          IF(PLV.EQ.1) THEN
              SAVE_KDP(1)=SAVEINPT(1)
C
C     REGISTERS USED ARE 1 THROUGH NUMWAVE FOR THE FOCUS SHIFTS
C     AND                11 THROU  10+NUMWAVE FOR THE WAVELENGTHS
C     WE DO THIS FIRST FOR THE XZ-PLANE, THEN THE YZ-PLANE
C
C     YZ-PLAME MARGINAL
              DO I=1,NUMWAVE
                  GPREG(I)=SHIFT_TABLE(I,3)
                  GPREG(I+10)=SHIFT_TABLE(I,2)
              END DO
              INPUT='PLOT UPLOT 1,'//XHIGHH//',11,'//YHIGHH//',0'
              CALL PROCES
C
              REST_KDP(1)=RESTINPT(1)
              SAVE_KDP(1)=SAVEINPT(1)
C
C     REGISTERS USED ARE 1 THROUGH NUMWAVE FOR THE FOCUS SHIFTS
C     AND                11 THROU  10+NUMWAVE FOR THE WAVELENGTHS
C     WE DO THIS FIRST FOR THE XZ-PLANE, THEN THE YZ-PLANE
C
C     YZ-PLAME CHIEF
              DO I=1,NUMWAVE
                  GPREG(I)=SHIFT_TABLE(I,4)
                  GPREG(I+10)=SHIFT_TABLE(I,2)
              END DO
              INPUT='PLOT UPLOT 1,'//XHIGHH//',11,'//YHIGHH//',2'
              CALL PROCES
C
              REST_KDP(1)=RESTINPT(1)
          END IF
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT CHNOTE 2'
          CALL PROCES
          IF(PLV.EQ.1) THEN
              INPUT='PNOTE FOCAL SHIFT VERSUS WAVELENGTH, YZ-PLANE'
              CALL PROCES
              INPUT='PLOT NOTE 5000 400'
              CALL PROCES
          END IF
          IF(PLV.EQ.2) THEN
              INPUT='PNOTE FOCAL SHIFT VERSUS WAVELENGTH, XZ-PLANE'
              CALL PROCES
              INPUT='PLOT NOTE 5000 400'
              CALL PROCES
          END IF
          INPUT='PLOT LSTYLE 0'
          CALL PROCES
          INPUT='PLOT PEN 2000 6500 3'
          CALL PROCES
          INPUT='PLOT PEN 3000 6500 2'
          CALL PROCES
          INPUT='PLOT LSTYLE 2'
          CALL PROCES
          INPUT='PLOT PEN 2000 6000 3'
          CALL PROCES
          INPUT='PLOT PEN 3000 6000 2'
          CALL PROCES
          INPUT='PLOT CHNOTE 2'
          CALL PROCES
          INPUT='PNOTE MARGINAL RAY AXIAL FOCUS SHIFT'
          CALL PROCES
          INPUT='PLOT NOTE 5300 6400'
          CALL PROCES
          INPUT='PNOTE CHIEF RAY LATERAL SHIFT'
          CALL PROCES
          INPUT='PLOT NOTE 4800 5900'
          CALL PROCES
          INPUT='PLOT LSTYLE 2'
          CALL PROCES
          INPUT='PLOT CHNOTE 1'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='DRAW'
          call setonecolors2
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
C
C
          RETURN
      END
C SUB CHRSHIFT.FOR
      SUBROUTINE CHRSHIFT
          IMPLICIT NONE
          LOGICAL AF,TABEXIST
          INTEGER WVCOUNT,I,J,K
          INTEGER NUMWAVE
          COMMON/WAVEMAX/NUMWAVE
          REAL*8 SHIFT_TABLE(1:10,1:6),LAM_TABLE(1:10,1:2)
          REAL*8 JK_A(1:10),JK_1(1:10),JK_2(1:10),ZERO(1:4)
          REAL*8 SPY,SPUY,SPCY,SPUCY
          REAL*8 SPX,SPUX,SPCX,SPUCX
          REAL*8 SCWPY,SCWPUY,SCWPCY,SCWPUCY
          REAL*8 SCWPX,SCWPUX,SCWPCX,SCWPUCX
          COMMON/TABLESHIFT/SHIFT_TABLE,TABEXIST
          CHARACTER LAM_CHAR(1:10)*2,CW_CHAR*2
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          LOGICAL NOPRINT
          NOPRINT=.FALSE.
          IF(WQ.EQ.'ACC') THEN
              NOPRINT=.TRUE.
              SQ=0
              WQ='        '
          END IF

          IF(STI.EQ.1) THEN
              OUTLYNE=
     1          '"CHRSHIFT" DISPLAYS CHROMATIC FOCUS SHIFT DATA'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1          '"CHRSHIFT" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     AFOCAL OR NOT
          AF=.FALSE.
          IF(SYSTEM1(30).GE.3.0D0) AF=.TRUE.
C     ENOUGH WAVELENGTHS ?
          WVCOUNT=0
          TABEXIST=.FALSE.
          IF(SYSTEM1(1).NE.0.0D0.AND.SYSTEM1(31).NE.0.0D0) WVCOUNT=WVCOUNT+1
          IF(SYSTEM1(2).NE.0.0D0.AND.SYSTEM1(32).NE.0.0D0) WVCOUNT=WVCOUNT+1
          IF(SYSTEM1(3).NE.0.0D0.AND.SYSTEM1(33).NE.0.0D0) WVCOUNT=WVCOUNT+1
          IF(SYSTEM1(4).NE.0.0D0.AND.SYSTEM1(34).NE.0.0D0) WVCOUNT=WVCOUNT+1
          IF(SYSTEM1(5).NE.0.0D0.AND.SYSTEM1(35).NE.0.0D0) WVCOUNT=WVCOUNT+1
          IF(SYSTEM1(71).NE.0.0D0.AND.SYSTEM1(76).NE.0.0D0)
     1    WVCOUNT=WVCOUNT+1
          IF(SYSTEM1(72).NE.0.0D0.AND.SYSTEM1(77).NE.0.0D0)
     1    WVCOUNT=WVCOUNT+1
          IF(SYSTEM1(73).NE.0.0D0.AND.SYSTEM1(78).NE.0.0D0)
     1    WVCOUNT=WVCOUNT+1
          IF(SYSTEM1(74).NE.0.0D0.AND.SYSTEM1(79).NE.0.0D0)
     1    WVCOUNT=WVCOUNT+1
          IF(SYSTEM1(75).NE.0.0D0.AND.SYSTEM1(80).NE.0.0D0)
     1    WVCOUNT=WVCOUNT+1
C
          IF(WVCOUNT.LE.1) THEN
              WRITE(OUTLYNE,*)
     1        'MORE THAN ONE ACTIVE WAVELENGTH MUST EXIST FOR "CRSHIFT"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'TO WORK.'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          LAM_TABLE(1:10,1:2)=0.0D0
          SHIFT_TABLE(1:10,1:6)=0.0D0
C     SORT ACTIVE WAVELENGTS AND COUNT MAXIMUM ACTIVE WAVELENGTHS
          J=0
          IF(SYSTEM1(1).NE.0.0D0.AND.SYSTEM1(31).NE.0.0D0) THEN
              J=J+1
              LAM_TABLE(J,1)=1.0D0
              LAM_TABLE(J,2)=SYSTEM1(1)
          END IF
          IF(SYSTEM1(2).NE.0.0D0.AND.SYSTEM1(32).NE.0.0D0) THEN
              J=J+1
              LAM_TABLE(J,1)=2.0D0
              LAM_TABLE(J,2)=SYSTEM1(2)
          END IF
          IF(SYSTEM1(3).NE.0.0D0.AND.SYSTEM1(33).NE.0.0D0) THEN
              J=J+1
              LAM_TABLE(J,1)=3.0D0
              LAM_TABLE(J,2)=SYSTEM1(3)
          END IF
          IF(SYSTEM1(4).NE.0.0D0.AND.SYSTEM1(34).NE.0.0D0) THEN
              J=J+1
              LAM_TABLE(J,1)=4.0D0
              LAM_TABLE(J,2)=SYSTEM1(4)
          END IF
          IF(SYSTEM1(5).NE.0.0D0.AND.SYSTEM1(35).NE.0.0D0) THEN
              J=J+1
              LAM_TABLE(J,1)=5.0D0
              LAM_TABLE(J,2)=SYSTEM1(5)
          END IF
          IF(SYSTEM1(71).NE.0.0D0.AND.SYSTEM1(76).NE.0.0D0) THEN
              J=J+1
              LAM_TABLE(J,1)=6.0D0
              LAM_TABLE(J,2)=SYSTEM1(71)
          END IF
          IF(SYSTEM1(72).NE.0.0D0.AND.SYSTEM1(77).NE.0.0D0) THEN
              J=J+1
              LAM_TABLE(J,1)=7.0D0
              LAM_TABLE(J,2)=SYSTEM1(72)
          END IF
          IF(SYSTEM1(73).NE.0.0D0.AND.SYSTEM1(78).NE.0.0D0) THEN
              J=J+1
              LAM_TABLE(J,1)=8.0D0
              LAM_TABLE(J,2)=SYSTEM1(73)
          END IF
          IF(SYSTEM1(74).NE.0.0D0.AND.SYSTEM1(79).NE.0.0D0) THEN
              LAM_TABLE(J,1)=9.0D0
              LAM_TABLE(J,2)=SYSTEM1(74)
          END IF
          IF(SYSTEM1(75).NE.0.0D0.AND.SYSTEM1(80).NE.0.0D0) THEN
              J=J+1
              LAM_TABLE(J,1)=10.0D0
              LAM_TABLE(J,2)=SYSTEM1(75)
          END IF
          NUMWAVE=J
C     WE NOW HAVE NUMWAVE ENTRIES IN LAM_TABLE TO SORT BY
C     INCREASING WAVELENGTH IN MICROMETER
          DO I=1,NUMWAVE
              JK_A(I)=LAM_TABLE(I,2)
              JK_1(I)=LAM_TABLE(I,1)
              JK_2(I)=LAM_TABLE(I,2)
          END DO
          CALL SORT_JK1(NUMWAVE,JK_A(1:NUMWAVE),JK_1(1:NUMWAVE)
     1    ,JK_2(1:NUMWAVE))
          DO I=1,NUMWAVE
              LAM_TABLE(I,1)=JK_1(I)
              LAM_TABLE(I,2)=JK_2(I)
              SHIFT_TABLE(I,1)=JK_1(I)
              SHIFT_TABLE(I,2)=JK_2(I)
          END DO
          DO I=1,NUMWAVE
              IF(SHIFT_TABLE(I,1).EQ.1.0D0)  LAM_CHAR(I)=' 1'
              IF(SHIFT_TABLE(I,1).EQ.2.0D0)  LAM_CHAR(I)=' 2'
              IF(SHIFT_TABLE(I,1).EQ.3.0D0)  LAM_CHAR(I)=' 3'
              IF(SHIFT_TABLE(I,1).EQ.4.0D0)  LAM_CHAR(I)=' 4'
              IF(SHIFT_TABLE(I,1).EQ.5.0D0)  LAM_CHAR(I)=' 5'
              IF(SHIFT_TABLE(I,1).EQ.6.0D0)  LAM_CHAR(I)=' 6'
              IF(SHIFT_TABLE(I,1).EQ.7.0D0)  LAM_CHAR(I)=' 7'
              IF(SHIFT_TABLE(I,1).EQ.8.0D0)  LAM_CHAR(I)=' 8'
              IF(SHIFT_TABLE(I,1).EQ.9.0D0)  LAM_CHAR(I)=' 9'
              IF(SHIFT_TABLE(I,1).EQ.10.0D0) LAM_CHAR(I)='10'
          END DO
          DO I=1,NUMWAVE
              IF(SYSTEM1(11).EQ.1.0D0)   CW_CHAR=' 1'
              IF(SYSTEM1(11).EQ.2.0D0)   CW_CHAR=' 2'
              IF(SYSTEM1(11).EQ.3.0D0)   CW_CHAR=' 3'
              IF(SYSTEM1(11).EQ.4.0D0)   CW_CHAR=' 4'
              IF(SYSTEM1(11).EQ.5.0D0)   CW_CHAR=' 5'
              IF(SYSTEM1(11).EQ.6.0D0)   CW_CHAR=' 6'
              IF(SYSTEM1(11).EQ.7.0D0)   CW_CHAR=' 7'
              IF(SYSTEM1(11).EQ.8.0D0)   CW_CHAR=' 8'
              IF(SYSTEM1(11).EQ.9.0D0)   CW_CHAR=' 9'
              IF(SYSTEM1(11).EQ.10.0D0)  CW_CHAR='10'
          END DO
          SAVE_KDP(13)=SAVEINPT(13)
          INPUT='GET PY,,'//CW_CHAR
          CALL PROCES
          SCWPY=REG(9)
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          INPUT='GET PUY,,'//CW_CHAR
          CALL PROCES
          SCWPUY=REG(9)
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          INPUT='GET PCY,,'//CW_CHAR
          CALL PROCES
          SCWPCY=REG(9)
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          INPUT='GET PUCY,,'//CW_CHAR
          CALL PROCES
          SCWPUCY=REG(9)
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          INPUT='GET PX,,'//CW_CHAR
          CALL PROCES
          SCWPX=REG(9)
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          INPUT='GET PUX,,'//CW_CHAR
          CALL PROCES
          SCWPUX=REG(9)
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          INPUT='GET PCX,,'//CW_CHAR
          CALL PROCES
          SCWPCX=REG(9)
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          INPUT='GET PUCX,,'//CW_CHAR
          CALL PROCES
          SCWPUCX=REG(9)
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.AF) THEN
C     FOCAL
              ZERO(1)=-SCWPY*SCWPUY
              ZERO(2)=SCWPCY
              ZERO(3)=-SCWPX*SCWPUX
              ZERO(4)=SCWPCX
          ELSE
C     AFOCAL
              ZERO(1)=SCWPUY
              ZERO(2)=SCWPUCY
              ZERO(3)=SCWPUX
              ZERO(4)=SCWPUCX
          END IF
C
          IF(.NOT.NOPRINT) THEN
              IF(.NOT.AF) WRITE(OUTLYNE,20)
              IF(AF) WRITE(OUTLYNE,21)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,24)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,23)
              CALL SHOWIT(0)
20            FORMAT('CHROMATIC SHIFT IN MICROMETER, WAVELENGTH IN MICROMETER')
21            FORMAT('CHROMATIC SHIFT IN MILLIRADIANS, WAVELENGTH IN MICROMETER')
24            FORMAT('ALL SHIFTS REFERENCED TO THE CURRENT CONTROL WAVELENGTH')
22            FORMAT('WAVELENGTH',3X,'MARG. Y-SHIFT',3X,'CHIEF Y-SHIFT'
     1        ,3X,'MARG. X-SHIFT',3X,'CHIEF X-SHIFT')
              WRITE(OUTLYNE,22)
              CALL SHOWIT(0)
29            FORMAT(' (MICROMETER)',2X,'(AXIAL MOTION)',3X,'(TRANSVERSE) '
     1        ,2X,'(AXIAL MOTION)',3X,'(TRANSVERSE) ')
              WRITE(OUTLYNE,29)
              CALL SHOWIT(0)
23            FORMAT(1X)
          END IF

          DO I=1,NUMWAVE
              SAVE_KDP(13)=SAVEINPT(13)
              INPUT='GET PY,,'//LAM_CHAR(I)
              CALL PROCES
              SPY=REG(9)
              REST_KDP(13)=RESTINPT(13)
              SAVE_KDP(13)=SAVEINPT(13)
              INPUT='GET PUY,,'//LAM_CHAR(I)
              CALL PROCES
              SPUY=REG(9)
              REST_KDP(13)=RESTINPT(13)
              SAVE_KDP(13)=SAVEINPT(13)
              INPUT='GET PCY,,'//LAM_CHAR(I)
              CALL PROCES
              SPCY=REG(9)
              REST_KDP(13)=RESTINPT(13)
              SAVE_KDP(13)=SAVEINPT(13)
              INPUT='GET PUCY,,'//LAM_CHAR(I)
              CALL PROCES
              SPUCY=REG(9)
              REST_KDP(13)=RESTINPT(13)
              SAVE_KDP(13)=SAVEINPT(13)
              INPUT='GET PX,,'//LAM_CHAR(I)
              CALL PROCES
              SPX=REG(9)
              REST_KDP(13)=RESTINPT(13)
              SAVE_KDP(13)=SAVEINPT(13)
              INPUT='GET PUX,,'//LAM_CHAR(I)
              CALL PROCES
              SPUX=REG(9)
              REST_KDP(13)=RESTINPT(13)
              SAVE_KDP(13)=SAVEINPT(13)
              INPUT='GET PCX,,'//LAM_CHAR(I)
              CALL PROCES
              SPCX=REG(9)
              REST_KDP(13)=RESTINPT(13)
              SAVE_KDP(13)=SAVEINPT(13)
              INPUT='GET PUCX,,'//LAM_CHAR(I)
              CALL PROCES
              SPUCX=REG(9)
              REST_KDP(13)=RESTINPT(13)

              IF(.NOT.AF) THEN
C     FOCAL
C     CONVERT TO MICROMETER
                  SHIFT_TABLE(I,3)=(-SPY*SPUY)-ZERO(1)
                  SHIFT_TABLE(I,4)=(SPCY)-ZERO(2)
                  SHIFT_TABLE(I,5)=(-SPX*SPUX)-ZERO(3)
                  SHIFT_TABLE(I,6)=(SPCX)-ZERO(4)
                  DO K=3,6
                      IF(SYSTEM1(6).EQ.1.0D0)
     1                SHIFT_TABLE(I,K)=(SHIFT_TABLE(I,K)/25.4D0)*1.0D3
                      IF(SYSTEM1(6).EQ.2.0D0)
     1                SHIFT_TABLE(I,K)=SHIFT_TABLE(I,K)*1.0D4
                      IF(SYSTEM1(6).EQ.3.0D0)
     1                SHIFT_TABLE(I,K)=SHIFT_TABLE(I,K)*1.0D3
                      IF(SYSTEM1(6).EQ.4.0D0)
     1                SHIFT_TABLE(I,K)=SHIFT_TABLE(I,K)*1.0D6
                  END DO
              ELSE
C     AFOCAL
C     CONVERT TO MILLIRADIANS
                  SHIFT_TABLE(I,3)=DATAN(SPUY-ZERO(1))*1.0D3
                  SHIFT_TABLE(I,4)=DATAN(SPUCY-ZERO(2))*1.0D3
                  SHIFT_TABLE(I,5)=DATAN(SPUX-ZERO(3))*1.0D3
                  SHIFT_TABLE(I,6)=DATAN(SPUCX-ZERO(4))*1.0D3
              END IF
              IF(.NOT.NOPRINT) THEN
                  WRITE(OUTLYNE,10) SNGL(SHIFT_TABLE(I,2)),SHIFT_TABLE(I,3)
     1            ,SHIFT_TABLE(I,4),SHIFT_TABLE(I,5)
     2            ,SHIFT_TABLE(I,6)
                  CALL SHOWIT(0)
              END IF
          END DO
          REST_KDP(1)=RESTINPT(1)

          TABEXIST=.TRUE.
10        FORMAT(F10.5,3X,G13.6,3X,G13.6,3X,G13.6,3X,G13.6)
          CHRSHIFTEXT=.TRUE.
          RETURN
      END
C SUB CAPFIX.FOR
      SUBROUTINE CAPFIX
C
          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE BEST AND NOTILT COMMANDS
C
          INTEGER I

          REAL*8 NUMT1,
     1    NUMT2,NUMT3,NUMT4,NUMT5,NUMT6,NUMT7,NUMT8,NUMT9,NUMT10
     2    ,NUMTOTAL,NUMT0
C
          REAL*8 WEIS,REFHT,SUMSIG,WEI(1:10),LAM(1:10)
     1    ,SUML2,SUML4
C
          REAL*8 ARG
C
          REAL*8 SUMONE(0:10),SUMTWO(0:10),OPPIT(0:10),
     1    SIG,OPPEAK(0:10),TOT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          PTOVOPD(0:10)=0.0D0
          SUMONE(0:10)=0.0D0
          SUMTWO(0:10)=0.0D0
          RMSOP(0:10)=0.0D0
          OPPEAK(0:10)=-1.0D99
          OPPIT(0:10) = 1.0D99
          RMSOPD=0.0D0
C
C     COMPUTE THE EFFECTIVE WAVELENGTH
          SUML2=0.0D0
          SUML4=0.0D0
          LAMAVE=0.0D0
          WEI(1)=SYSTEM1(31)
          WEI(2)=SYSTEM1(32)
          WEI(3)=SYSTEM1(33)
          WEI(4)=SYSTEM1(34)
          WEI(5)=SYSTEM1(35)
          WEI(6)=SYSTEM1(76)
          WEI(7)=SYSTEM1(77)
          WEI(8)=SYSTEM1(78)
          WEI(9)=SYSTEM1(79)
          WEI(10)=SYSTEM1(80)
          LAM(1)=SYSTEM1(1)
          LAM(2)=SYSTEM1(2)
          LAM(3)=SYSTEM1(3)
          LAM(4)=SYSTEM1(4)
          LAM(5)=SYSTEM1(5)
          LAM(6)=SYSTEM1(71)
          LAM(7)=SYSTEM1(72)
          LAM(8)=SYSTEM1(73)
          LAM(9)=SYSTEM1(74)
          LAM(10)=SYSTEM1(75)
          WEIS=0.0D0

          DO I=1,10
              WEIS=WEIS+WEI(I)
          END DO

          IF(WEIS.EQ.0.0D0) WEIS=1.0D0

          DO I=1,10
              WEI(I)=WEI(I)/WEIS
          END DO

          DO I=1,10
              IF(LAM(I).NE.0.0D0.AND.WEI(I).NE.0.0D0) THEN
                  SUML2=SUML2+(WEI(I)/(LAM(I)**2))
                  SUML4=SUML4+(WEI(I)/(LAM(I)**4))
              END IF
          END DO
          LAMAVE=DSQRT(SUML2/SUML4)
          IF(SYSTEM1(6).EQ.1.0D0) THEN
              LAMAVE=LAMAVE*3.93700787402D-5
          END IF
          IF(SYSTEM1(6).EQ.2.0D0) THEN
              LAMAVE=LAMAVE*1.0D-4
          END IF
          IF(SYSTEM1(6).EQ.3.0D0) THEN
              LAMAVE=LAMAVE*1.0D-3
          END IF
          IF(SYSTEM1(6).EQ.4.0D0) THEN
              LAMAVE=LAMAVE*1.0D-6
          END IF
C
          IF(STI.EQ.1) THEN
              OUTLYNE= WC//' ACTS ON THE EXISTING CAPFN'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE= WC//' TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     OPEN AND LOAD THE APPROPRIATE CAPFN DATA AND THEN PLOT IT
          IF(.NOT.CPFNEXT) THEN
              OUTLYNE=
     1        'NO CAPFN EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     DETERMINE THE REFERNCE APERTURE HEIGHT
C     OF THE REF SURFACE COORDINATES
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.DABS(ALENS(9,NEWREF))
     1    .LE.5.0D0) THEN
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  REFHT=(ALENS(10,NEWREF))
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=(ALENS(10,NEWREF))
                  ELSE
                      REFHT=(ALENS(11,NEWREF))
                  END IF
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=(ALENS(10,NEWREF))
                  ELSE
                      REFHT=(ALENS(11,NEWREF))
                  END IF
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=(ALENS(10,NEWREF))
                  ELSE
                      REFHT=(ALENS(11,NEWREF))
                  END IF
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  REFHT=(ALENS(10,NEWREF))
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              IF(PXTRAY(1,NEWREF).GE.PXTRAX(1,NEWREF)) THEN
                  REFHT=((PXTRAY(1,NEWREF)))
              ELSE
                  REFHT=((PXTRAX(1,NEWREF)))
              END IF
          END IF

          IF(WC.EQ.'NOTILT') CALL CAPFIX1(1,REFHT)
          IF(WC.EQ.'BEST') CALL CAPFIX1(2,REFHT)
          NUMT0=0.0D0
          NUMT1=0.0D0
          NUMT2=0.0D0
          NUMT3=0.0D0
          NUMT4=0.0D0
          NUMT5=0.0D0
          NUMT6=0.0D0
          NUMT7=0.0D0
          NUMT8=0.0D0
          NUMT9=0.0D0
          NUMT10=0.0D0
          NUMTOTAL=0.0D0
          TOT=0.0D0
          NUMTOT=0
          DO I=1,ITOT-1
C     LOAD DSPOTT(*ID) INTO DSPOT(*,)
              ID=I
              CALL SPOTIT(4)
              IF(DSPOT(12).NE.0.0D0) THEN
                  TOT=TOT+(DSPOT(17)*DSPOT(11))
                  NUMTOT=NUMTOT+1
              END IF
              IF(DSPOT(12).NE.0.0D0) THEN
                  SUMONE(0)=SUMONE(0)+(DSPOT(4)/(TWOPII))
                  SUMTWO(0)=SUMTWO(0)+((DSPOT(4)/(TWOPII))**2)
                  NUMT0=NUMT0+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.1.0D0) THEN
                  SUMONE(1)=SUMONE(1)+(DSPOT(4)/(TWOPII))
                  SUMTWO(1)=SUMTWO(1)+((DSPOT(4)/(TWOPII))**2)
                  NUMT1=NUMT1+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.2.0D0) THEN
                  SUMONE(2)=SUMONE(2)+(DSPOT(4)/(TWOPII))
                  SUMTWO(2)=SUMTWO(2)+((DSPOT(4)/(TWOPII))**2)
                  NUMT2=NUMT2+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.3.0D0) THEN
                  SUMONE(3)=SUMONE(3)+(DSPOT(4)/(TWOPII))
                  SUMTWO(3)=SUMTWO(3)+((DSPOT(4)/(TWOPII))**2)
                  NUMT3=NUMT3+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.4.0D0) THEN
                  SUMONE(4)=SUMONE(4)+(DSPOT(4)/(TWOPII))
                  SUMTWO(4)=SUMTWO(4)+((DSPOT(4)/(TWOPII))**2)
                  NUMT4=NUMT4+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.5.0D0) THEN
                  SUMONE(5)=SUMONE(5)+(DSPOT(4)/(TWOPII))
                  SUMTWO(5)=SUMTWO(5)+((DSPOT(4)/(TWOPII))**2)
                  NUMT5=NUMT5+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.6.0D0) THEN
                  SUMONE(6)=SUMONE(6)+(DSPOT(4)/(TWOPII))
                  SUMTWO(6)=SUMTWO(6)+((DSPOT(4)/(TWOPII))**2)
                  NUMT6=NUMT6+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.7.0D0) THEN
                  SUMONE(7)=SUMONE(7)+(DSPOT(4)/(TWOPII))
                  SUMTWO(7)=SUMTWO(7)+((DSPOT(4)/(TWOPII))**2)
                  NUMT7=NUMT7+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.8.0D0) THEN
                  SUMONE(8)=SUMONE(8)+(DSPOT(4)/(TWOPII))
                  SUMTWO(8)=SUMTWO(8)+((DSPOT(4)/(TWOPII))**2)
                  NUMT8=NUMT8+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.9.0D0) THEN
                  SUMONE(9)=SUMONE(9)+(DSPOT(4)/(TWOPII))
                  SUMTWO(9)=SUMTWO(9)+((DSPOT(4)/(TWOPII))**2)
                  NUMT9=NUMT9+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.10.0D0) THEN
                  SUMONE(10)=SUMONE(10)+(DSPOT(4)/(TWOPII))
                  SUMTWO(10)=SUMTWO(10)+((DSPOT(4)/(TWOPII))**2)
                  NUMT10=NUMT10+1
              END IF

C
              IF(DSPOT(12).NE.0.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(0))
     1            OPPEAK(0)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(0))
     1            OPPIT(0)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.1.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(1))
     1            OPPEAK(1)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(1))
     1            OPPIT(1)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.2.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(2))
     1            OPPEAK(2)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(2))
     1            OPPIT(2)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.3.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(3))
     1            OPPEAK(3)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(3))
     1            OPPIT(3)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.4.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(4))
     1            OPPEAK(4)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(4))
     1            OPPIT(4)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.5.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(5))
     1            OPPEAK(5)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(5))
     1            OPPIT(5)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.6.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(6))
     1            OPPEAK(6)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(6))
     1            OPPIT(6)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.7.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(7))
     1            OPPEAK(7)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(7))
     1            OPPIT(7)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.8.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(8))
     1            OPPEAK(8)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(8))
     1            OPPIT(8)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.9.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(9))
     1            OPPEAK(9)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(9))
     1            OPPIT(9)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.10.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(10))
     1            OPPEAK(10)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(10))
     1            OPPIT(10)=DSPOT(4)/(TWOPII)
              END IF
          END DO
          IF(NUMT0.NE.0.0D0) THEN
              ARG=((SUMTWO(0)-((SUMONE(0)**2)/
     1        DBLE(NUMT0)))/DBLE(NUMT0-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(0)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT1.NE.0.0D0) THEN
              ARG=((SUMTWO(1)-((SUMONE(1)**2)/
     1        DBLE(NUMT1)))/DBLE(NUMT1-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(1)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT2.NE.0.0D0) THEN
              ARG=((SUMTWO(2)-((SUMONE(2)**2)/
     1        DBLE(NUMT2)))/DBLE(NUMT2-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(2)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT3.NE.0.0D0) THEN
              ARG=((SUMTWO(3)-((SUMONE(3)**2)/
     1        DBLE(NUMT3)))/DBLE(NUMT3-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(3)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT4.NE.0.0D0) THEN
              ARG=((SUMTWO(4)-((SUMONE(4)**2)/
     1        DBLE(NUMT4)))/DBLE(NUMT4-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(4)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT5.NE.0.0D0) THEN
              ARG=((SUMTWO(5)-((SUMONE(5)**2)/
     1        DBLE(NUMT5)))/DBLE(NUMT5-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(5)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT6.NE.0.0D0) THEN
              ARG=((SUMTWO(6)-((SUMONE(6)**2)/
     1        DBLE(NUMT6)))/DBLE(NUMT6-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(6)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT7.NE.0.0D0) THEN
              ARG=((SUMTWO(7)-((SUMONE(7)**2)/
     1        DBLE(NUMT7)))/DBLE(NUMT7-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(7)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT8.NE.0.0D0) THEN
              ARG=((SUMTWO(8)-((SUMONE(8)**2)/
     1        DBLE(NUMT8)))/DBLE(NUMT8-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(8)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT9.NE.0.0D0) THEN
              ARG=((SUMTWO(9)-((SUMONE(9)**2)/
     1        DBLE(NUMT9)))/DBLE(NUMT9-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(9)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT10.NE.0.0D0) THEN
              ARG=((SUMTWO(10)-((SUMONE(10)**2)/
     1        DBLE(NUMT10)))/DBLE(NUMT10-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(10)=DSQRT(ARG)
              END IF
          END IF
C     NOW THE POLY CHROMATIC RMSOPD
          IF(LAMAVE.NE.0.0D0) THEN
C     CALC SUMSIG
              SUMSIG=0.0D0
              DO I=1,10
                  IF(WEI(I).NE.0.0D0.AND.RMSOP(I).NE.0.0D0.AND.
     1            LAM(I).NE.0.0D0)
     1            SUMSIG=SUMSIG+((WEI(I)*(RMSOP(I)**2))/(LAM(I)**2))
              END DO
          END IF
          SIG=0.0D0
          IF(SUML2.NE.0.0D0.AND.SUMSIG.NE.0.0D0)
     1    RMSOPD=DSQRT(SUMSIG/SUML2)
          RMSOP(0)=RMSOPD
          PTOVOPD(0)=OPPEAK(0)-OPPIT(0)
          PTOVOPD(1)=OPPEAK(1)-OPPIT(1)
          PTOVOPD(2)=OPPEAK(2)-OPPIT(2)
          PTOVOPD(3)=OPPEAK(3)-OPPIT(3)
          PTOVOPD(4)=OPPEAK(4)-OPPIT(4)
          PTOVOPD(5)=OPPEAK(5)-OPPIT(5)
          PTOVOPD(6)=OPPEAK(6)-OPPIT(6)
          PTOVOPD(7)=OPPEAK(7)-OPPIT(7)
          PTOVOPD(8)=OPPEAK(8)-OPPIT(8)
          PTOVOPD(9)=OPPEAK(9)-OPPIT(9)
          PTOVOPD(10)=OPPEAK(10)-OPPIT(10)
          RETURN
      END
C SUB CAPFIX1.FOR
      SUBROUTINE CAPFIX1(FTYPE,REFHT)

          USE SVDSUB

          IMPLICIT NONE
C
          EXTERNAL FF3
C
          INTEGER SSN,SM,NP2,MP,COUNT,I,J,N,M,II,IIP
     1    ,ISS,FTYPE
C
          COMMON/PRSIZE/COUNT
C
C       THIS PASSES THE SOLUTION OF THE LEAST SQUARES PROBLEM
          COMMON/SOLU/X
!      LOGICAL ERRR
C
          REAL*8 ACCUM(1:96,1:96),REFHT,TERM1,TERM2,
     1    WAVEX,CCOL(1:96),X(1:96),SUML2,SUML4,WEIS,
     2    VALX,VALY,WEI(1:10),LAM(1:10)
     3    ,DLLZX,DLLZY
     4    ,TERM3
C
          COMMON/ACDATA/ACCUM,CCOL
C
!      REAL*8 WVAL
C
          INTEGER III,JJ,ALLOERR,WWVN
C
          REAL*8 DWW1,DWW2,DWW3,DWW4,
     1    TERM,FF3,RHO,THETA
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
          REAL*8 W,V,U,WV,
     2    B,XXX
C
          DIMENSION W(:),V(:,:),U(:,:),
     2    B(:),XXX(:)
C
          ALLOCATABLE :: W,V,U,B,XXX
          DEALLOCATE (W,V,U,B,XXX
     1    ,STAT=ALLOERR)
          ALLOCATE (W(1:96),V(1:96,1:96),U(1:96,1:96),B(1:96),XXX(1:96)
     1    ,STAT=ALLOERR)
C
C     COMPUTE THE EFFECTIVE WAVELENGTH
          SUML2=0.0D0
          SUML4=0.0D0
          LAMAVE=0.0D0
          WEI(1)=SYSTEM1(31)
          WEI(2)=SYSTEM1(32)
          WEI(3)=SYSTEM1(33)
          WEI(4)=SYSTEM1(34)
          WEI(5)=SYSTEM1(35)
          WEI(6)=SYSTEM1(76)
          WEI(7)=SYSTEM1(77)
          WEI(8)=SYSTEM1(78)
          WEI(9)=SYSTEM1(79)
          WEI(10)=SYSTEM1(80)
          LAM(1)=SYSTEM1(1)
          LAM(2)=SYSTEM1(2)
          LAM(3)=SYSTEM1(3)
          LAM(4)=SYSTEM1(4)
          LAM(5)=SYSTEM1(5)
          LAM(6)=SYSTEM1(71)
          LAM(7)=SYSTEM1(72)
          LAM(8)=SYSTEM1(73)
          LAM(9)=SYSTEM1(74)
          LAM(10)=SYSTEM1(75)
          WEIS=0.0D0
          DO ISS=1,10
              WEIS=WEIS+WEI(ISS)
          END DO
          IF(WEIS.EQ.0.0D0) WEIS=1.0D0
          DO ISS=1,10
              WEI(ISS)=WEI(ISS)/WEIS
          END DO
          DO ISS=1,10
              IF(LAM(ISS).NE.0.0D0.AND.WEI(ISS).NE.0.0D0) THEN
                  SUML2=SUML2+(WEI(ISS)/(LAM(ISS)**2))
                  SUML4=SUML4+(WEI(ISS)/(LAM(ISS)**4))
              END IF
          END DO
          LAMAVE=DSQRT(SUML2/SUML4)
          IF(SYSTEM1(6).EQ.1.0D0) THEN
              LAMAVE=LAMAVE*3.93700787402D-5
          END IF
          IF(SYSTEM1(6).EQ.2.0D0) THEN
              LAMAVE=LAMAVE*1.0D-4
          END IF
          IF(SYSTEM1(6).EQ.3.0D0) THEN
              LAMAVE=LAMAVE*1.0D-3
          END IF
          IF(SYSTEM1(6).EQ.4.0D0) THEN
              LAMAVE=LAMAVE*1.0D-6
          END IF
C
          WAVEX=0.0D0
          DLLX=0.0D0
          DLLY=0.0D0
          DLLZ=0.0D0
          CCOL(1:96)=0.0D0
          CFTYPE(1:96)=0.0D0
          X(1:96)=0.0D0
          XXX(1:96)=0.0D0
          ACCUM(1:96,1:96)=0.0D0
C
          DO IIP=1,ITOT-1
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IIP
              CALL SPOTIT(4)
              IF(DSPOT(16).EQ.1.0D0)  WV=SYSTEM1(1)
              IF(DSPOT(16).EQ.2.0D0)  WV=SYSTEM1(2)
              IF(DSPOT(16).EQ.3.0D0)  WV=SYSTEM1(3)
              IF(DSPOT(16).EQ.4.0D0)  WV=SYSTEM1(4)
              IF(DSPOT(16).EQ.5.0D0)  WV=SYSTEM1(5)
              IF(DSPOT(16).EQ.6.0D0)  WV=SYSTEM1(71)
              IF(DSPOT(16).EQ.7.0D0)  WV=SYSTEM1(72)
              IF(DSPOT(16).EQ.8.0D0)  WV=SYSTEM1(73)
              IF(DSPOT(16).EQ.9.0D0)  WV=SYSTEM1(74)
              IF(DSPOT(16).EQ.10.0D0) WV=SYSTEM1(75)
              IF(DSPOT(16).EQ.1.0D0)  WWVN=46
              IF(DSPOT(16).EQ.2.0D0)  WWVN=47
              IF(DSPOT(16).EQ.3.0D0)  WWVN=48
              IF(DSPOT(16).EQ.4.0D0)  WWVN=49
              IF(DSPOT(16).EQ.5.0D0)  WWVN=50
              IF(DSPOT(16).EQ.6.0D0)  WWVN=71
              IF(DSPOT(16).EQ.7.0D0)  WWVN=72
              IF(DSPOT(16).EQ.8.0D0)  WWVN=73
              IF(DSPOT(16).EQ.9.0D0)  WWVN=74
              IF(DSPOT(16).EQ.10.0D0) WWVN=75
              IF(SYSTEM1(6).EQ.1.0D0) THEN
                  WV=WV*3.93700787402D-5
              END IF
              IF(SYSTEM1(6).EQ.2.0D0) THEN
                  WV=WV*1.0D-4
              END IF
              IF(SYSTEM1(6).EQ.3.0D0) THEN
                  WV=WV*1.0D-3
              END IF
              IF(SYSTEM1(6).EQ.4.0D0) THEN
                  WV=WV*1.0D-6
              END IF
              DWW1=DSPOT(6)/REFHT
              DWW2=DSPOT(5)/REFHT
              DWW3=((DSPOT(33)/(TWOPII)))*WV
              DWW4=DSPOT(12)
C       PROCEED WITH FITTING A TYPE 3 TYPE/SURFACE
              RHO=DSQRT((DWW1**2)+(DWW2**2))
              IF(DABS(DWW1).GE.DABS(((1.0D35)*DWW2))) THEN
                  IF(DWW1.GE.0.0D0) THETA=PII/2.0D0
                  IF(DWW1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(DWW1).LE.1.0D-15.AND.DABS(DWW2).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(DWW1,DWW2)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
C
              III=0
              DO I=1,4
C       FILL THE COLLUMN ARRAY
                  III=III+1
C       CALCULATE COLUMN VECTOR
C       FUNCTION FF3 RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       WITHOUT THE GIVEN DEPENDENT VARIABLE VALUE
C       OF THE WEIGHT FUNCTION FACTORED IN. THIS IS
C       FOR THE STANDARD ZERNIKE. I RANGES FROM 1 TO 4
                  TERM= FF3(RHO,THETA,I)
                  TERM=TERM*DWW3*DWW4
                  CCOL(III)=CCOL(III)+TERM
                  JJ=0
                  DO J=1,4
C       DO THE APPROPRIATE ROW FOR COLUMN I
                      JJ=JJ+1
                      TERM= FF3(RHO,THETA,I)*FF3(RHO,THETA,J)
                      ACCUM(III,JJ)=ACCUM(III,JJ)+(DWW4*TERM)
                  END DO
              END DO
              COUNT=III
          END DO
C       THE LARGE ARRAY WITH ACCUMULATED DATA IS
C       ACCUM. THE COLUMN VECTOR WITH FUNCTIONAL DATA
C       IS CCOL.
          MP=96
          NP2=96
          N=COUNT
          M=COUNT
          DO I=1,COUNT
              B(I)=CCOL(I)
              DO J=1,COUNT
                  U(I,J)=ACCUM(I,J)
              END DO
          END DO
C       DO SINGULAR VALUE DECOMPOSITION
C
          SM=M
          SSN=N
          CALL SVDCMP(U,SM,SSN,MP,NP2,W,V)
          M=SM
          N=SSN
C
C       SOLVE LINEAR EQUATION
C
          SM=M
          SSN=N
          CALL SVBKSB(U,W,V,SM,SSN,MP,NP2,B,XXX)
          M=SM
          N=SSN
          II=0
          DO I=1,4
              II=II+1
              CFTYPE(I)=XXX(II)
              X(I)=CFTYPE(I)
          END DO
C     NOW COMPUTE THE TILTED PART AND SUBTRACT IT OFF
          DO IIP=1,ITOT-1
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IIP
              CALL SPOTIT(4)
              IF(DSPOT(16).EQ.1.0D0)  WV=SYSTEM1(1)
              IF(DSPOT(16).EQ.2.0D0)  WV=SYSTEM1(2)
              IF(DSPOT(16).EQ.3.0D0)  WV=SYSTEM1(3)
              IF(DSPOT(16).EQ.4.0D0)  WV=SYSTEM1(4)
              IF(DSPOT(16).EQ.5.0D0)  WV=SYSTEM1(5)
              IF(DSPOT(16).EQ.6.0D0)  WV=SYSTEM1(71)
              IF(DSPOT(16).EQ.7.0D0)  WV=SYSTEM1(72)
              IF(DSPOT(16).EQ.8.0D0)  WV=SYSTEM1(73)
              IF(DSPOT(16).EQ.9.0D0)  WV=SYSTEM1(74)
              IF(DSPOT(16).EQ.10.0D0) WV=SYSTEM1(75)
              IF(SYSTEM1(6).EQ.1.0D0) THEN
                  WV=WV*3.93700787402D-5
              END IF
              IF(SYSTEM1(6).EQ.2.0D0) THEN
                  WV=WV*1.0D-4
              END IF
              IF(SYSTEM1(6).EQ.3.0D0) THEN
                  WV=WV*1.0D-3
              END IF
              IF(SYSTEM1(6).EQ.4.0D0) THEN
                  WV=WV*1.0D-6
              END IF
              DWW1=DSPOT(6)/REFHT
              DWW2=DSPOT(5)/REFHT
              DWW3=((DSPOT(33)/(TWOPII)))*WV
              RHO=DSQRT((DWW1**2)+(DWW2**2))
              IF(DABS(DWW1).GE.DABS(((1.0D35)*DWW2))) THEN
                  IF(DWW1.GE.0.0D0) THETA=PII/2.0D0
                  IF(DWW1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(DWW1).LE.1.0D-15.AND.DABS(DWW2).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(DWW1,DWW2)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              TERM=0.0D0
              IF(FTYPE.EQ.1) THEN
                  TERM1=(X(2)*FF3(RHO,THETA,2))
     1            +(X(3)*FF3(RHO,THETA,3))
                  TERM2=0.0D0
              END IF
              IF(FTYPE.EQ.2) THEN
                  TERM1=(X(2)*FF3(RHO,THETA,2))
     1            +(X(3)*FF3(RHO,THETA,3))
                  TERM2=(X(1)*FF3(RHO,THETA,1))
     1            +(X(4)*FF3(RHO,THETA,4))
              END IF
              TERM3=TERM1+TERM2
              DWW3=DWW3-TERM3
              IF(DSPOT(12).NE.0.0D0) DSPOT(4)=
     1        (DWW3*(TWOPII))/WV
              IF(DSPOT(12).EQ.0.0D0) DSPOT(4)=0.0D0
C     LOAD DSPOTT(*,ID) WITH DSPOT
              ID=IIP
              CALL SPOTIT(3)
          END DO

          DLLX=((X(2)*FF3(1.0D0,0.0D0,2))
     1    -(X(2)*FF3(1.0D0,(PII),2)))
          DLLY=((X(3)*FF3(1.0D0,(PII/2.0D0),3))
     1    -(X(3)*FF3(1.0D0,(3.0D0*PII/2.0D0),3)))
          IF(FTYPE.EQ.1)  DLLZ=0.0D0
          IF(FTYPE.EQ.2)  DLLZ=((X(1)*FF3(1.0D0,0.0D0,1))
     1    +(X(4)*FF3(1.0D0,0.0D0,4)))
          VALX=EXDIAX
          VALY=EXDIAY
          DLLX=-((DLLX/(2.0D0*VALX))*RREF)*DCOS(REFRY(11,NEWIMG))
          DLLY=-((DLLY/(2.0D0*VALY))*RREF)*DCOS(REFRY(12,NEWIMG))
          DLLZX=
     1    (DLLZ*2.0D0)/((ALENS(WWVN,NEWIMG-1)*
     2    DSIN(DATAN(1.0D0/(2.0D0*RBFNX))))**2)
          DLLZY=
     1    (DLLZ*2.0D0)/((ALENS(WWVN,NEWIMG-1)*
     2    DSIN(DATAN(1.0D0/(2.0D0*RBFNY))))**2)
          DLLZ=(DLLZX+DLLZY)/2.0D0
C
          DEALLOCATE(U,W,V,XXX,B,STAT=ALLOERR)
          RETURN
      END
C SUB PLTCAPFN.FOR
      SUBROUTINE PLTCAPFN
C
          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE PLOT CAPFNOPD AND PLOT CAPFNAPD COMMANDS
C
!      INTEGER COLPAS
C
          INTEGER I,G,KKK,KVAL,KKV
C
          REAL REFHT
C
          REAL*8 WVAL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          LOGICAL PLOTIT(1:10)
          COMMON/ITPLOT/PLOTIT
C
C
          AUTOZSCALE=.TRUE.
          IF(STI.EQ.1) THEN
              OUTLYNE= '"PLOT CAPFN(OPD AND APD)" PLOTS THE EXISTING CAPFN'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE= '"PLOT CAPFN(OPD OR APD)" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'CAPFNOPD') THEN
              IF(S5.EQ.1) THEN
                  OUTLYNE= '"PLOT CAPFNOPD" TAKES NO NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE= 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'CAPFNOPD') THEN
              IF(DF3.EQ.1.AND.DF4.EQ.0.OR.DF3.EQ.0.AND.DF4.EQ.1) THEN
                  OUTLYNE=
     1            'FOR "PLOT CAPFNOPD":'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'EITHER USE NO EXPLICIT NUMERIC WORD #3 AND #4 (AUTO Z-SCALING)'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'OR USE BOTH NUMERIC WORD #3 (Z-MAX) AND NUMERIC WORD #4 (Z-MIN)'
                  CALL SHOWIT(1)
                  OUTLYNE= 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF3.EQ.0.AND.DF4.EQ.0) THEN
                  IF(W3.LT.W4) THEN
                  ELSE
                      OUTLYNE=
     1                'FOR "PLOT CAPFNOPD":'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'NUMERIC WORD #3 MUST BE LESS THAN NUMERIC WORK #4'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'WHEN BOTH ARE ISSUED DURING CAPFNOPD PLOTTING'
                      CALL SHOWIT(1)
                      OUTLYNE= 'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
          IF(WQ.EQ.'CAPFNAPD') THEN
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE= '"PLOT CAPFNAPD" TAKES NO NUMERIC WORD'
                  CALL SHOWIT(1)
                  OUTLYNE='#3, #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE= 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF

          IF(DF1.EQ.1) W1=1.0D0
          IF(DF1.EQ.1) S1=1
          IF(DF1.EQ.1) DF1=0
          IF(INT(W1).LT.1.OR.INT(W1).GT.10) THEN
              OUTLYNE=
     1        'NUMERIC WORD #1 MUST BE 1, 2, 3, 4, 5, 6, 7, 8, 9 OR 10'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     OPEN AND LOAD THE APPROPRIATE CAPFN DATA AND THEN PLOT IT
          IF(.NOT.CPFNEXT) THEN
              OUTLYNE=
     1        'NO CAPFN EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     DETERMINE THE REFERNCE APERTURE HEIGHT
C     OF THE REF SURFACE COORDINATES
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.DABS(ALENS(9,NEWREF))
     1    .LE.5.0D0) THEN
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  REFHT=SNGL(ALENS(10,NEWREF))
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=SNGL(ALENS(10,NEWREF))
                  ELSE
                      REFHT=SNGL(ALENS(11,NEWREF))
                  END IF
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=SNGL(ALENS(10,NEWREF))
                  ELSE
                      REFHT=SNGL(ALENS(11,NEWREF))
                  END IF
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=SNGL(ALENS(10,NEWREF))
                  ELSE
                      REFHT=SNGL(ALENS(11,NEWREF))
                  END IF
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  REFHT=SNGL(ALENS(10,NEWREF))
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              IF(PXTRAY(1,NEWREF).GE.PXTRAX(1,NEWREF)) THEN
                  REFHT=SNGL((PXTRAY(1,NEWREF)))
              ELSE
                  REFHT=SNGL((PXTRAX(1,NEWREF)))
              END IF
          END IF
C
          WVAL=(W1)
          KKV=(ITOT-1)/NUMCOL
C     KKV IS THE TOTAL NUMBER OF POINTS AT THE DESIRED COLOR
C     AND IT IS AN EXACT SQUARE
          PLOTIT(1)=.FALSE.
          PLOTIT(2)=.FALSE.
          PLOTIT(3)=.FALSE.
          PLOTIT(4)=.FALSE.
          PLOTIT(5)=.FALSE.
          PLOTIT(6)=.FALSE.
          PLOTIT(7)=.FALSE.
          PLOTIT(8)=.FALSE.
          PLOTIT(9)=.FALSE.
          PLOTIT(10)=.FALSE.
          DO I=1,ITOT-1
C     LOAD DSPOTT(*,ID) INTO DSPOT(*)
              ID=I
              CALL SPOTIT(4)
C
              IF(DSPOT(16).EQ.WVAL) THEN
                  PLOTIT(INT(WVAL))=.TRUE.
                  KVAL=I
                  GO TO 10
              END IF
C
          END DO
 10       CONTINUE
          G=ITOT+1
C

          KKK=NINT(SQRT(FLOAT(KKV)))
          IF(.NOT.PLOTIT(INT(W1))) THEN
              OUTLYNE= 'CAPFN DATA FOR THE WAVELENGTH VALUE (NUMERIC WORD 1)'
              CALL SHOWIT(1)
              OUTLYNE= 'DOES NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE= 'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WQ.EQ.'CAPFNOPD') CALL CAPPLOT(1,REFHT,WVAL,KKV,KKK)
          IF(WQ.EQ.'CAPFNAPD') CALL CAPPLOT(2,REFHT,WVAL,KKV,KKK)
          RETURN
      END


C SUB CAPPLOT.FOR

      SUBROUTINE CAPPLOT(IJ,REFHT,WVAL,KKV,KKK)
C     KVAL IS THE FIRST RECORD OF THE SPOT FILE TO READ
C     REFHT IS THE NORMALIZING AP HT.
C     WVAL IS THE DESIRED WAVELENGTH NUMBER
C     KKV IS THE TOTAL NUMBER OF POINTS TO BE READ IN FROM DSPOTT
C
          IMPLICIT NONE
C
          LOGICAL ITSFLAT
C
          INTEGER IJ,I,J,KKI2,KKK,IQ,KKV
          INTEGER ALLOERR,WVNUMB
C
          REAL*8 WVAL
          REAL REFHT,ZMAX2,ZMIN2,RI,RII
          REAL XYVALUE,XPLT,YPLT,F1PLT,F2PLT,DELGRID
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          DIMENSION F1PLT(:,:),F2PLT(:,:),XPLT(:),YPLT(:)
C
          ALLOCATABLE :: F1PLT,F2PLT,YPLT,XPLT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          KKI2=NINT(SQRT(FLOAT(KKV)))
          DEALLOCATE(XPLT,YPLT,F1PLT,F2PLT,STAT=ALLOERR)
          ALLOCATE(XPLT(KKK),YPLT(KKK),F1PLT(KKK,KKK),
     1    F2PLT(KKK,KKK),STAT=ALLOERR)
C
          ZMAX1=-1.0E10
          ZMIN1=1.0E10
          ZMAX2=1.0E0
          ZMIN2=0.0E0
          I=1
          J=0
          DO IQ=1,ITOT-1
C     LOAD DSPOTT(*,ID) INTO DSPOT(*)
              ID=IQ
              CALL SPOTIT(4)
              IF(DSPOT(16).NE.WVAL) THEN
                  GO TO 101
              END IF
              J=J+1
              IF(J.GT.KKK) THEN
                  J=1
                  I=I+1
              END IF
              F1PLT(J,I)=SNGL(DSPOT(4)/(TWOPII))
              F2PLT(J,I)=SNGL(DSPOT(12))
              WVNUMB=INT(DSPOT(16))
              IF(F2PLT(J,I).NE.0.0) THEN
                  IF(F1PLT(J,I).GT.ZMAX1) ZMAX1=(F1PLT(J,I))
                  IF(F1PLT(J,I).LE.ZMIN1) ZMIN1=(F1PLT(J,I))
              END IF
 101          CONTINUE
          END DO
          ITSFLAT=.FALSE.
          IF(ABS(ZMIN1-ZMAX1).LT.1.0E-6) THEN
              IF(ABS(ZMIN1).LE.1.0E-6) ZMIN1=0.0
              ZMAX1=ZMIN1+1.0
              ITSFLAT=.TRUE.
              GO TO 5
          END IF
C
  5       DO I=-10000,10000,1
              RI=FLOAT(I)/100.0
              RII=FLOAT(I+1)/100.0
              IF(ZMAX1.GT.RI.AND.ZMAX1.LE.RII) THEN
                  ZMAX1=RII
                  GO TO 10
              END IF
          END DO
 10       CONTINUE
          DO I=-10000,10000,1
              RI=FLOAT(I)/100.0
              RII=FLOAT(I+1)/100.0
              IF(ZMIN1.GE.RI.AND.ZMIN1.LT.RII) THEN
                  ZMIN1=RI
                  GO TO 20
              END IF
          END DO
 20       CONTINUE
          IF(ZMIN1.EQ.ZMAX1) THEN
              ZMAX1=ZMIN1+0.01
              DO J=1,KKI2
                  DO I=1,KKI2
                      IF(F1PLT(I,J).LT.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1                F1PLT(I,J)=-0.0001
                      IF(F1PLT(I,J).GE.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1                F1PLT(I,J)= 0.0001
                  END DO
              END DO
          END IF
          CONTINUE
          DO J=1,KKI2
              DO I=1,KKI2
                  IF(F1PLT(I,J).LT.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1            F1PLT(I,J)=-0.0001
                  IF(F1PLT(I,J).GE.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1            F1PLT(I,J)= 0.0001
              END DO
          END DO

          DELGRID=2.0/(FLOAT(KKI2)-1.0)
          XPLT(1)=-1.0
          YPLT(1)=-1.0
          XPLT(KKI2)=1.0
          YPLT(KKI2)=1.0
          XYVALUE=-1.0
          DO IQ=2,KKI2-1
              XYVALUE=XYVALUE+DELGRID
              XPLT(IQ)=XYVALUE
              YPLT(IQ)=XYVALUE
          END DO
          DO IQ=1,KKI2
              XPLT(IQ)=XPLT(IQ)*.99
              YPLT(IQ)=YPLT(IQ)*.99
          END DO
          IF(DF3.EQ.0.AND.DF4.EQ.0) THEN
              ZMAX1=SNGL(W4)
              ZMIN1=SNGL(W3)
          END IF
          DO J=1,KKI2
              DO I=1,KKI2
                  F1PLT(I,J)=(F1PLT(I,J)-ZMIN1)/(ZMAX1-ZMIN1)
                  IF(ABS(F1PLT(I,J)).LT.1E-6) F1PLT(I,J)=0.0
                  IF(ABS(F2PLT(I,J)).LT.1E-6) F2PLT(I,J)=0.0
              END DO
          END DO
          IF(ITSFLAT) THEN
              J=KKI2
              I=KKI2
              F1PLT(1:I,1:J)=0.01
          END IF

          CALL FFT3(KKI2,XPLT,YPLT,F1PLT,F2PLT,REFHT,IJ,WVNUMB)
          DEALLOCATE(XPLT,YPLT,F1PLT,F2PLT,STAT=ALLOERR)
          RETURN
      END



      SUBROUTINE PLOTCAPFN(PCOUNT,XPLT,YPLT,F1PLT,F2PLT
     1,ROT,IJ,REFHT,WVNUMB)

          IMPLICIT NONE
C     F IS THE FUNTION FPLT(X,Y),XPLT AND YPLT ARE THE POINT COORDINATES
C     PCOUNT=NUMBER OF POINTS IN THE CAPFN FILE
C
          CHARACTER B*80,UNN*9,BLNOTE*80,BL20*20,NNTT1*99,CRANGE*15
     1    ,UNN1*9,TMY*8,DTY*10,CCRANGE*11
C
          INTEGER PCOUNT,ROT,WVWT,NT1ANG,NT1SIZ,ALLOERR
     1    ,K,COLPAS,II,IIX,IIY,IIXM1,IIYM1,IPN,H,WVNUMB

          REAL*8 XTEST1,XTEST2,YTEST1,YTEST2,SLOPE
C
          REAL F1PLT,F2PLT,XPLT,YPLT,REFHT
C
          DIMENSION XPLT(PCOUNT),YPLT(PCOUNT),F1PLT(PCOUNT,PCOUNT),
     1    H(:),F2PLT(PCOUNT,PCOUNT)
C
          EXTERNAL WVWT
C
!      LOGICAL ERROR1
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          INTEGER I,J,IJ
          REAL*8 XV,YV,RANGE1
          REAL*8 XX,YY,XY2
          DIMENSION XX(:),YY(:),XY2(:)
          REAL F_JK,X_JK,Y_JK
          DIMENSION F_JK(:,:),X_JK(:),Y_JK(:)
          ALLOCATABLE :: XX,YY,F_JK,X_JK,Y_JK,XY2,H
          INCLUDE 'datmai.inc'
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          DEALLOCATE(XX,YY,F_JK,X_JK,Y_JK,XY2,H,STAT=ALLOERR)
          ALLOCATE(XX(PCOUNT),YY(PCOUNT),XY2(PCOUNT),
     1    F_JK(PCOUNT,PCOUNT),X_JK(PCOUNT),Y_JK(PCOUNT),H(2740:7260),
     2    STAT=ALLOERR)
          DO I=2740,7260
              H(I)=-10000
          END DO
          DO I=1,PCOUNT
              X_JK(I)=XPLT(I)
              Y_JK(I)=YPLT(I)
              XX(I)=0.0
              YY(I)=0.0
              DO J=1,PCOUNT
                  IF(IJ.EQ.1) F_JK(I,J)=F1PLT(I,J)
                  IF(IJ.EQ.2) F_JK(I,J)=F2PLT(I,J)
              END DO
          END DO
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
C
C     GENERATE GRAPHIC
C     DO A PLOT NEW
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
C     GENERATE GRAPHIC
          CALL PLTDEV
          GRASET=.TRUE.
          PLEXIS=.TRUE.
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
C
C     PLOT THE FUNCTION RIGHT HERE
C     STARTING AT -Y, PLOT A ROW FROM -X TO X
C     SET THE STARTING POINT
          IIX=INT(2750.0+XPLT(1))
          IF(IJ.EQ.1) IIY=INT(2000.0+F1PLT(1,1))
          IF(IJ.EQ.2) IIY=INT(2500.0+F2PLT(1,1))
          CALL MY_PLOT(IIX,IIY,0,0,-10,10010,-10,7010)
          IPN=1

          DO J=1,PCOUNT
C     LOAD THE XX AND YY ARRAYS FOR A SPECIFIC J VALUE
              DO K=1,PCOUNT
                  XX(K)=XPLT(K)
                  IF(IJ.EQ.1) YY(K)=F1PLT(K,J)
                  IF(IJ.EQ.2) YY(K)=F2PLT(K,J)
              END DO
C     NOW PLOTTING STEPS A INTEGER 10 AND THERE ARE GOING TO BE
C     100 POINTS PER LINE
              DO I=1,3000
                  XV=DBLE(I-1)
                  YV=0.0D0
                  DO II=1,PCOUNT-1
                      XTEST1=XX(II)
                      XTEST2=XX(II+1)
                      YTEST1=YY(II)
                      YTEST2=YY(II+1)
                      IF(XV.GE.XTEST1.AND.XV.LE.XTEST2) THEN
C     CALC A VALUE FOR YV AND RETURN
                          IF((XTEST2-XTEST1).NE.0.0D0) THEN
                              SLOPE=(YTEST2-YTEST1)/(XTEST2-XTEST1)
                              YV=(SLOPE*(XV-XTEST1))+YTEST1
                          ELSE
C     SLOPE IS VERTICAL, TAKE THE AVERAGE Y VALUE
                              YV=(YTEST2+YTEST1)/2.0D0
                          END IF
                          GO TO 2001
                      END IF
                  END DO
 2001             CONTINUE
                  IIXM1=IIX
                  IIYM1=IIY
                  IIX=INT((2750+XV)+(0.5*YPLT(J)))
                  IIY=INT((2000+YV)+(0.5*YPLT(J)))
C     IIX IS IN THE RANGE 2750 TO 7250
                  IPN=0
                  IF((IIY).GT.H(IIX)) THEN
                      IF(IPN.EQ.0.AND.I.NE.1.AND.IIYM1.LT.IIY.AND.H(IIX).GT.-10000
     1                .AND.IIYM1.LT.H(IIXM1))
     1                CALL MY_PLOT(IIX,H(IIX),0,0,-10,10010,-10,7010)
                      H(IIX)=(IIY)
                      IPN=1
                  ELSE
C     IIY LESS THAN OR EQUAL TO H(IIX)
                      IPN=0
                  END IF
                  IF(I.EQ.1) IPN=0
                  CALL MY_PLOT(IIX,IIY,IPN,0,-10,10010,-10,7010)

!      call drawdatasave(IIX,IIY,IPN,1)

                  IF(IPN.EQ.0.AND.I.EQ.1) IPN=1
              END DO
              IPN=0
          END DO
C
C
          IF(ROT.EQ.90) THEN
C     XAXIS
              CALL MY_PLOT(2550,1800,0,0,0,10000,0,7000)
              CALL MY_PLOT(2750,2000,1,0,0,10000,0,7000)
              CALL MY_PLOT(2750,2000,0,0,0,10000,0,7000)
C     XAXIS LABEL
              CALL MY_JUSTSTRING(2200,1910,'+X'
     1        ,0,2,3)
C     YAXIS
              CALL MY_PLOT(2750,2000,0,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2000,1,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2000,0,0,0,10000,0,7000)
C     YAXIS LABEL
              CALL MY_JUSTSTRING(6182,1910,'+Y'
     1        ,0,2,3)
          END IF
          IF(ROT.EQ.0) THEN
C     YAXIS
              CALL MY_PLOT(5750,2000,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,3700,1,0,0,10000,0,7000)
              CALL MY_PLOT(7450,3700,0,0,0,10000,0,7000)
C     YAXIS LABEL
              CALL MY_JUSTSTRING(7550,3800,'+Y',0,2,3)
C     XAXIS
              CALL MY_PLOT(2750,2000,0,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2000,1,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2000,0,0,0,10000,0,7000)
C     XAXIS LABEL
              CALL MY_JUSTSTRING(6182,1910,'+X',0,2,3)
          END IF
C     ZAXIS
          CALL MY_PLOT(7250,3500,0,0,0,10000,0,7000)
          CALL MY_PLOT(7250,5500,1,0,0,10000,0,7000)
          CALL MY_PLOT(7250,5500,0,0,0,10000,0,7000)
C
          IF(IJ.EQ.1) THEN
C     Z AXIS TIC MARKS, OPD
              CALL MY_PLOT(7250,3500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,3500,1,0,0,10000,0,7000)
C
              RANGE1=ZMIN1
              WRITE(B,102) RANGE1
              READ(B,202) CCRANGE
              NNTT1=CCRANGE//' WAVES'
              CALL MY_JUSTSTRING(7550,3460,NNTT1(1:20),0,1,3)
              CALL MY_PLOT(7250,3700,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,3700,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,3900,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,3900,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4100,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4100,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4300,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4300,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,4500,1,0,0,10000,0,7000)
              RANGE1=(ZMAX1+ZMIN1)/2.0D0
              WRITE(B,102) RANGE1
              READ(B,202) CCRANGE
              NNTT1=CCRANGE//' WAVES'
              CALL MY_JUSTSTRING(7550,4460,NNTT1(1:20),0,1,3)
              CALL MY_PLOT(7250,4700,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4700,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4900,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4900,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5100,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,5100,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5300,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,5300,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,5500,1,0,0,10000,0,7000)
              RANGE1=ZMAX1
              WRITE(B,102) RANGE1
              READ(B,202) CCRANGE
              NNTT1=CCRANGE//' WAVES'
              CALL MY_JUSTSTRING(7550,5460,NNTT1(1:20),0,1,3)
          END IF
          IF(IJ.EQ.2) THEN
C     Z AXIS TIC MARKS, INTENSITY
              CALL MY_PLOT(7250,3500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,3500,1,0,0,10000,0,7000)
              CALL MY_JUSTSTRING(7550,3460,'0.0',0,1,3)
              CALL MY_PLOT(7250,3700,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,3700,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,3900,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,3900,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4100,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4100,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4300,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4300,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,4500,1,0,0,10000,0,7000)
              CALL MY_JUSTSTRING(7550,4460,'0.5',0,1,3)
              CALL MY_PLOT(7250,4700,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4700,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4900,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4900,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5100,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,5100,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5300,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,5300,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,5500,1,0,0,10000,0,7000)
              CALL MY_JUSTSTRING(7550,5460,'1.0',0,1,3)
          END IF
C     Z AXIS LABLE
          IF(IJ.EQ.1)
     1    CALL MY_JUSTSTRING(6500,5750,' (Wavefront Error)',0,1,3)
          IF(IJ.EQ.2)
     1    CALL MY_JUSTSTRING(6500,5750,'(Relative Intensity)',0,1,3)
C
          CALL PLOTBOX
C
C     NOW FOR PLOT ANNOTATION
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
          IF(STMPT) CALL MYTIME(TMY)
          IF(STMPD) CALL MYDATE(DTY)
          IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
          IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
          IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
          IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_JUSTSTRING(200,650,NNTT1(1:80),NT1ANG,NT1SIZ,3)
          ELSE
C     LI BLANK, NOT ACTION
          END IF
          IF(IJ.EQ.1) THEN
              IF(ROT.EQ.0) CALL MY_JUSTSTRING(5200,300,
     1        'Complex Aperture Function Wavefront Phase',
     1         0,2,3)
              IF(ROT.EQ.90) CALL MY_JUSTSTRING(5200,300,
     1        'Complex Aperture Function Wavefront Phase-ROTATED',
     1         0,2,3)
          END IF
          IF(IJ.EQ.2) THEN
              IF(ROT.EQ.0) CALL MY_JUSTSTRING(5200,300,
     1        'Complex Aperture Function Wavefront Intensity',
     1         0,2,3)
              IF(ROT.EQ.90) CALL MY_JUSTSTRING(5200,300,
     1        'Complex Aperture Function Wavefront Intensity-ROTATED',
     1         0,2,3)
          END IF
C
C     DO THE PLOTTING OF THE EXTENT
          IF(SYSTEM1(6).EQ.1.0D0) UNN='in(s)    '
          IF(SYSTEM1(6).EQ.2.0D0) UNN='cm(s)    '
          IF(SYSTEM1(6).EQ.3.0D0) UNN='mm(s)    '
          IF(SYSTEM1(6).EQ.4.0D0) UNN='meter(s) '
C     UNITS ARE NOW SET
C
C     NOW WRITE = "VALUE" UNN
          RANGE1=DBLE(2.0*REFHT)
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='REFERENCE APERTURE WIDTH = '//CRANGE//' '//UNN
C
          CALL MY_JUSTSTRING(200,1200,NNTT1(1:53),NT1ANG,NT1SIZ,3)
C
          IF(IJ.EQ.1) THEN
C     NOW WRITE = "VALUE" UNN
              RANGE1=PTOVOPD(WVNUMB)
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='P-V OPD = '//CRANGE//' WAVES'
C
              CALL MY_JUSTSTRING(200,5900,NNTT1(1:60),NT1ANG,NT1SIZ,3)
C
C     NOW WRITE = "VALUE" UNN
              RANGE1=RMSOP(WVNUMB)
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='RMS OPD = '//CRANGE//' WAVES'
C
              CALL MY_JUSTSTRING(200,5700,NNTT1(1:45),NT1ANG,NT1SIZ,3)
          END IF
C
          IF(WVNUMB.GE.1.AND.WVNUMB.LE.5) RANGE1=SYSTEM1(WVNUMB)
          IF(WVNUMB.GE.6.AND.WVNUMB.LE.10) RANGE1=SYSTEM1(WVNUMB+65)
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='WAVELENGTH = '//CRANGE//' MICROMETER'
C
          CALL MY_JUSTSTRING(200,5500,NNTT1(1:40),NT1ANG,NT1SIZ,3)
C
C
C     NOW WRITE = "VALUE" UNN
          IF(WVNUMB.EQ.1) RANGE1=SYSTEM1(1)
          IF(WVNUMB.EQ.2) RANGE1=SYSTEM1(2)
          IF(WVNUMB.EQ.3) RANGE1=SYSTEM1(3)
          IF(WVNUMB.EQ.4) RANGE1=SYSTEM1(4)
          IF(WVNUMB.EQ.5) RANGE1=SYSTEM1(5)
          IF(WVNUMB.EQ.6) RANGE1=SYSTEM1(71)
          IF(WVNUMB.EQ.7) RANGE1=SYSTEM1(72)
          IF(WVNUMB.EQ.8) RANGE1=SYSTEM1(73)
          IF(WVNUMB.EQ.9) RANGE1=SYSTEM1(74)
          IF(WVNUMB.EQ.10) RANGE1=SYSTEM1(75)
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='REFERENCE WAVELENGTH = '//CRANGE//' MICROMETER'
C
          CALL MY_JUSTSTRING(200,1600,NNTT1(1:48),NT1ANG,NT1SIZ,3)
C
          IF(.NOT.SUMMOR) THEN
              IF(IJ.EQ.1) THEN
C     DO REF SPHERE SHIFTS IF NOT ZERO
                  IF(SYSTEM1(6).EQ.1.0D0) UNN='in(s)    '
                  IF(SYSTEM1(6).EQ.2.0D0) UNN='cm(s)    '
                  IF(SYSTEM1(6).EQ.3.0D0) UNN='mm(s)    '
                  IF(SYSTEM1(6).EQ.4.0D0) UNN='meter(s) '
                  IF(DABS(DLLX).GT.1.0D-14) THEN
                      WRITE(B,101) DLLX
                      READ(B,200) CRANGE
                      CALL MY_JUSTSTRING(5200,1600,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  ELSE
                      NNTT1='REF. SPHERE CENTER NOT SHIFTED IN X'
                      CALL MY_JUSTSTRING(5200,1600,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  END IF
                  IF(DABS(DLLY).GT.1.0D-14) THEN
                      WRITE(B,101) DLLY
                      READ(B,200) CRANGE
                      NNTT1='REF. SPHERE Y-SHIFT = '//CRANGE//' '//UNN
                      CALL MY_JUSTSTRING(5200,1400,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  ELSE
                      NNTT1='REF. SPHERE CENTER NOT SHIFTED IN Y'
                      CALL MY_JUSTSTRING(5200,1400,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  END IF
                  IF(DABS(DLLZ).GT.1.0D-14) THEN
                      WRITE(B,101) DLLZ
                      READ(B,200) CRANGE
                      NNTT1='REF. SPHERE Z-SHIFT = '//CRANGE//' '//UNN
                      CALL MY_JUSTSTRING(5200,1200,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  ELSE
                      NNTT1='REF. SPHERE CENTER NOT SHIFTED IN Z'
                      CALL MY_JUSTSTRING(5200,1200,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  END IF
              END IF
C
C     NOT SUMMED CAPFN, DO FOV STUFF
C     FIELD OF VIEW DATA
C
              IF(SYSTEM1(19).EQ.1.0D0) THEN
C     SCX FANG
                  RANGE1=SYSTEM1(23)*LFOB(2)
                  UNN1='DEGREE(S)'
              ELSE
                  RANGE1=SYSTEM1(16)*LFOB(2)
                  IF(SYSTEM1(6).EQ.1.0D0) UNN1='in(s)    '
                  IF(SYSTEM1(6).EQ.2.0D0) UNN1='cm(s)    '
                  IF(SYSTEM1(6).EQ.3.0D0) UNN1='mm(s)    '
                  IF(SYSTEM1(6).EQ.4.0D0) UNN1='meter(s) '
C     SCX
              END IF
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='OBJECT POINT-X'
C
              CALL MY_JUSTSTRING(200,6500,NNTT1(1:14),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN1
C
              CALL MY_JUSTSTRING(1500,6500,NNTT1(1:28),NT1ANG,NT1SIZ,3)
C
C
              IF(SYSTEM1(18).EQ.1.0D0) THEN
C     SCY FANG
                  RANGE1=SYSTEM1(21)*LFOB(1)
                  UNN1='DEGREE(S)'
              ELSE
                  RANGE1=SYSTEM1(14)*LFOB(1)
                  IF(SYSTEM1(6).EQ.1.0D0) UNN1='in(s)    '
                  IF(SYSTEM1(6).EQ.2.0D0) UNN1='cm(s)    '
                  IF(SYSTEM1(6).EQ.3.0D0) UNN1='mm(s)    '
                  IF(SYSTEM1(6).EQ.4.0D0) UNN1='meter(s) '
C     SCY
              END IF
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='OBJECT POINT-Y'
C
              CALL MY_JUSTSTRING(200,6300,NNTT1(1:14),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN1
C
              CALL MY_JUSTSTRING(1500,6300,NNTT1(1:28),NT1ANG,NT1SIZ,3)
          END IF
C
C
          call setonecolors

          IF(DF2.EQ.1) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='DRAW'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
101       FORMAT(1PG15.8)
102       FORMAT(1F11.4)
200       FORMAT(A15)
202       FORMAT(A11)
          DEALLOCATE(XX,YY,X_JK,Y_JK,F_JK,XY2,H,STAT=ALLOERR)
          RETURN
      END


      SUBROUTINE FFT3(PCOUNT,XPLT,YPLT,F1PLT,F2PLT,REFHT,IJ,WVNUMB)

          IMPLICIT NONE
!      REAL SPACER
          INTEGER ROT,I,J,ALLOERR,PCOUNT,IJ,WVNUMB
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          REAL F1PLT,F2PLT,XPLT,YPLT,FTF1,FTF2,REFHT
C
          DIMENSION FTF1(:,:),F1PLT(PCOUNT,PCOUNT),XPLT(PCOUNT),YPLT(PCOUNT)
     1    ,F2PLT(PCOUNT,PCOUNT),FTF2(:,:)
          ALLOCATABLE :: FTF1,FTF2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'dathgr.inc'
          DEALLOCATE(FTF1,FTF2,STAT=ALLOERR)
          ALLOCATE(FTF1(PCOUNT,PCOUNT),FTF2(PCOUNT,PCOUNT),STAT=ALLOERR)
C     FIX THE ARRAY SO IT PLOTS LIKE IN CODE-V
C     WE NEED TO REFLECT IN THE Y-DIRECTION
          ROT=0
          IF(ROTCAPFN) ROT=90
C
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  FTF1(I,J)=F1PLT(PCOUNT+1-I,J)
                  FTF2(I,J)=F2PLT(PCOUNT+1-I,J)
              END DO
          END DO
          DO J=1,PCOUNT
              DO I=1,PCOUNT
                  F1PLT(I,J)=FTF1(I,J)
                  F2PLT(I,J)=FTF2(I,J)
              END DO
          END DO
C     WE NEED TO REFLECT IN THE X-DIRECTION
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  FTF1(I,J)=F1PLT(PCOUNT+1-I,J)
                  FTF2(I,J)=F2PLT(PCOUNT+1-I,J)
              END DO
          END DO
          DO J=1,PCOUNT
              DO I=1,PCOUNT
                  F1PLT(I,J)=FTF1(I,J)
                  F2PLT(I,J)=FTF2(I,J)
              END DO
          END DO
          IF(ROT.EQ.90) THEN
C     ROTATE THE F ARRAY BY 90 DEGREES (CLOCKWISE LOOKING DOWN
C     FROM THE PLUS INTENSITY Z AXIS TOWARD THE MINUS INTENSITY
C     Z AXIS. STORE THE TEMPORARY FUNCTION IN ARRAY FTF
              DO J=1,PCOUNT
                  DO I=1,PCOUNT
                      FTF1(I,J)=F1PLT(PCOUNT+1-J,I)
                      FTF2(I,J)=F2PLT(PCOUNT+1-J,I)
                  END DO
              END DO
              DO J=1,PCOUNT
                  DO I=1,PCOUNT
                      F1PLT(I,J)=FTF1(I,J)
                      F2PLT(I,J)=FTF2(I,J)
                  END DO
              END DO
          END IF
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  F1PLT(I,J)=(F1PLT(I,J))
                  IF(F2PLT(I,J).EQ.0.0) THEN
                      F1PLT(I,J)=0.0
                  END IF
              END DO
          END DO
          DO I=1,PCOUNT
              XPLT(I)=((XPLT(I))*1500.0)+1500.0
              YPLT(I)=((YPLT(I))*1500.0)+1500.0
          END DO
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  IF(F2PLT(I,J).EQ.0.0D0) F1PLT(I,J)=0.0
                  F1PLT(I,J)=(F1PLT(I,J))*2000.0
                  F2PLT(I,J)=(F2PLT(I,J))*2000.0
              END DO
          END DO
C
C     NOW FUNCTION GOES FROM 0 TO 2000 AND XPLT AND YPLT GO FROM
C     0 TO +2000 EACH
C
C     THE PLOT WILL BE SEEN IN ORTHOGRAPHIC PROJECTION, XPLT ACROSS THE SCREEN,
C     YPLT INTO THE SCREEN AT 45 DEG EL AND AZ AND FPLT UP ON THE SCREEN
C

          CALL PLOTCAPFN(PCOUNT,XPLT,YPLT,F1PLT,F2PLT,ROT,IJ,REFHT,WVNUMB)
C
          DEALLOCATE(FTF1,FTF2,STAT=ALLOERR)
          RETURN
      END


C SUB PLTAX.FOR
      SUBROUTINE PLTAX
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES PLOT AXIS AND PLOT NOAXIS AT THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       PLOT AXIS
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'AXIS') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"PLOT AXIS" TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              PLTAXS=.TRUE.
              CALL DOAX
              IF(STI.EQ.1) THEN
                  IF(PLTAXS) OUTLYNE= 'AXIS PLOTTING IS CURRENTLY ENABLED'
                  IF(.NOT.PLTAXS)
     1            OUTLYNE= 'AXIS PLOTTING IS NOT CURRENTLY ENABLED'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              RETURN
          END IF
          RETURN
      END


C SUB PLTACC.FOR
      SUBROUTINE PLTACC
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE "PLOT ACC" COMMAND AT THE CMD LEVEL
C       THE SIZE AND ANGLE ARE RESET USING "PLOT CHNOTE"
C
          INTEGER COLPAS,DECIM,IIX,IIY,IB
C
          LOGICAL INTEG
C
          CHARACTER CNUMBR*40
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
C       CHECK SYNTAX
          IF(S5.EQ.1) THEN
              OUTLYNE='"PLOT ACC" TAKES NO NUMERIC WORD #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(W3.LT.0.0D0.OR.W3.GT.10.0D0) THEN
              OUTLYNE=
     1         'VALID NUMERIC WORD #3 VALUES ARE: 1,2,3,4,5,6,7,8,9 OR 10'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE='"PLOT ACC" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C       STI
          IF(STI.EQ.1) THEN
C
              WRITE(OUTLYNE,15) REG(9)
              CALL SHOWIT(0)
 15           FORMAT('THE ACCUMULATOR VALUE TO BE PLOTTED IS = ',D23.15)
              RETURN
          ELSE
C       NOT ?, DO THE PLOTTING
          END IF
C       DEFAULT VALUE DETERMINATIONS
C       IF W1 IS DEFAULT, USE THE CURRENT CURRENT X PEN POS
          IF(DF1.EQ.1) W1=DBLE(PENPOSX)
C       IF W2 IS DEFAULT, USE THE CURRENT CURRENT Y PEN POS
          IF(DF2.EQ.1) W2=DBLE(PENPOSY)
C       IF W3 IS DEFAULT, W3=3.0D0
          IF(DF3.EQ.1) W3=3.0D0
C       IF W4 IS DEFAULT, SET INTEG FALSE
          IF(DF4.EQ.1) INTEG=.FALSE.
C
          DECIM=INT(W3)
          IF(W4.NE.0.0D0) INTEG=.TRUE.
          IF(W4.EQ.0.0D0) INTEG=.FALSE.
          CALL HMAT(REG(9),DECIM,CNUMBR,INTEG,IB)
C
C       HERE IS WHERE THE ACCUMULATOR CONTENTS ARE PLOTTED
C
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          IIX=INT(W1)
          IIY=INT(W2)

          CALL MY_JUSTSTRING(IIX,IIY,CNUMBR(1:IB),NTANG,NTSIZ,3)

C
          RETURN
      END


C SUB PLTABL.FOR
      SUBROUTINE PLTABL
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO ENTER THE SPECT LEVEL
C       COMMAND "PLOT" AT THE SPECTR LEVEL
C
          CHARACTER TNAME*8,LABX*40,LABY*40
C
          INTEGER CTAB,I,NT1SIZ,NT1ANG,FEND,COLPAS,IX(1:1001),IY(1:1001)
          INTEGER NX,NY
C
          !       REAL OXMIN,OXMAX,OYMIN,OYMAX
C
          LOGICAL TORR
C
          COMMON/TTOORR/TORR
C
          REAL X(1:1001),Y(1:1001)
C
          REAL LAMB1,LAMB2,TABLE(1:1001,1:3),UFUNC,LFUNC,HOLD,LLIM
     +    ,ULIM,DELF1,DELX1,RDELA,RDELB,RDELC
C
          COMMON/TABL/TABLE,CTAB
C
          COMMON/CURNAM/TNAME
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          LABX='WAVELENGTH (MICROMETER)'
          NX=20
          IF(TORR)      LABY='TRANSMITTANCE'
          IF(TORR)      NY=13
          IF(.NOT.TORR) LABY='REFLECTANCE'
          IF(.NOT.TORR) NY=11

C
C     GENERATE GRAPHIC
C     DO A PLOT NEW
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
          CALL PLTDEV
          GRASET=.TRUE.
          PLEXIS=.TRUE.
C     SET LETTER SIZE AND ANGLE
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
C       LOWER AND UPPER LIMITS OF WAVELENGTH ARE
          LAMB1=TABLE(1,2)
          LAMB2=TABLE(CTAB,2)
C       CTAB IS THE MAXIMUM NUMBER OF ENTRIES IN THE TABLE ARRAYS
C       PROCEED
C
          DO 10 I = 1, CTAB
              X(I)  = REAL(TABLE(I,2))
              Y(I)  = REAL(TABLE(I,3))
10        CONTINUE
C
C       NOW DETERMINE THE LOWER AND UPPER X LIMITS OF THE PLOT
C
C       LAMB1 IS THE SHORTEST WAVELENGTH, LLIM WILL BE THE NEXT
C       LOWEST INTEGER VALUE OF WAVELENGTH IN MICROMETER
          LLIM=REAL(LAMB1)
          IF(LLIM.LT.0) LLIM=0
C       AND THE UPPER LIMIT IS CONVERSELY:
          ULIM=REAL(LAMB2)
C       MAX FUNCTIONAL VALUE IS:
          UFUNC=TABLE(1,3)
          DO I=2,CTAB
              IF(TABLE(I,3).GE.UFUNC) UFUNC=TABLE(I,3)
          END DO
C       MIN FUNCTIONAL VALUE IS:
          LFUNC=TABLE(1,3)
          DO I=2,CTAB
              IF(TABLE(I,3).LE.LFUNC) LFUNC=TABLE(I,3)
          END DO
C       DEFAULT OVERRIDES DONE HERE
C
          IF(DF2.EQ.0) LLIM=REAL(W2)
          IF(DF3.EQ.0) ULIM=REAL(W3)
          IF(DF4.EQ.0) LFUNC=REAL(W4)
          IF(DF5.EQ.0) UFUNC=REAL(W5)
          IF(LLIM.GE.ULIM) THEN
C
              HOLD=ULIM
              ULIM=LLIM
              LLIM=HOLD
          ELSE
          END IF
C
          IF(LFUNC.GE.UFUNC) THEN
              HOLD=UFUNC
              UFUNC=LFUNC
              LFUNC=HOLD
          ELSE
          END IF
          IF(LLIM.EQ.ULIM) THEN
              RETURN
          END IF
C
C
C       CORRECT SCALING IS IMPORTANT
          DO I=1,CTAB
              IX(I)=INT((((X(I)-LLIM)/(ULIM-LLIM))*6000.0)+2000.0)
              IY(I)=INT((((Y(I)-LFUNC)/(UFUNC-LFUNC))*4200.0)+1400.0)
          END DO
C
          DELF1=(UFUNC-LFUNC)/10.0
          DELX1=(ULIM-LLIM)/5.0
          IF(ULIM.EQ.LLIM) THEN
              OUTLYNE='UPPER AND LOWER WAVELENGTH VALUES ARE EQUAL'
              CALL SHOWIT(1)
              OUTLYNE='NO PLOT CAN BE GENERATED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C     SET LETTER SIZE AND ANGLE
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
C     LIFT PEN, MOVE TO FRAME START
C     PLOT THE BIG BOX
          CALL PLOTBOX
C       LOWER AND UPPER LIMITS OF WAVELENGTH ARE
          LAMB1=TABLE(1,2)
          LAMB2=TABLE(CTAB,2)
C       CTAB IS THE MAXIMUM NUMBER OF ENTRIES IN THE TABLE ARRAYS
C       PROCEED
C
C     PLOT THE AXES AND TICS
          CALL PLOTAXES3
C     PLOT THE HORIZONTAL AXIS NAME
          CALL PLOTHNAME(LABX,NX)
C     PLOT THE VERTICAL AXIS NAME
          CALL PLOTVNAME(LABY,NY)

C     PLOT THE VALUES FOR THE TIC MARKS
          CALL PLOTVVAL(LFUNC,DELF1)
          CALL PLOTHVAL(LLIM,DELX1)

C
          RDELA=ULIM-LLIM
          RDELC=UFUNC-LFUNC
          IF(RDELC.EQ.0.0) RDELB=0.1
          IF(RDELC.NE.0.0) RDELB=RDELC/10.0
C
          COLPAS=COLSPE
          CALL MY_COLTYP(COLPAS)
          FEND=CTAB
          call setonecolors
          CALL PLOTFUNC2(X,Y,FEND,LLIM,ULIM,LFUNC,UFUNC,150)
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='DRAW'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          RETURN
      END


C SUB PLOTSPD
      SUBROUTINE PLOTSPD
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS PLOTS THE SPOT DIAGRAM
C
          CHARACTER B*80,UNN*9,BLNOTE*80,BL20*20,NNTT1*99,CRANGE*15
     1    ,UNN1*9,TMY*8,DTY*10
C
          INTEGER SPREC,NT1ANG,NT1SIZ,I,I3
C
          REAL*8 XCENTER,YCENTER,A,X10,X20,X30,X40
     1    ,Y10,Y20,Y30,Y40,X11,X21,X31,X41,Y11,Y21,Y31,Y41,RANGE1
     2    ,PLANG,CENTXX,CENTYY,PII2
C
          INTEGER COLPAS,XPOINT,YPOINT
C
          REAL*8 JA,JB,XUP,XLO,YUP,YLO,MAXX,MAXY
C
          COMMON/BIGGY/XUP,YUP,XLO,YLO,MAXX,MAXY
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'dathgr.inc'
C
          JA=COS_A_ANG
          JB=COS_B_ANG

          PII2=TWOPII
C
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
C     DO A PLOT NEW
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
          CALL PLTDEV
          GRASET=.TRUE.
          PLEXIS=.TRUE.
C     SET LETTER SIZE AND ANGLE
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
C
          IF(WQ.EQ.'SUM') THEN
              CENTXX=0.0D0
              CENTYY=0.0D0
          ELSE
              CENTXX=CENTX
              CENTYY=CENTY
          END IF
C
C
C
C     LIFT PEN, MOVE TO FRAME START
C
          CALL PLOTBOX
C
C     LIFT PEN, MOVE TO INNER WINDOW START
C
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     SPOT DIAGRAM HEADING
          IF(WQ.EQ.'SUM') THEN
              CALL MY_JUSTSTRING(3150,5600,'SUMMED SPOT DIAGRAM'
     1        ,0,2,3)
          ELSE
              CALL MY_JUSTSTRING(5000,5600,'SPOT DIAGRAM'
     1        ,0,2,3)
          END IF
C
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          CALL MY_PLOT(3000,1500,0,0,0,10000,0,7000)
C     DROP PEN, DRAW BOX
          CALL MY_PLOT(7000,1500,1,0,0,10000,0,7000)
          CALL MY_PLOT(7000,5500,1,0,0,10000,0,7000)
          CALL MY_PLOT(3000,5500,1,0,0,10000,0,7000)
          CALL MY_PLOT(3000,1500,1,0,0,10000,0,7000)
C
C     LIFT PEN, MOVE TO DRAW HORIZONTAL CENTER LINE
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          CALL MY_PLOT(3000,3500,0,0,0,10000,0,7000)
C     DROP PEN, DRAW HORIZONTAL LINE
          CALL MY_PLOT(7000,3500,1,0,0,10000,0,7000)
C     LIFT PEN, MOVE TO DRAW VERTICAL CENTER LINE
          CALL MY_PLOT(5000,5500,0,0,0,10000,0,7000)
C     DROP PEN, DRAW HORIZONTAL LINE
          CALL MY_PLOT(5000,1500,1,0,0,10000,0,7000)
C
C     NOW CALCULATE THE SPDSSIVAL IF NEED BE
          IF(.NOT.SPOTSSI) THEN
              SPOTSSI=.TRUE.
              IF(MAXX.GT.MAXY) THEN
                  IF(SYSTEM1(30).LE.2.0D0) THEN
                      SPDSSIVAL=(MAXX+(2.0D0*
     1                DABS((CENTXX-REFRY(1,NEWIMG))/JB)))/3.6D0
                  ELSE
                      IF(REFRY(11,NEWIMG).LE.PII)
     1                SPDSSIVAL=(MAXX+(2.0D0*DABS(CENTXX-REFRY(11,NEWIMG))))/3.6D0
                      IF(REFRY(11,NEWIMG).GT.PII)
     1                SPDSSIVAL=(MAXX+(2.0D0*DABS(CENTXX-(REFRY(11,NEWIMG)-PII2)
     2                )))/3.6D0
                  END IF
              ELSE
                  IF(SYSTEM1(30).LE.2.0D0) THEN
                      SPDSSIVAL=(MAXY+(2.0D0*
     1                DABS((CENTYY-REFRY(2,NEWIMG))/JA)))/3.6D0
                  ELSE
                      IF(REFRY(12,NEWIMG).LE.PII)
     1                SPDSSIVAL=(MAXY+(2.0D0*DABS(CENTYY-REFRY(12,NEWIMG))))/3.6D0
                      IF(REFRY(12,NEWIMG).GT.PII)
     1                SPDSSIVAL=(MAXY+(2.0D0*DABS(CENTYY-(REFRY(12,NEWIMG)-PII2)
     2                )))/3.6D0
                  END IF
              END IF
          ELSE
C     USE THE INPUT VALUE
          END IF
C
C     NOW PLOT THE DETECTOR IF NEEDED, FOCAL MODE ONLY
          IF(DETTYP.EQ.1.AND.DETER.AND.SYSTEM1(30).LE.2.0D0) THEN
C     CIRCULAR DETECTOR
              XCENTER=DETDELX
              YCENTER=DETDELY
              DO I=0,720
                  PLANG=360.0D0*(DBLE(I)/720.0D0)*PII/180.0D0
                  XPOINT=INT(((((DETDIAM/2.0D0)*DCOS(PLANG))+XCENTER)
     1            *1000.0D0)/SPDSSIVAL)+5000
                  YPOINT=INT(((((DETDIAM/2.0D0)*DSIN(PLANG))+YCENTER)
     1            *1000.0D0)/SPDSSIVAL)+3500
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  IF(I.EQ.0)
     1            CALL MY_PLOT(XPOINT,YPOINT,0,3000,7000,1500,5500)
                  IF(I.GT.0)
     1            CALL MY_PLOT(XPOINT,YPOINT,1,2,3000,7000,1500,5500)

              END DO
          END IF
          IF(DETTYP.EQ.2.AND.DETER.AND.SYSTEM1(30).LE.2.0D0) THEN
C     RECTANGULAR DETECTOR
              XCENTER=DETDELX
              YCENTER=DETDELY
C     BEFORE ROTATION THROUGH THETA
C     CORNER PONITS ARE: LEFT TOP
C     -(DETTX/2)+XCENTER, (DETTY/2)+CENTYY
              X10=-(DETTX/2.0D0)+XCENTER
              Y10= (DETTY/2.0D0)+YCENTER
C     CORNER PONITS ARE: RIGHT TOP
C     (DETTX/2)+XCENTER, (DETTY/2)+CENTYY
              X20= (DETTX/2.0D0)+XCENTER
              Y20= (DETTY/2.0D0)+YCENTER
C     CORNER PONITS ARE: LEFT BOTTOM
C     -(DETTX/2)+XCENTER, -(DETTY/2)+CENTYY
              X30=-(DETTX/2.0D0)+XCENTER
              Y30=-(DETTY/2.0D0)+YCENTER
C     CORNER PONITS ARE: RIGHT BOTTOM
C     (DETTX/2)+XCENTER, -(DETTY/2)+CENTYY
              X40= (DETTX/2.0D0)+XCENTER
              Y40=-(DETTY/2.0D0)+YCENTER
              A=DETTHETA*PII/180.0D0
C     AFTER ROTATION THE LEFT TOP POINT IS X11,Y11
              X11=(X10*DCOS(A))-(Y10*DSIN(A))
              Y11=(X10*DSIN(A))+(Y10*DCOS(A))
C     AFTER ROTATION THE RIGHT TOP POINT IS X21,Y21
              X21=(X20*DCOS(A))-(Y20*DSIN(A))
              Y21=(X20*DSIN(A))+(Y20*DCOS(A))
C     AFTER ROTATION THE LEFT BOTTOM POINT IS X31,Y31
              X31=(X30*DCOS(A))-(Y30*DSIN(A))
              Y31=(X30*DSIN(A))+(Y30*DCOS(A))
C     AFTER ROTATION THE RIGHT BOTTOM POINT IS X41,Y41
              X41=(X40*DCOS(A))-(Y40*DSIN(A))
              Y41=(X40*DSIN(A))+(Y40*DCOS(A))
C     GO TO STARTING POINT
              XPOINT=INT(X11*1000.0D0/SPDSSIVAL)+5000
              YPOINT=INT(Y11*1000.0D0/SPDSSIVAL)+3500
              COLPAS=COLFRM
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(XPOINT,YPOINT,0,2,3000,7000,1500,5500)
              XPOINT=INT(X21*1000.0D0/SPDSSIVAL)+5000
              YPOINT=INT(Y21*1000.0D0/SPDSSIVAL)+3500
              CALL MY_PLOT(XPOINT,YPOINT,1,2,3000,7000,1500,5500)
              XPOINT=INT(X41*1000.0D0/SPDSSIVAL)+5000
              YPOINT=INT(Y41*1000.0D0/SPDSSIVAL)+3500
              CALL MY_PLOT(XPOINT,YPOINT,1,2,3000,7000,1500,5500)
              XPOINT=INT(X31*1000.0D0/SPDSSIVAL)+5000
              YPOINT=INT(Y31*1000.0D0/SPDSSIVAL)+3500
              CALL MY_PLOT(XPOINT,YPOINT,1,2,3000,7000,1500,5500)
              XPOINT=INT(X11*1000.0D0/SPDSSIVAL)+5000
              YPOINT=INT(Y11*1000.0D0/SPDSSIVAL)+3500
              CALL MY_PLOT(XPOINT,YPOINT,1,2,3000,7000,1500,5500)
          END IF
C
C     NOW PLOT THE SPOT DATA
          IF(SPDCENT) THEN
              XCENTER=CENTXX
              YCENTER=CENTYY
          END IF
          IF(SPDCHIEF) THEN
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  XCENTER=REFRY(1,NEWIMG)
                  YCENTER=REFRY(2,NEWIMG)
              ELSE
                  IF(REFRY(11,NEWIMG).LE.PII) XCENTER=REFRY(11,NEWIMG)
                  IF(REFRY(12,NEWIMG).LE.PII) YCENTER=REFRY(12,NEWIMG)
                  IF(REFRY(11,NEWIMG).GT.PII) XCENTER=REFRY(11,NEWIMG)-PII2
                  IF(REFRY(12,NEWIMG).GT.PII) YCENTER=REFRY(12,NEWIMG)-PII2
              END IF
          END IF
          IF(WQ.EQ.'ZERO') THEN
              XCENTER=0.0D0
              YCENTER=0.0D0
          END IF
          IF(WQ.EQ.'SUM') THEN
              OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//'SPOTS.DAT',
     1        FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
              READ(UNIT=32,REC=1) ITOT
              SPREC=32
          END IF
          IF(WQ.EQ.'CENT'.OR.WQ.EQ.'CHIEF'.OR.WQ.EQ.'ZERO') THEN
              SPREC=61
          END IF

          DO I=2,ITOT

              IF(SPREC.EQ.61) THEN
C     LOAD DSPOT(*) WITH DSPOTT(*,ID) DATA
                  ID=I-1
                  CALL SPOTIT(4)
              END IF
              IF(SPREC.EQ.32)
     1        READ(UNIT=32,REC=I)
     1          DSPOT(1),DSPOT(2),DSPOT(3),DSPOT(4),
     1          DSPOT(5),DSPOT(6),DSPOT(7),DSPOT(8),
     1          DSPOT(9),DSPOT(10),DSPOT(11),DSPOT(12),
     1          DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16),
     1          DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16),
     1          DSPOT(17),DSPOT(18),DSPOT(19),DSPOT(20),
     1          DSPOT(21),DSPOT(22),DSPOT(23),DSPOT(24)
     6        ,DSPOT(25),DSPOT(26),DSPOT(27),DSPOT(28),DSPOT(29)
     6        ,DSPOT(30),DSPOT(31),DSPOT(32),DSPOT(33),DSPOT(34),DSPOT(35)
     6        ,DSPOT(36),DSPOT(37),DSPOT(38),DSPOT(39),DSPOT(40),DSPOT(41)
     6        ,DSPOT(42),DSPOT(43),DSPOT(44),DSPOT(45),DSPOT(46),DSPOT(47)
     6        ,DSPOT(48),DSPOT(49),DSPOT(50)
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  XPOINT=INT((DSPOT(1)-XCENTER)*1000.0D0/SPDSSIVAL)+5000
                  XPOINT=INT(XPOINT/JB)
                  YPOINT=INT((DSPOT(2)-YCENTER)*1000.0D0/SPDSSIVAL)+3500
                  YPOINT=INT(YPOINT/JA)
              ELSE
                  XPOINT=INT((DSPOT(9)-XCENTER)*1000.0D0/SPDSSIVAL)+5000
                  YPOINT=INT((DSPOT(10)-YCENTER)*1000.0D0/SPDSSIVAL)+3500

              END IF
C
C     SELECT A COLOR
              IF(INT(DSPOT(16)).EQ.1)  COLPAS=(COLR1)
              IF(INT(DSPOT(16)).EQ.2)  COLPAS=(COLR2)
              IF(INT(DSPOT(16)).EQ.3)  COLPAS=(COLR3)
              IF(INT(DSPOT(16)).EQ.4)  COLPAS=(COLR4)
              IF(INT(DSPOT(16)).EQ.5)  COLPAS=(COLR5)
              IF(INT(DSPOT(16)).EQ.6)  COLPAS=(COLR6)
              IF(INT(DSPOT(16)).EQ.7)  COLPAS=(COLR7)
              IF(INT(DSPOT(16)).EQ.8)  COLPAS=(COLR8)
              IF(INT(DSPOT(16)).EQ.9)  COLPAS=(COLR9)
              IF(INT(DSPOT(16)).EQ.10) COLPAS=(COLR10)
              CALL MY_COLTYP(COLPAS)

C     MOVE TO THE POINT
              IF(INT(DSPOT(7)).EQ.0.AND.DSPOT(12).NE.0.0D0) THEN
                  CALL MY_PLOTC(-1,-1,0,0,3000,7000,1500,5500)
                  CALL MY_PLOTC(XPOINT-10,YPOINT,1,0,3000,7000,1500,5500)
                  CALL MY_PLOTC(XPOINT+10,YPOINT,1,0,3000,7000,1500,5500)
                  CALL MY_PLOTC(-1,-1,0,0,3000,7000,1500,5500)
                  CALL MY_PLOTC(XPOINT,YPOINT-10,1,0,3000,7000,1500,5500)
                  CALL MY_PLOTC(XPOINT,YPOINT+10,1,0,3000,7000,1500,5500)
              END IF
C
          END DO
          IF(SPDCHIEF.AND.WQ.NE.'SUM'.AND.WQ.NE.'ZERO') THEN
C     PLOT CENT LOCATION
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  XPOINT=INT(((CENTXX-REFRY(1,NEWIMG))*1000.0D0)/SPDSSIVAL)+5000
                  YPOINT=INT(((CENTYY-REFRY(2,NEWIMG))*1000.0D0)/SPDSSIVAL)+3500
              ELSE
                  IF(REFRY(11,NEWIMG).LE.PII)
     1            XPOINT=INT(((CENTXX-REFRY(11,NEWIMG))*1000.0D0)/SPDSSIVAL)+5000
                  IF(REFRY(12,NEWIMG).LE.PII)
     1            YPOINT=INT(((CENTYY-REFRY(12,NEWIMG))*1000.0D0)/SPDSSIVAL)+3500
                  IF(REFRY(11,NEWIMG).GT.PII)
     1            XPOINT=INT(((CENTXX-(REFRY(11,NEWIMG)-PII2))
     2            *1000.0D0)/SPDSSIVAL)+5000
                  IF(REFRY(12,NEWIMG).GT.PII)
     1            YPOINT=INT(((CENTYY-(REFRY(12,NEWIMG)-PII2))
     2            *1000.0D0)/SPDSSIVAL)+3500
              END IF
              CALL MY_PLOT(3000,YPOINT,0,2,3000,7000,1500,5500)
              CALL MY_PLOT(7000,YPOINT,1,2,3000,7000,1500,5500)
              CALL MY_PLOT(XPOINT,1500,0,2,3000,7000,1500,5500)
              CALL MY_PLOT(XPOINT,5500,1,2,3000,7000,1500,5500)
          END IF
          IF(SPDCENT.AND.WQ.NE.'SUM'.AND.WQ.NE.'ZERO') THEN
C     PLOT CHIEF RAY LOCATION
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  XPOINT=INT(((REFRY(1,NEWIMG)-CENTXX)*1000.0D0)/SPDSSIVAL)+5000
                  YPOINT=INT(((REFRY(2,NEWIMG)-CENTYY)*1000.0D0)/SPDSSIVAL)+3500
              ELSE
                  IF(REFRY(11,NEWIMG).LE.PII)
     1            XPOINT=INT(((REFRY(11,NEWIMG)-CENTXX)*1000.0D0)/SPDSSIVAL)+5000
                  IF(REFRY(12,NEWIMG).LE.PII)
     2            YPOINT=INT(((REFRY(12,NEWIMG)-CENTYY)*1000.0D0)/SPDSSIVAL)+3500
                  IF(REFRY(11,NEWIMG).GT.PII)
     1            XPOINT=INT((((REFRY(11,NEWIMG)-PII2)-CENTXX)
     2            *1000.0D0)/SPDSSIVAL)+5000
                  IF(REFRY(12,NEWIMG).GT.PII)
     1            YPOINT=INT((((REFRY(12,NEWIMG)-PII2)-CENTYY)
     2            *1000.0D0)/SPDSSIVAL)+3500
              END IF
              CALL MY_PLOT(3000,YPOINT,0,2,3000,7000,1500,5500)
              CALL MY_PLOT(7000,YPOINT,1,2,3000,7000,1500,5500)
              CALL MY_PLOT(XPOINT,1500,0,2,3000,7000,1500,5500)
              CALL MY_PLOT(XPOINT,5500,1,2,3000,7000,1500,5500)
          END IF
          PLEXIS=.TRUE.
C     NOW FOR PLOT ANNOTATION
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
          NT1SIZ=1
          NT1ANG=0
          IF(STMPT) CALL MYTIME(TMY)
          IF(STMPD) CALL MYDATE(DTY)
          IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
          IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
          IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
          IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_JUSTSTRING(200,500,NNTT1(1:80),NT1ANG,NT1SIZ,3)
          ELSE
C     LI BLANK, NOT ACTION
          END IF
C     DO THE CENTERING MESSAGE AT 200,250
          NT1SIZ=1
          NT1ANG=0
          IF(SPDCENT.AND.WQ.NE.'SUM') THEN
              NNTT1='SPOT DIAGRAM PLOT CENTERED ON SPOT CENTROID'
     1        //',DASHED LINES MARK CHIEF RAY LOCATION'
          END IF
          IF(SPDCHIEF.AND.WQ.NE.'SUM'.AND.WQ.NE.'ZERO') THEN
              NNTT1='SPOT DIAGRAM PLOT CENTERED ON CHIEF RAY'
     1        //',DASHED LINES MARK SPOT CENTROID LOCATION'
          END IF
          IF(WQ.EQ.'ZERO') THEN
              NNTT1='SPOT DIAGRAM PLOT CENTERED AT X=0, Y=0'
          END IF
          IF(WQ.NE.'SUM') THEN
              CALL MY_JUSTSTRING(200,250,NNTT1(1:80),NT1ANG,NT1SIZ,3)
          END IF
C
C     DO THE PLOTTING OF THE SPOT PLOT SSI
          NT1SIZ=1
          NT1ANG=0
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C     FOCAL OR UFOCAL
              IF(SYSTEM1(6).EQ.1.0D0) UNN='in(s)    '
              IF(SYSTEM1(6).EQ.2.0D0) UNN='cm(s)    '
              IF(SYSTEM1(6).EQ.3.0D0) UNN='mm(s)    '
              IF(SYSTEM1(6).EQ.4.0D0) UNN='meter(s) '
          END IF
          IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
C     AFOCAL OR UAFOCAL
              UNN='RADIAN(S)'
          END IF
C     UNITS ARE NOW SET
C
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          CALL MY_PLOT(6000,850,0,0,0,10000,0,7000)
          CALL MY_PLOT(6000,750,1,0,0,10000,0,7000)
          CALL MY_PLOT(6000,800,0,0,0,10000,0,7000)
          CALL MY_PLOT(7000,800,1,0,0,10000,0,7000)
          CALL MY_PLOT(7000,850,0,0,0,10000,0,7000)
          CALL MY_PLOT(7000,750,1,0,0,10000,0,7000)
          CALL MY_PLOT(7050,800,0,0,0,10000,0,7000)
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     NOW WRITE = "VALUE" UNN
          RANGE1=SPDSSIVAL
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1=' = '//CRANGE//' '//UNN
C
          CALL MY_JUSTSTRING(7050,800,NNTT1(1:28),NT1ANG,NT1SIZ,3)
C
C
C
          IF(WQ.NE.'SUM') THEN
C     DO THE PLOTTING OF THE CENTROID POSITIONS
C     AT X=200, Y=6500
              NT1SIZ=1
              NT1ANG=0
              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C     FOCAL OR UFOCAL
                  IF(SYSTEM1(6).EQ.1.0D0) UNN='in(s)    '
                  IF(SYSTEM1(6).EQ.2.0D0) UNN='cm(s)    '
                  IF(SYSTEM1(6).EQ.3.0D0) UNN='mm(s)    '
                  IF(SYSTEM1(6).EQ.4.0D0) UNN='meter(s) '
              END IF
              IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
C     AFOCAL OR UAFOCAL
                  UNN='RADIAN(S)'
              END IF
C     UNITS ARE NOW SET
C
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
C     NOW WRITE = "VALUE" UNN
              RANGE1=CENTXX
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
101           FORMAT(G15.8)
200           FORMAT(A15)
              NNTT1='CENT X-POSITION'
C
              CALL MY_JUSTSTRING(200,6500,NNTT1(1:15),NT1ANG,NT1SIZ,3)

              NNTT1=' = '//CRANGE//' '//UNN
C
              CALL MY_JUSTSTRING(1600,6500,NNTT1(1:28),NT1ANG,NT1SIZ,3)
C
              RANGE1=CENTYY
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='CENT Y-POSITION'
C
              CALL MY_JUSTSTRING(200,6300,NNTT1(1:15),NT1ANG,NT1SIZ,3)

              NNTT1=' = '//CRANGE//' '//UNN
C
              CALL MY_JUSTSTRING(1600,6300,NNTT1(1:28),NT1ANG,NT1SIZ,3)
C
C
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  RANGE1=REFRY(1,NEWIMG)
              ELSE
                  IF(REFRY(11,NEWIMG).LE.PII)RANGE1=REFRY(11,NEWIMG)
                  IF(REFRY(11,NEWIMG).GT.PII)RANGE1=REFRY(11,NEWIMG)-PII2
              END IF
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='CHIEF RAY X-POS'
C
              CALL MY_JUSTSTRING(200,6100,NNTT1(1:15),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN
C
              CALL MY_JUSTSTRING(1600,6100,NNTT1(1:28),NT1ANG,NT1SIZ,3)
C
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  RANGE1=REFRY(2,NEWIMG)
              ELSE
                  IF(REFRY(12,NEWIMG).LE.PII)RANGE1=REFRY(12,NEWIMG)
                  IF(REFRY(12,NEWIMG).GT.PII)RANGE1=REFRY(12,NEWIMG)-PII2
              END IF
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='CHIEF RAY Y-POS'
C

              CALL MY_JUSTSTRING(200,5900,NNTT1(1:15),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN
C
              CALL MY_JUSTSTRING(1600,5900,NNTT1(1:28),NT1ANG,NT1SIZ,3)
C
C     FIELD OF VIEW DATA
C
              IF(SYSTEM1(19).EQ.1.0D0) THEN
C     SCX FANG
                  RANGE1=SYSTEM1(23)*LFOB(2)
                  UNN1='DEGREE(S)'
              ELSE
                  RANGE1=SYSTEM1(16)*LFOB(2)
                  IF(SYSTEM1(6).EQ.1.0D0) UNN1='in(s)    '
                  IF(SYSTEM1(6).EQ.2.0D0) UNN1='cm(s)    '
                  IF(SYSTEM1(6).EQ.3.0D0) UNN1='mm(s)    '
                  IF(SYSTEM1(6).EQ.4.0D0) UNN1='meter(s) '
C     SCX
              END IF
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='OBJECT POINT-X'
C
              CALL MY_JUSTSTRING(6000,6500,NNTT1(1:14),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN1
C
              CALL MY_JUSTSTRING(7300,6500,NNTT1(1:28),NT1ANG,NT1SIZ,3)
C
C
              IF(SYSTEM1(18).EQ.1.0D0) THEN
C     SCY FANG
                  RANGE1=SYSTEM1(21)*LFOB(1)
                  UNN1='DEGREE(S)'
              ELSE
                  RANGE1=SYSTEM1(14)*LFOB(1)
                  IF(SYSTEM1(6).EQ.1.0D0) UNN1='in(s)    '
                  IF(SYSTEM1(6).EQ.2.0D0) UNN1='cm(s)    '
                  IF(SYSTEM1(6).EQ.3.0D0) UNN1='mm(s)    '
                  IF(SYSTEM1(6).EQ.4.0D0) UNN1='meter(s) '
C     SCY
              END IF
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='OBJECT POINT-Y'
C
              CALL MY_JUSTSTRING(6000,6300,NNTT1(1:14),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN1
C
              CALL MY_JUSTSTRING(7300,6300,NNTT1(1:28),NT1ANG,NT1SIZ,3)
C
          END IF
C
C     NOW DIMENSIONAL LINES
C     LIFT PEN
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
C     HORIZONTAL
          CALL MY_PLOT(3000,1350,0,0,0,10000,0,7000)
          CALL MY_PLOT(3000,1450,1,0,0,10000,0,7000)
C
          CALL MY_PLOT(3000,1400,0,0,0,10000,0,7000)
          CALL MY_PLOT(4500,1400,1,0,0,10000,0,7000)

          CALL MY_PLOT(5500,1400,0,0,0,10000,0,7000)
          CALL MY_PLOT(7000,1400,1,0,0,10000,0,7000)

          CALL MY_PLOT(7000,1350,0,0,0,10000,0,7000)
          CALL MY_PLOT(7000,1450,1,0,0,10000,0,7000)
C     VERTICAL
          CALL MY_PLOT(7050,5500,0,0,0,10000,0,7000)
          CALL MY_PLOT(7150,5500,1,0,0,10000,0,7000)

          CALL MY_PLOT(7100,5500,0,0,0,10000,0,7000)
          CALL MY_PLOT(7100,4000,1,0,0,10000,0,7000)
C
          CALL MY_PLOT(7100,3000,0,0,0,10000,0,7000)
          CALL MY_PLOT(7100,1500,1,0,0,10000,0,7000)

          CALL MY_PLOT(7050,1500,0,0,0,10000,0,7000)
          CALL MY_PLOT(7150,1500,1,0,0,10000,0,7000)

C     VERTICAL BAR LABLE
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     NOW WRITE = "VALUE" UNN
          RANGE1=4.0D0*SPDSSIVAL
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='Y = '//CRANGE//' '//UNN
C
          CALL MY_JUSTSTRING(7100,3500,NNTT1(1:29),NT1ANG,NT1SIZ,3)
C
C     HORIZONTAL BAR LABLE
C     NOW WRITE = "VALUE" UNN
          RANGE1=4.0D0*SPDSSIVAL
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='X = '//CRANGE//' '//UNN
C
          CALL MY_JUSTSTRING(4000,1200,NNTT1(1:29),NT1ANG,NT1SIZ,3)
          I3=1
          RETURN
      END


C     THE FOLLOWING ARE FOR LSF AND FIELD ABERRATION PLOTS
      SUBROUTINE PLOTFUNC2(X,Y,FEND,LLIM,ULIM,LFUNC,UFUNC,SPNUM)
C
          IMPLICIT NONE
C
          REAL X,Y

          REAL LLIM,ULIM,LFUNC,UFUNC
C
          REAL*8 XX,YY
C
          INTEGER FEND,I,IX,IY,ALLOERR,SPNUM,COLPAS
C
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          DIMENSION IX(:),IY(:),X(FEND),Y(FEND),XX(:),YY(:)
          ALLOCATABLE :: IX,IY,XX,YY
C
          DEALLOCATE(IX,IY,XX,YY,STAT=ALLOERR)

          ALLOCATE(IX(1:FEND),IY(1:SPNUM)
     1    ,XX(1:FEND),YY(1:FEND),STAT=ALLOERR)
          COLPAS=COLDEF
          CALL MY_COLTYP(COLPAS)
          DO I=1,FEND
              XX(I)=DBLE(X(I))
              YY(I)=DBLE(Y(I))
          END DO
          DO I=1,FEND
              IX(I)=INT((((REAL(XX(I))-LLIM)/(ULIM-LLIM))*6000.0)+2000.0)
              IY(I)=INT((((REAL(YY(I))-LFUNC)/(UFUNC-LFUNC))*4200.0)+1400.0)
          END DO

          CALL MY_PLOTC(IX(1),IY(1),0,0,2000,8000,1400,5600)
          DO I=1,FEND
              CALL MY_PLOTC(IX(I),IY(I),1,0,2000,8000,1400,5600)
          END DO
          DEALLOCATE(IX,IY,XX,YY,STAT=ALLOERR)
      END


C     THE FOLLOWING ARE FOR LSF AND FIELD ABERRATION PLOTS
      SUBROUTINE PLOTFUNC4(X,Y,FEND,LLIM,ULIM,LFUNC,UFUNC,LT)
C
          IMPLICIT NONE
C
          REAL X,Y,LLIM,ULIM,LFUNC,UFUNC
C
          INTEGER FEND,I,IX,IY,ALLOERR,LT,COLPAS
C
          DIMENSION IX(:),IY(:),X(*),Y(*)
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          ALLOCATABLE :: IX,IY
C
          DEALLOCATE(IX,IY,STAT=ALLOERR)

          ALLOCATE(IX(1:FEND),IY(1:FEND),STAT=ALLOERR)
          COLPAS=COLDEF
          CALL MY_COLTYP(COLPAS)
          DO I=1,FEND
              IX(I)=INT((((X(I)-LLIM)/(ULIM-LLIM))*4000.0)+3000.0)
              IY(I)=INT((((Y(I)-LFUNC)/(UFUNC-LFUNC))*3500.0)+1500.0)
          END DO

          CALL MY_PLOTC(IX(1),IY(1),0,LT,3000,7000,1500,5000)

          DO I=1,FEND
              CALL MY_PLOTC(IX(I),IY(I),1,LT,3000,7000,1500,5000)
          END DO

          DEALLOCATE(IX,IY,STAT=ALLOERR)

      END


      SUBROUTINE PLOTFUNC3(X,Y,FEND,LLIM,ULIM,LFUNC,UFUNC,SPNUM)
C
          IMPLICIT NONE
C
          REAL X,Y,LLIM,ULIM,LFUNC,UFUNC
C
          REAL*8 Y2,XX,YY,XDEL,XXX,YYY
C
          INTEGER FEND,I,IX,IY,ALLOERR,SPNUM,COLPAS
C
          DIMENSION IX(:),IY(:),X(FEND),Y(FEND),XX(:),YY(:),
     1    Y2(:),XXX(:),YYY(:)
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          ALLOCATABLE :: IX,IY,XX,YY,Y2,XXX,YYY
C
          DEALLOCATE(IX,IY,XX,YY,Y2,XXX,YYY,STAT=ALLOERR)

          ALLOCATE(IX(1:SPNUM),IY(1:SPNUM),Y2(1:FEND)
     1    ,XX(1:SPNUM),YY(1:SPNUM),XXX(1:SPNUM),YYY(1:SPNUM)
     2    ,STAT=ALLOERR)
          COLPAS=COLDEF
          CALL MY_COLTYP(COLPAS)
          DO I=1,FEND
              XXX(I)=DBLE(X(I))
              YYY(I)=DBLE(Y(I))
          END DO
          CALL SPLINE(XXX,YYY,FEND,1.0D31,1.0D31,Y2)
          XX(1)=DBLE(LLIM)
          CALL SPLINT(XXX,YYY,Y2,FEND,XX(1),YY(1))
          XDEL=DBLE(ULIM-LLIM)/(DBLE(SPNUM-1))
          DO I=2,SPNUM
              XX(I)=XX(I-1)+XDEL
              CALL SPLINT(XXX,YYY,Y2,FEND,XX(I),YY(I))
          END DO
          DO I=1,SPNUM
              IX(I)=INT((((REAL(XX(I))-LLIM)/(ULIM-LLIM))*4000.0)+3000.0)
              IY(I)=INT((((REAL(YY(I))-LFUNC)/(UFUNC-LFUNC))*3500.0)+1500.0)
          END DO

          CALL MY_PLOTC(IX(1),IY(1),0,0,3000,7000,1500,5000)
          DO I=2,SPNUM
              CALL MY_PLOTC(IX(I),IY(I),1,0,3000,7000,1500,5000)
          END DO
          DEALLOCATE(IX,IY,XX,YY,Y2,XXX,YYY,STAT=ALLOERR)
      END


      SUBROUTINE PLOTBOX
          USE GLOBALS
C     DOES THE BIG FRAME AROUND PLOTS
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INTEGER COLPAS
          IF(FRAME) THEN
C     LIFT PEN, MOVE TO FRAME START
              COLPAS=COLFRM
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(0,0,0,0,-10,10010,-10,7010)
C     DROP PEN, DRAW BOX

              CALL MY_PLOT(10000,0,1,0,-10,10010,-10,7010)
              CALL MY_PLOT(10000,7000,1,0,-10,10010,-10,7010)
              CALL MY_PLOT(0,7000,1,0-10,10010,-10,7010)
              CALL MY_PLOT(0,0,1,0,-10,10010,-10,7010)

          END IF
          RETURN
      END

      SUBROUTINE PLOTAXES3
          IMPLICIT NONE
          INTEGER COLPAS
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
C     PLOTS A SET OF AXES WITH 10 DIVISION UP AND 5 ACROSS
          COLPAS=COLAXS
          CALL MY_COLTYP(COLPAS)
C       PLOT THE AXES

          CALL MY_PLOT(2000,1400,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,5600,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,1400,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(8000,1400,1,0,-10,10010,-10,7010)

          IF(MTFGRIDS) THEN
C       PLOT THE VERTICAL GRID LINES (11 OF THEM FROM 0 TO 10)

              CALL MY_PLOT(2000,1820,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,1820,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,2240,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,2240,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,2660,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,2660,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,3080,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,3080,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,3500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,3500,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,3920,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,3920,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,4340,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,4340,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,4760,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,4760,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,5180,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,5180,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,5600,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,5600,1,1,-10,10010,-10,7010)

C       PLOT THE HORIZONTAL TICS (11 OF THEM FROM 0 TO 10)
              CALL MY_PLOT(3200,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(3200,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(4400,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(4400,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(5600,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(5600,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(6800,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(6800,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,5600,1,1,-10,10010,-10,7010)

          END IF
C       PLOT THE VERTICAL TICS (11 OF THEM FROM 0 TO 10)

          CALL MY_PLOT(1900,1400,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,1400,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,1820,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,1820,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,2240,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,2240,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,2660,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,2660,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,3080,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,3080,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,3500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,3500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,3920,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,3920,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,4340,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,4340,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,4760,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,4760,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,5180,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,5180,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,5600,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,5600,1,0,-10,10010,-10,7010)
C       PLOT THE HORIZONTAL TICS (11 OF THEM FROM 0 TO 10)
          CALL MY_PLOT(2000,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,1500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(3200,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(3200,1500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4400,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(4400,1500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(5600,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5600,1500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(6800,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(6800,1500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(8000,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(8000,1500,1,0,-10,10010,-10,7010)


          RETURN
      END

      SUBROUTINE PLOTAXES4
          IMPLICIT NONE
          INTEGER COLPAS
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
C     PLOTS A SET OF AXES WITH 10 DIVISION UP AND 5 ACROSS
          COLPAS=COLAXS
          CALL MY_COLTYP(COLPAS)
C       PLOT THE AXES

          CALL MY_PLOT(2000,1400,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,5600,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(8000,5600,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(8000,1400,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,1400,1,0,-10,10010,-10,7010)

          IF(MTFGRIDS) THEN
C       PLOT THE VERTICAL GRID LINES (11 OF THEM FROM 0 TO 10)

              CALL MY_PLOT(2000,1820,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,1820,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,2240,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,2240,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,2660,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,2660,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,3080,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,3080,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,3500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,3500,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,3920,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,3920,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,4340,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,4340,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,4760,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,4760,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,5180,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,5180,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,5600,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,5600,1,1,-10,10010,-10,7010)
C       PLOT THE HORIZONTAL TICS (11 OF THEM FROM 0 TO 10)
              CALL MY_PLOT(3200,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(3200,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(4400,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(4400,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(5600,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(5600,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(6800,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(6800,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(8000,5600,1,1,-10,10010,-10,7010)

          END IF
C       PLOT THE VERTICAL TICS (11 OF THEM FROM 0 TO 10)

          CALL MY_PLOT(1900,1400,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,1400,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,1820,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,1820,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,2240,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,2240,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,2660,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,2660,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,3080,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,3080,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,3500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,3500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,3920,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,3920,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,4340,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,4340,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,4760,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,4760,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,5180,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,5180,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,5600,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2100,5600,1,0,-10,10010,-10,7010)
C       PLOT THE HORIZONTAL TICS (11 OF THEM FROM 0 TO 10)
          CALL MY_PLOT(2000,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,1500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(3200,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(3200,1500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4400,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(4400,1500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(5600,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5600,1500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(6800,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(6800,1500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(8000,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(8000,1500,1,0,-10,10010,-10,7010)


          RETURN
      END


      SUBROUTINE PLOTHNAME(NAME,N)
          IMPLICIT NONE
          CHARACTER NAME*40
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INTEGER COLPAS,N
C       PLOT THE HORIZONTAL AXIS NAME
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(5000,850,NAME(1:N),0,2,2)
          RETURN
      END

      SUBROUTINE PLOTHNAME2(NAME,N)
          IMPLICIT NONE
          CHARACTER NAME*40
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INTEGER COLPAS,N
C       PLOT THE HORIZONTAL AXIS NAME
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(4500,850,NAME(1:N),0,2,2)
          RETURN
      END

      SUBROUTINE PLOTVNAME(NAME,N)
          IMPLICIT NONE
          CHARACTER NAME*40
          INTEGER COLPAS,N
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
C       PLOT THE VERTICAL AXIS NAME
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(850,3500,NAME(1:N),90,2,2)
          RETURN
      END

      SUBROUTINE PLOTVVAL(LFUNC,DELF1)
          IMPLICIT NONE
          REAL NUMBER,NUMJIM,LFUNC,DELF1
          CHARACTER*40 ANUMBER
          INTEGER COLPAS,IB
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
C
C       PLOT THE VERTICAL AXIS VALUE LABELS
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          NUMJIM=LFUNC
          NUMBER=NUMJIM

          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(1000,1400-50,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELF1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(1000,1820-50,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELF1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(1000,2240-50,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELF1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(1000,2660-50,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELF1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(1000,3080-50,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELF1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(1000,3500-50,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELF1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(1000,3920-50,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELF1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(1000,4340-50,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELF1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(1000,4760-50,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELF1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(1000,5180-50,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELF1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(1000,5600-50,ANUMBER(1:IB),0,1,3)
          RETURN
      END


      SUBROUTINE PLOTHVAL(LLIM,DELX1)
          IMPLICIT NONE
          REAL*4 NUMBER,NUMJIM,LLIM,DELX1
          INTEGER COLPAS,IB
          CHARACTER*40 ANUMBER
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     PLOT THE HORIZONTAL AXIS VALUE LABELS
          NUMJIM=LLIM
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(2000-350,1400-250,ANUMBER(1:IB),0,1,3)
          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(3200-350,1400-250,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(4400-350,1400-250,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(5600-350,1400-250,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(6800-350,1400-250,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(8000-350,1400-250,ANUMBER(1:IB),0,1,3)
          RETURN
      END


      SUBROUTINE PLOTHVAL1(LLIM,DELX1)
          IMPLICIT NONE
          REAL NUMBER,NUMJIM,LLIM,DELX1
          INTEGER COLPAS,IB
          CHARACTER*40 ANUMBER
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     PLOT THE HORIZONTAL AXIS VALUE LABELS
          NUMJIM=LLIM
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(2000-350,1400-250,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(3000-350,1400-250,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(4000-350,1400-250,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(5000-350,1400-250,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0

          CALL MY_JUSTSTRING(6000-350,1400-250,ANUMBER(1:IB),0,1,3)
          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          IF(NUMBER.LT.0.00001) NUMBER=0.0
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)

          CALL MY_JUSTSTRING(7000-350,1400-250,ANUMBER(1:IB),0,1,3)
          RETURN
      END


      SUBROUTINE PLOTAXES2
          IMPLICIT NONE
          INTEGER COLPAS
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
C     PLOTS A SET OF AXES WITH 10 DIVISION UP AND 5 ACROSS
          COLPAS=COLAXS
          CALL MY_COLTYP(COLPAS)
C       PLOT THE AXES
          CALL MY_PLOT(5000,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5000,5000,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(3000,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(7000,1500,1,0,-10,10010,-10,7010)
          IF(MTFGRIDS) THEN
C       PLOT THE VERTICAL GRID LINES (11 OF THEM FROM 0 TO 10)
              CALL MY_PLOT(3000,1850,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,1850,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3000,2200,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,2200,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3000,2550,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,2550,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3000,2900,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,2900,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3000,3250,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,3250,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3000,3600,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,3600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3000,3950,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,3950,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3000,4300,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,4300,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3000,4650,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,4650,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3000,5000,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,5000,1,1,-10,10010,-10,7010)
C       PLOT THE HORIZONTAL GRIDS (11 OF THEM FROM 0 TO 10)
              CALL MY_PLOT(3000,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(3000,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3200,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(3200,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3400,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(3400,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3600,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(3600,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(3800,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(3800,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(4000,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(4000,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(4200,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(4200,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(4400,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(4400,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(4600,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(4600,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(4800,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(4800,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(5200,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(5200,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(5400,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(5400,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(5600,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(5600,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(5800,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(5800,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(6000,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(6000,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(6200,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(6200,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(6400,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(6400,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(6600,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(6600,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(6800,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(6800,5000,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,1500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,5000,1,1,-10,10010,-10,7010)
          END IF
C       PLOT THE VERTICAL TICS (11 OF THEM FROM 0 TO 10)
          CALL MY_PLOT(4975,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5025,1500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4975,1850,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5025,1850,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4975,2200,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5025,2200,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4975,2550,0,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5025,2550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4975,2900,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5025,2900,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4950,3250,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5050,3250,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4975,3600,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5025,3600,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4975,3950,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5025,3950,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4975,4300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5025,4300,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4975,4650,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5025,4650,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4950,5000,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5050,5000,1,0,-10,10010,-10,7010)
C       PLOT THE HORIZONTAL TICS (11 OF THEM FROM 0 TO 10)
          CALL MY_PLOT(3000,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(3000,1650,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(3200,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(3200,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(3400,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(3400,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(3600,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(3600,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(3800,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(3800,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4000,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(4000,1650,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4200,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(4200,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4400,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(4400,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4600,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(4600,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4800,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(4800,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(5000,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5000,1650,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(5200,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5200,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(5400,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5400,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(5600,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5600,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(5800,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5800,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(6000,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(6000,1650,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(6200,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(6200,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(6400,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(6400,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(6600,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(6600,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(6800,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(6800,1550,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(7000,1500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(7000,1650,1,0,-10,10010,-10,7010)
          RETURN
      END




      SUBROUTINE PLOTVVAL2(DELF1)
          IMPLICIT NONE
          REAL NUMBER,DELF1
          CHARACTER*40 ANUMBER
          INTEGER COLPAS,IB
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
C
C       PLOT THE VERTICAL AXIS VALUE LABELS
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          NUMBER=DELF1
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(5100,3250-50,ANUMBER(1:IB),0,1,3)

          NUMBER=NUMBER+DELF1
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(5100,5000-50,ANUMBER(1:IB),0,1,3)

          RETURN
      END



      SUBROUTINE PLOTHVAL2(LLIM,DELX1)
          IMPLICIT NONE
          REAL NUMBER,NUMJIM,LLIM,DELX1
          INTEGER COLPAS,IB
          CHARACTER*40 ANUMBER
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     PLOT THE HORIZONTAL AXIS VALUE LABELS
          NUMJIM=LLIM
          NUMBER=NUMJIM
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(3000-350,1450-250,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(4000-350,1450-250,ANUMBER(1:IB),0,1,3)
          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(5000-350,1450-250,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(6000-350,1450-250,ANUMBER(1:IB),0,1,3)

          NUMJIM=NUMJIM+DELX1
          NUMBER=NUMJIM
          CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
          CALL MY_JUSTSTRING(7000-350,1450-250,ANUMBER(1:IB),0,1,3)

          RETURN
      END

      SUBROUTINE PLOTAXES
          IMPLICIT NONE
          INTEGER COLPAS
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
C     PLOTS A SET OF AXES WITH 10 DIVISION UP AND 5 ACROSS
          COLPAS=COLAXS
          CALL MY_COLTYP(COLPAS)
C       PLOT THE AXES
          CALL MY_PLOT(2000,1400,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,5600,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,1400,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(7000,1400,1,0,-10,10010,-10,7010)
          IF(MTFGRIDS) THEN
C       PLOT THE VERTICAL GRID LINES (11 OF THEM FROM 0 TO 10)
              CALL MY_PLOT(2000,1820,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,1820,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,2240,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,2240,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,2660,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,2660,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,3080,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,3080,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,3500,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,3500,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,3920,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,3920,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,4340,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,4340,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,4760,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,4760,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,5180,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,5180,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(2000,5600,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,5600,1,1,-10,10010,-10,7010)
C
C       PLOT THE HORIZONTAL TICS (5 OF THEM FROM 0 TO 10)
              CALL MY_PLOT(3000,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(3000,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(4000,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(4000,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(5000,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(5000,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(6000,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(6000,5600,1,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,1400,0,1,-10,10010,-10,7010)
              CALL MY_PLOT(7000,4760,1,1,-10,10010,-10,7010)
          END IF
C       PLOT THE VERTICAL TICS (11 OF THEM FROM 0 TO 10)
          CALL MY_PLOT(1900,1400,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,1400,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,1820,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,1820,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,2240,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,2240,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,2660,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,2660,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,3080,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,3080,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,3500,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,3500,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,3920,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,3920,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,4340,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,4340,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,4760,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,4760,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,5180,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,5180,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(1900,5600,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,5600,1,0,-10,10010,-10,7010)
C
C       PLOT THE HORIZONTAL TICS (11 OF THEM FROM 0 TO 10)
          CALL MY_PLOT(2000,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(2000,1400,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(3000,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(3000,1400,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(4000,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(4000,1400,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(5000,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(5000,1400,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(6000,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(6000,1400,1,0,-10,10010,-10,7010)
          CALL MY_PLOT(7000,1300,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(7000,1400,1,0,-10,10010,-10,7010)
          RETURN
      END


      SUBROUTINE FOVCAPTION1(CT,J,IX,IY)
          IMPLICIT NONE
          INTEGER CT,J,IX,IY,I
          INTEGER NT1SIZ,NT1ANG
          CHARACTER STRING*40
          REAL*8 FLDCODE(1:2,0:10)
          CHARACTER FLDUNIT(0:10)*3,CRANGE1*9,CRANGE2*9,B*40,BTYPE*8
          COMMON/FLDLAB/FLDCODE,FLDUNIT
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          CALL MY_SETFONT(1,0)
C     SET LETTER SIZE AND ANGLE
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETFONT(1,0)
          CALL MY_SETCHARASPECT(1.25,1.25)
          BTYPE=' (HORZ.)'
          IF(CT.EQ.1.OR.CT.EQ.3.OR.CT.EQ.5.OR.CT.EQ.7.OR.CT.EQ.9.OR.
     1    CT.EQ.11.OR.CT.EQ.13.OR.CT.EQ.15.OR.CT.EQ.17.OR.CT.EQ.19)
     2    BTYPE=' (VERT).'
          WRITE(B,101) FLDCODE(1,J)
          READ(B,200) CRANGE1
          WRITE(B,101) FLDCODE(2,J)
          READ(B,200) CRANGE2
101       FORMAT(F9.3)
200       FORMAT(A9)
          DO I=1,9
              IF(CRANGE1(1:1).EQ.' ') CRANGE1(1:9)=CRANGE1(2:9)//' '
              IF(CRANGE2(1:1).EQ.' ') CRANGE2(1:9)=CRANGE2(2:9)//' '
          END DO
          STRING='X='//CRANGE1//' Y='//CRANGE2//' '//FLDUNIT(J)(1:3)//BTYPE
          NT1SIZ=1
          CALL MY_JUSTSTRING(IX-350,IY-10,STRING(1:37),NT1ANG,NT1SIZ,3)
          RETURN
      END


      SUBROUTINE FOVCAPTION(CT,J,IX,IY)
          IMPLICIT NONE
          INTEGER CT,J,IX,IY,I
          INTEGER NT1SIZ,NT1ANG
          CHARACTER STRING*40
          REAL*8 FLDCODE(1:2,0:10)
          CHARACTER FLDUNIT(0:10)*3,CRANGE1*9,CRANGE2*9,B*40,BTYPE*8
          COMMON/FLDLAB/FLDCODE,FLDUNIT
          INCLUDE 'dathgr.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          CALL MY_SETFONT(1,0)
C     SET LETTER SIZE AND ANGLE
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETFONT(1,0)
          CALL MY_SETCHARASPECT(1.25,1.25)
          BTYPE=' (HORZ.)'
          IF(CT.EQ.0.OR.CT.EQ.2.OR.CT.EQ.4.OR.CT.EQ.6.OR.CT.EQ.8.OR.
     1    CT.EQ.10.OR.CT.EQ.12.OR.CT.EQ.14.OR.CT.EQ.16.OR.
     1    CT.EQ.18.OR.CT.EQ.20)
     2    BTYPE=' (VERT.)'
          WRITE(B,101) FLDCODE(1,J)
          READ(B,200) CRANGE1
          WRITE(B,101) FLDCODE(2,J)
          READ(B,200) CRANGE2
          DO I=1,9
              IF(CRANGE1(1:1).EQ.' ') CRANGE1(1:9)=CRANGE1(2:9)//' '
              IF(CRANGE2(1:1).EQ.' ') CRANGE2(1:9)=CRANGE2(2:9)//' '
          END DO
101       FORMAT(F9.3)
200       FORMAT(A9)
          STRING='X='//CRANGE1//' Y='//CRANGE2//' '//FLDUNIT(J)(1:3)//BTYPE
          IF(J.EQ.0.AND.CT.EQ.1) STRING=
     1    'DIFFRACTION LIMIT HORIZONTAL BARS    '
          IF(J.EQ.0.AND.CT.EQ.2) STRING=
     1    'DIFFRACTION LIMIT VERTICAL BARS      '
          CALL MY_JUSTSTRING(IX-350,IY-10,STRING(1:37),NT1ANG,NT1SIZ,3)
          RETURN
      END


      SUBROUTINE PLOTFUNCB(X,Y,FEND,LLIM,ULIM,LFUNC,UFUNC,SPNUM,LT,CT,J)
C
          IMPLICIT NONE
C
          REAL X,Y,LLIM,ULIM,LFUNC,UFUNC
C
          REAL*8 Y2,XX,YY,XDEL,XXX,YYY
C
          INTEGER FEND,I,IX,IY,ALLOERR,SPNUM,LT,CT,J
C
          LOGICAL DONE(1:22)
C
          REAL*8 FLDCODE(1:2,0:10)
          CHARACTER FLDUNIT(0:10)*3
          COMMON/FLDLAB/FLDCODE,FLDUNIT
C
          DIMENSION IX(:),IY(:),X(FEND),Y(FEND),XX(:),YY(:),
     1    Y2(:),XXX(:),YYY(:)
C
          INCLUDE 'datmai.inc'
C
          ALLOCATABLE :: IX,IY,XX,YY,Y2,XXX,YYY
C
          DEALLOCATE(IX,IY,XX,YY,Y2,XXX,YYY,STAT=ALLOERR)

          ALLOCATE(IX(1:SPNUM),IY(1:SPNUM),Y2(1:FEND)
     1    ,XX(1:SPNUM),YY(1:SPNUM),XXX(1:SPNUM),YYY(1:SPNUM),STAT=ALLOERR)
          DO I=1,22
              DONE(I)=.FALSE.
          END DO
          DO I=1,FEND
              XXX(I)=DBLE(X(I))
              YYY(I)=DBLE(Y(I))
          END DO
          CALL SPLINE(XXX,YYY,FEND,1.0D31,1.0D31,Y2)
          XX(1)=DBLE(LLIM)
          CALL SPLINT(XXX,YYY,Y2,FEND,XX(1),YY(1))
          XDEL=DBLE(ULIM-LLIM)/(DBLE(SPNUM-1))
          DO I=2,SPNUM
              XX(I)=XX(I-1)+XDEL
              CALL SPLINT(XXX,YYY,Y2,FEND,XX(I),YY(I))
          END DO
          DO I=1,SPNUM
              IX(I)=INT((((REAL(XX(I))-LLIM)/(ULIM-LLIM))*5000.0)+2000.0)
              IY(I)=INT((((REAL(YY(I))-LFUNC)/(UFUNC-LFUNC))*4200.0)+1400.0)
          END DO

          CALL MY_PLOTC(IX(1),IY(1),0,LT,2000,7000,1400,5600)

          DO I=2,SPNUM

              if (IX(I).GE.7000.OR.IY(I).LT.1400) then
                  call MY_PLOTC(IX(I),IY(I),0,LT,2000,7000,1400,5600)
              else
                  CALL MY_PLOTC(IX(I),IY(I),1,LT,2000,7000,1400,5600)
              end if

              IF(CT.EQ.1.AND..NOT.DONE(1)) THEN
                  IF(I.GT.1.AND.CT.EQ.1.AND.IX(I-1).LE.2200.AND.
     1            IX(I).GE.2200) THEN
C     DRAW CAPTION LINE 1
                      CALL MY_PLOTC(7100,5000,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),5000,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,5000)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(1)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.2.AND..NOT.DONE(2)) THEN
                  IF(I.GT.1.AND.CT.EQ.2.AND.IX(I-1).LE.2400.AND.
     1            IX(I).GE.2400) THEN
C     DRAW CAPTION LINE 2
                      CALL MY_PLOTC(7100,4850,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4850,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,4850)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(2)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.3.AND..NOT.DONE(3)) THEN
                  IF(I.GT.1.AND.CT.EQ.3.AND.IX(I-1).LE.2600.AND.
     1            IX(I).GE.2600) THEN
C     DRAW CAPTION LINE 3
                      CALL MY_PLOTC(7100,4700,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4700,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,4700)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(3)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.4.AND..NOT.DONE(4)) THEN
                  IF(I.GT.1.AND.CT.EQ.4.AND.IX(I-1).LE.2800.AND.
     1            IX(I).GE.2800) THEN
C     DRAW CAPTION LINE 4
                      CALL MY_PLOTC(7100,4550,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4550,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,4550)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(4)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.5.AND..NOT.DONE(5)) THEN
                  IF(I.GT.1.AND.CT.EQ.5.AND.IX(I-1).LE.3000.AND.
     1            IX(I).GE.3000) THEN
C     DRAW CAPTION LINE 5
                      CALL MY_PLOTC(7100,4400,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4400,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,4400)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(5)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.6.AND..NOT.DONE(6)) THEN
                  IF(I.GT.1.AND.CT.EQ.6.AND.IX(I-1).LE.3200.AND.
     1            IX(I).GE.3200) THEN
C     DRAW CAPTION LINE 6
                      CALL MY_PLOTC(7100,4250,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4250,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,4250)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(6)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.7.AND..NOT.DONE(7)) THEN
                  IF(I.GT.1.AND.CT.EQ.7.AND.IX(I-1).LE.3400.AND.
     1            IX(I).GE.3400) THEN
C     DRAW CAPTION LINE 7
                      CALL MY_PLOTC(7100,4100,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4100,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,4100)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(7)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.8.AND..NOT.DONE(8)) THEN
                  IF(I.GT.1.AND.CT.EQ.8.AND.IX(I-1).LE.3600.AND.
     1            IX(I).GE.3600) THEN
C     DRAW CAPTION LINE 8
                      CALL MY_PLOTC(7100,3950,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3950,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,3950)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(8)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.9.AND..NOT.DONE(9)) THEN
                  IF(I.GT.1.AND.CT.EQ.9.AND.IX(I-1).LE.3800.AND.
     1            IX(I).GE.3800) THEN
C     DRAW CAPTION LINE 9
                      CALL MY_PLOTC(7100,3800,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3800,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,3800)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(9)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.10.AND..NOT.DONE(10)) THEN
                  IF(I.GT.1.AND.CT.EQ.10.AND.IX(I-1).LE.4000.AND.
     1            IX(I).GE.4000) THEN
C     DRAW CAPTION LINE 10
                      CALL MY_PLOTC(7100,3650,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3650,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,3650)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(10)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.11.AND..NOT.DONE(11)) THEN
                  IF(I.GT.1.AND.CT.EQ.11.AND.IX(I-1).LE.4200.AND.
     1            IX(I).GE.4200) THEN
C     DRAW CAPTION LINE 11
                      CALL MY_PLOTC(7100,3500,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3500,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,3500)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(11)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.12.AND..NOT.DONE(12)) THEN
                  IF(I.GT.1.AND.CT.EQ.12.AND.IX(I-1).LE.4400.AND.
     1            IX(I).GE.4400) THEN
C     DRAW CAPTION LINE 12
                      CALL MY_PLOTC(7100,3350,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3350,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,3350)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(12)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.13.AND..NOT.DONE(13)) THEN
                  IF(I.GT.1.AND.CT.EQ.13.AND.IX(I-1).LE.4600.AND.
     1            IX(I).GE.4600) THEN
C     DRAW CAPTION LINE 13
                      CALL MY_PLOTC(7100,3200,1,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3200,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,3200)
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      DONE(13)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.14.AND..NOT.DONE(14)) THEN
                  IF(I.GT.1.AND.CT.EQ.14.AND.IX(I-1).LE.4800.AND.
     1            IX(I).GE.4800) THEN
C     DRAW CAPTION LINE 14
                      CALL MY_PLOTC(7100,3050,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3050,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,3050)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(14)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.15.AND..NOT.DONE(15)) THEN
                  IF(I.GT.1.AND.CT.EQ.15.AND.IX(I-1).LE.5000.AND.
     1            IX(I).GE.5000) THEN
C     DRAW CAPTION LINE 15
                      CALL MY_PLOTC(7100,2900,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2900,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,2900)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(15)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.16.AND..NOT.DONE(16)) THEN
                  IF(I.GT.1.AND.CT.EQ.16.AND.IX(I-1).LE.5200.AND.
     1            IX(I).GE.5200) THEN
C     DRAW CAPTION LINE 16
                      CALL MY_PLOTC(7100,2750,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2750,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,2750)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(16)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.17.AND..NOT.DONE(17)) THEN
                  IF(I.GT.1.AND.CT.EQ.17.AND.IX(I-1).LE.5400.AND.
     1            IX(I).GE.5400) THEN
C     DRAW CAPTION LINE 17
                      CALL MY_PLOTC(7100,2600,1,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2600,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,2600)
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      DONE(17)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.18.AND..NOT.DONE(18)) THEN
                  IF(I.GT.1.AND.CT.EQ.18.AND.IX(I-1).LE.5600.AND.
     1            IX(I).GE.5600) THEN
C     DRAW CAPTION LINE 18
                      CALL MY_PLOTC(7100,2450,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2450,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,2450)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(18)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.19.AND..NOT.DONE(19)) THEN
                  IF(I.GT.1.AND.CT.EQ.19.AND.IX(I-1).LE.5800.AND.
     1            IX(I).GE.5800) THEN
C     DRAW CAPTION LINE 19
                      CALL MY_PLOTC(7100,2300,1,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2300,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,2300)
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      DONE(19)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.20.AND..NOT.DONE(20)) THEN
                  IF(I.GT.1.AND.CT.EQ.20.AND.IX(I-1).LE.6000.AND.
     1            IX(I).GE.6000) THEN
C     DRAW CAPTION LINE 20
                      CALL MY_PLOTC(7100,2150,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2150,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,2150)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(20)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.21.AND..NOT.DONE(21)) THEN
                  IF(I.GT.1.AND.CT.EQ.21.AND.IX(I-1).LE.6200.AND.
     1            IX(I).GE.6200) THEN
C     DRAW CAPTION LINE 21
                      CALL MY_PLOTC(7100,2000,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2000,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,2000)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(21)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.22.AND..NOT.DONE(22)) THEN
                  IF(I.GT.1.AND.CT.EQ.22.AND.IX(I-1).LE.6400.AND.
     1            IX(I).GE.6400) THEN
C     DRAW CAPTION LINE 22
                      CALL MY_PLOTC(7100,1850,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),1850,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION(CT,J,7500,1850)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(22)=.TRUE.
                  END IF
              END IF
          END DO
          DEALLOCATE(IX,IY,XX,YY,Y2,XXX,YYY,STAT=ALLOERR)
      END


      SUBROUTINE FOVCAPTION2(CT,IX,IY,MAXFREQ)
          IMPLICIT NONE
          INTEGER CT,IX,IY,I
          INTEGER NT1SIZ,NT1ANG
          CHARACTER STRING*40,FRQQ*7
          REAL*8 MAXFREQ
          REAL FRQ
          CHARACTER CRANGE1*9,B*40,BTYPE*8
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          CALL MY_SETFONT(1,0)
          IF(SPACEBALL.EQ.2.AND.SYSTEM1(30).LE.2.0D0) THEN
              FRQQ='lp/mm  '
          END IF
          IF(SPACEBALL.EQ.1.AND.NEAR) THEN
              FRQQ='lp/mm  '
          END IF
          IF(SPACEBALL.EQ.1.AND..NOT.NEAR) THEN
              FRQQ='lp/mrad'
          END IF
          IF(SPACEBALL.EQ.2.AND.SYSTEM1(30).GE.3.0D0) THEN
              FRQQ='lp/mrad'
          END IF
C     SET LETTER SIZE AND ANGLE
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETFONT(1,0)
          CALL MY_SETCHARASPECT(1.25,1.25)
          BTYPE=' (HORZ.)'
          IF(CT.EQ.1.OR.CT.EQ.3.OR.CT.EQ.5.OR.CT.EQ.7.OR.CT.EQ.11.OR.
     1    CT.EQ.13.OR.CT.EQ.15.OR.CT.EQ.17.OR.CT.EQ.19)
     2    BTYPE=' (VERT).'
          IF(CT.EQ.1.OR.CT.EQ.2) FRQ=0.0
          IF(CT.EQ.3.OR.CT.EQ.4) FRQ=SNGL(MAXFREQ)
          IF(CT.EQ.5.OR.CT.EQ.6) FRQ=SNGL((2.0D0*MAXFREQ))
          IF(CT.EQ.7.OR.CT.EQ.8) FRQ=SNGL((3.0D0*MAXFREQ))
          IF(CT.EQ.9.OR.CT.EQ.10) FRQ=SNGL((4.0D0*MAXFREQ))
          IF(CT.EQ.11.OR.CT.EQ.12) FRQ=SNGL((5.0D0*MAXFREQ))
          IF(CT.EQ.13.OR.CT.EQ.14) FRQ=SNGL((6.0D0*MAXFREQ))
          IF(CT.EQ.15.OR.CT.EQ.16) FRQ=SNGL((7.0D0*MAXFREQ))
          IF(CT.EQ.17.OR.CT.EQ.18) FRQ=SNGL((8.0D0*MAXFREQ))
          IF(CT.EQ.19.OR.CT.EQ.20) FRQ=SNGL((9.0D0*MAXFREQ))
          IF(CT.EQ.21.OR.CT.EQ.22) FRQ=SNGL((10.0D0*MAXFREQ))
          WRITE(B,101)FRQ
          READ(B,200) CRANGE1
101       FORMAT(F9.3)
200       FORMAT(A9)
          DO I=1,9
              IF(CRANGE1(1:1).EQ.' ') CRANGE1(1:9)=CRANGE1(2:9)//' '
          END DO
          STRING='FREQ='//CRANGE1//FRQQ//BTYPE
          NT1SIZ=1
          CALL MY_JUSTSTRING(IX-350,IY-10,STRING(1:35),NT1ANG,NT1SIZ,3)
          RETURN
      END


      SUBROUTINE PLOTFUNCA(X,Y,FEND,LLIM,ULIM,LFUNC,UFUNC,SPNUM,LT,CT,J)

          IMPLICIT NONE

          REAL X,Y,LLIM,ULIM,LFUNC,UFUNC
          REAL*8 Y2,XX,YY,XDEL,XXX,YYY
          INTEGER FEND,I,IX,IY,ALLOERR,SPNUM,LT,CT,J
          REAL*8 FLDCODE(1:2,0:10)
          CHARACTER FLDUNIT(0:10)*3
          COMMON/FLDLAB/FLDCODE,FLDUNIT

          DIMENSION IX(:),IY(:),X(FEND),Y(FEND),XX(:),YY(:),
     1    Y2(:),XXX(:),YYY(:)

          LOGICAL DONE(1:20)

          ALLOCATABLE :: IX,IY,XX,YY,Y2,XXX,YYY

          DEALLOCATE(IX,IY,XX,YY,Y2,XXX,YYY,STAT=ALLOERR)

          ALLOCATE(IX(1:SPNUM),IY(1:SPNUM),Y2(1:FEND)
     1    ,XX(1:SPNUM),YY(1:SPNUM),XXX(1:SPNUM),YYY(1:SPNUM),STAT=ALLOERR)
C
C     CT REPRESENTS 5 POINTS ON THE HORIZINTAL AXIS
C

          DONE(1:20) =.FALSE.

          DO I=1,FEND
              XXX(I)=DBLE(X(I))
              YYY(I)=DBLE(Y(I))
          END DO
           
          CALL SPLINE(XXX,YYY,FEND,1.0D31,1.0D31,Y2)
          XX(1)=DBLE(LLIM)
          CALL SPLINT(XXX,YYY,Y2,FEND,XX(1),YY(1))
          XDEL=DBLE(ULIM-LLIM)/(DBLE(SPNUM-1))
          DO I=2,SPNUM
              XX(I)=XX(I-1)+XDEL
              CALL SPLINT(XXX,YYY,Y2,FEND,XX(I),YY(I))
          END DO
          DO I=1,SPNUM
              IX(I)=INT((((REAL(XX(I))-LLIM)/(ULIM-LLIM))*5000.0)+2000.0)
              IY(I)=INT((((REAL(YY(I))-LFUNC)/(UFUNC-LFUNC))*4200.0)+1400.0)
          END DO
          CALL MY_PLOTC(IX(1),IY(1),0,LT,2000,7000,1400,5600)
          DO I=2,SPNUM
              CALL MY_PLOTC(IX(I),IY(I),1,LT,2000,7000,1400,5600)
              IF(CT.EQ.1.AND..NOT.DONE(1)) THEN
                  IF(I.GT.1.AND.CT.EQ.1.AND.IX(I-1).LE.2200.AND.
     1            IX(I).GE.2200) THEN
C     DRAW CAPTION LINE 1
                      CALL MY_PLOTC(7100,5000,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),5000,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,5000)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)
                      DONE(1)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.2.AND..NOT.DONE(2)) THEN
                  IF(I.GT.1.AND.CT.EQ.2.AND.IX(I-1).LE.2400.AND.
     1            IX(I).GE.2400) THEN
C     DRAW CAPTION LINE 2

                      CALL MY_PLOTC(7100,4850,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4850,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,4850)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(2)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.3.AND..NOT.DONE(3)) THEN
                  IF(I.GT.1.AND.CT.EQ.3.AND.IX(I-1).LE.2600.AND.
     1            IX(I).GE.2600) THEN
C     DRAW CAPTION LINE 3

                      CALL MY_PLOTC(7100,4700,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4700,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,4700)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(3)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.4.AND..NOT.DONE(4)) THEN
                  IF(I.GT.1.AND.CT.EQ.4.AND.IX(I-1).LE.2800.AND.
     1            IX(I).GE.2800) THEN
C     DRAW CAPTION LINE 4

                      CALL MY_PLOTC(7100,4550,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4550,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,4550)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(4)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.5.AND..NOT.DONE(5)) THEN
                  IF(I.GT.1.AND.CT.EQ.5.AND.IX(I-1).LE.3000.AND.
     1            IX(I).GE.3000) THEN
C     DRAW CAPTION LINE 5

                      CALL MY_PLOTC(7100,4400,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4400,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,4400)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(5)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.6.AND..NOT.DONE(6)) THEN
                  IF(I.GT.1.AND.CT.EQ.6.AND.IX(I-1).LE.3200.AND.
     1            IX(I).GE.3200) THEN
C     DRAW CAPTION LINE 6

                      CALL MY_PLOTC(7100,4250,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4250,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,4250)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(6)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.7.AND..NOT.DONE(7)) THEN
                  IF(I.GT.1.AND.CT.EQ.7.AND.IX(I-1).LE.3400.AND.
     1            IX(I).GE.3400) THEN
C     DRAW CAPTION LINE 7
                      CALL MY_PLOTC(7100,4100,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),4100,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,4100)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(7)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.8.AND..NOT.DONE(8)) THEN
                  IF(I.GT.1.AND.CT.EQ.8.AND.IX(I-1).LE.3600.AND.
     1            IX(I).GE.3600) THEN
C     DRAW CAPTION LINE 8
                      CALL MY_PLOTC(7100,3950,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3950,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,3950)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(8)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.9.AND..NOT.DONE(9)) THEN
                  IF(I.GT.1.AND.CT.EQ.9.AND.IX(I-1).LE.3800.AND.
     1            IX(I).GE.3800) THEN
C     DRAW CAPTION LINE 9
                      CALL MY_PLOTC(7100,3800,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3800,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,3800)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(9)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.10.AND..NOT.DONE(10)) THEN
                  IF(I.GT.1.AND.CT.EQ.10.AND.IX(I-1).LE.4000.AND.
     1            IX(I).GE.4000) THEN
C     DRAW CAPTION LINE 10
                      CALL MY_PLOTC(7100,3650,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3650,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,3650)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)


                      DONE(10)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.11.AND..NOT.DONE(11)) THEN
                  IF(I.GT.1.AND.CT.EQ.11.AND.IX(I-1).LE.4200.AND.
     1            IX(I).GE.4200) THEN
C     DRAW CAPTION LINE 11
                      CALL MY_PLOTC(7100,3500,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3500,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,3500)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(11)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.12.AND..NOT.DONE(12)) THEN
                  IF(I.GT.1.AND.CT.EQ.12.AND.IX(I-1).LE.4400.AND.
     1            IX(I).GE.4400) THEN
C     DRAW CAPTION LINE 12
                      CALL MY_PLOTC(7100,3350,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3350,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,3350)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(12)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.13.AND..NOT.DONE(13)) THEN
                  IF(I.GT.1.AND.CT.EQ.13.AND.IX(I-1).LE.4600.AND.
     1            IX(I).GE.4600) THEN
C     DRAW CAPTION LINE 13
                      CALL MY_PLOTC(7100,3200,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3200,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,3200)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(13)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.14.AND..NOT.DONE(14)) THEN
                  IF(I.GT.1.AND.CT.EQ.14.AND.IX(I-1).LE.4800.AND.
     1            IX(I).GE.4800) THEN
C     DRAW CAPTION LINE 14
                      CALL MY_PLOTC(7100,3050,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),3050,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,3050)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(14)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.15.AND..NOT.DONE(15)) THEN
                  IF(I.GT.1.AND.CT.EQ.15.AND.IX(I-1).LE.5000.AND.
     1            IX(I).GE.5000) THEN
C     DRAW CAPTION LINE 15
                      CALL MY_PLOTC(7100,2900,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2900,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,2900)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(15)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.16.AND..NOT.DONE(16)) THEN
                  IF(I.GT.1.AND.CT.EQ.16.AND.IX(I-1).LE.5200.AND.
     1            IX(I).GE.5200) THEN
C     DRAW CAPTION LINE 16
                      CALL MY_PLOTC(7100,2750,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2750,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,2750)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(16)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.17.AND..NOT.DONE(17)) THEN
                  IF(I.GT.1.AND.CT.EQ.17.AND.IX(I-1).LE.5400.AND.
     1            IX(I).GE.5400) THEN
C     DRAW CAPTION LINE 17
                      CALL MY_PLOTC(7100,2600,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2600,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,2600)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(17)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.18.AND..NOT.DONE(18)) THEN
                  IF(I.GT.1.AND.CT.EQ.18.AND.IX(I-1).LE.5600.AND.
     1            IX(I).GE.5600) THEN
C     DRAW CAPTION LINE 18
                      CALL MY_PLOTC(7100,2450,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2450,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,2450)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(18)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.19.AND..NOT.DONE(19)) THEN
                  IF(I.GT.1.AND.CT.EQ.19.AND.IX(I-1).LE.5800.AND.
     1            IX(I).GE.5800) THEN
C     DRAW CAPTION LINE 19
                      CALL MY_PLOTC(7100,2300,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2300,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,2300)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(19)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.20.AND..NOT.DONE(20)) THEN
                  IF(I.GT.1.AND.CT.EQ.20.AND.IX(I-1).LE.6000.AND.
     1            IX(I).GE.6000) THEN
C     DRAW CAPTION LINE 20
                      CALL MY_PLOTC(7100,2150,0,1,2000,7100,1400,5600)
                      CALL MY_PLOTC(IX(I),2150,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION1(CT,J,7500,2150)
                      CALL MY_PLOTC(IX(I),IY(I),1,1,2000,7000,1400,5600)

                      DONE(20)=.TRUE.
                  END IF
              END IF
          END DO
          DEALLOCATE(IX,IY,XX,YY,Y2,XXX,YYY,STAT=ALLOERR)
      END


C     THE FOLLOWING ARE FOR LSF AND FIELD ABERRATION PLOTS
C     THE FOLLOWING ARE FOR LSF AND FIELD ABERRATION PLOTS
      SUBROUTINE PLOTFUNC1(X,Y,FEND,LLIM,ULIM,LFUNC,UFUNC,SPNUM,LT)
C
          IMPLICIT NONE
C
          REAL X,Y,LLIM,ULIM,LFUNC,UFUNC
C
          REAL*8 Y2,XX,YY,XDEL,XXX,YYY
C
          INTEGER FEND,I,IX,IY,ALLOERR,SPNUM,LT
C
          DIMENSION IX(:),IY(:),X(FEND),Y(FEND),XX(:),YY(:),
     1    Y2(:),XXX(:),YYY(:)
          ALLOCATABLE :: IX,IY,XX,YY,Y2,XXX,YYY
C
          DEALLOCATE(IX,IY,XX,YY,Y2,XXX,YYY,STAT=ALLOERR)

          ALLOCATE(IX(1:SPNUM),IY(1:SPNUM),Y2(1:FEND)
     1    ,XX(1:SPNUM),YY(1:SPNUM),XXX(1:SPNUM),YYY(1:SPNUM),STAT=ALLOERR)

          DO I=1,FEND
              XXX(I)=DBLE(X(I))
              YYY(I)=DBLE(Y(I))
          END DO

          CALL SPLINE(XXX,YYY,FEND,1.0D31,1.0D31,Y2)
          XX(1)=DBLE(LLIM)
          CALL SPLINT(XXX,YYY,Y2,FEND,XX(1),YY(1))
          XDEL=DBLE(ULIM-LLIM)/(DBLE(SPNUM-1))

          DO I=2,SPNUM
              XX(I)=XX(I-1)+XDEL
              CALL SPLINT(XXX,YYY,Y2,FEND,XX(I),YY(I))
          END DO

          DO I=1,SPNUM
              IX(I)=INT((((REAL(XX(I))-LLIM)/(ULIM-LLIM))*5000.0)+2000.0)
              IY(I)=INT((((REAL(YY(I))-LFUNC)/(UFUNC-LFUNC))*4200.0)+1400.0)

          END DO

          CALL MY_PLOTC(IX(1),IY(1),0,LT,2000,7000,1400,5600)

          DO I=2,SPNUM
              CALL MY_PLOTC(IX(I),IY(I),1,LT,2000,7000,1400,5600)

          END DO

          DEALLOCATE(IX,IY,XX,YY,Y2,XXX,YYY,STAT=ALLOERR)
      END


      SUBROUTINE PLOTFUNCC(X,Y,FEND,LLIM,ULIM,LFUNC,UFUNC,SPNUM,LT,
     1CT,MAXFREQ)
C
          IMPLICIT NONE
C
          REAL X,Y,LLIM,ULIM,LFUNC,UFUNC
C
          REAL*8 Y2,XX,YY,XDEL,XXX,YYY,MAXFREQ
C
          INTEGER FEND,I,IX,IY,ALLOERR,SPNUM,LT,CT
C
          LOGICAL DONE(1:22)
C
          REAL*8 FLDCODE(1:2,0:10)
          CHARACTER FLDUNIT(0:10)*3
          COMMON/FLDLAB/FLDCODE,FLDUNIT
C
          DIMENSION IX(:),IY(:),X(FEND),Y(FEND),XX(:),YY(:),
     1    Y2(:),XXX(:),YYY(:)
C
          INCLUDE 'datmai.inc'
C
          ALLOCATABLE :: IX,IY,XX,YY,Y2,XXX,YYY
C
          DEALLOCATE(IX,IY,XX,YY,Y2,XXX,YYY,STAT=ALLOERR)

          ALLOCATE(IX(1:SPNUM),IY(1:SPNUM),Y2(1:FEND)
     1    ,XX(1:SPNUM),YY(1:SPNUM),XXX(1:SPNUM),YYY(1:SPNUM),STAT=ALLOERR)
          DO I=1,22
              DONE(I)=.FALSE.
          END DO
          DO I=1,FEND
              XXX(I)=DBLE(X(I))
              YYY(I)=DBLE(Y(I))
          END DO
          CALL SPLINE(XXX,YYY,FEND,1.0D31,1.0D31,Y2)
          XX(1)=DBLE(LLIM)
          CALL SPLINT(XXX,YYY,Y2,FEND,XX(1),YY(1))
          XDEL=DBLE(ULIM-LLIM)/(DBLE(SPNUM-1))
          DO I=2,SPNUM
              XX(I)=XX(I-1)+XDEL
              CALL SPLINT(XXX,YYY,Y2,FEND,XX(I),YY(I))
          END DO

          DO I=1,SPNUM

              IX(I)=INT((((REAL(XX(I))-LLIM)/(ULIM-LLIM))*5000.0)+2000.0)
              IY(I)=INT((((REAL(YY(I))-LFUNC)/(UFUNC-LFUNC))*4200.0)+1400.0)
          END DO

          CALL MY_PLOTC(IX(1),IY(1),0,LT,2000,7000,1400,5600)

          DO I=2,SPNUM

              if (IX(I).GE.7000.OR.IY(I).LT.1400) then
                  call MY_PLOTC(IX(I),IY(I),0,LT,2000,7000,1400,5600)
              else
                  CALL MY_PLOTC(IX(I),IY(I),1,LT,2000,7000,1400,5600)
              end if

              IF(CT.EQ.1.AND..NOT.DONE(1)) THEN
                  IF(I.GT.1.AND.CT.EQ.1.AND.IX(I-1).LE.2200.AND.
     1            IX(I).GE.2200) THEN
C     DRAW CAPTION LINE 1
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),5000,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,5000,MAXFREQ)
                      CALL MY_PLOTC(7100,5000,1,1,2000,7100,1400,5600)


                      DONE(1)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.2.AND..NOT.DONE(2)) THEN
                  IF(I.GT.1.AND.CT.EQ.2.AND.IX(I-1).LE.2400.AND.
     1            IX(I).GE.2400) THEN
C     DRAW CAPTION LINE 2
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),4850,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,4850,MAXFREQ)
                      CALL MY_PLOTC(7100,4850,1,1,2000,7100,1400,5600)

                      DONE(2)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.3.AND..NOT.DONE(3)) THEN
                  IF(I.GT.1.AND.CT.EQ.3.AND.IX(I-1).LE.2600.AND.
     1            IX(I).GE.2600) THEN
C     DRAW CAPTION LINE 3
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),4700,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,4700,MAXFREQ)
                      CALL MY_PLOTC(7100,4700,1,1,2000,7100,1400,5600)

                      DONE(3)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.4.AND..NOT.DONE(4)) THEN
                  IF(I.GT.1.AND.CT.EQ.4.AND.IX(I-1).LE.2800.AND.
     1            IX(I).GE.2800) THEN
C     DRAW CAPTION LINE 4
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),4550,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,4550,MAXFREQ)
                      CALL MY_PLOTC(7100,4550,1,1,2000,7100,1400,5600)

                      DONE(4)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.5.AND..NOT.DONE(5)) THEN
                  IF(I.GT.1.AND.CT.EQ.5.AND.IX(I-1).LE.3000.AND.
     1            IX(I).GE.3000) THEN
C     DRAW CAPTION LINE 5
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),4400,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,4400,MAXFREQ)
                      CALL MY_PLOTC(7100,4400,1,1,2000,7100,1400,5600)


                      DONE(5)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.6.AND..NOT.DONE(6)) THEN
                  IF(I.GT.1.AND.CT.EQ.6.AND.IX(I-1).LE.3200.AND.
     1            IX(I).GE.3200) THEN
C     DRAW CAPTION LINE 6
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),4250,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,4250,MAXFREQ)
                      CALL MY_PLOTC(7100,4250,1,1,2000,7100,1400,5600)


                      DONE(6)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.7.AND..NOT.DONE(7)) THEN
                  IF(I.GT.1.AND.CT.EQ.7.AND.IX(I-1).LE.3400.AND.
     1            IX(I).GE.3400) THEN
C     DRAW CAPTION LINE 7
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),4100,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,4100,MAXFREQ)
                      CALL MY_PLOTC(7100,4100,1,1,2000,7100,1400,5600)

                      DONE(7)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.8.AND..NOT.DONE(8)) THEN
                  IF(I.GT.1.AND.CT.EQ.8.AND.IX(I-1).LE.3600.AND.
     1            IX(I).GE.3600) THEN
C     DRAW CAPTION LINE 8
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),3950,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,3950,MAXFREQ)
                      CALL MY_PLOTC(7100,3950,1,1,2000,7100,1400,5600)

                      DONE(8)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.9.AND..NOT.DONE(9)) THEN
                  IF(I.GT.1.AND.CT.EQ.9.AND.IX(I-1).LE.3800.AND.
     1            IX(I).GE.3800) THEN
C     DRAW CAPTION LINE 9
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),3800,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,3800,MAXFREQ)
                      CALL MY_PLOTC(7100,3800,1,1,2000,7100,1400,5600)

                      DONE(9)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.10.AND..NOT.DONE(10)) THEN
                  IF(I.GT.1.AND.CT.EQ.10.AND.IX(I-1).LE.4000.AND.
     1            IX(I).GE.4000) THEN
C     DRAW CAPTION LINE 10
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),3650,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,3650,MAXFREQ)
                      CALL MY_PLOTC(7100,3650,1,1,2000,7100,1400,5600)

                      DONE(10)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.11.AND..NOT.DONE(11)) THEN
                  IF(I.GT.1.AND.CT.EQ.11.AND.IX(I-1).LE.4200.AND.
     1            IX(I).GE.4200) THEN
C     DRAW CAPTION LINE 11
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),3500,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,3500,MAXFREQ)
                      CALL MY_PLOTC(7100,3500,1,1,2000,7100,1400,5600)

                      DONE(11)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.12.AND..NOT.DONE(12)) THEN
                  IF(I.GT.1.AND.CT.EQ.12.AND.IX(I-1).LE.4400.AND.
     1            IX(I).GE.4400) THEN
C     DRAW CAPTION LINE 12
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),3350,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,3350,MAXFREQ)
                      CALL MY_PLOTC(7100,3350,1,1,2000,7100,1400,5600)

                      DONE(12)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.13.AND..NOT.DONE(13)) THEN
                  IF(I.GT.1.AND.CT.EQ.13.AND.IX(I-1).LE.4600.AND.
     1            IX(I).GE.4600) THEN
C     DRAW CAPTION LINE 13
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),3200,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,3200,MAXFREQ)
                      CALL MY_PLOTC(7100,3200,1,1,2000,7100,1400,5600)

                      DONE(13)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.14.AND..NOT.DONE(14)) THEN
                  IF(I.GT.1.AND.CT.EQ.14.AND.IX(I-1).LE.4800.AND.
     1            IX(I).GE.4800) THEN
C     DRAW CAPTION LINE 14
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),3050,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,3050,MAXFREQ)
                      CALL MY_PLOTC(7100,3050,1,1,2000,7100,1400,5600)

                      DONE(14)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.15.AND..NOT.DONE(15)) THEN
                  IF(I.GT.1.AND.CT.EQ.15.AND.IX(I-1).LE.5000.AND.
     1            IX(I).GE.5000) THEN
C     DRAW CAPTION LINE 15
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),2900,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,2900,MAXFREQ)
                      CALL MY_PLOTC(7100,2900,1,1,2000,7100,1400,5600)

                      DONE(15)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.16.AND..NOT.DONE(16)) THEN
                  IF(I.GT.1.AND.CT.EQ.16.AND.IX(I-1).LE.5200.AND.
     1            IX(I).GE.5200) THEN
C     DRAW CAPTION LINE 16
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),2750,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,2750,MAXFREQ)
                      CALL MY_PLOTC(7100,2750,1,1,2000,7100,1400,5600)

                      DONE(16)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.17.AND..NOT.DONE(17)) THEN
                  IF(I.GT.1.AND.CT.EQ.17.AND.IX(I-1).LE.5400.AND.
     1            IX(I).GE.5400) THEN
C     DRAW CAPTION LINE 17
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),2600,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,2600,MAXFREQ)
                      CALL MY_PLOTC(7100,2600,1,1,2000,7100,1400,5600)

                      DONE(17)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.18.AND..NOT.DONE(18)) THEN
                  IF(I.GT.1.AND.CT.EQ.18.AND.IX(I-1).LE.5600.AND.
     1            IX(I).GE.5600) THEN
C     DRAW CAPTION LINE 18
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),2450,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,2450,MAXFREQ)
                      CALL MY_PLOTC(7100,2450,1,1,2000,7100,1400,5600)

                      DONE(18)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.19.AND..NOT.DONE(19)) THEN
                  IF(I.GT.1.AND.CT.EQ.19.AND.IX(I-1).LE.5800.AND.
     1            IX(I).GE.5800) THEN
C     DRAW CAPTION LINE 19
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),2300,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,2300,MAXFREQ)
                      CALL MY_PLOTC(7100,2300,1,1,2000,7100,1400,5600)

                      DONE(19)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.20.AND..NOT.DONE(20)) THEN
                  IF(I.GT.1.AND.CT.EQ.20.AND.IX(I-1).LE.6000.AND.
     1            IX(I).GE.6000) THEN
C     DRAW CAPTION LINE 20
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),2150,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,2150,MAXFREQ)
                      CALL MY_PLOTC(7100,2150,1,1,2000,7100,1400,5600)

                      DONE(20)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.21.AND..NOT.DONE(21)) THEN
                  IF(I.GT.1.AND.CT.EQ.21.AND.IX(I-1).LE.6200.AND.
     1            IX(I).GE.6200) THEN
C     DRAW CAPTION LINE 21
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),2000,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,2000,MAXFREQ)
                      CALL MY_PLOTC(7100,2000,1,1,2000,7100,1400,5600)

                      DONE(21)=.TRUE.
                  END IF
              END IF
              IF(CT.EQ.22.AND..NOT.DONE(22)) THEN
                  IF(I.GT.1.AND.CT.EQ.22.AND.IX(I-1).LE.6400.AND.
     1            IX(I).GE.6400) THEN
C     DRAW CAPTION LINE 22
                      CALL MY_PLOTC(IX(I),IY(I),0,1,2000,7000,1400,5600)
                      CALL MY_PLOTC(IX(I),1850,1,1,2000,7000,1400,5600)
                      CALL FOVCAPTION2(CT,7500,1850,MAXFREQ)
                      CALL MY_PLOTC(7100,1850,1,1,2000,7100,1400,5600)

                      DONE(22)=.TRUE.
                  END IF
              END IF
          END DO
          DEALLOCATE(IX,IY,XX,YY,Y2,XXX,YYY,STAT=ALLOERR)
      END


C SUB PLTCAPCO.FOR
      SUBROUTINE PLTCAPCO(IIU)
C
          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE PLOTCON CAPFNOPD AND
C       PLOTCON CAPFNAPD COMMANDS
C
          REAL REFHT
C
          REAL*8 WVAL
C
          LOGICAL PLOTIT(1:10)
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          INTEGER KKV,KKK,I,KVAL,G,IU,IIU
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
          PLOTCAPCON=.TRUE.
C
          AUTOZSCALE=.TRUE.
C
          IF(DF2.EQ.1) IIU=0
          IF(DF2.EQ.0) IIU=1
C
          IF(STI.EQ.1) THEN
              OUTLYNE= '"PLOTCON CAPFN(OPD AND APD)" PLOTS THE EXISTING CAPFN'
              CALL SHOWIT(1)
              OUTLYNE= '"AS A CONTOUR PLOT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE= '"PLOTCON CAPFN(OPD OR APD)" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'CAPFNOPD') THEN
              IF(S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE= '"PLOTCON CAPFNOPD" TAKES NO NUMERIC WORD #4 or #5'
                  CALL SHOWIT(1)
                  OUTLYNE= '#4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE= 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'CAPFNAPD') THEN
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE= '"PLOT CAPFNAPD" TAKES NO NUMERIC WORD'
                  CALL SHOWIT(1)
                  OUTLYNE='#3, #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE= 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF

          IF(DF1.EQ.1) W1=1.0D0
          IF(DF1.EQ.1) S1=1
          IF(DF1.EQ.1) DF1=0
          IF(INT(W1).LT.1.OR.INT(W1).GT.10) THEN
              OUTLYNE=
     1        'NUMERIC WORD #1 MUST BE 1, 2, 3, 4, 5, 6, 7, 8, 9 OR 10'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'CAPFNOPD'.AND.DF3.EQ.0.AND.W3.GT.0.0D0) THEN
              AUTOZSCALE=.FALSE.
          END IF
C     OPEN AND LOAD THE APPROPRIATE CAPFN DATA AND THEN PLOT IT
          IF(.NOT.CPFNEXT) THEN
              OUTLYNE=
     1        'NO CAPFN EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     DETERMINE THE REFERNCE APERTURE HEIGHT
C     OF THE REF SURFACE COORDINATES
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.DABS(ALENS(9,NEWREF))
     1    .LE.5.0D0) THEN
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  REFHT=SNGL(ALENS(10,NEWREF))
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=SNGL(ALENS(10,NEWREF))
                  ELSE
                      REFHT=SNGL(ALENS(11,NEWREF))
                  END IF
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=SNGL(ALENS(10,NEWREF))
                  ELSE
                      REFHT=SNGL(ALENS(11,NEWREF))
                  END IF
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=SNGL(ALENS(10,NEWREF))
                  ELSE
                      REFHT=SNGL(ALENS(11,NEWREF))
                  END IF
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  REFHT=SNGL(ALENS(10,NEWREF))
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              IF(PXTRAY(1,NEWREF).GE.PXTRAX(1,NEWREF)) THEN
                  REFHT=SNGL((PXTRAY(1,NEWREF)))
              ELSE
                  REFHT=SNGL((PXTRAX(1,NEWREF)))
              END IF
          END IF
C
          WVAL=(W1)
          KKV=(ITOT-1)/NUMCOL
C     KKV IS THE TOTAL NUMBER OF POINTS AT THE DESIRED COLOR
C     AND IT IS AN EXACT SQUARE
          PLOTIT(1)=.FALSE.
          PLOTIT(2)=.FALSE.
          PLOTIT(3)=.FALSE.
          PLOTIT(4)=.FALSE.
          PLOTIT(5)=.FALSE.
          PLOTIT(6)=.FALSE.
          PLOTIT(7)=.FALSE.
          PLOTIT(8)=.FALSE.
          PLOTIT(9)=.FALSE.
          PLOTIT(10)=.FALSE.
          DO I=1,ITOT-1
C     LOAD DSPOTT(*,ID) INTO DSPOT(*)
              ID=I
              CALL SPOTIT(4)
C
              IF(DSPOT(16).EQ.WVAL) THEN
                  PLOTIT(INT(WVAL))=.TRUE.
                  KVAL=I
                  GO TO 10
              END IF
C
          END DO
 10       CONTINUE
          G=ITOT+1
C

          KKK=NINT(SQRT(FLOAT(KKV)))
          IF(.NOT.PLOTIT(INT(W1))) THEN
              OUTLYNE= 'CAPFN DATA FOR THE WAVELENGTH VALUE (NUMERIC WORD 1)'
              CALL SHOWIT(1)
              OUTLYNE= 'DOES NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE= 'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     NOW CALL ROUTINES TO DO THE CONTOUR PLOT
          IF(WQ.EQ.'CAPFNOPD') CALL CON_CAPPLOT(1,WVAL,KKV,KKK
     1    ,IU)
          IF(WQ.EQ.'CAPFNAPD') CALL CON_CAPPLOT(2,WVAL,KKV,KKK
     1    ,IU)
C
          RETURN
      END


C SUB CON_CAPPLOT.FOR
      SUBROUTINE CON_CAPPLOT(IJ,WVAL,KKV,KKK,IU)
C     KVAL IS THE FIRST RECORD OF THE SPOT FILE TO READ
C     REFHT IS THE NORMALIZING AP HT.
C     WVAL IS THE DESIRED WAVELENGTH NUMBER
C     KKV IS THE TOTAL NUMBER OF POINTS TO BE READ IN FROM DSPOTT
C
          IMPLICIT NONE
C
          LOGICAL ITSFLAT
C
          INTEGER IJ,I,J,KKI2,KKK,IQ,KKV
          INTEGER ALLOERR,WVNUMB,IU
C
          REAL*8 WVAL

          REAL ZMAX2,ZMIN2,RI,RII,ZSTEP
          REAL F1PLT,F2PLT,DELGRID
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          DIMENSION F1PLT(:,:),F2PLT(:,:)
C
          ALLOCATABLE :: F1PLT,F2PLT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          KKI2=NINT(SQRT(FLOAT(KKV)))
          DEALLOCATE(F1PLT,F2PLT,STAT=ALLOERR)
          ALLOCATE(F1PLT(KKK,KKK),
     1    F2PLT(KKK,KKK),STAT=ALLOERR)
C
          ZMAX1=-1.0E10
          ZMIN1=1.0E10
          ZMAX2=1.0E0
          ZMIN2=0.0E0
          I=1
          J=0

          DO IQ=1,ITOT-1
C     LOAD DSPOTT(*,ID) INTO DSPOT(*)
              ID=IQ
              CALL SPOTIT(4)
              IF(DSPOT(16).NE.WVAL) THEN
                  GO TO 101
              END IF
              J=J+1
              IF(J.GT.KKK) THEN
                  J=1
                  I=I+1
              END IF
              F1PLT(J,I)=SNGL(DSPOT(4)/(TWOPII))
              F2PLT(J,I)=SNGL(DSPOT(12))
              WVNUMB=INT(DSPOT(16))
              IF(F2PLT(J,I).NE.0.0) THEN
                  IF(F1PLT(J,I).GT.ZMAX1) ZMAX1=(F1PLT(J,I))
                  IF(F1PLT(J,I).LE.ZMIN1) ZMIN1=(F1PLT(J,I))
              END IF
 101          CONTINUE
          END DO
          ITSFLAT=.FALSE.
          IF(ABS(ZMIN1-ZMAX1).LT.1.0E-6) THEN
              IF(ABS(ZMIN1).LE.1.0E-6) ZMIN1=0.0
              ZMAX1=ZMIN1+1.0
              ITSFLAT=.TRUE.
              GO TO 5
          END IF
C
  5       DO I=-10000,10000,1
              RI=FLOAT(I)/100.0
              RII=FLOAT(I+1)/100.0
              IF(ZMAX1.GT.RI.AND.ZMAX1.LE.RII) THEN
                  ZMAX1=RII
                  GO TO 10
              END IF
          END DO
 10       CONTINUE
          DO I=-10000,10000,1
              RI=FLOAT(I)/100.0
              RII=FLOAT(I+1)/100.0
              IF(ZMIN1.GE.RI.AND.ZMIN1.LT.RII) THEN
                  ZMIN1=RI
                  GO TO 20
              END IF
          END DO
 20       CONTINUE
          IF(ZMIN1.EQ.ZMAX1) THEN
              ZMAX1=ZMIN1+0.01
              DO J=1,KKI2
                  DO I=1,KKI2
                      IF(F1PLT(I,J).LT.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1                F1PLT(I,J)=-0.0001
                      IF(F1PLT(I,J).GE.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1                F1PLT(I,J)= 0.0001
                  END DO
              END DO
          END IF
          CONTINUE
          DO J=1,KKI2
              DO I=1,KKI2
                  IF(F1PLT(I,J).LT.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1            F1PLT(I,J)=-0.0001
                  IF(F1PLT(I,J).GE.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1            F1PLT(I,J)= 0.0001
              END DO
          END DO

          DELGRID=1.0/(FLOAT(KKI2)-1.0)
          DO J=1,KKI2
              DO I=1,KKI2
                  F1PLT(I,J)=(F1PLT(I,J)-ZMIN1)/(ZMAX1-ZMIN1)
                  IF(ABS(F1PLT(I,J)).LT.1E-6) F1PLT(I,J)=0.0
                  IF(ABS(F2PLT(I,J)).LT.1E-6) F2PLT(I,J)=0.0
              END DO
          END DO
          IF(ITSFLAT) THEN
              J=KKI2
              I=KKI2
              F1PLT(1:I,1:J)=0.01
          END IF
C     WE NOW HAVE PLOTABLE ARRAYS F1PLT AND F2PLT
C     THE ARRAYS ARE DIMENSIONED 1 BY KKI2
C     THE REFERENCE APERTURE WIDTH IS REFHT, MIN AND MAX Z VALUES
C     ARE 0 AND 1.0
C     IJ IS 1 FOR OPD AND 2 FOR INTENSITY
C     WAVLENGTH NUMBER IS WVNUMB
          ZSTEP=0.0
          IF(W3.GT.0.0D0.AND.IJ.EQ.1) ZSTEP=SNGL(W3)
          CALL PLTCONT(KKI2,F1PLT,F2PLT,IJ
     1    ,WVNUMB,ZSTEP,IU)

          DEALLOCATE(F1PLT,F2PLT,STAT=ALLOERR)
          RETURN
      END


      SUBROUTINE PLTCONT(PCOUNT,F1PLT,F2PLT,IJ
     1,WVNUMB,ZSTEP,IU)

          IMPLICIT NONE
          !     REAL SPACER
          INTEGER I,J,ALLOERR,PCOUNT,COLPAS,IJ,WVNUMB,NSTEP,IU
C
          CHARACTER B*10,WAVVAL*10
C
          REAL ZMAX1,ZMIN1,ZSTEP,OPDPEAK,OPDPIT
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          REAL F1PLT,F2PLT,FTF1,FTF2
C
          INTEGER NT1SIZ,NT1ANG,IF1PLT,IF2PLT
C
          DIMENSION IF1PLT(PCOUNT,PCOUNT)
     1    ,IF2PLT(PCOUNT,PCOUNT)
C
          DIMENSION FTF1(:,:),F1PLT(PCOUNT,PCOUNT)
     1    ,F2PLT(PCOUNT,PCOUNT),FTF2(:,:)
          ALLOCATABLE :: FTF1,FTF2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'dathgr.inc'

          DEALLOCATE(FTF1,FTF2,STAT=ALLOERR)
          ALLOCATE(FTF1(PCOUNT,PCOUNT),FTF2(PCOUNT,PCOUNT),STAT=ALLOERR)
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  F1PLT(I,J)=(F1PLT(I,J))
                  IF(F2PLT(I,J).EQ.0.0) THEN
                      F1PLT(I,J)=0.0
                  END IF
              END DO
          END DO
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  IF(F2PLT(I,J).EQ.0.0) F1PLT(I,J)=0.0
                  F1PLT(I,J)=(F1PLT(I,J))*5000.0
                  IF1PLT(I,J)=INT(F1PLT(I,J))
                  F2PLT(I,J)=(F2PLT(I,J))*5000.0
                  IF2PLT(I,J)=INT(F2PLT(I,J))
              END DO
          END DO
C
C
C     NSTEP IS DISTANCE IN Z BETWEEN CONTOURS IN D. I. U.
          IF(AUTOZSCALE.OR.ZSTEP.EQ.0.0) THEN
              NSTEP=500
          ELSE
              IF(IJ.EQ.1) NSTEP=NINT((ABS(ZSTEP/(ZMAX1-ZMIN1)))*5000.0)
              IF(IJ.EQ.2) NSTEP=500
          END IF
C     CALL PLOT CONTOUR HERE
          IF(NSTEP.LT.250) NSTEP=250
C
C     GENERATE GRAPHIC
C     DO A PLOT NEW
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
C     GENERATE GRAPHIC
          CALL PLTDEV
          GRASET=.TRUE.
          PLEXIS=.TRUE.
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT FRAME'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT LI'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          CALL MY_PLOT(5400,1500,1,0,0,10000,0,7000)
          CALL MY_PLOT(5400,2000,0,0,0,10000,0,7000)
          CALL MY_PLOT(5400,1500,0,0,0,10000,0,7000)
          CALL MY_PLOT(5900,1500,1,0,0,10000,0,7000)
          CALL MY_JUSTSTRING(5285,2040,'+Y',0,1,3)
          CALL MY_JUSTSTRING(5940,1470,'+X',0,1,3)
          IF(WVNUMB.GE.1.AND.WVNUMB.LE.5) THEN
              WRITE(B,180)REAL(SYSTEM1(WVNUMB))
          END IF
          IF(WVNUMB.GE.6.AND.WVNUMB.LE.10) THEN
              WRITE(B,180)REAL(SYSTEM1(65+WVNUMB))
          END IF
          READ(B,200) WAVVAL
180       FORMAT(G10.4)
200       FORMAT(A10)

          IF(IJ.EQ.1) THEN
              CALL MY_JUSTSTRING(100,250,
     1        'WAVEFRONT CONTOUR MAP AT WAVELENGTH = '
     2        //TRIM(WAVVAL)//' MICROMETER',0,1,3)
              CALL MY_JUSTSTRING(6550,5400,
     1        'WAVEFRONT CONTOUR LEVELS'
     2        ,0,1,3)
          END IF
          IF(IJ.EQ.2) THEN
              CALL MY_JUSTSTRING(100,250,
     1        'PUPIL INTENSITY MAP AT WAVELENGTH = '
     2        //TRIM(WAVVAL)//' MICROMETER',0,1,3)
              CALL MY_JUSTSTRING(6550,5400,
     1        'RELATIVE PUPIL INTENSITY'
     2        ,0,1,3)
          END IF
C     OPD
          OPDPEAK=ZMAX1
          OPDPIT=ZMIN1

          IF(IJ.EQ.1)
     1    CALL PLOT_CONTOUR_OPD(PCOUNT,IF1PLT,NSTEP,IU,OPDPEAK,OPDPIT)
C     APD
          IF(IJ.EQ.2)
     1    CALL PLOT_CONTOUR_APD(PCOUNT,IF2PLT,NSTEP,IU)
C
          DEALLOCATE(FTF1,FTF2,STAT=ALLOERR)
          RETURN
      END


      SUBROUTINE PLOTUFUNCX
C
          IMPLICIT NONE
C
          INTEGER IX,IY
C
          LOGICAL USCALEX,USCALEY
          COMMON/SCALEU/USCALEX,USCALEY
          INTEGER COLPAS
          REAL YL,YU,SCALEITX,SCALEITY
          COMMON/YLYU/YL,YU
          REAL XL,XU
          COMMON/XLXU/XL,XU
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"PLOT UXLINE" PLOTS HORIZONTAL LINE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE='"PLOT UXLINE" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S4.EQ.1.OR.S3.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"PLOT UXLINE" ONLY TAKES NUMERIC WORDS #1 AND #2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1) LNTYPE=0
          IF(DF2.EQ.0) LNTYPE=INT(W2)
          IF(LNTYPE.GT.0.OR.LNTYPE.LT.0) LNTYPE=0
          IF(DF1.EQ.1) THEN
              OUTLYNE='"PLOT UXLINE" REQUIRES EXPLICIT'
              CALL SHOWIT(1)
              OUTLYNE='"NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.USCALEX) THEN
              OUTLYNE='"PLOT UXLINE" REQUIRES THE X-AXIS TO BE PLOTTED FIRST'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.USCALEY) THEN
              OUTLYNE='"PLOT UXLINE" REQUIRES THE Y-AXIS TO BE PLOTTED FIRST'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          SCALEITX=(UIXSTOP-UIXSTART)/(XU-XL)
          SCALEITY=(UIYSTOP-UIYSTART)/(YU-YL)
          COLPAS=COLPEN
          CALL MY_COLTYP(COLPAS)

          IX=INT(UIXSTART)
          IY=INT(((W1-YL)*SCALEITY)+UIYSTART)
          CALL MY_PLOTC(IX,IY,0,0,UIXSTART,UIXSTOP,UIYSTART,UIYSTOP)
          IX=INT(UIXSTOP)
          IY=INT(((W1-YL)*SCALEITY)+UIYSTART)
          CALL MY_PLOTC(IX,IY,1,LNTYPE,UIXSTART,UIXSTOP,UIYSTART,UIYSTOP)
          RETURN
      END


      SUBROUTINE PLOTUFUNCY
C
          IMPLICIT NONE
C
          INTEGER IX,IY
C
          LOGICAL USCALEX,USCALEY
          COMMON/SCALEU/USCALEX,USCALEY
          INTEGER COLPAS
          REAL YL,YU,SCALEITX,SCALEITY
          COMMON/YLYU/YL,YU
          REAL XL,XU
          COMMON/XLXU/XL,XU
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"PLOT UYLINE" PLOTS VERTICAL LINE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE='"PLOT UYLINE" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S4.EQ.1.OR.S3.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"PLOT UYLINE" ONLY TAKES NUMERIC WORDS #1 AND #2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1) LNTYPE=0
          IF(DF2.EQ.0) LNTYPE=INT(W2)
          IF(LNTYPE.GT.0.OR.LNTYPE.LT.0) LNTYPE=0
          IF(DF1.EQ.1) THEN
              OUTLYNE='"PLOT UYLINE" REQUIRES EXPLICIT'
              CALL SHOWIT(1)
              OUTLYNE='"NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.USCALEX) THEN
              OUTLYNE='"PLOT UYLINE" REQUIRES THE X-AXIS TO BE PLOTTED FIRST'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.USCALEY) THEN
              OUTLYNE='"PLOT UYLINE" REQUIRES THE Y-AXIS TO BE PLOTTED FIRST'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          COLPAS=COLPEN
          CALL MY_COLTYP(COLPAS)
          SCALEITX=(UIXSTOP-UIXSTART)/(XU-XL)
          SCALEITY=(UIYSTOP-UIYSTART)/(YU-YL)

          IY=INT(UIYSTART)
          IX=INT(((W1-XL)*SCALEITX)+UIXSTART)
          CALL MY_PLOTC(IX,IY,0,0,UIXSTART,UIXSTOP,UIYSTART,UIYSTOP)
          IY=INT(UIYSTOP)
          IX=INT(((W1-XL)*SCALEITX)+UIXSTART)
          CALL MY_PLOTC(IX,IY,1,LNTYPE,UIXSTART,UIXSTOP,UIYSTART,UIYSTOP)
          RETURN
      END


      SUBROUTINE PLOTUXAXIS(XLOWER,XUPPER)
          IMPLICIT NONE
          INTEGER COLPAS,IB
          CHARACTER*40 ANUMBER
          LOGICAL USCALEX,USCALEY
          COMMON/SCALEU/USCALEX,USCALEY
          REAL XLOWER,XUPPER,NUMBER,NUMJIM,DELF1
          REAL XL,XU
          INTEGER IX,DIX,I
          COMMON/XLXU/XL,XU
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE='"PLOT UXAXIS" PLOTS THE USER-DEFINED X-AXIS'
              CALL SHOWIT(1)
              OUTLYNE='X-AXIS LABEL'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT UXAXIS" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              OUTLYNE='"PLOT UXAXIS" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LE.W1.AND.DF5.EQ.1) THEN
              OUTLYNE='"PLOT UXAXIS" REQUIRES NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='TO BE NUMERICALLY LESS THAN NUMERIC WORD #2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF4.EQ.1) W4=0.0D0
          COLPAS=COLAXS
          CALL MY_COLTYP(COLPAS)
          CALL MY_SETCHARASPECT(1.5,1.5)
C       PLOT THE UXAXES

          CALL MY_PLOT(UIXSTART,UIYSTART,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(UIXSTOP,UIYSTART,1,0,-10,10010,-10,7010)

C       PLOT THE HORIZONTAL TICS (UUDX+1 OF THEM FROM 0 TO UUDX)
          IX=0
          DIX=(UIXSTOP-UIXSTART)/INT(UUDX)
          DO I=1,INT(UUDX)+1
              CALL MY_PLOT(UIXSTART+IX,UIYSTART-100,0,0,-10,10010,-10,7010)
              CALL MY_PLOT(UIXSTART+IX,UIYSTART,1,0,-10,10010,-10,7010)
              IX=IX+DIX
          END DO

          XL=XLOWER
          XU=XUPPER

          IF(DF5.EQ.1) THEN
C       PLOT LABELS
C
              DELF1=(XUPPER-XLOWER)/UUDX
C     PLOT THE HORIZONTAL AXIS VALUE LABELS
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              NUMJIM=XLOWER
              NUMBER=NUMJIM
              IX=0
              DO I=1,INT(UUDX)+1
                  IF(W4.NE.0.0D0) THEN
                      IF(I.NE.1.AND.I.NE.INT(UUDX)+1) GO TO 5
                  END IF
                  IF(ABS(NUMBER).LT.0.00001) NUMBER=0.0
                  CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
                  CALL MY_JUSTSTRING(UIXSTART+IX-350,UIYSTART-250,
     1            ANUMBER(1:IB),0,1,3)
 5                CONTINUE
                  NUMJIM=NUMJIM+DELF1
                  NUMBER=NUMJIM
                  IX=IX+DIX
              END DO
          END IF
          GRASET=.TRUE.
          PLEXIS=.TRUE.
          USCALEY=.TRUE.
          RETURN
      END


      SUBROUTINE PLOTUYAXIS(YLOWER,YUPPER)
          IMPLICIT NONE
          INTEGER COLPAS,IB,I,IY
          REAL YLOWER,YUPPER,DELF1,NUMJIM,NUMBER
          CHARACTER*40 ANUMBER
          LOGICAL USCALEX,USCALEY
          COMMON/SCALEU/USCALEX,USCALEY
          REAL YL,YU,DIY
          COMMON/YLYU/YL,YU
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE='"PLOT UYAXIS" PLOTS THE USER-DEFINED Y-AXIS'
              CALL SHOWIT(1)
              OUTLYNE='X-AXIS LABEL'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT UYAXIS" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LE.W1.AND.DF5.EQ.1) THEN
              OUTLYNE='"PLOT UYAXIS" REQUIRES NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='TO BE NUMERICALLY LESS THAN NUMERIC WORD #2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              OUTLYNE='"PLOT UYAXIS" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF4.EQ.1) W4=0.0D0
          COLPAS=COLAXS
          CALL MY_COLTYP(COLPAS)
          CALL MY_SETCHARASPECT(1.5,1.5)
C       PLOT THE AXES

          CALL MY_PLOT(UIXSTART,UIYSTART,0,0,-10,10010,-10,7010)
          CALL MY_PLOT(UIXSTART,UIYSTOP,1,0,-10,10010,-10,7010)
C       PLOT THE VERTICAL TICS (UUDY+1 OF THEM FROM 0 TO UUDY)
          IY=0
          DIY=(UIYSTOP-UIYSTART)/INT(UUDY)
          DO I=1,INT(UUDY)+1
              CALL MY_PLOT(UIXSTART-100,UIYSTART+IY,0,0,-10,10010,-10,7010)
              CALL MY_PLOT(UIXSTART,UIYSTART+IY,1,0,-10,10010,-10,7010)
              IY=IY+INT(DIY)
          END DO

          IF(DF5.EQ.1) THEN
C       PLOT LABELS
              YL=YLOWER
              YU=YUPPER
              DELF1=(YUPPER-YLOWER)/UUDY
C     PLOT THE VERTICAL AXIS VALUE LABELS
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              NUMJIM=YLOWER
              NUMBER=NUMJIM
              IY=0
              DO I=1,INT(UUDY)+1
                  IF(ABS(NUMBER).LT.0.00001) NUMBER=0.0
                  IF(W4.NE.0.0D0) THEN
                      IF(I.NE.1.AND.I.NE.INT(UUDY)+1) GO TO 5
                  END IF
                  CALL HMAT(DBLE(NUMBER),3,ANUMBER,.FALSE.,IB)
                  CALL MY_JUSTSTRING(UIXSTART-1000,UIYSTART+IY-50,
     1            ANUMBER(1:IB),0,1,3)
 5                CONTINUE
                  NUMJIM=NUMJIM+DELF1
                  NUMBER=NUMJIM
                  IY=IY+INT(DIY)
              END DO
          END IF
          GRASET=.TRUE.
          PLEXIS=.TRUE.
          USCALEX=.TRUE.
          RETURN
      END


      SUBROUTINE PLOTUXAXRNG(XLOWER,XUPPER)
          IMPLICIT NONE
          REAL XLOWER,XUPPER
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE='"PLOT UXAXRNG" SETS USER-DEFINED X-AXIS RANGE'
              CALL SHOWIT(1)
              OUTLYNE='X-AXIS LABEL'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(S3.EQ.1.OR.SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT UXAXRNG" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              OUTLYNE='"PLOT UYAXRNG" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LE.W1) THEN
              OUTLYNE='"PLOT UXAXRNG" REQUIRES NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='TO BE NUMERICALLY LESS THAN NUMERIC WORD #2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     SET THE STARTING AND ENDING POINTS OF THE X-DIRECTION
C     FOR THE USER-DEFINED PLOT
          XLOWER=XLOWER*1000.0
          XUPPER=XUPPER*1000.0
          UIXSTART=INT(XLOWER)
          UIXSTOP=INT(XUPPER)
          RETURN
      END


      SUBROUTINE PLOTUYAXRNG(YLOWER,YUPPER)
          IMPLICIT NONE
          REAL YLOWER,YUPPER
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE='"PLOT UYAXRNG" SETS USER-DEFINED X-AXIS RANGE'
              CALL SHOWIT(1)
              OUTLYNE='X-AXIS LABEL'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(S3.EQ.1.OR.SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT UYAXRNG" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              OUTLYNE='"PLOT UYAXRNG" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LE.W1) THEN
              OUTLYNE='"PLOT UYAXRNG" REQUIRES NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='TO BE NUMERICALLY LESS THAN NUMERIC WORD #2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     SET THE STARTING AND ENDING POINTS OF THE Y-DIRECTION
C     FOR THE USER-DEFINED PLOT
          YLOWER=YLOWER*1000
          YUPPER=YUPPER*1000
          UIYSTART=INT(YLOWER)
          UIYSTOP=INT(YUPPER)
          RETURN
      END


      SUBROUTINE PLOTUXAXISLB(NAME,N)
          IMPLICIT NONE
          CHARACTER NAME*40
          LOGICAL USCALEX,USCALEY
          COMMON/SCALEU/USCALEX,USCALEY
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INTEGER COLPAS,N
          IF(STI.EQ.1) THEN
              OUTLYNE='"PLOT UXAXISLB" PLOTS THE USER-DEFINED'
              CALL SHOWIT(1)
              OUTLYNE='X-AXIS LABEL'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1) THEN
              OUTLYNE='"PLOT UXAXISLB" ONLY TAKES STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          CALL MY_SETCHARASPECT(1.0,1.0)
C       PLOT THE HORIZONTAL AXIS NAME
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(UIXSTART+((UIXSTOP-UIXSTART)/2),
     1    UIYSTART-550,NAME(1:N),0,2,2)
          RETURN
      END


      SUBROUTINE PLOTUYAXISLB(NAME,N)
          IMPLICIT NONE
          CHARACTER NAME*40
          INTEGER COLPAS,N
          LOGICAL USCALEX,USCALEY
          COMMON/SCALEU/USCALEX,USCALEY
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE='"PLOT UYAXISLB" PLOTS THE USER-DEFINED'
              CALL SHOWIT(1)
              OUTLYNE='Y-AXIS LABEL'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1) THEN
              OUTLYNE='"PLOT UYAXISLB" ONLY TAKES STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       PLOT THE VERTICAL AXIS NAME
          CALL MY_SETCHARASPECT(1.0,1.0)
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(UIXSTART-1150,
     1    UIYSTART+((UIYSTOP-UIYSTART)/2),NAME(1:N),90,2,2)
          RETURN
      END


      SUBROUTINE PLOTUFUNC
C
          IMPLICIT NONE
C
          INTEGER IX,IY,I,J,LXREG,MXREG,LYREG,MYREG
C
          LOGICAL USCALEX,USCALEY
          COMMON/SCALEU/USCALEX,USCALEY
          INTEGER COLPAS
          REAL YL,YU,SCALEITX,SCALEITY
          COMMON/YLYU/YL,YU
          REAL XL,XU
          COMMON/XLXU/XL,XU
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"PLOT UPLOT" PLOTS THE USER-DEFINED FUNCTION'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE='"PLOT UPLOT" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S5.EQ.1) LNTYPE=0
          IF(F5.EQ.0) LNTYPE=INT(W5)
          IF(LNTYPE.LT.0.OR.LNTYPE.GT.9) LNTYPE=0
          IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1.OR.DF4.EQ.1) THEN
              OUTLYNE='"PLOT UPLOT" REQUIRES EXPLICIT'
              CALL SHOWIT(1)
              OUTLYNE='"NUMERIC WORD #1 TO #4 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LE.W1) THEN
              OUTLYNE='"PLOT UPLOT" REQUIRES NUMERIC WORD #2 TO BE GREATER'
              CALL SHOWIT(1)
              OUTLYNE='"THAN NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W4.LE.W3) THEN
              OUTLYNE='"PLOT UPLOT" REQUIRES NUMERIC WORD #4 TO BE GREATER'
              CALL SHOWIT(1)
              OUTLYNE='"THAN NUMERIC WORD #3'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.USCALEX) THEN
              OUTLYNE='"PLOT UPLOT" REQUIRES THE X-AXIS TO BE PLOTTED FIRST'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.USCALEY) THEN
              OUTLYNE='"PLOT UPLOT" REQUIRES THE Y-AXIS TO BE PLOTTED FIRST'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          LXREG=INT(W1)
          MXREG=INT(W2)
          LYREG=INT(W3)
          MYREG=INT(W4)
          IF(W1.LT.1.0D0.OR.W1.GT.MAXREG.OR.
     1    W2.LT.1.0D0.OR.W2.GT.MAXREG.OR.
     2    W3.LT.1.0D0.OR.W3.GT.MAXREG.OR.
     3    W4.LT.1.0D0.OR.W4.GT.MAXREG) THEN
              OUTLYNE='GENERAL PURPOSE STORAGE REGISTER NUMBER OUT OF RANGE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'VALID VALUES FROM 1 TO ',MAXREG,' FOR EACH ENTRY'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          COLPAS=COLPEN
          CALL MY_COLTYP(COLPAS)
          SCALEITX=(UIXSTOP-UIXSTART)/(XU-XL)
          SCALEITY=(UIYSTOP-UIYSTART)/(YU-YL)

          I=LXREG-1
          J=LYREG-1
          IX=INT(((GPREG(I+1)-XL)*SCALEITX)+UIXSTART)
          IY=INT(((GPREG(J+1)-YL)*SCALEITY)+UIYSTART)
          CALL MY_PLOTC(IX,IY,0,0,UIXSTART,UIXSTOP,UIYSTART,UIYSTOP)
 100      CONTINUE
          I=I+1
          J=J+1
          IX=INT(((GPREG(I)-XL)*SCALEITX)+UIXSTART)
          IY=INT(((GPREG(J)-YL)*SCALEITY)+UIYSTART)
          CALL MY_PLOTC(IX,IY,1,LNTYPE,UIXSTART,UIXSTOP,UIYSTART,UIYSTOP)
          IF(I.GE.MXREG.OR.J.GE.MYREG) THEN
              RETURN
          ELSE
              GO TO 100
          END IF
      END
