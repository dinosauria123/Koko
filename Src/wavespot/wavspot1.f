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

C       FIRST FILE OF CAPFN/SPOT ROUTINES

C SUB OPDIN.FOR
      SUBROUTINE OPDIN
C
          IMPLICIT NONE
C
C     THIS ROUTINE INPUTS THE COMPEX APERTURE FUNCTION FROM CAPFNOUT.DAT
C     THEN IT CALCULATES THE PTOV AND RMS OPD
C
C
          INTEGER I,J,K,ALLOERR,MEANCNT,INUMPTS,IVAL3,IVAL4
C
          REAL*8 REFHT,SPACEYY,SYSWV,PIT,PEAK,MEAN
C
          REAL*8 NUMPTS,CAPFNIN,
     1    WV,VAL1,VAL2,PTVOPD,MEAN2,ARG,VAL5
C
          DIMENSION CAPFNIN(:,:,:)
C
          ALLOCATABLE CAPFNIN
C
          LOGICAL EXIS73
          LOGICAL EXIS43
C
          CHARACTER CAPOUT*80,BCAPOUT*80
C
          SAVE CAPFNIN
C
          LOGICAL PLOTIT(1:10)
          COMMON/ITPLOT/PLOTIT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
C
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'CAPFNIN') OUTLYNE=
     1        '"CAPFNIN" INPUTS A CAPFN FROM AN ASCII FILE'
              IF(WC.EQ.'CAPFNIN') OUTLYNE=
     1        '"CAPFNCLR" DEALLOCATES THE CAPFNIN/CAPFNADD ARRAYS'
              IF(WC.EQ.'CAPFNADD') OUTLYNE=
     1        '"CAPFNADD" INPUTS AND ADDS A CAPFN FROM AN ASCII FILE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WC.EQ.'CAPFNCLR') THEN
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*) WC,
     1            ' TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DEALLOCATE (CAPFNIN,STAT=ALLOERR)
              CPFNEXT=.FALSE.
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
              ALLO=.FALSE.
              SUMMOR=.FALSE.
              ALLONUM=0
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*) WC,
     1        ' TAKES NO NUMERIC OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          CAPOUT=trim(HOME)//TRIM(WS)//'.DAT'
          BCAPOUT=trim(HOME)//TRIM(WS)//'.BIN'
          IF(SST.EQ.0) CAPOUT='CAPFNOUT.DAT'
          IF(SST.EQ.0) BCAPOUT='CAPFNOUT.BIN'
          IF(WC.EQ.'CAPFNIN') CPFNEXT=.FALSE.
          IF(WC.EQ.'CAPFNIN') THEN
              DEALLOCATE (CAPFNIN,STAT=ALLOERR)
              ALLO=.FALSE.
              SUMMOR=.FALSE.
              ALLONUM=0
          END IF
          INQUIRE(FILE=trim(HOME)//CAPOUT,EXIST=EXIS73)
          INQUIRE(FILE=trim(HOME)//BCAPOUT,EXIST=EXIS43)
          IF(EXIS73) CPFNEXT=.TRUE.
          IF(.NOT.EXIS73) THEN
              OUTLYNE=
     1        'THE REQUESTED ASCII CAPFN FILE DOES NOT EXIST TO INPUT'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     OPEN FILE
C
          OPEN(UNIT=73,FILE=CAPOUT,
     1    FORM='FORMATTED',STATUS='UNKNOWN')
          OPEN(UNIT=43,ACCESS='DIRECT',FILE=BCAPOUT,
     1    FORM='UNFORMATTED',RECL=(100*NRECL),STATUS='UNKNOWN')
          REWIND(UNIT=73)
C
          READ(73,*) NUMPTS,SPACEYY
          READ(73,*) REFHT,SYSWV
C
          WV=SYSWV
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
          IF(WC.EQ.'CAPFNIN') THEN
              DEALLOCATE(CAPFNIN,STAT=ALLOERR)
              ALLOCATE(CAPFNIN(INT(NUMPTS),INT(NUMPTS),2),STAT=ALLOERR)
              ALLO=.TRUE.
              SUMMOR=.TRUE.
              ALLONUM=INT(NUMPTS)
          END IF
C
          IF(WC.EQ.'CAPFNIN') THEN
              J=INT(NUMPTS)
              I=INT(NUMPTS)
              CAPFNIN(1:I,1:J,1:2)=0.0D0
          END IF
C
          IF(WC.EQ.'CAPFNADD'.AND..NOT.ALLO) THEN
              OUTLYNE=
     1        '"CAPFNIN" MUST HAVE BEEN USED BEFORE "CAPFNADD" MAY BE USED'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL CLOSE_FILE(73,1)
              CALL CLOSE_FILE(43,1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'CAPFNADD'.AND.ALLONUM.NE.INT(NUMPTS)) THEN
              OUTLYNE=
     1        '"ADDED CAPFN IS NOT THE SAME DIMENSION AS CAPFN IN STORAGE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL CLOSE_FILE(73,1)
              CALL CLOSE_FILE(43,1)
              CALL MACFAL
              RETURN
          END IF
C
C     READ THE FILE AND ADD THE NON-ZERO ELEMENTS
          DO J=1,INT(NUMPTS)
              DO I=1,INT(NUMPTS)
                  READ(73,*) VAL1,VAL2,IVAL3,IVAL4,VAL5
                  IF(CAPFNIN(I,J,1).EQ.0.0D0.AND.VAL1.NE.0.0D0) THEN
                      CAPFNIN(I,J,1)=CAPFNIN(I,J,1) + VAL1
                      CAPFNIN(I,J,2)=CAPFNIN(I,J,2) + VAL2
                  END IF
              END DO
          END DO
          INUMPTS=INT(NUMPTS)
C     NEXT REPLACE THE OLD OPD VALUE IN THE LAST READIN CAPFN FILE WITH
C     THE NEW CORRECTED VALUE BY READING IN UNIT 43, CHANGING DSPOT(4)
C     AND WRITING IT OUT AGAIN.
          PEAK=-1.0D+100
          PIT = 1.0D+100
          MEAN=0.0D0
          MEAN2=0.0D0
          RMSOPD=0.0D0
          PTVOPD=0.0D0
          MEANCNT=0
          DO J=1,INT(NUMPTS)
              DO I=1,INT(NUMPTS)
                  IF(CAPFNIN(I,J,1).NE.0.0D0) THEN
                      IF(CAPFNIN(I,J,2).GE.PEAK) PEAK=CAPFNIN(I,J,2)
                      IF(CAPFNIN(I,J,2).LE.PIT)  PIT =CAPFNIN(I,J,2)
                      MEAN=MEAN+((CAPFNIN(I,J,2))/(TWOPII))
                      MEAN2=MEAN2+(((CAPFNIN(I,J,2))/(TWOPII))**2)
                      MEANCNT=MEANCNT+1
                  END IF
              END DO
          END DO
          PTVOPD=(PEAK-PIT)/(TWOPII)
          PTOVOPD(0)=PTVOPD
          PTOVOPD(1)=PTVOPD
          PTOVOPD(2)=PTVOPD
          PTOVOPD(3)=PTVOPD
          PTOVOPD(4)=PTVOPD
          PTOVOPD(5)=PTVOPD
          PTOVOPD(6)=PTVOPD
          PTOVOPD(7)=PTVOPD
          PTOVOPD(8)=PTVOPD
          PTOVOPD(9)=PTVOPD
          PTOVOPD(10)=PTVOPD

          ARG=((MEAN2-((MEAN**2)/
     1    DBLE(MEANCNT)))/DBLE(MEANCNT-1))
          IF(ARG.LT.0.0D0) THEN
              OUTLYNE=
     1        'RMSOPD ERROR OCCURED, CAPFNIN RMSOPD CALCULATION HAS FAILED'
              CALL SHOWIT(1)
              CALL MACFAL
              CALL CLOSE_FILE(73,1)
              CALL CLOSE_FILE(43,1)
              RETURN
          END IF
          RMSOPD=DSQRT(ARG)
          RMSOP(1)=DSQRT(ARG)
          RMSOP(2)=DSQRT(ARG)
          RMSOP(3)=DSQRT(ARG)
          RMSOP(4)=DSQRT(ARG)
          RMSOP(5)=DSQRT(ARG)
          RMSOP(6)=DSQRT(ARG)
          RMSOP(7)=DSQRT(ARG)
          RMSOP(8)=DSQRT(ARG)
          RMSOP(9)=DSQRT(ARG)
          RMSOP(10)=DSQRT(ARG)

C
          IF(WC.EQ.'CAPFNIN'.OR.WC.EQ.'CAPFNADD') THEN
C     SAVE THE SUMMED CAPFN
              ITOT=(INT(NUMPTS)**2)+1
              K=1
              DO J=2,ITOT
                  READ(UNIT=43,REC=J) DSPOT(1),DSPOT(2),DSPOT(3),DSPOT(4)
     1            ,DSPOT(5),DSPOT(6),DSPOT(7),DSPOT(8),DSPOT(9),DSPOT(10),
     2            DSPOT(11),DSPOT(12),DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16)
     3            ,DSPOT(17)
     4            ,DSPOT(18),DSPOT(19),DSPOT(20),DSPOT(21),DSPOT(22),DSPOT(23)
     5            ,DSPOT(24)
     6            ,DSPOT(25),DSPOT(26),DSPOT(27),DSPOT(28),DSPOT(29)
     6            ,DSPOT(30),DSPOT(31),DSPOT(32),DSPOT(33),DSPOT(34),DSPOT(35)
     6            ,DSPOT(36),DSPOT(37),DSPOT(38),DSPOT(39),DSPOT(40),DSPOT(41)
     6            ,DSPOT(42),DSPOT(43),DSPOT(44),DSPOT(45),DSPOT(46),DSPOT(47)
     6            ,DSPOT(48),DSPOT(49),DSPOT(50)
                  IF(DSPOT(12).NE.0.0D0.AND.WC.EQ.'CAPFNADD'.OR.
     1            WC.EQ.'CAPFNIN') THEN
C     LOAD DSPOT(*) INTO DSPOTT(*,ID)
                      ID=J-1
                      CALL SPOTIT(3)
                  END IF
              END DO
              NUMCOL=1
              READ(UNIT=43,REC=ITOT+1) WVSHORT,SHORT,NUMCOL,LFOB(1),LFOB(2)
     1        ,LFOB(3),LFOB(4),LFOB(5),LFOB(6),LFOB(7),IW,DDEELL
              CALL CLOSE_FILE(43,1)
              CPFNEXT=.TRUE.
              PLOTIT(1)=.TRUE.
              PLOTIT(2)=.FALSE.
              PLOTIT(3)=.FALSE.
              PLOTIT(4)=.FALSE.
              PLOTIT(5)=.FALSE.
              PLOTIT(6)=.FALSE.
              PLOTIT(7)=.FALSE.
              PLOTIT(8)=.FALSE.
              PLOTIT(9)=.FALSE.
              PLOTIT(10)=.FALSE.
          END IF
C
          CALL CLOSE_FILE(73,1)
          CALL CLOSE_FILE(43,1)
          RETURN
      END
C SUB CAPGRID.FOR
      SUBROUTINE CAPGRID
C
          IMPLICIT NONE
C
C     THIS ROUTINE OUTPUTS THE COMPEX APERTURE FUNCTION AS A PHASE
C     AND AN APODIZATION GRID FILE FOR LATER INPUT
C
          INTEGER I,J,K,NUMPTS,WVN1
C
          CHARACTER UNIS*6
C
          LOGICAL READOK
C
          REAL*8 SPACING,WAV,MAXX,MAXY,MINX,MINY
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"CAPGRID" OUTPUTS THE CURRENT CAPFN TO AN ASCII FILES'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"GRIDAPD" AND "GRIDOPD'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"CAPGRID" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"CAPGRID" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0) THEN
              OUTLYNE=
     1        '"CAPGRID" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.CPFNEXT) THEN
              OUTLYNE=
     1        'NO CAPFN EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     DEFAULT WAVELENGTH IS CONTROL WAVELENGTH
          IF(INT(W1).LT.1.OR.INT(W1).GT.10) THEN
              OUTLYNE=
     1        'NUMERIC WORD #1 MUST BE 1, 2, 3, 4, 5, 6, 7, 8, 9 OR 10'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          OPEN(UNIT=45,FILE=trim(HOME)//'GRIDAPD.DAT',
     1    FORM='FORMATTED',STATUS='UNKNOWN')
          CALL CLOSE_FILE(45,0)
          OPEN(UNIT=46,FILE=trim(HOME)//'GRIDOPD.DAT',
     1    FORM='FORMATTED',STATUS='UNKNOWN')
          CALL CLOSE_FILE(46,0)
C
C     OPEN GRIDAPD AND GRIDOPD.DAT
C
          OPEN(UNIT=45,FILE=trim(HOME)//'GRIDAPD.DAT',
     1    FORM='FORMATTED',STATUS='UNKNOWN')
          OPEN(UNIT=46,FILE=trim(HOME)//'GRIDOPD.DAT',
     1    FORM='FORMATTED',STATUS='UNKNOWN')
C
          WVN1=INT(LFOB(4))
          IF(WVN1.GE.1.AND.WVN1.LE.5) THEN
              WAV=SYSTEM1(WVN1)
          END IF
          IF(WVN1.GE.6.AND.WVN1.LE.10) THEN
              WAV=SYSTEM1(WVN1+65)
          END IF
          IF(SYSTEM1(6).EQ.1.0D0) WAV=WAV*
     1    ((1.0D-3)/(25.4D0))
          IF(SYSTEM1(6).EQ.2.0D0) WAV=WAV*(1.0D-4)
          IF(SYSTEM1(6).EQ.3.0D0) WAV=WAV*(1.0D-3)
          IF(SYSTEM1(6).EQ.4.0D0) WAV=WAV*(1.0D-6)
          READOK=.FALSE.
          NUMPTS=INT((DSQRT((DBLE(ITOT-1)/DBLE(NUMCOL)))))
          MAXX=-1E30
          MAXY=-1E30
          MINX=1E30
          MINY=1E30
          K=1
          DO J=1,NUMPTS
              DO I=1,NUMPTS
                  K=K+1
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
                  ID=K-1
                  CALL SPOTIT(4)
                  IF(DSPOT(16).EQ.W1) THEN
                      IF(DSPOT(5).GE.MAXX) MAXX=DSPOT(5)
                      IF(DSPOT(6).GE.MAXY) MAXY=DSPOT(6)
                      IF(DSPOT(5).LE.MINX) MINX=DSPOT(5)
                      IF(DSPOT(6).LE.MINY) MINY=DSPOT(6)
                      WRITE(45,15) I,J,DSPOT(12)
                      WRITE(46,15) I,J,(DSPOT(4)/(TWOPII))*WAV
                      READOK=.TRUE.
                  END IF
              END DO
          END DO
 15       FORMAT(I6,',',I6,',',D23.15)
C
          IF(READOK) THEN
              CALL CLOSE_FILE(45,1)
              CALL CLOSE_FILE(46,1)
              WRITE(OUTLYNE,*)
     1        'CAPFN DATA FOR WAVELENGTH NUMBER ',INT(W1)
              CALL SHOWIT(1)
              OUTLYNE='EXISTS. "GRIDAPD.DAT" AND "GRIDOPD.DAT" WERE WRITTEN'
              CALL SHOWIT(1)
              IF((MAXX-MINX).GE.(MAXY-MINY)) THEN
                  SPACING=(MAXX-MINX)/(DBLE(NUMPTS)-1.0D0)
              ELSE
                  SPACING=(MAXY-MINY)/(DBLE(NUMPTS)-1.0D0)
              END IF
              IF(SYSTEM1(6).EQ.1.0D0) UNIS='INCHES'
              IF(SYSTEM1(6).EQ.2.0D0) UNIS='CM    '
              IF(SYSTEM1(6).EQ.3.0D0) UNIS='MM    '
              IF(SYSTEM1(6).EQ.4.0D0) UNIS='METERS'
              REG(40)=REG(9)
              REG(9)=SPACING
              WRITE(OUTLYNE,20) SPACING,UNIS
              CALL SHOWIT(0)
 20           FORMAT(
     1        'GRID SPACING AT THE CURRENT REFERENCE SURFACE = ',G13.6,
     2        1X,A6)
          ELSE
              CALL CLOSE_FILE(44,1)
              CALL CLOSE_FILE(43,1)
              WRITE(OUTLYNE,*)
     1        'CAPFN DATA FOR WAVELENGTH NUMBER ',INT(W1)
              CALL SHOWIT(1)
              OUTLYNE='DOES NOT EXIST. NO ASCII CAPFN FILE WAS WRITTEN'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
          END IF
          RETURN
      END


C SUB OPDOUT.FOR
      SUBROUTINE OPDOUT
C
          IMPLICIT NONE
C
C     THIS ROUTINE OUTPUTS THE COMPEX APERTURE FUNCTION TO AN ASCII FILE
C
C
          INTEGER I,KK,G,WWVN
C
          REAL*8 NUMPTS,REFHT,SPACEYY
C
          CHARACTER CAPOUT*80,BCAPOUT*80
C
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"CAPFNOUT" OUTPUTS THE CURRENT CAPFN TO AN ASCII FILE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"CAPFNOUT" TAKES NO STRING OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          CAPOUT=trim(HOME)//TRIM(WS)//'.DAT'
          BCAPOUT=trim(HOME)//TRIM(WS)//'.BIN'
          IF(WS.EQ.'        ')  CAPOUT='CAPFNOUT.DAT'
          IF(WS.EQ.'        ') BCAPOUT='CAPFNOUT.BIN'
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
!        CALL IOSDELETEFILE(CAPOUT)
!        CALL IOSDELETEFILE(BCAPOUT)
C
C     OPEN CAPFNOUT.DAT
C
          OPEN(UNIT=73,FILE=CAPOUT,
     1    FORM='FORMATTED',STATUS='UNKNOWN')
          OPEN(UNIT=43,ACCESS='DIRECT',FILE=BCAPOUT,
     1    FORM='UNFORMATTED',RECL=(100*NRECL),STATUS='UNKNOWN')
C
C     DETERMINE THE REFERNCE APERTURE HEIGHT
C     OF THE REF SURFACE COORDINATES
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.DABS(ALENS(9,NEWREF))
     1    .LE.5.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  IF(ALENS(10,NEWREF).LE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  REFHT=DABS(ALENS(10,NEWREF))
              END IF
C
C       IPOLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                  REFHT=DABS(ALENS(14,NEWREF))
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              IF(PXTRAY(1,NEWREF).GE.PXTRAX(1,NEWREF)) THEN
                  REFHT=PXTRAY(1,NEWREF)
              ELSE
                  REFHT=PXTRAX(1,NEWREF)
              END IF
          END IF
C
          KK=1
          WRITE(UNIT=43,REC=1) ITOT
          G=ITOT+1
          WRITE(UNIT=43,REC=G) WVSHORT,SHORT,NUMCOL,LFOB(1),LFOB(2)
     1    ,LFOB(3),LFOB(4),LFOB(5),LFOB(6),LFOB(7),IW,DDEELL
C
          WVN=INT(LFOB(4))
          IF(WVN.EQ.1) WWVN=1
          IF(WVN.EQ.2) WWVN=2
          IF(WVN.EQ.3) WWVN=3
          IF(WVN.EQ.4) WWVN=4
          IF(WVN.EQ.5) WWVN=5
          IF(WVN.EQ.6) WWVN=71
          IF(WVN.EQ.7) WWVN=72
          IF(WVN.EQ.8) WWVN=73
          IF(WVN.EQ.9) WWVN=74
          IF(WVN.EQ.10) WWVN=75
          REFHT=REFHT*DDEELL
          NUMPTS=(DSQRT((DBLE(ITOT-1)/DBLE(NUMCOL))))
          SPACEYY=REFHT/(NUMPTS-1.0D0)
          REWIND(UNIT=73)
          WRITE(73,15) NUMPTS,SPACEYY
          DO I=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=I-1
              CALL SPOTIT(4)
              WRITE(UNIT=43,REC=I) DSPOT(1),DSPOT(2),DSPOT(3),DSPOT(4)
     1        ,DSPOT(5),DSPOT(6),DSPOT(7),DSPOT(8),DSPOT(9),DSPOT(10),
     2        DSPOT(11),DSPOT(12),DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16)
     3        ,DSPOT(17)
     4        ,DSPOT(18),DSPOT(19),DSPOT(20),DSPOT(21),DSPOT(22),DSPOT(23)
     5        ,DSPOT(24)
     6        ,DSPOT(25),DSPOT(26),DSPOT(27),DSPOT(28),DSPOT(29)
     6        ,DSPOT(30),DSPOT(31),DSPOT(32),DSPOT(33),DSPOT(34),DSPOT(35)
     6        ,DSPOT(36),DSPOT(37),DSPOT(38),DSPOT(39),DSPOT(40),DSPOT(41)
     6        ,DSPOT(42),DSPOT(43),DSPOT(44),DSPOT(45),DSPOT(46),DSPOT(47)
     6        ,DSPOT(48),DSPOT(49),DSPOT(50)
              WRITE(73,19) DSPOT(12),DSPOT(4),INT(DSPOT(7))
     1        ,INT(DSPOT(8)),DSPOT(35)
          END DO
          WRITE(73,17) REFRY(1,NEWOBJ),REFRY(2,NEWOBJ),REFRY(3,NEWOBJ)
          WRITE(73,17) REFRY(4,NEWOBJ),REFRY(5,NEWOBJ),REFRY(6,NEWOBJ)
          WRITE(73,18) REFRY(25,NEWOBJ)
 15       FORMAT(D23.15,',',D23.15)
! 16   FORMAT(D23.15,',',D23.15,',',I3,',',I3)
 17       FORMAT(D23.15,',',D23.15,',',D23.15)
 18       FORMAT(D23.15)
 19       FORMAT(D23.15,',',D23.15,',',I3,',',I3,',',D23.15)
C
          CALL CLOSE_FILE(73,1)
          CALL CLOSE_FILE(43,1)
          RETURN
      END
C SUB WAMAP.FOR
      SUBROUTINE WAMAP
C
          IMPLICIT NONE
C
          INTEGER KKK,KVAL,I,KKV,ALLOERR
C
          REAL*8 F,X,Y
          DIMENSION F(:,:),X(:),Y(:)
          ALLOCATABLE :: F,X,Y
C
          REAL*8 REFHT,WVAL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"WAMAP" GENERATES A WAVEFRONT MAP USING CAPFN DATA'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        '"WAMAP" TAKES NO QUALIFIER OR ALPHANUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C
          IF(S3.EQ.1.OR.S2.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"WAMAP" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     DEFAULT WAVELENGTH IS CONTROL WAVELENGTH
          IF(DF1.EQ.1) W1=SYSTEM1(11)
          IF(INT(W1).LT.1.OR.INT(W1).GT.10) THEN
              OUTLYNE=
     1        'NUMERIC WORD #1 MUST BE 1, 2, 3, 4, 5, 6, 7, 8, 9 OR 10'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
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
     1    .LE.5.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  IF(ALENS(10,NEWREF).LE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  REFHT=DABS(ALENS(10,NEWREF))
              END IF
C        IPOLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                  REFHT=DABS(ALENS(14,NEWREF))
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              IF(PXTRAY(1,NEWREF).GE.PXTRAX(1,NEWREF)) THEN
                  REFHT=(PXTRAY(1,NEWREF))
              ELSE
                  REFHT=(PXTRAX(1,NEWREF))
              END IF
          END IF
C
          WVAL=(W1)
          KKV=(ITOT-1)/NUMCOL
C     KKV IS THE TOTAL NUMBER OF POINTS AT THE DESIRED COLOR
C     AND IT IS AN EXACT SQUARE
          DO I=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=I-1
              CALL SPOTIT(4)
C
              IF((DSPOT(16)).EQ.WVAL) THEN
                  KVAL=I
                  GO TO 10
              END IF
C
          END DO
 10       CONTINUE
C
          IF(WVAL.GT.NUMCOL) THEN
              OUTLYNE= 'CAPFN DATA FOR THE WAVELENGTH VALUE (NUMERIC WORD 1)'
              CALL SHOWIT(1)
              OUTLYNE= 'DOES NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE= 'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          KKK=INT(SQRT(REAL(KKV)))
          DEALLOCATE(F,X,Y,STAT=ALLOERR)
          ALLOCATE(F(KKK,KKK),X(KKK),Y(KKK),STAT=ALLOERR)
C
          CALL WAPPLOT(REFHT,WVAL,F,X,Y,KKK)
          DEALLOCATE(F,X,Y,STAT=ALLOERR)
C
          RETURN
      END
C SUB AMAP.FOR
      SUBROUTINE AMAP
C
          IMPLICIT NONE
C
          INTEGER KKK,KVAL,I,KKV,ALLOERR
C
          REAL*8 F,X,Y
          DIMENSION F(:,:),X(:),Y(:)
          ALLOCATABLE :: F,X,Y
c
          REAL*8 REFHT,WVAL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"AMAP" GENERATES AN APERTURE APODIZATION FILE USING CAPFN DATA'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        '"AMAP" TAKES NO QUALIFIER OR ALPHANUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C
          IF(S3.EQ.1.OR.S2.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"AMAP" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     DEFAULT WAVELENGTH IS CONTROL WAVELENGTH
          IF(DF1.EQ.1) W1=SYSTEM1(11)
          IF(INT(W1).LT.1.OR.INT(W1).GT.10) THEN
              OUTLYNE=
     1        'NUMERIC WORD #1 MUST BE 1, 2, 3, 4, 5, 6, 7, 8, 9 OR 10'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
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
     1    .LE.5.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  IF(ALENS(10,NEWREF).LE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  REFHT=DABS(ALENS(10,NEWREF))
              END IF
C        IPOLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                  REFHT=DABS(ALENS(14,NEWREF))
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              IF(PXTRAY(1,NEWREF).GE.PXTRAX(1,NEWREF)) THEN
                  REFHT=PXTRAY(1,NEWREF)
              ELSE
                  REFHT=PXTRAX(1,NEWREF)
              END IF
          END IF
C
          WVAL=(W1)
          KKV=(ITOT-1)/NUMCOL
C     KKV IS THE TOTAL NUMBER OF POINTS AT THE DESIRED COLOR
C     AND IT IS AN EXACT SQUARE
          DO I=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=I-1
              CALL SPOTIT(4)
C
              IF((DSPOT(16)).EQ.WVAL) THEN
                  KVAL=I
                  GO TO 10
              END IF
C
          END DO
 10       CONTINUE
C
          IF(WVAL.GT.NUMCOL) THEN
              OUTLYNE= 'CAPFN DATA FOR THE WAVELENGTH VALUE (NUMERIC WORD 1)'
              CALL SHOWIT(1)
              OUTLYNE= 'DOES NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE= 'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          KKK=INT(SQRT(REAL(KKV)))
C
          DEALLOCATE(F,X,Y,STAT=ALLOERR)
          ALLOCATE(F(KKK,KKK),X(KKK),Y(KKK),STAT=ALLOERR)
          CALL APPLOT(REFHT,WVAL,F,X,Y,KKK)
C
          DEALLOCATE(F,X,Y,STAT=ALLOERR)
          RETURN
      END


C SUB WAPPLOT.FOR
      SUBROUTINE WAPPLOT(REFHT,WVAL,F,X,Y,KKK)
C     KVAL IS THE FIRST RECORD OF THE SPOT FILE TO READ
C     REFHT IS THE NORMALIZING AP HT.
C     WVAL IS THE DESIRED WAVELENGTH NUMBER
C     KKV IS THE TOTAL NUMBER OF POINTS TO BE READ IN FROM DSPOTT
C
          IMPLICIT NONE
C
          INTEGER I,J,KKK,IQ,II,IIVAL
C
          CHARACTER AV(1:129)*129,SSYM*1
C
          REAL*8 F,X,Y,DL1,FV
     1    ,REFHT,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,WVAL
C
          DIMENSION F(KKK,KKK),X(KKK),Y(KKK)
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
C
          XMAX=-1.0E10
          YMAX=-1.0E10
          ZMAX=-1.0E10
          XMIN=1.0E10
          YMIN=1.0E10
          ZMIN=1.0E10
          AV(1:129)=' '
          FV=0.0D0
C
C     OPEN OPDMAP.DAT FOR OUTPUT
C
          OPEN(UNIT=70,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'OPDMAP.DAT'
     2    ,STATUS='UNKNOWN')
C
          I=1
          J=0
          DO IQ=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IQ-1
              CALL SPOTIT(4)
C
              IF(DSPOT(16).NE.WVAL) THEN
                  GO TO 101
              END IF
C
              J=J+1
              IF(J.GT.KKK) THEN
                  J=1
                  I=I+1
              END IF
              F(J,I)=DSPOT(4)/(TWOPII)
 1000         FORMAT(D14.6,1X,D14.6,1X,D14.6,1X,D14.6)
C     WRITE X AND Y RAY COORD AT REF SURFACE AND OPD VALUE AND REFHT
              WRITE(70,1000) DSPOT(5),DSPOT(6),F(J,I),REFHT
C     INCLUDE IT IN SEARCH FOR ZMAX AND ZMIN
              IF(F(J,I).GT.ZMAX) ZMAX=F(J,I)
              IF(F(J,I).LE.ZMIN) ZMIN=F(J,I)
 101          CONTINUE
          END DO
          J=0
          I=1
          DO IQ=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IQ-1
              CALL SPOTIT(4)
C
              IF(DSPOT(16).NE.WVAL) THEN
                  GO TO 1100
              END IF
C
              J=J+1
              IF(J.GT.KKK) THEN
                  GO TO 100
              END IF
              X(J)=DSPOT(5)/REFHT
              Y(J)=DSPOT(6)/REFHT
              IF(X(J).GT.XMAX) XMAX=X(J)
              IF(X(J).LE.XMIN) XMIN=X(J)
              IF(Y(J).GT.YMAX) YMAX=X(J)
              IF(Y(J).LE.YMIN) YMIN=X(J)
 1100         CONTINUE
          END DO
 100      CONTINUE
C     NOW DISPLAY THE WAVEFRONT MAP IN A TEXT FORM USING
C     1 THROUGH 9 AND A THROUGH Z TO REPRESENT 35 LEVELS FROM ZMIN
C     TO ZMAX. THE 0 IS RESERVED FOR OPD'S WHICH ARE EXACTLY ZERO
C     WHICH MIGHT MEAN ZERO OPD OR NO SYSTEM THROUGHPUT.
          DL1=DABS((ZMAX-ZMIN)/35.0D0)
C     Z REPRESENTS VALUES FROM ZMIN TO ZMIN+DL1
C     Y REPRESENTS VALUES FROM (ZMIN+DL1) TO
C         (ZMIN+(2.0D0*DL1)
C     ETC TILL
C     1 REPRESENTS ZMAX-DL1 TO ZMAX
C
          J=0
          I=1
C     READ DSPOTT
          DO IQ=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IQ-1
              CALL SPOTIT(4)
C
              IF(DSPOT(16).NE.WVAL) THEN
                  GO TO 105
              END IF
C
              J=J+1
              IF(J.GT.KKK) THEN
                  J=1
                  I=I+1
              END IF
              FV=DSPOT(4)/(TWOPII)
              IF(FV.GE.ZMIN.AND.FV.LT.(ZMIN+DL1)) SSYM='Z'
              IF(FV.GE.ZMIN+(1.0D0*DL1).AND.FV.LT.(ZMIN+(2.0D0*DL1))) SSYM='Y'
              IF(FV.GE.ZMIN+(2.0D0*DL1).AND.FV.LT.(ZMIN+(3.0D0*DL1))) SSYM='X'
              IF(FV.GE.ZMIN+(3.0D0*DL1).AND.FV.LT.(ZMIN+(4.0D0*DL1))) SSYM='W'
              IF(FV.GE.ZMIN+(4.0D0*DL1).AND.FV.LT.(ZMIN+(5.0D0*DL1))) SSYM='V'
              IF(FV.GE.ZMIN+(5.0D0*DL1).AND.FV.LT.(ZMIN+(6.0D0*DL1))) SSYM='U'
              IF(FV.GE.ZMIN+(6.0D0*DL1).AND.FV.LT.(ZMIN+(7.0D0*DL1))) SSYM='T'
              IF(FV.GE.ZMIN+(7.0D0*DL1).AND.FV.LT.(ZMIN+(8.0D0*DL1))) SSYM='S'
              IF(FV.GE.ZMIN+(8.0D0*DL1).AND.FV.LT.(ZMIN+(9.0D0*DL1))) SSYM='R'
              IF(FV.GE.ZMIN+(9.0D0*DL1).AND.FV.LT.(ZMIN+(10.0D0*DL1))) SSYM='Q'
              IF(FV.GE.ZMIN+(10.0D0*DL1).AND.FV.LT.(ZMIN+(11.0D0*DL1))) SSYM='P'
              IF(FV.GE.ZMIN+(11.0D0*DL1).AND.FV.LT.(ZMIN+(12.0D0*DL1))) SSYM='O'
              IF(FV.GE.ZMIN+(12.0D0*DL1).AND.FV.LT.(ZMIN+(13.0D0*DL1))) SSYM='N'
              IF(FV.GE.ZMIN+(13.0D0*DL1).AND.FV.LT.(ZMIN+(14.0D0*DL1))) SSYM='M'
              IF(FV.GE.ZMIN+(14.0D0*DL1).AND.FV.LT.(ZMIN+(15.0D0*DL1))) SSYM='L'
              IF(FV.GE.ZMIN+(15.0D0*DL1).AND.FV.LT.(ZMIN+(16.0D0*DL1))) SSYM='K'
              IF(FV.GE.ZMIN+(16.0D0*DL1).AND.FV.LT.(ZMIN+(17.0D0*DL1))) SSYM='J'
              IF(FV.GE.ZMIN+(17.0D0*DL1).AND.FV.LT.(ZMIN+(18.0D0*DL1))) SSYM='I'
              IF(FV.GE.ZMIN+(18.0D0*DL1).AND.FV.LT.(ZMIN+(19.0D0*DL1))) SSYM='H'
              IF(FV.GE.ZMIN+(19.0D0*DL1).AND.FV.LT.(ZMIN+(20.0D0*DL1))) SSYM='G'
              IF(FV.GE.ZMIN+(20.0D0*DL1).AND.FV.LT.(ZMIN+(21.0D0*DL1))) SSYM='F'
              IF(FV.GE.ZMIN+(21.0D0*DL1).AND.FV.LT.(ZMIN+(22.0D0*DL1))) SSYM='E'
              IF(FV.GE.ZMIN+(22.0D0*DL1).AND.FV.LT.(ZMIN+(23.0D0*DL1))) SSYM='D'
              IF(FV.GE.ZMIN+(23.0D0*DL1).AND.FV.LT.(ZMIN+(24.0D0*DL1))) SSYM='C'
              IF(FV.GE.ZMIN+(24.0D0*DL1).AND.FV.LT.(ZMIN+(25.0D0*DL1))) SSYM='B'
              IF(FV.GE.ZMIN+(25.0D0*DL1).AND.FV.LT.(ZMIN+(26.0D0*DL1))) SSYM='A'
              IF(FV.GE.ZMIN+(26.0D0*DL1).AND.FV.LT.(ZMIN+(27.0D0*DL1))) SSYM='9'
              IF(FV.GE.ZMIN+(27.0D0*DL1).AND.FV.LT.(ZMIN+(28.0D0*DL1))) SSYM='8'
              IF(FV.GE.ZMIN+(28.0D0*DL1).AND.FV.LT.(ZMIN+(29.0D0*DL1))) SSYM='7'
              IF(FV.GE.ZMIN+(29.0D0*DL1).AND.FV.LT.(ZMIN+(30.0D0*DL1))) SSYM='6'
              IF(FV.GE.ZMIN+(30.0D0*DL1).AND.FV.LT.(ZMIN+(31.0D0*DL1))) SSYM='5'
              IF(FV.GE.ZMIN+(31.0D0*DL1).AND.FV.LT.(ZMIN+(32.0D0*DL1))) SSYM='4'
              IF(FV.GE.ZMIN+(32.0D0*DL1).AND.FV.LT.(ZMIN+(33.0D0*DL1))) SSYM='3'
              IF(FV.GE.ZMIN+(33.0D0*DL1).AND.FV.LT.(ZMIN+(34.0D0*DL1))) SSYM='2'
              IF(FV.GE.ZMIN+(34.0D0*DL1).AND.FV.LT.(ZMIN+(35.0D0*DL1))) SSYM='1'
              IF(DSPOT(12).EQ.0.0D0) SSYM='0'
              AV(I)(J:J)=SSYM
 105          CONTINUE
          END DO
          IIVAL=I
C     NOW DISPLAY THE MAP
          IF(IIVAL.GT.64) THEN
              IF(OUT.NE.6) THEN
                  DO II=1,IIVAL
                      WRITE(OUTLYNE,*) AV(II)(1:IIVAL)
                      CALL SHOWIT(0)
                  END DO
                  GO TO 202
              ELSE
                  DO II=1,IIVAL
                      WRITE(OUTLYNE,*) AV(II)(1:64)
                      CALL SHOWIT(0)
                  END DO
                  DO II=1,IIVAL
                      WRITE(OUTLYNE,*) AV(II)(65:IIVAL)
                      CALL SHOWIT(0)
                  END DO
                  GO TO 202
              END IF
          ELSE
C             IIVAL LE 64
              DO II=1,IIVAL
                  WRITE(OUTLYNE,*) AV(II)(1:IIVAL)
                  CALL SHOWIT(0)
              END DO
          END IF
 202      CONTINUE
          WRITE(OUTLYNE,*)
     1    '35 LEVELS FROM "Z" (MINIMUM) TO "1" (MAXIMUM)'
          CALL SHOWIT(0)
          WRITE(OUTLYNE,*)
     1    'EACH LEVEL REPRESENTS OPD RANGE OF ',DL1, ' WAVES'
          CALL SHOWIT(0)
          IF(DL1.NE.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"1" = ',ZMAX-(0.0D0*DL1),' TO ',ZMAX-(1.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"2" = ',ZMAX-(1.0D0*DL1),' TO ',ZMAX-(2.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"3" = ',ZMAX-(2.0D0*DL1),' TO ',ZMAX-(3.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"4" = ',ZMAX-(3.0D0*DL1),' TO ',ZMAX-(4.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"5" = ',ZMAX-(4.0D0*DL1),' TO ',ZMAX-(5.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"6" = ',ZMAX-(5.0D0*DL1),' TO ',ZMAX-(6.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"7" = ',ZMAX-(6.0D0*DL1),' TO ',ZMAX-(7.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"8" = ',ZMAX-(7.0D0*DL1),' TO ',ZMAX-(8.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"9" = ',ZMAX-(8.0D0*DL1),' TO ',ZMAX-(9.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"A" = ',ZMAX-(9.0D0*DL1),' TO ',ZMAX-(10.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"B" = ',ZMAX-(10.0D0*DL1),' TO ',ZMAX-(11.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"C" = ',ZMAX-(11.0D0*DL1),' TO ',ZMAX-(12.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"D" = ',ZMAX-(12.0D0*DL1),' TO ',ZMAX-(13.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"E" = ',ZMAX-(13.0D0*DL1),' TO ',ZMAX-(14.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"F" = ',ZMAX-(14.0D0*DL1),' TO ',ZMAX-(15.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"G" = ',ZMAX-(15.0D0*DL1),' TO ',ZMAX-(16.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"H" = ',ZMAX-(16.0D0*DL1),' TO ',ZMAX-(17.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"I" = ',ZMAX-(17.0D0*DL1),' TO ',ZMAX-(18.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"J" = ',ZMAX-(18.0D0*DL1),' TO ',ZMAX-(19.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"K" = ',ZMAX-(19.0D0*DL1),' TO ',ZMAX-(20.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"L" = ',ZMAX-(20.0D0*DL1),' TO ',ZMAX-(21.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"M" = ',ZMAX-(21.0D0*DL1),' TO ',ZMAX-(22.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"N" = ',ZMAX-(22.0D0*DL1),' TO ',ZMAX-(23.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"O" = ',ZMAX-(23.0D0*DL1),' TO ',ZMAX-(24.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"P" = ',ZMAX-(24.0D0*DL1),' TO ',ZMAX-(25.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"Q" = ',ZMAX-(25.0D0*DL1),' TO ',ZMAX-(26.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"R" = ',ZMAX-(26.0D0*DL1),' TO ',ZMAX-(27.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"S" = ',ZMAX-(27.0D0*DL1),' TO ',ZMAX-(28.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"T" = ',ZMAX-(28.0D0*DL1),' TO ',ZMAX-(29.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"U" = ',ZMAX-(29.0D0*DL1),' TO ',ZMAX-(30.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"V" = ',ZMAX-(30.0D0*DL1),' TO ',ZMAX-(31.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"W" = ',ZMAX-(31.0D0*DL1),' TO ',ZMAX-(32.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"X" = ',ZMAX-(32.0D0*DL1),' TO ',ZMAX-(33.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"Y" = ',ZMAX-(33.0D0*DL1),' TO ',ZMAX-(34.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"Z" = ',ZMAX-(34.0D0*DL1),' TO ',ZMAX-(35.0D0*DL1),' WAVES'
              CALL SHOWIT(0)
C
              WRITE(OUTLYNE,*)'UPPER LEFT HAND CORNER OF MAP IS -X, -Y'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)'LOWER RIGHT HAND CORNER OF MAP IS +X, +Y'
              CALL SHOWIT(0)
              RETURN
          END IF
      END
C SUB APPLOT.FOR
      SUBROUTINE APPLOT(REFHT,WVAL,F,X,Y,KKK)
C     KVAL IS THE FIRST RECORD OF THE SPOT FILE TO READ
C     REFHT IS THE NORMALIZING AP HT.
C     WVAL IS THE DESIRED WAVELENGTH NUMBER
C     KKV IS THE TOTAL NUMBER OF POINTS TO BE READ IN FROM DSPOTT
C
          IMPLICIT NONE
C
          INTEGER I,J,KKK,IQ,II,IIVAL
C
          CHARACTER AV(1:129)*129,SSYM*1
C
          REAL*8 F,X,Y,DL1,FV
     1    ,REFHT,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,WVAL
C
          DIMENSION F(KKK,KKK),X(KKK),Y(KKK)
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
C
          XMAX=-1.0E10
          YMAX=-1.0E10
          ZMAX=-1.0E10
          XMIN=1.0E10
          YMIN=1.0E10
          ZMIN=1.0E10
          AV(1:129)=' '
          FV=0.0D0
C
C     OPEN APMAP.DAT FOR OUTPUT
C
          OPEN(UNIT=72,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'APMAP.DAT'
     2    ,STATUS='UNKNOWN')
C
          I=1
          J=0
          DO IQ=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IQ-1
              CALL SPOTIT(4)
C
              IF(DSPOT(16).NE.WVAL) THEN
                  GO TO 101
              END IF
C
              J=J+1
              IF(J.GT.KKK) THEN
                  J=1
                  I=I+1
              END IF
              F(J,I)=DSPOT(12)
 1000         FORMAT(D14.6,1X,D14.6,1X,D14.6,1X,D14.6)
C     WRITE X AND Y RAY COORD AT REF SURFACE AND VALUE AND REFHT
              WRITE(72,1000) DSPOT(5),DSPOT(6),F(J,I),REFHT
C     INCLUDE IT IN SEARCH FOR ZMAX AND ZMIN
              IF(F(J,I).GE.ZMAX) ZMAX=F(J,I)
              IF(F(J,I).LE.ZMIN) ZMIN=F(J,I)
 101          CONTINUE
          END DO
          IF(ZMAX.EQ.-1.0D10) ZMAX=1.0D0
          IF(ZMIN.EQ.1.0D10) ZMIN=0.0D0
          J=0
          I=1
          DO IQ=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IQ-1
              CALL SPOTIT(4)
C
              IF(DSPOT(16).NE.WVAL) THEN
                  GO TO 1100
              END IF
C
              J=J+1
              IF(J.GT.KKK) THEN
                  J=1
                  I=I+1
              END IF
              X(J)=DSPOT(5)/REFHT
              Y(J)=DSPOT(6)/REFHT
              IF(X(J).GE.XMAX) XMAX=X(J)
              IF(X(J).LE.XMIN) XMIN=X(J)
              IF(Y(J).GE.YMAX) YMAX=Y(J)
              IF(Y(J).LE.YMIN) YMIN=Y(J)
 1100         CONTINUE
          END DO
          CONTINUE
C     NOW DISPLAY THE APMAP IN A TEXT FORM USING
C     1 THROUGH 9 AND A THROUGH Z TO REPRESENT 35 LEVELS FROM ZMIN
C     TO ZMAX. THE 0 IS RESERVED FOR VALUES WHICH ARE EXACTLY ZERO
C     WHICH MEANS NO SYSTEM THROUGHPUT.
          DL1=DABS((ZMAX-ZMIN)/35.0D0)
C     Z REPRESENTS VALUES FROM ZMIN TO ZMIN+DL1
C     Y REPRESENTS VALUES FROM (ZMIN+DL1) TO
C         (ZMIN+(2.0D0*DL1)
C     ETC TILL
C     1 REPRESENTS ZMAX-DL1 TO ZMAX
C
          J=0
          I=1
C     READ DSPOTT
          DO IQ=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IQ-1
              CALL SPOTIT(4)
C
              IF(DSPOT(16).NE.WVAL) THEN
                  GO TO 105
              END IF
C
              J=J+1
              IF(J.GT.KKK) THEN
                  J=1
                  I=I+1
              END IF
              FV=DSPOT(12)
              IF(FV.LT.(ZMIN+DL1)) SSYM='Z'
              IF(FV.GE.ZMIN+(1.0D0*DL1).AND.FV.LT.(ZMIN+(2.0D0*DL1))) SSYM='Y'
              IF(FV.GE.ZMIN+(2.0D0*DL1).AND.FV.LT.(ZMIN+(3.0D0*DL1))) SSYM='X'
              IF(FV.GE.ZMIN+(3.0D0*DL1).AND.FV.LT.(ZMIN+(4.0D0*DL1))) SSYM='W'
              IF(FV.GE.ZMIN+(4.0D0*DL1).AND.FV.LT.(ZMIN+(5.0D0*DL1))) SSYM='V'
              IF(FV.GE.ZMIN+(5.0D0*DL1).AND.FV.LT.(ZMIN+(6.0D0*DL1))) SSYM='U'
              IF(FV.GE.ZMIN+(6.0D0*DL1).AND.FV.LT.(ZMIN+(7.0D0*DL1))) SSYM='T'
              IF(FV.GE.ZMIN+(7.0D0*DL1).AND.FV.LT.(ZMIN+(8.0D0*DL1))) SSYM='S'
              IF(FV.GE.ZMIN+(8.0D0*DL1).AND.FV.LT.(ZMIN+(9.0D0*DL1))) SSYM='R'
              IF(FV.GE.ZMIN+(9.0D0*DL1).AND.FV.LT.(ZMIN+(10.0D0*DL1))) SSYM='Q'
              IF(FV.GE.ZMIN+(10.0D0*DL1).AND.FV.LT.(ZMIN+(11.0D0*DL1))) SSYM='P'
              IF(FV.GE.ZMIN+(11.0D0*DL1).AND.FV.LT.(ZMIN+(12.0D0*DL1))) SSYM='O'
              IF(FV.GE.ZMIN+(12.0D0*DL1).AND.FV.LT.(ZMIN+(13.0D0*DL1))) SSYM='N'
              IF(FV.GE.ZMIN+(13.0D0*DL1).AND.FV.LT.(ZMIN+(14.0D0*DL1))) SSYM='M'
              IF(FV.GE.ZMIN+(14.0D0*DL1).AND.FV.LT.(ZMIN+(15.0D0*DL1))) SSYM='L'
              IF(FV.GE.ZMIN+(15.0D0*DL1).AND.FV.LT.(ZMIN+(16.0D0*DL1))) SSYM='K'
              IF(FV.GE.ZMIN+(16.0D0*DL1).AND.FV.LT.(ZMIN+(17.0D0*DL1))) SSYM='J'
              IF(FV.GE.ZMIN+(17.0D0*DL1).AND.FV.LT.(ZMIN+(18.0D0*DL1))) SSYM='I'
              IF(FV.GE.ZMIN+(18.0D0*DL1).AND.FV.LT.(ZMIN+(19.0D0*DL1))) SSYM='H'
              IF(FV.GE.ZMIN+(19.0D0*DL1).AND.FV.LT.(ZMIN+(20.0D0*DL1))) SSYM='G'
              IF(FV.GE.ZMIN+(20.0D0*DL1).AND.FV.LT.(ZMIN+(21.0D0*DL1))) SSYM='F'
              IF(FV.GE.ZMIN+(21.0D0*DL1).AND.FV.LT.(ZMIN+(22.0D0*DL1))) SSYM='E'
              IF(FV.GE.ZMIN+(22.0D0*DL1).AND.FV.LT.(ZMIN+(23.0D0*DL1))) SSYM='D'
              IF(FV.GE.ZMIN+(23.0D0*DL1).AND.FV.LT.(ZMIN+(24.0D0*DL1))) SSYM='C'
              IF(FV.GE.ZMIN+(24.0D0*DL1).AND.FV.LT.(ZMIN+(25.0D0*DL1))) SSYM='B'
              IF(FV.GE.ZMIN+(25.0D0*DL1).AND.FV.LT.(ZMIN+(26.0D0*DL1))) SSYM='A'
              IF(FV.GE.ZMIN+(26.0D0*DL1).AND.FV.LT.(ZMIN+(27.0D0*DL1))) SSYM='9'
              IF(FV.GE.ZMIN+(27.0D0*DL1).AND.FV.LT.(ZMIN+(28.0D0*DL1))) SSYM='8'
              IF(FV.GE.ZMIN+(28.0D0*DL1).AND.FV.LT.(ZMIN+(29.0D0*DL1))) SSYM='7'
              IF(FV.GE.ZMIN+(29.0D0*DL1).AND.FV.LT.(ZMIN+(30.0D0*DL1))) SSYM='6'
              IF(FV.GE.ZMIN+(30.0D0*DL1).AND.FV.LT.(ZMIN+(31.0D0*DL1))) SSYM='5'
              IF(FV.GE.ZMIN+(31.0D0*DL1).AND.FV.LT.(ZMIN+(32.0D0*DL1))) SSYM='4'
              IF(FV.GE.ZMIN+(32.0D0*DL1).AND.FV.LT.(ZMIN+(33.0D0*DL1))) SSYM='3'
              IF(FV.GE.ZMIN+(33.0D0*DL1).AND.FV.LT.(ZMIN+(34.0D0*DL1))) SSYM='2'
              IF(FV.GE.ZMIN+(34.0D0*DL1)) SSYM='1'
              IF(DSPOT(12).EQ.0.0D0) SSYM='0'
              AV(I)(J:J)=SSYM
 105          CONTINUE
          END DO
          IIVAL=I
C     NOW DISPLAY THE MAP
          IF(IIVAL.GT.64) THEN
              IF(OUT.NE.6) THEN
                  DO II=1,IIVAL
                      WRITE(OUTLYNE,*) AV(II)(1:IIVAL)
                      CALL SHOWIT(0)
                  END DO
                  GO TO 202
              ELSE
                  DO II=1,IIVAL
                      WRITE(OUTLYNE,*) AV(II)(1:64)
                      CALL SHOWIT(0)
                  END DO
                  DO II=1,IIVAL
                      WRITE(OUTLYNE,*) AV(II)(65:IIVAL)
                      CALL SHOWIT(0)
                  END DO
                  GO TO 202
              END IF
          ELSE
C             IIVAL LE 64
              DO II=1,IIVAL
                  WRITE(OUTLYNE,*) AV(II)(1:IIVAL)
                  CALL SHOWIT(0)
              END DO
          END IF
 202      CONTINUE
          WRITE(OUTLYNE,*)
     1    '35 LEVELS FROM "Z" (MINIMUM) TO "1" (MAXIMUM)'
          CALL SHOWIT(0)
          WRITE(OUTLYNE,*)'EACH LEVEL REPRESENTS RANGE OF ',DL1
          CALL SHOWIT(0)
          IF(DL1.NE.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"1" = ',ZMAX-(0.0D0*DL1),' TO ',ZMAX-(1.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"2" = ',ZMAX-(1.0D0*DL1),' TO ',ZMAX-(2.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"3" = ',ZMAX-(2.0D0*DL1),' TO ',ZMAX-(3.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"4" = ',ZMAX-(3.0D0*DL1),' TO ',ZMAX-(4.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"5" = ',ZMAX-(4.0D0*DL1),' TO ',ZMAX-(5.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"6" = ',ZMAX-(5.0D0*DL1),' TO ',ZMAX-(6.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"7" = ',ZMAX-(6.0D0*DL1),' TO ',ZMAX-(7.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"8" = ',ZMAX-(7.0D0*DL1),' TO ',ZMAX-(8.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"9" = ',ZMAX-(8.0D0*DL1),' TO ',ZMAX-(9.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"A" = ',ZMAX-(9.0D0*DL1),' TO ',ZMAX-(10.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"B" = ',ZMAX-(10.0D0*DL1),' TO ',ZMAX-(11.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"C" = ',ZMAX-(11.0D0*DL1),' TO ',ZMAX-(12.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"D" = ',ZMAX-(12.0D0*DL1),' TO ',ZMAX-(13.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"E" = ',ZMAX-(13.0D0*DL1),' TO ',ZMAX-(14.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"F" = ',ZMAX-(14.0D0*DL1),' TO ',ZMAX-(15.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"G" = ',ZMAX-(15.0D0*DL1),' TO ',ZMAX-(16.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"H" = ',ZMAX-(16.0D0*DL1),' TO ',ZMAX-(17.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"I" = ',ZMAX-(17.0D0*DL1),' TO ',ZMAX-(18.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"J" = ',ZMAX-(18.0D0*DL1),' TO ',ZMAX-(19.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"K" = ',ZMAX-(19.0D0*DL1),' TO ',ZMAX-(20.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"L" = ',ZMAX-(20.0D0*DL1),' TO ',ZMAX-(21.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"M" = ',ZMAX-(21.0D0*DL1),' TO ',ZMAX-(22.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"N" = ',ZMAX-(22.0D0*DL1),' TO ',ZMAX-(23.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"O" = ',ZMAX-(23.0D0*DL1),' TO ',ZMAX-(24.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"P" = ',ZMAX-(24.0D0*DL1),' TO ',ZMAX-(25.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"Q" = ',ZMAX-(25.0D0*DL1),' TO ',ZMAX-(26.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"R" = ',ZMAX-(26.0D0*DL1),' TO ',ZMAX-(27.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"S" = ',ZMAX-(27.0D0*DL1),' TO ',ZMAX-(28.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"T" = ',ZMAX-(28.0D0*DL1),' TO ',ZMAX-(29.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"U" = ',ZMAX-(29.0D0*DL1),' TO ',ZMAX-(30.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"V" = ',ZMAX-(30.0D0*DL1),' TO ',ZMAX-(31.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"W" = ',ZMAX-(31.0D0*DL1),' TO ',ZMAX-(32.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"X" = ',ZMAX-(32.0D0*DL1),' TO ',ZMAX-(33.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"Y" = ',ZMAX-(33.0D0*DL1),' TO ',ZMAX-(34.0D0*DL1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)
     1        '"Z" = ',ZMAX-(34.0D0*DL1),' TO ',ZMAX-(35.0D0*DL1)
              CALL SHOWIT(0)
C
              WRITE(OUTLYNE,*)'UPPER LEFT HAND CORNER OF MAP IS -X, -Y'
              CALL SHOWIT(0)
              WRITE(OUTLYNE,*)'LOWER RIGHT HAND CORNER OF MAP IS +X, +Y'
              CALL SHOWIT(0)
              RETURN
          END IF
          END

      
C SUB OPDLOD.FOR
      SUBROUTINE OPDLOD
C
          USE SVDSUB
      
          IMPLICIT NONE
C
          EXTERNAL FF3
C
          INTEGER SSN,SM,NP2,MP,COUNT,I,J,N,M,II,IIP
C
          LOGICAL OPMAP
C
          COMMON/OPOPMP/OPMAP
C
          COMMON/PRSIZE/COUNT
C
C       THIS PASSES THE SOLUTION OF THE LEAST SQUARES PROBLEM
          COMMON/SOLU/X
C
          REAL*8 ACCUM(1:96,1:96),REFHT,CCOL(1:96),X(1:96)
          COMMON/ACDATA/ACCUM,CCOL
C
          INTEGER III,JJ,KK,ALLOERR
C
          REAL*8 DWW1,DWW2,DWW3,DWW4,
     1    TERM,FF3,RHO,THETA
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 W,V,U,B,XXX
          DIMENSION W(:),V(:,:),U(:,:),B(:),XXX(:)
C
          ALLOCATABLE :: W,V,U,B,XXX
          ALLOCATE (W(1:96),V(1:96,1:96),U(1:96,1:96),B(1:96),XXX(1:96)
     1    ,STAT=ALLOERR)
C
C       DWW1=REF SURF Y-COORDINATE
C       DWW2=REF SURF X-COORDINATE
C       DWW3=OPD VALUE
C       DWW4=INTENSITY VALUE
C
          IF(SQ.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        '"FITZERN" TAKES NO QUALIFIER OR ALPHANUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(U,W,V,XXX,B,STAT=ALLOERR)
              RETURN
          END IF
C
C
          IF(S3.EQ.1.OR.S2.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"FITZERN" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(U,W,V,XXX,B,STAT=ALLOERR)
              RETURN
          END IF
C
C     DEFAULT WAVELENGTH IS CONTROL WAVELENGTH
          IF(DF1.EQ.1) W1=SYSTEM1(11)
          IF(INT(W1).LT.1.OR.INT(W1).GT.10) THEN
              OUTLYNE=
     1        'NUMERIC WORD #1 MUST BE 1, 2, 3, 4, 5, 6, 7, 8, 9 OR 10'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(U,W,V,XXX,B,STAT=ALLOERR)
              RETURN
          END IF
          IF(.NOT.CPFNEXT) THEN
              OUTLYNE=
     1        'NO CAPFN EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(U,W,V,XXX,B,STAT=ALLOERR)
              RETURN
          END IF
     C
          OUTLYNE=
     1    'FITTING THE WAVEFRONT TO A 37 TERM FRINGE ZERNIKE'
          CALL SHOWIT(1)
          CCOL(1:96)=0.0D0
          CFTYPE(1:96)=0.0D0
          X(1:96)=0.0D0
          XXX(1:96)=0.0D0
          ACCUM(1:96,1:96)=0.0D0
C
C     DETERMINE THE REFERNCE APERTURE HEIGHT
C     OF THE REF SURFACE COORDINATES
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.DABS(ALENS(9,NEWREF))
     1    .LE.5.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  IF(ALENS(10,NEWREF).LE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  REFHT=DABS(ALENS(10,NEWREF))
              END IF
C        IPOLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                  REFHT=DABS(ALENS(14,NEWREF))
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              IF(PXTRAY(1,NEWREF).GE.PXTRAX(1,NEWREF)) THEN
                  REFHT=PXTRAY(1,NEWREF)
              ELSE
                  REFHT=PXTRAX(1,NEWREF)
              END IF
          END IF
C
          OPEN(UNIT=64,ACCESS='DIRECT',FILE=LIBSPO//'OPDDAT.DAT',
     1    FORM='UNFORMATTED',RECL=(10*NRECL),STATUS='UNKNOWN')
          CALL CLOSE_FILE(64,0)
C
C     OPEN OPDDAT.DAT
C
          OPEN(UNIT=64,ACCESS='DIRECT',FILE=LIBSPO//'OPDDAT.DAT',
     1    FORM='UNFORMATTED',RECL=(10*NRECL),STATUS='UNKNOWN')
          KK=1
          DO IIP=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IIP-1
              CALL SPOTIT(4)
C
              IF(DSPOT(16).EQ.W1.AND.DSPOT(12).NE.0.0D0.OR.
     1        DSPOT(16).EQ.W1.AND.DSPOT(12).EQ.0.0D0.AND.
     1        DSPOT(7).EQ.7.0D0) KK=KK+1
          END DO
          IF(KK.LT.37.0D0) THEN
C     NOT ENOUGH DATA FOR A FIT
              OUTLYNE=
     1        'NOT ENOUGH DATA POINTS EXIST FOR A RELIABLE FIT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'OR NO DATA WAS AVAILABLE AT THE SPECIFIED WAVELENGTH'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'TRY TRACING A DENSER GRID OF RAYS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(U,W,V,XXX,B,STAT=ALLOERR)
              RETURN
          END IF
C
          KK=1
          DO IIP=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IIP-1
              CALL SPOTIT(4)
C
              DWW1=DSPOT(6)/REFHT
              DWW2=DSPOT(5)/REFHT
              DWW3=DSPOT(4)/(TWOPII)
              DWW4=DSPOT(12)
C
              IF(DSPOT(16).EQ.W1) THEN
C
C     DATA FOR DESIRED WAVELENGTH, PROCEED
C
C       PROCEED WITH FITTING A TYPE 3 TYPE/SURFACE
                  RHO=DSQRT((DWW1**2)+(DWW2**2))
                  IF(DABS(DWW1).GE.DABS(((1.0D35)*DWW2))) THEN
                      IF(DWW1.GE.0.0D0) THETA=PII/2.0D0
                      IF(DWW1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(DWW1).EQ.0.0D0.AND.DABS(DWW2).EQ.0.0D0) THEN
                          THETA=0.0D0
                      ELSE
                          THETA=DATAN2(DWW1,DWW2)
                      END IF
                      IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
                  END IF
C
                  III=0
                  DO I=1,37
C       FILL THE COLLUMN ARRAY
                      III=III+1
C       CALCULATE COLUMN VECTOR
C       FUNCTION FF3 RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       WITHOUT THE GIVEN DEPENDENT VARIABLE VALUE
C       OF THE WEIGHT FUNCTION FACTORED IN. THIS IS
C       FOR THE STANDARD ZERNIKE. I RANGES FROM 1 TO 37
                      TERM= FF3(RHO,THETA,I)
                      TERM=TERM*DWW3*DWW4
                      CCOL(III)=CCOL(III)+TERM
                      JJ=0
                      DO J=1,37
C       DO THE APPROPRIATE ROW FOR COLUMN I
                          JJ=JJ+1
                          TERM= FF3(RHO,THETA,I)*FF3(RHO,THETA,J)
                          ACCUM(III,JJ)=ACCUM(III,JJ)+(DWW4*TERM)
                      END DO
                  END DO
                  COUNT=III
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C
                  KK=KK+1
                  WRITE(UNIT=64,REC=KK) DWW1,DWW2,DWW3,DWW4
              END IF
          END DO
C     KK IS THE NUMBER OF FITTED DATA ITEMS
          WRITE(UNIT=64,REC=1) KK
C
C       THE LARGE ARRAY WITH ACCUMULATED DATA IS
C       ACCUM. THE COLUMN VECTOR WITH FUNCTIONAL DATA
C       IS CCOL.
          MP=96
          NP2=96
          N=COUNT
          M=COUNT
          I=COUNT
          B(1:I)=CCOL(1:I)
          J=COUNT
          U(1:I,1:J)=ACCUM(1:I,1:J)
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
C
C       SIMPLE DATA FIT
          II=0
          DO I=1,37
              II=II+1
              CFTYPE(I)=XXX(II)
              X(I)=CFTYPE(I)
          END DO
          DO I=38,96
              II=II+1
              CFTYPE(I)=0.0D0
              X(I)=CFTYPE(I)
          END DO
          OPMAP=.TRUE.
          OUTLYNE=
     1    'FITTING COMPLETED'
          CALL SHOWIT(1)
          DEALLOCATE(U,W,V,XXX,B,STAT=ALLOERR)
          RETURN
      END
C SUB WAVESLP1.FOR
      SUBROUTINE WAVESLP1(DSPOTT,IITOT,JTYPE)

          USE SVDSUB
      
          IMPLICIT NONE
C
          EXTERNAL FF3
C
          INTEGER SSN,SM,NP2,MP,JTYPE,IITOT,COUNT,I,J,N,M,II,IIP
     1    ,ISS
C
          COMMON/PRSIZE/COUNT
C
C       THIS PASSES THE SOLUTION OF THE LEAST SQUARES PROBLEM
          COMMON/SOLU/X
          LOGICAL ERRR
C
          DOUBLE PRECISION ACCUM(1:96,1:96),REFHT,TERM1,TERM2,
     1    WAVEX,CCOL(1:96),DSPOTT(35,IITOT),X(1:96),SUML2,SUML4,WEIS
     2    ,VALL,VALX,VALY,WEI(1:10),LAM(1:10)
     3    ,DLLZX,DLLZY,TERM3
C
          COMMON/ACDATA/ACCUM,CCOL
C
          INTEGER III,JJ,ALLOERR
C
          DOUBLE PRECISION DWW1,DWW2,DWW3,DWW4,
     1    TERM,FF3,RHO,THETA
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
          DOUBLE PRECISION W,V,U,
     2    B,XXX,WV
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
C       DWW1=REF SURF Y-COORDINATE
C       DWW2=REF SURF X-COORDINATE
C       DWW3=OPD VALUE
C       DWW4=INTENSITY VALUE
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
C     DETERMINE THE REFERNCE APERTURE HEIGHT
C     OF THE REF SURFACE COORDINATES
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.DABS(ALENS(9,NEWREF))
     1    .LE.5.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  IF(ALENS(10,NEWREF).LE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  REFHT=DABS(ALENS(10,NEWREF))
              END IF
C        IPOLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                  REFHT=DABS(ALENS(14,NEWREF))
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              IF(PXTRAY(1,NEWREF).GE.PXTRAX(1,NEWREF)) THEN
                  REFHT=PXTRAY(1,NEWREF)
              ELSE
                  REFHT=PXTRAX(1,NEWREF)
              END IF
          END IF
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
          DO IIP=1,IITOT
              IF(DSPOTT(16,IIP).EQ.1.0D0)  WV=SYSTEM1(1)
              IF(DSPOTT(16,IIP).EQ.2.0D0)  WV=SYSTEM1(2)
              IF(DSPOTT(16,IIP).EQ.3.0D0)  WV=SYSTEM1(3)
              IF(DSPOTT(16,IIP).EQ.4.0D0)  WV=SYSTEM1(4)
              IF(DSPOTT(16,IIP).EQ.5.0D0)  WV=SYSTEM1(5)
              IF(DSPOTT(16,IIP).EQ.6.0D0)  WV=SYSTEM1(71)
              IF(DSPOTT(16,IIP).EQ.7.0D0)  WV=SYSTEM1(72)
              IF(DSPOTT(16,IIP).EQ.8.0D0)  WV=SYSTEM1(73)
              IF(DSPOTT(16,IIP).EQ.9.0D0)  WV=SYSTEM1(74)
              IF(DSPOTT(16,IIP).EQ.10.0D0) WV=SYSTEM1(75)
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
              IF(DSPOTT(17,IIP).NE.0.0D0
     1        .AND.DSPOTT(7,IIP).EQ.0.0D0.AND.DSPOTT(12,IIP).NE.0.0D0) THEN
                  DWW1=DSPOTT(6,IIP)/REFHT
                  DWW2=DSPOTT(5,IIP)/REFHT
                  DWW3=((DSPOTT(33,IIP)/(TWOPII)))*WV
                  DWW4=DSPOTT(12,IIP)
                  WAVEX=1
C
C       PROCEED WITH FITTING A TYPE 3 TYPE/SURFACE
                  RHO=DSQRT((DWW1**2)+(DWW2**2))
                  IF(DABS(DWW1).GE.DABS(((1.0D35)*DWW2))) THEN
                      IF(DWW1.GE.0.0D0) THETA=PII/2.0D0
                      IF(DWW1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(DWW1).EQ.0.0D0.AND.DABS(DWW2).EQ.0.0D0) THEN
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
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C                     ELSE
C     POINT DOSE NOT CONTRIBUTE THIS TIME
              END IF
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
          DO IIP=1,IITOT
              IF(DSPOTT(16,IIP).EQ.1.0D0)  WV=SYSTEM1(1)
              IF(DSPOTT(16,IIP).EQ.2.0D0)  WV=SYSTEM1(2)
              IF(DSPOTT(16,IIP).EQ.3.0D0)  WV=SYSTEM1(3)
              IF(DSPOTT(16,IIP).EQ.4.0D0)  WV=SYSTEM1(4)
              IF(DSPOTT(16,IIP).EQ.5.0D0)  WV=SYSTEM1(5)
              IF(DSPOTT(16,IIP).EQ.6.0D0)  WV=SYSTEM1(71)
              IF(DSPOTT(16,IIP).EQ.7.0D0)  WV=SYSTEM1(72)
              IF(DSPOTT(16,IIP).EQ.8.0D0)  WV=SYSTEM1(73)
              IF(DSPOTT(16,IIP).EQ.9.0D0)  WV=SYSTEM1(74)
              IF(DSPOTT(16,IIP).EQ.10.0D0) WV=SYSTEM1(75)
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
              DWW1=DSPOTT(6,IIP)/REFHT
              DWW2=DSPOTT(5,IIP)/REFHT
              DWW3=((DSPOTT(33,IIP)/(TWOPII)))*WV
              DWW4=DSPOTT(12,IIP)
              RHO=DSQRT((DWW1**2)+(DWW2**2))
              IF(DABS(DWW1).GE.DABS(((1.0D35)*DWW2))) THEN
                  IF(DWW1.GE.0.0D0) THETA=PII/2.0D0
                  IF(DWW1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(DWW1).EQ.0.0D0.AND.DABS(DWW2).EQ.0.0D0) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(DWW1,DWW2)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              TERM=0.0D0
              IF(JTYPE.EQ.3) THEN
                  TERM1=(X(2)*FF3(RHO,THETA,2))
     1            +(X(3)*FF3(RHO,THETA,3))
                  TERM2=0.0D0
              END IF
              IF(JTYPE.EQ.4) THEN
                  TERM1=(X(2)*FF3(RHO,THETA,2))
     1            +(X(3)*FF3(RHO,THETA,3))
                  TERM2=(X(1)*FF3(RHO,THETA,1))
     1            +(X(4)*FF3(RHO,THETA,4))
              END IF
              TERM3=TERM1+TERM2
              DWW3=DWW3-TERM3
              IF(DSPOTT(12,IIP).NE.0.0D0)
     1        DSPOTT(4,IIP)=(DWW3*(TWOPII))/WV
              IF(DSPOTT(12,IIP).EQ.0.0D0) DSPOTT(4,IIP)=0.0D0
          END DO
          IF(WAVEX.EQ.1) THEN
              DLLX=((X(2)*FF3(1.0D0,0.0D0,2))
     1        -(X(2)*FF3(1.0D0,(PII),2)))
              DLLY=((X(3)*FF3(1.0D0,(PII/2.0D0),3))
     1        -(X(3)*FF3(1.0D0,(3.0D0*PII/2.0D0),3)))
              IF(JTYPE.EQ.3)  DLLZ=0.0D0
              IF(JTYPE.EQ.4)  DLLZ=((X(1)*FF3(1.0D0,0.0D0,1))
     1        +(X(4)*FF3(1.0D0,0.0D0,4)))
              ERRR=.FALSE.
              MSG=.FALSE.
              CALL EXPDIAX(VALL,ERRR)
              IF(.NOT.ERRR) VALX=VALL
              IF(ERRR) VALX=EXDIAX
              ERRR=.FALSE.
              MSG=.FALSE.
              CALL EXPDIAY(VALL,ERRR)
              IF(.NOT.ERRR) VALY=VALL
              IF(ERRR) VALY=EXDIAY
              DLLX=-((DLLX/(2.0D0*VALX))*RREF)*DCOS(REFRY(11,NEWIMG))
              DLLY=-((DLLY/(2.0D0*VALY))*RREF)*DCOS(REFRY(12,NEWIMG))
              DLLZX=(DLLZ*2.0D0)/(DSIN(DATAN(1.0D0/(2.0D0*RBFNY)))**2)
              DLLZY=(DLLZ*2.0D0)/(DSIN(DATAN(1.0D0/(2.0D0*RBFNX)))**2)
              DLLZ=(DLLZX+DLLZY)/2.0D0
          END IF
C
          DEALLOCATE(U,W,V,XXX,B,STAT=ALLOERR)
          RETURN
      END
C SUB OPDLIS.FOR
      SUBROUTINE OPDLIS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE OPDLIS. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "OPDLIST" COMMAND FROM THE CMD LEVEL
C
          EXTERNAL FF3
C
          INTEGER DATCNT,COUNT,INUMBER,
     7    IDATA,I,II,IIIII
C
          LOGICAL OPMAP,OPEN64
C
          COMMON/OPOPMP/OPMAP
C
          COMMON/PRSIZE/COUNT
C
          COMMON/NUMDAT/DATCNT
C
          REAL*8 TERM,X(1:96),RHO,FF3,DDDATA(1:4),REFHT
C
          COMMON/SPIDAT/IDATA
C
          COMMON/SOLU/X
C
          REAL*8 THETA
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"OPDLIST" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(.NOT.OPMAP.OR..NOT.CPFNEXT) THEN
              OUTLYNE=
     1        'NO OPD MAP FIT COEFFICIENTS EXIST'
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
     1    .LE.5.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  IF(ALENS(10,NEWREF).LE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  REFHT=DABS(ALENS(10,NEWREF))
              END IF
C        IPOLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                  REFHT=DABS(ALENS(14,NEWREF))
              END IF
          ELSE
C       NO CLAP ON REF SURF.
              IF(PXTRAY(1,NEWREF).GE.PXTRAX(1,NEWREF)) THEN
                  REFHT=PXTRAY(1,NEWREF)
              ELSE
                  REFHT=PXTRAX(1,NEWREF)
              END IF
          END IF
C
C       PROCEED WITH LISTING
C
C       WRITE THE HEADING THEN THE DATA
          WRITE(OUTLYNE,1106)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1105)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1106)
          CALL SHOWIT(0)
C
          OPEN64=.FALSE.
          INQUIRE(FILE=LIBSPO//'OPDDAT.DAT',OPENED=OPEN64)
          IF(.NOT.OPEN64)
     1      OPEN(UNIT=64,ACCESS='DIRECT',FILE=LIBSPO//'OPDDAT.DAT',
     1      FORM='UNFORMATTED',RECL=(10*NRECL),STATUS='UNKNOWN')
          READ(UNIT=64,REC=1) INUMBER
C
          DO IIIII=1,INUMBER-1
              READ(UNIT=64,REC=IIIII+1)
     1        DDDATA(1),DDDATA(2),DDDATA(3),DDDATA(4)
C       PROCEED WITH EVALUATION
              II=0
              TERM=0.0D0
              RHO=DSQRT((DDDATA(1)**2)+(DDDATA(2)**2))
              IF(DABS(DDDATA(1)).GE.
     1        DABS(((1.0D35)*DDDATA(2)))) THEN
                  IF(DDDATA(1).GE.0.0D0) THETA=PII/2.0D0
                  IF(DDDATA(1).LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(DDDATA(1)).EQ.0.0D0.AND.DABS(DDDATA(2)).EQ.0.0D0)
     1            THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(DDDATA(1),DDDATA(2))
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              DO I=1,37
                  TERM=TERM+(X(I)*FF3(RHO,THETA,I))
              END DO
C       DO A LINE OF OUTPUT
              DDDATA(1)=DDDATA(1)*REFHT
              DDDATA(2)=DDDATA(2)*REFHT
              WRITE(OUTLYNE,1110) DDDATA(1),DDDATA(2),
     1        DDDATA(3),TERM,(DDDATA(3)-TERM)
              CALL SHOWIT(0)
          END DO
          CALL CLOSE_FILE(64,1)
          RETURN
C
 1110     FORMAT(D15.8,1X,D15.8,1X,D15.8,1X,D15.8,1X,D15.8)
 1106     FORMAT(1X)
 1105     FORMAT('   Y-COORD     ',1X,'   X-COORD     ',
     1    1X,' INPUT  (OPD)  ',1X,
     2    ' FITTED  (OPD) ',1X,'FITTING ERROR')
C
      END
C SUB WRTCOEFS.FOR
      SUBROUTINE WRTCOEFS
C
          IMPLICIT NONE
C
C       THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "LISTZERN" COMMAND FROM THE CMD LEVEL
C
          EXTERNAL FF3
C
          INTEGER DATCNT,COUNT,IDATA,I
C
          LOGICAL OPMAP
C
          COMMON/OPOPMP/OPMAP
C
          COMMON/PRSIZE/COUNT
C
          COMMON/NUMDAT/DATCNT
C
          REAL*8 X(1:96),FF3
C
          COMMON/SPIDAT/IDATA
C
          COMMON/SOLU/X
C
!        REAL*8 THETA
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"LISTZERN" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(.NOT.OPMAP.OR..NOT.CPFNEXT) THEN
              OUTLYNE=
     1        'NO OPD MAP FIT COEFFICIENTS EXIST'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C
C       PROCEED WITH LISTING
C
C       WRITE THE HEADING THEN THE DATA
          WRITE(OUTLYNE,1101)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1106)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1105)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1106)
          CALL SHOWIT(0)
C
          DO I=1,37
              WRITE(OUTLYNE,1110) I,X(I)
              CALL SHOWIT(0)
          END DO
          RETURN
C
 1110     FORMAT(6X,I2,7X,1X,D15.8)
 1106     FORMAT(1X)
 1101     FORMAT(' ZERNIKE COEFFICIENT LIST FROM LAST WAVEFRONT FIT')
 1105     FORMAT(' COEF. NUMBER  ',1X,'  COEF. VALUE  ')
      END
C SUB WRTREPORT.FOR
      SUBROUTINE WRTREPORT
C
          IMPLICIT NONE
C
C       THIS DOES THE LISTREPT COMMAND
C
          REAL*8 HIGHORD,VCF(1:37),EMN,NV
     1    ,VC1,VC2,VC3,VC4,VC5,VC6,VC7,VC8,VC9,X(1:96)
C
          LOGICAL OPMAP
C
          COMMON/OPOPMP/OPMAP
C
C
          COMMON/SOLU/X
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"LISTREPT" GENERATES A REPORT FOR 37-TERM FRINGE ZERNIKE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'WAVEFRONT FITTING RESULTS)'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"LISTREPT" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(.NOT.OPMAP.OR..NOT.CPFNEXT) THEN
              OUTLYNE=
     1        'NO OPD MAP FIT COEFFICIENTS EXIST'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          WRITE(OUTLYNE,6)
 6        FORMAT('  ')
          CALL SHOWIT(0)
          WRITE(OUTLYNE,5)
          CALL SHOWIT(0)
 5        FORMAT('37-TERN FRINGE ZERNIKE REPORT')
          WRITE(OUTLYNE,40)
 40       FORMAT
     1    ('RMS WAVEFRONT ERROR IN WAVES (FROM THE LAST WAVEFRONT MAP FIT)')
          CALL SHOWIT(0)
          NV=0.0D0
          EMN=2.0D0
          VCF(1)=(EMN/(2.0D0*(NV+1.0D0)))*(X(1)**2)
          NV=1.0D0
          EMN=1.0D0
          VCF(2)=(EMN/(2.0D0*(NV+1.0D0)))*(X(2)**2)
          NV=1.0D0
          EMN=1.0D0
          VCF(3)=(EMN/(2.0D0*(NV+1.0D0)))*(X(3)**2)
          NV=2.0D0
          EMN=2.0D0
          VCF(4)=(EMN/(2.0D0*(NV+1.0D0)))*(X(4)**2)
          NV=2.0D0
          EMN=1.0D0
          VCF(5)=(EMN/(2.0D0*(NV+1.0D0)))*(X(5)**2)
          NV=2.0D0
          EMN=1.0D0
          VCF(6)=(EMN/(2.0D0*(NV+1.0D0)))*(X(6)**2)
          NV=3.0D0
          EMN=1.0D0
          VCF(7)=(EMN/(2.0D0*(NV+1.0D0)))*(X(7)**2)
          NV=3.0D0
          EMN=1.0D0
          VCF(8)=(EMN/(2.0D0*(NV+1.0D0)))*(X(8)**2)
          NV=4.0D0
          EMN=2.0D0
          VCF(9)=(EMN/(2.0D0*(NV+1.0D0)))*(X(9)**2)
          NV=3.0D0
          EMN=1.0D0
          VCF(10)=(EMN/(2.0D0*(NV+1.0D0)))*(X(10)**2)
          NV=3.0D0
          EMN=1.0D0
          VCF(11)=(EMN/(2.0D0*(NV+1.0D0)))*(X(11)**2)
          NV=4.0D0
          EMN=1.0D0
          VCF(12)=(EMN/(2.0D0*(NV+1.0D0)))*(X(12)**2)
          NV=4.0D0
          EMN=1.0D0
          VCF(13)=(EMN/(2.0D0*(NV+1.0D0)))*(X(13)**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(14)=(EMN/(2.0D0*(NV+1.0D0)))*(X(14)**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(15)=(EMN/(2.0D0*(NV+1.0D0)))*(X(15)**2)
          NV=6.0D0
          EMN=2.0D0
          VCF(16)=(EMN/(2.0D0*(NV+1.0D0)))*(X(16)**2)
          NV=4.0D0
          EMN=1.0D0
          VCF(17)=(EMN/(2.0D0*(NV+1.0D0)))*(X(17)**2)
          NV=4.0D0
          EMN=1.0D0
          VCF(18)=(EMN/(2.0D0*(NV+1.0D0)))*(X(18)**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(19)=(EMN/(2.0D0*(NV+1.0D0)))*(X(19)**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(20)=(EMN/(2.0D0*(NV+1.0D0)))*(X(20)**2)
          NV=6.0D0
          EMN=1.0D0
          VCF(21)=(EMN/(2.0D0*(NV+1.0D0)))*(X(21)**2)
          NV=6.0D0
          EMN=1.0D0
          VCF(22)=(EMN/(2.0D0*(NV+1.0D0)))*(X(22)**2)
          NV=7.0D0
          EMN=1.0D0
          VCF(23)=(EMN/(2.0D0*(NV+1.0D0)))*(X(23)**2)
          NV=7.0D0
          EMN=1.0D0
          VCF(24)=(EMN/(2.0D0*(NV+1.0D0)))*(X(24)**2)
          NV=8.0D0
          EMN=1.0D0
          VCF(25)=(EMN/(2.0D0*(NV+1.0D0)))*(X(25)**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(26)=(EMN/(2.0D0*(NV+1.0D0)))*(X(26)**2)
          NV=5.0D0
          EMN=1.0D0
          VCF(27)=(EMN/(2.0D0*(NV+1.0D0)))*(X(27)**2)
          NV=6.0D0
          EMN=1.0D0
          VCF(28)=(EMN/(2.0D0*(NV+1.0D0)))*(X(28)**2)
          NV=6.0D0
          EMN=1.0D0
          VCF(29)=(EMN/(2.0D0*(NV+1.0D0)))*(X(29)**2)
          NV=7.0D0
          EMN=1.0D0
          VCF(30)=(EMN/(2.0D0*(NV+1.0D0)))*(X(30)**2)
          NV=7.0D0
          EMN=1.0D0
          VCF(31)=(EMN/(2.0D0*(NV+1.0D0)))*(X(31)**2)
          NV=8.0D0
          EMN=1.0D0
          VCF(32)=(EMN/(2.0D0*(NV+1.0D0)))*(X(32)**2)
          NV=8.0D0
          EMN=1.0D0
          VCF(33)=(EMN/(2.0D0*(NV+1.0D0)))*(X(33)**2)
          NV=9.0D0
          EMN=1.0D0
          VCF(34)=(EMN/(2.0D0*(NV+1.0D0)))*(X(34)**2)
          NV=9.0D0
          EMN=1.0D0
          VCF(35)=(EMN/(2.0D0*(NV+1.0D0)))*(X(35)**2)
          NV=10.0D0
          EMN=2.0D0
          VCF(36)=(EMN/(2.0D0*(NV+1.0D0)))*(X(36)**2)
          NV=12.0D0
          EMN=2.0D0
          VCF(37)=(EMN/(2.0D0*(NV+1.0D0)))*(X(37)**2)
          VC1=DSQRT(VCF(1))
          VC2=DSQRT(VCF(2))
          VC3=DSQRT(VCF(3))
          VC4=DSQRT(VCF(4))
          VC5=DSQRT(VCF(5))
          VC6=DSQRT(VCF(6))
          VC7=DSQRT(VCF(7))
          VC8=DSQRT(VCF(8))
          VC9=DSQRT(VCF(9))
          WRITE(OUTLYNE,100) VC1
          CALL SHOWIT(0)
 100      FORMAT('                   CONSTANT TERM = ',G20.12)
          WRITE(OUTLYNE,200) VC2
          CALL SHOWIT(0)
 200      FORMAT('                X-AXIS TILT TERM = ',G20.12)
          WRITE(OUTLYNE,300) VC3
          CALL SHOWIT(0)
 300      FORMAT('                Y-AXIS TILT TERM = ',G20.12)
          WRITE(OUTLYNE,400) VC4
          CALL SHOWIT(0)
 400      FORMAT('                      FOCUS TERM = ',G20.12)
          WRITE(OUTLYNE,500) VC5
          CALL SHOWIT(0)
 500      FORMAT('         0 OR 90 DEG ASTIG. TERM = ',G20.12)
          WRITE(OUTLYNE,600) VC6
          CALL SHOWIT(0)
 600      FORMAT('           +/-45 DEG ASTIG. TERM = ',G20.12)
          WRITE(OUTLYNE,700) VC9
          CALL SHOWIT(0)
 700      FORMAT('        3RD ORDER SPHERICAL TERM = ',G20.12)
          WRITE(OUTLYNE,800) VC8
          CALL SHOWIT(0)
 800      FORMAT('      3RD ORDER X-AXIS COMA TERM = ',G20.12)
          WRITE(OUTLYNE,900) VC7
          CALL SHOWIT(0)
 900      FORMAT('      3RD ORDER Y-AXIS COMA TERM = ',G20.12)
          HIGHORD=0.0D0
          DO I=10,37
              HIGHORD=HIGHORD+VCF(I)
          END DO
          WRITE(OUTLYNE,1000) DSQRT(HIGHORD)
          CALL SHOWIT(0)
 1000     FORMAT('              HIGHER ORDER TERMS = ',G20.12)
          HIGHORD=0.0D0
          DO I=1,37
              HIGHORD=HIGHORD+VCF(I)
          END DO
          WRITE(OUTLYNE,1100) DSQRT(HIGHORD)
          CALL SHOWIT(0)
 1100     FORMAT('      (TOTAL) ALL TERMS INCLUDED = ',G20.12)
          HIGHORD=0.0D0
          DO I=2,37
              HIGHORD=HIGHORD+VCF(I)
          END DO
          WRITE(OUTLYNE,1201) DSQRT(HIGHORD)
          CALL SHOWIT(0)
 1201     FORMAT('             (TOTAL) MINUS CONST = ',G20.12)
          HIGHORD=0.0D0
          DO I=4,37
              HIGHORD=HIGHORD+VCF(I)
          END DO
          WRITE(OUTLYNE,1200) DSQRT(HIGHORD)
          CALL SHOWIT(0)
 1200     FORMAT('      (TOTAL) MINUS CONST & TILT = ',G20.12)
          HIGHORD=0.0D0
          DO I=5,37
              HIGHORD=HIGHORD+VCF(I)
          END DO
          WRITE(OUTLYNE,1300) DSQRT(HIGHORD)
          CALL SHOWIT(0)
 1300     FORMAT('(TOTAL) MINUS CONST,TILT & FOCUS = ',G20.12)
C
          RETURN
      END
C SUB AWRTSUM.FOR
      SUBROUTINE AWRTSUM
C
          IMPLICIT NONE
C
C     THIS IS SUBROUTINE AWRTSUM.FOR.
C     THIS WRITES A SUMMED SPOT DIAGRAM TO THE FILE WHOSE
C     NAME IS THE QUALIFIER WORD AND WHOSE EXT IS JUST (ASC)
C     (ASCII VERSION)
C
          LOGICAL EXIS65,EXIS32
C
          INTEGER I,JKNN,FJKNN
C
          CHARACTER NAMER*8,FNAMER*8
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"AWRTSUM" WRITES A SUMMED SPOT DIAGRAM TO AN ASCII FILE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"AWRTSUM" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.1) THEN
              OUTLYNE='"AWRTSUM" TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          NAMER=WQ
          IF(SST.EQ.0) WS(1:8)='SPOTS   '
          JKNN=8
          DO I=8,1,-1
              IF(NAMER(I:I).NE.' ') THEN
                  JKNN=I
                  GO TO 20
              END IF
          END DO
 20       CONTINUE
          FNAMER=WS(1:8)
          FJKNN=8
          DO I=8,1,-1
              IF(FNAMER(I:I).NE.' ') THEN
                  FJKNN=I
                  GO TO 30
              END IF
          END DO
 30       CONTINUE
          EXIS32=.FALSE.
          INQUIRE(FILE=LIBSPO//FNAMER(1:FJKNN)//'.DAT',EXIST=EXIS32)
          IF(.NOT.EXIS32) THEN
              OUTLYNE=
     1        'SUMMED SPOT DIAGRAM DOES NOT EXIST TO WRITE TO DISK'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     SAVE THE SUMMED DIAGRAM TO THE NAMED FILE. IF THE NAME EXISTS,
C     OVERWRITE THE FILE
          EXIS65=.FALSE.
          INQUIRE(FILE=trim(HOME)//NAMER(1:JKNN)//'.ASC',EXIST=EXIS65)
          IF(EXIS65) THEN
              OPEN(UNIT=65,ACCESS='SEQUENTIAL',BLANK='NULL'
     1          ,FORM='FORMATTED',FILE=trim(HOME)//NAMER(1:JKNN)//'.ASC'
     2          ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(65,0)
          END IF
C     NOW OPEN FOR READING AND WRITING
          OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//
     1    FNAMER(1:FJKNN)//'.DAT',
     1    FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
C
          OPEN(UNIT=65,ACCESS='SEQUENTIAL',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE=trim(HOME)//NAMER(1:JKNN)//'.ASC'
     2      ,STATUS='UNKNOWN')
C
C     READ FROM FILE THEN WRITE TO NAMER.ASC
C
          READ(UNIT=32,REC=1) ITOT
          WRITE(UNIT=65,FMT=200) ITOT
C
          DO I=2,ITOT
              READ(UNIT=32,REC=I) DSPOT(1),DSPOT(2),DSPOT(3),DSPOT(4)
     1        ,DSPOT(5),DSPOT(6),DSPOT(7),DSPOT(8),DSPOT(9),DSPOT(10),
     2        DSPOT(11),DSPOT(12),DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16)
     3        ,DSPOT(17)
     4        ,DSPOT(18),DSPOT(19),DSPOT(20),DSPOT(21),DSPOT(22),DSPOT(23)
     5        ,DSPOT(24)
     6        ,DSPOT(25),DSPOT(26),DSPOT(27),DSPOT(28),DSPOT(29)
     6        ,DSPOT(30),DSPOT(31),DSPOT(32),DSPOT(33),DSPOT(34),DSPOT(35)
     6        ,DSPOT(36),DSPOT(37),DSPOT(38),DSPOT(39),DSPOT(40),DSPOT(41)
     6        ,DSPOT(42),DSPOT(43),DSPOT(44),DSPOT(45),DSPOT(46),DSPOT(47)
     6        ,DSPOT(48),DSPOT(49),DSPOT(50)
              WRITE(UNIT=65,FMT=100) DSPOT(1),DSPOT(2)
              WRITE(UNIT=65,FMT=100) DSPOT(3),DSPOT(4)
              WRITE(UNIT=65,FMT=100) DSPOT(5),DSPOT(6)
              WRITE(UNIT=65,FMT=100) DSPOT(7),DSPOT(8)
              WRITE(UNIT=65,FMT=100) DSPOT(9),DSPOT(10)
              WRITE(UNIT=65,FMT=100) DSPOT(11),DSPOT(12)
              WRITE(UNIT=65,FMT=100) DSPOT(13),DSPOT(14)
              WRITE(UNIT=65,FMT=100) DSPOT(15),DSPOT(16)
              WRITE(UNIT=65,FMT=100) DSPOT(17),DSPOT(18)
              WRITE(UNIT=65,FMT=100) DSPOT(19),DSPOT(20)
              WRITE(UNIT=65,FMT=100) DSPOT(21),DSPOT(22)
              WRITE(UNIT=65,FMT=100) DSPOT(23),DSPOT(24)
              WRITE(UNIT=65,FMT=100) DSPOT(25),DSPOT(26)
              WRITE(UNIT=65,FMT=100) DSPOT(27),DSPOT(28)
              WRITE(UNIT=65,FMT=100) DSPOT(29),DSPOT(30)
              WRITE(UNIT=65,FMT=100) DSPOT(31),DSPOT(32)
              WRITE(UNIT=65,FMT=100) DSPOT(33),DSPOT(34)
              WRITE(UNIT=65,FMT=100) DSPOT(35),DSPOT(36)
              WRITE(UNIT=65,FMT=100) DSPOT(37),DSPOT(38)
              WRITE(UNIT=65,FMT=100) DSPOT(39),DSPOT(40)
              WRITE(UNIT=65,FMT=100) DSPOT(41),DSPOT(42)
              WRITE(UNIT=65,FMT=100) DSPOT(43),DSPOT(44)
              WRITE(UNIT=65,FMT=100) DSPOT(45),DSPOT(46)
              WRITE(UNIT=65,FMT=100) DSPOT(47),DSPOT(48)
              WRITE(UNIT=65,FMT=100) DSPOT(49),DSPOT(50)
          END DO
 100      FORMAT(D23.15,1X,D23.15)
 200      FORMAT(I10)
C     FINISHED WRITING, CLOSE THE FILE NAMER.ASC
          CALL CLOSE_FILE(65,1)
          RETURN
      END
C SUB BWRTSUM.FOR
      SUBROUTINE BWRTSUM
C
          IMPLICIT NONE
C
C     THIS IS SUBROUTINE BWRTSUM.FOR.
C     THIS WRITES A SUMMED SPOT DIAGRAM TO THE FILE WHOSE
C     NAME IS THE QUALIFIER WORD AND WHOSE EXT IS JUST (SPD)
C     (BINARY VERSION)
C
          LOGICAL OPEN64,EXIS64,EXIS32
C
          INTEGER I,JKNN,FJKNN
C
          CHARACTER NAMER*8,FNAMER*8
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"BWRTSUM" WRITES A SUMMED SPOT DIAGRAM TO A BINARY FILE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"BWRTSUM" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.1) THEN
              OUTLYNE='"BWRTSUM" TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          NAMER=WQ
          IF(SST.EQ.0) WS(1:8)='SPOTS   '
          JKNN=8
          DO I=8,1,-1
              IF(NAMER(I:I).NE.' ') THEN
                  JKNN=I
                  GO TO 20
              END IF
          END DO
 20       CONTINUE
          FNAMER=WS(1:8)
          FJKNN=8
          DO I=8,1,-1
              IF(FNAMER(I:I).NE.' ') THEN
                  FJKNN=I
                  GO TO 30
              END IF
          END DO
 30       CONTINUE
          OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//
     1    FNAMER(1:FJKNN)//'.DAT',
     1    FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
          EXIS32=.FALSE.
          INQUIRE(FILE=LIBSPO//FNAMER(1:FJKNN)//'.DAT',EXIST=EXIS32)
          IF(.NOT.EXIS32) THEN
              OUTLYNE=
     1        'SUMMED SPOT DIAGRAM DOES NOT EXISTS TO WRITE TO DISK'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     SAVE THE SPOT DIAGRAM TO THE NAMED FILE. IF THE NAME EXISTS,
C     OVERWRITE THE FILE
          OPEN64=.FALSE.
          INQUIRE(FILE=trim(HOME)//NAMER(1:JKNN)//'.SPD',EXIST=EXIS64)
          IF(EXIS64) THEN
              OPEN(UNIT=64,ACCESS='DIRECT',
     1        FILE=trim(HOME)//NAMER(1:JKNN)//'.SPD',
     1        FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
              CALL CLOSE_FILE(64,0)
          END IF
C     NOW OPEN FOR WRITING
          OPEN(UNIT=64,ACCESS='DIRECT',
     1    FILE=trim(HOME)//NAMER(1:JKNN)//'.SPD',
     1    FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
C
          OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//
     1    FNAMER(1:FJKNN)//'.DAT',
     1    FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
C
C     READ FROM FILE THEN WRITE TO NAMER.SPD
C
          READ(UNIT=32,REC=1) ITOT
          WRITE(UNIT=64,REC=1) ITOT
C
          DO I=2,ITOT
              READ(UNIT=32,REC=I) DSPOT(1),DSPOT(2),DSPOT(3),DSPOT(4)
     1        ,DSPOT(5),DSPOT(6),DSPOT(7),DSPOT(8),DSPOT(9),DSPOT(10),
     2        DSPOT(11),DSPOT(12),DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16)
     3        ,DSPOT(17)
     4        ,DSPOT(18),DSPOT(19),DSPOT(20),DSPOT(21),DSPOT(22),DSPOT(23)
     5        ,DSPOT(24)
     6        ,DSPOT(25),DSPOT(26),DSPOT(27),DSPOT(28),DSPOT(29)
     6        ,DSPOT(30),DSPOT(31),DSPOT(32),DSPOT(33),DSPOT(34),DSPOT(35)
     6        ,DSPOT(36),DSPOT(37),DSPOT(38),DSPOT(39),DSPOT(40),DSPOT(41)
     6        ,DSPOT(42),DSPOT(43),DSPOT(44),DSPOT(45),DSPOT(46),DSPOT(47)
     6        ,DSPOT(48),DSPOT(49),DSPOT(50)
              WRITE(UNIT=64,REC=I) DSPOT(1),DSPOT(2),DSPOT(3),DSPOT(4)
     1        ,DSPOT(5),DSPOT(6),DSPOT(7),DSPOT(8),DSPOT(9),DSPOT(10),
     2        DSPOT(11),DSPOT(12),DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16)
     3        ,DSPOT(17)
     4        ,DSPOT(18),DSPOT(19),DSPOT(20),DSPOT(21),DSPOT(22),DSPOT(23)
     5        ,DSPOT(24)
     6        ,DSPOT(25),DSPOT(26),DSPOT(27),DSPOT(28),DSPOT(29)
     6        ,DSPOT(30),DSPOT(31),DSPOT(32),DSPOT(33),DSPOT(34),DSPOT(35)
     6        ,DSPOT(36),DSPOT(37),DSPOT(38),DSPOT(39),DSPOT(40),DSPOT(41)
     6        ,DSPOT(42),DSPOT(43),DSPOT(44),DSPOT(45),DSPOT(46),DSPOT(47)
     6        ,DSPOT(48),DSPOT(49),DSPOT(50)
          END DO
C     FINISHED WRITING, CLOSE THE FILE NAMER.SPD
          CALL CLOSE_FILE(64,1)
C
          RETURN
      END
C
C SUB AWRTSPOT.FOR
      SUBROUTINE AWRTSPOT
C
          IMPLICIT NONE
C
C     THIS IS SUBROUTINE AWRTSPOT.FOR.
C     THIS WRITES THE CURRENT SPOT DIAGRAM TO THE FILE WHOSE
C     NAME IS THE QUALIFIER WORD AND WHOSE EXT IS JUST (ASC)
C     (ASCII VERSION)
C
          LOGICAL OPEN65,EXIS65
C
          INTEGER I,JKNN
C
          CHARACTER NAMER*8
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"AWRTSPOT" WRITES THE CURRENT SPOT DIAGRAM TO AN ASCII FILE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"AWRTSPOT" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"AWRTSPOT" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.SPDEXT) THEN
              OUTLYNE='NO SPOT DIAGRAM EXISTS TO WRITE TO DISK'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          NAMER=WQ
          JKNN=8
          DO I=8,1,-1
              IF(NAMER(I:I).NE.' ') THEN
                  JKNN=I
                  GO TO 20
              END IF
          END DO
 20       CONTINUE
C
C     SAVE THE SPOT DIAGRAM TO THE NAMED FILE. IF THE NAME EXISTS,
C     OVERWRITE THE FILE
          OPEN65=.FALSE.
          INQUIRE(FILE=trim(HOME)//NAMER(1:JKNN)//'.ASC',EXIST=EXIS65)
          IF(EXIS65) THEN
              OPEN(UNIT=65,ACCESS='SEQUENTIAL',BLANK='NULL'
     1          ,FORM='FORMATTED',FILE=trim(HOME)//NAMER(1:JKNN)//'.ASC'
     2          ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(65,0)
          END IF
C     NOW OPEN FOR WRITING
          OPEN(UNIT=65,ACCESS='SEQUENTIAL',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE=trim(HOME)//NAMER(1:JKNN)//'.ASC'
     2      ,STATUS='UNKNOWN')
C     READ FROM FILE THEN WRITE TO NAMER.ASC
C
          WRITE(UNIT=65,FMT=200) ITOT
C
          DO I=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=I-1
              CALL SPOTIT(4)
              WRITE(UNIT=65,FMT=100) DSPOT(1),DSPOT(2)
              WRITE(UNIT=65,FMT=100) DSPOT(3),DSPOT(4)
              WRITE(UNIT=65,FMT=100) DSPOT(5),DSPOT(6)
              WRITE(UNIT=65,FMT=100) DSPOT(7),DSPOT(8)
              WRITE(UNIT=65,FMT=100) DSPOT(9),DSPOT(10)
              WRITE(UNIT=65,FMT=100) DSPOT(11),DSPOT(12)
              WRITE(UNIT=65,FMT=100) DSPOT(13),DSPOT(14)
              WRITE(UNIT=65,FMT=100) DSPOT(15),DSPOT(16)
              WRITE(UNIT=65,FMT=100) DSPOT(17),DSPOT(18)
              WRITE(UNIT=65,FMT=100) DSPOT(19),DSPOT(20)
              WRITE(UNIT=65,FMT=100) DSPOT(21),DSPOT(22)
              WRITE(UNIT=65,FMT=100) DSPOT(23),DSPOT(24)
              WRITE(UNIT=65,FMT=100) DSPOT(25),DSPOT(26)
              WRITE(UNIT=65,FMT=100) DSPOT(27),DSPOT(28)
              WRITE(UNIT=65,FMT=100) DSPOT(29),DSPOT(30)
              WRITE(UNIT=65,FMT=100) DSPOT(31),DSPOT(32)
              WRITE(UNIT=65,FMT=100) DSPOT(33),DSPOT(34)
              WRITE(UNIT=65,FMT=100) DSPOT(35),DSPOT(36)
              WRITE(UNIT=65,FMT=100) DSPOT(37),DSPOT(38)
              WRITE(UNIT=65,FMT=100) DSPOT(39),DSPOT(40)
              WRITE(UNIT=65,FMT=100) DSPOT(41),DSPOT(42)
              WRITE(UNIT=65,FMT=100) DSPOT(43),DSPOT(44)
              WRITE(UNIT=65,FMT=100) DSPOT(45),DSPOT(46)
              WRITE(UNIT=65,FMT=100) DSPOT(47),DSPOT(48)
              WRITE(UNIT=65,FMT=100) DSPOT(49),DSPOT(50)
          END DO
          WRITE(UNIT=65,FMT=101) REFRY(1,NEWOBJ)
          WRITE(UNIT=65,FMT=101) REFRY(2,NEWOBJ)
          WRITE(UNIT=65,FMT=101) REFRY(3,NEWOBJ)
          WRITE(UNIT=65,FMT=101) REFRY(4,NEWOBJ)
          WRITE(UNIT=65,FMT=101) REFRY(5,NEWOBJ)
          WRITE(UNIT=65,FMT=101) REFRY(6,NEWOBJ)
          WRITE(UNIT=65,FMT=101) REFRY(32,NEWOBJ)
 100      FORMAT(D23.15,1X,D23.15)
 101      FORMAT(D23.15)
 200      FORMAT(I5)
C     FINISHED WRITING, CLOSE THE FILE NAMER.ASC
          CALL CLOSE_FILE(65,1)
          RETURN
      END
C SUB BWRTSPOT.FOR
      SUBROUTINE BWRTSPOT
C
          IMPLICIT NONE
C
C     THIS IS SUBROUTINE BWRTSPOT.FOR.
C     THIS WRITES THE CURRENT SPOT DIAGRAM TO THE FILE WHOSE
C     NAME IS THE QUALIFIER WORD AND WHOSE EXT IS JUST (SPD)
C     (BINARY VERSION)
C
          LOGICAL OPEN64,EXIS64
C
          INTEGER I,JKNN
C
          CHARACTER NAMER*8
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"BWRTSPOT" WRITES THE CURRENT SPOT DIAGRAM TO A BINARY FILE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"BWRTSPOT" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"BWRTSPOT" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.SPDEXT) THEN
              OUTLYNE='NO SPOT DIAGRAM EXISTS TO WRITE TO DISK'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          NAMER=WQ
          JKNN=8
          DO I=8,1,-1
              IF(NAMER(I:I).NE.' ') THEN
                  JKNN=I
                  GO TO 20
              END IF
          END DO
 20       CONTINUE
C
C     SAVE THE SPOT DIAGRAM TO THE NAMED FILE. IF THE NAME EXISTS,
C     OVERWRITE THE FILE
          OPEN64=.FALSE.
          INQUIRE(FILE=trim(HOME)//NAMER(1:JKNN)//'.SPD',EXIST=EXIS64)
          IF(EXIS64) THEN
              OPEN(UNIT=64,ACCESS='DIRECT',
     1        FILE=trim(HOME)//NAMER(1:JKNN)//'.SPD',
     1        FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
              CALL CLOSE_FILE(64,0)
          END IF
C     NOW OPEN FOR WRITING
          OPEN(UNIT=64,ACCESS='DIRECT',
     1    FILE=trim(HOME)//NAMER(1:JKNN)//'.SPD',
     1    FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
C     READ FROM FILE THEN WRITE TO NAMER.SPD
C
          WRITE(UNIT=64,REC=1) ITOT
C
          DO I=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=I-1
              CALL SPOTIT(4)
              WRITE(UNIT=64,REC=I) DSPOT(1),DSPOT(2),DSPOT(3),DSPOT(4)
     1        ,DSPOT(5),DSPOT(6),DSPOT(7),DSPOT(8),DSPOT(9),DSPOT(10),
     2        DSPOT(11),DSPOT(12),DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16)
     3        ,DSPOT(17)
     4        ,DSPOT(18),DSPOT(19),DSPOT(20),DSPOT(21),DSPOT(22),DSPOT(23)
     5        ,DSPOT(24)
     6        ,DSPOT(25),DSPOT(26),DSPOT(27),DSPOT(28),DSPOT(29)
     6        ,DSPOT(30),DSPOT(31),DSPOT(32),DSPOT(33),DSPOT(34),DSPOT(35)
     6        ,DSPOT(36),DSPOT(37),DSPOT(38),DSPOT(39),DSPOT(40),DSPOT(41)
     6        ,DSPOT(42),DSPOT(43),DSPOT(44),DSPOT(45),DSPOT(46),DSPOT(47)
     6        ,DSPOT(48),DSPOT(49),DSPOT(50)
          END DO
          WRITE(UNIT=64,REC=ITOT+1)
     1    REFRY(1,NEWOBJ),REFRY(2,NEWOBJ),REFRY(3,NEWOBJ),REFRY(4,NEWOBJ),
     2    REFRY(5,NEWOBJ),REFRY(6,NEWOBJ),REFRY(32,NEWOBJ)
C     FINISHED WRITING, CLOSE THE FILE NAMER.SPD
          CALL CLOSE_FILE(64,1)
          RETURN
      END
C SUB STATT.FOR

      SUBROUTINE STATT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE STATT.FOR.
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"STATS" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.OR.STI.EQ.1) THEN
              IF(STATSP)       WRITE(OUTLYNE,100)
              IF(.NOT. STATSP) WRITE(OUTLYNE,200)
              CALL SHOWIT(0)
              RETURN
          ELSE
          END IF
          IF(WQ.NE.'FULL'.AND.WQ.NE.'MIN') THEN
              OUTLYNE=
     1        'INVALID QUALIFIER USED WITH "STATS"'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'FULL') STATSP=.TRUE.
          IF(WQ.EQ.'MIN') STATSP=.FALSE.
 100      FORMAT('SPOT DIAGRAM STATISTICS ARE CURRENTLY SET TO "FULL"')
 200      FORMAT('SPOT DIAGRAM STATISTICS ARE CURRENTLY SET TO "MIN"')
C
          RETURN
      END
C SUB SPSAVE.FOR
      SUBROUTINE SPSAVE
C
          IMPLICIT NONE
C
          LOGICAL EXIS32,OPEN32
C
          INTEGER I,K,N
C
          REAL*8 SPT1,SPT2,SPT3,SPT4,SPT5,SPT6,SPT7,SPT8
     1    ,SPT9,SPT10
C
          COMMON/SPTWTS/SPT1,SPT2,SPT3,SPT4,SPT5
     1    ,SPT6,SPT7,SPT8,SPT9,SPT10
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"SPDSAVE" OR "SPDADD" TAKE NO'
              CALL SHOWIT(1)
              OUTLYNE='STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF1.EQ.1) W1=1.0D0
          IF(SQ.EQ.0)  WQ='SPOTS'
C
          DO I=8,1,-1
              IF(WQ(I:I).NE.' ') THEN
                  N=I
                  GO TO 10
              END IF
          END DO
 10       CONTINUE
C
C       SAVE
          IF(WC.EQ.'SPDSAVE') THEN
              EXIS32=.FALSE.
              INQUIRE(FILE=LIBSPO//WQ(1:N)//'.DAT',EXIST=EXIS32)
              IF(EXIS32) THEN
C       DELETE IT
                  OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//WQ(1:N)//'.DAT',
     1            FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
                  CALL CLOSE_FILE(32,0)
              END IF
              OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//WQ(1:N)//'.DAT',
     1        FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
C
              K=2
              IF(.NOT.SPDEXT) THEN
                  OUTLYNE='NO SPOT DIAGRAM EXISTS TO SAVE OR ADD'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SPDEXT) THEN
                  DO I=2,ITOT+1
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
                      ID=I-1
                      CALL SPOTIT(4)
                      DSPOT(12)=DSPOT(12)*W1
                      DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
                      IF(DSPOT(12).NE.0.0D0) THEN
                          WRITE(UNIT=32,REC=K)
     1                    DSPOT(1),DSPOT(2),DSPOT(3),DSPOT(4),
     1                    DSPOT(5),DSPOT(6),DSPOT(7),DSPOT(8),
     1                    DSPOT(9),DSPOT(10),DSPOT(11),DSPOT(12),
     1                    DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16),
     1                    DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16),
     1                    DSPOT(17),DSPOT(18),DSPOT(19),DSPOT(20),
     1                    DSPOT(21),DSPOT(22),DSPOT(23),DSPOT(24)
     6                    ,DSPOT(25),DSPOT(26),DSPOT(27),DSPOT(28),DSPOT(29)
     6                    ,DSPOT(30),DSPOT(31),DSPOT(32),DSPOT(33),DSPOT(34),DSPOT(35)
     6                    ,DSPOT(36),DSPOT(37),DSPOT(38),DSPOT(39),DSPOT(40),DSPOT(41)
     6                    ,DSPOT(42),DSPOT(43),DSPOT(44),DSPOT(45),DSPOT(46),DSPOT(47)
     6                    ,DSPOT(48),DSPOT(49),DSPOT(50)
C       COMPUT THE RECORD NUMBER OF THE NEXT RECORD TO BE WRITTEN
                          K=K+1
                      END IF
                  END DO
C       WRITE THE RECORD NUMBER OF THE LAST RECORD
                  WRITE(UNIT=32,REC=1) K-1
                  CALL CLOSE_FILE(32,1)
                  RETURN
              ELSE
              END IF
              IF(.NOT.SPDEXT) THEN
                  OUTLYNE='NO SPOT DIAGRAM EXISTS TO SAVE OR ADD'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
C       NOT SPDSAVE
          END IF
C       SPDADD
          IF(WC.EQ.'SPDADD') THEN
C
              EXIS32=.FALSE.
              INQUIRE(FILE=LIBSPO//WQ(1:N)//'.DAT',EXIST=EXIS32)
              IF(.NOT.EXIS32) THEN
C       NO FILE EXISTS, ERROR AND STOP
                  OUTLYNE='NO '//WQ(1:N)//'.DAT FILE EXISTS TO ADD ON TO'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       PROCEED
              END IF
C       SPOTS.DAT EXISTS, IF NOT OPEN, OPEN IT
              OPEN32=.FALSE.
              INQUIRE(FILE=LIBSPO//WQ(1:N)//'.DAT',OPENED=OPEN32)
              IF(.NOT.OPEN32) THEN
                  OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//WQ(1:N)//'.DAT',
     1            FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
              ELSE
C       IT WAS OPENED, PROCEED
              END IF
C
C       READ THE RECORD NUMBER OF THE LAST RECORD IN THE FILE
              READ(UNIT=32,REC=1) K
C       CREATE THE NEXT RECORD NUMBER TO WRITE
              K=K+1
C       ADD THE CURRENT SPOT DIAGRAM
              IF(.NOT.SPDEXT) THEN
                  OUTLYNE='NO SPOT DIAGRAM EXISTS TO SAVE OR ADD'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SPDEXT) THEN
                  DO I=2,ITOT+1
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
                      ID=I-1
                      CALL SPOTIT(4)
                      DSPOT(12)=DSPOT(12)*W1
                      DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
                      IF(DSPOT(12).NE.0.0D0) THEN
                          WRITE(UNIT=32,REC=K)
     1                    DSPOT(1),DSPOT(2),DSPOT(3),DSPOT(4),
     1                    DSPOT(5),DSPOT(6),DSPOT(7),DSPOT(8),
     1                    DSPOT(9),DSPOT(10),DSPOT(11),DSPOT(12),
     1                    DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16),
     1                    DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16),
     1                    DSPOT(17),DSPOT(18),DSPOT(19),DSPOT(20),
     1                    DSPOT(21),DSPOT(22),DSPOT(23),DSPOT(24)
     6                    ,DSPOT(25),DSPOT(26),DSPOT(27),DSPOT(28),DSPOT(29)
     6                    ,DSPOT(30),DSPOT(31),DSPOT(32),DSPOT(33),DSPOT(34),DSPOT(35)
     6                    ,DSPOT(36),DSPOT(37),DSPOT(38),DSPOT(39),DSPOT(40),DSPOT(41)
     6                    ,DSPOT(42),DSPOT(43),DSPOT(44),DSPOT(45),DSPOT(46),DSPOT(47)
     6                    ,DSPOT(48),DSPOT(49),DSPOT(50)
                          K=K+1
                      END IF
                  END DO
C       WRITE THE TOTAL NUMBER OF RECORD WRITTEN
                  WRITE(UNIT=32,REC=1) K-1
                  CALL CLOSE_FILE(32,1)
                  RETURN
              END IF
              IF(.NOT.SPDEXT) THEN
                  OUTLYNE='NO SPOT DIAGRAM EXISTS TO SAVE OR ADD'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
C       NOT SPDADD
          END IF
          RETURN
      END


C     SUB OPDLOD2.FOR
      SUBROUTINE OPDLOD2
          USE SVDSUB
C
          IMPLICIT NONE
C
          EXTERNAL FF3
          INTEGER SSN,SM,NP2,MP,COUNT,I,J,N,M,II,IIP
          LOGICAL OPMAP,ERROP
          COMMON/OPOPMP/OPMAP
          COMMON/PRSIZE/COUNT

C       THIS PASSES THE SOLUTION OF THE LEAST SQUARES PROBLEM
          COMMON/SOLU/X
C
          REAL*8 ACCUM(1:96,1:96),REFHT,
     1    CCOL(1:96),
     2    X(1:96)
C
          COMMON/ACDATA/ACCUM,CCOL
C
          INTEGER III,JJ,KK,ALLOERR,WVNUMOP
C
          REAL*8 DWW1,DWW2,DWW3,DWW4,
     1    TERM,FF3,RHO,THETA
C
          COMMON/OPDFIT1/ERROP
          COMMON/OPDFIT2/WVNUMOP
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 W,V,U,B,XXX
C
          DIMENSION W(:),V(:,:),U(:,:),B(:),XXX(:)
C
          ALLOCATABLE :: W,V,U,B,XXX
          ALLOCATE (W(1:96),V(1:96,1:96),U(1:96,1:96),B(1:96),XXX(1:96)
     1    ,STAT=ALLOERR)
C
C       DWW1=REF SURF Y-COORDINATE
C       DWW2=REF SURF X-COORDINATE
C       DWW3=OPD VALUE
C       DWW4=INTENSITY VALUE
C
C
          IF(.NOT.CPFNEXT) THEN
              CALL MACFAL
              DEALLOCATE(U,W,V,XXX,B,STAT=ALLOERR)
              ERROP=.TRUE.
              RETURN
          END IF
     C
          CCOL(1:96)=0.0D0
          CFTYPE(1:96)=0.0D0
          X(1:96)=0.0D0
          XXX(1:96)=0.0D0
          ACCUM(1:96,1:96)=0.0D0
C
C     DETERMINE THE REFERNCE APERTURE HEIGHT
C     OF THE REF SURFACE COORDINATES
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.DABS(ALENS(9,NEWREF))
     1    .LE.5.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  IF(ALENS(10,NEWREF).LE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=ALENS(10,NEWREF)
                  ELSE
                      REFHT=ALENS(11,NEWREF)
                  END IF
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                  REFHT=DSQRT((ALENS(10,NEWREF)**2)+(ALENS(11,NEWREF)**2))
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  REFHT=DABS(ALENS(10,NEWREF))
              END IF
C        IPOLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                  REFHT=DABS(ALENS(14,NEWREF))
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              IF(PXTRAY(1,NEWREF).GE.PXTRAX(1,NEWREF)) THEN
                  REFHT=PXTRAY(1,NEWREF)
              ELSE
                  REFHT=PXTRAX(1,NEWREF)
              END IF
          END IF
C
          OPEN(UNIT=64,ACCESS='DIRECT',FILE=LIBSPO//'OPDDAT.DAT',
     1    FORM='UNFORMATTED',RECL=(10*NRECL),STATUS='UNKNOWN')
          CALL CLOSE_FILE(64,0)
C
C     OPEN OPDDAT.DAT
C
          OPEN(UNIT=64,ACCESS='DIRECT',FILE=LIBSPO//'OPDDAT.DAT',
     1    FORM='UNFORMATTED',RECL=(10*NRECL),STATUS='UNKNOWN')
          KK=1
          DO IIP=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IIP-1
              CALL SPOTIT(4)
C
              IF(DSPOT(16).EQ.WVNUMOP.AND.DSPOT(12).NE.0.0D0.OR.
     1        DSPOT(16).EQ.WVNUMOP.AND.DSPOT(12).EQ.0.0D0.AND.
     1        DSPOT(7).EQ.7.0D0) KK=KK+1
          END DO
          IF(KK.LT.37.0D0) THEN
C     NOT ENOUGH DATA FOR A FIT
              CALL MACFAL
              DEALLOCATE(U,W,V,XXX,B,STAT=ALLOERR)
              ERROP=.TRUE.
              RETURN
          END IF
C
          KK=1
          DO IIP=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IIP-1
              CALL SPOTIT(4)
C
              DWW1=DSPOT(6)/REFHT
              DWW2=DSPOT(5)/REFHT
              DWW3=DSPOT(4)/(TWOPII)
              DWW4=DSPOT(12)
C
              IF(DSPOT(16).EQ.WVNUMOP) THEN
C
C     DATA FOR DESIRED WAVELENGTH, PROCEED
C
C       PROCEED WITH FITTING A TYPE 3 TYPE/SURFACE
                  RHO=DSQRT((DWW1**2)+(DWW2**2))
                  IF(DABS(DWW1).GE.DABS(((1.0D35)*DWW2))) THEN
                      IF(DWW1.GE.0.0D0) THETA=PII/2.0D0
                      IF(DWW1.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(DWW1).EQ.0.0D0.AND.DABS(DWW2).EQ.0.0D0) THEN
                          THETA=0.0D0
                      ELSE
                          THETA=DATAN2(DWW1,DWW2)
                      END IF
                      IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
                  END IF
C
                  III=0
                  DO I=1,37
C       FILL THE COLLUMN ARRAY
                      III=III+1
C       CALCULATE COLUMN VECTOR
C       FUNCTION FF3 RETURNS THE CORRECT VALUE
C       FOR THE ITH ENTRY IN THE COLUMN VECTOR
C       WITHOUT THE GIVEN DEPENDENT VARIABLE VALUE
C       OF THE WEIGHT FUNCTION FACTORED IN. THIS IS
C       FOR THE STANDARD ZERNIKE. I RANGES FROM 1 TO 37
                      TERM= FF3(RHO,THETA,I)
                      TERM=TERM*DWW3*DWW4
                      CCOL(III)=CCOL(III)+TERM
                      JJ=0
                      DO J=1,37
C       DO THE APPROPRIATE ROW FOR COLUMN I
                          JJ=JJ+1
                          TERM= FF3(RHO,THETA,I)*FF3(RHO,THETA,J)
                          ACCUM(III,JJ)=ACCUM(III,JJ)+(DWW4*TERM)
                      END DO
                  END DO
                  COUNT=III
C
C       ALL VALUES CALCULATED FOR THE CURRENT "DATA"
C       STATEMENT.
C
                  KK=KK+1
                  WRITE(UNIT=64,REC=KK) DWW1,DWW2,DWW3,DWW4
              END IF
          END DO
C     KK IS THE NUMBER OF FITTED DATA ITEMS
          WRITE(UNIT=64,REC=1) KK
C
C       THE LARGE ARRAY WITH ACCUMULATED DATA IS
C       ACCUM. THE COLUMN VECTOR WITH FUNCTIONAL DATA
C       IS CCOL.
          MP=96
          NP2=96
          N=COUNT
          M=COUNT
          I=COUNT
          B(1:I)=CCOL(1:I)
          J=COUNT
          U(1:I,1:J)=ACCUM(1:I,1:J)
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
C
C       SIMPLE DATA FIT
          II=0
          DO I=1,37
              II=II+1
              CFTYPE(I)=XXX(II)
              X(I)=CFTYPE(I)
          END DO
          DO I=38,96
              II=II+1
              CFTYPE(I)=0.0D0
              X(I)=CFTYPE(I)
          END DO
          OPMAP=.TRUE.
          DEALLOCATE(U,W,V,XXX,B,STAT=ALLOERR)
          ERROP=.FALSE.
          RETURN
      END
