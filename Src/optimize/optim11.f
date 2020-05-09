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

C       ELEVENTH SET OF OPTIMIZATION ROUTINES

      SUBROUTINE MONTEEND(NN1)
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          REAL*8 STATTOP(1:10),STATCOMP(1:10),STATCRIT(1:10)
     1    ,AVG,STDEV
C
          INTEGER I,NN1,KK,KKK
C
          INTEGER STATNUM
C
          COMMON/MONTESTAT/STATTOP,STATCOMP,STATCRIT,STATNUM
C
C     PRINT HEADER INFORMATION
C
          WRITE(OUTLYNE,300)
          CALL SHOWIT(0)
          WRITE(19,1300)
          WRITE(OUTLYNE,400)
          CALL SHOWIT(0)
          WRITE(19,1400)
          WRITE(OUTLYNE,401) NN1
          CALL SHOWIT(0)
          WRITE(19,1401)
 400      FORMAT(25X,'****** MONTE-CARLO SUMMARY ******')
 401      FORMAT('TOTAL NUMBER OF MONTE-CARLO CYCLES = ',I10)
 1400     FORMAT(25X,'****** MONTE-CARLO SUMMARY ******')
 1401     FORMAT('TOTAL NUMBER OF MONTE-CARLO CYCLES = ',I10)
C
          DO I=1,MAXTOP
              IF(ISTOP(I))THEN
                  AVG=STATTOP(I)/DBLE(NN1)
                  IF(NN1.EQ.1) THEN
                      STDEV=0.0D0
                  ELSE
                      STDEV=((STATTOP(I+5)-((STATTOP(I)**2)/DBLE(NN1)))/
     1                (DBLE(NN1)-1.0D0))
                      IF(STDEV.LT.0.0D0) STDEV=DSQRT(-STDEV)
                      IF(STDEV.GE.0.0D0) STDEV=DSQRT(STDEV)
                  END IF
 500              FORMAT('    FOR TOLERANCE OPERAND # ',I2)
 501              FORMAT('AVERAGE OPERAND CHANGE VALUE = ',G15.8)
 502              FORMAT('     WITH STANDARD DEVIATION = ',G15.8)
 1500             FORMAT('    FOR TOLERANCE OPERAND # ',I2)
 1501             FORMAT('AVERAGE OPERAND CHANGE VALUE = ',G15.8)
 1502             FORMAT('     WITH STANDARD DEVIATION = ',G15.8)
                  WRITE(OUTLYNE,500) I
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,501) AVG
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,502) STDEV
                  CALL SHOWIT(0)
                  WRITE(19,1500) I
                  WRITE(19,1501) AVG
                  WRITE(19,1502) STDEV
              END IF
          END DO
          DO KK=1,MAXCMP
              IF(ISCOMP(KK)) THEN
                  AVG=STATCOMP(KK)/DBLE(NN1)
                  IF(NN1.EQ.1) THEN
                      STDEV=0.0D0
                  ELSE
                      STDEV=((STATCOMP(KK+5)-((STATCOMP(KK)**2)/DBLE(NN1)))/
     1                (DBLE(NN1)-1.0D0))
                  END IF
                  IF(STDEV.LT.0.0D0) STDEV=DSQRT(-STDEV)
                  IF(STDEV.GE.0.0D0) STDEV=DSQRT(STDEV)
 600              FORMAT('          FOR COMPENSATOR # ',I2)
 601              FORMAT('AVERAGE COMPENSATOR VALUE = ',G15.8)
 602              FORMAT('  WITH STANDARD DEVIATION = ',G15.8)
 1600             FORMAT('          FOR COMPENSATOR # ',I2)
 1601             FORMAT('AVERAGE COMPENSATOR VALUE = ',G15.8)
 1602             FORMAT('  WITH STANDARD DEVIATION = ',G15.8)
                  WRITE(OUTLYNE,600) KK
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,601) AVG
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,602) STDEV
                  CALL SHOWIT(0)
                  WRITE(19,1600) KK
                  WRITE(19,1601) AVG
                  WRITE(19,1602) STDEV
              END IF
          END DO
          DO KKK=1,MAXFOCRIT
              IF(ISCRIT(KKK))THEN
                  AVG=STATCRIT(KKK)/DBLE(NN1)
                  IF(NN1.EQ.1) THEN
                      STDEV=0.0D0
                  ELSE
                      STDEV=((STATCRIT(KKK+5)-((STATCRIT(KKK)**2)/DBLE(NN1)))/
     1                (DBLE(NN1)-1.0D0))
                  END IF
                  IF(STDEV.LT.0.0D0) STDEV=DSQRT(-STDEV)
                  IF(STDEV.GE.0.0D0) STDEV=DSQRT(STDEV)
 700              FORMAT('               FOR FOCRIT # ',I2)
 701              FORMAT('             FOCRIT CHANGE= ',G15.8)
 702              FORMAT('  WITH STANDARD DEVIATION = ',G15.8)
 1700             FORMAT('           FOR FOCRIT # ',I2)
 1701             FORMAT('        FOCRIT CHANGE = ',G15.8)
 1702             FORMAT('  WITH STANDARD DEVIATION = ',G15.8)
                  WRITE(OUTLYNE,700) KKK
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,701) AVG
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,702) STDEV
                  CALL SHOWIT(0)
                  WRITE(19,1700) KKK
                  WRITE(19,1701) AVG
                  WRITE(19,1702) STDEV
              END IF
          END DO
C
          WRITE(OUTLYNE,300)
          CALL SHOWIT(0)
          WRITE(19,1300)
 300      FORMAT('**************************************************',
     1    '****************************')
 1300     FORMAT('**************************************************',
     1    '****************************')
          WRITE(OUTLYNE,100)
          CALL SHOWIT(0)
          WRITE(19,1100)
 100      FORMAT(18x,'****** MONTE-CARLO ANALYSIS COMPLETED ******')
 1100     FORMAT(18x,'****** MONTE-CARLO ANALYSIS COMPLETED ******')
C
C     CLOSE MONTEOUT.DAT FILE
C
          CALL CLOSE_FILE(19,1)
C
          RETURN
      END
      SUBROUTINE MONTEOUT
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          REAL*8 STATTOP(1:10),STATCOMP(1:10),STATCRIT(1:10)
C
          INTEGER STATNUM
C
          COMMON/MONTESTAT/STATTOP,STATCOMP,STATCRIT,STATNUM
C
          REAL*8 MOT(1:10)
C
          COMMON/LOCOMOTION/MOT
C
          LOGICAL CRITTERS
C
          REAL*8 CRITCENT(1:10),OCRIT(10),NCRIT(10),CRITCENT2(1:10)
C
          COMMON/ONCRIT/OCRIT,NCRIT
C
          COMMON/CENTCRIT/CRITCENT,CRITCENT2
C
          INTEGER I,II
C
          REAL*8 CHGVAL(10),V1,OPVAL(10)
C
          COMMON/OUTP2/CHGVAL,V1,OPVAL
C
C     THIS ROUTINE DOES THE MONTE-CARLO OUTPUT FOR ALL VARIABLES
C
C
 3701     FORMAT(A8,'::',A69)
 4701     FORMAT(A8,'::',A69)
          WRITE(OUTLYNE,300)
          CALL SHOWIT(0)
          WRITE(19,1300)
 300      FORMAT('**************************************************',
     1    '****************************')
 1300     FORMAT('**************************************************',
     1    '****************************')
C     NOW OUTPUT TOL OPERAND CHANGE VALUES AND THEIR OPTIONAL DESCRIPTIONS
 801      FORMAT('(TOLERANCE OPERAND VALUES)')
 1801     FORMAT('(TOLERANCE OPERAND VALUES)')
          WRITE(OUTLYNE,801)
          CALL SHOWIT(0)
          WRITE(19,1801)
 800      FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'*CHANGE = ',1X,G15.8)
 810      FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'* VALUE = ',1X,G15.8)
 1800     FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'*CHANGE = ',1X,G15.8)
 1810     FORMAT('TOLERANCE OPERAND(#',I3,') = ',
     1    A8,'* VALUE = ',1X,G15.8)
          DO I=1,MAXTOP
              II=I+MAXFOCRIT
              IF(ISTOP(I)) THEN
                  WRITE(OUTLYNE,800) I,OPNAM(II),CHGVAL(I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,810) I,OPNAM(II),OPVAL(I)
                  CALL SHOWIT(0)
                  WRITE(19,1800) I,OPNAM(II),CHGVAL(I)
                  WRITE(19,1810) I,OPNAM(II),OPVAL(I)
                  IF(OPERDESC(II)(1:8).NE.'        ')
     1            WRITE(OUTLYNE,3701) OPNAM(II),OPERDESC(II)(1:69)
                  CALL SHOWIT(0)
                  IF(OPERDESC(II)(1:8).NE.'        ')
     1            WRITE(19,4701) OPNAM(II),OPERDESC(II)(1:69)
              END IF
          END DO
C     ARE THERE COMPENSATORS
          CRITTERS=.FALSE.
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) CRITTERS=.TRUE.
          END DO
C
C     NOW OUTPUT COMPENSATOR MOTIONS
C
C     NOW OUTPUT FORCRIT CHANGE AND FOCRIT OPTIONAL DESCRIPTIONS
C
 802      FORMAT('(FOCUS CRITERIA (FOCRIT) DATA)')
 1802     FORMAT('(FOCUS CRITERIA (FOCRIT) DATA)')
          IF(CRITTERS) WRITE(OUTLYNE,802)
          CALL SHOWIT(0)
          IF(CRITTERS) WRITE(19,1802)
C 806  FORMAT('FOCRIT(#',I3,') = ',
C     1A8,'FOCRIT CHANGE = ',1X,G15.8)
C 1806 FORMAT('FOCRIT(#',I3,') = ',
C     1A8,'FOCRIT CHANGE = ',1X,G15.8)
C 807  FORMAT('FOCRIT(#',I3,') = ',
C     1A8,'FOCRIT %CHANGE = ',1X,G15.8)
C 1807 FORMAT('FOCRIT(#',I3,') = ',
C     1A8,'FOCRIT %CHANGE = ',1X,G15.8)
 806      FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'      FOCRIT STARTING VALUE = ',1X,G15.8)

 1806     FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'      FOCRIT STARTING VALUE = ',1X,G15.8)
 808      FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'FOCRIT FINAL RESTORED VALUE = ',1X,G15.8)

 1808     FORMAT('FOCRIT(#',I3,') = ',
     1    A8,'FOCRIT FINAL RESTORED VALUE = ',1X,G15.8)
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) THEN
C      WRITE(OUTLYNE,806) I,OPNAM(I),CRITCENT(I)
C      CALL SHOWIT(0)
C      WRITE(19,1806) I,OPNAM(I),CRITCENT(I)
C      WRITE(OUTLYNE,807) I,OPNAM(I),CRITCENT2(I)
C      CALL SHOWIT(0)
C      WRITE(19,1807) I,OPNAM(I),CRITCENT2(I)
                  WRITE(OUTLYNE,806) I,OPNAM(I),OCRIT(I)
                  CALL SHOWIT(0)
                  WRITE(19,1806) I,OPNAM(I),NCRIT(I)
                  WRITE(OUTLYNE,808) I,OPNAM(I),OCRIT(I)
                  CALL SHOWIT(0)
                  WRITE(19,1808) I,OPNAM(I),NCRIT(I)
                  IF(OPERDESC(I)(1:8).NE.'        ')
     1            WRITE(OUTLYNE,3701) OPNAM(I),OPERDESC(I)(1:69)
                  CALL SHOWIT(0)
                  IF(OPERDESC(I)(1:8).NE.'        ')
     1            WRITE(19,4701) OPNAM(I),OPERDESC(I)(1:69)
              END IF
          END DO
 901      FORMAT('(COMPENSATOR VARIABLE DATA)')
 1901     FORMAT('(COMPENSATOR VARIABLE DATA)')
          IF(CRITTERS) WRITE(OUTLYNE,901)
          CALL SHOWIT(0)
          IF(CRITTERS) WRITE(19,1901)
 900      FORMAT('COMPENSATOR VARIABLE(#',I3,') = ',
     1    A8,'MOTION = ',1X,G15.8)
 1900     FORMAT('COMPENSATOR VARIABLE(#',I3,') = ',
     1    A8,'MOTION = ',1X,G15.8)
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) THEN
                  WRITE(OUTLYNE,900) I,VARNAM(I),MOT(I)
                  CALL SHOWIT(0)
                  WRITE(19,1900) I,VARNAM(I),MOT(I)
              END IF
          END DO
C
C     NOW DO STATISTICAL ADDITIONS
          STATNUM=STATNUM+1
          DO I=1,MAXTOP
              IF(ISTOP(I)) STATTOP(I)=STATTOP(I)+CHGVAL(I)
              IF(ISTOP(I)) STATTOP(I+5)=STATTOP(I+5)+(CHGVAL(I)**2)
          END DO
          DO I=1,MAXCMP
              IF(ISCOMP(I)) STATCOMP(I)=STATCOMP(I)+MOT(I)
              IF(ISCOMP(I)) STATCOMP(I+5)=STATCOMP(I+5)+(MOT(I)**2)
          END DO
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) STATCRIT(I)=STATCRIT(I)+CRITCENT(I)
              IF(ISCRIT(I)) STATCRIT(I+5)=STATCRIT(I+5)+(CRITCENT(I)**2)
          END DO
C
          RETURN
      END
      SUBROUTINE MONTEHDR
          IMPLICIT NONE
C
!      LOGICAL NOPRT
C
!      INTEGER I,II
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
C     INITIALIZE THE MONTEOUT.DAT FILE
C     WHICH IS INITIALIZED THE SAME AS EDITTEXT.DAT WITH APPEND TRUE
C     NOW OPEN MONTEOUT.DAT
          OPEN(UNIT=19,ACCESS='APPEND',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE='MONTEOUT.DAT'
     2      ,STATUS='UNKNOWN')
C
C     PRINT HEADER INFORMATION
          WRITE(OUTLYNE,100)
          CALL SHOWIT(0)
          WRITE(19,1100)
 100      FORMAT(23x,'****** MONTE-CARLO ANALYSIS ******')
 1100     FORMAT(23x,'****** MONTE-CARLO ANALYSIS ******')
          WRITE(OUTLYNE,101)
          CALL SHOWIT(0)
          WRITE(19,1101)
 101      FORMAT(' ')
 1101     FORMAT(' ')
C
          RETURN
      END
C SUB MONTE.FOR
      SUBROUTINE MONTE
C
          IMPLICIT NONE
C
          INTEGER KKK,I,J,VTYPE,II
C
          LOGICAL TORRY
C
          REAL*8 GASDEV
C
          LOGICAL COMPYES,TOLYES
C
          COMMON/YESCOMP/COMPYES
C
          COMMON/YESTOL/TOLYES
C
          REAL*8 RANDXX,MWW1,MWW2,OLDVAL,CHGVAL(1:10),V1
     1    ,OPVAL(1:10)
C
          COMMON/OUTP2/CHGVAL,V1,OPVAL
C
          LOGICAL PLL
C
          COMMON/PLLPLL/PLL
C
          REAL*8 STATTOP(1:10),STATCOMP(1:10),STATCRIT(1:10)
     1    ,RESLT
C
          INTEGER STATNUM
C
          INTEGER ALLOERR,AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
C
          CHARACTER LICA*80,LIA*80,GLANMA*13,ALBL*80,LLTYPEA*80,INNIA*80
C
          REAL*8 SYSA,ALENA,SLVA,PIKA,FT01A,MULTCLAPA,MULTCOBSA
C
          REAL*8 AIPOLYX,AIPOLYY
C
          DIMENSION SYSA(:),ALENA(:,:),SLVA(:,:),PIKA(:,:,:),
     1    FT01A(:,:),LICA(:),GLANMA(:,:),ALBL(:),MULTCLAPA(:,:,:)
     2    ,MULTCOBSA(:,:,:),AIPOLYX(:,:,:),AIPOLYY(:,:,:)
C
          ALLOCATABLE :: SYSA,ALENA,SLVA,PIKA,FT01A,LICA,GLANMA,ALBL,
     1    MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY
C
          COMMON/MONTESTAT/STATTOP,STATCOMP,STATCRIT,STATNUM
C
          REAL*8 OCRIT(10),NCRIT(10)
C
          COMMON/ONCRIT/OCRIT,NCRIT
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
C       THIS IS SUBROUTINE MONTE. THIS IS THE SUBROUTINE WHICH
C       HANDLES A MONTE-CARLO ANALYSIS
C
C     PREPARE THE ARCHIEVE ARRAYS
C
C     ALLOCATE
          AM1=SSIZ
          AM2=LSIZ
          AM3=MAXSUR
          AM4=PSIZ
          AM5=0
          AM6=9
          AM7=6
          AM8=96
          AM9=2
          AM10=0
          DEALLOCATE(SYSA,ALENA,SLVA
     1    ,PIKA,FT01A,LICA,AIPOLYX,AIPOLYY,
     2    GLANMA,ALBL,MULTCLAPA,MULTCOBSA,STAT=ALLOERR)
          ALLOCATE(SYSA(AM1),ALENA(AM2,AM10:AM3),SLVA(AM5:AM6,AM10:AM3)
     1    ,PIKA(AM7,AM10:AM3,AM4),FT01A(AM8,AM10:AM3),LICA(AM6),
     2    GLANMA(AM10:AM3,AM9),ALBL(AM10:AM3),
     3    MULTCLAPA(1:1000,1:3,0:AM3),MULTCOBSA(1:1000,1:3,0:499)
     4    ,AIPOLYX(1:200,AM10:AM3,1:4),AIPOLYY(1:200,AM10:AM3,1:4)
     4    ,STAT=ALLOERR)
C       BEFORE READING IN THE LENS LIBRARY, WE MUST DO SOME
C       THINGS WITH THE CURRENT LENS.
          SYSA(1:AM1)=0.0D0
          LICA(1:AM6)='                  '
          ALBL(AM10:AM3)(1:80)=' '
          ALENA(1:AM2,AM10:AM3)=0.0D0
          AIPOLYX(1:200,AM10:AM3,1:4)=0.0D0
          AIPOLYY(1:200,AM10:AM3,1:4)=0.0D0
          MULTCLAPA(1:1000,1:3,0:AM3)=0.0D0
          MULTCOBSA(1:1000,1:3,0:AM3)=0.0D0
          SLVA(AM5:AM6,AM10:AM3)=0.0D0
          PIKA(1:AM7,AM10:AM3,1:AM4)=0.0D0
          FT01A(1:AM8,AM10:AM3)=0.0D0
          GLANMA(AM10:AM3,1:AM9)='             '
C
C       NOW DELETE ALL BUT THE MAIN CFG
          SYSTEM1(50)=1.0D0
          SYSTEM1(56)=1.0D0
C
C
C       NOW SAVE LENS 1, MAIN CONFIG, TO THE ACHIEVE LENS STORAGE
          CALL CTOA(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1    ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2    ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
C
C     NOW WE CHECK FOR VALID INPUT
C     MONTE TAKES NO INPUT
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"MONTE" INITIATES A MONTE-CARLO ANALYSIS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"MONTE" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"MONTE" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) W1=1.0D0
          IF(DF2.EQ.1) W2=0.5D0
          IF(W1.LT.1.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #1 MUST BE GREATER THAN OR EQUAL TO 1.0'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LE.0.0D0.OR.W2.GE.1.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #2 MUST LIE BETWEEN 0.0 AND 1.0'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          MWW1=W1
          MWW2=W2
C
C     NOW DO MONTE
C     CHECK THAT STUFF EXISTS
          IF(TVBCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"MONTE" REQUIRES TOLERANCE VARIABLES TO BE DEFINED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          COMPYES=.FALSE.
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) THEN
C     SET COMPYES
C     COMPYES TRUE MEANS WE CALCULATE FOCRITS IN OPCALC
                  COMPYES=.TRUE.
              END IF
          END DO
C
          TOLYES=.FALSE.
          DO I=1,MAXTOP
              IF(ISTOP(I)) THEN
C     SET TOLOPS NOMINAL VALUES
C     COMPYES TRUE MEANS WE CALCULATE FOCRITS IN OPCALC
                  TOLYES=.TRUE.
              END IF
          END DO
          IF(.NOT.TOLYES.AND.TVBCNT.NE.0) THEN
              WRITE(OUTLYNE,*)
     1        '"MONTE" REQUIRES TOLERANCE OPERANDS TO BE DEFINED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(TVBCNT.EQ.0.AND.TOLYES) THEN
              WRITE(OUTLYNE,*)
     1        '"MONTE" REQUIRES TOLERANCE VARIABLES TO BE DEFINED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(TVBCNT.EQ.0.AND..NOT.TOLYES) THEN
              WRITE(OUTLYNE,*)
     1        '"MONTE" REQUIRES TOLERANCE VARIABLES AND OPERANDS TO BE DEFINED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(COMPYES) THEN
              OPCALC_TYPE=1
              CALL OPCALC
              IF(F31.EQ.0) RETURN
              CALL OPLOAD
              IF(F31.EQ.0) RETURN
C     THE TOLOP NOMINALS HAVE BEEN SET
              DO II=1,MAXFOCRIT
                  IF(ISCRIT(II)) OCRIT(II)=OPERND(II,4)
              END DO
          END IF
          IF(TOLYES) THEN
C     THERE ARE TOLERANCE OPERANDS, EVALUATE THEM AND SET
C     THE NOMINALS TO BE THE CURRENT VALUES. THIS IS DONE JUST ONCE.
              OPCALC_TYPE=2
              CALL OPCALC
              IF(F31.EQ.0) RETURN
              CALL OPLOAD
              IF(F31.EQ.0) RETURN
C     THE TOLOP NOMINALS HAVE BEEN SET
              DO II=MAXFOCRIT+1,MAXFOCRIT+MAXTOP
                  OLDOP(II,1:20)=OPERND(II,1:20)
              END DO
          END IF
C
C     SET FLAG THAT TRACKS IF COMPENSATION MUST BE DONE
          COMPYES=.FALSE.
          DO I=1,MAXFOCRIT
              IF(ISCOMP(I).AND.ISCRIT(I)) THEN
C     COMPYES TRUE MEANS WE CALCULATE FOCRITS IN OPCALC
                  COMPYES=.TRUE.
              END IF
          END DO
          IF(COMPYES) THEN
C     SET MAX COUNTERS FOR OPTIMIZATION
C                  NO DERIVATIVES EXIST
C     THERE ARE COMPENSATOR OPERANDS, EVALUATE THEM AND SET
C     THE TARGETS TO BE THE CURRENT VALUES. THIS IS DONE JUST ONCE.
              DO II=1,MAXCMP
                  OPERND(II,2)=OCRIT(II)
              END DO
          END IF
C     THE COMPENSATOR TARGETS HAVE BEEN SET
C
C     PRINT THE COMPENSATOR TARGETS
C
C     NOW ALL OPERANDS HAVE THEIR ORIGINAL VALUES OR TARGET VALUES
C     AND AT LEAST TVARS AND TOPERS EXIST
C
C     RESET STATISTICAL VARIABLES
          STATNUM=0
          STATTOP(1:10)=0.0D0
          STATCOMP(1:10)=0.0D0
          STATCRIT(1:10)=0.0D0
C
C     NOW DO A MONTE
C
C     WRITE OUT THE HEADER INFORMATION
C
          CALL MONTEHDR
C
C     WE CHANGE VARIABLES, THEN REFOCUS, THE RECALC ALL TOPERS AND
C     CALCULATE TOPER CHANGES. TOPER CHANGES,
C     FOCRIT CHANGE AND COMPVAR MOTION.
C
          DO KKK=1,INT(MWW1)
C     THIS IS THE MONTE LOOP
C     UPDATE THE LENS
              F6=1
              F1=0
              F22=0
              DO I=1+MAXCMP,TVBCNT+MAXCMP
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                  VTYPE=INT(VARABL(I,1))
                  IF(VTYPE.EQ.1.OR.VTYPE.EQ.2.OR.VTYPE.EQ.134.OR.VTYPE.EQ.135) THEN
C     SURFACE RADIUS OR CURVATURE
                      SURF=INT(VARABL(I,3))
                      SAVE_KDP(7)=SAVEINPT(7)
                      OLDVAL=ALENS(1,SURF)
                      IF(VTYPE.EQ.2.OR.VTYPE.EQ.135) WC='CV'
                      IF(VTYPE.EQ.1.OR.VTYPE.EQ.134) WC='RD'
                      IF(VTYPE.EQ.2.OR.VTYPE.EQ.135) WC='CV'
                      IF(VTYPE.EQ.1.OR.VTYPE.EQ.2) WQ='CENT'
                      IF(VTYPE.EQ.134.OR.VTYPE.EQ.135) WQ='DELTFR'
                      SQ=1
                      S1=1
                      S2=0
                      S3=0
                      S4=0
                      S5=0
                      DF1=0
                      DF2=1
                      DF3=1
                      DF4=1
                      DF5=1
                      SN=1
                      SST=0
                      CALL RANDGET(RESLT)
                      RANDXX=(2.0D0*RESLT)-1.0D0
                      W1=DABS(VARABL(I,8))*RANDXX
                      V1=W1
                      CALL LENUP
                      REST_KDP(7)=RESTINPT(7)
                  END IF
C
C     COMPOUND VARIABLES
C
                  IF(VTYPE.GE.154.AND.VTYPE.LE.164) THEN
                      SURF=INT(VARABL(I,3))
                      SAVE_KDP(7)=SAVEINPT(7)
                      IF(VTYPE.EQ.154) WC='DISP    '
                      IF(VTYPE.EQ.155) WC='DISP    '
                      IF(VTYPE.EQ.156) WC='DISP    '
                      IF(VTYPE.EQ.157) WC='STILT   '
                      IF(VTYPE.EQ.158) WC='STILT   '
                      IF(VTYPE.EQ.159) WC='STILT   '
                      IF(VTYPE.EQ.160) WC='BTILT   '
                      IF(VTYPE.EQ.161) WC='BTILT   '
                      IF(VTYPE.EQ.162) WC='BTILT   '
                      IF(VTYPE.EQ.163) WC='ROLL    '
                      IF(VTYPE.EQ.164) WC='ROLL    '
                      RANDXX=GASDEV()
                      IF(VTYPE.GE.157.AND.VTYPE.LE.162) THEN
C     DO A PIVOT
                          WQ='PIVOT'
                          SQ=1
                          S1=1
                          S2=1
                          S3=1
                          S4=0
                          S5=0
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=1
                          DF5=1
                          SN=1
                          SST=0
                          W1=VARABL(I,9)
                          W2=VARABL(I,10)
                          W3=VARABL(I,11)
                          CALL CONTRO
                      END IF
                      IF(VTYPE.EQ.154) THEN
                          WQ='        '
                          SQ=0
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=1
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=0
                          SN=1
                          SST=0
                          W1=VARABL(I,3)
                          W2=VARABL(I,7)
                          W3=DABS(VARABL(I,8))*RANDXX
                          W4=0.0D0
                          W5=0.0D0
                          V1=W3
                          CALL HEXDISP
                      END IF
                      IF(VTYPE.EQ.155) THEN
                          WQ='        '
                          SQ=0
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=1
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=0
                          SN=1
                          SST=0
                          W1=VARABL(I,3)
                          W2=VARABL(I,7)
                          W3=0.0D0
                          W4=DABS(VARABL(I,8))*RANDXX
                          W5=0.0D0
                          V1=W4
                          CALL HEXDISP
                      END IF
                      IF(VTYPE.EQ.156) THEN
                          WQ='        '
                          SQ=0
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=1
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=0
                          SN=1
                          SST=0
                          W1=VARABL(I,3)
                          W2=VARABL(I,7)
                          W3=0.0D0
                          W4=0.0D0
                          W5=DABS(VARABL(I,8))*RANDXX
                          V1=W5
                          CALL HEXDISP
                      END IF
                      IF(VTYPE.EQ.157) THEN
                          WQ='        '
                          SQ=0
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=0
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=1
                          SN=1
                          SST=0
                          W1=VARABL(I,3)
                          W2=DABS(VARABL(I,8))*RANDXX
                          W3=0.0D0
                          W4=0.0D0
                          W5=0.0D0
                          V1=W2
                          CALL HEXSTILT
                      END IF
                      IF(VTYPE.EQ.158) THEN
                          WQ='        '
                          SQ=0
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=0
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=1
                          SN=1
                          SST=0
                          W1=VARABL(I,3)
                          W2=0.0D0
                          W3=DABS(VARABL(I,8))*RANDXX
                          W4=0.0D0
                          W5=0.0D0
                          V1=W3
                          CALL HEXSTILT
                      END IF
                      IF(VTYPE.EQ.159) THEN
                          WQ='        '
                          SQ=0
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=0
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=1
                          SN=1
                          SST=0
                          W1=VARABL(I,3)
                          W2=0.0D0
                          W3=0.0D0
                          W4=DABS(VARABL(I,8))*RANDXX
                          W5=0.0D0
                          V1=W4
                          CALL HEXSTILT
                      END IF
                      IF(VTYPE.EQ.160) THEN
                          WQ='        '
                          SQ=0
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=1
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=0
                          SN=1
                          SST=0
                          W1=VARABL(I,3)
                          W2=VARABL(I,7)
                          W3=DABS(VARABL(I,8))*RANDXX
                          W4=0.0D0
                          W5=0.0D0
                          V1=W3
                          CALL HEXBTILT
                      END IF
                      IF(VTYPE.EQ.161) THEN
                          WQ='        '
                          SQ=0
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=1
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=0
                          SN=1
                          SST=0
                          W1=VARABL(I,3)
                          W2=VARABL(I,7)
                          W3=0.0D0
                          W4=DABS(VARABL(I,8))*RANDXX
                          W5=0.0D0
                          V1=W4
                          CALL HEXBTILT
                      END IF
                      IF(VTYPE.EQ.162) THEN
                          WQ='        '
                          SQ=0
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=1
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=0
                          SN=1
                          SST=0
                          W1=VARABL(I,3)
                          W2=VARABL(I,7)
                          W3=0.0D0
                          W4=0.0D0
                          W5=DABS(VARABL(I,8))*RANDXX
                          V1=W5
                          CALL HEXBTILT
                      END IF
                      IF(VTYPE.EQ.163) THEN
                          WQ='        '
                          SQ=0
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=1
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=0
                          SN=1
                          SST=0
                          W1=VARABL(I,3)
                          W2=VARABL(I,7)
                          W3=DABS(VARABL(I,8))*RANDXX
                          W4=0.0D0
                          W5=VARABL(I,12)
                          V1=W3
                          CALL HEXROLL
                      END IF
                      IF(VTYPE.EQ.164) THEN
                          WQ='        '
                          SQ=0
                          S1=1
                          S2=1
                          S3=1
                          S4=1
                          S5=1
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=0
                          SN=1
                          SST=0
                          W1=VARABL(I,3)
                          W2=VARABL(I,7)
                          W3=0.0D0
                          W4=DABS(VARABL(I,8))*RANDXX
                          W5=VARABL(I,12)
                          V1=W3
                          CALL HEXROLL
                      END IF
                      REST_KDP(7)=RESTINPT(7)
                  END IF
C
                  IF(VTYPE.GE.3.AND.VTYPE.LE.8.OR.VTYPE.GE.11.AND.VTYPE.LE.
     1            25.OR.VTYPE.EQ.75.OR.VTYPE.GE.124.AND.VTYPE.LT.133
     2            .OR.VTYPE.EQ.138.OR.VTYPE.EQ.139.OR.VTYPE.EQ.140.OR.
     3            VTYPE.GE.141.0D0.AND.VTYPE.LE.153.0D0) THEN
                      SURF=INT(VARABL(I,3))
                      SAVE_KDP(7)=SAVEINPT(7)
                      IF(VTYPE.EQ.4) WC='CC'
                      IF(VTYPE.EQ.3) WC='TH'
                      IF(VTYPE.EQ.5) WC='AD'
                      IF(VTYPE.EQ.6) WC='AE'
                      IF(VTYPE.EQ.7) WC='AF'
                      IF(VTYPE.EQ.8) WC='AG'
                      IF(VTYPE.EQ.11) WC='CCTOR'
                      IF(VTYPE.EQ.12) WC='ADTOR'
                      IF(VTYPE.EQ.13) WC='AETOR'
                      IF(VTYPE.EQ.14) WC='AFTOR'
                      IF(VTYPE.EQ.15) WC='AGTOR'
                      IF(VTYPE.EQ.16) WC='ALPHA'
                      IF(VTYPE.EQ.17) WC='BETA'
                      IF(VTYPE.EQ.18) WC='GAMMA'
                      IF(VTYPE.EQ.19) WC='XD'
                      IF(VTYPE.EQ.20) WC='YD'
                      IF(VTYPE.EQ.21) WC='N1'
                      IF(VTYPE.EQ.22) WC='N2'
                      IF(VTYPE.EQ.23) WC='N3'
                      IF(VTYPE.EQ.24) WC='N4'
                      IF(VTYPE.EQ.25) WC='N5'
                      IF(VTYPE.EQ.75) WC='AC'
                      IF(VTYPE.EQ.124) WC='N6'
                      IF(VTYPE.EQ.125) WC='N7'
                      IF(VTYPE.EQ.126) WC='N8'
                      IF(VTYPE.EQ.127) WC='N9'
                      IF(VTYPE.EQ.128) WC='N10'
                      IF(VTYPE.EQ.129) WC='AH'
                      IF(VTYPE.EQ.130) WC='AI'
                      IF(VTYPE.EQ.131) WC='AJ'
                      IF(VTYPE.EQ.132) WC='AK'
                      IF(VTYPE.EQ.133) WC='AL'
                      IF(VTYPE.EQ.138) WC='ZD'
                      IF(VTYPE.EQ.139) WC='INDEX'
                      IF(VTYPE.EQ.140) WC='VNUM'
                      IF(VTYPE.EQ.141) WC='PIVX'
                      IF(VTYPE.EQ.142) WC='PIVY'
                      IF(VTYPE.EQ.143) WC='PIVZ'
                      IF(VTYPE.EQ.144) WC='DPART'
                      IF(VTYPE.EQ.145) WC='CLPX '
                      IF(VTYPE.EQ.146) WC='CLPY '
                      IF(VTYPE.EQ.147) WC='GDX  '
                      IF(VTYPE.EQ.148) WC='GDY  '
                      IF(VTYPE.EQ.149) WC='GDZ  '
                      IF(VTYPE.EQ.150) WC='GALPHA'
                      IF(VTYPE.EQ.151) WC='GBETA'
                      IF(VTYPE.EQ.152) WC='GGAMMA'
                      IF(VTYPE.EQ.153) WC='GRS'
                      IF(VTYPE.EQ.4) RANDXX=GASDEV()
                      IF(VTYPE.EQ.3) CALL RANDGET(RESLT)
                      IF(VTYPE.EQ.3) RANDXX=(2.0D0*RESLT)-1.0D0
                      IF(VTYPE.GE.5.AND.VTYPE.LE.8) RANDXX=GASDEV()
                      IF(VTYPE.GE.11.AND.VTYPE.LE.20) RANDXX=GASDEV()
                      IF(VTYPE.EQ.138) RANDXX=GASDEV()
                      IF(VTYPE.GE.21.AND.VTYPE.LE.25) CALL RANDGET(RESLT)
                      IF(VTYPE.GE.21.AND.VTYPE.LE.25) RANDXX=(2.0D0*RESLT)-1.0D0
                      IF(VTYPE.GE.124.AND.VTYPE.LE.128) CALL RANDGET(RESLT)
                      IF(VTYPE.GE.124.AND.VTYPE.LE.128) RANDXX=(2.0D0*RESLT)-1.0D0
                      IF(VTYPE.GE.139.AND.VTYPE.LE.140) CALL RANDGET(RESLT)
                      IF(VTYPE.GE.139.AND.VTYPE.LE.140) RANDXX=(2.0D0*RESLT)-1.0D0
                      IF(VTYPE.EQ.75) RANDXX=GASDEV()
                      IF(VTYPE.GE.129.AND.VTYPE.LE.133) RANDXX=GASDEV()
                      IF(VTYPE.GE.147.AND.VTYPE.LE.153) RANDXX=GASDEV()
                      WQ='DELT'
                      SQ=1
                      S1=1
                      S2=0
                      S3=0
                      S4=0
                      S5=0
                      DF1=0
                      DF2=1
                      DF3=1
                      DF4=1
                      DF5=1
                      SN=1
                      SST=0
                      W1=DABS(VARABL(I,8))*RANDXX
                      V1=W1
                      CALL LENUP
                      REST_KDP(7)=RESTINPT(7)
                  END IF
C
                  IF(VTYPE.EQ.9.OR.VTYPE.EQ.10) THEN
C     TORIC RADIUS OR CURVATUE
                      SURF=INT(VARABL(I,3))
                      IF(ALENS(23,SURF).EQ.0.0D0) THEN
C     NOT TORIC SET AS SO
                          TORRY=.FALSE.
                          SAVE_KDP(7)=SAVEINPT(7)
                          WC='YTORIC'
                          SQ=0
                          S1=0
                          S2=0
                          S3=0
                          S4=0
                          S5=0
                          DF1=1
                          DF2=1
                          DF3=1
                          DF4=1
                          DF5=1
                          SN=0
                          SST=0
                          CALL LENUP
                          REST_KDP(7)=RESTINPT(7)

                      ELSE
                          TORRY=.TRUE.
                      END IF
                      SAVE_KDP(7)=SAVEINPT(7)
                      OLDVAL=ALENS(24,SURF)
                      IF(VTYPE.EQ.9.OR.VTYPE.EQ.136) WC='RDTOR'
                      IF(VTYPE.EQ.10.OR.VTYPE.EQ.137) WC='CVTOR'
                      IF(VTYPE.EQ.9.OR.VTYPE.EQ.10) WQ='CENT'
                      IF(VTYPE.EQ.136.OR.VTYPE.EQ.137) WQ='DELTFR'
                      SQ=1
                      S1=1
                      S2=0
                      S3=0
                      S4=0
                      S5=0
                      DF1=0
                      DF2=1
                      DF3=1
                      DF4=1
                      DF5=1
                      SN=1
                      SST=0
                      CALL RANDGET(RESLT)
                      RANDXX=(2.0D0*RESLT)-1.0D0
                      W1=DABS(VARABL(I,8))*RANDXX
                      V1=W1
                      CALL LENUP
                      IF(VTYPE.EQ.9.OR.VTYPE.EQ.136) THEN
                          REST_KDP(7)=RESTINPT(7)
                      END IF

                      IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
                          RANDXX=GASDEV()
                          V1=DABS(VARABL(I,8))*RANDXX
                          FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1+
     1                    FTFL01((VTYPE-26),INT(VARABL(I,3)))
                      END IF
                      IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
                          RANDXX=GASDEV()
                          V1=DABS(VARABL(I,8))*RANDXX
                          FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1+
     1                    FTFL01((VTYPE-27),INT(VARABL(I,3)))
                      END IF
                  END IF
              END DO
C     ALL CHANGES HAVE BEEN MADE TO LENS
              LNSTYP=3
              CALL LNSEOS
C     FOCUS COMPENSATION GOES HERE
              CALL FOCOMP
C     CALCULATE OPERANDS AND LOAD THEM
              OPCALC_TYPE=2
              CALL OPCALC
              IF(F31.EQ.0) RETURN
              CALL OPLOAD
              IF(F31.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE CHANGE
              DO J=MAXFOCRIT+1,MAXTOP+MAXFOCRIT
                  IF(ISTOP(J-MAXFOCRIT)) THEN
                      CHGVAL(J-MAXFOCRIT)=(OPERND(J,4)-OLDOP(J,4))
                      OPVAL(J-MAXFOCRIT)=OPERND(J,4)
C     LOOP TO NEXT CHGVAL
                  END IF
              END DO
              CALL MONTEOUT
C     WRITE FOOTER INFO AND CLOSE MONTEOUT.DAT
C
              CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1        ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2        ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
              F1=0
              F6=1
              F22=0
              LNSTYP=2
              CALL LNSEOS
C
C     END OF LOOP THROUGH MONTE CASES
          END DO
C
          CALL MONTEEND(INT(MWW1))
C
          RETURN
      END


      SUBROUTINE THRDLIM
          IMPLICIT NONE
C
C       THIS SETS GLOBAL TH AND RD LIMITS FOR OPTIMIZATION
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsub.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=WC(1:3)//' TAKES NO STRING OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=WC(1:3)//' TAKES NO NUMERIC WORD #2 TO #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0.OR.STI.EQ.1) THEN
              IF(WC.EQ.'MNT')
     1        WRITE(OUTLYNE,10) WC(1:3),THMINLIM
              IF(WC.EQ.'MXT')
     1        WRITE(OUTLYNE,10) WC(1:3),THMAXLIM
              IF(WC.EQ.'MPR')
     1        WRITE(OUTLYNE,10) WC(1:3),RDPOSLIM
              IF(WC.EQ.'MNR')
     1        WRITE(OUTLYNE,10) WC(1:3),RDNEGLIM
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC(1:3).EQ.'MNT') THEN
              THMINLIM=W1
          END IF
          IF(WC(1:3).EQ.'MXT') THEN
              THMAXLIM=W1
          END IF
          IF(WC(1:3).EQ.'MPR') THEN
              RDPOSLIM=W1
          END IF
          IF(WC(1:3).EQ.'NPR') THEN
              RDNEGLIM=W1
          END IF
 10       FORMAT(A3,' HAS A CURRENT VALUE = ',G23.15)
          RETURN
      END


C SUB OPLOAD.FOR
      SUBROUTINE OPLOAD
C
          IMPLICIT NONE
C
          INTEGER I,CNT_CNT,STRTCNT
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          IF(OPCALC_TYPE.EQ.1) STRTCNT=1
          IF(OPCALC_TYPE.EQ.1) CNT_CNT=MAXFOCRIT
          IF(OPCALC_TYPE.EQ.2) STRTCNT=MAXFOCRIT+1
          IF(OPCALC_TYPE.EQ.2) CNT_CNT=MAXTOP+MAXFOCRIT
          IF(OPCALC_TYPE.EQ.3) STRTCNT=1
          IF(OPCALC_TYPE.EQ.3) CNT_CNT=OPCNT

          DO I=STRTCNT,CNT_CNT
              IF(OPERND(I,1).EQ.0.0D0) THEN
C     OPERAND VALUE ALREADY LOADED IN OPCALC
C     JUST JUMP TO THE END OF THE LOOP
                  GO TO 666
              END IF
              IF(OPCALC_TYPE.EQ.1.AND..NOT.ISCRIT(I)) GO TO 666
              IF(OPCALC_TYPE.EQ.2.AND..NOT.ISTOP(I-MAXFOCRIT)) GO TO 666
              IF(OPERND(I,15).EQ.0.0D0) THEN
C     SET ORIGINAL VALUES AND CHANGE HLD TO COR WITH
C     ORIG VALUE AS TARGET
C     SET ORIGINAL VALUE
                  OPERND(I,3)=GPREG(INT(OPERND(I,8)))
                  IF(OPERND(I,13).EQ.10.0D0) THEN
C     HLD FOUND TO CONVERT
                      OPERND(I,2)=OPERND(I,3)
                      OPERND(I,13)=1.0D0
                  END IF
                  OPERND(I,15)=1.0D0
              END IF
C     SET PREVIOUS VALUE TO OLD CURRENT VALUE
              OPERND(I,5)=OPERND(I,4)
C     LOAD NEW CURRENT VALUE
              OPERND(I,4)=GPREG(INT(OPERND(I,8)))
C     CALCULATE NEW CHANGE VALUE
              OPERND(I,6)=OPERND(I,4)-OPERND(I,5)
C
C     NOW CALCULATE THE SQUARE ROOT OF THE CONTRIBUTION TO THE MERIT FUMCTION
C     WHICH IS THE THE:
C     CURRENT OPERAND VALUE-TARGET VALUE FOR THE OPERAND
C     MULTIPLIED BY THE SQUARE ROOT OF THE OPERAND WEIGHT
C
              IF(OPERND(I,13).EQ.0.0D0)
     1        OPERND(I,14)=0.0D0
C
              IF(OPERND(I,13).EQ.1.0D0) THEN
                  IF(OPERND(I,19).EQ.0.0D0)OPERND(I,14)=(OPERND(I,4)-OPERND(I,2))
                  IF(OPERND(I,14).EQ.0.0D0) OPERND(I,4)=OPERND(I,2)
                  IF(OPERND(I,19).EQ.0.0D0)
     1            OPERND(I,14)=((OPERND(I,7)))*(OPERND(I,14))
              END IF
C
              IF(OPERND(I,13).EQ.-2.0D0) THEN
                  IF(OPERND(I,4).LT.OPERND(I,2))THEN
                      IF(OPERND(I,19).EQ.0.0D0)OPERND(I,14)=(OPERND(I,4)-OPERND(I,2))
                      IF(OPERND(I,14).EQ.0.0D0) OPERND(I,4)=OPERND(I,2)
                      IF(OPERND(I,19).EQ.0.0D0)
     1                OPERND(I,14)=((OPERND(I,7)))*(OPERND(I,14))
                  END IF
                  IF(OPERND(I,4).GE.OPERND(I,2))
     1            OPERND(I,14)=0.0D0
              END IF
C
              IF(OPERND(I,13).EQ.2.0D0) THEN
                  IF(OPERND(I,4).GT.OPERND(I,2)) THEN
                      IF(OPERND(I,19).EQ.0.0D0)OPERND(I,14)=(OPERND(I,4)-OPERND(I,2))
                      IF(OPERND(I,14).EQ.0.0D0) OPERND(I,4)=OPERND(I,2)
                      IF(OPERND(I,19).EQ.0.0D0)
     1                OPERND(I,14)=((OPERND(I,7)))*(OPERND(I,14))
                  END IF
                  IF(OPERND(I,4).LE.OPERND(I,2))
     1            OPERND(I,14)=0.0D0
              END IF
C
 666          CONTINUE
          END DO
          RETURN
      END


      SUBROUTINE CLEARANCE(CLEAR,CLRTYP)
          IMPLICIT NONE
          INTEGER SURF1,SURF2,FOB1,FOB2,RAY1,RAY2,CLRTYP
          CHARACTER AFOB1*132,AFOB2*132,
     1    ARAY1*132,ARAY2*132,ASUR2*15
          REAL*8 CLEAR,GX,GY,GZ,GL,GM,GN,GX0,GY0,GZ0
          REAL*8 A,B,C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsub.inc'
          FOB1=INT(CLFOB1)
          FOB2=INT(CLFOB2)
          RAY1=INT(CLRAY1)
          RAY2=INT(CLRAY2)
          SURF1=INT(CLSRF1)
          SURF2=INT(CLSRF2)
C
          WRITE(ASUR2,10) SURF2
          WRITE(AFOB1,11)
     1    FIELDY(FOB1),FIELDX(FOB1),FIELDZ(FOB1),FIELDW(FOB1)
          WRITE(AFOB2,11)
     1    FIELDY(FOB2),FIELDX(FOB2),FIELDZ(FOB2),FIELDW(FOB2)
          WRITE(ARAY1,12)
     1    RAYY(RAY1),RAYX(RAY1),RAYW(RAY1)
          WRITE(ARAY2,12)
     1    RAYY(RAY2),RAYX(RAY2),RAYW(RAY2)
 10       FORMAT('GLOBAL,',I3)
 11       FORMAT('FOB ',G23.15,',',G23.15,',',G23.15,',',G23.15)
 12       FORMAT('RAY ',G23.15,',',G23.15,',',G23.15)
C       SET GLOBAL SURFACE TO SURF2
          SAVE_KDP(33)=SAVEINPT(33)
          INPUT=ASUR2
          CALL PROCES
          INPUT=AFOB1
          CALL PROCES
          INPUT=ARAY1
          CALL PROCES
          GX=GLRAY(1,SURF1)
          GY=GLRAY(2,SURF1)
          GZ=GLRAY(3,SURF1)
          GL=GLRAY(4,SURF1)
          GM=GLRAY(5,SURF1)
          GN=GLRAY(6,SURF1)
          INPUT=AFOB2
          CALL PROCES
          INPUT=ARAY2
          CALL PROCES
          GX0=GLRAY(1,SURF2)
          GY0=GLRAY(2,SURF2)
          GZ0=GLRAY(3,SURF2)
          IF(CLRTYP.EQ.1) THEN
C       CLEARX
C       CLEAR IS + IF THE POINT IS ABOVE THE LINE AND NEGATIVE
C       IF IT IS BELOW IT
              IF(GN.NE.0.0D0) THEN
                  A=1
                  B=-GL/GN
                  C=((GL/GN)*GZ)-GX
                  CLEAR=((A*GX0)+(B*GZ0)+C)/DSQRT((A**2)+(B**2))
              ELSE
C       CLEAR IS + IF THE POINT IS TO THE RIGHT OF THE LINE AND NEGATIVE
C       IF IT IS TO THE LEFT
                  CLEAR=GZ0-GZ
              END IF
          END IF
          IF(CLRTYP.EQ.2) THEN
C       CLEARY
C       CLEAR IS + IF THE POINT IS ABOVE THE LINE AND NEGATIVE
C       IF IT IS BELOW IT
              IF(GN.NE.0.0D0) THEN
                  A=1
                  B=-GM/GN
                  C=((GM/GN)*GZ)-GY
                  CLEAR=((GY0)+(B*GZ0)+C)/DSQRT((1.0D0+B**2))
              ELSE
C       CLEAR IS + IF THE POINT IS TO THE RIGHT OF THE LINE AND NEGATIVE
C       IF IT IS TO THE LEFT
                  CLEAR=GZ0-GZ
              END IF
          END IF
          REST_KDP(33)=RESTINPT(33)
          RETURN
      END
