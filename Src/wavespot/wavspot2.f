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

C       SECOND FILE OF CAPFN/SPOT ROUTINES

C SUB SUMSPREDSQ.FOR
      SUBROUTINE SUMSPREDSQ
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SUMSPREDSQ.FOR.
C     CALLED BY CMDER FOR COMMAND REDSUMSQ
C
          REAL*8 GDTARP(1:101),GDTAR(1:101)
C
          REAL*8 SPDELX,SPDELY,DELZ,MTHETA
C
          INTEGER ENNL,ALLOERR
C
          COMMON/REDPASS/ENNL,GDTARP,GDTAR,MTHETA,SPDELX,SPDELY,DELZ
C
          LOGICAL EXTKED,EXTRED,EXTREDSQ,CENTRED,EXIS32,SUMMS
C
          COMMON/REDEXTSQ/EXTREDSQ,CENTRED,SUMMS
C
          COMMON/REDEXT/EXTRED,EXTKED
C
          REAL*8 EMAX,YVAL,
     1    XV,YV,RED1,RED2,RED3,REDP1,REDP2,REDP3
C
          LOGICAL OPEN32
C
          INTEGER N,I,NER,M1,N1
C
          REAL*8 W1W
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
          DIMENSION LARGER(:,:)
          ALLOCATABLE :: LARGER
C
          EXTREDSQ=.FALSE.
          EXTRED=.FALSE.
          EXTKED=.FALSE.
          CENTRED=.FALSE.
          SUMMS=.TRUE.
          ENNL=0
          IF(SQ.EQ.0) WQ='SPOTS'
          DO I=8,1,-1
              IF(WQ(I:I).NE.' ') THEN
                  N=I
                  GO TO 10
              END IF
          END DO
 10       CONTINUE
C
C     STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"REDSUMSQ"'
              CALL SHOWIT(1)
              OUTLYNE='TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     NUMERIC WORD #1
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        '"REDSUMSQ"'
              CALL SHOWIT(1)
              OUTLYNE='REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1..OR.S4.EQ.1) THEN
              OUTLYNE=
     1        '"REDSUMSQ"'
              CALL SHOWIT(1)
              OUTLYNE='TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       PROCEED
C
          EXIS32=.FALSE.
          INQUIRE(FILE=LIBSPO//WQ(1:N)//'.DAT',EXIST=EXIS32)
          IF(.NOT.EXIS32) THEN
              OUTLYNE=
     1        'NO SAVED AND SUMMED SPOT DIAGRAM DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     HERE IS WHERE THE ENSQUARED ENERGY DIST DATA IS PROCESSED.
C     LOAD THE CURRENT SPOT DIAGRAM DATA INTO THE RED ARRAY
C     THE RED ARRAY HAS 2 COLUMNS. COLUMN 1 INITIALLY CONTAINS
C     THE RAY ENERGY TERM. COLUMN 2 EITHER HAS THE X,Y OR RADIAL
C     COMPONENT MINUS EITHER THE CORRESPONDING CHEIF RAY COMPONENT
C     OR THE CORRESPONDINIG COMPONENT OF THE SPOT CENTROID. IF
C     FOB NULL WAS USED, NO RED CAN STILL BE DONE.
C
          IF(W1.LT.0.0D0.OR.W1.GT.100.0D0) THEN
              OUTLYNE=
     1        'REQUESTED ENERGY MUST BE BETWEEN 0 AND 100 PERCENT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     SET REDVLX AND REDVLY TO THE CENTROID LOCATION
C
          CALL SUMSTAT
          REDVLX=CENTX
          REDVLY=CENTY
C
C     REDVLX AND REDVLY HAVE BEEN SET
C
C     NOW PROCEED WITH THE DATA LOADING
C
          DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
C
          IRED=1
C
          OPEN32=.FALSE.
          INQUIRE(FILE=LIBSPO//WQ(1:N)//'.DAT',OPENED=OPEN32)
          IF(.NOT.OPEN32)
     1      OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//WQ(1:N)//'.DAT',
     1      FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
          READ(UNIT=32,REC=1) ITOT
          ALLOCATE(REDDAT1(ITOT),REDDAT2(ITOT),REDDAT3(ITOT),STAT=ALLOERR)
          DO I=2,ITOT
              READ(UNIT=32,REC=I)
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
              IF(DSPOT(17).NE.0) THEN
                  IF(DSPOT(12).NE.0.0D0) THEN
                      IRED=IRED+1
                      RED1=DSPOT(12)
C
                      IF(SYSTEM1(30).LT.3.0D0) THEN
C     FOCAL
                          XV=DSPOT(1)
                          YV=DSPOT(2)
                      ELSE
C     AFOCAL
                          XV=(DSPOT(9))
                          YV=(DSPOT(10))
                      END IF
C
                      IF(SYSTEM1(30).LT.3.0D0) THEN
                          RED2=(XV-REDVLX)/COS_B_ANG
                          RED3=(YV-REDVLY)/COS_A_ANG
                      ELSE
                          RED2=XV-REDVLX
                          RED3=YV-REDVLY
                      END IF
C
                  ELSE
C     NO CALCULATION, WEIGHT WAS ZERO
                  END IF
              ELSE
C     NO CALCULATION, ENERGY WAS ZERO
              END IF

              REDDAT1(IRED)=RED1
              REDDAT2(IRED)=RED2
              REDDAT3(IRED)=RED3
          END DO
C
C     NOW THE ARRAY CALLED LARGER
C
          NER=IRED-1
C
          M1=IRED
          N1=3
          DEALLOCATE(LARGER,STAT=ALLOERR)
          ALLOCATE(LARGER(M1,N1),STAT=ALLOERR)
C
C     NOW FOR EACH COORDINATE VALUE, SUM ALL ENERGY AT IT
C     AND BELOW IT
          NUMBEROF=ITOT
          IF(IRED.GT.2) CALL REDSUMS(LARGER,M1,N1)
          DEALLOCATE(LARGER,STAT=ALLOERR)
          OPEN32=.FALSE.
          RED1=REDDAT1(IRED)
          RED2=REDDAT2(IRED)
          RED3=REDDAT3(IRED)
          EMAX=RED1
          DO I=2,IRED
              RED1=REDDAT1(I)
              RED2=REDDAT2(I)
              RED3=REDDAT3(I)
              RED1=RED1/EMAX
              REDDAT1(I)=RED1
              REDDAT2(I)=RED2
              REDDAT3(I)=RED3
          END DO
C     INTERPOLATE THE DATA REQUESTED
C
          YVAL=0.0D0
C     CALCULATE A NEW YVAL
          W1W=W1/100.0D0
          DO I=2,IRED-1
              RED1=REDDAT1(I)
              RED2=REDDAT2(I)
              RED3=REDDAT3(I)
              REDP1=REDDAT1(I+1)
              REDP2=REDDAT2(I+1)
              REDP3=REDDAT3(I+1)
              IF(I.EQ.2.AND.REAL(W1W).LT.REAL(RED1)) THEN
                  YVAL=0.0D0
                  GO TO 90
              ELSE
C     READ NEXT DATA
              END IF
              IF(I.EQ.(IRED-1).AND.REAL(W1W).GT.REAL(REDP1)) THEN
                  YVAL=REDP2
                  GO TO 90
              ELSE
C     READ NEXT DATA
              END IF
              IF(W1W.GE.RED1.AND.W1W.LE.REDP1) THEN
                  YVAL=(((RED2-REDP2)/(RED1-REDP1))*(W1W-REDP1))+REDP2
                  IF(YVAL.LT.0.0D0) YVAL=0.0D0
                  GO TO 90
              ELSE
C     READ NEXT DATA
              END IF
          END DO
 90       CONTINUE
C
          REG(40)=REG(9)
          REG(9)=YVAL
C     ENERGY CALCULATIONS ARE COMPLETE
          RETURN
      END
C SUB SPREDSQ.FOR
      SUBROUTINE SPREDSQ
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPREDSQ.FOR.
C     CALLED BY CMDER FOR COMMANDS RED, ESEDX AND ESEDY
C     THESE DO THE GEOMETRICAL ENERGY DISTRIBUTIONS
C
          REAL*8 GDTARP(1:101),GDTAR(1:101)
C
          REAL*8 SPDELX,SPDELY,DELZ,MTHETA
C
          INTEGER IE,ENNL,UNITTT
C
          COMMON/REDPASS/ENNL,GDTARP,GDTAR,MTHETA,SPDELX,SPDELY,DELZ
C
          LOGICAL SSPDEXT,EXTRED,EXTREDSQ,EXTKED,CENTRED,SUMMS,NOPRINT
C
          COMMON/REDEXTSQ/EXTREDSQ,CENTRED,SUMMS
C
          COMMON/REDEXT/EXTRED,EXTKED
C
          REAL*8 ABC,EMAX,XVAL,YVAL,
     1    DELSP,XV,YV,RED1,RED2,REDP1,REDP2,RED3,REDP3
C
          LOGICAL SUMMIT,OPEN32,EXIS32
C
          INTEGER ALLOERR,I,NER,M1,N1
C
          REAL*8 W1W
C
          CHARACTER J_UN*13
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
          DIMENSION LARGER(:,:)
          ALLOCATABLE :: LARGER
C
          NOPRINT=.FALSE.
          EXTREDSQ=.FALSE.
          EXTKED=.FALSE.
          EXTRED=.FALSE.
          CENTRED=.FALSE.
          ENNL=0
          SUMMIT=.FALSE.
          SUMMS=.FALSE.
          IF(WC(1:1).EQ.'S') SUMMIT=.TRUE.
          IF(WC(1:1).EQ.'S') SUMMS=.TRUE.
C
C     STRING INPUT
          IF(SST.EQ.1.AND..NOT.SUMMIT) THEN
              OUTLYNE='"REDSQ" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.AND.SUMMIT) THEN
              CALL SHOWIT(1)
              OUTLYNE='"SREDSQ" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'N') THEN
              SQ=0
              WQ='        '
              NOPRINT=.TRUE.
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'NCENT') THEN
              SQ=1
              WQ='CENT    '
              NOPRINT=.TRUE.
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'CENT'.AND.WQ.NE.'ACC'.AND.WQ.NE.'CACC'
     1        .AND.WQ.NE.'ACCX'.AND.WQ.NE.'CACCX') THEN
                  OUTLYNE=
     1            '"CENT", "ACC","CACC", "ACCX" AND "CACCX"'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'ARE THE ONLY OPTIONAL QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     NUMERIC WORD #1
          IF(WQ.NE.'CACC'.AND.WQ.NE.'ACC'.AND.WQ.NE.'ACCX'.AND.
     1    WQ.NE.'CACCX') THEN
C     NUMMERIC WORD 1 IS A DELTA ENERGY DEFAULT AT 10 PERCENT
              IF(DF1.EQ.1) W1=10.0D0
          ELSE
              IF(DF1.EQ.1) THEN
C     MUST BE ACC OR CACC
                  OUTLYNE='EXPLICIT NUMERIC WORD #1 INPUT REQUIRED WHEN THE'
                  CALL SHOWIT(1)
                  OUTLYNE='QUALIFIER IS "ACC", "CACC", "ACCX" OR "CACCX"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=0.0D0
C     NUMERIC WORD 3
          IF(WQ.EQ.'CENT'.OR.WQ.EQ.'CACC'.OR.
     1    WQ.EQ.'CACCX') THEN
              IF(DF3.EQ.0.OR.DF4.EQ.0.OR.DF5.EQ.0) THEN
                  IF(.NOT.SUMMIT) OUTLYNE=
     1            '"REDSQ (CENT, CACC, CACCX)" ONLY TAKE NUMERIC WORDS #1 AND #2'
                  IF(SUMMIT) OUTLYNE=
     1            '"SREDSQ (CENT, CACC, CACCX)" ONLY TAKE NUMERIC WORDS #1 AND #2'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     NOT CENT OR CACC OR CACCX
          END IF
C     NUMERIC WORD 5
          IF(DF5.EQ.0) THEN
              IF(.NOT.SUMMIT)OUTLYNE=
     1        '"REDSQ ( )" TAKES NO NUMERIC WORD #5 INPUT'
              IF(SUMMIT)OUTLYNE=
     1        '"SREDSQ ( )" TAKES NO NUMERIC WORD #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       PROCEED
          IF(WQ.EQ.'CENT'.AND.DF2.EQ.0.AND.W2.NE.0.0D0.OR.
     1    WQ.EQ.'CACC'.AND.DF2.EQ.0.AND.W2.NE.0.0D0.OR.
     1    WQ.EQ.'CACCX'.AND.DF2.EQ.0.AND.W2.NE.0.0D0) THEN
              ABC=W2
              DELZ=W2
              CALL SPMV(ABC)
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.DF4.EQ.0.AND.W4.NE.0.0D0.OR.
     1    WQ.NE.'CACC'.AND.DF4.EQ.0.AND.W4.NE.0.0D0.OR.
     1    WQ.NE.'CACCX'.AND.DF4.EQ.0.AND.W4.NE.0.0D0) THEN
              DELZ=W4
              ABC=W4
              CALL SPMV(ABC)
          ELSE
          END IF
C
          IF(WC(1:1).EQ.'S') THEN
              SSPDEXT=.FALSE.
              EXIS32=.FALSE.
              INQUIRE(FILE=LIBSPO//'SPOTS.DAT',EXIST=EXIS32)
              IF(.NOT.EXIS32) THEN
                  OUTLYNE='SUMMED SPOT DIAGRAM FILE "SPOTS.DAT"'
                  CALL SHOWIT(1)
                  OUTLYNE='DOES NOT EXIT'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       FILE EXISTS, IF NOT OPEN, OPEN IT
              OPEN32=.FALSE.
              INQUIRE(FILE=LIBSPO//'SPOTS.DAT',OPENED=OPEN32)
              IF(.NOT.OPEN32) THEN
                  OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//'SPOTS.DAT',
     1            FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
                  SSPDEXT=.TRUE.
              ELSE
C       IT WAS OPENED, PROCEED
              END IF
          ELSE
C     NOT SUMMED COMMANDS
          END IF
          IF(.NOT.SSPDEXT.AND.WC(1:1).EQ.'S') THEN
              OUTLYNE=
     1        'NO CURRENT SUMMED SPOT DIAGRAM DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.SPDEXT.AND.WC(1:1).NE.'S') THEN
              OUTLYNE=
     1        'NO CURRENT SPOT DIAGRAM DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     HERE IS WHERE THE RADIAL ENERGY DIST DATA IS PROCESSED.
C     LOAD THE CURRENT SPOT DIAGRAM DATA INTO THE RED ARRAY
C     THE RED ARRAY HAS 2 COLUMNS. COLUMN 1 INITIALLY CONTAINS
C     THE RAY ENERGY TERM. COLUMN 2 EITHER HAS THE X,Y OR RADIAL
C     COMPONENT MINUS EITHER THE CORRESPONDING CHEIF RAY COMPONENT
C     OR THE CORRESPONDINIG COMPONENT OF THE SPOT CENTROID. IF
C     FOB NULL WAS USED, RED CAN STILL BE DONE.
C
C
          IF(WQ.EQ.'ACC'.OR.WQ.EQ.'CACC') THEN
              IF(W1.LT.0.0D0.OR.W1.GT.100.0D0) THEN
                  OUTLYNE=
     1            'REQUESTED ENERGY MUST BE BETWEEN 0 AND 100 PERCENT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.NE.'ACC'.AND.WQ.NE.'CACC'.AND.WQ.NE.'ACCX'.AND.
     1    WQ.NE.'CACCX') THEN
              IF(W1.LT.0.1D0.OR.W1.GT.100.0D0) THEN
                  OUTLYNE=
     1            'REQUESTED ENERGY INCREMENT MUST BE BETWEEN 0.1 AND 100 PERCENT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     GET THE NEW MOVED CENTRIOD POSITIONS IF NECESSARY
          IF(WQ.EQ.'CENT'.AND.DF2.EQ.0.AND.W2.NE.0.0D0) THEN
              IF(SYSTEM1(30).GT.2.0D0) THEN
                  OUTLYNE=
     1            'DEFOCUSING IS NOT ALLOWED IN MODES AFOCAL AND UAFOCAL'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  ABC=W2
                  CALL SPMV(ABC)
              END IF
          END IF
          IF(WQ.NE.'CENT'.AND.DF4.EQ.0.AND.W4.NE.0.0D0) THEN
              IF(SYSTEM1(30).GT.2.0D0) THEN
                  OUTLYNE=
     1            'DEFOCUSING IS NOT ALLOWED IN MODES AFOCAL AND UAFOCAL'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  ABC=W4
                  CALL SPMV(ABC)
              END IF
          END IF
C
C     CALCULATE REDVLX AND REDVLY (THE OFFSET VALUES)
C
          IF(WQ.EQ.'CENT'.OR.WQ.EQ.'CACC'.OR.WQ.EQ.'CACCX') THEN
              REDVLX=CENTX
              REDVLY=CENTY
          ELSE
              IF(SYSTEM1(30).LT.3.0D0) THEN
C     FOCAL
                  REDVLX=REFRY(1,NEWIMG)+W2
     1            +(DTAN(REFRY(11,NEWIMG))*W4)
                  SPDELX=W2
                  REDVLY=REFRY(2,NEWIMG)+W3
     1            +(DTAN(REFRY(12,NEWIMG))*W4)
                  SPDELY=W3
              ELSE
C     AFOCAL
                  REDVLX=REFRY(11,NEWIMG)+W2
                  SPDELX=W2
                  REDVLY=REFRY(12,NEWIMG)+W3
                  SPDELY=W3
              END IF
          END IF
C
C     REDVLX AND REDVLY HAVE BEEN SET
C
C     NOW PROCEED WITH THE DATA LOADING
C
          DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
C
          IRED=1
C
          IF(WC(1:1).EQ.'S') UNITTT=32
          IF(WC(1:1).NE.'S') UNITTT=61
          IF(UNITTT.EQ.32) READ(UNIT=UNITTT,REC=1) ITOT
          ALLOCATE(REDDAT1(ITOT),REDDAT2(ITOT),REDDAT3(ITOT),STAT=ALLOERR)
          DO I=2,ITOT
              IF(UNITTT.EQ.61) THEN
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
                  ID=I-1
                  CALL SPOTIT(4)
              END IF
              IF(UNITTT.EQ.32)
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
              IF(DSPOT(17).NE.0) THEN
                  IF(DSPOT(12).NE.0.0D0) THEN
                      IRED=IRED+1
                      RED1=DSPOT(12)
C
                      IF(SYSTEM1(30).LT.3.0D0) THEN
C     FOCAL
                          IF(WQ.NE.'CENT'.AND.WQ.NE.'CACC'.AND.WQ.NE.'CACCX') THEN
                              XV=(DSPOT(1))+(DTAN(DSPOT(9))*W4)
                              YV=(DSPOT(2))+(DTAN(DSPOT(10))*W4)
                          ELSE
                              XV=(DSPOT(1))+(DTAN(DSPOT(9))*W2)
                              YV=(DSPOT(2))+(DTAN(DSPOT(10))*W2)
                          END IF
C
                          IF(SYSTEM1(30).LT.3.0D0) THEN
                              RED2=(XV-REDVLX)/COS_B_ANG
                              RED3=(YV-REDVLY)/COS_A_ANG
                          ELSE
                              RED2=XV-REDVLX
                              RED3=YV-REDVLY
                          END IF
                      ELSE
C     AFOCAL
                          IF(WQ.NE.'CENT'.AND.WQ.NE.'CACC'.AND.WQ.NE.'CACCX') THEN
                              XV=(DSPOT(9))
                              YV=(DSPOT(10))
                          ELSE
                              XV=(DSPOT(9))
                              YV=(DSPOT(10))
                          END IF
C
                          IF(SYSTEM1(30).LT.3.0D0) THEN
                              RED2=(XV-REDVLX)/COS_B_ANG
                              RED3=(YV-REDVLY)/COS_A_ANG
                          ELSE
                              RED2=XV-REDVLX
                              RED3=YV-REDVLY
                          END IF
                      END IF
C
                  ELSE
C     NO CALCULATION, WEIGHT WAS ZERO
                  END IF
              ELSE
C     NO CALCULATION, ENERGY WAS ZERO
              END IF

              REDDAT1(IRED)=RED1
              REDDAT2(IRED)=RED2
              REDDAT3(IRED)=RED3
          END DO
C
C     NOW LOAD THE ARRAY CALLED LARGER
C
          NER=IRED-1
C
          M1=IRED
          N1=3
          DEALLOCATE(LARGER,STAT=ALLOERR)
          ALLOCATE (LARGER(M1,N1),STAT=ALLOERR)
C
          NUMBEROF=ITOT
          IF(IRED.GT.2) CALL REDSUMS(LARGER,M1,N1)
          DEALLOCATE(LARGER,STAT=ALLOERR)
          RED1=REDDAT1(IRED)
          RED2=REDDAT2(IRED)
          RED3=REDDAT3(IRED)
          EMAX=RED1
          DO I=2,IRED
              RED1=REDDAT1(I)
              RED2=REDDAT2(I)
              RED3=REDDAT3(I)
              RED1=RED1/EMAX
              REDDAT1(I)=RED1
              REDDAT2(I)=RED2
              REDDAT3(I)=RED3
          END DO
C     INTERPOLATE THE DATA REQUESTED
C
          IF(WQ.EQ.'ACC'.OR.WQ.EQ.'CACC') THEN
C
              YVAL=0.0D0
C     CALCULATE A NEW YVAL
              W1W=W1/100.0D0
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  RED3=REDDAT3(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  REDP3=REDDAT3(I+1)
                  IF(I.EQ.2.AND.REAL(W1W).LT.REAL(RED1)) THEN
                      YVAL=0.0D0
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(W1W).GT.REAL(REDP1)) THEN
                      YVAL=REDP2
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(W1W.GE.RED1.AND.W1W.LE.REDP1) THEN
                      YVAL=(((RED2-REDP2)/(RED1-REDP1))*(W1W-REDP1))+REDP2
                      IF(YVAL.LT.0.0D0) YVAL=0.0D0
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
 90           CONTINUE
C
              REG(40)=REG(9)
              REG(9)=YVAL
              CALL CLOSE_FILE(32,1)
              RETURN
          ELSE
C     WQ NOT ACC OR CACC
          END IF
          IF(WQ.EQ.'ACCX'.OR.WQ.EQ.'CACCX') THEN
C
              XVAL=0.0D0
C     CALCULATE A NEW XVAL
              W1W=W1
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  RED3=REDDAT3(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  REDP3=REDDAT3(I+1)
                  IF(I.EQ.2.AND.REAL(W1W).LT.REAL(RED2)) THEN
                      YVAL=0.0D0
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(W1W).GT.REAL(REDP2)) THEN
                      XVAL=REDP1
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(W1W.GE.RED2.AND.W1W.LE.REDP2) THEN
                      XVAL=(((RED1-REDP1)/(RED2-REDP2))*(W1W-REDP2))+REDP1
                      IF(XVAL.LT.0.0D0) XVAL=0.0D0
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
 91           CONTINUE
C
              REG(40)=REG(9)
              REG(9)=XVAL*100.0D0
              CALL CLOSE_FILE(32,1)
              RETURN
          ELSE
C     WQ NOT ACCX OR CACCX
          END IF
C     NOW INTERPOLATE THE VALUES AND DISPLAY THEM
C
          IF(.NOT.NOPRINT) THEN
              IF(WS(1:1).NE.'S') WRITE(OUTLYNE,10)
              IF(WS(1:1).EQ.'S') WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
          END IF
 10       FORMAT('GEOMETRIC ENSQUARED ENERGY DISTRIBUTION')
 101      FORMAT('GEOMETRIC SUMMED ENSQUERD ENERGY DISTRIBUTION')
C
          IF(WQ.EQ.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
              IF(.NOT.NOPRINT) THEN
                  DELZ=W3
                  IF(W3.NE.0.0D0) WRITE(OUTLYNE,40) W3,J_UN
                  IF(W3.NE.0.0D0) CALL SHOWIT(0)
              END IF
 40           FORMAT('APPLIED DEFOCUS (Z-DIRECTION) = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
              IF(.NOT.NOPRINT) THEN
                  IF(W2.NE.0.0D0) WRITE(OUTLYNE,41) W2,J_UN
                  IF(W2.NE.0.0D0) CALL SHOWIT(0)
 41               FORMAT('APPLIED X-OFFSET = ',G13.6,1X,A13)
                  IF(W3.NE.0.0D0) WRITE(OUTLYNE,42) W3,J_UN
                  IF(W3.NE.0.0D0) CALL SHOWIT(0)
 42               FORMAT('APPLIED Y-OFFSET = ',G13.6,1X,A13)
                  IF(W3.NE.0.0D0) WRITE(OUTLYNE,40) W4,J_UN
                  IF(W3.NE.0.0D0) CALL SHOWIT(0)
              END IF
          END IF
          IF(WQ.NE.'CENT'.AND.SYSTEM1(30).GT.2.0D0) THEN
              J_UN='RADIAN(S)'
              IF(.NOT.NOPRINT) THEN
                  IF(W2.NE.0.0D0) WRITE(OUTLYNE,43) W2,J_UN
                  IF(W2.NE.0.0D0) CALL SHOWIT(0)
 43               FORMAT('APPLIED X-ANGULAR OFFSET = ',G13.6,1X,A13)
                  IF(W3.NE.0.0D0) WRITE(OUTLYNE,44) W3,J_UN
                  IF(W3.NE.0.0D0) CALL SHOWIT(0)
 44               FORMAT('APPLIED Y-ANGULAR OFFSET = ',G13.6,1X,A13)
              END IF
          END IF
          IF(WQ.EQ.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
              IF(.NOT.NOPRINT) THEN
                  WRITE(OUTLYNE,50) CENTX,J_UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,60) CENTY,J_UN
                  CALL SHOWIT(0)
              END IF
 50           FORMAT('CENTROID X-COORDINATE = ',G13.6,1X,A13)
 60           FORMAT('CENTROID Y-COORDINATE = ',G13.6,1X,A13)
          END IF
          IF(WQ.EQ.'CENT'.AND.SYSTEM1(30).GT.2.0D0) THEN
              J_UN='RADIAN(S)'
              IF(.NOT.NOPRINT) THEN
                  WRITE(OUTLYNE,51) CENTX,J_UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,61) CENTY,J_UN
                  CALL SHOWIT(0)
              END IF
 51           FORMAT('CENTROID X-ANGULAR COORDINATE = ',G13.6,1X,A13)
 61           FORMAT('CENTROID Y-ANGULAR COORDINATE = ',G13.6,1X,A13)
          END IF
          IF(WQ.NE.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
              IF(.NOT.NOPRINT) THEN
                  IF(WC(1:1).NE.'S') THEN
                      WRITE(OUTLYNE,70) REFRY(1,NEWIMG),J_UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,80) REFRY(2,NEWIMG),J_UN
                      CALL SHOWIT(0)
                  END IF
              END IF
 70           FORMAT('CHIEF RAY X-COORDINATE = ',G13.6,1X,A13)
 80           FORMAT('CHIEF RAY Y-COORDINATE = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.SYSTEM1(30).GT.2.0D0) THEN
              J_UN='RADIAN(S)'
              IF(.NOT.NOPRINT) THEN
                  IF(WC(1:1).NE.'S') THEN
                      WRITE(OUTLYNE,71) REFRY(11,NEWIMG),J_UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,81) REFRY(12,NEWIMG),J_UN
                      CALL SHOWIT(0)
                  END IF
              END IF
 71           FORMAT('CHIEF RAY X-ANGULAR COORDINATE = ',G13.6,1X,A13)
 81           FORMAT('CHIEF RAY Y-ANGULAR COORDINATE = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
          ELSE
              J_UN='RADIAN(S)'
          END IF
          IF(.NOT.NOPRINT) THEN
              WRITE(OUTLYNE,901) J_UN
              CALL SHOWIT(0)
          END IF
 901      FORMAT('PERCENT ENCLOSED ENERGY',5X,'SIDE LENGTH',1X,A13)
C
          DELSP=W1/100.0D0
          XVAL=0.0D0
          YVAL=0.0D0
          IE=-1
 100      CONTINUE
C                       CALCULATE A NEW YVAL
          YVAL=0.0D0
C     CALCULATE A NEW YVAL,POSITION
C     XVAL IS THE ENERGY
          IF(IRED.GT.2) THEN
              IF(WQ.EQ.'CENT') CENTRED=.TRUE.
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  RED3=REDDAT3(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  REDP3=REDDAT3(I+1)
                  IF(I.EQ.2.AND.REAL(XVAL).LT.REAL(RED1)) THEN
                      YVAL=0.0D0
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      IF(WC(1:1).EQ.'R'.OR.WC(1:2).EQ.'SR') EXTREDSQ=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(XVAL).GT.REAL(REDP1)) THEN
                      YVAL=REDP2
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      IF(WC(1:1).EQ.'R'.OR.WC(1:2).EQ.'SR') EXTREDSQ=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(REAL(XVAL).GE.REAL(RED1).AND.REAL(XVAL).LE.REAL(REDP1)) THEN
                      YVAL=(((RED2-REDP2)/(RED1-REDP1))*(XVAL-REDP1))+REDP2
                      IF(YVAL.LT.0.0D0) YVAL=0.0D0
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      IF(WC(1:1).EQ.'R'.OR.WC(1:2).EQ.'SR') EXTREDSQ=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
          ELSE
              IF(.NOT.NOPRINT) THEN
                  RED1=REDDAT1(2)
                  RED2=REDDAT2(2)
                  RED3=REDDAT3(2)
                  WRITE(OUTLYNE,903) DABS((100.0D0*RED1)),DABS(RED2)
                  CALL SHOWIT(0)
              END IF
              IE=IE+1
              GDTARP(IE+1)=DABS(YVAL)
              GDTAR(IE+1)=DABS(100.0D0*XVAL)
              ENNL=IE
              IF(WC(1:1).EQ.'R'.OR.WC(1:2).EQ.'SR') EXTREDSQ=.TRUE.
          END IF
 903      FORMAT(5X,F8.1,19X,G13.6)
 200      CONTINUE
C
C     ENERGY CALCULATIONS ARE COMPLETE
C
          CALL CLOSE_FILE(32,1)
          RETURN
      END
C SUB SUMSPRED.FOR
      SUBROUTINE SUMSPRED
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SUMSPRED.FOR.
C     CALLED BY CMDER FOR COMMAND RED ACCSUM
C
          REAL*8 GDTARP(1:101),GDTAR(1:101)
C
          REAL*8 JA,JB,SPDELX,SPDELY,DELZ,MTHETA
C
          INTEGER ENNL,ALLOERR,IIRED
C
          COMMON/REDPASS/ENNL,GDTARP,GDTAR,MTHETA,SPDELX,SPDELY,DELZ
C
          LOGICAL EXTREDSQ,EXTRED,EXTKED,CENTRED,EXIS32,SUMMS
C
          COMMON/REDEXT/EXTRED,EXTKED
C
          COMMON/REDEXTSQ/EXTREDSQ,CENTRED,SUMMS
C
          REAL*8 EMAX,YVAL,
     1    XV,YV,RED1,RED2,REDP1,REDP2
C
          LOGICAL OPEN32
C
          INTEGER N,I,NER,M1,N1
C
          REAL*8 W1W
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
          DIMENSION LARGER(:,:)
          ALLOCATABLE :: LARGER
C
          JA=COS_A_ANG
          JB=COS_B_ANG
          EXTRED=.FALSE.
          EXTREDSQ=.FALSE.
          SUMMS=.TRUE.
          EXTKED=.FALSE.
          CENTRED=.FALSE.
          ENNL=0
          IF(SQ.EQ.0) WQ='SPOTS'
          DO I=8,1,-1
              IF(WQ(I:I).NE.' ') THEN
                  N=I
                  GO TO 10
              END IF
          END DO
 10       CONTINUE
C
C     STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"REDSUM"'
              CALL SHOWIT(1)
              OUTLYNE='TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     NUMERIC WORD #1
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        '"REDSUM"'
              CALL SHOWIT(1)
              OUTLYNE='REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1..OR.S4.EQ.1) THEN
              OUTLYNE=
     1        '"REDSUM"'
              CALL SHOWIT(1)
              OUTLYNE='TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       PROCEED
C
          EXIS32=.FALSE.
          INQUIRE(FILE=LIBSPO//WQ(1:N)//'.DAT',EXIST=EXIS32)
          IF(.NOT.EXIS32) THEN
              OUTLYNE=
     1        'NO SAVED AND SUMMED SPOT DIAGRAM DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     HERE IS WHERE THE RADIAL ENERGY DIST DATA IS PROCESSED.
C     LOAD THE CURRENT SPOT DIAGRAM DATA INTO THE RED ARRAY
C     THE RED ARRAY HAS 2 COLUMNS. COLUMN 1 INITIALLY CONTAINS
C     THE RAY ENERGY TERM. COLUMN 2 EITHER HAS THE X,Y OR RADIAL
C     COMPONENT MINUS EITHER THE CORRESPONDING CHEIF RAY COMPONENT
C     OR THE CORRESPONDINIG COMPONENT OF THE SPOT CENTROID. IF
C     FOB NULL WAS USED, NO RED CAN STILL BE DONE.
C
          IF(W1.LT.0.0D0.OR.W1.GT.100.0D0) THEN
              OUTLYNE=
     1        'REQUESTED ENERGY MUST BE BETWEEN 0 AND 100 PERCENT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     SET REDVLX AND REDVLY TO THE CENTROID LOCATION
C
          CALL SUMSTAT
          REDVLX=CENTX
          REDVLY=CENTY
C
C     REDVLX AND REDVLY HAVE BEEN SET
C
C     NOW PROCEED WITH THE DATA LOADING
C
          DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
C
          IRED=1
C
          OPEN32=.FALSE.
          INQUIRE(FILE=LIBSPO//WQ(1:N)//'.DAT',OPENED=OPEN32)
          IF(.NOT.OPEN32)
     1      OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//WQ(1:N)//'.DAT',
     1      FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
          READ(UNIT=32,REC=1) ITOT
          ALLOCATE(REDDAT1(ITOT),REDDAT2(ITOT),REDDAT3(ITOT),STAT=ALLOERR)
          DO I=2,ITOT
              READ(UNIT=32,REC=I)
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
              IF(DSPOT(17).NE.0) THEN
                  IF(DSPOT(12).NE.0.0D0) THEN
                      IRED=IRED+1
                      RED1=DSPOT(12)
C
                      IF(SYSTEM1(30).LT.3.0D0) THEN
C     FOCAL
                          XV=DSPOT(1)
                          YV=DSPOT(2)
                      ELSE
C     AFOCAL
                          XV=(DSPOT(9))
                          YV=(DSPOT(10))
                      END IF
C
                      IF(SYSTEM1(30).LT.3.0D0) THEN
                          RED2=DSQRT((((XV-REDVLX)/JB)**2)+
     2                                     (((YV-REDVLY)/JA)**2))
                      ELSE
                          RED2=DSQRT(((XV-REDVLX)**2)+
     2                                     ((YV-REDVLY)**2))
                      END IF
C
                  ELSE
C     NO CALCULATION, WEIGHT WAS ZERO
                  END IF
              ELSE
C     NO CALCULATION, ENERGY WAS ZERO
              END IF

              REDDAT1(IRED)=RED1
              REDDAT2(IRED)=RED2
          END DO
C
C     NOW SORT THE ARRAY REDDAT IN INCREASING VALUES OF REDDAT(K,2)
C
          NER=IRED-1
C
          M1=IRED
          N1=2
          DEALLOCATE(LARGER,STAT=ALLOERR)
          ALLOCATE(LARGER(M1,N1),STAT=ALLOERR)
          IIRED=IRED
          NUMBEROF=ITOT
          IF(IRED.GT.2) CALL REDSOR(NER,LARGER,M1,N1,IIRED)
C
          IF(IRED.GT.2)CALL REDPAK(LARGER,M1,N1,IIRED)
C     NOW FOR EACH COORDINATE VALUE, SUM ALL ENERGY AT IT
C     AND BELOW IT
          NUMBEROF=ITOT
          IF(IRED.GT.2) CALL REDSUM(LARGER,M1,N1,IIRED)
          DEALLOCATE(LARGER,STAT=ALLOERR)
          OPEN32=.FALSE.
          RED1=REDDAT1(IRED)
          RED2=REDDAT2(IRED)
          EMAX=RED1
          DO I=2,IRED
              RED1=REDDAT1(I)
              RED2=REDDAT2(I)
              RED1=RED1/EMAX
              REDDAT1(I)=RED1
              REDDAT2(I)=RED2
          END DO
C     INTERPOLATE THE DATA REQUESTED
          YVAL=0.0D0
C     CALCULATE A NEW YVAL
          W1W=W1/100.0D0
          DO I=2,IRED-1
              RED1=REDDAT1(I)
              RED2=REDDAT2(I)
              REDP1=REDDAT1(I+1)
              REDP2=REDDAT2(I+1)
              IF(I.EQ.2.AND.REAL(W1W).LT.REAL(RED1)) THEN
                  YVAL=0.0D0
                  GO TO 90
              ELSE
C     READ NEXT DATA
              END IF
              IF(I.EQ.(IRED-1).AND.REAL(W1W).GT.REAL(REDP1)) THEN
                  YVAL=REDP2
                  GO TO 90
              ELSE
C     READ NEXT DATA
              END IF
              IF(W1W.GE.RED1.AND.W1W.LE.REDP1) THEN
                  YVAL=(((RED2-REDP2)/(RED1-REDP1))*(W1W-REDP1))+REDP2
                  IF(YVAL.LT.0.0D0) YVAL=0.0D0
                  GO TO 90
              ELSE
C     READ NEXT DATA
              END IF
          END DO
 90       CONTINUE
C
          REG(40)=REG(9)
          REG(9)=YVAL
C     ENERGY CALCULATIONS ARE COMPLETE
          DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
          RETURN
      END
C SUB SUMSTAT.FOR
      SUBROUTINE SUMSTAT
C     USED FOR FIELD AVERAGED SPOT DIAGRAM ANALYSIS IN CONJUNCTION
C     WITH SPD SAVE OR SPD SAVEADD BUT PRODUCES NO OUTPUT
C
          IMPLICIT NONE
C
          LOGICAL OPEN32
C
          INTEGER I,K,NUMT1,NUMT2,NUMT3,NUMT4,NUMT5,N
     1    ,NUMT6,NUMT7,NUMT8,NUMT9,NUMT10
C
          REAL*8 TOT,W,SCENTX,SCENTY,
     1    APFAC,
     2    DS9,DS12
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(SQ.EQ.0) WQ='SPOTS'
          DO I=8,1,-1
              IF(WQ(I:I).NE.' ') THEN
                  N=I
                  GO TO 10
              END IF
          END DO
 10       CONTINUE
C
C       FILE EXISTS, IF NOT OPEN, OPEN IT
          OPEN32=.FALSE.
          INQUIRE(FILE=LIBSPO//WQ(1:N)//'.DAT',OPENED=OPEN32)
          IF(.NOT.OPEN32) THEN
              OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//WQ(1:N)//'.DAT',
     1        FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
          ELSE
C       IT WAS OPENED, PROCEED
          END IF
C
          READ(UNIT=32,REC=1) K
C       K IS THE TOTAL NUMBER OF FILE ENTRIES
          TOT=0.0D0
          NUMTOT=0
          NUMT1=0
          NUMT2=0
          NUMT3=0
          NUMT4=0
          NUMT5=0
          NUMT6=0
          NUMT7=0
          NUMT8=0
          NUMT9=0
          NUMT10=0
          SPA=0.0D0
          SPB=0.0D0
          SPC=0.0D0
          SPD=0.0D0
          AFSPB=0.0D0
          AFSPD=0.0D0
          SCENTX=0.0D0
          SCENTY=0.0D0
          DO I=2,K
              READ (UNIT=32,REC=I)
     1        DSPOT(1),DSPOT(2),DSPOT(3),DSPOT(4),
     1        DSPOT(5),DSPOT(6),DSPOT(7),DSPOT(8),
     1        DSPOT(9),DSPOT(10),DSPOT(11),DSPOT(12),
     1        DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16),
     1        DSPOT(13),DSPOT(14),DSPOT(15),DSPOT(16),
     1        DSPOT(17),DSPOT(18),DSPOT(19),DSPOT(20),
     1        DSPOT(21),DSPOT(22),DSPOT(23),DSPOT(24)
     6        ,DSPOT(25),DSPOT(26),DSPOT(27),DSPOT(28),DSPOT(29)
     6        ,DSPOT(30),DSPOT(31),DSPOT(32),DSPOT(33),DSPOT(34),DSPOT(35)
     6        ,DSPOT(36),DSPOT(37),DSPOT(38),DSPOT(39),DSPOT(40),DSPOT(41)
     6        ,DSPOT(42),DSPOT(43),DSPOT(44),DSPOT(45),DSPOT(46),DSPOT(47)
     6        ,DSPOT(48),DSPOT(49),DSPOT(50)
              DS9=DSPOT(16)
              DS12=DSPOT(12)
C       DO FIRST SET OF STATISTICS
C       CHECK IF RAYCOD IS ZERO
C       NO RAYS ARE STORED IF SPTWT WAS ZERO SO DON'T CHECK IT AGAIN
              IF(DSPOT(7).EQ.0.0D0) THEN
C       RAY DID NOT FAIL
                  APFAC=DSPOT(11)
                  TOT=TOT+(DS12*APFAC)
C       NUMTOT IS NUMBER OF PASSED RAYS
                  NUMTOT=NUMTOT+1
C       AT EACH WAVELENGTH THE PASSED RAYS ARE:
                  IF(INT(DS9).EQ.1) NUMT1=NUMT1+1
                  IF(INT(DS9).EQ.2) NUMT2=NUMT2+1
                  IF(INT(DS9).EQ.3) NUMT3=NUMT3+1
                  IF(INT(DS9).EQ.4) NUMT4=NUMT4+1
                  IF(INT(DS9).EQ.5) NUMT5=NUMT5+1
                  IF(INT(DS9).EQ.6) NUMT6=NUMT6+1
                  IF(INT(DS9).EQ.7) NUMT7=NUMT7+1
                  IF(INT(DS9).EQ.8) NUMT8=NUMT8+1
                  IF(INT(DS9).EQ.9) NUMT9=NUMT9+1
                  IF(INT(DS9).EQ.10) NUMT10=NUMT10+1
C       NOW THE CENTROID
                  SPA=SPA+(DS12*DSPOT(1)*APFAC)
                  SPC=SPC+(DS12*DSPOT(2)*APFAC)
                  SPB=SPB+(DS12*DTAN(DSPOT(9))*APFAC)
                  SPD=SPD+(DS12*DTAN(DSPOT(10))*APFAC)
                  AFSPB=AFSPB+(DS12*(DSPOT(9))*APFAC)
                  AFSPD=AFSPD+(DS12*(DSPOT(10))*APFAC)
              ELSE
C       RAY FAILED
              END IF
          END DO
          W=TOT
          SPA=SPA/W
          SPC=SPC/W
          SPB=SPB/W
          SPD=SPD/W
          AFSPB=AFSPB/W
          AFSPD=AFSPD/W
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL OR UFOCAL
              SCENTX=SPA
              SCENTY=SPC
          ELSE
C       MODE AFOCAL OR UAFOCAL
              SCENTX=AFSPB
              SCENTY=AFSPD
          END IF
          IF(DABS(SCENTX).LT.1.0D-15) SCENTX=0.0D0
          IF(DABS(SCENTY).LT.1.0D-15) SCENTY=0.0D0
          CENTX=SCENTX
          CENTY=SCENTY
          RETURN
      END
C SUB SPRED.FOR
      SUBROUTINE SPRED
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPRED.FOR.
C     CALLED BY CMDER FOR COMMANDS RED, ESEDX AND ESEDY
C     THESE DO THE GEOMETRICAL ENERGY DISTRIBUTIONS
C
          REAL*8 GDTARP(1:101),GDTAR(1:101)
C
          REAL*8 JA,JB,SPDELX,SPDELY,DELZ,MTHETA
C
          INTEGER IE,ENNL,UNITTT,IIRED
C
          COMMON/REDPASS/ENNL,GDTARP,GDTAR,MTHETA,SPDELX,SPDELY,DELZ
C
          LOGICAL EXTRED,EXTREDSQ,EXTKED,CENTRED,SUMMS,SSPDEXT
C
          COMMON/REDEXT/EXTRED,EXTKED
C
          COMMON/REDEXTSQ/EXTREDSQ,CENTRED,SUMMS
C
          REAL*8 ABC,EMAX,THETA,XVAL,YVAL,
     1    DELSP,XV,YV,RED1,RED2,REDP1,REDP2
C
          LOGICAL SUMMIT,OPEN32,EXIS32,NOPRINT
C
          INTEGER ALLOERR,I,NER,M1,N1
C
          REAL*8 W1W
C
          CHARACTER J_UN*13
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
          DIMENSION LARGER(:,:)
          ALLOCATABLE :: LARGER
          JA=COS_A_ANG
          JB=COS_B_ANG
C
          NOPRINT=.FALSE.
          EXTRED=.FALSE.
          EXTREDSQ=.FALSE.
          SUMMS=.FALSE.
          EXTKED=.FALSE.
          CENTRED=.FALSE.
          ENNL=0
          SUMMIT=.FALSE.
          IF(WC(1:1).EQ.'S') SUMMIT=.TRUE.
          IF(WC(1:1).EQ.'S') SUMMS=.TRUE.
C
C     STRING INPUT
          IF(SST.EQ.1.AND..NOT.SUMMIT) THEN
              OUTLYNE=
     1        '"RED", "ESEDX", "ESEDY", "ESED",'
              CALL SHOWIT(1)
              OUTLYNE='TAKE NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.AND.SUMMIT) THEN
              OUTLYNE=
     1        '"SRED", "SESEDX", "SESEDY", "SESED",'
              CALL SHOWIT(1)
              OUTLYNE='TAKE NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'N') THEN
              SQ=0
              WQ='        '
              NOPRINT=.TRUE.
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'NCENT') THEN
              SQ=1
              WQ='CENT    '
              NOPRINT=.TRUE.
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'CENT'.AND.WQ.NE.'ACC'.AND.WQ.NE.'CACC'
     1        .AND.WQ.NE.'ACCX'.AND.WQ.NE.'CACCX') THEN
                  OUTLYNE=
     1            '"CENT", "ACC", "CACC", "ACCX" AND "CACCX"'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'ARE THE ONLY OPTIONAL QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     NUMERIC WORD #1
          IF(WQ.NE.'CACC'.AND.WQ.NE.'ACC'.AND.
     1    WQ.NE.'CACCX'.AND.WQ.NE.'ACCX') THEN
C     NUMMERIC WORD 1 IS A DELTA ENERGY DEFAULT AT 10 PERCENT
              IF(DF1.EQ.1) W1=10.0D0
          ELSE
              IF(DF1.EQ.1) THEN
C     MUST BE ACC OR CACC OR ACCX OR CACCX
                  OUTLYNE='EXPLICIT NUMERIC WORD #1 INPUT REQUIRED WHEN THE'
                  CALL SHOWIT(1)
                  OUTLYNE='QUALIFIER IS "ACC", "CACC", "ACCX" OR "CACCX"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=0.0D0
C     NUMERIC WORD 3
          IF(WC.NE.'ESED'.AND.WC.NE.'SESED') THEN
              IF(WQ.EQ.'CENT'.OR.WQ.EQ.'CACC'.OR.WQ.EQ.'CACCX') THEN
                  IF(DF3.EQ.0.OR.DF4.EQ.0.OR.DF5.EQ.0) THEN
                      IF(.NOT.SUMMIT)OUTLYNE=
     1                '"RED (CENT)", "ESEDX (CENT)" AND "ESEDY (CENT)"'
                      IF(.NOT.SUMMIT) CALL SHOWIT(1)
                      IF(.NOT.SUMMIT)OUTLYNE=
     1                '"RED (CACC)", "ESEDX (CACC)" AND "ESEDY (CACC)"'
                      IF(.NOT.SUMMIT) CALL SHOWIT(1)
                      IF(.NOT.SUMMIT)OUTLYNE=
     1                '"RED (CACCX)", "ESEDX (CACCX)" AND "ESEDY (CACCX)"'
                      IF(.NOT.SUMMIT) CALL SHOWIT(1)
                      IF(SUMMIT)OUTLYNE=
     1                '"SRED (CENT)", "SESEDX (CENT)" AND "SESEDY (CENT)"'
                      IF(SUMMIT) CALL SHOWIT(1)
                      IF(SUMMIT)OUTLYNE=
     1                '"SRED (CACC)", "SESEDX (CACC)" AND "SESEDY (CACC)"'
                      IF(SUMMIT) CALL SHOWIT(1)
                      IF(SUMMIT)OUTLYNE=
     1                '"SRED (CACCX)", "SESEDX (CACCX)" AND "SESEDY (CACCX)"'
                      IF(SUMMIT) CALL SHOWIT(1)
                      OUTLYNE='TAKE NO NUMERIC WORD 3, 4 OR 5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              ELSE
C     NOT CENT OR CACC OE CACCX
              END IF
          END IF
C     NUMERIC WORDS 4 AND 5
          IF(WC.EQ.'ESED'.AND.WQ.EQ.'CENT'.OR.WC.EQ.'ESED'.AND.WQ
     1    .EQ.'CACC'.OR.WC.EQ.'ESED'.AND.WQ.EQ.'CACCX') THEN
              IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                  IF(.NOT.SUMMIT)OUTLYNE=
     1            '"ESED (CENT OR CACC OR CACCX)"'
                  OUTLYNE='TAKE NO NUMERIC WORD 4 OR 5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'SESED'.AND.WQ.EQ.'CENT'.OR.WC.EQ.'SESED'.AND.WQ
     1    .EQ.'CACC'.OR.WC.EQ.'SESED'.AND.WQ.EQ.'CACCX') THEN
              IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                  IF(.NOT.SUMMIT)OUTLYNE=
     1            '"ESED (CENT OR CACC OR CACCX)"'
                  IF(SUMMIT)OUTLYNE=
     1            '"SESED (CENT OR CACC OR CACCX)"'
                  CALL SHOWIT(1)
                  OUTLYNE='TAKE NO NUMERIC WORD 4 OR 5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     NUMERIC WORD 5
          IF(WC.EQ.'ESED'.AND.SQ.EQ.0.OR.WC.EQ.'ESED'.AND.WQ.EQ.'ACC'.OR.
     1    WC.EQ.'ESED'.AND.WQ.EQ.'ACCX'.OR.
     1    WC.EQ.'SESED'.AND.SQ.EQ.0.OR.WC.EQ.'SESED'.AND.WQ.EQ.'ACC'.OR.
     1    WC.EQ.'SESED'.AND.WQ.EQ.'ACCX') THEN
              GO TO 9898
          ELSE
              IF(DF5.EQ.0) THEN
                  IF(.NOT.SUMMIT)OUTLYNE=
     1            '"RED ( )", "ESEDX ( )", "ESEDY ( )", "ESED CENT"'
                  IF(SUMMIT)OUTLYNE=
     1            '"SRED ( )", "SESEDX ( )", "SESEDY ( )", "SESED CENT"'
                  CALL SHOWIT(1)
                  IF(.NOT.SUMMIT)OUTLYNE=
     1            'AND "ESED CACC"'
                  IF(SUMMIT)OUTLYNE=
     1            'AND "ESED CACC"'
                  CALL SHOWIT(1)
                  OUTLYNE='TAKE NO NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
 9898     CONTINUE
C       PROCEED
          IF(WQ.EQ.'CENT'.AND.DF2.EQ.0.AND.W2.NE.0.0D0.OR.
     1    WQ.EQ.'CACC'.AND.DF2.EQ.0.AND.W2.NE.0.0D0.OR.
     1    WQ.EQ.'CACCX'.AND.DF2.EQ.0.AND.W2.NE.0.0D0) THEN
              ABC=W2
              DELZ=W2
              CALL SPMV(ABC)
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.DF4.EQ.0.AND.W4.NE.0.0D0.OR.
     1    WQ.NE.'CACCX'.AND.DF4.EQ.0.AND.W4.NE.0.0D0.OR.
     1    WQ.NE.'CACC'.AND.DF4.EQ.0.AND.W4.NE.0.0D0) THEN
              DELZ=W4
              ABC=W4
              CALL SPMV(ABC)
          ELSE
          END IF
C
          IF(WC(1:1).EQ.'S') THEN
              SSPDEXT=.FALSE.
              EXIS32=.FALSE.
              INQUIRE(FILE=LIBSPO//'SPOTS.DAT',EXIST=EXIS32)
              IF(.NOT.EXIS32) THEN
                  OUTLYNE='SUMMED SPOT DIAGRAM FILE "SPOTS.DAT"'
                  CALL SHOWIT(1)
                  OUTLYNE='DOES NOT EXIT'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC(1:1).EQ.'S') SSPDEXT=.TRUE.
C       FILE EXISTS, IF NOT OPEN, OPEN IT
              OPEN32=.FALSE.
              INQUIRE(FILE=LIBSPO//'SPOTS.DAT',OPENED=OPEN32)
              IF(.NOT.OPEN32) THEN
                  OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//'SPOTS.DAT',
     1            FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
              ELSE
C       IT WAS OPENED, PROCEED
              END IF
          ELSE
C     NOT SUMMED COMMANDS
          END IF
          IF(.NOT.SSPDEXT.AND.WC(1:1).EQ.'S') THEN
              OUTLYNE=
     1        'NO CURRENT SUMMED SPOT DIAGRAM DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.SPDEXT.AND.WC(1:1).NE.'S') THEN
              OUTLYNE=
     1        'NO CURRENT SPOT DIAGRAM DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     HERE IS WHERE THE RADIAL ENERGY DIST DATA IS PROCESSED.
C     LOAD THE CURRENT SPOT DIAGRAM DATA INTO THE RED ARRAY
C     THE RED ARRAY HAS 2 COLUMNS. COLUMN 1 INITIALLY CONTAINS
C     THE RAY ENERGY TERM. COLUMN 2 EITHER HAS THE X,Y OR RADIAL
C     COMPONENT MINUS EITHER THE CORRESPONDING CHEIF RAY COMPONENT
C     OR THE CORRESPONDINIG COMPONENT OF THE SPOT CENTROID. IF
C     FOB NULL WAS USED, RED CAN STILL BE DONE.
C
C
          IF(WQ.EQ.'ACC'.OR.WQ.EQ.'CACC') THEN
              IF(W1.LT.0.0D0.OR.W1.GT.100.0D0) THEN
                  OUTLYNE=
     1            'REQUESTED ENERGY MUST BE BETWEEN 0 AND 100 PERCENT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.NE.'ACC'.AND.WQ.NE.'CACC'.AND.WQ.NE.'ACCX'.AND.
     1    WQ.NE.'CACCX') THEN
              IF(W1.LT.0.1D0.OR.W1.GT.100.0D0) THEN
                  OUTLYNE=
     1            'REQUESTED ENERGY INCREMENT MUST BE BETWEEN 0.1 AND 100 PERCENT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     GET THE NEW MOVED CENTRIOD POSITIONS IF NECESSARY
          IF(WQ.EQ.'CENT'.AND.DF2.EQ.0.AND.W2.NE.0.0D0) THEN
              IF(SYSTEM1(30).GT.2.0D0) THEN
                  OUTLYNE=
     1            'DEFOCUSING IS NOT ALLOWED IN MODES AFOCAL AND UAFOCAL'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  ABC=W2
                  CALL SPMV(ABC)
              END IF
          END IF
          IF(WQ.NE.'CENT'.AND.DF4.EQ.0.AND.W4.NE.0.0D0) THEN
              IF(SYSTEM1(30).GT.2.0D0) THEN
                  OUTLYNE=
     1            'DEFOCUSING IS NOT ALLOWED IN MODES AFOCAL AND UAFOCAL'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  ABC=W4
                  CALL SPMV(ABC)
              END IF
          END IF
C
C     CALCULATE REDVLX AND REDVLY (THE OFFSET VALUES)
C
          IF(WQ.EQ.'CENT'.OR.WQ.EQ.'CACC'.OR.WQ.EQ.'CACCX') THEN
              REDVLX=CENTX
              REDVLY=CENTY
          ELSE
              IF(SYSTEM1(30).LT.3.0D0) THEN
C     FOCAL
                  REDVLX=REFRY(1,NEWIMG)+W2
     1            +(DTAN(REFRY(11,NEWIMG))*W4)
                  SPDELX=W2
                  REDVLY=REFRY(2,NEWIMG)+W3
     1            +(DTAN(REFRY(12,NEWIMG))*W4)
                  SPDELY=W3
              ELSE
C     AFOCAL
                  REDVLX=REFRY(11,NEWIMG)+W2
                  SPDELX=W2
                  REDVLY=REFRY(12,NEWIMG)+W3
                  SPDELY=W3
              END IF
          END IF
C
C     REDVLX AND REDVLY HAVE BEEN SET
C
C     NOW PROCEED WITH THE DATA LOADING
C
          DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
C
          IRED=1
C
          IF(WC(1:1).EQ.'S') UNITTT=32
          IF(WC(1:1).NE.'S') UNITTT=61
          IF(UNITTT.EQ.32) READ(UNIT=UNITTT,REC=1) ITOT
          ALLOCATE(REDDAT1(ITOT),REDDAT2(ITOT),REDDAT3(ITOT),STAT=ALLOERR)
          DO I=2,ITOT
              IF(UNITTT.EQ.61) THEN
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
                  ID=I-1
                  CALL SPOTIT(4)
              END IF
              IF(DSPOT(17).NE.0) THEN
                  IF(DSPOT(12).NE.0.0D0) THEN
                      IRED=IRED+1
                      RED1=DSPOT(12)
C
                      IF(SYSTEM1(30).LT.3.0D0) THEN
C     FOCAL
                          IF(WC.EQ.'RED'.OR.WC.EQ.'SRED') THEN
                              IF(WQ.NE.'CENT'.AND.WQ.NE.'CACC'.AND.WQ.NE.'CACCX') THEN
                                  XV=(DSPOT(1))+(DTAN(DSPOT(9))*W4)
                                  YV=(DSPOT(2))+(DTAN(DSPOT(10))*W4)
                              ELSE
                                  XV=(DSPOT(1))+(DTAN(DSPOT(9))*W2)
                                  YV=(DSPOT(2))+(DTAN(DSPOT(10))*W2)
                              END IF
                          ELSE
                              IF(WQ.NE.'CENT'.AND.WQ.NE.'CACC'.AND.WQ.NE.'CACCX') THEN
                                  XV=((DSPOT(1))+(DTAN(DSPOT(9))*W4))
                                  YV=((DSPOT(2))+(DTAN(DSPOT(10))*W4))
                              ELSE
C     WQ MUST BE CENT OR CACC
                                  XV=((DSPOT(1))+(DTAN(DSPOT(9))*W2))
                                  YV=((DSPOT(2))+(DTAN(DSPOT(10))*W2))
                              END IF
                          END IF
C
                          IF(WC.EQ.'RED'.OR.WC.EQ.'SRED') THEN
                              IF(SYSTEM1(30).LT.3.0D0) THEN
                                  RED2=DSQRT((((XV-REDVLX)/JB)**2)+
     2                                             (((YV-REDVLY/JA))**2))
                              ELSE
                                  RED2=DSQRT((((XV-REDVLX))**2)+
     2                                             (((YV-REDVLY))**2))
                              END IF
                          END IF
                          IF(WC.NE.'RED'.AND.WC.NE.'SRED') THEN
                              IF(WC.EQ.'ESEDX'.OR.WC.EQ.'SESEDX') THETA=0.0D0
                              IF(WC.EQ.'ESEDX'.OR.WC.EQ.'SESEDX') MTHETA=0.0D0
                              IF(WC.EQ.'ESEDY'.OR.WC.EQ.'SESEDY') THETA=1.570796327D0
                              IF(WC.EQ.'ESEDY'.OR.WC.EQ.'SESEDY') MTHETA=90.0D0
C
                              IF(WC.EQ.'ESED'.AND.WQ.NE.'CENT'
     1                        .AND.WQ.NE.'CACCX'
     1                        .AND.WQ.NE.'CACC') THETA=W5*(PII/180.0D0)
C
                              IF(WC.EQ.'SESED'.AND.WQ.NE.'CENT'
     1                        .AND.WQ.NE.'CACCX'
     1                        .AND.WQ.NE.'CACC') THETA=W5*(PII/180.0D0)
C
                              IF(WC.EQ.'ESED'.AND.WQ.NE.'CENT'
     1                        .AND.WQ.NE.'CACCX'
     1                        .AND.WQ.NE.'CACC') MTHETA=W5
C
                              IF(WC.EQ.'SESED'.AND.WQ.NE.'CENT'
     1                        .AND.WQ.NE.'CACCX'
     1                        .AND.WQ.NE.'CACC') MTHETA=W5
C
                              IF(WC.EQ.'ESED'.AND.WQ.EQ.'CENT'
     1                        .OR.WC.EQ.'ESED'.AND.WQ.EQ.'CACCX'
     1                        .OR.WC.EQ.'ESED'.AND.WQ.EQ.'CACC')THETA=W3*(PII/180.0D0)
C
                              IF(WC.EQ.'SESED'.AND.WQ.EQ.'CENT'
     1                        .OR.WC.EQ.'SESED'.AND.WQ.EQ.'CACCX'
     1                        .OR.WC.EQ.'SESED'.AND.WQ.EQ.'CACC')THETA=W3*(PII/180.0D0)
C
                              IF(WC.EQ.'ESED'.AND.WQ.EQ.'CENT'
     1                        .OR.WC.EQ.'ESED'.AND.WQ.EQ.'CACCX'
     1                        .OR.WC.EQ.'ESED'.AND.WQ.EQ.'CACC') MTHETA=W3
C
                              IF(WC.EQ.'SESED'.AND.WQ.EQ.'CENT'
     1                        .OR.WC.EQ.'SESED'.AND.WQ.EQ.'CACCX'
     1                        .OR.WC.EQ.'SESED'.AND.WQ.EQ.'CACC') MTHETA=W3
                              IF(SYSTEM1(30).LT.3.0D0) THEN
                                  RED2=(DABS(((XV-REDVLX)/JB)*DCOS(THETA))+
     1                            DABS(((YV-REDVLY)/JA)*DSIN(THETA)))
                              ELSE
                                  RED2=(DABS((XV-REDVLX)*DCOS(THETA))+
     1                            DABS((YV-REDVLY)*DSIN(THETA)))
                              END IF
                          ELSE
C     NOT RED
                          END IF
                      ELSE
C     AFOCAL
                          IF(WC.EQ.'RED'.OR.WC.EQ.'SRED') THEN
                              IF(WQ.NE.'CENT'.AND.WQ.NE.'CACC'.AND.WQ.NE.'CACCX') THEN
                                  XV=(DSPOT(9))
                                  YV=(DSPOT(10))

                              ELSE
                                  XV=(DSPOT(9))
                                  YV=(DSPOT(10))
                              END IF
                          ELSE
                              IF(WQ.NE.'CENT'.AND.WQ.NE.'CACC'.AND.WQ.NE.'CACCX') THEN
                                  XV=(((DSPOT(9))))
                                  YV=(((DSPOT(10))))
                              ELSE
C     WQ MUST BE CENT OR CACC OR CACCX
                                  XV=(((DSPOT(9))))
                                  YV=(((DSPOT(10))))
                              END IF
                          END IF
C
                          IF(WC.EQ.'RED'.OR.WC.EQ.'SRED') THEN
                              IF(SYSTEM1(30).LT.3.0D0) THEN
                                  RED2=DSQRT((((XV-REDVLX)/JB)**2)+
     2                            (((YV-REDVLY)/JA)**2))
                              ELSE
                                  RED2=DSQRT(((XV-REDVLX)**2)+
     2                            ((YV-REDVLY)**2))
                              END IF
                          END IF
                          IF(WC.NE.'RED'.AND.WC.NE.'SRED') THEN
                              IF(WC.EQ.'ESEDX') THETA=0.0D0
                              IF(WC.EQ.'ESEDX') MTHETA=0.0D0
                              IF(WC.EQ.'ESEDY') THETA=1.570796327D0
                              IF(WC.EQ.'ESEDY') MTHETA=90.0D0
C
                              IF(WC.EQ.'ESED'.AND.WQ.NE.'CENT'
     1                        .AND.WQ.NE.'CACCX'.AND.WQ.NE.'CACC') THETA=W5*(PII/180.0D0)
C
                              IF(WC.EQ.'ESED'.AND.WQ.NE.'CENT'
     1                        .AND.WQ.NE.'CACCX'.AND.WQ.NE.'CACC') MTHETA=W5
C
                              IF(WC.EQ.'ESED'.AND.WQ.EQ.'CENT'
     1                        .OR.WC.EQ.'ESED'.AND.WQ.EQ.'CACCX'
     1                        .OR.WC.EQ.'ESED'.AND.WQ.EQ.'CACC') THETA=W3*(PII/180.0D0)
C
                              IF(WC.EQ.'ESED'.AND.WQ.EQ.'CENT'
     1                        .OR.WC.EQ.'ESED'.AND.WQ.EQ.'CACCX'
     1                        .OR.WC.EQ.'ESED'.AND.WQ.EQ.'CACC') MTHETA=W3
C
                              IF(WC.EQ.'SESEDX') THETA=0.0D0
                              IF(WC.EQ.'SESEDX') MTHETA=0.0D0
                              IF(WC.EQ.'SESEDY') THETA=1.570796327D0
                              IF(WC.EQ.'SESEDY') MTHETA=90.0D0
C
                              IF(WC.EQ.'SESED'.AND.WQ.NE.'CENT'
     1                        .AND.WQ.NE.'CACCX'.AND.WQ.NE.'CACC') THETA=W5*(PII/180.0D0)
C
                              IF(WC.EQ.'SESED'.AND.WQ.NE.'CENT'
     1                        .OR.WC.EQ.'SESED'.AND.WQ.NE.'CACC') MTHETA=W5
C
                              IF(WC.EQ.'SESED'.AND.WQ.EQ.'CENT'
     1                        .OR.WC.EQ.'SESED'.AND.WQ.EQ.'CACCX'
     1                        .OR.WC.EQ.'SESED'.AND.WQ.EQ.'CACC') THETA=W3*(PII/180.0D0)
C
                              IF(WC.EQ.'SESED'.AND.WQ.EQ.'CENT'
     1                        .OR.WC.EQ.'SESED'.AND.WQ.EQ.'CACCX'
     1                        .OR.WC.EQ.'SESED'.AND.WQ.EQ.'CACC') MTHETA=W3
C
                              IF(SYSTEM1(30).LT.3.0D0) THEN
                                  RED2=(DABS((XV-REDVLX)*DCOS(THETA))+
     1                            DABS((YV-REDVLY)*DSIN(THETA)))
                              ELSE
                                  RED2=(DABS(((XV-REDVLX)/JB)*DCOS(THETA))+
     1                            DABS(((YV-REDVLY)/JA)*DSIN(THETA)))
                              END IF
                          ELSE
C     NOT RED OR SRED
                          END IF
                      END IF
C
                  ELSE
C     NO CALCULATION, WEIGHT WAS ZERO
                  END IF
              ELSE
C     NO CALCULATION, ENERGY WAS ZERO
              END IF

              REDDAT1(IRED)=RED1
              REDDAT2(IRED)=RED2
          END DO
C
C     NOW SORT THE ARRAY REDDAT IN INCREASING VALUES OF REDDAT(K,2)
C
          NER=IRED-1
C
          M1=IRED
          N1=2
          DEALLOCATE (LARGER,STAT=ALLOERR)
          ALLOCATE (LARGER(M1,N1),STAT=ALLOERR)
          IIRED=IRED
          NUMBEROF=ITOT
          IF(IRED.GT.2) CALL REDSOR(NER,LARGER,M1,N1,IIRED)
C
          IF(IRED.GT.2)CALL REDPAK(LARGER,M1,N1,IIRED)
C     NOW FOR EACH COORDINATE VALUE, SUM ALL ENERGY AT IT
C     AND BELOW IT
          NUMBEROF=ITOT
          IF(IRED.GT.2) CALL REDSUM(LARGER,M1,N1,IIRED)
          DEALLOCATE(LARGER,STAT=ALLOERR)
          RED1=REDDAT1(IRED)
          RED2=REDDAT2(IRED)
          EMAX=RED1
          DO I=2,IRED
              RED1=REDDAT1(I)
              RED2=REDDAT2(I)
              RED1=RED1/EMAX
              REDDAT1(I)=RED1
              REDDAT2(I)=RED2
          END DO
C     INTERPOLATE THE DATA REQUESTED
C
          IF(WQ.EQ.'ACC'.OR.WQ.EQ.'CACC') THEN
C
              YVAL=0.0D0
C     CALCULATE A NEW YVAL
              W1W=W1/100.0D0
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  IF(I.EQ.2.AND.REAL(W1W).LT.REAL(RED1)) THEN
                      YVAL=0.0D0
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(W1W).GT.REAL(REDP1)) THEN
                      YVAL=REDP2
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(W1W.GE.RED1.AND.W1W.LE.REDP1) THEN
                      YVAL=(((RED2-REDP2)/(RED1-REDP1))*(W1W-REDP1))+REDP2
                      IF(YVAL.LT.0.0D0) YVAL=0.0D0
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
 90           CONTINUE
C
              REG(40)=REG(9)

              REG(9)=YVAL
              CALL CLOSE_FILE(32,1)
              DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
              RETURN
          ELSE
C     WQ NOT ACC OR CACC
          END IF
          IF(WQ.EQ.'ACCX'.OR.WQ.EQ.'CACCX') THEN
C
              XVAL=0.0D0
C     CALCULATE A NEW XVAL
              W1W=W1
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  IF(I.EQ.2.AND.REAL(W1W).LT.REAL(RED2)) THEN
                      XVAL=0.0D0
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(W1W).GT.REAL(REDP2)) THEN
                      XVAL=REDP1
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(W1W.GE.RED2.AND.W1W.LE.REDP2) THEN
                      XVAL=(((RED1-REDP1)/(RED2-REDP2))*(W1W-REDP2))+REDP1
                      IF(XVAL.LT.0.0D0) XVAL=0.0D0
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
 91           CONTINUE
C
              REG(40)=REG(9)
              REG(9)=XVAL*100.0D0
              CALL CLOSE_FILE(32,1)
              DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
              RETURN
          ELSE
C     WQ NOT ACCX OR CACCX
          END IF
C     NOW INTERPOLATE THE VALUES AND DISPLAY THEM
C
          IF(WC.EQ.'RED'.OR.WC.EQ.'SRED') THEN
              IF(.NOT.NOPRINT) THEN
                  IF(WS(1:1).NE.'S') WRITE(OUTLYNE,10)
                  IF(WS(1:1).EQ.'S') WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
              END IF
 10           FORMAT('GEOMETRIC RADIAL ENERGY DISTRIBUTION')
 101          FORMAT('GEOMETRIC SUMMED RADIAL ENERGY DISTRIBUTION')
          ELSE
          END IF
          IF(WC.EQ.'ESED'.OR.WC.EQ.'ESEDX'.OR.WC.EQ.'ESEDY'
     1    .OR.WC.EQ.'SESED'.OR.WC.EQ.'SESEDX'.OR.WC.EQ.'SESEDY') THEN
              IF(.NOT.NOPRINT) THEN
                  IF(WS(1:1).NE.'S') WRITE(OUTLYNE,20)
                  IF(WS(1:1).EQ.'S') WRITE(OUTLYNE,201)
                  CALL SHOWIT(0)
              END IF
 20           FORMAT('EXPANDING SLIT ENERGY DISTRIBUTION')
 201          FORMAT('SUMMED EXPANDING SLIT ENERGY DISTRIBUTION')
          ELSE
          END IF
          IF(WC.NE.'RED'.AND.WC.NE.'SRED') THEN
C     PRINT ORIENTATION OF EXPANDING SLIT PLOT
              IF(.NOT.NOPRINT) THEN
                  WRITE(OUTLYNE,30) THETA*(180.0D0/PII)
                  CALL SHOWIT(0)
              END IF
 30           FORMAT('EXPANDING SLIT SCAN ORIENTATION = '
     1        ,F7.2,' DEGREE(S)')
          ELSE
          END IF
C
          IF(WQ.EQ.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
              IF(.NOT.NOPRINT) THEN
                  DELZ=W3
                  IF(W3.NE.0.0D0) WRITE(OUTLYNE,40) W3,J_UN
                  IF(W3.NE.0.0D0) CALL SHOWIT(0)
              END IF
 40           FORMAT('APPLIED DEFOCUS (Z-DIRECTION) = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
              IF(.NOT.NOPRINT) THEN
                  IF(W2.NE.0.0D0) WRITE(OUTLYNE,41) W2,J_UN
                  IF(W2.NE.0.0D0) CALL SHOWIT(0)
 41               FORMAT('APPLIED X-OFFSET = ',G13.6,1X,A13)
                  IF(W3.NE.0.0D0) WRITE(OUTLYNE,42) W3,J_UN
                  IF(W3.NE.0.0D0) CALL SHOWIT(0)
 42               FORMAT('APPLIED Y-OFFSET = ',G13.6,1X,A13)
                  IF(W4.NE.0.0D0) WRITE(OUTLYNE,40) W4,J_UN
                  IF(W4.NE.0.0D0) CALL SHOWIT(0)
              END IF
              IF(WQ.NE.'CENT'.AND.SYSTEM1(30).GT.2.0D0) THEN
                  J_UN='RADIAN(S)'
                  IF(W2.NE.0.0D0) WRITE(OUTLYNE,43) W2,J_UN
                  IF(W2.NE.0.0D0) CALL SHOWIT(0)
 43               FORMAT('APPLIED X-ANGULAR OFFSET = ',G13.6,1X,A13)
                  IF(W3.NE.0.0D0) WRITE(OUTLYNE,44) W3,J_UN
                  IF(W3.NE.0.0D0) CALL SHOWIT(0)
              END IF
 44           FORMAT('APPLIED Y-ANGULAR OFFSET = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(WQ.EQ.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
              IF(.NOT.NOPRINT) THEN
                  WRITE(OUTLYNE,50) CENTX,J_UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,60) CENTY,J_UN
                  CALL SHOWIT(0)
 50               FORMAT('CENTROID X-COORDINATE = ',G13.6,1X,A13)
 60               FORMAT('CENTROID Y-COORDINATE = ',G13.6,1X,A13)
              END IF
          ELSE
          END IF
          IF(WQ.EQ.'CENT'.AND.SYSTEM1(30).GT.2.0D0) THEN
              J_UN='RADIAN(S)'
              IF(.NOT.NOPRINT) THEN
                  WRITE(OUTLYNE,51) CENTX,J_UN
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,61) CENTY,J_UN
                  CALL SHOWIT(0)
 51               FORMAT('CENTROID X-ANGULAR COORDINATE = ',G13.6,1X,A13)
 61               FORMAT('CENTROID Y-ANGULAR COORDINATE = ',G13.6,1X,A13)
              END IF
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
              IF(.NOT.NOPRINT) THEN
                  IF(WC(1:1).NE.'S') THEN
                      WRITE(OUTLYNE,70) REFRY(1,NEWIMG),J_UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,80) REFRY(2,NEWIMG),J_UN
                      CALL SHOWIT(0)
                  END IF
              END IF
 70           FORMAT('CHIEF RAY X-COORDINATE = ',G13.6,1X,A13)
 80           FORMAT('CHIEF RAY Y-COORDINATE = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(WQ.NE.'CENT'.AND.SYSTEM1(30).GT.2.0D0) THEN
              J_UN='RADIAN(S)'
              IF(.NOT.NOPRINT) THEN
                  IF(WC(1:1).NE.'S') THEN
                      WRITE(OUTLYNE,71) REFRY(11,NEWIMG),J_UN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,81) REFRY(12,NEWIMG),J_UN
                      CALL SHOWIT(0)
                  END IF
              END IF
 71           FORMAT('CHIEF RAY X-ANGULAR COORDINATE = ',G13.6,1X,A13)
 81           FORMAT('CHIEF RAY Y-ANGULAR COORDINATE = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(SYSTEM1(30).LT.3.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
          ELSE
              J_UN='RADIAN(S)'
          END IF
          IF(.NOT.NOPRINT) THEN
              IF(WC.EQ.'RED'.OR.WC.EQ.'SRED') WRITE(OUTLYNE,901) J_UN
              IF(WC.EQ.'RED'.OR.WC.EQ.'SRED') CALL SHOWIT(0)
              IF(WC.NE.'RED'.AND.WC.NE.'SRED') WRITE(OUTLYNE,902) J_UN
              IF(WC.NE.'RED'.AND.WC.NE.'SRED') CALL SHOWIT(0)
          END IF
 901      FORMAT('PERCENT ENCLOSED ENERGY',5X,'SEMI-DIAMETER',1X,A13)
 902      FORMAT('PERCENT ENCLOSED ENERGY',5X,'SLIT SEMI-WIDTH',1X,A13)
C
          DELSP=W1/100.0D0
          XVAL=0.0D0
          YVAL=0.0D0
          IE=-1
 100      CONTINUE
C                       CALCULATE A NEW YVAL
          YVAL=0.0D0
C     CALCULATE A NEW YVAL,POSITION
C     XVAL IS THE ENERGY
          IF(IRED.GT.2) THEN
              IF(WQ.EQ.'CENT') CENTRED=.TRUE.
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  IF(I.EQ.2.AND.REAL(XVAL).LT.REAL(RED1)) THEN
                      YVAL=0.0D0
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      IF(WC(1:1).EQ.'R'.OR.WC(1:2).EQ.'SR') EXTRED=.TRUE.
                      IF(WC(1:1).EQ.'E'.OR.WC(1:2).EQ.'SE') EXTKED=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(XVAL).GT.REAL(REDP1)) THEN
                      YVAL=REDP2
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      IF(WC(1:1).EQ.'R'.OR.WC(1:2).EQ.'SR') EXTRED=.TRUE.
                      IF(WC(1:1).EQ.'E'.OR.WC(1:2).EQ.'SE') EXTKED=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(REAL(XVAL).GE.REAL(RED1).AND.REAL(XVAL).LE.REAL(REDP1)) THEN
                      YVAL=(((RED2-REDP2)/(RED1-REDP1))*(XVAL-REDP1))+REDP2
                      IF(YVAL.LT.0.0D0) YVAL=0.0D0
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      IF(WC(1:1).EQ.'R'.OR.WC(1:2).EQ.'SR') EXTRED=.TRUE.
                      IF(WC(1:1).EQ.'E'.OR.WC(1:2).EQ.'SE') EXTKED=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
          ELSE
              IF(.NOT.NOPRINT) THEN
                  RED1=REDDAT1(2)
                  RED2=REDDAT2(2)
                  WRITE(OUTLYNE,903) DABS((100.0D0*RED1)),DABS(RED2)
                  CALL SHOWIT(0)
              END IF
              IE=IE+1
              GDTARP(IE+1)=DABS(YVAL)
              GDTAR(IE+1)=DABS(100.0D0*XVAL)
              ENNL=IE
              IF(WC(1:1).EQ.'R'.OR.WC(1:2).EQ.'SR') EXTRED=.TRUE.
              IF(WC(1:1).EQ.'E'.OR.WC(1:2).EQ.'SE') EXTKED=.TRUE.
          END IF
 903      FORMAT(5X,F8.1,19X,G13.6)
 200      CONTINUE
C
C     ENERGY CALCULATIONS ARE COMPLETE

          CALL CLOSE_FILE(32,1)
          DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
          RETURN
      END
C SUB DSPRED.FOR
      SUBROUTINE DSPRED(MMM)
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE DSPRED.FOR.
C     CALLED BY CMDER FOR COMMAND DRED
C     THIS DOES THE DIFFRACTION ENERGY DISTRIBUTIONS
C
          REAL*8 GDTARP(1:101),GDTAR(1:101)
C
          REAL*8 PSF,SPDELX,SPDELY,DELZ,MTHETA
C
          INTEGER IE,ENNL,UNITTT,IIRED
C
          COMMON/REDPASS/ENNL,GDTARP,GDTAR,MTHETA,SPDELX,SPDELY,DELZ
C
          LOGICAL DEXTRED,DEXTREDSQ
C
          COMMON/DREDEXT/DEXTRED
C
          COMMON/DREDEXTSQ/DEXTREDSQ

          LOGICAL EXTREDSQ,EXTRED,EXTKED,CENTRED,SUMMS
C
          COMMON/REDEXT/EXTRED,EXTKED
C
          COMMON/REDEXTSQ/EXTREDSQ,CENTRED,SUMMS
C
          REAL*8 EMAX,XVAL,YVAL,
     1    DELSP,X,Y,RED1,RED2,REDP1,REDP2
C
          LOGICAL NOPRINT,EXIS53
C
          INTEGER ALLOERR,I,NER,M1,N1,MMM,II,J
C
          REAL*8 W1W
C
          CHARACTER J_UN*13

          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
          DIMENSION LARGER(:,:)
          ALLOCATABLE :: LARGER
C
          NOPRINT=.FALSE.
          EXTRED=.FALSE.
          EXTREDSQ=.FALSE.
          DEXTRED=.FALSE.
          DEXTREDSQ=.FALSE.
          EXTKED=.FALSE.
          SUMMS=.FALSE.
          ENNL=0
C
C     STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"DRED" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'N') THEN
              SQ=0
              WQ='        '
              NOPRINT=.TRUE.
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'ACC'.AND.WQ.NE.'ACCX') THEN
                  OUTLYNE=
     1            '"ACC" AND "ACCX" ARE THE ONLY OPTIONAL QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     NUMERIC WORD #1
          IF(WQ.NE.'ACC'.AND.WQ.NE.'ACCX') THEN
C     NUMMERIC WORD 1 IS A DELTA ENERGY DEFAULT AT 10 PERCENT
              IF(DF1.EQ.1) W1=10.0D0
          ELSE
              IF(DF1.EQ.1) THEN
C     MUST BE ACC
                  OUTLYNE='EXPLICIT NUMERIC WORD #1 INPUT REQUIRED WHEN THE'
                  CALL SHOWIT(1)
                  OUTLYNE='QUALIFIER IS "ACC"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=0.0D0
          IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
              OUTLYNE='"DRED" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(.NOT.PSFEXT) THEN
              OUTLYNE=
     1        'NO CURRENT PSF DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     HERE IS WHERE THE RADIAL ENERGY DIST DATA IS PROCESSED.
C     LOAD THE CURRENT SPOT DIAGRAM DATA INTO THE RED ARRAY
C     THE RED ARRAY HAS 2 COLUMNS. COLUMN 1 INITIALLY CONTAINS
C     THE RAY ENERGY TERM. COLUMN 2 EITHER HAS THE X,Y OR RADIAL
C     COMPONENT MINUS EITHER THE CORRESPONDING CHEIF RAY COMPONENT
C     OR THE CORRESPONDINIG COMPONENT OF THE SPOT CENTROID. IF
C     FOB NULL WAS USED, RED CAN STILL BE DONE.
C
C
          IF(WQ.EQ.'ACC') THEN
              IF(W1.LT.0.0D0.OR.W1.GT.100.0D0) THEN
                  OUTLYNE=
     1            'REQUESTED ENERGY MUST BE BETWEEN 0 AND 100 PERCENT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.NE.'ACC'.AND.WQ.NE.'ACCX') THEN
              IF(W1.LT.0.1D0.OR.W1.GT.100.0D0) THEN
                  OUTLYNE=
     1            'REQUESTED ENERGY INCREMENT MUST BE BETWEEN 0.1 AND 100 PERCENT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C     CALCULATE REDVLX AND REDVLY (THE OFFSET VALUES)
C
          REDVLX=W2
          SPDELX=W2
          REDVLY=W3
          SPDELY=W3
C
C     REDVLX AND REDVLY HAVE BEEN SET
C
C     NOW PROCEED WITH THE DATA LOADING
C
          DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
C
          IRED=1
C
          UNITTT=53
          EXIS53=.FALSE.
          INQUIRE(FILE=trim(HOME)//'SPDPSF.DAT',EXIST=EXIS53)
          IF(.NOT.EXIS53) THEN
              OUTLYNE=
     1        'NO PSF SPOT FILE EXISTS, NO DIFFRACTION ENERGY DISTRIBUTION'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'CAN BE CREATED'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          NUMBEROF=MMM*MMM
          IRED=2
          ALLOCATE(REDDAT1(NUMBEROF),REDDAT2(NUMBEROF),REDDAT3(NUMBEROF)
     1    ,STAT=ALLOERR)

          II=1
          IRED=1
          DO I=1,MMM
              DO J=1,MMM
                  X=SPDPSF1(II)
                  Y=SPDPSF2(II)
                  PSF=SPDPSF3(II)
                  RED1=PSF
                  RED2=DSQRT(((X-REDVLX)**2)+
     2                             ((Y-REDVLY)**2))

                  REDDAT1(IRED)=RED1
                  REDDAT2(IRED)=RED2
                  IRED=IRED+1
                  II=II+1
              END DO
          END DO
C
C     NOW SORT THE ARRAY REDDAT IN INCREASING VALUES OF REDDAT(K,2)
C
          NER=IRED-1
C
          M1=IRED
          N1=2
          DEALLOCATE (LARGER,STAT=ALLOERR)
          ALLOCATE (LARGER(M1,N1),STAT=ALLOERR)
          IIRED=IRED
          IF(IRED.GT.2) CALL REDSOR(NER,LARGER,M1,N1,IIRED)
C
          IF(IRED.GT.2)CALL REDPAK(LARGER,M1,N1,IIRED)
C     NOW FOR EACH COORDINATE VALUE, SUM ALL ENERGY AT IT
C     AND BELOW IT
          IF(IRED.GT.2) CALL REDSUM(LARGER,M1,N1,IIRED)
          DEALLOCATE(LARGER,STAT=ALLOERR)
          RED1=REDDAT1(IRED)
          RED2=REDDAT2(IRED)
          EMAX=RED1
          DO I=2,IRED
              RED1=REDDAT1(I)
              RED2=REDDAT2(I)
              RED1=RED1/EMAX
              REDDAT1(I)=RED1
              REDDAT2(I)=RED2
          END DO
C     INTERPOLATE THE DATA REQUESTED
C
          IF(WQ.EQ.'ACC') THEN
C
              YVAL=0.0D0
C     CALCULATE A NEW YVAL
              W1W=W1/100.0D0
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  IF(I.EQ.2.AND.REAL(W1W).LT.REAL(RED1)) THEN
                      YVAL=0.0D0
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(W1W).GT.REAL(REDP1)) THEN
                      YVAL=REDP2
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(W1W.GE.RED1.AND.W1W.LE.REDP1) THEN
                      YVAL=(((RED2-REDP2)/(RED1-REDP1))*(W1W-REDP1))+REDP2
                      IF(YVAL.LT.0.0D0) YVAL=0.0D0
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
 90           CONTINUE
              REG(40)=REG(9)
              REG(9)=YVAL
              DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
              RETURN
          ELSE
C     WQ NOR ACC
          END IF
          IF(WQ.EQ.'ACCX') THEN
C
              XVAL=0.0D0
C     CALCULATE A NEW XVAL
              W1W=W1
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  IF(I.EQ.2.AND.REAL(W1W).LT.REAL(RED2)) THEN
                      XVAL=0.0D0
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(W1W).GT.REAL(REDP2)) THEN
                      XVAL=REDP1
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(W1W.GE.RED2.AND.W1W.LE.REDP2) THEN
                      XVAL=(((RED1-REDP1)/(RED2-REDP2))*(W1W-REDP2))+REDP1
                      IF(XVAL.LT.0.0D0) XVAL=0.0D0
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
 91           CONTINUE
C
              REG(40)=REG(9)
              REG(9)=XVAL*100.0D0
              DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
              RETURN
          ELSE
C     WQ NOR ACCX
          END IF
C     NOW INTERPOLATE THE VALUES AND DISPLAY THEM
C
          IF(.NOT.NOPRINT) THEN
              WRITE(OUTLYNE,10)
              CALL SHOWIT(0)
 10           FORMAT('DIFFRACTION RADIAL ENERGY DISTRIBUTION')
          ELSE
          END IF
C
          IF(SYSTEM1(30).LE.2.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
          ELSE
              J_UN='RADIAN(S)'
          END IF
          IF(.NOT.NOPRINT) THEN
              IF(W2.NE.0.0D0) WRITE(OUTLYNE,41) W2,J_UN
              IF(W2.NE.0.0D0) CALL SHOWIT(0)
 41           FORMAT('APPLIED X-OFFSET = ',G13.6,1X,A13)
              IF(W3.NE.0.0D0) WRITE(OUTLYNE,42) W3,J_UN
              IF(W3.NE.0.0D0) CALL SHOWIT(0)
 42           FORMAT('APPLIED Y-OFFSET = ',G13.6,1X,A13)
          END IF
! 44   FORMAT('APPLIED Y-ANGULAR OFFSET = ',G13.6,1X,A13)
          IF(SYSTEM1(30).LE.2.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
          ELSE
              J_UN='RADIAN(S)'
          END IF
          IF(.NOT.NOPRINT) THEN
              IF(SYSTEM1(30).LE.2.0D0)
     1        WRITE(OUTLYNE,70) REFRY(1,NEWIMG),J_UN
              IF(SYSTEM1(30).GE.3.0D0)
     1        WRITE(OUTLYNE,70) REFRY(11,NEWIMG),J_UN
              CALL SHOWIT(0)
              IF(SYSTEM1(30).LE.2.0D0)
     1        WRITE(OUTLYNE,80) REFRY(2,NEWIMG),J_UN
              IF(SYSTEM1(30).GE.3.0D0)
     1        WRITE(OUTLYNE,80) REFRY(12,NEWIMG),J_UN
              CALL SHOWIT(0)
          END IF
 70       FORMAT('CHIEF RAY X-COORDINATE = ',G13.6,1X,A13)
 80       FORMAT('CHIEF RAY Y-COORDINATE = ',G13.6,1X,A13)
          IF(SYSTEM1(30).LE.2.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
          ELSE
              J_UN='RADIAN(S)'
          END IF
          IF(.NOT.NOPRINT) THEN
              WRITE(OUTLYNE,901) J_UN
              CALL SHOWIT(0)
          END IF
 901      FORMAT('PERCENT ENCLOSED ENERGY',5X,'SEMI-DIAMETER',1X,A13)
C
          DELSP=W1/100.0D0
          XVAL=0.0D0
          YVAL=0.0D0
          IE=-1
 100      CONTINUE
C                       CALCULATE A NEW YVAL
          YVAL=0.0D0
C     CALCULATE A NEW YVAL,POSITION
C     XVAL IS THE ENERGY
          IF(IRED.GT.2) THEN
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  IF(I.EQ.2.AND.REAL(XVAL).LT.REAL(RED1)) THEN
                      YVAL=0.0D0
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      DEXTRED=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(XVAL).GT.REAL(REDP1)) THEN
                      YVAL=REDP2
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      DEXTRED=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(REAL(XVAL).GE.REAL(RED1).AND.REAL(XVAL).LE.REAL(REDP1)) THEN
                      YVAL=(((RED2-REDP2)/(RED1-REDP1))*(XVAL-REDP1))+REDP2
                      IF(YVAL.LT.0.0D0) YVAL=0.0D0
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      DEXTRED=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
          ELSE
              IF(.NOT.NOPRINT) THEN
                  RED1=REDDAT1(2)
                  RED2=REDDAT2(2)
                  WRITE(OUTLYNE,903) DABS((100.0D0*RED1)),DABS(RED2)
                  CALL SHOWIT(0)
              END IF
              IE=IE+1
              GDTARP(IE+1)=DABS(YVAL)
              GDTAR(IE+1)=DABS(100.0D0*XVAL)
              ENNL=IE
              DEXTRED=.TRUE.
          END IF
 903      FORMAT(5X,F8.1,19X,G13.6)
 200      CONTINUE
C
C     ENERGY CALCULATIONS ARE COMPLETE
C
          DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
          RETURN
      END
C SUB DSPREDSQ.FOR
      SUBROUTINE DSPREDSQ(MMM)
C
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE DSPREDSQ.FOR.
C     CALLED BY CMDER FOR COMMANDS DRED
C     THESE DO THE DIFFRACTION ENSQUARED ENERGY DISTRIBUTIONS
C
          REAL*8 GDTARP(1:101),GDTAR(1:101)
C
          REAL*8 X,Y,PSF,SPDELX,SPDELY,DELZ,MTHETA
C
          INTEGER IE,ENNL,UNITTT
C
          COMMON/REDPASS/ENNL,GDTARP,GDTAR,MTHETA,SPDELX,SPDELY,DELZ
C
          LOGICAL NOPRINT
C
          LOGICAL DEXTRED,DEXTREDSQ
C
          COMMON/DREDEXT/DEXTRED
C
          COMMON/DREDEXTSQ/DEXTREDSQ

          LOGICAL EXTREDSQ,EXTRED,EXTKED,CENTRED,SUMMS
C
          COMMON/REDEXT/EXTRED,EXTKED
C
          COMMON/REDEXTSQ/EXTREDSQ,CENTRED,SUMMS
C
C
          REAL*8 EMAX,XVAL,YVAL,
     1    DELSP,RED1,RED2,REDP1,REDP2,RED3,REDP3
C
          LOGICAL EXIS53
C
          INTEGER ALLOERR,I,NER,M1,N1,MMM,II,J
C
          REAL*8 W1W
C
          CHARACTER J_UN*13
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 LARGER
          DIMENSION LARGER(:,:)
          ALLOCATABLE :: LARGER
C
          NOPRINT=.FALSE.
          DEXTREDSQ=.FALSE.
          DEXTRED=.FALSE.
          EXTREDSQ=.FALSE.
          EXTKED=.FALSE.
          EXTRED=.FALSE.
          SUMMS=.FALSE.
          ENNL=0
C
C     STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"DREDSQ" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'N') THEN
              SQ=0
              WQ='        '
              NOPRINT=.TRUE.
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'ACC'.AND.WQ.NE.'ACCX') THEN
                  OUTLYNE=
     1            '"ACC" AND "ACCX" ARE THE ONLY OPTIONAL QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     NUMERIC WORD #1
          IF(WQ.NE.'ACC'.AND.WQ.NE.'ACCX') THEN
C     NUMMERIC WORD 1 IS A DELTA ENERGY DEFAULT AT 10 PERCENT
              IF(DF1.EQ.1) W1=10.0D0
          ELSE
              IF(DF1.EQ.1) THEN
C     MUST BE ACC OR ACCX
                  OUTLYNE='EXPLICIT NUMERIC WORD #1 INPUT REQUIRED WHEN THE'
                  CALL SHOWIT(1)
                  OUTLYNE='QUALIFIER IS "ACC"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=0.0D0
C     NUMERIC WORD 3
          IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
              OUTLYNE=
     1        '"DREDSQ" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       PROCEED
C
          IF(.NOT.PSFEXT) THEN
              OUTLYNE=
     1        'NO CURRENT PSF DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     FOB NULL WAS USED, RED CAN STILL BE DONE.
C
C
          IF(WQ.EQ.'ACC') THEN
              IF(W1.LT.0.0D0.OR.W1.GT.100.0D0) THEN
                  OUTLYNE=
     1            'REQUESTED ENERGY MUST BE BETWEEN 0 AND 100 PERCENT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.NE.'ACC'.AND.WQ.NE.'ACCX') THEN
              IF(W1.LT.0.1D0.OR.W1.GT.100.0D0) THEN
                  OUTLYNE=
     1            'REQUESTED ENERGY INCREMENT MUST BE BETWEEN 0.1 AND 100 PERCENT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          REDVLX=W2
          SPDELX=W2
          REDVLY=W3
          SPDELY=W3
C
C
C     REDVLX AND REDVLY HAVE BEEN SET
C
C     NOW PROCEED WITH THE DATA LOADING
C
          DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
C
          IRED=1
C
          UNITTT=53
          EXIS53=.FALSE.
          INQUIRE(FILE=trim(HOME)//'SPDPSF.DAT',EXIST=EXIS53)
          IF(.NOT.EXIS53) THEN
              OUTLYNE=
     1        'NO PSF SPOT FILE EXISTS, NO DIFFRACTION ENERGY DISTRIBUTION'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'CAN BE CREATED'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          NUMBEROF=MMM*MMM
          IRED=1
          ALLOCATE(REDDAT1(NUMBEROF),REDDAT2(NUMBEROF),REDDAT3(NUMBEROF),
     1    STAT=ALLOERR)
          II=1
          IRED=1
          DO I=1,MMM
              DO J=1,MMM
                  X=SPDPSF1(II)
                  Y=SPDPSF2(II)
                  PSF=SPDPSF3(II)
                  RED1=PSF
                  RED2=(X-REDVLX)
                  RED3=(Y-REDVLY)
                  REDDAT1(II)=RED1
                  REDDAT2(II)=RED2
                  REDDAT3(II)=RED3

                  IRED=IRED+1
                  II=II+1
              END DO
          END DO
C
C     NOW LOAD THE ARRAY CALLED LARGER
C
          NER=IRED-1
C
          M1=IRED
          N1=3
          DEALLOCATE (LARGER,STAT=ALLOERR)
          ALLOCATE (LARGER(M1,N1),STAT=ALLOERR)
C
          IF(IRED.GT.2) CALL REDSUMS(LARGER,M1,N1)
          DEALLOCATE(LARGER,STAT=ALLOERR)
          RED1=REDDAT1(IRED)
          RED2=REDDAT2(IRED)
          RED3=REDDAT3(IRED)
          EMAX=RED1
          DO I=2,IRED
              RED1=RED1/EMAX
              REDDAT1(I)=REDDAT1(I)/EMAX
          END DO
C     INTERPOLATE THE DATA REQUESTED
C
          IF(WQ.EQ.'ACC') THEN
C
              YVAL=0.0D0
C     CALCULATE A NEW YVAL
              W1W=W1/100.0D0
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  RED3=REDDAT3(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  REDP3=REDDAT3(I+1)
                  IF(I.EQ.2.AND.REAL(W1W).LT.REAL(RED1)) THEN
                      YVAL=0.0D0
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(W1W).GT.REAL(REDP1)) THEN
                      YVAL=REDP2
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(W1W.GE.RED1.AND.W1W.LE.REDP1) THEN
                      YVAL=(((RED2-REDP2)/(RED1-REDP1))*(W1W-REDP1))+REDP2
                      IF(YVAL.LT.0.0D0) YVAL=0.0D0
                      GO TO 90
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
 90           CONTINUE
C
              REG(40)=REG(9)
              REG(9)=YVAL
              CALL CLOSE_FILE(32,1)
              DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
              RETURN
          ELSE
C     WQ NOT ACC
          END IF
          IF(WQ.EQ.'ACCX') THEN
C
              XVAL=0.0D0
C     CALCULATE A NEW XVAL
              W1W=W1
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  RED3=REDDAT3(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  REDP3=REDDAT3(I+1)
                  IF(I.EQ.2.AND.REAL(W1W).LT.REAL(RED2)) THEN
                      XVAL=0.0D0
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(W1W).GT.REAL(REDP2)) THEN
                      XVAL=REDP1
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(W1W.GE.RED2.AND.W1W.LE.REDP2) THEN
                      XVAL=(((RED1-REDP1)/(RED2-REDP2))*(W1W-REDP2))+REDP1
                      IF(XVAL.LT.0.0D0) XVAL=0.0D0
                      GO TO 91
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
 91           CONTINUE
C
              REG(40)=REG(9)
              REG(9)=XVAL*100.0D0
              CALL CLOSE_FILE(32,1)
              DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
              RETURN
          ELSE
C     WQ NOT ACCX
          END IF
C     NOW INTERPOLATE THE VALUES AND DISPLAY THEM
          IF(.NOT.NOPRINT) THEN
              WRITE(OUTLYNE,10)
              CALL SHOWIT(0)
          END IF
 10       FORMAT('GEOMETRIC ENSQUARED ENERGY DISTRIBUTION')
C
          IF(SYSTEM1(30).LE.2.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
          ELSE
              J_UN='RADIAN(S)'
          END IF
          IF(.NOT.NOPRINT) THEN

              IF(W2.NE.0.0D0) WRITE(OUTLYNE,41) W2,J_UN
              IF(W2.NE.0.0D0) CALL SHOWIT(0)
 41           FORMAT('APPLIED X-OFFSET = ',G13.6,1X,A13)

              IF(W3.NE.0.0D0) WRITE(OUTLYNE,42) W3,J_UN
              IF(W3.NE.0.0D0) CALL SHOWIT(0)
 42           FORMAT('APPLIED Y-OFFSET = ',G13.6,1X,A13)

          END IF
          IF(SYSTEM1(30).LE.2.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
          ELSE
              J_UN='RADIAN(S)'
          END IF
          IF(.NOT.NOPRINT) THEN
              IF(WC(1:1).NE.'S') THEN
                  IF(SYSTEM1(30).LE.2.0D0)
     1            WRITE(OUTLYNE,70) REFRY(1,NEWIMG),J_UN
                  IF(SYSTEM1(30).GE.3.0D0)
     1            WRITE(OUTLYNE,70) REFRY(11,NEWIMG),J_UN
                  CALL SHOWIT(0)
                  IF(SYSTEM1(30).LE.2.0D0)
     1            WRITE(OUTLYNE,80) REFRY(2,NEWIMG),J_UN
                  IF(SYSTEM1(30).GE.3.0D0)
     1            WRITE(OUTLYNE,80) REFRY(12,NEWIMG),J_UN
                  CALL SHOWIT(0)
              END IF
 70           FORMAT('CHIEF RAY X-COORDINATE = ',G13.6,1X,A13)
 80           FORMAT('CHIEF RAY Y-COORDINATE = ',G13.6,1X,A13)
          ELSE
          END IF
          IF(SYSTEM1(30).LE.2.0D0) THEN
              IF(SYSTEM1(6).EQ.1.0D0) J_UN='INCH(S)'
              IF(SYSTEM1(6).EQ.2.0D0) J_UN='CENTIMETER(S)'
              IF(SYSTEM1(6).EQ.3.0D0) J_UN='MILLIMETER(S)'
              IF(SYSTEM1(6).EQ.4.0D0) J_UN='METER(S)'
          ELSE
              J_UN='RADIAN(S)'
          END IF
          IF(.NOT.NOPRINT) THEN
              WRITE(OUTLYNE,901) J_UN
              CALL SHOWIT(0)
          END IF
 901      FORMAT('PERCENT ENCLOSED ENERGY',5X,'SIDE LENGTH',1X,A13)
C
          DELSP=W1/100.0D0
          XVAL=0.0D0
          YVAL=0.0D0
          IE=-1
 100      CONTINUE
C                       CALCULATE A NEW YVAL
          YVAL=0.0D0
C     CALCULATE A NEW YVAL,POSITION
C     XVAL IS THE ENERGY
          IF(IRED.GT.2) THEN
              DO I=2,IRED-1
                  RED1=REDDAT1(I)
                  RED2=REDDAT2(I)
                  RED3=REDDAT3(I)
                  REDP1=REDDAT1(I+1)
                  REDP2=REDDAT2(I+1)
                  REDP3=REDDAT3(I+1)
                  IF(I.EQ.2.AND.REAL(XVAL).LT.REAL(RED1)) THEN
                      YVAL=0.0D0
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      DEXTREDSQ=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(I.EQ.(IRED-1).AND.REAL(XVAL).GT.REAL(REDP1)) THEN
                      YVAL=REDP2
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      EXTREDSQ=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
                  IF(REAL(XVAL).GE.REAL(RED1).AND.REAL(XVAL).LE.REAL(REDP1)) THEN
                      YVAL=(((RED2-REDP2)/(RED1-REDP1))*(XVAL-REDP1))+REDP2
                      IF(YVAL.LT.0.0D0) YVAL=0.0D0
                      IF(ABS(REAL(XVAL*100.0D0)).GT.101.0) GO TO 200
                      IF(.NOT.NOPRINT) THEN
                          WRITE(OUTLYNE,903) DABS((100.0D0*XVAL)),DABS(YVAL)
                          CALL SHOWIT(0)
                      END IF
                      IE=IE+1
                      GDTARP(IE+1)=DABS(YVAL)
                      GDTAR(IE+1)=DABS(100.0D0*XVAL)
                      ENNL=IE
                      DEXTREDSQ=.TRUE.
                      XVAL=XVAL+DELSP
                      GO TO 100
                  ELSE
C     READ NEXT DATA
                  END IF
              END DO
          ELSE
              IF(.NOT.NOPRINT) THEN
                  RED1=REDDAT1(2)
                  RED2=REDDAT2(2)
                  RED3=REDDAT3(2)
                  WRITE(OUTLYNE,903) DABS((100.0D0*RED1)),DABS(RED2)
                  CALL SHOWIT(0)
              END IF
              IE=IE+1
              GDTARP(IE+1)=DABS(YVAL)
              GDTAR(IE+1)=DABS(100.0D0*XVAL)
              ENNL=IE
              DEXTREDSQ=.TRUE.
          END IF
 903      FORMAT(5X,F8.1,19X,G13.6)
 200      CONTINUE
C
C     ENERGY CALCULATIONS ARE COMPLETE
C
          CALL CLOSE_FILE(32,1)
          DEALLOCATE(REDDAT1,REDDAT2,REDDAT3,STAT=ALLOERR)
          RETURN
      END
C SUB OPSPOTSET.FOR

      SUBROUTINE OPSPOTSET
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE OPSPOTSET.FOR. THIS DOES ALL SPOT SETUPS.
C     FOR OPTIM AND TOLERANCING
C
          INTEGER I,IV
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(WC.EQ.'OPSPDRST') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"OPSPDRST" RESETS OPSPOT DIAGRAM SETTINGS TO THEIR DEFAULTS'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"OPSPDRST" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       INPUT OK
C
C     DEFAULT VALUES FOR SPOT DIAGRAMS
                  OPNRECT=10
                  OPRINGTOT=4
                  OPRNUMBR=200
                  OPSPDTYPE=2
                  OPRINGRAD(0)=0.0D0
                  OPRINGRAD(1)=0.4D0
                  OPRINGRAD(2)=0.7D0
                  OPRINGRAD(3)=0.866D0
                  OPRINGRAD(4)=1.0D0
                  OPRINGPNT(0)=1
                  OPRINGPNT(1)=8
                  OPRINGPNT(2)=8
                  OPRINGPNT(3)=8
                  OPRINGPNT(4)=8
                  OPRINGANG(0)=0.0D0
                  OPRINGANG(1)=0.0D0
                  OPRINGANG(2)=0.0D0
                  OPRINGANG(3)=0.0D0
                  OPRINGANG(4)=0.0D0
                  RETURN
C
              END IF
          ELSE
C     NOT WC=OPSPDRST
          END IF
          IF(WC.EQ.'OPSPOT') THEN
              IF(STI.EQ.1.OR.SN.EQ.0.AND.SQ.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE=
     1            '"OPSPOT" SETS OPSPOT DIAGRAM GRID TYPE'
                  CALL SHOWIT(1)
                  IF(OPSPDTYPE.EQ.1) OUTLYNE=
     1            'CURRENT OPSPOT DIAGRAM GRID TYPE IS "RECT"'
                  IF(OPSPDTYPE.EQ.2) OUTLYNE=
     1            'CURRENT OPSPOT DIAGRAM GRID TYPE IS "RING"'
                  IF(OPSPDTYPE.EQ.3) OUTLYNE=
     1            'CURRENT OPSPOT DIAGRAM GRID TYPE IS "RAND"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"OPSPOT" TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  OUTLYNE='"OPSPOT" REQUIRES EXPLICIT QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.NE.'RECT'.AND.WQ.NE.'RING'.AND.WQ.NE.'RAND') THEN
                  OUTLYNE=
     1            'VALID "OPSPOT" QUALIFIERS ARE "RECT", "RING" OR "RAND"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     INPUT OK
              IF(WQ.EQ.'RECT') OPSPDTYPE=1
              IF(WQ.EQ.'RING') OPSPDTYPE=2
              IF(WQ.EQ.'RAND') OPSPDTYPE=3
              RETURN
          ELSE
C     NOT WC=OPSPOT
          END IF
          IF(WC.EQ.'OPRINGS') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE=
     1            '"OPRINGS" SETS THE NUMBER OF OPSPOT DIAGRAM RINGS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'CURRENT NUMBER OF RINGS IS ',OPRINGTOT
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"OPRINGS" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"OPRINGS" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     INPUT OK
              IF(W1.LT.1.0D0.OR.W1.GT.50.0D0) THEN
                  OUTLYNE=
     1            'NUMBER OF RINGS ALLOWED RANGES FROM 1 TO 50'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     CHECK FOR OVERFLOW
              IV=0
              DO I=0,INT(W1)
                  IV=IV+OPRINGPNT(I)
              END DO
              OPRINGTOT=INT(W1)
          END IF
          IF(WC.EQ.'OPRECT') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE=
     1            '"OPRECT" SETS THE RECTANGULAR OPSPOT GRID SIZE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'CURRENT SIZE IS ',OPNRECT,' BY ',OPNRECT
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"OPRECT" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"OPRECT" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     INPUT OK

              OPNRECT=INT(W1)
              RETURN
          END IF
          IF(WC.EQ.'OPRANNUM') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE=
     1            '"OPRANNUM" SETS THE OPSPOT DIAGRAM RANDOM PATTERN SIZE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'CURRENT SIZE IS ',OPRNUMBR, 'TOTAL RAYS'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"OPRANNUM" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"OPRANNUM" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.10.0D0.OR.W1.GT.100.0D0) THEN
                  OUTLYNE=
     1            '"OPRANNUM" MAY RANGE FROM 10 TO 100 RAYS PER COLOR'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     INPUT OK
              OPRNUMBR=INT(W1)
              RETURN
          ELSE
C     NOT WC=OPRANNUM
          END IF
          IF(WC.EQ.'OPRING') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE=
     1            '"OPRING" SETS THE OPSPOT RING PARAMETERS'
                  WRITE(OUTLYNE,200)
                  CALL SHOWIT(1)
                  DO I=0,int(OPRINGTOT)
                      WRITE(OUTLYNE,100) I,OPRINGRAD(I),OPRINGPNT(I),OPRINGANG(I)
                      CALL SHOWIT(1)
 200                  FORMAT('RING#',4X,'RADIUS',4X,'# OF RAYS',1X,'OFF-SET ANGLE')
 100                  FORMAT(2X,I2,2X,G13.6,4X,I4,3X,G13.6)
                  END DO
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"OPRING" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S5.EQ.1) THEN
                  OUTLYNE=
     1            '"OPRING" TAKES NO NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.1.OR.INT(W1).GT.OPRINGTOT) THEN
                  WRITE(OUTLYNE,*)
     1            'RING NUMBER MAY RANGE FROM 1 TO ',int(OPRINGTOT)
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LE.0.0D0.OR.W2.GT.1.0D0) THEN
                  OUTLYNE=
     1            'RING RADIUS MUST BE GREATER THAN ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'AND LESS THAN OR EQUAL TO 1.0'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W3).LT.2.OR.INT(W3).GT.100) THEN
                  OUTLYNE=
     1            'RAYS PER RING MAY RANGE FROM 2 TO 100'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W4.LT.0.0D0.OR.W4.GT.360.0D0) THEN
                  OUTLYNE=
     1            'RING OFF-SET ANGLE MAY RANGE FROM 0.0 TO 360.0 DEGREES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     INPUT OK
C     CHECK FOR OVERFLOW
              IV=0
              DO I=0,INT(W1)-1
                  IV=IV+OPRINGPNT(I)
              END DO
              DO I=INT(W1)+1,int(OPRINGTOT)
                  IV=IV+OPRINGPNT(I)
              END DO
              IV=IV+INT(W3)
              OPRINGPNT(INT(W1))=INT(W3)
              OPRINGRAD(INT(W1))=W2
              OPRINGANG(INT(W1))=W4
              RETURN
          ELSE
C     NOT WC=OPRINGS
          END IF
          RETURN
      END
C SUB SPOTSET.FOR

      SUBROUTINE SPOTSET
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE OPSPOTSET.FOR. THIS DOES ALL SPOT SETUPS
C     EXCEPT FOR OPTIM AND TOLERANCING
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(WC.EQ.'SPDRESET') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"SPDRESET" RESETS SPOT DIAGRAM SETTINGS TO THEIR DEFAULTS'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"SPDRESET" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       INPUT OK
C
C     DEFAULT VALUES FOR SPOT DIAGRAMS
                  NRECT=10
                  RINGTOT=4
                  RNUMBR=200
                  SPDTYPE=2
                  RINGRAD(0)=0.0D0
                  RINGRAD(1)=0.4D0
                  RINGRAD(2)=0.7D0
                  RINGRAD(3)=0.866D0
                  RINGRAD(4)=1.0D0
                  RINGPNT(0)=1
                  RINGPNT(1)=8
                  RINGPNT(2)=8
                  RINGPNT(3)=8
                  RINGPNT(4)=8
                  RINGANG(0)=0.0D0
                  RINGANG(1)=0.0D0
                  RINGANG(2)=0.0D0
                  RINGANG(3)=0.0D0
                  RINGANG(4)=0.0D0
                  RETURN
C
              END IF
          ELSE
C     NOT WC=SPDRESET
          END IF
          IF(WC.EQ.'SPOT') THEN
              IF(STI.EQ.1.OR.SN.EQ.0.AND.SQ.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE=
     1            '"SPOT" SETS SPOT DIAGRAM GRID TYPE'
                  CALL SHOWIT(1)
                  IF(SPDTYPE.EQ.1) OUTLYNE=
     1            'CURRENT SPOT DIAGRAM GRID TYPE IS "RECT"'
                  IF(SPDTYPE.EQ.2) OUTLYNE=
     1            'CURRENT SPOT DIAGRAM GRID TYPE IS "RING"'
                  IF(SPDTYPE.EQ.3) OUTLYNE=
     1            'CURRENT SPOT DIAGRAM GRID TYPE IS "RAND"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"SPOT" TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  OUTLYNE='"SPOT" REQUIRES EXPLICIT QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.NE.'RECT'.AND.WQ.NE.'RING'.AND.WQ.NE.'RAND') THEN
                  OUTLYNE=
     1            'VALID "SPOT" QUALIFIERS ARE "RECT", "RING" OR "RAND"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     INPUT OK
              IF(WQ.EQ.'RECT') SPDTYPE=1
              IF(WQ.EQ.'RING') SPDTYPE=2
              IF(WQ.EQ.'RAND') SPDTYPE=3
              RETURN
          ELSE
C     NOT WC=SPOT
          END IF
          IF(WC.EQ.'RINGS') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE=
     1            '"RINGS" SETS THE NUMBER OF SPOT DIAGRAM RINGS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'CURRENT NUMBER OF RINGS IS ',RINGTOT
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"RINGS" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"RINGS" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     INPUT OK
              IF(W1.LT.1.0D0.OR.W1.GT.50.0D0) THEN
                  OUTLYNE=
     1            'NUMBER OF RINGS ALLOWED RANGES FROM 1 TO 50'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              RINGTOT=INT(W1)
          END IF
          IF(WC.EQ.'RECT') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE=
     1            '"RECT" SETS THE RECTANGULAR SPOT GRID SIZE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'CURRENT SIZE IS ',NRECT,' BY ',NRECT
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"RECT" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"RECT" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.2.0D0) THEN
                  OUTLYNE=
     1            'SMALLEST GRID SIZE ALLOWED IS 2x2'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              NRECT=INT(W1)
              RETURN
          END IF
          IF(WC.EQ.'RANNUM') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE=
     1            '"RANNUM" SETS THE SPOT DIAGRAM RANDOM PATTERN SIZE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'CURRENT SIZE IS ',RNUMBR, 'TOTAL RAYS'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"RANNUM" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"RANNUM" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.100.0D0) THEN
                  OUTLYNE=
     1            'MINIMUM "RANNUM" IS 100 RAYS PER COLOR'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     INPUT OK
              RNUMBR=INT(W1)
              RETURN
          END IF
          IF(WC.EQ.'RING') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE=
     1            '"RING" SETS THE SPOT RING PARAMETERS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,200)
                  CALL SHOWIT(1)
                  DO I=0,RINGTOT
                      WRITE(OUTLYNE,100) I,RINGRAD(I),RINGPNT(I),RINGANG(I)
                      CALL SHOWIT(1)
 200                  FORMAT('RING#',4X,'RADIUS',4X,'# OF RAYS',1X,'OFF-SET ANGLE')
 100                  FORMAT(2X,I2,2X,G13.6,4X,I4,3X,G13.6)
                  END DO
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"RING" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S5.EQ.1) THEN
                  OUTLYNE=
     1            '"RING" TAKES NO NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.1.OR.INT(W1).GT.RINGTOT) THEN
                  WRITE(OUTLYNE,*)
     1            'RING NUMBER MAY RANGE FROM 1 TO ',RINGTOT
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LE.0.0D0.AND.W2.GT.1.0D0) THEN
                  OUTLYNE=
     1            'RING RADIUS MUST BE GREATER THAN ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'AND LESS THAN OR EQUAL TO 1.0'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W3).LT.2.OR.INT(W3).GT.2500) THEN
                  OUTLYNE=
     1            'RAYS PER RING MAY RANGE FROM 2 TO 2500'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W4.LT.0.0D0.OR.W4.GT.360.0D0) THEN
                  OUTLYNE=
     1            'RING OFF-SET ANGLE MAY RANGE FROM 0.0 TO 360.0 DEGREES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     INPUT OK
              RINGPNT(INT(W1))=INT(W3)
              RINGRAD(INT(W1))=W2
              RINGANG(INT(W1))=W4
              RETURN
          ELSE
C     NOT WC=RINGS
          END IF
          IF(WC.EQ.'APOD') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE=
     1            '"APOD" SETS THE APODIZATION SETTINGS'
                  CALL SHOWIT(1)
                  OUTLYNE= 'CURRENT SETTINGS ARE:'
                  CALL SHOWIT(1)
                  IF(APODGAUSS) THEN
                      WRITE(OUTLYNE,*)'FORM = GAUSSIAN'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'LOSS AT REFERENCE SURFACE EDGE = '
     1                ,APODDBLOSS, 'DECIBLES'
                      CALL SHOWIT(1)
                  ELSE
                      WRITE(OUTLYNE,*)'FORM = UNIFORM'
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  OUTLYNE='"APOD" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  OUTLYNE=
     1            '"APOD" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"APOD" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.NE.'NONE'.AND.WQ.NE.'GAUSS') THEN
                  OUTLYNE=
     1            '"APOD" ONLY TAKES "NON" OR "GAUSS" AS QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'NONE') APODGAUSS=.FALSE.
              IF(WQ.EQ.'GAUSS') APODGAUSS=.TRUE.
              IF(DF1.EQ.1) W1=0.0D0
C     INPUT OK
              APODDBLOSS=W1
              RETURN
          ELSE
C     NOT WC=APOD
          END IF
          RETURN
      END
