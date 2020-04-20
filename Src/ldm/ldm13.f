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

C       THIRTEENTH FILE FOR LENS DATABASE MANAGER FILES

C SUB FNDGLS.FOR
      SUBROUTINE FNDGLS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FNDGLS WHICH IMPLEMENTS THE FINDGLAS
C       COMMAND AT THE CMD LEVEL.
C
          INTEGER NUMSRCH,ALLOERR,I,J,K,TOTAL,II,JJ,KK
          REAL*8 SORTIT,A0,A1,A2,A3,A4,A5,D,NMOD,NCAT
          REAL*8 LAMBDA,PN,PNSC,SORTIT2
          DIMENSION SORTIT(:),SORTIT2(:)
          CHARACTER ASORTIT*13,NAME*13,NUMBER*13,ASORTIT2*13
          DIMENSION ASORTIT(:,:),ASORTIT2(:,:)
          ALLOCATABLE :: SORTIT,ASORTIT,SORTIT2,ASORTIT2
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          PN(LAMBDA,A0,A1,A2,A3,A4,A5)=
     1    DSQRT(A0+(A1*(LAMBDA**2))+(A2*(1.0D0/(LAMBDA**2)))+
     2    (A3*(1.0D0/(LAMBDA**4)))+(A4*(1.0D0/(LAMBDA**6)))+
     3    (A5*(1.0D0/(LAMBDA**8))))
C
          PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)=
     1    DSQRT(
     1    ((A0*(LAMBDA**2))/((LAMBDA**2)-A3))+
     2    ((A1*(LAMBDA**2))/((LAMBDA**2)-A4))+
     2    ((A2*(LAMBDA**2))/((LAMBDA**2)-A5))+1.0D0)
C
          DEALLOCATE(SORTIT,ASORTIT,SORTIT2,ASORTIT2,STAT=ALLOERR)
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"FINDGLAS" SEARCHES FOR GLASSES CLOSE TO A "MODEL" GLASS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE='"FINDGLAS" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              SQ=1
              WQ='ALL'
          END IF
          IF(WQ.NE.'SCHOTT'.AND.WQ.NE.'HOYA'.AND.WQ.NE.'OHARA'
     1    .AND.WQ.NE.'CORNIN'.AND.WQ.NE.'CHANCE'.AND.WQ.NE.'ALL'
     1    .AND.WQ.NE.'HIKARI'.AND.WQ.NE.'SCH2000') THEN
              OUTLYNE='INVALID CATALOG NAME ISSUED WITH "FINDGLAS"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"FINDGLAS" ONLY TAKES NUMERIC WORDS #1 AND #2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE='"FINDGLAS" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
              OUTLYNE='SURFACE NUMBER BEYOND LEGAL BOUNDS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1) THEN
              DF2=0
              S2=1
              W2=5.0D0
          END IF
          IF(W2.LT.1.0D0) THEN
              OUTLYNE='"N" MUST BE 1 GREATER'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          NUMSRCH=INT(W2)
          IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
              OUTLYNE='SURFACE NUMBER BEYOND LEGAL BOUNDS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(GLANAM(INT(W1),1)(1:5).NE.'MODEL') THEN
              OUTLYNE='GLASS IS NOT A "MODEL" GLASS'
              CALL SHOWIT(1)
              OUTLYNE='"FINDGLAS" TOOK NO ACTION'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

          ALLOCATE(SORTIT(1:10000),ASORTIT(1:2,1:10000),STAT=ALLOERR)
          SORTIT(1:10000)=0.0D0
          ASORTIT(1:2,1:10000)='             '

C     NOW SEARCH THE CATALOGS AND CALCULATE THE DISTANCE AND TABULATE IT
          IF(WQ.EQ.'SCHOTT') THEN
              K=0
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'SCHOTT.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL

              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='SCHOTT       '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              GO TO 999
          END IF

          IF(WQ.EQ.'SCH2000') THEN
              K=0
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'SCH2000.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='SCH2000      '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              GO TO 999
          END IF

          IF(WQ.EQ.'HOYA') THEN
              K=0
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'HOYA.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='HOYA         '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              GO TO 999
          END IF
          IF(WQ.EQ.'HIKARI') THEN
              K=0
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'HIKARI.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='HIKARI       '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              GO TO 999
          END IF
          IF(WQ.EQ.'OHARA') THEN
              K=0
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'OHARA.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='OHARA        '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
C     NOW THE SECOND OHARA FILE
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'OHARA-O.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='OHARA        '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              GO TO 999
          END IF
          IF(WQ.EQ.'CORNIN') THEN
              K=0
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'CORNIN.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='CORNIN       '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              GO TO 999
          END IF
          IF(WQ.EQ.'CHANCE') THEN
              K=0
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'CHANCE.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='CHANCE       '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              GO TO 999
          END IF
          IF(WQ.EQ.'ALL') THEN
              K=0
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'SCHOTT.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='SCHOTT       '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'SCH2000.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='SCH2000      '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'HOYA.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='HOYA         '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'HIKARI.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='HIKARI       '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'OHARA.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='OHARA        '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'OHARA-O.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='OHARA        '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'CORNIN.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='CORNIN       '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              OPEN(UNIT=99,ACCESS='DIRECT',FILE=trim(LIBGLA)//'CHANCE.BIN',
     1        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
              READ(UNIT=99,REC=1) TOTAL
              DO J=2,TOTAL+1
                  READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
                  D=0.0D0
                  DO I=1,10
                      IF(I.EQ.1)  II=31
                      IF(I.EQ.2)  II=32
                      IF(I.EQ.3)  II=33
                      IF(I.EQ.4)  II=34
                      IF(I.EQ.5)  II=35
                      IF(I.EQ.6)  II=76
                      IF(I.EQ.7)  II=77
                      IF(I.EQ.8)  II=78
                      IF(I.EQ.9)  II=79
                      IF(I.EQ.10) II=80
                      IF(I.EQ.1)  JJ=1
                      IF(I.EQ.2)  JJ=2
                      IF(I.EQ.3)  JJ=3
                      IF(I.EQ.4)  JJ=4
                      IF(I.EQ.5)  JJ=5
                      IF(I.EQ.6)  JJ=71
                      IF(I.EQ.7)  JJ=72
                      IF(I.EQ.8)  JJ=73
                      IF(I.EQ.9)  JJ=74
                      IF(I.EQ.10) JJ=75
                      IF(I.EQ.1)  KK=46
                      IF(I.EQ.2)  KK=47
                      IF(I.EQ.3)  KK=48
                      IF(I.EQ.4)  KK=49
                      IF(I.EQ.5)  KK=50
                      IF(I.EQ.6)  KK=71
                      IF(I.EQ.7)  KK=72
                      IF(I.EQ.8)  KK=73
                      IF(I.EQ.9)  KK=74
                      IF(I.EQ.10) KK=75
                      IF(SYSTEM1(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
                          IF(JJ.GE.1.AND.JJ.LE.5)
     1                    LAMBDA=SYSTEM1(JJ)
                          IF(JJ.GE.6.AND.JJ.LE.10)
     1                    LAMBDA=SYSTEM1(65+JJ)
C     GET INDEX OF MODEL GLASS
                          NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
                          NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
                          D=D+((NMOD-NCAT)**2)
                      END IF
                  END DO
                  K=K+1
                  ASORTIT(1,K)='CHANCE       '
                  ASORTIT(2,K)=NAME
                  SORTIT(K)=DSQRT(D)
              END DO
C     FINISHED LOADING, GO TO THE SORT
              CALL CLOSE_FILE(99,1)
              GO TO 999
          END IF
 999      CONTINUE
C     SORT SORTIT IN ASSENDING ORDER AND REORDER
C      THE ASORTIT ARRAY AT THE SAME TIME
          ALLOCATE(SORTIT2(1:K),ASORTIT2(1:2,1:K),STAT=ALLOERR)
          SORTIT2(1:K)=SORTIT(1:K)
          ASORTIT2(1:2,1:K)=ASORTIT(1:2,1:K)
          CALL SORT_JK(K,SORTIT2,ASORTIT2)
 10       FORMAT('THE CLOSEST ',I3,' CATALOG GLASSES')
 20       FORMAT('FOUND IN THE LAST "FINDGLAS" SEARCH')
 30       FORMAT('FOR SURFACE #',I3,' ARE:')
          WRITE(OUTLYNE,10) NUMSRCH
          CALL SHOWIT(0)
          WRITE(OUTLYNE,20)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,30) INT(W1)
          CALL SHOWIT(0)
 50       FORMAT('  # ',1X,'CAT. NAME    ',1X,'GLASS NAME   ',6X,'DISTANCE')
          WRITE(OUTLYNE,50)
          CALL SHOWIT(0)
          IF(NUMSRCH.GT.5000) NUMSRCH=5000
          DO I=1,NUMSRCH

              IF(ASORTIT2(1,I)(1:13).NE.'SCHOTT       '.AND.
     1         ASORTIT2(1,I)(1:13).NE.'SCH2000      '.AND.
     1         ASORTIT2(1,I)(1:13).NE.'OHARA        '.AND.
     2         ASORTIT2(1,I)(1:13).NE.'HOYA         '.AND.
     3         ASORTIT2(1,I)(1:13).NE.'CHANCE       '.AND.
     4         ASORTIT2(1,I)(1:13).NE.'CORNIN       '.AND.
     5         ASORTIT2(1,I)(1:13).NE.'HIKARI       ') THEN
                  GO TO 41
              ELSE
                  WRITE(OUTLYNE,40) I,ASORTIT2(1,I),ASORTIT2(2,I),SORTIT2(I)
                  CALL SHOWIT(0)
              END IF
          END DO
 41       CONTINUE
 40       FORMAT(I4,1X,A13,1X,A13,1X,G23.15)
C
          DEALLOCATE(SORTIT,ASORTIT,STAT=ALLOERR)
          DEALLOCATE(SORTIT2,ASORTIT2,STAT=ALLOERR)
          RETURN
      END
      SUBROUTINE GLASSP_RUSSIAN1
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          CHARACTER*13 RNAME(1:113)
          INTEGER I
          RNAME(1)=   'LK1'
          RNAME(2)=   'LK3'
          RNAME(3)=   'LK4'
          RNAME(4)=   'LK5'
          RNAME(5)=   'LK6'
          RNAME(6)=   'LK7'
          RNAME(7)=   'LK8'
          RNAME(8)=   'LK11'
          RNAME(9)=   'LK13'
          RNAME(10)=  'LK14'
          RNAME(11)=  'TFK1'
          RNAME(12)=  'K100'
          RNAME(13)=  'K2'
          RNAME(14)=  'K3'
          RNAME(15)=  'K8'
          RNAME(16)=  'K14'
          RNAME(17)=  'K15'
          RNAME(18)=  'K18'
          RNAME(19)=  'K19'
          RNAME(20)=  'K20'
          RNAME(21)=  'BK4'
          RNAME(22)=  'BK6'
          RNAME(23)=  'BK8'
          RNAME(24)=  'BK10'
          RNAME(25)=  'BK13'
          RNAME(26)=  'TK2'
          RNAME(27)=  'TK4'
          RNAME(28)=  'TK8'
          RNAME(29)=  'TK9'
          RNAME(30)=  'TK12'
          RNAME(31)=  'TK13'
          RNAME(32)=  'TK14'
          RNAME(33)=  'TK16'
          RNAME(34)=  'TK17'
          RNAME(35)=  'TK20'
          RNAME(36)=  'TK21'
          RNAME(37)=  'TK23'
          RNAME(38)=  'STK3'
          RNAME(39)=  'STK7'
          RNAME(40)=  'STK8'
          RNAME(41)=  'STK9'
          RNAME(42)=  'STK10'
          RNAME(43)=  'STK12'
          RNAME(44)=  'STK15'
          RNAME(45)=  'STK16'
          RNAME(46)=  'STK19'
          RNAME(47)=  'STK20'
          RNAME(48)=  'OK1'
          RNAME(49)=  'OK2'
          RNAME(50)=  'KF4'
          RNAME(51)=  'KF6'
          RNAME(52)=  'KF7'
          RNAME(53)=  'BF1'
          RNAME(54)=  'BF4'
          RNAME(55)=  'BF6'
          RNAME(56)=  'BF7'
          RNAME(57)=  'BF8'
          RNAME(58)=  'BF11'
          RNAME(59)=  'BF12'
          RNAME(60)=  'BF13'
          RNAME(61)=  'BF16'
          RNAME(62)=  'BF21'
          RNAME(63)=  'BF24'
          RNAME(64)=  'BF25'
          RNAME(65)=  'BF26'
          RNAME(66)=  'BF27'
          RNAME(67)=  'BF28'
          RNAME(68)=  'BF32'
          RNAME(69)=  'TBF3'
          RNAME(70)=  'TBF4'
          RNAME(71)=  'TBF8'
          RNAME(72)=  'TBF9'
          RNAME(73)=  'TBF25'
          RNAME(74)=  'TBF10'
          RNAME(75)=  'TBF11'
          RNAME(76)=  'LF5'
          RNAME(77)=  'LF7'
          RNAME(78)=  'LF8'
          RNAME(79)=  'LF9'
          RNAME(80)=  'LF10'
          RNAME(81)=  'LF11'
          RNAME(82)=  'LF12'
          RNAME(83)=  'F1'
          RNAME(84)=  'F2'
          RNAME(85)=  'F4'
          RNAME(86)=  'F6'
          RNAME(87)=  'F9'
          RNAME(88)=  'F13'
          RNAME(89)=  'F18'
          RNAME(90)=  'TF1'
          RNAME(91)=  'TF2'
          RNAME(92)=  'TF3'
          RNAME(93)=  'TF4'
          RNAME(94)=  'TF5'
          RNAME(95)=  'TF7'
          RNAME(96)=  'TF8'
          RNAME(97)=  'TF10'
          RNAME(98)=  'TF11'
          RNAME(99)=  'TF12'
          RNAME(100)= 'TF13'
          RNAME(101)= 'ST2'
          RNAME(102)= 'ST3'
          RNAME(103)= 'ST11'
          RNAME(104)= 'OF1'
          RNAME(105)= 'OF3'
          RNAME(106)= 'OF4'
          RNAME(107)= 'OF5'
          RNAME(108)= 'OF6'
          RNAME(109)= 'KY1'
          RNAME(110)= 'KY2'
          RNAME(111)= 'KB'
          RNAME(112)= 'KI'
          RNAME(113)= 'KB-P'
1         FORMAT('RUSSIAN       ',A13)
          DO I=1,113



              WRITE(OUTLYNE,1) RNAME(I)(1:13)
              CALL SHOWIT(0)
          END DO
          RETURN
      END


      SUBROUTINE GLASSP_RUSSIAN2
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          CHARACTER*13 RNAME(1:113),NAME1,NAME2
          CHARACTER*12 FLNAME
          REAL*8 LAMUPP,LAMLOW
          INTEGER I
          RNAME(1)=   'LK1'
          RNAME(2)=   'LK3'
          RNAME(3)=   'LK4'
          RNAME(4)=   'LK5'
          RNAME(5)=   'LK6'
          RNAME(6)=   'LK7'
          RNAME(7)=   'LK8'
          RNAME(8)=   'LK11'
          RNAME(9)=   'LK13'
          RNAME(10)=  'LK14'
          RNAME(11)=  'TFK1'
          RNAME(12)=  'K100'
          RNAME(13)=  'K2'
          RNAME(14)=  'K3'
          RNAME(15)=  'K8'
          RNAME(16)=  'K14'
          RNAME(17)=  'K15'
          RNAME(18)=  'K18'
          RNAME(19)=  'K19'
          RNAME(20)=  'K20'
          RNAME(21)=  'BK4'
          RNAME(22)=  'BK6'
          RNAME(23)=  'BK8'
          RNAME(24)=  'BK10'
          RNAME(25)=  'BK13'
          RNAME(26)=  'TK2'
          RNAME(27)=  'TK4'
          RNAME(28)=  'TK8'
          RNAME(29)=  'TK9'
          RNAME(30)=  'TK12'
          RNAME(31)=  'TK13'
          RNAME(32)=  'TK14'
          RNAME(33)=  'TK16'
          RNAME(34)=  'TK17'
          RNAME(35)=  'TK20'
          RNAME(36)=  'TK21'
          RNAME(37)=  'TK23'
          RNAME(38)=  'STK3'
          RNAME(39)=  'STK7'
          RNAME(40)=  'STK8'
          RNAME(41)=  'STK9'
          RNAME(42)=  'STK10'
          RNAME(43)=  'STK12'
          RNAME(44)=  'STK15'
          RNAME(45)=  'STK16'
          RNAME(46)=  'STK19'
          RNAME(47)=  'STK20'
          RNAME(48)=  'OK1'
          RNAME(49)=  'OK2'
          RNAME(50)=  'KF4'
          RNAME(51)=  'KF6'
          RNAME(52)=  'KF7'
          RNAME(53)=  'BF1'
          RNAME(54)=  'BF4'
          RNAME(55)=  'BF6'
          RNAME(56)=  'BF7'
          RNAME(57)=  'BF8'
          RNAME(58)=  'BF11'
          RNAME(59)=  'BF12'
          RNAME(60)=  'BF13'
          RNAME(61)=  'BF16'
          RNAME(62)=  'BF21'
          RNAME(63)=  'BF24'
          RNAME(64)=  'BF25'
          RNAME(65)=  'BF26'
          RNAME(66)=  'BF27'
          RNAME(67)=  'BF28'
          RNAME(68)=  'BF32'
          RNAME(69)=  'TBF3'
          RNAME(70)=  'TBF4'
          RNAME(71)=  'TBF8'
          RNAME(72)=  'TBF9'
          RNAME(73)=  'TBF25'
          RNAME(74)=  'TBF10'
          RNAME(75)=  'TBF11'
          RNAME(76)=  'LF5'
          RNAME(77)=  'LF7'
          RNAME(78)=  'LF8'
          RNAME(79)=  'LF9'
          RNAME(80)=  'LF10'
          RNAME(81)=  'LF11'
          RNAME(82)=  'LF12'
          RNAME(83)=  'F1'
          RNAME(84)=  'F2'
          RNAME(85)=  'F4'
          RNAME(86)=  'F6'
          RNAME(87)=  'F9'
          RNAME(88)=  'F13'
          RNAME(89)=  'F18'
          RNAME(90)=  'TF1'
          RNAME(91)=  'TF2'
          RNAME(92)=  'TF3'
          RNAME(93)=  'TF4'
          RNAME(94)=  'TF5'
          RNAME(95)=  'TF7'
          RNAME(96)=  'TF8'
          RNAME(97)=  'TF10'
          RNAME(98)=  'TF11'
          RNAME(99)= 'TF12'
          RNAME(100)= 'TF13'
          RNAME(101)= 'ST2'
          RNAME(102)= 'ST3'
          RNAME(103)= 'ST11'
          RNAME(104)= 'OF1'
          RNAME(105)= 'OF3'
          RNAME(106)= 'OF4'
          RNAME(107)= 'OF5'
          RNAME(108)= 'OF6'
          RNAME(109)= 'KY1'
          RNAME(110)= 'KY2'
          RNAME(111)= 'KB'
          RNAME(112)= 'KI'
          RNAME(113)= 'KB-P'
1         FORMAT('RUSSIAN       ',A13)
10        FORMAT('Normal Cubic Spline Fit to SOVOPTIKS Data')
          DO I=1,113
              IF(WS(1:13).EQ.RNAME(I)(1:13)) THEN
 991              FORMAT('WAVELENGTH RANGE:')
 992              FORMAT('LOWER WAVELENGTH = ',G15.8,' MICRONS')
 993              FORMAT('UPPER WAVELENGTH = ',G15.8,' MICRONS')
                  NAME1='RUSSIAN      '
                  NAME2=RNAME(I)(1:13)
                  FLNAME=' '
                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                  WRITE(OUTLYNE,991)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,992) LAMLOW
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,993) LAMUPP
                  CALL SHOWIT(0)



                  WRITE(OUTLYNE,1) RNAME(I)(1:13)
                  CALL SHOWIT(0)



                  WRITE(OUTLYNE,10)
                  CALL SHOWIT(0)
                  GO TO 3
              ELSE
              END IF
          END DO



          OUTLYNE='THE REQUESTED GLASS NAME WAS NOT FOUND'
          CALL SHOWIT(1)
          OUTLYNE='RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
3         CONTINUE
          RETURN
      END


C SUB GLASSP.FOR
      SUBROUTINE GLASSP
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GLASSP WHICH IMPLEMENTS THE GLASSP
C       COMMAND AT THE CMD LEVEL.
C
          INTEGER J,TOTAL
C
          REAL*8 A0,A1,A2,A3,A4,A5,LAMUPP,LAMLOW
C
          CHARACTER NAME*13,NUMBER*13,FLNAME*12,NAME1*13,NAME2*13
     1    ,CATNAME*13
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
!        LOGICAL EXIS_USER
C
C
          IF(SN.EQ.1) THEN
              OUTLYNE='"GLASSP" TAKES NO NUMERIC WORD INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.AND.SQ.EQ.0) THEN
              OUTLYNE='"THE GLASS CATALOG NAME IS MISSING'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     SYNTAX OK, PROCEED
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"GLASSP" INTERROGATES GLASS CATALOGS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SST.EQ.0.AND.SQ.EQ.0) THEN
C     JUST GLASSP
              WRITE(OUTLYNE,100)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,102)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,103)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,104)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,105)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,108)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,106)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,117)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,116)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2161)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,107)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2107)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2081)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2082)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2083)
              CALL SHOWIT(0)
 100          FORMAT(
     1        'THE CURRENT PROGRAM GLASS CATALOG NAMES ARE:')
 101          FORMAT(
     1        10X,'"SCHOTT"   (Schott Optical Glass Catalog)')
 102          FORMAT(
     1        10X,'"HOYA"     (Hoya Optical Glass Catalog)')
 103          FORMAT(
     1        10X,'"OHARA"    (Ohara Optical Glass Catalog)')
 104          FORMAT(
     1        10X,'"CORNIN"   (Corning-France Optical Glass Catalog)')
 105          FORMAT(
     1        10X,'"CHANCE"   (Chance-Pilkington Optical Glass Catalog)')
 108          FORMAT(
     1        10X,'"HIKARI"   (HIKARI Optical Glass Catalog)')
 106          FORMAT(
     1        10X,'"GLCAT     (Searches all catalogs)')
 117          FORMAT(
     1        10X,
     2        '"GLASS     (Searches all catalogs)')
 116          FORMAT(
     1        10X,'            [specified without giving catalog name]')
 2161         FORMAT(
     1        10X,'              [but just by glass name or number]')
 107          FORMAT(
     1        10X,'"MATL"     (Special IR, UV, Visual Materials and Plastics)')
 2107         FORMAT(
     1        10X,'"RUSSIAN"  (SOVOPTICS glass catalog)')
 2081         FORMAT(
     1        10X,'"RADHARD"  (Radiation Resistant Optical Materials)')
 2082         FORMAT(
     1        10X,'"USER"     (Use "EDIT USER.DAT" from the command line)')
 2083         FORMAT(
     1        10X,'"SCH2000"  (SCHOTT POST-2000 Optical Glass Catalog)')
              RETURN
          ELSE
C     QUALIFIER AND STRING NOT BLANK
          END IF
          IF(SQ.EQ.1.AND.SST.EQ.0) THEN
C     OPEN THE APPROPRIATE CATALOG AND DISPLAY THE GLASS NAMES
C     AND NUMBERS
C       IS THE SURFACE CATALOG IMPLEMENTED ?
              IF(WQ.NE.'SCHOTT'.AND.WQ.NE.'CHANCE'.AND.WQ.NE.'GLA'
     1        .AND.WQ.NE.'OHARA'.AND.WQ.NE.'CORNIN'.AND.WQ.NE.'HIKARI'
     2        .AND.WQ.NE.'HOYA'.AND.WQ.NE.'GLCAT'.AND.WQ.NE.'MATL'
     3        .AND.WQ.NE.'RUSSIAN'.AND.WQ.NE.'RADHARD'.AND.WQ.NE.'USER'
     4        .AND.WQ.NE.'SCH2000') THEN
C     NOT A VALID CATALOG
                  WRITE(OUTLYNE,*) WQ
                  CALL SHOWIT(1)
                  OUTLYNE='THE REQUESTED GLASS CATALOG WAS NOT RECOGNIZED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  IF(WQ.EQ.'USER') THEN
                      WRITE(OUTLYNE,2082)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(WQ.NE.'GLCAT'.AND.WQ.NE.'GLA') THEN
C     VALID SINGLE CATALOG, PROCEED LISTING GLASSES
C     DETERMINE FILE NAME
                      IF(WQ.NE.'MATL'.AND.WQ.NE.'RUSSIAN') THEN
                          IF(WQ.EQ.'SCHOTT')   FLNAME='SCHOTT.BIN  '
                          IF(WQ.EQ.'SCH2000')  FLNAME='SCH2000.BIN '
                          IF(WQ.EQ.'HOYA')     FLNAME='HOYA.BIN    '
                          IF(WQ.EQ.'HIKARI')   FLNAME='HIKARI.BIN  '
                          IF(WQ.EQ.'OHARA')    FLNAME='OHARAMULTI  '
                          IF(WQ.EQ.'CORNIN')   FLNAME='CORNIN.BIN  '
                          IF(WQ.EQ.'CHANCE')   FLNAME='CHANCE.BIN  '
                          IF(WQ.EQ.'RADHARD')  FLNAME='RADHARD.BIN '
                          IF(FLNAME.NE.'OHARAMULTI  ') THEN
                              OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//FLNAME,
     1                        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                              READ(UNIT=36,REC=1) TOTAL
! 109  FORMAT(A25)
! 119  FORMAT(A33)
! 111  FORMAT('GLASS NAME       GLASS NUMBER')
                              DO J=2,TOTAL+1
                                  READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                                  INCLUDE 'catnamer.inc'
                                  WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                                  CALL SHOWIT(0)
 110                              FORMAT(A13,1X,A13,1X,A13)
                              END DO
                              CALL CLOSE_FILE(36,1)
                          ELSE
                              OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'OHARA.BIN',
     1                        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                              READ(UNIT=36,REC=1) TOTAL
                              DO J=2,TOTAL+1
                                  READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5

                                  INCLUDE 'catnamer.inc'
                                  WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                                  CALL SHOWIT(0)
                              END DO
                              CALL CLOSE_FILE(36,1)
                              OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'OHARA-O.BIN',
     1                        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                              READ(UNIT=36,REC=1) TOTAL
                              DO J=2,TOTAL+1
                                  READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                                  INCLUDE 'catnamer.inc'
                                  WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                                  CALL SHOWIT(0)
                              END DO
                              CALL CLOSE_FILE(36,1)
                          END IF
                          RETURN
                      ELSE
C     WQ WAS MATL OR RUSSIAN
                          IF(WQ.EQ.'MATL') THEN
 3000                         FORMAT('MATL          ACRYLIC      ',
     1                        '-   Acrylic (Lucite)')
                              WRITE(OUTLYNE,3000)
                              CALL SHOWIT(0)
 3010                         FORMAT('MATL          PLYSTY       ',
     1                        '-   Polystyrene')
                              WRITE(OUTLYNE,3010)
                              CALL SHOWIT(0)
 3020                         FORMAT('MATL          POLYCARB     ',
     1                        '-   Polycarbonate')
                              WRITE(OUTLYNE,3020)
                              CALL SHOWIT(0)
 3030                         FORMAT('MATL          SAN          ',
     1                        '-   Copolymer Styrene-Acrylonitrile (SAN)')
                              WRITE(OUTLYNE,3030)
                              CALL SHOWIT(0)

 200                          FORMAT('MATL          GERMSC       ',
     1                        '-   Single Crystal Germanium')
                              WRITE(OUTLYNE,200)
                              CALL SHOWIT(0)
 201                          FORMAT('MATL          GERMPC       ',
     1                        '-   Poly-Crystalline Germanium')
                              WRITE(OUTLYNE,201)
                              CALL SHOWIT(0)
 202                          FORMAT('MATL          SILICON      ',
     1                        '-   Silicon')
                              WRITE(OUTLYNE,202)
                              CALL SHOWIT(0)
 203                          FORMAT('MATL          IRG100       ',
     1                        '-   Schott IRG100 Infrared Glass')
                              WRITE(OUTLYNE,203)
                              CALL SHOWIT(0)
 204                          FORMAT('MATL          ZNSE         ',
     1                        '-   CVD Zinc Selenide (IRTRAN4)')
                              WRITE(OUTLYNE,204)
                              CALL SHOWIT(0)
 205                          FORMAT('MATL          ZNS          ',
     1                        '-   CVD Zinc Sulfide (IRTRAN2)')
                              WRITE(OUTLYNE,205)
                              CALL SHOWIT(0)
1215                          FORMAT('MATL          IRTRAN1      ',
     1                        '-   IRTRAN1 (Magnesium Fluoride (o) ray')
                              WRITE(OUTLYNE,1215)
                              CALL SHOWIT(0)
1205                          FORMAT('MATL          IRTRAN2      ',
     1                        '-   IRTRAN2 (CVD Zinc Sulfide)')
                              WRITE(OUTLYNE,1205)
                              CALL SHOWIT(0)
1217                          FORMAT('MATL          IRTRAN3      ',
     1                        '-   IRTRAN3 (Calcium Fluoride)')
                              WRITE(OUTLYNE,1217)
                              CALL SHOWIT(0)
1204                          FORMAT('MATL          IRTRAN4      ',
     1                        '-   IRTRAN4 (CVD Zinc Selenide)')
                              WRITE(OUTLYNE,1204)
                              CALL SHOWIT(0)
1218                          FORMAT('MATL          IRTRAN5      ',
     1                        '-   IRTRAN5 (Magnesium Oxide)')
                              WRITE(OUTLYNE,1218)
                              CALL SHOWIT(0)
1214                          FORMAT('MATL          IRTRAN6      ',
     1                        '-   IRTRAN6 (Cadmium Telluride)')
                              WRITE(OUTLYNE,1214)
                              CALL SHOWIT(0)
 206                          FORMAT('MATL          CLRTRAN      ',
     1                        '-   CVD CLEARTRAN')
                              WRITE(OUTLYNE,206)
                              CALL SHOWIT(0)
 207                          FORMAT('MATL          SILICA       ',
     1                        '-   Fused Silica (SiO2)')
                              WRITE(OUTLYNE,1207)
                              CALL SHOWIT(0)
 2071                         FORMAT('MATL          SIO2         ',
     1                        '-   Fused Silica (SiO2) (SILICA equiv.)')
                              WRITE(OUTLYNE,1207)
                              CALL SHOWIT(0)
 1207                         FORMAT('MATL          SUPRASIL     ',
     1                        '-   Fused Silica (SiO2)')
                              WRITE(OUTLYNE,2207)
                              CALL SHOWIT(0)
 2207                         FORMAT('MATL          HOMOSIL      ',
     1                        '-   Fused Silica (SiO2)')
                              WRITE(OUTLYNE,207)
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,2071)
                              CALL SHOWIT(0)
 208                          FORMAT('MATL          SAPPHIRE     ',
     1                        '-  Sapphire (Al2O3) (o)rdinary ray')
                              WRITE(OUTLYNE,208)
                              CALL SHOWIT(0)
 209                          FORMAT('MATL          DYNASIL      ',
     1                        '-   Synthetic Fused Silica')
                              WRITE(OUTLYNE,209)
                              CALL SHOWIT(0)
 210                          FORMAT('MATL          AMTIR1       ',
     1                        '-   AMTIR-1')
                              WRITE(OUTLYNE,210)
                              CALL SHOWIT(0)
 211                          FORMAT('MATL          AMTIR3       ',
     1                        '-   AMTIR-3 (TI 1173 Equivalent)')
                              WRITE(OUTLYNE,211)
                              CALL SHOWIT(0)
 212                          FORMAT('MATL          AS2S3        ',
     1                        '-   Arsenic Trisulfide IR Glass')
                              WRITE(OUTLYNE,212)
                              CALL SHOWIT(0)
 213                          FORMAT('MATL          GAAS         ',
     1                        '-   Gallium Arsenide')
                              WRITE(OUTLYNE,213)
                              CALL SHOWIT(0)
 214                          FORMAT('MATL          CDTE         ',
     1                        '-   Cadmium Telluride (IRTRAN6)')
                              WRITE(OUTLYNE,214)
                              CALL SHOWIT(0)
 215                          FORMAT('MATL          MGF2(O)      ',
     1                        '-   Magnesium Fluoride (IRTRAN1) (o) ray')
                              WRITE(OUTLYNE,215)
                              CALL SHOWIT(0)
2215                          FORMAT('MATL          MGF2         ',
     1                        '-   Magnesium Fluoride (IRTRAN1) (o) ray')
                              WRITE(OUTLYNE,2215)
                              CALL SHOWIT(0)
 216                          FORMAT('MATL          MGF2(E)      ',
     1                        '-   Magnesium Fluoride (IRTRAN1) (e) ray')
                              WRITE(OUTLYNE,216)
                              CALL SHOWIT(0)
 217                          FORMAT('MATL          CAF2         ',
     1                        '-   Calcium Fluoride (IRTRAN3)')
                              WRITE(OUTLYNE,217)
                              CALL SHOWIT(0)
 218                          FORMAT('MATL          MGO          ',
     1                        '-   Magnesium Oxide (IRTRAN5)')
                              WRITE(OUTLYNE,218)
                              CALL SHOWIT(0)
 219                          FORMAT('MATL          BAF2         ',
     1                        '-   Barium Fluoride')
                              WRITE(OUTLYNE,219)
                              CALL SHOWIT(0)
 220                          FORMAT('MATL          KBR          ',
     1                        '-   Potassium Bromide')
                              WRITE(OUTLYNE,220)
                              CALL SHOWIT(0)
 221                          FORMAT('MATL          CSI          ',
     1                        '-   Cesium Iodide')
                              WRITE(OUTLYNE,221)
                              CALL SHOWIT(0)
 222                          FORMAT('MATL         CSBR          ',
     1                        '-   Cesium Bromide')
                              WRITE(OUTLYNE,222)
                              CALL SHOWIT(0)
 223                          FORMAT('MATL         KRS5          ',
     1                        '-   Thallium Bromoiodide (KRS-5)')
                              WRITE(OUTLYNE,223)
                              CALL SHOWIT(0)
 224                          FORMAT('MATL         SIO2O         ',
     1                        '-   Crystalline Quartz (SiO2) (o) ray')
                              WRITE(OUTLYNE,224)
                              CALL SHOWIT(0)
 225                          FORMAT('MATL         SIO2E         ',
     1                        '-   Crystalline Quartz (SiO2) (e) ray')
                              WRITE(OUTLYNE,225)
                              CALL SHOWIT(0)
 226                          FORMAT('MATL         NACL          ',
     1                        '-   Sodium Chloride')
                              WRITE(OUTLYNE,226)
                              CALL SHOWIT(0)
 227                          FORMAT('MATL         LIF           ',
     1                        '-   Lithium Fluoride')
                              WRITE(OUTLYNE,227)
                              CALL SHOWIT(0)
 228                          FORMAT('MATL         VIR3          ',
     1                        '-   Corning VIR 3 Infrared Glass')
                              WRITE(OUTLYNE,228)
                              CALL SHOWIT(0)
 229                          FORMAT('MATL         9754          ',
     1                        '-   Corning 9754 Infrared Glass')
                              WRITE(OUTLYNE,229)
                              CALL SHOWIT(0)
 230                          FORMAT('MATL         ALON          ',
     1                        '-   Raytheon Aluminum Oxynitride')
                              WRITE(OUTLYNE,230)
                              CALL SHOWIT(0)
 231                          FORMAT('MATL         SPINEL        ',
     1                        '-   Magnesium Aluminate Spinel (MgAl2O4)')
                              WRITE(OUTLYNE,231)
                              CALL SHOWIT(0)
 232                          FORMAT('MATL         CALAL         ',
     1                        '-   Calcium Aluminate Glass')
                              WRITE(OUTLYNE,232)
                              CALL SHOWIT(0)
 233                          FORMAT('MATL         B270          ',
     1                        '-   Schott B270 Glass')
                              WRITE(OUTLYNE,233)
                              CALL SHOWIT(0)
 234                          FORMAT('MATL         IRG2          ',
     1                        '-   Schott IRG2 Glass')
                              WRITE(OUTLYNE,234)
                              CALL SHOWIT(0)
 235                          FORMAT('MATL         IRG3          ',
     1                        '-   Schott IRG3 Glass')
                              WRITE(OUTLYNE,235)
                              CALL SHOWIT(0)
 236                          FORMAT('MATL         IRGN6         ',
     1                        '-   Schott IRGN6 Glass')
                              WRITE(OUTLYNE,236)
                              CALL SHOWIT(0)
 237                          FORMAT('MATL         IRG7          ',
     1                        '-   Schott IRG7 Glass')
                              WRITE(OUTLYNE,237)
                              CALL SHOWIT(0)
 238                          FORMAT('MATL         IRG9          ',
     1                        '-   Schott IRG9 Glass')
                              WRITE(OUTLYNE,238)
                              CALL SHOWIT(0)
 239                          FORMAT('MATL         IRG11         ',
     1                        '-   Schott IRG11 Glass')
                              WRITE(OUTLYNE,239)
                              CALL SHOWIT(0)
 240                          FORMAT('MATL         IRG15         ',
     1                        '-   Schott IRG15 Glass')
                              WRITE(OUTLYNE,240)
                              CALL SHOWIT(0)
 241                          FORMAT('MATL         H2O           ',
     1                        '-   Distilled Water')
                              WRITE(OUTLYNE,241)
                              CALL SHOWIT(0)
 242                          FORMAT('MATL         VAC           ',
     1                        '-   Vacuum')
                              WRITE(OUTLYNE,242)
                              CALL SHOWIT(0)
 243                          FORMAT('MATL         ZNS-MS        ',
     1                        '-   II-VI Multi-Spectral ZNS')
                              WRITE(OUTLYNE,243)
                              CALL SHOWIT(0)
 244                          FORMAT('MATL         CEF3          ',
     1                        '-   Cerium Flouride for coatings')
                              WRITE(OUTLYNE,244)
                              CALL SHOWIT(0)
 245                          FORMAT('MATL         LA2O3         ',
     1                        '-   Lanthanum Oxide for coatings')
                              WRITE(OUTLYNE,245)
                              CALL SHOWIT(0)
 246                          FORMAT('MATL         THF4          ',
     1                        '-   Thorium Flouride for coatings')
                              WRITE(OUTLYNE,246)
                              CALL SHOWIT(0)
 247                          FORMAT('MATL         ZRO2          ',
     1                        '-   Zirconium Oxide for coatings')
                              WRITE(OUTLYNE,247)
                              CALL SHOWIT(0)
 248                          FORMAT('MATL         DIAMOND       ',
     1                        '-   Herzberger formula')
                              WRITE(OUTLYNE,248)
                              CALL SHOWIT(0)
 249                          FORMAT('MATL         YAG - Yittrium Aluminum Garnet')
                              WRITE(OUTLYNE,249)
                              CALL SHOWIT(0)
                          END IF
                          IF(WQ.EQ.'RUSSIAN') CALL GLASSP_RUSSIAN1
                      END IF
                  ELSE
                  END IF
                  IF(WQ.EQ.'GLCAT'.OR.WQ.EQ.'GLA') THEN
C     GLCAT MEANS DO ALL CATALOGS
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'SCHOTT.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL
                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          INCLUDE 'catnamer.inc'
                          WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                          CALL SHOWIT(0)
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'OHARA.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL
                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          INCLUDE 'catnamer.inc'
                          WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                          CALL SHOWIT(0)
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'OHARA-O.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL
                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          INCLUDE 'catnamer.inc'
                          WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                          CALL SHOWIT(0)
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'SCH2000.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL
                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          INCLUDE 'catnamer.inc'
                          WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                          CALL SHOWIT(0)
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'OHARA.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL
                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          INCLUDE 'catnamer.inc'
                          WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                          CALL SHOWIT(0)
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'OHARA-O.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL
                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          INCLUDE 'catnamer.inc'
                          WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                          CALL SHOWIT(0)
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'HOYA.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL
                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          INCLUDE 'catnamer.inc'
                          WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                          CALL SHOWIT(0)
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'HIKARI.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL
                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          INCLUDE 'catnamer.inc'
                          WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                          CALL SHOWIT(0)
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'CORNIN.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL
                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          INCLUDE 'catnamer.inc'
                          WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                          CALL SHOWIT(0)
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'CHANCE.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL
                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          INCLUDE 'catnamer.inc'
                          WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
                          CALL SHOWIT(0)
                      END DO
                      CALL CLOSE_FILE(36,1)
                      RETURN
                  END IF
              END IF
          ELSE
C     NOT A REQUEST FOR GLASS NAMES IN A CATALOG
          END IF
C
          IF(SQ.EQ.1.AND.SST.EQ.1) THEN
C     OPEN THE APPROPRIATE CATALOG AND DISPLAY DATA FOR THE
C     SPECIFIED GLASS
C       IS THE SURFACE CATALOG IMPLEMENTED ?
              IF(WQ.NE.'SCHOTT'.AND.WQ.NE.'CHANCE'.AND.WQ.NE.'GLA'
     1        .AND.WQ.NE.'OHARA'.AND.WQ.NE.'CORNIN'.AND.WQ.NE.'HIKARI'
     2        .AND.WQ.NE.'HOYA'.AND.WQ.NE.'GLCAT'.AND.WQ.NE.'MATL'
     3        .AND.WQ.NE.'RUSSIAN'.AND.WQ.NE.'RADHARD'.AND.WQ.NE.'SCH2000')
     4         THEN
C     NOT A VALID CATALOG
                  OUTLYNE='2THE REQUESTED GLASS CATALOG WAS NOT RECOGNIZED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     VALID CATALOG, PROCEED LISTING GLASSES
C     DETERMINE FILE NAME
                  IF(WQ.NE.'GLCAT'.AND.WQ.NE.'GLA') THEN
                      IF(WQ.NE.'MATL'.AND.WQ.NE.'RUSSIAN') THEN
                          IF(WQ.EQ.'SCHOTT') FLNAME='SCHOTT.BIN  '
                          IF(WQ.EQ.'SCH2000')FLNAME='SCH2000.BIN '
                          IF(WQ.EQ.'HOYA')   FLNAME='HOYA.BIN    '
                          IF(WQ.EQ.'HIKARI') FLNAME='HIKARI.BIN  '
                          IF(WQ.EQ.'OHARA')  FLNAME='OHARAMULTI  '
                          IF(WQ.EQ.'CORNIN') FLNAME='CORNIN.BIN  '
                          IF(WQ.EQ.'CHANCE') FLNAME='CHANCE.BIN  '
                          IF(WQ.EQ.'RADHARD') FLNAME='RADHARD.BIN '
                          IF(FLNAME.NE.'OHARAMULTI  ') THEN
                              OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//FLNAME,
     1                        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                              READ(UNIT=36,REC=1) TOTAL



 991                          FORMAT('WAVELENGTH RANGE:')
 992                          FORMAT('LOWER WAVELENGTH = ',G15.8,' MICRONS')
 993                          FORMAT('UPPER WAVELENGTH = ',G15.8,' MICRONS')

                              DO J=2,TOTAL+1
                                  READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                                  IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN
C
                                      NAME1=WQ//'     '
                                      NAME2=NAME
                                      CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                      WRITE(OUTLYNE,991)
                                      CALL SHOWIT(0)
                                      WRITE(OUTLYNE,992) LAMLOW
                                      CALL SHOWIT(0)
                                      WRITE(OUTLYNE,993) LAMUPP
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,120) NAME
                                      CALL SHOWIT(0)
 120                                  FORMAT('GLASS NAME   = ',A13)




                                      WRITE(OUTLYNE,121) NUMBER
                                      CALL SHOWIT(0)
 121                                  FORMAT('GLASS NUMBER = ',A13)
 122                                  FORMAT('          A0 = ',G23.15)
 123                                  FORMAT('          A1 = ',G23.15)
 124                                  FORMAT('          A2 = ',G23.15)
 125                                  FORMAT('          A3 = ',G23.15)
 126                                  FORMAT('          A4 = ',G23.15)
 127                                  FORMAT('          A5 = ',G23.15)
 1122                                 FORMAT('          B1 = ',G23.15)
 1123                                 FORMAT('          B2 = ',G23.15)
 1124                                 FORMAT('          B3 = ',G23.15)
 1125                                 FORMAT('          C1 = ',G23.15)
 1126                                 FORMAT('          C2 = ',G23.15)
 1127                                 FORMAT('          C3 = ',G23.15)
 22                                   FORMAT('SCHOTT TYPE INTERPOLATION EQUATION')
 2222                                 FORMAT('SELLMEIER INTERPOLATION EQUATION')
 2223                                 FORMAT('HERZBERGER INTERPOLATION EQUATION')
                                      IF(FLNAME.NE.'SCHOTT.BIN  '.AND.FLNAME.NE.'SCH2000.BIN ') THEN




                                          WRITE(OUTLYNE,22)
                                          CALL SHOWIT(0)




                                          WRITE(OUTLYNE,122) A0
                                          CALL SHOWIT(0)




                                          WRITE(OUTLYNE,123) A1
                                          CALL SHOWIT(0)




                                          WRITE(OUTLYNE,124) A2
                                          CALL SHOWIT(0)




                                          WRITE(OUTLYNE,125) A3
                                          CALL SHOWIT(0)




                                          WRITE(OUTLYNE,126) A4
                                          CALL SHOWIT(0)




                                          WRITE(OUTLYNE,127) A5
                                          CALL SHOWIT(0)
                                      ELSE




                                          WRITE(OUTLYNE,2222)
                                          CALL SHOWIT(0)







                                          WRITE(OUTLYNE,1122) A0
                                          CALL SHOWIT(0)




                                          WRITE(OUTLYNE,1123) A1
                                          CALL SHOWIT(0)




                                          WRITE(OUTLYNE,1124) A2
                                          CALL SHOWIT(0)




                                          WRITE(OUTLYNE,1125) A3
                                          CALL SHOWIT(0)




                                          WRITE(OUTLYNE,1126) A4
                                          CALL SHOWIT(0)




                                          WRITE(OUTLYNE,1127) A5
                                          CALL SHOWIT(0)
                                      END IF
                                      CALL CLOSE_FILE(36,1)
                                      RETURN
                                  ELSE
C     KEEP LOOKING
                                  END IF
                              END DO
                              CALL CLOSE_FILE(36,1)
                          END IF
C     OHARAMULTI
                          IF(FLNAME.EQ.'OHARAMULTI  ') THEN
                              OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'OHARA.BIN',
     1                        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                              READ(UNIT=36,REC=1) TOTAL




                              DO J=2,TOTAL+1
                                  READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                                  IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN
                                      NAME1=WQ//'     '
                                      NAME2=NAME
                                      CALL LAMHILO(NAME1,NAME2,'OHARA.BIN   ',LAMUPP,LAMLOW)
                                      WRITE(OUTLYNE,991)
                                      CALL SHOWIT(0)
                                      WRITE(OUTLYNE,992) LAMLOW
                                      CALL SHOWIT(0)
                                      WRITE(OUTLYNE,993) LAMUPP
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,120) NAME
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,121) NUMBER
                                      CALL SHOWIT(0)







                                      WRITE(OUTLYNE,2222)
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,1122) A0
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,1123) A1
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,1124) A2
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,1125) A3
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,1126) A4
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,1127) A5
                                      CALL SHOWIT(0)
                                      CALL CLOSE_FILE(36,1)
                                      RETURN
                                  ELSE
                                  END IF
                              END DO
                              OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'OHARA-O.BIN',
     1                        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                              READ(UNIT=36,REC=1) TOTAL




                              DO J=2,TOTAL+1
                                  READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                                  IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN
                                      NAME1=WQ//'     '
                                      NAME2=NAME
                                      CALL LAMHILO(NAME1,NAME2,'OHARA-O.BIN ',LAMUPP,LAMLOW)
                                      WRITE(OUTLYNE,991)
                                      CALL SHOWIT(0)
                                      WRITE(OUTLYNE,992) LAMLOW
                                      CALL SHOWIT(0)
                                      WRITE(OUTLYNE,993) LAMUPP
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,120) NAME
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,121) NUMBER
                                      CALL SHOWIT(0)







                                      WRITE(OUTLYNE,22)
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,122) A0
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,123) A1
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,124) A2
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,125) A3
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,126) A4
                                      CALL SHOWIT(0)




                                      WRITE(OUTLYNE,127) A5
                                      CALL SHOWIT(0)
                                      CALL CLOSE_FILE(36,1)
                                      RETURN
                                  END IF
                              END DO
                          END IF
                          CALL CLOSE_FILE(36,1)



                          OUTLYNE='THE REQUESTED GLASS NAME WAS NOT FOUND'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
C     GLASS NOT FOUND
                          CALL CLOSE_FILE(36,1)
                          RETURN
                      ELSE
C     MATL
                          IF(WQ.EQ.'MATL') THEN
C
                              IF(WS.EQ.'ACRYLIC') THEN
 400                              FORMAT('Acrylic (Lucite) - 492:574')




                                  WRITE(OUTLYNE,400)
                                  CALL SHOWIT(0)
 401                              FORMAT(
     1                            'Normal Cubic Spline Fit to `Modern Optical Eng`. data')




                                  WRITE(OUTLYNE,401)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'PLYSTY') THEN
 402                              FORMAT('Polystyrene - 590:309')




                                  WRITE(OUTLYNE,402)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,401)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'POLYCARB') THEN
 403                              FORMAT('Polycarbonate - 585:299')




                                  WRITE(OUTLYNE,403)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,401)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'SAN') THEN
 404                              FORMAT('Copolymer Styrene-Acrylonitrile (SAN) - 567:348')




                                  WRITE(OUTLYNE,404)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,401)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
C
                              IF(WS.EQ.'GERMSC') THEN
 300                              FORMAT('Single Crystal Germanium - at 300 degrees Kelvin')




                                  WRITE(OUTLYNE,300)
                                  CALL SHOWIT(0)
 301                              FORMAT(
     1                            'Normal Cubic Spline Fit to EXOTIC MATERIALS INC. data')




                                  WRITE(OUTLYNE,301)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
C
                              IF(WS.EQ.'GERMPC') THEN
 302                              FORMAT('Poly-Crystalline Germanium - at 300 degrees Kelvin')




                                  WRITE(OUTLYNE,302)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,301)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
C
                              IF(WS.EQ.'SILICON') THEN
 303                              FORMAT('Silicon - at 299 degrees Kelvin')




                                  WRITE(OUTLYNE,303)
                                  CALL SHOWIT(0)
 304                              FORMAT(
     1                            'Normal Cubic Spline Fit to WILLOW RUN LABORATORIES data')




                                  WRITE(OUTLYNE,304)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRG100') THEN
 305                              FORMAT('Schott IRG100 - Infrared Glass')




                                  WRITE(OUTLYNE,305)
                                  CALL SHOWIT(0)
 306                              FORMAT('Normal Cubic Spline Fit to Schott data')




                                  WRITE(OUTLYNE,306)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'ZNSE') THEN
 307                              FORMAT('CVD Zinc Selenide - at 293 degrees Kelvin')




                                  WRITE(OUTLYNE,307)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,308)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRTRAN4') THEN
1307                              FORMAT(
     1                            'IRTRAN4 - equiv. to CVD Zinc Selenide - at 293 degrees Kelvin')




                                  WRITE(OUTLYNE,1307)
                                  CALL SHOWIT(0)
 308                              FORMAT('Normal Cubic Spline Fit to CVD Inc. data')




                                  WRITE(OUTLYNE,308)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'ZNS') THEN
 309                              FORMAT('CVD Zinc Sulfide - at 293 degrees Kelvin')




                                  WRITE(OUTLYNE,309)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,308)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRTRAN2') THEN
1309                              FORMAT(
     1                            'IRTRAN2 - equiv. to CVD Zinc Sulfide - at 293 degrees Kelvin')




                                  WRITE(OUTLYNE,1309)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,308)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'CLRTRAN') THEN
 310                              FORMAT('CVD CLEARTRAN - at 293 degrees Kelvin')




                                  WRITE(OUTLYNE,310)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,308)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'SILICA'.OR.WS.EQ.'SIO2        ') THEN
 311                              FORMAT('Fused Silica - at 293 degrees Kelvin')




                                  WRITE(OUTLYNE,311)
                                  CALL SHOWIT(0)
 321                              FORMAT('Handbook of Optics')




                                  WRITE(OUTLYNE,321)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'SUPRASIL') THEN
 1311                             FORMAT('SUPRASIL - at 293 degrees Kelvin')




                                  WRITE(OUTLYNE,1311)
                                  CALL SHOWIT(0)
 1321                             FORMAT('Normal Cubic Spline Fit to HERAEUS AMERSIL DATA')




                                  WRITE(OUTLYNE,1321)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'HOMOSIL') THEN
 2311                             FORMAT('HOMOSIL - at 293 degrees Kelvin')




                                  WRITE(OUTLYNE,2311)
                                  CALL SHOWIT(0)
 2321                             FORMAT('Normal Cubic Spline Fit to HERAEUS AMERSIL DATA')




                                  WRITE(OUTLYNE,2321)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'SAPPHIRE'.OR.WS.EQ.'SAPHIR') THEN
 312                              FORMAT('Sapphire (ordinary ray) - at 297 degrees Kelvin')




                                  WRITE(OUTLYNE,312)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,340)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'DYNASIL') THEN
 313                              FORMAT('Synthetic Fused Silica - at 293 degrees Kelvin')




                                  WRITE(OUTLYNE,313)
                                  CALL SHOWIT(0)
 314                              FORMAT('Dynasil Corp. Interpolation Equation')




                                  WRITE(OUTLYNE,314)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'AMTIR1') THEN
 315                              FORMAT('AMTIR 1 - at 298 degrees Kelvin')




                                  WRITE(OUTLYNE,315)
                                  CALL SHOWIT(0)
 316                              FORMAT('Normal Cubic Spline Fit to Amorphous Materials Data')




                                  WRITE(OUTLYNE,316)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'AMTIR3') THEN
 317                              FORMAT('AMTIR 3 - at 298 degrees Kelvin')




                                  WRITE(OUTLYNE,317)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,316)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'AS2S3') THEN
 318                              FORMAT('Arsenic Trisulfide - at 298 degrees Kelvin')




                                  WRITE(OUTLYNE,318)
                                  CALL SHOWIT(0)
 340                              FORMAT('I. R. Handbook Interpolation Equation')




                                  WRITE(OUTLYNE,340)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'GAAS') THEN
 319                              FORMAT('Gallium Arsenide - at 298 degrees Kelvin')




                                  WRITE(OUTLYNE,319)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,316)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'CDTE') THEN
 320                              FORMAT('Cadmium Telluride (IRTRAN6) - at 298 degrees Kelvin')




                                  WRITE(OUTLYNE,320)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,316)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRTRAN6') THEN
1320                              FORMAT(
     1                            'IRTRAN6 - equiv. to Cadmium Telluride at 298 degrees Kelvin')




                                  WRITE(OUTLYNE,1320)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,316)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'MGF2(O)') THEN
 322                              FORMAT('Magnesium Fluoride (IRTRAN1) - at 294 degrees Kelvin')




                                  WRITE(OUTLYNE,322)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,323)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,1324)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'MGF2') THEN
2322                              FORMAT('Magnesium Fluoride (IRTRAN1) - at 294 degrees Kelvin')




                                  WRITE(OUTLYNE,2322)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,323)
                                  CALL SHOWIT(0)
 324                              FORMAT('Harshaw/Filtrol Interpolation Equation')
1324                              FORMAT('Handbook of Optics Interpolation Equation')




                                  WRITE(OUTLYNE,1324)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRTRAN1') THEN
1322                              FORMAT(
     1                            'IRTRAN1 - equiv. to (MgF2) (o) ray - at 294 degrees Kelvin')




                                  WRITE(OUTLYNE,1322)
                                  CALL SHOWIT(0)
 323                              FORMAT('Ordinary Ray')




                                  WRITE(OUTLYNE,323)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,1324)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'MGF2(E)') THEN
 325                              FORMAT('Magnesium Fluoride (IRTRAN1) - at 294 degrees Kelvin')




                                  WRITE(OUTLYNE,325)
                                  CALL SHOWIT(0)
 326                              FORMAT('Extraordinary Ray')




                                  WRITE(OUTLYNE,326)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,1324)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'CAF2') THEN
 327                              FORMAT('Calcium Fluoride (IRTRAN3) - at 292 degrees Kelvin')




                                  WRITE(OUTLYNE,327)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,324)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRTRAN3') THEN
1327                              FORMAT('IRTRAN3 - equiv. to CaF2 at 292 degrees Kelvin')




                                  WRITE(OUTLYNE,1327)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,324)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'MGO') THEN
 328                              FORMAT('Magnesium Oxide (IRTRAN5) - at 295.3 degrees Kelvin')




                                  WRITE(OUTLYNE,328)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,321)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRTRAN5') THEN
1328                              FORMAT(
     1                            'IRTRAN5 - equiv. to Magnesium Oxide at 295.3 degrees Kelvin')




                                  WRITE(OUTLYNE,1328)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,321)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'BAF2') THEN
 329                              FORMAT('Barium Fluoride - at 298 degrees Kelvin')




                                  WRITE(OUTLYNE,329)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,324)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'KBR') THEN
 330                              FORMAT('Potassium Bromide - at 295 degrees Kelvin')




                                  WRITE(OUTLYNE,330)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,324)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'CSI') THEN
 331                              FORMAT('Cesium Iodide - at 297 degrees Kelvin')




                                  WRITE(OUTLYNE,331)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,324)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'CSBR') THEN
 332                              FORMAT('Cesium Bromide - at 300 degrees Kelvin')




                                  WRITE(OUTLYNE,332)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,324)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'KRS5') THEN
 333                              FORMAT('Thallium Bromoiodide (KRS-5) - at 298 degrees Kelvin')




                                  WRITE(OUTLYNE,333)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,324)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'SIO2O') THEN
 334                              FORMAT('Crystal Quartz - at 293 degrees Kelvin')




                                  WRITE(OUTLYNE,334)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,323)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,304)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'SIO2E') THEN




                                  WRITE(OUTLYNE,334)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,326)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,304)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'NACL') THEN




 335                              FORMAT('Sodium Chloride - at 293 degrees Kelvin')
                                  WRITE(OUTLYNE,335)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,304)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'LIF') THEN
 336                              FORMAT('Lithium Fluoride - at 296.6 degrees Kelvin')
                                  WRITE(OUTLYNE,336)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,340)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'VIR3') THEN
 337                              FORMAT('Corning VIR 3 - at 293.0 degrees Kelvin')
                                  WRITE(OUTLYNE,337)
                                  CALL SHOWIT(0)




 341                              FORMAT('Normal Cubic Spline Fit to Corning Data')
                                  WRITE(OUTLYNE,341)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'9754') THEN
 338                              FORMAT('Corning 9754 - at 293.0 degrees Kelvin')
                                  WRITE(OUTLYNE,338)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,341)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'ALON') THEN
 339                              FORMAT('Raytheon ALON- at 293.0 degrees Kelvin')
                                  WRITE(OUTLYNE,339)
                                  CALL SHOWIT(0)




 342                              FORMAT('Normal Cubic Spline Fit to Raytheon Data')
                                  WRITE(OUTLYNE,342)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'SPINEL') THEN
 350                              FORMAT('Magnesium Aluminate Spinel - at 293.0 degrees Kelvin')
                                  WRITE(OUTLYNE,350)
                                  CALL SHOWIT(0)




 351                              FORMAT('Normal Cubic Spline Fit to Alpha Data')
                                  WRITE(OUTLYNE,351)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'CALAL') THEN
 352                              FORMAT('Calcium Aluminate Glass - at 293.0 degrees Kelvin')
                                  WRITE(OUTLYNE,352)
                                  CALL SHOWIT(0)




 353                              FORMAT('Normal Cubic Spline Fit to Sassoon Data')
                                  WRITE(OUTLYNE,353)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'B270') THEN
 354                              FORMAT('Schott B270 - Water White Crown Glass')




                                  WRITE(OUTLYNE,354)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,306)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRG2') THEN
 355                              FORMAT('Schott IRG2 - Schott Germinate Glass')




                                  WRITE(OUTLYNE,355)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,306)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRG3') THEN
 356                              FORMAT('Schott IRG3 - Schott Lanthan Dense Flint Glass')




                                  WRITE(OUTLYNE,356)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,306)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRGN6') THEN
 357                              FORMAT('Schott IRGN6 - Schott CaAl Silicate Glass')




                                  WRITE(OUTLYNE,357)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,306)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRG7') THEN
 358                              FORMAT('Schott IRG7 - Schott Lead Silicate Glass')




                                  WRITE(OUTLYNE,358)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,306)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRG9') THEN
 359                              FORMAT('Schott IRG9 - Schott Fluorophosphate Glass')




                                  WRITE(OUTLYNE,359)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,306)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRG11') THEN
 360                              FORMAT('Schott IRG11 - Schott Ca Aluminate Glass')




                                  WRITE(OUTLYNE,360)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,306)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'IRG15') THEN
 361                              FORMAT('Schott IRG15 - Schott Zinc Crown Glass')




                                  WRITE(OUTLYNE,361)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,306)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'H2O') THEN
 362                              FORMAT('Distilled Water - at 293 degrees Kelvin')




                                  WRITE(OUTLYNE,362)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,363)
                                  CALL SHOWIT(0)
 363                              FORMAT('Normal Cubic Spline fit to published data')
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'VAC') THEN
 364                              FORMAT(
     1                            'Vacuum - relative to AIR at 293 degrees K and 760 mm Hg')




                                  WRITE(OUTLYNE,364)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,365)
                                  CALL SHOWIT(0)
 365                              FORMAT('Born & Wolf algorithm')
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'ZNS-MS') THEN
 366                              FORMAT(
     1                            'ZNS-Multi-Spectral Zinc Selenide, II-VI Corp. Data')




                                  WRITE(OUTLYNE,366)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,363)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'CEF3') THEN
 367                              FORMAT(
     1                            'Cerium Flouride for coatings, 0.55 and 2.00 microns')




                                  WRITE(OUTLYNE,367)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,368)
 368                              FORMAT(
     1                            'Linear fit in range, constant values outside range')
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'LA2O3') THEN
 369                              FORMAT(
     1                            'Lanthanum Oxide for coatings, 0.55 and 2.00 microns')




                                  WRITE(OUTLYNE,369)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,368)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'THF4') THEN
 370                              FORMAT(
     1                            'Thorium Flouride for coatings, 0.40 and 0.75 micron')




                                  WRITE(OUTLYNE,370)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,368)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'ZRO2') THEN
 371                              FORMAT(
     1                            'Zirconium Oxide for coatings, 0.55 and 2.00 microns ')




                                  WRITE(OUTLYNE,371)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,368)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'DIAMOND') THEN
 372                              FORMAT(
     1                            'Diamond (Cubic Carbon) - CRC , 0.23 to 20.00 microns ')




                                  WRITE(OUTLYNE,372)
                                  CALL SHOWIT(0)




                                  WRITE(OUTLYNE,2223)
                                  CALL SHOWIT(0)
                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF
                              IF(WS.EQ.'YAG') THEN
 373                              FORMAT(
     1                            'YAG - Handbook of Optics Equation - CRC , 0.4 to 4.0 microns ')




                                  WRITE(OUTLYNE,373)
                                  CALL SHOWIT(0)




                                  NAME1=WQ//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                                  WRITE(OUTLYNE,991)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,992) LAMLOW
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,993) LAMUPP
                                  CALL SHOWIT(0)
                                  RETURN
                              END IF



                              OUTLYNE='THE REQUESTED GLASS NAME WAS NOT FOUND'
                              CALL SHOWIT(1)
                              OUTLYNE='RE-ENTER COMMAND'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          IF(WQ.EQ.'RUSSIAN') CALL GLASSP_RUSSIAN2
C
                      END IF
                  ELSE
C     GLCAT/GLASS SEARCH
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'SCHOTT.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL




                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN
                              NAME1=WQ//'     '
                              NAME2=NAME
                              CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                              WRITE(OUTLYNE,991)
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,992) LAMLOW
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,993) LAMUPP
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,120) NAME
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,121) NUMBER
                              CALL SHOWIT(0)







                              WRITE(OUTLYNE,2222)
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1122) A0
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1123) A1
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1124) A2
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1125) A3
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1126) A4
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1127) A5
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,140) 'SCHOTT           '
                              CALL SHOWIT(0)
 140                          FORMAT('GLASS MANUFACTURED BY : ',A17)
                              CALL CLOSE_FILE(36,1)
                              RETURN
                          ELSE
C     KEEP LOOKING
                          END IF
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'SCH2000.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL




                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN
                              NAME1=WQ//'     '
                              NAME2=NAME
                              CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                              WRITE(OUTLYNE,991)
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,992) LAMLOW
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,993) LAMUPP
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,120) NAME
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,121) NUMBER
                              CALL SHOWIT(0)







                              WRITE(OUTLYNE,2222)
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1122) A0
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1123) A1
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1124) A2
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1125) A3
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1126) A4
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1127) A5
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,140) 'SCHOTT           '
                              CALL SHOWIT(0)
                              CALL CLOSE_FILE(36,1)
                              RETURN
                          ELSE
C     KEEP LOOKING
                          END IF
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'HOYA.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL




                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN
                              NAME1=WQ//'     '
                              NAME2=NAME
                              CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                              WRITE(OUTLYNE,991)
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,992) LAMLOW
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,993) LAMUPP
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,120) NAME
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,121) NUMBER
                              CALL SHOWIT(0)







                              WRITE(OUTLYNE,22)
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,122) A0
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,123) A1
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,124) A2
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,125) A3
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,126) A4
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,127) A5
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,140) 'HOYA             '
                              CALL SHOWIT(0)
                              CALL CLOSE_FILE(36,1)
                              RETURN
                          ELSE
C     KEEP LOOKING
                          END IF
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'HIKARI.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL




                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN
                              NAME1=WQ//'     '
                              NAME2=NAME
                              CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                              WRITE(OUTLYNE,991)
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,992) LAMLOW
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,993) LAMUPP
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,120) NAME
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,121) NUMBER
                              CALL SHOWIT(0)







                              WRITE(OUTLYNE,22)
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,122) A0
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,123) A1
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,124) A2
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,125) A3
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,126) A4
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,127) A5
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,140) 'HIKARI           '
                              CALL SHOWIT(0)
                              CALL CLOSE_FILE(36,1)
                              RETURN
                          ELSE
C     KEEP LOOKING
                          END IF
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'OHARA.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL




                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN
                              NAME1=WQ//'     '
                              NAME2=NAME
                              CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                              WRITE(OUTLYNE,991)
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,992) LAMLOW
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,993) LAMUPP
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,120) NAME
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,121) NUMBER
                              CALL SHOWIT(0)







                              WRITE(OUTLYNE,2222)
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1122) A0
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1123) A1
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1124) A2
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1125) A3
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1126) A4
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,1127) A5
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,140) 'OHARA            '
                              CALL SHOWIT(0)
                              CALL CLOSE_FILE(36,1)
                              RETURN
                          ELSE
C     KEEP LOOKING
                          END IF
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'OHARA-O.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL




                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN
                              NAME1=WQ//'     '
                              NAME2=NAME
                              CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                              WRITE(OUTLYNE,991)
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,992) LAMLOW
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,993) LAMUPP
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,120) NAME
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,121) NUMBER
                              CALL SHOWIT(0)







                              WRITE(OUTLYNE,22)
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,122) A0
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,123) A1
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,124) A2
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,125) A3
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,126) A4
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,127) A5
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,140) 'OHARA            '
                              CALL SHOWIT(0)
                              CALL CLOSE_FILE(36,1)
                              RETURN
                          ELSE
C     KEEP LOOKING
                          END IF
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'CHANCE.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL




                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN
                              NAME1=WQ//'     '
                              NAME2=NAME
                              CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                              WRITE(OUTLYNE,991)
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,992) LAMLOW
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,993) LAMUPP
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,120) NAME
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,121) NUMBER
                              CALL SHOWIT(0)







                              WRITE(OUTLYNE,22)
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,122) A0
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,123) A1
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,124) A2
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,125) A3
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,126) A4
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,127) A5
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,140) 'CHANCE-PILKINGTON'
                              CALL SHOWIT(0)
                              CALL CLOSE_FILE(36,1)
                              RETURN
                          ELSE
C     KEEP LOOKING
                          END IF
                      END DO
                      CALL CLOSE_FILE(36,1)
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=LIBGLA//'CORNIN.BIN',
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL




                      DO J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN
                              NAME1=WQ//'     '
                              NAME2=NAME
                              CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
                              WRITE(OUTLYNE,991)
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,992) LAMLOW
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,993) LAMUPP
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,120) NAME
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,121) NUMBER
                              CALL SHOWIT(0)







                              WRITE(OUTLYNE,22)
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,122) A0
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,123) A1
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,124) A2
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,125) A3
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,126) A4
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,127) A5
                              CALL SHOWIT(0)




                              WRITE(OUTLYNE,140) 'CORNING-FRANCE   '
                              CALL SHOWIT(0)
                              CALL CLOSE_FILE(36,1)
                              RETURN
                          ELSE
C     KEEP LOOKING
                          END IF
                      END DO



                      OUTLYNE='THE REQUESTED GLASS NAME WAS NOT FOUND'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
C     GLASS NOT FOUND

                      CALL CLOSE_FILE(36,1)
                      RETURN
                  END IF
              END IF
          END IF
          RETURN
      END
