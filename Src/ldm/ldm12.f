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

C       TWELFTH FILE FOR LENS DATABASE MANAGER FILES

C SUB ILF.FOR
      SUBROUTINE ILF
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE INITIALIZES OR BLANKS OUT THE CURRENT
C       LENS LIBRARY DIRECTORY.
C
          INTEGER I,II,N,J
C
          CHARACTER BLANK*80,FN*10,AN*3
C
          LOGICAL EXISJK
C
          INCLUDE 'datmai.inc'
C
          BLANK=AA//AA//AA//AA
C
C       OPEN UNIT 22 FOR I/O
C       OPEN UNIT 27 FOR I/O
C
          OPEN(UNIT=22,ACCESS='DIRECT',FILE=LIBLEN//'LIB.DAT',FORM=
     1    'UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
          CALL CLOSE_FILE(22,0)
          OPEN(UNIT=27,ACCESS='DIRECT',FILE=LIBLEN//'LIBTAG.DAT',FORM=
     1    'UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
          CALL CLOSE_FILE(27,0)
          DO N=1,999
              CALL CCOONN(N,AN)
              IF(N.GT.0.AND.N.LE.9) FN='LIB00'//AN(3:3)//'.DAT'
              IF(N.GT.9.AND.N.LE.99) FN='LIB0'//AN(2:3)//'.DAT'
              IF(N.GT.99.AND.N.LE.999) FN='LIB'//AN(1:3)//'.DAT'
              EXISJK=.FALSE.
              INQUIRE(FILE=LIBLEN//FN,EXIST=EXISJK)
              IF(EXISJK) THEN
                  OPEN(UNIT=22,ACCESS='SEQUENTIAL',BLANK='NULL'
     1            ,FORM='FORMATTED',FILE=LIBLEN//FN
     2            ,STATUS='UNKNOWN')
                  CALL CLOSE_FILE(22,0)
              END IF
          END DO
C
          OPEN(UNIT=22,ACCESS='DIRECT',FILE=LIBLEN//'LIB.DAT',FORM=
     1    'UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
          OPEN(UNIT=27,ACCESS='DIRECT',FILE=LIBLEN//'LIBTAG.DAT',FORM=
     1    'UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
C
          II=0
          DO 25 I=1,999
              WRITE(UNIT=22,REC=I)II,BLANK
              DO J=1,10
                  WRITE(UNIT=27,REC=I-1+J) BLANK(1:75)
              END DO
 25       CONTINUE
C
          CALL CLOSE_FILE(22,1)
          CALL CLOSE_FILE(27,1)
          OUTLYNE='LENS LIBRARY INITIALIZED'
          CALL SHOWIT(1)
          RETURN
      END
C SUB CVSOLV.FOR
      SUBROUTINE CVSOLV
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE CVSOLV WHICH IMPLEMENTS THE
C       CURVATURE SOLVES
C       COMMAND AT THE LENS INPUT AND UPDATE LENS LEVEL.
C
          INTEGER SF
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(WC.EQ.'APY'.OR.WC.EQ.'APX'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PIX'
     1    .OR.WC.EQ.'PUY'.OR.WC.EQ.'PUX'.OR.WC.EQ.'APCY'.OR.WC.EQ.'APCX'
     2    .OR.WC.EQ.'PICY'.OR.WC.EQ.'PICX'.OR.WC.EQ.'PUCY'.OR.WC.EQ.
     3    'PUCX'.OR.WC.EQ.'COCY'.OR.WC.EQ.'COCX') THEN
              IF(STI.EQ.1) THEN
C
                  IF(WC.EQ.'APY') THEN
                      IF(SOLVE(8,SURF).EQ.1.0D0)
     1                WRITE(OUTLYNE,101)SOLVE(9,SURF),SURF
 101                  FORMAT('"APY" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'APX') THEN
                      IF(SOLVE(2,SURF).EQ.8.0D0)
     1                WRITE(OUTLYNE,102)SOLVE(1,SURF),SURF
 102                  FORMAT('"APX" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'PIY') THEN
                      IF(SOLVE(8,SURF).EQ.2.0D0)
     1                WRITE(OUTLYNE,103)SOLVE(9,SURF),SURF
 103                  FORMAT('"PIY" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'PIX') THEN
                      IF(SOLVE(2,SURF).EQ.9.0D0)
     1                WRITE(OUTLYNE,104)SOLVE(1,SURF),SURF
 104                  FORMAT('"PIX" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'PUY') THEN
                      IF(SOLVE(8,SURF).EQ.3.0D0)
     1                WRITE(OUTLYNE,105)SOLVE(9,SURF),SURF
 105                  FORMAT('"PUY" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'PUX') THEN
                      IF(SOLVE(2,SURF).EQ.10.0D0)
     1                WRITE(OUTLYNE,106)SOLVE(1,SURF),SURF
 106                  FORMAT('"PUX" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'APCY') THEN
                      IF(SOLVE(8,SURF).EQ.4.0D0)
     1                WRITE(OUTLYNE,107)SOLVE(9,SURF),SURF
 107                  FORMAT('"APCY" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'APCX') THEN
                      IF(SOLVE(2,SURF).EQ.11.0D0)
     1                WRITE(OUTLYNE,108)SOLVE(1,SURF),SURF
 108                  FORMAT('"APCX" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'PICY') THEN
                      IF(SOLVE(8,SURF).EQ.5.0D0)
     1                WRITE(OUTLYNE,109)SOLVE(9,SURF),SURF
 109                  FORMAT('"PICY" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'PICX') THEN
                      IF(SOLVE(2,SURF).EQ.12.0D0)
     1                WRITE(OUTLYNE,110)SOLVE(1,SURF),SURF
 110                  FORMAT('"PICX" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'PUCY') THEN
                      IF(SOLVE(8,SURF).EQ.6.0D0)
     1                WRITE(OUTLYNE,111)SOLVE(9,SURF),SURF
 111                  FORMAT('"PUCY" = ',G23.15,' AT SURFACE #',I3)
                  ELSE
                  END IF
C
                  IF(WC.EQ.'PUCX') THEN
                      IF(SOLVE(2,SURF).EQ.13.0D0)
     1                WRITE(OUTLYNE,112)SOLVE(1,SURF),SURF
 112                  FORMAT('"PUCX" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'COCY') THEN
                      IF(SOLVE(8,SURF).EQ.7.0D0)
     1                WRITE(OUTLYNE,113)SOLVE(9,SURF),SURF
 113                  FORMAT('"COCY" = ',G23.15,' AT SURFACE #',I3)
                  END IF
C
                  IF(WC.EQ.'COCX') THEN
                      IF(SOLVE(2,SURF).EQ.14.0D0)
     1                WRITE(OUTLYNE,114)SOLVE(1,SURF),SURF
 114                  FORMAT('"COCX" = ',G23.15,' AT SURFACE #',I3)
                  END IF
                  CALL SHOWIT(0)
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'CSD'.OR.WC.EQ.'CSDX'.OR.WC.EQ.'CSDY') THEN
              IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"CSD", "CSDX" AND "CSDY" TAKE NO STRING'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'OR NUMERIC WORD #3 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       PROCEED
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'ALL     ') THEN
                  OUTLYNE=
     1            '"CSD", "CSDX" AND "CSDY" ONLY ACCEPT "ALL" AS'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'VALID QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       PROCEED
              END IF
              IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
                  W1=0.0
                  W2=SYSTEM1(20)
                  S1=1
                  S2=1
                  DF1=0
                  DF2=0
                  SN=1
              END IF

              IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
                  W1=DBLE(SURF)
                  W2=DBLE(SURF)
                  S1=1
                  S2=1
                  SN=1
                  DF1=0
                  DF2=0
              END IF
              IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
                  OUTLYNE='"CSD", "CSDX" AND "CSDY"'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'USE EITHER TWO OR ZERO NUMERIC WORDS OR QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.0) THEN
                  OUTLYNE=
     1            'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W2).GT.INT(SYSTEM1(20))) THEN
                  WRITE(OUTLYNE,*)
     1            'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',
     1            INT(SYSTEM1(20))
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.GT.W2) THEN
                  WRITE(OUTLYNE,*)
     1            'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'THE STARTING SURFACE #'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DO SF=INT(W1),INT(W2)
                  IF(SF.EQ.0) THEN
                      OUTLYNE='OBJECT SURFACE NEVER HAS SOLVES'
                      CALL SHOWIT(1)
                      GO TO 900
                  END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
                  IF(WC.EQ.'CSD') THEN
                      IF(SOLVE(8,SF).NE.0.0D0.OR.SOLVE(2,SF).NE.0.0D0
     1                .OR.SOLVE(9,SF).NE.0.0D0.OR.SOLVE(1,SF).NE.0.0D0) THEN
                          SOLVE(8,SF)=0.0D0
                          SOLVE(9,SF)=0.0D0
                          SOLVE(2,SF)=0.0D0
                          SOLVE(1,SF)=0.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :ALL CURVATURE SOLVES DELETED'
                          CALL SHOWIT(1)
                      ELSE
                          WRITE(OUTLYNE,*)'SURFACE',SF,' :NO CURVATURE SOLVE TO DELETE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(WC.EQ.'CSDY') THEN
                      IF(SOLVE(8,SF).NE.0.0D0.OR.SOLVE(9,SF).NE.0.0D0) THEN
                          SOLVE(8,SF)=0.0D0
                          SOLVE(9,SF)=0.0D0
                          WRITE(OUTLYNE,*)
     1                    'SURFACE',SF,' :YZ PLANE CURVATURE SOLVE DELETED'
                          CALL SHOWIT(1)
                      ELSE
                          WRITE(OUTLYNE,*)
     1                    'SURFACE',SF,' :NO YZ CURVATURE SOLVE TO DELETE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(WC.EQ.'CSDX') THEN
                      IF(SOLVE(2,SF).NE.0.0D0.OR.SOLVE(1,SF).NE.0.0D0) THEN
                          SOLVE(2,SF)=0.0D0
                          SOLVE(1,SF)=0.0D0
                          WRITE(OUTLYNE,*)
     1                    'SURFACE',SF,' :XZ PLANE CURVATURE SOLVE DELETED'
                          CALL SHOWIT(1)
                      ELSE
                          WRITE(OUTLYNE,*)
     1                    'SURFACE',SF,' :NO XZ CURVATURE SOLVE TO DELETE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
C
C
C       RE CALCULATE ALENS(33,SF)
C
                  ALENS(33,SF)=0.0D0
                  IF(SOLVE(6,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+1.0D0
                  IF(SOLVE(4,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+0.1D0
                  IF(SOLVE(8,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+2.0D0
                  IF(SOLVE(2,SF).GT.0.0D0) ALENS(33,SF)=ALENS(33,SF)+0.2D0
 900              CONTINUE
              END DO
              RETURN
          ELSE
C       NOT CSD,CSDX OR CSDY
          END IF
          IF(WC.NE.'CSD'.AND.WC.NE.'CSDX'
     1    .AND.WC.NE.'CSDY') THEN
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
C
C
                  IF(WC.EQ.'APY') THEN
                      OUTLYNE='APY" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'APX') THEN
                      OUTLYNE='"APX" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  IF(WC.EQ.'PIY') THEN
                      OUTLYNE='"PIY" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PIX') THEN
                      OUTLYNE='"PIX" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  IF(WC.EQ.'PUY') THEN
                      OUTLYNE=
     1                '"PUY" ONLY ACCEPTS QUALIFIER AND NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PUX') THEN
                      OUTLYNE=
     1                '"PUX" ONLY ACCEPTS QUALIFIER AND NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'FN') THEN
                  IF(WC.EQ.'PUY') THEN
                      OUTLYNE=
     1                'INVALID UALIFIER USED WITH "PUY"'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PUX') THEN
                      OUTLYNE=
     1                'INVALID QUALIFIER USED WITH "PUX"'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WQ.EQ.'FN'.AND.DF1.EQ.1) THEN
                  IF(WC.EQ.'PUY') THEN
                      OUTLYNE=
     1                '"PUY FN" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PUX') THEN
                      OUTLYNE=
     1                '"PUX FN" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WQ.EQ.'FN'.AND.W1.EQ.0.0D0) THEN
                  IF(WC.EQ.'PUY') THEN
                      OUTLYNE=
     1                '"PUY FN" REQUIRES NON-ZERO NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PUX') THEN
                      OUTLYNE=
     1                '"PUX FN" REQUIRES NON-ZERO NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                  IF(WC.EQ.'APCY') THEN
                      OUTLYNE='"APCY" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'APCX') THEN
                      OUTLYNE='"APCX" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  IF(WC.EQ.'PICY') THEN
                      OUTLYNE='"PICY" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PICX') THEN
                      OUTLYNE='"PICX" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PUCY') THEN
                      OUTLYNE='"PUCY" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PUCX') THEN
                      OUTLYNE='"PUCX" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'COCY') THEN
                      OUTLYNE='"COCY" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'COCX') THEN
                      OUTLYNE='"COCX" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C
C       NO CURVATURE SOLVES ARE ALLOWED ON THE OBJECT SURFACE
C       OR ON SURFACE 1
C
          IF(WC.EQ.'PUCY'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'PICY'.OR.
     1    WC.EQ.'PICX'.OR.WC.EQ.'APCX'.OR.WC.EQ.'APCY') THEN
              IF(SURF.LT.1.AND.SYSTEM1(26).EQ.-99.0D0.OR.SURF.LT.
     1        INT(SYSTEM1(26)).AND.SYSTEM1(26).NE.-99.0D0) THEN
                  OUTLYNE='             CHIEF RAY SOLVES ARE NOT ALLOWED'
                  CALL SHOWIT(1)
                  OUTLYNE='                       OBJECT SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='                            OR'
                  CALL SHOWIT(1)
                  OUTLYNE='               BEFORE THE APERTURE STOP SURFACE.'
                  CALL SHOWIT(1)
                  OUTLYNE='                       RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       HANDEL PIKUPS RD,CV,RDTOR,CVTOR I.E. REMOVE AS APPROPRIATE
C       AND PRO AND NPRO
C
C       IF ANY CURVATURE SOLVE OCCURED, THEN PRO AND NPRO
C       PIKUPS GO
C
C       DUMP PIKUP PRO AND NPRO IF FOUND
          IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
              PIKUP(1:6,SURF,11)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
              CALL SHOWIT(1)
          END IF
          IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
              PIKUP(1:6,SURF,12)=0.0D0
              ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
              WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
              CALL SHOWIT(1)
          END IF
C
C       IF SURFACE IS NOT TORIC THEN NO RDTOR OR CVTOR TO CONSIDER
          IF(ALENS(23,SURF).EQ.0.0D0) THEN
C       NON-TORIC
              IF(WC.EQ.'APY'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PUY'.OR.
     1        WC.EQ.'APCY'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY'.OR.
     2        WC.EQ.'COCY') THEN
                  IF(PIKUP(1,SURF,1).EQ.1.0D0) THEN
                      PIKUP(1:6,SURF,1)=0.0D0
                      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                      WRITE(OUTLYNE,*)'SURFACE',SURF,' :RADIUS PIKUP DELETED'
                      CALL SHOWIT(1)
                      IF(PIKUP(1,SURF,2).EQ.1.0D0) THEN
                          PIKUP(1:6,SURF,2)=0.0D0
                          ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :CURVATURE PIKUP DELETED'
                          CALL SHOWIT(1)
                      END IF
                  END IF
              END IF
              IF(ALENS(23,SURF).EQ.1.0) THEN
C       SURFACE IS A YTORIC
                  IF(WC.EQ.'APY'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PUY'.OR.
     1            WC.EQ.'APCY'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY'.OR.
     2            WC.EQ.'COCY') THEN
                      IF(PIKUP(1,SURF,1).EQ.1.0D0) THEN
                          PIKUP(1:6,SURF,1)=0.0D0
                          ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :RADIUS PIKUP DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(PIKUP(1,SURF,2).EQ.1.0D0) THEN
                          PIKUP(1:6,SURF,2)=0.0D0
                          ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :CURVATURE PIKUP DELETED'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(WC.EQ.'APX'.OR.WC.EQ.'PIX'.OR.WC.EQ.'PUX'.OR.
     1            WC.EQ.'APCX'.OR.WC.EQ.'PICX'.OR.WC.EQ.'PUCX'.OR.
     2            WC.EQ.'COCX') THEN
                      IF(PIKUP(1,SURF,9).EQ.1.0D0) THEN
                          PIKUP(1:6,SURF,9)=0.0D0
                          ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :(CVTOR) PIKUP DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(PIKUP(1,SURF,10).EQ.1.0D0) THEN
                          PIKUP(1:6,SURF,10)=0.0D0
                          ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :(RDTOR) PIKUP DELETED'
                          CALL SHOWIT(1)
                      END IF
                  END IF
              END IF
              IF(ALENS(23,SURF).EQ.2.0D0) THEN
C       SURFACE IS A XTORIC
                  IF(WC.EQ.'APY'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PUY'.OR.
     1            WC.EQ.'APCY'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY'.OR.
     2            WC.EQ.'COCY') THEN
                      IF(PIKUP(1,SURF,9).EQ.1.0D0) THEN
                          PIKUP(1:6,SURF,9)=0.0D0
                          ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :(CVTOR) PIKUP DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(PIKUP(1,SURF,10).EQ.1.0D0) THEN
                          PIKUP(1:6,SURF,10)=0.0D0
                          ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :(RDTOR) PIKUP DELETED'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(WC.EQ.'APX'.OR.WC.EQ.'PIX'.OR.WC.EQ.'PUX'.OR.
     1            WC.EQ.'APCX'.OR.WC.EQ.'PICX'.OR.WC.EQ.'PUCX'.OR.
     2            WC.EQ.'COCX') THEN
                      IF(PIKUP(1,SURF,1).EQ.1.0D0) THEN
                          PIKUP(1:6,SURF,1)=0.0D0
                          ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :RADIUS PIKUP DELETED'
                          CALL SHOWIT(1)
                      END IF
                      IF(PIKUP(1,SURF,2).EQ.1.0D0) THEN
                          PIKUP(1:6,SURF,2)=0.0D0
                          ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
                          WRITE(OUTLYNE,*)'SURFACE',SURF,' :CURVATURE PIKUP DELETED'
                          CALL SHOWIT(1)
                      END IF
                  END IF
              END IF
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              IF(WC.EQ.'APY')  SOLVE(8,SURF)=1.0D0
              IF(WC.EQ.'PIY')  SOLVE(8,SURF)=2.0D0
              IF(WC.EQ.'PUY')  SOLVE(8,SURF)=3.0D0
              IF(WC.EQ.'APCY') SOLVE(8,SURF)=4.0D0
              IF(WC.EQ.'PICY') SOLVE(8,SURF)=5.0D0
              IF(WC.EQ.'PUCY') SOLVE(8,SURF)=6.0D0
              IF(WC.EQ.'PUY'.AND.SQ.EQ.0) SOLVE(9,SURF)=W1
              IF(WC.EQ.'PUY'.AND.WQ.EQ.'FN')
     1        SOLVE(9,SURF)=-1.0D0/(2.0D0*W1)
              IF(WC.EQ.'PIY'
     1        .OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY')
     2                SOLVE(9,SURF)=W1
              IF(WC.EQ.'APY'.OR.WC.EQ.'APCY')
     2                SOLVE(9,SURF)=0.0D0
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(WC.EQ.'COCY'.AND.W1.GT.SYSTEM1(20)) THEN
                  OUTLYNE='(COCY) SOLVE REFERED TO A SURFACE BEYOND'
                  CALL SHOWIT(1)
                  OUTLYNE='       THE IMAGE SURFACE. SOLVE IGNORED'
                  CALL SHOWIT(1)
              ELSE
                  IF(WC.EQ.'COCY'.AND.W1.EQ.DBLE(SURF)) THEN
                      OUTLYNE='(COCY) SOLVE CAN NOT REFER TO ITSELF'
                      CALL SHOWIT(1)
                      OUTLYNE='SOLVE IGNORED'
                      CALL SHOWIT(1)
                  ELSE
                      IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                      IF(WC.EQ.'COCY'.AND.W1.LE.SYSTEM1(20)) THEN
                          SOLVE(8,SURF)=7.0D0
                          SOLVE(9,SURF)=W1
                      ELSE
                      END IF
                  END IF
              END IF
C
C
              IF(WC.EQ.'APX'.AND.ALENS(23,SURF).EQ.0.0D0
     1        .OR.WC.EQ.'PIX'.AND.ALENS(23,SURF).EQ.0.0D0
     2        .OR.WC.EQ.'PUX'.AND.ALENS(23,SURF).EQ.0.0D0
     3        .OR.WC.EQ.'APCX'.AND.ALENS(23,SURF).EQ.0.0D0
     4        .OR.WC.EQ.'PICX'.AND.ALENS(23,SURF).EQ.0.0D0
     5        .OR.WC.EQ.'PUCX'.AND.ALENS(23,SURF).EQ.0.0D0
     2        .OR.WC.EQ.'COCX'.AND.ALENS(23,SURF).EQ.0.0D0) THEN
                  OUTLYNE='XZ CURVATURE SOLVES VALID ONLY FOR TORIC SURFACES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'XZ CURVATURE SOLVE NOT ASSIGNED FOR SURFACE',SURF
                  CALL SHOWIT(1)
                  SOLVE(2,SURF)=0.0D0
                  SOLVE(1,SURF)=0.0D0
              ELSE
                  IF(ALENS(23,SURF).GT.0.0D0) THEN
C       SURFACE IS A TORIC.
                      IF(WC.EQ.'APX')THEN
                          SOLVE(2,SURF)=8.0D0
                          SOLVE(1,SURF)=0.0D0
                      ELSE
                      END IF
                      IF(WC.EQ.'PIX') THEN
                          SOLVE(2,SURF)=9.0D0
                          SOLVE(1,SURF)=W1
                      ELSE
                      END IF
                      IF(WC.EQ.'PUX'.AND.SQ.EQ.0) THEN
                          SOLVE(2,SURF)=10.0D0
                          SOLVE(1,SURF)=W1
                      ELSE
                      END IF
                      IF(WC.EQ.'PUX'.AND.WQ.EQ.'FN') THEN
                          SOLVE(2,SURF)=10.0D0
                          SOLVE(1,SURF)=-1.0D0/(2.0D0*W1)
                      ELSE
                      END IF
                      IF(WC.EQ.'APCX') THEN
                          SOLVE(2,SURF)=11.0D0
                          SOLVE(1,SURF)=0.0D0
                      ELSE
                      END IF
                      IF(WC.EQ.'PICX') THEN
                          SOLVE(2,SURF)=12.0D0
                          SOLVE(1,SURF)=W1
                      ELSE
                      END IF
                      IF(WC.EQ.'PUCX') THEN
                          SOLVE(2,SURF)=13.0D0
                          SOLVE(1,SURF)=W1
                      ELSE
                      END IF
                      IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                      IF(WC.EQ.'COCX'.AND.W1.GT.SYSTEM1(20)) THEN
                          OUTLYNE='(COCX) SOLVE REFERED TO A SURFACE BEYOND'
                          CALL SHOWIT(1)
                          OUTLYNE='       THE IMAGE SURFACE. SOLVE IGNORED'
                          CALL SHOWIT(1)
                      ELSE
                          IF(WC.EQ.'COCX'.AND.W1.EQ.DBLE(SURF)) THEN
                              OUTLYNE='(COCX) SOLVE CAN NOT REFER TO ITSELF'
                              CALL SHOWIT(1)
                              OUTLYNE='SOLVE IGNORED'
                              CALL SHOWIT(1)
                          ELSE
                              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                              IF(WC.EQ.'COCX'.AND.W1.LE.SYSTEM1(20)) THEN
                                  SOLVE(8,SURF)=14.0D0
                                  SOLVE(9,SURF)=W1
                              ELSE
                              END IF
                          END IF
                      END IF
C       XZ CURVATURE SOLVE WAS ASSIGNED TO A TORIC
                  ELSE
                  END IF
              END IF
C
C       NOW UPDATE THE STATUS OF ALENS(33,SURF) TO PROPERLY
C       REPRESENT THE SOLVE STATUS ON SURFACE (SURF)
C
              ALENS(33,SURF)=0.0D0
              IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
              IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
              IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
              IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
              RETURN
          ELSE
C       MUST BE A DELETION
          END IF
          RETURN
      END
C SUB COERRS.FOR

      SUBROUTINE COERRS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE COERRS.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE OBSCURATION ERASE CHECKING AS CALLED BY CACHEK.FOR
C
          EXTERNAL INSID2
C
          INTEGER CAERAS,COERAS
C
          INTEGER II,I,N,III
C
          REAL*8 X,Y,Z,ANGLE,
     1    XR,YR,LS1,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,
     2    X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,
     3    YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4
C
          COMMON/CACO/CAERAS,COERAS,LS
C
          LOGICAL INS,INSID2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       NOW ALL THE COERAS WAS ALREADY SET AND THE RAY WAS
C       BLOCKED BY A COBS ON SURFACE I. DOES THE BLOCK GET CANCELED
C       BY THE COBS ERASE ON I+I = II
C
          I=R_I
          X=R_X
          Y=R_Y
          Z=R_Z
          II=I
C
C       COERAS=1 CIRCULAR COBS ERASE, DOES IT STOP THE RAY
          IF(COERAS.EQ.1) THEN
              LS1=0.0D0
C
C       CIRCULAR COBS EQUATION IS:
C
C       (X)**2 + (Y)**2 = R**2
C
C       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
C       LESS THAN THE RIGHT SIDE, THE RAY IS BLOCKED
C       ELSE IT IS NOT BLOCKED.
C
C       IF NON-ZERO COBS ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE DECENTERED
C       COBS ERASE. REMEMBER.
C
              XR=X-ALENS(65,II)
              YR=Y-ALENS(64,II)
C
              LS1=DSQRT((XR**2)+(YR**2))
C
              RS=DSQRT(ALENS(62,II)**2)+AIMTOL
              IF(REAL(LS1).GT.REAL(RS)) THEN
                  LS1=10.0D0
              ELSE
                  LS1=0.0D0
              END IF
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
              RETURN
C       COERAS NOT 1
          END IF
C
C
C       COERAS=2 RECTANGULAR COBS ERASE, DOES IT STOP THE RAY
          IF(COERAS.EQ.2) THEN
              LS1=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
C       COORDINATES ARE:
              X1=-ALENS(63,II)-AIMTOL
              Y1=ALENS(62,II)+AIMTOL
              X2=-ALENS(63,II)-AIMTOL
              Y2=-ALENS(62,II)-AIMTOL
              X3=ALENS(63,II)+AIMTOL
              Y3=-ALENS(62,II)-AIMTOL
              X4=ALENS(63,II)+AIMTOL
              Y4=ALENS(62,II)+AIMTOL
C
              XRD=X
              YRD=Y
              XRD=XRD-ALENS(65,II)
              YRD=YRD-ALENS(64,II)
              XR=(XRD*DCOS(ALENS(67,II)))+(YRD*DSIN(ALENS(67,II)))
              YR=(YRD*DCOS(ALENS(67,II)))-(XRD*DSIN(ALENS(67,II)))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR INSIDE
              XT(1)=X1
              YT(1)=Y1
              XT(2)=X2
              YT(2)=Y2
              XT(3)=X3
              YT(3)=Y3
              XT(4)=X4
              YT(4)=Y4
              NP=4
              X0=XR
              Y0=YR
              INS=INSID2()
              IF(INS) THEN
C       RAY NOT BLOCKED
                  LS1=0.0D0
              ELSE
C       RAY BLOCKED
                  LS1=10.0D0
              END IF
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
              RETURN
C       COERAS NOT 2
          END IF
C
C       COERAS=3 ELLIPTICAL COBS, DOES IT STOP THE RAY
          IF(COERAS.EQ.3) THEN
              LS1=0.0D0
C
C       ELLIPTICAL COBS EQUATION IS:
C
C       (X)**2/A**2  + (Y)**2/B**2 = 1
C
C       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
C       LESS THAN 1.0D0, THE RAY IS BLOCKED
C       ELSE IT IS NOT BLOCKED.
              XRD=X
              YRD=Y
              XRD=XRD-ALENS(65,II)
              YRD=YRD-ALENS(64,II)
              XR=(XRD*DCOS(ALENS(67,II)))+(YRD*DSIN(ALENS(67,II)))
              YR=(YRD*DCOS(ALENS(67,II)))-(XRD*DSIN(ALENS(67,II)))
C
              LS=((XR**2)/(ALENS(63,II)**2))+
     1           ((YR**2)/(ALENS(62,II)**2))
C
              IF(REAL(LS).GT.(1.0*(AIMTOL**2))) THEN
C       RAY BLOCKED
                  LS1=10.0D0
              ELSE
                  LS1=0.0D0
C       NOT BLOCKED
              END IF
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
              RETURN
C       COERAS NOT 3
          END IF
C
C
C       COERAS=4 RACETRACK COBS ERASE, DOES IT STOP THE RAY
          IF(COERAS.EQ.4) THEN
              LS1=0.0D0
C
C       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
C       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
C       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
C       AND THE FOUR CIRCLES NEED TO BE CHECKED.
C
              IF(ALENS(62,II).LE.ALENS(63,II)) THEN
C       ALENS(63,II) = MAXSID
                  MAXSID=ALENS(63,II)
              ELSE
                  MAXSID=ALENS(62,II)
              END IF
              IF(ALENS(66,II).LT.MAXSID) THEN
C       SETUP THE 8 SIDED BOX
                  N=8
                  X1=-ALENS(63,II)+ALENS(66,II)-AIMTOL
                  Y1=ALENS(62,II)+AIMTOL
                  X2=-ALENS(63,II)-AIMTOL
                  Y2=ALENS(62,II)-ALENS(66,II)+AIMTOL
                  X3=-ALENS(63,II)-AIMTOL
                  Y3=-ALENS(62,II)+ALENS(66,II)-AIMTOL
                  X4=-ALENS(63,II)+ALENS(66,II)-AIMTOL
                  Y4=-ALENS(62,II)-AIMTOL
                  X5=ALENS(63,II)-ALENS(66,II)+AIMTOL
                  Y5=-ALENS(62,II)-AIMTOL
                  X6=ALENS(63,II)+AIMTOL
                  Y6=-ALENS(62,II)+ALENS(66,II)-AIMTOL
                  X7=ALENS(63,II)+AIMTOL
                  Y7=ALENS(62,II)-ALENS(66,II)+AIMTOL
                  X8=ALENS(63,II)-ALENS(66,II)+AIMTOL
                  Y8=ALENS(62,II)+AIMTOL
              ELSE
C       SET UP THE FOUR SIDED BOX
                  N=4
                  X1=-ALENS(63,II)-AIMTOL
                  Y1=ALENS(62,II)+AIMTOL
                  X2=-ALENS(63,II)-AIMTOL
                  Y2=-ALENS(62,II)-AIMTOL
                  X3=ALENS(63,II)+AIMTOL
                  Y3=-ALENS(62,II)-AIMTOL
                  X4=ALENS(63,II)+AIMTOL
                  Y4=ALENS(62,II)+AIMTOL
              END IF
C
              XRD=X
              YRD=Y
              XRD=XRD-ALENS(65,II)
              YRD=YRD-ALENS(64,II)
              XR=(XRD*DCOS(ALENS(67,II)))+(YRD*DSIN(ALENS(67,II)))
              YR=(YRD*DCOS(ALENS(67,II)))-(XRD*DSIN(ALENS(67,II)))
C
C       ARE THE POINTS XR AND YR ON THE POLYGON OR INSIDE
              IF(N.EQ.4) THEN
                  XT(1)=X1
                  YT(1)=Y1
                  XT(2)=X2
                  YT(2)=Y2
                  XT(3)=X3
                  YT(3)=Y3
                  XT(4)=X4
                  YT(4)=Y4
              ELSE
                  XT(1)=X1
                  YT(1)=Y1
                  XT(2)=X2
                  YT(2)=Y2
                  XT(3)=X3
                  YT(3)=Y3
                  XT(4)=X4
                  YT(4)=Y4
                  XT(5)=X5
                  YT(5)=Y5
                  XT(6)=X6
                  YT(6)=Y6
                  XT(7)=X7
                  YT(7)=Y7
                  XT(8)=X8
                  YT(8)=Y8
              END IF
              NP=N
              X0=XR
              Y0=XR
              INS=INSID2()
              IF(INS) THEN
C       RAY NOT BLOCKED
                  LS1=0.0D0
              ELSE
C       RAY BLOCKED
                  LS1=10.0D0
              END IF
C NOW IS THE POINT INSIDE ANY OF THE FOUR CIRCLES
C       CENTER OF THE FIRST CIRCLE IS AT
              XC1=-ALENS(63,II)+ALENS(66,II)
              YC1= ALENS(62,II)-ALENS(66,II)
              CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
C       CENTER OF THE SECOND CIRCLE IS AT
              XC2= -ALENS(63,II)+ALENS(66,II)
              YC2= -ALENS(62,II)+ALENS(66,II)
              CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
C       CENTER OF THE THIRD CIRCLE IS AT
              XC3= ALENS(63,II)-ALENS(66,II)
              YC3=-ALENS(62,II)+ALENS(66,II)
              CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
C       CENTER OF THE FIRST CIRCLE IS AT
              XC4=ALENS(63,II)-ALENS(66,II)
              YC4=ALENS(62,II)-ALENS(66,II)
              CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
C
              RAD2=DSQRT(ALENS(66,II)**2)+AIMTOL
C
              IF(INS.OR.REAL(CS1).GT.REAL(RAD2).OR.REAL(CS2).GT.REAL(RAD2)
     1        .OR.REAL(CS3).GT.REAL(RAD2).OR.REAL(CS4).GT.REAL(RAD2)) THEN
C     RAD BLOCKED BY BOX OR A CIRCLE
                  LS1=10.0D0
              ELSE
                  LS1=0.0D0
              END IF
C
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
C
              RETURN
C       COERAS NOT 4
          END IF
C
C       COERAS=5 POLY COBS ERASE, DOES IT STOP THE RAY
          IF(COERAS.EQ.5) THEN
              LS1=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(63,II), CENTER TO CORNER DISTANCE
C       IS ALENS(62,II). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
              ANGLE=0.0D0
              DO III=1,INT(ALENS(63,II))
                  XT(III)=ALENS(62,II)*DCOS(ANGLE+(PII/2.0D0))
                  YT(III)=ALENS(62,II)*DSIN(ANGLE+(PII/2.0D0))
                  ANGLE=ANGLE+((TWOPII)/ALENS(63,II))
              END DO
              XRD=X
              YRD=Y
C
C       IF NON-ZERO COBS ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION ERASE. REMEMBER.
C
              XRD=XRD-ALENS(65,II)
              YRD=YRD-ALENS(64,II)
C
C       IF A NON-ZERO COBS ERASE TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE ERASE. REMEMBER.

              XR=(XRD*DCOS(ALENS(67,II)))+(YRD*DSIN(ALENS(67,II)))
              YR=(YRD*DCOS(ALENS(67,II)))-(XRD*DSIN(ALENS(67,II)))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(63,II))
              INS=INSID2()
              IF(INS) THEN
C       RAY NOT BLOCKED
                  LS1=0.0D0
              ELSE
C       RAY BLOCKED
                  LS1=10.0D0
              END IF
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
              RETURN
C       COERAS NOT 5
          END IF
C
C       COERAS=6 IPOLY COBS ERASE, DOES IT STOP THE RAY
          IF(COERAS.EQ.6) THEN
              LS1=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(63,II), CENTER TO CORNER DISTANCE
C       IS ALENS(62,II). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
              ANGLE=0.0D0
              DO III=1,INT(ALENS(63,II))
                  XT(III)=IPOLYX(III,II,4)
                  YT(III)=IPOLYY(III,II,4)
              END DO
              XRD=X
              YRD=Y
C
C       IF NON-ZERO COBS ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION ERASE. REMEMBER.
C
              XRD=XRD-ALENS(65,II)
              YRD=YRD-ALENS(64,II)
C
C       IF A NON-ZERO COBS ERASE TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE ERASE. REMEMBER.

              XR=(XRD*DCOS(ALENS(67,II)))+(YRD*DSIN(ALENS(67,II)))
              YR=(YRD*DCOS(ALENS(67,II)))-(XRD*DSIN(ALENS(67,II)))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(63,II))
              INS=INSID2()
              IF(INS) THEN
C       RAY NOT BLOCKED
                  LS1=0.0D0
              ELSE
C       RAY BLOCKED
                  LS1=10.0D0
              END IF
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
              RETURN
C       COERAS NOT 6
          END IF
C
          RETURN
      END
C SUB CAERRS.FOR

      SUBROUTINE CAERRS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE CAERRS.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CLEAR APERTURE ERASE CHECKING AS CALLED BY CACHEK.FOR
C
          EXTERNAL INSID1
C
          INTEGER II,I,N,III
C
          INTEGER CAERAS,COERAS
C
          REAL*8 X,Y,Z,ANGLE,
     1    XR,YR,LS1,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,
     2    X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,
     3    YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4
C
          COMMON/CACO/CAERAS,COERAS,LS
C
          LOGICAL INS,INSID1
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       NOW ALL THE CAERAS WAS ALREADY SET AND THE RAY WAS
C       BLOCKED BY A CLAP ON SURFACE I. DOES THE BLOCK GET CANCELED
C       BY THE CLAP ERASE ON I+I = II
C
          I=R_I
          X=R_X
          Y=R_Y
          Z=R_Z
          II=I
C
C       CAERAS=1 CIRCULAR CLAP ERASE, DOES IT STOP THE RAY
          IF(CAERAS.EQ.1) THEN
              LS1=0.0D0
C
C       CIRCULAR CLAP EQUATION IS:
C
C       (X)**2 + (Y)**2 = R**2
C
C       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
C       GREATER THAN THE RIGHT SIDE, THE RAY IS BLOCKED
C       ELSE IT IS NOT BLOCKED.
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.

              XR=X-ALENS(55,II)
              YR=Y-ALENS(54,II)
C
              LS1=DSQRT((XR**2)+(YR**2))
C
              RS=DSQRT(ALENS(52,II)**2)+AIMTOL
              IF(REAL(LS1).GT.REAL(RS)) THEN
                  LS1=10.0D0
              ELSE
                  LS1=0.0D0
              END IF
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
              RETURN
C       CAERAS NOT 1
          END IF
C
C
C       CAERAS=2 RECTANGULAR CLAP ERASE, DOES IT STOP THE RAY
          IF(CAERAS.EQ.2) THEN
              LS1=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
C       COORDINATES ARE:
              X1=-ALENS(53,II)-AIMTOL
              Y1=ALENS(52,II)+AIMTOL
              X2=-ALENS(53,II)-AIMTOL
              Y2=-ALENS(52,II)-AIMTOL
              X3=ALENS(53,II)+AIMTOL
              Y3=-ALENS(52,II)-AIMTOL
              X4=ALENS(53,II)+AIMTOL
              Y4=ALENS(52,II)+AIMTOL
C
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
              XRD=XRD-ALENS(55,II)
              YRD=YRD-ALENS(54,II)
C
C       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE ERASE. REMEMBER.

              XR=(XRD*DCOS(ALENS(57,II)))+(YRD*DSIN(ALENS(57,II)))
              YR=(YRD*DCOS(ALENS(57,II)))-(XRD*DSIN(ALENS(57,II)))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              XT(1)=X1
              YT(1)=Y1
              XT(2)=X2
              YT(2)=Y2
              XT(3)=X3
              YT(3)=Y3
              XT(4)=X4
              YT(4)=Y4
              X0=XR
              Y0=YR
              NP=4
              INS=INSID1()
              IF(INS) THEN
C       RAY NOT BLOCKED
                  LS1=0.0D0
              ELSE
C       RAY BLOCKED
                  LS1=10.0D0
              END IF
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
              RETURN
C       CAERAS NOT 2
          END IF
C
C       CAERAS=3 ELLIPTICAL CLAP, DOES IT STOP THE RAY
          IF(CAERAS.EQ.3) THEN
              LS1=0.0D0
C
C       ELLIPTICAL CLAP EQUATION IS:
C
C       (X)**2/A**2  + (Y)**2/B**2 = 1
C
C       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
C       GREATER THAN 1.0D0, THE RAY IS BLOCKED
C       ELSE IT IS NOT BLOCKED.
C
              XRD=X
              YRD=Y
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
              XRD=XRD-ALENS(55,II)
              YRD=YRD-ALENS(54,II)
C
C       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE ERASE.
C
              XR=(XRD*DCOS(ALENS(57,II)))+(YRD*DSIN(ALENS(57,II)))
              YR=(YRD*DCOS(ALENS(57,II)))-(XRD*DSIN(ALENS(57,II)))
C
C
              LS=((XR**2)/(ALENS(53,II)**2))+
     1           ((YR**2)/(ALENS(52,II)**2))
C
              IF(REAL(LS).GT.(1.0+(AIMTOL**2))) THEN
C       RAY BLOCKED
                  LS1=10.0D0
              ELSE
C       NOT BLOCKED
                  LS1=0.0D0
              END IF
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
              RETURN
C       CAERAS NOT 3
          END IF
C
C
C       CAERAS=4 RACETRACK CLAP ERASE, DOES IT STOP THE RAY
          IF(CAERAS.EQ.4) THEN
              LS1=0.0D0
C
C       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
C       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
C       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
C       AND THE FOUR CIRCLES NEED TO BE CHECKED.
C
              IF(ALENS(52,II).LE.ALENS(53,II)) THEN
C       ALENS(53,II) = MAXSID
                  MAXSID=ALENS(53,II)
              ELSE
                  MAXSID=ALENS(52,II)
              END IF
              IF(ALENS(56,II).LT.MAXSID) THEN
C       SETUP THE 8 SIDED BOX
                  N=8
                  X1=-ALENS(53,II)+ALENS(56,II)-AIMTOL
                  Y1=ALENS(52,II)+AIMTOL
                  X2=-ALENS(53,II)-AIMTOL
                  Y2=ALENS(52,II)-ALENS(56,II)+AIMTOL
                  X3=-ALENS(53,II)-AIMTOL
                  Y3=-ALENS(52,II)+ALENS(56,II)-AIMTOL
                  X4=-ALENS(53,II)+ALENS(56,II)-AIMTOL
                  Y4=-ALENS(52,II)-AIMTOL
                  X5=ALENS(53,II)-ALENS(56,II)+AIMTOL
                  Y5=-ALENS(52,II)-AIMTOL
                  X6=ALENS(53,II)+AIMTOL
                  Y6=-ALENS(52,II)+ALENS(56,II)-AIMTOL
                  X7=ALENS(53,II)+AIMTOL
                  Y7=ALENS(52,II)-ALENS(56,II)+AIMTOL
                  X8=ALENS(53,II)-ALENS(56,II)+AIMTOL
                  Y8=ALENS(52,II)+AIMTOL
              ELSE
C       SET UP THE FOUR SIDED BOX
                  N=4
                  X1=-ALENS(53,II)-AIMTOL
                  Y1=ALENS(52,II)+AIMTOL
                  X2=-ALENS(53,II)-AIMTOL
                  Y2=-ALENS(52,II)-AIMTOL
                  X3=ALENS(53,II)+AIMTOL
                  Y3=-ALENS(52,II)-AIMTOL
                  X4=ALENS(53,II)+AIMTOL
                  Y4=ALENS(52,II)+AIMTOL
              END IF
C
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
              XRD=XRD-ALENS(55,II)
              YRD=YRD-ALENS(54,II)
C
C       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE ERASE .
C
              XR=(XRD*DCOS(ALENS(57,II)))+(YRD*DSIN(ALENS(57,II)))
              YR=(YRD*DCOS(ALENS(57,II)))-(XRD*DSIN(ALENS(57,II)))
C
C
C       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
              IF(N.EQ.4) THEN
                  XT(1)=X1
                  YT(1)=Y1
                  XT(2)=X2
                  YT(2)=Y2
                  XT(3)=X3
                  YT(3)=Y3
                  XT(4)=X4
                  YT(4)=Y4
              ELSE
                  XT(1)=X1
                  YT(1)=Y1
                  XT(2)=X2
                  YT(2)=Y2
                  XT(3)=X3
                  YT(3)=Y3
                  XT(4)=X4
                  YT(4)=Y4
                  XT(5)=X5
                  YT(5)=Y5
                  XT(6)=X6
                  YT(6)=Y6
                  XT(7)=X7
                  YT(7)=Y7
                  XT(8)=X8
                  YT(8)=Y8
              END IF
              NP=N
              X0=XR
              Y0=YR
              INS=INSID1()
              IF(INS) THEN
C       RAY NOT BLOCKED
                  LS1=0.0D0
              ELSE
C       RAY BLOCKED
                  LS1=10.0D0
              END IF
C NOW IS THE POINT INSIDE ANY OF THE FOUR CIRCLES
C       CENTER OF THE FIRST CIRCLE IS AT
              XC1=-ALENS(53,II)+ALENS(56,II)
              YC1= ALENS(52,II)-ALENS(56,II)
              CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
C       CENTER OF THE SECOND CIRCLE IS AT
              XC2=-ALENS(53,II)+ALENS(56,II)
              YC2=-ALENS(52,II)+ALENS(56,II)
              CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
C       CENTER OF THE THIRD CIRCLE IS AT
              XC3= ALENS(53,II)-ALENS(56,II)
              YC3=-ALENS(52,II)+ALENS(56,II)
              CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
C       CENTER OF THE FIRST CIRCLE IS AT
              XC4=ALENS(53,II)-ALENS(56,II)
              YC4=ALENS(52,II)-ALENS(56,II)
              CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
C
              RAD2=DSQRT(ALENS(56,II)**2)+AIMTOL
C
              IF(.NOT.INS.AND.REAL(CS1).GT.REAL(RAD2).AND.REAL(CS2)
     1        .GT.REAL(RAD2).AND.REAL(CS3).GT.
     1        REAL(RAD2).AND.REAL(CS4).GT.REAL(RAD2)) THEN
C     RAY BLOCKED BY BOX AND CIRCLES
                  LS1=10.0D0
              ELSE
                  LS1=0.0D0
              END IF
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
C
              RETURN
C       CAERAS NOT 4
          END IF
C
C       CAERAS=5 POLY CLAP ERASE, DOES IT STOP THE RAY
          IF(CAERAS.EQ.5) THEN
              LS1=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(53,II), CENTER TO CORNER DISTANCE
C       IS ALENS(52,II). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
              ANGLE=0.0D0
              DO III=1,INT(ALENS(53,II))
                  XT(III)=ALENS(52,II)*DCOS(ANGLE+(PII/2.0D0))
                  YT(III)=ALENS(52,II)*DSIN(ANGLE+(PII/2.0D0))
                  ANGLE=ANGLE+((TWOPII)/ALENS(53,II))
              END DO
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
              XRD=XRD-ALENS(55,II)
              YRD=YRD-ALENS(54,II)
C
C       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE ERASE. REMEMBER.

              XR=(XRD*DCOS(ALENS(57,II)))+(YRD*DSIN(ALENS(57,II)))
              YR=(YRD*DCOS(ALENS(57,II)))-(XRD*DSIN(ALENS(57,II)))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(53,II))
              INS=INSID1()
              IF(INS) THEN
C       RAY NOT BLOCKED
                  LS1=0.0D0
              ELSE
C       RAY BLOCKED
                  LS1=10.0D0
              END IF
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
              RETURN
C       CAERAS NOT 5
          END IF
C       CAERAS=6 IPOLY CLAP ERASE, DOES IT STOP THE RAY
          IF(CAERAS.EQ.6) THEN
              LS1=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(53,II), CENTER TO CORNER DISTANCE
C       IS ALENS(52,II). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
              ANGLE=0.0D0
              DO III=1,INT(ALENS(53,II))
                  XT(III)=IPOLYX(III,II,2)
                  YT(III)=IPOLYY(III,II,2)
              END DO
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
              XRD=XRD-ALENS(55,II)
              YRD=YRD-ALENS(54,II)
C
C       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE ERASE. REMEMBER.

              XR=(XRD*DCOS(ALENS(57,II)))+(YRD*DSIN(ALENS(57,II)))
              YR=(YRD*DCOS(ALENS(57,II)))-(XRD*DSIN(ALENS(57,II)))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(53,II))
              INS=INSID1()
              IF(INS) THEN
C       RAY NOT BLOCKED
                  LS1=0.0D0
              ELSE
C       RAY BLOCKED
                  LS1=10.0D0
              END IF
              IF(LS1.EQ.0.0D0) LS=0.0D0
              IF(LS1.EQ.10.0D0) LS=10.0D0
              RETURN
C       CAERAS NOT 6
          END IF
C
          RETURN
      END

C SUB FNBDE.FOR
      SUBROUTINE FNBDE(I)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FNBDE(I) WHICH REMOVES ALL
C       FNBY,FNBX,ERY,ERX,MAGY AND MAGX ENTRIES FROM CONFIG I.
C
          INTEGER SCRCNT,I,J,ALLOERR
C
          CHARACTER BLANK*140
C
          COMMON/BLAAA/BLANK
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmai.inc'
C
          CHARACTER*140 SCRATH
          DIMENSION SCRATH(:)
          ALLOCATABLE :: SCRATH
          INTEGER NANA
          NANA=2000
          DEALLOCATE (SCRATH,STAT=ALLOERR)
          ALLOCATE (SCRATH(NANA),STAT=ALLOERR)
C
          BLANK=AA//AA//AA//AA//AA//AA//AA
          SCRATH(1:2000)=BLANK
          SCRCNT=0
          DO 15 J=1,CFGCNT(I)
              EE12=CONFG(I,J)
              HOLDER=EE12
              IF((HOLDER(1:3)).EQ.'MAG'.OR.
     1        (HOLDER(1:3)).EQ.'FNB'.OR.
     2        (HOLDER(1:2)).EQ.'ER') THEN
                  IF((HOLDER(1:4)).EQ.'FNBY') THEN
                      WRITE(OUTLYNE,*)'"FNBY" REMOVED FROM CONFIGURATION #',I
                      CALL SHOWIT(1)
                  ELSE
                  END IF
                  IF((HOLDER(1:4)).EQ.'FNBX') THEN

                      WRITE(OUTLYNE,*)'"FNBX" REMOVED FROM CONFIGURATION #',I
                      CALL SHOWIT(1)
                  END IF
                  IF((HOLDER(1:3)).EQ.'ERY') THEN

                      WRITE(OUTLYNE,*)'"ERY" REMOVED FROM CONFIGURATION #',I
                      CALL SHOWIT(1)
                  END IF
                  IF((HOLDER(1:3)).EQ.'ERX') THEN

                      WRITE(OUTLYNE,*)'"ERX" REMOVED FROM CONFIGURATION #',I
                      CALL SHOWIT(1)
                  END IF
                  IF((HOLDER(1:4)).EQ.'MAGY') THEN

                      WRITE(OUTLYNE,*)'"MAGY" REMOVED FROM CONFIGURATION #',I
                      CALL SHOWIT(1)
                  END IF
                  IF((HOLDER(1:4)).EQ.'MAGX') THEN

                      WRITE(OUTLYNE,*)'"MAGX" REMOVED FROM CONFIGURATION #',I
                      CALL SHOWIT(1)
                  END IF
                  HOLDER=BLANK
                  GO TO 15
              ELSE
                  SCRATH(J)=HOLDER
                  SCRCNT=SCRCNT+1
              END IF
 15       CONTINUE
          DO 16 J=1,SCRCNT
              EE12=SCRATH(J)(1:140)
              CONFG(I,J)=EE12
 16       CONTINUE
          CFGCNT(I)=SCRCNT
          DEALLOCATE(SCRATH,STAT=ALLOERR)
          RETURN
      END
C SUB GLSWVL.FOR
      SUBROUTINE GLSWVL
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS CONTROLS THE OPERATION OF THE "GLASSWV" COMMAND
C       COMMAND
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"GLASSWV" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'LENS'.AND.WQ.NE.'NSSLENS') THEN
              WRITE(OUTLYNE,*)'"GLASSWV" TAKES NO QUALIFIER WORD OR IT TAKES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'"LENS" OR "NSSLENS"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'LENS') THEN
              GLSWV(1:10)=0.0D0
              GLSWV(1)=SYSTEM1(1)
              GLSWV(2)=SYSTEM1(2)
              GLSWV(3)=SYSTEM1(3)
              GLSWV(4)=SYSTEM1(4)
              GLSWV(5)=SYSTEM1(5)
              GLSWV(6)=SYSTEM1(71)
              GLSWV(7)=SYSTEM1(72)
              GLSWV(8)=SYSTEM1(73)
              GLSWV(9)=SYSTEM1(74)
              GLSWV(10)=SYSTEM1(75)
          END IF
          IF(WQ.EQ.'NSSLENS') THEN
              IF(NEXISTN) THEN
                  GLSWV(1:10)=0.0D0
                  GLSWV(1)=NSSSYSTEM(1)
                  GLSWV(2)=NSSSYSTEM(2)
                  GLSWV(3)=NSSSYSTEM(3)
                  GLSWV(4)=NSSSYSTEM(4)
                  GLSWV(5)=NSSSYSTEM(5)
                  GLSWV(6)=NSSSYSTEM(6)
                  GLSWV(7)=NSSSYSTEM(7)
                  GLSWV(8)=NSSSYSTEM(8)
                  GLSWV(9)=NSSSYSTEM(9)
                  GLSWV(10)=NSSSYSTEM(10)
              ELSE
                  WRITE(OUTLYNE,*)'NO NSS DATABASE EXISTS, "GLASSWV NSSLENS"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'CAN NOT SET GLASS WAVELENGTHS'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(STI.EQ.1.OR.SN.EQ.0.AND.SQ.EQ.0.AND.SST.EQ.0) THEN
! 100  FORMAT(
!     1'THE TEN GLASS EVALUATION WAVELENGTHS IN MICRONS CURRENTLY ARE:')
 101          FORMAT(
     1        'WAVELENGTH #1  = ',G13.6,'MICRONS')
 102          FORMAT(
     1        'WAVELENGTH #2  = ',G13.6,'MICRONS')
 103          FORMAT(
     1        'WAVELENGTH #3  = ',G13.6,'MICRONS')
 104          FORMAT(
     1        'WAVELENGTH #4  = ',G13.6,'MICRONS')
 105          FORMAT(
     1        'WAVELENGTH #5  = ',G13.6,'MICRONS')
 106          FORMAT(
     1        'WAVELENGTH #6  = ',G13.6,'MICRONS')
 107          FORMAT(
     1        'WAVELENGTH #7  = ',G13.6,'MICRONS')
 108          FORMAT(
     1        'WAVELENGTH #8  = ',G13.6,'MICRONS')
 109          FORMAT(
     1        'WAVELENGTH #9  = ',G13.6,'MICRONS')
 110          FORMAT(
     1        'WAVELENGTH #10 = ',G13.6,'MICRONS')
              WRITE(OUTLYNE,101) GLSWV(1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,102) GLSWV(2)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,103) GLSWV(3)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,104) GLSWV(4)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,105) GLSWV(5)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,106) GLSWV(6)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,107) GLSWV(7)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,108) GLSWV(8)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,109) GLSWV(9)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,110) GLSWV(10)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              GLSWV(1:10)=0.0D0
              IF(DF1.EQ.0) GLSWV(1)=W1
              IF(DF2.EQ.0) GLSWV(2)=W2
              IF(DF3.EQ.0) GLSWV(3)=W3
              IF(DF4.EQ.0) GLSWV(4)=W4
              IF(DF5.EQ.0) GLSWV(5)=W5
          END IF
C     ASSIGN NEW VALUES

          RETURN
      END
C SUB GLSRIN.FOR
      SUBROUTINE GLSRIN
C
          IMPLICIT NONE
C
C       IMPLICIT NONE
C
          LOGICAL EXIS36
C
          CHARACTER FLNAME*12,NAME*13,NUMBER*13,FLTP*12
C
          CHARACTER*13 NAME1,NAME2
C
          INTEGER I,J,TOTAL,COUNT,LASCNT
C
          REAL*8 LMAX,LMIN,LAMBDA,A0,A1,
     2    A2,A3,A4,A5,PN,LAM(1:10),LAMLOW,
     3    LAMUPP,PNSC
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       NOW RESOLVE THE CALCULATION OF REFRACTIVE INDICES VIA
C       GLASS CATALOG SEARCHES.
C
C       THERE IS AN EQUATION FOR CALCULATION OF INDICES USED
C       ACROSS THE INDUSTRY. IT IS
C
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
C       DETERMINE MINIMUM AND MAX GLSWV WAVELENGTHS
          LMIN=GLSWV(1)
          IF(GLSWV(2).LT.LMIN) LMIN=GLSWV(2)
          IF(GLSWV(3).LT.LMIN) LMIN=GLSWV(3)
          IF(GLSWV(4).LT.LMIN) LMIN=GLSWV(4)
          IF(GLSWV(5).LT.LMIN) LMIN=GLSWV(5)
          LMAX=GLSWV(1)
          IF(GLSWV(2).GT.LMAX) LMAX=GLSWV(2)
          IF(GLSWV(3).GT.LMAX) LMAX=GLSWV(3)
          IF(GLSWV(4).GT.LMAX) LMAX=GLSWV(4)
          IF(GLSWV(5).GT.LMAX) LMAX=GLSWV(5)
          IF(LMIN.EQ.0.0D0) LMIN=LMAX
          IF(LMAX.EQ.0.0D0) LMAX=LMIN
C       SET LAM(1) to LAM(10) TO THE 10 WAVELENGTHS
          LAM(1)=GLSWV(1)
          LAM(2)=GLSWV(2)
          LAM(3)=GLSWV(3)
          LAM(4)=GLSWV(4)
          LAM(5)=GLSWV(5)
          LAM(6)=GLSWV(6)
          LAM(7)=GLSWV(7)
          LAM(8)=GLSWV(8)
          LAM(9)=GLSWV(9)
          LAM(10)=GLSWV(10)
          IF(WC.EQ.'AIR') THEN
              GPREG(1)=1.0D0
              GPREG(2)=1.0D0
              GPREG(3)=1.0D0
              GPREG(4)=1.0D0
              GPREG(5)=1.0D0
              GPREG(6)=1.0D0
              GPREG(7)=1.0D0
              GPREG(8)=1.0D0
              GPREG(9)=1.0D0
              GPREG(10)=1.0D0
              RETURN
          END IF
C
C
          IF(SST.EQ.0) THEN
              OUTLYNE='"'//WC//'" REQUIRES AN EXPLICIT GLASS NAME'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"'//WC//'" TAKES NO QUALIFIER OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              OUTLYNE='NO ADDITIONAL INFORMATION AVAILABLE'
              CALL SHOWIT(1)
              RETURN
          END IF
C
C       IS THE SURFACE MATERIAL A CATALOG MATERIAL?
          IF(WC.EQ.'SCHOTT'.OR.WC.EQ.'CHANCE'.OR.WC.EQ.'SCH2000'
     1    .OR.WC.EQ.'OHARA'.OR.WC.EQ.'CORNIN'
     2    .OR.WC.EQ.'HOYA'.OR.WC.EQ.'GLA'
     5    .OR.WC.EQ.'USER'.OR.WC.EQ.'RUSSIAN'
     6    .OR.WC.EQ.'GLCAT'.OR.WC.EQ.'RADHARD'
     7    .OR.WC.EQ.'MATL'.OR.WC.EQ.'HIKARI') THEN
              NAME1=WC//'     '
              NAME2=WS(1:13)
              FLNAME=' '
              CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
C
C       COMPARE LAMLOW,LAMUPP,LMIN AND LMAX
              IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW
     1        .AND.LMIN.NE.0.0D0) THEN
C       STOP AND GO TO CMD LEVEL AND PRINT WARNING
                  OUTLYNE=
     1            'THE GLASS - '//WS
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'IS NOT DEFINED ACROSS THE CURRENT GLSWV WAVELENGTH RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,200) LAMLOW,LAMUPP
                  CALL SHOWIT(1)
 200              FORMAT('IT IS DEFINED ONLY FROM ',F9.4,' TO ',F9.4,
     1            ' MICRON(S)')
                  OUTLYNE=
     1            'ALL REFRACTIVE INDICES FOR THIS GLASS HAVE BEEN SET TO 1.0'
                  CALL SHOWIT(1)
                  GPREG(1)=1.0D0
                  GPREG(2)=1.0D0
                  GPREG(3)=1.0D0
                  GPREG(4)=1.0D0
                  GPREG(5)=1.0D0
                  GPREG(6)=1.0D0
                  GPREG(7)=1.0D0
                  GPREG(8)=1.0D0
                  GPREG(9)=1.0D0
                  GPREG(10)=1.0D0
                  RETURN
              ELSE
C       PROCEED, THE GLASS IS DEFINED WITHIN THE CURRENT
C       LENS GLSWV WAVELENGTHS
              END IF
C       NOW LOOK UP THE GLASS IN THE CATALOG, RETRIEVE THE
C       COEFFICIENTS A0 TO A5 AND CALCULATE AND SET THE
C       REFRACTIVE INDICES.
C
              FLTP='            '
              IF(WC.EQ.'SCHOTT')   FLNAME='SCHOTT.BIN'
              IF(WC.EQ.'SCH2000')  FLNAME='SCH2000.BIN'
              IF(WC.EQ.'HOYA')     FLNAME='HOYA.BIN  '
              IF(WC.EQ.'HIKARI')   FLNAME='HIKARI.BIN'
              IF(WC.EQ.'OHARA')    FLTP='MULTIOHARA  '
              IF(WC.EQ.'CORNIN')   FLNAME='CORNIN.BIN'
              IF(WC.EQ.'CHANCE')   FLNAME='CHANCE.BIN'
              IF(WC.EQ.'RADHARD')  FLNAME='RADHARD.BIN'
              IF(WC.EQ.'USER')     FLNAME='USER.DAT  '
              IF(WC.EQ.'GLCAT')    FLTP='MULTI       '
              IF(WC.EQ.'GLA')      FLTP='MULTI       '
              IF(WC.EQ.'MATL  ')   FLTP='MATL        '
              IF(WC.EQ.'RUSSIAN')  FLTP='RUSSIAN     '
C
              IF(FLTP.EQ.'            '.AND.FLNAME.NE.'USER.DAT  ') THEN
C     REGULAR GLASS CATALOG
C
                  EXIS36=.FALSE.
                  INQUIRE(FILE=trim(LIBGLA)//FLNAME,EXIST=EXIS36)
                  IF(EXIS36) THEN
C               PROCEED
                      OPEN(UNIT=36,ACCESS='DIRECT',FILE=trim(LIBGLA)//FLNAME,
     1                FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                      READ(UNIT=36,REC=1) TOTAL
                      DO 300 J=2,TOTAL+1
                          READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                          IF(WS.EQ.NAME.OR.WS.EQ.NUMBER) THEN
C       CALCULATE INDEX THEN RETURN
                              IF(LAM(1).EQ.0.0D0) THEN
                                  GPREG(1)=1.0D0
                              ELSE
C
                                  IF(FLNAME.EQ.'HOYA.BIN')
     1                              GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'HIKARI.BIN')
     1                              GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CORNIN.BIN')
     1                              GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CHANCE.BIN')
     1                              GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'RADHARD.BIN')
     1                              GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA-O.BIN')
     1                              GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
C
                                  IF(FLNAME.EQ.'SCHOTT.BIN')
     1                              GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'SCH2000.BIN')
     1                              GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA.BIN')
     1                              GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                              END IF
                              IF(LAM(2).EQ.0.0D0) THEN
                                  GPREG(2)=1.0D0
                              ELSE
C
                                  IF(FLNAME.EQ.'HOYA.BIN')
     1                              GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'HIKARI.BIN')
     1                              GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CORNIN.BIN')
     1                              GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CHANCE.BIN')
     1                              GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'RADHARD.BIN')
     1                              GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA-O.BIN')
     1                              GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
C
                                  IF(FLNAME.EQ.'SCHOTT.BIN')
     1                              GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'SCH2000.BIN')
     1                              GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA.BIN')
     1                              GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                              END IF
                              IF(LAM(3).EQ.0.0D0) THEN
                                  GPREG(3)=1.0D0
                              ELSE
C
                                  IF(FLNAME.EQ.'HOYA.BIN')
     1                              GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'HIKARI.BIN')
     1                              GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CORNIN.BIN')
     1                              GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CHANCE.BIN')
     1                              GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'RADHARD.BIN')
     1                              GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA-O.BIN')
     1                              GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
C
                                  IF(FLNAME.EQ.'SCHOTT.BIN')
     1                              GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'SCH2000.BIN')
     1                              GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA.BIN')
     1                              GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                              END IF
                              IF(LAM(4).EQ.0.0D0) THEN
                                  GPREG(4)=1.0D0
                              ELSE
C
                                  IF(FLNAME.EQ.'HOYA.BIN')
     1                              GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'HIKARI.BIN')
     1                              GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CORNIN.BIN')
     1                              GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CHANCE.BIN')
     1                              GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'RADHARD.BIN')
     1                              GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA-O.BIN')
     1                              GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
C
                                  IF(FLNAME.EQ.'SCHOTT.BIN')
     1                              GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'SCH2000.BIN')
     1                              GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA.BIN')
     1                              GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                              END IF
                              IF(LAM(5).EQ.0.0D0) THEN
                                  GPREG(5)=1.0D0
                              ELSE
C
                                  IF(FLNAME.EQ.'HOYA.BIN')
     1                              GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'HIKARI.BIN')
     1                              GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CORNIN.BIN')
     1                              GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CHANCE.BIN')
     1                              GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'RADHARD.BIN')
     1                              GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA-O.BIN')
     1                              GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
C
                                  IF(FLNAME.EQ.'SCHOTT.BIN')
     1                              GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'SCH2000.BIN')
     1                              GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA.BIN')
     1                              GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                              END IF
                              IF(LAM(6).EQ.0.0D0) THEN
                                  GPREG(6)=1.0D0
                              ELSE
C
                                  IF(FLNAME.EQ.'HOYA.BIN')
     1                              GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'HIKARI.BIN')
     1                              GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CORNIN.BIN')
     1                              GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CHANCE.BIN')
     1                              GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'RADHARD.BIN')
     1                              GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA-O.BIN')
     1                              GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
C
                                  IF(FLNAME.EQ.'SCHOTT.BIN')
     1                              GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'SCH2000.BIN')
     1                              GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA.BIN')
     1                              GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                              END IF
                              IF(LAM(7).EQ.0.0D0) THEN
                                  GPREG(7)=1.0D0
                              ELSE
C
                                  IF(FLNAME.EQ.'HOYA.BIN')
     1                              GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'HIKARI.BIN')
     1                              GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CORNIN.BIN')
     1                              GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CHANCE.BIN')
     1                              GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'RADHARD.BIN')
     1                              GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA-O.BIN')
     1                              GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
C
                                  IF(FLNAME.EQ.'SCHOTT.BIN')
     1                              GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'SCH2000.BIN')
     1                              GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA.BIN')
     1                              GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                              END IF
                              IF(LAM(8).EQ.0.0D0) THEN
                                  GPREG(8)=1.0D0
                              ELSE
C
                                  IF(FLNAME.EQ.'HOYA.BIN')
     1                              GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'HIKARI.BIN')
     1                              GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CORNIN.BIN')
     1                              GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CHANCE.BIN')
     1                              GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'RADHARD.BIN')
     1                              GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA-O.BIN')
     1                              GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
C
                                  IF(FLNAME.EQ.'SCHOTT.BIN')
     1                              GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'SCH2000.BIN')
     1                              GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA.BIN')
     1                              GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                              END IF
                              IF(LAM(9).EQ.0.0D0) THEN
                                  GPREG(9)=1.0D0
                              ELSE
C
                                  IF(FLNAME.EQ.'HOYA.BIN')
     1                              GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'HIKARI.BIN')
     1                              GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CORNIN.BIN')
     1                              GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CHANCE.BIN')
     1                              GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'RADHARD.BIN')
     1                              GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA-O.BIN')
     1                              GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
C
                                  IF(FLNAME.EQ.'SCHOTT.BIN')
     1                              GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'SCH2000.BIN')
     1                              GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA.BIN')
     1                              GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                              END IF
                              IF(LAM(10).EQ.0.0D0) THEN
                                  GPREG(10)=1.0D0
                              ELSE
C
                                  IF(FLNAME.EQ.'HOYA.BIN')
     1                              GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'HIKARI.BIN')
     1                              GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CORNIN.BIN')
     1                              GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'CHANCE.BIN')
     1                              GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'RADHARD.BIN')
     1                              GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA-O.BIN')
     1                              GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
C
                                  IF(FLNAME.EQ.'SCHOTT.BIN')
     1                              GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'SCH2000.BIN')
     1                              GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                                  IF(FLNAME.EQ.'OHARA.BIN')
     1                              GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                              END IF
                              RETURN
                          ELSE
C       KEEP SEARCHING THE CATALOG
                          END IF
 300                  CONTINUE
C       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
                      OUTLYNE=
     1                'GLASS NOT FOUND IN THE '//WC//' CATALOG'
                      CALL SHOWIT(1)
                      OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
                      CALL SHOWIT(1)
                      GPREG(1:5)=1.0D0
C
                  ELSE
C       CATALOG REQUESTED NOT YET INSTALLED.
                      OUTLYNE=
     1                WC//' GLASS CATALOG NOT YET INSTALLED'
                      CALL SHOWIT(1)
                      OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
                      CALL SHOWIT(1)
                      GPREG(1:5)=1.0D0
                  END IF
              ELSE
C     FLTP WAS NOT BLANK
              END IF
C
              IF(FLTP.EQ.'MULTI       '.AND.FLNAME.NE.'USER.DAT  ') THEN
C     SEARCH 5 EXISTING CATALOGS
                  LASCNT=0
                  DO COUNT=1,9
                      LASCNT=COUNT
                      IF(COUNT.EQ.1) FLNAME='SCHOTT.BIN'
                      IF(COUNT.EQ.2) FLNAME='SCH2000.BIN'
                      IF(COUNT.EQ.3) FLNAME='HOYA.BIN'
                      IF(COUNT.EQ.4) FLNAME='OHARA.BIN'
                      IF(COUNT.EQ.5) FLNAME='OHARA-O.BIN'
                      IF(COUNT.EQ.6) FLNAME='CORNIN.BIN'
                      IF(COUNT.EQ.7) FLNAME='CHANCE.BIN'
                      IF(COUNT.EQ.8) FLNAME='RADHARD.BIN'
                      IF(COUNT.EQ.9) FLNAME='HIKARI.BIN'
                      IF(COUNT.EQ.1) WC='SCHOTT  '
                      IF(COUNT.EQ.2) WC='SCH2000 '
                      IF(COUNT.EQ.3) WC='HOYA    '
                      IF(COUNT.EQ.4) WC='OHARA   '
                      IF(COUNT.EQ.5) WC='OHARA   '
                      IF(COUNT.EQ.6) WC='CORNIN  '
                      IF(COUNT.EQ.7) WC='CHANCE  '
                      IF(COUNT.EQ.8) WC='RADHARD '
                      IF(COUNT.EQ.9) WC='HIKARI  '
                      EXIS36=.FALSE.
                      INQUIRE(FILE=trim(LIBGLA)//FLNAME,EXIST=EXIS36)
                      IF(EXIS36) THEN
C               PROCEED
                          OPEN(UNIT=36,ACCESS='DIRECT',FILE=trim(LIBGLA)//FLNAME,
     1                    FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                          READ(UNIT=36,REC=1) TOTAL
                          DO J=2,TOTAL+1
                              READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                              IF(WS.EQ.NAME.OR.WS.EQ.NUMBER) THEN
C     FOUND THE GLASS
                                  IF(COUNT.EQ.1) WRITE(OUTLYNE,*) 'GLASS MFG. IS SCHOTT'
                                  IF(COUNT.EQ.2) WRITE(OUTLYNE,*) 'GLASS MFG. IS SCHOTT(POST 2000)'
                                  IF(COUNT.EQ.3) WRITE(OUTLYNE,*) 'GLASS MFG. IS HOYA'
                                  IF(COUNT.EQ.4) WRITE(OUTLYNE,*) 'GLASS MFG. IS OHARA'
                                  IF(COUNT.EQ.5) WRITE(OUTLYNE,*) 'GLASS MFG. IS OHARA'
                                  IF(COUNT.EQ.6) WRITE(OUTLYNE,*) 'GLASS MFG. IS CORNING'
                                  IF(COUNT.EQ.7) WRITE(OUTLYNE,*) 'GLASS MFG. IS CHANCE-PILKINGTON'
                                  IF(COUNT.EQ.8) WRITE(OUTLYNE,*) 'GLASS MFG. IS HIKARI'
                                  IF(COUNT.GE.1.AND.COUNT.LE.8) CALL SHOWIT(1)
C
C     NOW RECHECK THE WAVELENGTH RANGE
                                  NAME1=WC//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
C
C       COMPARE LAMLOW,LAMUPP,LMIN AND LMAX
                                  IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW
     1                            .AND.LMIN.NE.0.0D0) THEN
C       STOP AND GO TO CMD LEVEL AND PRINT WARNING
                                      WRITE(OUTLYNE,*)
     1                                  'GLASS DEFINITION WARNING: FOR SURFACE ',I
                                      CALL SHOWIT(1)
                                      OUTLYNE=
     1                                'THE GLASS - '//WS
                                      CALL SHOWIT(1)
                                      OUTLYNE=
     1                                'IS NOT DEFINED ACROSS THE CURRENT SYSTEM WAVELENGTH RANGE'
                                      CALL SHOWIT(1)
                                      WRITE(OUTLYNE,200) LAMLOW,LAMUPP
                                      CALL SHOWIT(1)
                                      OUTLYNE=
     1                                'ALL REFRACTIVE INDICES FOR THIS SURFACE HAVE BEEN SET TO 1.0'
                                      CALL SHOWIT(1)
                                      GPREG(1:5)=1.0D0
                                  END IF
C       CALCULATE INDEX THEN RETURN
                                  IF(LAM(1).EQ.0.0D0) THEN
                                      GPREG(1)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                  GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                  GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                  GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                  GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(2).EQ.0.0D0) THEN
                                      GPREG(2)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                  GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                  GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                  GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                  GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(3).EQ.0.0D0) THEN
                                      GPREG(3)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                  GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                  GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                  GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                  GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(4).EQ.0.0D0) THEN
                                      GPREG(4)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                  GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                  GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                  GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                  GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(5).EQ.0.0D0) THEN
                                      GPREG(5)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                  GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                  GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                  GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                  GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(6).EQ.0.0D0) THEN
                                      GPREG(6)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                  GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                  GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                  GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                  GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(7).EQ.0.0D0) THEN
                                      GPREG(7)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                  GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                  GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                  GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                  GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(8).EQ.0.0D0) THEN
                                      GPREG(8)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                  GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                  GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                  GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                  GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(9).EQ.0.0D0) THEN
                                      GPREG(9)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                  GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                  GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                  GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                  GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(10).EQ.0.0D0) THEN
                                      GPREG(10)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                  GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                  GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                  GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                  GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  RETURN
                              ELSE
C       KEEP SEARCHING THE CATALOG
                              END IF
                          END DO
C       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
                          IF(LASCNT.EQ.8) THEN
                              OUTLYNE=
     1                        'GLASS NOT FOUND IN ANY STANDARD OPTICAL GLASS CATALOG'
                              CALL SHOWIT(1)
                              OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
                              CALL SHOWIT(1)
                              GPREG(1:5)=1.0D0
                          END IF
C
                      ELSE
C       CATALOG REQUESTED NOT YET INSTALLED.
                          OUTLYNE=
     1                    WC//' GLASS CATALOG NOT YET INSTALLED'
                          CALL SHOWIT(1)
                          OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
                          CALL SHOWIT(1)
                          GPREG(1:5)=1.0D0
                      END IF
                  END DO
              ELSE
C     FLTP NOT MULTI
              END IF
              IF(FLTP.EQ.'MULTIOHARA  '.AND.FLNAME.NE.'USER.DAT  ') THEN
C     SEARCH 5 EXISTING CATALOGS
                  LASCNT=0
                  DO COUNT=1,2
                      LASCNT=COUNT
                      IF(COUNT.EQ.1) FLNAME='OHARA.BIN'
                      IF(COUNT.EQ.2) FLNAME='OHARA-O.BIN'
                      WC='OHARA   '
                      EXIS36=.FALSE.
                      INQUIRE(FILE=trim(LIBGLA)//FLNAME,EXIST=EXIS36)
                      IF(EXIS36) THEN
C               PROCEED
                          OPEN(UNIT=36,ACCESS='DIRECT',FILE=trim(LIBGLA)//FLNAME,
     1                    FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                          READ(UNIT=36,REC=1) TOTAL
                          DO J=2,TOTAL+1
                              READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                              IF(WS.EQ.NAME.OR.WS.EQ.NUMBER) THEN
C     FOUND THE GLASS
                                  IF(COUNT.GE.1.AND.COUNT.LE.2) CALL SHOWIT(1)
C     NOW RECHECK THE WAVELENGTH RANGE
                                  NAME1=WC//'     '
                                  NAME2=WS(1:13)
                                  CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
C
C       COMPARE LAMLOW,LAMUPP,LMIN AND LMAX
                                  IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW
     1                            .AND.LMIN.NE.0.0D0) THEN
C       STOP AND GO TO CMD LEVEL AND PRINT WARNING
                                      WRITE(OUTLYNE,*)
     1                                  'GLASS DEFINITION WARNING: FOR SURFACE ',I
                                      CALL SHOWIT(1)
                                      OUTLYNE=
     1                                'THE GLASS - '//WS
                                      CALL SHOWIT(1)
                                      OUTLYNE=
     1                                'IS NOT DEFINED ACROSS THE CURRENT SYSTEM WAVELENGTH RANGE'
                                      CALL SHOWIT(1)
                                      WRITE(OUTLYNE,200) LAMLOW,LAMUPP
                                      CALL SHOWIT(1)
                                      OUTLYNE=
     1                                'ALL REFRACTIVE INDICES FOR THIS SURFACE HAVE BEEN SET TO 1.0'
                                      CALL SHOWIT(1)
                                      GPREG(1)=1.0D0
                                      GPREG(2)=1.0D0
                                      GPREG(3)=1.0D0
                                      GPREG(4)=1.0D0
                                      GPREG(5)=1.0D0
                                  ELSE
C         PROCEED
                                  END IF
C       CALCULATE INDEX THEN RETURN
                                  IF(LAM(1).EQ.0.0D0) THEN
                                      GPREG(1)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                GPREG(1)=PNSC(LAM(1),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(2).EQ.0.0D0) THEN
                                      GPREG(2)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                GPREG(2)=PNSC(LAM(2),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(3).EQ.0.0D0) THEN
                                      GPREG(3)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                GPREG(3)=PNSC(LAM(3),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(4).EQ.0.0D0) THEN
                                      GPREG(4)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                GPREG(4)=PNSC(LAM(4),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(5).EQ.0.0D0) THEN
                                      GPREG(5)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                GPREG(5)=PNSC(LAM(5),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(6).EQ.0.0D0) THEN
                                      GPREG(6)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                GPREG(6)=PNSC(LAM(6),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(7).EQ.0.0D0) THEN
                                      GPREG(7)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                GPREG(7)=PNSC(LAM(7),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(8).EQ.0.0D0) THEN
                                      GPREG(8)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                GPREG(8)=PNSC(LAM(8),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(9).EQ.0.0D0) THEN
                                      GPREG(9)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                GPREG(9)=PNSC(LAM(9),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM(10).EQ.0.0D0) THEN
                                      GPREG(10)=1.0D0
                                  ELSE
C
                                      IF(FLNAME.EQ.'HOYA.BIN')
     1                                GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'HIKARI.BIN')
     1                                GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CORNIN.BIN')
     1                                GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'CHANCE.BIN')
     1                                GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'RADHARD.BIN')
     1                                GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
C
                                      IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'SCH2000.BIN')
     1                                GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                                      IF(FLNAME.EQ.'OHARA.BIN')
     1                                GPREG(10)=PNSC(LAM(10),A0,A1,A2,A3,A4,A5)
                                  END IF
                                  RETURN
                              ELSE
C       KEEP SEARCHING THE CATALOG
                              END IF
                          END DO
C       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
                          IF(LASCNT.EQ.2) THEN
                              OUTLYNE=
     1                        'GLASS NOT FOUND IN ANY STANDARD OPTICAL GLASS CATALOG'
                              CALL SHOWIT(1)
                              OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
                              CALL SHOWIT(1)
                              GPREG(1:5)=1.0D0
                          ELSE
C     LASCNT NOT 2
                          END IF
C
                      ELSE
C       CATALOG REQUESTED NOT YET INSTALLED.
                          OUTLYNE=
     1                    WC//' GLASS CATALOG NOT YET INSTALLED'
                          CALL SHOWIT(1)
                          OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
                          CALL SHOWIT(1)
                          GPREG(1:5)=1.0D0
                      END IF
                  END DO
              ELSE
C     FLTP NOT MULTI
              END IF
C
              IF(FLNAME.EQ.'USER.DAT  ') THEN
                  EXIS36=.FALSE.
                  INQUIRE(FILE=trim(HOME)//FLNAME,EXIST=EXIS36)
                  IF(EXIS36) THEN
C               PROCEED
                      OPEN(UNIT=36,ACCESS='SEQUENTIAL',FILE=trim(HOME)//FLNAME,
     1                RECL=(NRECL*33),STATUS='UNKNOWN')
                      REWIND(UNIT=36)
 15                   READ(UNIT=36,FMT=*,END=9915) NAME,A0,A1,A2,A3,A4,A5
                      IF(WS.EQ.NAME) THEN
C       CALCULATE INDEX THEN RETURN
                          IF(LAM(1).EQ.0.0D0) THEN
                              GPREG(1)=1.0D0
                          ELSE
                              GPREG(1)=PN(LAM(1),A0,A1,A2,A3,A4,A5)
                          END IF
                          IF(LAM(2).EQ.0.0D0) THEN
                              GPREG(2)=1.0D0
                          ELSE
                              GPREG(2)=PN(LAM(2),A0,A1,A2,A3,A4,A5)
                          END IF
                          IF(LAM(3).EQ.0.0D0) THEN
                              GPREG(3)=1.0D0
                          ELSE
                              GPREG(3)=PN(LAM(3),A0,A1,A2,A3,A4,A5)
                          END IF
                          IF(LAM(4).EQ.0.0D0) THEN
                              GPREG(4)=1.0D0
                          ELSE
                              GPREG(4)=PN(LAM(4),A0,A1,A2,A3,A4,A5)
                          END IF
                          IF(LAM(5).EQ.0.0D0) THEN
                              GPREG(5)=1.0D0
                          ELSE
                              GPREG(5)=PN(LAM(5),A0,A1,A2,A3,A4,A5)
                          END IF
                          IF(LAM(6).EQ.0.0D0) THEN
                              GPREG(6)=1.0D0
                          ELSE
                              GPREG(6)=PN(LAM(6),A0,A1,A2,A3,A4,A5)
                          END IF
                          IF(LAM(7).EQ.0.0D0) THEN
                              GPREG(7)=1.0D0
                          ELSE
                              GPREG(7)=PN(LAM(7),A0,A1,A2,A3,A4,A5)
                          END IF
                          IF(LAM(8).EQ.0.0D0) THEN
                              GPREG(8)=1.0D0
                          ELSE
                              GPREG(8)=PN(LAM(8),A0,A1,A2,A3,A4,A5)
                          END IF
                          IF(LAM(9).EQ.0.0D0) THEN
                              GPREG(9)=1.0D0
                          ELSE
                              GPREG(9)=PN(LAM(9),A0,A1,A2,A3,A4,A5)
                          END IF
                          IF(LAM(10).EQ.0.0D0) THEN
                              GPREG(10)=1.0D0
                          ELSE
                              GPREG(10)=PN(LAM(10),A0,A1,A2,A3,A4,A5)
                          END IF
                          CALL CLOSE_FILE(36,1)
                          RETURN
                      ELSE
C       KEEP SEARCHING THE CATALOG
                          GO TO 15
                      END IF
 9915                 CONTINUE
                      CALL CLOSE_FILE(36,1)
C       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
                      OUTLYNE=
     1                'GLASS NOT FOUND IN THE '//WC//' CATALOG'
                      CALL SHOWIT(1)
                      OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
                      CALL SHOWIT(1)
                      GPREG(1:5)=1.0D0
C
                  ELSE
C       CATALOG REQUESTED NOT YET INSTALLED.
                      OUTLYNE=
     1                WC//' GLASS CATALOG NOT YET INSTALLED'
                      CALL SHOWIT(1)
                      OUTLYNE='REFRACTIVE INDICES SET TO 1.0'
                      CALL SHOWIT(1)
                      GPREG(1:5)=1.0D0
                  END IF
              ELSE
C       FLNAME WAS NOT USER
              END IF
C     HERE IS WHERE OTHER CATALOG CALLS GO
C
C     DO MATL
              IF(FLTP.EQ.'MATL        ') THEN
C     SECOND ENTRY IDENTIFIES THE MATERIAL TO SPCGLS
C
                  IF(WS.EQ.'GERMSC')  THEN
                      CALL SPCGL(1)
                      RETURN
                  END IF
                  IF(WS.EQ.'GERMPC')  THEN
                      CALL SPCGL(2)
                      RETURN
                  END IF
                  IF(WS.EQ.'SILICON') THEN
                      CALL SPCGL(3)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRG100')  THEN
                      CALL SPCGL(4)
                      RETURN
                  END IF
                  IF(WS.EQ.'ZNSE')    THEN
                      CALL SPCGL(5)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRTRAN4')    THEN
                      CALL SPCGL(5)
                      RETURN
                  END IF
                  IF(WS.EQ.'ZNS')     THEN
                      CALL SPCGL(6)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRTRAN2')     THEN
                      CALL SPCGL(6)
                      RETURN
                  END IF
                  IF(WS.EQ.'CLRTRAN') THEN
                      CALL SPCGL(7)
                      RETURN
                  END IF
                  IF(WS.EQ.'SILICA')  THEN
                      CALL SPCGL(8)
                      RETURN
                  END IF
                  IF(WS.EQ.'SIO2  ')  THEN
                      CALL SPCGL(8)
                      RETURN
                  END IF
                  IF(WS.EQ.'SAPPHIRE'
     1            .OR.WS.EQ.'SAPHIR') THEN
                      CALL SPCGL(9)
                      RETURN
                  END IF
                  IF(WS.EQ.'DYNASIL') THEN
                      CALL SPCGL(10)
                      RETURN
                  END IF
                  IF(WS.EQ.'AMTIR1')  THEN
                      CALL SPCGL(11)
                      RETURN
                  END IF
                  IF(WS.EQ.'AMTIR3')  THEN
                      CALL SPCGL(12)
                      RETURN
                  END IF
                  IF(WS.EQ.'AS2S3')   THEN
                      CALL SPCGL(13)
                      RETURN
                  END IF
                  IF(WS.EQ.'GAAS')    THEN
                      CALL SPCGL(14)
                      RETURN
                  END IF
                  IF(WS.EQ.'CDTE')    THEN
                      CALL SPCGL(15)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRTRAN6')    THEN
                      CALL SPCGL(15)
                      RETURN
                  END IF
                  IF(WS.EQ.'MGF2(O)'.OR.WS.EQ.'MGF2')   THEN
                      CALL SPCGL(16)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRTRAN1')   THEN
                      CALL SPCGL(16)
                      RETURN
                  END IF
                  IF(WS.EQ.'MGF2(E)')   THEN
                      CALL SPCGL(17)
                      RETURN
                  END IF
                  IF(WS.EQ.'CAF2')    THEN
                      CALL SPCGL(18)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRTRAN3')    THEN
                      CALL SPCGL(18)
                      RETURN
                  END IF
                  IF(WS.EQ.'MGO')     THEN
                      CALL SPCGL(19)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRTRAN5')     THEN
                      CALL SPCGL(19)
                      RETURN
                  END IF
                  IF(WS.EQ.'BAF2')    THEN
                      CALL SPCGL(20)
                      RETURN
                  END IF
                  IF(WS.EQ.'KBR')     THEN
                      CALL SPCGL(21)
                      RETURN
                  END IF
                  IF(WS.EQ.'CSI')     THEN
                      CALL SPCGL(22)
                      RETURN
                  END IF
                  IF(WS.EQ.'CSBR')    THEN
                      CALL SPCGL(23)
                      RETURN
                  END IF
                  IF(WS.EQ.'KRS5')    THEN
                      CALL SPCGL(24)
                      RETURN
                  END IF
                  IF(WS.EQ.'LIF')     THEN
                      CALL SPCGL(25)
                      RETURN
                  END IF
                  IF(WS.EQ.'NACL')    THEN
                      CALL SPCGL(26)
                      RETURN
                  END IF
                  IF(WS.EQ.'SIO2O')   THEN
                      CALL SPCGL(27)
                      RETURN
                  END IF
                  IF(WS.EQ.'SIO2E')   THEN
                      CALL SPCGL(28)
                      RETURN
                  END IF
                  IF(WS.EQ.'VIR3')    THEN
                      CALL SPCGL(29)
                      RETURN
                  END IF
                  IF(WS.EQ.'9754')    THEN
                      CALL SPCGL(30)
                      RETURN
                  END IF
                  IF(WS.EQ.'ALON')    THEN
                      CALL SPCGL(31)
                      RETURN
                  END IF
                  IF(WS.EQ.'SPINEL')  THEN
                      CALL SPCGL(32)
                      RETURN
                  END IF
                  IF(WS.EQ.'CALAL')   THEN
                      CALL SPCGL(33)
                      RETURN
                  END IF
                  IF(WS.EQ.'B270')    THEN
                      CALL SPCGL(34)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRG2')    THEN
                      CALL SPCGL(35)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRG3')    THEN
                      CALL SPCGL(36)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRGN6')   THEN
                      CALL SPCGL(37)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRG7')    THEN
                      CALL SPCGL(38)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRG9')    THEN
                      CALL SPCGL(39)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRG11')   THEN
                      CALL SPCGL(40)
                      RETURN
                  END IF
                  IF(WS.EQ.'IRG15')   THEN
                      CALL SPCGL(41)
                      RETURN
                  END IF
                  IF(WS.EQ.'VAC')     THEN
                      CALL SPCGL(42)
                      RETURN
                  END IF
                  IF(WS.EQ.'H2O')     THEN
                      CALL SPCGL(43)
                      RETURN
                  END IF
                  IF(WS.EQ.'SUPRASIL')  THEN
                      CALL SPCGL(44)
                      RETURN
                  END IF
                  IF(WS.EQ.'HOMOSIL')  THEN
                      CALL SPCGL(45)
                      RETURN
                  END IF
                  IF(WS.EQ.'ZNS-MS')  THEN
                      CALL SPCGL(46)
                      RETURN
                  END IF
                  IF(WS.EQ.'CEF3')  THEN
                      CALL SPCGL(47)
                      RETURN
                  END IF
                  IF(WS.EQ.'LA2O3')  THEN
                      CALL SPCGL(48)
                      RETURN
                  END IF
                  IF(WS.EQ.'THF4')  THEN
                      CALL SPCGL(49)
                      RETURN
                  END IF
                  IF(WS.EQ.'ZRO2')  THEN
                      CALL SPCGL(50)
                      RETURN
                  END IF
                  IF(WS.EQ.'DIAMOND')  THEN
                      CALL SPCGL(51)
                      RETURN
                  END IF
                  IF(WS.EQ.'YAG')  THEN
                      CALL SPCGL(52)
                      RETURN
                  END IF
                  IF(WS.EQ.'ACRYLIC')  THEN
                      CALL SPCGL(101)
                      RETURN
                  END IF
                  IF(WS.EQ.'PLYSTY')   THEN
                      CALL SPCGL(102)
                      RETURN
                  END IF
                  IF(WS.EQ.'POLYCARB') THEN
                      CALL SPCGL(103)
                      RETURN
                  END IF
                  IF(WS.EQ.'SAN')      THEN
                      CALL SPCGL(104)
                      RETURN
                  END IF
                  OUTLYNE=
     1            'GLASS NOT FOUND IN THE '//GLANAM(I,1)//' CATALOG'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     DO RUSSIAN
          IF(FLTP.EQ.'RUSSIAN     ') THEN
              CALL RUSSIAN1(LMIN,LMAX)
              RETURN
          END IF
C
C       GLASS RESOLUTIONS COMPLETED
          CALL CLOSE_FILE(36,1)
          RETURN
      END
C SUB GLSRES.FOR
      SUBROUTINE GLSRES
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GLSRES. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE GLASS CATALOG SEARCH AND REFRACTIVE INDEX
C       CALCULATION. THE VALUE BADGLS IS SET TO TRUE AND
C       AN IMMEDIATE RETURN IS MADE IF A GLASS CAN NOT BE FOUND.
C       ERROR MESSAGES SENT FROM HERE IN THAT CASE.
C
          LOGICAL BADGLS,GOGO,EXIS36,ISOK
C
          CHARACTER FLNAME*12,NAME*13,NUMBER*13,FLTP*12
C
          INTEGER I,J,TOTAL,COUNT,LASCNT,GNUMBER1,GNUMBER2
C
          EXTERNAL GNUMBER1,GNUMBER2
C
          REAL*8 LMAX,LMIN,LAMBDA,A0,A1,
     2    A2,A3,A4,A5,PN,LAM1,LAM2,LAM3,LAM4,LAM5,LAMLOW,
     3    LAMUPP,PNSC,LAM6,LAM7,LAM8,LAM9,LAM10
C
          COMMON/GLSBAD/BADGLS
C
          CHARACTER*13 NAME1,NAME2
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       NOW RESOLVE THE CALCULATION OF REFRACTIVE INDICES VIA
C       GLASS CATALOG SEARCHES.
C
C       THERE IS AN EQUATION FOR CALCULATION OF INDICES USED
C       ACROSS THE INDUSTRY. IT IS
C
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
C       SET BADGLS
          BADGLS=.FALSE.
          GOGO=.TRUE.
C
C
C       DETERMINE MINIMUM AND MAX SYSTEM WAVELENGTHS
          IF(SYSTEM1(1).NE.0.0D0) LMIN=SYSTEM1(1)
          IF(SYSTEM1(2).LT.LMIN.AND.SYSTEM1(2).NE.0.0D0) LMIN=SYSTEM1(2)
          IF(SYSTEM1(3).LT.LMIN.AND.SYSTEM1(3).NE.0.0D0) LMIN=SYSTEM1(3)
          IF(SYSTEM1(4).LT.LMIN.AND.SYSTEM1(4).NE.0.0D0) LMIN=SYSTEM1(4)
          IF(SYSTEM1(5).LT.LMIN.AND.SYSTEM1(5).NE.0.0D0) LMIN=SYSTEM1(5)
          IF(SYSTEM1(71).LT.LMIN.AND.SYSTEM1(71).NE.0.0D0)
     1    LMIN=SYSTEM1(71)
          IF(SYSTEM1(72).LT.LMIN.AND.SYSTEM1(72).NE.0.0D0)
     1    LMIN=SYSTEM1(72)
          IF(SYSTEM1(73).LT.LMIN.AND.SYSTEM1(73).NE.0.0D0)
     1    LMIN=SYSTEM1(73)
          IF(SYSTEM1(75).LT.LMIN.AND.SYSTEM1(74).NE.0.0D0)
     1    LMIN=SYSTEM1(74)
          IF(SYSTEM1(75).LT.LMIN.AND.SYSTEM1(75).NE.0.0D0)
     1    LMIN=SYSTEM1(75)
          IF(SYSTEM1(1).NE.0.0D0) LMAX=SYSTEM1(1)
          IF(SYSTEM1(2).GT.LMAX.AND.SYSTEM1(2).NE.0.0D0) LMAX=SYSTEM1(2)
          IF(SYSTEM1(3).GT.LMAX.AND.SYSTEM1(3).NE.0.0D0) LMAX=SYSTEM1(3)
          IF(SYSTEM1(4).GT.LMAX.AND.SYSTEM1(4).NE.0.0D0) LMAX=SYSTEM1(4)
          IF(SYSTEM1(5).GT.LMAX.AND.SYSTEM1(5).NE.0.0D0) LMAX=SYSTEM1(5)
          IF(SYSTEM1(71).GT.LMAX.AND.SYSTEM1(71).NE.0.0D0)
     1    LMAX=SYSTEM1(71)
          IF(SYSTEM1(72).GT.LMAX.AND.SYSTEM1(72).NE.0.0D0)
     1    LMAX=SYSTEM1(72)
          IF(SYSTEM1(73).GT.LMAX.AND.SYSTEM1(73).NE.0.0D0)
     1    LMAX=SYSTEM1(73)
          IF(SYSTEM1(75).GT.LMAX.AND.SYSTEM1(74).NE.0.0D0)
     1    LMAX=SYSTEM1(74)
          IF(SYSTEM1(75).GT.LMAX.AND.SYSTEM1(75).NE.0.0D0)
     1    LMAX=SYSTEM1(75)
C       SET LAM1 TO LAM10 TO THE 10 WAVELENGTHS
          LAM1=SYSTEM1(1)
          LAM2=SYSTEM1(2)
          LAM3=SYSTEM1(3)
          LAM4=SYSTEM1(4)
          LAM5=SYSTEM1(5)
          LAM6=SYSTEM1(71)
          LAM7=SYSTEM1(72)
          LAM8=SYSTEM1(73)
          LAM9=SYSTEM1(74)
          LAM10=SYSTEM1(75)
          DO 10 I=0,INT(SYSTEM1(20))
              IF(LNSTYP.NE.1.AND.GLANAM(I,1).NE.'MODEL') GO TO 10
C
C       IF THE SURFACE HAS A GLASS PIKUP, SKIP AND GO TO NEXT
C       SURFACE. (JUST JUMP TO 10 AND GO TO THE NEXT SURFACE.
C
              IF(PIKUP(1,I,20).NE.1.0D0) THEN
C
C       IS THE SURFACE MATERIAL A CATALOG MATERIAL?
                  IF(GLANAM(I,1).EQ.'SCHOTT'.OR.GLANAM(I,1).EQ.'CHANCE'
     1            .OR.GLANAM(I,1).EQ.'SCH2000'
     1            .OR.GLANAM(I,1).EQ.'CORNIN'.OR.GLANAM(I,1).EQ.'OHARA'
     2            .OR.GLANAM(I,1).EQ.'HOYA'.OR.GLANAM(I,1).EQ.'GLCAT'
     5            .OR.GLANAM(I,1).EQ.'USER'.OR.GLANAM(I,1).EQ.'RUSSIAN'
     6            .OR.GLANAM(I,1).EQ.'RADHARD'.OR.GLANAM(I,1).EQ.'GLA'
     7            .OR.GLANAM(I,1).EQ.'MATL'.OR.GLANAM(I,1).EQ.'HIKARI') THEN
                      IF(GLANAM(I,1).EQ.'RUSSIAN') THEN
                          ISOK=.FALSE.
                          CALL RUSSIAN2(I,LMIN,LMAX,ISOK)
                          GO TO 10
                      END IF
                      NAME1=GLANAM(I,1)
                      NAME2=GLANAM(I,2)
                      FLNAME=' '
                      CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
C
C       COMPARE LAMLOW,LAMUPP,LMIN AND LMAX
                      IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW
     1                .AND.LMIN.NE.0.0D0) THEN
C       STOP AND GO TO CMD LEVEL AND PRINT WARNING
                          BADGLS=.TRUE.
                          WRITE(OUTLYNE,*)
     1                      'GLASS DEFINITION WARNING: FOR SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'THE GLASS - '//GLANAM(I,2)
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'IS NOT DEFINED ACROSS THE CURRENT SYSTEM WAVELENGTH RANGE'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,200) LAMLOW,LAMUPP
                          CALL SHOWIT(1)
 200                      FORMAT('IT IS DEFINED ONLY FROM ',F9.4,' TO ',F9.4,
     1                    ' MICRON(S)')
                          OUTLYNE=
     1                    'ALL REFRACTIVE INDICES FOR THIS SURFACE HAVE BEEN SET TO 1.0'
                          CALL SHOWIT(1)
                          BADGLS=.TRUE.
                          ALENS(122:123,I)=0.0D0
                          ALENS(46:50,I)=1.0D0
                          ALENS(71:75,I)=1.0D0
                          GO TO 10
                      ELSE
C       PROCEED, THE GLASS IS DEFINED WITHIN THE CURRENT
C       LENS SYSTEM WAVELENGTHS
                      END IF
C       NOW LOOK UP THE GLASS IN THE CATALOG, RETRIEVE THE
C       COEFFICIENTS A0 TO A5 AND CALCULATE AND SET THE SURFACE
C       REFRACTIVE INDICES.
C
                      FLTP='            '
                      IF(GLANAM(I,1).EQ.'SCHOTT')   FLNAME='SCHOTT.BIN'
                      IF(GLANAM(I,1).EQ.'SCH2000')  FLNAME='SCH2000.BIN'
                      IF(GLANAM(I,1).EQ.'HOYA')     FLNAME='HOYA.BIN  '
                      IF(GLANAM(I,1).EQ.'HIKARI')   FLNAME='HIKARI.BIN'
                      IF(GLANAM(I,1).EQ.'CORNIN')   FLNAME='CORNIN.BIN'
                      IF(GLANAM(I,1).EQ.'CHANCE')   FLNAME='CHANCE.BIN'
                      IF(GLANAM(I,1).EQ.'RADHARD')  FLNAME='RADHARD.BIN'
                      IF(GLANAM(I,1).EQ.'USER')     FLNAME='USER.DAT  '
                      IF(GLANAM(I,1).EQ.'GLCAT')    FLTP='MULTI       '
                      IF(GLANAM(I,1).EQ.'GLA')      FLTP='MULTI       '
                      IF(GLANAM(I,1).EQ.'MATL  ')   FLTP='MATL        '
                      IF(GLANAM(I,1).EQ.'RUSSIAN')  FLTP='RUSSIAN     '
                      IF(GLANAM(I,1).EQ.'OHARA')    FLTP='MULTIOHARA  '
C
                      IF(FLTP.EQ.'            '.AND.FLNAME.NE.'USER.DAT  ') THEN
C     REGULAR GLASS CATALOG
C
                          EXIS36=.FALSE.
                          INQUIRE(FILE=trim(LIBGLA)//FLNAME,EXIST=EXIS36)
                          IF(GOGO.AND.EXIS36) THEN
C               PROCEED
                              OPEN(UNIT=36,ACCESS='DIRECT',FILE=trim(LIBGLA)//FLNAME,
     1                        FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                              READ(UNIT=36,REC=1) TOTAL
                              DO 300 J=2,TOTAL+1
                                  READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                                  IF(GLANAM(I,2).EQ.NAME.OR.GLANAM(I,2).EQ.NUMBER) THEN
                                      ALENS(122,I)=GNUMBER1(NUMBER)
                                      ALENS(123,I)=GNUMBER2(NUMBER)
C       CALCULATE INDEX THEN GO TO 10 AND CONTINUE
                                      IF(LAM1.EQ.0.0D0) THEN
                                          ALENS(46,I)=1.0D0
                                      ELSE
C
                                          IF(FLNAME.EQ.'HOYA.BIN')
     1                                      ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'HIKARI.BIN')
     1                                      ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CORNIN.BIN')
     1                                      ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CHANCE.BIN')
     1                                      ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'RADHARD.BIN')
     1                                      ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                      ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
C
                                          IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                      ALENS(46,I)=PNSC(LAM1,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'SCH2000.BIN')
     1                                      ALENS(46,I)=PNSC(LAM1,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA.BIN')
     1                                      ALENS(46,I)=PNSC(LAM1,A0,A1,A2,A3,A4,A5)
                                      END IF
                                      IF(LAM2.EQ.0.0D0) THEN
                                          ALENS(47,I)=1.0D0
                                      ELSE
C
                                          IF(FLNAME.EQ.'HOYA.BIN')
     1                                      ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'HIKARI.BIN')
     1                                      ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CORNIN.BIN')
     1                                      ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CHANCE.BIN')
     1                                      ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'RADHARD.BIN')
     1                                      ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                      ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
C
                                          IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                      ALENS(47,I)=PNSC(LAM2,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'SCH2000.BIN')
     1                                      ALENS(47,I)=PNSC(LAM2,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA.BIN')
     1                                      ALENS(47,I)=PNSC(LAM2,A0,A1,A2,A3,A4,A5)
                                      END IF
                                      IF(LAM3.EQ.0.0D0) THEN
                                          ALENS(48,I)=1.0D0
                                      ELSE
C
                                          IF(FLNAME.EQ.'HOYA.BIN')
     1                                      ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'HIKARI.BIN')
     1                                      ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CORNIN.BIN')
     1                                      ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CHANCE.BIN')
     1                                      ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'RADHARD.BIN')
     1                                      ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                      ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
C
                                          IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                      ALENS(48,I)=PNSC(LAM3,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'SCH2000.BIN')
     1                                      ALENS(48,I)=PNSC(LAM3,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA.BIN')
     1                                      ALENS(48,I)=PNSC(LAM3,A0,A1,A2,A3,A4,A5)
                                      END IF
                                      IF(LAM4.EQ.0.0D0) THEN
                                          ALENS(49,I)=1.0D0
                                      ELSE
C
                                          IF(FLNAME.EQ.'HOYA.BIN')
     1                                      ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'HIKARI.BIN')
     1                                      ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CORNIN.BIN')
     1                                      ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CHANCE.BIN')
     1                                      ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'RADHARD.BIN')
     1                                      ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                      ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
C
                                          IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                      ALENS(49,I)=PNSC(LAM4,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'SCH2000.BIN')
     1                                      ALENS(49,I)=PNSC(LAM4,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA.BIN')
     1                                      ALENS(49,I)=PNSC(LAM4,A0,A1,A2,A3,A4,A5)
                                      END IF
                                      IF(LAM5.EQ.0.0D0) THEN
                                          ALENS(50,I)=1.0D0
                                      ELSE
C
                                          IF(FLNAME.EQ.'HOYA.BIN')
     1                                      ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'HIKARI.BIN')
     1                                      ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CORNIN.BIN')
     1                                      ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CHANCE.BIN')
     1                                      ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'RADHARD.BIN')
     1                                      ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                      ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
C
                                          IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                      ALENS(50,I)=PNSC(LAM5,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'SCH2000.BIN')
     1                                      ALENS(50,I)=PNSC(LAM5,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA.BIN')
     1                                      ALENS(50,I)=PNSC(LAM5,A0,A1,A2,A3,A4,A5)
                                      END IF
                                      IF(LAM6.EQ.0.0D0) THEN
                                          ALENS(71,I)=1.0D0
                                      ELSE
C
                                          IF(FLNAME.EQ.'HOYA.BIN')
     1                                      ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'HIKARI.BIN')
     1                                      ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CORNIN.BIN')
     1                                      ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CHANCE.BIN')
     1                                      ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'RADHARD.BIN')
     1                                      ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                      ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
C
                                          IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                      ALENS(71,I)=PNSC(LAM6,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'SCH2000.BIN')
     1                                      ALENS(71,I)=PNSC(LAM6,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA.BIN')
     1                                      ALENS(71,I)=PNSC(LAM6,A0,A1,A2,A3,A4,A5)
                                      END IF
                                      IF(LAM7.EQ.0.0D0) THEN
                                          ALENS(72,I)=1.0D0
                                      ELSE
C
                                          IF(FLNAME.EQ.'HOYA.BIN')
     1                                      ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'HIKARI.BIN')
     1                                      ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CORNIN.BIN')
     1                                      ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CHANCE.BIN')
     1                                      ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'RADHARD.BIN')
     1                                      ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                      ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
C
                                          IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                      ALENS(72,I)=PNSC(LAM7,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'SCH2000.BIN')
     1                                      ALENS(72,I)=PNSC(LAM7,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA.BIN')
     1                                      ALENS(72,I)=PNSC(LAM7,A0,A1,A2,A3,A4,A5)
                                      END IF
                                      IF(LAM8.EQ.0.0D0) THEN
                                          ALENS(73,I)=1.0D0
                                      ELSE
C
                                          IF(FLNAME.EQ.'HOYA.BIN')
     1                                      ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'HIKARI.BIN')
     1                                      ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CORNIN.BIN')
     1                                      ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CHANCE.BIN')
     1                                      ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'RADHARD.BIN')
     1                                      ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                      ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
C
                                          IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                      ALENS(73,I)=PNSC(LAM8,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'SCH2000.BIN')
     1                                      ALENS(73,I)=PNSC(LAM8,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA.BIN')
     1                                      ALENS(73,I)=PNSC(LAM8,A0,A1,A2,A3,A4,A5)
                                      END IF
                                      IF(LAM9.EQ.0.0D0) THEN
                                          ALENS(74,I)=1.0D0
                                      ELSE
C
                                          IF(FLNAME.EQ.'HOYA.BIN')
     1                                      ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'HIKARI.BIN')
     1                                      ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CORNIN.BIN')
     1                                      ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CHANCE.BIN')
     1                                      ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'RADHARD.BIN')
     1                                      ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                      ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
C
                                          IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                      ALENS(74,I)=PNSC(LAM9,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'SCH2000.BIN')
     1                                      ALENS(74,I)=PNSC(LAM9,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA.BIN')
     1                                      ALENS(74,I)=PNSC(LAM9,A0,A1,A2,A3,A4,A5)
                                      END IF
                                      IF(LAM10.EQ.0.0D0) THEN
                                          ALENS(75,I)=1.0D0
                                      ELSE
C
                                          IF(FLNAME.EQ.'HOYA.BIN')
     1                                      ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'HIKARI.BIN')
     1                                      ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CORNIN.BIN')
     1                                      ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'CHANCE.BIN')
     1                                      ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'RADHARD.BIN')
     1                                      ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                      ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
C
                                          IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                      ALENS(75,I)=PNSC(LAM10,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'SCH2000.BIN')
     1                                      ALENS(75,I)=PNSC(LAM10,A0,A1,A2,A3,A4,A5)
                                          IF(FLNAME.EQ.'OHARA.BIN')
     1                                      ALENS(75,I)=PNSC(LAM10,A0,A1,A2,A3,A4,A5)
                                      END IF
                                      GO TO 10
                                  ELSE
C       KEEP SEARCHING THE CATALOG
                                  END IF
 300                          CONTINUE
C       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
                              BADGLS=.TRUE.
                              WRITE(OUTLYNE,*)
     1                          'WARNING: FOR SURFACE ',I
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)
     1                         'GLASS NOT FOUND IN THE ',GLANAM(I,1),' CATALOG'
                              CALL SHOWIT(1)
                              OUTLYNE='REFRACTIVE INDICES FOR SURFACE SET TO 1.0'
                              CALL SHOWIT(1)
                              BADGLS=.TRUE.
                              ALENS(122:123,I)=0.0D0
                              ALENS(46:50,I)=1.0D0
                              ALENS(71:75,I)=1.0D0
C
                          ELSE
C       CATALOG REQUESTED NOT YET INSTALLED.

                              WRITE(OUTLYNE,*)
     1                          'WARNING: FOR SURFACE ',I
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        GLANAM(I,1)//' GLASS CATALOG NOT YET INSTALLED'
                              CALL SHOWIT(1)
                              OUTLYNE='REFRACTIVE INDICES FOR SURFACE SET TO 1.0'
                              CALL SHOWIT(1)
                              ALENS(122:123,I)=0.0D0
                              ALENS(46:50,I)=1.0D0
                              ALENS(71:75,I)=1.0D0
                          END IF
                      ELSE
C     FLTP WAS NOT BLANK
                      END IF
C
                      IF(FLTP.EQ.'MULTI       ') THEN
C     SEARCH ALL EXISTING CATALOGS
                          LASCNT=0
                          DO COUNT=1,9
                              LASCNT=COUNT
                              IF(COUNT.EQ.1) FLNAME='SCHOTT.BIN'
                              IF(COUNT.EQ.2) FLNAME='SCH2000.BIN'
                              IF(COUNT.EQ.3) FLNAME='HOYA.BIN'
                              IF(COUNT.EQ.4) FLNAME='OHARA.BIN'
                              IF(COUNT.EQ.5) FLNAME='OHARA-O.BIN'
                              IF(COUNT.EQ.6) FLNAME='CORNIN.BIN'
                              IF(COUNT.EQ.7) FLNAME='CHANCE.BIN'
                              IF(COUNT.EQ.8) FLNAME='RADHARD.BIN'
                              IF(COUNT.EQ.9) FLNAME='HIKARI.BIN'

                              EXIS36=.FALSE.
                              INQUIRE(FILE=trim(LIBGLA)//FLNAME,EXIST=EXIS36)

                              IF(GOGO.AND.EXIS36) THEN
C               PROCEED
                                  OPEN(UNIT=36,ACCESS='DIRECT',FILE=trim(LIBGLA)//FLNAME,
     1                            FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')

                                  READ(UNIT=36,REC=1) TOTAL
                                  DO J=2,TOTAL+1
                                      READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                                      IF(GLANAM(I,2).EQ.NAME.OR.GLANAM(I,2).EQ.NUMBER) THEN
                                          ALENS(122,I)=GNUMBER1(NUMBER)
                                          ALENS(123,I)=GNUMBER2(NUMBER)
C     RESET GLCAT/GLASS TO MFG NAME
                                          IF(COUNT.EQ.1) GLANAM(I,1)='SCHOTT       '
                                          IF(COUNT.EQ.2) GLANAM(I,1)='SCH2000      '
                                          IF(COUNT.EQ.3) GLANAM(I,1)='HOYA         '
                                          IF(COUNT.EQ.4) GLANAM(I,1)='OHARA        '
                                          IF(COUNT.EQ.5) GLANAM(I,1)='OHARA        '
                                          IF(COUNT.EQ.6) GLANAM(I,1)='CORNIN       '
                                          IF(COUNT.EQ.7) GLANAM(I,1)='CHANCE       '
                                          IF(COUNT.EQ.8) GLANAM(I,1)='RADHARD      '
                                          IF(COUNT.EQ.9) GLANAM(I,1)='HIKARI       '
C
                                          IF(GLANAM(I,1).EQ.'SCHOTT'.OR.GLANAM(I,1).EQ.'CHANCE'
     1                                    .OR.GLANAM(I,1).EQ.'OHARA'.OR.GLANAM(I,1).EQ.'CORNIN'
     2                                    .OR.GLANAM(I,1).EQ.'HOYA'.OR.GLANAM(I,1).EQ.'RADHARD'
     3                                    .OR.GLANAM(I,1).EQ.'HIKARI'.OR.GLANAM(I,1).EQ.'SCH2000') THEN
C       SET CAT UPPER AND LOWER LAMBDA BOUNDS
C     NOW RECHECK THE WAVELENGTH RANGE
                                              NAME1=GLANAM(I,1)
                                              NAME2=GLANAM(I,2)
                                              CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
C
C       COMPARE LAMLOW,LAMUPP,LMIN AND LMAX
                                              IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW
     1                                        .AND.LMIN.NE.0.0D0) THEN
C       STOP AND GO TO CMD LEVEL AND PRINT WARNING
                                                  BADGLS=.TRUE.
                                                  WRITE(OUTLYNE,*)
     1                                              'GLASS DEFINITION WARNING: FOR SURFACE ',I
                                                  CALL SHOWIT(1)
                                                  OUTLYNE=
     1                                            'THE GLASS - '//GLANAM(I,2)
                                                  CALL SHOWIT(1)
                                                  OUTLYNE=
     1                                            'IS NOT DEFINED ACROSS THE CURRENT SYSTEM WAVELENGTH RANGE'
                                                  CALL SHOWIT(1)
                                                  WRITE(OUTLYNE,200) LAMLOW,LAMUPP
                                                  CALL SHOWIT(1)
                                                  OUTLYNE=
     1                                            'ALL REFRACTIVE INDICES FOR THIS SURFACE HAVE BEEN SET TO 1.0'
                                                  CALL SHOWIT(1)
                                                  BADGLS=.TRUE.
                                                  ALENS(122:123,I)=0.0D0
                                                  ALENS(46:50,I)=1.0D0
                                                  ALENS(71:75,I)=1.0D0
                                                  GO TO 10
                                              ELSE
C     PROCEED
                                              END IF
                                          END IF
C       CALCULATE INDEX THEN GO TO 10 AND CONTINUE
                                          ALENS(122,I)=GNUMBER1(NUMBER)
                                          ALENS(123,I)=GNUMBER2(NUMBER)
                                          IF(LAM1.EQ.0.0D0) THEN
                                              ALENS(46,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(46,I)=PNSC(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(46,I)=PNSC(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(46,I)=PNSC(LAM1,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM2.EQ.0.0D0) THEN
                                              ALENS(47,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(47,I)=PNSC(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(47,I)=PNSC(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(47,I)=PNSC(LAM2,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM3.EQ.0.0D0) THEN
                                              ALENS(48,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(48,I)=PNSC(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(48,I)=PNSC(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(48,I)=PNSC(LAM3,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM4.EQ.0.0D0) THEN
                                              ALENS(49,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(49,I)=PNSC(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(49,I)=PNSC(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(49,I)=PNSC(LAM4,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM5.EQ.0.0D0) THEN
                                              ALENS(50,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(50,I)=PNSC(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(50,I)=PNSC(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(50,I)=PNSC(LAM5,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM6.EQ.0.0D0) THEN
                                              ALENS(71,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(71,I)=PNSC(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(71,I)=PNSC(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(71,I)=PNSC(LAM6,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM7.EQ.0.0D0) THEN
                                              ALENS(72,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(72,I)=PNSC(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(72,I)=PNSC(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(72,I)=PNSC(LAM7,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM8.EQ.0.0D0) THEN
                                              ALENS(73,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(73,I)=PNSC(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(73,I)=PNSC(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(73,I)=PNSC(LAM8,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM9.EQ.0.0D0) THEN
                                              ALENS(74,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(74,I)=PNSC(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(74,I)=PNSC(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(74,I)=PNSC(LAM9,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM10.EQ.0.0D0) THEN
                                              ALENS(75,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(75,I)=PNSC(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(75,I)=PNSC(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(75,I)=PNSC(LAM10,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          GO TO 10
                                      ELSE
C       KEEP SEARCHING THE CATALOG
                                      END IF
                                  END DO
                              END IF
                          END DO
C     NOTHING FOUND IN THE ABOVE CATALOGS
C     HERE IS WHERE WE SEARCH USER.DAT AND THEN THE MATL CATALOG
C
C     CODE FOR USER.DAT AND MATL SEARCH GOES HERE. A FIND RESULTS IN
C     GO TO 10 ELSE FALL THROUGH TO THE ERROR CONDITION
C
C     CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
C     FELL THROUGH, GLASS NOT IN THE CATALOGS
                          EXIS36=.FALSE.
                          INQUIRE(FILE=trim(HOME)//'USER.DAT',EXIST=EXIS36)
                          IF(GOGO.AND.EXIS36) THEN
C               PROCEED
                              OPEN(UNIT=36,ACCESS='SEQUENTIAL',
     1                        FILE=trim(HOME)//'USER.DAT',
     1                        RECL=(NRECL*33),STATUS='UNKNOWN')
                              REWIND(UNIT=36)
 151                          READ(UNIT=36,FMT=*,END=9916) NAME,A0,A1,A2,A3,A4,A5
                              IF(GLANAM(I,2).EQ.NAME) THEN
C       CALCULATE INDEX THEN GO TO 10 AND CONTINUE
                                  ALENS(122,I)=GNUMBER1(NUMBER)
                                  ALENS(123,I)=GNUMBER2(NUMBER)
                                  IF(LAM1.EQ.0.0D0) THEN
                                      ALENS(46,I)=1.0D0
                                  ELSE
                                      ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM2.EQ.0.0D0) THEN
                                      ALENS(47,I)=1.0D0
                                  ELSE
                                      ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM3.EQ.0.0D0) THEN
                                      ALENS(48,I)=1.0D0
                                  ELSE
                                      ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM4.EQ.0.0D0) THEN
                                      ALENS(49,I)=1.0D0
                                  ELSE
                                      ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM5.EQ.0.0D0) THEN
                                      ALENS(50,I)=1.0D0
                                  ELSE
                                      ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM6.EQ.0.0D0) THEN
                                      ALENS(71,I)=1.0D0
                                  ELSE
                                      ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM7.EQ.0.0D0) THEN
                                      ALENS(72,I)=1.0D0
                                  ELSE
                                      ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM8.EQ.0.0D0) THEN
                                      ALENS(73,I)=1.0D0
                                  ELSE
                                      ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM9.EQ.0.0D0) THEN
                                      ALENS(74,I)=1.0D0
                                  ELSE
                                      ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM10.EQ.0.0D0) THEN
                                      ALENS(75,I)=1.0D0
                                  ELSE
                                      ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                  END IF
C                   GLANAM(I,1)='USER         '
                                  CALL CLOSE_FILE(36,1)
                                  GO TO 10
                              ELSE
                                  GO TO 151
                              END IF
 9916                         CONTINUE
                              CALL CLOSE_FILE(36,1)
                          END IF
C     MATL LIMITS
                          NAME1=GLANAM(I,1)
                          NAME2=GLANAM(I,2)
                          FLNAME=' '
                          CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
C       COMPARE LAMLOW,LAMUPP,LMIN AND LMAX
                          IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW
     1                    .AND.LMIN.NE.0.0D0) THEN
C       STOP AND GO TO CMD LEVEL AND PRINT WARNING
                              BADGLS=.TRUE.
                              WRITE(OUTLYNE,*)
     1                          'GLASS DEFINITION WARNING: FOR SURFACE ',I
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE GLASS - '//GLANAM(I,2)
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'IS NOT DEFINED ACROSS THE CURRENT SYSTEM WAVELENGTH RANGE'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,200) LAMLOW,LAMUPP
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'ALL REFRACTIVE INDICES FOR THIS SURFACE HAVE BEEN SET TO 1.0'
                              CALL SHOWIT(1)
                              BADGLS=.TRUE.
                              ALENS(122:123,I)=0.0D0
                              ALENS(46:50,I)=1.0D0
                              ALENS(71:75,I)=1.0D0
                              GO TO 10
                          ELSE
C     PROCEED
                          END IF
C
                          IF(GLANAM(I,2).EQ.'GERMSC')  THEN
                              CALL SPCGLS(I,1)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'GERMPC')  THEN
                              CALL SPCGLS(I,2)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'SILICON') THEN
                              CALL SPCGLS(I,3)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG100')  THEN
                              CALL SPCGLS(I,4)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'ZNSE')    THEN
                              CALL SPCGLS(I,5)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN4')    THEN
                              CALL SPCGLS(I,5)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'ZNS')     THEN
                              CALL SPCGLS(I,6)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN2')     THEN
                              CALL SPCGLS(I,6)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'CLRTRAN') THEN
                              CALL SPCGLS(I,7)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'SILICA')  THEN
                              CALL SPCGLS(I,8)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'SIO2  ')  THEN
                              CALL SPCGLS(I,8)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'SAPPHIRE'
     1                    .OR.GLANAM(I,2).EQ.'SAPHIR') THEN
                              CALL SPCGLS(I,9)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'DYNASIL') THEN
                              CALL SPCGLS(I,10)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'AMTIR1')  THEN
                              CALL SPCGLS(I,11)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'AMTIR3')  THEN
                              CALL SPCGLS(I,12)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'AS2S3')   THEN
                              CALL SPCGLS(I,13)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'GAAS')    THEN
                              CALL SPCGLS(I,14)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'CDTE')    THEN
                              CALL SPCGLS(I,15)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN6')    THEN
                              CALL SPCGLS(I,15)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'MGF2(O)'.OR.GLANAM(I,2).EQ.'MGF2')   THEN
                              CALL SPCGLS(I,16)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN1')   THEN
                              CALL SPCGLS(I,16)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'MGF2(E)')   THEN
                              CALL SPCGLS(I,17)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'CAF2')    THEN
                              CALL SPCGLS(I,18)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN3')    THEN
                              CALL SPCGLS(I,18)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'MGO')     THEN
                              CALL SPCGLS(I,19)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN5')     THEN
                              CALL SPCGLS(I,19)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'BAF2')    THEN
                              CALL SPCGLS(I,20)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'KBR')     THEN
                              CALL SPCGLS(I,21)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'CSI')     THEN
                              CALL SPCGLS(I,22)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'CSBR')    THEN
                              CALL SPCGLS(I,23)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'KRS5')    THEN
                              CALL SPCGLS(I,24)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'LIF')     THEN
                              CALL SPCGLS(I,25)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'NACL')    THEN
                              CALL SPCGLS(I,26)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'SIO2O')   THEN
                              CALL SPCGLS(I,27)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'SIO2E')   THEN
                              CALL SPCGLS(I,28)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'VIR3')    THEN
                              CALL SPCGLS(I,29)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'9754')    THEN
                              CALL SPCGLS(I,30)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'ALON')    THEN
                              CALL SPCGLS(I,31)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'SPINEL')  THEN
                              CALL SPCGLS(I,32)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'CALAL')   THEN
                              CALL SPCGLS(I,33)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'B270')    THEN
                              CALL SPCGLS(I,34)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG2')    THEN
                              CALL SPCGLS(I,35)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG3')    THEN
                              CALL SPCGLS(I,36)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRGN6')   THEN
                              CALL SPCGLS(I,37)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG7')    THEN
                              CALL SPCGLS(I,38)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG9')    THEN
                              CALL SPCGLS(I,39)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG11')   THEN
                              CALL SPCGLS(I,40)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG15')   THEN
                              CALL SPCGLS(I,41)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'VAC')     THEN
                              CALL SPCGLS(I,42)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'H2O')     THEN
                              CALL SPCGLS(I,43)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'SUPRASIL')  THEN
                              CALL SPCGLS(I,44)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'HOMOSIL')  THEN
                              CALL SPCGLS(I,45)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'ZNS-MS')  THEN
                              CALL SPCGLS(I,46)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'CEF3')  THEN
                              CALL SPCGLS(I,47)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'LA2O3')  THEN
                              CALL SPCGLS(I,48)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'THF4')  THEN
                              CALL SPCGLS(I,49)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'ZRO2')  THEN
                              CALL SPCGLS(I,50)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'DIAMOND')  THEN
                              CALL SPCGLS(I,51)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'YAG')  THEN
                              CALL SPCGLS(I,52)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
C
                          IF(GLANAM(I,2).EQ.'ACRYLIC')  THEN
                              CALL SPCGLS(I,101)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'PLYSTY')   THEN
                              CALL SPCGLS(I,102)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'POLYCARB') THEN
                              CALL SPCGLS(I,103)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
                          IF(GLANAM(I,2).EQ.'SAN')      THEN
                              CALL SPCGLS(I,104)
                              GLANAM(I,1)='MATL         '
                              GO TO 10
                          END IF
C     LAST TRY IS THE RUSSIAN CATALOG
                          ISOK=.FALSE.
                          CALL RUSSIAN2(I,LMIN,LMAX,ISOK)
                          IF(ISOK) THEN
                              GO TO 10
                          END IF
                          BADGLS=.TRUE.
                          WRITE(OUTLYNE,*)
     1                      'WARNING: FOR SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'GLASS NOT FOUND IN ANY STANDARD OPTICAL GLASS CATALOG'
                          CALL SHOWIT(1)
                          OUTLYNE='REFRACTIVE INDICES FOR SURFACE SET TO 1.0'
                          CALL SHOWIT(1)
                          ALENS(122:123,I)=0.0D0
                          ALENS(46:50,I)=1.0D0
                          ALENS(71:75,I)=1.0D0
                      ELSE
C     FLTP NOT MULTI
                      END IF
                      IF(FLTP.EQ.'MULTIOHARA  '.AND.FLNAME.NE.'USER.DAT    ') THEN
C     SEARCH 2 EXISTING OHARA CATALOGS
                          LASCNT=0
                          DO COUNT=1,2
                              LASCNT=COUNT
                              IF(COUNT.EQ.1) FLNAME='OHARA.BIN'
                              IF(COUNT.EQ.2) FLNAME='OHARA-O.BIN'
                              EXIS36=.FALSE.
                              INQUIRE(FILE=trim(LIBGLA)//FLNAME,EXIST=EXIS36)
                              IF(GOGO.AND.EXIS36) THEN
C               PROCEED
                                  OPEN(UNIT=36,ACCESS='DIRECT',FILE=trim(LIBGLA)//FLNAME,
     1                            FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
                                  READ(UNIT=36,REC=1) TOTAL
                                  DO J=2,TOTAL+1
                                      READ(UNIT=36,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
                                      IF(GLANAM(I,2).EQ.NAME.OR.GLANAM(I,2).EQ.NUMBER) THEN
                                          ALENS(122,I)=GNUMBER1(NUMBER)
                                          ALENS(123,I)=GNUMBER2(NUMBER)
C     RESET GLCAT/GLASS TO MFG NAME
                                          IF(COUNT.EQ.1) GLANAM(I,1)='OHARA        '
                                          IF(COUNT.EQ.2) GLANAM(I,1)='OHARA        '
                                          NAME1=GLANAM(I,1)
                                          NAME2=GLANAM(I,2)
                                          CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
C
C       COMPARE LAMLOW,LAMUPP,LMIN AND LMAX
                                          IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW
     1                                    .AND.LMIN.NE.0.0D0) THEN
C       STOP AND GO TO CMD LEVEL AND PRINT WARNING
                                              BADGLS=.TRUE.
                                              WRITE(OUTLYNE,*)
     1                                          'GLASS DEFINITION WARNING: FOR SURFACE ',I
                                              CALL SHOWIT(1)
                                              OUTLYNE=
     1                                        'THE GLASS - '//GLANAM(I,2)
                                              CALL SHOWIT(1)
                                              OUTLYNE=
     1                                        'IS NOT DEFINED ACROSS THE CURRENT SYSTEM WAVELENGTH RANGE'
                                              CALL SHOWIT(1)
                                              WRITE(OUTLYNE,200) LAMLOW,LAMUPP
                                              CALL SHOWIT(1)
                                              OUTLYNE=
     1                                        'ALL REFRACTIVE INDICES FOR THIS SURFACE HAVE BEEN SET TO 1.0'
                                              CALL SHOWIT(1)
                                              BADGLS=.TRUE.
                                              ALENS(122:123,I)=0.0D0
                                              ALENS(46:50,I)=1.0D0
                                              ALENS(71:75,I)=1.0D0
                                              GO TO 10
                                          ELSE
C     PROCEED
                                          END IF
C       CALCULATE INDEX THEN GO TO 10 AND CONTINUE
                                          ALENS(122,I)=GNUMBER1(NUMBER)
                                          ALENS(123,I)=GNUMBER2(NUMBER)
                                          IF(LAM1.EQ.0.0D0) THEN
                                              ALENS(46,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(46,I)=PNSC(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(46,I)=PNSC(LAM1,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(46,I)=PNSC(LAM1,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM2.EQ.0.0D0) THEN
                                              ALENS(47,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(47,I)=PNSC(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(47,I)=PNSC(LAM2,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(47,I)=PNSC(LAM2,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM3.EQ.0.0D0) THEN
                                              ALENS(48,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(48,I)=PNSC(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(48,I)=PNSC(LAM3,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(48,I)=PNSC(LAM3,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM4.EQ.0.0D0) THEN
                                              ALENS(49,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(49,I)=PNSC(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(49,I)=PNSC(LAM4,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(49,I)=PNSC(LAM4,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM5.EQ.0.0D0) THEN
                                              ALENS(50,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(50,I)=PNSC(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(50,I)=PNSC(LAM5,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(50,I)=PNSC(LAM5,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM6.EQ.0.0D0) THEN
                                              ALENS(71,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(71,I)=PNSC(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(71,I)=PNSC(LAM6,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(71,I)=PNSC(LAM6,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM7.EQ.0.0D0) THEN
                                              ALENS(72,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(72,I)=PNSC(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(72,I)=PNSC(LAM7,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(72,I)=PNSC(LAM7,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM8.EQ.0.0D0) THEN
                                              ALENS(73,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(73,I)=PNSC(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(73,I)=PNSC(LAM8,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(73,I)=PNSC(LAM8,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM9.EQ.0.0D0) THEN
                                              ALENS(74,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(74,I)=PNSC(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(74,I)=PNSC(LAM9,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(74,I)=PNSC(LAM9,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          IF(LAM10.EQ.0.0D0) THEN
                                              ALENS(75,I)=1.0D0
                                          ELSE
C
                                              IF(FLNAME.EQ.'HOYA.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'HIKARI.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CORNIN.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'CHANCE.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'RADHARD.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA-O.BIN')
     1                                          ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
C
                                              IF(FLNAME.EQ.'SCHOTT.BIN')
     1                                          ALENS(75,I)=PNSC(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'SCH2000.BIN')
     1                                          ALENS(75,I)=PNSC(LAM10,A0,A1,A2,A3,A4,A5)
                                              IF(FLNAME.EQ.'OHARA.BIN')
     1                                          ALENS(75,I)=PNSC(LAM10,A0,A1,A2,A3,A4,A5)
                                          END IF
                                          GO TO 10
                                      ELSE
C       KEEP SEARCHING THE CATALOG
                                      END IF
                                  END DO
C       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
                                  IF(LASCNT.EQ.2) THEN
                                      BADGLS=.TRUE.
                                      WRITE(OUTLYNE,*)
     1                                  'WARNING: FOR SURFACE ',I
                                      CALL SHOWIT(1)
                                      OUTLYNE=
     1                                'GLASS NOT FOUND IN ANY STANDARD OPTICAL GLASS CATALOG'
                                      CALL SHOWIT(1)
                                      OUTLYNE='REFRACTIVE INDICES FOR SURFACE SET TO 1.0'
                                      CALL SHOWIT(1)
                                      ALENS(122:123,I)=0.0D0
                                      ALENS(46:50,I)=1.0D0
                                      ALENS(71:75,I)=1.0D0
                                  ELSE
C     LASCNT NOT 2
                                  END IF
C
                              ELSE
C       CATALOG REQUESTED NOT YET INSTALLED.
                                  WRITE(OUTLYNE,*)
     1                              'WARNING: FOR SURFACE ',I
                                  CALL SHOWIT(1)
                                  OUTLYNE=
     1                            GLANAM(I,1)//' GLASS CATALOG NOT YET INSTALLED'
                                  CALL SHOWIT(1)
                                  OUTLYNE='REFRACTIVE INDICES FOR SURFACE SET TO 1.0'
                                  CALL SHOWIT(1)
                                  ALENS(122:123,I)=0.0D0
                                  ALENS(46:50,I)=1.0D0
                                  ALENS(71:75,I)=1.0D0
                              END IF
                          END DO
                      ELSE
C     FLTP NOT MULTI
                      END IF
C
                      IF(FLNAME.EQ.'USER.DAT  ') THEN
                          EXIS36=.FALSE.
                          INQUIRE(FILE=trim(HOME)//FLNAME,EXIST=EXIS36)
                          IF(GOGO.AND.EXIS36) THEN
C               PROCEED
                              OPEN(UNIT=36,ACCESS='SEQUENTIAL',
     1                        FILE=trim(HOME)//FLNAME,
     1                        RECL=(NRECL*33),STATUS='UNKNOWN')
                              REWIND(UNIT=36)
 15                           READ(UNIT=36,FMT=*,END=9915) NAME,A0,A1,A2,A3,A4,A5
                              ALENS(122:123,I)=0.0D0
                              IF(GLANAM(I,2).EQ.NAME) THEN
C       CALCULATE INDEX THEN GO TO 10 AND CONTINUE
                                  IF(LAM1.EQ.0.0D0) THEN
                                      ALENS(46,I)=1.0D0
                                  ELSE
                                      ALENS(46,I)=PN(LAM1,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM2.EQ.0.0D0) THEN
                                      ALENS(47,I)=1.0D0
                                  ELSE
                                      ALENS(47,I)=PN(LAM2,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM3.EQ.0.0D0) THEN
                                      ALENS(48,I)=1.0D0
                                  ELSE
                                      ALENS(48,I)=PN(LAM3,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM4.EQ.0.0D0) THEN
                                      ALENS(49,I)=1.0D0
                                  ELSE
                                      ALENS(49,I)=PN(LAM4,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM5.EQ.0.0D0) THEN
                                      ALENS(50,I)=1.0D0
                                  ELSE
                                      ALENS(50,I)=PN(LAM5,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM6.EQ.0.0D0) THEN
                                      ALENS(71,I)=1.0D0
                                  ELSE
                                      ALENS(71,I)=PN(LAM6,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM7.EQ.0.0D0) THEN
                                      ALENS(72,I)=1.0D0
                                  ELSE
                                      ALENS(72,I)=PN(LAM7,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM8.EQ.0.0D0) THEN
                                      ALENS(73,I)=1.0D0
                                  ELSE
                                      ALENS(73,I)=PN(LAM8,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM9.EQ.0.0D0) THEN
                                      ALENS(74,I)=1.0D0
                                  ELSE
                                      ALENS(74,I)=PN(LAM9,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  IF(LAM10.EQ.0.0D0) THEN
                                      ALENS(75,I)=1.0D0
                                  ELSE
                                      ALENS(75,I)=PN(LAM10,A0,A1,A2,A3,A4,A5)
                                  END IF
                                  CALL CLOSE_FILE(36,1)
                                  GO TO 10
                              ELSE
C       KEEP SEARCHING THE CATALOG
                                  GO TO 15
                              END IF
 9915                         CONTINUE
                              CALL CLOSE_FILE(36,1)
C       CAN'T FIND THE GLASS, DO WARNING AND RETURN TO CMD LEVEL
                              BADGLS=.TRUE.
                              WRITE(OUTLYNE,*)
     1                          'WARNING: FOR SURFACE ',I
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'GLASS NOT FOUND IN THE '//GLANAM(I,1)//' CATALOG'
                              CALL SHOWIT(1)
                              OUTLYNE='REFRACTIVE INDICES FOR SURFACE SET TO 1.0'
                              CALL SHOWIT(1)
                              BADGLS=.TRUE.
                              ALENS(122:123,I)=0.0D0
                              ALENS(46:50,I)=1.0D0
                              ALENS(71:75,I)=1.0D0
C
                          ELSE
C       CATALOG REQUESTED NOT YET INSTALLED.
                              WRITE(OUTLYNE,*)
     1                          'WARNING: FOR SURFACE ',I
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        GLANAM(I,1)//' GLASS CATALOG NOT YET INSTALLED'
                              CALL SHOWIT(1)
                              OUTLYNE='REFRACTIVE INDICES FOR SURFACE SET TO 1.0'
                              CALL SHOWIT(1)
                              ALENS(122:123,I)=0.0D0
                              ALENS(46:50,I)=1.0D0
                              ALENS(71:75,I)=1.0D0
                          END IF
                      ELSE
C       FLNAME WAS NOT USER
                      END IF
C     HERE IS WHERE OTHER CATALOG CALLS GO
C
C     DO MATL
                      IF(FLTP.EQ.'MATL        ') THEN
C     SECOND ENTRY IDENTIFIES THE MATERIAL TO SPCGLS
C
                          IF(GLANAM(I,2).EQ.'GERMSC')  THEN
                              CALL SPCGLS(I,1)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'GERMPC')  THEN
                              CALL SPCGLS(I,2)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'SILICON') THEN
                              CALL SPCGLS(I,3)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG100')  THEN
                              CALL SPCGLS(I,4)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'ZNSE')    THEN
                              CALL SPCGLS(I,5)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN4')    THEN
                              CALL SPCGLS(I,5)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'ZNS')     THEN
                              CALL SPCGLS(I,6)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN2')     THEN
                              CALL SPCGLS(I,6)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'CLRTRAN') THEN
                              CALL SPCGLS(I,7)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'SILICA')  THEN
                              CALL SPCGLS(I,8)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'SIO2  ')  THEN
                              CALL SPCGLS(I,8)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'SAPPHIRE'
     1                    .OR.GLANAM(I,2).EQ.'SAPHIR') THEN
                              CALL SPCGLS(I,9)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'DYNASIL') THEN
                              CALL SPCGLS(I,10)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'AMTIR1')  THEN
                              CALL SPCGLS(I,11)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'AMTIR3')  THEN
                              CALL SPCGLS(I,12)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'AS2S3')   THEN
                              CALL SPCGLS(I,13)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'GAAS')    THEN
                              CALL SPCGLS(I,14)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'CDTE')    THEN
                              CALL SPCGLS(I,15)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN6')    THEN
                              CALL SPCGLS(I,15)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'MGF2(O)'.OR.GLANAM(I,2).EQ.'MGF2')   THEN
                              CALL SPCGLS(I,16)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN1')   THEN
                              CALL SPCGLS(I,16)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'MGF2(E)')   THEN
                              CALL SPCGLS(I,17)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'CAF2')    THEN
                              CALL SPCGLS(I,18)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN3')    THEN
                              CALL SPCGLS(I,18)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'MGO')     THEN
                              CALL SPCGLS(I,19)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRTRAN5')     THEN
                              CALL SPCGLS(I,19)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'BAF2')    THEN
                              CALL SPCGLS(I,20)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'KBR')     THEN
                              CALL SPCGLS(I,21)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'CSI')     THEN
                              CALL SPCGLS(I,22)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'CSBR')    THEN
                              CALL SPCGLS(I,23)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'KRS5')    THEN
                              CALL SPCGLS(I,24)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'LIF')     THEN
                              CALL SPCGLS(I,25)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'NACL')    THEN
                              CALL SPCGLS(I,26)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'SIO2O')   THEN
                              CALL SPCGLS(I,27)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'SIO2E')   THEN
                              CALL SPCGLS(I,28)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'VIR3')    THEN
                              CALL SPCGLS(I,29)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'9754')    THEN
                              CALL SPCGLS(I,30)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'ALON')    THEN
                              CALL SPCGLS(I,31)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'SPINEL')  THEN
                              CALL SPCGLS(I,32)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'CALAL')   THEN
                              CALL SPCGLS(I,33)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'B270')    THEN
                              CALL SPCGLS(I,34)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG2')    THEN
                              CALL SPCGLS(I,35)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG3')    THEN
                              CALL SPCGLS(I,36)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRGN6')   THEN
                              CALL SPCGLS(I,37)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG7')    THEN
                              CALL SPCGLS(I,38)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG9')    THEN
                              CALL SPCGLS(I,39)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG11')   THEN
                              CALL SPCGLS(I,40)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'IRG15')   THEN
                              CALL SPCGLS(I,41)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'VAC')     THEN
                              CALL SPCGLS(I,42)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'H2O')     THEN
                              CALL SPCGLS(I,43)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'SUPRASIL')  THEN
                              CALL SPCGLS(I,44)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'HOMOSIL')  THEN
                              CALL SPCGLS(I,45)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'ZNS-MS')  THEN
                              CALL SPCGLS(I,46)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'CEF3')  THEN
                              CALL SPCGLS(I,47)
                              GLANAM(I,1)='MATL         '
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'LA2O3')  THEN
                              CALL SPCGLS(I,48)
                              GLANAM(I,1)='MATL         '
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'THF4')  THEN
                              CALL SPCGLS(I,49)
                              GLANAM(I,1)='MATL         '
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'ZRO2')  THEN
                              CALL SPCGLS(I,50)
                              GLANAM(I,1)='MATL         '
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'DIAMOND')  THEN
                              CALL SPCGLS(I,51)
                              GLANAM(I,1)='MATL         '
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'YAG')  THEN
                              CALL SPCGLS(I,52)
                              GLANAM(I,1)='MATL         '
                              GO TO 222
                          END IF
C
                          IF(GLANAM(I,2).EQ.'ACRYLIC')  THEN
                              CALL SPCGLS(I,101)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'PLYSTY')   THEN
                              CALL SPCGLS(I,102)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'POLYCARB') THEN
                              CALL SPCGLS(I,103)
                              GO TO 222
                          END IF
                          IF(GLANAM(I,2).EQ.'SAN')      THEN
                              CALL SPCGLS(I,104)
                              GO TO 222
                          END IF
                          WRITE(OUTLYNE,*)
     1                      'WARNING: FOR SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'GLASS NOT FOUND IN THE '//GLANAM(I,1)//' CATALOG'
                          CALL SHOWIT(1)
                          OUTLYNE='REFRACTIVE INDICES FOR SURFACE SET TO 1.0'
                          CALL SHOWIT(1)
                          BADGLS=.TRUE.
                          ALENS(122:123,I)=0.0D0
                          ALENS(46:50,I)=1.0D0
                          ALENS(71:75,I)=1.0D0
                      ELSE
C     NOT "MATL" CATALOG
                      END IF
 222                  CONTINUE
                  ELSE
C       NOT A CATALOG MATERIAL, GO TO NEXT SURFACE
C       THIS IS WHERE 'GLASS', 'AIR' AND 'REFL' AND 'MODEL'
C       AND 'REFLTIR', 'REFLTIRO'
C       'IDEAL' AND 'PERFECT' ARE TAKEN CARE OF.
                      IF(GLANAM(I,1).EQ.'MODEL') CALL FICTRES(I)
                  END IF
              ELSE
C       SURFACE HAD GLASS PIKUP ON IT
              END IF
 10       CONTINUE
C
C       GLASS RESOLUTIONS COMPLETED
          CALL CLOSE_FILE(36,1)
          RETURN
      END
      SUBROUTINE RUSSIAN1(LMIN,LMAX)
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          REAL*8 LAMLOW,LAMUPP,LMIN,LMAX
          CHARACTER*13 RNAME(1:113)
          INTEGER J
          LAMLOW=0.3126D0
          LAMUPP=2.3254D0
          IF(WS(1:3).EQ.'LK8'.OR.
     1    WS(1:4).EQ.'FK11'.OR.
     2    WS(1:4).EQ.'FK14'.OR.
     3    WS(1:4).EQ.'K100'.OR.
     4    WS(1:5).EQ.'STK20'.OR.
     5    WS(1:5).EQ.'TBF25'.OR.
     6    WS(1:4).EQ.'TF12'.OR.
     7    WS(1:4).EQ.'TF13') THEN
              LAMLOW=0.40466D0
              LAMUPP=2.3254D0
          END IF
          IF(WS(1:3).EQ.'ST2'.OR.
     1    WS(1:4).EQ.'ST11'.OR.
     2    WS(1:3).EQ.'LK4'.OR.
     1    WS(1:3).EQ.'KY1'.OR.
     1    WS(1:3).EQ.'KY2'.OR.
     1    WS(1:2).EQ.'KB'.OR.
     1    WS(1:2).EQ.'KI'.OR.
     7    WS(1:4).EQ.'KB-P') THEN
              LAMLOW=0.3650D0
              LAMUPP=1.9701D0
          END IF
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
          DO J=1,113
              IF(WS(1:13).EQ.RNAME(J))  THEN
C       COMPARE LAMLOW,LAMUPP,LMIN AND LMAX
                  IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW
     1            .AND.LMIN.NE.0.0D0) THEN
C       STOP AND GO TO CMD LEVEL AND PRINT WARNING
                      OUTLYNE=
     1                'THE GLASS - '//RNAME(J)
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'IS NOT DEFINED ACROSS THE CURRENT GLSWV WAVELENGTH RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,200) LAMLOW,LAMUPP
                      CALL SHOWIT(1)
 200                  FORMAT('IT IS DEFINED ONLY FROM ',F9.4,' TO ',F9.4,
     1                ' MICRON(S)')
                      OUTLYNE=
     1                'ALL REFRACTIVE INDICES FOR THIS GLASS HAVE BEEN SET TO 1.0'
                      CALL SHOWIT(1)
                      GPREG(1:5)=1.0D0
                      RETURN
                  ELSE
C       PROCEED, THE GLASS IS DEFINED WITHIN THE CURRENT
C       LENS GLSWV WAVELENGTHS
                  END IF
                  CALL RGLASS1(J)
                  RETURN
              END IF
          END DO
          RETURN
      END
      SUBROUTINE RUSSIAN2(I,LMIN,LMAX,ISOK)
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          LOGICAL ISOK
          REAL*8 LAMLOW,LAMUPP,LMIN,LMAX
          INTEGER I,J
          CHARACTER*13 RNAME(1:113)
          LOGICAL BADGLS
          COMMON/GLSBAD/BADGLS
          LAMLOW=0.3126D0
          LAMUPP=2.3254D0
          IF(GLANAM(I,2).EQ.'LK8'.OR.
     1    GLANAM(I,2).EQ.'FK11'.OR.
     2    GLANAM(I,2).EQ.'FK14'.OR.
     3    GLANAM(I,2).EQ.'K100'.OR.
     4    GLANAM(I,2).EQ.'STK20'.OR.
     5    GLANAM(I,2).EQ.'TBF25'.OR.
     6    GLANAM(I,2).EQ.'TF12'.OR.
     7    GLANAM(I,2).EQ.'TF13') THEN
              LAMLOW=0.40466D0
              LAMUPP=2.3254D0
          END IF
          IF(GLANAM(I,2).EQ.'ST2'.OR.
     1    GLANAM(I,2).EQ.'ST11'.OR.
     2    GLANAM(I,2).EQ.'LK4'.OR.
     1    GLANAM(I,2).EQ.'KY1'.OR.
     1    GLANAM(I,2).EQ.'KY2'.OR.
     1    GLANAM(I,2).EQ.'KB'.OR.
     1    GLANAM(I,2).EQ.'KI'.OR.
     7    GLANAM(I,2).EQ.'KB-P') THEN
              LAMLOW=0.3650D0
              LAMUPP=1.9701D0
          END IF
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
          DO J=1,113
              IF(GLANAM(I,2).EQ.RNAME(J))  THEN
                  IF(LMAX.GT.LAMUPP.AND.LMAX.NE.0.0D0.OR.LMIN.LT.LAMLOW
     1            .AND.LMIN.NE.0.0D0) THEN
C       STOP AND GO TO CMD LEVEL AND PRINT WARNING
                      BADGLS=.TRUE.
                      ISOK=.FALSE.
                      WRITE(OUTLYNE,*)
     1                  'GLASS DEFINITION WARNING: FOR SURFACE ',I
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'THE GLASS - '//RNAME(J)
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'IS NOT DEFINED ACROSS THE CURRENT SYSTEM WAVELENGTH RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,200) LAMLOW,LAMUPP
                      CALL SHOWIT(1)
 200                  FORMAT('IT IS DEFINED ONLY FROM ',F9.4,' TO ',F9.4,
     1                ' MICRON(S)')
                      OUTLYNE=
     1                'ALL REFRACTIVE INDICES FOR THIS SURFACE HAVE BEEN SET TO 1.0'
                      CALL SHOWIT(1)
                      ISOK=.FALSE.
                      BADGLS=.TRUE.
                      ALENS(122:123,I)=0.0D0
                      ALENS(46:50,I)=1.0D0
                      ALENS(71:75,I)=1.0D0
                      RETURN
                  ELSE
C       PROCEED, THE GLASS IS DEFINED WITHIN THE CURRENT
C       LENS SYSTEM WAVELENGTHS
                  END IF
                  CALL RGLASS2(I,J)
                  GLANAM(I,1)='RUSSIAN      '
                  ISOK=.TRUE.
                  RETURN
              END IF
          END DO
          RETURN
      END
C SUB FICTRES.FOR
      SUBROUTINE FICTRES(I)
C
          IMPLICIT NONE
C
          INTEGER I,J
C
          REAL*8 XND,DDDISS
C
          INCLUDE 'datlen.inc'
C
          REAL*8 PNSC,LAMBDA,A0,A1,A2,A3,A4,A5
     1    ,A01,A11,A21,A31,A41,A51,A02,A12,A22,A32,A42,A52,XDISP
     2    ,XVD,ND1,NF1,NC1,ND2,NF2,NC2,DISP1,DISP2,NX1,NX2
     3    ,P1,P2,V1,V2,SLOPE,NEWP
C
          PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)=
     1    DSQRT(
     1    ((A0*(LAMBDA**2))/((LAMBDA**2)-A3))+
     2    ((A1*(LAMBDA**2))/((LAMBDA**2)-A4))+
     2    ((A2*(LAMBDA**2))/((LAMBDA**2)-A5))+1.0D0)
C
C     FORMULA FOR GETTING THE NF-NC FROM Nd AND Vd
          DDDISS(XND,XVD)=(XND-1.0D0)/XVD
C
C      FOR K7
          A01=1.12735550D+0
          A11=1.24412303D-1
          A21=8.27100531D-1
          A31=7.20341707D-3
          A41=2.69835916D-3
          A51=1.00384588D+2

C      FOR F2
          A02=1.34533359D+0
          A12=2.09073176D-1
          A22=9.37357162D-1
          A32=9.97743871D-3
          A42=4.70450767D-2
          A52=1.11886764D+2
C
C                          PROCEEDURE FOLLOWS
C
C     CALCULATE THE REFRACTIVE INDEX VALUES FOR THE BASE GLASSES AT THE
C     STANDARD SCHOTT WAVELENGTHS
C
C     LOAD UP THE SURFACE INPUT VALUES OF ND, VD AND DISP
          XND=ALENS(86,I)
          XVD=ALENS(87,I)
          XDISP=DDDISS(XND,XVD)
C     CALCULATE THE ND,NC,NF AND DISP VALUES FOR THE STANDARD GLASSES
          A0=A01
          A1=A11
          A2=A21
          A3=A31
          A4=A41
          A5=A51
          LAMBDA=0.5875618D0
          ND1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          LAMBDA=0.48613D0
          NF1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          LAMBDA=0.65627D0
          NC1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          DISP1=NF1-NC1
          A0=A02
          A1=A12
          A2=A22
          A3=A32
          A4=A42
          A5=A52
          LAMBDA=0.5875618D0
          ND2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          LAMBDA=0.48613D0
          NF2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          LAMBDA=0.65627D0
          NC2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          DISP2=NF2-NC2
C     FOR EACH WAVELENGTH WHICH IS NON-ZERO WE CALCULATE THE REFRACIVE
C     INDEX
C
          DO J=1,10
C                               J=1
              IF(J.EQ.1) THEN
                  IF(SYSTEM1(1).NE.0.0D0.AND.SYSTEM1(31).NE.0.0D0) THEN
                      A0=A01
                      A1=A11
                      A2=A21
                      A3=A31
                      A4=A41
                      A5=A51
                      LAMBDA=SYSTEM1(1)
                      NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      A0=A02
                      A1=A12
                      A2=A22
                      A3=A32
                      A4=A42
                      A5=A52
                      LAMBDA=SYSTEM1(1)
                      NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      P1=(NX1-ND1)/(NF1-NC1)
                      P2=(NX2-ND2)/(NF2-NC2)
                      V1=(ND1-1.0D0)/(NF1-NC1)
                      V2=(ND2-1.0D0)/(NF2-NC2)
                      SLOPE=(P1-P2)/(V1-V2)
                      NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+ALENS(89,I)
                      ALENS(46,I)=(NEWP*XDISP)+XND
C     CALCULATE MODEL GLASS INDEX AT THIS WAVELENGTH
                  ELSE
                      ALENS(46,I)=1.0D0
                  END IF
              END IF
C                               J=2
              IF(J.EQ.2) THEN
                  IF(SYSTEM1(2).NE.0.0D0.AND.SYSTEM1(32).NE.0.0D0) THEN
C     CALCULATE CBAR
                      A0=A01
                      A1=A11
                      A2=A21
                      A3=A31
                      A4=A41
                      A5=A51
                      LAMBDA=SYSTEM1(2)
                      NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      A0=A02
                      A1=A12
                      A2=A22
                      A3=A32
                      A4=A42
                      A5=A52
                      LAMBDA=SYSTEM1(2)
                      NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      P1=(NX1-ND1)/(NF1-NC1)
                      P2=(NX2-ND2)/(NF2-NC2)
                      V1=(ND1-1.0D0)/(NF1-NC1)
                      V2=(ND2-1.0D0)/(NF2-NC2)
                      SLOPE=(P1-P2)/(V1-V2)
                      NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+ALENS(89,I)
                      ALENS(47,I)=(NEWP*XDISP)+XND
                  ELSE
                      ALENS(47,I)=1.0D0
                  END IF
              END IF
C                               J=3
              IF(J.EQ.3) THEN
                  IF(SYSTEM1(3).NE.0.0D0.AND.SYSTEM1(33).NE.0.0D0) THEN
C     CALCULATE CBAR
                      A0=A01
                      A1=A11
                      A2=A21
                      A3=A31
                      A4=A41
                      A5=A51
                      LAMBDA=SYSTEM1(3)
                      NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      A0=A02
                      A1=A12
                      A2=A22
                      A3=A32
                      A4=A42
                      A5=A52
                      LAMBDA=SYSTEM1(3)
                      NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      P1=(NX1-ND1)/(NF1-NC1)
                      P2=(NX2-ND2)/(NF2-NC2)
                      V1=(ND1-1.0D0)/(NF1-NC1)
                      V2=(ND2-1.0D0)/(NF2-NC2)
                      SLOPE=(P1-P2)/(V1-V2)
                      NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+ALENS(89,I)
                      ALENS(48,I)=(NEWP*XDISP)+XND
                  ELSE
                      ALENS(48,I)=1.0D0
                  END IF
              END IF
C                               J=4
              IF(J.EQ.4) THEN
                  IF(SYSTEM1(4).NE.0.0D0.AND.SYSTEM1(34).NE.0.0D0) THEN
C     CALCULATE CBAR
                      A0=A01
                      A1=A11
                      A2=A21
                      A3=A31
                      A4=A41
                      A5=A51
                      LAMBDA=SYSTEM1(4)
                      NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      A0=A02
                      A1=A12
                      A2=A22
                      A3=A32
                      A4=A42
                      A5=A52
                      LAMBDA=SYSTEM1(4)
                      NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      P1=(NX1-ND1)/(NF1-NC1)
                      P2=(NX2-ND2)/(NF2-NC2)
                      V1=(ND1-1.0D0)/(NF1-NC1)
                      V2=(ND2-1.0D0)/(NF2-NC2)
                      SLOPE=(P1-P2)/(V1-V2)
                      NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+ALENS(89,I)
                      ALENS(49,I)=(NEWP*XDISP)+XND
                  ELSE
                      ALENS(49,I)=1.0D0
                  END IF
              END IF
C                               J=5
              IF(J.EQ.5) THEN
                  IF(SYSTEM1(5).NE.0.0D0.AND.SYSTEM1(35).NE.0.0D0) THEN
C     CALCULATE CBAR
                      A0=A01
                      A1=A11
                      A2=A21
                      A3=A31
                      A4=A41
                      A5=A51
                      LAMBDA=SYSTEM1(5)
                      NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      A0=A02
                      A1=A12
                      A2=A22
                      A3=A32
                      A4=A42
                      A5=A52
                      LAMBDA=SYSTEM1(5)
                      NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      P1=(NX1-ND1)/(NF1-NC1)
                      P2=(NX2-ND2)/(NF2-NC2)
                      V1=(ND1-1.0D0)/(NF1-NC1)
                      V2=(ND2-1.0D0)/(NF2-NC2)
                      SLOPE=(P1-P2)/(V1-V2)
                      NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+ALENS(89,I)
                      ALENS(50,I)=(NEWP*XDISP)+XND
                  ELSE
                      ALENS(50,I)=1.0D0
                  END IF
              END IF
C                               J=6
              IF(J.EQ.6) THEN
                  IF(SYSTEM1(71).NE.0.0D0.AND.SYSTEM1(76).NE.0.0D0) THEN
C     CALCULATE CBAR
                      A0=A01
                      A1=A11
                      A2=A21
                      A3=A31
                      A4=A41
                      A5=A51
                      LAMBDA=SYSTEM1(71)
                      NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      A0=A02
                      A1=A12
                      A2=A22
                      A3=A32
                      A4=A42
                      A5=A52
                      LAMBDA=SYSTEM1(71)
                      NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      P1=(NX1-ND1)/(NF1-NC1)
                      P2=(NX2-ND2)/(NF2-NC2)
                      V1=(ND1-1.0D0)/(NF1-NC1)
                      V2=(ND2-1.0D0)/(NF2-NC2)
                      SLOPE=(P1-P2)/(V1-V2)
                      NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+ALENS(89,I)
                      ALENS(71,I)=(NEWP*XDISP)+XND
                  ELSE
                      ALENS(71,I)=1.0D0
                  END IF
              END IF
C                               J=7
              IF(J.EQ.7) THEN
                  IF(SYSTEM1(72).NE.0.0D0.AND.SYSTEM1(77).NE.0.0D0) THEN
C     CALCULATE CBAR
                      A0=A01
                      A1=A11
                      A2=A21
                      A3=A31
                      A4=A41
                      A5=A51
                      LAMBDA=SYSTEM1(72)
                      NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      A0=A02
                      A1=A12
                      A2=A22
                      A3=A32
                      A4=A42
                      A5=A52
                      LAMBDA=SYSTEM1(72)
                      NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      P1=(NX1-ND1)/(NF1-NC1)
                      P2=(NX2-ND2)/(NF2-NC2)
                      V1=(ND1-1.0D0)/(NF1-NC1)
                      V2=(ND2-1.0D0)/(NF2-NC2)
                      SLOPE=(P1-P2)/(V1-V2)
                      NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+ALENS(89,I)
                      ALENS(72,I)=(NEWP*XDISP)+XND
                  ELSE
                      ALENS(72,I)=1.0D0
                  END IF
              END IF
C                               J=8
              IF(J.EQ.8) THEN
                  IF(SYSTEM1(73).NE.0.0D0.AND.SYSTEM1(78).NE.0.0D0) THEN
C     CALCULATE CBAR
                      A0=A01
                      A1=A11
                      A2=A21
                      A3=A31
                      A4=A41
                      A5=A51
                      LAMBDA=SYSTEM1(73)
                      NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      A0=A02
                      A1=A12
                      A2=A22
                      A3=A32
                      A4=A42
                      A5=A52
                      LAMBDA=SYSTEM1(73)
                      NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      P1=(NX1-ND1)/(NF1-NC1)
                      P2=(NX2-ND2)/(NF2-NC2)
                      V1=(ND1-1.0D0)/(NF1-NC1)
                      V2=(ND2-1.0D0)/(NF2-NC2)
                      SLOPE=(P1-P2)/(V1-V2)
                      NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+ALENS(89,I)
                      ALENS(73,I)=(NEWP*XDISP)+XND
                  ELSE
                      ALENS(73,I)=1.0D0
                  END IF
              END IF
C                               J=9
              IF(J.EQ.9) THEN
                  IF(SYSTEM1(74).NE.0.0D0.AND.SYSTEM1(79).NE.0.0D0) THEN
C     CALCULATE CBAR
                      A0=A01
                      A1=A11
                      A2=A21
                      A3=A31
                      A4=A41
                      A5=A51
                      LAMBDA=SYSTEM1(74)
                      NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      A0=A02
                      A1=A12
                      A2=A22
                      A3=A32
                      A4=A42
                      A5=A52
                      LAMBDA=SYSTEM1(74)
                      NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      P1=(NX1-ND1)/(NF1-NC1)
                      P2=(NX2-ND2)/(NF2-NC2)
                      V1=(ND1-1.0D0)/(NF1-NC1)
                      V2=(ND2-1.0D0)/(NF2-NC2)
                      SLOPE=(P1-P2)/(V1-V2)
                      NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+ALENS(89,I)
                      ALENS(74,I)=(NEWP*XDISP)+XND
                  ELSE
                      ALENS(74,I)=1.0D0
                  END IF
              END IF
C                               J=10
              IF(J.EQ.10) THEN
                  IF(SYSTEM1(75).NE.0.0D0.AND.SYSTEM1(80).NE.0.0D0) THEN
C     CALCULATE CBAR
                      A0=A01
                      A1=A11
                      A2=A21
                      A3=A31
                      A4=A41
                      A5=A51
                      LAMBDA=SYSTEM1(75)
                      NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      A0=A02
                      A1=A12
                      A2=A22
                      A3=A32
                      A4=A42
                      A5=A52
                      LAMBDA=SYSTEM1(75)
                      NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
                      P1=(NX1-ND1)/(NF1-NC1)
                      P2=(NX2-ND2)/(NF2-NC2)
                      V1=(ND1-1.0D0)/(NF1-NC1)
                      V2=(ND2-1.0D0)/(NF2-NC2)
                      SLOPE=(P1-P2)/(V1-V2)
                      NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+ALENS(89,I)
                      ALENS(75,I)=(NEWP*XDISP)+XND
                  ELSE
                      ALENS(75,I)=1.0D0
                  END IF
              END IF
          END DO
C
          RETURN
      END
      FUNCTION GNUMBER1(NUMBER)
          IMPLICIT NONE
          INTEGER GNUMBER1
          CHARACTER NUMBER*13,ANUMBER*3,B*80
          ANUMBER=NUMBER(1:3)
          WRITE(B,201) ANUMBER
          READ(B,101) GNUMBER1
 101      FORMAT(I3)
 201      FORMAT(A3)
          RETURN
      END
      FUNCTION GNUMBER2(NUMBER)
          IMPLICIT NONE
          INTEGER GNUMBER2
          CHARACTER NUMBER*13,ANUMBER*3,B*80
          ANUMBER=NUMBER(4:6)
          WRITE(B,201) ANUMBER
          READ(B,101) GNUMBER2
 101      FORMAT(I3)
 201      FORMAT(A3)
          RETURN
      END
C SUB FICT.FOR
      SUBROUTINE FICT(INDEX,ND,ABBE,DPART)
C       USED TO CONVERT CAT GLASS INTO MODEL GLASS
C
          IMPLICIT NONE
C
!        INTEGER I
C
          REAL*8 XND,DDDISS
C
          REAL*8 ND,ABBE,DPART,INDEX
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          REAL*8 PNSC,LAMBDA,A0,A1,A2,A3,A4,A5
     1    ,A01,A11,A21,A31,A41,A51,A02,A12,A22,A32,A42,A52,XDISP
     2    ,XVD,ND1,NF1,NC1,ND2,NF2,NC2,DISP1,DISP2,NX1,NX2
     3    ,P1,P2,V1,V2,SLOPE,NEWP
C
          PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)=
     1    DSQRT(
     1    ((A0*(LAMBDA**2))/((LAMBDA**2)-A3))+
     2    ((A1*(LAMBDA**2))/((LAMBDA**2)-A4))+
     2    ((A2*(LAMBDA**2))/((LAMBDA**2)-A5))+1.0D0)
C
C     FORMULA FOR GETTING THE NF-NC FROM Nd AND Vd
          DDDISS(XND,XVD)=(XND-1.0D0)/XVD
C
C      FOR K7
          A01=1.12735550D+0
          A11=1.24412303D-1
          A21=8.27100531D-1
          A31=7.20341707D-3
          A41=2.69835916D-3
          A51=1.00384588D+2

C      FOR F2
          A02=1.34533359D+0
          A12=2.09073176D-1
          A22=9.37357162D-1
          A32=9.97743871D-3
          A42=4.70450767D-2
          A52=1.11886764D+2
C
C                          PROCEEDURE FOLLOWS
C
C     CALCULATE THE REFRACTIVE INDEX VALUES FOR THE BASE GLASSES AT THE
C     STANDARD SCHOTT WAVELENGTHS
C
C     LOAD UP THE SURFACE INPUT VALUES OF ND, VD AND DISP
          XND=ND
          XVD=ABBE
          XDISP=DDDISS(XND,XVD)
C     CALCULATE THE ND,NC,NF AND DISP VALUES FOR THE STANDARD GLASSES
          A0=A01
          A1=A11
          A2=A21
          A3=A31
          A4=A41
          A5=A51
          LAMBDA=0.5875618D0
          ND1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          LAMBDA=0.48613D0
          NF1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          LAMBDA=0.65627D0
          NC1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          DISP1=NF1-NC1
          A0=A02
          A1=A12
          A2=A22
          A3=A32
          A4=A42
          A5=A52
          LAMBDA=0.5875618D0
          ND2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          LAMBDA=0.48613D0
          NF2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          LAMBDA=0.65627D0
          NC2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          DISP2=NF2-NC2
C     FOR EACH WAVELENGTH WHICH IS NON-ZERO WE CALCULATE THE REFRACIVE
C     INDEX
C
          A0=A01
          A1=A11
          A2=A21
          A3=A31
          A4=A41
          A5=A51
          LAMBDA=0.5875618D0
          NX1=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          A0=A02
          A1=A12
          A2=A22
          A3=A32
          A4=A42
          A5=A52
          LAMBDA=SYSTEM1(1)
          NX2=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
          P1=(NX1-ND1)/(NF1-NC1)
          P2=(NX2-ND2)/(NF2-NC2)
          V1=(ND1-1.0D0)/(NF1-NC1)
          V2=(ND2-1.0D0)/(NF2-NC2)
          SLOPE=(P1-P2)/(V1-V2)
          NEWP=SLOPE*(XVD)-(SLOPE*V2)+P2+DPART
          INDEX=(NEWP*XDISP)+XND
          RETURN
      END
