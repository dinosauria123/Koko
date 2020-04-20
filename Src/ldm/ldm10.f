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

C       TENTH FILE FOR LENS DATABASE MANAGER FILES

      SUBROUTINE CTOA(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,
     1AM10,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL
     2,LLTYPEA,INNIA,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
C     COPIES THE CURRENT LENS TO THE ARCHIEVE LENS
C
          IMPLICIT NONE
C
          INTEGER AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9
     1    ,AM10
C
          INCLUDE 'datlen.inc'
C
C     ADDED 1/1/96 FOR LENADD BUG FIX
C       VARIABLES FOR THE ARCHIEVE LENS USED IN LENADD
          CHARACTER LICA*80,LIA*80,GLANMA*13,LLTYPEA*80,INNIA*80
     2    ,ALBL*80
C
          DIMENSION LICA(AM6),GLANMA(AM10:AM3,AM9)
     2    ,ALBL(AM10:AM3)
C
          REAL*8 SYSA,ALENA,AIPOLYX,AIPOLYY,
     1    SLVA,PIKA,FT01A,MULTCLAPA,MULTCOBSA
C
          DIMENSION SYSA(AM1),ALENA(AM2,AM10:AM3),
     1    SLVA(AM5:AM6,AM10:AM3),PIKA(AM7,AM10:AM3,AM4),FT01A(AM8,AM10:AM3)
     2    ,MULTCLAPA(1:1000,1:3,0:AM3),MULTCOBSA(1:1000,1:3,0:AM3),
     3    AIPOLYX(1:200,AM10:AM3,1:4),AIPOLYY(1:200,AM10:AM3,1:4)
C
C     DO THE MULTIPLE FIELD DEFINITIONS
C
          AFLDTYPE=CFLDTYPE
          AFLDCNT=CFLDCNT
          AFLDS=CFLDS
C
          LIA(1:80)=LI(1:80)
          LLTYPEA(1:80)=LLTYPE(1:80)
          INNIA(1:80)=INNIP(1:80)
          LICA(1:4)(1:80)=LIC(1:4)(1:80)
          SYSA(1:AM1)=SYSTEM1(1:AM1)
          ALBL(0:AM3)=LBL(0:AM3)
          FT01A(1:AM8,AM10:AM3)=FTFL01(1:AM8,AM10:AM3)
          SLVA(AM5:AM6,AM10:AM3)=SOLVE(AM5:AM6,AM10:AM3)
          GLANMA(AM10:AM3,1:AM9)=GLANAM(AM10:AM3,1:AM9)
          ALENA(1:AM2,AM10:AM3)=ALENS(1:AM2,AM10:AM3)
          AIPOLYX(1:200,AM10:AM3,1:4)=IPOLYX(1:200,AM10:AM3,1:4)
          AIPOLYY(1:200,AM10:AM3,1:4)=IPOLYY(1:200,AM10:AM3,1:4)
          PIKA(1:AM7,AM10:AM3,1:AM4)=PIKUP(1:AM7,AM10:AM3,1:AM4)
          MULTCLAPA(1:1000,1:3,0:AM3)=MULTCLAP(1:1000,1:3,0:AM3)
          MULTCOBSA(1:1000,1:3,0:AM3)=MULTCOBS(1:1000,1:3,0:AM3)
          RETURN
      END
      SUBROUTINE ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,
     1AM10,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,
     2INNIA,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
C     COPIES THE ARCHIEVE LENS TO THE CURRENT LENS
C
C      IMPLICIT NONE
C
          INTEGER AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9
     1    ,AM10
C
          INCLUDE 'datlen.inc'
C
C
C     ADDED 1/1/96 FOR LENADD BUG FIX
C       VARIABLES FOR THE ARCHIEVE LENS USED IN LENADD
          CHARACTER LICA*80,LIA*80,GLANMA*13
     2    ,ALBL*80,LLTYPEA*80,INNIA*80
C
          DIMENSION LICA(AM6),GLANMA(AM10:AM3,AM9)
     2    ,ALBL(AM10:AM3)
C
          REAL*8 SYSA,ALENA,AIPOLYX,AIPOLYY,
     1    SLVA,PIKA,FT01A,MULTCLAPA,MULTCOBSA
C
          DIMENSION SYSA(AM1),ALENA(AM2,AM10:AM3),
     1    SLVA(AM5:AM6,AM10:AM3),PIKA(AM7,AM10:AM3,AM4),FT01A(AM8,AM10:AM3)
     2    ,MULTCLAPA(1:1000,1:3,0:AM3)
     3    ,MULTCOBSA(1:1000,1:3,0:AM3)
     4    ,AIPOLYX(1:200,AM10:AM3,1:4),AIPOLYY(1:200,AM10:AM3,1:4)
C
C     DO THE MULTIPLE FIELD DEFINITIONS
C
          CFLDTYPE=AFLDTYPE
          CFLDCNT=AFLDCNT
          CFLDS=AFLDS
C
          LI(1:80)=LIA(1:80)
          LLTYPE(1:80)=LLTYPEA(1:80)
          INNI(1:80)=INNIA(1:80)
          LIC(1:4)(1:80)=LICA(1:4)(1:80)
          SYSTEM1(1:AM1)=SYSA(1:AM1)
          LBL(0:AM3)=ALBL(0:AM3)
          FTFL01(1:AM8,AM10:AM3)=FT01A(1:AM8,AM10:AM3)
          SOLVE(AM5:AM6,AM10:AM3)=SLVA(AM5:AM6,AM10:AM3)
          GLANAM(AM10:AM3,1:AM9)=GLANMA(AM10:AM3,1:AM9)
          ALENS(1:AM2,AM10:AM3)=ALENA(1:AM2,AM10:AM3)
          IPOLYX(1:200,AM10:AM3,1:4)=AIPOLYX(1:200,AM10:AM3,1:4)
          IPOLYY(1:200,AM10:AM3,1:4)=AIPOLYY(1:200,AM10:AM3,1:4)
          PIKUP(1:AM7,AM10:AM3,1:AM4)=PIKA(1:AM7,AM10:AM3,1:AM4)
          MULTCLAP(1:1000,1:3,0:AM3)=MULTCLAPA(1:1000,1:3,0:AM3)
          MULTCOBS(1:1000,1:3,0:AM3)=MULTCOBSA(1:1000,1:3,0:AM3)
          RETURN
      END
      SUBROUTINE PTOC
C     COPIES THE PERMANENT LENS TO THE CURRENT LENS
C
          IMPLICIT NONE
C
          INTEGER SYS20
C
          INCLUDE 'datlen.inc'
C
C     DO THE MULTIPLE FIELD DEFINITIONS
C
          SYS20=INT(SYSTEM1(20))
          CFLDTYPE=PFLDTYPE
          CFLDCNT=PFLDCNT
          CFLDS(1:2,1:10)=PFLDS(1:2,1:10)
C
          LI(1:80)=LIP(1:80)
          LLTYPE(1:80)=LLTYPEP(1:80)
          INNI(1:80)=INNIP(1:80)
          LIC(1:4)(1:80)=LICP(1:4)(1:80)
          SYSTEM1(1:SSIZ)=SYSP(1:SSIZ)
          LBL(0:SYS20)=PLBL(0:SYS20)
          FTFL01(1:96,0:SYS20)=FT01P(1:96,0:SYS20)
          SOLVE(1:9,0:SYS20)=SLVP(1:9,0:SYS20)
          GLANAM(0:SYS20,1:2)=GLANMP(0:SYS20,1:2)
          ALENS(1:LSIZ,0:SYS20)=ALENP(1:LSIZ,0:SYS20)
          PIKUP(1:6,0:SYS20,1:PSIZ)=PIKP(1:6,0:SYS20,1:PSIZ)
          MULTCLAP(1:1000,1:3,0:SYS20)=MULTCLAPP(1:1000,1:3,0:SYS20)
          MULTCOBS(1:1000,1:3,0:SYS20)=MULTCOBSP(1:1000,1:3,0:SYS20)
          RETURN
      END
      SUBROUTINE CTOP
C     COPIES THE CURRENT LENS TO THE PERMANENT LENS
C
          IMPLICIT NONE
C
          INTEGER SYS20
C
          INCLUDE 'datlen.inc'
C
C     DO THE MULTIPLE FIELD DEFINITIONS
C
          PFLDTYPE=CFLDTYPE
          PFLDCNT=CFLDCNT
          PFLDS=CFLDS
          SYS20=INT(SYSTEM1(20))

          LIP(1:80)=LI(1:80)
          LLTYPEP(1:80)=LLTYPE(1:80)
          INNIP(1:80)=INNI(1:80)
          LICP(1:4)(1:80)=LIC(1:4)(1:80)
          SYSP(1:SSIZ)=SYSTEM1(1:SSIZ)
          PLBL(0:SYS20)=LBL(0:SYS20)
          FT01P(1:96,0:SYS20)=FTFL01(1:96,0:SYS20)
          SLVP(1:9,0:SYS20)=SOLVE(1:9,0:SYS20)
          GLANMP(0:SYS20,1:2)=GLANAM(0:SYS20,1:2)
          ALENP(1:LSIZ,0:SYS20)=ALENS(1:LSIZ,0:SYS20)
          PIKP(1:6,0:SYS20,1:PSIZ)=PIKUP(1:6,0:SYS20,1:PSIZ)
          MULTCLAPP(1:1000,1:3,0:SYS20)=MULTCLAP(1:1000,1:3,0:SYS20)
          MULTCOBSP(1:1000,1:3,0:SYS20)=MULTCOBS(1:1000,1:3,0:SYS20)

          RETURN
      END

C SUB PRSLV.FOR
      SUBROUTINE PRSLV
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRSLV WHICH IMPLEMENTS THE SLV
C       COMMAND AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
C
          CHARACTER TYPE1*4,TYPE2*2,TYPE3*4,TYPE4*2
C
          INTEGER SLVCNT,SUR,FRACPT,INTPRT,SURFF
C
          REAL*8 PARVAL,SLVVAL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED
C
          TYPE1='    '
          TYPE2='  '
          TYPE3='    '
          TYPE4='  '
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"SLV" TAKES NO NUMERIC WORD #2 THROUGH #5 OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              S1=0
              DF1=0
              W1=0.0D0
              OUTLYNE=
     1        '"SLV" TAKES EITHER QUALIFIER OR'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'NUMERIC WORD #1 INPUT BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO SOLVES EXIST'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       W1 DESIGNATES THE SURFACE FOR WHICH THE SLV
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE SOLVE DATA FOR
C       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
C       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
C
C       IF THE DEFAULT BLANK ENTRY FOR W1 IS USED, THEN SET
C       W1 TO THE VALUE OF THE CURRENT SURFACE
C
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C
C       THIS IS SAME AS SLV,0
              S1=1
              DF1=0
              SQ=0
              WQ='        '
              W1=0.0D0
          ELSE
C       WQ NOT OB OR OBJ
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ALL') THEN
C
C       WE HAVE INVALID QUALIFIER
              OUTLYNE='INVALID QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
C       HANDEL AN INDIVIDUAL SURFACE INCLUDING "OB" AND "OBJ"
              IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
              SURFF=INT(W1)
              IF(SURFF.GT.(INT(SYSTEM1(20))).OR.SURFF.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C FIRST PRINT ALL THICKNESS SOLVES,THEN ALL CURVATURE SOLVES
C       THE ORDER OF RESOLUTION IS
C       PY,PCY,CAY,PX,PCX,CAX,APY,PIY,PUY,APCY,PICY,PUCY,COCY,
C       APX,PIX,PUX,APCX,PICX,PUCX,COCX
C
C       OUTPUT FORM IS:
C
C       SURFACE NUMBER,SOLVE TYPE,SOLVE PARAMETER(TH,CV,CVY OR CVX)
C       PARAMETER VALUE AND SOLVE DATUM(TARGET VALUE OF SOLVE)
C
C       FIRST DETERMINE IF THERE ARE SOLVES ON A SURFACE
C
              IF(ALENS(33,SURFF).EQ.0.0D0) THEN
C       NO SOLVES
                  WRITE(OUTLYNE,110) SURFF
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
C       THERE ARE SOLVES
              INTPRT=INT(ALENS(33,SURFF))
              IF(INTPRT.EQ.1.OR.INTPRT.EQ.3) THEN
C       THERE ARE YZ PLANE THICKNESS SOLVES TO PRINT
                  IF(SOLVE(6,SURFF).EQ.1.0D0) TYPE1='PY'
                  IF(SOLVE(6,SURFF).EQ.2.0D0) TYPE1='PCY'
                  IF(SOLVE(6,SURFF).EQ.3.0D0) TYPE1='CAY'
                  TYPE2='TH'
                  PARVAL=ALENS(3,SURFF)
                  SLVVAL=SOLVE(7,SURFF)
                  IF(HEADIN) WRITE(OUTLYNE,2000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,200) SURFF,TYPE1,TYPE2,PARVAL,SLVVAL
                  CALL SHOWIT(0)
              ELSE
C       NO YZ THICKNESS SOLVES
              END IF
C       NOW HOW ABOUT XZ PLANE
              FRACPT=INT(INT((ALENS(33,SURFF)*10))-
     1        (DBLE(INT(ALENS(33,SURFF)))*10.0D0))
              IF(FRACPT.EQ.1.OR.FRACPT.EQ.3) THEN
C       THERE ARE XZ PLANE THICKNESS SOLVES TO PRINT
                  IF(SOLVE(4,SURFF).EQ.4.0D0) TYPE1='PX'
                  IF(SOLVE(4,SURFF).EQ.5.0D0) TYPE1='PCX'
                  IF(SOLVE(4,SURFF).EQ.6.0D0) TYPE1='CAX'
                  TYPE2='TH'
                  PARVAL=ALENS(3,SURFF)
                  SLVVAL=SOLVE(3,SURFF)
                  IF(HEADIN) WRITE(OUTLYNE,2000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,200) SURFF,TYPE1,TYPE2,PARVAL,SLVVAL
                  CALL SHOWIT(0)
              ELSE
              END IF
C       CHECK FOR CURVATURE SOLVES
              INTPRT=INT(ALENS(33,SURFF))
              IF(INTPRT.EQ.2.OR.INTPRT.EQ.3) THEN
C       YZ PLANE CURVATURE SOLVES TO PRINT
                  IF(SOLVE(8,SURFF).EQ.1.0D0)  TYPE3='APY'
                  IF(SOLVE(8,SURFF).EQ.2.0D0)  TYPE3='PIY'
                  IF(SOLVE(8,SURFF).EQ.3.0D0)  TYPE3='PUY'
                  IF(SOLVE(8,SURFF).EQ.4.0D0)  TYPE3='APCY'
                  IF(SOLVE(8,SURFF).EQ.5.0D0)  TYPE3='PICY'
                  IF(SOLVE(8,SURFF).EQ.6.0D0)  TYPE3='PUCY'
                  IF(SOLVE(8,SURFF).EQ.7.0D0)  TYPE3='COCY'
                  TYPE4='CV'
                  PARVAL=ALENS(1,SURFF)
                  SLVVAL=SOLVE(9,SURFF)
                  IF(HEADIN) WRITE(OUTLYNE,2000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,200) SURFF,TYPE3,TYPE4,PARVAL,SLVVAL
                  CALL SHOWIT(0)
              ELSE
C       NO YZ PLANE CURVATURE SOLVES TO PRINT
              END IF
C       CHECK FOR XZ PLANE CURVATURE SOLVE
              FRACPT=INT(INT((ALENS(33,SURFF)*10))-
     1        (DBLE(INT(ALENS(33,SURFF)))*10.0D0))
              IF(FRACPT.EQ.2.OR.FRACPT.EQ.3) THEN
C       XZ PLANE CURVATURE SOLVES TO PRINT
                  IF(SOLVE(2,SURFF).EQ.8.0D0)  TYPE3='APX'
                  IF(SOLVE(2,SURFF).EQ.9.0D0)  TYPE3='PIX'
                  IF(SOLVE(2,SURFF).EQ.10.0D0)  TYPE3='PUX'
                  IF(SOLVE(2,SURFF).EQ.11.0D0)  TYPE3='APCX'
                  IF(SOLVE(2,SURFF).EQ.12.0D0)  TYPE3='PICX'
                  IF(SOLVE(2,SURFF).EQ.13.0D0)  TYPE3='PUCX'
                  IF(SOLVE(2,SURFF).EQ.14.0D0)  TYPE3='COCX'
                  TYPE4='CV'
                  PARVAL=ALENS(1,SURFF)
                  SLVVAL=SOLVE(1,SURFF)
                  IF(HEADIN) WRITE(OUTLYNE,2000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,200) SURFF,TYPE3,TYPE4,PARVAL,SLVVAL
                  CALL SHOWIT(0)
              ELSE
C       NO XZ PLANE CURVATURE SOLVES TO PRINT
              END IF
          ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
              SLVCNT=0
C       PRINT HEADING DATA
C
C       SET THE SOLVE COUNTER. IT IS USED TO DETERMINE
C       THE CASE OF NO SOLVES IN A LENS
              DO 16 SUR=0,INT(SYSTEM1(20))
                  IF(ALENS(33,SUR).NE.0.0D0) THEN
                      SLVCNT=SLVCNT+1
                  ELSE
                  END IF
 16           CONTINUE
              IF(SLVCNT.EQ.0) THEN
                  WRITE(OUTLYNE,100)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C       PROCEED WITH PRINTOUT
              END IF
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)
              CALL SHOWIT(0)
              DO 15 SUR=0,INT(SYSTEM1(20))
C
C       CHECK FOR A SOLVE ON CURRENT SURFACE
                  IF(ALENS(33,SUR).EQ.0.0D0) THEN
C       NO SOLVES, GO TO NEXT SURFACE BY A (GO TO 15)
                      GO TO 15
                  ELSE
                  END IF
C       THERE ARE SOLVES
                  INTPRT=INT(ALENS(33,SUR))
                  IF(INTPRT.EQ.1.OR.INTPRT.EQ.3) THEN
C       THERE ARE YZ PLANE THICKNESS SOLVES TO PRINT
                      IF(SOLVE(6,SUR).EQ.1.0D0) TYPE1='PY'
                      IF(SOLVE(6,SUR).EQ.2.0D0) TYPE1='PCY'
                      IF(SOLVE(6,SUR).EQ.3.0D0) TYPE1='CAY'
                      TYPE2='TH'
                      PARVAL=ALENS(3,SUR)
                      SLVVAL=SOLVE(7,SUR)
                      WRITE(OUTLYNE,200) SUR,TYPE1,TYPE2,PARVAL,SLVVAL
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  FRACPT=INT(INT((ALENS(33,SUR)*10))-
     1            (DBLE(INT(ALENS(33,SUR)))*10.0D0))
                  IF(FRACPT.EQ.1.OR.FRACPT.EQ.3) THEN
C       THERE ARE XZ PLANE THICKNESS SOLVES TO PRINT
                      IF(SOLVE(4,SUR).EQ.4.0D0) TYPE1='PX'
                      IF(SOLVE(4,SUR).EQ.5.0D0) TYPE1='PCX'
                      IF(SOLVE(4,SUR).EQ.6.0D0) TYPE1='CAX'
                      TYPE2='TH'
                      PARVAL=ALENS(3,SUR)
                      SLVVAL=SOLVE(3,SUR)
                      WRITE(OUTLYNE,200) SUR,TYPE1,TYPE2,PARVAL,SLVVAL
                      CALL SHOWIT(0)
                  ELSE
                  END IF
C       CHECK FOR CURVATURE SOLVES
                  INTPRT=(INT(ALENS(33,SUR)))
                  IF(INTPRT.EQ.2.OR.INTPRT.EQ.3) THEN
C       YZ PLANE CURVATURE SOLVES TO PRINT
                      IF(SOLVE(8,SUR).EQ.1.0D0)  TYPE3='APY'
                      IF(SOLVE(8,SUR).EQ.2.0D0)  TYPE3='PIY'
                      IF(SOLVE(8,SUR).EQ.3.0D0)  TYPE3='PUY'
                      IF(SOLVE(8,SUR).EQ.4.0D0)  TYPE3='APCY'
                      IF(SOLVE(8,SUR).EQ.5.0D0)  TYPE3='PICY'
                      IF(SOLVE(8,SUR).EQ.6.0D0)  TYPE3='PUCY'
                      IF(SOLVE(8,SUR).EQ.7.0D0)  TYPE3='COCY'
                      TYPE4='CV'
                      PARVAL=ALENS(1,SUR)
                      SLVVAL=SOLVE(9,SUR)
                      WRITE(OUTLYNE,200) SUR,TYPE3,TYPE4,PARVAL,SLVVAL
                      CALL SHOWIT(0)
                  ELSE
C       NO YZ PLANE CURVATURE SOLVES TO PRINT
                  END IF
C       CHECK FOR XZ PLANE CURVATURE SOLVE
                  FRACPT=INT(INT((ALENS(33,SUR)*10))-
     1            (DBLE(INT(ALENS(33,SUR)))*10.0D0))
                  IF(FRACPT.EQ.2.OR.FRACPT.EQ.3) THEN
C       XZ PLANE CURVATURE SOLVES TO PRINT
                      IF(SOLVE(2,SUR).EQ.8.0D0)  TYPE3='APX'
                      IF(SOLVE(2,SUR).EQ.9.0D0)  TYPE3='PIX'
                      IF(SOLVE(2,SUR).EQ.10.0D0)  TYPE3='PUX'
                      IF(SOLVE(2,SUR).EQ.11.0D0)  TYPE3='APCX'
                      IF(SOLVE(2,SUR).EQ.12.0D0)  TYPE3='PICX'
                      IF(SOLVE(2,SUR).EQ.13.0D0)  TYPE3='PUCX'
                      IF(SOLVE(2,SUR).EQ.14.0D0)  TYPE3='COCX'
                      TYPE4='CV'
                      PARVAL=ALENS(1,SUR)
                      SLVVAL=SOLVE(1,SUR)
                      WRITE(OUTLYNE,200) SUR,TYPE3,TYPE4,PARVAL,SLVVAL
                      CALL SHOWIT(0)
                  ELSE
C       NO XZ PLANE CURVATURE SOLVES TO PRINT
                  END IF
C
 15           CONTINUE
          END IF
C
 200      FORMAT(I3,2X,A4,6X,A2,8X,G12.5,4X,G12.5)
C
 110      FORMAT('SURF',1X,I3,1X,
     1    ':NO SOLVE DATA')
 100      FORMAT('NO SOLVE DATA')
 1000     FORMAT('SOLVES')
 2000     FORMAT('SURF',1X,'TYPE',3X,'PARAMETER',7X,'VALUE',11X,
     1    'SLV DATUM')
 1500     FORMAT(1X)
          RETURN
      END



C SUB PRES.FOR
      SUBROUTINE PRES
C
          IMPLICIT NONE
C
          INTEGER I
C
          LOGICAL GONOGO
C
          REAL*8 A,B,LM1,DUMA,DUMB,DUML,LA1,LA2,LA3,LA4,LA5,DSGN
     1    ,LA6,LA7,LA8,LA9,LA10
C
          EXTERNAL DSGN
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       LM1
          LM1(DUMA,DUMB,DUML)=DUMA*(1.0D0+(DUMB/(DUML**2)))
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"PRES" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'NO ADDITIONAL INFORMATION'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"PRES" TAKES NO NUMERIC WORD #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(S5.EQ.1.AND.WQ.EQ.'GAS'.OR.S4.EQ.1.AND.WQ.EQ.'GAS') THEN
              WRITE(OUTLYNE,*)
     1        '"PRES GAS" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"PRES" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WQ.NE.'GAS'.AND.WQ.NE.'AIR'.AND.WQ.NE.'NITROGEN'
     1    .AND.WQ.NE.'HELIUM'.AND.WQ.NE.'HYDROGEN'.AND.WQ.NE.'ARGON'
     2    .AND.WQ.NE.'OXYGEN'.AND.WQ.NE.'METHANE'.AND.WQ.NE.'ETHANE') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER USED WITH "PRES"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) W1=0.0D0
          IF(DF2.EQ.1) W2=SYSTEM1(20)
          IF(DF3.EQ.1) THEN
              WRITE(OUTLYNE,*)'"PRES" REQUIRES EXPLICIT NUMERIC WORD #3'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF4.EQ.1.AND.WQ.EQ.'GAS') THEN
              WRITE(OUTLYNE,*)'"PRES GAS" REQUIRES EXPLICIT NUMERIC WORD #4'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0) THEN
              WRITE(OUTLYNE,*)'STARTING SURFACE NUMBER MUST BE 0 OR GREATER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.GT.SYSTEM1(20)) THEN
              WRITE(OUTLYNE,*)
     1        'ENDING SURFACE NUMBER MUST BE LESS THAN',INT(SYSTEM1(20)+1.0D0)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LT.W1) THEN
              WRITE(OUTLYNE,*)
     1        'ERROR:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'ENDING SURFACE NUMBER LESS THAN STARTING SURFACE NUMBER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C DO THE PRES OPERATION
          IF(WQ.EQ.'GAS') THEN
              IF(INT(W1).EQ.INT(W2)) THEN
                  I=INT(W1)
                  CALL CHKGLSP(GONOGO,I)
                  IF(.NOT.GONOGO) THEN
                  ELSE
                      RETURN
                  END IF
                  IF(GLANAM(2,I).EQ.'REFLTIR      '.OR.
     1            GLANAM(2,I).EQ.'REFLTIRO     '.OR.
     2            GLANAM(2,I).EQ.'REFL         ') THEN
                      WRITE(OUTLYNE,*)
     1                '"PRES GAS" DID NOT MODIFY "REFL" SURFACE # ',I
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(GLANAM(2,I).EQ.'PERFECT      ') THEN
                      WRITE(OUTLYNE,*)
     1                '"PRES GAS" DID NOT MODIFY "PERFECT" SURFACE # ',I
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(GLANAM(2,I).EQ.'IDEAL        ') THEN
                      WRITE(OUTLYNE,*)
     1                '"PRES GAS" DID NOT MODIFY "IDEAL" SURFACE # ',I
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(GLANAM(2,I).NE.'REFL         '.AND.
     1            GLANAM(2,I).NE.'PERFECT      '.AND.
     1            GLANAM(2,I).NE.'REFLTIR      '.AND.
     1            GLANAM(2,I).NE.'REFLTIRO     '.AND.
     1            GLANAM(2,I).NE.'IDEAL        ') THEN
                      IF(DABS(ALENS(46,I)).LE.1.1D0.AND.DABS(ALENS(47,I)).LE.1.1D0
     1                .OR.DABS(ALENS(48,I)).LE.1.1D0.AND.DABS(ALENS(49,I)).LE.1.1D0
     2                .AND.DABS(ALENS(50,I)).LE.1.1D0) THEN
                          IF(SYSTEM1(1).NE.0.0D0)
     1                    ALENS(46,I)=ALENS(46,I)+(DSGN(ALENS(46,I))*W3*W4)
                          IF(SYSTEM1(2).NE.0.0D0)
     1                    ALENS(47,I)=ALENS(47,I)+(DSGN(ALENS(47,I))*W3*W4)
                          IF(SYSTEM1(3).NE.0.0D0)
     1                    ALENS(48,I)=ALENS(48,I)+(DSGN(ALENS(48,I))*W3*W4)
                          IF(SYSTEM1(4).NE.0.0D0)
     1                    ALENS(49,I)=ALENS(49,I)+(DSGN(ALENS(49,I))*W3*W4)
                          IF(SYSTEM1(5).NE.0.0D0)
     1                    ALENS(50,I)=ALENS(50,I)+(DSGN(ALENS(50,I))*W3*W4)
                          GLANAM(1,I)='GLASS'
                          GLANAM(2,I)='GAS'
                      END IF
                      IF(DABS(ALENS(71,I)).LE.1.1D0.AND.DABS(ALENS(72,I)).LE.1.1D0
     1                .OR.DABS(ALENS(73,I)).LE.1.1D0.AND.DABS(ALENS(74,I)).LE.1.1D0
     2                .AND.DABS(ALENS(75,I)).LE.1.1D0) THEN
                          IF(SYSTEM1(71).NE.0.0D0)
     1                    ALENS(71,I)=ALENS(71,I)+(DSGN(ALENS(71,I))*W3*W4)
                          IF(SYSTEM1(72).NE.0.0D0)
     1                    ALENS(72,I)=ALENS(72,I)+(DSGN(ALENS(72,I))*W3*W4)
                          IF(SYSTEM1(73).NE.0.0D0)
     1                    ALENS(73,I)=ALENS(73,I)+(DSGN(ALENS(73,I))*W3*W4)
                          IF(SYSTEM1(74).NE.0.0D0)
     1                    ALENS(74,I)=ALENS(74,I)+(DSGN(ALENS(74,I))*W3*W4)
                          IF(SYSTEM1(75).NE.0.0D0)
     1                    ALENS(75,I)=ALENS(75,I)+(DSGN(ALENS(75,I))*W3*W4)
                          GLANAM(1,I)='GLASS'
                          GLANAM(2,I)='GAS'
                      END IF
                  END IF
              ELSE
                  DO I=INT(W1),INT(W2)
                      CALL CHKGLSP(GONOGO,I)
                      IF(.NOT.GONOGO) THEN
                      ELSE
                          IF(GLANAM(2,I).EQ.'REFL         '.OR.
     1                    GLANAM(2,I).EQ.'REFLTIRO     '.OR.
     2                    GLANAM(2,I).EQ.'REFLTIR      ') THEN
                              WRITE(OUTLYNE,*)
     1                        '"PRES GAS" DID NOT MODIFY "REFL" SURFACE # ',I
                              CALL SHOWIT(1)
                          END IF
                          IF(GLANAM(2,I).EQ.'PERFECT      ') THEN
                              WRITE(OUTLYNE,*)
     1                        '"PRES GAS" DID NOT MODIFY "PERFECT" SURFACE # ',I
                              CALL SHOWIT(1)
                          END IF
                          IF(GLANAM(2,I).EQ.'IDEAL        ') THEN
                              WRITE(OUTLYNE,*)
     1                        '"PRES GAS" DID NOT MODIFY "IDEAL" SURFACE # ',I
                              CALL SHOWIT(1)
                              RETURN
                          END IF
                          IF(GLANAM(2,I).NE.'REFL         '.AND.
     1                    GLANAM(2,I).NE.'PERFECT      '.AND.
     1                    GLANAM(2,I).NE.'REFLTIR      '.AND.
     1                    GLANAM(2,I).NE.'REFLTIRO     '.AND.
     1                    GLANAM(2,I).NE.'IDEAL        ') THEN
                              IF(DABS(ALENS(46,I)).LE.1.1D0.AND.DABS(ALENS(47,I)).LE.1.1D0
     1                        .OR.DABS(ALENS(48,I)).LE.1.1D0.AND.DABS(ALENS(49,I)).LE.1.1D0
     2                        .AND.DABS(ALENS(50,I)).LE.1.1D0) THEN
                                  IF(SYSTEM1(1).NE.0.0D0)
     1                            ALENS(46,I)=ALENS(46,I)+(DSGN(ALENS(46,I))*W3*W4)
                                  IF(SYSTEM1(2).NE.0.0D0)
     1                            ALENS(47,I)=ALENS(47,I)+(DSGN(ALENS(47,I))*W3*W4)
                                  IF(SYSTEM1(3).NE.0.0D0)
     1                            ALENS(48,I)=ALENS(48,I)+(DSGN(ALENS(48,I))*W3*W4)
                                  IF(SYSTEM1(4).NE.0.0D0)
     1                            ALENS(49,I)=ALENS(49,I)+(DSGN(ALENS(49,I))*W3*W4)
                                  IF(SYSTEM1(5).NE.0.0D0)
     1                            ALENS(50,I)=ALENS(50,I)+(DSGN(ALENS(50,I))*W3*W4)
                                  GLANAM(1,I)='GLASS'
                                  GLANAM(2,I)='GAS'
                              END IF
                              IF(DABS(ALENS(71,I)).LE.1.1D0.AND.DABS(ALENS(72,I)).LE.1.1D0
     1                        .OR.DABS(ALENS(73,I)).LE.1.1D0.AND.DABS(ALENS(74,I)).LE.1.1D0
     2                        .AND.DABS(ALENS(75,I)).LE.1.1D0) THEN
                                  IF(SYSTEM1(71).NE.0.0D0)
     1                            ALENS(71,I)=ALENS(71,I)+(DSGN(ALENS(71,I))*W3*W4)
                                  IF(SYSTEM1(72).NE.0.0D0)
     1                            ALENS(72,I)=ALENS(72,I)+(DSGN(ALENS(72,I))*W3*W4)
                                  IF(SYSTEM1(73).NE.0.0D0)
     1                            ALENS(73,I)=ALENS(73,I)+(DSGN(ALENS(73,I))*W3*W4)
                                  IF(SYSTEM1(74).NE.0.0D0)
     1                            ALENS(74,I)=ALENS(74,I)+(DSGN(ALENS(74,I))*W3*W4)
                                  IF(SYSTEM1(75).NE.0.0D0)
     1                            ALENS(75,I)=ALENS(75,I)+(DSGN(ALENS(75,I))*W3*W4)
                                  GLANAM(1,I)='GLASS'
                                  GLANAM(2,I)='GAS'
                              END IF
                          END IF
                      END IF
                  END DO
              END IF
              F1=0
              F6=1
              F22=1
              LNSTYP=1
              CALL LNSEOS
          END IF
          IF(WQ.EQ.'ARGON') THEN
              A=27.92D-5
              B=5.6D-5
          END IF
          IF(WQ.EQ.'NITROGEN') THEN
              A=29.19D-5
              B=7.7D-5
          END IF
          IF(WQ.EQ.'HELIUM') THEN
              A=3.48D-5
              B=2.3D-5
          END IF
          IF(WQ.EQ.'HYDROGEN') THEN
              A=13.6D-5
              B=7.7D-5
          END IF
          IF(WQ.EQ.'OXYGEN') THEN
              A=26.63D-5
              B=5.07D-5
          END IF
          IF(WQ.EQ.'AIR') THEN
              A=28.79D-5
              B=5.67D-5
          END IF
          IF(WQ.EQ.'ETHANE') THEN
              A=73.65D-5
              B=9.08D-5
          END IF
          IF(WQ.EQ.'METHANE') THEN
              A=42.6D-5
              B=14.41D-5
          END IF
C     LA1 TO LA5 ARE THE BASE N-1 VALUES AT THE 5 WAVELENGTHS AT
C     760 MM OF MERCURY. IF THE PRESSURE GOES UP, THE DENSITY GOES UP
C     IN LINEAR PROPORTION AND THE N-1 VALUE SCALES LINEARLY
C
          IF(INT(W1).EQ.INT(W2)) THEN
              I=INT(W1)
              CALL CHKGLSP(GONOGO,I)
              IF(.NOT.GONOGO) THEN
              ELSE
                  RETURN
              END IF
C
              IF(GLANAM(2,I).EQ.'REFLTIRO     '.OR.
     1        GLANAM(2,I).EQ.'REFLTIR      '.OR.
     2        GLANAM(2,I).EQ.'REFL         ') THEN
                  WRITE(OUTLYNE,*)
     1            '"PRES ',WQ,'" DID NOT MODIFY "REFL" SURFACE # ',I
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(GLANAM(2,I).EQ.'PERFECT      ') THEN
                  WRITE(OUTLYNE,*)
     1            '"PRES ',WQ,'" DID NOT MODIFY "PERFECT" SURFACE # ',I
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(GLANAM(2,I).EQ.'IDEAL        ') THEN
                  WRITE(OUTLYNE,*)
     1            '"PRES GAS" DID NOT MODIFY "IDEAL" SURFACE # ',I
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(GLANAM(2,I).NE.'REFL         '.AND.
     1        GLANAM(2,I).NE.'PERFECT      '.AND.
     1        GLANAM(2,I).NE.'REFLTIR      '.AND.
     1        GLANAM(2,I).NE.'REFLTIRO     '.AND.
     1        GLANAM(2,I).NE.'IDEAL        ') THEN
                  IF(DABS(ALENS(46,I)).LE.1.1D0.AND.DABS(ALENS(47,I)).LE.1.1D0
     1            .OR.DABS(ALENS(48,I)).LE.1.1D0.AND.DABS(ALENS(49,I)).LE.1.1D0
     2            .AND.DABS(ALENS(50,I)).LE.1.1D0.AND.
     1            DABS(ALENS(71,I)).LE.1.1D0.AND.DABS(ALENS(72,I)).LE.1.1D0
     1            .OR.DABS(ALENS(73,I)).LE.1.1D0.AND.DABS(ALENS(74,I)).LE.1.1D0
     2            .AND.DABS(ALENS(75,I)).LE.1.1D0) THEN
                      IF(SYSTEM1(1).NE.0.0D0) THEN
                          LA1=LM1(A,B,SYSTEM1(1))
                          ALENS(46,I)=ALENS(46,I)+(((LA1*((W3+760.00)/760.0D0))-LA1)
     1                    *DSGN(ALENS(46,I)))
                      END IF
                      IF(SYSTEM1(2).NE.0.0D0) THEN
                          LA2=LM1(A,B,SYSTEM1(2))
                          ALENS(47,I)=ALENS(47,I)+(((LA2*((W3+760.00)/760.0D0))-LA2)
     1                    *DSGN(ALENS(47,I)))
                      END IF
                      IF(SYSTEM1(3).NE.0.0D0) THEN
                          LA3=LM1(A,B,SYSTEM1(3))
                          ALENS(48,I)=ALENS(48,I)+(((LA3*((W3+760.00)/760.0D0))-LA3)
     1                    *DSGN(ALENS(48,I)))
                      END IF
                      IF(SYSTEM1(4).NE.0.0D0) THEN
                          LA4=LM1(A,B,SYSTEM1(4))
                          ALENS(49,I)=ALENS(49,I)+(((LA4*((W3+760.00)/760.0D0))-LA4)
     1                    *DSGN(ALENS(49,I)))
                      END IF
                      IF(SYSTEM1(5).NE.0.0D0) THEN
                          LA5=LM1(A,B,SYSTEM1(5))
                          ALENS(50,I)=ALENS(50,I)+(((LA5*((W3+760.00)/760.0D0))-LA5)
     1                    *DSGN(ALENS(50,I)))
                      END IF
                      IF(SYSTEM1(71).NE.0.0D0) THEN
                          LA6=LM1(A,B,SYSTEM1(71))
                          ALENS(71,I)=ALENS(71,I)+(((LA6*((W3+760.00)/760.0D0))-LA6)
     1                    *DSGN(ALENS(71,I)))
                      END IF
                      IF(SYSTEM1(72).NE.0.0D0) THEN
                          LA7=LM1(A,B,SYSTEM1(72))
                          ALENS(72,I)=ALENS(72,I)+(((LA7*((W3+760.00)/760.0D0))-LA7)
     1                    *DSGN(ALENS(72,I)))
                      END IF
                      IF(SYSTEM1(73).NE.0.0D0) THEN
                          LA8=LM1(A,B,SYSTEM1(73))
                          ALENS(73,I)=ALENS(73,I)+(((LA8*((W3+760.00)/760.0D0))-LA8)
     1                    *DSGN(ALENS(73,I)))
                      END IF
                      IF(SYSTEM1(74).NE.0.0D0) THEN
                          LA9=LM1(A,B,SYSTEM1(74))
                          ALENS(74,I)=ALENS(74,I)+(((LA9*((W3+760.00)/760.0D0))-LA9)
     1                    *DSGN(ALENS(74,I)))
                      END IF
                      IF(SYSTEM1(75).NE.0.0D0) THEN
                          LA10=LM1(A,B,SYSTEM1(75))
                          ALENS(75,I)=ALENS(75,I)+(((LA10*((W3+760.00)/760.0D0))-LA10)
     1                    *DSGN(ALENS(75,I)))
                      END IF
                      GLANAM(1,I)='GLASS'
                      GLANAM(2,I)='WQ'
                      RETURN
                  ELSE
                      RETURN
                  END IF
              END IF
          ELSE
              DO I=INT(W1),INT(W2)
                  CALL CHKGLSP(GONOGO,I)
                  IF(.NOT.GONOGO) THEN
                  ELSE
                      IF(GLANAM(2,I).EQ.'REFL         '.OR.
     1                GLANAM(2,I).EQ.'REFLTIRO     '.OR.
     2                GLANAM(2,I).EQ.'REFLTIR      ') THEN
                          WRITE(OUTLYNE,*)
     1                    '"PRES ',WQ,'" DID NOT MODIFY "REFL" SURFACE # ',I
                          CALL SHOWIT(1)
                      END IF
                      IF(GLANAM(2,I).EQ.'PERFECT      ') THEN
                          WRITE(OUTLYNE,*)
     1                    '"PRES ',WQ,'" DID NOT MODIFY "PERFECT" SURFACE # ',I
                          CALL SHOWIT(1)
                      END IF
                      IF(GLANAM(2,I).EQ.'IDEAL        ') THEN
                          WRITE(OUTLYNE,*)
     1                    '"PRES GAS" DID NOT MODIFY "IDEAL" SURFACE # ',I
                          CALL SHOWIT(1)
                          RETURN
                      END IF
                      IF(GLANAM(2,I).NE.'REFL         '.AND.
     1                GLANAM(2,I).NE.'PERFECT      '.AND.
     1                GLANAM(2,I).NE.'REFLTIR      '.AND.
     1                GLANAM(2,I).NE.'REFLTIRO     '.AND.
     1                GLANAM(2,I).NE.'IDEAL        ') THEN
                          IF(DABS(ALENS(46,I)).LE.1.1D0.AND.DABS(ALENS(47,I)).LE.1.1D0
     1                    .OR.DABS(ALENS(48,I)).LE.1.1D0.AND.DABS(ALENS(49,I)).LE.1.1D0
     2                    .AND.DABS(ALENS(50,I)).LE.1.1D0.AND.
     1                    DABS(ALENS(71,I)).LE.1.1D0.AND.DABS(ALENS(72,I)).LE.1.1D0
     1                    .OR.DABS(ALENS(73,I)).LE.1.1D0.AND.DABS(ALENS(74,I)).LE.1.1D0
     2                    .AND.DABS(ALENS(75,I)).LE.1.1D0) THEN
                              IF(SYSTEM1(1).NE.0.0D0) THEN
                                  LA1=LM1(A,B,SYSTEM1(1))
                                  ALENS(46,I)=ALENS(46,I)+(((LA1*((W3+760.00)/760.0D0))-LA1)
     1                            *DSGN(ALENS(46,I)))
                              END IF
                              IF(SYSTEM1(2).NE.0.0D0) THEN
                                  LA2=LM1(A,B,SYSTEM1(2))
                                  ALENS(47,I)=ALENS(47,I)+(((LA2*((W3+760.00)/760.0D0))-LA2)
     1                            *DSGN(ALENS(47,I)))
                              END IF
                              IF(SYSTEM1(3).NE.0.0D0) THEN
                                  LA3=LM1(A,B,SYSTEM1(3))
                                  ALENS(48,I)=ALENS(48,I)+(((LA3*((W3+760.00)/760.0D0))-LA3)
     1                            *DSGN(ALENS(48,I)))
                              END IF
                              IF(SYSTEM1(4).NE.0.0D0) THEN
                                  LA4=LM1(A,B,SYSTEM1(4))
                                  ALENS(49,I)=ALENS(49,I)+(((LA4*((W3+760.00)/760.0D0))-LA4)
     1                            *DSGN(ALENS(49,I)))
                              END IF
                              IF(SYSTEM1(5).NE.0.0D0) THEN
                                  LA5=LM1(A,B,SYSTEM1(5))
                                  ALENS(50,I)=ALENS(50,I)+(((LA5*((W3+760.00)/760.0D0))-LA5)
     1                            *DSGN(ALENS(50,I)))
                              END IF
                              IF(SYSTEM1(71).NE.0.0D0) THEN
!      LA1=LM1(A,B,SYSTEM1(71))
                                  LA6=LM1(A,B,SYSTEM1(71))      ! Mod by ENDO
                                  ALENS(71,I)=ALENS(71,I)+(((LA6*((W3+760.00)/760.0D0))-LA6)
     1                            *DSGN(ALENS(71,I)))
                              END IF
                              IF(SYSTEM1(72).NE.0.0D0) THEN
!      LA2=LM1(A,B,SYSTEM1(72))
                                  LA7=LM1(A,B,SYSTEM1(72))
                                  ALENS(72,I)=ALENS(72,I)+(((LA7*((W3+760.00)/760.0D0))-LA7)
     1                            *DSGN(ALENS(72,I)))
                              END IF
                              IF(SYSTEM1(73).NE.0.0D0) THEN
!      LA3=LM1(A,B,SYSTEM1(73))
                                  LA8=LM1(A,B,SYSTEM1(73))
                                  ALENS(73,I)=ALENS(73,I)+(((LA8*((W3+760.00)/760.0D0))-LA8)
     1                            *DSGN(ALENS(73,I)))
                              END IF
                              IF(SYSTEM1(74).NE.0.0D0) THEN
!      LA4=LM1(A,B,SYSTEM1(74))
                                  LA9=LM1(A,B,SYSTEM1(74))
                                  ALENS(74,I)=ALENS(74,I)+(((LA9*((W3+760.00)/760.0D0))-LA9)
     1                            *DSGN(ALENS(74,I)))
                              END IF
                              IF(SYSTEM1(75).NE.0.0D0) THEN
!      LA5=LM1(A,B,SYSTEM1(75))
                                  LA10=LM1(A,B,SYSTEM1(75))
                                  ALENS(75,I)=ALENS(75,I)+(((LA10*((W3+760.00)/760.0D0))-LA10)
     1                            *DSGN(ALENS(75,I)))
                              END IF
                              GLANAM(1,I)='GLASS'
                              GLANAM(2,I)='WQ'
                          END IF
                      END IF
                  END IF
              END DO
          END IF
          RETURN
      END



      SUBROUTINE ANGLECALC(ALPHA,BETA,LL1,MM1,NN1)
          IMPLICIT NONE
          REAL*8 ALPHA,BETA,LL1,MM1,NN1,
     1    D31,D32,D33,COSB,SINB
C
          INCLUDE 'datmai.inc'
C
C     LL1,MM1,NN1 ARE THE L,M AND N-DIRECTION COSINES OF THE
C     ROTATED COORDINATE SYSTEM. 0.0, 0.0 AND 1.0 ARE THE DIRECTION
C     COSINES OF THE UNROTATED SYSTEM. ONLY AN ALPHA AND A BETA ROTATION
C     RELATE THE TWO SYSTEMS. GAMMA IS ALWAYS ZERO.
          D31=LL1
          D32=MM1
          D33=NN1
C
C     NOW CALCULATE ALPHA,BETA
C     CALCULATE BETA
          BETA=DASIN(-D31)
          COSB=DCOS(BETA)
          IF(COSB.NE.0.0D0) THEN
C     COSINE OF BETA IS NOT ZERO
C     COSINE OF BETA IS NOT ZERO
              IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).NE.0.0D0) ALPHA=0.0D0
              IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=0.0D0
              IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=PII/2.0D0
              IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).NE.0.0D0)
     1        ALPHA=DATAN2((D32/COSB),(D33/COSB))
          END IF
          IF(COSB.EQ.0.0D0) THEN
C     COSINE OF BETA IS ZERO
              IF(D31.EQ.-1.0D0) SINB=1
              IF(D31.EQ.1.0D0) SINB=-1
              IF(SINB.EQ.1) BETA=PII/2.0D0
              IF(SINB.EQ.-1) BETA=-PII/2.0D0
              IF(SINB.EQ.1) THEN
                  IF((D32).EQ.0.0D0.AND.(D33).NE.0.0D0) ALPHA=0.0D0
                  IF((D32).EQ.0.0D0.AND.(D33).EQ.0.0D0) ALPHA=0.0D0
                  IF((D32).NE.0.0D0.AND.(D33).EQ.0.0D0) ALPHA=PII/2.0D0
                  IF((D32).NE.0.0D0.AND.(D33).NE.0.0D0)
     1            ALPHA=DATAN2((D32),(D33))
              END IF
              IF(SINB.EQ.-1) THEN
                  IF((D32).EQ.0.0D0.AND.(D33).NE.0.0D0) ALPHA=0.0D0
                  IF((D32).EQ.0.0D0.AND.(D33).EQ.0.0D0) ALPHA=0.0D0
                  IF((D32).NE.0.0D0.AND.(D33).EQ.0.0D0) ALPHA=PII/2.0D0
                  IF((D32).NE.0.0D0.AND.(D33).NE.0.0D0)
     1            ALPHA=DATAN2((-D32),(-D33))
              END IF
          END IF
C     CONVERT TO DEGREES
          ALPHA=(180.0D0/PII)*ALPHA
          BETA=(180.0D0/PII)*BETA
          RETURN
      END
      SUBROUTINE NEWANGLES(AEEA,BEEB,CEEC,LX,MX,NX,LY,MY,NY,LZ,MZ,NZ)
          IMPLICIT NONE
          INTEGER SINB
          REAL*8 D11,D12,D13,LX,MX,NX,LY,MY,NY,LZ,MZ,NZ
          REAL*8 D21,D22,D23
          REAL*8 D31,D32,D33,BEEB,AEEA,CEEC,COSB
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          D11=LX
          D12=MX
          D13=NX
          D21=LY
          D22=MY
          D23=NY
          D31=LZ
          D32=MZ
          D33=NZ

C     NOW CALCULATE AEEA,BEEB AND CEEC AND RETURN
C     CALCULATE BEEB
          BEEB=DASIN(-D31)
          COSB=DCOS(BEEB)
          IF(COSB.NE.0.0D0) THEN
C     COSINE OF BEEB IS NOT ZERO
              IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).NE.0.0D0) AEEA=0.0D0
              IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).EQ.0.0D0) AEEA=0.0D0
              IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).EQ.0.0D0) AEEA=PII/2.0D0
              IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).NE.0.0D0)
     1        AEEA=DATAN2((D32/COSB),(D33/COSB))
              IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).NE.0.0D0) CEEC=0.0D0
              IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).EQ.0.0D0) CEEC=0.0D0
              IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).EQ.0.0D0) CEEC=PII/2.0D0
              IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).NE.0.0D0)
     1        CEEC=DATAN2((-D21/COSB),(D11/COSB))
          END IF
          IF(COSB.EQ.0.0D0) THEN
C     COSINE OF BEEB IS ZERO
              IF(D31.EQ.-1.0D0) SINB=1
              IF(D31.EQ.1.0D0) SINB=-1
              IF(SINB.EQ.1) BEEB=PII/2.0D0
              IF(SINB.EQ.-1) BEEB=-PII/2.0D0
              CEEC=0.0D0
              IF(SINB.EQ.1) THEN
                  IF((D12).EQ.0.0D0.AND.(D13).NE.0.0D0) AEEA=0.0D0
                  IF((D12).EQ.0.0D0.AND.(D13).EQ.0.0D0) AEEA=0.0D0
                  IF((D12).NE.0.0D0.AND.(D13).EQ.0.0D0) AEEA=PII/2.0D0
                  IF((D12).NE.0.0D0.AND.(D13).NE.0.0D0)
     1            AEEA=DATAN2((D12),(D13))
              END IF
              IF(SINB.EQ.-1) THEN
                  IF((D12).EQ.0.0D0.AND.(D13).NE.0.0D0) AEEA=0.0D0
                  IF((D12).EQ.0.0D0.AND.(D13).EQ.0.0D0) AEEA=0.0D0
                  IF((D12).NE.0.0D0.AND.(D13).EQ.0.0D0) AEEA=PII/2.0D0
                  IF((D12).NE.0.0D0.AND.(D13).NE.0.0D0)
     1            AEEA=DATAN2((-D12),(-D13))
              END IF
          END IF
          AEEA=(180.0D0/PII)*AEEA
          BEEB=(180.0D0/PII)*BEEB
          CEEC=(180.0D0/PII)*CEEC
          RETURN
      END
C SUB OCD.FOR
      SUBROUTINE OCD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE OCD. THIS SUBROUTINE IMPLEMENTS
C       THE OPERATING CONDITION PRINTOUT AT THE CMD LEVEL
C       USING CMD LEVEL COMMAND "OCDY" AND "OCDX". PRINTOUT
C       IS FOR CURRENT LENS FILE AND CURRENT CONFIGURATION ONLY.
C
          INTEGER I,J,K
C
          REAL*8
     4    EFLY,EFLX,BFD
     5    ,FNY,FNX,LENGTH,GIHY,GIHX,SYSP11,OAL,TMAGY,TMAGX,
     6    ERELY,ERELX,EPY,EPX,AMAGY,AMAGX
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(WC.EQ.'OCDY') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"OCDY" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
C       SET DEFAULT TO CONTROL WAVELENGTH
                  W1=SYSTEM1(11)
              END IF
              IF(W1.LT.1.0D0.OR.W1.GT.5.0D0) THEN
                  WRITE(OUTLYNE,*)'"OCDY" TAKES NUMERIC VALUES 1,2,3,4 OR 5 ONLY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'OCDX') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"OCDX" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
C       SET DEFAULT FOR CONTROL WAVELENGTH
                  W1=SYSTEM1(11)
              END IF
              IF(W1.LT.1.0D0.OR.W1.GT.5.0D0) THEN
                  WRITE(OUTLYNE,*)'"OCDX" TAKES NUMERIC VALUES 1,2,3,4 OR 5 ONLY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO OPERATING CODITIONS EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CALCULATE ALL DATA AT WAVELENGTH INT(W1)
C       IS INT(W1) THE CURRENT CONTROL WAVELENGTH?
          IF(INT(W1).NE.INT(SYSTEM1(11))) THEN
C       CHANGE CW AND RECALC ALL PARAX QUANTITIES
              SYSP11=SYSTEM1(11)
              SYSTEM1(11)=DABS(W1)
              F1=0
              F6=1
              F22=0
              LNSTYP=1
              CALL LNSEOS
C       NOW ALL PARAX. QUANTITIES ARE AT DESIRES WAVELENGTH
          ELSE
              SYSP11=SYSTEM1(11)
          END IF
C
C       DEPENDING ON THE "STATE" OF THE CURRENT LENS FILE
C       DIFFERENT THINGS ARE PRINTED OUT.
C
C       STATE 1:FOCAL LENS, OBJECT AT INFINITY, FINAL PUY
C               NOT ZERO. (FOCAL/UFOCAL MODE)
C       PRINT OUT EFL, BACK FOCUS (TH(IMAGE-1)), F-NUMBER,
C       LENGTH(SURF 1 TO IMAGE-1), GAUSSIAN IMAGE HEIGHT
C
C       STATE 2: FOCAL LENS AT FINITE CONJUGATES (FOCAL/UFOCAL MODE)
C       PRINT EFL,BACK FOCUS,IMGAE SPACE F-NUMBER,LENGTH AS
C       IN STATE 1, OAL OBJECT TO IMAGE, T-MAG (TRANSVERSE MAG)
C
C       STATE 3: AFOCAL MODE (UAFOCAL AS WELL)
C       EXIT PUPIL TO IMAGE DIST, EXIT PUPIL RADIUS,
C       ANGULAR MAG, OAL 1 TO IMAGE)
C
C       STATE 1 AND 2
C
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       FOCAL MODE
C               IS IT STATE 1 OR 2?
              IF(DABS(ALENS(3,0)).GE.1.0D10) THEN
C       STATE 1
C               EFL
                  I=0
                  J=INT(SYSTEM1(20))
                  EFLY=-(((PXTRAY(2,I)*PXTRAY(5,I+1))-(PXTRAY(1,I+1)*
     1            PXTRAY(6,I)))/
     2            ((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
                  EFLX=-(((PXTRAX(2,I)*PXTRAX(5,I+1))-(PXTRAX(1,I+1)*
     1            PXTRAX(6,I)))/
     2            ((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J))))
C               BACK FOCAL DISTANCE
                  BFD=ALENS(3,(J-1))
C               F-NUMBER YZ
                  IF(DABS(PXTRAY(2,J)).GE.1.0D-15) THEN
                      FNY=-1.0D0/(2.0D0*PXTRAY(2,J))
                  ELSE
                      FNY=0.0D0
                  END IF
C               F-NUMBER XZ
                  IF(DABS(PXTRAX(2,J)).GE.1.0D-15) THEN
                      FNX=-1.0D0/(2.0D0*PXTRAX(2,J))
                  ELSE
                      FNX=0.0
                  END IF
C       DIST FROM 1 TO IMAGE-1
                  LENGTH=0.0
                  DO 10 K=1,(J-1)
                      LENGTH=LENGTH+ALENS(3,K)
 10               CONTINUE
C       GIH
                  GIHY=PXTRAY(5,J)
                  GIHX=PXTRAX(5,J)
                  IF(WC.EQ.'OCDY')THEN
                      IF(DABS(PXTRAY(2,J)).GE.1.0D-15) THEN
                          WRITE(OUTLYNE,80)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,100)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,101)EFLY,BFD,FNY,LENGTH,GIHY
                          CALL SHOWIT(0)
                          SYSTEM1(11)=SYSP11
                          RETURN
                      ELSE
                          WRITE(OUTLYNE,99)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,80)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,100)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,102)EFLY,BFD,LENGTH
                          CALL SHOWIT(0)
                          SYSTEM1(11)=SYSP11
                      END IF
                      RETURN
                  END IF
                  IF(WC.EQ.'OCDX')THEN
                      IF(DABS(PXTRAX(2,J)).GE.1.0D-15) THEN
                          WRITE(OUTLYNE,90)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,201)EFLX,BFD,FNX,LENGTH,GIHX
                          CALL SHOWIT(0)
                          SYSTEM1(11)=SYSP11
                          RETURN
                      ELSE
                          WRITE(OUTLYNE,99)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,90)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,202)EFLX,BFD,LENGTH
                          CALL SHOWIT(0)
                          SYSTEM1(11)=SYSP11
                      END IF
                      RETURN
                  END IF
              ELSE
C       STATE 2
C               EFL
                  I=0
                  J=INT(SYSTEM1(20))
                  EFLY=-(((PXTRAY(2,I)*PXTRAY(5,I+1))-(PXTRAY(1,I+1)*
     1            PXTRAY(6,I)))/
     2            ((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
                  EFLX=-(((PXTRAX(2,I)*PXTRAX(5,I+1))-(PXTRAX(1,I+1)*
     1            PXTRAX(6,I)))/
     2            ((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J))))
C               BACK FOCAL DISTANCE
                  BFD=ALENS(3,(J-1))
C               F-NUMBER YZ IN IMGAE SPACE
                  IF(DABS(PXTRAY(2,J)).GE.1.0D-15) THEN
                      FNY=-1.0D0/(2.0D0*PXTRAY(2,J))
                  ELSE
                      FNY=0.0
                  END IF
C               F-NUMBER XZ IN IMAGE SPACE
                  IF(DABS(PXTRAX(2,J)).GE.1.0D-15) THEN
                      FNX=-1.0D0/(2.0D0*PXTRAX(2,J))
                  ELSE
                      FNX=0.0
                  END IF
C       DIST FROM 1 TO IMAGE-1
                  LENGTH=0.0
                  DO 11 K=1,(J-1)
                      LENGTH=LENGTH+ALENS(3,K)
 11               CONTINUE
C       OAL
                  OAL=0.0
                  DO 12 K=0,J
                      OAL=OAL+ALENS(3,K)
 12               CONTINUE
C       TMAG
                  IF(DABS(PXTRAY(5,0)).GE.1.0D-15) THEN
                      TMAGY=PXTRAY(5,J)/PXTRAY(5,0)
                  ELSE
                      TMAGY=0.0
                  END IF
                  IF(DABS(PXTRAX(5,0)).GE.1.0D-15) THEN
                      TMAGX=PXTRAX(5,J)/PXTRAX(5,0)
                  ELSE
                      TMAGX=0.0
                  END IF
                  IF(WC.EQ.'OCDY')THEN
                      IF(DABS(PXTRAY(2,J)).GE.1.0D-15) THEN
                          WRITE(OUTLYNE,80)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,301)EFLY,BFD,FNY,LENGTH,OAL,TMAGY
                          CALL SHOWIT(0)
                          SYSTEM1(11)=SYSP11
                          RETURN
                      ELSE
                          WRITE(OUTLYNE,99)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,80)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,300)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,302)EFLY,BFD,LENGTH,OAL
                          CALL SHOWIT(0)
                          SYSTEM1(11)=SYSP11
                      END IF
                      RETURN
                  END IF
                  IF(WC.EQ.'OCDX')THEN
                      IF(DABS(PXTRAX(2,J)).GE.1.0D-15) THEN
                          WRITE(OUTLYNE,90)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,401)EFLX,BFD,FNX,LENGTH,OAL,TMAGX
                          CALL SHOWIT(0)
                          SYSTEM1(11)=SYSP11
                          RETURN
                      ELSE
                          WRITE(OUTLYNE,99)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,90)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,400)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,402)EFLX,BFD,LENGTH,OAL
                          CALL SHOWIT(0)
                          SYSTEM1(11)=SYSP11
                      END IF
                      RETURN
                  END IF
              END IF
          ELSE
C       MUST BE AFOCAL OR UAFOCAL, STATE 3
C       STATE 3:
              J=INT(SYSTEM1(20))
C       EX P DIST
              IF(DABS(PXTRAY(6,J)).GE.1E-15) THEN
                  ERELY=-PXTRAY(5,J)/PXTRAY(6,J)
              ELSE
                  ERELY=0.0
              END IF
              IF(DABS(PXTRAX(6,J)).GE.1E-15) THEN
                  ERELX=-PXTRAX(5,J)/PXTRAX(6,J)
              ELSE
                  ERELX=0.0
              END IF
C       CALCULATE EXIT PUPIL RADIUS
              EPY=PXTRAY(1,J)+(PXTRAY(2,J)*ERELY)
              EPX=PXTRAX(1,J)+(PXTRAX(2,J)*ERELX)
C       ANGULAR MAG
              IF(DABS(PXTRAY(6,0)).GE.1E-15) THEN
                  AMAGY=PXTRAY(6,J)/PXTRAY(6,0)
              ELSE
                  AMAGY=0.0
              END IF
              IF(DABS(PXTRAX(6,0)).GE.1E-15) THEN
                  AMAGX=PXTRAX(6,J)/PXTRAX(6,0)
              ELSE
                  AMAGX=0.0
              END IF

C       DIST FROM 1 TO IMAGE
              LENGTH=0.0
              DO 15 K=1,J
                  LENGTH=LENGTH+ALENS(3,K)
 15           CONTINUE
              IF(WC.EQ.'OCDY')THEN
                  IF(DABS(PXTRAY(6,J)).GT.1.0D-15) THEN
                      WRITE(OUTLYNE,80)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,500)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,501)ERELY,EPY,AMAGY,LENGTH
                      CALL SHOWIT(0)
                      SYSTEM1(11)=SYSP11
                      RETURN
                  ELSE
                      WRITE(OUTLYNE,99)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,80)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,500)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,502) EPY,LENGTH
                      CALL SHOWIT(0)
                      SYSTEM1(11)=SYSP11
                  END IF
                  RETURN
              END IF
              IF(WC.EQ.'OCDX')THEN
                  IF(DABS(PXTRAX(6,J)).GT.1.0D-15) THEN
                      WRITE(OUTLYNE,90)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,600)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,601)ERELX,EPX,AMAGX,LENGTH
                      CALL SHOWIT(0)
                      SYSTEM1(11)=SYSP11
                      RETURN
                  ELSE
                      WRITE(OUTLYNE,99)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,90)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,600)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,602)EPX,LENGTH
                      CALL SHOWIT(0)
                      SYSTEM1(11)=SYSP11
                  END IF
                  RETURN
              END IF
          END IF
          RETURN
C
 100      FORMAT(2X,'EFL',10X,'BFD',10X,'F-NUM',8X
     1    ,'LENGTH',6X,'GIH')
 200      FORMAT(2X,'EFL',10X,'BFD',10X,'F-NUM',8X
     1    ,'LENGTH',6X,'GIH')
 300      FORMAT(2X,'EFL',10X,'BFD',10X,'IMG F-NUM',4X
     1    ,'LENGTH',7X,'OAL',10X,'T-MAG')
 400      FORMAT(2X,'EFL',10X,'BFD',10X,'IMG F-NUM',4X
     1    ,'LENGTH',7X,'OAL',10X,'T-MAG')
 500      FORMAT(2X,'EX PUP DIST',2X,'EX PUP RAD',4X
     1    ,'A-MAG',8X,'OAL')
 600      FORMAT(2X,'EX PUP DIST',2X,'EX PUP RAD',4X
     1    ,'A-MAG',8X,'OAL')
C
 101      FORMAT(G12.5,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5)
 102      FORMAT(G12.5,1X,G12.5,1X,'NOT DEFINED '
     1    ,1X,G12.5,1X,'NOT DEFINED')
 201      FORMAT(G12.5,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5)
 202      FORMAT(G12.5,1X,G12.5,1X,'NOT DEFINED '
     1    ,1X,G12.5,1X,'NOT DEFINED')
 301      FORMAT(G12.5,1X,G12.5,1X,G12.5,1X,G12.5,1X,
     1    G12.5,1X,G12.5)
 302      FORMAT(G12.5,1X,G12.5,1X,'NOT DEFINED '
     1    ,1X,G12.5,1X,G12.5,1X,'NOT DEFINED ')
 401      FORMAT(G12.5,1X,G12.5,1X,G12.5,1X,G12.5,1X,
     1    G12.5,1X,G12.5)
 402      FORMAT(G12.5,1X,G12.5,1X,'NOT DEFINED '
     1    ,1X,G12.5,1X,G12.5,1X,'NOT DEFINED ')
 501      FORMAT(G12.5,1X,G12.5,2X,G12.5,1X,G12.5)
 502      FORMAT('NOT DEFINED ',1X,G12.5,2X
     1    ,'NOT DEFINED ',1X,G12.5)
 601      FORMAT(G12.5,1X,G12.5,2X,G12.5,1X,G12.5)
 602      FORMAT('NOT DEFINED ',1X,G12.5,2X
     1    ,'NOT DEFINED ',1X,G12.5)
 99       FORMAT(1X)
 80       FORMAT('YZ-PLANE FIRST ORDER OPERATING CONDITIONS')
 90       FORMAT('XZ-PLANE FIRST ORDER OPERATING CONDITIONS')
      END
C SUB PRPIK.FOR
      SUBROUTINE PRPIK
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRPIK WHICH IMPLEMENTS THE PIK
C       COMMAND AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
C
          CHARACTER TYPE*7,A10*10,B10*10
C
          INTEGER PIKCNT,JSURF,KSURF,I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED
C
          A10='FROM CFG#1'
          B10='          '
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PIK" TAKES NO NUMERIC WORD #2 THROUGH #5 OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              S1=0
              DF1=0
              W1=0.0D0
              OUTLYNE=
     1        '"PIK" TAKES EITHER QUALIFIER OR'
              CALL SHOWIT(1)
              OUTLYNE='NUMERIC WORD #1 INPUT BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO PIKUPS EXIST'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       W1 DESIGNATES THE SURFACE FOR WHICH THE PIKUP
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE PIKUP DATA FOR
C       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
C       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C
C       THIS IS SAME AS SLV,0
              S1=1
              DF1=0
              SQ=0
              WQ='        '
              W1=0.0
          ELSE
C       WQ NOT OB OR OBJ
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ALL') THEN
C
C       WE HAVE INVALID QUALIFIER
              OUTLYNE='INVALID QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
C       HANDEL AN INDIVIDUAL SURFACE INCLUDING "OB" AND "OBJ"
              IF(DF1.EQ.1) W1=DBLE(INT(SYSTEM1(20)))
              SURF=INT(W1)
              IF(SURF.GT.(INT(SYSTEM1(20))).OR.SURF.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PIKUP DATA IS PRINTED OUT IN THE ORDER THAT IT IS STORED
C       AS DESIGNATED BY THE THIRD DIMENSION OF THE PIKUP ARRAY
C       PIKUP(I,SURF,1) IS PRINTED BEFORE PIKUP(I,SURF,2), WHERE HERE
C       I RUNS FROM 1 TO 6 FOR ALL THE DATA FOR ONE PIKUP
C
C       OUTPUT FORM IS:
C
C       SURFACE NUMBER,PIKUP TYPE,SURFACE NUMBER FROM WHICH DATA
C       IS PIKED UP,THE MULTIPLIER A,THE ADDITIVE CONSTANT B
C
C       FIRST DETERMINE IF THERE ARE PIKUPS ON A SURFACE
C
              IF(ALENS(32,SURF).EQ.0.0D0) THEN
C       NO PIKUPS
                  WRITE(OUTLYNE,110) SURF
                  CALL SHOWIT(0)
                  RETURN
              END IF
C       THERE ARE PIKUPS
C       SWEEP THROUGH THE LIST AND PRINT ALL PIKUPS
C       PRESENT FOR A SURFACE
C
C       NOTE: GO TO 11 AND GO TO 21 ARE USED TO JUMP TO THE
C       BOTTOM OF THE
C       SEARCH LOOP. I CORNERED MYSELF INTO A (GO TO). SORRY
C       IF THAT MAKES THE CODE HARDER TO FOLLOW.
C
              IF(HEADIN) WRITE(OUTLYNE,2000)
              CALL SHOWIT(0)
              DO 10 I=1,PSIZ
                  IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
C       PRINT THE PIKUP
                      IF(I.EQ.1) THEN
                          TYPE='RD     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.2) THEN
                          TYPE='CV     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.3) THEN
                          TYPE='TH     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.32) THEN
                          TYPE='THOAL  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          KSURF=INT(PIKUP(3,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,202) SURF,TYPE,JSURF,KSURF,PIKUP(4,SURF,I),
     1                        PIKUP(5,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,202) SURF,TYPE,JSURF,KSURF,PIKUP(4,SURF,I),
     1                        PIKUP(5,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.34) THEN
                          TYPE='PIVX   '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.35) THEN
                          TYPE='PIVY   '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.36) THEN
                          TYPE='PIVZ   '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.4) THEN
                          TYPE='CC     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.5) THEN
                          TYPE='AD     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.6) THEN
                          TYPE='AE     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.7) THEN
                          TYPE='AF     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.8) THEN
                          TYPE='AG     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.9) THEN
                          TYPE='CVTOR  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.10)  THEN
                          TYPE='RDTOR  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.11) THEN
                          TYPE='PRO    '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.12) THEN
                          TYPE='NPRO   '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.37)  THEN
                          TYPE='GDX    '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.38)  THEN
                          TYPE='GDY    '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.39)  THEN
                          TYPE='GDZ    '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.13)  THEN
                          TYPE='YD     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.14) THEN
                          TYPE='XD     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.33) THEN
                          TYPE='ZD     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(4,SURF,I),
     1                        PIKUP(5,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(4,SURF,I),
     1                        PIKUP(5,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.15) THEN
                          TYPE='ALPHA  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.16) THEN
                          TYPE='BETA   '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.17) THEN
                          TYPE='GAMMA  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.40) THEN
                          TYPE='GALPHA '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.41) THEN
                          TYPE='GBETA  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.42) THEN
                          TYPE='GGAMMA '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.18) THEN
                          TYPE='CLAP   '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.19) THEN
                          TYPE='COBS   '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.20) THEN
                          TYPE='GLASS  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0)
     1                    WRITE(OUTLYNE,201) SURF,TYPE,JSURF,B10
                          IF(PIKUP(6,SURF,I).NE.0.0D0)
     1                    WRITE(OUTLYNE,201) SURF,TYPE,JSURF,A10
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.44) THEN
                          TYPE='COATING'
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0)
     1                    WRITE(OUTLYNE,201) SURF,TYPE,JSURF,B10
                          IF(PIKUP(6,SURF,I).NE.0.0D0)
     1                    WRITE(OUTLYNE,201) SURF,TYPE,JSURF,A10
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.43) THEN
                          TYPE='GRT    '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0)
     1                    WRITE(OUTLYNE,201) SURF,TYPE,JSURF,B10
                          IF(PIKUP(6,SURF,I).NE.0.0D0)
     1                    WRITE(OUTLYNE,201) SURF,TYPE,JSURF,A10
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.21) THEN
                          TYPE='CCTOR  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.22) THEN
                          TYPE='ADTOR  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.23) THEN
                          TYPE='AETOR  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.24) THEN
                          TYPE='AFTOR  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.25) THEN
                          TYPE='AGTOR  '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.27) THEN
                          TYPE='AH     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.28) THEN
                          TYPE='AI     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.29) THEN
                          TYPE='AJ     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.30) THEN
                          TYPE='AK     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.31) THEN
                          TYPE='AL     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                      IF(I.EQ.26) THEN
                          TYPE='AC     '
                          JSURF=INT(PIKUP(2,SURF,I))
                          IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),B10
                          ELSE
                              WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                        PIKUP(4,SURF,I),A10
                          END IF
                          CALL SHOWIT(0)
                          GO TO 11
                      END IF
                  END IF
 11               CONTINUE
 10           CONTINUE
C       ALL DONE FOR THIS SURFACE, JUST RETURN
              RETURN
C
          ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
              PIKCNT=0
C       PRINT HEADING DATA
C
C       SET THE PIKUP COUNTER. IT IS USED TO DETERMINE
C       THE CASE OF NO PIKUPS IN A LENS
              DO 16 SURF=0,INT(SYSTEM1(20))
                  IF(ALENS(32,SURF).NE.0.0D0) THEN
                      PIKCNT=PIKCNT+1
                  ELSE
                  END IF
 16           CONTINUE
              IF(PIKCNT.EQ.0) THEN
                  WRITE(OUTLYNE,100)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C       PROCEED WITH PRINTOUT
              END IF
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)
              CALL SHOWIT(0)
              DO 17 SURF=0,INT(SYSTEM1(20))
C
C       CHECK FOR A PIKUP ON CURRENT SURFACE
                  IF(ALENS(32,SURF).EQ.0.0D0) THEN
C       NO PIKUPS
C       GO TO NEXT SURFACE IN LOOP
                      GO TO 17
                  END IF
C       THERE ARE PIKUPS
C       SWEEP THROUGH THE LIST AND PRINT ALL PIKUPS
C       PRESENT FOR A SURFACE
                  DO 20 I=1,PSIZ
                      IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
C       PRINT THE PIKUP
                          IF(I.EQ.1) THEN
                              TYPE='RD     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.2) THEN
                              TYPE='CV     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.3) THEN
                              TYPE='TH     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.32) THEN
                              TYPE='THOAL  '
                              JSURF=INT(PIKUP(2,SURF,I))
                              KSURF=INT(PIKUP(3,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,202) SURF,TYPE,JSURF,KSURF,PIKUP(4,SURF,I),
     1                            PIKUP(5,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,202) SURF,TYPE,JSURF,KSURF,PIKUP(4,SURF,I),
     1                            PIKUP(5,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.34) THEN
                              TYPE='PIVX   '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.35) THEN
                              TYPE='PIVY   '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.36) THEN
                              TYPE='PIVZ   '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.4) THEN
                              TYPE='CC     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.5) THEN
                              TYPE='AD     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.6) THEN
                              TYPE='AE     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.7) THEN
                              TYPE='AF     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.8) THEN
                              TYPE='AG     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.9) THEN
                              TYPE='CVTOR  '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.11) THEN
                              TYPE='PRO    '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.12) THEN
                              TYPE='NPRO   '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.37)  THEN
                              TYPE='GDX    '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.38)  THEN
                              TYPE='GDY    '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.39)  THEN
                              TYPE='GDZ    '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.13)  THEN
                              TYPE='YD     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.14) THEN
                              TYPE='XD     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.33) THEN
                              TYPE='ZD     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(4,SURF,I),
     1                            PIKUP(5,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(4,SURF,I),
     1                            PIKUP(5,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.15) THEN
                              TYPE='ALPHA  '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF
     1                            ,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I)
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,
     1                            PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I)
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.16) THEN
                              TYPE='BETA   '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,
     1                            PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I)
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,
     1                            PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I)
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.17) THEN
                              TYPE='GAMMA  '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.40) THEN
                              TYPE='GALPHA '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF
     1                            ,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I)
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,
     1                            PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I)
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.41) THEN
                              TYPE='GBETA  '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,
     1                            PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I)
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,
     1                            PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I)
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.42) THEN
                              TYPE='GGAMMA '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.18) THEN
                              TYPE='CLAP   '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.19) THEN
                              TYPE='COBS   '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.20) THEN
                              TYPE='GLASS  '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0)
     1                        WRITE(OUTLYNE,201) SURF,TYPE,JSURF,B10
                              IF(PIKUP(6,SURF,I).NE.0.0D0)
     1                        WRITE(OUTLYNE,201) SURF,TYPE,JSURF,A10
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.44) THEN
                              TYPE='COATING'
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0)
     1                        WRITE(OUTLYNE,201) SURF,TYPE,JSURF,B10
                              IF(PIKUP(6,SURF,I).NE.0.0D0)
     1                        WRITE(OUTLYNE,201) SURF,TYPE,JSURF,A10
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.43) THEN
                              TYPE='GRT    '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0)
     1                        WRITE(OUTLYNE,201) SURF,TYPE,JSURF,B10
                              IF(PIKUP(6,SURF,I).NE.0.0D0)
     1                        WRITE(OUTLYNE,201) SURF,TYPE,JSURF,A10
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.21) THEN
                              TYPE='CCTOR  '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.22) THEN
                              TYPE='ADTOR  '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.23) THEN
                              TYPE='AETOR  '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.24) THEN
                              TYPE='AFTOR  '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              CALL SHOWIT(0)
                              GO TO 21
                          END IF
                          IF(I.EQ.25) THEN
                              TYPE='AGTOR  '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              GO TO 21
                          END IF
                          IF(I.EQ.27) THEN
                              TYPE='AH     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              GO TO 21
                          END IF
                          IF(I.EQ.28) THEN
                              TYPE='AI     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              GO TO 21
                          END IF
                          IF(I.EQ.29) THEN
                              TYPE='AJ     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              GO TO 21
                          END IF
                          IF(I.EQ.30) THEN
                              TYPE='AK     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              GO TO 21
                          END IF
                          IF(I.EQ.31) THEN
                              TYPE='AL     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              GO TO 21
                          END IF
                          IF(I.EQ.26) THEN
                              TYPE='AC     '
                              JSURF=INT(PIKUP(2,SURF,I))
                              IF(PIKUP(6,SURF,I).EQ.0.0D0) THEN
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),B10
                              ELSE
                                  WRITE(OUTLYNE,200) SURF,TYPE,JSURF,PIKUP(3,SURF,I),
     1                            PIKUP(4,SURF,I),A10
                              END IF
                              GO TO 21
                          END IF
                      END IF
 21                   CONTINUE
 20               CONTINUE
C       LOOK AT NEXT SURFACE NOW
 17           CONTINUE
          END IF
C
 200      FORMAT(I3,3X,A7,1X,I3,3X,5X,3X,G12.5,3X,G12.5,1X,A10)
 202      FORMAT(I3,3X,A7,1X,I3,4X,I3,4X,G12.5,3X,G12.5,1X,A10)
 201      FORMAT(I3,3X,A7,1X,I3,3X,23X,3X,12X,1X,A10)
 110      FORMAT('SURF',1X,I3,1X,
     1    ':NO PIKUP DATA')
 100      FORMAT('NO PIKUP DATA')
 1000     FORMAT('PIKUP DATA')
 2000     FORMAT('SURF',1X,'TYPE  ',4X,'J',6X,'K',8X,'A',14X,'B')
 1500     FORMAT(1X)
          RETURN
      END
