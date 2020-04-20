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

C       THIRD FILE FOR LENS DATABASE MANAGER FILES

C SUB STAD.FOR
      SUBROUTINE STAD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE STAD WHICH IMPLEMENTS THE TAD
C       COMMAND AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
C
          CHARACTER TYPE1*5,TYPE2*3
C
          INTEGER TCNT,DCNT,TAD,DEC,TADCNT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
              OUTLYNE=
     1        '"TAD" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
C
              OUTLYNE=
     1        '"TAD" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO TILTS OR DECENTERS EXIST'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE TAD
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE TILT/DEC DATA FOR
C       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
C       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C
C       THIS IS SAME AS TAD,0
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
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C SET DEC AND TAD APPROPRIATELY
C
              IF(ALENS(25,SURF).NE.0.0D0) THEN
                  TAD=1
              ELSE
                  TAD=0
              END IF
              IF(ALENS(29,SURF).NE.0.0D0) THEN
                  DEC=1
              ELSE
                  DEC=0
              END IF
C
C       FIRST PRINT TAD DATA. ONLY ONE KIND OF TAD CAN EXITS
C       ON A SURFACE
C
C               HANDEL NO DATA FOR A SURFACE
              IF(TAD.EQ.0.AND.DEC.EQ.0) THEN
                  WRITE(OUTLYNE,100) SURF
                  CALL SHOWIT(0)
                  RETURN
              END IF
C
C                       DO PRINTING NOW
C*****************************************************************
C
C
C
              IF(ALENS(25,SURF).NE.0.0D0) THEN
                  IF(ALENS(25,SURF).EQ.1.0.AND.ALENS(77,SURF).EQ.0.0D0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='   '
                  END IF
                  IF(ALENS(25,SURF).EQ.-1.0) THEN
                      TYPE1 ='RTILT'
                      TYPE2 ='   '
                  END IF
                  IF(ALENS(25,SURF).EQ.2.0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='  A'
                  END IF
                  IF(ALENS(25,SURF).EQ.3.0) THEN
                      TYPE1 =' TILT'
                      TYPE2 =' AM'
                  END IF
                  IF(ALENS(25,SURF).EQ.4.0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='BEN'
                  END IF
                  IF(ALENS(25,SURF).EQ.5.0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='DAR'
                  END IF
                  IF(ALENS(25,SURF).EQ.7.0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='REV'
                  END IF
                  IF(ALENS(25,SURF).EQ.6.0.OR.ALENS(25,SURF).EQ.1.0D0.AND.
     1            ALENS(77,SURF).EQ.1.0D0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='RET'
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,2000)
                  IF(HEADIN) CALL SHOWIT(0)
                  IF(ALENS(25,SURF).EQ.6.0D0.OR.ALENS(25,SURF).EQ.1.0D0.AND.
     1            ALENS(77,SURF).EQ.1.0D0) THEN
                      WRITE(OUTLYNE,2001) SURF,TYPE1,TYPE2,
     1                ALENS(26,SURF),ALENS(27,SURF),ALENS(28,SURF),
     1                INT(ALENS(70,SURF))
                  ELSE
                      WRITE(OUTLYNE,200) SURF,TYPE1,TYPE2,
     1                ALENS(26,SURF),ALENS(27,SURF),ALENS(28,SURF)
                  END IF
                  CALL SHOWIT(0)
              END IF
              IF(ALENS(29,SURF).NE.0.0D0) THEN
                  TYPE1='  DEC'
                  TYPE2='   '
C      WRITE(OUTLYNE,1500)
C      CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,3000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,200) SURF,TYPE1,TYPE2,ALENS(30,SURF),ALENS(31,SURF),
     1            ALENS(69,SURF)
                  CALL SHOWIT(0)
              END IF
C     GLOBAL DECENTERS AND TILTS
              IF(ALENS(90,SURF).NE.0.0D0.OR.ALENS(91,SURF).NE.0.0D0.OR.
     1        ALENS(92,SURF).NE.0.0D0.OR.ALENS(93,SURF).NE.0.0D0.OR.
     2        ALENS(94,SURF).NE.0.0D0.OR.ALENS(95,SURF).NE.0.0D0) THEN
                  WRITE(OUTLYNE,9000) SURF
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,9001) ALENS(90,SURF)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,9002) ALENS(91,SURF)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,9003) ALENS(92,SURF)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,9004) ALENS(93,SURF)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,9005) ALENS(94,SURF)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,9006) ALENS(95,SURF)
                  CALL SHOWIT(0)
              END IF
              RETURN
          ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
C       PRINT HEADING DATA
C
C       SET THE TILT COUNTER. IT IS USED TO DETERMINE
C       THE CASE OF NO TILTS OR DECENTERS IN A LENS
              TADCNT=0
              DCNT=0
              TCNT=0
              DO SURF=0,INT(SYSTEM1(20))
                  IF(ALENS(25,SURF).NE.0.0.AND.ALENS(29,SURF).NE.0.0
     1            .OR.ALENS(25,SURF).NE.0.0.OR.ALENS(29,SURF)
     2            .NE.0.0D0) THEN
                      IF(ALENS(25,SURF).NE.0.0D0) TCNT=TCNT+1
                      IF(ALENS(29,SURF).NE.0.0D0) DCNT=DCNT+1
                      TADCNT=TADCNT+1
                  ELSE
                  END IF
              END DO
              IF(TADCNT.EQ.0) THEN
                  WRITE(OUTLYNE,110)
                  CALL SHOWIT(0)
                  RETURN
              END IF
              WRITE(OUTLYNE,1500)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000)
              CALL SHOWIT(0)
              DO SURF=0,INT(SYSTEM1(20))
C
C SET DEC AND TAD APPROPRIATELY
C
                  IF(ALENS(25,SURF).NE.0.0D0) THEN
                      TAD=1
                  ELSE
                      TAD=0
                  END IF
                  IF(TAD.EQ.0) GO TO 15
C
C       SET THE TYPE1 AND TYPE2 TYPES CORRECTLY
C
                  IF(ALENS(25,SURF).EQ.1.0.AND.ALENS(77,SURF).EQ.0.0D0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='   '
                  END IF
                  IF(ALENS(25,SURF).EQ.-1.0) THEN
                      TYPE1 ='RTILT'
                      TYPE2 ='   '
                  END IF
                  IF(ALENS(25,SURF).EQ.2.0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='  A'
                  END IF
                  IF(ALENS(25,SURF).EQ.3.0) THEN
                      TYPE1 =' TILT'
                      TYPE2 =' AM'
                  END IF
                  IF(ALENS(25,SURF).EQ.4.0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='BEN'
                  END IF
                  IF(ALENS(25,SURF).EQ.5.0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='DAR'
                  END IF
                  IF(ALENS(25,SURF).EQ.7.0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='REV'
                  END IF
                  IF(ALENS(25,SURF).EQ.6.0.OR.ALENS(25,SURF).EQ.1.0D0.AND.
     1            ALENS(77,SURF).EQ.1.0D0) THEN
                      TYPE1 =' TILT'
                      TYPE2 ='RET'
                  END IF
C
C       FIRST PRINT TAD DATA. ONLY ONE KIND OF TAD CAN EXITS
C       ON A SURFACE
C
C                       DO PRINTING NOW
C*****************************************************************
C
                  IF(ALENS(25,SURF).EQ.6.0.OR.ALENS(25,SURF).EQ.1.0D0.AND.
     1            ALENS(77,SURF).EQ.1.0D0) THEN
                      WRITE(OUTLYNE,2001) SURF,TYPE1,TYPE2
     1                ,ALENS(26,SURF),ALENS(27,SURF),ALENS(28,SURF),
     1                INT(ALENS(70,SURF))
                  ELSE
                      WRITE(OUTLYNE,200) SURF,TYPE1,TYPE2
     1                ,ALENS(26,SURF),ALENS(27,SURF),ALENS(28,SURF)
                  END IF
                  CALL SHOWIT(0)
 15               CONTINUE
              END DO
C
C     DECENTERS
              IF(DCNT.NE.0) THEN
                  WRITE(OUTLYNE,1500)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000)
                  CALL SHOWIT(0)
C
                  DO SURF=0,INT(SYSTEM1(20))
C
C SET DEC
C
                      IF(ALENS(29,SURF).NE.0.0D0) THEN
                          DEC=1
                      ELSE
                          DEC=0
                      END IF
C
C
                      IF(DEC.EQ.1) THEN
C       YOU GOT HERE BECAUSE DEC=1 NOT BECAUSE TAD=1
                          TYPE1='  DEC'
                          TYPE2='   '
                      END IF
C
C       FIRST PRINT TAD DATA. ONLY ONE KIND OF TAD CAN EXITS
C       ON A SURFACE
C
C               HANDEL NO DATA FOR A SURFACE
                      IF(DEC.EQ.0) THEN
C       JUMP TO THE CONTINUE STATEMENT AND DO THE NEXT SURFACE
                          GO TO 16
                      ELSE
C       PROCEED
                      END IF
C
C                       DO PRINTING NOW
C*****************************************************************
C
                      WRITE(OUTLYNE,200) SURF,TYPE1,TYPE2
     1                ,ALENS(30,SURF),ALENS(31,SURF)
     1                ,ALENS(69,SURF)
                      CALL SHOWIT(0)
C
 16                   CONTINUE
                  END DO
              END IF
              DO SURF=0,INT(SYSTEM1(20))
C     GLOBAL DECENTERS AND TILTS
                  IF(ALENS(90,SURF).NE.0.0D0.OR.ALENS(91,SURF).NE.0.0D0.OR.
     1            ALENS(92,SURF).NE.0.0D0.OR.ALENS(93,SURF).NE.0.0D0.OR.
     2            ALENS(94,SURF).NE.0.0D0.OR.ALENS(95,SURF).NE.0.0D0) THEN
                      WRITE(OUTLYNE,9000) SURF
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,9001) ALENS(90,SURF)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,9002) ALENS(91,SURF)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,9003) ALENS(92,SURF)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,9004) ALENS(93,SURF)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,9005) ALENS(94,SURF)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,9006) ALENS(95,SURF)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,1500)
                      CALL SHOWIT(0)
                  END IF
              END DO
          END IF
C
 200      FORMAT(I3,1X,A5,1X,A3,1X,G15.8,1X,G15.8,1X,G15.8)
 2001     FORMAT(I3,1X,A5,1X,A3,1X,G15.8,1X,G15.8,1X,G15.8,5X,I3)
C
 100      FORMAT('SURF',1X,I3,1X,
     1    ':NO TILT AND DECENTER DATA')
 110      FORMAT('NO TILT AND DECENTER DATA')
 2000     FORMAT('SURF',1X,'TYPE',9X,'ALPHA',10X,'BETA',12X,'GAMMA'
     1    ,9X,'RETURN SURF ')
 3000     FORMAT('SURF',1X,'TYPE',8X,'  YD ',12X,' XD ',11X,'  ZD ')
 1500     FORMAT(1X)
 9000     FORMAT('SURFACE NUMBER = ',I3)
 9001     FORMAT('GLOBAL X-DECENTRATION (GDX)    = ',G15.8)
 9002     FORMAT('GLOBAL Y-DECENTRATION (GDY)    = ',G15.8)
 9003     FORMAT('GLOBAL Z-DECENTRATION (GDZ)    = ',G15.8)
 9004     FORMAT('GLOBAL ALPHA TILT     (GALPHA) = ',G15.8)
 9005     FORMAT('GLOBAL BETA TILT      (GBETA)  = ',G15.8)
 9006     FORMAT('GLOBAL GAMMA TILT     (GGAMMA) = ',G15.8)
          RETURN
      END
C SUB SPIV.FOR
      SUBROUTINE SPIV
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPIV WHICH IMPLEMENTS THE PIV
C       COMMAND AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
C
          CHARACTER TYPE1*5
C
          INTEGER PCNT,PIV,PIVCNT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
              OUTLYNE=
     1        '"PIVOT" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
C
              OUTLYNE=
     1        '"PIVOT" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO PIVOT DEFINITIONS EXIST'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE PIVOT
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE PIVOT DATA FOR
C       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
C       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C
C       THIS IS SAME AS PIV,0
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
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C SET PIV APPROPRIATELY
C
              IF(ALENS(59,SURF).NE.0.0D0) THEN
                  PIV=1
              ELSE
                  PIV=0
              END IF
C
C       FIRST PRINT PIV DATA. ONLY ONE KIND OF TAD CAN EXITS
C       ON A SURFACE
C
C               HANDEL NO DATA FOR A SURFACE
              IF(PIV.EQ.0) THEN
                  WRITE(OUTLYNE,100) SURF
                  CALL SHOWIT(0)
                  RETURN
              END IF
C
C                       DO PRINTING NOW
C*****************************************************************
C
C
C
              IF(ALENS(59,SURF).NE.0.0D0) THEN
                  IF(ALENS(59,SURF).EQ.1.0D0) THEN
                      TYPE1 ='PIVOT'
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,3000)
                  IF(HEADIN) CALL SHOWIT(0)
                  IF(ALENS(59,SURF).EQ.1.0D0) THEN
                      WRITE(OUTLYNE,200) SURF,TYPE1,
     1                ALENS(78,SURF),ALENS(79,SURF),ALENS(80,SURF)
                      CALL SHOWIT(0)
                  END IF
              END IF
              RETURN
          ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
C       PRINT HEADING DATA
C
C       SET THE PIVOT COUNTER. IT IS USED TO DETERMINE
C       THE CASE OF NO PIVOT IN A LENS
              PIVCNT=0
              PCNT=0
              DO SURF=0,INT(SYSTEM1(20))
                  IF(ALENS(59,SURF).NE.0.0D0) THEN
                      IF(ALENS(59,SURF).NE.0.0D0) PCNT=PCNT+1
                      PIVCNT=PIVCNT+1
                  ELSE
                  END IF
              END DO
              IF(PIVCNT.EQ.0) THEN
                  WRITE(OUTLYNE,110)
                  CALL SHOWIT(0)
                  RETURN
              END IF
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,3000)
              CALL SHOWIT(0)
              DO SURF=0,INT(SYSTEM1(20))
C
C SET PIVAPPROPRIATELY
C
                  IF(ALENS(59,SURF).NE.0.0D0) THEN
                      PIV=1
                  ELSE
                      PIV=0
                  END IF
                  IF(PIV.EQ.0) GO TO 15
C
C       SET TYPE1 CORRECTLY
C
                  IF(ALENS(59,SURF).EQ.1.0D0) THEN
                      TYPE1 ='PIVOT'
                  END IF
C
C       FIRST PRINT PIV DATA. ONLY ONE KIND OF PIV CAN EXITS
C       ON A SURFACE
C
C                       DO PRINTING NOW
C*****************************************************************
C
                  IF(ALENS(59,SURF).EQ.1.0D0) THEN
                      WRITE(OUTLYNE,200) SURF,TYPE1
     1                ,ALENS(78,SURF),ALENS(79,SURF),ALENS(80,SURF)
                      CALL SHOWIT(0)
                  END IF
 15               CONTINUE
              END DO
C
          END IF
C
 200      FORMAT(I3,2X,A5,3X,1X,G15.8,1X,G15.8,1X,G15.8)
C
 100      FORMAT('SURF',1X,I3,1X,
     1    ':NO USER-DEFINED PIVOT POINT DATA')
 110      FORMAT('NO USER-DEFINED PIVOT POINT DATA')
 1000     FORMAT(
     1    'USER DEFINED PIVOT POINT DATA')
 3000     FORMAT('SURF',1X,'    ',8X,'  X  ',12X,' Y  ',11X,'  Z  ')
 1500     FORMAT(1X)
          RETURN
      END
C SUB SPIDEROUT.FOR
      SUBROUTINE SPIDEROUT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPIDEROUT WHICH IMPLEMENTS THE SPIDER
C       COMMAND AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
C
          INTEGER SCNT,SPID,SPIDCNT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
              OUTLYNE=
     1        '"SPIDER" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
C
              OUTLYNE=
     1        '"SPIDER" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='NO SPIDER DEFINITIONS EXIST'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE SPIDER
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE SPIDER DATA FOR
C       THE ENTIRE LENS IS PRINTED. IF THE QUALIFIER IS "OB" OR "OBJ"
C       THEN DATA FOR THE OBJECT SURFACE IS PRINTED.
C
          IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') THEN
C
C       THIS IS SAME AS SPIDER,0
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
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C SET SPID APPROPRIATELY
C
              IF(ALENS(134,SURF).NE.0.0D0) THEN
                  SPID=1
              ELSE
                  SPID=0
              END IF
C
C       FIRST PRINT SPIDER DATA.
C
C               HANDEL NO DATA FOR A SURFACE
              IF(SPID.EQ.0) THEN
                  WRITE(OUTLYNE,100) SURF
                  CALL SHOWIT(0)
                  RETURN
              END IF
C
C                       DO PRINTING NOW
C*****************************************************************
C
C
C
              IF(ALENS(134,SURF).NE.0.0D0) THEN
                  IF(HEADIN) WRITE(OUTLYNE,3000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,200) SURF,
     1            INT(ALENS(135,SURF)),ALENS(136,SURF),
     2            ALENS(137,SURF)
                  CALL SHOWIT(0)
              END IF
              RETURN
          ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
C       PRINT HEADING DATA
C
C       SET THE PIVOT COUNTER. IT IS USED TO DETERMINE
C       THE CASE OF NO PIVOT IN A LENS
              SPIDCNT=0
              SCNT=0
              DO SURF=0,INT(SYSTEM1(20))
                  IF(ALENS(134,SURF).NE.0.0D0) THEN
                      IF(ALENS(134,SURF).NE.0.0D0) SCNT=SCNT+1
                      SPIDCNT=SPIDCNT+1
                  ELSE
                  END IF
              END DO
              IF(SPIDCNT.EQ.0) THEN
                  WRITE(OUTLYNE,110)
                  CALL SHOWIT(0)
                  RETURN
              END IF
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,3000)
              CALL SHOWIT(0)
              DO SURF=0,INT(SYSTEM1(20))
C
                  IF(ALENS(134,SURF).NE.0.0D0) THEN
                      SPID=1
                  ELSE
                      SPID=0
                  END IF
                  IF(SPID.EQ.0) GO TO 15
C
C
C       FIRST PRINT SPIDER DATA.
C
C                       DO PRINTING NOW
C*****************************************************************
C
                  WRITE(OUTLYNE,200) SURF,
     1            INT(ALENS(135,SURF)),ALENS(136,SURF),ALENS(137,SURF)
                  CALL SHOWIT(0)
 15               CONTINUE
              END DO
C
          END IF
C
 200      FORMAT(I3,2X,I4,3X,G15.8,1X,G15.8)
C
 100      FORMAT('SURF',1X,I3,1X,
     1    ':NO SPIDER EXISTS')
 110      FORMAT('NO SPIDER EXISTS')
 1000     FORMAT(
     1    'SPIDER DATA')
 3000     FORMAT('SURF',4X,'N',8X,'W',15X,'L')
 1500     FORMAT(1X)
          RETURN
      END
C SUB SSPTWT.FOR
      SUBROUTINE SSPTWT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SSPTWT WHICH IMPLEMENTS THE SPTWT
C       COMMAND AT THE CMD LEVEL.
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
          IF(WC.EQ.'SPTWT') THEN
C
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),
     1            SYSTEM1(4),SYSTEM1(5)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) SYSTEM1(31),SYSTEM1(32),SYSTEM1(33),
     1            SYSTEM1(34),SYSTEM1(35)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) SYSTEM1(71),SYSTEM1(72),SYSTEM1(73),
     1            SYSTEM1(74),SYSTEM1(75)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) SYSTEM1(76),SYSTEM1(77),SYSTEM1(78),
     1            SYSTEM1(79),SYSTEM1(80)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C       NOT STI
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE='"SPTWT" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1
     1        .AND.DF5.EQ.1) THEN
C       SPTWT TYPED IN WITH NO INPUT SO PRINT OUT CURRENT VALUES
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),
     1            SYSTEM1(4),SYSTEM1(5)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) SYSTEM1(31),SYSTEM1(32),SYSTEM1(33),
     1            SYSTEM1(34),SYSTEM1(35)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) SYSTEM1(71),SYSTEM1(72),SYSTEM1(73),
     1            SYSTEM1(74),SYSTEM1(75)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) SYSTEM1(76),SYSTEM1(77),SYSTEM1(78),
     1            SYSTEM1(79),SYSTEM1(80)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C       ALL NUMERIC INPUT NOT DEFAULT, IF SOME ARE, THEN LEAVE
C       THE ASSOCIATED VALUES AS THEY WERE BEFORE.
C       SET VALUES
C
                  IF(DF1.EQ.1) W1=SYSTEM1(31)
                  IF(DF2.EQ.1) W2=SYSTEM1(32)
                  IF(DF3.EQ.1) W3=SYSTEM1(33)
                  IF(DF4.EQ.1) W4=SYSTEM1(34)
                  IF(DF5.EQ.1) W5=SYSTEM1(35)
                  IF(DF1.EQ.1) DF1=0
                  IF(DF2.EQ.1) DF2=0
                  IF(DF3.EQ.1) DF3=0
                  IF(DF4.EQ.1) DF4=0
                  IF(DF5.EQ.1) DF5=0
              END IF
              IF(W1.LT.0.0D0.OR.W2.LT.0.0D0.OR.W3.LT.0.0D0.OR.
     1        W4.LT.0.0D0.OR.W5.LT.0.0D0) THEN
                  OUTLYNE='SPECTRAL WEIGHTS MUST BE GREATER THAN ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  SYSTEM1(31)=W1
              ELSE
                  IF(SYSTEM1(31).NE.0.0D0) THEN
                      SYSTEM1(31)=0.0D0
                      OUTLYNE=
     1                'WAVELENGTH #1 WAS 0.0 SO SPECTRAL WEIGHT #1 SET TO 0.0 ALSO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  SYSTEM1(32)=W2
              ELSE
                  IF(SYSTEM1(32).NE.0.0D0) THEN
                      SYSTEM1(32)=0.0D0
                      OUTLYNE=
     1                'WAVELENGTH #2 WAS 0.0 SO SPECTRAL WEIGHT #2 SET TO 0.0 ALSO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  SYSTEM1(33)=W3
              ELSE
                  IF(SYSTEM1(33).NE.0.0D0) THEN
                      SYSTEM1(33)=0.0D0
                      OUTLYNE=
     1                'WAVELENGTH #3 WAS 0.0 SO SPECTRAL WEIGHT #3 SET TO 0.0 ALSO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  SYSTEM1(34)=W4
              ELSE
                  IF(SYSTEM1(34).NE.0.0D0) THEN
                      SYSTEM1(34)=0.0D0
                      OUTLYNE=
     1                'WAVELENGTH #4 WAS 0.0 SO SPECTRAL WEIGHT #4 SET TO 0.0 ALSO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  SYSTEM1(35)=W5
              ELSE
                  IF(SYSTEM1(35).NE.0.0D0) THEN
                      SYSTEM1(35)=0.0D0
                      OUTLYNE=
     1                'WAVELENGTH #5 WAS 0.0 SO SPECTRAL WEIGHT #5 SET TO 0.0 ALSO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(F12.EQ.1) THEN
C       UPDATE PERMANENT FILES AS WELL
                  SYSP(31)=SYSTEM1(31)
                  SYSP(32)=SYSTEM1(32)
                  SYSP(33)=SYSTEM1(33)
                  SYSP(34)=SYSTEM1(34)
                  SYSP(35)=SYSTEM1(35)
              ELSE
C       DON'T
              END IF
              RETURN
          ELSE
C     NOT SPTWT
          END IF
          IF(WC.EQ.'SPTWT2') THEN
C
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),
     1            SYSTEM1(4),SYSTEM1(5)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) SYSTEM1(31),SYSTEM1(32),SYSTEM1(33),
     1            SYSTEM1(34),SYSTEM1(35)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) SYSTEM1(71),SYSTEM1(72),SYSTEM1(73),
     1            SYSTEM1(74),SYSTEM1(75)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) SYSTEM1(76),SYSTEM1(77),SYSTEM1(78),
     1            SYSTEM1(79),SYSTEM1(80)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C       NOT STI
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE='"SPTWT2" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1
     1        .AND.DF5.EQ.1) THEN
C       SPTWT2 TYPED IN WITH NO INPUT SO PRINT OUT CURRENT VALUES
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),
     1            SYSTEM1(4),SYSTEM1(5)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) SYSTEM1(31),SYSTEM1(32),SYSTEM1(33),
     1            SYSTEM1(34),SYSTEM1(35)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) SYSTEM1(71),SYSTEM1(72),SYSTEM1(73),
     1            SYSTEM1(74),SYSTEM1(75)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) SYSTEM1(76),SYSTEM1(77),SYSTEM1(78),
     1            SYSTEM1(79),SYSTEM1(80)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C       ALL NUMERIC INPUT NOT DEFAULT, IF SOME ARE, THEN LEAVE
C       THE ASSOCIATED VALUES AS THEY WERE BEFORE.
C       SET VALUES
C
                  IF(DF1.EQ.1) W1=SYSTEM1(76)
                  IF(DF2.EQ.1) W2=SYSTEM1(77)
                  IF(DF3.EQ.1) W3=SYSTEM1(78)
                  IF(DF4.EQ.1) W4=SYSTEM1(79)
                  IF(DF5.EQ.1) W5=SYSTEM1(80)
                  IF(DF1.EQ.1) DF1=0
                  IF(DF2.EQ.1) DF2=0
                  IF(DF3.EQ.1) DF3=0
                  IF(DF4.EQ.1) DF4=0
                  IF(DF5.EQ.1) DF5=0
              END IF
              IF(W1.LT.0.0D0.OR.W2.LT.0.0D0.OR.W3.LT.0.0D0.OR.
     1        W4.LT.0.0D0.OR.W5.LT.0.0D0) THEN
                  OUTLYNE='SPECTRAL WEIGHTS MUST BE GREATER THAN ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  SYSTEM1(76)=W1
              ELSE
                  IF(SYSTEM1(76).NE.0.0D0) THEN
                      SYSTEM1(76)=0.0D0
                      OUTLYNE=
     1                'WAVELENGTH #6 WAS 0.0 SO SPECTRAL WEIGHT #6 SET TO 0.0 ALSO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  SYSTEM1(77)=W2
              ELSE
                  IF(SYSTEM1(77).NE.0.0D0) THEN
                      SYSTEM1(77)=0.0D0
                      OUTLYNE=
     1                'WAVELENGTH #7 WAS 0.0 SO SPECTRAL WEIGHT #7 SET TO 0.0 ALSO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  SYSTEM1(78)=W3
              ELSE
                  IF(SYSTEM1(78).NE.0.0D0) THEN
                      SYSTEM1(78)=0.0D0
                      OUTLYNE=
     1                'WAVELENGTH #8 WAS 0.0 SO SPECTRAL WEIGHT #8 SET TO 0.0 ALSO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  SYSTEM1(79)=W4
              ELSE
                  IF(SYSTEM1(79).NE.0.0D0) THEN
                      SYSTEM1(79)=0.0D0
                      OUTLYNE=
     1                'WAVELENGTH #9 WAS 0.0 SO SPECTRAL WEIGHT #9 SET TO 0.0 ALSO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  SYSTEM1(80)=W5
              ELSE
                  IF(SYSTEM1(80).NE.0.0D0) THEN
                      SYSTEM1(80)=0.0D0
                      OUTLYNE=
     1                'WAVELENGTH #10 WAS 0.0 SO SPECTRAL WEIGHT #10 SET TO 0.0 ALSO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  END IF
              END IF
              IF(F12.EQ.1) THEN
C       UPDATE PERMANENT FILES AS WELL
                  SYSP(76)=SYSTEM1(76)
                  SYSP(77)=SYSTEM1(77)
                  SYSP(78)=SYSTEM1(78)
                  SYSP(79)=SYSTEM1(79)
                  SYSP(80)=SYSTEM1(80)
              ELSE
C       DON'T
              END IF
              RETURN
          ELSE
C     NOT SPTWT
          END IF
 1000     FORMAT('WAVL NBR',10X,'1',12X,'2',12X,'3',12X
     1    ,'4',12X,'5')
 1001     FORMAT('WAVL NBR',10X,'6',12X,'7',12X,'8',12X
     1    ,'9',12X,'10')
 2000     FORMAT('WAVELENGTH',3X,G12.5,1X,G12.5,1X,G12.5,1X,
     1    G12.5,1X,G12.5)
 3000     FORMAT('SPECTRAL WT',2X,G11.4,2X,G11.4,2X,G11.4,2X,
     1    G11.4,2X,G11.4)
      END

C SUB SSPC.FOR
      SUBROUTINE SSPC
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SSPC WHICH IMPLEMENTS THE SPC
C       COMMAND AT THE CMD LEVEL. THIS COMMAND IS NOT
C       THE SAME AS HEXAGONS. THERE IS NO NUMERIC,QUALIFIER
C       OR STRING INPUT USED. ALL ASI,FNBY,FNBX,ERY,ERX AND
C       NORMAL STATUS IS LISTED FOR THE CURRENT LENS
C
!        INTEGER ASICNT,I,NRCNT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"SPC" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       WHAT IF NO SURFACES EXIST
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='SPECIAL CONDITIONS NOT DEFINED'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       PRINT SPECIAL CONDITION HEADER
          WRITE(OUTLYNE,999)
          CALL SHOWIT(0)
C
C       NOW DO THE FNBY AND FNBX
          IF(SYSTEM1(44).EQ.-1.0) THEN
C       THERE IS FNBY HOLD ASSIGNMENT
              WRITE(OUTLYNE,601) SYSTEM1(46)
              CALL SHOWIT(0)
          END IF
          IF(SYSTEM1(45).EQ.-1.0) THEN
C       THERE IS FNBX HOLD ASSIGNMENT
              WRITE(OUTLYNE,701) SYSTEM1(47)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,7000)
              CALL SHOWIT(0)
          END IF
C
C       NOW DO THE ERY AND ERX
          IF(SYSTEM1(44).EQ.-2.0) THEN
C       THERE IS ERY HOLD ASSIGNMENT
              WRITE(OUTLYNE,801) SYSTEM1(46)
              CALL SHOWIT(0)
          END IF
          IF(SYSTEM1(45).EQ.-2.0) THEN
C       THERE IS ERX HOLD ASSIGNMENT
              WRITE(OUTLYNE,901) SYSTEM1(47)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,9000)
              CALL SHOWIT(0)
          END IF
C
!  100    FORMAT('NO ALTERNATE SURFACE INTERSECTION IN CURRENT LENS')
!  101    FORMAT('NO NON-REGULAR SURFACE NORMAL DEFINITIONS')
!  102    FORMAT('IN CURRENT LENS')
!  200    FORMAT('ALTERNATE SURFACE INTERSECTION ON SURFACE',I3)
!  201 FORMAT('"REVERSE"  SURFACE NORMAL DEFINITION ON SURFACE',I3)
!  202 FORMAT('"POSITIVE" SURFACE NORMAL DEFINITION ON SURFACE',I3)
!  203 FORMAT('"NEGATIVE" SURFACE NORMAL DEFINITION ON SURFACE',I3)
  601     FORMAT('FNBY HLD =',G12.5)
  701     FORMAT('FNBX HLD =',G12.5)
  801     FORMAT('ERY HLD =',G12.5)
  901     FORMAT('ERX HLD =',G12.5)
 7000     FORMAT('"FNBX" ADJUSTMENTS ONLY ACTIVE WITH ANAMORPHIC',
     1    1X,'SYSTEMS')
 9000     FORMAT('"ERX" ADJUSTMENTS ONLY ACTIVE WITH ANAMORPHIC',
     1    1X,'SYSTEMS')
C
 999      FORMAT('SPECIAL CONDITIONS')
          RETURN
      END
C SUB SSC.FOR
      SUBROUTINE SSC
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SSC WHICH IMPLEMENTS THE SCY/SCX
C       COMMAND AT THE LENS OF UPDATE LENS LEVEL OR THE SCY/SCX
C       COMMAND AT THE CMD LEVEL. A QUALIFIER "FANG" IS ALLOWED
C       WHICH CAUSES DATA INPUT TO BE READ AS ANGULAR IN FRACTIONAL
C       DEGREES AND CAUSES SYSTEM1(18) OR SYSTEM1(19)
C       TO BE SET TO 1.0 FROM ITS
C       DEFAULT 0.0 VALUE
C       FOR SCY OR SCX
C       IF Y1 IS EXPLICITLY SET SYSTEM1(51) =1
C       IF X1 IS EXPLICITLY SET SYSTEM1(52) =1
C       FOR SCY FANG OR SCX FANG
C       IF Y1 IS EXPLICITLY SET SYSTEM1(53) =1
C       IF X1 IS EXPLICITLY SET SYSTEM1(54) =1
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.AND.WQ.NE.'FANG') THEN
              OUTLYNE=
     1        'INVALID QUALIFIER WORD USED WITH "'//WC(1:3)//'" COMMAND.'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.AND.F1.EQ.1) THEN
              OUTLYNE='AT THE CMD LEVEL, "SCY" AND "SCX"'
              CALL SHOWIT(1)
              OUTLYNE='OR "SCY FANG" AND "SCX FANG"'
              CALL SHOWIT(1)
              OUTLYNE='TAKE NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F1.EQ.1) THEN
              IF(WQ.NE.'FANG') THEN
                  IF(WC.EQ.'SCY') THEN
                      WRITE(OUTLYNE,2002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2000) SYSTEM1(14),SYSTEM1(15)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(18).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY OBJECT ANGLE'
                      IF(SYSTEM1(18).EQ.0.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY OBJECT HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(WC.EQ.'SCX') THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,4000) SYSTEM1(16),SYSTEM1(17)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(19).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY OBJECT ANGLE'
                      IF(SYSTEM1(19).EQ.0.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY OBJECT HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
              ELSE
              END IF
              IF(WQ.EQ.'FANG') THEN
                  IF(WC.EQ.'SCY') THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3000) SYSTEM1(21),SYSTEM1(22)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(18).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY OBJECT ANGLE'
                      IF(SYSTEM1(18).EQ.0.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY OBJECT HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(WC.EQ.'SCX') THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,5000) SYSTEM1(23),SYSTEM1(24)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(19).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY OBJECT ANGLE'
                      IF(SYSTEM1(19).EQ.0.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY OBJECT HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
              ELSE
              END IF
              RETURN
          ELSE
C               NOT AT CMD LEVEL
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
                  IF(WC.EQ.'SCY'.AND.WQ.EQ.'FANG'.AND.
     1            DABS(ALENS(3,NEWOBJ)).LE.1.0D10) THEN
                      OUTLYNE='"SCY FANG" REQUIRES AN OBJECT DISTANCE'
                      CALL SHOWIT(1)
                      OUTLYNE='TO BE GREATER THAN 1.0D10 LENS UNITS IN MAGNITUDE'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'SCX'.AND.WQ.EQ.'FANG'.AND.
     1            DABS(ALENS(3,NEWOBJ)).LE.1.0D10) THEN
                      OUTLYNE='"SCX FANG" REQUIRES AN OBJECT DISTANCE'
                      CALL SHOWIT(1)
                      OUTLYNE='TO BE GREATER THAN 1.0D10 LENS UNITS IN MAGNITUDE'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'SCY') THEN
                      OUTLYNE='"SCY AND SCY FANG" TAKE NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='OR NUMERIC WORD #3, #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'SCX') THEN
                      OUTLYNE='"SCX AND SCX FANG" TAKE NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='OR NUMERIC WORD #3, #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DF1.EQ.1) THEN
C
                  IF(WC.EQ.'SCY') THEN
                      OUTLYNE=
     1                '"SCY AND SCY FANG" REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='AND ACCEPT OPTIONAL NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'SCX') THEN
                      OUTLYNE=
     1                '"SCX AND SCX FANG" REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='AND ACCEPT OPTIONAL NUMERIC WORD #2 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              IF(WQ.EQ.'FANG') THEN
C       MAKE SURE YOU DON'T STEP ON AN ENTRANCE PUPIL ADJUSTMENT
                  IF(SYSTEM1(26).NE.-99.0D0) THEN
C       THERE IS AN ASTOP
C       IS THERE AN EN ADJUSTMENT
                      IF(SYSTEM1(27).EQ.1.0D0.OR.SYSTEM1(27).EQ.2.0D0) THEN
C       YES EN ADJUSTMENT, DON'T ALLOW FANG ENTRIES
                          IF(WC.EQ.'SCY') THEN
                              OUTLYNE='AN ASTOP (EN) ADJUSTMENT IS IN EFFECT'
                              CALL SHOWIT(1)
                              OUTLYNE='"SCY FANG" INPUT IS NOT ALLOWED'
                              CALL SHOWIT(1)
                              OUTLYNE='RE-ENTER OBJECT DATA WITH AN "SCY" VALUE'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          IF(WC.EQ.'SCX') THEN
                              OUTLYNE='AN ASTOP (EN) ADJUSTMENT IS IN EFFECT'
                              CALL SHOWIT(1)
                              OUTLYNE='"SCX FANG" INPUT IS NOT ALLOWED'
                              CALL SHOWIT(1)
                              OUTLYNE='RE-ENTER OBJECT DATA WITH AN "SCX" VALUE'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          RETURN
                      END IF
                  END IF
C
C     ASSIGN VALUES
C
                  IF(WC.EQ.'SCY') SYSTEM1(18)=1.0D0
                  IF(WC.EQ.'SCX') SYSTEM1(19)=1.0D0
C       FANG IS USED HERE
                  IF(WC.EQ.'SCY') THEN
                      IF(DABS(W1).GE.90.0D0) THEN
                          OUTLYNE=
     1                    'THE MAGNITUDE OF THE "SCY FANG" VALUE'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'MUST BE LESS THAN 90 DEGREES'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      SYSTEM1(21)=(W1)
                      SYSTEM1(92:99)=0.0D0
                      IF(DF2.EQ.1) W2=0.0D0
                      IF(DF2.EQ.0) SYSTEM1(51)=1.0D0
                      IF(DF2.EQ.0) SYSTEM1(53)=1.0D0
                      SYSTEM1(22)=W2
                      SYSTEM1(15)=W2
                      IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
                          SYSTEM1(60)=1.0D0
                          SYSTEM1(61)=1.0D0
                      END IF
                  ELSE
                  END IF
                  IF(WC.EQ.'SCX') THEN
                      IF(DABS(W1).GE.90.0D0) THEN
                          OUTLYNE=
     1                    'THE MAGNITUDE OF THE "SCX FANG" VALUE'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'MUST BE LESS THAN 90 DEGREES'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      SYSTEM1(23)=(W1)
                      SYSTEM1(92:99)=0.0D0
                      IF(SYSTEM1(49).EQ.0.0D0) SYSTEM1(49)=2.0D0
                      IF(SYSTEM1(49).EQ.1.0D0) SYSTEM1(49)=3.0D0
                      IF(DF2.EQ.1) W2=0.0D0
                      IF(DF2.EQ.0) SYSTEM1(52)=1.0D0
                      IF(DF2.EQ.0) SYSTEM1(54)=1.0D0
                      SYSTEM1(24)=W2
                      SYSTEM1(17)=W2
                      IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
                          SYSTEM1(60)=1.0D0
                          SYSTEM1(61)=1.0D0
                      END IF
                  ELSE
                  END IF
              ELSE
C     NOT FANG
                  IF(WC.EQ.'SCY') THEN
                      SYSTEM1(18)=0.0D0
                      IF(DF2.EQ.1) W2=0.0D0
                      IF(DF2.EQ.0) SYSTEM1(51)=1.0D0
                      IF(DF2.EQ.0) SYSTEM1(53)=1.0D0
                      SYSTEM1(14)=W1
                      SYSTEM1(92:99)=0.0D0
                      SYSTEM1(15)=W2
                      SYSTEM1(22)=W2
                      IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
                          SYSTEM1(60)=1.0D0
                          SYSTEM1(61)=1.0D0
                      END IF
                  ELSE
                  END IF
                  IF(WC.EQ.'SCX') THEN
                      SYSTEM1(19)=0.0D0
                      IF(DF2.EQ.1) W2=0.0D0
                      IF(DF2.EQ.0) SYSTEM1(52)=1.0D0
                      IF(DF2.EQ.0) SYSTEM1(54)=1.0D0
                      SYSTEM1(16)=W1
                      SYSTEM1(92:99)=0.0D0
                      IF(SYSTEM1(49).EQ.0.0D0) SYSTEM1(49)=2.0D0
                      IF(SYSTEM1(49).EQ.1.0D0) SYSTEM1(49)=3.0D0
                      SYSTEM1(17)=W2
                      SYSTEM1(24)=W2
                      IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
                          SYSTEM1(60)=1.0D0
                          SYSTEM1(61)=1.0D0
                      END IF
                  ELSE
                  END IF
              END IF
C     GET RID OF REVRAY SETTING IF IN EFFECT
              IF(SYSTEM1(100).NE.0.0D0) THEN
                  SYSTEM1(100)=0.0D0
                  OUTLYNE='"REVRAY" SETTING HAS BEEN AUTOMATICALLY CANCELLED'
                  CALL SHOWIT(1)
              END IF
              RETURN
          ELSE
          END IF
  100     FORMAT(1X)
 2002     FORMAT('REFERENCE OBJECT HEIGHT')
 3002     FORMAT('REFERENCE OBJECT ANGLE')
 2000     FORMAT
     1    ('SCY(YZ-PLANE IN LENS UNITS)=',1X,D23.15,1X,',',1X,D23.15)
 4000     FORMAT
     1    ('SCX(XZ-PLANE IN LENS UNITS)=',1X,D23.15,1X,',',1X,D23.15)
 3000     FORMAT
     1    ('SCY FANG(YZ-PLANE IN DEGREES)=',1X,D23.15,1X,',',1X,D23.15)
 5000     FORMAT
     1    ('SCX FANG(XZ-PLANE IN DEGREES)=',1X,D23.15,1X,',',1X,D23.15)
          RETURN
      END



      SUBROUTINE PXYIM
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.AND.WQ.NE.'FANG') THEN
              OUTLYNE=
     1        'INVALID QUALIFIER WORD USED WITH "'//WC(1:4)//'" COMMAND.'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.AND.F1.EQ.1) THEN
              OUTLYNE='AT THE CMD LEVEL, "PXIM" AND "PYIM"'
              CALL SHOWIT(1)
              OUTLYNE='OR "PXIM FANG" AND "PYIM FANG"'
              CALL SHOWIT(1)
              OUTLYNE='TAKE NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F1.EQ.1) THEN
              IF(WC.EQ.'PXIM'.AND.SYSTEM1(94).EQ.0.0D0) THEN
                  OUTLYNE=
     1            'REF. OBJ. HT. NOT SET BY PARAXIAL IMAGE SPACE HT OR ANGLE'
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(WC.EQ.'PYIM'.AND.SYSTEM1(95).EQ.0.0D0) THEN
                  OUTLYNE=
     1            'REF. OBJ. HT. NOT SET BY PARAXIAL IMAGE SPACE HT OR ANGLE'
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(WQ.NE.'FANG') THEN
                  IF(WC.EQ.'PYIM'.AND.SYSTEM1(95).NE.0.0D0) THEN
                      WRITE(OUTLYNE,2002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2000) SYSTEM1(93)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(94).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY PARAXIAL IMAGE SPACE ANGLE'
                      IF(SYSTEM1(94).EQ.-1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY PARAXIAL IMAGE HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(WC.EQ.'PXIM'.AND.SYSTEM1(94).NE.0.0D0) THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,4000) SYSTEM1(92)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(95).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY PARAXIAL IMAGE SPACE ANGLE'
                      IF(SYSTEM1(95).EQ.-1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY PARAXIAL IMAGE HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
              ELSE
              END IF
              IF(WQ.EQ.'FANG') THEN
                  IF(WC.EQ.'PYIM'.AND.SYSTEM1(95).NE.0.0D0) THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3000) SYSTEM1(93)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(94).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY PARAXIAL IMAGE SPACE ANGLE'
                      IF(SYSTEM1(94).EQ.-1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY PARAXIAL IMAGE SPACE HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(WC.EQ.'PXIM'.AND.SYSTEM1(94).NE.0.0D0) THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,5000) SYSTEM1(92)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(95).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY PAXAXIAL IMAGE SPACE ANGLE'
                      IF(SYSTEM1(95).EQ.-1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY PARAXIAL IMAGE SPACE HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
              ELSE
              END IF
              RETURN
          ELSE
C               NOT AT CMD LEVEL
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
                  IF(WC.EQ.'PYIM') THEN
                      OUTLYNE='"PYIM AND PYIM FANG" TAKE NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='OR NUMERIC WORD #2, #3, #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PXIM') THEN
                      OUTLYNE='"PXIM AND PXIM FANG" TAKE NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='OR NUMERIC WORD #2, #3, #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DF1.EQ.1) THEN
C
                  IF(WC.EQ.'PYIM') THEN
                      OUTLYNE=
     1                '"PYIM AND PYIM FANG" REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'PXIM') THEN
                      OUTLYNE=
     1                '"PXIM AND PXIM FANG" REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              IF(WQ.EQ.'FANG') THEN
C       FANG IS USED HERE
                  IF(WC.EQ.'PYIM') THEN
                      IF(DABS(W1).GE.90.0D0) THEN
                          OUTLYNE=
     1                    'THE MAGNITUDE OF THE "PYIM FANG" VALUE'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'MUST BE LESS THAN 90 DEGREES'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      SYSTEM1(93)=(W1)
                      SYSTEM1(95)=1.0D0
                      SYSTEM1(96:99)=0.0D0
                      SYSTEM1(60)=1.0D0
                      SYSTEM1(61)=1.0D0
                  ELSE
                  END IF
                  IF(WC.EQ.'PXIM') THEN
                      IF(DABS(W1).GE.90.0D0) THEN
                          OUTLYNE=
     1                    'THE MAGNITUDE OF THE "PXIM FANG" VALUE'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'MUST BE LESS THAN 90 DEGREES'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      SYSTEM1(96:99)=0.0D0
                      SYSTEM1(92)=(W1)
                      SYSTEM1(94)=1.0D0
                      SYSTEM1(61)=1.0D0
                      SYSTEM1(60)=1.0D0
                      IF(SYSTEM1(49).EQ.0.0D0) SYSTEM1(49)=2.0D0
                      IF(SYSTEM1(49).EQ.1.0D0) SYSTEM1(49)=3.0D0
                  ELSE
                  END IF
              ELSE
C     NOT FANG
                  IF(WC.EQ.'PXIM') THEN
                      SYSTEM1(94)=-1.0D0
                      SYSTEM1(92)=W1
                      SYSTEM1(96:99)=0.0D0
                      IF(SYSTEM1(49).EQ.0.0D0) SYSTEM1(49)=2.0D0
                      IF(SYSTEM1(49).EQ.1.0D0) SYSTEM1(49)=3.0D0
                      SYSTEM1(61)=1.0D0
                      SYSTEM1(60)=1.0D0
                  ELSE
                  END IF
                  IF(WC.EQ.'PYIM') THEN
                      SYSTEM1(95)=-1.0D0
                      SYSTEM1(93)=W1
                      SYSTEM1(96:99)=0.0D0
                      SYSTEM1(60)=1.0D0
                      SYSTEM1(61)=1.0D0
                  ELSE
                  END IF
              END IF
C     GET RID OF REVRAY SETTING IF IN EFFECT
              IF(SYSTEM1(100).NE.0.0D0) THEN
                  SYSTEM1(100)=0.0D0
                  OUTLYNE='"REVRAY" SETTING HAS BEEN AUTOMATICALLY CANCELLED'
                  CALL SHOWIT(1)
              END IF
              IF(WC.EQ.'PYIM') THEN
                  IF(SYSTEM1(94).EQ.0.0D0) THEN
                      SYSTEM1(94)=SYSTEM1(95)
                      SYSTEM1(92)=SYSTEM1(93)
                  ELSE
                      SYSTEM1(94)=SYSTEM1(95)
                  END IF
              END IF
              IF(WC.EQ.'PXIM') THEN
                  IF(SYSTEM1(95).EQ.0.0D0) THEN
                      SYSTEM1(95)=SYSTEM1(94)
                      SYSTEM1(93)=SYSTEM1(92)
                  ELSE
                      SYSTEM1(95)=SYSTEM1(94)
                  END IF
              END IF
              RETURN
          ELSE
          END IF
  100     FORMAT(1X)
 2002     FORMAT('PARAXIAL REFERENCE IMAGE HEIGHT')
 3002     FORMAT('PARAXIAL REFERENCE IMAGE ANGLE')
 2000     FORMAT
     1    ('PYIM(YZ-PLANE IN LENS UNITS)=',1X,D23.15)
 4000     FORMAT
     1    ('PXIM(XZ-PLANE IN LENS UNITS)=',1X,D23.15)
 3000     FORMAT
     1    ('PYIM FANG(YZ-PLANE IN DEGREES)=',1X,D23.15)
 5000     FORMAT
     1    ('PXIM FANG(XZ-PLANE IN DEGREES)=',1X,D23.15)
          RETURN
      END
      SUBROUTINE RXYIM
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.AND.WQ.NE.'FANG') THEN
              OUTLYNE=
     1        'INVALID QUALIFIER WORD USED WITH "'//WC(1:4)//'" COMMAND.'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.AND.F1.EQ.1) THEN
              OUTLYNE='AT THE CMD LEVEL, "RXIM" AND "RYIM"'
              CALL SHOWIT(1)
              OUTLYNE='OR "RXIM FANG" AND "RYIM FANG"'
              CALL SHOWIT(1)
              OUTLYNE='TAKE NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F1.EQ.1) THEN
              IF(WC.EQ.'RXIM'.AND.SYSTEM1(98).EQ.0.0D0) THEN
                  OUTLYNE=
     1            'REF. OBJ. HT. NOT SET BY REAL IMAGE SPACE HT OR ANGLE'
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(WC.EQ.'RYIM'.AND.SYSTEM1(99).EQ.0.0D0) THEN
                  OUTLYNE=
     1            'REF. OBJ. HT. NOT SET BY REAL IMAGE SPACE HT OR ANGLE'
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(WQ.NE.'FANG') THEN
                  IF(WC.EQ.'RYIM'.AND.SYSTEM1(99).NE.0.0D0) THEN
                      WRITE(OUTLYNE,2002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2000) SYSTEM1(97)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(98).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY REAL IMAGE SPACE ANGLE'
                      IF(SYSTEM1(98).EQ.-1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY REAL IMAGE HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(WC.EQ.'RXIM'.AND.SYSTEM1(98).NE.0.0D0) THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,2002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,4000) SYSTEM1(96)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(99).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY REAL IMAGE SPACE ANGLE'
                      IF(SYSTEM1(99).EQ.-1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY REAL IMAGE HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
              ELSE
              END IF
              IF(WQ.EQ.'FANG') THEN
                  IF(WC.EQ.'RYIM'.AND.SYSTEM1(99).NE.0.0D0) THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3000) SYSTEM1(97)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(98).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY REAL IMAGE SPACE ANGLE'
                      IF(SYSTEM1(98).EQ.-1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY REAL IMAGE SPACE HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
                  IF(WC.EQ.'RXIM'.AND.SYSTEM1(98).NE.0.0D0) THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,3002)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,5000) SYSTEM1(96)
                      CALL SHOWIT(0)
                      IF(SYSTEM1(99).EQ.1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY REAL IMAGE SPACE ANGLE'
                      IF(SYSTEM1(95).EQ.-1.0D0)
     1                OUTLYNE='REF. OBJ. HT. SET BY REAL IMAGE SPACE HEIGHT'
                      CALL SHOWIT(0)
                  ELSE
                  END IF
              ELSE
              END IF
              RETURN
          ELSE
C               NOT AT CMD LEVEL
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
                  IF(WC.EQ.'RYIM') THEN
                      OUTLYNE='"RYIM AND RYIM FANG" TAKE NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='OR NUMERIC WORD #2, #3, #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'RXIM') THEN
                      OUTLYNE='"RXIM AND RXIM FANG" TAKE NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='OR NUMERIC WORD #2, #3, #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DF1.EQ.1) THEN
C
                  IF(WC.EQ.'RYIM') THEN
                      OUTLYNE=
     1                '"RYIM AND RYIM FANG" REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'RXIM') THEN
                      OUTLYNE=
     1                '"RXIM AND RXIM FANG" REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              IF(WQ.EQ.'FANG') THEN
C       FANG IS USED HERE
                  IF(WC.EQ.'RYIM') THEN
                      IF(DABS(W1).GE.90.0D0) THEN
                          OUTLYNE=
     1                    'THE MAGNITUDE OF THE "RYIM FANG" VALUE'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'MUST BE LESS THAN 90 DEGREES'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      SYSTEM1(97)=(W1)
                      SYSTEM1(92:95)=0.0D0
                      SYSTEM1(99)=1.0D0
                      SYSTEM1(60)=1.0D0
                      SYSTEM1(61)=1.0D0
                  ELSE
                  END IF
                  IF(WC.EQ.'RXIM') THEN
                      IF(DABS(W1).GE.90.0D0) THEN
                          OUTLYNE=
     1                    'THE MAGNITUDE OF THE "RXIM FANG" VALUE'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'MUST BE LESS THAN 90 DEGREES'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      SYSTEM1(96)=(W1)
                      SYSTEM1(92:95)=0.0D0
                      IF(SYSTEM1(49).EQ.0.0D0) SYSTEM1(49)=2.0D0
                      IF(SYSTEM1(49).EQ.1.0D0) SYSTEM1(49)=3.0D0
                      SYSTEM1(98)=1.0D0
                      SYSTEM1(61)=1.0D0
                      SYSTEM1(60)=1.0D0
                  ELSE
                  END IF
              ELSE
C     NOT FANG
                  IF(WC.EQ.'RXIM') THEN
                      SYSTEM1(98)=-1.0D0
                      SYSTEM1(96)=W1
                      SYSTEM1(92:95)=0.0D0
                      IF(SYSTEM1(49).EQ.0.0D0) SYSTEM1(49)=2.0D0
                      IF(SYSTEM1(49).EQ.1.0D0) SYSTEM1(49)=3.0D0
                      SYSTEM1(61)=1.0D0
                      SYSTEM1(60)=1.0D0
                  ELSE
                  END IF
                  IF(WC.EQ.'RYIM') THEN
                      SYSTEM1(99)=-1.0D0
                      SYSTEM1(97)=W1
                      SYSTEM1(92:95)=0.0D0
                      SYSTEM1(60)=1.0D0
                      SYSTEM1(61)=1.0D0
                  ELSE
                  END IF
              END IF
              IF(WC.EQ.'RYIM') THEN
                  IF(SYSTEM1(98).EQ.0.0D0) THEN
                      SYSTEM1(98)=SYSTEM1(99)
                      SYSTEM1(96)=SYSTEM1(97)
                  ELSE
                      SYSTEM1(98)=SYSTEM1(99)
                  END IF
              END IF
              IF(WC.EQ.'RXIM') THEN
                  IF(SYSTEM1(99).EQ.0.0D0) THEN
                      SYSTEM1(99)=SYSTEM1(98)
                      SYSTEM1(97)=SYSTEM1(96)
                  ELSE
                      SYSTEM1(99)=SYSTEM1(98)
                  END IF
              END IF
              RETURN
          ELSE
          END IF
  100     FORMAT(1X)
 2002     FORMAT('REAL REFERENCE IMAGE HEIGHT')
 3002     FORMAT('REAL REFERENCE IMAGE ANGLE')
 2000     FORMAT
     1    ('RYIM(YZ-PLANE IN LENS UNITS)=',1X,D23.15)
 4000     FORMAT
     1    ('RXIM(XZ-PLANE IN LENS UNITS)=',1X,D23.15)
 3000     FORMAT
     1    ('RYIM FANG(YZ-PLANE IN DEGREES)=',1X,D23.15)
 5000     FORMAT
     1    ('RXIM FANG(XZ-PLANE IN DEGREES)=',1X,D23.15)
          RETURN
      END
C SUB SBD.FOR
      SUBROUTINE SBD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SBD WHICH IMPLEMENTS THE BDX,BDY COMMANDS
C       AT THE LENS OF UPDATE LENS LEVEL OR THE BDX,BDY COMMANDS AT
C       THE CMD LEVEL
C
          REAL*8 WAVER
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(F1.EQ.1) THEN
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='AT THE CMD LEVEL, "BDY" AND "BDX"'
                  CALL SHOWIT(1)
                  OUTLYNE='TAKE NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(WC.EQ.'BDY') THEN
                  WRITE(OUTLYNE,2002)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) SYSTEM1(88)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C     NOT BDY
              END IF
              IF(WC.EQ.'BDX') THEN
                  WRITE(OUTLYNE,2001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2002)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) SYSTEM1(87)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C     NOT BDX
              END IF
          ELSE
C               NOT AT CMD LEVEL
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1) THEN
C
                  IF(WC.EQ.'BDY') THEN
                      OUTLYNE='"BDY" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'BDX') THEN
                      OUTLYNE='"BDX" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1
     1        .OR.S4.EQ.1.OR.S5.EQ.1) THEN
C
                  IF(WC.EQ.'BDY') THEN
                      OUTLYNE=
     1                '"BDY" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'BDX') THEN
                      OUTLYNE=
     1                '"BDX" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'TEM00') THEN
C
                  IF(WC.EQ.'BDY') THEN
                      OUTLYNE=
     1                '"BDY" ONLY TAKES "TEM00" AS AN OPTIONAL QUALIFIER INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'BDX') THEN
                      OUTLYNE=
     1                '"BDX" ONLY TAKES "TEM00" AS AN OPTIONAL QUALIFIER INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     DEFAULT NW1
C
              IF(DF1.EQ.1.AND.SQ.EQ.0) THEN
C
                  IF(WC.EQ.'BDY') THEN
                      OUTLYNE='"BDY" WITH NO QUALIFIER WORD'
                      CALL SHOWIT(1)
                      OUTLYNE='"REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'BDX') THEN
                      OUTLYNE='"BDX" WITH NO QUALIFIER WORD'
                      CALL SHOWIT(1)
                      OUTLYNE='"REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DF1.EQ.1.AND.SQ.EQ.1) W1=SYSTEM1(11)
              IF(SQ.EQ.1) THEN
                  IF(W1.NE.1.0D0.AND.W1.NE.2.0D0.AND.W1.NE.3.0D0.AND.W1.NE.4.0D0
     1            .AND.W1.NE.5.0D0.AND.W1.NE.6.0D0.AND.W1.NE.7.0D0.AND.W1.NE.
     2            8.0D0.AND.W1.NE.9.0D0.AND.W1.NE.10.0D0) THEN
                      OUTLYNE='"BDX TEM00" AND "BDY TEM00"'
                      CALL SHOWIT(1)
                      OUTLYNE='"REQUIRE A WAVELENGTH NUMBER FROM 1 TO 10'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W1.EQ.1.0D0)  WAVER=SYSTEM1(1)
                  IF(W1.EQ.2.0D0)  WAVER=SYSTEM1(2)
                  IF(W1.EQ.3.0D0)  WAVER=SYSTEM1(3)
                  IF(W1.EQ.4.0D0)  WAVER=SYSTEM1(4)
                  IF(W1.EQ.5.0D0)  WAVER=SYSTEM1(5)
                  IF(W1.EQ.6.0D0)  WAVER=SYSTEM1(71)
                  IF(W1.EQ.7.0D0)  WAVER=SYSTEM1(72)
                  IF(W1.EQ.8.0D0)  WAVER=SYSTEM1(73)
                  IF(W1.EQ.9.0D0)  WAVER=SYSTEM1(74)
                  IF(W1.EQ.10.0D0) WAVER=SYSTEM1(75)
                  IF(SYSTEM1(6).EQ.1.0D0) WAVER=WAVER*(1.0D-3/25.4D0)
                  IF(SYSTEM1(6).EQ.2.0D0) WAVER=WAVER*1.0D-4
                  IF(SYSTEM1(6).EQ.3.0D0) WAVER=WAVER*1.0D-3
                  IF(SYSTEM1(6).EQ.4.0D0) WAVER=WAVER*1.0D-6
              END IF
C
              IF(SQ.EQ.0) THEN
                  IF(F6.EQ.1.AND.W1.LE.0.0D0.AND.WC.EQ.'BDX') THEN
                      OUTLYNE='"BDX" WITH NO QUALIFIER WORD'
                      CALL SHOWIT(1)
                      OUTLYNE='"REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(F6.EQ.1.AND.W1.LE.0.0D0.AND.WQ.EQ.'BDY') THEN
                      OUTLYNE='"BDY" WITH NO QUALIFIER WORD'
                      CALL SHOWIT(1)
                      OUTLYNE='"REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'BDX') SYSTEM1(87)=W1
                  IF(WC.EQ.'BDY') SYSTEM1(88)=W1
              END IF
              IF(SQ.EQ.1) THEN
                  IF(WC.EQ.'BDX') SYSTEM1(87)=1000.0D0*(WAVER)/(PII*SYSTEM1(85))
                  IF(WC.EQ.'BDY') SYSTEM1(88)=1000.0D0*(WAVER)/(PII*SYSTEM1(86))
              END IF
              RETURN
          END IF
 2001     FORMAT(1X)
 2002     FORMAT('HALF-ANGLE BEAM DIVERGENCE')
 2000     FORMAT('BDY(YZ-PLANE IN MRAD) = ',1X,D23.15)
 3000     FORMAT('BDX(XZ-PLANE IN MRAD) = ',1X,D23.15)
      END
C SUB SWR.FOR
      SUBROUTINE SWR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SWR WHICH IMPLEMENTS THE WRX,WRY COMMANDS
C       AT THE LENS OF UPDATE LENS LEVEL OR THE WRX,WRY COMMANDS AT
C       THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'

C
          IF(F1.EQ.1) THEN
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='AT THE CMD LEVEL, "WRY" AND "WRX"'
                  CALL SHOWIT(1)
                  OUTLYNE='TAKE NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(WC.EQ.'WRY') THEN
                  WRITE(OUTLYNE,2002)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) SYSTEM1(86)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C     NOT WRY
              END IF
              IF(WC.EQ.'WRX') THEN
                  WRITE(OUTLYNE,2001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2002)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) SYSTEM1(85)
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C     NOT WRX
              END IF
          ELSE
C               NOT AT CMD LEVEL
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1) THEN
C
                  IF(WC.EQ.'WRY') THEN
                      OUTLYNE='"WRY" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'WRX') THEN
                      OUTLYNE='"WRX" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1
     1        .OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
C
                  IF(WC.EQ.'WRY') THEN
                      OUTLYNE=
     1                '"WRY" TAKES NO QUALIFIER OR NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'WRX') THEN
                      OUTLYNE=
     1                '"WRX" TAKES NO QUALIFIER OR NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C     DEFAULT NW1
C
              IF(DF1.EQ.1) THEN
C
                  IF(WC.EQ.'WRY') THEN
                      OUTLYNE='"WRY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'WRX') THEN
                      OUTLYNE='"WRX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(W1.LE.0.0D0) THEN
                  IF(WC.EQ.'WRY') THEN
                      OUTLYNE='"WRY" MAY NOT BE SET TO LESS THAN ZERO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'WRX') THEN
                      OUTLYNE='"WRX" MAY NOT BE SET TO LESS THAN ZERO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'WRX') SYSTEM1(85)=W1
              IF(WC.EQ.'WRY') SYSTEM1(86)=W1
              RETURN
          END IF
 2001     FORMAT(1X)
 2002     FORMAT('1/e2 WAIST RADIUS')
 2000     FORMAT('WRY(YZ-PLANE IN LENS UNITS) = ',1X,D23.15)
 3000     FORMAT('WRX(XZ-PLANE IN LENS UNITS) = ',1X,D23.15)
      END
C SUB SSA.FOR
      SUBROUTINE SSA
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SSAY WHICH IMPLEMENTS THE SAY COMMAND
C       AT THE LENS OF UPDATE LENS LEVEL OR THE SAY COMMAND AT
C       THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(F1.EQ.1) THEN
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='AT THE CMD LEVEL, "SAY" AND "SAX"'
                  CALL SHOWIT(1)
                  OUTLYNE='TAKE NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(WC.EQ.'SAY') THEN
                  WRITE(OUTLYNE,2002)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) SYSTEM1(12)
                  CALL SHOWIT(0)
                  IF(SYSTEM1(64).NE.0.0D0) THEN
                      WRITE(OUTLYNE,2010)
                      CALL SHOWIT(0)
 2010                 FORMAT('"SAY" IS BEING HELD WITH AN "NAOY" ASSIGNMENT')
                  END IF
                  IF(SYSTEM1(67).NE.0.0D0) THEN
                      WRITE(OUTLYNE,2020)
                      CALL SHOWIT(0)
 2020                 FORMAT('"SAY" IS BEING HELD WITH AN "FNOY" ASSIGNMENT')
                  END IF
                  IF(SYSTEM1(83).NE.0.0D0) THEN
                      WRITE(OUTLYNE,2050)
                      CALL SHOWIT(0)
 2050                 FORMAT('"SAY" IS CURRENTLY FLOATING')
                  END IF
                  RETURN
              ELSE
C     NOT SAY
              END IF
              IF(WC.EQ.'SAX') THEN
                  WRITE(OUTLYNE,2001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2002)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) SYSTEM1(13)
                  CALL SHOWIT(0)
C
                  IF(SYSTEM1(64).NE.0.0D0) THEN
                      WRITE(OUTLYNE,2030)
                      CALL SHOWIT(0)
 2030                 FORMAT('"SAX" IS BEING HELD WITH AN "NAOX" ASSIGNMENT')
                  END IF
                  IF(SYSTEM1(67).NE.0.0D0) THEN
                      WRITE(OUTLYNE,2040)
                      CALL SHOWIT(0)
 2040                 FORMAT('"SAX" IS BEING HELD WITH AN "FNOX" ASSIGNMENT')
                  END IF
                  IF(SYSTEM1(84).NE.0.0D0) THEN
                      WRITE(OUTLYNE,2060)
                      CALL SHOWIT(0)
 2060                 FORMAT('"SAX" IS CURRENTLY FLOATING')
                  END IF
                  RETURN
              ELSE
C     NOT SAX
              END IF
          ELSE
C               NOT AT CMD LEVEL
          END IF
C
          IF(F5.EQ.1.OR.F6.EQ.1) THEN
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1) THEN
                  OUTLYNE='"'//WC(1:3)//'" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1
     1        .OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"'//WC(1:3)//
     1            '" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'FLOAT'.AND.SYSTEM1(44).NE.0.0D0.OR.WQ.EQ.'FLOAT'
     1        .AND.SYSTEM1(45).NE.0.0D0) THEN
                  OUTLYNE='"FLOAT" MAY NOT BE USED SINCE THERE ARE EXIT PUPIL'
                  CALL SHOWIT(1)
                  OUTLYNE='OR F-NUMBER HOLDS PRESENT'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(SQ.EQ.1.AND.F5.EQ.1.AND.WQ.NE.'FLOAT'.AND.WQ.NE.
     1        'NOFLOAT') THEN
C
C     CHECK FOR CLAP ON ASTOP IF FLOAT
                  IF(WQ.EQ.'FLOAT') THEN
                      IF(SYSTEM1(26).LE.0.0D0) THEN
C     NO ASTOP ASSIGNED
                          OUTLYNE='NO APERTURE STOP IS DEFINED, "FLOAT" MAY NOT BE USED'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(ALENS(9,INT(SYSTEM1(26))).EQ.0.0D0) THEN
                          OUTLYNE='NO CLEAR APERTURE EXISTS ON THE APERTURE STOP SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE='"FLOAT" MAY NOT BE USED'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
C     NO CLAP ON ASTOP, RETURN
                          RETURN
                      END IF
                  END IF
C
                  IF(WC.EQ.'SAY') THEN
                      OUTLYNE='"FLOAT" AND "NOFLOAT" ARE THE ONLY VALID QUALIFIER'
                      CALL SHOWIT(1)
                      OUTLYNE='WORDS FOR "SAY" IN LENS INPUT MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'SAX') THEN
                      OUTLYNE='"FLOAT" AND "NOFLOAT" ARE THE ONLY VALID QUALIFIER'
                      CALL SHOWIT(1)
                      OUTLYNE='WORDS FOR "SAX" IN LENS INPUT MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(F5.EQ.1) THEN
                  IF(WQ.NE.'        '.AND.
     1            WQ.NE.'FLOAT'.AND.WQ.NE.'NOFLOAT') THEN
C
                      IF(WC.EQ.'SAY') THEN
                          OUTLYNE='INVALID QUALIFIER WORD USED WITH "SAY"'
                          CALL SHOWIT(1)
                          OUTLYNE='AT THE LENS INPUT LEVEL'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(WC.EQ.'SAX') THEN
                          OUTLYNE='INVALID QUALIFIER WORD USED WITH "SAX"'
                          CALL SHOWIT(1)
                          OUTLYNE='AT THE LENS INPUT LEVEL'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
              END IF
              IF(F6.EQ.1) THEN
                  IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT'.AND.
     1            WQ.NE.'FLOAT'.AND.WQ.NE.'NOFLOAT') THEN
C
                      IF(WC.EQ.'SAY') THEN
                          OUTLYNE='INVALID QUALIFIER WORD USED WITH "SAY"'
                          CALL SHOWIT(1)
                          OUTLYNE='AT THE LENS UPDATE LEVEL'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(WC.EQ.'SAX') THEN
                          OUTLYNE='INVALID QUALIFIER WORD USED WITH "SAX"'
                          CALL SHOWIT(1)
                          OUTLYNE='AT THE LENS UPDATE LEVEL'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
              END IF
              IF(WQ.EQ.'FLOAT')   SYSTEM1(83)=1.0D0
              IF(WQ.EQ.'FLOAT')   SYSTEM1(84)=1.0D0
              IF(WQ.EQ.'NOFLOAT') SYSTEM1(83)=0.0D0
              IF(WQ.EQ.'NOFLOAT') SYSTEM1(84)=0.0D0
C     IF NUMERIC INPUT, SHUT OFF FLOAT
              IF(DF1.EQ.0.AND.WC.EQ.'SAY'.OR.
     1         DF1.EQ.0.AND.WC.EQ.'SAX') THEN
                  SYSTEM1(83)=0.0D0
                  SYSTEM1(84)=0.0D0
              END IF
C
              IF(DF1.EQ.1.AND.WQ.NE.'FLOAT'.AND.WQ.NE.'NOFLOAT') THEN
                  IF(WC.EQ.'SAY') THEN
                      OUTLYNE='"SAY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'SAX') THEN
                      OUTLYNE='"SAX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
C
              IF(W1.EQ.0.0.AND.WQ.NE.'FLOAT'.AND.WQ.NE.'NOFLOAT') THEN
                  IF(WC.EQ.'SAY') THEN
                      OUTLYNE='"SAY" MAY NOT BE SET TO ZERO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WC.EQ.'SAX') THEN
                      OUTLYNE='"SAX" MAY NOT BE SET TO ZERO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  SYSTEM1(64)=0.0D0
                  SYSTEM1(67)=0.0D0
                  IF(WC.EQ.'SAY') SYSTEM1(12)=(W1)
                  IF(WC.EQ.'SAX') SYSTEM1(13)=(W1)
C     IF NUMERIC INPUT, SHUT OFF FLOAT
                  IF(WC.EQ.'SAY') SYSTEM1(83)=0.0D0
                  IF(WC.EQ.'SAX') SYSTEM1(84)=0.0D0
                  IF(WC.EQ.'SAX') THEN
                      IF(SYSTEM1(49).EQ.0.0D0) SYSTEM1(49)=1.0D0
                      IF(SYSTEM1(49).EQ.2.0D0) SYSTEM1(49)=3.0D0
                  END IF
              END IF
              IF(WQ.EQ.'DELT') THEN
                  SYSTEM1(64)=0.0D0
                  SYSTEM1(67)=0.0D0
                  IF(WC.EQ.'SAY') SYSTEM1(12)=SYSTEM1(12)+(W1)
                  IF(WC.EQ.'SAX') SYSTEM1(13)=SYSTEM1(13)+(W1)
                  IF(WC.EQ.'SAY') SYSTEM1(83)=0.0D0
                  IF(WC.EQ.'SAX') SYSTEM1(84)=0.0D0
                  IF(WC.EQ.'SAX') THEN
                      IF(SYSTEM1(49).EQ.0.0D0) SYSTEM1(49)=1.0D0
                      IF(SYSTEM1(49).EQ.2.0D0) SYSTEM1(49)=3.0D0
                  END IF
              END IF
              IF(WQ.EQ.'CENT') THEN
                  SYSTEM1(64)=0.0D0
                  SYSTEM1(67)=0.0D0
                  IF(WC.EQ.'SAY') SYSTEM1(12)=SYSTEM1(12)+(W1*0.01D0*SYSTEM1(12))
                  IF(WC.EQ.'SAX') SYSTEM1(13)=SYSTEM1(13)+(W1*0.01D0*SYSTEM1(13))
                  IF(WC.EQ.'SAY') SYSTEM1(83)=0.0D0
                  IF(WC.EQ.'SAX') SYSTEM1(84)=0.0D0
                  IF(WC.EQ.'SAX') THEN
                      IF(SYSTEM1(49).EQ.0.0D0) SYSTEM1(49)=1.0D0
                      IF(SYSTEM1(49).EQ.2.0D0) SYSTEM1(49)=3.0D0
                  END IF
              END IF
              RETURN
          END IF
 2001     FORMAT(1X)
 2002     FORMAT('SEMI-APERTURE REFERENCE RAY HEIGHT AT SURFACE #1')
 2000     FORMAT('SAY(YZ-PLANE IN LENS UNITS) = ',1X,D23.15)
 3000     FORMAT('SAX(XZ-PLANE IN LENS UNITS) = ',1X,D23.15)
          RETURN
      END

C SUB SRTG.FOR
      SUBROUTINE SRTG(LBT,RTGERROR)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SRTG. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE RTG AND CTG (AND LBL) CMD LEVEL LENS OUTPUT COMMANDS.
C
          CHARACTER SPEC*1,SPECC*75
C
          INTEGER SF,I,NF
C
          REAL*8 RD,CV
     1    ,INDEX,DISP,VNUM,PARTL
C
          LOGICAL AIRY,LBT,SPECF,RTGERROR
C
          COMMON/NSIN/NF,INDEX,DISP,VNUM,PARTL
          integer idraw1
          COMMON/DRAWI1/IDRAW1
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       LENS DATA IS STORED IN C-T-G MODE. IF A CURVATURE
C       IS ZERO, AND RTG VICE CTG IS ISSUED THEN THE RADIUS
C       PRINTS OUT AS ZERO EVEN THOUGH IT IS REALLY INF.
C
C       THE RTG  AND CTG COMMANDS ACCEPT QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. THEY DO NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
          RTGERROR=.FALSE.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              OUTLYNE=
     1        '"'//WC(1:3)//'"'//
     1        ' TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RTGERROR=.TRUE.
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"'//WC(1:3)//'"'//
     1        ' TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RTGERROR=.TRUE.
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(20).EQ.0.0D0) THEN
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RTGERROR=.TRUE.
              CALL MACFAL
              RETURN
          END IF

          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'RTG'.OR.WC.EQ.'RTGLBL') THEN
                  WRITE(OUTLYNE,5001)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5000)
                  CALL SHOWIT(0)
              ELSE
              END IF
              IF(WC.EQ.'CTG'.OR.WC.EQ.'CTGLBL') THEN
                  WRITE(OUTLYNE,6001)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,6000)
                  CALL SHOWIT(0)
              ELSE
              END IF
              DO 10 I=0,INT(SYSTEM1(20))
                  NF=I
                  CALL SINDEX
                  IF(WC.EQ.'RTG'.OR.WC.EQ.'RTGLBL') THEN
                      IF(ALENS(1,I).EQ.0.0D0) THEN
                          RD=0.0D0
                      ELSE
                          RD=1.0D0/(ALENS(1,I))
                      END IF
                      SPEC=' '
                      SPECC=AA//AA//AA//'               '
                      SPECF=.TRUE.
C       CONIC
                      IF(ALENS(2,I).NE.0.0D0) SPEC='*'
C       ASPHERIC
                      IF(ALENS(8,I).NE.0.0D0) SPEC='*'
C       TORIC
                      IF(ALENS(23,I).NE.0.0D0) SPEC='*'
C       TILT
                      IF(ALENS(25,I).NE.0.0D0) SPEC='*'
C       DECENTER
                      IF(ALENS(29,I).NE.0.0D0) SPEC='*'
C       SPECIAL SURFACE (SPSRF TYPE)
                      IF(ALENS(34,I).NE.0.0D0) SPEC='*'
C       PIKUPS
                      IF(ALENS(32,I).NE.0.0D0) SPEC='*'
C       SOLVES
                      IF(ALENS(33,I).NE.0.0D0) SPEC='*'
C       REFS
                      IF(INT(SYSTEM1(25)).EQ.I) SPEC='*'
C       STOP
                      IF(INT(SYSTEM1(26)).EQ.I) SPEC='*'
C       GRATING
                      IF(ALENS(96,I).EQ.1.0D0) SPEC='*'
C       CONIC
                      IF(ALENS(2,I).NE.0.0D0) SPECC(1:5)='Conic'
C       ASPHERIC
                      IF(ALENS(8,I).NE.0.0D0) SPECC(7:13)='Asphere'
C       TORIC
                      IF(ALENS(23,I).NE.0.0D0) SPECC(15:19)='Toric'
C       TILT
                      IF(ALENS(25,I).NE.0.0D0) SPECC(21:24)='Tilt'
C       DECENTER
                      IF(ALENS(29,I).NE.0.0D0) SPECC(26:33)='Decenter'
C       SPECIAL SURFACE (SPSRF TYPE)
                      IF(ALENS(34,I).NE.0.0D0) SPECC(35:41)='Special'
C       PIKUPS
                      IF(ALENS(32,I).NE.0.0D0) SPECC(43:47)='Pikup'
C       SOLVES
                      IF(ALENS(33,I).NE.0.0D0) SPECC(49:53)='Solve'
C       REFS
                      IF(INT(SYSTEM1(25)).EQ.I) SPECC(55:58)='REFS'
C       STOP
                      IF(INT(SYSTEM1(26)).EQ.I) SPECC(60:63)='STOP'
C       GRT
                      IF(ALENS(96,I).EQ.1.0D0) SPECC(65:67)='GRT'
C       DEFORMABLE SURFACE
                      IF(ALENS(103,I).EQ.1.0D0) SPECC(69:71)='DEF'
C       NSS SURFACE
                      IF(ALENS(126,I).NE.0.0D0) SPECC(73:75)='NSS'
                      IF(SPECC(1:75).EQ.AA//AA//AA//'               ') SPECF=.FALSE.
C
                      IF(GLANAM(I,2).EQ.'LAST SURFACE') THEN
                          IF(GLANAM((I-1),2).EQ.'AIR'.OR.GLANAM((I-1),2).EQ.'REFLTIRO'.OR.
     1                    GLANAM((I-1),2).EQ.'REFLTIR'.OR.
     2                    GLANAM((I-1),2).EQ.'REFL') THEN
C
                              AIRY=.TRUE.
                              IF(DABS(ALENS(46,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(47,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(48,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(49,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(50,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(71,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(72,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(73,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(75,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(GLANAM(I-1,2).EQ.'REFL'.AND..NOT.AIRY.OR.
     1                        GLANAM(I-1,2).EQ.'REFLTIR'.AND..NOT.AIRY.OR.
     2                        GLANAM(I-1,2).EQ.'REFLTIRO'.AND..NOT.AIRY) THEN
                              ELSE
C      WRITE(OUTLYNE,2501)
C      CALL SHOWIT(0)
                              END IF
                              WRITE(OUTLYNE,2000)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                              CALL SHOWIT(0)
                              IF(SPECF) THEN
                                  CALL SHRINKSPEC(SPECC)
                                  WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                  CALL SHOWIT(0)
                              END IF
                              IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
 1001                             FORMAT(I3,1X,A74)
                                  WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                  CALL SHOWIT(0)
                              END IF
                          ELSE
                              WRITE(OUTLYNE,2500)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                              CALL SHOWIT(0)
                              IF(SPECF) THEN
                                  CALL SHRINKSPEC(SPECC)
                                  WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                  CALL SHOWIT(0)
                              END IF
                              IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                  WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                  CALL SHOWIT(0)
                              END IF
                          END IF
                      ELSE
                      END IF
C
                      IF(I.NE.0) THEN
                          IF(GLANAM(I,2).NE.'LAST SURFACE') THEN
                              IF(GLANAM(I,2).EQ.'AIR'.OR.GLANAM(I,2).EQ.'REFL'.OR.
     1                        GLANAM(I,2).EQ.'REFLTIR'.OR.
     2                        GLANAM(I,2).EQ.'REFLTIRO')THEN
                                  IF(GLANAM((I-1),2).EQ.'AIR'.OR.GLANAM((I-1),2).EQ.'REFL'.OR.
     1                            GLANAM((I-1),2).EQ.'REFLTIR'.OR.
     2                            GLANAM((I-1),2).EQ.'REFLTIRO') THEN
C
                                      AIRY=.TRUE.
                                      IF(DABS(ALENS(46,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(47,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(48,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(49,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(50,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(71,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(72,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(73,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(75,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(GLANAM(I-1,2).EQ.'REFL'.AND..NOT.AIRY.OR.
     1                                GLANAM(I-1,2).EQ.'REFLTIRO'.AND..NOT.AIRY.OR.
     2                                GLANAM(I-1,2).EQ.'REFLTIR'.AND..NOT.AIRY) THEN
                                      ELSE
C      WRITE(OUTLYNE,2501)
C      CALL SHOWIT(0)
                                      END IF
                                      WRITE(OUTLYNE,2000)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                                      CALL SHOWIT(0)
                                      IF(SPECF) THEN
                                          CALL SHRINKSPEC(SPECC)
                                          WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                  ELSE
                                      WRITE(OUTLYNE,2500)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                                      CALL SHOWIT(0)
                                      IF(SPECF) THEN
                                          CALL SHRINKSPEC(SPECC)
                                          WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                  END IF
                              ELSE
                                  IF(GLANAM((I-1),2).NE.'AIR'.AND.GLANAM((I-1),2)
     1                            .NE.'REFL'.AND.
     2                            GLANAM((I-1),2).NE.'REFLTIR'.AND.GLANAM(I-1,2).NE.'REFLTIRO') THEN
                                      IF(GLANAM(I,1).EQ.'MODEL') THEN
                                          WRITE(OUTLYNE,1006)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          CALL SHOWIT(0)
                                          WRITE(OUTLYNE,1009)ALENS(86,I),ALENS(87,I),ALENS(89,I)
                                          CALL SHOWIT(0)
 1009                                     FORMAT(
     1                                    '(MODEL DATA: Nd=',G13.6,1X,'Vd=',G13.6,1X,'DPART=',G13.6,')')
                                          IF(SPECF) THEN
                                              CALL SHRINKSPEC(SPECC)
                                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                              CALL SHOWIT(0)
                                          END IF
                                      ELSE
                                          IF(DABS(VNUM).LT.100.0D0)
     1                                    WRITE(OUTLYNE,1005)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          IF(DABS(VNUM).GE.100.0D0)
     1                                    WRITE(OUTLYNE,1000)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          CALL SHOWIT(0)
                                          IF(SPECF) THEN
                                              CALL SHRINKSPEC(SPECC)
                                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                              CALL SHOWIT(0)
                                          END IF
                                      END IF
                                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                  ELSE
C
                                      AIRY=.TRUE.
                                      IF(DABS(ALENS(46,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(47,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(48,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(49,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(50,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(71,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(72,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(73,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(75,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(GLANAM(I-1,2).EQ.'REFL'.AND..NOT.AIRY.OR.
     1                                GLANAM(I-1,2).EQ.'REFLTIR'.AND..NOT.AIRY.OR.
     2                                GLANAM(I-1,2).EQ.'REFLTIRO'.AND..NOT.AIRY) THEN
                                      ELSE
                                      END IF
                                      IF(GLANAM(I,1).EQ.'MODEL') THEN
                                          WRITE(OUTLYNE,1006)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          CALL SHOWIT(0)
                                          WRITE(OUTLYNE,1009)ALENS(86,I),ALENS(87,I),ALENS(89,I)
                                          CALL SHOWIT(0)
                                          IF(SPECF) THEN
                                              CALL SHRINKSPEC(SPECC)
                                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                              CALL SHOWIT(0)
                                          END IF
                                      ELSE
                                          IF(DABS(VNUM).LT.100.0D0)
     1                                    WRITE(OUTLYNE,1005)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          IF(DABS(VNUM).GE.100.0D0)
     1                                    WRITE(OUTLYNE,1000)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          CALL SHOWIT(0)
                                          IF(SPECF) THEN
                                              CALL SHRINKSPEC(SPECC)
                                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                              CALL SHOWIT(0)
                                          END IF
                                      END IF
                                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                  END IF
                              END IF
                          ELSE
C       WAS "LAST SURFACE" AND WAS ALREADY DONE JUST ABOVE
                          END IF
                      ELSE
C       I MUST BE 0
                      END IF
C
                      IF(I.EQ.0) THEN
                          IF(GLANAM(I,2).EQ.'AIR'.OR.GLANAM(I,2).EQ.'REFL'.OR.
     1                    GLANAM(I,2).EQ.'PERFECT'.OR.GLANAM(I,2).EQ.'IDEAL'.OR.
     1                    GLANAM(I,2).EQ.'REFLTIRO'.OR.GLANAM(I,2).EQ.'REFLTIR')THEN
                              WRITE(OUTLYNE,2500)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                              CALL SHOWIT(0)
                              IF(SPECF) THEN
                                  CALL SHRINKSPEC(SPECC)
                                  WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                  CALL SHOWIT(0)
                              END IF
                              IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                  WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                  CALL SHOWIT(0)
                              END IF
                          ELSE
                              IF(GLANAM(I,1).EQ.'MODEL') THEN
                                  WRITE(OUTLYNE,1006)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                            INDEX,VNUM
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1009)ALENS(86,I),ALENS(87,I),ALENS(89,I)
                                  CALL SHOWIT(0)
                                  IF(SPECF) THEN
                                      CALL SHRINKSPEC(SPECC)
                                      WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                      CALL SHOWIT(0)
                                  END IF
                              ELSE
                                  IF(DABS(VNUM).LT.100.0D0)
     1                            WRITE(OUTLYNE,1005)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                            INDEX,VNUM
                                  IF(DABS(VNUM).GE.100.0D0)
     1                            WRITE(OUTLYNE,1000)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                            INDEX,VNUM
                                  CALL SHOWIT(0)
                                  IF(SPECF) THEN
                                      CALL SHRINKSPEC(SPECC)
                                      WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                      CALL SHOWIT(0)
                                  END IF
                              END IF
                              IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                  WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                  CALL SHOWIT(0)
                              END IF
                          END IF
                      ELSE
C       NOT 0
                      END IF
C
                  ELSE
C       WC MUST BE CTG
C
                      CV=(ALENS(1,I))
                      SPEC=' '
                      SPECC=AA//AA//AA//'               '
                      SPECF=.TRUE.
C       CONIC
                      IF(ALENS(2,I).NE.0.0D0) SPEC='*'
C       ASPHERIC
                      IF(ALENS(8,I).NE.0.0D0) SPEC='*'
C       TORIC
                      IF(ALENS(23,I).NE.0.0D0) SPEC='*'
C       TILT
                      IF(ALENS(25,I).NE.0.0D0) SPEC='*'
C       DECENTER
                      IF(ALENS(29,I).NE.0.0D0) SPEC='*'
C       SPECIAL SURFACE (SPSRF TYPE)
                      IF(ALENS(34,I).NE.0.0D0) SPEC='*'
C       PIKUPS
                      IF(ALENS(32,I).NE.0.0D0) SPEC='*'
C       SOLVES
                      IF(ALENS(33,I).NE.0.0D0) SPEC='*'
C       REFS
                      IF(INT(SYSTEM1(25)).EQ.I) SPEC='*'
C       STOP
                      IF(INT(SYSTEM1(26)).EQ.I) SPEC='*'
C       CONIC
                      IF(ALENS(2,I).NE.0.0D0) SPECC(1:5)='Conic'
C       ASPHERIC
                      IF(ALENS(8,I).NE.0.0D0) SPECC(7:13)='Asphere'
C       TORIC
                      IF(ALENS(23,I).NE.0.0D0) SPECC(15:19)='Toric'
C       TILT
                      IF(ALENS(25,I).NE.0.0D0) SPECC(21:24)='Tilt'
C       DECENTER
                      IF(ALENS(29,I).NE.0.0D0) SPECC(26:33)='Decenter'
C       SPECIAL SURFACE (SPSRF TYPE)
                      IF(ALENS(34,I).NE.0.0D0) SPECC(35:41)='Special'
C       PIKUPS
                      IF(ALENS(32,I).NE.0.0D0) SPECC(43:47)='Pikup'
C       SOLVES
                      IF(ALENS(33,I).NE.0.0D0) SPECC(49:53)='Solve'
C       REFS
                      IF(INT(SYSTEM1(25)).EQ.I) SPECC(55:58)='REFS'
C       STOP
                      IF(INT(SYSTEM1(26)).EQ.I) SPECC(60:63)='STOP'
C       GRT
                      IF(ALENS(96,I).EQ.1.0D0) SPECC(65:67)='GRT'
C       DEFORMABLE SURFACE
                      IF(ALENS(103,I).EQ.1.0D0) SPECC(69:71)='DEF'
C       NSS SURFACE
                      IF(ALENS(126,I).NE.0.0D0) SPECC(73:75)='NSS'
                      IF(SPECC(1:75).EQ.AA//AA//AA//'               ') SPECF=.FALSE.
C
                      IF(GLANAM(I,2).EQ.'LAST SURFACE') THEN
                          IF(GLANAM((I-1),2).EQ.'AIR'.OR.GLANAM((I-1),2).EQ.'REFL'.OR.
     1                    GLANAM((I-1),2).EQ.'REFLTIRO'.OR.GLANAM((I-1),2).EQ.'REFLTIR')
     2                    THEN
                              AIRY=.TRUE.
                              IF(DABS(ALENS(46,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(47,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(48,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(49,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(50,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(71,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(72,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(73,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(DABS(ALENS(75,I-1)).NE.1.0D0) AIRY=.FALSE.
                              IF(GLANAM(I-1,2).EQ.'REFL'.AND..NOT.AIRY.OR.
     1                        GLANAM(I-1,2).EQ.'REFLTIR'.AND..NOT.AIRY.OR.
     2                        GLANAM(I-1,2).EQ.'REFLTIRO'.AND..NOT.AIRY) THEN
                              ELSE
                              END IF
                              WRITE(OUTLYNE,2000)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                              CALL SHOWIT(0)
                              IF(SPECF) THEN
                                  CALL SHRINKSPEC(SPECC)
                                  WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                  CALL SHOWIT(0)
                              END IF
                              IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                  WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                  CALL SHOWIT(0)
                              END IF
                          ELSE
                              WRITE(OUTLYNE,2500)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                              CALL SHOWIT(0)
                              IF(SPECF) THEN
                                  CALL SHRINKSPEC(SPECC)
                                  WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                  CALL SHOWIT(0)
                              END IF
                              IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                  WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                  CALL SHOWIT(0)
                              END IF
                          END IF
                      ELSE
                      END IF
C
                      IF(I.NE.0) THEN
                          IF(GLANAM(I,2).NE.'LAST SURFACE') THEN
                              IF(GLANAM(I,2).EQ.'AIR'.OR.GLANAM(I,2).EQ.'REFL'.OR.
     1                        GLANAM(I,2).EQ.'TIR'.OR.GLANAM(I,2).EQ.'REFLTIRO')THEN
                                  IF(GLANAM((I-1),2).EQ.'AIR'.OR.GLANAM((I-1),2).EQ.'REFL'.OR.
     1                            GLANAM((I-1),2).EQ.'REFLTIR'.OR.GLANAM((I-1),2).EQ.'REFLTIRO')
     2                            THEN
                                      AIRY=.TRUE.
                                      IF(DABS(ALENS(46,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(47,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(48,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(49,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(50,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(71,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(72,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(73,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(75,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(GLANAM(I-1,2).EQ.'REFL'.AND..NOT.AIRY.OR.
     1                                GLANAM(I-1,2).EQ.'REFLTIR'.AND..NOT.AIRY.OR.
     2                                GLANAM(I-1,2).EQ.'REFLTIRO'.AND..NOT.AIRY) THEN
                                      ELSE
                                      END IF
                                      WRITE(OUTLYNE,2000)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                                      CALL SHOWIT(0)
                                      IF(SPECF) THEN
                                          CALL SHRINKSPEC(SPECC)
                                          WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                  ELSE
                                      WRITE(OUTLYNE,2500)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                                      CALL SHOWIT(0)
                                      IF(SPECF) THEN
                                          CALL SHRINKSPEC(SPECC)
                                          WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                  END IF
                              ELSE
                                  IF(GLANAM((I-1),2).NE.'AIR'.AND.GLANAM((I-1),2).NE.'REFL'
     1                            .AND.GLANAM((I-1),2).NE.'REFLTIRO'
     2                            .AND.GLANAM((I-1),2).NE.'REFLTIR') THEN
                                      IF(GLANAM(I,1).EQ.'MODEL') THEN
                                          WRITE(OUTLYNE,1006)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          CALL SHOWIT(0)
                                          WRITE(OUTLYNE,1009)ALENS(86,I),ALENS(87,I),ALENS(89,I)
                                          CALL SHOWIT(0)
                                          IF(SPECF) THEN
                                              CALL SHRINKSPEC(SPECC)
                                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                              CALL SHOWIT(0)
                                          END IF
                                      ELSE
                                          IF(DABS(VNUM).LT.100.0D0)
     1                                    WRITE(OUTLYNE,1005)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          IF(DABS(VNUM).GE.100.0D0)
     1                                    WRITE(OUTLYNE,1000)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          CALL SHOWIT(0)
                                          IF(SPECF) THEN
                                              CALL SHRINKSPEC(SPECC)
                                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                              CALL SHOWIT(0)
                                          END IF
                                      END IF
                                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                  ELSE
C
                                      AIRY=.TRUE.
                                      IF(DABS(ALENS(46,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(47,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(48,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(49,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(50,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(71,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(72,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(73,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(DABS(ALENS(75,I-1)).NE.1.0D0) AIRY=.FALSE.
                                      IF(GLANAM(I-1,2).EQ.'REFL'.AND..NOT.AIRY.OR.
     1                                GLANAM(I-1,2).EQ.'REFLTIR'.AND..NOT.AIRY.OR.
     2                                GLANAM(I-1,2).EQ.'REFLTIRO'.AND..NOT.AIRY) THEN
                                      ELSE
                                      END IF

                                      IF(GLANAM(I,1).EQ.'MODEL') THEN
                                          WRITE(OUTLYNE,1006)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          CALL SHOWIT(0)
                                          WRITE(OUTLYNE,1009)ALENS(86,I),ALENS(87,I),ALENS(89,I)
                                          CALL SHOWIT(0)
                                          IF(SPECF) THEN
                                              CALL SHRINKSPEC(SPECC)
                                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                              CALL SHOWIT(0)
                                          END IF
                                      ELSE
                                          IF(DABS(VNUM).LT.100.0D0)
     1                                    WRITE(OUTLYNE,1005)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          IF(DABS(VNUM).GE.100.0D0)
     1                                    WRITE(OUTLYNE,1000)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                                    INDEX,VNUM
                                          CALL SHOWIT(0)
                                          IF(SPECF) THEN
                                              CALL SHRINKSPEC(SPECC)
                                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                              CALL SHOWIT(0)
                                          END IF
                                      END IF
                                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                          CALL SHOWIT(0)
                                      END IF
                                  END IF
                              END IF
                          ELSE
C       WAS "LAST SURFACE" AND WAS ALREADY DONE JUST ABOVE
                          END IF
                      ELSE
C       I MUST BE 0
                      END IF
C
                      IF(I.EQ.0) THEN
                          IF(GLANAM(I,2).EQ.'AIR'.OR.GLANAM(I,2).EQ.'REFL'.OR.
     1                    GLANAM(I,2).EQ.'REFLTIR'.OR.GLANAM(I,2).EQ.'REFLTIRO'.OR.
     1                    GLANAM(I,2).EQ.'PERFECT'.OR.GLANAM(I,2).EQ.'IDEAL') THEN
                              WRITE(OUTLYNE,2500)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                              CALL SHOWIT(0)
                              IF(SPECF) THEN
                                  CALL SHRINKSPEC(SPECC)
                                  WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                  CALL SHOWIT(0)
                              END IF
                              IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                  WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                  CALL SHOWIT(0)
                              END IF
                          ELSE
                              IF(GLANAM(I,1).EQ.'MODEL') THEN
                                  WRITE(OUTLYNE,1006)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                            INDEX,VNUM
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1009)ALENS(86,I),ALENS(87,I),ALENS(89,I)
                                  CALL SHOWIT(0)
                                  IF(SPECF) THEN
                                      CALL SHRINKSPEC(SPECC)
                                      WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                      CALL SHOWIT(0)
                                  END IF
                              ELSE
                                  IF(DABS(VNUM).LT.100.0D0)
     1                            WRITE(OUTLYNE,1005)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                            INDEX,VNUM
                                  IF(DABS(VNUM).GE.100.0D0)
     1                            WRITE(OUTLYNE,1000)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                            INDEX,VNUM
                                  CALL SHOWIT(0)
                                  IF(SPECF) THEN
                                      CALL SHRINKSPEC(SPECC)
                                      WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                                      CALL SHOWIT(0)
                                  END IF
                              END IF
                              IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                                  WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                                  CALL SHOWIT(0)
                              END IF
                          END IF
                      ELSE
C       NOT 0
                      END IF
                  END IF
C
 10           CONTINUE
              RETURN
          ELSE
C       QUALIFIER NOT "ALL"
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              NF=I
              CALL SINDEX
              IF(WC.EQ.'RTG'.OR.WC.EQ.'RTGLBL') THEN
                  IF(ALENS(1,0).EQ.0.0D0) THEN
                      RD=0.0D0
                  ELSE
                      RD=1.0D0/(ALENS(1,0))
                  END IF
                  SURF=0
                  I=0
C
                  SPEC=' '
                  SPECC=AA//AA//AA//'               '
                  SPECC=AA//AA//AA//'               '
                  SPECF=.TRUE.
C       CONIC
                  I=SURF
                  IF(ALENS(2,I).NE.0.0D0) SPEC='*'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPEC='*'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPEC='*'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPEC='*'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPEC='*'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPEC='*'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPEC='*'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPEC='*'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPEC='*'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPEC='*'
C       CONIC
                  IF(ALENS(2,I).NE.0.0D0) SPECC(1:5)='Conic'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPECC(7:13)='Asphere'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPECC(15:19)='Toric'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPECC(21:24)='Tilt'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPECC(26:33)='Decenter'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPECC(35:41)='Special'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPECC(43:47)='Pikup'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPECC(49:53)='Solve'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPECC(55:58)='REFS'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPECC(60:63)='STOP'
C       GRT
                  IF(ALENS(96,I).EQ.1.0D0) SPECC(65:67)='GRT'
C       DEFORMABLE SURFACE
                  IF(ALENS(103,I).EQ.1.0D0) SPECC(69:71)='DEF'
C       NSS SURFACE
                  IF(ALENS(126,I).NE.0.0D0) SPECC(73:75)='NSS'
                  IF(SPECC(1:75).EQ.AA//AA//AA//'               ') SPECF=.FALSE.
                  IF(GLANAM(0,2).EQ.'AIR'.OR.GLANAM(0,2).EQ.'REFL'.OR.
     1            GLANAM(0,2).EQ.'REFLTIRO'.OR.GLANAM(0,2).EQ.'REFLTIR'.OR.
     2            GLANAM(0,2).EQ.'PERFECT'.OR.GLANAM(0,2).EQ.'IDEAL') THEN
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,2500) SURF,SPEC,RD,ALENS(3,SURF),GLANAM(SURF,1),
     1                  GLANAM(SURF,2)
                      CALL SHOWIT(0)
                      IF(SPECF) THEN
                          CALL SHRINKSPEC(SPECC)
                          WRITE(OUTLYNE,0202) SURF,SPEC//SPECC(1:75)
                          CALL SHOWIT(0)
                      END IF
                      IF(LBT.AND.ALENS(44,SURF).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1001) SURF,LBL(SURF)(1:75)
                          CALL SHOWIT(0)
                      END IF
                  ELSE
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      IF(GLANAM(I,1).EQ.'MODEL') THEN
                          WRITE(OUTLYNE,1501)SURF,SPEC,RD,ALENS(3,SURF),GLANAM(SURF,1)
     1                    ,GLANAM(SURF,2),INDEX,VNUM
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1009)ALENS(86,SURF),ALENS(87,SURF),ALENS(89,SURF)
                          CALL SHOWIT(0)
                          IF(SPECF) THEN
                              CALL SHRINKSPEC(SPECC)
                              WRITE(OUTLYNE,0202) SURF,SPEC//SPECC(1:75)
                              CALL SHOWIT(0)
                          END IF
                      ELSE
                          WRITE(OUTLYNE,1500) SURF,SPEC,RD,ALENS(3,SURF),GLANAM(SURF,1)
     1                    ,GLANAM(SURF,2),INDEX,VNUM
                          CALL SHOWIT(0)
                          IF(SPECF) THEN
                              CALL SHRINKSPEC(SPECC)
                              WRITE(OUTLYNE,0202) SURF,SPEC//SPECC(1:75)
                              CALL SHOWIT(0)
                          END IF
                      END IF
                      IF(LBT.AND.ALENS(44,SURF).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1001) SURF,LBL(SURF)(1:75)
                          CALL SHOWIT(0)
                      END IF
                  END IF
                  RETURN
              ELSE
C       WC NOT RTG, MUST BE CTG
                  CV=(ALENS(1,0))
                  SURF=0
                  I=0
C
                  SPEC=' '
                  SPECC=AA//AA//AA//'               '
                  SPECF=.TRUE.
C       CONIC
                  IF(ALENS(2,I).NE.0.0D0) SPEC='*'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPEC='*'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPEC='*'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPEC='*'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPEC='*'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPEC='*'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPEC='*'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPEC='*'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPEC='*'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPEC='*'
C       CONIC
                  IF(ALENS(2,I).NE.0.0D0) SPECC(1:5)='Conic'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPECC(7:13)='Asphere'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPECC(15:19)='Toric'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPECC(21:24)='Tilt'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPECC(26:33)='Decenter'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPECC(35:41)='Special'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPECC(43:47)='Pikup'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPECC(49:53)='Solve'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPECC(55:58)='REFS'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPECC(60:63)='STOP'
C       GRT
                  IF(ALENS(96,I).EQ.1.0D0) SPECC(65:67)='GRT'
C       DEFORMABLE SURFACE
                  IF(ALENS(103,I).EQ.1.0D0) SPECC(69:71)='DEF'
C       NSS SURFACE
                  IF(ALENS(126,I).NE.0.0D0) SPECC(73:75)='NSS'
                  IF(SPECC(1:75).EQ.AA//AA//AA//'               ') SPECF=.FALSE.
                  IF(GLANAM(0,2).EQ.'AIR'.OR.GLANAM(0,2).EQ.'REFL'.OR.
     1            GLANAM(0,2).EQ.'PERFECT'.OR.GLANAM(0,2).EQ.'IDEAL'.OR.
     1            GLANAM(0,2).EQ.'REFLTIRO'.OR.GLANAM(0,2).EQ.'REFLTIR') THEN
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,2500) SURF,SPEC,CV,ALENS(3,SURF),GLANAM(SURF,1),
     1                  GLANAM(I,2)
                      CALL SHOWIT(0)
                      IF(SPECF) THEN
                          CALL SHRINKSPEC(SPECC)
                          WRITE(OUTLYNE,0202) SURF,SPEC//SPECC(1:75)
                          CALL SHOWIT(0)
                      END IF
                      IF(LBT.AND.ALENS(44,SURF).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1001) SURF,LBL(SURF)(1:75)
                          CALL SHOWIT(0)
                      END IF
                  ELSE
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      IF(GLANAM(I,1).EQ.'MODEL') THEN
                          WRITE(OUTLYNE,1501)SURF,SPEC,CV,ALENS(3,SURF),GLANAM(SURF,1)
     1                    ,GLANAM(SURF,2),INDEX,VNUM
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1009)ALENS(86,SURF),ALENS(87,SURF),ALENS(89,SURF)
                          CALL SHOWIT(0)
                          IF(SPECF) THEN
                              CALL SHRINKSPEC(SPECC)
                              WRITE(OUTLYNE,0202) SURF,SPEC//SPECC(1:75)
                              CALL SHOWIT(0)
                          END IF
                      ELSE
                          WRITE(OUTLYNE,1500) SURF,SPEC,CV,ALENS(3,SURF),GLANAM(SURF,1)
     1                      ,GLANAM(SURF,2),INDEX,VNUM
                          CALL SHOWIT(0)
                          IF(SPECF) THEN
                              CALL SHRINKSPEC(SPECC)
                              WRITE(OUTLYNE,0202) SURF,SPEC//SPECC(1:75)
                              CALL SHOWIT(0)
                          END IF
                      END IF
                      IF(LBT.AND.ALENS(44,SURF).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1001) SURF,LBL(SURF)(1:75)
                          CALL SHOWIT(0)
                      END IF
                  END IF
                  RETURN
              END IF
          ELSE
C       QUALIFIER NOT "OBJ" OR "OB"
          END IF
C
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB') THEN
              OUTLYNE='INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RTGERROR=.TRUE.
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=INT(SYSTEM1(20))
              NF=SF
              CALL SINDEX
              IF(WC.EQ.'RTG'.OR.WC.EQ.'RTGLBL') THEN
                  IF(ALENS(1,SF).EQ.0.0D0) THEN
                      RD=0.0D0
                  ELSE
                      RD=1.0D0/(ALENS(1,SF))
                  END IF
                  I=SF
C
                  SPEC=' '
                  SPECC=AA//AA//AA//'               '
                  SPECF=.TRUE.
C       CONIC
                  IF(ALENS(2,I).NE.0.0D0) SPEC='*'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPEC='*'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPEC='*'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPEC='*'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPEC='*'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPEC='*'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPEC='*'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPEC='*'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPEC='*'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPEC='*'
C       CONIC
                  IF(ALENS(2,I).NE.0.0D0) SPECC(1:5)='Conic'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPECC(7:13)='Asphere'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPECC(15:19)='Toric'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPECC(21:24)='Tilt'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPECC(26:33)='Decenter'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPECC(35:41)='Special'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPECC(43:47)='Pikup'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPECC(49:53)='Solve'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPECC(55:58)='REFS'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPECC(60:63)='STOP'
C       GRT
                  IF(ALENS(96,I).EQ.1.0D0) SPECC(65:67)='GRT'
C       DEFORMABLE SURFACE
                  IF(ALENS(103,I).EQ.1.0D0) SPECC(69:71)='DEF'
C       NSS SURFACE
                  IF(ALENS(126,I).NE.0.0D0) SPECC(73:75)='NSS'
                  IF(SPECC(1:75).EQ.AA//AA//AA//'               ') SPECF=.FALSE.
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2500) SF,SPEC,RD,ALENS(3,SF),GLANAM(SF,1)
     1            ,GLANAM(SF,2)
                  CALL SHOWIT(0)
                  IF(SPECF) THEN
                      CALL SHRINKSPEC(SPECC)
                      WRITE(OUTLYNE,0202) SF,SPEC//SPECC(1:75)
                      CALL SHOWIT(0)
                  END IF
                  IF(LBT.AND.ALENS(44,SF).EQ.1.0D0) THEN
                      WRITE(OUTLYNE,1001) SF,LBL(SF)(1:75)
                      CALL SHOWIT(0)
                  END IF
                  RETURN
              ELSE
C       WC NOT RTG,MUST BE CTG
                  CV=(ALENS(1,SF))
                  I=SF
C
                  SPEC=' '
                  SPECC=AA//AA//AA//'               '
                  SPECF=.TRUE.
C       CONIC
                  IF(ALENS(2,I).NE.0.0D0) SPEC='*'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPEC='*'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPEC='*'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPEC='*'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPEC='*'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPEC='*'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPEC='*'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPEC='*'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPEC='*'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPEC='*'
C       CONIC
                  IF(ALENS(2,I).NE.0.0D0) SPECC(1:5)='Conic'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPECC(7:13)='Asphere'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPECC(15:19)='Toric'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPECC(21:24)='Tilt'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPECC(26:33)='Decenter'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPECC(35:41)='Special'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPECC(43:47)='Pikup'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPECC(49:53)='Solve'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPECC(55:58)='REFS'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPECC(60:63)='STOP'
C       GRT
                  IF(ALENS(96,I).EQ.1.0D0) SPECC(65:67)='GRT'
C       DEFORMABLE SURFACE
                  IF(ALENS(103,I).EQ.1.0D0) SPECC(69:71)='DEF'
C       NSS SURFACE
                  IF(ALENS(126,I).NE.0.0D0) SPECC(73:75)='NSS'
                  IF(SPECC(1:75).EQ.AA//AA//AA//'               ') SPECF=.FALSE.
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2500) SF,SPEC,CV,ALENS(3,SF),GLANAM(SF,1)
     1            ,GLANAM(SF,2)
                  CALL SHOWIT(0)
                  IF(SPECF) THEN
                      CALL SHRINKSPEC(SPECC)
                      WRITE(OUTLYNE,0202) SF,SPEC//SPECC(1:75)
                      CALL SHOWIT(0)
                  END IF
                  IF(LBT.AND.ALENS(44,SF).EQ.1.0D0) THEN
                      WRITE(OUTLYNE,1001) SF,LBL(SF)(1:75)
                      CALL SHOWIT(0)
                  END IF
                  RETURN
              END IF
          ELSE
C       DON'T OUTPUT IMAGE SURFACE
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RTGERROR=.TRUE.
                  CALL MACFAL
                  RETURN
              END IF
              NF=I
              CALL SINDEX
              IF(WC.EQ.'RTG'.OR.WC.EQ.'RTGLBL') THEN
                  IF(ALENS(1,I).EQ.0.0D0) THEN
                      RD=0.0D0
                  ELSE
                      RD=1.0D0/(ALENS(1,I))
                  END IF
C
                  SPEC=' '
                  SPECC=AA//AA//AA//'               '
                  SPECF=.TRUE.
C       CONIC
                  IF(ALENS(2,I).NE.0.0D0) SPEC='*'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPEC='*'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPEC='*'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPEC='*'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPEC='*'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPEC='*'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPEC='*'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPEC='*'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPEC='*'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPEC='*'
C       CONIC
                  IF(ALENS(2,I).NE.0.0D0) SPECC(1:5)='Conic'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPECC(7:13)='Asphere'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPECC(15:19)='Toric'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPECC(21:24)='Tilt'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPECC(26:33)='Decenter'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPECC(35:41)='Special'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPECC(43:47)='Pikup'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPECC(49:53)='Solve'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPECC(55:58)='REFS'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPECC(60:63)='STOP'
C       GRT
                  IF(ALENS(96,I).EQ.1.0D0) SPECC(65:67)='GRT'
C       DEFORMABLE SURFACE
                  IF(ALENS(103,I).EQ.1.0D0) SPECC(69:71)='DEF'
C       NSS SURFACE
                  IF(ALENS(126,I).NE.0.0D0) SPECC(73:75)='NSS'
                  IF(SPECC(1:75).EQ.AA//AA//AA//'               ') SPECF=.FALSE.
                  IF(GLANAM(I,2).EQ.'AIR'.OR.GLANAM(I,2).EQ.'REFL'
     1            .OR.GLANAM(I,2).EQ.'LAST SURFACE'
     2            .OR.GLANAM(I,2).EQ.'PERFECT'.OR.GLANAM(I,2).EQ.'IDEAL'.OR.
     2            GLANAM(I,2).EQ.'REFLTIR'.OR.GLANAM(I,2).EQ.'REFLTIR') THEN
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,2500) I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                      CALL SHOWIT(0)
                      IF(SPECF) THEN
                          CALL SHRINKSPEC(SPECC)
                          WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                          CALL SHOWIT(0)
                      END IF
                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                          CALL SHOWIT(0)
                      END IF
                  ELSE
                      IF(HEADIN) WRITE(OUTLYNE,5000)
                      IF(HEADIN) CALL SHOWIT(0)
                      IF(GLANAM(I,1).EQ.'MODEL') THEN
                          WRITE(OUTLYNE,1501)I,SPEC,RD,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                    INDEX,VNUM
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1009)ALENS(86,I),ALENS(87,I),ALENS(89,I)
                          CALL SHOWIT(0)
                          IF(SPECF) THEN
                              CALL SHRINKSPEC(SPECC)
                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                              CALL SHOWIT(0)
                          END IF
                      ELSE
                          WRITE(OUTLYNE,1500) I,SPEC,RD,ALENS(3,I),GLANAM(I,1),
     1                    GLANAM(I,2),INDEX,VNUM

                          CALL SHOWIT(0)
                          IF(SPECF) THEN
                              CALL SHRINKSPEC(SPECC)
                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                              CALL SHOWIT(0)
                          END IF
                      END IF
                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                          CALL SHOWIT(0)
                      END IF
                  END IF
                  RETURN
              ELSE
C       WC NOT RTG, MUST BE CTG
                  CV=(ALENS(1,I))
C
                  SPEC=' '
                  SPECC=AA//AA//AA//'               '
                  SPECF=.TRUE.
C       CONIC
                  IF(ALENS(2,I).NE.0.0D0) SPEC='*'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPEC='*'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPEC='*'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPEC='*'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPEC='*'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPEC='*'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPEC='*'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPEC='*'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPEC='*'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPEC='*'
C       CONIC
                  IF(ALENS(2,I).NE.0.0D0) SPECC(1:5)='Conic'
C       ASPHERIC
                  IF(ALENS(8,I).NE.0.0D0) SPECC(7:13)='Asphere'
C       TORIC
                  IF(ALENS(23,I).NE.0.0D0) SPECC(15:19)='Toric'
C       TILT
                  IF(ALENS(25,I).NE.0.0D0) SPECC(21:24)='Tilt'
C       DECENTER
                  IF(ALENS(29,I).NE.0.0D0) SPECC(26:33)='Decenter'
C       SPECIAL SURFACE (SPSRF TYPE)
                  IF(ALENS(34,I).NE.0.0D0) SPECC(35:41)='Special'
C       PIKUPS
                  IF(ALENS(32,I).NE.0.0D0) SPECC(43:47)='Pikup'
C       SOLVES
                  IF(ALENS(33,I).NE.0.0D0) SPECC(49:53)='Solve'
C       REFS
                  IF(INT(SYSTEM1(25)).EQ.I) SPECC(55:58)='REFS'
C       STOP
                  IF(INT(SYSTEM1(26)).EQ.I) SPECC(60:63)='STOP'
C       GRT
                  IF(ALENS(96,I).EQ.1.0D0) SPECC(65:67)='GRT'
C       DEFORMABLE SURFACE
                  IF(ALENS(103,I).EQ.1.0D0) SPECC(69:71)='DEF'
C       NSS SURFACE
                  IF(ALENS(126,I).NE.0.0D0) SPECC(73:75)='NSS'
                  IF(SPECC(1:75).EQ.AA//AA//AA//'               ') SPECF=.FALSE.
                  IF(GLANAM(I,2).EQ.'AIR'.OR.GLANAM(I,2).EQ.'REFL'
     1            .OR.GLANAM(I,2).EQ.'LAST SURFACE'
     1            .OR.GLANAM(I,2).EQ.'PERFECT'.OR.GLANAM(I,2).EQ.'IDEAL'
     1            .OR.GLANAM(I,2).EQ.'REFLTIRO'.OR.GLANAM(I,2).EQ.'REFLTIR') THEN
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      WRITE(OUTLYNE,2500) I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2)
                      CALL SHOWIT(0)
                      IF(SPECF) THEN
                          CALL SHRINKSPEC(SPECC)
                          WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                          CALL SHOWIT(0)
                      END IF
                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                          CALL SHOWIT(0)
                      END IF
                  ELSE
                      IF(HEADIN) WRITE(OUTLYNE,6000)
                      IF(HEADIN) CALL SHOWIT(0)
                      IF(GLANAM(I,1).EQ.'MODEL') THEN
                          WRITE(OUTLYNE,1501)I,SPEC,CV,ALENS(3,I),GLANAM(I,1),GLANAM(I,2),
     1                    INDEX,VNUM
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1009)ALENS(86,I),ALENS(87,I),ALENS(89,I)
                          CALL SHOWIT(0)
                          IF(SPECF) THEN
                              CALL SHRINKSPEC(SPECC)
                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                              CALL SHOWIT(0)
                          END IF
                      ELSE
                          WRITE(OUTLYNE,1500) I,SPEC,CV,ALENS(3,I),GLANAM(I,1),
     1                    GLANAM(I,2),INDEX,VNUM

                          CALL SHOWIT(0)
                          IF(SPECF) THEN
                              CALL SHRINKSPEC(SPECC)
                              WRITE(OUTLYNE,0202) I,SPEC//SPECC(1:75)
                              CALL SHOWIT(0)
                          END IF
                      END IF
                      IF(LBT.AND.ALENS(44,I).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,1001) I,LBL(I)(1:75)
                          CALL SHOWIT(0)
                      END IF
                  END IF
                  RETURN
              END IF
          ELSE
          END IF
          RETURN
 1500     FORMAT(I3,A1,1X,G13.6,1X,G13.6,1X,A7,
     1    1X,A13,1X,G13.6,1X,G10.3)
 1501     FORMAT(I3,A1,1X,G13.6,1X,G13.6,1X,A7,
     1    1X,A13,1X,G13.6,1X,G10.3)
 2500     FORMAT(I3,A1,1X,G13.6,1X,G13.6,1X,A7,1X,A13)
 2000     FORMAT(I3,A1,1X,G13.6,1X,G13.6,1X,A7,1X,A13)
 1000     FORMAT(I3,A1,1X,G13.6,1X,G13.6,1X,A7,
     1    1X,A13,1X,G13.6,1X,G10.3)
 1005     FORMAT(I3,A1,1X,G13.6,1X,G13.6,1X,A7,
     1    1X,A13,1X,G13.6,1X,G10.3)
 1006     FORMAT(I3,A1,1X,G13.6,1X,G13.6,1X,A7,
     1    1X,A13,1X,G13.6,1X,G10.3)
 5000     FORMAT('SURF',2X,'RADIUS',8X,'THICKNESS',10X,'MATERIAL',
     1    10X,'INDEX',8X,'V-NUM')
 6000     FORMAT('SURF',2X,'CURVATURE',5X,'THICKNESS',10X,'MATERIAL',
     1    10X,'INDEX',8X,'V-NUM')
 5001     FORMAT('BASIC LENS DATA (RADIUS MODE)')
 6001     FORMAT('BASIC LENS DATA (CURVATURE MODE)')
 2501     FORMAT(1X)
 0202     FORMAT(I3,A75)
      END
      SUBROUTINE SHRINKSPEC(SPECC)
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          CHARACTER SPECC*75,SP1*5,SP2*7,SP3*5,SP4*4,SP5*8,SP6*7,SP7*5
     1    ,SP8*5,SP9*4,SP10*4,SP11*3,SP12*3,SP13*3
          INTEGER I,L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13
          L1 =LEN_TRIM(SPECC(1:5))
          L2 =LEN_TRIM(SPECC(7:13))
          L3 =LEN_TRIM(SPECC(15:19))
          L4 =LEN_TRIM(SPECC(21:24))
          L5 =LEN_TRIM(SPECC(26:33))
          L6 =LEN_TRIM(SPECC(35:41))
          L7 =LEN_TRIM(SPECC(43:47))
          L8 =LEN_TRIM(SPECC(49:53))
          L9 =LEN_TRIM(SPECC(55:58))
          L10=LEN_TRIM(SPECC(60:63))
          L11=LEN_TRIM(SPECC(65:67))
          L12=LEN_TRIM(SPECC(69:71))
          L13=LEN_TRIM(SPECC(73:75))
          SP1=SPECC(1:5)
          SP2=SPECC(7:13)
          SP3=SPECC(15:19)
          SP4=SPECC(21:24)
          SP5=SPECC(26:33)
          SP6=SPECC(35:41)
          SP7=SPECC(43:47)
          SP8=SPECC(49:53)
          SP9=SPECC(55:58)
          SP10=SPECC(60:63)
          SP11=SPECC(65:67)
          SP12=SPECC(69:71)
          SP13=SPECC(73:75)
          SPECC(1:75)=
     1    '                                                               '
          I=0
          IF(L1.EQ.0) THEN
          ELSE
              SPECC(1:L1)=SP1(1:L1)
              I=L1
          END IF
          IF(L2.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L2)=SPECC(1:I)//','//SP2(1:L2)
              I=I+1+L2
          END IF
          IF(L3.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L3)=SPECC(1:I)//','//SP3(1:L3)
              I=I+1+L3
          END IF
          IF(L4.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L4)=SPECC(1:I)//','//SP4(1:L4)
              I=I+1+L4
          END IF
          IF(L5.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L5)=SPECC(1:I)//','//SP5(1:L5)
              I=I+1+L5
          END IF
          IF(L6.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L6)=SPECC(1:I)//','//SP6(1:L6)
              I=I+1+L6
          END IF
          IF(L7.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L7)=SPECC(1:I)//','//SP7(1:L7)
              I=I+1+L7
          END IF
          IF(L8.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L8)=SPECC(1:I)//','//SP8(1:L8)
              I=I+1+L8
          END IF
          IF(L9.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L9)=SPECC(1:I)//','//SP9(1:L9)
              I=I+1+L9
          END IF
          IF(L10.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L10)=SPECC(1:I)//','//SP10(1:L10)
              I=I+1+L10
          END IF
          IF(L11.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L11)=SPECC(1:I)//','//SP11(1:L11)
              I=I+1+L11
          END IF
          IF(L12.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L12)=SPECC(1:I)//','//SP12(1:L12)
              I=I+1+L12
          END IF
          IF(L13.EQ.0) THEN
          ELSE
              SPECC(1:I+1+L13)=SPECC(1:I)//','//SP13(1:L13)
              I=I+1+L13
          END IF
          DO I=1,75
              IF(SPECC(1:1).EQ.' ') SPECC(1:75)=SPECC(2:75)//' '
              IF(SPECC(1:1).NE.' ') GO TO 20
          END DO
 20       IF(SPECC(1:1).EQ.',') SPECC(1:75)=SPECC(2:75)//' '
          RETURN
      END
C SUB SPL23.FOR

      SUBROUTINE SPL23(AR,AX,AY,AZ,IFUNC)
C
          IMPLICIT NONE
C
C     MAIN CUBIC SPLINE SURFACE ROUTINE
C
          REAL*8 XV,XARAY(1:96),YARAY(1:96),
     1    AX,AY,AZ,SPARRAY,INCRM,ARHO
C
          INTEGER AR,IFUNC,ISURF,ALLOERR,I,NV
C
          DIMENSION SPARRAY(1:96)
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          REAL*8 SPL23A
          DIMENSION SPL23A(:,:)
          ALLOCATABLE :: SPL23A
C
          NV=INT(FTFL01(1,AR))
          ISURF=AR
C
C     IF IFUNC = 1, JUST DEALLOCATE THE SPL23 ARRAYS. THIS HAPPENS
C     WHENEVER LENNS IS CALLED. THIS FREES MEMORY
C
C     IF IFUNC = 3, INTERPOLATE A SPLINE TO GET A SAG VALUE
C
          IF(IFUNC.EQ.1) THEN
              DEALLOCATE(SPL23A,STAT=ALLOERR)
              RETURN
          END IF
          IF(IFUNC.EQ.3) THEN
C     ALLOCATE THE SPL23 ARRAY FOR THIS SURFACE AND LOAD IT
              DEALLOCATE(SPL23A,STAT=ALLOERR)
              ALLOCATE(SPL23A(1:NV,1:3),STAT=ALLOERR)
              IF(FTFL01(2,AR).EQ.0.0D0) THEN
                  INCRM=ALENS(76,INT(ISURF/(DABS(FTFL01(1,ISURF))-1.0D0)))
              ELSE
                  INCRM=DABS(FTFL01(2,ISURF)/(DABS(FTFL01(1,ISURF))-1.0D0))
              END IF
              XV=0.0D0
              NV=INT(FTFL01(1,ISURF))
              DO I=1,NV
                  XARAY(I)=XV
                  YARAY(I)=FTFL01(I+2,ISURF)
                  XV=XV+INCRM
              END DO
              CALL SPLINE(XARAY,YARAY,NV,1.0D35,1.0D35,SPARRAY)
              NV=INT(FTFL01(1,ISURF))
              ARHO=DSQRT((AX**2)+(AY**2))
              CALL SPLINT(XARAY,YARAY,SPARRAY,NV,ARHO,AZ)
          END IF
          DEALLOCATE(SPL23A,STAT=ALLOERR)
          RETURN
      END
