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

C       FOURTH FILE FOR LENS DATABASE MANAGER FILES

C SUB COATING.FOR
      SUBROUTINE COATING
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE COATING WHICH IMPLEMENTS THE COATING
C       COMMAND AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(WC.EQ.'COATING') THEN
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"COATING" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                  S1=0
                  DF1=0
                  W1=0.0D0
                  OUTLYNE=
     1            '"COATING" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
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
                  OUTLYNE='NO SURFACE COATINGS EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
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
C
C
                  IF(HEADIN) WRITE(OUTLYNE,2000)
                  IF(HEADIN) CALL SHOWIT(0)
                  IF(ALENS(112,SURF).NE.0.0D0)
     1            WRITE(OUTLYNE,300) SURF,INT(ALENS(112,SURF))
                  IF(ALENS(112,SURF).EQ.0.0D0)
     1            WRITE(OUTLYNE,301) SURF
                  CALL SHOWIT(0)
              ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
C       PRINT HEADING DATA
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)
                  CALL SHOWIT(0)
                  DO SURF=0,INT(SYSTEM1(20))
                      IF(ALENS(112,SURF).NE.0.0D0)
     1                WRITE(OUTLYNE,300) SURF,INT(ALENS(112,SURF))
                      IF(ALENS(112,SURF).EQ.0.0D0)
     1                WRITE(OUTLYNE,301) SURF
                      CALL SHOWIT(0)
                  END DO
              END IF
          ELSE
C     NOT RIN
          END IF
C
 300      FORMAT(I3,11X,I4)
 301      FORMAT(I3,5X,'SURFACE NOT COATED')
 1000     FORMAT('SURFACE COATING NUMBER DATA')
 2000     FORMAT('SURF',4X,'COATING NUMBER')
 1500     FORMAT(1X)
          RETURN
      END
C SUB SNDEX.FOR
      SUBROUTINE SNDEX
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SNDEX WHICH IMPLEMENTS THE NDEX, NDEX2
C       COMMAND AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          LOGICAL PRINTIT
C
          IF(WC.EQ.'NDEX') THEN
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"NDEX" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                  S1=0
                  DF1=0
                  W1=0.0D0
                  OUTLYNE=
     1            '"NDEX" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
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
                  OUTLYNE='NO REFRACTIVE INDICES EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE NDEX
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE NDEX DATA FOR
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
C
                  IF(HEADIN) WRITE(OUTLYNE,2000)
                  IF(HEADIN) CALL SHOWIT(0)
                  IF(DABS(ALENS(46,SURF)).EQ.1.0D0.AND.
     1            DABS(ALENS(47,SURF)).EQ.1.0D0.AND.
     1            DABS(ALENS(48,SURF)).EQ.1.0D0.AND.
     1            DABS(ALENS(49,SURF)).EQ.1.0D0.AND.
     1            DABS(ALENS(50,SURF)).EQ.1.0D0) THEN
                      WRITE(OUTLYNE,201) SURF
                      CALL SHOWIT(0)
 201                  FORMAT('FOR SURFACE ',I3,', N1 TO N5 HAVE UNIT VALUES')
 202                  FORMAT('FOR ALL SURFACES, N1 TO N5 HAVE UNIT VALUES')
 203                  FORMAT('FOR SURFACE ',I3,', N6 TO N10 HAVE UNIT VALUES')
 204                  FORMAT('FOR ALL SURFACES, N6 TO N10 HAVE UNIT VALUES')
                  ELSE
                      WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,200) SURF,ALENS(46,SURF),ALENS(47,SURF),
     1                ALENS(48,SURF),ALENS(49,SURF),ALENS(50,SURF)
                      CALL SHOWIT(0)
                  END IF
              ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
C       PRINT HEADING DATA
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)
                  CALL SHOWIT(0)


                  PRINTIT=.FALSE.
                  DO SURF=0,INT(SYSTEM1(20))
                      IF(DABS(ALENS(46,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(47,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(48,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(49,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(50,SURF)).EQ.1.0D0) THEN
                      ELSE
                          PRINTIT=.TRUE.
                      END IF
                  END DO
                  IF(.NOT.PRINTIT) THEN
                      WRITE(OUTLYNE,202)
                      CALL SHOWIT(0)
                      RETURN
                  END IF
                  DO SURF=0,INT(SYSTEM1(20))
                      IF(DABS(ALENS(46,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(47,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(48,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(49,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(50,SURF)).EQ.1.0D0) THEN
                      ELSE
                          WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) SURF,ALENS(46,SURF),ALENS(47,SURF),
     1                    ALENS(48,SURF),ALENS(49,SURF),ALENS(50,SURF)
                          CALL SHOWIT(0)
                      END IF
                  END DO
              END IF
          ELSE
C     NOT NDEX
          END IF
          IF(WC.EQ.'NDEX2') THEN
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"NDEX" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
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
     1            '"NDEX2" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
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
                  OUTLYNE='NO REFRACTIVE INDICES EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE NDEX2
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE NDEX2 DATA FOR
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
                  IF(HEADIN) WRITE(OUTLYNE,2002)
                  IF(HEADIN) CALL SHOWIT(0)
                  IF(DABS(ALENS(71,SURF)).EQ.1.0D0.AND.
     1            DABS(ALENS(72,SURF)).EQ.1.0D0.AND.
     1            DABS(ALENS(73,SURF)).EQ.1.0D0.AND.
     1            DABS(ALENS(74,SURF)).EQ.1.0D0.AND.
     1            DABS(ALENS(75,SURF)).EQ.1.0D0) THEN
                      WRITE(OUTLYNE,203) SURF
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,200) SURF,ALENS(71,SURF),ALENS(72,SURF),
     1                ALENS(73,SURF),ALENS(74,SURF),ALENS(75,SURF)
                      CALL SHOWIT(0)
                  END IF
              ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
C       PRINT HEADING DATA
                  WRITE(OUTLYNE,1002)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2002)
                  CALL SHOWIT(0)
                  PRINTIT=.FALSE.
                  DO SURF=0,INT(SYSTEM1(20))
                      IF(DABS(ALENS(71,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(72,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(73,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(74,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(75,SURF)).EQ.1.0D0) THEN
                      ELSE
                          PRINTIT=.TRUE.
                      END IF
                  END DO
                  IF(.NOT.PRINTIT) THEN
                      WRITE(OUTLYNE,204)
                      CALL SHOWIT(0)
                      RETURN
                  END IF
                  DO SURF=0,INT(SYSTEM1(20))
                      IF(DABS(ALENS(71,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(72,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(73,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(74,SURF)).EQ.1.0D0.AND.
     1                DABS(ALENS(75,SURF)).EQ.1.0D0) THEN
                      ELSE
                          WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,200) SURF,ALENS(71,SURF),ALENS(72,SURF),
     1                    ALENS(73,SURF),ALENS(74,SURF),ALENS(75,SURF)
                          CALL SHOWIT(0)
                      END IF
                  END DO
              END IF
          ELSE
C     NOT NDEX2
          END IF
C
 1002     FORMAT('REFRACTIVE INDEX DATA (WAVELENGTHS #6 TO #10)')
 2002     FORMAT('SURF',4X,'N6',13X,'N7',13X,'N8',13X,'N9',13X,'N10')
 200      FORMAT(I3,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6)
 300      FORMAT(I3,1X,A13,1X,A13)
C
 1000     FORMAT('REFRACTIVE INDEX DATA (WAVELENGTHS #1 TO #5)')
 2000     FORMAT('SURF',4X,'N1',13X,'N2',13X,'N3',13X,'N4',13X,'N5')
 1500     FORMAT(1X)
          RETURN
      END
C SUB SGRATT.FOR
      SUBROUTINE SGRATT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SRIN WHICH IMPLEMENTS THE GRT
C       COMMAND AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
C
          LOGICAL NOGRT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"GRT" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              S1=0
              DF1=0
              W1=0.0D0
              OUTLYNE=
     1        '"GRT" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
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
              OUTLYNE='NO LINEAR GRATINGS EXIST'
              CALL SHOWIT(1)
              OUTLYNE='LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE RIN
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE RIN DATA FOR
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
              IF(HEADIN) WRITE(OUTLYNE,2000)
              IF(HEADIN) CALL SHOWIT(0)
              IF(ALENS(96,SURF).EQ.1.0D0) THEN
                  WRITE(OUTLYNE,200) SURF,ALENS(97,SURF),ALENS(98,SURF),
     1            ALENS(99,SURF),ALENS(100,SURF),ALENS(101,SURF)
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,300)
                  CALL SHOWIT(0)
              END IF
          ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
C       PRINT HEADING DATA
              NOGRT=.TRUE.
              DO SURF=0,INT(SYSTEM1(20))
C
                  IF(ALENS(96,SURF).EQ.1.0D0) THEN
                      NOGRT=.FALSE.
                      GO TO 20
                  END IF
              END DO
 20           CONTINUE
              IF(.NOT.NOGRT) THEN
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)
                  CALL SHOWIT(0)
                  DO SURF=0,INT(SYSTEM1(20))
C
                      IF(ALENS(96,SURF).EQ.1.0D0) THEN
                          WRITE(OUTLYNE,200) SURF,ALENS(97,SURF),ALENS(98,SURF),
     1                    ALENS(99,SURF),ALENS(100,SURF),ALENS(101,SURF)
                          CALL SHOWIT(0)
                      END IF
                  END DO
              ELSE
C     NO GRATINGS
                  WRITE(OUTLYNE,300)
                  CALL SHOWIT(0)
              END IF
          END IF
C
 200      FORMAT(I3,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6)
C
  300     FORMAT('NO LINEAR DIFFRATION GRATING DATA FOUND')
 1000     FORMAT('LINEAR DIFFRACTION GRATING DATA')
 2000     FORMAT('SURF',4X,'GRO',12X,'GRS',12X,'GRX',12X,'GRY',12X,'GRZ')
 1500     FORMAT(1X)
          RETURN
      END
C SUB SRIN.FOR
      SUBROUTINE SRIN
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SRIN WHICH IMPLEMENTS THE RIN
C       COMMAND AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(WC.EQ.'RIN') THEN
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"RIN" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                  S1=0
                  DF1=0
                  W1=0.0D0
                  OUTLYNE=
     1            '"RIN" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
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
                  OUTLYNE='NO REFRACTIVE INDICES EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE RIN
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE RIN DATA FOR
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
                  IF(HEADIN) WRITE(OUTLYNE,2000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,200) SURF,ALENS(46,SURF),ALENS(47,SURF),
     1            ALENS(48,SURF),ALENS(49,SURF),ALENS(50,SURF)
                  CALL SHOWIT(0)
              ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
C       PRINT HEADING DATA
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)
                  CALL SHOWIT(0)
                  DO SURF=0,INT(SYSTEM1(20))
                      WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,200) SURF,ALENS(46,SURF),ALENS(47,SURF),
     1                ALENS(48,SURF),ALENS(49,SURF),ALENS(50,SURF)
                      CALL SHOWIT(0)
                  END DO
              END IF
          ELSE
C     NOT RIN
          END IF
          IF(WC.EQ.'RIN2') THEN
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"RIN2" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
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
     1            '"RIN2" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
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
                  OUTLYNE='NO REFRACTIVE INDICES EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE RIN2
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE RIN2 DATA FOR
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
                  IF(HEADIN) WRITE(OUTLYNE,2002)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,200) SURF,ALENS(71,SURF),ALENS(72,SURF),
     1            ALENS(73,SURF),ALENS(74,SURF),ALENS(75,SURF)
                  CALL SHOWIT(0)
              ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
C       PRINT HEADING DATA
                  WRITE(OUTLYNE,1002)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2002)
                  CALL SHOWIT(0)
                  DO SURF=0,INT(SYSTEM1(20))
                      WRITE(OUTLYNE,300) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,200) SURF,ALENS(71,SURF),ALENS(72,SURF),
     1                ALENS(73,SURF),ALENS(74,SURF),ALENS(75,SURF)
                      CALL SHOWIT(0)
                  END DO
              END IF
          ELSE
C     NOT RIN2
          END IF
C
 1002     FORMAT('REFRACTIVE INDEX DATA (WAVELENGTHS #6 TO #10)')
 2002     FORMAT('SURF',4X,'N6',13X,'N7',13X,'N8',13X,'N9',13X,'N10')
C
 200      FORMAT(I3,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6,1X,G14.6)
 300      FORMAT(I3,1X,A13,1X,A13)
C
 1000     FORMAT('REFRACTIVE INDEX DATA (WAVELENGTHS #1 TO #5)')
 2000     FORMAT('SURF',4X,'N1',13X,'N2',13X,'N3',13X,'N4',13X,'N5')
 1500     FORMAT(1X)
          RETURN
      END
C SUB IMAGEDIR.FOR
      SUBROUTINE IMAGEDIR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE IMAGEDIR WHICH IMPLEMENTS THE IMAGEDIR
C       COMMAND AT THE CMD LEVEL
C       THE ONLY VALID QUALIFIERS ARE (BLANK) AND "ALL","OB" AND "OBJ"
C       THE NUMERIC ENTRY W1, IS THE SURFACE DESIGNATOR
C       FOR PRINTOUT OF ONE SURFACE'S DATA ONLY
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(WC.EQ.'IMAGEDIR') THEN
C               THIS IS A CMD LEVEL COMMAND ONLY
C
C               CHECK FOR ADDITIONAL INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"IMAGEDIR" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                  S1=0
                  DF1=0
                  W1=0.0D0
                  OUTLYNE=
     1            '"IMAGEDIR" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
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
                  OUTLYNE='NO IMAGE ORIENTATION DATA EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(.NOT.SREFDIFEXT) THEN
                  OUTLYNE='NO IMAGE ORIENTATION DATA EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='NO DIFFERENTIAL RAY DATA EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='ISSUE AN "FOB" COMMAND TO PROCEED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       W1 DESIGNATES THE SURFACE FOR WHICH THE ORIENTATION
C       DATA IS TO BE OUTPUT.
C       IF THE QUALIFIER "ALL" IS USED, THEN THE IMAGE ORIENTATION DATA FOR
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
                  IF(HEADIN) WRITE(OUTLYNE,2000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,300) SURF,POLANGX(SURF),POLANGY(SURF)
                  CALL SHOWIT(0)
              ELSE
C*********************************************************************
C       SQ=1 AND ONLY ALLOWED WQ BY NOW IS "ALL"
C       HANDEL THE WHOLE LENS
C       PRINT HEADING DATA
                  WRITE(OUTLYNE,1000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)
                  CALL SHOWIT(0)
                  DO SURF=0,INT(SYSTEM1(20))
                      WRITE(OUTLYNE,300) SURF,POLANGX(SURF),POLANGY(SURF)
                      CALL SHOWIT(0)
                  END DO
              END IF
          ELSE
C     NOT IMAGEDIR
          END IF
C
 1000     FORMAT('IMAGE ORIENTATION DATA')
 2000     FORMAT('SURF',5X,'X-VECTOR',16X,'Y-VECTOR')
 300      FORMAT(I3,1X,G23.15,1X,G23.15)
C
 1500     FORMAT(1X)
          RETURN
      END
C SUB SREFS.FOR
      SUBROUTINE SREFS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SREFS WHICH IMPLEMENTS THE REFS
C       COMMAND AT THE LENS OF UPDATE LENS LEVEL.
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR PRESENCE OF STRING,QUALIFIER,OR NUMERIC
C               INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE='"REFS" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.0.0D0.OR.W1.GT.360.0D0) THEN
              OUTLYNE= 'ORIENTATION ANGLE RANGE TO 0.0 TO 360.0 DEGREES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CHECK THAT REFS NOT ASSIGNED TO OBJECT SURFACE
C
          IF(DF1.EQ.1) W1=0.0D0
          IF(SURF.EQ.0) THEN
              OUTLYNE='OBJECT SURFACE CAN NOT BE THE REFERENCE SURFACE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ALENS(127,SURF).NE.0.0D0) THEN
              OUTLYNE='THE CURRENT SURFACE HAS MULTIPLE APERTURES ASSIGNED'
              CALL SHOWIT(1)
              OUTLYNE='AND THEREFORE MAY NOT BE SET AS THE REFERENCE SURFACE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          SYSTEM1(25)=DBLE(SURF)
          SYSTEM1(59)=W1
          NEWREF=INT(SYSTEM1(25))
C     IF THE ASTOP IS ON -99 THEN IF REFSURF IS NOT SURFACE 1
C     SET ASTOP SURF TO BE EQUAL TO REF SURF
          IF(SYSTEM1(26).LT.0.0D0) THEN
              IF(NEWREF.NE.1) THEN
C     RE-ASSIGN THE ASTOP TO THE REFSURF
                  SYSTEM1(27)=0.0D0
                  SYSTEM1(26)=SYSTEM1(25)
                  IF(F6.EQ.1) THEN
                      OUTLYNE='ASTOP AUTOMATICALLY SET TO BE ON THE REFERENCE SURFACE'
                      CALL SHOWIT(1)
                  END IF
              END IF
          END IF
          RETURN
      END
C SUB SREF.FOR
      SUBROUTINE SREF
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SREF WHICH IMPLEMENTS THE REF
C       COMMAND AT THE CMD LEVEL.
C
          REAL*8 SYS12,SYS13
C
          CHARACTER UNI1*7
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR PRESENCE OF STRING,QUALIFIER,OR NUMERIC
C               INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"REF" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F1.EQ.1) THEN
C
C               WE ARE AT THE CMD LEVEL
C
C       WHAT IS THERE ARE NO SURFACES
              IF(SYSTEM1(20).EQ.0.0D0) THEN
                  OUTLYNE= 'NO REFERENCE SURFACE EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE= 'LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  OUTLYNE= 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       WRITE STATEMENTS GO HERE
C
C*************************************************************
              IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.
     1        DABS(ALENS(9,NEWREF)).LE.6.0D0.AND.ALENS(127,NEWREF)
     2        .EQ.0.0D0) THEN
C
C       CLAP IS ON REFERENCE SURFACE
C
C       CIRCULAR CLAP
C
                  IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                      IF(ALENS(10,NEWREF).LE.ALENS(11,NEWREF)) THEN
                          SYS12=ALENS(10,NEWREF)
                          SYS13=ALENS(10,NEWREF)
                      ELSE
                          SYS12=ALENS(11,NEWREF)
                          SYS13=ALENS(11,NEWREF)
                      END IF
                  ELSE
C       NOT CIRCULAR CLAP
                  END IF
C        RECT CLAP
C
                  IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                      SYS12=ALENS(10,NEWREF)
                      SYS13=ALENS(11,NEWREF)
                  ELSE
C       NOT RECT CLAP
                  END IF
C        ELIP CLAP
C
                  IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                      SYS12=ALENS(10,NEWREF)
                      SYS13=ALENS(11,NEWREF)
                  ELSE
C       NOT ELIP CLAP
                  END IF
C        RCTK CLAP
C
                  IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                      SYS12=ALENS(10,NEWREF)
                      SYS13=ALENS(11,NEWREF)
                  ELSE
C       NOT RCTK CLAP
                  END IF
C        POLY CLAP
C
                  IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                      SYS12=ALENS(10,NEWREF)
                      SYS13=ALENS(10,NEWREF)
                  ELSE
C       NOT POLY CLAP
                  END IF
C        IPOLY CLAP
C
                  IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                      SYS12=ALENS(14,NEWREF)
                      SYS13=ALENS(14,NEWREF)
                  ELSE
C       NOT IPOLY CLAP
                  END IF
C
              ELSE
C       NO CLAP ON REF SURF.
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              END IF
C*************************************************************
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000) SYSTEM1(14),SYSTEM1(21),SYS12,
     2        NEWOBJ,NEWREF,NEWIMG
C
              CALL SHOWIT(0)
              WRITE(OUTLYNE,3000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,4000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000) SYSTEM1(16),SYSTEM1(23),SYS13,
     2        NEWOBJ,NEWREF,NEWIMG
              CALL SHOWIT(0)
              IF(SYSTEM1(59).NE.0.0D0) THEN
                  WRITE(OUTLYNE,3000)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,6000) SYSTEM1(59)
                  CALL SHOWIT(0)
              ELSE
              END IF
          ELSE
          END IF
          IF(SYSTEM1(94).NE.0.0D0) THEN
              IF(SYSTEM1(94).LT.0.0D0.AND.SYSTEM1(6).EQ.1.0D0) UNI1='INCH   '
              IF(SYSTEM1(94).LT.0.0D0.AND.SYSTEM1(6).EQ.2.0D0) UNI1='CM     '
              IF(SYSTEM1(94).LT.0.0D0.AND.SYSTEM1(6).EQ.3.0D0) UNI1='MM     '
              IF(SYSTEM1(94).LT.0.0D0.AND.SYSTEM1(6).EQ.4.0D0) UNI1='METER  '
              IF(SYSTEM1(94).GT.0.0D0)                         UNI1='DEGREES'
              WRITE(OUTLYNE,3000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5004) SYSTEM1(92),UNI1
              CALL SHOWIT(0)
          END IF
          IF(SYSTEM1(95).NE.0.0D0) THEN
              IF(SYSTEM1(95).LT.0.0D0.AND.SYSTEM1(6).EQ.1.0D0) UNI1='INCH   '
              IF(SYSTEM1(95).LT.0.0D0.AND.SYSTEM1(6).EQ.2.0D0) UNI1='CM     '
              IF(SYSTEM1(95).LT.0.0D0.AND.SYSTEM1(6).EQ.3.0D0) UNI1='MM     '
              IF(SYSTEM1(95).LT.0.0D0.AND.SYSTEM1(6).EQ.4.0D0) UNI1='METER  '
              IF(SYSTEM1(95).GT.0.0D0)                         UNI1='DEGREES'
              WRITE(OUTLYNE,3000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5001)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5005) SYSTEM1(93),UNI1
              CALL SHOWIT(0)
          END IF
          IF(SYSTEM1(98).NE.0.0D0) THEN
              IF(SYSTEM1(98).LT.0.0D0.AND.SYSTEM1(6).EQ.1.0D0) UNI1='INCH   '
              IF(SYSTEM1(98).LT.0.0D0.AND.SYSTEM1(6).EQ.2.0D0) UNI1='CM     '
              IF(SYSTEM1(98).LT.0.0D0.AND.SYSTEM1(6).EQ.3.0D0) UNI1='MM     '
              IF(SYSTEM1(98).LT.0.0D0.AND.SYSTEM1(6).EQ.4.0D0) UNI1='METER  '
              IF(SYSTEM1(98).GT.0.0D0)                         UNI1='DEGREES'
              WRITE(OUTLYNE,3000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5002)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5006) SYSTEM1(96),UNI1
              CALL SHOWIT(0)
          END IF
          IF(SYSTEM1(99).NE.0.0D0) THEN
              IF(SYSTEM1(99).LT.0.0D0.AND.SYSTEM1(6).EQ.1.0D0) UNI1='INCH   '
              IF(SYSTEM1(99).LT.0.0D0.AND.SYSTEM1(6).EQ.2.0D0) UNI1='CM     '
              IF(SYSTEM1(99).LT.0.0D0.AND.SYSTEM1(6).EQ.3.0D0) UNI1='MM     '
              IF(SYSTEM1(99).LT.0.0D0.AND.SYSTEM1(6).EQ.4.0D0) UNI1='METER  '
              IF(SYSTEM1(99).GT.0.0D0)                         UNI1='DEGREES'
              WRITE(OUTLYNE,3000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5003)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5007) SYSTEM1(97),UNI1
              CALL SHOWIT(0)
          END IF
 1000     FORMAT('REF OBJ Y-HT',20X,'REF AP Y-HT',9X
     1    ,'OBJ SF#  REF SF#  IMG SF#')
 2000     FORMAT(G13.6,'(',G11.4,1X,'DG)',4X,G11.4,11X,I3,6X,I3,6X,I3)
 3000     FORMAT(1X)
 4000     FORMAT('REF OBJ X-HT',20X,'REF AP X-HT',9X
     1    ,'OBJ SF#  REF SF#  IMG SF#')
 6000     FORMAT('ORIENTATION ANGLE = ',G11.4,' DEGREES')
 5000     FORMAT('X-REF OBJ. VIA PARAXIAL IMAGE HT/ANGLE SPEC.')
 5001     FORMAT('Y-REF OBJ. VIA PARAXIAL IMAGE HT/ANGLE SPEC.')
 5002     FORMAT('X-REF OBJ. VIA REAL HT/ANGLE SPEC.')
 5003     FORMAT('Y-REF OBJ. VIA REAL HT/ANGLE SPEC.')
 5004     FORMAT('PXIM = ',D23.15,1X,A7)
 5005     FORMAT('PYIM = ',D23.15,1X,A7)
 5006     FORMAT('RXIM = ',D23.15,1X,A7)
 5007     FORMAT('RYIM = ',D23.15,1X,A7)
          RETURN
      END

C SUB SPCGLS.FOR
      SUBROUTINE SPCGLS(I,MTYPE)
          IMPLICIT NONE
C
          INTEGER I,MTYPE,J,N
C
          REAL*8 X(1:100),Y(1:100),Y2(1:100),LA,CSI
     1    ,YPN,YP1,GSCX(1:20),GSCY(1:20),GPCX(1:17),GPCY(1:17)
     2    ,SIL,GPLX(1:13),GPL1Y(1:13),GPL2Y(1:13),VIR3X(1:18),VIR3Y(1:18)
     3    ,GPL3Y(1:13),GPL4Y(1:13),IRGX(1:25),IRGY(1:25),ZNSEX(1:56)
     4    ,ZNSEY(1:56),ZNSX(1:59),ZNSY(1:59),CLRX(1:29),CLRY(1:29)
     5    ,SAPH,FSIL,MGF2O,MGF2E,CAF2,MGO,G9754X(1:43),G9754Y(1:43)
     6    ,AMTR1X(1:17),AMTR1Y(1:17),DYN,BAF2,KBR,CSBR,KRS5
     7    ,AMTR3X(1:12),AMTR3Y(1:12),AS2S3,ALONX(1:14),ALONY(1:14)
     8    ,GAASX(1:13),GAASY(1:13),CDTEX(1:10),CDTEY(1:10)
     9    ,SPINX(1:11),SPINY(1:11),CALALX(1:12),CALALY(1:12)
     1    ,H2OX(1:21),H2OY(1:21),SUPX(1:60),SUPY(1:60),HOMOX(1:47),
     2    HOMOY(1:47),ZNSMSX(1:59),ZNSMSY(1:59),YAG
C
          REAL*8 SIO2OX(1:29),SIO2OY(1:29),SIO2EX(1:20)
     1    ,SIO2EY(1:20),LIF,NACLX(1:60),NACLY(1:60)
     1    ,B270X(1:10),B270Y(1:10),DIAMOND
     2    ,IRG2X(1:18),IRG2Y(1:18),IRG3X(1:17),IRG3Y(1:17),IRGN6X(1:16)
     3    ,IRGN6Y(1:16),IRG7X(1:16),IRG7Y(1:16),IRG9X(1:16),IRG9Y(1:16)
     4    ,IRG11X(1:16),IRG11Y(1:16),IRG15X(1:16),IRG15Y(1:16)
     5    ,DUMA,DUMB,DUML,LM1,A,B,LA1,LA2,LA3,LA4,LA5
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     VACUME
          LM1(DUMA,DUMB,DUML)=DUMA*(1.0D0+(DUMB/(DUML**2)))
C
C     SILICA/SIO2 INTERPOLATION FORMULA (HANDBOOK OF OPTICS 11/14/2000)
          FSIL(LA)=DSQRT(1.0D0+
     1    ((0.6961663*(LA**2))/((LA**2)-((0.0684043)**2)))+
     1    ((0.4079426*(LA**2))/((LA**2)-((0.1162414)**2)))+
     1    ((0.8974794*(LA**2))/((LA**2)-((9.896161)**2)))
     1    )
C
C
C     DIAMOND INTERPOLATION FORMULA
          DIAMOND(LA)=
     1    2.37837D0+
     1    ((1.18897E-2)*(1.0D0/((LA**2)-0.028D0)))+
     2    ((-1.0083E-4)*((1.0D0/((LA**2)-0.028D0))**2))+
     3    ((-2.3676E-5)*(LA**2))+
     4    ((3.24263E-8)*(LA**4))
C
C     LiF INTERPOLATION FORMULA
          LIF(LA)=
     1     1.38761D0+(0.001796D0/((LA**2)-0.028D0))
     2     +(0.000041D0/(((LA**2)-0.028D0)**2))-(0.0023045D0*(LA**2))
     3    -(0.00000557D0*(LA**4))
C
C     SILICON INTERPOLATION FORMULA
          SIL(LA)=
     1     3.41696D0+(0.138497D0/((LA**2)-0.028D0))
     2     +(0.013924D0/(((LA**2)-0.028D0)**2))-(0.0000209D0*(LA**2))
     3    +(0.000000148D0*(LA**4))
C
C     KBr INTERPOLATION FORMULA
          KBR(LA)=DSQRT(
     1     2.361323D0-(3.11497D-4*(LA**2))-(5.8613D-8*(LA**4))+
     1     (0.007676D0/(LA**2))+(0.0156569D0/((LA**2)-0.0324D0)))
C
C     CsBr INTERPOLATION FORMULA
          CSBR(LA)=DSQRT(
     1     5.640752D0-(3.338D-6*(LA**2))+
     1     (0.0018612D0/(LA**2))+(41110.49D0/((LA**2)-14390.4D0))
     1     +(0.0290764D0/((LA**2)-0.024964D0)))
C
C     MGO INTERPOLATION FORMULA
          MGO(LA)=DSQRT(
     1     2.956362D0-(0.01062387D0*(LA**2))-(0.0000204968D0*(LA**4))-
     2     (0.02195770/((LA**2)-0.01428322D0)))
C
C     DYNASIL INTERPOLATION FORMULA
          DYN(LA)=DSQRT(
     1     ((0.6961663D0*(LA**2))/((LA**2)-((0.0684043D0)**2)))+
     1     ((0.4079426D0*(LA**2))/((LA**2)-((0.1162414D0)**2)))+
     1     ((0.8974794D0*(LA**2))/((LA**2)-((9.896161D0)**2)))+1.0D0)
C
C     SAPPHIRE INTERPOLATION FORMULA
          SAPH(LA)=DSQRT(
     1     ((1.023798D0*(LA**2))/((LA**2)-(0.00377588D0)))+
     1     ((1.058264D0*(LA**2))/((LA**2)-(0.01225440D0)))+
     1     ((5.280792D0*(LA**2))/((LA**2)-(321.361600D0)))+1.0D0)
C
C     YAG INTERPOLATION FORMULA
          YAG(LA)=DSQRT(
     1     ((2.293D0*(LA**2))/((LA**2)-(0.1095D0**2)))+
     1     ((3.705D0*(LA**2))/((LA**2)-(17.825D0**2)))+1.0D0)
C
C     CSI INTERPOLATION FORMULA
          CSI(LA)=DSQRT(
     1     ((0.34617251D0*(LA**2))/((LA**2)-0.00052701D0))+
     1     ((1.0080886D0*(LA**2))/((LA**2)-0.02149156D0))+
     1     ((0.28551800D0*(LA**2))/((LA**2)-0.032761D0))+
     1     ((0.39743178D0*(LA**2))/((LA**2)-0.044944D0))+
     1     ((3.3605359D0*(LA**2))/((LA**2)-25621.0D0))+1.0D0)
C
C     KRS5 INTERPOLATION FORMULA
          KRS5(LA)=DSQRT(
     1     ((1.8293958D0*(LA**2))/((LA**2)-(0.0225D0**2)))+
     1     ((1.6675593D0*(LA**2))/((LA**2)-(0.0625D0**2)))+
     1     ((1.1210424D0*(LA**2))/((LA**2)-(0.1225D0**2)))+
     1     ((0.04513366D0*(LA**2))/((LA**2)-(0.2025D0**2)))+
     1     ((12.380234D0*(LA**2))/((LA**2)-(27089.737D0**2)))+1.0D0)
C
C     As2S3 INTERPOLATION FORMULA
          AS2S3(LA)=DSQRT(
     1     ((1.8983678D0*(LA**2))/((LA**2)-(0.15D0**2)))+
     1     ((1.9222979D0*(LA**2))/((LA**2)-(0.25D0**2)))+
     1     ((0.8765134D0*(LA**2))/((LA**2)-(0.350D0**2)))+
     1     ((0.1188704D0*(LA**2))/((LA**2)-(0.450D0**2)))+
     1     ((0.9569903D0*(LA**2))/((LA**2)-(27.3861D0**2)))+1.0D0)
C
C     BAF2 INTERPOLATION FORMULA
          BAF2(LA)=DSQRT(
     1     ((0.643356D0*(LA**2))/((LA**2)-((0.057789D0)**2)))+
     1     ((0.506762D0*(LA**2))/((LA**2)-((0.10968D0)**2)))+
     1     ((3.8261D0*(LA**2))/((LA**2)-((46.3864D0)**2)))+1.0D0)
C
C     CAF2 INTERPOLATION FORMULA
          CAF2(LA)=DSQRT(
     1     ((0.5675888D0*(LA**2))/((LA**2)-((0.050263605D0)**2)))+
     1     ((0.4710914D0*(LA**2))/((LA**2)-((0.1003909D0)**2)))+
     1     ((3.8484723D0*(LA**2))/((LA**2)-((34.649040D0)**2)))+1.0D0)
C
C     MGF2o INTERPOLATION FORMULA
          MGF2O(LA)=DSQRT(
     1    1.0D0+
     2    ((.48755108D0*(LA**2))/((LA**2)-(0.04338408D0**2)))
     3    +((.39875031D0*(LA**2))/((LA**2)-(0.09461442D0**2)))
     4    +((2.3120353D0*(LA**2))/((LA**2)-(23.7936040D0**2)))
     5    )
C
C     MGF2E INTERPOLATION FORMULA
          MGF2E(LA)=DSQRT(
     1    1.0D0+
     2    ((.41344023D0*(LA**2))/((LA**2)-(0.03684262D0**2)))
     3    +((.50497499D0*(LA**2))/((LA**2)-(0.09076162D0**2)))
     4    +((2.4904862D0*(LA**2))/((LA**2)-(12.771995D0**2)))
     5    )
C
C     THIS SUBROUTINE IS USED TO CALCULATE REFRACTIVE
C     INDICES OF THE MATERIALS WHOSE NAMES ARE THEIR COMMANDS
C
          IF(MTYPE.EQ.1) THEN
C     INTERPOLATE THE SINGLE CRYSTALLINE GERMANIUN DATA
              N=20
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR SINGLE CRYSTAL GERMANIUM
C
              DATA (GSCX(J), J = 1,20)/2.0581D0,2.1526D0,2.3126D0,2.4374D0,
     1         2.577D0,
     1         2.7144D0,2.998D0,3.3033D0,3.4188D0,4.258D0,4.866D0,6.238D0,
     2         8.66D0,9.72D0,11.04D0,12.20D0,13.02D0,14.21D0,15.08D0,16.0D0/
C
              DATA (GSCY(J), J = 1,20)/
     1        4.1016D0,4.0919D0,4.0786D0,4.0708D0,
     1        4.0609D0,4.0552D0,4.0452D0,4.0369D0,4.0334D0,
     2        4.0216D0,4.0170D0,4.0094D0,4.0043D0,4.0034D0,
     3        4.0026D0,4.0023D0,4.0021D0,4.0015D0,4.0014D0,4.0012D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=GSCX(1:N)
              Y(1:N)=GSCY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.2) THEN
C     INTERPOLATE THE POLY CRYSTALLINE GERMANIUN DATA
              N=17
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR POLY CRYSTAL GERMANIUM
C
              DATA (GPCX(J), J = 1,17)/2.0581D0,2.1526D0,2.3126D0,2.4374D0,
     1         2.577D0,
     1         2.7144D0,2.998D0,3.3033D0,3.4188D0,4.258D0,4.866D0,6.238D0,
     2         8.66D0,9.72D0,11.04D0,12.20D0,13.02D0/
C
              DATA (GPCY(J), J = 1,17)/4.1018D0,4.0919D0,4.0785D0,4.0709D0,
     1        4.0608D0,4.0554D0,4.0452D0,4.0372D0,4.0339D0,
     2        4.0217D0,4.0167D0,4.0095D0,4.0043D0,4.0033D0,
     3        4.0025D0,4.0020D0,4.0018D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=GPCX(1:N)
              Y(1:N)=GPCY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.3) THEN
C     INTERPOLATE THE SILICON DATA
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=SIL(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=SIL(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=SIL(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=SIL(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=SIL(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=SIL(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=SIL(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=SIL(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=SIL(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=SIL(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
          END IF
          IF(MTYPE.EQ.4) THEN
C     INTERPOLATE THE IRG100 DATA
              N=25
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR IRG100
C
              DATA (IRGX(J), J = 1,25)/1.0D0,1.5D0,2.0D0,2.5D0,3.0D0,3.5D0
     1        ,4.0D0,4.5D0,5.0D0,5.5D0,6.0D0,6.5D0,7.0D0,7.5D0,8.0D0,8.5D0
     2        ,9.0D0,9.5D0,10.0D0,10.5D0,11.0D0,11.5D0,12.0D0,13.0D0,14.0D0/
C
              DATA (IRGY(J), J = 1,25)/2.7235D0,2.6577D0,2.6404D0,2.6314D0
     1        ,2.6262D0,2.6227D0,2.6201D0,2.6181D0,2.6164D0,2.6148D0,2.6133D0
     2        ,2.6118D0,2.6103D0,2.6088D0,2.6072D0,2.6056D0,2.6039D0,2.6022D0
     3        ,2.6004D0,2.5985D0,2.5966D0,2.5946D0,2.5925D0,2.5880D0,2.5832D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=IRGX(1:N)
              Y(1:N)=IRGY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.5) THEN
C     INTERPOLATE THE ZNSE DATA
              N=56
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR ZNSE
C
              DATA (ZNSEX(J), J = 1,56)/0.54D0,0.58D0
     1        ,0.62D0,0.66D0,0.70D0,0.74D0,0.78D0,0.82D0,0.86D0,0.90D0
     2        ,0.94D0,0.98D0,1.0D0,1.4D0,1.8D0,2.2D0,2.6D0,3.0D0,3.4D0
     3        ,3.8D0,4.2D0,4.6D0,5.0D0,5.4D0,5.8D0,6.2D0,6.6D0,7.0D0,7.4D0
     4        ,7.8D0,8.2D0,8.6D0,9.0D0,9.4D0,9.8D0,10.2D0,10.6D0,11.0D0
     5        ,11.4D0,11.8D0,12.2D0,12.6D0,13.0D0,13.4D0,13.8D0,14.2D0,14.6D0
     6        ,15.0D0,15.4D0,15.8D0,16.2D0,16.6D0,17.0D0,17.4D0,17.8D0,18.2D0/
C
              DATA (ZNSEY(J), J = 1,56)/2.6754D0,2.6312D0,2.5994D0,2.5755D0
     1        ,2.5568D0,2.5418D0,2.5295D0,2.5193D0,2.5107D0,2.5034D0,2.4971D0
     2        ,2.4916D0,2.4892D0,2.4609D0,2.4496D0,2.4437D0,2.4401D0,2.4376D0
     3        ,2.4356D0,2.4339D0,2.4324D0,2.4309D0,2.4295D0,2.4281D0,2.4266D0
     4        ,2.4251D0,2.4235D0,2.4218D0,2.4201D0,2.4183D0,2.4163D0,2.4143D0
     5        ,2.4122D0,2.4100D0,2.4077D0,2.4053D0,2.4028D0,2.4001D0,2.3974D0
     6        ,2.3945D0,2.3915D0,2.3883D0,2.3850D0,2.3816D0,2.3781D0,2.3744D0
     7        ,2.3705D0,2.3665D0,2.3623D0,2.3579D0,2.3534D0,2.3487D0,2.3438D0
     8        ,2.3387D0,2.3333D0,2.3278D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=ZNSEX(1:N)
              Y(1:N)=ZNSEY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.6) THEN
C     INTERPOLATE THE ZNS DATA
              N=59
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR ZNS
C
              DATA (ZNSX(J), J = 1,59)/0.42D0,0.46D0,0.5D0,0.54D0,0.58D0
     1        ,0.62D0,0.66D0,0.70D0,0.74D0,0.78D0,0.82D0,0.86D0,0.90D0
     2        ,0.94D0,0.98D0,1.0D0,1.4D0,1.8D0,2.2D0,2.6D0,3.0D0,3.4D0
     3        ,3.8D0,4.2D0,4.6D0,5.0D0,5.4D0,5.8D0,6.2D0,6.6D0,7.0D0,7.4D0
     4        ,7.8D0,8.2D0,8.6D0,9.0D0,9.4D0,9.8D0,10.2D0,10.6D0,11.0D0
     5        ,11.4D0,11.8D0,12.2D0,12.6D0,13.0D0,13.4D0,13.8D0,14.2D0,14.6D0
     6        ,15.0D0,15.4D0,15.8D0,16.2D0,16.6D0,17.0D0,17.4D0,17.8D0,18.2D0/
C
              DATA (ZNSY(J), J = 1,59)/2.516D0,2.458D0,2.419D0,2.391D0,2.371D0
     1        ,2.355D0,2.342D0,2.332D0,2.323D0,2.316D0,2.310D0,2.305D0,2.301D0
     2        ,2.297D0,2.294D0,2.292D0,2.275D0,2.267D0,2.263D0,2.260D0,2.257D0
     3        ,2.255D0,2.253D0,2.251D0,2.248D0,2.246D0,2.244D0,2.241D0,2.238D0
     4        ,2.235D0,2.232D0,2.228D0,2.225D0,2.221D0,2.217D0,2.212D0,2.208D0
     5        ,2.203D0,2.198D0,2.192D0,2.186D0,2.180D0,2.173D0,2.167D0,2.159D0
     6        ,2.152D0,2.143D0,2.135D0,2.126D0,2.116D0,2.106D0,2.095D0,2.084D0
     7        ,2.072D0,2.059D0,2.045D0,2.030D0,2.015D0,1.998D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=ZNSX(1:N)
              Y(1:N)=ZNSY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.7) THEN
C     INTERPOLATE THE CLRTRAN DATA
              N=29
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR CLRTRAN
C
              DATA (CLRX(J), J = 1,29)/0.4047D0,0.4358D0,0.4678D0,0.48D0
     1        ,0.5086D0,0.5461D0,0.5876D0,0.6438D0,0.6678D0,0.7065D0,0.78D0
     2        ,0.7948D0,0.8521D0,0.8943D0,1.014D0,1.1287D0,1.5296D0,2.0581D0
     3        ,3.0D0,3.5D0,4.0D0,4.5D0,5.0D0,8.0D0,9.0D0,10.0D0,11.25D0,12.0D0
     4        ,13.0D0/
C
              DATA (CLRY(J), J = 1,29)/2.54515D0,2.48918D0,2.44915D0,2.43691D0
     1        ,2.41279D0,2.38838D0,2.36789D0,2.34731D0,2.34033D0,2.33073D0
     2        ,2.31669D0,2.31438D0,2.30659D0,2.30183D0,2.29165D0,2.28485D0
     3        ,2.27191D0,2.26442D0,2.25772D0,2.25498D0,2.25231D0,2.24955D0
     4        ,2.24661D0,2.22334D0,2.21290D0,2.20084D0,2.18317D0,2.17101D0
     5        ,2.15252D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=CLRX(1:N)
              Y(1:N)=CLRY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.8) THEN
C     INTERPOLATE THE WILLOW RUN FUSED SILICA/SIO2
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=FSIL(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=FSIL(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=FSIL(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=FSIL(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=FSIL(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=FSIL(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=FSIL(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=FSIL(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=FSIL(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=FSIL(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
          END IF
          IF(MTYPE.EQ.9) THEN
C     INTERPOLATE SAPPHIRE
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=SAPH(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=SAPH(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=SAPH(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=SAPH(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=SAPH(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=SAPH(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=SAPH(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=SAPH(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=SAPH(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=SAPH(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.10) THEN
C     INTERPOLATE DYNASIL
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=DYN(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=DYN(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=DYN(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=DYN(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=DYN(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=DYN(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=DYN(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=DYN(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=DYN(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=DYN(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.11) THEN
C     INTERPOLATE THE AMTIR1
              N=17
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR AMTIR1
C
              DATA (AMTR1X(J), J = 1,17)/1.0D0,1.064D0,1.5D0,2.0D0,2.4D0
     1        ,3.0D0,4.0D0,5.0D0,6.0D0,7.0D0,8.0D0,9.0D0,10.0D0,11.0D0
     2        ,12.0D0,13.0D0,14.0D0/
C
              DATA (AMTR1Y(J), J = 1,17)/2.6055D0,2.5933D0,2.5469D0,2.5310D0
     1        ,2.5250D0,2.5184D0,2.5146D0,2.5112D0,2.5086D0,2.5062D0,2.5036D0
     2        ,2.5008D0,2.4977D0,2.4942D0,2.4902D0,2.4862D0,2.4825D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=AMTR1X(1:N)
              Y(1:N)=AMTR1Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.12) THEN
C     INTERPOLATE THE AMTIR3
              N=12
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR AMTIR3
C
              DATA (AMTR3X(J), J = 1,12)/3.0D0,4.0D0,5.0D0,6.0D0,7.0D0,8.0D0
     1        ,9.0D0,10.0D0,11.0D0,12.0D0,13.0D0,14.0D0/
C
              DATA (AMTR3Y(J), J = 1,12)/2.6266D0,2.6210D0,2.6173D0,2.6142D0
     1        ,2.6117D0,2.6088D0,2.6055D0,2.6023D0,2.5983D0,2.5942D0,2.5892D0
     2        ,2.5843D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=AMTR3X(1:N)
              Y(1:N)=AMTR3Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.13) THEN
C     INTERPOLATE THE As2S3
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=AS2S3(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=AS2S3(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=AS2S3(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=AS2S3(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=AS2S3(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=AS2S3(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=AS2S3(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=AS2S3(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=AS2S3(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=AS2S3(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.14) THEN
C     INTERPOLATE THE GaAs
              N=13
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR GaAs
C
              DATA (GAASX(J), J = 1,13)/2.5D0,3.0D0,4.0D0,5.0D0,6.0D0
     1        ,7.0D0,8.0D0,9.0D0,10.0D0,11.0D0,12.0D0,13.0D0,14.0D0/
C
              DATA (GAASY(J), J = 1,13)/3.3256D0,3.3169D0,3.3069D0
     1        ,3.3010,3.2963D0,3.2923D0,3.2878D0,3.2830D0,3.2778D0
     2        ,3.2725D0,3.2666D0,3.2589D0,3.2509D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=GAASX(1:N)
              Y(1:N)=GAASY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.15) THEN
C     INTERPOLATE THE CdTe
              N=10
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR CdTe
C
              DATA (CDTEX(J), J = 1,10)/3.0D0,4.0D0,5.0D0,6.0D0,7.0D0
     1        ,8.0D0,9.0D0,10.0D0,11.0D0,12.0D0/
C
              DATA (CDTEY(J), J = 1,10)/2.7026D0,2.6971D0,2.6922D0
     1        ,2.6886D0,2.6865D0,2.6846D0,2.6825D0,2.6797D0,2.6766D0
     2        ,2.6749D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=CDTEX(1:N)
              Y(1:N)=CDTEY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.16) THEN
C     INTERPOLATE MGF2O
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=MGF2O(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=MGF2O(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=MGF2O(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=MGF2O(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=MGF2O(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=MGF2O(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=MGF2O(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=MGF2O(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=MGF2O(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=MGF2O(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.17) THEN
C     INTERPOLATE MGF2E
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=MGF2E(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=MGF2E(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=MGF2E(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=MGF2E(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=MGF2E(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=MGF2E(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=MGF2E(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=MGF2E(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=MGF2E(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=MGF2E(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.18) THEN
C     INTERPOLATE CAF2
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=CAF2(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=CAF2(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=CAF2(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=CAF2(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=CAF2(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=CAF2(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=CAF2(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=CAF2(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=CAF2(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=CAF2(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.19) THEN
C     INTERPOLATE MGO
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=MGO(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=MGO(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=MGO(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=MGO(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=MGO(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=MGO(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=MGO(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=MGO(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=MGO(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=MGO(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.20) THEN
C     INTERPOLATE BAF2
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=BAF2(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=BAF2(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=BAF2(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=BAF2(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=BAF2(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=BAF2(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=BAF2(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=BAF2(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=BAF2(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=BAF2(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.21) THEN
C     INTERPOLATE KBR
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=KBR(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=KBR(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=KBR(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=KBR(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=KBR(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=KBR(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=KBR(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=KBR(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=KBR(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=KBR(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.22) THEN
C     INTERPOLATE CSI
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=CSI(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=CSI(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=CSI(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=CSI(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=CSI(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=CSI(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=CSI(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=CSI(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=CSI(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=CSI(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.23) THEN
C     INTERPOLATE CSBR
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=CSBR(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=CSBR(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=CSBR(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=CSBR(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=CSBR(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=CSBR(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=CSBR(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=CSBR(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=CSBR(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=CSBR(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.24) THEN
C     INTERPOLATE KRS5
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=KRS5(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=KRS5(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=KRS5(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=KRS5(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=KRS5(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=KRS5(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=KRS5(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=KRS5(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=KRS5(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=KRS5(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.25) THEN
C     INTERPOLATE THE LiF
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=LIF(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=LIF(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=LIF(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=LIF(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=LIF(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=LIF(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=LIF(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=LIF(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=LIF(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=LIF(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
          END IF
          IF(MTYPE.EQ.26) THEN
C     INTERPOLATE THE NaCl
              N=60
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR NaCl
C
              DATA (NACLX(J), J = 1,60)/0.589D0,0.64D0,0.6874D0,0.7604D0
     1        ,0.7858D0,0.8835D0,0.9033D0,0.9724D0,1.0084D0,1.0540D0
     2        ,1.0810D0,1.1058D0,1.1420D0,1.1786D0,1.2016D0,1.2604D0
     3        ,1.3126D0,1.4874D0,1.5552D0,1.6368D0,1.6848D0,1.7670D0
     4        ,2.0736D0,2.1824D0,2.2464D0,2.3560D0,2.6505D0,2.9466D0
     5        ,3.2736D0,3.5359D0,3.6288D0,3.8192D0,4.1230D0,4.7120D0
     6        ,5.0092D0,5.3009D0,5.8932D0,6.4825D0,6.8000D0,7.0718D0
     7        ,7.2200D0,7.5900D0,7.6611D0,7.9558D0,8.0400D0,8.8398D0
     8        ,9.0000D0,9.5000D0,10.0184D0,11.7864D0,12.5D0,12.9650D0
     9        ,13.50D0,14.1436D0,14.7330D0,15.3223D0,15.9116D0,17.93D0
     1        ,20.57D0,22.3D0/
C
              DATA (NACLY(J), J = 1,60)/1.54427D0,1.54141D0,1.53930D0
     1        ,1.53682D0,1.53607D0,1.53395D0,1.53361D0,1.53253D0,1.53206D0
     2        ,1.53153D0,1.53123D0,1.53098D0,1.53063D0,1.53031D0,1.53014D0
     3        ,1.52971D0,1.52937D0,1.52845D0,1.52815D0,1.52781D0,1.52764D0
     4        ,1.52736D0,1.52649D0,1.52621D0,1.52606D0,1.52579D0,1.52512D0
     5        ,1.52466D0,1.52371D0,1.52312D0,1.52286D0,1.52238D0,1.52156D0
     6        ,1.51979D0,1.51883D0,1.51790D0,1.51593D0,1.51347D0,1.51200D0
     7        ,1.51093D0,1.51020D0,1.50850D0,1.50822D0,1.50665D0,1.50640D0
     8        ,1.50192D0,1.50100D0,1.49980D0,1.49462D0,1.48171D0,1.47568D0
     9        ,1.47160D0,1.46660D0,1.46044D0,1.45427D0,1.44743D0,1.44090D0
     &        ,1.41490D0,1.37350D0,1.34030D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=NACLX(1:N)
              Y(1:N)=NACLY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.27) THEN
C     INTERPOLATE THE SiO2o
              N=29
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR SiO2o
C
              DATA (SIO2OX(J), J = 1,29)/0.185D0,0.198D0,0.231D0,0.34D0
     1        ,0.394D0,0.434D0,0.508D0,0.5893D0,0.768D0,0.8325D0,0.9914D0
     2        ,1.1592D0,1.3070D0,1.3958D0,1.4792D0,1.5414D0,1.6815D0
     3        ,1.7614D0,1.9457D0,2.0531D0,2.3D0,2.6D0,3.0D0,3.5D0,4.0D0
     4        ,4.2D0,5.0D0,6.45D0,7.0D0/
C
              DATA (SIO2OY(J), J = 1,29)/1.65751D0,1.65087D0,1.61395D0
     1        ,1.56747D0,1.55846D0,1.55396D0,1.54822D0,1.54424D0,1.53903D0
     2        ,1.53773D0,1.53514D0,1.53283D0,1.53090D0,1.52877D0,1.52865D0
     3        ,1.52781D0,1.52583D0,1.52468D0,1.52184D0,1.52005D0,1.51561D0
     4        ,1.50986D0,1.49953D0,1.48451D0,1.46617D0,1.4569D0,1.417D0,1.274D0
     5        ,1.167D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=SIO2OX(1:N)
              Y(1:N)=SIO2OY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.28) THEN
C     INTERPOLATE THE SiO2e
              N=20
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR SiO2e
C
              DATA (SIO2EX(J), J = 1,20)/0.185D0,0.198D0,0.231D0,0.34D0
     1        ,0.394D0,0.434D0,0.508D0,0.5893D0,0.768D0,0.8325D0,0.9914D0
     2        ,1.1592D0,1.3070D0,1.3958D0,1.4792D0,1.5414D0,1.6815D0
     3        ,1.7614D0,1.9457D0,2.0531D0/
C
              DATA (SIO2EY(J), J = 1,20)/1.68988D0,1.66394D0,1.62555D0
     1        ,1.57737D0,1.56805D0,1.56339D0,1.55746D0,1.55335D0,1.54794D0
     2        ,1.54661D0,1.54392D0,1.54152D0,1.53951D0,1.53832D0,1.53716D0
     3        ,1.53630D0,1.53422D0,1.53301D0,1.53004D0,1.52823D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=SIO2EX(1:N)
              Y(1:N)=SIO2EY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.29) THEN
C     INTERPOLATE THE VIR3
              N=18
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR VIR3
C
              DATA (VIR3X(J), J = 1,18)/0.4047D0,0.5461D0,0.7065D0,1.0D0
     1        ,1.0D0,2.0D0,2.5D0,3.0D0,3.25D0,3.5D0,3.75D0,4.0D0,4.25D0
     2        ,4.5D0,4.75D0,5.0D0,5.5D0,6.0D0/
C
              DATA (VIR3Y(J), J = 1,18)/1.92568D0,1.87002D0,1.84694D0
     1        ,1.831D0,1.818D0,1.812D0,1.806D0,1.799D0,1.795D0,1.791D0
     2        ,1.786D0,1.781D0,1.775D0,1.769D0,1.762D0,1.756D0,1.741D0,1.725D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=VIR3X(1:N)
              Y(1:N)=VIR3Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.30) THEN
C     INTERPOLATE THE 9754
              N=43
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR 9754
C
              DATA (G9754X(J), J = 1,43)/0.4D0,0.425D0,0.45D0
     1        ,0.475D0,0.5D0,0.525D0
     1        ,0.55D0,0.575D0,0.6D0,0.635D0,0.65D0,0.675D0,0.7D0,0.725D0,0.75D0
     2        ,0.775D0,0.8D0,0.825D0,0.85D0,0.875D0,0.9D0,0.925D0,0.95D0
     3        ,0.975D0,1.0D0,1.25D0,1.5D0,1.75D0,2.0D0,2.25D0,2.5D0,2.75D0
     4        ,3.0D0,3.25D0,3.5D0,3.75D0,4.0D0,4.25D0,4.5D0,4.75D0,5.0D0
     5        ,5.25D0,5.5D0/
C
              DATA (G9754Y(J), J = 1,43)/1.69093D0,1.68502D0,1.68020D0
     1        ,1.67621D0,1.67285D0,1.67000D0,1.66754D0,1.66542D0,1.66356D0
     2        ,1.66192D0,1.66046D0,1.65916D0,1.65800D0,1.65694D0,1.65599D0
     3        ,1.65511D0,1.65431D0,1.65358D0,1.65289D0,1.65226D0,1.65167D0
     4        ,1.65112D0,1.65060D0,1.65011D0,1.64964D0,1.64595D0,1.64310D0
     5        ,1.64049D0,1.63785D0,1.63505D0,1.63203D0,1.62874D0,1.62514D0
     6        ,1.62119D0,1.61686D0,1.61214D0,1.60698D0,1.60135D0,1.59521D0
     7        ,1.58853D0,1.58125D0,1.57332D0,1.66469D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=G9754X(1:N)
              Y(1:N)=G9754Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.31) THEN
C     INTERPOLATE THE ALON
              N=14
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR ALON
C
              DATA (ALONX(J), J = 1,14)/0.365D0,0.405D0,0.435D0,0.546D0
     1        ,0.852D0,1.014D0,1.53D0,1.97D0,2.325D0,2.8D0,3.39D0,4.0D0
     2        ,4.6D0,5.0D0/
C
              DATA (ALONY(J), J = 1,14)/1.819D0,1.811D0,1.806D0,1.792D0
     1        ,1.778D0,1.773D0,1.765D0,1.758D0,1.752D0,1.743D0,1.729D0
     2        ,1.710D0,1.689D0,1.672D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=ALONX(1:N)
              Y(1:N)=ALONY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.32) THEN
C     INTERPOLATE THE SPINEL
              N=11
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR SPINEL
C
              DATA (SPINX(J), J = 1,11)/0.4047D0,0.4358D0,0.5461D0,0.8521D0
     1        ,1.014D0,1.53D0,1.97D0,3.0D0,4.0D0,5.0D0,5.5D0/
C
              DATA (SPINY(J), J = 1,11)/1.73574D0,1.73054D0,1.71896D0
     2        ,1.70728D0,1.703D0,1.69468D0,1.68763D0,1.6647D0,1.6414D0
     3        ,1.5978D0,1.5719D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=SPINX(1:N)
              Y(1:N)=SPINY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.33) THEN
C     INTERPOLATE THE CALCIUM ALUMINATE GLASS
              N=12
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR CALAL
C
              DATA (CALALX(J), J = 1,12)/0.4861D0,0.5893D0,0.6563D0,0.8D0
     1        ,1.0D0,1.5D0,2.0D0,2.5D0,3.0D0,3.5D0,4.0D0,4.5D0/
C
              DATA (CALALY(J), J = 1,12)/1.6794D0,1.669D0,1.6647D0,1.6588D0
     1        ,1.6538D0,1.6463D0,1.6403D0,1.6341D0,1.6266D0,1.6180D0
     2        ,1.6074D0,1.5952D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=CALALX(1:N)
              Y(1:N)=CALALY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.34) THEN
C     INTERPOLATE THE B270 GLASS
              N=8
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR B270
C
              DATA (B270X(J), J = 1,8)/
     1        0.4358343D0,0.4799914D0,0.4861327D0
     1        ,0.5460740D0,0.5875618D0,0.5892938D0,0.6438469D0,0.6562725D0/
C
              DATA (B270Y(J), J = 1,8)/
     1        1.534D0,1.5297D0,1.5292D0,1.5251D0
     2        ,1.523D0,1.5229D0,1.5207D0,1.5202D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=B270X(1:N)
              Y(1:N)=B270Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.35) THEN
C     INTERPOLATE THE IRG2 GLASS
              N=18
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR IRG2
C
              DATA (IRG2X(J), J = 1,18)/0.365D0,0.4047D0,0.48D0,0.4861D0
     1        ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2        ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
C
              DATA (IRG2Y(J), J = 1,18)/1.9750D0,1.9462D0,1.9147D0,1.9129D0
     1        ,1.8988D0,1.8918D0,1.8845D0,1.8832D0,1.8785D0,1.8692D0,1.8630D0
     2        ,1.8526D0,1.8464D0,1.8414D0,1.8362D0,1.8253D0,1.8041D0,1.7954D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=IRG2X(1:N)
              Y(1:N)=IRG2Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.36) THEN
C     INTERPOLATE THE IRG3 GLASS
              N=17
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR IRG3
C
              DATA (IRG3X(J), J = 1,17)/0.4047D0,0.48D0,0.4861D0
     1        ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2        ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
C
              DATA (IRG3Y(J), J = 1,17)/1.8925D0,1.8649D0,1.8633D0,1.8510D0
     1        ,1.8449D0,1.8385D0,1.8373D0,1.8331D0,1.8249D0,1.8193D0,1.8089D0
     2        ,1.8021D0,1.7963D0,1.7900D0,1.7764D0,1.7491D0,1.7375D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=IRG3X(1:N)
              Y(1:N)=IRG3Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.37) THEN
C     INTERPOLATE THE IRGN6 GLASS
              N=16
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR IRGN6
C
              DATA (IRGN6X(J), J = 1,16)/0.4047D0,0.48D0,0.4861D0
     1        ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2        ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0/
C
              DATA (IRGN6Y(J), J = 1,16)/1.6069D0,1.5971D0,1.5965D0,1.5915D0
     1        ,1.5892D0,1.5863D0,1.5857D0,1.5842D0,1.5807D0,1.5777D0,1.5716D0
     2        ,1.5667D0,1.5620D0,1.5567D0,1.5451D0,1.5209D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=IRGN6X(1:N)
              Y(1:N)=IRGN6Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.38) THEN
C     INTERPOLATE THE IRG7 GLASS
              N=16
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR IRG7
C
              DATA (IRG7X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0
     1        ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2        ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
C
              DATA (IRG7Y(J), J = 1,16)/1.5983D0,1.5871D0,1.5743D0,1.5735D0
     1        ,1.5675D0,1.5644D0,1.5612D0,1.5606D0,1.5585D0,1.5541D0,1.5509D0
     2        ,1.5442D0,1.5389D0,1.5341D0,1.5286D0,1.5164D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=IRG7X(1:N)
              Y(1:N)=IRG7Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.39) THEN
C     INTERPOLATE THE IRG9 GLASS
              N=16
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR IRG9
C
              DATA (IRG9X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0
     1        ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2        ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
C
              DATA (IRG9Y(J), J = 1,16)/1.5005D0,1.4961D0,1.4905D0,1.4902D0
     1        ,1.4875D0,1.4861D0,1.4845D0,1.4842D0,1.4832D0,1.4810D0,1.4793D0
     2        ,1.4755D0,1.4722D0,1.4692D0,1.4658D0,1.4583D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=IRG9X(1:N)
              Y(1:N)=IRG9Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.40) THEN
C     INTERPOLATE THE IRG11 GLASS
              N=16
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR IRG11
C
              DATA (IRG11X(J), J = 1,16)/0.48D0,0.4861D0
     1        ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2        ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
C
              DATA (IRG11Y(J), J = 1,16)/1.6926D0,1.6917D0,1.6845D0,1.6809D0
     1        ,1.6770D0,1.6763D0,1.6741D0,1.6686D0,1.6650D0,1.6581D0,1.6532D0
     2        ,1.6491D0,1.6445D0,1.6349D0,1.6158D0,1.6077D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=IRG11X(1:N)
              Y(1:N)=IRG11Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.41) THEN
C     INTERPOLATE THE IRG15 GLASS
              N=16
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR IRG15
C
              DATA (IRG15X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0
     1        ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2        ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
C
              DATA (IRG15Y(J), J = 1,16)/1.5883D0,1.5506D0,1.5415D0,1.5410D0
     1        ,1.5366D0,1.5343D0,1.5318D0,1.5314D0,1.5297D0,1.5263D0,1.5237D0
     2        ,1.5179D0,1.5131D0,1.5086D0,1.5038D0,1.4924D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=IRG15X(1:N)
              Y(1:N)=IRG15Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.42) THEN
C     INTERPOLATE THE VAC GLASS
              A=28.79D-5
              B=5.67D-5
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  LA1=LM1(A,B,SYSTEM1(1))
                  LA1=1.0D0+LA1
                  ALENS(46,I)=1.0D0/LA1
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  LA2=LM1(A,B,SYSTEM1(2))
                  LA2=1.0D0+LA2
                  ALENS(47,I)=1.0D0/LA2
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  LA3=LM1(A,B,SYSTEM1(3))
                  LA3=1.0D0+LA3
                  ALENS(48,I)=1.0D0/LA3
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  LA4=LM1(A,B,SYSTEM1(4))
                  LA4=1.0D0+LA4
                  ALENS(49,I)=1.0D0/LA4
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  LA5=LM1(A,B,SYSTEM1(5))
                  LA5=1.0D0+LA5
                  ALENS(50,I)=1.0D0/LA5
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  LA5=LM1(A,B,SYSTEM1(71))
                  LA5=1.0D0+LA5
                  ALENS(71,I)=1.0D0/LA5
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  LA5=LM1(A,B,SYSTEM1(72))
                  LA5=1.0D0+LA5
                  ALENS(72,I)=1.0D0/LA5
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  LA5=LM1(A,B,SYSTEM1(73))
                  LA5=1.0D0+LA5
                  ALENS(73,I)=1.0D0/LA5
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  LA5=LM1(A,B,SYSTEM1(74))
                  LA5=1.0D0+LA5
                  ALENS(74,I)=1.0D0/LA5
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  LA5=LM1(A,B,SYSTEM1(75))
                  LA5=1.0D0+LA5
                  ALENS(75,I)=1.0D0/LA5
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.43) THEN
C     INTERPOLATE THE H2O
              N=21
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR H2O
C
              DATA (H2OX(J), J = 1,21)/0.1829D0,0.20255D0,0.25020D0,0.30822D0,
     1        0.35871D0,0.40466D0,0.44715D0,0.50157D0,0.54607D0,0.58926D0,
     2        0.65628D0,0.70652D0,0.76820D0,0.808D0,0.871D0,0.943D0,1.028D0,
     3        1.130D0,1.256D0,1.617D0,1.968/
C
              DATA (H2OY(J), J = 1,21)/1.46379D0,1.41993D0,1.37734D0,1.35671D0,
     1        1.34795D0,1.342724D0,1.339423D0,1.336363D0,1.334466D0,1.332988D0,
     2        1.331151D0,1.330019D0,1.32890D0,1.3286D0,1.3273D0,1.3262D0,
     3        1.3250D0,1.3234D0,1.3215D0,1.3149D0,1.3078D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=H2OX(1:N)
              Y(1:N)=H2OY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.44) THEN
C     INTERPOLATE THE SUPRASIL
              N=60
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR SUPRASIL
C
              DATA (SUPX(J), J = 1,60)/0.19D0,0.20D0,0.21D0,0.22D0,
     1        0.23D0,0.24D0,0.25D0,0.26D0,0.27D0,0.28D0,
     2        0.29D0,0.30D0,0.32D0,0.34D0,0.36D0,0.36548D0,0.38D0,
     3        0.4D0,0.40465D0,0.43583D0,0.45D0,0.48613D0,0.5D0,0.54607D0,
     4        0.55D0,0.58756D0,0.6D0,0.65D0,0.65627D0,0.7D0,0.75D0,0.8D0,
     5        0.85D0,0.9D0,1.0D0,1.1D0,1.2D0,1.3D0,1.4D0,1.5D0,1.6D0,1.7D0,
     6        1.8D0,1.9D0,2.0D0,2.1D0,2.2D0,2.3D0,2.4D0,2.5D0,2.6D0,2.7D0,
     7        2.8D0,2.9D0,3.0D0,3.1D0,3.2D0,3.3D0,3.4D0,3.5D0/
C
              DATA (SUPY(J), J = 1,60)/
     1        1.56572D0,1.55051D0,1.53836D0,1.52845D0,1.52024D0,1.51333D0,
     2        1.50745D0,1.50239D0,1.49800D0,1.49416D0,1.49079D0,1.48779D0,
     3        1.48274D0,1.47865D0,1.47529D0,1.47447D0,1.47248D0,1.47012D0,
     4        1.46962D0,1.46669D0,1.46557D0,1.46313D0,1.46233D0,1.46008D0,
     5        1.45991D0,1.45846D0,1.45804D0,1.45653D0,1.45637D0,1.45529D0,
     6        1.45424D0,1.45332D0,1.45250D0,1.45175D0,1.45042D0,1.44920D0,
     7        1.44805D0,1.44692D0,1.44758D0,1.44462D0,1.44342D0,1.44217D0,
     8        1.44087D0,1.43951D0,1.43809D0,1.43659D0,1.43501D0,1.43336D0,
     9        1.43163D0,1.42980D0,1.42789D0,1.42588D0,1.42377D0,1.42156D0,
     1        1.41925D0,1.41682D0,1.41427D0,1.41161D0,1.40881D0,1.40589D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=SUPX(1:N)
              Y(1:N)=SUPY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.45) THEN
C     INTERPOLATE THE HOMOSIL
              N=47
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR SUPRASIL
C
              DATA (HOMOX(J), J = 1,47)/
     1        0.34D0,0.36D0,0.36548D0,0.38D0,
     3        0.4D0,0.40465D0,0.43583D0,0.45D0,0.48613D0,0.5D0,0.54607D0,
     4        0.55D0,0.58756D0,0.6D0,0.65D0,0.65627D0,0.7D0,0.75D0,0.8D0,
     5        0.85D0,0.9D0,1.0D0,1.1D0,1.2D0,1.3D0,1.4D0,1.5D0,1.6D0,1.7D0,
     6        1.8D0,1.9D0,2.0D0,2.1D0,2.2D0,2.3D0,2.4D0,2.5D0,2.6D0,2.7D0,
     7        2.8D0,2.9D0,3.0D0,3.1D0,3.2D0,3.3D0,3.4D0,3.5D0/
C
              DATA (HOMOY(J), J = 1,47)/1.47881D0,1.47544D0,1.47462D0,
     1        1.47262D0,1.47025D0,1.46975D0,1.46681D0,1.46568D0,1.46324D0,
     2        1.46243D0,1.46018D0,1.46001D0,1.45856D0,1.45814D0,1.45663D0,
     3        1.45646D0,1.45539D0,1.45433D0,1.45341D0,1.45259D0,1.45185D0,
     4        1.45051D0,1.44930D0,1.44815D0,1.44702D0,1.44589D0,1.44473D0,
     5        1.44353D0,1.44229D0,1.44099D0,1.43964D0,1.43821D0,1.43672D0,
     6        1.43515D0,1.43350D0,1.43177D0,1.42995D0,1.42804D0,1.42604D0,
     7        1.42393D0,1.42172D0,1.41941D0,1.41698D0,1.41444D0,1.41177D0,
     8        1.40897D0,1.40605D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=HOMOX(1:N)
              Y(1:N)=HOMOY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.46) THEN
C     INTERPOLATE THE II-IV ZNS-MS
              N=59
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR ZNS-MS
C
              DATA (ZNSMSX(J), J = 1,59)/
     1        0.42D0,0.46D0,0.50D0,0.54D0,0.58D0,0.62D0,0.66D0,0.70D0,
     2        0.74D0,0.78D0,0.82D0,0.86D0,0.90D0,0.94D0,0.98D0,1.00D0,
     3        1.40D0,1.80D0,2.20D0,2.60D0,3.00D0,3.40D0,3.80D0,4.20D0,
     4        4.60D0,5.00D0,5.40D0,5.80D0,6.20D0,6.60D0,7.00D0,7.40D0,
     5        7.80D0,8.20D0,8.60D0,9.00D0,9.40D0,9.80D0,10.20D0,10.60D0,
     6        11.00D0,11.40D0,11.80D0,12.20D0,12.60D0,13.0D0,13.40D0,13.80D0,
     7        14.20D0,14.60D0,15.00D0,15.40D0,15.80D0,16.2D0,16.60D0,17.00D0,
     8        17.40D0,17.80D0,18.20D0/
C
              DATA (ZNSMSY(J), J = 1,59)/
     1        2.516D0,2.458D0,2.419D0,2.391D0,2.371D0,2.355D0,2.342D0,2.332D0,
     2        2.323D0,2.316D0,2.310D0,2.305D0,2.301D0,2.297D0,2.294D0,2.292D0,
     3        2.275D0,2.267D0,2.263D0,2.260D0,2.257D0,2.255D0,2.253D0,2.251D0,
     4        2.248D0,2.246D0,2.244D0,2.241D0,2.238D0,2.235D0,2.232D0,2.228D0,
     5        2.225D0,2.221D0,2.217D0,2.212D0,2.208D0,2.203D0,2.198D0,2.192D0,
     6        2.186D0,2.180D0,2.173D0,2.167D0,2.159D0,2.152D0,2.143D0,2.135D0,
     7        2.126D0,2.116D0,2.106D0,2.095D0,2.084D0,2.072D0,2.059D0,2.045D0,
     8        2.030D0,2.015D0,1.998D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=ZNSMSX(1:N)
              Y(1:N)=ZNSMSY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
C
          IF(MTYPE.EQ.47) THEN
C     INTERPOLATE THE CEF3
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(SYSTEM1(1)-2.0D0))+1.59D0
                  IF(SYSTEM1(1).LE.0.55D0) ALENS(46,I)=1.63D0
                  IF(SYSTEM1(1).GE.2.00D0) ALENS(46,I)=1.59D0
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(SYSTEM1(2)-2.0D0))+1.59D0
                  IF(SYSTEM1(2).LE.0.55D0) ALENS(47,I)=1.63D0
                  IF(SYSTEM1(2).GE.2.00D0) ALENS(47,I)=1.59D0
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(SYSTEM1(3)-2.0D0))+1.59D0
                  IF(SYSTEM1(3).LE.0.55D0) ALENS(48,I)=1.63D0
                  IF(SYSTEM1(3).GE.2.00D0) ALENS(48,I)=1.59D0
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(SYSTEM1(4)-2.0D0))+1.59D0
                  IF(SYSTEM1(4).LE.0.55D0) ALENS(49,I)=1.63D0
                  IF(SYSTEM1(4).GE.2.00D0) ALENS(49,I)=1.59D0
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(SYSTEM1(5)-2.0D0))+1.59D0
                  IF(SYSTEM1(5).LE.0.55D0) ALENS(50,I)=1.63D0
                  IF(SYSTEM1(5).GE.2.00D0) ALENS(50,I)=1.59D0
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(SYSTEM1(71)-2.0D0))+1.590
                  IF(SYSTEM1(71).LE.0.55D0) ALENS(71,I)=1.63D0
                  IF(SYSTEM1(71).GE.2.00D0) ALENS(71,I)=1.59D0
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(SYSTEM1(72)-2.0D0))+1.590
                  IF(SYSTEM1(72).LE.0.55D0) ALENS(72,I)=1.63D0
                  IF(SYSTEM1(72).GE.2.00D0) ALENS(72,I)=1.59D0
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(SYSTEM1(73)-2.0D0))+1.590
                  IF(SYSTEM1(73).LE.0.55D0) ALENS(73,I)=1.63D0
                  IF(SYSTEM1(73).GE.2.00D0) ALENS(73,I)=1.59D0
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(SYSTEM1(74)-2.0D0))+1.590
                  IF(SYSTEM1(74).LE.0.55D0) ALENS(74,I)=1.63D0
                  IF(SYSTEM1(74).GE.2.00D0) ALENS(74,I)=1.59D0
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(SYSTEM1(75)-2.0D0))+1.590
                  IF(SYSTEM1(75).LE.0.55D0) ALENS(75,I)=1.63D0
                  IF(SYSTEM1(75).GE.2.00D0) ALENS(75,I)=1.59D0
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.48) THEN
C     INTERPOLATE THE LS203
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(SYSTEM1(1)-2.0D0))+1.86D0
                  IF(SYSTEM1(1).LE.0.55D0) ALENS(46,I)=1.95D0
                  IF(SYSTEM1(1).GE.2.00D0) ALENS(46,I)=1.86D0
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(SYSTEM1(2)-2.0D0))+1.86D0
                  IF(SYSTEM1(2).LE.0.55D0) ALENS(47,I)=1.95D0
                  IF(SYSTEM1(2).GE.2.00D0) ALENS(47,I)=1.86D0
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(SYSTEM1(3)-2.0D0))+1.86D0
                  IF(SYSTEM1(3).LE.0.55D0) ALENS(48,I)=1.95D0
                  IF(SYSTEM1(3).GE.2.00D0) ALENS(48,I)=1.86D0
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(SYSTEM1(4)-2.0D0))+1.86D0
                  IF(SYSTEM1(4).LE.0.55D0) ALENS(49,I)=1.95D0
                  IF(SYSTEM1(4).GE.2.00D0) ALENS(49,I)=1.86D0
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(SYSTEM1(5)-2.0D0))+1.86D0
                  IF(SYSTEM1(5).LE.0.55D0) ALENS(50,I)=1.95D0
                  IF(SYSTEM1(5).GE.2.00D0) ALENS(50,I)=1.86D0
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(SYSTEM1(71)-2.0D0))+1.860
                  IF(SYSTEM1(71).LE.0.55D0) ALENS(71,I)=1.95D0
                  IF(SYSTEM1(71).GE.2.00D0) ALENS(71,I)=1.86D0
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(SYSTEM1(72)-2.0D0))+1.860
                  IF(SYSTEM1(72).LE.0.55D0) ALENS(72,I)=1.95D0
                  IF(SYSTEM1(72).GE.2.00D0) ALENS(72,I)=1.86D0
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(SYSTEM1(73)-2.0D0))+1.860
                  IF(SYSTEM1(73).LE.0.55D0) ALENS(73,I)=1.95D0
                  IF(SYSTEM1(73).GE.2.00D0) ALENS(73,I)=1.86D0
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(SYSTEM1(74)-2.0D0))+1.860
                  IF(SYSTEM1(74).LE.0.55D0) ALENS(74,I)=1.95D0
                  IF(SYSTEM1(74).GE.2.00D0) ALENS(74,I)=1.86D0
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(SYSTEM1(75)-2.0D0))+1.860
                  IF(SYSTEM1(75).LE.0.55D0) ALENS(75,I)=1.95D0
                  IF(SYSTEM1(75).GE.2.00D0) ALENS(75,I)=1.86D0
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.49) THEN
C     INTERPOLATE THE THF4
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(SYSTEM1(1)-.75D0))+1.51D0
                  IF(SYSTEM1(1).LE.0.40D0) ALENS(46,I)=1.52D0
                  IF(SYSTEM1(1).GE.0.75D0) ALENS(46,I)=1.51D0
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(SYSTEM1(2)-.75D0))+1.51D0
                  IF(SYSTEM1(2).LE.0.40D0) ALENS(47,I)=1.52D0
                  IF(SYSTEM1(2).GE.0.75D0) ALENS(47,I)=1.51D0
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(SYSTEM1(3)-.75D0))+1.51D0
                  IF(SYSTEM1(3).LE.0.40D0) ALENS(48,I)=1.52D0
                  IF(SYSTEM1(3).GE.0.75D0) ALENS(48,I)=1.51D0
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(SYSTEM1(4)-.75D0))+1.51D0
                  IF(SYSTEM1(4).LE.0.40D0) ALENS(49,I)=1.52D0
                  IF(SYSTEM1(4).GE.0.75D0) ALENS(49,I)=1.51D0
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(SYSTEM1(5)-.75D0))+1.51D0
                  IF(SYSTEM1(5).LE.0.40D0) ALENS(50,I)=1.52D0
                  IF(SYSTEM1(5).GE.0.75D0) ALENS(50,I)=1.51D0
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(SYSTEM1(71)-.75D0))+1.51D0
                  IF(SYSTEM1(71).LE.0.40D0) ALENS(71,I)=1.52D0
                  IF(SYSTEM1(71).GE.0.75D0) ALENS(71,I)=1.51D0
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(SYSTEM1(72)-.75D0))+1.51D0
                  IF(SYSTEM1(72).LE.0.40D0) ALENS(72,I)=1.52D0
                  IF(SYSTEM1(72).GE.0.75D0) ALENS(72,I)=1.51D0
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(SYSTEM1(73)-.75D0))+1.51D0
                  IF(SYSTEM1(73).LE.0.40D0) ALENS(73,I)=1.52D0
                  IF(SYSTEM1(73).GE.0.75D0) ALENS(73,I)=1.51D0
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(SYSTEM1(74)-.75D0))+1.51D0
                  IF(SYSTEM1(74).LE.0.40D0) ALENS(74,I)=1.52D0
                  IF(SYSTEM1(74).GE.0.75D0) ALENS(74,I)=1.51D0
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(SYSTEM1(75)-.75D0))+1.51D0
                  IF(SYSTEM1(75).LE.0.40D0) ALENS(75,I)=1.52D0
                  IF(SYSTEM1(75).GE.0.75D0) ALENS(75,I)=1.51D0
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.50) THEN
C     INTERPOLATE THE ZRO2
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(SYSTEM1(1)-.75D0))+2.00D0
                  IF(SYSTEM1(1).LE.0 .55D0) ALENS(46,I)=2.10D0
                  IF(SYSTEM1(1).GE..75D0) ALENS(46,I)=2.00D0
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(SYSTEM1(2)-.75D0))+2.00D0
                  IF(SYSTEM1(2).LE.0 .55D0) ALENS(47,I)=2.10D0
                  IF(SYSTEM1(2).GE..75D0) ALENS(47,I)=2.00D0
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(SYSTEM1(3)-.75D0))+2.00D0
                  IF(SYSTEM1(3).LE.0 .55D0) ALENS(48,I)=2.10D0
                  IF(SYSTEM1(3).GE..75D0) ALENS(48,I)=2.00D0
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(SYSTEM1(4)-.75D0))+2.00D0
                  IF(SYSTEM1(4).LE.0 .55D0) ALENS(49,I)=2.10D0
                  IF(SYSTEM1(4).GE..75D0) ALENS(49,I)=2.00D0
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(SYSTEM1(5)-.75D0))+2.00D0
                  IF(SYSTEM1(5).LE.0 .55D0) ALENS(50,I)=2.10D0
                  IF(SYSTEM1(5).GE..75D0) ALENS(50,I)=2.00D0
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(SYSTEM1(71)-.75D0))+2.00D0
                  IF(SYSTEM1(71).LE.0 .55D0) ALENS(71,I)=2.10D0
                  IF(SYSTEM1(71).GE..75D0) ALENS(71,I)=2.00D0
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(SYSTEM1(72)-.75D0))+2.00D0
                  IF(SYSTEM1(72).LE.0 .55D0) ALENS(72,I)=2.10D0
                  IF(SYSTEM1(72).GE..75D0) ALENS(72,I)=2.00D0
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(SYSTEM1(73)-.75D0))+2.00D0
                  IF(SYSTEM1(73).LE.0 .55D0) ALENS(73,I)=2.10D0
                  IF(SYSTEM1(73).GE..75D0) ALENS(73,I)=2.00D0
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(SYSTEM1(74)-.75D0))+2.00D0
                  IF(SYSTEM1(74).LE.0 .55D0) ALENS(74,I)=2.10D0
                  IF(SYSTEM1(74).GE..75D0) ALENS(74,I)=2.00D0
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(SYSTEM1(75)-.75D0))+2.00D0
                  IF(SYSTEM1(75).LE.0 .55D0) ALENS(75,I)=2.10D0
                  IF(SYSTEM1(75).GE..75D0) ALENS(75,I)=2.00D0
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.51) THEN
C     INTERPOLATE THE DIAMOND
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=DIAMOND(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=DIAMOND(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=DIAMOND(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=DIAMOND(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=DIAMOND(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=DIAMOND(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=DIAMOND(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=DIAMOND(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=DIAMOND(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=DIAMOND(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
          END IF
          IF(MTYPE.EQ.52) THEN
C     INTERPOLATE YAG
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  ALENS(46,I)=YAG(SYSTEM1(1))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  ALENS(47,I)=YAG(SYSTEM1(2))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  ALENS(48,I)=YAG(SYSTEM1(3))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  ALENS(49,I)=YAG(SYSTEM1(4))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  ALENS(50,I)=YAG(SYSTEM1(5))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  ALENS(71,I)=YAG(SYSTEM1(71))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  ALENS(72,I)=YAG(SYSTEM1(72))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  ALENS(73,I)=YAG(SYSTEM1(73))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  ALENS(74,I)=YAG(SYSTEM1(74))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  ALENS(75,I)=YAG(SYSTEM1(75))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.GE.101.AND.MTYPE.LE.104) THEN
C
C     PLASTICS
C
              DATA (GPLX(J), J = 1,13)/0.36501D0,0.40466D0,0.43484D0
     1        ,0.47999D0,0.48613D0,0.54607D0,0.58756D0,0.58929D0,0.64385D0
     2        ,0.65627D0,0.70652D0,0.85211D0,1.01398D0/
C
              DATA (GPL1Y(J), J = 1,13)/1.513613D0,1.506607D0,1.502557D0
     1        ,1.498258D0,1.497760D0,1.493795D0,1.491757D0,1.491681D0
     2        ,1.489603D0,1.489201D0,1.487552D0,1.484965D0,1.483115D0/
C
              DATA (GPL2Y(J), J = 1,13)/1.643126D0,1.625341D0,1.615466D0
     1        ,1.605241D0,1.604079D0,1.595010D0,1.590481D0,1.590315D0
     2        ,1.585808D0,1.584949D0,1.581954D0,1.576196D0,1.572553D0/
C
              DATA (GPL3Y(J), J = 1,13)/1.643231D0,1.622447D0,1.611519D0
     1        ,1.600654D0,1.599439D0,1.590081D0,1.585470D0,1.585302D0
     2        ,1.580734D0,1.579864D0,1.576831D0,1.570981D0,1.567248D0/
C
              DATA (GPL4Y(J), J = 1,13)/1.612490D0,1.597075D0,1.588640D0
     1        ,1.579985D0,1.579000D0,1.571300D0,1.567400D0,1.567298D0
     2        ,1.563438D0,1.562700D0,1.560119D0,1.555108D0,1.551870D0/
C
              N=13
C
              IF(MTYPE.EQ.101) THEN
C     INTERPOLATE THE ACRYLIC DATA
                  X(1:N)=GPLX(1:N)
                  Y(1:N)=GPL1Y(1:N)
              END IF
              IF(MTYPE.EQ.102) THEN
C     INTERPOLATE THE POLYSTYRENE DATA
                  X(1:N)=GPLX(1:N)
                  Y(1:N)=GPL2Y(1:N)
              END IF
              IF(MTYPE.EQ.103) THEN
C     INTERPOLATE THE POLYCARBONATE DATA
                  X(1:N)=GPLX(1:N)
                  Y(1:N)=GPL3Y(1:N)
              END IF
              IF(MTYPE.EQ.104) THEN
C     INTERPOLATE THE (SAN) DATA
                  X(1:N)=GPLX(1:N)
                  Y(1:N)=GPL4Y(1:N)
              END IF
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     MOVE TO LARGE ARRAYS
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              IF(SYSTEM1(1).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(1),ALENS(46,I))
              ELSE
                  ALENS(46,I)=1.0D0
              END IF
              IF(SYSTEM1(2).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(2),ALENS(47,I))
              ELSE
                  ALENS(47,I)=1.0D0
              END IF
              IF(SYSTEM1(3).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(3),ALENS(48,I))
              ELSE
                  ALENS(48,I)=1.0D0
              END IF
              IF(SYSTEM1(4).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(4),ALENS(49,I))
              ELSE
                  ALENS(49,I)=1.0D0
              END IF
              IF(SYSTEM1(5).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(5),ALENS(50,I))
              ELSE
                  ALENS(50,I)=1.0D0
              END IF
              IF(SYSTEM1(71).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(71),ALENS(71,I))
              ELSE
                  ALENS(71,I)=1.0D0
              END IF
              IF(SYSTEM1(72).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(72),ALENS(72,I))
              ELSE
                  ALENS(72,I)=1.0D0
              END IF
              IF(SYSTEM1(73).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(73),ALENS(73,I))
              ELSE
                  ALENS(73,I)=1.0D0
              END IF
              IF(SYSTEM1(74).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(74),ALENS(74,I))
              ELSE
                  ALENS(74,I)=1.0D0
              END IF
              IF(SYSTEM1(75).NE.0.0D0) THEN
                  CALL SPLINT(X,Y,Y2,N,SYSTEM1(75),ALENS(75,I))
              ELSE
                  ALENS(75,I)=1.0D0
              END IF
C
          END IF
          RETURN
      END
C SUB SPCGL.FOR
      SUBROUTINE SPCGL(MTYPE)
          IMPLICIT NONE
C
          INTEGER MTYPE,J,N
C
          REAL*8 X(1:100),Y(1:100),Y2(1:100),LA,CSI
     1    ,YPN,YP1,GSCX(1:20),GSCY(1:20),GPCX(1:17),GPCY(1:17)
     2    ,SIL,GPLX(1:13),GPL1Y(1:13),GPL2Y(1:13),VIR3X(1:18),VIR3Y(1:18)
     3    ,GPL3Y(1:13),GPL4Y(1:13),IRGX(1:25),IRGY(1:25),ZNSEX(1:56)
     4    ,ZNSEY(1:56),ZNSX(1:59),ZNSY(1:59),CLRX(1:29),CLRY(1:29)
     5    ,SAPH,FSIL,MGF2O,MGF2E,CAF2,MGO,G9754X(1:43),G9754Y(1:43)
     6    ,AMTR1X(1:17),AMTR1Y(1:17),DYN,BAF2,KBR,CSBR,KRS5
     7    ,AMTR3X(1:12),AMTR3Y(1:12),AS2S3,ALONX(1:14),ALONY(1:14)
     8    ,GAASX(1:13),GAASY(1:13),CDTEX(1:10),CDTEY(1:10)
     9    ,SPINX(1:11),SPINY(1:11),CALALX(1:12),CALALY(1:12)
     1    ,DUMA,DUMB,DUML,ZNSMSX(1:59),ZNSMSY(1:59),YAG
C
          REAL*8 SIO2OX(1:29),SIO2OY(1:29),SIO2EX(1:20)
     1    ,SIO2EY(1:20),LIF,NACLX(1:60),NACLY(1:60),DIAMOND
     1    ,B270X(1:8),B270Y(1:8),LM1,LA1,LA2,LA3,LA4,LA5,A,B,H2OX(1:21)
     1    ,H2OY(1:21),SUPX(1:60),SUPY(1:60),HOMOX(1:47),HOMOY(1:47)
     2    ,IRG2X(1:18),IRG2Y(1:18),IRG3X(1:17),IRG3Y(1:17),IRGN6X(1:16)
     3    ,IRGN6Y(1:16),IRG7X(1:16),IRG7Y(1:16),IRG9X(1:16),IRG9Y(1:16)
     4    ,IRG11X(1:16),IRG11Y(1:16),IRG15X(1:16),IRG15Y(1:16)
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     VACUME
          LM1(DUMA,DUMB,DUML)=DUMA*(1.0D0+(DUMB/(DUML**2)))
C
C     SILICA/SIO2 INTERPOLATION FORMULA (HANDBOOK OF OPTICS 11/14/2000)
          FSIL(LA)=DSQRT(1.0D0+
     1    ((0.6961663*(LA**2))/((LA**2)-((0.0684043)**2)))+
     1    ((0.4079426*(LA**2))/((LA**2)-((0.1162414)**2)))+
     1    ((0.8974794*(LA**2))/((LA**2)-((9.896161)**2)))
     1    )

C
C     DIAMOND INTERPOLATION FORMULA
          DIAMOND(LA)=
     1    2.37837D0+
     1    ((1.18897E-2)*(1.0D0/((LA**2)-0.028D0)))+
     2    ((-1.0083E-4)*((1.0D0/((LA**2)-0.028D0))**2))+
     3    ((-2.3676E-5)*(LA**2))+
     4    ((3.24263E-8)*(LA**4))
C
C     LiF INTERPOLATION FORMULA
          LIF(LA)=
     1     1.38761D0+(0.001796D0/((LA**2)-0.028D0))
     2     +(0.000041D0/(((LA**2)-0.028D0)**2))-(0.0023045D0*(LA**2))
     3    -(0.00000557D0*(LA**4))
C
C     SILICON INTERPOLATION FORMULA
          SIL(LA)=
     1     3.41696D0+(0.138497D0/((LA**2)-0.028D0))
     2     +(0.013924D0/(((LA**2)-0.028D0)**2))-(0.0000209D0*(LA**2))
     3    +(0.000000148D0*(LA**4))
C
C     KBr INTERPOLATION FORMULA
          KBR(LA)=DSQRT(
     1     2.361323D0-(3.11497D-4*(LA**2))-(5.8613D-8*(LA**4))+
     1     (0.007676D0/(LA**2))+(0.0156569D0/((LA**2)-0.0324D0)))
C
C     CsBr INTERPOLATION FORMULA
          CSBR(LA)=DSQRT(
     1     5.640752D0-(3.338D-6*(LA**2))+
     1     (0.0018612D0/(LA**2))+(41110.49D0/((LA**2)-14390.4D0))
     1     +(0.0290764D0/((LA**2)-0.024964D0)))
C
C     MGO INTERPOLATION FORMULA
          MGO(LA)=DSQRT(
     1     2.956362D0-(0.01062387D0*(LA**2))-(0.0000204968D0*(LA**4))-
     2     (0.02195770/((LA**2)-0.01428322D0)))
C
C     DYNASIL INTERPOLATION FORMULA
          DYN(LA)=DSQRT(
     1     ((0.6961663D0*(LA**2))/((LA**2)-((0.0684043D0)**2)))+
     1     ((0.4079426D0*(LA**2))/((LA**2)-((0.1162414D0)**2)))+
     1     ((0.8974794D0*(LA**2))/((LA**2)-((9.896161D0)**2)))+1.0D0)
C
C     SAPPHIRE INTERPOLATION FORMULA
          SAPH(LA)=DSQRT(
     1     ((1.023798D0*(LA**2))/((LA**2)-(0.00377588D0)))+
     1     ((1.058264D0*(LA**2))/((LA**2)-(0.01225440D0)))+
     1     ((5.280792D0*(LA**2))/((LA**2)-(321.361600D0)))+1.0D0)
C
C     YAG INTERPOLATION FORMULA
          YAG(LA)=DSQRT(
     1     ((2.293D0*(LA**2))/((LA**2)-(0.1095D0**2)))+
     1     ((3.705D0*(LA**2))/((LA**2)-(17.825D0**2)))+1.0D0)
C
C     CSI INTERPOLATION FORMULA
          CSI(LA)=DSQRT(
     1     ((0.34617251D0*(LA**2))/((LA**2)-0.00052701D0))+
     1     ((1.0080886D0*(LA**2))/((LA**2)-0.02149156D0))+
     1     ((0.28551800D0*(LA**2))/((LA**2)-0.032761D0))+
     1     ((0.39743178D0*(LA**2))/((LA**2)-0.044944D0))+
     1     ((3.3605359D0*(LA**2))/((LA**2)-25621.0D0))+1.0D0)
C
C     KRS5 INTERPOLATION FORMULA
          KRS5(LA)=DSQRT(
     1     ((1.8293958D0*(LA**2))/((LA**2)-(0.0225D0**2)))+
     1     ((1.6675593D0*(LA**2))/((LA**2)-(0.0625D0**2)))+
     1     ((1.1210424D0*(LA**2))/((LA**2)-(0.1225D0**2)))+
     1     ((0.04513366D0*(LA**2))/((LA**2)-(0.2025D0**2)))+
     1     ((12.380234D0*(LA**2))/((LA**2)-(27089.737D0**2)))+1.0D0)
C
C     As2S3 INTERPOLATION FORMULA
          AS2S3(LA)=DSQRT(
     1     ((1.8983678D0*(LA**2))/((LA**2)-(0.15D0**2)))+
     1     ((1.9222979D0*(LA**2))/((LA**2)-(0.25D0**2)))+
     1     ((0.8765134D0*(LA**2))/((LA**2)-(0.350D0**2)))+
     1     ((0.1188704D0*(LA**2))/((LA**2)-(0.450D0**2)))+
     1     ((0.9569903D0*(LA**2))/((LA**2)-(27.3861D0**2)))+1.0D0)
C
C     BAF2 INTERPOLATION FORMULA
          BAF2(LA)=DSQRT(
     1     ((0.643356D0*(LA**2))/((LA**2)-((0.057789D0)**2)))+
     1     ((0.506762D0*(LA**2))/((LA**2)-((0.10968D0)**2)))+
     1     ((3.8261D0*(LA**2))/((LA**2)-((46.3864D0)**2)))+1.0D0)
C
C     CAF2 INTERPOLATION FORMULA
          CAF2(LA)=DSQRT(
     1     ((0.5675888D0*(LA**2))/((LA**2)-((0.050263605D0)**2)))+
     1     ((0.4710914D0*(LA**2))/((LA**2)-((0.1003909D0)**2)))+
     1     ((3.8484723D0*(LA**2))/((LA**2)-((34.649040D0)**2)))+1.0D0)
C
C     MGF2o INTERPOLATION FORMULA
          MGF2O(LA)=DSQRT(
     1    1.0D0+
     2    ((.48755108D0*(LA**2))/((LA**2)-(0.04338408D0**2)))
     3    +((.39875031D0*(LA**2))/((LA**2)-(0.09461442D0**2)))
     4    +((2.3120353D0*(LA**2))/((LA**2)-(23.7936040D0**2)))
     5    )
C
C     MGF2E INTERPOLATION FORMULA
          MGF2E(LA)=DSQRT(
     1    1.0D0+
     2    ((.41344023D0*(LA**2))/((LA**2)-(0.03684262D0**2)))
     3    +((.50497499D0*(LA**2))/((LA**2)-(0.09076162D0**2)))
     4    +((2.4904862D0*(LA**2))/((LA**2)-(12.771995D0**2)))
     5    )
C
C     THIS SUBROUTINE IS USED TO CALCULATE REFRACTIVE
C     INDICES OF THE MATERIALS WHOSE NAMES ARE THEIR COMMANDS
C
          IF(MTYPE.EQ.1) THEN
C     INTERPOLATE THE SINGLE CRYSTALLINE GERMANIUN DATA
              N=20
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR SINGLE CRYSTAL GERMANIUM
C
              DATA (GSCX(J), J = 1,20)/2.0581D0,2.1526D0,2.3126D0,2.4374D0,
     1         2.577D0,
     1         2.7144D0,2.998D0,3.3033D0,3.4188D0,4.258D0,4.866D0,6.238D0,
     2         8.66D0,9.72D0,11.04D0,12.20D0,13.02D0,14.21D0,15.08D0,16.0D0/
C
              DATA (GSCY(J), J = 1,20)/4.1016D0,4.0919D0,4.0786D0,4.0708D0,
     1        4.0609D0,4.0552D0,4.0452D0,4.0369D0,4.0334D0,
     2        4.0216D0,4.0170D0,4.0094D0,4.0043D0,4.0034D0,
     3        4.0026D0,4.0023D0,4.0021D0,4.0015D0,4.0014D0,4.0012D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=GSCX(1:N)
              Y(1:N)=GSCY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.2) THEN
C     INTERPOLATE THE POLY CRYSTALLINE GERMANIUN DATA
              N=17
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR POLY CRYSTAL GERMANIUM
C
              DATA (GPCX(J), J = 1,17)/2.0581D0,2.1526D0,2.3126D0,2.4374D0,
     1         2.577D0,
     1         2.7144D0,2.998D0,3.3033D0,3.4188D0,4.258D0,4.866D0,6.238D0,
     2         8.66D0,9.72D0,11.04D0,12.20D0,13.02D0/
C
              DATA (GPCY(J), J = 1,17)/4.1018D0,4.0919D0,4.0785D0,4.0709D0,
     1        4.0608D0,4.0554D0,4.0452D0,4.0372D0,4.0339D0,
     2        4.0217D0,4.0167D0,4.0095D0,4.0043D0,4.0033D0,
     3        4.0025D0,4.0020D0,4.0018D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=GPCX(1:N)
              Y(1:N)=GPCY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.3) THEN
C     INTERPOLATE THE SILICON DATA
C     CALCULATE THE INDICES
              GPREG(1)=SIL(GLSWV(1))
              GPREG(2)=SIL(GLSWV(2))
              GPREG(3)=SIL(GLSWV(3))
              GPREG(4)=SIL(GLSWV(4))
              GPREG(5)=SIL(GLSWV(5))
          END IF
          IF(MTYPE.EQ.4) THEN
C     INTERPOLATE THE IRG100 DATA
              N=25
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR IRG100
C
              DATA (IRGX(J), J = 1,25)/1.0D0,1.5D0,2.0D0,2.5D0,3.0D0,3.5D0
     1        ,4.0D0,4.5D0,5.0D0,5.5D0,6.0D0,6.5D0,7.0D0,7.5D0,8.0D0,8.5D0
     2        ,9.0D0,9.5D0,10.0D0,10.5D0,11.0D0,11.5D0,12.0D0,13.0D0,14.0D0/
C
              DATA (IRGY(J), J = 1,25)/2.7235D0,2.6577D0,2.6404D0,2.6314D0
     1        ,2.6262D0,2.6227D0,2.6201D0,2.6181D0,2.6164D0,2.6148D0,2.6133D0
     2        ,2.6118D0,2.6103D0,2.6088D0,2.6072D0,2.6056D0,2.6039D0,2.6022D0
     3        ,2.6004D0,2.5985D0,2.5966D0,2.5946D0,2.5925D0,2.5880D0,2.5832D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=IRGX(1:N)
              Y(1:N)=IRGY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.5) THEN
C     INTERPOLATE THE ZNSE DATA
              N=56
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR ZNSE
C
              DATA (ZNSEX(J), J = 1,56)/0.54D0,0.58D0
     1        ,0.62D0,0.66D0,0.70D0,0.74D0,0.78D0,0.82D0,0.86D0,0.90D0
     2        ,0.94D0,0.98D0,1.0D0,1.4D0,1.8D0,2.2D0,2.6D0,3.0D0,3.4D0
     3        ,3.8D0,4.2D0,4.6D0,5.0D0,5.4D0,5.8D0,6.2D0,6.6D0,7.0D0,7.4D0
     4        ,7.8D0,8.2D0,8.6D0,9.0D0,9.4D0,9.8D0,10.2D0,10.6D0,11.0D0
     5        ,11.4D0,11.8D0,12.2D0,12.6D0,13.0D0,13.4D0,13.8D0,14.2D0,14.6D0
     6        ,15.0D0,15.4D0,15.8D0,16.2D0,16.6D0,17.0D0,17.4D0,17.8D0,18.2D0/
C
              DATA (ZNSEY(J), J = 1,56)/2.6754D0,2.6312D0,2.5994D0,2.5755D0
     1        ,2.5568D0,2.5418D0,2.5295D0,2.5193D0,2.5107D0,2.5034D0,2.4971D0
     2        ,2.4916D0,2.4892D0,2.4609D0,2.4496D0,2.4437D0,2.4401D0,2.4376D0
     3        ,2.4356D0,2.4339D0,2.4324D0,2.4309D0,2.4295D0,2.4281D0,2.4266D0
     4        ,2.4251D0,2.4235D0,2.4218D0,2.4201D0,2.4183D0,2.4163D0,2.4143D0
     5        ,2.4122D0,2.4100D0,2.4077D0,2.4053D0,2.4028D0,2.4001D0,2.3974D0
     6        ,2.3945D0,2.3915D0,2.3883D0,2.3850D0,2.3816D0,2.3781D0,2.3744D0
     7        ,2.3705D0,2.3665D0,2.3623D0,2.3579D0,2.3534D0,2.3487D0,2.3438D0
     8        ,2.3387D0,2.3333D0,2.3278D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=ZNSEX(1:N)
              Y(1:N)=ZNSEY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.6) THEN
C     INTERPOLATE THE ZNS DATA
              N=59
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR ZNS
C
              DATA (ZNSX(J), J = 1,59)/0.42D0,0.46D0,0.5D0,0.54D0,0.58D0
     1        ,0.62D0,0.66D0,0.70D0,0.74D0,0.78D0,0.82D0,0.86D0,0.90D0
     2        ,0.94D0,0.98D0,1.0D0,1.4D0,1.8D0,2.2D0,2.6D0,3.0D0,3.4D0
     3        ,3.8D0,4.2D0,4.6D0,5.0D0,5.4D0,5.8D0,6.2D0,6.6D0,7.0D0,7.4D0
     4        ,7.8D0,8.2D0,8.6D0,9.0D0,9.4D0,9.8D0,10.2D0,10.6D0,11.0D0
     5        ,11.4D0,11.8D0,12.2D0,12.6D0,13.0D0,13.4D0,13.8D0,14.2D0,14.6D0
     6        ,15.0D0,15.4D0,15.8D0,16.2D0,16.6D0,17.0D0,17.4D0,17.8D0,18.2D0/
C
              DATA (ZNSY(J), J = 1,59)/2.516D0,2.458D0,2.419D0,2.391D0,2.371D0
     1        ,2.355D0,2.342D0,2.332D0,2.323D0,2.316D0,2.310D0,2.305D0,2.301D0
     2        ,2.297D0,2.294D0,2.292D0,2.275D0,2.267D0,2.263D0,2.260D0,2.257D0
     3        ,2.255D0,2.253D0,2.251D0,2.248D0,2.246D0,2.244D0,2.241D0,2.238D0
     4        ,2.235D0,2.232D0,2.228D0,2.225D0,2.221D0,2.217D0,2.212D0,2.208D0
     5        ,2.203D0,2.198D0,2.192D0,2.186D0,2.180D0,2.173D0,2.167D0,2.159D0
     6        ,2.152D0,2.143D0,2.135D0,2.126D0,2.116D0,2.106D0,2.095D0,2.084D0
     7        ,2.072D0,2.059D0,2.045D0,2.030D0,2.015D0,1.998D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=ZNSX(1:N)
              Y(1:N)=ZNSY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.7) THEN
C     INTERPOLATE THE CLRTRAN DATA
              N=29
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR CLRTRAN
C
              DATA (CLRX(J), J = 1,29)/0.4047D0,0.4358D0,0.4678D0,0.48D0
     1        ,0.5086D0,0.5461D0,0.5876D0,0.6438D0,0.6678D0,0.7065D0,0.78D0
     2        ,0.7948D0,0.8521D0,0.8943D0,1.014D0,1.1287D0,1.5296D0,2.0581D0
     3        ,3.0D0,3.5D0,4.0D0,4.5D0,5.0D0,8.0D0,9.0D0,10.0D0,11.25D0,12.0D0
     4        ,13.0D0/
C
              DATA (CLRY(J), J = 1,29)/2.54515D0,2.48918D0,2.44915D0,2.43691D0
     1        ,2.41279D0,2.38838D0,2.36789D0,2.34731D0,2.34033D0,2.33073D0
     2        ,2.31669D0,2.31438D0,2.30659D0,2.30183D0,2.29165D0,2.28485D0
     3        ,2.27191D0,2.26442D0,2.25772D0,2.25498D0,2.25231D0,2.24955D0
     4        ,2.24661D0,2.22334D0,2.21290D0,2.20084D0,2.18317D0,2.17101D0
     5        ,2.15252D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=CLRX(1:N)
              Y(1:N)=CLRY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.8) THEN
C     INTERPOLATE THE WILLOW RUN FUSED SILICA/SIO2
C
C     CALCULATE THE INDICES
              GPREG(1)=FSIL(GLSWV(1))
              GPREG(2)=FSIL(GLSWV(2))
              GPREG(3)=FSIL(GLSWV(3))
              GPREG(4)=FSIL(GLSWV(4))
              GPREG(5)=FSIL(GLSWV(5))
          END IF
          IF(MTYPE.EQ.9) THEN
C     INTERPOLATE SAPPHIRE
C
C     CALCULATE THE INDICES
              GPREG(1)=SAPH(GLSWV(1))
              GPREG(2)=SAPH(GLSWV(2))
              GPREG(3)=SAPH(GLSWV(3))
              GPREG(4)=SAPH(GLSWV(4))
              GPREG(5)=SAPH(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.10) THEN
C     INTERPOLATE DYNASIL
C
C     CALCULATE THE INDICES
              GPREG(1)=DYN(GLSWV(1))
              GPREG(2)=DYN(GLSWV(2))
              GPREG(3)=DYN(GLSWV(3))
              GPREG(4)=DYN(GLSWV(4))
              GPREG(5)=DYN(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.11) THEN
C     INTERPOLATE THE AMTIR1
              N=17
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR AMTIR1
C
              DATA (AMTR1X(J), J = 1,17)/1.0D0,1.064D0,1.5D0,2.0D0,2.4D0
     1        ,3.0D0,4.0D0,5.0D0,6.0D0,7.0D0,8.0D0,9.0D0,10.0D0,11.0D0
     2        ,12.0D0,13.0D0,14.0D0/
C
              DATA (AMTR1Y(J), J = 1,17)/2.6055D0,2.5933D0,2.5469D0,2.5310D0
     1        ,2.5250D0,2.5184D0,2.5146D0,2.5112D0,2.5086D0,2.5062D0,2.5036D0
     2        ,2.5008D0,2.4977D0,2.4942D0,2.4902D0,2.4862D0,2.4825D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=AMTR1X(1:N)
              Y(1:N)=AMTR1Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.12) THEN
C     INTERPOLATE THE AMTIR3
              N=12
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR AMTIR3
C
              DATA (AMTR3X(J), J = 1,12)/3.0D0,4.0D0,5.0D0,6.0D0,7.0D0,8.0D0
     1        ,9.0D0,10.0D0,11.0D0,12.0D0,13.0D0,14.0D0/
C
              DATA (AMTR3Y(J), J = 1,12)/2.6266D0,2.6210D0,2.6173D0,2.6142D0
     1        ,2.6117D0,2.6088D0,2.6055D0,2.6023D0,2.5983D0,2.5942D0,2.5892D0
     2        ,2.5843D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=AMTR3X(1:N)
              Y(1:N)=AMTR3Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.13) THEN
C     INTERPOLATE THE As2S3
C
C     CALCULATE THE INDICES
              GPREG(1)=AS2S3(GLSWV(1))
              GPREG(2)=AS2S3(GLSWV(2))
              GPREG(3)=AS2S3(GLSWV(3))
              GPREG(4)=AS2S3(GLSWV(4))
              GPREG(5)=AS2S3(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.14) THEN
C     INTERPOLATE THE GaAs
              N=13
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR GaAs
C
              DATA (GAASX(J), J = 1,13)/2.5D0,3.0D0,4.0D0,5.0D0,6.0D0
     1        ,7.0D0,8.0D0,9.0D0,10.0D0,11.0D0,12.0D0,13.0D0,14.0D0/
C
              DATA (GAASY(J), J = 1,13)/3.3256D0,3.3169D0,3.3069D0
     1        ,3.3010,3.2963D0,3.2923D0,3.2878D0,3.2830D0,3.2778D0
     2        ,3.2725D0,3.2666D0,3.2589D0,3.2509D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=GAASX(1:N)
              Y(1:N)=GAASY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.15) THEN
C     INTERPOLATE THE CdTe
              N=10
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR CdTe
C
              DATA (CDTEX(J), J = 1,10)/3.0D0,4.0D0,5.0D0,6.0D0,7.0D0
     1        ,8.0D0,9.0D0,10.0D0,11.0D0,12.0D0/
C
              DATA (CDTEY(J), J = 1,10)/2.7026D0,2.6971D0,2.6922D0
     1        ,2.6886D0,2.6865D0,2.6846D0,2.6825D0,2.6797D0,2.6766D0
     2        ,2.6749D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=CDTEX(1:N)
              Y(1:N)=CDTEY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.16) THEN
C     INTERPOLATE MGF2O
C
C     CALCULATE THE INDICES
              GPREG(1)=MGF2O(GLSWV(1))
              GPREG(2)=MGF2O(GLSWV(2))
              GPREG(3)=MGF2O(GLSWV(3))
              GPREG(4)=MGF2O(GLSWV(4))
              GPREG(5)=MGF2O(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.17) THEN
C     INTERPOLATE MGF2E
C
C     CALCULATE THE INDICES
              GPREG(1)=MGF2E(GLSWV(1))
              GPREG(2)=MGF2E(GLSWV(2))
              GPREG(3)=MGF2E(GLSWV(3))
              GPREG(4)=MGF2E(GLSWV(4))
              GPREG(5)=MGF2E(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.18) THEN
C     INTERPOLATE CAF2
C
C     CALCULATE THE INDICES
              GPREG(1)=CAF2(GLSWV(1))
              GPREG(2)=CAF2(GLSWV(2))
              GPREG(3)=CAF2(GLSWV(3))
              GPREG(4)=CAF2(GLSWV(4))
              GPREG(5)=CAF2(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.19) THEN
C     INTERPOLATE MGO
C
C     CALCULATE THE INDICES
              GPREG(1)=MGO(GLSWV(1))
              GPREG(2)=MGO(GLSWV(2))
              GPREG(3)=MGO(GLSWV(3))
              GPREG(4)=MGO(GLSWV(4))
              GPREG(5)=MGO(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.20) THEN
C     INTERPOLATE BAF2
C
C     CALCULATE THE INDICES
              GPREG(1)=BAF2(GLSWV(1))
              GPREG(2)=BAF2(GLSWV(2))
              GPREG(3)=BAF2(GLSWV(3))
              GPREG(4)=BAF2(GLSWV(4))
              GPREG(5)=BAF2(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.21) THEN
C     INTERPOLATE KBR
C
C     CALCULATE THE INDICES
              GPREG(1)=KBR(GLSWV(1))
              GPREG(2)=KBR(GLSWV(2))
              GPREG(3)=KBR(GLSWV(3))
              GPREG(4)=KBR(GLSWV(4))
              GPREG(5)=KBR(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.22) THEN
C     INTERPOLATE CSI
C
C     CALCULATE THE INDICES
              GPREG(1)=CSI(GLSWV(1))
              GPREG(2)=CSI(GLSWV(2))
              GPREG(3)=CSI(GLSWV(3))
              GPREG(4)=CSI(GLSWV(4))
              GPREG(5)=CSI(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.23) THEN
C     INTERPOLATE CSBR
C
C     CALCULATE THE INDICES
              GPREG(1)=CSBR(GLSWV(1))
              GPREG(2)=CSBR(GLSWV(2))
              GPREG(3)=CSBR(GLSWV(3))
              GPREG(4)=CSBR(GLSWV(4))
              GPREG(5)=CSBR(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.24) THEN
C     INTERPOLATE KRS5
C
C     CALCULATE THE INDICES
              GPREG(1)=KRS5(GLSWV(1))
              GPREG(2)=KRS5(GLSWV(2))
              GPREG(3)=KRS5(GLSWV(3))
              GPREG(4)=KRS5(GLSWV(4))
              GPREG(5)=KRS5(GLSWV(5))
C
          END IF
          IF(MTYPE.EQ.25) THEN
C     INTERPOLATE THE LiF
C
C     CALCULATE THE INDICES
              GPREG(1)=LIF(GLSWV(1))
              GPREG(2)=LIF(GLSWV(2))
              GPREG(3)=LIF(GLSWV(3))
              GPREG(4)=LIF(GLSWV(4))
              GPREG(5)=LIF(GLSWV(5))
          END IF
          IF(MTYPE.EQ.26) THEN
C     INTERPOLATE THE NaCl
              N=60
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR NaCl
C
              DATA (NACLX(J), J = 1,60)/0.589D0,0.64D0,0.6874D0,0.7604D0
     1        ,0.7858D0,0.8835D0,0.9033D0,0.9724D0,1.0084D0,1.0540D0
     2        ,1.0810D0,1.1058D0,1.1420D0,1.1786D0,1.2016D0,1.2604D0
     3        ,1.3126D0,1.4874D0,1.5552D0,1.6368D0,1.6848D0,1.7670D0
     4        ,2.0736D0,2.1824D0,2.2464D0,2.3560D0,2.6505D0,2.9466D0
     5        ,3.2736D0,3.5359D0,3.6288D0,3.8192D0,4.1230D0,4.7120D0
     6        ,5.0092D0,5.3009D0,5.8932D0,6.4825D0,6.8000D0,7.0718D0
     7        ,7.2200D0,7.5900D0,7.6611D0,7.9558D0,8.0400D0,8.8398D0
     8        ,9.0000D0,9.5000D0,10.0184D0,11.7864D0,12.5D0,12.9650D0
     9        ,13.50D0,14.1436D0,14.7330D0,15.3223D0,15.9116D0,17.93D0
     1        ,20.57D0,22.3D0/
C
              DATA (NACLY(J), J = 1,60)/1.54427D0,1.54141D0,1.53930D0
     1        ,1.53682D0,1.53607D0,1.53395D0,1.53361D0,1.53253D0,1.53206D0
     2        ,1.53153D0,1.53123D0,1.53098D0,1.53063D0,1.53031D0,1.53014D0
     3        ,1.52971D0,1.52937D0,1.52845D0,1.52815D0,1.52781D0,1.52764D0
     4        ,1.52736D0,1.52649D0,1.52621D0,1.52606D0,1.52579D0,1.52512D0
     5        ,1.52466D0,1.52371D0,1.52312D0,1.52286D0,1.52238D0,1.52156D0
     6        ,1.51979D0,1.51883D0,1.51790D0,1.51593D0,1.51347D0,1.51200D0
     7        ,1.51093D0,1.51020D0,1.50850D0,1.50822D0,1.50665D0,1.50640D0
     8        ,1.50192D0,1.50100D0,1.49980D0,1.49462D0,1.48171D0,1.47568D0
     9        ,1.47160D0,1.46660D0,1.46044D0,1.45427D0,1.44743D0,1.44090D0
     &        ,1.41490D0,1.37350D0,1.34030D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=NACLX(1:N)
              Y(1:N)=NACLY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.27) THEN
C     INTERPOLATE THE SiO2o
              N=29
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR SiO2o
C
              DATA (SIO2OX(J), J = 1,29)/0.185D0,0.198D0,0.231D0,0.34D0
     1        ,0.394D0,0.434D0,0.508D0,0.5893D0,0.768D0,0.8325D0,0.9914D0
     2        ,1.1592D0,1.3070D0,1.3958D0,1.4792D0,1.5414D0,1.6815D0
     3        ,1.7614D0,1.9457D0,2.0531D0,2.3D0,2.6D0,3.0D0,3.5D0,4.0D0
     4        ,4.2D0,5.0D0,6.45D0,7.0D0/
C
              DATA (SIO2OY(J), J = 1,29)/1.65751D0,1.65087D0,1.61395D0
     1        ,1.56747D0,1.55846D0,1.55396D0,1.54822D0,1.54424D0,1.53903D0
     2        ,1.53773D0,1.53514D0,1.53283D0,1.53090D0,1.52877D0,1.52865D0
     3        ,1.52781D0,1.52583D0,1.52468D0,1.52184D0,1.52005D0,1.51561D0
     4        ,1.50986D0,1.49953D0,1.48451D0,1.46617D0,1.4569D0,1.417D0,1.274D0
     5        ,1.167D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=SIO2OX(1:N)
              Y(1:N)=SIO2OY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.28) THEN
C     INTERPOLATE THE SiO2e
              N=20
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR SiO2e
C
              DATA (SIO2EX(J), J = 1,20)/0.185D0,0.198D0,0.231D0,0.34D0
     1        ,0.394D0,0.434D0,0.508D0,0.5893D0,0.768D0,0.8325D0,0.9914D0
     2        ,1.1592D0,1.3070D0,1.3958D0,1.4792D0,1.5414D0,1.6815D0
     3        ,1.7614D0,1.9457D0,2.0531D0/
C
              DATA (SIO2EY(J), J = 1,20)/1.68988D0,1.66394D0,1.62555D0
     1        ,1.57737D0,1.56805D0,1.56339D0,1.55746D0,1.55335D0,1.54794D0
     2        ,1.54661D0,1.54392D0,1.54152D0,1.53951D0,1.53832D0,1.53716D0
     3        ,1.53630D0,1.53422D0,1.53301D0,1.53004D0,1.52823D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=SIO2EX(1:N)
              Y(1:N)=SIO2EY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.29) THEN
C     INTERPOLATE THE VIR3
              N=18
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR VIR3
C
              DATA (VIR3X(J), J = 1,18)/0.4047D0,0.5461D0,0.7065D0,1.0D0
     1        ,1.0D0,2.0D0,2.5D0,3.0D0,3.25D0,3.5D0,3.75D0,4.0D0,4.25D0
     2        ,4.5D0,4.75D0,5.0D0,5.5D0,6.0D0/
C
              DATA (VIR3Y(J), J = 1,18)/1.92568D0,1.87002D0,1.84694D0
     1        ,1.831D0,1.818D0,1.812D0,1.806D0,1.799D0,1.795D0,1.791D0
     2        ,1.786D0,1.781D0,1.775D0,1.769D0,1.762D0,1.756D0,1.741D0,1.725D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=VIR3X(1:N)
              Y(1:N)=VIR3Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.30) THEN
C     INTERPOLATE THE 9754
              N=43
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR 9754
C
              DATA (G9754X(J), J = 1,43)/0.4D0,0.425D0,0.45D0
     1        ,0.475D0,0.5D0,0.525D0
     1        ,0.55D0,0.575D0,0.6D0,0.635D0,0.65D0,0.675D0,0.7D0,0.725D0,0.75D0
     2        ,0.775D0,0.8D0,0.825D0,0.85D0,0.875D0,0.9D0,0.925D0,0.95D0
     3        ,0.975D0,1.0D0,1.25D0,1.5D0,1.75D0,2.0D0,2.25D0,2.5D0,2.75D0
     4        ,3.0D0,3.25D0,3.5D0,3.75D0,4.0D0,4.25D0,4.5D0,4.75D0,5.0D0
     5        ,5.25D0,5.5D0/
C
              DATA (G9754Y(J), J = 1,43)/1.69093D0,1.68502D0,1.68020D0
     1        ,1.67621D0,1.67285D0,1.67000D0,1.66754D0,1.66542D0,1.66356D0
     2        ,1.66192D0,1.66046D0,1.65916D0,1.65800D0,1.65694D0,1.65599D0
     3        ,1.65511D0,1.65431D0,1.65358D0,1.65289D0,1.65226D0,1.65167D0
     4        ,1.65112D0,1.65060D0,1.65011D0,1.64964D0,1.64595D0,1.64310D0
     5        ,1.64049D0,1.63785D0,1.63505D0,1.63203D0,1.62874D0,1.62514D0
     6        ,1.62119D0,1.61686D0,1.61214D0,1.60698D0,1.60135D0,1.59521D0
     7        ,1.58853D0,1.58125D0,1.57332D0,1.66469D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=G9754X(1:N)
              Y(1:N)=G9754Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.31) THEN
C     INTERPOLATE THE ALON
              N=14
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR ALON
C
              DATA (ALONX(J), J = 1,14)/0.365D0,0.405D0,0.435D0,0.546D0
     1        ,0.852D0,1.014D0,1.53D0,1.97D0,2.325D0,2.8D0,3.39D0,4.0D0
     2        ,4.6D0,5.0D0/
C
              DATA (ALONY(J), J = 1,14)/1.819D0,1.811D0,1.806D0,1.792D0
     1        ,1.778D0,1.773D0,1.765D0,1.758D0,1.752D0,1.743D0,1.729D0
     2        ,1.710D0,1.689D0,1.672D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=ALONX(1:N)
              Y(1:N)=ALONY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.32) THEN
C     INTERPOLATE THE SPINEL
              N=11
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR SPINEL
C
              DATA (SPINX(J), J = 1,11)/0.4047D0,0.4358D0,0.5461D0,0.8521D0
     1        ,1.014D0,1.53D0,1.97D0,3.0D0,4.0D0,5.0D0,5.5D0/
C
              DATA (SPINY(J), J = 1,11)/1.73574D0,1.73054D0,1.71896D0
     2        ,1.70728D0,1.703D0,1.69468D0,1.68763D0,1.6647D0,1.6414D0
     3        ,1.5978D0,1.5719D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=SPINX(1:N)
              Y(1:N)=SPINY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          IF(MTYPE.EQ.33) THEN
C     INTERPOLATE THE CALCIUM ALUMINATE GLASS
              N=12
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     DATA FOR CALAL
C
              DATA (CALALX(J), J = 1,12)/0.4861D0,0.5893D0,0.6563D0,0.8D0
     1        ,1.0D0,1.5D0,2.0D0,2.5D0,3.0D0,3.5D0,4.0D0,4.5D0/
C
              DATA (CALALY(J), J = 1,12)/1.6794D0,1.669D0,1.6647D0,1.6588D0
     1        ,1.6538D0,1.6463D0,1.6403D0,1.6341D0,1.6266D0,1.6180D0
     2        ,1.6074D0,1.5952D0/
C
C     MOVE TO LARGE ARRAYS
              X(1:N)=CALALX(1:N)
              Y(1:N)=CALALY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              IF(MTYPE.EQ.34) THEN
C     INTERPOLATE THE B270 GLASS
                  N=8
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
C
C     DATA FOR B270
C
                  DATA (B270X(J), J = 1,8)/0.4358343D0,0.4799914D0,0.4861327D0
     1            ,0.5460740D0,0.5875618D0,0.5892938D0,0.6438469D0,0.6562725D0/
C
                  DATA (B270Y(J), J = 1,8)/1.534D0,1.5297D0,1.5292D0,1.5251D0
     2            ,11.523D0,1.5229D0,1.5207D0,1.5202D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=B270X(1:N)
                  Y(1:N)=B270Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              END IF
              IF(MTYPE.EQ.35) THEN
C     INTERPOLATE THE IRG2 GLASS
                  N=18
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
C
C     DATA FOR IRG2
C
                  DATA (IRG2X(J), J = 1,18)/0.365D0,0.4047D0,0.48D0,0.4861D0
     1            ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2            ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
C
                  DATA (IRG2Y(J), J = 1,18)/1.9750D0,1.9462D0,1.9147D0,1.9129D0
     1            ,1.8988D0,1.8918D0,1.8845D0,1.8832D0,1.8785D0,1.8692D0,1.8630D0
     2            ,1.8526D0,1.8464D0,1.8414D0,1.8362D0,1.8253D0,1.8041D0,1.7954D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=IRG2X(1:N)
                  Y(1:N)=IRG2Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              END IF
              IF(MTYPE.EQ.36) THEN
C     INTERPOLATE THE IRG3 GLASS
                  N=17
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
C
C     DATA FOR IRG3
C
                  DATA (IRG3X(J), J = 1,17)/0.4047D0,0.48D0,0.4861D0
     1            ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2            ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
C
                  DATA (IRG3Y(J), J = 1,17)/1.8925D0,1.8649D0,1.8633D0,1.8510D0
     1            ,1.8449D0,1.8385D0,1.8373D0,1.8331D0,1.8249D0,1.8193D0,1.8089D0
     2            ,1.8021D0,1.7963D0,1.7900D0,1.7764D0,1.7491D0,1.7375D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=IRG3X(1:N)
                  Y(1:N)=IRG3Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              END IF
              IF(MTYPE.EQ.37) THEN
C     INTERPOLATE THE IRGN6 GLASS
                  N=16
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
C
C     DATA FOR IRGN6
C
                  DATA (IRGN6X(J), J = 1,16)/0.4047D0,0.48D0,0.4861D0
     1            ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2            ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0/
C
                  DATA (IRGN6Y(J), J = 1,16)/1.6069D0,1.5971D0,1.5965D0,1.5915D0
     1            ,1.5892D0,1.5863D0,1.5857D0,1.5842D0,1.5807D0,1.5777D0,1.5716D0
     2            ,1.5667D0,1.5620D0,1.5567D0,1.5451D0,1.5209D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=IRGN6X(1:N)
                  Y(1:N)=IRGN6Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              END IF
              IF(MTYPE.EQ.38) THEN
C     INTERPOLATE THE IRG7 GLASS
                  N=16
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
C
C     DATA FOR IRG7
C
                  DATA (IRG7X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0
     1            ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2            ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
C
                  DATA (IRG7Y(J), J = 1,16)/1.5983D0,1.5871D0,1.5743D0,1.5735D0
     1            ,1.5675D0,1.5644D0,1.5612D0,1.5606D0,1.5585D0,1.5541D0,1.5509D0
     2            ,1.5442D0,1.5389D0,1.5341D0,1.5286D0,1.5164D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=IRG7X(1:N)
                  Y(1:N)=IRG7Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              END IF
              IF(MTYPE.EQ.39) THEN
C     INTERPOLATE THE IRG9 GLASS
                  N=16
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
C
C     DATA FOR IRG9
C
                  DATA (IRG9X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0
     1            ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2            ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
C
                  DATA (IRG9Y(J), J = 1,16)/1.5005D0,1.4961D0,1.4905D0,1.4902D0
     1            ,1.4875D0,1.4861D0,1.4845D0,1.4842D0,1.4832D0,1.4810D0,1.4793D0
     2            ,1.4755D0,1.4722D0,1.4692D0,1.4658D0,1.4583D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=IRG9X(1:N)
                  Y(1:N)=IRG9Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              END IF
              IF(MTYPE.EQ.40) THEN
C     INTERPOLATE THE IRG11 GLASS
                  N=16
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
C
C     DATA FOR IRG11
C
                  DATA (IRG11X(J), J = 1,16)/0.48D0,0.4861D0
     1            ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2            ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0,4.258D0,4.586D0/
C
                  DATA (IRG11Y(J), J = 1,16)/1.6926D0,1.6917D0,1.6845D0,1.6809D0
     1            ,1.6770D0,1.6763D0,1.6741D0,1.6686D0,1.6650D0,1.6581D0,1.6532D0
     2            ,1.6491D0,1.6445D0,1.6349D0,1.6158D0,1.6077D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=IRG11X(1:N)
                  Y(1:N)=IRG11Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              END IF
              IF(MTYPE.EQ.41) THEN
C     INTERPOLATE THE IRG15 GLASS
                  N=16
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
C
C     DATA FOR IRG15
C
                  DATA (IRG15X(J), J = 1,16)/0.365D0,0.4047D0,0.48D0,0.4861D0
     1            ,0.5461D0,0.5876D0,0.6438D0,0.6563D0,0.7065D0,0.8521D0,1.014D0
     2            ,1.5296D0,1.9701D0,2.3254D0,2.674D0,3.303D0/
C
                  DATA (IRG15Y(J), J = 1,16)/1.5883D0,1.5506D0,1.5415D0,1.5410D0
     1            ,1.5366D0,1.5343D0,1.5318D0,1.5314D0,1.5297D0,1.5263D0,1.5237D0
     2            ,1.5179D0,1.5131D0,1.5086D0,1.5038D0,1.4924D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=IRG15X(1:N)
                  Y(1:N)=IRG15Y(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              END IF
              IF(MTYPE.EQ.42) THEN
C     INTERPOLATE THE VAC GLASS
                  A=28.79D-5
                  B=5.67D-5
                  IF(GLSWV(1).NE.0.0D0) THEN
                      LA1=LM1(A,B,GLSWV(1))
                      LA1=1.0D0+LA1
                      GPREG(1)=1.0D0/LA1
                  ELSE
                      GPREG(1)=1.0D0
                  END IF
                  IF(GLSWV(2).NE.0.0D0) THEN
                      LA2=LM1(A,B,GLSWV(2))
                      LA2=1.0D0+LA2
                      GPREG(2)=1.0D0/LA2
                  ELSE
                      GPREG(2)=1.0D0
                  END IF
                  IF(GLSWV(3).NE.0.0D0) THEN
                      LA3=LM1(A,B,GLSWV(3))
                      LA3=1.0D0+LA3
                      GPREG(3)=1.0D0/LA3
                  ELSE
                      GPREG(3)=1.0D0
                  END IF
                  IF(GLSWV(4).NE.0.0D0) THEN
                      LA4=LM1(A,B,GLSWV(4))
                      LA4=1.0D0+LA4
                      GPREG(4)=1.0D0/LA4
                  ELSE
                      GPREG(4)=1.0D0
                  END IF
                  IF(GLSWV(5).NE.0.0D0) THEN
                      LA5=LM1(A,B,GLSWV(5))
                      LA5=1.0D0+LA5
                      GPREG(5)=1.0D0/LA5
                  ELSE
                      GPREG(5)=1.0D0
                  END IF
C
              END IF
C     DATA FOR H2O
              IF(MTYPE.EQ.43) THEN
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
                  N=21
C
                  DATA (H2OX(J), J = 1,21)/0.1829D0,0.20255D0,0.25020D0,0.30822D0,
     1            0.35871D0,0.40466D0,0.44715D0,0.50157D0,0.54607D0,0.58926D0,
     2            0.65628D0,0.70652D0,0.76820D0,0.808D0,0.871D0,0.943D0,1.028D0,
     3            1.130D0,1.256D0,1.617D0,1.968/
C
                  DATA (H2OY(J), J = 1,21)/1.46379D0,1.41993D0,1.37734D0,1.35671D0,
     1            1.34795D0,1.342724D0,1.339423D0,1.336363D0,1.334466D0,1.332988D0,
     2            1.331151D0,1.330019D0,1.32890D0,1.3286D0,1.3273D0,1.3262D0,
     3            1.3250D0,1.3234D0,1.3215D0,1.3149D0,1.3078D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=H2OX(1:N)
                  Y(1:N)=H2OY(1:N)
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
              END IF
              IF(MTYPE.EQ.44) THEN
C     INTERPOLATE THE SUPRASIL
                  N=60
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
C
C     DATA FOR SUPRASIL
C
                  DATA (SUPX(J), J = 1,60)/0.19D0,0.20D0,0.21D0,0.22D0,
     1            0.23D0,0.24D0,0.25D0,0.26D0,0.27D0,0.28D0,
     2            0.29D0,0.30D0,0.32D0,0.34D0,0.36D0,0.36548D0,0.38D0,
     3            0.4D0,0.40465D0,0.43583D0,0.45D0,0.48613D0,0.5D0,0.54607D0,
     4            0.55D0,0.58756D0,0.6D0,0.65D0,0.65627D0,0.7D0,0.75D0,0.8D0,
     5            0.85D0,0.9D0,1.0D0,1.1D0,1.2D0,1.3D0,1.4D0,1.5D0,1.6D0,1.7D0,
     6            1.8D0,1.9D0,2.0D0,2.1D0,2.2D0,2.3D0,2.4D0,2.5D0,2.6D0,2.7D0,
     7            2.8D0,2.9D0,3.0D0,3.1D0,3.2D0,3.3D0,3.4D0,3.5D0/
C
                  DATA (SUPY(J), J = 1,60)/
     1            1.56572D0,1.55051D0,1.53836D0,1.52845D0,1.52024D0,1.51333D0,
     2            1.50745D0,1.50239D0,1.49800D0,1.49416D0,1.49079D0,1.48779D0,
     3            1.48274D0,1.47865D0,1.47529D0,1.47447D0,1.47248D0,1.47012D0,
     4            1.46962D0,1.46669D0,1.46557D0,1.46313D0,1.46233D0,1.46008D0,
     5            1.45991D0,1.45846D0,1.45804D0,1.45653D0,1.45637D0,1.45529D0,
     6            1.45424D0,1.45332D0,1.45250D0,1.45175D0,1.45042D0,1.44920D0,
     7            1.44805D0,1.44692D0,1.44758D0,1.44462D0,1.44342D0,1.44217D0,
     8            1.44087D0,1.43951D0,1.43809D0,1.43659D0,1.43501D0,1.43336D0,
     9            1.43163D0,1.42980D0,1.42789D0,1.42588D0,1.42377D0,1.42156D0,
     1            1.41925D0,1.41682D0,1.41427D0,1.41161D0,1.40881D0,1.40589D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=SUPX(1:N)
                  Y(1:N)=SUPY(1:N)
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              END IF
              IF(MTYPE.EQ.45) THEN
C     INTERPOLATE THE HOMOSIL
                  N=47
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
C
C     DATA FOR SUPRASIL
C
                  DATA (HOMOX(J), J = 1,47)/
     1            0.34D0,0.36D0,0.36548D0,0.38D0,
     3            0.4D0,0.40465D0,0.43583D0,0.45D0,0.48613D0,0.5D0,0.54607D0,
     4            0.55D0,0.58756D0,0.6D0,0.65D0,0.65627D0,0.7D0,0.75D0,0.8D0,
     5            0.85D0,0.9D0,1.0D0,1.1D0,1.2D0,1.3D0,1.4D0,1.5D0,1.6D0,1.7D0,
     6            1.8D0,1.9D0,2.0D0,2.1D0,2.2D0,2.3D0,2.4D0,2.5D0,2.6D0,2.7D0,
     7            2.8D0,2.9D0,3.0D0,3.1D0,3.2D0,3.3D0,3.4D0,3.5D0/
C
                  DATA (HOMOY(J), J = 1,47)/1.47881D0,1.47544D0,1.47462D0,
     1            1.47262D0,1.47025D0,1.46975D0,1.46681D0,1.46568D0,1.46324D0,
     2            1.46243D0,1.46018D0,1.46001D0,1.45856D0,1.45814D0,1.45663D0,
     3            1.45646D0,1.45539D0,1.45433D0,1.45341D0,1.45259D0,1.45185D0,
     4            1.45051D0,1.44930D0,1.44815D0,1.44702D0,1.44589D0,1.44473D0,
     5            1.44353D0,1.44229D0,1.44099D0,1.43964D0,1.43821D0,1.43672D0,
     6            1.43515D0,1.43350D0,1.43177D0,1.42995D0,1.42804D0,1.42604D0,
     7            1.42393D0,1.42172D0,1.41941D0,1.41698D0,1.41444D0,1.41177D0,
     8            1.40897D0,1.40605D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=HOMOX(1:N)
                  Y(1:N)=HOMOY(1:N)
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              END IF
              IF(MTYPE.EQ.46) THEN
C     INTERPOLATE THE ZNS-MS II-VI DATA
                  N=59
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
                  YP1=1.0D35
                  YPN=1.0D35
C
C     DATA FOR ZNS-MS II-VI DATA
C
                  DATA (ZNSMSX(J), J = 1,59)/
     1            0.42D0,0.46D0,0.50D0,0.54D0,0.58D0,0.62D0,0.66D0,0.70D0,
     2            0.74D0,0.78D0,0.82D0,0.86D0,0.90D0,0.94D0,0.98D0,1.00D0,
     3            1.40D0,1.80D0,2.20D0,2.60D0,3.00D0,3.40D0,3.80D0,4.20D0,
     4            4.60D0,5.00D0,5.40D0,5.80D0,6.20D0,6.60D0,7.00D0,7.40D0,
     5            7.80D0,8.20D0,8.60D0,9.00D0,9.40D0,9.80D0,10.20D0,10.60D0,
     6            11.00D0,11.40D0,11.80D0,12.20D0,12.60D0,13.0D0,13.40D0,13.80D0,
     7            14.20D0,14.60D0,15.00D0,15.40D0,15.80D0,16.2D0,16.60D0,17.00D0,
     8            17.40D0,17.80D0,18.20D0/
C
                  DATA (ZNSMSY(J), J = 1,59)/
     1            2.516D0,2.458D0,2.419D0,2.391D0,2.371D0,2.355D0,2.342D0,2.332D0,
     2            2.323D0,2.316D0,2.310D0,2.305D0,2.301D0,2.297D0,2.294D0,2.292D0,
     3            2.275D0,2.267D0,2.263D0,2.260D0,2.257D0,2.255D0,2.253D0,2.251D0,
     4            2.248D0,2.246D0,2.244D0,2.241D0,2.238D0,2.235D0,2.232D0,2.228D0,
     5            2.225D0,2.221D0,2.217D0,2.212D0,2.208D0,2.203D0,2.198D0,2.192D0,
     6            2.186D0,2.180D0,2.173D0,2.167D0,2.159D0,2.152D0,2.143D0,2.135D0,
     7            2.126D0,2.116D0,2.106D0,2.095D0,2.084D0,2.072D0,2.059D0,2.045D0,
     8            2.030D0,2.015D0,1.998D0/
C
C     MOVE TO LARGE ARRAYS
                  X(1:N)=ZNSMSX(1:N)
                  Y(1:N)=ZNSMSY(1:N)
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
                  CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
                  CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
                  CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
              END IF
          END IF
C
C
          IF(MTYPE.EQ.47) THEN
C     INTERPOLATE THE CEF3
C
C     CALCULATE THE INDICES
              IF(GLSWV(1).NE.0.0D0) THEN
                  GPREG(1)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(GLSWV(1)-2.0D0))+1.59D0
                  IF(GLSWV(1).LE.0.55D0) GPREG(1)=1.63D0
                  IF(GLSWV(1).GE.2.00D0) GPREG(1)=1.59D0
              ELSE
                  GPREG(1)=1.0D0
              END IF
              IF(GLSWV(2).NE.0.0D0) THEN
                  GPREG(2)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(GLSWV(2)-2.0D0))+1.59D0
                  IF(GLSWV(2).LE.0.55D0) GPREG(2)=1.63D0
                  IF(GLSWV(2).GE.2.00D0) GPREG(2)=1.59D0
              ELSE
                  GPREG(2)=1.0D0
              END IF
              IF(GLSWV(3).NE.0.0D0) THEN
                  GPREG(3)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(GLSWV(3)-2.0D0))+1.59D0
                  IF(GLSWV(3).LE.0.55D0) GPREG(3)=1.63D0
                  IF(GLSWV(3).GE.2.00D0) GPREG(3)=1.59D0
              ELSE
                  GPREG(3)=1.0D0
              END IF
              IF(GLSWV(4).NE.0.0D0) THEN
                  GPREG(4)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(GLSWV(4)-2.0D0))+1.59D0
                  IF(GLSWV(4).LE.0.55D0) GPREG(4)=1.63D0
                  IF(GLSWV(4).GE.2.00D0) GPREG(4)=1.59D0
              ELSE
                  GPREG(4)=1.0D0
              END IF
              IF(GLSWV(5).NE.0.0D0) THEN
                  GPREG(5)
     1            =(((1.63D0-1.59D0)/(0.55D0-2.0D0))*(GLSWV(5)-2.0D0))+1.59D0
                  IF(GLSWV(5).LE.0.55D0) GPREG(5)=1.63D0
                  IF(GLSWV(5).GE.2.00D0) GPREG(5)=1.59D0
              ELSE
                  GPREG(5)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.48) THEN
C     INTERPOLATE THE LS203
C
C     CALCULATE THE INDICES
              IF(GLSWV(1).NE.0.0D0) THEN
                  GPREG(1)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(GLSWV(1)-2.0D0))+1.86D0
                  IF(GLSWV(1).LE.0.55D0) GPREG(1)=1.95D0
                  IF(GLSWV(1).GE.2.00D0) GPREG(1)=1.86D0
              ELSE
                  GPREG(1)=1.0D0
              END IF
              IF(GLSWV(2).NE.0.0D0) THEN
                  GPREG(2)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(GLSWV(2)-2.0D0))+1.86D0
                  IF(GLSWV(2).LE.0.55D0) GPREG(2)=1.95D0
                  IF(GLSWV(2).GE.2.00D0) GPREG(2)=1.86D0
              ELSE
                  GPREG(2)=1.0D0
              END IF
              IF(GLSWV(3).NE.0.0D0) THEN
                  GPREG(3)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(GLSWV(3)-2.0D0))+1.86D0
                  IF(GLSWV(3).LE.0.55D0) GPREG(3)=1.95D0
                  IF(GLSWV(3).GE.2.00D0) GPREG(3)=1.86D0
              ELSE
                  GPREG(3)=1.0D0
              END IF
              IF(GLSWV(4).NE.0.0D0) THEN
                  GPREG(4)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(GLSWV(4)-2.0D0))+1.86D0
                  IF(GLSWV(4).LE.0.55D0) GPREG(4)=1.95D0
                  IF(GLSWV(4).GE.2.00D0) GPREG(4)=1.86D0
              ELSE
                  GPREG(4)=1.0D0
              END IF
              IF(GLSWV(5).NE.0.0D0) THEN
                  GPREG(5)
     1            =(((1.95D0-1.86D0)/(0.55D0-2.0D0))*(GLSWV(5)-2.0D0))+1.86D0
                  IF(GLSWV(5).LE.0.55D0) GPREG(5)=1.95D0
                  IF(GLSWV(5).GE.2.00D0) GPREG(5)=1.86D0
              ELSE
                  GPREG(5)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.49) THEN
C     INTERPOLATE THE THF4
C
C     CALCULATE THE INDICES
              IF(GLSWV(1).NE.0.0D0) THEN
                  GPREG(1)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(GLSWV(1)-.75D0))+1.51D0
                  IF(GLSWV(1).LE.0.40D0) GPREG(1)=1.52D0
                  IF(GLSWV(1).GE.0.75D0) GPREG(1)=1.51D0
              ELSE
                  GPREG(1)=1.0D0
              END IF
              IF(GLSWV(2).NE.0.0D0) THEN
                  GPREG(2)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(GLSWV(2)-.75D0))+1.51D0
                  IF(GLSWV(2).LE.0.40D0) GPREG(2)=1.52D0
                  IF(GLSWV(2).GE.0.75D0) GPREG(2)=1.51D0
              ELSE
                  GPREG(2)=1.0D0
              END IF
              IF(GLSWV(3).NE.0.0D0) THEN
                  GPREG(3)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(GLSWV(3)-.75D0))+1.51D0
                  IF(GLSWV(3).LE.0.40D0) GPREG(3)=1.52D0
                  IF(GLSWV(3).GE.0.75D0) GPREG(3)=1.51D0
              ELSE
                  GPREG(3)=1.0D0
              END IF
              IF(GLSWV(4).NE.0.0D0) THEN
                  GPREG(4)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(GLSWV(4)-.75D0))+1.51D0
                  IF(GLSWV(4).LE.0.40D0) GPREG(4)=1.52D0
                  IF(GLSWV(4).GE.0.75D0) GPREG(4)=1.51D0
              ELSE
                  GPREG(4)=1.0D0
              END IF
              IF(GLSWV(5).NE.0.0D0) THEN
                  GPREG(5)
     1            =(((1.52D0-1.51D0)/(0.40D0-0.75D0))*(GLSWV(5)-.75D0))+1.51D0
                  IF(GLSWV(5).LE.0.40D0) GPREG(5)=1.52D0
                  IF(GLSWV(5).GE.0.75D0) GPREG(5)=1.51D0
              ELSE
                  GPREG(5)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.50) THEN
C     INTERPOLATE THE ZRO2
C
C     CALCULATE THE INDICES
              IF(GLSWV(1).NE.0.0D0) THEN
                  GPREG(1)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(GLSWV(1)-.75D0))+2.00D0
                  IF(GLSWV(1).LE.0 .55D0) GPREG(1)=2.10D0
                  IF(GLSWV(1).GE..75D0) GPREG(1)=2.00D0
              ELSE
                  GPREG(1)=1.0D0
              END IF
              IF(GLSWV(2).NE.0.0D0) THEN
                  GPREG(2)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(GLSWV(2)-.75D0))+2.00D0
                  IF(GLSWV(2).LE.0 .55D0) GPREG(2)=2.10D0
                  IF(GLSWV(2).GE..75D0) GPREG(2)=2.00D0
              ELSE
                  GPREG(2)=1.0D0
              END IF
              IF(GLSWV(3).NE.0.0D0) THEN
                  GPREG(3)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(GLSWV(3)-.75D0))+2.00D0
                  IF(GLSWV(3).LE.0 .55D0) GPREG(3)=2.10D0
                  IF(GLSWV(3).GE..75D0) GPREG(3)=2.00D0
              ELSE
                  GPREG(3)=1.0D0
              END IF
              IF(GLSWV(4).NE.0.0D0) THEN
                  GPREG(4)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(GLSWV(4)-.75D0))+2.00D0
                  IF(GLSWV(4).LE.0 .55D0) GPREG(4)=2.10D0
                  IF(GLSWV(4).GE..75D0) GPREG(4)=2.00D0
              ELSE
                  GPREG(4)=1.0D0
              END IF
              IF(GLSWV(5).NE.0.0D0) THEN
                  GPREG(5)
     1            =(((2.10D0-2.00D0)/(0.55D0-2.0D0))*(GLSWV(5)-.75D0))+2.00D0
                  IF(GLSWV(5).LE.0 .55D0) GPREG(5)=2.10D0
                  IF(GLSWV(5).GE..75D0) GPREG(5)=2.00D0
              ELSE
                  GPREG(5)=1.0D0
              END IF
C
          END IF
          IF(MTYPE.EQ.51) THEN
C     INTERPOLATE THE DIAMOND
C
C     CALCULATE THE INDICES
              GPREG(1)=DIAMOND(GLSWV(1))
              GPREG(2)=DIAMOND(GLSWV(2))
              GPREG(3)=DIAMOND(GLSWV(3))
              GPREG(4)=DIAMOND(GLSWV(4))
              GPREG(5)=DIAMOND(GLSWV(5))
          END IF
          IF(MTYPE.EQ.52) THEN
C     INTERPOLATE THE YAG
C
C     CALCULATE THE INDICES
              GPREG(1)=YAG(GLSWV(1))
              GPREG(2)=YAG(GLSWV(2))
              GPREG(3)=YAG(GLSWV(3))
              GPREG(4)=YAG(GLSWV(4))
              GPREG(5)=YAG(GLSWV(5))
          END IF
          IF(MTYPE.GE.101.AND.MTYPE.LE.104) THEN
C
C     PLASTICS
C
              DATA (GPLX(J), J = 1,13)/0.36501D0,0.40466D0,0.43484D0
     1        ,0.47999D0,0.48613D0,0.54607D0,0.58756D0,0.58929D0,0.64385D0
     2        ,0.65627D0,0.70652D0,0.85211D0,1.01398D0/
C
              DATA (GPL1Y(J), J = 1,13)/1.513613D0,1.506607D0,1.502557D0
     1        ,1.498258D0,1.497760D0,1.493795D0,1.491757D0,1.491681D0
     2        ,1.489603D0,1.489201D0,1.487552D0,1.484965D0,1.483115D0/
C
              DATA (GPL2Y(J), J = 1,13)/1.643126D0,1.625341D0,1.615466D0
     1        ,1.605241D0,1.604079D0,1.595010D0,1.590481D0,1.590315D0
     2        ,1.585808D0,1.584949D0,1.581954D0,1.576196D0,1.572553D0/
C
              DATA (GPL3Y(J), J = 1,13)/1.643231D0,1.622447D0,1.611519D0
     1        ,1.600654D0,1.599439D0,1.590081D0,1.585470D0,1.585302D0
     2        ,1.580734D0,1.579864D0,1.576831D0,1.570981D0,1.567248D0/
C
              DATA (GPL4Y(J), J = 1,13)/1.612490D0,1.597075D0,1.588640D0
     1        ,1.579985D0,1.579000D0,1.571300D0,1.567400D0,1.567298D0
     2        ,1.563438D0,1.562700D0,1.560119D0,1.555108D0,1.551870D0/
C
              N=13
C
              IF(MTYPE.EQ.101) THEN
C     INTERPOLATE THE ACRYLIC DATA
                  X(1:N)=GPLX(1:N)
                  Y(1:N)=GPL1Y(1:N)
              END IF
              IF(MTYPE.EQ.102) THEN
C     INTERPOLATE THE POLYSTYRENE DATA
                  X(1:N)=GPLX(1:N)
                  Y(1:N)=GPL2Y(1:N)
              END IF
              IF(MTYPE.EQ.103) THEN
C     INTERPOLATE THE POLYCARBONATE DATA
                  X(1:N)=GPLX(1:N)
                  Y(1:N)=GPL3Y(1:N)
              END IF
              IF(MTYPE.EQ.104) THEN
C     INTERPOLATE THE (SAN) DATA
                  X(1:N)=GPLX(1:N)
                  Y(1:N)=GPL4Y(1:N)
              END IF
C     USE NORMAL SPLINES WITH ZERO SECOND DERIVATIVE AT END POINTS
              YP1=1.0D35
              YPN=1.0D35
C
C     MOVE TO LARGE ARRAYS
C
C     CALCULATE SECOND DERIVATIVES AND RETURN THEM IN Y2
              CALL SPLINE(X,Y,N,YP1,YPN,Y2)
C
C     CALCULATE THE INDICES
              CALL SPLINT(X,Y,Y2,N,GLSWV(1),GPREG(1))
              CALL SPLINT(X,Y,Y2,N,GLSWV(2),GPREG(2))
              CALL SPLINT(X,Y,Y2,N,GLSWV(3),GPREG(3))
              CALL SPLINT(X,Y,Y2,N,GLSWV(4),GPREG(4))
              CALL SPLINT(X,Y,Y2,N,GLSWV(5),GPREG(5))
C
          END IF
          RETURN
      END
