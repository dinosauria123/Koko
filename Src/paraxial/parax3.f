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

C       THIRD SET OF PARAXIAL ROUTINES GO HERE

C SUB ENEXRS.FOR
      SUBROUTINE ENEXRS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE ENEXRS. IT IS CALLED FROM PRTRA1
C       AND IT USES YZ-PLANE DATA TO ESTABLISH ENTRANCE AND
C       EXIT PUPIL ADJUSTMENTS (WHERE REQUESTED WITH EN, EX, OR
C       ENEX QUALIFIERS WITH THE ASTOP COMMAND)
C
          INTEGER J,IMSUR,IM2,IM1
C
          REAL*8 THNEW,THNEW2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       IS THERE AN ASTOP DEFINED OR IS TEL ON?
          IF(SYSTEM1(26).LT.0.0D0.OR.SYSTEM1(63).EQ.1.0D0) THEN
C       NO ASTOP, RETURN ONLY
              RETURN
          ELSE
C       ASTOP EXISTS
          END IF
C       IS THERE AN ADJUSTMENT?
          IF(SYSTEM1(27).EQ.0.0D0) THEN
C       NO ADJUSTMENT, JUST RETURN
              RETURN
          ELSE
C       THERE IS AN ADJUSTMENT REQUESTED
          END IF
          IF(SYSTEM1(27).EQ.1.0D0.OR.SYSTEM1(27).EQ.2.0D0) THEN
C       ASTOP EN OR ASTOP ENEX REQUESTED
              IF(SYSTEM1(26).EQ.1.0D0) THEN
C       ASTOP IS ON SURFACE 1, NO EN ADJUSTMENT IS NECESSARY
                  GO TO 100
              ELSE
C       ADJUSTMENT IS NECESSARY, PROCEED
              END IF
C       ASTOP
C       DO AN ENTRANCE PUPIL ADJUSTMENT
C       THIS ADJUSTMENT IS PERFORMED BY CHANGING THE OBJECT
C       DISTANCE TH(0) SO THAT SURFACE 1 HAS PCY(1) = 0.0
C       THEN THE AMOUNT ADDED TO THE THICKNESS OF SURFACE 0
C       IS ADDED WITH THE OPPOSITE SIGN TO THE THICKNESS
C       OF SURFACE 1.
C
C       (WARNING:) ASTOP EN OR ASTOP ENEX WILL NOT PERFORM
C       THE ENTRACE PUPIL ADJUSTMENT UNLESS SURFACE 1 AND 2
C       ARE DUMMY SURFACES WITH THE SAME INDEX OF
C       REFRACTION ON EACH SIDE. THERE
C       MUST BE NO SOLVES ASSIGNED TO SURFACE 1 AND 2
C       ALSO, NO THICKNESS PIKUPS ARE ALLOWED ON
C       SURFACE 0,1 OR 2
C               IS THIS THE CASE?
              DO 22 J=0,2
                  IF(PIKUP(1,J,3).NE.0.0D0.OR.PIKUP(1,J,32).NE.0.0D0) THEN
C       FOUND A THICKNESS PIKUP,CAN'T DO THE
C       ADJUSTMENT
                      OUTLYNE= 'A "TH" OR "THOAL" PIKUP ON SURFACE 0, 1 OR 2'
                      CALL SHOWIT(1)
                      OUTLYNE= 'EXISTS. THE (EN) ADJUSTMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      OUTLYNE='THE (EN) ADJUSTMENT IS BEING CANCELLED'
                      CALL SHOWIT(1)
                      IF(SYSTEM1(27).EQ.1.0D0) SYSTEM1(27)=0.0D0
                      IF(SYSTEM1(27).EQ.2.0D0) SYSTEM1(27)=-1.0D0
                      GO TO 100
                  ELSE
C       PROCEED
                  END IF
 22           CONTINUE
              IF(SOLVE(4,1).EQ.0.0D0
     1        .AND.SOLVE(2,1).EQ.0.0D0
     2        .AND.SOLVE(6,1).EQ.0.0D0
     3        .AND.SOLVE(8,1).EQ.0.0D0
     1        .AND.SOLVE(4,2).EQ.0.0D0
     1        .AND.SOLVE(2,2).EQ.0.0D0
     2        .AND.SOLVE(6,2).EQ.0.0D0
     3        .AND.SOLVE(8,2).EQ.0.0D0
     4        .AND.ALENS(46,0).EQ.ALENS(46,1)
     5        .AND.ALENS(47,0).EQ.ALENS(47,1)
     6        .AND.ALENS(48,0).EQ.ALENS(48,1)
     7        .AND.ALENS(49,0).EQ.ALENS(49,1)
     8        .AND.ALENS(50,0).EQ.ALENS(50,1)
     4        .AND.ALENS(46,0).EQ.ALENS(46,2)
     5        .AND.ALENS(47,0).EQ.ALENS(47,2)
     6        .AND.ALENS(48,0).EQ.ALENS(48,2)
     7        .AND.ALENS(49,0).EQ.ALENS(49,2)
     8        .AND.ALENS(50,0).EQ.ALENS(50,2)) THEN
C       YES, SURFACE IS OK FOR ADJUSTMENT
              ELSE
C       ADJUSTMENT CAN'T BE DONE.
                  OUTLYNE= 'WARNING: ENTRANCE PUPIL ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'SURFACES 1 AND/OR 2 ARE EITHER NOT DUMMY SURFACES OR THEY'
                  CALL SHOWIT(1)
                  OUTLYNE= 'HAVE SOLVES ASSIGNED TO THEM'
                  CALL SHOWIT(1)
                  OUTLYNE='THE (EN) ADJUSTMENT IS BEING CANCELLED'
                  CALL SHOWIT(1)
                  IF(SYSTEM1(27).EQ.1.0D0) SYSTEM1(27)=0.0D0
                  IF(SYSTEM1(27).EQ.2.0D0) SYSTEM1(27)=-1.0D0
                  GO TO 100
              END IF
C
C       THE VALUES OF PXTRAY(1,1) THROUGH PXTRAY(8,1)
C       ALREADY EXIST. WE FIRST SOLVE FOR THE NEW THICKNESS
C       OF SURFACE 1 SO THAT PCY(2)=0
C       THIS IS THE SAME AS A PCY SOLVE
C       FROM OUR WORK WITH SOLVES WE KNOW THAT
C       TH(L)=(SOLVE TARGET-PCY(L-1))/PUCY(L)
C       AND FOR OUR SPECIFIC CASE
C       TH(1)[NEW]=(0.0-SCY)/EXISTING PUCY(1) ANGEL
C
              IF(DABS(PXTRAY(6,1)).LT.1.0D-20) THEN
                  THNEW=1.0D20
              ELSE
                  THNEW=(-PXTRAY(5,(1)))/(PXTRAY(6,1))
              END IF
C
              ALENS(3,1)=THNEW
              ALENS(3,2)=-THNEW
C
C       NOW WE RE-CALCULATE ALL THE PARAXIAL VALUES AT
C       SURFACE 2
C
C        ASTOP EN DOES NOT AFFECT RAY SLOPES ONLY PCY AND PY AT 2
C
C       PY(2) = USED TO BE THE SAY VALUE WHICH MUST BE ADJUSTED NOW:
C       PY(2)=PY(1)+(TH(1)*PUY(1))
              PXTRAY(1,2)=PXTRAY(1,1)+(PXTRAY(2,1)*ALENS(3,1))
C       PCY(2) = PCY(1)+(TH(1)*PUCY(1)) ; THIS IS THE TRANSFER EQUATION
              PXTRAY(5,2)=PXTRAY(5,(1))+(ALENS(3,(1))*PXTRAY(6,(1)))
C
          ELSE
C        NO EN ADJUSTMENT WAS REQUESTED, PROCEED WITH PROCESSING
          END IF
 100      IF(SYSTEM1(27).EQ.-1.0D0.OR.SYSTEM1(27).EQ.2.0D0) THEN
              IF(SYSTEM1(26).EQ.(SYSTEM1(20)-1.0D0))THEN
C       ASTOP IS ON SURFACE IMAGE-1, NO ADJUSTMENT IS NECESSARY
                  GO TO 200
              ELSE
C       ADJUSTMENT IS NECESSARY, PROCEED
              END IF
C       DO AN EXIT PUPIL ADJUSTMENT
C       THIS ADJUSTMENT IS PERFORMED BY CHANGING THE THICKNESS OF
C       SURFACE (IMAGE SURF-2) SO THAT PCY(IMAGE SURF-1) = 0.0
C       THEN THE AMOUNT ADDED TO THE THICKNESS OF SURFACE (IMAGE SURF-2)
C       IS ADDED WITH THE OPPOSITE SIGN TO THE THICKNESS
C       OF SURFACE (IMAGE SURF-1).
C
C       (WARNING:) ASTOP EX OR ASTOP ENEX WILL NOT PERFORM
C       THE EXIT PUPIL ADJUSTMENT UNLESS SURFACES IMAGE-1 AND IMAGE-2
C       ARE DUMMY SURFACES WITH NO SOLVES ASSIGNED TO THEM
C       ALSO, NO THICKNESS PIKUPS ARE ALLOWED ON
C       SURFACE (IMAGE-2) OR SURFACE (IMAGE-1)
C       IS THIS THE CASE
C               IS THIS THE CASE?
              DO 44 J=(INT(SYSTEM1(20))-2),(INT(SYSTEM1(20))-1)
                  IF(PIKUP(1,J,3).NE.0.0D0.OR.PIKUP(1,J,32).NE.0.0D0) THEN
C       FOUND A THICKNESS PIKUP,CAN'T DO THE
C       ADJUSTMENT
                      OUTLYNE=
     1                'A "TH" OR "THOAL" PIKUP ON SURFACE (IMAGE-2) OR (IMAGE-1)'
                      CALL SHOWIT(1)
                      OUTLYNE= 'EXISTS. THE (EX) ADJUSTMENT WAS NOT MADE'
                      CALL SHOWIT(1)
                      OUTLYNE='THE (EX) ADJUSTMENT IS BEING CANCELLED'
                      CALL SHOWIT(1)
                      IF(SYSTEM1(27).EQ.-1.0D0) SYSTEM1(27)=0.0D0
                      IF(SYSTEM1(27).EQ.2.0D0) SYSTEM1(27)=1.0D0
                      GO TO 200
                  ELSE
C       PROCEED
                  END IF
 44           CONTINUE
              IMSUR=INT(SYSTEM1(20))
              IM2=IMSUR-2
              IM1=IMSUR-1
              IF(SOLVE(4,IM1).EQ.0.0D0.AND.SOLVE(2,IM1).EQ.0.0D0
     1        .AND.SOLVE(6,IM1).EQ.0.0D0.AND.SOLVE(8,IM1).EQ.0.0D0
     2        .AND.ALENS(46,IM1).EQ.ALENS(46,IMSUR)
     3        .AND.ALENS(47,IM1).EQ.ALENS(47,IMSUR)
     4        .AND.ALENS(48,IM1).EQ.ALENS(48,IMSUR)
     5        .AND.ALENS(49,IM1).EQ.ALENS(49,IMSUR)
     6        .AND.ALENS(50,IM1).EQ.ALENS(50,IMSUR)
     7        .AND.ALENS(46,IM1).EQ.ALENS(46,IM2)
     8        .AND.ALENS(47,IM1).EQ.ALENS(47,IM2)
     9        .AND.ALENS(48,IM1).EQ.ALENS(48,IM2)
     1        .AND.ALENS(49,IM1).EQ.ALENS(49,IM2)
     2        .AND.ALENS(50,IM1).EQ.ALENS(50,IM2)
     3        .AND.ALENS(46,IM1).EQ.ALENS(46,IM2-1)
     4        .AND.ALENS(47,IM1).EQ.ALENS(47,IM2-1)
     5        .AND.ALENS(48,IM1).EQ.ALENS(48,IM2-1)
     6        .AND.ALENS(49,IM1).EQ.ALENS(49,IM2-1)
     7        .AND.ALENS(50,IM1).EQ.ALENS(50,IM2-1)
     8        .AND.SOLVE(4,IM2).EQ.0.0D0.AND.SOLVE(2,IM2).EQ.0.0D0
     9        .AND.SOLVE(6,IM2).EQ.0.0D0.AND.SOLVE(8,IM2).EQ.0.0D0) THEN
C       YES, SURFACE IS OK FOR ADJUSTMENT
              ELSE
C       ADJUSTMENT CAN'T BE DONE.
                  OUTLYNE='WARNING: EXIT PUPIL ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'SURFACES (IMAGE-1) AND (IMAGE-2) ARE EITHER NOT DUMMY'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'SURFACES OR THEY HAVE SOLVES ASSIGNED TO THEM'
                  CALL SHOWIT(1)
                  OUTLYNE='THE (EX) ADJUSTMENT IS BEING CANCELLED'
                  CALL SHOWIT(1)
                  IF(SYSTEM1(27).EQ.-1.0D0) SYSTEM1(27)=0.0D0
                  IF(SYSTEM1(27).EQ.2.0D0) SYSTEM1(27)=1.0D0
                  GO TO 200
              END IF
C
C       THE VALUES OF PXTRAY(1,IM2) THROUGH PXTRAY(8,IM2)
C       ALREADY EXIST. WE FIRST SOLVE FOR THE NEW THICKNESS
C       OF SURFACE IM2 SO THAT PCY(IM1)=0
C       THIS IS THE SAME AS A PCY SOLVE
C       FROM OUR WORK WITH SOLVES WE KNOW THAT
C       TH(L)=(SOLVE TARGET-PCY(L-1))/PUCY(L)
C       AND FOR OUR SPECIFIC CASE
C       TH(IM2)[NEW]=(0.0-PCY(IM2)/EXISTING PUCY(IM2)
C
              IF(DABS(PXTRAY(6,IM2)).LT.1.0D-20) THEN
                  THNEW2=1.0D20
              ELSE
                  THNEW2=(-PXTRAY(5,(IM2)))/(PXTRAY(6,IM2))
              END IF
C
              ALENS(3,IM2)=THNEW2
              ALENS(3,IM1)=-THNEW2
C
C       NOW WE RE-CALCULATE ALL THE PARAXIAL VALUES AT
C       SURFACE IM1 ONLY THE INTERCEPTS NEED RECALCULATION
C       AS ASTOP EX DOES NOT AFFECT SLOPES
C
C       PY(IM1) = PY(IM2)+TH(IM2)*PUY(IM2) ; THIS IS THE TRANSFER EQUATION
              PXTRAY(1,IM1)=PXTRAY(1,(IM2))+(ALENS(3,(IM2))*PXTRAY(2,(IM2)))
C
C       PCY(IM1) = PCY(IM2)+TH(IM2)*PUCY(IM2) ; THIS IS THE TRANSFER EQUATION
              PXTRAY(5,IM1)=PXTRAY(5,(IM2))+(ALENS(3,(IM2))*PXTRAY(6,(IM2)))
C
          ELSE
C       WE ARE DONE WITH THE ADJUSTMENTS
          END IF
 200      CONTINUE
          RETURN
      END
C SUB ERADJ.FOR
      SUBROUTINE ERADJ
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE ERADJ. THIS IS THE SUBROUTINE
C       WHICH PERFORMS THE ERY/X ADJUSTMENT
C
          INTEGER ITYPEP
C
          COMMON/PTYPER/ITYPEP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(ITYPEP.EQ.1) THEN
C
C       FIRST CHECK FOR PUY (NOT ) =0 AT IMAGE PLANE
              IF(DABS(PXTRAY(2,(INT(SYSTEM1(20))))).GT.1.0D-15) THEN
                  OUTLYNE='FINAL "PUY" VALUE IS NOT ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='"ERY" ADJUSTMENT NOT DEFINED AND WILL BE REMOVED'
                  CALL SHOWIT(1)
                  OUTLYNE='NOT ADJUSTMENT PERFORMED'
                  CALL SHOWIT(1)
                  SYSTEM1(44)=0.0D0
                  SYSTEM1(46)=0.0D0
                  RETURN
              ELSE
C       PUY(FINAL) IS ZERO, PROCEED
              END IF
C       NOW, THE CURRENT EXIT PUPIL RADIUS IS PY(FINAL)
C       THE DESIRED EXIT PUPIL RADIUS IS STORED IN SYSTEM1(46)
C       IF ALL SOLVED ARE SHUT OFF, THE NEW EXIT PUPIL RADIUS
C       IS REALIZED BY SCALING THE SAY VALUE BY:
C
C               NEW DESIRED EXIT PUPIL RADIUS/OLD EXIT PUPIL RADIUS
              IF(DABS(PXTRAY(1,(INT(SYSTEM1(20))))).LT.1.0D-15) THEN
                  OUTLYNE='CURRENT EXIT PUPIL RADIUS IS ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='"ERY" ADJUSTMENT NOT DEFINED AND WILL BE REMOVED'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ADJUSTMENT PERFORMED'
                  CALL SHOWIT(1)
                  SYSTEM1(44)=0.0D0
                  SYSTEM1(46)=0.0D0
                  RETURN
              ELSE
              END IF
C
              SYSTEM1(12)=SYSTEM1(12)*(SYSTEM1(46)/
     1        PXTRAY(1,(INT(SYSTEM1(20)))))
C
C       NOW WE PERFORM A PARAXIAL RAY TRACE WITHOUT AND SOLVES
C
C       TELECENTRIC STUFF, 11/12/2000
              IF(SYSTEM1(63).EQ.1.0D0) THEN
                  IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                      OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                      CALL SHOWIT(1)
                      OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                      CALL SHOWIT(1)
                      OUTLYNE='VALUES'
                      CALL SHOWIT(1)
                      OUTLYNE='PARAXIAL TRACE STOPPED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      IF(SYSTEM1(64).EQ.1.0D0) THEN
                          SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                          SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                      END IF
                      IF(SYSTEM1(67).EQ.1.0D0) THEN
                          SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                          SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                      END IF
                  END IF
              END IF
C
              CALL TR
C
C       NOW THE PARAXIAL SOLVE DATA ARRAYS ARE LOADED WITH THE CORRECT
C       SOLVELESS DATA. NOW RE-SET ALL SOLVE TARGET VALUES
C
C       WHEN THE RETURN OCCURS, THE RETURN WILL GO TO LNSEOS IN
C       THE CASE OF HLDS OR TO SFNBY.FOR OR SER.FOR. LNSEOS.FOR
C       WILL AGAIN PERFORM THE TRACE FIXING THE EXIT PUPIL DISTACES AND
C       CALCULATING CHROMATIC AND OTHER VALUES.
C       A CALL TO PRTRA1 AND PRTRA2 IS PLACED IN SER.FOR AND SFNB.FOR
C       TO PERFORM A SIMILAR FUNCTION.
C       SVYSET RESETS ALL YZ-PLANE PARAXIAL SOLVES USING CURRENT YZ-PLANE
C       PARAXIAL DATA.
              CALL SVSET
              RETURN
          ELSE
C       ITYPEP NOT 1
          END IF
          IF(ITYPEP.EQ.2) THEN
C
C       FIRST CHECK FOR PUX (NOT ) =0 AT IMAGE PLANE
              IF(DABS(PXTRAX(2,(INT(SYSTEM1(20))))).GT.1.0D-15) THEN
                  OUTLYNE= 'FINAL "PUX" VALUE IS NOT ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE= '"ERX" ADJUSTMENT NOT DEFINED AND WILL BE REMOVED'
                  CALL SHOWIT(1)
                  OUTLYNE= 'NOT ADJUSTMENT PERFORMED'
                  CALL SHOWIT(1)
                  SYSTEM1(45)=0.0D0
                  SYSTEM1(47)=0.0D0
                  RETURN
              ELSE
C       PUX(FINAL) IS ZERO, PROCEED
              END IF
C       NOW, THE CURRENT EXIT PUPIL RADIUS IS PX(FINAL)
C       THE DESIRED EXIT PUPIL RADIUS IS STORED IN SYSTEM1(47)
C       IF ALL SOLVED ARE SHUT OFF, THE NEW EXIT PUPIL RADIUS
C       IS REALIZED BY SCALING THE SAX VALUE BY:
C
C               NEW DESIRED EXIT PUPIL RADIUS/OLD EXIT PUPIL RADIUS
              IF(DABS(PXTRAX(1,(INT(SYSTEM1(20))))).LT.1.0D-15) THEN
                  OUTLYNE= 'CURRENT EXIT PUPIL RADIUS IS ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE= '"ERX" ADJUSTMENT NOT DEFINED AND WILL BE REMOVED'
                  CALL SHOWIT(1)
                  OUTLYNE= 'NO ADJUSTMENT PERFORMED'
                  CALL SHOWIT(1)
                  SYSTEM1(45)=0.0D0
                  SYSTEM1(47)=0.0D0
                  RETURN
              ELSE
              END IF
C
              SYSTEM1(13)=SYSTEM1(13)*(SYSTEM1(46)/
     1        PXTRAX(1,(INT(SYSTEM1(20)))))
C
C       NOW WE PERFORM A PARAXIAL RAY TRACE WITHOUT AND SOLVES
C
C       TELECENTRIC STUFF, 11/12/2000
              IF(SYSTEM1(63).EQ.1.0D0) THEN
                  IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                      OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                      CALL SHOWIT(1)
                      OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                      CALL SHOWIT(1)
                      OUTLYNE='VALUES'
                      CALL SHOWIT(1)
                      OUTLYNE='PARAXIAL TRACE STOPPED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      IF(SYSTEM1(64).EQ.1.0D0) THEN
                          SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                          SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                      END IF
                      IF(SYSTEM1(67).EQ.1.0D0) THEN
                          SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                          SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                      END IF
                  END IF
              END IF
C
              CALL TR
C
C       NOW THE PARAXIAL SOLVE DATA ARRAYS ARE LOADED WITH THE CORRECT
C       SOLVELESS DATA. NOW RE-SET ALL SOLVE TARGET VALUES
C
C       WHEN THE RETURN OCCURS, THE RETURN WILL GO TO LNSEOS IN
C       THE CASE OF HLDS OR TO SFNB.FOR OR SER.FOR. LNSEOS.FOR
C       WILL AGAIN PERFORM THE TRACE FIXING THE EXIT PUPIL DISTACES AND
C       CALCULATING CHROMATIC AND OTHER VALUES.
C       A CALL TO PRTRA1 AND PRTRA2 IS PLACED IN SER.FOR AND SFNB.FOR
C       TO PERFORM A SIMILAR FUNCTION.
C       SVXSET RESETS ALL XZ-PLANE PARAXIAL SOLVES USING CURRENT XZ-PLANE
C       PARAXIAL DATA.
              CALL SVSET
              RETURN
          ELSE
C       ITYPEP NOT 2
          END IF
          RETURN
      END
C SUB INVAR.FOR
      SUBROUTINE INVAR
C
C       THIS SUBROUTINE CALCULATES AND DISPLAYS THE
C       OPTICAL INVARIANT OF THE OPTICAL SYSTEM IN THE
C       YZ AND XZ PLANES.
C       ALGORITHM DESCRIBED IN LENSSTORE.DOC
C       AND IN MATTHEW RIMMER'S THESIS U OF R 1963
C
          IMPLICIT NONE
C
          INTEGER CW,SF
C
          REAL*8 INVY,INVX
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"INVAR" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       HANDLE NO SURFACES
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO PARAXIAL DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       THE CONTROL WAVELENGTH NUMBER IS STORED IN
C       SYSTEM1(11)
C
C       CALCULATE THE OPTICAL INVARIANT
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INVY=((PXTRAY(5,SF)*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1)))
     1    -(PXTRAY(1,SF)*ALENS(CW,(SF-1))*PXTRAY(6,(SF-1))))
          INVX=((PXTRAX(5,SF)*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1)))
     1    -(PXTRAX(1,SF)*ALENS(CW,(SF-1))*PXTRAX(6,(SF-1))))
          WRITE(OUTLYNE,1500)INVY
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,2500)INVX
          CALL SHOWIT(0)
 1000     FORMAT(1X)
 1500     FORMAT('Y-Z PLANE LAGRANGE INVARIANT = ',G18.10)
 2500     FORMAT('X-Z PLANE LAGRANGE INVARIANT = ',G18.10)

          RETURN
      END
C SUB PCD3.FOR
      SUBROUTINE PCD3
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PCD3. THIS SUBROUTINE IMPLEMENTS
C       THE PCD3,XPCD3,SCD3,XSCD3
C       (THIRD ORDER ABERRATION CHROMATIC DIFFERENCE)
C       PRINTOUT AT THE CMD LEVEL
C
          INTEGER SF,CW,I
C
          REAL*8 C1,C2,C3,C4,C5,C1T,C2T,C3T,C4T,C5T
C
          REAL*8 INV
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          CALL PRTRD
C
C       COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'PCD3') THEN
                  WRITE(OUTLYNE,*)
     1            '"PCD3" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XPCD3') THEN
                  WRITE(OUTLYNE,*)
     1            '"XPCD3" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'PCD3') THEN
                  WRITE(OUTLYNE,*)
     1            '"PCD3" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XPCD3') THEN
                  WRITE(OUTLYNE,*)
     1            '"XPCD3" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
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
              WRITE(OUTLYNE,*)'NO PARAXIAL OR THIRD ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'PCD3'.OR.WC.EQ.'SCD3')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XPCD3'.OR.WC.EQ.'XSCD3')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'PCD3'.OR.WC.EQ.'SCD3')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XPCD3'.OR.WC.EQ.'XSCD3')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
          END IF
          IF(INV.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'PCD3'.OR.WC.EQ.'SCD3')WRITE(OUTLYNE,5001)
              IF(WC.EQ.'XPCD3'.OR.WC.EQ.'XSCD3')WRITE(OUTLYNE,6001)
              CALL SHOWIT(0)
              IF(WC.EQ.'PCD3'.OR.WC.EQ.'XPCD3')
     1        WRITE(OUTLYNE,5002) INT(F12)
              IF(WC.EQ.'SCD3'.OR.WC.EQ.'XSCD3')
     1        WRITE(OUTLYNE,6002) INT(F12)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'PCD3') THEN
                          C1=PDF3(1,I)/INV
                          C2=3.0D0*PDF3(2,I)/INV
                          C3=PDF3(3,I)/INV
                          C4=PDF3(4,I)/INV
                          C5=PDF3(5,I)/INV
                      END IF
                      IF(WC.EQ.'XPCD3') THEN
                          C1=XPDF3(1,I)/INV
                          C2=3.0D0*XPDF3(2,I)/INV
                          C3=XPDF3(3,I)/INV
                          C4=XPDF3(4,I)/INV
                          C5=XPDF3(5,I)/INV
                      END IF
                      IF(WC.EQ.'SCD3') THEN
                          C1=SDF3(1,I)/INV
                          C2=3.0D0*SDF3(2,I)/INV
                          C3=SDF3(3,I)/INV
                          C4=SDF3(4,I)/INV
                          C5=SDF3(5,I)/INV
                      END IF
                      IF(WC.EQ.'XSCD3') THEN
                          C1=XSDF3(1,I)/INV
                          C2=3.0D0*XSDF3(2,I)/INV
                          C3=XSDF3(3,I)/INV
                          C4=XSDF3(4,I)/INV
                          C5=XSDF3(5,I)/INV
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                      GO TO 10
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'PCD3') THEN
                          C1=PDF3(1,I)
                          C2=3.0D0*PDF3(2,I)
                          C3=PDF3(3,I)
                          C4=PDF3(4,I)
                          C5=PDF3(5,I)
                      END IF
                      IF(WC.EQ.'XPCD3') THEN
                          C1=XPDF3(1,I)
                          C2=3.0D0*XPDF3(2,I)
                          C3=XPDF3(3,I)
                          C4=XPDF3(4,I)
                          C5=XPDF3(5,I)
                      END IF
                      IF(WC.EQ.'SCD3') THEN
                          C1=SDF3(1,I)
                          C2=3.0D0*SDF3(2,I)
                          C3=SDF3(3,I)
                          C4=SDF3(4,I)
                          C5=SDF3(5,I)
                      END IF
                      IF(WC.EQ.'XSCD3') THEN
                          C1=XSDF3(1,I)
                          C2=3.0D0*XSDF3(2,I)
                          C3=XSDF3(3,I)
                          C4=XSDF3(4,I)
                          C5=XSDF3(5,I)
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'PCD3') THEN
                      C1T=C1T+PDF3(1,I)
                      C2T=C2T+(3.0D0*PDF3(2,I))
                      C3T=C3T+PDF3(3,I)
                      C4T=C4T+PDF3(4,I)
                      C5T=C5T+PDF3(5,I)
                  END IF
                  IF(WC.EQ.'XPCD3') THEN
                      C1T=C1T+XPDF3(1,I)
                      C2T=C2T+(3.0D0*XPDF3(2,I))
                      C3T=C3T+XPDF3(3,I)
                      C4T=C4T+XPDF3(4,I)
                      C5T=C5T+XPDF3(5,I)
                  END IF
                  IF(WC.EQ.'SCD3') THEN
                      C1T=C1T+SDF3(1,I)
                      C2T=C2T+(3.0D0*SDF3(2,I))
                      C3T=C3T+SDF3(3,I)
                      C4T=C4T+SDF3(4,I)
                      C5T=C5T+SDF3(5,I)
                  END IF
                  IF(WC.EQ.'XSCD3') THEN
                      C1T=C1T+XSDF3(1,I)
                      C2T=C2T+(3.0D0*XSDF3(2,I))
                      C3T=C3T+XSDF3(3,I)
                      C4T=C4T+XSDF3(4,I)
                      C5T=C5T+XSDF3(5,I)
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 25
              END IF
C       PRINT TOTALS WITH LABELING
 25           CONTINUE
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'PCD3') THEN
                      C1=PDF3(1,SF)/INV
                      C2=3.0D0*PDF3(2,SF)/INV
                      C3=PDF3(3,SF)/INV
                      C4=PDF3(4,SF)/INV
                      C5=PDF3(5,SF)/INV
                  END IF
                  IF(WC.EQ.'XPCD3') THEN
                      C1=XPDF3(1,SF)/INV
                      C2=3.0D0*XPDF3(2,SF)/INV
                      C3=XPDF3(3,SF)/INV
                      C4=XPDF3(4,SF)/INV
                      C5=XPDF3(5,SF)/INV
                  END IF
                  IF(WC.EQ.'SCD3') THEN
                      C1=SDF3(1,SF)/INV
                      C2=3.0D0*SDF3(2,SF)/INV
                      C3=SDF3(3,SF)/INV
                      C4=SDF3(4,SF)/INV
                      C5=SDF3(5,SF)/INV
                  END IF
                  IF(WC.EQ.'XSCD3') THEN
                      C1=XSDF3(1,SF)/INV
                      C2=3.0D0*XSDF3(2,SF)/INV
                      C3=XSDF3(3,SF)/INV
                      C4=XSDF3(4,SF)/INV
                      C5=XSDF3(5,SF)/INV
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'PCD3') THEN
                      C1=PDF3(1,SF)
                      C2=3.0D0*PDF3(2,SF)
                      C3=PDF3(3,SF)
                      C4=PDF3(4,SF)
                      C5=PDF3(5,SF)
                  END IF
                  IF(WC.EQ.'XPCD3') THEN
                      C1=XPDF3(1,SF)
                      C2=3.0D0*XPDF3(2,SF)
                      C3=XPDF3(3,SF)
                      C4=XPDF3(4,SF)
                      C5=XPDF3(5,SF)
                  END IF
                  IF(WC.EQ.'SCD3') THEN
                      C1=SDF3(1,SF)
                      C2=3.0D0*SDF3(2,SF)
                      C3=SDF3(3,SF)
                      C4=SDF3(4,SF)
                      C5=SDF3(5,SF)
                  END IF
                  IF(WC.EQ.'XSCD3') THEN
                      C1=XSDF3(1,SF)
                      C2=3.0D0*XSDF3(2,SF)
                      C3=XSDF3(3,SF)
                      C4=XSDF3(4,SF)
                      C5=XSDF3(5,SF)
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'PCD3') THEN
                      C1T=C1T+PDF3(1,I)
                      C2T=C2T+(3.0D0*PDF3(2,I))
                      C3T=C3T+PDF3(3,I)
                      C4T=C4T+PDF3(4,I)
                      C5T=C5T+PDF3(5,I)
                  END IF
                  IF(WC.EQ.'XPCD3') THEN
                      C1T=C1T+XPDF3(1,I)
                      C2T=C2T+(3.0D0*XPDF3(2,I))
                      C3T=C3T+XPDF3(3,I)
                      C4T=C4T+XPDF3(4,I)
                      C5T=C5T+XPDF3(5,I)
                  END IF
                  IF(WC.EQ.'SCD3') THEN
                      C1T=C1T+SDF3(1,I)
                      C2T=C2T+(3.0D0*SDF3(2,I))
                      C3T=C3T+SDF3(3,I)
                      C4T=C4T+SDF3(4,I)
                      C5T=C5T+SDF3(5,I)
                  END IF
                  IF(WC.EQ.'XSCD3') THEN
                      C1T=C1T+XSDF3(1,I)
                      C2T=C2T+(3.0D0*XSDF3(2,I))
                      C3T=C3T+XSDF3(3,I)
                      C4T=C4T+XSDF3(4,I)
                      C5T=C5T+XSDF3(5,I)
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 250
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'PCD3') THEN
                      C1=PDF3(1,I)/INV
                      C2=3.0D0*PDF3(2,I)/INV
                      C3=PDF3(3,I)/INV
                      C4=PDF3(4,I)/INV
                      C5=PDF3(5,I)/INV
                  END IF
                  IF(WC.EQ.'XPCD3') THEN
                      C1=XPDF3(1,I)/INV
                      C2=3.0D0*XPDF3(2,I)/INV
                      C3=XPDF3(3,I)/INV
                      C4=XPDF3(4,I)/INV
                      C5=XPDF3(5,I)/INV
                  END IF
                  IF(WC.EQ.'SCD3') THEN
                      C1=SDF3(1,I)/INV
                      C2=3.0D0*SDF3(2,I)/INV
                      C3=SDF3(3,I)/INV
                      C4=SDF3(4,I)/INV
                      C5=SDF3(5,I)/INV
                  END IF
                  IF(WC.EQ.'XSCD3') THEN
                      C1=XSDF3(1,I)/INV
                      C2=3.0D0*XSDF3(2,I)/INV
                      C3=XSDF3(3,I)/INV
                      C4=XSDF3(4,I)/INV
                      C5=XSDF3(5,I)/INV
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'PCD3') THEN
                      C1=PDF3(1,I)
                      C2=3.0D0*PDF3(2,I)
                      C3=PDF3(3,I)
                      C4=PDF3(4,I)
                      C5=PDF3(5,I)
                  END IF
                  IF(WC.EQ.'XPCD3') THEN
                      C1=XPDF3(1,I)
                      C2=3.0D0*XPDF3(2,I)
                      C3=XPDF3(3,I)
                      C4=XPDF3(4,I)
                      C5=XPDF3(5,I)
                  END IF
                  IF(WC.EQ.'SCD3') THEN
                      C1=SDF3(1,I)
                      C2=3.0D0*SDF3(2,I)
                      C3=SDF3(3,I)
                      C4=SDF3(4,I)
                      C5=SDF3(5,I)
                  END IF
                  IF(WC.EQ.'XSCD3') THEN
                      C1=XSDF3(1,I)
                      C2=3.0D0*XSDF3(2,I)
                      C3=XSDF3(3,I)
                      C4=XSDF3(4,I)
                      C5=XSDF3(5,I)
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
!     1  2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
     1    2X,G12.5)
 2001     FORMAT(6X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'SA3 ',10X,'CMA3',10X,'AST3',
     1    10X,'DIS3',10X,'PTZ3')
 5001     FORMAT(
     1    '(Y-Z) PLANE, THIRD ORDER')
 6001     FORMAT(
     1    '(X-Z) PLANE, THIRD ORDER')
 5002     FORMAT('PRIMARY CHROMATIC DIFFERENCES'
     1    ,' - (CFG #',I2,')')
 6002     FORMAT('SECONDARY CHROMATIC DIFFERENCES'
     1    ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB PCD5.FOR
      SUBROUTINE PCD5
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PCD5. THIS SUBROUTINE IMPLEMENTS
C       THE PCD5,XPCD5,SCD5,XSCD5 CMD LEVEL COMMAND

          INTEGER SF,CW,I
C
          REAL*8 C1,C2,C3,C4,C5,C1T,C2T,C3T,C4T,C5T
C
          REAL*8 INV
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          CALL PRTRD
C
C       COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'PCD5') THEN
                  WRITE(OUTLYNE,*)
     1            '"PCD5" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XPCD5') THEN
                  WRITE(OUTLYNE,*)
     1            '"XPCD5" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'PCD5') THEN
                  WRITE(OUTLYNE,*)
     1            '"PCD5" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XPCD5') THEN
                  WRITE(OUTLYNE,*)
     1            '"XPCD5" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              RETURN
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO PARAXIAL OR FIFTH ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'PCD5'.OR.WC.EQ.'SCD5')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XPCD5'.OR.WC.EQ.'XSCD5')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'PCD5'.OR.WC.EQ.'SCD5')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XPCD5'.OR.WC.EQ.'XSCD5')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
          END IF
          IF(INV.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'PCD5'.OR.WC.EQ.'SCD5')WRITE(OUTLYNE,5001)
              IF(WC.EQ.'XPCD5'.OR.WC.EQ.'XSCD5')WRITE(OUTLYNE,6001)
              CALL SHOWIT(0)
              IF(WC.EQ.'PCD5'.OR.WC.EQ.'XPCD5')
     1        WRITE(OUTLYNE,5002) INT(F12)
              IF(WC.EQ.'SCD5'.OR.WC.EQ.'XSCD5')
     1        WRITE(OUTLYNE,6002) INT(F12)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'PCD5') THEN
                          C1=PDF57(1,I)/INV
                          C2=(PDF57(2,I)+PDF57(3,I))/INV
                          C3=PDF57(10,I)/INV
                          C4=PDF57(12,I)/INV
                          C5=PDF57(11,I)/INV
                      END IF
                      IF(WC.EQ.'XPCD5') THEN
                          C1=XPDF57(1,I)/INV
                          C2=(XPDF57(2,I)+XPDF57(3,I))/INV
                          C3=XPDF57(10,I)/INV
                          C4=XPDF57(12,I)/INV
                          C5=XPDF57(11,I)/INV
                      END IF
                      IF(WC.EQ.'SCD5') THEN
                          C1=SDF57(1,I)/INV
                          C2=(SDF57(2,I)+SDF57(3,I))/INV
                          C3=SDF57(10,I)/INV
                          C4=SDF57(12,I)/INV
                          C5=SDF57(11,I)/INV
                      END IF
                      IF(WC.EQ.'XSCD5') THEN
                          C1=XSDF57(1,I)/INV
                          C2=(XSDF57(2,I)+XSDF57(3,I))/INV
                          C3=XSDF57(10,I)/INV
                          C4=XSDF57(12,I)/INV
                          C5=XSDF57(11,I)/INV
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                      GO TO 10
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'PCD5') THEN
                          C1=PDF57(1,I)
                          C2=PDF57(2,I)+PDF57(3,I)
                          C3=PDF57(10,I)
                          C4=PDF57(12,I)
                          C5=PDF57(11,I)
                      END IF
                      IF(WC.EQ.'XPCD5') THEN
                          C1=XPDF57(1,I)
                          C2=XPDF57(2,I)+XPDF57(3,I)
                          C3=XPDF57(10,I)
                          C4=XPDF57(12,I)
                          C5=XPDF57(11,I)
                      END IF
                      IF(WC.EQ.'SCD5') THEN
                          C1=SDF57(1,I)
                          C2=SDF57(2,I)+SDF57(3,I)
                          C3=SDF57(10,I)
                          C4=SDF57(12,I)
                          C5=SDF57(11,I)
                      END IF
                      IF(WC.EQ.'XSCD5') THEN
                          C1=XSDF57(1,I)
                          C2=XSDF57(2,I)+XSDF57(3,I)
                          C3=XSDF57(10,I)
                          C4=XSDF57(12,I)
                          C5=XSDF57(11,I)
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'PCD5') THEN
                      C1T=C1T+PDF57(1,I)
                      C2T=C2T+PDF57(2,I)+PDF57(3,I)
                      C3T=C3T+PDF57(10,I)
                      C4T=C4T+PDF57(12,I)
                      C5T=C5T+PDF57(11,I)
                  END IF
                  IF(WC.EQ.'XPCD5') THEN
                      C1T=C1T+XPDF57(1,I)
                      C2T=C2T+XPDF57(2,I)+XPDF57(3,I)
                      C3T=C3T+XPDF57(10,I)
                      C4T=C4T+XPDF57(12,I)
                      C5T=C5T+XPDF57(11,I)
                  END IF
                  IF(WC.EQ.'SCD5') THEN
                      C1T=C1T+SDF57(1,I)
                      C2T=C2T+SDF57(2,I)+SDF57(3,I)
                      C3T=C3T+SDF57(10,I)
                      C4T=C4T+SDF57(12,I)
                      C5T=C5T+SDF57(11,I)
                  END IF
                  IF(WC.EQ.'XSCD5') THEN
                      C1T=C1T+XSDF57(1,I)
                      C2T=C2T+XSDF57(2,I)+XSDF57(3,I)
                      C3T=C3T+XSDF57(10,I)
                      C4T=C4T+XSDF57(12,I)
                      C5T=C5T+XSDF57(11,I)
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 25
              END IF
C       PRINT TOTALS WITH LABELING
 25           CONTINUE
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'PCD5') THEN
                      C1=PDF57(1,SF)/INV
                      C2=(PDF57(2,SF)+PDF57(3,SF))/INV
                      C3=PDF57(10,SF)/INV
                      C4=PDF57(12,SF)/INV
                      C5=PDF57(11,SF)/INV
                  END IF
                  IF(WC.EQ.'XPCD5') THEN
                      C1=XPDF57(1,SF)/INV
                      C2=(XPDF57(2,SF)+XPDF57(3,SF))/INV
                      C3=XPDF57(10,SF)/INV
                      C4=XPDF57(12,SF)/INV
                      C5=XPDF57(11,SF)/INV
                  END IF
                  IF(WC.EQ.'SCD5') THEN
                      C1=SDF57(1,SF)/INV
                      C2=(SDF57(2,SF)+SDF57(3,SF))/INV
                      C3=SDF57(10,SF)/INV
                      C4=SDF57(12,SF)/INV
                      C5=SDF57(11,SF)/INV
                  END IF
                  IF(WC.EQ.'XSCD5') THEN
                      C1=XSDF57(1,SF)/INV
                      C2=(XSDF57(2,SF)+XSDF57(3,SF))/INV
                      C3=XSDF57(10,SF)/INV
                      C4=XSDF57(12,SF)/INV
                      C5=XSDF57(11,SF)/INV
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'PCD5') THEN
                      C1=PDF57(1,SF)
                      C2=PDF57(2,SF)+PDF57(3,SF)
                      C3=PDF57(10,SF)
                      C4=PDF57(12,SF)
                      C5=PDF57(11,SF)
                  END IF
                  IF(WC.EQ.'XPCD5') THEN
                      C1=XPDF57(1,SF)
                      C2=XPDF57(2,SF)+XPDF57(3,SF)
                      C3=XPDF57(10,SF)
                      C4=XPDF57(12,SF)
                      C5=XPDF57(11,SF)
                  END IF
                  IF(WC.EQ.'SCD5') THEN
                      C1=SDF57(1,SF)
                      C2=SDF57(2,SF)+SDF57(3,SF)
                      C3=SDF57(10,SF)
                      C4=SDF57(12,SF)
                      C5=SDF57(11,SF)
                  END IF
                  IF(WC.EQ.'XSCD5') THEN
                      C1=XSDF57(1,SF)
                      C2=XSDF57(2,SF)+XSDF57(3,SF)
                      C3=XSDF57(10,SF)
                      C4=XSDF57(12,SF)
                      C5=XSDF57(11,SF)
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'PCD5') THEN
                      C1T=C1T+PDF57(1,I)
                      C2T=C2T+PDF57(2,I)+PDF57(3,I)
                      C3T=C3T+PDF57(10,I)
                      C4T=C4T+PDF57(12,I)
                      C5T=C5T+PDF57(11,I)
                  END IF
                  IF(WC.EQ.'XPCD5') THEN
                      C1T=C1T+XPDF57(1,I)
                      C2T=C2T+XPDF57(2,I)+XPDF57(3,I)
                      C3T=C3T+XPDF57(10,I)
                      C4T=C4T+XPDF57(12,I)
                      C5T=C5T+XPDF57(11,I)
                  END IF
                  IF(WC.EQ.'SCD5') THEN
                      C1T=C1T+SDF57(1,I)
                      C2T=C2T+SDF57(2,I)+SDF57(3,I)
                      C3T=C3T+SDF57(10,I)
                      C4T=C4T+SDF57(12,I)
                      C5T=C5T+SDF57(11,I)
                  END IF
                  IF(WC.EQ.'XSCD5') THEN
                      C1T=C1T+XSDF57(1,I)
                      C2T=C2T+XSDF57(2,I)+XSDF57(3,I)
                      C3T=C3T+XSDF57(10,I)
                      C4T=C4T+XSDF57(12,I)
                      C5T=C5T+XSDF57(11,I)
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 250
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'PCD5') THEN
                      C1=PDF57(1,I)/INV
                      C2=(PDF57(2,I)+PDF57(3,I))/INV
                      C3=PDF57(10,I)/INV
                      C4=PDF57(12,I)/INV
                      C5=PDF57(11,I)/INV
                  END IF
                  IF(WC.EQ.'XPCD5') THEN
                      C1=XPDF57(1,I)/INV
                      C2=(XPDF57(2,I)+XPDF57(3,I))/INV
                      C3=XPDF57(10,I)/INV
                      C4=XPDF57(12,I)/INV
                      C5=XPDF57(11,I)/INV
                  END IF
                  IF(WC.EQ.'SCD5') THEN
                      C1=SDF57(1,I)/INV
                      C2=(SDF57(2,I)+SDF57(3,I))/INV
                      C3=SDF57(10,I)/INV
                      C4=SDF57(12,I)/INV
                      C5=PDF57(11,I)/INV
                  END IF
                  IF(WC.EQ.'XSCD5') THEN
                      C1=XSDF57(1,I)/INV
                      C2=(XSDF57(2,I)+XSDF57(3,I))/INV
                      C3=XSDF57(10,I)/INV
                      C4=XSDF57(12,I)/INV
                      C5=XSDF57(11,I)/INV
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'PCD5') THEN
                      C1=PDF57(1,I)
                      C2=PDF57(2,I)+PDF57(3,I)
                      C3=PDF57(10,I)
                      C4=PDF57(12,I)
                      C5=PDF57(11,I)
                  END IF
                  IF(WC.EQ.'XPCD5') THEN
                      C1=XPDF57(1,I)
                      C2=XPDF57(2,I)+XPDF57(3,I)
                      C3=XPDF57(10,I)
                      C4=XPDF57(12,I)
                      C5=XPDF57(11,I)
                  END IF
                  IF(WC.EQ.'SCD5') THEN
                      C1=SDF57(1,I)
                      C2=SDF57(2,I)+SDF57(3,I)
                      C3=SDF57(10,I)
                      C4=SDF57(12,I)
                      C5=SDF57(11,I)
                  END IF
                  IF(WC.EQ.'XSCD5') THEN
                      C1=XSDF57(1,I)
                      C2=XSDF57(2,I)+XSDF57(3,I)
                      C3=XSDF57(10,I)
                      C4=XSDF57(12,I)
                      C5=XSDF57(11,I)
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
!     1  2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
     1    2X,G12.5)
 2001     FORMAT(6X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'SA5 ',10X,'CMA5',10X,'AST5',
     1    10X,'DIS5',10X,'PTZ5')
 5001     FORMAT(
     1    '(Y-Z) PLANE, FIFTH ORDER')
 6001     FORMAT(
     1    '(X-Z) PLANE, FIFTH ORDER')
 5002     FORMAT('PRIMARY CHROMATIC DIFFERENCES'
     1    ,' - (CFG #',I2,')')
 6002     FORMAT('SECONDARY CHROMATIC DIFFERENCES'
     1    ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB PCDX5.FOR
      SUBROUTINE PCDX5
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PCDX5. THIS SUBROUTINE IMPLEMENTS
C       THE PCDX5,XPCDX5,SCDX5,XSCDX5 CMD LEVEL COMMAND

          INTEGER SF,CW,I
C
          REAL*8 C1,C2,C3,C4,C5,C1T,C2T,C3T,C4T,C5T
C
          REAL*8 INV
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          CALL PRTRD
C
C       COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'PCDX5') THEN
                  WRITE(OUTLYNE,*)
     1            '"PCDX5" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XPCDX5') THEN
                  WRITE(OUTLYNE,*)
     1            '"XPCDX5" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'PCDX5') THEN
                  WRITE(OUTLYNE,*)
     1            '"PCDX5" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XPCDX5') THEN
                  WRITE(OUTLYNE,*)
     1            '"XPCDX5" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
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
              WRITE(OUTLYNE,*)'NO PARAXIAL OR FIFTH ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'PCDX5'.OR.WC.EQ.'SCDX5')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XPCDX5'.OR.WC.EQ.'XSCDX5')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'PCDX5'.OR.WC.EQ.'SCDX5')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XPCDX5'.OR.WC.EQ.'XSCDX5')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
          END IF
          IF(INV.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'PCDX5'.OR.WC.EQ.'SCDX5')
     1        WRITE(OUTLYNE,5001)
              IF(WC.EQ.'XPCDX5'.OR.WC.EQ.'XSCDX5')
     1        WRITE(OUTLYNE,6001)
              CALL SHOWIT(0)
              IF(WC.EQ.'PCDX5'.OR.WC.EQ.'XPCDX5')
     1        WRITE(OUTLYNE,5002) INT(F12)
              IF(WC.EQ.'SCDX5'.OR.WC.EQ.'XSCDX5')
     1        WRITE(OUTLYNE,5002) INT(F12)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'PCDX5') THEN
                          C1=(PDF57(4,I)+PDF57(5,I)+PDF57(6,I))/INV
                          C2=PDF57(5,I)/INV
                          C3=(PDF57(7,I)+PDF57(8,I))/INV
                          C4=(PDF57(11,I)+(5.0*PDF57(10,I)))/INV
                          C5=(PDF57(11,I)+PDF57(10,I))/INV
                      END IF
                      IF(WC.EQ.'XPCDX5') THEN
                          C1=(XPDF57(4,I)+XPDF57(5,I)+XPDF57(6,I))/INV
                          C2=XPDF57(5,I)/INV
                          C3=(XPDF57(7,I)+XPDF57(8,I))/INV
                          C4=(XPDF57(11,I)+(5.0*XPDF57(10,I)))/INV
                          C5=(XPDF57(11,I)+XPDF57(10,I))/INV
                      END IF
                      IF(WC.EQ.'SCDX5') THEN
                          C1=(SDF57(4,I)+SDF57(5,I)+SDF57(6,I))/INV
                          C2=SDF57(5,I)/INV
                          C3=(SDF57(7,I)+SDF57(8,I))/INV
                          C4=(SDF57(11,I)+(5.0*SDF57(10,I)))/INV
                          C5=(SDF57(11,I)+SDF57(10,I))/INV
                      END IF
                      IF(WC.EQ.'XSCDX5') THEN
                          C1=(XSDF57(4,I)+XSDF57(5,I)+XSDF57(6,I))/INV
                          C2=XSDF57(5,I)/INV
                          C3=(XSDF57(7,I)+XSDF57(8,I))/INV
                          C4=(XSDF57(11,I)+(5.0*XSDF57(10,I)))/INV
                          C5=(XSDF57(11,I)+XSDF57(10,I))/INV
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                      GO TO 10
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'PCDX5') THEN
                          C1=PDF57(4,I)+PDF57(5,I)+PDF57(6,I)
                          C2=PDF57(5,I)
                          C3=PDF57(7,I)+PDF57(8,I)
                          C4=PDF57(11,I)+(PDF57(10,I)*5.0)
                          C5=PDF57(11,I)+PDF57(10,I)
                      END IF
                      IF(WC.EQ.'XPCDX5') THEN
                          C1=XPDF57(4,I)+XPDF57(5,I)+XPDF57(6,I)
                          C2=XPDF57(5,I)
                          C3=XPDF57(7,I)+XPDF57(8,I)
                          C4=XPDF57(11,I)+(XPDF57(10,I)*5.0)
                          C5=XPDF57(11,I)+XPDF57(10,I)
                      END IF
                      IF(WC.EQ.'SCDX5') THEN
                          C1=SDF57(4,I)+SDF57(5,I)+SDF57(6,I)
                          C2=SDF57(5,I)
                          C3=SDF57(7,I)+SDF57(8,I)
                          C4=SDF57(11,I)+(SDF57(10,I)*5.0)
                          C5=SDF57(11,I)+SDF57(10,I)
                      END IF
                      IF(WC.EQ.'XSCDX5') THEN
                          C1=XSDF57(4,I)+XSDF57(5,I)+XSDF57(6,I)
                          C2=XSDF57(5,I)
                          C3=XSDF57(7,I)+XSDF57(8,I)
                          C4=XSDF57(11,I)+(XSDF57(10,I)*5.0)
                          C5=XSDF57(11,I)+XSDF57(10,I)
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'PCDX5') THEN
                      C1T=C1T+PDF57(4,I)+PDF57(5,I)+PDF57(6,I)
                      C2T=C2T+PDF57(5,I)
                      C3T=C3T+PDF57(7,I)+PDF57(8,I)
                      C4T=C4T+PDF57(11,I)+(PDF57(10,I)*5.0)
                      C5T=C5T+PDF57(10,I)+PDF57(11,I)
                  END IF
                  IF(WC.EQ.'XPCDX5') THEN
                      C1T=C1T+XPDF57(4,I)+XPDF57(5,I)+XPDF57(6,I)
                      C2T=C2T+XPDF57(5,I)
                      C3T=C3T+XPDF57(7,I)+XPDF57(8,I)
                      C4T=C4T+XPDF57(11,I)+(XPDF57(10,I)*5.0)
                      C5T=C5T+XPDF57(10,I)+XPDF57(11,I)
                  END IF
                  IF(WC.EQ.'SCDX5') THEN
                      C1T=C1T+SDF57(4,I)+SDF57(5,I)+SDF57(6,I)
                      C2T=C2T+SDF57(5,I)
                      C3T=C3T+SDF57(7,I)+SDF57(8,I)
                      C4T=C4T+SDF57(11,I)+(SDF57(10,I)*5.0)
                      C5T=C5T+SDF57(10,I)+SDF57(11,I)
                  END IF
                  IF(WC.EQ.'XSCDX5') THEN
                      C1T=C1T+XSDF57(4,I)+XSDF57(5,I)+XSDF57(6,I)
                      C2T=C2T+XSDF57(5,I)
                      C3T=C3T+XSDF57(7,I)+XSDF57(8,I)
                      C4T=C4T+XSDF57(11,I)+(XSDF57(10,I)*5.0)
                      C5T=C5T+XSDF57(10,I)+XSDF57(11,I)
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 25
C       NO CONVERSION REQUIRED
              END IF
C       PRINT TOTALS WITH LABELING
 25           CONTINUE
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
C       QUALIFIER NOT "ALL"
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'PCDX5') THEN
                      C1=(PDF57(4,SF)+PDF57(5,SF)+PDF57(6,SF))/INV
                      C2=(PDF57(5,SF))/INV
                      C3=(PDF57(7,SF)+PDF57(8,SF))/INV
                      C4=(PDF57(10,SF)+(5.0*PDF57(11,SF)))/INV
                      C5=(PDF57(10,SF)+PDF57(11,SF))/INV
                  END IF
                  IF(WC.EQ.'XPCDX5') THEN
                      C1=(XPDF57(4,SF)+XPDF57(5,SF)+XPDF57(6,SF))/INV
                      C2=(XPDF57(5,SF))/INV
                      C3=(XPDF57(7,SF)+XPDF57(8,SF))/INV
                      C4=(XPDF57(10,SF)+(5.0*XPDF57(11,SF)))/INV
                      C5=(XPDF57(10,SF)+XPDF57(11,SF))/INV
                  END IF
                  IF(WC.EQ.'SCDX5') THEN
                      C1=(SDF57(4,SF)+SDF57(5,SF)+SDF57(6,SF))/INV
                      C2=(SDF57(5,SF))/INV
                      C3=(SDF57(7,SF)+SDF57(8,SF))/INV
                      C4=(SDF57(10,SF)+(5.0*SDF57(11,SF)))/INV
                      C5=(SDF57(10,SF)+SDF57(11,SF))/INV
                  END IF
                  IF(WC.EQ.'XSCDX5') THEN
                      C1=(XSDF57(4,SF)+XSDF57(5,SF)+XSDF57(6,SF))/INV
                      C2=(XSDF57(5,SF))/INV
                      C3=(XSDF57(7,SF)+XSDF57(8,SF))/INV
                      C4=(XSDF57(10,SF)+(5.0*XSDF57(11,SF)))/INV
                      C5=(XSDF57(10,SF)+XSDF57(11,SF))/INV
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'PCDX5') THEN
                      C1=PDF57(4,SF)+PDF57(5,SF)+PDF57(6,SF)
                      C2=PDF57(5,SF)
                      C3=PDF57(7,SF)+PDF57(8,SF)
                      C4=PDF57(10,SF)+(5.0*PDF57(11,SF))
                      C5=PDF57(10,SF)+PDF57(11,SF)
                  END IF
                  IF(WC.EQ.'XPCDX5') THEN
                      C1=XPDF57(4,SF)+XPDF57(5,SF)+XPDF57(6,SF)
                      C2=XPDF57(5,SF)
                      C3=XPDF57(7,SF)+XPDF57(8,SF)
                      C4=XPDF57(10,SF)+(5.0*XPDF57(11,SF))
                      C5=XPDF57(10,SF)+XPDF57(11,SF)
                  END IF
                  IF(WC.EQ.'SCDX5') THEN
                      C1=SDF57(4,SF)+SDF57(5,SF)+SDF57(6,SF)
                      C2=SDF57(5,SF)
                      C3=SDF57(7,SF)+SDF57(8,SF)
                      C4=SDF57(10,SF)+(5.0*SDF57(11,SF))
                      C5=SDF57(10,SF)+SDF57(11,SF)
                  END IF
                  IF(WC.EQ.'XSCDX5') THEN
                      C1=XSDF57(4,SF)+XSDF57(5,SF)+XSDF57(6,SF)
                      C2=XSDF57(5,SF)
                      C3=XSDF57(7,SF)+XSDF57(8,SF)
                      C4=XSDF57(10,SF)+(5.0*XSDF57(11,SF))
                      C5=XSDF57(10,SF)+XSDF57(11,SF)
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'PCDX5') THEN
                      C1T=C1T+PDF57(4,I)+PDF57(5,I)+PDF57(6,I)
                      C2T=C2T+PDF57(5,I)
                      C3T=C3T+PDF57(7,I)+PDF57(8,I)
                      C4T=C4T+PDF57(10,I)+(5.0*PDF57(11,I))
                      C5T=C5T+PDF57(10,I)+PDF57(11,I)
                  END IF
                  IF(WC.EQ.'XPCDX5') THEN
                      C1T=C1T+XPDF57(4,I)+XPDF57(5,I)+XPDF57(6,I)
                      C2T=C2T+XPDF57(5,I)
                      C3T=C3T+XPDF57(7,I)+XPDF57(8,I)
                      C4T=C4T+XPDF57(10,I)+(5.0*XPDF57(11,I))
                      C5T=C5T+XPDF57(10,I)+XPDF57(11,I)
                  END IF
                  IF(WC.EQ.'SCDX5') THEN
                      C1T=C1T+SDF57(4,I)+SDF57(5,I)+SDF57(6,I)
                      C2T=C2T+SDF57(5,I)
                      C3T=C3T+SDF57(7,I)+SDF57(8,I)
                      C4T=C4T+SDF57(10,I)+(5.0*SDF57(11,I))
                      C5T=C5T+SDF57(10,I)+SDF57(11,I)
                  END IF
                  IF(WC.EQ.'XSCDX5') THEN
                      C1T=C1T+XSDF57(4,I)+XSDF57(5,I)+XSDF57(6,I)
                      C2T=C2T+XSDF57(5,I)
                      C3T=C3T+XSDF57(7,I)+XSDF57(8,I)
                      C4T=C4T+XSDF57(10,I)+(5.0*XSDF57(11,I))
                      C5T=C5T+XSDF57(10,I)+XSDF57(11,I)
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 250
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
C       NOT IMAGE SURFACE
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'PCDX5') THEN
                      C1=(PDF57(4,I)+PDF57(5,I)+PDF57(6,I))/INV
                      C2=PDF57(5,I)/INV
                      C3=(PDF57(7,I)+PDF57(8,I))/INV
                      C4=(PDF57(10,I)+(PDF57(11,I)*5.0))/INV
                      C5=(PDF57(10,I)+PDF57(11,I))/INV
                  END IF
                  IF(WC.EQ.'XPCDX5') THEN
                      C1=(XPDF57(4,I)+XPDF57(5,I)+XPDF57(6,I))/INV
                      C2=XPDF57(5,I)/INV
                      C3=(XPDF57(7,I)+XPDF57(8,I))/INV
                      C4=(XPDF57(10,I)+(XPDF57(11,I)*5.0))/INV
                      C5=(XPDF57(10,I)+XPDF57(11,I))/INV
                  END IF
                  IF(WC.EQ.'SCDX5') THEN
                      C1=(SDF57(4,I)+SDF57(5,I)+SDF57(6,I))/INV
                      C2=SDF57(5,I)/INV
                      C3=(SDF57(7,I)+SDF57(8,I))/INV
                      C4=(SDF57(10,I)+(SDF57(11,I)*5.0))/INV
                      C5=(SDF57(10,I)+SDF57(11,I))/INV
                  END IF
                  IF(WC.EQ.'XSCDX5') THEN
                      C1=(XSDF57(4,I)+XSDF57(5,I)+XSDF57(6,I))/INV
                      C2=XSDF57(5,I)/INV
                      C3=(XSDF57(7,I)+XSDF57(8,I))/INV
                      C4=(XSDF57(10,I)+(XSDF57(11,I)*5.0))/INV
                      C5=(XSDF57(10,I)+XSDF57(11,I))/INV
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'PCDX5') THEN
                      C1=PDF57(4,I)+PDF57(5,I)+PDF57(6,I)
                      C2=PDF57(5,I)
                      C3=PDF57(7,I)+PDF57(8,I)
                      C4=PDF57(10,I)+(5.0*PDF57(11,I))
                      C5=PDF57(10,I)+PDF57(11,I)
                  END IF
                  IF(WC.EQ.'XPCDX5') THEN
                      C1=XPDF57(4,I)+XPDF57(5,I)+XPDF57(6,I)
                      C2=XPDF57(5,I)
                      C3=XPDF57(7,I)+XPDF57(8,I)
                      C4=XPDF57(10,I)+(5.0*XPDF57(11,I))
                      C5=XPDF57(10,I)+XPDF57(11,I)
                  END IF
                  IF(WC.EQ.'SCDX5') THEN
                      C1=SDF57(4,I)+SDF57(5,I)+SDF57(6,I)
                      C2=SDF57(5,I)
                      C3=SDF57(7,I)+SDF57(8,I)
                      C4=SDF57(10,I)+(5.0*SDF57(11,I))
                      C5=SDF57(10,I)+SDF57(11,I)
                  END IF
                  IF(WC.EQ.'XSCDX5') THEN
                      C1=XSDF57(4,I)+XSDF57(5,I)+XSDF57(6,I)
                      C2=XSDF57(5,I)
                      C3=XSDF57(7,I)+XSDF57(8,I)
                      C4=XSDF57(10,I)+(5.0*XSDF57(11,I))
                      C5=XSDF57(10,I)+XSDF57(11,I)
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
!     1  2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
     1    2X,G12.5)
 2001     FORMAT(6X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'TOBSA',9X,'SOBSA',9X,'ELCMA',
     1    9X,'TAS ',10X,'SAS')
 5001     FORMAT(
     1    '(Y-Z) PLANE, FIFTH ORDER (EXTENDED)' )
 5002     FORMAT('PRIMARY CHROMATIC CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 6001     FORMAT(
     1    '(X-Z) PLANE, FIFTH ORDER (EXTENDED)' )
! 6002   FORMAT('SECONDARY CHROMATIC CONTRIBUTIONS'
!     1  ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB PCDP3.FOR
      SUBROUTINE PCDP3
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PCDP3. THIS SUBROUTINE IMPLEMENTS
C       THE PCDP3,XPCDP3,SCDP3 AND XSCDP3  CMD LEVEL COMMAND
C
          INTEGER SF,CW,I
C
          REAL*8 C1,C2,C3,C4,C5,C1T,C2T,C3T,C4T,C5T
C
          REAL*8 INV
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          CALL PRTRD
C
C       COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'PCDP3') THEN
                  WRITE(OUTLYNE,*)
     1            '"PCDP3" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XPCDP3') THEN
                  WRITE(OUTLYNE,*)
     1            '"XPCDP3" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'PCDP3') THEN
                  WRITE(OUTLYNE,*)
     1            '"PCDP3" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XPCDP3') THEN
                  WRITE(OUTLYNE,*)
     1            '"XPCDP3" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
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
              WRITE(OUTLYNE,*)'NO PARAXIAL OR THIRD ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'PCDP3'.OR.WC.EQ.'SCDP3')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XPCDP3'.OR.WC.EQ.'XSCDP3')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'PCDP3'.OR.WC.EQ.'SCDP3')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XPCDP3'.OR.WC.EQ.'XSCDP3')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
          END IF
          IF(INV.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'PCDP3'.OR.WC.EQ.'SCDP3')
     1        WRITE(OUTLYNE,5001)
              IF(WC.EQ.'XPCDP3'.OR.WC.EQ.'XSCDP3')
     1        WRITE(OUTLYNE,6001)
              CALL SHOWIT(0)
              IF(WC.EQ.'PCDP3'.OR.WC.EQ.'XPCDP3')
     1        WRITE(OUTLYNE,5002) INT(F12)
              IF(WC.EQ.'SCDP3'.OR.WC.EQ.'XSCDP3')
     1        WRITE(OUTLYNE,6002) INT(F12)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'PCDP3')THEN
                          C1=PDF3(6,I)/INV
                          C2=3.0D0*PDF3(7,I)/INV
                          C3=PDF3(8,I)/INV
                          C4=PDF3(9,I)/INV
                          C5=PDF3(10,I)/INV
                      END IF
                      IF(WC.EQ.'XPCDP3')THEN
                          C1=XPDF3(6,I)/INV
                          C2=3.0D0*XPDF3(7,I)/INV
                          C3=XPDF3(8,I)/INV
                          C4=XPDF3(9,I)/INV
                          C5=XPDF3(10,I)/INV
                      END IF
                      IF(WC.EQ.'SCDP3')THEN
                          C1=SDF3(6,I)/INV
                          C2=3.0D0*SDF3(7,I)/INV
                          C3=SDF3(8,I)/INV
                          C4=SDF3(9,I)/INV
                          C5=SDF3(10,I)/INV
                      END IF
                      IF(WC.EQ.'XSCDP3')THEN
                          C1=XSDF3(6,I)/INV
                          C2=3.0D0*XSDF3(7,I)/INV
                          C3=XSDF3(8,I)/INV
                          C4=XSDF3(9,I)/INV
                          C5=XSDF3(10,I)/INV
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                      GO TO 10
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'PCDP3')THEN
                          C1=PDF3(6,I)
                          C2=3.0D0*PDF3(7,I)
                          C3=PDF3(8,I)
                          C4=PDF3(9,I)
                          C5=PDF3(10,I)
                      END IF
                      IF(WC.EQ.'XPCDP3')THEN
                          C1=XPDF3(6,I)
                          C2=3.0D0*XPDF3(7,I)
                          C3=XPDF3(8,I)
                          C4=XPDF3(9,I)
                          C5=XPDF3(10,I)
                      END IF
                      IF(WC.EQ.'SCDP3')THEN
                          C1=SDF3(6,I)
                          C2=3.0D0*SDF3(7,I)
                          C3=SDF3(8,I)
                          C4=SDF3(9,I)
                          C5=SDF3(10,I)
                      END IF
                      IF(WC.EQ.'XSCDP3')THEN
                          C1=XSDF3(6,I)
                          C2=3.0D0*XSDF3(7,I)
                          C3=XSDF3(8,I)
                          C4=XSDF3(9,I)
                          C5=XSDF3(10,I)
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                      CALL SHOWIT(0)
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'PCDP3') THEN
                      C1T=C1T+PDF3(6,I)
                      C2T=C2T+(3.0D0*PDF3(7,I))
                      C3T=C3T+PDF3(8,I)
                      C4T=C4T+PDF3(9,I)
                      C5T=C5T+PDF3(10,I)
                  END IF
                  IF(WC.EQ.'XPCDP3') THEN
                      C1T=C1T+XPDF3(6,I)
                      C2T=C2T+(3.0D0*XPDF3(7,I))
                      C3T=C3T+XPDF3(8,I)
                      C4T=C4T+XPDF3(9,I)
                      C5T=C5T+XPDF3(10,I)
                  END IF
                  IF(WC.EQ.'SCDP3') THEN
                      C1T=C1T+SDF3(6,I)
                      C2T=C2T+(3.0D0*SDF3(7,I))
                      C3T=C3T+SDF3(8,I)
                      C4T=C4T+SDF3(9,I)
                      C5T=C5T+SDF3(10,I)
                  END IF
                  IF(WC.EQ.'XSCDP3') THEN
                      C1T=C1T+XSDF3(6,I)
                      C2T=C2T+(3.0D0*XSDF3(7,I))
                      C3T=C3T+XSDF3(8,I)
                      C4T=C4T+XSDF3(9,I)
                      C5T=C5T+XSDF3(10,I)
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 25
              END IF
C       PRINT TOTALS WITH LABELING
 25           CONTINUE
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'PCDP3') THEN
                      C1=PDF3(6,SF)/INV
                      C2=3.0D0*PDF3(7,SF)/INV
                      C3=PDF3(8,SF)/INV
                      C4=PDF3(9,SF)/INV
                      C5=PDF3(10,SF)/INV
                  END IF
                  IF(WC.EQ.'XPCDP3') THEN
                      C1=XPDF3(6,SF)/INV
                      C2=3.0D0*XPDF3(7,SF)/INV
                      C3=XPDF3(8,SF)/INV
                      C4=XPDF3(9,SF)/INV
                      C5=XPDF3(10,SF)/INV
                  END IF
                  IF(WC.EQ.'SCDP3') THEN
                      C1=SDF3(6,SF)/INV
                      C2=3.0D0*SDF3(7,SF)/INV
                      C3=SDF3(8,SF)/INV
                      C4=SDF3(9,SF)/INV
                      C5=SDF3(10,SF)/INV
                  END IF
                  IF(WC.EQ.'XSCDP3') THEN
                      C1=XSDF3(6,SF)/INV
                      C2=3.0D0*XSDF3(7,SF)/INV
                      C3=XSDF3(8,SF)/INV
                      C4=XSDF3(9,SF)/INV
                      C5=XSDF3(10,SF)/INV
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'PCDP3') THEN
                      C1=PDF3(6,SF)
                      C2=3.0D0*PDF3(7,SF)
                      C3=PDF3(8,SF)
                      C4=PDF3(9,SF)
                      C5=PDF3(10,SF)
                  END IF
                  IF(WC.EQ.'XPCDP3') THEN
                      C1=XPDF3(6,SF)
                      C2=3.0D0*XPDF3(7,SF)
                      C3=XPDF3(8,SF)
                      C4=XPDF3(9,SF)
                      C5=XPDF3(10,SF)
                  END IF
                  IF(WC.EQ.'SCDP3') THEN
                      C1=SDF3(6,SF)
                      C2=3.0D0*SDF3(7,SF)
                      C3=SDF3(8,SF)
                      C4=SDF3(9,SF)
                      C5=SDF3(10,SF)
                  END IF
                  IF(WC.EQ.'XSCDP3') THEN
                      C1=XSDF3(6,SF)
                      C2=3.0D0*XSDF3(7,SF)
                      C3=XSDF3(8,SF)
                      C4=XSDF3(9,SF)
                      C5=XSDF3(10,SF)
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              C4T=0.0
              C5T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'PCDP3') THEN
                      C1T=C1T+PDF3(6,I)
                      C2T=C2T+(3.0D0*PDF3(7,I))
                      C3T=C3T+PDF3(8,I)
                      C4T=C4T+PDF3(9,I)
                      C5T=C5T+PDF3(10,I)
                  END IF
                  IF(WC.EQ.'XPCDP3') THEN
                      C1T=C1T+XPDF3(6,I)
                      C2T=C2T+(3.0D0*XPDF3(7,I))
                      C3T=C3T+XPDF3(8,I)
                      C4T=C4T+XPDF3(9,I)
                      C5T=C5T+XPDF3(10,I)
                  END IF
                  IF(WC.EQ.'SCDP3') THEN
                      C1T=C1T+SDF3(6,I)
                      C2T=C2T+(3.0D0*SDF3(7,I))
                      C3T=C3T+SDF3(8,I)
                      C4T=C4T+SDF3(9,I)
                      C5T=C5T+SDF3(10,I)
                  END IF
                  IF(WC.EQ.'XSCDP3') THEN
                      C1T=C1T+XSDF3(6,I)
                      C2T=C2T+(3.0D0*XSDF3(7,I))
                      C3T=C3T+XSDF3(8,I)
                      C4T=C4T+XSDF3(9,I)
                      C5T=C5T+XSDF3(10,I)
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  C4T=C4T/INV
                  C5T=C5T/INV
                  GO TO 250
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
              C4=C4T
              C5=C5T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3,C4,C5
              CALL SHOWIT(0)
              RETURN
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'PCDP3') THEN
                      C1=PDF3(6,I)/INV
                      C2=3.0D0*PDF3(7,I)/INV
                      C3=PDF3(8,I)/INV
                      C4=PDF3(9,I)/INV
                      C5=PDF3(10,I)/INV
                  END IF
                  IF(WC.EQ.'XPCDP3') THEN
                      C1=XPDF3(6,I)/INV
                      C2=3.0D0*XPDF3(7,I)/INV
                      C3=XPDF3(8,I)/INV
                      C4=XPDF3(9,I)/INV
                      C5=XPDF3(10,I)/INV
                  END IF
                  IF(WC.EQ.'SCDP3') THEN
                      C1=SDF3(6,I)/INV
                      C2=3.0D0*SDF3(7,I)/INV
                      C3=SDF3(8,I)/INV
                      C4=SDF3(9,I)/INV
                      C5=SDF3(10,I)/INV
                  END IF
                  IF(WC.EQ.'XSCDP3') THEN
                      C1=XSDF3(6,I)/INV
                      C2=3.0D0*XSDF3(7,I)/INV
                      C3=XSDF3(8,I)/INV
                      C4=XSDF3(9,I)/INV
                      C5=XSDF3(10,I)/INV
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'PCDP3') THEN
                      C1=PDF3(6,I)
                      C2=3.0D0*PDF3(7,I)
                      C3=PDF3(8,I)
                      C4=PDF3(9,I)
                      C5=PDF3(10,I)
                  END IF
                  IF(WC.EQ.'XPCDP3') THEN
                      C1=XPDF3(6,I)
                      C2=3.0D0*XPDF3(7,I)
                      C3=XPDF3(8,I)
                      C4=XPDF3(9,I)
                      C5=XPDF3(10,I)
                  END IF
                  IF(WC.EQ.'SCDP3') THEN
                      C1=SDF3(6,I)
                      C2=3.0D0*SDF3(7,I)
                      C3=SDF3(8,I)
                      C4=SDF3(9,I)
                      C5=SDF3(10,I)
                  END IF
                  IF(WC.EQ.'XSCDP3') THEN
                      C1=XSDF3(6,I)
                      C2=3.0D0*XSDF3(7,I)
                      C3=XSDF3(8,I)
                      C4=XSDF3(9,I)
                      C5=XSDF3(10,I)
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3,C4,C5
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
!     1  2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,
     1    2X,G12.5)
 2001     FORMAT(6X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'PSA3 ',9X,'PCMA3',9X,'PAST3',
     1    9X,'PDIS3',9X,'PPTZ3')
 5001     FORMAT(
     1    '(Y-Z) PLANE, EXIT PUPIL THIRD ORDER ')
 6001     FORMAT(
     1    '(X-Z) PLANE, EXIT PUPIL THIRD ORDER ')
 5002     FORMAT('PRIMARY CHROMATIC CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 6002     FORMAT('SECONDARY CHROMATIC CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB PCDSA.FOR
      SUBROUTINE PCDSA
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PCDSA. THIS SUBROUTINE IMPLEMENTS
C       THE PCDSA,XPCDSA,SCDSA,XSCDSA CMD LEVEL COMMAND
C
          INTEGER SF,CW,I
C
          REAL*8 C1,C2,C3
C
          REAL*8 INV,C1T,C2T,C3T
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          CALL PRTRD
C
C       COMMAND ACCEPTS QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       "I","IM", AND "IMAGE"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'PCDSA') THEN
                  WRITE(OUTLYNE,*)
     1            '"PCDSA" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XPCDSA') THEN
                  WRITE(OUTLYNE,*)
     1            '"XPCDSA" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'PCDSA') THEN
                  WRITE(OUTLYNE,*)
     1            '"PCDSA" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'XPCDSA') THEN
                  WRITE(OUTLYNE,*)
     1            '"XPCDSA" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
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
              WRITE(OUTLYNE,*)'NO PARAXIAL,3RD, 5TH OR 7TH  ORDER DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          INV=1.0D0
          IF(SYSTEM1(30).EQ.1.0D0) THEN
C       MODE IS FOCAL
              IF(WC.EQ.'PCDSA'.OR.WC.EQ.'SCDSA')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
              IF(WC.EQ.'XPCDSA'.OR.WC.EQ.'XSCDSA')
     1        INV=-2.0*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1))
          END IF
          IF(SYSTEM1(30).EQ.3.0D0) THEN
C       MODE IS AFOCAL
              IF(WC.EQ.'PCDSA'.OR.WC.EQ.'SCDSA')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
              IF(WC.EQ.'XPCDSA'.OR.WC.EQ.'XSCDSA')
     1        INV= 2.0*ALENS(CW,(SF-1))*PXTRAX(1,SF)
          END IF
          IF(INV.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'ABERRATIONS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              IF(SYSTEM1(30).EQ.1.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
              IF(SYSTEM1(30).EQ.3.0D0)
     1        WRITE(OUTLYNE,*)'CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'THEN RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'PCDSA'.OR.WC.EQ.'SCDSA')
     1        WRITE(OUTLYNE,5001)
              IF(WC.EQ.'XPCDSA'.OR.WC.EQ.'XSCDSA')
     1        WRITE(OUTLYNE,6001)
              CALL SHOWIT(0)
              IF(WC.EQ.'PCDSA'.OR.WC.EQ.'XPCDSA')
     1        WRITE(OUTLYNE,5002) INT(F12)
              IF(WC.EQ.'SCDSA'.OR.WC.EQ.'XSCDSA')
     1        WRITE(OUTLYNE,6002) INT(F12)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.
     1                    SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                      IF(WC.EQ.'PCDSA') THEN
                          C1=PDF3(1,I)/INV
                          C2=PDF57(1,I)/INV
                          C3=PDF57(14,I)/INV
                      END IF
                      IF(WC.EQ.'XPCDSA') THEN
                          C1=XPDF3(1,I)/INV
                          C2=XPDF57(1,I)/INV
                          C3=XPDF57(14,I)/INV
                      END IF
                      IF(WC.EQ.'SCDSA') THEN
                          C1=SDF3(1,I)/INV
                          C2=SDF57(1,I)/INV
                          C3=SDF57(14,I)/INV
                      END IF
                      IF(WC.EQ.'XSCDSA') THEN
                          C1=XSDF3(1,I)/INV
                          C2=XSDF57(1,I)/INV
                          C3=XSDF57(14,I)/INV
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3
                      CALL SHOWIT(0)
                      GO TO 10
                  END IF
                  IF(SYSTEM1(30).EQ.2.0.OR.
     1                    SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'PCDSA') THEN
                          C1=PDF3(1,I)
                          C2=PDF57(1,I)
                          C3=PDF57(14,I)
                      END IF
                      IF(WC.EQ.'XPCDSA') THEN
                          C1=XPDF3(1,I)
                          C2=XPDF57(1,I)
                          C3=XPDF57(14,I)
                      END IF
                      IF(WC.EQ.'SCDSA') THEN
                          C1=SDF3(1,I)
                          C2=SDF57(1,I)
                          C3=SDF57(14,I)
                      END IF
                      IF(WC.EQ.'XSCDSA') THEN
                          C1=XSDF3(1,I)
                          C2=XSDF57(1,I)
                          C3=XSDF57(14,I)
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3
                      CALL SHOWIT(0)
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              DO 20 I=0,SF
                  IF(WC.EQ.'PCDSA') THEN
                      C1T=C1T+PDF3(1,I)
                      C2T=C2T+PDF57(1,I)
                      C3T=C3T+PDF57(14,I)
                  END IF
                  IF(WC.EQ.'XPCDSA') THEN
                      C1T=C1T+XPDF3(1,I)
                      C2T=C2T+XPDF57(1,I)
                      C3T=C3T+XPDF57(14,I)
                  END IF
                  IF(WC.EQ.'SCDSA') THEN
                      C1T=C1T+SDF3(1,I)
                      C2T=C2T+SDF57(1,I)
                      C3T=C3T+SDF57(14,I)
                  END IF
                  IF(WC.EQ.'XSCDSA') THEN
                      C1T=C1T+XSDF3(1,I)
                      C2T=C2T+XSDF57(1,I)
                      C3T=C3T+XSDF57(14,I)
                  END IF
 20           CONTINUE
C       NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  GO TO 25
C       NO CONVERSION REQUIRED
              END IF
C       PRINT TOTALS WITH LABELING
 25           CONTINUE
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              C1=C1T
              C2=C2T
              C3=C3T
              WRITE(OUTLYNE,2001) C1,C2,C3
              CALL SHOWIT(0)
              RETURN
C       QUALIFIER NOT "ALL"
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     2    SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              IF(WQ.EQ.'OB'.OR.WQ.EQ.'OBJ') SF=0
              IF(WQ.EQ.'I'.OR.WQ.EQ.'IM'.OR.WQ.EQ.'IMAGE')
     1        SF=INT(SYSTEM1(20))
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'PCDSA') THEN
                      C1=PDF3(1,SF)/INV
                      C2=PDF57(1,SF)/INV
                      C3=PDF57(14,SF)/INV
                  END IF
                  IF(WC.EQ.'XPCDSA') THEN
                      C1=XPDF3(1,SF)/INV
                      C2=XPDF57(1,SF)/INV
                      C3=XPDF57(14,SF)/INV
                  END IF
                  IF(WC.EQ.'SCDSA') THEN
                      C1=SDF3(1,SF)/INV
                      C2=SDF57(1,SF)/INV
                      C3=SDF57(14,SF)/INV
                  END IF
                  IF(WC.EQ.'XSCDSA') THEN
                      C1=XSDF3(1,SF)/INV
                      C2=XSDF57(1,SF)/INV
                      C3=XSDF57(14,SF)/INV
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'PCDSA') THEN
                      C1=PDF3(1,SF)
                      C2=PDF57(1,SF)
                      C3=PDF57(14,SF)
                  END IF
                  IF(WC.EQ.'XPCDSA') THEN
                      C1=XPDF3(1,SF)
                      C2=XPDF57(1,SF)
                      C3=XPDF57(14,SF)
                  END IF
                  IF(WC.EQ.'SCDSA') THEN
                      C1=SDF3(1,SF)
                      C2=SDF57(1,SF)
                      C3=SDF57(14,SF)
                  END IF
                  IF(WC.EQ.'XSCDSA') THEN
                      C1=XSDF3(1,SF)
                      C2=XSDF57(1,SF)
                      C3=XSDF57(14,SF)
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)SF,C1,C2,C3
                  CALL SHOWIT(0)
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB'.OR.SQ.EQ.1.AND.
     2    WQ.NE.'I'.OR.SQ.EQ.1.AND.WQ.NE.'IM'.OR.SQ.EQ.1.AND.
     3    WQ.NE.'IMAGE') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1.OR.SQ.EQ.1.AND.WQ.EQ.'IM'.OR.
     1    SQ.EQ.1.AND.WQ.EQ.'I'.OR.SQ.EQ.1.AND.WQ.EQ.'IMAGE') THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT SYSTEM TOTALS
C
C       THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              C1T=0.0
              C2T=0.0
              C3T=0.0
              DO 220 I=0,SF
                  IF(WC.EQ.'PCDSA') THEN
                      C1T=C1T+PDF3(1,I)
                      C2T=C2T+PDF57(1,I)
                      C3T=C3T+PDF57(14,I)
                  END IF
                  IF(WC.EQ.'XPCDSA') THEN
                      C1T=C1T+XPDF3(1,I)
                      C2T=C2T+XPDF57(1,I)
                      C3T=C3T+XPDF57(14,I)
                  END IF
                  IF(WC.EQ.'SCDSA') THEN
                      C1T=C1T+SDF3(1,I)
                      C2T=C2T+SDF57(1,I)
                      C3T=C3T+SDF57(14,I)
                  END IF
                  IF(WC.EQ.'XSCDSA') THEN
                      C1T=C1T+XSDF3(1,I)
                      C2T=C2T+XSDF57(1,I)
                      C3T=C3T+XSDF57(14,I)
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.3.0) THEN
C       MODE IS FOCAL OR AFOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  C1T=C1T/INV
                  C2T=C2T/INV
                  C3T=C3T/INV
                  GO TO 250
              END IF
 250          CONTINUE
              C1=C1T
              C2=C2T
              C3=C3T
C       PRINT TOTALS WITH OUT LABELING
              IF(HEADIN) WRITE(OUTLYNE,5000)
              IF(HEADIN) CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) C1,C2,C3
              CALL SHOWIT(0)
              RETURN
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.
     1                SYSTEM1(30).EQ.3.0) THEN
C               FOCAL OR AFOCAL, CONVERT
                  IF(WC.EQ.'PCDSA') THEN
                      C1=PDF3(1,I)/INV
                      C2=PDF57(1,I)/INV
                      C3=PDF57(14,I)/INV
                  END IF
                  IF(WC.EQ.'XPCDSA') THEN
                      C1=XPDF3(1,I)/INV
                      C2=XPDF57(1,I)/INV
                      C3=XPDF57(14,I)/INV
                  END IF
                  IF(WC.EQ.'SCDSA') THEN
                      C1=SDF3(1,I)/INV
                      C2=SDF57(1,I)/INV
                      C3=SDF57(14,I)/INV
                  END IF
                  IF(WC.EQ.'XSCDSA') THEN
                      C1=XSDF3(1,I)/INV
                      C2=XSDF57(1,I)/INV
                      C3=XSDF57(14,I)/INV
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.2.0.OR.
     1                SYSTEM1(30).EQ.4.0) THEN
C               UFOCAL OR UAFOCAL, DON'T CONVERT
                  IF(WC.EQ.'PCDSA') THEN
                      C1=PDF3(1,I)
                      C2=PDF57(1,I)
                      C3=PDF57(14,I)
                  END IF
                  IF(WC.EQ.'XPCDSA') THEN
                      C1=XPDF3(1,I)
                      C2=XPDF57(1,I)
                      C3=XPDF57(14,I)
                  END IF
                  IF(WC.EQ.'SCDSA') THEN
                      C1=SDF3(1,I)
                      C2=SDF57(1,I)
                      C3=SDF57(14,I)
                  END IF
                  IF(WC.EQ.'XSCDSA') THEN
                      C1=XSDF3(1,I)
                      C2=XSDF57(1,I)
                      C3=XSDF57(14,I)
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000)I,C1,C2,C3
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
! 1500   FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5)
 2000     FORMAT(I3,2X,G12.5,2X,G12.5,2X,G12.5)
 2001     FORMAT(6X,G12.5,2X,G12.5,2X,G12.5)
 5000     FORMAT('SURF',5X,'SA3 ',10X,'SA5 ',10X,'SA7')
 5001     FORMAT(
     1    '(Y-Z) PLANE, SPHERICAL ABERRATION (3RD,5TH,7TH)')
 5002     FORMAT('PRIMARY CHROMATIC CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 6001     FORMAT(
     1    '(X-Z) PLANE, SPHERICAL ABERRATION (3RD,5TH,7TH)')
 6002     FORMAT('SECONDARY CHROMATIC CONTRIBUTIONS'
     1    ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
      END
C SUB CCOL.FOR
      SUBROUTINE CCOL
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE CALCULATES THE YZ PLANE (ITYPEP=1)
C       AND XY (ITYPEP=2)
C       CROMATIC ABERRATION COEFICIENTS AND LOADS THEM
C       INTO COLORY(1,0:MAXSUR) TO COLORY(8,0:MAXSUR)
C       THE TRANSVERSE COMPONENTS ARE
C       THE FIRST ENTRY IS   PACY/X
C       THE SECOND ENTRY IS  PLCY/X
C       THE THIRD ENTRY IS   SACY/X
C       THE THIRD ENTRY IS   SLCY/X
C
C       THE ANGULAR COMPONENTS ARE
C       THE FIFTH ENTRY IS   PACY/X
C       THE SIXTH ENTRY IS  PLCY/X
C       THE SEVENTH ENTRY IS   SAC/XY
C       THE EIGTH ENTRY IS   SLCY/X
C
C       ALGORITHM DESCRIBED IN LENSSTORE.DOC
C
          INTEGER  CW,PW1,PW2,SW1,SW2,ITYPEP,I
C
          COMMON/PTYPER/ITYPEP
C
          REAL*8 DN,DNP,PN,J_NP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(ITYPEP.EQ.1) THEN
C       CALCULATE AND LOAD PACY,PLCY,SACY,SLCY
C       THE CONTROL WAVELENGTH NUMBER IS STORED IN
C       SYSTEM1(11),THE PRIMARY WAVELENTH PAIR NUMBERS
C       ARE STORED IN SYSTEM1(7) AND SYSTEM1(8), AND THE SECONDARY
C       WAVELENGTH PAIRS ARE STORED IN SYSTEM1(9) AND SYSTEM1(10)
C
              IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5)
     1         CW= (INT(SYSTEM1(11))+45)
              IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10)
     1         CW= (INT(SYSTEM1(11))+65)
C
              IF(INT(SYSTEM1(7)).GE.1.AND.INT(SYSTEM1(7)).LE.5)
     1         PW1= (INT(SYSTEM1(7))+45)
              IF(INT(SYSTEM1(7)).GE.6.AND.INT(SYSTEM1(7)).LE.10)
     1         PW1= (INT(SYSTEM1(7))+65)
C
              IF(INT(SYSTEM1(8)).GE.1.AND.INT(SYSTEM1(8)).LE.5)
     1         PW2= (INT(SYSTEM1(8))+45)
              IF(INT(SYSTEM1(8)).GE.6.AND.INT(SYSTEM1(8)).LE.10)
     1         PW2= (INT(SYSTEM1(8))+65)
C
              IF(INT(SYSTEM1(9)).GE.1.AND.INT(SYSTEM1(9)).LE.5)
     1         SW1= (INT(SYSTEM1(9))+45)
              IF(INT(SYSTEM1(9)).GE.6.AND.INT(SYSTEM1(9)).LE.10)
     1         SW1= (INT(SYSTEM1(9))+65)
C
              IF(INT(SYSTEM1(10)).GE.1.AND.INT(SYSTEM1(10)).LE.5)
     1         SW2= (INT(SYSTEM1(10))+45)
              IF(INT(SYSTEM1(10)).GE.6.AND.INT(SYSTEM1(10)).LE.10)
     1         SW2= (INT(SYSTEM1(10))+65)
C
C       REFRACTIVE INDICES ARE IN ALENS(46,SURF) TO
C       ALENS(50,SURF)
C
C       INDICES ARE ADDRESSED IN THE FOLLOWING MANNER
C       INDEX AT CW IS ALENS(CW,I)
C               SW1 IS ALENS(SW1,I)
C               SW2 IS ALENS(SW2,I)
C               PW1 IS ALENS(PW1,I)
C               PW2 IS ALENS(PW2,I)
C
C       AT OBJECT SURFACE
              COLORY(1:8,0)=0.0D0
              DO I=1,INT(SYSTEM1(20))
C
C       PRIMARY AXIAL  AND LATERAL COLOR
C
C       TRANSVERSE COMPONENTS
C       SETUP
                  PN=ALENS(CW,(I-1))
                  J_NP=ALENS(CW,I)
                  DN=ALENS(PW1,(I-1))-ALENS(PW2,(I-1))
                  DNP=ALENS(PW1,I)-ALENS(PW2,I)
C       AXIAL COLOR (PRIMARY)
                  COLORY(1,I)=((PXTRAY(1,(INT(SYSTEM1(20))))*PXTRAY(5,I))-
     1            (PXTRAY(5,(INT(SYSTEM1(20))))*PXTRAY(1,I)))*PXTRAY(3,I)*PN*
     2            ((DN/PN)-(DNP/J_NP))
C       LATERAL COLOR (PRIMARY)
                  IF(PXTRAY(3,I).EQ.0.0D0) THEN
C       CASE OF NORMAL INCIDENCE
                      COLORY(2,I)=COLORY(1,I)
                  ELSE
                      COLORY(2,I)=COLORY(1,I)*(PXTRAY(7,I)/PXTRAY(3,I))
                  END IF
C
C       SETUP
                  DN=ALENS(SW1,(I-1))-ALENS(SW2,(I-1))
                  DNP=ALENS(SW1,I)-ALENS(SW2,I)
C       AXIAL  COLOR (SECONDARY)
                  COLORY(3,I)=((PXTRAY(1,(INT(SYSTEM1(20))))*PXTRAY(5,I))-
     1            (PXTRAY(5,(INT(SYSTEM1(20))))*PXTRAY(1,I)))*PXTRAY(3,I)*PN*
     2            ((DN/PN)-(DNP/J_NP))
C       LATERAL COLOR (SECONDARY)
                  IF(PXTRAY(3,I).EQ.0.0D0) THEN
C       CASE OF NORMAL INCIDENCE
                      COLORY(4,I)=COLORY(3,I)
                  ELSE
                      COLORY(4,I)=COLORY(3,I)*(PXTRAY(7,I)/PXTRAY(3,I))
                  END IF
C       ANGULAR COMPONENTS
C       SETUP
                  PN=ALENS(CW,(I-1))
                  J_NP=ALENS(CW,I)
                  DN=ALENS(PW1,(I-1))-ALENS(PW2,(I-1))
                  DNP=ALENS(PW1,I)-ALENS(PW2,I)
C       AXIAL COLOR (PRIMARY)
                  COLORY(5,I)=((PXTRAY(5,I)*PXTRAY(2,((INT(SYSTEM1(20)))-1)))-
     1            (PXTRAY(1,I)*PXTRAY(6,((INT(SYSTEM1(20)))-1))))*PXTRAY(3,I)*PN*
     2            ((DN/PN)-(DNP/J_NP))
C       LATERAL COLOR (PRIMARY)
                  IF(PXTRAY(3,I).EQ.0.0D0) THEN
C       CASE OF NORMAL INCIDENCE
                      COLORY(6,I)=COLORY(5,I)
                  ELSE
                      COLORY(6,I)=COLORY(5,I)*(PXTRAY(7,I)/PXTRAY(3,I))
                  END IF
C
C       SETUP
                  DN=ALENS(SW1,(I-1))-ALENS(SW2,(I-1))
                  DNP=ALENS(SW1,I)-ALENS(SW2,I)
C       AXIAL  COLOR (SECONDARY)
                  COLORY(7,I)=((PXTRAY(5,I)*PXTRAY(2,((INT(SYSTEM1(20)))-1)))-
     1            (PXTRAY(1,I)*PXTRAY(6,((INT(SYSTEM1(20)))-1))))*PXTRAY(3,I)*PN*
     2            ((DN/PN)-(DNP/J_NP))
C       LATERAL COLOR (SECONDARY)
                  IF(PXTRAY(3,I).EQ.0.0D0) THEN
C       CASE OF NORMAL INCIDENCE
                      COLORY(8,I)=COLORY(7,I)
                  ELSE
                      COLORY(8,I)=COLORY(7,I)*(PXTRAY(7,I)/PXTRAY(3,I))
                  END IF
C
              END DO
C
              RETURN
          ELSE
C       ITYPEP NOT 1
          END IF
          IF(ITYPEP.EQ.2) THEN
C
C       CALCULATE AND LOAD PACX,PLCX,SACX,SLCX
C       THE CONTROL WAVELENGTH NUMBER IS STORED IN
C       SYSTEM1(11),THE PRIMARY WAVELENTH PAIR NUMBERS
C       ARE STORED IN SYSTEM1(7) AND SYSTEM1(8), AND THE SECONDARY
C       WAVELENGTH PAIRS ARE STORED IN SYSTEM1(9) AND SYSTEM1(10)
C
              IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5)
     1         CW= (INT(SYSTEM1(11))+45)
              IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10)
     1         CW= (INT(SYSTEM1(11))+65)
C
              IF(INT(SYSTEM1(7)).GE.1.AND.INT(SYSTEM1(7)).LE.5)
     1         PW1= (INT(SYSTEM1(7))+45)
              IF(INT(SYSTEM1(7)).GE.6.AND.INT(SYSTEM1(7)).LE.10)
     1         PW1= (INT(SYSTEM1(7))+65)
C
              IF(INT(SYSTEM1(8)).GE.1.AND.INT(SYSTEM1(8)).LE.5)
     1         PW2= (INT(SYSTEM1(8))+45)
              IF(INT(SYSTEM1(8)).GE.6.AND.INT(SYSTEM1(8)).LE.10)
     1         PW2= (INT(SYSTEM1(8))+65)
C
              IF(INT(SYSTEM1(9)).GE.1.AND.INT(SYSTEM1(9)).LE.5)
     1         SW1= (INT(SYSTEM1(9))+45)
              IF(INT(SYSTEM1(9)).GE.6.AND.INT(SYSTEM1(9)).LE.10)
     1         SW1= (INT(SYSTEM1(9))+65)
C
              IF(INT(SYSTEM1(10)).GE.1.AND.INT(SYSTEM1(10)).LE.5)
     1         SW2= (INT(SYSTEM1(10))+45)
              IF(INT(SYSTEM1(10)).GE.6.AND.INT(SYSTEM1(10)).LE.10)
     1         SW2= (INT(SYSTEM1(10))+65)
C
C       REFRACTIVE INDICES ARE IN ALENS(46,SURF) TO
C       ALENS(50,SURF)
C
C       INDICES ARE ADDRESSED IN THE FOLLOWING MANNER
C       INDEX AT CW IS ALENS(CW,I)
C               SW1 IS ALENS(SW1,I)
C               SW2 IS ALENS(SW2,I)
C               PW1 IS ALENS(PW1,I)
C               PW2 IS ALENS(PW2,I)
C
C       AT THE OBJECT ALL IS 0.0
              COLORX(1:8,0)=0.0D0
              DO I=1,INT(SYSTEM1(20))
C
C       PRIMARY AXIAL  AND LATERAL COLOR
C
C       TRANSVERSE COMPONENTS
C       SETUP
                  PN=ALENS(CW,(I-1))
                  J_NP=ALENS(CW,I)
                  DN=ALENS(PW1,(I-1))-ALENS(PW2,(I-1))
                  DNP=ALENS(PW1,I)-ALENS(PW2,I)
C       AXIAL COLOR (PRIMARY)
                  COLORX(1,I)=((PXTRAX(1,(INT(SYSTEM1(20))))*PXTRAX(5,I))-
     1            (PXTRAX(5,(INT(SYSTEM1(20))))*PXTRAX(1,I)))*PXTRAX(3,I)*PN*
     2            ((DN/PN)-(DNP/J_NP))
C       LATERAL COLOR (PRIMARY)
                  IF(PXTRAX(3,I).EQ.0.0D0) THEN
C       CASE OF NORMAL INCIDENCE
                      COLORX(2,I)=COLORX(1,I)
                  ELSE
                      COLORX(2,I)=COLORX(1,I)*(PXTRAX(7,I)/PXTRAX(3,I))
                  END IF
C
C       SETUP
                  DN=ALENS(SW1,(I-1))-ALENS(SW2,(I-1))
                  DNP=ALENS(SW1,I)-ALENS(SW2,I)
C       AXIAL  COLOR (SECONDARY)
                  COLORX(3,I)=((PXTRAX(1,(INT(SYSTEM1(20))))*PXTRAX(5,I))-
     1            (PXTRAX(5,(INT(SYSTEM1(20))))*PXTRAX(1,I)))*PXTRAX(3,I)*PN*
     2            ((DN/PN)-(DNP/J_NP))
C       LATERAL COLOR (SECONDARY)
                  IF(PXTRAX(3,I).EQ.0.0D0) THEN
C       CASE OF NORMAL INCIDENCE
                      COLORX(4,I)=COLORX(3,I)
                  ELSE
                      COLORX(4,I)=COLORX(3,I)*(PXTRAX(7,I)/PXTRAX(3,I))
                  END IF
C       ANGULAR COMPONENTS
C       SETUP
                  PN=ALENS(CW,(I-1))
                  J_NP=ALENS(CW,I)
                  DN=ALENS(PW1,(I-1))-ALENS(PW2,(I-1))
                  DNP=ALENS(PW1,I)-ALENS(PW2,I)
C       AXIAL COLOR (PRIMARY)
                  COLORX(5,I)=((PXTRAX(5,I)*PXTRAX(2,((INT(SYSTEM1(20)))-1)))-
     1            (PXTRAX(1,I)*PXTRAX(6,((INT(SYSTEM1(20)))-1))))*PXTRAX(3,I)*PN*
     2            ((DN/PN)-(DNP/J_NP))
C       LATERAL COLOR (PRIMARY)
                  IF(PXTRAX(3,I).EQ.0.0D0) THEN
C       CASE OF NORMAL INCIDENCE
                      COLORX(6,I)=COLORX(5,I)
                  ELSE
                      COLORX(6,I)=COLORX(5,I)*(PXTRAX(7,I)/PXTRAX(3,I))
                  END IF
C
C       SETUP
                  DN=ALENS(SW1,(I-1))-ALENS(SW2,(I-1))
                  DNP=ALENS(SW1,I)-ALENS(SW2,I)
C       AXIAL  COLOR (SECONDARY)
                  COLORX(7,I)=((PXTRAX(5,I)*PXTRAX(2,((INT(SYSTEM1(20)))-1)))-
     1            (PXTRAX(1,I)*PXTRAX(6,((INT(SYSTEM1(20)))-1))))*PXTRAX(3,I)*PN*
     2            ((DN/PN)-(DNP/J_NP))
C       LATERAL COLOR (SECONDARY)
                  IF(PXTRAX(3,I).EQ.0.0D0) THEN
C       CASE OF NORMAL INCIDENCE
                      COLORX(8,I)=COLORX(7,I)
                  ELSE
                      COLORX(8,I)=COLORX(7,I)*(PXTRAX(7,I)/PXTRAX(3,I))
                  END IF
C
              END DO
C
              RETURN
          ELSE
C       ITYPEP NOT 2
          END IF
      END
C SUB FIRD.FOR
      SUBROUTINE FIRD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FIRD. THIS SUBROUTINE IMPLEMENTS
C       THE PARAXIAL RAY TRACE EFL,BFL,FFL PRINTOUT FOR THE YZ PLANE
C       AT THE CMD LEVEL. IF THE EFL,BFL AND FFL IN THE XZ PLANE
C       ARE NOT THE SAME AS THE VALUES FOR THE YZ PLANE, THE XZ VALUES
C       ARE PRINTED AS WELL.
C
C       FORM OF THE FIRD COMMAND IS:
C
C       FIRD,NW1 NW2
C       WHERE NW1 IS THE FIRST SURFACE CONSIDERED,
C       AND NW2 IS SECOND SURFACE CONSIDERED.
C
          CHARACTER UN*11
C
          INTEGER I,J
C
          REAL*8 NEWWAVE,OLDWAVE,
     4    EFLY,EFLX,BFLY,BFLX,FFLY,FFLX,LAMBDA
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE FIRD COMMAND ACCEPTS ONLY NW1 AND NW2 NUMERIC
C       INPUT . IT DOES NOT ACCEPT STING OR QUALIFIER
C       INPUT.
          IF(SYSTEM1(6).EQ.1.0) UN='INCHES     '
          IF(SYSTEM1(6).EQ.2.0) UN='CENTIMETERS'
          IF(SYSTEM1(6).EQ.3.0) UN='MILLIMETERS'
          IF(SYSTEM1(6).EQ.4.0) UN='METERS'
C
C
          IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"FIRD" ONLY TAKES NUMERIC WORD #1, #2'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'AND (RARELY) #3 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'"AS WELL AS QUALIFIER INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'QUIET') THEN
              WRITE(OUTLYNE,*)
     1        '"FIRD" ONLY TAKES "QUIET" AS AN OPTIONAL QUALIFIER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'"AND QUALIFIER INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       HANDLE DEFAULT VALUES FOR NW1 AND NW2
C
          IF(DF1.EQ.1) W1=1.0
          IF(W1.EQ.0.0) THEN
              W1=1.0
          END IF
          IF(DF2.EQ.1) W2=SYSTEM1(20)-1.0D0
          IF(W2.EQ.0.0) W2=SYSTEM1(20)
          IF(W1.LT.0.0D0.OR.W2.GT.SYSTEM1(20).OR.
     1    W2.LE.W1) THEN
              WRITE(OUTLYNE,*)'SURFACE NUMBER(S) BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C       HANDLE NO SURFACES
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO PARAXIAL DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF3.EQ.0) THEN
              IF(W3.EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*) 'REQUESTED WAVELENGTH IS ZERO'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) '"FIRD CALCULATION CAN NOT PROCEED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     EXPLICIT WAVELEGTH ENTERED
                  NEWWAVE=W3
                  OLDWAVE=SYSTEM1(INT(SYSTEM1(11)))
                  SYSTEM1(INT(SYSTEM1(11)))=NEWWAVE
                  F1=0
                  F6=1
                  F22=1
                  LNSTYP=1
                  CALL LNSEOS
              END IF
          END IF
C
C       CALCULATION PERFOMED USING PARAXIAL DATA FROM
C       CURRENT CONFIGURATION AT CURRENT CONTROL WAVELENGTH
C
C       YZ PLANE EFL,BFL,FFL CALCULATION
C
          I=INT(W1)-1
          J=INT(W2)
          IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J)))
     1    .NE.0.0D0) THEN
              EFLY=-(((PXTRAY(2,I)*PXTRAY(5,I+1))-(PXTRAY(1,I+1)*PXTRAY(6,I
     1        )))/((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
          ELSE
              EFLY=1.0D20
          END IF
          IF(((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J)))
     1    .NE.0.0D0) THEN
              EFLX=-(((PXTRAX(2,I)*PXTRAX(5,I+1))-(PXTRAX(1,I+1)*PXTRAX(6,I
     1        )))/((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J))))
          ELSE
              EFLX=1.0D20
          END IF
          IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J)))
     1    .NE.0.0D0) THEN
              BFLY=-(((PXTRAY(2,I)*PXTRAY(5,J))-(PXTRAY(6,I)*PXTRAY(1,J)))/
     1        ((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
          ELSE
              BFLY=1.0D20
          END IF
          IF(((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J)))
     1    .NE.0.0D0) THEN
              BFLX=-(((PXTRAX(2,I)*PXTRAX(5,J))-(PXTRAX(6,I)*PXTRAX(1,J)))/
     1        ((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J))))
          ELSE
              BFLX=1.0D20
          END IF
          IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J)))
     1    .NE.0.0D0) THEN
              FFLY=-(((PXTRAY(1,I+1)*PXTRAY(6,J))-(PXTRAY(2,J)*PXTRAY(5,I+1
     1        )))/((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
          ELSE
              FFLY=1.0D20
          END IF
          IF(((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J)))
     1    .NE.0.0D0) THEN
              FFLX=-(((PXTRAX(1,I+1)*PXTRAX(6,J))-(PXTRAX(2,J)*PXTRAX(5,I+1
     1        )))/((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J))))
          ELSE
              FFLX=1.0D20
          END IF
          PPY1=FFLY+EFLY
          PPY2=BFLY-EFLY
          PPX1=FFLX+EFLX
          PPX2=BFLX-EFLX
          IF(WQ.EQ.'QUIET') THEN
              GPREG(1)=EFLY
              GPREG(2)=BFLY
              GPREG(3)=FFLY
              GPREG(4)=PPY1
              GPREG(5)=PPY2
              GPREG(6)=EFLX
              GPREG(7)=BFLX
              GPREG(8)=FFLX
              GPREG(9)=PPX1
              GPREG(10)=PPX2
              IF(DF3.EQ.0) THEN
C     EXPLICIT WAVELEGTH ENTERED
                  NEWWAVE=OLDWAVE
                  SYSTEM1(INT(SYSTEM1(11)))=NEWWAVE
                  F1=0
                  F6=1
                  F22=1
                  LNSTYP=1
                  CALL LNSEOS
              END IF
              RETURN
          END IF
          IF((EFLY).EQ.(EFLX).AND.
     1    (BFLY).EQ.(BFLX).AND.
     2    (FFLY).EQ.(FFLX)) THEN
              GPREG(1)=EFLY
              GPREG(2)=BFLY
              GPREG(3)=FFLY
              GPREG(4)=PPY1
              GPREG(5)=PPY2
              GPREG(6)=EFLX
              GPREG(7)=BFLX
              GPREG(8)=FFLX
              GPREG(9)=PPX1
              GPREG(10)=PPX2
C       YZ AND XZ VALUES SAME
              WRITE(OUTLYNE,2000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) INT(W1),INT(W2)
              CALL SHOWIT(0)
              IF(SYSTEM1(11).GE.1.0D0.AND.SYSTEM1(11).LE.5.0D0)
     1        LAMBDA=SYSTEM1((INT(SYSTEM1(11))))
              IF(SYSTEM1(11).GE.6.0D0.AND.SYSTEM1(11).LE.10.0D0)
     1        LAMBDA=SYSTEM1((65+INT(SYSTEM1(11))))
              WRITE(OUTLYNE,2002) LAMBDA
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2003) F12
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,100) EFLY,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,200) BFLY,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,300) FFLY,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,400) PPY1,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,500) PPY2,UN
              CALL SHOWIT(0)
          ELSE
C       YZ AND XZ VALUES NOT SAME
              WRITE(OUTLYNE,2010)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) INT(W1),INT(W2)
              CALL SHOWIT(0)
              LAMBDA=SYSTEM1((INT(SYSTEM1(11))))
              WRITE(OUTLYNE,2002) LAMBDA
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2003) F12
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101) EFLY,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,201) BFLY,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,301) FFLY,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,401) PPY1,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,501) PPY2,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,102) EFLX,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,202) BFLX,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,302) FFLX,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,402) PPX1,UN
              CALL SHOWIT(0)
              WRITE(OUTLYNE,502) PPX2,UN
              CALL SHOWIT(0)
          END IF
C
 1000     FORMAT(1X)
 2000     FORMAT('EFL,BFL AND FFL FROM:')
 2010     FORMAT('EFL,BFL AND FFL (YZ AND XZ PLANES) FROM:')
 2001     FORMAT('SURFACE ',I2,' TO SURFACE ',I2)
 2002     FORMAT('FOR A WAVELENGTH OF ',G12.4,' MICROMETER')
 2003     FORMAT('DATA IS FOR CONFIGURATION ',I2)
  100     FORMAT('EFL = ',G18.10,1X,A11)
  200     FORMAT('BFL = ',G18.10,1X,A11)
  300     FORMAT('FFL = ',G18.10,1X,A11)
  400     FORMAT('PP1 = ',G18.10,1X,A11)
  500     FORMAT('PP2 = ',G18.10,1X,A11)
  101     FORMAT('EFL(YZ-PLANE) = ',G18.10,1X,A11)
  201     FORMAT('BFL(YZ-PLANE) = ',G18.10,1X,A11)
  301     FORMAT('FFL(YZ-PLANE) = ',G18.10,1X,A11)
  401     FORMAT('PP1(YZ-PLANE) = ',G18.10,1X,A11)
  501     FORMAT('PP2(YZ-PLANE) = ',G18.10,1X,A11)
  102     FORMAT('EFL(XZ-PLANE) = ',G18.10,1X,A11)
  202     FORMAT('BFL(XZ-PLANE) = ',G18.10,1X,A11)
  302     FORMAT('FFL(XZ-PLANE) = ',G18.10,1X,A11)
  402     FORMAT('PP1(XZ-PLANE) = ',G18.10,1X,A11)
  502     FORMAT('PP2(XZ-PLANE) = ',G18.10,1X,A11)
          IF(DF3.EQ.0) THEN
C     EXPLICIT WAVELEGTH ENTERED
              NEWWAVE=OLDWAVE
              SYSTEM1(INT(SYSTEM1(11)))=NEWWAVE
              F1=0
              F6=1
              F22=1
              LNSTYP=1
              CALL LNSEOS
          END IF
          RETURN
      END
C SUB FINIYZ.FOR
      SUBROUTINE FINIYZ
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FINIYZ. THIS IS THE
C       SUBROUTINE WHICH IMPLEMENTS THE PARAXIAL RAY TRACE.
C       AT A SOLVE SURFACE (K)
C
          INTEGER K,FINY,COMI
C
          COMMON/FINER/FINY
C
          COMMON/PIKCOM/COMI
C
          REAL*8 CURV
C
          INTEGER WWVN
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          COMI=FINY
          K=FINY
          IF(INT(SYSTEM1(11)).EQ.1) WWVN=46
          IF(INT(SYSTEM1(11)).EQ.2) WWVN=47
          IF(INT(SYSTEM1(11)).EQ.3) WWVN=48
          IF(INT(SYSTEM1(11)).EQ.4) WWVN=49
          IF(INT(SYSTEM1(11)).EQ.5) WWVN=50
          IF(INT(SYSTEM1(11)).EQ.6) WWVN=71
          IF(INT(SYSTEM1(11)).EQ.7) WWVN=72
          IF(INT(SYSTEM1(11)).EQ.8) WWVN=73
          IF(INT(SYSTEM1(11)).EQ.9) WWVN=74
          IF(INT(SYSTEM1(11)).EQ.10) WWVN=75
C
C               VALUES AT SURFACE K
C       CALL PIKRES FOR THE SURFACE K
C
          IF(ALENS(32,K).NE.0.0D0) CALL PIKRES
C
          PXTRAY(1,K)=PXTRAY(1,(K-1))+(ALENS(3,(K-1))*PXTRAY(2,(K-1)))
C
          IF(ALENS(23,K).EQ.2.0D0) THEN
              CURV=ALENS(24,K)
          ELSE
              IF(ALENS(1,K).EQ.0.0D0.AND.ALENS(43,K).NE.0.0D0) THEN
                  CURV=ALENS(43,K)/2.0D0
              ELSE
                  CURV=ALENS(1,K)
              END IF
          END IF
          PXTRAY(2,K)=-CURV*PXTRAY(1,K)*
     1    (((ALENS(WWVN,K))-
     2    (ALENS(WWVN,(K-1))))/
     3    (ALENS(WWVN,K)))+
     3    ((ALENS(WWVN,(K-1)))/
     5    (ALENS(WWVN,K)))*PXTRAY(2,(K-1))
          IF(GLANAM(K,2).EQ.'PERFECT      ')
     1    PXTRAY(2,K)=(-(1.0D0/ALENS(3,K))*PXTRAY(1,K))+PXTRAY(2,K-1)
          IF(GLANAM(K,2).EQ.'IDEAL        ')
     1    PXTRAY(2,K)=(-(1.0D0/ALENS(121,K))*PXTRAY(1,K))+PXTRAY(2,K-1)
C
          IF(ALENS(23,K).EQ.2.0D0) THEN
              CURV=ALENS(24,K)
          ELSE
              IF(ALENS(1,K).EQ.0.0D0.AND.ALENS(43,K).NE.0.0D0) THEN
                  CURV=ALENS(43,K)/2.0D0
              ELSE
                  CURV=ALENS(1,K)
              END IF
          END IF
          PXTRAY(3,K)=CURV*PXTRAY(1,K)+PXTRAY(2,(K-1))
C
          PXTRAY(4,K)=((ALENS((WWVN),(K-1)))/
     1    (ALENS((WWVN),K)))*PXTRAY(3,K)
C
          PXTRAY(5,K)=PXTRAY(5,(K-1))+(ALENS(3,(K-1))*PXTRAY(6,(K-1)))
C
          IF(ALENS(23,K).EQ.2.0D0) THEN
              CURV=ALENS(24,K)
          ELSE
              IF(ALENS(1,K).EQ.0.0D0.AND.ALENS(43,K).NE.0.0D0) THEN
                  CURV=ALENS(43,K)/2.0D0
              ELSE
                  CURV=ALENS(1,K)
              END IF
          END IF
          PXTRAY(6,K)=-CURV*PXTRAY(5,K)*
     1    (((ALENS(WWVN,K))-
     2    (ALENS(WWVN,(K-1))))/
     3    (ALENS(WWVN,K)))+
     4    ((ALENS(WWVN,(K-1)))/
     5    (ALENS(WWVN,K)))*PXTRAY(6,(K-1))
          IF(GLANAM(K,2).EQ.'PERFECT      ')
     1    PXTRAY(6,K)=(-(1.0D0/ALENS(3,K))*PXTRAY(5,K))+PXTRAY(6,K-1)
          IF(GLANAM(K,2).EQ.'IDEAL        ')
     1    PXTRAY(6,K)=(-(1.0D0/ALENS(121,K))*PXTRAY(5,K))+PXTRAY(6,K-1)
C
          IF(ALENS(23,K).EQ.2.0D0) THEN
              CURV=ALENS(24,K)
          ELSE
              IF(ALENS(1,K).EQ.0.0D0.AND.ALENS(43,K).NE.0.0D0) THEN
                  CURV=ALENS(43,K)/2.0D0
              ELSE
                  CURV=ALENS(1,K)
              END IF
          END IF
          PXTRAY(7,K)=(CURV*PXTRAY(5,K))+PXTRAY(6,(K-1))
C
          PXTRAY(8,K)=((ALENS((WWVN),(K-1)))/
     1    (ALENS((WWVN),K)))*PXTRAY(7,K)
          RETURN
      END
C SUB FINIXZ.FOR
      SUBROUTINE FINIXZ
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FINIXZ. THIS IS THE
C       SUBROUTINE WHICH IMPLEMENTS THE PARAXIAL RAY TRACE.
C       AT A SOLVE SURFACE (K)
C
          INTEGER K,FINY,COMI
C
          COMMON/FINER/FINY
C
          COMMON/PIKCOM/COMI
C
          REAL*8 CURV
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          K=FINY
          COMI=FINY
          IF(INT(SYSTEM1(11)).EQ.1) WWVN=46
          IF(INT(SYSTEM1(11)).EQ.2) WWVN=47
          IF(INT(SYSTEM1(11)).EQ.3) WWVN=48
          IF(INT(SYSTEM1(11)).EQ.4) WWVN=49
          IF(INT(SYSTEM1(11)).EQ.5) WWVN=50
          IF(INT(SYSTEM1(11)).EQ.6) WWVN=71
          IF(INT(SYSTEM1(11)).EQ.7) WWVN=72
          IF(INT(SYSTEM1(11)).EQ.8) WWVN=73
          IF(INT(SYSTEM1(11)).EQ.9) WWVN=74
          IF(INT(SYSTEM1(11)).EQ.10) WWVN=75
C
C               VALUES AT SURFACE K
C       CALL PIKRES FOR THE SURFACE K
C
          IF(ALENS(32,K).NE.0.0D0) CALL PIKRES
C
          PXTRAX(1,K)=PXTRAX(1,(K-1))+(ALENS(3,(K-1))*PXTRAX(2,(K-1)))
C
          IF(ALENS(23,K).EQ.1.0D0) THEN
              CURV=ALENS(24,K)
          ELSE
              IF(ALENS(1,K).EQ.0.0D0.AND.ALENS(43,K).NE.0.0D0) THEN
                  CURV=ALENS(43,K)/2.0D0
              ELSE
                  CURV=ALENS(1,K)
              END IF
          END IF
          PXTRAX(2,K)=-CURV*PXTRAX(1,K)*
     1    (((ALENS(WWVN,K))-
     2    (ALENS(WWVN,(K-1))))/
     3    (ALENS(WWVN,K)))+
     4    ((ALENS(WWVN,(K-1)))/
     5    (ALENS(WWVN,K)))*PXTRAX(2,(K-1))
          IF(GLANAM(K,2).EQ.'PERFECT      ')
     1    PXTRAY(2,K)=(-(1.0D0/ALENS(3,K))*PXTRAY(1,K))+PXTRAY(2,K-1)
          IF(GLANAM(K,2).EQ.'IDEAL        ')
     1    PXTRAY(2,K)=(-(1.0D0/ALENS(121,K))*PXTRAY(1,K))+PXTRAY(2,K-1)

          IF(ALENS(23,K).EQ.1.0D0) THEN
              CURV=ALENS(24,K)
          ELSE
              IF(ALENS(1,K).EQ.0.0D0.AND.ALENS(43,K).NE.0.0D0) THEN
                  CURV=ALENS(43,K)/2.0D0
              ELSE
                  CURV=ALENS(1,K)
              END IF
          END IF
          PXTRAX(3,K)=CURV*PXTRAX(1,K)+PXTRAX(2,(K-1))
C
          PXTRAX(4,K)=((ALENS((WWVN),(K-1)))/
     1    (ALENS((WWVN),K)))*PXTRAX(3,K)
C
          PXTRAX(5,K)=PXTRAX(5,(K-1))+(ALENS(3,(K-1))*PXTRAX(6,(K-1)))
C
          IF(ALENS(23,K).EQ.1.0D0) THEN
              CURV=ALENS(24,K)
          ELSE
              IF(ALENS(1,K).EQ.0.0D0.AND.ALENS(43,K).NE.0.0D0) THEN
                  CURV=ALENS(43,K)/2.0D0
              ELSE
                  CURV=ALENS(1,K)
              END IF
          END IF
          PXTRAX(6,K)=-CURV*PXTRAX(5,K)*
     1    (((ALENS(WWVN,K))-
     2    (ALENS(WWVN,(K-1))))/
     3    (ALENS(WWVN,K)))+
     4    ((ALENS(WWVN,(K-1)))/
     5    (ALENS(WWVN,K)))*PXTRAX(6,(K-1))
          IF(GLANAM(K,2).EQ.'PERFECT      ')
     1    PXTRAX(6,K)=(-(1.0D0/ALENS(3,K))*PXTRAX(5,K))+PXTRAX(6,K-1)
          IF(GLANAM(K,2).EQ.'IDEAL        ')
     1    PXTRAX(6,K)=(-(1.0D0/ALENS(121,K))*PXTRAX(5,K))+PXTRAX(6,K-1)
C
          IF(ALENS(23,K).EQ.1.0D0) THEN
              CURV=ALENS(24,K)
          ELSE
              IF(ALENS(1,K).EQ.0.0D0.AND.ALENS(43,K).NE.0.0D0) THEN
                  CURV=ALENS(43,K)/2.0D0
              ELSE
                  CURV=ALENS(1,K)
              END IF
          END IF
          PXTRAX(7,K)=(CURV*PXTRAX(5,K))+PXTRAX(6,(K-1))
C
          PXTRAX(8,K)=((ALENS((WWVN),(K-1)))/
     1    (ALENS((WWVN),K)))*PXTRAX(7,K)
C
          RETURN
      END
C SUB FCH.FOR
      SUBROUTINE FCH
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FCH. THIS SUBROUTINE IMPLEMENTS
C       THE PARAXIAL RAY TRACE FCHX AND FCHY COLOR PRINTOUT
C       FOR THE XZ AND YZ PLANE
C       AT THE CMD LEVEL.
C
          INTEGER SF,CW,I
C
          REAL*8 INV
     5    ,C1,C2,C3,C4,TPAC,TPLC,TSAC,TSLC
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          CALL PRTRB
C
C       THE FCHX AND FCHY COMMANDS ACCEPT QUALIFIER OR NUMERIC
C       INPUT IN AN EITHER OR MODE. THEY DOES NOT ACCEPT STING
C       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
C       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
C       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
C
          SF=INT(SYSTEM1(20))
          IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
              CW=INT(SYSTEM1(11))+45
          END IF
          IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
              CW=INT(SYSTEM1(11))+65
          END IF
          IF(WC.EQ.'FCHX') THEN
              INV=((PXTRAX(5,SF)*ALENS(CW,(SF-1))*PXTRAX(2,(SF-1)))-
     1        (PXTRAX(1,SF)*ALENS(CW,(SF-1))*PXTRAX(6,(SF-1))))
          END IF
          IF(WC.EQ.'FCHY') THEN
              INV=((PXTRAY(5,SF)*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1)))-
     1        (PXTRAY(1,SF)*ALENS(CW,(SF-1))*PXTRAY(6,(SF-1))))
          END IF
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'FCHY') WRITE(OUTLYNE,*)
     1        '"FCHY" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
              IF(WC.EQ.'FCHX') WRITE(OUTLYNE,*)
     1        '"FCHX" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              IF(WC.EQ.'FCHY') WRITE(OUTLYNE,*)
     1        '"FCHY" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
              IF(WC.EQ.'FCHX') WRITE(OUTLYNE,*)
     1        '"FCHX" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO PARAXIAL DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'FCHX')WRITE(OUTLYNE,5001) INT(F12)
              IF(WC.EQ.'FCHY')WRITE(OUTLYNE,5006) INT(F12)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.1.0) WRITE(OUTLYNE,5501)
              IF(SYSTEM1(30).EQ.2.0) WRITE(OUTLYNE,5502)
              IF(SYSTEM1(30).EQ.3.0) WRITE(OUTLYNE,5503)
              IF(SYSTEM1(30).EQ.4.0) WRITE(OUTLYNE,5504)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              IF(WC.EQ.'FCHX')WRITE(OUTLYNE,5000)
              IF(WC.EQ.'FCHY')WRITE(OUTLYNE,5005)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.2.0) THEN
C       MODE FOCAL OR UFOCAL
                      IF(SYSTEM1(30).EQ.1.0) THEN
C       FOCAL, CONVERT
                          IF(WC.EQ.'FCHX') THEN
                              C1=COLORX(1,I)/INV
                              C2=COLORX(2,I)/INV
                              C3=COLORX(3,I)/INV
                              C4=COLORX(4,I)/INV
                          END IF
                          IF(WC.EQ.'FCHY') THEN
                              C1=COLORY(1,I)/INV
                              C2=COLORY(2,I)/INV
                              C3=COLORY(3,I)/INV
                              C4=COLORY(4,I)/INV
                          END IF
                          WRITE(OUTLYNE,2000)I,C1,C2,C3,C4
                          CALL SHOWIT(0)
                          GO TO 10
                      END IF
                      IF(SYSTEM1(30).EQ.2.0) THEN
C       UFOCAL, DON'T CONVERT
                          IF(WC.EQ.'FCHX') THEN
                              C1=COLORX(1,I)
                              C2=COLORX(2,I)
                              C3=COLORX(3,I)
                              C4=COLORX(4,I)
                          END IF
                          IF(WC.EQ.'FCHY') THEN
                              C1=COLORY(1,I)
                              C2=COLORY(2,I)
                              C3=COLORY(3,I)
                              C4=COLORY(4,I)
                          END IF
                          WRITE(OUTLYNE,2000)I,C1,C2,C3,C4
                          CALL SHOWIT(0)
                          GO TO 10
                      END IF
                  END IF
                  IF(SYSTEM1(30).EQ.3.0.OR.SYSTEM1(30).EQ.4.0) THEN
C       MODE AFOCAL OR UAFOCAL
                      IF(SYSTEM1(30).EQ.3.0) THEN
C       AFOCAL, CONVERT
                          IF(WC.EQ.'FCHX') THEN
                              C1=COLORX(5,I)/INV
                              C2=COLORX(6,I)/INV
                              C3=COLORX(7,I)/INV
                              C4=COLORX(8,I)/INV
                          END IF
                          IF(WC.EQ.'FCHY') THEN
                              C1=COLORY(5,I)/INV
                              C2=COLORY(6,I)/INV
                              C3=COLORY(7,I)/INV
                              C4=COLORY(8,I)/INV
                          END IF
                          WRITE(OUTLYNE,2000)I,C1,C2,C3,C4
                          CALL SHOWIT(0)
                          GO TO 10
                      END IF
                      IF(SYSTEM1(30).EQ.4.0) THEN
C       UAFOCAL, DON'T CONVERT
                          IF(WC.EQ.'FCHX') THEN
                              C1=COLORX(5,I)
                              C2=COLORX(6,I)
                              C3=COLORX(7,I)
                              C4=COLORX(8,I)
                          END IF
                          IF(WC.EQ.'FCHY') THEN
                              C1=COLORY(5,I)
                              C2=COLORY(6,I)
                              C3=COLORY(7,I)
                              C4=COLORY(8,I)
                          END IF
                          WRITE(OUTLYNE,2000)I,C1,C2,C3,C4
                          CALL SHOWIT(0)
                          GO TO 10
                      END IF
                  END IF
 10           CONTINUE
C       NOW HANDEL THE CALCULATION AND PRINTING OF THE
C       TOTALS
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              TPAC=0.0
              TPLC=0.0
              TSAC=0.0
              TSLC=0.0
              DO 20 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.2.0) THEN
                      IF(WC.EQ.'FCHX') THEN
                          TPAC=TPAC+COLORX(1,I)
                          TPLC=TPLC+COLORX(2,I)
                          TSAC=TSAC+COLORX(3,I)
                          TSLC=TSLC+COLORX(4,I)
                      END IF
                      IF(WC.EQ.'FCHY') THEN
                          TPAC=TPAC+COLORY(1,I)
                          TPLC=TPLC+COLORY(2,I)
                          TSAC=TSAC+COLORY(3,I)
                          TSLC=TSLC+COLORY(4,I)
                      END IF
                  END IF
                  IF(SYSTEM1(30).EQ.3.0.OR.SYSTEM1(30).EQ.4.0) THEN
                      IF(WC.EQ.'FCHX') THEN
                          TPAC=TPAC+COLORX(5,I)
                          TPLC=TPLC+COLORX(6,I)
                          TSAC=TSAC+COLORX(7,I)
                          TSLC=TSLC+COLORX(8,I)
                      END IF
                      IF(WC.EQ.'FCHY') THEN
                          TPAC=TPAC+COLORY(5,I)
                          TPLC=TPLC+COLORY(6,I)
                          TSAC=TSAC+COLORY(7,I)
                          TSLC=TSLC+COLORY(8,I)
                      END IF
                  END IF
 20           CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 25
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0) THEN
C       MODE IS FOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  TPAC=TPAC*(1.0D0/INV)
                  TPLC=TPLC*(1.0D0/INV)
                  TSAC=TSAC*(1.0D0/INV)
                  TSLC=TSLC*(1.0D0/INV)
                  GO TO 25
              END IF
C       MODE AFOCAL
              IF(SYSTEM1(30).EQ.3.0) THEN
C       MODE IS AFOCAL
                  TPAC=TPAC*(1.0D0/INV)
                  TPLC=TPLC*(1.0D0/INV)
                  TSAC=TSAC*(1.0D0/INV)
                  TSLC=TSLC*(1.0D0/INV)
C
                  GO TO 25
              END IF
C       PRINT TOTALS WITH LABELING
 25           CONTINUE
              WRITE(OUTLYNE,998)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2001) TPAC,TPLC,TSAC,TSLC
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=0
              IF(WC.EQ.'FCHX') THEN
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,COLORX(1,SF),COLORX(2,SF)
     2            ,COLORX(3,SF),COLORX(4,SF)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'FCHY') THEN
                  IF(HEADIN) WRITE(OUTLYNE,5005)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,COLORY(1,SF),COLORY(2,SF)
     2            ,COLORY(3,SF),COLORY(4,SF)
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
              SF=INT(SYSTEM1(20))
C       OUTPUT TOTALS
C
C       IN ANY CASE, THE SURFACE CONTRIBUTIONS MUST BE SUMMED
              TPAC=0.0
              TPLC=0.0
              TSAC=0.0
              TSLC=0.0
              DO 220 I=0,SF
                  IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.2.0) THEN
                      IF(WC.EQ.'FCHX') THEN
                          TPAC=TPAC+COLORX(1,I)
                          TPLC=TPLC+COLORX(2,I)
                          TSAC=TSAC+COLORX(3,I)
                          TSLC=TSLC+COLORX(4,I)
                      END IF
                      IF(WC.EQ.'FCHY') THEN
                          TPAC=TPAC+COLORY(1,I)
                          TPLC=TPLC+COLORY(2,I)
                          TSAC=TSAC+COLORY(3,I)
                          TSLC=TSLC+COLORY(4,I)
                      END IF
                  END IF
                  IF(SYSTEM1(30).EQ.3.0.OR.SYSTEM1(30).EQ.4.0) THEN
                      IF(WC.EQ.'FCHX') THEN
                          TPAC=TPAC+COLORX(5,I)
                          TPLC=TPLC+COLORX(6,I)
                          TSAC=TSAC+COLORX(7,I)
                          TSLC=TSLC+COLORX(8,I)
                      END IF
                      IF(WC.EQ.'FCHY') THEN
                          TPAC=TPAC+COLORY(5,I)
                          TPLC=TPLC+COLORY(6,I)
                          TSAC=TSAC+COLORY(7,I)
                          TSLC=TSLC+COLORY(8,I)
                      END IF
                  END IF
 220          CONTINUE
C NOW FOR CONVERSIONS
C       MODE UFOCAL
              IF(SYSTEM1(30).EQ.2.0) THEN
C       MODE IS UFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE UAFOCAL
              IF(SYSTEM1(30).EQ.4.0) THEN
C       MODE IS UAFOCAL, NO CONVERSIONS
                  GO TO 250
              END IF
C       MODE FOCAL
              IF(SYSTEM1(30).EQ.1.0) THEN
C       MODE IS FOCAL, CONVERT SUMS TO TRANSVERSE
C       CHROMATIC ABERRATION
                  TPAC=TPAC*(1.0D0/INV)
                  TPLC=TPLC*(1.0D0/INV)
                  TSAC=TSAC*(1.0D0/INV)
                  TSLC=TSLC*(1.0D0/INV)
                  GO TO 250
              END IF
C       MODE AFOCAL
              IF(SYSTEM1(30).EQ.3.0) THEN
C       MODE IS AFOCAL
                  TPAC=TPAC/(INV)
                  TPLC=TPLC/(INV)
                  TSAC=TSAC/(INV)
                  TSLC=TSLC/(INV)
C
                  GO TO 250
              END IF
 250          CONTINUE
C       PRINT TOTALS WITH OUT LABELING
              IF(WC.EQ.'FCHX') THEN
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'FCHY') THEN

                  IF(HEADIN) WRITE(OUTLYNE,5005)
                  IF(HEADIN) CALL SHOWIT(0)
              END IF
              WRITE(OUTLYNE,2001) TPAC,TPLC,TSAC,TSLC
              CALL SHOWIT(0)
          END IF
C
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SYSTEM1(30).EQ.1.0.OR.SYSTEM1(30).EQ.2.0) THEN
C       MODE FOCAL OR UFOCAL
                  IF(SYSTEM1(30).EQ.1.0) THEN
C       FOCAL, CONVERT
                      IF(WC.EQ.'FCHX') THEN
                          C1=COLORX(1,I)/INV
                          C2=COLORX(2,I)/INV
                          C3=COLORX(3,I)/INV
                          C4=COLORX(4,I)/INV
                      END IF
                      IF(WC.EQ.'FCHY') THEN
                          C1=COLORY(1,I)/INV
                          C2=COLORY(2,I)/INV
                          C3=COLORY(3,I)/INV
                          C4=COLORY(4,I)/INV
                      END IF
                      IF(WC.EQ.'FCHX') THEN
                          IF(HEADIN) WRITE(OUTLYNE,5000)
                          IF(HEADIN) CALL SHOWIT(0)
                      END IF
                      IF(WC.EQ.'FCHY') THEN
                          IF(HEADIN) WRITE(OUTLYNE,5005)
                          IF(HEADIN) CALL SHOWIT(0)
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4
                      CALL SHOWIT(0)
                      RETURN
                  END IF
                  IF(SYSTEM1(30).EQ.2.0) THEN
C       UFOCAL, DON'T CONVERT
                      IF(WC.EQ.'FCHX') THEN
                          C1=COLORX(1,I)
                          C2=COLORX(2,I)
                          C3=COLORX(3,I)
                          C4=COLORX(4,I)
                      END IF
                      IF(WC.EQ.'FCHY') THEN
                          C1=COLORY(1,I)
                          C2=COLORY(2,I)
                          C3=COLORY(3,I)
                          C4=COLORY(4,I)
                      END IF
                      IF(WC.EQ.'FCHX') THEN
                          IF(HEADIN) WRITE(OUTLYNE,5000)
                          IF(HEADIN) CALL SHOWIT(0)
                      END IF
                      IF(WC.EQ.'FCHY') THEN
                          IF(HEADIN) WRITE(OUTLYNE,5005)
                          IF(HEADIN) CALL SHOWIT(0)
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4
                      CALL SHOWIT(0)
                      RETURN
                  END IF
              END IF
              IF(SYSTEM1(30).EQ.3.0.OR.SYSTEM1(30).EQ.4.0) THEN
C       MODE AFOCAL OR UAFOCAL
                  IF(SYSTEM1(30).EQ.3.0) THEN
C       AFOCAL, CONVERT
                      IF(WC.EQ.'FCHX') THEN
                          C1=COLORX(5,I)/INV
                          C2=COLORX(6,I)/INV
                          C3=COLORX(7,I)/INV
                          C4=COLORX(8,I)/INV
                      END IF
                      IF(WC.EQ.'FCHY') THEN
                          C1=COLORY(5,I)/INV
                          C2=COLORY(6,I)/INV
                          C3=COLORY(7,I)/INV
                          C4=COLORY(8,I)/INV
                      END IF
                      IF(WC.EQ.'FCHX') THEN
                          IF(HEADIN) WRITE(OUTLYNE,5000)
                          IF(HEADIN) CALL SHOWIT(0)
                      END IF
                      IF(WC.EQ.'FCHY') THEN
                          IF(HEADIN) WRITE(OUTLYNE,5005)
                          IF(HEADIN) CALL SHOWIT(0)
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4
                      CALL SHOWIT(0)
                      RETURN
                  END IF
                  IF(SYSTEM1(30).EQ.4.0) THEN
C       UAFOCAL, DON'T CONVERT
                      IF(WC.EQ.'FCHX') THEN
                          C1=COLORX(5,I)
                          C2=COLORX(6,I)
                          C3=COLORX(7,I)
                          C4=COLORX(8,I)
                      END IF
                      IF(WC.EQ.'FCHY') THEN
                          C1=COLORY(5,I)
                          C2=COLORY(6,I)
                          C3=COLORY(7,I)
                          C4=COLORY(8,I)
                      END IF
                      IF(WC.EQ.'FCHX') THEN
                          IF(HEADIN) WRITE(OUTLYNE,5000)
                          IF(HEADIN) CALL SHOWIT(0)
                      END IF
                      IF(WC.EQ.'FCHY') THEN

                          IF(HEADIN) WRITE(OUTLYNE,5005)
                          IF(HEADIN) CALL SHOWIT(0)
                      END IF
                      WRITE(OUTLYNE,2000)I,C1,C2,C3,C4
                      CALL SHOWIT(0)
                      RETURN
                  END IF
              END IF
              RETURN
          END IF
 1500     FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6)
 2000     FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6)
 2001     FORMAT(5X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6)
 5000     FORMAT('SURF',6X,'PACX',11X,'PLCX',11X,'SACX',11X,'SLCX')
 5001     FORMAT('FIRST ORDER CHROMATIC ABERRATION (XZ-PLANE)'
     1    ,' - (CFG #',I2,')')
 5501     FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
 5502     FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
 5503     FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
 5504     FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
 2501     FORMAT(1X)
 998      FORMAT('SYSTEM TOTALS ARE:')
 5005     FORMAT('SURF',6X,'PACY',11X,'PLCY',11X,'SACY',11X,'SLCY')
 5006     FORMAT('FIRST ORDER CHROMATIC ABERRATION (YZ-PLANE)'
     1    ,' - (CFG #',I2,')')
      END
