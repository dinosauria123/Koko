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

C       FIRST SET OF PARAXIAL ROUTINES GO HERE

C SUB PRCOL.FOR
      SUBROUTINE PRCOL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRCOL. THIS DOES A PARAXIAL
C       TRACE AT WAVELENGTH NUMBER WV WITHOUT RAY AIMING
C       OR SOLVE RESOLUTION. ITYP=1=YZ,ITYP=2=XZ
C
          INTEGER ITYP,L
C
          REAL*8 CURV,WV
C
          COMMON/PRCOM/WV,ITYP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          IF(INT(WV).EQ.1) WWVN=46
          IF(INT(WV).EQ.2) WWVN=47
          IF(INT(WV).EQ.3) WWVN=48
          IF(INT(WV).EQ.4) WWVN=49
          IF(INT(WV).EQ.5) WWVN=50
          IF(INT(WV).EQ.6) WWVN=71
          IF(INT(WV).EQ.7) WWVN=72
          IF(INT(WV).EQ.8) WWVN=73
          IF(INT(WV).EQ.9) WWVN=74
          IF(INT(WV).EQ.10) WWVN=75
C
          IF(ITYP.EQ.1) THEN
C
C       ALL PARAXIAL TRACING OPERATES ON THE CURRENT LENS
C       THIS COULD BE AN ALTERNATE CONFIGURATION.
C       THE PARAXIAL RAYTRACE PERFORMED HERE IGNORS
C       ALL YZ- PLANE SOLVES.
C
C       THE WAVELENGTH USED IN THE EQUATIONS TO FOLLOW IS
C
C                       WV
C
C               INITIAL VALUES AT SURFACE 0 COME FROM THE
C       PARAXIAL TRACE DATA AT THE CONTROL WAVELENGTH
C***************************************************************
C               INITIAL VALUES AT SURFACE 0
              COLY(1,0)=PXTRAY(1,0)
              COLY(2,0)=PXTRAY(2,0)
              COLY(3,0)=PXTRAY(3,0)
              COLY(4,0)=PXTRAY(4,0)
              COLY(5,0)=PXTRAY(5,0)
              COLY(6,0)=PXTRAY(6,0)
              COLY(7,0)=PXTRAY(7,0)
              COLY(8,0)=PXTRAY(8,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C
C               INITIAL VALUES AT SURFACE 1
C       COLY(1,1)
              COLY(1,1)=PXTRAY(1,1)
C       COLY(2,1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
              IF(ALENS(23,1).EQ.2.0D0) THEN
                  CURV=ALENS(24,1)
              ELSE
                  IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                      CURV=ALENS(43,1)*2.0D0
                  ELSE
                      CURV=ALENS(1,1)
                  END IF
              END IF
              COLY(2,1)=-CURV*COLY(1,1)*
     1        (((ALENS(WWVN,1))-
     2        (ALENS(WWVN,0)))/
     3        (ALENS(WWVN,1)))+
     4        ((ALENS(WWVN,0))/
     5        (ALENS(WWVN,1)))*COLY(2,0)
C
C       COLY(3,1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
              IF(ALENS(23,1).EQ.2.0D0) THEN
                  CURV=ALENS(24,1)
              ELSE
                  IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                      CURV=ALENS(43,1)*2.0D0
                  ELSE
                      CURV=ALENS(1,1)
                  END IF
              END IF
              COLY(3,1)=CURV*COLY(1,1)+COLY(2,0)
C       COLY(4,1)
              COLY(4,1)=((ALENS((WWVN),0))/
     1        (ALENS((WWVN),1)))*COLY(3,1)
C
C       COLY(5,1)
              COLY(5,1)=PXTRAY(5,1)
C
C       COLY(6,1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
              IF(ALENS(23,1).EQ.2.0D0) THEN
                  CURV=ALENS(24,1)
              ELSE
                  IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                      CURV=ALENS(43,1)*2.0D0
                  ELSE
                      CURV=ALENS(1,1)
                  END IF
              END IF
              COLY(6,1)=-CURV*COLY(5,1)*
     1        (((ALENS(WWVN,1))-
     2        (ALENS(WWVN,0)))/
     3        (ALENS(WWVN,1)))+
     4        ((ALENS(WWVN,0))/
     5        (ALENS(WWVN,1)))*COLY(6,0)
C
C       COLY(7,1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
              IF(ALENS(23,1).EQ.2.0D0) THEN
                  CURV=ALENS(24,1)
              ELSE
                  IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                      CURV=ALENS(43,1)*2.0D0
                  ELSE
                      CURV=ALENS(1,1)
                  END IF
              END IF
              COLY(7,1)=(CURV*COLY(5,1))+COLY(6,0)
C       COLY(8,1)
              COLY(8,1)=((ALENS((WWVN),0))/
     1        (ALENS((WWVN),1)))*COLY(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C
C       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
C       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
              DO 80 L=2,INT(SYSTEM1(20))
C               VALUES AT SURFACE L
C******************************************************************************
C       NOW CALCULATE COLY(1,L) VALUE
                  COLY(1,L)=COLY(1,(L-1))+(ALENS(3,(L-1))*COLY(2,(L-1)))
C******************************************************************************
C       NOW CALCULATE COLY(2,L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,L).EQ.2.0D0) THEN
                      CURV=ALENS(24,L)
                  ELSE
                      IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                          CURV=ALENS(43,L)*2.0D0
                      ELSE
                          CURV=ALENS(1,L)
                      END IF
                  END IF
                  COLY(2,L)=-CURV*COLY(1,L)*
     1            (((ALENS(WWVN,L))-
     2            (ALENS(WWVN,(L-1))))/
     3            (ALENS(WWVN,L)))+
     4            ((ALENS(WWVN,(L-1)))/
     5            (ALENS(WWVN,L)))*COLY(2,(L-1))
C*****************************************************************
C       COLY(3,L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,L).EQ.2.0D0) THEN
                      CURV=ALENS(24,L)
                  ELSE
                      IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                          CURV=ALENS(43,L)*2.0D0
                      ELSE
                          CURV=ALENS(1,L)
                      END IF
                  END IF
                  COLY(3,L)=CURV*COLY(1,L)+COLY(2,(L-1))
C*******************************************************************
C       COLY(4,L)
                  COLY(4,L)=((ALENS((WWVN),(L-1)))/
     1            (ALENS((WWVN),L)))*COLY(3,L)
C*******************************************************************
C       COLY(5,L)
                  COLY(5,L)=COLY(5,(L-1))+(ALENS(3,(L-1))*COLY(6,(L-1)))
C************************************************************************
C       COLY(6,L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,L).EQ.2.0D0) THEN
                      CURV=ALENS(24,L)
                  ELSE
                      IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                          CURV=ALENS(43,L)*2.0D0
                      ELSE
                          CURV=ALENS(1,L)
                      END IF
                  END IF
                  COLY(6,L)=-CURV*COLY(5,L)*
     1            (((ALENS(WWVN,L))-
     2            (ALENS(WWVN,(L-1))))/
     3            (ALENS(WWVN,L)))+
     4            ((ALENS(WWVN,(L-1)))/
     5            (ALENS(WWVN,L)))*COLY(6,(L-1))
C**************************************************************
C       COLY(7,L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,L).EQ.2.0D0) THEN
                      CURV=ALENS(24,L)
                  ELSE
                      IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                          CURV=ALENS(43,L)*2.0D0
                      ELSE
                          CURV=ALENS(1,L)
                      END IF
                  END IF
                  COLY(7,L)=(CURV*COLY(5,L))+COLY(6,(L-1))
C***************************************************************
                  COLY(8,L)=((ALENS((WWVN),(L-1)))/
     1            (ALENS((WWVN),L)))*COLY(7,L)
C***************************************************************
 80           CONTINUE
C       PARAXIAL TRACE COMPLETED
              RETURN
          ELSE
C       ITYP NOT 1
          END IF
          IF(ITYP.EQ.2) THEN
C
C       THE WAVELENGTH USED IN THE EQUATIONS TO FOLLOW IS
C
C                       WV
C
C               INITIAL VALUES AT SURFACE 0 COME FROM THE
C       PARAXIAL TRACE DATA AT THE CONTROL WAVELENGTH
C***************************************************************
C               INITIAL VALUES AT SURFACE 0
              COLX(1,0)=PXTRAX(1,0)
              COLX(2,0)=PXTRAX(2,0)
              COLX(3,0)=PXTRAX(3,0)
              COLX(4,0)=PXTRAX(4,0)
              COLX(5,0)=PXTRAX(5,0)
              COLX(6,0)=PXTRAX(6,0)
              COLX(7,0)=PXTRAX(7,0)
              COLX(8,0)=PXTRAX(8,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C
C               INITIAL VALUES AT SURFACE 1
C       COLX(1,1)
              COLX(1,1)=PXTRAX(1,1)
C       COLX(2,1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
              IF(ALENS(23,1).EQ.1.0D0) THEN
                  CURV=ALENS(24,1)
              ELSE
                  IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                      CURV=ALENS(43,1)*2.0D0
                  ELSE
                      CURV=ALENS(1,1)
                  END IF
              END IF
              COLX(2,1)=-CURV*COLX(1,1)*
     1        (((ALENS(WWVN,1))-
     2        (ALENS(WWVN,0)))/
     3        (ALENS(WWVN,1)))+
     4        ((ALENS(WWVN,0))/
     5        (ALENS(WWVN,1)))*COLX(2,0)
C
C       COLX(3,1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
              IF(ALENS(23,1).EQ.1.0D0) THEN
                  CURV=ALENS(24,1)
              ELSE
                  IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                      CURV=ALENS(43,1)*2.0D0
                  ELSE
                      CURV=ALENS(1,1)
                  END IF
              END IF
              COLX(3,1)=CURV*COLX(1,1)+COLX(2,0)
C       COLX(4,1)
              COLX(4,1)=((ALENS((WWVN),0))/
     1        (ALENS((WWVN),1)))*COLX(3,1)
C
C       COLX(5,1)
              COLX(5,1)=PXTRAX(5,1)
C
C       COLX(6,1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
              IF(ALENS(23,1).EQ.1.0D0) THEN
                  CURV=ALENS(24,1)
              ELSE
                  IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                      CURV=ALENS(43,1)*2.0D0
                  ELSE
                      CURV=ALENS(1,1)
                  END IF
              END IF
              COLX(6,1)=-CURV*COLX(5,1)*
     1        (((ALENS(WWVN,1))-
     2        (ALENS(WWVN,0)))/
     3        (ALENS(WWVN,1)))+
     4        ((ALENS(WWVN,0))/
     5        (ALENS(WWVN,1)))*COLX(6,0)
C
C       COLX(7,1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
              IF(ALENS(23,1).EQ.1.0D0) THEN
                  CURV=ALENS(24,1)
              ELSE
                  IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                      CURV=ALENS(43,1)*2.0D0
                  ELSE
                      CURV=ALENS(1,1)
                  END IF
              END IF
              COLX(7,1)=(CURV*COLX(5,1))+COLX(6,0)
C       COLX(8,1)
              COLX(8,1)=((ALENS((WWVN),0))/
     1        (ALENS((WWVN),1)))*COLX(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C
C       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
C       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
              DO 800 L=2,INT(SYSTEM1(20))
C               VALUES AT SURFACE L
C******************************************************************************
C       NOW CALCULATE COLX(1,L) VALUE
                  COLX(1,L)=COLX(1,(L-1))+(ALENS(3,(L-1))*COLX(2,(L-1)))
C******************************************************************************
C       NOW CALCULATE COLX(2,L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,L).EQ.1.0D0) THEN
                      CURV=ALENS(24,L)
                  ELSE
                      IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                          CURV=ALENS(43,L)*2.0D0
                      ELSE
                          CURV=ALENS(1,L)
                      END IF
                  END IF
                  COLX(2,L)=-CURV*COLX(1,L)*
     1            (((ALENS(WWVN,L))-
     2            (ALENS(WWVN,(L-1))))/
     3            (ALENS(WWVN,L)))+
     4            ((ALENS(WWVN,(L-1)))/
     5            (ALENS(WWVN,L)))*COLX(2,(L-1))
C*****************************************************************
C       COLX(3,L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,L).EQ.1.0D0) THEN
                      CURV=ALENS(24,L)
                  ELSE
                      IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                          CURV=ALENS(43,L)*2.0D0
                      ELSE
                          CURV=ALENS(1,L)
                      END IF
                  END IF
                  COLX(3,L)=CURV*COLX(1,L)+COLX(2,(L-1))
C*******************************************************************
C       COLX(4,L)
                  COLX(4,L)=((ALENS((WWVN),(L-1)))/
     1            (ALENS((WWVN),L)))*COLX(3,L)
C*******************************************************************
C       COLX(5,L)
                  COLX(5,L)=COLX(5,(L-1))+(ALENS(3,(L-1))*COLX(6,(L-1)))
C************************************************************************
C       COLX(6,L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,L).EQ.1.0D0) THEN
                      CURV=ALENS(24,L)
                  ELSE
                      IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                          CURV=ALENS(43,L)*2.0D0
                      ELSE
                          CURV=ALENS(1,L)
                      END IF
                  END IF
                  COLX(6,L)=-CURV*COLX(5,L)*
     1            (((ALENS(WWVN,L))-
     2            (ALENS(WWVN,(L-1))))/
     3            (ALENS(WWVN,L)))+
     4            ((ALENS(WWVN,(L-1)))/
     5            (ALENS(WWVN,L)))*COLX(6,(L-1))
C**************************************************************
C       COLX(7,L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,L).EQ.1.0D0) THEN
                      CURV=ALENS(24,L)
                  ELSE
                      IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                          CURV=ALENS(43,L)*2.0D0
                      ELSE
                          CURV=ALENS(1,L)
                      END IF
                  END IF
                  COLX(7,L)=(CURV*COLX(5,L))+COLX(6,(L-1))
C***************************************************************
                  COLX(8,L)=((ALENS((WWVN),(L-1)))/
     1            (ALENS((WWVN),L)))*COLX(7,L)
C***************************************************************
 800          CONTINUE
C       PARAXIAL TRACE COMPLETED
              RETURN
          ELSE
C       ITYP NOT 2
          END IF
      END



C SUB PRTRA.FOR
      SUBROUTINE PRTRA
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRTRA. THIS IS THE
C       SUBROUTINE WHICH IMPLEMENTS THE PARAXIAL RAY TRACE.
C       IT IS CALLED FROM LNSEOS. THIS SUBROUTINE
C       TRACES IN THE YZ PLANE IF ITYPW=1, THE XZ PLANE
C       IF ITYPEP=2 AND THE YZ AND XZ PLANE IF ITYPEP=3.
C       PIKUPS ARE ALSO RESOLVED BY
C       CALLS TO PIKRES FOR EACH SURFACE FROM THIS
C       SUBROUTINE. THE COLOR CONTRIBUTIONS FOR THE YZ PLANE
C       ARE CALCULATED WITH A CALL TO CCOL(1) AND XZ BY CCOL(2).
C
          INTEGER ITYPEP,JK,L,ITYP,SLV1,SLV2,COMI
C
          COMMON/PTYPER/ITYPEP
C
          COMMON/CSLVRS/SLV1,SLV2
C
          REAL*8 SYS13,TMP15A,TMP15B,
     1    CON,CURV,TMP17A,TMP17B,WV
C
          COMMON/PRCOM/WV,ITYP
C
          COMMON/PIKCOM/COMI
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
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
          CON=SYSTEM1(15)
          IF(ITYPEP.EQ.1.OR.ITYPEP.EQ.3) THEN
C
C       THIS IS THE FIRST OF THE PARAXIAL RAY TRACING SUBROUTINES
C       IT IS AUTOMATICALLY CALLED FROM LNSEOS AFTER LENS INPUT
C       OR LENS UPDATE AND IS ALSO CALLED WHEN SPECIFIC PARAXIAL
C       DATA IS NEEDED WHICH IS NOT ALREADY ON-FILE IN THE
C       PARAXIAL RAYTRACE STORAGE ARRAY.
C
C       ALL PARAXIAL TRACING OPERATES ON THE CURRENT LENS
C       THIS COULD BE AN ALTERNATE CONFIGURATION.
C       THE PARAXIAL RAYTRACE PERFORMED HERE HANDLES
C       ALL YZ- PLANE SOLVES THROUGH A CALL TO SUBROUTINE
C       SLVRSY.
C       7/23/91 SET CON = SYSTEM1(15) FOR THE YZ PLANE TRACE
C
C       IF AN APERTURE STOP IS DEFINED ON ANY SURFACE
C       THE VALUE OF SYSTEM1(15) NEEDS TO BE REFINED.
C
C       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
C       UP TO THE APERTURE STOP SURFACE (UNLESS THERE IS NO
C       APERTURE STOP DEFINED) USING TWO DIFFERENT VALUES
C       OF SYSTEM1(15) [HEIGTH OF CHIEF RAY AT SURF 1]
C
C       THE TWO VALUES USED ARE 0.0 AND 0.1
C
C       THE CORRECET VALUE OF SYSTEM1(15) WHICH MAKES PCY ON THE
C       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
C
C       PCY(AT ASTOP FOR SYSTEM1(15)=0.0) IS CALLED TMP15A
C       PCY(AT ASTOP FOR SYSTEM1(15)=0.1) IS CALLED TMP15B
C
C       SYSTEM1(15)=((-.1*TMP15A)/(TMP15B-TMP15A))+SYSTEM1(15)
C
              IF(SYSTEM1(26).GT.0.0D0.AND.SYSTEM1(63).EQ.0.0D0) THEN
C
C       RECALCULATE THE CORRECT VALUE OF SYSTEM1(15)
C       OTHERWISE, USE THE USER PROVIDED VALUE OF SYSTEM1(15)
C
C                       RAY TARGETING INFORMATION
C
                  DO 60 JK=1,2
                      IF(JK.EQ.1) CON=CON+0.0D0
                      IF(JK.EQ.2) CON=CON+0.1D0
C*************************************************************************
C       INITIAL TARGET OF RAY TRACE
C               THE INITIAL PARAXIAL RAYTRACE TARGETING
C               DATA IS:
C                       AT SURFACE 0 (OBJ)
C
C       STARTING MARGINAL RAY HEIGHT = 0
C       STARTING CHIEF RAY HEIGHT    = SCY
C                       AT SURFACE 1 (INITIAL REF SURF)
C       STARTING MARGINAL RAY HEIGHT = SAY
C       STARTING CHIEF RAY HEIGHT = CON
C
C
C               INITIAL VALUES AT SURFACE 0
C***************************************************************
C       CALL PIKRES FOR THE OBJECT SURFACE
                      COMI=0
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(0)=0,  ALWAYS
                      PXTRAY(1,0)=0.0D0
C
C       PUY(0)=SAY/TH(0)
C
                      IF(ALENS(3,0).EQ.0.0D0) THEN
                          OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      PXTRAY(2,0)=(SYSTEM1(12))/ALENS(3,0)
C
C       PIY(0) =PUY(0)
                      PXTRAY(3,0)=PXTRAY(2,0)
C
C       PIY'(0)=PUY(0)
                      PXTRAY(4,0)=PXTRAY(3,0)
C
C       PCY(0) =-SCY
                      PXTRAY(5,0)=(SYSTEM1(14))
                      IF(SYSTEM1(14).EQ.0.0D0) PXTRAY(5,0)=1.0D0
C
C       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
C       CON IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
C
                      IF(ALENS(3,0).EQ.0.0D0) THEN
                          OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      PXTRAY(6,0)=-((SYSTEM1(14))-CON)/ALENS(3,0)
                      IF(SYSTEM1(14).EQ.0.0D0) PXTRAY(6,0)=
     1                -(1.0D0-CON)/ALENS(3,0)
C
C       PICY(0) AT OBJECT, PICY = PUCY
                      PXTRAY(7,0)=PXTRAY(6,0)
C
C       PICY'(0) AT OBJECT PICY'=PICY
                      PXTRAY(8,0)=PXTRAY(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C
                      L=1
                      SLV1=L
                      SLV2=1
                      IF(SOLVE(6,L).NE.0.0D0.OR.
     1                SOLVE(8,L).NE.0.0D0) CALL SLVRS
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                      COMI=1
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN SYSTEM1(12)
                      PXTRAY(1,1)=(SYSTEM1(12))
C

C       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
C
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.2.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAY(2,1)=-CURV*PXTRAY(1,1)*
     1                (((ALENS(WWVN,1))-
     2                (ALENS(WWVN,0)))/
     3                (ALENS(WWVN,1)))+
     4                ((ALENS(WWVN,0))/
     5                (ALENS(WWVN,1)))*PXTRAY(2,0)
                      IF(GLANAM(1,2).EQ.'PERFECT      ')
     1                PXTRAY(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
                      IF(GLANAM(1,2).EQ.'IDEAL        ')
     1                PXTRAY(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(2,1-1)

C
C       PIY(1)=CV(1)*PY(1)+PUY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.2.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
C
C       PIY'(1)=(N/N')*PIY(1)
                      PXTRAY(4,1)=((ALENS((WWVN),0))/
     1                (ALENS((WWVN),1)))*PXTRAY(3,1)
C
C       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                      PXTRAY(5,1)=CON
C
C       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.2.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAY(6,1)=-CURV*PXTRAY(5,1)*
     1                (((ALENS(WWVN,1))-
     2                (ALENS(WWVN,0)))/
     3                (ALENS(WWVN,1)))+
     4                ((ALENS(WWVN,0))/
     5                (ALENS(WWVN,1)))*PXTRAY(6,0)
                      IF(GLANAM(1,2).EQ.'PERFECT      ')
     1                PXTRAY(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
                      IF(GLANAM(1,2).EQ.'IDEAL        ')
     1                PXTRAY(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
C
C       PICY(1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.2.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
C       PICY'(1)
                      PXTRAY(8,1)=((ALENS((WWVN),0))/
     1                (ALENS((WWVN),1)))*PXTRAY(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C *****************************************************************************
C       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:

                      DO 50 L=2,INT(SYSTEM1(26))
C               VALUES AT SURFACE L
                          SLV1=L
                          SLV2=1
                          IF(SOLVE(6,L).NE.0.0D0.OR.
     1                    SOLVE(8,L).NE.0.0D0) CALL SLVRS
C       CALL PIKRES FOR THE SURFACE L
                          COMI=L
                          IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
                          PXTRAY(1,L)=PXTRAY(1,(L-1))+(ALENS(3,(L-1))*PXTRAY(2,(L-1)))
C
C       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.2.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAY(2,L)=-CURV*PXTRAY(1,L)*
     1                    (((ALENS(WWVN,L))-
     2                    (ALENS(WWVN,(L-1))))/
     3                    (ALENS(WWVN,L)))+
     4                    ((ALENS(WWVN,(L-1)))/
     5                    (ALENS(WWVN,L)))*PXTRAY(2,(L-1))
                          IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                    PXTRAY(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
                          IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                    PXTRAY(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
C
C       PIY(L)=CV(1)*PY(L)+PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.2.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
C
C       PIY'(L)=(N/N')*PIY(L)
                          PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/
     1                    (ALENS((WWVN),L)))*PXTRAY(3,L)
C
C       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
                          PXTRAY(5,L)=PXTRAY(5,(L-1))+(ALENS(3,(L-1))*PXTRAY(6,(L-1)))
C
C       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.2.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAY(6,L)=-CURV*PXTRAY(5,L)*
     1                    (((ALENS(WWVN,L))-
     2                    (ALENS(WWVN,(L-1))))/
     3                    (ALENS(WWVN,L)))+
     4                    ((ALENS(WWVN,(L-1)))/
     5                    (ALENS(WWVN,L)))*PXTRAY(6,(L-1))
                          IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                    PXTRAY(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
                          IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                    PXTRAY(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
C
C       PICY(L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.2.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
C       PICY'(L)
                          PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/
     1                    (ALENS((WWVN),L)))*PXTRAY(7,L)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
C       WHEN ASTOP IS NOT ON SURFACE 1
C
 50                   CONTINUE
                      IF(JK.EQ.1) TMP15A=PXTRAY(5,(INT(SYSTEM1(26))))
                      IF(JK.EQ.2) TMP15B=PXTRAY(5,(INT(SYSTEM1(26))))
 60               CONTINUE
                  IF(TMP15A.EQ.TMP15B) THEN
                      OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
                      CALL SHOWIT(1)
                      OUTLYNE='APERTURE STOP SURFACE.'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      SYSTEM1(15)=((-.1D0*TMP15A)/(TMP15B-TMP15A))+SYSTEM1(15)
                  END IF
C
C       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
C       USING THIS VALUE OF SYSTEM1(15)
C
C               INITIAL VALUES AT SURFACE 0
C***************************************************************
C
C       CALL PIKRES FOR THE OBJECT SURFACE
                  COMI=0
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(0)=0,  ALWAYS
                  PXTRAY(1,0)=0.0D0
C
C       PUY(0)=SAY/TH(0)
                  PXTRAY(2,0)=(SYSTEM1(12))/ALENS(3,0)
C
C       PIY(0) =PUY(0)
                  PXTRAY(3,0)=PXTRAY(2,0)
C
C       PIY'(0)=PUY(0)
                  PXTRAY(4,0)=PXTRAY(3,0)
C
C       PCY(0) =-SCY
                  PXTRAY(5,0)=(SYSTEM1(14))
                  IF(SYSTEM1(14).EQ.0.0D0) PXTRAY(5,0)=1.0D0
C
C       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
C       SYSTEM1(15) IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
                  PXTRAY(6,0)=-((SYSTEM1(14))-SYSTEM1(15))/ALENS(3,0)
                  IF(SYSTEM1(14).EQ.0.0D0) PXTRAY(6,0)=
     1            -(1.0D0-SYSTEM1(15))/ALENS(3,0)
C
C       PICY(0) AT OBJECT, PICY = PUCY
                  PXTRAY(7,0)=PXTRAY(6,0)
C
C       PICY'(0) AT OBJECT PICY'=PICY
                  PXTRAY(8,0)=PXTRAY(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
                  L=1
                  SLV1=L
                  SLV2=1
                  IF(SOLVE(6,L).NE.0.0D0.OR.
     1            SOLVE(8,L).NE.0.0D0) CALL SLVRS
C
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                  COMI=1
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN SYSTEM1(12)
                  PXTRAY(1,1)=(SYSTEM1(12))
C
C       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(2,1)=-CURV*PXTRAY(1,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/
     4            (ALENS(WWVN,1)))*PXTRAY(2,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAY(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAY(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
C
C       PIY(1)=CV(1)*PY(1)+PUY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
C
C       PIY'(1)=(N/N')*PIY(1)
                  PXTRAY(4,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAY(3,1)
C
C       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                  PXTRAY(5,1)=SYSTEM1(15)
C
C       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(6,1)=-CURV*PXTRAY(5,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/
     4            (ALENS(WWVN,1)))*PXTRAY(6,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAY(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAY(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
C
C       PICY(1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
C       PICY'(1)
                  PXTRAY(8,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAY(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C *****************************************************************************
C       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
                  DO 70 L=2,INT(SYSTEM1(26))
                      SLV1=L
                      SLV2=1
                      IF(SOLVE(6,L).NE.0.0D0.OR.
     1                SOLVE(8,L).NE.0.0D0) CALL SLVRS
C               VALUES AT SURFACE L
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
                      PXTRAY(1,L)=PXTRAY(1,(L-1))+(ALENS(3,(L-1))*PXTRAY(2,(L-1)))
C
C       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(2,L)=-CURV*PXTRAY(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     3                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
C
C       PIY(L)=CV(1)*PY(L)+PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
C
C       PIY'(L)=(N/N')*PIY(L)
                      PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(3,L)
C
C       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
                      PXTRAY(5,L)=PXTRAY(5,(L-1))+(ALENS(3,(L-1))*PXTRAY(6,(L-1)))
C
C       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(6,L)=-CURV*PXTRAY(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
C
C       PICY(L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
C       PICY'(L)
                      PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(7,L)
C
C
 70               CONTINUE
C
C       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
C       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
C       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
C       HERE. WE HAVE DEFINED. THIS IS THE
C       APERTURE STOP SURFACE. REDEFINE IT AS L.
                  L=INT(SYSTEM1(26))
C       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE.
C       THIS IS DONE BY CALLING SUBROUTINE
                  SLV1=L
                  SLV2=1
                  IF(SOLVE(6,L).NE.0.0D0.OR.
     1            SOLVE(8,L).NE.0.0D0) CALL SLVRS
C
C               ALL APERTURE STOP SURFACE SOLVES NOW HANDLED.
C               PROCEED TO NEXT SURFACES
C
C*******************************************************************************
C       NOW TRACE FROM SURFACE AFTER THE ASTOP TO IMAGE PLANE,
C       CORRECTLY HANDLING SOLVES
C       AND PIKUPS ALONG THE WAY. THEN PROCEED. CURVATURE SOLVES
C       AFFECT THE CURVATURE OF THE SURFACE THAT THE SOLVES ARE ON.
C       THICKNESS SOLVES AFFECT THE DISTANCE FROM THE SURFACE THAT THE
C       SOLVES ARE ON TO THE NEXT SURFACE. BECAUSE OF THIS, CURVATURE
C       SOLVES MUST BE HANDLED FIRST FOLLOWED BY THICKNESS SOLVES.
C
C       NOW TRACE FROM THE APERTURE STOP SURFACE+1  (SYSTEM1(26)+1)
C       TO THE IMAGE SURFACE  WHERE:
                  DO 90 L=((INT(SYSTEM1(26)))+1),INT(SYSTEM1(20))
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C               VALUES AT SURFACE L
C
C       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
C       NOW CALCULATE PY VALUE
                      PXTRAY(1,L)=PXTRAY(1,(L-1))+(ALENS(3,(L-1))*PXTRAY(2,(L-1)))
C
C       FINISHED WITH PY(L)
C*******************************************************************************
C
C       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(2,L)=-CURV*PXTRAY(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
C
C       PIY(L)=CV(1)*PY(L)+PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
C
C       PIY'(L)=(N/N')*PIY(L)
                      PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(3,L)
C
C       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
C       NO CALCULATE PCY VALUE
                      PXTRAY(5,L)=PXTRAY(5,(L-1))+(ALENS(3,(L-1))*PXTRAY(6,(L-1)))
C
C       FINISHED WITH PCY(L)
C************************************************************************
C
C       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(6,L)=-CURV*PXTRAY(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
C
C       PICY(L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
C       PICY'(L)
                      PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(7,L)
C
C                       SOLVES ON SURFACE L
C***************************************************************************
C       HANDLE SOLVES ON SURFACE L BY CALLING SUBROUTINE
                      SLV1=L
                      SLV2=1
                      IF(SOLVE(6,L).NE.0.0D0.OR.
     1                SOLVE(8,L).NE.0.0D0) CALL SLVRS
C
C               NOW ALL SOLVES ON SURFACE L HAVE BEEN HANDLED
C
 90               CONTINUE
C       TRACE COMPLETED
********************************************************************************
              ELSE
C
C       NO ASTOP OR TEL SET, USE THE EXISTING VALUE OF SYSTEM1(15)
C
C*******************************************************************************
C       TRACE FROM THE OBJECT TO THE IMAGE, PROPERLY HANDLING ALL PIKUPS
C       AND SOLVES ALONG THE WAY. THEN PROCEED. NO SOLVES OCCUR ON SURFACES
C       0 OR 1.
C               INITIAL VALUES AT SURFACE 0
C       CALL PIKRES FOR THE OBJECT SURFACE
                  COMI=0
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C
C       PY(0)=0,  ALWAYS
                  PXTRAY(1,0)=0.0D0
C
C       PUY(0)=SAY/TH(0)
                  PXTRAY(2,0)=(SYSTEM1(12))/ALENS(3,0)
C
C       PIY(0) =PUY(0)
                  PXTRAY(3,0)=PXTRAY(2,0)
C
C       PIY'(0)=PUY(0)
                  PXTRAY(4,0)=PXTRAY(3,0)
C
C       PCY(0) =-SCY
                  PXTRAY(5,0)=(SYSTEM1(14))
                  IF(SYSTEM1(14).EQ.0.0D0) PXTRAY(5,0)=1.0D0
C
C       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
C       SYSTEM1(15) IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
                  IF(SYSTEM1(63).EQ.0.0D0)
     1            PXTRAY(6,0)=-((SYSTEM1(14))-SYSTEM1(15))/ALENS(3,0)
                  IF(SYSTEM1(14).EQ.0.0D0) PXTRAY(6,0)=
     1            -(1.0D0-SYSTEM1(15))/ALENS(3,0)
                  IF(SYSTEM1(63).EQ.1.0D0)
     1            PXTRAY(6,0)=0.0D0
C
C       PICY(0) AT OBJECT, PICY = PUCY
                  PXTRAY(7,0)=PXTRAY(6,0)
C
C       PICY'(0) AT OBJECT PICY'=PICY
                  PXTRAY(8,0)=PXTRAY(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                  COMI=1
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN SYSTEM1(12)
                  PXTRAY(1,1)=(SYSTEM1(12))
C
C       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(2,1)=-CURV*PXTRAY(1,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+
     4            ((ALENS(WWVN,0))/
     5            (ALENS(WWVN,1)))*PXTRAY(2,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAY(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAY(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
C
C       PIY(1)=CV(1)*PY(1)+PUY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
C
C       PIY'(1)=(N/N')*PIY(1)
                  PXTRAY(4,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAY(3,1)
C
C       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                  IF(SYSTEM1(63).EQ.0.0D0) PXTRAY(5,1)=SYSTEM1(15)
                  IF(SYSTEM1(63).EQ.1.0D0) PXTRAY(5,1)=PXTRAY(5,0)
C
C       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(6,1)=-CURV*PXTRAY(5,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+
     4            ((ALENS(WWVN,0))/
     5            (ALENS(WWVN,1)))*PXTRAY(6,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAY(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAY(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
C
C       PICY(1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
C       PICY'(1)
                  PXTRAY(8,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAY(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C
C       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
C       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
C       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
C       HERE. REDEFINE IT AS L.
                  L=1
C       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE
C       DO THIS BY CALLING SLVRS
                  SLV1=L
                  SLV2=1
                  IF(SOLVE(6,L).NE.0.0D0.OR.
     1            SOLVE(8,L).NE.0.0D0) CALL SLVRS
C               ALL APERTURE STOP SURFACE SOLVES NOW HANDLED.
C               PROCEED TO NEXT SURFACES
C *****************************************************************************
C       NOW TRACE FROM SURFACE 2 TO IMAGE PLANE,
C       CORRECTLY HANDLING SOLVES
C       AND PIKUPS ALONG THE WAY. THEN PROCEED. CURVATURE SOLVES
C       AFFECT THE CURVATURE OF THE SURFACE THAT THE SOLVES ARE ON.
C       THICKNESS SOLVES AFFECT THE DISTANCE FROM THE SURFACE THAT THE
C       SOLVES ARE ON TO THE NEXT SURFACE. BECAUSE OF THIS, CURVATURE
C       SOLVES MUST BE HANDLED FIRST FOLLOWED BY THICKNESS SOLVES.
                  DO 80 L=2,INT(SYSTEM1(20))
C               VALUES AT SURFACE L
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C*******************************************************************************
C       PY(L)=PY(L-1)+CV(L-1)*PUY(L-1)
C       NOW CALCULATE PY VALUE
                      PXTRAY(1,L)=PXTRAY(1,(L-1))+(ALENS(3,(L-1))*PXTRAY(2,(L-1)))
C
C       FINISHED WITH PY(L)
C
C*******************************************************************************
C
C       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(2,L)=-CURV*PXTRAY(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
C
C       PIY(L)=CV(1)*PY(L)+PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
C
C       PIY'(L)=(N/N')*PIY(L)
                      PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(3,L)
C
C       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
C*******************************************************************************
C       NO CALCULATE PCY VALUE
                      PXTRAY(5,L)=PXTRAY(5,(L-1))+(ALENS(3,(L-1))*PXTRAY(6,(L-1)))
C
C       FINISHED WITH PCY(L)
C************************************************************************
C
C       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(6,L)=-CURV*PXTRAY(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
C
C       PICY(L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
C       PICY'(L)
                      PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(7,L)
C
C                       NOW HANDLE SOLVES ON SURFACE L
C***************************************************************************
C       DO THIS BY CALLING SLVRS
                      SLV1=L
                      SLV2=1
                      IF(SOLVE(6,L).NE.0.0D0.OR.
     1                SOLVE(8,L).NE.0.0D0) CALL SLVRS
C       ALL SOLVES ON SURFACE L HANDLED
C
 80               CONTINUE
C       PARAXIAL TRACE COMPLETED
              END IF
C       NOW CALL ENEXES TO RESOLVE ANY ASTOP EN AND/OR EX
C       ENTRANCE/EXIT PUPIL ADJUSTMENTS.
C       THIS CALL IS ONLY MADE FROM PRTRA1 AND ONLY
C       USES YZ-PLANE DATA.
              CALL ENEXRS
C
          ELSE
C       ITYPEP NOT 1 OR 3
          END IF
C
C       SET CON = SYSTEM1(17)
          CON=SYSTEM1(17)
C
          IF(ITYPEP.EQ.2.OR.ITYPEP.EQ.3) THEN
C
C       THIS IS SUBROUTINE PRTRA2. THIS IS THE FIRST OF THE
C       SUBROUTINES WHICH IMPLEMENT THE PARAXIAL RAY TRACE.
C       IT IS CALLED FROM LNSEOS. THIS SUBROUTINE
C       TRACES ONLY IN THE XZ PLANE. PIKUPS ARE ALSO RESOLVED BY
C       CALLS TO PIKRES FOR EACH SURFACE FROM THIS
C       SUBROUTINE. THE XZ PLANE CROMATIC SURFACE COEFICIENTS
C       ARE CALCULATED WITH A CALL TO CCOLX.FOR
C
              SYS13=SYSTEM1(13)
C
C       THIS IS THE FIRST OF THE PARAXIAL RAY TRACING SUBROUTINES
C       IT IS AUTOMATICALLY CALLED FROM LNSEOS AFTER LENS INPUT
C       OR LENS UPDATE AND IS ALSO CALLED WHEN SPECIFIC PARAXIAL
C       DATA IS NEEDED WHICH IS NOT ALREADY ON-FILE IN THE
C       PARAXIAL RAYTRACE STORAGE ARRAY.
C
C       ALL XZ PLANE SOLVES ARE HANDLED THROUGH CALLS TO
C       SUBROUTINE SLVRSX
C
C       ALL PARAXIAL TRACING OPERATES ON THE CURRENT LENS
C       THIS COULD BE AN ALTERNATE CONFIGURATION.
C
C       IF AN APERTURE STOP IS DEFINED ON ANY SURFACE
C       THE VALUE OF SYSTEM1(17) NEEDS TO BE
C       REFINED.
C
C       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
C       UP TO THE APERTURE STOP SURFACE (WHEN THE APERTURE STOP
C       IS NOT ON SURFACE 1) USING TWO DIFFERENT VALUES
C       OF SYSTEM1(17) [HEIGTH OF CHIEF RAY AT SURF 1]
C
C       THE TWO VALUES USED ARE 0.0 AND 0.1
C
C       THE CORRECET VALUE OF SYSTEM1(17) WHICH MAKES PCX ON THE
C       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
C
C       PCX(AT ASTOP FOR SYSTEM1(17)=0.0) IS CALLED TMP17A
C       PCX(AT ASTOP FOR SYSTEM1(17)=0.1) IS CALLED TMP17B
C
C       SYSTEM1(17)=((-.1D0*TMP17A)/(TMP17B-TMP17A))+SYSTEM1(17)
C
              IF(SYSTEM1(26).GT.0.0D0.AND.SYSTEM1(63).EQ.0.0D0) THEN
C
C       RECALCULATE THE CORRECT VALUE OF SYSTEM1(17)
C       OTHERWISE, USE THE USER PROVIDED VALUE OF SYSTEM1(17)
C
C                       RAY
                  DO 6000 JK=1,2
                      IF(JK.EQ.1) CON=CON+0.0D0
                      IF(JK.EQ.2) CON=CON+0.1D0
C*************************************************************************
C       INITIAL TARGET OF RAY TRACE
C               THE INITIAL PARAXIAL RAYTRACE TARGETING
C               DATA IS:
C                       AT SURFACE 0 (OBJ)
C
C       STARTING MARGINAL RAY HEIGHT = 0
C       STARTING CHIEF RAY HEIGHT    = SCX
C                       AT SURFACE 1 (INITIAL REF SURF)
C       STARTING MARGINAL RAY HEIGHT = SAX
C
C       STARTING CHIEF RAY HEIGHT = CON
C
C               INITIAL VALUES AT SURFACE 0
C***************************************************************
C       CALL PIKRES FOR THE OBJECT SURFACE
                      COMI=0
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(0)=0,  ALWAYS
                      PXTRAX(1,0)=0.0D0
C
C       PUX(0)=SAX/TH(0)
C
                      IF(ALENS(3,0).EQ.0.0D0) THEN
                          OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      PXTRAX(2,0)=(SYS13)/ALENS(3,0)
C
C       PIX(0) =PUX(0)
                      PXTRAX(3,0)=PXTRAX(2,0)
C
C       PIX'(0)=PUX(0)
                      PXTRAX(4,0)=PXTRAX(3,0)
C
C       PCX(0) =-SCX
                      PXTRAX(5,0)=(SYSTEM1(16))
                      IF(SYSTEM1(16).EQ.0.0D0) PXTRAX(5,0)=1.0D0
C
C       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
C       CON IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
C
                      IF(ALENS(3,0).EQ.0.0D0) THEN
                          OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      PXTRAX(6,0)=-((SYSTEM1(16))-CON)/ALENS(3,0)
                      IF(SYSTEM1(16).EQ.0.0D0) PXTRAX(6,0)=
     1                -(1.0D0-CON)/ALENS(3,0)
C
C       PICX(0) AT OBJECT, PICX = PUCX
                      PXTRAX(7,0)=PXTRAX(6,0)
C
C       PICX'(0) AT OBJECT PICX'=PICX
                      PXTRAX(8,0)=PXTRAX(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
                      L=1
                      SLV1=L
                      SLV2=2
                      IF(SOLVE(4,L).NE.0.0D0.OR.
     1                SOLVE(2,L).NE.0.0D0) CALL SLVRS
C
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                      COMI=1
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                      PXTRAX(1,1)=(SYS13)
C

C       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
C
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.1.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAX(2,1)=-CURV*PXTRAX(1,1)*
     1                (((ALENS(WWVN,1))-
     2                (ALENS(WWVN,0)))/
     3                (ALENS(WWVN,1)))+
     4                ((ALENS(WWVN,0))/
     5                (ALENS(WWVN,1)))*PXTRAX(2,0)
                      IF(GLANAM(1,2).EQ.'PERFECT      ')
     1                PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
                      IF(GLANAM(1,2).EQ.'IDEAL        ')
     1                PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)

C
C       PIX(1)=CV(1)*PX(1)+PUX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.1.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
C
C       PIX'(1)=(N/N')*PIX(1)
                      PXTRAX(4,1)=((ALENS((WWVN),0))/
     1                (ALENS((WWVN),1)))*PXTRAX(3,1)
C
C       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                      PXTRAX(5,1)=CON
C
C       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.1.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAX(6,1)=-CURV*PXTRAX(5,1)*
     1                (((ALENS(WWVN,1))-
     2                (ALENS(WWVN,0)))/
     3                (ALENS(WWVN,1)))+
     4                ((ALENS(WWVN,0))/
     5                (ALENS(WWVN,1)))*PXTRAX(6,0)
                      IF(GLANAM(1,2).EQ.'PERFECT      ')
     1                PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
                      IF(GLANAM(1,2).EQ.'IDEAL        ')
     1                PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
C
C       PICY(1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.1.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
C       PICX'(1)
                      PXTRAX(8,1)=((ALENS((WWVN),0))/
     1                (ALENS((WWVN),1)))*PXTRAX(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C *****************************************************************************
C       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
                      DO 5000 L=2,INT(SYSTEM1(26))
                          SLV1=L
                          SLV2=2
                          IF(SOLVE(4,L).NE.0.0D0.OR.
     1                    SOLVE(2,L).NE.0.0D0) CALL SLVRS
C               VALUES AT SURFACE L
C       CALL PIKRES FOR THE SURFACE L
                          COMI=L
                          IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
                          PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
C
C       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.1.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAX(2,L)=-CURV*PXTRAX(1,L)*
     1                    (((ALENS(WWVN,L))-
     2                    (ALENS(WWVN,(L-1))))/
     3                    (ALENS(WWVN,L)))+
     4                    ((ALENS(WWVN,(L-1)))/
     5                    (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
                          IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                    PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
                          IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                    PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
C
C       PIX(L)=CV(1)*PX(L)+PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.1.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
C
C       PIX'(L)=(N/N')*PIX(L)
                          PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/
     1                    (ALENS((WWVN),L)))*PXTRAX(3,L)
C
C       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
                          PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
C
C       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.1.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAX(6,L)=-CURV*PXTRAX(5,L)*
     1                    (((ALENS(WWVN,L))-
     2                    (ALENS(WWVN,(L-1))))/
     3                    (ALENS(WWVN,L)))+
     4                    ((ALENS(WWVN,(L-1)))/
     5                    (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
                          IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                    PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
                          IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                    PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
C
C       PICX(L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.1.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
C       PICX'(L)
                          PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/
     1                    (ALENS((WWVN),L)))*PXTRAX(7,L)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
C       WHEN ASTOP IS NOT ON SURFACE 1
C
 5000                 CONTINUE
                      IF(JK.EQ.1) TMP17A=PXTRAX(5,(INT(SYSTEM1(26))))
                      IF(JK.EQ.2) TMP17B=PXTRAX(5,(INT(SYSTEM1(26))))
 6000             CONTINUE
                  IF(TMP17A.EQ.TMP17B) THEN
                      OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
                      CALL SHOWIT(1)
                      OUTLYNE='APERTURE STOP SURFACE.'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      SYSTEM1(17)=((-.1D0*TMP17A)/(TMP17B-TMP17A))+SYSTEM1(17)
                  END IF
C
C       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
C       USING THIS VALUE OF SYSTEM1(17)
C
C               INITIAL VALUES AT SURFACE 0
C***************************************************************
C
C       CALL PIKRES FOR THE OBJECT SURFACE
                  COMI=0
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(0)=0,  ALWAYS
                  PXTRAX(1,0)=0.0D0
C
C       PUX(0)=SAX/TH(0)
                  PXTRAX(2,0)=(SYS13)/ALENS(3,0)
C
C       PIX(0) =PUX(0)
                  PXTRAX(3,0)=PXTRAX(2,0)
C
C       PIX'(0)=PUX(0)
                  PXTRAX(4,0)=PXTRAX(3,0)
C
C       PCX(0) =-SCX
                  PXTRAX(5,0)=(SYSTEM1(16))
                  IF(SYSTEM1(16).EQ.0.0D0) PXTRAX(5,0)=1.0D0
C
C       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
C       SYSTEM1(17) IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
                  PXTRAX(6,0)=-((SYSTEM1(16))-SYSTEM1(17))/ALENS(3,0)
                  IF(SYSTEM1(16).EQ.0.0D0) PXTRAX(6,0)=
     1            -(1.0D0-SYSTEM1(17))/ALENS(3,0)
C
C       PICX(0) AT OBJECT, PICX = PUCX
                  PXTRAX(7,0)=PXTRAX(6,0)
C
C       PICX'(0) AT OBJECT PICX'=PICX
                  PXTRAX(8,0)=PXTRAX(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C

                  SLV1=L
                  SLV2=2
                  IF(SOLVE(4,L).NE.0.0D0.OR.
     1            SOLVE(2,L).NE.0.0D0) CALL SLVRS
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                  COMI=1
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                  PXTRAX(1,1)=(SYS13)
C
C       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(2,1)=-CURV*PXTRAX(1,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/
     4            (ALENS(WWVN,1)))*PXTRAX(2,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
C
C       PIX(1)=CV(1)*PX(1)+PUX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
C
C       PIX'(1)=(N/N')*PIX(1)
                  PXTRAX(4,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAX(3,1)
C
C       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                  PXTRAX(5,1)=SYSTEM1(17)
C
C       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(6,1)=-CURV*PXTRAX(5,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/
     4            (ALENS(WWVN,1)))*PXTRAX(6,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
C
C       PICX(1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
C       PICX'(1)
                  PXTRAX(8,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAX(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C *****************************************************************************
C       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
                  DO 7000 L=2,INT(SYSTEM1(26))
                      SLV1=L
                      SLV2=2
                      IF(SOLVE(4,L).NE.0.0D0.OR.
     1                SOLVE(2,L).NE.0.0D0) CALL SLVRS
C               VALUES AT SURFACE L
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
                      PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
C
C       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(2,L)=-CURV*PXTRAX(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     3                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
C
C       PIX(L)=CV(1)*PX(L)+PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
C
C       PIX'(L)=(N/N')*PIX(L)
                      PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(3,L)
C
C       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
                      PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
C
C       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(6,L)=-CURV*PXTRAX(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
C
C       PICX(L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
C       PICX'(L)
                      PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(7,L)
C
C
 7000             CONTINUE
C
C       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
C       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
C       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
C       HERE. WE HAVE DEFINED . THIS IS THE
C       APERTURE STOP SURFACE. REDEFINE IT AS L.
                  L=INT(SYSTEM1(26))
C       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE.
C       THIS IS DONE BY CALLING SUBROUTINE
C                               SLVRS
                  SLV1=L
                  SLV2=2
                  IF(SOLVE(4,L).NE.0.0D0.OR.
     1            SOLVE(2,L).NE.0.0D0) CALL SLVRS
C
C               ALL APERTURE STOP SURFACE SOLVES NOW HANDLED.
C               PROCEED TO NEXT SURFACES
C
C*******************************************************************************
C       NOW TRACE FROM SURFACE AFTER THE ASTOP TO IMAGE PLANE,
C       CORRECTLY HANDLING SOLVES
C       AND PIKUPS ALONG THE WAY. THEN PROCEED. CURVATURE SOLVES
C       AFFECT THE CURVATURE OF THE SURFACE THAT THE SOLVES ARE ON.
C       THICKNESS SOLVES AFFECT THE DISTANCE FROM THE SURFACE THAT THE
C       SOLVES ARE ON TO THE NEXT SURFACE. BECAUSE OF THIS, CURVATURE
C       SOLVES MUST BE HANDLED FIRST FOLLOWED BY THICKNESS SOLVES.
C
C       NOW TRACE FROM THE APERTURE STOP SURFACE+1  (SYSTEM+1)
C       TO THE IMAGE SURFACE  WHERE:
                  DO 9000 L=((INT(SYSTEM1(26)))+1),INT(SYSTEM1(20))
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C               VALUES AT SURFACE L
C
C       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
C       NOW CALCULATE PX VALUE
                      PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
C
C       FINISHED WITH PX(L)
C*******************************************************************************
C
C       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(2,L)=-CURV*PXTRAX(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
C
C       PIX(L)=CV(1)*PX(L)+PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
C
C       PIX'(L)=(N/N')*PIX(L)
                      PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(3,L)
C
C       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
C       NO CALCULATE PCX VALUE
                      PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
C
C       FINISHED WITH PCX(L)
C************************************************************************
C
C       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(6,L)=-CURV*PXTRAX(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
C
C       PICX(L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
C       PICX'(L)
                      PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(7,L)
C
C                       SOLVES ON SURFACE L
C***************************************************************************
C       HANDLE SOLVES ON SURFACE L BY CALLING SUBROUTINE
C                       SLVRS
                      SLV1=L
                      SLV2=2
                      IF(SOLVE(4,L).NE.0.0D0.OR.
     1                SOLVE(2,L).NE.0.0D0) CALL SLVRS
C
C               NOW ALL SOLVES ON SURFACE L HAVE BEEN HANDLED
C
 9000             CONTINUE
C       TRACE COMPLETED
********************************************************************************
              ELSE
C
C       NO ASTOP ASSIGNED OR TEL ON, USE THE EXISTING VALUE OF SYSTEM1(17)
C
C*******************************************************************************
C       TRACE FROM THE OBJECT TO THE IMAGE, PROPERLY HANDLING ALL PIKUPS
C       AND SOLVES ALONG THE WAY. THEN PROCEED. NO SOLVES OCCUR ON SURFACES
C       0 OR 1.
C               INITIAL VALUES AT SURFACE 0
C       CALL PIKRES FOR THE OBJECT SURFACE
                  COMI=0
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C
C       PX(0)=0,  ALWAYS
                  PXTRAX(1,0)=0.0D0
C
C       PUX(0)=SAX/TH(0)
                  PXTRAX(2,0)=(SYS13)/ALENS(3,0)
C
C       PIX(0) =PUX(0)
                  PXTRAX(3,0)=PXTRAX(2,0)
C
C       PIX'(0)=PUX(0)
                  PXTRAX(4,0)=PXTRAX(3,0)
C
C       PCX(0) =-SCX
                  PXTRAX(5,0)=(SYSTEM1(16))
                  IF(SYSTEM1(16).EQ.0.0D0) PXTRAX(5,0)=1.0D0
C
C       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
C       SYSTEM1(17) IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
                  IF(SYSTEM1(63).EQ.0.0D0)
     1            PXTRAX(6,0)=-((SYSTEM1(16))-SYSTEM1(17))/ALENS(3,0)
                  IF(SYSTEM1(16).EQ.0.0D0) PXTRAX(6,0)=
     1            -(1.0D0-SYSTEM1(17))/ALENS(3,0)
                  IF(SYSTEM1(63).EQ.1.0D0)
     1            PXTRAX(6,0)=0.0D0
C
C       PICX(0) AT OBJECT, PICX = PUCX
                  PXTRAX(7,0)=PXTRAX(6,0)
C
C       PICX'(0) AT OBJECT PICX'=PICX
                  PXTRAX(8,0)=PXTRAX(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C
                  L=1
                  SLV1=L
                  SLV2=2
                  IF(SOLVE(4,L).NE.0.0D0.OR.
     1            SOLVE(2,L).NE.0.0D0) CALL SLVRS
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                  COMI=1
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                  PXTRAX(1,1)=(SYS13)
C
C       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(2,1)=-CURV*PXTRAX(1,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+
     4            ((ALENS(WWVN,0))/
     5            (ALENS(WWVN,1)))*PXTRAX(2,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
C
C       PIX(1)=CV(1)*PX(1)+PUX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
C
C       PIX'(1)=(N/N')*PIX(1)
                  PXTRAX(4,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAX(3,1)
C
C       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                  IF(SYSTEM1(63).EQ.0.0D0) PXTRAX(5,1)=SYSTEM1(17)
                  IF(SYSTEM1(63).EQ.1.0D0) PXTRAX(5,1)=PXTRAX(5,0)
C
C       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(6,1)=-CURV*PXTRAX(5,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+
     4            ((ALENS(WWVN,0))/
     5            (ALENS(WWVN,1)))*PXTRAX(6,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
C
C       PICX(1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
C       PICX'(1)
                  PXTRAX(8,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAX(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C
C       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
C       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
C       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
C       HERE. REDEFINE IT AS L.
                  L=1
C       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE
C       DO THIS BY CALLING SLVRS
                  SLV1=L
                  SLV2=2
                  IF(SOLVE(4,L).NE.0.0D0.OR.
     1            SOLVE(2,L).NE.0.0D0) CALL SLVRS
C               ALL APERTURE STOP SURFACE SOLVES NOW HANDLED.
C               PROCEED TO NEXT SURFACES
C *****************************************************************************
C       NOW TRACE FROM SURFACE 2 TO IMAGE PLANE,
C       CORRECTLY HANDLING SOLVES
C       AND PIKUPS ALONG THE WAY. THEN PROCEED. CURVATURE SOLVES
C       AFFECT THE CURVATURE OF THE SURFACE THAT THE SOLVES ARE ON.
C       THICKNESS SOLVES AFFECT THE DISTANCE FROM THE SURFACE THAT THE
C       SOLVES ARE ON TO THE NEXT SURFACE. BECAUSE OF THIS, CURVATURE
C       SOLVES MUST BE HANDLED FIRST FOLLOWED BY THICKNESS SOLVES.
                  DO 8000 L=2,INT(SYSTEM1(20))
C               VALUES AT SURFACE L
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C*******************************************************************************
C       PX(L)=PX(L-1)+CV(L-1)*PUX(L-1)
C       NOW CALCULATE PX VALUE
                      PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
C
C       FINISHED WITH PX(L)
C
C*******************************************************************************
C
C       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(2,L)=-CURV*PXTRAX(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
C
C       PIX(L)=CV(1)*PX(L)+PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
C
C       PIX'(L)=(N/N')*PIX(L)
                      PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(3,L)
C
C       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
C*******************************************************************************
C       NOW CALCULATE PCX VALUE
                      PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
C
C       FINISHED WITH PCX(L)
C************************************************************************
C
C       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(6,L)=-CURV*PXTRAX(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
C
C       PICX(L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
C       PICX'(L)
                      PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(7,L)
C
C                       NOW HANDLE SOLVES ON SURFACE L
C***************************************************************************
C       DO THIS BY CALLING SLVRS
                      SLV1=L
                      SLV2=2
                      IF(SOLVE(4,L).NE.0.0D0.OR.
     1                SOLVE(2,L).NE.0.0D0) CALL SLVRS
C       ALL SOLVES ON SURFACE L HANDLED
C
C
 8000             CONTINUE
C       PARAXIAL TRACE COMPLETED
              END IF
C
          ELSE
C       ITYPEP NOT 2 OR 3)
          END IF
          RETURN

      END
C SUB PRTRB.FOR
      SUBROUTINE PRTRB
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRTRB. THIS IS THE
C       SUBROUTINE WHICH IMPLEMENTS PARAXIAL COLOR TRACE
C       THIS SUBROUTINE
C
          INTEGER ITYPEP
C
          COMMON/PTYPER/ITYPEP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       NOW CALL CCOLY.FOR WHICH CALCULATES THE
C       CHROMATIC ABERRATION SURFACE COEFICIENTS FROM THE YZ PLANE
C       PARAXIAL TRACE
C
          ITYPEP=1
          CALL CCOL
C
C       NOW CALL CCOLX.FOR TO CALCULATE THE XZ-PLANE CHROMATIC
C       ABERRATION SURFACE COEFICIENTS.
C
          ITYPEP=2
          CALL CCOL
C
          RETURN
      END


C SUB PRTRC.FOR
      SUBROUTINE PRTRC
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRTRC. THIS IS THE
C       SUBROUTINE WHICH IMPLEMENTS MONOCHROMATIC 3, 5 AND 7 ABERRATIONS
C       THIS SUBROUTINE
C
          INTEGER CW,AITYPE
C
          COMMON/PAS357/CW,AITYPE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          CW=INT(SYSTEM1(11))
          AITYPE=1
          CALL AB357
C
          CW=INT(SYSTEM1(11))
          AITYPE=2
          CALL AB357
          RETURN
      END


C SUB PRTRD.FOR
      SUBROUTINE PRTRD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PRTRD. THIS IS THE
C       SUBROUTINE WHICH IMPLEMENTS 3, 5 AND 7 COLOR CONTRIBUTIONS
C       THIS SUBROUTINE
C
          INTEGER CW,I,ITYP,AITYPE
C
          REAL*8 WV
C
          COMMON/PRCOM/WV,ITYP
C
          COMMON/PAS357/CW,AITYPE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       NOW CALL AB357 TO CALCULATE THIRD ORDER ABERRATIONS
C       AND THE CHROMATIC ABERRATION DIFFERENCES.
C
C       SAVE THE ORIGINAL PARAXIAL DATA
          I=INT(SYSTEM1(20))
          SAVE(1:8,0:I)=PXTRAY(1:8,0:I)
C
C       NOW CALL PRCOL(SYSTEM1(7),1)
          WV=SYSTEM1(7)
          ITYP=1
          CALL PRCOL
C       SWAP COLY INTO PXTRAY
          I=INT(SYSTEM1(20))
          PXTRAY(1:8,0:I)=COLY(1:8,0:I)
          CW=INT(SYSTEM1(7))
          AITYPE=1
          CALL AB357
C
C       SAVE THE DATA IN PDF3 AND PDF57
          I=INT(SYSTEM1(20))
          PDF3(1:10,0:I)=MAB3(1:10,0:I)
          PDF57(1:20,0:I)=MAB57(1:20,0:I)
C
C       NOW CALL PRCOL(SYSTEM1(8),1)
          WV=SYSTEM1(8)
          ITYP=1
          CALL PRCOL
C       SWAP COLY INTO PXTRAY
          I=INT(SYSTEM1(20))
          PXTRAY(1:8,0:I)=COLY(1:8,0:I)
C       CALL AB357 AGAIN
          CW=INT(SYSTEM1(8))
          AITYPE=1
          CALL AB357
C       SUBTRACT THE DATA IN PDF3 AND PDF57
          I=INT(SYSTEM1(20))
          PDF3(1:10,0:I)=PDF3(1:10,0:I)-MAB3(1:10,0:I)
          PDF57(1:20,0:I)=PDF57(1:20,0:I)-MAB57(1:20,0:I)
C       ALL OF THE PRIMARY CHROMATIC DIFFERENCES IN THE
C       YZ PLANE HAVE BEEN FORMED
C
C       NOW FOR THE SECONDARY DIFFERENCES
C
C       NOW CALL PRCOL(SYSTEM1(9),1)
          WV=SYSTEM1(9)
          ITYP=1
          CALL PRCOL
C       SWAP COLY INTO PXTRAY
          I=INT(SYSTEM1(20))
          PXTRAY(1:8,0:I)=COLY(1:8,0:I)
          CW=INT(SYSTEM1(9))
          AITYPE=1
          CALL AB357
C
C       SAVE THE DATA IN SDF3 AND SDF57
          I=INT(SYSTEM1(20))
          SDF3(1:10,0:I)=MAB3(1:10,0:I)
          SDF57(1:20,0:I)=MAB57(1:20,0:I)
C
C       NOW CALL PRCOL(SYSTEM1(10),1)
          WV=SYSTEM1(10)
          ITYP=1
          CALL PRCOL
C       SWAP COLY INTO PXTRAY
          I=INT(SYSTEM1(20))
          PXTRAY(1:8,I)=COLY(1:8,I)
C       CALL AB357 AGAIN
          CW=INT(SYSTEM1(10))
          AITYPE=1
          CALL AB357
C       SUBTRACT THE DATA IN PDF3 AND PDF57
          I=INT(SYSTEM1(20))
          SDF3(1:10,0:I)=SDF3(1:10,0:I)-MAB3(1:10,0:I)
          SDF57(1:20,0:I)=SDF57(1:20,0:I)-MAB57(1:20,0:I)
C       ALL OF THE PRIMARY CHROMATIC DIFFERENCES IN THE
C       YZ PLANE HAVE BEEN FORMED
C
C       RESTORE OLD YZ PARAXIAL TRACE DATA
C
          I=INT(SYSTEM1(20))
          PXTRAY(1:8,0:I)=SAVE(1:8,0:I)
          CW=INT(SYSTEM1(11))
C
C       SAVE THE ORIGINAL PARAXIAL DATA
          I=INT(SYSTEM1(20))
          SAVE(1:8,0:I)=PXTRAX(1:8,0:I)
C
C       NOW CALL PRCOL(SYSTEM1(7),2)
          WV=SYSTEM1(7)
          ITYP=2
          CALL PRCOL
C       SWAP COLX INTO PXTRAX
          I=INT(SYSTEM1(20))
          PXTRAX(1:8,0:I)=COLX(1:8,0:I)
          CW=INT(SYSTEM1(7))
          AITYPE=2
          CALL AB357
C
C       SAVE THE DATA IN XPDF3 AND XPDF57
          I=INT(SYSTEM1(20))
          XPDF3(1:10,0:I)=XMAB3(1:10,0:I)
          XPDF57(1:20,0:I)=XMAB57(1:20,0:I)
C
C       NOW CALL PRCOL(SYSTEM1(8),2)
          WV=SYSTEM1(8)
          ITYP=2
          CALL PRCOL
C       SWAP COLX INTO PXTRAX
          I=INT(SYSTEM1(20))
          PXTRAX(1:8,0:I)=COLX(1:8,0:I)
C             CALL AB357 AGAIN
          CW=INT(SYSTEM1(8))
          AITYPE=2
          CALL AB357
C       SUBTRACT THE DATA IN PDF3 AND PDF57
          I=INT(SYSTEM1(20))
          XPDF3(1:10,0:I)=XPDF3(1:10,0:I)-XMAB3(1:10,0:I)
          XPDF57(1:20,0:I)=XPDF57(1:20,0:I)-XMAB57(1:20,0:I)
C       ALL OF THE PRIMARY CHROMATIC DIFFERENCES IN THE
C       XZ PLANE HAVE BEEN FORMED
C
C       NOW FOR THE SECONDARY DIFFERENCES
C
C       NOW CALL PRCOL(SYSTEM1(9)2)
          WV=SYSTEM1(9)
          ITYP=2
          CALL PRCOL
C       SWAP COLY INTO PXTRAX
          I=INT(SYSTEM1(20))
          PXTRAX(1:8,0:I)=COLX(1:8,0:I)
          CW=INT(SYSTEM1(9))
          AITYPE=2
          CALL AB357
C       SAVE THE DATA IN XSDF3 AND XSDF57
          I=INT(SYSTEM1(20))
          XSDF3(1:10,0:I)=XMAB3(1:10,0:I)
          XSDF57(1:20,0:I)=XMAB57(1:20,0:I)
C
C       NOW CALL PRCOL(SYSTEM1(10),2)
          WV=SYSTEM1(10)
          ITYP=2
          CALL PRCOL
C       SWAP COLX INTO PXTRAX
          I=INT(SYSTEM1(20))
          PXTRAX(1:8,0:I)=COLX(1:8,0:I)
C       CALL AB357 AGAIN
          CW=INT(SYSTEM1(10))
          AITYPE=2
          CALL AB357
C       SUBTRACT THE DATA IN XPDF3 AND XPDF57
          I=INT(SYSTEM1(20))
          XSDF3(1:10,0:I)=XSDF3(1:10,0:I)-XMAB3(1:10,0:I)
          XSDF57(1:20,0:I)=XSDF57(1:20,0:I)-XMAB57(1:20,0:I)
C       ALL OF THE PRIMARY CHROMATIC DIFFERENCES IN THE
C       XZ PLANE HAVE BEEN FORMED
C
C       RESTORE OLD XZ PARAXIAL TRACE DATA AND CALL
C       AB357 FOR THE LAST TIME
C
          I=INT(SYSTEM1(20))
          PXTRAX(1:8,0:I)=SAVE(1:8,0:I)
          CW=INT(SYSTEM1(11))
          AITYPE=1
          CALL AB357
          RETURN
      END


C SUB PAROUT.FOR
      SUBROUTINE PAROUT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE PAROUT. THIS SUBROUTINE IMPLEMENTS
C       THE PARAXIAL RAY TRACE PRINTOUT COMMANDS.
C
C
          INTEGER SF,I,J
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        'PARAXIAL RAYTRACE OUTPUT COMMANDS ONLY TAKE QUALIFIER OR'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        'PARAXIAL RAYTRACE OUTPUT COMMANDS TAKE EITHER QUALIFIER OR'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #1 INPUT BUT NOT BOTH'
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
              IF(WC.EQ.'PXTX') WRITE(OUTLYNE,6001) INT(F12)
              IF(WC.EQ.'PXTY') WRITE(OUTLYNE,6004) INT(F12)
              IF(WC.EQ.'PITX') WRITE(OUTLYNE,6007) INT(F12)
              IF(WC.EQ.'PITY') WRITE(OUTLYNE,6010) INT(F12)
              IF(WC.EQ.'PRTX') WRITE(OUTLYNE,6013) INT(F12)
              IF(WC.EQ.'PRTY') WRITE(OUTLYNE,6016) INT(F12)
              CALL SHOWIT(0)
              IF(WC.EQ.'PXTX') WRITE(OUTLYNE,6002)
              IF(WC.EQ.'PXTY') WRITE(OUTLYNE,6005)
              IF(WC.EQ.'PITX') WRITE(OUTLYNE,6008)
              IF(WC.EQ.'PITY') WRITE(OUTLYNE,6011)
              IF(WC.EQ.'PRTX') WRITE(OUTLYNE,6014)
              IF(WC.EQ.'PRTY') WRITE(OUTLYNE,6017)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              IF(WC.EQ.'PXTX') WRITE(OUTLYNE,6000)
              IF(WC.EQ.'PXTY') WRITE(OUTLYNE,6003)
              IF(WC.EQ.'PITX') WRITE(OUTLYNE,6006)
              IF(WC.EQ.'PITY') WRITE(OUTLYNE,6009)
              IF(WC.EQ.'PRTX') WRITE(OUTLYNE,6012)
              IF(WC.EQ.'PRTY') WRITE(OUTLYNE,6015)
              CALL SHOWIT(0)
              DO I=0,SF
                  DO J=1,8
                      IF(DABS(PXTRAX(J,I)).LT.1.0D-14) PXTRAX(J,I)=0.0D0
                      IF(DABS(PXTRAY(J,I)).LT.1.0D-14) PXTRAY(J,I)=0.0D0
                  END DO
              END DO
              DO 10 I=0,SF
                  IF(WC.EQ.'PXTX') WRITE(OUTLYNE,2000)I,PXTRAX(1,I),PXTRAX(2,I)
     1              ,PXTRAX(5,I),PXTRAX(6,I)
                  IF(WC.EQ.'PXTY') WRITE(OUTLYNE,2000)I,PXTRAY(1,I),PXTRAY(2,I)
     1              ,PXTRAY(5,I),PXTRAY(6,I)
                  IF(WC.EQ.'PITX') WRITE(OUTLYNE,2000)I,PXTRAX(1,I),PXTRAX(3,I)
     1              ,PXTRAX(5,I),PXTRAX(7,I)
                  IF(WC.EQ.'PITY') WRITE(OUTLYNE,2000)I,PXTRAY(1,I),PXTRAY(3,I)
     1              ,PXTRAY(5,I),PXTRAY(7,I)
                  IF(WC.EQ.'PRTX') WRITE(OUTLYNE,2000)I,PXTRAX(1,I),PXTRAX(4,I)
     1              ,PXTRAX(5,I),PXTRAX(8,I)
                  IF(WC.EQ.'PRTY') WRITE(OUTLYNE,2000)I,PXTRAY(1,I),PXTRAY(4,I)
     1              ,PXTRAY(5,I),PXTRAY(8,I)
                  CALL SHOWIT(0)
 10           CONTINUE
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=0
              IF(WC.EQ.'PXTX')THEN
                  IF(HEADIN) THEN
                  END IF
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(2,SF)
     1             ,PXTRAX(5,SF),PXTRAX(6,SF)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PXTY')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6003)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(2,SF)
     1             ,PXTRAY(5,SF),PXTRAY(6,SF)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PITX')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6006)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(3,SF)
     1             ,PXTRAX(5,SF),PXTRAX(7,SF)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PITY')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6009)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(3,SF)
     1             ,PXTRAY(5,SF),PXTRAY(7,SF)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRTX')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6012)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(4,SF)
     1             ,PXTRAX(5,SF),PXTRAX(8,SF)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRTY')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6015)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(4,SF)
     1             ,PXTRAY(5,SF),PXTRAY(8,SF)
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'PXTX')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(2,SF)
     1             ,PXTRAX(5,SF),PXTRAX(6,SF)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PXTY')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6003)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(2,SF)
     1             ,PXTRAY(5,SF),PXTRAY(6,SF)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PITX')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6006)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(3,SF)
     1             ,PXTRAX(5,SF),PXTRAX(7,SF)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PITY')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6009)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(3,SF)
     1             ,PXTRAY(5,SF),PXTRAY(7,SF)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRTX')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6012)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(4,SF)
     1             ,PXTRAX(5,SF),PXTRAX(8,SF)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRTY')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6015)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(4,SF)
     1             ,PXTRAY(5,SF),PXTRAY(8,SF)
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'PXTX') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)I,PXTRAX(1,I),PXTRAX(2,I)
     1              ,PXTRAX(5,I),PXTRAX(6,I)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PXTY') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6003)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)I,PXTRAY(1,I),PXTRAY(2,I)
     1              ,PXTRAY(5,I),PXTRAY(6,I)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PITX') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6006)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)I,PXTRAX(1,I),PXTRAX(3,I)
     1              ,PXTRAX(5,I),PXTRAX(7,I)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PITY') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6009)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)I,PXTRAY(1,I),PXTRAY(3,I)
     1              ,PXTRAY(5,I),PXTRAY(7,I)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRTX') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6012)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)I,PXTRAX(1,I),PXTRAX(4,I)
     1              ,PXTRAX(5,I),PXTRAX(8,I)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRTY') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6015)
                  IF(HEADIN) CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'PRTY') THEN
                  WRITE(OUTLYNE,1500)I,PXTRAY(1,I),PXTRAY(4,I)
     1            ,PXTRAY(5,I),PXTRAY(8,I)
                  CALL SHOWIT(0)
              END IF
          END IF
 1500     FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6)
 2000     FORMAT(I3,1X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6)
C
C       PXTX
C
 6000     FORMAT('SURF',6X,'PX',12X,'PUX',13X,'PCX',11X,'PUCX')
 6001     FORMAT('PARAXIAL RAYTRACE DATA (XZ-PLANE)',
     1    ' - (CFG #',I2,')')
 6002     FORMAT(
     1    '(PUX AND PUCX) MEASURED WITH RESPECT TO THE Z-AXIS')
C
C       PXTY
C
 6003     FORMAT('SURF',6X,'PY',12X,'PUY',13X,'PCY',11X,'PUCY')
 6004     FORMAT('PARAXIAL RAYTRACE DATA (YZ-PLANE)',
     1    ' - (CFG #',I2,')')
 6005     FORMAT(
     1    '(PUY AND PUCY) MEASURED WITH RESPECT TO THE Z-AXIS')
 2501     FORMAT(1X)
C
C       PITX
C
 6006     FORMAT('SURF',6X,'PX',12X,'PIX',13X,'PCX',11X,'PICX')
 6007     FORMAT('PARAXIAL RAYTRACE DATA (XZ-PLANE)',
     1    ' - (CFG # ',I2,')')
 6008     FORMAT('(PIX AND PICX) - ANGLES OF INCIDENCE')
C
C       PITY
C
 6009     FORMAT('SURF',6X,'PY',12X,'PIY',13X,'PCY',11X,'PICY')
 6010     FORMAT('PARAXIAL RAYTRACE DATA (YZ-PLANE)',
     1    ' - (CFG # ',I2,')')
 6011     FORMAT('(PIY AND PICY) - ANGLES OF INCIDENCE')
C
C       PRTX
C
 6012     FORMAT('SURF',6X,'PX',12X,'PRX',13X,'PCX',11X,'PRCX')
 6013     FORMAT('PARAXIAL RAYTRACE DATA (XZ-PLANE)',
     1    ' - (CFG #',I2,')')
 6014     FORMAT('(PRX AND PRCX) - ANGLES OF REFRACTION/REFLECTION')
C
C       PRTY
C
 6015     FORMAT('SURF',6X,'PY',12X,'PRY',13X,'PCY',11X,'PRCY')
 6016     FORMAT('PARAXIAL RAYTRACE DATA (YZ-PLANE)',
     1    ' - (CFG #',I2,')')
 6017     FORMAT('(PRY AND PRCY) - ANGLES OF REFRACTION/REFLECTION')
      END
C SUB TR.FOR
      SUBROUTINE TR
C
          IMPLICIT NONE
C
C       THIS IS CALLED BY SUBROUTINE FADJ AND ERADJ.
C
          INTEGER JK,L,COMI
C
          COMMON/PIKCOM/COMI
C
          INTEGER ITYPEP
C
          COMMON/PTYPER/ITYPEP
C
C
          REAL*8 TMP15A,TMP15B,CON,CURV,SYS13,TMP17A,TMP17B
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
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
          CON=SYSTEM1(15)
C
          IF(ITYPEP.EQ.1) THEN
C       NOW WE PERFORM A PARAXIAL RAY TRACE WITHOUT AND SOLVES
C
C       IF AN APERTURE STOP IS DEFINED,
C       THE VALUE OF SYSTEM1(15) NEEDS TO BE
C       REFINED.
C
C       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
C       UP TO THE APERTURE STOP SURFACE
C       USING TWO DIFFERENT VALUES
C       OF SYSTEM1(15) [HEIGTH OF CHIEF RAY AT SURF 1]
C
C       THE TWO VALUES USED ARE 0.0 AND 0.1
C
C       THE CORRECET VALUE OF SYSTEM1(15) WHICH MAKES PCY ON THE
C       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
C
C       PCY(AT ASTOP FOR SYSTEM1(15)=0.0) IS CALLED TMP15A
C       PCY(AT ASTOP FOR SYSTEM1(15)=0.1) IS CALLED TMP15B
C
C       SYSTEM1(15)=((-.1*TMP15A)/(TMP15B-TMP15A))+SYSTEM1(15)
C
              IF(SYSTEM1(26).GT.0.0D0
     1        .AND.SYSTEM1(63).EQ.0.0D0) THEN
C
C       RECALCULATE THE CORRECT VALUE OF SYSTEM1(15)
C       OTHERWISE, USE THE USER PROVIDED VALUE OF SYSTEM1(15)
C
C                       RAY TARGETING INFORMATION
C
                  DO 60 JK=1,2
                      IF(JK.EQ.1) CON=CON+0.0D0
                      IF(JK.EQ.2) CON=CON+0.1D0
C*************************************************************************
C       INITIAL TARGET OF RAY TRACE
C               THE INITIAL PARAXIAL RAYTRACE TARGETING
C               DATA IS:
C                       AT SURFACE 0 (OBJ)
C
C       STARTING MARGINAL RAY HEIGHT = 0
C       STARTING CHIEF RAY HEIGHT    = SCY
C                       AT SURFACE 1 (INITIAL REF SURF)
C       STARTING MARGINAL RAY HEIGHT = SAY
C       STARTING CHIEF RAY HEIGHT = CON
C
C
C               INITIAL VALUES AT SURFACE 0
C***************************************************************
C       CALL PIKRES FOR THE OBJECT SURFACE
                      COMI=0
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(0)=0,  ALWAYS
                      PXTRAY(1,0)=0.0D0
C
C       PUY(0)=SAY/TH(0)
C
                      IF(ALENS(3,0).EQ.0.0D0) THEN
                          OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      PXTRAY(2,0)=(SYSTEM1(12))/ALENS(3,0)
C
C       PIY(0) =PUY(0)
                      PXTRAY(3,0)=PXTRAY(2,0)
C
C       PIY'(0)=PUY(0)
                      PXTRAY(4,0)=PXTRAY(3,0)
C
C       PCY(0) =-SCY
                      PXTRAY(5,0)=-(SYSTEM1(14))
C
C       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
C       CON IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
C
                      IF(ALENS(3,0).EQ.0.0D0) THEN
                          OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      PXTRAY(6,0)=-((SYSTEM1(14))-CON)/ALENS(3,0)
C
C       PICY(0) AT OBJECT, PICY = PUCY
                      PXTRAY(7,0)=PXTRAY(6,0)
C
C       PICY'(0) AT OBJECT PICY'=PICY
                      PXTRAY(8,0)=PXTRAY(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                      COMI=1
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN SYSTEM1(12)
                      PXTRAY(1,1)=(SYSTEM1(12))
C

C       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
C
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.2.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAY(2,1)=-CURV*PXTRAY(1,1)*
     1                (((ALENS(WWVN,1))-
     2                (ALENS(WWVN,0)))/
     3                (ALENS(WWVN,1)))+
     4                ((ALENS(WWVN,0))/
     5                (ALENS(WWVN,1)))*PXTRAY(2,0)
                      IF(GLANAM(1,2).EQ.'PERFECT      ')
     1                PXTRAY(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
                      IF(GLANAM(1,2).EQ.'IDEAL        ')
     1                PXTRAY(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(2,1-1)

C
C       PIY(1)=CV(1)*PY(1)+PUY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.2.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
C
C       PIY'(1)=(N/N')*PIY(1)
                      PXTRAY(4,1)=((ALENS((WWVN),0))/
     1                (ALENS((WWVN),1)))*PXTRAY(3,1)
C
C       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                      PXTRAY(5,1)=CON
C
C       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.2.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAY(6,1)=-CURV*PXTRAY(5,1)*
     1                (((ALENS(WWVN,1))-
     2                (ALENS(WWVN,0)))/
     3                (ALENS(WWVN,1)))+
     4                ((ALENS(WWVN,0))/
     5                (ALENS(WWVN,1)))*PXTRAY(6,0)
                      IF(GLANAM(1,2).EQ.'PERFECT      ')
     1                PXTRAY(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(6,1-1)
                      IF(GLANAM(1,2).EQ.'IDEAL        ')
     1                PXTRAY(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(6,1-1)
C
C       PICY(1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.2.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
C       PICY'(1)
                      PXTRAY(8,1)=((ALENS((WWVN),0))/
     1                (ALENS((WWVN),1)))*PXTRAY(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C *****************************************************************************
C       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
                      DO 50 L=2,INT(SYSTEM1(26))
C               VALUES AT SURFACE L
C       CALL PIKRES FOR THE SURFACE L
                          COMI=L
                          IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
                          PXTRAY(1,L)=PXTRAY(1,(L-1))+(ALENS(3,(L-1))*PXTRAY(2,(L-1)))
C
C       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.2.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAY(2,L)=-CURV*PXTRAY(1,L)*
     1                    (((ALENS(WWVN,L))-
     2                    (ALENS(WWVN,(L-1))))/
     3                    (ALENS(WWVN,L)))+
     4                    ((ALENS(WWVN,(L-1)))/
     5                    (ALENS(WWVN,L)))*PXTRAY(2,(L-1))
                          IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                    PXTRAY(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
                          IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                    PXTRAY(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(2,L-1)

C
C       PIY(L)=CV(1)*PY(L)+PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.2.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
C
C       PIY'(L)=(N/N')*PIY(L)
                          PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/
     1                    (ALENS((WWVN),L)))*PXTRAY(3,L)
C
C       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
                          PXTRAY(5,L)=PXTRAY(5,(L-1))+(ALENS(3,(L-1))*PXTRAY(6,(L-1)))
C
C       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.2.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAY(6,L)=-CURV*PXTRAY(5,L)*
     1                    (((ALENS(WWVN,L))-
     2                    (ALENS(WWVN,(L-1))))/
     3                    (ALENS(WWVN,L)))+
     4                    ((ALENS(WWVN,(L-1)))/
     5                    (ALENS(WWVN,L)))*PXTRAY(6,(L-1))
                          IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                    PXTRAY(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(6,L-1)
                          IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                    PXTRAY(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(6,L-1)
C
C       PICY(L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.2.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
C       PICY'(L)
                          PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/
     1                    (ALENS((WWVN),L)))*PXTRAY(7,L)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
C
 50                   CONTINUE
                      IF(JK.EQ.1) TMP15A=PXTRAY(5,(INT(SYSTEM1(26))))
                      IF(JK.EQ.2) TMP15B=PXTRAY(5,(INT(SYSTEM1(26))))
 60               CONTINUE
                  IF(TMP15A.EQ.TMP15B) THEN
                      OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
                      CALL SHOWIT(1)
                      OUTLYNE='APERTURE STOP SURFACE.'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      SYSTEM1(15)=((-.1*TMP15A)/(TMP15B-TMP15A))+SYSTEM1(15)
                  END IF
C
C       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
C       USING THIS VALUE OF SYSTEM1(15)
C
C               INITIAL VALUES AT SURFACE 0
C***************************************************************
C
C       CALL PIKRES FOR THE OBJECT SURFACE
                  COMI=0
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(0)=0,  ALWAYS
                  PXTRAY(1,0)=0.0D0
C
C       PUY(0)=SAY/TH(0)
                  PXTRAY(2,0)=(SYSTEM1(12))/ALENS(3,0)
C
C       PIY(0) =PUY(0)
                  PXTRAY(3,0)=PXTRAY(2,0)
C
C       PIY'(0)=PUY(0)
                  PXTRAY(4,0)=PXTRAY(3,0)
C
C       PCY(0) =-SCY
                  PXTRAY(5,0)=-(SYSTEM1(14))
C
C       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
C       SYSTEM1(15) IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
                  PXTRAY(6,0)=-((SYSTEM1(14))-SYSTEM1(15))/ALENS(3,0)
C
C       PICY(0) AT OBJECT, PICY = PUCY
                  PXTRAY(7,0)=PXTRAY(6,0)
C
C       PICY'(0) AT OBJECT PICY'=PICY
                  PXTRAY(8,0)=PXTRAY(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                  COMI=1
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN SYSTEM1(12)
                  PXTRAY(1,1)=(SYSTEM1(12))
C
C       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(2,1)=-CURV*PXTRAY(1,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/
     4            (ALENS(WWVN,1)))*PXTRAY(2,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAY(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAY(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
C
C       PIY(1)=CV(1)*PY(1)+PUY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
C
C       PIY'(1)=(N/N')*PIY(1)
                  PXTRAY(4,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAY(3,1)
C
C       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                  PXTRAY(5,1)=SYSTEM1(15)
C
C       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(6,1)=-CURV*PXTRAY(5,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/
     4            (ALENS(WWVN,1)))*PXTRAY(6,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAY(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(6,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAY(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(6,1-1)
C
C       PICY(1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
C       PICY'(1)
                  PXTRAY(8,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAY(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C *****************************************************************************
C       NOW TRACE TO THE APERTURE STOP SURFACE WHERE:
                  DO 70 L=2,INT(SYSTEM1(26))
C               VALUES AT SURFACE L
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
                      PXTRAY(1,L)=PXTRAY(1,(L-1))+(ALENS(3,(L-1))*PXTRAY(2,(L-1)))
C
C       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(2,L)=-CURV*PXTRAY(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     3                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
C
C       PIY(L)=CV(1)*PY(L)+PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
C
C       PIY'(L)=(N/N')*PIY(L)
                      PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(3,L)
C
C       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
                      PXTRAY(5,L)=PXTRAY(5,(L-1))+(ALENS(3,(L-1))*PXTRAY(6,(L-1)))
C
C       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(6,L)=-CURV*PXTRAY(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(6,L-1)
C
C       PICY(L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
C       PICY'(L)
                      PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(7,L)
C
C
 70               CONTINUE
C
C       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
C       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
C       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
C       HERE. THIS IS THE
C       APERTURE STOP SURFACE. REDEFINE IT AS L.
                  L=INT(SYSTEM1(26))
C       PROCEED TO NEXT SURFACES
C
C       NOW TRACE FROM SURFACE AFTER THE ASTOP TO IMAGE PLANE,
C       AND PIKUPS ALONG THE WAY. THEN PROCEED.
C       NOW TRACE FROM THE APERTURE STOP SURFACE+1  (SYSTEM1(26)+1)
C       TO THE IMAGE SURFACE  WHERE:
                  DO 90 L=((INT(SYSTEM1(26)))+1),INT(SYSTEM1(20))
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C               VALUES AT SURFACE L
C
C       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
C       NOW CALCULATE PY VALUE
                      PXTRAY(1,L)=PXTRAY(1,(L-1))+(ALENS(3,(L-1))*PXTRAY(2,(L-1)))
C
C       FINISHED WITH PY(L)
C**********************************************************************************
C
C       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(2,L)=-CURV*PXTRAY(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
C
C       PIY(L)=CV(1)*PY(L)+PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
C
C       PIY'(L)=(N/N')*PIY(L)
                      PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(3,L)
C
C       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
C       NO CALCULATE PCY VALUE
                      PXTRAY(5,L)=PXTRAY(5,(L-1))+(ALENS(3,(L-1))*PXTRAY(6,(L-1)))
C
C       FINISHED WITH PCY(L)
C************************************************************************
C
C       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(6,L)=-CURV*PXTRAY(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(6,L-1)
C
C       PICY(L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
C       PICY'(L)
                      PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(7,L)
C
 90               CONTINUE
C       TRACE COMPLETED
*******************************************************************************
              ELSE
C
C       NO ASTOP WAS DEFINED OR TEL ON.
C       IF NO ASTOP IS DEFINED, SYSTEM1(15) IS USED AS IT WAS STORED
C       DURING LENS INPUT.
C******************************************************************************
C       TRACE FROM THE OBJECT TO THE IMAGE, PROPERLY HANDLING ALL PIKUPS
C       AND SOLVES ALONG THE WAY. THEN PROCEED. NO SOLVES OCCUR ON SURFACES
C       0 OR 1.
C               INITIAL VALUES AT SURFACE 0
C       CALL PIKRES FOR THE OBJECT SURFACE
                  COMI=0
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C
C       PY(0)=0,  ALWAYS
                  PXTRAY(1,0)=0.0D0
C
C       PUY(0)=SAY/TH(0)
                  PXTRAY(2,0)=(SYSTEM1(12))/ALENS(3,0)
C
C       PIY(0) =PUY(0)
                  PXTRAY(3,0)=PXTRAY(2,0)
C
C       PIY'(0)=PUY(0)
                  PXTRAY(4,0)=PXTRAY(3,0)
C
C       PCY(0) =-SCY
                  PXTRAY(5,0)=-(SYSTEM1(14))
C
C       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
C       SYSTEM1(15) IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
                  IF(SYSTEM1(63).EQ.0.0D0)
     1            PXTRAY(6,0)=-((SYSTEM1(14))-SYSTEM1(15))/ALENS(3,0)
                  IF(SYSTEM1(63).EQ.1.0D0)
     1            PXTRAY(6,0)=0.0D0
C
C       PICY(0) AT OBJECT, PICY = PUCY
                  PXTRAY(7,0)=PXTRAY(6,0)
C
C       PICY'(0) AT OBJECT PICY'=PICY
                  PXTRAY(8,0)=PXTRAY(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                  COMI=1
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN SYSTEM1(12)
                  PXTRAY(1,1)=(SYSTEM1(12))
C
C       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(2,1)=-CURV*PXTRAY(1,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+
     4            ((ALENS(WWVN,0))/
     5            (ALENS(WWVN,1)))*PXTRAY(2,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAY(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAY(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
C
C       PIY(1)=CV(1)*PY(1)+PUY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
C
C       PIY'(1)=(N/N')*PIY(1)
                  PXTRAY(4,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAY(3,1)
C
C       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                  IF(SYSTEM1(63).EQ.0.0D0) PXTRAY(5,1)=SYSTEM1(15)
                  IF(SYSTEM1(63).EQ.1.0D0) PXTRAY(5,1)=PXTRAY(5,0)
C
C       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(6,1)=-CURV*PXTRAY(5,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+
     4            ((ALENS(WWVN,0))/
     5            (ALENS(WWVN,1)))*PXTRAY(6,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAY(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(6,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAY(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(6,1-1)
C
C       PICY(1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.2.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
C       PICY'(1)
                  PXTRAY(8,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAY(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C
C       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
C       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
C       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
C       HERE. REDEFINE IT AS L.
                  L=1
C       NOW TRACE FROM SURFACE 2 TO IMAGE PLANE,
C       CORRECTLY HANDLING SOLVES
C       AND PIKUPS ALONG THE WAY. THEN PROCEED.
                  DO 80 L=2,INT(SYSTEM1(20))
C               VALUES AT SURFACE L
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C******************************************************************************
C       PY(L)=PY(L-1)+CV(L-1)*PUY(L-1)
C       NOW CALCULATE PY VALUE
                      PXTRAY(1,L)=PXTRAY(1,(L-1))+(ALENS(3,(L-1))*PXTRAY(2,(L-1)))
C
C       FINISHED WITH PY(L)
C
C******************************************************************************
C
C       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(2,L)=-CURV*PXTRAY(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
C
C       PIY(L)=CV(1)*PY(L)+PUY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
C
C       PIY'(L)=(N/N')*PIY(L)
                      PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(3,L)
C
C       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
C******************************************************************************
C       NO CALCULATE PCY VALUE
                      PXTRAY(5,L)=PXTRAY(5,(L-1))+(ALENS(3,(L-1))*PXTRAY(6,(L-1)))
C
C       FINISHED WITH PCY(L)
C************************************************************************
C
C       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(6,L)=-CURV*PXTRAY(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAY(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAY(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(6,L-1)
C
C       PICY(L)
C       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.2.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
C       PICY'(L)
                      PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAY(7,L)
C
 80               CONTINUE
C       PARAXIAL TRACE COMPLETED
              END IF
              RETURN
          ELSE
C       ITYPEP NOT 1
          END IF
          CON=SYSTEM1(17)
          IF(ITYPEP.EQ.2) THEN
C
              SYS13=SYSTEM1(13)
C
C       NOW WE PERFORM A PARAXIAL RAY TRACE WITHOUT AND SOLVES
C
C       IF AN APERTURE STOP IS DEFINED
C       , THE VALUE OF SYSTEM1(17) NEEDS TO BE
C       REFINED.
C
C       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
C       UP TO THE APERTURE STOP SURFACE
C       USING TWO DIFFERENT VALUES
C       OF SYSTEM1(17) [HEIGTH OF CHIEF RAY AT SURF 1]
C
C       THE TWO VALUES USED ARE 0.0 AND 0.1
C
C       THE CORRECET VALUE OF SYSTEM1(17) WHICH MAKES PCX ON THE
C       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
C
C       PCX(AT ASTOP FOR SYSTEM1(17)=0.0) IS CALLED TMP17A
C       PCX(AT ASTOP FOR SYSTEM1(17)=0.1) IS CALLED TMP17B
C
C       SYSTEM1(17)=((-.1*TMP17A)/(TMP17B-TMP17A))+SYSTEM1(17)
C
              IF(SYSTEM1(26).GT.0.0D0.AND.SYSTEM1(63).EQ.0.0D0) THEN
C
C       RECALCULATE THE CORRECT VALUE OF SYSTEM1(17)
C       OTHERWISE, USE THE USER PROVIDED VALUE OF SYSTEM1(17)
C
C                       RAY
                  DO 600 JK=1,2
                      IF(JK.EQ.1) CON=CON+0.0D0
                      IF(JK.EQ.2) CON=CON+0.1D0
C*************************************************************************
C       INITIAL TARGET OF RAY TRACE
C               THE INITIAL PARAXIAL RAYTRACE TARGETING
C               DATA IS:
C                       AT SURFACE 0 (OBJ)
C
C       STARTING MARGINAL RAY HEIGHT = 0
C       STARTING CHIEF RAY HEIGHT    = SCX
C                       AT SURFACE 1 (INITIAL REF SURF)
C       STARTING MARGINAL RAY HEIGHT = SAX
C       STARTING CHIEF RAY HEIGHT = CON
C
C
C               INITIAL VALUES AT SURFACE 0
C***************************************************************
C       CALL PIKRES FOR THE OBJECT SURFACE
                      COMI=0
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(0)=0,  ALWAYS
                      PXTRAX(1,0)=0.0D0
C
C       PUX(0)=SAX/TH(0)
C
                      IF(ALENS(3,0).EQ.0.0D0) THEN
                          OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      PXTRAX(2,0)=(SYS13)/ALENS(3,0)
C
C       PIX(0) =PUX(0)
                      PXTRAX(3,0)=PXTRAX(2,0)
C
C       PIX'(0)=PUX(0)
                      PXTRAX(4,0)=PXTRAX(3,0)
C
C       PCX(0) =-SCX
                      PXTRAX(5,0)=-(SYSTEM1(16))
C
C       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
C       CON IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
C
                      IF(ALENS(3,0).EQ.0.0D0) THEN
                          OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      PXTRAX(6,0)=-((SYSTEM1(16))-CON)/ALENS(3,0)
C
C       PICX(0) AT OBJECT, PICX = PUCX
                      PXTRAX(7,0)=PXTRAX(6,0)
C
C       PICX'(0) AT OBJECT PICX'=PICX
                      PXTRAX(8,0)=PXTRAX(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                      COMI=1
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                      PXTRAX(1,1)=(SYS13)
C

C       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
C
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.1.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAX(2,1)=-CURV*PXTRAX(1,1)*
     1                (((ALENS(WWVN,1))-
     2                (ALENS(WWVN,0)))/
     3                (ALENS(WWVN,1)))+
     4                ((ALENS(WWVN,0))/
     5                (ALENS(WWVN,1)))*PXTRAX(2,0)
                      IF(GLANAM(1,2).EQ.'PERFECT      ')
     1                PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
                      IF(GLANAM(1,2).EQ.'IDEAL        ')
     1                PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)

C
C       PIX(1)=CV(1)*PX(1)+PUX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.1.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
C
C       PIX'(1)=(N/N')*PIX(1)
                      PXTRAX(4,1)=((ALENS((WWVN),0))/
     1                (ALENS((WWVN),1)))*PXTRAX(3,1)
C
C       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                      PXTRAX(5,1)=CON
C
C       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.1.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAX(6,1)=-CURV*PXTRAX(5,1)*
     1                (((ALENS(WWVN,1))-
     2                (ALENS(WWVN,0)))/
     3                (ALENS(WWVN,1)))+
     4                ((ALENS(WWVN,0))/
     5                (ALENS(WWVN,1)))*PXTRAX(6,0)
                      IF(GLANAM(1,2).EQ.'PERFECT      ')
     1                PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(6,1-1)
                      IF(GLANAM(1,2).EQ.'IDEAL        ')
     1                PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(6,1-1)
C
C       PICY(1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,1).EQ.1.0D0) THEN
                          CURV=ALENS(24,1)
                      ELSE
                          IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                              CURV=ALENS(43,1)*2.0D0
                          ELSE
                              CURV=ALENS(1,1)
                          END IF
                      END IF
                      PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
C       PICX'(1)
                      PXTRAX(8,1)=((ALENS((WWVN),0))/
     1                (ALENS((WWVN),1)))*PXTRAX(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C *****************************************************************************
C       NOW TRACE TO THE APERTURE STOP SURFACE WHERE:
                      DO 500 L=2,INT(SYSTEM1(26))
C               VALUES AT SURFACE L
C       CALL PIKRES FOR THE SURFACE L
                          COMI=L
                          IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
                          PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
C
C       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.1.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAX(2,L)=-CURV*PXTRAX(1,L)*
     1                    (((ALENS(WWVN,L))-
     2                    (ALENS(WWVN,(L-1))))/
     3                    (ALENS(WWVN,L)))+
     4                    ((ALENS(WWVN,(L-1)))/
     5                    (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
                          IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                    PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
                          IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                    PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)

C
C       PIX(L)=CV(1)*PX(L)+PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.1.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
C
C       PIX'(L)=(N/N')*PIX(L)
                          PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/
     1                    (ALENS((WWVN),L)))*PXTRAX(3,L)
C
C       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
                          PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
C
C       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.1.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAX(6,L)=-CURV*PXTRAX(5,L)*
     1                    (((ALENS(WWVN,L))-
     2                    (ALENS(WWVN,(L-1))))/
     3                    (ALENS(WWVN,L)))+
     4                    ((ALENS(WWVN,(L-1)))/
     5                    (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
                          IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                    PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(6,L-1)
                          IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                    PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(6,L-1)
C
C       PICX(L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                          IF(ALENS(23,L).EQ.1.0D0) THEN
                              CURV=ALENS(24,L)
                          ELSE
                              IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                                  CURV=ALENS(43,L)*2.0D0
                              ELSE
                                  CURV=ALENS(1,L)
                              END IF
                          END IF
                          PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
C       PICX'(L)
                          PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/
     1                    (ALENS((WWVN),L)))*PXTRAX(7,L)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
C
 500                  CONTINUE
                      IF(JK.EQ.1) TMP17A=PXTRAX(5,(INT(SYSTEM1(26))))
                      IF(JK.EQ.2) TMP17B=PXTRAX(5,(INT(SYSTEM1(26))))
 600              CONTINUE
                  IF(TMP17A.EQ.TMP17B) THEN
                      OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
                      CALL SHOWIT(1)
                      OUTLYNE='APERTURE STOP SURFACE.'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      SYSTEM1(17)=((-.1*TMP17A)/(TMP17B-TMP17A))+SYSTEM1(17)
                  END IF
C
C       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
C       USING THIS VALUE OF SYSTEM1(17)
C
C               INITIAL VALUES AT SURFACE 0
C***************************************************************
C
C       CALL PIKRES FOR THE OBJECT SURFACE
                  COMI=0
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(0)=0,  ALWAYS
                  PXTRAX(1,0)=0.0D0
C
C       PUX(0)=SAX/TH(0)
                  PXTRAX(2,0)=(SYS13)/ALENS(3,0)
C
C       PIX(0) =PUX(0)
                  PXTRAX(3,0)=PXTRAX(2,0)
C
C       PIX'(0)=PUX(0)
                  PXTRAX(4,0)=PXTRAX(3,0)
C
C       PCX(0) =-SCX
                  PXTRAX(5,0)=-(SYSTEM1(16))
C
C       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
C       SYSTEM1(17) IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
                  PXTRAX(6,0)=-((SYSTEM1(16))-SYSTEM1(17))/ALENS(3,0)
C
C       PICX(0) AT OBJECT, PICX = PUCX
                  PXTRAX(7,0)=PXTRAX(6,0)
C
C       PICX'(0) AT OBJECT PICX'=PICX
                  PXTRAX(8,0)=PXTRAX(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                  COMI=1
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                  PXTRAX(1,1)=(SYS13)
C
C       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(2,1)=-CURV*PXTRAX(1,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/
     4            (ALENS(WWVN,1)))*PXTRAX(2,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
C
C       PIX(1)=CV(1)*PX(1)+PUX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
C
C       PIX'(1)=(N/N')*PIX(1)
                  PXTRAX(4,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAX(3,1)
C
C       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                  PXTRAX(5,1)=SYSTEM1(17)
C
C       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(6,1)=-CURV*PXTRAX(5,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/
     4            (ALENS(WWVN,1)))*PXTRAX(6,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(6,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(6,1-1)
C
C       PICX(1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
C       PICX'(1)
                  PXTRAX(8,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAX(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C *****************************************************************************
C       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
                  DO 700 L=2,INT(SYSTEM1(26))
C               VALUES AT SURFACE L
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
                      PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
C
C       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(2,L)=-CURV*PXTRAX(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     3                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
C
C       PIX(L)=CV(1)*PX(L)+PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
C
C       PIX'(L)=(N/N')*PIX(L)
                      PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(3,L)
C
C       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
                      PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
C
C       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(6,L)=-CURV*PXTRAX(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(6,L-1)
C
C       PICX(L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
C       PICX'(L)
                      PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(7,L)
C
C
 700              CONTINUE
C
C       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
C       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
C       WE HAVE DEFINED. THIS IS THE
C       APERTURE STOP SURFACE. REDEFINE IT AS L.
C       NOW TRACE FROM SURFACE AFTER THE ASTOP TO IMAGE PLANE.
C
C       NOW TRACE FROM THE APERTURE STOP SURFACE+1
C       TO THE IMAGE SURFACE  WHERE:
                  DO 900 L=((INT(SYSTEM1(26)))+1),INT(SYSTEM1(20))
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C               VALUES AT SURFACE L
C
C       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
C       NOW CALCULATE PX VALUE
                      PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
C
C       FINISHED WITH PX(L)
C**********************************************************************************
C
C       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(2,L)=-CURV*PXTRAX(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
C
C       PIX(L)=CV(1)*PX(L)+PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
C
C       PIX'(L)=(N/N')*PIX(L)
                      PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(3,L)
C
C       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
C       NO CALCULATE PCX VALUE
                      PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
C
C       FINISHED WITH PCX(L)
C************************************************************************
C
C       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(6,L)=-CURV*PXTRAX(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(6,L-1)
C
C       PICX(L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
C       PICX'(L)
                      PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(7,L)
C
 900              CONTINUE
C       TRACE COMPLETED
*******************************************************************************
              ELSE
C
C       NO ASTOP WAS DEFINED OR TEL ON.
C       IF NO ASTOP IS DEFINED, SYSTEM1(17) IS USED AS IT WAS STORED
C       DURING LENS INPUT.
C******************************************************************************
C       TRACE FROM THE OBJECT TO THE IMAGE.
C               INITIAL VALUES AT SURFACE 0
C       CALL PIKRES FOR THE OBJECT SURFACE
                  COMI=0
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C
C       PX(0)=0,  ALWAYS
                  PXTRAX(1,0)=0.0D0
C
C       PUX(0)=SAX/TH(0)
                  PXTRAX(2,0)=(SYS13)/ALENS(3,0)
C
C       PIX(0) =PUX(0)
                  PXTRAX(3,0)=PXTRAX(2,0)
C
C       PIX'(0)=PUX(0)
                  PXTRAX(4,0)=PXTRAX(3,0)
C
C       PCX(0) =-SCX
                  PXTRAX(5,0)=-(SYSTEM1(16))
C
C       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
C       SYSTEM1(17) IS CHIEF RAY POSITION ON SURFACE 1
C       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
                  IF(SYSTEM1(63).EQ.0.0D0)
     1            PXTRAX(6,0)=-((SYSTEM1(16))-SYSTEM1(17))/ALENS(3,0)
                  IF(SYSTEM1(63).EQ.1.0D0)
     1            PXTRAX(6,0)=0.0D0
C
C       PICX(0) AT OBJECT, PICX = PUCX
                  PXTRAX(7,0)=PXTRAX(6,0)
C
C       PICX'(0) AT OBJECT PICX'=PICX
                  PXTRAX(8,0)=PXTRAX(7,0)
C
C       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
C
C               INITIAL VALUES AT SURFACE 1
C       CALL PIKRES FOR THE SURFACE 1
                  COMI=1
                  IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C
C       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                  PXTRAX(1,1)=(SYS13)
C
C       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(2,1)=-CURV*PXTRAX(1,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+
     4            ((ALENS(WWVN,0))/
     5            (ALENS(WWVN,1)))*PXTRAX(2,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
C
C       PIX(1)=CV(1)*PX(1)+PUX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
C
C       PIX'(1)=(N/N')*PIX(1)
                  PXTRAX(4,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAX(3,1)
C
C       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                  IF(SYSTEM1(63).EQ.0.0D0) PXTRAX(5,1)=SYSTEM1(17)
                  IF(SYSTEM1(63).EQ.1.0D0) PXTRAX(5,1)=PXTRAX(5,0)
C
C       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(6,1)=-CURV*PXTRAX(5,1)*
     1            (((ALENS(WWVN,1))-
     2            (ALENS(WWVN,0)))/
     3            (ALENS(WWVN,1)))+
     4            ((ALENS(WWVN,0))/
     5            (ALENS(WWVN,1)))*PXTRAX(6,0)
                  IF(GLANAM(1,2).EQ.'PERFECT      ')
     1            PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(6,1-1)
                  IF(GLANAM(1,2).EQ.'IDEAL        ')
     1            PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(6,1-1)
C
C       PICX(1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                  IF(ALENS(23,1).EQ.1.0D0) THEN
                      CURV=ALENS(24,1)
                  ELSE
                      IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                          CURV=ALENS(43,1)*2.0D0
                      ELSE
                          CURV=ALENS(1,1)
                      END IF
                  END IF
                  PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
C       PICX'(1)
                  PXTRAX(8,1)=((ALENS((WWVN),0))/
     1            (ALENS((WWVN),1)))*PXTRAX(7,1)
C
C       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
C
C       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
C       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
C       NOW TRACE FROM SURFACE 2 TO IMAGE PLANE,
                  DO 800 L=2,INT(SYSTEM1(20))
C               VALUES AT SURFACE L
C       CALL PIKRES FOR THE SURFACE L
                      COMI=L
                      IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
C******************************************************************************
C       PX(L)=PX(L-1)+CV(L-1)*PUX(L-1)
C       NOW CALCULATE PX VALUE
                      PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
C
C       FINISHED WITH PX(L)
C
C******************************************************************************
C
C       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(2,L)=-CURV*PXTRAX(1,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
C
C       PIX(L)=CV(1)*PX(L)+PUX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
C
C       PIX'(L)=(N/N')*PIX(L)
                      PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(3,L)
C
C       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
C******************************************************************************
C       NOW CALCULATE PCX VALUE
                      PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
C
C       FINISHED WITH PCX(L)
C************************************************************************
C
C       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(6,L)=-CURV*PXTRAX(5,L)*
     1                (((ALENS(WWVN,L))-
     2                (ALENS(WWVN,(L-1))))/
     3                (ALENS(WWVN,L)))+
     4                ((ALENS(WWVN,(L-1)))/
     5                (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
                      IF(GLANAM(L,2).EQ.'PERFECT      ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(6,L-1)
                      IF(GLANAM(L,2).EQ.'IDEAL        ')
     1                PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(6,L-1)
C
C       PICX(L)
C       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
C       ELSE SET CURV=ALENS(1,-)
                      IF(ALENS(23,L).EQ.1.0D0) THEN
                          CURV=ALENS(24,L)
                      ELSE
                          IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                              CURV=ALENS(43,L)*2.0D0
                          ELSE
                              CURV=ALENS(1,L)
                          END IF
                      END IF
                      PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
C       PICX'(L)
                      PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/
     1                (ALENS((WWVN),L)))*PXTRAX(7,L)
C
 800              CONTINUE
C       PARAXIAL TRACE COMPLETED
              END IF
              RETURN
          ELSE
C       ITYPEP NOT 2
          END IF
      END
