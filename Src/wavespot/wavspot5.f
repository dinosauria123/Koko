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

C SUB HITEX1.FOR
      SUBROUTINE HITEX1
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE HITEX1.FOR. THIS SUBROUTINE IS
C       CALLED BY EXFIT. IT CALCULATES OTHER RAY AND REFERENCE RAY
C       INTERSECTIONS WITH A TANGENT PLANE TO THE EXIT PUPIL
C       IT ALSO CALCULATES THE REFERENCE SURFACE RADIUS OF CURVATURE
C       THIS WAS DERIVED FROM LOPD.FOR ON 1/11/94
C
          DOUBLE PRECISION XREFI,XO,XOOY,XOOX,YO,YOOX,YOOY,ZO,ZOOX,ZOOY,
     1    YREFI,RAD,LO,LOOX,LOOY,MO,MOOX,MOOY,
     2    ZREFI,RL0,RM0,RN0,NO,NOOX,NOOY,THYNUM,THXNUM,
     3    RRX0,RRZ0,T,RX0,RY0,RZ0,RRY0,M0,L0,N0,THXDEN,THYDEN,V1,V2,V3,
     4    ZB,ZA,ZAR,ZBR,RADD,V4,V5,V6

          COMMON/HITME1/ZA,ZB,ZAR,ZBR,RADD,V1,V2,V3,V4,V5,V6
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
C
C       CASE OF AFOCAL SYSTEMS
C     SET UP RAYS
          RX0=REFRY(1,NEWIMG)
          RY0=REFRY(2,NEWIMG)
          RZ0=REFRY(3,NEWIMG)
          RL0=REFRY(19,NEWIMG)
          RM0=REFRY(20,NEWIMG)
          RN0=REFRY(21,NEWIMG)
C       NOW THE OTHER RAY:
          RRX0=V1
          RRY0=V2
          RRZ0=V3
          L0=V4
          M0=V5
          N0=V6
C
          IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
C
C       FLAT REFERENCE PLANE
C       THE SO-CALLED FLAT REFERENCE PLANE IS A PLANE
C       PERPENDICULAR TO THE REF RAY. THE INTERSECTION
C       OF THIS REFERENCE PLANE WITH THE REFERENCE RAY OCCURRS
C       AT THE FINAL SURFACE (NEWOBJ). WITH THIS DEFINITION,
C       THE CORRECTION TERM FOR THE REFERENCE RAY IS ALWAYS
C       ZERO.
              RADD=1.0D10
C
C       THE PLANE WHICH HAS ITS ORIGIN WHERE THE
C       REFERENCE RAY INTERSECTS THE IMAGE PLANE
C       AND IS PERPENDICULAR TO THE REFERENCE RAY HAS THE FORM:
C
C       (RL0*(X-RX0))+(RM0*(Y-RY0))+(RN0*(Z-RZ0))=0
C       X,Y AND Z ARE ANY POINTS ON THE PLANE.
C
C       NOW DETERMINE THE INTERSECTION OF AN "OTHER" RAY
C       WITH THIS REFERENCE PLANE.
C
C       THE EQUATION OF THE OTHER RAY IS
C               ZA=RRX0+(T*L0)
C               ZB=RRY0+(T*M0)
C THE PARAMETER T IS DETERMINED BY SUBSTITUTING THIS LINE
C       EQUATION INTO THE REFERENCE SURAFCE EQUATION
C       AND SOLVING FOR T.
C
              T=((RL0*(RX0-RRX0))+(RM0*(RY0-RRY0))+(RN0*(RZ0-RRZ0)))/
     1        ((RL0*L0)+(RM0*M0)+(RN0*N0))
              IF(T.GT.0.0D0.AND.RADD.EQ.1.0D10) T=1.0D10
              IF(T.LT.0.0D0.AND.RADD.EQ.1.0D10) T=-1.0D10
C
C       NOW X,Y AND Z ARE:
              ZA=RRX0+(T*L0)
              ZB=RRY0+(T*M0)
C     THE COORDINATES ON THE CHIEF RAY INTERSECTION WITH THE
C     EXIT PUPIL ARE:
              ZAR=RX0+(T*RL0)
              ZBR=RY0+(T*RM0)
C     DONE
              ZA=ZA-ZAR
              ZB=ZB-ZBR
              RETURN
          ELSE
          END IF
C     AFOCAL DONE
C
C
C
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C
C       SYSTEM FOCAL.
              IF(.NOT.EXPAUT.OR.EXPAUT.AND..NOT.LDIF2) THEN
C
C     CASE OF NOT THE REAL RAY EXIT PUPIL
C
C       THE REFERENCE SPHERE IS CENTERED WHERE THE REAL CHIEF
C       RAY CROSSES THE FINAL SURFACE.
C       THE RADIUS IS EQUAL
C       TO THE DISTANCE FROM THE FINAL IMAGE SURFACE TO THE SURFACE
C       PRECEEDING THE IMAGE SURFACE IF THE I-1 THICKNESS IS NON-ZERO
C       AND IS THE DISTANCE TO THE PARAXIAL EXIT PUPIL IF TH (I-1)=0
C       THIS IS JUST AS IN ACCOS V AND
C       HEXAGON AND GIVES THE USER THE CHOICE FOR RADIUS. BY USING
C       PIKUPS (NOT AVAILABLE IN CODE V), A WIDE VARIETY OF REFERENCE
C       SURFACES MAY BE SELECTED.
C
C     IF EXPAUT AND NOT LDIF2, AN ASTOP EX IS DONE EVEN IF THE I-1
C     THICKNESS IS NOT ZERO.
C
C       COORDINATES OF THE REAL CHIEF RAY AT THE IMAGE PLANE ARE:
C       THIS IS THE REFERENCE SPHERE CENTER LOCATION.
                  XREFI=REFRY(1,NEWIMG)
                  YREFI=REFRY(2,NEWIMG)
                  ZREFI=REFRY(3,NEWIMG)
                  IF(ALENS(3,(NEWIMG-1)).EQ.0.0D0.OR.ALENS(3,(NEWIMG-1)).NE.
     1            0.0D0.AND..NOT.LDIF2) THEN
C       DIST FROM I-1 TO I IS ZERO OR LDIF2 IS FALSE AND DIST NOT 0
C       DO AN INTERNAL "ASTOP EX" ADJUSTMENT
                      IF(DABS(PXTRAY(5,NEWIMG)*1.0D-10).LT.
     1                DABS(PXTRAY(6,NEWIMG))) THEN
                          RAD=(PXTRAY(5,NEWIMG)/PXTRAY(6,NEWIMG))
                          RADD=DABS(RAD)
                      ELSE
                      END IF
                      IF(DABS(PXTRAY(5,NEWIMG)*1.0D-10).GE.
     1                DABS(PXTRAY(6,NEWIMG))) THEN
C       REF RAY IS TELECENTRIC, SET RAD INFINITY
C
                          RADD=1.0D10
C
C       THE LINE DIRECTION NUMBERS ARE:
C
C       THE PLANE WHICH HAS ITS ORIGIN WHERE THE
C       REFERENCE RAY INTERSECTS THE IMAGE PLANE
C       AND IS PERPENDICULAR TO THE REFERENCE RAY HAS THE FORM:
C
C       (RL0*(X-RX0))+(RM0*(Y-RY0))+(RN0*(Z-RZ0))=0
C       X,Y AND Z ARE ANY POINTS ON THE PLANE.
C
C       NOW DETERMINE THE INTERSECTION OF AN "OTHER" RAY
C       WITH THIS REFERENCE PLANE.
C
C       THE EQUATION OF THE OTHER RAY IS
C               ZA=RRX0+(T*L0)
C               ZB=RRY0+(T*M0)
C THE PARAMETER T IS DETERMINED BY SUBSTITUTING THIS LINE
C       EQUATION INTO THE REFERENCE SURAFCE EQUATION
C       AND SOLVING FOR T.
C
                          T=((RL0*(RX0-RRX0))+(RM0*(RY0-RRY0))+(RN0*(RZ0-RRZ0)))/
     1                    ((RL0*L0)+(RM0*M0)+(RN0*N0))
                          IF(T.GT.0.0D0.AND.RADD.EQ.1.0D10) T=1.0D10
                          IF(T.LT.0.0D0.AND.RADD.EQ.1.0D10) T=-1.0D10
C
C       NOW X,Y AND Z ARE:
                          ZA=RRX0+(T*L0)
                          ZB=RRY0+(T*M0)
C     THE COORDINATES ON THE CHIEF RAY INTERSECTION WITH THE
C     EXIT PUPIL ARE:
                          ZAR=RX0+(T*RL0)
                          ZBR=RY0+(T*RM0)
                          ZA=ZA-ZAR
                          ZB=ZB-ZBR
                          RETURN
                      ELSE
                      END IF
                  ELSE
C       USE EXISTING THICKNESS OF NEWIMG-1 AS RAD
                      RAD=ALENS(3,NEWIMG-1)
                      RADD=DABS(RAD)
                  END IF
C     IF RAD IS POSITIVE, THE EXIT PUPIL LIES ON THE NEGATIVE
C     SIDE OF THE IMAGE PLANE. IF RAD IS NEGATIVE, THE EXIT PUPIL
C     LIES ON THE POSITIVE SIDE OF THE IMAGE PLANE.
C     NOW CALCULATE ZAR,ZBR AT THE PARAXIAL EXIT PUPIL
C     THE COORDINATES ON THE CHIEF RAY INTERSECTION WITH THE
C     EXIT PUPIL ARE:
                  ZAR=RX0+(-RAD*RL0)
                  ZBR=RY0+(-RAD*RM0)
C     THEN CALCULATE THE OTEHER RAY INTERSECTION WITH THE EXIT PUPIL
C     TANGENT PLANE
C
C       NOW X,Y AND Z ARE:
                  ZA=RRX0+(-RAD*L0)
                  ZB=RRY0+(-RAD*M0)
                  ZA=ZA-ZAR
                  ZB=ZB-ZBR
                  RETURN
              ELSE
C     EXPAUT NOT FALSE
              END IF
C
C
              IF(EXPAUT) THEN
C
C     CASE OF THE REAL RAY EXIT PUPIL
C
C       THE REFERENCE SPHERE IS CENTERED WHERE THE REAL CHIEF
C       RAY CROSSES THE FINAL SURFACE.
C       THE RADIUS IS EQUAL
C       TO THE DISTANCE FROM POINT TO WHERE THE CHIEF RAY CROSSES THE REAL
C     EXIT PUPIL
C       THIS IS JUST AS IN CODE-V AND IS THE DEFAULT WHEN THE PROGRAM
C     BEGINS
C
C       COORDINATES OF THE REAL CHIEF RAY AT THE IMAGE PLANE ARE:
C       THIS IS THE REFERENCE SPHERE CENTER LOCATION.
                  IF(.NOT.CENCEN) THEN
                      XREFI=REFRY(1,NEWIMG)
                      YREFI=REFRY(2,NEWIMG)
                      ZREFI=REFRY(3,NEWIMG)
                  ELSE
                      XREFI=CENTX
                      YREFI=CENTY
                      ZREFI=REFRY(3,NEWIMG)
                  END IF
C
C       DO A REAL RAY INTERNAL EXIT PUPIL ADJUSTMENT
C
                  XO=REFRY(1,NEWIMG)
                  YO=REFRY(2,NEWIMG)
                  ZO=REFRY(3,NEWIMG)
                  LO=REFRY(4,NEWIMG)
                  MO=REFRY(5,NEWIMG)
                  NO=REFRY(6,NEWIMG)
                  XOOX=RFDIFF(1,NEWIMG)
                  YOOX=RFDIFF(2,NEWIMG)
                  ZOOX=RFDIFF(3,NEWIMG)
                  LOOX=RFDIFF(4,NEWIMG)
                  MOOX=RFDIFF(5,NEWIMG)
                  NOOX=RFDIFF(6,NEWIMG)
                  XOOY=RFDIFF(7,NEWIMG)
                  YOOY=RFDIFF(8,NEWIMG)
                  ZOOY=RFDIFF(9,NEWIMG)
                  LOOY=RFDIFF(10,NEWIMG)
                  MOOY=RFDIFF(11,NEWIMG)
                  NOOY=RFDIFF(12,NEWIMG)
                  THXNUM=((XO-XOOX)*(LO-LOOX))+((YO-YOOX)*(MO-MOOX))
     1            +((ZO-ZOOX)*(NO-NOOX))
                  THYNUM=((XO-XOOY)*(LO-LOOY))+((YO-YOOY)*(MO-MOOY))
     1            +((ZO-ZOOY)*(NO-NOOY))
                  THXDEN=((LO-LOOX)**2)+((MO-MOOX)**2)
     1            +((NO-NOOX)**2)
                  THYDEN=((LO-LOOY)**2)+((MO-MOOY)**2)
     1            +((NO-NOOY)**2)
                  IF(DABS(THXNUM*1.0D-10).LT.DABS(THXDEN).AND.
     1            DABS(THYNUM*1.0D-10).LT.
     1            DABS(THYDEN)) THEN
                      RAD=-((-THXNUM/THXDEN)+(-THYNUM/THYDEN))/2.0D0
C     THE EXIT PUPIL IS LOCATED AT:
C     IN THE COORDINATE SYSTEM OF THE IMAGE SURFACE
                      RADD=DABS(RAD)
                      ZAR=RX0+(-RAD*RL0)
                      ZBR=RY0+(-RAD*RM0)
                      ZA=RRX0+(-RAD*L0)
                      ZB=RRY0+(-RAD*M0)
                      ZA=ZA-ZAR
                      ZB=ZB-ZBR
                      RETURN
                  ELSE
                  END IF
                  IF(DABS(THXNUM*1.0D-10).GE.DABS(THXDEN).OR.
     1            DABS(THYNUM*1.0D-10).GE.
     1            DABS(THYDEN)) THEN
C       REF RAY IS TELECENTRIC, SET RAD INFINITY
C       FLAT REFERENCE PLANE
C       THE SO-CALLED FLAT REFERENCE PLANE IS A PLANE
C       PERPENDICULAR TO THE REF RAY. THE INTERSECTION
C       OF THIS REFERENCE PLANE WITH THE REFERENCE RAY OCCURRS
C       AT THE FINAL SURFACE (NEWIMG).
                      RADD=1.0D10
                      RAD=1.0D10
                      ZAR=RX0+(-RAD*RL0)
                      ZBR=RX0+(-RAD*RM0)
                      ZA=RRX0+(-RAD*L0)
                      ZB=RRY0+(-RAD*M0)
                      ZA=ZA-ZAR
                      ZB=ZB-ZBR
                      RETURN
                  ELSE
                  END IF
              ELSE
C     EXPAUT NOT TRUE
              END IF
              RETURN
          ELSE
C     NOT FOCAL
          END IF
          RETURN
      END
C       FIFTH FILE OF CAPFN/SPOT ROUTINES

C SUB APSTREHL.FOR
      SUBROUTINE APSTREHL
          IMPLICIT NONE
          REAL*8 STREHLR,SUML2,SUML4,SUMSIG,WEI(1:10),WEIS
     1    ,SIG,LAM(1:10),STVALUE
          INTEGER I
          LOGICAL STERROR
          LOGICAL NOCOBSPSF
          COMMON/PSFCOBS/NOCOBSPSF
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          IF(WC.EQ.'STREHL') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"STREHL" CALCULATES EXACT STREHL RATIO FOR CURRENT LEN'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='"STREHL" TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'ACC'.AND.WC.EQ.'STREHL'.AND.
     1        WQ.NE.'NOOB'.AND.WQ.NE.'ACCNOOB') THEN
                  OUTLYNE='ONLY "ACC", "NOOB" AND "ACCNOOB" CAN BE ISSUED'
                  CALL SHOWIT(1)
                  OUTLYNE='AS OPTIONAL QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='WITH "STREHL"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(.NOT.REFEXT) THEN
                  OUTLYNE='NO REFERENCE RAY DATA EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='THE "STREHL" COMMAND CAN NOT BE EXECUTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              STERROR=.FALSE.
              STVALUE=0.0D0
              NOCOBSPSF=.FALSE.
              IF(WQ.EQ.'NOOB'.OR.WQ.EQ.'ACCNOOB') NOCOBSPSF=.TRUE.
              CALL TSTREHL(STERROR,STVALUE)
              IF(STERROR) THEN
                  OUTLYNE='THE CURRENT "STREHL" CALCULATION FAILED'
                  CALL SHOWIT(1)
                  OUTLYNE='NO CAPFN OR PSF COULD BE COMPUTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              REG(40)=REG(9)
              REG(9)=STVALUE
              RSTREHL=STVALUE
              RSTREHL_EXIST=.TRUE.
              IF(WQ.NE.'ACC'.AND.WQ.NE.'ACCNOOB') THEN
                  WRITE(OUTLYNE,9000) REG(9)
                  CALL SHOWIT(0)
              END IF
 9000         FORMAT('THE CURRENT EXACT STREHL RATIO = ',G15.8)
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'VAR')
     1        OUTLYNE='"VAR" RETURNS WAVEFRONT VARIENCE FOR CURRENT CAPFN'
              IF(WC.EQ.'APSTREHL')
     1        OUTLYNE=
     1        '"APSTREHL" CALCULATES APPROX. STREHL RATIO FOR CURRENT CAPFN'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1.AND.WC.EQ.'VAR') THEN
              OUTLYNE='"VAR" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              IF(WC.EQ.'APSTREHL')
     1        OUTLYNE='"APSTREHL" TAKES NO NUMERIC OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ACC'.AND.WC.EQ.'APSTREHL') THEN
              OUTLYNE='ONLY "ACC" CAN BE ISSUED AS AN OPTIONAL QUALIFIER'
              CALL SHOWIT(1)
              OUTLYNE='WITH "APSTREHL"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.CPFNEXT) THEN
              IF(WC.EQ.'VAR')
     1        OUTLYNE='"VAR" REQUIRES AN EXISTING CAPFN'
              IF(WC.EQ.'APSTREHL')
     1        OUTLYNE='"APSTREHL" REQUIRES AN EXISTING CAPFN'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'VAR') THEN
              REG(40)=REG(9)
              REG(9)=(RMSOPD**2)
              WRITE(OUTLYNE,1000) REG(9)
              CALL SHOWIT(0)
 1000         FORMAT('THE CURRENT CAPFN WAVEFRONT VARIENCE = ',G15.8,' WAVES')
              RETURN
          END IF
          IF(WC.EQ.'APSTREHL') THEN
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
              DO I=1,10
                  WEIS=WEIS+WEI(I)
              END DO
              IF(WEIS.EQ.0.0D0) WEIS=1.0D0
              DO I=1,10
                  WEI(I)=WEI(I)/WEIS
              END DO
              DO I=1,10
                  IF(LAM(I).NE.0.0D0.AND.WEI(I).NE.0.0D0) THEN
                      SUML2=SUML2+(WEI(I)/(LAM(I)**2))
                      SUML4=SUML4+(WEI(I)/(LAM(I)**4))
                  END IF
              END DO
              IF(SUML2.NE.0.0D0.AND.SUML4.NE.0.0D0)
     1        LAMAVE=DSQRT(SUML2/SUML4)
              IF(LAMAVE.NE.0.0D0) THEN
C     CALC SUMSIG
                  SUMSIG=0.0D0
                  DO I=1,10
                      IF(WEI(I).NE.0.0D0.AND.RMSOP(I).NE.0.0D0.AND.
     1                LAM(I).NE.0.0D0)
     1                SUMSIG=SUMSIG+((WEI(I)*(RMSOP(I)**2))/(LAM(I)**2))
                  END DO
              END IF
              SIG=0.0D0
              IF(SUML2.NE.0.0D0.AND.SUMSIG.NE.0.0D0)
     1        SIG=DSQRT(SUMSIG/SUML2)
              STREHLR=DEXP(-((TWOPII*SIG)**2))
              REG(40)=REG(9)
              REG(9)=STREHLR
              REG(10)=LAMAVE
              IF(WQ.NE.'ACC') THEN
                  WRITE(OUTLYNE,2000) REG(9)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000) REG(10)
                  CALL SHOWIT(0)
              END IF
              RETURN
 2000         FORMAT('THE CURRENT CAPFN STREHL RATIO = ',G15.8)
 3000         FORMAT('AT THE WEIGHTED INTEGRATED WAVELENGTH = ',G15.8,'MICROMETER')
          END IF
C
C
      END
      SUBROUTINE TSTREHL(STERROR,STVALUE)
          IMPLICIT NONE
          REAL*8 STVALUE
          LOGICAL STERROR,NOCOBSPSF
          COMMON/PSFCOBS/NOCOBSPSF
          INTEGER TES
          COMMON/TESSET/TES
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          SAVE_KDP(1)=SAVEINPT(1)
          IF(PSFHC) INPUT='PSFPLOT HCONLY'
          IF(.NOT.PSFHC) INPUT='PSFPLOT NO'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          DOSTREHL=.TRUE.
          MSGSPD=.FALSE.
          TES=0
          IF(NOCOBSPSF) TES=1
          IF(TES.EQ.1) NOCOBSPSF=.FALSE.
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PSF'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          IF(TES.EQ.1) NOCOBSPSF=.TRUE.
          TES=0
          IF(.NOT.PSFEXT.OR..NOT.CPFNEXT) THEN
              STERROR=.TRUE.
              STVALUE=0.0D0
              MSGSPD=.TRUE.
              DOSTREHL=.FALSE.
              IF(.NOT.PSFHC) THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='PSFPLOT YES'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
              END IF
              RETURN
          END IF
          STVALUE=GPREG(101)
          MSGSPD=.FALSE.
          DOSTREHL=.TRUE.

          SAVE_KDP(1)=SAVEINPT(1)
          IF(NOCOBSPSF) INPUT='PSF PERFNOOB'
          IF(.NOT.NOCOBSPSF) INPUT='PSF PERFECT'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          IF(.NOT.PSFEXT.OR..NOT.CPFNEXT) THEN
              STERROR=.TRUE.
              STVALUE=0.0D0
              MSGSPD=.TRUE.
              DOSTREHL=.FALSE.
              IF(.NOT.PSFHC) THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='PSFPLOT YES'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
              END IF
              RETURN
          END IF
          IF(GPREG(101).EQ.0.0D0) STERROR=.TRUE.
          IF(STERROR) THEN
              STVALUE=0.0D0
              MSGSPD=.TRUE.
              DOSTREHL=.FALSE.
              IF(.NOT.PSFHC) THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='PSFPLOT YES'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
              END IF
              RETURN
          END IF
          STVALUE=STVALUE/GPREG(101)
          MSGSPD=.TRUE.
          DOSTREHL=.FALSE.
          IF(.NOT.PSFHC) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='PSFPLOT YES'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          RETURN
      END
C SUB TFDOTF.FOR
      SUBROUTINE TFDOTF
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE TFDOTF.FOR. IT DOES THRU-FOCUS DOTF
C       INITIATED BY THE TFDOTF COMMAND.
C
          INTEGER MINTF,MAXTF,I
C
          REAL*8 XDOLD,YDOLD,ZDOLD,FPOS,TFSPACE
     1    ,CUTFR,CUTFRX,CUTFRY,XMOD,YMOD,XPHA,YPHA,MMENT,AL,FREQ1,FREQ2
C
          LOGICAL REFERR,ERROR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(SYSTEM1(6).EQ.1.0D0) AL=DABS(ALENS(3,NEWOBJ))*25.4D0
          IF(SYSTEM1(6).EQ.2.0D0) AL=DABS(ALENS(3,NEWOBJ))*10.0D0
          IF(SYSTEM1(6).EQ.3.0D0) AL=DABS(ALENS(3,NEWOBJ))
          IF(SYSTEM1(6).EQ.4.0D0) AL=DABS(ALENS(3,NEWOBJ))*1000.0D0
C
C     DETERMINE NEAR FOR FAR
          IF(NEAR_FAR.EQ.0) NEAR=.TRUE.
          IF(NEAR_FAR.EQ.1) NEAR=.FALSE.
          IF(NEAR.AND.SPACEBALL.EQ.1.AND.AL.GT.
     1    1.0D5) THEN
              OUTLYNE='NO GOTF CAN BE PERFORMED WHEN SPACE IS SET TO "O"'
              CALL SHOWIT(1)
              OUTLYNE='WITH "NEAR" AND WITH AN OBJECT DISTANCE'
              CALL SHOWIT(1)
              OUTLYNE='GREATER THAN 1.0D5 MILLIMETERS.'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          ERROR=.FALSE.
          CALL CUTTOFF(FREQ1,FREQ2,ERROR)
C
          IF(ERROR) THEN
              OUTLYNE='ERROR IN OBJECT/IMAGE SPACE FREQUENCY RELATIONSHIP'
              CALL SHOWIT(1)
              OUTLYNE='CALCULATION OCCURED'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     FREQ1 IS THE IMAGE  SPACE CUTOFF FREQ
C     FREQ2 IS THE OBJECT SPACE CUTOFF FREQ
          IF(SPACEBALL.EQ.1) THEN
              CUTFRY=FREQ2
              CUTFRX=FREQ2
              CUTFR=FREQ2
          ELSE
              CUTFRY=FREQ1
              CUTFRX=FREQ1
              CUTFR=FREQ1
          END IF
          CUTFRY=CUTFRY
          CUTFRX=CUTFRX
          CUTFR=CUTFR
C     CUTFR HAS BEEN COMPUTED
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"TFDOTF" PERFORMS A THRU-FOCUS DOTF AT THE SPATIAL'
              CALL SHOWIT(1)
              OUTLYNE='FREQUENCY CALLED OUT IN NUMERIC WORD #1'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.
     1    S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"TFDOTF" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0) THEN
              OUTLYNE='"TFDOTF" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          TFFREQ=DABS(W1)
          IF(DF2.EQ.1) THEN
              W2=16.0D0
              S2=1
              DF2=0
          END IF
          IF(W2.LT.16.0D0.OR.
     1    (W2-DBLE(INT(W2))).NE.0.0D0.OR.
     2    ((W2/2.0D0)-DBLE(INT(W2/2.0D0))).NE.0.0D0) THEN
              OUTLYNE='NUMERIC WORD #2 IS THE RAY GRID SIZE FOR THE CAPFN'
              CALL SHOWIT(1)
              OUTLYNE=
     1          'CALCULATION. IT MUST BE A POSITIVE'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'EVEN INTEGER VALUE GREATER THAN OR EQUAL TO 16'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          TFSPACE=W2
C
          IF(.NOT.REFEXT) THEN
              OUTLYNE='NO CHIEF RAY EXISTS, NO "TFDOTF" MAY BE PERFORMED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     SAVE THE LAST FOB DATA FOR RE-USE
          TFFOB(1)=LFOB(1)
          TFFOB(2)=LFOB(2)
          TFFOB(3)=LFOB(3)
          TFFOB(4)=LFOB(4)
          TFFOB(5)=LFOB(5)
          TFFOB(6)=LFOB(6)
          TFFOB(7)=LFOB(7)
C     NOW SET UP THE LOOP AND ALLOCATE STORAGE ARRAYS FOR THE THRU-FOCUS
C     CALCULATION
          MAXTF=NINT(DABS((TFTMAX-TFTMIN)/TFDELT))
          MINTF=0
C     GET XD,YD OR ZD FOR THE FOCUS SURFACE AND REMEMBER IT
          XDOLD=ALENS(114,TFSURF)
          YDOLD=ALENS(115,TFSURF)
          ZDOLD=ALENS(116,TFSURF)
          WRITE(OUTLYNE,300)
          CALL SHOWIT(0)
 300      FORMAT('THRU-FOCUS DOTF ANALYSIS')
          IF(TFDIRECTION.EQ.0) WRITE(OUTLYNE,200)
          IF(TFDIRECTION.EQ.1) WRITE(OUTLYNE,201)
          IF(TFDIRECTION.EQ.2) WRITE(OUTLYNE,202)
          CALL SHOWIT(0)
          DO I=0,MAXTF
              FPOS=TFTMIN+(DBLE(I)*TFDELT)
C     UPDATE THE LENS
              F1=0
              F6=1
              F22=0
              LNSTYP=1
              IF(TFDIRECTION.EQ.0) ALENS(31,TFSURF)=XDOLD+FPOS
              IF(TFDIRECTION.EQ.1) ALENS(30,TFSURF)=YDOLD+FPOS
              IF(TFDIRECTION.EQ.2) ALENS(69,TFSURF)=ZDOLD+FPOS
              IF(TFDIRECTION.EQ.0) ALENS(114,TFSURF)=XDOLD+FPOS
              IF(TFDIRECTION.EQ.1) ALENS(115,TFSURF)=YDOLD+FPOS
              IF(TFDIRECTION.EQ.2) ALENS(116,TFSURF)=ZDOLD+FPOS
              IF(TFDIRECTION.EQ.0) MMENT=ALENS(114,TFSURF)
              IF(TFDIRECTION.EQ.1) MMENT=ALENS(115,TFSURF)
              IF(TFDIRECTION.EQ.2) MMENT=ALENS(116,TFSURF)
              CALL LNSEOS
C     NOW TRACE THE CHIEF RAY
              SAVE_KDP(1)=SAVEINPT(1)
              WC='FOB     '
              SQ=0
              SST=0
              STI=0
              DF1=0
              DF2=0
              DF3=0
              DF4=0
              DF5=1
              S1=1
              S2=1
              S3=1
              S4=1
              S5=0
              SN=1
              W1=TFFOB(1)
              W2=TFFOB(2)
              W3=TFFOB(3)
              W4=TFFOB(4)
              IF(RAYCLEAR) REFEXT=.FALSE.
              CALL FFOB
              IF(.NOT.REFEXT) THEN
                  OUTLYNE='CHIEF RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  OUTLYNE='"TFDOTF" ANALYSIS TERMINATED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C     NOW DO THE CAPFN
              SAVE_KDP(1)=SAVEINPT(1)
              MSGSPD=.FALSE.
              WC='CAPFN'
              SQ=0
              SST=0
              STI=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              SN=1
              W1=TFSPACE
              REFERR=.FALSE.
              NRDFACTOR=1.0D0
              OLDIF=LDIF
              LDIF=.FALSE.
              CALL COMPAP(REFERR,1)
              LDIF=OLDIF
              IF(.NOT.CPFNEXT) THEN
                  OUTLYNE='COMPLEX APERTURE FUNCTION COULD NOT BE GENERATED'
                  CALL SHOWIT(1)
                  OUTLYNE='"TFDOTF" ANALYSIS TERMINATED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
              MSGSPD=.TRUE.

C     NOW DO THE X AND Y DOTFS AT THE SPECIFIED FREQUENCY
              SAVE_KDP(1)=SAVEINPT(1)
              WC='DOTF    '
              WQ='XACC    '
              SQ=1
              SST=0
              STI=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              SN=1
              W1=TFFREQ
              CALL DOTF
              XMOD=REG(9)
              XPHA=REG(10)
              REST_KDP(1)=RESTINPT(1)
              SAVE_KDP(1)=SAVEINPT(1)
              WC='DOTF    '
              WQ='YACC    '
              SQ=1
              SST=0
              STI=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              SN=1
              W1=TFFREQ
              CALL DOTF
              YMOD=REG(9)
              YPHA=REG(10)
              REST_KDP(1)=RESTINPT(1)
C     DISPLAY THE RESULTS
              WRITE(OUTLYNE,100)MMENT,XMOD,XPHA,YMOD,YPHA
              CALL SHOWIT(0)
 100          FORMAT(G13.6,2X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6)
 200          FORMAT('   X-MOTION  ',2X,' XZ-MTF(MOD) ',2X,'XZ-MTF(PHASE)'
     1        ,2X,' YZ-MTF(MOD) ',2X,'YZ-MTF(PHASE)')
 201          FORMAT('   Y-MOTION  ',2X,' XZ-MTF(MOD) ',2X,'XZ-MTF(PHASE)'
     1        ,2X,' YZ-MTF(MOD) ',2X,'YZ-MTF(PHASE)')
 202          FORMAT('   Z-MOTION  ',2X,' XZ-MTF(MOD) ',2X,'XZ-MTF(PHASE)'
     1        ,2X,' YZ-MTF(MOD) ',2X,'YZ-MTF(PHASE)')
C     DO THE NEXT FOCUS POSITION
          END DO
          ALENS(114,TFSURF)=XDOLD
          ALENS(115,TFSURF)=YDOLD
          ALENS(116,TFSURF)=ZDOLD
          ALENS(31,TFSURF)=XDOLD
          ALENS(30,TFSURF)=YDOLD
          ALENS(69,TFSURF)=ZDOLD
          F1=0
          F6=1
          F22=1
          LNSTYP=1
          CALL LNSEOS
          RETURN
      END
C SUB SPACER.FOR
      SUBROUTINE SPACER
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SPACER.FOR.
C     CALLED BY CMDER FOR COMMAND SPACE
C     THIS SETS THE SPACE FOR DIFFRACTION CALCULATIONS
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"SPACE" SETS DIFFRACTION CALCULATION SPACE TO'
              CALL SHOWIT(1)
              OUTLYNE='"O" FOR OBJECT SPACE OR "I" FOR IMAGE SPACE'
              CALL SHOWIT(1)
              IF(SPACEBALL.EQ.1) OUTLYNE='CURRENT SETTING IS "O"'
              IF(SPACEBALL.EQ.2) OUTLYNE='CURRENT SETTING IS "I"'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE='"SPACE" TAKES NO NUMERIC OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"SPACE" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'I'.AND.WQ.NE.'O') THEN
                  OUTLYNE='"SPACE" QUALIFIER MUST BE "O" OR "I"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'O') SPACEBALL=1
              IF(WQ.EQ.'I') SPACEBALL=2
          END IF
C     NO, NO, NO, NOT SPACEBALLS!
          RETURN
      END
C SUB CUTOFF.FOR
      SUBROUTINE CUTOFF
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE CUTOFF.FOR.
C     CALLED BY CMDER FOR COMMAND CUTOFF
C     THIS CALCULATES CUTOFF FREQ
C
          REAL*8 REFIN,NREFIN,CUTFRX,CUTFRY,GRNX,GRNY
C
!      LOGICAL ERRR
C
          INTEGER SHTNM
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          IF(STI.EQ.1.OR.SN.EQ.0) THEN
              OUTLYNE='"CUTOFF" CALCULATES "CUTOFF" FREQ.'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE='"CUTOFF" TAKES NO NUMERIC OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"CUTOFF" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'I'.AND.WQ.NE.'O'.AND.WQ.NE.'IACC'.AND.WQ.NE.
     1        'OACC') THEN
                  OUTLYNE='"CUTOFF" QUALIFIER MUST BE "O", "I", "IACC" OR "OACC"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
C                       PROCEED
              END IF
          END IF
C       CALC THE SHRTWAVE
          SHRTWAVE=0.0D0
          IF(SYSTEM1(1).NE.0.0D0.AND.SYSTEM1(31).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(1)
              GO TO 314
          END IF
          IF(SYSTEM1(2).NE.0.0D0.AND.SYSTEM1(32).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(2)
              GO TO 314
          END IF
          IF(SYSTEM1(3).NE.0.0D0.AND.SYSTEM1(33).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(3)
              GO TO 314
          END IF
          IF(SYSTEM1(4).NE.0.0D0.AND.SYSTEM1(34).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(4)
              GO TO 314
          END IF
          IF(SYSTEM1(5).NE.0.0D0.AND.SYSTEM1(35).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(5)
              GO TO 314
          END IF
          IF(SYSTEM1(71).NE.0.0D0.AND.SYSTEM1(76).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(71)
              GO TO 314
          END IF
          IF(SYSTEM1(72).NE.0.0D0.AND.SYSTEM1(77).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(72)
              GO TO 314
          END IF
          IF(SYSTEM1(73).NE.0.0D0.AND.SYSTEM1(78).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(73)
              GO TO 314
          END IF
          IF(SYSTEM1(74).NE.0.0D0.AND.SYSTEM1(79).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(74)
              GO TO 314
          END IF
          IF(SYSTEM1(75).NE.0.0D0.AND.SYSTEM1(80).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(75)
              GO TO 314
          END IF
 314      CONTINUE
          IF(SHRTWAVE.EQ.0.0D0) THEN
              OUTLYNE='WAVELENGTHS ARE ALL ZERO'
              CALL SHOWIT(1)
              OUTLYNE='NO CUTOFF FREQUENCY CAN BE DEFINED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'I'.AND.WQ.NE.'O'.AND.WQ.NE.'IACC'.AND.WQ.NE.
     1        'OACC') THEN
                  OUTLYNE='"CUTOFF" QUALIFIER MUST BE "O", "I", "IACC" OR "OACC"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
C
              IF(SYSTEM1(1).LE.SHRTWAVE
     1        .AND.SYSTEM1(1).NE.0.0D0.AND.SYSTEM1(31).NE.0.0D0) THEN
                  SHRTWAVE=SYSTEM1(1)
                  SHTNM=1
              END IF
              IF(SYSTEM1(2).LE.SHRTWAVE
     1        .AND.SYSTEM1(2).NE.0.0D0.AND.SYSTEM1(32).NE.0.0D0) THEN
                  SHRTWAVE=SYSTEM1(2)
                  SHTNM=2
              END IF
              IF(SYSTEM1(3).LE.SHRTWAVE
     1        .AND.SYSTEM1(3).NE.0.0D0.AND.SYSTEM1(33).NE.0.0D0) THEN
                  SHRTWAVE=SYSTEM1(3)
                  SHTNM=3
              END IF
              IF(SYSTEM1(4).LE.SHRTWAVE
     1        .AND.SYSTEM1(4).NE.0.0D0.AND.SYSTEM1(34).NE.0.0D0) THEN
                  SHRTWAVE=SYSTEM1(4)
                  SHTNM=4
              END IF
              IF(SYSTEM1(5).LE.SHRTWAVE
     1        .AND.SYSTEM1(5).NE.0.0D0.AND.SYSTEM1(35).NE.0.0D0) THEN
                  SHRTWAVE=SYSTEM1(5)
                  SHTNM=5
              END IF
              IF(SYSTEM1(71).LE.SHRTWAVE
     1        .AND.SYSTEM1(71).NE.0.0D0.AND.SYSTEM1(76).NE.0.0D0) THEN
                  SHRTWAVE=SYSTEM1(71)
                  SHTNM=6
              END IF
              IF(SYSTEM1(72).LE.SHRTWAVE
     1        .AND.SYSTEM1(72).NE.0.0D0.AND.SYSTEM1(77).NE.0.0D0) THEN
                  SHRTWAVE=SYSTEM1(72)
                  SHTNM=7
              END IF
              IF(SYSTEM1(73).LE.SHRTWAVE
     1        .AND.SYSTEM1(73).NE.0.0D0.AND.SYSTEM1(78).NE.0.0D0) THEN
                  SHRTWAVE=SYSTEM1(73)
                  SHTNM=8
              END IF
              IF(SYSTEM1(74).LE.SHRTWAVE
     1        .AND.SYSTEM1(74).NE.0.0D0.AND.SYSTEM1(79).NE.0.0D0) THEN
                  SHRTWAVE=SYSTEM1(74)
                  SHTNM=9
              END IF
              IF(SYSTEM1(75).LE.SHRTWAVE
     1        .AND.SYSTEM1(75).NE.0.0D0.AND.SYSTEM1(80).NE.0.0D0) THEN
                  SHRTWAVE=SYSTEM1(75)
                  SHTNM=10
              END IF
C
              SHRTWAVE=(SHRTWAVE*1.0D-3)
          ELSE
          END IF
C
C     TRY TO TRACE A GUT RAY IF ONE DOES NOT EXITS
          IF(.NOT.REFEXT) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              WC='FOB     '
              SQ=0
              SST=0
              STI=0
              DF1=1
              DF2=1
              DF3=1
              DF4=0
              DF5=1
              S1=0
              S2=0
              S3=0
              S4=1
              S5=0
              SN=0
              W4=DBLE(SHTNM)
              CALL FFOB
              REST_KDP(1)=RESTINPT(1)
          ELSE
          END IF
C
C     DO THE CALCULATION FOR IMAGE SPACE
          IF(WQ.EQ.'I'.OR.WQ.EQ.'IACC') THEN
C     IMAGE SPACE
              IF(SYSTEM1(30).GE.3.0D0) THEN
C     AFOCAL
C     GET THE X AND Y EPDSx0.001 AND DIVIDE BY THE SHORTEST WAVELENGTH
C
                  GRNX=EXDIAX
                  GRNY=EXDIAY
C
                  IF(SYSTEM1(6).EQ.1.0D0) GRNX=GRNX*25.4D0
                  IF(SYSTEM1(6).EQ.2.0D0) GRNX=GRNX*10.0D0
                  IF(SYSTEM1(6).EQ.3.0D0) GRNX=GRNX
                  IF(SYSTEM1(6).EQ.4.0D0) GRNX=GRNX*1000.0D0
                  IF(SYSTEM1(6).EQ.1.0D0) GRNY=GRNY*25.4D0
                  IF(SYSTEM1(6).EQ.2.0D0) GRNY=GRNY*10.0D0
                  IF(SYSTEM1(6).EQ.3.0D0) GRNY=GRNY
                  IF(SYSTEM1(6).EQ.4.0D0) GRNY=GRNY*1000.0D0
                  CUTFRX=DABS((0.001D0*(GRNX))/SHRTWAVE)
                  CUTFRY=DABS((0.001D0*(GRNY))/SHRTWAVE)
                  CUTFRY=CUTFRY
                  CUTFRX=CUTFRX
                  REG(40)=REG(9)
                  REG(10)=CUTFRX
                  REG(9)=CUTFRY
                  IF(WQ.EQ.'I') THEN
 10                   FORMAT('XZ-PLANE IMAGE SPACE CUTOFF FREQUENCY = ',
     1                G13.6,' LP/MRAD')
 11                   FORMAT('YZ-PLANE IMAGE SPACE CUTOFF FREQUENCY = ',
     1                G13.6,' LP/MRAD')
                      IF(WQ(2:4).NE.'ACC') THEN
                          WRITE(OUTLYNE,10) (CUTFRX)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,11) (CUTFRY)
                          CALL SHOWIT(0)
                      END IF
                  END IF
C
              ELSE
C     FOCAL
C     GET THE X AND Y FNUM MULT BY THE SHORTEST WAVELENGTH
C     AND INVERT
C
                  GRNX=RBFNX
                  GRNY=RBFNY
C
C     NEXT FEW LINES SCALE THE F/NUMBER FOR INDEX IN SPACE
C     OF OBJECT OR IMAGE
                  IF(SYSTEM1(11).GE.1.0D0.AND.SYSTEM1(11).LE.5.0D0) THEN
                      REFIN=ALENS(45+INT(SYSTEM1(11)),NEWIMG-1)
                  END IF
                  IF(SYSTEM1(11).GE.6.0D0.AND.SYSTEM1(11).LE.10.0D0) THEN
                      REFIN=ALENS(65+INT(SYSTEM1(11)),NEWIMG-1)
                  END IF
                  IF(SHTNM.GE.1.AND.SHTNM.LE.5) THEN
                      NREFIN=ALENS(45+SHTNM,NEWIMG-1)
                  END IF
                  IF(SHTNM.GE.6.AND.SHTNM.LE.10) THEN
                      NREFIN=ALENS(65+SHTNM,NEWIMG-1)
                  END IF
                  GRNX=GRNX*REFIN/NREFIN
                  GRNY=GRNY*REFIN/NREFIN
C
                  CUTFRX=DABS(1.0D0/(SHRTWAVE*GRNX))
                  CUTFRY=DABS(1.0D0/(SHRTWAVE*GRNY))
                  CUTFRY=CUTFRY
                  CUTFRX=CUTFRX
C
                  REG(40)=REG(9)
                  REG(10)=CUTFRX
                  REG(9)=CUTFRY
 20               FORMAT('XZ-PLANE IMAGE SPACE CUTOFF FREQUENCY = ',
     1            G13.6,' LP/MM')
 21               FORMAT('YZ-PLANE IMAGE SPACE CUTOFF FREQUENCY = ',
     1            G13.6,' LP/MM')
                  IF(WQ(2:4).NE.'ACC') THEN
                      WRITE(OUTLYNE,20) (CUTFRX)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,21) (CUTFRY)
                      CALL SHOWIT(0)
                  END IF
              END IF
          END IF
C
C
C     DETERMINE NEAR FOR FAR
          IF(NEAR_FAR.EQ.0) NEAR=.TRUE.
          IF(NEAR_FAR.EQ.1) NEAR=.FALSE.
          IF(WQ.EQ.'O'.OR.WQ.EQ.'OACC') THEN
C     DO THE CALCULATION FOR OBJECT SPACE
              IF(NEAR) THEN
C
                  GRNX=RFFNX
                  GRNY=RFFNY
C
C     NEXT FEW LINES SCALE THE F/NUMBER FOR INDEX IN SPACE
C     OF OBJECT OR IMAGE
                  IF(SYSTEM1(11).GE.1.0D0.AND.SYSTEM1(11).LE.5.0D0) THEN
                      REFIN=ALENS(45+INT(SYSTEM1(11)),NEWOBJ)
                  END IF
                  IF(SYSTEM1(11).GE.6.0D0.AND.SYSTEM1(11).LE.10.0D0) THEN
                      REFIN=ALENS(65+INT(SYSTEM1(11)),NEWOBJ)
                  END IF
                  IF(SHTNM.GE.1.AND.SHTNM.LE.5) THEN
                      NREFIN=ALENS(45+SHTNM,NEWOBJ)
                  END IF
                  IF(SHTNM.GE.6.AND.SHTNM.LE.10) THEN
                      NREFIN=ALENS(65+SHTNM,NEWOBJ)
                  END IF
                  GRNX=GRNX*REFIN/NREFIN
                  GRNY=GRNY*REFIN/NREFIN
C
                  CUTFRX=DABS(1.0D0/(GRNX*SHRTWAVE))
                  CUTFRY=DABS(1.0D0/(GRNY*SHRTWAVE))
                  CUTFRY=CUTFRY
                  CUTFRX=CUTFRX
                  REG(40)=REG(9)
                  REG(10)=CUTFRX
                  REG(9)=CUTFRY
 30               FORMAT('XZ-PLANE OBJECT SPACE CUTOFF FREQUENCY = ',
     1            G13.6,' LP/MM')
 31               FORMAT('YZ-PLANE OBJECT SPACE CUTOFF FREQUENCY = ',
     1            G13.6,' LP/MM')
                  IF(WQ(2:4).NE.'ACC') THEN
                      WRITE(OUTLYNE,30) (CUTFRX)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,31) (CUTFRY)
                      CALL SHOWIT(0)
                  END IF
              ELSE
C     FAR
                  GRNX=ENDIAX
                  GRNY=ENDIAY
C
                  IF(SYSTEM1(6).EQ.1.0D0) GRNX=GRNX*25.4D0
                  IF(SYSTEM1(6).EQ.2.0D0) GRNX=GRNX*10.0D0
                  IF(SYSTEM1(6).EQ.3.0D0) GRNX=GRNX
                  IF(SYSTEM1(6).EQ.4.0D0) GRNX=GRNX*1000.0D0
                  IF(SYSTEM1(6).EQ.1.0D0) GRNY=GRNY*25.4D0
                  IF(SYSTEM1(6).EQ.2.0D0) GRNY=GRNY*10.0D0
                  IF(SYSTEM1(6).EQ.3.0D0) GRNY=GRNY
                  IF(SYSTEM1(6).EQ.4.0D0) GRNY=GRNY*1000.0D0
C
                  CUTFRX=DABS((0.001D0*GRNX)/(SHRTWAVE))
                  CUTFRY=DABS((0.001D0*GRNY)/(SHRTWAVE))
                  CUTFRY=CUTFRY
                  CUTFRX=CUTFRX
C
                  REG(40)=REG(9)
                  REG(10)=CUTFRX
                  REG(9)=CUTFRY
 40               FORMAT('XZ-PLANE OBJECT SPACE CUTOFF FREQUENCY = ',
     1            G13.6,' LP/MRAD')
 41               FORMAT('YZ-PLANE OBJECT SPACE CUTOFF FREQUENCY = ',
     1            G13.6,' LP/MRAD')
                  IF(WQ(2:4).NE.'ACC') THEN
                      WRITE(OUTLYNE,40) (CUTFRX)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,41) (CUTFRY)
                      CALL SHOWIT(0)
                  END IF
              END IF
          END IF
          RETURN
      END
C SUB CUTTOFF.FOR
      SUBROUTINE CUTTOFF(FREQ1,FREQ2,ERROR)
C
          IMPLICIT NONE
C
          REAL*8 FREQ1,FREQ2
     1    ,REFIN,NREFIN,CUTFRX,CUTFRY,GRNX,GRNY
C
          LOGICAL ERROR
C
          INTEGER SHTNM,TYPE
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          ERROR=.FALSE.
C       CALC THE SHRTWAVE
          SHRTWAVE=0.0D0
          IF(SYSTEM1(1).NE.0.0D0.AND.SYSTEM1(31).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(1)
              GO TO 314
          END IF
          IF(SYSTEM1(2).NE.0.0D0.AND.SYSTEM1(32).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(2)
              GO TO 314
          END IF
          IF(SYSTEM1(3).NE.0.0D0.AND.SYSTEM1(33).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(3)
              GO TO 314
          END IF
          IF(SYSTEM1(4).NE.0.0D0.AND.SYSTEM1(34).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(4)
              GO TO 314
          END IF
          IF(SYSTEM1(5).NE.0.0D0.AND.SYSTEM1(35).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(5)
              GO TO 314
          END IF
          IF(SYSTEM1(71).NE.0.0D0.AND.SYSTEM1(76).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(71)
              GO TO 314
          END IF
          IF(SYSTEM1(72).NE.0.0D0.AND.SYSTEM1(77).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(72)
              GO TO 314
          END IF
          IF(SYSTEM1(73).NE.0.0D0.AND.SYSTEM1(78).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(73)
              GO TO 314
          END IF
          IF(SYSTEM1(74).NE.0.0D0.AND.SYSTEM1(79).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(74)
              GO TO 314
          END IF
          IF(SYSTEM1(75).NE.0.0D0.AND.SYSTEM1(80).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(75)
              GO TO 314
          END IF
 314      CONTINUE
          IF(SHRTWAVE.EQ.0.0D0) THEN
              ERROR=.TRUE.
              RETURN
          END IF
C
          IF(SYSTEM1(1).LE.SHRTWAVE
     1    .AND.SYSTEM1(1).NE.0.0D0.AND.SYSTEM1(31).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(1)
              SHTNM=1
          END IF
          IF(SYSTEM1(2).LE.SHRTWAVE
     1    .AND.SYSTEM1(2).NE.0.0D0.AND.SYSTEM1(32).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(2)
              SHTNM=2
          END IF
          IF(SYSTEM1(3).LE.SHRTWAVE
     1    .AND.SYSTEM1(3).NE.0.0D0.AND.SYSTEM1(33).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(3)
              SHTNM=3
          END IF
          IF(SYSTEM1(4).LE.SHRTWAVE
     1    .AND.SYSTEM1(4).NE.0.0D0.AND.SYSTEM1(34).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(4)
              SHTNM=4
          END IF
          IF(SYSTEM1(5).LE.SHRTWAVE
     1    .AND.SYSTEM1(5).NE.0.0D0.AND.SYSTEM1(35).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(5)
              SHTNM=5
          END IF
          IF(SYSTEM1(71).LE.SHRTWAVE
     1    .AND.SYSTEM1(71).NE.0.0D0.AND.SYSTEM1(76).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(71)
              SHTNM=6
          END IF
          IF(SYSTEM1(72).LE.SHRTWAVE
     1    .AND.SYSTEM1(72).NE.0.0D0.AND.SYSTEM1(77).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(72)
              SHTNM=7
          END IF
          IF(SYSTEM1(73).LE.SHRTWAVE
     1    .AND.SYSTEM1(73).NE.0.0D0.AND.SYSTEM1(78).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(73)
              SHTNM=8
          END IF
          IF(SYSTEM1(74).LE.SHRTWAVE
     1    .AND.SYSTEM1(74).NE.0.0D0.AND.SYSTEM1(79).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(74)
              SHTNM=9
          END IF
          IF(SYSTEM1(75).LE.SHRTWAVE
     1    .AND.SYSTEM1(75).NE.0.0D0.AND.SYSTEM1(80).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(75)
              SHTNM=10
          END IF
C
          SHRTWAVE=(SHRTWAVE*1.0D-3)
C
C     IF NO GUT RAY EXISTS, RETURN
          IF(.NOT.REFEXT) THEN
              ERROR=.TRUE.
              RETURN
          END IF
C
C     DO THE CALCULATION FOR IMAGE SPACE
          TYPE=1
          IF(TYPE.EQ.1) THEN
C     IMAGE SPACE
              IF(SYSTEM1(30).GE.3.0D0) THEN
C     AFOCAL
C     GET THE X AND Y EPDSx0.001 AND DIVIDE BY THE SHORTEST WAVELENGTH
C
                  GRNX=EXDIAX
                  GRNY=EXDIAY
C
                  IF(SYSTEM1(6).EQ.1.0D0) GRNX=GRNX*25.4D0
                  IF(SYSTEM1(6).EQ.2.0D0) GRNX=GRNX*10.0D0
                  IF(SYSTEM1(6).EQ.3.0D0) GRNX=GRNX
                  IF(SYSTEM1(6).EQ.4.0D0) GRNX=GRNX*1000.0D0
                  IF(SYSTEM1(6).EQ.1.0D0) GRNY=GRNY*25.4D0
                  IF(SYSTEM1(6).EQ.2.0D0) GRNY=GRNY*10.0D0
                  IF(SYSTEM1(6).EQ.3.0D0) GRNY=GRNY
                  IF(SYSTEM1(6).EQ.4.0D0) GRNY=GRNY*1000.0D0
                  CUTFRX=DABS((0.001D0*(GRNX))/SHRTWAVE)
                  CUTFRY=DABS((0.001D0*(GRNY))/SHRTWAVE)
                  CUTFRY=CUTFRY
                  CUTFRX=CUTFRX
                  REG(40)=REG(9)
                  REG(10)=CUTFRX
                  REG(9)=CUTFRY
              ELSE
C     FOCAL
C     GET THE X AND Y FNUM MULT BY THE SHORTEST WAVELENGTH
C     AND INVERT
C
                  GRNX=RBFNX
                  GRNY=RBFNY
C
C     NEXT FEW LINES SCALE THE F/NUMBER FOR INDEX IN SPACE
C     OF OBJECT OR IMAGE
                  IF(SYSTEM1(11).GE.1.0D0.AND.SYSTEM1(11).LE.5.0D0) THEN
                      REFIN=ALENS(45+INT(SYSTEM1(11)),NEWIMG-1)
                  END IF
                  IF(SYSTEM1(11).GE.6.0D0.AND.SYSTEM1(11).LE.10.0D0) THEN
                      REFIN=ALENS(65+INT(SYSTEM1(11)),NEWIMG-1)
                  END IF
                  IF(SHTNM.GE.1.AND.SHTNM.LE.5) THEN
                      NREFIN=ALENS(45+SHTNM,NEWIMG-1)
                  END IF
                  IF(SHTNM.GE.6.AND.SHTNM.LE.10) THEN
                      NREFIN=ALENS(65+SHTNM,NEWIMG-1)
                  END IF
                  GRNX=GRNX*REFIN/NREFIN
                  GRNY=GRNY*REFIN/NREFIN
C
                  CUTFRX=DABS(1.0D0/(SHRTWAVE*GRNX))
                  CUTFRY=DABS(1.0D0/(SHRTWAVE*GRNY))
                  CUTFRY=CUTFRY
                  CUTFRX=CUTFRX
C
                  REG(40)=REG(9)
                  REG(10)=CUTFRX
                  REG(9)=CUTFRY
              END IF
          END IF
          FREQ1=REG(9)
C
C
C     DETERMINE NEAR FOR FAR
          IF(NEAR_FAR.EQ.0) NEAR=.TRUE.
          IF(NEAR_FAR.EQ.1) NEAR=.FALSE.
          TYPE=2
          IF(TYPE.EQ.2) THEN
C     DO THE CALCULATION FOR OBJECT SPACE
              IF(NEAR) THEN
C
                  GRNX=RFFNX
                  GRNY=RFFNY
C
C     NEXT FEW LINES SCALE THE F/NUMBER FOR INDEX IN SPACE
C     OF OBJECT OR IMAGE
                  IF(SYSTEM1(11).GE.1.0D0.AND.SYSTEM1(11).LE.5.0D0) THEN
                      REFIN=ALENS(45+INT(SYSTEM1(11)),NEWOBJ)
                  END IF
                  IF(SYSTEM1(11).GE.6.0D0.AND.SYSTEM1(11).LE.10.0D0) THEN
                      REFIN=ALENS(65+INT(SYSTEM1(11)),NEWOBJ)
                  END IF
                  IF(SHTNM.GE.1.AND.SHTNM.LE.5) THEN
                      NREFIN=ALENS(45+SHTNM,NEWOBJ)
                  END IF
                  IF(SHTNM.GE.6.AND.SHTNM.LE.10) THEN
                      NREFIN=ALENS(65+SHTNM,NEWOBJ)
                  END IF
                  GRNX=GRNX*REFIN/NREFIN
                  GRNY=GRNY*REFIN/NREFIN
C
                  CUTFRX=DABS(1.0D0/(GRNX*SHRTWAVE))
                  CUTFRY=DABS(1.0D0/(GRNY*SHRTWAVE))
                  CUTFRY=CUTFRY
                  CUTFRX=CUTFRX
                  REG(40)=REG(9)
                  REG(10)=CUTFRX
                  REG(9)=CUTFRY
              ELSE
C     FAR
                  GRNX=ENDIAX
                  GRNY=ENDIAY
C
                  IF(SYSTEM1(6).EQ.1.0D0) GRNX=GRNX*25.4D0
                  IF(SYSTEM1(6).EQ.2.0D0) GRNX=GRNX*10.0D0
                  IF(SYSTEM1(6).EQ.3.0D0) GRNX=GRNX
                  IF(SYSTEM1(6).EQ.4.0D0) GRNX=GRNX*1000.0D0
                  IF(SYSTEM1(6).EQ.1.0D0) GRNY=GRNY*25.4D0
                  IF(SYSTEM1(6).EQ.2.0D0) GRNY=GRNY*10.0D0
                  IF(SYSTEM1(6).EQ.3.0D0) GRNY=GRNY
                  IF(SYSTEM1(6).EQ.4.0D0) GRNY=GRNY*1000.0D0
C
                  CUTFRX=DABS((0.001D0*GRNX)/(SHRTWAVE))
                  CUTFRY=DABS((0.001D0*GRNY)/(SHRTWAVE))
                  CUTFRY=CUTFRY
                  CUTFRX=CUTFRX
C
                  REG(40)=REG(9)
                  REG(10)=CUTFRX
                  REG(9)=CUTFRY
              END IF
          END IF
          FREQ2=REG(9)
          RETURN
      END
C SUB COMPAP.FOR
      SUBROUTINE COMPAP(REFERR,TPT)
C
          IMPLICIT NONE
C
!        CHARACTER UN*11
c
          REAL*8 SPT,SPT1,SPT2,SPT3,SPT4,IWLIJK(1:10)
     1    ,SPT5,SPT6,SPT7,SPT8,SPT9,SPT10,LAMFACTOR
     2    ,OPPEAK(0:10),OPPIT(0:10),OPPTV(0:10),SUMONE(0:10),
     3    SUMTWO(0:10),ARG,LAM(1:10),WEI(1:10),SUML2,SUML4
     4    ,WEIS,SIG,SUMSIG,DELFOB,APODX2,APODR2
C
          REAL*8 DSPO
C
          DIMENSION DSPO(:,:)
C
          LOGICAL REFERR,EXTDMTF1,EXTDMTF2,PERF
          COMMON/DMTFEXT/EXTDMTF1,EXTDMTF2,PERF
C
          LOGICAL NOCOBSPSF
          COMMON/PSFCOBS/NOCOBSPSF
C
          COMMON/SPTWTS/SPT1,SPT2,SPT3,SPT4,SPT5
     1    ,SPT6,SPT7,SPT8,SPT9,SPT10
C
          LOGICAL TCLPRF,SPDTRA
C
          INTEGER IJK,NNTOT,ISS,IWIW2,NP1,TPT,SPDCD1,IJ,SPDCD2,ALLOERR
C
          COMMON/PEPITO/IWLIJK,IJK
C
          COMMON/SPRA1/SPDTRA
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          COMMON/SLOPY/X,Y
C
C       THIS IS COMPAP.FOR.
C       CREATES THE COMPLEX APERTURE FUNCTION FROM THE SPOT DIAGRAM
C       THERE MUST BE AN n**2 X n**2 GRID OF RAYS IN THE SPOT DIAGRAM
C       FO THIS TO WORK PROPERLY
C
C     THESE COMMANDS ONLY TAKE NUMERIC WORD #1
C
C     NUMERIC WORD #1 IS TH NUMBER OF RAYS ACROSS A RECTANGULAR GRID
C
C                           MIN VALUE = 8
C
C     ONLY VAULES WHICH ARE POWERS OF 2 ARE ALLOWED
C
!        INTEGER KIK,KIKI
C
          REAL*8 TOT,W,X,Y
C
          REAL*8 APFAC
C
          INTEGER I,J,IWL,NUMT1,NUMT2,NUMT3,NUMT4,
     1    NUMT5,NUMT6,NUMT7,NUMT8,NUMT9,NUMT10
     2    ,NSTART,NSTOP,IX,IY,NUMT0,TEMPHOLDER
C
          LOGICAL FOBB0
     1    ,FOBB0X,FOBB0Y
C
          COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
C     THE FOLLOWING VARIABLES TRACK THE REFERENCE SPHERE RADIUS
C     AND THE "OTHER RAY INTERSECTION POINTS WITH THIS REFERENCE
C     SPHERE
C
          REAL*8 XSPH,YSPH,ZSPH,RADSPH,ADJUSTW1,ADJUSTW2
C
          COMMON/REFSPR/XSPH,YSPH,ZSPH,RADSPH
C
          LOGICAL COMPYES,TOLYES
C
          COMMON/YESCOMP/COMPYES
C
          COMMON/YESTOL/TOLYES
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          ALLOCATABLE :: DSPO
C
          ADJUSTW1=1.0D0
          ADJUSTW2=1.0D0
          IF(USEOLREF.AND.SAVEREF) CALL REST_CHIEF_RAY_DATA
          SUMMOR=.FALSE.
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
          IF(SUML2.NE.0.0D0.AND.SUML4.NE.0.0D0)
     1    LAMAVE=DSQRT(SUML2/SUML4)
          DLLX=0.0D0
          DLLY=0.0D0
          DLLZ=0.0D0
          PERF=.FALSE.
          IF(WQ.EQ.'SILENT') MSGSPD=.FALSE.
C
C     TPT=1 FOR REGULAR CAPFN , 2 FOR TOL AND OPT
C
C     TEST VALUES FOR OPD LIMITS
          OPPEAK(0)=-1.0E20
          OPPEAK(1)=-1.0E20
          OPPEAK(2)=-1.0E20
          OPPEAK(3)=-1.0E20
          OPPEAK(4)=-1.0E20
          OPPEAK(5)=-1.0E20
          OPPEAK(6)=-1.0E20
          OPPEAK(7)=-1.0E20
          OPPEAK(8)=-1.0E20
          OPPEAK(9)=-1.0E20
          OPPEAK(10)=-1.0E20
          OPPIT(0)=+1.0E20
          OPPIT(1)=+1.0E20
          OPPIT(2)=+1.0E20
          OPPIT(3)=+1.0E20
          OPPIT(4)=+1.0E20
          OPPIT(5)=+1.0E20
          OPPIT(6)=+1.0E20
          OPPIT(7)=+1.0E20
          OPPIT(8)=+1.0E20
          OPPIT(9)=+1.0E20
          OPPIT(10)=+1.0E20
          EXTDMTF1=.FALSE.
          EXTDMTF2=.FALSE.
          GRASET=.FALSE.
C
          IF(TPT.EQ.2) THEN
              IF(TOLYES.OR.COMPYES) THEN
                  IW=TOLNRD
                  W1=DBLE(IW)
              ELSE
                  IW=OPNRD
                  W1=DBLE(IW)
              END IF
          END IF
          IF(TPT.EQ.1) THEN
C
C       DEFAULT NUMERIC INPUT, ALL DEFAULTS ARE ZERO
              IF(WC.EQ.'CAPFN') THEN
                  IF(DF1.EQ.1) THEN
                      W1=CAPDEF
                      NRD=INT(DABS(CAPDEF))
                      IF(NRD.EQ.16) TGR=64
                      IF(NRD.GT.16.AND.NRD.LE.32) TGR=128
                      IF(NRD.GT.32.AND.NRD.LE.64) TGR=256
                      IF(NRD.GT.64.AND.NRD.LE.128) TGR=512
                      IF(NRD.GT.128.AND.NRD.LE.256) TGR=1024
                      IF(NRD.GT.256.AND.NRD.LE.512) TGR=2048
                      PGR=TGR-1
                  END IF
                  IF(DF1.EQ.0) THEN
                      CAPDEF=DBLE(INT(W1))
                      NRD=INT(DABS(W1))
                      IF(NRD.EQ.16) TGR=64
                      IF(NRD.GT.16.AND.NRD.LE.32) TGR=128
                      IF(NRD.GT.32.AND.NRD.LE.64) TGR=256
                      IF(NRD.GT.64.AND.NRD.LE.128) TGR=512
                      IF(NRD.GT.128.AND.NRD.LE.256) TGR=1024
                      IF(NRD.GT.256.AND.NRD.LE.512) TGR=2048
                      PGR=TGR-1
                  ELSE
                  END IF
              END IF
              IF(WC.EQ.'PSF'.OR.WC.EQ.'PUPIL') THEN
                  W1=DBLE(NRD)
              END IF
          END IF
          IF(W1.LE.0.0D0.AND.TPT.EQ.1.AND.WC.NE.'PSF'.AND.WC.NE.'PUPIL')THEN
              OUTLYNE=
     1        '"CAPFN" REQUIERS SOME POSITIVE NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
C     DEALLOCATE DSPOTT
              CALL SPOTIT(1)
              CALL MACFAL
              RETURN
C       NOT ALL ZERO INPUT , PROCEED
          END IF
          IF(WC.EQ.'CAPFN'.AND.TPT.EQ.1) THEN
              IF(W1.LT.16.0D0.OR.W1.GT.8192.0D0.OR.
     1        (W1-DBLE(INT(W1))).NE.0.0D0.OR.
     2        ((W1/2.0D0)-DBLE(INT(W1/2.0D0))).NE.0.0D0) THEN
                  IF(WC.EQ.'CAPFN') OUTLYNE=
     1              '"CAPFN" REQUIERS NUMERIC WORD #1 TO BE A POSITIVE'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'EVEN INTEGER VALUE FROM 16 TO 8192'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  CALL MACFAL
                  RETURN
C       NOT ALL ZERO INPUT , PROCEED
              END IF
          END IF
          IF(TPT.EQ.1.AND.WC.EQ.'CAPFN') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1)THEN
                  OUTLYNE=
     1              '"CAPFN" TAKES NO'
                  CALL SHOWIT(1)
                  OUTLYNE='NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  CALL MACFAL
                  RETURN
C       NOT ALL ZERO INPUT , PROCEED
              END IF
          END IF
          IF(TPT.EQ.1.AND.WC.EQ.'PSF') THEN
              IF(S4.EQ.1.OR.S5.EQ.1)THEN
                  OUTLYNE=
     1              '"PSF" TAKES NO NUMERIC WORD #4 TO #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  CALL MACFAL
                  RETURN
C       NOT ALL ZERO INPUT , PROCEED
              END IF
          END IF
          IF(TPT.EQ.1.AND.WC.EQ.'PUPIL') THEN
              IF(S5.EQ.1)THEN
                  OUTLYNE=
     1              '"PUPIL" TAKES NO NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  CALL MACFAL
                  RETURN
C       NOT ALL ZERO INPUT , PROCEED
              END IF
          END IF
C
          SPDEXT=.FALSE.
          GSPDEXT=.FALSE.
          EXTDMTF1=.FALSE.
          EXTDMTF2=.FALSE.
          CPFNEXT=.FALSE.
          CALL DELPSF
C
C     CHECK IF ALL SPECTRAL WEIGHTS ARE ZERO
C
          IF(SYSTEM1(31).LE.0.0D0.AND.SYSTEM1(32).LE.0.0D0
     1    .AND.SYSTEM1(33).LE.0.0D0.AND.SYSTEM1(34).LE.0.0D0
     1    .AND.SYSTEM1(76).LE.0.0D0.AND.SYSTEM1(77).LE.0.0D0
     1    .AND.SYSTEM1(78).LE.0.0D0.AND.SYSTEM1(79).LE.0.0D0
     2    .AND.SYSTEM1(80).LE.0.0D0.AND.SYSTEM1(35).LE.0.0D0) THEN
              IF(TPT.EQ.1) THEN
                  OUTLYNE='ALL SPECTRAL WEIGHTS ARE ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='NO COMPLEX APERTURE FUNCTION WAS GENERATED'
                  CALL SHOWIT(1)
              END IF
              CPFNEXT=.FALSE.
              CALL DELPSF
C     DEALLOCATE DSPOTT
              CALL SPOTIT(1)
              CALL MACFAL
              RETURN
          ELSE
C
C       NOT ALL SPECTRAL WEIGHTS ZERO, CALCULATE NUMBER OF RAYS MAXIMUM
C       PER WAVELENGTH ASSUMING SPECTRAL WEIGHTS NOT ZERO
C
              SPT1=SYSTEM1(31)
              SPT2=SYSTEM1(32)
              SPT3=SYSTEM1(33)
              SPT4=SYSTEM1(34)
              SPT5=SYSTEM1(35)
              SPT6=SYSTEM1(76)
              SPT7=SYSTEM1(77)
              SPT8=SYSTEM1(78)
              SPT9=SYSTEM1(79)
              SPT10=SYSTEM1(80)
              TEMPHOLDER=0
              IF(SPT1.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
              IF(SPT2.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
              IF(SPT3.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
              IF(SPT4.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
              IF(SPT5.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
              IF(SPT6.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
              IF(SPT7.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
              IF(SPT8.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
              IF(SPT9.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
              IF(SPT10.NE.0.0D0) TEMPHOLDER=TEMPHOLDER+1
              IW=INT(DABS(W1))
              IWIW=IW**2
              IWIW2=IWIW*TEMPHOLDER
              TEMPHOLDER=0
C     DEALLOCATE DSPOTT
              CALL SPOTIT(1)
C     ALLOCATE DSPOTT
              NDSPOTT=IWIW2
              CALL SPOTIT(2)
              DO IJ=1,IWIW2
C     ZERO OUT DSPOTT(*,ID)
                  ID=IJ
                  CALL SPOTIT(5)
              END DO

C     FIND THE SHORTEST WAVELENGTH
              SHORT=0
C     FIND THE TOTAL NUMBER OF NON-ZERO WEIGHT WAVELENGTHS
              NUMCOL=0
              WVSHORT=1.0D10
              DO IWL=1,10
C
                  IF(IWL.GE.1.AND.IWL.LE.5) THEN
                      IF(SYSTEM1(30+IWL).NE.0.0D0) THEN
                          NUMCOL=NUMCOL+1
                          IF(SYSTEM1(IWL).LT.WVSHORT) THEN
                              WVSHORT=SYSTEM1(IWL)
                              SHORT=IWL
                          END IF
                      END IF
                  END IF
C
                  IF(IWL.GE.6.AND.IWL.LE.10) THEN
                      IF(SYSTEM1(70+IWL).NE.0.0D0) THEN
                          NUMCOL=NUMCOL+1
                          IF(SYSTEM1(70+IWL-5).LT.WVSHORT) THEN
                              WVSHORT=SYSTEM1(70+IWL-5)
                              SHORT=IWL
                          END IF
                      END IF
                  END IF
              END DO
C     SHORT HAS THE WAVELENGTH NUMBER OF THE SHORTEST WAVELENGTH
C     WVSHORT IS THE SHORTEST WAVELENGTH IN MICROMETER
C
          END IF
          LSF=.FALSE.
C
C       THE LAST FOB INPUT VALUES ARE STORED IN CHLFOB AND
C       THE ARRAY LFOB(1:7).
C
C       NOTE: DSPOT#(12) IS A RAW, UN-NORMALIZED
C       RAY ENERGY TERM BUT IS LATER (WHEN SPA IS CALC.) NORMALIZED
C       TO A FRACTIONAL ENERGY TERM BY DIVISION BY THE WEIGHTING
C       TERM TOT.
C     DOES THE NEWREF SURFACE HAVE A CLAP ON IT ?
          TCLPRF=.FALSE.
          IF(ALENS(9,NEWREF).EQ.0.0D0.OR.ALENS(127,NEWREF).NE.0.0D0) THEN
C     ASSIGN A TEMPORARY CIRCULAR CLAP
              ALENS(9,NEWREF)=1.0D0
              IF(PXTRAY(1,NEWREF).GT.PXTRAX(1,NEWREF)) THEN
                  ALENS(10,NEWREF)=PXTRAY(1,NEWREF)
                  ALENS(11,NEWREF)=ALENS(10,NEWREF)
              ELSE
                  ALENS(10,NEWREF)=PXTRAX(1,NEWREF)
                  ALENS(11,NEWREF)=ALENS(10,NEWREF)
              END IF
              ALENS(12,NEWREF)=0.0D0
              ALENS(13,NEWREF)=0.0D0
              ALENS(14,NEWREF)=0.0D0
              ALENS(15,NEWREF)=0.0D0
              TCLPRF=.TRUE.
              IF(TPT.EQ.1) THEN
                  IF(F28.EQ.1.OR.F31.EQ.1) MSGSPD=.FALSE.
                  IF(DOSTREHL) MSGSPD=.FALSE.
                  IF(MSGSPD)WRITE(OUTLYNE,301) PXTRAY(1,NEWREF)
                  IF(MSGSPD)CALL SHOWIT(0)
                  IF(MSGSPD)WRITE(OUTLYNE,*)
     1            'HAS BEEN ASSIGNED TO THE REFERENCE SURFACE'
                  IF(MSGSPD)CALL SHOWIT(0)
                  IF(WC.EQ.'CAPFN') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'FOR THE CURRENT CAPFN CALCULATIONS'
                  END IF
                  IF(WC.EQ.'PSF') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'FOR THE CURRENT PSF CALCULATIONS'
                  END IF
                  IF(WC.EQ.'PUPIL') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'FOR THE CURRENT PUPIL CALCULATIONS'
                  END IF
                  IF(MSGSPD)CALL SHOWIT(0)
              END IF
 301          FORMAT('TEMPORARY CIRCULAR CLEAR APERTURE = ',G11.4)
          ELSE
              IF(ALENS(9,NEWREF).EQ.2.0D0) THEN
                  IF(ALENS(11,NEWREF).GT.ALENS(10,NEWREF)) THEN
                      ADJUSTW1=ALENS(11,NEWREF)/ALENS(10,NEWREF)
                      ADJUSTW2=1.0D0
                  ELSE
                      ADJUSTW1=1.0D0
                      ADJUSTW2=ALENS(10,NEWREF)/ALENS(11,NEWREF)
                  END IF
              END IF
              IF(ALENS(9,NEWREF).EQ.3.0D0) THEN
                  IF(ALENS(11,NEWREF).GT.ALENS(10,NEWREF)) THEN
                      ADJUSTW1=ALENS(11,NEWREF)/ALENS(10,NEWREF)
                      ADJUSTW2=1.0D0
                  ELSE
                      ADJUSTW1=1.0D0
                      ADJUSTW2=ALENS(10,NEWREF)/ALENS(11,NEWREF)
                  END IF
              END IF
              IF(ALENS(9,NEWREF).EQ.4.0D0) THEN
                  IF(ALENS(11,NEWREF).GT.ALENS(10,NEWREF)) THEN
                      ADJUSTW1=ALENS(11,NEWREF)/ALENS(10,NEWREF)
                      ADJUSTW2=1.0D0
                  ELSE
                      ADJUSTW1=1.0D0
                      ADJUSTW2=ALENS(10,NEWREF)/ALENS(11,NEWREF)
                  END IF
              END IF
              IF(ALENS(9,NEWREF).EQ.5.0D0) THEN
                  ADJUSTW1=1.0D0
                  ADJUSTW2=1.0D0
              END IF
              IF(ALENS(9,NEWREF).EQ.6.0D0) THEN
                  ADJUSTW1=1.0D0
                  ADJUSTW2=1.0D0
              END IF
          END IF
C
C       IWL COUNTS THROUGH THE 10 WAVELENGTH NUMBERS
          I=1
          IJK=0
          DO IWL=1,10
              IF(IWL.GE.1.AND.IWL.LE.5) SPT=SYSTEM1(30+IWL)
              IF(IWL.GE.6.AND.IWL.LE.10) SPT=SYSTEM1(75+IWL-5)
              IF(SPT.GT.0.0D0) THEN
C     ONLY TRACE RAYS FOR NON-ZERO SPECTRAL WEIGHTS
C       TRACE RAYS AT THAT WAVELENGTH
C       W1 IS EVEN
                  NSTART=0
                  NSTOP=INT(DABS(W1))-1
C
                  IF(SPT.NE.0.0D0) THEN
                      IF(TPT.EQ.1) THEN
                          IF(F28.EQ.0.AND.F31.EQ.0) THEN
                              IF(PERFECT) THEN
                                  IF(MSGSPD) WRITE(OUTLYNE,201)IWIW,IWL
                                  IF(MSGSPD) CALL SHOWIT(0)
                              ELSE
                                  IF(MSGSPD) WRITE(OUTLYNE,200)IWIW,IWL
                                  IF(MSGSPD) CALL SHOWIT(0)
                              END IF
                          END IF
                      END IF
                      IJK=IJK+1
                      IF(IWL.LE.5) IWLIJK(IJK)=SYSTEM1(IWL)
                      IF(IWL.GT.5) IWLIJK(IJK)=SYSTEM1(IWL+65)
                  END IF
 200              FORMAT('TRACING ',I10,' RAYS AT WAVELENGTH ',I1)
 201              FORMAT('TRACING ',I10,' "PERFECT" RAYS AT WAVELENGTH ',I1)
                  DELFOB=2.0D0/DBLE(W1)
                  DO IY=NSTART,NSTOP
                      DO IX=NSTART,NSTOP
                          I=I+1
C
C       THE CALL TO RAYTRA2 HAS INPUTS:
C               QUALIFIER
                          WWQ='CAOB'
C       WW1 IS Y THE RELATIVE APERTURE HT OF THE RAY
C               WW1
                          DDEELL=1.0D0
                          IF(WC.EQ.'PSF') DDEELL=0.9999D0
                          IF(WC.EQ.'PUPIL') DDEELL=0.9999D0
                          WW1=(-1.0+(DELFOB/2.0D0))+(DBLE(IY)*DELFOB)
                          WW2=(-1.0+(DELFOB/2.0D0))+(DBLE(IX)*DELFOB)
                          WW1=WW1*ADJUSTW1
                          WW2=WW2*ADJUSTW2
                          WW3=IWL
                          WVN=IWL
C               CACOCH IS SET TO 1 FOR SPOT DIAGRAMS
                          CACOCH = 1
                          SPDTRA=.TRUE.
                          MSG = .FALSE.
                          STOPP=0
C
C THE NEWOBJ,NEWREF AND NEWIMG ARE SET BY FOB
C       TRACE RAY AND RETURN
                          DSPOT(1)=0.0D0
                          DSPOT(2)=0.0D0
                          DSPOT(3)=0.0D0
                          DSPOT(4)=0.0D0
                          DSPOT(5)=0.0D0
                          DSPOT(6)=0.0D0
                          DSPOT(7)=0.0D0
                          DSPOT(8)=0.0D0
                          DSPOT(9)=0.0D0
                          DSPOT(10)=0.0D0
                          DSPOT(11)=0.0D0
                          DSPOT(12)=0.0D0
                          DSPOT(13)=0.0D0
                          DSPOT(14)=0.0D0
                          DSPOT(15)=0.0D0
                          DSPOT(16)=0.0D0
                          DSPOT(17)=0.0D0
                          DSPOT(18)=0.0D0
                          DSPOT(19)=0.0D0
                          DSPOT(20)=0.0D0
                          DSPOT(21)=0.0D0
                          DSPOT(22)=0.0D0
                          DSPOT(23)=0.0D0
                          DSPOT(24)=0.0D0
                          DSPOT(25)=0.0D0
                          DSPOT(26)=0.0D0
                          DSPOT(27)=0.0D0
                          DSPOT(28)=0.0D0
                          DSPOT(29)=0.0D0
                          DSPOT(30)=0.0D0
                          DSPOT(31)=0.0D0
                          DSPOT(32)=0.0D0
                          DSPOT(33)=0.0D0
                          DSPOT(34)=0.0D0
                          DSPOT(35)=0.0D0
C
                          DSPOT(35)=WEI(IWL)
C     THIS NEXT PART IS ONLY USED WHEN DOING A PSF NOT AN MTF
C     NOW FIX THE RAY AIMING COORDINATES IF THE WAVELENGTH IS NOT
C     THE SHORTEST WAVELENGTH SO THAT THE SPACING WILL BE THE SAME IN THE PSF
                          IF(IWL.NE.SHORT.AND.WC.EQ.'PSF'.OR.IWL.EQ.SHORT.AND.WC
     1                    .EQ.'PUPIL') THEN
C     INCREASE THE WW1 AND WW2 BY THE RATIO OD THE CURRENT WAVELENGTH
C     DIVIDED BY THE SHORTEST WAVELENGTH
                              IF(IWL.LE.5) LAMFACTOR=SYSTEM1(IWL)/WVSHORT
                              IF(IWL.GT.5) LAMFACTOR=SYSTEM1(IWL+65)/WVSHORT
                              WW1=WW1*LAMFACTOR*NRDFACTOR
                              WW2=WW2*LAMFACTOR*NRDFACTOR
                          END IF
                          ANAAIM=.FALSE.
                          WW4=1.0D0
                          WW1=WW1*DDEELL
                          WW2=WW2*DDEELL
                          NOCOAT=.FALSE.
                          GRASET=.FALSE.
                          DXFSET=.FALSE.
                          CALL RAYTRA2
                          SPDCD1=RAYCOD(1)
                          SPDCD2=RAYCOD(2)
                          CACOCH = 0
                          ANAAIM=.TRUE.
                          DSPOT(7)=DBLE(SPDCD1)
                          IF(NOOB.AND.DSPOT(7).EQ.7) DSPOT(7)=0.0D0
                          DSPOT(8)=DBLE(SPDCD2)
                          IF(DSPOT(7).NE.0.0D0) GO TO 1941
                          SPDTRA=.FALSE.
                          DSPOT(1)=RAYRAY(1,NEWIMG)
                          DSPOT(2)=RAYRAY(2,NEWIMG)
                          DSPOT(3)=RAYRAY(3,NEWIMG)
C     RAY SLOPES
                          X=RAYRAY(11,NEWIMG)
                          Y=RAYRAY(12,NEWIMG)
                          CALL SLOPES
                          DSPOT(9)=X
                          DSPOT(10)=Y
                          DSPOT(11)=RAYRAY(25,NEWIMG)
                          IF(APODGAUSS) THEN
                              APODX2=-DLOG(10.0D0**(-DABS(APODDBLOSS)/10.0D0))
                              APODR2=(WW1**2)+(WW2**2)
                              DSPOT(11)=DSPOT(11)*DEXP(-APODX2*APODR2)
                          END IF
C       NOW CALCULATE THE RAY ENERGY TERM
C     THIS IS SPECTRAL WEIGHT TIMES APODIZATION FACTOR FOR THIS RAY
C
                          DSPOT(12)=(DSPOT(11))
                          DSPOT(34)=(DSPOT(11))
                          IF(DSPOT(7).NE.0.0D0) DSPOT(12)=0.0D0
                          DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
C       KEEP TRACK OF THE TOTAL OPL ALONE THE RAY FROM NEWOBJ TO NEWI
                          DSPOT(13)=RAYRAY(22,NEWIMG)
C               J=14 X RAY COORD AT TANGENT PLANE OF NEWOBJ+1
                          DSPOT(14)=RAYRAY(1,NEWOBJ+1)
C               J=15 Y RAY COORD AT TANGENT PLANE OF NEWOBJ+1
                          DSPOT(15)=RAYRAY(2,NEWOBJ+1)
C               J=18 Z RAY COORD AT TANGENT PLANE OF NEWOBJ+1
                          DSPOT(18)=RAYRAY(3,NEWOBJ+1)
C               J=5 X COORD OF RAY AT NEWREF
                          DSPOT(5)=RAYRAY(1,NEWREF)
C               J=6 Y COORD OF RAY AT NEWREF
                          DSPOT(6)=RAYRAY(2,NEWREF)
C
                          DSPOT(16)=DBLE(IWL)
                          DSPOT(19)=RAYRAY(19,NEWOBJ+1)
                          DSPOT(20)=RAYRAY(20,NEWOBJ+1)
                          DSPOT(21)=RAYRAY(21,NEWOBJ+1)
                          DSPOT(22)=RAYRAY(19,NEWIMG)
                          DSPOT(23)=RAYRAY(20,NEWIMG)
                          DSPOT(24)=RAYRAY(21,NEWIMG)
                          DSPOT(25)=RAYRAY(9,NEWIMG)
                          DSPOT(26)=RAYRAY(10,NEWIMG)
                          DSPOT(27)=RAYRAY(1,NEWIMG-1)
                          DSPOT(28)=RAYRAY(2,NEWIMG-1)
                          DSPOT(29)=RAYRAY(3,NEWIMG-1)
                          DSPOT(30)=RAYRAY(19,NEWIMG-1)
                          DSPOT(31)=RAYRAY(20,NEWIMG-1)
                          DSPOT(32)=RAYRAY(21,NEWIMG-1)


C
                          IF(IWL.GE.1.AND.IWL.LE.5)
     1                    DSPOT(17)=SYSTEM1(30+IWL)
                          IF(IWL.GE.6.AND.IWL.LE.10)
     1                    DSPOT(17)=SYSTEM1(75+IWL-5)
C
C     OPD CALCULATION
                          OOPD=0.0D0
                          CURLAM=DBLE(IWL)
                          IF(DSPOT(7).EQ.0.0D0) CALL SPOPD1
                          IF(DSPOT(7).NE.0.0D0) OOPD=0.0D0
                          IF(DSPOT(7).NE.0.0D0) OPDW=0.0D0
                          IF(PERFECT) OOPD=0.0D0
                          DSPOT(4)=OOPD
                          DSPOT(33)=OOPD
C
 1941                     DSPOT(16)=DBLE(IWL)
C
                          IF(IWL.GE.1.AND.IWL.LE.5)
     1                    DSPOT(17)=SYSTEM1(30+IWL)
                          IF(IWL.GE.6.AND.IWL.LE.10)
     1                    DSPOT(17)=SYSTEM1(75+IWL-5)
C     LOAD DSPOTT(*,ID) WITH DSPOT(*)
                          ID=I-1
                          CALL SPOTIT(3)
                      END DO
                  END DO
C       SPECTRAL WEIGHT WAS ZERO, GO TO NEXT WAVELENGTH
C     NO FILE OUTPUT WAS DONE
              END IF
          END DO
          ITOT=I
C     ALL THE RAYS HAVE BEEN TRACED, NOW DO THE STATISTICS AND
C     OUTPUT.
          IF(TPT.EQ.1) THEN
              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  IF(WC.EQ.'CAPFN') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'CAPFN RAY TRACING COMPLETED'
                  END IF
                  IF(WC.EQ.'PSF') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'PSF RAY TRACING COMPLETED'
                  END IF
                  IF(WC.EQ.'PUPIL') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'PUPIL RAY TRACING COMPLETED'
                  END IF
                  IF(MSGSPD)CALL SHOWIT(0)
              END IF
          END IF
          IF(TCLPRF) THEN
C     REMOVE TEMP CLAP ON NEWREF
              TCLPRF=.FALSE.
              ALENS(9,NEWREF)=0.0D0
              ALENS(10,NEWREF)=0.0D0
              ALENS(11,NEWREF)=0.0D0
              ALENS(12,NEWREF)=0.0D0
              ALENS(13,NEWREF)=0.0D0
              ALENS(14,NEWREF)=0.0D0
              ALENS(15,NEWREF)=0.0D0
              IF(TPT.EQ.1) THEN
                  IF(F28.EQ.1.OR.F31.EQ.1) MSGSPD=.FALSE.
                  IF(DOSTREHL) MSGSPD=.FALSE.
                  IF(MSGSPD)WRITE(OUTLYNE,301) PXTRAY(1,NEWREF)
                  IF(MSGSPD)CALL SHOWIT(0)
                  IF(MSGSPD)WRITE(OUTLYNE,*)
     1            'HAS BEEN REMOVED FROM THE REFERENCE SURFACE'
                  IF(MSGSPD)CALL SHOWIT(0)
                  IF(WC.EQ.'CAPFN') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'FOR THE CURRENT CAPFN CALCULATIONS'
                  END IF
                  IF(WC.EQ.'PSF') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'FOR THE CURRENT PSF CALCULATIONS'
                  END IF
                  IF(WC.EQ.'PUPIL') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'FOR THE CURRENT PUPIL CALCULATIONS'
                  END IF
                  IF(MSGSPD)CALL SHOWIT(0)
              END IF
          ELSE
          END IF
          IF(TPT.EQ.1) THEN
              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  IF(WC.EQ.'CAPFN') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'PERFORMING "CAPFN" CALCULATIONS...'
                  END IF
                  IF(WC.EQ.'PSF') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'PERFORMING "PSF" CALCULATIONS...'
                  END IF
                  IF(WC.EQ.'PUPIL') THEN
                      IF(MSGSPD)WRITE(OUTLYNE,*)'PERFORMING "PUPIL" CALCULATIONS...'
                  END IF
                  IF(MSGSPD)CALL SHOWIT(0)
              END IF
          END IF
C
C CENTROID OF SPOT CALCULATIONS
          SPA=0.0D0
          SPC=0.0D0
          AFSPB=0.0D0
          AFSPD=0.0D0
C       TOT IS THE WEIGTHED TOTAL
C       NUMTOT IS THE RAW TOTAL OF NON-FAILED RAYS
          TOT=0.0D0
          NUMTOT=0
          NUMT0=0
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
          DO IJ=1,ITOT-1
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IJ
              CALL SPOTIT(4)
              SPT=DSPOT(17)
              APFAC=DSPOT(11)
C     SPA IS A WEIGHTED SUMMATION OF THE X COORDINATE OF A RAY
C     IN THE SPOT DIAGRAM, WEIGHTED BY THE SPECTRAL WEIGHT
C     AND THE APODIZATION FACTOR
C     SPC IS FOR THE Y COMPONENT
C     AFSPB IS THE WEIGHTED X SLOPE ANGLE (IN XZ PLANE)
C     AFSPD IS THE WEIGHTED Y SLOPE ANGLE (IN YZ PLANE)
C
              IF(DSPOT(12).NE.0.0D0) NUMT0=NUMT0+1
C
              IF(SPT.NE.0.0D0) THEN
                  IF(DSPOT(7).EQ.0.0D0) THEN
                      APFAC=DSPOT(11)
                      SPA=SPA+(SPT*DSPOT(1)*APFAC)
                      SPC=SPC+(SPT*DSPOT(2)*APFAC)
                      AFSPB=AFSPB+(SPT*(DSPOT(9))*APFAC)
                      AFSPD=AFSPD+(SPT*(DSPOT(10))*APFAC)
C     RAY FAILED
                  END IF
C     SPT = 0
              END IF
              IF(DSPOT(12).NE.0.0D0) THEN
                  TOT=TOT+(DSPOT(17)*DSPOT(11))
                  NUMTOT=NUMTOT+1
              END IF
          END DO
          IF(NUMTOT.EQ.0) THEN
              IF(MSGSPD)WRITE(OUTLYNE,*)'NO RAYS COULD BE SUCCESSFULLY TRACED'
              IF(MSGSPD) CALL SHOWIT(1)
              PSFEXT=.FALSE.
              CPFNEXT=.FALSE.
C     DEALLOCATE DSPOTT
              CALL SPOTIT(1)
              CALL MACFAL
              RETURN
          END IF
C       W IS THE NORMALIZING FACTOR FOR THE SPOT CALCULATIONS
          W=TOT
          JK_TRANS=TOT/DBLE(NUMTOT)*100.0D0
          SPA=SPA/W
          SPC=SPC/W
          AFSPB=AFSPB/W
          AFSPD=AFSPD/W
C     NOW SPA,SPC,AFSPB AND AFSPD ARE NORMALIZED
C
C     CALCULATE CENTROID LOCATIONS
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C     FOCAL SYSTEMS
              CENTX=SPA
              CENTY=SPC
          ELSE
C     AFOCAL SYSTEMS
              CENTX=AFSPB
              CENTY=AFSPD
          END IF
C     GET RID OF REDICULOIUSLY SMALL VALUES
          IF(DABS(CENTX).EQ.1.0D-15) CENTX=0.0D0
          IF(DABS(CENTY).LT.1.0D-15) CENTY=0.0D0
C
C     NOW DO PART 2 OF THE OPD CALCULATIONS
C     THIS TAKES CARE OF ENTRANCE AND EXIT REFERENCE SPHERES
          RMSOPD=0.0D0
          RMSOP(0:10)=0.0D0
          SUMONE(0:10)=0.0D0
          SUMTWO(0:10)=0.0D0
          DO IJ=1,ITOT-1
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IJ
              CALL SPOTIT(4)
              IF(DSPOT(7).EQ.0.0D0) THEN
                  OOPD=DSPOT(4)
                  IF(PERFECT) OOPD=0.0D0
                  CURLAM=DSPOT(16)
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
                  ID=IJ
                  CALL SPOTIT(4)
                  OOPD=DSPOT(4)
                  IF(PERFECT) OOPD=0.0D0
                  CURLAM=DSPOT(16)
C     NOW FINISH THE OPD CALCULATION
                  CALL SPOPD2(REFERR,TPT)
                  IF(REFERR) THEN
                      CPFNEXT=.FALSE.
                      CALL DELPSF
                      RETURN
                  END IF
C     HERE WE MULTIPLY IN 2PI FOR THE LATER PUPIL FUNCTION
                  IF(PERFECT) OPDW=0.0D0
                  IF(DABS(OPDW).LT.1.0D-4) THEN
                      OPDW=0.0D0
                      OOPD=0.0D0
                  END IF
                  DSPOT(4)=TWOPII*OPDW
                  DSPOT(33)=TWOPII*OPDW
              ELSE
                  OOPD=0.0D0
                  OPDW=0.0D0
                  DSPOT(4)=0.0D0
                  DSPOT(33)=0.0D0
              END IF
C     LOAD DSPOTT(*,ID) WITH DSPOT(*)
              ID=IJ
              CALL SPOTIT(3)
          END DO
          NP1=IW
          NNTOT=ITOT
          CALL ENEXFT(NNTOT)
          DSPOT(12)=DSPOT(12)*DSPOT(34)
          DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
C
          IF(SYSTEM1(30).LE.2.0D0.AND.REFLOC.EQ.3.OR.
     1    SYSTEM1(30).LE.2.0D0.AND.REFLOC.EQ.4) THEN
              ALLOCATE(DSPO(1:35,1:ITOT-1),STAT=ALLOERR)
              DO IJ=1,ITOT-1
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
                  ID=IJ
                  CALL SPOTIT(4)
                  DO J=1,35
                      DSPO(J,IJ)=DSPOT(J)
                  END DO
              END DO
              IF(REFLOC.EQ.3) CALL WAVESLP1(DSPO,ITOT-1,3)
              IF(REFLOC.EQ.4) CALL WAVESLP1(DSPO,ITOT-1,4)
              DO IJ=1,ITOT-1
                  DO J=1,35
                      DSPOT(J)=DSPO(J,IJ)
                  END DO
C     LOAD DSPOTT(*,ID) WITH DSPOT(*)
                  ID=IJ
                  CALL SPOTIT(3)
              END DO
              DEALLOCATE(DSPO,STAT=ALLOERR)
          END IF
          DO IJ=1,ITOT-1
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=IJ
              CALL SPOTIT(4)
              IF(DSPOT(12).NE.0.0D0) THEN
                  SUMONE(0)=SUMONE(0)+(DSPOT(4)/(TWOPII))
                  SUMTWO(0)=SUMTWO(0)+((DSPOT(4)/(TWOPII))**2)
                  NUMT0=NUMT0+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.1.0D0) THEN
                  SUMONE(1)=SUMONE(1)+(DSPOT(4)/(TWOPII))
                  SUMTWO(1)=SUMTWO(1)+((DSPOT(4)/(TWOPII))**2)
                  NUMT1=NUMT1+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.2.0D0) THEN
                  SUMONE(2)=SUMONE(2)+(DSPOT(4)/(TWOPII))
                  SUMTWO(2)=SUMTWO(2)+((DSPOT(4)/(TWOPII))**2)
                  NUMT2=NUMT2+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.3.0D0) THEN
                  SUMONE(3)=SUMONE(3)+(DSPOT(4)/(TWOPII))
                  SUMTWO(3)=SUMTWO(3)+((DSPOT(4)/(TWOPII))**2)
                  NUMT3=NUMT3+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.4.0D0) THEN
                  SUMONE(4)=SUMONE(4)+(DSPOT(4)/(TWOPII))
                  SUMTWO(4)=SUMTWO(4)+((DSPOT(4)/(TWOPII))**2)
                  NUMT4=NUMT4+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.5.0D0) THEN
                  SUMONE(5)=SUMONE(5)+(DSPOT(4)/(TWOPII))
                  SUMTWO(5)=SUMTWO(5)+((DSPOT(4)/(TWOPII))**2)
                  NUMT5=NUMT5+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.6.0D0) THEN
                  SUMONE(6)=SUMONE(6)+(DSPOT(4)/(TWOPII))
                  SUMTWO(6)=SUMTWO(6)+((DSPOT(4)/(TWOPII))**2)
                  NUMT6=NUMT6+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.7.0D0) THEN
                  SUMONE(7)=SUMONE(7)+(DSPOT(4)/(TWOPII))
                  SUMTWO(7)=SUMTWO(7)+((DSPOT(4)/(TWOPII))**2)
                  NUMT7=NUMT7+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.8.0D0) THEN
                  SUMONE(8)=SUMONE(8)+(DSPOT(4)/(TWOPII))
                  SUMTWO(8)=SUMTWO(8)+((DSPOT(4)/(TWOPII))**2)
                  NUMT8=NUMT8+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.9.0D0) THEN
                  SUMONE(9)=SUMONE(9)+(DSPOT(4)/(TWOPII))
                  SUMTWO(9)=SUMTWO(9)+((DSPOT(4)/(TWOPII))**2)
                  NUMT9=NUMT9+1
              END IF
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.10.0D0) THEN
                  SUMONE(10)=SUMONE(10)+(DSPOT(4)/(TWOPII))
                  SUMTWO(10)=SUMTWO(10)+((DSPOT(4)/(TWOPII))**2)
                  NUMT10=NUMT10+1
              END IF
C
              IF(DSPOT(12).NE.0.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(0))
     1            OPPEAK(0)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(0))
     1            OPPIT(0)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.1.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(1))
     1            OPPEAK(1)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(1))
     1            OPPIT(1)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.2.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(2))
     1            OPPEAK(2)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(2))
     1            OPPIT(2)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.3.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(3))
     1            OPPEAK(3)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(3))
     1            OPPIT(3)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.4.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(4))
     1            OPPEAK(4)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(4))
     1            OPPIT(4)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.5.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(5))
     1            OPPEAK(5)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(5))
     1            OPPIT(5)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.6.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(6))
     1            OPPEAK(6)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(6))
     1            OPPIT(6)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.7.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(7))
     1            OPPEAK(7)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(7))
     1            OPPIT(7)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.8.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(8))
     1            OPPEAK(8)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(8))
     1            OPPIT(8)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.9.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(9))
     1            OPPEAK(9)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(9))
     1            OPPIT(9)=DSPOT(4)/(TWOPII)
              END IF
C
              IF(DSPOT(12).NE.0.0D0.AND.DSPOT(16).EQ.10.0D0) THEN
                  IF((DSPOT(4)/(TWOPII)).GE.OPPEAK(10))
     1            OPPEAK(10)=DSPOT(4)/(TWOPII)
                  IF((DSPOT(4)/(TWOPII)).LE.OPPIT(10))
     1            OPPIT(10)=DSPOT(4)/(TWOPII)
              END IF
          END DO
C
C       THE NUMBER OF RAYS WHICH REPRESENT 100% OF THE ENERGY
C       EQUAL THE NUMBER OF RAYS TRACED (IWIW) TIMES THE NUNBER
C       OF NON-ZERO SPECTRAL WEIGHTS.
C
          IF(NUMT0.NE.0.0D0) THEN
              ARG=((SUMTWO(0)-((SUMONE(0)**2)/
     1        DBLE(NUMT0)))/DBLE(NUMT0-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(0)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT1.NE.0.0D0) THEN
              ARG=((SUMTWO(1)-((SUMONE(1)**2)/
     1        DBLE(NUMT1)))/DBLE(NUMT1-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(1)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT2.NE.0.0D0) THEN
              ARG=((SUMTWO(2)-((SUMONE(2)**2)/
     1        DBLE(NUMT2)))/DBLE(NUMT2-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(2)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT3.NE.0.0D0) THEN
              ARG=((SUMTWO(3)-((SUMONE(3)**2)/
     1        DBLE(NUMT3)))/DBLE(NUMT3-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(3)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT4.NE.0.0D0) THEN
              ARG=((SUMTWO(4)-((SUMONE(4)**2)/
     1        DBLE(NUMT4)))/DBLE(NUMT4-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(4)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT5.NE.0.0D0) THEN
              ARG=((SUMTWO(5)-((SUMONE(5)**2)/
     1        DBLE(NUMT5)))/DBLE(NUMT5-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(5)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT6.NE.0.0D0) THEN
              ARG=((SUMTWO(6)-((SUMONE(6)**2)/
     1        DBLE(NUMT6)))/DBLE(NUMT6-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(6)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT7.NE.0.0D0) THEN
              ARG=((SUMTWO(7)-((SUMONE(7)**2)/
     1        DBLE(NUMT7)))/DBLE(NUMT7-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(7)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT8.NE.0.0D0) THEN
              ARG=((SUMTWO(8)-((SUMONE(8)**2)/
     1        DBLE(NUMT8)))/DBLE(NUMT8-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(8)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT9.NE.0.0D0) THEN
              ARG=((SUMTWO(9)-((SUMONE(9)**2)/
     1        DBLE(NUMT9)))/DBLE(NUMT9-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(9)=DSQRT(ARG)
              END IF
          END IF
          IF(NUMT10.NE.0.0D0) THEN
              ARG=((SUMTWO(10)-((SUMONE(10)**2)/
     1        DBLE(NUMT10)))/DBLE(NUMT10-1))
              IF(ARG.LT.0.0D0) THEN
                  OUTLYNE=
     1            'RMSOPD ERROR OCCURED, CAPFN CALCULATION HAS FAILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
C     DEALLOCATE DSPOTT
                  CALL SPOTIT(1)
                  RETURN
              ELSE
                  RMSOP(10)=DSQRT(ARG)
              END IF
          END IF
C     NOW THE POLY CHROMATIC RMSOPD
          IF(LAMAVE.NE.0.0D0) THEN
C     CALC SUMSIG
              SUMSIG=0.0D0
              DO ISS=1,10
                  IF(WEI(ISS).NE.0.0D0.AND.RMSOP(ISS).NE.0.0D0.AND.
     1            LAM(ISS).NE.0.0D0)
     1            SUMSIG=SUMSIG+((WEI(ISS)*(RMSOP(ISS)**2))/(LAM(ISS)**2))
              END DO
          END IF
          SIG=0.0D0
          IF(SUML2.NE.0.0D0.AND.SUMSIG.NE.0.0D0)
     1    RMSOPD=DSQRT(SUMSIG/SUML2)
          RMSOP(0)=RMSOPD
C
C
C       TOT IS THE SUM OF ALL THE ENERGY FOR THE NON-FAILED RAYS
C
C               IF(TOT.EQ.0.0D0) THEN
C     IF(TPT.EQ.1) THEN
C       OUTLYNE='NO RAYS COULD GET THROUGH THE SYSTEM'
C     CALL SHOWIT(1)
C     IF(WC.EQ.'CAPFN')
C    1OUTLYNE='NO CAPFN WAS CREATED'
C     IF(WC.EQ.'PSF')
C    1OUTLYNE='NO PSF WAS CREATED'
C     IF(WC.EQ.'PUPIL')
C    1OUTLYNE='NO PUPIL WAS CREATED'
C     CALL SHOWIT(1)
C              END IF
C                       CALL MACFAL
C                      CPFNEXT=.FALSE.
C              CALL DELPSF
C     DEALLOCATE DSPOTT
C     CALL SPOTIT(1)
C                       RETURN
C                       END IF
C     PUT THE RMSOPD IN THE ACCUMULATOR
          REG(40)=REG(9)
          REG(9)=RMSOPD
          OPPTV(0)=OPPEAK(0)-OPPIT(0)
          OPPTV(1)=OPPEAK(1)-OPPIT(1)
          OPPTV(2)=OPPEAK(2)-OPPIT(2)
          OPPTV(3)=OPPEAK(3)-OPPIT(3)
          OPPTV(4)=OPPEAK(4)-OPPIT(4)
          OPPTV(5)=OPPEAK(5)-OPPIT(5)
          OPPTV(6)=OPPEAK(6)-OPPIT(6)
          OPPTV(7)=OPPEAK(7)-OPPIT(7)
          OPPTV(8)=OPPEAK(8)-OPPIT(8)
          OPPTV(9)=OPPEAK(9)-OPPIT(9)
          OPPTV(10)=OPPEAK(10)-OPPIT(10)
          PTOVOPD(0)=OPPTV(0)
          PTOVOPD(1)=OPPTV(1)
          PTOVOPD(2)=OPPTV(2)
          PTOVOPD(3)=OPPTV(3)
          PTOVOPD(4)=OPPTV(4)
          PTOVOPD(5)=OPPTV(5)
          PTOVOPD(6)=OPPTV(6)
          PTOVOPD(7)=OPPTV(7)
          PTOVOPD(8)=OPPTV(8)
          PTOVOPD(9)=OPPTV(9)
          PTOVOPD(10)=OPPTV(10)
          IF(DABS(PTOVOPD(0)).GT.1.0D15) PTOVOPD(0)=0.0D0
          IF(DABS(PTOVOPD(1)).GT.1.0D15) PTOVOPD(1)=0.0D0
          IF(DABS(PTOVOPD(2)).GT.1.0D15) PTOVOPD(2)=0.0D0
          IF(DABS(PTOVOPD(3)).GT.1.0D15) PTOVOPD(3)=0.0D0
          IF(DABS(PTOVOPD(4)).GT.1.0D15) PTOVOPD(4)=0.0D0
          IF(DABS(PTOVOPD(5)).GT.1.0D15) PTOVOPD(5)=0.0D0
          IF(DABS(PTOVOPD(6)).GT.1.0D15) PTOVOPD(6)=0.0D0
          IF(DABS(PTOVOPD(7)).GT.1.0D15) PTOVOPD(7)=0.0D0
          IF(DABS(PTOVOPD(8)).GT.1.0D15) PTOVOPD(8)=0.0D0
          IF(DABS(PTOVOPD(9)).GT.1.0D15) PTOVOPD(9)=0.0D0
          IF(DABS(PTOVOPD(10)).GT.1.0D15) PTOVOPD(10)=0.0D0
C     WE HAVE PEAK TO VALLEY OPD (OPPTV) IN WAVES
C     IF WE HAVE NORE THAN LAMBDA/2 WAVES PER GRID IN THE NRD
C     THEN THE CAPFN IS FOR SHIT AND WE BETTER NOT CONTINUE
C     WITH ANY CALCULATION
          IF(DABS(OPPTV(0)/(DBLE(NRD))).GE.0.25D0) THEN
              IF(TPT.EQ.1) THEN
                  IF(F28.EQ.1.OR.F31.EQ.1.OR..NOT.MSGSPD) THEN
                  ELSE
                      OUTLYNE='CAUTION MESSAGE ONLY:'
                      CALL SHOWIT(1)
                      OUTLYNE='PUPIL FUCTION MIGHT NOT BE SAMPLED SUFFICIENTLY'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'POSSIBLY USE A DENSER RAY GRID'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'USE CAUTION INTERPRETING DIFFRACTION BASED RESULTS.'
                      CALL SHOWIT(1)
                  END IF
              END IF
          ELSE
              IF(DABS(OPPTV(0)).GE.1.0D0) THEN
                  IF(TPT.EQ.1) THEN
                      IF(F28.EQ.1.OR.F31.EQ.1.OR..NOT.MSGSPD) THEN
                      ELSE
                          OUTLYNE='CAUTION MESSAGE ONLY:'
                          CALL SHOWIT(1)
                          OUTLYNE='(P-V) OPD EXCEEDS 1.0 WAVE,'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'USE CAUTION INTERPRETING DIFFRACTION BASED RESULTS.'
                          CALL SHOWIT(1)
                      END IF
                  END IF
              END IF
          END IF
C
          SPDEXT=.FALSE.
          CPFNEXT=.TRUE.
C
C     IT IS NOW 4/20/93
C     DSPOTT CONTAINS THE X AND Y COORDINATES
C     OF EACH RAY AT THE REFERENCE SURFACE IN DSPOT(5) AND DSPOT(6)
C     THE RAY ENERGY TERM FOR THE RAY, ADJUSTED FOR PUPIL ABERRATIONS
C     AND SOLID ANGLE SUBTENT AS SEEN FROM THE CENTER OF THE REFERENCE
C     SPHERE IS STORED IN DSPOT(12)
C     THE 2pi*OPD OF THE RAY IS STORED IN DSPOT(4). IT IS REPRESENTED IN
C     WAVES AT WAVELENGTH (WAV) WHERE WAV IS EQUAL TO THE WAVELENGTH
C     AT WHICH THE LAST CHIEF RAY WAS TRACED. ITS UNIT ARE RADIANS
C
C     REMEMBER THE SHORTEST WAVELENGTH, SHORTEST WAVELENGTH #,
C     TOTAL NUMBER OF COLORS, THE LAST FOB DATA, THE GRID SIZE
          RETURN
      END
C SUB DOTF.FOR
      SUBROUTINE DOTF
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE DOTF.FOR.
C     CALLED BY CMDER FOR COMMAND DOTF
C     THIS DOES DIFFRACTION OTF CALCULATIONS
C
          INTEGER DORI1,DORI2,N,I,J,IX,IY,SHTNM,K,L,M,II,IG,IIG
     1    ,SSSQS,IJ,ALLOERR,IJK,SS1,SS2,SSN,DDFF1,DDFF2
          COMMON/GII/IIG
C
          LOGICAL TFSTOPTF,ERROR
C
          LOGICAL MULTIOTF
          COMMON/OTFMULTI/MULTIOTF
C
          COMMON/STOPTF/TFSTOPTF
C
          CHARACTER*8 WWWQW
C
          REAL*8 SUM1,SUM2,CUTFRX,CUTFRY,JK_TEMP,COFACT
     1    ,WVNUM,FREQFACT,X,Y1,Y2,SHRTWV
     2    ,IWLIJK(1:10)
C
          LOGICAL REFERR,EXTDMTF1,EXTDMTF2,PERF
          COMMON/DMTFEXT/EXTDMTF1,EXTDMTF2,PERF
C
          COMMON/PEPITO/IWLIJK,IJK
C
          REAL*8 YP1,YPN,CUTFR,SPTT
     1    ,NORMI,DOTFR1,DOTFI1,DOTFM1,DOTFP1,XFR
     3    ,DOTFM0,DOTFP0,DOTFI0,DOTFR0,XFR0
C
          REAL DDTAF(1:101),DDTAM(1:101,0:21),DDTAP(1:101,0:21)
C
          COMMON/DOTFPAS/DDTAF,DDTAM,DDTAP,N,DORI1,CUTFRX,CUTFRY
     1    ,DORI2
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          REAL*8 PUPL,SUMMER1,SUMMER2,FREQ,XY12,XY22,FREQ1,FREQ2,AL,
     1    WWWW1,WWWW2,DOTFR,DOTFI,DOTFP,DOTFM,FR
          DIMENSION PUPL(:,:,:),SUMMER1(:),SUMMER2(:),FREQ(:),XY12(:),
     1    XY22(:),DOTFI(:),DOTFM(:),DOTFR(:),DOTFP(:),FR(:)
          ALLOCATABLE :: PUPL,SUMMER1,SUMMER2,FREQ,XY12,XY22,DOTFI,DOTFR
     1    ,DOTFP,DOTFM,FR
C
          COFACT=COHERENCE_FACTOR
          CALL SORTFIELDS
          IF(SYSTEM1(6).EQ.1.0D0) AL=DABS(ALENS(3,NEWOBJ))*25.4D0
          IF(SYSTEM1(6).EQ.2.0D0) AL=DABS(ALENS(3,NEWOBJ))*10.0D0
          IF(SYSTEM1(6).EQ.3.0D0) AL=DABS(ALENS(3,NEWOBJ))
          IF(SYSTEM1(6).EQ.4.0D0) AL=DABS(ALENS(3,NEWOBJ))*1000.0D0
C
C     DETERMINE NEAR FOR FAR
          IF(NEAR_FAR.EQ.0) NEAR=.TRUE.
          IF(NEAR_FAR.EQ.1) NEAR=.FALSE.
          IF(NEAR.AND.SPACEBALL.EQ.1.AND.AL.GE.
     1    1.0D5) THEN
              OUTLYNE='NO DOTF CAN BE PERFORMED WHEN SPACE IS SET TO "O"'
              CALL SHOWIT(1)
              OUTLYNE='WITH "NEAR" AND WITH AN OBJECT DISTANCE'
              CALL SHOWIT(1)
              OUTLYNE='GREATER THAN 1.0D5 MILLIMETERS.'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          ERROR=.FALSE.
          CALL CUTTOFF(FREQ1,FREQ2,ERROR)
C
          IF(ERROR) THEN
              OUTLYNE='ERROR IN OBJECT/IMAGE SPACE FREQUENCY RELATIONSHIP'
              CALL SHOWIT(1)
              OUTLYNE='NO DOTF CALCULATION OCCURED'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     FREQ1 IS THE IMAGE  SPACE CUTOFF FREQ
C     FREQ2 IS THE OBJECT SPACE CUTOFF FREQ
          IF(SPACEBALL.EQ.1) THEN
              CUTFRY=FREQ2
              CUTFRX=FREQ2
              CUTFR=FREQ2
          ELSE
              CUTFRY=FREQ1
              CUTFRX=FREQ1
              CUTFR=FREQ1
          END IF
          CUTFRY=CUTFRY
          CUTFRX=CUTFRX
          CUTFR=CUTFR
C     CUTFR HAS BEEN COMPUTED
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"DOTF" CALCULATES A DIFFRACTION OTF'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(SST.EQ.1) THEN
              OUTLYNE='"DOTF" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'Y'.AND.WQ.NE.'X'.AND.WQ.NE.'YACC'.AND.
     1        WQ.NE.'XACC') THEN
                  OUTLYNE='"DOTF" QUALIFIER MUST BE "Y", "X", "YACC" OR "XACC"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(WQ.EQ.'ACC'.AND.DF1.EQ.1) THEN
              OUTLYNE='"DOTF ACC" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'XACC'.AND.DF1.EQ.1) THEN
              OUTLYNE='"DOTF XACC" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'YACC'.AND.DF1.EQ.1) THEN
              OUTLYNE='"DOTF YACC" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(.NOT.REFEXT) THEN
              OUTLYNE='NO CHIEF RAY EXISTS, NO "DOTF" MAY BE PERFORMED'
              CALL SHOWIT(1)
              IIG=1
              CALL MACFAL
              RETURN
          END IF
c
          IF(.NOT.CPFNEXT) THEN
              wwww1=w1
              wwww2=w2
              ddff1=df1
              ddff2=df2
              ss1=s1
              ss2=s2
              ssn=sn
              WWWQW=WQ
              SSSQS=SQ
              SAVE_KDP(1)=SAVEINPT(1)
              WC='CAPFN'
              SQ=0
              SST=0
              STI=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              SN=1
              W1=CAPDEF
              REFERR=.FALSE.
              NRDFACTOR=1.0D0
              OLDIF=LDIF
              LDIF=.FALSE.
              CALL COMPAP(REFERR,1)
              LDIF=OLDIF
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.CPFNEXT.OR.REFERR) THEN
                  OUTLYNE='"NO COMPLEX APERTURE FUNCTION COULD NOT BE COMPUTED"'
                  CALL SHOWIT(1)
                  OUTLYNE='"DOTF" NOT POSSIBLE WITH EXISTING SYSTEM'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              w1=wwww1
              w2=wwww2
              df1=ddff1
              df2=ddff2
              s1=ss1
              s2=ss2
              sn=ssn
              sq=SSSQS
              WQ=WWWQW
              sst=0
              sti=0
          END IF
C
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"DOTF" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1) W2=10.0D0
          IF(W2.LT.3.0D0.OR.W2.GT.100.0D0) THEN
              OUTLYNE=
     1        'VALID NUMBER OF FREQUENCY POINTS RANGES FROM 3 TO 100'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          N=INT(W2)
          IF(DF1.EQ.0) THEN
              IF(W1.LT.0.0D0) THEN
                  OUTLYNE=
     1            'FREQUENCY MUST BE GREATER THAN 0.0D0'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          EXTDMTF1=.FALSE.
          EXTDMTF2=.FALSE.
C
C     CALC THE SHRTWAVE
          SHRTWAVE=0.0D0
          IF(SYSTEM1(1).NE.0.0D0.AND.SYSTEM1(31).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(1)
              GO TO 314
          END IF
          IF(SYSTEM1(2).NE.0.0D0.AND.SYSTEM1(32).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(2)
              GO TO 314
          END IF
          IF(SYSTEM1(3).NE.0.0D0.AND.SYSTEM1(33).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(3)
              GO TO 314
          END IF
          IF(SYSTEM1(4).NE.0.0D0.AND.SYSTEM1(34).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(4)
              GO TO 314
          END IF
          IF(SYSTEM1(5).NE.0.0D0.AND.SYSTEM1(35).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(5)
              GO TO 314
          END IF
          IF(SYSTEM1(71).NE.0.0D0.AND.SYSTEM1(76).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(71)
              GO TO 314
          END IF
          IF(SYSTEM1(72).NE.0.0D0.AND.SYSTEM1(77).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(72)
              GO TO 314
          END IF
          IF(SYSTEM1(73).NE.0.0D0.AND.SYSTEM1(78).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(73)
              GO TO 314
          END IF
          IF(SYSTEM1(74).NE.0.0D0.AND.SYSTEM1(79).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(74)
              GO TO 314
          END IF
          IF(SYSTEM1(75).NE.0.0D0.AND.SYSTEM1(80).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(75)
              GO TO 314
          END IF
 314      CONTINUE
          IF(SHRTWAVE.EQ.0.0D0) THEN
              OUTLYNE='WAVELENGTHS ARE ALL ZERO'
              CALL SHOWIT(1)
              OUTLYNE='NO "DOTF" CAN BE CALCULATED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SYSTEM1(1).LE.SHRTWAVE
     1    .AND.SYSTEM1(1).NE.0.0D0.AND.SYSTEM1(31).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(1)
              SHTNM=1
          END IF
          IF(SYSTEM1(2).LE.SHRTWAVE
     1    .AND.SYSTEM1(2).NE.0.0D0.AND.SYSTEM1(32).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(2)
              SHTNM=2
          END IF
          IF(SYSTEM1(3).LE.SHRTWAVE
     1    .AND.SYSTEM1(3).NE.0.0D0.AND.SYSTEM1(33).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(3)
              SHTNM=3
          END IF
          IF(SYSTEM1(4).LE.SHRTWAVE
     1    .AND.SYSTEM1(4).NE.0.0D0.AND.SYSTEM1(34).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(4)
              SHTNM=4
          END IF
          IF(SYSTEM1(5).LE.SHRTWAVE
     1    .AND.SYSTEM1(5).NE.0.0D0.AND.SYSTEM1(35).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(5)
              SHTNM=5
          END IF
          IF(SYSTEM1(71).LE.SHRTWAVE
     1    .AND.SYSTEM1(71).NE.0.0D0.AND.SYSTEM1(76).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(71)
              SHTNM=6
          END IF
          IF(SYSTEM1(72).LE.SHRTWAVE
     1    .AND.SYSTEM1(72).NE.0.0D0.AND.SYSTEM1(77).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(72)
              SHTNM=7
          END IF
          IF(SYSTEM1(73).LE.SHRTWAVE
     1    .AND.SYSTEM1(73).NE.0.0D0.AND.SYSTEM1(78).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(73)
              SHTNM=8
          END IF
          IF(SYSTEM1(74).LE.SHRTWAVE
     1    .AND.SYSTEM1(74).NE.0.0D0.AND.SYSTEM1(79).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(74)
              SHTNM=9
          END IF
          IF(SYSTEM1(75).LE.SHRTWAVE
     1    .AND.SYSTEM1(75).NE.0.0D0.AND.SYSTEM1(80).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(75)
              SHTNM=10
          END IF
          SHRTWV=SHRTWAVE
          SHRTWAVE=(SHRTWAVE*1.0D-3)
C
          IF(DF1.EQ.0.AND.W1.GT.CUTFR) THEN
              OUTLYNE='REQUESTED FREQUENCY IS GREATER THAN THE CUTOFF FREQUENCY'
              CALL SHOWIT(1)
              OUTLYNE='NO DOTF CALCULATION CAN BE PERFORMED'
              CALL SHOWIT(1)
              TFSTOPTF=.TRUE.
              CALL MACFAL
              RETURN
          ELSE
              TFSTOPTF=.FALSE.
          END IF
C
          DEALLOCATE (
     1    PUPL,SUMMER1,SUMMER2,FREQ
     1    ,XY12,XY22,STAT=ALLOERR)
          ALLOCATE (
     1    PUPL(IW,IW,2),SUMMER1(IW+1),SUMMER2(IW+1),FREQ(IW+1)
     1    ,XY12(IW+1),XY22(IW+1),STAT=ALLOERR)
          IWIW=IW**2
C     IF NO QUAL AND W1 INPUT, SET WQ TO 'Y'
          IF(SQ.EQ.0.AND.DF1.EQ.0) WQ='Y'
C
C
C     DO THE IG LOOP
C
          DO IG=1,IIG
C     BEFORE WE START AN IG LOOKM ZERO OUT THE VARIABLES WHICH HOLD THE
C     MODULUS AND PHASE TRANSFER FUNCTIONS
              DEALLOCATE(DOTFR,DOTFI,FR,DOTFM,DOTFP,FREQ,XY12,XY22,STAT=ALLOERR)
              ALLOCATE(DOTFR(N+1),DOTFI(N+1),FR(N+1),DOTFM(N+1)
     1        ,DOTFP(N+1),XY12(IW+1),XY22(IW+1),FREQ(IW+1),STAT=ALLOERR)
              IJ=IW+1
              FREQ(1:IJ)=0.0D0
              XY12(1:IJ)=0.0D0
              XY22(1:IJ)=0.0D0
              IJ=N+1
              FR(1:IJ)=0.0D0
              DOTFR(1:IJ)=0.0D0
              DOTFI(1:IJ)=0.0D0
              DOTFM(1:IJ)=0.0D0
              DOTFP(1:IJ)=0.0D0
              DOTFR1=0.0D0
              DOTFI1=0.0D0
              DOTFR0=0.0D0
              DOTFI0=0.0D0
              DOTFM0=0.0D0
              DOTFP0=0.0D0
              DOTFM1=0.0D0
              DOTFP1=0.0D0
C     VARAIBLES ZEROED
C
              I=2
              IF(IIG.EQ.2.AND.IG.EQ.1.AND.SQ.EQ.0) WQ='Y'
              IF(IIG.EQ.2.AND.IG.EQ.2.AND.SQ.EQ.0) WQ='X'
              DO J=1,NUMCOL
C     DOING A COLOR NOW, ZERO OUT THE SUMMER ARRAYS
C     BEFORE WE DO A COLOR, ZERO THE SUMMER1 AND SUMMER2 ARRAYS
                  IJ=IW+1
                  SUMMER1(1:IJ)=0.0D0
                  SUMMER2(1:IJ)=0.0D0
                  FREQ(1:IJ)=0.0D0
C     ARRAYS FOR A COLOR ZEROED
                  IX=0
                  IY=1
                  II=0

C     WE WANT TO DO IWIW READS AND LOADS OF THE PUPL ARRAY
 10               II=II+1
                  IX=IX+1
                  IF(IX.GT.IW) THEN
                      IY=IY+1
                      IX=1
                  END IF
C     INSTEAD OF READING FROM UNIT 61 FROM RECORD I
C     TRANSFER DATA FROM DSPOTT(*,ID) WITH ID SET TO I-1
                  ID=I-1
                  CALL SPOTIT(4)
                  SPTT=DSPOT(17)
                  I=I+1
C     LOAD AN ARRAY ELEMENT WITH REAL AND IMAGINARY CAPFN
C     FOR A COLOR
C     PUPL IS COL,ROW INDEXED
C     THIS IS THE REAL PART
                  PUPL(IX,IY,1)=DSQRT(DABS(DSPOT(12)))
C
C     THIS IS THE IMAGINARY PART 2*PI*OPD IS FRACTIONS OF A WAVE
C     AT THE REFERENCE WAVELENGTH (UNITS ARE RADIANS)
                  IF(PERFECT) DSPOT(4)=0.0D0
                  PUPL(IX,IY,2)=DSPOT(4)
C     THE PARTIAL COHERENCE FACTOR IS COHERENCE_FACTOR*PII
C     WHERE THE COHERENCE FACTOR CAN RANGE FROM 0=INCOHERENT
C     TO 1=COHERENT
                  WVNUM=DSPOT(16)
                  IF(II.LT.IWIW) GO TO 10
C     FELL THROUGH, FINISHED READING IWIW POINTS
C     THE ARRAYS FOR A COLOR ARE WRITTEN, NOW SHEAR THEM
C     IN Y IF 'Y' WAS THE QUALIFIER OR IN X IF 'X' WAS THE QUALIFIER
C     'Y' MEANS (Y SHEAR) Y-RESPONSE (HORIZONTAL BARS)
C     'X' MEANS (X SHEAR) X-RESPONSE (VERTICAL BARS)
C     OUTPUT THE ROTF AND IOTF AS A FUNCTION OF FRACTIONAL
C     CUTOFF FREQUENCY
C     K COUNTS THE SHIFTS. THE SHIFTS START WITH 0 AND END WITH IW-1
C     THE IWTH SHIFT ALWAYS YIELDS A ZERO TERM
C
                  DO K=0,IW-1
C     SUM1 AND SUM2 ARE RE-INITIALIZED TO ZERO FOR EACH NEW K
                      SUM1=0.0D0
                      SUM2=0.0D0
C
                      IF(WQ.EQ.'X'.OR.WQ.EQ.'XACC') THEN
C     X-RESPONSE (VERTICAL BARS)
C
                          DO L=(1+K),IW
                              DO M=1,IW
                                  SUM1=SUM1
     1                            +((PUPL(L,M,1)*PUPL(L-K,M,1))*
     2                            DCOS(-PUPL(L-K,M,2)+PUPL(L,M,2)))
                                  SUM2=SUM2
     1                            +((PUPL(L,M,1)*PUPL(L-K,M,1))*
     2                            DSIN(-PUPL(L-K,M,2)+PUPL(L,M,2)))
                              END DO
                          END DO
                      END IF
                      IF(WQ.EQ.'Y'.OR.WQ.EQ.'YACC') THEN
C     Y-RESPONSE (HORIZONTAL BARS)


                          DO L=(1+K),IW
                              DO M=1,IW
                                  SUM1=SUM1
     1                            +((PUPL(M,L,1)*PUPL(M,L-K,1))*
     2                            DCOS(-PUPL(M,L-K,2)+PUPL(M,L,2)))
                                  SUM2=SUM2
     1                            +((PUPL(M,L,1)*PUPL(M,L-K,1))*
     2                            DSIN(-PUPL(M,L-K,2)+PUPL(M,L,2)))
                              END DO
                          END DO
                      END IF
C     WRITE THE TERM DOWN AND ZERO THE SUM
                      IF(K.EQ.0.AND.WVNUM.LE.5.0D0) THEN
                          FREQFACT=SHRTWV/SYSTEM1(INT(WVNUM))
                      END IF
                      IF(K.EQ.0.AND.WVNUM.GT.5.0D0) THEN
                          FREQFACT=SHRTWV/SYSTEM1(INT(WVNUM)-5+70)
                      END IF
C
C     WRITE THE SUMS INTO THE STORAGE ARRAYS
C     PARTS OF THE OTF
                      SUMMER1(K+1)=SUM1
                      SUMMER2(K+1)=SUM2
                      IF(WQ(1:1).EQ.'X')
     1                FREQ(K+1)=DABS((DBLE(K)*CUTFRX*FREQFACT)/DBLE(IW))
                      IF(WQ(1:1).EQ.'Y')
     1                FREQ(K+1)=DABS((DBLE(K)*CUTFRY*FREQFACT)/DBLE(IW))
                  END DO
                  IF(WQ(1:1).EQ.'X')
     1            FREQ(IW+1)=CUTFRX*FREQFACT
                  IF(WQ(1:1).EQ.'Y')
     1            FREQ(IW+1)=CUTFRY*FREQFACT
                  SUMMER1(IW+1)=0.0D0
                  SUMMER2(IW+1)=0.0D0
C
C     ALL DATA CALCULATED FOR A WAVELENGTH AND STORED
C     NOW SPLINE FIT THIS DATA, USE THE FIT TO GENERATE "IW" POINTS,
C     MULTIPLY THESE "IW" POINTS BY THE SPECTRAL WEIGHT AND SAVE
C     THE VALUES IN THE DOTFR AND DOTFI ARRAYS
C     AT N POINTS AND SAVE IT
C
C     SPLINE INITIALIZATION
C
                  YP1=1.0D35
                  YPN=1.0D35
                  DO IJ=1,IW+1
                      XY12(IJ)=0.0D0
                      XY22(IJ)=0.0D0
                  END DO
                  CALL SPLINE(FREQ,SUMMER1,IW+1,YP1,YPN,XY12)
                  CALL SPLINE(FREQ,SUMMER2,IW+1,YP1,YPN,XY22)
C
                  IF(DF1.EQ.1) THEN
C     DO A RANGE OF MTFS
C
                      DO IJ=0,N
C
C     CALCULATE X, THE INPUT FREQUENCY
C     N REPRESENTS THE CUTOFF FREQ
C
                          X=DABS((DBLE(IJ)/DBLE(N))*CUTFR)
                          IF(WQ(1:1).EQ.'X') THEN
                              IF(X.LT.(CUTFRX*FREQFACT)) THEN
                                  CALL SPLINT(FREQ,SUMMER1,XY12,IW+1,X,Y1)
                                  CALL SPLINT(FREQ,SUMMER2,XY22,IW+1,X,Y2)
                              ELSE
                                  Y1=0.0D0
                                  Y2=0.0D0
                              END IF
                          END IF
                          IF(WQ(1:1).EQ.'Y') THEN
                              IF(X.LT.(CUTFRY*FREQFACT)) THEN
                                  CALL SPLINT(FREQ,SUMMER1,XY12,IW+1,X,Y1)
                                  CALL SPLINT(FREQ,SUMMER2,XY22,IW+1,X,Y2)
                              ELSE
                                  Y1=0.0D0
                                  Y2=0.0D0
                              END IF
                          END IF

                          DOTFR(IJ+1)=DOTFR(IJ+1)+(Y1*SPTT)
                          DOTFI(IJ+1)=DOTFI(IJ+1)+(Y2*SPTT)
                          FR(IJ+1)=X
                      END DO
                  END IF
                  IF(DF1.EQ.0) THEN

C     JUST DO A SINGLE MTF AT FREQUENCY W1
C
                      XFR0=0.0D0
                      XFR=W1
C
                      CALL SPLINT(FREQ,SUMMER1,XY12,IW+1,XFR0,Y1)
                      CALL SPLINT(FREQ,SUMMER2,XY22,IW+1,XFR0,Y2)
                      DOTFR0=DOTFR0+(Y1*SPTT)
                      DOTFI0=DOTFI0+(Y2*SPTT)
C
                      IF(WQ(1:1).EQ.'X') THEN
                          IF(XFR.LT.(CUTFRX*FREQFACT)) THEN
C
                              CALL SPLINT(FREQ,SUMMER1,XY12,IW+1,XFR,Y1)
                              CALL SPLINT(FREQ,SUMMER2,XY22,IW+1,XFR,Y2)
                          ELSE
                              Y1=0.0D0
                              Y2=0.0D0
                          END IF
                      END IF
                      IF(WQ(1:1).EQ.'Y') THEN
                          IF(XFR.LT.(CUTFRY*FREQFACT)) THEN
C
                              CALL SPLINT(FREQ,SUMMER1,XY12,IW+1,XFR,Y1)
                              CALL SPLINT(FREQ,SUMMER2,XY22,IW+1,XFR,Y2)
                          ELSE
                              Y1=0.0D0
                              Y2=0.0D0
                          END IF
                      END IF
                      DOTFR1=DOTFR1+(Y1*SPTT)
                      DOTFI1=DOTFI1+(Y2*SPTT)
                  END IF
C                     END OF THE COLOR LOOP
              END DO
C
C     DONE WITH ALL COLORS NOW
C
              IF(DF1.EQ.1) THEN
 200              FORMAT('DIFFRACTION OPTICAL TRANSFER FUNCTION')
 201              FORMAT('IMAGE SPACE SPACIAL FREQUENCY UNITS = LP/MM')
 202              FORMAT('OBJECT SPACE SPACIAL FREQUENCY UNITS LP/MM')
 203              FORMAT('IMAGE SPACE SPACIAL FREQUENCY UNITS = LP/MRAD')
 204              FORMAT('OBJECT SPACE SPACIAL FREQUENCY UNITS = LP/MRAD')
 205              FORMAT('Y-RESPONSE (HORIZONTAL BAR TARGET)')
 206              FORMAT('X-RESPONSE (VERTICAL BAR TARGET)')
 507              FORMAT('FREQ.-(lp/mm)',7X,'MODULUS',11X,'PHASE(DEG)')
 508              FORMAT('FREQ.-(lp/mrad)',5X,'MODULUS',11X,'PHASE(DEG)')
 208              FORMAT(G13.6,5X,G13.6,7X,G13.6)
 209              FORMAT('DIFFRACTION LIMIT CASE')
                  WRITE(OUTLYNE,200)
                  CALL SHOWIT(0)
                  IF(PERF) THEN
                      WRITE(OUTLYNE,209)
                      CALL SHOWIT(0)
                  END IF
                  IF(SPACEBALL.EQ.1.AND.NEAR)                WRITE(OUTLYNE,202)
                  IF(SPACEBALL.EQ.2.AND.SYSTEM1(30).LE.2.0D0) WRITE(OUTLYNE,201)
                  IF(SPACEBALL.EQ.1.AND..NOT.NEAR)           WRITE(OUTLYNE,204)
                  IF(SPACEBALL.EQ.2.AND.SYSTEM1(30).GT.2.0D0) WRITE(OUTLYNE,203)
                  CALL SHOWIT(0)
                  IF(WQ(1:1).EQ.'Y') WRITE(OUTLYNE,205)
                  IF(WQ(1:1).EQ.'X') WRITE(OUTLYNE,206)
                  CALL SHOWIT(0)
                  IF(SPACEBALL.EQ.1.AND.NEAR.OR.
     1            SPACEBALL.EQ.2.AND.SYSTEM1(30).LE.2.0D0) WRITE(OUTLYNE,507)
                  IF(SPACEBALL.EQ.1.AND..NOT.NEAR.OR.
     1            SPACEBALL.EQ.2.AND.SYSTEM1(30).GT.2.0D0) WRITE(OUTLYNE,508)
                  CALL SHOWIT(0)
C     DO A RANGE OF VALUES
                  IF(WQ.EQ.'Y'.AND.IG.EQ.1) DORI1=0
                  IF(WQ.EQ.'Y'.AND.IG.EQ.2) DORI2=0
                  IF(WQ.EQ.'X'.AND.IG.EQ.1) DORI1=1
                  IF(WQ.EQ.'X'.AND.IG.EQ.2) DORI2=1
                  IF(IIG.EQ.1) EXTDMTF1=.TRUE.
                  IF(IIG.EQ.2.AND.IG.EQ.1) EXTDMTF1=.TRUE.
                  IF(IIG.EQ.2.AND.IG.EQ.2) EXTDMTF2=.TRUE.
C     INTERPOLATE INTO THE DOTFM/P ARRAYS
                  YP1=1.0D35
                  YPN=1.0D35
C
                  DO IJ=0,N
C
C     CALCULATE X, THE INPUT FREQUENCY
C     N REPRESENTS THE CUTOFF FREQ
C
C     OUTPUT THE DOTFM AND P AND FR ARRAYS AT N+1 POINTS
                      DOTFM(IJ+1)=DSQRT((DOTFR(IJ+1)**2)+(DOTFI(IJ+1)**2))
                      IF(SNGL(DOTFR(IJ+1)).EQ.0.0.AND.SNGL(DOTFI(IJ+1)).NE.0.0) THEN
                          IF(DOTFI(IJ+1).GT.0.0D0) DOTFP(IJ+1)=90.0D0
                          IF(DOTFI(IJ+1).LT.0.0D0) DOTFP(IJ+1)=270.0D0
                      END IF
                      IF(DABS(DOTFI(IJ+1)).LE.1.0D-3) THEN
                          DOTFP(IJ+1)=0.0D0
                      END IF
                      IF(DOTFR(IJ+1).NE.0.0D0.AND.DABS(DOTFI(IJ+1)).GT.1.0D-3) THEN
                          IF(DABS(DOTFI(IJ+1)).EQ.0.0D0.AND.
     1                    DABS(DOTFR(IJ+1)).EQ.0.0D0) THEN
                              JK_TEMP=0.0D0
                          ELSE
                              JK_TEMP=DATAN2(DOTFI(IJ+1),DOTFR(IJ+1))
                          END IF
                          DOTFP(IJ+1)=(JK_TEMP)*(180.0D0/PII)
                          IF(DOTFP(IJ+1).GT.360.0D0) DOTFP(IJ+1)=DOTFP(IJ+1)-360.0D0
                      END IF
                      IF(IJ.EQ.0) NORMI=DOTFM(1)
                      WRITE(OUTLYNE,208) FR(IJ+1),(DOTFM(IJ+1)/NORMI),
     1                DOTFP(IJ+1)
                      CALL SHOWIT(0)
C
                      DDTAF(IJ+1)=SNGL(FR(IJ+1))
                      IF(IG.EQ.1) THEN
                          DDTAM(IJ+1,(OTFPAIR*2))=SNGL((DOTFM(IJ+1)/NORMI))
                          DDTAP(IJ+1,(OTFPAIR*2))=SNGL(DOTFP(IJ+1))
                      END IF
                      IF(IG.EQ.2) THEN
                          DDTAM(IJ+1,(OTFPAIR*2)+1)=SNGL((DOTFM(IJ+1)/NORMI))
                          DDTAP(IJ+1,(OTFPAIR*2)+1)=SNGL(DOTFP(IJ+1))
                      END IF
                  END DO
              ELSE
C     DO A SINGLE VALUE
                  DOTFM0=DSQRT((DOTFR0**2)+(DOTFI0**2))
                  DOTFM1=DSQRT((DOTFR1**2)+(DOTFI1**2))
                  IF(SNGL(DOTFR1).EQ.0.0.AND.SNGL(DOTFI1).NE.0.0) THEN
                      IF(DOTFI1.GT.0.0D0) DOTFP1=90.0D0
                      IF(DOTFI1.LT.0.0D0) DOTFP1=270.0D0
                  END IF
                  IF(DABS(DOTFI1).LE.1.0D-3) THEN
                      DOTFP1=0.0D0
                  END IF
                  IF(DOTFR1.NE.0.0D0.AND.DABS(DOTFI1).GT.1.0D-3) THEN
                      IF(DABS(DOTFI1).EQ.0.0D0.AND.
     1                DABS(DOTFR1).EQ.0.0D0) THEN
                          JK_TEMP=0.0D0
                      ELSE
                          JK_TEMP=DATAN2(DOTFI1,DOTFR1)
                      END IF
                      DOTFP1=(JK_TEMP)*(180.0D0/PII)
                      IF(DOTFP1.GT.360.0D0) DOTFP1=DOTFP1-360.0D0
                  END IF
                  DOTFM1=DOTFM1/DOTFM0
                  IF(WQ(2:4).EQ.'ACC') THEN
C     NO OUTPUT
                      REG(40)=REG(9)
                      REG(11)=XFR
                      REG(9)=DOTFM1
                      REG(10)=DOTFP1
                  ELSE
                      REG(40)=REG(9)
                      REG(11)=XFR
                      REG(10)=DOTFP1
                      REG(9)=DOTFM1
 101                  FORMAT('AT IMAGE SPACE SPACIAL FREQUENCY ',G13.6,' LP/MM')
 102                  FORMAT('AT OBJECT SPACE SPACIAL FREQUENCY ',G13.6,' LP/MM')
 103                  FORMAT('AT IMAGE SPACE SPACIAL FREQUENCY ',G13.6,' LP/MRAD')
 104                  FORMAT('AT OBJECT SPACE SPACIAL FREQUENCY ',G13.6,' LP/MRAD')
C
 111                  FORMAT(
     1                'THE DIFFRACTION MODULATION TRANSFER FUNCTION VALUE = ',G13.6)
 112                  FORMAT(
     1                '     THE DIFFRACTION PHASE TRANSFER FUNCTION VALUE = ',G13.6)
                      IF(WQ(1:1).EQ.'X') WRITE(OUTLYNE,206)
                      IF(WQ(1:1).EQ.'Y') WRITE(OUTLYNE,205)
                      CALL SHOWIT(0)
                      IF(SPACEBALL.EQ.1.AND.NEAR)
     1                WRITE(OUTLYNE,102) XFR
                      IF(SPACEBALL.EQ.2.AND.SYSTEM1(30).LE.2.0D0)
     1                WRITE(OUTLYNE,101) XFR
                      IF(SPACEBALL.EQ.1.AND..NOT.NEAR)
     1                WRITE(OUTLYNE,104) XFR
                      IF(SPACEBALL.EQ.2.AND.SYSTEM1(30).GT.2.0D0)
     1                WRITE(OUTLYNE,103) XFR
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,111) DOTFM1
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,112) DOTFP1
                      CALL SHOWIT(0)
                  END IF
              END IF
          END DO
C     NEXT END DO IS AT END OF IG LOOP
          DEALLOCATE(PUPL,SUMMER1,SUMMER2,FREQ,XY12,XY22
     1    ,DOTFI,DOTFR,DOTFP,DOTFM,STAT=ALLOERR)
          RETURN
      END
