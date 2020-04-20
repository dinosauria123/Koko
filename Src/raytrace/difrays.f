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

C
C       DIFFERENTIAL RAY TRACES
C
C SUB FRFDIF.FOR
      SUBROUTINE FRFDIF
C
C     FULL RAY AIMING PRE 2/10/2006, KEPT AS A BACKUP
C
          IMPLICIT NONE
C
C       DIFFERENTIAL RAY WITH RESPECT TO A CHANGE IN RAY HEIGHT AT
C       SURFACE 0.
C       DIFFERENTIAL RAY TRACE. THIS IS A SET OF
C       CLOSE RAYS TO THE RAY WHICH
C       ARE TRACED
C       FROM THE ORIGINAL OBJECT
C       POINT PLUS SOME DELTA AT NEWOBJECT TOWARD THE
C       CHIEF RAY LOCATION AT SURFACE NEWREF
C
          INTEGER I,KKK
C
          REAL*8 DDELX,DDELY,TARX,TARY,YANG,XANG
     1    ,D11,D12,D21,D22,MF1,MF2,SHIFTX,SHIFTY,HIT,
     2    MAG,LSTART,MSTART,NSTART,XPARTX,YPARTX,XPARTY,YPARTY
     3    ,LOLD,MOLD,NOLD,XC1,YC1,ZC1
     4    ,ZPARTX,ZPARTY
C
          REAL*8 SIG,X,XHT,YHT,ZHT,XSCYFAC,YSCYFAC
     1    ,Y,Z,L,M,N,X1ONE,Y1ONE,
     2    X1LAST,Y1LAST,RXONE,RYONE,RXLAST,RYLAST
C
          LOGICAL DELFAIL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          SREFDIFEXT=.TRUE.
C     PROPER INITIALIZE PROCEEDURE 3/3/96
          R_X=0.0D0
          R_Y=0.0D0
          R_Z=0.0D0
          XOLD=0.0D0
          YOLD=0.0D0
          ZOLD=0.0D0
          LOLD=0.0D0
          MOLD=0.0D0
          NOLD=0.0D0
C
C       NOTE: CHIEF RAY IS ALWAYS AIMED TO THE VERTEX OR LOCAL
C       ORIGIN OF THE REFERENCE SURFACE OR TO THE "CENTER"
C       OF THE CLEAR APERTURE ON THE REFERENCE SURFACE IF THERE IS
C       A CLEAR APERTURE DEFINED. THIS ALLOWS SHIFTING THE AIMPONIT
C       BY DECENTERING THE CLEAR APERTURE!

C       THE REF OBJECT HEIGHTS ARE:
C       (AT THE OBJECT SURFACE)
          XHT=REFRY(1,NEWOBJ)
          YHT=REFRY(2,NEWOBJ)
          ZHT=REFRY(3,NEWOBJ)
C
C       THE FOLLOWING ARE INITIAL VALUES FOR VARIABLES USED IN
C       AIMING THE REFERENCE RAY AT THE CENTER OF THE REFERENCE
C       SURFACE.
C
C       SET INITIAL VALUES FOR X1ONE, Y1ONE, X1LAST AND Y1LAST
C
          X1ONE=-99999.9D0
          Y1ONE=-99999.9D0
          X1LAST=-99999.9D0
          Y1LAST=-99999.9D0
C
C       SET INITIAL VALUES FOR RXONE, RYONE, RXLAST AND RYLAST
          RXONE=-99999.9D0
          RYONE=-99999.9D0
          RXLAST=-99999.9D0
          RYLAST=-99999.9D0
C
          FOB0=0
          DDELX=0.001D0
          DDELY=0.001D0
C
C       FIRST XZ PLANE
          IF(XHT.NE.0.0D0)
     1    SIG=XHT/DABS(XHT)
          IF(XHT.EQ.0.0D0)
     1    SIG=1.0D0
C     WE NEED AN ANGLE WHICH HAS ITS TANGENT EQUAL TO IS RADIAN MEASURE
C     TO 10 DIGITS THIS IS TRUE AT 1 ARC SEC
C
          XSCYFAC=DABS(0.000004848D0*ALENS(3,NEWOBJ))
C
          SHIFTX=XSCYFAC*SIG
          IF(SYSTEM1(16).GE.0.0D0) SHIFTX=DABS(SHIFTX)
          IF(SYSTEM1(16).LT.0.0D0) SHIFTX=-DABS(SHIFTX)
C
          XSTRT=REFRY(1,(NEWOBJ))+(SHIFTX)
          RFDELX=SHIFTX
          YSTRT=REFRY(2,NEWOBJ)
          ZSTRT=REFRY(3,NEWOBJ)
C
C       THESE ARE THE INITIAL AIMING COORDINATES IN THE NEWOBJ+1 SURFACE
C       IN THE LOCAL COORDINATE SYSTEM OF SURFACE NEWOBJ+1
C       THESE COORDINATES NEED TO BE TRANSLATED INTO THE LOCAL
C       COORDINATE SYSTEM OF SURFACE NEWOBJ BEFORE THE STARTING
C       DIRECTION COSINES CAN BE CALCULATED USING BAKONE.FOR
C
          X1AIM=REFRY(1,(NEWOBJ+1))
          Y1AIM=REFRY(2,(NEWOBJ+1))
          Z1AIM=REFRY(3,(NEWOBJ+1))
          XC=X1AIM
          YC=Y1AIM
          ZC=Z1AIM
          IF(XC.LT.0.0D0) DDELX=DABS(DDELX)
          IF(YC.LT.0.0D0) DDELY=DABS(DDELY)
          IF(XC.GE.0.0D0) DDELX=-DABS(DDELX)
          IF(YC.GE.0.0D0) DDELY=-DABS(DDELY)
          XAIMOL=XC
          YAIMOL=YC
          ZAIMOL=ZC
          R_TX=X1AIM
          R_TY=Y1AIM
          R_TZ=Z1AIM
          CALL BAKONE
          X1AIM=R_TX
          Y1AIM=R_TY
          Z1AIM=R_TZ
C
C
C       INITIAL DIRECTION COSINES ARE
          MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)
     1    +((ZSTRT-Z1AIM)**2))
          LSTART=(X1AIM-XSTRT)/MAG
          MSTART=(Y1AIM-YSTRT)/MAG
          NSTART=(Z1AIM-ZSTRT)/MAG
          NINTY=.FALSE.
          IF(NSTART.LT.0.0D0) NINTY=.TRUE.
          IF(NINTY) RVSTART=.TRUE.
          IF(NSTART.EQ.0.0D0) THEN
              YANG=PII/2.0D0
              XANG=PII/2.0D0
          ELSE
              IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  YANG=0.0D0
              ELSE
                  YANG=DATAN2(MSTART,NSTART)
              END IF
              IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  XANG=0.0D0
              ELSE
                  XANG=DATAN2(LSTART,NSTART)
              END IF
          END IF
          X=XSTRT
          Y=YSTRT
          Z=ZSTRT
          L=LSTART
          M=MSTART
          N=NSTART
C
          KKK=0
C       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
C       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
C       CHIEF RAY AIMING.
C
 9        CONTINUE
          IF(ALENS(3,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
          IF(ALENS(3,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
          RV=.FALSE.
C
C       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
C       SURFACE ZERO POINT INTERSECTION
          KKK=KKK+1
          REFEXT=.TRUE.
          STOPP=0
C        PROCEED
C
          IF(KKK.NE.1) THEN
C
              MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)
     1        +((ZSTRT-Z1AIM)**2))
              LSTART=(X1AIM-XSTRT)/MAG
              MSTART=(Y1AIM-YSTRT)/MAG
              NSTART=(Z1AIM-ZSTRT)/MAG
              NINTY=.FALSE.
              IF(NSTART.LT.0.0D0) NINTY=.TRUE.
              IF(NINTY) RVSTART=.TRUE.
              IF(NSTART.EQ.0.0D0) THEN
                  YANG=PII/2.0D0
                  XANG=PII/2.0D0
              ELSE
                  IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      YANG=0.0D0
                  ELSE
                      YANG=DATAN2(MSTART,NSTART)
                  END IF
                  IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      XANG=0.0D0
                  ELSE
                      XANG=DATAN2(LSTART,NSTART)
                  END IF
              END IF
              X=XSTRT
              Y=YSTRT
              Z=ZSTRT
              L=LSTART
              M=MSTART
              N=NSTART
C       KKK=1
          END IF
          LOLD=LSTART
          MOLD=MSTART
          NOLD=NSTART
          RFDIFF(1,NEWOBJ)=X
          RFDIFF(2,NEWOBJ)=Y
          RFDIFF(3,NEWOBJ)=Z
          RFDIFF(4,NEWOBJ)=L
          RFDIFF(5,NEWOBJ)=M
          RFDIFF(6,NEWOBJ)=N
          RFDIFF(13,NEWOBJ)=LOLD
          RFDIFF(14,NEWOBJ)=MOLD
          RFDIFF(15,NEWOBJ)=NOLD
          DO 10 I=NEWOBJ+1,NEWIMG

              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              CALL TRNSF2
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              STOPP=0
              REFEXT=.TRUE.
              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              LOLD=L
              MOLD=M
              NOLD=N
              R_XAIM=XAIMOL
              R_YAIM=YAIMOL
              R_ZAIM=ZAIMOL
              IF(REFRY(24,R_I-1).GT.0.0D0) POSRAY=.TRUE.
              IF(REFRY(24,R_I-1).LT.0.0D0) POSRAY=.FALSE.
              CALL HITSUR
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(STOPP.EQ.1) THEN
                  IF(MSG) THEN
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                  'RAY FAILURE OCCURRED AT SURFACE ',I
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'WHILE TRACING THE XZ-PLANE CHIEF DIFFERENTIAL RAY'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  SREFDIFEXT=.FALSE.
                  RETURN
              ELSE
                  STOPP=0
                  REFEXT=.TRUE.
              END IF
C       CHECK THE RAY HEIGHT AT
C       NEWREF AND ADJUST THE YAIMOL AND XAIMOL AT SURFACE
C       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWREF IS WITHIN
C       .001 AIMTOL . MAXIMUN NUMBER OF ITERRATIONS IS
C       100.
C
              IF(I.EQ.NEWREF) THEN
C       CALCULATE TARX AND TARY
C     AIM POINT IS SAME AS THAT OF THE ORIGINAL REFRAY JUST TRACED
C
                  TARY=REFRY(2,NEWREF)
                  TARX=REFRY(1,NEWREF)
C
                  IF((0.001D0*AIMTOL).LE.1.0D-10) HIT=1.0D-10
                  IF((0.001D0*AIMTOL).GT.1.0D-10) HIT=0.001D0*AIMTOL
                  IF(KKK.GT.2
     1            .OR.SYSTEM1(62).EQ.0.0D0) THEN
C       AIM IS GOOD ENOUGH, PROCEED
                      GO TO 100
C       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                  END IF
C
C       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
                  X1ONE=X1LAST
                  Y1ONE=Y1LAST
C       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
                  X1LAST=XAIMOL
                  Y1LAST=YAIMOL
C       SET RXONE AND RYONE TO RXLAST AND RYLAST
                  RXONE=RXLAST
                  RYONE=RYLAST
C       SET RXLAST AND RYLAST TO X AND Y
                  RXLAST=X
                  RYLAST=Y
C       X AND Y ARE THE CURRENT RAY COORDINATES AT THE
C       REFERENCE SURFACE.
C
C       GUESS CASE 1, REPRESENTS THE FIRST REFINEMENT
C       OF THE AIMING POINT.
C       THIS OCCURS IF KKK=1
                  IF(KKK.EQ.1) THEN
C       THIS IS CASE 1
C       IN THIS CASE WE SET THE SURFACE 1 AIMING POINTS
C       EQUAL TO THE OLD AIM POINTS PLUS A SMALL DELTA
C       IN ORDER TO CALCULATE DERIVATIVES.
C
                      X1AIM=XAIMOL+DDELX
                      Y1AIM=YAIMOL+DDELY
                      Z1AIM=ZAIMOL
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                      IF(ALENS(1,1).NE.0.0D0)
     1                CALL GETZEE1
                      X1AIM=XC
                      Y1AIM=YC
                      Z1AIM=ZC
C
C       CALCULATE NEW SET OF VALUES FOR DERIVATIVES
C
                      XAIMOL=XC1
                      YAIMOL=YC1
                      ZAIMOL=ZC1
                      R_TX=X1AIM
                      R_TY=Y1AIM
                      R_TZ=Z1AIM
                      CALL BAKONE
                      X1AIM=R_TX
                      Y1AIM=R_TY
                      Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
                      GO TO 9
C       THIS IS NOT THE FIRST REFINEMENT, KKK NOT = 1
                  END IF
C
                  CALL RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE
     1            ,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
C
C       MF1=TARX-RXLAST, MF2=TARY-RYLAST
C
C       TARX AND TARY ARE THE COORDINATES OF THE CENTER
C       OF THE DECENTERED CLEAR APERTURE ON THE REFERENCE
C       SURFACE IF THERE IS A CLEAR APERTURE DEFINED AND IT
C       IS DECENTERED. IF THESE CONDITIONS DO NOT EXIST,
C       TARY AND TARX ARE BOTH IDENTICALLY 0.0D0.
C
C
                  MF1=TARX-RXLAST
                  MF2=TARY-RYLAST
                  DELFAIL=.FALSE.
                  CALL NEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL)
                  IF(DELFAIL) THEN
                      RETURN
                  ELSE
                      GO TO 9
                  END IF
C       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
              END IF
C
 100          CONTINUE
C
C       THE COORDINATES OF THE DIFFERENTIAL RAY AT NEWIMG ARE:
C                       X,Y,Z WITH DIRECTION COSINES
C                       L,M,N
C       NOW RENAME THEM TO:
              RFDIFF(1,I)=X
              RFDIFF(2,I)=Y
              RFDIFF(3,I)=Z
              RFDIFF(4,I)=L
              RFDIFF(5,I)=M
              RFDIFF(6,I)=N
              RFDIFF(13,I)=LOLD
              RFDIFF(14,I)=MOLD
              RFDIFF(15,I)=NOLD
 10       CONTINUE
C
          DDELX=0.001D0
          DDELY=0.001D0
C       SECOND YZ PLANE
C
          X1ONE=-99999.9D0
          Y1ONE=-99999.9D0
          X1LAST=-99999.9D0
          Y1LAST=-99999.9D0
C
C       SET INITIAL VALUES FOR RXONE, RYONE, RXLAST AND RYLAST
          RXONE=-99999.9D0
          RYONE=-99999.9D0
          RXLAST=-99999.9D0
          RYLAST=-99999.9D0
C
          IF(YHT.NE.0.0D0) SIG=YHT/DABS(YHT)
          IF(YHT.EQ.0.0D0) SIG=1.0D0
C     WE NEED AN ANGLE WHICH HAS ITS TANGENT EQUAL TO IS RADIAN MEASURE
C     TO 10 DIGITS THIS IS TRUE AT 1 ARC SEC
C
          YSCYFAC=DABS(0.000004848D0*ALENS(3,NEWOBJ))
C
          SHIFTY=YSCYFAC*SIG
          IF(SYSTEM1(14).GE.0.0D0) SHIFTY=DABS(SHIFTY)
          IF(SYSTEM1(14).LT.0.0D0) SHIFTY=-DABS(SHIFTY)
C
          YSTRT=REFRY(2,(NEWOBJ))+(SHIFTY)
          RFDELY=SHIFTY
          XSTRT=REFRY(1,NEWOBJ)
          ZSTRT=REFRY(3,NEWOBJ)
          X1AIM=REFRY(1,(NEWOBJ+1))
          Y1AIM=REFRY(2,(NEWOBJ+1))
          Z1AIM=REFRY(3,(NEWOBJ+1))
          XC=X1AIM
          YC=Y1AIM
          ZC=Z1AIM
          IF(XC.LT.0.0D0) DDELX=DABS(DDELX)
          IF(YC.LT.0.0D0) DDELY=DABS(DDELY)
          IF(XC.GE.0.0D0) DDELX=-DABS(DDELX)
          IF(YC.GE.0.0D0) DDELY=-DABS(DDELY)
          XAIMOL=XC
          YAIMOL=YC
          ZAIMOL=ZC
          R_TX=X1AIM
          R_TY=Y1AIM
          R_TZ=Z1AIM
          CALL BAKONE
          X1AIM=R_TX
          Y1AIM=R_TY
          Z1AIM=R_TZ
C
C    INITIAL DIRECTION COSINES ARE
          MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)
     1    +((ZSTRT-Z1AIM)**2))
          LSTART=(X1AIM-XSTRT)/MAG
          MSTART=(Y1AIM-YSTRT)/MAG
          NSTART=(Z1AIM-ZSTRT)/MAG
          NINTY=.FALSE.
          IF(NSTART.LT.0.0D0) NINTY=.TRUE.
          IF(NINTY) RVSTART=.TRUE.
          IF(NSTART.EQ.0.0D0) THEN
              YANG=PII/2.0D0
              XANG=PII/2.0D0
          ELSE
              IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  YANG=0.0D0
              ELSE
                  YANG=DATAN2(MSTART,NSTART)
              END IF
              IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  XANG=0.0D0
              ELSE
                  XANG=DATAN2(LSTART,NSTART)
              END IF
          END IF
          X=XSTRT
          Y=YSTRT
          Z=ZSTRT
          L=LSTART
          M=MSTART
          N=NSTART
          KKK=0
C       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
C       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
C       CHIEF RAY AIMING.
C
 19       CONTINUE
          IF(ALENS(3,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
          IF(ALENS(3,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
          RV=.FALSE.
C
C       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
C       SURFACE ZERO POINT INTERSECTION
          KKK=KKK+1
          STOPP=0
          REFEXT=.TRUE.
C        PROCEED
C
          IF(KKK.NE.1) THEN
C    INITIAL DIRECTION COSINES ARE
              MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)
     1        +((ZSTRT-Z1AIM)**2))
              LSTART=(X1AIM-XSTRT)/MAG
              MSTART=(Y1AIM-YSTRT)/MAG
              NSTART=(Z1AIM-ZSTRT)/MAG
              NINTY=.FALSE.
              IF(NSTART.LT.0.0D0) NINTY=.TRUE.
              IF(NINTY) RVSTART=.TRUE.
              IF(NSTART.EQ.0.0D0) THEN
                  YANG=PII/2.0D0
                  XANG=PII/2.0D0
              ELSE
                  IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      YANG=0.0D0
                  ELSE
                      YANG=DATAN2(MSTART,NSTART)
                  END IF
                  IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      XANG=0.0D0
                  ELSE
                      XANG=DATAN2(LSTART,NSTART)
                  END IF
              END IF
              X=XSTRT
              Y=YSTRT
              Z=ZSTRT
              L=LSTART
              M=MSTART
              N=NSTART
C       KKK=1
          END IF
          LOLD=LSTART
          MOLD=MSTART
          NOLD=NSTART
          RFDIFF(7,NEWOBJ)=X
          RFDIFF(8,NEWOBJ)=Y
          RFDIFF(9,NEWOBJ)=Z
          RFDIFF(10,NEWOBJ)=L
          RFDIFF(11,NEWOBJ)=M
          RFDIFF(12,NEWOBJ)=N
          RFDIFF(16,NEWOBJ)=LOLD
          RFDIFF(17,NEWOBJ)=MOLD
          RFDIFF(18,NEWOBJ)=NOLD
          DO 20 I=NEWOBJ+1,NEWIMG

              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              CALL TRNSF2
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              STOPP=0
              REFEXT=.TRUE.
C
              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              LOLD=L
              MOLD=M
              NOLD=N
              R_XAIM=XAIMOL
              R_YAIM=YAIMOL
              R_ZAIM=ZAIMOL
              IF(REFRY(24,R_I-1).GT.0.0D0) POSRAY=.TRUE.
              IF(REFRY(24,R_I-1).LT.0.0D0) POSRAY=.FALSE.
              CALL HITSUR
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(STOPP.EQ.1) THEN
                  IF(MSG) THEN
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                  'RAY FAILURE OCCURRED AT SURFACE ',I
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'WHILE TRACING THE YZ-PLANE CHIEF DIFFERENTIAL RAY'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  SREFDIFEXT=.FALSE.
                  RETURN
              ELSE
                  STOPP=0
                  REFEXT=.TRUE.
              END IF
C       CHECK THE RAY HEIGHT AT
C       NEWREF AND ADJUST THE YAIMOL AND XAIMOL AT SURFACE
C       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWREF IS WITHIN
C       .001 AIMTOL. MAXIMUN NUMBER OF ITERRATIONS IS
C       100.
C
              IF(I.EQ.NEWREF) THEN
C       CALCULATE TARX AND TARY
                  IF(ALENS(9,I).GE.1.0D0.AND.ALENS(9,I).LE.6.0D0.AND.
     1            ALENS(127,I).EQ.0.0D0) THEN
C       REF SURF HAS CLAP ON IT
C       SET TARGET TO CENTER OF DECENTERED CLAP, ALENS(12,I),
C       AND ALENS(13,I) ARE CLAP DECENTRATIONS
C       HERE IS WERE THE TARGET FOR RAY AIMING IS SET FOR THE
C       CHIEF RAY. A SIMILAR BY MORE COMPLEX SETTING IS REQUIRED
C       FOR NON-CHIEF RAYS IN THE SUBROUTINE RAYTRA.FOR
                      TARY=ALENS(12,I)
                      TARX=ALENS(13,I)
                  ELSE
C       NO CLAP OF REF SURF.
                      TARX=0.0D0
                      TARY=0.0D0
                  END IF
C
                  IF((0.001D0*AIMTOL).LE.1.0D-10) HIT=1.0D-10
                  IF((0.001D0*AIMTOL).GT.1.0D-10) HIT=0.001D0*AIMTOL
                  IF(KKK.GT.2
     1            .OR.SYSTEM1(62).EQ.0.0D0) THEN
C       AIM IS GOOD ENOUGH, PROCEED
                      GO TO 1200
C       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                  END IF
C
C       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
                  X1ONE=X1LAST
                  Y1ONE=Y1LAST
C       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
                  X1LAST=XAIMOL
                  Y1LAST=YAIMOL
C       SET RXONE AND RYONE TO RXLAST AND RYLAST
                  RXONE=RXLAST
                  RYONE=RYLAST
C       SET RXLAST AND RYLAST TO X AND Y
                  RXLAST=X
                  RYLAST=Y
C       X AND Y ARE THE CURRENT RAY COORDINATES AT THE
C       REFERENCE SURFACE.
C
C       GUESS CASE 1, REPRESENTS THE FIRST REFINEMENT
C       OF THE AIMING POINT.
C       THIS OCCURS IF KKK=1
                  IF(KKK.EQ.1) THEN
C       THIS IS CASE 1
C       IN THIS CASE WE SET THE SURFACE 1 AIMING POINTS
C       EQUAL TO THE OLD AIM POINTS PLUS A SMALL DDELTA
C       IN ORDER TO CALCULATE DERIVATIVES.
C
                      X1AIM=XAIMOL+DDELX
                      Y1AIM=YAIMOL+DDELY
                      Z1AIM=ZAIMOL
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                      IF(ALENS(1,1).NE.0.0D0)
     1                CALL GETZEE1
                      X1AIM=XC
                      Y1AIM=YC
                      Z1AIM=ZC
C
C       CALCULATE NEW SET OF VALUES FOR DERIVATIVES
C
                      XAIMOL=XC1
                      YAIMOL=YC1
                      ZAIMOL=ZC1
                      R_TX=X1AIM
                      R_TY=Y1AIM
                      R_TZ=Z1AIM
                      CALL BAKONE
                      X1AIM=R_TX
                      Y1AIM=R_TY
                      Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
                      GO TO 19
C       THIS IS NOT THE FIRST REFINEMENT, KKK NOT = 1
                  END IF
C
                  CALL RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE
     1            ,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
C
C       MF1=TARX-RXLAST, MF2=TARY-RYLAST
C
C       TARX AND TARY ARE THE COORDINATES OF THE CENTER
C       OF THE DECENTERED CLEAR APERTURE ON THE REFERENCE
C       SURFACE IF THERE IS A CLEAR APERTURE DEFINED AND IT
C       IS DECENTERED. IF THESE CONDITIONS DO NOT EXIST,
C       TARY AND TARX ARE BOTH IDENTICALLY 0.0D0.
C
C
                  MF1=TARX-RXLAST
                  MF2=TARY-RYLAST
                  DELFAIL=.FALSE.
                  CALL NEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL)
                  IF(DELFAIL) THEN
                      RETURN
                  ELSE
                      GO TO 19
                  END IF
C       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
              END IF
C
 1200         CONTINUE
C
C       THE COORDINATES OF THE DIFFERENTIAL RAY AT NEWIMG ARE:
C                       X,Y,Z WITH DIRECTION COSINES
C                       L,M,N
              RFDIFF(7,I)=X
              RFDIFF(8,I)=Y
              RFDIFF(9,I)=Z
              RFDIFF(10,I)=L
              RFDIFF(11,I)=M
              RFDIFF(12,I)=N
              RFDIFF(16,I)=LOLD
              RFDIFF(17,I)=MOLD
              RFDIFF(18,I)=NOLD
C
C       STORE THESES DIFFERENTIALS
C
 20       CONTINUE
          DO I=NEWOBJ,NEWIMG
              XPARTX=RFDIFF(1,I)-REFRY(1,I)
              YPARTX=RFDIFF(2,I)-REFRY(2,I)
              ZPARTX=RFDIFF(3,I)-REFRY(3,I)
              IF(XPARTX.EQ.0.0D0.AND.YPARTX.EQ.0.0D0.AND.I.NE.0)
     1        POLANGX(I)=-POLANGX(I-1)
              IF(XPARTX.EQ.0.0D0.AND.YPARTX.GT.0.0D0) POLANGX(I)=PII/2.0D0
              IF(XPARTX.EQ.0.0D0.AND.YPARTX.LT.0.0D0) POLANGX(I)=-PII/2.0D0
C
              IF(YPARTX.EQ.0.0D0.AND.XPARTX.GT.0.0D0) POLANGX(I)=0.0D0
              IF(YPARTX.EQ.0.0D0.AND.XPARTX.LT.0.0D0) POLANGX(I)=PII
              IF(XPARTX.NE.0.0D0.AND.YPARTX.NE.0.0D0)
     1        POLANGX(I)=DATAN2(XPARTX,YPARTX)
              XPARTY=RFDIFF(7,I)-REFRY(1,I)
              YPARTY=RFDIFF(8,I)-REFRY(2,I)
              ZPARTY=RFDIFF(9,I)-REFRY(3,I)
              IF(XPARTY.EQ.0.0D0.AND.YPARTY.EQ.0.0D0.AND.I.NE.0)
     1        POLANGY(I)=-POLANGY(I-1)
              IF(XPARTY.EQ.0.0D0.AND.YPARTY.GT.0.0D0) POLANGY(I)=PII/2.0D0
              IF(XPARTY.EQ.0.0D0.AND.YPARTY.LT.0.0D0) POLANGY(I)=-PII/2.0D0
C
              IF(YPARTY.EQ.0.0D0.AND.XPARTY.GT.0.0D0) POLANGY(I)=0.0D0
              IF(YPARTY.EQ.0.0D0.AND.XPARTY.LT.0.0D0) POLANGY(I)=PII
              IF(XPARTY.NE.0.0D0.AND.YPARTY.NE.0.0D0)
     1        POLANGY(I)=DATAN2(XPARTY,YPARTY)
              IF(SHIFTX.LT.0.0D0) THEN
                  IF(POLANGX(I).EQ.0.0D0) THEN
                      POLANGX(I)=PII
                  ELSE
                      IF(POLANGX(I).EQ.PII) THEN
                          POLANGX(I)=0.0D0
                      ELSE
                          IF(POLANGX(I).NE.0.0D0.AND.POLANGX(I).NE.PII) THEN
                              POLANGX(I)=-POLANGX(I)
                          END IF
                      END IF
                  END IF
              END IF
              IF(SHIFTY.LT.0.0D0) THEN
                  IF(POLANGY(I).EQ.0.0D0) THEN
                      POLANGY(I)=PII
                  ELSE
                      IF(POLANGY(I).EQ.PII) THEN
                          POLANGY(I)=0.0D0
                      ELSE
                          IF(POLANGY(I).NE.0.0D0.AND.POLANGY(I).NE.PII) THEN
                              POLANGY(I)=-POLANGY(I)
                          END IF
                      END IF
                  END IF
              END IF
              MAG=DSQRT((XPARTX**2)+(YPARTX**2)+(ZPARTX**2))
              IF(MAG.NE.0.0D0) THEN
                  RXL(I)=XPARTX/MAG
                  RXM(I)=YPARTX/MAG
                  RXN(I)=ZPARTX/MAG
              ELSE
                  RXL(I)=1.0D0
                  RXM(I)=0.0D0
                  RXN(I)=0.0D0
              END IF
              MAG=DSQRT((XPARTY**2)+(YPARTY**2)+(ZPARTY**2))
              IF(MAG.NE.0.0D0) THEN
                  RYL(I)=XPARTY/MAG
                  RYM(I)=YPARTY/MAG
                  RYN(I)=ZPARTY/MAG
              ELSE
                  RYL(I)=0.0D0
                  RYM(I)=1.0D0
                  RYN(I)=0.0D0
              END IF
          END DO
          RETURN
      END
C       DIFFERENTIAL RAY TRACING
C SUB DIFRAY.FOR
      SUBROUTINE DIFRAY
C
C     FULL RAY AIMING PRE 2/10/2006, KEPT AS A BACKUP
C
          IMPLICIT NONE
C
C       DIFFERENTIAL RAY WITH RESPECT TO A CHANGE IN RAY HEIGHT AT
C       THE REFERENCE SURFACE.
C       THIS IS A SET OF
C       CLOSE RAYS TO THE RAY TRACED WITH THE RAY COMMAND WHICH
C       ARE TRACED WITH AIMING TO THE REFERENCE SURFACE WITH A SMALL
C       SHIFT IN AIM POINT IN X AND Y AND TRACED
C       FROM THE ORIGINAL OBJECT POINT
C
C       THE FOLLOWING DEFINE AND PASS THE DIFFERENTIAL RAY TRACE
C       WITH RESPECT TO AN AIMING CHANGE AT SURFACE NEWREF
C
          INTEGER I,KKK
C
          REAL*8 DDELX,DDELY,TARX,TARY,YANG,XANG,HIT,
     2    D11,D12,D21,D22,MF1,MF2,RSHIFTX,RSHIFTY,SYS12,SYS13
     3    ,MAG,LSTART,MSTART,NSTART,LOLD,MOLD,NOLD
     4    ,XC1,YC1,ZC1
C
          COMMON/SHIFTY/RSHIFTX,RSHIFTY
C
          REAL*8 SIG,X,
     1    Y,Z,L,M,N,X1ONE,Y1ONE,
     2    X1LAST,Y1LAST,RXONE,RYONE,RXLAST,RYLAST
C
          LOGICAL DELFAIL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          SRAYDIFEXT=.TRUE.
C
          DDELX=0.001D0
          DDELY=0.001D0
C
C     PROPER INITIALIZE PROCEEDURE 3/3/96
          R_X=0.0D0
          R_Y=0.0D0
          R_Z=0.0D0
          XOLD=0.0D0
          YOLD=0.0D0
          ZOLD=0.0D0
          LOLD=0.0D0
          MOLD=0.0D0
          NOLD=0.0D0
C
C     SET REF AP HT
C
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.
     1    DABS(ALENS(9,NEWREF)).LE.6.0D0.AND.
     1    ALENS(127,NEWREF).EQ.0.0D0) THEN
C
C       CLAP IS ON REFERENCE SURFACE
C
C       CIRCULAR CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0.AND.
     1        ALENS(127,NEWREF).EQ.0.0D0) THEN
                  IF(ALENS(10,NEWREF).LE.ALENS(11,NEWREF)) THEN
                      SYS12=ALENS(10,NEWREF)
                      SYS13=ALENS(10,NEWREF)
                  ELSE
                      SYS12=ALENS(11,NEWREF)
                      SYS13=ALENS(11,NEWREF)
                  END IF
C       NOT CIRCULAR CLAP
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0.AND.
     1        ALENS(127,NEWREF).EQ.0.0D0) THEN
                  SYS12=ALENS(10,NEWREF)
                  SYS13=ALENS(11,NEWREF)
C       NOT RECT CLAP
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0.AND.
     1        ALENS(127,NEWREF).EQ.0.0D0) THEN
                  SYS12=ALENS(10,NEWREF)
                  SYS13=ALENS(11,NEWREF)
C       NOT ELIP CLAP
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0.AND.
     1        ALENS(127,NEWREF).EQ.0.0D0) THEN
                  SYS12=ALENS(10,NEWREF)
                  SYS13=ALENS(11,NEWREF)
C       NOT RCTK CLAP
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0.AND.
     1        ALENS(127,NEWREF).EQ.0.0D0) THEN
                  SYS12=ALENS(10,NEWREF)
                  SYS13=ALENS(10,NEWREF)
C       NOT POLY CLAP
              END IF
C        IPOLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0.AND.
     1        ALENS(127,NEWREF).EQ.0.0D0) THEN
                  SYS12=ALENS(14,NEWREF)
                  SYS13=ALENS(14,NEWREF)
C       NOT IPOLY CLAP
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              SYS13=PXTRAX(1,NEWREF)
              SYS12=PXTRAY(1,NEWREF)
          END IF
C
C       SET INITIAL VALUES FOR X1ONE, Y1ONE, X1LAST AND Y1LAST
C
          X1ONE=-99999.9D0
          Y1ONE=-99999.9D0
          X1LAST=-99999.9D0
          Y1LAST=-99999.9D0
C
C       SET INITIAL VALUES FOR RXONE, RYONE, RXLAST AND RYLAST
          RXONE=-99999.9D0
          RYONE=-99999.9D0
          RXLAST=-99999.9D0
          RYLAST=-99999.9D0
C
          FOB0=0
C
C       FIRST XZ PLANE
          SIG=SYS13/DABS(SYS13)
          RSHIFTX=DIFTOL*DBLE(INT(SIG))
C
          XSTRT=RAYRAY(1,NEWOBJ)
          RSHIFTX=DABS(RSHIFTX)
          DELX=DABS(RSHIFTX)
          IF(W2.LT.0.0D0) DELX=-DELX
          YSTRT=RAYRAY(2,NEWOBJ)
          ZSTRT=RAYRAY(3,NEWOBJ)
C       THE INITIAL AIMING POINT IN NEWOBJ+1 IN NEWOBJ+1 COORDINATES IS:
          X1AIM=RAYRAY(1,(NEWOBJ+1))+RSHIFTX
          Y1AIM=RAYRAY(2,(NEWOBJ+1))
          Z1AIM=RAYRAY(3,(NEWOBJ+1))
          XC=X1AIM
          YC=Y1AIM
          ZC=Z1AIM
          IF(XC.LT.0.0D0) DDELX=DABS(DDELX)
          IF(YC.LT.0.0D0) DDELY=DABS(DDELY)
          IF(XC.GE.0.0D0) DDELX=-DABS(DDELX)
          IF(YC.GE.0.0D0) DDELY=-DABS(DDELY)
          XAIMOL=XC
          YAIMOL=YC
          ZAIMOL=ZC
C
C       CONVERT TO COORD SYS OF NEWOBJ
          R_TX=X1AIM
          R_TY=Y1AIM
          R_TZ=Z1AIM
          CALL BAKONE
          X1AIM=R_TX
          Y1AIM=R_TY
          Z1AIM=R_TZ
C       THESE ARE NOW IN COORD SYS OF SURFACE NEWOBJ
C
C       INITIAL DIRECTION COSINES ARE:
          MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)
     1    +((ZSTRT-Z1AIM)**2))
          LSTART=(X1AIM-XSTRT)/MAG
          MSTART=(Y1AIM-YSTRT)/MAG
          NSTART=(Z1AIM-ZSTRT)/MAG
          NINTY=.FALSE.
          IF(NSTART.LT.0.0D0) NINTY=.TRUE.
          IF(NINTY) RVSTART=.TRUE.
          IF(NSTART.EQ.0.0D0) THEN
              YANG=PII/2.0D0
              XANG=PII/2.0D0
          ELSE
              IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  YANG=0.0D0
              ELSE
                  YANG=DATAN2(MSTART,NSTART)
              END IF
              IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  XANG=0.0D0
              ELSE
                  XANG=DATAN2(LSTART,NSTART)
              END IF
          END IF
          X=XSTRT
          Y=YSTRT
          Z=ZSTRT
          L=LSTART
          M=MSTART
          N=NSTART

C
          KKK=0
C       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
C       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
C       CHIEF RAY AIMING.
C
 9        CONTINUE
          IF(ALENS(3,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
          IF(ALENS(3,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
          RV=.FALSE.
C
C       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
C       SURFACE INTERSECTION
          KKK=KKK+1
          REFEXT=.TRUE.
          FAIL=.FALSE.
          STOPP=0
C        PROCEED
C
          IF(KKK.NE.1) THEN
              MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)
     1        +((ZSTRT-Z1AIM)**2))
              LSTART=(X1AIM-XSTRT)/MAG
              MSTART=(Y1AIM-YSTRT)/MAG
              NSTART=(Z1AIM-ZSTRT)/MAG
              NINTY=.FALSE.
              IF(NSTART.LT.0.0D0) NINTY=.TRUE.
              IF(NINTY) RVSTART=.TRUE.
              IF(NSTART.EQ.0.0D0) THEN
                  YANG=PII/2.0D0
                  XANG=PII/2.0D0
              ELSE
                  IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      YANG=0.0D0
                  ELSE
                      YANG=DATAN2(MSTART,NSTART)
                  END IF
                  IF(DABS(LSTART).LE.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      XANG=0.0D0
                  ELSE
                      XANG=DATAN2(LSTART,NSTART)
                  END IF
              END IF
              X=XSTRT
              Y=YSTRT
              Z=ZSTRT
              L=LSTART
              M=MSTART
              N=NSTART
C       KKK=1
          END IF
          LOLD=LSTART
          MOLD=MSTART
          NOLD=NSTART
          DIFF(1,NEWOBJ)=X
          DIFF(2,NEWOBJ)=Y
          DIFF(3,NEWOBJ)=Z
          DIFF(4,NEWOBJ)=L
          DIFF(5,NEWOBJ)=M
          DIFF(6,NEWOBJ)=N
          DIFF(13,NEWOBJ)=LOLD
          DIFF(14,NEWOBJ)=MOLD
          DIFF(15,NEWOBJ)=NOLD
          DO 10 I=NEWOBJ+1,NEWIMG

              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              CALL TRNSF2
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              STOPP=0
              RAYEXT=.TRUE.
              FAIL=.FALSE.
              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              LOLD=L
              MOLD=M
              NOLD=N
              R_I=I
              R_XAIM=XAIMOL
              R_YAIM=YAIMOL
              R_ZAIM=ZAIMOL
              IF(RAYRAY(24,R_I-1).GT.0.0D0) POSRAY=.TRUE.
              IF(RAYRAY(24,R_I-1).LT.0.0D0) POSRAY=.FALSE.
              CALL HITSUR
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(STOPP.EQ.1) THEN
                  IF(MSG) THEN
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                  'RAY FAILURE OCCURRED AT SURFACE ',I
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'WHILE TRACING THE XZ-PLANE MARGINAL DIFFERENTIAL RAY'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  SRAYDIFEXT=.FALSE.
                  RETURN
              ELSE
                  STOPP=0
                  RAYEXT=.TRUE.
                  FAIL=.FALSE.
              END IF
C
C       CHECK THE RAY HEIGHT AT
C       NEWREF AND ADJUST THE YAIMOL AND XAIMOL AT SURFACE
C       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWREF IS WITHIN
C       .001 AIMTOL. MAXIMUN NUMBER OF ITERRATIONS IS 100
C
              IF(I.EQ.NEWREF) THEN
C       CALCULATE TARX AND TARY
                  IF(RAYRAY(1,NEWREF).LE.0.0D0) TARX=RAYRAY(1,NEWREF)+RSHIFTX
                  IF(RAYRAY(1,NEWREF).GT.0.0D0) TARX=RAYRAY(1,NEWREF)-RSHIFTX
                  TARY=RAYRAY(2,NEWREF)
C
                  IF((0.001D0*AIMTOL).LE.1.0D-10) HIT=1.0D-10
                  IF((0.001D0*AIMTOL).GT.1.0D-10) HIT=0.001D0*AIMTOL
                  IF(KKK.GT.2
     1            ) THEN
C       AIM IS GOOD ENOUGH, PROCEED
                      GO TO 100
C       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                  END IF
C
C       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
                  X1ONE=X1LAST
                  Y1ONE=Y1LAST
C       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
                  X1LAST=XAIMOL
                  Y1LAST=YAIMOL
C       SET RXONE AND RYONE TO RXLAST AND RYLAST
                  RXONE=RXLAST
                  RYONE=RYLAST
C       SET RXLAST AND RYLAST TO X AND Y
                  RXLAST=X
                  RYLAST=Y
C       X AND Y ARE THE CURRENT RAY COORDINATES AT THE
C       REFERENCE SURFACE.
C
C       GUESS CASE 1, REPRESENTS THE FIRST REFINEMENT
C       OF THE AIMING POINT.
C       THIS OCCURS IF KKK=1
                  IF(KKK.EQ.1) THEN
C       THIS IS CASE 1
C       IN THIS CASE WE SET THE SURFACE 1 AIMING POINTS
C       EQUAL TO THE OLD AIM POINTS PLUS A SMALL DELTA
C       IN ORDER TO CALCULATE DERIVATIVES.
C
                      X1AIM=XAIMOL+DDELX
                      Y1AIM=YAIMOL+DDELY
                      Z1AIM=ZAIMOL
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                      IF(ALENS(1,1).NE.0.0D0)
     1                CALL GETZEE1
                      X1AIM=XC
                      Y1AIM=YC
                      Z1AIM=ZC
C
C       CALCULATE NEW SET OF VALUES FOR DERIVATIVES
C
                      XAIMOL=XC1
                      YAIMOL=YC1
                      ZAIMOL=ZC1
                      R_TX=X1AIM
                      R_TY=Y1AIM
                      R_TZ=Z1AIM
                      CALL BAKONE
                      X1AIM=R_TX
                      Y1AIM=R_TY
                      Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
                      GO TO 9
C       THIS IS NOT THE FIRST REFINEMENT, KKK NOT = 1
                  END IF
C
                  CALL RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE
     1            ,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
C
C       MF1=TARX-RXLAST, MF2=TARY-RYLAST
C
C       TARX AND TARY ARE THE COORDINATES OF THE CENTER
C       OF THE DECENTERED CLEAR APERTURE ON THE REFERENCE
C       SURFACE IF THERE IS A CLEAR APERTURE DEFINED AND IT
C       IS DECENTERED. IF THESE CONDITIONS DO NOT EXIST,
C       TARY AND TARX ARE BOTH IDENTICALLY 0.0D0.
C
C
                  MF1=TARX-RXLAST
                  MF2=TARY-RYLAST
                  DELFAIL=.FALSE.
                  CALL NEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL)
                  IF(DELFAIL) THEN
                      RETURN
                  ELSE
                      GO TO 9
                  END IF
C       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
              END IF
C
 100          CONTINUE
C
C       THE COORDINATES OF THE DIFFERENTIAL RAY AT SURFACE I ARE:
C                       X,Y,Z WITH DIRECTION COSINES
C                       L,M,N
C       NOW RENAME THEM TO:
              DIFF(1,I)=X
              DIFF(2,I)=Y
              DIFF(3,I)=Z
              DIFF(4,I)=L
              DIFF(5,I)=M
              DIFF(6,I)=N
              DIFF(13,I)=LOLD
              DIFF(14,I)=MOLD
              DIFF(15,I)=NOLD
 10       CONTINUE
C
          DDELX=0.001D0
          DDELY=0.001D0
C       NEXT THE YZ PLANE
          X1ONE=-99999.9D0
          Y1ONE=-99999.9D0
          X1LAST=-99999.9D0
          Y1LAST=-99999.9D0
C
C       SET INITIAL VALUES FOR RXONE, RYONE, RXLAST AND RYLAST
          RXONE=-99999.9D0
          RYONE=-99999.9D0
          RXLAST=-99999.9D0
          RYLAST=-99999.9D0
          SIG=SYS12/DABS(SYS12)
          RSHIFTY=DIFTOL*DBLE(INT(SIG))
C
          YSTRT=RAYRAY(2,(NEWOBJ))
          DELY=DABS(RSHIFTY)
          IF(W1.LT.0.0D0) DELY=-DELY
          XSTRT=RAYRAY(1,NEWOBJ)
          ZSTRT=RAYRAY(3,NEWOBJ)
          X1AIM=RAYRAY(1,(NEWOBJ+1))
          Y1AIM=RAYRAY(2,(NEWOBJ+1))+RSHIFTY
          Z1AIM=RAYRAY(3,(NEWOBJ+1))
          XC=X1AIM
          YC=Y1AIM
          ZC=Z1AIM
          IF(XC.LT.0.0D0) DDELX=-DABS(DDELX)
          IF(YC.LT.0.0D0) DDELY=-DABS(DDELY)
          XAIMOL=XC
          YAIMOL=YC
          ZAIMOL=ZC
          R_TX=X1AIM
          R_TY=Y1AIM
          R_TZ=Z1AIM
          CALL BAKONE
          X1AIM=R_TX
          Y1AIM=R_TY
          Z1AIM=R_TZ
C
C    INITIAL DIRECTION COSINES ARE
          MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)
     1    +((ZSTRT-Z1AIM)**2))
          LSTART=(X1AIM-XSTRT)/MAG
          MSTART=(Y1AIM-YSTRT)/MAG
          NSTART=(Z1AIM-ZSTRT)/MAG
          NINTY=.FALSE.
          IF(NSTART.LT.0.0D0) NINTY=.TRUE.
          IF(NINTY) RVSTART=.TRUE.
          IF(NSTART.EQ.0.0D0) THEN
              YANG=PII/2.0D0
              XANG=PII/2.0D0
          ELSE
              IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  YANG=0.0D0
              ELSE
                  YANG=DATAN2(MSTART,NSTART)
              END IF
              IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  XANG=0.0D0
              ELSE
                  XANG=DATAN2(LSTART,NSTART)
              END IF
          END IF
          X=XSTRT
          Y=YSTRT
          Z=ZSTRT
          L=LSTART
          M=MSTART
          N=NSTART
          KKK=0
C       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
C       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
C       CHIEF RAY AIMING.
C
 19       CONTINUE
          IF(ALENS(3,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
          IF(ALENS(3,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
          RV=.FALSE.
C
C       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
C       SURFACE ZERO POINT INTERSECTION
          KKK=KKK+1
          STOPP=0
          RAYEXT=.TRUE.
          FAIL=.FALSE.
C        PROCEED
C
          IF(KKK.NE.1) THEN
C    INITIAL DIRECTION COSINES ARE
              MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)
     1        +((ZSTRT-Z1AIM)**2))
              LSTART=(X1AIM-XSTRT)/MAG
              MSTART=(Y1AIM-YSTRT)/MAG
              NSTART=(Z1AIM-ZSTRT)/MAG
              NINTY=.FALSE.
              IF(NSTART.LT.0.0D0) NINTY=.TRUE.
              IF(NINTY) RVSTART=.TRUE.
              IF(NSTART.EQ.0.0D0) THEN
                  YANG=PII/2.0D0
                  XANG=PII/2.0D0
              ELSE
                  IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      YANG=0.0D0
                  ELSE
                      YANG=DATAN2(MSTART,NSTART)
                  END IF
                  IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      XANG=0.0D0
                  ELSE
                      XANG=DATAN2(LSTART,NSTART)
                  END IF
              END IF
              X=XSTRT
              Y=YSTRT
              Z=ZSTRT
              L=LSTART
              M=MSTART
              N=NSTART
C       KKK=1
          END IF
          LOLD=LSTART
          MOLD=MSTART
          NOLD=NSTART
          DIFF(7,NEWOBJ)=X
          DIFF(8,NEWOBJ)=Y
          DIFF(9,NEWOBJ)=Z
          DIFF(10,NEWOBJ)=L
          DIFF(11,NEWOBJ)=M
          DIFF(12,NEWOBJ)=N
          DIFF(16,NEWOBJ)=LOLD
          DIFF(17,NEWOBJ)=MOLD
          DIFF(18,NEWOBJ)=NOLD
          DO 20 I=NEWOBJ+1,NEWIMG

              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              CALL TRNSF2
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              STOPP=0
              RAYEXT=.TRUE.
              FAIL=.FALSE.
C
              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              LOLD=L
              MOLD=M
              NOLD=N
              R_I=I
              R_XAIM=XAIMOL
              R_YAIM=YAIMOL
              R_ZAIM=ZAIMOL
              IF(RAYRAY(24,R_I-1).GT.0.0D0) POSRAY=.TRUE.
              IF(RAYRAY(24,R_I-1).LT.0.0D0) POSRAY=.FALSE.
              CALL HITSUR
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(STOPP.EQ.1) THEN
                  IF(MSG) THEN
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                  'RAY FAILURE OCCURRED AT SURFACE ',I
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'WHILE TRACING THE YZ-PLANE MARGINAL DIFFERENTIAL RAY'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  SRAYDIFEXT=.FALSE.
                  RETURN
              ELSE
                  STOPP=0
                  RAYEXT=.TRUE.
                  FAIL=.FALSE.
              END IF
C       CHECK THE RAY HEIGHT AT
C       NEWREF AND ADJUST THE YAIMOL AND XAIMOL AT SURFACE
C       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWREF IS WITHIN
C       .001 AIMTOL. MAXIMUN NUMBER OF ITERRATIONS IS 100
C
              IF(I.EQ.NEWREF) THEN
C       CALCULATE TARX AND TARY
                  IF(RAYRAY(2,NEWREF).LE.0.0D0) TARY=RAYRAY(2,NEWREF)+RSHIFTY
                  IF(RAYRAY(2,NEWREF).GT.0.0D0) TARY=RAYRAY(2,NEWREF)-RSHIFTY
                  TARX=RAYRAY(1,NEWREF)
C
                  IF((0.001D0*AIMTOL).LE.1.0D-10) HIT=1.0D-10
                  IF((0.001D0*AIMTOL).GT.1.0D-10) HIT=0.001D0*AIMTOL
                  IF(KKK.GT.2
     1            ) THEN
C       AIM IS GOOD ENOUGH, PROCEED
                      GO TO 1200
C       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                  END IF
C
C       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
                  X1ONE=X1LAST
                  Y1ONE=Y1LAST
C       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
                  X1LAST=XAIMOL
                  Y1LAST=YAIMOL
C       SET RXONE AND RYONE TO RXLAST AND RYLAST
                  RXONE=RXLAST
                  RYONE=RYLAST
C       SET RXLAST AND RYLAST TO X AND Y
                  RXLAST=X
                  RYLAST=Y
C       X AND Y ARE THE CURRENT RAY COORDINATES AT THE
C       REFERENCE SURFACE.
C
C       GUESS CASE 1, REPRESENTS THE FIRST REFINEMENT
C       OF THE AIMING POINT.
C       THIS OCCURS IF KKK=1
                  IF(KKK.EQ.1) THEN
C       THIS IS CASE 1
C       IN THIS CASE WE SET THE SURFACE 1 AIMING POINTS
C       EQUAL TO THE OLD AIM POINTS PLUS A SMALL DDELTA
C       IN ORDER TO CALCULATE DERIVATIVES.
C
                      X1AIM=XAIMOL+DDELX
                      Y1AIM=YAIMOL+DDELY
                      Z1AIM=ZAIMOL
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                      IF(ALENS(1,1).NE.0.0D0)
     1                CALL GETZEE1
                      X1AIM=XC
                      Y1AIM=YC
                      Z1AIM=ZC
C
C       CALCULATE NEW SET OF VALUES FOR DERIVATIVES
C
                      XAIMOL=XC1
                      YAIMOL=YC1
                      ZAIMOL=ZC1
                      R_TX=X1AIM
                      R_TY=Y1AIM
                      R_TZ=Z1AIM
                      CALL BAKONE
                      X1AIM=R_TX
                      Y1AIM=R_TY
                      Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
                      GO TO 19
C       THIS IS NOT THE FIRST REFINEMENT, KKK NOT = 1
                  END IF
C
                  CALL RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE
     1            ,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
C
C       MF1=TARX-RXLAST, MF2=TARY-RYLAST
C
C       TARX AND TARY ARE THE COORDINATES OF THE CENTER
C       OF THE DECENTERED CLEAR APERTURE ON THE REFERENCE
C       SURFACE IF THERE IS A CLEAR APERTURE DEFINED AND IT
C       IS DECENTERED. IF THESE CONDITIONS DO NOT EXIST,
C       TARY AND TARX ARE BOTH IDENTICALLY 0.0D0.
C
C
                  MF1=TARX-RXLAST
                  MF2=TARY-RYLAST
                  DELFAIL=.FALSE.
                  CALL NEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL)
                  IF(DELFAIL) THEN
                      RETURN
                  ELSE
                      GO TO 19
                  END IF
C       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
              END IF
C
 1200         CONTINUE
C
C       THE COORDINATES OF THE DIFFERENTIAL RAY AT SURFACE I ARE:
C                       X,Y,Z WITH DIRECTION COSINES
C                       L,M,N
              DIFF(7,I)=X
              DIFF(8,I)=Y
              DIFF(9,I)=Z
              DIFF(10,I)=L
              DIFF(11,I)=M
              DIFF(12,I)=N
              DIFF(16,I)=LOLD
              DIFF(17,I)=MOLD
              DIFF(18,I)=NOLD
C
C       STORE THESES DIFFERENTIALS
C
 20       CONTINUE
          RETURN
      END
C
C SUB FOBDIF.FOR
      SUBROUTINE FOBDIF
C
          IMPLICIT NONE
C
C       DIFFERENTIAL TRACE FOR CHIEF RAY. THIS IS A SET OF
C       CLOSE RAYS TO THE CHIEF RAY WHICH
C       ARE TRACED WITHOUT ADDITIONAL RAY AIMING FROM THE ORIGINAL OBJECT
C       POINT AT NEWOBJECT TOWARD THE ORIGINAL COORDINATE PLUS SOME
C       SMALL DELTA ON SURFACE NEWOBJECT+1.
C
C       THIS IS USED FOR THE FOB OPTION WHICH SHOWS THE X AND Y
C       FOCII ALONG A CLOSE RAY TRACE ABOUT THE CHIEF RAY
C
          INTEGER I,FFS,FFT
C
          REAL*8 D11,D12,D21,D22,HIT,
     2    DET,DDELX,DDELY,SHIFTY,SHIFTX,LSTART,
     3    MSTART,NSTART,YANG,XANG,TARY,TARX,MF1,MF2
     4    ,MAG,LOLD,MOLD,NOLD,XC1,YC1,ZC1
C
          INTEGER KKK
C
          REAL*8 FT,FS,X,X1ONE,Y1ONE,X1LAST,Y1LAST,
     1    Y,Z,L,M,N,RXONE,RYONE,
     2    RXLAST,RYLAST
C
          REAL*8 SYS12,SYS13,
     2    A11,A12,A21,A22,JK_AA,JK_BB,FS1,FS2,FT1,FT2,S,T
C
          COMMON/FBDIFF/FS,FT,FFS,FFT
C
          LOGICAL DELFAIL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          SREFDIFEXT=.TRUE.
C
          DDELX=0.001D0
          DDELY=0.001D0
C
C     PROPER INITIALIZE PROCEEDURE 3/3/96
          R_X=0.0D0
          R_Y=0.0D0
          R_Z=0.0D0
          XOLD=0.0D0
          YOLD=0.0D0
          ZOLD=0.0D0
          LOLD=0.0D0
          MOLD=0.0D0
          NOLD=0.0D0
C
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.
     1    DABS(ALENS(9,NEWREF)).LE.6.0D0.AND.
     1    ALENS(127,NEWREF).EQ.0.0D0) THEN
C
C       CLAP IS ON REFERENCE SURFACE
C
C       CIRCULAR CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0
     1        .AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
                  IF(ALENS(10,NEWREF).LE.ALENS(11,NEWREF)) THEN
                      SYS12=ALENS(10,NEWREF)
                      SYS13=ALENS(10,NEWREF)
                  ELSE
                      SYS12=ALENS(11,NEWREF)
                      SYS13=ALENS(11,NEWREF)
                  END IF
C       NOT CIRCULAR CLAP
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0
     1        .AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
                  SYS12=ALENS(10,NEWREF)
                  SYS13=ALENS(11,NEWREF)
C       NOT RECT CLAP
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0
     1        .AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
                  SYS12=ALENS(10,NEWREF)
                  SYS13=ALENS(11,NEWREF)
C       NOT ELIP CLAP
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0
     1        .AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
                  SYS12=ALENS(10,NEWREF)
                  SYS13=ALENS(11,NEWREF)
C       NOT RCTK CLAP
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0
     1        .AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
                  SYS12=ALENS(10,NEWREF)
                  SYS13=ALENS(10,NEWREF)
C       NOT POLY CLAP
              END IF
C        IPOLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0
     1        .AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
                  SYS12=ALENS(14,NEWREF)
                  SYS13=ALENS(14,NEWREF)
C       NOT IPOLY CLAP
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              SYS13=PXTRAX(1,NEWREF)
              SYS12=PXTRAY(1,NEWREF)
          END IF
C
          FOB0=0
          FT=0.0D0
          FS=0.0D0
          FFT=0
          FFS=0
          IF(.NOT.LDIF) THEN
              OUTLYNE=
     1        'DIFFERENTIAL RAYTRACE IS "OFF"'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'ALL DIFFERENTIAL RAY DATA IS ZERO'
              CALL SHOWIT(1)
              STOPP=1
              SREFDIFEXT=.FALSE.
              RETURN
C       LDIF TRUE, PROCEED
          END IF
C
C       SET INITIAL VALUES FOR X1ONE, Y1ONE, X1LAST AND Y1LAST
C
          X1ONE=-99999.9D0
          Y1ONE=-99999.9D0
          X1LAST=-99999.9D0
          Y1LAST=-99999.9D0
C
C       SET INITIAL VALUES FOR RXONE, RYONE, RXLAST AND RYLAST
          RXONE=-99999.9D0
          RYONE=-99999.9D0
          RXLAST=-99999.9D0
          RYLAST=-99999.9D0
C
C       FIRST XZ PLANE
          SHIFTX=DABS(DIFTOL)
C        SHIFTX=DABS(DIFTOL*DABS(SYS13))
C
          XSTRT=RAYRAY(1,NEWOBJ)
          YSTRT=RAYRAY(2,NEWOBJ)
          ZSTRT=RAYRAY(3,NEWOBJ)
C       THE INITIAL AIMING POINT IN NEWOBJ+1 IN NEWOBJ+1 COORDINATES IS:
          X1AIM=RAYRAY(1,(NEWOBJ+1))
          Y1AIM=RAYRAY(2,(NEWOBJ+1))
          Z1AIM=RAYRAY(3,(NEWOBJ+1))
          XC=X1AIM
          YC=Y1AIM
          ZC=Z1AIM
          IF(XC.LT.0.0D0) DDELX=DABS(DDELX)
          IF(YC.LT.0.0D0) DDELY=DABS(DDELY)
          IF(XC.GE.0.0D0) DDELX=-DABS(DDELX)
          IF(YC.GE.0.0D0) DDELY=-DABS(DDELY)
          XAIMOL=XC
          YAIMOL=YC
          ZAIMOL=ZC
C
C       CONVERT TO COORD SYS OF NEWOBJ
          R_TX=X1AIM
          R_TY=Y1AIM
          R_TZ=Z1AIM
          CALL BAKONE
          X1AIM=R_TX
          Y1AIM=R_TY
          Z1AIM=R_TZ
C       THESE ARE NOW IN COORD SYS OF SURFACE NEWOBJ
C
C       INITIAL DIRECTION COSINES ARE:
          LSTART=RAYRAY(4,NEWOBJ)
          MSTART=RAYRAY(5,NEWOBJ)
          NSTART=RAYRAY(6,NEWOBJ)
          NINTY=.FALSE.
          IF(NSTART.LT.0.0D0) NINTY=.TRUE.
          IF(NINTY) RVSTART=.TRUE.
          IF(NSTART.EQ.0.0D0) THEN
              YANG=PII/2.0D0
              XANG=PII/2.0D0
          ELSE
              IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  YANG=0.0D0
              ELSE
                  YANG=DATAN2(MSTART,NSTART)
              END IF
              IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  XANG=0.0D0
              ELSE
                  XANG=DATAN2(LSTART,NSTART)
              END IF
          END IF
          X=XSTRT
          Y=YSTRT
          Z=ZSTRT
          L=LSTART
          M=MSTART
          N=NSTART
C
          KKK=0
C       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
C       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
C       CHIEF RAY AIMING.
C
 9        CONTINUE
          IF(ALENS(3,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
          IF(ALENS(3,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
          RV=.FALSE.
C
C       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
C       SURFACE INTERSECTION
          KKK=KKK+1
C       IF KKK EXCEEDS 100 TRIES, PRINT RAY ITERRATION ERROR
C       AND STOP SEARCHING.
          IF(KKK.GT.NRAITR) THEN
              OUTLYNE='WARNING:'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'MARGINAL DIFFERENTIAL RAY (XZ-PLANE) FAILURE:'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RAY FAILED TO CONVERGE TO RAY-AIM POINT'
              CALL SHOWIT(1)
              STOPP=1
              SREFDIFEXT=.FALSE.
              RETURN
          ELSE
              RAYEXT=.TRUE.
              FAIL=.FALSE.
              STOPP=0
C        PROCEED
          END IF
C
          IF(KKK.NE.1) THEN
              MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)
     1        +((ZSTRT-Z1AIM)**2))
              LSTART=(X1AIM-XSTRT)/MAG
              MSTART=(Y1AIM-YSTRT)/MAG
              NSTART=(Z1AIM-ZSTRT)/MAG
              NINTY=.FALSE.
              IF(NSTART.LT.0.0D0) NINTY=.TRUE.
              IF(NINTY) RVSTART=.TRUE.
              IF(NSTART.EQ.0.0D0) THEN
                  YANG=PII/2.0D0
                  XANG=PII/2.0D0
              ELSE
                  IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      YANG=0.0D0
                  ELSE
                      YANG=DATAN2(MSTART,NSTART)
                  END IF
                  IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      XANG=0.0D0
                  ELSE
                      XANG=DATAN2(LSTART,NSTART)
                  END IF
              END IF
              X=XSTRT
              Y=YSTRT
              Z=ZSTRT
              L=LSTART
              M=MSTART
              N=NSTART
C       KKK=1
          END IF
          DO 10 I=NEWOBJ+1,NEWIMG

              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              CALL TRNSF2
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              STOPP=0
              RAYEXT=.TRUE.
              FAIL=.FALSE.
              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              LOLD=L
              MOLD=M
              NOLD=N
              R_XAIM=XAIMOL
              R_YAIM=YAIMOL
              R_ZAIM=ZAIMOL
              IF(REFRY(24,R_I-1).GT.0.0D0) POSRAY=.TRUE.
              IF(REFRY(24,R_I-1).LT.0.0D0) POSRAY=.FALSE.
              CALL HITSUR
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(STOPP.EQ.1) THEN
                  IF(MSG) THEN
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                  'RAY FAILURE OCCURRED AT SURFACE ',I
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'WHILE TRACING THE XZ-PLANE MARGINAL DIFFERENTIAL RAY'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  SREFDIFEXT=.FALSE.
                  RETURN
              ELSE
                  STOPP=0
                  RAYEXT=.TRUE.
                  FAIL=.FALSE.
              END IF
C       CHECK THE RAY HEIGHT AT
C       NEWREF AND ADJUST THE YAIMOL AND XAIMOL AT SURFACE
C       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWREF IS WITHIN
C       .001 AIMTOL. MAXIMUN NUMBER OF ITERRATIONS IS 100
C
              IF(I.EQ.NEWREF) THEN
C       CALCULATE TARX AND TARY
                  IF(RAYRAY(1,NEWREF).LT.0.0D0) TARX=RAYRAY(1,NEWREF)+SHIFTX
                  IF(RAYRAY(1,NEWREF).GE.0.0D0) TARX=RAYRAY(1,NEWREF)-SHIFTX
                  TARY=RAYRAY(2,NEWREF)
C
                  IF((0.001D0*AIMTOL).LE.1.0D-10) HIT=1.0D-10
                  IF((0.001D0*AIMTOL).GT.1.0D-10) HIT=0.001D0*AIMTOL
                  IF(DSQRT(((TARX-X)**2)+((TARY-Y)**2)).LE.(HIT).AND.KKK.GT.1
     1            ) THEN
C       AIM IS GOOD ENOUGH, PROCEED
                      GO TO 100
C       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                  END IF
C
C       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
                  X1ONE=X1LAST
                  Y1ONE=Y1LAST
C       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
                  X1LAST=XAIMOL
                  Y1LAST=YAIMOL
C       SET RXONE AND RYONE TO RXLAST AND RYLAST
                  RXONE=RXLAST
                  RYONE=RYLAST
C       SET RXLAST AND RYLAST TO X AND Y
                  RXLAST=X
                  RYLAST=Y
C       X AND Y ARE THE CURRENT RAY COORDINATES AT THE
C       REFERENCE SURFACE.
C
C       GUESS CASE 1, REPRESENTS THE FIRST REFINEMENT
C       OF THE AIMING POINT.
C       THIS OCCURS IF KKK=1
                  IF(KKK.EQ.1) THEN
C       THIS IS CASE 1
C       IN THIS CASE WE SET THE SURFACE 1 AIMING POINTS
C       EQUAL TO THE OLD AIM POINTS PLUS A SMALL DELTA
C       IN ORDER TO CALCULATE DERIVATIVES.
C
                      X1AIM=XAIMOL+DDELX
                      Y1AIM=YAIMOL+DDELY
                      Z1AIM=ZAIMOL
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                      IF(ALENS(1,1).NE.0.0D0)
     1                CALL GETZEE1
                      X1AIM=XC
                      Y1AIM=YC
                      Z1AIM=ZC
C
C       CALCULATE NEW SET OF VALUES FOR DERIVATIVES
C
                      XAIMOL=XC1
                      YAIMOL=YC1
                      ZAIMOL=ZC1
                      R_TX=X1AIM
                      R_TY=Y1AIM
                      R_TZ=Z1AIM
                      CALL BAKONE
                      X1AIM=R_TX
                      Y1AIM=R_TY
                      Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
                      GO TO 9
C       THIS IS NOT THE FIRST REFINEMENT, KKK NOT = 1
                  END IF
C
                  CALL RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE
     1            ,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
C       MF1=TARX-RXLAST, MF2=TARY-RYLAST
C
C       TARX AND TARY ARE THE COORDINATES OF THE CENTER
C       OF THE DECENTERED CLEAR APERTURE ON THE REFERENCE
C       SURFACE IF THERE IS A CLEAR APERTURE DEFINED AND IT
C       IS DECENTERED. IF THESE CONDITIONS DO NOT EXIST,
C       TARY AND TARX ARE BOTH IDENTICALLY 0.0D0.
C
C
                  MF1=TARX-RXLAST
                  MF2=TARY-RYLAST
                  DELFAIL=.FALSE.
                  CALL NEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL)
                  IF(DELFAIL) THEN
                      RETURN
                  ELSE
                      GO TO 9
                  END IF
C       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
              END IF
C
 100          CONTINUE
  10      CONTINUE
C       THE COORDINATES OF THE DIFFERENTIAL RAY AT NEWIMG ARE:
C                       X,Y,Z WITH DIRECTION COSINES
C                       L,M,N
C       THE COORDINATES OF THE REFERENCE RAY ARE:
C       REFRY(1,NEWIMG),REFRAY(2,NEWIMG),REFRY(3,NEWIMG)
C       WITH DIRECTION COSINES
C       REFRY(4,NEWING),REFRY(5,NEWIMG),REFRY(6,NEWIMG)
C       THESE COORDINATES AND DIRECTION COSINES ARE IN THE
C       LOCAL COORDINATE SYSTEM OF (NEWIMG).
C       THE Z COORDINATE OF THE INTERSECTION OF THESE TWO
C       RAYS WILL BE THE VALUE FS.
          A11=L-M
          A12=-(REFRY(4,NEWIMG)-REFRY(5,NEWIMG))
          A21=M-N
          A22=-(REFRY(5,NEWIMG)-REFRY(6,NEWIMG))
          JK_AA=((REFRY(1,NEWIMG)-X)-(REFRY(2,NEWIMG)-Y))
          JK_BB=((REFRY(2,NEWIMG)-Y)-(REFRY(3,NEWIMG)-Z))
          DET=(A11*A22)-(A12*A21)
          IF(DABS(DET).LE.1.0D-6) DET=0.0D0
          IF(DET.EQ.0.0D0) THEN
              FS=0.0D0
              FT=0.0D0
              FFS=0
              FFT=0
C       NO SOLUTION EXISTS
              SREFDIFEXT=.FALSE.
              RETURN
          ELSE
C       A SOLUTION EXISTS
              FFS=1
              S=((A11*JK_BB)-(A21*JK_AA))/DET
              T=(JK_AA-(A12*S))/A11
              FS1=Z+(N*T)
              FS2=REFRY(3,NEWIMG)+(REFRY(6,NEWIMG)*S)
C       CALCULATE FS
              FS=(FS1+FS2)/2.0D0
          END IF
C
          DDELX=0.001D0
          DDELY=0.001D0
C       NEXT THE YZ PLANE
          SHIFTY=DABS(DIFTOL)
C        SHIFTY=DABS(DIFTOL*DABS(SYS12))
C
          YSTRT=RAYRAY(2,(NEWOBJ))
          XSTRT=RAYRAY(1,NEWOBJ)
          ZSTRT=RAYRAY(3,NEWOBJ)
          X1AIM=RAYRAY(1,(NEWOBJ+1))
          Y1AIM=RAYRAY(2,(NEWOBJ+1))
          Z1AIM=RAYRAY(3,(NEWOBJ+1))
          IF(XC.LT.0.0D0) DDELX=-DABS(DDELX)
          IF(YC.LT.0.0D0) DDELY=-DABS(DDELY)
          XAIMOL=XC
          YAIMOL=YC
          ZAIMOL=ZC
          R_TX=X1AIM
          R_TY=Y1AIM
          R_TZ=Z1AIM
          CALL BAKONE
          X1AIM=R_TX
          Y1AIM=R_TY
          Z1AIM=R_TZ
C
C    INITIAL DIRECTION COSINES ARE
          LSTART=RAYRAY(4,NEWOBJ)
          MSTART=RAYRAY(5,NEWOBJ)
          NSTART=RAYRAY(6,NEWOBJ)
          NINTY=.FALSE.
          IF(NSTART.LT.0.0D0) NINTY=.TRUE.
          IF(NINTY) RVSTART=.TRUE.
          IF(NSTART.EQ.0.0D0) THEN
              YANG=PII/2.0D0
              XANG=PII/2.0D0
          ELSE
              IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  YANG=0.0D0
              ELSE
                  YANG=DATAN2(MSTART,NSTART)
              END IF
              IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                  XANG=0.0D0
              ELSE
                  XANG=DATAN2(LSTART,NSTART)
              END IF
          END IF
          X=XSTRT
          Y=YSTRT
          Z=ZSTRT
          L=LSTART
          M=MSTART
          N=NSTART
          KKK=0
C       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
C       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
C       CHIEF RAY AIMING.
C
 19       CONTINUE
          IF(ALENS(3,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
          IF(ALENS(3,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
          RV=.FALSE.
C
C       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
C       SURFACE ZERO POINT INTERSECTION
          KKK=KKK+1
C       IF KKK EXCEEDS 100 TRIES, PRINT RAY ITERRATION ERROR
C       AND STOP SEARCHING.
          IF(KKK.GT.NRAITR) THEN
              OUTLYNE='WARNING:'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'MARGINAL DIFFERENTIAL RAY (YZ-PLANE) FAILURE:'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RAY FAILED TO CONVERGE TO RAY-AIM POINT'
              CALL SHOWIT(1)
              STOPP=1
              SREFDIFEXT=.FALSE.
              RETURN
          ELSE
              STOPP=0
              RAYEXT=.TRUE.
              FAIL=.FALSE.
C        PROCEED
          END IF
C
          IF(KKK.NE.1) THEN
C    INITIAL DIRECTION COSINES ARE
              MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)
     1        +((ZSTRT-Z1AIM)**2))
              LSTART=(X1AIM-XSTRT)/MAG
              MSTART=(Y1AIM-YSTRT)/MAG
              NSTART=(Z1AIM-ZSTRT)/MAG
              NINTY=.FALSE.
              IF(NSTART.LT.0.0D0) NINTY=.TRUE.
              IF(NINTY) RVSTART=.TRUE.
              IF(NSTART.EQ.0.0D0) THEN
                  YANG=PII/2.0D0
                  XANG=PII/2.0D0
              ELSE
                  IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      YANG=0.0D0
                  ELSE
                      YANG=DATAN2(MSTART,NSTART)
                  END IF
                  IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
                      XANG=0.0D0
                  ELSE
                      XANG=DATAN2(LSTART,NSTART)
                  END IF
              END IF
              X=XSTRT
              Y=YSTRT
              Z=ZSTRT
              L=LSTART
              M=MSTART
              N=NSTART
C       KKK=1
          END IF
          DO 20 I=NEWOBJ+1,NEWIMG

              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              CALL TRNSF2
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              STOPP=0
              RAYEXT=.TRUE.
              FAIL=.FALSE.
C
              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              LOLD=L
              MOLD=M
              NOLD=N
              R_XAIM=XAIMOL
              R_YAIM=YAIMOL
              R_ZAIM=ZAIMOL
              IF(REFRY(24,R_I-1).GT.0.0D0) POSRAY=.TRUE.
              IF(REFRY(24,R_I-1).LT.0.0D0) POSRAY=.FALSE.
              CALL HITSUR
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(STOPP.EQ.1) THEN
                  IF(MSG) THEN
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                  'RAY FAILURE OCCURRED AT SURFACE ',I
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'WHILE TRACING THE YZ-PLANE MARGINAL DIFFERENTIAL RAY'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  SREFDIFEXT=.FALSE.
                  RETURN
              ELSE
                  STOPP=0
                  RAYEXT=.TRUE.
                  FAIL=.FALSE.
              END IF
C       CHECK THE RAY HEIGHT AT
C       NEWREF AND ADJUST THE YAIMOL AND XAIMOL AT SURFACE
C       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWREF IS WITHIN
C       .001 AIMTOL. MAXIMUN NUMBER OF ITERRATIONS IS  100
C
              IF(I.EQ.NEWREF) THEN
C       CALCULATE TARX AND TARY
                  IF(RAYRAY(2,NEWREF).LT.0.0D0)TARY=RAYRAY(2,NEWREF)+SHIFTY
                  IF(RAYRAY(2,NEWREF).GE.0.0D0)TARY=RAYRAY(2,NEWREF)-SHIFTY
                  TARX=RAYRAY(1,NEWREF)
C
                  IF((0.001D0*AIMTOL).LE.1.0D-10) HIT=1.0D-10
                  IF((0.001D0*AIMTOL).GT.1.0D-10) HIT=0.001D0*AIMTOL
                  IF(DSQRT(((TARX-X)**2)+((TARY-Y)**2)).LE.(HIT).AND.KKK.GT.1
     1            ) THEN
C       AIM IS GOOD ENOUGH, PROCEED
                      GO TO 1200
C       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                  END IF
C
C       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
                  X1ONE=X1LAST
                  Y1ONE=Y1LAST
C       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
                  X1LAST=XAIMOL
                  Y1LAST=YAIMOL
C       SET RXONE AND RYONE TO RXLAST AND RYLAST
                  RXONE=RXLAST
                  RYONE=RYLAST
C       SET RXLAST AND RYLAST TO X AND Y
                  RXLAST=X
                  RYLAST=Y
C       X AND Y ARE THE CURRENT RAY COORDINATES AT THE
C       REFERENCE SURFACE.
C
C       GUESS CASE 1, REPRESENTS THE FIRST REFINEMENT
C       OF THE AIMING POINT.
C       THIS OCCURS IF KKK=1
                  IF(KKK.EQ.1) THEN
C       THIS IS CASE 1
C       IN THIS CASE WE SET THE SURFACE 1 AIMING POINTS
C       EQUAL TO THE OLD AIM POINTS PLUS A SMALL DDELTA
C       IN ORDER TO CALCULATE DERIVATIVES.
C
                      X1AIM=XAIMOL+DDELX
                      Y1AIM=YAIMOL+DDELY
                      Z1AIM=ZAIMOL
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                      IF(ALENS(1,1).NE.0.0D0)
     1                CALL GETZEE1
                      X1AIM=XC
                      Y1AIM=YC
                      Z1AIM=ZC
C
C       CALCULATE NEW SET OF VALUES FOR DERIVATIVES
                      XAIMOL=XC1
                      YAIMOL=YC1
                      ZAIMOL=ZC1
                      R_TX=X1AIM
                      R_TY=Y1AIM
                      R_TZ=Z1AIM
                      CALL BAKONE
                      X1AIM=R_TX
                      Y1AIM=R_TY
                      Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
C
                      GO TO 19
C       THIS IS NOT THE FIRST REFINEMENT, KKK NOT = 1
                  END IF
C
                  CALL RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE
     1            ,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
C
C       MF1=TARX-RXLAST, MF2=TARY-RYLAST
C
C       TARX AND TARY ARE THE COORDINATES OF THE CENTER
C       OF THE DECENTERED CLEAR APERTURE ON THE REFERENCE
C       SURFACE IF THERE IS A CLEAR APERTURE DEFINED AND IT
C       IS DECENTERED. IF THESE CONDITIONS DO NOT EXIST,
C       TARY AND TARX ARE BOTH IDENTICALLY 0.0D0.
C
C
                  MF1=TARX-RXLAST
                  MF2=TARY-RYLAST
                  DELFAIL=.FALSE.
                  CALL NEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL)
                  IF(DELFAIL) THEN
                      RETURN
                  ELSE
                      GO TO 19
                  END IF
C       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
              END IF
C
 1200         CONTINUE
   20     CONTINUE
C
C       THE COORDINATES OF THE DIFFERENTIAL RAY AT NEWIMG ARE:
C                       X,Y,Z WITH DIRECTION COSINES
C                       L,M,N
C       THE COORDINATES OF THE REFERENCE RAY ARE:
C       REFRY(1,NEWIMG),REFRY(2,NEWIMG),REFRY(3,NEWIMG)
C       WITH DIRECTION COSINES
C       REFRY(4,NEWING),REFRY(5,NEWIMG),REFRY(6,NEWIMG)
C       THESE COORDINATES AND DIRECTION COSINES ARE IN THE
C       LOCAL COORDINATE SYSTEM OF (NEWIMG).
C       THE Z COORDINATE OF THE INTERSECTION OF THESE TWO
C       RAYS WILL BE THE VALUE FT.
          A11=L-M
          A12=-(REFRY(4,NEWIMG)-REFRY(5,NEWIMG))
          A21=M-N
          A22=-(REFRY(5,NEWIMG)-REFRY(6,NEWIMG))
          JK_AA=((REFRY(1,NEWIMG)-X)-(REFRY(2,NEWIMG)-Y))
          JK_BB=((REFRY(2,NEWIMG)-Y)-(REFRY(3,NEWIMG)-Z))
          DET=(A11*A22)-(A12*A21)
          IF(DABS(DET).LE.1.0D-6) DET=0.0D0
          IF(DET.EQ.0.0D0) THEN
              FS=0.0D0
              FT=0.0D0
              FFS=0
              FFT=0
C       NO SOLUTION EXISTS
              SREFDIFEXT=.FALSE.
              RETURN
          ELSE
C       A SOLUTION EXISTS
              FFT=1
              S=((A11*JK_BB)-(A21*JK_AA))/DET
              T=(JK_AA-(A12*S))/A11
              FT1=Z+(N*T)
              FT2=REFRY(3,NEWIMG)+(REFRY(6,NEWIMG)*S)
C       CALCULATE FT
              FT=(FT1+FT2)/2.0D0
          END IF
C
          RETURN
      END
