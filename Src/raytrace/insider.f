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

C SUB MISSREF.FOR

      SUBROUTINE MISSREF(X,Y)
C
          IMPLICIT NONE
C
          EXTERNAL INSID1,INSID2
C
C       THIS CHECKS IF X AND Y IS INSIDE THE CLAP ON THE REFERENCE SURFACE
C
          INTEGER I,CAFLG,N,III
C
          REAL*8 X,Y,ANGLE,JK1,JK2,JK3,
     1    XR,YR,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,
     2    X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,
     3    YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4,A15
C
          LOGICAL INS,INSID1,INSID2
C
          LOGICAL NOCOBSPSF
          COMMON/PSFCOBS/NOCOBSPSF
C
          INTEGER CAERAS,COERAS
C
          COMMON/CACO/CAERAS,COERAS,LS
          COMMON/JK/JK1,JK2,JK3            !Add by ENDO
C
C     VARIABLES FOR SPOT TRACING
C
          LOGICAL SPDTRA
C
          COMMON/SPRA1/SPDTRA
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          I=NEWREF
          LS=0
C
C       I IS THE REF SURFACE NUMBER
C
C
          IF(ALENS(9,I).EQ.0.0D0) THEN
C
C       NO CLAPS OR COBS, JUST RETURN
              RETURN
C       THERE ARE CLAPS AND COBS, PROCEED CHECKING
          END IF
C
          IF(ALENS(9,I).NE.0.0D0) THEN
              CAFLG=INT(ALENS(9,I))
C       CLAP EXISTS
C******************************************************************
C
C       CAFLG=1 CIRCULAR CLAP, DOES IT STOP THE RAY
              IF(CAFLG.EQ.1) THEN
C
                  LS=0.0D0
C       CIRCULAR CLAP EQUATION IS:
C
C       (X)**2 + (Y)**2 = R**2
C
C       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
C       GREATER THAN THE RIGHT SIDE, THE RAY IS BLOCKED
C       ELSE IT IS NOT BLOCKED.
C
C       IF NON-ZERO CLAP DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE. REMEMBER.

                  XR=X-ALENS(13,I)-JK1
                  YR=Y-ALENS(12,I)-JK2
C
                  LS=DSQRT((XR**2)+(YR**2))
C
                  IF(DABS(ALENS(10,I)).LE.DABS(ALENS(11,I))) THEN
                      RS=DSQRT(ALENS(10,I)**2)+AIMTOL
                  ELSE
                      RS=DSQRT(ALENS(11,I)**2)+AIMTOL
                  END IF
                  IF(REAL(LS).GT.REAL(RS)) THEN
                      LS=10.0D0
                  ELSE
                      LS=0.0D0
                  END IF
C
                  IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY CLAP ON SURFACE I
                      REFMISS=.TRUE.
                      RETURN
                  END IF
              END IF
C       CAFLG NOT 1
          END IF
C
C
C       CAFLG=2 RECTANGULAR CLAP, DOES IT STOP THE RAY
          IF(CAFLG.EQ.2) THEN
C
              LS=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
C       COORDINATES ARE:
              X1=-ALENS(11,I)-AIMTOL
              Y1=ALENS(10,I)+AIMTOL
              X2=-ALENS(11,I)-AIMTOL
              Y2=-ALENS(10,I)-AIMTOL
              X3=ALENS(11,I)+AIMTOL
              Y3=-ALENS(10,I)-AIMTOL
              X4=ALENS(11,I)+AIMTOL
              Y4=ALENS(10,I)+AIMTOL
C
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE. REMEMBER.

              XRD=XRD-ALENS(13,I)-JK1
              YRD=YRD-ALENS(12,I)-JK2
C
C       IF A NON-ZERO CLAP TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION
C     (THE ROTATION IS ALWAYS ABOUT THE LOCAL Z-AXIS OF THE SURFACE VERTEX)

              A15=(ALENS(15,I)+JK3)*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
C       IF YES, SET LS=10.0D0
C       IF NO SET LS=0.0D0
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
              INS=INSID1()
              IF(INS) THEN
C       RAY NOT BLOCKED
                  LS=0.0D0
              ELSE
C       RAY BLOCKED
                  LS=10.0D0
              END IF
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY CLAP ON SURFACE I
                  REFMISS=.TRUE.
                  RETURN
              END IF
C       CAFLG NOT 2
          END IF
C
C       CAFLG=3 ELLIPTICAL CLAP, DOES IT STOP THE RAY
          IF(CAFLG.EQ.3) THEN
C
              LS=0.0D0
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
C
C       IF NON-ZERO CLAP DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE. REMEMBER.

              XRD=XRD-ALENS(13,I)-JK1
              YRD=YRD-ALENS(12,I)-JK2
C
C       IF A NON-ZERO CLAP TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

              A15=ALENS(15,I)*PII/180.0D0
              A15=(ALENS(15,I)+JK3)*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
              LS=((XR**2)/(ALENS(11,I)**2))+
     1           ((YR**2)/(ALENS(10,I)**2))
C
              IF(REAL(LS).GT.(1.0+(AIMTOL**2))) THEN
C       RAY BLOCKED
                  LS=10.0D0
              ELSE
C       NOT BLOCKED
                  LS=0.0D0
              END IF
C
              IF(LS.EQ.10.0D0) THEN
                  REFMISS=.TRUE.
                  RETURN
              END IF
C       CAFLG NOT 3
          END IF
C
C
C       CAFLG=4 RACETRACK CLAP, DOES IT STOP THE RAY
          IF(CAFLG.EQ.4) THEN
C
              LS=0.0D0
C
C       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
C       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
C       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
C       AND THE FOUR CIRCLES NEED TO BE CHECKED.
C
              IF(ALENS(10,I).LE.ALENS(11,I)) THEN
C       ALENS(11,I) = MAXSID
                  MAXSID=ALENS(11,I)
              ELSE
                  MAXSID=ALENS(10,I)
              END IF
              IF(ALENS(14,I).LT.MAXSID) THEN
C       SETUP THE 8 SIDED BOX
                  N=8
                  X1=-ALENS(11,I)+ALENS(14,I)-AIMTOL
                  Y1=ALENS(10,I)+AIMTOL
                  X2=-ALENS(11,I)-AIMTOL
                  Y2=ALENS(10,I)-ALENS(14,I)+AIMTOL
                  X3=-ALENS(11,I)-AIMTOL
                  Y3=-ALENS(10,I)+ALENS(14,I)-AIMTOL
                  X4=-ALENS(11,I)+ALENS(14,I)-AIMTOL
                  Y4=-ALENS(10,I)-AIMTOL
                  X5=ALENS(11,I)-ALENS(14,I)+AIMTOL
                  Y5=-ALENS(10,I)-AIMTOL
                  X6=ALENS(11,I)+AIMTOL
                  Y6=-ALENS(10,I)+ALENS(14,I)-AIMTOL
                  X7=ALENS(11,I)+AIMTOL
                  Y7=ALENS(10,I)-ALENS(14,I)+AIMTOL
                  X8=ALENS(11,I)-ALENS(14,I)+AIMTOL
                  Y8=ALENS(10,I)+AIMTOL
C
C
              ELSE
C       SET UP THE FOUR SIDED BOX
                  N=4
                  X1=-ALENS(11,I)-AIMTOL
                  Y1=ALENS(10,I)+AIMTOL
                  X2=-ALENS(11,I)-AIMTOL
                  Y2=-ALENS(10,I)-AIMTOL
                  X3=ALENS(11,I)+AIMTOL
                  Y3=-ALENS(10,I)-AIMTOL
                  X4=ALENS(11,I)+AIMTOL
                  Y4=ALENS(10,I)+AIMTOL
              END IF
C
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE. REMEMBER.

              XRD=XRD-ALENS(13,I)-JK1
              YRD=YRD-ALENS(12,I)-JK2
C
C       IF A NON-ZERO CLAP TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

              A15=ALENS(15,I)*PII/180.0D0
              A15=(ALENS(15,I)+JK3)*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
C       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
C       IF YES, SET LS=10.0D0
C       IF NO SET LS=0.0D0
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
                  LS=0.0D0
              ELSE
C       RAY BLOCKED
                  LS=10.0D0
              END IF
C NOW IS THE POINT INSIDE ANY OF THE FOUR CIRCLES
C       CENTER OF THE FIRST CIRCLE IS AT
              XC1=-ALENS(11,I)+ALENS(14,I)
              YC1= ALENS(10,I)-ALENS(14,I)
              CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
C       CENTER OF THE SECOND CIRCLE IS AT
              XC2= -ALENS(11,I)+ALENS(14,I)
              YC2= -ALENS(10,I)+ALENS(14,I)
              CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
C       CENTER OF THE THIRD CIRCLE IS AT
              XC3= ALENS(11,I)-ALENS(14,I)
              YC3=-ALENS(10,I)+ALENS(14,I)
              CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
C       CENTER OF THE FIRST CIRCLE IS AT
              XC4=ALENS(11,I)-ALENS(14,I)
              YC4=ALENS(10,I)-ALENS(14,I)
              CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
C
              RAD2=DSQRT(ALENS(14,I)**2)+AIMTOL
C

              IF(.NOT.INS.AND.REAL(CS1).GT.REAL(RAD2)
     1        .AND.REAL(CS2).GT.REAL(RAD2).AND.
     1        REAL(CS3).GT.REAL(RAD2).AND.REAL(CS4).GT.REAL(RAD2)) THEN
C       RAY IS BLOCKED BY ONE OF THE CIRCLES AND THE BOX
                  LS=10.0D0
              ELSE
C     RAY PASSES THROUGH A CIRCLE
                  LS=0.0D0
              END IF
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY CLAP ON SURFACE I
                  REFMISS=.TRUE.
                  RETURN
              END IF
C       CAFLG NOT 4
          END IF
C
C       CAFLG=5 POLY CLAP, DOES IT STOP THE RAY
          IF(CAFLG.EQ.5) THEN
              LS=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(11,I), CENTER TO CORNER DISTANCE
C       IS ALENS(10,II). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
              ANGLE=0.0D0
              DO III=1,INT(ALENS(11,I))
                  XT(III)=ALENS(10,I)*DCOS(ANGLE+(PII/2.0D0))
                  YT(III)=ALENS(10,I)*DSIN(ANGLE+(PII/2.0D0))
                  ANGLE=ANGLE+((TWOPII)/ALENS(11,I))
              END DO
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
              XRD=XRD-ALENS(13,I)-JK1
              YRD=YRD-ALENS(12,I)-JK2
C
C       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE ERASE. REMEMBER.

              A15=ALENS(15,I)*PII/180.0D0
              A15=(ALENS(15,I)+JK3)*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(11,I))
              INS=INSID1()
              IF(INS) THEN
C       RAY NOT BLOCKED
                  LS=0.0D0
              ELSE
C       RAY BLOCKED
                  LS=10.0D0
              END IF
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY CLAP ON SURFACE I
                  REFMISS=.TRUE.
                  RETURN
              END IF
C       CAFLG NOT 5
          END IF
C
C       CAFLG=6 IPOLY CLAP, DOES IT STOP THE RAY
          IF(CAFLG.EQ.6) THEN
              LS=0.0D0
C
C       POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
              ANGLE=0.0D0
              DO III=1,INT(ALENS(11,I))
                  XT(III)=IPOLYX(III,I,1)
                  YT(III)=IPOLYY(III,I,1)
              END DO
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
              XRD=XRD-ALENS(13,I)-JK1
              YRD=YRD-ALENS(12,I)-JK2
C
C       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE ERASE. REMEMBER.

              A15=ALENS(15,I)*PII/180.0D0
              A15=(ALENS(15,I)+JK3)*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(11,I))
              INS=INSID1()
              IF(INS) THEN
C       RAY NOT BLOCKED
                  LS=0.0D0
              ELSE
C       RAY BLOCKED
                  LS=10.0D0
              END IF
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY CLAP ON SURFACE I
                  REFMISS=.TRUE.
                  RETURN
              END IF
C       CAFLG NOT 6
          END IF
C
          RETURN
      END
