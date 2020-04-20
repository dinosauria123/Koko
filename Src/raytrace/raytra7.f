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
C       THIS IS THE SEVENTH FILE OF RAYTRACING ROUTINES

C SUB SAGCACO.FOR

      SUBROUTINE SAGCACO(JK_I,JK_X,JK_Y,JK_SAG,ISITIN)
C
          IMPLICIT NONE
C
          EXTERNAL INSID1,INSID2
C
C       THIS IS SUBROUTINE SAGCACO.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CLEAR APERTURE CHECKING AS CALLED BY SSAAGG.FOR
C
          INTEGER I,CAFLG,COFLG,N,JK_I,III
C
          REAL*8 X,Y,Z,JK_X,JK_Y,JK_SAG,PRAD,PRAD1,PRAD2
     1    ,XR,YR,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,ANGLE,
     2    X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,
     3    YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4,A15,A22
C
          LOGICAL INS,INSID1,INSID2
C
          LOGICAL NOCOBSPSF
          COMMON/PSFCOBS/NOCOBSPSF
C
          INTEGER CAERAS,COERAS
C
          COMMON/CACO/CAERAS,COERAS,LS
C
C     VARIABLES FOR SPOT TRACING
C
          LOGICAL SPDTRA,ISITIN
C
          COMMON/SPRA1/SPDTRA
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          ISITIN=.FALSE.
C
C       THE CALL TO THIS ROUTINE IS:
C       CALL CACHEK
          I=JK_I
          X=JK_X
          Y=JK_Y
          Z=JK_SAG
C
C       I IS THE CURRENT SURFACE NUMBER
C
C       SET FLAGS CAFLG AND COFLG
          IF(ALENS(9,I).NE.0.0D0) THEN
C       CLAP EXISTS
              CAFLG=INT(DABS(ALENS(9,I)))
          ELSE
              CAFLG=0
          END IF
          IF(ALENS(16,I).NE.0.0D0) THEN
C       COBS EXISTS
              COFLG=INT(DABS(ALENS(16,I)))
          ELSE
              COFLG=0
          END IF
C
C
C       NOW ALL THE CAFLG AND COFLG HAVE BEEN SET
C
C******************************************************************
C       CAFLG NON-ZERO, RESOLVE ALL CLAP BLOCKAGES NOW
C******************************************************************
C
          IF(CAFLG.EQ.1.OR.CAFLG.EQ.0) THEN
C
              LS=0.0D0
C       CIRCULAR CLAP EQUATION IS:
C
C       (X)**2 + (Y)**2 = R**2

              XR=X-ALENS(13,I)
              YR=Y-ALENS(12,I)
C
              LS=DSQRT((XR**2)+(YR**2))
C
              PRAD1=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
              PRAD2=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
              PRAD=PRAD1
              IF(PRAD2.GT.PRAD1) PRAD=PRAD2
C
              IF(CAFLG.EQ.0) THEN
                  RS=DSQRT(PRAD**2)+AIMTOL
              END IF
              IF(CAFLG.EQ.1) THEN
                  IF(DABS(ALENS(10,I)).LE.DABS(ALENS(11,I))) THEN
                      RS=DSQRT(ALENS(10,I)**2)+AIMTOL
                  ELSE
                      RS=DSQRT(ALENS(11,I)**2)+AIMTOL
                  END IF
              END IF
              IF(REAL(LS).GT.REAL(RS)) THEN
                  JK_SAG=0.0D0
                  ISITIN=.FALSE.
                  RETURN
              ELSE
                  ISITIN=.TRUE.
              END IF
C       CAFLG NOT 1
          END IF
C
C
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

              XRD=XRD-ALENS(13,I)
              YRD=YRD-ALENS(12,I)

              A15=ALENS(15,I)*PII/180.0D0
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
                  ISITIN=.TRUE.
              ELSE
                  JK_SAG=0.0D0
                  ISITIN=.FALSE.
                  RETURN
              END IF
C
C       CAFLG NOT 2
          END IF
C
          IF(CAFLG.EQ.3) THEN
C
              LS=0.0D0
C       ELLIPTICAL CLAP EQUATION IS:
C
C       (X)**2/A**2  + (Y)**2/B**2 = 1
C
              XRD=X
              YRD=Y

              XRD=XRD-ALENS(13,I)
              YRD=YRD-ALENS(12,I)
C

              A15=ALENS(15,I)*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
              LS=((XR**2)/(ALENS(11,I)**2))+
     1           ((YR**2)/(ALENS(10,I)**2))
C
              IF(REAL(LS).GT.(1.0+(AIMTOL**2))) THEN
                  JK_SAG=0.0D0
                  ISITIN=.FALSE.
                  RETURN
              ELSE
                  ISITIN=.TRUE.
              END IF
C       CAFLG NOT 3
          END IF
C
C
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

              XRD=XRD-ALENS(13,I)
              YRD=YRD-ALENS(12,I)
C

              A15=ALENS(15,I)*PII/180.0D0
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
                  ISITIN=.TRUE.
              ELSE
                  JK_SAG=0.0D0
                  ISITIN=.FALSE.
                  RETURN
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
                  JK_SAG=0.0D0
                  ISITIN=.FALSE.
                  RETURN
              ELSE
                  ISITIN=.TRUE.
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
              XR=XR-ALENS(13,I)
              YR=YR-ALENS(12,I)
C
C       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE ERASE. REMEMBER.

              A15=ALENS(15,I)*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(11,I))
              INS=INSID1()
              IF(INS) THEN
C       RAY BLOCKED
                  ISITIN=.FALSE.
                  JK_SAG=0.0D0
                  RETURN
              ELSE
                  ISITIN=.TRUE.
              END IF
          ELSE
C       CAFLG NOT 5
          END IF
C
C       CAFLG=6 IPOLY CLAP, DOES IT STOP THE RAY
          IF(CAFLG.EQ.6) THEN
              LS=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(11,I), CENTER TO CORNER DISTANCE
C       IS ALENS(10,II). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
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
              XR=XR-ALENS(13,I)
              YR=YR-ALENS(12,I)
C
C       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       CLEAR APERTURE ERASE. REMEMBER.

              A15=ALENS(15,I)*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(11,I))
              INS=INSID1()
              IF(INS) THEN
C       RAY BLOCKED
                  ISITIN=.FALSE.
                  JK_SAG=0.0D0
                  RETURN
              ELSE
                  ISITIN=.TRUE.
              END IF
          ELSE
C       CAFLG NOT 6
          END IF
C
C******************************************************************
C       COFLG NON-ZERO, RESOLVE ALL COBS BLOCKAGES NOW
C******************************************************************
C
          IF(COFLG.EQ.1) THEN
C
              LS=0.0D0
C       CIRCULAR COBS EQUATION IS:
C
C       (X)**2 + (Y)**2 = R**2
C
              XR=X-ALENS(20,I)
              YR=Y-ALENS(19,I)
C
              LS=DSQRT((XR**2)+(YR**2))
C
              RS=DSQRT(ALENS(17,I)**2)-AIMTOL
              IF(REAL(LS).LT.REAL(RS)) THEN
                  JK_SAG=0.0D0
                  RETURN
              ELSE
              END IF
C       COFLG NOT 1
          END IF
C
          IF(COFLG.EQ.2) THEN
C
              LS=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
C       COORDINATES ARE:
              X1=-ALENS(18,I)-AIMTOL
              Y1=ALENS(17,I)+AIMTOL
              X2=-ALENS(18,I)-AIMTOL
              Y2=-ALENS(17,I)-AIMTOL
              X3=ALENS(18,I)+AIMTOL
              Y3=-ALENS(17,I)-AIMTOL
              X4=ALENS(18,I)+AIMTOL
              Y4=ALENS(17,I)+AIMTOL
              XRD=X
              YRD=Y
C
              XRD=XRD-ALENS(20,I)
              YRD=YRD-ALENS(19,I)
C

              A22=(ALENS(22,I))*PII/180.0D0
              XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
              YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
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
              INS=INSID2()
              IF(INS) THEN
                  JK_SAG=0.0D0
                  RETURN
              ELSE
              END IF
C       COFLG NOT 2
          END IF
C
          IF(COFLG.EQ.3) THEN
C
              LS=0.0D0
C       ELLIPTICAL COBS EQUATION IS:
C
C       (X)**2/A**2  + (Y)**2/B**2 = 1
C
C
              XRD=X
              YRD=Y
C
              XRD=XRD-ALENS(20,I)
              YRD=YRD-ALENS(19,I)
C

              A22=(ALENS(22,I))*PII/180.0D0
              XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
              YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
              LS=((XR**2)/(ALENS(18,I)**2))+
     1           ((YR**2)/(ALENS(17,I)**2))
C
              IF(REAL(LS).LT.(1.0-(AIMTOL**2))) THEN
                  JK_SAG=0.0D0
                  RETURN
              ELSE
              END IF
C       COFLG NOT 3
          END IF
C
C
          IF(COFLG.EQ.4) THEN
C
              LS=0.0D0
C
C       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
C       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
C       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
C       AND THE FOUR CIRCLES NEED TO BE CHECKED.
C
              IF(ALENS(17,I).LE.ALENS(18,I)) THEN
C       ALENS(18,I) = MAXSID
                  MAXSID=ALENS(18,I)
              ELSE
                  MAXSID=ALENS(17,I)
              END IF
              IF(ALENS(21,I).LT.MAXSID) THEN
C       SETUP THE 8 SIDED BOX
                  N=8
                  X1=-ALENS(18,I)+ALENS(21,I)+AIMTOL
                  Y1=ALENS(17,I)-AIMTOL
                  X2=-ALENS(18,I)+AIMTOL
                  Y2=ALENS(17,I)-ALENS(21,I)-AIMTOL
                  X3=-ALENS(18,I)+AIMTOL
                  Y3=-ALENS(17,I)+ALENS(21,I)+AIMTOL
                  X4=-ALENS(18,I)+ALENS(21,I)+AIMTOL
                  Y4=-ALENS(17,I)+AIMTOL
                  X5=ALENS(18,I)-ALENS(21,I)-AIMTOL
                  Y5=-ALENS(17,I)+AIMTOL
                  X6=ALENS(18,I)-AIMTOL
                  Y6=-ALENS(17,I)+ALENS(21,I)+AIMTOL
                  X7=ALENS(18,I)-AIMTOL
                  Y7=ALENS(17,I)-ALENS(21,I)-AIMTOL
                  X8=ALENS(18,I)-ALENS(21,I)-AIMTOL
                  Y8=ALENS(17,I)-AIMTOL
              ELSE
C       SET UP THE FOUR SIDED BOX
                  N=4
                  X1=-ALENS(18,I)-AIMTOL
                  Y1=ALENS(17,I)+AIMTOL
                  X2=-ALENS(18,I)-AIMTOL
                  Y2=-ALENS(17,I)-AIMTOL
                  X3=ALENS(18,I)+AIMTOL
                  Y3=-ALENS(17,I)-AIMTOL
                  X4=ALENS(18,I)+AIMTOL
                  Y4=ALENS(17,I)+AIMTOL
              END IF
C
              XRD=X
              YRD=Y
C
              XRD=XRD-ALENS(20,I)
              YRD=YRD-ALENS(19,I)
C

              A22=(ALENS(22,I))*PII/180.0D0
              XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
              YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
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
              INS=INSID2()
              IF(INS) THEN
                  JK_SAG=0.0D0
                  RETURN
              ELSE
              END IF
C NOW IS THE POINT OUTSIDE OR ON ANY OF THE FOUR CIRCLES
C       CENTER OF THE FIRST CIRCLE IS AT
              XC1=-ALENS(18,I)+ALENS(21,I)
              YC1= ALENS(17,I)-ALENS(21,I)
              CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
C       CENTER OF THE SECOND CIRCLE IS AT
              XC2= -ALENS(18,I)+ALENS(21,I)
              YC2= -ALENS(17,I)+ALENS(21,I)
              CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
C       CENTER OF THE THIRD CIRCLE IS AT
              XC3= ALENS(18,I)-ALENS(21,I)
              YC3=-ALENS(17,I)+ALENS(21,I)
              CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
C       CENTER OF THE FIRST CIRCLE IS AT
              XC4=ALENS(18,I)-ALENS(21,I)
              YC4=ALENS(17,I)-ALENS(21,I)
              CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
C
              RAD2=DSQRT(ALENS(21,I)**2)-AIMTOL
C
              IF(INS.OR.
     1        REAL(CS1).LT.REAL(RAD2).OR.REAL(CS2).LT.REAL(RAD2)
     1        .OR.REAL(CS3).LT.REAL(RAD2).OR.REAL(CS4).LT.REAL(RAD2))
     1                          THEN
                  JK_SAG=0.0D0
                  RETURN
              ELSE
              END IF
C       COFLG NOT 4
          END IF
C
C       COFLG=5 POLY CLAP, DOES IT STOP THE RAY
          IF(COFLG.EQ.5) THEN
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(18,I), CENTER TO CORNER DISTANCE
C       IS ALENS(17,II). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
              ANGLE=0.0D0
              DO III=1,INT(ALENS(18,I))
                  XT(III)=ALENS(17,I)*DCOS(ANGLE+(PII/2.0D0))
                  YT(III)=ALENS(17,I)*DSIN(ANGLE+(PII/2.0D0))
                  ANGLE=ANGLE+((TWOPII)/ALENS(18,I))
              END DO
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
              XR=XR-ALENS(20,I)
              YR=YR-ALENS(19,I)
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED

              A22=(ALENS(22,I))*PII/180.0D0
              XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
              YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(18,I))
              INS=INSID2()
              IF(INS) THEN
C       RAY BLOCKED
                  JK_SAG=0.0D0
                  RETURN
              ELSE
                  RETURN
C       RAY NOT BLOCKED
              END IF
C
          ELSE
C       COFLG NOT 5
          END IF
C
C       COFLG=6 IPOLY CLAP, DOES IT STOP THE RAY
          IF(COFLG.EQ.6) THEN
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(18,I), CENTER TO CORNER DISTANCE
C       IS ALENS(17,II). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
              DO III=1,INT(ALENS(18,I))
                  XT(III)=IPOLYX(III,I,3)
                  YT(III)=IPOLYY(III,I,3)
              END DO
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
              XR=XR-ALENS(20,I)
              YR=YR-ALENS(19,I)
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED

              A22=(ALENS(22,I))*PII/180.0D0
              XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
              YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(18,I))
              INS=INSID2()
              IF(INS) THEN
C       RAY BLOCKED
                  JK_SAG=0.0D0
                  RETURN
              ELSE
                  RETURN
C       RAY NOT BLOCKED
              END IF
C
          ELSE
C       COFLG NOT 6
          END IF
          RETURN
      END
C SUB FDISTOP.FOR
      SUBROUTINE FDISTOP(IW1,ERROR)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FDISTOP. THIS SUBROUTINE IMPLEMENTS
C       THE FISHEYE DIST IN OPTIMIZATION
C
C          IW1=FIELD NUMBER
C
          REAL*8 PCY,PCX,PUCY,PUCX
     1    ,XREAL,YREAL,PY,PX,PUY,PUX,HR,HP,HPU,HRU,FACTOR,FACTOR2
     2    ,MYW1,MYW2,MYW3,MYW4
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K,ERROR,IW1
C
          REAL*8 VALUE1
          REAL*8 O18,O19
          COMMON/O18O19/O18,O19
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsub.inc'
          GRASET=.FALSE.
          O18=SYSTEM1(18)
          O19=SYSTEM1(19)
          SYSTEM1(18)=0.0D0
          SYSTEM1(19)=0.0D0
C
          ERROR=0
          K=NEWIMG
          MYW1=FIELDY(IW1)
          MYW2=FIELDX(IW1)
          MYW3=FIELDZ(IW1)
          MYW4=0.0D0
          CALL GNPRT(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1    ,0.0D0,0.0D0,0.0D0,MYW4,0)

          IF(ERROR.EQ.1) THEN
              SYSTEM1(18)=O18
              SYSTEM1(19)=O19
              RETURN
          END IF
C     PROCEED WITH DISTORTION CALCULATION USING PCY AND PCX
C     TRACE THE DESIRED GUT RAY WITHOUT DIFFERENTIAL TRACING
C     AND GET ITS X AND Y POSITIONS AT THE IMAGE PLANE.
          SAVE_KDP(15)=SAVEINPT(15)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.FALSE.
          LDIF=.FALSE.
          WQ='        '
          SQ=0
          SST=0
          STI=0
          W1=MYW1
          W2=MYW2
          W3=MYW3
          W4=MYW4
          W5=0.0D0
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
C     SET MSG TO FALSE
          MSG=.FALSE.
          ERROR=0
          WC='FOB     '
          CALL FFOB
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(1)=RESTINPT(1)
          IF(ERROR.EQ.1) THEN
              SYSTEM1(18)=O18
              SYSTEM1(19)=O19
              RETURN
          END IF
          IF(SYSTEM1(30).LE.2.0D0) THEN
C     MODE FOCAL
C     PARAX IMAGE POSITION IS JUST
              HP=DSQRT(((MYW1*PCY)**2)+((PCX*MYW2)**2))
              HPU=DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2))
C     REAL IMAGE POSITION IS JUST
              XREAL=GUTRAY(1,NEWIMG)-REFRY(1,NEWIMG)
              YREAL=GUTRAY(2,NEWIMG)-REFRY(2,NEWIMG)
              HR=DSQRT(((YREAL)**2)+((XREAL)**2))
C     REAL ANGLE IS JUST
              XREAL=GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG)
              YREAL=GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG)
              HRU=DSQRT(((YREAL)**2)+((XREAL)**2))
              IF(HRU.NE.0.0D0) FACTOR=(HR/DTAN(HRU))*HRU
              IF(HRU.EQ.0.0D0) FACTOR=0.0D0
              IF(HPU.NE.0.0D0) FACTOR2=(HP/HPU)*DATAN(HPU)
              IF(HPU.EQ.0.0D0) FACTOR2=0.0D0
              IF(FACTOR2.NE.0.0D0) THEN
                  VALUE1=((FACTOR-FACTOR2)/FACTOR2)*100.0D0
              END IF
              IF(FACTOR2.EQ.0.0D0) VALUE1=0.0D0
          END IF
          IF(SYSTEM1(30).GE.3.0D0) THEN
C     PROCEED WITH FISHDIST CALC.
C     PARAX IMAGE SLOPE IS JUST
              HP=DATAN(DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2)))
C     REAL IMAGE SLOPE IS JUST
              XREAL=(GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG))
              YREAL=(GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG))
              IF(DABS(XREAL).GT.PII) XREAL=DABS((TWOPII)-DABS(XREAL))
              IF(DABS(YREAL).GT.PII) YREAL=DABS((TWOPII)-DABS(YREAL))
              IF(DABS(XREAL).GE.(PII/2.0D0))XREAL=DABS((DABS(XREAL))-PII)
              IF(DABS(YREAL).GE.(PII/2.0D0))YREAL=DABS((DABS(YREAL))-PII)
              HR=DSQRT(((YREAL)**2)+((XREAL)**2))
              IF(HP.NE.0.0D0) VALUE1=((HR-HP)/HP)*100.0D0
              IF(HP.EQ.0.0D0) VALUE1=0.0D0
          END IF
          SYSTEM1(18)=O18
          SYSTEM1(19)=O19
          RETURN
      END
C SUB FDISTOR.FOR
      SUBROUTINE FDISTOR(DWORD1,DWORD2,ERROR)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE DISTOR. THIS SUBROUTINE IMPLEMENTS
C       THE GET DIST COMMAND
C
C     THE GET DIST COMMAND RESTURNS IN VALUE1, THE VALUE1 OF THE
C     DISTORTION IN PERCENT AT THE FOB POSITION GIVEN BY:
C          YFOB=DWORD1
C          XFOB=DWORD2
C
          REAL*8 PCY,PCX,PUCY,PUCX
     1    ,XREAL,YREAL,PY,PX,PUY,PUX,HR,HP,DWORD1,DWORD2
     2    ,MYW1,MYW2,MYW3,MYW4,HPU,HRU,FACTOR,FACTOR2
          REAL*8 O18,O19
          COMMON/O18O19/O18,O19
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K,ERROR
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          O18=SYSTEM1(18)
          O19=SYSTEM1(19)
          SYSTEM1(18)=0.0D0
          SYSTEM1(19)=0.0D0
C
          ERROR=0
          K=NEWIMG
C
          IF(DWORD1.EQ.0.0D0) DWORD1=1.0D-10
          IF(DWORD2.EQ.0.0D0) DWORD2=1.0D-10
          MYW1=DWORD1
          MYW2=DWORD2
          MYW3=0.0D0
          MYW4=SYSTEM1(11)
          CALL GNPRT(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1    ,0.0D0,0.0D0,0.0D0,MYW4,0)

          IF(ERROR.EQ.1) THEN
              SYSTEM1(18)=O18
              SYSTEM1(19)=O19
              RETURN
          END IF
C     PROCEED WITH DISTORTION CALCULATION USING PCY AND PCX
C     TRACE THE DESIRED GUT RAY WITHOUT DIFFERENTIAL TRACING
C     AND GET ITS X AND Y POSITIONS AT THE IMAGE PLANE.
          SAVE_KDP(15)=SAVEINPT(15)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.FALSE.
          LDIF=.FALSE.
          WQ='        '
          SQ=0
          SST=0
          STI=0
          W1=MYW1
          W2=MYW2
          W3=MYW3
          W4=MYW4
          W5=0.0D0
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
C     SET MSG TO FALSE
          MSG=.FALSE.
          ERROR=0
          WC='FOB     '
          CALL FFOB
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(1)=RESTINPT(1)
          IF(ERROR.EQ.1) THEN
              SYSTEM1(18)=O18
              SYSTEM1(19)=O19
              RETURN
          END IF
          IF(SYSTEM1(30).LE.2.0D0) THEN
C     MODE FOCAL
C     PARAX IMAGE POSITION IS JUST
              HP=DSQRT(((MYW1*PCY)**2)+((PCX*MYW2)**2))
              HPU=DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2))
C     REAL IMAGE POSITION IS JUST
              XREAL=GUTRAY(1,NEWIMG)-REFRY(1,NEWIMG)
              YREAL=GUTRAY(2,NEWIMG)-REFRY(2,NEWIMG)
              HR=DSQRT(((YREAL)**2)+((XREAL)**2))
              XREAL=GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG)
              YREAL=GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG)
              HRU=DSQRT(((YREAL)**2)+((XREAL)**2))
              IF(HRU.NE.0.0D0) FACTOR=(HR/DTAN(HRU))*HRU
              IF(HRU.EQ.0.0D0) FACTOR=0.0D0
              IF(HPU.NE.0.0D0) FACTOR2=(HP/HPU)*DATAN(HPU)
              IF(HPU.EQ.0.0D0) FACTOR2=0.0D0
              IF(FACTOR2.NE.0.0D0) THEN
                  VALUE1=((FACTOR-FACTOR2)/FACTOR2)*100.0D0
              END IF
              IF(FACTOR2.EQ.0.0D0) VALUE1=0.0D0
          END IF
          IF(SYSTEM1(30).GE.3.0D0) THEN
C     PROCEED WITH FISHDIST CALC.
C     PARAX IMAGE SLOPE IS JUST
              HP=DATAN(DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2)))
C     REAL IMAGE SLOPE IS JUST
              XREAL=(GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG))
              YREAL=(GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG))
              IF(DABS(XREAL).GT.PII) XREAL=DABS((TWOPII)-DABS(XREAL))
              IF(DABS(YREAL).GT.PII) YREAL=DABS((TWOPII)-DABS(YREAL))
              IF(DABS(XREAL).GE.(PII/2.0D0))XREAL=DABS((DABS(XREAL))-PII)
              IF(DABS(YREAL).GE.(PII/2.0D0))YREAL=DABS((DABS(YREAL))-PII)
              HR=DSQRT(((YREAL)**2)+((XREAL)**2))
              IF(HP.NE.0.0D0) VALUE1=((HR-HP)/HP)*100.0D0
              IF(HP.EQ.0.0D0) VALUE1=0.0D0
          END IF
          SYSTEM1(18)=O18
          SYSTEM1(19)=O19
          RETURN
      END
C SUB DISTOP.FOR
      SUBROUTINE DISTOP(IW1,ERROR)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE DISTOP. THIS SUBROUTINE IMPLEMENTS
C       THE DIST IN OPTIMIZATION
C
C          IW1=FIELD NUMBER
C
          REAL*8 PCY,PCX,PUCY,PUCX
     1    ,XREAL,YREAL,PY,PX,PUY,PUX,HR,HP
     2    ,MYW1,MYW2,MYW3,MYW4
          REAL*8 O18,O19
          COMMON/O18O19/O18,O19
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K,ERROR,IW1
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsub.inc'
          GRASET=.FALSE.
          O18=SYSTEM1(18)
          O19=SYSTEM1(19)
          SYSTEM1(18)=0.0D0
          SYSTEM1(19)=0.0D0
C
          ERROR=0
          K=NEWIMG
          MYW1=FIELDY(IW1)
          MYW2=FIELDX(IW1)
          MYW3=FIELDZ(IW1)
          MYW4=0.0D0
          CALL GNPRT(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1    ,0.0D0,0.0D0,0.0D0,MYW4,0)

          IF(ERROR.EQ.1) THEN
              SYSTEM1(18)=O18
              SYSTEM1(19)=O19
              RETURN
          END IF
C     PROCEED WITH DISTORTION CALCULATION USING PCY AND PCX
C     TRACE THE DESIRED GUT RAY WITHOUT DIFFERENTIAL TRACING
C     AND GET ITS X AND Y POSITIONS AT THE IMAGE PLANE.
          SAVE_KDP(15)=SAVEINPT(15)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.FALSE.
          LDIF=.FALSE.
          WQ='        '
          SQ=0
          SST=0
          STI=0
          W1=MYW1
          W2=MYW2
          W3=MYW3
          W4=MYW4
          W5=0.0D0
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
C     SET MSG TO FALSE
          MSG=.FALSE.
          ERROR=0
          WC='FOB     '
          CALL FFOB
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(1)=RESTINPT(1)
          IF(ERROR.EQ.1) THEN
              SYSTEM1(18)=O18
              SYSTEM1(19)=O19
              RETURN
          END IF
C     PROCEED WITH DIST CALC.
          IF(SYSTEM1(30).LE.2.0D0) THEN
C     MODE FOCAL
C     PARAX IMAGE POSITION IS JUST
              HP=DSQRT(((MYW1*PCY)**2)+((PCX*MYW2)**2))
C     REAL IMAGE POSITION IS JUST
              XREAL=GUTRAY(1,NEWIMG)-REFRY(1,NEWIMG)
              YREAL=GUTRAY(2,NEWIMG)-REFRY(2,NEWIMG)
              HR=DSQRT(((YREAL)**2)+((XREAL)**2))
              IF(HP.NE.0.0D0) VALUE1=((HR-HP)/HP)*100.0D0
              IF(HP.EQ.0.0D0) VALUE1=0.0D0
          END IF
          IF(SYSTEM1(30).GE.3.0D0) THEN
C     MODE AFOCAL
C     PARAX IMAGE SLOPE IS JUST
              HP=DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2))
C     REAL IMAGE SLOPE IS JUST
              XREAL=DTAN(GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG))
              YREAL=DTAN(GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG))
              HR=DSQRT(((YREAL)**2)+((XREAL)**2))
              IF(HP.NE.0.0D0) VALUE1=((HR-HP)/HP)*100.0D0
              IF(HP.EQ.0.0D0) VALUE1=0.0D0
          END IF
          SYSTEM1(18)=O18
          SYSTEM1(19)=O19
          RETURN
      END
C SUB GNPR1.FOR
      SUBROUTINE GNPR1(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
C
C     PY
C
          IMPLICIT NONE
C
          REAL*8 VAL1,VAL2,SIG,XHT,YHT
     1    ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INTEGER ERROR,GFLAG
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          ERROR=0
C
          IF(GFLAG.EQ.1) THEN
C     GAUSSIAN CALC
C     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
C
              IF(NEWREF.NE.1) THEN
                  OLDREF=NEWREF
                  NEWREF=1
              END IF
              XHT=SYSTEM1(87)*0.001D0*ALENS(3,0)
              YHT=SYSTEM1(88)*0.001D0*ALENS(3,0)
              SYS13=SYSTEM1(85)
              SYS12=SYSTEM1(86)
          END IF
          IF(GFLAG.EQ.0) THEN
C     NON-GAUSSIAN CALC
C       THE REF OBJECT HEIGHTS ARE:
C       (AT THE OBJECT SURFACE)
              XHT=PXTRAX(5,NEWOBJ)
              YHT=PXTRAY(5,NEWOBJ)
C     SET REF AP HT
C
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              ELSE
C     TEL ON
                  SYS13=PXTRAX(1,NEWOBJ+1)
                  SYS12=PXTRAY(1,NEWOBJ+1)
              END IF
          END IF
C
C     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
C     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
C     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
C     AT THE IMAGE.
C     DO AN FOB 0
          SAVE_KDP(13)=SAVEINPT(13)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.TRUE.
          LDIF=.TRUE.
          WQ='        '
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
          W1=NWN1
          W2=NWN2
          W3=NWN3
          W4=NWN4
C     SET MSG TO FALSE
          MSG=.FALSE.
          WC='FOB     '
          CALL FFOB
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          WQ='        '
          SQ=0
          SST=0
          STI=0
          DF1=1
          DF2=1
          DF3=0
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=1
          S4=0
          S5=0
          SN=1
          W3=NWN4
          WC='RAY     '
          NOCOAT=.TRUE.
          GRASET=.FALSE.
          DXFSET=.FALSE.
          CALL RRAY
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
              ERROR=1
              RETURN
          END IF
C
C     PY
          VAL1=(DIFF(8,K)-RAYRAY(2,K))/DELY
          VAL2=(DIFF(7,K)-RAYRAY(1,K))/DELY
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          VALUE1=SIG*DSQRT((VAL1**2)+(VAL2**2))
          VALUE1=VALUE1*SYS12
          IF(GFLAG.EQ.1) NEWREF=OLDREF
          RETURN
      END
C SUB GNPR2.FOR
      SUBROUTINE GNPR2(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
C
C     PX
C
          IMPLICIT NONE
C
          REAL*8 VAL1,VAL2,SIG,XHT,YHT
     1    ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
C
          REAL*8 VALUE1
          INTEGER NUM5,GFLAG
          COMMON/GV/VALUE1,NUM5
C
          INTEGER ERROR
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          ERROR=0
C
          IF(GFLAG.EQ.1) THEN
C     GAUSSIAN CALC
C     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
C
              IF(NEWREF.NE.1) THEN
                  OLDREF=NEWREF
                  NEWREF=1
              END IF
              XHT=SYSTEM1(87)*0.001D0*ALENS(3,0)
              YHT=SYSTEM1(88)*0.001D0*ALENS(3,0)
              SYS13=SYSTEM1(85)
              SYS12=SYSTEM1(86)
          END IF
          IF(GFLAG.EQ.0) THEN
C     NON-GAUSSIAN CALC
C       THE REF OBJECT HEIGHTS ARE:
              XHT=PXTRAX(5,NEWOBJ)
              YHT=PXTRAY(5,NEWOBJ)
C     SET REF AP HT
C
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              ELSE
C     TEL ON
                  SYS13=PXTRAX(1,NEWOBJ+1)
                  SYS12=PXTRAY(1,NEWOBJ+1)
              END IF
          END IF
C
C     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
C     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
C     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
C     AT THE IMAGE.
C     DO AN FOB 0
          SAVE_KDP(13)=SAVEINPT(13)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.TRUE.
          LDIF=.TRUE.
          WQ='        '
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
          W1=NWN1
          W2=NWN2
          W3=NWN3
          W4=NWN4
          SN=1
C     SET MSG TO FALSE
          MSG=.FALSE.
          WC='FOB     '
          CALL FFOB
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          WQ='        '
          SQ=0
          SST=0
          STI=0
          DF1=1
          DF2=1
          DF3=0
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=1
          S4=0
          S5=0
          SN=1
          W3=NWN4
          WC='RAY     '
          NOCOAT=.TRUE.
          GRASET=.FALSE.
          DXFSET=.FALSE.
          CALL RRAY
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
              ERROR=1
              RETURN
          END IF
C
C     PX
          VAL1=(DIFF(1,K)-RAYRAY(1,K))/DELX
          VAL2=(DIFF(2,K)-RAYRAY(2,K))/DELX
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          VALUE1=SIG*DSQRT((VAL1**2)+(VAL2**2))
          VALUE1=VALUE1*SYS13
          IF(GFLAG.EQ.1) NEWREF=OLDREF
          RETURN
      END
C SUB GNPR3.FOR
      SUBROUTINE GNPR3(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
C
C     PUY
C
          IMPLICIT NONE
C
          REAL*8 V1,VAL1,VAL2,SIG,XHT,YHT
     1    ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INTEGER ERROR
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K,GFLAG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          ERROR=0
C
          IF(GFLAG.EQ.1) THEN
C     GAUSSIAN CALC
C     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
C
              IF(NEWREF.NE.1) THEN
                  OLDREF=NEWREF
                  NEWREF=1
              END IF
              XHT=SYSTEM1(87)*0.001D0*ALENS(3,0)
              YHT=SYSTEM1(88)*0.001D0*ALENS(3,0)
              SYS13=SYSTEM1(85)
              SYS12=SYSTEM1(86)
          END IF
          IF(GFLAG.EQ.0) THEN
C     NON-GAUSSIAN CALC
C       THE REF OBJECT HEIGHTS ARE:
              XHT=PXTRAX(5,NEWOBJ)
              YHT=PXTRAY(5,NEWOBJ)
C     SET REF AP HT
C
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              ELSE
C     TEL ON
                  SYS13=PXTRAX(1,NEWOBJ+1)
                  SYS12=PXTRAY(1,NEWOBJ+1)
              END IF
          END IF
C
C     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
C     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
C     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
C     AT THE IMAGE.
C     DO AN FOB 0
          SAVE_KDP(13)=SAVEINPT(13)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.TRUE.
          LDIF=.TRUE.
          WQ='        '
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
          W1=NWN1
          W2=NWN2
          W3=NWN3
          W4=NWN4
          SN=1
C     SET MSG TO FALSE
          MSG=.FALSE.
          WC='FOB     '
          CALL FFOB
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          WQ='        '
          SQ=0
          SST=0
          STI=0
          DF1=1
          DF2=1
          DF3=0
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=1
          S4=0
          S5=0
          SN=1
          W3=NWN4
          WC='RAY     '
          NOCOAT=.TRUE.
          GRASET=.FALSE.
          DXFSET=.FALSE.
          CALL RRAY
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
              ERROR=1
              RETURN
          END IF
C
C     PUY
          IF(DABS(DIFF(11,K)).GE.
     1    (1.0D35*DABS(DIFF(12,K)))) THEN
              IF(SNGL(DIFF(11,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(11,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(11,K),DIFF(12,K))
              END IF
C        IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-RAYRAY(12,K))
C      IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/DELY)*SYS12
          IF(DABS(DIFF(10,K)).GE.
     1    (1.0D35*DABS(DIFF(12,K)))) THEN
              IF(SNGL(DIFF(10,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(10,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(10,K),DIFF(12,K))
              END IF
C       IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-RAYRAY(11,K))
C     IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/DELY)*SYS12
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          VALUE1=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
C
          IF(GFLAG.EQ.1) NEWREF=OLDREF
          RETURN
      END
C SUB GNPR4.FOR
      SUBROUTINE GNPR4(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
C
C     PUX
C
          IMPLICIT NONE
C
          REAL*8 V1,VAL1,VAL2,SIG,XHT,YHT
     1    ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
C
          REAL*8 VALUE1
          INTEGER NUM5,GFLAG
          COMMON/GV/VALUE1,NUM5
C
          INTEGER ERROR
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          ERROR=0
C
          IF(GFLAG.EQ.1) THEN
C     GAUSSIAN CALC
C     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
C
              IF(NEWREF.NE.1) THEN
                  OLDREF=NEWREF
                  NEWREF=1
              END IF
              XHT=SYSTEM1(87)*0.001D0*ALENS(3,0)
              YHT=SYSTEM1(88)*0.001D0*ALENS(3,0)
              SYS13=SYSTEM1(85)
              SYS12=SYSTEM1(86)
          END IF
          IF(GFLAG.EQ.0) THEN
C     NON-GAUSSIAN CALC
C       THE REF OBJECT HEIGHTS ARE:
C       (AT THE OBJECT SURFACE)
              XHT=PXTRAX(5,NEWOBJ)
              YHT=PXTRAY(5,NEWOBJ)
c
C     SET REF AP HT
C
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              ELSE
C     TEL ON
                  SYS13=PXTRAX(1,NEWOBJ+1)
                  SYS12=PXTRAY(1,NEWOBJ+1)
              END IF
          END IF
C
C     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
C     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
C     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
C     AT THE IMAGE.
C     DO AN FOB 0
          SAVE_KDP(13)=SAVEINPT(13)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.TRUE.
          LDIF=.TRUE.
          WQ='        '
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
          W1=NWN1
          W2=NWN2
          W3=NWN3
          W4=NWN4
          SN=1
C     SET MSG TO FALSE
          MSG=.FALSE.
          WC='FOB     '
          CALL FFOB
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          WQ='        '
          SQ=0
          SST=0
          STI=0
          DF1=1
          DF2=1
          DF3=0
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=1
          S4=0
          S5=0
          SN=1
          W3=NWN4
          WC='RAY     '
          NOCOAT=.TRUE.
          GRASET=.FALSE.
          DXFSET=.FALSE.
          CALL RRAY
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
              ERROR=1
              RETURN
          END IF
C
C     PUX
          IF(DABS(DIFF(4,K)).GE.
     1    (1.0D35*DABS(DIFF(6,K)))) THEN
              IF(SNGL(DIFF(4,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(4,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(4,K),DIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-RAYRAY(11,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/DELX)*SYS13
          IF(DABS(DIFF(5,K)).GE.
     1    (1.0D35*DABS(DIFF(6,K)))) THEN
              IF(SNGL(DIFF(5,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(5,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(5,K),DIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-RAYRAY(12,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/DELX)*SYS13
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          VALUE1=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
C     ALL THE PARAXIAL STUFF IS DONE
          IF(GFLAG.EQ.1) NEWREF=OLDREF
          RETURN
      END
C SUB GNPR5.FOR
      SUBROUTINE GNPR5(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
C
C     PCY
C
          IMPLICIT NONE
C
          REAL*8 VAL1,VAL2,SIG,XHT,YHT
     1    ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INTEGER ERROR
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K,GFLAG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          ERROR=0
C
          IF(GFLAG.EQ.1) THEN
C     GAUSSIAN CALC
C     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
C
              IF(NEWREF.NE.1) THEN
                  OLDREF=NEWREF
                  NEWREF=1
              END IF
              XHT=SYSTEM1(87)*0.001D0*ALENS(3,0)
              YHT=SYSTEM1(88)*0.001D0*ALENS(3,0)
              SYS13=SYSTEM1(85)
              SYS12=SYSTEM1(86)
          END IF
          IF(GFLAG.EQ.0) THEN
C     NON-GAUSSIAN CALC
C       THE REF OBJECT HEIGHTS ARE:
C       (AT THE OBJECT SURFACE)
              XHT=PXTRAX(5,NEWOBJ)
              YHT=PXTRAY(5,NEWOBJ)
C
C     SET REF AP HT
C
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              ELSE
C     TEL ON
                  SYS13=PXTRAX(1,NEWOBJ+1)
                  SYS12=PXTRAY(1,NEWOBJ+1)
              END IF
          END IF
C
C     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
C     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
C     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
C     AT THE IMAGE.
C     DO AN FOB 0
          SAVE_KDP(13)=SAVEINPT(13)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.TRUE.
          LDIF=.TRUE.
          WQ='        '
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
          W1=NWN1
          W2=NWN2
          W3=NWN3
          W4=NWN4
          SN=1
C     SET MSG TO FALSE
          MSG=.FALSE.
          WC='FOB     '
          CALL FFOB
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          WQ='        '
          SQ=0
          SST=0
          STI=0
          DF1=1
          DF2=1
          DF3=0
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=1
          S4=0
          S5=0
          SN=1
          W3=NWN4
          WC='RAY     '
          NOCOAT=.TRUE.
          GRASET=.FALSE.
          DXFSET=.FALSE.
          CALL RRAY
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
              ERROR=1
              RETURN
          END IF
C
C     PCY
          VAL1=(RFDIFF(8,K)-REFRY(2,K))/RFDELY
          VAL2=(RFDIFF(7,K)-REFRY(1,K))/RFDELY
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1=SIG*DSQRT((VAL1**2)+(VAL2**2))
          VALUE1=VALUE1*YHT
C
          IF(GFLAG.EQ.1) NEWREF=OLDREF
          RETURN
      END
C SUB GNPR6.FOR
      SUBROUTINE GNPR6(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
C
C     PCX
C
          IMPLICIT NONE
C
          REAL*8 VAL1,VAL2,SIG,XHT,YHT
     1    ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INTEGER ERROR
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K,GFLAG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          ERROR=0
C
          IF(GFLAG.EQ.1) THEN
C     GAUSSIAN CALC
C     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
C
              IF(NEWREF.NE.1) THEN
                  OLDREF=NEWREF
                  NEWREF=1
              END IF
              XHT=SYSTEM1(87)*0.001D0*ALENS(3,0)
              YHT=SYSTEM1(88)*0.001D0*ALENS(3,0)
              SYS13=SYSTEM1(85)
              SYS12=SYSTEM1(86)
          END IF
          IF(GFLAG.EQ.0) THEN
C     NON-GAUSSIAN CALC
C       THE REF OBJECT HEIGHTS ARE:
C       (AT THE OBJECT SURFACE)
              XHT=PXTRAX(5,NEWOBJ)
              YHT=PXTRAY(5,NEWOBJ)
C     SET REF AP HT
C
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              ELSE
C     TEL ON
                  SYS13=PXTRAX(1,NEWOBJ+1)
                  SYS12=PXTRAY(1,NEWOBJ+1)
              END IF
          END IF
C
C     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
C     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
C     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
C     AT THE IMAGE.
C     DO AN FOB 0
          SAVE_KDP(13)=SAVEINPT(13)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.TRUE.
          LDIF=.TRUE.
          WQ='        '
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
          W1=NWN1
          W2=NWN2
          W3=NWN3
          W4=NWN4
          SN=1
C     SET MSG TO FALSE
          MSG=.FALSE.
          WC='FOB     '
          CALL FFOB
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          WQ='        '
          SQ=0
          SST=0
          STI=0
          DF1=1
          DF2=1
          DF3=0
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=1
          S4=0
          S5=0
          SN=1
          W3=NWN4
          WC='RAY     '
          NOCOAT=.TRUE.
          GRASET=.FALSE.
          DXFSET=.FALSE.
          CALL RRAY
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
              ERROR=1
              RETURN
          END IF
C
C     PCX
          VAL1=(RFDIFF(1,K)-REFRY(1,INT(K)))/RFDELX
          VAL2=(RFDIFF(2,K)-REFRY(2,K))/RFDELX
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1=SIG*DSQRT((VAL1**2)+(VAL2**2))
          VALUE1=VALUE1*XHT
          IF(GFLAG.EQ.1) NEWREF=OLDREF
          RETURN
      END
C SUB GNPR7.FOR
      SUBROUTINE GNPR7(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
C
C     PUCY
C
          IMPLICIT NONE
C
          REAL*8 V1,VAL1,VAL2,SIG,XHT,YHT
     1    ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INTEGER ERROR
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K,GFLAG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          ERROR=0
C
          IF(GFLAG.EQ.1) THEN
C     GAUSSIAN CALC
C     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
C
              IF(NEWREF.NE.1) THEN
                  OLDREF=NEWREF
                  NEWREF=1
              END IF
              XHT=SYSTEM1(87)*0.001D0*ALENS(3,0)
              YHT=SYSTEM1(88)*0.001D0*ALENS(3,0)
              SYS13=SYSTEM1(85)
              SYS12=SYSTEM1(86)
          END IF
          IF(GFLAG.EQ.0) THEN
C     NON-GAUSSIAN CALC
C       THE REF OBJECT HEIGHTS ARE:
C       (AT THE OBJECT SURFACE)
              XHT=PXTRAX(5,NEWOBJ)
              YHT=PXTRAY(5,NEWOBJ)
C     SET REF AP HT
C
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              ELSE
C     TEL ON
                  SYS13=PXTRAX(1,NEWOBJ+1)
                  SYS12=PXTRAY(1,NEWOBJ+1)
              END IF
          END IF
C
C     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
C     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
C     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
C     AT THE IMAGE.
C     DO AN FOB 0
          SAVE_KDP(13)=SAVEINPT(13)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.TRUE.
          LDIF=.TRUE.
          WQ='        '
          SQ=0
          SST=0
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
          W1=NWN1
          W2=NWN2
          W3=NWN3
          W4=NWN4
          SN=1
C     SET MSG TO FALSE
          MSG=.FALSE.
          WC='FOB     '
          CALL FFOB
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          WQ='        '
          SQ=0
          SST=0
          STI=0
          DF1=1
          DF2=1
          DF3=0
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=1
          S4=0
          S5=0
          SN=1
          W3=NWN4
          WC='RAY     '
          NOCOAT=.TRUE.
          GRASET=.FALSE.
          DXFSET=.FALSE.
          CALL RRAY
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
              ERROR=1
              RETURN
          END IF
C
C     PUCY
          IF(DABS(RFDIFF(11,K)).GE.
     1    (1.0D35*DABS(RFDIFF(12,K)))) THEN
              IF(SNGL(RFDIFF(11,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(11,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(11,K),RFDIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-REFRY(12,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/RFDELY)*YHT
          IF(DABS(RFDIFF(10,K)).GE.
     1    (1.0D35*DABS(RFDIFF(12,K)))) THEN
              IF(SNGL(RFDIFF(10,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(10,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(10,K),RFDIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-REFRY(11,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/RFDELY)*YHT
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
          IF(GFLAG.EQ.1) NEWREF=OLDREF
          RETURN
      END
C SUB GNPR8.FOR
      SUBROUTINE GNPR8(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
C
C     PUCX
C
          IMPLICIT NONE
C
          REAL*8 V1,VAL1,VAL2,SIG,XHT,YHT
     1    ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INTEGER ERROR
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K,GFLAG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          ERROR=0
C
          IF(GFLAG.EQ.1) THEN
C     GAUSSIAN CALC
C     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
C
              IF(NEWREF.NE.1) THEN
                  OLDREF=NEWREF
                  NEWREF=1
              END IF
              XHT=SYSTEM1(87)*0.001D0*ALENS(3,0)
              YHT=SYSTEM1(88)*0.001D0*ALENS(3,0)
              SYS13=SYSTEM1(85)
              SYS12=SYSTEM1(86)
          END IF
          IF(GFLAG.EQ.0) THEN
C     NON-GAUSSIAN CALC
C       THE REF OBJECT HEIGHTS ARE:
C       (AT THE OBJECT SURFACE)
              XHT=PXTRAX(5,NEWOBJ)
              YHT=PXTRAY(5,NEWOBJ)
C     SET REF AP HT
C
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              ELSE
C     TEL ON
                  SYS13=PXTRAX(1,NEWOBJ+1)
                  SYS12=PXTRAY(1,NEWOBJ+1)
              END IF
          END IF
C
C     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
C     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
C     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
C     AT THE IMAGE.
C     DO AN FOB 0
          SAVE_KDP(13)=SAVEINPT(13)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.TRUE.
          LDIF=.TRUE.
          WQ='        '
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
          W1=NWN1
          W2=NWN2
          W3=NWN3
          W4=NWN4
          SN=1
C     SET MSG TO FALSE
          MSG=.FALSE.
          WC='FOB     '
          CALL FFOB
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          WQ='        '
          SQ=0
          SST=0
          STI=0
          DF1=1
          DF2=1
          DF3=0
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=1
          S4=0
          S5=0
          SN=1
          W3=NWN4
          WC='RAY     '
          NOCOAT=.TRUE.
          GRASET=.FALSE.
          DXFSET=.FALSE.
          CALL RRAY
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
              ERROR=1
              RETURN
          END IF
C
C     PUCX
          IF(DABS(RFDIFF(4,K)).GE.
     1    (1.0D35*DABS(RFDIFF(6,K)))) THEN
              IF(SNGL(RFDIFF(4,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(4,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(4,K),RFDIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-REFRY(11,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/RFDELX)*XHT
          IF(DABS(RFDIFF(5,K)).GE.
     1    (1.0D35*DABS(RFDIFF(6,K)))) THEN
              IF(SNGL(RFDIFF(5,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(5,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(5,K),RFDIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-REFRY(12,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/RFDELX)*XHT
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
          IF(GFLAG.EQ.1) NEWREF=OLDREF
          RETURN
      END
C SUB GNPRT.FOR
      SUBROUTINE GNPRT(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR,
     1MYW1,MYW2,MYW3,MYW4,GFLAG)
C
          IMPLICIT NONE
C
          REAL*8 V1,PX,PY,PUX,PUY,PCX,VAL1,VAL2,SIG,XHT,YHT
     1    ,SYS12,SYS13,PCY,PUCX,PUCY,MYW1,MYW2,MYW3,MYW4
C
          INTEGER ERROR,GFLAG
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          PY=0.0D0
          PX=0.0D0
          PCY=0.0D0
          PCX=0.0D0
          PUY=0.0D0
          PUX=0.0D0
          PUCY=0.0D0
          PUCX=0.0D0
C
          ERROR=0
C
          IF(GFLAG.EQ.1) THEN
C     GAUSSIAN CALC
C     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
C
              IF(NEWREF.NE.1) THEN
                  OLDREF=NEWREF
                  NEWREF=1
              END IF
              XHT=SYSTEM1(87)*0.001D0*ALENS(3,0)
              YHT=SYSTEM1(88)*0.001D0*ALENS(3,0)
              SYS13=SYSTEM1(85)
              SYS12=SYSTEM1(86)
          END IF
          IF(GFLAG.EQ.0) THEN
C     NON-GAUSSIAN CALC
C       THE REF OBJECT HEIGHTS ARE:
C       (AT THE OBJECT SURFACE)
              XHT=PXTRAX(5,NEWOBJ)
              YHT=PXTRAY(5,NEWOBJ)
C     SET REF AP HT
C
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              ELSE
C     TEL ON
                  SYS13=PXTRAX(1,NEWOBJ+1)
                  SYS12=PXTRAY(1,NEWOBJ+1)
              END IF
          END IF
C
C     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
C     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
C     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
C     AT THE IMAGE.
C     DO AN FOB 0
          SAVE_KDP(13)=SAVEINPT(13)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.TRUE.
          LDIF=.TRUE.
          WQ='        '
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
          W1=MYW1
          W2=MYW2
          W3=MYW3
          W4=MYW4
C     SET MSG TO FALSE
          MSG=.FALSE.
          WC='FOB     '
          CALL FFOB
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          WQ='        '
          SQ=0
          W3=MYW4
          SST=0
          DF1=1
          DF2=1
          DF3=0
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=1
          S4=0
          S5=0
          SN=1
          WC='RAY     '
          NOCOAT=.TRUE.
          GRASET=.FALSE.
          DXFSET=.FALSE.
          CALL RRAY
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
              ERROR=1
              RETURN
          END IF
C
C     PY
          VAL1=(DIFF(8,K)-RAYRAY(2,K))/DELY
          VAL2=(DIFF(7,K)-RAYRAY(1,K))/DELY
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PY=SIG*DSQRT((VAL1**2)+(VAL2**2))
          PY=PY*SYS12
C
C     PX
          VAL1=(DIFF(1,K)-RAYRAY(1,K))/DELX
          VAL2=(DIFF(2,K)-RAYRAY(2,K))/DELX
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PX=SIG*DSQRT((VAL1**2)+(VAL2**2))
          PX=PX*SYS13
C
C     PUY
          IF(DABS(DIFF(11,K)).GE.
     1    (1.0D35*DABS(DIFF(12,K)))) THEN
              IF(SNGL(DIFF(11,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(11,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(11,K),DIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-RAYRAY(12,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/DELY)*SYS12
          IF(DABS(DIFF(10,K)).GE.
     1    (1.0D35*DABS(DIFF(12,K)))) THEN
              IF(SNGL(DIFF(10,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(10,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(10,K),DIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-RAYRAY(11,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/DELY)*SYS12
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PUY=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
C
C     PUX
          IF(DABS(DIFF(4,K)).GE.
     1    (1.0D35*DABS(DIFF(6,K)))) THEN
              IF(SNGL(DIFF(4,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(4,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(4,K),DIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-RAYRAY(11,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/DELX)*SYS13
          IF(DABS(DIFF(5,K)).GE.
     1    (1.0D35*DABS(DIFF(6,K)))) THEN
              IF(SNGL(DIFF(5,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(5,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(5,K),DIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-RAYRAY(12,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/DELX)*SYS13
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PUX=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
C
C     PCY
          VAL1=(RFDIFF(8,K)-REFRY(2,K))/RFDELY
          VAL2=(RFDIFF(7,K)-REFRY(1,K))/RFDELY
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PCY=SIG*DSQRT((VAL1**2)+(VAL2**2))
          PCY=PCY*YHT
C
C     PCX
          VAL1=(RFDIFF(1,K)-REFRY(1,INT(K)))/RFDELX
          VAL2=(RFDIFF(2,K)-REFRY(2,K))/RFDELX
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PCX=SIG*DSQRT((VAL1**2)+(VAL2**2))
          PCX=PCX*XHT
C
C     PUCY
          IF(DABS(RFDIFF(11,K)).GE.
     1    (1.0D35*DABS(RFDIFF(12,K)))) THEN
              IF(SNGL(RFDIFF(11,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(11,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(11,K),RFDIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-REFRY(12,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/RFDELY)*YHT
          IF(DABS(RFDIFF(10,K)).GE.
     1    (1.0D35*DABS(RFDIFF(12,K)))) THEN
              IF(SNGL(RFDIFF(10,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(10,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(10,K),RFDIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-REFRY(11,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/RFDELY)*YHT
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PUCY=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
C
C     PUCX
          IF(DABS(RFDIFF(4,K)).GE.
     1    (1.0D35*DABS(RFDIFF(6,K)))) THEN
              IF(SNGL(RFDIFF(4,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(4,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(4,K),RFDIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-REFRY(11,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/RFDELX)*XHT
          IF(DABS(RFDIFF(5,K)).GE.
     1    (1.0D35*DABS(RFDIFF(6,K)))) THEN
              IF(SNGL(RFDIFF(5,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(5,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(5,K),RFDIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-REFRY(12,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/RFDELX)*XHT
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PUCX=SIG*(DSQRT((VAL1**2)+(VAL2**2)))

C     ALL THE PARAXIAL STUFF IS DONE
          IF(GFLAG.EQ.1) NEWREF=OLDREF
          RETURN
      END
C SUB GNPRTGEN.FOR
      SUBROUTINE GNPRTGEN(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1,DWORD1,DWORD2,DWORD3,DWORD4,GFLAG)
C
          IMPLICIT NONE
C
          REAL*8 V1,PX,PY,PUX,PUY,PCX,VAL1,VAL2,SIG,XHT,YHT
     1    ,SYS12,SYS13,PCY,PUCX,PUCY,DWORD1,DWORD2,DWORD3,DWORD4
C
C     DWORD1 IS Y FOB
C     DWORD2 IS X FOB
C     DWORD3 IS Z OBJ. SHIFT
C     DWORD4 IS WAVELENGTH #
C
          INTEGER ERROR,GFLAG
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          ERROR=0
          PX=0.0D0
          PY=0.0D0
          PUX=0.0D0
          PUY=0.0D0
          PCX=0.0D0
          PCY=0.0D0
          PUCX=0.0D0
          PUCY=0.0D0
C
          IF(GFLAG.EQ.1) THEN
C     GAUSSIAN CALC
C     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
C
              IF(NEWREF.NE.1) THEN
                  OLDREF=NEWREF
                  NEWREF=1
              END IF
              XHT=SYSTEM1(87)*0.001D0*ALENS(3,0)
              YHT=SYSTEM1(88)*0.001D0*ALENS(3,0)
              SYS13=SYSTEM1(85)
              SYS12=SYSTEM1(86)
          END IF
          IF(GFLAG.EQ.0) THEN
C     NON-GAUSSIAN CALC
C       THE REF OBJECT HEIGHTS ARE:
C       (AT THE OBJECT SURFACE)
              XHT=PXTRAX(5,NEWOBJ)
              YHT=PXTRAY(5,NEWOBJ)
C     SET REF AP HT
C
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              ELSE
C     TEL ON
                  SYS13=PXTRAX(1,NEWOBJ+1)
                  SYS12=PXTRAY(1,NEWOBJ+1)
              END IF
          END IF
C
C     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
C     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
C     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
C     AT THE IMAGE.
C     DO AN FOB 0
          SAVE_KDP(13)=SAVEINPT(13)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.TRUE.
          LDIF=.TRUE.
          WQ='        '
          SQ=0
          SST=0
          STI=0
          W1=DWORD1
          W2=DWORD2
          W3=DWORD3
          W4=DWORD4
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
C     SET MSG TO FALSE
          MSG=.FALSE.
          WC='FOB     '
          CALL FFOB
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          WQ='        '
          SQ=0
          SST=0
          W3=DWORD4
          DF1=1
          DF2=1
          DF3=0
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=1
          S4=0
          S5=0
          SN=1
          WC='RAY     '
          NOCOAT=.TRUE.
          GRASET=.FALSE.
          DXFSET=.FALSE.
          CALL RRAY
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
              ERROR=1
              RETURN
          END IF
C
C     PY
          VAL1=(DIFF(8,K)-RAYRAY(2,K))/DELY
          VAL2=(DIFF(7,K)-RAYRAY(1,K))/DELY
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PY=SIG*DSQRT((VAL1**2)+(VAL2**2))
          PY=PY*SYS12
C
C     PX
          VAL1=(DIFF(1,K)-RAYRAY(1,K))/DELX
          VAL2=(DIFF(2,K)-RAYRAY(2,K))/DELX
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PX=SIG*DSQRT((VAL1**2)+(VAL2**2))
          PX=PX*SYS13
C
C     PUY
          IF(DABS(DIFF(11,K)).GE.
     1    (1.0D35*DABS(DIFF(12,K)))) THEN
              IF(SNGL(DIFF(11,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(11,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(11,K),DIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-RAYRAY(12,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/DELY)*SYS12
          IF(DABS(DIFF(10,K)).GE.
     1    (1.0D35*DABS(DIFF(12,K)))) THEN
              IF(SNGL(DIFF(10,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(10,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(10,K),DIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-RAYRAY(11,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/DELY)*SYS12
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PUY=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
C
C     PUX
          IF(DABS(DIFF(4,K)).GE.
     1    (1.0D35*DABS(DIFF(6,K)))) THEN
              IF(SNGL(DIFF(4,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(4,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(4,K),DIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-RAYRAY(11,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/DELX)*SYS13
          IF(DABS(DIFF(5,K)).GE.
     1    (1.0D35*DABS(DIFF(6,K)))) THEN
              IF(SNGL(DIFF(5,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(5,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(5,K),DIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-RAYRAY(12,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/DELX)*SYS13
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PUX=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
C
C     PCY
          VAL1=(RFDIFF(8,K)-REFRY(2,K))/RFDELY
          VAL2=(RFDIFF(7,K)-REFRY(1,K))/RFDELY
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PCY=SIG*DSQRT((VAL1**2)+(VAL2**2))
          PCY=PCY*YHT
C
C     PCX
          VAL1=(RFDIFF(1,K)-REFRY(1,INT(K)))/RFDELX
          VAL2=(RFDIFF(2,K)-REFRY(2,K))/RFDELX
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PCX=SIG*DSQRT((VAL1**2)+(VAL2**2))
          PCX=PCX*XHT
C
C     PUCY
          IF(DABS(RFDIFF(11,K)).GE.
     1    (1.0D35*DABS(RFDIFF(12,K)))) THEN
              IF(SNGL(RFDIFF(11,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(11,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(11,K),RFDIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-REFRY(12,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/RFDELY)*YHT
          IF(DABS(RFDIFF(10,K)).GE.
     1    (1.0D35*DABS(RFDIFF(12,K)))) THEN
              IF(SNGL(RFDIFF(10,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(10,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(10,K),RFDIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-REFRY(11,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/RFDELY)*YHT
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PUCY=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
C
C     PUCX
          IF(DABS(RFDIFF(4,K)).GE.
     1    (1.0D35*DABS(RFDIFF(6,K)))) THEN
              IF(SNGL(RFDIFF(4,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(4,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(4,K),RFDIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-REFRY(11,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/RFDELX)*XHT
          IF(DABS(RFDIFF(5,K)).GE.
     1    (1.0D35*DABS(RFDIFF(6,K)))) THEN
              IF(SNGL(RFDIFF(5,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(5,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(5,K),RFDIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-REFRY(12,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/RFDELX)*XHT
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          PUCX=SIG*(DSQRT((VAL1**2)+(VAL2**2)))

C     ALL THE PARAXIAL STUFF IS DONE
          IF(GFLAG.EQ.1) NEWREF=OLDREF
          RETURN
      END
C SUB GNPR12345678.FOR
      SUBROUTINE GNPR12345678(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
C
C     PY
C
          IMPLICIT NONE
C
          REAL*8 V1,VAL1,VAL2,SIG,XHT,YHT
     1    ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
          REAL*8 VALUE1_AUXFOB(1:8)
          COMMON/FOBAUX/VALUE1_AUXFOB
C
          INTEGER ERROR,GFLAG
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          ERROR=0
C
          IF(GFLAG.EQ.1) THEN
C     GAUSSIAN CALC
C     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
C
              IF(NEWREF.NE.1) THEN
                  OLDREF=NEWREF
                  NEWREF=1
              END IF
              XHT=SYSTEM1(87)*0.001D0*ALENS(3,0)
              YHT=SYSTEM1(88)*0.001D0*ALENS(3,0)
              SYS13=SYSTEM1(85)
              SYS12=SYSTEM1(86)
          END IF
          IF(GFLAG.EQ.0) THEN
C     NON-GAUSSIAN CALC
C       THE REF OBJECT HEIGHTS ARE:
C       (AT THE OBJECT SURFACE)
              XHT=PXTRAX(5,NEWOBJ)
              YHT=PXTRAY(5,NEWOBJ)
C     SET REF AP HT
C
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  SYS13=PXTRAX(1,NEWREF)
                  SYS12=PXTRAY(1,NEWREF)
              ELSE
C     TEL ON
                  SYS13=PXTRAX(1,NEWOBJ+1)
                  SYS12=PXTRAY(1,NEWOBJ+1)
              END IF
          END IF
C
C     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
C     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
C     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
C     AT THE IMAGE.
C     DO AN FOB 0
          SAVE_KDP(13)=SAVEINPT(13)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.TRUE.
          LDIF=.TRUE.
          WQ='        '
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
          W1=NWN1
          W2=NWN2
          W3=NWN3
          W4=NWN4
C     SET MSG TO FALSE
          MSG=.FALSE.
          WC='FOB     '
          CALL FFOB
          REST_KDP(13)=RESTINPT(13)
          SAVE_KDP(13)=SAVEINPT(13)
          WQ='        '
          SQ=0
          SST=0
          STI=0
          DF1=1
          DF2=1
          DF3=0
          DF4=1
          DF5=1
          S1=0
          S2=0
          S3=1
          S4=0
          S5=0
          SN=1
          W3=NWN4
          WC='RAY     '
          NOCOAT=.TRUE.
          GRASET=.FALSE.
          DXFSET=.FALSE.
          CALL RRAY
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(13)=RESTINPT(13)
C
          IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
              ERROR=1
              RETURN
          END IF
C
C     PY
          VAL1=(DIFF(8,K)-RAYRAY(2,K))/DELY
          VAL2=(DIFF(7,K)-RAYRAY(1,K))/DELY
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1=SIG*DSQRT((VAL1**2)+(VAL2**2))
          VALUE1_AUXFOB(1)=VALUE1*SYS12
C
C     PX
          VAL1=(DIFF(1,K)-RAYRAY(1,K))/DELX
          VAL2=(DIFF(2,K)-RAYRAY(2,K))/DELX
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1=SIG*DSQRT((VAL1**2)+(VAL2**2))
          VALUE1_AUXFOB(2)=VALUE1*SYS13
C
C     PUY
          IF(DABS(DIFF(11,K)).GE.
     1    (1.0D35*DABS(DIFF(12,K)))) THEN
              IF(SNGL(DIFF(11,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(11,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(11,K),DIFF(12,K))
              END IF
C        IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-RAYRAY(12,K))
C      IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/DELY)*SYS12
          IF(DABS(DIFF(10,K)).GE.
     1    (1.0D35*DABS(DIFF(12,K)))) THEN
              IF(SNGL(DIFF(10,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(10,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(10,K),DIFF(12,K))
              END IF
C       IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-RAYRAY(11,K))
C     IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/DELY)*SYS12
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1_AUXFOB(3)=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
C
C     PUX
          IF(DABS(DIFF(4,K)).GE.
     1    (1.0D35*DABS(DIFF(6,K)))) THEN
              IF(SNGL(DIFF(4,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(4,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(4,K),DIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-RAYRAY(11,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/DELX)*SYS13
          IF(DABS(DIFF(5,K)).GE.
     1    (1.0D35*DABS(DIFF(6,K)))) THEN
              IF(SNGL(DIFF(5,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(DIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(DIFF(5,K)).LE.1.0D-15.AND.
     1        DABS(DIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(DIFF(5,K),DIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-RAYRAY(12,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/DELX)*SYS13
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1_AUXFOB(4)=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
C
C     PCY
          VAL1=(RFDIFF(8,K)-REFRY(2,K))/RFDELY
          VAL2=(RFDIFF(7,K)-REFRY(1,K))/RFDELY
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1=SIG*DSQRT((VAL1**2)+(VAL2**2))
          VALUE1_AUXFOB(5)=VALUE1*YHT
C
C     PCX
          VAL1=(RFDIFF(1,K)-REFRY(1,INT(K)))/RFDELX
          VAL2=(RFDIFF(2,K)-REFRY(2,K))/RFDELX
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1=SIG*DSQRT((VAL1**2)+(VAL2**2))
          VALUE1_AUXFOB(6)=VALUE1*XHT
C
C     PUCY
          IF(DABS(RFDIFF(11,K)).GE.
     1    (1.0D35*DABS(RFDIFF(12,K)))) THEN
              IF(SNGL(RFDIFF(11,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(11,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(11,K),RFDIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-REFRY(12,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/RFDELY)*YHT
          IF(DABS(RFDIFF(10,K)).GE.
     1    (1.0D35*DABS(RFDIFF(12,K)))) THEN
              IF(SNGL(RFDIFF(10,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(10,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(10,K),RFDIFF(12,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-REFRY(11,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/RFDELY)*YHT
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1_AUXFOB(7)=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
C
C     PUCX
          IF(DABS(RFDIFF(4,K)).GE.
     1    (1.0D35*DABS(RFDIFF(6,K)))) THEN
              IF(SNGL(RFDIFF(4,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(4,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(4,K),RFDIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL1=(V1-REFRY(11,K))
          IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
          IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
          VAL1=(VAL1/RFDELX)*XHT
          IF(DABS(RFDIFF(5,K)).GE.
     1    (1.0D35*DABS(RFDIFF(6,K)))) THEN
              IF(SNGL(RFDIFF(5,K)).GE.0.0) V1=PII/2.0D0
              IF(SNGL(RFDIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(RFDIFF(5,K)).LE.1.0D-15.AND.
     1        DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
                  V1=0.0D0
              ELSE
                  V1=DATAN2(RFDIFF(5,K),RFDIFF(6,K))
              END IF
              IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
          END IF
          VAL2=(V1-REFRY(12,K))
          IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
          IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
          VAL2=(VAL2/RFDELX)*XHT
          IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
          IF(VAL1.EQ.0.0D0) SIG=1.0D0
          IF(ISNAN(VAL1))VAL1=0.0D0
          IF(ISNAN(VAL2))VAL2=0.0D0
          VALUE1_AUXFOB(8)=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
          IF(GFLAG.EQ.1) NEWREF=OLDREF
          RETURN
      END
C SUB DISTOR.FOR
      SUBROUTINE DISTOR(DWORD1,DWORD2,ERROR)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE DISTOR. THIS SUBROUTINE IMPLEMENTS
C       THE GET DIST COMMAND
C
C     THE GET DIST COMMAND RESTURNS IN VALUE1, THE VALUE1 OF THE
C     DISTORTION IN PERCENT AT THE FOB POSITION GIVEN BY:
C          YFOB=DWORD1
C          XFOB=DWORD2
C
          REAL*8 PCY,PCX,PUCY,PUCX
     1    ,XREAL,YREAL,PY,PX,PUY,PUX,HR,HP,DWORD1,DWORD2
     2    ,MYW1,MYW2,MYW3,MYW4,HPX,HPY,HRX,HRY,VALUE1X,VALUE1Y
          REAL*8 O18,O19
          COMMON/O18O19/O18,O19
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER K,ERROR
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          O18=SYSTEM1(18)
          O19=SYSTEM1(19)
          SYSTEM1(18)=0.0D0
          SYSTEM1(19)=0.0D0
C
          ERROR=0
          K=NEWIMG
C
          IF(DWORD1.EQ.0.0D0) DWORD1=1.0D-10
          IF(DWORD2.EQ.0.0D0) DWORD2=1.0D-10
          MYW1=DWORD1
          MYW2=DWORD2
          MYW3=0.0D0
          MYW4=SYSTEM1(11)
          CALL GNPRT(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1    ,0.0D0,0.0D0,0.0D0,MYW4,0)

          IF(ERROR.EQ.1) THEN
              SYSTEM1(18)=O18
              SYSTEM1(19)=O19
              RETURN
          END IF
C     PROCEED WITH DISTORTION CALCULATION USING PCY AND PCX
C     TRACE THE DESIRED GUT RAY WITHOUT DIFFERENTIAL TRACING
C     AND GET ITS X AND Y POSITIONS AT THE IMAGE PLANE.
          SAVE_KDP(15)=SAVEINPT(15)
          OLDLDIF2=LDIF2
          OLDLDIF=LDIF
          LDIF2=.FALSE.
          LDIF=.FALSE.
          WQ='        '
          SQ=0
          SST=0
          STI=0
          W1=MYW1
          W2=MYW2
          W3=MYW3
          W4=MYW4
          W5=0.0D0
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
C     SET MSG TO FALSE
          MSG=.FALSE.
          ERROR=0
          WC='FOB'
          CALL FFOB
          LDIF2=OLDLDIF2
          LDIF=OLDLDIF
          REST_KDP(1)=RESTINPT(1)
          IF(ERROR.EQ.1) THEN
              SYSTEM1(18)=O18
              SYSTEM1(19)=O19
              RETURN
          END IF
C     PROCEED WITH DIST CALC.
          IF(SYSTEM1(30).LE.2.0D0) THEN
C     MODE FOCAL
C     PARAX IMAGE POSITION IS JUST
              HPX=DSQRT((PCX*MYW2)**2)
              HPY=DSQRT((MYW1*PCY)**2)
              HP=DSQRT(((MYW1*PCY)**2)+((PCX*MYW2)**2))
C     REAL IMAGE POSITION IS JUST
              XREAL=GUTRAY(1,NEWIMG)-REFRY(1,NEWIMG)
              YREAL=GUTRAY(2,NEWIMG)-REFRY(2,NEWIMG)
              HRX=DSQRT((XREAL)**2)
              HRY=DSQRT((YREAL)**2)
              HR=DSQRT(((YREAL)**2)+((XREAL)**2))
              IF(HPX.EQ.0.0D0) VALUE1X=0.0D0
              IF(HPX.NE.0.0D0) VALUE1X=((HRX-HPX)/HPX)*100.0D0
              IF(HPY.EQ.0.0D0) VALUE1Y=0.0D0
              IF(HPY.NE.0.0D0) VALUE1Y=((HRY-HPY)/HPY)*100.0D0
              IF(HP.EQ.0.0D0) VALUE1=0.0D0
              IF(HP.NE.0.0D0) VALUE1=((HR-HP)/HP)*100.0D0
          END IF
          IF(SYSTEM1(30).GE.3.0D0) THEN
C     MODE AFOCAL
C     PARAX IMAGE SLOPE IS JUST
              HP=DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2))
              HPX=((PUCX*MYW2)**2)
              HPY=((PUCX*MYW2)**2)
C     REAL IMAGE SLOPE IS JUST
              XREAL=DTAN(GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG))
              YREAL=DTAN(GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG))
              HRX=DSQRT((XREAL)**2)
              HRY=DSQRT((YREAL)**2)
              HR=DSQRT(((YREAL)**2)+((XREAL)**2))
              IF(HPX.NE.0.0D0) VALUE1X=((HRX-HPX)/HPX)*100.0D0
              IF(HPX.EQ.0.0D0) VALUE1X=0.0D0
              IF(HPY.NE.0.0D0) VALUE1Y=((HRY-HPY)/HPY)*100.0D0
              IF(HPY.EQ.0.0D0) VALUE1Y=0.0D0
              IF(HP.NE.0.0D0) VALUE1=((HR-HP)/HP)*100.0D0
              IF(HP.EQ.0.0D0) VALUE1=0.0D0
          END IF
          SYSTEM1(18)=O18
          SYSTEM1(19)=O19
          RETURN
      END
C SUB CACHEK.FOR

      SUBROUTINE CACHEK(JK1,JK2,JK3,CACOCA)
C
          IMPLICIT NONE
C
          EXTERNAL INSID1,INSID2
C
C       THIS IS SUBROUTINE CACHEK.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CLEAR APERTURE CHECKING AS CALLED BY RAYTRA.FOR
C       AND REFRAY.FOR
C
          INTEGER I,CAFLG,COFLG,N,III,CACOCA
C
          REAL*8 X,Y,Z,ANGLE,JK1,JK2,JK3,
     1    XR,YR,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,
     2    X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,
     3    YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4,A15,A22
C
          LOGICAL INS,INSID1,INSID2
C
          LOGICAL NOCOBSPSF
          COMMON/PSFCOBS/NOCOBSPSF
C
          INTEGER CAERAS,COERAS
C
          COMMON/CACO/CAERAS,COERAS,LS
C
C     VARIABLES FOR SPOT TRACING
C
          LOGICAL SPDTRA
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          COMMON/SPRA1/SPDTRA
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       THE CALL TO THIS ROUTINE IS:
C       CALL CACHEK
          I=R_I
          X=R_X
          Y=R_Y
          Z=R_Z
          LS=0
          RAYCOD(1)=0
          RAYCOD(2)=I
C       ADDED TO SUPPORT VIIRS FOOTPRINTS ON 10/28/2004
          IF(ALENS(58,I).EQ.1.0D0) THEN
C       FOOTBLOCK WAS ON FOR THIS SURFACE SO SKIP THE CLAP/COBS CHECK
              STOPP=0
              RETURN
          END IF
C
C       I IS THE CURRENT SURFACE NUMBER
C
C       THIS ROUTINE SETS THE FLAGS
C       CAERAS AND COERAS ARE
C       FLAGS SET IF THE NEXT CURRENT SURFACE HAS A COBS OR
C       CLAP ERASE.
C
          IF(ALENS(9,I).EQ.0.0D0.AND.ALENS(16,I).EQ.0.0D0) THEN
C
C       NO CLAPS OR COBS, JUST RETURN
              STOPP=0
              RETURN
C       THERE ARE CLAPS AND COBS, PROCEED CHECKING
          END IF
C
C       DO CAERAS,COERAS RESOLUTIONS
C
          IF(ALENS(51,I).GT.0.0D0) THEN
C       CLAP ERASE EXISTS
              CAERAS=INT(DABS(ALENS(51,I)))
          ELSE
              CAERAS=0
          END IF
          IF(ALENS(61,I).GT.0.0D0) THEN
C       COBS ERASE EXISTS
              COERAS=INT(DABS(ALENS(61,I)))
          ELSE
              COERAS=0
          END IF
C
C       NOW ALL THE CAERAS AND COERAS HAVE BEEN SET
C
C       SET FLAGS CAFLG AND COFLG
          IF(ALENS(9,I).NE.0.0D0.AND.CACOCA.EQ.0.OR.
     1    ALENS(9,I).NE.0.0D0.AND.CACOCA.EQ.1) THEN
C       CLAP EXISTS
              CAFLG=INT(DABS(ALENS(9,I)))
          ELSE
              CAFLG=0
          END IF
          IF(ALENS(16,I).NE.0.0D0.AND.CACOCA.EQ.0.OR.
     1    ALENS(16,I).NE.0.0D0.AND.CACOCA.EQ.2) THEN
C       COBS EXISTS
              COFLG=INT(DABS(ALENS(16,I)))
          ELSE
              COFLG=0
          END IF
C
          IF(NOCOBSPSF) COFLG=0

C
C       NOW ALL THE CAFLG AND COFLG HAVE BEEN SET
C
C******************************************************************
C       CAFLG NON-ZERO, RESOLVE ALL CLAP BLOCKAGES NOW
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
C       RESOLVE CLAP ERASE
              IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL CAERRS
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY CLAP ON SURFACE I
                  RAYCOD(1)=6
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
                  IF(MSG) THEN
                      IF(ALENS(9,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY CIRCULAR CLEAR APERTURE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  STOPP=1
                  RETURN
C       RAY NOT BLOCKED BY CIRCULAR CLAP, CONTINUE
              ELSE
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
C       RESOLVE CLAP ERASE
              IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL CAERRS
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY CLAP ON SURFACE I
C
                  RAYCOD(1)=6
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
                  IF(MSG) THEN
                      IF(ALENS(9,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY RECTANGULAR CLEAR APERTURE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
C
                  STOPP=1
                  RETURN
C       RAY NOT BLOCKED BY RECTANGULAR CLAP, CONTINUE
              ELSE
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
C       RESOLVE CLAP ERASE
              IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL CAERRS
C
              IF(LS.EQ.10.0D0) THEN
                  RAYCOD(1)=6
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
C       RAY IS BLOCKED BY CLAP ON SURFACE I
C
                  IF(MSG) THEN
                      IF(ALENS(9,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY ELLIPTICAL CLEAR APERTURE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
C
                  STOPP=1
                  RETURN
C       RAY NOT BLOCKED BY ELLIPTICAL CLAP, CONTINUE
              ELSE
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
C       RESOLVE CLAP ERASE
              IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL CAERRS
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY CLAP ON SURFACE I
                  RAYCOD(1)=6
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
C
                  IF(MSG) THEN
                      IF(ALENS(9,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY RACETRACK CLEAR APERTURE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  LS=0.0D0
                  STOPP=1
                  RETURN
              ELSE
C       RAY NOT BLOCKED BY RACETRACK CLAP, CONTINUE
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
C       RESOLVE CLAP ERASE
              IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL CAERRS
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY CLAP ON SURFACE I
                  RAYCOD(1)=6
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
C
                  IF(MSG) THEN
                      IF(ALENS(9,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY POLYGON CLEAR APERTURE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  LS=0.0D0
                  STOPP=1
                  RETURN
              ELSE
              END IF
C       CAFLG NOT 5
          END IF
C
C       CAFLG=6 IPOLY CLAP, DOES IT STOP THE RAY
          IF(CAFLG.EQ.6) THEN
              LS=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(11,I), CENTER TO CORNER DISTANCE
C       IS ALENS(10,II). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
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
C       RESOLVE CLAP ERASE
              IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL CAERRS
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY CLAP ON SURFACE I
                  RAYCOD(1)=6
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
C
                  IF(MSG) THEN
                      IF(ALENS(9,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY IRREGULAR POLYGON CLEAR APERTURE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  LS=0.0D0
                  STOPP=1
                  RETURN
              ELSE
              END IF
C       CAFLG NOT 6
          END IF
C
C******************************************************************
C       COFLG NON-ZERO, RESOLVE ALL COBS BLOCKAGES NOW
C******************************************************************
C
C       COFLG=1 CIRCULAR COBS, DOES IT STOP THE RAY
          IF(COFLG.EQ.1) THEN
C
              LS=0.0D0
C       CIRCULAR COBS EQUATION IS:
C
C       (X)**2 + (Y)**2 = R**2
C
C       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
C       LESS THAN OR EQUAL TO THE RIGHT SIDE, THE RAY IS BLOCKED
C       ELSE IT IS NOT BLOCKED.
C
C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
              XR=X-ALENS(20,I)-JK1
              YR=Y-ALENS(19,I)-JK2
C
              LS=DSQRT((XR**2)+(YR**2))
C
              RS=DSQRT(ALENS(17,I)**2)-AIMTOL
              IF(REAL(LS).LT.REAL(RS)) THEN
C       RAY IS BLOCKED BY COBS ON SURFACE I
                  LS=10.0D0
              ELSE
                  LS=0.0D0
              END IF
C
C       RESOLVE COBS ERASE
              IF(COERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL COERRS
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY COBS ON SURFACE I
                  RAYCOD(1)=7
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
C
                  IF(MSG) THEN
                      IF(ALENS(16,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY CIRCULAR OBSCURATION'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  STOPP=1
                  RETURN
              ELSE
C       RAY NOT BLOCKED BY CIRCULAR OBSCURATION, CONTINUE
              END IF
C       COFLG NOT 1
          END IF
C
C       COFLG=2 RECTANGULAR OBSCURATION, DOES IT STOP THE RAY
          IF(COFLG.EQ.2) THEN
C
              LS=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
C       COORDINATES ARE:
              X1=-ALENS(18,I)-AIMTOL
              Y1=ALENS(17,I)+AIMTOL
              X2=-ALENS(18,I)-AIMTOL
              Y2=-ALENS(17,I)-AIMTOL
              X3=ALENS(18,I)+AIMTOL
              Y3=-ALENS(17,I)-AIMTOL
              X4=ALENS(18,I)+AIMTOL
              Y4=ALENS(17,I)+AIMTOL
              XRD=X
              YRD=Y
C
C
C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
              XRD=XRD-ALENS(20,I)-JK1
              YRD=YRD-ALENS(19,I)-JK2
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

              A22=(ALENS(22,I)+JK3)*PII/180.0D0
              XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
              YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
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
              INS=INSID2()
              IF(INS) THEN
C       RAY  BLOCKED
                  LS=10.0D0
              ELSE
                  LS=0.0D0
C       RAY NOT BLOCKED
              END IF
C
C       RESOLVE COBS ERASE
              IF(COERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL COERRS
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY COBS ON SURFACE I
                  RAYCOD(1)=7
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
C
                  IF(MSG) THEN
                      IF(ALENS(16,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY RECTANGULAR OBSCURATION'
                          CALL SHOWIT(1)
                      END IF
                  END IF
C
                  STOPP=1
                  RETURN
              ELSE
C       RAY NOT BLOCKED BY RECTANGULAR OBSCURATION, CONTINUE
              END IF
C       COFLG NOT 2
          END IF
C
C       COFLG=3 ELLIPTICAL COBS, DOES IT STOP THE RAY
          IF(COFLG.EQ.3) THEN
C
              LS=0.0D0
C       ELLIPTICAL COBS EQUATION IS:
C
C       (X)**2/A**2  + (Y)**2/B**2 = 1
C
C       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
C       LESS THAN 1.0D0, THE RAY IS BLOCKED
C       ELSE IT IS NOT BLOCKED.
C
              XRD=X
              YRD=Y
C
C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
              XRD=XRD-ALENS(20,I)-JK1
              YRD=YRD-ALENS(19,I)-JK2
C
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

              A22=(ALENS(22,I)+JK3)*PII/180.0D0
              XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
              YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
              LS=((XR**2)/(ALENS(18,I)**2))+
     1           ((YR**2)/(ALENS(17,I)**2))
C
              IF(REAL(LS).LT.(1.0-(AIMTOL**2))) THEN
C       RAY IS BLOCKED BY COBS ON SURFACE I
                  LS=10.0D0
              ELSE
                  LS=0.0D0
              END IF
C
C       RESOLVE COBS ERASE
              IF(COERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL COERRS
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY COBS ON SURFACE I
                  RAYCOD(1)=7
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
C
                  IF(MSG) THEN
                      IF(ALENS(16,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY ELLIPTICAL OBSCURATION'
                          CALL SHOWIT(1)
                      END IF
                  END IF
C
                  STOPP=1
                  RETURN
              ELSE
C       RAY NOT BLOCKED BY ELLIPTICAL COBS, CONTINUE
              END IF
C       COFLG NOT 3
          END IF
C
C
C       COFLG=4 RACETRACK COBS, DOES IT STOP THE RAY
          IF(COFLG.EQ.4) THEN
C
              LS=0.0D0
C
C       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
C       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
C       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
C       AND THE FOUR CIRCLES NEED TO BE CHECKED.
C
              IF(ALENS(17,I).LE.ALENS(18,I)) THEN
C       ALENS(18,I) = MAXSID
                  MAXSID=ALENS(18,I)
              ELSE
                  MAXSID=ALENS(17,I)
              END IF
              IF(ALENS(21,I).LT.MAXSID) THEN
C       SETUP THE 8 SIDED BOX
                  N=8
                  X1=-ALENS(18,I)+ALENS(21,I)+AIMTOL
                  Y1=ALENS(17,I)-AIMTOL
                  X2=-ALENS(18,I)+AIMTOL
                  Y2=ALENS(17,I)-ALENS(21,I)-AIMTOL
                  X3=-ALENS(18,I)+AIMTOL
                  Y3=-ALENS(17,I)+ALENS(21,I)+AIMTOL
                  X4=-ALENS(18,I)+ALENS(21,I)+AIMTOL
                  Y4=-ALENS(17,I)+AIMTOL
                  X5=ALENS(18,I)-ALENS(21,I)-AIMTOL
                  Y5=-ALENS(17,I)+AIMTOL
                  X6=ALENS(18,I)-AIMTOL
                  Y6=-ALENS(17,I)+ALENS(21,I)+AIMTOL
                  X7=ALENS(18,I)-AIMTOL
                  Y7=ALENS(17,I)-ALENS(21,I)-AIMTOL
                  X8=ALENS(18,I)-ALENS(21,I)-AIMTOL
                  Y8=ALENS(17,I)-AIMTOL
              ELSE
C       SET UP THE FOUR SIDED BOX
                  N=4
                  X1=-ALENS(18,I)-AIMTOL
                  Y1=ALENS(17,I)+AIMTOL
                  X2=-ALENS(18,I)-AIMTOL
                  Y2=-ALENS(17,I)-AIMTOL
                  X3=ALENS(18,I)+AIMTOL
                  Y3=-ALENS(17,I)-AIMTOL
                  X4=ALENS(18,I)+AIMTOL
                  Y4=ALENS(17,I)+AIMTOL
              END IF
C
              XRD=X
              YRD=Y
C
C       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       OBSCURATION. REMEMBER.
C
              XRD=XRD-ALENS(20,I)-JK1
              YRD=YRD-ALENS(19,I)-JK2
C
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
C       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
C       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

              A22=(ALENS(22,I)+JK3)*PII/180.0D0
              XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
              YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
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
              INS=INSID2()
              IF(INS) THEN
C       RAY BLOCKED
                  LS=10.0D0
              ELSE
                  LS=0.0D0
C       RAY NOT BLOCKED
              END IF
C NOW IS THE POINT OUTSIDE OR ON ANY OF THE FOUR CIRCLES
C       CENTER OF THE FIRST CIRCLE IS AT
              XC1=-ALENS(18,I)+ALENS(21,I)
              YC1= ALENS(17,I)-ALENS(21,I)
              CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
C       CENTER OF THE SECOND CIRCLE IS AT
              XC2= -ALENS(18,I)+ALENS(21,I)
              YC2= -ALENS(17,I)+ALENS(21,I)
              CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
C       CENTER OF THE THIRD CIRCLE IS AT
              XC3= ALENS(18,I)-ALENS(21,I)
              YC3=-ALENS(17,I)+ALENS(21,I)
              CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
C       CENTER OF THE FIRST CIRCLE IS AT
              XC4=ALENS(18,I)-ALENS(21,I)
              YC4=ALENS(17,I)-ALENS(21,I)
              CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
C
              RAD2=DSQRT(ALENS(21,I)**2)-AIMTOL
C
C     RAY BLOCKED BY THE BOX AND CIRCLES
              IF(INS.OR.
     1        REAL(CS1).LT.REAL(RAD2).OR.REAL(CS2).LT.REAL(RAD2)
     1        .OR.REAL(CS3).LT.REAL(RAD2).OR.REAL(CS4).LT.REAL(RAD2))
     1                          THEN
C       RAY IS BLOCKED BY ONE OF THE CIRCLES
                  LS=10.0D0
              ELSE
                  LS=0.0D0
              END IF
C
C       RESOLVE COBS ERASE
              IF(COERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL COERRS
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY COBS ON SURFACE I
                  RAYCOD(1)=7
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
C
                  IF(MSG) THEN
                      IF(ALENS(16,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY RACETRACK OBSCURATION'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  LS=0.0D0
C
                  STOPP=1
                  RETURN
              ELSE
C       RAY NOT BLOCKED BY RACETRACK COBS, CONTINUE
              END IF
C       COFLG NOT 4
          END IF
C
C       COFLG=5 POLY CLAP, DOES IT STOP THE RAY
          IF(COFLG.EQ.5) THEN
              LS=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(18,I), CENTER TO CORNER DISTANCE
C       IS ALENS(17,II). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
              ANGLE=0.0D0
              DO III=1,INT(ALENS(18,I))
                  XT(III)=ALENS(17,I)*DCOS(ANGLE+(PII/2.0D0))
                  YT(III)=ALENS(17,I)*DSIN(ANGLE+(PII/2.0D0))
                  ANGLE=ANGLE+((TWOPII)/ALENS(18,I))
              END DO
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
              XRD=XRD-ALENS(20,I)-JK1
              YRD=YRD-ALENS(19,I)-JK2
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED

              A22=(ALENS(22,I)+JK3)*PII/180.0D0
              XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
              YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(18,I))
              INS=INSID2()
              IF(INS) THEN
C       RAY BLOCKED
                  LS=10.0D0
              ELSE
C       RAY NOT BLOCKED
                  LS=0.0D0
              END IF
C
C       RESOLVE COBS ERASE
              IF(COERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL COERRS
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY COBS ON SURFACE I
                  RAYCOD(1)=7
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
C
                  IF(MSG) THEN
                      IF(ALENS(16,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY POLYGON OBSCURATION'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  LS=0.0D0
                  STOPP=1
                  RETURN
              END IF
          ELSE
C       COFLG NOT 5
          END IF
C
C       COFLG=6 IPOLY CLAP, DOES IT STOP THE RAY
          IF(COFLG.EQ.6) THEN
              LS=0.0D0
C
C       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
C       NUMBER OF POINTS IS ALENS(18,I), CENTER TO CORNER DISTANCE
C       IS ALENS(17,II). POINTS GO COUNTER CLOCKWISE LOOKING
C       TOWARD THE +Z DIRECTION
C       COORDINATES ARE:
              DO III=1,INT(ALENS(18,I))
                  XT(III)=IPOLYX(III,I,3)
                  YT(III)=IPOLYY(III,I,3)
              END DO
              XRD=X
              YRD=Y
C
C       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
C       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
C       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
C       CLEAR APERTURE ERASE. REMEMBER.
C
              XRD=XRD-ALENS(20,I)-JK1
              YRD=YRD-ALENS(19,I)-JK2
C
C       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
C       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
C       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED

              A22=(ALENS(22,I)+JK3)*PII/180.0D0
              XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
              YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
C
C       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
              X0=XR
              Y0=YR
              NP=INT(ALENS(18,I))
              INS=INSID2()
              IF(INS) THEN
C       RAY BLOCKED
                  LS=10.0D0
              ELSE
C       RAY NOT BLOCKED
                  LS=0.0D0
              END IF
C
C       RESOLVE COBS ERASE
              IF(COERAS.NE.0.AND.LS.EQ.10.0D0)
     1        CALL COERRS
C
              IF(LS.EQ.10.0D0) THEN
C       RAY IS BLOCKED BY IRREGULAR POLYGON COBS ON SURFACE I
                  RAYCOD(1)=7
                  RAYCOD(2)=I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=I
C
                  IF(MSG) THEN
                      IF(ALENS(16,I).GT.0.0D0) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'RAY BLOCKED BY IRREGULAR POLYGON OBSCURATION'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  LS=0.0D0
                  STOPP=1
                  RETURN
              END IF
          ELSE
C       COFLG NOT 6
          END IF
          RETURN
      END
