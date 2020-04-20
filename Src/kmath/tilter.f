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

C SUB PIVDEC.FOR
      SUBROUTINE PIVDEC(I)
C
          IMPLICIT NONE
C
C
          REAL*8 A11,A12,A13,A21,A22,A23,A31,A32,A33,
     1    B11,B12,B13,B21,B22,B23,B31,B32,B33,JX,JY,JZ,LL1,MM1,NN1,
     1    C11,C12,C13,C21,C22,C23,C31,C32,C33,AP,BP,CP,
     1    JK_AA,AN,X1,Y1,Z1,TX,TY,TZ,TTX,TTY,TTZ,AEEA,BEEB,CEEC,
     1    LX,LY,LZ,MX,MY,MZ,NX,NY,NZ,XEEX,YEEY,ZEEZ,ALPHA,BETA,
     1    LX0,LY0,LZ0,MX0,MY0,MZ0,NX0,NY0,NZ0,JLX,JLY,JLZ,
     1    JLX1,JLY1,JLZ1,JMX1,JMY1,JMZ1,JNX1,JNY1,JNZ1
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C     SET AEEA,BEEB,CEEC
          AEEA=ALENS(26,I)
          BEEB=ALENS(27,I)
          CEEC=ALENS(28,I)
          XEEX=ALENS(114,I)
          YEEY=ALENS(115,I)
          ZEEZ=ALENS(116,I)
          LX0=1.0D0
          MX0=0.0D0
          NX0=0.0D0
          LY0=0.0D0
          MY0=1.0D0
          NY0=0.0D0
          LZ0=0.0D0
          MZ0=0.0D0
          NZ0=1.0D0
C     IF THE SURFACE HAS A TILT, RTILT OR TILT DAR AND IF THERE IS
C     AN ALTERNATE PIVOT POINT DEFINED AND IF PIVAXIS IS SET TO NORMAL
C     THEN COMPUTE A MODIFIED SET OF INTERNAL TILT VALUES
          IF(ALENS(25,I).EQ.1.0D0.AND.ALENS(59,I).EQ.1.0D0.AND.
     1    ALENS(113,I).EQ.1.0D0.OR.
     2    ALENS(25,I).EQ.-1.0D0.AND.ALENS(59,I).EQ.1.0D0.AND.
     3    ALENS(113,I).EQ.1.0D0.OR.
     2    ALENS(25,I).EQ.5.0D0.AND.ALENS(59,I).EQ.1.0D0.AND.
     3    ALENS(113,I).EQ.1.0D0) THEN
C     JX AND JY ARE THE X AND Y PIVOT POSITIONS
              JX=ALENS(78,I)
              JY=ALENS(79,I)
C     THE SURFACE NORMAL DIRECTION COSINES AT THIS JX AND JY ARE:
C     LL1,MM1,NN1
              CALL SAGINT(I,JX,JY,JZ,LL1,MM1,NN1)
C     THIS IS PERFORMED VIA AN ALPHA AND A BETA ROTATION ONLY
              CALL ANGLECALC(ALPHA,BETA,LL1,MM1,NN1)
C     NOW ROTATE THE AXES DIRECTION COSINES BY ALPHA AND BETA
C     AND THEN BY THE USER ASSIGNED ROTATION ANGLES
              LX=LX0
              MX=MX0
              NX=NX0
              LY=LY0
              MY=MY0
              NY=NY0
              LZ=LZ0
              MZ=MZ0
              NZ=NZ0
              IF(ALPHA.NE.0.0D0) THEN
                  JK_AA=ALPHA*(PII/180.0D0)
                  JLX1=(LX*A11(JK_AA))+(MX*A12(JK_AA))+(NX*A13(JK_AA))
                  JMX1=(LX*A21(JK_AA))+(MX*A22(JK_AA))+(NX*A23(JK_AA))
                  JNX1=(LX*A31(JK_AA))+(MX*A32(JK_AA))+(NX*A33(JK_AA))
                  JLY1=(LY*A11(JK_AA))+(MY*A12(JK_AA))+(NY*A13(JK_AA))
                  JMY1=(LY*A21(JK_AA))+(MY*A22(JK_AA))+(NY*A23(JK_AA))
                  JNY1=(LY*A31(JK_AA))+(MY*A32(JK_AA))+(NY*A33(JK_AA))
                  JLZ1=(LZ*A11(JK_AA))+(MZ*A12(JK_AA))+(NZ*A13(JK_AA))
                  JMZ1=(LZ*A21(JK_AA))+(MZ*A22(JK_AA))+(NZ*A23(JK_AA))
                  JNZ1=(LZ*A31(JK_AA))+(MZ*A32(JK_AA))+(NZ*A33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(BETA.NE.0.0D0) THEN
                  JK_AA=BETA*(PII/180.0D0)
                  JLX1=(LX*B11(JK_AA))+(MX*B12(JK_AA))+(NX*B13(JK_AA))
                  JMX1=(LX*B21(JK_AA))+(MX*B22(JK_AA))+(NX*B23(JK_AA))
                  JNX1=(LX*B31(JK_AA))+(MX*B32(JK_AA))+(NX*B33(JK_AA))
                  JLY1=(LY*B11(JK_AA))+(MY*B12(JK_AA))+(NY*B13(JK_AA))
                  JMY1=(LY*B21(JK_AA))+(MY*B22(JK_AA))+(NY*B23(JK_AA))
                  JNY1=(LY*B31(JK_AA))+(MY*B32(JK_AA))+(NY*B33(JK_AA))
                  JLZ1=(LZ*B11(JK_AA))+(MZ*B12(JK_AA))+(NZ*B13(JK_AA))
                  JMZ1=(LZ*B21(JK_AA))+(MZ*B22(JK_AA))+(NZ*B23(JK_AA))
                  JNZ1=(LZ*B31(JK_AA))+(MZ*B32(JK_AA))+(NZ*B33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(AEEA.NE.0.0D0) THEN
                  JK_AA=AEEA*(PII/180.0D0)
                  JLX1=(LX*A11(JK_AA))+(MX*A12(JK_AA))+(NX*A13(JK_AA))
                  JMX1=(LX*A21(JK_AA))+(MX*A22(JK_AA))+(NX*A23(JK_AA))
                  JNX1=(LX*A31(JK_AA))+(MX*A32(JK_AA))+(NX*A33(JK_AA))
                  JLY1=(LY*A11(JK_AA))+(MY*A12(JK_AA))+(NY*A13(JK_AA))
                  JMY1=(LY*A21(JK_AA))+(MY*A22(JK_AA))+(NY*A23(JK_AA))
                  JNY1=(LY*A31(JK_AA))+(MY*A32(JK_AA))+(NY*A33(JK_AA))
                  JLZ1=(LZ*A11(JK_AA))+(MZ*A12(JK_AA))+(NZ*A13(JK_AA))
                  JMZ1=(LZ*A21(JK_AA))+(MZ*A22(JK_AA))+(NZ*A23(JK_AA))
                  JNZ1=(LZ*A31(JK_AA))+(MZ*A32(JK_AA))+(NZ*A33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(BEEB.NE.0.0D0) THEN
                  JK_AA=BEEB*(PII/180.0D0)
                  JLX1=(LX*B11(JK_AA))+(MX*B12(JK_AA))+(NX*B13(JK_AA))
                  JMX1=(LX*B21(JK_AA))+(MX*B22(JK_AA))+(NX*B23(JK_AA))
                  JNX1=(LX*B31(JK_AA))+(MX*B32(JK_AA))+(NX*B33(JK_AA))
                  JLY1=(LY*B11(JK_AA))+(MY*B12(JK_AA))+(NY*B13(JK_AA))
                  JMY1=(LY*B21(JK_AA))+(MY*B22(JK_AA))+(NY*B23(JK_AA))
                  JNY1=(LY*B31(JK_AA))+(MY*B32(JK_AA))+(NY*B33(JK_AA))
                  JLZ1=(LZ*B11(JK_AA))+(MZ*B12(JK_AA))+(NZ*B13(JK_AA))
                  JMZ1=(LZ*B21(JK_AA))+(MZ*B22(JK_AA))+(NZ*B23(JK_AA))
                  JNZ1=(LZ*B31(JK_AA))+(MZ*B32(JK_AA))+(NZ*B33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(CEEC.NE.0.0D0) THEN
                  JK_AA=CEEC*(PII/180.0D0)
                  JLX1=(LX*C11(JK_AA))+(MX*C12(JK_AA))+(NX*C13(JK_AA))
                  JMX1=(LX*C21(JK_AA))+(MX*C22(JK_AA))+(NX*C23(JK_AA))
                  JNX1=(LX*C31(JK_AA))+(MX*C32(JK_AA))+(NX*C33(JK_AA))
                  JLY1=(LY*C11(JK_AA))+(MY*C12(JK_AA))+(NY*C13(JK_AA))
                  JMY1=(LY*C21(JK_AA))+(MY*C22(JK_AA))+(NY*C23(JK_AA))
                  JNY1=(LY*C31(JK_AA))+(MY*C32(JK_AA))+(NY*C33(JK_AA))
                  JLZ1=(LZ*C11(JK_AA))+(MZ*C12(JK_AA))+(NZ*C13(JK_AA))
                  JMZ1=(LZ*C21(JK_AA))+(MZ*C22(JK_AA))+(NZ*C23(JK_AA))
                  JNZ1=(LZ*C31(JK_AA))+(MZ*C32(JK_AA))+(NZ*C33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(BETA.NE.0.0D0) THEN
                  JK_AA=-BETA*(PII/180.0D0)
                  JLX1=(LX*B11(JK_AA))+(MX*B12(JK_AA))+(NX*B13(JK_AA))
                  JMX1=(LX*B21(JK_AA))+(MX*B22(JK_AA))+(NX*B23(JK_AA))
                  JNX1=(LX*B31(JK_AA))+(MX*B32(JK_AA))+(NX*B33(JK_AA))
                  JLY1=(LY*B11(JK_AA))+(MY*B12(JK_AA))+(NY*B13(JK_AA))
                  JMY1=(LY*B21(JK_AA))+(MY*B22(JK_AA))+(NY*B23(JK_AA))
                  JNY1=(LY*B31(JK_AA))+(MY*B32(JK_AA))+(NY*B33(JK_AA))
                  JLZ1=(LZ*B11(JK_AA))+(MZ*B12(JK_AA))+(NZ*B13(JK_AA))
                  JMZ1=(LZ*B21(JK_AA))+(MZ*B22(JK_AA))+(NZ*B23(JK_AA))
                  JNZ1=(LZ*B31(JK_AA))+(MZ*B32(JK_AA))+(NZ*B33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(ALPHA.NE.0.0D0) THEN
                  JK_AA=-ALPHA*(PII/180.0D0)
                  JLX1=(LX*A11(JK_AA))+(MX*A12(JK_AA))+(NX*A13(JK_AA))
                  JMX1=(LX*A21(JK_AA))+(MX*A22(JK_AA))+(NX*A23(JK_AA))
                  JNX1=(LX*A31(JK_AA))+(MX*A32(JK_AA))+(NX*A33(JK_AA))
                  JLY1=(LY*A11(JK_AA))+(MY*A12(JK_AA))+(NY*A13(JK_AA))
                  JMY1=(LY*A21(JK_AA))+(MY*A22(JK_AA))+(NY*A23(JK_AA))
                  JNY1=(LY*A31(JK_AA))+(MY*A32(JK_AA))+(NY*A33(JK_AA))
                  JLZ1=(LZ*A11(JK_AA))+(MZ*A12(JK_AA))+(NZ*A13(JK_AA))
                  JMZ1=(LZ*A21(JK_AA))+(MZ*A22(JK_AA))+(NZ*A23(JK_AA))
                  JNZ1=(LZ*A31(JK_AA))+(MZ*A32(JK_AA))+(NZ*A33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
C     THE NEW AEEA,BEEB AND CEEC ANGLES ARE THE ONES THAT GO FROM
C     THE UNROTATED COORDINATE SYSTEM TO THESE NEW DIRECTION COSINES
              CALL NEWANGLES(AEEA,BEEB,CEEC,LX,MX,NX,LY,MY,NY,LZ,MZ,NZ)
C       DONE WITH AEEA,BEEM,CEEC,XEEX,YEEY AND ZEEZ ADJUSTMENTS
              ALENS(26,I)=AEEA
              ALENS(27,I)=BEEB
              ALENS(28,I)=CEEC
C     NOW CALCULATE THE CORRECTED XEEX,YEEY AND ZEEZ
              JLX=XEEX
              JLY=YEEY
              JLZ=ZEEZ
              IF(BETA.NE.0.0D0) THEN
                  JK_AA=-BETA*(PII/180.0D0)
                  XEEX=(JLX*B11(JK_AA))+(JLY*B12(JK_AA))+(JLZ*B13(JK_AA))
                  YEEY=(JLX*B21(JK_AA))+(JLY*B22(JK_AA))+(JLZ*B23(JK_AA))
                  ZEEZ=(JLX*B31(JK_AA))+(JLY*B32(JK_AA))+(JLZ*B33(JK_AA))
                  JLX=XEEX
                  JLY=YEEY
                  JLZ=ZEEZ
              END IF
              IF(ALPHA.NE.0.0D0) THEN
                  JK_AA=-ALPHA*(PII/180.0D0)
                  XEEX=(JLX*A11(JK_AA))+(JLY*A12(JK_AA))+(JLZ*A13(JK_AA))
                  YEEY=(JLX*A21(JK_AA))+(JLY*A22(JK_AA))+(JLZ*A23(JK_AA))
                  ZEEZ=(JLX*A31(JK_AA))+(JLY*A32(JK_AA))+(JLZ*A33(JK_AA))
              END IF
          END IF
C     NOW AEEA,BEEB AND CEEC ARE DONE
C     IF A PIVOT POINT IS DEFINED, REMEMBER THAT THE EXTRA DECENTER AMOUNTS
C     WILL BE ADDED BACK IN LATER AS NEEDED.
          ALENS(117,I)=0.0D0
          IF(ALENS(114,I).NE.0.0D0.OR.ALENS(115,I).NE.0.0D0.OR.
     1    ALENS(116,I).NE.0.0D0) ALENS(117,I)=1.0D0
          ALENS(30,I)=0.0D0
          ALENS(31,I)=0.0D0
          ALENS(69,I)=0.0D0
C     TX,TY AND TZ IS THE PIVOT POINT LOCATION
          TX=ALENS(78,I)
          TY=ALENS(79,I)
          TZ=ALENS(80,I)
          TTX=TX
          TTY=TY
          TTZ=TZ
          AP=AEEA
          BP=BEEB
          CP=CEEC
C     NOT COMPUTE THE AMMOUNT OF DECENTRATION OF THE SURFACE VERTEX
C     WHICH RESULTS FROM THE SPECIFIED ROTATION ABOUT THE SPECIFIED
C     PIVOT POINT.
C
          IF(ALENS(25,I).EQ.-1.0D0.OR.ALENS(25,I).EQ.1.0D0.OR.
     1    ALENS(25,I).EQ.5.0D0) THEN
C     WE HAVE A TILT OR A TILT DAR WITH A PIVOT

              JK_AA=-AP*(PII/180.0D0)
              X1=(TX*A11(JK_AA))+(TY*A12(JK_AA))+(TZ*A13(JK_AA))
              Y1=(TX*A21(JK_AA))+(TY*A22(JK_AA))+(TZ*A23(JK_AA))
              Z1=(TX*A31(JK_AA))+(TY*A32(JK_AA))+(TZ*A33(JK_AA))
              TX=X1
              TY=Y1
              TZ=Z1
              JK_AA=-BP*(PII/180.0D0)
              X1=(TX*B11(JK_AA))+(TY*B12(JK_AA))+(TZ*B13(JK_AA))
              Y1=(TX*B21(JK_AA))+(TY*B22(JK_AA))+(TZ*B23(JK_AA))
              Z1=(TX*B31(JK_AA))+(TY*B32(JK_AA))+(TZ*B33(JK_AA))
              TX=X1
              TY=Y1
              TZ=Z1
              JK_AA=-CP*(PII/180.0D0)
              X1=(TX*C11(JK_AA))+(TY*C12(JK_AA))+(TZ*C13(JK_AA))
              Y1=(TX*C21(JK_AA))+(TY*C22(JK_AA))+(TZ*C23(JK_AA))
              Z1=(TX*C31(JK_AA))+(TY*C32(JK_AA))+(TZ*C33(JK_AA))
              ALENS(31,I)=XEEX-(X1-TTX)
              ALENS(30,I)=YEEY-(Y1-TTY)
              ALENS(69,I)=ZEEZ-(Z1-TTZ)
              ALENS(29,I)=1.0D0
              IF(ALENS(30,I).EQ.0.0D0.AND.ALENS(31,I).EQ.0.0D0.AND.
     1        ALENS(69,I).EQ.0.0D0) ALENS(29,I)=0.0D0
              RETURN
          END IF
          IF(ALENS(25,I).EQ.-1.0D0) THEN
C     WE HAVE AN RTILT WITH A PIVOT

C       NOW ROTATE THE SUCKER!
              JK_AA=CP*(PII/180.0D0)
              X1=(TX*C11(JK_AA))+(TY*C12(JK_AA))+(TZ*C13(JK_AA))
              Y1=(TX*C21(JK_AA))+(TY*C22(JK_AA))+(TZ*C23(JK_AA))
              Z1=(TX*C31(JK_AA))+(TY*C32(JK_AA))+(TZ*C33(JK_AA))
              TX=X1
              TY=Y1
              TZ=Z1
              JK_AA=BP*(PII/180.0D0)
              X1=(TX*B11(JK_AA))+(TY*B12(JK_AA))+(TZ*B13(JK_AA))
              Y1=(TX*B21(JK_AA))+(TY*B22(JK_AA))+(TZ*B23(JK_AA))
              Z1=(TX*B31(JK_AA))+(TY*B32(JK_AA))+(TZ*B33(JK_AA))
              TX=X1
              TY=Y1
              TZ=Z1
              JK_AA=AP*(PII/180.0D0)
              X1=(TX*A11(JK_AA))+(TY*A12(JK_AA))+(TZ*A13(JK_AA))
              Y1=(TX*A21(JK_AA))+(TY*A22(JK_AA))+(TZ*A23(JK_AA))
              Z1=(TX*A31(JK_AA))+(TY*A32(JK_AA))+(TZ*A33(JK_AA))
              ALENS(31,I)=ALENS(114,I)-(X1-TTX)
              ALENS(30,I)=ALENS(115,I)-(Y1-TTY)
              ALENS(69,I)=ALENS(116,I)-(Z1-TTZ)
              ALENS(29,I)=1.0D0
              IF(ALENS(30,I).EQ.0.0D0.AND.ALENS(31,I).EQ.0.0D0.AND.
     1        ALENS(69,I).EQ.0.0D0) ALENS(29,I)=0.0D0
              RETURN
          END IF
      END
C SUB BAKONE.FOR
      SUBROUTINE BAKONE
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE TAKES THE COORDINATES R_TX,R_TY AND R_TZ
C       IN THE LOCAL COORDINTE SYSTEM OF SURFACE NEWOBJ+1 AND
C       EXPRESSES THEM IN THE LOCAL COORDINATE SYSTEM OF SURFACE
C       NEWOBJ.
C
          REAL*8 A11,A12,A13,A21
     1    ,A22,A23,A31,A32,A33,AEEA,BEEB,CEEC,XEEX,YEEY,ZEEZ
     2    ,AN,JK_AA,TX1,TY1,TZ1
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     1    ,C11,C12,C13,C21,C22,C23,C31,C32,C33
C
          REAL*8 LX0,LY0,LZ0,MX0,MY0,MZ0,NX0,NY0,NZ0

C
          INTEGER NI
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
          NI=NEWOBJ+1
C     BEFORE A ROTATION, THE LOCAL AXIS DIRECTION COSINES ARE:
C
          LX0=1.0D0
          MX0=0.0D0
          NX0=0.0D0
          LY0=0.0D0
          MY0=1.0D0
          NY0=0.0D0
          LZ0=0.0D0
          MZ0=0.0D0
          NZ0=1.0D0
          AEEA=ALENS(26,NI)
          BEEB=ALENS(27,NI)
          CEEC=ALENS(28,NI)
          XEEX=ALENS(31,NI)
          YEEY=ALENS(30,NI)
          ZEEZ=ALENS(69,NI)
C
C       DO THE COORDINATE TRANSFORMATION
C
C       IF DECENTERS PRESENT
          IF(ALENS(25,NEWOBJ+1).EQ.0.0D0) THEN
C     NO TILTS
C     IF DECENTERS
              IF(ALENS(29,NEWOBJ+1).EQ.1.0D0) THEN
                  R_TX=R_TX+XEEX
                  R_TY=R_TY+YEEY
                  R_TZ=R_TZ+ZEEZ
              END IF
          ELSE
C       TILTS OR RTILTS
              IF(ALENS(25,NEWOBJ+1).EQ.-1.0D0) THEN
C       NEWOBJ+1 WAS RTILTED, APPLY A TILT
C     IF DECENTERS
                  IF(ALENS(29,NEWOBJ+1).EQ.1.0D0) THEN
                      R_TX=R_TX-XEEX
                      R_TY=R_TY-YEEY
                      R_TZ=R_TZ-ZEEZ
                  END IF
C
                  IF(AEEA.NE.0.0D0) THEN
                      JK_AA=AEEA*(PII/180.0D0)
                      TX1=(R_TX*A11(JK_AA))+(R_TY*A12(JK_AA))+(R_TZ*A13(JK_AA))
                      TY1=(R_TX*A21(JK_AA))+(R_TY*A22(JK_AA))+(R_TZ*A23(JK_AA))
                      TZ1=(R_TX*A31(JK_AA))+(R_TY*A32(JK_AA))+(R_TZ*A33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(BEEB.NE.0.0D0) THEN
                      JK_AA=BEEB*(PII/180.0D0)
                      TX1=(R_TX*B11(JK_AA))+(R_TY*B12(JK_AA))+(R_TZ*B13(JK_AA))
                      TY1=(R_TX*B21(JK_AA))+(R_TY*B22(JK_AA))+(R_TZ*B23(JK_AA))
                      TZ1=(R_TX*B31(JK_AA))+(R_TY*B32(JK_AA))+(R_TZ*B33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(CEEC.NE.0.0D0) THEN
                      JK_AA=CEEC*(PII/180.0D0)
                      TX1=(R_TX*C11(JK_AA))+(R_TY*C12(JK_AA))+(R_TZ*C13(JK_AA))
                      TY1=(R_TX*C21(JK_AA))+(R_TY*C22(JK_AA))+(R_TZ*C23(JK_AA))
                      TZ1=(R_TX*C31(JK_AA))+(R_TY*C32(JK_AA))+(R_TZ*C33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
              END IF
C       NOT RTILT
C
              IF(ALENS(25,NEWOBJ+1).EQ.1.0D0.OR.ALENS(25,NEWOBJ+1).EQ.4.0D0
     1        .OR.ALENS(25,NEWOBJ+1).EQ.5.0D0) THEN
C       NEWOBJ+1 WAS TILTED, APPLY AN RTILT
                  IF(CEEC.NE.0.0D0) THEN
                      JK_AA=-CEEC*(PII/180.0D0)
                      TX1=(R_TX*C11(JK_AA))+(R_TY*C12(JK_AA))+(R_TZ*C13(JK_AA))
                      TY1=(R_TX*C21(JK_AA))+(R_TY*C22(JK_AA))+(R_TZ*C23(JK_AA))
                      TZ1=(R_TX*C31(JK_AA))+(R_TY*C32(JK_AA))+(R_TZ*C33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(BEEB.NE.0.0D0) THEN
                      JK_AA=-BEEB*(PII/180.0D0)
                      TX1=(R_TX*B11(JK_AA))+(R_TY*B12(JK_AA))+(R_TZ*B13(JK_AA))
                      TY1=(R_TX*B21(JK_AA))+(R_TY*B22(JK_AA))+(R_TZ*B23(JK_AA))
                      TZ1=(R_TX*B31(JK_AA))+(R_TY*B32(JK_AA))+(R_TZ*B33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(AEEA.NE.0.0D0) THEN
                      JK_AA=-AEEA*(PII/180.0D0)
                      TX1=(R_TX*A11(JK_AA))+(R_TY*A12(JK_AA))+(R_TZ*A13(JK_AA))
                      TY1=(R_TX*A21(JK_AA))+(R_TY*A22(JK_AA))+(R_TZ*A23(JK_AA))
                      TZ1=(R_TX*A31(JK_AA))+(R_TY*A32(JK_AA))+(R_TZ*A33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
C       IF DECENTERS PRESENT
                  IF(ALENS(29,NEWOBJ+1).EQ.1.0D0) THEN
                      R_TX=R_TX+XEEX
                      R_TY=R_TY+YEEY
                      R_TZ=R_TZ+ZEEZ
                  END IF
                  IF(ALENS(95,NEWOBJ+1).NE.0.0D0) THEN
                      JK_AA=-ALENS(95,NEWOBJ+1)*(PII/180.0D0)
                      TX1=(R_TX*C11(JK_AA))+(R_TY*C12(JK_AA))+(R_TZ*C13(JK_AA))
                      TY1=(R_TX*C21(JK_AA))+(R_TY*C22(JK_AA))+(R_TZ*C23(JK_AA))
                      TZ1=(R_TX*C31(JK_AA))+(R_TY*C32(JK_AA))+(R_TZ*C33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(ALENS(94,NEWOBJ+1).NE.0.0D0) THEN
                      JK_AA=-ALENS(94,NEWOBJ+1)*(PII/180.0D0)
                      TX1=(R_TX*B11(JK_AA))+(R_TY*B12(JK_AA))+(R_TZ*B13(JK_AA))
                      TY1=(R_TX*B21(JK_AA))+(R_TY*B22(JK_AA))+(R_TZ*B23(JK_AA))
                      TZ1=(R_TX*B31(JK_AA))+(R_TY*B32(JK_AA))+(R_TZ*B33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(ALENS(93,NEWOBJ+1).NE.0.0D0) THEN
                      JK_AA=-ALENS(93,NEWOBJ+1)*(PII/180.0D0)
                      TX1=(R_TX*A11(JK_AA))+(R_TY*A12(JK_AA))+(R_TZ*A13(JK_AA))
                      TY1=(R_TX*A21(JK_AA))+(R_TY*A22(JK_AA))+(R_TZ*A23(JK_AA))
                      TZ1=(R_TX*A31(JK_AA))+(R_TY*A32(JK_AA))+(R_TZ*A33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  R_TX=R_TX+ALENS(90,(NEWOBJ+1))
                  R_TY=R_TY+ALENS(91,(NEWOBJ+1))
                  R_TZ=R_TZ+ALENS(92,(NEWOBJ+1))
C       NOT TILT
              END IF
              IF(ALENS(25,NEWOBJ+1).EQ.4.0D0) THEN
C       NEWOBJ+1 WAS TILTED, APPLY AN RTILT TWICE
                  IF(CEEC.NE.0.0D0) THEN
                      JK_AA=-CEEC*(PII/180.0D0)
                      TX1=(R_TX*C11(JK_AA))+(R_TY*C12(JK_AA))+(R_TZ*C13(JK_AA))
                      TY1=(R_TX*C21(JK_AA))+(R_TY*C22(JK_AA))+(R_TZ*C23(JK_AA))
                      TZ1=(R_TX*C31(JK_AA))+(R_TY*C32(JK_AA))+(R_TZ*C33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(BEEB.NE.0.0D0) THEN
                      JK_AA=-BEEB*(PII/180.0D0)
                      TX1=(R_TX*B11(JK_AA))+(R_TY*B12(JK_AA))+(R_TZ*B13(JK_AA))
                      TY1=(R_TX*B21(JK_AA))+(R_TY*B22(JK_AA))+(R_TZ*B23(JK_AA))
                      TZ1=(R_TX*B31(JK_AA))+(R_TY*B32(JK_AA))+(R_TZ*B33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(AEEA.NE.0.0D0) THEN
                      JK_AA=-AEEA*(PII/180.0D0)
                      TX1=(R_TX*A11(JK_AA))+(R_TY*A12(JK_AA))+(R_TZ*A13(JK_AA))
                      TY1=(R_TX*A21(JK_AA))+(R_TY*A22(JK_AA))+(R_TZ*A23(JK_AA))
                      TZ1=(R_TX*A31(JK_AA))+(R_TY*A32(JK_AA))+(R_TZ*A33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(CEEC.NE.0.0D0) THEN
                      JK_AA=-CEEC*(PII/180.0D0)
                      TX1=(R_TX*C11(JK_AA))+(R_TY*C12(JK_AA))+(R_TZ*C13(JK_AA))
                      TY1=(R_TX*C21(JK_AA))+(R_TY*C22(JK_AA))+(R_TZ*C23(JK_AA))
                      TZ1=(R_TX*C31(JK_AA))+(R_TY*C32(JK_AA))+(R_TZ*C33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(BEEB.NE.0.0D0) THEN
                      JK_AA=-BEEB*(PII/180.0D0)
                      TX1=(R_TX*B11(JK_AA))+(R_TY*B12(JK_AA))+(R_TZ*B13(JK_AA))
                      TY1=(R_TX*B21(JK_AA))+(R_TY*B22(JK_AA))+(R_TZ*B23(JK_AA))
                      TZ1=(R_TX*B31(JK_AA))+(R_TY*B32(JK_AA))+(R_TZ*B33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(AEEA.NE.0.0D0) THEN
                      JK_AA=-AEEA*(PII/180.0D0)
                      TX1=(R_TX*A11(JK_AA))+(R_TY*A12(JK_AA))+(R_TZ*A13(JK_AA))
                      TY1=(R_TX*A21(JK_AA))+(R_TY*A22(JK_AA))+(R_TZ*A23(JK_AA))
                      TZ1=(R_TX*A31(JK_AA))+(R_TY*A32(JK_AA))+(R_TZ*A33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
C       IF DECENTERS PRESENT
                  IF(ALENS(29,NEWOBJ+1).EQ.1.0D0) THEN
                      R_TX=R_TX+XEEX
                      R_TY=R_TY+YEEY
                      R_TZ=R_TZ+ZEEZ
                  END IF
C       NOT TILT BEN
              END IF
          END IF
          R_TZ=R_TZ+ALENS(3,NEWOBJ)
          RETURN
      END
C SUB FORONEL.FOR
      SUBROUTINE FORONEL
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE TAKES THE COORDINATES R_TX,R_TY AND R_TZ
C       IN THE LOCAL COORDINTE SYSTEM OF SURFACE NEWOBJ AND
C       EXPRESSES THEM IN THE LOCAL COORDINATE SYSTEM OF SURFACE
C       NEWOBJ+1.
C
          REAL*8 A11,A12,A13,A21,ALPHA,BETA,
     1    A22,A23,A31,A32,A33,AEEA,BEEB,CEEC,XEEX,YEEY,ZEEZ,
     2    AN,JK_AA,TX1,TY1,TZ1,JX,JY,LL1,MM1,NN1,
     1    B11,B12,B13,B21,B22,B23,B31,B32,B33,JZ,
     1    C11,C12,C13,C21,C22,C23,C31,C32,C33
C
          REAL*8 LX,LY,LZ,MX,MY,MZ,NX,NY,NZ
     1    ,LX0,LY0,LZ0,MX0,MY0,MZ0,NX0,NY0,NZ0
     2    ,JLX1,JLY1,JLZ1,JMX1,JMY1,JMZ1,JNX1,JNY1,JNZ1
C
          INTEGER NI
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
          NI=NEWOBJ+1
C     BEFORE A ROTATION, THE LOCAL AXIS DIRECTION COSINES ARE:
C
          LX0=1.0D0
          MX0=0.0D0
          NX0=0.0D0
          LY0=0.0D0
          MY0=1.0D0
          NY0=0.0D0
          LZ0=0.0D0
          MZ0=0.0D0
          NZ0=1.0D0
          AEEA=ALENS(26,NI)
          BEEB=ALENS(27,NI)
          CEEC=ALENS(28,NI)
          XEEX=ALENS(31,NI)
          YEEY=ALENS(30,NI)
          ZEEZ=ALENS(69,NI)
C     IF THE SURFACE HAS A TILT, RTILT OR TILT DAR AND IF THERE IS
C     AN ALTERNATE PIVOT POINT DEFINED AND IF PIVAXIS IS SET TO NORMAL
C     THEN COMPUTE A MODIFIED SET OF INTERNAL TILT VALUES
          IF(ALENS(25,NI).EQ.1.0D0.AND.ALENS(59,NI).EQ.1.0D0.AND.
     1    ALENS(113,NI).EQ.1.0D0.OR.
     2    ALENS(25,NI).EQ.-1.0D0.AND.ALENS(59,NI).EQ.1.0D0.AND.
     3    ALENS(113,NI).EQ.1.0D0.OR.
     2    ALENS(25,NI).EQ.5.0D0.AND.ALENS(59,NI).EQ.1.0D0.AND.
     3    ALENS(113,NI).EQ.1.0D0) THEN
C     JX AND JY ARE THE X AND Y PIVOT POSITIONS
              JX=ALENS(78,NI)
              JY=ALENS(79,NI)
C     THE SURFACE NORMAL DIRECTION COSINES AT THIS JX AND JY ARE:
C     LL1,MM1,NN1
              CALL SAGINT(NI,JX,JY,JZ,LL1,MM1,NN1)
C     THIS IS PERFORMED VIA AN ALPHA AND A BETA ROTATION ONLY
              CALL ANGLECALC(ALPHA,BETA,LL1,MM1,NN1)
C     NOW ROTATE THE AXES DIRECTION COSINES BY ALPHA AND BETA
C     AND THEN BY THE USER ASSIGNED ROTATION ANGLES
              LX=LX0
              MX=MX0
              NX=NX0
              LY=LY0
              MY=MY0
              NY=NY0
              LZ=LZ0
              MZ=MZ0
              NZ=NZ0
              IF(ALPHA.NE.0.0D0) THEN
                  JK_AA=ALPHA*(PII/180.0D0)
                  JLX1=(LX*A11(JK_AA))+(MX*A12(JK_AA))+(NX*A13(JK_AA))
                  JMX1=(LX*A21(JK_AA))+(MX*A22(JK_AA))+(NX*A23(JK_AA))
                  JNX1=(LX*A31(JK_AA))+(MX*A32(JK_AA))+(NX*A33(JK_AA))
                  JLY1=(LY*A11(JK_AA))+(MY*A12(JK_AA))+(NY*A13(JK_AA))
                  JMY1=(LY*A21(JK_AA))+(MY*A22(JK_AA))+(NY*A23(JK_AA))
                  JNY1=(LY*A31(JK_AA))+(MY*A32(JK_AA))+(NY*A33(JK_AA))
                  JLZ1=(LZ*A11(JK_AA))+(MZ*A12(JK_AA))+(NZ*A13(JK_AA))
                  JMZ1=(LZ*A21(JK_AA))+(MZ*A22(JK_AA))+(NZ*A23(JK_AA))
                  JNZ1=(LZ*A31(JK_AA))+(MZ*A32(JK_AA))+(NZ*A33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(BETA.NE.0.0D0) THEN
                  JK_AA=BETA*(PII/180.0D0)
                  JLX1=(LX*B11(JK_AA))+(MX*B12(JK_AA))+(NX*B13(JK_AA))
                  JMX1=(LX*B21(JK_AA))+(MX*B22(JK_AA))+(NX*B23(JK_AA))
                  JNX1=(LX*B31(JK_AA))+(MX*B32(JK_AA))+(NX*B33(JK_AA))
                  JLY1=(LY*B11(JK_AA))+(MY*B12(JK_AA))+(NY*B13(JK_AA))
                  JMY1=(LY*B21(JK_AA))+(MY*B22(JK_AA))+(NY*B23(JK_AA))
                  JNY1=(LY*B31(JK_AA))+(MY*B32(JK_AA))+(NY*B33(JK_AA))
                  JLZ1=(LZ*B11(JK_AA))+(MZ*B12(JK_AA))+(NZ*B13(JK_AA))
                  JMZ1=(LZ*B21(JK_AA))+(MZ*B22(JK_AA))+(NZ*B23(JK_AA))
                  JNZ1=(LZ*B31(JK_AA))+(MZ*B32(JK_AA))+(NZ*B33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(AEEA.NE.0.0D0) THEN
                  JK_AA=AEEA*(PII/180.0D0)
                  JLX1=(LX*A11(JK_AA))+(MX*A12(JK_AA))+(NX*A13(JK_AA))
                  JMX1=(LX*A21(JK_AA))+(MX*A22(JK_AA))+(NX*A23(JK_AA))
                  JNX1=(LX*A31(JK_AA))+(MX*A32(JK_AA))+(NX*A33(JK_AA))
                  JLY1=(LY*A11(JK_AA))+(MY*A12(JK_AA))+(NY*A13(JK_AA))
                  JMY1=(LY*A21(JK_AA))+(MY*A22(JK_AA))+(NY*A23(JK_AA))
                  JNY1=(LY*A31(JK_AA))+(MY*A32(JK_AA))+(NY*A33(JK_AA))
                  JLZ1=(LZ*A11(JK_AA))+(MZ*A12(JK_AA))+(NZ*A13(JK_AA))
                  JMZ1=(LZ*A21(JK_AA))+(MZ*A22(JK_AA))+(NZ*A23(JK_AA))
                  JNZ1=(LZ*A31(JK_AA))+(MZ*A32(JK_AA))+(NZ*A33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(BEEB.NE.0.0D0) THEN
                  JK_AA=BEEB*(PII/180.0D0)
                  JLX1=(LX*B11(JK_AA))+(MX*B12(JK_AA))+(NX*B13(JK_AA))
                  JMX1=(LX*B21(JK_AA))+(MX*B22(JK_AA))+(NX*B23(JK_AA))
                  JNX1=(LX*B31(JK_AA))+(MX*B32(JK_AA))+(NX*B33(JK_AA))
                  JLY1=(LY*B11(JK_AA))+(MY*B12(JK_AA))+(NY*B13(JK_AA))
                  JMY1=(LY*B21(JK_AA))+(MY*B22(JK_AA))+(NY*B23(JK_AA))
                  JNY1=(LY*B31(JK_AA))+(MY*B32(JK_AA))+(NY*B33(JK_AA))
                  JLZ1=(LZ*B11(JK_AA))+(MZ*B12(JK_AA))+(NZ*B13(JK_AA))
                  JMZ1=(LZ*B21(JK_AA))+(MZ*B22(JK_AA))+(NZ*B23(JK_AA))
                  JNZ1=(LZ*B31(JK_AA))+(MZ*B32(JK_AA))+(NZ*B33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(CEEC.NE.0.0D0) THEN
                  JK_AA=CEEC*(PII/180.0D0)
                  JLX1=(LX*C11(JK_AA))+(MX*C12(JK_AA))+(NX*C13(JK_AA))
                  JMX1=(LX*C21(JK_AA))+(MX*C22(JK_AA))+(NX*C23(JK_AA))
                  JNX1=(LX*C31(JK_AA))+(MX*C32(JK_AA))+(NX*C33(JK_AA))
                  JLY1=(LY*C11(JK_AA))+(MY*C12(JK_AA))+(NY*C13(JK_AA))
                  JMY1=(LY*C21(JK_AA))+(MY*C22(JK_AA))+(NY*C23(JK_AA))
                  JNY1=(LY*C31(JK_AA))+(MY*C32(JK_AA))+(NY*C33(JK_AA))
                  JLZ1=(LZ*C11(JK_AA))+(MZ*C12(JK_AA))+(NZ*C13(JK_AA))
                  JMZ1=(LZ*C21(JK_AA))+(MZ*C22(JK_AA))+(NZ*C23(JK_AA))
                  JNZ1=(LZ*C31(JK_AA))+(MZ*C32(JK_AA))+(NZ*C33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(BETA.NE.0.0D0) THEN
                  JK_AA=-BETA*(PII/180.0D0)
                  JLX1=(LX*B11(JK_AA))+(MX*B12(JK_AA))+(NX*B13(JK_AA))
                  JMX1=(LX*B21(JK_AA))+(MX*B22(JK_AA))+(NX*B23(JK_AA))
                  JNX1=(LX*B31(JK_AA))+(MX*B32(JK_AA))+(NX*B33(JK_AA))
                  JLY1=(LY*B11(JK_AA))+(MY*B12(JK_AA))+(NY*B13(JK_AA))
                  JMY1=(LY*B21(JK_AA))+(MY*B22(JK_AA))+(NY*B23(JK_AA))
                  JNY1=(LY*B31(JK_AA))+(MY*B32(JK_AA))+(NY*B33(JK_AA))
                  JLZ1=(LZ*B11(JK_AA))+(MZ*B12(JK_AA))+(NZ*B13(JK_AA))
                  JMZ1=(LZ*B21(JK_AA))+(MZ*B22(JK_AA))+(NZ*B23(JK_AA))
                  JNZ1=(LZ*B31(JK_AA))+(MZ*B32(JK_AA))+(NZ*B33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
              IF(ALPHA.NE.0.0D0) THEN
                  JK_AA=-ALPHA*(PII/180.0D0)
                  JLX1=(LX*A11(JK_AA))+(MX*A12(JK_AA))+(NX*A13(JK_AA))
                  JMX1=(LX*A21(JK_AA))+(MX*A22(JK_AA))+(NX*A23(JK_AA))
                  JNX1=(LX*A31(JK_AA))+(MX*A32(JK_AA))+(NX*A33(JK_AA))
                  JLY1=(LY*A11(JK_AA))+(MY*A12(JK_AA))+(NY*A13(JK_AA))
                  JMY1=(LY*A21(JK_AA))+(MY*A22(JK_AA))+(NY*A23(JK_AA))
                  JNY1=(LY*A31(JK_AA))+(MY*A32(JK_AA))+(NY*A33(JK_AA))
                  JLZ1=(LZ*A11(JK_AA))+(MZ*A12(JK_AA))+(NZ*A13(JK_AA))
                  JMZ1=(LZ*A21(JK_AA))+(MZ*A22(JK_AA))+(NZ*A23(JK_AA))
                  JNZ1=(LZ*A31(JK_AA))+(MZ*A32(JK_AA))+(NZ*A33(JK_AA))
                  LX=JLX1
                  MX=JMX1
                  NX=JNX1
                  LY=JLY1
                  MY=JMY1
                  NY=JNY1
                  LZ=JLZ1
                  MZ=JMZ1
                  NZ=JNZ1
              END IF
C     THE NEW AEEA,BEEB AND CEEC ANGLES ARE THE ONES THAT GO FROM
C     THE UNROTATED COORDINATE SYSTEM TO THESE NEW DIRECTION COSINES
              CALL NEWANGLES(AEEA,BEEB,CEEC,LX,MX,NX,LY,MY,NY,LZ,MZ,NX)
C       DONE WITH AEEA,BEEM,CEEC,XEEX,YEEY AND ZEEZ ADJUSTMENTS
          END IF
C
C       DO THE COORDINATE TRANSFORMATION
C
          IF(ALENS(25,NEWOBJ+1).EQ.0.0D0) THEN
C     NO TILTS
          ELSE
C       TILTS OR RTILTS
              IF(ALENS(25,NEWOBJ+1).EQ.-1.0D0) THEN
C       NEWOBJ+1 WAS RTILTED, APPLY AN RTILT
                  IF(CEEC.NE.0.0D0) THEN
                      JK_AA=-CEEC*(PII/180.0D0)
                      TX1=(R_TX*C11(JK_AA))+(R_TY*C12(JK_AA))+(R_TZ*C13(JK_AA))
                      TY1=(R_TX*C21(JK_AA))+(R_TY*C22(JK_AA))+(R_TZ*C23(JK_AA))
                      TZ1=(R_TX*C31(JK_AA))+(R_TY*C32(JK_AA))+(R_TZ*C33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(BEEB.NE.0.0D0) THEN
                      JK_AA=-BEEB*(PII/180.0D0)
                      TX1=(R_TX*B11(JK_AA))+(R_TY*B12(JK_AA))+(R_TZ*B13(JK_AA))
                      TY1=(R_TX*B21(JK_AA))+(R_TY*B22(JK_AA))+(R_TZ*B23(JK_AA))
                      TZ1=(R_TX*B31(JK_AA))+(R_TY*B32(JK_AA))+(R_TZ*B33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
C
                  IF(AEEA.NE.0.0D0) THEN
                      JK_AA=-AEEA*(PII/180.0D0)
                      TX1=(R_TX*A11(JK_AA))+(R_TY*A12(JK_AA))+(R_TZ*A13(JK_AA))
                      TY1=(R_TX*A21(JK_AA))+(R_TY*A22(JK_AA))+(R_TZ*A23(JK_AA))
                      TZ1=(R_TX*A31(JK_AA))+(R_TY*A32(JK_AA))+(R_TZ*A33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
              END IF
C       NOT RTILT
C
              IF(ALENS(25,NEWOBJ+1).EQ.1.0D0) THEN
C       NEWOBJ+1 WAS TILTED, APPLY A TILT
                  IF(AEEA.NE.0.0D0) THEN
                      JK_AA=AEEA*(PII/180.0D0)
                      TX1=(R_TX*A11(JK_AA))+(R_TY*A12(JK_AA))+(R_TZ*A13(JK_AA))
                      TY1=(R_TX*A21(JK_AA))+(R_TY*A22(JK_AA))+(R_TZ*A23(JK_AA))
                      TZ1=(R_TX*A31(JK_AA))+(R_TY*A32(JK_AA))+(R_TZ*A33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(BEEB.NE.0.0D0) THEN
                      JK_AA=BEEB*(PII/180.0D0)
                      TX1=(R_TX*B11(JK_AA))+(R_TY*B12(JK_AA))+(R_TZ*B13(JK_AA))
                      TY1=(R_TX*B21(JK_AA))+(R_TY*B22(JK_AA))+(R_TZ*B23(JK_AA))
                      TZ1=(R_TX*B31(JK_AA))+(R_TY*B32(JK_AA))+(R_TZ*B33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(CEEC.NE.0.0D0) THEN
                      JK_AA=CEEC*(PII/180.0D0)
                      TX1=(R_TX*C11(JK_AA))+(R_TY*C12(JK_AA))+(R_TZ*C13(JK_AA))
                      TY1=(R_TX*C21(JK_AA))+(R_TY*C22(JK_AA))+(R_TZ*C23(JK_AA))
                      TZ1=(R_TX*C31(JK_AA))+(R_TY*C32(JK_AA))+(R_TZ*C33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
C       NOT TILT
              END IF
              IF(ALENS(25,NEWOBJ+1).EQ.4.0D0) THEN
C       NEWOBJ+1 WAS TILTED, APPLY AN TILT TWICE
                  IF(AEEA.NE.0.0D0) THEN
                      JK_AA=AEEA*(PII/180.0D0)
                      TX1=(R_TX*A11(JK_AA))+(R_TY*A12(JK_AA))+(R_TZ*A13(JK_AA))
                      TY1=(R_TX*A21(JK_AA))+(R_TY*A22(JK_AA))+(R_TZ*A23(JK_AA))
                      TZ1=(R_TX*A31(JK_AA))+(R_TY*A32(JK_AA))+(R_TZ*A33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(BEEB.NE.0.0D0) THEN
                      JK_AA=BEEB*(PII/180.0D0)
                      TX1=(R_TX*B11(JK_AA))+(R_TY*B12(JK_AA))+(R_TZ*B13(JK_AA))
                      TY1=(R_TX*B21(JK_AA))+(R_TY*B22(JK_AA))+(R_TZ*B23(JK_AA))
                      TZ1=(R_TX*B31(JK_AA))+(R_TY*B32(JK_AA))+(R_TZ*B33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(CEEC.NE.0.0D0) THEN
                      JK_AA=CEEC*(PII/180.0D0)
                      TX1=(R_TX*C11(JK_AA))+(R_TY*C12(JK_AA))+(R_TZ*C13(JK_AA))
                      TY1=(R_TX*C21(JK_AA))+(R_TY*C22(JK_AA))+(R_TZ*C23(JK_AA))
                      TZ1=(R_TX*C31(JK_AA))+(R_TY*C32(JK_AA))+(R_TZ*C33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(AEEA.NE.0.0D0) THEN
                      JK_AA=AEEA*(PII/180.0D0)
                      TX1=(R_TX*A11(JK_AA))+(R_TY*A12(JK_AA))+(R_TZ*A13(JK_AA))
                      TY1=(R_TX*A21(JK_AA))+(R_TY*A22(JK_AA))+(R_TZ*A23(JK_AA))
                      TZ1=(R_TX*A31(JK_AA))+(R_TY*A32(JK_AA))+(R_TZ*A33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(BEEB.NE.0.0D0) THEN
                      JK_AA=BEEB*(PII/180.0D0)
                      TX1=(R_TX*B11(JK_AA))+(R_TY*B12(JK_AA))+(R_TZ*B13(JK_AA))
                      TY1=(R_TX*B21(JK_AA))+(R_TY*B22(JK_AA))+(R_TZ*B23(JK_AA))
                      TZ1=(R_TX*B31(JK_AA))+(R_TY*B32(JK_AA))+(R_TZ*B33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
                  IF(CEEC.NE.0.0D0) THEN
                      JK_AA=CEEC*(PII/180.0D0)
                      TX1=(R_TX*C11(JK_AA))+(R_TY*C12(JK_AA))+(R_TZ*C13(JK_AA))
                      TY1=(R_TX*C21(JK_AA))+(R_TY*C22(JK_AA))+(R_TZ*C23(JK_AA))
                      TZ1=(R_TX*C31(JK_AA))+(R_TY*C32(JK_AA))+(R_TZ*C33(JK_AA))
                      R_TX=TX1
                      R_TY=TY1
                      R_TZ=TZ1
                  END IF
C       NOT TILT, TILT BEN OR TILT DAR
              END IF
          END IF
          RETURN
      END
C SUB TRNSF2.FOR
      SUBROUTINE TRNSF2
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE TRNSF2.FOR. THIS SUBROUTINE IMPLEMENTS
C       TRANSFER TO THE NEXT SURFACE IN RAYTRACING.(NON-REFERENCE RAY)
C       NO TILT AUTOS TO DEAL WITH HERE
C
          REAL*8 JK_AA,A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33,TDECX,TDECY,TDECZ
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33
     3    ,AN,X1,Y1,Z1,L1,M1,N1
     4    ,AEEA,BEEB,CEEC,XEEX,YEEY,ZEEZ
     5    ,AEEAM,BEEBM,CEECM,XEEXM,YEEYM,ZEEZM
C
!      INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C     BEFORE A ROTATION, THE LOCAL AXIS DIRECTION COSINES ARE:
C
          AEEA=ALENS(26,R_I)
          BEEB=ALENS(27,R_I)
          CEEC=ALENS(28,R_I)
          XEEX=ALENS(31,R_I)
          YEEY=ALENS(30,R_I)
          ZEEZ=ALENS(69,R_I)
          AEEAM=ALENS(26,R_I-1)
          BEEBM=ALENS(27,R_I-1)
          CEECM=ALENS(28,R_I-1)
          XEEXM=ALENS(31,R_I-1)
          YEEYM=ALENS(30,R_I-1)
          ZEEZM=ALENS(69,R_I-1)
C
          IF(ALENS(25,R_I-1).EQ.7.0D0 ) THEN
C     RESOLVE TILT REV ISSUES
C     WE MUST DO AN RTILT ON SURFACE I-1
C       ROTATE THE SUCKER!
              IF(CEECM.NE.0.0D0) THEN
                  JK_AA=-CEECM*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(BEEBM.NE.0.0D0) THEN
                  JK_AA=-BEEBM*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(AEEAM.NE.0.0D0) THEN
                  JK_AA=-AEEAM*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
C       IF DECENTERS PRESENT
              IF(ALENS(29,R_I-1).EQ.1.0D0) THEN
                  R_X=R_X+XEEXM
                  R_Y=R_Y+YEEYM
                  R_Z=R_Z+ZEEZM
              END IF
C     NOW PROCEED WITH THE TILTS AND TRANSFER TO SURFACE I COORDINATES
          END IF
C       TILT REV RESOLVED.
C       FOR ALL OTHER CASES CHECK FOR TILT BEN, TILT DAR
C
C     ARE THERE TILT BEN OR TILT DAR ISSUES TO RESOLVE
          IF(ALENS(25,R_I-1).EQ.4.0D0) THEN
C     RESOLVE TILT BEN ISSUES
C     THERE IS A TILT BEN ON SURFACE I-1. WE NEED TO DO ONE MORE
C     TILT AT SURFACE I-1
C       TILT PRESENT
C
C       NOW ROTATE THE SUCKER!
              IF(AEEAM.NE.0.0D0) THEN
                  JK_AA=AEEAM*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(BEEBM.NE.0.0D0) THEN
                  JK_AA=BEEBM*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(CEECM.NE.0.0D0) THEN
                  JK_AA=CEECM*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
C     NOW TAKE CARE OF THE TILTS AND DECENTERS AND TRANSFER TO SURFACE I
C     COORDINATES
          END IF
          IF(ALENS(25,R_I-1).EQ.5.0D0) THEN
C     RESOLVE TILT DAR ISSUES
C     WE MUST DO AN RTILT ON SURFACE I-1
C       ROTATE THE SUCKER!
              IF(CEECM.NE.0.0D0) THEN
                  JK_AA=-CEECM*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(BEEBM.NE.0.0D0) THEN
                  JK_AA=-BEEBM*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(AEEAM.NE.0.0D0) THEN
                  JK_AA=-AEEAM*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
C       IF DECENTERS PRESENT
              IF(ALENS(29,R_I-1).EQ.1.0D0) THEN
                  R_X=R_X+XEEXM
                  R_Y=R_Y+YEEYM
                  R_Z=R_Z+ZEEZM
              END IF
C     NOW PROCEED WITH THE TILTS AND TRANSFER TO SURFACE I COORDINATES
          END IF
C
          IF(ALENS(25,R_I).EQ.7.0D0) THEN
              R_Z=R_Z-ALENS(3,(R_I-1))
              RETURN
          END IF
C
          IF(ALENS(25,R_I).EQ.0.0D0) THEN
C       NO TILTS
C       IF DECENTERS PRESENT
              IF(ALENS(29,R_I).EQ.1.0D0) THEN
                  R_X=R_X-XEEX
                  R_Y=R_Y-YEEY
                  R_Z=R_Z-ZEEZ
              END IF
              R_Z=R_Z-ALENS(3,(R_I-1))
              RETURN
C       TILTS PRESENT, WHAT KIND
          END IF
C       TILT
          IF(ALENS(25,R_I).EQ.1.0D0.OR.ALENS(25,R_I).EQ.6.0D0.OR.
     1    ALENS(25,R_I)
     2    .EQ.2.0D0.OR.ALENS(25,R_I)
     3    .EQ.3.0D0
     4    .OR.ALENS(25,R_I).EQ.4.0D0.OR.ALENS(25,R_I).EQ.5.0D0) THEN
C       TILT PRESENT
              TDECX=XEEX
              TDECY=YEEY
              TDECZ=ZEEZ
              IF(ALENS(29,R_I).EQ.1.0D0) THEN
                  R_X=R_X-TDECX
                  R_Y=R_Y-TDECY
                  R_Z=R_Z-TDECZ
              END IF
              R_Z=R_Z-ALENS(3,(R_I-1))
C
C       NOW ROTATE THE SUCKER!
              IF(AEEA.NE.0.0D0) THEN
                  JK_AA=AEEA*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(BEEB.NE.0.0D0) THEN
                  JK_AA=BEEB*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(CEEC.NE.0.0D0) THEN
                  JK_AA=CEEC*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              R_X=R_X-ALENS(90,R_I)
              R_Y=R_Y-ALENS(91,R_I)
              R_Z=R_Z-ALENS(92,R_I)
C       NOW ROTATE THE SUCKER!
              IF(ALENS(93,R_I).NE.0.0D0) THEN
                  JK_AA=ALENS(93,R_I)*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(ALENS(94,R_I).NE.0.0D0) THEN
                  JK_AA=ALENS(94,R_I)*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(ALENS(95,R_I).NE.0.0D0) THEN
                  JK_AA=ALENS(95,R_I)*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              RETURN
          END IF
C       KEEP CHECKING
C       RTILT
          IF(ALENS(25,R_I).EQ.-1.0D0) THEN
C       RTILT PRESENT
C       ROTATE THE SUCKER!
              IF(CEEC.NE.0.0D0) THEN
                  JK_AA=-CEEC*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(BEEB.NE.0.0D0) THEN
                  JK_AA=-BEEB*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(AEEA.NE.0.0D0) THEN
                  JK_AA=-AEEA*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
C       IF DECENTERS PRESENT
              IF(ALENS(29,R_I).EQ.1.0D0) THEN
                  R_X=R_X+XEEX
                  R_Y=R_Y+YEEY
                  R_Z=R_Z+ZEEZ
              END IF
              R_Z=R_Z-ALENS(3,(R_I-1))
          END IF
          RETURN
      END
C SUB TRANSF.FOR
      SUBROUTINE TRANSF
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE TRANSF.FOR. THIS SUBROUTINE IMPLEMENTS
C       TRANSFER TO THE NEXT SURFACE IN RAYTRACING.
C
C
          REAL*8 A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33
     3    ,AN,JK_AA,TDECX,TDECY,TDECZ
     4    ,T0,NUSUBV,XP,YP,ZP,ANGY,ANGX
     5    ,X1,Y1,Z1,L1,M1,N1,OR_X,OR_Y,OR_Z,OR_L,OR_M,OR_N
     6    ,AEEA,BEEB,CEEC,XEEX,YEEY,ZEEZ
     7    ,AEEAM,BEEBM,CEECM,XEEXM,YEEYM,ZEEZM
C
          !       INTEGER P_I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C     BEFORE A ROTATION, THE LOCAL AXIS DIRECTION COSINES ARE:
C
          OR_X=R_X
          OR_Y=R_Y
          OR_Z=R_Z
          OR_L=R_L
          OR_M=R_M
          OR_N=R_N
          AEEA=ALENS(26,R_I)
          BEEB=ALENS(27,R_I)
          CEEC=ALENS(28,R_I)
          XEEX=ALENS(31,R_I)
          YEEY=ALENS(30,R_I)
          ZEEZ=ALENS(69,R_I)
          AEEAM=ALENS(26,R_I-1)
          BEEBM=ALENS(27,R_I-1)
          CEECM=ALENS(28,R_I-1)
          XEEXM=ALENS(31,R_I-1)
          YEEYM=ALENS(30,R_I-1)
          ZEEZM=ALENS(69,R_I-1)
C
          IF(ALENS(25,R_I-1).EQ.7.0D0) THEN
C     RESOLVE TILT REV ISSUES
C     WE MUST DO AN RTILT ON SURFACE I-1
C       ROTATE THE SUCKER!
              IF(CEECM.NE.0.0D0) THEN
                  JK_AA=-CEECM*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(BEEBM.NE.0.0D0) THEN
                  JK_AA=-BEEBM*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(AEEAM.NE.0.0D0) THEN
                  JK_AA=-AEEAM*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
C       IF DECENTERS PRESENT
              IF(ALENS(29,R_I-1).EQ.1.0D0) THEN
                  R_X=R_X+XEEXM
                  R_Y=R_Y+YEEYM
                  R_Z=R_Z+ZEEZM
              END IF
          END IF
C
C     ARE THERE TILT BEN OR TILT DAR ISSUES TO RESOLVE
          IF(ALENS(25,R_I-1).EQ.4.0D0) THEN
C     RESOLVE TILT BEN ISSUES
C     THERE IS A TILT BEN ON SURFACE I-1. WE NEED TO DO ONE MORE
C     TILT AT SURFACE I-1
C       TILT PRESENT
C
C       NOW ROTATE THE SUCKER!
              IF(AEEAM.NE.0.0D0) THEN
                  JK_AA=AEEAM*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(BEEBM.NE.0.0D0) THEN
                  JK_AA=BEEBM*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(CEECM.NE.0.0D0) THEN
                  JK_AA=CEECM*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
C     NOW TAKE CARE OF THE TILTS AND DECENTERS AND TRANSFER TO SURFACE I
C     COORDINATES
          END IF
          IF(ALENS(25,R_I-1).EQ.5.0D0) THEN
C     RESOLVE TILT DAR ISSUES
C     WE MUST DO AN RTILT ON SURFACE I-1
C       ROTATE THE SUCKER!
              IF(CEECM.NE.0.0D0) THEN
                  JK_AA=-CEECM*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(BEEBM.NE.0.0D0) THEN
                  JK_AA=-BEEBM*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(AEEAM.NE.0.0D0) THEN
                  JK_AA=-AEEAM*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
C       IF DECENTERS PRESENT
              IF(ALENS(29,R_I-1).EQ.1.0D0) THEN
                  R_X=R_X+XEEXM
                  R_Y=R_Y+YEEYM
                  R_Z=R_Z+ZEEZM
              END IF
C     NOW PROCEED WITH THE TILTS AND TRANSFER TO SURFACE I COORDINATES
          END IF
C
          IF(ALENS(25,R_I).EQ.7.0D0) THEN
              R_Z=R_Z-ALENS(3,(R_I-1))
              RETURN
          END IF
          IF(ALENS(25,R_I).EQ.0.0D0) THEN
C       NO TILTS
C       IF DECENTERS PRESENT
              IF(ALENS(29,R_I).EQ.1.0D0) THEN
                  R_X=R_X-XEEX
                  R_Y=R_Y-YEEY
                  R_Z=R_Z-ZEEZ
              END IF
              R_Z=R_Z-ALENS(3,(R_I-1))
              RETURN
          END IF
C       TILTS PRESENT, WHAT KIND
C       TILT
          IF(ALENS(25,R_I).EQ.1.0D0.OR.ALENS(25,R_I).EQ.6.0D0.OR.
     1    ALENS(25,R_I)
     2    .EQ.2.0D0.AND.FOB0.EQ.0.OR.ALENS(25,R_I)
     3    .EQ.3.0D0.AND.FOB0.EQ.0
     4    .OR.ALENS(25,R_I).EQ.4.0D0.OR.ALENS(25,R_I).EQ.5.0D0) THEN
C       REGULAR TILT PRESENT
C       IF DECENTERS PRESENT
              TDECX=XEEX
              TDECY=YEEY
              TDECZ=ZEEZ
              IF(ALENS(29,R_I).EQ.1.0D0) THEN
                  R_X=R_X-TDECX
                  R_Y=R_Y-TDECY
                  R_Z=R_Z-TDECZ
              END IF
              R_Z=R_Z-ALENS(3,(R_I-1))
C
C       NOW ROTATE THE SUCKER!
C
              IF(AEEA.NE.0.0D0) THEN
                  JK_AA=AEEA*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(BEEB.NE.0.0D0) THEN
                  JK_AA=BEEB*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(CEEC.NE.0.0D0) THEN
                  JK_AA=CEEC*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              R_X=R_X-ALENS(90,R_I)
              R_Y=R_Y-ALENS(91,R_I)
              R_Z=R_Z-ALENS(92,R_I)
C
C       NOW ROTATE THE SUCKER!
C
              IF(ALENS(93,R_I).NE.0.0D0) THEN
                  JK_AA=ALENS(93,R_I)*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(ALENS(94,R_I).NE.0.0D0) THEN
                  JK_AA=ALENS(94,R_I)*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(ALENS(95,R_I).NE.0.0D0) THEN
                  JK_AA=ALENS(95,R_I)*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              RETURN
          END IF
C       KEEP CHECKING
C       RTILT
          IF(ALENS(25,R_I).EQ.-1.0D0) THEN
C       RTILT PRESENT
C       ROTATE THE SUCKER!
              IF(CEEC.NE.0.0D0) THEN
                  JK_AA=-CEEC*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(BEEB.NE.0.0D0) THEN
                  JK_AA=-BEEB*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(AEEA.NE.0.0D0) THEN
                  JK_AA=-AEEA*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
C       CHECK DECENTERS
C       IF DECENTERS PRESENT
              IF(ALENS(29,R_I).EQ.1.0D0) THEN
                  R_X=R_X+XEEX
                  R_Y=R_Y+YEEY
                  R_Z=R_Z+ZEEZ
              END IF
              R_Z=R_Z-ALENS(3,(R_I-1))
              RETURN
          END IF
C       KEEP CHECKING
C       CHECK FOR TILT AUTOS AND HANDLE
          IF(ALENS(25,R_I)
     1    .EQ.2.0D0.AND.FOB0.EQ.1.OR.ALENS(25,R_I)
     2    .EQ.3.0D0.AND.FOB0.EQ.1) THEN
              IF(ALENS(25,R_I).EQ.2.0D0) THEN
C       TILT AUTO, IMPLEMENT IT
C       THE ALPHA,BETA AND GAMMA INPUT WITH
C       TILT AUTO WERE STARTING VALUES WHICH ARE NOW
C       GOING TO BE RESET BY TILT AUTO. TILT AUTO, IN THIS
C       CONTEXT, DECENTERS AND TILTS THE LOCAL COORDINATE
C       SYSTEM OF SURFACE I SO THAT THE FOB 0, RAY 0
C       PASSES THROUGH THE ORIGIN OR THE COORDINATE SYSTEM
C       AND ALIGNS THE RAY WITH THE POSITIVE Z-AXIS OR THIS LOCAL
C       SYSTEM SO THAT THE RAY TRAVELS IN THE POSITIVE Z
C       DIRECTION.
C       THE LOCATION AND DIRECTION OF THE RAY AT SURFACE I-1 IS
C       R_X,R_Y,R_Z,R_L,R_M,R_N. THE RAY WILL HAVE COORDINATES
C       X'=0,Y'=0,Z'=0,L'=0,M'=0,N'=1 AT SURFACE I
C       AFTER SURFACE I HAS BEEN DECENTERED BY
C       YD AND XD AND AFTER IT HAS BEEN TILTED BY
C       ALPHA,BETA AND GAMMA(A,B,C).
C       FIRST TRANSFER TO THE COORDINATE SYSTEM OF SURFACE I
C       IGNORE INITIAL DECENTERS
                  R_Z=R_Z-ALENS(3,(R_I-1))
C       IGNORE INITIAL TILTS
C       NOW CALCULATE THE INTERSECTION POINT OF THIS RAY
C       WITH THE PLANE LOCATED AT SURFACE I BUT DON'T
C       CHANGE THE X,Y,Z VALUES FOR REAL.
                  T0=R_Z
C
C       IF(T0.LE.0.0D0.AND.R_N.GE.0.0D0) DON'T REVERSE DIRCOS
C       IF(T0.GT.0.0D0.AND.R_N.GT.0.0D0) REVERSE DIRCOS
C       IF(T0.LT.0.0D0.AND.R_N.LT.0.0D0) REVERSE DIRCOS
C       IF(T0.GE.0.0D0.AND.R_N.LE.0.0D0) DON'T REVERSE DIRCOS
                  IF(T0.GT.0.0D0.AND.R_N.GT.0.0D0.OR.
     1            T0.LT.0.0D0.AND.R_N.LT.0.0D0) THEN
                      R_L=-R_L
                      R_M=-R_M
                      R_N=-R_N
                  END IF
                  NUSUBV=-T0/R_N
                  XP=R_X+NUSUBV*R_L
                  YP=R_Y+NUSUBV*R_M
C       ZP=R_Z+NUSUBV*R_N WHICH IS IDENTICALLY ZERO
                  ZP=0.0D0
C       NOW IF THE COORDINATE OF THE RAY INTERSECTION IS
C       POSITIVE XP AND POSITIVE YP THEN WE MUST DECENTER
C       SURFACE I BY YD=YP AND XD=XP
                  ALENS(29,R_I)=0.0D0
                  ALENP(29,R_I)=0.0D0
                  IF(YP.NE.0.0D0.OR.XP.NE.0.0D0) THEN
                      ALENS(29,R_I)=1.0D0
                      ALENS(31,R_I)=XP
                      ALENS(30,R_I)=YP
                      ALENS(69,R_I)=ZP
                      ALENP(29,R_I)=1.0D0
                      ALENP(31,R_I)=XP
                      ALENP(30,R_I)=YP
                      ALENP(69,R_I)=ZP
                      ALENS(114,R_I)=XP
                      ALENS(115,R_I)=YP
                      ALENS(116,R_I)=ZP
                      ALENP(114,R_I)=XP
                      ALENP(115,R_I)=YP
                      ALENP(116,R_I)=ZP
                      XEEX=ALENS(31,R_I)
                      YEEY=ALENS(30,R_I)
                      ZEEZ=ALENS(69,R_I)
                  ELSE
                      ALENP(29,R_I)=0.0D0
                      ALENS(30,R_I)=0.0D0
                      ALENS(31,R_I)=0.0D0
                      ALENS(69,R_I)=0.0D0
                      ALENP(29,R_I)=0.0D0
                      ALENP(30,R_I)=0.0D0
                      ALENP(31,R_I)=0.0D0
                      ALENP(69,R_I)=0.0D0
                      ALENS(114,R_I)=0.0D0
                      ALENS(115,R_I)=0.0D0
                      ALENS(116,R_I)=0.0D0
                      ALENP(114,R_I)=0.0D0
                      ALENP(115,R_I)=0.0D0
                      ALENP(116,R_I)=0.0D0
                      XEEX=ALENS(31,R_I)
                      YEEY=ALENS(30,R_I)
                      ZEEZ=ALENS(69,R_I)
C                       NO DECENTER
                  END IF
C
C       NOW ALIGN THE LOCAL Z AXIS WITH THE +N DIRECTION COSINE
                  IF(DABS(R_M).GE.DABS(((1.0D35)*R_N))) THEN
                      IF(R_M.GE.0.0D0) ANGY=PII/2.0D0
                      IF(R_M.LT.0.0D0) ANGY=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(R_M).EQ.0.0D0.AND.DABS(R_N).EQ.0.0D0) THEN
                          ANGY=0.0D0
                      ELSE
                          ANGY=DATAN2(R_M,R_N)
                      END IF
                      IF(ANGY.LT.0.0D0) ANGY=ANGY+(TWOPII)
                  END IF
                  IF(DABS(R_L).GE.DABS(((1.0D35)*R_N))) THEN
                      IF(R_L.GE.0.0D0) ANGX=PII/2.0D0
                      IF(R_L.LT.0.0D0) ANGX=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(R_L).EQ.0.0D0.AND.DABS(R_N).EQ.0.0D0) THEN
                          ANGX=0.0D0
                      ELSE
                          ANGX=DATAN2(R_L,R_N)
                      END IF
                      IF(ANGX.LT.0.0D0) ANGX=ANGX+(TWOPII)
                  END IF
C       IN DEGREES WE GET
                  ANGY=ANGY*(180.0D0/PII)
                  ANGX=ANGX*(180.0D0/PII)

                  IF(ANGX.GT.90.0D0) ANGX=ANGX-180.0D0
                  IF(ANGY.GT.90.0D0) ANGY=ANGY-180.0D0
                  IF(ANGX.LT.-90.0D0) ANGX=ANGX+180.0D0
                  IF(ANGY.LT.-90.0D0) ANGY=ANGY+180.0D0
                  IF(DABS(ANGX).EQ.360.0D0) ANGX=0.0
                  IF(DABS(ANGY).EQ.360.0D0) ANGY=0.0
C       AN ALPHA ROTATION (IN DEGREES) OF ANGY BRINGS THE Z AXIS
C       INTO ALIGNMENT WITH THE RAY
C       A BETA ROTATION (IN DEGREES) OF -ANGX BRINGS THE
C       X- COMPONENT INTO ALIGNMENT WITH THE Z AXIS.
C       NO GAMMA ROTATION IS REQUIRED
                  ALENS(26,R_I)=ANGY
                  ALENS(27,R_I)=-ANGX
                  ALENS(28,R_I)=0.0D0
                  ALENP(26,R_I)=ANGY
                  ALENP(27,R_I)=-ANGX
                  ALENP(28,R_I)=0.0D0
                  ALENS(118,R_I)=ANGY
                  ALENS(119,R_I)=-ANGX
                  ALENS(120,R_I)=0.0D0
                  ALENP(118,R_I)=ANGY
                  ALENP(119,R_I)=-ANGX
                  ALENP(120,R_I)=0.0D0
                  AEEA=ALENS(26,R_I)
                  BEEB=ALENS(27,R_I)
                  CEEC=ALENS(28,R_I)
                  XEEX=ALENS(31,R_I)
                  YEEY=ALENS(30,R_I)
                  ZEEZ=ALENS(69,R_I)
              ELSE
C       TILT AUTOM, IMPLEMENT IT
C       TILT AUTOM, IMPLEMENT IT
C       THIS IS THE SAME AS TILT AUTO EXCEPT THAT THE RAY IS ALIGNED
C       ANTI-PARALLEL TO THE LOCAL Z AXIS OF SURFACE I
C       IGNORE INITIAL DECENTERS
                  R_Z=R_Z-ALENS(3,(R_I-1))
C       IGNORE INITIAL TILTS
C       NOW CALCULATE THE INTERSECTION POINT OF THIS RAY
C       WITH THE PLANE LOCATED AT SURFACE I BUT DON'T
C       CHANGE THE X,Y,Z VALUES FOR REAL.
                  T0=R_Z
C
C       IF(T0.LT.0.0D0.AND.R_N.GT.0.0D0) DON'T REVERSE DIRCOS
C       IF(T0.GT.0.0D0.AND.R_N.GT.0.0D0) REVERSE DIRCOS
C       IF(T0.LT.0.0D0.AND.R_N.LT.0.0D0) REVERSE DIRCOS
C       IF(T0.GT.0.0D0.AND.R_N.LT.0.0D0) DON'T REVERSE DIRCOS
                  IF(T0.GT.0.0D0.AND.R_N.GT.0.0D0.OR.
     1            T0.LT.0.0D0.AND.R_N.LT.0.0D0) THEN
                      R_L=-R_L
                      R_M=-R_M
                      R_N=-R_N
                  END IF
                  NUSUBV=-T0/R_N
                  XP=R_X+NUSUBV*R_L
                  YP=R_Y+NUSUBV*R_M
C       ZP=R_Z-NUSUBV*R_N WHICH IS IDENTICALLY ZERO
                  ZP=0.0D0
C       NOW IF THE COORDINATE OF THE RAY INTERSECTION IS
C       POSITIVE XP AND POSITIVE YP THEN WE MUST DECENTER
C       SURFACE I BY YD=YP AND XD=XP
                  ALENS(29,R_I)=0.0D0
                  ALENP(29,R_I)=0.0D0
                  IF(YP.NE.0.0D0.OR.XP.NE.0.0D0) THEN
                      ALENS(29,R_I)=1.0D0
                      ALENS(31,R_I)=XP
                      ALENS(30,R_I)=YP
                      ALENS(69,R_I)=ZP
                      ALENP(29,R_I)=1.0D0
                      ALENP(31,R_I)=XP
                      ALENP(30,R_I)=YP
                      ALENP(69,R_I)=ZP
                      ALENS(114,R_I)=XP
                      ALENS(115,R_I)=YP
                      ALENS(116,R_I)=ZP
                      ALENP(114,R_I)=XP
                      ALENP(115,R_I)=YP
                      ALENP(116,R_I)=ZP
                      XEEX=ALENS(31,R_I)
                      YEEY=ALENS(30,R_I)
                      ZEEZ=ALENS(69,R_I)
                  ELSE
                      ALENP(29,R_I)=0.0D0
                      ALENS(30,R_I)=0.0D0
                      ALENS(31,R_I)=0.0D0
                      ALENS(69,R_I)=0.0D0
                      ALENP(29,R_I)=0.0D0
                      ALENP(30,R_I)=0.0D0
                      ALENP(31,R_I)=0.0D0
                      ALENP(69,R_I)=0.0D0
                      ALENS(114,R_I)=0.0D0
                      ALENS(115,R_I)=0.0D0
                      ALENS(116,R_I)=0.0D0
                      ALENP(114,R_I)=0.0D0
                      ALENP(115,R_I)=0.0D0
                      ALENP(116,R_I)=0.0D0
                      XEEX=ALENS(31,R_I)
                      YEEY=ALENS(30,R_I)
                      ZEEZ=ALENS(69,R_I)
C                       NO DECENTER
                  END IF
C
C       NOW ALIGN THE LOCAL Z AXIS WITH THE +N DIRECTION COSINE
                  IF(DABS(R_M).GE.DABS(((1.0D35)*R_N))) THEN
                      IF(R_M.GE.0.0D0) ANGY=PII/2.0D0
                      IF(R_M.LT.0.0D0) ANGY=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(R_M).EQ.0.0D0.AND.DABS(R_N).EQ.0.0D0) THEN
                          ANGY=0.0D0
                      ELSE
                          ANGY=DATAN2(R_M,R_N)
                      END IF
                      IF(ANGY.LT.0.0D0) ANGY=ANGY+(TWOPII)
                  END IF
                  IF(DABS(R_L).GE.DABS(((1.0D35)*R_N))) THEN
                      IF(R_L.GE.0.0D0) ANGX=PII/2.0D0
                      IF(R_L.LT.0.0D0) ANGX=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(R_L).EQ.0.0D0.AND.DABS(R_N).EQ.0.0D0) THEN
                          ANGX=0.0D0
                      ELSE
                          ANGX=DATAN2(R_L,R_N)
                      END IF
                      IF(ANGX.LT.0.0D0) ANGX=ANGX+(TWOPII)
                  END IF
C       IN DEGREES WE GET
                  ANGY=ANGY*(180.0D0/PII)
                  ANGX=ANGX*(180.0D0/PII)
                  IF(ANGX.GT.90.0D0) ANGX=ANGX-180.0D0
                  IF(ANGY.GT.90.0D0) ANGY=ANGY-180.0D0
                  IF(ANGX.LT.-90.0D0) ANGX=ANGX+180.0D0
                  IF(ANGY.LT.-90.0D0) ANGY=ANGY+180.0D0
                  IF(DABS(ANGX).EQ.360.0D0) ANGX=0.0
                  IF(DABS(ANGY).EQ.360.0D0) ANGY=0.0
C       AN ALPHA ROTATION (IN DEGREES) OF ANGY BRINGS THE Z AXIS
C       INTO ALIGNMENT WITH THE RAY
C       A BETA ROTATION (IN DEGREES) OF -ANGX BRINGS THE
C       X- COMPONENT INTO ALIGNMENT WITH THE Z AXIS.
C       NO GAMMA ROTATION IS REQUIRED
                  ALENS(26,R_I)=+ANGY
                  ALENS(27,R_I)=-ANGX
                  ALENS(27,R_I)=ALENS(27,R_I)+180.0D0
                  ALENS(28,R_I)=0.0D0
                  ALENP(26,R_I)=+ANGY
                  ALENP(27,R_I)=-ANGX
                  ALENP(27,R_I)=ALENP(27,R_I)+180.0D0
                  ALENP(28,R_I)=0.0D0
                  ALENS(118,R_I)=+ANGY
                  ALENS(119,R_I)=-ANGX
                  ALENS(119,R_I)=ALENS(27,R_I)+180.0D0
                  ALENS(120,R_I)=0.0D0
                  ALENP(118,R_I)=+ANGY
                  ALENP(119,R_I)=-ANGX
                  ALENP(119,R_I)=ALENP(27,R_I)+180.0D0
                  ALENP(120,R_I)=0.0D0
                  AEEA=ALENS(26,R_I)
                  BEEB=ALENS(27,R_I)
                  CEEC=ALENS(28,R_I)
                  XEEX=ALENS(31,R_I)
                  YEEY=ALENS(30,R_I)
                  ZEEZ=ALENS(69,R_I)
              END IF
              R_X=OR_X
              R_Y=OR_Y
              R_Z=OR_Z
              R_L=OR_L
              R_M=OR_M
              R_N=OR_N
C
C     NOW PERFORM THE TILTS AND DECENTERS
C     AS IF REGULAR TILT AND DECENTERS PRESENT
              TDECX=XEEX
              TDECY=YEEY
              TDECZ=ZEEZ
              IF(ALENS(29,R_I).EQ.1.0D0) THEN
                  R_X=R_X-TDECX
                  R_Y=R_Y-TDECY
                  R_Z=R_Z-TDECZ
              END IF
              R_Z=R_Z-ALENS(3,(R_I-1))
C
C       NOW ROTATE THE SUCKER!
C
              IF(AEEA.NE.0.0D0) THEN
                  JK_AA=AEEA*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(BEEB.NE.0.0D0) THEN
                  JK_AA=BEEB*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(CEEC.NE.0.0D0) THEN
                  JK_AA=CEEC*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              R_X=R_X-ALENS(90,R_I)
              R_Y=R_Y-ALENS(91,R_I)
              R_Z=R_Z-ALENS(92,R_I)
C
C       NOW ROTATE THE SUCKER!
C
              IF(ALENS(93,R_I).NE.0.0D0) THEN
                  JK_AA=ALENS(93,R_I)*(PII/180.0D0)
                  X1=(R_X*A11(JK_AA))+(R_Y*A12(JK_AA))+(R_Z*A13(JK_AA))
                  Y1=(R_X*A21(JK_AA))+(R_Y*A22(JK_AA))+(R_Z*A23(JK_AA))
                  Z1=(R_X*A31(JK_AA))+(R_Y*A32(JK_AA))+(R_Z*A33(JK_AA))
                  L1=(R_L*A11(JK_AA))+(R_M*A12(JK_AA))+(R_N*A13(JK_AA))
                  M1=(R_L*A21(JK_AA))+(R_M*A22(JK_AA))+(R_N*A23(JK_AA))
                  N1=(R_L*A31(JK_AA))+(R_M*A32(JK_AA))+(R_N*A33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(ALENS(94,R_I).NE.0.0D0) THEN
                  JK_AA=ALENS(94,R_I)*(PII/180.0D0)
                  X1=(R_X*B11(JK_AA))+(R_Y*B12(JK_AA))+(R_Z*B13(JK_AA))
                  Y1=(R_X*B21(JK_AA))+(R_Y*B22(JK_AA))+(R_Z*B23(JK_AA))
                  Z1=(R_X*B31(JK_AA))+(R_Y*B32(JK_AA))+(R_Z*B33(JK_AA))
                  L1=(R_L*B11(JK_AA))+(R_M*B12(JK_AA))+(R_N*B13(JK_AA))
                  M1=(R_L*B21(JK_AA))+(R_M*B22(JK_AA))+(R_N*B23(JK_AA))
                  N1=(R_L*B31(JK_AA))+(R_M*B32(JK_AA))+(R_N*B33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
              IF(ALENS(95,R_I).NE.0.0D0) THEN
                  JK_AA=ALENS(95,R_I)*(PII/180.0D0)
                  X1=(R_X*C11(JK_AA))+(R_Y*C12(JK_AA))+(R_Z*C13(JK_AA))
                  Y1=(R_X*C21(JK_AA))+(R_Y*C22(JK_AA))+(R_Z*C23(JK_AA))
                  Z1=(R_X*C31(JK_AA))+(R_Y*C32(JK_AA))+(R_Z*C33(JK_AA))
                  L1=(R_L*C11(JK_AA))+(R_M*C12(JK_AA))+(R_N*C13(JK_AA))
                  M1=(R_L*C21(JK_AA))+(R_M*C22(JK_AA))+(R_N*C23(JK_AA))
                  N1=(R_L*C31(JK_AA))+(R_M*C32(JK_AA))+(R_N*C33(JK_AA))
                  R_X=X1
                  R_Y=Y1
                  R_Z=Z1
                  R_L=L1
                  R_M=M1
                  R_N=N1
              END IF
          ELSE
C       NOT TILT AUTO OR TILT AUTOM TO HANDLE
          END IF
          RETURN
      END
C SUB GLVERT.FOR
      SUBROUTINE GLVERT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GLVERT.FOR. THIS SUBROUTINE IMPLEMENTS
C       CREATION OF GLOBAL VERTEX DATA
C
          INTEGER JK,I,GSURFF
C
          REAL*8 A,A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33
     3    ,X,Y,Z,TH,AN,TDECX,TDECY,TDECZ,AVALUE,BVALUE,CVALUE
     4    ,X1,Y1,Z1,LX,LY,LZ,MX,MY,MZ,NX,NY,NZ,JLX1,JLY1,JLZ1,JMX1,JMY1,JMZ1
     5    ,X00,Y00,Z0,LX0,LY0,LZ0,JNX1,JNY1,JNZ1
     6    ,MX0,MY0,MZ0,NX0,NY0,NZ0,GDELX,GDELY,GDELZ
     7    ,TLX,TLY,TLZ,TMX,TMY,TMZ,TNX,TNY,TNZ
     8    ,TTLX,TTLY,TTLZ,TTMX,TTMY,TTMZ,TTNX,TTNY,TTNZ
     9    ,AEEA,BEEB,CEEC,XEEX,YEEY,ZEEZ
     1    ,AEEAM,BEEBM,CEECM,XEEXM,YEEYM,ZEEZM

C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     ALENS(25,surf#) -- Tilt flag
C                       0.0=NO TILT (DEFAULT VALUE)
C                       1.0= TILT
C                      -1.0= RTILT
C                       2.0= TILT AUTO
C                       3.0= TILT AUTOM
C                       4.0= TILT BEN
C                       5.0= TILT DAR
C                       6.0= TILT RET
C                       7.0= TILT REV
C     ALENS(26,SURF) TO ALENS(28,SURF) STORE THE ALPHA,BETA AND GAMMA
C                       ANGLES IN DEGREES
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C
C       VERTEX DETERMINATION IS PERFORMED IN TWO STEPS.
C       STEP 1 ASSUMES THAT THE GLOBAL REFERENCE IS SURFACE 0
C       (1 IF ABS(THICKNESS OF 0) GT 1.0D10
C       AND THERE ARE NO OFFSET SHIFTS OF DECENTERS.
C
C       STEP TWO INVOLVES TRANSFORMING USING THE OFFSETS AND
C       SHIFTING TO THE DESIRED GLOBAL REFERENCE SURFACE (NOT THE
C       SAME AS THE "REFERENCE SURFACE".)
C
          IF(DABS(ALENS(3,0)).LE.1.0D10) THEN
C       FINITE OBJECT DISTANCE
              X=0.0D0
              Y=0.0D0
              Z=0.0D0
              VERTEX(1,0)=X
              VERTEX(2,0)=Y
              VERTEX(3,0)=Z
C       THE DIRECTION COSINE OF TH X-AXIS IS:
              LX=1.0D0
              MX=0.0D0
              NX=0.0D0
C       THE DIRECTION COSINE OF TH Y-AXIS IS:
              LY=0.0D0
              MY=1.0D0
              NY=0.0D0
C       THE DIRECTION COSINE OF TH Z-AXIS IS:
              LZ=0.0D0
              MZ=0.0D0
              NZ=1.0D0
              VERTEX(4,0)=LX
              VERTEX(5,0)=MX
              VERTEX(6,0)=NX
              VERTEX(7,0)=LY
              VERTEX(8,0)=MY
              VERTEX(9,0)=NY
              VERTEX(10,0)=LZ
              VERTEX(11,0)=MZ
              VERTEX(12,0)=NZ
              GSURFF=0
              JK=1
          ELSE
C       INFINITE OBJECT THICKNESS
              X=0.0D0
              Y=0.0D0
              Z=0.0D0
              VERTEX(1,0)=X
              VERTEX(2,0)=Y
              VERTEX(3,0)=Z
              VERTEX(1,1)=X
              VERTEX(2,1)=Y
              VERTEX(3,1)=Z
C       THE DIRECTION COSINE OF TH X-AXIS IS:
              LX=1.0D0
              MX=0.0D0
              NX=0.0D0
C       THE DIRECTION COSINE OF TH Y-AXIS IS:
              LY=0.0D0
              MY=1.0D0
              NY=0.0D0
C       THE DIRECTION COSINE OF TH Z-AXIS IS:
              LZ=0.0D0
              MZ=0.0D0
              NZ=1.0D0
              VERTEX(4,0)=LX
              VERTEX(5,0)=MX
              VERTEX(6,0)=NX
              VERTEX(7,0)=LY
              VERTEX(8,0)=MY
              VERTEX(9,0)=NY
              VERTEX(10,0)=LZ
              VERTEX(11,0)=MZ
              VERTEX(12,0)=NZ
              VERTEX(4,1)=LX
              VERTEX(5,1)=MX
              VERTEX(6,1)=NX
              VERTEX(7,1)=LY
              VERTEX(8,1)=MY
              VERTEX(9,1)=NY
              VERTEX(10,1)=LZ
              VERTEX(11,1)=MZ
              VERTEX(12,1)=NZ
              GSURFF=1
              JK=0+2
          END IF
C       NOW PROCEED THROUGH THE REST OF THE SURFACES.
C
          DO 10 I=JK,NEWIMG
              AEEA=ALENS(26,I)
              BEEB=ALENS(27,I)
              CEEC=ALENS(28,I)
              XEEX=ALENS(31,I)
              YEEY=ALENS(30,I)
              ZEEZ=ALENS(69,I)
              AEEAM=ALENS(26,I-1)
              BEEBM=ALENS(27,I-1)
              CEECM=ALENS(28,I-1)
              XEEXM=ALENS(31,I-1)
              YEEYM=ALENS(30,I-1)
              ZEEZM=ALENS(69,I-1)

              IF(ALENS(25,I).EQ.7.0D0) THEN
C     SURFACE HAS A TILT REV
C
C     CODE GOES HERE IF DAR OR BEN WAS ON JK-1
C
                  IF(ALENS(25,I-1).EQ.4.0D0) THEN
C     TILT BEN, THEN APPLY A TILT AT SURFACE I-1
                      TLX=1.0D0
                      TMX=0.0D0
                      TNX=0.0D0
                      TLY=0.0D0
                      TMY=1.0D0
                      TNY=0.0D0
                      TLZ=0.0D0
                      TMZ=0.0D0
                      TNZ=1.0D0
C       NOW ROTATE THE LOCAL X,Y AND Z AXES BY ALPHA, BETA THEN GAMMA
                      IF(CEECM.NE.0.0D0) THEN
                          A=-CEECM*(PII/180.0D0)
                          JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                          JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                          JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                          JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                          JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                          JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                          JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                          JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                          JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(BEEBM.NE.0.0D0) THEN
                          A=-BEEBM*(PII/180.0D0)
                          JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                          JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                          JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                          JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                          JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                          JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                          JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                          JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                          JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(AEEAM.NE.0.0D0) THEN
                          A=-AEEAM*(PII/180.0D0)
                          JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                          JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                          JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                          JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                          JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                          JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                          JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                          JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                          JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
C     NOW THE DIR COSINES ARE IN THE LOCAL COORDINATES SYSTEM OF
C     SURFACE I-1. TRANSFORM THEM TO THE GLOBAL COORDINATE SYSTEM
                      TTLX=LX
                      TTMX=MX
                      TTNX=NX
                      TTLY=LY
                      TTMY=MY
                      TTNY=NY
                      TTLZ=LZ
                      TTMZ=MZ
                      TTNZ=NZ
                      LX=(TLX*TTLX)+(TMX*TTLY)+(TNX*TTLZ)
                      MX=(TLX*TTMX)+(TMX*TTMY)+(TNX*TTMZ)
                      NX=(TLX*TTNX)+(TMX*TTNY)+(TNX*TTNZ)
                      LY=(TLY*TTLX)+(TMY*TTLY)+(TNY*TTLZ)
                      MY=(TLY*TTMX)+(TMY*TTMY)+(TNY*TTMZ)
                      NY=(TLY*TTNX)+(TMY*TTNY)+(TNY*TTNZ)
                      LZ=(TLZ*TTLX)+(TMZ*TTLY)+(TNZ*TTLZ)
                      MZ=(TLZ*TTMX)+(TMZ*TTMY)+(TNZ*TTMZ)
                      NZ=(TLZ*TTNX)+(TMZ*TTNY)+(TNZ*TTNZ)
C     APPLY THEM HOWEVER TO SURFACE I
                      VERTEX(4,I)=LX
                      VERTEX(5,I)=MX
                      VERTEX(6,I)=NX
                      VERTEX(7,I)=LY
                      VERTEX(8,I)=MY
                      VERTEX(9,I)=NY
                      VERTEX(10,I)=LZ
                      VERTEX(11,I)=MZ
                      VERTEX(12,I)=NZ
C     DONE WITH PRECALC FOR TILT BEN
                  END IF
                  IF(ALENS(25,I-1).EQ.5.0D0
     1            .OR.ALENS(25,I-1).EQ.7.0D0)THEN
C       DO TILT DAR, TILT BEN
                      TLX=1.0D0
                      TMX=0.0D0
                      TNX=0.0D0
                      TLY=0.0D0
                      TMY=1.0D0
                      TNY=0.0D0
                      TLZ=0.0D0
                      TMZ=0.0D0
                      TNZ=1.0D0
C       NOW ROTATE THE LOCAL X,Y AND Z AXES BY -GAMMA, -BETA AND -ALPHA
                      IF(AEEAM.NE.0.0D0) THEN
                          A=AEEAM*(PII/180.0D0)
                          JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                          JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                          JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                          JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                          JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                          JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                          JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                          JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                          JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(BEEBM.NE.0.0D0) THEN
                          A=BEEBM*(PII/180.0D0)
                          JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                          JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                          JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                          JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                          JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                          JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                          JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                          JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                          JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(CEECM.NE.0.0D0) THEN
                          A=CEECM*(PII/180.0D0)
                          JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                          JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                          JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                          JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                          JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                          JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                          JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                          JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                          JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
C     NOW THE DIR COSINES ARE IN THE LOCAL COORDINATES SYSTEM OF
C     SURFACE I. TRANSFORM THEM TO THE GLOBAL COORDINATE SYSTEM
                      TTLX=LX
                      TTMX=MX
                      TTNX=NX
                      TTLY=LY
                      TTMY=MY
                      TTNY=NY
                      TTLZ=LZ
                      TTMZ=MZ
                      TTNZ=NZ
                      LX=(TLX*TTLX)+(TMX*TTLY)+(TNX*TTLZ)
                      MX=(TLX*TTMX)+(TMX*TTMY)+(TNX*TTMZ)
                      NX=(TLX*TTNX)+(TMX*TTNY)+(TNX*TTNZ)
                      LY=(TLY*TTLX)+(TMY*TTLY)+(TNY*TTLZ)
                      MY=(TLY*TTMX)+(TMY*TTMY)+(TNY*TTMZ)
                      NY=(TLY*TTNX)+(TMY*TTNY)+(TNY*TTNZ)
                      LZ=(TLZ*TTLX)+(TMZ*TTLY)+(TNZ*TTLZ)
                      MZ=(TLZ*TTMX)+(TMZ*TTMY)+(TNZ*TTMZ)
                      NZ=(TLZ*TTNX)+(TMZ*TTNY)+(TNZ*TTNZ)
                      VERTEX(4,I)=LX
                      VERTEX(5,I)=MX
                      VERTEX(6,I)=NX
                      VERTEX(7,I)=LY
                      VERTEX(8,I)=MY
                      VERTEX(9,I)=NY
                      VERTEX(10,I)=LZ
                      VERTEX(11,I)=MZ
                      VERTEX(12,I)=NZ
                      GDELX=-XEEXM
                      GDELY=-YEEYM
                      GDELZ=-ZEEZM
                      TH=GDELZ
                      X=X+(TH*LZ)+(GDELX*LX)+(GDELY*LY)
                      Y=Y+(TH*MZ)+(GDELX*MX)+(GDELY*MY)
                      Z=Z+(TH*NZ)+(GDELX*NX)+(GDELY*NY)
                      VERTEX(1,I)=X
                      VERTEX(2,I)=Y
                      VERTEX(3,I)=Z
C     DONE WITH PRECALC FOR TILT DAR
                  END IF
C     THIS FOR TYPE 7 ONLY
                  GDELX=0.0D0
                  GDELY=0.0D0
                  GDELZ=0.0D0
                  TH=ALENS(3,I-1)+GDELZ
                  X=X+(TH*LZ)+(GDELX*LX)+(GDELY*LY)
                  Y=Y+(TH*MZ)+(GDELX*MX)+(GDELY*MY)
                  Z=Z+(TH*NZ)+(GDELX*NX)+(GDELY*NY)
                  VERTEX(1,I)=X
                  VERTEX(2,I)=Y
                  VERTEX(3,I)=Z
                  VERTEX(4,I)=LX
                  VERTEX(5,I)=MX
                  VERTEX(6,I)=NX
                  VERTEX(7,I)=LY
                  VERTEX(8,I)=MY
                  VERTEX(9,I)=NY
                  VERTEX(10,I)=LZ
                  VERTEX(11,I)=MZ
                  VERTEX(12,I)=NZ
C       FINISHED WITH TILT REV ON SURFACE I
              END IF
C
              IF(ALENS(25,I).NE.-1.0D0
     1        .AND.ALENS(25,I).NE.7.0D0) THEN
C       THIS DOES EVERYTHING EXCEPT TILT REV AND RTILT
C
C     CODE GOES HERE IF DAR OR BEN WAS ON JK-1
C
                  IF(ALENS(25,I-1).EQ.4.0D0) THEN
C     TILT BEN, THEN APPLY A TILT AT SURFACE I-1
                      TLX=1.0D0
                      TMX=0.0D0
                      TNX=0.0D0
                      TLY=0.0D0
                      TMY=1.0D0
                      TNY=0.0D0
                      TLZ=0.0D0
                      TMZ=0.0D0
                      TNZ=1.0D0
C       NOW ROTATE THE LOCAL X,Y AND Z AXES BY ALPHA, BETA THEN GAMMA
                      IF(CEECM.NE.0.0D0) THEN
                          A=-CEECM*(PII/180.0D0)
                          JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                          JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                          JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                          JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                          JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                          JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                          JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                          JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                          JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(BEEBM.NE.0.0D0) THEN
                          A=-BEEBM*(PII/180.0D0)
                          JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                          JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                          JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                          JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                          JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                          JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                          JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                          JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                          JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(AEEAM.NE.0.0D0) THEN
                          A=-AEEAM*(PII/180.0D0)
                          JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                          JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                          JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                          JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                          JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                          JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                          JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                          JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                          JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
C     NOW THE DIR COSINES ARE IN THE LOCAL COORDINATES SYSTEM OF
C     SURFACE I-1. TRANSFORM THEM TO THE GLOBAL COORDINATE SYSTEM
                      TTLX=LX
                      TTMX=MX
                      TTNX=NX
                      TTLY=LY
                      TTMY=MY
                      TTNY=NY
                      TTLZ=LZ
                      TTMZ=MZ
                      TTNZ=NZ
                      LX=(TLX*TTLX)+(TMX*TTLY)+(TNX*TTLZ)
                      MX=(TLX*TTMX)+(TMX*TTMY)+(TNX*TTMZ)
                      NX=(TLX*TTNX)+(TMX*TTNY)+(TNX*TTNZ)
                      LY=(TLY*TTLX)+(TMY*TTLY)+(TNY*TTLZ)
                      MY=(TLY*TTMX)+(TMY*TTMY)+(TNY*TTMZ)
                      NY=(TLY*TTNX)+(TMY*TTNY)+(TNY*TTNZ)
                      LZ=(TLZ*TTLX)+(TMZ*TTLY)+(TNZ*TTLZ)
                      MZ=(TLZ*TTMX)+(TMZ*TTMY)+(TNZ*TTMZ)
                      NZ=(TLZ*TTNX)+(TMZ*TTNY)+(TNZ*TTNZ)
C     APPLY THEM HOWEVER TO SURFACE I
                      VERTEX(4,I)=LX
                      VERTEX(5,I)=MX
                      VERTEX(6,I)=NX
                      VERTEX(7,I)=LY
                      VERTEX(8,I)=MY
                      VERTEX(9,I)=NY
                      VERTEX(10,I)=LZ
                      VERTEX(11,I)=MZ
                      VERTEX(12,I)=NZ
C     DONE WITH PRECALC FOR TILT BEN
                  END IF
                  IF(ALENS(25,I-1).EQ.5.0D0
     1            .OR.ALENS(25,I-1).EQ.7.0D0) THEN
C       TILT DAR, TILT REV
                      TLX=1.0D0
                      TMX=0.0D0
                      TNX=0.0D0
                      TLY=0.0D0
                      TMY=1.0D0
                      TNY=0.0D0
                      TLZ=0.0D0
                      TMZ=0.0D0
                      TNZ=1.0D0
C       NOW ROTATE THE LOCAL X,Y AND Z AXES BY -GAMMA, -BETA AND -ALPHA
                      IF(AEEAM.NE.0.0D0) THEN
                          A=AEEAM*(PII/180.0D0)
                          JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                          JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                          JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                          JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                          JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                          JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                          JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                          JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                          JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(BEEBM.NE.0.0D0) THEN
                          A=BEEBM*(PII/180.0D0)
                          JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                          JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                          JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                          JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                          JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                          JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                          JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                          JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                          JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(CEECM.NE.0.0D0) THEN
                          A=CEECM*(PII/180.0D0)
                          JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                          JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                          JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                          JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                          JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                          JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                          JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                          JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                          JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
C     NOW THE DIR COSINES ARE IN THE LOCAL COORDINATES SYSTEM OF
C     SURFACE I. TRANSFORM THEM TO THE GLOBAL COORDINATE SYSTEM
                      TTLX=LX
                      TTMX=MX
                      TTNX=NX
                      TTLY=LY
                      TTMY=MY
                      TTNY=NY
                      TTLZ=LZ
                      TTMZ=MZ
                      TTNZ=NZ
                      LX=(TLX*TTLX)+(TMX*TTLY)+(TNX*TTLZ)
                      MX=(TLX*TTMX)+(TMX*TTMY)+(TNX*TTMZ)
                      NX=(TLX*TTNX)+(TMX*TTNY)+(TNX*TTNZ)
                      LY=(TLY*TTLX)+(TMY*TTLY)+(TNY*TTLZ)
                      MY=(TLY*TTMX)+(TMY*TTMY)+(TNY*TTMZ)
                      NY=(TLY*TTNX)+(TMY*TTNY)+(TNY*TTNZ)
                      LZ=(TLZ*TTLX)+(TMZ*TTLY)+(TNZ*TTLZ)
                      MZ=(TLZ*TTMX)+(TMZ*TTMY)+(TNZ*TTMZ)
                      NZ=(TLZ*TTNX)+(TMZ*TTNY)+(TNZ*TTNZ)
                      VERTEX(4,I)=LX
                      VERTEX(5,I)=MX
                      VERTEX(6,I)=NX
                      VERTEX(7,I)=LY
                      VERTEX(8,I)=MY
                      VERTEX(9,I)=NY
                      VERTEX(10,I)=LZ
                      VERTEX(11,I)=MZ
                      VERTEX(12,I)=NZ
                      GDELX=-XEEXM
                      GDELY=-YEEYM
                      GDELZ=-ZEEZM
                      TH=GDELZ
                      X=X+(TH*LZ)+(GDELX*LX)+(GDELY*LY)
                      Y=Y+(TH*MZ)+(GDELX*MX)+(GDELY*MY)
                      Z=Z+(TH*NZ)+(GDELX*NX)+(GDELY*NY)
                      VERTEX(1,I)=X
                      VERTEX(2,I)=Y
                      VERTEX(3,I)=Z
C     DONE WITH PRECALC FOR TILT DAR
                  END IF
C     GO TO THE VERTEX BEFORE THE RTILT
                  TH=ALENS(3,I-1)
                  X=X+(TH*LZ)
                  Y=Y+(TH*MZ)
                  Z=Z+(TH*NZ)
                  TDECX=XEEX
                  TDECY=YEEY
                  TDECZ=ZEEZ
                  GDELX=TDECX
                  GDELY=TDECY
                  GDELZ=TDECZ
                  TH=GDELZ
                  X=X+(TH*LZ)+(GDELX*LX)+(GDELY*LY)
                  Y=Y+(TH*MZ)+(GDELX*MX)+(GDELY*MY)
                  Z=Z+(TH*NZ)+(GDELX*NX)+(GDELY*NY)
                  VERTEX(1,I)=X
                  VERTEX(2,I)=Y
                  VERTEX(3,I)=Z
C       DO REVERSE TILT THEN REVERSE DECENTER
C
                  TLX=1.0D0
                  TMX=0.0D0
                  TNX=0.0D0
                  TLY=0.0D0
                  TMY=1.0D0
                  TNY=0.0D0
                  TLZ=0.0D0
                  TMZ=0.0D0
                  TNZ=1.0D0
C       NOW ROTATE THE LOCAL X,Y AND Z AXES BY ALPHA, BETA THEN GAMMA
                  IF(CEEC.NE.0.0D0) THEN
                      A=-CEEC*(PII/180.0D0)
                      JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                      JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                      JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                      JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                      JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                      JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                      JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                      JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                      JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                  END IF
                  IF(BEEB.NE.0.0D0) THEN
                      A=-BEEB*(PII/180.0D0)
                      JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                      JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                      JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                      JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                      JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                      JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                      JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                      JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                      JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                  END IF
                  IF(AEEA.NE.0.0D0) THEN
                      A=-AEEA*(PII/180.0D0)
                      JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                      JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                      JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                      JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                      JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                      JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                      JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                      JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                      JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                  END IF
C     NOW THE DIR COSINES ARE IN THE LOCAL COORDINATES SYSTEM OF
C     SURFACE I. TRANSFORM THEM TO THE GLOBAL COORDINATE SYSTEM
                  TTLX=LX
                  TTMX=MX
                  TTNX=NX
                  TTLY=LY
                  TTMY=MY
                  TTNY=NY
                  TTLZ=LZ
                  TTMZ=MZ
                  TTNZ=NZ
                  LX=(TLX*TTLX)+(TMX*TTLY)+(TNX*TTLZ)
                  MX=(TLX*TTMX)+(TMX*TTMY)+(TNX*TTMZ)
                  NX=(TLX*TTNX)+(TMX*TTNY)+(TNX*TTNZ)
                  LY=(TLY*TTLX)+(TMY*TTLY)+(TNY*TTLZ)
                  MY=(TLY*TTMX)+(TMY*TTMY)+(TNY*TTMZ)
                  NY=(TLY*TTNX)+(TMY*TTNY)+(TNY*TTNZ)
                  LZ=(TLZ*TTLX)+(TMZ*TTLY)+(TNZ*TTLZ)
                  MZ=(TLZ*TTMX)+(TMZ*TTMY)+(TNZ*TTMZ)
                  NZ=(TLZ*TTNX)+(TMZ*TTNY)+(TNZ*TTNZ)
                  VERTEX(4,I)=LX
                  VERTEX(5,I)=MX
                  VERTEX(6,I)=NX
                  VERTEX(7,I)=LY
                  VERTEX(8,I)=MY
                  VERTEX(9,I)=NY
                  VERTEX(10,I)=LZ
                  VERTEX(11,I)=MZ
                  VERTEX(12,I)=NZ
              END IF
C
              IF(ALENS(25,I).EQ.1.0D0) THEN
C       REGULAT TILT
                  TLX=1.0D0
                  TMX=0.0D0
                  TNX=0.0D0
                  TLY=0.0D0
                  TMY=1.0D0
                  TNY=0.0D0
                  TLZ=0.0D0
                  TMZ=0.0D0
                  TNZ=1.0D0
                  GDELX=ALENS(90,I)
                  GDELY=ALENS(91,I)
                  GDELZ=ALENS(92,I)
                  X=X+(GDELZ*TLZ)+(GDELX*TLX)+(GDELY*TLY)
                  Y=Y+(GDELZ*TMZ)+(GDELX*TMX)+(GDELY*TMY)
                  Z=Z+(GDELZ*TNZ)+(GDELX*TNX)+(GDELY*TNY)
                  VERTEX(1,I)=X
                  VERTEX(2,I)=Y
                  VERTEX(3,I)=Z
C       DO REVERSE TILT THEN REVERSE DECENTER
C
C       NOW ROTATE THE LOCAL X,Y AND Z AXES BY ALPHA, BETA THEN GAMMA
                  IF(ALENS(95,I).NE.0.0D0) THEN
                      A=-ALENS(95,I)*(PII/180.0D0)
                      JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                      JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                      JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                      JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                      JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                      JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                      JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                      JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                      JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                  END IF
                  IF(ALENS(94,I).NE.0.0D0) THEN
                      A=-ALENS(94,I)*(PII/180.0D0)
                      JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                      JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                      JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                      JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                      JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                      JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                      JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                      JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                      JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                  END IF
                  IF(ALENS(93,I).NE.0.0D0) THEN
                      A=-ALENS(93,I)*(PII/180.0D0)
                      JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                      JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                      JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                      JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                      JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                      JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                      JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                      JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                      JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                  END IF
C     NOW THE DIR COSINES ARE IN THE LOCAL COORDINATES SYSTEM OF
C     SURFACE I. TRANSFORM THEM TO THE GLOBAL COORDINATE SYSTEM
                  TTLX=LX
                  TTMX=MX
                  TTNX=NX
                  TTLY=LY
                  TTMY=MY
                  TTNY=NY
                  TTLZ=LZ
                  TTMZ=MZ
                  TTNZ=NZ
                  LX=(TLX*TTLX)+(TMX*TTLY)+(TNX*TTLZ)
                  MX=(TLX*TTMX)+(TMX*TTMY)+(TNX*TTMZ)
                  NX=(TLX*TTNX)+(TMX*TTNY)+(TNX*TTNZ)
                  LY=(TLY*TTLX)+(TMY*TTLY)+(TNY*TTLZ)
                  MY=(TLY*TTMX)+(TMY*TTMY)+(TNY*TTMZ)
                  NY=(TLY*TTNX)+(TMY*TTNY)+(TNY*TTNZ)
                  LZ=(TLZ*TTLX)+(TMZ*TTLY)+(TNZ*TTLZ)
                  MZ=(TLZ*TTMX)+(TMZ*TTMY)+(TNZ*TTMZ)
                  NZ=(TLZ*TTNX)+(TMZ*TTNY)+(TNZ*TTNZ)
                  VERTEX(4,I)=LX
                  VERTEX(5,I)=MX
                  VERTEX(6,I)=NX
                  VERTEX(7,I)=LY
                  VERTEX(8,I)=MY
                  VERTEX(9,I)=NY
                  VERTEX(10,I)=LZ
                  VERTEX(11,I)=MZ
                  VERTEX(12,I)=NZ
              END IF
              IF(ALENS(25,I).EQ.-1.0D0) THEN
C     RTILT DONE HERE
C
C     CODE GOES HERE IF DAR OR BEN WAS ON JK-1
                  IF(ALENS(25,I-1).EQ.4.0D0) THEN
C     TILT BEN, THEN APPLY A TILT AT SURFACE I-1
                      TLX=1.0D0
                      TMX=0.0D0
                      TNX=0.0D0
                      TLY=0.0D0
                      TMY=1.0D0
                      TNY=0.0D0
                      TLZ=0.0D0
                      TMZ=0.0D0
                      TNZ=1.0D0
C       NOW ROTATE THE LOCAL X,Y AND Z AXES BY ALPHA, BETA THEN GAMMA
                      IF(CEECM.NE.0.0D0) THEN
                          A=-CEECM*(PII/180.0D0)
                          JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                          JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                          JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                          JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                          JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                          JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                          JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                          JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                          JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(BEEBM.NE.0.0D0) THEN
                          A=-BEEBM*(PII/180.0D0)
                          JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                          JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                          JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                          JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                          JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                          JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                          JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                          JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                          JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(AEEAM.NE.0.0D0) THEN
                          A=-AEEAM*(PII/180.0D0)
                          JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                          JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                          JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                          JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                          JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                          JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                          JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                          JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                          JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
C     NOW THE DIR COSINES ARE IN THE LOCAL COORDINATES SYSTEM OF
C     SURFACE I-1. TRANSFORM THEM TO THE GLOBAL COORDINATE SYSTEM
                      TTLX=LX
                      TTMX=MX
                      TTNX=NX
                      TTLY=LY
                      TTMY=MY
                      TTNY=NY
                      TTLZ=LZ
                      TTMZ=MZ
                      TTNZ=NZ
                      LX=(TLX*TTLX)+(TMX*TTLY)+(TNX*TTLZ)
                      MX=(TLX*TTMX)+(TMX*TTMY)+(TNX*TTMZ)
                      NX=(TLX*TTNX)+(TMX*TTNY)+(TNX*TTNZ)
                      LY=(TLY*TTLX)+(TMY*TTLY)+(TNY*TTLZ)
                      MY=(TLY*TTMX)+(TMY*TTMY)+(TNY*TTMZ)
                      NY=(TLY*TTNX)+(TMY*TTNY)+(TNY*TTNZ)
                      LZ=(TLZ*TTLX)+(TMZ*TTLY)+(TNZ*TTLZ)
                      MZ=(TLZ*TTMX)+(TMZ*TTMY)+(TNZ*TTMZ)
                      NZ=(TLZ*TTNX)+(TMZ*TTNY)+(TNZ*TTNZ)
C     APPLY THEM HOWEVER TO SURFACE I
                      VERTEX(4,I)=LX
                      VERTEX(5,I)=MX
                      VERTEX(6,I)=NX
                      VERTEX(7,I)=LY
                      VERTEX(8,I)=MY
                      VERTEX(9,I)=NY
                      VERTEX(10,I)=LZ
                      VERTEX(11,I)=MZ
                      VERTEX(12,I)=NZ
C     DONE WITH PRECALC FOR RTILT BEN
                  END IF
                  IF(ALENS(25,I-1).EQ.5.0D0
     1            .OR.ALENS(25,I-1).EQ.7.0D0) THEN
C       TILT DAR, TILT REV
                      TLX=1.0D0
                      TMX=0.0D0
                      TNX=0.0D0
                      TLY=0.0D0
                      TMY=1.0D0
                      TNY=0.0D0
                      TLZ=0.0D0
                      TMZ=0.0D0
                      TNZ=1.0D0
C       NOW ROTATE THE LOCAL X,Y AND Z AXES BY -GAMMA, -BETA AND -ALPHA
                      IF(AEEAM.NE.0.0D0) THEN
                          A=AEEAM*(PII/180.0D0)
                          JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                          JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                          JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                          JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                          JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                          JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                          JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                          JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                          JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(BEEBM.NE.0.0D0) THEN
                          A=BEEBM*(PII/180.0D0)
                          JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                          JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                          JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                          JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                          JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                          JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                          JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                          JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                          JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
                      IF(CEECM.NE.0.0D0) THEN
                          A=CEECM*(PII/180.0D0)
                          JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                          JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                          JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                          JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                          JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                          JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                          JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                          JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                          JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                          TLX=JLX1
                          TMX=JMX1
                          TNX=JNX1
                          TLY=JLY1
                          TMY=JMY1
                          TNY=JNY1
                          TLZ=JLZ1
                          TMZ=JMZ1
                          TNZ=JNZ1
                      END IF
C     NOW THE DIR COSINES ARE IN THE LOCAL COORDINATES SYSTEM OF
C     SURFACE I. TRANSFORM THEM TO THE GLOBAL COORDINATE SYSTEM
                      TTLX=LX
                      TTMX=MX
                      TTNX=NX
                      TTLY=LY
                      TTMY=MY
                      TTNY=NY
                      TTLZ=LZ
                      TTMZ=MZ
                      TTNZ=NZ
                      LX=(TLX*TTLX)+(TMX*TTLY)+(TNX*TTLZ)
                      MX=(TLX*TTMX)+(TMX*TTMY)+(TNX*TTMZ)
                      NX=(TLX*TTNX)+(TMX*TTNY)+(TNX*TTNZ)
                      LY=(TLY*TTLX)+(TMY*TTLY)+(TNY*TTLZ)
                      MY=(TLY*TTMX)+(TMY*TTMY)+(TNY*TTMZ)
                      NY=(TLY*TTNX)+(TMY*TTNY)+(TNY*TTNZ)
                      LZ=(TLZ*TTLX)+(TMZ*TTLY)+(TNZ*TTLZ)
                      MZ=(TLZ*TTMX)+(TMZ*TTMY)+(TNZ*TTMZ)
                      NZ=(TLZ*TTNX)+(TMZ*TTNY)+(TNZ*TTNZ)
                      VERTEX(4,I)=LX
                      VERTEX(5,I)=MX
                      VERTEX(6,I)=NX
                      VERTEX(7,I)=LY
                      VERTEX(8,I)=MY
                      VERTEX(9,I)=NY
                      VERTEX(10,I)=LZ
                      VERTEX(11,I)=MZ
                      VERTEX(12,I)=NZ
                      GDELX=-XEEXM
                      GDELY=-YEEYM
                      GDELZ=-ZEEZM
                      TH=GDELZ
                      X=X+(TH*LZ)+(GDELX*LX)+(GDELY*LY)
                      Y=Y+(TH*MZ)+(GDELX*MX)+(GDELY*MY)
                      Z=Z+(TH*NZ)+(GDELX*NX)+(GDELY*NY)
                      VERTEX(1,I)=X
                      VERTEX(2,I)=Y
                      VERTEX(3,I)=Z
C     DONE WITH PRECALC FOR TILT DAR
                  END IF
C
C     GO TO THE VERTEX BEFORE THE RTILT
                  TH=ALENS(3,I-1)
                  X=X+(TH*LZ)
                  Y=Y+(TH*MZ)
                  Z=Z+(TH*NZ)
C
C       DO REVERSE TILT THEN REVERSE DECENTER
C       REVERSE ROTATE
C       NOW ROTATE
C
                  TLX=1.0D0
                  TMX=0.0D0
                  TNX=0.0D0
                  TLY=0.0D0
                  TMY=1.0D0
                  TNY=0.0D0
                  TLZ=0.0D0
                  TMZ=0.0D0
                  TNZ=1.0D0
C       NOW ROTATE THE LOCAL X,Y AND Z AXES BY -GAMMA, -BETA AND -ALPHA
                  IF(AEEA.NE.0.0D0) THEN
                      A=AEEA*(PII/180.0D0)
                      JLX1=(TLX*A11(A))+(TMX*A12(A))+(TNX*A13(A))
                      JMX1=(TLX*A21(A))+(TMX*A22(A))+(TNX*A23(A))
                      JNX1=(TLX*A31(A))+(TMX*A32(A))+(TNX*A33(A))
                      JLY1=(TLY*A11(A))+(TMY*A12(A))+(TNY*A13(A))
                      JMY1=(TLY*A21(A))+(TMY*A22(A))+(TNY*A23(A))
                      JNY1=(TLY*A31(A))+(TMY*A32(A))+(TNY*A33(A))
                      JLZ1=(TLZ*A11(A))+(TMZ*A12(A))+(TNZ*A13(A))
                      JMZ1=(TLZ*A21(A))+(TMZ*A22(A))+(TNZ*A23(A))
                      JNZ1=(TLZ*A31(A))+(TMZ*A32(A))+(TNZ*A33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                  END IF
                  IF(BEEB.NE.0.0D0) THEN
                      A=BEEB*(PII/180.0D0)
                      JLX1=(TLX*B11(A))+(TMX*B12(A))+(TNX*B13(A))
                      JMX1=(TLX*B21(A))+(TMX*B22(A))+(TNX*B23(A))
                      JNX1=(TLX*B31(A))+(TMX*B32(A))+(TNX*B33(A))
                      JLY1=(TLY*B11(A))+(TMY*B12(A))+(TNY*B13(A))
                      JMY1=(TLY*B21(A))+(TMY*B22(A))+(TNY*B23(A))
                      JNY1=(TLY*B31(A))+(TMY*B32(A))+(TNY*B33(A))
                      JLZ1=(TLZ*B11(A))+(TMZ*B12(A))+(TNZ*B13(A))
                      JMZ1=(TLZ*B21(A))+(TMZ*B22(A))+(TNZ*B23(A))
                      JNZ1=(TLZ*B31(A))+(TMZ*B32(A))+(TNZ*B33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                  END IF
                  IF(CEEC.NE.0.0D0) THEN
                      A=CEEC*(PII/180.0D0)
                      JLX1=(TLX*C11(A))+(TMX*C12(A))+(TNX*C13(A))
                      JMX1=(TLX*C21(A))+(TMX*C22(A))+(TNX*C23(A))
                      JNX1=(TLX*C31(A))+(TMX*C32(A))+(TNX*C33(A))
                      JLY1=(TLY*C11(A))+(TMY*C12(A))+(TNY*C13(A))
                      JMY1=(TLY*C21(A))+(TMY*C22(A))+(TNY*C23(A))
                      JNY1=(TLY*C31(A))+(TMY*C32(A))+(TNY*C33(A))
                      JLZ1=(TLZ*C11(A))+(TMZ*C12(A))+(TNZ*C13(A))
                      JMZ1=(TLZ*C21(A))+(TMZ*C22(A))+(TNZ*C23(A))
                      JNZ1=(TLZ*C31(A))+(TMZ*C32(A))+(TNZ*C33(A))
                      TLX=JLX1
                      TMX=JMX1
                      TNX=JNX1
                      TLY=JLY1
                      TMY=JMY1
                      TNY=JNY1
                      TLZ=JLZ1
                      TMZ=JMZ1
                      TNZ=JNZ1
                  END IF
C     NOW THE DIR COSINES ARE IN THE LOCAL COORDINATES SYSTEM OF
C     SURFACE I. TRANSFORM THEM TO THE GLOBAL COORDINATE SYSTEM
                  TTLX=LX
                  TTMX=MX
                  TTNX=NX
                  TTLY=LY
                  TTMY=MY
                  TTNY=NY
                  TTLZ=LZ
                  TTMZ=MZ
                  TTNZ=NZ
                  LX=(TLX*TTLX)+(TMX*TTLY)+(TNX*TTLZ)
                  MX=(TLX*TTMX)+(TMX*TTMY)+(TNX*TTMZ)
                  NX=(TLX*TTNX)+(TMX*TTNY)+(TNX*TTNZ)
                  LY=(TLY*TTLX)+(TMY*TTLY)+(TNY*TTLZ)
                  MY=(TLY*TTMX)+(TMY*TTMY)+(TNY*TTMZ)
                  NY=(TLY*TTNX)+(TMY*TTNY)+(TNY*TTNZ)
                  LZ=(TLZ*TTLX)+(TMZ*TTLY)+(TNZ*TTLZ)
                  MZ=(TLZ*TTMX)+(TMZ*TTMY)+(TNZ*TTMZ)
                  NZ=(TLZ*TTNX)+(TMZ*TTNY)+(TNZ*TTNZ)
                  VERTEX(4,I)=LX
                  VERTEX(5,I)=MX
                  VERTEX(6,I)=NX
                  VERTEX(7,I)=LY
                  VERTEX(8,I)=MY
                  VERTEX(9,I)=NY
                  VERTEX(10,I)=LZ
                  VERTEX(11,I)=MZ
                  VERTEX(12,I)=NZ
                  GDELX=XEEX
                  GDELY=YEEY
                  GDELZ=ZEEZ
                  TH=GDELZ
                  X=X-(TH*LZ)-(GDELX*LX)-(GDELY*LY)
                  Y=Y-(TH*MZ)-(GDELX*MX)-(GDELY*MY)
                  Z=Z-(TH*NZ)-(GDELX*NX)-(GDELY*NY)
                  VERTEX(1,I)=X
                  VERTEX(2,I)=Y
                  VERTEX(3,I)=Z
              END IF
 10       CONTINUE
C
C       THE DATA IN THE VERTEX ARRAY REPRESENTS GLOBAL
C       COORDINATES OF ALL SURFACE VERTICIES WITH RESPECT
C       TO THE LOCAL COORDINATE SYSTEM OF SURFACE 0.
C
C       NOW TRANSFORM TO THE COORDINATE SYSTEM CENTERED
C       AT THE VERTEX OF SURFACE GLSURF.
C       THE VERTEX COORDINATES AND DIR COS OF THE GLSURF
C       VERTEX ARE:
C

          IF(GLSURF.NE.-99) THEN
C       GLOBAL RAY TRACING IS EXPLICITLY TURNED "ON"
              GSURFF=GLSURF
C
              X00=VERTEX(1,GLSURF)
              Y00=VERTEX(2,GLSURF)
              Z0=VERTEX(3,GLSURF)
              LX0=VERTEX(4,GLSURF)
              MX0=VERTEX(5,GLSURF)
              NX0=VERTEX(6,GLSURF)
              LY0=VERTEX(7,GLSURF)
              MY0=VERTEX(8,GLSURF)
              NY0=VERTEX(9,GLSURF)
              LZ0=VERTEX(10,GLSURF)
              MZ0=VERTEX(11,GLSURF)
              NZ0=VERTEX(12,GLSURF)
C
              IF(DABS(ALENS(3,0)).GT.1.0D10) JK=1
              IF(DABS(ALENS(3,0)).LE.1.0D10) JK=0
              DO I=JK,NEWIMG
                  X1=(LX0*(VERTEX(1,I)-X00))+(MX0*(VERTEX(2,I)-Y00))
     1            +(NX0*(VERTEX(3,I)-Z0))
                  Y1=(LY0*(VERTEX(1,I)-X00))+(MY0*(VERTEX(2,I)-Y00))
     1            +(NY0*(VERTEX(3,I)-Z0))
                  Z1=(LZ0*(VERTEX(1,I)-X00))+(MZ0*(VERTEX(2,I)-Y00))
     1            +(NZ0*(VERTEX(3,I)-Z0))
C
                  JLX1=(LX0*(VERTEX(4,I)))+(MX0*(VERTEX(5,I)))
     1            +(NX0*(VERTEX(6,I)))
                  JMX1=(LY0*(VERTEX(4,I)))+(MY0*(VERTEX(5,I)))
     1            +(NY0*(VERTEX(6,I)))
                  JNX1=(LZ0*(VERTEX(4,I)))+(MZ0*(VERTEX(5,I)))
     1            +(NZ0*(VERTEX(6,I)))
C
                  JLY1=(LX0*(VERTEX(7,I)))+(MX0*(VERTEX(8,I)))
     1            +(NX0*(VERTEX(9,I)))
                  JMY1=(LY0*(VERTEX(7,I)))+(MY0*(VERTEX(8,I)))
     1            +(NY0*(VERTEX(9,I)))
                  JNY1=(LZ0*(VERTEX(7,I)))+(MZ0*(VERTEX(8,I)))
     1            +(NZ0*(VERTEX(9,I)))
C
                  JLZ1=(LX0*(VERTEX(10,I)))+(MX0*(VERTEX(11,I)))
     1            +(NX0*(VERTEX(12,I)))
                  JMZ1=(LY0*(VERTEX(10,I)))+(MY0*(VERTEX(11,I)))
     1            +(NY0*(VERTEX(12,I)))
                  JNZ1=(LZ0*(VERTEX(10,I)))+(MZ0*(VERTEX(11,I)))
     1            +(NZ0*(VERTEX(12,I)))
C
                  VERTEX(1,I)=X1
                  VERTEX(2,I)=Y1
                  VERTEX(3,I)=Z1
                  VERTEX(4,I)=JLX1
                  VERTEX(5,I)=JMX1
                  VERTEX(6,I)=JNX1
                  VERTEX(7,I)=JLY1
                  VERTEX(8,I)=JMY1
                  VERTEX(9,I)=JNY1
                  VERTEX(10,I)=JLZ1
                  VERTEX(11,I)=JMZ1
                  VERTEX(12,I)=JNZ1
              END DO
          ELSE
C       GLOBAL RAY TRACING WAS EXPLICITLY "OFF"
              GLSURF=GSURFF
              X00=VERTEX(1,GSURFF)
              Y00=VERTEX(2,GSURFF)
              Z0=VERTEX(3,GSURFF)
              LX0=VERTEX(4,GSURFF)
              MX0=VERTEX(5,GSURFF)
              NX0=VERTEX(6,GSURFF)
              LY0=VERTEX(7,GSURFF)
              MY0=VERTEX(8,GSURFF)
              NY0=VERTEX(9,GSURFF)
              LZ0=VERTEX(10,GSURFF)
              MZ0=VERTEX(11,GSURFF)
              NZ0=VERTEX(12,GSURFF)
C
              IF(DABS(ALENS(3,0)).GT.1.0D10) JK=1
              IF(DABS(ALENS(3,0)).LE.1.0D10) JK=0
              DO I=JK,NEWIMG
                  X1=(LX0*(VERTEX(1,I)-X00))+(MX0*(VERTEX(2,I)-Y00))
     1            +(NX0*(VERTEX(3,I)-Z0))
                  Y1=(LY0*(VERTEX(1,I)-X00))+(MY0*(VERTEX(2,I)-Y00))
     1            +(NY0*(VERTEX(3,I)-Z0))
                  Z1=(LZ0*(VERTEX(1,I)-X00))+(MZ0*(VERTEX(2,I)-Y00))
     1            +(NZ0*(VERTEX(3,I)-Z0))
C
                  JLX1=(LX0*(VERTEX(4,I)))+(MX0*(VERTEX(5,I)))
     1            +(NX0*(VERTEX(6,I)))
                  JMX1=(LY0*(VERTEX(4,I)))+(MY0*(VERTEX(5,I)))
     1            +(NY0*(VERTEX(6,I)))
                  JNX1=(LZ0*(VERTEX(4,I)))+(MZ0*(VERTEX(5,I)))
     1            +(NZ0*(VERTEX(6,I)))
C
                  JLY1=(LX0*(VERTEX(7,I)))+(MX0*(VERTEX(8,I)))
     1            +(NX0*(VERTEX(9,I)))
                  JMY1=(LY0*(VERTEX(7,I)))+(MY0*(VERTEX(8,I)))
     1            +(NY0*(VERTEX(9,I)))
                  JNY1=(LZ0*(VERTEX(7,I)))+(MZ0*(VERTEX(8,I)))
     1            +(NZ0*(VERTEX(9,I)))
C
                  JLZ1=(LX0*(VERTEX(10,I)))+(MX0*(VERTEX(11,I)))
     1            +(NX0*(VERTEX(12,I)))
                  JMZ1=(LY0*(VERTEX(10,I)))+(MY0*(VERTEX(11,I)))
     1            +(NY0*(VERTEX(12,I)))
                  JNZ1=(LZ0*(VERTEX(10,I)))+(MZ0*(VERTEX(11,I)))
     1            +(NZ0*(VERTEX(12,I)))
C
                  VERTEX(1,I)=X1
                  VERTEX(2,I)=Y1
                  VERTEX(3,I)=Z1
                  VERTEX(4,I)=JLX1
                  VERTEX(5,I)=JMX1
                  VERTEX(6,I)=JNX1
                  VERTEX(7,I)=JLY1
                  VERTEX(8,I)=JMY1
                  VERTEX(9,I)=JNY1
                  VERTEX(10,I)=JLZ1
                  VERTEX(11,I)=JMZ1
                  VERTEX(12,I)=JNZ1
              END DO
          END IF
C
C       THE DATA IN THE VERTEX ARRAY REPRESENTS GLOBAL
C       COORDINATES OF ALL SURFACE VERTICIES WITH RESPECT
C       TO THE LOCAL COORDINATE SYSTEM OF SURFACE GLSURF.
C
C       IF THE OFFSETS ARE NOT ALL ZERO, MAKE APPROPRIATE
C       CORRECTIONS TO THE VERTEX DATA.
C
          IF(OFFX.NE.0.0D0.OR.OFFY.NE.0.0D0.OR.OFFZ.NE.0.0D0.OR.
     1    OFFA.NE.0.0D0.OR.OFFB.NE.0.0D0.OR.OFFC.NE.0.0D0) THEN
C       MAKE A CORRECTION
C       FIRST, CALCULATE THE NEW ORIGIN COORDINATES AND
C       DIRECTION COSINES IN THE COORDINATE SYSTEM CENTERED AT
C       SURFACE GLSURF
C       DO DEC THEN TILTS
              GDELX=OFFX
              GDELY=OFFY
              TH=OFFZ
              X=VERTEX(1,GLSURF)
              Y=VERTEX(2,GLSURF)
              Z=VERTEX(3,GLSURF)
              LX=VERTEX(4,GLSURF)
              MX=VERTEX(5,GLSURF)
              NX=VERTEX(6,GLSURF)
              LY=VERTEX(7,GLSURF)
              MY=VERTEX(8,GLSURF)
              NY=VERTEX(9,GLSURF)
              LZ=VERTEX(10,GLSURF)
              MZ=VERTEX(11,GLSURF)
              NZ=VERTEX(12,GLSURF)
              X00=X+(TH*LZ)+(GDELX*LX)+(GDELY*LY)
              Y00=Y+(TH*MZ)+(GDELX*MX)+(GDELY*MY)
              Z0=Z+(TH*NZ)+(GDELX*NX)+(GDELY*NY)
C       NOW ROTATE

              A=OFFA*(PII/180.0D0)
              JLX1=(LX*A11(A))+(MX*A12(A))+(NX*A13(A))
              JMX1=(LX*A21(A))+(MX*A22(A))+(NX*A23(A))
              JNX1=(LX*A31(A))+(MX*A32(A))+(NX*A33(A))
              JLY1=(LY*A11(A))+(MY*A12(A))+(NY*A13(A))
              JMY1=(LY*A21(A))+(MY*A22(A))+(NY*A23(A))
              JNY1=(LY*A31(A))+(MY*A32(A))+(NY*A33(A))
              JLZ1=(LZ*A11(A))+(MZ*A12(A))+(NZ*A13(A))
              JMZ1=(LZ*A21(A))+(MZ*A22(A))+(NZ*A23(A))
              JNZ1=(LZ*A31(A))+(MZ*A32(A))+(NZ*A33(A))
              LX=JLX1
              MX=JMX1
              NX=JNX1
              LY=JLY1
              MY=JMY1
              NY=JNY1
              LZ=JLZ1
              MZ=JMZ1
              NZ=JNZ1
              A=OFFB*(PII/180.0D0)
              JLX1=(LX*B11(A))+(MX*B12(A))+(NX*B13(A))
              JMX1=(LX*B21(A))+(MX*B22(A))+(NX*B23(A))
              JNX1=(LX*B31(A))+(MX*B32(A))+(NX*B33(A))
              JLY1=(LY*B11(A))+(MY*B12(A))+(NY*B13(A))
              JMY1=(LY*B21(A))+(MY*B22(A))+(NY*B23(A))
              JNY1=(LY*B31(A))+(MY*B32(A))+(NY*B33(A))
              JLZ1=(LZ*B11(A))+(MZ*B12(A))+(NZ*B13(A))
              JMZ1=(LZ*B21(A))+(MZ*B22(A))+(NZ*B23(A))
              JNZ1=(LZ*B31(A))+(MZ*B32(A))+(NZ*B33(A))
              LX=JLX1
              MX=JMX1
              NX=JNX1
              LY=JLY1
              MY=JMY1
              NY=JNY1
              LZ=JLZ1
              MZ=JMZ1
              NZ=JNZ1
              A=OFFC*(PII/180.0D0)
              JLX1=(LX*C11(A))+(MX*C12(A))+(NX*C13(A))
              JMX1=(LX*C21(A))+(MX*C22(A))+(NX*C23(A))
              JNX1=(LX*C31(A))+(MX*C32(A))+(NX*C33(A))
              JLY1=(LY*C11(A))+(MY*C12(A))+(NY*C13(A))
              JMY1=(LY*C21(A))+(MY*C22(A))+(NY*C23(A))
              JNY1=(LY*C31(A))+(MY*C32(A))+(NY*C33(A))
              JLZ1=(LZ*C11(A))+(MZ*C12(A))+(NZ*C13(A))
              JMZ1=(LZ*C21(A))+(MZ*C22(A))+(NZ*C23(A))
              JNZ1=(LZ*C31(A))+(MZ*C32(A))+(NZ*C33(A))
              LX=JLX1
              MX=JMX1
              NX=JNX1
              LY=JLY1
              MY=JMY1
              NY=JNY1
              LZ=JLZ1
              MZ=JMZ1
              NZ=JNZ1
              LX0=JLX1
              MX0=JMX1
              NX0=JNX1
              LY0=JLY1
              MY0=JMY1
              NY0=JNY1
              LZ0=JLZ1
              MZ0=JMZ1
              NZ0=JNZ1
C       THESE ARE THE COORDINATES OF THE NEW ORIGIN AND THE NEW
C       COORDINATE SYSTEMS AXES DIRECTION COSINES. NOW TRANSFORM
C       ALL VERTEX VALUES TO BE REPRESENTED WITH RESPECT TO THIS
C       NEW SYSTEM.
              IF(DABS(ALENS(3,0)).GT.1.0D10) JK=1
              IF(DABS(ALENS(3,0)).LE.1.0D10) JK=0
              DO 12 I=JK,NEWIMG
                  X1=(LX0*(VERTEX(1,I)-X00))+(MX0*(VERTEX(2,I)-Y00))
     1            +(NX0*(VERTEX(3,I)-Z0))
                  Y1=(LY0*(VERTEX(1,I)-X00))+(MY0*(VERTEX(2,I)-Y00))
     1            +(NY0*(VERTEX(3,I)-Z0))
                  Z1=(LZ0*(VERTEX(1,I)-X00))+(MZ0*(VERTEX(2,I)-Y00))
     1            +(NZ0*(VERTEX(3,I)-Z0))
C
                  JLX1=(LX0*(VERTEX(4,I)))+(MX0*(VERTEX(5,I)))
     1            +(NX0*(VERTEX(6,I)))
                  JMX1=(LY0*(VERTEX(4,I)))+(MY0*(VERTEX(5,I)))
     1            +(NY0*(VERTEX(6,I)))
                  JNX1=(LZ0*(VERTEX(4,I)))+(MZ0*(VERTEX(5,I)))
     1            +(NZ0*(VERTEX(6,I)))
C
                  JLY1=(LX0*(VERTEX(7,I)))+(MX0*(VERTEX(8,I)))
     1            +(NX0*(VERTEX(9,I)))
                  JMY1=(LY0*(VERTEX(7,I)))+(MY0*(VERTEX(8,I)))
     1            +(NY0*(VERTEX(9,I)))
                  JNY1=(LZ0*(VERTEX(7,I)))+(MZ0*(VERTEX(8,I)))
     1            +(NZ0*(VERTEX(9,I)))
C
                  JLZ1=(LX0*(VERTEX(10,I)))+(MX0*(VERTEX(11,I)))
     1            +(NX0*(VERTEX(12,I)))
                  JMZ1=(LY0*(VERTEX(10,I)))+(MY0*(VERTEX(11,I)))
     1            +(NY0*(VERTEX(12,I)))
                  JNZ1=(LZ0*(VERTEX(10,I)))+(MZ0*(VERTEX(11,I)))
     1            +(NZ0*(VERTEX(12,I)))
C
                  VERTEX(1,I)=X1
                  VERTEX(2,I)=Y1
                  VERTEX(3,I)=Z1
                  VERTEX(4,I)=JLX1
                  VERTEX(5,I)=JMX1
                  VERTEX(6,I)=JNX1
                  VERTEX(7,I)=JLY1
                  VERTEX(8,I)=JMY1
                  VERTEX(9,I)=JNY1
                  VERTEX(10,I)=JLZ1
                  VERTEX(11,I)=JMZ1
                  VERTEX(12,I)=JNZ1
                  AVALUE=(VERTEX(4,I)**2)+(VERTEX(5,I)**2)+(VERTEX(6,I)**2)
                  BVALUE=(VERTEX(7,I)**2)+(VERTEX(8,I)**2)+(VERTEX(9,I)**2)
                  CVALUE=(VERTEX(10,I)**2)+(VERTEX(11,I)**2)+(VERTEX(12,I)**2)
                  VERTEX(4,I)=VERTEX(4,I)/DSQRT(AVALUE)
                  VERTEX(5,I)=VERTEX(5,I)/DSQRT(AVALUE)
                  VERTEX(6,I)=VERTEX(6,I)/DSQRT(AVALUE)
                  VERTEX(7,I)=VERTEX(7,I)/DSQRT(BVALUE)
                  VERTEX(8,I)=VERTEX(8,I)/DSQRT(BVALUE)
                  VERTEX(9,I)=VERTEX(9,I)/DSQRT(BVALUE)
                  VERTEX(10,I)=VERTEX(10,I)/DSQRT(CVALUE)
                  VERTEX(11,I)=VERTEX(11,I)/DSQRT(CVALUE)
                  VERTEX(12,I)=VERTEX(12,I)/DSQRT(CVALUE)
 12           CONTINUE
C
C       ALL GLOBAL VERTEX TRANSFORMATIONS ARE COMPLETE
C       NO CORRECTION TO MAKE
          END IF
          RETURN
      END
C SUB VERT123
      SUBROUTINE VERT123(
     1X1,Y1,Z1,XL1,XM1,XN1,YL1,YM1,YN1,ZL1,ZM1,ZN1,
     2X2,Y2,Z2,XL2,XM2,XN2,YL2,YM2,YN2,ZL2,ZM2,ZN2,
     3X3,Y3,Z3,XL3,XM3,XN3,YL3,YM3,YN3,ZL3,ZM3,ZN3,
     4ALPHA,BETA,GAMMA)
C
          IMPLICIT NONE
C
          REAL*8 X1,Y1,Z1,XL1,XM1,XN1,YL1,YM1,YN1,ZL1,ZM1,ZN1
          REAL*8 X2,Y2,Z2,XL2,XM2,XN2,YL2,YM2,YN2,ZL2,ZM2,ZN2
          REAL*8 X3,Y3,Z3,XL3,XM3,XN3,YL3,YM3,YN3,ZL3,ZM3,ZN3
          REAL*8 D11,D12,D13,D21,D22,D23,D31,D32,D33
          REAL*8 ALPHA,BETA,GAMMA,COSB,SINB
C
          INCLUDE 'datmai.inc'
C
C       GIVEN X1,Y1,Z1,L1,M1,N1,XL1,XM1,XN1,YL1,YM1 AND YZ1
C       AND   X2,Y2,Z2,L2,M2,N2,XL2,XM2,XN2,YL2,YM2 AND YZ2
C       WHICH ARE THE COORDINATES AND ORIENTATION OF TWO COORDINATE
C       SYSTEMS REPRESENTED IN THE GLOBAL COORDINATE SYSTEM OF A
C       THIRD "GLOBAL" REFERENCE FRAME, REPRESENT THE COORDINTES
C       AND ORIENTATION OF "2" IN THE COORDINATE SYSTEM OF "1"
C       RESULTS ARE X3,Y3,Z3,L3,M3,N3,XL3,XM3,XN3,YL3,YM3 AND YZ3
C
C
          X3=(XL1*(X2-X1))+(XM1*(Y2-Y1))
     1    +(XN1*(Z2-Z1))
          Y3=(YL1*(X2-X1))+(YM1*(Y2-Y1))
     1    +(YN1*(Z2-Z1))
          Z3=(ZL1*(X2-X1))+(ZM1*(Y2-Y1))
     1    +(ZN1*(Z2-Z1))
C
          XL3=(XL1*XL2)+(XM1*XM2)
     1    +(XN1*XN2)
          XM3=(YL1*XL2)+(YM1*XM2)
     1    +(YN1*XN2)
          XN3=(ZN1*XL2)+(ZM1*XM2)
     1    +(ZN1*XN2)
C
          YL3=(XL1*YL2)+(XM1*YM2)
     1    +(XN1*YN2)
          YM3=(YL1*YL2)+(YM1*YM2)
     1    +(YN1*YN2)
          YN3=(ZN1*YL2)+(ZM1*YM2)
     1    +(ZN1*YN2)
C
          ZL3=(XL1*ZL2)+(XM1*ZM2)
     1    +(XN1*ZN2)
          ZM3=(YL1*ZL2)+(YM1*ZM2)
     1    +(YN1*ZN2)
          ZN3=(ZL1*ZL2)+(ZM1*ZM2)
     1    +(ZN1*ZN2)
C
          D11=XL3
          D12=XM3
          D13=XN3
          D21=YL3
          D22=YM3
          D23=YN3
          D31=ZL3
          D32=ZM3
          D33=ZN3

C     NOW CALCULATE XD,YD,ZD,ALPHA,BETA AND GAMMA
C     THESE ARE THE DECENTERS AND TILT ANGLES WHICH WHOULD PLACE COORDINATE 1 AT COORDINATE 2
C     AND IN THE SAME ORIENTATION
C
C     CALCULATE BETA
          BETA=DASIN(-D31)
          COSB=DCOS(BETA)
          IF(COSB.NE.0.0D0) THEN
C     COSINE OF BETA IS NOT ZERO
              IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).NE.0.0D0) ALPHA=0.0D0
              IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=0.0D0
              IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=PII/2.0D0
              IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).NE.0.0D0)
     1        ALPHA=DATAN2((D32/COSB),(D33/COSB))
              IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).NE.0.0D0) GAMMA=0.0D0
              IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=0.0D0
              IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=PII/2.0D0
              IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).NE.0.0D0)
     1        GAMMA=DATAN2((-D21/COSB),(D11/COSB))
          END IF
          IF(COSB.EQ.0.0D0) THEN
C     COSINE OF BETA IS ZERO
              IF(D31.EQ.-1.0D0) SINB=1
              IF(D31.EQ.1.0D0) SINB=-1
              IF(SINB.EQ.1) BETA=PII/2.0D0
              IF(SINB.EQ.-1) BETA=-PII/2.0D0
              GAMMA=0.0D0
              IF(SINB.EQ.1) THEN
                  IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1            ALPHA=0.0D0
                  IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1            ALPHA=0.0D0
                  IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1            ALPHA=PII/2.0D0
                  IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1            ALPHA=DATAN2((D12),(D13))
              END IF
              IF(SINB.EQ.-1) THEN
                  IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1            ALPHA=0.0D0
                  IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1            ALPHA=0.0D0
                  IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1            ALPHA=PII/2.0D0
                  IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1            ALPHA=DATAN2((-D12),(-D13))
              END IF
          END IF
          ALPHA=ALPHA*(180.0D0/PII)
          BETA=BETA*(180.0D0/PII)
          GAMMA=GAMMA*(180.0D0/PII)
C
          WRITE(OUTLYNE,11)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) X1,Y1,Z1
          CALL SHOWIT(0)
          WRITE(OUTLYNE,12)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) XL1,XM1,XN1
          CALL SHOWIT(0)
          WRITE(OUTLYNE,13)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) YL1,YM1,YN1
          CALL SHOWIT(0)
          WRITE(OUTLYNE,14)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) ZL1,ZM1,ZN1
          CALL SHOWIT(0)
C
          WRITE(OUTLYNE,21)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) X2,Y2,Z2
          CALL SHOWIT(0)
          WRITE(OUTLYNE,22)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) XL2,XM2,XN2
          CALL SHOWIT(0)
          WRITE(OUTLYNE,23)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) YL2,YM2,YN2
          CALL SHOWIT(0)
          WRITE(OUTLYNE,24)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) ZL2,ZM2,ZN2
          CALL SHOWIT(0)
C
          WRITE(OUTLYNE,31)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) X3,Y3,Z3
          CALL SHOWIT(0)
          WRITE(OUTLYNE,32)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) XL3,XM3,XN3
          CALL SHOWIT(0)
          WRITE(OUTLYNE,33)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) YL3,YM3,YN3
          CALL SHOWIT(0)
          WRITE(OUTLYNE,34)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) ZL3,ZM3,ZN3
          CALL SHOWIT(0)
          WRITE(OUTLYNE,35)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) X3,Y3,Z3
          CALL SHOWIT(0)
          WRITE(OUTLYNE,36)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,10) ALPHA,BETA,GAMMA
          CALL SHOWIT(0)
 10       FORMAT(3(1X,D23.15))
 11       FORMAT(11X,'X1',22X,'Y1',22X,'Z1')
 12       FORMAT(10X,'XL1',21X,'XM1',21X,'XN1')
 13       FORMAT(10X,'YL1',21X,'YM1',21X,'YN1')
 14       FORMAT(10X,'ZL1',21X,'ZM1',21X,'ZN1')
 21       FORMAT(11X,'X2',22X,'Y2',22X,'Z2')
 22       FORMAT(10X,'XL2',21X,'XM2',21X,'XN2')
 23       FORMAT(10X,'YL2',21X,'YM2',21X,'YN2')
 24       FORMAT(10X,'ZL2',21X,'ZM2',21X,'ZN2')
 31       FORMAT(11X,'X3',22X,'Y3',22X,'Z3')
 32       FORMAT(10X,'XL3',21X,'XM3',21X,'XN3')
 33       FORMAT(10X,'YL3',21X,'YM3',21X,'YN3')
 34       FORMAT(10X,'ZL3',21X,'ZM3',21X,'ZN3')
 35       FORMAT(11X,'XD',22X,'YD',22X,'ZD')
 36       FORMAT(8X,'ALPHA',20X,'BETA',19X,'GAMMA')
          RETURN
      END
C SUB ROOFTILTT.FOR
      SUBROUTINE ROOFTILT(N)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE ROOFTILT.FOR. THIS SUBROUTINE IMPLEMENTS
C       CREATES CORRECTLY ORIENTED ROOF SURFACES FOR DRAWING THE CCR
C       ROOF SURFACES IN THE SEQUENTIAL LENS DATABASE.
C
C       SURFACES ARE ASSUMED TO GET TO THEIR ORIENTATION VIA SIMPLE "TILTS"
C
C     IF N = 2, SURFACE IS A PRISM ROOF. IF N = 3, SURFACE IS A CCR ROOF
C
          INTEGER N
C
          REAL*8 A11,A12,A13,A21,A22,A23,A31,A32,A33
     1    ,B11,B12,B13,B21,B22,B23,B31,B32,B33
     2    ,C11,C12,C13,C21,C22,C23,C31,C32,C33,AN
C
          REAL*8 LX11,LY11,LZ11,MX11,MY11,MZ11,NX11,NY11,NZ11,
     1    LX22,LY22,LZ22,MX22,MY22,MZ22,NX22,NY22,NZ22,
     2    LX33,LY33,LZ33,MX33,MY33,MZ33,NX33,NY33,NZ33,
     3    LX,LY,LZ,MX,MY,MZ,NX,NY,NZ
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C
C       ROTATION MATRICIES FUNCTIONS
C
C       A11
          A11(AN)=1.0D0
C       A12
          A12(AN)=0.0D0
C       A13
          A13(AN)=0.0D0
C       A21
          A21(AN)=0.0D0
C       A22
          A22(AN)=DCOS(AN)
C       A23
          A23(AN)=-DSIN(AN)
C       A31
          A31(AN)=0.0D0
C       A32
          A32(AN)=DSIN(AN)
C       A33
          A33(AN)=DCOS(AN)
C       B11
          B11(AN)=DCOS(AN)
C       B12
          B12(AN)=0.0D0
C       B13
          B13(AN)=DSIN(AN)
C       B21
          B21(AN)=0.0D0
C       B22
          B22(AN)=1.0D0
C       B23
          B23(AN)=0.0D0
C       B31
          B31(AN)=-DSIN(AN)
C       B32
          B32(AN)=0.0D0
C       B33
          B33(AN)=DCOS(AN)
C       C11
          C11(AN)=DCOS(AN)
C       C12
          C12(AN)=DSIN(AN)
C       C13
          C13(AN)=0.0D0
C       C21
          C21(AN)=-DSIN(AN)
C       C22
          C22(AN)=DCOS(AN)
C       C23
          C23(AN)=0.0D0
C       C31
          C31(AN)=0.0D0
C       C32
          C32(AN)=0.0D0
C       C33
          C33(AN)=1.0D0
C
C       ALL ROOVES STARTS WITH
          LX=1.0D0
          MX=0.0D0
          NX=0.0D0
          LY=0.0D0
          MY=1.0D0
          NY=0.0D0
          LZ=0.0D0
          MZ=0.0D0
          NZ=1.0D0
          LX11=1.0D0
          MX11=0.0D0
          NX11=0.0D0
          LY11=0.0D0
          MY11=1.0D0
          NY11=0.0D0
          LZ11=0.0D0
          MZ11=0.0D0
          NZ11=1.0D0
          LX22=1.0D0
          MX22=0.0D0
          NX22=0.0D0
          LY22=0.0D0
          MY22=1.0D0
          NY22=0.0D0
          LZ22=0.0D0
          MZ22=0.0D0
          NZ22=1.0D0
          LX33=1.0D0
          MX33=0.0D0
          NX33=0.0D0
          LY33=0.0D0
          MY33=1.0D0
          NY33=0.0D0
          LZ33=0.0D0
          MZ33=0.0D0
          NZ33=1.0D0
C
          IF(N.EQ.2) THEN
C     ROOF #1 IS ROTATED BY +BETA. THIS ROOF LIES TO THE -X SIDE OF THE LOCAL
C     YZ-PLANE. BETA IS 45 DEGREEDS PLUS THE ANGLE ERROR PASSED FOR THIS SURFACE
C     ROOF #2 IS ROTATED BY -BETA. THIS ROOF LIES TO THE +X SIDE OF THE LOCAL
C     YZ-PLANE. BETA IS 45 DEGREEDS PLUS THE ANGLE ERROR PASSED FOR THIS SURFACE
          END IF
          IF(N.EQ.3) THEN
C     ROOF #1 IS ROTATED BY +ALPHA AND THEN BY -GAMMA WHERE ALPHA IS 45 DEGREES.
C     PLUS THE ANGLE ERROR. THIS SURFACE LIES TO THE +X SIDE OF THE YZ-PLANE.
C     GAMMA = 45 DEGREES
C     ROOF #2 IS ROTATED BY +ALPHA AND THEN BY -GAMMA WHERE ALPHA IS 45 DEGREES.
C     PLUS THE ANGLE ERROR. THIS SURFACE LIES TO THE -Y SIDE OF THE XZ-PLANE.
C     GAMMA = 180 DEGREES
C     ROOF #3 IS ROTATED BY +ALPHA AND THEN BY -GAMMA WHERE ALPHA IS 45 DEGREES.
C     PLUS THE ANGLE ERROR. THIS SURFACE LIES TO THE -Y SIDE OF THE XZ-PLANE.
C     GAMMA = 315 DEGREES
          END IF
C
          RETURN
      END
