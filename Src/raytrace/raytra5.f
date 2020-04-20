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

C       THIS IS THE FIFTH FILE OF RAYTRACING ROUTINES

C SUB QRRAY.FOR
      SUBROUTINE QRRAY
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE QRRAY.FOR. THIS SUBROUTINE IMPLEMENTS
C       QUICK RAY TRACING AND CALLS QTRA1.FOR
C
          REAL*8 OLDAIM,X,Y,AREA1,AREA2
C
          REAL*8 SYS12,SYS13,SMALLAREA,BIGAREA
C
          INTEGER*1 ACOD
C
          INTEGER KSF,K,I,J,FOTLIM,SFNUMFT,RAYTOT,RAYCD1,RAYCD2
     1    ,RAYCD3
C
          COMMON/RAYCD/RAYCD1,RAYCD2,RAYCD3
C
          LOGICAL TEMPER,OMSG
C
          COMMON/FOOTNUM/SFNUMFT
C
          COMMON/LIMMER/FOTLIM
C
          REAL RX,RY,RZ,RL,RM,RN,SFL,SFM,SFN,AREA3
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C     CHECK INPUT
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"FOOT" CAUSES A FOOTPRINT RAY GRID TO BE TRACED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"FOOT" ONLY TAKES NUMERIC WORD #1 AND QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'GRID'.AND.
     2        WQ.NE.'APE') THEN
                  OUTLYNE=
     1            '"FOOT" ONLY TAKES "GRID" AND "APE" AS QUALIFIER WORDS'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE=
     1        '"FOOT" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
              OUTLYNE=
     1        'SURFACE NUMBER BEYOND RANGE SPECIFIED IN "FOB" COMMAND'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CHECK IF REFEXT=.FALSE. IF SO STOP AND PRINT  MESSAGE
C
          IF(.NOT.REFEXT) THEN
C       NO CHIEF RAY EXISTS, STOP
              OUTLYNE=
     1        'AN "FOB" COMMAND MUST BE ISSUED BEFORE RAYS CAN BE TRACED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          TEMPER=.FALSE.
          IF(ALENS(9,NEWREF).EQ.0.0D0.OR.ALENS(127,NEWREF).NE.0.0D0) THEN
C     ASSIGN A TEMPORARY CIRCULAR CLAP
              TEMPER=.TRUE.
              ALENS(9,NEWREF)=1.0D0
              IF(PXTRAY(1,NEWREF).GT.PXTRAX(1,NEWREF)) THEN
                  ALENS(10,NEWREF)=PXTRAY(1,NEWREF)
                  ALENS(11,NEWREF)=PXTRAY(1,NEWREF)
              ELSE
                  ALENS(10,NEWREF)=PXTRAX(1,NEWREF)
                  ALENS(11,NEWREF)=PXTRAX(1,NEWREF)
              END IF
          END IF
C
C     TOTAL NUMBER OF RECORDS IN FILE IS 1+(((2*FOTLIM)+1)**2)
C
          OPEN(UNIT=94,ACCESS='DIRECT'
     1    ,FORM='UNFORMATTED',FILE=trim(HOME)//'FOOT1.DAT',RECL=(80*NRECL)
     2    ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(94,0)
          OPEN(UNIT=94,ACCESS='DIRECT'
     1    ,FORM='UNFORMATTED',FILE=trim(HOME)//'FOOT1.DAT',RECL=(80*NRECL)
     2    ,STATUS='UNKNOWN')
C
          OUTLYNE=
     1    'BEAM FOOTPRINT DATA BEING GENERATED'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)
     1    'CURRENT RAY GRID SIZE IS : ',(2*FOTLIM)+1,' x ',(2*FOTLIM)+1
          CALL SHOWIT(1)
          OUTLYNE='PLEASE WAIT...'
          CALL SHOWIT(1)
C
C
C     AREA OF ONE SQUARE ON THE REFERENCE SURFACE IS EQUAL TO 4 TIMES THE AREA
C     OF A RECTANGLE WHOSE DIMENSIONS ARE THE Y-REF AP HT
C     TIMES THE X-REF AP HT DIVIDED BY THE SQUARE OF THE FOTLIM VALUE
C
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.
     1    DABS(ALENS(9,NEWREF)).LE.6.0D0.AND.
     1    ALENS(127,NEWREF).EQ.0.0D0) THEN
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
          BIGAREA=(SYS12*SYS13)
          SMALLAREA=BIGAREA/DBLE(FOTLIM**2)
C
          RAYTOT=(((2*FOTLIM)+1)**2)+1
          KSF=INT(W1)
          SFNUMFT=INT(W1)
          WRITE(UNIT=94,REC=1) RAYTOT,KSF
C
          K=2
          OLDAIM=AIMTOL
          AIMTOL=1.0D-3
          DO I=-FOTLIM,FOTLIM
              DO J=-FOTLIM,FOTLIM
C       SET RAYCODS
                  RAYCOD(1)=0
                  Y=DBLE(I)/DBLE(FOTLIM)
                  X=DBLE(J)/DBLE(FOTLIM)
                  WW1=Y
                  WW2=X
                  WW3=LFOB(4)
                  WVN=LFOB(4)
C       TRACE RAY AND WRITE RAYCOD(1)
                  MSG=.FALSE.
                  RAYCD1=0
                  RAYCD3=0
                  RAYCD2=-1
                  CALL QTRA1(.TRUE.)
                  MSG=.TRUE.
                  IF(WQ.EQ.'APE') THEN
C     CONSIDER ALL CLAPS AND COBS AND OTHER BLOCKERS
C     ACOD=1 MEANS RAY SUCCEDDED
C     ACOD=0 MEANS RAY FAILED
                      ACOD=1
                      IF(RAYCD3.NE.0) ACOD=0
                  ELSE
C
C     QUALIFIER WORD WAS NOT 'APE'
C
C     ACOD=1 MEANS RAY SUCCEDDED
C     ACOD=0 MEANS RAY FAILED
                      ACOD=1
                      IF(RAYCD1.EQ.6.OR.RAYCD1.EQ.7) THEN
                          IF(RAYCD2.EQ.NEWREF) ACOD=0
C     IS THE SURFACE WHERE THE CLAP OR COBS BLOCK OCCURRED DEFINED AS
C     A FOOTPRINT BLOCKING SURFACE? IS SO, RAY FAILED
                          IF(RAYCD3.NE.0.AND.ALENS(58,RAYCD2).EQ.1.0D0) ACOD=0
                      END IF
                      IF(RAYCOD(1).NE.0.AND.RAYCOD(1).NE.6.AND.RAYCOD(1).NE.7)
     1                ACOD=0
C     SINCE RAY REALLY DID FAIL AND COULD NOT GET THROUGH.
                  END IF
C
                  KSF=INT(W1)
                  SFNUMFT=INT(W1)
                  IF(ACOD.EQ.1) THEN
                      RX=REAL(QRAY(1,KSF))
                      RY=REAL(QRAY(2,KSF))
                      RZ=REAL(QRAY(3,KSF))
                      RL=REAL(QRAY(4,KSF))
                      RM=REAL(QRAY(5,KSF))
                      RN=REAL(QRAY(6,KSF))
                      SFL=REAL(QRAY(7,KSF))
                      SFM=REAL(QRAY(8,KSF))
                      SFN=REAL(QRAY(9,KSF))
                  END IF
                  IF(ACOD.EQ.0) THEN
                      RX=0.0
                      RY=0.0
                      RZ=0.0
                  END IF
C
C     TRACE DIFFERENTIAL RAYS
C
C       RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
C       DATA
C       SAVE RAYRAY DATA
                  SAVE=RAYRAY
                  OMSG=MSG
                  MSG=.FALSE.
                  SRAYDIFEXT=.FALSE.
                  DIFRAYTRACE=.TRUE.
                  CALL DIFRAY
                  DIFRAYTRACE=.FALSE.
                  MSG=OMSG
                  IF(STOPP.EQ.1) THEN
                      ACOD=0
                      STOPP=0
                      RAYRAY=SAVE
                  ELSE
                      STOPP=0
                      RAYEXT=.TRUE.
                  END IF
C
                  IF(ACOD.NE.0) THEN
C       CALC AN AREA
                      AREA1=((DIFF(1,KSF)-RAYRAY(1,KSF))*
     1                (DIFF(8,KSF)-RAYRAY(2,KSF)))/(DELX*DELY)
                      AREA2=((DIFF(1,NEWREF)-RAYRAY(1,NEWREF))*
     1                (DIFF(8,NEWREF)-RAYRAY(2,NEWREF)))/(DELX*DELY)
                      AREA3=REAL((AREA1/AREA2)*SMALLAREA)
                  END IF
                  WRITE(UNIT=94,REC=K) RX,RY,RZ,RL,RM,RN,SFL,SFM,SFN,ACOD,AREA3

                  K=K+1
C
C     DO THE NEXT RAY
C
C
              END DO
          END DO
          AIMTOL=OLDAIM
          CALL CLOSE_FILE(94,1)
          IF(TEMPER) THEN
              ALENS(9:14,NEWREF)=0.0D0
          END IF
          RETURN
      END


C
      SUBROUTINE FTGRID
C
          IMPLICIT NONE
C
          INTEGER FOTLIM
C
          COMMON/LIMMER/FOTLIM
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C     CHECK INPUT
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"FOOT GRID" CAUSES A FOOTPRINT GRID SIZE TO BE SET'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'CURRENT FOOT RAY GRID  IS : ',(2*FOTLIM+1),' x ',(2*FOTLIM)+1
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        '"FOOT GRID" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.2.0D0) THEN
              OUTLYNE=
     1        '"FOOT GRID" MINIMUM VALUE FOR NUMERIC WORD #1 IS 2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF1.EQ.1) W1=16.0D0
          FOTLIM=INT(W1)
          RETURN
      END
C SUB QTRA1.FOR

      SUBROUTINE QTRA1(FOOT_TRACE)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE QTRA1.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE TRACING OF A QUICK RAY FOR VIGCAL.
C
          INTEGER JK,I,ISYS20,KKK

          INTEGER CAERAS,COERAS
C
          REAL*8 X,Y,Z,L,M,N,XC1,YC1,ZC1
     1    ,WWW1,WWW2,D21,D22,GAMMA,JKX,JKY,
     3    D11,D12,LS,SNINDX,SNIND2,JK1,JK2,JK3,
     6    LSTART,MSTART,NSTART,YANG,XANG,X1LAST,LARGE,
     7    Y1LAST,X1ONE,Y1ONE,RXLAST,RYLAST,RXONE,RYONE,DDELX,DDELY,
     8    MAG,MF1,MF2,TARX,TARY,XVALUE,YVALUE,
     9    LOLD,MOLD,NOLD,TARRY,TARRX
C
          COMMON/CACO/CAERAS,COERAS,LS
          COMMON/JK/JK1,JK2,JK3           !Add by ENDO
C
          LOGICAL AIMOK,CLAPT,OLDPASS,MMSG,FOOT_TRACE,DELFAIL
          COMMON/PASSOLD/OLDPASS
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
          INTEGER RAYCD1,RAYCD2,RAYCD3
          COMMON/RAYCD/RAYCD1,RAYCD2,RAYCD3
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
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
          KKK=0
          DDELX=0.001D0
          DDELY=0.001D0
C       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
C       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
C       RAY AIMING.
C
          IF(WW3.GE.1.0D0.AND.WW3.LE.5.0D0) THEN
              IF(SYSTEM1(INT(WW3)).EQ.0.0D0) THEN
                  STOPP=1
                  RAYCOD(1)=12
                  RAYCOD(2)=NEWOBJ
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  RETURN
C       PROCEED
              END IF
C       PROCEED
          END IF
          IF(WW3.GE.6.0D0.AND.WW3.LE.10.0D0) THEN
              IF(SYSTEM1(65+INT(WW3)).EQ.0.0D0) THEN
                  STOPP=1
                  RAYCOD(1)=12
                  RAYCOD(2)=NEWOBJ
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  RETURN
              END IF
          END IF
C
C       SET RAYCOD DEFAULTS
          RAYCOD(1)=0
          RAYCOD(2)=-1
C
          AIMOK=.FALSE.
C
          RELY=WW1
          RELX=WW2
C
C       SET INITIAL VALUES FOR X1ONE, Y1ONE, X1LAST AND Y1LAST
C
          LARGE=-99999.9D0
          X1ONE=LARGE
          Y1ONE=LARGE
          X1LAST=LARGE
          Y1LAST=LARGE
C
C       SET INITIAL VALUES FOR RXONE, RYONE, RXLAST AND RYLAST
          RXONE=LARGE
          RYONE=LARGE
          RXLAST=LARGE
          RYLAST=LARGE
C
C       THE FIRST THING TO DO IS TO DETERMINE THE STARTING
C       COORDINATES OF THE RAY AT THE OBJECT SURFACE.
C       THE STARTING RAY COORDINATES AT SURFACE NEWOBJ
C       ARE JUST THE RAY COORDINATES AT THIS SURFACE
C
C
          XSTRT=REFRY(1,NEWOBJ)
          YSTRT=REFRY(2,NEWOBJ)
          ZSTRT=REFRY(3,NEWOBJ)
C
C       THE WAVELENGTH NUMBER FOR THE RAY TRACE IS:
C       INT(WW3) OR INT(SYSTEM1(11))
C
C       NOW WHAT ARE THE STARTING DIRECTION COSINES AND HOW ARE
C       THEY DETERMINED?
C
C       START BY LOOKING AT THE PY+ PCY AND PX+PCX
C       VALUES AT SURFACE NEWOBJ+1.
C       THESE VALUES CORRESPOND TO THE INTERSECTIONS OF CHIEF
C       PLUS MARGINAL RAYS AT FULL 1.0 FRACTIONAL OBJECT HEIGHTS
C       AND FULL REFERENCE APERTURE HEIGHTS.
C       FOR FOB 1.0 1.0 THE TARGET RAY HEIGHTS AT SURFACE NEWOBJ+1
C       FOR THE FULL APERTURE RAY ARE
C       ARE X=PX(NEWOBJ+1)+PCX(NEWOBJ+1)
C       AND Y=PY(NEWOBJ+1)+PCY(NEWOBJ+1).
C
C       IN GENERAL THE RAY INTERESECTION
C       POINTS FOR FIRST GUESS AIMING WILL BE:
C       THE POINT AT SURFACE NEWOBJ+1 AT WHICH THE
C       PARAXIAL CHIEF RAY PLUS THE PARAXIAL MARGINAL RAY
C        IS AIMED.
C     EXCEPT IF THE CLAP VALUE IS SMALLER
          IF(SYSTEM1(63).EQ.0.0D0) THEN
C       TELECENTRIC AIMING OFF
              JKX=PXTRAX(1,NEWOBJ+1)
              JKY=PXTRAY(1,NEWOBJ+1)
              IF(ALENS(9,NEWOBJ+1).EQ.1.0D0) THEN
C     CIRCULAR AP
                  IF(ALENS(10,NEWOBJ+1).LE.ALENS(11,NEWOBJ+1)) THEN
                      IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1                JKX=DABS(ALENS(10,NEWOBJ+1))
                      IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1                JKY=DABS(ALENS(10,NEWOBJ+1))
                  ELSE
                      IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1                JKX=DABS(ALENS(11,NEWOBJ+1))
                      IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1                JKY=DABS(ALENS(11,NEWOBJ+1))
                  END IF
              END IF
              IF(ALENS(9,NEWOBJ+1).EQ.5.0D0) THEN
C     CIRCULAR AP
                  IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1            JKX=DABS(ALENS(10,NEWOBJ+1))
                  IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1            JKY=DABS(ALENS(10,NEWOBJ+1))
              END IF
              IF(ALENS(9,NEWOBJ+1).EQ.6.0D0) THEN
C     CIRCULAR AP
                  IF(DABS(ALENS(14,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1            JKX=DABS(ALENS(14,NEWOBJ+1))
                  IF(DABS(ALENS(14,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1            JKY=DABS(ALENS(14,NEWOBJ+1))
              END IF
              IF(ALENS(9,NEWOBJ+1).GT.1.0D0.AND.
     1        ALENS(9,NEWOBJ+1).LE.4.0D0) THEN
C     OTHER AP
                  IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1            JKX=DABS(ALENS(11,NEWOBJ+1))
                  IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1            JKY=DABS(ALENS(10,NEWOBJ+1))
              END IF
          ELSE
C       TELECENTRIC AIMING ON
              JKX=PXTRAX(1,NEWOBJ+1)
              JKY=PXTRAY(1,NEWOBJ+1)
          END IF
C
 989      CONTINUE
          IF(.NOT.FOOT_TRACE.AND.KKK.GT.1.AND.RAYCOD(1).EQ.1) THEN
              STOPP=1
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              RETURN
          END IF
          IF(NULL) THEN
              IF(SYSTEM1(62).EQ.0.0D0) THEN
                  IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                      X1AIM=((PXTRAX(5,(NEWOBJ+1)))+(WW2*JKX))
                      X1AIM=(X1AIM)-ALENS(31,NEWOBJ+1)
                      Y1AIM=((PXTRAY(5,(NEWOBJ+1)))+(WW1*JKY))
                      Y1AIM=(Y1AIM)-ALENS(31,NEWOBJ+1)
                      Z1AIM=0.0D0
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                  ELSE
C     TEL ON
                      IF(SYSTEM1(16).NE.0.0D0)X1AIM=((PXTRAX(5,NEWOBJ))+(WW2*JKX))
                      IF(SYSTEM1(16).EQ.0.0D0)X1AIM=((WW2*JKX))
                      IF(SYSTEM1(14).NE.0.0D0)Y1AIM=((PXTRAY(5,NEWOBJ))+(WW1*JKY))
                      IF(SYSTEM1(14).EQ.0.0D0)Y1AIM=((WW1*JKY))
                      Z1AIM=0.0D0
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                  END IF
              ELSE
                  X1AIM=((PXTRAX(5,NEWOBJ+1))+(WW2*JKX))
                  X1AIM=(X1AIM)-ALENS(31,NEWOBJ+1)
                  Y1AIM=((PXTRAY(5,NEWOBJ+1))+(WW1*JKY))
                  Y1AIM=(Y1AIM)-ALENS(31,NEWOBJ+1)
                  Z1AIM=0.0D0
                  XC=X1AIM
                  YC=Y1AIM
                  ZC=Z1AIM
                  XC1=XC
                  YC1=YC
                  ZC1=ZC
                  IF(XC.LT.0.0D0) DDELX=-DDELX
                  IF(YC.LT.0.0D0) DDELY=-DDELY
              END IF
          ELSE
C     NOT NULL
              IF(SYSTEM1(62).EQ.0.0D0) THEN
                  IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                      X1AIM=((REFRY(1,(NEWOBJ+1)))+(WW2*JKX))
                      Y1AIM=((REFRY(2,(NEWOBJ+1)))+(WW1*JKY))
                      Z1AIM=REFRY(3,NEWOBJ+1)
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                  ELSE
C     TEL ON
                      X1AIM=((REFRY(1,NEWOBJ))+(WW2*JKX))
                      Y1AIM=((REFRY(2,NEWOBJ))+(WW1*JKY))
                      Z1AIM=0.0D0
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                  END IF
              ELSE
                  X1AIM=REFRY(1,NEWOBJ+1)
                  Y1AIM=REFRY(2,NEWOBJ+1)
                  Z1AIM=REFRY(3,NEWOBJ+1)
                  XC=X1AIM
                  YC=Y1AIM
                  ZC=Z1AIM
                  XC1=XC
                  YC1=YC
                  ZC1=ZC
                  IF(XC.LT.0.0D0) DDELX=-DDELX
                  IF(YC.LT.0.0D0) DDELY=-DDELY
              END IF
          END IF
          XAIMOL=XC1
          YAIMOL=YC1
          ZAIMOL=ZC1
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
          R_TX=X1AIM
          R_TY=Y1AIM
          R_TZ=Z1AIM
          CALL BAKONE
          X1AIM=R_TX
          Y1AIM=R_TY
          Z1AIM=R_TZ
C
C       NOW THE STARTING DIRECTION COSINES ARE RELATIVELY SIMPLE.
C       THEY ARE CALLED LSTART,MSTART AND NSTART
C
C       FIRST CONVERT THE SURFACE 0 AND 1 COORDINATES INTO YANG AND XANG
C       ANGULAR VALUES AS IN HEXAGON AND CODE V.
C       YANG AND XANG ANGLES IN RADIANS.
          IF(DABS(Z1AIM-ZSTRT).EQ.0.0D0)THEN
              RAYCOD(1)=8
              RAYCOD(2)=NEWOBJ
              STOPP=1
              RETURN
          ELSE
              STOPP=0
C       PROCEED
          END IF
C
C       THE STARTING COORDINATES AND DIRECTION COSINES FOR THE VERY
C       FIRST PART OF THE RAY TRACE HAVE BEEN CALCULATED
C       BASED UPON THE RESULTS OF THE CURRENT PARAXIAL RAY TRACE AND
C       ASSUMING THAT SURFACE NEWOBJ+1 IS PLANO.
C
C       NEXT STEPS
C
C       1. TRANSLATE TO THE NEXT SURFACE
C       2. IF THAT SURFACE IS TILTED AND/OR DECENTERED,
C          TRANSFORM TO THAT SURFACES COORDINATE SYSTEM.
C       3. INTERSECT THE SURFACE
C       4. INTERACT WITH THAT SURFACE
C       5. REPEAT STEP 1.
C
C       1. TRANSFER TO NEXT SURFACES COORDINATE SYSTEM LOCATED
C       AT THE NEXT SURFACES VERTEX (THIS INCLUDES THE AXIAL THICKNESS).
C       AND TILTS AND DECENTERS.
C
C       CALL TRNSF2
C
C       2. INTERSECT AND INTERACT
C
C       CALL HITSUR
C
C       3. REPEAT WITH A DO LOOP
C
C       STORE THE RAY DATA FOR SURFACE 0 IN THE ARRAY
C       QRAY(1:9,0:MAXSUR)
C
C
C       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
C       SURFACE POINT INTERSECTION
 9        CONTINUE
          IF(ALENS(3,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
          IF(ALENS(3,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
          RV=.FALSE.
          KKK=KKK+1
C       IF KKK EXCEEDS 100 TRIES, PRINT RAY ITERRATION ERROR
C       AND STOP SEARCHING.
          IF(KKK.GT.NRAITR) THEN
              RAYCOD(1)=3
              RAYCOD(2)=NEWREF
              STOPP=1
              RETURN
          ELSE
              STOPP=0
C        PROCEED
          END IF
C
C       THIS IS THE RE-ENTRY POINT FOR RAY AIMING. RE-ENTRY
C       IS PERFORMED AFTER NEW VALUES OF X1AIM AND Y1AIM WERE
C       CALCULATED JUST BEFORE ENCOUNTERING THE (GO TO 9)
C       STATEMENT NEAR THE END OF THIS ROUTINE.
C
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
C
C       FOR SURFACE NEWOBJ
C       QRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
C       QRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
C       QRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
C       QRAY(4,SURF)=X(LOCAL)- L RAY DIR COS
C       QRAY(5,SURF)=Y(LOCAL)- M RAY DIR COS
C       QRAY(6,SURF)=Z(LOCAL)- N RAY DIR COS
C       QRAY(7,SURF)=X(LOCAL)- L SURFACE NORM
C       QRAY(8,SURF)=Y(LOCAL)- M SURFACE NORM
C       QRAY(9,SURF)=Z(LOCAL)- N SURFACE NORM
          QRAY(1,NEWOBJ)=XSTRT
          QRAY(2,NEWOBJ)=YSTRT
          QRAY(3,NEWOBJ)=ZSTRT
          QRAY(4,NEWOBJ)=LSTART
          QRAY(5,NEWOBJ)=MSTART
          QRAY(6,NEWOBJ)=NSTART
          QRAY(7,NEWOBJ)=0.0D0
          QRAY(8,NEWOBJ)=0.0D0
          QRAY(9,NEWOBJ)=1.0D0
          RAYRAY(1,NEWOBJ)=XSTRT
          RAYRAY(2,NEWOBJ)=YSTRT
          RAYRAY(3,NEWOBJ)=ZSTRT
          RAYRAY(4,NEWOBJ)=LSTART
          RAYRAY(5,NEWOBJ)=MSTART
          RAYRAY(6,NEWOBJ)=NSTART
C
          X=XSTRT
          Y=YSTRT
          Z=ZSTRT
          L=LSTART
          M=MSTART
          N=NSTART
C
          ISYS20=NEWIMG
          I=0
          DO 10 I=(NEWOBJ+1),ISYS20
C
C
C       WE ARE CURRENTLY AT SURFACE I-1 AND KNOW WHERE
C       WE ARE AND WHICH DIRECTION WE WANT TO GO.
C       CALLING TRNSF2.FOR TRANSFORMS THE X,Y,Z,L,M AND N INTO
C       THE COORDINATE SYSTEM OF SURFACE I,FOB0 TRACKS IF FOB 0 IS
C       USED OR FOB (SOMETHING NON-ZERO)
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
              XOLD=X
              YOLD=Y
              ZOLD=Z
              LOLD=L
              MOLD=M
              NOLD=N
C       XOLD,YOLD AND ZOLD ARE THE COORDINATES OF THE CURRENT RAY
C       AT SURFACE I-1 IN THE COORDINATE SYSTEM OF SURFACE I
C       NOW INTERSECT THE SURFACE
C
              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              R_XAIM=XAIMOL
              R_YAIM=YAIMOL
              R_ZAIM=ZAIMOL
              IF(SNIND2.GT.0.0D0) RAYRAY(24,I)=1.0D0
              IF(SNIND2.LT.0.0D0) RAYRAY(24,I)=-1.0D0
              IF(SNIND2.GT.0.0D0) POSRAY=.TRUE.
              IF(SNIND2.LT.0.0D0) POSRAY=.FALSE.
              CALL HITSUR
              IF(STOPP.EQ.1) THEN
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  RETURN
              END IF
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(STOPP.EQ.1) THEN
C NEW STUFF 6/2/94
                  IF(KKK.EQ.1.AND.RAYCOD(1).EQ.1) THEN
                      JKX=0.0D0
                      JKY=0.0D0
                      KKK=KKK+1
                      STOPP=0
                      GO TO 989
                  END IF
                  RETURN
              ELSE
                  STOPP=0
              END IF
C       LOAD REF RAY REGISTERS
C       FOR SURFACE NEWOBJ
C       QRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
C       QRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
C       QRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
C       QRAY(4,SURF)=X(LOCAL)- L RAY DIR COS
C       QRAY(5,SURF)=Y(LOCAL)- M RAY DIR COS
C       QRAY(6,SURF)=Z(LOCAL)- N RAY DIR COS
C       QRAY(7,SURF)=X(LOCAL)- L SURFACE NORM
C       QRAY(8,SURF)=Y(LOCAL)- M SURFACE NORM
C       QRAY(9,SURF)=Z(LOCAL)- N SURFACE NORM
              QRAY(1,I)=X
              QRAY(2,I)=Y
              QRAY(3,I)=Z
              QRAY(4,I)=L
              QRAY(5,I)=M
              QRAY(6,I)=N
              QRAY(7,I)=LN
              QRAY(8,I)=MN
              QRAY(9,I)=NN
              RAYRAY(1,I)=X
              RAYRAY(2,I)=Y
              RAYRAY(3,I)=Z
              RAYRAY(4,I)=L
              RAYRAY(5,I)=M
              RAYRAY(6,I)=N
C
C     WHAT IS THE SIGN OF THE INDEX IN THE I-1 SPACE
              IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) THEN
                  SNINDX=DABS(ALENS(45+INT(WW3),I-1))/ALENS(45+INT(WW3),I-1)
                  SNIND2=DABS(ALENS(45+INT(WW3),I))/ALENS(45+INT(WW3),I)
              END IF
              IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) THEN
                  SNINDX=DABS(ALENS(65+INT(WW3),I-1))/ALENS(65+INT(WW3),I-1)
                  SNIND2=DABS(ALENS(65+INT(WW3),I))/ALENS(65+INT(WW3),I)
              END IF
C
C     PHYSICAL LENGTH OF THE RAY FROM I-1 TO I IS THE MAG OF THE
C     DIST TRAVELED TIME THE SNINDX TIMES -1 IF RV IN THE SPACE
C     FROM I-1 TO I
C
C       CHECK THE RAY HEIGHT AT
C       NEWR AND ADJUST THE Y1AIM AND X1AIM AT SURFACE
C       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWR IS WITHIN
C       AIMTOL . MAXIMUN NUMBER OF ITERRATIONS IS
C       NRAITR. (DEFAULT IS 100).
C
              IF(I.EQ.NEWREF) THEN
C       CALCULATE TARX AND TARY
                  IF(DABS(ALENS(9,I)).GE.1.0D0.AND.
     1            DABS(ALENS(9,I)).LE.6.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
C     CLAPT=TRUE FOR CLAP TILTS AND DECENTED AND FALSE OTHERWISE
                      CLAPT=.FALSE.
                      IF(ALENS(12,I).NE.0.0D0.OR.ALENS(13,I).NE.0.0D0.OR.ALENS(15,I)
     1                .NE.0.0D0) CLAPT=.TRUE.
C       REF SURF HAS CLAP ON IT
C       THE VALUES OF TARY AND TARX ARE COORDINATES IN THE
C       LOCAL COORDINATE SYSTEM OF THE REFERENCE SYSTEM.
C       IF THE CLAP IS DECENTERED, THE TAR() VALUES ARE
C       MODIFIED BY ADDING THE CLAP DECENTRATIONS SINCE THE RAY
C       IS AIMED TO THE RELATIVE REFERENCE SURFACE COORDINATES.
C
C       SET TARGET TO CENTER OF DECENTERED CLAP, ALENS(12,I),
C       AND ALENS(13,I) ARE CLAP DECENTRATIONS
C
                      WWW1=WW1
                      WWW2=WW2
                      IF(SYSTEM1(70).EQ.1.0D0.AND.ALENS(1,I).NE.0.0D0.AND.
     1                ALENS(9,I).EQ.1.0D0.AND.ALENS(12,I).EQ.0.0D0.AND.
     2                ALENS(13,I).EQ.0.0D0.AND.ALENS(15,I).EQ.0.0D0) THEN
                          IF(DABS(1.0D0/ALENS(1,I)).GE.DABS(ALENS(10,I)).AND.
     1                    DABS(1.0D0/ALENS(1,I)).GE.DABS(ALENS(11,I)))
     1                    CALL APLANA(I,WW1,WW2,WWW1,WWW2)
                      END IF
C
C       CIRCULAR CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.1.0D0) THEN
                          IF(CLAPT) THEN
                              IF(ALENS(10,I).LE.ALENS(11,I)) THEN
                                  TARY=(ALENS(10,I)*WWW1)
                                  TARX=(ALENS(10,I)*WWW2)
                              ELSE
                                  TARY=(ALENS(11,I)*WWW1)
                                  TARX=(ALENS(11,I)*WWW2)
                              END IF
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
C     NO CLAP DEC OR TILTS
                              IF(ALENS(10,I).LE.ALENS(11,I)) THEN
                                  TARY=(ALENS(10,I)*WWW1)
                                  TARX=(ALENS(10,I)*WWW2)
                              ELSE
                                  TARY=(ALENS(11,I)*WWW1)
                                  TARX=(ALENS(11,I)*WWW2)
                              END IF
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
C       NOW IS THE REF SURF ORIENTATION ANGLE ?
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT CIRCULAR CLAP
                      END IF
C        RECT CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.2.0D0) THEN
                          IF(CLAPT) THEN
                              TARY=(ALENS(10,I)*WW1)
                              TARX=(ALENS(11,I)*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              TARY=(ALENS(10,I)*WW1)
                              TARX=(ALENS(11,I)*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT RECT CLAP
                      END IF
C        ELIP CLAP
C
                      YVALUE=ALENS(10,I)
                      XVALUE=ALENS(11,I)
C
                      IF(DABS(ALENS(9,I)).EQ.3.0D0) THEN
                          IF(CLAPT) THEN
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT ELIP CLAP
                      END IF
C        RCTK CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.4.0D0) THEN
                          IF(CLAPT) THEN
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(11,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(11,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT RCTK CLAP
                      END IF
C        POLY CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.5.0D0) THEN
                          IF(CLAPT) THEN
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(10,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(10,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT POLY CLAP
                      END IF
                      IF(DABS(ALENS(9,I)).EQ.6.0D0) THEN
                          IF(CLAPT) THEN
                              YVALUE=ALENS(14,I)
                              XVALUE=ALENS(14,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              YVALUE=ALENS(11,I)
                              XVALUE=ALENS(11,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT IPOLY CLAP
                      END IF
C
                  ELSE
C       NO CLAP ON REF SURF.
                      TARY=(PXTRAY(1,I)*WW1)
                      TARX=(PXTRAX(1,I)*WW2)
                      IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                      IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                      GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                      TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                      TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                      TARY=TARRY
                      TARX=TARRX
                  END IF
C
                  IF(DSQRT(((TARX-X)**2)+((TARY-Y)**2)).LE.AIMTOL
     1            .OR.SYSTEM1(62).EQ.0.0D0) THEN
C       AIM IS GOOD ENOUGH, PROCEED
                      AIMOK=.TRUE.
                      GO TO 100
                  ELSE
                      AIMOK=.FALSE.
C       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                  END IF
C
C       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
C       X1ONE AND Y1ONE ARE THE FIRST SET OF RAY
C       COORDINATES AT SURFACE 1
                  X1ONE=X1LAST
                  Y1ONE=Y1LAST
C       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
C       X1LAST AND Y1LAST ARE THE LAST RAY COORDINATES
C       AT SURFACE 1
                  X1LAST=XAIMOL
                  Y1LAST=YAIMOL
C       SET RXONE AND RYONE TO RXLAST AND RYLAST
C       RYONE ANE RXONE ARE THE FIRST SET OF RAY
C       COORDINATES AT THE REFERENCE SURFACE
                  RXONE=RXLAST
                  RYONE=RYLAST
C       SET RXLAST AND RYLAST TO X AND Y
C       RXLAST AND RYLAST ARE THE LAST X AND Y RAY COORDINATES
C       ON THE REFERENCE SURFACE.
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
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                      R_TX=X1AIM
                      R_TY=Y1AIM
                      R_TZ=Z1AIM
                      CALL BAKONE
                      X1AIM=R_TX
                      Y1AIM=R_TY
                      Z1AIM=R_TZ
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
 10       CONTINUE
          DO R_I=NEWOBJ+1,NEWIMG-1
C       HERE WE CHECK FOR BLOCKAGE BY CLAP OR COBS.
C       AND DO THE APPROPRIATE THINGS.
C       CALL CLAP CHECKING ROUTINE
              IF(DABS(ALENS(34,R_I)).NE.24.0D0) THEN
C     NO CLAP/COBS CHECK FOR TYPE 24 SPECIAL SURFACE
C
                  R_X=QRAY(1,R_I)
                  R_Y=QRAY(2,R_I)
                  R_Z=QRAY(3,R_I)
                  RAYCOD(1)=0
                  RAYCOD(2)=-1
                  MMSG=MSG
                  IF(INT(ALENS(127,R_I)).EQ.0.AND.INT(ALENS(128,R_I)).EQ.0) THEN
                      CALL CACHEK(0.0D0,0.0D0,0.0D0,0)
                  ELSE
                      IF(INT(ALENS(127,R_I)).NE.0) THEN
                          DO JK=1,INT(ALENS(127,R_I))
                              IF(MMSG) THEN
                                  MSG=.TRUE.
                                  IF(JK.LT.INT(ALENS(127,R_I))) MSG=.FALSE.
                              END IF
                              JK1=MULTCLAP(JK,1,R_I)
                              JK2=MULTCLAP(JK,2,R_I)
                              JK3=MULTCLAP(JK,3,R_I)
                              CALL CACHEK(JK1,JK2,JK3,1)
                              IF(RAYCOD(1).EQ.0) THEN
                                  SPDCD1=RAYCOD(1)
                                  SPDCD2=RAYCOD(2)
                                  STOPP=0
                                  RAYEXT=.TRUE.
                                  GO TO 25
                              END IF
                          END DO
 25                       CONTINUE
                      END IF
                      IF(INT(ALENS(128,R_I)).NE.0) THEN
                          DO JK=1,INT(ALENS(128,R_I))
                              IF(MMSG) THEN
                                  MSG=.TRUE.
                              END IF
                              JK1=MULTCOBS(JK,1,R_I)
                              JK2=MULTCOBS(JK,2,R_I)
                              JK3=MULTCOBS(JK,3,R_I)
                              CALL CACHEK(JK1,JK2,JK3,2)
                              IF(RAYCOD(1).NE.0) THEN
                                  SPDCD1=RAYCOD(1)
                                  SPDCD2=RAYCOD(2)
                                  STOPP=1
                                  RAYEXT=.FALSE.
                                  POLEXT=.FALSE.
                                  GO TO 26
                              END IF
                          END DO
 26                       CONTINUE
                      END IF
                  END IF
              END IF
              IF(.NOT.FOOT_TRACE.AND.RAYCOD(1).NE.0.AND.RAYCOD(1).NE.7) RETURN
              IF(FOOT_TRACE) THEN
                  IF(RAYCOD(1).EQ.6.OR.RAYCOD(1).EQ.7) THEN
                      IF(RAYCOD(2).EQ.NEWREF) RAYCD1=RAYCOD(1)
                      IF(RAYCOD(2).EQ.NEWREF) RAYCD2=RAYCOD(2)
                      IF(ALENS(58,RAYCOD(2)).EQ.1.0D0) THEN
                          RAYCD1=RAYCOD(1)
                          RAYCD2=RAYCOD(2)
                      END IF
                  END IF
                  IF(RAYCOD(1).NE.0) RAYCD3=RAYCOD(1)
              END IF
C       THIS ROUTINE SETS THE FLAGS
C       CAERAS AND COERAS
C       SET IF THE CURRENT SURFACE HAD A COBS OR
C       CLAP ERASE.
              STOPP=0
          END DO
          RETURN
      END
C SUB RAYDMP.FOR
      SUBROUTINE RAYDMP
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE CONTROLS THE OPERATION OF THE "RAYDUMP"
C       COMMAND
C
          INTEGER I
C
          REAL*8 ACLENG
C
          CHARACTER*17 POS,REV
C
          COMMON/ACLEN/ACLENG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)'"RAYDUMP" TAKES NO INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
C     FIRST PRINT A MESSAGE AS TO WHETHER OR NOT THE LAST RAY
C     FAILED OR NOT OR WAS NEVER TRACED.
C     IF RAYCOD(1)=0 AND RAYEXT FALSE
          IF(RAYCOD(1).EQ.0.AND..NOT.RAYEXT) THEN
C       NO RAY WAS TRACED, NO DATA EXISTS TO DUMP
              WRITE(OUTLYNE,5)
     1        'NO CURRENT RAY DATA EXISTS TO DUMP'
              CALL SHOWIT(0)
 5            FORMAT(A34)
              RETURN
          END IF
C     IF RAYCOD(1)=0 AND RAYCOD(2).EQ.NEWIMG.AND.RAYEXT.TRUE
C     RAY WAS TRACED SUCCESSFULLY
          IF(RAYCOD(1).EQ.0.AND.RAYEXT) THEN
C       RAY WAS TRACED SUCCESSFULLY
              WRITE(OUTLYNE,10)
     1        'CURRENT RAY WAS TRACED SUCCESSFULLY'
              CALL SHOWIT(0)
 10           FORMAT(A35)
C
C     HERE IS WHERE OUTPUT STATEMENTS GO FOR RAY DUMP
              DO I=NEWOBJ,NEWIMG

                  WRITE(OUTLYNE,100) I,RAYRAY(1,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,101) I,RAYRAY(2,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,102) I,RAYRAY(3,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,103) I,RAYRAY(4,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,104) I,RAYRAY(5,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,105) I,RAYRAY(6,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,106) I,RAYRAY(7,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,107) I,RAYRAY(8,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,108) I,RAYRAY(9,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,109) I,RAYRAY(10,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,110) I,RAYRAY(11,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,111) I,RAYRAY(12,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,112) I,RAYRAY(13,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,113) I,RAYRAY(14,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,114) I,RAYRAY(15,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,115) I,RAYRAY(16,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,116) I,RAYRAY(17,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,117) I,RAYRAY(18,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,118) I,RAYRAY(19,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,119) I,RAYRAY(20,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,120) I,RAYRAY(21,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,121) I,RAYRAY(22,I)
                  CALL SHOWIT(0)
                  IF(RAYRAY(23,I).EQ.1.0D0) REV= 'RAY NOT REVERSED '
                  IF(RAYRAY(23,I).EQ.-1.0D0) REV='RAY REVERSED     '

                  WRITE(OUTLYNE,122) I,REV
                  CALL SHOWIT(0)
                  IF(RAYRAY(24,I).EQ.1.0D0) POS= 'RAY IS POSRAY    '
                  IF(RAYRAY(24,I).EQ.-1.0D0) POS='RAY IS NOT POSRAY'

                  WRITE(OUTLYNE,122) I,POS
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,124) I,RAYRAY(25,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,125) I,RAYRAY(26,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,126) I,RAYRAY(27,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,127) I,RAYRAY(28,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,128) I,RAYRAY(29,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,129) I,RAYRAY(30,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,130) I,RAYRAY(31,I)
                  CALL SHOWIT(0)
 100              FORMAT('SURF=',I3,1X,'        X-COORDINATE=',D23.15)
 101              FORMAT('SURF=',I3,1X,'        Y-COORDINATE=',D23.15)
 102              FORMAT('SURF=',I3,1X,'        Z-COORDINATE=',D23.15)
 103              FORMAT('SURF=',I3,1X,'       L-DIR. COSINE=',D23.15)
 104              FORMAT('SURF=',I3,1X,'       M-DIR. COSINE=',D23.15)
 105              FORMAT('SURF=',I3,1X,'       N-DIR. COSINE=',D23.15)
 106              FORMAT('SURF=',I3,1X,'     PHYSICAL LENGTH=',D23.15)
 107              FORMAT('SURF=',I3,1X,'OPL-(SURF-1 TO SURF)=',D23.15)
 108              FORMAT('SURF=',I3,1X,'           COSINE(I)=',D23.15)
 109              FORMAT('SURF=',I3,1X,'          COSINE(IP)=',D23.15)
 110              FORMAT('SURF=',I3,1X,'   XZ-SLOPE(RADIANS)=',D23.15)
 111              FORMAT('SURF=',I3,1X,'   YZ-SLOPE(RADIANS)=',D23.15)
 112              FORMAT('SURF=',I3,1X,'  L-(SURFACE NORMAL)=',D23.15)
 113              FORMAT('SURF=',I3,1X,'  M-(SURFACE NORMAL)=',D23.15)
 114              FORMAT('SURF=',I3,1X,'  N-(SURFACE NORMAL)=',D23.15)
 115              FORMAT('SURF=',I3,1X,' X-(PRE-INTERACTION)=',D23.15)
 116              FORMAT('SURF=',I3,1X,' Y-(PRE-INTERACTION)=',D23.15)
 117              FORMAT('SURF=',I3,1X,' Z-(PRE-INTERACTION)=',D23.15)
 118              FORMAT('SURF=',I3,1X,' L-(PRE-INTERACTION)=',D23.15)
 119              FORMAT('SURF=',I3,1X,' M-(PRE-INTERACTION)=',D23.15)
 120              FORMAT('SURF=',I3,1X,' N-(PRE-INTERACTION)=',D23.15)
 121              FORMAT('SURF=',I3,1X,'OPL-(NEWOBJ TO SURF)=',D23.15)
 122              FORMAT('SURF=',I3,1X,A17)
 124              FORMAT('SURF=',I3,1X,'     RAY ENERGY TERM=',D23.15)
 125              FORMAT('SURF=',I3,1X,'       XL DIR COSINE=',D23.15)
 126              FORMAT('SURF=',I3,1X,'       XM DIR COSINE=',D23.15)
 127              FORMAT('SURF=',I3,1X,'       XN DIR COSINE=',D23.15)
 128              FORMAT('SURF=',I3,1X,'       YL DIR COSINE=',D23.15)
 129              FORMAT('SURF=',I3,1X,'       YM DIR COSINE=',D23.15)
 130              FORMAT('SURF=',I3,1X,'       YN DIR COSINE=',D23.15)
              END DO
C
              RETURN
          END IF
C     IF RAYCOD(1)NOT=0 RAY WAS TRACED UNSUCCESSFULLY TO SURFACE
C     RAYCOD(2)
          IF(RAYCOD(1).NE.0) THEN
C       RAY WAS TRACED UNSUCCESSFULLY TO SURFACE RAYCOD(2)
              WRITE(OUTLYNE,15)
     1        'CURRENT RAY WAS TRACED UNSUCCESSFULLY AND FAILED AT SURFACE # '
     2        ,RAYCOD(2)
              CALL SHOWIT(0)
 15           FORMAT(A62,I3)
C
              IF(RAYCOD(1).EQ.1) THEN
                  WRITE(OUTLYNE,*)'RAY DID NOT INTERSECT THE SURFACE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.2) THEN
                  WRITE(OUTLYNE,*)
     1            'CONVERGENCE FAILURE OCCURRED DURING RAY INTERSECTION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'WITH ASPHERIC, TORIC OR SPECIAL SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'ACCURACY OF INTERSECTION WAS = ',ACLENG
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.3) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY FAILED TO CONVERGE TO REFERENCE SURFACE RAY-AIM POINT'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.4) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'TOTAL INTERNAL REFLECTION'
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(RAYCOD(1).EQ.5) THEN
                  WRITE(OUTLYNE,*)
     1            'ANGLE OF DIFFRACTION AT THE GRATING IS PHYSICALLY UNREALIZABLE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.6) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY BLOCKED BY CLEAR APERTURE ERASE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.7) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY BLOCKED BY OBSCURATION ERASE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.8) THEN
                  WRITE(OUTLYNE,*)
     1            'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) '10',RAYCOD(1),RAYCOD(2)
                  CALL SHOWIT(1)
              END IF
              IF(RAYCOD(1).EQ.9) THEN
                  WRITE(OUTLYNE,*)
     1            'ANGLE OF DIFFRACTION AT THE HOE IS PHYSICALLY UNREALIZABLE'
                  CALL SHOWIT(0)
              END IF

              IF(RAYCOD(1).EQ.10) THEN
                  WRITE(OUTLYNE,*)
     1            'HOE CONSTRUCTION POINTS CONFLICT WITH TRANSMISSION/REFLECTION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'MODE OR CONSTRUCTION POINTS ARE NOT DEFINED'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.11) THEN
                  WRITE(OUTLYNE,*)
     1            'REFERENCE RAY CAN NOT BE TRACED AT ZERO WAVELENGTH '
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.12) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY CAN NOT BE TRACED AT ZERO WAVELENGTH '
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.13) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY MISSED GRAZING INCIDENCE SURFACE SECTION'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.14) THEN
                  WRITE(OUTLYNE,*)
     1            'ILLUMINATION RAY BLOCKED BY CLEAR APERTURE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.15) THEN
                  WRITE(OUTLYNE,*)
     1            'NO GRID FILE EXISTS FOR THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.16) THEN
                  WRITE(OUTLYNE,*)
     1            'SPECIFIED OBJECT POINT DOES NOT EXIST'
                  CALL SHOWIT(0)
              END IF
C
C     HERE IS WHERE OUTPUT STATEMENTS GO FOR RAY DUMP
              DO I=NEWOBJ,RAYCOD(2)
                  IF(I.EQ.RAYCOD(2))WRITE(OUTLYNE,*)
     1            'DATA FOR THIS LAST SURFACE IS SUSPECT'
                  IF(I.EQ.RAYCOD(2)) CALL SHOWIT(0)

                  WRITE(OUTLYNE,100) I,RAYRAY(1,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,101) I,RAYRAY(2,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,102) I,RAYRAY(3,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,103) I,RAYRAY(4,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,104) I,RAYRAY(5,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,105) I,RAYRAY(6,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,106) I,RAYRAY(7,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,107) I,RAYRAY(8,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,108) I,RAYRAY(9,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,109) I,RAYRAY(10,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,110) I,RAYRAY(11,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,111) I,RAYRAY(12,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,112) I,RAYRAY(13,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,113) I,RAYRAY(14,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,114) I,RAYRAY(15,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,115) I,RAYRAY(16,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,116) I,RAYRAY(17,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,117) I,RAYRAY(18,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,118) I,RAYRAY(19,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,119) I,RAYRAY(20,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,120) I,RAYRAY(21,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,121) I,RAYRAY(22,I)
                  CALL SHOWIT(0)

                  IF(RAYRAY(23,I).EQ.1.0D0) REV= 'RAY NOT REVERSED '
                  IF(RAYRAY(23,I).EQ.-1.0D0) REV='RAY REVERSED     '

                  WRITE(OUTLYNE,122) I,REV
                  CALL SHOWIT(0)
                  IF(RAYRAY(24,I).EQ.1.0D0) POS= 'RAY IS POSRAY    '
                  IF(RAYRAY(24,I).EQ.-1.0D0) POS='RAY IS NOT POSRAY'

                  WRITE(OUTLYNE,122) I,POS
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,124) I,RAYRAY(25,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,125) I,RAYRAY(26,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,126) I,RAYRAY(27,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,127) I,RAYRAY(28,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,128) I,RAYRAY(29,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,129) I,RAYRAY(30,I)
                  CALL SHOWIT(0)

                  WRITE(OUTLYNE,130) I,RAYRAY(31,I)
                  CALL SHOWIT(0)
                  IF(RAYRAY(23,I).EQ.1.0D0) REV= 'RAY NOT REVERSED '
                  IF(RAYRAY(23,I).EQ.-1.0D0) REV='RAY REVERSED     '

                  WRITE(OUTLYNE,122) I,REV
                  CALL SHOWIT(0)
                  IF(RAYRAY(24,I).EQ.1.0D0) POS= 'RAY IS POSRAY    '
                  IF(RAYRAY(24,I).EQ.-1.0D0) POS='RAY IS NOT POSRAY'

                  WRITE(OUTLYNE,122) I,POS
                  CALL SHOWIT(0)
              END DO
C
              RETURN
          END IF
          RETURN
      END
C SUB RAYTRA.FOR

      SUBROUTINE RAYTRA
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE RAYTRA.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE TRACING OF A RAY. THIS IS INITIATED
C       BY THE "RAY" COMMAND.
C
          INTEGER IPASS1,JK,I,ISYS20,KKK,J,ISURF,IK,WA3

          INTEGER CAERAS,COERAS,N_HITS
C
          REAL*8 OPLXCOR(1:2),OPLYCOR(1:2),OPLZCOR(1:2)
C
          COMMON/COROPL/OPLXCOR,OPLYCOR,OPLZCOR
C
          REAL*8 X,Y,Z,L,M,N,WW1W,WW2W,TANN1,TANN2,IA,IAP
     1    ,D21,D22,GAMMA,TWW1,TWW2,XL,XM,XN,YL,YM,YN,RN1,RN2,
     2    WWW1,WWW2,D11,D12,LS,SNINDX,SNIND2,WW1WW,WW2WW,JK1,JK2,JK3,
     3    LSTART,MSTART,NSTART,YANG,XANG,X1LAST,LARGE,ENERGY_FACTOR,
     4    Y1LAST,X1ONE,Y1ONE,RXLAST,RYLAST,RXONE,RYONE,DDELX,DDELY,
     5    XC1,YC1,ZC1,MF1,MF2,TARX,TARY,XVALUE,YVALUE,
     6    TEST,MAG,LOLD,MOLD,NOLD,TARRY,TARRX,JKX,JKY,
     7    JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2,JK_CPL,JK_CPM,JK_CPN
     7    ,SA_CPL,SA_CPM,SA_CPN,DP,POLANG,FACT_PAR,FACT_PER,PHASE_PAR
     8    ,PHASE_PER,PATHL,STEPL,STEPL1
C
          COMMON/CACO/CAERAS,COERAS,LS
C
C     VARIABLES FOR SPOT TRACING
          LOGICAL SPDTRA,MMSG
          LOGICAL AIMOK,CLAPT,OLDPASS,GERROR,DELFAIL
          COMMON/PASSOLD/OLDPASS
C
          INTEGER SPDCD1,SPDCD2
C
          COMMON/SPRA1/SPDTRA
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          REAL*8 XPASS,YPASS,ZPASS
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS

          REAL*8 AOI,D,H,S,FACTOR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'

C
          KKK=0
C                       DDELX=0.001D0*SYSTEM1(12)
C                       DDELY=0.001D0*SYSTEM1(13)
          DDELX=0.001D0
          DDELY=0.001D0
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
C       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
C       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
C       RAY AIMING.
C
C
          IF(WW3.GE.1.0D0.AND.WW3.LE.5.0D0) THEN
              IF(SYSTEM1(INT(WW3)).EQ.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  RAYCOD(1)=12
                  RAYCOD(2)=NEWOBJ
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  RETURN
C       PROCEED
              END IF
C       PROCEED
          END IF
          IF(WW3.GE.6.0D0.AND.WW3.LE.10.0D0) THEN
              IF(SYSTEM1(65+INT(WW3)).EQ.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
                      CALL SHOWIT(1)
                  END IF
                  STOPP=1
                  RAYCOD(1)=12
                  RAYCOD(2)=NEWOBJ
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  RETURN
              END IF
          END IF
C
C       SET RAYCOD DEFAULTS
          RAYCOD(1)=-1
          RAYCOD(2)=-1
C
          AIMOK=.FALSE.
C
          RELY=WW1
          RELX=WW2
C
C       SET INITIAL VALUES FOR X1ONE, Y1ONE, X1LAST AND Y1LAST
C
          LARGE=-99999.9D0
          X1ONE=LARGE
          Y1ONE=LARGE
          X1LAST=LARGE
          Y1LAST=LARGE
C
C       SET INITIAL VALUES FOR RXONE, RYONE, RXLAST AND RYLAST
          RXONE=LARGE
          RYONE=LARGE
          RXLAST=LARGE
          RYLAST=LARGE
C
C       THE FIRST THING TO DO IS TO DETERMINE THE STARTING
C       COORDINATES OF THE RAY AT THE OBJECT SURFACE.
C       THE STARTING RAY COORDINATES AT SURFACE NEWOBJ
C       ARE JUST THE RAY COORDINATES AT THIS SURFACE
C
C
          XSTRT=REFRY(1,NEWOBJ)
          YSTRT=REFRY(2,NEWOBJ)
          ZSTRT=REFRY(3,NEWOBJ)
C
C       THE WAVELENGTH NUMBER FOR THE RAY TRACE IS:
C       INT(WW3) OR INT(SYSTEM1(11))
C
C       NOW WHAT ARE THE STARTING DIRECTION COSINES AND HOW ARE
C       THEY DETERMINED?
C
C       START BY LOOKING AT THE PY+ PCY AND PX+PCX
C       VALUES AT SURFACE NEWOBJ+1.
C       THESE VALUES CORRESPOND TO THE INTERSECTIONS OF CHIEF
C       PLUS MARGINAL RAYS AT FULL 1.0 FRACTIONAL OBJECT HEIGHTS
C       AND FULL REFERENCE APERTURE HEIGHTS.
C       FOR FOB 1.0 1.0 THE TARGET RAY HEIGHTS AT SURFACE NEWOBJ+1
C       FOR THE FULL APERTURE RAY ARE
C       ARE X=PX(NEWOBJ+1)+PCX(NEWOBJ+1)
C       AND Y=PY(NEWOBJ+1)+PCY(NEWOBJ+1).
C
C       IN GENERAL THE RAY INTERESECTION
C       POINTS FOR FIRST GUESS AIMING WILL BE:
C       THE POINT AT SURFACE NEWOBJ+1 AT WHICH THE
C       PARAXIAL CHIEF RAY PLUS THE PARAXIAL MARGINAL RAY
          IF(SYSTEM1(63).EQ.0.0D0) THEN
C       TELECENTRIC AIMING IS OFF
              JKX=(PXTRAX(1,NEWOBJ+1))
              JKY=(PXTRAY(1,NEWOBJ+1))
              IF(ALENS(9,NEWOBJ+1).EQ.1.0D0) THEN
C     CIRCULAR AP
                  IF(ALENS(10,NEWOBJ+1).LE.ALENS(11,NEWOBJ+1)) THEN
                      IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1                JKX=DABS(ALENS(10,NEWOBJ+1))
                      IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1                JKY=DABS(ALENS(10,NEWOBJ+1))
                  ELSE
                      IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1                JKX=DABS(ALENS(11,NEWOBJ+1))
                      IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1                JKY=DABS(ALENS(11,NEWOBJ+1))
                  END IF
              END IF
              IF(ALENS(9,NEWOBJ+1).EQ.5.0D0) THEN
C     CIRCULAR AP
                  IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1            JKX=DABS(ALENS(10,NEWOBJ+1))
                  IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1            JKY=DABS(ALENS(10,NEWOBJ+1))
              END IF
              IF(ALENS(9,NEWOBJ+1).EQ.6.0D0) THEN
C     CIRCULAR AP
                  IF(DABS(ALENS(14,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1            JKX=DABS(ALENS(14,NEWOBJ+1))
                  IF(DABS(ALENS(14,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1            JKY=DABS(ALENS(14,NEWOBJ+1))
              END IF
              IF(ALENS(9,NEWOBJ+1).GT.1.0D0.AND.ALENS(9,NEWOBJ+1)
     1        .LE.4.0D0) THEN
C     OTHER AP
                  IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1            JKX=DABS(ALENS(11,NEWOBJ+1))
                  IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1            JKY=DABS(ALENS(10,NEWOBJ+1))
              END IF
          ELSE
C       TELECENTRIC AIMING IS ON
              JKX=(PXTRAX(1,NEWOBJ+1))
              JKY=(PXTRAY(1,NEWOBJ+1))
          END IF
C
 989      CONTINUE
          IF(RAYCOD(1).EQ.1.AND.KKK.GT.1) THEN
              STOPP=1
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              FAIL=.TRUE.
              RETURN
          END IF
          IF(.NOT.ITRACE) THEN
              IF(NULL.AND..NOT.REFEXT) THEN
C     NULL WITH FAILED CHIEF RAY
                  IF(SYSTEM1(62).EQ.0.0D0) THEN
C     NO RAY AIMING
                      IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
C     RAY AIMING IS OFF, TELECENTRIC RAY AIMING IS OFF
                          X1AIM=WW2*JKX
                          X1AIM=(X1AIM)-ALENS(31,NEWOBJ+1)
                          Y1AIM=WW1*JKY
                          Y1AIM=(Y1AIM)-ALENS(30,NEWOBJ+1)
                          Z1AIM=0.0D0
                          XC=X1AIM
                          YC=Y1AIM
                          ZC=Z1AIM
                          XC1=XC
                          YC1=YC
                          ZC1=ZC
C
                      ELSE
C     TEL ON
                          IF(SYSTEM1(16).NE.0.0D0)
     1                    X1AIM=(LFOB(2)*DABS(PXTRAX(5,NEWOBJ)))+(WW2*JKX)
                          IF(SYSTEM1(16).EQ.0.0D0)
     1                    X1AIM=(WW2*JKX)
                          IF(SYSTEM1(14).NE.0.0D0)
     1                    Y1AIM=(LFOB(1)*(PXTRAY(5,NEWOBJ)))+(WW1*JKY)
                          IF(SYSTEM1(14).EQ.0.0D0)
     1                    Y1AIM=(WW1*JKY)
                          Z1AIM=0.0D0
                          XC=X1AIM
                          YC=Y1AIM
                          ZC=Z1AIM
                          XC1=XC
                          YC1=YC
                          ZC1=ZC
                      END IF
                  ELSE
C     RAY AIMING
                      X1AIM=(LFOB(2)*DABS(PXTRAX(5,NEWOBJ+1)))+(WW2*JKX)
                      X1AIM=(X1AIM)-ALENS(31,NEWOBJ+1)
                      Y1AIM=(LFOB(1)*DABS(PXTRAY(5,NEWOBJ+1)))+(WW1*JKY)
                      Y1AIM=(Y1AIM)-ALENS(30,NEWOBJ+1)
                      Z1AIM=0.0D0
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                      IF(XC.LT.0.0D0) DDELX=-DDELX
                      IF(YC.LT.0.0D0) DDELY=-DDELY
                  END IF
              END IF
C     CHIEF RAY EXISTS
              IF(SYSTEM1(62).EQ.0.0D0) THEN
C     NO RAY AIMING
                  IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                      X1AIM=((REFRY(1,(NEWOBJ+1)))+(WW2*JKX))
                      Y1AIM=((REFRY(2,(NEWOBJ+1)))+(WW1*JKY))
                      Z1AIM=REFRY(3,(NEWOBJ+1))
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                  ELSE
C     TEL ON
                      X1AIM=((REFRY(1,NEWOBJ))+(WW2*JKX))
                      Y1AIM=((REFRY(2,NEWOBJ))+(WW1*JKY))
                      Z1AIM=0.0D0
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                  END IF
              ELSE
C     RAY AIMING
                  X1AIM=REFRY(1,NEWOBJ+1)
                  Y1AIM=REFRY(2,NEWOBJ+1)
                  Z1AIM=REFRY(3,NEWOBJ+1)
                  XC=X1AIM
                  YC=Y1AIM
                  ZC=Z1AIM
                  XC1=XC
                  YC1=YC
                  ZC1=ZC
                  IF(XC.LT.0.0D0) DDELX=-DDELX
                  IF(YC.LT.0.0D0) DDELY=-DDELY
              END IF
              XAIMOL=XC1
              YAIMOL=YC1
              ZAIMOL=ZC1
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
              R_TX=X1AIM
              R_TY=Y1AIM
              R_TZ=Z1AIM
              CALL BAKONE
              X1AIM=R_TX
              Y1AIM=R_TY
              Z1AIM=R_TZ
          END IF
          IF(ITRACE) THEN
C     ILLUMINATION TRACE
C     TANGENTS OF CHIEF RAY SLOPE
              TANN1=REFRY(4,0)/REFRY(6,0)
              TANN2=REFRY(5,0)/REFRY(6,0)
          END IF
C
C       NOW THE STARTING DIRECTION COSINES ARE RELATIVELY SIMPLE.
C       THEY ARE CALLED LSTART,MSTART AND NSTART
C
C       FIRST CONVERT THE SURFACE 0 AND 1 COORDINATES INTO YANG AND XANG
C       ANGULAR VALUES AS IN HEXAGON AND CODE V.
C       YANG AND XANG ANGLES IN RADIANS.
          STOPP=0
          RAYEXT=.TRUE.
          FAIL=.FALSE.
C
C       THE STARTING COORDINATES AND DIRECTION COSINES FOR THE VERY
C       FIRST PART OF THE RAY TRACE HAVE BEEN CALCULATED
C       BASED UPON THE RESULTS OF THE CURRENT PARAXIAL RAY TRACE AND
C       ASSUMING THAT SURFACE NEWOBJ+1 IS PLANO.
C
C       NEXT STEPS
C
C       1. TRANSLATE TO THE NEXT SURFACE
C       2. IF THAT SURFACE IS TILTED AND/OR DECENTERED,
C          TRANSFORM TO THAT SURFACES COORDINATE SYSTEM.
C       3. INTERSECT THE SURFACE
C       4. INTERACT WITH THAT SURFACE
C       5. REPEAT STEP 1.
C
C       1. TRANSFER TO NEXT SURFACES COORDINATE SYSTEM LOCATED
C       AT THE NEXT SURFACES VERTEX (THIS INCLUDES THE AXIAL THICKNESS).
C       AND TILTS AND DECENTERS.
C
C       CALL TRNSF2
C
C       2. INTERSECT AND INTERACT
C
C       CALL HITSUR
C
C       3. REPEAT WITH A DO LOOP
C
C       STORE THE RAY DATA FOR SURFACE 0 IN THE ARRAY
C       RAYRAY(1:50,0:MAXSUR)
C
C       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
C       SURFACE POINT INTERSECTION
 9        CONTINUE
          IF(ALENS(3,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
          IF(ALENS(3,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
          RV=.FALSE.
          KKK=KKK+1
C       IF KKK EXCEEDS 100 TRIES, PRINT RAY ITERRATION ERROR
C       AND STOP SEARCHING.
          IF(KKK.GT.NRAITR) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)
     1              'RAY FAILURE OCCURRED AT SURFACE ',NEWREF
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RAY FAILED TO CONVERGE TO REFERENCE SURFACE RAY-AIM POINT'
                  CALL SHOWIT(1)
              END IF
              RAYCOD(1)=3
              RAYCOD(2)=NEWREF
              SPDCD1=RAYCOD(1)
              SPDCD2=NEWREF
              STOPP=1
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              FAIL=.TRUE.
              RETURN
          ELSE
              STOPP=0
              RAYEXT=.TRUE.
              FAIL=.FALSE.
C        PROCEED
          END IF
C
C       THIS IS THE RE-ENTRY POINT FOR RAY AIMING. RE-ENTRY
C       IS PERFORMED AFTER NEW VALUES OF X1AIM AND Y1AIM WERE
C       CALCULATED JUST BEFORE ENCOUNTERING THE (GO TO 9)
C       STATEMENT NEAR THE END OF THIS ROUTINE.
C     COMPUTE DIR COS DIRECTLY FROM POSITIONS
C
          IF(.NOT.ITRACE) THEN
C     NOT ILLUMINATION TRACING
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
          ELSE
C
C     ILLUMINATION TRACING
C
              IF(WW1.GT.89.9999D0.AND.
     1        WW1.LT.90.0001D0)  WW1=89.9999D0
C
C     CONVERT TO RADIANS
              TWW1=WW1
              TWW2=WW2
              WW1=WW1*PII/180.0D0
              WW2=WW2*PII/180.0D0
C
C     CONVERT WW1 AND WW2 FROM THETA AND PHI TO
C     XANG AND YANG
              WW1WW=DTAN(WW1)*DSIN(WW2)
              WW2WW=DTAN(WW1)*DCOS(WW2)
C     CALC X AND Y TANGENTS OF REGULAR RAY INCLUDING CHIEF RAY ANGLE
              WW1W=(WW1WW+TANN2)/(1.0D0-(WW1WW*TANN2))
              WW2W=(WW2WW+TANN1)/(1.0D0-(WW2WW*TANN1))
C
C
C     DETERMINE LSTART,MSTART AND NSTART DIRECTLY FRON WW1 AND WW2
              NSTART=DSQRT(1.0D0/(((WW1W)**2)+((WW2W)**2)+1.0D0))
              IF(TWW1.GT.90.0D0) NSTART=-NSTART
              LSTART=NSTART*(WW2W)
              MSTART=NSTART*(WW1W)
              MAG=DSQRT((LSTART**2)+(MSTART**2)+(NSTART**2))
              LSTART=LSTART/MAG
              MSTART=MSTART/MAG
              NSTART=NSTART/MAG
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
C     CALCULATE X1AIM,Y1AIM AND Z1AIM IN THE COORDINATE SYSTEM OBJ SURF
C     DON'T NEED BAKONE HERE
              X1AIM=WW2WW*ALENS(3,0)
              Y1AIM=WW1WW*ALENS(3,0)
              Z1AIM=0.0D0
              XC=X1AIM
              YC=Y1AIM
              ZC=Z1AIM
              XC1=XC
              YC1=YC
              ZC1=ZC
              XAIMOL=XC1
              YAIMOL=YC1
              ZAIMOL=ZC1
          END IF
C
C       FOR SURFACE NEWOBJ
C       RAYRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(4,SURF)=L(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(5,SURF)=M(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(6,SURF)=N(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(8,SURF)=LEN - PHYSICAL LENGTH ALONG RAY FROM SURF I-1 TO I
C       RAYRAY(7,SURF)=OPL - OPL FROM SURFACE I-1 TO I
C     ZERO IF I-1 IS PERFECT
C       RAYRAY(9,SURF)=COSINE(I)
C       RAYRAY(10,SURF)=COSINE(IP)
C       RAYRAY(11,SURF)=UX - (RADIAN) MARGINAL XZ SLOPE ANGLE
C       RAYRAY(12,SURF)=UY - (RADIAN) MARGINAL YZ SLOPE ANGLE
C       RAYRAY(13,SURF)=LN - SURFACE NORMAL DIRECTION COSINE
C       RAYRAY(14,SURF)=MN - SURFACE NORMAL DIRECTION COSINE
C       RAYRAY(15,SURF)=NN - SURFACE NORMAL DIRECTION COSINE
C       RAYRAY(16,SURF)=XOLD  The next 6 items are the ray values
C       RAYRAY(17,SURF)=YOLD  just before interaction with the surface
C       RAYRAY(18,SURF)=ZOLD  in the surface local coordinate system.
C       RAYRAY(19,SURF)=OLDL
C       RAYRAY(20,SURF)=OLDM
C       RAYRAY(21,SURF)=OLDN
C     THESE LAST 6 ITEMS ARE THE COORDINATES OF THE RAY AT
C     SURFACE I-1 IN THE COORDINATE SYSTEM OF SURFACE I
C       RAYRAY(22,SURF)=OPL - OPL FROM SURFACE NEWOBJ TO I
C     OR TO I-1 IF I-1 IS "PERFECT'
C       RAYRAY(23,SURF)=1 FOR NOT RV, -1 FOR RV
C       RAYRAY(24,SURF)=1.0D0 FOR POSRAY,-1 FOR NEG RAY
C       RAYRAY(25,SURF)=RAY ENERGY TERM
C       RAYRAY(26,SURF)=RAY XL DIR COS
C       RAYRAY(27,SURF)=RAY XM DIR COS
C       RAYRAY(28,SURF)=RAY XN DIR COS
C       RAYRAY(29,SURF)=RAY YL DIR COS
C       RAYRAY(30,SURF)=RAY YM DIR COS
C       RAYRAY(31,SURF)=RAY YN DIR COS
C       RAYRAY(34,SURF)=FACT_PAR
C       RAYRAY(35,SURF)=FACT_PER
C       RAYRAY(36,SURF)=PHASE_PAR
C       RAYRAY(37,SURF)=PHASE_PER
C       RAYRAY(38,SURF)=POLARIZATION ANGLE IN DEGREES BETWEEN Y-RAY VECTOR AND PARALLEL PLANE
C       RAYRAY(39,SURF)=Y-COMPONENT OF THE ANGLE OF INCIDENCE
C       RAYRAY(40,SURF)=X-COMPONENT OF THE ANGLE OF INCIDENCE
C       RAYRAY(41,SURF) TO RAYRAY(50,SURF) RESERVED
          RAYRAY(34:35,NEWOBJ)=1.0D0
          RAYRAY(36:38,NEWOBJ)=1.0D0
          RAYRAY(32,NEWOBJ)=WW3
          RAYRAY(1,NEWOBJ)=XSTRT
          RAYRAY(2,NEWOBJ)=YSTRT
          RAYRAY(3,NEWOBJ)=ZSTRT
          RAYRAY(4,NEWOBJ)=LSTART
          RAYRAY(5,NEWOBJ)=MSTART
          RAYRAY(6,NEWOBJ)=NSTART
          RAYRAY(7,NEWOBJ)=0.0D0
          RAYRAY(8,NEWOBJ)=0.0D0
          IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) THEN
              SNIND2=DABS(ALENS(45+INT(WW3),NEWOBJ))/ALENS(45+INT(WW3),NEWOBJ)
              RN1=(ALENS(45+INT(WW3),NEWOBJ))
              RN2=(ALENS(45+INT(WW3),NEWOBJ))
          END IF
          IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) THEN
              SNIND2=DABS(ALENS(65+INT(WW3),NEWOBJ))/ALENS(65+INT(WW3),NEWOBJ)
              RN1=(ALENS(65+INT(WW3),NEWOBJ))
              RN2=(ALENS(65+INT(WW3),NEWOBJ))
          END IF
          IF(SNIND2.GT.0.0D0) RAYRAY(24,NEWOBJ)=1.0D0
          IF(SNIND2.LT.0.0D0) RAYRAY(24,NEWOBJ)=-1.0D0
          IF(SNIND2.GT.0.0D0) POSRAY=.TRUE.
          IF(SNIND2.LT.0.0D0) POSRAY=.FALSE.
          RAYRAY(9,NEWOBJ)=NSTART
          RAYRAY(10,NEWOBJ)=NSTART
          IA=DACOS(RAYRAY(9,NEWOBJ))
          IAP=DACOS(RAYRAY(10,NEWOBJ))
          RAYRAY(11,NEWOBJ)=XANG
          IF((RAYRAY(11,NEWOBJ)).LT.0.0D0) RAYRAY(11,NEWOBJ)=
     1    RAYRAY(11,NEWOBJ)+(TWOPII)
          RAYRAY(12,NEWOBJ)=YANG
          IF((RAYRAY(12,NEWOBJ)).LT.0.0D0) RAYRAY(12,NEWOBJ)=
     1    RAYRAY(12,NEWOBJ)+(TWOPII)
          RAYRAY(13,NEWOBJ)=0.0D0
          RAYRAY(14,NEWOBJ)=0.0D0
          RAYRAY(15,NEWOBJ)=1.0D0
          RAYRAY(16,NEWOBJ)=XSTRT
          RAYRAY(17,NEWOBJ)=YSTRT
          RAYRAY(18,NEWOBJ)=ZSTRT
          RAYRAY(19,NEWOBJ)=LSTART
          RAYRAY(20,NEWOBJ)=MSTART
          RAYRAY(21,NEWOBJ)=NSTART
          RAYRAY(22,NEWOBJ)=0.0D0
          RAYRAY(26,NEWOBJ)=(1.0D0*DCOS(XANG))
          RAYRAY(27,NEWOBJ)=0.0D0
          RAYRAY(28,NEWOBJ)=-(1.0D0*DSIN(XANG))
          RAYRAY(29,NEWOBJ)=0.0D0
          RAYRAY(30,NEWOBJ)=(1.0D0*DCOS(XANG))
          RAYRAY(31,NEWOBJ)=-(1.0D0*DSIN(YANG))

C
C
          X=XSTRT
          Y=YSTRT
          Z=ZSTRT
          L=LSTART
          M=MSTART
          N=NSTART
          XL=(1.0D0*DCOS(XANG))
          XM=0.0D0
          XN=-(1.0D0*DSIN(XANG))
          YL=0.0D0
          YM=(1.0D0*DCOS(XANG))
          YN=-(1.0D0*DSIN(YANG))
C
          ISYS20=NEWIMG
          I=0
          DO 10 I=(NEWOBJ+1),ISYS20

C
C
C       WE ARE CURRENTLY AT SURFACE I-1 AND KNOW WHERE
C       WE ARE AND WHICH DIRECTION WE WANT TO GO.
C       CALLING TRNSF2.FOR TRANSFORMS THE X,Y,Z,L,M AND N INTO
C       THE COORDINATE SYSTEM OF SURFACE I,FOB0 TRACKS IF FOB 0 IS
C       USED OR FOB (SOMETHING NON-ZERO)
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
              XOLD=X
              YOLD=Y
              ZOLD=Z
              LOLD=L
              MOLD=M
              NOLD=N
C       XOLD,YOLD AND ZOLD ARE THE COORDINATES OF THE CURRENT RAY
C       AT SURFACE I-1 IN THE COORDINATE SYSTEM OF SURFACE I
C       NOW INTERSECT THE SURFACE
C
              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              R_XAIM=XAIMOL
              R_YAIM=YAIMOL
              R_ZAIM=ZAIMOL
              IF(RAYRAY(24,R_I-1).GT.0.0D0) POSRAY=.TRUE.
              IF(RAYRAY(24,R_I-1).LT.0.0D0) POSRAY=.FALSE.
              CALL HITSUR
              IF(STOPP.EQ.1) THEN
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  RETURN
              END IF
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(RV) RAYRAY(23,I)=-1.0D0
              IF(.NOT.RV) RAYRAY(23,I)=1.0D0
C       LOAD REF RAY REGISTERS
C       FOR SURFACE NEWOBJ
C       RAYRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(4,SURF)=L(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(5,SURF)=M(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(6,SURF)=N(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(8,SURF)=LEN - PHYSICAL LENGTH ALONG RAY FROM SURF I-1 TO I
C       RAYRAY(7,SURF)=OPL - OPL FROM SURFACE I-1 TO I
C     ZERO IF I-1 IS PERFECT
C       RAYRAY(9,SURF)=COSINE(I)
C       RAYRAY(10,SURF)=COSINE(IP)
C       RAYRAY(11,SURF)=UX - (RADIAN) MARGINAL XZ SLOPE ANGLE
C       RAYRAY(12,SURF)=UY - (RADIAN) MARGINAL YZ SLOPE ANGLE
C       RAYRAY(13,SURF)=L DIRECTION COSINE OF SURFACE NORMAL
C       RAYRAY(14,SURF)=M DIRECTION COSINE OF SURFACE NORMAL
C       RAYRAY(15,SURF)=N DIRECTION COSINE OF SURFACE NORMAL
C       RAYRAY(16,SURF)=XOLD
C       RAYRAY(17,SURF)=YOLD
C       RAYRAY(18,SURF)=ZOLD
C       RAYRAY(19,SURF)=LOLD
C       RAYRAY(20,SURF)=MOLD
C       RAYRAY(21,SURF)=NOLD
C       RAYRAY(22,SURF)=OPL - OPL FROM SURFACE NEWOBJ TO I
C     OR TO I-1 IF I-1 IS "PERFECT'
C       RAYRAY(23,SURF)=1 FOR NOT RV, -1 FOR RV
C       RAYRAY(24,SURF)=1.0D0 FOR POS RAY, -1 FOR NEG RAY
C       RAYRAY(25,SURF)=RAY ENERGY TERM
C       RAYRAY(26,SURF)=RAY XL DIR COS
C       RAYRAY(27,SURF)=RAY XM DIR COS
C       RAYRAY(28,SURF)=RAY XN DIR COS
C       RAYRAY(29,SURF)=RAY YL DIR COS
C       RAYRAY(30,SURF)=RAY YM DIR COS
C       RAYRAY(31,SURF)=RAY YN DIR COS

C       RAYRAY(41,SURF) TO RAYRAY(50,SURF) RESERVED
              RAYRAY(32,I)=WW3
              RAYRAY(1,I)=X
              RAYRAY(2,I)=Y
              RAYRAY(3,I)=Z
              RAYRAY(4,I)=L
              RAYRAY(5,I)=M
              RAYRAY(6,I)=N
              RAYRAY(26,I)=(M*YN)-(N*YM)
              RAYRAY(27,I)=-((L*YN)-(N*YL))
              RAYRAY(28,I)=(L*YM)-(M*YL)
              RAYRAY(9,I)=COSI
              RAYRAY(10,I)=COSIP
C
C     WHAT IS THE SIGN OF THE INDEX IN THE I-1 SPACE
              IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) THEN
                  SNINDX=DABS(ALENS(45+INT(WW3),I-1))/ALENS(45+INT(WW3),I-1)
                  SNIND2=DABS(ALENS(45+INT(WW3),I))/ALENS(45+INT(WW3),I)
                  RN1=(ALENS(45+INT(WW3),I-1))
                  RN2=(ALENS(45+INT(WW3),I))
              END IF
              IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) THEN
                  SNINDX=DABS(ALENS(65+INT(WW3),I-1))/ALENS(65+INT(WW3),I-1)
                  SNIND2=DABS(ALENS(65+INT(WW3),I))/ALENS(65+INT(WW3),I)
                  RN1=(ALENS(65+INT(WW3),I-1))
                  RN2=(ALENS(65+INT(WW3),I))
              END IF
              RAYRAY(29,I)=YL
              RAYRAY(30,I)=YM
              RAYRAY(31,I)=YN
              RAYRAY(13,I)=LN
              RAYRAY(14,I)=MN
              RAYRAY(15,I)=NN
              RAYRAY(16,I)=XOLD
              RAYRAY(17,I)=YOLD
              RAYRAY(18,I)=ZOLD
              RAYRAY(19,I)=LOLD
              RAYRAY(20,I)=MOLD
              RAYRAY(21,I)=NOLD
C
C     PHYSICAL LENGTH OF THE RAY FROM I-1 TO I IS THE MAG OF THE
C     DIST TRAVELED TIME THE SNINDX TIMES -1 IF RV IN THE SPACE
C     FROM I-1 TO I
C
C     THE NEXT LINES FIX THE RAY DIRECTION WHEN THERE IS
C     NO Z COMPONENT OF MOTION.
C     THE MAGNITUDE OF THE DISTANCE THE RAY TRAVELED FROM I-1 TO I
C     IS ALWAYS JUST:
C
C

              IF(NUMHITS(I-1).EQ.1.AND.I.GE.1) THEN
                  RAYRAY(8,I)=DSQRT(
     1            ((RAYRAY(3,I)-ZOLD)**2)+((RAYRAY(2,I)-YOLD)**2)
     2            +((RAYRAY(1,I)-XOLD)**2))
              ELSE
C       NUMHITS STORED ON LAST SURFACE WAS GREATER THAN 1
                  RAYRAY(8,I)=0.0D0
C       I-1 WAS AN NSS
                  XOLD=0.0D0
                  YOLD=0.0D0
                  ZOLD=0.0D0
                  DO N_HITS=1,NUMHITS(I-1)
                      STEPL=DSQRT(
     2                (((MULTIRAY_DATA(3,I-1,N_HITS))-ZOLD)**2)+
     2                (((MULTIRAY_DATA(2,I-1,N_HITS))-YOLD)**2)+
     2                (((MULTIRAY_DATA(1,I-1,N_HITS))-XOLD)**2)
     3                )
                      IF(N_HITS.EQ.1) STEPL1=STEPL
                      RAYRAY(8,I)=RAYRAY(8,I)+STEPL
C       CREATE NEW XOLD,YOLD,ZOLD
                      XOLD=MULTIRAY_DATA(1,I-1,N_HITS)
                      YOLD=MULTIRAY_DATA(2,I-1,N_HITS)
                      ZOLD=MULTIRAY_DATA(3,I-1,N_HITS)
                  END DO
                  RAYRAY(8,I)=RAYRAY(8,I)-STEPL1
              END IF
C
              IF(RV) RAYRAY(8,I)=-RAYRAY(8,I)
              IF(.NOT.RV) RAYRAY(8,I)=RAYRAY(8,I)
              IF(DABS(RAYRAY(8,I)).GE.1.0D10) RAYRAY(8,I)=0.0D0
C
              IF(SNIND2.GT.0.0D0) RAYRAY(24,I)=1.0D0
              IF(SNIND2.LT.0.0D0) RAYRAY(24,I)=-1.0D0
C
              IF(GLANAM(I-1,2).EQ.'PERFECT      ') THEN
                  RAYRAY(7,I)=0.0D0
                  RAYRAY(8,I)=0.0D0
              END IF
              IF(GLANAM(I-1,2).EQ.'IDEAL        ') THEN
                  RAYRAY(8,I)=-(ALENS(121,I-1)-ALENS(3,I-1))*RAYRAY(6,I-1)
              END IF
              IF(INT(WW3).GE.1.AND.INT(WW3).LE.5)
     1        RAYRAY(7,I)=RAYRAY(8,I)*DABS(ALENS(45+INT(WW3),(I-1)))
              IF(INT(WW3).GE.6.AND.INT(WW3).LE.10)
     1        RAYRAY(7,I)=RAYRAY(8,I)*DABS(ALENS(65+INT(WW3),(I-1)))
              IF(.NOT.RV) RAYRAY(7,I)=RAYRAY(7,I)+PHASE
              IF(RV) RAYRAY(7,I)=RAYRAY(7,I)-PHASE
C
              IF(L.EQ.0.0D0) THEN
                  IF(N.GE.0.0D0) RAYRAY(11,I)=0.0D0
                  IF(N.LT.0.0D0) RAYRAY(11,I)=PII
              ELSE
                  IF(DABS(L).GE.DABS(1.0D35*N)) THEN
                      IF(L.GE.0.0D0) RAYRAY(11,I)=PII/2.0D0
                      IF(L.LT.0.0D0) RAYRAY(11,I)=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(L).EQ.0.0D0.AND.DABS(N).EQ.0.0D0) THEN
                          RAYRAY(11,I)=0.0D0
                      ELSE
                          RAYRAY(11,I)=DATAN2(L,N)
                      END IF
                      IF((RAYRAY(11,I)).LT.0.0D0) RAYRAY(11,I)=
     1                RAYRAY(11,I)+(TWOPII)
                  END IF
              END IF
              IF(M.EQ.0.0D0) THEN
                  IF(N.GE.0.0D0) RAYRAY(12,I)=0.0D0
                  IF(N.LT.0.0D0) RAYRAY(12,I)=PII
              ELSE
                  IF(DABS(M).GE.DABS(1.0D35*N)) THEN
                      IF(M.GE.0.0D0) RAYRAY(12,I)=PII/2.0D0
                      IF(M.LT.0.0D0) RAYRAY(12,I)=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(M).EQ.0.0D0.AND.DABS(N).EQ.0.0D0) THEN
                          RAYRAY(12,I)=0.0D0
                      ELSE
                          RAYRAY(12,I)=DATAN2(M,N)
                      END IF
                      IF((RAYRAY(12,I)).LT.0.0D0) RAYRAY(12,I)=
     1                RAYRAY(12,I)+(TWOPII)
                  END IF
              END IF
              RAYRAY(22,I)=RAYRAY(22,(I-1))+RAYRAY(7,I)
              IF(STOPP.EQ.1) THEN
C NEW STUFF 6/2/94
                  IF(KKK.EQ.1.AND.RAYCOD(1).EQ.1.AND..NOT.ITRACE) THEN
                      JKX=0.0D0
                      JKY=0.0D0
                      STOPP=0
                      KKK=KKK+1
                      GO TO 989
                  END IF
                  FAIL=.TRUE.
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
C       IF GLOBAL IS TRUE, CALCULATE THE CURRENT RAY'S GLOBAL
C       COORDINATES AND LOAD THEM IN ARRAY GLRAY
                  IF(GLOBE) THEN
                      CALL GLBRAY
                  END IF
                  IF(GRASET) THEN
C     WE ARE PLANNING TO PLOT THE RAY
C     SHUT OFF GLOBAL IF IT IS ON AND RE-ASSIGN THE GLSURF
                      IF(GLOBE) THEN
                          OUTLYNE=
     1                    'GLOBAL RAY TRACING HAS BEEN SHUT OFF IN PREPARATION'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'FOR RAY PLOTTING'
                          CALL SHOWIT(1)
                      END IF
                      GLSURF=-99
                      DO IK=0,NEWIMG
                          IF(DABS(ALENS(3,IK)).LE.1.0D10) THEN
                              GLSURF=IK
                              GO TO 8761
                          END IF
                      END DO
 8761                 CONTINUE
                      IF(GLSURF.EQ.-99) THEN
                          GLOBE=.FALSE.
                          OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
                          CALL SHOWIT(1)
                          OUTLYNE='NO OPTICAL SYSTEM PLOT COULD BE MADE'
                          CALL SHOWIT(1)
                          RETURN
                      END IF
                      GLOBE=.TRUE.
                      OFFX=0.0D0
                      OFFY=0.0D0
                      OFFZ=0.0D0
                      OFFA=0.0D0
                      OFFB=0.0D0
                      OFFC=0.0D0
                      CALL GLVERT
                      CALL GLPRY
                      GLOBE=.FALSE.
                  END IF
                  RETURN
              ELSE
                  FAIL=.FALSE.
                  STOPP=0
                  RAYEXT=.TRUE.
              END IF
C
C       CHECK THE RAY HEIGHT AT
C       NEWR AND ADJUST THE Y1AIM AND X1AIM AT SURFACE
C       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWR IS WITHIN
C       AIMTOL . MAXIMUN NUMBER OF ITERRATIONS IS
C       NRAITR. (DEFAULT IS 100).
C
              IF(I.EQ.NEWREF) THEN
C       CALCULATE TARX AND TARY
                  IF(DABS(ALENS(9,I)).GE.1.0D0.AND.
     1            DABS(ALENS(9,I)).LE.6.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
C     CLAPT=TRUE FOR CLAP TILTS AND DECENTED AND FALSE OTHERWISE
                      CLAPT=.FALSE.
                      IF(ALENS(12,I).NE.0.0D0.OR.ALENS(13,I).NE.0.0D0.OR.ALENS(15,I)
     1                .NE.0.0D0) CLAPT=.TRUE.
C       REF SURF HAS CLAP ON IT
C       THE VALUES OF TARY AND TARX ARE COORDINATES IN THE
C       LOCAL COORDINATE SYSTEM OF THE REFERENCE SYSTEM.
C       IF THE CLAP IS DECENTERED, THE TAR() VALUES ARE
C       MODIFIED BY ADDING THE CLAP DECENTRATIONS SINCE THE RAY
C       IS AIMED TO THE RELATIVE REFERENCE SURFACE COORDINATES.
C
C       SET TARGET TO CENTER OF DECENTERED CLAP, ALENS(12,I),
C       AND ALENS(13,I) ARE CLAP DECENTRATIONS
C
                      WWW1=WW1
                      WWW2=WW2
                      IF(SYSTEM1(70).EQ.1.0D0.AND.ALENS(1,I).NE.0.0D0.AND.
     1                ALENS(9,I).EQ.1.0D0.AND.ALENS(12,I).EQ.0.0D0.AND.
     2                ALENS(13,I).EQ.0.0D0.AND.ALENS(15,I).EQ.0.0D0) THEN
                          IF(DABS(1.0D0/ALENS(1,I)).GE.DABS(ALENS(10,I)).AND.
     1                    DABS(1.0D0/ALENS(1,I)).GE.DABS(ALENS(11,I)))
     1                    CALL APLANA(I,WW1,WW2,WWW1,WWW2)
                      END IF
C
C       CIRCULAR CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.1.0D0) THEN
                          IF(CLAPT) THEN
                              IF(ALENS(10,I).LE.ALENS(11,I)) THEN
                                  TARY=(ALENS(10,I)*WWW1)
                                  TARX=(ALENS(10,I)*WWW2)
                              ELSE
                                  TARY=(ALENS(12,I))+(ALENS(11,I)*WWW1)
                                  TARX=(ALENS(13,I))+(ALENS(11,I)*WWW2)
                              END IF
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
C     NO CLAP DEC OR TILTS
                              TARY=(ALENS(10,I)*WWW1)
                              TARX=(ALENS(10,I)*WWW2)
                              IF(ALENS(10,I).LE.ALENS(11,I)) THEN
                                  TARY=(ALENS(10,I)*WWW1)
                                  TARX=(ALENS(10,I)*WWW2)
                              ELSE
                                  TARY=(ALENS(11,I)*WWW1)
                                  TARX=(ALENS(11,I)*WWW2)
                              END IF
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
C       NOW IS THE REF SURF ORIENTATION ANGLE ?
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT CIRCULAR CLAP
                      END IF
C        RECT CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.2.0D0) THEN
                          IF(CLAPT) THEN
                              IF(ANAAIM) THEN
                                  TARY=(ALENS(10,I)*WW1)
                                  TARX=(ALENS(11,I)*WW2)
                              ELSE
                                  IF(DABS(ALENS(10,I)).GT.DABS(ALENS(11,I))) THEN
                                      TARY=(ALENS(10,I)*WW1)
                                      TARX=(ALENS(10,I)*WW2)
                                  ELSE
                                      TARY=(ALENS(12,I))+(ALENS(11,I)*WW1)
                                      TARX=(ALENS(13,I))+(ALENS(11,I)*WW2)
                                  END IF
                              END IF
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              TARY=(ALENS(10,I)*WW1)
                              TARX=(ALENS(11,I)*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT RECT CLAP
                      END IF
C        ELIP CLAP
C
                      YVALUE=ALENS(10,I)
                      XVALUE=ALENS(11,I)
C
                      IF(DABS(ALENS(9,I)).EQ.3.0D0) THEN
                          IF(CLAPT) THEN
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT ELIP CLAP
                      END IF
C        RCTK CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.4.0D0) THEN
                          IF(CLAPT) THEN
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(11,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(11,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT RCTK CLAP
                      END IF
C        POLY CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.5.0D0) THEN
                          IF(CLAPT) THEN
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(10,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(10,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT POLY CLAP
                      END IF
                      IF(DABS(ALENS(9,I)).EQ.6.0D0) THEN
                          IF(CLAPT) THEN
                              YVALUE=ALENS(14,I)
                              XVALUE=ALENS(14,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              YVALUE=ALENS(11,I)
                              XVALUE=ALENS(11,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT IPOLY CLAP
                      END IF
C
                  ELSE
C       NO CLAP ON REF SURF OR MULTI-CLAP
                      TARY=(PXTRAY(1,I)*WW1)
                      TARX=(PXTRAX(1,I)*WW2)
                      IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                      IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                      GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                      TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                      TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                      TARY=TARRY
                      TARX=TARRX
                  END IF
C
                  TEST=DSQRT(((TARX-X)**2)+((TARY-Y)**2))
                  IF(TEST.LE.AIMTOL
     1            .OR.SYSTEM1(62).EQ.0.0D0.OR.ITRACE) THEN
C       AIM IS GOOD ENOUGH, PROCEED
                      AIMOK=.TRUE.
                      GO TO 100
                  ELSE
                      AIMOK=.FALSE.
C       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                  END IF
C
C       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
C       X1ONE AND Y1ONE ARE THE FIRST SET OF RAY
C       COORDINATES AT SURFACE 1
                  X1ONE=X1LAST
                  Y1ONE=Y1LAST
C       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
C       X1LAST AND Y1LAST ARE THE LAST RAY COORDINATES
C       AT SURFACE 1
                  X1LAST=XAIMOL
                  Y1LAST=YAIMOL
C       SET RXONE AND RYONE TO RXLAST AND RYLAST
C       RYONE ANE RXONE ARE THE FIRST SET OF RAY
C       COORDINATES AT THE REFERENCE SURFACE
                  RXONE=RXLAST
                  RYONE=RYLAST
C       SET RXLAST AND RYLAST TO X AND Y
C       RXLAST AND RYLAST ARE THE LAST X AND Y RAY COORDINATES
C       ON THE REFERENCE SURFACE.
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
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                      R_TX=X1AIM
                      R_TY=Y1AIM
                      R_TZ=Z1AIM
                      CALL BAKONE
                      X1AIM=R_TX
                      Y1AIM=R_TY
                      Z1AIM=R_TZ
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
 10       CONTINUE
C       CACOCH IS THE FLAG WHICH TELLS WHETHER OR NOT
C       TO CHECK FOR CLAP/COBS INTERFERENCE.
C       IF CACOCH=0 DO NOT CHECK
C       IF CACOCH=1 DO THE CHECK
C       HERE WE CHECK FOR BLOCKAGE BY CLAP OR COBS.
C       AND DO THE APPROPRIATE THINGS.
          IF(CACOCH.EQ.1) THEN
              DO R_I=NEWOBJ+1,NEWIMG-1
C       CALL CLAP CHECKING ROUTINE
                  R_X=RAYRAY(1,R_I)
                  R_Y=RAYRAY(2,R_I)
                  R_Z=RAYRAY(3,R_I)
C     DON'T CHECK ON OBJECT OR IMAGE SURFACES.
C
                  MMSG=MSG
                  IF(DABS(ALENS(34,R_I)).NE.24.0D0) THEN
C     NO CLAP/COBS CHECK FOR TYPE 24 SPECIAL SURFACE
                      IF(INT(ALENS(127,R_I)).EQ.0.AND.INT(ALENS(128,R_I)).EQ.0) THEN
                          CALL CACHEK(0.0D0,0.0D0,0.0D0,0)
                      ELSE
                          IF(INT(ALENS(127,R_I)).NE.0) THEN
                              DO JK=1,INT(ALENS(127,R_I))
                                  IF(MMSG) THEN
                                      MSG=.TRUE.
                                      IF(JK.LT.INT(ALENS(127,R_I))) MSG=.FALSE.
                                  END IF
                                  JK1=MULTCLAP(JK,1,R_I)
                                  JK2=MULTCLAP(JK,2,R_I)
                                  JK3=MULTCLAP(JK,3,R_I)
                                  CALL CACHEK(JK1,JK2,JK3,1)
                                  IF(RAYCOD(1).EQ.0) THEN
                                      SPDCD1=RAYCOD(1)
                                      SPDCD2=RAYCOD(2)
                                      STOPP=0
                                      RAYEXT=.TRUE.
                                      GO TO 25
                                  END IF
                              END DO
 25                           CONTINUE
                          END IF
                          IF(INT(ALENS(128,R_I)).NE.0) THEN
                              DO JK=1,INT(ALENS(128,R_I))
                                  IF(MMSG) THEN
                                      MSG=.TRUE.
                                  END IF
                                  JK1=MULTCOBS(JK,1,R_I)
                                  JK2=MULTCOBS(JK,2,R_I)
                                  JK3=MULTCOBS(JK,3,R_I)
                                  CALL CACHEK(JK1,JK2,JK3,2)
                                  IF(RAYCOD(1).NE.0) THEN
                                      SPDCD1=RAYCOD(1)
                                      SPDCD2=RAYCOD(2)
                                      STOPP=1
                                      RAYEXT=.TRUE.
                                      GO TO 26
                                  END IF
                              END DO
 26                           CONTINUE
                          END IF
                      END IF
                  END IF
C
C       THIS ROUTINE SETS THE FLAGS
C       CAERAS AND COERAS
C       SET IF THE CURRENT SURFACE HAD A COBS OR
C       CLAP ERASE.
                  IF(STOPP.EQ.1) GO TO 90
                  STOPP=0
                  FAIL=.FALSE.
                  RAYEXT=.TRUE.
C       CONTINUE THE RAYTRACE
              END DO
              GO TO 91
          ELSE
C       NO CHECK TO BE MADE FOR CLAPS/COBS BLOCKAGE
          END IF
          GO TO 91
 90       CONTINUE
          FAIL=.TRUE.
          RAYEXT=.FALSE.
          POLEXT=.FALSE.
 91       CONTINUE
C
C     FINISHED CLAP/COBS CHEKING
C       IF GLOBAL IS TRUE, CALCULATE THE CURRENT RAY'S GLOBAL
C       COORDINATES AND LOAD THEM IN ARRAY GLRAY
          IF(GLOBE) THEN
              CALL GLBRAY
          END IF
          IF(GRASET) THEN
C     WE ARE PLANNING TO PLOT THE RAY
C     SHUT OFF GLOBAL IF IT IS ON AND RE-ASSIGN THE GLSURF
              IF(GLOBE) THEN
                  OUTLYNE=
     1            'GLOBAL RAY TRACING HAS BEEN SHUT OFF IN PREPARATION'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'FOR RAY PLOTTING'
                  CALL SHOWIT(1)
              END IF
              GLSURF=-99
              DO I=0,NEWIMG
                  IF(DABS(ALENS(3,I)).LE.1.0D10) THEN
                      GLSURF=I
                      GO TO 876
                  END IF
              END DO
 876          CONTINUE
              IF(GLSURF.EQ.-99) THEN
                  GLOBE=.FALSE.
                  OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
                  CALL SHOWIT(1)
                  OUTLYNE='NO OPTICAL SYSTEM PLOT COULD BE MADE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              GLOBE=.TRUE.
              OFFX=0.0D0
              OFFY=0.0D0
              OFFZ=0.0D0
              OFFA=0.0D0
              OFFB=0.0D0
              OFFC=0.0D0
              CALL GLVERT
              CALL GLPRY
              GLOBE=.FALSE.
          END IF
          RAYRAY(25,NEWOBJ:NEWIMG)=0.0D0
          RAYRAY(34:38,NEWOBJ:NEWIMG)=0.0D0
C     COMPUTE RAY ENERGY
          RAYRAY(34:38,I)=0.0D0
          DO I=NEWOBJ,NEWIMG
              IF(I.EQ.NEWOBJ) THEN
                  IF(REFEXT) THEN
                      RAYRAY(25,I)=WW4*REFRY(9,NEWOBJ)
                  ELSE
                      RAYRAY(25,I)=WW4
                  END IF
              ELSE
                  RAYRAY(25,I)=RAYRAY(25,I-1)
              END IF
              IF(I.EQ.NEWOBJ) THEN
                  IF(WA3.GE.1.AND.WA3.LE.5) THEN
                      RN1=(ALENS(45+WA3,I))
                      RN2=(ALENS(45+WA3,I))
                  END IF
                  IF(WA3.GE.6.AND.WA3.LE.10) THEN
                      RN1=(ALENS(65+WA3,I))
                      RN2=(ALENS(65+WA3,I))
                  END IF
              ELSE
                  IF(WA3.GE.1.AND.WA3.LE.5) THEN
                      RN1=(ALENS(45+WA3,I-1))
                      RN2=(ALENS(45+WA3,I))
                  END IF
                  IF(WA3.GE.6.AND.WA3.LE.10) THEN
                      RN1=(ALENS(65+WA3,I-1))
                      RN2=(ALENS(65+WA3,I))
                  END IF
              END IF
              IF(ALENS(34,I).EQ.19.0D0) THEN
C     CALL THE GRIDS ROUTINE WITH ARG = 2
C     THIS CAUSES THE RAY ENERGY TO BE MULTIPLIED BY THE
C     APODIZATION REPRESENTED IN THE APGRI FILE
                  ISURF=I
                  GERROR=.FALSE.
                  XPASS=X
                  YPASS=Y
                  IPASS1=2
                  CALL GRIDS(2,ISURF,GERROR)
                  IF(.NOT.GERROR) GRIDSUNLOADED19(I)=.FALSE.
                  IF(GERROR) THEN
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'GRID FILE DOES NOT EXIST FOR THIS SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
                          CALL SHOWIT(1)
                      END IF
                      STOPP=1
                      RAYCOD(1)=15
                      RAYCOD(2)=I
                      RAYEXT=.FALSE.
                      POLEXT=.FALSE.
                      RETURN
                  END IF
                  FACT_PAR=0.0D0
                  FACT_PER=0.0D0
                  PHASE_PAR=0.0D0
                  PHASE_PER=0.0D0
                  POLANG=0.0D0
              END IF
              IF(SYSTEM1(103).EQ.1.0D0) THEN
C     SCREEN SURFACE
                  IF(I.EQ.INT(SYSTEM1(104))) THEN
C       GOT THE SCREEN SURFACE
                      AOI=DABS(DACOS(RAYRAY(9,I)))
                      D=SYSTEM1(105)
                      H=SYSTEM1(106)
                      S=SYSTEM1(107)
                      IF(DCOS(AOI).EQ.0.0D0.OR.
     1                AOI.GE.DABS(SYSTEM1(108))) THEN
                          FACTOR=0.0D0
                      ELSE
                          FACTOR=PII*(((D)-(H*DSIN(AOI)))
     1                    *(((D)*DCOS(AOI))-(H*DSIN(AOI))))/(4.0D0*S*S*DCOS(AOI))
                      END IF
                      IF(FACTOR.LT.0.0D0) FACTOR=0.0D0
                      RAYRAY(25,I)=RAYRAY(25,I)*FACTOR
                  END IF
              END IF
              IF(ALENS(34,I).NE.19.0D0) THEN
C
C     NOT AN APODIZATION SURFACE
                  IF(DUM(I).AND.I.GT.0) THEN
                      RAYRAY(34:38,I)=0.0D0
                  END IF
C       EVEN THOUGH WE DON'T ALWAYS DO POLARIZATION, WE NEED THE POLANG
C       THE UNIT VECTOR JK_CPL,JK_CPM,JK_CPN IS NORMAL TO
C       THE PLANE OF INCIDENCE AND LIES IN A PLANE
C       NORMAL TO THE SURFACE AT THE POINT OF INTERSECTION
                  JK_L1=RAYRAY(19,I)
                  JK_M1=RAYRAY(20,I)
                  JK_N1=RAYRAY(21,I)
                  JK_L2=RAYRAY(4,I)
                  JK_M2=RAYRAY(5,I)
                  JK_N2=RAYRAY(6,I)
                  IF(JK_L1.EQ.JK_L2.AND.JK_M1.EQ.JK_M2.AND.JK_N1.EQ.JK_N2) THEN
                      JK_L1=RAYRAY(13,I)
                      JK_M1=RAYRAY(14,I)
                      JK_N1=RAYRAY(15,I)
                      JK_L2=RAYRAY(4,I)
                      JK_M2=RAYRAY(5,I)
                      JK_N2=RAYRAY(6,I)
                  END IF
                  CALL CROSS_PRODUCT(JK_CPL,JK_CPM,JK_CPN
     1            ,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
                  MAG=DSQRT((JK_CPL**2)+(JK_CPM**2)+(JK_CPN**2))
                  JK_CPL=DABS(JK_CPL)
                  JK_CPM=DABS(JK_CPM)
                  JK_CPN=DABS(JK_CPN)
                  IF((MAG).NE.0.0D0) THEN
                      JK_CPL=JK_CPL/MAG
                      JK_CPM=JK_CPM/MAG
                      JK_CPN=JK_CPN/MAG
                  ELSE
                      JK_CPL=1.0D0
                      JK_CPM=0.0D0
                      JK_CPN=0.0D0
                  END IF
C       THE UNIT VECTOR IN THE
C       INCIDENT DIRECTION IS:
                  JK_L1=JK_CPL
                  JK_M1=JK_CPM
                  JK_N1=JK_CPN
                  JK_L2=RAYRAY(13,I)
                  JK_M2=RAYRAY(14,I)
                  JK_N2=RAYRAY(15,I)
                  CALL CROSS_PRODUCT(SA_CPL,SA_CPM,SA_CPN
     1            ,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
                  MAG=DSQRT((SA_CPL**2)+(SA_CPM**2)+(SA_CPN**2))
                  SA_CPL=DABS(SA_CPL)
                  SA_CPM=DABS(SA_CPM)
                  SA_CPN=DABS(SA_CPN)
                  IF((MAG).NE.0.0D0) THEN
                      SA_CPL=SA_CPL/MAG
                      SA_CPM=SA_CPM/MAG
                      SA_CPN=SA_CPN/MAG
                  ELSE
                      SA_CPL=0.0D0
                      SA_CPM=1.0D0
                      SA_CPN=0.0D0
                  END IF
C       WE NEED TO USE THIS INCIDENT DIRECTION VECTOR BUT IT NEEDS TO BE
C       MODIFIED SO THAT ITS Z-ORIENTATION IS PARALLEL TO THE Z-COORDINATE
C       OF THE RYL,RYM,RYN VECTOR
                  SA_CPN = RYN(I)
                  MAG=DSQRT((1.0D0-(SA_CPN**2))/((SA_CPL**2)+(SA_CPM**2)))
                  SA_CPL=MAG*SA_CPL
                  SA_CPM=MAG*SA_CPM
                  MAG=DSQRT((SA_CPL**2)+(SA_CPM**2)+(SA_CPN**2))
C       THE DOT PRODUCT OF THIS UNIT VECTOR WITH RYL,RYM,RYM UNIT VECTOR
C       GIVES THE COSINE OF THE ANGLE BETWEEN THE Y-VECTOR OF THE RAY
C       AND THE DIRECTION OF INCIDENCE
                  JK_L1=RYL(I)
                  JK_M1=RYM(I)
                  JK_N1=RYN(I)
                  JK_L2=SA_CPL
                  JK_M2=SA_CPM
                  JK_N2=SA_CPN
                  CALL DOT_PRODUCT(DP,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
C       POLARIZATION COSINE IS THEN
                  IF(DP.GT.1.0D0) DP=1.0D0
                  IF(DP.LT.-1.0D0) DP=-1.0D0
                  POLANG=DACOS(DP)
                  IF(POLANG.GT.PII/2.0D0) POLANG=PII-POLANG
                  IF(POLANG.LT.-PII/2.0D0) POLANG=PII+POLANG
                  RAYRAY(38,I)=(POLANG*180.0D0)/PII
                  RAYRAY(39,I)=DCOS(POLANG)*DACOS(RAYRAY(9,I))
                  RAYRAY(40,I)=DSIN(POLANG)*DACOS(RAYRAY(9,I))
C       POL ANG DONE
                  IF(COATSET) THEN
                      J=INT(ALENS(112,I))
                      IF(RAYRAY(9,I).GT.1.0D0) RAYRAY(9,I)=1.0D0
                      IF(RAYRAY(9,I).LT.-1.0D0) RAYRAY(9,I)=-1.0D0
                      IF(RAYRAY(10,I).GT.1.0D0) RAYRAY(10,I)=1.0D0
                      IF(RAYRAY(10,I).LT.-1.0D0) RAYRAY(10,I)=-1.0D0
                      IA=DACOS(RAYRAY(9,I))
                      IAP=DACOS(RAYRAY(10,I))
                      WA3=INT(WW3)
                      PATHL=RAYRAY(8,I)
                      IF(I.EQ.NEWOBJ) OLDABSCOEF(1:10)=0.0D0
                      IF(I.EQ.NEWOBJ) ABSCOEF(1:10)=0.0D0
                      CALL ENERGY_ADJUST(ENERGY_FACTOR,I,J,IA,IAP,RN1,RN2,WA3,POLANG
     1                ,FACT_PAR,FACT_PER,PHASE_PAR,PHASE_PER,PATHL)
                      IF(I.GT.NEWOBJ) RAYRAY(25,I)=RAYRAY(25,I)*ENERGY_FACTOR
                  END IF
                  IF(ALENS(96,I).EQ.1.0D0.AND.ALENS(98,I).NE.0.0D0) THEN
                      IA=DACOS(RAYRAY(9,I))
                      WA3=INT(WW3)
                      ENERGY_FACTOR=1.0D0
                      CALL DIFFRACTION_EFFICIENCY(ENERGY_FACTOR,I,IA,WA3)
                      IF(I.GT.NEWOBJ)RAYRAY(25,I)=RAYRAY(25,I)*ENERGY_FACTOR
                  END IF
                  RAYRAY(34,I)=FACT_PAR
                  RAYRAY(35,I)=FACT_PER
                  RAYRAY(36,I)=PHASE_PAR
                  RAYRAY(37,I)=PHASE_PER
              END IF
          END DO
          RETURN
      END


C SUB RAYTRA2.FOR

      SUBROUTINE RAYTRA2
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE RAYTRA2.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE TRACING OF A RAY IN OPTIMIZATION AND TOLERANCING
C
          INTEGER IPASS1,JK,I,ISYS20,KKK,J,ISURF,WA3

          INTEGER CAERAS,COERAS,N_HITS
C
          REAL*8 X,Y,Z,L,M,N,TANN1,TANN2,IA,IAP,GAMMA
     1    ,D21,D22,XL,XM,XN,YL,YM,YN,RN1,RN2,
     2    WWW1,WWW2,D11,D12,LS,SNINDX,SNIND2,JK1,JK2,JK3,
     3    LSTART,MSTART,NSTART,YANG,XANG,X1LAST,LARGE,ENERGY_FACTOR,
     4    Y1LAST,X1ONE,Y1ONE,RXLAST,RYLAST,RXONE,RYONE,DDELX,DDELY,
     5    XC1,YC1,ZC1,MF1,MF2,TARX,TARY,XVALUE,YVALUE,
     6    TEST,MAG,LOLD,MOLD,NOLD,TARRY,TARRX,JKX,JKY,STEPL,STEPL1,
     7    JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2,JK_CPL,JK_CPM,JK_CPN
     7    ,SA_CPL,SA_CPM,SA_CPN,DP,POLANG,FACT_PAR,FACT_PER,PHASE_PAR
     8    ,PHASE_PER,PATHL
C
          COMMON/CACO/CAERAS,COERAS,LS
C
C     VARIABLES FOR SPOT TRACING
          LOGICAL SPDTRA,MMSG
          LOGICAL AIMOK,CLAPT,OLDPASS,GERROR,DELFAIL
          COMMON/PASSOLD/OLDPASS
C
          INTEGER SPDCD1,SPDCD2
C
          COMMON/SPRA1/SPDTRA
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          REAL*8 XPASS,YPASS,ZPASS
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          REAL*8 AOI,D,H,S,FACTOR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'

C
          KKK=0
C                       DDELX=0.001D0*SYSTEM1(12)
C                       DDELY=0.001D0*SYSTEM1(13)
          DDELX=0.001D0
          DDELY=0.001D0
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
C       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
C       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
C       RAY AIMING.
C
C       SET RAYCOD DEFAULTS
          RAYCOD(1)=-1
          RAYCOD(2)=-1
C
          AIMOK=.FALSE.
C
          RELY=WW1
          RELX=WW2
C
C       SET INITIAL VALUES FOR X1ONE, Y1ONE, X1LAST AND Y1LAST
C
          LARGE=-99999.9D0
          X1ONE=LARGE
          Y1ONE=LARGE
          X1LAST=LARGE
          Y1LAST=LARGE
C
C       SET INITIAL VALUES FOR RXONE, RYONE, RXLAST AND RYLAST
          RXONE=LARGE
          RYONE=LARGE
          RXLAST=LARGE
          RYLAST=LARGE
C
C       THE FIRST THING TO DO IS TO DETERMINE THE STARTING
C       COORDINATES OF THE RAY AT THE OBJECT SURFACE.
C       THE STARTING RAY COORDINATES AT SURFACE NEWOBJ
C       ARE JUST THE RAY COORDINATES AT THIS SURFACE
C
C
          XSTRT=REFRY(1,NEWOBJ)
          YSTRT=REFRY(2,NEWOBJ)
          ZSTRT=REFRY(3,NEWOBJ)
C
C       THE WAVELENGTH NUMBER FOR THE RAY TRACE IS:
C       INT(WW3) OR INT(SYSTEM1(11))
C
C       NOW WHAT ARE THE STARTING DIRECTION COSINES AND HOW ARE
C       THEY DETERMINED?
C
C       START BY LOOKING AT THE PY+ PCY AND PX+PCX
C       VALUES AT SURFACE NEWOBJ+1.
C       THESE VALUES CORRESPOND TO THE INTERSECTIONS OF CHIEF
C       PLUS MARGINAL RAYS AT FULL 1.0 FRACTIONAL OBJECT HEIGHTS
C       AND FULL REFERENCE APERTURE HEIGHTS.
C       FOR FOB 1.0 1.0 THE TARGET RAY HEIGHTS AT SURFACE NEWOBJ+1
C       FOR THE FULL APERTURE RAY ARE
C       ARE X=PX(NEWOBJ+1)+PCX(NEWOBJ+1)
C       AND Y=PY(NEWOBJ+1)+PCY(NEWOBJ+1).
C
C       IN GENERAL THE RAY INTERESECTION
C       POINTS FOR FIRST GUESS AIMING WILL BE:
C       THE POINT AT SURFACE NEWOBJ+1 AT WHICH THE
C       PARAXIAL CHIEF RAY PLUS THE PARAXIAL MARGINAL RAY
          IF(SYSTEM1(63).EQ.0.0D0) THEN
C       TELECENTRIC AIMING IS OFF
              JKX=(PXTRAX(1,NEWOBJ+1))
              JKY=(PXTRAY(1,NEWOBJ+1))
              IF(ALENS(9,NEWOBJ+1).EQ.1.0D0) THEN
C     CIRCULAR AP
                  IF(ALENS(10,NEWOBJ+1).LE.ALENS(11,NEWOBJ+1)) THEN
                      IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1                JKX=DABS(ALENS(10,NEWOBJ+1))
                      IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1                JKY=DABS(ALENS(10,NEWOBJ+1))
                  ELSE
                      IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1                JKX=DABS(ALENS(11,NEWOBJ+1))
                      IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1                JKY=DABS(ALENS(11,NEWOBJ+1))
                  END IF
              END IF
              IF(ALENS(9,NEWOBJ+1).EQ.5.0D0) THEN
C     CIRCULAR AP
                  IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1            JKX=DABS(ALENS(10,NEWOBJ+1))
                  IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1            JKY=DABS(ALENS(10,NEWOBJ+1))
              END IF
              IF(ALENS(9,NEWOBJ+1).EQ.6.0D0) THEN
C     CIRCULAR AP
                  IF(DABS(ALENS(14,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1            JKX=DABS(ALENS(14,NEWOBJ+1))
                  IF(DABS(ALENS(14,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1            JKY=DABS(ALENS(14,NEWOBJ+1))
              END IF
              IF(ALENS(9,NEWOBJ+1).GT.1.0D0.AND.ALENS(9,NEWOBJ+1)
     1        .LE.4.0D0) THEN
C     OTHER AP
                  IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))
     1            JKX=DABS(ALENS(11,NEWOBJ+1))
                  IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))
     1            JKY=DABS(ALENS(10,NEWOBJ+1))
              END IF
          ELSE
C       TELECENTRIC AIMING IS ON
              JKX=(PXTRAX(1,NEWOBJ+1))
              JKY=(PXTRAY(1,NEWOBJ+1))
          END IF
C
 989      CONTINUE
          IF(RAYCOD(1).EQ.1.AND.KKK.GT.1) THEN
              STOPP=1
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              FAIL=.TRUE.
              IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
                  CALL MACFAL
              END IF
              RETURN
          END IF
          IF(.NOT.ITRACE) THEN
              IF(NULL.AND..NOT.REFEXT) THEN
C     NULL WITH FAILED CHIEF RAY
                  IF(SYSTEM1(62).EQ.0.0D0) THEN
C     NO RAY AIMING
                      IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
C     RAY AIMING IS OFF, TELECENTRIC RAY AIMING IS OFF
                          X1AIM=WW2*JKX
                          X1AIM=(X1AIM)-ALENS(31,NEWOBJ+1)
                          Y1AIM=WW1*JKY
                          Y1AIM=(Y1AIM)-ALENS(30,NEWOBJ+1)
                          Z1AIM=0.0D0
                          XC=X1AIM
                          YC=Y1AIM
                          ZC=Z1AIM
                          XC1=XC
                          YC1=YC
                          ZC1=ZC
C
                      ELSE
C     TEL ON
                          IF(SYSTEM1(16).NE.0.0D0)
     1                    X1AIM=(LFOB(2)*DABS(PXTRAX(5,NEWOBJ)))+(WW2*JKX)
                          IF(SYSTEM1(16).EQ.0.0D0)
     1                    X1AIM=(WW2*JKX)
                          IF(SYSTEM1(14).NE.0.0D0)
     1                    Y1AIM=(LFOB(1)*(PXTRAY(5,NEWOBJ)))+(WW1*JKY)
                          IF(SYSTEM1(14).EQ.0.0D0)
     1                    Y1AIM=(WW1*JKY)
                          Z1AIM=0.0D0
                          XC=X1AIM
                          YC=Y1AIM
                          ZC=Z1AIM
                          XC1=XC
                          YC1=YC
                          ZC1=ZC
                      END IF
                  ELSE
C     RAY AIMING
                      X1AIM=(LFOB(2)*DABS(PXTRAX(5,NEWOBJ+1)))+(WW2*JKX)
                      X1AIM=(X1AIM)-ALENS(31,NEWOBJ+1)
                      Y1AIM=(LFOB(1)*DABS(PXTRAY(5,NEWOBJ+1)))+(WW1*JKY)
                      Y1AIM=(Y1AIM)-ALENS(30,NEWOBJ+1)
                      Z1AIM=0.0D0
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                      IF(XC.LT.0.0D0) DDELX=-DDELX
                      IF(YC.LT.0.0D0) DDELY=-DDELY
                  END IF
              END IF
C     CHIEF RAY EXISTS
              IF(SYSTEM1(62).EQ.0.0D0) THEN
C     NO RAY AIMING
                  IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                      X1AIM=((REFRY(1,(NEWOBJ+1)))+(WW2*JKX))
                      Y1AIM=((REFRY(2,(NEWOBJ+1)))+(WW1*JKY))
                      Z1AIM=REFRY(3,(NEWOBJ+1))
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                  ELSE
C     TEL ON
                      X1AIM=((REFRY(1,NEWOBJ))+(WW2*JKX))
                      Y1AIM=((REFRY(2,NEWOBJ))+(WW1*JKY))
                      Z1AIM=0.0D0
                      XC=X1AIM
                      YC=Y1AIM
                      ZC=Z1AIM
                      XC1=XC
                      YC1=YC
                      ZC1=ZC
                  END IF
              ELSE
C     RAY AIMING
                  X1AIM=REFRY(1,NEWOBJ+1)
                  Y1AIM=REFRY(2,NEWOBJ+1)
                  Z1AIM=REFRY(3,NEWOBJ+1)
                  XC=X1AIM
                  YC=Y1AIM
                  ZC=Z1AIM
                  XC1=XC
                  YC1=YC
                  ZC1=ZC
                  IF(XC.LT.0.0D0) DDELX=-DDELX
                  IF(YC.LT.0.0D0) DDELY=-DDELY
              END IF
              XAIMOL=XC1
              YAIMOL=YC1
              ZAIMOL=ZC1
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
              R_TX=X1AIM
              R_TY=Y1AIM
              R_TZ=Z1AIM
              CALL BAKONE
              X1AIM=R_TX
              Y1AIM=R_TY
              Z1AIM=R_TZ
          END IF
          IF(ITRACE) THEN
C     ILLUMINATION TRACE
C     TANGENTS OF CHIEF RAY SLOPE
              TANN1=REFRY(4,0)/REFRY(6,0)
              TANN2=REFRY(5,0)/REFRY(6,0)
          END IF
C
C       NOW THE STARTING DIRECTION COSINES ARE RELATIVELY SIMPLE.
C       THEY ARE CALLED LSTART,MSTART AND NSTART
C
C       FIRST CONVERT THE SURFACE 0 AND 1 COORDINATES INTO YANG AND XANG
C       ANGULAR VALUES AS IN HEXAGON AND CODE V.
C       YANG AND XANG ANGLES IN RADIANS.
          STOPP=0
          RAYEXT=.TRUE.
          FAIL=.FALSE.
C
C       THE STARTING COORDINATES AND DIRECTION COSINES FOR THE VERY
C       FIRST PART OF THE RAY TRACE HAVE BEEN CALCULATED
C       BASED UPON THE RESULTS OF THE CURRENT PARAXIAL RAY TRACE AND
C       ASSUMING THAT SURFACE NEWOBJ+1 IS PLANO.
C
C       NEXT STEPS
C
C       1. TRANSLATE TO THE NEXT SURFACE
C       2. IF THAT SURFACE IS TILTED AND/OR DECENTERED,
C          TRANSFORM TO THAT SURFACES COORDINATE SYSTEM.
C       3. INTERSECT THE SURFACE
C       4. INTERACT WITH THAT SURFACE
C       5. REPEAT STEP 1.
C
C       1. TRANSFER TO NEXT SURFACES COORDINATE SYSTEM LOCATED
C       AT THE NEXT SURFACES VERTEX (THIS INCLUDES THE AXIAL THICKNESS).
C       AND TILTS AND DECENTERS.
C
C       CALL TRNSF2
C
C       2. INTERSECT AND INTERACT
C
C       CALL HITSUR
C
C       3. REPEAT WITH A DO LOOP
C
C       STORE THE RAY DATA FOR SURFACE 0 IN THE ARRAY
C       RAYRAY(1:50,0:MAXSUR)
C
C       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
C       SURFACE POINT INTERSECTION
 9        CONTINUE
          IF(ALENS(3,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
          IF(ALENS(3,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
          RV=.FALSE.
          KKK=KKK+1
C       IF KKK EXCEEDS 100 TRIES, PRINT RAY ITERRATION ERROR
C       AND STOP SEARCHING.
          IF(KKK.GT.NRAITR) THEN
              RAYCOD(1)=3
              RAYCOD(2)=NEWREF
              SPDCD1=RAYCOD(1)
              SPDCD2=NEWREF
              STOPP=1
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              FAIL=.TRUE.
              IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
                  CALL MACFAL
              END IF
              RETURN
          ELSE
              STOPP=0
              RAYEXT=.TRUE.
              FAIL=.FALSE.
C        PROCEED
          END IF
C
C       THIS IS THE RE-ENTRY POINT FOR RAY AIMING. RE-ENTRY
C       IS PERFORMED AFTER NEW VALUES OF X1AIM AND Y1AIM WERE
C       CALCULATED JUST BEFORE ENCOUNTERING THE (GO TO 9)
C       STATEMENT NEAR THE END OF THIS ROUTINE.
C     COMPUTE DIR COS DIRECTLY FROM POSITIONS
C
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
C
C       FOR SURFACE NEWOBJ
C       RAYRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(4,SURF)=L(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(5,SURF)=M(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(6,SURF)=N(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(8,SURF)=LEN - PHYSICAL LENGTH ALONG RAY FROM SURF I-1 TO I
C       RAYRAY(7,SURF)=OPL - OPL FROM SURFACE I-1 TO I
C     ZERO IF I-1 IS PERFECT
C       RAYRAY(9,SURF)=COSINE(I)
C       RAYRAY(10,SURF)=COSINE(IP)
C       RAYRAY(11,SURF)=UX - (RADIAN) MARGINAL XZ SLOPE ANGLE
C       RAYRAY(12,SURF)=UY - (RADIAN) MARGINAL YZ SLOPE ANGLE
C       RAYRAY(13,SURF)=LN - SURFACE NORMAL DIRECTION COSINE
C       RAYRAY(14,SURF)=MN - SURFACE NORMAL DIRECTION COSINE
C       RAYRAY(15,SURF)=NN - SURFACE NORMAL DIRECTION COSINE
C       RAYRAY(16,SURF)=XOLD  The next 6 items are the ray values
C       RAYRAY(17,SURF)=YOLD  just before interaction with the surface
C       RAYRAY(18,SURF)=ZOLD  in the surface local coordinate system.
C       RAYRAY(19,SURF)=OLDL
C       RAYRAY(20,SURF)=OLDM
C       RAYRAY(21,SURF)=OLDN
C     THESE LAST 6 ITEMS ARE THE COORDINATES OF THE RAY AT
C     SURFACE I-1 IN THE COORDINATE SYSTEM OF SURFACE I
C       RAYRAY(22,SURF)=OPL - OPL FROM SURFACE NEWOBJ TO I
C     OR TO I-1 IF I-1 IS "PERFECT'
C       RAYRAY(23,SURF)=1 FOR NOT RV, -1 FOR RV
C       RAYRAY(24,SURF)=1.0D0 FOR POSRAY,-1 FOR NEG RAY
C       RAYRAY(25,SURF)=RAY ENERGY TERM
C       RAYRAY(26,SURF)=RAY XL DIR COS
C       RAYRAY(27,SURF)=RAY XM DIR COS
C       RAYRAY(28,SURF)=RAY XN DIR COS
C       RAYRAY(29,SURF)=RAY YL DIR COS
C       RAYRAY(30,SURF)=RAY YM DIR COS
C       RAYRAY(31,SURF)=RAY YN DIR COS
C       RAYRAY(34,SURF)=FACT_PAR
C       RAYRAY(35,SURF)=FACT_PER
C       RAYRAY(36,SURF)=PHASE_PAR
C       RAYRAY(37,SURF)=PHASE_PER
C       RAYRAY(38,SURF)=POLARIZATION ANGLE IN DEGREES BETWEEN Y-RAY VECTOR AND PARALLEL PLANE
C       RAYRAY(39,SURF)=Y-COMPONENT OF THE ANGLE OF INCIDENCE
C       RAYRAY(40,SURF)=X-COMPONENT OF THE ANGLE OF INCIDENCE
C       RAYRAY(41,SURF) TO RAYRAY(50,SURF) RESERVED
          RAYRAY(34:35,NEWOBJ)=1.0D0
          RAYRAY(36:38,NEWOBJ)=0.0D0
          RAYRAY(32,NEWOBJ)=WW3
          RAYRAY(1,NEWOBJ)=XSTRT
          RAYRAY(2,NEWOBJ)=YSTRT
          RAYRAY(3,NEWOBJ)=ZSTRT
          RAYRAY(4,NEWOBJ)=LSTART
          RAYRAY(5,NEWOBJ)=MSTART
          RAYRAY(6,NEWOBJ)=NSTART
          RAYRAY(7,NEWOBJ)=0.0D0
          RAYRAY(8,NEWOBJ)=0.0D0
          IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) THEN
              SNIND2=DABS(ALENS(45+INT(WW3),NEWOBJ))/ALENS(45+INT(WW3),NEWOBJ)
              RN1=(ALENS(45+INT(WW3),NEWOBJ))
              RN2=(ALENS(45+INT(WW3),NEWOBJ))
          END IF
          IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) THEN
              SNIND2=DABS(ALENS(65+INT(WW3),NEWOBJ))/ALENS(65+INT(WW3),NEWOBJ)
              RN1=(ALENS(65+INT(WW3),NEWOBJ))
              RN2=(ALENS(65+INT(WW3),NEWOBJ))
          END IF
          IF(SNIND2.GT.0.0D0) RAYRAY(24,NEWOBJ)=1.0D0
          IF(SNIND2.LT.0.0D0) RAYRAY(24,NEWOBJ)=-1.0D0
          IF(SNIND2.GT.0.0D0) POSRAY=.TRUE.
          IF(SNIND2.LT.0.0D0) POSRAY=.FALSE.
          RAYRAY(9,NEWOBJ)=NSTART
          RAYRAY(10,NEWOBJ)=NSTART
          IA=DACOS(RAYRAY(9,NEWOBJ))
          IAP=DACOS(RAYRAY(10,NEWOBJ))
          RAYRAY(11,NEWOBJ)=XANG
          IF((RAYRAY(11,NEWOBJ)).LT.0.0D0) RAYRAY(11,NEWOBJ)=
     1    RAYRAY(11,NEWOBJ)+(TWOPII)
          RAYRAY(12,NEWOBJ)=YANG
          IF((RAYRAY(12,NEWOBJ)).LT.0.0D0) RAYRAY(12,NEWOBJ)=
     1    RAYRAY(12,NEWOBJ)+(TWOPII)
          RAYRAY(13,NEWOBJ)=0.0D0
          RAYRAY(14,NEWOBJ)=0.0D0
          RAYRAY(15,NEWOBJ)=1.0D0
          RAYRAY(16,NEWOBJ)=XSTRT
          RAYRAY(17,NEWOBJ)=YSTRT
          RAYRAY(18,NEWOBJ)=ZSTRT
          RAYRAY(19,NEWOBJ)=LSTART
          RAYRAY(20,NEWOBJ)=MSTART
          RAYRAY(21,NEWOBJ)=NSTART
          RAYRAY(22,NEWOBJ)=0.0D0
          RAYRAY(26,NEWOBJ)=(1.0D0*DCOS(XANG))
          RAYRAY(27,NEWOBJ)=0.0D0
          RAYRAY(28,NEWOBJ)=-(1.0D0*DSIN(XANG))
          RAYRAY(29,NEWOBJ)=0.0D0
          RAYRAY(30,NEWOBJ)=(1.0D0*DCOS(XANG))
          RAYRAY(31,NEWOBJ)=-(1.0D0*DSIN(YANG))
C
C
          X=XSTRT
          Y=YSTRT
          Z=ZSTRT
          L=LSTART
          M=MSTART
          N=NSTART
          XL=(1.0D0*DCOS(XANG))
          XM=0.0D0
          XN=-(1.0D0*DSIN(XANG))
          YL=0.0D0
          YM=(1.0D0*DCOS(XANG))
          YN=-(1.0D0*DSIN(YANG))
C
          ISYS20=NEWIMG
          I=0
          DO 10 I=(NEWOBJ+1),ISYS20

C
C
C       WE ARE CURRENTLY AT SURFACE I-1 AND KNOW WHERE
C       WE ARE AND WHICH DIRECTION WE WANT TO GO.
C       CALLING TRNSF2.FOR TRANSFORMS THE X,Y,Z,L,M AND N INTO
C       THE COORDINATE SYSTEM OF SURFACE I,FOB0 TRACKS IF FOB 0 IS
C       USED OR FOB (SOMETHING NON-ZERO)
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
              XOLD=X
              YOLD=Y
              ZOLD=Z
              LOLD=L
              MOLD=M
              NOLD=N
C       XOLD,YOLD AND ZOLD ARE THE COORDINATES OF THE CURRENT RAY
C       AT SURFACE I-1 IN THE COORDINATE SYSTEM OF SURFACE I
C       NOW INTERSECT THE SURFACE
C
              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              R_XAIM=XAIMOL
              R_YAIM=YAIMOL
              R_ZAIM=ZAIMOL
              IF(RAYRAY(24,R_I-1).GT.0.0D0) POSRAY=.TRUE.
              IF(RAYRAY(24,R_I-1).LT.0.0D0) POSRAY=.FALSE.
              CALL HITSUR
              IF(STOPP.EQ.1) THEN
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  RETURN
              END IF
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(RV) RAYRAY(23,I)=-1.0D0
              IF(.NOT.RV) RAYRAY(23,I)=1.0D0
C       LOAD REF RAY REGISTERS
C       FOR SURFACE NEWOBJ
C       RAYRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
C       RAYRAY(4,SURF)=L(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(5,SURF)=M(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(6,SURF)=N(LOCAL)- DIRECTION COSINE AT SURFACE I
C       RAYRAY(8,SURF)=LEN - PHYSICAL LENGTH ALONG RAY FROM SURF I-1 TO I
C       RAYRAY(7,SURF)=OPL - OPL FROM SURFACE I-1 TO I
C     ZERO IF I-1 IS PERFECT
C       RAYRAY(9,SURF)=COSINE(I)
C       RAYRAY(10,SURF)=COSINE(IP)
C       RAYRAY(11,SURF)=UX - (RADIAN) MARGINAL XZ SLOPE ANGLE
C       RAYRAY(12,SURF)=UY - (RADIAN) MARGINAL YZ SLOPE ANGLE
C       RAYRAY(13,SURF)=L DIRECTION COSINE OF SURFACE NORMAL
C       RAYRAY(14,SURF)=M DIRECTION COSINE OF SURFACE NORMAL
C       RAYRAY(15,SURF)=N DIRECTION COSINE OF SURFACE NORMAL
C       RAYRAY(16,SURF)=XOLD
C       RAYRAY(17,SURF)=YOLD
C       RAYRAY(18,SURF)=ZOLD
C       RAYRAY(19,SURF)=LOLD
C       RAYRAY(20,SURF)=MOLD
C       RAYRAY(21,SURF)=NOLD
C       RAYRAY(22,SURF)=OPL - OPL FROM SURFACE NEWOBJ TO I
C     OR TO I-1 IF I-1 IS "PERFECT'
C       RAYRAY(23,SURF)=1 FOR NOT RV, -1 FOR RV
C       RAYRAY(24,SURF)=1.0D0 FOR POS RAY, -1 FOR NEG RAY
C       RAYRAY(25,SURF)=RAY ENERGY TERM
C       RAYRAY(26,SURF)=RAY XL DIR COS
C       RAYRAY(27,SURF)=RAY XM DIR COS
C       RAYRAY(28,SURF)=RAY XN DIR COS
C       RAYRAY(29,SURF)=RAY YL DIR COS
C       RAYRAY(30,SURF)=RAY YM DIR COS
C       RAYRAY(31,SURF)=RAY YN DIR COS

C       RAYRAY(41,SURF) TO RAYRAY(50,SURF) RESERVED
              RAYRAY(32,I)=WW3
              RAYRAY(1,I)=X
              RAYRAY(2,I)=Y
              RAYRAY(3,I)=Z
              RAYRAY(4,I)=L
              RAYRAY(5,I)=M
              RAYRAY(6,I)=N
              RAYRAY(26,I)=(M*YN)-(N*YM)
              RAYRAY(27,I)=-((L*YN)-(N*YL))
              RAYRAY(28,I)=(L*YM)-(M*YL)
              RAYRAY(9,I)=COSI
              RAYRAY(10,I)=COSIP
C
C     WHAT IS THE SIGN OF THE INDEX IN THE I-1 SPACE
              IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) THEN
                  SNINDX=DABS(ALENS(45+INT(WW3),I-1))/ALENS(45+INT(WW3),I-1)
                  SNIND2=DABS(ALENS(45+INT(WW3),I))/ALENS(45+INT(WW3),I)
                  RN1=(ALENS(45+INT(WW3),I-1))
                  RN2=(ALENS(45+INT(WW3),I))
              END IF
              IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) THEN
                  SNINDX=DABS(ALENS(65+INT(WW3),I-1))/ALENS(65+INT(WW3),I-1)
                  SNIND2=DABS(ALENS(65+INT(WW3),I))/ALENS(65+INT(WW3),I)
                  RN1=(ALENS(65+INT(WW3),I-1))
                  RN2=(ALENS(65+INT(WW3),I))
              END IF
              RAYRAY(29,I)=YL
              RAYRAY(30,I)=YM
              RAYRAY(31,I)=YN
              RAYRAY(13,I)=LN
              RAYRAY(14,I)=MN
              RAYRAY(15,I)=NN
              RAYRAY(16,I)=XOLD
              RAYRAY(17,I)=YOLD
              RAYRAY(18,I)=ZOLD
              RAYRAY(19,I)=LOLD
              RAYRAY(20,I)=MOLD
              RAYRAY(21,I)=NOLD
C
C     PHYSICAL LENGTH OF THE RAY FROM I-1 TO I IS THE MAG OF THE
C     DIST TRAVELED TIME THE SNINDX TIMES -1 IF RV IN THE SPACE
C     FROM I-1 TO I
C
C     THE NEXT LINES FIX THE RAY DIRECTION WHEN THERE IS
C     NO Z COMPONENT OF MOTION.
C     THE MAGNITUDE OF THE DISTANCE THE RAY TRAVELED FROM I-1 TO I
C     IS ALWAYS JUST:
C
              IF(NUMHITS(I-1).EQ.1.AND.I.GE.1) THEN
                  RAYRAY(8,I)=DSQRT(
     1            ((RAYRAY(3,I)-ZOLD)**2)+((RAYRAY(2,I)-YOLD)**2)
     2            +((RAYRAY(1,I)-XOLD)**2))
              ELSE
C       NUMHITS STORED ON LAST SURFACE WAS GREATER THAN 1
                  RAYRAY(8,I)=0.0D0
C       I-1 WAS AN NSS
                  XOLD=0.0D0
                  YOLD=0.0D0
                  ZOLD=0.0D0
                  DO N_HITS=1,NUMHITS(I-1)
                      STEPL=DSQRT(
     2                (((MULTIRAY_DATA(3,I-1,N_HITS))-ZOLD)**2)+
     2                (((MULTIRAY_DATA(2,I-1,N_HITS))-YOLD)**2)+
     2                (((MULTIRAY_DATA(1,I-1,N_HITS))-XOLD)**2)
     3                )
                      IF(N_HITS.EQ.1) STEPL1=STEPL
                      RAYRAY(8,I)=RAYRAY(8,I)+STEPL
C       CREATE NEW XOLD,YOLD,ZOLD
                      XOLD=MULTIRAY_DATA(1,I-1,N_HITS)
                      YOLD=MULTIRAY_DATA(2,I-1,N_HITS)
                      ZOLD=MULTIRAY_DATA(3,I-1,N_HITS)
                  END DO
                  RAYRAY(8,I)=RAYRAY(8,I)-STEPL1
              END IF
C
              IF(RV) RAYRAY(8,I)=-RAYRAY(8,I)
              IF(.NOT.RV) RAYRAY(8,I)=RAYRAY(8,I)
              IF(DABS(RAYRAY(8,I)).GE.1.0D10) RAYRAY(8,I)=0.0D0
C
              IF(SNIND2.GT.0.0D0) RAYRAY(24,I)=1.0D0
              IF(SNIND2.LT.0.0D0) RAYRAY(24,I)=-1.0D0
C
              IF(GLANAM(I-1,2).EQ.'PERFECT      ') THEN
                  RAYRAY(7,I)=0.0D0
                  RAYRAY(8,I)=0.0D0
              END IF
              IF(GLANAM(I-1,2).EQ.'IDEAL        ') THEN
                  RAYRAY(8,I)=-(ALENS(121,I-1)-ALENS(3,I-1))*RAYRAY(6,I-1)
              END IF
              IF(INT(WW3).GE.1.AND.INT(WW3).LE.5)
     1        RAYRAY(7,I)=RAYRAY(8,I)*DABS(ALENS(45+INT(WW3),(I-1)))
              IF(INT(WW3).GE.6.AND.INT(WW3).LE.10)
     1        RAYRAY(7,I)=RAYRAY(8,I)*DABS(ALENS(65+INT(WW3),(I-1)))
              IF(.NOT.RV) RAYRAY(7,I)=RAYRAY(7,I)+PHASE
              IF(RV) RAYRAY(7,I)=RAYRAY(7,I)-PHASE
C
              IF(L.EQ.0.0D0) THEN
                  IF(N.GE.0.0D0) RAYRAY(11,I)=0.0D0
                  IF(N.LT.0.0D0) RAYRAY(11,I)=PII
              ELSE
                  IF(DABS(L).GE.DABS(1.0D35*N)) THEN
                      IF(L.GE.0.0D0) RAYRAY(11,I)=PII/2.0D0
                      IF(L.LT.0.0D0) RAYRAY(11,I)=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(L).EQ.0.0D0.AND.DABS(N).EQ.0.0D0) THEN
                          RAYRAY(11,I)=0.0D0
                      ELSE
                          RAYRAY(11,I)=DATAN2(L,N)
                      END IF
                      IF((RAYRAY(11,I)).LT.0.0D0) RAYRAY(11,I)=
     1                RAYRAY(11,I)+(TWOPII)
                  END IF
              END IF
              IF(M.EQ.0.0D0) THEN
                  IF(N.GE.0.0D0) RAYRAY(12,I)=0.0D0
                  IF(N.LT.0.0D0) RAYRAY(12,I)=PII
              ELSE
                  IF(DABS(M).GE.DABS(1.0D35*N)) THEN
                      IF(M.GE.0.0D0) RAYRAY(12,I)=PII/2.0D0
                      IF(M.LT.0.0D0) RAYRAY(12,I)=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(M).EQ.0.0D0.AND.DABS(N).EQ.0.0D0) THEN
                          RAYRAY(12,I)=0.0D0
                      ELSE
                          RAYRAY(12,I)=DATAN2(M,N)
                      END IF
                      IF((RAYRAY(12,I)).LT.0.0D0) RAYRAY(12,I)=
     1                RAYRAY(12,I)+(TWOPII)
                  END IF
              END IF
              RAYRAY(22,I)=RAYRAY(22,(I-1))+RAYRAY(7,I)
              IF(STOPP.EQ.1) THEN
C NEW STUFF 6/2/94
                  IF(KKK.EQ.1.AND.RAYCOD(1).EQ.1.AND..NOT.ITRACE) THEN
                      JKX=0.0D0
                      JKY=0.0D0
                      STOPP=0
                      KKK=KKK+1
                      GO TO 989
                  END IF
                  FAIL=.TRUE.
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
                      CALL MACFAL
                  END IF
C       IF GLOBAL IS TRUE, CALCULATE THE CURRENT RAY'S GLOBAL
C       COORDINATES AND LOAD THEM IN ARRAY GLRAY
                  IF(GLOBE) THEN
                      CALL GLBRAY
                  END IF
                  RETURN
              ELSE
                  FAIL=.FALSE.
                  STOPP=0
                  RAYEXT=.TRUE.
              END IF
C
C       CHECK THE RAY HEIGHT AT
C       NEWR AND ADJUST THE Y1AIM AND X1AIM AT SURFACE
C       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWR IS WITHIN
C       AIMTOL . MAXIMUN NUMBER OF ITERRATIONS IS
C       NRAITR. (DEFAULT IS 100).
C
              IF(I.EQ.NEWREF) THEN
C       CALCULATE TARX AND TARY
                  IF(DABS(ALENS(9,I)).GE.1.0D0.AND.
     1            DABS(ALENS(9,I)).LE.6.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
C     CLAPT=TRUE FOR CLAP TILTS AND DECENTED AND FALSE OTHERWISE
                      CLAPT=.FALSE.
                      IF(ALENS(12,I).NE.0.0D0.OR.ALENS(13,I).NE.0.0D0.OR.ALENS(15,I)
     1                .NE.0.0D0) CLAPT=.TRUE.
C       REF SURF HAS CLAP ON IT
C       THE VALUES OF TARY AND TARX ARE COORDINATES IN THE
C       LOCAL COORDINATE SYSTEM OF THE REFERENCE SYSTEM.
C       IF THE CLAP IS DECENTERED, THE TAR() VALUES ARE
C       MODIFIED BY ADDING THE CLAP DECENTRATIONS SINCE THE RAY
C       IS AIMED TO THE RELATIVE REFERENCE SURFACE COORDINATES.
C
C       SET TARGET TO CENTER OF DECENTERED CLAP, ALENS(12,I),
C       AND ALENS(13,I) ARE CLAP DECENTRATIONS
C
                      WWW1=WW1
                      WWW2=WW2
                      IF(SYSTEM1(70).EQ.1.0D0.AND.ALENS(1,I).NE.0.0D0.AND.
     1                ALENS(9,I).EQ.1.0D0.AND.ALENS(12,I).EQ.0.0D0.AND.
     2                ALENS(13,I).EQ.0.0D0.AND.ALENS(15,I).EQ.0.0D0) THEN
                          IF(DABS(1.0D0/ALENS(1,I)).GE.DABS(ALENS(10,I)).AND.
     1                    DABS(1.0D0/ALENS(1,I)).GE.DABS(ALENS(11,I)))
     1                    CALL APLANA(I,WW1,WW2,WWW1,WWW2)
                      END IF
C
C       CIRCULAR CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.1.0D0) THEN
                          IF(CLAPT) THEN
                              IF(ALENS(10,I).LE.ALENS(11,I)) THEN
                                  TARY=(ALENS(10,I)*WWW1)
                                  TARX=(ALENS(10,I)*WWW2)
                              ELSE
                                  TARY=(ALENS(11,I)*WWW1)
                                  TARX=(ALENS(11,I)*WWW2)
                              END IF
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
C     NO CLAP DEC OR TILTS
                              TARY=(ALENS(10,I)*WWW1)
                              TARX=(ALENS(10,I)*WWW2)
                              IF(ALENS(10,I).LE.ALENS(11,I)) THEN
                                  TARY=(ALENS(10,I)*WWW1)
                                  TARX=(ALENS(10,I)*WWW2)
                              ELSE
                                  TARY=(ALENS(11,I)*WWW1)
                                  TARX=(ALENS(11,I)*WWW2)
                              END IF
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
C       NOW IS THE REF SURF ORIENTATION ANGLE ?
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT CIRCULAR CLAP
                      END IF
C        RECT CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.2.0D0) THEN
                          IF(CLAPT) THEN
                              IF(ANAAIM) THEN
                                  TARY=(ALENS(10,I)*WW1)
                                  TARX=(ALENS(11,I)*WW2)
                              ELSE
                                  IF(DABS(ALENS(10,I)).GT.DABS(ALENS(11,I))) THEN
                                      TARY=(ALENS(10,I)*WW1)
                                      TARX=(ALENS(10,I)*WW2)
                                  ELSE
                                      TARY=(ALENS(11,I)*WW1)
                                      TARX=(ALENS(11,I)*WW2)
                                  END IF
                              END IF
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              TARY=(ALENS(10,I)*WW1)
                              TARX=(ALENS(11,I)*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT RECT CLAP
                      END IF
C        ELIP CLAP
C
                      YVALUE=ALENS(10,I)
                      XVALUE=ALENS(11,I)
C
                      IF(DABS(ALENS(9,I)).EQ.3.0D0) THEN
                          IF(CLAPT) THEN
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT ELIP CLAP
                      END IF
C        RCTK CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.4.0D0) THEN
                          IF(CLAPT) THEN
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(11,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(11,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT RCTK CLAP
                      END IF
C        POLY CLAP
C
                      IF(DABS(ALENS(9,I)).EQ.5.0D0) THEN
                          IF(CLAPT) THEN
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(10,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              YVALUE=ALENS(10,I)
                              XVALUE=ALENS(10,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT POLY CLAP
                      END IF
                      IF(DABS(ALENS(9,I)).EQ.6.0D0) THEN
                          IF(CLAPT) THEN
                              YVALUE=ALENS(14,I)
                              XVALUE=ALENS(14,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              TARX=TARX+ALENS(13,I)
                              TARY=TARY+ALENS(12,I)
C       NOW IS THE CLAP TILTED ?
                              GAMMA=(ALENS(15,I)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          ELSE
                              YVALUE=ALENS(11,I)
                              XVALUE=ALENS(11,I)
                              TARY=(YVALUE*WW1)
                              TARX=(XVALUE*WW2)
                              IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                              IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                              GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                              TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                              TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                              TARY=TARRY
                              TARX=TARRX
                          END IF
C       NOT IPOLY CLAP
                      END IF
C
                  ELSE
C       NO CLAP ON REF SURF OR MULTI-CLAP
                      TARY=(PXTRAY(1,I)*WW1)
                      TARX=(PXTRAX(1,I)*WW2)
                      IF(SYSTEM1(128).NE.0.0D0) TARX=-TARX
                      IF(SYSTEM1(129).NE.0.0D0) TARY=-TARY
                      GAMMA=(SYSTEM1(59)*(PII))/180.0D0
                      TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                      TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                      TARY=TARRY
                      TARX=TARRX
                  END IF
C
                  TEST=DSQRT(((TARX-X)**2)+((TARY-Y)**2))
                  IF(TEST.LE.AIMTOL
     1            .OR.SYSTEM1(62).EQ.0.0D0.OR.ITRACE) THEN
C       AIM IS GOOD ENOUGH, PROCEED
                      AIMOK=.TRUE.
                      REFMISS=.FALSE.
                      CALL MISSREF(X,Y)
                      GO TO 100
                  ELSE
                      AIMOK=.FALSE.
C       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                  END IF
C
C       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
C       X1ONE AND Y1ONE ARE THE FIRST SET OF RAY
C       COORDINATES AT SURFACE 1
                  X1ONE=X1LAST
                  Y1ONE=Y1LAST
C       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
C       X1LAST AND Y1LAST ARE THE LAST RAY COORDINATES
C       AT SURFACE 1
                  X1LAST=XAIMOL
                  Y1LAST=YAIMOL
C       SET RXONE AND RYONE TO RXLAST AND RYLAST
C       RYONE ANE RXONE ARE THE FIRST SET OF RAY
C       COORDINATES AT THE REFERENCE SURFACE
                  RXONE=RXLAST
                  RYONE=RYLAST
C       SET RXLAST AND RYLAST TO X AND Y
C       RXLAST AND RYLAST ARE THE LAST X AND Y RAY COORDINATES
C       ON THE REFERENCE SURFACE.
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
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                      R_TX=X1AIM
                      R_TY=Y1AIM
                      R_TZ=Z1AIM
                      CALL BAKONE
                      X1AIM=R_TX
                      Y1AIM=R_TY
                      Z1AIM=R_TZ
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
 10       CONTINUE
C       CACOCH IS THE FLAG WHICH TELLS WHETHER OR NOT
C       TO CHECK FOR CLAP/COBS INTERFERENCE.
C       IF CACOCH=0 DO NOT CHECK
C       IF CACOCH=1 DO THE CHECK
C       HERE WE CHECK FOR BLOCKAGE BY CLAP OR COBS.
C       AND DO THE APPROPRIATE THINGS.
          IF(CACOCH.EQ.1) THEN
              DO R_I=NEWOBJ+1,NEWIMG-1
C       CALL CLAP CHECKING ROUTINE
                  R_X=RAYRAY(1,R_I)
                  R_Y=RAYRAY(2,R_I)
                  R_Z=RAYRAY(3,R_I)
C     DON'T CHECK ON OBJECT OR IMAGE SURFACES.
C
                  MMSG=MSG
                  IF(DABS(ALENS(34,R_I)).NE.24.0D0) THEN
C     NO CLAP/COBS CHECK FOR TYPE 24 SPECIAL SURFACE
                      IF(INT(ALENS(127,R_I)).EQ.0.AND.INT(ALENS(128,R_I)).EQ.0) THEN
                          CALL CACHEK(0.0D0,0.0D0,0.0D0,0)
                      ELSE
                          IF(INT(ALENS(127,R_I)).NE.0) THEN
                              DO JK=1,INT(ALENS(127,R_I))
                                  IF(MMSG) THEN
                                      MSG=.TRUE.
                                      IF(JK.LT.INT(ALENS(127,R_I))) MSG=.FALSE.
                                  END IF
                                  JK1=MULTCLAP(JK,1,R_I)
                                  JK2=MULTCLAP(JK,2,R_I)
                                  JK3=MULTCLAP(JK,3,R_I)
                                  CALL CACHEK(JK1,JK2,JK3,1)
                                  IF(RAYCOD(1).EQ.0) THEN
                                      SPDCD1=RAYCOD(1)
                                      SPDCD2=RAYCOD(2)
                                      STOPP=0
                                      RAYEXT=.TRUE.
                                      GO TO 25
                                  END IF
                              END DO
 25                           CONTINUE
                          END IF
                          IF(INT(ALENS(128,R_I)).NE.0) THEN
                              DO JK=1,INT(ALENS(128,R_I))
                                  IF(MMSG) THEN
                                      MSG=.TRUE.
                                  END IF
                                  JK1=MULTCOBS(JK,1,R_I)
                                  JK2=MULTCOBS(JK,2,R_I)
                                  JK3=MULTCOBS(JK,3,R_I)
                                  CALL CACHEK(JK1,JK2,JK3,2)
                                  IF(RAYCOD(1).NE.0) THEN
                                      SPDCD1=RAYCOD(1)
                                      SPDCD2=RAYCOD(2)
                                      STOPP=1
                                      RAYEXT=.TRUE.
                                      GO TO 26
                                  END IF
                              END DO
 26                           CONTINUE
                          END IF
                      END IF
                  END IF
C
C       THIS ROUTINE SETS THE FLAGS
C       CAERAS AND COERAS
C       SET IF THE CURRENT SURFACE HAD A COBS OR
C       CLAP ERASE.
                  IF(STOPP.EQ.1) GO TO 90
                  STOPP=0
                  FAIL=.FALSE.
                  RAYEXT=.TRUE.
C       CONTINUE THE RAYTRACE
              END DO
              GO TO 91
          ELSE
C       NO CHECK TO BE MADE FOR CLAPS/COBS BLOCKAGE
          END IF
          GO TO 91
 90       CONTINUE
          FAIL=.TRUE.
          RAYEXT=.FALSE.
          POLEXT=.FALSE.
          IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
              CALL MACFAL
          END IF
 91       CONTINUE
C
C     FINISHED CLAP/COBS CHEKING
C       IF GLOBAL IS TRUE, CALCULATE THE CURRENT RAY'S GLOBAL
C       COORDINATES AND LOAD THEM IN ARRAY GLRAY
          IF(GLOBE) THEN
              CALL GLBRAY
          END IF
          RAYRAY(25,NEWOBJ:NEWIMG)=0.0D0
          RAYRAY(34:38,NEWOBJ:NEWIMG)=0.0D0
C     COMPUTE RAY ENERGY
          RAYRAY(34:38,I)=0.0D0
          DO I=NEWOBJ,NEWIMG
              IF(I.EQ.NEWOBJ) THEN
                  IF(REFEXT) THEN
                      RAYRAY(25,I)=WW4*REFRY(9,NEWOBJ)
                  ELSE
                      RAYRAY(25,I)=WW4
                  END IF
              ELSE
                  RAYRAY(25,I)=RAYRAY(25,I-1)
              END IF
              IF(I.EQ.NEWOBJ) THEN
                  IF(WA3.GE.1.AND.WA3.LE.5) THEN
                      RN1=(ALENS(45+WA3,I))
                      RN2=(ALENS(45+WA3,I))
                  END IF
                  IF(WA3.GE.6.AND.WA3.LE.10) THEN
                      RN1=(ALENS(65+WA3,I))
                      RN2=(ALENS(65+WA3,I))
                  END IF
              ELSE
                  IF(WA3.GE.1.AND.WA3.LE.5) THEN
                      RN1=(ALENS(45+WA3,I-1))
                      RN2=(ALENS(45+WA3,I))
                  END IF
                  IF(WA3.GE.6.AND.WA3.LE.10) THEN
                      RN1=(ALENS(65+WA3,I-1))
                      RN2=(ALENS(65+WA3,I))
                  END IF
              END IF
              IF(ALENS(34,I).EQ.19.0D0) THEN
C     CALL THE GRIDS ROUTINE WITH ARG = 2
C     THIS CAUSES THE RAY ENERGY TO BE MULTIPLIED BY THE
C     APODIZATION REPRESENTED IN THE APGRI FILE
                  ISURF=I
                  GERROR=.FALSE.
                  XPASS=X
                  YPASS=Y
                  IPASS1=2
                  CALL GRIDS(2,ISURF,GERROR)
                  IF(.NOT.GERROR) GRIDSUNLOADED19(I)=.FALSE.
                  IF(GERROR) THEN
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'GRID FILE DOES NOT EXIST FOR THIS SURFACE'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
                          CALL SHOWIT(1)
                      END IF
                      STOPP=1
                      RAYCOD(1)=15
                      RAYCOD(2)=I
                      RAYEXT=.FALSE.
                      POLEXT=.FALSE.
                      RETURN
                  END IF
                  FACT_PAR=0.0D0
                  FACT_PER=0.0D0
                  PHASE_PAR=0.0D0
                  PHASE_PER=0.0D0
                  POLANG=0.0D0
              END IF
              IF(SYSTEM1(103).EQ.1.0D0) THEN
C     SCREEN SURFACE
                  IF(I.EQ.INT(SYSTEM1(104))) THEN
C       GOT THE SCREEN SURFACE
                      AOI=DABS(DACOS(RAYRAY(9,I)))
                      D=SYSTEM1(105)
                      H=SYSTEM1(106)
                      S=SYSTEM1(107)
                      IF(DCOS(AOI).EQ.0.0D0.OR.
     1                AOI.GE.DABS(SYSTEM1(108))) THEN
                          FACTOR=0.0D0
                      ELSE
                          FACTOR=PII*(((D)-(H*DSIN(AOI)))
     1                    *(((D)*DCOS(AOI))-(H*DSIN(AOI))))/(4.0D0*S*S*DCOS(AOI))
                      END IF
                      IF(FACTOR.LT.0.0D0) FACTOR=0.0D0
                      RAYRAY(25,I)=RAYRAY(25,I)*FACTOR
                  END IF
              END IF
              IF(ALENS(34,I).NE.19.0D0) THEN
C
C
C     NOT AN APODIZATION SURFACE
                  IF(DUM(I).AND.I.GT.0) THEN
                      RAYRAY(34:38,I)=0.0D0
                  END IF
C       EVEN THOUGH WE DON'T ALWAYS DO POLARIZATION, WE NEED THE POLANG
C       THE UNIT VECTOR JK_CPL,JK_CPM,JK_CPN IS NORMAL TO
C       THE PLANE OF INCIDENCE AND LIES IN A PLANE
C       NORMAL TO THE SURFACE AT THE POINT OF INTERSECTION
                  JK_L1=RAYRAY(19,I)
                  JK_M1=RAYRAY(20,I)
                  JK_N1=RAYRAY(21,I)
                  JK_L2=RAYRAY(4,I)
                  JK_M2=RAYRAY(5,I)
                  JK_N2=RAYRAY(6,I)
                  IF(JK_L1.EQ.JK_L2.AND.JK_M1.EQ.JK_M2.AND.JK_N1.EQ.JK_N2) THEN
                      JK_L1=RAYRAY(13,I)
                      JK_M1=RAYRAY(14,I)
                      JK_N1=RAYRAY(15,I)
                      JK_L2=RAYRAY(4,I)
                      JK_M2=RAYRAY(5,I)
                      JK_N2=RAYRAY(6,I)
                  END IF
                  CALL CROSS_PRODUCT(JK_CPL,JK_CPM,JK_CPN
     1            ,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
                  MAG=DSQRT((JK_CPL**2)+(JK_CPM**2)+(JK_CPN**2))
                  JK_CPL=DABS(JK_CPL)
                  JK_CPM=DABS(JK_CPM)
                  JK_CPN=DABS(JK_CPN)
                  IF((MAG).NE.0.0D0) THEN
                      JK_CPL=JK_CPL/MAG
                      JK_CPM=JK_CPM/MAG
                      JK_CPN=JK_CPN/MAG
                  ELSE
                      JK_CPL=1.0D0
                      JK_CPM=0.0D0
                      JK_CPN=0.0D0
                  END IF
C       THE UNIT VECTOR IN THE
C       INCIDENT DIRECTION IS:
                  JK_L1=JK_CPL
                  JK_M1=JK_CPM
                  JK_N1=JK_CPN
                  JK_L2=RAYRAY(13,I)
                  JK_M2=RAYRAY(14,I)
                  JK_N2=RAYRAY(15,I)
                  CALL CROSS_PRODUCT(SA_CPL,SA_CPM,SA_CPN
     1            ,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
                  MAG=DSQRT((SA_CPL**2)+(SA_CPM**2)+(SA_CPN**2))
                  SA_CPL=DABS(SA_CPL)
                  SA_CPM=DABS(SA_CPM)
                  SA_CPN=DABS(SA_CPN)
                  IF((MAG).NE.0.0D0) THEN
                      SA_CPL=SA_CPL/MAG
                      SA_CPM=SA_CPM/MAG
                      SA_CPN=SA_CPN/MAG
                  ELSE
                      SA_CPL=0.0D0
                      SA_CPM=1.0D0
                      SA_CPN=0.0D0
                  END IF
C       WE NEED TO USE THIS INCIDENT DIRECTION VECTOR BUT IT NEEDS TO BE
C       MODIFIED SO THAT ITS Z-ORIENTATION IS PARALLEL TO THE Z-COORDINATE
C       OF THE RYL,RYM,RYN VECTOR
                  SA_CPN = RYN(I)
                  MAG=DSQRT((1.0D0-(SA_CPN**2))/((SA_CPL**2)+(SA_CPM**2)))
                  SA_CPL=MAG*SA_CPL
                  SA_CPM=MAG*SA_CPM
                  MAG=DSQRT((SA_CPL**2)+(SA_CPM**2)+(SA_CPN**2))
C       THE DOT PRODUCT OF THIS UNIT VECTOR WITH RYL,RYM,RYM UNIT VECTOR
C       GIVES THE COSINE OF THE ANGLE BETWEEN THE Y-VECTOR OF THE RAY
C       AND THE DIRECTION OF INCIDENCE
                  JK_L1=RYL(I)
                  JK_M1=RYM(I)
                  JK_N1=RYN(I)
                  JK_L2=SA_CPL
                  JK_M2=SA_CPM
                  JK_N2=SA_CPN
                  CALL DOT_PRODUCT(DP,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
C       POLARIZATION COSINE IS THEN
                  IF(DP.GT.1.0D0) DP=1.0D0
                  IF(DP.LT.-1.0D0) DP=-1.0D0
                  POLANG=DACOS(DP)
                  IF(POLANG.GT.PII/2.0D0) POLANG=PII-POLANG
                  IF(POLANG.LT.-PII/2.0D0) POLANG=PII+POLANG
                  RAYRAY(38,I)=(POLANG*180.0D0)/PII
                  RAYRAY(39,I)=DCOS(POLANG)*DACOS(RAYRAY(9,I))
                  RAYRAY(40,I)=DSIN(POLANG)*DACOS(RAYRAY(9,I))
C       POL ANG DONE
                  IF(COATSET) THEN
                      J=INT(ALENS(112,I))
                      IF(RAYRAY(9,I).GT.1.0D0) RAYRAY(9,I)=1.0D0
                      IF(RAYRAY(9,I).LT.-1.0D0) RAYRAY(9,I)=-1.0D0
                      IF(RAYRAY(10,I).GT.1.0D0) RAYRAY(10,I)=1.0D0
                      IF(RAYRAY(10,I).LT.-1.0D0) RAYRAY(10,I)=-1.0D0
                      IA=DACOS(RAYRAY(9,I))
                      IAP=DACOS(RAYRAY(10,I))
                      WA3=INT(WW3)
                      PATHL=RAYRAY(8,I)
                      IF(I.EQ.NEWOBJ) OLDABSCOEF(1:10)=0.0D0
                      IF(I.EQ.NEWOBJ) ABSCOEF(1:10)=0.0D0
                      CALL ENERGY_ADJUST(ENERGY_FACTOR,I,J,IA,IAP,RN1,RN2,WA3,POLANG
     1                ,FACT_PAR,FACT_PER,PHASE_PAR,PHASE_PER,PATHL)
                      IF(I.GT.NEWOBJ) RAYRAY(25,I)=RAYRAY(25,I)*ENERGY_FACTOR
                  END IF
                  IF(ALENS(96,I).EQ.1.0D0.AND.ALENS(98,I).NE.0.0D0) THEN
                      IA=DACOS(RAYRAY(9,I))
                      WA3=INT(WW3)
                      ENERGY_FACTOR=1.0D0
                      CALL DIFFRACTION_EFFICIENCY(ENERGY_FACTOR,I,IA,WA3)
                      IF(I.GT.NEWOBJ)RAYRAY(25,I)=RAYRAY(25,I)*ENERGY_FACTOR
                  END IF
                  RAYRAY(34,I)=FACT_PAR
                  RAYRAY(35,I)=FACT_PER
                  RAYRAY(36,I)=PHASE_PAR
                  RAYRAY(37,I)=PHASE_PER
              END IF
          END DO
          RETURN
      END
