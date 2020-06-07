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

C SUB FLDCRV.FOR
      SUBROUTINE FLDCRV(ORIEN,DWORD1,DWORD2,ERROR)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FLDCRV. THIS SUBROUTINE IMPLEMENTS
C       THE FIELD CURVATURE CALCULATIONS
C
C     ORIEN = 0 FOR YZ PLANE CALCULATIONS AT THE REF SURF
C     THE VALUE1 CALCULATED RETURNS IN VARIABLE VALUE1.
C     VALUE1 OF
C     ORIEN = 1 FOR XZ PLANE CALCULATIONS AT THE REF SURF
C     VALUE1 OF
C     ORIEN = 2 FOR AST CALCULATIONS
C     VALUE1 OF THE
C     FOB POSITION GIVEN BY:
C          YFOB=DWORD1
C          XFOB=DWORD2
C     ON FOCAL MODE, FIELD CURVATURE IS DISTANCE FROM FOCUS TO
C     IMAGE SURFACE MEASURED IN A DIRECTION PARALLEL TO THE
C     FOB 0 0 1 RAY.
C     ERROR=0 IF CALCULATION WORKED
C     ERROR=1 IF CALCULATION FAILED
C
          REAL*8 YFC1,XFC1,ZFC1
     1    ,DWORD1,DWORD2,THFINAL1,THNUM,THDEN
     2    ,MYW1,MYW2,XO,YO,ZO,LO,MO,NO,XOO,YOO,ZOO,LOO,MOO,NOO
     3    ,SAG,ARG,ARG1,X,Y,C,K,CX,CY,KX,KY,DX,DY,EX,EY,FX,FY,GX,GY
     4    ,SAG1,SAG2,ARG2,YFC2,XFC2,ZFC2,THFINAL2,PX,PY,PUX,PUY
     5    ,PCX,PCY,PUCX,PUCY,DWORD3,DWORD4,VALUE11,VALUE12
C
          EXTERNAL ARG1,ARG2
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER ERROR,ORIEN,I,GOO
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          DWORD3=0.0D0
          DWORD4=SYSTEM1(11)
C
          ERROR=0
C
          MYW1=DWORD1
          MYW2=DWORD2
          IF(SYSTEM1(30).LE.2.0D0) THEN
C     MODE FOCAL
C     TRACE THE DESIRED GUT RAY WITH DIFFERENTIAL TRACING
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              LDIF2=.TRUE.
              LDIF=.TRUE.
              WQ='        '
              SQ=0
              SST=0
              STI=0
              W1=MYW1
              W2=MYW2
              DF1=0
              DF2=0
              DF3=1
              DF4=1
              DF5=1
              S1=1
              S2=1
              S3=0
              S4=0
              S5=0
              SN=1
C     SET MSG TO FALSE
              MSG=.FALSE.
              ERROR=0
              WC='FOB     '
              CALL FFOB
              REST_KDP(1)=RESTINPT(1)
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              SN=0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              LDIF2=OLDLDIF2
              LDIF=OLDLDIF
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.REFEXT.OR..NOT.RAYEXT) ERROR=1
              IF(ERROR.EQ.1) RETURN
C     PROCEED WITH FLDCRV CALC.
              IF(ORIEN.EQ.0.OR.ORIEN.EQ.2) THEN
C     YZ PLANE CALC WITH RESPECT TO REFERENCE SURFACE
                  XO=RAYRAY(1,NEWIMG)
                  YO=RAYRAY(2,NEWIMG)
                  ZO=RAYRAY(3,NEWIMG)
                  LO=RAYRAY(4,NEWIMG)
                  MO=RAYRAY(5,NEWIMG)
                  NO=RAYRAY(6,NEWIMG)
                  XOO=DIFF(7,NEWIMG)
                  YOO=DIFF(8,NEWIMG)
                  ZOO=DIFF(9,NEWIMG)
                  LOO=DIFF(10,NEWIMG)
                  MOO=DIFF(11,NEWIMG)
                  NOO=DIFF(12,NEWIMG)
                  THNUM=((XO-XOO)*(LO-LOO))+((YO-YOO)*(MO-MOO))
     1            +((ZO-ZOO)*(NO-NOO))
                  THDEN=((LO-LOO)**2)+((MO-MOO)**2)
     1            +((NO-NOO)**2)
                  IF(DABS(THNUM*1.0D-10).LT.DABS(THDEN)) THEN
                      THFINAL1=-(-THNUM/THDEN)
                  ELSE
                      THFINAL1=1.0D20
                  END IF
C     THE FOCUS IS LOCATED AT:
                  XFC1=XO+(THFINAL1*LO)
                  YFC1=YO+(THFINAL1*MO)
                  ZFC1=ZO+(THFINAL1*NO)
              END IF
              IF(ORIEN.EQ.1.OR.ORIEN.EQ.2) THEN
C     XZ PLANE CALC WITH RESPECT TO REFERENCE SURFACE
                  XO=RAYRAY(1,NEWIMG)
                  YO=RAYRAY(2,NEWIMG)
                  ZO=RAYRAY(3,NEWIMG)
                  LO=RAYRAY(4,NEWIMG)
                  MO=RAYRAY(5,NEWIMG)
                  NO=RAYRAY(6,NEWIMG)
                  XOO=DIFF(1,NEWIMG)
                  YOO=DIFF(2,NEWIMG)
                  ZOO=DIFF(3,NEWIMG)
                  LOO=DIFF(4,NEWIMG)
                  MOO=DIFF(5,NEWIMG)
                  NOO=DIFF(6,NEWIMG)
                  THNUM=((XO-XOO)*(LO-LOO))+((YO-YOO)*(MO-MOO))
     1            +((ZO-ZOO)*(NO-NOO))
                  THDEN=((LO-LOO)**2)+((MO-MOO)**2)
     1            +((NO-NOO)**2)
                  IF(DABS(THNUM*1.0D-10).LT.DABS(THDEN)) THEN
                      THFINAL2=-(-THNUM/THDEN)
                  ELSE
                      THFINAL2=1.0D20
                  END IF
C     THE FOCUS IS LOCATED AT:
                  XFC2=XO+(THFINAL2*LO)
                  YFC2=YO+(THFINAL2*MO)
                  ZFC2=ZO+(THFINAL2*NO)
              END IF
C       SAG DONE HERE. WE NEED SAG AT FINAL SURFACE FOR X=XFC AND
C             Y=YFC
C
              IF(ORIEN.EQ.0.OR.ORIEN.EQ.2) THEN
                  I=NEWIMG
C       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
                  IF(ALENS(1,I).EQ.0.0D0
     1            .AND.ALENS(23,I).EQ.0.0D0) THEN
                      X=XFC1
                      Y=YFC1
                      CALL SAGFLT(I,X,Y,SAG)
                      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                  ELSE
C       NOT PLANO WITH ASPHERICS
                  END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
                  IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
                      C=ALENS(1,I)
                      K=ALENS(2,I)
                      X=XFC1
                      Y=YFC1
                      ARG= ARG1(C,K,X,Y)
                      IF(ARG.LT.0.0D0) THEN
                          GOO=1
                          SAG=0.0D0
                      ELSE
                      END IF
C                       PROCEED
                      IF(GOO.NE.1) THEN
                          CALL SAGASP(I,X,Y,SAG)
                          IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                      ELSE
                      END IF
                      GOO=0
                  ELSE
C       NOT ROTATIONALLY SYMMETRIC ASPHERIC
                  END IF
                  IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
                      IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                          CY=ALENS(1,I)
                          KY=ALENS(2,I)
                          DY=ALENS(4,I)
                          EY=ALENS(5,I)
                          FY=ALENS(6,I)
                          GY=ALENS(7,I)
                          CX=ALENS(24,I)
                          KX=ALENS(41,I)
                          DX=ALENS(37,I)
                          EX=ALENS(38,I)
                          FX=ALENS(39,I)
                          GX=ALENS(40,I)
                      ELSE
                      END IF
                      IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                          CX=ALENS(1,I)
                          KX=ALENS(2,I)
                          DX=ALENS(4,I)
                          EX=ALENS(5,I)
                          FX=ALENS(6,I)
                          GX=ALENS(7,I)
                          CY=ALENS(24,I)
                          KY=ALENS(41,I)
                          DY=ALENS(37,I)
                          EY=ALENS(38,I)
                          FY=ALENS(39,I)
                          GY=ALENS(40,I)
                      ELSE
                      END IF
                      X=XFC1
                      Y=YFC1
                      ARG=ARG2(CX,CY,KX,KY,X,Y)
                      IF(ARG.LT.0.0D0) THEN
                          GOO=1
                          SAG=0.0D0
                      ELSE
                      END IF
C                       PROCEED
                      IF(GOO.NE.1) THEN
                          CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                          IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                      ELSE
                      END IF
                      GOO=0
                  ELSE
C       NOT ANAMORPHIC ASPHERIC
                  END IF
                  SAG1=SAG
              END IF
              IF(ORIEN.EQ.1.OR.ORIEN.EQ.2) THEN
                  I=NEWIMG
C       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
                  IF(ALENS(1,I).EQ.0.0D0
     1            .AND.ALENS(23,I).EQ.0.0D0) THEN
                      X=XFC2
                      Y=YFC2
                      CALL SAGFLT(I,X,Y,SAG)
                      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                  ELSE
C       NOT PLANO WITH ASPHERICS
                  END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
                  IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
                      C=ALENS(1,I)
                      K=ALENS(2,I)
                      X=XFC2
                      Y=YFC2
                      ARG= ARG1(C,K,X,Y)
                      IF(ARG.LT.0.0D0) THEN
                          GOO=1
                          SAG=0.0D0
                      ELSE
                      END IF
C                       PROCEED
                      IF(GOO.NE.1) THEN
                          CALL SAGASP(I,X,Y,SAG)
                          IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                      ELSE
                      END IF
                      GOO=0
                  ELSE
C       NOT ROTATIONALLY SYMMETRIC ASPHERIC
                  END IF
                  IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
                      IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                          CY=ALENS(1,I)
                          KY=ALENS(2,I)
                          DY=ALENS(4,I)
                          EY=ALENS(5,I)
                          FY=ALENS(6,I)
                          GY=ALENS(7,I)
                          CX=ALENS(24,I)
                          KX=ALENS(41,I)
                          DX=ALENS(37,I)
                          EX=ALENS(38,I)
                          FX=ALENS(39,I)
                          GX=ALENS(40,I)
                      ELSE
                      END IF
                      IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                          CX=ALENS(1,I)
                          KX=ALENS(2,I)
                          DX=ALENS(4,I)
                          EX=ALENS(5,I)
                          FX=ALENS(6,I)
                          GX=ALENS(7,I)
                          CY=ALENS(24,I)
                          KY=ALENS(41,I)
                          DY=ALENS(37,I)
                          EY=ALENS(38,I)
                          FY=ALENS(39,I)
                          GY=ALENS(40,I)
                      ELSE
                      END IF
                      X=XFC2
                      Y=YFC2
                      ARG=ARG2(CX,CY,KX,KY,X,Y)
                      IF(ARG.LT.0.0D0) THEN
                          GOO=1
                          SAG=0.0D0
                      ELSE
                      END IF
C                       PROCEED
                      IF(GOO.NE.1) THEN
                          CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                          IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                      ELSE
                      END IF
                      GOO=0
                  ELSE
C       NOT ANAMORPHIC ASPHERIC
                  END IF
                  SAG2=SAG
              END IF
              IF(ORIEN.EQ.0) VALUE1=ZFC1-SAG1
              IF(ORIEN.EQ.1) VALUE1=ZFC2-SAG2
              IF(ORIEN.EQ.2) VALUE1=(ZFC1-SAG1)-(ZFC2-SAG2)
              RETURN
          ELSE
C     NOT FOCAL
          END IF
          IF(SYSTEM1(30).GE.3.0D0) THEN
              I=NEWIMG
              CALL GNPRTGEN(I,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR,
     1        DWORD1,DWORD2,DWORD3,DWORD4,0)
              IF(PUY.NE.0) THEN
                  VALUE11=-PY/PUY
              ELSE
                  VALUE11=0.0D0
              END IF
              IF(PUX.NE.0) THEN
                  VALUE12=-PX/PUX
              ELSE
                  VALUE12=0.0D0
              END IF
C
C     NOW CONVERT TO DIOPTERS. CHANGE TO METER UNITS
C
              IF(SYSTEM1(6).EQ.1.0D0) VALUE11=VALUE11/39.3700787402D0
              IF(SYSTEM1(6).EQ.2.0D0) VALUE11=VALUE11/100.0D0
              IF(SYSTEM1(6).EQ.3.0D0) VALUE11=VALUE11/1000.0D0
              IF(SYSTEM1(6).EQ.1.0D0) VALUE12=VALUE12/39.3700787402D0
              IF(SYSTEM1(6).EQ.2.0D0) VALUE12=VALUE12/100.0D0
              IF(SYSTEM1(6).EQ.3.0D0) VALUE12=VALUE12/1000.0D0
              IF(VALUE11.NE.0.0D0) VALUE11=1.0D0/VALUE11
              IF(VALUE11.EQ.0.0D0) VALUE11=0.0D0
              IF(VALUE12.NE.0.0D0) VALUE12=1.0D0/VALUE12
              IF(VALUE12.EQ.0.0D0) VALUE12=0.0D0
              IF(ORIEN.EQ.0) VALUE1=VALUE11
              IF(ORIEN.EQ.1) VALUE1=VALUE12
              IF(ORIEN.EQ.2) VALUE1=VALUE11-VALUE12
          ELSE
C     NOT AFOCAL
          END IF
          RETURN
      END


C SUB FLDOP.FOR
      SUBROUTINE FLDOP(ORIEN,IW1,ERROR)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FLDCRV. THIS SUBROUTINE IMPLEMENTS
C       THE FIELD CURVATURE CALCULATIONS
C
C     ORIEN = 0 FOR YZ PLANE CALCULATIONS AT THE REF SURF
C     THE VALUE1 CALCULATED RETURNS IN VARIABLE VALUE1.
C     VALUE1 OF
C     ORIEN = 1 FOR XZ PLANE CALCULATIONS AT THE REF SURF
C     VALUE1 OF
C     ORIEN = 2 FOR AST CALCULATIONS
C     VALUE1 OF THE
C     FOB POSITION GIVEN BY:
C     ON FOCAL MODE, FIELD CURVATURE IS DISTANCE FROM FOCUS TO
C     IMAGE SURFACE MEASURED IN A DIRECTION PARALLEL TO THE
C     FOB 0 0 1 RAY.
C     ERROR=0 IF CALCULATION WORKED
C     ERROR=1 IF CALCULATION FAILED
C
          REAL*8 YFC1,XFC1,ZFC1
     1    ,THFINAL1,THNUM,THDEN,VALUE11,VALUE12
     2    ,MYW1,MYW2,XO,YO,ZO,LO,MO,NO,XOO,YOO,ZOO,LOO,MOO,NOO
     3    ,SAG,ARG,ARG1,X,Y,C,K,CX,CY,KX,KY,DX,DY,EX,EY,FX,FY,GX,GY
     4    ,SAG1,SAG2,ARG2,YFC2,XFC2,ZFC2,THFINAL2,PX,PY,PUX,PUY
     5    ,PCX,PCY,PUCX,PUCY,DWORD1,DWORD2,MYW3,MYW4,DWORD3,DWORD4
C
          EXTERNAL ARG1,ARG2
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          INTEGER IW1,ERROR,ORIEN,I,GOO
C
          REAL*8 VALUE1
          INTEGER NUM5
          COMMON/GV/VALUE1,NUM5
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsub.inc'
C
          ERROR=0
          GRASET=.FALSE.
C
          MYW1=FIELDY(IW1)
          MYW2=FIELDX(IW1)
          DWORD1=FIELDY(IW1)
          DWORD2=FIELDX(IW1)
          DWORD3=FIELDZ(IW1)
          DWORD4=FIELDW(IW1)
          MYW3=FIELDZ(IW1)
          MYW4=FIELDW(IW1)
          IF(SYSTEM1(30).LE.2.0D0) THEN
C     MODE FOCAL
C     TRACE THE DESIRED GUT RAY WITH DIFFERENTIAL TRACING
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              LDIF2=.TRUE.
              LDIF=.TRUE.
              WQ='        '
              SQ=0
              SST=0
              STI=0
              W1=MYW1
              W2=MYW2
              W3=MYW3
              W4=MYW4
              DF1=0
              DF2=0
              DF3=1
              DF4=1
              DF5=1
              S1=1
              S2=1
              S3=0
              S4=0
              S5=0
              SN=1
C     SET MSG TO FALSE
              MSG=.FALSE.
              ERROR=0
              WC='FOB     '
              CALL FFOB
              REST_KDP(1)=RESTINPT(1)
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
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
              SN=0
              W3=MYW4
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              LDIF2=OLDLDIF2
              LDIF=OLDLDIF
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.REFEXT.OR..NOT.RAYEXT) ERROR=1
              IF(ERROR.EQ.1) RETURN
C     PROCEED WITH FLDCRV CALC.
              IF(ORIEN.EQ.0.OR.ORIEN.EQ.2) THEN
C     YZ PLANE CALC WITH RESPECT TO REFERENCE SURFACE
                  XO=RAYRAY(1,NEWIMG)
                  YO=RAYRAY(2,NEWIMG)
                  ZO=RAYRAY(3,NEWIMG)
                  LO=RAYRAY(4,NEWIMG)
                  MO=RAYRAY(5,NEWIMG)
                  NO=RAYRAY(6,NEWIMG)
                  XOO=DIFF(7,NEWIMG)
                  YOO=DIFF(8,NEWIMG)
                  ZOO=DIFF(9,NEWIMG)
                  LOO=DIFF(10,NEWIMG)
                  MOO=DIFF(11,NEWIMG)
                  NOO=DIFF(12,NEWIMG)
                  THNUM=((XO-XOO)*(LO-LOO))+((YO-YOO)*(MO-MOO))
     1            +((ZO-ZOO)*(NO-NOO))
                  THDEN=((LO-LOO)**2)+((MO-MOO)**2)
     1            +((NO-NOO)**2)
                  IF(DABS(THNUM*1.0D-10).LT.DABS(THDEN)) THEN
                      THFINAL1=-(-THNUM/THDEN)
                  ELSE
                      THFINAL1=1.0D20
                  END IF
C     THE FOCUS IS LOCATED AT:
                  XFC1=XO+(THFINAL1*LO)
                  YFC1=YO+(THFINAL1*MO)
                  ZFC1=ZO+(THFINAL1*NO)
              END IF
              IF(ORIEN.EQ.1.OR.ORIEN.EQ.2) THEN
C     XZ PLANE CALC WITH RESPECT TO REFERENCE SURFACE
                  XO=RAYRAY(1,NEWIMG)
                  YO=RAYRAY(2,NEWIMG)
                  ZO=RAYRAY(3,NEWIMG)
                  LO=RAYRAY(4,NEWIMG)
                  MO=RAYRAY(5,NEWIMG)
                  NO=RAYRAY(6,NEWIMG)
                  XOO=DIFF(1,NEWIMG)
                  YOO=DIFF(2,NEWIMG)
                  ZOO=DIFF(3,NEWIMG)
                  LOO=DIFF(4,NEWIMG)
                  MOO=DIFF(5,NEWIMG)
                  NOO=DIFF(6,NEWIMG)
                  THNUM=((XO-XOO)*(LO-LOO))+((YO-YOO)*(MO-MOO))
     1            +((ZO-ZOO)*(NO-NOO))
                  THDEN=((LO-LOO)**2)+((MO-MOO)**2)
     1            +((NO-NOO)**2)
                  IF(DABS(THNUM*1.0D-10).LT.DABS(THDEN)) THEN
                      THFINAL2=-(-THNUM/THDEN)
                  ELSE
                      THFINAL2=1.0D20
                  END IF
C     THE FOCUS IS LOCATED AT:
                  XFC2=XO+(THFINAL2*LO)
                  YFC2=YO+(THFINAL2*MO)
                  ZFC2=ZO+(THFINAL2*NO)
              END IF
C       SAG DONE HERE. WE NEED SAG AT FINAL SURFACE FOR X=XFC AND
C             Y=YFC
C
              IF(ORIEN.EQ.0.OR.ORIEN.EQ.2) THEN
                  I=NEWIMG
C       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
                  IF(ALENS(1,I).EQ.0.0D0
     1            .AND.ALENS(23,I).EQ.0.0D0) THEN
                      X=XFC1
                      Y=YFC1
                      CALL SAGFLT(I,X,Y,SAG)
                      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                  ELSE
C       NOT PLANO WITH ASPHERICS
                  END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
                  IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
                      C=ALENS(1,I)
                      K=ALENS(2,I)
                      X=XFC1
                      Y=YFC1
                      ARG= ARG1(C,K,X,Y)
                      IF(ARG.LT.0.0D0) THEN
                          GOO=1
                          SAG=0.0D0
                      ELSE
                      END IF
C                       PROCEED
                      IF(GOO.NE.1) THEN
                          CALL SAGASP(I,X,Y,SAG)
                          IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                      ELSE
                      END IF
                      GOO=0
                  ELSE
C       NOT ROTATIONALLY SYMMETRIC ASPHERIC
                  END IF
                  IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
                      IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                          CY=ALENS(1,I)
                          KY=ALENS(2,I)
                          DY=ALENS(4,I)
                          EY=ALENS(5,I)
                          FY=ALENS(6,I)
                          GY=ALENS(7,I)
                          CX=ALENS(24,I)
                          KX=ALENS(41,I)
                          DX=ALENS(37,I)
                          EX=ALENS(38,I)
                          FX=ALENS(39,I)
                          GX=ALENS(40,I)
                      ELSE
                      END IF
                      IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                          CX=ALENS(1,I)
                          KX=ALENS(2,I)
                          DX=ALENS(4,I)
                          EX=ALENS(5,I)
                          FX=ALENS(6,I)
                          GX=ALENS(7,I)
                          CY=ALENS(24,I)
                          KY=ALENS(41,I)
                          DY=ALENS(37,I)
                          EY=ALENS(38,I)
                          FY=ALENS(39,I)
                          GY=ALENS(40,I)
                      ELSE
                      END IF
                      X=XFC1
                      Y=YFC1
                      ARG=ARG2(CX,CY,KX,KY,X,Y)
                      IF(ARG.LT.0.0D0) THEN
                          GOO=1
                          SAG=0.0D0
                      ELSE
                      END IF
C                       PROCEED
                      IF(GOO.NE.1) THEN
                          CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                          IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                      ELSE
                      END IF
                      GOO=0
                  ELSE
C       NOT ANAMORPHIC ASPHERIC
                  END IF
                  SAG1=SAG
              END IF
              IF(ORIEN.EQ.1.OR.ORIEN.EQ.2) THEN
                  I=NEWIMG
C       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
                  IF(ALENS(1,I).EQ.0.0D0
     1            .AND.ALENS(23,I).EQ.0.0D0) THEN
                      X=XFC2
                      Y=YFC2
                      CALL SAGFLT(I,X,Y,SAG)
                      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                  ELSE
C       NOT PLANO WITH ASPHERICS
                  END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
                  IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
                      C=ALENS(1,I)
                      K=ALENS(2,I)
                      X=XFC2
                      Y=YFC2
                      ARG= ARG1(C,K,X,Y)
                      IF(ARG.LT.0.0D0) THEN
                          GOO=1
                          SAG=0.0D0
                      ELSE
                      END IF
C                       PROCEED
                      IF(GOO.NE.1) THEN
                          CALL SAGASP(I,X,Y,SAG)
                          IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                      ELSE
                      END IF
                      GOO=0
                  ELSE
C       NOT ROTATIONALLY SYMMETRIC ASPHERIC
                  END IF
                  IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
                      IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                          CY=ALENS(1,I)
                          KY=ALENS(2,I)
                          DY=ALENS(4,I)
                          EY=ALENS(5,I)
                          FY=ALENS(6,I)
                          GY=ALENS(7,I)
                          CX=ALENS(24,I)
                          KX=ALENS(41,I)
                          DX=ALENS(37,I)
                          EX=ALENS(38,I)
                          FX=ALENS(39,I)
                          GX=ALENS(40,I)
                      ELSE
                      END IF
                      IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                          CX=ALENS(1,I)
                          KX=ALENS(2,I)
                          DX=ALENS(4,I)
                          EX=ALENS(5,I)
                          FX=ALENS(6,I)
                          GX=ALENS(7,I)
                          CY=ALENS(24,I)
                          KY=ALENS(41,I)
                          DY=ALENS(37,I)
                          EY=ALENS(38,I)
                          FY=ALENS(39,I)
                          GY=ALENS(40,I)
                      ELSE
                      END IF
                      X=XFC2
                      Y=YFC2
                      ARG=ARG2(CX,CY,KX,KY,X,Y)
                      IF(ARG.LT.0.0D0) THEN
                          GOO=1
                          SAG=0.0D0
                      ELSE
                      END IF
C                       PROCEED
                      IF(GOO.NE.1) THEN
                          CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                          IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                      ELSE
                      END IF
                      GOO=0
                  ELSE
C       NOT ANAMORPHIC ASPHERIC
                  END IF
                  SAG2=SAG
              END IF
              IF(ORIEN.EQ.0) VALUE1=ZFC1-SAG1
              IF(ORIEN.EQ.1) VALUE1=ZFC2-SAG2
              IF(ORIEN.EQ.2) VALUE1=(ZFC1-SAG1)-(ZFC2-SAG2)
              RETURN
          ELSE
C     NOT FOCAL
          END IF
          IF(SYSTEM1(30).GE.3.0D0) THEN
              I=NEWIMG
              CALL GNPRTGEN(I,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR,
     1        DWORD1,DWORD2,DWORD3,DWORD4,0)
              IF(PUY.NE.0) THEN
                  VALUE11=-PY/PUY
              ELSE
                  VALUE11=0.0D0
              END IF
              IF(PUX.NE.0) THEN
                  VALUE12=-PX/PUX
              ELSE
                  VALUE12=0.0D0
              END IF
C
C     NOW CONVERT TO DIOPTERS. CHANGE TO METER UNITS
C
              IF(SYSTEM1(6).EQ.1.0D0) VALUE11=VALUE11/39.3700787402D0
              IF(SYSTEM1(6).EQ.2.0D0) VALUE11=VALUE11/100.0D0
              IF(SYSTEM1(6).EQ.3.0D0) VALUE11=VALUE11/1000.0D0
              IF(SYSTEM1(6).EQ.1.0D0) VALUE12=VALUE12/39.3700787402D0
              IF(SYSTEM1(6).EQ.2.0D0) VALUE12=VALUE12/100.0D0
              IF(SYSTEM1(6).EQ.3.0D0) VALUE12=VALUE12/1000.0D0
              IF(VALUE11.NE.0.0D0) VALUE11=1.0D0/VALUE11
              IF(VALUE11.EQ.0.0D0) VALUE11=0.0D0
              IF(VALUE12.NE.0.0D0) VALUE12=1.0D0/VALUE12
              IF(VALUE12.EQ.0.0D0) VALUE12=0.0D0
              IF(ORIEN.EQ.0) VALUE1=VALUE11
              IF(ORIEN.EQ.1) VALUE1=VALUE12
              IF(ORIEN.EQ.2) VALUE1=VALUE11-VALUE12
          ELSE
C     NOT AFOCAL
          END IF
          RETURN
      END
C SUB FIELDABS.FOR
      SUBROUTINE FIELDABS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FIELDABS. THIS SUBROUTINE IMPLEMENTS
C       THE DIST, FISHDIST, AST AND FLDCV COMMANDS AT THE CMD LEVEL
C
          CHARACTER UNIFC*8
C
          REAL FLDAN(0:50)
C
          COMMON/FIFI/FLDAN
C
          REAL*8 VALUE1,DWORD1,DWORD2,WOR1(0:50),WOR2(0:50)
     1    ,WOR11,OW3,WOR12,FACTY,ORI,DTA11(0:50),DTA22(0:50)
     2    ,VI,DDTA(0:50),ADTA(0:50)
C
          INTEGER I,NUM5,ERROR,PNTNUM
C
          COMMON/NUMPNT/PNTNUM,ORI,FACTY
C
          COMMON/GV/VALUE1,NUM5
C
          LOGICAL ASTEXT,FLDEXT,DISEXT,FDISEXT
          COMMON/FIELDEXT/ASTEXT,FLDEXT,DISEXT,FDISEXT
C
          COMMON/ABSSS/WOR1,WOR2,DTA11,DTA22,DDTA,ADTA
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(WC.EQ.'FISHDIST') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"FISHDIST" CALCULATES AND DISPLAYS DISTORTION'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"FISHDIST" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=0.0D0
              IF(DF2.EQ.1) W2=1.0D0
              IF(DF3.EQ.1) W3=10.0D0
              IF(W1.LT.0.0D0.OR.W1.GT.360.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #1 MAY RANGE FROM 0 TO 360 DEGREES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #2 MUST BE GREATER THAN 0.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.LT.1.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #3 MUST BE GREATER THAN OR EQUAL TO 1.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.GT.50.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #3 MUST BE LESS THAN OR EQUAL TO 50.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              W3=DBLE(INT(W3))
C
C     CALCULTE FISHDIST
              FDISEXT=.FALSE.
              ERROR=0
              ORI=W1
              FACTY=W2
              OW3=W3
              PNTNUM=INT(OW3)
              DO I=0,PNTNUM
                  IF(I.EQ.0) VI=0.01D0
                  IF(I.NE.0) VI=DBLE(I)
                  DWORD1=(VI/OW3)*(FACTY*DCOS(PII*ORI/180.0D0))
                  DWORD2=(VI/OW3)*(FACTY*DSIN(PII*ORI/180.0D0))
                  IF(DABS(DWORD1).LE.1.0D-10) DWORD1=0.0D0
                  IF(DABS(DWORD2).LE.1.0D-10) DWORD2=0.0D0
                  ERROR=0
                  CACOCH=0
                  CALL FDISTOR(DWORD1,DWORD2,ERROR)
                  IF(ERROR.EQ.1) THEN
                      FDISEXT=.FALSE.
                      WRITE(OUTLYNE,*)
     1                'FISHEYE DISTORION NOT CALCULABLE, NO DATA GENERATED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      PNTNUM=0
                      CALL MACFAL
                      RETURN
                  END IF
                  WOR1(I)=DWORD1
                  WOR2(I)=DWORD2
                  DDTA(I)=VALUE1
              END DO
C     GENERATE PRINTOUT
 500          FORMAT('FISHEYE DISTORTION TABLE')
 602          FORMAT('Y-FIELD POSITION',5X,'X-FIELD POSITION'
     1        ,5X,'PERCENT FISHEYE DISTORTION')
              WRITE(OUTLYNE,500)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,103) W1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,104) W2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,111) SYSTEM1(INT(SYSTEM1(11)))
              CALL SHOWIT(0)
              WRITE(OUTLYNE,602)
              CALL SHOWIT(0)
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D0) THEN
C     ANGULAR FIELD POS
                  WRITE(OUTLYNE,106)
                  CALL SHOWIT(0)
              ELSE
C     SPATIAL FIELD POS
                  IF(SYSTEM1(6).EQ.1.0D0) WRITE(OUTLYNE,107)
                  IF(SYSTEM1(6).EQ.2.0D0) WRITE(OUTLYNE,108)
                  IF(SYSTEM1(6).EQ.3.0D0) WRITE(OUTLYNE,109)
                  IF(SYSTEM1(6).EQ.4.0D0) WRITE(OUTLYNE,110)
                  CALL SHOWIT(0)
              END IF

              DO I=0,INT(OW3)
                  IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                      WOR11=WOR1(I)*SYSTEM1(21)
                      WOR12=WOR2(I)*SYSTEM1(23)
                  ELSE
                      WOR11=WOR1(I)*SYSTEM1(14)
                      WOR12=WOR2(I)*SYSTEM1(16)
                  END IF
                  IF(DABS(WOR11).LT.1.0D-6) WOR11=0.0D0
                  IF(DABS(WOR12).LT.1.0D-6) WOR12=0.0D0
                  FLDAN(I)=REAL(DSQRT((WOR11**2)+(WOR12**2)))
                  WRITE(OUTLYNE,105) WOR11,WOR12,DDTA(I)
                  CALL SHOWIT(0)
              END DO
              FDISEXT=.TRUE.
              RETURN
C
          END IF
          IF(WC.EQ.'DIST') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DIST" CALCULATES AND DISPLAYS DISTORTION'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DIST" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=0.0D0
              IF(DF2.EQ.1) W2=1.0D0
              IF(DF3.EQ.1) W3=10.0D0
              IF(W1.LT.0.0D0.OR.W1.GT.360.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #1 MAY RANGE FROM 0 TO 360 DEGREES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #2 MUST BE GREATER THAN 0.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.LT.1.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #3 MUST BE GREATER THAN OR EQUAL TO 1.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.GT.50.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #3 MUST BE LESS THAN OR EQUAL TO 50.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              W3=DBLE(INT(W3))
C
C     CALCULTE DIST
              DISEXT=.FALSE.
              ERROR=0
              ORI=W1
              FACTY=W2
              OW3=W3
              PNTNUM=INT(OW3)
              DO I=0,PNTNUM
                  IF(I.EQ.0) VI=0.01D0
                  IF(I.NE.0) VI=DBLE(I)
                  DWORD1=(VI/OW3)*(FACTY*DCOS(PII*ORI/180.0D0))
                  DWORD2=(VI/OW3)*(FACTY*DSIN(PII*ORI/180.0D0))
                  IF(DABS(DWORD1).LE.1.0D-10) DWORD1=0.0D0
                  IF(DABS(DWORD2).LE.1.0D-10) DWORD2=0.0D0
                  ERROR=0
                  CACOCH=0
                  CALL DISTOR(DWORD1,DWORD2,ERROR)
                  IF(ERROR.EQ.1) THEN
                      DISEXT=.FALSE.
                      WRITE(OUTLYNE,*)
     1                'DISTORION NOT CALCULABLE, NO DATA GENERATED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      PNTNUM=0
                      CALL MACFAL
                      RETURN
                  END IF
                  WOR1(I)=DWORD1
                  WOR2(I)=DWORD2
                  DDTA(I)=VALUE1
              END DO
C     GENERATE PRINTOUT
 100          FORMAT('DISTORTION TABLE')
 101          FORMAT(' ')
 102          FORMAT('Y-FIELD POSITION',5X,'X-FIELD POSITION'
     1        ,5X,'PERCENT DISTORTION')
 106          FORMAT('     DEGREES    ',5X,'     DEGREES')
 107          FORMAT('      INCH      ',5X,'      INCH ')
 108          FORMAT('      CM        ',5X,'      CM   ')
 109          FORMAT('      MM        ',5X,'      MM   ')
 110          FORMAT('       M        ',5X,'       M   ')
 103          FORMAT('ORIENTATION ANGLE (DEGREES) = ',G13.6)
 104          FORMAT('                     FACTOR = ',G13.6)
 105          FORMAT(2X,G13.6,8X,G13.6,7X,G15.8)
 111          FORMAT('WAVELENGTH(MICROMETER) = ',D13.6)
              WRITE(OUTLYNE,100)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,103) W1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,104) W2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,111) SYSTEM1(INT(SYSTEM1(11)))
              CALL SHOWIT(0)
              WRITE(OUTLYNE,102)
              CALL SHOWIT(0)
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D0) THEN
C     ANGULAR FIELD POS
                  WRITE(OUTLYNE,106)
                  CALL SHOWIT(0)
              ELSE
C     SPATIAL FIELD POS
                  IF(SYSTEM1(6).EQ.1.0D0) WRITE(OUTLYNE,107)
                  IF(SYSTEM1(6).EQ.2.0D0) WRITE(OUTLYNE,108)
                  IF(SYSTEM1(6).EQ.3.0D0) WRITE(OUTLYNE,109)
                  IF(SYSTEM1(6).EQ.4.0D0) WRITE(OUTLYNE,110)
                  CALL SHOWIT(0)
              END IF

              DO I=0,INT(OW3)
                  IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                      WOR11=WOR1(I)*SYSTEM1(21)
                      WOR12=WOR2(I)*SYSTEM1(23)
                  ELSE
                      WOR11=WOR1(I)*SYSTEM1(14)
                      WOR12=WOR2(I)*SYSTEM1(16)
                  END IF
                  IF(DABS(WOR11).LT.1.0D-6) WOR11=0.0D0
                  IF(DABS(WOR12).LT.1.0D-6) WOR12=0.0D0
                  FLDAN(I)=REAL(DSQRT((WOR11**2)+(WOR12**2)))
                  WRITE(OUTLYNE,105) WOR11,WOR12,DDTA(I)
                  CALL SHOWIT(0)
              END DO
              DISEXT=.TRUE.
              RETURN
C
          END IF
          IF(WC.EQ.'FLDCV') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"FLDCV" CALCULATES AND DISPLAYS FIELD CURVATURE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"FLDCV" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=0.0D0
              IF(DF2.EQ.1) W2=1.0D0
              IF(DF3.EQ.1) W3=10.0D0
              IF(W1.LT.0.0D0.OR.W1.GT.360.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #1 MAY RANGE FROM 0 TO 360 DEGREES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #2 MUST BE GREATER THAN 0.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.LT.1.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #3 MUST BE GREATER THAN OR EQUAL TO 1.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.GT.50.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #3 MUST BE LESS THAN OR EQUAL TO 50.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              W3=DBLE(INT(W3))
C
C     CALCULTE FLDCV
              FLDEXT=.FALSE.
              ERROR=0
              ORI=W1
              FACTY=W2
              IF(SYSTEM1(30).GT.2.0D0) UNIFC='DIOPTERS'
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  IF(SYSTEM1(6).EQ.1.0D0) UNIFC='IN'
                  IF(SYSTEM1(6).EQ.2.0D0) UNIFC='CM'
                  IF(SYSTEM1(6).EQ.3.0D0) UNIFC='MM'
                  IF(SYSTEM1(6).EQ.4.0D0) UNIFC=' M'
              END IF
              ORI=W1
              FACTY=W2
              OW3=W3
              PNTNUM=INT(OW3)
              DO I=0,PNTNUM
                  IF(I.EQ.0) VI=1.0D-8
                  IF(I.NE.0) VI=DBLE(I)
                  DWORD1=(VI/OW3)*(FACTY*DCOS(PII*ORI/180.0D0))
                  DWORD2=(VI/OW3)*(FACTY*DSIN(PII*ORI/180.0D0))
                  IF(DABS(DWORD1).LE.1.0D-10) DWORD1=0.0D0
                  IF(DABS(DWORD2).LE.1.0D-10) DWORD2=0.0D0
                  ERROR=0
                  CACOCH=0
                  CALL FLDCRV(0,DWORD1,DWORD2,ERROR)
                  IF(ERROR.EQ.1) THEN
                      FLDEXT=.FALSE.
                      WRITE(OUTLYNE,*)
     1                'FIELD CURVATURE NOT CALCULABLE, NO DATA GENERATED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      PNTNUM=0
                      CALL MACFAL
                      RETURN
                  END IF
C     YZ OT T VALUE1
                  DTA11(I)=VALUE1
                  ERROR=0
                  CACOCH=0
                  CALL FLDCRV(1,DWORD1,DWORD2,ERROR)
                  IF(ERROR.EQ.1) THEN
                      FLDEXT=.FALSE.
                      WRITE(OUTLYNE,*)
     1                'FIELD CURVATURE NOT CALCULABLE, NO DATA GENERATED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  WOR1(I)=DWORD1
                  WOR2(I)=DWORD2
C     XZ OT S VALUE1
                  DTA22(I)=VALUE1
              END DO
C     GENERATE PRINTOUT
 200          FORMAT('FIELD CURVATURE TABLE')
 201          FORMAT(' ')
 202          FORMAT('Y-FIELD POSITION',3X,'X-FIELD POSITION'
     1        ,3X,'Y-FIELD CURV. (YZ)',3X,'X-FIELD CURV. (XZ)')
 206          FORMAT('     DEGREES    ',3X,'     DEGREES')
 207          FORMAT('      INCH      ',3X,'      INCH ')
 208          FORMAT('      CM        ',3X,'      CM   ')
 209          FORMAT('      MM        ',3X,'      MM   ')
 210          FORMAT('       M        ',3X,'       M   ')
 211          FORMAT('FIELD CURVATURE UNITS = ',A8)
 203          FORMAT('ORIENTATION ANGLE (DEGREES) = ',G13.6)
 204          FORMAT('                     FACTOR = ',G13.6)
 205          FORMAT(2X,G13.6,6X,G13.6,5X,G15.8,6X,G15.8)
              WRITE(OUTLYNE,200)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,201)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,203) W1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,204) W2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,111) SYSTEM1(INT(SYSTEM1(11)))
              CALL SHOWIT(0)
              WRITE(OUTLYNE,211) UNIFC
              CALL SHOWIT(0)
              WRITE(OUTLYNE,202)
              CALL SHOWIT(0)
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D0) THEN
C     ANGULAR FIELD POS
                  WRITE(OUTLYNE,206)
                  CALL SHOWIT(0)
              ELSE
C     SPATIAL FIELD POS
                  IF(SYSTEM1(6).EQ.1.0D0) WRITE(OUTLYNE,207)
                  IF(SYSTEM1(6).EQ.2.0D0) WRITE(OUTLYNE,208)
                  IF(SYSTEM1(6).EQ.3.0D0) WRITE(OUTLYNE,209)
                  IF(SYSTEM1(6).EQ.4.0D0) WRITE(OUTLYNE,210)
                  CALL SHOWIT(0)
              END IF

              DO I=0,INT(W3)
                  IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                      WOR11=WOR1(I)*SYSTEM1(21)
                      WOR12=WOR2(I)*SYSTEM1(23)
                  ELSE
                      WOR11=WOR1(I)*SYSTEM1(14)
                      WOR12=WOR2(I)*SYSTEM1(16)
                  END IF
                  IF(DABS(WOR11).LT.1.0D-6) WOR11=0.0D0
                  IF(DABS(WOR12).LT.1.0D-6) WOR12=0.0D0
                  FLDAN(I)=REAL(DSQRT((WOR11**2)+(WOR12**2)))
                  WRITE(OUTLYNE,205) WOR11,WOR12,DTA11(I),DTA22(I)
                  CALL SHOWIT(0)
              END DO
              FLDEXT=.TRUE.
              RETURN
          END IF
          IF(WC.EQ.'AST') THEN
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"AST" CALCULATES AND DISPLAYS ASTIGMATISM (Y-X)'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"AST" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=0.0D0
              IF(DF2.EQ.1) W2=1.0D0
              IF(DF3.EQ.1) W3=10.0D0
              IF(W1.LT.0.0D0.OR.W1.GT.360.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #1 MAY RANGE FROM 0 TO 360 DEGREES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #2 MUST BE GREATER THAN 0.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.LT.1.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #3 MUST BE GREATER THAN OR EQUAL TO 1.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W3.GT.50.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #3 MUST BE LESS THAN OR EQUAL TO 50.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              W3=DBLE(INT(W3))
C
C     CALCULTE AST
              ASTEXT=.FALSE.
              ERROR=0
              ORI=W1
              FACTY=W2
              IF(SYSTEM1(30).GT.2.0D0) UNIFC='DIOPTERS'
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  IF(SYSTEM1(6).EQ.1.0D0) UNIFC='IN'
                  IF(SYSTEM1(6).EQ.2.0D0) UNIFC='CM'
                  IF(SYSTEM1(6).EQ.3.0D0) UNIFC='MM'
                  IF(SYSTEM1(6).EQ.4.0D0) UNIFC=' M'
              END IF
              ORI=W1
              FACTY=W2
              OW3=W3
              PNTNUM=INT(OW3)
              DO I=0,PNTNUM
                  IF(I.EQ.0) VI=1.0D-8
                  IF(I.NE.0) VI=DBLE(I)
                  DWORD1=(VI/OW3)*(FACTY*DCOS(PII*ORI/180.0D0))
                  DWORD2=(VI/OW3)*(FACTY*DSIN(PII*ORI/180.0D0))
                  IF(DABS(DWORD1).LE.1.0D-10) DWORD1=0.0D0
                  IF(DABS(DWORD2).LE.1.0D-10) DWORD2=0.0D0
                  ERROR=0
                  CACOCH=0
                  CALL FLDCRV(2,DWORD1,DWORD2,ERROR)
                  IF(ERROR.EQ.1) THEN
                      ASTEXT=.FALSE.
                      WRITE(OUTLYNE,*)
     1                'ASTIGMATISM NOT CALCULABLE, NO DATA GENERATED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      PNTNUM=0
                      CALL MACFAL
                      RETURN
                  END IF
                  WOR1(I)=DWORD1
                  WOR2(I)=DWORD2
                  ADTA(I)=VALUE1
              END DO
C     GENERATE PRINTOUT
 300          FORMAT('ASTIGMATISM TABLE (Y-X)')
 301          FORMAT(' ')
 302          FORMAT('Y-FIELD POSITION',5X,'X-FIELD POSITION'
     1        ,5X,'   ASTIGMATISM (Y-X)')
 306          FORMAT('     DEGREES    ',5X,'     DEGREES')
 307          FORMAT('      INCH      ',5X,'      INCH ')
 308          FORMAT('      CM        ',5X,'      CM   ')
 309          FORMAT('      MM        ',5X,'      MM   ')
 310          FORMAT('       M        ',5X,'       M   ')
 303          FORMAT('ORIENTATION ANGLE (DEGREES) = ',G13.6)
 304          FORMAT('                     FACTOR = ',G13.6)
 311          FORMAT('ASTIGMATISM UNITS = ',A8)
 305          FORMAT(2X,G13.6,8X,G13.6,10X,G15.8)
              WRITE(OUTLYNE,300)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,301)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,303) W1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,304) W2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,311) UNIFC
              CALL SHOWIT(0)
              WRITE(OUTLYNE,111) SYSTEM1(INT(SYSTEM1(11)))
              CALL SHOWIT(0)
              WRITE(OUTLYNE,302)
              CALL SHOWIT(0)
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D0) THEN
C     ANGULAR FIELD POS
                  WRITE(OUTLYNE,306)
                  CALL SHOWIT(0)
              ELSE
C     SPATIAL FIELD POS
                  IF(SYSTEM1(6).EQ.1.0D0) WRITE(OUTLYNE,307)
                  IF(SYSTEM1(6).EQ.2.0D0) WRITE(OUTLYNE,308)
                  IF(SYSTEM1(6).EQ.3.0D0) WRITE(OUTLYNE,309)
                  IF(SYSTEM1(6).EQ.4.0D0) WRITE(OUTLYNE,310)
                  CALL SHOWIT(0)
              END IF

              DO I=0,INT(W3)
                  IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                      WOR11=WOR1(I)*SYSTEM1(21)
                      WOR12=WOR2(I)*SYSTEM1(23)
                  ELSE
                      WOR11=WOR1(I)*SYSTEM1(14)
                      WOR12=WOR2(I)*SYSTEM1(16)
                  END IF
                  IF(DABS(WOR11).LT.1.0D-6) WOR11=0.0D0
                  IF(DABS(WOR12).LT.1.0D-6) WOR12=0.0D0
                  FLDAN(I)=REAL(DSQRT((WOR11**2)+(WOR12**2)))
                  WRITE(OUTLYNE,305) WOR11,WOR12,ADTA(I)
                  CALL SHOWIT(0)
              END DO
              ASTEXT=.TRUE.
              RETURN
          END IF
          RETURN
      END

C SUB DRAWFAN.FOR
      SUBROUTINE DRAWFAN
C
          IMPLICIT NONE
C
C       THIS SUB DOES THE DRAWFAN COMMAND
C
          LOGICAL FANEXT,DR
C
          COMMON/FANEXI/FANEXT
C
          REAL*8 SW1
C
          INTEGER SA1
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"DRAWFAN" DRAWS THE LAST FAN TRACED'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE='"DRAWFAN" ONLY TAKES NUMERIC WORDS #1 AND #2'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.FANEXT) THEN
              OUTLYNE='NO ABERRATION FAN EXISTS FOR "DRAWFAN" TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1) DR=.TRUE.
          IF(DF2.EQ.0) DR=.FALSE.
          SA1=0
          SW1=0.0D0
          SA1=S1
          SW1=W1
          IF(S1.EQ.1.AND.W1.LE.0.0D0) THEN
              OUTLYNE='FAN SCALE FACTOR MUST BE GREATER THAN 0'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT NEW'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SSIFLG=.TRUE.
          IF(SA1.EQ.1) THEN
              SSI=SW1
              SSIFLG=.FALSE.
          END IF
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOTFANS GO'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          IF(DR) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='DRAW'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          RETURN
      END
C SUB FALRAY.FOR
      SUBROUTINE FALRAY
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FALRAY
C
          INTEGER I,K,NFAILL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          DO K=NEWOBJ,NEWIMG
C     K DESIGNATES THE SURFACE COUNTER IN FALFAL
              NFAILL=0
              DO I=2,ITOT
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
                  ID=I-1
                  CALL SPOTIT(4)
                  IF(DSPOT(17).NE.0.0D0) THEN
                      IF(DSPOT(7).NE.0.0D0) THEN
                          IF(INT(DSPOT(8)).EQ.K) NFAILL=NFAILL+1
                      ELSE
C     RAY DIDN'T FAIL
                      END IF
                  ELSE
C     WEIGHT 0
                  END IF
              END DO
              FALFAL(K)=NFAILL
          END DO
          RETURN
      END
C SUB PLT_FAN.FOR
      SUBROUTINE PLT_FAN
C
          IMPLICIT NONE
C
C       THIS SUB DOES THE PLT_FAN COMMAND
C
          LOGICAL FANEXT
C
          COMMON/FANEXI/FANEXT
C
          REAL*8 SW1
C
          INTEGER SA1
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"PLT_FAN" PLOTS WITHOUT DRAWING THE LAST FAN TRACED'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1.OR.S2.EQ.1) THEN
              OUTLYNE='"PLT_FAN" ONLY TAKES NUMERIC WORD #1'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.FANEXT) THEN
              OUTLYNE='NO ABERRATION FAN EXISTS FOR "PLT_FAN" TO DISPLAY'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          SA1=0
          SW1=0.0D0
          SA1=S1
          SW1=W1
          IF(S1.EQ.1.AND.W1.LE.0.0D0) THEN
              OUTLYNE='FAN SCALE FACTOR MUST BE GREATER THAN 0'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT NEW'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SSIFLG=.TRUE.
          IF(SA1.EQ.1) THEN
              SSI=SW1
              SSIFLG=.FALSE.
          END IF
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOTFANS GO'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          RETURN
      END



      SUBROUTINE GRIDS(ICODE,ISURF,GERROR)
C
          IMPLICIT NONE
C
C     PRIMARY GRID SPSRF SUBROUTINE
C
          REAL*8 BA,AP,FUNC,XR,YR,REFHIT,SUBHIT
     1    ,DX,DY,MAG,LL1,MM1,X1,X2,Y1,Y2,FX,FY
C
          REAL*8 XPASS,YPASS,ZPASS,V11,V12,V21,V22,DXV11,DXV12
     1    ,DXV21,DXV22,DYV11,DYV12,DYV21,DYV22
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          INTEGER I,J,M,IX,IY,ICODE,ALLOERR,ISURF,IX1,IX2,IY1,IY2,II
     1    ,ISF,N1,ENDIT,M1,I1,I2,I3,I4,I5,I6,I7,I8,MM
C
          COMMON/GD1GD1/I1,I2,I3,I4,I5,I6,I7,I8
C
          COMMON/GD2GD2/IX1,IX2,IY1,IY2,DXV11,DXV12,DXV21,DXV22
     1    ,DYV11,DYV12,DYV21,DYV22,V11,V12,V21,V22
C
          COMMON/GD3GD3/IX,IY
C
          CHARACTER FLNAME*12
C
          LOGICAL GERROR,EXIS26
C
          REAL*8 JIMRAY1,JIMRAY2,RAYENERGY
          COMMON/FOOTGRID/JIMRAY1,JIMRAY2,RAYENERGY
C
          REAL*8 GD001,
     1    GD002,GD003,GD004,
     2    GD005,GD006,GD007,GD008,GD009,
     3    GD010,GD011,GD012,GD013,GD014,
     4    GD015,GD016,GD017,GD018,GD019,
     5    GD020,GD021,GD022,GD023,GD024,
     6    GD025,GD026,GD027,GD028,GD029,
     7    GD030,GD031,GD032,GD033,GD034,
     8    GD035,GD036,GD037,GD038,GD039,
     9    GD040,GD041,GD042,GD043,GD044,
     1    GD045,GD046,GD047,GD048,GD049
     2    ,GD050
C
          DIMENSION GD001(:,:,:),
     1    GD002(:,:,:),GD003(:,:,:),GD004(:,:,:),
     2    GD005(:,:,:),GD006(:,:,:),GD007(:,:,:),GD008(:,:,:),GD009(:,:,:),
     3    GD010(:,:,:),GD011(:,:,:),GD012(:,:,:),GD013(:,:,:),GD014(:,:,:),
     4    GD015(:,:,:),GD016(:,:,:),GD017(:,:,:),GD018(:,:,:),GD019(:,:,:),
     5    GD020(:,:,:),GD021(:,:,:),GD022(:,:,:),GD023(:,:,:),GD024(:,:,:),
     6    GD025(:,:,:),GD026(:,:,:),GD027(:,:,:),GD028(:,:,:),GD029(:,:,:),
     7    GD030(:,:,:),GD031(:,:,:),GD032(:,:,:),GD033(:,:,:),GD034(:,:,:),
     8    GD035(:,:,:),GD036(:,:,:),GD037(:,:,:),GD038(:,:,:),GD039(:,:,:),
     9    GD040(:,:,:),GD041(:,:,:),GD042(:,:,:),GD043(:,:,:),GD044(:,:,:),
     1    GD045(:,:,:),GD046(:,:,:),GD047(:,:,:),GD048(:,:,:),GD049(:,:,:)
     2    ,GD050(:,:,:)
C
          ALLOCATABLE :: GD001,
     1    GD002,GD003,GD004,GD005,GD006,GD007,GD008,GD009,
     2    GD010,GD011,GD012,GD013,GD014,GD015,GD016,GD017,GD018,GD019,
     3    GD020,GD021,GD022,GD023,GD024,GD025,GD026,GD027,GD028,GD029,
     4    GD030,GD031,GD032,GD033,GD034,GD035,GD036,GD037,GD038,GD039,
     5    GD040,GD041,GD042,GD043,GD044,GD045,GD046,GD047,GD048,GD049,
     6    GD050
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          SAVE
C     IF ICODE = 1, JUST DEALLOCATE THE GD ARRAYS. THIS HAPPENS
C     WHENEVER SPFIT OR LENNS IS CALLED. THIS FREES MEMORY
C
C     IF ICODE = 2, JUST ALLOCATE AND LOAD THE GRID TYPE 19 SURFACE
C     IF ALENS(88,ISURF).EQ.0.0D0 AND THEN CALCULATE AND APPLY THE
C     APODIZATION FACTOR TO RAYRAY(25,ISURF) AND THEN RETURN
C
C     IF ICODE = 3, JUST ALLOCATE AND LOAD THE GRID TYPE 20 SURFACE
C     IF ALENS(88,ISURF).EQ.0.0D0 AND THEN CALCULATE AND APPLY THE
C     PHASE AND ADJUST THE RAY DIRECTION COSINES AND THEN RETURN
C
C     IF ICODE = 4, JUST ALLOCATE AND LOAD THE GRID TYPE 22 SURFACE
C     IF ALENS(88,ISURF).EQ.0.0D0 AND THEN CALCULATE AND APPLY THE
C     SAG AND ADJUST SURFACE NORMALS AND THEN RETURN

C     IF ICODE = 5, USED FOR SAG CALCULATIONS OF A GRID SAG SURFACE (TYPE 22)
C
C     IF ICODE = 6, JUST ALLOCATE AND LOAD THE GRID TYPE 19 SURFACE
C     IF ALENS(88,ISURF).EQ.0.0D0 AND THEN CALCULATE AND APPLY THE
C     APODIZATION FACTOR TO RAYENERGY AND THEN RETURN
C
          N1=3
C
          IF(ICODE.EQ.1) THEN
              DEALLOCATE(
     1        GD001,GD002,GD003,GD004,GD005,GD006,GD007,GD008,GD009,
     2        GD010,GD011,GD012,GD013,GD014,GD015,GD016,GD017,GD018,GD019,
     3        GD020,GD021,GD022,GD023,GD024,GD025,GD026,GD027,GD028,GD029,
     4        GD030,GD031,GD032,GD033,GD034,GD035,GD036,GD037,GD038,GD039,
     5        GD040,GD041,GD042,GD043,GD044,GD045,GD046,GD047,GD048,GD049,
     6        GD050,STAT=ALLOERR)
              GERROR=.FALSE.
              RETURN
          END IF
C
          IF(ICODE.EQ.2.OR.ICODE.EQ.6) THEN
              IF(ALENS(88,ISURF).EQ.0.0D0) THEN
C     ALLOCATE THE GD ARRAY FOR THIS SURFACE AND LOAD IT
C     DOES THE FILE EXIST AND IS IT OF THE CORRECT SIZE?
C     SET THE FILE NAME
                  IF(FTFL01(1,ISURF).EQ.1.0D0)   FLNAME='APGRID01.DAT'
                  IF(FTFL01(1,ISURF).EQ.2.0D0)   FLNAME='APGRID02.DAT'
                  IF(FTFL01(1,ISURF).EQ.3.0D0)   FLNAME='APGRID03.DAT'
                  IF(FTFL01(1,ISURF).EQ.4.0D0)   FLNAME='APGRID04.DAT'
                  IF(FTFL01(1,ISURF).EQ.5.0D0)   FLNAME='APGRID05.DAT'
                  IF(FTFL01(1,ISURF).EQ.6.0D0)   FLNAME='APGRID06.DAT'
                  IF(FTFL01(1,ISURF).EQ.7.0D0)   FLNAME='APGRID07.DAT'
                  IF(FTFL01(1,ISURF).EQ.8.0D0)   FLNAME='APGRID08.DAT'
                  IF(FTFL01(1,ISURF).EQ.9.0D0)   FLNAME='APGDID09.DAT'
                  IF(FTFL01(1,ISURF).EQ.10.0D0)  FLNAME='APGRID10.DAT'
                  IF(FTFL01(1,ISURF).EQ.11.0D0)  FLNAME='APGRID11.DAT'
                  IF(FTFL01(1,ISURF).EQ.12.0D0)  FLNAME='APGRID12.DAT'
                  IF(FTFL01(1,ISURF).EQ.13.0D0)  FLNAME='APGRID13.DAT'
                  IF(FTFL01(1,ISURF).EQ.14.0D0)  FLNAME='APGRID14.DAT'
                  IF(FTFL01(1,ISURF).EQ.15.0D0)  FLNAME='APGRID15.DAT'
                  IF(FTFL01(1,ISURF).EQ.16.0D0)  FLNAME='APGRID16.DAT'
                  IF(FTFL01(1,ISURF).EQ.17.0D0)  FLNAME='APGRID17.DAT'
                  IF(FTFL01(1,ISURF).EQ.18.0D0)  FLNAME='APGRID18.DAT'
                  IF(FTFL01(1,ISURF).EQ.19.0D0)  FLNAME='APGRID19.DAT'
                  IF(FTFL01(1,ISURF).EQ.20.0D0)  FLNAME='APGRID20.DAT'
                  IF(FTFL01(1,ISURF).EQ.21.0D0)  FLNAME='APGRID21.DAT'
                  IF(FTFL01(1,ISURF).EQ.22.0D0)  FLNAME='APGRID22.DAT'
                  IF(FTFL01(1,ISURF).EQ.23.0D0)  FLNAME='APGRID23.DAT'
                  IF(FTFL01(1,ISURF).EQ.24.0D0)  FLNAME='APGRID24.DAT'
                  IF(FTFL01(1,ISURF).EQ.25.0D0)  FLNAME='APGRID25.DAT'
                  IF(FTFL01(1,ISURF).EQ.26.0D0)  FLNAME='APGRID26.DAT'
                  IF(FTFL01(1,ISURF).EQ.27.0D0)  FLNAME='APGRID27.DAT'
                  IF(FTFL01(1,ISURF).EQ.28.0D0)  FLNAME='APGRID28.DAT'
                  IF(FTFL01(1,ISURF).EQ.29.0D0)  FLNAME='APGRID29.DAT'
                  IF(FTFL01(1,ISURF).EQ.30.0D0)  FLNAME='APGRID30.DAT'
                  IF(FTFL01(1,ISURF).EQ.31.0D0)  FLNAME='APGRID31.DAT'
                  IF(FTFL01(1,ISURF).EQ.32.0D0)  FLNAME='APGRID32.DAT'
                  IF(FTFL01(1,ISURF).EQ.33.0D0)  FLNAME='APGRID33.DAT'
                  IF(FTFL01(1,ISURF).EQ.34.0D0)  FLNAME='APGRID34.DAT'
                  IF(FTFL01(1,ISURF).EQ.35.0D0)  FLNAME='APGRID35.DAT'
                  IF(FTFL01(1,ISURF).EQ.36.0D0)  FLNAME='APGRID36.DAT'
                  IF(FTFL01(1,ISURF).EQ.37.0D0)  FLNAME='APGRID37.DAT'
                  IF(FTFL01(1,ISURF).EQ.38.0D0)  FLNAME='APGRID38.DAT'
                  IF(FTFL01(1,ISURF).EQ.39.0D0)  FLNAME='APGRID39.DAT'
                  IF(FTFL01(1,ISURF).EQ.40.0D0)  FLNAME='APGRID40.DAT'
                  IF(FTFL01(1,ISURF).EQ.41.0D0)  FLNAME='APGRID41.DAT'
                  IF(FTFL01(1,ISURF).EQ.42.0D0)  FLNAME='APGRID42.DAT'
                  IF(FTFL01(1,ISURF).EQ.43.0D0)  FLNAME='APGRID43.DAT'
                  IF(FTFL01(1,ISURF).EQ.44.0D0)  FLNAME='APGRID44.DAT'
                  IF(FTFL01(1,ISURF).EQ.45.0D0)  FLNAME='APGRID45.DAT'
                  IF(FTFL01(1,ISURF).EQ.46.0D0)  FLNAME='APGRID46.DAT'
                  IF(FTFL01(1,ISURF).EQ.47.0D0)  FLNAME='APGRID47.DAT'
                  IF(FTFL01(1,ISURF).EQ.48.0D0)  FLNAME='APGRID48.DAT'
                  IF(FTFL01(1,ISURF).EQ.49.0D0)  FLNAME='APGRID49.DAT'
                  IF(FTFL01(1,ISURF).EQ.50.0D0)  FLNAME='APGRID50.DAT'
                  EXIS26=.FALSE.
                  INQUIRE(FILE=trim(HOME)//FLNAME,EXIST=EXIS26)
                  IF(.NOT.EXIS26) THEN
C     NO FILE EXISTS, SET ERROR CODE AND RETURN
                      GERROR=.TRUE.
                      RETURN
                  END IF
              END IF
          END IF
          IF(ICODE.EQ.3) THEN
              IF(ALENS(88,ISURF).EQ.0.0D0) THEN
C     ALLOCATE THE GRID ARRAY FOR THIS SURFACE AND LOAD IT
C     DOES THE FILE EXIST AND IS IT OF THE CORRECT SIZE?
C     SET THE FILE NAME
                  IF(FTFL01(1,ISURF).EQ.1.0D0)   FLNAME='PHGRID01.DAT'
                  IF(FTFL01(1,ISURF).EQ.2.0D0)   FLNAME='PHGRID02.DAT'
                  IF(FTFL01(1,ISURF).EQ.3.0D0)   FLNAME='PHGRID03.DAT'
                  IF(FTFL01(1,ISURF).EQ.4.0D0)   FLNAME='PHGRID04.DAT'
                  IF(FTFL01(1,ISURF).EQ.5.0D0)   FLNAME='PHGRID05.DAT'
                  IF(FTFL01(1,ISURF).EQ.6.0D0)   FLNAME='PHGRID06.DAT'
                  IF(FTFL01(1,ISURF).EQ.7.0D0)   FLNAME='PHGRID07.DAT'
                  IF(FTFL01(1,ISURF).EQ.8.0D0)   FLNAME='PHGRID08.DAT'
                  IF(FTFL01(1,ISURF).EQ.9.0D0)   FLNAME='PHGRID09.DAT'
                  IF(FTFL01(1,ISURF).EQ.10.0D0)  FLNAME='PHGRID10.DAT'
                  IF(FTFL01(1,ISURF).EQ.11.0D0)  FLNAME='PHGRID11.DAT'
                  IF(FTFL01(1,ISURF).EQ.12.0D0)  FLNAME='PHGRID12.DAT'
                  IF(FTFL01(1,ISURF).EQ.13.0D0)  FLNAME='PHGRID13.DAT'
                  IF(FTFL01(1,ISURF).EQ.14.0D0)  FLNAME='PHGRID14.DAT'
                  IF(FTFL01(1,ISURF).EQ.15.0D0)  FLNAME='PHGRID15.DAT'
                  IF(FTFL01(1,ISURF).EQ.16.0D0)  FLNAME='PHGRID16.DAT'
                  IF(FTFL01(1,ISURF).EQ.17.0D0)  FLNAME='PHGRID17.DAT'
                  IF(FTFL01(1,ISURF).EQ.18.0D0)  FLNAME='PHGRID18.DAT'
                  IF(FTFL01(1,ISURF).EQ.19.0D0)  FLNAME='PHGRID19.DAT'
                  IF(FTFL01(1,ISURF).EQ.20.0D0)  FLNAME='PHGRID20.DAT'
                  IF(FTFL01(1,ISURF).EQ.21.0D0)  FLNAME='PHGRID21.DAT'
                  IF(FTFL01(1,ISURF).EQ.22.0D0)  FLNAME='PHGRID22.DAT'
                  IF(FTFL01(1,ISURF).EQ.23.0D0)  FLNAME='PHGRID23.DAT'
                  IF(FTFL01(1,ISURF).EQ.24.0D0)  FLNAME='PHGRID24.DAT'
                  IF(FTFL01(1,ISURF).EQ.25.0D0)  FLNAME='PHGRID25.DAT'
                  IF(FTFL01(1,ISURF).EQ.26.0D0)  FLNAME='PHGRID26.DAT'
                  IF(FTFL01(1,ISURF).EQ.27.0D0)  FLNAME='PHGRID27.DAT'
                  IF(FTFL01(1,ISURF).EQ.28.0D0)  FLNAME='PHGRID28.DAT'
                  IF(FTFL01(1,ISURF).EQ.29.0D0)  FLNAME='PHGRID29.DAT'
                  IF(FTFL01(1,ISURF).EQ.30.0D0)  FLNAME='PHGRID30.DAT'
                  IF(FTFL01(1,ISURF).EQ.31.0D0)  FLNAME='PHGRID31.DAT'
                  IF(FTFL01(1,ISURF).EQ.32.0D0)  FLNAME='PHGRID32.DAT'
                  IF(FTFL01(1,ISURF).EQ.33.0D0)  FLNAME='PHGRID33.DAT'
                  IF(FTFL01(1,ISURF).EQ.34.0D0)  FLNAME='PHGRID34.DAT'
                  IF(FTFL01(1,ISURF).EQ.35.0D0)  FLNAME='PHGRID35.DAT'
                  IF(FTFL01(1,ISURF).EQ.36.0D0)  FLNAME='PHGRID36.DAT'
                  IF(FTFL01(1,ISURF).EQ.37.0D0)  FLNAME='PHGRID37.DAT'
                  IF(FTFL01(1,ISURF).EQ.38.0D0)  FLNAME='PHGRID38.DAT'
                  IF(FTFL01(1,ISURF).EQ.39.0D0)  FLNAME='PHGRID39.DAT'
                  IF(FTFL01(1,ISURF).EQ.40.0D0)  FLNAME='PHGRID40.DAT'
                  IF(FTFL01(1,ISURF).EQ.41.0D0)  FLNAME='PHGRID41.DAT'
                  IF(FTFL01(1,ISURF).EQ.42.0D0)  FLNAME='PHGRID42.DAT'
                  IF(FTFL01(1,ISURF).EQ.43.0D0)  FLNAME='PHGRID43.DAT'
                  IF(FTFL01(1,ISURF).EQ.44.0D0)  FLNAME='PHGRID44.DAT'
                  IF(FTFL01(1,ISURF).EQ.45.0D0)  FLNAME='PHGRID45.DAT'
                  IF(FTFL01(1,ISURF).EQ.46.0D0)  FLNAME='PHGRID46.DAT'
                  IF(FTFL01(1,ISURF).EQ.47.0D0)  FLNAME='PHGRID47.DAT'
                  IF(FTFL01(1,ISURF).EQ.48.0D0)  FLNAME='PHGRID48.DAT'
                  IF(FTFL01(1,ISURF).EQ.49.0D0)  FLNAME='PHGRID49.DAT'
                  IF(FTFL01(1,ISURF).EQ.50.0D0)  FLNAME='PHGRID50.DAT'
                  EXIS26=.FALSE.
                  INQUIRE(FILE=trim(HOME)//FLNAME,EXIST=EXIS26)
                  IF(.NOT.EXIS26) THEN
C     NO FILE EXISTS, SET ERROR CODE AND RETURN
                      GERROR=.TRUE.
                      RETURN
                  END IF
              END IF
          END IF
          IF(ICODE.EQ.4.OR.ICODE.EQ.5) THEN
              IF(ALENS(88,ISURF).EQ.0.0D0) THEN
C     ALLOCATE THE GRID ARRAY FOR THIS SURFACE AND LOAD IT
C     DOES THE FILE EXIST AND IS IT OF THE CORRECT SIZE?
C     SET THE FILE NAME
                  IF(FTFL01(1,ISURF).EQ.1.0D0)   FLNAME='SGGRID01.DAT'
                  IF(FTFL01(1,ISURF).EQ.2.0D0)   FLNAME='SGGRID02.DAT'
                  IF(FTFL01(1,ISURF).EQ.3.0D0)   FLNAME='SGGRID03.DAT'
                  IF(FTFL01(1,ISURF).EQ.4.0D0)   FLNAME='SGGRID04.DAT'
                  IF(FTFL01(1,ISURF).EQ.5.0D0)   FLNAME='SGGRID05.DAT'
                  IF(FTFL01(1,ISURF).EQ.6.0D0)   FLNAME='SGGRID06.DAT'
                  IF(FTFL01(1,ISURF).EQ.7.0D0)   FLNAME='SGGRID07.DAT'
                  IF(FTFL01(1,ISURF).EQ.8.0D0)   FLNAME='SGGRID08.DAT'
                  IF(FTFL01(1,ISURF).EQ.9.0D0)   FLNAME='SGGRID09.DAT'
                  IF(FTFL01(1,ISURF).EQ.10.0D0)  FLNAME='SGGRID10.DAT'
                  IF(FTFL01(1,ISURF).EQ.11.0D0)  FLNAME='SGGRID11.DAT'
                  IF(FTFL01(1,ISURF).EQ.12.0D0)  FLNAME='SGGRID12.DAT'
                  IF(FTFL01(1,ISURF).EQ.13.0D0)  FLNAME='SGGRID13.DAT'
                  IF(FTFL01(1,ISURF).EQ.14.0D0)  FLNAME='SGGRID14.DAT'
                  IF(FTFL01(1,ISURF).EQ.15.0D0)  FLNAME='SGGRID15.DAT'
                  IF(FTFL01(1,ISURF).EQ.16.0D0)  FLNAME='SGGRID16.DAT'
                  IF(FTFL01(1,ISURF).EQ.17.0D0)  FLNAME='SGGRID17.DAT'
                  IF(FTFL01(1,ISURF).EQ.18.0D0)  FLNAME='SGGRID18.DAT'
                  IF(FTFL01(1,ISURF).EQ.19.0D0)  FLNAME='SGGRID19.DAT'
                  IF(FTFL01(1,ISURF).EQ.20.0D0)  FLNAME='SGGRID20.DAT'
                  IF(FTFL01(1,ISURF).EQ.21.0D0)  FLNAME='SGGRID21.DAT'
                  IF(FTFL01(1,ISURF).EQ.22.0D0)  FLNAME='SGGRID22.DAT'
                  IF(FTFL01(1,ISURF).EQ.23.0D0)  FLNAME='SGGRID23.DAT'
                  IF(FTFL01(1,ISURF).EQ.24.0D0)  FLNAME='SGGRID24.DAT'
                  IF(FTFL01(1,ISURF).EQ.25.0D0)  FLNAME='SGGRID25.DAT'
                  IF(FTFL01(1,ISURF).EQ.26.0D0)  FLNAME='SGGRID26.DAT'
                  IF(FTFL01(1,ISURF).EQ.27.0D0)  FLNAME='SGGRID27.DAT'
                  IF(FTFL01(1,ISURF).EQ.28.0D0)  FLNAME='SGGRID28.DAT'
                  IF(FTFL01(1,ISURF).EQ.29.0D0)  FLNAME='SGGRID29.DAT'
                  IF(FTFL01(1,ISURF).EQ.30.0D0)  FLNAME='SGGRID30.DAT'
                  IF(FTFL01(1,ISURF).EQ.31.0D0)  FLNAME='SGGRID31.DAT'
                  IF(FTFL01(1,ISURF).EQ.32.0D0)  FLNAME='SGGRID32.DAT'
                  IF(FTFL01(1,ISURF).EQ.33.0D0)  FLNAME='SGGRID33.DAT'
                  IF(FTFL01(1,ISURF).EQ.34.0D0)  FLNAME='SGGRID34.DAT'
                  IF(FTFL01(1,ISURF).EQ.35.0D0)  FLNAME='SGGRID35.DAT'
                  IF(FTFL01(1,ISURF).EQ.36.0D0)  FLNAME='SGGRID36.DAT'
                  IF(FTFL01(1,ISURF).EQ.37.0D0)  FLNAME='SGGRID37.DAT'
                  IF(FTFL01(1,ISURF).EQ.38.0D0)  FLNAME='SGGRID38.DAT'
                  IF(FTFL01(1,ISURF).EQ.39.0D0)  FLNAME='SGGRID39.DAT'
                  IF(FTFL01(1,ISURF).EQ.40.0D0)  FLNAME='SGGRID40.DAT'
                  IF(FTFL01(1,ISURF).EQ.41.0D0)  FLNAME='SGGRID41.DAT'
                  IF(FTFL01(1,ISURF).EQ.42.0D0)  FLNAME='SGGRID42.DAT'
                  IF(FTFL01(1,ISURF).EQ.43.0D0)  FLNAME='SGGRID43.DAT'
                  IF(FTFL01(1,ISURF).EQ.44.0D0)  FLNAME='SGGRID44.DAT'
                  IF(FTFL01(1,ISURF).EQ.45.0D0)  FLNAME='SGGRID45.DAT'
                  IF(FTFL01(1,ISURF).EQ.46.0D0)  FLNAME='SGGRID46.DAT'
                  IF(FTFL01(1,ISURF).EQ.47.0D0)  FLNAME='SGGRID47.DAT'
                  IF(FTFL01(1,ISURF).EQ.48.0D0)  FLNAME='SGGRID48.DAT'
                  IF(FTFL01(1,ISURF).EQ.49.0D0)  FLNAME='SGGRID49.DAT'
                  IF(FTFL01(1,ISURF).EQ.50.0D0)  FLNAME='SGGRID50.DAT'
                  EXIS26=.FALSE.
                  INQUIRE(FILE=trim(HOME)//FLNAME,EXIST=EXIS26)
                  IF(.NOT.EXIS26) THEN
C     NO FILE EXISTS, SET ERROR CODE AND RETURN
                      GERROR=.TRUE.
                      RETURN
                  END IF
              END IF
          END IF

          IF(ICODE.EQ.2.OR.ICODE.EQ.3.OR.ICODE.EQ.4.OR.ICODE.EQ.5
     1    .OR.ICODE.EQ.6) THEN
              IF(ALENS(88,ISURF).EQ.0.0D0) THEN
                  IF(ICODE.EQ.2.OR.ICODE.EQ.6) THEN
                      WRITE(OUTLYNE,*)'FOR SURFACE # ',ISURF
                      CALL SHOWIT(1)
                      OUTLYNE='LOADING APODIZATION GRID DATA FROM DISK...'
                      CALL SHOWIT(1)
                  END IF
                  IF(ICODE.EQ.3) THEN
                      WRITE(OUTLYNE,*)'FOR SURFACE # ',ISURF
                      CALL SHOWIT(1)
                      OUTLYNE='LOADING PHASE GRID DATA FROM DISK...'
                      CALL SHOWIT(1)
                  END IF
                  IF(ICODE.EQ.4) THEN
                      WRITE(OUTLYNE,*)'FOR SURFACE # ',ISURF
                      CALL SHOWIT(1)
                      OUTLYNE='LOADING SAG GRID DATA FROM DISK...'
                      CALL SHOWIT(1)
                  END IF
C     ALLOCATE THE GRID ARRAY FOR THIS SURFACE AND LOAD IT
C     DOES THE FILE EXIST AND IS IT OF THE CORRECT SIZE?
C     SET THE FILE NAME
                  EXIS26=.FALSE.
                  INQUIRE(FILE=trim(HOME)//FLNAME,EXIST=EXIS26)
                  IF(.NOT.EXIS26) THEN
C     NO FILE EXISTS, SET ERROR CODE AND RETURN
                      GERROR=.TRUE.
                      RETURN
                  END IF
C
                  OPEN(UNIT=26,FILE=trim(HOME)//FLNAME,
     1            STATUS='UNKNOWN')
                  M1=INT(FTFL01(2,ISURF))
                  M=M1
                  MM=M1**2
C
                  DO I=1,MM
                      READ(UNIT=26,FMT=*,END=777,ERR=6666)IX,IY,FUNC
                  END DO
                  GO TO 777
 6666             GERROR=.TRUE.
                  RETURN
 777              CONTINUE
                  CALL CLOSE_FILE(26,1)
                  OPEN(UNIT=26,FILE=trim(HOME)//FLNAME,
     1            STATUS='UNKNOWN')
C     IF HERE, THEN ALLOCATE THE ARRAY AND READ THE DATA AGAIN
                  ISF=INT(FTFL01(1,ISURF))
                  IF(ISF.EQ.1)   ALLOCATE(GD001(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.2)   ALLOCATE(GD002(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.3)   ALLOCATE(GD003(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.4)   ALLOCATE(GD004(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.5)   ALLOCATE(GD005(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.6)   ALLOCATE(GD006(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.7)   ALLOCATE(GD007(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.8)   ALLOCATE(GD008(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.9)   ALLOCATE(GD009(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.10)  ALLOCATE(GD010(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.11)  ALLOCATE(GD011(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.12)  ALLOCATE(GD012(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.13)  ALLOCATE(GD013(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.14)  ALLOCATE(GD014(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.15)  ALLOCATE(GD015(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.16)  ALLOCATE(GD016(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.17)  ALLOCATE(GD017(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.18)  ALLOCATE(GD018(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.19)  ALLOCATE(GD019(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.20)  ALLOCATE(GD020(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.21)  ALLOCATE(GD021(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.22)  ALLOCATE(GD022(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.23)  ALLOCATE(GD023(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.24)  ALLOCATE(GD024(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.25)  ALLOCATE(GD025(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.26)  ALLOCATE(GD026(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.27)  ALLOCATE(GD027(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.28)  ALLOCATE(GD028(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.29)  ALLOCATE(GD029(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.30)  ALLOCATE(GD030(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.31)  ALLOCATE(GD031(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.32)  ALLOCATE(GD032(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.33)  ALLOCATE(GD033(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.34)  ALLOCATE(GD034(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.35)  ALLOCATE(GD035(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.36)  ALLOCATE(GD036(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.37)  ALLOCATE(GD037(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.38)  ALLOCATE(GD038(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.39)  ALLOCATE(GD039(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.40)  ALLOCATE(GD040(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.41)  ALLOCATE(GD041(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.42)  ALLOCATE(GD042(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.43)  ALLOCATE(GD043(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.44)  ALLOCATE(GD044(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.45)  ALLOCATE(GD045(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.46)  ALLOCATE(GD046(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.47)  ALLOCATE(GD047(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.48)  ALLOCATE(GD048(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.49)  ALLOCATE(GD049(M1,M1,3),STAT=ALLOERR)
                  IF(ISF.EQ.50)  ALLOCATE(GD050(M1,M1,3),STAT=ALLOERR)
C     INITIALIZE TO 0.0D0
                  IF(ICODE.EQ.2.OR.ICODE.EQ.6) BA=1.0D0
                  IF(ICODE.EQ.3) BA=0.0D0
                  IF(ICODE.EQ.4) BA=0.0D0
                  IF(ICODE.EQ.5) BA=0.0D0
                  ISF=INT(FTFL01(1,ISURF))
                  DO I=1,M1
                      DO J=1,M1
                          IF(ISF.EQ.1)   GD001(I,J,1)=BA
                          IF(ISF.EQ.2)   GD002(I,J,1)=BA
                          IF(ISF.EQ.3)   GD003(I,J,1)=BA
                          IF(ISF.EQ.4)   GD004(I,J,1)=BA
                          IF(ISF.EQ.5)   GD005(I,J,1)=BA
                          IF(ISF.EQ.6)   GD006(I,J,1)=BA
                          IF(ISF.EQ.7)   GD007(I,J,1)=BA
                          IF(ISF.EQ.8)   GD008(I,J,1)=BA
                          IF(ISF.EQ.9)   GD009(I,J,1)=BA
                          IF(ISF.EQ.10)  GD010(I,J,1)=BA
                          IF(ISF.EQ.11)  GD011(I,J,1)=BA
                          IF(ISF.EQ.12)  GD012(I,J,1)=BA
                          IF(ISF.EQ.13)  GD013(I,J,1)=BA
                          IF(ISF.EQ.14)  GD014(I,J,1)=BA
                          IF(ISF.EQ.15)  GD015(I,J,1)=BA
                          IF(ISF.EQ.16)  GD016(I,J,1)=BA
                          IF(ISF.EQ.17)  GD017(I,J,1)=BA
                          IF(ISF.EQ.18)  GD018(I,J,1)=BA
                          IF(ISF.EQ.19)  GD019(I,J,1)=BA
                          IF(ISF.EQ.20)  GD020(I,J,1)=BA
                          IF(ISF.EQ.21)  GD021(I,J,1)=BA
                          IF(ISF.EQ.22)  GD022(I,J,1)=BA
                          IF(ISF.EQ.23)  GD023(I,J,1)=BA
                          IF(ISF.EQ.24)  GD024(I,J,1)=BA
                          IF(ISF.EQ.25)  GD025(I,J,1)=BA
                          IF(ISF.EQ.26)  GD026(I,J,1)=BA
                          IF(ISF.EQ.27)  GD027(I,J,1)=BA
                          IF(ISF.EQ.28)  GD028(I,J,1)=BA
                          IF(ISF.EQ.29)  GD029(I,J,1)=BA
                          IF(ISF.EQ.30)  GD030(I,J,1)=BA
                          IF(ISF.EQ.31)  GD031(I,J,1)=BA
                          IF(ISF.EQ.32)  GD032(I,J,1)=BA
                          IF(ISF.EQ.33)  GD033(I,J,1)=BA
                          IF(ISF.EQ.34)  GD034(I,J,1)=BA
                          IF(ISF.EQ.35)  GD035(I,J,1)=BA
                          IF(ISF.EQ.36)  GD036(I,J,1)=BA
                          IF(ISF.EQ.37)  GD037(I,J,1)=BA
                          IF(ISF.EQ.38)  GD038(I,J,1)=BA
                          IF(ISF.EQ.39)  GD039(I,J,1)=BA
                          IF(ISF.EQ.40)  GD040(I,J,1)=BA
                          IF(ISF.EQ.41)  GD041(I,J,1)=BA
                          IF(ISF.EQ.42)  GD042(I,J,1)=BA
                          IF(ISF.EQ.43)  GD043(I,J,1)=BA
                          IF(ISF.EQ.44)  GD044(I,J,1)=BA
                          IF(ISF.EQ.45)  GD045(I,J,1)=BA
                          IF(ISF.EQ.46)  GD046(I,J,1)=BA
                          IF(ISF.EQ.47)  GD047(I,J,1)=BA
                          IF(ISF.EQ.48)  GD048(I,J,1)=BA
                          IF(ISF.EQ.49)  GD049(I,J,1)=BA
                          IF(ISF.EQ.50)  GD050(I,J,1)=BA
                          IF(ICODE.EQ.3.0D0) THEN
                              IF(ISF.EQ.1)   GD001(I,J,2)=BA
                              IF(ISF.EQ.2)   GD002(I,J,2)=BA
                              IF(ISF.EQ.3)   GD003(I,J,2)=BA
                              IF(ISF.EQ.4)   GD004(I,J,2)=BA
                              IF(ISF.EQ.5)   GD005(I,J,2)=BA
                              IF(ISF.EQ.6)   GD006(I,J,2)=BA
                              IF(ISF.EQ.7)   GD007(I,J,2)=BA
                              IF(ISF.EQ.8)   GD008(I,J,2)=BA
                              IF(ISF.EQ.9)   GD009(I,J,2)=BA
                              IF(ISF.EQ.10)  GD010(I,J,2)=BA
                              IF(ISF.EQ.11)  GD011(I,J,2)=BA
                              IF(ISF.EQ.12)  GD012(I,J,2)=BA
                              IF(ISF.EQ.13)  GD013(I,J,2)=BA
                              IF(ISF.EQ.14)  GD014(I,J,2)=BA
                              IF(ISF.EQ.15)  GD015(I,J,2)=BA
                              IF(ISF.EQ.16)  GD016(I,J,2)=BA
                              IF(ISF.EQ.17)  GD017(I,J,2)=BA
                              IF(ISF.EQ.18)  GD018(I,J,2)=BA
                              IF(ISF.EQ.19)  GD019(I,J,2)=BA
                              IF(ISF.EQ.20)  GD020(I,J,2)=BA
                              IF(ISF.EQ.21)  GD021(I,J,2)=BA
                              IF(ISF.EQ.22)  GD022(I,J,2)=BA
                              IF(ISF.EQ.23)  GD023(I,J,2)=BA
                              IF(ISF.EQ.24)  GD024(I,J,2)=BA
                              IF(ISF.EQ.25)  GD025(I,J,2)=BA
                              IF(ISF.EQ.26)  GD026(I,J,2)=BA
                              IF(ISF.EQ.27)  GD027(I,J,2)=BA
                              IF(ISF.EQ.28)  GD028(I,J,2)=BA
                              IF(ISF.EQ.29)  GD029(I,J,2)=BA
                              IF(ISF.EQ.30)  GD030(I,J,2)=BA
                              IF(ISF.EQ.31)  GD031(I,J,2)=BA
                              IF(ISF.EQ.32)  GD032(I,J,2)=BA
                              IF(ISF.EQ.33)  GD033(I,J,2)=BA
                              IF(ISF.EQ.34)  GD034(I,J,2)=BA
                              IF(ISF.EQ.35)  GD035(I,J,2)=BA
                              IF(ISF.EQ.36)  GD036(I,J,2)=BA
                              IF(ISF.EQ.37)  GD037(I,J,2)=BA
                              IF(ISF.EQ.38)  GD038(I,J,2)=BA
                              IF(ISF.EQ.39)  GD039(I,J,2)=BA
                              IF(ISF.EQ.40)  GD040(I,J,2)=BA
                              IF(ISF.EQ.41)  GD041(I,J,2)=BA
                              IF(ISF.EQ.42)  GD042(I,J,2)=BA
                              IF(ISF.EQ.43)  GD043(I,J,2)=BA
                              IF(ISF.EQ.44)  GD044(I,J,2)=BA
                              IF(ISF.EQ.45)  GD045(I,J,2)=BA
                              IF(ISF.EQ.46)  GD046(I,J,2)=BA
                              IF(ISF.EQ.47)  GD047(I,J,2)=BA
                              IF(ISF.EQ.48)  GD048(I,J,2)=BA
                              IF(ISF.EQ.49)  GD049(I,J,2)=BA
                              IF(ISF.EQ.50)  GD050(I,J,2)=BA
                              IF(ISF.EQ.1)   GD001(I,J,3)=BA
                              IF(ISF.EQ.2)   GD002(I,J,3)=BA
                              IF(ISF.EQ.3)   GD003(I,J,3)=BA
                              IF(ISF.EQ.4)   GD004(I,J,3)=BA
                              IF(ISF.EQ.5)   GD005(I,J,3)=BA
                              IF(ISF.EQ.6)   GD006(I,J,3)=BA
                              IF(ISF.EQ.7)   GD007(I,J,3)=BA
                              IF(ISF.EQ.8)   GD008(I,J,3)=BA
                              IF(ISF.EQ.9)   GD009(I,J,3)=BA
                              IF(ISF.EQ.10)  GD010(I,J,3)=BA
                              IF(ISF.EQ.11)  GD011(I,J,3)=BA
                              IF(ISF.EQ.12)  GD012(I,J,3)=BA
                              IF(ISF.EQ.13)  GD013(I,J,3)=BA
                              IF(ISF.EQ.14)  GD014(I,J,3)=BA
                              IF(ISF.EQ.15)  GD015(I,J,3)=BA
                              IF(ISF.EQ.16)  GD016(I,J,3)=BA
                              IF(ISF.EQ.17)  GD017(I,J,3)=BA
                              IF(ISF.EQ.18)  GD018(I,J,3)=BA
                              IF(ISF.EQ.19)  GD019(I,J,3)=BA
                              IF(ISF.EQ.20)  GD020(I,J,3)=BA
                              IF(ISF.EQ.21)  GD021(I,J,3)=BA
                              IF(ISF.EQ.22)  GD022(I,J,3)=BA
                              IF(ISF.EQ.23)  GD023(I,J,3)=BA
                              IF(ISF.EQ.24)  GD024(I,J,3)=BA
                              IF(ISF.EQ.25)  GD025(I,J,3)=BA
                              IF(ISF.EQ.26)  GD026(I,J,3)=BA
                              IF(ISF.EQ.27)  GD027(I,J,3)=BA
                              IF(ISF.EQ.28)  GD028(I,J,3)=BA
                              IF(ISF.EQ.29)  GD029(I,J,3)=BA
                              IF(ISF.EQ.30)  GD030(I,J,3)=BA
                              IF(ISF.EQ.31)  GD031(I,J,3)=BA
                              IF(ISF.EQ.32)  GD032(I,J,3)=BA
                              IF(ISF.EQ.33)  GD033(I,J,3)=BA
                              IF(ISF.EQ.34)  GD034(I,J,3)=BA
                              IF(ISF.EQ.35)  GD035(I,J,3)=BA
                              IF(ISF.EQ.36)  GD036(I,J,3)=BA
                              IF(ISF.EQ.37)  GD037(I,J,3)=BA
                              IF(ISF.EQ.38)  GD038(I,J,3)=BA
                              IF(ISF.EQ.39)  GD039(I,J,3)=BA
                              IF(ISF.EQ.40)  GD040(I,J,3)=BA
                              IF(ISF.EQ.41)  GD041(I,J,3)=BA
                              IF(ISF.EQ.42)  GD042(I,J,3)=BA
                              IF(ISF.EQ.43)  GD043(I,J,3)=BA
                              IF(ISF.EQ.44)  GD044(I,J,3)=BA
                              IF(ISF.EQ.45)  GD045(I,J,3)=BA
                              IF(ISF.EQ.46)  GD046(I,J,3)=BA
                              IF(ISF.EQ.47)  GD047(I,J,3)=BA
                              IF(ISF.EQ.48)  GD048(I,J,3)=BA
                              IF(ISF.EQ.49)  GD049(I,J,3)=BA
                              IF(ISF.EQ.50)  GD050(I,J,3)=BA
                          END IF
                      END DO
                  END DO
C     FILL ARRAY
                  ISF=INT(FTFL01(1,ISURF))
                  DO I=1,MM
                      IF(ISF.EQ.1)   READ(26,FMT=*,END=771)IX,IY,GD001(IX,IY,1)
                      IF(ISF.EQ.2)   READ(26,FMT=*,END=771)IX,IY,GD002(IX,IY,1)
                      IF(ISF.EQ.3)   READ(26,FMT=*,END=771)IX,IY,GD003(IX,IY,1)
                      IF(ISF.EQ.4)   READ(26,FMT=*,END=771)IX,IY,GD004(IX,IY,1)
                      IF(ISF.EQ.5)   READ(26,FMT=*,END=771)IX,IY,GD005(IX,IY,1)
                      IF(ISF.EQ.6)   READ(26,FMT=*,END=771)IX,IY,GD006(IX,IY,1)
                      IF(ISF.EQ.7)   READ(26,FMT=*,END=771)IX,IY,GD007(IX,IY,1)
                      IF(ISF.EQ.8)   READ(26,FMT=*,END=771)IX,IY,GD008(IX,IY,1)
                      IF(ISF.EQ.9)   READ(26,FMT=*,END=771)IX,IY,GD009(IX,IY,1)
                      IF(ISF.EQ.10)  READ(26,FMT=*,END=771)IX,IY,GD010(IX,IY,1)
                      IF(ISF.EQ.11)  READ(26,FMT=*,END=771)IX,IY,GD011(IX,IY,1)
                      IF(ISF.EQ.12)  READ(26,FMT=*,END=771)IX,IY,GD012(IX,IY,1)
                      IF(ISF.EQ.13)  READ(26,FMT=*,END=771)IX,IY,GD013(IX,IY,1)
                      IF(ISF.EQ.14)  READ(26,FMT=*,END=771)IX,IY,GD014(IX,IY,1)
                      IF(ISF.EQ.15)  READ(26,FMT=*,END=771)IX,IY,GD015(IX,IY,1)
                      IF(ISF.EQ.16)  READ(26,FMT=*,END=771)IX,IY,GD016(IX,IY,1)
                      IF(ISF.EQ.17)  READ(26,FMT=*,END=771)IX,IY,GD017(IX,IY,1)
                      IF(ISF.EQ.18)  READ(26,FMT=*,END=771)IX,IY,GD018(IX,IY,1)
                      IF(ISF.EQ.19)  READ(26,FMT=*,END=771)IX,IY,GD019(IX,IY,1)
                      IF(ISF.EQ.20)  READ(26,FMT=*,END=771)IX,IY,GD020(IX,IY,1)
                      IF(ISF.EQ.21)  READ(26,FMT=*,END=771)IX,IY,GD021(IX,IY,1)
                      IF(ISF.EQ.22)  READ(26,FMT=*,END=771)IX,IY,GD022(IX,IY,1)
                      IF(ISF.EQ.23)  READ(26,FMT=*,END=771)IX,IY,GD023(IX,IY,1)
                      IF(ISF.EQ.24)  READ(26,FMT=*,END=771)IX,IY,GD024(IX,IY,1)
                      IF(ISF.EQ.25)  READ(26,FMT=*,END=771)IX,IY,GD025(IX,IY,1)
                      IF(ISF.EQ.26)  READ(26,FMT=*,END=771)IX,IY,GD026(IX,IY,1)
                      IF(ISF.EQ.27)  READ(26,FMT=*,END=771)IX,IY,GD027(IX,IY,1)
                      IF(ISF.EQ.28)  READ(26,FMT=*,END=771)IX,IY,GD028(IX,IY,1)
                      IF(ISF.EQ.29)  READ(26,FMT=*,END=771)IX,IY,GD029(IX,IY,1)
                      IF(ISF.EQ.30)  READ(26,FMT=*,END=771)IX,IY,GD030(IX,IY,1)
                      IF(ISF.EQ.31)  READ(26,FMT=*,END=771)IX,IY,GD031(IX,IY,1)
                      IF(ISF.EQ.32)  READ(26,FMT=*,END=771)IX,IY,GD032(IX,IY,1)
                      IF(ISF.EQ.33)  READ(26,FMT=*,END=771)IX,IY,GD033(IX,IY,1)
                      IF(ISF.EQ.34)  READ(26,FMT=*,END=771)IX,IY,GD034(IX,IY,1)
                      IF(ISF.EQ.35)  READ(26,FMT=*,END=771)IX,IY,GD035(IX,IY,1)
                      IF(ISF.EQ.36)  READ(26,FMT=*,END=771)IX,IY,GD036(IX,IY,1)
                      IF(ISF.EQ.37)  READ(26,FMT=*,END=771)IX,IY,GD037(IX,IY,1)
                      IF(ISF.EQ.38)  READ(26,FMT=*,END=771)IX,IY,GD038(IX,IY,1)
                      IF(ISF.EQ.39)  READ(26,FMT=*,END=771)IX,IY,GD039(IX,IY,1)
                      IF(ISF.EQ.40)  READ(26,FMT=*,END=771)IX,IY,GD040(IX,IY,1)
                      IF(ISF.EQ.41)  READ(26,FMT=*,END=771)IX,IY,GD041(IX,IY,1)
                      IF(ISF.EQ.42)  READ(26,FMT=*,END=771)IX,IY,GD042(IX,IY,1)
                      IF(ISF.EQ.43)  READ(26,FMT=*,END=771)IX,IY,GD043(IX,IY,1)
                      IF(ISF.EQ.44)  READ(26,FMT=*,END=771)IX,IY,GD044(IX,IY,1)
                      IF(ISF.EQ.45)  READ(26,FMT=*,END=771)IX,IY,GD045(IX,IY,1)
                      IF(ISF.EQ.46)  READ(26,FMT=*,END=771)IX,IY,GD046(IX,IY,1)
                      IF(ISF.EQ.47)  READ(26,FMT=*,END=771)IX,IY,GD047(IX,IY,1)
                      IF(ISF.EQ.48)  READ(26,FMT=*,END=771)IX,IY,GD048(IX,IY,1)
                      IF(ISF.EQ.49)  READ(26,FMT=*,END=771)IX,IY,GD049(IX,IY,1)
                      IF(ISF.EQ.50)  READ(26,FMT=*,END=771)IX,IY,GD050(IX,IY,1)
                  END DO
 771              CONTINUE
                  IF(ICODE.EQ.3) THEN
                      ISF=INT(FTFL01(1,ISURF))
                      DO IX=1,M1
                          DO IY=1,M1
C     CALC X AND Y DERIVATIVES
                              IF(ISF.EQ.1)   CALL GD3(GD001,M1,N1)
                              IF(ISF.EQ.2)   CALL GD3(GD002,M1,N1)
                              IF(ISF.EQ.3)   CALL GD3(GD003,M1,N1)
                              IF(ISF.EQ.4)   CALL GD3(GD004,M1,N1)
                              IF(ISF.EQ.5)   CALL GD3(GD005,M1,N1)
                              IF(ISF.EQ.6)   CALL GD3(GD006,M1,N1)
                              IF(ISF.EQ.7)   CALL GD3(GD007,M1,N1)
                              IF(ISF.EQ.8)   CALL GD3(GD008,M1,N1)
                              IF(ISF.EQ.9)   CALL GD3(GD009,M1,N1)
                              IF(ISF.EQ.10)  CALL GD3(GD010,M1,N1)
                              IF(ISF.EQ.11)  CALL GD3(GD011,M1,N1)
                              IF(ISF.EQ.12)  CALL GD3(GD012,M1,N1)
                              IF(ISF.EQ.13)  CALL GD3(GD013,M1,N1)
                              IF(ISF.EQ.14)  CALL GD3(GD014,M1,N1)
                              IF(ISF.EQ.15)  CALL GD3(GD015,M1,N1)
                              IF(ISF.EQ.16)  CALL GD3(GD016,M1,N1)
                              IF(ISF.EQ.17)  CALL GD3(GD017,M1,N1)
                              IF(ISF.EQ.18)  CALL GD3(GD018,M1,N1)
                              IF(ISF.EQ.19)  CALL GD3(GD019,M1,N1)
                              IF(ISF.EQ.20)  CALL GD3(GD020,M1,N1)
                              IF(ISF.EQ.21)  CALL GD3(GD021,M1,N1)
                              IF(ISF.EQ.22)  CALL GD3(GD022,M1,N1)
                              IF(ISF.EQ.23)  CALL GD3(GD023,M1,N1)
                              IF(ISF.EQ.24)  CALL GD3(GD024,M1,N1)
                              IF(ISF.EQ.25)  CALL GD3(GD025,M1,N1)
                              IF(ISF.EQ.26)  CALL GD3(GD026,M1,N1)
                              IF(ISF.EQ.27)  CALL GD3(GD027,M1,N1)
                              IF(ISF.EQ.28)  CALL GD3(GD028,M1,N1)
                              IF(ISF.EQ.29)  CALL GD3(GD029,M1,N1)
                              IF(ISF.EQ.30)  CALL GD3(GD030,M1,N1)
                              IF(ISF.EQ.31)  CALL GD3(GD031,M1,N1)
                              IF(ISF.EQ.32)  CALL GD3(GD032,M1,N1)
                              IF(ISF.EQ.33)  CALL GD3(GD033,M1,N1)
                              IF(ISF.EQ.34)  CALL GD3(GD034,M1,N1)
                              IF(ISF.EQ.35)  CALL GD3(GD035,M1,N1)
                              IF(ISF.EQ.36)  CALL GD3(GD036,M1,N1)
                              IF(ISF.EQ.37)  CALL GD3(GD037,M1,N1)
                              IF(ISF.EQ.38)  CALL GD3(GD038,M1,N1)
                              IF(ISF.EQ.39)  CALL GD3(GD039,M1,N1)
                              IF(ISF.EQ.40)  CALL GD3(GD040,M1,N1)
                              IF(ISF.EQ.41)  CALL GD3(GD041,M1,N1)
                              IF(ISF.EQ.42)  CALL GD3(GD042,M1,N1)
                              IF(ISF.EQ.43)  CALL GD3(GD043,M1,N1)
                              IF(ISF.EQ.44)  CALL GD3(GD044,M1,N1)
                              IF(ISF.EQ.45)  CALL GD3(GD045,M1,N1)
                              IF(ISF.EQ.46)  CALL GD3(GD046,M1,N1)
                              IF(ISF.EQ.47)  CALL GD3(GD047,M1,N1)
                              IF(ISF.EQ.48)  CALL GD3(GD048,M1,N1)
                              IF(ISF.EQ.49)  CALL GD3(GD049,M1,N1)
                              IF(ISF.EQ.50)  CALL GD3(GD050,M1,N1)
                          END DO
                      END DO
                  END IF
C     SET LOADED FLAG
                  ALENS(88,ISURF)=1.0D0
                  CALL CLOSE_FILE(26,1)
              ELSE
C     IT HAS ALREADY BEEN ALLOCATED AND LOADED, NOTHING TO DO RIGHT HERE
              END IF
          END IF
          IF(ICODE.EQ.2.OR.ICODE.EQ.3.OR.ICODE.EQ.4.OR.ICODE.EQ.5) THEN
C     CALCULATE THE VALUE1, APPLY IT TO RAYRAY(25,ISURF)
C     AND RETURN
C     CALCULATING VALUE1
C     THE CURRENT RAY COORDINATES AT THE SURFACE ARE
              IF(ICODE.EQ.2) THEN
                  XR=RAYRAY(1,ISURF)
                  YR=RAYRAY(2,ISURF)
              ELSE
                  XR=XPASS
                  YR=YPASS
              END IF
C     ASSIGN THE REFHIT VALUE1
              IF(FTFL01(3,ISURF).EQ.-99.0D0) REFHIT=ALENS(76,ISURF)
              IF(FTFL01(3,ISURF).NE.-99.0D0) REFHIT=DABS(FTFL01(3,ISURF))
C     ORIGIN ADJUSTED RAY COORDINATES ARE:
C     SIDE DIMENSIONS OF A SINGLE SQUARE is:
              SUBHIT=(2.0D0*REFHIT)/(DABS(FTFL01(2,ISURF))-1.0D0)
              ENDIT=INT(DABS(FTFL01(2,ISURF)))-1
              DO II=1,ENDIT
                  X1=(DABS(DBLE(II-1))*SUBHIT)-REFHIT
                  X2=(DABS(DBLE(II))*SUBHIT)-REFHIT
                  IF(XR.GE.X1.AND.XR.LT.X2.AND.II.LT.ENDIT) THEN
                      IX1=II
                      IX2=II+1
                      FX=(XR-X1)/(X2-X1)
                      GO TO 4000
                  END IF
                  IF(XR.GE.X1.AND.XR.LE.X2.AND.II.EQ.ENDIT) THEN
                      IX1=II
                      IX2=II+1
                      FX=(XR-X1)/(X2-X1)
                      GO TO 4000
                  END IF
              END DO
 4000         CONTINUE
              DO II=1,ENDIT
                  Y1=(DABS(DBLE(II-1))*SUBHIT)-REFHIT
                  Y2=(DABS(DBLE(II))*SUBHIT)-REFHIT
                  IF(YR.GE.Y1.AND.YR.LT.Y2.AND.II.LT.ENDIT) THEN
                      IY1=II
                      IY2=II+1
                      FY=(YR-Y1)/(Y2-Y1)
                      GO TO 6000
                  END IF
                  IF(YR.GE.Y1.AND.YR.LE.Y2.AND.II.EQ.ENDIT) THEN
                      IY1=II
                      IY2=II+1
                      FY=(YR-Y1)/(Y2-Y1)
                      GO TO 6000
                  END IF
              END DO
C     IF HERE, THE XR OR YR WAS OUTSIDE RANGE
              AP=0.0D0
              DX=0.0D0
              DY=0.0D0
              GO TO 333
 6000         CONTINUE
C     VALUE1 INSIDE GRID, CONTINUE
C     THE VALUE1S LIE INSIDE THE BOX FORMED BY IX1,IX2, IY1 AND IY2
              ISF=INT(FTFL01(1,ISURF))
              IF(ISF.EQ.001) CALL GD2(GD001,M1,N1,ICODE)
              IF(ISF.EQ.002) CALL GD2(GD002,M1,N1,ICODE)
              IF(ISF.EQ.003) CALL GD2(GD003,M1,N1,ICODE)
              IF(ISF.EQ.004) CALL GD2(GD004,M1,N1,ICODE)
              IF(ISF.EQ.005) CALL GD2(GD005,M1,N1,ICODE)
              IF(ISF.EQ.006) CALL GD2(GD006,M1,N1,ICODE)
              IF(ISF.EQ.007) CALL GD2(GD007,M1,N1,ICODE)
              IF(ISF.EQ.008) CALL GD2(GD008,M1,N1,ICODE)
              IF(ISF.EQ.009) CALL GD2(GD009,M1,N1,ICODE)
              IF(ISF.EQ.010) CALL GD2(GD010,M1,N1,ICODE)
              IF(ISF.EQ.011) CALL GD2(GD011,M1,N1,ICODE)
              IF(ISF.EQ.012) CALL GD2(GD012,M1,N1,ICODE)
              IF(ISF.EQ.013) CALL GD2(GD013,M1,N1,ICODE)
              IF(ISF.EQ.014) CALL GD2(GD014,M1,N1,ICODE)
              IF(ISF.EQ.015) CALL GD2(GD015,M1,N1,ICODE)
              IF(ISF.EQ.016) CALL GD2(GD016,M1,N1,ICODE)
              IF(ISF.EQ.017) CALL GD2(GD017,M1,N1,ICODE)
              IF(ISF.EQ.018) CALL GD2(GD018,M1,N1,ICODE)
              IF(ISF.EQ.019) CALL GD2(GD019,M1,N1,ICODE)
              IF(ISF.EQ.020) CALL GD2(GD020,M1,N1,ICODE)
              IF(ISF.EQ.021) CALL GD2(GD021,M1,N1,ICODE)
              IF(ISF.EQ.022) CALL GD2(GD022,M1,N1,ICODE)
              IF(ISF.EQ.023) CALL GD2(GD023,M1,N1,ICODE)
              IF(ISF.EQ.024) CALL GD2(GD024,M1,N1,ICODE)
              IF(ISF.EQ.025) CALL GD2(GD025,M1,N1,ICODE)
              IF(ISF.EQ.026) CALL GD2(GD026,M1,N1,ICODE)
              IF(ISF.EQ.027) CALL GD2(GD027,M1,N1,ICODE)
              IF(ISF.EQ.028) CALL GD2(GD028,M1,N1,ICODE)
              IF(ISF.EQ.029) CALL GD2(GD029,M1,N1,ICODE)
              IF(ISF.EQ.030) CALL GD2(GD030,M1,N1,ICODE)
              IF(ISF.EQ.031) CALL GD2(GD031,M1,N1,ICODE)
              IF(ISF.EQ.032) CALL GD2(GD032,M1,N1,ICODE)
              IF(ISF.EQ.033) CALL GD2(GD033,M1,N1,ICODE)
              IF(ISF.EQ.034) CALL GD2(GD034,M1,N1,ICODE)
              IF(ISF.EQ.035) CALL GD2(GD035,M1,N1,ICODE)
              IF(ISF.EQ.036) CALL GD2(GD036,M1,N1,ICODE)
              IF(ISF.EQ.037) CALL GD2(GD037,M1,N1,ICODE)
              IF(ISF.EQ.038) CALL GD2(GD038,M1,N1,ICODE)
              IF(ISF.EQ.039) CALL GD2(GD039,M1,N1,ICODE)
              IF(ISF.EQ.040) CALL GD2(GD040,M1,N1,ICODE)
              IF(ISF.EQ.041) CALL GD2(GD041,M1,N1,ICODE)
              IF(ISF.EQ.042) CALL GD2(GD042,M1,N1,ICODE)
              IF(ISF.EQ.043) CALL GD2(GD043,M1,N1,ICODE)
              IF(ISF.EQ.044) CALL GD2(GD044,M1,N1,ICODE)
              IF(ISF.EQ.045) CALL GD2(GD045,M1,N1,ICODE)
              IF(ISF.EQ.046) CALL GD2(GD046,M1,N1,ICODE)
              IF(ISF.EQ.047) CALL GD2(GD047,M1,N1,ICODE)
              IF(ISF.EQ.048) CALL GD2(GD048,M1,N1,ICODE)
              IF(ISF.EQ.049) CALL GD2(GD049,M1,N1,ICODE)
              IF(ISF.EQ.050) CALL GD2(GD050,M1,N1,ICODE)
              AP=0.0D0
              DX=0.0D0
              DY=0.0D0
              AP=((1.0D0-FX)*(1.0D0-FY)*V11)
     1        +((1.0D0-FX)*(FY)*V12)
     1        +((FX)*(1.0D0-FY)*V21)
     1        +((FX)*(FY)*V22)
              IF(ICODE.EQ.3) THEN
                  DX=((1.0D0-FX)*(1.0D0-FY)*DXV11)
     1            +((1.0D0-FX)*(FY)*DXV12)
     1            +((FX)*(1.0D0-FY)*DXV21)
     1            +((FX)*(FY)*DXV22)
                  DY=((1.0D0-FX)*(1.0D0-FY)*DYV11)
     1            +((1.0D0-FX)*(FY)*DYV12)
     1            +((FX)*(1.0D0-FY)*DYV21)
     1            +((FX)*(FY)*DYV22)
                  IF(ICODE.EQ.3) THEN
                      AP=-AP
                      DX=-DX
                      DY=-DY
                  END IF
              END IF
 333          CONTINUE
              IF(ICODE.EQ.2) THEN
                  AP=AP*FTFL01(4,ISURF)
                  RAYRAY(25,ISURF)=RAYRAY(25,ISURF-1)*AP

                  RETURN
              END IF
              IF(ICODE.EQ.3) THEN
C     AP IS THE PHASE, AND DX AND DY ARE THE SLOPES BUT WE MUST FIX UP
C     THE UNITS FIRST.
C
                  IF(INT(FTFL01(4,ISURF)).EQ.0) THEN
C     PHASE IN FRACTION OF REF WAVELENGTH FRACTION
C     FIRST CONVERT TO MICROMETER
                      IF(SYSTEM1(11).LE.5.0D0) THEN
                          AP=AP*SYSTEM1(INT(SYSTEM1(11)))
                          DX=DX*SYSTEM1(INT(SYSTEM1(11)))
                          DY=DY*SYSTEM1(INT(SYSTEM1(11)))
                      ELSE
                          AP=AP*SYSTEM1(INT(SYSTEM1(11))+65)
                          DX=DX*SYSTEM1(INT(SYSTEM1(11))+65)
                          DY=DY*SYSTEM1(INT(SYSTEM1(11))+65)
                      END IF
C     NOW GO FROM MICROMETER TO LENS UNITS
                      IF(SYSTEM1(6).EQ.1.0D0) AP=(AP*1.0D-3)/(25.4D0)
                      IF(SYSTEM1(6).EQ.2.0D0) AP=AP*1.0D-4
                      IF(SYSTEM1(6).EQ.3.0D0) AP=AP*1.0D-3
                      IF(SYSTEM1(6).EQ.4.0D0) AP=AP*1.0D-6
                      IF(SYSTEM1(6).EQ.1.0D0) DX=(DX*1.0D-3)/(25.4D0)
                      IF(SYSTEM1(6).EQ.2.0D0) DX=DX*1.0D-4
                      IF(SYSTEM1(6).EQ.3.0D0) DX=DX*1.0D-3
                      IF(SYSTEM1(6).EQ.4.0D0) DX=DX*1.0D-6
                      IF(SYSTEM1(6).EQ.1.0D0) DY=(DY*1.0D-3)/(25.4D0)
                      IF(SYSTEM1(6).EQ.2.0D0) DY=DY*1.0D-4
                      IF(SYSTEM1(6).EQ.3.0D0) DY=DY*1.0D-3
                      IF(SYSTEM1(6).EQ.4.0D0) DY=DY*1.0D-6
                  END IF
                  IF(INT(FTFL01(4,ISURF)).EQ.1) THEN
C     PHASE IN LENS UNITS, NO CONVERSIONS NEEDED
                  END IF
                  IF(INT(FTFL01(4,ISURF)).EQ.2) THEN
C     PHASE IN MICROMETER, CONVERT TO LENS UNITS
                      IF(SYSTEM1(6).EQ.1.0D0) AP=(AP*1.0D-3)/(25.4D0)
                      IF(SYSTEM1(6).EQ.2.0D0) AP=AP*1.0D-4
                      IF(SYSTEM1(6).EQ.3.0D0) AP=AP*1.0D-3
                      IF(SYSTEM1(6).EQ.4.0D0) AP=AP*1.0D-6
                      IF(SYSTEM1(6).EQ.1.0D0) DX=(DX*1.0D-3)/(25.4D0)
                      IF(SYSTEM1(6).EQ.2.0D0) DX=DX*1.0D-4
                      IF(SYSTEM1(6).EQ.3.0D0) DX=DX*1.0D-3
                      IF(SYSTEM1(6).EQ.4.0D0) DX=DX*1.0D-6
                      IF(SYSTEM1(6).EQ.1.0D0) DY=(DY*1.0D-3)/(25.4D0)
                      IF(SYSTEM1(6).EQ.2.0D0) DY=DY*1.0D-4
                      IF(SYSTEM1(6).EQ.3.0D0) DY=DY*1.0D-3
                      IF(SYSTEM1(6).EQ.4.0D0) DY=DY*1.0D-6
                  END IF
C     FINALLY FORM THE FULL DERIVATIVE
                  DX=DX/SUBHIT
                  DY=DY/SUBHIT
                  AP=AP*FTFL01(5,ISURF)
                  DX=DX*FTFL01(5,ISURF)
                  DY=DY*FTFL01(5,ISURF)
                  LL1=-DX
                  MM1=-DY
                  PHASE=AP
                  RLRL=RLRL+LL1
                  RMRM=RMRM+MM1
                  MAG=DSQRT((DBLE(RLRL**2))+(DBLE(RMRM**2))+(DBLE(RNRN**2)))
                  RLRL=RLRL/MAG
                  RMRM=RMRM/MAG
                  RNRN=RNRN/MAG
                  YRLRL=YRLRL-INT(LL1)
                  YRMRM=YRMRM-INT(MM1)
                  MAG=DSQRT((DBLE(YRLRL**2))+(DBLE(YRMRM**2))+(DBLE(YRNRN**2)))
                  YRLRL=INT(YRLRL/MAG)
                  YRMRM=INT(YRMRM/MAG)
                  YRNRN=INT(YRNRN/MAG)
                  RETURN
              END IF
              IF(ICODE.EQ.4) THEN
C     AP IS THE SAG
C
                  ZPASS=FTFL01(4,ISURF)*AP
                  RETURN
              END IF
              IF(ICODE.EQ.5) THEN
C     AP IS THE SAG
C
                  ZPASS=FTFL01(4,ISURF)*AP
                  RETURN
              END IF
          END IF
          IF(ICODE.EQ.6) THEN
C     CALCULATE THE VALUE1, APPLY IT TO RAYENERGY
C     AND RETURN
C     CALCULATING VALUE1
C     THE CURRENT RAY COORDINATES AT THE SURFACE ARE
              XR=JIMRAY1
              YR=JIMRAY2
C     ASSIGN THE REFHIT VALUE1
              IF(FTFL01(3,ISURF).EQ.-99.0D0) REFHIT=ALENS(76,ISURF)
              IF(FTFL01(3,ISURF).NE.-99.0D0) REFHIT=DABS(FTFL01(3,ISURF))
C     ORIGIN ADJUSTED RAY COORDINATES ARE:
C     SIDE DIMENSIONS OF A SINGLE SQUARE is:
              SUBHIT=(2.0D0*REFHIT)/(DABS(FTFL01(2,ISURF))-1.0D0)
              ENDIT=INT(DABS(FTFL01(2,ISURF)))-1
              DO II=1,ENDIT
                  X1=(DABS(DBLE(II-1))*SUBHIT)-REFHIT
                  X2=(DABS(DBLE(II))*SUBHIT)-REFHIT
                  IF(XR.GE.X1.AND.XR.LT.X2.AND.II.LT.ENDIT) THEN
                      IX1=II
                      IX2=II+1
                      FX=(XR-X1)/(X2-X1)
                      GO TO 4001
                  END IF
                  IF(XR.GE.X1.AND.XR.LE.X2.AND.II.EQ.ENDIT) THEN
                      IX1=II
                      IX2=II+1
                      FX=(XR-X1)/(X2-X1)
                      GO TO 4001
                  END IF
              END DO
 4001         CONTINUE
              DO II=1,ENDIT
                  Y1=(DABS(DBLE(II-1))*SUBHIT)-REFHIT
                  Y2=(DABS(DBLE(II))*SUBHIT)-REFHIT
                  IF(YR.GE.Y1.AND.YR.LT.Y2.AND.II.LT.ENDIT) THEN
                      IY1=II
                      IY2=II+1
                      FY=(YR-Y1)/(Y2-Y1)
                      GO TO 6001
                  END IF
                  IF(YR.GE.Y1.AND.YR.LE.Y2.AND.II.EQ.ENDIT) THEN
                      IY1=II
                      IY2=II+1
                      FY=(YR-Y1)/(Y2-Y1)
                      GO TO 6001
                  END IF
              END DO
C     IF HERE, THE XR OR YR WAS OUTSIDE RANGE
              AP=0.0D0
              DX=0.0D0
              DY=0.0D0
              GO TO 334
 6001         CONTINUE
C     VALUE1 INSIDE GRID, CONTINUE
C     THE VALUE1S LIE INSIDE THE BOX FORMED BY IX1,IX2, IY1 AND IY2
              ISF=INT(FTFL01(1,ISURF))
              IF(ISF.EQ.001) CALL GD2(GD001,M1,N1,ICODE)
              IF(ISF.EQ.002) CALL GD2(GD002,M1,N1,ICODE)
              IF(ISF.EQ.003) CALL GD2(GD003,M1,N1,ICODE)
              IF(ISF.EQ.004) CALL GD2(GD004,M1,N1,ICODE)
              IF(ISF.EQ.005) CALL GD2(GD005,M1,N1,ICODE)
              IF(ISF.EQ.006) CALL GD2(GD006,M1,N1,ICODE)
              IF(ISF.EQ.007) CALL GD2(GD007,M1,N1,ICODE)
              IF(ISF.EQ.008) CALL GD2(GD008,M1,N1,ICODE)
              IF(ISF.EQ.009) CALL GD2(GD009,M1,N1,ICODE)
              IF(ISF.EQ.010) CALL GD2(GD010,M1,N1,ICODE)
              IF(ISF.EQ.011) CALL GD2(GD011,M1,N1,ICODE)
              IF(ISF.EQ.012) CALL GD2(GD012,M1,N1,ICODE)
              IF(ISF.EQ.013) CALL GD2(GD013,M1,N1,ICODE)
              IF(ISF.EQ.014) CALL GD2(GD014,M1,N1,ICODE)
              IF(ISF.EQ.015) CALL GD2(GD015,M1,N1,ICODE)
              IF(ISF.EQ.016) CALL GD2(GD016,M1,N1,ICODE)
              IF(ISF.EQ.017) CALL GD2(GD017,M1,N1,ICODE)
              IF(ISF.EQ.018) CALL GD2(GD018,M1,N1,ICODE)
              IF(ISF.EQ.019) CALL GD2(GD019,M1,N1,ICODE)
              IF(ISF.EQ.020) CALL GD2(GD020,M1,N1,ICODE)
              IF(ISF.EQ.021) CALL GD2(GD021,M1,N1,ICODE)
              IF(ISF.EQ.022) CALL GD2(GD022,M1,N1,ICODE)
              IF(ISF.EQ.023) CALL GD2(GD023,M1,N1,ICODE)
              IF(ISF.EQ.024) CALL GD2(GD024,M1,N1,ICODE)
              IF(ISF.EQ.025) CALL GD2(GD025,M1,N1,ICODE)
              IF(ISF.EQ.026) CALL GD2(GD026,M1,N1,ICODE)
              IF(ISF.EQ.027) CALL GD2(GD027,M1,N1,ICODE)
              IF(ISF.EQ.028) CALL GD2(GD028,M1,N1,ICODE)
              IF(ISF.EQ.029) CALL GD2(GD029,M1,N1,ICODE)
              IF(ISF.EQ.030) CALL GD2(GD030,M1,N1,ICODE)
              IF(ISF.EQ.031) CALL GD2(GD031,M1,N1,ICODE)
              IF(ISF.EQ.032) CALL GD2(GD032,M1,N1,ICODE)
              IF(ISF.EQ.033) CALL GD2(GD033,M1,N1,ICODE)
              IF(ISF.EQ.034) CALL GD2(GD034,M1,N1,ICODE)
              IF(ISF.EQ.035) CALL GD2(GD035,M1,N1,ICODE)
              IF(ISF.EQ.036) CALL GD2(GD036,M1,N1,ICODE)
              IF(ISF.EQ.037) CALL GD2(GD037,M1,N1,ICODE)
              IF(ISF.EQ.038) CALL GD2(GD038,M1,N1,ICODE)
              IF(ISF.EQ.039) CALL GD2(GD039,M1,N1,ICODE)
              IF(ISF.EQ.040) CALL GD2(GD040,M1,N1,ICODE)
              IF(ISF.EQ.041) CALL GD2(GD041,M1,N1,ICODE)
              IF(ISF.EQ.042) CALL GD2(GD042,M1,N1,ICODE)
              IF(ISF.EQ.043) CALL GD2(GD043,M1,N1,ICODE)
              IF(ISF.EQ.044) CALL GD2(GD044,M1,N1,ICODE)
              IF(ISF.EQ.045) CALL GD2(GD045,M1,N1,ICODE)
              IF(ISF.EQ.046) CALL GD2(GD046,M1,N1,ICODE)
              IF(ISF.EQ.047) CALL GD2(GD047,M1,N1,ICODE)
              IF(ISF.EQ.048) CALL GD2(GD048,M1,N1,ICODE)
              IF(ISF.EQ.049) CALL GD2(GD049,M1,N1,ICODE)
              IF(ISF.EQ.050) CALL GD2(GD050,M1,N1,ICODE)
              AP=0.0D0
              DX=0.0D0
              DY=0.0D0
              AP=((1.0D0-FX)*(1.0D0-FY)*V11)
     1        +((1.0D0-FX)*(FY)*V12)
     1        +((FX)*(1.0D0-FY)*V21)
     1        +((FX)*(FY)*V22)
              IF(ICODE.EQ.3) THEN
                  DX=((1.0D0-FX)*(1.0D0-FY)*DXV11)
     1            +((1.0D0-FX)*(FY)*DXV12)
     1            +((FX)*(1.0D0-FY)*DXV21)
     1            +((FX)*(FY)*DXV22)
                  DY=((1.0D0-FX)*(1.0D0-FY)*DYV11)
     1            +((1.0D0-FX)*(FY)*DYV12)
     1            +((FX)*(1.0D0-FY)*DYV21)
     1            +((FX)*(FY)*DYV22)
              END IF
              IF(ICODE.EQ.3) THEN
                  AP=-AP
                  DX=-DX
                  DY=-DY
              END IF
 334          CONTINUE
              IF(ICODE.EQ.6) THEN
                  AP=AP*FTFL01(4,ISURF)
                  IF(AP.GE.0.5D0) AP=1.0D0
                  IF(AP.LT.0.5D0) AP=0.0D0
                  RAYENERGY=RAYENERGY*AP
                  RETURN
              END IF
          END IF
          RETURN
      END
      SUBROUTINE GD2(GRIDER,M1,N1,ICODE)
          IMPLICIT NONE
          REAL*8 V11,V12,V22,V21,GRIDER,DXV11,DXV12,DXV21,DXV22
     1    ,DYV11,DYV12,DYV21,DYV22
          INTEGER M1,N1,IX1,IX2,IY1,IY2,ICODE
          COMMON/GD2GD2/IX1,IX2,IY1,IY2,DXV11,DXV12,DXV21,DXV22
     1    ,DYV11,DYV12,DYV21,DYV22,V11,V12,V21,V22
          DIMENSION GRIDER(M1,M1,N1)
          INCLUDE 'datmai.inc'
          V11=GRIDER(IX1,IY1,1)
          V12=GRIDER(IX1,IY2,1)
          V21=GRIDER(IX2,IY1,1)
          V22=GRIDER(IX2,IY2,1)
          IF(ICODE.EQ.3) THEN
              DXV11=GRIDER(IX1,IY1,2)
              DXV12=GRIDER(IX1,IY2,2)
              DXV21=GRIDER(IX2,IY1,2)
              DXV22=GRIDER(IX2,IY2,2)
              DYV11=GRIDER(IX1,IY1,3)
              DYV12=GRIDER(IX1,IY2,3)
              DYV21=GRIDER(IX2,IY1,3)
              DYV22=GRIDER(IX2,IY2,3)
          END IF
          RETURN
      END
      SUBROUTINE GD3(GRIDER,M1,N1)
          IMPLICIT NONE
          INTEGER M1,N1,IX,IY
          REAL*8 GRIDER
          COMMON/GD3GD3/IX,IY
          DIMENSION GRIDER(M1,M1,N1)
          IF(IX.GT.1.AND.IX.LT.M1) THEN
              GRIDER(IX,IY,2)=
     1        ((GRIDER(IX-1,IY,1)
     1        -GRIDER(IX,IY,1))+
     1        (GRIDER(IX,IY,1)
     1        -GRIDER(IX+1,IY,1)))/2.0D0
          ELSE
              IF(IX.EQ.1) THEN
                  GRIDER(IX,IY,2)=
     1            (GRIDER(IX,IY,1)
     1            -GRIDER(IX+1,IY,1))
              END IF
              IF(IX.EQ.M1) THEN
                  GRIDER(IX,IY,2)=
     1            ((GRIDER(IX-1,IY,1)
     1            -GRIDER(IX,IY,1)))
              END IF
          END IF
          IF(IY.GT.1.AND.IY.LT.M1) THEN
              GRIDER(IX,IY,3)=
     1        ((GRIDER(IX,IY-1,1)
     1        -GRIDER(IX,IY,1))+
     1        ( GRIDER(IX,IY,1)
     1        - GRIDER(IX,IY+1,1)))/2.0D0
          ELSE
              IF(IY.EQ.1) THEN
                  GRIDER(IX,IY,3)=
     1            (GRIDER(IX,IY,1)
     1            -GRIDER(IX,IY+1,1))
              END IF
              IF(IY.EQ.M1) THEN
                  GRIDER(IX,IY,3)=
     1            ((GRIDER(IX,IY-1,1)
     1            -GRIDER(IX,IY,1)))
              END IF
          END IF
          RETURN
      END
C      THIS IS THE NINTH FILE OF RAYTRACING ROUTINES

      SUBROUTINE FIXDEFORMFILE
          IMPLICIT NONE
          INTEGER I
          LOGICAL ERR1,ERR2
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          ERR1=.FALSE.
          ERR2=.FALSE.
          DO I=0,INT(SYSTEM1(20))
              IF(ALENS(103,I).EQ.1.0D0) CALL DEFGRIDS(9,I,ERR1,ERR2)
          END DO
          RETURN
      END
C SUB DEFGRIDS.FOR

      SUBROUTINE DEFGRIDS(ICODE,ISURF,ERROR1,ERROR2)
C
          IMPLICIT NONE
C
C     PRIMARY GRID SPSRF SUBROUTINE
C
          INTEGER NUM5
C
          REAL*8 NEWDEFVAL,VALUE1
C
          COMMON/GV/VALUE1,NUM5
C
          COMMON/DEFVALCOM/NEWDEFVAL
C
          REAL*8 BA,AP,FUNC,XR,YR,HT108,X,Y
C
          REAL*8 XPASS,YPASS,ZPASS,V11,V12,V21,V22,DXV11,DXV12
     1    ,DXV21,DXV22,DYV11,DYV12,DYV21,DYV22,DEFVAL
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          INTEGER I,ICODE,ALLOERR,ISURF,IX1,IX2,IY1,IY2
     1    ,N1,I1,I2,I3,I4,I5,I6,I7,I8,MM,IFILE
     2    ,MACT
C
          COMMON/GD1GD1/I1,I2,I3,I4,I5,I6,I7,I8
C
          COMMON/GD2GD2/IX1,IX2,IY1,IY2,DXV11,DXV12,DXV21,DXV22
     1    ,DYV11,DYV12,DYV21,DYV22,V11,V12,V21,V22
C
          CHARACTER FLNAME*12
C
          CHARACTER WQLOCAL*8
C
          REAL*8 W2LOCAL
C
          COMMON/LOCALWQ/WQLOCAL,W2LOCAL
C
          LOGICAL ERROR1,ERROR2,EXIS26,OPEN26
C
          REAL*8 JIMRAY1,JIMRAY2,RAYENERGY
          COMMON/FOOTGRID/JIMRAY1,JIMRAY2,RAYENERGY
C
          INTEGER VBSURF
C
          COMMON/SURFVB/VBSURF
C
          REAL*8 AVSPACE,MAXX,MINX,MAXY,MINY
C
          REAL*8 DEFORM01
          REAL*8 DEFORM02
          REAL*8 DEFORM03
          REAL*8 DEFORM04
          REAL*8 DEFORM05
          REAL*8 DEFORM06
          REAL*8 DEFORM07
          REAL*8 DEFORM08
          REAL*8 DEFORM09
          REAL*8 DEFORM10

          DIMENSION DEFORM01(:,:)
          DIMENSION DEFORM02(:,:)
          DIMENSION DEFORM03(:,:)
          DIMENSION DEFORM04(:,:)
          DIMENSION DEFORM05(:,:)
          DIMENSION DEFORM06(:,:)
          DIMENSION DEFORM07(:,:)
          DIMENSION DEFORM08(:,:)
          DIMENSION DEFORM09(:,:)
          DIMENSION DEFORM10(:,:)
          ALLOCATABLE :: DEFORM01
          ALLOCATABLE :: DEFORM02
          ALLOCATABLE :: DEFORM03
          ALLOCATABLE :: DEFORM04
          ALLOCATABLE :: DEFORM05
          ALLOCATABLE :: DEFORM06
          ALLOCATABLE :: DEFORM07
          ALLOCATABLE :: DEFORM08
          ALLOCATABLE :: DEFORM09
          ALLOCATABLE :: DEFORM10
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
          SAVE
C
          IFILE=INT(ALENS(104,ISURF))
          DEFGR1=ALENS(103,ISURF)
          DEFGR2=ALENS(104,ISURF)
          DEFGR3=ALENS(105,ISURF)
          DEFGR4=ALENS(106,ISURF)
          DEFGR5=0.0D0
          DEFGR6=0.0D0
          DEFGR7=ALENS(109,ISURF)
          DEFGR8=0.0D0
C
C     IF ICODE = 1, JUST DEALLOCATE THE DEFGRID ARRAYS. THIS HAPPENS
C     WHENEVER SPFIT OR LENNS IS CALLED. THIS FREES MEMORY
C     CALLED ALSO FROM DEFIT.FOR
C
C     IF ICODE = 2, USED IN ASSIGNING VARIABLE CURRENT VALUE1S
C
C     IF ICODE = 3, USED FOR SAVING THE ARRAYS TO FILES
C
C     IF ICODE = 4, SAG ADJUSTMENT
C     IF ALENS(109,ISURF).EQ.0.0D0 AND THEN CALCULATE AND APPLY THE
C     SAG AND ADJUST SURFACE NORMALS AND THEN RETURN

C     IF ICODE = 5, USED FOR SAG CALCULATIONS
C
C     IF ICODE = 6, USED FOR OPTIMIZATION
C
C     IF ICODE = 7, GETTING A SINGLE ACTUATOR VALUE1
C
C     IF ICODE = 9, WRITES THE CURRENT ARRAY TO THE CORRECT FILE
C     VALUE1S
C
          N1=1
          MACT=INT(ALENS(105,ISURF))
C
          IF(ICODE.EQ.1) THEN
              DEALLOCATE(DEFORM01,STAT=ALLOERR)
              DEALLOCATE(DEFORM02,STAT=ALLOERR)
              DEALLOCATE(DEFORM03,STAT=ALLOERR)
              DEALLOCATE(DEFORM04,STAT=ALLOERR)
              DEALLOCATE(DEFORM05,STAT=ALLOERR)
              DEALLOCATE(DEFORM06,STAT=ALLOERR)
              DEALLOCATE(DEFORM07,STAT=ALLOERR)
              DEALLOCATE(DEFORM08,STAT=ALLOERR)
              DEALLOCATE(DEFORM09,STAT=ALLOERR)
              DEALLOCATE(DEFORM10,STAT=ALLOERR)
              ERROR1=.FALSE.
              RETURN
          END IF
          IF(ICODE.EQ.2) THEN
              ISURF=VBSURF
              IFILE=INT(DEFGR2)
          END IF
          IF(ICODE.EQ.2.OR.ICODE.EQ.4.OR.ICODE.EQ.5) THEN
              IF(ALENS(109,ISURF).EQ.0.0D0) THEN
C     ALLOCATE THE GRID ARRAY FOR THIS SURFACE AND LOAD IT
C     DOES THE FILE EXIST AND IS IT OF THE CORRECT SIZE?
C     SET THE FILE NAME
                  IF(IFILE.EQ.1.0D0)    FLNAME='DEFORM01.DAT'
                  IF(IFILE.EQ.2.0D0)    FLNAME='DEFORM02.DAT'
                  IF(IFILE.EQ.3.0D0)    FLNAME='DEFORM03.DAT'
                  IF(IFILE.EQ.4.0D0)    FLNAME='DEFORM04.DAT'
                  IF(IFILE.EQ.5.0D0)    FLNAME='DEFORM05.DAT'
                  IF(IFILE.EQ.6.0D0)    FLNAME='DEFORM06.DAT'
                  IF(IFILE.EQ.7.0D0)    FLNAME='DEFORM07.DAT'
                  IF(IFILE.EQ.8.0D0)    FLNAME='DEFORM08.DAT'
                  IF(IFILE.EQ.9.0D0)    FLNAME='DEFORM09.DAT'
                  IF(IFILE.EQ.10.0D0)   FLNAME='DEFORM10.DAT'
                  EXIS26=.FALSE.
                  INQUIRE(FILE=trim(HOME)//FLNAME,EXIST=EXIS26)
                  IF(.NOT.EXIS26) THEN
                      ERROR1=.TRUE.
                      ERROR2=.TRUE.
                      WRITE(OUTLYNE,*) 'DEFORMABLE SURFACE FILE ',FLNAME
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'DOES NOT EXIST'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  WRITE(OUTLYNE,*)'FOR SURFACE # ',ISURF
                  CALL SHOWIT(1)
                  OUTLYNE='LOADING DEFORMABLE SURFACE DATA FROM DISK...'
                  CALL SHOWIT(1)
C     ALLOCATE THE GRID ARRAY FOR THIS SURFACE AND LOAD IT
C     DOES THE FILE EXIST AND IS IT OF THE CORRECT SIZE?
C     SET THE FILE NAME
C
                  OPEN(UNIT=26,FILE=trim(HOME)//FLNAME,
     1            STATUS='UNKNOWN')
C
                  MAXX=-1.0D300
                  MINX=1.0D300
                  MAXY=-1.0D300
                  MINY=1.0D300
                  DO I=1,MACT
                      READ(UNIT=26,FMT=*,END=778,ERR=6666) X,Y,FUNC
                      IF(X.LT.MINX) MINX=X
                      IF(X.GT.MAXX) MAXX=X
                      IF(Y.LT.MINY) MINY=Y
                      IF(Y.GT.MAXY) MAXY=Y
                  END DO
                  AVSPACE=DABS(MAXX-MINX)/(DSQRT(DBLE(MACT))-1.0D0)
     1            +DABS(MAXY-MINY)/(DSQRT(DBLE(MACT))-1.0D0)
                  AVSPACE=AVSPACE/2.0D0
                  GO TO 777
 778              CONTINUE
 6666             ERROR1=.TRUE.
                  RETURN
 777              CONTINUE
                  IF(ERROR1) THEN
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL CLOSE_FILE(26,1)
                  OPEN(UNIT=26,FILE=trim(HOME)//FLNAME,
     1            STATUS='UNKNOWN')
C     IF HERE, THEN ALLOCATE THE ARRAY AND READ THE DATA AGAIN
                  IF(IFILE.EQ.1)   ALLOCATE(DEFORM01(MACT,1:3),STAT=ALLOERR)
                  IF(IFILE.EQ.2)   ALLOCATE(DEFORM02(MACT,1:3),STAT=ALLOERR)
                  IF(IFILE.EQ.3)   ALLOCATE(DEFORM03(MACT,1:3),STAT=ALLOERR)
                  IF(IFILE.EQ.4)   ALLOCATE(DEFORM04(MACT,1:3),STAT=ALLOERR)
                  IF(IFILE.EQ.5)   ALLOCATE(DEFORM05(MACT,1:3),STAT=ALLOERR)
                  IF(IFILE.EQ.6)   ALLOCATE(DEFORM06(MACT,1:3),STAT=ALLOERR)
                  IF(IFILE.EQ.7)   ALLOCATE(DEFORM07(MACT,1:3),STAT=ALLOERR)
                  IF(IFILE.EQ.8)   ALLOCATE(DEFORM08(MACT,1:3),STAT=ALLOERR)
                  IF(IFILE.EQ.9)   ALLOCATE(DEFORM09(MACT,1:3),STAT=ALLOERR)
                  IF(IFILE.EQ.10)  ALLOCATE(DEFORM10(MACT,1:3),STAT=ALLOERR)
C     INITIALIZE TO 0.0D0
                  IF(ICODE.EQ.4) BA=0.0D0
                  IF(ICODE.EQ.5) BA=0.0D0
                  IF(IFILE.EQ.1)   DEFORM01(1:MACT,1:3)=BA
                  IF(IFILE.EQ.2)   DEFORM02(1:MACT,1:3)=BA
                  IF(IFILE.EQ.3)   DEFORM03(1:MACT,1:3)=BA
                  IF(IFILE.EQ.4)   DEFORM04(1:MACT,1:3)=BA
                  IF(IFILE.EQ.5)   DEFORM05(1:MACT,1:3)=BA
                  IF(IFILE.EQ.6)   DEFORM06(1:MACT,1:3)=BA
                  IF(IFILE.EQ.7)   DEFORM07(1:MACT,1:3)=BA
                  IF(IFILE.EQ.8)   DEFORM08(1:MACT,1:3)=BA
                  IF(IFILE.EQ.9)   DEFORM09(1:MACT,1:3)=BA
                  IF(IFILE.EQ.10)  DEFORM10(1:MACT,1:3)=BA
C     FILL ARRAY
                  MAXX=-1.0D300
                  MINX=1.0D300
                  MAXY=-1.0D300
                  MINY=1.0D300
                  DO I=1,MACT
                      READ(26,FMT=*,END=771)X,Y,DEFVAL
                      IF(IFILE.EQ.1) DEFORM01(I,1)=X
                      IF(IFILE.EQ.1) DEFORM01(I,2)=Y
                      IF(IFILE.EQ.1) DEFORM01(I,3)=DEFVAL
                      IF(IFILE.EQ.2) DEFORM02(I,1)=X
                      IF(IFILE.EQ.2) DEFORM02(I,2)=Y
                      IF(IFILE.EQ.2) DEFORM02(I,3)=DEFVAL
                      IF(IFILE.EQ.3) DEFORM03(I,1)=X
                      IF(IFILE.EQ.3) DEFORM03(I,2)=Y
                      IF(IFILE.EQ.3) DEFORM03(I,3)=DEFVAL
                      IF(IFILE.EQ.4) DEFORM04(I,1)=X
                      IF(IFILE.EQ.4) DEFORM04(I,2)=Y
                      IF(IFILE.EQ.4) DEFORM04(I,3)=DEFVAL
                      IF(IFILE.EQ.5) DEFORM05(I,1)=X
                      IF(IFILE.EQ.5) DEFORM05(I,2)=Y
                      IF(IFILE.EQ.5) DEFORM05(I,3)=DEFVAL
                      IF(IFILE.EQ.6) DEFORM06(I,1)=X
                      IF(IFILE.EQ.6) DEFORM06(I,2)=Y
                      IF(IFILE.EQ.6) DEFORM06(I,3)=DEFVAL
                      IF(IFILE.EQ.7) DEFORM07(I,1)=X
                      IF(IFILE.EQ.7) DEFORM07(I,2)=Y
                      IF(IFILE.EQ.7) DEFORM07(I,3)=DEFVAL
                      IF(IFILE.EQ.8) DEFORM08(I,1)=X
                      IF(IFILE.EQ.8) DEFORM08(I,2)=Y
                      IF(IFILE.EQ.8) DEFORM08(I,3)=DEFVAL
                      IF(IFILE.EQ.9) DEFORM09(I,1)=X
                      IF(IFILE.EQ.9) DEFORM09(I,2)=Y
                      IF(IFILE.EQ.9) DEFORM09(I,3)=DEFVAL
                      IF(IFILE.EQ.10) DEFORM10(I,1)=X
                      IF(IFILE.EQ.10) DEFORM10(I,2)=Y
                      IF(IFILE.EQ.10) DEFORM10(I,3)=DEFVAL
                      IF(X.LT.MINX) MINX=X
                      IF(X.GT.MAXX) MAXX=X
                      IF(Y.LT.MINY) MINY=Y
                      IF(Y.GT.MAXY) MAXY=Y
                  END DO
 771              CONTINUE
                  AVSPACE=DABS(MAXX-MINX)/(DSQRT(DBLE(MACT))-1.0D0)
     1            +DABS(MAXY-MINY)/(DSQRT(DBLE(MACT))-1.0D0)
                  AVSPACE=AVSPACE/2.0D0
C     SET LOADED FLAG
                  ALENS(109,ISURF)=1.0D0
                  CALL CLOSE_FILE(26,1)
              ELSE
C     IT HAS ALREADY BEEN ALLOCATED AND LOADED, NOTHING TO DO RIGHT HERE
              END IF
          END IF
          IF(ICODE.EQ.2) THEN
C     THE ACTUATOR NUMBER IS ACTNUM PASSED IN THE DEFGRS COMMON BLOCK
C
              IF(INT(DEFGR2).EQ.1) THEN
C     USE THE DEFORM01 ARRAY
                  VARABL(VBCNT+1,4)= DEFORM01(ACTNUM,3)
                  VARABL(VBCNT+1,5)= DEFORM01(ACTNUM,3)
                  VARABL(VBCNT+1,13)=DEFORM01(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.2) THEN
C     USE THE DEFORM01 ARRAY
                  VARABL(VBCNT+1,4)= DEFORM02(ACTNUM,3)
                  VARABL(VBCNT+1,5)= DEFORM02(ACTNUM,3)
                  VARABL(VBCNT+1,13)=DEFORM02(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.3) THEN
C     USE THE DEFORM01 ARRAY
                  VARABL(VBCNT+1,4)= DEFORM03(ACTNUM,3)
                  VARABL(VBCNT+1,5)= DEFORM03(ACTNUM,3)
                  VARABL(VBCNT+1,13)=DEFORM03(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.4) THEN
C     USE THE DEFORM01 ARRAY
                  VARABL(VBCNT+1,4)= DEFORM04(ACTNUM,3)
                  VARABL(VBCNT+1,5)= DEFORM04(ACTNUM,3)
                  VARABL(VBCNT+1,13)=DEFORM04(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.5) THEN
C     USE THE DEFORM01 ARRAY
                  VARABL(VBCNT+1,4)= DEFORM05(ACTNUM,3)
                  VARABL(VBCNT+1,5)= DEFORM05(ACTNUM,3)
                  VARABL(VBCNT+1,13)=DEFORM05(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.6) THEN
C     USE THE DEFORM01 ARRAY
                  VARABL(VBCNT+1,4)= DEFORM06(ACTNUM,3)
                  VARABL(VBCNT+1,5)= DEFORM06(ACTNUM,3)
                  VARABL(VBCNT+1,13)=DEFORM06(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.7) THEN
C     USE THE DEFORM01 ARRAY
                  VARABL(VBCNT+1,4)= DEFORM07(ACTNUM,3)
                  VARABL(VBCNT+1,5)= DEFORM07(ACTNUM,3)
                  VARABL(VBCNT+1,13)=DEFORM07(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.8) THEN
C     USE THE DEFORM01 ARRAY
                  VARABL(VBCNT+1,4)= DEFORM08(ACTNUM,3)
                  VARABL(VBCNT+1,5)= DEFORM08(ACTNUM,3)
                  VARABL(VBCNT+1,13)=DEFORM08(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.9) THEN
C     USE THE DEFORM01 ARRAY
                  VARABL(VBCNT+1,4)= DEFORM09(ACTNUM,3)
                  VARABL(VBCNT+1,5)= DEFORM09(ACTNUM,3)
                  VARABL(VBCNT+1,13)=DEFORM09(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.10) THEN
C     USE THE DEFORM01 ARRAY
                  VARABL(VBCNT+1,4)= DEFORM10(ACTNUM,3)
                  VARABL(VBCNT+1,5)= DEFORM10(ACTNUM,3)
                  VARABL(VBCNT+1,13)=DEFORM10(ACTNUM,3)
              END IF
              RETURN
          END IF
C
          IF(ICODE.EQ.7) THEN
              IF(INT(DEFGR2).EQ.1) THEN
                  VALUE1=DEFORM01(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.2) THEN
                  VALUE1=DEFORM02(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.3) THEN
                  VALUE1=DEFORM03(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.4) THEN
                  VALUE1=DEFORM04(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.5) THEN
                  VALUE1=DEFORM05(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.6) THEN
                  VALUE1=DEFORM06(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.7) THEN
                  VALUE1=DEFORM07(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.8) THEN
                  VALUE1=DEFORM08(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.9) THEN
                  VALUE1=DEFORM09(ACTNUM,3)
              END IF
              IF(INT(DEFGR2).EQ.10) THEN
                  VALUE1=DEFORM10(ACTNUM,3)
              END IF
              RETURN
          END IF
          IF(ICODE.EQ.6) THEN
C     THE ACTUATOR NUMBER IS ACTNUM PASSED IN THE DEFGRS COMMON BLOCK
C
              IF(INT(DEFGR2).EQ.1) THEN
C     USE THE DEFORM01 ARRAY
                  DEFORM01(ACTNUM,3)=NEWDEFVAL
              END IF
              IF(INT(DEFGR2).EQ.2) THEN
C     USE THE DEFORM01 ARRAY
                  DEFORM02(ACTNUM,3)=NEWDEFVAL
              END IF
              IF(INT(DEFGR2).EQ.3) THEN
C     USE THE DEFORM01 ARRAY
                  DEFORM03(ACTNUM,3)=NEWDEFVAL
              END IF
              IF(INT(DEFGR2).EQ.4) THEN
C     USE THE DEFORM01 ARRAY
                  DEFORM04(ACTNUM,3)=NEWDEFVAL
              END IF
              IF(INT(DEFGR2).EQ.5) THEN
C     USE THE DEFORM01 ARRAY
                  DEFORM05(ACTNUM,3)=NEWDEFVAL
              END IF
              IF(INT(DEFGR2).EQ.6) THEN
C     USE THE DEFORM01 ARRAY
                  DEFORM06(ACTNUM,3)=NEWDEFVAL
              END IF
              IF(INT(DEFGR2).EQ.7) THEN
C     USE THE DEFORM01 ARRAY
                  DEFORM07(ACTNUM,3)=NEWDEFVAL
              END IF
              IF(INT(DEFGR2).EQ.8) THEN
C     USE THE DEFORM01 ARRAY
                  DEFORM08(ACTNUM,3)=NEWDEFVAL
              END IF
              IF(INT(DEFGR2).EQ.9) THEN
C     USE THE DEFORM01 ARRAY
                  DEFORM09(ACTNUM,3)=NEWDEFVAL
              END IF
              IF(INT(DEFGR2).EQ.10) THEN
C     USE THE DEFORM01 ARRAY
                  DEFORM10(ACTNUM,3)=NEWDEFVAL
              END IF
              RETURN
          END IF
C
          IF(ICODE.EQ.4.OR.ICODE.EQ.5) THEN
C     CALCULATING VALUE1
C     THE CURRENT RAY COORDINATES AT THE SURFACE ARE
              XR=XPASS
              YR=YPASS
              ERROR1=.FALSE.
              ERROR2=.FALSE.
              HT108=DABS(ALENS(106,ISURF))
              IF(IFILE.EQ.1)
     1        CALL SAGGER(DEFORM01,MACT,AP,XR,YR,AVSPACE,ISURF)
              IF(IFILE.EQ.2)
     1        CALL SAGGER(DEFORM02,MACT,AP,XR,YR,AVSPACE,ISURF)
              IF(IFILE.EQ.3)
     1        CALL SAGGER(DEFORM03,MACT,AP,XR,YR,AVSPACE,ISURF)
              IF(IFILE.EQ.4)
     1        CALL SAGGER(DEFORM04,MACT,AP,XR,YR,AVSPACE,ISURF)
              IF(IFILE.EQ.5)
     1        CALL SAGGER(DEFORM05,MACT,AP,XR,YR,AVSPACE,ISURF)
              IF(IFILE.EQ.6)
     1        CALL SAGGER(DEFORM06,MACT,AP,XR,YR,AVSPACE,ISURF)
              IF(IFILE.EQ.7)
     1        CALL SAGGER(DEFORM07,MACT,AP,XR,YR,AVSPACE,ISURF)
              IF(IFILE.EQ.8)
     1        CALL SAGGER(DEFORM08,MACT,AP,XR,YR,AVSPACE,ISURF)
              IF(IFILE.EQ.9)
     1        CALL SAGGER(DEFORM09,MACT,AP,XR,YR,AVSPACE,ISURF)
              IF(IFILE.EQ.10)
     1        CALL SAGGER(DEFORM10,MACT,AP,XR,YR,AVSPACE,ISURF)
              IF(ERROR1) THEN
                  CALL MACFAL
                  RETURN
              END IF
              IF(ERROR2) THEN
                  CALL MACFAL
                  RETURN
              END IF
              CONTINUE
C
              IF(ICODE.EQ.4) THEN
C     AP IS THE SAG
C
                  ZPASS=ALENS(106,ISURF)*AP
                  RETURN
              END IF
              IF(ICODE.EQ.5) THEN
C     AP IS THE SAG
C
                  ZPASS=ALENS(106,ISURF)*AP
                  RETURN
              END IF
          END IF
          IF(ICODE.EQ.3) THEN
C     FILLS THE APPROPRIATE FILE WITH FLAT ACTUATOR DATA AND FILLS THE
C     ARRAY WITH ZERO VALUE1S
              IFILE=INT(DEFGR2)
              IF(IFILE.EQ.1.0D0)    FLNAME='DEFORM01.DAT'
              IF(IFILE.EQ.2.0D0)    FLNAME='DEFORM02.DAT'
              IF(IFILE.EQ.3.0D0)    FLNAME='DEFORM03.DAT'
              IF(IFILE.EQ.4.0D0)    FLNAME='DEFORM04.DAT'
              IF(IFILE.EQ.5.0D0)    FLNAME='DEFORM05.DAT'
              IF(IFILE.EQ.6.0D0)    FLNAME='DEFORM06.DAT'
              IF(IFILE.EQ.7.0D0)    FLNAME='DEFORM07.DAT'
              IF(IFILE.EQ.8.0D0)    FLNAME='DEFORM08.DAT'
              IF(IFILE.EQ.9.0D0)    FLNAME='DEFORM09.DAT'
              IF(IFILE.EQ.10.0D0)   FLNAME='DEFORM10.DAT'
              EXIS26=.FALSE.
              OPEN26=.FALSE.
              INQUIRE(FILE=trim(HOME)//FLNAME,EXIST=EXIS26)
              INQUIRE(FILE=trim(HOME)//FLNAME,OPENED=OPEN26)
              IF(OPEN26) CALL CLOSE_FILE(26,1)
              OPEN(UNIT=26,FILE=trim(HOME)//FLNAME,
     1        STATUS='UNKNOWN')
C     IF HERE, THEN ALLOCATE THE ARRAY AND READ THE DATA AGAIN
              IF(IFILE.EQ.1)   ALLOCATE(DEFORM01(1:MACT,1:3),STAT=ALLOERR)
              IF(IFILE.EQ.2)   ALLOCATE(DEFORM02(1:MACT,1:3),STAT=ALLOERR)
              IF(IFILE.EQ.3)   ALLOCATE(DEFORM03(1:MACT,1:3),STAT=ALLOERR)
              IF(IFILE.EQ.4)   ALLOCATE(DEFORM04(1:MACT,1:3),STAT=ALLOERR)
              IF(IFILE.EQ.5)   ALLOCATE(DEFORM05(1:MACT,1:3),STAT=ALLOERR)
              IF(IFILE.EQ.6)   ALLOCATE(DEFORM06(1:MACT,1:3),STAT=ALLOERR)
              IF(IFILE.EQ.7)   ALLOCATE(DEFORM07(1:MACT,1:3),STAT=ALLOERR)
              IF(IFILE.EQ.8)   ALLOCATE(DEFORM08(1:MACT,1:3),STAT=ALLOERR)
              IF(IFILE.EQ.9)   ALLOCATE(DEFORM09(1:MACT,1:3),STAT=ALLOERR)
              IF(IFILE.EQ.10)   ALLOCATE(DEFORM10(1:MACT,1:3),STAT=ALLOERR)
C     INITIALIZE TO 0.0D0
              BA=0.0D0
              IF(IFILE.EQ.1)   DEFORM01(1:MACT,1:3)=BA
              IF(IFILE.EQ.2)   DEFORM02(1:MACT,1:3)=BA
              IF(IFILE.EQ.3)   DEFORM03(1:MACT,1:3)=BA
              IF(IFILE.EQ.4)   DEFORM04(1:MACT,1:3)=BA
              IF(IFILE.EQ.5)   DEFORM05(1:MACT,1:3)=BA
              IF(IFILE.EQ.6)   DEFORM06(1:MACT,1:3)=BA
              IF(IFILE.EQ.7)   DEFORM07(1:MACT,1:3)=BA
              IF(IFILE.EQ.8)   DEFORM08(1:MACT,1:3)=BA
              IF(IFILE.EQ.9)   DEFORM09(1:MACT,1:3)=BA
              IF(IFILE.EQ.10)  DEFORM10(1:MACT,1:3)=BA
              IF(EXIS26) THEN
C     READ EXISTING FILE
                  DO I=1,MM
                      READ(26,FMT=*,END=888) X,Y,DEFVAL
                      IF(IFILE.EQ.1) DEFORM01(I,1)=X
                      IF(IFILE.EQ.1) DEFORM01(I,2)=Y
                      IF(IFILE.EQ.1) DEFORM01(I,3)=DEFVAL
                      IF(IFILE.EQ.2) DEFORM02(I,1)=X
                      IF(IFILE.EQ.2) DEFORM02(I,2)=Y
                      IF(IFILE.EQ.2) DEFORM02(I,3)=DEFVAL
                      IF(IFILE.EQ.3) DEFORM03(I,1)=X
                      IF(IFILE.EQ.3) DEFORM03(I,2)=Y
                      IF(IFILE.EQ.3) DEFORM03(I,3)=DEFVAL
                      IF(IFILE.EQ.4) DEFORM04(I,1)=X
                      IF(IFILE.EQ.4) DEFORM04(I,2)=Y
                      IF(IFILE.EQ.4) DEFORM04(I,3)=DEFVAL
                      IF(IFILE.EQ.5) DEFORM05(I,1)=X
                      IF(IFILE.EQ.5) DEFORM05(I,2)=Y
                      IF(IFILE.EQ.5) DEFORM05(I,3)=DEFVAL
                      IF(IFILE.EQ.6) DEFORM06(I,1)=X
                      IF(IFILE.EQ.6) DEFORM06(I,2)=Y
                      IF(IFILE.EQ.6) DEFORM06(I,3)=DEFVAL
                      IF(IFILE.EQ.7) DEFORM07(I,1)=X
                      IF(IFILE.EQ.7) DEFORM07(I,2)=Y
                      IF(IFILE.EQ.7) DEFORM07(I,3)=DEFVAL
                      IF(IFILE.EQ.8) DEFORM08(I,1)=X
                      IF(IFILE.EQ.8) DEFORM08(I,2)=Y
                      IF(IFILE.EQ.8) DEFORM08(I,3)=DEFVAL
                      IF(IFILE.EQ.9) DEFORM09(I,1)=X
                      IF(IFILE.EQ.9) DEFORM09(I,2)=Y
                      IF(IFILE.EQ.9) DEFORM09(I,3)=DEFVAL
                      IF(IFILE.EQ.10) DEFORM10(I,1)=X
                      IF(IFILE.EQ.10) DEFORM10(I,2)=Y
                      IF(IFILE.EQ.10) DEFORM10(I,3)=DEFVAL
                  END DO
              END IF
 888          CONTINUE
C     FILL ARRAY
              IF(.NOT.EXIS26) THEN
                  ERROR1=.TRUE.
                  ERROR2=.TRUE.
                  WRITE(OUTLYNE,*) 'DEFORMABLE SURFACE FILE ',FLNAME
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'DOES NOT EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(ICODE.EQ.6) THEN
C     FILLS THE APPROPRIATE FILE WITH EXISTING DEFGRID VALUE1S
C     ARRAY WITH ZERO VALUE1S
              IFILE=INT(DEFGR2)
              IF(IFILE.EQ.1.0D0)    FLNAME='DEFORM01.DAT'
              IF(IFILE.EQ.2.0D0)    FLNAME='DEFORM02.DAT'
              IF(IFILE.EQ.3.0D0)    FLNAME='DEFORM03.DAT'
              IF(IFILE.EQ.4.0D0)    FLNAME='DEFORM04.DAT'
              IF(IFILE.EQ.5.0D0)    FLNAME='DEFORM05.DAT'
              IF(IFILE.EQ.6.0D0)    FLNAME='DEFORM06.DAT'
              IF(IFILE.EQ.7.0D0)    FLNAME='DEFORM07.DAT'
              IF(IFILE.EQ.8.0D0)    FLNAME='DEFORM08.DAT'
              IF(IFILE.EQ.9.0D0)    FLNAME='DEFORM09.DAT'
              IF(IFILE.EQ.10.0D0)   FLNAME='DEFORM10.DAT'
              EXIS26=.FALSE.
              OPEN26=.FALSE.
              INQUIRE(FILE=trim(HOME)//FLNAME,EXIST=EXIS26)
              INQUIRE(FILE=trim(HOME)//FLNAME,OPENED=OPEN26)
              IF(OPEN26) CALL CLOSE_FILE(26,1)
              OPEN(UNIT=26,FILE=trim(HOME)//FLNAME,
     1        STATUS='UNKNOWN')
C     ARRAY ALREADY ALLOCATED
C     FILL FILE WITH EXISTING ARRAY
              DO I=1,MACT
                  IF(IFILE.EQ.1) WRITE(26,FMT=*)
     1             DEFORM01(I,1),DEFORM01(I,2),DEFORM01(I,3)
                  IF(IFILE.EQ.2) WRITE(26,FMT=*)
     1             DEFORM02(I,1),DEFORM02(I,2),DEFORM02(I,3)
                  IF(IFILE.EQ.3) WRITE(26,FMT=*)
     1             DEFORM03(I,1),DEFORM03(I,2),DEFORM03(I,3)
                  IF(IFILE.EQ.4) WRITE(26,FMT=*)
     1             DEFORM04(I,1),DEFORM04(I,2),DEFORM04(I,3)
                  IF(IFILE.EQ.5) WRITE(26,FMT=*)
     1             DEFORM05(I,1),DEFORM05(I,2),DEFORM05(I,3)
                  IF(IFILE.EQ.6) WRITE(26,FMT=*)
     1             DEFORM06(I,1),DEFORM06(I,2),DEFORM06(I,3)
                  IF(IFILE.EQ.7) WRITE(26,FMT=*)
     1             DEFORM07(I,1),DEFORM07(I,2),DEFORM07(I,3)
                  IF(IFILE.EQ.8) WRITE(26,FMT=*)
     1             DEFORM08(I,1),DEFORM08(I,2),DEFORM08(I,3)
                  IF(IFILE.EQ.9) WRITE(26,FMT=*)
     1             DEFORM09(I,1),DEFORM09(I,2),DEFORM09(I,3)
                  IF(IFILE.EQ.10) WRITE(26,FMT=*)
     1             DEFORM10(I,1),DEFORM10(I,2),DEFORM10(I,3)
              END DO
C     SET LOADED FLAG
              ALENS(109,ISURF)=1.0D0
              CALL CLOSE_FILE(26,1)
              RETURN
          END IF
          IF(ICODE.EQ.9) THEN
C     FILLS THE APPROPRIATE FILE WITH CURRENT ARRAY VALUE1S
              IFILE=INT(DEFGR2)
              IF(IFILE.EQ.1.0D0)    FLNAME='DEFORM01.DAT'
              IF(IFILE.EQ.2.0D0)    FLNAME='DEFORM02.DAT'
              IF(IFILE.EQ.3.0D0)    FLNAME='DEFORM03.DAT'
              IF(IFILE.EQ.4.0D0)    FLNAME='DEFORM04.DAT'
              IF(IFILE.EQ.5.0D0)    FLNAME='DEFORM05.DAT'
              IF(IFILE.EQ.6.0D0)    FLNAME='DEFORM06.DAT'
              IF(IFILE.EQ.7.0D0)    FLNAME='DEFORM07.DAT'
              IF(IFILE.EQ.8.0D0)    FLNAME='DEFORM08.DAT'
              IF(IFILE.EQ.9.0D0)    FLNAME='DEFORM09.DAT'
              IF(IFILE.EQ.10.0D0)   FLNAME='DEFORM10.DAT'
              OPEN26=.FALSE.
              INQUIRE(FILE=trim(HOME)//FLNAME,OPENED=OPEN26)
              IF(OPEN26) CALL CLOSE_FILE(26,1)
              OPEN(UNIT=26,FILE=trim(HOME)//FLNAME,
     1        STATUS='UNKNOWN')
C     WRITE ARRAY TO FILE
              DO I=1,MACT
                  IF(IFILE.EQ.1) WRITE(26,FMT=*)
     1             DEFORM01(I,1),DEFORM01(I,2),DEFORM01(I,3)
                  IF(IFILE.EQ.2) WRITE(26,FMT=*)
     1             DEFORM02(I,1),DEFORM02(I,2),DEFORM02(I,3)
                  IF(IFILE.EQ.3) WRITE(26,FMT=*)
     1             DEFORM03(I,1),DEFORM03(I,2),DEFORM03(I,3)
                  IF(IFILE.EQ.4) WRITE(26,FMT=*)
     1             DEFORM04(I,1),DEFORM04(I,2),DEFORM04(I,3)
                  IF(IFILE.EQ.5) WRITE(26,FMT=*)
     1             DEFORM05(I,1),DEFORM05(I,2),DEFORM05(I,3)
                  IF(IFILE.EQ.6) WRITE(26,FMT=*)
     1             DEFORM06(I,1),DEFORM06(I,2),DEFORM06(I,3)
                  IF(IFILE.EQ.7) WRITE(26,FMT=*)
     1             DEFORM07(I,1),DEFORM07(I,2),DEFORM07(I,3)
                  IF(IFILE.EQ.8) WRITE(26,FMT=*)
     1             DEFORM08(I,1),DEFORM08(I,2),DEFORM08(I,3)
                  IF(IFILE.EQ.9) WRITE(26,FMT=*)
     1             DEFORM09(I,1),DEFORM09(I,2),DEFORM09(I,3)
                  IF(IFILE.EQ.10) WRITE(26,FMT=*)
     1             DEFORM10(I,1),DEFORM10(I,2),DEFORM10(I,3)
              END DO
              CALL CLOSE_FILE(26,1)
              RETURN
          END IF
          RETURN
      END
      SUBROUTINE SAGGER(DEFORMED,MACT,AP,XR,YR
     1,AVSPACE,ISURF)
          IMPLICIT NONE
          INTEGER MACT,I,ISURF
          REAL*8 DEFORMED(1:MACT,1:3),AP,XR,YR,A0,A1,A2,A3,A4,A5
          REAL*8 AVSPACE,XSI,SAG,PSI,X1,Y1
!      INTEGER ICODE
!      LOGICAL ERROR1,ERROR2
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C     A0=1.08342D0
C     A1=-0.0069D0
C     A2=+0.4860D0
C     A3=-0.0630D0
C     A4=+0.0050D0
C     A5=+0.0050D0
          A0=GPREG(100)
          A1=GPREG(101)
          A2=GPREG(102)
          A3=GPREG(103)
          A4=GPREG(104)
          A5=GPREG(105)
          SAG=0.0D0
          IF(AVSPACE.EQ.0.0D0) AVSPACE=1.0D0
          DO I=1,MACT
              X1=DEFORMED(I,1)+ALENS(13,ISURF)
              Y1=DEFORMED(I,2)+ALENS(12,ISURF)
              XSI=(DSQRT(((X1-XR)**2)+((Y1-YR)**2))
     1        /AVSPACE)
              PSI=(XSI-A1)/A2
              SAG=SAG+
     1        (DEFORMED(I,3)*((A0*DEXP(-(PSI**2)/2.0D0))
     1        +A3+(A4*XSI)+(A5*(XSI**2))))
          END DO
          AP=SAG
C
          RETURN
      END

C SUB FANS.FOR
      SUBROUTINE FANS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FANS.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CMD LEVEL COMMANDS YFAN, XFAN, NFAN AND PFAN.
C       IT IS CALLED BY CMDER.FOR.
C
          CHARACTER UNI*11,FANNAM*8,FANQAL*8
C
          LOGICAL FANEXT,RIM
C
          COMMON/RIMSHT/RIM
C
          COMMON/FANEXI/FANEXT
C
          INTEGER FANWAV
C
          COMMON/FANNER/FANWAV
C
          COMMON/PASFAN/FANNAM,FANQAL
C
          INTEGER J,JJ,IX,WWRF,WWVN
C
          REAL*8 COSARG,XTEMP,YTEMP,TEMP1,TEMP2,TEMP3,TEMP4,
     1    LOWERL,DELTA,OFFSET,FW4,XI,XX,YY,LLR,MMR,NNR,LLP,MMP,NNP,
     2    LPWP1,LPWP2,LCW,LSWP1,LSWP2,XXDIF,YYDIF,OOPD,OPDW,
     3    WAV,RRDIF,DIF1,DIF2,DIF3,DIF4,PW11,PW12,PW21,PW22,
     4    JA,JB,SW11,SW12,SW21,SW22,LAX,LAY,DX,DY,DTY,DTX
C
          LOGICAL FOBB0
     1    ,FOBB0X,FOBB0Y
C
          COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
          JA=COS_A_ANG
          JB=COS_B_ANG
          IF(INT(LFOB(4)).EQ.1) WWRF=46
          IF(INT(LFOB(4)).EQ.2) WWRF=47
          IF(INT(LFOB(4)).EQ.3) WWRF=48
          IF(INT(LFOB(4)).EQ.4) WWRF=49
          IF(INT(LFOB(4)).EQ.5) WWRF=50
          IF(INT(LFOB(4)).EQ.6) WWRF=71
          IF(INT(LFOB(4)).EQ.7) WWRF=72
          IF(INT(LFOB(4)).EQ.8) WWRF=73
          IF(INT(LFOB(4)).EQ.9) WWRF=74
          IF(INT(LFOB(4)).EQ.10) WWRF=75
C
          FANEXT=.FALSE.
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"'//WC(1:4)//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
C       PROCEED
          END IF
C       CHECK FOR VALID QUALIFIER INPUT
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'OPD'.AND.WQ.NE.'CD'.AND.WQ.NE.'LA') THEN
                  OUTLYNE=
     1            '"'//WC(1:4)//'" ENTERED WITH INVALID QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
C       QUALIFIERS ARE VALID, PROCEED
              END IF
C       NO QUALIFIERS
          END IF

C       CHECK IF REFEXT=.FALSE. IF SO STOP AND PRINT  MESSAGE
C
          IF(.NOT.REFEXT) THEN
C       NO CHIEF RAY EXISTS, STOP
              OUTLYNE=
     1        'AN "FOB" COMMAND MUST BE ISSUED BEFORE FANS CAN BE TRACED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CHECK FOR VALID WAVELENGTH NUMBER IF NOT DEFAULT
C       IT MUST BE 1,2,3,4 OR 5. IF DEFAULT, IT BECOMES THE
C       CONTROL WAVELENGTH.
C
          IF(DF3.EQ.1) THEN
              DF3=0
              WW3=SYSTEM1(11)
              FANWAV=INT(WW3)
              WVN=WW3
              IF(INT(WW3).EQ.1) WWVN=46
              IF(INT(WW3).EQ.2) WWVN=47
              IF(INT(WW3).EQ.3) WWVN=48
              IF(INT(WW3).EQ.4) WWVN=49
              IF(INT(WW3).EQ.5) WWVN=50
              IF(INT(WW3).EQ.6) WWVN=71
              IF(INT(WW3).EQ.7) WWVN=72
              IF(INT(WW3).EQ.8) WWVN=73
              IF(INT(WW3).EQ.9) WWVN=74
              IF(INT(WW3).EQ.10) WWVN=75
          ELSE
C       WAVELENGTH NUMBER NOT DEFAULT
              IF(W3.NE.1.0D0.AND.W3.NE.2.0D0.AND.W3.NE.3.0D0
     1        .AND.W3.NE.4.0D0.AND.W3.NE.5.0D0.AND.
     1        W3.NE.6.0D0.AND.W3.NE.7.0D0.AND.W3.NE.8.0D0
     1        .AND.W3.NE.9.0D0.AND.W3.NE.10.0D0) THEN
                  OUTLYNE=
     1            'INVALID WAVELENGTH NUMBER'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       WAVELENGTH NUMBER OK, PROCEED
                  WW3=W3
                  FANWAV=INT(WW3)
                  WVN=WW3
                  IF(INT(WW3).EQ.1) WWVN=46
                  IF(INT(WW3).EQ.2) WWVN=47
                  IF(INT(WW3).EQ.3) WWVN=48
                  IF(INT(WW3).EQ.4) WWVN=49
                  IF(INT(WW3).EQ.5) WWVN=50
                  IF(INT(WW3).EQ.6) WWVN=71
                  IF(INT(WW3).EQ.7) WWVN=72
                  IF(INT(WW3).EQ.8) WWVN=73
                  IF(INT(WW3).EQ.9) WWVN=74
                  IF(INT(WW3).EQ.10) WWVN=75
              END IF
          END IF
C
C       RESOLVE DEFAULT OFFSET
          IF(DF5.EQ.1) THEN
              OFFSET=0.0D0
          ELSE
C       OFFSET NOT DEFAULT, USE VALUE1 OF W5
              OFFSET=W5
          END IF
C
C       CHECK FOR VALID NUMBER OF RAYS. MUST BE POSITIVE
C       AND GREATER THAN 1 IF NOT DEFAULT. IF DEFAULT, VALUE1
C       IS DETERMINED BY SYMMETRY CONDITIONS LATER.
          IF(DF4.EQ.0) THEN
C       RAY NUMBER NOT DEFAULT
              FW4=DABS(DABS(W4)-W4)
              IF(W4.LE.1.0D0.OR.FW4.NE.0.0D0) THEN
C       NUMBER OF RAYS NOT OK
                  OUTLYNE='INVALID NUMBER OF RAYS REQUESTED'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'NUMBER OF RAYS MUST BE AN INTEGER AND GREATER THAN 1'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       DETERMINATION OF NUMBER OF RAYS DONE LATER
          END IF
C
C       WHEN FANS IS CALLED FROM THE CMD LEVEL,
C       CLEAR APERTURE/OBSCURATION CHECKING IS SET
C       TO "ON" BY SETTING CACOCH=1.
C
          CACOCH=1
C
C       SET DEFAULT NUMERICS
          IF(FOBB0) THEN
C       ON-AXIS, GO FROM 0 TO 1 DO HALF FAN IF ROTATIONAL SYMMETRY EXISTS
C       SYMMETRY CHECK
              IF(SYSTEM1(28).EQ.1.0D0.AND.SYSTEM1(48).EQ.1.0D0) THEN
                  IF(DF1.EQ.1) W1=0.0D0
                  IF(DF4.EQ.1) W4=6.0D0
              ELSE
                  IF(DF1.EQ.1) W1=-1.0D0
                  IF(DF4.EQ.1) W4=11.0D0
              END IF
              IF(DF2.EQ.1) W2=+1.0D0
          ELSE
C       NOT ON AXIS DO FULL FAN
              IF(DF1.EQ.1) W1=-1.0D0
              IF(DF2.EQ.1) W2=1.0D0
              IF(DF4.EQ.1) W4=11.0D0
          END IF
C       DELTA IS THE RELATIVE APERTURE DELTA BETWEEN RAYS
          DELTA=(W2-W1)/(W4-1.0D0)
C
          IF(W1.GE.W2) THEN
              OUTLYNE='FOR THE "'//WC(1:4)//'" COMMAND'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'NUMERIC WORD #1 MUST BE LESS THAN NUMERIC WORD #2'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       NOW LIMITS, DELTA(RELATED TO THE NUMBER OF RAYS),
C       WAVLENGTH NUMBER AND OFFSET HAVE BEEN DETERMINED.
          LOWERL=W1
C       WAVLENGTH NUMBER WAS W3
C               DELTA (SET ABOVE)
C               NUMBER OF RAYS WAS (W4)
C               OFFSET WAS W5
C       AT THIS POINT MSG=FALSE
C
C       NOW DETERMINE THE LIMITS OF THE DO LOOP FOR THE
C       ACTUAL FAN TRACE, THE INCREMENT, AND THE ALGORITHM
C       FOR GOING FROM ONE VALUE1 TO THE NEXT.
C
C       THE LOWER LIMIT IS LOWERL
C       THE INCREMENT IS DELTA
C       THE OFFSET IS OFFSET
C
C       THE ACTUAL COORDINATES OF THE RAYS TO BE TRACED (AT
C       THE REFERENCE SURFACE) ARE DETERMINED IN RAYTRA. ALL THAT NEEDS
C       BE SENT TO RAYTRA IS THE RELATIVE APERTURE POSITIONS.
C       CIRCULAR AND RECTANGULAR PUPILS ARE RESOLVED IN RAYTRA.FOR.
C
C       THE WALELENGTH NUMBER IS WW3
C
C       IF PUPIL IS SET TO RECT, TEMPORARILY RESET IT TO CIRCULAR
C       AS FANS ARE ONLY DEFINED FOR A CIRCULAR PUPIL DEFINITION
C       DUE TO THE PRESENCE OF PFAN AND NFAN. IN THIS TEMPORARY
C       REDEFINITION, WE DON'T CHANGE THE VALUE1 OF SAX OR A
C       CLAPX ON THE REFERENCE SURFACE, WE JUST DON'T USE IT.
C
          IF(SYSTEM1(6).EQ.1.0D0) UNI='INCHES     '
          IF(SYSTEM1(6).EQ.2.0D0) UNI='CENTIMETERS'
          IF(SYSTEM1(6).EQ.3.0D0) UNI='MILLIMETERS'
          IF(SYSTEM1(6).EQ.4.0D0) UNI='METERS     '
          LCW=SYSTEM1(INT(SYSTEM1(11)))
          LPWP1=SYSTEM1(INT(SYSTEM1(7)))
          LPWP2=SYSTEM1(INT(SYSTEM1(8)))
          LSWP1=SYSTEM1(INT(SYSTEM1(9)))
          LSWP2=SYSTEM1(INT(SYSTEM1(10)))
C       NO QUALIFIER
          IF(SQ.EQ.0) THEN
              WRITE(OUTLYNE,1000) UNI
              CALL SHOWIT(0)
 1000         FORMAT('TRANSVERSE ABERRATION TABLE : UNITS = '
     1        ,A11)
              IF(WC.EQ.'YFAN') WRITE(OUTLYNE,1001)
              IF(WC.EQ.'XFAN') WRITE(OUTLYNE,1002)
              IF(WC.EQ.'NFAN') WRITE(OUTLYNE,1003)
              IF(WC.EQ.'PFAN') WRITE(OUTLYNE,1004)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,9999)
                  CALL SHOWIT(0)
              END IF
              IF(SYSTEM1(30).EQ.4.0D0) THEN
                  WRITE(OUTLYNE,8888)
                  CALL SHOWIT(0)
              END IF
 9999         FORMAT(
     1        'MODE IS AFOCAL, "DXA", "DYA" AND "DRA" UNITS ARE RADIANS')
 8888         FORMAT(
     1        'MODE IS UAFOCAL, "DXA", "DYA" AND "DRA" UNITS ARE RADIANS')
 1001         FORMAT('RAY FAN TRACED IS A  "YFAN"')
 1002         FORMAT('RAY FAN TRACED IS AN "XFAN"')
 1003         FORMAT('RAY FAN TRACED IS AN "NFAN"')
 1004         FORMAT('RAY FAN TRACED IS A  "PFAN"')
              WRITE(OUTLYNE,1005) OFFSET
              CALL SHOWIT(0)
 1005         FORMAT(
     1        'FRACTIONAL FAN OFFSET = ',G14.6,' (RELATIVE TO FULL APERTURE)')
              WRITE(OUTLYNE,2005) SYSTEM1(INT(WW3))
              CALL SHOWIT(0)
 2005         FORMAT(
     1        'WAVLENGTH = ',G14.6,' MICROMETER')
          END IF
          IF(WQ.EQ.'LA'.AND.SYSTEM1(30).EQ.3.0D0.OR.WQ.EQ.'LA'
     1    .AND.SYSTEM1(30).EQ.4.0D0) THEN
              OUTLYNE=
     1        'THE CURRENT LENS MODE IS "AFOCAL" OR "UAFOCAL"'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'LONGITUDINAL ABERRATIONS ARE NOT DEFINED FOR THESE MODES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       QUALIFIER "LA"
          IF(WQ.EQ.'LA') THEN
              WRITE(OUTLYNE,1100) UNI
              CALL SHOWIT(0)
 1100         FORMAT('LONGITUDINAL ABERRATION TABLE : UNITS ='
     1        ,A11)
              IF(WC.EQ.'YFAN') WRITE(OUTLYNE,1001)
              IF(WC.EQ.'XFAN') WRITE(OUTLYNE,1002)
              IF(WC.EQ.'NFAN') WRITE(OUTLYNE,1003)
              IF(WC.EQ.'PFAN') WRITE(OUTLYNE,1004)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1005) OFFSET
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2005) SYSTEM1(INT(WW3))
              CALL SHOWIT(0)
          END IF
C       QUALIFIER "CD"
          IF(WQ.EQ.'CD') THEN
              WRITE(OUTLYNE,1200) UNI
              CALL SHOWIT(0)
 1200         FORMAT(
     1        'PRIMARY/SECONDARY CHROMATIC DIFFERENCE TABLE : UNITS = '
     1        ,A11)
              IF(WC.EQ.'YFAN') WRITE(OUTLYNE,1001)
              IF(WC.EQ.'XFAN') WRITE(OUTLYNE,1002)
              IF(WC.EQ.'NFAN') WRITE(OUTLYNE,1003)
              IF(WC.EQ.'PFAN') WRITE(OUTLYNE,1004)
              CALL SHOWIT(0)
              IF(SYSTEM1(30).EQ.3.0D0) THEN
                  WRITE(OUTLYNE,9995)
                  CALL SHOWIT(0)
              END IF
              IF(SYSTEM1(30).EQ.4.0D0) THEN
                  WRITE(OUTLYNE,8885)
                  CALL SHOWIT(0)
              END IF
 9995         FORMAT(
     1        'MODE IS AFOCAL, CHROMATIC DIFFERENCE UNITS ARE RADIANS')
 8885         FORMAT(
     1        'MODE IS UAFOCAL, CHROMATIC DIFFERENCE UNITS ARE RADIANS')
              WRITE(OUTLYNE,1006) OFFSET
              CALL SHOWIT(0)
 1006         FORMAT(
     1        'FRACTIONAL FAN OFFSET = ',G14.6,' (RELATIVE TO FULL APERTURE)')
              WRITE(OUTLYNE,1008) LCW
              CALL SHOWIT(0)
 1008         FORMAT('CONTROL WAVELENGTH = ',G14.6,' MICROMETER')
 1009         FORMAT('PRIMARY WAVELENGTH PAIR    = ',
     1        G14.6,' AND ',G14.6,' MICROMETER')
 1010         FORMAT('SECONDARY WAVELENGTH PAIR  = ',
     1        G14.6,' AND ',G14.6,' MICROMETER')
              WRITE(OUTLYNE,1009) LPWP1,LPWP2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1010) LSWP1,LSWP2
              CALL SHOWIT(0)
          END IF
C       QUALIFIER "OPD"
          IF(WQ.EQ.'OPD') THEN
              WRITE(OUTLYNE,1300) UNI
              CALL SHOWIT(0)
 1300         FORMAT('OPTICAL PATH DIFFERENCE TABLE : UNITS = '
     1        ,A11)
              IF(WC.EQ.'YFAN') WRITE(OUTLYNE,1001)
              IF(WC.EQ.'XFAN') WRITE(OUTLYNE,1002)
              IF(WC.EQ.'NFAN') WRITE(OUTLYNE,1003)
              IF(WC.EQ.'PFAN') WRITE(OUTLYNE,1004)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1011) OFFSET
              CALL SHOWIT(0)
 1011         FORMAT(
     1        'FRACTIONAL FAN OFFSET = ',G14.6,' (RELATIVE TO FULL APERTURE)')
              WRITE(OUTLYNE,2011) SYSTEM1(INT(WW3))
              CALL SHOWIT(0)
 2011         FORMAT(
     1        'OPD REFERENCE WAVLENGTH = ',G14.6,' MICROMETER')
          END IF
C
C       NOW PRINT THE COLUMN HEADINGS FOR THE OUTPUT.
C
C       ********************
C
C       QUALIFIER "     "
          IF(SQ.EQ.0) THEN
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
 5000         FORMAT(1X)
              IF(WC.EQ.'YFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,5001)
                      CALL SHOWIT(0)
 5001                 FORMAT(' REL AP HT ',9X,'DX ',17X,'DY ')
                  ELSE
C                       MODE AFOCAL
                      WRITE(OUTLYNE,5002)
                      CALL SHOWIT(0)
 5002                 FORMAT(' REL AP HT ',9X,'DXA',17X,'DYA')
                  END IF
              END IF
              IF(WC.EQ.'XFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,5001)
                      CALL SHOWIT(0)
                  ELSE
C                       MODE AFOCAL
                      WRITE(OUTLYNE,5002)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WC.EQ.'NFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,5003)
                      CALL SHOWIT(0)
 5003                 FORMAT(' REL AP HT ',9X,'DN ',17X,'DP ',17X,'DR')
                  ELSE
C                       MODE AFOCAL
                      WRITE(OUTLYNE,5004)
                      CALL SHOWIT(0)
 5004                 FORMAT(' REL AP HT ',9X,'DNA',17X,'DPA',17X,'DRA')
                  END IF
              END IF
              IF(WC.EQ.'PFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,5003)
                      CALL SHOWIT(0)
                  ELSE
C                       MODE AFOCAL
                      WRITE(OUTLYNE,5004)
                      CALL SHOWIT(0)
                  END IF
              END IF
C       QUALIFIER NOT BLANK
          END IF
C
C       ********************
C       ********************
C
C       QUALIFIER "OPD"
          IF(WQ.EQ.'OPD') THEN
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              IF(WC.EQ.'YFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,6001)
                      CALL SHOWIT(0)
 6001                 FORMAT(' REL AP HT ',3X,' OPD (LENS UNITS) ',
     1                4X,'OPD (WAVE UNITS)')
                  ELSE
C                       MODE AFOCAL
                      WRITE(OUTLYNE,6001)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WC.EQ.'XFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,6001)
                      CALL SHOWIT(0)
                  ELSE
C                       MODE AFOCAL
                      WRITE(OUTLYNE,6001)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WC.EQ.'NFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,6001)
                      CALL SHOWIT(0)
                  ELSE
C                       MODE AFOCAL
                      WRITE(OUTLYNE,6001)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WC.EQ.'PFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,6001)
                      CALL SHOWIT(0)
                  ELSE
C                       MODE AFOCAL
                      WRITE(OUTLYNE,6001)
                  END IF
              END IF
C       QUALIFIER NOT "OPD"
          END IF
C
C       ********************
C       ********************
C
C       QUALIFIER "CD"
          IF(WQ.EQ.'CD') THEN
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              IF(WC.EQ.'YFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,7001)
                      CALL SHOWIT(0)
 7001                 FORMAT(
     1                ' REL AP HT ',4X,'PCDX',10X,'PCDY',10X,'SCDX',10X,'SCDY')
                  ELSE
C       MODE AFOCAL
                      WRITE(OUTLYNE,7002)
                      CALL SHOWIT(0)
 7002                 FORMAT(
     1                ' REL AP HT ',4X,'PCDXA',9X,'PCDYA',9X,'SCDXA',9X,'SCDYA')
                  END IF
              END IF
              IF(WC.EQ.'XFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,7001)
                      CALL SHOWIT(0)
                  ELSE
C       MODE AFOCAL
                      WRITE(OUTLYNE,7002)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WC.EQ.'NFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,7003)
                      CALL SHOWIT(0)
 7003                 FORMAT(
     1                ' REL AP HT ',4X,'PCDN',10X,'PCDP',10X,'SCDN',10X,'SCDP')
                  ELSE
C       MODE AFOCAL
                      WRITE(OUTLYNE,7004)
                      CALL SHOWIT(0)
 7004                 FORMAT(
     1                ' REL AP HT ',4X,'PCDNA',9X,'PCDPA',9X,'SCDNA',9X,'SCDPA')
                  END IF
              END IF
              IF(WC.EQ.'PFAN') THEN
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      WRITE(OUTLYNE,7003)
                      CALL SHOWIT(0)
                  ELSE
C       MODE AFOCAL
                      WRITE(OUTLYNE,7004)
                      CALL SHOWIT(0)
                  END IF
              END IF
C       QUALIFIER NOT CD
          END IF
C
C       ********************
C       ********************
C
C       QUALIFIER "LA"
          IF(WQ.EQ.'LA') THEN
              WRITE(OUTLYNE,5000)
              CALL SHOWIT(0)
              IF(WC.EQ.'YFAN') THEN
                  WRITE(OUTLYNE,8001)
                  CALL SHOWIT(0)
 8001             FORMAT(
     1            ' REL AP HT ',4X,'LAX ',10X,'LAY ',10X,'DTX ',10X,'DTY ')
              END IF
              IF(WC.EQ.'XFAN') THEN
                  WRITE(OUTLYNE,8001)
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'NFAN') THEN
                  WRITE(OUTLYNE,8002)
                  CALL SHOWIT(0)
 8002             FORMAT(
     1            ' REL AP HT ',4X,'LAN ',10X,'LAP ',10X,'DTN ',10X,'DTP ')
              END IF
              IF(WC.EQ.'PFAN') THEN
                  WRITE(OUTLYNE,8002)
                  CALL SHOWIT(0)
              END IF
C       QUALIFIER NOT LA
          END IF
C
C       ********************
C
          XI=LOWERL
          DO 100 IX=1,(INT(W4))
C
              IF(DABS(XI).LE.1D-15) XI=0.0D0
C       FOR EACH XI COUNTED
C       FOR YFANS
              IF(WC.EQ.'YFAN') THEN
C       RELATIVE X COORDINATE IS:
                  XX=OFFSET
C       RELATIVE Y COORDINATE IS:
                  YY=XI
              END IF
C       FOR XFANS
              IF(WC.EQ.'XFAN') THEN
C       RELATIVE X COORDINATE IS:
                  XX=XI
C       RELATIVE Y COORDINATE IS:
                  YY=OFFSET
              END IF
C       FOR NFANS
              IF(WC.EQ.'NFAN') THEN
C       RELATIVE X COORDINATE IS:
                  XX=(XI-OFFSET)/(DSQRT(2.0D0))
C       RELATIVE Y COORDINATE IS:
                  YY=(XI+OFFSET)/(DSQRT(2.0D0))
              END IF
C       FOR PFANS
              IF(WC.EQ.'PFAN') THEN
C       RELATIVE X COORDINATE IS:
                  XX=(XI+OFFSET)/(DSQRT(2.0D0))
C       RELATIVE Y COORDINATE IS:
                  YY=-(XI-OFFSET)/(DSQRT(2.0D0))
              END IF
C
C       NOW CALL RAYTRA TO TRACE THE SPECIFIC RAY
              WWQ='        '
              WW1=YY
              WW2=XX
C
              IF(SQ.EQ.0) THEN
                  RAYCOD(1)=0
                  RAYCOD(2)=-1
                  RV=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  CALL RAYTRA
                  F58=0
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      XXDIF=(RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG))/JB
                      YYDIF=(RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG))/JA
                      RRDIF=DSQRT((XXDIF**2)+(YYDIF**2))
                  ELSE
C       MOD AFOCAL
                      XXDIF=RAYRAY(11,NEWIMG)
     1                -REFRY(11,NEWIMG)
                      IF((XXDIF).GT.(PII)) XXDIF=XXDIF-(TWOPII)
                      IF((XXDIF).LT.(-PII)) XXDIF=XXDIF+(TWOPII)
                      IF(ABS(XXDIF).EQ.ABS(TWOPII)) XXDIF=0.0D0
                      IF((XXDIF).LT.(-TWOPII)) XXDIF=XXDIF+(TWOPII)
                      YYDIF=RAYRAY(12,NEWIMG)
     1                -REFRY(12,NEWIMG)
                      IF((YYDIF).GT.(PII)) YYDIF=YYDIF-(TWOPII)
                      IF((YYDIF).LT.(-PII)) YYDIF=YYDIF+(TWOPII)
                      IF(ABS(YYDIF).EQ.ABS(TWOPII)) YYDIF=0.0D0
                      IF((YYDIF).LT.(-TWOPII)) YYDIF=YYDIF+(TWOPII)

                      COSARG=((RAYRAY(4,NEWIMG)*REFRY(4,NEWIMG))+
     1                (RAYRAY(5,NEWIMG)*REFRY(5,NEWIMG))+
     1                (RAYRAY(6,NEWIMG)*REFRY(6,NEWIMG)))
                      IF(COSARG.LT.0.0D0) COSARG=-COSARG
                      IF(COSARG.GT.1.0D0) COSARG=1.0D0
                      RRDIF=DACOS(COSARG)
                      IF((RRDIF).GT.(PII)) RRDIF=RRDIF-(TWOPII)
                      IF((RRDIF).LT.(-PII)) RRDIF=RRDIF+(TWOPII)
                      IF(ABS(RRDIF).EQ.ABS(TWOPII)) RRDIF=0.0D0
                      IF((RRDIF).LT.(-TWOPII)) RRDIF=RRDIF+(TWOPII)
                  END IF
C
                  IF(WC.EQ.'XFAN'.OR.WC.EQ.'YFAN') THEN
                      IF(RAYCOD(1).EQ.0.0D0) THEN
                          IF(DABS(XXDIF).LT.1.0D-7) XXDIF=0.0D0
                          IF(DABS(YYDIF).LT.1.0D-7) YYDIF=0.0D0
                          WRITE(OUTLYNE,991)XI,XXDIF,YYDIF
                          CALL SHOWIT(0)
 991                      FORMAT(F11.6,2X,G18.10,2X,G18.10)
                      ELSE
                          WRITE(OUTLYNE,992)XI,RAYCOD(1),RAYCOD(2)
                          CALL SHOWIT(0)
 992                      FORMAT(F11.6,
     1                    ' RAY FAILURE CODE = ',I2,' AT SURFACE = '
     2                    ,I3)
                      END IF
                  ELSE
C       WC IS NFAN OR PFAN
                      IF(RAYCOD(1).EQ.0.0D0) THEN
                          IF(WC.EQ.'PFAN') THEN
                              IF(DABS(XXDIF).LT.1.0D-7) XXDIF=0.0D0
                              IF(DABS(YYDIF).LT.1.0D-7) YYDIF=0.0D0
                              XTEMP=(XXDIF*DCOS(PII/4.0D0))+(YYDIF*DSIN(PII/4.0D0))
                              YTEMP=(YYDIF*DCOS(PII/4.0D0))-(XXDIF*DSIN(PII/4.0D0))
                              XXDIF=XTEMP
                              YYDIF=YTEMP
                          END IF
                          IF(WC.EQ.'NFAN') THEN
                              IF(DABS(XXDIF).LT.1.0D-7) XXDIF=0.0D0
                              IF(DABS(YYDIF).LT.1.0D-7) YYDIF=0.0D0
                              XTEMP=(XXDIF*DCOS(-PII/4.0D0))+(YYDIF*DSIN(-PII/4.0D0))
                              YTEMP=(YYDIF*DCOS(-PII/4.0D0))-(XXDIF*DSIN(-PII/4.0D0))
                              XXDIF=XTEMP
                              YYDIF=YTEMP
                          END IF
                          IF(DABS(XXDIF).LT.1.0D-7) XXDIF=0.0D0
                          IF(DABS(YYDIF).LT.1.0D-7) YYDIF=0.0D0
                          IF(WC.EQ.'PFAN') WRITE(OUTLYNE,9991)XI,-XXDIF,-YYDIF,RRDIF
                          IF(WC.EQ.'NFAN') WRITE(OUTLYNE,9991)XI,YYDIF,XXDIF,RRDIF
                          CALL SHOWIT(0)
 9991                     FORMAT(F11.6,2X,G18.10,2X,G18.10,2X,G18.10)
                      ELSE
                          WRITE(OUTLYNE,992)XI,RAYCOD(1),RAYCOD(2)
                          CALL SHOWIT(0)
                      END IF
                  END IF
C       NOT (BLANK) QUALIFIER
              END IF
C
              IF(WQ.EQ.'OPD') THEN
                  RAYCOD(1)=0
                  RAYCOD(2)=-1
                  RV=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  IF(RAYEXT.AND.REFEXT) THEN
                      OOPD=0.0D0
                      RCOR=0.0D0
                      OCOR=0.0D0
                      IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) JJ=NEWOBJ+2
                      IF(DABS(ALENS(3,NEWOBJ)).LT.1.0D10) JJ=NEWOBJ+1
                      DO J=JJ,NEWIMG
                          OOPD=OOPD+RAYRAY(7,J)
     1                    -(REFRY(7,J)*(ALENS(WWVN,J-1)/ALENS(WWRF,J-1)))
                      END DO
                      IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
C               RCOR=0.0D0
C               OCOR=0.0D0
                          CALL FOPD
C       CALCULATE THEN APPLY ADJUSTMENT FOR THE BEGINNING AND ENDING
C       REFERENCE SPHERES.
                          OOPD=OOPD-(OCOR*ALENS(WWVN,NEWOBJ))+
     1                    (RCOR*ALENS(WWVN,NEWOBJ))
                          RCOR=0.0D0
                          OCOR=0.0D0
                          CENCEN=.FALSE.
                          CALL LOPD
                          OOPD=OOPD-(OCOR*ALENS(WWVN,NEWIMG-1))+
     1                    (RCOR*ALENS(WWVN,NEWIMG-1))
                      ELSE
C       MODE AFOCAL
C               RCOR=0.0D0
C               OCOR=0.0D0
                          CALL FOPD
C       CALCULATE THEN APPLY ADJUSTMENT FOR THE BEGINNING AND ENDING
C       REFERENCE SPHERES.
                          OOPD=OOPD-(OCOR*ALENS(WWVN,NEWOBJ))+
     1                    (RCOR*ALENS(WWVN,NEWOBJ))
                          RCOR=0.0D0
                          OCOR=0.0D0
                          CENCEN=.FALSE.
                          CALL LOPD
                          OOPD=OOPD-(OCOR*ALENS(WWVN,NEWIMG-1))+
     1                    (RCOR*ALENS(WWVN,NEWIMG-1))
                      END IF
                      IF(DABS(OOPD).LT.1.0D-15) OOPD=0.0D0

C     CALCULATE LEN IN WAVES AT THE REFERENCE RAY WAVELENGTH
                      IF(FANWAV.GE.1.AND.FANWAV.LE.5) THEN
                          WAV=SYSTEM1(FANWAV)
                      END IF
                      IF(FANWAV.GE.6.AND.FANWAV.LE.10) THEN
                          WAV=SYSTEM1(FANWAV+65)
                      END IF
                      IF(SYSTEM1(6).EQ.1.0D0) WAV=WAV*
     1                ((1.0D-3)/(25.4D0))
                      IF(SYSTEM1(6).EQ.2.0D0) WAV=WAV*(1.0D-4)
                      IF(SYSTEM1(6).EQ.3.0D0) WAV=WAV*(1.0D-3)
                      IF(SYSTEM1(6).EQ.4.0D0) WAV=WAV*(1.0D-6)
                      OOPD=-OOPD
                      IF(REVSTR) OOPD=-OOPD
                      OPDW=OOPD/WAV
                  ELSE
C     RAY FAILED OR NO REF RAY
                      OOPD=0.0D0
                      OPDW=0.0D0
                  END IF
C
                  IF(RAYCOD(1).EQ.0.0D0) THEN
C       IF OPD IN WAVES IS LESS THAN 0.0001 WAVES, SET OPDS TO ZERO
                      WRITE(OUTLYNE,993)XI,OOPD,OPDW
                      CALL SHOWIT(0)
 993                  FORMAT(F9.4,4X,G18.10,5X,G13.5)
                  ELSE
                      WRITE(OUTLYNE,992)XI,RAYCOD(1),RAYCOD(2)
                      CALL SHOWIT(0)
                  END IF
C       NOT (OPD) QUALIFIER
              END IF
C
              IF(WQ.EQ.'CD') THEN
                  RAYCOD(1)=0
                  RAYCOD(2)=-1
                  WW3=SYSTEM1(7)
                  WVN=WW3
                  RV=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      PW11=RAYRAY(1,NEWIMG)
                      PW12=RAYRAY(2,NEWIMG)
                  ELSE
C       MODE AFOCAL
                      PW11=RAYRAY(11,NEWIMG)
                      PW12=RAYRAY(12,NEWIMG)
                  END IF
                  WW3=SYSTEM1(8)
                  WVN=WW3
                  RV=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      PW21=RAYRAY(1,NEWIMG)
                      PW22=RAYRAY(2,NEWIMG)
                  ELSE
C       MODE AFOCAL
                      PW21=RAYRAY(11,NEWIMG)
                      PW22=RAYRAY(12,NEWIMG)
                  END IF
                  WW3=SYSTEM1(9)
                  WVN=WW3
                  RV=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      SW11=RAYRAY(1,NEWIMG)
                      SW12=RAYRAY(2,NEWIMG)
                  ELSE
C       MODE AFOCAL
                      SW11=RAYRAY(11,NEWIMG)
                      SW12=RAYRAY(12,NEWIMG)
                  END IF
                  WW3=SYSTEM1(10)
                  WVN=WW3
                  RV=.FALSE.
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
                  IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                      SW21=RAYRAY(1,NEWIMG)
                      SW22=RAYRAY(2,NEWIMG)
                  ELSE
C       MODE AFOCAL
                      SW21=RAYRAY(11,NEWIMG)
                      SW22=RAYRAY(12,NEWIMG)
                  END IF
C       PRIMARY PAIR
C       X-VALUE1
                  DIF1=PW11-PW21
                  IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                      IF((DIF1).GT.(PII)) DIF1=DIF1-(TWOPII)
                      IF((DIF1).LT.(-PII)) DIF1=DIF1+(TWOPII)
                      IF(ABS(DIF1).EQ.ABS(TWOPII)) DIF1=0.0D0
                      IF((DIF1).LT.(-TWOPII)) DIF1=DIF1+(TWOPII)
                  END IF
C       Y-VALUE1
                  DIF2=PW12-PW22
                  IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                      IF((DIF2).GT.(PII)) DIF2=DIF2-(TWOPII)
                      IF((DIF2).LT.(-PII)) DIF2=DIF2+(TWOPII)
                      IF(ABS(DIF2).EQ.ABS(TWOPII)) DIF2=0.0D0
                      IF((DIF2).LT.(-TWOPII)) DIF2=DIF2+(TWOPII)
                  END IF
C       SECONDARY PAIR
C       X-VALUE1
                  DIF3=SW11-SW21
                  IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                      IF((DIF3).GT.(PII)) DIF3=DIF3-(TWOPII)
                      IF((DIF3).LT.(-PII)) DIF3=DIF3+(TWOPII)
                      IF(ABS(DIF3).EQ.ABS(TWOPII)) DIF3=0.0D0
                      IF((DIF3).LT.(-TWOPII)) DIF3=DIF3+(TWOPII)
                  END IF
C       Y-VALUE1
                  DIF4=SW12-SW22
                  IF(SYSTEM1(30).GT.2.0D0) THEN
C     AFOCAL
                      IF((DIF4).GT.(PII)) DIF4=DIF4-(TWOPII)
                      IF((DIF4).LT.(-PII)) DIF4=DIF4+(TWOPII)
                      IF(ABS(DIF4).EQ.ABS(TWOPII)) DIF4=0.0D0
                      IF((DIF4).LT.(-TWOPII)) DIF4=DIF4+(TWOPII)
                  END IF
                  IF(RAYCOD(1).EQ.0.0D0) THEN
                      IF(WC.EQ.'PFAN') THEN
                          IF(DABS(DIF1).LE.1.0D-7) DIF1=0.0D0
                          IF(DABS(DIF2).LE.1.0D-7) DIF2=0.0D0
                          IF(DABS(DIF3).LE.1.0D-7) DIF3=0.0D0
                          IF(DABS(DIF4).LE.1.0D-7) DIF4=0.0D0
                          TEMP1=(DIF1*DCOS(PII/4.0D0))+(DIF2*DSIN(PII/4.0D0))
                          TEMP2=(DIF2*DCOS(PII/4.0D0))-(DIF1*DSIN(PII/4.0D0))
                          TEMP3=(DIF3*DCOS(PII/4.0D0))+(DIF4*DSIN(PII/4.0D0))
                          TEMP4=(DIF4*DCOS(PII/4.0D0))-(DIF3*DSIN(PII/4.0D0))
                          DIF1=-TEMP1
                          DIF2=-TEMP2
                          DIF3=-TEMP3
                          DIF4=-TEMP4
                      END IF
                      IF(WC.EQ.'NFAN') THEN
                          IF(DABS(DIF1).LE.1.0D-7) DIF1=0.0D0
                          IF(DABS(DIF2).LE.1.0D-7) DIF2=0.0D0
                          IF(DABS(DIF3).LE.1.0D-7) DIF3=0.0D0
                          IF(DABS(DIF4).LE.1.0D-7) DIF4=0.0D0
                          TEMP1=(DIF1*DCOS(-PII/4.0D0))+(DIF2*DSIN(-PII/4.0D0))
                          TEMP2=(DIF2*DCOS(-PII/4.0D0))-(DIF1*DSIN(-PII/4.0D0))
                          TEMP3=(DIF3*DCOS(-PII/4.0D0))+(DIF4*DSIN(-PII/4.0D0))
                          TEMP4=(DIF4*DCOS(-PII/4.0D0))-(DIF3*DSIN(-PII/4.0D0))
                          DIF1=TEMP1
                          DIF2=TEMP2
                          DIF3=TEMP3
                          DIF4=TEMP4
                      END IF
                      IF(DABS(DIF1).LE.1.0D-7) DIF1=0.0D0
                      IF(DABS(DIF2).LE.1.0D-7) DIF2=0.0D0
                      IF(DABS(DIF3).LE.1.0D-7) DIF3=0.0D0
                      IF(DABS(DIF4).LE.1.0D-7) DIF4=0.0D0
                      IF(WC.EQ.'NFAN') WRITE(OUTLYNE,994)XI,DIF2,DIF1,DIF4,DIF3
                      IF(WC.NE.'NFAN') WRITE(OUTLYNE,994)XI,DIF1,DIF2,DIF3,DIF4
                      CALL SHOWIT(0)
 994                  FORMAT(F9.4,2X,G12.4,2X,G12.4,2X,G12.4,2X,G12.4)
                  ELSE
                      WRITE(OUTLYNE,992)XI,RAYCOD(1),RAYCOD(2)
                      CALL SHOWIT(0)
                  END IF
C
C       NOT CD
              END IF
C
              IF(WQ.EQ.'LA') THEN
                  RAYCOD(1)=0
                  RAYCOD(2)=-1
                  RV=.FALSE.
                  IF(WC.EQ.'YFAN') THEN
                      IF(REAL(WW1).LT.0.001.AND.WW1.GT.0.0D0)WW1=0.01D0
                      IF(REAL(WW1).GT.-0.001.AND.WW1.LT.0.0D0)WW1=-0.01D0
                      IF(REAL(WW1).EQ.0.0) WW1=.0001D0
                  END IF
                  IF(WC.EQ.'XFAN') THEN
                      IF(REAL(WW2).LT.0.001.AND.WW2.GT.0.0D0)WW2=0.001D0
                      IF(REAL(WW2).GT.-0.001.AND.WW2.LT.0.0D0)WW2=0.-001D0
                      IF(REAL(WW2).EQ.0.0) WW2=.0001D0
                  END IF
                  IF(WC.EQ.'NFAN'.OR.WC.EQ.'PFAN') THEN
                      IF(REAL(WW1).LT.0.001.AND.WW1.GT.0.0D0)WW1=0.001D0
                      IF(REAL(WW2).LT.0.001.AND.WW2.GT.0.0D0)WW2=0.001D0
                      IF(REAL(WW1).GT.-0.001.AND.WW1.LT.0.0D0)WW1=-0.001D0
                      IF(REAL(WW2).GT.-0.001.AND.WW2.LT.0.0D0)WW2=-0.001D0
                      IF(REAL(WW1).EQ.0.0) WW1=.0001D0
                      IF(REAL(WW2).EQ.0.0) WW2=.0001D0
                  END IF
                  WW4=1.0D0
                  F58=1
                  NOCOAT=.TRUE.
                  GRASET=.FALSE.
                  DXFSET=.FALSE.
                  CALL RAYTRA
                  F58=0
C       CALCULATION OF LA IN XZ AND YZ PLANE.
                  DX=(RAYRAY(1,NEWIMG)-REFRY(1,NEWIMG))/JB
                  DY=(RAYRAY(2,NEWIMG)-REFRY(2,NEWIMG))/JA
                  LLR=RAYRAY(4,NEWIMG)
                  MMR=RAYRAY(5,NEWIMG)
                  NNR=RAYRAY(6,NEWIMG)
                  LLP=REFRY(4,NEWIMG)
                  MMP=REFRY(5,NEWIMG)
                  NNP=REFRY(6,NEWIMG)
                  IF(WC.NE.'NFAN'.AND.WC.NE.'PFAN') THEN
                      DTX=((LLR/NNR)-(LLP/NNP))
                      DTY=((MMR/NNR)-(MMP/NNP))
                  END IF
                  IF(WC.EQ.'PFAN') THEN
                      TEMP1=(DX*DCOS(PII/4.0D0))+(DY*DSIN(PII/4.0D0))
                      TEMP2=(DY*DCOS(PII/4.0D0))-(DX*DSIN(PII/4.0D0))
                      DX=-TEMP1
                      DY=-TEMP2
                      TEMP3=(LLR*DCOS(PII/4.0D0))+(MMR*DSIN(PII/4.0D0))
                      TEMP4=(MMR*DCOS(PII/4.0D0))-(LLR*DSIN(PII/4.0D0))
                      LLR=TEMP3
                      MMR=TEMP4
                      TEMP3=(LLP*DCOS(PII/4.0D0))+(MMP*DSIN(PII/4.0D0))
                      TEMP4=(MMP*DCOS(PII/4.0D0))-(LLP*DSIN(PII/4.0D0))
                      LLP=TEMP3
                      MMP=TEMP4
                      DTX=((LLR/NNR)-(LLP/NNP))
                      DTY=((MMR/NNR)-(MMP/NNP))
                  END IF
                  IF(WC.EQ.'NFAN') THEN
                      TEMP1=(DX*DCOS(-PII/4.0D0))+(DY*DSIN(-PII/4.0D0))
                      TEMP2=(DY*DCOS(-PII/4.0D0))-(DX*DSIN(-PII/4.0D0))
                      DX=TEMP1
                      DY=TEMP2
                      TEMP3=(LLR*DCOS(-PII/4.0D0))+(MMR*DSIN(-PII/4.0D0))
                      TEMP4=(MMR*DCOS(-PII/4.0D0))-(LLR*DSIN(-PII/4.0D0))
                      LLR=TEMP3
                      MMR=TEMP4
                      TEMP3=(LLP*DCOS(-PII/4.0D0))+(MMP*DSIN(-PII/4.0D0))
                      TEMP4=(MMP*DCOS(-PII/4.0D0))-(LLP*DSIN(-PII/4.0D0))
                      LLP=TEMP3
                      MMP=TEMP4
                      DTX=((LLR/NNR)-(LLP/NNP))
                      DTY=((MMR/NNR)-(MMP/NNP))
                  END IF
                  IF(DX.EQ.0.0D0.OR.DTX.EQ.0.0D0) LAX=0.0D0
                  IF(DTX.NE.0.0D0) LAX=DX/DTX
                  IF(DY.EQ.0.0D0.OR.DTY.EQ.0.0D0) LAY=0.0D0
                  IF(DTY.NE.0.0D0) LAY=DY/DTY
                  IF(RAYCOD(1).EQ.0.0D0) THEN
                      IF(WC.EQ.'NFAN') WRITE(OUTLYNE,995)XI,LAY,LAX,DTY,DTX
                      IF(WC.NE.'NFAN') WRITE(OUTLYNE,995)XI,LAX,LAY,DTX,DTY
                      CALL SHOWIT(0)
 995                  FORMAT(F9.4,2X,G12.4,2X,G12.4,2X,G12.4,2X,G12.4)
                  ELSE
                      WRITE(OUTLYNE,992)XI,RAYCOD(1),RAYCOD(2)
                      CALL SHOWIT(0)
                  END IF
C
C       NOT LA
              END IF
C
              XI=LOWERL+(DBLE(IX)*DELTA)
 100      CONTINUE
C
          IF(.NOT.RIM) FANEXT=.TRUE.
          FANNAM(1:8)=WC(1:8)
          FANQAL(1:8)=WQ(1:8)
C     SET FANTYP AND QALTYP IN PREPARATION FOR USE WITH USER-DEFINED
C     FAN PLOTTING. tHESE WILL BE OVERWRITTEN IS REGULAR FAN PLOTTING
C     IS PERFORMED
          IF(FANNAM.EQ.'YFAN    ') FANTYP=1
          IF(FANNAM.EQ.'XFAN    ') FANTYP=2
          IF(FANNAM.EQ.'NFAN    ') FANTYP=3
          IF(FANNAM.EQ.'PFAN    ') FANTYP=4
          IF(FANQAL.EQ.'        ') QALTYP=0
          IF(FANQAL.EQ.'OPD     ') QALTYP=1
          IF(FANQAL.EQ.'CD      ') QALTYP=2
          IF(FANQAL.EQ.'LA      ') QALTYP=3
C                        PLEXIS=.FALSE.
C                        DEVTYP=0
C                        GRASET=.FALSE.
          RETURN
      END
