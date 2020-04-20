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

C       THIS IS THE SIXTH FILE OF RAYTRACING ROUTINES

C SUB PUPILS
      SUBROUTINE FNUMX(VALVAL,ERRR)
          IMPLICIT NONE
          REAL*8 VALVAL,JAA,JBB,JCC,A1,A2,B1,B2,C1,C2
     1    ,VXHI,VXLO,REFIN
          LOGICAL ERRR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(.NOT.REFEXT) THEN
              ERRR=.TRUE.
              VALVAL=0.0D0
              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  IF(MSG) THEN
                      OUTLYNE='NO REFERENCE RAY EXISTS'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "FOB" COMMAND IS REQUIRED'
                      CALL SHOWIT(1)
                      OUTLYNE='A "GET BFNX" WILL RESULT'
                      CALL SHOWIT(1)
                  END IF
              END IF
              RETURN
          ELSE
C     PROCEED
C     DO VIG CALCS
              VXHI=1.0D0
              VXLO=-1.0D0
              CALL VIGCAL(100,VXLO,VXHI,1)
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=0.0D0
              W2=VXHI
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET BFNX" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
              A1=RAYRAY(4,NEWIMG)
              B1=RAYRAY(5,NEWIMG)
              C1=RAYRAY(6,NEWIMG)
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=0.0D0
              W2=VXLO
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET BFNX" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
              A2=RAYRAY(4,NEWIMG)
              B2=RAYRAY(5,NEWIMG)
              C2=RAYRAY(6,NEWIMG)
              VALVAL=(A1*A2)+(B1*B2)+(C1*C2)
              JAA=DSQRT((A1*A1)+(B1*B1)+(C1*C1))
              JBB=DSQRT((A2*A2)+(B2*B2)+(C2*C2))
              JCC=JAA*JBB
              IF(JCC.NE.0.0D0) VALVAL=DACOS(VALVAL/JCC)/2.0D0
C     VALVAL IS ANGLE INCLUDED BETWEEN RAYS IN RADIANS
C     F-NUMBER IS JUST:
              IF(SYSTEM1(11).GE.1.0D0.AND.SYSTEM1(11).LE.5.0D0) THEN
                  REFIN=ALENS(45+INT(SYSTEM1(11)),NEWIMG-1)
              END IF
              IF(SYSTEM1(11).GE.6.0D0.AND.SYSTEM1(11).LE.10.0D0) THEN
                  REFIN=ALENS(65+INT(SYSTEM1(11)),NEWIMG-1)
              END IF
              VALVAL=1.0D0/(2.0D0*DTAN(VALVAL)*REFIN)
              VALVAL=(RBFNX/DABS(RBFNX))*DABS(VALVAL)
              RETURN
          END IF
      END
      SUBROUTINE FNUMY(VALVAL,ERRR)
          IMPLICIT NONE
          REAL*8 VALVAL,JAA,JBB,JCC,A1,A2,B1,B2,C1,C2
     1    ,VYHI,VYLO,REFIN
          LOGICAL ERRR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(.NOT.REFEXT) THEN
              ERRR=.TRUE.
              VALVAL=0.0D0

              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  IF(MSG) THEN
                      OUTLYNE='NO REFERENCE RAY EXISTS'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "FOB" COMMAND IS REQUIRED'
                      CALL SHOWIT(1)
                      OUTLYNE='A "GET BFNY" WILL RESULT'
                      CALL SHOWIT(1)
                  END IF
              END IF
              RETURN
          ELSE
C     PROCEED
              VYHI=1.0D0
              VYLO=-1.0D0
              CALL VIGCAL(100,VYLO,VYHI,2)
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=VYHI
              W2=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET BFNY" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
              A1=RAYRAY(4,NEWIMG)
              B1=RAYRAY(5,NEWIMG)
              C1=RAYRAY(6,NEWIMG)
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=VYLO
              W2=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET BFNY" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
              A2=RAYRAY(4,NEWIMG)
              B2=RAYRAY(5,NEWIMG)
              C2=RAYRAY(6,NEWIMG)
              VALVAL=(A1*A2)+(B1*B2)+(C1*C2)
              JAA=DSQRT((A1*A1)+(B1*B1)+(C1*C1))
              JBB=DSQRT((A2*A2)+(B2*B2)+(C2*C2))
              JCC=JAA*JBB
              IF(JCC.NE.0.0D0) VALVAL=DACOS(VALVAL/JCC)/2.0D0
C     VALVAL IS ANGLE INCLUDED BETWEEN RAYS IN RADIANS
C     F-NUMBER IS JUST:
              IF(SYSTEM1(11).GE.1.0D0.AND.SYSTEM1(11).LE.5.0D0) THEN
                  REFIN=ALENS(45+INT(SYSTEM1(11)),NEWIMG)
              END IF
              IF(SYSTEM1(11).GE.6.0D0.AND.SYSTEM1(11).LE.10.0D0) THEN
                  REFIN=ALENS(65+INT(SYSTEM1(11)),NEWIMG)
              END IF
              VALVAL=1.0D0/(2.0D0*DTAN(VALVAL)*REFIN)
              VALVAL=(RBFNY/DABS(RBFNY))*DABS(VALVAL)
              RETURN
          END IF
      END
      SUBROUTINE OBFNUMX(VALVAL,ERRR)
          IMPLICIT NONE
          REAL*8 VALVAL,JAA,JBB,JCC,A1,A2,B1,B2,C1,C2
     1    ,VXHI,VXLO,REFIN
          LOGICAL ERRR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(.NOT.REFEXT) THEN
              ERRR=.TRUE.
              VALVAL=0.0D0

              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  IF(MSG) THEN
                      OUTLYNE='NO REFERENCE RAY EXISTS'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "FOB" COMMAND IS REQUIRED'
                      CALL SHOWIT(1)
                      OUTLYNE='A "GET FFNX" WILL RESULT'
                      CALL SHOWIT(1)
                  END IF
              END IF
              RETURN
          ELSE
C     PROCEED
              VXHI=1.0D0
              VXLO=-1.0D0
              CALL VIGCAL(100,VXLO,VXHI,1)
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=0.0D0
              W2=VXHI
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET FFNX" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
              A1=RAYRAY(19,NEWOBJ+1)
              B1=RAYRAY(20,NEWOBJ+1)
              C1=RAYRAY(21,NEWOBJ+1)
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=0.0D0
              W2=VXLO
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET FFNX" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
              A2=RAYRAY(19,NEWOBJ+1)
              B2=RAYRAY(20,NEWOBJ+1)
              C2=RAYRAY(21,NEWOBJ+1)
              VALVAL=(A1*A2)+(B1*B2)+(C1*C2)
              JAA=DSQRT((A1*A1)+(B1*B1)+(C1*C1))
              JBB=DSQRT((A2*A2)+(B2*B2)+(C2*C2))
              JCC=JAA*JBB
              IF(JCC.NE.0.0D0) VALVAL=DACOS(VALVAL/JCC)/2.0D0
C     VALVAL IS ANGLE INCLUDED BETWEEN RAYS IN RADIANS
C     F-NUMBER IS JUST:
              IF(SYSTEM1(11).GE.1.0D0.AND.SYSTEM1(11).LE.5.0D0) THEN
                  REFIN=ALENS(45+INT(SYSTEM1(11)),NEWOBJ)
              END IF
              IF(SYSTEM1(11).GE.6.0D0.AND.SYSTEM1(11).LE.10.0D0) THEN
                  REFIN=ALENS(65+INT(SYSTEM1(11)),NEWOBJ)
              END IF
              VALVAL=1.0D0/(2.0D0*DTAN(VALVAL)*REFIN)
              VALVAL=(RFFNX/DABS(RFFNX))*DABS(VALVAL)
              RETURN
          END IF
      END
      SUBROUTINE OBFNUMY(VALVAL,ERRR)
          IMPLICIT NONE
          REAL*8 VALVAL,JAA,JBB,JCC,A1,A2,B1,B2,C1,C2
     1    ,VYHI,VYLO,REFIN
          LOGICAL ERRR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(.NOT.REFEXT) THEN
              ERRR=.TRUE.
              VALVAL=0.0D0

              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  IF(MSG) THEN
                      OUTLYNE='NO REFERENCE RAY EXISTS'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "FOB" COMMAND IS REQUIRED'
                      CALL SHOWIT(1)
                      OUTLYNE='A "GET FFNY" WILL RESULT'
                      CALL SHOWIT(1)
                  END IF
              END IF
              RETURN
          ELSE
C     PROCEED
              VYHI=1.0D0
              VYLO=-1.0D0
              CALL VIGCAL(100,VYLO,VYHI,2)
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=VYHI
              W2=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET FFNY" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
              A1=RAYRAY(19,NEWOBJ+1)
              B1=RAYRAY(20,NEWOBJ+1)
              C1=RAYRAY(21,NEWOBJ+1)
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=VYLO
              W2=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET FFNY" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
              A2=RAYRAY(19,NEWOBJ+1)
              B2=RAYRAY(20,NEWOBJ+1)
              C2=RAYRAY(21,NEWOBJ+1)
              VALVAL=(A1*A2)+(B1*B2)+(C1*C2)
              JAA=DSQRT((A1*A1)+(B1*B1)+(C1*C1))
              JBB=DSQRT((A2*A2)+(B2*B2)+(C2*C2))
              JCC=JAA*JBB
              IF(JCC.NE.0.0D0) VALVAL=DACOS(VALVAL/JCC)/2.0D0
C     VALVAL IS ANGLE INCLUDED BETWEEN RAYS IN RADIANS
C     F-NUMBER IS JUST:
              IF(SYSTEM1(11).GE.1.0D0.AND.SYSTEM1(11).LE.5.0D0) THEN
                  REFIN=ALENS(45+INT(SYSTEM1(11)),NEWOBJ)
              END IF
              IF(SYSTEM1(11).GE.6.0D0.AND.SYSTEM1(11).LE.10.0D0) THEN
                  REFIN=ALENS(65+INT(SYSTEM1(11)),NEWOBJ)
              END IF
              VALVAL=1.0D0/(2.0D0*DTAN(VALVAL)*REFIN)
              VALVAL=(RFFNY/DABS(RFFNY))*DABS(VALVAL)
              RETURN
          END IF
      END
      SUBROUTINE ENPDIAY(VALVAL,ERRR)
          IMPLICIT NONE
          REAL*8 VALVAL,RX0,RY0,RZ0,RLOLD,RMOLD,RNOLD,X1
     1    ,Y1,Z1,L1,M1,N1,XA,XB,YA,YB,ZA,ZB,T
     1    ,VYHI,VYLO
          LOGICAL ERRR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(.NOT.REFEXT) THEN
              ERRR=.TRUE.
              VALVAL=0.0D0

              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  IF(MSG) THEN
                      OUTLYNE='NO REFERENCE RAY EXISTS'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "FOB" COMMAND IS REQUIRED'
                      CALL SHOWIT(1)
                      OUTLYNE='A "GET ENDIAY" WILL RESULT'
                      CALL SHOWIT(1)
                  END IF
              END IF
              RETURN
          ELSE
C     PROCEED
              VYHI=1.0D0
              VYLO=-1.0D0
              CALL VIGCAL(100,VYLO,VYHI,2)
C     CHIEF RAY DATA AT NEWOBJ+1 IS:
C
              RX0=ENPUX
              RY0=ENPUY
              RZ0=ENPUZ
              RLOLD=REFRY(19,NEWOBJ+1)
              RMOLD=REFRY(20,NEWOBJ+1)
              RNOLD=REFRY(21,NEWOBJ+1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=VYHI
              W2=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET ENDIAY" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
C     THE ABOVE DATA IS FOR THE NON-CHIEF RAY
C     ENTRANCE PUPIL COORDINATES ARE AT:
C     ENPUX,ENPUY AND ENPUZ. THE PLANE GOING THROUGH THIS POINT AND
C     PERPENDICULAR TO THE CHIEF RAY IS EASILY DEFINED. THE INTERSECTION POINT
C     OF THIS NON-CHIEF RAY WITH THIS PLANE IS:
C             XX1,YY1,ZZ1
C       THE EQUATION OF THIS PLANE IS:
C       (X-RX0)*RLOLD + (Y-RY0)*RMOLD +(Z-RZ0)*RNOLD = 0
C       WHERE:
              X1=RAYRAY(1,NEWOBJ+1)
              Y1=RAYRAY(2,NEWOBJ+1)
              Z1=RAYRAY(3,NEWOBJ+1)
C       ITS DIRECTION COSINES ARE
              L1=RAYRAY(19,NEWOBJ+1)
              M1=RAYRAY(20,NEWOBJ+1)
              N1=RAYRAY(21,NEWOBJ+1)
C
C       THE OTHER RAY CAN BE REPRESENTED AS:
C
C       X=X1+(L1*T)
C       Y=Y1+(M1*T)
C       Z=Y1+(N1*T)
C       T IS THE DISTANCE FROM THE NEWOBJ+1 SURFACE INTERSECTION TO THE
C       PLANE INTERSECTION AND IS:
C
              T=((RLOLD*(RX0-X1))+(RMOLD*(RY0-Y1))+(RNOLD*(RZ0-Z1)))/
     1        ((L1*RLOLD)+(M1*RMOLD)+(N1*RNOLD))
C       THE OTHER RAY INTERSECTS THE PLANE AT:
              IF(DABS(T).GE.1.0D20) T=1.0D20
C
              XA=X1+(L1*T)
              YA=Y1+(M1*T)
              ZA=Z1+(N1*T)
C
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=VYLO
              W2=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET ENDIAY" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
C     THE ABOVE DATA IS FOR THE NON-CHIEF RAY
C     ENTRANCE PUPIL COORDINATES ARE AT:
C     ENPUX,ENPUY AND ENPUZ. THE PLANE GOING THROUGH THIS POINT AND
C     PERPENDICULAR TO THE CHIEF RAY IS EASILY DEFINED. THE INTERSECTION POINT
C     OF THIS NON-CHIEF RAY WITH THIS PLANE IS:
C             XX1,YY1,ZZ1
C       THE EQUATION OF THIS PLANE IS:
C       (X-RX0)*RLOLD + (Y-RY0)*RMOLD +(Z-RZ0)*RNOLD = 0
C       WHERE:
              X1=RAYRAY(1,NEWOBJ+1)
              Y1=RAYRAY(2,NEWOBJ+1)
              Z1=RAYRAY(3,NEWOBJ+1)
C       ITS DIRECTION COSINES ARE
              L1=RAYRAY(19,NEWOBJ+1)
              M1=RAYRAY(20,NEWOBJ+1)
              N1=RAYRAY(21,NEWOBJ+1)
C
C       THE OTHER RAY CAN BE REPRESENTED AS:
C
C       X=X1+(L1*T)
C       Y=Y1+(M1*T)
C       Z=Y1+(N1*T)
C       T IS THE DISTANCE FROM THE NEWOBJ+1 SURFACE INTERSECTION TO THE
C       PLANE INTERSECTION AND IS:
C
              T=((RLOLD*(RX0-X1))+(RMOLD*(RY0-Y1))+(RNOLD*(RZ0-Z1)))/
     1        ((L1*RLOLD)+(M1*RMOLD)+(N1*RNOLD))
C       THE OTHER RAY INTERSECTS THE PLANE AT:
              IF(DABS(T).GE.1.0D20) T=1.0D20
C
              XB=X1+(L1*T)
              YB=Y1+(M1*T)
              ZB=Z1+(N1*T)
C
C
C     THE ENTRANCE PUPIL DIAMETER IS JUST
              VALVAL=DSQRT(((XA-XB)**2)+((YA-YB)**2)+((ZA-ZB)**2))
              RETURN
          END IF
      END
      SUBROUTINE EXPDIAX(VALVAL,ERRR)
C     VIGNETTING IS DONE HERE
          IMPLICIT NONE
          REAL*8 VALVAL,RX0,RY0,RZ0,RLOLD,RMOLD,RNOLD,X1
     1    ,Y1,Z1,L1,M1,N1,XA,XB,YA,YB,ZA,ZB,T
     1    ,VXHI,VXLO
          LOGICAL ERRR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(.NOT.REFEXT) THEN
              ERRR=.TRUE.
              VALVAL=0.0D0

              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  IF(MSG) THEN
                      OUTLYNE='NO REFERENCE RAY EXISTS'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "FOB" COMMAND IS REQUIRED'
                      CALL SHOWIT(1)
                      OUTLYNE='A "GET EXDIAX" WILL RESULT'
                      CALL SHOWIT(1)
                  END IF
              END IF
              RETURN
          ELSE
C     PROCEED
              VXHI=1.0D0
              VXLO=-1.0D0
              CALL VIGCAL(100,VXLO,VXHI,1)
C     CHIEF RAY DATA AT NEWIMG IS:
C
              RX0=EXPUX
              RY0=EXPUY
              RZ0=EXPUZ
              RLOLD=REFRY(19,NEWIMG)
              RMOLD=REFRY(20,NEWIMG)
              RNOLD=REFRY(21,NEWIMG)
C
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=0.0D0
              W2=VXHI
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET EXDIAX" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
C     THE ABOVE DATA IS FOR THE NON-CHIEF RAY
C     EXIT PUPIL COORDINATES ARE AT:
C     EXPUX,EXPUY AND EXPUZ. THE PLANE GOING THROUGH THIS POINT AND
C     PERPENDICULAR TO THE CHIEF RAY IS EASILY DEFINED. THE INTERSECTION POINT
C     OF THIS NON-CHIEF RAY WITH THIS PLANE IS:
C             XX1,YY1,ZZ1
C       THE EQUATION OF THIS PLANE IS:
C       (X-RX0)*RLOLD + (Y-RY0)*RMOLD +(Z-RZ0)*RNOLD = 0
C       WHERE:
              X1=RAYRAY(1,NEWIMG)
              Y1=RAYRAY(2,NEWIMG)
              Z1=RAYRAY(3,NEWIMG)
C       ITS DIRECTION COSINES ARE
              L1=RAYRAY(19,NEWIMG)
              M1=RAYRAY(20,NEWIMG)
              N1=RAYRAY(21,NEWIMG)
C
C       THE OTHER RAY CAN BE REPRESENTED AS:
C
C       X=X1+(L1*T)
C       Y=Y1+(M1*T)
C       Z=Y1+(N1*T)
C       T IS THE DISTANCE FROM THE NEWIMG SURFACE INTERSECTION TO THE
C       PLANE INTERSECTION AND IS:
C
              T=((RLOLD*(RX0-X1))+(RMOLD*(RY0-Y1))+(RNOLD*(RZ0-Z1)))/
     1        ((L1*RLOLD)+(M1*RMOLD)+(N1*RNOLD))
C       THE OTHER RAY INTERSECTS THE PLANE AT:
              IF(DABS(T).GE.1.0D20) T=1.0D20
C
              XA=X1+(L1*T)
              YA=Y1+(M1*T)
              ZA=Z1+(N1*T)
C
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=0.0D0
              W2=VXLO
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET EXDIAX" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
C     THE ABOVE DATA IS FOR THE NON-CHIEF RAY
C     EXIT PUPIL COORDINATES ARE AT:
C     EXPUX,EXPUY AND EXPUZ. THE PLANE GOING THROUGH THIS POINT AND
C     PERPENDICULAR TO THE CHIEF RAY IS EASILY DEFINED. THE INTERSECTION POINT
C     OF THIS NON-CHIEF RAY WITH THIS PLANE IS:
C             XX1,YY1,ZZ1
C       THE EQUATION OF THIS PLANE IS:
C       (X-RX0)*RLOLD + (Y-RY0)*RMOLD +(Z-RZ0)*RNOLD = 0
C       WHERE:
              X1=RAYRAY(1,NEWIMG)
              Y1=RAYRAY(2,NEWIMG)
              Z1=RAYRAY(3,NEWIMG)
C       ITS DIRECTION COSINES ARE
              L1=RAYRAY(19,NEWIMG)
              M1=RAYRAY(20,NEWIMG)
              N1=RAYRAY(21,NEWIMG)
C
C       THE OTHER RAY CAN BE REPRESENTED AS:
C
C       X=X1+(L1*T)
C       Y=Y1+(M1*T)
C       Z=Y1+(N1*T)
C       T IS THE DISTANCE FROM THE NEWIMG SURFACE INTERSECTION TO THE
C       PLANE INTERSECTION AND IS:
C
              T=((RLOLD*(RX0-X1))+(RMOLD*(RY0-Y1))+(RNOLD*(RZ0-Z1)))/
     1        ((L1*RLOLD)+(M1*RMOLD)+(N1*RNOLD))
C       THE OTHER RAY INTERSECTS THE PLANE AT:
              IF(DABS(T).GE.1.0D20) T=1.0D20
C
              XB=X1+(L1*T)
              YB=Y1+(M1*T)
              ZB=Z1+(N1*T)
C
C
C     THE EXIT PUPIL DIAMETER IS JUST
              VALVAL=DSQRT(((XA-XB)**2)+((YA-YB)**2)+((ZA-ZB)**2))
              RETURN
          END IF
      END
      SUBROUTINE EXPDIAY(VALVAL,ERRR)
C     VIGNETTING IS DONE HERE
          IMPLICIT NONE
          REAL*8 VALVAL,RX0,RY0,RZ0,RLOLD,RMOLD,RNOLD,X1
     1    ,Y1,Z1,L1,M1,N1,XA,XB,YA,YB,ZA,ZB,T
     1    ,VYHI,VYLO
          LOGICAL ERRR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(.NOT.REFEXT) THEN
              ERRR=.TRUE.
              VALVAL=0.0D0

              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  IF(MSG) THEN
                      OUTLYNE='NO REFERENCE RAY EXISTS'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "FOB" COMMAND IS REQUIRED'
                      CALL SHOWIT(1)
                      OUTLYNE='A "GET EXDIAY" WILL RESULT'
                      CALL SHOWIT(1)
                  END IF
              END IF
              RETURN
          ELSE
C     PROCEED
              VYHI=1.0D0
              VYLO=-1.0D0
              CALL VIGCAL(100,VYLO,VYHI,2)
C     CHIEF RAY DATA AT NEWIMG IS:
C
              RX0=EXPUX
              RY0=EXPUY
              RZ0=EXPUZ
              RLOLD=REFRY(19,NEWIMG)
              RMOLD=REFRY(20,NEWIMG)
              RNOLD=REFRY(21,NEWIMG)
C
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=VYHI
              W2=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET EXDIAY" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
C     THE ABOVE DATA IS FOR THE NON-CHIEF RAY
C     EXIT PUPIL COORDINATES ARE AT:
C     EXPUX,EXPUY AND EXPUZ. THE PLANE GOING THROUGH THIS POINT AND
C     PERPENDICULAR TO THE CHIEF RAY IS EASILY DEFINED. THE INTERSECTION POINT
C     OF THIS NON-CHIEF RAY WITH THIS PLANE IS:
C             XX1,YY1,ZZ1
C       THE EQUATION OF THIS PLANE IS:
C       (X-RX0)*RLOLD + (Y-RY0)*RMOLD +(Z-RZ0)*RNOLD = 0
C       WHERE:
              X1=RAYRAY(1,NEWIMG)
              Y1=RAYRAY(2,NEWIMG)
              Z1=RAYRAY(3,NEWIMG)
C       ITS DIRECTION COSINES ARE
              L1=RAYRAY(19,NEWIMG)
              M1=RAYRAY(20,NEWIMG)
              N1=RAYRAY(21,NEWIMG)
C
C       THE OTHER RAY CAN BE REPRESENTED AS:
C
C       X=X1+(L1*T)
C       Y=Y1+(M1*T)
C       Z=Y1+(N1*T)
C       T IS THE DISTANCE FROM THE NEWIMG SURFACE INTERSECTION TO THE
C       PLANE INTERSECTION AND IS:
C
              T=((RLOLD*(RX0-X1))+(RMOLD*(RY0-Y1))+(RNOLD*(RZ0-Z1)))/
     1        ((L1*RLOLD)+(M1*RMOLD)+(N1*RNOLD))
C       THE OTHER RAY INTERSECTS THE PLANE AT:
              IF(DABS(T).GE.1.0D20) T=1.0D20
C
              XA=X1+(L1*T)
              YA=Y1+(M1*T)
              ZA=Z1+(N1*T)
C
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=VYLO
              W2=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET EXDIAY" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
C     THE ABOVE DATA IS FOR THE NON-CHIEF RAY
C     EXIT PUPIL COORDINATES ARE AT:
C     EXPUX,EXPUY AND EXPUZ. THE PLANE GOING THROUGH THIS POINT AND
C     PERPENDICULAR TO THE CHIEF RAY IS EASILY DEFINED. THE INTERSECTION POINT
C     OF THIS NON-CHIEF RAY WITH THIS PLANE IS:
C             XX1,YY1,ZZ1
C       THE EQUATION OF THIS PLANE IS:
C       (X-RX0)*RLOLD + (Y-RY0)*RMOLD +(Z-RZ0)*RNOLD = 0
C       WHERE:
              X1=RAYRAY(1,NEWIMG)
              Y1=RAYRAY(2,NEWIMG)
              Z1=RAYRAY(3,NEWIMG)
C       ITS DIRECTION COSINES ARE
              L1=RAYRAY(19,NEWIMG)
              M1=RAYRAY(20,NEWIMG)
              N1=RAYRAY(21,NEWIMG)
C
C       THE OTHER RAY CAN BE REPRESENTED AS:
C
C       X=X1+(L1*T)
C       Y=Y1+(M1*T)
C       Z=Y1+(N1*T)
C       T IS THE DISTANCE FROM THE NEWIMG SURFACE INTERSECTION TO THE
C       PLANE INTERSECTION AND IS:
C
              T=((RLOLD*(RX0-X1))+(RMOLD*(RY0-Y1))+(RNOLD*(RZ0-Z1)))/
     1        ((L1*RLOLD)+(M1*RMOLD)+(N1*RNOLD))
C       THE OTHER RAY INTERSECTS THE PLANE AT:
              IF(DABS(T).GE.1.0D20) T=1.0D20
C
              XB=X1+(L1*T)
              YB=Y1+(M1*T)
              ZB=Z1+(N1*T)
C
C
C     THE EXIT PUPIL DIAMETER IS JUST
              VALVAL=DSQRT(((XA-XB)**2)+((YA-YB)**2)+((ZA-ZB)**2))
              RETURN
          END IF
      END
      SUBROUTINE ENPDIAX(VALVAL,ERRR)
          IMPLICIT NONE
          REAL*8 VALVAL,RX0,RY0,RZ0,RLOLD,RMOLD,RNOLD,X1
     1    ,Y1,Z1,L1,M1,N1,XA,XB,YA,YB,ZA,ZB,T
     1    ,VXHI,VXLO
          LOGICAL ERRR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(.NOT.REFEXT) THEN
              ERRR=.TRUE.
              VALVAL=0.0D0

              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  IF(MSG) THEN
                      OUTLYNE='NO REFERENCE RAY EXISTS'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "FOB" COMMAND IS REQUIRED'
                      CALL SHOWIT(1)
                      OUTLYNE='A "GET ENDIAX" WILL RESULT'
                      CALL SHOWIT(1)
                  END IF
              END IF
              RETURN
          ELSE
C     PROCEED
              VXHI=1.0D0
              VXLO=-1.0D0
              CALL VIGCAL(100,VXLO,VXHI,1)
C     CHIEF RAY DATA AT NEWOBJ+1 IS:
C
              RX0=ENPUX
              RY0=ENPUY
              RZ0=ENPUZ
              RLOLD=REFRY(19,NEWOBJ+1)
              RMOLD=REFRY(20,NEWOBJ+1)
              RNOLD=REFRY(21,NEWOBJ+1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=0.0D0
              W2=VXHI
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET ENDIAX" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
C     THE ABOVE DATA IS FOR THE NON-CHIEF RAY
C     ENTRANCE PUPIL COORDINATES ARE AT:
C     ENPUX,ENPUY AND ENPUZ. THE PLANE GOING THROUGH THIS POINT AND
C     PERPENDICULAR TO THE CHIEF RAY IS EASILY DEFINED. THE INTERSECTION POINT
C     OF THIS NON-CHIEF RAY WITH THIS PLANE IS:
C             XX1,YY1,ZZ1
C       THE EQUATION OF THIS PLANE IS:
C       (X-RX0)*RLOLD + (Y-RY0)*RMOLD +(Z-RZ0)*RNOLD = 0
C       WHERE:
              X1=RAYRAY(1,NEWOBJ+1)
              Y1=RAYRAY(2,NEWOBJ+1)
              Z1=RAYRAY(3,NEWOBJ+1)
C       ITS DIRECTION COSINES ARE
              L1=RAYRAY(19,NEWOBJ+1)
              M1=RAYRAY(20,NEWOBJ+1)
              N1=RAYRAY(21,NEWOBJ+1)
C
C       THE OTHER RAY CAN BE REPRESENTED AS:
C
C       X=X1+(L1*T)
C       Y=Y1+(M1*T)
C       Z=Y1+(N1*T)
C       T IS THE DISTANCE FROM THE NEWOBJ+1 SURFACE INTERSECTION TO THE
C       PLANE INTERSECTION AND IS:
C
              T=((RLOLD*(RX0-X1))+(RMOLD*(RY0-Y1))+(RNOLD*(RZ0-Z1)))/
     1        ((L1*RLOLD)+(M1*RMOLD)+(N1*RNOLD))
C       THE OTHER RAY INTERSECTS THE PLANE AT:
              IF(DABS(T).GE.1.0D20) T=1.0D20
C
              XA=X1+(L1*T)
              YA=Y1+(M1*T)
              ZA=Z1+(N1*T)
C
              SAVE_KDP(1)=SAVEINPT(1)
              WQ='        '
              SQ=0
              SST=0
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
              W1=0.0D0
              W2=VXLO
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              REST_KDP(1)=RESTINPT(1)
              IF(.NOT.RAYEXT) THEN
                  ERRR=.TRUE.
                  VALVAL=0.0D0

                  IF(F28.EQ.0.AND.F31.EQ.0) THEN
                      IF(MSG) THEN
                          OUTLYNE='RAY CAN NOT BE TRACED'
                          CALL SHOWIT(1)
                          OUTLYNE='A "GET ENDIAX" WILL RESULT'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  RETURN
              END IF
C     THE ABOVE DATA IS FOR THE NON-CHIEF RAY
C     ENTRANCE PUPIL COORDINATES ARE AT:
C     ENPUX,ENPUY AND ENPUZ. THE PLANE GOING THROUGH THIS POINT AND
C     PERPENDICULAR TO THE CHIEF RAY IS EASILY DEFINED. THE INTERSECTION POINT
C     OF THIS NON-CHIEF RAY WITH THIS PLANE IS:
C             XX1,YY1,ZZ1
C       THE EQUATION OF THIS PLANE IS:
C       (X-RX0)*RLOLD + (Y-RY0)*RMOLD +(Z-RZ0)*RNOLD = 0
C       WHERE:
              X1=RAYRAY(1,NEWOBJ+1)
              Y1=RAYRAY(2,NEWOBJ+1)
              Z1=RAYRAY(3,NEWOBJ+1)
C       ITS DIRECTION COSINES ARE
              L1=RAYRAY(19,NEWOBJ+1)
              M1=RAYRAY(20,NEWOBJ+1)
              N1=RAYRAY(21,NEWOBJ+1)
C
C       THE OTHER RAY CAN BE REPRESENTED AS:
C
C       X=X1+(L1*T)
C       Y=Y1+(M1*T)
C       Z=Y1+(N1*T)
C       T IS THE DISTANCE FROM THE NEWOBJ+1 SURFACE INTERSECTION TO THE
C       PLANE INTERSECTION AND IS:
C
              T=((RLOLD*(RX0-X1))+(RMOLD*(RY0-Y1))+(RNOLD*(RZ0-Z1)))/
     1        ((L1*RLOLD)+(M1*RMOLD)+(N1*RNOLD))
C       THE OTHER RAY INTERSECTS THE PLANE AT:
              IF(DABS(T).GE.1.0D20) T=1.0D20
C
              XB=X1+(L1*T)
              YB=Y1+(M1*T)
              ZB=Z1+(N1*T)
C
C
C     THE ENTRANCE PUPIL DIAMETER IS JUST
              VALVAL=DSQRT(((XA-XB)**2)+((YA-YB)**2)+((ZA-ZB)**2))
              RETURN
          END IF
      END
C SUB AUXFOB.FOR
      SUBROUTINE AUXFOB()
C
          IMPLICIT NONE
C
C     AUXFOB IS CALLED AFTER FFOB TO CALCULATE DIFFERENTIAL RAY BASED
C     FFL,BFL,EFL,FN AND MAG OF THE SYSTEM
C
          REAL*8 RPYO,RPXO,RPCYO,RPCXO,RPUYO,RPUXO,RPUCYO,RPUCXO,
     1    RPYI,RPXI,RPCYI,RPCXI,RPUYI,RPUXI,RPUCYI,RPUCXI,
     1    RPY1,RPX1,RPCY1,RPCX1,RPUY1,RPUX1,RPUCY1,RPUCX1,
     1    RPYR,RPXR,RPCYR,RPCXR,RPUYR,RPUXR,RPUCYR,RPUCXR,
     2    NWN1,NWN2,NWN3,NWN4,JK20,VALUE1,RFX,RFY,RSHIFTX,RSHIFTY
C
          REAL*8 VALUE1_AUXFOB(1:8)
          COMMON/FOBAUX/VALUE1_AUXFOB
C
          INTEGER ERROR,NUM5
C
          COMMON/GV/VALUE1,NUM5
C
!      LOGICAL ERRFOB
C
          COMMON/SHIFTY/RSHIFTX,RSHIFTY
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          JK20=DBLE(NEWIMG-1)
          ERROR=0
          NWN1=0.0D0
          NWN2=0.0D0
          NWN3=0.0D0
          NWN4=SYSTEM1(11)
          CALL GNPR12345678(INT(JK20),NWN1,NWN2,NWN3,NWN4,ERROR,0)
          RPYI=VALUE1_AUXFOB(1)
          RPXI=VALUE1_AUXFOB(2)
          RPUYI=VALUE1_AUXFOB(3)
          RPUXI=VALUE1_AUXFOB(4)
          RPCYI=VALUE1_AUXFOB(5)
          RPCXI=VALUE1_AUXFOB(6)
          RPUCYI=VALUE1_AUXFOB(7)
          RPUCXI=VALUE1_AUXFOB(8)
          JK20=DBLE(NEWOBJ)
          ERROR=0
          NWN1=0.0D0
          NWN2=0.0D0
          NWN3=0.0D0
          NWN4=SYSTEM1(11)
          CALL GNPR12345678(INT(JK20),NWN1,NWN2,NWN3,NWN4,ERROR,0)
          RPYO=VALUE1_AUXFOB(1)
          RPXO=VALUE1_AUXFOB(2)
          RPUYO=VALUE1_AUXFOB(3)
          RPUXO=VALUE1_AUXFOB(4)
          RPCYO=VALUE1_AUXFOB(5)
          RPCXO=VALUE1_AUXFOB(6)
          RPUCYO=VALUE1_AUXFOB(7)
          RPUCXO=VALUE1_AUXFOB(8)
          JK20=1.0
          ERROR=0
          NWN1=0.0D0
          NWN2=0.0D0
          NWN3=0.0D0
          NWN4=SYSTEM1(11)
          CALL GNPR12345678(INT(JK20),NWN1,NWN2,NWN3,NWN4,ERROR,0)
          RPY1=VALUE1_AUXFOB(1)
          RPX1=VALUE1_AUXFOB(2)
          RPUY1=VALUE1_AUXFOB(3)
          RPUX1=VALUE1_AUXFOB(4)
          RPCY1=VALUE1_AUXFOB(5)
          RPCX1=VALUE1_AUXFOB(6)
          RPUCY1=VALUE1_AUXFOB(7)
          RPUCX1=VALUE1_AUXFOB(8)
          JK20=DBLE(NEWREF)
          ERROR=0
          NWN1=0.0D0
          NWN2=0.0D0
          NWN3=0.0D0
          NWN4=SYSTEM1(11)
          CALL GNPR12345678(INT(JK20),NWN1,NWN2,NWN3,NWN4,ERROR,0)
          RPYR=VALUE1_AUXFOB(1)
          RPXR=VALUE1_AUXFOB(2)
          RPUYR=VALUE1_AUXFOB(3)
          RPUXR=VALUE1_AUXFOB(4)
          RPCYR=VALUE1_AUXFOB(5)
          RPCXR=VALUE1_AUXFOB(6)
          RPUCYR=VALUE1_AUXFOB(7)
          RPUCXR=VALUE1_AUXFOB(8)
C       BROUGHT BACK FROM VERSION 2.80 TO RESTORE DIFFERENTIAL
C       RAYTRACE CALCS FOR FIRST ORDER LENS VALUE1S
C
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.
     1    DABS(ALENS(9,NEWREF)).LE.4.0D0) THEN
C
C       CLAP IS ON REFERENCE SURFACE
C
C       CIRCULAR CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  RFY=ALENS(10,NEWREF)
                  RFX=ALENS(10,NEWREF)
C       NOT CIRCULAR CLAP
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).GE.2.0D0.AND.DABS(ALENS(9,NEWREF))
     1        .LE.4.0D0) THEN
                  RFY=ALENS(10,NEWREF)
                  RFX=ALENS(11,NEWREF)
              END IF
          ELSE
              RFX=PXTRAX(1,NEWREF)
              RFY=PXTRAY(1,NEWREF)
          END IF
C
C
          IF(RPUCXR.NE.0.0D0) THEN
              MAGXOR=RPUCXO/RPUCXR
          ELSE
              IF(RPUCXO.GE.0.0D0) MAGXOR=1.0D10
              IF(RPUCXO.LT.0.0D0) MAGXOR=-1.0D10
          END IF
          IF(RPUCYR.NE.0.0D0) THEN
              MAGYOR=RPUCYO/RPUCYR
          ELSE
              IF(RPUCYO.GE.0.0D0) MAGYOR=1.0D10
              IF(RPUCYO.LT.0.0D0) MAGYOR=-1.0D10
          END IF
C     MAGX AND MAGY
          IF(RPUCXO.NE.0.0D0) THEN
              RMAGX=RPUCXI/RPUCXO
          ELSE
              IF(RPUCXI.GE.0.0D0) RMAGX=1.0D10
              IF(RPUCXI.LT.0.0D0) RMAGX=-1.0D10
          END IF
          IF(RPUCYO.NE.0.0D0) THEN
              RMAGY=RPUCYI/RPUCYO
          ELSE
              IF(RPUCYI.GE.0.0D0) RMAGY=1.0D10
              IF(RPUCYI.LT.0.0D0) RMAGY=-1.0D10
          END IF
C     EFLX,EFLY,FFLX,FFLY,BFLX,FFLY
C
          IF(
     1    ((RPUYO*RPUCYI)-(RPUCYO*RPUYI)).NE.0.0D0)
     1    REFLY=-(((RPUYO*RPCY1)-(RPY1*RPUCYO
     1      ))/((RPUYO*RPUCYI)-(RPUCYO*RPUYI)))
C
          IF(
     1    ((RPUXO*RPUCXI)-(RPUCXO*RPUXI)).NE.0.0D0)
     1    REFLX=-(((RPUXO*RPCX1)-(RPX1*RPUCXO
     1      ))/((RPUXO*RPUCXI)-(RPUCXO*RPUXI)))
C
          IF(
     1    ((RPUYO*RPUCYI)-(RPUCYO*RPUYI)).NE.0.0D0)
     1    RBFLY=-(((RPUYO*RPCYI)-(RPUCYO*RPYI))/
     1      ((RPUYO*RPUCYI)-(RPUCYO*RPUYI)))
C
          IF(
     1    ((RPUXO*RPUCXI)-(RPUCXO*RPUXI)).NE.0.0D0)
     1    RBFLX=-(((RPUXO*RPCXI)-(RPUCXO*RPXI))/
     1      ((RPUXO*RPUCXI)-(RPUCXO*RPUXI)))
C
          IF(
     1    ((RPUYO*RPUCYI)-(RPUCYO*RPUYI)).NE.0.0D0)
     1    RFFLY=-(((RPY1*RPUCYI)-(RPUYI*RPCY1
     1      ))/((RPUYO*RPUCYI)-(RPUCYO*RPUYI)))
C
          IF(
     1    ((RPUXO*RPUCXI)-(RPUCXO*RPUXI)).NE.0.0D0)
     1    RFFLX=-(((RPX1*RPUCXI)-(RPUXI*RPCX1
     1      ))/((RPUXO*RPUCXI)-(RPUCXO*RPUXI)))
C
C     FFNX,FFNY,BFNX,BFNY
          IF(RPUXO.NE.0.0D0) THEN
              RFFNX=-0.5D0/(RPUXO)
          ELSE
              IF(RFFLX.GE.0.0D0) RFFNX=1.0D10
              IF(RFFLX.LT.0.0D0) RFFNX=-1.0D10
          END IF
          IF(RPUYO.NE.0.0D0) THEN
              RFFNY=-0.5D0/(RPUYO)
          ELSE
              IF(RFFLY.GE.0.0D0) RFFNY=1.0D10
              IF(RFFLY.LT.0.0D0) RFFNY=-1.0D10
          END IF
          IF(RPUXI.NE.0.0D0) THEN
              RBFNX=-0.5D0/(RPUXI)
          ELSE
              IF(RBFLX.GE.0.0D0) RBFNX=1.0D10
              IF(RBFLX.LT.0.0D0) RBFNX=-1.0D10
          END IF
          IF(RPUYI.NE.0.0D0) THEN
              RBFNY=-0.5D0/(RPUYI)
          ELSE
              IF(RBFLY.GE.0.0D0) RBFNY=1.0D10
              IF(RBFLY.LT.0.0D0) RBFNY=-1.0D10
          END IF
          RETURN
      END


      SUBROUTINE ENERGY_ADJUST(ENERGY_FACTOR,J,IA,IAP,RN1,RN2,WA3,
     1POLANG,FACT_PAR,FACT_PER,PHASE_PAR,PHASE_PER,PATHL)
C     THIS ADJUSTS RAY ENERGY DUE TO COATING LOSSES
C     AND SETS UP ABSORBTION COEFFICIENT DATA FOR PATH TO THE NEXT SURFACE.
          IMPLICIT NONE
          INTEGER J,WA3,K,ALLOERR,NLAY,L
          CHARACTER CNAME*12,AJ4*8,AB*8
          LOGICAL EXISTCF
          REAL*8 ENERGY_FACTOR,FACTOR,IA,IAP,RN1,RN2,EFF,POLANG,PATHL,
     1    ACTIVE_WAVE,T
          REAL*8 NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10
          REAL*8 KK1,KK2,KK3,KK4,KK5,KK6,KK7,KK8,KK9,KK10
          REAL*8 FACT_PAR,FACT_PER,PHASE_PAR,PHASE_PER
          COMPLEX*16 CINDEX,SMALLTPAR,SMALLTPER,SMALLRPAR,SMALLRPER
          DIMENSION CINDEX(:,:)
          CHARACTER*13 LAYERNAME
          DIMENSION LAYERNAME(:)
          COMPLEX*16 THICKNESS,THETAI,THETAIP,INDEXI,INDEXIP
     1    ,INDEXL
          COMPLEX*16 COSTHETAL,ULPAR,ULPER,THICKNESSL
     1    ,ROOTONE
          COMPLEX*16 MPAR,MPER,MPARL,MPERL,CPII,CTWO,CACTIVE_WAVE,DELTAL
          REAL*8 T1,T2,T3,T4,T5,CTEMPR,CTEMPI
          COMPLEX*16 CTEMP
          DIMENSION MPAR(2,2),MPER(2,2),MPARL(2,2),MPERL(2,2)
          DIMENSION THICKNESS(:)
          ALLOCATABLE :: CINDEX,LAYERNAME,THICKNESS
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          LOGICAL TIR
          COMMON/RIT/TIR
          REAL*8 BIGTPAR,BIGTPER,BIGRPAR,BIGRPER
          REAL*8 PHASETPAR,PHASETPER,PHASERPAR,PHASERPER
!      REAL*8 R_BIGTPAR,R_BIGRPAR,R_BIGTPER,R_BIGRPER
!      REAL*8 R_SMALLTPAR,R_SMALLRPAR,R_SMALLTPER,R_SMALLRPER
          FACT_PAR=0.0D0
          FACT_PER=0.0D0
          PHASE_PAR=0.0D0
          PHASE_PER=0.0D0
C
C       COSPOL IS AN IMPORTANT NUMBER. IT IS THE COSINE OF THE ANGLE
C       BETWEEN THE INCIDENT PLANE DIRECTION AND THE Y=PLANE OF THE RAY
C       FOR ON-AXIS IN THE Y-Z PLANE OF A CENTERED SYSTEM, THE ANGLE IS 0
C       FOR ON-AXIS RAYS IN THE X-Z PLANE OF A CENTERED SYSTEM, THE ANGLE IS
C       PII/2
C
C     NOTE THAT IT IS ASSUMED THAT THE IMAGINARY PART OF THE INCIDENT MEDIUM'S
C     REFRACTIVE INDEX IS ALWAYS ZERO.
C     NOTE THAT IT IS ASSUMED THAT THE IMAGINARY PART OF THE SUBSTRATE MEDIUM'S
C     REFRACTIVE INDEX IS ALWAYS ZERO.
C     TIR CARRIES THE TIR STATE OF THE RAY
C     ACTIVE WAVELENGTH IN MICRONS
          IF(WA3.LE.5) ACTIVE_WAVE=SYSTEM1(WA3)
          IF(WA3.GT.5) ACTIVE_WAVE=SYSTEM1(WA3*65)
          CACTIVE_WAVE=DCMPLX(ACTIVE_WAVE,0.0D0)
          FACTOR=0.0D0
C       COMPLEX REPRESENTATION OF i, THE ROOT OF -1
          ROOTONE=DCMPLX(0.0D0,1.0D0)
C       COMPLEX REPRESENTATION OF PI
          CPII=DCMPLX(3.14159265358979323846D0,0.0D0)
C       COMPLEX REPRESENTATION OF 2.0
          CTWO=DCMPLX(2.0D0,0.0D0)
C     I IS THE SURFACE NUMBER
C     J IS THE FILE NAME/NUMBER
C       SET COMPLEX INDECIES AND ANGLES
C       INDEX OF THE INCIDENT MEDIUM
          INDEXI=DCMPLX(RN1,0.0D0)
C       INDEX OF THE SUBSTRATE
          INDEXIP=DCMPLX(RN2,0.0D0)
C       ANGLE OF INCIDENCE
          THETAI=DCMPLX(IA,0.0D0)
C       ANGLE OF REFRACTION,REFLECTION OR DIFFRACTION
          THETAIP=DCMPLX(IAP,0.0D0)
C
          IF(J.LT.1.OR.J.GT.1000) THEN
C     COATING TYPE 0, NO LOSSES, NO COATING FILE WAS ASSIGNED
              ENERGY_FACTOR=1.0D0
              POLANG=0.0D0
C     NO ABSORBTION LOSSES
              RETURN
          END IF
C
C     J REPRESENTS A FILE NUMBER. SEE IF IT EXISTS
          CALL ITOA4(AJ4,J)
          CNAME='COAT'//AJ4
          EXISTCF=.FALSE.
          INQUIRE(FILE=trim(HOME)//CNAME//'.DAT',EXIST=EXISTCF)
          IF(.NOT.EXISTCF) THEN
C     NO COATING FILE FOR THAT COATING NUMBER EXISTS, NO LOSSES
              ENERGY_FACTOR=1.0D0
              POLANG=0.0D0
C     NO ABSORBTION LOSSES
              RETURN
          ELSE
C     COATING FILE EXISTS, CLOSE IT, THEN OPEN IT
              CALL CLOSE_FILE(34,1)
              OPEN(UNIT=34,ACCESS='SEQUENTIAL',BLANK='NULL'
     1          ,FORM='FORMATTED',FILE=trim(HOME)//CNAME//'.DAT'
     2          ,STATUS='UNKNOWN')
          END IF
C       READ THE COATING TYPE
          READ(UNIT=34,FMT=*,ERR=999,END=999) K
          IF(K.NE.1.AND.K.NE.2.AND.K.NE.3.AND.K.NE.4) THEN
              CALL CLOSE_FILE(34,1)
C       NO COATING, NO LOSSES
              ENERGY_FACTOR=1.0D0
              POLANG=0.0D0
C     NO ABSORBTION LOSSES
              RETURN
          END IF

          IF(K.EQ.1) THEN
C     COATING TYPE 1
C     NO LOSSES
              ENERGY_FACTOR=1.0D0
              OLDABSCOEF(1:10)=ABSCOEF(1:10)
              ENERGY_FACTOR=ENERGY_FACTOR*
     1        DEXP(-DABS(OLDABSCOEF(INT(WA3))*PATHL))
C     CHECK FOR ABSORB
C     SET DEFAULT
              ABSCOEF(1:10)=0.0D0
              READ(UNIT=34,FMT=*,ERR=9991,END=9991) AB
              READ(UNIT=34,FMT=*,ERR=9991,END=9991) ABSCOEF(1)
              READ(UNIT=34,FMT=*,ERR=9991,END=9991) ABSCOEF(2)
              READ(UNIT=34,FMT=*,ERR=9991,END=9991) ABSCOEF(3)
              READ(UNIT=34,FMT=*,ERR=9991,END=9991) ABSCOEF(4)
              READ(UNIT=34,FMT=*,ERR=9991,END=9991) ABSCOEF(5)
              READ(UNIT=34,FMT=*,ERR=9991,END=9991) ABSCOEF(6)
              READ(UNIT=34,FMT=*,ERR=9991,END=9991) ABSCOEF(7)
              READ(UNIT=34,FMT=*,ERR=9991,END=9991) ABSCOEF(8)
              READ(UNIT=34,FMT=*,ERR=9991,END=9991) ABSCOEF(9)
              READ(UNIT=34,FMT=*,ERR=9991,END=9991) ABSCOEF(10)
9991          CALL CLOSE_FILE(34,1)
              RETURN
              POLANG=0.0D0
          END IF

          IF(K.EQ.2) THEN
              K=5
C     NO COATING IS THE SAME AS A HALF-WAVE COATING
          END IF

          IF(K.EQ.3) THEN
C     COATING TYPE 3, FRACTIONAL EFFICIENCY COATING
              READ(UNIT=34,FMT=*,ERR=999,END=999) EFF
              ENERGY_FACTOR=EFF
              OLDABSCOEF(1:10)=ABSCOEF(1:10)
              ENERGY_FACTOR=ENERGY_FACTOR*
     1        DEXP(-DABS(OLDABSCOEF(INT(WA3))*PATHL))
C     CHECK FOR ABSORB
C     SET DEFAULT
              ABSCOEF(1:10)=0.0D0
              READ(UNIT=34,FMT=*,ERR=9992,END=9992) AB
              READ(UNIT=34,FMT=*,ERR=9992,END=9992) ABSCOEF(1)
              READ(UNIT=34,FMT=*,ERR=9992,END=9992) ABSCOEF(2)
              READ(UNIT=34,FMT=*,ERR=9992,END=9992) ABSCOEF(3)
              READ(UNIT=34,FMT=*,ERR=9992,END=9992) ABSCOEF(4)
              READ(UNIT=34,FMT=*,ERR=9992,END=9992) ABSCOEF(5)
              READ(UNIT=34,FMT=*,ERR=9992,END=9992) ABSCOEF(6)
              READ(UNIT=34,FMT=*,ERR=9992,END=9992) ABSCOEF(7)
              READ(UNIT=34,FMT=*,ERR=9992,END=9992) ABSCOEF(8)
              READ(UNIT=34,FMT=*,ERR=9992,END=9992) ABSCOEF(9)
              READ(UNIT=34,FMT=*,ERR=9992,END=9992) ABSCOEF(10)
9992          CALL CLOSE_FILE(34,1)
C     FIX FOR TIR
              IF(TIR) ENERGY_FACTOR=1.0D0
              CALL CLOSE_FILE(34,1)
              POLANG=0.0D0
              RETURN
          END IF
C

          IF(K.EQ.4.OR.K.EQ.5) THEN
C     COATING TYPE 4
              ALLOCATE (CINDEX(1:100,1:10),LAYERNAME(1:100),THICKNESS(1:100),
     1        STAT=ALLOERR)
C     A MULTILAYER COATING, ASSUMING NO-COHERENCE
C     IF RN1 AND RN2 HAVE OPPOSITE SIGNS THEN WE HAVE A REFLECTION
C     SO WE USE THE R VALUE1
C     IF RN1 AND RN2 HAVE THE SAME SIGNS THEN WE HAVE A REFRACTION
C     SO WE USE THE T VALUE1
              NLAY=0
              IF(K.EQ.4) THEN
                  DO L=1,100
                      READ(UNIT=34,FMT=*,ERR=888,END=888)LAYERNAME(L),
     1                NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     2                KK1,KK2,KK3,KK4,KK5,KK6,KK7,KK8,KK9,KK10,T
                      NLAY=NLAY+1
                      CINDEX(L,1)=DCMPLX(NN1,KK1)
                      CINDEX(L,2)=DCMPLX(NN2,KK2)
                      CINDEX(L,3)=DCMPLX(NN3,KK3)
                      CINDEX(L,4)=DCMPLX(NN4,KK4)
                      CINDEX(L,5)=DCMPLX(NN5,KK5)
                      CINDEX(L,6)=DCMPLX(NN6,KK6)
                      CINDEX(L,7)=DCMPLX(NN7,KK7)
                      CINDEX(L,8)=DCMPLX(NN8,KK8)
                      CINDEX(L,9)=DCMPLX(NN9,KK9)
                      CINDEX(L,10)=DCMPLX(NN10,KK10)
                      THICKNESS(L)=DCMPLX(T,0.0D0)
                  END DO
                  OLDABSCOEF(1:10)=ABSCOEF(1:10)
                  ENERGY_FACTOR=ENERGY_FACTOR*
     1            DEXP(-DABS(OLDABSCOEF(INT(WA3))*PATHL))
C     CHECK FOR ABSORB
C     SET DEFAULT
                  ABSCOEF(1:10)=0.0D0
                  READ(UNIT=34,FMT=*,ERR=9993,END=9993) AB
                  READ(UNIT=34,FMT=*,ERR=9993,END=9993) ABSCOEF(1)
                  READ(UNIT=34,FMT=*,ERR=9993,END=9993) ABSCOEF(2)
                  READ(UNIT=34,FMT=*,ERR=9993,END=9993) ABSCOEF(3)
                  READ(UNIT=34,FMT=*,ERR=9993,END=9993) ABSCOEF(4)
                  READ(UNIT=34,FMT=*,ERR=9993,END=9993) ABSCOEF(5)
                  READ(UNIT=34,FMT=*,ERR=9993,END=9993) ABSCOEF(6)
                  READ(UNIT=34,FMT=*,ERR=9993,END=9993) ABSCOEF(7)
                  READ(UNIT=34,FMT=*,ERR=9993,END=9993) ABSCOEF(8)
                  READ(UNIT=34,FMT=*,ERR=9993,END=9993) ABSCOEF(9)
                  READ(UNIT=34,FMT=*,ERR=9993,END=9993) ABSCOEF(10)
9993              CALL CLOSE_FILE(34,1)
              END IF
              IF(K.EQ.5) THEN
                  NLAY=1
              END IF
 888          CONTINUE
C     NLAY AT LEAST 1, CONTINUE
C
              DO L=1,NLAY
C     CONSTRUCT THE 2X2 L-LAYER MATRIX
C     INDEXL IS THE COMPLEX INDEX AT THE TRACED WAVELENGTH FOR THE L-TH LAYER
                  IF(K.EQ.4) INDEXL=CINDEX(L,WA3)
                  IF(K.EQ.5) INDEXL=INDEXIP
C       T1 IS THE REAL PART OF THE LAYER INDEX
                  T1=DBLE(INDEXL)
C       T2 IS THE IMAGINARY PART OF THE LAYER INDEX
                  T2=DIMAG(INDEXL)
C       T3 IS THE REAL PART OF THE LAYER THICKNESS
                  IF(K.EQ.4) T3=DBLE(THICKNESSL)
                  IF(K.EQ.5) T3=DBLE(0.50D0*CACTIVE_WAVE)
C       T4 IS THE REAL PART OF THE INCIDENT MEDIA INDEX
                  T4=DBLE(INDEXI)
C       T5 IS THE REAL PART OF THE SINE OF THE INCIDENT ANGLE
                  T5=DSIN(IA)
C
                  CTEMPR=(T1**2)-(T2**2)-((T4**2)*(DSIN(IA)**2))
                  CTEMPI=2.0D0*T1*T2
                  CTEMP=DCMPLX(CTEMPR,CTEMPI)
                  CTEMP=CDSQRT(CTEMP)
                  COSTHETAL=CTEMP/INDEXL
C     THICKNESS OF L IN MICRONS IS:
                  IF(K.EQ.4) THICKNESSL=THICKNESS(L)
                  IF(K.EQ.5) THICKNESSL=DCMPLX(T3)
C     COS(PHIL)*THE COMPLEX INDEX OF THE LAYER IS JUST:
C     U SUB L PARALLEL IS:
                  ULPAR=INDEXL/COSTHETAL
C     U SUB L PERPENDICULAR IS:
                  ULPER=INDEXL*COSTHETAL
C     DELTAL IS:
                  DELTAL=(CTWO*CPII/CACTIVE_WAVE)*(INDEXL*THICKNESSL*COSTHETAL)
C     THE LAYER MATRIX FOR PARALLEL POLARIZATION IS:
                  MPARL(1,1)=CDCOS(DELTAL)
                  MPARL(1,2)=-(ROOTONE/ULPAR)*CDSIN(DELTAL)
                  MPARL(2,1)=-ROOTONE*ULPAR*CDSIN(DELTAL)
                  MPARL(2,2)=MPARL(1,1)
C     THE LAYER MATRIX FOR PERPENDICULAR POLARIZATION IS:
                  MPERL(1,1)=CDCOS(DELTAL)
                  MPERL(1,2)=-(ROOTONE/ULPER)*CDSIN(DELTAL)
                  MPERL(2,1)=-ROOTONE*ULPER*CDSIN(DELTAL)
                  MPERL(2,2)=MPERL(1,1)
C     MULTIPLY THE CURRENT L-LAYER M-MATRIX INTO THE FINAL M-MATRIX
                  IF(L.EQ.1) THEN
                      MPAR(1,1)=MPARL(1,1)
                      MPAR(1,2)=MPARL(1,2)
                      MPAR(2,1)=MPARL(2,1)
                      MPAR(2,2)=MPARL(2,2)
                      MPER(1,1)=MPERL(1,1)
                      MPER(1,2)=MPERL(1,2)
                      MPER(2,1)=MPERL(2,1)
                      MPER(2,2)=MPERL(2,2)
                  ELSE
C     L GREATER THAN 1
                      MPAR=MATMUL(MPARL,MPAR)
                      MPER=MATMUL(MPERL,MPER)
                  END IF
              END DO
C
C       NOW COMPUTE THE AMPLITUDE TRANSMITTANCE AND REFLECTANCE
C       COEFFICIENTS SMALLTPAR, SMALLTPER, SMALLRPAR AND SMALLRPER
              SMALLRPAR=((MPAR(1,1)*INDEXI)
     1        +(MPAR(1,2)*INDEXI*INDEXIP)
     2        -(MPAR(2,1))
     3        -(MPAR(2,2)*INDEXIP))/
     4         ((MPAR(1,1)*INDEXI)
     5        +(MPAR(1,2)*INDEXI*INDEXIP)
     6        +(MPAR(2,1))
     7        +(MPAR(2,2)*INDEXIP))
              SMALLRPER=((MPER(1,1)*INDEXI)
     1        +(MPER(1,2)*INDEXI*INDEXIP)
     2        -(MPER(2,1))
     3        -(MPER(2,2)*INDEXIP))/
     4         ((MPER(1,1)*INDEXI)
     5        +(MPER(1,2)*INDEXI*INDEXIP)
     6        +(MPER(2,1))
     7        +(MPER(2,2)*INDEXIP))
              SMALLTPAR=(CTWO*INDEXI)/
     1         ((MPAR(1,1)*INDEXI)
     2        +(MPAR(1,2)*INDEXI*INDEXIP)
     3        +(MPAR(2,1))
     4        +(MPAR(2,2)*INDEXIP))
              SMALLTPER=(CTWO*INDEXI)/
     1         ((MPER(1,1)*INDEXI)
     2        +(MPER(1,2)*INDEXI*INDEXIP)
     3        +(MPER(2,1))
     4        +(MPER(2,2)*INDEXIP))
C
C       THE INTENSITY TRANSMISSION AND REFLECTION COEFFICIENTS ARE:
              BIGTPAR=CDABS(INDEXIP/INDEXI)*((CDABS(SMALLTPAR))**2)
              BIGTPER=CDABS(INDEXIP/INDEXI)*((CDABS(SMALLTPER))**2)
              BIGRPAR=CDABS(SMALLRPAR)**2
              BIGRPER=CDABS(SMALLRPER)**2
              PHASETPAR=DATAN2(DBLE(SMALLTPAR),DIMAG(SMALLTPAR))
              PHASETPER=DATAN2(DBLE(SMALLTPER),DIMAG(SMALLTPER))
              PHASERPAR=DATAN2(DBLE(SMALLRPAR),DIMAG(SMALLRPAR))
              PHASERPER=DATAN2(DBLE(SMALLRPER),DIMAG(SMALLRPER))
C       NOW POLANG IS ANGLE BETWEEN THE PLANE OF INCIDENCE AND THE
C       PLANE FORMED BY THE "Y" RAY AND THE "Z" RAY VECTORS
C       THE T AND R COMPONENTS IN THE "Y" DIRECTION ARE:
              IF(RN1.GT.0.0D0.AND.RN2.GT.0.0D0.OR.RN1.LT.0.0D0.AND.RN2.LT.
     1        0.0D0) THEN
C     REFRACTION
                  OLDABSCOEF(1:10)=ABSCOEF(1:10)
                  ENERGY_FACTOR=(BIGTPAR+BIGTPER)/2.0D0
                  ENERGY_FACTOR=ENERGY_FACTOR*
     1            DEXP(-DABS(OLDABSCOEF(INT(WA3))*PATHL))
                  FACT_PAR=BIGTPAR
                  FACT_PER=BIGTPER
                  PHASE_PAR=PHASETPAR
                  PHASE_PER=PHASETPER
C     CHECK FOR ABSORB
C     SET DEFAULT
                  ABSCOEF(1:10)=0.0D0
                  READ(UNIT=34,FMT=*,ERR=9994,END=9994) AB
                  READ(UNIT=34,FMT=*,ERR=9994,END=9994) ABSCOEF(1)
                  READ(UNIT=34,FMT=*,ERR=9994,END=9994) ABSCOEF(2)
                  READ(UNIT=34,FMT=*,ERR=9994,END=9994) ABSCOEF(3)
                  READ(UNIT=34,FMT=*,ERR=9994,END=9994) ABSCOEF(4)
                  READ(UNIT=34,FMT=*,ERR=9994,END=9994) ABSCOEF(5)
                  READ(UNIT=34,FMT=*,ERR=9994,END=9994) ABSCOEF(6)
                  READ(UNIT=34,FMT=*,ERR=9994,END=9994) ABSCOEF(7)
                  READ(UNIT=34,FMT=*,ERR=9994,END=9994) ABSCOEF(8)
                  READ(UNIT=34,FMT=*,ERR=9994,END=9994) ABSCOEF(9)
                  READ(UNIT=34,FMT=*,ERR=9994,END=9994) ABSCOEF(10)
9994              CALL CLOSE_FILE(34,1)
              ELSE

C     REFLECTION
                  ENERGY_FACTOR=(BIGRPAR+BIGRPER)/2.0D0
                  OLDABSCOEF(1:10)=ABSCOEF(1:10)
                  ENERGY_FACTOR=ENERGY_FACTOR*
     1            DEXP(-DABS(OLDABSCOEF(INT(WA3))*PATHL))
                  FACT_PAR=BIGRPAR
                  FACT_PER=BIGRPER
                  PHASE_PAR=PHASERPAR
                  PHASE_PER=PHASERPER
C     CHECK FOR ABSORB
C     SET DEFAULT
                  ABSCOEF(1:10)=0.0D0
                  READ(UNIT=34,FMT=*,ERR=9995,END=9995) AB
                  READ(UNIT=34,FMT=*,ERR=9995,END=9995) ABSCOEF(1)
                  READ(UNIT=34,FMT=*,ERR=9995,END=9995) ABSCOEF(2)
                  READ(UNIT=34,FMT=*,ERR=9995,END=9995) ABSCOEF(3)
                  READ(UNIT=34,FMT=*,ERR=9995,END=9995) ABSCOEF(4)
                  READ(UNIT=34,FMT=*,ERR=9995,END=9995) ABSCOEF(5)
                  READ(UNIT=34,FMT=*,ERR=9995,END=9995) ABSCOEF(6)
                  READ(UNIT=34,FMT=*,ERR=9995,END=9995) ABSCOEF(7)
                  READ(UNIT=34,FMT=*,ERR=9995,END=9995) ABSCOEF(8)
                  READ(UNIT=34,FMT=*,ERR=9995,END=9995) ABSCOEF(9)
                  READ(UNIT=34,FMT=*,ERR=9995,END=9995) ABSCOEF(10)
9995              CALL CLOSE_FILE(34,1)
                  IF(TIR) ENERGY_FACTOR=1.0D0
                  IF(TIR) THEN
                      PHASE_PAR=-PII/2.0D0
                      PHASE_PER=-PII/2.0D0
                      FACT_PAR=1.0D0
                      FACT_PER=1.0D0
                      POLANG=0.0D0
                  END IF
              END IF
              DEALLOCATE (CINDEX,LAYERNAME,THICKNESS,STAT=ALLOERR)
              POLEXT=.TRUE.
              RETURN
          END IF
C**********************************************************************
 999      ENERGY_FACTOR=1.0D0
          POLANG=0.0D0
          CALL CLOSE_FILE(34,1)
          DEALLOCATE (CINDEX,LAYERNAME,THICKNESS,STAT=ALLOERR)
C       FILE READ ERROR
C**********************************************************************
          RETURN
      END



      SUBROUTINE DIFFRACTION_EFFICIENCY(ENERGY_FACTOR,I,IA,WA3)
          IMPLICIT NONE
          INTEGER I,WA3
          REAL*8 ENERGY_FACTOR,IA,A,WAV,WAVB,M,B1,B2,B3
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C     M IS ORDER
          M=ALENS(97,I)
          IF(M.EQ.0.0D0) THEN
              ENERGY_FACTOR=1.0D0
              RETURN
          END IF

C     A IS LINE SPACING IN MICRONS
          IF(SYSTEM1(6).EQ.1.0D0) A=ALENS(98,I)*25.4D0*1.0D3
          IF(SYSTEM1(6).EQ.2.0D0) A=ALENS(98,I)*10.0D0*1.0D3
          IF(SYSTEM1(6).EQ.3.0D0) A=ALENS(98,I)*1.0D3
          IF(SYSTEM1(6).EQ.4.0D0) A=ALENS(98,I)*1.0D6
C     WAVB IS THE REFERENCE WAVELENGTH (BLAZE WAVELEGTH)
          IF(INT(SYSTEM1(11)).EQ.1)  WAVB=SYSTEM1(1)
          IF(INT(SYSTEM1(11)).EQ.2)  WAVB=SYSTEM1(2)
          IF(INT(SYSTEM1(11)).EQ.3)  WAVB=SYSTEM1(3)
          IF(INT(SYSTEM1(11)).EQ.4)  WAVB=SYSTEM1(4)
          IF(INT(SYSTEM1(11)).EQ.5)  WAVB=SYSTEM1(5)
          IF(INT(SYSTEM1(11)).EQ.6)  WAVB=SYSTEM1(71)
          IF(INT(SYSTEM1(11)).EQ.7)  WAVB=SYSTEM1(72)
          IF(INT(SYSTEM1(11)).EQ.8)  WAVB=SYSTEM1(73)
          IF(INT(SYSTEM1(11)).EQ.9)  WAVB=SYSTEM1(74)
          IF(INT(SYSTEM1(11)).EQ.10) WAVB=SYSTEM1(75)
C     WAV IS THE CURRENT TRACE WAVELENGTH
          IF(WA3.EQ.1)   WAV=SYSTEM1(1)
          IF(WA3.EQ.2)   WAV=SYSTEM1(2)
          IF(WA3.EQ.3)   WAV=SYSTEM1(3)
          IF(WA3.EQ.4)   WAV=SYSTEM1(4)
          IF(WA3.EQ.5)   WAV=SYSTEM1(5)
          IF(WA3.EQ.6)   WAV=SYSTEM1(71)
          IF(WA3.EQ.7)   WAV=SYSTEM1(72)
          IF(WA3.EQ.8)   WAV=SYSTEM1(73)
          IF(WA3.EQ.9)   WAV=SYSTEM1(74)
          IF(WA3.EQ.10)  WAV=SYSTEM1(75)
          B1=DASIN((M*WAV/A)-DSIN(IA))
          B2=DASIN((M*WAVB/A)-DSIN(IA))
          B3=(PII*A/WAV)*(B1-B2)
          IF((B3**2).EQ.0.0D0) THEN
              ENERGY_FACTOR=1.3D0
          ELSE
              ENERGY_FACTOR=1.3D0*(DSIN(B3)**2)/(B3**2)
          END IF
          RETURN
      END



C SUB ENPU.FOR
      SUBROUTINE ENPU(PX,PY,PZ,DIAX,DIAY,DIST)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE ENPU.FOR.
C       IT CALCULATES THE X,Y AND Z COORDINIATES
C       OF THE REAL ENTRANCE PUPIL IN THE COORDINATE SYSTEM OF THE
C       NEWOBJ+1 SURFACE
C
          REAL*8 XO,XOOY,XOOX,YO,YOOX,YOOY,ZO,ZOOX,ZOOY,
     1    LO,LOOX,LOOY,MO,MOOX,MOOY,RSHIFTY,RSHIFTX,
     2    NO,NOOX,NOOY,THYNUM,THXNUM,RFX,RFY,
     3    THXDEN,THYDEN,
     4    LOO,MOO,NOO,PX,PY,PZ,DIST,DIAX,DIAY,XRX,XRY,YRX,YRY
C
          REAL*8 X11X,Y11X,Z11X,L11X,M11X,N11X,X11Y,Y11Y,Z11Y,
     1    L11Y,M11Y,N11Y
C
          COMMON/SHIFTY/RSHIFTX,RSHIFTY
C
          INCLUDE 'datlen.inc'
C
C       DO A REAL RAY INTERNAL EXIT PUPIL ADJUSTMENT
C
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
                      RFY=ALENS(10,NEWREF)
                      RFX=ALENS(10,NEWREF)
                  ELSE
                      RFY=ALENS(11,NEWREF)
                      RFX=ALENS(11,NEWREF)
                  END IF
C       NOT CIRCULAR CLAP
              END IF
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  RFY=ALENS(10,NEWREF)
                  RFX=ALENS(10,NEWREF)
C       NOT POLY CLAP
              END IF
              IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                  RFY=ALENS(14,NEWREF)
                  RFX=ALENS(14,NEWREF)
C       NOT IPOLY CLAP
              END IF
C
              IF(DABS(ALENS(9,NEWREF)).GE.2.0D0.AND.DABS(ALENS(9,NEWREF))
     1        .LE.4.0D0) THEN
                  RFY=ALENS(10,NEWREF)
                  RFX=ALENS(11,NEWREF)
              END IF
          ELSE
              RFX=PXTRAX(1,NEWREF)
              RFY=PXTRAY(1,NEWREF)
          END IF
C
C
          XO=REFRY(1,NEWOBJ+1)
          YO=REFRY(2,NEWOBJ+1)
          ZO=REFRY(3,NEWOBJ+1)
          LO=REFRY(19,NEWOBJ+1)
          MO=REFRY(20,NEWOBJ+1)
          NO=REFRY(21,NEWOBJ+1)
C
          XOOX=RFDIFF(1,NEWOBJ+1)
          YOOX=RFDIFF(2,NEWOBJ+1)
          ZOOX=RFDIFF(3,NEWOBJ+1)
          LOOX=RFDIFF(13,NEWOBJ+1)
          MOOX=RFDIFF(14,NEWOBJ+1)
          NOOX=RFDIFF(15,NEWOBJ+1)
C
          XOOY=RFDIFF(7,NEWOBJ+1)
          YOOY=RFDIFF(8,NEWOBJ+1)
          ZOOY=RFDIFF(9,NEWOBJ+1)
          LOOY=RFDIFF(16,NEWOBJ+1)
          MOOY=RFDIFF(17,NEWOBJ+1)
          NOOY=RFDIFF(18,NEWOBJ+1)
C
          X11X=DIFF(1,NEWOBJ+1)
          Y11X=DIFF(2,NEWOBJ+1)
          Z11X=DIFF(3,NEWOBJ+1)
          L11X=DIFF(13,NEWOBJ+1)
          M11X=DIFF(14,NEWOBJ+1)
          N11X=DIFF(15,NEWOBJ+1)
C
          X11Y=DIFF(7,NEWOBJ+1)
          Y11Y=DIFF(8,NEWOBJ+1)
          Z11Y=DIFF(9,NEWOBJ+1)
          L11Y=DIFF(16,NEWOBJ+1)
          M11Y=DIFF(17,NEWOBJ+1)
          N11Y=DIFF(18,NEWOBJ+1)
C
          LOO=REFRY(19,NEWOBJ+1)
          MOO=REFRY(20,NEWOBJ+1)
          NOO=REFRY(21,NEWOBJ+1)
          THXNUM=((XO-XOOX)*(LO-LOOX))+((YO-YOOX)*(MO-MOOX))
     1    +((ZO-ZOOX)*(NO-NOOX))
          THYNUM=((XO-XOOY)*(LO-LOOY))+((YO-YOOY)*(MO-MOOY))
     1    +((ZO-ZOOY)*(NO-NOOY))
          THXDEN=((LO-LOOX)**2)+((MO-MOOX)**2)
     1    +((NO-NOOX)**2)
          THYDEN=((LO-LOOY)**2)+((MO-MOOY)**2)
     1    +((NO-NOOY)**2)
          IF(DABS(THXNUM*1.0D-10).LT.DABS(THXDEN).AND.
     1    DABS(THYNUM*1.0D-10).LT.
     1    DABS(THYDEN)) THEN
              DIST=-((-THXNUM/THXDEN)+(-THYNUM/THYDEN))/2.0D0
C     THE EXIT PUPIL IS LOCATED AT:
C     IN THE COORDINATE SYSTEM OF THE NEWOBJ SURFACE
              PX=XO+(-DIST*LOO)
              PY=YO+(-DIST*MOO)
              PZ=ZO+(-DIST*NOO)
              XRX=X11X-(DIST*L11X)
              YRX=Y11X-(DIST*M11X)
              XRY=X11Y-(DIST*L11Y)
              YRY=Y11Y-(DIST*M11Y)
              DIAX=DABS(((2.0D0*DSQRT((XRX**2)+(YRX**2)))/RSHIFTY)*RFX)
              DIAY=DABS(((2.0D0*DSQRT((XRY**2)+(YRY**2)))/RSHIFTX)*RFY)
              RETURN
          END IF
          IF(DABS(THXNUM*1.0D-10).GE.DABS(THXDEN).OR.
     1    DABS(THYNUM*1.0D-10).GE.
     1    DABS(THYDEN)) THEN
C       REF RAY IS TELECENTRIC, SET DIST TO INFINITY
              DIST=1.0D10
              PX=XO+(-DIST*LOO)
              PY=YO+(-DIST*MOO)
              PZ=ZO+(-DIST*NOO)
              XRX=X11X-(DIST*L11X)
              YRX=Y11X-(DIST*M11X)
              XRY=X11Y-(DIST*L11Y)
              YRY=Y11Y-(DIST*M11Y)
              DIAX=DABS(((2.0D0*DSQRT((XRX**2)+(YRX**2)))/RSHIFTY)*RFX)
              DIAY=DABS(((2.0D0*DSQRT((XRY**2)+(YRY**2)))/RSHIFTX)*RFY)
              RETURN
          END IF
          RETURN
      END
C SUB EXPU.FOR
      SUBROUTINE EXPU(PX,PY,PZ,DIAX,DIAY,DIST)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE EXPU.FOR.
C       IT CALCULATES THE X,Y AND Z COORDINIATES
C       OF THE REAL EXIT PUPIL IN THE COORDINATE SYSTEM OF THE
C       NEWIMG SURFACE
C
          REAL*8 XO,XOOY,XOOX,YO,YOOX,YOOY,ZO,ZOOX,ZOOY,
     1    LO,LOOX,LOOY,MO,MOOX,MOOY,DIAX,DIAY,RSHIFTY,RSHIFTX,
     2    NO,NOOX,NOOY,THYNUM,THXNUM,
     3    THXDEN,THYDEN,
     4    LOO,MOO,NOO,PX,PY,PZ,DIST,XRX,XRY,YRX,YRY,RFX,RFY
C
          COMMON/SHIFTY/RSHIFTX,RSHIFTY
C
          REAL*8 X11X,Y11X,Z11X,L11X,M11X,N11X,X11Y,Y11Y,Z11Y,
     1    L11Y,M11Y,N11Y
C
          INCLUDE 'datlen.inc'
C
C       DO A REAL RAY INTERNAL EXIT PUPIL ADJUSTMENT
C
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
                      RFY=ALENS(10,NEWREF)
                      RFX=ALENS(10,NEWREF)
                  ELSE
                      RFY=ALENS(11,NEWREF)
                      RFX=ALENS(11,NEWREF)
                  END IF
C       NOT CIRCULAR CLAP
              END IF
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  RFY=ALENS(10,NEWREF)
                  RFX=ALENS(10,NEWREF)
C       NOT POLY CLAP
              END IF
              IF(DABS(ALENS(9,NEWREF)).EQ.6.0D0) THEN
                  RFY=ALENS(14,NEWREF)
                  RFX=ALENS(14,NEWREF)
C       NOT IPOLY CLAP
              END IF
C
              IF(DABS(ALENS(9,NEWREF)).GE.2.0D0.AND.DABS(ALENS(9,NEWREF))
     1        .LE.4.0D0) THEN
                  RFY=ALENS(10,NEWREF)
                  RFX=ALENS(11,NEWREF)
              END IF
          ELSE
              RFX=PXTRAX(1,NEWREF)
              RFY=PXTRAY(1,NEWREF)
          END IF
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
          X11X=DIFF(1,NEWIMG)
          Y11X=DIFF(2,NEWIMG)
          Z11X=DIFF(3,NEWIMG)
          L11X=DIFF(13,NEWIMG)
          M11X=DIFF(14,NEWIMG)
          N11X=DIFF(15,NEWIMG)
C
          X11Y=DIFF(7,NEWIMG)
          Y11Y=DIFF(8,NEWIMG)
          Z11Y=DIFF(9,NEWIMG)
          L11Y=DIFF(16,NEWIMG)
          M11Y=DIFF(17,NEWIMG)
          N11Y=DIFF(18,NEWIMG)
C
          LOO=REFRY(19,NEWIMG)
          MOO=REFRY(20,NEWIMG)
          NOO=REFRY(21,NEWIMG)
          THXNUM=((XO-XOOX)*(LO-LOOX))+((YO-YOOX)*(MO-MOOX))
     1    +((ZO-ZOOX)*(NO-NOOX))
          THYNUM=((XO-XOOY)*(LO-LOOY))+((YO-YOOY)*(MO-MOOY))
     1    +((ZO-ZOOY)*(NO-NOOY))
          THXDEN=((LO-LOOX)**2)+((MO-MOOX)**2)
     1    +((NO-NOOX)**2)
          THYDEN=((LO-LOOY)**2)+((MO-MOOY)**2)
     1    +((NO-NOOY)**2)
          IF(DABS(THXNUM*1.0D-10).LT.DABS(THXDEN).AND.
     1    DABS(THYNUM*1.0D-10).LT.
     1    DABS(THYDEN)) THEN
              DIST=-((-THXNUM/THXDEN)+(-THYNUM/THYDEN))/2.0D0
C     THE EXIT PUPIL IS LOCATED AT:
C     IN THE COORDINATE SYSTEM OF THE IMAGE SURFACE
              PX=XO+(-DIST*LOO)
              PY=YO+(-DIST*MOO)
              PZ=ZO+(-DIST*NOO)
              XRX=X11X-(DIST*L11X)
              YRX=Y11X-(DIST*M11X)
              XRY=X11Y-(DIST*L11Y)
              YRY=Y11Y-(DIST*M11Y)
              DIAX=DABS(((2.0D0*DSQRT((XRX**2)+(YRX**2)))/RSHIFTY)*RFX)
              DIAY=DABS(((2.0D0*DSQRT((XRY**2)+(YRY**2)))/RSHIFTX)*RFY)
              RETURN
          END IF
          IF(DABS(THXNUM*1.0D-10).GE.DABS(THXDEN).OR.
     1    DABS(THYNUM*1.0D-10).GE.
     1    DABS(THYDEN)) THEN
C       REF RAY IS TELECENTRIC, SET DIST TO INFINITY
              DIST=1.0D10
              PX=XO+(-DIST*LOO)
              PY=YO+(-DIST*MOO)
              PZ=ZO+(-DIST*NOO)
              XRX=X11X-(DIST*L11X)
              YRX=Y11X-(DIST*M11X)
              XRY=X11Y-(DIST*L11Y)
              YRY=Y11Y-(DIST*M11Y)
              DIAX=DABS(((2.0D0*DSQRT((XRX**2)+(YRX**2)))/RSHIFTY)*RFX)
              DIAY=DABS(((2.0D0*DSQRT((XRY**2)+(YRY**2)))/RSHIFTX)*RFY)
              RETURN
          END IF
          RETURN
      END
C SUB EXPUP.FOR
      SUBROUTINE EXPUP
C
          IMPLICIT NONE
C
C       THIS CONTROLS THE OPERATION OF THE "EXPUP (AUTO or NOAUTO)
C       COMMAND
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              IF(EXPAUT) THEN
C     AUTO IS SET
 100              FORMAT(
     1            'AUTOMATIC EXIT PUPIL LOCATION CALCULATION IS IN EFFECT')
                  WRITE(OUTLYNE,100)
                  CALL SHOWIT(1)
              ELSE
C     AUTO IS NOT SET
 200              FORMAT(
     1            'AUTOMATIC EXIT PUPIL LOCATION CALCULATION IS NOT IN EFFECT')
                  WRITE(OUTLYNE,200)
                  CALL SHOWIT(1)
              END IF
              RETURN
          END IF
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"EXPUP" ONLY TAKES QUALIFIER INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'AUTO'.AND.WQ.NE.'NOAUTO') THEN
                  WRITE(OUTLYNE,*)'INVALID QUALIFIER USED WITH "EXPUP"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.0.AND.STI.EQ.0) THEN
              WRITE(OUTLYNE,*)'"EXPUP" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WQ.EQ.'AUTO') THEN
              EXPAUT=.TRUE.
 300          FORMAT(
     1        'AUTOMATIC EXIT PUPIL LOCATION CALCULATION HAS BEEN TRUNED "ON"')
              WRITE(OUTLYNE,300)
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WQ.EQ.'NOAUTO') THEN
              EXPAUT=.FALSE.
 400          FORMAT(
     1        'AUTOMATIC EXIT PUPIL LOCATION CALCULATION HAS BEEN TURNED "OFF"')
              WRITE(OUTLYNE,400)
              CALL SHOWIT(1)
              RETURN
          END IF
          RETURN
      END
C SUB INSID1.FOR
C
      LOGICAL FUNCTION INSID1()
C
          IMPLICIT NONE
C
C  RESULT IS .TRUE. IF (X0,Y0) LIES INSIDE THE NP_GON
C  OR ON ITS BOUNDARY OR ON ITS POINTS
C  THIS ROUTINE IS USED TO CHECK CLEAR APERTURES
C  WHERE TRUE MEANS NO-BLOCKING AND FALSE MEANS BLOCKED RAYS
C  DEFINED BY THE TABLES OF YT VS. XT.
C
          REAL*8 TUPI,ANGL(1:200),ARG
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          TUPI=TWOPII
          IF(REAL(Y0).EQ.REAL(YT(1)).AND.REAL(X0).EQ.REAL(XT(1))) THEN
              INSID1=.TRUE.
              RETURN
          ELSE
              IF(DABS(YT(1)-Y0).LE.1.0D-15.AND.
     1        DABS(XT(1)-X0).LE.1.0D-15) THEN
                  ANGL(1)=0.0D0
              ELSE
                  ANGL(1) = DATAN2( YT(1)-Y0, XT(1)-X0)
              END IF
              IF(ANGL(1).LT.0.0D0) ANGL(1)=TUPI+ANGL(1)
          ENDIF
          DO I = 2, NP
              IF(REAL(Y0).EQ.REAL(YT(I)).AND.REAL(X0).EQ.REAL(XT(I))) THEN
                  INSID1=.TRUE.
                  RETURN
              ELSE
                  IF(DABS(YT(I)-Y0).LE.1.0D-15.AND.
     1            DABS(XT(I)-X0).LE.1.0D-15) THEN
                      ANGL(I)=0.0D0
                  ELSE
                      ANGL(I) = DATAN2( YT(I)-Y0, XT(I)-X0)
                  END IF
                  IF(ANGL(I).LT.0.0D0) ANGL(I)=TUPI+ANGL(I)
              ENDIF
          END DO
          INSID1=.TRUE.
          ARG=ANGL(1)-ANGL(NP)
          IF(ARG.LT.0.0D0) ARG=ARG+TUPI
          IF(ARG.GT.(PII+1.0D-10)) THEN
              INSID1=.FALSE.
              RETURN
          END IF
          DO I=NP,2,-1
              ARG=ANGL(I)-ANGL(I-1)
              IF(ARG.LT.0.0D0) ARG=ARG+TUPI
              IF(ARG.GT.(PII+1.0D-10)) THEN
                  INSID1=.FALSE.
                  RETURN
              END IF
          END DO
          RETURN
      END
C SUB INSID2.FOR
C
      LOGICAL FUNCTION INSID2()
C
          IMPLICIT NONE
C
C  RESULT IS .TRUE. IF (X0,Y0) LIES INSIDE THE NP_GON
C  AND FALSE IF ON ITS BOUNDARY OR ON ITS CORNER POINTS
C  THIS ROUTINE IS USED TO CHECK OBSCURATIONS
C  WHERE TRUE MEANS BLOCKING AND FALSE MEANS NO BLOCKED RAYS
C  DEFINED BY THE TABLES OF YT VS. XT.
C
          REAL*8 TUPI,ANGL(1:200),ARG
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          TUPI=TWOPII
          IF(REAL(Y0).EQ.REAL(YT(1)).AND.REAL(X0).EQ.REAL(XT(1))) THEN
              INSID2=.FALSE.
              RETURN
          ELSE
              IF(DABS(YT(1)-Y0).LE.1.0D-15.AND.
     1        DABS(XT(1)-X0).LE.1.0D-15) THEN
                  ANGL(1)=0.0D0
              ELSE
                  ANGL(1) = DATAN2( YT(1)-Y0, XT(1)-X0)
              END IF
              IF(ANGL(1).LT.0.0D0) ANGL(1)=TUPI+ANGL(1)
          ENDIF
          DO I = 2, NP
              IF(REAL(Y0).EQ.REAL(YT(I)).AND.REAL(X0).EQ.REAL(XT(I))) THEN
                  INSID2=.FALSE.
                  RETURN
              ELSE
                  IF(DABS(YT(I)-Y0).LE.1.0D-15.AND.
     1            DABS(XT(I)-X0).LE.1.0D-15) THEN
                      ANGL(I)=0.0D0
                  ELSE
                      ANGL(I) = DATAN2( YT(I)-Y0, XT(I)-X0)
                  END IF
                  IF(ANGL(I).LT.0.0D0) ANGL(I)=TUPI+ANGL(I)
              ENDIF
          END DO
          INSID2=.TRUE.
          ARG=ANGL(1)-ANGL(NP)
          IF(ARG.LT.0.0D0) ARG=ARG+TUPI
          IF(ARG.GT.(PII-1.0D-10)) THEN
              INSID2=.FALSE.
              RETURN
          END IF
          DO I=NP,2,-1
              ARG=ANGL(I)-ANGL(I-1)
              IF(ARG.LT.0.0D0) ARG=ARG+TUPI
              IF(ARG.GT.(PII-1.0D-10)) THEN
                  INSID2=.FALSE.
                  RETURN
              END IF
          END DO
          RETURN
      END
C
C     THIS ROUTINE DOES CROSS TRACK SPECTRAL
C
      SUBROUTINE CTS(VALVAL,TYPE,V1,V2,ERROR)
          IMPLICIT NONE
          INTEGER ERROR,TYPE
          REAL*8 VALVAL,V1,V2,YMAX,YMIN,YV1,YV2,YV3
          LOGICAL OLDLDIF,OLDLDIF2
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     IF TYPE=1 XFOB
C     IF TYPE=2 YFOB
          YV1=0.0D0
          YV2=0.0D0
          YV3=0.0D0
C     FOCAL AND UFOCAL ONLY
          IF(SYSTEM1(30).GE.3.0D0) THEN
              ERROR=1
              RETURN
          END IF
C
C     CHECK WAVELENGTH
          IF(V1.NE.1.0D0.AND.
     1    V1.NE.2.0D0.AND.
     1    V1.NE.3.0D0.AND.
     1    V1.NE.4.0D0.AND.
     1    V1.NE.5.0D0.AND.
     1    V1.NE.6.0D0.AND.
     1    V1.NE.7.0D0.AND.
     1    V1.NE.8.0D0.AND.
     1    V1.NE.9.0D0.AND.
     1    V1.NE.10.0D0) THEN
              ERROR=1
              RETURN
          END IF
          IF(V1.EQ.1.0D0.AND.SYSTEM1(1).EQ.0.0D0.OR.
     1    V1.EQ.2.0D0.AND.SYSTEM1(2).EQ.0.0D0.OR.
     1    V1.EQ.3.0D0.AND.SYSTEM1(3).EQ.0.0D0.OR.
     1    V1.EQ.4.0D0.AND.SYSTEM1(4).EQ.0.0D0.OR.
     1    V1.EQ.5.0D0.AND.SYSTEM1(5).EQ.0.0D0.OR.
     1    V1.EQ.6.0D0.AND.SYSTEM1(71).EQ.0.0D0.OR.
     1    V1.EQ.7.0D0.AND.SYSTEM1(72).EQ.0.0D0.OR.
     1    V1.EQ.8.0D0.AND.SYSTEM1(73).EQ.0.0D0.OR.
     1    V1.EQ.9.0D0.AND.SYSTEM1(74).EQ.0.0D0.OR.
     1    V1.EQ.10.0D0.AND.SYSTEM1(75).EQ.0.0D0) THEN
              ERROR=1
              RETURN
          END IF
C     WAVELENGTHS ARE OK
          IF(TYPE.EQ.1) THEN
C     Y COMPONENTS OF XFOB AT DESIGNATED WAVELENGTH
C
C     DO THE FOB 0 1 0 V1
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              LDIF2=.FALSE.
              LDIF=.FALSE.
              WQ='        '
              SQ=0
              SST=0
              STI=0
              W1=0.0D0
              W2=1.0D0
              W3=0.0D0
              W4=V1
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
              WC='FOB     '
              CALL FFOB
              IF(.NOT.REFEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C NOW THE RAY 0 0
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
              W1=0.0D0
              W2=0.0D0
              W3=V1
              W4=0.0D0
              W5=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              IF(.NOT.RAYEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C     WRITE DOWN THE FIRST Y VALUE1
              YV1=RAYRAY(2,NEWIMG)
C     DO THE FOB 0 0 0 V1
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              LDIF2=.FALSE.
              LDIF=.FALSE.
              WQ='        '
              SQ=0
              SST=0
              STI=0
              W1=0.0D0
              W2=0.0D0
              W3=0.0D0
              W4=V1
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
              WC='FOB     '
              CALL FFOB
              IF(.NOT.REFEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C NOW THE RAY 0 0
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
              W1=0.0D0
              W2=0.0D0
              W3=V1
              W4=0.0D0
              W5=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              IF(.NOT.RAYEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C     WRITE DOWN THE SECOND Y VALUE1
              YV2=RAYRAY(2,NEWIMG)
C     DO THE FOB 0 -1 0 V1
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              LDIF2=.FALSE.
              LDIF=.FALSE.
              WQ='        '
              SQ=0
              SST=0
              STI=0
              W1=0.0D0
              W2=-1.0D0
              W3=0.0D0
              W4=V1
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
              WC='FOB     '
              CALL FFOB
              IF(.NOT.REFEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C NOW THE RAY 0 0
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
              W1=0.0D0
              W2=0.0D0
              W3=V1
              W4=0.0D0
              W5=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              IF(.NOT.RAYEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C     WRITE DOWN THE THIRD Y VALUE1
              YV3=RAYRAY(2,NEWIMG)
              YMAX=-1.0D300
              YMIN=1.0D300
              IF(YV1.GT.YMAX) YMAX=YV1
              IF(YV2.GT.YMAX) YMAX=YV2
              IF(YV3.GT.YMAX) YMAX=YV3
              IF(YV1.LT.YMIN) YMIN=YV1
              IF(YV2.LT.YMIN) YMIN=YV2
              IF(YV3.LT.YMIN) YMIN=YV3
              VALVAL=DABS(YMAX-YMIN)
              IF(V2.NE.0.0D0) VALVAL=(VALVAL/V2)*100.0D0
              ERROR=0
              RETURN
          END IF
          IF(TYPE.EQ.2) THEN
C     XCOMPONENTS OF YFOB AT DESIGNATED WAVELENGTH
C
C     DO THE FOB 1 0 0 V1
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              LDIF2=.FALSE.
              LDIF=.FALSE.
              WQ='        '
              SQ=0
              SST=0
              STI=0
              W1=1.0D0
              W2=0.0D0
              W3=0.0D0
              W4=V1
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
              WC='FOB     '
              CALL FFOB
              IF(.NOT.REFEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C NOW THE RAY 0 0
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
              W1=0.0D0
              W2=0.0D0
              W3=V1
              W4=0.0D0
              W5=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              IF(.NOT.RAYEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C     WRITE DOWN THE FIRST X VALUE1
              YV1=RAYRAY(1,NEWIMG)
C     DO THE FOB 0 0 0 V1
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              LDIF2=.FALSE.
              LDIF=.FALSE.
              WQ='        '
              SQ=0
              SST=0
              STI=0
              W1=0.0D0
              W2=0.0D0
              W3=0.0D0
              W4=V1
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
              WC='FOB     '
              CALL FFOB
              IF(.NOT.REFEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C NOW THE RAY 0 0
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
              W1=0.0D0
              W2=0.0D0
              W3=V1
              W4=0.0D0
              W5=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              IF(.NOT.RAYEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C     WRITE DOWN THE SECOND X VALUE1
              YV2=RAYRAY(1,NEWIMG)
C     DO THE FOB -1 0 0 V1
              SAVE_KDP(1)=SAVEINPT(1)
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              LDIF2=.FALSE.
              LDIF=.FALSE.
              WQ='        '
              SQ=0
              SST=0
              STI=0
              W1=-1.0D0
              W2=0.0D0
              W3=0.0D0
              W4=V1
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
              WC='FOB     '
              CALL FFOB
              IF(.NOT.REFEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C NOW THE RAY 0 0
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
              W1=0.0D0
              W2=0.0D0
              W3=V1
              W4=0.0D0
              W5=0.0D0
              WC='RAY     '
              NOCOAT=.TRUE.
              GRASET=.FALSE.
              DXFSET=.FALSE.
              CALL RRAY
              IF(.NOT.RAYEXT) THEN
                  ERROR=1
                  RETURN
              END IF
              REST_KDP(1)=RESTINPT(1)
C     WRITE DOWN THE THIRD X VALUE1
              YV3=RAYRAY(1,NEWIMG)
              YMAX=-1.0D300
              YMIN=1.0D300
              IF(YV1.GT.YMAX) YMAX=YV1
              IF(YV2.GT.YMAX) YMAX=YV2
              IF(YV3.GT.YMAX) YMAX=YV3
              IF(YV1.LT.YMIN) YMIN=YV1
              IF(YV2.LT.YMIN) YMIN=YV2
              IF(YV3.LT.YMIN) YMIN=YV3
              VALVAL=DABS(YMAX-YMIN)
              IF(V2.NE.0.0D0) VALVAL=(VALVAL/V2)*100.0D0
              ERROR=0
              RETURN
          END IF
      END
      SUBROUTINE COFACTOR
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          COHERENCE_FACTOR=W1
          RETURN
      END
C SUB CLPCEN.FOR
      SUBROUTINE CLPCEN
C
          IMPLICIT NONE
C
C     KEEPS TRACK OF FOB 0 0 RAY LOCATIONS AT EACH SURFACE
C
!        INTEGER I
C
          REAL*8 CLPLCX(0:499),CLPLCY(0:499)
C
          COMMON/CLPLOC/CLPLCX,CLPLCY

C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CLPLCX(NEWOBJ:NEWIMG)=REFRY(1,NEWOBJ:NEWIMG)
          CLPLCY(NEWOBJ:NEWIMG)=REFRY(2,NEWOBJ:NEWIMG)
          RETURN
      END
