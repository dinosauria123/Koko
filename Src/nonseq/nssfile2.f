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

C       SECOND COLLECTION OF NSS FILES

      SUBROUTINE NSS_SURFACE_INTERACT(J)
          USE NSSMOD
C
          IMPLICIT NONE
C
          INTEGER J,INTERACTION_CODE,FILE_NUMBER
C
          REAL*8 N,NP,COSI,SINI,SDOTN,GRO,GRS,GRX,GRY,GRZ,IA,IAP,MAG
C
          REAL*8 BIGLAM,BIGGAM,QX,QY,QZ,PX,PY,PZ,DSPACE_EFF,FACTOR
C
          REAL*8 CHIX,CHIY,CHIZ,CHINOR,PNOR,QNOR,WAVE,A,B,SINIP,SDOTP,MU
C
          REAL*8 ROOTTERM,GAM1,SGNA,ENERGY
C
          LOGICAL GRATE,TIR
C
          INCLUDE 'datmai.inc'
C
          TIR=.FALSE.
C
C       LOCAL COORDINATE RAY DATA IS IN:
C               RRX
C               RRY
C               RRZ
C               RRL
C               RRM
C               RRN
C               XRRL
C               XRRM
C               XRRN
C               YRRL
C               YRRM
C               YRRN
C               NLN
C               NMN
C               NNN
          RRX=NSS_LRAY(1)
          RRY=NSS_LRAY(2)
          RRZ=NSS_LRAY(3)
          RRL=NSS_LRAY(4)
          RRM=NSS_LRAY(5)
          RRN=NSS_LRAY(6)
          XRRL=NSS_LRAY(7)
          XRRM=NSS_LRAY(8)
          XRRN=NSS_LRAY(9)
          YRRL=NSS_LRAY(10)
          YRRM=NSS_LRAY(11)
          YRRN=NSS_LRAY(12)
          NSSLN=NSS_LRAY(13)
          NSSMN=NSS_LRAY(14)
          NSSNN=NSS_LRAY(15)
          NLN=NSSLN
          NMN=NSSMN
          NNN=NSSNN
          REALINDEX1=NSSALENS(100+NSSWAVNUM,J)
          REALINDEX2=NSSALENS(120+NSSWAVNUM,J)
          IMAGINDEX1=NSSALENS(110+NSSWAVNUM,J)
          IMAGINDEX2=NSSALENS(130+NSSWAVNUM,J)
C       IF THE RAY'S LOCAL RRN IS POSITIVE, THEN IT IS TRAVELING IN THE TRAVEL=1
C       DIRECTION FROM REALINDEX1=N TO REALINDEX2=NSS_NP
C       SET THE VALUE OF TRAVEL
          IF(RRN.EQ.0.0D0) RRN=1.0D-10
          IF(RRN.GE.0.0D0) THEN
              TRAVEL=1
          ELSE
              TRAVEL=2
          END IF
C       SET VALUES OF NSS_N AND NSS_NP
          IF(TRAVEL.EQ.1) THEN
              NSS_N=REALINDEX1
              NSS_NP=REALINDEX2
          ELSE
              NSS_N=REALINDEX2
              NSS_NP=REALINDEX1
          END IF
C       SET THE VALUE OF THE NEW CURRENT INDEX VALUE
          CURINDEXR=NSS_N
          MU=DABS(NSS_N/NSS_NP)
C       DO WE NEED TO ADJUST THE WORKING VALUES OF THE SURFACE NORMALS?
C       GET CURRENT WAVELENGTH IN MICROMETER
          WAVE=NSSSYSTEM(NSSWAVNUM)
C       CONVERT TO SYSTEM UNITS
          IF(NSSSYSTEM(21).EQ.1.0D0) WAVE=WAVE*3.93700787402D-5
          IF(NSSSYSTEM(21).EQ.2.0D0) WAVE=WAVE*1.0D-4
          IF(NSSSYSTEM(21).EQ.3.0D0) WAVE=WAVE*1.0D-3
          IF(NSSSYSTEM(21).EQ.4.0D0) WAVE=WAVE*1.0D-6
C       IS THE SURFACE A GRATING?
          IF(NSSALENS(146,J).EQ.0.0D0) THEN
C       NO GRATING
              GRATE=.FALSE.
          ELSE
C       GRATING
              GRATE=.TRUE.
          END IF
C       FORM DOT PRODUCT OF INCOMING RAY WITH NORMAL TO SURFACE
          SDOTN=(RRL*NLN)+(RRM*NMN)+(RRN*NNN)
C       SDOTN IS THE COSINE OF THE ANGLE OF INCIDENCE
          COSI=SDOTN
          IF(COSI.LT.-1.0D0) COSI=-1.0D0
          IF(COSI.GT.1.0D0) COSI=1.0D0
          SINI=DSQRT(1.0D0-(COSI**2))
          SINIP=(SINI)/(MU)
          IF(.NOT.GRATE.AND.SINIP.GT.1.0D0) THEN
C       TIR IS TRUE
              TIR=.TRUE.
          END IF
C
          IF(GRATE) THEN
C       WE HAVE A DIFFRACTION GRATING TO DEAL WITH
              GRO=NSSALENS(141,J)
              GRS=NSSALENS(142,J)
              GRX=NSSALENS(143,J)
              GRY=NSSALENS(144,J)
              GRZ=NSSALENS(145,J)
              IF(GRX.EQ.0.0D0.AND.GRY.EQ.0.0D0
     1        .AND.GRZ.EQ.0.0D0) GRY=1.0D0
              CHINOR=DSQRT((GRX**2)+(GRY**2)+
     1        (GRZ**2))
C       THESE ARE THE DIRECTION COSINES OF THE NORMALS TO THE GRATING
C       GENERATING PLANES
              CHIX=GRX/CHINOR
              CHIY=GRY/CHINOR
              CHIZ=GRZ/CHINOR
C     CALCULATE THE COMPONENTS OF THE VECTOR Q WHICH IS PARALLEL
C     TO THE EFFECTIVE RULINGS
              QX=-(CHIY*NNN)+(CHIZ*NMN)
              QY=-(CHIZ*NLN)+(CHIX*NNN)
              QZ=-(CHIX*NMN)+(CHIY*NLN)
              QNOR=DSQRT((QX**2)+(QY**2)+(QZ**2))
              QX=QX/QNOR
              QY=QY/QNOR
              QZ=QZ/QNOR
              QNOR=DSQRT((QX**2)+(QY**2)+(QZ**2))
              QX=QX/QNOR
              QY=QY/QNOR
              QZ=QZ/QNOR
C     CALCULATE THE COMPONENTS OF THE VECTOR P WHICH IS NORMAL
C     TO THE EFFECTIVE RULINGS
              PX=(QY*NNN)-(QZ*NMN)
              PY=(QZ*NLN)-(QX*NNN)
              PZ=(QX*NMN)-(QY*NLN)
              PNOR=DSQRT((PX**2)+(PY**2)+(PZ**2))
              PX=PX/PNOR
              PY=PY/PNOR
              PZ=PZ/PNOR
              FACTOR=(CHIX*PX)+(CHIY*PY)+(CHIZ*PZ)
              IF(DABS(FACTOR).GT.1E-6) DSPACE_EFF=GRS/FACTOR
              IF(DABS(FACTOR).LE.1E-6) DSPACE_EFF=1.0D20
              IF((DSPACE_EFF*DABS(NSS_NP)).EQ.0.0D0) THEN
                  BIGLAM=0.0D0
              ELSE
                  BIGLAM=(GRO*WAVE)/(DSPACE_EFF*DABS(NSS_NP))
              END IF
              SDOTP=(RRL*PX)+(RRL*PY)*(RRZ*PZ)
          ELSE
C       NO GRATING EXISTS
              BIGLAM=0.0D0
              SDOTP=0.0D0
          END IF
C       NOW FOR A AND B GOOD FOR CASES OF GRATINGS OR NO GRATINGS
          A=2.0*MU*SDOTN
          B=(MU**2)-1.0D0-(BIGLAM**2)+(2.0D0*MU*BIGLAM*SDOTP)
          ROOTTERM=(A**2)-(4.0D0*B)
          IF(ROOTTERM.LT.0.0D0) THEN
              IF(.NOT.GRATE) THEN
C       SIMPLE TIR, SET ROOTTERM=0.0D0 AND CONTINUE
                  ROOTTERM=0.0D0
              ELSE
C       RAY FAILED DUE TO INREALISTIC ANGLE OF DIFFRACTION
                  NSSRAYFAIL=.TRUE.
                  NSSRAYFAILCODE(1)=J
                  NSSRAYFAILCODE(2)=1
                  RETURN
              END IF
          END IF
C       NOW ROOTTERM >=0
          IF(TRAVEL.EQ.1) THEN
C       DEAL WITH ABSORPTION OR OTHER FAILURE
              IF(NSSALENS(8,J).EQ.3.0D0.OR.NSSALENS(8,J)
     1        .EQ.0.0D0.AND.TIR.OR.DETSNUM.EQ.J) THEN
                  NSSRAYFAIL=.TRUE.
                  NSSRAY_ENERGY=0.0D0
                  NSSRAYFAILCODE(1)=J
                  NSSRAYFAILCODE(2)=1
                  RETURN
              END IF
              SGNA=1.0D0
              IF(A.NE.0.0D0) SGNA=A/DABS(A)
              GAM1=-0.5D0*(A+(SGNA*DSQRT(ROOTTERM)))
C       DEAL WITH REFRACTION
              IF(NSSALENS(8,J).EQ.1.0D0) THEN
C       REFRACTION
                  INTERACTION_CODE=1
                  BIGGAM=B/GAM1
              END IF
              IF(NSSALENS(8,J).EQ.2.0D0) THEN
C       REFLECTION
                  INTERACTION_CODE=2
                  BIGGAM=GAM1
              END IF
          ELSE
C       TRAVEL=2
C       DEAL WITH REFRACTION FIRST, THEN REFLECTION
              IF(NSSALENS(9,J).EQ.3.0D0.OR.NSSALENS(9,J)
     1        .EQ.0.0D0.AND.TIR.OR.DETSNUM.EQ.J) THEN
                  NSSRAY_ENERGY=0.0D0
                  NSSRAYFAIL=.TRUE.
                  NSSRAYFAILCODE(1)=J
                  NSSRAYFAILCODE(2)=1
                  RETURN
              END IF
              SGNA=1.0D0
              IF(A.NE.0.0D0) SGNA=A/DABS(A)
              GAM1=-0.5D0*(A+(SGNA*DSQRT(ROOTTERM)))
C       DEAL WITH REFRACTION
              IF(NSSALENS(9,J).EQ.1.0D0) THEN
C       REFRACTION
                  INTERACTION_CODE=1
                  BIGGAM=B/GAM1
              END IF
              IF(NSSALENS(9,J).EQ.2.0D0) THEN
C       REFLECTION
                  INTERACTION_CODE=2
                  BIGGAM=GAM1
              END IF
          END IF
C
C       NEW RAY DIRECTION COSINES ARE:
          RRL=(RRL*MU)-(BIGLAM*PX)+(BIGGAM*NLN)
          RRM=(RRM*MU)-(BIGLAM*PY)+(BIGGAM*NMN)
          RRN=(RRN*MU)-(BIGLAM*PZ)+(BIGGAM*NNN)
          MAG=DSQRT((RRL**2)+(RRM**2)+(RRN**2))
          IF(MAG.EQ.0.0D0) MAG=1.0D0
          RRL=RRL/MAG
          RRM=RRM/MAG
          RRN=RRN/MAG

          XRRL=(XRRL*MU)-(BIGLAM*PX)+(BIGGAM*NLN)
          XRRM=(XRRM*MU)-(BIGLAM*PY)+(BIGGAM*NMN)
          XRRN=(XRRN*MU)-(BIGLAM*PZ)+(BIGGAM*NNN)
          MAG=DSQRT((XRRL**2)+(XRRM**2)+(XRRN**2))
          IF(MAG.EQ.0.0D0) MAG=1.0D0
          XRRL=XRRL/MAG
          XRRM=XRRM/MAG
          XRRN=XRRN/MAG
          YRRL=(YRRL*MU)-(BIGLAM*PX)+(BIGGAM*NLN)
          YRRM=(YRRM*MU)-(BIGLAM*PY)+(BIGGAM*NMN)
          YRRN=(YRRN*MU)-(BIGLAM*PZ)+(BIGGAM*NNN)
          MAG=DSQRT((YRRL**2)+(YRRM**2)+(YRRN**2))
          IF(MAG.EQ.0.0D0) MAG=1.0D0
          YRRL=YRRL/MAG
          YRRM=YRRM/MAG
          YRRN=YRRN/MAG
          IA=DASIN(SINI)
          IAP=DASIN(SINIP)
          NSS_LRAYP(1)=RRX
          NSS_LRAYP(2)=RRY
          NSS_LRAYP(3)=RRZ
          NSS_LRAYP(4)=RRL
          NSS_LRAYP(5)=RRM
          NSS_LRAYP(6)=RRN
          NSS_LRAYP(7)=XRRL
          NSS_LRAYP(8)=XRRM
          NSS_LRAYP(9)=XRRN
          NSS_LRAYP(10)=YRRL
          NSS_LRAYP(11)=YRRM
          NSS_LRAYP(12)=YRRN
          NSS_LRAYP(13)=NLN
          NSS_LRAYP(14)=NMN
          NSS_LRAYP(15)=NNN
C       RESET CURRENT INDEX TO THE INDEX AFTER SURFACE INTERACTION
          PREVINDEXR=NSS_N
          CURINDEXR=NSS_NP
C
C       SET THE COATING FILE NUMBERS
C       TRAVEL DIRECTION MEDIA1 TO MEDIA2
          IF(TRAVEL.EQ.1) FILE_NUMBER=INT(NSSALENS(31,J))
C       TRAVEL DIRECTION MEDIA2 TO MEDIA1
          IF(TRAVEL.EQ.2) FILE_NUMBER=INT(NSSALENS(32,J))
          IF(FILE_NUMBER.LT.0.OR.FILE_NUMBER.GT.1000) FILE_NUMBER=0
          ENERGY=NSSRAY_ENERGY
          N=NSS_N
          NP=NSS_NP
C       CALL ENERGY_CALC(ENERGY,N,NP,IA,IAP,INTERACTION_CODE,TIR
C    1,FILE_NUMBER)
          NSSRAY_ENERGY=ENERGY
          NSS_LRAYP(16)=NSSRAY_ENERGY
          RETURN
      END


      SUBROUTINE ENERGY_CALC(ENERGY,
     1INTERACTION_CODE,TIR,FILE_NUMBER)
          IMPLICIT NONE
C       J=SURFACE NUMBER
C       N=MEDIA INDEX
C       NP=SUBSTRATE INDEX
C       IA=ANGLE OF INCIDENCE
C       IA=ANGLE OF REFRACTION,REFLECTION OR DIFFRACTION
C       INTERACTION_CODE
          INTEGER FILE_NUMBER
          REAL*8 ENERGY,EFFICIENCY
          LOGICAL TIR,EXISF,OPENF
          INTEGER INTERACTION_CODE,K
          CHARACTER AI4,CFILE_NAME*12
          include 'datmai.inc'

          IF(FILE_NUMBER.LT.0.OR.FILE_NUMBER.GT.1000) FILE_NUMBER=0
          IF(FILE_NUMBER.EQ.0) THEN
C       NO COATING, NO LOSSES,DONT'CHANGE ENERGY
              RETURN
          END IF
C       FILE NUMBER WAS NOT ZERO
C       DOES A CORRESPONDING FILE EXIST
          OPENF=.FALSE.
          EXISF=.FALSE.
          CALL ITOA4(AI4,FILE_NUMBER)
          CFILE_NAME=trim(HOME)//'COAT'//AI4//'.DAT'
          INQUIRE(FILE=CFILE_NAME,EXIST=EXISF)
          IF(.NOT.EXISF) THEN
C       FILE DOES NOT EXIST, NO ENERGY CHANGE
              RETURN
          END IF
C       CORRESPONDING FILE DOES EXIST
          OPEN(UNIT=68,FILE=CFILE_NAME)
C       READ COATING TYPE
          READ(68,*) K
          IF(K.LT.0.OR.K.GT.2) K=0
          IF(K.EQ.0) RETURN
C       K BETWEEN 1 AND 2
          IF(K.EQ.1) THEN
              READ(68,*) EFFICIENCY
              CALL CLOSE_FILE(68,1)
C       EFFICIENCY COATING
C       NO COATING, JUST EFFICIENCY
              IF(INTERACTION_CODE.EQ.1) THEN
C       REFRACTION
                  IF(TIR) THEN
                      ENERGY=0.0D0
                  ELSE
                      ENERGY=ENERGY*EFFICIENCY
                  END IF
              END IF
              IF(INTERACTION_CODE.EQ.2) THEN
C       REFLECTION
                  IF(TIR) THEN
C       NO LOSS
                  ELSE
                      ENERGY=ENERGY*EFFICIENCY
                  END IF
              END IF
          ELSE
C       NOT COATING TYPE = 1
          END IF
      END

C
      LOGICAL FUNCTION NSSINS(X0,Y0,X1,Y1,X2,Y2,X3,Y3,X4,Y4)
C
          IMPLICIT NONE
C
C  RESULT IS .TRUE. IF (X0,Y0) LIES INSIDE THE RECTANGLE FORMED BY THE
C  X,Y PAIRS 1 TO 4
C  OR ON ITS BOUNDARY OR ON ITS POINTS
C  THIS ROUTINE IS USED TO CHECK LIMITS BOX INTERSECTIONS IN NSS RAY TRACING
C  DEFINED BY THE TABLES OF YT VS. XT.
C
          REAL*8 TUPI,ANGL(1:4),ARG,X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,PII
C
          REAL*8 NXT(1:4),NYT(1:4)
C
          INTEGER I
C
          PII=3.14159265358979323846D0
C
          NXT(1)=X1
          NXT(2)=X2
          NXT(3)=X3
          NXT(4)=X4
          NYT(1)=Y1
          NYT(2)=Y2
          NYT(3)=Y3
          NYT(4)=Y4
C
          TUPI=2.0D0*PII
          IF(REAL(Y0).EQ.REAL(NYT(1)).AND.REAL(X0).EQ.REAL(NXT(1))) THEN
              NSSINS=.TRUE.
              RETURN
          ELSE
              IF(DABS(NYT(1)-Y0).LE.1.0D-15.AND.
     1        DABS(NXT(1)-X0).LE.1.0D-15) THEN
                  ANGL(1)=0.0D0
              ELSE
                  ANGL(1) = DATAN2( NYT(1)-Y0, NXT(1)-X0)
              END IF
              IF(ANGL(1).LT.0.0D0) ANGL(1)=TUPI+ANGL(1)
          ENDIF
          DO I = 2, 4
              IF(REAL(Y0).EQ.REAL(NYT(I)).AND.REAL(X0).EQ.REAL(NXT(I))) THEN
                  NSSINS=.TRUE.
                  RETURN
              ELSE
                  IF(DABS(NYT(I)-Y0).LE.1.0D-15.AND.
     1            DABS(NXT(I)-X0).LE.1.0D-15) THEN
                      ANGL(I)=0.0D0
                  ELSE
                      ANGL(I) = DATAN2( NYT(I)-Y0, NXT(I)-X0)
                  END IF
                  IF(ANGL(I).LT.0.0D0) ANGL(I)=TUPI+ANGL(I)
              ENDIF
          END DO
          NSSINS=.TRUE.
          ARG=ANGL(1)-ANGL(4)
          IF(ARG.LT.0.0D0) ARG=ARG+TUPI
          IF(ARG.GT.(PII+1.0D-10)) THEN
              NSSINS=.FALSE.
              RETURN
          END IF
          DO I=4,2,-1
              ARG=ANGL(I)-ANGL(I-1)
              IF(ARG.LT.0.0D0) ARG=ARG+TUPI
              IF(ARG.GT.(PII+1.0D-10)) THEN
                  NSSINS=.FALSE.
                  RETURN
              END IF
          END DO
          RETURN
      END
      SUBROUTINE BOUNDER(J)
          USE NSSMOD
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INTEGER J
C       IGNORE BOUNDS ON TUBE SURFACES AS THEY WERE ALREADY CHECKED
          IF(NSSALENS(1,J).EQ.6.0D0) THEN
              RETURN
          END IF
C       IF(J.EQ.25) THEN
C       WRITE(OUTLYNE,*) RRX,RRY,RRZ
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*) NSSBOUNDS(1,J),NSSBOUNDS(3,J),NSSBOUNDS(5,J)
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*) NSSBOUNDS(2,J),NSSBOUNDS(4,J),NSSBOUNDS(6,J)
C       CALL SHOWIT(1)
C       END IF
          IF(RRX.GE.NSSBOUNDS(1,J).AND.RRX.LE.NSSBOUNDS(2,J)
     1    .AND.RRY.GE.NSSBOUNDS(3,J).AND.RRY.LE.NSSBOUNDS(4,J)
     1    .AND.RRZ.GE.NSSBOUNDS(5,J).AND.RRZ.LE.NSSBOUNDS(6,J))THEN
              NSS_INTERSECT=.TRUE.
          ELSE
              NSS_INTERSECT=.FALSE.
          END IF
          RETURN
      END
      SUBROUTINE CLAPER(J)
          USE NSSMOD
          IMPLICIT NONE
          INTEGER J
          LOGICAL INSID1,INS
          EXTERNAL INSID1
          REAL*8 LS,RS,X1,X2,X3,X4,Y1,Y2,Y3,Y4,XR,YR,XRD,YRD,A15
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C       IGNORE CLAPS ON TUBE SURFACES AS THEY WERE ALREADY CHECKED
          IF(NSSALENS(1,J).EQ.6.0D0) THEN
              RETURN
          END IF
          IF(NSSALENS(19,J).EQ.0.0D0) THEN
              NSS_INTERSECT=.TRUE.
          END IF
          IF(NSSALENS(19,J).EQ.1.0D0) THEN
              LS=0.0D0
              XR=RRX-NSSALENS(22,J)
              YR=RRY-NSSALENS(23,J)
              LS=DSQRT((XR**2)+(YR**2))
              RS=DSQRT(NSSALENS(20,J)**2)
              IF(LS.GT.RS) THEN
                  NSS_INTERSECT=.FALSE.
              ELSE
                  NSS_INTERSECT=.TRUE.
              END IF
          END IF
          IF(NSSALENS(19,J).EQ.2) THEN
              X1=-NSSALENS(20,J)
              Y1=NSSALENS(21,J)
              X2=-NSSALENS(20,J)
              Y2=-NSSALENS(21,J)
              X3=NSSALENS(20,J)
              Y3=-NSSALENS(21,J)
              X4=NSSALENS(20,J)
              Y4=NSSALENS(21,J)
              XRD=RRX
              YRD=RRY
              XRD=XRD-NSSALENS(22,J)
              YRD=YRD-NSSALENS(23,J)
              A15=(NSSALENS(24,J))*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
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
                  NSS_INTERSECT=.TRUE.
              ELSE
                  NSS_INTERSECT=.FALSE.
              END IF
          END IF
          IF(NSSALENS(19,J).EQ.3.0D0) THEN
              LS=0.0D0
              XRD=RRX
              YRD=RRY
              XRD=XRD-NSSALENS(22,J)
              YRD=YRD-NSSALENS(23,J)
              A15=NSSALENS(24,J)*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
              LS=((XR**2)/(NSSALENS(20,J)**2))+
     1           ((YR**2)/(NSSALENS(21,J)**2))
C
              IF(LS.GT.1.0D0) THEN
                  NSS_INTERSECT=.FALSE.
              ELSE
                  NSS_INTERSECT=.TRUE.
                  LS=0.0D0
              END IF
          END IF
          RETURN
      END
      SUBROUTINE HOLEER(J)
          USE NSSMOD
          IMPLICIT NONE
          INTEGER J
          LOGICAL INSID2,INS
          EXTERNAL INSID2
          REAL*8 LS,RS,X1,X2,X3,X4,Y1,Y2,Y3,Y4,XR,YR,XRD,YRD,A15
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C       IGNORE HOLES ON TUBE SURFACES AS THEY WERE ALREADY CHECKED
          IF(NSSALENS(1,J).EQ.6.0D0) THEN
              RETURN
          END IF
          IF(NSSALENS(25,J).EQ.0.0D0) THEN
              NSS_INTERSECT=.TRUE.
          END IF
          IF(NSSALENS(25,J).EQ.1.0D0) THEN
              LS=0.0D0
              XR=RRX-NSSALENS(28,J)
              YR=RRY-NSSALENS(29,J)
              LS=DSQRT((XR**2)+(YR**2))
              RS=DSQRT(NSSALENS(26,J)**2)
              IF(LS.LT.RS) THEN
                  NSS_INTERSECT=.FALSE.
              ELSE
                  NSS_INTERSECT=.TRUE.
              END IF
          END IF
          IF(NSSALENS(25,J).EQ.2) THEN
              X1=-NSSALENS(26,J)
              Y1=NSSALENS(27,J)
              X2=-NSSALENS(26,J)
              Y2=-NSSALENS(27,J)
              X3=NSSALENS(26,J)
              Y3=-NSSALENS(27,J)
              X4=NSSALENS(26,J)
              Y4=NSSALENS(27,J)
              XRD=RRX
              YRD=RRY
              XRD=XRD-NSSALENS(28,J)
              YRD=YRD-NSSALENS(29,J)
              A15=(NSSALENS(30,J))*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
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
                  NSS_INTERSECT=.TRUE.
              ELSE
                  NSS_INTERSECT=.FALSE.
              END IF
          END IF
          IF(NSSALENS(25,J).EQ.3.0D0) THEN
              LS=0.0D0
              XRD=RRX
              YRD=RRY
              XRD=XRD-NSSALENS(28,J)
              YRD=YRD-NSSALENS(29,J)
              A15=NSSALENS(30,J)*PII/180.0D0
              XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
              YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
              LS=((XR**2)/(NSSALENS(26,J)**2))+
     1           ((YR**2)/(NSSALENS(27,J)**2))
C
              IF(LS.LT.1.0D0) THEN
                  NSS_INTERSECT=.FALSE.
              ELSE
                  NSS_INTERSECT=.TRUE.
                  LS=0.0D0
              END IF
          END IF
          RETURN
      END
      SUBROUTINE NSSFLA(J)
          USE NSSMOD
C
C       THIS ROUTINE INTERSECTS AN NSS RAY WITH A FLAT SURFACE
          IMPLICIT NONE
C
          INTEGER J
C
          REAL*8 NUSUBV,T0
C
          INCLUDE 'datmai.inc'
C
C       SURFACE IS PLANO AND CONTAINS 2ND, 4TH, 6TH, 8TH AND 10TH ORDER
C       ASPHERIC TERMS.
C       FIRST CALCULATE THE INTERSECTION TO THE SIMPLE PLANE
          T0=RRZ
C       T0/RRN IS THE DISTANCE ALONG THE RAY FROM
C       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
C       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
C       SYSTEM OF THE CURRENT SURFACE.
          RRX0=RRX
          RRY0=RRY
          RRZ0=RRZ
          IF(DABS(RRN).LE.1.0D-10) THEN
              NSS_INTERSECT=.FALSE.
              RETURN
          END IF
          NUSUBV=-T0/RRN
          RRX=RRX+(NUSUBV*RRL)
          RRY=RRY+(NUSUBV*RRM)
          RRZ=0.0D0
          NSS_INTERSECT=.TRUE.
          CALL NSSNR1(J)
          RETURN
      END
      SUBROUTINE NSSMEM(J)
          USE NSSMOD
C
C       THIS ROUTINE INTERSECTS AN NSS RAY WITH A MEM SURFACE
          IMPLICIT NONE
C
          INTEGER J
C
          REAL*8 NUSUBV,T0
C
          INCLUDE 'datmai.inc'
C
C       SURFACE IS PLANO
C       FIRST CALCULATE THE INTERSECTION TO THE SIMPLE PLANE
          T0=RRZ
C       T0/RRN IS THE DISTANCE ALONG THE RAY FROM
C       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
C       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
C       SYSTEM OF THE CURRENT SURFACE.
          RRX0=RRX
          RRY0=RRY
          RRZ0=RRZ
          IF(DABS(RRN).LE.1.0D-10) THEN
              NSS_INTERSECT=.FALSE.
              RETURN
          END IF
          NUSUBV=-T0/RRN
          RRX=RRX+(NUSUBV*RRL)
          RRY=RRY+(NUSUBV*RRM)
          RRZ=0.0D0
          NSS_INTERSECT=.TRUE.
          CALL NSSNR4(J)
          RETURN
      END
      SUBROUTINE NSSASP(J)
          USE NSSMOD
C
C       THIS ROUTINE INTERSECTS AN NSS RAY WITH A FLAT SURFACE
C       THEN THE ASPHERIC
          IMPLICIT NONE
C
          INTEGER J
C
          REAL*8 NUSUBV,T0
C
          INCLUDE 'datmai.inc'
C
C       SURFACE IS PLANO AND CONTAINS 2ND, 4TH, 6TH, 8TH AND 10TH ORDER
C       ASPHERIC TERMS.
C       FIRST CALCULATE THE INTERSECTION TO THE SIMPLE PLANE
          T0=RRZ
C       T0/RRN IS THE DISTANCE ALONG THE RAY FROM
C       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
C       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
C       SYSTEM OF THE CURRENT SURFACE.
          RRX0=RRX
          RRY0=RRY
          RRZ0=RRZ
          IF(DABS(RRN).LE.1.0D-10) THEN
              NSS_INTERSECT=.FALSE.
              RETURN
          END IF
          NUSUBV=-T0/RRN
          RRX=RRX+(NUSUBV*RRL)
          RRY=RRY+(NUSUBV*RRM)
          RRZ=0.0D0
          NSS_INTERSECT=.TRUE.
          CALL NSSNR2(J)
          RETURN
      END
      SUBROUTINE NSSANA(J)
          USE NSSMOD
C
C       THIS ROUTINE INTERSECTS AN NSS RAY WITH A FLAT SURFACE
C       THEN THE ANAMORPHIC
          IMPLICIT NONE
C
          INTEGER J
C
          REAL*8 NUSUBV,T0
C
          INCLUDE 'datmai.inc'
C
C       FIRST CALCULATE THE INTERSECTION TO THE SIMPLE PLANE
          T0=RRZ
C       T0/RRN IS THE DISTANCE ALONG THE RAY FROM
C       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
C       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
C       SYSTEM OF THE CURRENT SURFACE.
          RRX0=RRX
          RRY0=RRY
          RRZ0=RRZ
          IF(DABS(RRN).LE.1.0D-10) THEN
              NSS_INTERSECT=.FALSE.
              RETURN
          END IF
          NUSUBV=-T0/RRN
          RRX=RRX+(NUSUBV*RRL)
          RRY=RRY+(NUSUBV*RRM)
          RRZ=0.0D0
          NSS_INTERSECT=.TRUE.
          CALL NSSNR3(J)
          RETURN
      END
      SUBROUTINE NSSTUB(J)
          USE NSSMOD
C
C       THIS ROUTINE INTERSECTS AN NSS RAY WITH A TUBE SURFACE
          IMPLICIT NONE
C
          INTEGER J,INTERS
C
          LOGICAL ONE1,TWO2
C
          REAL*8 A,B,C,HV0,HV1,HV2,RRX1,RRY1,RRZ1,RRX2,RRY2,RRZ2
C
          REAL*8 Q,ARG,SIGNB
C
          INCLUDE 'datmai.inc'
C
C       SURFACE IS TUBE
C
C       HIT THE TUBE OF RADIUS R AND LENGTH L
C
          A=(RRL**2)+(RRM**2)
          B=2.0D0*((RRX*RRL)+(RRY*RRM))
          C=(RRX**2)+(RRY**2)-(NSSALENS(10,J)**2)
          IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
          IF(B.EQ.0.0D0) THEN
              NSS_INTERSECT=.FALSE.
              GO TO 90
          END IF
          IF(A.EQ.0.0D0) THEN
              HV0=-(C/B)
              INTERS=1
          ELSE
C       A NOT ZERO
              ARG=((B**2)-(4.0D0*A*C))
              IF(ARG.LT.0.0D0) THEN
                  NSS_INTERSECT=.FALSE.
                  GO TO 90
C       ARG NOT ZERO
              END IF
              Q=(-0.5D0*(B+(SIGNB*(DSQRT(ARG)))))
              IF(A.EQ.0.0D0) THEN
                  NSS_INTERSECT=.FALSE.
                  GO TO 90
              END IF
              IF(Q.EQ.0.0D0) THEN
                  NSS_INTERSECT=.FALSE.
                  GO TO 90
              END IF
              HV1=C/Q
              HV2=Q/A
              INTERS=2
          END IF
C       THE DISTANCE THAT THE RAY TRAVELS UNTIL IT INTERSECTS THE TUBE IS NUSUBV
C       AND IS SUCH THAT AT THAT POINT DSQRT(RRX**2+RRY**2) WILL EQUAL THE TUBE RADIUS
C       NSSALENS(10,J)
          IF(INTERS.EQ.1) THEN
              RRX1=RRX+(HV0*RRL)
              RRY1=RRY+(HV0*RRM)
              RRZ1=RRZ+(HV0*RRN)
C       CHECK Z-BOUNDS
              IF(RRZ1.GE.NSSBOUNDS(5,J).AND.RRZ1.LE.NSSBOUNDS(6,J)) THEN
                  NSS_INTERSECT=.TRUE.
              ELSE
                  NSS_INTERSECT=.FALSE.
                  GO TO 90
              END IF
          END IF
          IF(INTERS.EQ.2) THEN
              RRX1=RRX+(HV1*RRL)
              RRY1=RRY+(HV1*RRM)
              RRZ1=RRZ+(HV1*RRN)
              RRX2=RRX+(HV2*RRL)
              RRY2=RRY+(HV2*RRM)
              RRZ2=RRZ+(HV2*RRN)

C       CHECK Z-BOUNDS
              IF(RRZ1.GE.NSSBOUNDS(5,J).AND.RRZ1.LE.NSSBOUNDS(6,J)) THEN
                  ONE1=.TRUE.
              ELSE
                  ONE1=.FALSE.
              END IF
              IF(RRZ2.GE.NSSBOUNDS(5,J).AND.RRZ2.LE.NSSBOUNDS(6,J)) THEN
                  TWO2=.TRUE.
              ELSE
                  TWO2=.FALSE.
              END IF
          END IF
          IF(ONE1.AND..NOT.TWO2) THEN
              RRX=RRX+(HV1*RRL)
              RRY=RRY+(HV1*RRM)
              RRZ=RRZ+(HV1*RRM)
              NSS_INTERSECT=.TRUE.
C       COMPUTE THE INWARD SURFACE NORMAL
              NSSLN=-2.0*RRX
              NSSMN=-2.0*RRY
              NSSNN=0.0D0
          END IF
          IF(TWO2.AND..NOT.ONE1) THEN
              RRX=RRX+(HV2*RRL)
              RRY=RRY+(HV2*RRM)
              RRZ=RRZ+(HV2*RRM)
              NSS_INTERSECT=.TRUE.
C       COMPUTE THE INWARD SURFACE NORMAL
              NSSLN=-2.0*RRX
              NSSMN=-2.0*RRY
              NSSNN=0.0D0
          END IF
          IF(.NOT.TWO2.AND..NOT.ONE1) THEN
              NSS_INTERSECT=.FALSE.
              GO TO 90
          END IF
          IF(ONE1.AND.TWO2) THEN
              IF(HV1.GE.HV2) HV0=HV2
              IF(HV1.LT.HV2) HV0=HV1
              RRX=RRX+(HV0*RRL)
              RRY=RRY+(HV0*RRM)
              RRZ=RRZ+(HV0*RRM)
              NSS_INTERSECT=.TRUE.
C       COMPUTE THE INWARD SURFACE NORMAL
              NSSLN=-2.0*RRX
              NSSMN=-2.0*RRY
              NSSNN=0.0D0
          END IF
 90       CONTINUE
C       WRITE(OUTLYNE,*) NSS_INTERSECT
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*) RRX,RRY,RRZ
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*) NSSLN,NSSMN,NSSNN
C       CALL SHOWIT(1)
          RETURN
      END


      SUBROUTINE NSSNR1(J)
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE NSSNR1.INC. THIS SUBROUTINE IMPLEMENTS
C       INTERSECTION CALCULATION TO AN ASPHERIC PLANO SURFACE
C
          INTEGER J,I
          REAL*8 A1,A2,A3,A4,A5,A6,A7,A8,A9,A10
          REAL*8 AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,R,RHO,FUNC,NX,NY
C
          REAL*8 LENG,MAG,ZCAL,ZZTOP,X,Y
     1    ,FNXP,FNYP,FNZP,HV1,HV2,HV,XP,YP,ZP,DEL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C       SURFACE SAG FUNCTION
          FUNC(R,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)=
     1    (A1*(R**2))+(A2*(R**4))+(A3*(R**6))+(A4*(R**8))
     2    +(A5*(R**10))+(A6*(R**12))+(A7*(R**14))+(A8*(R**16))
     2    +(A9*(R**18))+(A10*(R**20))
C       SURFACE X DERIVATIVE FUNCTION
          NX(X,Y,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)=
     1    +((1.0D0*2.0D0*X)*A1)
     1    +((2.0D0*((X**2)+(Y**2))*2.0D0*X)*A2)
     1    +((3.0D0*(((X**2)+(Y**2))**2)*2.0D0*X)*A3)
     1    +((4.0D0*(((X**2)+(Y**2))**3)*2.0D0*X)*A4)
     1    +((5.0D0*(((X**2)+(Y**2))**4)*2.0D0*X)*A5)
     1    +((6.0D0*(((X**2)+(Y**2))**5)*2.0D0*X)*A6)
     1    +((7.0D0*(((X**2)+(Y**2))**6)*2.0D0*X)*A7)
     1    +((8.0D0*(((X**2)+(Y**2))**7)*2.0D0*X)*A8)
     1    +((9.0D0*(((X**2)+(Y**2))**8)*2.0D0*X)*A9)
     1    +((10.0D0*(((X**2)+(Y**2))**9)*2.0D0*X)*A10)
C       SURFACE Y DERIVATIVE FUNCTION
          NY(X,Y,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10)=
     1    +((1.0D0*2.0D0*Y)*A1)
     1    +((2.0D0*((X**2)+(Y**2))*2.0D0*Y)*A2)
     1    +((3.0D0*(((X**2)+(Y**2))**2)*2.0D0*Y)*A3)
     1    +((4.0D0*(((X**2)+(Y**2))**3)*2.0D0*Y)*A4)
     1    +((5.0D0*(((X**2)+(Y**2))**4)*2.0D0*Y)*A5)
     1    +((6.0D0*(((X**2)+(Y**2))**5)*2.0D0*Y)*A6)
     1    +((7.0D0*(((X**2)+(Y**2))**6)*2.0D0*Y)*A7)
     1    +((8.0D0*(((X**2)+(Y**2))**7)*2.0D0*Y)*A8)
     1    +((9.0D0*(((X**2)+(Y**2))**8)*2.0D0*Y)*A9)
     1    +((10.0D0*(((X**2)+(Y**2))**9)*2.0D0*Y)*A10)
C
          AC=NSSALENS(3,J)
          AD=NSSALENS(4,J)
          AE=NSSALENS(5,J)
          AF=NSSALENS(6,J)
          AG=NSSALENS(7,J)
          AH=NSSALENS(201,J)
          AI=NSSALENS(202,J)
          AJ=NSSALENS(203,J)
          AK=NSSALENS(204,J)
          AL=NSSALENS(205,J)
C
          DEL=DELSUR
C
C       THE RETURNED VALUES OF X,Y,Z ARE THE CORRECT INTERSECTIONS
C       TO WITHIN THE SURTOL VALUE
C
C       CALCULATE INTERSECTION
          DO I=1,NRAITR
C
C       X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
C
              FNXP=-NX(RRX,RRY,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL)
              FNYP=-NY(RRX,RRY,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL)
              FNZP=1.0D0
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
              MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
              NSSLN=FNXP/MAG
              NSSMN=FNYP/MAG
              NSSNN=FNZP/MAG
C       THE INTERSECTION OF THE RAY WITH THIS TANGENT
C       PLANE
C       USING:
C
              RHO=DSQRT((RRX**2)+(RRY**2))
              ZCAL=FUNC(RHO,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL)
              HV1=(ZCAL-RRZ)
              HV2=((RRL*NSSLN)+(RRM*NSSMN)+(RRN*NSSNN))
              HV=HV1/HV2
C       NEW X,Y AND Z ARE
              XP=RRX+(HV*RRL)
              YP=RRY+(HV*RRM)
              ZP=RRZ+(HV*RRN)
C       SHOULD WE CONTINUE?
              RHO=DSQRT((XP**2)+(YP**2))
              ZZTOP=FUNC(RHO,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL)
              LENG=DABS(ZZTOP-ZP)
              IF(LENG.LE.SURTOL) THEN
                  RRX=XP
                  RRY=YP
                  RRZ=ZP
C
C     X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
C
                  FNXP=-NX(RRX,RRY,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL)
                  FNYP=-NY(RRX,RRY,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL)
                  FNZP=1.0D0
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
                  MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
                  NSSLN=FNXP/MAG
                  NSSMN=FNYP/MAG
                  NSSNN=FNZP/MAG
                  RETURN
              ELSE
C       KEEP TRYING
C       MAKE X Y Z FROM XP YP ZP AND TRY AGAIN
                  RRX=XP
                  RRY=YP
                  RRZ=ZP
              END IF
          END DO
C       INTERATIONS EXCEEDED, NO INTERSECTION WAS POSSIBLE
          NSS_INTERSECT=.FALSE.
          RETURN
      END


      SUBROUTINE NSSEOS
          USE NSSMOD
C
          IMPLICIT NONE
          CHARACTER STRING*80,NBL80*80,NBL*20
          INTEGER J,K,L
          REAL*8 R1,MINX,MAXX,MINY,MAXY,DELX,DELY,X,Y,SAG,MAXZ,MINZ,NUDGE
          EXTERNAL NUDGE
C
C       THIS ROUTINE RESOLVES THE UNRESOLVED PARTS OF AN NSS DATABASE
C       PROGRAM

          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          NBL='                    '
          NBL80=NBL//NBL//NBL//NBL

          DO J=MAXS,1,-1
              IF(NSSALENS(100,J).EQ.1.0D0) THEN
                  LASTSUR=J
                  GO TO 101
              END IF
          END DO
C       ADDED 3/31/2004 TO SUPPORT I IN NSSTRACE WITH SPOT DIAGRAMS
 101      NSSSYSTEM(24)=DBLE(LASTSUR)
C       START NSS DATABASE
C       UNIVERSE
          IF(NSSSYSTEM(25).LT.0D0) NSSSYSTEM(25)=10000.D0
C       MINE
          IF(NSSSYSTEM(26).LT.0.0D0) NSSSYSTEM(26)=0.0D0
C       MAX HAY INTERSECTIONS PER SURFACE
          IF(NSSSYSTEM(27).LE.0.0D0) NSSSYSTEM(27)=20.0D0
C       SOURCE GRID DIMENSION
          IF(NSSSYSTEM(32).LE.0.0D0) NSSSYSTEM(32)=1.0D0
C       SOURCE GRID DIMENSION
          IF(NSSSYSTEM(36).LE.0.0D0) NSSSYSTEM(36)=1.0D0
C
C       NOW FOR THE SURFACE DATA
C       SET WAVELENGTHS FOR CATALOG LOOKUPS
C
          SAVE_KDP(23)=SAVEINPT(23)
          INPUT='GLASSWV NSSLENS'
          CALL PROCES
          REST_KDP(23)=RESTINPT(23)
C
          DO J=1,MAXS
C       FIND OUT IF THE CURRENT SURFACE #
C       IS SUPPOSED TO BE IN THE NSS DATABASE
              IF(NSSALENS(100,J).EQ.1.0D0) THEN
C       OUTPUT SURFACE NAME
                  IF(NSSSNAM(J)(1:80).EQ.NBL80) THEN
                      IF(J.LT.10) WRITE(STRING,5001) J
                      IF(J.LT.100) WRITE(STRING,5002) J
                      IF(J.LT.1000) WRITE(STRING,5003) J
                      IF(J.LT.10000) WRITE(STRING,5004) J
                      IF(J.LT.100000) WRITE(STRING,5005) J
                      IF(J.LT.1000000) WRITE(STRING,5006) J
                      IF(J.LT.10000000) WRITE(STRING,5007) J
                      IF(J.LT.100000000) WRITE(STRING,5008) J
                      IF(J.LT.1000000000) WRITE(STRING,5009) J
                      IF(J.GE.1000000000) WRITE(STRING,5010) J
                      NSSSNAM(J)=TRIM(STRING)
 5001                 FORMAT('SURFACE NAME IS ',I1)
 5002                 FORMAT('SURFACE NAME IS ',I2)
 5003                 FORMAT('SURFACE NAME IS ',I3)
 5004                 FORMAT('SURFACE NAME IS ',I4)
 5005                 FORMAT('SURFACE NAME IS ',I5)
 5006                 FORMAT('SURFACE NAME IS ',I6)
 5007                 FORMAT('SURFACE NAME IS ',I7)
 5008                 FORMAT('SURFACE NAME IS ',I8)
 5009                 FORMAT('SURFACE NAME IS ',I9)
 5010                 FORMAT('SURFACE NAME IS ',I10)
                  END IF
C       FIX CIRCULAR CLAPS AND HOLES
                  IF(NSSALENS(19,J).EQ.1.0D0) THEN
                      IF(DABS(NSSALENS(20,J)).GT.DABS(NSSALENS(21,J))) THEN
                          NSSALENS(20,J)=DABS(NSSALENS(20,J))
                          NSSALENS(21,J)=DABS(NSSALENS(20,J))
                      ELSE
                          NSSALENS(21,J)=DABS(NSSALENS(21,J))
                          NSSALENS(20,J)=DABS(NSSALENS(21,J))
                      END IF
                  END IF
                  IF(NSSALENS(25,J).EQ.1.0D0) THEN
                      IF(DABS(NSSALENS(26,J)).GT.DABS(NSSALENS(26,J))) THEN
                          NSSALENS(26,J)=DABS(NSSALENS(26,J))
                          NSSALENS(27,J)=DABS(NSSALENS(26,J))
                      ELSE
                          NSSALENS(27,J)=DABS(NSSALENS(27,J))
                          NSSALENS(26,J)=DABS(NSSALENS(27,J))
                      END IF
                  END IF
C       OUTPUT SURFACE STRA
                  IF(NSSALENS(89,J).EQ.0.0D0.AND.NSSALENS(90,J)
     1            .EQ.0.0D0.AND.NSSALENS(91,J).EQ.0.0D0)
     1            NSSALENS(89,J)=1.0D0
C
C       OPTICAL MATERIALS FIRST SPACE
                  IF(NSSGLASS1(1,J).EQ.'             '.AND.
     1            NSSGLASS1(2,J).EQ.'             ') THEN
                      NSSGLASS1(2,J)(1:13)='AIR          '
                  END IF
C       OPTICAL MATERIALS SECOND SPACE
                  IF(NSSGLASS2(1,J).EQ.'             '.AND.
     1            NSSGLASS2(2,J).EQ.'             ') THEN
                      NSSGLASS2(2,J)(1:13)='AIR          '
                  END IF
              ELSE
C       NO OUTPUT FOR THIS SURFACE
              END IF
          END DO
C       NOW DO AUTOMATIC BOUNDS BOXES FOR AS MANY SURFACES
C       AS IT IS POSSIBLE TO DO THEM
          DO J=1,MAXS
              IF(NSSALENS(100,J).EQ.1.0D0) THEN
                  IF(NSSALENS(1,J).EQ.4.0D0) THEN
                      IF(NSSBOUNDS(1,J).EQ.NSSBOUNDS(2,J).OR.
     1                NSSBOUNDS(3,J).EQ.NSSBOUNDS(4,J).OR.
     1                NSSBOUNDS(5,J).EQ.NSSBOUNDS(6,J)) THEN
                          WRITE(OUTLYNE,30) J
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,31)
                          CALL SHOWIT(1)
 30                       FORMAT('FOR USER-DEFINED SURFACE #',I6)
 31                       FORMAT('SURFACE BOUNDS MUST BE SET USING THE SBOUNDX,
     1  SBOUNDY AND SBOUNDZ COMMANDS')
                      END IF
                  END IF
                  IF(NSSALENS(1,J).EQ.5.0D0) THEN
                      IF(NSSBOUNDS(1,J).EQ.NSSBOUNDS(2,J).OR.
     1                NSSBOUNDS(3,J).EQ.NSSBOUNDS(4,J).OR.
     1                NSSBOUNDS(5,J).EQ.NSSBOUNDS(6,J)) THEN
                          WRITE(OUTLYNE,321) J
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,31)
                          CALL SHOWIT(1)
 321                      FORMAT('FOR MEM SURFACE #',I6)
                      END IF
                  END IF
                  IF(NSSALENS(1,J).EQ.6.0D0) THEN
C       TUBE NO BOUNDS USED
                  END IF
                  IF(NSSALENS(1,J).GE.1.0D0.AND.NSSALENS(1,J).LE.3.0D0) THEN
                      IF(NSSALENS(19,J).EQ.0.0D0) THEN
                      ELSE
                          IF(DABS(NSSALENS(20,J)).GT.DABS(NSSALENS(21,J))) THEN
                              R1=DABS(NSSALENS(20,J))
                          ELSE
                              R1=DABS(NSSALENS(21,J))
                          END IF
                          MINZ=1.0D20
                          MAXZ=-1.0D20
                          MINX=-R1+NSSALENS(22,J)
                          MAXX=+R1+NSSALENS(22,J)
                          MINY=-R1+NSSALENS(23,J)
                          MAXY=+R1+NSSALENS(23,J)
                          DELX=(MAXX-MINX)/20.0D0
                          DELY=(MAXY-MINY)/20.0D0
                          X=MINX
                          DO K=1,20
                              Y=MINY
                              X=X+DELX
                              DO L=1,20
                                  Y=Y+DELY
                                  IF(NSSALENS(1,J).NE.5.0D0) THEN
C       NOT MEM
                                      CALL NSSSAG(J,X,Y,SAG)
                                  END IF
                                  IF(NSSALENS(1,J).EQ.5.0D0) THEN
C       MEM SAG == 0.0D0
                                      SAG=0.0D0
                                  END IF
                                  IF(SAG.LT.MINZ) MINZ=SAG
                                  IF(SAG.GT.MAXZ) MAXZ=SAG
                              END DO
                          END DO
                          NSSBOUNDS(1,J)=MINX-(NUDGE(MINX))
                          NSSBOUNDS(2,J)=MAXX+(NUDGE(MAXX))
                          NSSBOUNDS(3,J)=MINY-(NUDGE(MINY))
                          NSSBOUNDS(4,J)=MAXY+(NUDGE(MAXY))
                          NSSBOUNDS(5,J)=MINZ-(NUDGE(MINZ))
                          NSSBOUNDS(6,J)=MAXZ+(NUDGE(MAXZ))
                      END IF
                  END IF
              END IF
          END DO
C
          DO J=1,MAXS
              IF(NSSALENS(100,J).NE.0.0D0) CALL NSSGLASSRES(J)
          END DO
C       RESOLVE OBJECT SPACE GLASS AND SET CURRENT GLASS AND INDEX
          CALL NSSGLASSRES(0)
C       NOW RESOLVE LINKAGES
          DO J=1,MAXS
              IF(NSSALENS(44,J).NE.DBLE(J).AND.NSSALENS(44,J).NE.0.0D0) THEN
C       RESOLVE SPROFILE LINK
                  K=INT(NSSALENS(44,J))
                  NSSALENS(1:7,J)=NSSALENS(1:7,K)
                  NSSALENS(10:30,J)=NSSALENS(10:30,K)
                  NSSALENS(201:400,J)=NSSALENS(201:400,K)
                  NSSBOUNDS(1:6,J)=NSSBOUNDS(1:6,K)
              END IF
              IF(NSSALENS(45,J).NE.DBLE(J).AND.NSSALENS(45,J).NE.0.0D0) THEN
C       RESOLVE SMEDIA LINK
                  K=INT(NSSALENS(45,J))
                  NSSALENS(101:140,J)=NSSALENS(101:140,K)
                  NSSGLASS1(1:2,J)=NSSGLASS1(1:2,K)
                  NSSGLASS2(1:2,J)=NSSGLASS2(1:2,K)
              END IF
              IF(NSSALENS(46,J).NE.DBLE(J).AND.NSSALENS(46,J).NE.0.0D0) THEN
C       RESOLVE SCOATING LINK
                  K=INT(NSSALENS(46,J))
                  NSSALENS(31:32,J)=NSSALENS(31:32,K)
              END IF
              IF(NSSALENS(47,J).NE.DBLE(J).AND.NSSALENS(47,J).NE.0.0D0) THEN
C       RESOLVE SINTERAC LINK
                  K=INT(NSSALENS(47,J))
C       INTERACTION CODES
                  NSSALENS(8:9,J)=NSSALENS(8:9,K)
C       GRATING DEFINITIONS
                  NSSALENS(141:145,J)=NSSALENS(141:145,K)
              END IF
          END DO
          RETURN
      END


      FUNCTION NUDGE(X)
          IMPLICIT NONE
          REAL*8 NUDGE,X
          IF(DABS(X).LT.1.0D0) NUDGE=0.001D0
          IF(DABS(X).GE.1.0D0) NUDGE=DABS(X)*0.001D0
          RETURN
      END


      SUBROUTINE NSSSAG(J,X,Y,SAG)
          USE NSSMOD
C
          IMPLICIT NONE
C
          INTEGER J
C
          REAL*8 X,Y,SAG,C,CT,D,DT,E,ET,F,FT,G,GT,K,KT
          REAL*8 D1,E1,MYF1,G1,H1,R1,R2,R3,R4,R,RHO,RHO2,C2
C
          INCLUDE 'datmai.inc'
          C=NSSALENS(3,J)
          CT=NSSALENS(206,J)
          NSSSAGERR=.FALSE.
C
          IF(NSSALENS(1,J).EQ.1.0D0) THEN
C       PLANO
              SAG=((((X**2)+(Y**2)))*   NSSALENS(3,J))+
     1            ((((X**2)+(Y**2))**2)*NSSALENS(4,J))+
     1            ((((X**2)+(Y**2))**3)*NSSALENS(5,J))+
     1            ((((X**2)+(Y**2))**4)*NSSALENS(6,J))+
     1            ((((X**2)+(Y**2))**5)*NSSALENS(7,J))+
     1            ((((X**2)+(Y**2))**6)*NSSALENS(201,J))+
     1            ((((X**2)+(Y**2))**7)*NSSALENS(202,J))+
     1            ((((X**2)+(Y**2))**8)*NSSALENS(203,J))+
     1            ((((X**2)+(Y**2))**9)*NSSALENS(204,J))+
     1            ((((X**2)+(Y**2))**10)*NSSALENS(205,J))
              RETURN
          END IF
C
          IF(NSSALENS(1,J).EQ.3.0D0) THEN
C       ANAMORPH
              K=NSSALENS(4,J)
              D=NSSALENS(5,J)
              E=NSSALENS(6,J)
              F=NSSALENS(7,J)
              G=NSSALENS(201,J)
              KT=NSSALENS(207,J)
              DT=NSSALENS(202,J)
              ET=NSSALENS(203,J)
              FT=NSSALENS(204,J)
              GT=NSSALENS(205,J)
              R1=D*((((1.0D0-DT)*(X**2))+((1.0D0+DT)*(Y**2)))**2)
              R2=E*((((1.0D0-ET)*(X**2))+((1.0D0+ET)*(Y**2)))**3)
              R3=F*((((1.0D0-FT)*(X**2))+((1.0D0+FT)*(Y**2)))**4)
              R4=G*((((1.0D0-GT)*(X**2))+((1.0D0+GT)*(Y**2)))**5)
              R=1.0D0-((KT+1.0D0)*(CT**2)*(X**2))-((K+1.0D0)*(C**2)*(Y**2))
              IF(R.LT.0.0D0) THEN
                  SAG=0.0D0
                  NSSSAGERR=.TRUE.
                  RETURN
              END IF
              SAG=((((CT*(X**2))+(C*(Y**2)))/(1.0D0+DSQRT(R))))
     1        +R1+R2+R3+R4
              RETURN
          END IF
C
          IF(NSSALENS(1,J).EQ.2.0D0) THEN
C     SPHERIC
              K=NSSALENS(4,J)
              D=NSSALENS(5,J)
              E=NSSALENS(6,J)
              F=NSSALENS(7,J)
              G=NSSALENS(201,J)
              D1=NSSALENS(202,J)
              E1=NSSALENS(203,J)
              MYF1=NSSALENS(204,J)
              G1=NSSALENS(205,J)
              H1=NSSALENS(206,J)
              RHO2=(X**2)+(Y**2)
              RHO=DSQRT(RHO2)
              C2=C**2
              R=(1.0D0-((K+1.0D0)*C2*RHO2))
              IF(R.LT.0.0D0) THEN
                  SAG=0.0D0
                  NSSSAGERR=.TRUE.
                  RETURN
              END IF
              SAG=((C*RHO2)/
     1        (1.0D0+DSQRT(R)))
     2        +(D*(RHO**4))
     2        +(E*(RHO**6))
     2        +(F*(RHO**8))
     2        +(G*(RHO**10))
     2        +(D1*(RHO**12))
     2        +(E1*(RHO**14))
     2        +(MYF1*(RHO**16))
     2        +(G1*(RHO**18))
     2        +(H1*(RHO**20))
              RETURN
          END IF
          IF(NSSALENS(1,J).EQ.4.0D0) THEN
C     USER
C       USER MUST CODE THIS
              SAG=0.0D0
              RETURN
          END IF
      END
      SUBROUTINE NSSGLASSRES(J)
          USE NSSMOD
C
          IMPLICIT NONE
C
          INTEGER J,K
C
          CHARACTER*13 NAME1,NAME2
C
          INCLUDE 'datmai.inc'
          IF(J.EQ.0) THEN
C       DO OBJECT SPACE INDECIES
              OBJINDEX(1:10)=1.0D0
              OBJINDEX(11:20)=0.0D0
              NAME1=NSSOGLASS(1)
              NAME2=NSSOGLASS(2)
              SAVE_KDP(23)=SAVEINPT(23)
              INPUT=NAME1//' '//NAME2
              CALL PROCES
              REST_KDP(23)=RESTINPT(23)

              IF(GPREG(1).NE.0.0D0) OBJINDEX(1)=GPREG(1)
              IF(GPREG(2).NE.0.0D0) OBJINDEX(2)=GPREG(2)
              IF(GPREG(3).NE.0.0D0) OBJINDEX(3)=GPREG(3)
              IF(GPREG(4).NE.0.0D0) OBJINDEX(4)=GPREG(4)
              IF(GPREG(5).NE.0.0D0) OBJINDEX(5)=GPREG(5)
              IF(GPREG(6).NE.0.0D0) OBJINDEX(6)=GPREG(6)
              IF(GPREG(7).NE.0.0D0) OBJINDEX(7)=GPREG(7)
              IF(GPREG(8).NE.0.0D0) OBJINDEX(8)=GPREG(8)
              IF(GPREG(9).NE.0.0D0) OBJINDEX(9)=GPREG(9)
              IF(GPREG(10).NE.0.0D0) OBJINDEX(10)=GPREG(10)
          ELSE
C       DO REGULAR INDECIES
              NSSALENS(101:110,J)=1.0D0
              NSSALENS(121:130,J)=1.0D0
              NSSALENS(111:120,J)=0.0D0
              NSSALENS(131:140,J)=0.0D0
              DO K=1,2
                  IF(K.EQ.1) THEN
                      NAME1=NSSGLASS1(1,J)
                      NAME2=NSSGLASS1(2,J)
                  ELSE
                      NAME1=NSSGLASS2(1,J)
                      NAME2=NSSGLASS2(2,J)
                  END IF
                  IF(NAME1.NE.'             ') THEN
                      SAVE_KDP(23)=SAVEINPT(23)
                      INPUT=NAME1//' '//NAME2
                      CALL PROCES
                      REST_KDP(23)=RESTINPT(23)
                      IF(K.EQ.1) THEN
                          IF(GPREG(1).NE.0.0D0) NSSALENS(101,J)=GPREG(1)
                          IF(GPREG(2).NE.0.0D0) NSSALENS(102,J)=GPREG(2)
                          IF(GPREG(3).NE.0.0D0) NSSALENS(103,J)=GPREG(3)
                          IF(GPREG(4).NE.0.0D0) NSSALENS(104,J)=GPREG(4)
                          IF(GPREG(5).NE.0.0D0) NSSALENS(105,J)=GPREG(5)
                          IF(GPREG(6).NE.0.0D0) NSSALENS(106,J)=GPREG(6)
                          IF(GPREG(7).NE.0.0D0) NSSALENS(107,J)=GPREG(7)
                          IF(GPREG(8).NE.0.0D0) NSSALENS(108,J)=GPREG(8)
                          IF(GPREG(9).NE.0.0D0) NSSALENS(109,J)=GPREG(9)
                          IF(GPREG(10).NE.0.0D0) NSSALENS(110,J)=GPREG(10)
                          NSSALENS(111:120,J)=0.0D0
                      ELSE
C       K WAS 2
                          IF(GPREG(1).NE.0.0D0) NSSALENS(121,J)=GPREG(1)
                          IF(GPREG(2).NE.0.0D0) NSSALENS(122,J)=GPREG(2)
                          IF(GPREG(3).NE.0.0D0) NSSALENS(123,J)=GPREG(3)
                          IF(GPREG(4).NE.0.0D0) NSSALENS(124,J)=GPREG(4)
                          IF(GPREG(5).NE.0.0D0) NSSALENS(125,J)=GPREG(5)
                          IF(GPREG(6).NE.0.0D0) NSSALENS(126,J)=GPREG(6)
                          IF(GPREG(7).NE.0.0D0) NSSALENS(127,J)=GPREG(7)
                          IF(GPREG(8).NE.0.0D0) NSSALENS(128,J)=GPREG(8)
                          IF(GPREG(9).NE.0.0D0) NSSALENS(129,J)=GPREG(9)
                          IF(GPREG(10).NE.0.0D0) NSSALENS(130,J)=GPREG(10)
                          NSSALENS(131:140,J)=0.0D0
                      END IF
                  END IF
              END DO
          END IF
          RETURN
      END


      SUBROUTINE NSSCALL
          USE NSSMOD
          IMPLICIT NONE
          INTEGER IUNIT,J,IV1,K,L
          REAL*8 V1,V2,V3,V4,V5,V0,MYX,NUDGE,SAG
          REAL*8 X,Y,DELY,DELX,MINX,MINY,MINZ,MAXX,MAXY,MAXZ,R1
          CHARACTER UNI*2,NSTRING*80,PTYPE*50,NNWORD*3,QWORD*13
     1    ,SWORD*13,CTYPE*4,ABL*20,BL80*80,VQ*8
          INCLUDE 'datmai.inc'
          EXTERNAL NUDGE
          ABL='                    '
          BL80=ABL//ABL//ABL//ABL
C       THIS CALLS ALL NSS DATABASE COMMAND SUBROUTINES AFTER DOING
C       COMMAND SYNTAX CHECKING
C
C       NSSORINT
          IF(WC.EQ.'NSSORINT') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1) THEN
                      WRITE(OUTLYNE,800)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,801)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,802)
                      CALL SHOWIT(1)
 800                  FORMAT('"NSSORINT" SETS THE "LOOK VECTOR" PARALLEL')
 801                  FORMAT('"TO THE LOCAL Z-AXIS OF THE SURFACE DESIGNATED')
 802                  FORMAT('"BY NUMERIC WORD #1')
                      RETURN
                  ELSE
                  END IF
C       CHECK SYNTAX
                  IF(SQ.EQ.1.OR.SST.EQ.1) THEN
                      OUTLYNE=
     1                '"NSSORINT" TAKES NO QUALIFIER OR STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"NSSORINT" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF1.EQ.1) THEN
                      OUTLYNE=
     1                '"NSSORINT" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W1.LT.0.0D0.OR.W1.GT.LASTSUR) THEN
                      OUTLYNE=
     1                'NUMERIC WORD #1, SURFACE #, BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V2=DBLE(DF2)
                  CALL NSSORINT(V1,V2)
                  RETURN
              ELSE
                  WRITE(OUTLYNE,*) 'NO NSS DATABASE EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) '"NSSORINT" ABORTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       NSSPOL
          IF(WC.EQ.'NSSPOL') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"NSSPOL" TURNS POLARIZATION CALC "ON" OR "OFF"'
                      CALL SHOWIT(1)
                      IF(NSSSYSTEM(39).EQ.1.0D0) OUTLYNE='"NSSPOL" IS CURRENTLY "ON"'
                      IF(NSSSYSTEM(39).EQ.0.0D0) OUTLYNE='"NSSPOL" IS CURRENTLY "OFF"'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='"NSSPOL" ONLY TAKES QUALIFIER INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.WQ.NE.'YES'.AND.
     1            WQ.NE.'NO') THEN
                      OUTLYNE='"NSSPOL" ONLY TAKES "YES", "NO", "ON" OR "OFF"'
                      CALL SHOWIT(1)
                      OUTLYNE='AS VALID QUALIFIERS'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WQ.EQ.'YES') NSSSYSTEM(39)=1.0D0
                  IF(WQ.EQ.'ON')  NSSSYSTEM(39)=1.0D0
                  IF(WQ.EQ.'NO')  NSSSYSTEM(39)=0.0D0
                  IF(WQ.EQ.'OFF') NSSSYSTEM(39)=0.0D0
                  RETURN
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='NO POLARIZATION SETTING CAN BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSINTER
          IF(WC.EQ.'NSSINTER') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE='"NSSINTER" SETS THE SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='INTERACTION CODE TO T, R OR A'
                  CALL SHOWIT(1)
                  OUTLYNE='FOR SPACES #1 AND #2'
                  CALL SHOWIT(1)
                  IF(NSSALENS(8,SFID).EQ.1.0D0)
     1            OUTLYNE='"NSSINTER" (SPACE#1) IS SET TO "T"'
                  IF(NSSALENS(8,SFID).EQ.2.0D0)
     1            OUTLYNE='"NSSINTER" (SPACE#1) IS SET TO "R"'
                  IF(NSSALENS(8,SFID).EQ.3.0D0)
     1            OUTLYNE='"NSSINTER" (SPACE#1) IS SET TO "A"'
                  CALL SHOWIT(1)
                  IF(NSSALENS(9,SFID).EQ.1.0D0)
     1            OUTLYNE='"NSSINTER" (SPACE#2) IS SET TO "T"'
                  IF(NSSALENS(9,SFID).EQ.2.0D0)
     1            OUTLYNE='"NSSINTER" (SPACE#2) IS SET TO "R"'
                  IF(NSSALENS(9,SFID).EQ.3.0D0)
     1            OUTLYNE='"NSSINTER" (SPACE#2) IS SET TO "A"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.
     1        S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"NSSINTER" ONLY TAKES QUALIFIER AND NUMERIC'
                  CALL SHOWIT(1)
                  OUTLYNE='WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.NE.'T'.AND.WQ.NE.'R'.AND.WQ.NE.'A') THEN
                  OUTLYNE='"NSSINTER" ONLY TAKES "T", "R" OR "A"'
                  CALL SHOWIT(1)
                  OUTLYNE='AS VALID QUALIFIERS'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.NE.1.0D0.AND.W1.NE.2.0D0) THEN
                  OUTLYNE='"NSSINTER" REQUIRES A 1 OR 2 FOR NUMERIC WORD 1'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'T'.AND.W1.EQ.1.0D0) NSSALENS(8,SFID)=1.0D0
              IF(WQ.EQ.'R'.AND.W1.EQ.1.0D0) NSSALENS(8,SFID)=2.0D0
              IF(WQ.EQ.'A'.AND.W1.EQ.1.0D0) NSSALENS(8,SFID)=3.0D0
              IF(WQ.EQ.'T'.AND.W1.EQ.2.0D0) NSSALENS(9,SFID)=1.0D0
              IF(WQ.EQ.'R'.AND.W1.EQ.2.0D0) NSSALENS(9,SFID)=2.0D0
              IF(WQ.EQ.'A'.AND.W1.EQ.2.0D0) NSSALENS(9,SFID)=3.0D0
              RETURN
          END IF
C
C       NSSLINK
          IF(WC.EQ.'NSSLINK') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE='"NSSLINK" SETS SURFACE NSS DATABASE LINKAGES'
                  CALL SHOWIT(1)
                  IF(NSSALENS(44,SFID).NE.DBLE(SFID).AND.
     1            NSSALENS(44,SFID).NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                '"SPROFILE" LINKED TO NSS SURFACE # ',NSSALENS(44,SFID)
                      CALL SHOWIT(1)
                  END IF
                  IF(NSSALENS(45,SFID).NE.DBLE(SFID).AND.
     1            NSSALENS(45,SFID).NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                '"SMEDIA" LINKED TO NSS SURFACE # ',NSSALENS(45,SFID)
                      CALL SHOWIT(1)
                  END IF
                  IF(NSSALENS(46,SFID).NE.DBLE(SFID).AND.
     1            NSSALENS(46,SFID).NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                '"SCOATING" LINKED TO NSS SURFACE # ',NSSALENS(46,SFID)
                      CALL SHOWIT(1)
                  END IF
                  IF(NSSALENS(47,SFID).NE.DBLE(SFID).AND.
     1            NSSALENS(47,SFID).NE.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                '"SINTERAC" LINKED TO NSS SURFACE # ',NSSALENS(47,SFID)
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  OUTLYNE='"NSSLINK TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0) THEN
                  OUTLYNE='"NSSLINK REQUIRES EXPLICIT QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1) THEN
                  IF(WQ.NE.'SPROFILE'.AND.WQ.NE.'SMEDIA'.AND.
     1            WQ.NE.'SCOATING'.AND.WQ.NE.'SINTERAC') THEN
                      OUTLYNE='"NSSLINK ONLY ACCEPTS QUALIFIERS:'
                      CALL SHOWIT(1)
                      OUTLYNE='             SPROFILE'
                      CALL SHOWIT(1)
                      OUTLYNE='             SMEDIA'
                      CALL SHOWIT(1)
                      OUTLYNE='             SCOATING'
                      CALL SHOWIT(1)
                      OUTLYNE='             SINTERAC'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"NSSLINK TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  OUTLYNE='"NSSLINK REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(.NOT.NEXISTN) THEN
                  OUTLYNE='NO NSS DATABASE EXISTS'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              V1=W1
              VQ=WQ
              CALL NSS_LINK(V1,VQ)
              RETURN
          END IF
C
C
C       NSSEOS
          IF(WC.EQ.'NSSEOS') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE='"NSSEOS" RESOLVES THE NSS DATABASE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"NSSEOS TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL NSSEOS
              RETURN
          END IF
C
C       NSSCOAT1
          IF(WC.EQ.'NSSCOAT1') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE='"NSSCOAT1" ASSIGNS A COATING 1 DEFINITION TO A SURFACE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1
     1        .OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"NSSCOAT1" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0.AND.W1.GT.1000.0D0) THEN
                  OUTLYNE='VALID COATING FILE NUMBERS RANGE FROM 0 TO 1000'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(.NOT.NEXISTN) THEN
                  OUTLYNE='NO NSS DATABASE EXISTS'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IV1=INT(W1)
              CALL NSS_COAT1(IV1)
              RETURN
          END IF
C
C       NSSCOAT2
          IF(WC.EQ.'NSSCOAT2') THEN
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  OUTLYNE='"NSSCOAT2" ASSIGNS A COATING 2 DEFINITION TO A SURFACE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1
     1        .OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"NSSCOAT2" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0.AND.W1.GT.1000.0D0) THEN
                  OUTLYNE='VALID COATING FILE NUMBERS RANGE FROM 0 TO 1000'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(.NOT.NEXISTN) THEN
                  OUTLYNE='NO NSS DATABASE EXISTS'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IV1=INT(W1)
              CALL NSS_COAT2(IV1)
              RETURN
          END IF
C       NSSDEL
          IF(WC.EQ.'NSSDEL') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE='"NSSDEL" DELETES THE EXISTING NSS DATABASE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"NSSDEL" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(NEXISTN) THEN
                  CALL DEL_NSS
                  RETURN
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS TO BE DELETED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C       NSSDET
          IF(WC.EQ.'NSSDET') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE='"NSSDET" SETS THE CURRENT SURFACE TO'
                  CALL SHOWIT(1)
                  OUTLYNE='BE THE DETECTOR SURFACE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"NSSDET" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(NEXISTN) THEN
                  NSSSYSTEM(49)=DBLE(SFID)
                  DETSNUM=SFID
                  RETURN
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS'
                  CALL SHOWIT(1)
                  OUTLYNE='NO DETECTOR SURFACE CAN BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSNEW
          IF(WC.EQ.'NSSNEW') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE='"NSSNEW" CREATES A NEW NSS DATABASE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"NSSNEW" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL NEW_NSS
              RETURN
          END IF
C
C       NSSUNITS
          IF(WC.EQ.'NSSUNITS') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"NSSUNITS" SETS NSS DATABASE UNITS'
                      CALL SHOWIT(1)
                      IF(NSSSYSTEM(21).EQ.1.0D0) UNI='IN'
                      IF(NSSSYSTEM(21).EQ.2.0D0) UNI='CM'
                      IF(NSSSYSTEM(21).EQ.3.0D0) UNI='MM'
                      IF(NSSSYSTEM(21).EQ.4.0D0) UNI=' M'
                      OUTLYNE='CURRENT NSS UNITS ARE: '//UNI
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='"NSSUNITS" TAKES NO STRING OR NUMERIC WORD INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WQ(1:2).NE.'IN'.AND.WQ(1:2).NE.'CM'.AND.WQ(1:2).NE.'MM'
     1            .AND.WQ(1:2).NE.'M ') THEN
                      OUTLYNE='"NSSUNITS" TAKES EITHER "IN", "CM", "MM OR "M "'
                      CALL SHOWIT(1)
                      OUTLYNE='AS VALID QUALIFIER INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      IF(WQ(1:2).EQ.'IN') IUNIT=1
                      IF(WQ(1:2).EQ.'CM') IUNIT=2
                      IF(WQ(1:2).EQ.'MM') IUNIT=3
                      IF(WQ(1:2).EQ.'M ') IUNIT=4
                      CALL NSS_UNITS(IUNIT)
                      RETURN
                  END IF
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NSS UNITS CAN NOT BE SET'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSWV
 10       FORMAT('WAVELENGTH #',A2,' = ',G13.6,' MICROMETER, WT = ',G13.6)
          IF(WC.EQ.'NSSWV') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"NSSWV" SETS NSS WAVELENGTHS 1 TO 10'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 1',NSSSYSTEM(1),NSSSYSTEM(11)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 2',NSSSYSTEM(2),NSSSYSTEM(12)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 3',NSSSYSTEM(3),NSSSYSTEM(13)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 4',NSSSYSTEM(4),NSSSYSTEM(14)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 5',NSSSYSTEM(5),NSSSYSTEM(15)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 6',NSSSYSTEM(6),NSSSYSTEM(16)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 7',NSSSYSTEM(7),NSSSYSTEM(17)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 8',NSSSYSTEM(8),NSSSYSTEM(18)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 9',NSSSYSTEM(9),NSSSYSTEM(19)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) '10',NSSSYSTEM(10),NSSSYSTEM(20)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE='"NSSWV" TAKES NO STRING, QUALIFIER OR NUMERIC WORD'
                      CALL SHOWIT(1)
                      OUTLYNE='#3, #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(INT(W1).NE.1.AND.INT(W1).NE.2.AND.
     1            INT(W1).NE.3.AND.INT(W1).NE.4.AND.INT(W1).NE.5.AND.
     1            INT(W1).NE.6.AND.INT(W1).NE.7.AND.INT(W1).NE.8.AND.
     1            INT(W1).NE.9.AND.INT(W1).NE.10) THEN
                      OUTLYNE='NUMERIC WORD #1 MUST BE 1,2,3,4,5,6,7,8,9 OR 10'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  CALL NSS_WV(V1,V2)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NSS WAVELENGTHS'
                  CALL SHOWIT(1)
                  OUTLYNE='CAN NOT BE SET'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSWT
          IF(WC.EQ.'NSSWT') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"NSSWT" SETS NSS WAVELENGTH WEIGHTS 1 TO 10'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 1',NSSSYSTEM(1),NSSSYSTEM(11)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 2',NSSSYSTEM(2),NSSSYSTEM(12)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 3',NSSSYSTEM(3),NSSSYSTEM(13)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 4',NSSSYSTEM(4),NSSSYSTEM(14)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 5',NSSSYSTEM(5),NSSSYSTEM(15)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 6',NSSSYSTEM(6),NSSSYSTEM(16)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 7',NSSSYSTEM(7),NSSSYSTEM(17)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 8',NSSSYSTEM(8),NSSSYSTEM(18)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) ' 9',NSSSYSTEM(9),NSSSYSTEM(19)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,10) '10',NSSSYSTEM(10),NSSSYSTEM(20)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE='"NSSWT" TAKES NO STRING, QUALIFIER OR NUMERIC WORD'
                      CALL SHOWIT(1)
                      OUTLYNE='#3, #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(INT(W1).NE.1.AND.INT(W1).NE.2.AND.
     1            INT(W1).NE.3.AND.INT(W1).NE.4.AND.INT(W1).NE.5.AND.
     1            INT(W1).NE.6.AND.INT(W1).NE.7.AND.INT(W1).NE.8.AND.
     1            INT(W1).NE.9.AND.INT(W1).NE.10) THEN
                      OUTLYNE='NUMERIC WORD #1 MUST BE 1,2,3,4,5,6,7,8,9 OR 10'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  CALL NSS_WT(V1,V2)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NSS WAVELENGTH'
                  CALL SHOWIT(1)
                  OUTLYNE='WEIGHTS CAN NOT BE SET'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       UNIVERSE
          IF(WC.EQ.'UNIVERSE') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      IF(NSSSYSTEM(21).EQ.1.0D0) UNI='IN'
                      IF(NSSSYSTEM(21).EQ.2.0D0) UNI='CM'
                      IF(NSSSYSTEM(21).EQ.3.0D0) UNI='MM'
                      IF(NSSSYSTEM(21).EQ.4.0D0) UNI=' M'
                      OUTLYNE='"UNIVERSE" SETS THE TERMINAL RAY DISTANCE'
                      CALL SHOWIT(1)
 24                   FORMAT('UNIVERSE RADIUS = ',G13.6,1X,A2)
                      WRITE(OUTLYNE,24) NSSSYSTEM(25),UNI
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S5.EQ.1.OR.S4.EQ.1.OR.S3.EQ.1
     1            .OR.S2.EQ.1) THEN
                      OUTLYNE='"UNIVERSE" ONLY TAKES NUMERIC WORD #1'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W1.LT.0.0D0) THEN
                      OUTLYNE='TERMINAL RAY DISTANCE MUST BE GREATER OR EQUAL TO ZERO'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W1.GE.0.0D0) THEN
                      V1=W1
                      CALL NSS_UNIVERSE(V1)
                  END IF
              ELSE
                  OUTLYNE=
     1            'NO NSS DATABASE EXISTS. NO TERMINAL RAY DISTANCE CAN BE SET'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SPOS
          IF(WC.EQ.'SPOS') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      IF(NSSSYSTEM(21).EQ.1.0D0) UNI='IN'
                      IF(NSSSYSTEM(21).EQ.2.0D0) UNI='CM'
                      IF(NSSSYSTEM(21).EQ.3.0D0) UNI='MM'
                      IF(NSSSYSTEM(21).EQ.4.0D0) UNI=' M'
                      OUTLYNE='"SPOS" SETS THE CURRENT SURFACE POSITION'
                      CALL SHOWIT(1)
 30                   FORMAT('CURRENT SURFACE X-POSITION = ',G13.6,1X,A2)
 31                   FORMAT('CURRENT SURFACE Y-POSITION = ',G13.6,1X,A2)
 32                   FORMAT('CURRENT SURFACE Z-POSITION = ',G13.6,1X,A2)
 321                  FORMAT('REFERENCE SURFACE NUMBER = ',I5)
                      WRITE(OUTLYNE,30) NSSALENS(34,SFID),UNI
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,31) NSSALENS(35,SFID),UNI
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,32) NSSALENS(36,SFID),UNI
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,321) INT(NSSALENS(37,SFID))
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE='"SPOS" ONLY TAKES NUMERIC WORDS #1, #2, #3 AND #4'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  V3=W3
                  V4=W4
                  IF(DF4.EQ.1) V4=-1.0D0
                  CALL NSS_SPOS(V1,V2,V3,V4)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SURFACE POSITION CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SROT
          IF(WC.EQ.'SROT') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SN.EQ.0.AND.SQ.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"SROT" SETS THE CURRENT SURFACE ROTATION ANGLES'
                      CALL SHOWIT(1)
 36                   FORMAT('CURRENT SURFACE ALPHA = ',G13.6,1X,'DEGREES')
 37                   FORMAT('CURRENT SURFACE BETA  = ',G13.6,1X,'DEGREES')
 38                   FORMAT('CURRENT SURFACE GAMMA = ',G13.6,1X,'DEGREES')
                      WRITE(OUTLYNE,36) NSSALENS(40,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,37) NSSALENS(41,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,38) NSSALENS(42,SFID)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE='"SROT" ONLY TAKES NUMERIC WORDS #1, #2, #3 AND #4'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  V3=W3
                  CALL NSS_SROT(V1,V2,V3)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SURFACE ROTATION CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SGRT
          IF(WC.EQ.'SGRT') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      IF(NSSSYSTEM(21).EQ.1.0D0) UNI='IN'
                      IF(NSSSYSTEM(21).EQ.2.0D0) UNI='CM'
                      IF(NSSSYSTEM(21).EQ.3.0D0) UNI='MM'
                      IF(NSSSYSTEM(21).EQ.4.0D0) UNI='M '
                      OUTLYNE='"SGRT" SETS THE CURRENT SURFACE TO BE A LINEAR'
                      CALL SHOWIT(1)
                      OUTLYNE='DIFFRACTION GRATING'
                      CALL SHOWIT(1)
 436                  FORMAT('CURRENT GRATING ORDER     = ',G13.6)
 437                  FORMAT('CURRENT GRATING SPACING   = ',G13.6,1X,A2)
 438                  FORMAT('CURRENT GRATING L-DIR COS.= ',G13.6)
 439                  FORMAT('CURRENT GRATING M-DIR COS.= ',G13.6)
 440                  FORMAT('CURRENT GRATING N-DIR COS.= ',G13.6)
                      WRITE(OUTLYNE,436) NSSALENS(141,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,437) NSSALENS(142,SFID),UNI
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,438) NSSALENS(143,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,439) NSSALENS(144,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,440) NSSALENS(145,SFID)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                      OUTLYNE='"SGRT" TAKES NO QUALIFIER OR STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                      OUTLYNE='"SGRT" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  V3=W3
                  V4=W4
                  V5=W5
                  IF(DF3.EQ.1) W3=1.0D0
                  IF(DF4.EQ.1) W4=0.0D0
                  IF(DF5.EQ.1) W5=0.0D0
                  NSSALENS(146,SFID)=1.0D0
                  NSSALENS(141,SFID)=V1
                  NSSALENS(142,SFID)=V2
                  NSSALENS(143,SFID)=V3
                  NSSALENS(144,SFID)=V4
                  NSSALENS(145,SFID)=V5
                  NSSALENS(146,SFID)=1.0D0
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO DIFFRACTION GRATING CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C
C       SGRTD
          IF(WC.EQ.'SGRTD') THEN
              IF(NEXISTN) THEN
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='"SGRTD" TAKES NO INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  NSSALENS(141:146,SFID)=0.0D0
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO DIFFRACTION GRATING'
                  CALL SHOWIT(1)
                  OUTLYNE='DELETION BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SBOUNDX
          IF(WC.EQ.'SBOUNDX') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"SBOUNDX" SETS THE CURRENT SURFACE X-LIMITS'
                      CALL SHOWIT(1)
 136                  FORMAT('CURRENT -X LIMIT = ',G13.6)
 137                  FORMAT('CURRENT +X LIMIT = ',G13.6)
                      WRITE(OUTLYNE,136) NSSBOUNDS(1,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,137) NSSBOUNDS(2,SFID)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S3.EQ.1) THEN
                      OUTLYNE='"SBOUNDX" ONLY TAKES NUMERIC WORDS #1 AND #2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  CALL NSS_SBOUNDX(V1,V2)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SURFACE X-BOUNDS CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SBOUNDY
          IF(WC.EQ.'SBOUNDY') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"SBOUNDY" SETS THE CURRENT SURFACE X-LIMITS'
                      CALL SHOWIT(1)
 236                  FORMAT('CURRENT -Y LIMIT = ',G13.6)
 237                  FORMAT('CURRENT +Y LIMIT = ',G13.6)
                      WRITE(OUTLYNE,236) NSSBOUNDS(3,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,237) NSSBOUNDS(4,SFID)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S3.EQ.1) THEN
                      OUTLYNE='"SBOUNDY" ONLY TAKES NUMERIC WORDS #1 AND #2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  CALL NSS_SBOUNDY(V1,V2)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SURFACE Y-BOUNDS CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SBOUNDZ
          IF(WC.EQ.'SBOUNDZ') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"SBOUNDZ" SETS THE CURRENT SURFACE Z-LIMITS'
                      CALL SHOWIT(1)
 336                  FORMAT('CURRENT -Z LIMIT = ',G13.6)
 337                  FORMAT('CURRENT +Z LIMIT = ',G13.6)
                      WRITE(OUTLYNE,336) NSSBOUNDS(5,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,337) NSSBOUNDS(6,SFID)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S3.EQ.1) THEN
                      OUTLYNE='"SBOUNDZ" ONLY TAKES NUMERIC WORDS #1 AND #2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  CALL NSS_SBOUNDZ(V1,V2)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SURFACE Z-BOUNDS CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SURFACE
          IF(WC.EQ.'SURFACE') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"SURFACE" SETS THE CURRENT SURFACE NUMBER'
                      CALL SHOWIT(1)
! 45    FORMAT('CURRENT SURFACE NUMBER = ',I5)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1
     1            .OR.S2.EQ.1.OR.S3.EQ.1) THEN
                      OUTLYNE='"SURFACE" ONLY TAKES NUMERIC WORD #1'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W1.LT.1.0D0) THEN
                      OUTLYNE='"SURFACE" NUMBER MUST BE GREATER THAN OR EQUAL TO 1'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
301               IF(INT(V1).GE.MAXS) THEN
C       MAKE NSS DATABASE BIGGER
                      CALL NSS_DOUBLE
                      GO TO 301
                  END IF
                  IF(LASTSUR.LT.SFID) LASTSUR=SFID
                  NSSALENS(100,INT(V1))=1.0D0
                  CALL NSS_SURFACE(V1)
                  IF(NSSSNAM(INT(V1)).EQ.BL80) THEN
                      IF(INT(V1).LT.10) WRITE(NSTRING,4301)INT(V1)
                      IF(INT(V1).LT.100) WRITE(NSTRING,4302)INT(V1)
                      IF(INT(V1).LT.1000) WRITE(NSTRING,4303)INT(V1)
                      IF(INT(V1).LT.10000) WRITE(NSTRING,4304)INT(V1)
                      IF(INT(V1).LT.100000) WRITE(NSTRING,4305)INT(V1)
                      IF(INT(V1).LT.1000000) WRITE(NSTRING,4306)INT(V1)
                      IF(INT(V1).LT.10000000) WRITE(NSTRING,4307)INT(V1)
                      IF(INT(V1).LT.100000000) WRITE(NSTRING,4308)INT(V1)
                      IF(INT(V1).LT.1000000000) WRITE(NSTRING,4309)INT(V1)
                      IF(INT(V1).GE.1000000000) WRITE(NSTRING,4310) INT(V1)
                      CALL NSS_SNAM(NSTRING)
 4301                 FORMAT('SURFACE NUMBER IS ',I1)
 4302                 FORMAT('SURFACE NUMBER IS ',I2)
 4303                 FORMAT('SURFACE NUMBER IS ',I3)
 4304                 FORMAT('SURFACE NUMBER IS ',I4)
 4305                 FORMAT('SURFACE NUMBER IS ',I5)
 4306                 FORMAT('SURFACE NUMBER IS ',I6)
 4307                 FORMAT('SURFACE NUMBER IS ',I7)
 4308                 FORMAT('SURFACE NUMBER IS ',I8)
 4309                 FORMAT('SURFACE NUMBER IS ',I9)
 4310                 FORMAT('SURFACE NUMBER IS ',I10)
                  END IF
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SURFACE CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SNAME
          IF(WC.EQ.'SNAME') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"SNAME" SETS THE CURRENT SURFACE NAME'
                      CALL SHOWIT(1)
 46                   FORMAT('CURRENT SURFACE NAME = ',A80)
                      WRITE(OUTLYNE,46) NSSSNAM(SFID)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SN.EQ.1.OR.SQ.EQ.1) THEN
                      OUTLYNE='"SNAME" ONLY TAKES STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  NSTRING=WS
                  CALL NSS_SNAM(NSTRING)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SURFACE NAME CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SPROFILE
          IF(WC.EQ.'SPROFILE') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"SPROFILE" SETS THE CURRENT SURFACE PROFILE'
                      CALL SHOWIT(1)
                      IF(NSSALENS(1,SFID).EQ.1.0D0)
     1                PTYPE='PLANO (POSSIBLY WITH ASPHERIC TERMS)'
                      IF(NSSALENS(1,SFID).EQ.2.0D0)
     1                PTYPE='SPHERIC (SPHERICAL,CONIC OR ASPHERIC)'
                      IF(NSSALENS(1,SFID).EQ.3.0D0)
     1                PTYPE='ANAMORPH (ANAMORPHIC OR ANAMORPHIC ASPHERIC)'
                      IF(NSSALENS(1,SFID).EQ.4.0D0)
     1                PTYPE='USER-DEFINED (IN NSSUSER.FOR IN NSSFILE1.FOR)'
                      IF(NSSALENS(1,SFID).EQ.5.0D0)
     1                PTYPE='MEM (MULTIPLE ELEMENT MIRROR)'
                      IF(NSSALENS(1,SFID).EQ.6.0D0)
     1                PTYPE='TUBE (CYLINDER) SURFACE'
C
 47                   FORMAT('CURRENT SURFACE PROFILE TYPE =',A40)
 48                   FORMAT('PARAMETER #1 = ',G13.6)
 49                   FORMAT('PARAMETER #2 = ',G13.6)
 50                   FORMAT('PARAMETER #3 = ',G13.6)
 51                   FORMAT('PARAMETER #4 = ',G13.6)
 52                   FORMAT('PARAMETER #5 = ',G13.6)
 481                  FORMAT('TUBE RADIUS  = ',G13.6)
 491                  FORMAT('TUBE LENGTH  = ',G13.6)
                      IF(NSSALENS(1,SFID).GE.1.0D0.AND.NSSALENS(1,SFID).LE.5.0D0) THEN
                          WRITE(OUTLYNE,47) PTYPE
                          CALL SHOWIT(1)
                          IF(NSSALENS(3,SFID).EQ.0.0D0)
     1                    WRITE(OUTLYNE,48) NSSALENS(3,SFID)
                          IF(NSSALENS(3,SFID).NE.0.0D0)
     1                    WRITE(OUTLYNE,48) 1.0D0/NSSALENS(3,SFID)
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,49) NSSALENS(4,SFID)
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,50) NSSALENS(5,SFID)
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,51) NSSALENS(6,SFID)
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,52) NSSALENS(7,SFID)
                          CALL SHOWIT(1)
                      END IF
                      IF(NSSALENS(1,SFID).EQ.6.0D0) THEN
                          WRITE(OUTLYNE,47) PTYPE
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,481) NSSALENS(10,SFID)
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,491) NSSALENS(11,SFID)
                          CALL SHOWIT(1)
                      END IF
                      RETURN
                  END IF
                  IF(SST.EQ.1) THEN
                      OUTLYNE='"SPROFILE" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.1) THEN
                      IF(WQ.NE.'PLANO'.AND.WQ.NE.'SPHERIC'.AND.
     1                WQ.NE.'ANAMORPH'.AND.WQ.NE.'TUBE'.AND.
     1                WQ.NE.'USER'.AND.WQ.NE.'MEM') THEN
                          WRITE(OUTLYNE,*)'VALID "SPROFILE" QUALIFIERS ARE:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'PLANO'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'SPHERIC'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'ANAMORP'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'MEM'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'USER'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'TUBE'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
                  IF(WQ.EQ.'PLANO   ') V0=1.0D0
                  IF(WQ.EQ.'SPHERIC ') V0=2.0D0
                  IF(WQ.EQ.'ANAMORPH') V0=3.0D0
                  IF(WQ.EQ.'USER    ') V0=4.0D0
                  IF(WQ.EQ.'MEM')      V0=5.0D0
                  IF(WQ.EQ.'TUBE')     V0=6.0D0
                  V1=W1
                  V2=W2
                  V3=W3
                  V4=W4
                  V5=W5
                  IF(WQ.EQ.'USER') THEN
                      OUTLYNE='THE NSS USER DEFINED SURFACE IS NOT YET OPERATIONAL'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL NSS_SPROFILE(V0,V1,V2,V3,V4,V5)
                  IF(NSSALENS(1,SFID).GE.1.0D0.AND.NSSALENS(1,SFID).LE.3.0D0.OR.
     1            NSSALENS(1,SFID).EQ.5.0D0) THEN
C       NOW DO AUTOMATIC BOUNDS BOXES FOR SURFACE SFID IF POSSIBLE
                      IF(NSSALENS(19,SFID).NE.0.0D0) THEN
C       CLAPS PRESENT
                          IF(DABS(NSSALENS(20,SFID)).GT.DABS(NSSALENS(21,SFID))) THEN
                              R1=DABS(NSSALENS(20,SFID))
                          ELSE
                              R1=DABS(NSSALENS(21,SFID))
                          END IF
                          MINZ=1.0D20
                          MAXZ=-1.0D20
                          MINX=-R1+NSSALENS(22,SFID)
                          MAXX=+R1+NSSALENS(22,SFID)
                          MINY=-R1+NSSALENS(23,SFID)
                          MAXY=+R1+NSSALENS(23,SFID)
                          DELX=(MAXX-MINX)/20.0D0
                          DELY=(MAXY-MINY)/20.0D0
                          X=MINX
                          DO K=1,20
                              Y=MINY
                              X=X+DELX
                              DO L=1,20
                                  Y=Y+DELY
                                  CALL NSSSAG(SFID,X,Y,SAG)
                                  IF(SAG.LT.MINZ) MINZ=SAG
                                  IF(SAG.GT.MAXZ) MAXZ=SAG
                              END DO
                          END DO
                          NSSBOUNDS(1,SFID)=MINX-(NUDGE(MINX))
                          NSSBOUNDS(2,SFID)=MAXX+(NUDGE(MAXX))
                          NSSBOUNDS(3,SFID)=MINY-(NUDGE(MINY))
                          NSSBOUNDS(4,SFID)=MAXY+(NUDGE(MAXY))
                          NSSBOUNDS(5,SFID)=MINZ-(NUDGE(MINZ))
                          NSSBOUNDS(6,SFID)=MAXZ+(NUDGE(MAXZ))
                      END IF
                  END IF
                  IF(NSSALENS(1,SFID).EQ.6.0D0) THEN
C       AUTOMATICALLY SET BOUNDS
                      MYX=DABS(NSSALENS(10,SFID))
                      NSSBOUNDS(1,SFID)=-DABS(NSSALENS(10,SFID))-NUDGE(MYX)
                      NSSBOUNDS(2,SFID)= DABS(NSSALENS(10,SFID))+NUDGE(MYX)
                      NSSBOUNDS(3,SFID)=-DABS(NSSALENS(10,SFID))-NUDGE(MYX)
                      NSSBOUNDS(4,SFID)= DABS(NSSALENS(10,SFID))+NUDGE(MYX)
                      MYX=DABS(NSSALENS(11,SFID))
                      NSSBOUNDS(5,SFID)=0.0D0-NUDGE(MYX)
                      NSSBOUNDS(6,SFID)=DABS(NSSALENS(11,SFID))+NUDGE(MYX)
                  END IF
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SURFACE NAME CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SPARAM
          IF(WC.EQ.'SPARAM') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"SPARAM" SETS THE SURFACE PARAMETER SPECIFIED'
                      CALL SHOWIT(1)
                      OUTLYNE='IN NUMERIC WORD #1 TO THE VALUE SPECIFIED IN'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD #2'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE='"SPARAM" ONLY USES NUMERIC WORDS #1 AND #2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(INT(W1).LT.1.OR.INT(W1).GT.200) THEN
                      OUTLYNE='INVALID PARAMERTER NUMBER INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='VALID PARAMETER NUMBERS RANGE FROM 1 TO 200'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  CALL NSS_SPARAM(V1,V2)
C       CLEAR BOUNDS ASSIGNMENTS SINCE SURFACE PARAMETERS CHANGE
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SURFACE PARAMETERS CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SCLEAR
          IF(WC.EQ.'SCLEAR') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1) THEN
                      OUTLYNE='"SCLEAR" REMOVES THE CURRENT NSS SURFACE FROM'
                      CALL SHOWIT(1)
                      OUTLYNE='THE NSS DATABASE.'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='"SCLEAR" TAKES NO ADDITIONAL INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL NSS_SCLEAR
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SURFACE CAN BE MODIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SUNCLEAR
          IF(WC.EQ.'SUNCLEAR') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1) THEN
                      OUTLYNE='"SUNCLEAR" ADDS THE CURRENT NSS SURFACE TO'
                      CALL SHOWIT(1)
                      OUTLYNE='THE NSS DATABASE.'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='"SUNCLEAR" TAKES NO ADDITIONAL INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL NSS_SUNCLEAR
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SURFACE CAN BE MODIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSMINE
          IF(WC.EQ.'NSSMINE') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"NSSMINE" SETS THE MINIMUM FRACTIONAL RAY ENERGY'
                      CALL SHOWIT(1)
58                    FORMAT('CURRENT MINIMUM FRACTIONAL RAY ENERGY  = ',G13.6)
                      WRITE(OUTLYNE,58) NSSSYSTEM(26)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S3.EQ.1
     1            .OR.S2.EQ.1) THEN
                      OUTLYNE='"NSSMINE" ONLY USES NUMERIC WORD #1'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  CALL NSS_MINE(V1)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO MIN ENERGY PARAMETER CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSMHIT
          IF(WC.EQ.'NSSMHIT') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"NSSMHIT" SETS THE MAXIMUM NUMBER OF RAY/SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE='INTERSECTIONS PER RAY'
                      CALL SHOWIT(1)
59                    FORMAT('CURRENT MAXIMUM RAY/SURFACE NUMBER IS  = ',G13.6)
                      WRITE(OUTLYNE,59) NSSSYSTEM(27)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S3.EQ.1
     1            .OR.S2.EQ.1) THEN
                      OUTLYNE='"NSSMHIT" ONLY USES NUMERIC WORD #1'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  CALL NSS_MHIT(V1)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO MAX RAY/SURFACE
     1 PARAMETER CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSSPLIT
          IF(WC.EQ.'NSSSPLIT') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"NSSSPLIT" SETS RAY SPLITTING TO "ON" OR "OFF"'
                      CALL SHOWIT(1)
                      IF(NSSSYSTEM(28).EQ.0.0D0) NNWORD='OFF'
                      IF(NSSSYSTEM(28).EQ.1.0D0) NNWORD='ON'
95                    FORMAT('CURRENT CURRENT SETTING IS  = ',A2)
                      WRITE(OUTLYNE,95) NNWORD
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='"NSSSPLIT" ONLY USES QUALIFIER INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.WQ.NE.'YES'
     1            .AND.WQ.NE.'NO') THEN
                      OUTLYNE='"NSSSPLIT" ONLY TAKES "ON", "OFF", "YES" OR "NO"'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WQ.EQ.'ON'.OR.WQ.EQ.'YES') V1=1.0D0
                  IF(WQ.EQ.'OFF'.OR.WQ.EQ.'NO') V1=0.0D0
                  CALL NSS_SPLIT(V1)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO NSS RAY SPLITTING CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSOBJ
          IF(WC.EQ.'NSSOBJ') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"NSSOBJ" SETS THE SOURCE GRID CENTER LOCATION'
                      CALL SHOWIT(1)
60                    FORMAT('CURRENT SOURCE GRID X-POSITION  = ',G13.6)
61                    FORMAT('CURRENT SOURCE GRID Y-POSITION  = ',G13.6)
62                    FORMAT('CURRENT SOURCE GRID Z-POSITION  = ',G13.6)
                      WRITE(OUTLYNE,60) NSSSYSTEM(29)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,61) NSSSYSTEM(30)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,62) NSSSYSTEM(31)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"NSSOBJ" ONLY USES QUALIFIER AND NUMERIC WORDS #1, #2 AND #3'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.0) WQ='REAL'
                  IF(WQ.NE.'REAL'.AND.WQ.NE.'VIRTUAL') THEN
                      OUTLYNE=
     1                '"NSSOBJ" ONLY TAKES "REAL" AND "VIRTUAL" AS VALID QUALIFIERS'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  V3=W3
                  VQ=WQ
                  CALL NSS_OBJ(VQ,V1,V2,V3)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SOURCE POSITION CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSREF
          IF(WC.EQ.'NSSREF') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"NSSREF" SETS THE REFERENCE GRID CENTER LOCATION'
                      CALL SHOWIT(1)
601                   FORMAT('CURRENT REFERENCE GRID X-POSITION  = ',G13.6)
611                   FORMAT('CURRENT REFERENCE GRID Y-POSITION  = ',G13.6)
621                   FORMAT('CURRENT REFERENCE GRID Z-POSITION  = ',G13.6)
                      WRITE(OUTLYNE,601) NSSSYSTEM(40)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,611) NSSSYSTEM(41)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,621) NSSSYSTEM(42)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
                      OUTLYNE=
     1                '"NSSREF" ONLY USES NUMERIC WORDS #1, #2 AND #3'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  V3=W3
                  CALL NSS_REF(V1,V2,V3)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO REFERENCE GRID POSITION CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSGRIDS
          IF(WC.EQ.'NSSGRIDS') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE=
     1                '"NSSGRIDS" SETS THE SOURCE GRID SIZE, SHAPE AND SPACING'
                      CALL SHOWIT(1)
66                    FORMAT('CURRENT SOURCE GRID DIMENSIONALITY IS: ',I8,' BY ',I8)
                      WRITE(OUTLYNE,66) INT(NSSSYSTEM(32)),INT(NSSSYSTEM(32))
                      CALL SHOWIT(1)
                      IF(NSSSYSTEM(21).EQ.1.0D0) UNI='IN'
                      IF(NSSSYSTEM(21).EQ.2.0D0) UNI='CM'
                      IF(NSSSYSTEM(21).EQ.3.0D0) UNI='MM'
                      IF(NSSSYSTEM(21).EQ.4.0D0) UNI=' M'
                      WRITE(OUTLYNE,671) NSSSYSTEM(38),UNI
661                   FORMAT('CURRENT SOURCE GRID SPACING  = ',D23.15,' ',A2)
                      WRITE(OUTLYNE,661) NSSSYSTEM(37),UNI
                      CALL SHOWIT(1)
                      IF(NSSSYSTEM(33).EQ.0.0D0) WRITE(OUTLYNE,664)
                      IF(NSSSYSTEM(33).EQ.1.0D0) WRITE(OUTLYNE,663)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
664               FORMAT('CURRENT SOURCE GRID IS CLIPPED CIRCULARLY')
663               FORMAT('CURRENT SOURCE GRID IS A FULL RECTANGLE')
                  IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S3.EQ.1) THEN
                      OUTLYNE=
     1                '"NSSGRIDS" ONLY USES QUALIFIER AND NUMERIC WORDS #1 and #2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.0) THEN
                      WQ='CIRC'
                      SQ=1
                  END IF
                  IF(WQ.NE.'CIRC'.AND.WQ.NE.'RECT') THEN
                      OUTLYNE=
     1                '"NSSGRIDS" ONLY "CIRC" AND "RECT" AS QUALIFIER WORDS'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  VQ=WQ
                  CALL NSS_GRIDS(V1,V2,VQ)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO SOURCE GRID CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSGRIDR
          IF(WC.EQ.'NSSGRIDR') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1) THEN
                      OUTLYNE=
     1                '"NSSGRIDR" SETS THE REFERENCE GRID SIZE, SHAPE AND SPACING'
                      CALL SHOWIT(1)
67                    FORMAT('CURRENT REFERENCE GRID DIMENSIONALITY IS: ',I8,' BY ',I8)
                      WRITE(OUTLYNE,67) INT(NSSSYSTEM(36)),INT(NSSSYSTEM(36))
                      CALL SHOWIT(1)
671                   FORMAT('CURRENT REFERENCE GRID SPACING  = ',D23.15,' ',A2)
674                   FORMAT('CURRENT REFERENCE GRID IS CLIPPED CIRCULARLY')
673                   FORMAT('CURRENT REFERENCE GRID IS A FULL RECTANGLE')
                      IF(NSSSYSTEM(21).EQ.1.0D0) UNI='IN'
                      IF(NSSSYSTEM(21).EQ.2.0D0) UNI='CM'
                      IF(NSSSYSTEM(21).EQ.3.0D0) UNI='MM'
                      IF(NSSSYSTEM(21).EQ.4.0D0) UNI=' M'
                      WRITE(OUTLYNE,671) NSSSYSTEM(38),UNI
                      CALL SHOWIT(1)
                      IF(NSSSYSTEM(22).EQ.0) WRITE(OUTLYNE,674)
                      IF(NSSSYSTEM(22).EQ.1) WRITE(OUTLYNE,673)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SQ.EQ.0) WQ='CIRC'
                  IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S3.EQ.1) THEN
                      OUTLYNE=
     1                '"NSSGRIDR" ONLY USES QUALIFIER AND NUMERIC WORDS #1 AND #2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WQ.NE.'CIRC'.AND.WQ.NE.'RECT') THEN
                      OUTLYNE=
     1                '"NSSGRIDR" ONLY TAKES "CIRC" AND "RECT" AS QUALIFIERS'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  VQ=WQ
                  CALL NSS_GRIDR(V1,V2,VQ)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO REFERENCE GRID CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSAPODR
          IF(WC.EQ.'NSSAPODR') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE=
     1                '"NSSAPODR" SETS THE REFERENCE GRID GAUSSIAN APODIZATION'
                      CALL SHOWIT(1)
672                   FORMAT('CURRENT REFERENCE GRID DBLOSS SETTING IS: ',D23.15)
                      WRITE(OUTLYNE,672) NSSSYSTEM(23)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S3.EQ.1.OR.
     1            S2.EQ.1) THEN
                      OUTLYNE='"NSSAPODR" ONLY USES NUMERIC WORD #1'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  CALL NSS_GRIDRAPOD(V1)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO APODIZATION CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       OBJMEDIA
          IF(WC.EQ.'OBJMEDIA') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"OBJMEDIA SETS THE MEDIA IN THE OBJECT SPACE'
                      CALL SHOWIT(1)
681                   FORMAT('CURRENT OBJECT MEDIA IS:',A13,1X,A13)
                      IF(NSSOGLASS(1).NE.'             '.AND.
     1                NSSOGLASS(2).NE.'             ') THEN
                          WRITE(OUTLYNE,681) NSSOGLASS(1),
     1                    NSSOGLASS(2)
                          CALL SHOWIT(1)
                      END IF
                      RETURN
                  END IF
                  IF(SN.EQ.1) THEN
                      OUTLYNE='"OBJMEDIA" TAKES NO NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.0.AND.SST.EQ.0) THEN
                      SST=1
                      WS='AIR'
                  END IF
                  IF(WQ(1:3).EQ.'AIR') THEN
                      WQ='        '
                      WS(1:13)='AIR          '
                  END IF
                  IF(WQ(1:4).EQ.'REFL') THEN
                      OUTLYNE='"OBJMEDIA" MAY NOT BE SET TO "REFL"'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  QWORD=WQ//'     '
                  SWORD=WS(1:13)
                  CALL NSS_OBJMEDIA(QWORD,SWORD)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO OBJMEDIA MATERIAL TYPE CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C
C       MEDIA1
          IF(WC.EQ.'MEDIA1') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"MEDIA1 SETS THE "MEDIA1" OPTICAL MATERIAL TYPE'
                      CALL SHOWIT(1)
68                    FORMAT('CURRENT SURFACE MEDIA1 IS:',A13,1X,A13)
                      IF(NSSGLASS1(1,SFID).NE.'             '.AND.
     1                NSSGLASS1(2,SFID).NE.'             ') THEN
                          WRITE(OUTLYNE,68) NSSGLASS1(1,SFID),
     1                    NSSGLASS2(2,SFID)
                          CALL SHOWIT(1)
                      END IF
                      RETURN
                  END IF
                  IF(SN.EQ.1) THEN
                      OUTLYNE='"MEDIA1" TAKES NO NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.0.AND.SST.EQ.0) THEN
                      SST=1
                      WS='AIR'
                  END IF
                  IF(WQ(1:3).EQ.'AIR') THEN
                      WQ='        '
                      WS(1:13)='AIR          '
                  END IF
                  IF(WQ(1:4).EQ.'REFL') THEN
                      OUTLYNE='MEDIA1 MAY NOT BE SET TO "REFL"'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  QWORD=WQ//'     '
                  SWORD=WS(1:13)
                  CALL NSS_MEDIA1(QWORD,SWORD)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO MEDIA1 MATERIAL TYPE CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       MEDIA2
          IF(WC.EQ.'MEDIA2') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"MEDIA2 SETS THE "MEDIA1" OPTICAL MATERIAL TYPE'
                      CALL SHOWIT(1)
69                    FORMAT('CURRENT SURFACE MEDIA2 IS: ',A13,1X,A13)
                      IF(NSSGLASS2(1,SFID).NE.'             '.AND.
     1                NSSGLASS2(2,SFID).NE.'             ') THEN
                          WRITE(OUTLYNE,69) NSSGLASS2(1,SFID),
     1                    NSSGLASS2(2,SFID)
                          CALL SHOWIT(1)
                      END IF
                      RETURN
                  END IF
                  IF(SN.EQ.1) THEN
                      OUTLYNE='"MEDIA2" TAKES NO NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.0.AND.SST.EQ.0) THEN
                      SST=1
                      WS='AIR'
                  END IF
                  IF(WQ(1:3).EQ.'AIR') THEN
                      WQ='        '
                      WS(1:13)='AIR          '
                  END IF
                  IF(WQ(1:4).EQ.'REFL') THEN
                      WQ='        '
                      WS(1:13)='REFL         '
                  END IF
                  QWORD=WQ//'     '
                  SWORD=WS(1:13)
                  CALL NSS_MEDIA2(QWORD,SWORD)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO MEDIA1 MATERIAL TYPE CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSN
          IF(WC.EQ.'NSSN') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"NSSN" SETS EXPLICIT REAL AND IMAGINARY REFRACTIVE'
                      CALL SHOWIT(1)
                      OUTLYNE='INDEX VALUES FOR MEDIA1 AND MEDIA2 MATERIALS IF THE'
                      CALL SHOWIT(1)
                      OUTLYNE='FIRST HALF OF THE MEDIA NAME IS "USER"'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SQ.EQ.1.OR.SST.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE='"NSSN" ONLY TAKES NUMERIC WORDS 1 TO 4'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W1.NE.1.0D0.AND.W1.NE.2.0D0) THEN
                      OUTLYNE='NUMERIC WORD #1 MUST BE 1 OR 2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W2.NE.1.0D0.AND.W2.NE.2.0D0.AND.W2.NE.3.0D0.AND.W2.NE.4.0D0
     1            .AND.W2.NE.5.0D0.AND.W2.NE.6.0D0.AND.W2.NE.7.0D0
     2            .AND.W2.NE.8.0D0.AND.W2.NE.9.0D0.AND.W2.NE.10.0D0) THEN
                      OUTLYNE='NUMERIC WORD #2 MUST BE 1, 2, 3, 4, 5, 6, 7, 8, 9 OR 10'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V3=1.0D0
                  V4=0.0D0
                  V1=W1
                  V2=W2
                  IF(DF3.EQ.0) V3=W3
                  IF(DF4.EQ.0) V4=W4
                  CALL NSS_NSSN(V1,V2,V3,V4)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO INDEX DATA CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       SCLAP
          IF(WC.EQ.'SCLAP') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"SCLAP" SETS CURRENT SURFACE CLEAR APERTURE VALUES'
                      CALL SHOWIT(1)
                      IF(NSSALENS(19,SFID).EQ.0.0D0) CTYPE='NONE'
                      IF(NSSALENS(19,SFID).EQ.1.0D0) CTYPE='CIRC'
                      IF(NSSALENS(19,SFID).EQ.2.0D0) CTYPE='RECT'
                      IF(NSSALENS(19,SFID).EQ.3.0D0) CTYPE='ELIP'
 70                   FORMAT('CURRENT SURFACE CLEAR APERTURE TYPE IS :',A4)
 71                   FORMAT(' FIRST CLAP VALUE = ',G13.6)
! 72     FORMAT('SECOND CLAP VALUE = ',G13.6)
! 73     FORMAT(' THIRD CLAP VALUE = ',G13.6)
! 74     FORMAT('FOURTH CLAP VALUE = ',G13.6)
! 75     FORMAT(' FIFTH CLAP VALUE = ',G13.6)
                      WRITE(OUTLYNE,70) CTYPE
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,71) NSSALENS(20,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,71) NSSALENS(21,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,71) NSSALENS(22,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,71) NSSALENS(23,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,71) NSSALENS(24,SFID)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1) THEN
                      OUTLYNE='"SCLAP" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.0) V0=1.0D0
                  IF(SQ.EQ.1) THEN
                      IF(WQ.NE.'RECT'.AND.WQ.NE.'NONE'.AND.WQ.NE.'ELIP') THEN
                          OUTLYNE='NONE, RECT AND ELIP ARE THE ONLY VALID QUALIFIERS'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
                  IF(WQ.EQ.'NONE') V0=0.0D0
                  IF(WQ.EQ.'RECT') V0=2.0D0
                  IF(WQ.EQ.'ELIP') V0=3.0D0
                  IF(W1.LT.0.0D0.OR.W2.LT.0.0D0) THEN
                      OUTLYNE='NUMERIC WORDS #1 AND #2 MAY BOTH BE NEGATIVE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  V3=W3
                  V4=W4
                  V5=W5
                  CALL NSS_SCLAP(V0,V1,V2,V3,V4,V5)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO CLEAR APERTURE DATA CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C       SHOLE
          IF(WC.EQ.'SHOLE') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                      OUTLYNE='"SHOLE" SETS CURRENT SURFACE HOLE VALUES'
                      CALL SHOWIT(1)
                      IF(NSSALENS(25,SFID).EQ.0.0D0) CTYPE='NONE'
                      IF(NSSALENS(25,SFID).EQ.1.0D0) CTYPE='CIRC'
                      IF(NSSALENS(25,SFID).EQ.2.0D0) CTYPE='RECT'
                      IF(NSSALENS(25,SFID).EQ.3.0D0) CTYPE='ELIP'
 82                   FORMAT('CURRENT SURFACE OBSCURATION TYPE IS :',A4)
 83                   FORMAT(' FIRST HOLE VALUE = ',G13.6)
 84                   FORMAT('SECOND HOLE VALUE = ',G13.6)
 85                   FORMAT(' THIRD HOLE VALUE = ',G13.6)
 86                   FORMAT('FOURTH HOLE VALUE = ',G13.6)
 87                   FORMAT(' FIFTH HOLE VALUE = ',G13.6)
                      WRITE(OUTLYNE,82) CTYPE
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,83) NSSALENS(26,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,84) NSSALENS(27,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,85) NSSALENS(28,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,86) NSSALENS(29,SFID)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,87) NSSALENS(30,SFID)
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1) THEN
                      OUTLYNE='"SHOLE" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.0) V0=1.0D0
                  IF(SQ.EQ.1) THEN
                      IF(WQ.NE.'RECT'.AND.WQ.NE.'NONE'.AND.WQ.NE.'ELIP') THEN
                          OUTLYNE='NONE, RECT AND ELIP ARE THE ONLY VALID QUALIFIERS'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
                  IF(WQ.EQ.'NONE') V0=0.0D0
                  IF(WQ.EQ.'RECT') V0=2.0D0
                  IF(WQ.EQ.'ELIP') V0=3.0D0
                  IF(W1.LT.0.0D0.OR.W2.LT.0.0D0) THEN
                      OUTLYNE='NUMERIC WORDS #1 AND #2 MAY BOTH BE NEGATIVE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  V1=W1
                  V2=W2
                  V3=W3
                  V4=W4
                  V5=W5
                  CALL NSS_SHOLE(V0,V1,V2,V3,V4,V5)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO HOLE DATA CAN'
                  CALL SHOWIT(1)
                  OUTLYNE='BE SPECIFIED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSLIST
          IF(WC.EQ.'NSSLIST') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1) THEN
                      OUTLYNE='"NSSLIST" DISPLAYS THE CURRENT NSS DATABASE'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1) THEN
                      OUTLYNE='"NSSLIST" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.1.AND.WQ.NE.'NOSURF') THEN
                      OUTLYNE=
     1                '"NSSLIST" ONLY TAKES "NOSURF" AS OPTIONAL QUALIFIER WORD INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL NSSEOS
                  CALL NSS_LIST
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO NSS DATABASE'
                  CALL SHOWIT(1)
                  OUTLYNE='CAN BE DISPLAYED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSTRACE
          IF(WC.EQ.'NSSTRACE') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1) THEN
                      OUTLYNE='"NSSTRACE" DOES THE NSS RAYTRACE IN THE CURRENT'
                      CALL SHOWIT(1)
                      OUTLYNE='NSS DATABASE'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='"NSSTRACE" TAKES NO STRING OR NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.1) THEN
                      IF(WQ.NE.'SPOT'.AND.WQ.NE.'SPOTADD') THEN
                          OUTLYNE='"NSSTRACE" ONLY TAKES OPTIONAL QUALIFIERS:'
                          CALL SHOWIT(1)
                          OUTLYNE='SPOT'
                          CALL SHOWIT(1)
                          OUTLYNE='SPOTADD'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                          IF(SQ.EQ.0)         NSSSPOTTYPE=0
                          IF(WQ.EQ.'SPOT')    NSSSPOTTYPE=1
                          IF(WQ.EQ.'SPOTADD') NSSSPOTTYPE=2
                          IF(SQ.EQ.0) NSSSPOTEXIST=.FALSE.
                      END IF
                  END IF
                  CALL NSSEOS
                  CALL NSSSPOTSETUP
                  CALL NSS_TRACE
C       CLOSE THE SPOT DIAGRAM FILES IF NECESSARY
                  IF(NSSSPOTTYPE.NE.0) CALL CLOSE_FILE(65,1)
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO NSS RAY TRACE'
                  CALL SHOWIT(1)
                  OUTLYNE='CAN BE PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSSPOT
          IF(WC.EQ.'NSSSPOT') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1) THEN
                      OUTLYNE='"NSSSPOT" GENERATES THE NSS SPOT DIAGRAM'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                      OUTLYNE='"NSSSPOT" TAKES NO ADDITIONAL INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      NSSSPOTEXIST=.FALSE.
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE='"NSSSPOT" ONLY TAKES NUMERIC WORDS #1 AND #2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      NSSSPOTEXIST=.FALSE.
                      CALL MACFAL
                      RETURN
                  END IF
                  NSSSPOTSURF=INT(W1)
                  NSSSPOTHITS=INT(W2)
                  IF(DF1.EQ.1) NSSSPOTSURF=DETSNUM
                  IF(DF2.EQ.1) NSSSPOTHITS=0
                  NSSSPOTEXIST=.FALSE.
                  CALL NSSSTATS
                  RETURN
              ELSE
                  NSSSPOTEXIST=.FALSE.
                  OUTLYNE='NO NSS DATABASE EXISTS. NO NSS SPOT CAN GE GENERATED'
                  CALL SHOWIT(1)
                  OUTLYNE='CAN BE PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       NSSLENO
          IF(WC.EQ.'NSSLENO') THEN
              IF(NEXISTN) THEN
                  IF(STI.EQ.1) THEN
                      OUTLYNE='"NSSLENO" OUTPUTS THE CURRENT NSS DATABASE'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
                      OUTLYNE='"NSSLENO" TAKES NO ADDITIONAL INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL NSSEOS
                  CALL NSSLENO
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO NSS DATABASE'
                  CALL SHOWIT(1)
                  OUTLYNE='CAN BE OUTPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSSAVE
          IF(WC.EQ.'NSSSAVE') THEN
              IF(NEXISTN) THEN
                  CALL NSSEOS
                  CALL NSSSAVE
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO NSS DATABASE'
                  CALL SHOWIT(1)
                  OUTLYNE='CAN BE SAVED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
C       NSSREST
          IF(WC.EQ.'NSSREST') THEN
              CALL NSSREST
              RETURN
          END IF
C
C       NSSVERT
          IF(WC.EQ.'NSSVERT') THEN
              IF(NEXISTN) THEN
                  CALL NSSEOS
                  CALL NSS_LIST_VERTEX
              ELSE
                  OUTLYNE='NO NSS DATABASE EXISTS. NO NSS DATABASE'
                  CALL SHOWIT(1)
                  OUTLYNE='CAN BE DISPLAYED'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
C
          RETURN
      END
      SUBROUTINE DEL_NSS
C       DELETES THE CURRENT NSS DATABASE
          USE NSSMOD
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INTEGER ALLOERR
          DEALLOCATE (NSSALENS,NSSSYSTEM,NSSGLASS1,NSSGLASS2,
     1    NSSSNAM,LVECTOR,VECTORL,VECTORM,VECTORN,STAT=ALLOERR)
          NEXISTN=.FALSE.
          SFID=0
          RETURN
      END
      SUBROUTINE NSS_TRACE
C       PERFORMS A FULL NSS RAY TRACE ON THE CURRENT NSS DATABASE
          USE NSSMOD
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          CALL NSSTRACE
          RETURN
      END
      SUBROUTINE NEW_NSS
C       STARTS A NEW NSS DATABASE
          USE NSSMOD
          IMPLICIT NONE
          CHARACTER*20 ABL
          INCLUDE 'datmai.inc'
          INTEGER ALLOERR
          MAXS=200
          ABL='                    '
          ALLOCATE (NSSSYSTEM(1:100),STAT=ALLOERR)
          ALLOCATE (NSSALENS(1:400,1:MAXS),STAT=ALLOERR)
          ALLOCATE (LVECTOR(1:MAXS),STAT=ALLOERR)
          ALLOCATE (VECTORL(1:MAXS),STAT=ALLOERR)
          ALLOCATE (VECTORM(1:MAXS),STAT=ALLOERR)
          ALLOCATE (VECTORN(1:MAXS),STAT=ALLOERR)
          ALLOCATE (NSSVERTEX(0:12,1:MAXS),STAT=ALLOERR)
          ALLOCATE (NSSBOUNDS(1:6,1:MAXS),STAT=ALLOERR)
          ALLOCATE (NSSGLASS1(1:3,1:MAXS),STAT=ALLOERR)
          ALLOCATE (NSSGLASS2(1:3,1:MAXS),STAT=ALLOERR)
          ALLOCATE (NSSSNAM(1:MAXS),STAT=ALLOERR)
          NSSOGLASS(1)='AIR'
          NSSOGLASS(2)='AIR'
          OBJINDEX(1:10)=1.0D0
          OBJINDEX(11:20)=0.0D0
          FROM_MEDIA(1)=NSSOGLASS(1)
          FROM_MEDIA(2)=NSSOGLASS(2)
          NSSSYSTEM(1:100)=0.0D0
          NSSSNAM(1:MAXS)=ABL//ABL//ABL//ABL
          NSSVERTEX(0:12,1:MAXS)=0.0D0
          NSSBOUNDS(1:6,1:MAXS)=0.0D0
          NSSALENS(1:400,1:MAXS)=0.0D0
          NSSGLASS1(1:3,1:MAXS)='             '
          NSSGLASS2(1:3,1:MAXS)='             '
C       SET UNITS TO INCHES
          NSSSYSTEM(21)=1.0D0
C       SET UNIVERSE TO 10000
          NSSSYSTEM(25)=10000.0D0
C       SET MHITS TO 1000
          NSSSYSTEM(27)=1000.0D0
          NEXISTN=.TRUE.
          SFID=1
          NSSSYSTEM(1)=1.0D0
          NSSSYSTEM(2:10)=0.0D0
          NSSSYSTEM(11)=1.0D0
          NSSSYSTEM(12:20)=0.0D0
          LASTSUR=1
C       SET DETSNUM TO 0
          DETSNUM=0
          NSSSYSTEM(49)=0.0D0
          RETURN
      END
      SUBROUTINE NSS_LINK(V1,VQ)
C       SETS NSS UNITS
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1
          CHARACTER*8 VQ
          INCLUDE 'datmai.inc'
          IF(VQ.EQ.'SPROFILE')  NSSALENS(44,SFID)=V1
          IF(VQ.EQ.'SMEDIA')    NSSALENS(45,SFID)=V1
          IF(VQ.EQ.'SCOATING')  NSSALENS(46,SFID)=V1
          IF(VQ.EQ.'SINTERAC')  NSSALENS(47,SFID)=V1
          RETURN
      END
      SUBROUTINE NSS_UNITS(IUNIT)
C       SETS NSS UNITS
          USE NSSMOD
          IMPLICIT NONE
          INTEGER IUNIT
          INCLUDE 'datmai.inc'
          NSSSYSTEM(21)=DBLE(IUNIT)
          RETURN
      END
      SUBROUTINE NSS_WV(V1,V2)
C       SETS NSS WAVELENGTHS
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2
          INCLUDE 'datmai.inc'
          NSSSYSTEM(INT(V1))=V2
          RETURN
      END
      SUBROUTINE NSS_WT(V1,V2)
C       SETS NSS WAVELENGTH WEIGTHS
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2
          INCLUDE 'datmai.inc'
          NSSSYSTEM(INT(V1)+10)=V2
          RETURN
      END
      SUBROUTINE NSS_UNIVERSE(V1)
C       SETS NSS TERMINAL RAY DISTANCE
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1
          INCLUDE 'datmai.inc'
          NSSSYSTEM(25)=V1
          RETURN
      END
      SUBROUTINE NSS_COAT1(IV1)
C       SETS NSS SURFACE COATING 1 ID NUMBER
          USE NSSMOD
          IMPLICIT NONE
          INTEGER IV1
          INCLUDE 'datmai.inc'
          NSSALENS(31,SFID)=DBLE(IV1)
          RETURN
      END
      SUBROUTINE NSS_COAT2(IV1)
C       SETS NSS SURFACE COATING 2 ID NUMBER
          USE NSSMOD
          IMPLICIT NONE
          INTEGER IV1
          INCLUDE 'datmai.inc'
          NSSALENS(32,SFID)=DBLE(IV1)
          RETURN
      END
      SUBROUTINE NSS_SPOS(V1,V2,V3,V4)
C       SETS NSS SURFACE POSITION
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2,V3,V4
          INCLUDE 'datmai.inc'
          NSSALENS(34,SFID)=V1
          NSSALENS(35,SFID)=V2
          NSSALENS(36,SFID)=V3
          IF(V4.EQ.0.0D0) V4=-1.0D0
          NSSALENS(37,SFID)=V4
          RETURN
      END
      SUBROUTINE NSS_SROT(V1,V2,V3)
C       SETS NSS SURACE ROTATION
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2,V3
          INCLUDE 'datmai.inc'
          NSSALENS(40,SFID)=V1
          NSSALENS(41,SFID)=V2
          NSSALENS(42,SFID)=V3
          RETURN
      END
      SUBROUTINE NSS_SBOUNDX(V1,V2)
C       SETS NSS SURACE X-BOUNDS
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2
          INCLUDE 'datmai.inc'
          NSSBOUNDS(1,SFID)=V1
          NSSBOUNDS(2,SFID)=V2
          RETURN
      END
      SUBROUTINE NSS_SBOUNDY(V1,V2)
C       SETS NSS SURACE Y-BOUNDS
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2
          INCLUDE 'datmai.inc'
          NSSBOUNDS(3,SFID)=V1
          NSSBOUNDS(4,SFID)=V2
          RETURN
      END
      SUBROUTINE NSS_SBOUNDZ(V1,V2)
C       SETS NSS SURACE Z-BOUNDS
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2
          INCLUDE 'datmai.inc'
          NSSBOUNDS(5,SFID)=V1
          NSSBOUNDS(6,SFID)=V2
          RETURN
      END
      SUBROUTINE NSS_SURFACE(V1)
C       SETS NSS SURFACE NUMBER
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1
          INCLUDE 'datmai.inc'
          SFID=INT(V1)
          RETURN
      END
      SUBROUTINE NSS_SNAM(NSTRING)
C       SETS NSS SURFACE NAME
          USE NSSMOD
          IMPLICIT NONE
          CHARACTER*80 NSTRING
          INCLUDE 'datmai.inc'
          NSSSNAM(SFID)(1:80)=NSTRING(1:80)
          RETURN
      END
      SUBROUTINE NSS_SPROFILE(V0,V1,V2,V3,V4,V5)
C       SETS NSS SURFACE PROFILE
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V0,V1,V2,V3,V4,V5
          INCLUDE 'datmai.inc'
          NSSALENS(1,SFID)=V0
          IF(V0.EQ.2.0D0.OR.V0.EQ.3.0D0) THEN
C       STORE CURVATURE THOUGH RADIUS WAS INPUT
              IF(V1.EQ.0.0D0)
     1        NSSALENS(3,SFID)=V1
              IF(V1.NE.0.0D0)
     1        NSSALENS(3,SFID)=1.0D0/V1
          END IF
          IF(V0.EQ.4.0D0) THEN
C       USER-DEFINES SURFACE
              NSSALENS(3,SFID)=V1
              NSSALENS(4,SFID)=V2
              NSSALENS(5,SFID)=V3
              NSSALENS(6,SFID)=V4
              NSSALENS(7,SFID)=V5
          END IF
          IF(V0.EQ.5.0D0) THEN
C       MEM SURFACE
              NSSALENS(3,SFID)=V1
              NSSALENS(4,SFID)=V2
              NSSALENS(5,SFID)=V3
              NSSALENS(6,SFID)=V4
              NSSALENS(7,SFID)=V5
          END IF
          IF(V0.EQ.6.0D0) THEN
              NSSALENS(3:7,SFID)=0.0D0
C       STORE TUBE RADIUS AND LENGTH
              NSSALENS(10,SFID)=V1
              NSSALENS(11,SFID)=V2
          END IF
          RETURN
      END
      SUBROUTINE NSS_SPARAM(V1,V2)
C       SETS NSS SURFACE PROFILE PARAMETERS
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2
          INCLUDE 'datmai.inc'
          NSSALENS(INT(V1)+200,SFID)=V2
          RETURN
      END
      SUBROUTINE NSS_SCLEAR
C       CLEARS AN NSS SURFACE FLAG
          USE NSSMOD
          IMPLICIT NONE
!        REAL*8 V1,V2
          INCLUDE 'datmai.inc'
          NSSALENS(100,SFID)=0.0D0
          LVECTOR(SFID)=0.0D0
          VECTORL(SFID)=0.0D0
          VECTORM(SFID)=0.0D0
          VECTORN(SFID)=0.0D0
          RETURN
      END
      SUBROUTINE NSS_SUNCLEAR
C       UNCLEARS AN NSS SURFACE FLAG
          USE NSSMOD
          IMPLICIT NONE
          !       REAL*8 V1,V2
          INCLUDE 'datmai.inc'
          NSSALENS(100,SFID)=1.0D0
          LVECTOR(SFID)=0.0D0
          VECTORL(SFID)=0.0D0
          VECTORM(SFID)=0.0D0
          VECTORN(SFID)=0.0D0
          RETURN
      END
      SUBROUTINE NSS_MINE(V1)
C       SETS NSS MIN RAY ENERGY
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1
          INCLUDE 'datmai.inc'
          NSSSYSTEM(26)=V1
          RETURN
      END
      SUBROUTINE NSS_MHIT(V1)
C       SETS NSS MAX RAY/SURFACE INTERSECTIONS
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1
          INCLUDE 'datmai.inc'
          NSSSYSTEM(27)=V1
          RETURN
      END
      SUBROUTINE NSS_SPLIT(V1)
C       SETS NSS RAY SPLITTING
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1
          INCLUDE 'datmai.inc'
          NSSSYSTEM(28)=V1
          RETURN
      END
      SUBROUTINE NSS_OBJ(VQ,V1,V2,V3)
C       SETS NSS OBJECT VALUES
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2,V3
          CHARACTER VQ*8
          INCLUDE 'datmai.inc'
          NSSSYSTEM(29)=V1
          NSSSYSTEM(30)=V2
          NSSSYSTEM(31)=V3
          IF(VQ.EQ.'REAL') NSSSYSTEM(34)=1.0D0
          IF(VQ.EQ.'VIRTUAL') NSSSYSTEM(34)=-1.0D0
          RETURN
      END
      SUBROUTINE NSS_REF(V1,V2,V3)
C       SETS NSS REFERENCE GRID VALUES
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2,V3
          INCLUDE 'datmai.inc'
          NSSSYSTEM(40)=V1
          NSSSYSTEM(41)=V2
          NSSSYSTEM(42)=V3
          RETURN
      END
      SUBROUTINE NSS_GRIDS(V1,V2,VQ)
C       SETS NSS OBJECT GRID VALUES
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2
          CHARACTER*8 VQ
          INCLUDE 'datmai.inc'
          NSSSYSTEM(32)=DBLE(INT(V1))
          NSSSYSTEM(37)=V2
          IF(VQ.EQ.'CIRC') NSSSYSTEM(33)=0.0D0
          IF(VQ.EQ.'RECT') NSSSYSTEM(33)=1.0D0
          RETURN
      END
      SUBROUTINE NSS_GRIDR(V1,V2,VQ)
C       SETS NSS REFERENCE GRID VALUES
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2
          CHARACTER*8 VQ
          INCLUDE 'datmai.inc'
          NSSSYSTEM(36)=DBLE(INT(V1))
          NSSSYSTEM(38)=V2
          IF(VQ.EQ.'CIRC') NSSSYSTEM(22)=0.0D0
          IF(VQ.EQ.'RECT') NSSSYSTEM(22)=1.0D0
          RETURN
      END
      SUBROUTINE NSS_GRIDRAPOD(V1)
C       SETS NSS REFERENCE GRID APODIZATION VALUE
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1
          INCLUDE 'datmai.inc'
          NSSSYSTEM(23)=V1
          RETURN
      END
      SUBROUTINE NSS_OBJMEDIA(WORD1,WORD2)
C       SETS NSS OBJMEDIA
          USE NSSMOD
          IMPLICIT NONE
          CHARACTER*13 WORD1,WORD2
          INCLUDE 'datmai.inc'
          NSSOGLASS(1)=WORD1
          NSSOGLASS(2)=WORD2
          FROM_MEDA=WORD1
          FROM_MEDB=WORD2
          TO_MEDA=WORD1
          TO_MEDB=WORD2
          RETURN
      END
      SUBROUTINE NSS_MEDIA1(WORD1,WORD2)
C       SETS NSS MEDIA1
          USE NSSMOD
          IMPLICIT NONE
          CHARACTER*13 WORD1,WORD2
          INCLUDE 'datmai.inc'
          NSSGLASS1(1,SFID)=WORD1
          NSSGLASS1(2,SFID)=WORD2
          RETURN
      END
      SUBROUTINE NSS_MEDIA2(WORD1,WORD2)
C       SETS NSS MEDIA1
          USE NSSMOD
          IMPLICIT NONE
          CHARACTER*13 WORD1,WORD2
          INCLUDE 'datmai.inc'
          NSSGLASS2(1,SFID)=WORD1
          NSSGLASS2(2,SFID)=WORD2
          RETURN
      END
      SUBROUTINE NSS_NSSN(V1,V2,V3,V4)
C       SETS NSS OBJECT VALUES
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V1,V2,V3,V4
          INCLUDE 'datmai.inc'
          IF(INT(V1).EQ.1) THEN
              NSSALENS(100+INT(V2),SFID)=V3
              NSSALENS(110+INT(V2),SFID)=V4
          END IF
          IF(INT(V1).EQ.2) THEN
              NSSALENS(120+INT(V2),SFID)=V3
              NSSALENS(130+INT(V2),SFID)=V4
          END IF
          RETURN
      END
      SUBROUTINE NSS_SCLAP(V0,V1,V2,V3,V4,V5)
C       SETS NSS CLAP VALUES
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V0,V1,V2,V3,V4,V5
          INCLUDE 'datmai.inc'
          NSSALENS(19,SFID)=V0
          NSSALENS(20,SFID)=V1
          NSSALENS(21,SFID)=V2
          NSSALENS(22,SFID)=V3
          NSSALENS(23,SFID)=V4
          NSSALENS(24,SFID)=V5
          RETURN
      END
      SUBROUTINE NSS_SHOLE(V0,V1,V2,V3,V4,V5)
C       SETS NSS HOLE VALUES
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 V0,V1,V2,V3,V4,V5
          INCLUDE 'datmai.inc'
          NSSALENS(25,SFID)=V0
          NSSALENS(26,SFID)=V1
          NSSALENS(27,SFID)=V2
          NSSALENS(28,SFID)=V3
          NSSALENS(29,SFID)=V4
          NSSALENS(30,SFID)=V5
          RETURN
      END
      SUBROUTINE NSS_LIST
C       DISPLAYS THE NSS DATABASE
          USE NSSMOD
          IMPLICIT NONE
          CALL NSSLIST
          RETURN
      END
      SUBROUTINE NSS_DOUBLE
C       DOUBLES THE SIZE OF THE CURENT NSS DATABASE
          USE NSSMOD
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          REAL*8 TMPSYSTEM,TMPALENS,TMPVERTEX,TMPBOUNDS,TMPLVECTOR
          REAL*8 TMPVECTORL,TMPVECTORM,TMPVECTORN
          INTEGER J,K
          CHARACTER TMPGLASS1*13,TMPGLASS2*13,TMPSNAM*80
          DIMENSION TMPSYSTEM(:),TMPALENS(:,:),TMPGLASS1(:,:),
     1    TMPVERTEX(:,:),TMPGLASS2(:,:),TMPBOUNDS(:,:)
     2    ,TMPLVECTOR(:),TMPVECTORL(:),TMPVECTORM(:),TMPVECTORN(:)
          DIMENSION TMPSNAM(:)
          ALLOCATABLE TMPSYSTEM,TMPALENS,TMPGLASS1,TMPVERTEX,TMPGLASS2,
     1    TMPSNAM,TMPBOUNDS,TMPLVECTOR,TMPVECTORL,TMPVECTORM,TMPVECTORN
          INTEGER ALLOERR
          MAXS=MAXS*2
          ALLOCATE (TMPSYSTEM(1:100),STAT=ALLOERR)
          ALLOCATE (TMPALENS(1:400,1:MAXS),STAT=ALLOERR)
          ALLOCATE (TMPLVECTOR(1:MAXS),STAT=ALLOERR)
          ALLOCATE (TMPVECTORL(1:MAXS),STAT=ALLOERR)
          ALLOCATE (TMPVECTORM(1:MAXS),STAT=ALLOERR)
          ALLOCATE (TMPVECTORN(1:MAXS),STAT=ALLOERR)
          ALLOCATE (TMPVERTEX(0:12,1:MAXS),STAT=ALLOERR)
          ALLOCATE (TMPBOUNDS(1:6,1:MAXS),STAT=ALLOERR)
          ALLOCATE (TMPGLASS1(1:3,1:MAXS),STAT=ALLOERR)
          ALLOCATE (TMPGLASS2(1:3,1:MAXS),STAT=ALLOERR)
          ALLOCATE (TMPSNAM(1:MAXS),STAT=ALLOERR)
          DO K=1,100
              TMPSYSTEM(K)=NSSSYSTEM(K)
          END DO
          DO J=1,MAXS/2
              TMPSNAM(J)=NSSSNAM(J)
              DO K=1,3
                  TMPGLASS1(K,J)=NSSGLASS1(K,J)
                  TMPGLASS2(K,J)=NSSGLASS2(K,J)
              END DO
              TMPLVECTOR(J)=LVECTOR(J)
              TMPVECTORL(J)=VECTORL(J)
              TMPVECTORM(J)=VECTORM(J)
              TMPVECTORN(J)=VECTORN(J)
              DO K=1,400
                  TMPALENS(K,J)=NSSALENS(K,J)
              END DO
              DO K=0,12
                  TMPVERTEX(K,J)=NSSVERTEX(K,J)
              END DO
              DO K=1,6
                  TMPBOUNDS(K,J)=NSSBOUNDS(K,J)
              END DO
          END DO
          DEALLOCATE (NSSSYSTEM,STAT=ALLOERR)
          DEALLOCATE (NSSALENS,STAT=ALLOERR)
          DEALLOCATE (NSSVERTEX,STAT=ALLOERR)
          DEALLOCATE (LVECTOR,STAT=ALLOERR)
          DEALLOCATE (VECTORL,STAT=ALLOERR)
          DEALLOCATE (VECTORM,STAT=ALLOERR)
          DEALLOCATE (VECTORN,STAT=ALLOERR)
          DEALLOCATE (NSSBOUNDS,STAT=ALLOERR)
          DEALLOCATE (NSSGLASS1,STAT=ALLOERR)
          DEALLOCATE (NSSGLASS2,STAT=ALLOERR)
          DEALLOCATE (NSSSNAM,STAT=ALLOERR)
          ALLOCATE (NSSSYSTEM(1:100),STAT=ALLOERR)
          ALLOCATE (NSSALENS(1:400,1:MAXS),STAT=ALLOERR)
          ALLOCATE (LVECTOR(1:MAXS),STAT=ALLOERR)
          ALLOCATE (VECTORL(1:MAXS),STAT=ALLOERR)
          ALLOCATE (VECTORM(1:MAXS),STAT=ALLOERR)
          ALLOCATE (VECTORN(1:MAXS),STAT=ALLOERR)
          ALLOCATE (NSSVERTEX(0:12,1:MAXS),STAT=ALLOERR)
          ALLOCATE (NSSBOUNDS(1:6,1:MAXS),STAT=ALLOERR)
          ALLOCATE (NSSGLASS1(1:3,1:MAXS),STAT=ALLOERR)
          ALLOCATE (NSSGLASS2(1:3,1:MAXS),STAT=ALLOERR)
          ALLOCATE (NSSSNAM(1:MAXS),STAT=ALLOERR)
          DO K=1,100
              NSSSYSTEM(K)=TMPSYSTEM(K)
          END DO
          DO J=1,MAXS/2
              NSSSNAM(J)=TMPSNAM(J)
              DO K=1,3
                  NSSGLASS1(K,J)=TMPGLASS1(K,J)
                  NSSGLASS2(K,J)=TMPGLASS2(K,J)
              END DO
              LVECTOR(J)=TMPLVECTOR(J)
              VECTORL(J)=TMPVECTORL(J)
              VECTORM(J)=TMPVECTORM(J)
              VECTORN(J)=TMPVECTORN(J)
              DO K=1,400
                  NSSALENS(K,J)=TMPALENS(K,J)
              END DO
              DO K=0,12
                  NSSVERTEX(K,J)=TMPVERTEX(K,J)
              END DO
              DO K=1,6
                  NSSBOUNDS(K,J)=TMPBOUNDS(K,J)
              END DO
          END DO
          DEALLOCATE (TMPSYSTEM,STAT=ALLOERR)
          DEALLOCATE (TMPALENS,STAT=ALLOERR)
          DEALLOCATE (TMPLVECTOR,STAT=ALLOERR)
          DEALLOCATE (TMPVECTORL,STAT=ALLOERR)
          DEALLOCATE (TMPVECTORM,STAT=ALLOERR)
          DEALLOCATE (TMPVECTORN,STAT=ALLOERR)
          DEALLOCATE (TMPVERTEX,STAT=ALLOERR)
          DEALLOCATE (TMPBOUNDS,STAT=ALLOERR)
          DEALLOCATE (TMPGLASS1,STAT=ALLOERR)
          DEALLOCATE (TMPGLASS2,STAT=ALLOERR)
          DEALLOCATE (TMPSNAM,STAT=ALLOERR)
          RETURN
      END


      SUBROUTINE NSSSAVE
          USE NSSMOD
C
          IMPLICIT NONE
C
          CHARACTER LFILENAME*12
C
          INTEGER I,WSCNT
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"NSSSAVE" SAVES THE CURRENT LENS IN AN ASCII FILE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        '"NSSSAVE" TAKES NO NUMERIC OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SQ.EQ.0) WQ='NSSLENS'
          IF(SQ.EQ.0) WSCNT=4
          DO I=8,1,-1
              IF(WQ(I:I).NE.' ') THEN
                  WSCNT=I
                  GO TO 10
              END IF
          END DO
 10       CONTINUE
          LFILENAME=trim(WQ(1:WSCNT)//'.NSS')
          WSCNT=WSCNT+4

C
C       ***************************************************************
C     DELETE CURRENT FILE
          OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE=NSSDIR//LFILENAME(1:WSCNT)
     2      ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(97,0)
C
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='OUT FILE '//NSSDIR//LFILENAME(1:WSCNT)
          CALL PROCES
          INPUT='NSSLENO'
          CALL PROCES
          INPUT='IN TP'
          CALL PROCES
          INPUT='OUT TP'
          CALL PROCES
C     RESTORE I/0
          REST_KDP(1)=RESTINPT(1)
          OUTLYNE=
     1    'CURRENT NSS DATABASE SAVED AS:'//NSSDIR//LFILENAME(1:WSCNT)
          CALL SHOWIT(1)
          RETURN
      END
      SUBROUTINE NSSREST
          USE NSSMOD
C
          IMPLICIT NONE
C
          CHARACTER LFILENAME*12
C
          INTEGER I,WSCNT
C
          LOGICAL EXISJK
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"NSSREST" RESTORES THE NSS DATABASE FROM AN ASCII FILE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        '"NSSREST" TAKES NO NUMERIC OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              SQ=1
              WQ='NSSLENS'
          END IF
C
          DO I=8,1,-1
              IF(WQ(I:I).NE.' ') THEN
                  WSCNT=I
                  GO TO 10
              END IF
          END DO
 10       CONTINUE
          LFILENAME=trim(WQ(1:WSCNT)//'.NSS')
          WSCNT=WSCNT+4
C
C       ***************************************************************
C     DOES THE FILE EXIST?
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(NSSDIR)//LFILENAME,EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE=
     1        'NSS FILE NAMED '//trim(NSSDIR)//LFILENAME//
     &        ' DOES NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE='NO NSS DATABASE RESTORATION WAS PERFORMED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C      INPUT LENS
C
          WRITE(OUTLYNE,*)
     1    'RESTORING NSS DATABASE FILE '//trim(NSSDIR)//LFILENAME//
     1    ' PLEASE WAIT...'
          CALL SHOWIT(1)
C     RESTORE
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='IN FILE '//trim(NSSDIR)//LFILENAME
          CALL PROCES
          INPUT='IN TP'
          CALL PROCES
          INPUT='OUT TP'
          CALL PROCES
C     RESTORE I/0
          REST_KDP(1)=RESTINPT(1)
          OUTLYNE=
     1    'NSS DATABASE SAVED AS: '//trim(NSSDIR)//LFILENAME//
     1    ' HAS BEEN RESTORED'
          CALL SHOWIT(1)
          CALL NSSEOS
          RETURN
      END
C
      SUBROUTINE NSSCAOJK(YMIN,XMIN,YMAX,XMAX,
     1YMINO,XMINO,YMAXO,XMAXO,CAFLG,
     2COFLG,I,
     3YMIN2,XMIN2,YMAX2,XMAX2,THETA,ZDELZ)
          USE NSSMOD
C
C     THIS ROUTINE GETS THE SAGS OF LIMITING POINTS AROUND A SURFACE
C     CLAP OR COBS AND RECOGNIZES THE EXISTENCE OF FLATS ON CONCAVE
C     SURFACES.
C
          IMPLICIT NONE
C
          INTEGER CAFLG,COFLG,I
C
          LOGICAL ISITIN,ISAIR,ISAIR2
C
          EXTERNAL ISAIR,ISAIR2,ISITIN
C
          REAL*8 X,Y,XMIN,YMIN,XMAX,YMAX,ZDELZ
     1    ,XMINO,YMINO,XMAXO,YMAXO,GAM
     2    ,CLPLCX(0:499),CLPLCY(0:499),PEEPEE,
     3    YMIN2,XMIN2,YMAX2,XMAX2,THETA,THETA2
C
          COMMON/CLPLOC/CLPLCX,CLPLCY
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          ZDELZ=0.0D0
C
          THETA2=THETA+PII
C
C       SET FLAGS CAFLG AND COFLG
          CAFLG=INT(NSSALENS(19,I))
          COFLG=INT(NSSALENS(25,I))
          GAM=0.0D0
C
C       CAFLG AND COFLG HAVE BEEN SET
C
C       CAFLG=0 NO CLAP, USE PARAXIAL DATA
C
          IF(CAFLG.EQ.0) THEN
C
C     COORDINATES OF THE END POINTS FOR PROF
              XMAX=(((DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))))*DCOS(THETA))
     1        +CLPLCX(I)
              YMAX=(((DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))))*DSIN(THETA))
     1        +CLPLCY(I)
              XMIN=(((DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))))*DCOS(THETA2))
     1        +CLPLCX(I)
              YMIN=(((DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))))*DSIN(THETA2))
     1        +CLPLCY(I)
C
              XMIN2=XMIN
              YMIN2=YMIN
              XMAX2=XMAX
              YMAX2=YMAX
          ELSE
C       CAFLG NOT 0
          END IF
C
C       CAFLG=1 CIRCULAR CLAP
          IF(CAFLG.EQ.1) THEN
C
              PEEPEE=DABS(NSSALENS(20,I))
C
C       RIGHT POINT
              XMAX=((PEEPEE)*DCOS(THETA))+NSSALENS(22,I)
              YMAX=((PEEPEE)*DSIN(THETA))+NSSALENS(23,I)
              XMIN=((PEEPEE)*DCOS(THETA2))+NSSALENS(22,I)
              YMIN=((PEEPEE)*DSIN(THETA2))+NSSALENS(23,I)
              XMAX2=XMAX
              YMAX2=YMAX
              XMIN2=XMIN
              YMIN2=YMIN
          ELSE
C       CAFLG NOT 1
          END IF
C
C       CAFLG=2 OR 3
          IF(CAFLG.EQ.2.OR.CAFLG.EQ.3) THEN
C
              GAM=(PII/180.0D0)*NSSALENS(24,I)
              XMAX=(NSSALENS(20,I))*DCOS(THETA)
              YMAX=(NSSALENS(21,I))*DSIN(THETA)
              X=(XMAX*DCOS(GAM))-(YMAX*DSIN(GAM))
              Y=(YMAX*DCOS(GAM))+(XMAX*DSIN(GAM))
              XMAX=X+ALENS(22,I)
              YMAX=Y+ALENS(23,I)
              XMAX2=X+ALENS(22,I)
              YMAX2=Y+ALENS(23,I)
              XMIN=(ALENS(20,I))*DCOS(THETA2)
              YMIN=(ALENS(21,I))*DSIN(THETA2)
              X=(XMIN*DCOS(GAM))-(YMIN*DSIN(GAM))
              Y=(YMIN*DCOS(GAM))+(XMIN*DSIN(GAM))
              XMIN=X+ALENS(22,I)
              YMIN=Y+ALENS(23,I)
              XMIN2=X+ALENS(22,I)
              YMIN2=Y+ALENS(23,I)
          ELSE
C     CAFLG NOT 2 OR 3
          END IF
          CONTINUE
C       CAFLG=1 CIRCULAR CLAP
          IF(ALENS(23,I).EQ.0.0D0.AND.ALENS(24,I).EQ.0.0D0) THEN
              IF(CAFLG.EQ.1) THEN
C
C     CLAP DEC MUST BE 0
C
                  PEEPEE=NSSALENS(20,I)
                  XMAX2=(PEEPEE)*DCOS(THETA)
                  YMAX2=(PEEPEE)*DSIN(THETA)
                  XMIN2=(PEEPEE)*DCOS(THETA2)
                  YMIN2=(PEEPEE)*DSIN(THETA2)
              ELSE
C       CAFLG NOT 1
              END IF
          ELSE
C     DEC CLAP, NO FLAT CALC
              XMAX2=XMAX
              YMAX2=YMAX
              XMIN2=XMIN
              YMIN2=YMIN
          END IF
C       COFLG=0 NO COBS
          IF(COFLG.EQ.0) THEN
              XMINO=0.0D0
              YMINO=0.0D0
              XMAXO=0.0D0
              YMAXO=0.0D0
              RETURN
          ELSE
C     MUST BE SOME COBS, CONTINUE
          END IF
C       COFLG=1 CIRCULAR COBS
          IF(COFLG.EQ.1) THEN
C       RIGHT POINT
              XMAXO=((ALENS(26,I))*DCOS(THETA))
              YMAXO=((ALENS(27,I))*DSIN(THETA))
              XMINO=((ALENS(26,I))*DCOS(THETA2))
              YMINO=((ALENS(27,I))*DSIN(THETA2))
              XMAXO=XMAXO+ALENS(28,I)
              XMINO=XMINO+ALENS(28,I)
              YMAXO=YMAXO+ALENS(29,I)
              YMINO=YMINO+ALENS(29,I)
          ELSE
C       COFLG NOT 1
          END IF
C
C       COFLG=2 0R 3)
          IF(COFLG.EQ.2.OR.COFLG.EQ.3) THEN
C
              GAM=(PII/180.0D0)*NSSALENS(30,I)
              XMAXO=(NSSALENS(26,I))*DCOS(THETA)
              YMAXO=(NSSALENS(27,I))*DSIN(THETA)
              X=(XMAXO*DCOS(GAM))-(YMAXO*DSIN(GAM))
              Y=(YMAXO*DCOS(GAM))+(XMAXO*DSIN(GAM))
              XMAXO=X+NSSALENS(28,I)
              YMAXO=Y+NSSALENS(29,I)
              XMINO=(NSSALENS(26,I))*DCOS(THETA2)
              YMINO=(NSSALENS(27,I))*DSIN(THETA2)
              X=(XMINO*DCOS(GAM))-(YMINO*DSIN(GAM))
              Y=(YMINO*DCOS(GAM))+(XMINO*DSIN(GAM))
              XMINO=X+NSSALENS(28,I)
              YMINO=Y+NSSALENS(29,I)
          ELSE
C     COFLG NOT 2 OR 3
          END IF
          RETURN
      END
      SUBROUTINE NSSROT
          USE NSSMOD
C
          IMPLICIT NONE
C
C     THIS ROUTINE DETERMINES THE CENTER OF ROTATION FOR PLOT LOOK
C     AND PLOT VIEW FOR THE NSS PLOTTING COMMANDS USING VERTEX DATA
C
          REAL*8 X,Y,Z
     3    ,XMAXIX,XMINIX,YMAXIY,YMINIY,ZMAXIZ,ZMINIZ
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(.NOT.ROTSET) THEN
              XMINIX=1.0D300
              XMAXIX=-1.0D300
              YMINIY=1.0D300
              YMAXIY=-1.0D300
              ZMINIZ=1.0D300
              ZMAXIZ=-1.0D300
              XROT=0.0D0
              YROT=0.0D0
              ZROT=0.0D0
              DO I=1,MAXS
                  IF(NSSVERTEX(0,I).NE.0.0D0) THEN
                      X=NSSVERTEX(1,I)
                      Y=NSSVERTEX(2,I)
                      Z=NSSVERTEX(3,I)
                      IF(X.LE.XMINIX) XMINIX=X
                      IF(X.GT.XMAXIX) XMAXIX=X
                      IF(Y.LE.YMINIY) YMINIY=Y
                      IF(Y.GT.YMAXIY) YMAXIY=Y
                      IF(Z.LE.ZMINIZ) ZMINIZ=Z
                      IF(Z.GT.ZMAXIZ) ZMAXIZ=Z
                  END IF
              END DO
              XROT=(XMAXIX+XMINIX)/2.0D0
              YROT=(YMAXIY+YMINIY)/2.0D0
              ZROT=(ZMAXIZ+ZMINIZ)/2.0D0
              ROTSET=.TRUE.
          END IF
          RETURN
      END
      SUBROUTINE NSSPLTSC
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE AUTO SCALE FACTOR FOR NSS PLOTTING
C
          INCLUDE 'datmai.inc'
C
          IF(NSSSCFA.EQ.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'THE NSS PLOT SCALE FACTOR IS ZERO AND WILL BE RESET TO 1.0'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'USE "PLOT NSSSCALE" TO SET IT TO A USER SPECIFIED VALUE'
              CALL SHOWIT(1)
              SAVE_KDP(27)=SAVEINPT(27)
              INPUT='PLOT NSSSCALE 1'
              CALL PROCES
              REST_KDP(27)=RESTINPT(27)
          END IF
C
          RETURN
      END


C USED FOR NSS CLAP PLOTTING
      SUBROUTINE NSSCAO1(I,X,Y,ANGLE,AN2)

          USE NSSMOD
C
          IMPLICIT NONE
C
          INTEGER CAFLG,I
C
          REAL*8 X,Y,GAM,THETA1,THETA2,THETA3,THETA4

          REAL*8 A,B,RAD,XR,YR,ANGLE,AN2,XCOR1,XCOR2,XCOR3,XCOR4

          REAL*8 YCOR1,YCOR2,YCOR3,YCOR4

          REAL*8 XDIM,YDIM,XOFF,YOFF,CANG
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CAFLG=INT(NSSALENS(19,I))
          XDIM=NSSALENS(20,I)
          YDIM=NSSALENS(21,I)
          XOFF=NSSALENS(22,I)
          YOFF=NSSALENS(23,I)
          CANG=NSSALENS(24,I)
C       CAFLG=1 CIRCULAR CLAP
          IF(CAFLG.EQ.1) THEN
              RAD=DABS(XDIM)
              X=(DCOS(ANGLE)*RAD)+XOFF
              Y=(DSIN(ANGLE)*RAD)+YOFF
          END IF
C       CAFLG=2, CLAP RECT
          IF(CAFLG.EQ.2) THEN
C     CORNER 1 HAS COORDINATES
              XCOR1=+DABS(XDIM)
              YCOR1=+DABS(YDIM)
              IF(DABS(YCOR1).LE.1.0D-15.AND.DABS(XCOR1).LE.1.0D-15) THEN
                  THETA1=0.0D0
              ELSE
                  THETA1=DATAN2(YCOR1,XCOR1)
              END IF
              IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
C     CORNER 2 HAS COORDINATES
              XCOR2=-DABS(XDIM)
              YCOR2=+DABS(YDIM)
              IF(DABS(YCOR2).LE.1.0D-15.AND.DABS(XCOR2).LE.1.0D-15) THEN
                  THETA2=0.0D0
              ELSE
                  THETA2=DATAN2(YCOR2,XCOR2)
              END IF
              IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
C     CORNER 3 HAS COORDINATES
              XCOR3=-DABS(XDIM)
              YCOR3=-DABS(YDIM)
              IF(DABS(YCOR3).LE.1.0D-15.AND.DABS(XCOR3).LE.1.0D-15) THEN
                  THETA3=0.0D0
              ELSE
                  THETA3=DATAN2(YCOR3,XCOR3)
              END IF
              IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
C     CORNER 4 HAS COORDINATES
              XCOR4=+DABS(XDIM)
              YCOR4=-DABS(YDIM)
              IF(DABS(YCOR4).LE.1.0D-15.AND.DABS(XCOR4).LE.1.0D-15) THEN
                  THETA4=0.0D0
              ELSE
                  THETA4=DATAN2(YCOR4,XCOR4)
              END IF
              IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
C
              IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
                  X=DABS(XDIM)
                  Y=X*DTAN(ANGLE)
              END IF
              IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
                  Y=DABS(YDIM)
                  X=Y*DTAN((PII/2.0D0)-ANGLE)
              END IF
              IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
                  X=-DABS(XDIM)
                  Y=X*DTAN(ANGLE)
              END IF
              IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
                  Y=-DABS(YDIM)
                  X=Y*DTAN((PII/2.0D0)-ANGLE)
              END IF
              IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
                  X=DABS(XDIM)
                  Y=X*DTAN(ANGLE)
              END IF

              IF(ANGLE.LE.THETA1.AND.AN2.GE.THETA1) THEN
                  X=XCOR1
                  Y=YCOR1
              END IF
              IF(ANGLE.LE.THETA2.AND.AN2.GE.THETA2) THEN
                  X=XCOR2
                  Y=YCOR2
              END IF
              IF(ANGLE.LE.THETA3.AND.AN2.GE.THETA3) THEN
                  X=XCOR3
                  Y=YCOR3
              END IF
              IF(ANGLE.LE.THETA4.AND.AN2.GE.THETA4) THEN
                  X=XCOR4
                  Y=YCOR4
              END IF
C
C     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
              GAM=(PII/180.0D0)*(CANG)
              XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
              YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
              X=XR+XOFF
              Y=YR+YOFF
          END IF
C
C       CAFLG=3, ELLIPTICAL CLAP
          IF(CAFLG.EQ.3) THEN
C       X-SEMI-MAJOR AXIS IS ALENS(11,I)
C       Y-SEMI-MAJOR AXIS IS ALENS(10,I)
              A=DABS(XDIM)
              B=DABS(YDIM)
              RAD=((A**2)*(B**2))/
     1        (((B**2)*((DCOS(ANGLE))**2))+((A**2)*((DSIN(ANGLE)**2))))
              RAD=DSQRT(RAD)
              X=RAD*DCOS(ANGLE)
              Y=RAD*DSIN(ANGLE)
C     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
              GAM=(PII/180.0D0)*(CANG)
              XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
              YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
              X=XR+ALENS(13,I)
              Y=YR+ALENS(12,I)
          END IF
          RETURN
      END


C USED FOR NSS HOLE PLOTTING
      SUBROUTINE NSSCAO2(I,X,Y,ANGLE,AN2)

          USE NSSMOD
C
          IMPLICIT NONE
C
          INTEGER CAFLG,I
C
          REAL*8 X,Y,GAM,THETA1,THETA2,THETA3,THETA4

          REAL*8 A,B,RAD,XR,YR,ANGLE,AN2,XCOR1,XCOR2,XCOR3,XCOR4

          REAL*8 YCOR1,YCOR2,YCOR3,YCOR4

          REAL*8 XDIM,YDIM,XOFF,YOFF,CANG
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CAFLG=INT(NSSALENS(25,I))
          XDIM=NSSALENS(26,I)
          YDIM=NSSALENS(27,I)
          XOFF=NSSALENS(28,I)
          YOFF=NSSALENS(29,I)
          CANG=NSSALENS(30,I)
C       CAFLG=1 CIRCULAR CLAP
          IF(CAFLG.EQ.1) THEN
              RAD=DABS(XDIM)
              X=(DCOS(ANGLE)*RAD)+XOFF
              Y=(DSIN(ANGLE)*RAD)+YOFF
          END IF
C       CAFLG=2, CLAP RECT
          IF(CAFLG.EQ.2) THEN
C     CORNER 1 HAS COORDINATES
              XCOR1=+DABS(XDIM)
              YCOR1=+DABS(YDIM)
              IF(DABS(YCOR1).LE.1.0D-15.AND.DABS(XCOR1).LE.1.0D-15) THEN
                  THETA1=0.0D0
              ELSE
                  THETA1=DATAN2(YCOR1,XCOR1)
              END IF
              IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
C     CORNER 2 HAS COORDINATES
              XCOR2=-DABS(XDIM)
              YCOR2=+DABS(YDIM)
              IF(DABS(YCOR2).LE.1.0D-15.AND.DABS(XCOR2).LE.1.0D-15) THEN
                  THETA2=0.0D0
              ELSE
                  THETA2=DATAN2(YCOR2,XCOR2)
              END IF
              IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
C     CORNER 3 HAS COORDINATES
              XCOR3=-DABS(XDIM)
              YCOR3=-DABS(YDIM)
              IF(DABS(YCOR3).LE.1.0D-15.AND.DABS(XCOR3).LE.1.0D-15) THEN
                  THETA3=0.0D0
              ELSE
                  THETA3=DATAN2(YCOR3,XCOR3)
              END IF
              IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
C     CORNER 4 HAS COORDINATES
              XCOR4=+DABS(XDIM)
              YCOR4=-DABS(YDIM)
              IF(DABS(YCOR4).LE.1.0D-15.AND.DABS(XCOR4).LE.1.0D-15) THEN
                  THETA4=0.0D0
              ELSE
                  THETA4=DATAN2(YCOR4,XCOR4)
              END IF
              IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
C
              IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
                  X=DABS(XDIM)
                  Y=X*DTAN(ANGLE)
              END IF
              IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
                  Y=DABS(YDIM)
                  X=Y*DTAN((PII/2.0D0)-ANGLE)
              END IF
              IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
                  X=-DABS(XDIM)
                  Y=X*DTAN(ANGLE)
              END IF
              IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
                  Y=-DABS(YDIM)
                  X=Y*DTAN((PII/2.0D0)-ANGLE)
              END IF
              IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
                  X=DABS(XDIM)
                  Y=X*DTAN(ANGLE)
              END IF

              IF(ANGLE.LE.THETA1.AND.AN2.GE.THETA1) THEN
                  X=XCOR1
                  Y=YCOR1
              END IF
              IF(ANGLE.LE.THETA2.AND.AN2.GE.THETA2) THEN
                  X=XCOR2
                  Y=YCOR2
              END IF
              IF(ANGLE.LE.THETA3.AND.AN2.GE.THETA3) THEN
                  X=XCOR3
                  Y=YCOR3
              END IF
              IF(ANGLE.LE.THETA4.AND.AN2.GE.THETA4) THEN
                  X=XCOR4
                  Y=YCOR4
              END IF
C
C     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
              GAM=(PII/180.0D0)*(CANG)
              XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
              YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
              X=XR+XOFF
              Y=YR+YOFF
          END IF
C
C       CAFLG=3, ELLIPTICAL CLAP
          IF(CAFLG.EQ.3) THEN
C       X-SEMI-MAJOR AXIS IS ALENS(11,I)
C       Y-SEMI-MAJOR AXIS IS ALENS(10,I)
              A=DABS(XDIM)
              B=DABS(YDIM)
              RAD=((A**2)*(B**2))/
     1        (((B**2)*((DCOS(ANGLE))**2))+((A**2)*((DSIN(ANGLE)**2))))
              RAD=DSQRT(RAD)
              X=RAD*DCOS(ANGLE)
              Y=RAD*DSIN(ANGLE)
C     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
              GAM=(PII/180.0D0)*(CANG)
              XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
              YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
              X=XR+ALENS(13,I)
              Y=YR+ALENS(12,I)
          END IF
          RETURN
      END


C USED FOR NSS TUBE CLAP PLOTTING (AND ALSO PROFILE PLOTTING)
      SUBROUTINE NSSCAO3(I,X,Y,ANGLE)

          USE NSSMOD
C
          IMPLICIT NONE
C
          INTEGER I
C
          REAL*8 X,Y

          REAL*8 RAD,ANGLE
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          RAD=NSSALENS(10,I)
          X=(DCOS(ANGLE)*RAD)
          Y=(DSIN(ANGLE)*RAD)
          RETURN
      END
      SUBROUTINE NSSNR3(J)
          USE NSSMOD
C
          IMPLICIT NONE
C
          INTEGER I,J
C
          EXTERNAL NSSFNZ3
C
          EXTERNAL NSSDER3X,NSSDER3Y,NSSFUNC3X,NSSFUNC3Y
C
          REAL*8 C1,C2,C3,C4,ACLENG,DEL,NSSFNZ3
     1    ,FNXP,FNYP,FNZP,HV1,HV2,HV,XP,YP,ZP,SNGX,SNGY,SNGZ,
     2    LENG,MAG,ZCAL,QQ,C5,C6,C8,C10,C11,C12,C13,C14,
     3    CY,KY,EY,FY,DY,GY,CX,KX,DX,EX,FX,GX,ZZTOP
     2    ,NSSDER3X,NSSDER3Y,NSSFUNC3X,NSSFUNC3Y
!       REAL*8 R1,R2,RD
          INTEGER JPASS
          COMMON/IJPASSER/JPASS
          LOGICAL ERR
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(NSSLN.NE.0.0D0) SNGX=NSSLN/DABS(NSSLN)
          IF(NSSMN.NE.0.0D0) SNGY=NSSMN/DABS(NSSMN)
          IF(NSSNN.NE.0.0D0) SNGZ=NSSNN/DABS(NSSNN)
          IF(NSSLN.EQ.0.0D0) SNGX=1.0D0
          IF(NSSMN.EQ.0.0D0) SNGY=1.0D0
          IF(NSSNN.EQ.0.0D0) SNGZ=1.0D0
          JPASS=J
C
C     AD
          C1=NSSALENS(5,J)
C     AE
          C2=NSSALENS(6,J)
C     AF
          C3=NSSALENS(7,J )
C     AG
          C4=NSSALENS(201,J)
C     CC
          C5=NSSALENS(4,J)
C
          C6=NSSALENS(3,J)
          C8=NSSALENS(206,J)
C
C     ADTOR
          C10=NSSALENS(202,J)
C     AETOR
          C11=NSSALENS(203,J)
C     AFTOR
          C12=NSSALENS(204,J)
C     AGTOR
          C13=NSSALENS(205,J)
C     CCTOR
          C14=NSSALENS(207,J)
C
C       IF THE SURFACE IS AN Y-TORIC THE BASE CURVATURE (CV)
C       LIES IN THE YZ PLANE. IF,HOWEVER, THE SURFACE IS A
C       Y-TORIC, THE BASE CURVATURE LIES IN THE XZ PLANE.
C
          CY=C6
          KY=C5
          DY=C1
          EY=C2
          FY=C3
          GY=C4
          CX=C8
          KX=C14
          DX=C10
          EX=C11
          FX=C12
          GX=C13
C
C       CALCULATE INTERSECTION
          DO 10 I=1,NRAITR
              QQ=(1.0D0-((KX+1.0D0)*(CX**2)*(RRX**2))
     1        -((KY+1.0D0)*(CY**2)*(RRY**2)))
              IF(QQ.LT.0.0D0) THEN
                  NSS_INTERSECT=.FALSE.
                  RETURN
              END IF
              QQ=DSQRT(QQ)
              QQ=1.0D0+QQ
C       CALCULATE CURRENT TANGENT PLANE NORMAL
              DEL=DELSUR
C
C     X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
C     FUNCTION NSSFNZ3 EVALUATES THE FUNCTION
C
              FNXP=-NSSDER3X(NSSFUNC3X,RRX,0.005D0,ERR)
              FNYP=-NSSDER3Y(NSSFUNC3Y,RRY,0.005D0,ERR)
              FNZP=SNGZ
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
              MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
              NSSLN=FNXP/MAG
              NSSMN=FNYP/MAG
              NSSNN=FNZP/MAG
C       THE INTERSECTION OF THE RAY WITH THIS TANGENT
C       PLANE
C       USING:
C
              ZCAL=NSSFNZ3(RRX,RRY,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY,
     1        QQ)
              HV1=ZCAL-RRZ
              HV2=((RRL*NSSLN)+(RRM*NSSMN)+(RRN*NSSNN))
              HV=HV1/HV2
C       NEW X,Y AND Z ARE
              XP=RRX+(HV*RRL)
              YP=RRY+(HV*RRM)
              ZP=RRZ+(HV*RRN)
C       SHOULD WE CONTINUE?
              ZZTOP=NSSFNZ3(XP,YP,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY,
     1        QQ)
              LENG=DABS(ZZTOP-ZP)
              ACLENG=LENG
              IF(LENG.LE.SURTOL) THEN
                  RRX=XP
                  RRY=YP
                  RRZ=ZP
                  QQ=(1.0D0-((KX+1.0D0)*(CX**2)*(RRX**2))
     1            -((KY+1.0D0)*(CY**2)*(RRY**2)))
                  IF(QQ.LT.0.0D0) THEN
                      NSS_INTERSECT=.FALSE.
                      RETURN
                  END IF
                  QQ=DSQRT(QQ)
                  QQ=1.0D0+QQ
C       CALCULATE CURRENT TANGENT PLANE NORMAL
C
C     X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
C     FUNCTION NSSFNZ3 EVALUATES THE FUNCTION
C
                  FNXP=-NSSDER3X(NSSFUNC3X,RRX,0.005D0,ERR)
                  FNYP=-NSSDER3Y(NSSFUNC3Y,RRY,0.005D0,ERR)
                  FNZP=SNGZ
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
                  MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
                  NSSLN=FNXP/MAG
                  NSSMN=FNYP/MAG
                  NSSNN=FNZP/MAG
                  STOPP=0
                  RETURN
              ELSE
C       KEEP TRYING
C       MAKE X Y Z FROM XP YP ZP AND TRY AGAIN
                  RRX=XP
                  RRY=YP
                  RRZ=ZP
              END IF
 10       CONTINUE
          NSS_INTERSECT=.FALSE.
          RETURN
          RETURN
      END
      FUNCTION NSSFNZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
C
          IMPLICIT NONE
C
          REAL*8 NSSFNZ3,AX,AY,ACX,ACY,ADX,ADY,AACX,AACY,AADX
     1    ,AADY,AAX,AAY,NSSFNZZ3,AQ,Q,AEX,AEY,
     2    AAEX,AAEY,AAFX,AAFY,AAGX,AAGY,AFX,AFY,AGX,AGY

!      REAL*8 OMEGAX,OMEGAY
C
          INCLUDE 'datmai.inc'
C
!      INTEGER I
C
          NSSFNZZ3(AAX,AAY,AACX,AACY,AADX,AADY,AAEX,AAEY,AAFX,AAFY,
     1     AAGX,AAGY,AQ)=
     1            (((AACX*(AAX**2))+(AACY*(AAY**2)))/AQ)
     1      +(AADY*((((1.0D0-AADX)*(AAX**2))+((1.0D0+AADX)*(AAY**2)))**2))
     1      +(AAEY*((((1.0D0-AAEX)*(AAX**2))+((1.0D0+AAEX)*(AAY**2)))**3))
     1      +(AAFY*((((1.0D0-AAFX)*(AAX**2))+((1.0D0+AAFX)*(AAY**2)))**4))
     1      +(AAGY*((((1.0D0-AAGX)*(AAX**2))+((1.0D0+AAGX)*(AAY**2)))**5))
C
          NSSFNZ3=NSSFNZZ3(AX,AY,ACX,ACY,ADX,ADY,AEX,AEY,AFX,AFY,AGX,AGY,Q)
          RETURN
      END
      FUNCTION NSSDER3X(NSSFUNC3X,X,DELH,DERERROR)
C
C     SIMPLE X-DERIVATIVE FUNCTION
C
          IMPLICIT NONE
          REAL*8 NSSDER3X,DELH,X,NSSFUNC3X
          LOGICAL DERERROR

          EXTERNAL NSSFUNC3X
C
          NSSDER3X=(NSSFUNC3X(X+DELH)-NSSFUNC3X(X-DELH))/(2.0D0*DELH)
          IF(DABS(NSSDER3X).GT.10.0D0) DERERROR=.TRUE.
          RETURN
      END
      FUNCTION NSSDER3Y(NSSFUNC3Y,Y,DELH,DERERROR)
C
C     SIMPLE Y-DERIVATIVE FUNCTION
C
          IMPLICIT NONE
          REAL*8 NSSDER3Y,DELH,Y,NSSFUNC3Y
          LOGICAL DERERROR

          EXTERNAL NSSFUNC3Y
C
          NSSDER3Y=(NSSFUNC3Y(Y+DELH)-NSSFUNC3Y(Y-DELH))/(2.0D0*DELH)
          IF(DABS(NSSDER3Y).GT.10.0D0) DERERROR=.TRUE.
          RETURN
      END
      FUNCTION NSSFUNC3X(AX)
          USE NSSMOD
C
          IMPLICIT NONE
C
          INTEGER JPASS
          COMMON/IJPASSER/JPASS
C
          EXTERNAL NSSFNZ3
C
          REAL*8 NSSFUNC3X,AX,NSSFNZ3,C1,C2,C3,C4,C5,C6,C8
     1    ,C10,C11,C12,C13,C14,CX,CY,KX,KY,DX,DY,EX,EY,FX,FY,GX,GY
     2    ,QQ
C
          INCLUDE 'datmai.inc'
C
C     AD
          C1=NSSALENS(5,JPASS)
C     AE
          C2=NSSALENS(6,JPASS)
C     AF
          C3=NSSALENS(7,JPASS)
C     AG
          C4=NSSALENS(201,JPASS)
C     CC
          C5=NSSALENS(4,JPASS)
          C6=NSSALENS(3,JPASS)
          C8=NSSALENS(206,JPASS)
C     ADTOR
          C10=NSSALENS(202,JPASS)
C     AETOR
          C11=NSSALENS(203,JPASS)
C     AFTOR
          C12=NSSALENS(204,JPASS)
C     AGTOR
          C13=NSSALENS(205,JPASS)
C     CCTOR
          C14=NSSALENS(207,JPASS)
          CY=C6
          KY=C5
          DY=C1
          EY=C2
          FY=C3
          GY=C4
          CX=C8
          KX=C14
          DX=C10
          EX=C11
          FX=C12
          GX=C13
          QQ=(1.0D0-((KX+1.0D0)*(CX**2)*(AX**2))
     1    -((KY+1.0D0)*(CY**2)*(RRY**2)))
          QQ=DSQRT(DABS(QQ))
          QQ=1.0D0+QQ
C
          NSSFUNC3X=NSSFNZ3(AX,RRY,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY,QQ)
C
          RETURN
      END
      FUNCTION NSSFUNC3Y(AY)
          USE NSSMOD
C
          IMPLICIT NONE
C
          INTEGER JPASS
          COMMON/IJPASSER/JPASS
C
          EXTERNAL NSSFNZ3
C
          REAL*8 NSSFUNC3Y,AY,NSSFNZ3,C1,C2,C3,C4,C5,C6,C8
     1    ,C10,C11,C12,C13,C14,CX,CY,KX,KY,DX,DY,EX,EY,FX,FY,GX,GY
     2    ,QQ
C
          INCLUDE 'datmai.inc'
C
C     AD
          C1=NSSALENS(5,JPASS)
C     AE
          C2=NSSALENS(6,JPASS)
C     AF
          C3=NSSALENS(7,JPASS)
C     AG
          C4=NSSALENS(201,JPASS)
C     CC
          C5=NSSALENS(4,JPASS)
          C6=NSSALENS(3,JPASS)
          C8=NSSALENS(206,JPASS)
C     ADTOR
          C10=NSSALENS(202,JPASS)
C     AETOR
          C11=NSSALENS(203,JPASS)
C     AFTOR
          C12=NSSALENS(204,JPASS)
C     AGTOR
          C13=NSSALENS(205,JPASS)
C     CCTOR
          C14=NSSALENS(207,JPASS)
          CY=C6
          KY=C5
          DY=C1
          EY=C2
          FY=C3
          GY=C4
          CX=C8
          KX=C14
          DX=C10
          EX=C11
          FX=C12
          GX=C13
          QQ=(1.0D0-((KX+1.0D0)*(CX**2)*(RRX**2))
     1    -((KY+1.0D0)*(CY**2)*(AY**2)))
          QQ=DSQRT(DABS(QQ))
          QQ=1.0D0+QQ
C
          NSSFUNC3Y=NSSFNZ3(RRX,AY,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY,QQ)
C
          RETURN
      END
      SUBROUTINE NSS_FIND_SURFACE(JJ)
          USE NSSMOD
          IMPLICIT NONE
          INTEGER JJ,J,NSSJTEST,JJJ,JJJJ
          REAL*8 NSSLTEST
          INCLUDE 'datmai.inc'
C       THIS ROUTINE FINDS THE SURFACE NUMBER FOR WHICH THE RAY TRAVELS THE LEAST,
C       POSITIVE DISTANCE WITH THE RAY INSIDE BOUNDS AND CLAP BUT NOT IN HOLE.
C       LASTSUR IS THE HIGHEST SURFACE NUMBER IN THE CURRENT NSS DATABASE
C       SET THE PERMANENT GLOBAL RAY DATA BEFORE THE SEARCH
C      GLOBAL RAY DATA BEFORE SURFACE SEARCH
          NSSJTEST=0
          NSSLTEST=1.0D300
          GRRX=NSS_GRAY(1)
          GRRY=NSS_GRAY(2)
          GRRZ=NSS_GRAY(3)
          GRRL=NSS_GRAY(4)
          GRRM=NSS_GRAY(5)
          GRRN=NSS_GRAY(6)
          GXRRL=NSS_GRAY(7)
          GXRRM=NSS_GRAY(8)
          GXRRN=NSS_GRAY(9)
          GYRRL=NSS_GRAY(10)
          GYRRM=NSS_GRAY(11)
          GYRRN=NSS_GRAY(12)
          DO J=1,LASTSUR
C       WE ARE NOW AT J
C       THE REFRACTIVE INDEX VALUES ARE:
C       WHERE 1 AND 2 REFER TO THE SPACES ON EACH SIDE OF THE SURFACE
              REALINDEX1=NSSALENS(100+NSSWAVNUM,J)
              REALINDEX2=NSSALENS(120+NSSWAVNUM,J)
              IMAGINDEX1=NSSALENS(110+NSSWAVNUM,J)
              IMAGINDEX2=NSSALENS(130+NSSWAVNUM,J)
C       WE ALREADY KNOW WHAT THE CURINDEXR AND CURINDXI ARE!
C       CHECK THAT THE NEXT SURFACE IS ACTIVE
              IF(NSSALENS(100,J).EQ.1.0D0) THEN
C       SURFACE IS TO BE CONSIDERED FOR POSSIBLE INTERSECTION
C       TRANSFORM THE CURRENT GLOBAL RAY DATA INTO THE COORDINATE SYSTEM OF SURFACE J
C       THIS DOES NOT CHANGE THE RAY DATA IN THE PERMANENT RAY STORAGE LOCATIONS
                  JJJ=J
                  CALL NSS_FNDTRANSF(JJJ)
C       THE ARRAY NSS_LRAY NOW HOLDS THE DATA FOR THE RAY IN THE LOCAL COORDINATE
C       SYSTEM OF SURFACE J. DOES THE RAY INTERSECT THIS SURFACE
C       INSIDE THE SURFACE BOUNDS OF THE SURFACE. IF IT DOES AND THE
C       OPL IS LESS THAN THE LAST SAVED OPL, THEN REPLACE THE PROVIOUS SURFACE
C       AS THE BEST SURFACE INTERSECTION CANDIDATE, ELSE PROCEED TO THE NEXT SURFACE.
                  JJJ=J
                  CALL NSS_FIND_PART2(JJJ)
              ELSE
C       SKIP THAT SURFACE NUMBER
              END IF
          END DO
C       WE NOW HAVE THE LIST OF ALL SURFACES WITH AN ASSOCIATED LVECTOR
C       WHICH IS THE DISTANCE FROM THE STARTING RAY LOCATION IN GLOBAL
C       COORDINATES TO THE GLOBAL COORDINATE POSITION OF THE RAY AT EACH
C       SURFACE.
          DO JJJJ=1,LASTSUR
              IF(LVECTOR(JJJJ).GT.0.0D0.AND.
     1        LVECTOR(JJJJ).LT.NSSLTEST) THEN
                  NSSLTEST=LVECTOR(JJJJ)
                  NSSJTEST=JJJJ
              END IF
          END DO
c                       DO JJJJ=1,LASTSUR
c       WRITE(OUTLYNE,*)JJJJ,LVECTOR(JJJJ),NSSALENS(100,JJJ)
c       CALL SHOWIT(1)
c                       END DO
C       DID WE MISS EVERYONE?
          DO JJJJ=1,LASTSUR
              IF(LVECTOR(JJJJ).GT.0.0D0) GO TO 10
          END DO
C       IF WE GOT HERE, THE RAY MISSED ALL SURFACES SO WE SET NSSJTEST = MAXS+1
          NSSJTEST=MAXS+1
C       NOW THE VALUE OF JJ IS THE NEXT SURFACE NUMBER WHICH SHOULD BE
C       INTERSECTED AND INTERACTED WITH.
 10       JJ=NSSJTEST
          RETURN
      END
C
      SUBROUTINE NSS_FIND_PART2(J)
          USE NSSMOD
          INTEGER J
          REAL*8 RTESTINDEX,ITESTINDEX
          REAL*8 GX0,GY0,GZ0,GX,GY,GZ,GL0,GM0,GN0,GL,GM,GN
          INCLUDE 'datmai.inc'
          NSS_INTERSECT=.TRUE.
          RRX=LRRX
          RRY=LRRY
          RRZ=LRRZ
          RRL=LRRL
          RRM=LRRM
          RRN=LRRN
          XRRL=LXRRL
          XRRM=LXRRM
          XRRN=LXRRN
          YRRL=LYRRL
          YRRM=LYRRM
          YRRN=LYRRN
C       THE SURFACE DIR COSINES AT THE SURFACE VERTEX ARE:
          NSSLN=0.0D0
          NSSMN=0.0D0
          NSSNN=1.0D0
          NSS_INTERSECT=.TRUE.
          IF(INT(NSSALENS(1,J)).EQ.1)  CALL NSSFLA(J)
          IF(INT(NSSALENS(1,J)).EQ.2)  CALL NSSASP(J)
          IF(INT(NSSALENS(1,J)).EQ.3)  CALL NSSANA(J)
          IF(INT(NSSALENS(1,J)).EQ.4)  CALL NSSUSER()
          IF(INT(NSSALENS(1,J)).EQ.5)  CALL NSSMEM(J)
          IF(INT(NSSALENS(1,J)).EQ.6)  CALL NSSTUB(J)
          IF(.NOT.NSS_INTERSECT)THEN
C       THE RAY DID NOT HIT THE SURFACE
              LVECTOR(J)=-1.0D0
C       IF(J.EQ.25) THEN
C       write(outlyne,*) 'herea fail',J
C       call showit(1)
C               END IF
              RETURN
          END IF
C        THE RAY CAN HIT THE SURFACE, PROCEED
C
C        IF(INT(NSSALENS(1,J)).EQ.4) CALL NSSUSER(J)
C
C        IS THE ONE INTERSECTION POINT A VALID ONE WITH RESPECT TO
C        THE MATERIAL IN WHICH IT WAS TRAVELING?
C        THE CURRENT INDEX IN WHICH THE RAY THINKS IT IS TRAVELING IS:
C                       CURINDEXR
C                       CURINDEXI
C        IS THE RAY APPROACHING THE SURFACE IN A SPACE WHICH HAS
C        A MATCHING INDEX?
C        COMPUTE THE COSINE OF THE ANGLE BETWEEN THE LOCAL RAY DIRECTION AND
C        THE SURFACE NORMAL
          IF(RRN.EQ.0.0D0) RRN=1.0D-10
          IF(RRN.GT.0.0D0) TRAVEL=1
          IF(RRN.LT.0.0D0) TRAVEL=2
          IF(TRAVEL.EQ.1) THEN
C        THE RAY IS TRAVELING TOWARD THE INTERFACE IN THE #1 MATERIAL
              TRAVEL=1
              RTESTINDEX=REALINDEX1
              ITESTINDEX=IMAGINDEX1
          ELSE
C        THE RAY IS TRAVELING TOWARD THE INTERFACE IN THE #2 MATERIAL
              TRAVEL=2
              RTESTINDEX=REALINDEX2
              ITESTINDEX=IMAGINDEX2
          END IF

          IF(RTESTINDEX.EQ.CURINDEXR.AND.ITESTINDEX.EQ.
     1    CURINDEXI) THEN
C       MATERIALS MATCH
              NSS_INTERSECT=.TRUE.
          ELSE
C       MATERIALS DONT MATCH
C       NOT A VALID INTERSECTION, TRY ANOTHER SURFACE
              NSS_INTERSECT=.FALSE.
              LVECTOR(J)=-1.0D0
C       IF(J.EQ.25) THEN
C       write(outlyne,*) 'hereb mat mismatch',J
C       call showit(1)
C               END IF
              RETURN
          END IF
C        NOW WE HAVE ONE SURFACE INTERSECTION POINT FOR THE SURFACE
C        IN LOCAL COORDINATES.
          CALL BOUNDER(J)
          IF(.NOT.NSS_INTERSECT)THEN
C       RAY HITS SURFACE OUTSIDE SURFACE BOUND, NOT A VALID INTERSECTION
              LVECTOR(J)=-1.0D0
C       IF(J.EQ.25) THEN
C       write(outlyne,*) 'herec outside bound',J
C       call showit(1)
C               END IF
              RETURN
          ELSE
          END IF
          CALL CLAPER(J)
          IF(.NOT.NSS_INTERSECT)THEN
C       RAY HITS SURFACE OUTSIDE CLEAR APERTURE, NOT A VALID INTERSECTION
              LVECTOR(J)=-1.0D0
C       IF(J.EQ.25) THEN
C        write(outlyne,*) 'hered outside clap',J
C        call showit(1)
C               END IF
              RETURN
          ELSE
          END IF
          CALL HOLEER(J)
          IF(.NOT.NSS_INTERSECT)THEN
C       RAY HITS SURFACE INSIDE HOLE, NOT A VALID INTERSECTION
              LVECTOR(J)=-1.0D0
C       IF(J.EQ.25) THEN
C        write(outlyne,*) 'heree inside hole',J
C        call showit(1)
C               END IF
              RETURN
          ELSE
          END IF
C       RAY HAS A VALID INTERSECTION INSIDE BOUNDS AND CLAP AND OUTSIDE HOLE
C       AND INDEX VALUES MATCH.
C       COMPUTE LVECTOR, THIS IS THE DISTANCE IN GLOBAL COORDINATES
C       BETWEEN THE STARTING GLOBAL RAY POSITION AND THE GLOBAL POSITION
C       AT THE INTERSECTION. WE ALREADY KNOW THE DIRECTION IS OK OR
C       LVECTOR(J) WOULD HAVE ALREADY BEEN SET TO -1.0D0.
C       COMPUTE THE GLOBAL COORDINATES GIVEN THE RRX0, RRY0, RRZ0, RX, RY, RZ
C       THE GX0,GY0,GZ0 WILL BE THE RETURNED GLOBAL RAY COORDINATES
C       BEFORE THE INTERSECTION, GX, GY AND GZ WILL BE AFTER INTERSECTION.
          GX0=RRX0
          GY0=RRY0
          GZ0=RRZ0
          GL0=RRL0
          GM0=RRM0
          GN0=RRN0
          GX=RRX
          GY=RRY
          GZ=RRZ
          GL=RRL
          GM=RRM
          GN=RRN
C
          CALL LOCAL_TO_GLOBAL(GX0,GY0,GZ0,GL0,GM0,GN0,GX,GY,GZ,GL,GM,GN,J)
C
C       LVECTOR IS GLOBAL RAY TRAVEL TO INTERSECTION POINT.
          LVECTOR(J)=DSQRT(((GX0-GX)**2)+((GY0-GY)**2)+((GZ0-GZ)**2))
          IF(LVECTOR(J).GT.0.0D0) THEN
              VECTORL(J)=(GX-GX0)/LVECTOR(J)
              VECTORM(J)=(GY-GY0)/LVECTOR(J)
              VECTORN(J)=(GZ-GZ0)/LVECTOR(J)
              IF(DABS(GL).LT.1.0D-10) GL=0.0D0
              IF(DABS(GM).LE.1.0D-10) GM=0.0D0
              IF(DABS(GN).LE.1.0D-10) GN=0.0D0
              IF(DABS(GL0).LE.1.0D-10) GL0=0.0D0
              IF(DABS(GM0).LE.1.0D-10) GM0=0.0D0
              IF(DABS(GN0).LE.1.0D-10) GN0=0.0D0
              IF(DABS(VECTORL(J)).LE.1.0D-10) VECTORL(J)=0.0D0
              IF(DABS(VECTORM(J)).LE.1.0D-10) VECTORM(J)=0.0D0
              IF(DABS(VECTORN(J)).LE.1.0D-10) VECTORN(J)=0.0D0
C       TAKEN OUT ON 5/14/2004
C       WRITE(OUTLYNE,*) J
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*) GL,GM,GN
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*) VECTORL(J),VECTORM(J),VECTORN(J)
C       CALL SHOWIT(1)
              IF(REAL(VECTORL(J)).NE.REAL(GL).OR.
     1        REAL(VECTORM(J)).NE.REAL(GM).OR.
     2        REAL(VECTORN(J)).NE.REAL(GN)) THEN
                  LVECTOR(J)=-1.0D0
C       IF(J.EQ.25) THEN
C        write(outlyne,*) 'heref',J
C        call showit(1)
C               END IF
              END IF
          ELSE
              LVECTOR(J)=-1.0D0
C       IF(J.EQ.25) THEN
C        write(outlyne,*) 'hereg lvector < 0',J
C        call showit(1)
C               END IF
          END IF
          IF(DABS(LVECTOR(J)).LT.1.0D-8) THEN
              LVECTOR(J)=-1.0D0
C       IF(J.EQ.25) THEN
C        write(outLlyne,*) 'hereh lvect too small',J
C        call showit(1)
C               END IF
          END IF
C       AS WE RETURN, WE HAVE A RAY WHICH STRICKS A SURFACE AFTER TRAVELING
C       A NON-ZERO POSITIVE DISTANCE AND WHICH HITS THE J SURFACE INSIDE
C       BOUNDS, CLAPS AND OUTSIDE HOLES OR IT HAS A NEGATIVE LVECTOR.
          RETURN
      END
      SUBROUTINE NSSNR2(J)
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE NSSNR2.INC. THIS SUBROUTINE IMPLEMENTS
C       INTERSECTION CALCULATION TO AN ASPHERIC SURFACE
C
          INTEGER I,J
          REAL*8 A4,A6,A8,A10,A12,A14,A16,A18,A20,QQ
          REAL*8 CV,AD,AE,AF,AG,AH,AI,AJ,AK,AL,R,RHO,FUNC,NX,NY
          REAL*8 CC,AQ,Q,C,K,RTEST
C
          REAL*8 LENG,MAG,ZCAL,ZZTOP,X,Y
     1    ,FNXP,FNYP,FNZP,HV1,HV2,HV,XP,YP,ZP,DEL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C       SURFACE SAG FUNCTION
          FUNC(R,C,K,A4,A6,A8,A10,A12,A14,A16,A18,A20)=
     1    (((R**2)*C)/(1.0D0+DSQRT(1.0D0-((K+1.0D0)*(C**2)*(R**2)))))
     2    +(A4*(R**4))+(A6*(R**6))+(A8*(R**8))
     3    +(A10*(R**10))+(A12*(R**12))+(A14*(R**14))+(A16*(R**16))
     4    +(A18*(R**18))+(A20*(R**20))
C
C       SURFACE X DERIVATIVE FUNCTION
          NX(X,Y,C,K,A4,A6,A8,A10,A12,A14,A16,A18,A20,QQ)=(
     1    (((2.0D0*C*X*QQ*(QQ-1.0D0))+
     1    ((C**3)*X*((X**2)+(Y**2))*(K+1.0D0)))/((QQ-1.0D0)*(QQ**2)))
     1    +((2.0D0*((X**2)+(Y**2))*2.0D0*X)*A4)
     1    +((3.0D0*(((X**2)+(Y**2))**2)*2.0D0*X)*A6)
     1    +((4.0D0*(((X**2)+(Y**2))**3)*2.0D0*X)*A8)
     1    +((5.0D0*(((X**2)+(Y**2))**4)*2.0D0*X)*A10)
     1    +((6.0D0*(((X**2)+(Y**2))**5)*2.0D0*X)*A12)
     1    +((7.0D0*(((X**2)+(Y**2))**6)*2.0D0*X)*A14)
     1    +((8.0D0*(((X**2)+(Y**2))**7)*2.0D0*X)*A16)
     1    +((9.0D0*(((X**2)+(Y**2))**8)*2.0D0*X)*A18)
     1    +((10.0D0*(((X**2)+(Y**2))**9)*2.0D0*X)*A20)
     1    )
C
C       SURFACE Y DERIVATIVE FUNCTION
          NY(X,Y,C,K,A4,A6,A8,A10,A12,A14,A16,A18,A20,QQ)=(
     1    (((2.0D0*C*Y*QQ*(QQ-1.0D0))+
     1    ((C**3)*Y*((X**2)+(Y**2))*(K+1.0D0)))/((QQ-1.0D0)*(QQ**2)))
     1    +((2.0D0*((X**2)+(Y**2))*2.0D0*Y)*A4)
     1    +((3.0D0*(((X**2)+(Y**2))**2)*2.0D0*Y)*A6)
     1    +((4.0D0*(((X**2)+(Y**2))**3)*2.0D0*Y)*A8)
     1    +((5.0D0*(((X**2)+(Y**2))**4)*2.0D0*Y)*A10)
     1    +((6.0D0*(((X**2)+(Y**2))**5)*2.0D0*Y)*A12)
     1    +((7.0D0*(((X**2)+(Y**2))**6)*2.0D0*Y)*A14)
     1    +((8.0D0*(((X**2)+(Y**2))**7)*2.0D0*Y)*A16)
     1    +((9.0D0*(((X**2)+(Y**2))**8)*2.0D0*Y)*A18)
     1    +((10.0D0*(((X**2)+(Y**2))**9)*2.0D0*Y)*A20)
     1    )
C
C       Q FUNCTION
          Q(X,Y,C,K)=
     1    (1.0D0+DSQRT(1.0D0-((K+1.0D0)*(C**2)*((X**2)+(Y**2)))))
C
C
C
          CV=NSSALENS(3,J)
          CC=NSSALENS(4,J)
          RTEST=1.0D0-((CC+1.0D0)*(CV**2)*((RRX**2)+(RRY**2)))
          IF(RTEST.LT.0.0D0) THEN
              NSS_INTERSECT=.FALSE.
              RETURN
          END IF
          AD=NSSALENS(5,J)
          AE=NSSALENS(6,J)
          AF=NSSALENS(7,J)
          AG=NSSALENS(201,J)
          AH=NSSALENS(202,J)
          AI=NSSALENS(203,J)
          AJ=NSSALENS(204,J)
          AK=NSSALENS(205,J)
          AL=NSSALENS(206,J)

C
          DEL=DELSUR
C
C       THE RETURNED VALUES OF X,Y,Z ARE THE CORRECT INTERSECTIONS
C       TO WITHIN THE SURTOL VALUE
C
C       CALCULATE INTERSECTION
          DO I=1,NRAITR
C
C       X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
              AQ=Q(RRX,RRY,CV,CC)
              FNXP=-NX(RRX,RRY,CV,CC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AQ)
              FNYP=-NY(RRX,RRY,CV,CC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AQ)
              FNZP=1.0D0
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
              MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
              NSSLN=FNXP/MAG
              NSSMN=FNYP/MAG
              NSSNN=FNZP/MAG
C       THE INTERSECTION OF THE RAY WITH THIS TANGENT
C       PLANE
C       USING:
C
              RHO=DSQRT((RRX**2)+(RRY**2))
              ZCAL=FUNC(RHO,CV,CC,AD,AE,AF,AG,AH,AI,AJ,AK,AL)
              HV1=(ZCAL-RRZ)
              HV2=((RRL*NSSLN)+(RRM*NSSMN)+(RRN*NSSNN))
              HV=HV1/HV2
C       NEW X,Y AND Z ARE
              XP=RRX+(HV*RRL)
              YP=RRY+(HV*RRM)
              ZP=RRZ+(HV*RRN)
C       SHOULD WE CONTINUE?
              RHO=DSQRT((XP**2)+(YP**2))
              RTEST=1.0D0-((CC+1.0D0)*(CV**2)*((RHO**2)))
              IF(RTEST.LT.0.0D0) THEN
                  NSS_INTERSECT=.FALSE.
                  RETURN
              END IF
              ZZTOP=FUNC(RHO,CV,CC,AD,AE,AF,AG,AH,AI,AJ,AK,AL)
              LENG=DABS(ZZTOP-ZP)
              IF(LENG.LE.SURTOL) THEN
                  RRX=XP
                  RRY=YP
                  RRZ=ZP
C
C     X AND Y DERIVATIVES OF THE SURFACE ARE CALCULATED HERE:
C
                  AQ=Q(RRX,RRY,CV,CC)
                  FNXP=-NX(RRX,RRY,CV,CC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AQ)
                  FNYP=-NY(RRX,RRY,CV,CC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AQ)
                  FNZP=1.0D0
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
                  MAG=DSQRT((FNXP**2)+(FNYP**2)+(FNZP**2))
                  NSSLN=FNXP/MAG
                  NSSMN=FNYP/MAG
                  NSSNN=FNZP/MAG
                  RETURN
              ELSE
C       KEEP TRYING
C       MAKE X Y Z FROM XP YP ZP AND TRY AGAIN
                  RRX=XP
                  RRY=YP
                  RRZ=ZP
              END IF
          END DO
C       INTERATIONS EXCEEDED, NO INTERSECTION WAS POSSIBLE
          NSS_INTERSECT=.FALSE.
          RETURN
      END
C SUB NSSPLTSZ.FOR
      SUBROUTINE NSSPLTSZ
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT NSSSCALE
C       COMMANDS AT THE CMD LEVEL
C
          CHARACTER PU1*11
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       SCALE FACTORS IN PLOTTING
C
C       PLOT SCALE
C
C       CHECK SYNTAX
          IF(.NOT.NEXISTN) THEN
              WRITE(OUTLYNE,*) 'NO NSS DATABASE EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'NO NSS SCALE FACTOR MAY BE SET'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT NSSSCALE" TAKES NO NUMERIC WORD #2, #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT NSSSCALE" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.0) THEN
              IF(DF1.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT NSSSCALE REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(STI.EQ.1) THEN
              IF(NSSSYSTEM(21).EQ.1.0D0) PU1='INCHES     '
              IF(NSSSYSTEM(21).EQ.2.0D0) PU1='CENTIMETERS'
              IF(NSSSYSTEM(21).EQ.3.0D0) PU1='MILLIMETERS'
              IF(NSSSYSTEM(21).EQ.4.0D0) PU1='METERS     '
              WRITE(OUTLYNE,189) NSSPSIZ,PU1
              CALL SHOWIT(1)
              RETURN
          END IF

 189      FORMAT('NSS SCALE FACTOR = ',G15.8,1X,A11,
     1     ' OF FULL SIZE')
C       CHECK FOR ZERO VALUES AND DIS-ALLOW
          IF(W1.LE.0.0D0) THEN
              OUTLYNE='"PLOT NSSSCALE" REQUIRES NON-ZERO, POSTIVE INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          NSSSCFAP=1.0D0/W1
          NSSPSIZP=W1
          IF(NSSSYSTEM(21).EQ.1.0D0) NSSSCFA=NSSSCFAP
          IF(NSSSYSTEM(21).EQ.2.0D0) NSSSCFA=NSSSCFAP*2.54D0
          IF(NSSSYSTEM(21).EQ.3.0D0) NSSSCFA=NSSSCFAP*25.4D0
          IF(NSSSYSTEM(21).EQ.4.0D0) NSSSCFA=NSSSCFAP*0.0254
          NSSPSIZ=1.0D0/NSSSCFA
          NSSPLSZ=.TRUE.
          NSSPLSC=.FALSE.
          RETURN
      END
