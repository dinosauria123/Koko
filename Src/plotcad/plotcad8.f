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

C       EIGHT FILE OF PLOT/CAD ROUTINES

C SUB DOVUE.FOR
      SUBROUTINE DOVUE
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE VIEW ANGLES
C
          CHARACTER NNTT1*80,VALUE*10,B*140
C
          INTEGER NT1ANG,NT1SIZ,IIX,IIY,IIUD
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          NT1SIZ=1
          NT1ANG=0
C     DO THE PLOTTING OF THE ELEVATION ANGLE
          IIX=4000
          IIY=500
          IIUD=0
          CALL MY_PLOT(IIX,IIY,IIUD,0,-10,10010,-10,7010)
          WRITE(B,180)REAL(VIEALF)
          READ(B,200) VALUE
180       FORMAT(G10.3)
200       FORMAT(A10)
          NNTT1='ELEVATION ANGLE = '//VALUE
          CALL MY_SETCHARASPECT(1.5,1.5)
          CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:28),NT1ANG,NT1SIZ,3)
C     DO THE PLOTTING OF THE AZUMITH ANGLE
          IIX=4000
          IIY=300
          IIUD=0
          WRITE(B,180)REAL(VIEPHI)
          READ(B,200) VALUE
          NNTT1='AZIMUTH   ANGLE = '//VALUE
          CALL MY_SETCHARASPECT(1.5,1.5)
          CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:28),NT1ANG,NT1SIZ,3)
          RETURN
      END


C SUB DOSZ.FOR
      SUBROUTINE DOSZ
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE "SCALE FACTOR"
C
          CHARACTER NNTT1*80,VALUE*10,B*140
C
          INTEGER COLPAS,NT1ANG,NT1SIZ,IIX,IIY,IIUD,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
C     MOVE TO THE STARTING POSITION FOR THE LI
C
          NT1SIZ=1
          NT1ANG=0
          IF(PSIZXP.NE.PSIZYP) THEN
C     SHOW BOTH SCALE FACTORS ON THE PLOT
C     DO THE PLOTTING OF THE X SCALE FACTOR
              IIX=100
              IIY=500
              IIUD=0
              WRITE(B,180)REAL(PSIZXP)
              READ(B,200) VALUE
              DO I=10,1,-1
                  IF(VALUE(I:I).NE.' ') GO TO 11
                  VALUE(I:I)='0'
              END DO
11            CONTINUE
180           FORMAT(G10.3)
200           FORMAT(A10)
              NNTT1='X-SCALE FACTOR = '//VALUE//' X'
              CALL MY_SETCHARASPECT(1.5,1.5)
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:29),NT1ANG,NT1SIZ,3)
C     DO THE PLOTTING OF THE Y SIZE FACTOR
              IIX=100
              IIY=300
              IIUD=0
              WRITE(B,180)REAL(PSIZYP)
              READ(B,200) VALUE
              DO I=10,1,-1
                  IF(VALUE(I:I).NE.' ') GO TO 12
                  VALUE(I:I)='0'
              END DO
12            CONTINUE
              NNTT1='Y-SCALE FACTOR = '//VALUE//' X'
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:29),NT1ANG,NT1SIZ,3)
          ELSE
              NT1SIZ=1
              NT1ANG=0
C     DO THE PLOTTING OF THE Y SIZE FACTOR
              IIX=100
              IIY=300
              IIUD=0
              WRITE(B,180)REAL(PSIZYP)
              READ(B,200) VALUE
              DO I=10,1,-1
                  IF(VALUE(I:I).NE.' ') GO TO 121
                  VALUE(I:I)='0'
              END DO
121           CONTINUE
              NNTT1='SCALE FACTOR = '//VALUE//' X'
              CALL MY_SETCHARASPECT(1.5,1.5)
              CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:29),NT1ANG,NT1SIZ,3)
C     JUST SHOW ONE FACTOR SINCE BOTH ARE THE SAME
          END IF
          RETURN
      END


C SUB DOSC.FOR
      SUBROUTINE DOSC
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE "SIZE FACTORS"
C
          CHARACTER NNTT1*80,VALUE*10,B*140
C
          INTEGER COLPAS,NT1ANG,NT1SIZ,IIX,IIY,IIUD,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
C     MOVE TO THE STARTING POSITION FOR THE LI
C
          NT1SIZ=1
          NT1ANG=0
          IF(SCFAXP.NE.SCFAYP) THEN
C     PLOT BOTH FACTORS
C     DO THE PLOTTING OF THE X SCALE FACTOR
              IIX=100
              IIY=500
              IIUD=0
              WRITE(B,180)REAL(SCFAXP)
              READ(B,200) VALUE
              DO I=10,1,-1
                  IF(VALUE(I:I).NE.' ') GO TO 10
                  VALUE(I:I)='0'
              END DO
10            CONTINUE
180           FORMAT(G10.3)
200           FORMAT(A10)
              IF(SYSTEM1(6).EQ.1.0D0)
     1        NNTT1='(X-SIZE) ONE INCH = '//VALUE//' in(s)'
              IF(SYSTEM1(6).EQ.2.0D0)
     1        NNTT1='(X-SIZE) ONE INCH = '//VALUE//' cm(s)'
              IF(SYSTEM1(6).EQ.3.0D0)
     1        NNTT1='(X-SIZE) ONE INCH = '//VALUE//' mm(s)'
              IF(SYSTEM1(6).EQ.4.0D0)
     1        NNTT1='(X-SIZE) ONE INCH = '//VALUE//' m(s) '
              CALL MY_SETCHARASPECT(1.5,1.5)
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:36),NT1ANG,NT1SIZ,3)
C     DO THE PLOTTING OF THE Y SCALE FACTOR
              IIX=100
              IIY=300
              IIUD=0
              WRITE(B,180)REAL(SCFAYP)
              READ(B,200) VALUE
              DO I=10,1,-1
                  IF(VALUE(I:I).NE.' ') GO TO 11
                  VALUE(I:I)='0'
              END DO
11            CONTINUE
              IF(SYSTEM1(6).EQ.1.0D0)
     1        NNTT1='(Y-SIZE) ONE INCH = '//VALUE//' in(s)'
              IF(SYSTEM1(6).EQ.2.0D0)
     1        NNTT1='(Y-SIZE) ONE INCH = '//VALUE//' cm(s)'
              IF(SYSTEM1(6).EQ.3.0D0)
     1        NNTT1='(Y-SIZE) ONE INCH = '//VALUE//' mm(s)'
              IF(SYSTEM1(6).EQ.4.0D0)
     1        NNTT1='(Y-SIZE) ONE INCH = '//VALUE//' m(s) '
              CALL MY_SETCHARASPECT(1.5,1.5)
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:36),NT1ANG,NT1SIZ,3)
          ELSE
C     DO THE PLOTTING OF THE SIZE FACTOR
              IIX=100
              IIY=300
              IIUD=0
              WRITE(B,180)REAL(SCFAYP)
              READ(B,200) VALUE
              DO I=10,1,-1
                  IF(VALUE(I:I).NE.' ') GO TO 13
                  VALUE(I:I)='0'
              END DO
13            CONTINUE
              IF(SYSTEM1(6).EQ.1.0D0)
     1        NNTT1='(SIZE) ONE INCH = '//VALUE//' in(s)'
              IF(SYSTEM1(6).EQ.2.0D0)
     1        NNTT1='(SIZE) ONE INCH = '//VALUE//' cm(s)'
              IF(SYSTEM1(6).EQ.3.0D0)
     1        NNTT1='(SIZE) ONE INCH = '//VALUE//' mm(s)'
              IF(SYSTEM1(6).EQ.4.0D0)
     1        NNTT1='(SIZE) ONE INCH = '//VALUE//' m(s) '
              CALL MY_SETCHARASPECT(1.5,1.5)
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:34),NT1ANG,NT1SIZ,3)
          END IF
          RETURN
      END


C SUB DOLOK.FOR
      SUBROUTINE DOLOK
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE LOOK VECTOR
C
          CHARACTER NNTT1*80,VALUE*10,B*140
C
          INTEGER COLPAS,NT1ANG,NT1SIZ,IIX,IIY,IIUD
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          NT1SIZ=1
          NT1ANG=0
C     DO THE PLOTTING OF THE X LOOK VECTOR
          IIX=4000
          IIY=700
          IIUD=0
          WRITE(B,180)REAL(LOOKX)
          READ(B,200) VALUE
180       FORMAT(G10.3)
200       FORMAT(A10)
          NNTT1='X-LOOK VECTOR = '//VALUE
          CALL MY_SETCHARASPECT(1.5,1.5)
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:26),NT1ANG,NT1SIZ,3)
C     DO THE PLOTTING OF THE Y LOOK VECTOR
          IIX=4000
          IIY=500
          IIUD=0
          WRITE(B,180)REAL(LOOKY)
          READ(B,200) VALUE
          NNTT1='Y-LOOK VECTOR = '//VALUE
          CALL MY_SETCHARASPECT(1.5,1.5)
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:26),NT1ANG,NT1SIZ,3)
C     DO THE PLOTTING OF THE Z LOOK VECTOR
          IIX=4000
          IIY=300
          IIUD=0
          WRITE(B,180)REAL(LOOKZ)
          READ(B,200) VALUE
          NNTT1='Z-LOOK VECTOR = '//VALUE
          CALL MY_SETCHARASPECT(1.5,1.5)
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:26),NT1ANG,NT1SIZ,3)
          RETURN
      END


C SUB DOLBL.FOR
      SUBROUTINE DOLBL
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE LENS "LBL"
C
          CHARACTER BL20*20,BLNOTE*80,NNTT1*80
C
          INTEGER COLPAS,NT1ANG,NT1SIZ,IIX,IIY,IIUD,I,IB
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
C
C     MOVE TO THE STARTING POSITION FOR THE LBL
C
          IIX=100
          IIY=6800
          IIUD=0

C     DO THE PLOTTING OF THE LENS IDENTIFIER
          NT1SIZ=1
          NT1ANG=0
          NNTT1=LBL(LBLSURF)
          IB=1
          DO I=80,1,-1
              IF(NNTT1(I:I).NE.' ') THEN
                  IB=I
                  GO TO 100
              END IF
          END DO
 100      CONTINUE
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_SETCHARASPECT(1.5,1.5)
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:IB),NT1ANG,NT1SIZ,3)
          ELSE
C     LBL BLANK, NOT ACTION
          END IF
C
          RETURN
      END


C SUB PLTFIG.FOR
      SUBROUTINE PLTFIG
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE FIG
C
          CHARACTER BL20*20,BLNOTE*80,NNTT1*141
C
          INTEGER I,IB,COLPAS,NT1ANG,NT1SIZ,IIX,IIY,IIUD
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
C
C     MOVE TO THE STARTING POSITION FOR THE FIG
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT FIGURE" PLOTS A FIGURE NAME'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT FIGURE" ONLY TAKES OPTIONAL NUMERIC WORDS #1 AND #2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF


          IF(DF1.EQ.1) IIX=100
          IF(DF2.EQ.1) IIY=900
          IF(DF1.EQ.0) IIX=INT(W1)
          IF(DF2.EQ.0) IIY=INT(W2)
          IIUD=0
C     DO THE PLOTTING OF THE FIG
          NT1SIZ=1
          NT1ANG=0
          NNTT1=FIGTITLE
          IB=1
          DO I=80,1,-1
              IF(NNTT1(I:I).NE.' ') THEN
                  IB=I
                  GO TO 100
              END IF
          END DO
 100      CONTINUE
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_SETCHARASPECT(1.5,1.5)
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:IB),NT1ANG,NT1SIZ,3)
          ELSE
C     FIGTITLE BLANK, NOT ACTION
          END IF
C
          RETURN
      END


C SUB DOLI.FOR
      SUBROUTINE DOLI
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE LENS IDENTIFIER "LI"
C
          CHARACTER TMY*8,DTY*10,BL20*20,BLNOTE*80,NNTT1*99
C
          INTEGER COLPAS,NT1ANG,NT1SIZ,IIX,IIY,IIUD
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
C
C     MOVE TO THE STARTING POSITION FOR THE LI
C
          IIX=100
          IIY=100
          IIUD=0

C     DO THE PLOTTING OF THE LENS IDENTIFIER
          NT1SIZ=1
          NT1ANG=0
          IF(STMPT) CALL MYTIME(TMY)
          IF(STMPD) CALL MYDATE(DTY)
          IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
          IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
          IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
          IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_SETCHARASPECT(1.5,1.5)
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:80),NT1ANG,NT1SIZ,3)
          ELSE
C     LI BLANK, NOT ACTION
          END IF
C
          RETURN
      END


C SUB VIGSHO.FOR
      SUBROUTINE VIGSHO
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF VIGNETTING STATUS IN VIE PLOTS
C
          CHARACTER NNTT1*80
C
          INTEGER COLPAS,NT1ANG,NT1SIZ,IIX,IIY,IIUD
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          IIX=100
          IIY=700
          IIUD=0
          NT1SIZ=1
          NT1ANG=0
          IF(.NOT.VIGOFF) THEN
              NNTT1='AUTOMATIC VIGNETTING IS OPERATIONAL'
              CALL MY_SETCHARASPECT(1.5,1.5)
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:40),NT1ANG,NT1SIZ,3)
          END IF
          RETURN
      END

C SUB DOPNAME.FOR
      SUBROUTINE DOPNAME(PPPLI)
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE PLOT NAME
C
          CHARACTER PPPLI*60,TMY*8,DTY*10,BL20*20,BLNOTE*80,NNTT1*80
C
          INTEGER COLPAS,NT1ANG,NT1SIZ,IIX,IIY,IIUD
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
C
C     MOVE TO THE STARTING POSITION FOR THE LI
C
          IIX=100
          IIY=6600
          IIUD=0

C     DO THE PLOTTING OF THE LENS IDENTIFIER
          NT1SIZ=1
          NT1ANG=0
          IF(STMPT) CALL MYTIME(TMY)
          IF(STMPD) CALL MYDATE(DTY)
          IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=PPPLI(1:60)
          IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//PPPLI(1:60)
          IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//PPPLI(1:60)
          IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//PPPLI(1:60)
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_SETCHARASPECT(1.5,1.5)
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(IIX,IIY,NNTT1(1:80),NT1ANG,NT1SIZ,3)
          ELSE
C     PLLI BLANK, NOT ACTION
          END IF
C
          RETURN
      END


C SUB DOAX.FOR
      SUBROUTINE DOAX
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE AXES IN LENS PLOTTING
C
          CHARACTER XXX*1,YYY*1,ZZZ*1
C
          REAL*8 A0,XO,YO,ZO,XX,YX,ZX,XY,YY,ZY,XZ,YZ,ZZ
     1    ,ROT1X,ROT1Z,ROT2Z,ROT2Y,AX,AY,AZ,APHI,AALF,XN,YN,ZN
     2    ,X,Y,Z,VIEPH,VIEAL,ZETA,DELXX,DELYY,AA1
C
          INTEGER COLPAS,IIX,IIY,IIUD,NT1SZ
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
C     LOOK.VIEW TRANSFORMS

          ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
          ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
          ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
          ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
C
          COLPAS=COLAXS
          CALL MY_COLTYP(COLPAS)
C
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C
          NT1SZ=1
          CALL MY_SETCHARASPECT(1.5,1.5)
C
          XXX='X'
          YYY='Y'
          ZZZ='Z'
C
C     MOVE TO THE STARTING POSITION FOR THE ORIGIN OF THE AXES
C
C     THE LENGTH OF THE AXES ARE 500 UNITS
          A0=500.0D0
C     BEFORE AND AFTER ROTATION, THE ORIGIN IS ALWAYS AT X=8900,Y=1100
C     THE COORDINATE OF THE POSITIVE END OF THE X AXIS IS AT:
C     X=9400,Y=1100,Z=0
C     THE COORDINATE OF THE POSITIVE END OF THE Y AXIS IS AT:
C     X=9400,Y=1600,Z=0
C     THE COORDINATE OF THE POSITIVE END OF THE Z AXIS IS AT:
C     X=8900,Y=1100,Z=500
C
          XO=8900.0D0
          YO=1100.0D0
          ZO=0.0D0
C
          XX=XO+A0
          YX=YO
          ZX=ZO
C
          XY=XO
          YY=YO+A0
          ZY=ZO
C
          XZ=XO
          YZ=YO
          ZZ=ZO+A0
C
C     NOW APPLY THE ROTATIONS FROM PLOT LOOK
C     THE ORIGIN NEVER MOVES
          X=XX-XO
          Y=YX-YO
          Z=ZX-ZO
          XN=ROT1X(X,Z,VIEPH)
          YN=Y
          ZN=ROT1Z(X,Z,VIEPH)
          X=XN
          Y=YN
          Z=ZN
          XN=X
          YN=ROT2Y(Z,Y,VIEAL)
          ZN=ROT2Z(Z,Y,VIEAL)
          XX=XN+XO
          YX=YN+YO
          ZX=ZN+ZO
C
          X=XY-XO
          Y=YY-YO
          Z=ZY-ZO
          XN=ROT1X(X,Z,VIEPH)
          YN=Y
          ZN=ROT1Z(X,Z,VIEPH)
          X=XN
          Y=YN
          Z=ZN
          XN=X
          YN=ROT2Y(Z,Y,VIEAL)
          ZN=ROT2Z(Z,Y,VIEAL)
          XY=XN+XO
          YY=YN+YO
          ZY=ZN+ZO
C
          X=XZ-XO
          Y=YZ-YO
          Z=ZZ-ZO
          XN=ROT1X(X,Z,VIEPH)
          YN=Y
          ZN=ROT1Z(X,Z,VIEPH)
          X=XN
          Y=YN
          Z=ZN
          XN=X
          YN=ROT2Y(Z,Y,VIEAL)
          ZN=ROT2Z(Z,Y,VIEAL)
          XZ=XN+XO
          YZ=YN+YO
          ZZ=ZN+ZO
C
C
C     NOW CONVERT TO INTEGERS AND DRAW THE 3 LINES WITH EACH LINE TERMINATING
C     WITH THE APPROPRIATE AXIS LABEL
C
C     MOVE TO THE STARTING POSITION FOR THE ORIGIN OF THE AXES
          IIX=8900
          IIY=1100
          IIUD=0
          CALL MY_PLOT(IIX,IIY,IIUD,0,-10,10010,-10,7010)
C     DRAW TO THE TIP OF THE X-AXIS
          IIX=INT(XX)
          IIY=INT(YX)
          IIUD=1
          CALL MY_PLOT(IIX,IIY,IIUD,0,-10,10010,-10,7010)
C
C     LABEL THE AXIS
C
          AA1=88.0D0
          IF(YX.EQ.1100.AND.XX.EQ.8900) THEN
C     DON'T WRITE AN AXIS LABEL
          ELSE
C     WRITE LABEL
              IF(DABS(YX-1100.0D0).LE.1.0D-15.AND.
     1        DABS(XX-8900.0D0).LE.1.0D-15) THEN
                  ZETA=0.0D0
              ELSE
                  ZETA=DATAN2((YX-1100.0D0),(XX-8900.0D0))
              END IF
              DELYY=AA1*DSIN(ZETA)
              DELXX=AA1*DCOS(ZETA)
              YX=YX+DELYY
              XX=XX+DELXX
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(INT(XX)-40,INT(YX)-40,XXX,0,NT1SZ,3)
              COLPAS=COLAXS
              CALL MY_COLTYP(COLPAS)
          END IF
C
C     MOVE TO THE STARTING POSITION FOR THE ORIGIN OF THE AXES
          IIX=8900
          IIY=1100
          IIUD=0
          CALL MY_PLOT(IIX,IIY,IIUD,0,-10,10010,-10,7010)
C     DRAW TO THE TIP OF THE Y-AXIS
          IIX=INT(XY)
          IIY=INT(YY)
          IIUD=1
          CALL MY_PLOT(IIX,IIY,IIUD,0,-10,10010,-10,7010)
C
          IF(YY.EQ.1100.AND.XY.EQ.8900) THEN
C     DON'T WRITE AN AXIS LABEL
          ELSE
C     WRITE LABEL
              IF(DABS(YY-1100.0D0).LE.1.0D-15.AND.
     1        DABS(XY-8900.0D0).LE.1.0D-15) THEN
                  ZETA=0.0D0
              ELSE
                  ZETA=DATAN2((YY-1100.0D0),(XY-8900.0D0))
              END IF
              DELYY=AA1*DSIN(ZETA)
              DELXX=AA1*DCOS(ZETA)
              YY=YY+DELYY
              XY=XY+DELXX
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(INT(XY)-40,INT(YY)-40,YYY,0,NT1SZ,3)
              COLPAS=COLAXS
              CALL MY_COLTYP(COLPAS)
          END IF
C
C     MOVE TO THE STARTING POSITION FOR THE ORIGIN OF THE AXES
          IIX=8900
          IIY=1100
          IIUD=0
          CALL MY_PLOT(IIX,IIY,IIUD,0,-10,10010,-10,7010)
C     DRAW TO THE TIP OF THE Z-AXIS
          IIX=INT(XZ)
          IIY=INT(YZ)
          IIUD=1
          CALL MY_PLOT(IIX,IIY,IIUD,0,-10,10010,-10,7010)
C
          IF(YZ.EQ.1100.AND.XZ.EQ.8900) THEN
C     DON'T WRITE AN AXIS LABEL
          ELSE
C     WRITE LABEL
              IF(DABS(YZ-1100.0D0).LE.1.0D-15.AND.
     1        DABS(XZ-8900.0D0).LE.1.0D-15) THEN
                  ZETA=0.0D0
              ELSE
                  ZETA=DATAN2((YZ-1100.0D0),(XZ-8900.0D0))
              END IF
              DELYY=AA1*DSIN(ZETA)
              DELXX=AA1*DCOS(ZETA)
              YZ=YZ+DELYY
              XZ=XZ+DELXX
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(INT(XZ)-40,INT(YZ)-40,ZZZ,0,NT1SZ,3)
              COLPAS=COLAXS
              CALL MY_COLTYP(COLPAS)
          END IF
          RETURN
      END


      SUBROUTINE DNOTES(NOTE_NUMBER,NOTEA,LINES)
          IMPLICIT NONE
          INTEGER NOTE_NUMBER,LINES,I
          CHARACTER*80 NOTEA(1:15)
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datpts.inc'
          DO I=1,15
              NOTEA(I)=AA//AA//AA//AA
          END DO
          IF(NOTE_NUMBER.EQ.100) GO TO 100
          IF(NOTE_NUMBER.EQ.11)   GO TO 11
          IF(NOTE_NUMBER.EQ.12)   GO TO 12
          IF(NOTE_NUMBER.EQ.13)   GO TO 13
          IF(NOTE_NUMBER.EQ.14)   GO TO 14
          IF(NOTE_NUMBER.EQ.15)   GO TO 15
          IF(NOTE_NUMBER.EQ.16)   GO TO 16
          IF(NOTE_NUMBER.EQ.2)    GO TO 2
          IF(NOTE_NUMBER.EQ.3)    GO TO 3
          IF(NOTE_NUMBER.EQ.4)    GO TO 4
          IF(NOTE_NUMBER.EQ.5)    GO TO 5
          IF(NOTE_NUMBER.EQ.6)    GO TO 6
          IF(NOTE_NUMBER.EQ.7)    GO TO 7
          IF(NOTE_NUMBER.EQ.81)   GO TO 81
          IF(NOTE_NUMBER.EQ.82)   GO TO 82
          IF(NOTE_NUMBER.EQ.9)    GO TO 82
          IF(NOTE_NUMBER.EQ.10)   GO TO 82
          IF(NOTE_NUMBER.EQ.1100) GO TO 1100
100       CONTINUE
C     HEADING
          LINES=1
          NOTEA(1)= 'UNLESS OTHERWIZE SPECIFIED'
          RETURN
11        CONTINUE
C     NOTE#1 (ZNS)
          LINES=8
          NOTEA(1)=
     1    '1. MATERIAL: POLYCRYSTALLINE OPTICAL GRADE ZINC SULFIDE (ZNS),'
          NOTEA(2)=
     1    'CHEMICAL VAPOR DEPOSITED (CVD), STRESS FREE, FINE ANNEALED.'
          NOTEA(3)=
     1    'THE AVERAGE TRANSMITTANCE THROUGH A POLISHED, UNCOATED 0.20'
          NOTEA(4)=
     1    'IN. THICK (MIN.), NOMINAL SIZE SAMPLE CUT FROM THE SAME LOT'
          NOTEA(5)=
     1    'OF MATERIAL AS THE LENS BLANK SHALL NOT BE LESS THAN:'
          NOTEA(6)=
     1    'A.   2.0 -  5.0 MICROMETERS 70%'
          NOTEA(7)=
     1    'B.   7.5 - 10.0 MICROMETERS 71%'
          NOTEA(8)=
     1    'C.     11.25    MICROMETERS 60%'
          RETURN
12        CONTINUE
C     NOTE#1 (GE or GERM)
          LINES=8
          NOTEA(1)=
     1    '1. MATERIAL: POLYCRYSTALLINE OPTICAL GRADE (5-30 OHM-CM) N-TYPE'
          NOTEA(2)=
     1    'GERMANIUM, STRESS FREE, FINE ANNEALED. THE TRANSMITTANCE'
          NOTEA(3)=
     1    'THROUGH A POLISHED, UNCOATED 0.20 IN. THICK (MIN.) NOMINAL'
          NOTEA(4)=
     1    'SIZE SAMPLE CUT FROM THE SAME LOT OF MATERIAL AS THE LENS'
          NOTEA(5)=
     1    'BLANK SHALL NOT BE LESS THAN:'
          NOTEA(6)=
     1    'A.   2.5 - 10.0 MICROMETERS 46%'
          NOTEA(7)=
     1    'B.  10.0 - 11.0 MICROMETERS 45%'
          NOTEA(8)=
     1    'C.  11.0 - 11.5 MICROMETERS 43%'
          RETURN
13        CONTINUE
C     NOTE#1 (ZNSE)
          LINES=6
          NOTEA(1)=
     1    '1. MATERIAL: POLYCRYSTALLINE OPTICAL GRADE ZINC SELENIDE (ZNS),'
          NOTEA(2)=
     1    'CHEMICAL VAPOR DEPOSITED (CVD), STRESS FREE, FINE ANNEALED.'
          NOTEA(3)=
     1    'THE AVERAGE TRANSMITTANCE THROUGH A POLISHED, UNCOATED 0.20'
          NOTEA(4)=
     1    'IN. THICK (MIN.), NOMINAL SIZE SAMPLE CUT FROM THE SAME LOT'
          NOTEA(5)=
     1    'OF MATERIAL AS THE LENS BLANK SHALL NOT BE LESS THAN 70%'
          NOTEA(6)=
     1    'AVERAGED OVER 2.0 TO 12.0 MICROMETERS'
14        CONTINUE
C     NOTE#1 (OMIT)
          LINES=11
          NOTEA(1)=
     1    '1. MATERIAL:'
          RETURN
15        CONTINUE
C     NOTE#1 (AMTIR or AMTIR1)
          LINES=12
          NOTEA(1)=
     1    '1. MATERIAL: OPTICAL GRADE AMTIR-1 OR EQUIVALENT THAT MEETS'
          NOTEA(2)=
     1    'THE FOLLOWING SPECIFICATIONS:'
          NOTEA(3)=
     1    'A.  STRIA FREE.'
          NOTEA(4)=
     1    'B.  BUBBLE AND INCLUSION FREE WHEN OBSERVED THROUGH A 10X'
          NOTEA(5)=
     1    'INFRARED MICROSCOPE.'
          NOTEA(6)=
     1    'C.  TRANSMITTANCE THROUGH A POLISHED, UNCOATED 0.20 IN.'
          NOTEA(7)=
     1    '    THICK (MIN.) NOMINAL SIZE SAMPLE CUR FROM THE SAME MELT'
          NOTEA(8)=
     1    '    RUN AS THE LENS BLANK SHALL NOT BE LESS THAN:'
          NOTEA(9)=
     1    '      2.0 - 10.5 MICROMETERS 67%'
          NOTEA(10)=
     1    '         11.0    MICROMETERS 65%'
          NOTEA(11)=
     1    '         11.5    MICROMETERS 63%'
          NOTEA(12)=
     1    '         12.0    MICROMETERS 62%'
          RETURN
16        CONTINUE
C     NOTE#1 (GLASS)
          LINES=2
          NOTEA(1)=
     1    '1. MATERIAL: GLASS OPTICAL TYPE   -   CLASS 1, GRADE B,'
          NOTEA(1)=
     1    '   FINE ANNEALED, PER MIL-G-174'
          RETURN
2         CONTINUE
C     NOTE#2
          LINES=1
          NOTEA(1)=
     1    '2. ELEMENT IN ACCORDANCE WITH MIL-0-13830'
          RETURN
3         CONTINUE
C     NOTE#3
          LINES=2
          NOTEA(1)=
     1    '3. SURFACE MARKED "P" POLISHED.  ALL OTHERS GROUND WITH 220'
          NOTEA(2)=
     1    '   GRIT SIZE OR EQUIVALENT'
          RETURN
4         CONTINUE
C     NOTE#4
          LINES=2
          NOTEA(1)=
     1    '4. CLEAR APERTURE : ENTRANCE:'
          NOTEA(2)=
     1    '                       EXITT:'
          RETURN
5         CONTINUE
C     NOTE#5
          LINES=1
          NOTEA(1)=
     1    '5. SURFACE QUALITY:              PER MIL-C-48497'
          RETURN
6         CONTINUE
C     NOTE#6
          LINES=10
          NOTEA(1)=
     1    '6. RADIUS OF CURVATURE OF FINISHED PART SHALL BE WITHIN THE'
          NOTEA(2)=
     1    '   TOLERANCE RANGE SPECIFIED.'
          NOTEA(3)=
     1    '   WHEN USING A TEST GLASS ( @ 0.5461 MICROMETER NOMINAL):'
          NOTEA(4)=
     1    '   A.  TO MEASURE IRREGULARITY, SURFACE OF FINISHED PART SHALL'
          NOTEA(5)=
     1    '       FIT TEST GLASS WITHIN 4 TIMES THE IRREGULARITY CALL OUT.'
          NOTEA(6)=
     1    '   B.  TO CALCULATE SURFACE RADIUS, INCLUDE FRINGE COUNT'
          NOTEA(7)=
     1    '      DEPARTURE FROM MEASURED TEST GLASS RADIUS AND'
          NOTEA(8)=
     1    '      UNCERTAINTY OF THE TEST GLASS RADIUS MEASUREMENT.'
          NOTEA(9)=
     1    '      FOR         R.  R/FRINGE=            AT            DIA.'
          NOTEA(10)=
     1    '                  R.  R/FRINGE=            AT            DIA.'
          RETURN
7         CONTINUE
C     NOTE#7
          LINES=2
          NOTEA(1)=
     1    '7. DATUM   -A-   IS THE LINE CONNECTING THE TWO CENTERS OF'
          NOTEA(2)=
     1    '   CURVATURE'
          RETURN
81        CONTINUE
C     NOTE#8 (NOT GLASS)
          LINES=3
          NOTEA(1)=
     1    '8. CENTERING ERROR:  EDGE THICKNESS DIFFERENCE AT           DIA'
          NOTEA(2)=
     1    '   MINIMUM TO BE LESS THAN '
          NOTEA(3)=
     1    '   (REF. ONLY:                  DIA. TIR)'
          RETURN
82        CONTINUE
C     NOTE#8 (GLASS)
          LINES=3
          NOTEA(1)=
     1    '8. CENTERING ERROR:          ARCMIN. DEVIATION'
          NOTEA(2)=
     1    '   MAX. (REF. ONLY:             DIA. TIR)'
          RETURN
          CONTINUE
C     NOTE#9
          LINES=1
          NOTEA(1)=
     1    '9. BREAK EDGES                FACE WIDTH MAX.'
          RETURN
          CONTINUE
C     NOTE#10
          LINES=1
          NOTEA(1)=
     1    '10. COAT CLEAR APERTURES PER'
          RETURN
1100      CONTINUE
C     NOTE#11 (GLASS)
          LINES=4
          NOTEA(1)=
     1    '11. REF. ONLY: @ 0.5461 MICROMETER, NOMINAL,'
          NOTEA(2)=
     1    '    EFL = '
          NOTEA(3)=
     1    '    BFL = '
          NOTEA(4)=
     1    '    FFL = '
          RETURN
      END


C SUB FRMBOX
      SUBROUTINE FRMBOX
C
          IMPLICIT NONE
C
C       THIS ROUTINE DRAWS THE BOX FRAMING FOR ALL RAY FAN
C       PLOTTING
C
          INTEGER IJKX,IJKY
C
          REAL*8 VAA
C
          CHARACTER CS1*4,CS2*4,CS3*11,CS4*11,CS5*11,CS6*11,CS7*11,CS8*11
     1    ,XTOPLB*11,XN1*28,XN2*28,XN3*28,YN1*28,YN2*28,YN3*28,YFOBN3*18,
     2    YTOPLB*11,XFOBN1*18,XFOBN2*18,XFOBN3*18,YFOBN1*18,YFOBN2*18,
     3    B*140,VALUE1*8
C
          INTEGER COLPAS,NT1ANG,NT1SIZ
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          CS1='+1.0'
          CS2='-1.0'
C
          IF(FANTYP.EQ.1) THEN
              CS3='OPD (Y-FAN)'
              CS4='OPD (Y-FAN)'
          END IF
          IF(FANTYP.EQ.2) THEN
              CS3='OPD (X-FAN)'
              CS4='OPD (X-FAN)'
          END IF
          IF(FANTYP.EQ.3) THEN
              CS3='OPD (N-FAN)'
              CS4='OPD (N-FAN)'
          END IF
          IF(FANTYP.EQ.4) THEN
              CS3='OPD (P-FAN)'
              CS4='OPD (P-FAN)'
          END IF
C
          IF(FANTYP.EQ.1) THEN
              CS5='DX (Y-FAN)'
              CS6='DY (Y-FAN)'
C
              CS7='DXA (Y-FAN)'
              CS8='DYA (Y-FAN)'
          END IF
          IF(FANTYP.EQ.2) THEN
              CS5='DX (X-FAN)'
              CS6='DY (X-FAN)'
C
              CS7='DXA (X-FAN)'
              CS8='DYA (X-FAN)'
          END IF
          IF(FANTYP.EQ.3) THEN
              CS5='DN (N-FAN)'
              CS6='DP (N-FAN)'
C
              CS7='DNA (N-FAN)'
              CS8='DPA (N-FAN)'
          END IF
          IF(FANTYP.EQ.4) THEN
              CS5='DN (P-FAN)'
              CS6='DP (P-FAN)'
C
              CS7='DNA (P-FAN)'
              CS8='DPA (P-FAN)'
          END IF
          IF(FANTYP.EQ.5) THEN
              IF(QALTYP.NE.1) THEN
                  CS5='DX (X-FAN)'
                  CS6='DY (Y-FAN)'
C
                  CS7='DXA (X-FAN)'
                  CS8='DYA (Y-FAN)'
              ELSE
                  CS5='OPD (X-FAN)'
                  CS6='OPD (Y-FAN)'
C
                  CS7='OPD (X-FAN)'
                  CS8='OPD (Y-FAN)'
              END IF
          END IF
          IF(FANTYP.EQ.6) THEN
              IF(QALTYP.NE.1) THEN
                  CS5='DY (X-FAN)'
                  CS6='DX (Y-FAN)'
C
                  CS7='DYA (X-FAN)'
                  CS8='DXA (Y-FAN)'
              ELSE
                  CS5='OPD (X-FAN)'
                  CS6='OPD (Y-FAN)'
C
                  CS7='OPD (X-FAN)'
                  CS8='OPD (Y-FAN)'
              END IF
          END IF
C
          IF(FANNUM.EQ.1) THEN
              XFOB2=0.0D0
              YFOB2=0.0D0
              XFOB3=0.0D0
              YFOB3=0.0D0
          ELSE
          END IF
          IF(FANNUM.EQ.2) THEN
              XFOB3=0.0D0
              YFOB3=0.0D0
          ELSE
          END IF
C
C     DETERMINE FOB AND FIELD OF VIEW NAMES
          WRITE(B,180) REAL(XFOB1)
          READ(B,200) VALUE1
          XFOBN1='X-FOB = '//VALUE1
          WRITE(B,180) REAL(YFOB1)
          READ(B,200) VALUE1
          YFOBN1='Y-FOB = '//VALUE1
C
          WRITE(B,180) REAL(XFOB2)
          READ(B,200) VALUE1
          XFOBN2='X-FOB = '//VALUE1
          WRITE(B,180) REAL(YFOB2)
          READ(B,200) VALUE1
          YFOBN2='Y-FOB = '//VALUE1
C
          WRITE(B,180) REAL(XFOB3)
          READ(B,200) VALUE1
          XFOBN3='X-FOB = '//VALUE1
          WRITE(B,180) REAL(YFOB3)
          READ(B,200) VALUE1
          YFOBN3='Y-FOB = '//VALUE1
C
          IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
C     FIELD ANGLE
              VAA=XFOB1*SYSTEM1(21)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              XN1='X-F.O.V. (DEG) = '//VALUE1
              VAA=YFOB1*SYSTEM1(21)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              YN1='Y-F.O.V. (DEG) = '//VALUE1
C
              VAA=XFOB2*SYSTEM1(21)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              XN2='X-F.O.V. (DEG) = '//VALUE1
              VAA=YFOB2*SYSTEM1(21)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              YN2='Y-F.O.V. (DEG) = '//VALUE1
C
              VAA=XFOB3*SYSTEM1(21)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              XN3='X-F.O.V. (DEG) = '//VALUE1
              VAA=YFOB3*SYSTEM1(21)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              YN3='Y-F.O.V. (DEG) = '//VALUE1
C
          ELSE
C     FIELD HEIGHT
              VAA=XFOB1*SYSTEM1(14)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              IF(SYSTEM1(6).EQ.1.0D0) XN1='X-OBJ. HT. (IN) = '//VALUE1
              IF(SYSTEM1(6).EQ.2.0D0) XN1='X-OBJ. HT. (CM) = '//VALUE1
              IF(SYSTEM1(6).EQ.3.0D0) XN1='X-OBJ. HT. (MM) = '//VALUE1
              IF(SYSTEM1(6).EQ.4.0D0) XN1='X-OBJ. HT. (M)  = '//VALUE1
              VAA=YFOB1*SYSTEM1(14)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              IF(SYSTEM1(6).EQ.1.0D0) YN1='Y-OBJ. HT. (IN) = '//VALUE1
              IF(SYSTEM1(6).EQ.2.0D0) YN1='Y-OBJ. HT. (CM) = '//VALUE1
              IF(SYSTEM1(6).EQ.3.0D0) YN1='Y-OBJ. HT. (MM) = '//VALUE1
              IF(SYSTEM1(6).EQ.4.0D0) YN1='Y-OBJ. HT. (M)  = '//VALUE1
C
              VAA=XFOB2*SYSTEM1(14)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              IF(SYSTEM1(6).EQ.1.0D0) XN2='X-OBJ. HT. (IN) = '//VALUE1
              IF(SYSTEM1(6).EQ.2.0D0) XN2='X-OBJ. HT. (CM) = '//VALUE1
              IF(SYSTEM1(6).EQ.3.0D0) XN2='X-OBJ. HT. (MM) = '//VALUE1
              IF(SYSTEM1(6).EQ.4.0D0) XN2='X-OBJ. HT. (M)  = '//VALUE1
              VAA=YFOB2*SYSTEM1(14)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              IF(SYSTEM1(6).EQ.1.0D0) YN2='Y-OBJ. HT. (IN) = '//VALUE1
              IF(SYSTEM1(6).EQ.2.0D0) YN2='Y-OBJ. HT. (CM) = '//VALUE1
              IF(SYSTEM1(6).EQ.3.0D0) YN2='Y-OBJ. HT. (MM) = '//VALUE1
              IF(SYSTEM1(6).EQ.4.0D0) YN2='Y-OBJ. HT. (M)  = '//VALUE1
C
              VAA=XFOB3*SYSTEM1(14)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              IF(SYSTEM1(6).EQ.1.0D0) XN3='X-OBJ. HT. (IN) = '//VALUE1
              IF(SYSTEM1(6).EQ.2.0D0) XN3='X-OBJ. HT. (CM) = '//VALUE1
              IF(SYSTEM1(6).EQ.3.0D0) XN3='X-OBJ. HT. (MM) = '//VALUE1
              IF(SYSTEM1(6).EQ.4.0D0) XN3='X-OBJ. HT. (M)  = '//VALUE1
              VAA=YFOB3*SYSTEM1(14)
              WRITE(B,180)REAL(VAA)
              READ(B,200) VALUE1
              IF(SYSTEM1(6).EQ.1.0D0) YN3='Y-OBJ. HT. (IN) = '//VALUE1
              IF(SYSTEM1(6).EQ.2.0D0) YN3='Y-OBJ. HT. (CM) = '//VALUE1
              IF(SYSTEM1(6).EQ.3.0D0) YN3='Y-OBJ. HT. (MM) = '//VALUE1
              IF(SYSTEM1(6).EQ.4.0D0) YN3='Y-OBJ. HT. (M)  = '//VALUE1
C
          END IF

C     SET UP LABEL FOR TOP OF ABERRATION AXES
          IF(FANTYP.LE.4.AND.QALTYP.EQ.1) THEN
              XTOPLB=CS3
              YTOPLB=CS4
          ELSE
C     NOT OPD SINGLE FAN
              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C     FOCAL
                  XTOPLB=CS5
                  YTOPLB=CS6
              ELSE
C     AFOCAL
                  XTOPLB=CS7
                  YTOPLB=CS8
              END IF
          END IF
C
C     SET LETTER SIZE AND ANGLE
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
C
C     LIFT PEN, MOVE TO BOX START
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C     DROP PEN, DRAW BOX
          CALL MY_PLOT(10000,0,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(10000,7000,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(0,7000,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(0,0,1,0,-10,10010,-10,10010)
C
C     NOW DRAW BOTTOM OF FAN BOX BELOW WHICH THE CAPTION GOES
          CALL MY_PLOT(0,2000,0,0,-10,10010,-10,10010)
          CALL MY_PLOT(10000,2000,1,0,-10,10010,-10,10010)
C
C     LIFT PEN GO TO X=0, Y=4500 AND DRAW A LINE FROM THERE
C     TO X=10000, Y=4500 UNLESS QALTYP=1(OPD) AND FANTYP <= 4
          IF(FANTYP.LE.4.AND.QALTYP.NE.1.OR.FANTYP.GT.4) THEN
              COLPAS=COLFRM
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(0,4500,0,0,-10,10010,-10,10010)
              CALL MY_PLOT(10000,4500,1,0,-10,10010,-10,10010)
          ELSE
C     OPD SINGLE FAN, DON'T DRAW THIS LINE
          END IF
C
C
C     HORIZONTALS DIVIDING THE FANS AT DIFFERENT FIELDS
C
          IF(FANNUM.EQ.1) THEN
C
              IF(FANTYP.LE.4.AND.QALTYP.NE.1.OR.FANTYP.GT.4) THEN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(40,4520,XN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(40,4620,YN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,4520,XFOBN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,4620,YFOBN1,NT1ANG,NT1SIZ,3)
C
                  CALL MY_JUSTSTRING(40,2020,XN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(40,2120,YN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2020,XFOBN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2120,YFOBN1,NT1ANG,NT1SIZ,3)
              ELSE
C     OPD SINGLE FAN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(40,2020,XN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(40,2120,YN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2020,XFOBN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2120,YFOBN1,NT1ANG,NT1SIZ,3)
              END IF
C
C     NO HORIZONTAL LINE
C     NOW DRAW THE FAN AXES
C     THE Y COORDINATES ARE ALWAYS AT 3250 AND 5750
C     UNLESS IT IS AN OPD PLOT THEN THE Y COORDINATE IS 4500
C     THE Y EXTENT IS A TOTAL OF 2000 UNITS
C     THE X EXTENT IS A TOTAL OF 2500 UNITS
C     THE X CENTER IS AT 5000
C
              IF(FANTYP.LE.4.AND.QALTYP.NE.1.OR.FANTYP.GT.4) THEN
C     LEFT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(5000,4750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(5000,6750,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(3750,5750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(6250,5750,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     LEFT LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=5000-1600
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=5000+1300
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     LEFT FAN
                  IF(XTOPLB(1:2).EQ.'DX'.OR.XTOPLB(1:2).EQ.'DN') THEN
                      IJKX=5000-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DXA'.OR.XTOPLB(1:3).EQ.'DNA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=5000-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:2).EQ.'DY'.OR.XTOPLB(1:2).EQ.'DP') THEN
                      IJKX=5000-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DYA'.OR.XTOPLB(1:3).EQ.'DPA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=5000-450
                      IJKY=6800
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
C
C     RIGHT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(5000,2250,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(5000,4250,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(3750,3250,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(6250,3250,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     RIGHT FAN
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=5000-1600
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=5000+1300
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     RIGHT FAN
                  IF(YTOPLB(1:2).EQ.'DY'.OR.YTOPLB(1:2).EQ.'DP') THEN
                      IJKX=5000-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:2).EQ.'DX'.OR.YTOPLB(1:2).EQ.'DN') THEN
                      IJKX=5000-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DYA'.OR.YTOPLB(1:3).EQ.'DPA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=5000-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DXA'.OR.YTOPLB(1:3).EQ.'DNA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=5000-450
                      IJKY=4300
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,YTOPLB,NT1ANG,NT1SIZ,3)
              ELSE
C     OPD
C     OPD FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(5000,3500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(5000,5500,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(3750,4500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(6250,4500,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     OPD LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=5000-1600
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=5000+1300
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     OPD FAN
                  IJKX=5000-450
                  IJKY=5550
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
              END IF
C
C     RETURN TO ORIGIN
              CALL MY_PLOT(10,10,0,0,-10,10010,-10,7010)
          ELSE
          END IF
          IF(FANNUM.EQ.2) THEN
C
              IF(FANTYP.LE.4.AND.QALTYP.NE.1.OR.FANTYP.GT.4) THEN
C     BOTTOM FANS
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(40,4520,XN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(40,4620,YN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,4520,XFOBN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,4620,YFOBN1,NT1ANG,NT1SIZ,3)
C
                  CALL MY_JUSTSTRING(40,2020,XN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(40,2120,YN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2020,XFOBN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2120,YFOBN1,NT1ANG,NT1SIZ,3)
C
C     TOP FANS
                  CALL MY_JUSTSTRING(5040,4520,XN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(5040,4620,YN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(6740,4520,XFOBN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(6740,4620,YFOBN2,NT1ANG,NT1SIZ,3)
C
                  CALL MY_JUSTSTRING(5040,2020,XN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(5040,2120,YN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(6740,2020,XFOBN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(6740,2020,YFOBN2,NT1ANG,NT1SIZ,3)
              ELSE
C     OPD
C     BOTTOM FANS
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETCHARASPECT(1.0,1.0)
                  CALL MY_JUSTSTRING(40,2020,XN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(40,2120,YN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2020,XFOBN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2120,YFOBN1,NT1ANG,NT1SIZ,3)
C
C     TOP FANS
                  CALL MY_JUSTSTRING(5040,2020,XN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(5040,2120,YN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(6740,2020,XFOBN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(6740,2120,YFOBN2,NT1ANG,NT1SIZ,3)
              END IF
C
C     ONE HORIZONTAL LINE BETWEEN 0 AND 10000
              COLPAS=COLFRM
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(5000,2000,0,0,-10,10010,-10,10010)
              CALL MY_PLOT(5000,7000,1,0,-10,10010,-10,10010)
C     NOW DRAW THE FAN AXES
C     THE Y COORDINATES ARE ALWAYS AT 1750 AND 5250
C     THE Y EXTENT IS A TOTAL OF 2000 UNITS
C     THE X EXTENT IS A TOTAL OF 2500 UNITS
C     THE X CENTERS ARE AT 3330 AND 6660
C
              IF(FANTYP.LE.4.AND.QALTYP.NE.1.OR.FANTYP.GT.4) THEN
C     TOP FAN (X-CENTER AT X=6666)
C
C     LEFT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(6666,4750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(6666,6750,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(5416,4750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(7916,4750,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     LEFT LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=6666-1600
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=6666+1300
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     LEFT FAN
                  IF(XTOPLB(1:2).EQ.'DX'.OR.XTOPLB(1:2).EQ.'DN') THEN
                      IJKX=6666-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DXA'.OR.XTOPLB(1:3).EQ.'DNA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=6666-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:2).EQ.'DY'.OR.XTOPLB(1:2).EQ.'DP') THEN
                      IJKX=6666-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DYA'.OR.XTOPLB(1:3).EQ.'DPA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=6666-450
                      IJKY=6800
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
C
C     RIGHT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(6666,2250,0,0,-10,10010,-10,7010)
                  CALL MY_PLOT(6666,4250,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(5416,3250,0,0,-10,10010,-10,7010)
                  CALL MY_PLOT(7916,3250,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     RIGHT FAN
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=6666-1600
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=6666+1300
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     RIGHT FAN
                  IF(YTOPLB(1:2).EQ.'DY'.OR.YTOPLB(1:2).EQ.'DP') THEN
                      IJKX=6666-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:2).EQ.'DX'.OR.YTOPLB(1:2).EQ.'DN') THEN
                      IJKX=6666-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DYA'.OR.YTOPLB(1:3).EQ.'DPA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=6666-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DXA'.OR.YTOPLB(1:3).EQ.'DNA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=6666-450
                      IJKY=4300
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,YTOPLB,NT1ANG,NT1SIZ,3)
C
C     BOTTOM FAN (X-CENTER AT X=3990)
C
C     LEFT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(3333,4750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(3333,6750,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(2083,5750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(4583,5750,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     LEFT LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=3333-1600
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=3333+1300
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     LEFT FAN
                  IF(XTOPLB(1:2).EQ.'DX'.OR.XTOPLB(1:2).EQ.'DN') THEN
                      IJKX=3333-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DXA'.OR.XTOPLB(1:3).EQ.'DNA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=3333-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:2).EQ.'DY'.OR.XTOPLB(1:2).EQ.'DP') THEN
                      IJKX=3333-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DYA'.OR.XTOPLB(1:3).EQ.'DPA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=3333-450
                      IJKY=6800
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
C
C     RIGHT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(3333,2250,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(3333,4250,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(2083,3250,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(4583,3250,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     RIGHT FAN
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=3333-1600
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=3333+1300
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     RIGHT FAN
                  IF(YTOPLB(1:2).EQ.'DY'.OR.YTOPLB(1:2).EQ.'DP') THEN
                      IJKX=3333-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:2).EQ.'DX'.OR.YTOPLB(1:2).EQ.'DN') THEN
                      IJKX=3333-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DYA'.OR.YTOPLB(1:3).EQ.'DPA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=3333-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DXA'.OR.YTOPLB(1:3).EQ.'DNA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=3333-450
                      IJKY=4300
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,YTOPLB,NT1ANG,NT1SIZ,3)
              ELSE
C     OPD
C     TOP FAN (X-CENTER AT X=6666)
C
C     OPD FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(6666,3500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(6666,5500,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(4516,4500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(7916,4500,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     OPD LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=6666-1600
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=6666+1300
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     OPD FAN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=6666-450
                  IJKY=5550
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
C
C     BOTTOM FAN (X-CENTER AT X=3990)
C
C     OPD FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(3333,3500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(3333,5500,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(2083,4500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(4583,4500,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     OPD LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=6666-1600
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=6666+1300
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     LEFT FAN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=3333-450
                  IJKY=5550
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
              END IF
C
C
C     RETURN TO ORIGIN
              CALL MY_PLOT(10,10,0,0,-10,10010,-10,7010)
          ELSE
          END IF
          IF(FANNUM.EQ.3) THEN
              IF(FANTYP.LE.4.AND.QALTYP.NE.1.OR.FANTYP.GT.4) THEN
                  CALL MY_SETCHARASPECT(1.0,1.0)
C     BOTTOM FANS
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(40,4520,XN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(40,4620,YN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,4520,XFOBN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,4620,YFOBN1,NT1ANG,NT1SIZ,3)
C
                  CALL MY_JUSTSTRING(40,2020,XN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(40,2120,YN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2020,XFOBN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2120,YFOBN1,NT1ANG,NT1SIZ,3)
C
C     MIDDLE FANS
                  CALL MY_JUSTSTRING(3373,4520,XN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(3373,4620,YN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(5073,4520,XFOBN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(5073,4620,YFOBN2,NT1ANG,NT1SIZ,3)
C
                  CALL MY_JUSTSTRING(3373,2020,XN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(3373,2120,YN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(5073,2020,XFOBN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(5073,2120,YFOBN2,NT1ANG,NT1SIZ,3)
C
C     TOP FANS
                  CALL MY_JUSTSTRING(6706,4520,XN3,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(6706,4620,YN3,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(8406,4520,XFOBN3,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(8406,4620,YFOBN3,NT1ANG,NT1SIZ,3)
C
                  CALL MY_JUSTSTRING(6706,2020,XN3,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(6706,2120,YN3,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(8406,2020,XFOBN3,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(8406,2120,YFOBN3,NT1ANG,NT1SIZ,3)

              ELSE
C     OPD
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_SETCHARASPECT(1.0,1.0)
C     BOTTOM FANS
                  CALL MY_JUSTSTRING(40,2020,XN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(40,2120,YN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2020,XFOBN1,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(1740,2120,YFOBN1,NT1ANG,NT1SIZ,3)
C
C     MIDDLE FANS
                  CALL MY_JUSTSTRING(3373,2020,XN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(3373,2120,YN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(5040,2020,XFOBN2,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(5040,2120,YFOBN2,NT1ANG,NT1SIZ,3)
C
C     TOP FANS
                  CALL MY_JUSTSTRING(6706,2020,XN3,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(6706,2120,YN3,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(8406,2020,XFOBN3,NT1ANG,NT1SIZ,3)
                  CALL MY_JUSTSTRING(8406,2120,YFOBN3,NT1ANG,NT1SIZ,3)
              END IF
C
C     TWO HORIZONTAL LINES BETWEEN 0 AND 10000
              COLPAS=COLFRM
              CALL MY_COLTYP(COLPAS)
              CALL MY_PLOT(3333,2000,0,0,-10,10010,-10,10010)
              CALL MY_PLOT(3333,7000,1,0,-10,10010,-10,10010)
              CALL MY_PLOT(6666,2000,0,0,-10,10010,-10,10010)
              CALL MY_PLOT(6666,7000,1,0,-10,10010,-10,10010)
C     NOW DRAW THE FAN AXES
C     THE Y COORDINATES ARE ALWAYS AT 3250 AND 5750
C     THE Y EXTENT IS A TOTAL OF 2000 UNITS
C     THE X EXTENT IS A TOTAL OF 2500 UNITS
C     THE X CENTERS ARE AT 1666,5000,8332
C
              IF(FANTYP.LE.4.AND.QALTYP.NE.1.OR.FANTYP.GT.4) THEN
C     TOP FAN (X-CENTER AT X=8332)
C
C     LEFT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(8332,4750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(8332,6750,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(7082,5750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(9582,5750,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     LEFT LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=8332-1600
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=8332+1300
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     LEFT FAN
                  IF(XTOPLB(1:2).EQ.'DX'.OR.XTOPLB(1:2).EQ.'DN') THEN
                      IJKX=8332-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DXA'.OR.XTOPLB(1:3).EQ.'DNA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=8332-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:2).EQ.'DY'.OR.XTOPLB(1:2).EQ.'DP') THEN
                      IJKX=8332-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DYA'.OR.XTOPLB(1:3).EQ.'DPA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=8332-450
                      IJKY=6800
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
C
C     RIGHT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(8332,2250,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(8332,4250,1,0,-10,10010,-10,10010)
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(7082,3250,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(9582,3250,1,0,-10,10010,-10,10010)
                  CALL MY_COLTYP(COLPAS)
C     DRAW Y-AXES
C     NOW LABEL THE RELATIVE APT. AXES
C     RIGHT FAN
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=8332-1600
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=8332+1300
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     RIGHT FAN
                  IF(YTOPLB(1:2).EQ.'DY'.OR.YTOPLB(1:2).EQ.'DP') THEN
                      IJKX=8332-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:2).EQ.'DX'.OR.YTOPLB(1:2).EQ.'DN') THEN
                      IJKX=8332-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DYA'.OR.YTOPLB(1:3).EQ.'DPA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=8332-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DXA'.OR.YTOPLB(1:3).EQ.'DNA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=8332-450
                      IJKY=4300
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,YTOPLB,NT1ANG,NT1SIZ,3)
C
C     MIDDLE FAN (X-CENTER AT X=5000)
C
C     LEFT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(5000,4750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(5000,6750,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(3750,5750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(6250,5750,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     LEFT LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=5000-1600
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=5000+1300
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     LEFT FAN
                  IF(XTOPLB(1:2).EQ.'DX'.OR.XTOPLB(1:2).EQ.'DN') THEN
                      IJKX=5000-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DXA'.OR.XTOPLB(1:3).EQ.'DNA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=5000-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:2).EQ.'DY'.OR.XTOPLB(1:2).EQ.'DP') THEN
                      IJKX=5000-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DYA'.OR.XTOPLB(1:3).EQ.'DPA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=5000-450
                      IJKY=6800
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
C
C     RIGHT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(5000,2250,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(5000,4250,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(3750,3250,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(6250,3250,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     RIGHT FAN
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=5000-1600
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=5000+1300
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     RIGHT FAN
                  IF(YTOPLB(1:2).EQ.'DY'.OR.YTOPLB(1:2).EQ.'DP') THEN
                      IJKX=5000-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:2).EQ.'DX'.OR.YTOPLB(1:2).EQ.'DN') THEN
                      IJKX=5000-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DYA'.OR.YTOPLB(1:3).EQ.'DPA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=5000-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DXA'.OR.YTOPLB(1:3).EQ.'DNA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=5000-450
                      IJKY=4300
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,YTOPLB,NT1ANG,NT1SIZ,3)
C
C     BOTTOM FAN (X-CENTER AT X=1666)
C
C     LEFT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(1666,4750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(1666,6750,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(416,5750,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(2916,5750,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     LEFT LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=1666-1600
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=1666+1300
                  IJKY=5700
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     LEFT FAN
                  IF(XTOPLB(1:2).EQ.'DX'.OR.XTOPLB(1:2).EQ.'DN') THEN
                      IJKX=1666-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DXA'.OR.XTOPLB(1:3).EQ.'DNA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=1666-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:2).EQ.'DY'.OR.XTOPLB(1:2).EQ.'DP') THEN
                      IJKX=1666-450
                      IJKY=6800
                  ELSE
                  END IF
                  IF(XTOPLB(1:3).EQ.'DYA'.OR.XTOPLB(1:3).EQ.'DPA'.OR.
     1            XTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=1666-450
                      IJKY=6800
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
C
C     RIGHT FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(1666,2250,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(1666,4250,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(416,3250,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(2916,3250,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     RIGHT FAN
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=1666-1600
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=1666+1300
                  IJKY=3200
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     RIGHT FAN
                  IF(YTOPLB(1:2).EQ.'DY'.OR.YTOPLB(1:2).EQ.'DP') THEN
                      IJKX=1666-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:2).EQ.'DX'.OR.YTOPLB(1:2).EQ.'DN') THEN
                      IJKX=1666-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DYA'.OR.YTOPLB(1:3).EQ.'DPA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=1666-450
                      IJKY=4300
                  ELSE
                  END IF
                  IF(YTOPLB(1:3).EQ.'DXA'.OR.YTOPLB(1:3).EQ.'DNA'.OR.
     1            YTOPLB(1:3).EQ.'OPD') THEN
                      IJKX=1666-450
                      IJKY=4300
                  ELSE
                  END IF
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_JUSTSTRING(IJKX,IJKY,YTOPLB,NT1ANG,NT1SIZ,3)
              ELSE
C     OPD
C     TOP FAN (X-CENTER AT X=8657)
C
C     OPD FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(8332,3500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(8332,5500,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(7082,4500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(9582,4500,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     OPD LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=8332-1600
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=8332+1300
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     OPD FAN
                  IJKX=8332-450
                  IJKY=5550
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
C
C     MIDDLE FAN (X-CENTER AT X=5000)
C
C     OPD FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(5000,3500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(5000,5500,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(3750,4500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(6250,4500,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     LEFT LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=5000-1600
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=5000+1300
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     OPD FAN
                  IJKX=5000-450
                  IJKY=5550
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
C
C     BOTTOM FAN (X-CENTER AT X=1666)
C
C     OPD FAN AXES
C     DRAW X-AXES
                  COLPAS=COLFRM
                  CALL MY_COLTYP(COLPAS)
                  CALL MY_PLOT(1666,3500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(1666,5500,1,0,-10,10010,-10,10010)
C     DRAW Y-AXES
                  CALL MY_PLOT(416,4500,0,0,-10,10010,-10,10010)
                  CALL MY_PLOT(2916,4500,1,0,-10,10010,-10,10010)
C     NOW LABEL THE RELATIVE APT. AXES
C     OPD LABEL
C     NEGATIVE SIDE 400 UNITS LEFT AND 40 DOWN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
                  IJKX=1666-1600
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS2,NT1ANG,NT1SIZ,3)
C     POSITIVE SIDE 50 UNITS RIGHT AND 40 DOWN
                  IJKX=1666+1300
                  IJKY=4450
                  CALL MY_JUSTSTRING(IJKX,IJKY,CS1,NT1ANG,NT1SIZ,3)
C
C     NOW TOP OF AXES
C     OPD FAN
                  IJKX=1666-450
                  IJKY=5550
                  CALL MY_JUSTSTRING(IJKX,IJKY,XTOPLB,NT1ANG,NT1SIZ,3)
              END IF
C
C     RETURN TO ORIGIN
              CALL MY_PLOT(10,10,0,0,-10,10010,-10,7010)
          ELSE
          END IF
C     LIFT PEN, RETURN TO 0,0
          CALL MY_PLOT(0,0,0,0,-10,10010,-10,7010)
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
180       FORMAT(G8.2)
200       FORMAT(A8)
          RETURN
      END


C SUB FILCOB.FOR
      SUBROUTINE FILCOB
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES COBS PATTERN FILLING
C
          INTEGER IK,IX(0:360),IY(0:360)
C
!      REAL XWORLD,YWORLD
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          DO IK=0,360
C     FILL THE POLYGON WITH A CROSS HASHED PATTERN
              IX(IK)=(P1ARAY(IK,1,1))
              IY(IK)=(P1ARAY(IK,2,1))
          END DO
C     NOW DRAW THE FILL LINES
          CALL PENMV1(IX(1),IY(1),0)
          DO IK=1,180
              CALL PENMV1(IX(IK),IY(IK),1)
              CALL PENMV1(IX(360-IK),IY(360-IK),1)
          END DO
          CALL PENMV1(IX(90),IY(90),0)
          DO IK=90,0,-1
              CALL PENMV1(IX(IK),IY(IK),1)
              CALL PENMV1(IX(180-IK),IY(180-IK),1)
          END DO
          CALL PENMV1(IX(270),IY(270),0)
          DO IK=270,360
              CALL PENMV1(IX(IK),IY(IK),1)
              CALL PENMV1(IX(540-IK),IY(540-IK),1)
          END DO
          RETURN
      END


C SUB FANTP.FOR
      SUBROUTINE FANTP
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF FAN TYPE AND UNITS
C     FOR FAN PLOTTING
C
          CHARACTER UNN*41,NNTT3*45,V1*3,V2*3,V3*3
     1    ,VV1*38,B*140
C
          INTEGER COLPAS,NUNN,NNT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          CALL MY_SETCHARASPECT(1.5,1.5)
C
          IF(FANTYP.EQ.1.AND.QALTYP.EQ.0)
     1    NNTT3='YZ-PLANE TRANSVERSE ABERRATIONS'
          IF(FANTYP.EQ.1.AND.QALTYP.EQ.1)
     1    NNTT3='YZ-PLANE OPTICAL PATH DIFFERENCES'
          IF(FANTYP.EQ.1.AND.QALTYP.EQ.2)
     1    NNTT3='YZ-PLANE CHROMATIC DIFFERENCES'
          IF(FANTYP.EQ.1.AND.QALTYP.EQ.3)
     1    NNTT3='YZ-PLANE LONGITUDINAL ABERRATIONS'
C
          IF(FANTYP.EQ.2.AND.QALTYP.EQ.0)
     1    NNTT3='XZ-PLANE TRANSVERSE ABERRATIONS'
          IF(FANTYP.EQ.2.AND.QALTYP.EQ.1)
     1    NNTT3='XZ-PLANE OPTICAL PATH DIFFERENCES'
          IF(FANTYP.EQ.2.AND.QALTYP.EQ.2)
     1    NNTT3='XZ-PLANE CHROMATIC DIFFERENCES'
          IF(FANTYP.EQ.2.AND.QALTYP.EQ.3)
     1    NNTT3='XZ-PLANE LONGITUDINAL ABERRATIONS'
C
          IF(FANTYP.EQ.3.AND.QALTYP.EQ.0)
     1    NNTT3='+45 DEG. TRANSVERSE ABERRATIONS'
          IF(FANTYP.EQ.3.AND.QALTYP.EQ.1)
     1    NNTT3='+45 DEG. OPTICAL PATH DIFFERENCES'
          IF(FANTYP.EQ.3.AND.QALTYP.EQ.2)
     1    NNTT3='+45 DEG. CHROMATIC DIFFERENCES'
          IF(FANTYP.EQ.3.AND.QALTYP.EQ.3)
     1    NNTT3='+45 DEG. LONGITUDINAL ABERRATIONS'
C
          IF(FANTYP.EQ.4.AND.QALTYP.EQ.0)
     1    NNTT3='-45 DEG. TRANSVERSE ABERRATIONS'
          IF(FANTYP.EQ.4.AND.QALTYP.EQ.1)
     1    NNTT3='-45 DEG. OPTICAL PATH DIFFERENCES'
          IF(FANTYP.EQ.4.AND.QALTYP.EQ.2)
     1    NNTT3='-45 DEG. CHROMATIC DIFFERENCES'
          IF(FANTYP.EQ.4.AND.QALTYP.EQ.3)
     1    NNTT3='-45 DEG. LONGITUDINAL ABERRATIONS'
C
          IF(FANTYP.EQ.5.AND.QALTYP.EQ.0)
     1    NNTT3='XZ/YZ-PLANE TRANSVERSE ABERRATIONS'
          IF(FANTYP.EQ.5.AND.QALTYP.EQ.1)
     1    NNTT3='XZ/YZ-PLANE OPTICAL PATH DIFFERENCES'
          IF(FANTYP.EQ.5.AND.QALTYP.EQ.2)
     1    NNTT3='XZ/YZ-PLANE CHROMATIC DIFFERENCES'
          IF(FANTYP.EQ.5.AND.QALTYP.EQ.3)
     1    NNTT3='XZ/YZ-PLANE LONGITUDINAL ABERRATIONS'
C
          IF(FANTYP.EQ.6.AND.QALTYP.EQ.0)
     1    NNTT3='XZ/YZ-PLANE TRANSVERSE ABERRATIONS'
          IF(FANTYP.EQ.6.AND.QALTYP.EQ.1)
     1    NNTT3='XZ/YZ-PLANE OPTICAL PATH DIFFERENCES'
          IF(FANTYP.EQ.6.AND.QALTYP.EQ.2)
     1    NNTT3='XZ/YZ-PLANE CHROMATIC DIFFERENCES'
          IF(FANTYP.EQ.6.AND.QALTYP.EQ.3)
     1    NNTT3='XZ/YZ-PLANE LONGITUDINAL ABERRATIONS'
C
          IF(FANTYP.EQ.1.AND.QALTYP.EQ.0)
     1    NNT=31
          IF(FANTYP.EQ.1.AND.QALTYP.EQ.1)
     1    NNT=33
          IF(FANTYP.EQ.1.AND.QALTYP.EQ.2)
     1    NNT=30
          IF(FANTYP.EQ.1.AND.QALTYP.EQ.3)
     1    NNT=33
C
          IF(FANTYP.EQ.2.AND.QALTYP.EQ.0)
     1    NNT=31
          IF(FANTYP.EQ.2.AND.QALTYP.EQ.1)
     1    NNT=33
          IF(FANTYP.EQ.2.AND.QALTYP.EQ.2)
     1    NNT=30
          IF(FANTYP.EQ.2.AND.QALTYP.EQ.3)
     1    NNT=33
C
          IF(FANTYP.EQ.3.AND.QALTYP.EQ.0)
     1    NNT=31
          IF(FANTYP.EQ.3.AND.QALTYP.EQ.1)
     1    NNT=33
          IF(FANTYP.EQ.3.AND.QALTYP.EQ.2)
     1    NNT=30
          IF(FANTYP.EQ.3.AND.QALTYP.EQ.3)
     1    NNT=33
C
          IF(FANTYP.EQ.4.AND.QALTYP.EQ.0)
     1    NNT=31
          IF(FANTYP.EQ.4.AND.QALTYP.EQ.1)
     1    NNT=33
          IF(FANTYP.EQ.4.AND.QALTYP.EQ.2)
     1    NNT=30
          IF(FANTYP.EQ.4.AND.QALTYP.EQ.3)
     1    NNT=33
C
          IF(FANTYP.EQ.5.AND.QALTYP.EQ.0)
     1    NNT=34
          IF(FANTYP.EQ.5.AND.QALTYP.EQ.1)
     1    NNT=36
          IF(FANTYP.EQ.5.AND.QALTYP.EQ.2)
     1    NNT=33
          IF(FANTYP.EQ.5.AND.QALTYP.EQ.3)
     1    NNT=36
C
          IF(FANTYP.EQ.6.AND.QALTYP.EQ.0)
     1    NNT=34
          IF(FANTYP.EQ.6.AND.QALTYP.EQ.1)
     1    NNT=36
          IF(FANTYP.EQ.6.AND.QALTYP.EQ.2)
     1    NNT=33
          IF(FANTYP.EQ.6.AND.QALTYP.EQ.3)
     1    NNT=36
C
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C     FOCAL OR UFOCAL
              IF(SYSTEM1(6).EQ.1.0D0) UNN='UNITS = in(s)'
              IF(SYSTEM1(6).EQ.2.0D0) UNN='UNITS = cm(s)'
              IF(SYSTEM1(6).EQ.3.0D0) UNN='UNITS = mm(s)'
              IF(SYSTEM1(6).EQ.4.0D0) UNN='UNITS = meter(s)'
              IF(SYSTEM1(6).EQ.1.0D0) NUNN=13
              IF(SYSTEM1(6).EQ.2.0D0) NUNN=13
              IF(SYSTEM1(6).EQ.3.0D0) NUNN=13
              IF(SYSTEM1(6).EQ.4.0D0) NUNN=16
          ELSE
C     NOT FOCAL
          END IF
          IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
C     AFOCAL OR UAFOCAL
              UNN='UNITS = radian(s)'
              NUNN=17
          ELSE
C     NOT AFOCAL
          END IF
C     IF OPD RESET ABERRATION UNITS
          IF(QALTYP.EQ.1)
     1    UNN='UNITS = WAVES AT REFERENCE WAVELENGTH'
          IF(QALTYP.EQ.1)
     1    NUNN=37
C     NOW PLOT NNTT3 ABOVE UNN JUST TO THE RIGHT OF THE WAVELENGTH
C     LEGEND
C
          WRITE(B,181) NEWOBJ
          READ(B,201) V1
          WRITE(B,181) NEWREF
          READ(B,201) V2
          WRITE(B,181) NEWIMG
          READ(B,201) V3
181       FORMAT(I3)
201       FORMAT(A3)
C
          VV1=V1//'=OBJ SURF '//V2//'=REF SURF '//V3//'=IMG SURF'

C
C     DO THE PLOTTING OF THE PLOT TYPE
C
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(4100,1175,NNTT3(1:NNT),0,1,3)
          CALL MY_JUSTSTRING(4100,1000,UNN(1:NUNN),0,1,3)
          CALL MY_JUSTSTRING(4100,850,VV1(1:37),0,1,3)
C
C     NOW BOX THE LEGENDS OFF
C
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
C     LIFT PEN, MOVE TO TOP
          CALL MY_PLOT(3000,1325,0,0,-10,10010,-10,70010)
C     DROP PEN, DRAW LINE
          CALL MY_PLOT(10000,1325,1,0,-10,10010,-10,70010)
          CALL MY_PLOT(3000,700,0,0,-10,10010,-10,70010)
          CALL MY_PLOT(10000,700,1,0,-10,10010,-10,70010)
C     LIFT PEN, RETURN TO 0,0
          CALL MY_PLOT(0,0,0,0,-10,10010,-10,70010)
C
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
          RETURN
      END


C SUB FANRWV.FOR
      SUBROUTINE FANRWV
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE REFERENCE WAVELENGTH LEGEND
C     FOR FAN PLOTTING
C
          CHARACTER NNTT1*8,NNTT2*16,VALUE*8,NNTT3*10,VALUE1*10,NNTT4*22
     1    ,NNTT11*10,NNTT12*8,NNTT10*16,B*140
C
          INTEGER COLPAS,NT1ANG,NT1SIZ
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
C     SET LETTER SIZE AND ANGLE
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
C
C     DO THE PLOTTING OF THE LEGEND FOR THE REFERENCE WAVELENGTH
          NNTT4='REFERENCE WAVELENGTH'
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(3200,550,NNTT4(1:20),NT1ANG,NT1SIZ,3)
C
C     DO THE PLOTTING OF THE LEGEND FOR THE REFERENCE WAVELENGTH
          IF(SYSTEM1(REFWV).GT.99.9D0) THEN
              IF(REFWV.GE.1.AND.REFWV.LE.5) THEN
                  WRITE(B,180)REAL(SYSTEM1(REFWV))
              END IF
              IF(REFWV.GE.6.AND.REFWV.LE.10) THEN
                  WRITE(B,180)REAL(SYSTEM1(65+REFWV))
              END IF
              READ(B,200) VALUE1
180           FORMAT(G10.4)
200           FORMAT(A10)
              NNTT3=VALUE1
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(3200,350,NNTT3(1:10),NT1ANG,NT1SIZ,3)
          ELSE
              IF(REFWV.GE.1.AND.REFWV.LE.5) THEN
                  WRITE(B,181)REAL(SYSTEM1(REFWV))
              END IF
              IF(REFWV.GE.6.AND.REFWV.LE.10) THEN
                  WRITE(B,181)REAL(SYSTEM1(65+REFWV))
              END IF
              READ(B,201) VALUE
181           FORMAT(F8.5)
201           FORMAT(A8)
              NNTT1=VALUE
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(3200,350,NNTT1(1:8),NT1ANG,NT1SIZ,3)
          END IF
          NNTT2='MICROMETER'
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(4200,350,NNTT2,NT1ANG,NT1SIZ,3)
C
          NNTT10='REL. APT. OFFSET'
          CALL MY_JUSTSTRING(5350,550,NNTT10(1:16),NT1ANG,NT1SIZ,3)
C
          IF(FANOFF.GT.99.9D0) THEN
              WRITE(B,180)REAL(FANOFF)
              READ(B,200) VALUE1
              NNTT11=VALUE1
              CALL MY_JUSTSTRING(5350,350,NNTT11(1:10),NT1ANG,NT1SIZ,3)
          ELSE
              WRITE(B,181)REAL(FANOFF)
              READ(B,201) VALUE
              NNTT12=VALUE
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(5350,350,NNTT12(1:8),NT1ANG,NT1SIZ,3)
          END IF
C
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
C     NOW BOX THE LEGENDS OFF
C
C     LIFT PEN, MOVE TO TOP
          CALL MY_PLOT(3000,700,0,0,-10,10010,-10,10010)
C     DROP PEN, DRAW LINE
          CALL MY_PLOT(5200,700,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(5200,250,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(3000,700,0,0,-10,10010,-10,10010)
C     DROP PEN, DRAW LINE
          CALL MY_PLOT(10000,700,1,0,-10,10010,-10,10010)
C     LIFT PEN, RETURN TO 0,0
          CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
          RETURN
      END

C SUB FANLI.FOR
      SUBROUTINE FANLI
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOTTING OF THE LENS IDENTIFIER "LI"
C     FOR FAN PLOTTING
C
          CHARACTER DTY*10,TMY*8,BL20*20,BLNOTE*80,NNTT1*99
C
          INTEGER COLPAS
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
C
C     MOVE TO THE STARTING POSITION FOR THE LI
C
          CALL MY_PLOT(50,100,0,0,-10,10010,-10,10010)

C     DO THE PLOTTING OF THE LENS IDENTIFIER
          IF(STMPT) CALL MYTIME(TMY)
          IF(STMPD) CALL MYDATE(DTY)
          IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
          IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
          IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
          IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_SETCHARASPECT(1.5,1.5)
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
              CALL MY_JUSTSTRING(50,100,NNTT1(1:80),0,1,3)

              COLPAS=COLFRM
              CALL MY_COLTYP(COLPAS)
C     LIFT PEN, MOVE TO TOP
              CALL MY_PLOT(0,250,0,0,-10,10010,-10,10010)
C     DROP PEN, DRAW LINE
              CALL MY_PLOT(10000,250,1,0,-10,10010,-10,10010)
C     LIFT PEN, RETURN TO 0,0
              CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
              IF(.NOT.PLEXIS) PLEXIS=.TRUE.
          ELSE
C     LI BLANK, NOT ACTION
          END IF
C
          RETURN
      END


C SUB FANFOV.FOR
      SUBROUTINE FANFOV
C
C       THIS IS SUBROUTINE FANFOV. THIS IS THE SUBROUTINE WHICH
C       HANDLES CMD LEVEL COMMAND "FANFIELD"
C
          IMPLICIT NONE
C
          REAL*8 F1X,F1Y,F2X,F2Y,F3X,F3Y
C
          COMMON/FANFOB/F1X,F1Y,F2X,F2Y,F3X,F3Y
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1.OR.SQ.EQ.0) THEN
              OUTLYNE=
     1        '"FANFIELD" SETS UP FIELDS FOR AUTOMATED FAN PLOTS'
              CALL SHOWIT(1)
              OUTLYNE='CURRENT FIELD SETTINGS ARE:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'F1 (y-fob,xfob) = ',F1Y,' , ',F1X
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'F2 (y-fob,xfob) = ',F2Y,' , ',F2X
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'F3 (y-fob,xfob) = ',F3Y,' , ',F3X
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.AND.SN.EQ.0) THEN
              IF(WQ.EQ.'F1') OUTLYNE=
     1        'CURRENT "F1" FIELD SETTINGS ARE:'
              IF(WQ.EQ.'F1') CALL SHOWIT(1)
              IF(WQ.EQ.'F1')
     1        WRITE(OUTLYNE,*)'F1 (y-fob,xfob) = ',F1Y,' , ',F1X
              IF(WQ.EQ.'F1') CALL SHOWIT(1)

              IF(WQ.EQ.'F2') OUTLYNE=
     1        'CURRENT "F2" FIELD SETTINGS ARE:'
              IF(WQ.EQ.'F2') CALL SHOWIT(1)
              IF(WQ.EQ.'F2')
     1        WRITE(OUTLYNE,*)'F2 (y-fob,xfob) = ',F2Y,' , ',F2X
              IF(WQ.EQ.'F2') CALL SHOWIT(1)

              IF(WQ.EQ.'F3') OUTLYNE=
     1        'CURRENT "F3" FIELD SETTINGS ARE:'
              IF(WQ.EQ.'F3') CALL SHOWIT(1)
              IF(WQ.EQ.'F3')
     1        WRITE(OUTLYNE,*)'F3 (y-fob,xfob) = ',F3Y,' , ',F3X
              IF(WQ.EQ.'F3') CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE='"FANFIELD" TAKES NO ALPHANUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"FANFIELD" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     IF(W1 OR W2 ARE DEFAULT, DON'T CHANGE THEM
          IF(WQ.EQ.'F1') THEN
              IF(DF1.EQ.0) F1Y=W1
              IF(DF2.EQ.0) F1X=W2
          END IF
          IF(WQ.EQ.'F2') THEN
              IF(DF1.EQ.0) F2Y=W1
              IF(DF2.EQ.0) F2X=W2
          END IF
          IF(WQ.EQ.'F3') THEN
              IF(DF1.EQ.0) F3Y=W1
              IF(DF2.EQ.0) F3X=W2
          END IF
          RETURN
C       ALL DONE
      END


C SUB FANDO1.FOR
      SUBROUTINE FANDO1(JCOMP,KCOMP)
C
          IMPLICIT NONE
C
C     DOES FAN DATA PLOTTING FOR USER-DEFINED FAN PLOTTING
C
          INTEGER I,JJJ,IK,IIK,JJJJ,IXVALUE,IYVALUE,JCOMP,KCOMP
     1    ,COLPAS,ITES,XA1,XB1,YA1,YB1,ENDER,XVAL
C
          INTEGER JIMX(1:102),JIMY(1:102),JIMFLG(1:102)
C
          REAL*8 YMAX,RANGE1,YMIN,STEPJP1
     1    ,XP1,XP2
     2    ,YVAL,DRAXX,FACTOR,XPOS,XXPOS,XXPOS1
C
          REAL*8 XY2(1:102)

          LOGICAL SPLFIT,FANEXT
C
          COMMON/FANEXI/FANEXT
C
          INTEGER XTENT,YTENT,CLIP
C
          COMMON/USEFAN/XTENT,YTENT,CLIP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'

          COLPAS=JCOMP
          CALL MY_COLTYP(COLPAS)
          IF(KCOMP.LT.0.OR.KCOMP.GT.9) KCOMP=0
          LNTYPE=KCOMP
          STEPJP1=DBLE(XTENT)/DBLE(MAXFAN)
C
          ENDER=INT(DBLE(XTENT)/(DBLE(XTENT)/100.0D0))
          I=ENDER
          JIMFLG(1:I)=0
          JIMX(1:I)=0
          JIMY(1:I)=0
          XY2(1:I)=0.0D0
          DRAFLG(1:21)=0
          DRAFLG2(1:21)=0
          NDRAY(1:21)=0
          NDRAX(1:21)=0
c
C     FIRST THE FSSI FACTOR
C     THE FSSI FACTOR IS ALWAYS MADE USING THE FIRST FIELD POSITION
C     AND THE FIRST WAVELENGTH FOR WHICH A FAN EXISTS
C
          IF(FSSIFLG) THEN
C     SCALE FACTOR EXISTS, DON'T CALCULATE IT
          ELSE
C     CALCULATE SSI BASED ON BIGGEST FAN VALUE
              YMAX=-1.0D10
C     PROCEED WITH THE CALCULATION
              DO JJJ=1,MAXFAN+1
                  IF((FFAANN(1,JJJ,1,1)).GE.YMAX)
     1            YMAX=(FFAANN(1,JJJ,1,1))
              END DO
              YMIN=1.0D10
C     PROCEED WITH THE CALCULATION
              DO JJJ=1,MAXFAN+1
                  IF((FFAANN(1,JJJ,1,1)).LE.YMIN)
     1            YMIN=(FFAANN(1,JJJ,1,1))
              END DO
              RANGE1=DABS(YMAX-YMIN)
              RANGE1=RANGE1*2.0D0
              IF(RANGE1.EQ.0.0D0) RANGE1=1.0D20
              FSSI=RANGE1
              FSSIFLG=.TRUE.
          END IF
C
C     NOW SCALE THE DATA BY DIVIDING BY SSI AND MULTIPLYING
C     BY (YTENT/2), THE EXTENT OF ALL VERTICAL PLOT AXES
C
          DO JJJ=1,MAXFAN+1
              FFAANN(1,JJJ,1,1)=((FFAANN(1,JJJ,1,1)/FSSI)*DBLE(YTENT/2))
          END DO
C     THE ACTUAL PLOTTING IS NOW DONE
C
C     SHIFT PLOT TO START AT LEFT EDGE
          DO JJJ=1,MAXFAN+1
              FFAANN(1,JJJ,1,1)=FFAANN(1,JJJ,1,1)+DBLE(YTENT/2)
          END DO
C     FAN DATA GOES FROM 0.0 TO YTENT.0
C     COLOR WAS SET BY JCOMP AT THE TOP OF THIS ROUTINE
C     LINESTYLE IS ALWAYS SET BY THE LINE STYLE COMMAND
C
C     ABSISSA DATA GOES FROM 0.0 TO XTENT
          ABSIS=DBLE(XTENT/2)/(DBLE(MAXFAN)/2.0D0)
          DRAXX=0.0D0
          FACTOR=0.0D0
          DO JJJ=1,MAXFAN+1
              DRAY(JJJ)=FFAANN(1,JJJ,1,1)
              DRAX(JJJ)=DRAXX+FACTOR
              FACTOR=FACTOR+ABSIS
              DRAFLG(JJJ)=INT(FFAANN(1,JJJ,5,1))
          END DO
          XP1=1.0D35
          XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO ENDER POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
          SPLFIT=.FALSE.
          DO ITES=2,MAXFAN+1
              IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
          END DO
          IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
          IK=1
          DO XVAL=0,INT(DBLE(XTENT)),INT(DBLE(XTENT)/100.0D0)
              IF(SPLFIT) CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
              JIMX(IK)=INT(DRAY(1))
              IF(SPLFIT) JIMX(IK)=INT(YVAL)
              JIMY(IK)=-INT(XVAL)
              IK=IK+1
          END DO
C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
          DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
              XXPOS=DBLE(JJJJ-1)*STEPJP1
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
              XXPOS1=DBLE(JJJJ)*STEPJP1
C
              DO JJJ=1,ENDER
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                  XPOS=DBLE(JJJ-1)*(DBLE(XTENT)/(DBLE(ENDER)-1.0D0))
C
                  IF(XPOS.GT.XXPOS1) GO TO 82
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                  IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                      IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                          JIMFLG(JJJ)=0
                      ELSE
                          JIMFLG(JJJ)=1
                      END IF
C
                  ELSE
C     NO TEST, PROCEED
                  END IF
                  IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                      IF(DRAFLG(JJJJ).EQ.0) THEN
                          JIMFLG(JJJ)=0
                      ELSE
                          JIMFLG(JJJ)=1
                      END IF
C
                  ELSE
C     NO TEST, PROCEED
                  END IF
                  IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                      IF(DRAFLG(JJJJ+1).EQ.0) THEN
                          JIMFLG(JJJ)=0
                      ELSE
                          JIMFLG(JJJ)=1
                      END IF
C
                  ELSE
C     NO TEST, PROCEED
                  END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
              END DO
82            CONTINUE
          END DO
C
          IF(CLIP.EQ.0) THEN
              XA1=FANAXX-(XTENT/2)
              XB1=FANAXX+(XTENT/2)
              YA1=FANAXY-(YTENT/2)
              YB1=FANAXY+(YTENT/2)
          ELSE
              XA1=0
              XB1=10000
              YA1=0
              YB1=7000
          END IF
          IXVALUE=-JIMY(1)+FANAXX-(XTENT/2)
          IYVALUE=JIMX(1)+FANAXY-(YTENT/2)

          CALL MY_PLOT(IXVALUE,IYVALUE,0,LNTYPE,XA1,XB1,YA1,YB1)
          DO IIK=2,ENDER
              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0)THEN
                  IXVALUE=-JIMY(IIK)+FANAXX-(XTENT/2)
                  IYVALUE=JIMX(IIK)+FANAXY-(YTENT/2)
                  CALL MY_PLOTC(IXVALUE,IYVALUE,0,LNTYPE,XA1,XB1,YA1,YB1)
              ELSE
                  IXVALUE=-JIMY(IIK)+FANAXX-(XTENT/2)
                  IYVALUE=JIMX(IIK)+FANAXY-(YTENT/2)
                  CALL MY_PLOTC(IXVALUE,IYVALUE,1,LNTYPE,XA1,XB1,YA1,YB1)
              END IF
          END DO
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          LNTYPE=1

          RETURN
      END


C ALL INTERACTER/WINTERACTER CALLS HAVE BEEN MOVED TO ISS.FOR
C SUB FANDO0.FOR
      SUBROUTINE FANDO0
C
          IMPLICIT NONE
C
C     DOES FAN DATA PLOTTING FOR FAN PLOTTING
C
          INTEGER I,JJJ,K,IK,IIK,L,JJJJ,RRPOS,RLPOS,LLPOS,LRPOS
     1    ,COLPAS,OPLPOS,OPRPOS,ITES,XA1,XB1,YA1,YB1,IX
C
          CHARACTER NNTT1*80,CRANGE*15,B*80,UNN*40
C
          INTEGER JIMX(1:101),JIMY(1:101),XA,XB,JIMFLG(1:101),FAN1,
     1    FAN2,FAN3,FAN4,FAN5,FAN6,FAN7,FAN8,FAN9,FAN10
C
          REAL*8 XMAX,YMAX,RANGE1,WAVE1,WAVE2,WAVE3,WAVE4,WAVE5
     1    ,XMAX1,XMAX2,YMAX1,YMAX2,XP1,XP2,WAVE6,WAVE7,WAVE8,WAVE9,WAVE10
     2    ,XVAL,YVAL,DRAXX,FACTOR,XPOS,XXPOS,XXPOS1
C
          REAL*8 XY2(1:101)
C
          LOGICAL SPLFIT,FANEXT
C
          COMMON/FANEXI/FANEXT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          JIMFLG(1:101)=0
          JIMX(1:101)=0
          JIMY(1:101)=0
          XY2(1:101)=0.0D0
          DRAFLG(1:21)=0
          DRAFLG2(1:21)=0
          NDRAY(1:21)=0
          NDRAX(1:21)=0
C
          IF(FANEXT) THEN
              FAN1=0
              FAN2=0
              FAN3=0
              FAN4=0
              FAN5=0
              FAN6=0
              FAN7=0
              FAN8=0
              FAN9=0
              FAN10=0
              IF(FANWV1) WAVE1=SYSTEM1(1)
              IF(FANWV2) WAVE1=SYSTEM1(2)
              IF(FANWV3) WAVE1=SYSTEM1(3)
              IF(FANWV4) WAVE1=SYSTEM1(4)
              IF(FANWV5) WAVE1=SYSTEM1(5)
              IF(FANWV6) WAVE1=SYSTEM1(71)
              IF(FANWV7) WAVE1=SYSTEM1(72)
              IF(FANWV8) WAVE1=SYSTEM1(73)
              IF(FANWV9) WAVE1=SYSTEM1(74)
              IF(FANWV10) WAVE1=SYSTEM1(75)
              IF(FANWV1) FAN1=1
              IF(FANWV2) FAN1=2
              IF(FANWV3) FAN1=3
              IF(FANWV4) FAN1=4
              IF(FANWV5) FAN1=5
              IF(FANWV6) FAN1=6
              IF(FANWV7) FAN1=7
              IF(FANWV8) FAN1=8
              IF(FANWV9) FAN1=9
              IF(FANWV10) FAN1=10
          ELSE
              FAN1=0
              FAN2=0
              FAN3=0
              FAN4=0
              FAN5=0
              FAN6=0
              FAN7=0
              FAN8=0
              FAN9=0
              FAN10=0
C     SCAN FOR THE 10 WAVELENGTHS TO USE

              DO I=1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
C     FANWV1 IS THE FIRST FAN
                      FAN1=1
                      WAVE1=SYSTEM1(1)
                      GO TO 10
                  ELSE
C     FANWV1 IS NOT THE FIRST FAN
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
C     FANWV2 IS THE FIRST FAN
                      FAN1=2
                      WAVE1=SYSTEM1(2)
                      GO TO 10
                  ELSE
C     FANWV2 IS NOT THE FIRST FAN
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
C     FANWV3 IS THE FIRST FAN
                      FAN1=3
                      WAVE1=SYSTEM1(3)
                      GO TO 10
                  ELSE
C     FANWV3 IS NOT THE FIRST FAN
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
C     FANWV4 IS THE FIRST FAN
                      FAN1=4
                      WAVE1=SYSTEM1(4)
                      GO TO 10
                  ELSE
C     FANWV4 IS NOT THE FIRST FAN
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
C     FANWV5 IS THE FIRST FAN
                      FAN1=5
                      WAVE1=SYSTEM1(5)
                      GO TO 10
                  ELSE
C     FANWV5 IS NOT THE FIRST FAN
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
C     FANWV6 IS THE FIRST FAN
                      FAN1=6
                      WAVE1=SYSTEM1(71)
                      GO TO 10
                  ELSE
C     FANWV6 IS NOT THE FIRST FAN
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
C     FANWV7 IS THE FIRST FAN
                      FAN1=7
                      WAVE1=SYSTEM1(72)
                      GO TO 10
                  ELSE
C     FANWV7 IS NOT THE FIRST FAN
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
C     FANWV8 IS THE FIRST FAN
                      FAN1=8
                      WAVE1=SYSTEM1(73)
                      GO TO 10
                  ELSE
C     FANWV8 IS NOT THE FIRST FAN
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
C     FANWV2 IS THE FIRST FAN
                      FAN1=9
                      WAVE1=SYSTEM1(74)
                      GO TO 10
                  ELSE
C     FANWV9 IS NOT THE FIRST FAN
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
C     FANWV10 IS THE FIRST FAN
                      FAN1=10
                      WAVE1=SYSTEM1(75)
                      GO TO 10
                  ELSE
C     FANWV10 IS NOT THE FIRST FAN
                  END IF
              END DO
              FAN1=11
 10           CONTINUE
              DO I=FAN1+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
C     FANWV1 IS THE SECOND FAN
                      FAN2=1
                      WAVE2=SYSTEM1(1)
                      GO TO 20
                  ELSE
C     FANWV1 IS NOT THE SECOND FAN
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
C     FANWV2 IS THE SECOND FAN
                      FAN2=2
                      WAVE2=SYSTEM1(2)
                      GO TO 20
                  ELSE
C     FANWV2 IS NOT THE SECOND FAN
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
C     FANWV3 IS THE SECOND FAN
                      FAN2=3
                      WAVE2=SYSTEM1(3)
                      GO TO 20
                  ELSE
C     FANWV3 IS NOT THE SECOND FAN
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
C     FANWV1 IS THE SECOND FAN
                      FAN2=4
                      WAVE2=SYSTEM1(4)
                      GO TO 20
                  ELSE
C     FANWV4 IS NOT THE SECOND FAN
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
C     FANWV5 IS THE SECOND FAN
                      FAN2=5
                      WAVE2=SYSTEM1(5)
                      GO TO 20
                  ELSE
C     FANWV5 IS NOT THE SECOND FAN
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
C     FANWV1 IS THE SECOND FAN
                      FAN2=6
                      WAVE2=SYSTEM1(71)
                      GO TO 20
                  ELSE
C     FANWV6 IS NOT THE SECOND FAN
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
C     FANWV1 IS THE SECOND FAN
                      FAN2=7
                      WAVE2=SYSTEM1(72)
                      GO TO 20
                  ELSE
C     FANWV7 IS NOT THE SECOND FAN
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
C     FANWV8 IS THE SECOND FAN
                      FAN2=8
                      WAVE2=SYSTEM1(73)
                      GO TO 20
                  ELSE
C     FANWV8 IS NOT THE SECOND FAN
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
C     FANWV9 IS THE SECOND FAN
                      FAN2=9
                      WAVE2=SYSTEM1(74)
                      GO TO 20
                  ELSE
C     FANWV9 IS NOT THE SECOND FAN
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
C     FANWV10 IS THE DECOND FAN
                      FAN2=10
                      WAVE2=SYSTEM1(75)
                      GO TO 20
                  ELSE
C     FANWV10 IS NOT THE SECOND FAN
                  END IF
              END DO
              FAN2=11
 20           CONTINUE
              DO I=FAN2+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN3=1
                      WAVE3=SYSTEM1(1)
                      GO TO 30
                  ELSE
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN3=2
                      WAVE3=SYSTEM1(2)
                      GO TO 30
                  ELSE
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN3=3
                      WAVE3=SYSTEM1(3)
                      GO TO 30
                  ELSE
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN3=4
                      WAVE3=SYSTEM1(4)
                      GO TO 30
                  ELSE
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN3=5
                      WAVE3=SYSTEM1(5)
                      GO TO 30
                  ELSE
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN3=6
                      WAVE3=SYSTEM1(71)
                      GO TO 30
                  ELSE
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN3=7
                      WAVE3=SYSTEM1(72)
                      GO TO 30
                  ELSE
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN3=8
                      WAVE3=SYSTEM1(73)
                      GO TO 30
                  ELSE
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN3=9
                      WAVE3=SYSTEM1(74)
                      GO TO 30
                  ELSE
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN3=10
                      WAVE3=SYSTEM1(75)
                      GO TO 30
                  ELSE
                  END IF
              END DO
              FAN3=11
 30           CONTINUE
              DO I=FAN3+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN4=1
                      WAVE4=SYSTEM1(1)
                      GO TO 40
                  ELSE
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN4=2
                      WAVE4=SYSTEM1(2)
                      GO TO 40
                  ELSE
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN4=3
                      WAVE4=SYSTEM1(3)
                      GO TO 40
                  ELSE
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN4=4
                      WAVE4=SYSTEM1(4)
                      GO TO 40
                  ELSE
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN4=5
                      WAVE4=SYSTEM1(5)
                      GO TO 40
                  ELSE
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN4=6
                      WAVE4=SYSTEM1(71)
                      GO TO 40
                  ELSE
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN4=7
                      WAVE4=SYSTEM1(72)
                      GO TO 40
                  ELSE
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN4=8
                      WAVE4=SYSTEM1(73)
                      GO TO 40
                  ELSE
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN4=9
                      WAVE4=SYSTEM1(74)
                      GO TO 40
                  ELSE
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN4=10
                      WAVE4=SYSTEM1(75)
                      GO TO 40
                  ELSE
                  END IF
              END DO
              FAN4=11
 40           CONTINUE
              DO I=FAN4+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN5=1
                      WAVE5=SYSTEM1(1)
                      GO TO 50
                  ELSE
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN5=2
                      WAVE5=SYSTEM1(2)
                      GO TO 50
                  ELSE
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN5=3
                      WAVE5=SYSTEM1(3)
                      GO TO 50
                  ELSE
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN5=4
                      WAVE5=SYSTEM1(4)
                      GO TO 50
                  ELSE
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN5=5
                      WAVE5=SYSTEM1(5)
                      GO TO 50
                  ELSE
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN5=6
                      WAVE5=SYSTEM1(71)
                      GO TO 50
                  ELSE
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN5=7
                      WAVE5=SYSTEM1(72)
                      GO TO 50
                  ELSE
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN5=8
                      WAVE5=SYSTEM1(73)
                      GO TO 50
                  ELSE
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN5=9
                      WAVE5=SYSTEM1(74)
                      GO TO 50
                  ELSE
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN5=10
                      WAVE5=SYSTEM1(75)
                      GO TO 50
                  ELSE
                  END IF
              END DO
              FAN5=11
 50           CONTINUE
              DO I=FAN5+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN6=1
                      WAVE6=SYSTEM1(1)
                      GO TO 60
                  ELSE
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN6=2
                      WAVE6=SYSTEM1(2)
                      GO TO 60
                  ELSE
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN6=3
                      WAVE6=SYSTEM1(3)
                      GO TO 60
                  ELSE
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN6=4
                      WAVE6=SYSTEM1(4)
                      GO TO 60
                  ELSE
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN6=5
                      WAVE6=SYSTEM1(5)
                      GO TO 60
                  ELSE
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN6=6
                      WAVE6=SYSTEM1(71)
                      GO TO 60
                  ELSE
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN6=7
                      WAVE6=SYSTEM1(72)
                      GO TO 60
                  ELSE
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN6=8
                      WAVE6=SYSTEM1(73)
                      GO TO 60
                  ELSE
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN6=9
                      WAVE6=SYSTEM1(74)
                      GO TO 60
                  ELSE
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN6=10
                      WAVE6=SYSTEM1(75)
                      GO TO 60
                  ELSE
                  END IF
              END DO
              FAN6=11
 60           CONTINUE
              DO I=FAN6+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN7=1
                      WAVE7=SYSTEM1(1)
                      GO TO 70
                  ELSE
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN7=2
                      WAVE7=SYSTEM1(2)
                      GO TO 70
                  ELSE
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN7=3
                      WAVE7=SYSTEM1(3)
                      GO TO 70
                  ELSE
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN7=4
                      WAVE7=SYSTEM1(4)
                      GO TO 70
                  ELSE
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN7=5
                      WAVE7=SYSTEM1(5)
                      GO TO 70
                  ELSE
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN7=6
                      WAVE7=SYSTEM1(71)
                      GO TO 70
                  ELSE
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN7=7
                      WAVE7=SYSTEM1(72)
                      GO TO 70
                  ELSE
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN7=8
                      WAVE7=SYSTEM1(73)
                      GO TO 70
                  ELSE
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN7=9
                      WAVE7=SYSTEM1(74)
                      GO TO 70
                  ELSE
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN7=10
                      WAVE7=SYSTEM1(75)
                      GO TO 70
                  ELSE
                  END IF
              END DO
              FAN7=11
 70           CONTINUE
              DO I=FAN7+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN8=1
                      WAVE8=SYSTEM1(1)
                      GO TO 80
                  ELSE
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN8=2
                      WAVE8=SYSTEM1(2)
                      GO TO 80
                  ELSE
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN8=3
                      WAVE8=SYSTEM1(3)
                      GO TO 80
                  ELSE
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN8=4
                      WAVE8=SYSTEM1(4)
                      GO TO 80
                  ELSE
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN8=5
                      WAVE8=SYSTEM1(5)
                      GO TO 80
                  ELSE
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN8=6
                      WAVE8=SYSTEM1(71)
                      GO TO 80
                  ELSE
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN8=7
                      WAVE8=SYSTEM1(72)
                      GO TO 80
                  ELSE
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN8=8
                      WAVE8=SYSTEM1(73)
                      GO TO 80
                  ELSE
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN8=9
                      WAVE8=SYSTEM1(74)
                      GO TO 80
                  ELSE
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN8=10
                      WAVE8=SYSTEM1(75)
                      GO TO 80
                  ELSE
                  END IF
              END DO
              FAN8=11
 80           CONTINUE
              DO I=FAN8+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN9=1
                      WAVE9=SYSTEM1(1)
                      GO TO 90
                  ELSE
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN9=2
                      WAVE9=SYSTEM1(2)
                      GO TO 90
                  ELSE
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN9=3
                      WAVE9=SYSTEM1(3)
                      GO TO 90
                  ELSE
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN9=4
                      WAVE9=SYSTEM1(4)
                      GO TO 90
                  ELSE
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN9=5
                      WAVE9=SYSTEM1(5)
                      GO TO 90
                  ELSE
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN9=6
                      WAVE9=SYSTEM1(71)
                      GO TO 90
                  ELSE
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN9=7
                      WAVE9=SYSTEM1(72)
                      GO TO 90
                  ELSE
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN9=8
                      WAVE9=SYSTEM1(73)
                      GO TO 90
                  ELSE
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN9=9
                      WAVE9=SYSTEM1(74)
                      GO TO 90
                  ELSE
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN9=10
                      WAVE9=SYSTEM1(75)
                      GO TO 90
                  ELSE
                  END IF
              END DO
              FAN9=11
 90           CONTINUE
              DO I=FAN9+1,10
                  IF(I.EQ.1.AND.FANWV1) THEN
                      FAN10=1
                      WAVE10=SYSTEM1(1)
                      GO TO 100
                  ELSE
                  END IF
                  IF(I.EQ.2.AND.FANWV2) THEN
                      FAN10=2
                      WAVE10=SYSTEM1(2)
                      GO TO 100
                  ELSE
                  END IF
                  IF(I.EQ.3.AND.FANWV3) THEN
                      FAN10=3
                      WAVE10=SYSTEM1(3)
                      GO TO 100
                  ELSE
                  END IF
                  IF(I.EQ.4.AND.FANWV4) THEN
                      FAN10=4
                      WAVE10=SYSTEM1(4)
                      GO TO 100
                  ELSE
                  END IF
                  IF(I.EQ.5.AND.FANWV5) THEN
                      FAN10=5
                      WAVE10=SYSTEM1(5)
                      GO TO 100
                  ELSE
                  END IF
                  IF(I.EQ.6.AND.FANWV6) THEN
                      FAN10=6
                      WAVE10=SYSTEM1(71)
                      GO TO 100
                  ELSE
                  END IF
                  IF(I.EQ.7.AND.FANWV7) THEN
                      FAN10=7
                      WAVE10=SYSTEM1(72)
                      GO TO 100
                  ELSE
                  END IF
                  IF(I.EQ.8.AND.FANWV8) THEN
                      FAN10=8
                      WAVE10=SYSTEM1(73)
                      GO TO 100
                  ELSE
                  END IF
                  IF(I.EQ.9.AND.FANWV9) THEN
                      FAN10=9
                      WAVE10=SYSTEM1(74)
                      GO TO 100
                  ELSE
                  END IF
                  IF(I.EQ.10.AND.FANWV10) THEN
                      FAN10=10
                      WAVE10=SYSTEM1(75)
                      GO TO 100
                  ELSE
                  END IF
              END DO
              FAN10=11
 100          CONTINUE
          END IF
          IF(FAN1.EQ.11) FAN1=0
          IF(FAN2.EQ.11) FAN2=0
          IF(FAN3.EQ.11) FAN3=0
          IF(FAN4.EQ.11) FAN4=0
          IF(FAN5.EQ.11) FAN5=0
          IF(FAN6.EQ.11) FAN6=0
          IF(FAN7.EQ.11) FAN7=0
          IF(FAN8.EQ.11) FAN8=0
          IF(FAN9.EQ.11) FAN9=0
          IF(FAN10.EQ.11) FAN10=0
C
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
c
C     FIRST THE SSI FACTOR
C     THE SSI FACTOR IS ALWAYS MADE USING THE FIRST FIELD POSITION
C     AND THE FIRST WAVELENGTH FOR WHICH A FAN EXISTS
C
          IF(.NOT.SSIFLG) THEN
C     SCALE FACTOR EXISTS, DON'T CALCULATE IT
          ELSE
C     CALCULATE SSI BASED ON BIGGEST FAN VALUE
              IF(QALTYP.NE.2) THEN
                  XMAX=-1.0D10
                  YMAX=-1.0D10
                  I=1
C     PROCEED WITH THE CALCULATION
                  DO K=1,10
                      DO JJJ=1,MAXFAN+1
                          IF(DABS(FFAANN(I,JJJ,1,K)).GE.XMAX) XMAX=DABS(FFAANN(I,JJJ,1,K))
                          IF(DABS(FFAANN(I,JJJ,2,K)).GE.YMAX) YMAX=DABS(FFAANN(I,JJJ,2,K))
                      END DO
                  END DO
                  RANGE1=0.0D0
                  IF(YMAX.GE.RANGE1) RANGE1=YMAX
                  IF(XMAX.GE.RANGE1) RANGE1=XMAX
                  SSI=2.0D0*DABS(RANGE1)
                  SSIFLG=.TRUE.
                  IF(SSI.EQ.0.0D0) SSI=0.001D0
C               SSI FACTOR CALCULATED, PROCEED
C
              ELSE
C     QALTYP=2
                  XMAX1=-1.0D10
                  XMAX2=-1.0D10
                  YMAX1=-1.0D10
                  YMAX2=-1.0D10
                  I=1
C     PROCEED WITH THE CALCULATION
                  DO K=1,10
                      DO JJJ=1,MAXFAN+1
                          IF(DABS(FFAANN(I,JJJ,1,K)).GE.XMAX1)XMAX1=DABS(FFAANN(I,JJJ,1,K))
                          IF(DABS(FFAANN(I,JJJ,2,K)).GE.YMAX1)YMAX1=DABS(FFAANN(I,JJJ,2,K))
                          IF(DABS(FFAANN(I,JJJ,3,K)).GE.XMAX2)XMAX2=DABS(FFAANN(I,JJJ,3,K))
                          IF(DABS(FFAANN(I,JJJ,4,K)).GE.YMAX2)YMAX2=DABS(FFAANN(I,JJJ,4,K))
                      END DO
                  END DO
                  RANGE1=0.0D0
                  IF(XMAX1.GE.RANGE1) RANGE1=XMAX1
                  IF(XMAX2.GE.RANGE1) RANGE1=XMAX2
                  IF(YMAX1.GE.RANGE1) RANGE1=YMAX1
                  IF(YMAX2.GE.RANGE1) RANGE1=YMAX2
                  SSI=2.0D0*DABS(RANGE1)
                  IF(SSI.EQ.0.0D0) SSI=0.001D0
                  SSIFLG=.TRUE.
C               SSI FACTOR CALCULATED, PROCEED
C
              END IF
          END IF
C
C     NOW SCALE THE DATA BY DIVIDING BY SSI AND MULTIPLYING
C     BY 1000, THE EXTENT OF ALL VERTICAL PLOT AXES
C
          DO I=1,FANNUM
              DO K=1,10
                  DO JJJ=1,MAXFAN+1
                      FFAANN(I,JJJ,1,K)=((FFAANN(I,JJJ,1,K)/SSI)*1000.0D0)
                      FFAANN(I,JJJ,2,K)=((FFAANN(I,JJJ,2,K)/SSI)*1000.0D0)
                      FFAANN(I,JJJ,3,K)=((FFAANN(I,JJJ,3,K)/SSI)*1000.0D0)
                      FFAANN(I,JJJ,4,K)=((FFAANN(I,JJJ,4,K)/SSI)*1000.0D0)
                      FFAANN(I,JJJ,5,K)=FFAANN(I,JJJ,5,K)
                      FFAANN(I,JJJ,6,K)=FFAANN(I,JJJ,6,K)
                  END DO
              END DO
          END DO
C     ALL ARRAYS HAVE BEEN SCALED, THE ACTUAL PLOTTING IS NOW DONE
C
          DO I=1,FANNUM
              DO K=1,10
C
                  IF(K.EQ.1.AND.FAN1.EQ.0) THEN
                      JJJ=MAXFAN+1
                      FFAANN(I,1:JJJ,1:6,1)=0.0D0
                  ELSE
C     DO NOTHING
                  END IF
C
                  IF(K.EQ.2.AND.FAN2.EQ.0) THEN
                      JJJ=MAXFAN+1
                      FFAANN(I,1:JJJ,1:6,2)=0.0D0
                  ELSE
C     DO NOTHING
                  END IF
C
                  IF(K.EQ.3.AND.FAN3.EQ.0) THEN
                      JJJ=MAXFAN+1
                      FFAANN(I,1:JJJ,1:6,3)=0.0D0
                  ELSE
C     DO NOTHING
                  END IF
C
                  IF(K.EQ.4.AND.FAN4.EQ.0) THEN
                      JJJ=MAXFAN+1
                      FFAANN(I,1:JJJ,1:6,4)=0.0D0
                  ELSE
C     DO NOTHING
                  END IF
C
                  IF(K.EQ.5.AND.FAN5.EQ.0) THEN
                      JJJ=MAXFAN+1
                      FFAANN(I,1:JJJ,1:6,5)=0.0D0
                  ELSE
C     DO NOTHING
                  END IF
C
                  IF(K.EQ.6.AND.FAN6.EQ.0) THEN
                      JJJ=MAXFAN+1
                      FFAANN(I,1:JJJ,1:6,6)=0.0D0
                  ELSE
C     DO NOTHING
                  END IF
C
                  IF(K.EQ.7.AND.FAN7.EQ.0) THEN
                      JJJ=MAXFAN+1
                      FFAANN(I,1:JJJ,1:6,7)=0.0D0
                  ELSE
C     DO NOTHING
                  END IF
C
                  IF(K.EQ.8.AND.FAN8.EQ.0) THEN
                      JJJ=MAXFAN+1
                      FFAANN(I,1:JJJ,1:6,8)=0.0D0
                  ELSE
C     DO NOTHING
                  END IF
C
                  IF(K.EQ.9.AND.FAN9.EQ.0) THEN
                      JJJ=MAXFAN+1
                      FFAANN(I,1:JJJ,1:6,9)=0.0D0
                  ELSE
C     DO NOTHING
                  END IF
C
                  IF(K.EQ.10.AND.FAN10.EQ.0) THEN
                      JJJ=MAXFAN+1
                      FFAANN(I,1:JJJ,1:6,10)=0.0D0
                  ELSE
C     DO NOTHING
                  END IF
C
              END DO
          END DO
C
C     PLOT THE SCALE FACTOR
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C     FOCAL OR UFOCAL
              IF(SYSTEM1(6).EQ.1.0D0) UNN='in(s)'
              IF(SYSTEM1(6).EQ.2.0D0) UNN='cm(s)'
              IF(SYSTEM1(6).EQ.3.0D0) UNN='mm(s)'
              IF(SYSTEM1(6).EQ.4.0D0) UNN='meter(s)'
          ELSE
              UNN='RADIANS'
          END IF
          IF(QALTYP.EQ.1)
     1    UNN='WAVE(S)'
!    scale bar
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          CALL MY_PLOT(100,1683,0,0,-10,10010,-10,10010)
          CALL MY_PLOT(100,1733,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(100,1708,0,0,-10,10010,-10,10010)
          CALL MY_PLOT(1100,1708,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(1100,1683,0,0,-10,10010,-10,10010)
          CALL MY_PLOT(1100,1733,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(1100,1673,0,0,-10,10010,-10,10010)
C     NOW DO THE TEXT

C     DO THE PLOTTING OF THE SCALE BAR AND ITS CAPTION
          WRITE(B,101) SSI
          READ(B,200) CRANGE
101       FORMAT(D15.8)
200       FORMAT(A15)
          NNTT1='  = '//CRANGE//' '//UNN
          CALL MY_SETCHARASPECT(1.5,1.5)
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          CALL MY_JUSTSTRING(1100,1673,NNTT1,0,1,3)
          NNTT1='VERTICAL ABERRATION SCALE BAR'
          CALL MY_SETCHARASPECT(1.5,1.5)
          CALL MY_JUSTSTRING(100,1500,NNTT1,0,1,3)

C
C     NOW THE DERIVATIVES ARE SCALED CORRECTLY TO PLOTTER UNITS
C     NOW CALCULATE THE ABSIS VALUE
          ABSIS=1250.0D0/(DBLE(MAXFAN)/2.0D0)
C     ABSIS IS THE SPACING OF THE POINTS TO BE PLOTTED IN
C     DEVICE INDEPENDENT COORDINATES IN THE REL APT DIRECTION
C
C     ALL DATA HAS BEEN SCALED AND ZEROED AS IS APPROPRIATE
C
C     WHERE ARE THE CENTERS OF THE FAN AXES LOCATED?
C
C     THE FAN AXES LOCATED AT PLOT COORDINATES
C                       Y=1750 (FROM 500 TO 3000)
          RLPOS=3000
          RRPOS=500
C                       Y=5250 (FROM 4000 TO 6500)
          LLPOS=6500
          LRPOS=4000
C     UNLESS WE ARE PLOTTING OPD AND THEN
C                       Y=3500 (FROM 2550 TO 4750)
          OPLPOS=4750
          OPRPOS=2250
C
C     FOR ONE FOV, THE X COORDINATE CENTER IS 5990 WITH A 2000 UNIT
C     TOTAL EXTENT( FROM 4990 TO 6990)
C
C     FOR TWO FOVS, THE X COORDINATE CENTERS ARE:
C               AT 3990 (FROM 2990 TO 4990)
C     AND
C               AT 7990 (FROM 6990 TO 8990)
C
C     FOR THREE FOVS, THE X COORDINATE CENTERS ARE:
C               AT 3323 (FROM 2323 TO 4323)
C     AND
C               AT 5990 (FROM 4990 TO 6990)
C     AND
C               AT 8657 (FROM 7657 TO 9657)
C
C     DO APPROPRIATE ORIGIN SHIFTING IN THE ABERRATION DIRECTION
C
          IF(FANNUM.EQ.1) THEN
C     ONE FOV
              DO K=1,10
                  DO JJJ=1,MAXFAN+1
                      FFAANN(1,JJJ,1,K)=(FFAANN(1,JJJ,1,K)+5990.0D0)
                      FFAANN(1,JJJ,2,K)=(FFAANN(1,JJJ,2,K)+5990.0D0)
                      FFAANN(1,JJJ,3,K)=(FFAANN(1,JJJ,3,K)+5990.0D0)
                      FFAANN(1,JJJ,4,K)=(FFAANN(1,JJJ,4,K)+5990.0D0)
C     LAST TERM IS NOT SHIFTED AS IT IS THE PLOT/NOPLOT FLAG
                  END DO
              END DO

          ELSE
C     NOT ONE FOV
          END IF
C
          IF(FANNUM.EQ.2) THEN
C     TWO FOVS
              DO K=1,10
                  DO JJJ=1,MAXFAN+1
                      FFAANN(1,JJJ,1,K)=(FFAANN(1,JJJ,1,K)+3990.0D0)
                      FFAANN(1,JJJ,2,K)=(FFAANN(1,JJJ,2,K)+3990.0D0)
                      FFAANN(1,JJJ,3,K)=(FFAANN(1,JJJ,3,K)+3990.0D0)
                      FFAANN(1,JJJ,4,K)=(FFAANN(1,JJJ,4,K)+3990.0D0)
                      FFAANN(2,JJJ,1,K)=(FFAANN(2,JJJ,1,K)+7990.0D0)
                      FFAANN(2,JJJ,2,K)=(FFAANN(2,JJJ,2,K)+7990.0D0)
                      FFAANN(2,JJJ,3,K)=(FFAANN(2,JJJ,3,K)+7990.0D0)
                      FFAANN(2,JJJ,4,K)=(FFAANN(2,JJJ,4,K)+7990.0D0)
C     LAST TERMS IS NOT SHIFTED AS IT IS THE PLOT/NOPLOT FLAG
                  END DO
              END DO
          ELSE
C     NOT TWO FOVS
          END IF
C
          IF(FANNUM.EQ.3) THEN
C     THREE FOVS
              DO K=1,10
                  DO JJJ=1,MAXFAN+1
                      FFAANN(1,JJJ,1,K)=(FFAANN(1,JJJ,1,K)+3323.0D0)
                      FFAANN(1,JJJ,2,K)=(FFAANN(1,JJJ,2,K)+3323.0D0)
                      FFAANN(1,JJJ,3,K)=(FFAANN(1,JJJ,3,K)+3323.0D0)
                      FFAANN(1,JJJ,4,K)=(FFAANN(1,JJJ,4,K)+3323.0D0)
                      FFAANN(2,JJJ,1,K)=(FFAANN(2,JJJ,1,K)+5990.0D0)
                      FFAANN(2,JJJ,2,K)=(FFAANN(2,JJJ,2,K)+5990.0D0)
                      FFAANN(2,JJJ,3,K)=(FFAANN(2,JJJ,3,K)+5990.0D0)
                      FFAANN(2,JJJ,4,K)=(FFAANN(2,JJJ,4,K)+5990.0D0)
                      FFAANN(3,JJJ,1,K)=(FFAANN(3,JJJ,1,K)+8657.0D0)
                      FFAANN(3,JJJ,2,K)=(FFAANN(3,JJJ,2,K)+8657.0D0)
                      FFAANN(3,JJJ,3,K)=(FFAANN(3,JJJ,3,K)+8657.0D0)
                      FFAANN(3,JJJ,4,K)=(FFAANN(3,JJJ,4,K)+8657.0D0)
                  END DO
              END DO
C     LAST TERMS IS NOT SHIFTED AS IT IS THE PLOT/NOPLOT FLAG
          ELSE
C     NOT THREE FOVS
          END IF
C
C**********************************************************************
C                       TRANSVERSE ABERRATION FANS QALTYP=0
C**********************************************************************
C
          IF(QALTYP.EQ.0) THEN
C     TRANSVERSE ABERRATION FANS
C     DO THE XZ-PLANE COMPONENT FIRST, THEN REPEAT FOR THE YZ-PLANE
C     COMPONENT
C
C     THE XZ PLANE COMPONENT
              DO L=1,FANNUM
C     LOAD THE DRAX,DRAY AND DRAFLG ARRAYS
                  DO K=1,10
                      IF(K.EQ.1) COLPAS=COLR1
                      IF(K.EQ.2) COLPAS=COLR2
                      IF(K.EQ.3) COLPAS=COLR3
                      IF(K.EQ.4) COLPAS=COLR4
                      IF(K.EQ.5) COLPAS=COLR5
                      IF(K.EQ.6) COLPAS=COLR6
                      IF(K.EQ.7) COLPAS=COLR7
                      IF(K.EQ.8) COLPAS=COLR8
                      IF(K.EQ.9) COLPAS=COLR9
                      IF(K.EQ.10) COLPAS=COLR10
                      CALL MY_COLTYP(COLPAS)
C
                      DRAXX=0.0D0
                      FACTOR=0.0D0
                      DO JJJ=1,MAXFAN+1
                          DRAY(JJJ)=FFAANN(L,JJJ,1,K)
                          DRAX(JJJ)=DRAXX+FACTOR
                          FACTOR=FACTOR+ABSIS
                          DRAFLG(JJJ)=INT(FFAANN(L,JJJ,5,K))
                      END DO
                      XP1=1.0D35
                      XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO 101 POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
                      SPLFIT=.FALSE.
                      DO ITES=2,MAXFAN+1
                          IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
                      END DO

                      IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
                      IK=1
                      DO IX=0,2500,25
                          XVAL=IX
                          IF(SPLFIT) CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
                          JIMX(IK)=INT(DRAY(1))
                          IF(SPLFIT) JIMX(IK)=INT(YVAL)
                          JIMY(IK)=-INT(XVAL)+LLPOS
                          IK=IK+1
                      END DO

C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
                      DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS=DBLE(JJJJ-1)*STEPJP
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS1=DBLE(JJJJ)*STEPJP
C
                          DO JJJ=1,101
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                              XPOS=DBLE(JJJ-1)*25.0D0
C
                              IF(XPOS.GT.XXPOS1) GO TO 82
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                              IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                            .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                            .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
                          END DO
82                        CONTINUE
                      END DO
C
                      IK=IK-1
                      IF(FANNUM.EQ.1) THEN
                          XA=4990
                          XB=6990
                      ELSE
                      END IF
                      IF(FANNUM.EQ.2) THEN
                          IF(L.EQ.1) THEN
                              XA=2990
                              XB=4990
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=6990
                              XB=8990
                          ELSE
                          END IF
                      ELSE
                      END IF
                      IF(FANNUM.EQ.3) THEN
                          IF(L.EQ.1) THEN
                              XA=2323
                              XB=4323
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=4990
                              XB=6990
                          ELSE
                          END IF
                          IF(L.EQ.3) THEN
                              XA=7657
                              XB=9657
                          ELSE
                          END IF
                      ELSE
                      END IF
C
                      XA1=XA
                      XB1=XB
                      YA1=LRPOS
                      YB1=LLPOS
                      CALL FIXIT(JIMX(1),JIMY(1),XA1,XB1,YA1,YB1)
                      CALL MY_PLOT(JIMX(1),JIMY(1),0,0,XA1,XB1,YA1,YB1)
                      DO IIK=2,IK
C
                          IF(K.EQ.1.AND.FAN1.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,0,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,0,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.2.AND.FAN2.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,1,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,1,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.3.AND.FAN3.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,2,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,2,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.4.AND.FAN4.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,3,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,3,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.5.AND.FAN5.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,4,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,4,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.6.AND.FAN6.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,5,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,5,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.7.AND.FAN7.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,6,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,6,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.8.AND.FAN8.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,7,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,7,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.9.AND.FAN9.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,8,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,8,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.10.AND.FAN10.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,9,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,9,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                      END DO
                      CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C
                  END DO
C     DO NEXT FOV
              END DO
C
C     THE YZ PLANE COMPONENT
              DO L=1,FANNUM
C     LOAD THE DRAX,DRAY AND DRAFLG ARRAYS
                  DO K=1,10
                      IF(K.EQ.1) COLPAS=COLR1
                      IF(K.EQ.2) COLPAS=COLR2
                      IF(K.EQ.3) COLPAS=COLR3
                      IF(K.EQ.4) COLPAS=COLR4
                      IF(K.EQ.5) COLPAS=COLR5
                      IF(K.EQ.6) COLPAS=COLR6
                      IF(K.EQ.7) COLPAS=COLR7
                      IF(K.EQ.8) COLPAS=COLR8
                      IF(K.EQ.9) COLPAS=COLR9
                      IF(K.EQ.10) COLPAS=COLR10
                      CALL MY_COLTYP(COLPAS)
C
                      DRAXX=0.0D0
                      FACTOR=0.0D0
                      DO JJJ=1,MAXFAN+1
                          DRAY(JJJ)=FFAANN(L,JJJ,2,K)
                          DRAX(JJJ)=DRAXX+FACTOR
                          FACTOR=FACTOR+ABSIS
                          DRAFLG(JJJ)=INT(FFAANN(L,JJJ,6,K))
                      END DO
                      XP1=1.0D35
                      XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO 101 POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
                      SPLFIT=.FALSE.
                      DO ITES=2,MAXFAN+1
                          IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
                      END DO
                      IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
                      IK=1
                      DO IX=0,2500,25
                          XVAL=IX
                          IF(SPLFIT) CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
                          JIMX(IK)=INT(DRAY(1))
                          IF(SPLFIT)JIMX(IK)=INT(YVAL)
                          JIMY(IK)=-INT(XVAL)+RLPOS
                          IK=IK+1
                      END DO
C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
                      DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS=DBLE(JJJJ-1)*STEPJP
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS1=DBLE(JJJJ)*STEPJP
C
                          DO JJJ=1,101
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                              XPOS=DBLE(JJJ-1)*25.0D0
C
                              IF(XPOS.GT.XXPOS1) GO TO 83
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                              IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                            .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                            .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
                          END DO
83                        CONTINUE
                      END DO
C
                      IK=IK-1
                      IF(FANNUM.EQ.1) THEN
                          XA=4990
                          XB=6990
                      ELSE
                      END IF
                      IF(FANNUM.EQ.2) THEN
                          IF(L.EQ.1) THEN
                              XA=2990
                              XB=4990
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=6990
                              XB=8990
                          ELSE
                          END IF
                      ELSE
                      END IF
                      IF(FANNUM.EQ.3) THEN
                          IF(L.EQ.1) THEN
                              XA=2323
                              XB=4323
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=4990
                              XB=6990
                          ELSE
                          END IF
                          IF(L.EQ.3) THEN
                              XA=7657
                              XB=9657
                          ELSE
                          END IF
                      ELSE
                      END IF
C
                      XA1=XA
                      XB1=XB
                      YA1=RRPOS
                      YB1=RLPOS
                      CALL FIXIT(JIMX(1),JIMY(1),XA1,XB1,YA1,YB1)
                      CALL MY_PLOTC(JIMX(1),JIMY(1),0,0,XA1,XB1,YA1,YB1)
C
                      DO IIK=2,IK
                          IF(K.EQ.1.AND.FAN1.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,0,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,0,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.2.AND.FAN2.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,1,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,1,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.3.AND.FAN3.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,2,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,2,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.4.AND.FAN4.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,3,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,3,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.5.AND.FAN5.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,4,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,4,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.6.AND.FAN6.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,5,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,5,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.7.AND.FAN7.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,6,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,6,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.8.AND.FAN8.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,7,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,7,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.9.AND.FAN9.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,8,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,8,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.10.AND.FAN10.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     1                        IIK.eq.2)THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,9,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,9,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                      END DO
                      CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)

C
                  END DO
C     DO NEXT FOV
              END DO
              RETURN
          ELSE
C     NOT TRANSVERSE ABERRATION FANS
          END IF
C
C**********************************************************************
C                       TWO FAN OPD PLOTS QALTYP=1
C**********************************************************************
C
          IF(QALTYP.EQ.1.AND.FANTYP.GT.4) THEN
C     OPD FANS
C     DO THE XZ-PLANE COMPONENT FIRST, THEN REPEAT FOR THE YZ-PLANE
C     COMPONENT
C
C     THE XZ PLANE COMPONENT
              DO L=1,FANNUM
C     LOAD THE DRAX,DRAY AND DRAFLG ARRAYS
                  DO K=1,10
                      IF(K.EQ.1) COLPAS=COLR1
                      IF(K.EQ.2) COLPAS=COLR2
                      IF(K.EQ.3) COLPAS=COLR3
                      IF(K.EQ.4) COLPAS=COLR4
                      IF(K.EQ.5) COLPAS=COLR5
                      IF(K.EQ.6) COLPAS=COLR6
                      IF(K.EQ.7) COLPAS=COLR7
                      IF(K.EQ.8) COLPAS=COLR8
                      IF(K.EQ.9) COLPAS=COLR9
                      IF(K.EQ.10) COLPAS=COLR10
                      CALL MY_COLTYP(COLPAS)
C
                      DRAXX=0.0D0
                      FACTOR=0.0D0
                      DO JJJ=1,MAXFAN+1
                          DRAY(JJJ)=FFAANN(L,JJJ,1,K)
                          DRAX(JJJ)=DRAXX+FACTOR
                          FACTOR=FACTOR+ABSIS
                          DRAFLG(JJJ)=INT(FFAANN(L,JJJ,5,K))
                      END DO
                      XP1=1.0D35
                      XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO 101 POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
                      SPLFIT=.FALSE.
                      DO ITES=2,MAXFAN+1
                          IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
                      END DO
                      IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
                      IK=1
                      DO IX=0,2500,25
                          XVAL=IX
                          IF(SPLFIT) CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
                          JIMX(IK)=INT(DRAY(1))
                          IF(SPLFIT) JIMX(IK)=INT(YVAL)
                          JIMY(IK)=-INT(XVAL)+LLPOS
                          IK=IK+1
                      END DO
C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
                      DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS=DBLE(JJJJ-1)*STEPJP
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS1=DBLE(JJJJ)*STEPJP
C
                          DO JJJ=1,101
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                              XPOS=DBLE(JJJ-1)*25.0D0
C
                              IF(XPOS.GT.XXPOS1) GO TO 182
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                              IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                            .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                            .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
                          END DO
182                       CONTINUE
                      END DO
C
                      IK=IK-1
                      IF(FANNUM.EQ.1) THEN
                          XA=4990
                          XB=6990
                      ELSE
                      END IF
                      IF(FANNUM.EQ.2) THEN
                          IF(L.EQ.1) THEN
                              XA=2990
                              XB=4990
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=6990
                              XB=8990
                          ELSE
                          END IF
                      ELSE
                      END IF
                      IF(FANNUM.EQ.3) THEN
                          IF(L.EQ.1) THEN
                              XA=2323
                              XB=4323
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=4990
                              XB=6990
                          ELSE
                          END IF
                          IF(L.EQ.3) THEN
                              XA=7657
                              XB=9657
                          ELSE
                          END IF
                      ELSE
                      END IF
C
                      XA1=XA
                      XB1=XB
                      YA1=LRPOS
                      YB1=LLPOS
                      CALL FIXIT(JIMX(1),JIMY(1),XA1,XB1,YA1,YB1)
                      CALL MY_PLOTC(JIMX(1),JIMY(1),0,0,XA1,XB1,YA1,YB1)
                      DO IIK=2,IK
C
                          IF(K.EQ.1.AND.FAN1.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,0,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,0,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.2.AND.FAN2.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,1,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,1,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.3.AND.FAN3.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,2,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,2,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.4.AND.FAN4.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,3,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,3,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.5.AND.FAN5.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,4,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,4,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.6.AND.FAN6.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,5,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,5,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.7.AND.FAN7.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,6,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,6,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.8.AND.FAN8.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,7,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,7,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.9.AND.FAN9.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,8,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,8,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.10.AND.FAN10.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,9,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,9,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                      END DO
                      CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C
                  END DO
C     DO NEXT FOV
              END DO
C
C     THE YZ PLANE COMPONENT
              DO L=1,FANNUM
C     LOAD THE DRAX,DRAY AND DRAFLG ARRAYS
                  DO K=1,10
                      IF(K.EQ.1) COLPAS=COLR1
                      IF(K.EQ.2) COLPAS=COLR2
                      IF(K.EQ.3) COLPAS=COLR3
                      IF(K.EQ.4) COLPAS=COLR4
                      IF(K.EQ.5) COLPAS=COLR5
                      IF(K.EQ.6) COLPAS=COLR6
                      IF(K.EQ.7) COLPAS=COLR7
                      IF(K.EQ.8) COLPAS=COLR8
                      IF(K.EQ.9) COLPAS=COLR9
                      IF(K.EQ.10) COLPAS=COLR10
                      CALL MY_COLTYP(COLPAS)
C
                      DRAXX=0.0D0
                      FACTOR=0.0D0
                      DO JJJ=1,MAXFAN+1
                          DRAY(JJJ)=FFAANN(L,JJJ,2,K)
                          DRAX(JJJ)=DRAXX+FACTOR
                          FACTOR=FACTOR+ABSIS
                          DRAFLG(JJJ)=INT(FFAANN(L,JJJ,6,K))
                      END DO
                      XP1=1.0D35
                      XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO 101 POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
                      SPLFIT=.FALSE.
                      DO ITES=2,MAXFAN+1
                          IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
                      END DO
                      IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
                      IK=1
                      DO IX=0,2500,25
                          XVAL=IX
                          IF(SPLFIT) CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
                          JIMX(IK)=INT(DRAY(1))
                          IF(SPLFIT)JIMX(IK)=INT(YVAL)
                          JIMY(IK)=-INT(XVAL)+RLPOS
                          IK=IK+1
                      END DO
C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
                      DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS=DBLE(JJJJ-1)*STEPJP
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS1=DBLE(JJJJ)*STEPJP
C
                          DO JJJ=1,101
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                              XPOS=DBLE(JJJ-1)*25.0D0
C
                              IF(XPOS.GT.XXPOS1) GO TO 183
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                              IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                            .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                            .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
                          END DO
183                       CONTINUE
                      END DO
C
                      IK=IK-1
                      IF(FANNUM.EQ.1) THEN
                          XA=4990
                          XB=6990
                      ELSE
                      END IF
                      IF(FANNUM.EQ.2) THEN
                          IF(L.EQ.1) THEN
                              XA=2990
                              XB=4990
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=6990
                              XB=8990
                          ELSE
                          END IF
                      ELSE
                      END IF
                      IF(FANNUM.EQ.3) THEN
                          IF(L.EQ.1) THEN
                              XA=2323
                              XB=4323
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=4990
                              XB=6990
                          ELSE
                          END IF
                          IF(L.EQ.3) THEN
                              XA=7657
                              XB=9657
                          ELSE
                          END IF
                      ELSE
                      END IF
C
                      XA1=XA
                      XB1=XB
                      YA1=RRPOS
                      YB1=RLPOS
                      CALL FIXIT(JIMX(1),JIMY(1),XA1,XB1,YA1,YB1)
                      CALL MY_PLOTC(JIMX(1),JIMY(1),0,0,XA1,XB1,YA1,YB1)
C
                      DO IIK=2,IK
                          IF(K.EQ.1.AND.FAN1.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,0,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,0,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.2.AND.FAN2.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,1,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,1,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.3.AND.FAN3.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,2,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,2,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.4.AND.FAN4.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,3,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,3,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.5.AND.FAN5.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,4,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,4,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.6.AND.FAN6.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,5,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,5,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.7.AND.FAN7.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,6,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,6,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.8.AND.FAN8.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,7,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,7,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.9.AND.FAN9.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,8,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,8,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.10.AND.FAN10.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,9,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,9,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                      END DO
                      CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C
                  END DO
C     DO NEXT FOV
              END DO
              RETURN
          ELSE
C     NOT DOUBLE OPD FANS
          END IF
C
C**********************************************************************
C     LONGITUDINAL ABERRATION FANS QALTYP=3
C**********************************************************************
C
          IF(QALTYP.EQ.3) THEN
C     LONGITUDINAL ABERRATION FANS
C     DO THE XZ-PLANE COMPONENT FIRST, THEN REPEAT FOR THE YZ-PLANE
C     COMPONENT
C
C     THE XZ PLANE COMPONENT
              DO L=1,FANNUM
C     LOAD THE DRAX,DRAY AND DRAFLG ARRAYS
                  DO K=1,10
                      IF(K.EQ.1) COLPAS=COLR1
                      IF(K.EQ.2) COLPAS=COLR2
                      IF(K.EQ.3) COLPAS=COLR3
                      IF(K.EQ.4) COLPAS=COLR4
                      IF(K.EQ.5) COLPAS=COLR5
                      IF(K.EQ.6) COLPAS=COLR6
                      IF(K.EQ.7) COLPAS=COLR7
                      IF(K.EQ.8) COLPAS=COLR8
                      IF(K.EQ.9) COLPAS=COLR9
                      IF(K.EQ.10) COLPAS=COLR10
                      CALL MY_COLTYP(COLPAS)
C
                      DRAXX=0.0D0
                      FACTOR=0.0D0
                      DO JJJ=1,MAXFAN+1
                          DRAY(JJJ)=FFAANN(L,JJJ,1,K)
                          DRAX(JJJ)=DRAXX+FACTOR
                          FACTOR=FACTOR+ABSIS
                          DRAFLG(JJJ)=INT(FFAANN(L,JJJ,5,K))
                      END DO
                      XP1=1.0D35
                      XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO 101 POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
                      SPLFIT=.FALSE.
                      DO ITES=2,MAXFAN+1
                          IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
                      END DO
                      IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
                      IK=1
                      DO IX=0,2500,25
                          XVAL=IX
                          IF(SPLFIT) CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
                          JIMX(IK)=INT(DRAY(1))
                          IF(SPLFIT)JIMX(IK)=INT(YVAL)
                          JIMY(IK)=-INT(XVAL)+LLPOS
                          IK=IK+1
                      END DO
C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
                      DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS=DBLE(JJJJ-1)*STEPJP
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS1=DBLE(JJJJ)*STEPJP
C
                          DO JJJ=1,101
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                              XPOS=DBLE(JJJ-1)*25.0D0
C
                              IF(XPOS.GT.XXPOS1) GO TO 86
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                              IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                            .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                            .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
                          END DO
86                        CONTINUE
                      END DO
C
                      IK=IK-1
                      IF(FANNUM.EQ.1) THEN
                          XA=4990
                          XB=6990
                      ELSE
                      END IF
                      IF(FANNUM.EQ.2) THEN
                          IF(L.EQ.1) THEN
                              XA=2990
                              XB=4990
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=6990
                              XB=8990
                          ELSE
                          END IF
                      ELSE
                      END IF
                      IF(FANNUM.EQ.3) THEN
                          IF(L.EQ.1) THEN
                              XA=2323
                              XB=4323
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=4990
                              XB=6990
                          ELSE
                          END IF
                          IF(L.EQ.3) THEN
                              XA=7657
                              XB=9657
                          ELSE
                          END IF
                      ELSE
                      END IF
C
                      XA1=XA
                      XB1=XB
                      YA1=LRPOS
                      YB1=LLPOS
                      CALL FIXIT(JIMX(1),JIMY(1),XA1,XB1,YA1,YB1)
                      CALL MY_PLOTC(JIMX(1),JIMY(1),0,0,XA1,XB1,YA1,YB1)
                      DO IIK=2,IK
C
                          IF(K.EQ.1.AND.FAN1.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,0,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,0,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.2.AND.FAN2.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,1,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,1,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.3.AND.FAN3.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,2,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,2,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.4.AND.FAN4.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,3,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,3,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.5.AND.FAN5.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,4,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,4,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.6.AND.FAN6.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,5,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,5,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.7.AND.FAN7.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,6,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,6,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.8.AND.FAN8.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,7,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,7,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.9.AND.FAN9.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,8,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,8,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.10.AND.FAN10.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,9,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=LRPOS
                                  YB1=LLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,9,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                      END DO
                      CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C
                  END DO
C     DO NEXT FOV
              END DO
C
C     THE YZ PLANE COMPONENT
              DO L=1,FANNUM
C     LOAD THE DRAX,DRAY AND DRAFLG ARRAYS
                  DO K=1,10
                      IF(K.EQ.1) COLPAS=COLR1
                      IF(K.EQ.2) COLPAS=COLR2
                      IF(K.EQ.3) COLPAS=COLR3
                      IF(K.EQ.4) COLPAS=COLR4
                      IF(K.EQ.5) COLPAS=COLR5
                      IF(K.EQ.6) COLPAS=COLR6
                      IF(K.EQ.7) COLPAS=COLR7
                      IF(K.EQ.8) COLPAS=COLR8
                      IF(K.EQ.9) COLPAS=COLR9
                      IF(K.EQ.10) COLPAS=COLR10
                      CALL MY_COLTYP(COLPAS)
C
                      DRAXX=0.0D0
                      FACTOR=0.0D0
                      DO JJJ=1,MAXFAN+1
                          DRAY(JJJ)=FFAANN(L,JJJ,2,K)
                          DRAX(JJJ)=DRAXX+FACTOR
                          FACTOR=FACTOR+ABSIS
                          DRAFLG(JJJ)=INT(FFAANN(L,JJJ,6,K))
                      END DO
                      XP1=1.0D35
                      XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO 101 POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
                      SPLFIT=.FALSE.
                      DO ITES=2,MAXFAN+1
                          IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
                      END DO
                      IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
                      IK=1
                      DO IX=0,2500,25
                          XVAL=IX
                          IF(SPLFIT) CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
                          JIMX(IK)=INT(DRAY(1))
                          IF(SPLFIT)JIMX(IK)=INT(YVAL)
                          JIMY(IK)=-INT(XVAL)+RLPOS
                          IK=IK+1
                      END DO
C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
                      DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS=DBLE(JJJJ-1)*STEPJP
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS1=DBLE(JJJJ)*STEPJP
C
                          DO JJJ=1,101
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                              XPOS=DBLE(JJJ-1)*25.0D0
C
                              IF(XPOS.GT.XXPOS1) GO TO 85
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                              IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                            .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                            .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
                          END DO
85                        CONTINUE
                      END DO
C
                      IK=IK-1
                      IF(FANNUM.EQ.1) THEN
                          XA=4990
                          XB=6990
                      ELSE
                      END IF
                      IF(FANNUM.EQ.2) THEN
                          IF(L.EQ.1) THEN
                              XA=2990
                              XB=4990
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=6990
                              XB=8990
                          ELSE
                          END IF
                      ELSE
                      END IF
                      IF(FANNUM.EQ.3) THEN
                          IF(L.EQ.1) THEN
                              XA=2323
                              XB=4323
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=4990
                              XB=6990
                          ELSE
                          END IF
                          IF(L.EQ.3) THEN
                              XA=7657
                              XB=9657
                          ELSE
                          END IF
                      ELSE
                      END IF
C
                      XA1=XA
                      XB1=XB
                      YA1=RRPOS
                      YB1=RLPOS
                      CALL FIXIT(JIMX(1),JIMY(1),XA1,XB1,YA1,YB1)
                      CALL MY_PLOTC(JIMX(1),JIMY(1),0,0,XA1,XB1,YA1,YB1)
C
                      DO IIK=2,IK
                          IF(K.EQ.1.AND.FAN1.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,0,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,0,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.2.AND.FAN2.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,1,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,1,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.3.AND.FAN3.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,2,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,2,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.4.AND.FAN4.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,3,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,3,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.5.AND.FAN5.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,4,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,4,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.6.AND.FAN6.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,5,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,5,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.7.AND.FAN7.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,6,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,6,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.8.AND.FAN8.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,7,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,7,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.9.AND.FAN9.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,8,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,8,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.10.AND.FAN10.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,9,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=RRPOS
                                  YB1=RLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,9,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                      END DO
                      CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C
                  END DO
C     DO NEXT FOV
              END DO
              RETURN
          ELSE
C     NOT LONGITUDINAL ABERRATION FANS
          END IF
C
C**********************************************************************
C               OPD FANS QALTYP=1 (SINGLE FAN PER FIELD)
C**********************************************************************
C
          IF(QALTYP.EQ.1.AND.FANTYP.NE.5.AND.FANTYP.NE.6) THEN
C                       OPD FANS
              DO L=1,FANNUM
C     LOAD THE DRAX,DRAY AND DRAFLG ARRAYS
                  DO K=1,10
                      IF(K.EQ.1) COLPAS=COLR1
                      IF(K.EQ.2) COLPAS=COLR2
                      IF(K.EQ.3) COLPAS=COLR3
                      IF(K.EQ.4) COLPAS=COLR4
                      IF(K.EQ.5) COLPAS=COLR5
                      IF(K.EQ.6) COLPAS=COLR6
                      IF(K.EQ.7) COLPAS=COLR7
                      IF(K.EQ.8) COLPAS=COLR8
                      IF(K.EQ.9) COLPAS=COLR9
                      IF(K.EQ.10) COLPAS=COLR10
                      CALL MY_COLTYP(COLPAS)
C
                      DRAXX=0.0D0
                      FACTOR=0.0D0
                      DO JJJ=1,MAXFAN+1
                          DRAY(JJJ)=FFAANN(L,JJJ,1,K)
                          DRAX(JJJ)=DRAXX+FACTOR
                          FACTOR=FACTOR+ABSIS
                          DRAFLG(JJJ)=INT(FFAANN(L,JJJ,5,K))
                      END DO
                      XP1=1.0D35
                      XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO 101 POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
                      SPLFIT=.FALSE.
                      DO ITES=2,MAXFAN+1
                          IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
                      END DO
                      IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
                      IK=1
                      DO IX=0,2500,25
                          XVAL=IX
                          IF(SPLFIT) CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
                          JIMX(IK)=INT(DRAY(1))
                          IF(SPLFIT) JIMX(IK)=INT(YVAL)
                          JIMY(IK)=-INT(XVAL)+OPLPOS
                          IK=IK+1
                      END DO
C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
                      DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS=DBLE(JJJJ-1)*STEPJP
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                          XXPOS1=DBLE(JJJJ)*STEPJP
C
                          DO JJJ=1,101
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                              XPOS=DBLE(JJJ-1)*25.0D0
C
                              IF(XPOS.GT.XXPOS1) GO TO 84
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                              IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                            .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                            .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
                              IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                                  IF(DRAFLG(JJJJ+1).EQ.0) THEN
                                      JIMFLG(JJJ)=0
                                  ELSE
                                      JIMFLG(JJJ)=1
                                  END IF
C
                              ELSE
C     NO TEST, PROCEED
                              END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
                          END DO
84                        CONTINUE
                      END DO
C
                      IK=IK-1
                      IF(FANNUM.EQ.1) THEN
                          XA=4990
                          XB=6990
                      ELSE
                      END IF
                      IF(FANNUM.EQ.2) THEN
                          IF(L.EQ.1) THEN
                              XA=2990
                              XB=4990
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=6990
                              XB=8990
                          ELSE
                          END IF
                      ELSE
                      END IF
                      IF(FANNUM.EQ.3) THEN
                          IF(L.EQ.1) THEN
                              XA=2323
                              XB=4323
                          ELSE
                          END IF
                          IF(L.EQ.2) THEN
                              XA=4990
                              XB=6990
                          ELSE
                          END IF
                          IF(L.EQ.3) THEN
                              XA=7657
                              XB=9657
                          ELSE
                          END IF
                      ELSE
                      END IF
C
                      XA1=XA
                      XB1=XB
                      YA1=OPRPOS
                      YB1=OPLPOS
                      CALL FIXIT(JIMX(1),JIMY(1),XA1,XB1,YA1,YB1)
                      CALL MY_PLOTC(JIMX(1),JIMY(1),0,0,XA1,XB1,YA1,YB1)
C
                      DO IIK=2,IK
                          IF(K.EQ.1.AND.FAN1.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,0,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,0,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.2.AND.FAN2.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,1,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,1,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.3.AND.FAN3.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,2,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,2,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.4.AND.FAN4.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,3,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,3,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.5.AND.FAN5.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,4,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,4,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.6.AND.FAN6.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,5,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,5,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.7.AND.FAN7.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,6,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,6,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.8.AND.FAN8.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,7,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,7,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.9.AND.FAN9.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,8,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,8,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                          IF(K.EQ.10.AND.FAN10.NE.0) THEN
                              IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                        JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                        JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                        IIK.eq.2) THEN
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,9,XA1,XB1,YA1,YB1)
                              ELSE
                                  XA1=XA
                                  XB1=XB
                                  YA1=OPRPOS
                                  YB1=OPLPOS
                                  CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                                  CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,9,XA1,XB1,YA1,YB1)
                              END IF
                          ELSE
                          END IF
                      END DO
                      CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C
                  END DO
C     DO NEXT FOV
              END DO
              RETURN
C
          ELSE
C     NOT OPD FANS
          END IF
C
C**********************************************************************
C     CHROMATIC DIFFERENCE FANS QALTYP=2
C**********************************************************************
C
          IF(QALTYP.EQ.2) THEN
C     CHROMATIC DIFFERENCE FANS
C     FIRST PRIMARY DIFFERENCES, THEN SECONDARY DIFFERENCES
C     K IS ALWAYS = 1
              COLPAS=COLR1
              CALL MY_COLTYP(COLPAS)
C
C     PRIMARY DIFFERENCES
C
C     DO THE XZ-PLANE COMPONENT FIRST, THEN REPEAT FOR THE YZ-PLANE
C     COMPONENT
C
C     THE XZ PLANE COMPONENT
              DO L=1,FANNUM
C     LOAD THE DRAX,DRAY AND DRAFLG ARRAYS
C
                  DRAXX=0.0D0
                  FACTOR=0.0D0
                  DO JJJ=1,MAXFAN+1
                      DRAY(JJJ)=FFAANN(L,JJJ,1,1)
                      DRAX(JJJ)=DRAXX+FACTOR
                      FACTOR=FACTOR+ABSIS
                      DRAFLG(JJJ)=INT(FFAANN(L,JJJ,5,1))
                  END DO
                  XP1=1.0D35
                  XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO 101 POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
                  SPLFIT=.FALSE.
                  DO ITES=2,MAXFAN+1
                      IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
                  END DO
                  IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
                  IK=1
                  DO IX=0,2500,25
                      XVAL=IX
                      IF(SPLFIT)CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
                      JIMX(IK)=INT(DRAY(1))
                      IF(SPLFIT)JIMX(IK)=INT(YVAL)
                      JIMY(IK)=-INT(XVAL)+LLPOS
                      IK=IK+1
                  END DO
C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
                  DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                      XXPOS=DBLE(JJJJ-1)*STEPJP
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                      XXPOS1=DBLE(JJJJ)*STEPJP
C
                      DO JJJ=1,101
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                          XPOS=DBLE(JJJ-1)*25.0D0
C
                          IF(XPOS.GT.XXPOS1) GO TO 87
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                          IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                        .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                        .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
                          IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
                          IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ+1).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
                      END DO
87                    CONTINUE
                  END DO
C
                  IK=IK-1
                  IF(FANNUM.EQ.1) THEN
                      XA=4990
                      XB=6990
                  ELSE
                  END IF
                  IF(FANNUM.EQ.2) THEN
                      IF(L.EQ.1) THEN
                          XA=2990
                          XB=4990
                      ELSE
                      END IF
                      IF(L.EQ.2) THEN
                          XA=6990
                          XB=8990
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(FANNUM.EQ.3) THEN
                      IF(L.EQ.1) THEN
                          XA=2323
                          XB=4323
                      ELSE
                      END IF
                      IF(L.EQ.2) THEN
                          XA=4990
                          XB=6990
                      ELSE
                      END IF
                      IF(L.EQ.3) THEN
                          XA=7657
                          XB=9657
                      ELSE
                      END IF
                  ELSE
                  END IF
C
                  XA1=XA
                  XB1=XB
                  YA1=LRPOS
                  YB1=LLPOS
                  CALL FIXIT(JIMX(1),JIMY(1),XA1,XB1,YA1,YB1)
                  CALL MY_PLOTC(JIMX(1),JIMY(1),0,0,XA1,XB1,YA1,YB1)
                  DO IIK=2,IK
C
                      IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                IIK.eq.2) THEN
                          XA1=XA
                          XB1=XB
                          YA1=LRPOS
                          YB1=LLPOS
                          CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                          CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,0,XA1,XB1,YA1,YB1)
                      ELSE
                          XA1=XA
                          XB1=XB
                          YA1=LRPOS
                          YB1=LLPOS
                          CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                          CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,0,XA1,XB1,YA1,YB1)
                      END IF
                  END DO
                  CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C     DO NEXT FOV
              END DO
C
C     THE YZ PLANE COMPONENT
              DO L=1,FANNUM
C     LOAD THE DRAX,DRAY AND DRAFLG ARRAYS
C
                  DRAXX=0.0D0
                  FACTOR=0.0D0
                  DO JJJ=1,MAXFAN+1
                      DRAY(JJJ)=FFAANN(L,JJJ,2,1)
                      DRAX(JJJ)=DRAXX+FACTOR
                      FACTOR=FACTOR+ABSIS
                      DRAFLG(JJJ)=INT(FFAANN(L,JJJ,6,1))
                  END DO
                  XP1=1.0D35
                  XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO 101 POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
                  SPLFIT=.FALSE.
                  DO ITES=2,MAXFAN+1
                      IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
                  END DO
                  IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
                  IK=1
                  DO IX=0,2500,25
                      XVAL=IX
                      IF(SPLFIT) CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
                      JIMX(IK)=INT(DRAY(1))
                      IF(SPLFIT)JIMX(IK)=INT(YVAL)
                      JIMY(IK)=-INT(XVAL)+RLPOS
                      IK=IK+1
                  END DO
C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
                  DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                      XXPOS=DBLE(JJJJ-1)*STEPJP
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                      XXPOS1=DBLE(JJJJ)*STEPJP
C
                      DO JJJ=1,101
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                          XPOS=DBLE(JJJ-1)*25.0D0
C
                          IF(XPOS.GT.XXPOS1) GO TO 88
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                          IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                        .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                        .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
                          IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
                          IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ+1).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
                      END DO
88                    CONTINUE
                  END DO
C
                  IK=IK-1
                  IF(FANNUM.EQ.1) THEN
                      XA=4990
                      XB=6990
                  ELSE
                  END IF
                  IF(FANNUM.EQ.2) THEN
                      IF(L.EQ.1) THEN
                          XA=2990
                          XB=4990
                      ELSE
                      END IF
                      IF(L.EQ.2) THEN
                          XA=6990
                          XB=8990
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(FANNUM.EQ.3) THEN
                      IF(L.EQ.1) THEN
                          XA=2323
                          XB=4323
                      ELSE
                      END IF
                      IF(L.EQ.2) THEN
                          XA=4990
                          XB=6990
                      ELSE
                      END IF
                      IF(L.EQ.3) THEN
                          XA=7657
                          XB=9657
                      ELSE
                      END IF
                  ELSE
                  END IF
C
                  XA1=XA
                  XB1=XB
                  YA1=RRPOS
                  YB1=RLPOS
                  CALL FIXIT(JIMX(1),JIMY(1),XA1,XB1,YA1,YB1)
                  CALL MY_PLOTC(JIMX(1),JIMY(1),0,0,XA1,XB1,YA1,YB1)
C
                  DO IIK=2,IK
                      IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                IIK.eq.2) THEN
                          XA1=XA
                          XB1=XB
                          YA1=RRPOS
                          YB1=RLPOS
                          CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                          CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,0,XA1,XB1,YA1,YB1)
                      ELSE
                          XA1=XA
                          XB1=XB
                          YA1=RRPOS
                          YB1=RLPOS
                          CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                          CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,0,XA1,XB1,YA1,YB1)
                      END IF
                  END DO
                  CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C
C     DO NEXT FOV
              END DO
C
C     SECONDARY DIFFERENCES
              COLPAS=COLR2
              CALL MY_COLTYP(COLPAS)
C
C     DO THE XZ-PLANE COMPONENT FIRST, THEN REPEAT FOR THE YZ-PLANE
C     COMPONENT
C
C     THE XZ PLANE COMPONENT
              DO L=1,FANNUM
C     LOAD THE DRAX,DRAY AND DRAFLG ARRAYS
C
                  DRAXX=0.0D0
                  FACTOR=0.0D0
                  DO JJJ=1,MAXFAN+1
                      DRAY(JJJ)=FFAANN(L,JJJ,3,1)
                      DRAX(JJJ)=DRAXX+FACTOR
                      FACTOR=FACTOR+ABSIS
                      DRAFLG(JJJ)=INT(FFAANN(L,JJJ,5,1))
                  END DO
                  XP1=1.0D35
                  XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO 101 POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
                  SPLFIT=.FALSE.
                  DO ITES=2,MAXFAN+1
                      IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
                  END DO
                  IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
                  IK=1
                  DO IX=0,2500,25
                      XVAL=IX
                      IF(SPLFIT) CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
                      JIMX(IK)=INT(DRAY(1))
                      IF(SPLFIT)JIMX(IK)=INT(YVAL)
                      JIMY(IK)=-INT(XVAL)+LLPOS
                      IK=IK+1
                  END DO
C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
                  DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                      XXPOS=DBLE(JJJJ-1)*STEPJP
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                      XXPOS1=DBLE(JJJJ)*STEPJP
C
                      DO JJJ=1,101
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                          XPOS=DBLE(JJJ-1)*25.0D0
C
                          IF(XPOS.GT.XXPOS1) GO TO 89
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                          IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                        .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                        .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
                          IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
                          IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ+1).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
                      END DO
89                    CONTINUE
                  END DO
C
                  IK=IK-1
                  IF(FANNUM.EQ.1) THEN
                      XA=4990
                      XB=6990
                  ELSE
                  END IF
                  IF(FANNUM.EQ.2) THEN
                      IF(L.EQ.1) THEN
                          XA=2990
                          XB=4990
                      ELSE
                      END IF
                      IF(L.EQ.2) THEN
                          XA=6990
                          XB=8990
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(FANNUM.EQ.3) THEN
                      IF(L.EQ.1) THEN
                          XA=2323
                          XB=4323
                      ELSE
                      END IF
                      IF(L.EQ.2) THEN
                          XA=4990
                          XB=6990
                      ELSE
                      END IF
                      IF(L.EQ.3) THEN
                          XA=7657
                          XB=9657
                      ELSE
                      END IF
                  ELSE
                  END IF
C
                  XA1=XA
                  XB1=XB
                  YA1=LRPOS
                  YB1=LLPOS
                  CALL FIXIT(JIMX(1),JIMY(1),XA1,XB1,YA1,YB1)
                  CALL MY_PLOTC(JIMX(1),JIMY(1),0,0,XA1,XB1,YA1,YB1)
                  DO IIK=2,IK
C
                      IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                IIK.eq.2) THEN
                          XA1=XA
                          XB1=XB
                          YA1=LRPOS
                          YB1=LLPOS
                          CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                          CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,1,XA1,XB1,YA1,YB1)
                      ELSE
                          XA1=XA
                          XB1=XB
                          YA1=LRPOS
                          YB1=LLPOS
                          CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                          CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,1,XA1,XB1,YA1,YB1)
                      END IF
                  END DO
                  CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C     DO NEXT FOV
              END DO
C
C     THE YZ PLANE COMPONENT
              DO L=1,FANNUM
C     LOAD THE DRAX,DRAY AND DRAFLG ARRAYS
C
                  DRAXX=0.0D0
                  FACTOR=0.0D0
                  DO JJJ=1,MAXFAN+1
                      DRAY(JJJ)=FFAANN(L,JJJ,4,1)
                      DRAX(JJJ)=DRAXX+FACTOR
                      FACTOR=FACTOR+ABSIS
                      DRAFLG(JJJ)=INT(FFAANN(L,JJJ,6,1))
                  END DO
                  XP1=1.0D35
                  XP2=1.0D35
C     NOW SPLINE FIT THESE AND INTERPOLATE TO 101 POINTS
C     ACROSS THE Y PLOT DIRECTION
C
C     DON'T DO THE FIT IF ALL THE DRAY VALUES ARE THE SAME
                  SPLFIT=.FALSE.
                  DO ITES=2,MAXFAN+1
                      IF(REAL(DRAY(ITES-1)).NE.REAL(DRAY(ITES))) SPLFIT=.TRUE.
                  END DO
                  IF(SPLFIT) CALL SPLINE(DRAX,DRAY,MAXFAN+1,XP1,XP2,XY2)
C     NOW INTERPOLATE THE VALUES AND PLOT THEM
C
                  IK=1
                  DO IX=0,2500,25
                      XVAL=IX
                      IF(SPLFIT)CALL SPLINT(DRAX,DRAY,XY2,MAXFAN+1,XVAL,YVAL)
                      JIMX(IK)=INT(DRAY(1))
                      IF(SPLFIT)JIMX(IK)=INT(YVAL)
                      JIMY(IK)=-INT(XVAL)+RLPOS
                      IK=IK+1
                  END DO
C HERE DO THE APPROPRIATE SETTING OF JIMFLG()
                  DO JJJJ=1,MAXFAN
C     XXPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                      XXPOS=DBLE(JJJJ-1)*STEPJP
C     XXPOS1 IS THE NEXT POSITION ACROSS THE FAN IN THE NON-
C     INTERPOLATED DATA
                      XXPOS1=DBLE(JJJJ)*STEPJP
C
                      DO JJJ=1,101
C     XPOS IS THE CURRENT POSITION ACROSS THE FAN IN THE INTERPOLATED
C     DATA
                          XPOS=DBLE(JJJ-1)*25.0D0
C
                          IF(XPOS.GT.XXPOS1) GO TO 91
C     IF XPOS LIES AT OR BETWEEN XXPOS AND XXPOS1, DO A TEST
C     ELSE PROCEED
                          IF(XPOS.GT.XXPOS.AND.XPOS.LT.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.0
     1                        .OR.DRAFLG(JJJJ).EQ.0.AND.DRAFLG(JJJJ+1).EQ.1
     2                        .OR.DRAFLG(JJJJ).EQ.1.AND.DRAFLG(JJJJ+1).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
                          IF(XPOS.EQ.XXPOS) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
                          IF(XPOS.EQ.XXPOS1) THEN
C     DO THE TEST AND SET JIMFLG APPROPRIATELY
C
                              IF(DRAFLG(JJJJ+1).EQ.0) THEN
                                  JIMFLG(JJJ)=0
                              ELSE
                                  JIMFLG(JJJ)=1
                              END IF
C
                          ELSE
C     NO TEST, PROCEED
                          END IF
C     XPOS MUST BE BETWEEN OR BELOW XXPOS TO GET HERE
                      END DO
91                    CONTINUE
                  END DO
C
                  IK=IK-1
                  IF(FANNUM.EQ.1) THEN
                      XA=4990
                      XB=6990
                  ELSE
                  END IF
                  IF(FANNUM.EQ.2) THEN
                      IF(L.EQ.1) THEN
                          XA=2990
                          XB=4990
                      ELSE
                      END IF
                      IF(L.EQ.2) THEN
                          XA=6990
                          XB=8990
                      ELSE
                      END IF
                  ELSE
                  END IF
                  IF(FANNUM.EQ.3) THEN
                      IF(L.EQ.1) THEN
                          XA=2323
                          XB=4323
                      ELSE
                      END IF
                      IF(L.EQ.2) THEN
                          XA=4990
                          XB=6990
                      ELSE
                      END IF
                      IF(L.EQ.3) THEN
                          XA=7657
                          XB=9657
                      ELSE
                      END IF
                  ELSE
                  END IF
C
                  XA1=XA
                  XB1=XB
                  YA1=RRPOS
                  YB1=RLPOS
                  CALL FIXIT(JIMX(1),JIMY(1),XA1,XB1,YA1,YB1)
                  CALL MY_PLOTC(JIMX(1),JIMY(1),0,0,XA1,XB1,YA1,YB1)
C
                  DO IIK=2,IK
                      IF(JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.0.OR.
     1                JIMFLG(IIK-1).EQ.0.AND.JIMFLG(IIK).EQ.1.OR.
     1                JIMFLG(IIK-1).EQ.1.AND.JIMFLG(IIK).EQ.0.or.
     &                IIK.eq.2) THEN
                          XA1=XA
                          XB1=XB
                          YA1=RRPOS
                          YB1=RLPOS
                          CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                          CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),0,1,XA1,XB1,YA1,YB1)
                      ELSE
                          XA1=XA
                          XB1=XB
                          YA1=RRPOS
                          YB1=RLPOS
                          CALL FIXIT(JIMX(IIK),JIMY(IIK),XA1,XB1,YA1,YB1)
                          CALL MY_PLOTC(JIMX(IIK),JIMY(IIK),1,1,XA1,XB1,YA1,YB1)
                      END IF
                  END DO
                  CALL MY_PLOT(0,0,0,0,-10,10010,-10,10010)
C     DO NEXT FOV
              END DO
              RETURN
          ELSE
C     NOT CHROMATIC DIFFERENCE FANS
          END IF
C
          RETURN
      END


      SUBROUTINE FIXIT(X,Y,XA,XB,YA,YB)
          IMPLICIT NONE
          INTEGER X,Y,XA,XB,YA,YB,CENTX,CENTY,TEMPX,TEMPY
     1    ,NEWCENTX,NEWCENTY,NEWX,NEWY,NEWXA,NEWXB,NEWYA,NEWYB
C     THE CURRENT FAN IS CENTERED AT:
          CENTX=(XA+XB)/2
          CENTY=(YA+YB)/2
C     X STARTS OUT REPRESENTING THE X-DIRECTION ON THE PLOT(ABER)
C     Y STARTS OUT REPRESENTING THE Y-DIRECTION ON THE PLOT(PUPIL)
C
C     DETERMINE THE NEW X AND Y LOCATIONS OF THE NEW PLOT CENTER
C     NEW Y CENTER

          IF(CENTX.EQ.5990) NEWCENTX=5000
          IF(CENTX.EQ.3990) NEWCENTX=3333
          IF(CENTX.EQ.7990) NEWCENTX=6666
          IF(CENTX.EQ.3323) NEWCENTX=1666
          IF(CENTX.EQ.8657) NEWCENTX=8332
          IF(CENTY.EQ.5250) NEWCENTY=5750
          IF(CENTY.EQ.1750) NEWCENTY=3250
          IF(CENTY.EQ.3500) NEWCENTY=4500
C     NEW CENTERS HAVE BEEN PICKED
          TEMPY=-(Y-CENTY)
          TEMPX=X-CENTX
          NEWY=NEWCENTY+TEMPX
          NEWX=NEWCENTX+TEMPY
          NEWXA=NEWCENTX-1250
          NEWXB=NEWCENTX+1250
          NEWYA=NEWCENTY-1000
          NEWYB=NEWCENTY+1000
          X=NEWX
          Y=NEWY
          XA=NEWXA
          XB=NEWXB
          YA=NEWYA
          YB=NEWYB
          RETURN
      END
