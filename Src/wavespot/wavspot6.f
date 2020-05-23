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

C     SIXTH FILE OF CAPFN/SPOT ROUTINES

      SUBROUTINE FOURN(DATA,NPOINTS,NDAT)
C
          IMPLICIT NONE
C
C     NDAT COMES IN AS 2*NPOINTS*NPOINTS
C
          INTEGER NTOT,NDIM,NN(2),ISIGN,NPREV,IP1,IP2,IP3,N
     1    ,IFP1,NREM,IBIT,IDIM,I1,I2,I3,I2REV,I3REV,NDAT
     2    ,IFP2,K1,K2,NPOINTS
C
          REAL*8 TWOPI,WR,WI,WPR,WPI,WTEMP,THETA
C
          REAL*8 DATA,TEMPI,TEMPR
C
          DIMENSION DATA(NDAT)
C
C     SET NN ARRAY
          NN(1)=NPOINTS
          NN(2)=NPOINTS
C
C     SET A FORWARD TRANSFER
          ISIGN=+1
          NDIM=2
C
          NTOT=1
          DO 11 IDIM=1,NDIM
              NTOT=NTOT*NN(IDIM)
11        CONTINUE
          NPREV=1
          DO 18 IDIM=1,NDIM
              N=NN(IDIM)
              NREM=NTOT/(N*NPREV)
              IP1=2*NPREV
              IP2=IP1*N
              IP3=IP2*NREM
              I2REV=1
              DO 14 I2=1,IP2,IP1
                  IF(I2.LT.I2REV)THEN
                      DO 13 I1=I2,I2+IP1-2,2
                          DO 12 I3=I1,IP3,IP2
                              I3REV=I2REV+I3-I2
                              TEMPR=DATA(I3)
                              TEMPI=DATA(I3+1)
                              DATA(I3)=DATA(I3REV)
                              DATA(I3+1)=DATA(I3REV+1)
                              DATA(I3REV)=TEMPR
                              DATA(I3REV+1)=TEMPI
12                        CONTINUE
13                    CONTINUE
                  ENDIF
                  IBIT=IP2/2
1                 IF ((IBIT.GE.IP1).AND.(I2REV.GT.IBIT)) THEN
                      I2REV=I2REV-IBIT
                      IBIT=IBIT/2
                      GO TO 1
                  ENDIF
                  I2REV=I2REV+IBIT
14            CONTINUE
              IFP1=IP1
2             IF(IFP1.LT.IP2)THEN
                  IFP2=2*IFP1
                  TWOPI= (2.0D0*3.14159265358979323846D0)
                  THETA=ISIGN*TWOPI/(IFP2/IP1)
                  WPR=-2.D0*DSIN(0.5D0*THETA)**2
                  WPI= DSIN(THETA)
                  WR=1.0D0
                  WI=0.0D0
                  DO 17 I3=1,IFP1,IP1
                      DO 16 I1=I3,I3+IP1-2,2
                          DO 15 I2=I1,IP3,IFP2
                              K1=I2
                              K2=K1+IFP1
                              TEMPR=(WR)*DATA(K2)-(WI)*DATA(K2+1)
                              TEMPI=(WR)*DATA(K2+1)+(WI)*DATA(K2)
                              DATA(K2)=DATA(K1)-TEMPR
                              DATA(K2+1)=DATA(K1+1)-TEMPI
                              DATA(K1)=DATA(K1)+TEMPR
                              DATA(K1+1)=DATA(K1+1)+TEMPI
15                        CONTINUE
16                    CONTINUE
                      WTEMP=WR
                      WR=WR*WPR-WI*WPI+WR
                      WI=WI*WPR+WTEMP*WPI+WI
17                CONTINUE
                  IFP1=IFP2
                  GO TO 2
              ENDIF
              NPREV=N*NPREV
18        CONTINUE
          RETURN
      END



      SUBROUTINE PLOTPSF(PCOUNT,XPLT,YPLT,FPLT,ROT,DFLAG)
          IMPLICIT NONE
C     F IS THE FUNTION FPLT(X,Y),XPLT AND YPLT ARE THE POINT COORDINATES
C     PCOUNT=NUMBER OF POINTS IN THE PSF FILE
C
          CHARACTER B*80,UNN*9,BLNOTE*80,BL20*20,NNTT1*99,CRANGE*15
     1    ,UNN2*9,UNN1*9,TMY*8,DTY*10,NRANGE*12
C
          INTEGER PCOUNT,ROT,IV,WVWT,NT1ANG,NT1SIZ,ALLOERR,DFLAG
     1    ,K,COLPAS,II,IIX,IIY,IPN,H,IIXM1,IIYM1,WVNUMB

          REAL*8 XTEST1,XTEST2,YTEST1,YTEST2,SLOPE
!      REAL*8 XMIN,XMAX
C
          REAL PEXTENT,FPLT,XPLT,YPLT,PSPACING
     1    ,CNTX,CNTY
C
          DIMENSION XPLT(PCOUNT),YPLT(PCOUNT),FPLT(PCOUNT,PCOUNT),H(:)
C
          COMMON/PSFPLOTER/PEXTENT,PSPACING,CNTX,CNTY
C
          EXTERNAL WVWT
C
          LOGICAL ERROR1
C
          INTEGER I,J
          REAL*8 RANGE1
          REAL*8 XV,YV
          REAL*8 XX,YY,XY2
          DIMENSION XX(:),YY(:),XY2(:)
          REAL F_JK,X_JK,Y_JK
          INTEGER JSTEP
          DIMENSION F_JK(:,:),X_JK(:),Y_JK(:)
          ALLOCATABLE :: XX,YY,F_JK,X_JK,Y_JK,XY2,H
          INCLUDE 'datmai.inc'
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
C
          DEALLOCATE(XX,YY,F_JK,X_JK,Y_JK,XY2,H,STAT=ALLOERR)
          ALLOCATE(XX(PCOUNT),YY(PCOUNT),XY2(PCOUNT),
     1    F_JK(PCOUNT,PCOUNT),X_JK(PCOUNT),Y_JK(PCOUNT),H(2740:7260),
     2    STAT=ALLOERR)
          H(2740:7260)=-10000
          I=PCOUNT
          X_JK(1:I)=XPLT(1:I)
          Y_JK(1:I)=YPLT(1:I)
          XX(1:I)=0.0
          YY(1:I)=0.0
          J=PCOUNT
          F_JK(1:I,1:J)=FPLT(1:I,1:J)
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
C
C     GENERATE GRAPHIC
C     DO A PLOT NEW
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
C     GENERATE GRAPHIC
          CALL PLTDEV
          GRASET=.TRUE.
          PLEXIS=.TRUE.
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
C
C     PLOT THE FUNCTION RIGHT HERE
C     STARTING AT -Y, PLOT A ROW FROM -X TO X
C     SET THE STARTING POINT
          IIX=INT(2750.0+XPLT(1))
          IIY=INT(2500.0+FPLT(1,1))
          CALL MY_PLOT(IIX,IIY,0,0,-10,10010,-10,7010)
          IPN=1
          JSTEP=NINT(DBLE(PGR)/50.0D0)
          IF(JSTEP.LT.1) JSTEP=1

          DO J=1,PCOUNT,JSTEP
C     LOAD THE XX AND YY ARRAYS FOR A SPECIFIC J VALUE
              DO K=1,PCOUNT
                  XX(K)=XPLT(K)
                  YY(K)=FPLT(K,J)
              END DO
C     NOW PLOTTING STEPS ARE 1 AND THERE ARE GOING TO BE
C     3000 POINTS PER LINE
              DO I=1,3000,1
                  XV=DBLE(I-1)
                  YV=-1.0D30
                  DO II=1,PCOUNT-1
                      XTEST1=XX(II)
                      XTEST2=XX(II+1)
                      YTEST1=YY(II)
                      YTEST2=YY(II+1)
                      IF(XV.GE.XTEST1.AND.XV.LE.XTEST2) THEN
C     CALC A VALUE FOR YV AND RETURN
                          IF((XTEST2-XTEST1).NE.0.0D0) THEN
                              SLOPE=(YTEST2-YTEST1)/(XTEST2-XTEST1)
                              YV=(SLOPE*(XV-XTEST1))+YTEST1
                          ELSE
                              YV=(YTEST2+YTEST1)/2.0D0
                          END IF
                          GO TO 2001
                      END IF
                  END DO
 2001             CONTINUE
                  IIXM1=IIX
                  IIYM1=IIY
                  IIX=INT((2750+XV)+(0.5*YPLT(J)))
                  IIY=INT((2500+YV)+(0.5*YPLT(J)))
C     IIX IS IN THE RANGE 2750 TO 7250
                  IF(IPN.EQ.0.AND.I.NE.1.AND.IIYM1.LT.IIY.AND.H(IIX).GT.-10000
     1            .AND.IIYM1.LT.H(IIXM1))
     1            CALL MY_PLOT(IIX,H(IIX),0,0,-10,10010,-10,7010)
                  IPN=0
                  IF((IIY).GE.H(IIX)) THEN
                      H(IIX)=(IIY)
                      IPN=1
                  ELSE
C     IIY LESS THAN OR EQUAL TO H(IIX)
                      IPN=0
                  END IF
                  IF(I.EQ.1) IPN=0
                  CALL MY_PLOT(IIX,IIY,IPN,0,-10,10010,-10,7010)

                  call drawdatasave(IIX,IIY,IPN,0)

                  IF(IPN.EQ.0.AND.I.EQ.1) IPN=1
              END DO
              IPN=0
          END DO
C
          IF(ROT.EQ.90) THEN
C     XAXIS

              CALL MY_PLOT(2550,2300,0,0,0,10000,0,7000)
              CALL MY_PLOT(2750,2500,1,0,0,10000,0,7000)
              CALL MY_PLOT(2750,2500,0,0,0,10000,0,7000)

C     XAXIS LABEL
              CALL MY_JUSTSTRING(2250,2000,'+X',0,2,3)
C     YAXIS

              CALL MY_PLOT(2750,2500,0,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2500,1,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2500,0,0,0,10000,0,7000)
              CALL MY_JUSTSTRING(6182,2410,'+Y',0,2,3)
          END IF
          IF(ROT.EQ.0) THEN
C     YAXIS

              CALL MY_PLOT(5750,2500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,4200,1,0,0,10000,0,7000)
              CALL MY_PLOT(7450,4200,0,0,0,10000,0,7000)

C     YAXIS LABEL
              CALL MY_JUSTSTRING(7550,4300,'+Y',0,2,3)
C     XAXIS

              CALL MY_PLOT(2750,2500,0,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2500,1,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2500,0,0,0,10000,0,7000)

C     XAXIS LABEL
              CALL MY_JUSTSTRING(6182,2410,'+X',0,2,3)
          END IF
C
          CALL PLOTBOX
C
C     NOW FOR PLOT ANNOTATION
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
          IF(STMPT) CALL MYTIME(TMY)
          IF(STMPD) CALL MYDATE(DTY)
          IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
          IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
          IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
          IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
          IF(NNTT1.NE.BLNOTE) THEN

              CALL MY_JUSTSTRING(200,650,NNTT1(1:80),NT1ANG,NT1SIZ,3)
          ELSE
C     LI BLANK, NOT ACTION
          END IF
          IF(PSFLIN.EQ.1) THEN
              IF(ROT.EQ.0) CALL MY_JUSTSTRING(5000,300,
     1        'Diffraction PSF (Intensity)',0,2,3)
              IF(ROT.EQ.90) CALL MY_JUSTSTRING(5000,300,
     1        'Diffraction PSF (Intensity)-ROTATED',0,2,3)
          ELSE
              IF(ROT.EQ.0) CALL MY_JUSTSTRING(5000,300,
     1        'Diffraction PSF (Log10 Intensity)',0,2,3)
              IF(ROT.EQ.90) CALL MY_JUSTSTRING(5000,300,
     1        'Diffraction PSF (Log10 Intensity)-ROTATED',0,2,3)
          END IF
C
C     DO THE PLOTTING OF THE EXTENT
C     FOCAL OR UFOCAL
          IF(SYSTEM1(6).EQ.1.0D0) UNN='in(s)    '
          IF(SYSTEM1(6).EQ.2.0D0) UNN='cm(s)    '
          IF(SYSTEM1(6).EQ.3.0D0) UNN='mm(s)    '
          IF(SYSTEM1(6).EQ.4.0D0) UNN='meter(s) '
          IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
C     AFOCAL OR UAFOCAL
              IF(SYSTEM1(6).EQ.1.0D0) UNN='RADIAN(S)'
          END IF
C     UNITS ARE NOW SET
C
C     NOW WRITE = "VALUE" UNN
          RANGE1=ABS(PEXTENT)
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          IF(SYSTEM1(30).LE.2.0D0)
     1    NNTT1='PLOTTED EXTENT = '//CRANGE//' '//UNN
          IF(SYSTEM1(30).GE.3.0D0)
     1    NNTT1='PLOTTED EXTENT = '//CRANGE//' '//UNN2
C
          CALL MY_JUSTSTRING(200,1200,NNTT1(1:42),NT1ANG,NT1SIZ,3)

C
C     NOW WRITE = "VALUE" UNN
          GRI=DABS(DBLE(PSPACING))
          RANGE1=ABS(PSPACING)
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          IF(SYSTEM1(30).LE.2.0D0)
     1    NNTT1='           GRI = '//CRANGE//' '//UNN
          IF(SYSTEM1(30).GE.3.0D0)
     1    NNTT1='           GRI = '//CRANGE//' '//UNN2
C
          CALL MY_JUSTSTRING(200,1000,NNTT1(1:42),NT1ANG,NT1SIZ,3)
C
C     NOW WRITE = "VALUE" UNN
          RANGE1=CNTX
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          IF(SYSTEM1(30).LE.2.0D0)
     1    NNTT1='X-CENTROID LOCATION = '//CRANGE//' '//UNN
          IF(SYSTEM1(30).GE.3.0D0)
     1    NNTT1='X-CENTROID LOCATION = '//CRANGE//' '//UNN2
C
          CALL MY_JUSTSTRING(200,1600,NNTT1(1:47),NT1ANG,NT1SIZ,3)
C     NOW WRITE = "VALUE" UNN
          RANGE1=CNTY
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          IF(SYSTEM1(30).LE.2.0D0)
     1    NNTT1='Y-CENTROID LOCATION = '//CRANGE//' '//UNN
          IF(SYSTEM1(30).GE.3.0D0)
     1    NNTT1='Y-CENTROID LOCATION = '//CRANGE//' '//UNN2
C
          CALL MY_JUSTSTRING(200,1400,NNTT1(1:47),NT1ANG,NT1SIZ,3)
C
C
C     FIELD OF VIEW DATA
C
          UNN2='RADIANS  '
          IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
C     SCX FANG
              RANGE1=SYSTEM1(23)*LFOB(2)
              UNN1='DEGREE(S)'
          ELSE
              RANGE1=SYSTEM1(16)*LFOB(2)
              UNN1(1:9)=UNN(1:9)
C     SCX
          END IF
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='OBJECT POINT-X'
C
          CALL MY_JUSTSTRING(200,6500,NNTT1(1:14),NT1ANG,NT1SIZ,3)
          NNTT1=' = '//CRANGE//' '//UNN1
C
          CALL MY_JUSTSTRING(1500,6500,NNTT1(1:28),NT1ANG,NT1SIZ,3)
C
C
          IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
C     SCY FANG
              RANGE1=SYSTEM1(21)*LFOB(1)
              UNN1='DEGREE(S)'
          ELSE
              RANGE1=SYSTEM1(14)*LFOB(1)
              UNN1(1:9)=UNN(1:9)
C     SCY
          END IF
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='OBJECT POINT-Y'
C
          CALL MY_JUSTSTRING(200,6300,NNTT1(1:14),NT1ANG,NT1SIZ,3)
          NNTT1=' = '//CRANGE//' '//UNN1
C
          CALL MY_JUSTSTRING(1500,6300,NNTT1(1:28),NT1ANG,NT1SIZ,3)
C
          WRITE(B,102) NRD
          READ(B,202) NRANGE
          DO I=1,12
              IF(NRANGE(1:1).EQ.' ') NRANGE(1:12)=NRANGE(2:12)//' '
          END DO
          NNTT1='NRD = '//NRANGE
          CALL MY_JUSTSTRING(200,6100,NNTT1(1:18),NT1ANG,NT1SIZ,3)
C
          WRITE(B,102) TGR
          READ(B,202) NRANGE
          DO I=1,12
              IF(NRANGE(1:1).EQ.' ') NRANGE(1:12)=NRANGE(2:12)//' '
          END DO
          NNTT1='TGR = '//NRANGE
          CALL MY_JUSTSTRING(200,5900,NNTT1(1:18),NT1ANG,NT1SIZ,3)
C
          WRITE(B,102) PGR
          READ(B,202) NRANGE
          DO I=1,12
              IF(NRANGE(1:1).EQ.' ') NRANGE(1:12)=NRANGE(2:12)//' '
          END DO
          NNTT1='PGR = '//NRANGE
          CALL MY_JUSTSTRING(200,5700,NNTT1(1:18),NT1ANG,NT1SIZ,3)
C
C     NOW WRITE = "VALUE" UNN
          RANGE1=PTOVOPD(0)
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='P-V OPD = '//CRANGE//' WAVES'
C
          CALL MY_JUSTSTRING(200,5500,NNTT1(1:45),NT1ANG,NT1SIZ,3)
C
C     NOW WRITE = "VALUE" UNN
          RANGE1=RMSOPD
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='RMS OPD = '//CRANGE//' WAVES'
C
          CALL MY_JUSTSTRING(200,5300,NNTT1(1:45),NT1ANG,NT1SIZ,3)
C
          WVNUMB=INT(SYSTEM1(11))
          IF(WVNUMB.GE.1.AND.WVNUMB.LE.5) RANGE1=SYSTEM1(WVNUMB)
          IF(WVNUMB.GE.6.AND.WVNUMB.LE.10) RANGE1=SYSTEM1(WVNUMB+65)
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='WAVELENGTH = '//CRANGE//' MICROMETER'
C
          CALL MY_JUSTSTRING(200,5100,NNTT1(1:40),NT1ANG,NT1SIZ,3)
c
C     DO REF SPHERE SHIFTS IF NOT ZERO
          IF(SYSTEM1(6).EQ.1.0D0) UNN='in(s)    '
          IF(SYSTEM1(6).EQ.2.0D0) UNN='cm(s)    '
          IF(SYSTEM1(6).EQ.3.0D0) UNN='mm(s)    '
          IF(SYSTEM1(6).EQ.4.0D0) UNN='meter(s) '
          IF(DABS(DLLX).GT.1.0D-15) THEN
              WRITE(B,101) DLLX
              READ(B,200) CRANGE
              NNTT1='REF. SPHERE X-SHIFT = '//CRANGE//' '//UNN
              CALL MY_JUSTSTRING(5200,1600,NNTT1(1:50),NT1ANG,NT1SIZ,3)
          ELSE
              NNTT1='REF. SPHERE CENTER NOT SHIFTED IN X'
              CALL MY_JUSTSTRING(5200,1600,NNTT1(1:50),NT1ANG,NT1SIZ,3)
          END IF
          IF(DABS(DLLY).GT.1.0D-15) THEN
              WRITE(B,101) DLLY
              READ(B,200) CRANGE
              NNTT1='REF. SPHERE Y-SHIFT = '//CRANGE//' '//UNN
              CALL MY_JUSTSTRING(5200,1400,NNTT1(1:50),NT1ANG,NT1SIZ,3)
          ELSE
              NNTT1='REF. SPHERE CENTER NOT SHIFTED IN Y'
              CALL MY_JUSTSTRING(5200,1400,NNTT1(1:50),NT1ANG,NT1SIZ,3)
          END IF
          IF(DABS(DLLZ).GT.1.0D-15) THEN
              WRITE(B,101) DLLZ
              READ(B,200) CRANGE
              NNTT1='REF. SPHERE Z-SHIFT = '//CRANGE//' '//UNN
              CALL MY_JUSTSTRING(5200,1200,NNTT1(1:50),NT1ANG,NT1SIZ,3)
          ELSE
              NNTT1='REF. SPHERE CENTER NOT SHIFTED IN Z'
              CALL MY_JUSTSTRING(5200,1200,NNTT1(1:50),NT1ANG,NT1SIZ,3)
          END IF
C
          IF(RSTREHL_EXIST) THEN
              RANGE1=RSTREHL
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='REAL STREHL RATIO = '//CRANGE
C
              CALL MY_JUSTSTRING(200,4900,NNTT1(1:40),NT1ANG,NT1SIZ,3)
              RSTREHL_EXIST=.FALSE.
          END IF
C
C     SPCTRAL WEIGHTS
          IV=WVWT(7000,6900,ERROR1)
C
C
          IF(.NOT.PSFHC) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              IF(DFLAG.EQ.0) THEN
                  IF(DFLAG.EQ.0) INPUT='DRAW'
                  call setonecolors
                  CALL PROCES

              END IF
              REST_KDP(1)=RESTINPT(1)
          END IF

101       FORMAT(1PG15.8)
102       FORMAT(I6)
200       FORMAT(A15)
202       FORMAT(A6)
          DEALLOCATE(XX,YY,X_JK,Y_JK,F_JK,XY2,H,STAT=ALLOERR)
          RETURN
      END


      SUBROUTINE FFT2(PCOUNT,XPLT,YPLT,FPLT,DFLAG)
          IMPLICIT NONE
          REAL PEXTENT,PSPACING,PEAK
          INTEGER ROT,I,J,ALLOERR,PCOUNT,DFLAG
C
          REAL FPLT,XPLT,YPLT,FTF,CNTX,CNTY,FACTORL
C
          DIMENSION FTF(:,:),FPLT(PCOUNT,PCOUNT),XPLT(PCOUNT),YPLT(PCOUNT)
          ALLOCATABLE :: FTF
C
          COMMON/PSFPLOTER/PEXTENT,PSPACING,CNTX,CNTY

          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'dathgr.inc'
          DEALLOCATE(FTF,STAT=ALLOERR)
          ALLOCATE(FTF(PCOUNT,PCOUNT),STAT=ALLOERR)
C     FIX THE ARRAY SO IT PLOTS LIKE IN CODE-V
C     WE NEED TO REFLECT IN THE Y-DIRECTION
          ROT=0
          IF(ROTPSF) ROT=90
C
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  FTF(I,J)=FPLT(PCOUNT+1-I,J)
              END DO
          END DO
          DO J=1,PCOUNT
              DO I=1,PCOUNT
                  FPLT(I,J)=FTF(I,J)
              END DO
          END DO
C     WE NEED TO REFLECT IN THE Y-DIRECTION
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  FTF(I,J)=FPLT(I,PCOUNT+1-J)
              END DO
          END DO
          DO J=1,PCOUNT
              DO I=1,PCOUNT
                  FPLT(I,J)=FTF(I,J)
              END DO
          END DO
          IF(ROT.EQ.90) THEN
C     ROTATE THE F ARRAY BY 90 DEGREES (CLOCKWISE LOOKING DOWN
C     FROM THE PLUS INTENSITY Z AXIS TOWARD THE MINUS INTENSITY
C     Z AXIS. STORE THE TEMPORARY FUNCTION IN ARRAY FTF
              DO J=1,PCOUNT
                  DO I=1,PCOUNT
                      FTF(I,J)=FPLT(PCOUNT+1-J,I)
                  END DO
              END DO
              DO J=1,PCOUNT
                  DO I=1,PCOUNT
                      FPLT(I,J)=FTF(I,J)
                  END DO
              END DO
          END IF
          PEAK=-1.0E10
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  IF(FPLT(I,J).GE.PEAK) PEAK=FPLT(I,J)
              END DO
          END DO
          IF(PEAK.EQ.0.0) PEAK=1.0
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  FPLT(I,J)=(FPLT(I,J)/PEAK)*1.9
              END DO
          END DO

          DO I=1,PCOUNT
              XPLT(I)=((XPLT(I)/.95)*1500.0)+1500.0
              YPLT(I)=((YPLT(I)/.95)*1500.0)+1500.0
          END DO
          IF(PSFLIN.EQ.1) FACTORL=2000.0
          IF(PSFLIN.EQ.0) FACTORL=1500.0
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  FPLT(I,J)=(FPLT(I,J)/1.9)*FACTORL
              END DO
          END DO
C
C     NOW FUNCTION GOES FROM 0 TO 1800 AND XPLT AND YPLT GO FROM
C     0 TO +2000 EACH
C
C     THE PLOT WILL BE SEEN IN ORTHOGRAPHIC PROJECTION, XPLT ACROSS THE SCREEN,
C     YPLT INTO THE SCREEN AT 45 DEG EL AND AZ AND FPLT UP ON THE SCREEN
C
          CALL PLOTPSF(PCOUNT,XPLT,YPLT,FPLT,ROT,DFLAG)
C
          DEALLOCATE(FTF,STAT=ALLOERR)
          RETURN
      END


C SUB DOPSF.FOR

      SUBROUTINE DOPSF
          USE GLOBALS
C
          IMPLICIT NONE
C
C     THIS IS SUBROUTINE DOPSF.FOR.
C     CALLED BY CMDER FOR COMMAND PSF
C     THIS DOES DIFFRACTION PSF CALCULATIONS
C
          CHARACTER*9 UNN,UNN1
C
          REAL*8 VALUE,VALVAL,SHTVALUE,GRNX,GRNY,XRANGE,YRANGE
     1    ,WVNUM,DIAM,OUTGRIDEXTENT,EXEX,EXEY,V1,APEAK,
     2    OUTGRIDSPACING
     3    ,CENTERX,CENTERY
C
          INTEGER IT,IJK,PCOUNT,HOLN
C
          INTEGER BINNREC,UNIT_FLAG,MODE_FLAG,IREC
C
          REAL*8 SCALE_FACTOR
C
          REAL*8 FSUM,IIII,IV,PSFXCENT,PSFYCENT
     1    ,FACTER1,SPTT,SPTOT,FTOT,PUP,DATA
     2    ,SPACER,PEAKER,CRAYX,CRAYY,IWLIJK(1:10)
     3    ,F,FHOLDF,FHOLDF1,PEAKADJ
C
          COMMON/PEPITO/IWLIJK,IJK
C
          REAL SPACING,EXTENT,XPLT,CNTX,CNTY,PEXTENT,PSPACING
     1    ,YPLT,FPLT
          DIMENSION XPLT(:),YPLT(:),FPLT(:,:)
          ALLOCATABLE :: XPLT,YPLT,FPLT
C
          DIMENSION F(:,:),DATA(:),PUP(:,:,:)
     1    ,FHOLDF(:,:),FHOLDF1(:,:)
     2    ,CENTERX(:),CENTERY(:)
          ALLOCATABLE :: F,DATA,PUP,FHOLDF,FHOLDF1
     1    ,CENTERX,CENTERY
C
          LOGICAL EXIS51,OPEN51,NOCOBSPSF
C
          LOGICAL EXIS106,OPEN106
C
          COMMON/PSFCOBS/NOCOBSPSF
C
          LOGICAL ERR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          INTEGER IREAL,IIMAG,INDEX,SHTNM,IX,IY,II,I,J,NDAT,NDIM,
     1    MM,III,JJJ,IIX,IIY,ALLOERR,HI,MMM,IIIX,IIIY,DFLAG
C
!      LOGICAL ERRR,ERRFOB
C
          COMMON/PSFPLOTER/PEXTENT,PSPACING,CNTX,CNTY
C
          DEALLOCATE(F,PUP,FHOLDF,FHOLDF1,FIMG,STAT=ALLOERR)
          GPREG(101:110)=0.0D0
C
          PSFWV(1)=SYSTEM1(1)
          PSFWV(2)=SYSTEM1(2)
          PSFWV(3)=SYSTEM1(3)
          PSFWV(4)=SYSTEM1(4)
          PSFWV(5)=SYSTEM1(5)
          PSFWV(6)=SYSTEM1(71)
          PSFWV(7)=SYSTEM1(72)
          PSFWV(8)=SYSTEM1(73)
          PSFWV(9)=SYSTEM1(74)
          PSFWV(10)=SYSTEM1(75)
C
C     M IS THE DIMENSION OF THE REQUESTED GRID OVER THE PUPIL
C     THE TRANSFORM IS TWICE THIS BIG
          HI=0
          MM=TGR
          MMM=MM-1
          NDAT=MM*MM*2
          NDIM=2
C
C     HERE IS WHERE WE BUILD THE PSF
          IF(S1.EQ.1) DFLAG=1
          IF(S1.EQ.0) DFLAG=0
C
          IF(.NOT.CPFNEXT) THEN
              OUTLYNE='NO COMPLEX APERTURE FUNCTION EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO PSF CALCULATION IS POSSIBLE'
              CALL SHOWIT(1)
              CALL MACFAL
              CALL DELPSF
              RETURN
          END IF
C     CALC SPTOT
          SPTOT=0.0D0
          SPTOT=SPTOT+(SYSTEM1(31))
          SPTOT=SPTOT+(SYSTEM1(32))
          SPTOT=SPTOT+(SYSTEM1(33))
          SPTOT=SPTOT+(SYSTEM1(34))
          SPTOT=SPTOT+(SYSTEM1(35))
          SPTOT=SPTOT+(SYSTEM1(76))
          SPTOT=SPTOT+(SYSTEM1(77))
          SPTOT=SPTOT+(SYSTEM1(78))
          SPTOT=SPTOT+(SYSTEM1(79))
          SPTOT=SPTOT+(SYSTEM1(80))
C
C     NOW COMPUTE THE SHORTEST WAVELENGTH IN CURENT LENS UNITS
C     CALC THE SHRTWAVE
          SHRTWAVE=0.0D0
          IF(SYSTEM1(31).NE.0.0D0.AND.SYSTEM1(1).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(1)
              GO TO 314
          END IF
          IF(SYSTEM1(32).NE.0.0D0.AND.SYSTEM1(2).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(2)
              GO TO 314
          END IF
          IF(SYSTEM1(33).NE.0.0D0.AND.SYSTEM1(3).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(3)
              GO TO 314
          END IF
          IF(SYSTEM1(34).NE.0.0D0.AND.SYSTEM1(4).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(4)
              GO TO 314
          END IF
          IF(SYSTEM1(35).NE.0.0D0.AND.SYSTEM1(5).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(5)
              GO TO 314
          END IF
          IF(SYSTEM1(76).NE.0.0D0.AND.SYSTEM1(71).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(71)
              GO TO 314
          END IF
          IF(SYSTEM1(77).NE.0.0D0.AND.SYSTEM1(72).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(72)
              GO TO 314
          END IF
          IF(SYSTEM1(78).NE.0.0D0.AND.SYSTEM1(73).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(73)
              GO TO 314
          END IF
          IF(SYSTEM1(79).NE.0.0D0.AND.SYSTEM1(74).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(74)
              GO TO 314
          END IF
          IF(SYSTEM1(80).NE.0.0D0.AND.SYSTEM1(75).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(75)
              GO TO 314
          END IF
 314      CONTINUE
          IF(SHRTWAVE.EQ.0.0D0) THEN
              OUTLYNE='WAVELENGTHS ARE ALL ZERO'
              CALL SHOWIT(1)
              OUTLYNE='NO "PSF" CAN BE CALCULATED'
              CALL SHOWIT(1)
              CALL MACFAL
              CALL DELPSF
              RETURN
          END IF
C
          IF(SYSTEM1(31).NE.0.0D0.AND.SYSTEM1(1).LE.SHRTWAVE
     1    .AND.SYSTEM1(1).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(1)
              SHTNM=1
          END IF
          IF(SYSTEM1(32).NE.0.0D0.AND.SYSTEM1(2).LE.SHRTWAVE
     1    .AND.SYSTEM1(2).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(2)
              SHTNM=2
          END IF
          IF(SYSTEM1(33).NE.0.0D0.AND.SYSTEM1(3).LE.SHRTWAVE
     1    .AND.SYSTEM1(3).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(3)
              SHTNM=3
          END IF
          IF(SYSTEM1(34).NE.0.0D0.AND.SYSTEM1(4).LE.SHRTWAVE
     1    .AND.SYSTEM1(4).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(4)
              SHTNM=4
          END IF
          IF(SYSTEM1(35).NE.0.0D0.AND.SYSTEM1(5).LE.SHRTWAVE
     1    .AND.SYSTEM1(5).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(5)
              SHTNM=5
          END IF
          IF(SYSTEM1(76).NE.0.0D0.AND.SYSTEM1(71).LE.SHRTWAVE
     1    .AND.SYSTEM1(71).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(71)
              SHTNM=6
          END IF
          IF(SYSTEM1(77).NE.0.0D0.AND.SYSTEM1(72).LE.SHRTWAVE
     1    .AND.SYSTEM1(72).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(72)
              SHTNM=7
          END IF
          IF(SYSTEM1(78).NE.0.0D0.AND.SYSTEM1(73).LE.SHRTWAVE
     1    .AND.SYSTEM1(73).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(73)
              SHTNM=8
          END IF
          IF(SYSTEM1(79).NE.0.0D0.AND.SYSTEM1(74).LE.SHRTWAVE
     1    .AND.SYSTEM1(74).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(74)
              SHTNM=9
          END IF
          IF(SYSTEM1(80).NE.0.0D0.AND.SYSTEM1(75).LE.SHRTWAVE
     1    .AND.SYSTEM1(75).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(75)
              SHTNM=10
          END IF
C     SHRTWAVE IS IN MICROMETER, CHANGE TO LENS UNITS
          VALUE=SHRTWAVE
          IF(SYSTEM1(6).EQ.1.0D0) SHTVALUE=((VALUE*1.0D-3)/25.4D0)
          IF(SYSTEM1(6).EQ.2.0D0) SHTVALUE=(VALUE*1.0D-4)
          IF(SYSTEM1(6).EQ.3.0D0) SHTVALUE=(VALUE*1.0D-3)
          IF(SYSTEM1(6).EQ.4.0D0) SHTVALUE=(VALUE*1.0D-6)
C
C     WE NOW HAVE THE WAVELENGTH AND WAVELENGTH NUMBER OF THE
C     SHORTEST WAVELENGTH FOR WHICH THE SPECTRAL WEIGHTING FACTOR
C     IS NON-ZERO. THESE ARE SHTVALUE AND SHTNM
C
C     TGR ALWAYS RULES AND IS NOT CHANGED.
C
C     NOW COMPUTE THE PUPIL GRID SPACING AND EXTENT
C     THIS SETS THE GRID TO BE USED FOR THE PSF
C     IF THE MODE IS FOCAL OR UFOCAL
C     WE WANT THE ENTRANCE PUPIL DIAMETERS IN THE X AND Y DIRECTIONS
C     ENTRANCE PUPIL DIAMETERS IN THE X AND Y DIRECTION AT THE SHORT WAVELENGTH
C     THE LARGER OF THE TWO ENTRANCE PUPIL DIAMETERS WILL ACT AS THE
C     DIMENSION FOR COMPUTING THE PUPIL GRID SPACING AND EXTENT. THIS
C     IS CONSISTENT WITH THE WAY THE GRID IS SET IN THE CAPFN RAYTRACE
C     GRID IN COMPAP.FOR. THESE ARE THE UNVIGNETTED ENTRANCE PUPIL
C     VALUES. VIGNETTING IS ACCOUNTED FOR IN THE PSF RAY TRACE.
C
          IF(SYSTEM1(30).LE.2.0D0) THEN
              IF(REFEXT) THEN
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL FNUMX(VALVAL,ERR)
                  IF(.NOT.ERR) GRNX=VALVAL
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL FNUMY(VALVAL,ERR)
                  IF(.NOT.ERR) GRNY=VALVAL
              ELSE
                  GRNX=RBFNX
                  GRNY=RBFNY
              END IF
          ELSE
C     AFOCAL
              IF(REFEXT) THEN
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL EXPDIAX(VALVAL,ERR)
                  IF(.NOT.ERR) EXEX=VALVAL
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL EXPDIAY(VALVAL,ERR)
                  IF(.NOT.ERR) EXEY=VALVAL
              ELSE
                  EXEX=EXDIAX
                  EXEY=EXDIAY
              END IF
          END IF
C
          IF(SYSTEM1(30).LE.2.0D0) THEN
C     FOCAL
              DIAM=GRNX
              IF(GRNY.LT.GRNX) DIAM=GRNY
              IF(DIAM.EQ.0.0D0) THEN
                  OUTLYNE='F/NUMBER WAS ZERO, NO PSF CALCULATION POSSIBLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  CALL DELPSF
                  RETURN
              END IF
          ELSE
C     AFOCAL
              DIAM=EXEX
              IF(EXEY.GT.EXEX) DIAM=EXEY
              IF(DIAM.EQ.0.0D0) THEN
                  OUTLYNE=
     1            'EXIT PUPIL DIAMETER WAS ZERO, NO PSF CALCULATION POSSIBLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  CALL DELPSF
                  RETURN
              ELSE
C     MAKE THE SYSTEM HAVE A 100 UNIT FOCAL LENGTH
                  DIAM=1.0/DIAM
              END IF
          END IF

          IF(GRIFLG.EQ.1)
     1    OUTGRIDEXTENT=DIAM*(NNRD-1.0D0)*SHTVALUE
          IF(GRIFLG.EQ.0)
     1    OUTGRIDEXTENT=DIAM*(DBLE(NRD)-1.0D0)*SHTVALUE
          OUTGRIDSPACING=OUTGRIDEXTENT/DBLE(TGR-1)
          GRI=OUTGRIDSPACING
C
C     NOW PROCESS THE CAPFN DATA INTO PSF DATA
C
          DEALLOCATE(F,FHOLDF,PUP,FIMG,STAT=ALLOERR)
          ALLOCATE(PUP(MM,MM,NDIM),STAT=ALLOERR)
          ALLOCATE(F(MMM+1,MMM+1),FHOLDF(MMM+1,MMM+1),FIMG(MMM+1,MMM+1)
     1    ,STAT=ALLOERR)
C
          IWIW=IW**2
          I=2
C
          III=MMM
          JJJ=MMM
          FHOLDF(1:III,1:JJJ)=0.0D0
          DO J=1,NUMCOL
              III=MM
              JJJ=MM
              PUP(1:III,1:JJJ,1:2)=0.0D0
              III=MMM
              JJJ=MMM
              F(1:III,1:JJJ)=0.0D0
C     DOING A COLOR NOW
              IX=0
              IY=1
              II=0

C     WE WANT TO DO IWIW READS AND LOADS OF THE A ARRAY
 10           II=II+1
              IX=IX+1
              IF(IX.GT.IW) THEN
                  IY=IY+1
                  IX=1
              END IF
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=I-1
              CALL SPOTIT(4)
              SPTT=(DSPOT(17))
              I=I+1
C     LOAD AN ARRAY ELEMENT WITH REAL AND IMAGINARY CAPFN
C     FOR A COLOR
C     COL,ROW INDEXED AND ZERO PADDED, THAT IS WHY THE IIX AND IIY
C     ARE NEEDED

              IIIX=((MM/2)-((NRD)/2))
              IIIY=((MM/2)-((NRD)/2))
              IF(IIIX.LT.0) IIIX=0
              IF(IIIY.LT.0) IIIY=0
              IIX=IX+IIIX
              IIY=IY+IIIY
              IF(DSPOT(12).NE.0.0D0) THEN
              END IF
C     REAL PART
              DSPOT(12)=DSPOT(12)*W3
              DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
              PUP(IIX,IIY,1)=
     1        DSQRT(DABS(DSPOT(12)))*DCOS(DSPOT(4))
C     IMAGINARY PART
              PUP(IIX,IIY,2)=
     1        -DSQRT(DABS(DSPOT(12)))*DSIN(DSPOT(4))
              WVNUM=DSPOT(16)
C
              IF(II.LT.IWIW) GO TO 10
C     FELL THROUGH, FINISHED READING IWIW POINTS
C     NOW SET UP THE CALL TO THE FFT ROUTINE AND DO THE FFT
C
C     NOW DO THE PSF
C     CHANGE EVERY OTHER SIGN SO WE DON'T NEED TO INVERT THE ANSWER
              DO III=1,MM
                  DO JJJ=1,MM
                      IT=0
                      IF(
     1                (((INT(DBLE(III+JJJ)/2.0D0))*2)-(III+JJJ)).NE.0
     2                ) IT=1
                      IF(IT.NE.0) PUP(III,JJJ,1)=-PUP(III,JJJ,1)
                      IF(IT.NE.0) PUP(III,JJJ,2)=-PUP(III,JJJ,2)
                  END DO
              END DO
C     NOW ORDER THE PUP ARRAY INTO THE DATA ARRAY
              DEALLOCATE(DATA,STAT=ALLOERR)
              ALLOCATE(DATA(NDAT),STAT=ALLOERR)
              DATA(1:NDAT)=0.0D0
              DO III = 1, MM
                  DO JJJ = 1, MM
                      INDEX = ( (III-1) * MM ) + JJJ
                      IREAL = 2*INDEX-1
                      IIMAG = 2*INDEX
                      DATA(IREAL) = PUP(III,JJJ,1)
                      DATA(IIMAG) = PUP(III,JJJ,2)
                  END DO
              END DO
C     NOW DO THE FFT
              CALL FOURN(DATA,MM,NDAT)
C     NOW DO THE SIMPLE REORDER WITHOUT INVERSION
              DO  III = 1, MM
                  DO  JJJ = 1, MM
                      INDEX = (JJJ-1)*MM+III
                      IREAL=INDEX*2-1
                      IIMAG=INDEX*2
                      PUP(III,JJJ,1)  = DATA(IREAL)
                      PUP(III,JJJ,2)  = DATA(IIMAG)
                  END DO
              END DO
              DEALLOCATE(DATA,STAT=ALLOERR)
              IF(SYSTEM1(30).LE.2.0D0) THEN
C       FOCAL
                  EXTENT=SNGL(OUTGRIDEXTENT)
                  SPACING=SNGL(OUTGRIDSPACING)
              ELSE
C       AFOCAL
                  EXTENT=SNGL(OUTGRIDEXTENT)
                  SPACING=SNGL(OUTGRIDSPACING)
              END IF
              PEXTENT=EXTENT*REAL(PGR)/REAL(TGR-1)
              PSPACING=SPACING
C     FILL UP THE REAL INTENSITY PSF ARRAY F
              DO III=2,MM
                  DO JJJ=2,MM
                      F(III-1,JJJ-1)=
     2                ((PUP(III,JJJ,1)**2)+(PUP(III,JJJ,2)**2))
                  END DO
              END DO
C
              DEALLOCATE(FHOLDF1,STAT=ALLOERR)
              ALLOCATE(FHOLDF1(MMM+1,MMM+1),STAT=ALLOERR)
              DO JJJ=1,MMM
                  DO III=1,MMM
                      FHOLDF1(III,JJJ)=F(MMM+1-JJJ,III)
                  END DO
              END DO
              DO JJJ=1,MMM
                  DO III=1,MMM
                      F(III,JJJ)=FHOLDF1(III,JJJ)
                  END DO
              END DO
              DEALLOCATE(FHOLDF1,STAT=ALLOERR)
C     REFLEXT IN Y
              ALLOCATE(FHOLDF1(MMM+1,MMM+1),STAT=ALLOERR)
              DO JJJ=1,MMM
                  DO III=1,MMM
                      FHOLDF1(III,JJJ)=F(III,MMM+1-JJJ)
                  END DO
              END DO
              DO JJJ=1,MMM
                  DO III=1,MMM
                      F(III,JJJ)=FHOLDF1(III,JJJ)
                  END DO
              END DO
              DEALLOCATE(FHOLDF1,STAT=ALLOERR)
C
C     IF WAVELENGTH IS SHORTEST, STORE THE PSF IN THE ARRAY
C     FHOLDF AFTER MULTIPLYING THE INTENSITY BY THE FRACTIONAL
C     SPECTRAL WEIGHT.
              IF(SHTNM.EQ.INT(WVNUM).AND.HI.EQ.0) THEN
                  HI=HI+1
C     AT SHORT WAVELENGTH FIRST TIME
C
C     NOW SCALE THE PSF AT CURRENT LAMBDA TO A PEAK OF 1.0
                  PEAKER=-1.0D300
                  DO III=1,MMM
                      DO JJJ=1,MMM
                          IF(F(III,JJJ).GT.PEAKER) PEAKER=F(III,JJJ)
                      END DO
                  END DO
                  GPREG(INT(WVNUM)+100)=PEAKER
                  IF(PEAKER.EQ.0.0D0) PEAKER=1.0D0
C     MULT BY SPECTRAL WEIGHT AND STORE IN FHOLDF
                  FACTER1=SPTT/SPTOT
                  DO III=1,MMM
                      DO JJJ=1,MMM
                          FHOLDF(III,JJJ)=FACTER1*F(III,JJJ)
                      END DO
                  END DO
              ELSE
C     NOT AT SHORTEST WAVELENGTH, INTERPOLATION NEEDED
C
C
C     NOW SCALE THE PSF AT CURRENT LAMBDA TO A PEAK OF 1.0
                  PEAKER=-1.0D300
                  DO III=1,MMM
                      DO JJJ=1,MMM
                          IF(F(III,JJJ).GT.PEAKER) PEAKER=F(III,JJJ)
                      END DO
                  END DO
                  PEAKADJ=(PSFWV(INT(WVNUM))/SHRTWAVE)**2

                  IF(PEAKER.EQ.0.0D0) PEAKER=1.0D0
                  GPREG(INT(WVNUM)+100)=PEAKER*PEAKADJ
C
                  FACTER1=(SPTT/SPTOT)*PEAKADJ
C     NRD CHANGES IN COMPAP FOR PSF BUT NOT DOR CAPFN OR DOTF
C       NO PSF INTERPOLATION NECESSARY
C
C     NOW STORE/ADD WITH SPECTRAL WEIGHTING FACTORS INTO FHOLDF
                  DO III=1,MMM
                      DO JJJ=1,MMM
                          FHOLDF(III,JJJ)=FHOLDF(III,JJJ)+(FACTER1*F(III,JJJ))
                      END DO
                  END DO
C
              END IF
C     THE ARRAYS FOR A COLOR ARE WRITTEN, NOW SET UP THE X AND Y ARRAYS
          END DO
C     NOW COPY BACK TO F
          DEALLOCATE(FIMG,STAT=ALLOERR)
          ALLOCATE(FIMG(MMM+1,MMM+1),STAT=ALLOERR)
          FIMG(1:MMM+1,1:MMM+1)=0.0D0
          PEAKER=-1.0D300
          DO III=1,MMM
              DO JJJ=1,MMM
                  IF(FHOLDF(III,JJJ).GE.PEAKER) PEAKER=FHOLDF(III,JJJ)
                  F(III,JJJ)=FHOLDF(III,JJJ)
              END DO
          END DO
          FIMG(1:MMM,1:MMM)=F(1:MMM,1:MMM)/PEAKER
          WRITE(OUTLYNE,*) 'PSF PEAK VALUE = ',PEAKER
          CALL SHOWIT(0)
          APEAK=PEAKER
          IF(SYSTEM1(30).LE.2.0D0) THEN
              XCENTOFF=NINT(REFRY(1,INT(SYSTEM1(20)))/GRI)
              YCENTOFF=NINT(REFRY(2,INT(SYSTEM1(20)))/GRI)
          ELSE
              V1=REFRY(4,INT(SYSTEM1(20)))/REFRY(6,INT(SYSTEM1(20)))
              XCENTOFF=NINT(DATAN(V1)/GRI)
              V1=REFRY(5,INT(SYSTEM1(20)))/REFRY(6,INT(SYSTEM1(20)))
              YCENTOFF=NINT(DATAN(V1)/GRI)
          END IF
          WRITE(OUTLYNE,*)'CHIEF RAY X-OFFSET BY: ',XCENTOFF,' GRI UNITS'
          CALL SHOWIT(0)
          WRITE(OUTLYNE,*)'CHIEF RAY Y-OFFSET BY: ',YCENTOFF,' GRI UNITS'
          CALL SHOWIT(0)
          MMMIMG=MMM
          GRIIMG=GRI
          PGRIMG=PGR
          DEALLOCATE(FHOLDF,STAT=ALLOERR)
C
C     F IS THE FULL INTENSITY PSF FILE ON THE TGRxTGR GRID AFTER ALL
C     COLORS HAVE BEEN ADDED IN AND ON THE ORIGINAL GRI GRID SPACING
C     WHICH WAS SET BY THE VALUES OF NRD AND TGR
          CENTERVAL=F(((TGR/2)-1),((TGR/2)-1))
C
          PEAKER=-1.0D300
          DO JJJ=1,MMM
              DO III=1,MMM
                  IF(F(III,JJJ).GT.PEAKER) PEAKER=F(III,JJJ)
              END DO
          END DO
          SCALE_FACTOR=PEAKER
          DO JJJ=1,MMM
              DO III=1,MMM
                  IF(PEAKER.NE.0.0D0) F(III,JJJ)=DNINT((F(III,JJJ)/PEAKER)
     1            *32767.0D0)
                  IF(PEAKER.EQ.0.0D0) F(III,JJJ)=DNINT(F(III,JJJ))
              END DO
          END DO
          IF(PSFLIN.EQ.0) THEN
              DO III=1,MMM
                  DO JJJ=1,MMM
C     LOG REPRESENTATION
                      IF(F(III,JJJ).LT.(10.0D0**(-PSFLOG)))
     1                F(III,JJJ)=(10.0D0**(-PSFLOG))
                      F(III,JJJ)=(DBLE(PSFLOG)+DLOG10(F(III,JJJ)))/DBLE(PSFLOG)
                  END DO
              END DO
          END IF

          DEALLOCATE(CENTERX,CENTERY,STAT=ALLOERR)
          ALLOCATE(CENTERX(PGR),CENTERY(PGR),STAT=ALLOERR)
          CENTERX(1:PGR)=0.0D0
          CENTERY(1:PGR)=0.0D0
C     CALCULATE THE TRUE PSF X-CENTROID POSITION
          PSFXCENT=0.0D0
          IIII=0.0D0
          IV=0.0D0
          FTOT=0.0D0
          DO III=1,PGR
              IIII=IIII+1.0D0
              IV=IV+IIII
              FSUM=0.0D0
              DO JJJ=1,PGR
                  FSUM=FSUM+
     1            F((((TGR-1)-PGR)/2)+III,(((TGR-1)-PGR)/2)+JJJ)
              END DO
              FTOT=FTOT+FSUM
              CENTERX(III)=FSUM
              PSFXCENT=PSFXCENT+(IIII*FSUM)
          END DO
          PSFXCENT=-((PSFXCENT/FTOT)-(IV/DBLE(PGR)))
C     CALCULATE THE TRUE PSF Y-CENTROID POSITION
          PSFYCENT=0.0D0
          IIII=0.0D0
          IV=0.0D0
          FTOT=0.0D0
          DO JJJ=1,PGR
              IIII=IIII+1.0D0
              IV=IV+IIII
              FSUM=0.0D0
              DO III=1,PGR
                  FSUM=FSUM+
     1            F((((TGR-1)-PGR)/2)+III,(((TGR-1)-PGR)/2)+JJJ)
              END DO
              FTOT=FTOT+FSUM
              CENTERY(JJJ)=FSUM
              PSFYCENT=PSFYCENT+(IIII*FSUM)
          END DO
          PSFYCENT=-((PSFYCENT/FTOT)-(IV/DBLE(PGR)))
C
          REG(40)=REG(9)
          REG(10)=0.0D0
          REG(9)=0.0D0
          REG(10)=PSFYCENT*DBLE(SPACING)
          REG(9)=PSFXCENT*DBLE(SPACING)
C
          CNTY=SNGL(PSFYCENT)*SPACING
          CNTX=SNGL(PSFXCENT)*SPACING
          IF(ABS(CNTY).LT.1.0E-8) CNTY=0.0
          IF(ABS(CNTX).LT.1.0E-8) CNTX=0.0
          CRAYX=(REFRY(1,NEWIMG))
          CRAYY=(REFRY(2,NEWIMG))
C
          PSFEXT=.TRUE.
          CPFNEXT=.TRUE.
C
C
C     IF PSFWRITE IS SET TO "ON", (DEFAULT IS
C     "ON") THEN THE PSF FILE IS PEAK NORMALIZED TO 32767 AND THEN IS
C     WRITTEN IN 2I4,I8 FORMAT [III,JJJ,F(III,JJJ)]
          IF(.NOT.PSFWRITE) THEN
              EXIS51=.FALSE.
              OPEN51=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',OPENED=OPEN51)
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
              IF(OPEN51.AND.EXIS51) CALL CLOSE_FILE(51,0)
              EXIS51=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
              IF(EXIS51) THEN
                  OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//'PSF.DAT'
     2              ,STATUS='UNKNOWN')
                  CALL CLOSE_FILE(51,0)
              END IF
          END IF
          IF(PSFWRITE) THEN
C     WRITE FILE
              EXIS51=.FALSE.
              OPEN51=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',OPENED=OPEN51)
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
              IF(OPEN51.AND.EXIS51) CALL CLOSE_FILE(51,0)
              EXIS51=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
              IF(EXIS51) THEN
                  OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//'PSF.DAT'
     2              ,STATUS='UNKNOWN')
                  CALL CLOSE_FILE(51,0)
              END IF
C     OPEN NEW FILE
              OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1          ,FORM='FORMATTED',FILE=trim(HOME)//'PSF.DAT'
     2          ,STATUS='UNKNOWN')
              PEAKER=-1.0D300
              DO JJJ=1,MMM
                  DO III=1,MMM
                      IF(F(III,JJJ).GT.PEAKER) PEAKER=F(III,JJJ)
                  END DO
              END DO
C     WRITE FILE HEADER
              DEALLOCATE(IPSF1,IPSF2,IPSF3,STAT=ALLOERR)
              WRITE(OUTLYNE,*) 'WRITING PSF TO FILE'
              CALL SHOWIT(1)
              WRITE(51,1001) PSFTAG
 1001         FORMAT(A12)
              WRITE(51,1002) PSFLI
 1002         FORMAT(A78)
              WRITE(51,1003) MMM,PGR
 1003         FORMAT(I10,1X,I10)
              IF(SYSTEM1(6).EQ.1.0D0) WRITE(51,1004)
              IF(SYSTEM1(6).EQ.2.0D0) WRITE(51,1005)
              IF(SYSTEM1(6).EQ.3.0D0) WRITE(51,1006)
              IF(SYSTEM1(6).EQ.4.0D0) WRITE(51,1007)
 1004         FORMAT('IN')
 1005         FORMAT('CM')
 1006         FORMAT('MM')
 1007         FORMAT('M')
              WRITE(51,1008) EXTENT,SPACING
              WRITE(51,1008) PSFXCENT*DBLE(SPACING),PSFYCENT*DBLE(SPACING)


              IF(SYSTEM1(6).EQ.1.0D0) UNN='in(s)    '
              IF(SYSTEM1(6).EQ.2.0D0) UNN='cm(s)    '
              IF(SYSTEM1(6).EQ.3.0D0) UNN='mm(s)    '
              IF(SYSTEM1(6).EQ.4.0D0) UNN='meter(s) '
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
C     SCY FANG
                  YRANGE=SYSTEM1(21)*LFOB(1)
                  UNN1='DEGREE(S)'
C     SCX FANG
                  XRANGE=SYSTEM1(23)*LFOB(2)
                  UNN1='DEGREE(S)'
              ELSE
                  XRANGE=SYSTEM1(16)*LFOB(2)
                  UNN1(1:9)=UNN(1:9)
C     SCX
                  YRANGE=SYSTEM1(14)*LFOB(1)
                  UNN1(1:9)=UNN(1:9)
C     SCY
              END IF
              WRITE(51,1008) XRANGE,YRANGE,UNN1
              ALLOCATE(IPSF1((MMM+1)**2),IPSF2((MMM+1)**2),IPSF3((MMM+1)**2),
     1        STAT=ALLOERR)
 1008         FORMAT(E15.8,1x,E15.8,1X,A9)
              II=1
              DO JJJ=1,MMM
                  DO III=1,MMM
                      IPSF1(II)=III
                      IPSF2(II)=JJJ
                      IPSF3(II)=INT(F(III,JJJ))
                      II=II+1
                      WRITE(51,3001) III,JJJ,INT(F(III,JJJ))
! 2000 FORMAT(2I4,I8)
 3000                 FORMAT(I4,D23.15)
 3001                 FORMAT(I4,I4,I8)
                  END DO
              END DO
              PEAKER=-1.0D300
              DO III=1,PGR
                  IF(CENTERX(III).GT.PEAKER) PEAKER=CENTERX(III)
              END DO
              DO III=1,PGR
                  WRITE(51,3000) III, ((CENTERX(III)/PEAKER)*1.0D0)
              END DO
              PEAKER=-1.0D300
              DO III=1,PGR
                  IF(CENTERY(III).GT.PEAKER) PEAKER=CENTERY(III)
              END DO
              DO III=1,PGR
                  WRITE(51,3000) III, ((CENTERY(III)/PEAKER)*1.0D0)
              END DO
              WRITE(51,1010) APEAK
 1010         FORMAT(D23.15)
              WRITE(51,1009)
 1009         FORMAT('EOF')
              DEALLOCATE(CENTERX,CENTERY,STAT=ALLOERR)
          END IF
C
C
C     IF PSFBIN IS SET TO "OFF", (DEFAULT IS
C     "OFF")
          IF(.NOT.PSFBIN) THEN
              EXIS106=.FALSE.
              OPEN106=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSFBIN.DAT',OPENED=OPEN106)
              INQUIRE(FILE=trim(HOME)//'PSFBIN.DAT',EXIST=EXIS106)
              IF(OPEN106.AND.EXIS106) CALL CLOSE_FILE(106,0)
              EXIS106=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSFBIN.DAT',EXIST=EXIS106)
              IF(EXIS106) THEN
                  OPEN(UNIT=106,ACCESS='DIRECT',FILE=trim(HOME)//'PSFBIN.DAT',
     1              FORM='UNFORMATTED',RECL=(20*NRECL),STATUS='UNKNOWN')
                  CALL CLOSE_FILE(106,0)
              END IF
          END IF
          IF(PSFBIN) THEN
C     WRITE FILE
              EXIS106=.FALSE.
              OPEN106=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSFBIN.DAT',OPENED=OPEN106)
              INQUIRE(FILE=trim(HOME)//'PSFBIN.DAT',EXIST=EXIS106)
              IF(OPEN106.AND.EXIS106) CALL CLOSE_FILE(106,0)
              EXIS106=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PSFBIN.DAT',EXIST=EXIS106)
              IF(EXIS106) THEN
                  OPEN(UNIT=106,ACCESS='DIRECT',FILE=trim(HOME)//'PSFBIN.DAT',
     1              FORM='UNFORMATTED',RECL=(20*NRECL),STATUS='UNKNOWN')
                  CALL CLOSE_FILE(106,0)
              END IF
C     OPEN NEW FILE
              OPEN(UNIT=106,ACCESS='DIRECT',FILE=trim(HOME)//'PSFBIN.DAT',
     1          FORM='UNFORMATTED',RECL=(20*NRECL),STATUS='UNKNOWN')
C
              BINNREC=((TGR-1)**2)+4
C     WRITE NUMBER OF RECORDS
              WRITE(UNIT=106,REC=1) BINNREC
C
C     WRITE TGR-1,GRI,SCALE_FACTOR,UNIT_FLAG,MODE_FLAG
              UNIT_FLAG=INT(SYSTEM1(6))
              IF(SYSTEM1(30).LE.2.0D0) THEN
                  MODE_FLAG=1
              ELSE
                  MODE_FLAG=2
              END IF
              WRITE(UNIT=106,REC=2) TGR-1,GRI,
     1         32767.0D0/SCALE_FACTOR,
     2         UNIT_FLAG,MODE_FLAG
C
C     WRITE WAVELENGTHS
              WRITE(UNIT=106,REC=3)
     1        SYSTEM1(1),SYSTEM1(2),SYSTEM1(3),SYSTEM1(4),SYSTEM1(5),
     1        SYSTEM1(71),SYSTEM1(72),SYSTEM1(73),SYSTEM1(74),SYSTEM1(75)
C
C     WRITE WAVELENGTH WEIGHTS
              WRITE(UNIT=106,REC=4)
     1        SYSTEM1(31),SYSTEM1(32),SYSTEM1(33),SYSTEM1(34),SYSTEM1(35),
     1        SYSTEM1(76),SYSTEM1(77),SYSTEM1(78),SYSTEM1(79),SYSTEM1(80)
              IREC=4
              DO JJJ=1,MMM
                  DO III=1,MMM
                      IREC=IREC+1
                      WRITE(UNIT=106,REC=IREC) III,JJJ,F(III,JJJ)*SCALE_FACTOR
                  END DO
              END DO

              CALL CLOSE_FILE(106,1)
C
          END IF
C
          IF(PSFPLOT) THEN

              DEALLOCATE(XPLT,YPLT,FPLT,STAT=ALLOERR)
              ALLOCATE(XPLT(PGR),YPLT(PGR),FPLT(PGR,PGR),STAT=ALLOERR)
              PCOUNT=PGR
C
C     SET UP THE XPLT AND YPLT ARRAYS TO PGRxPGR
              YPLT(1)=-1.0
              XPLT(1)=-1.0
              SPACER=2.0D0/DBLE(PGR-1)
              DO III=2,PCOUNT
                  XPLT(III)=XPLT(III-1)+SNGL(SPACER)
                  YPLT(III)=YPLT(III-1)+SNGL(SPACER)
              END DO
              DO III=1,PCOUNT
                  XPLT(III)=XPLT(III)*0.95
                  YPLT(III)=YPLT(III)*0.95
              END DO
C
              HOLN=((TGR-1)-PGR)/2
              DO III=1,PCOUNT
                  DO JJJ=1,PCOUNT
                      FPLT(III,JJJ)=SNGL(F(III+HOLN,JJJ+HOLN))
                  END DO
              END DO
C
              PEAKER=-1.0E36
              DO III=1,PCOUNT
                  DO JJJ=1,PCOUNT
                      IF(FPLT(III,JJJ).GT.PEAKER) PEAKER=FPLT(III,JJJ)
                  END DO
              END DO
              DO III=1,PCOUNT
                  DO JJJ=1,PCOUNT
                      FPLT(III,JJJ)=FPLT(III,JJJ)/REAL(PEAKER)
                  END DO
              END DO
C
C     DEALLOCATE FHOLDF
              DEALLOCATE (FHOLDF,STAT=ALLOERR)
C
C     PLOT FUNCTION
              DEALLOCATE (F,STAT=ALLOERR)
C
              IF(PSFEXT) THEN
                  CALL FFT2(PCOUNT,XPLT,YPLT,FPLT,DFLAG)
              END IF
              DEALLOCATE (FPLT,XPLT,YPLT,STAT=ALLOERR)
          END IF
C
          RETURN
      END
C SUB DOPUPIL.FOR

      SUBROUTINE DOPUPIL(KKK)
          USE GLOBALS
C
          IMPLICIT NONE
C
C     THIS IS SUBROUTINE DOPUPIL.FOR.
C     CALLED BY CMDER FOR COMMAND PUPIL IN FOE
C     THIS CREATES A PUPIL FUNCTION
C
!        CHARACTER*9 UNN,UNN1
C
          REAL*8 VALUE,VALVAL,SHTVALUE,GRNX,GRNY
     1    ,WVNUM,DIAM,OUTGRIDEXTENT,EXEX,EXEY,OUTGRIDSPACING
C
          INTEGER IJK,KKK
C
          REAL*8 SPTT,SPTOT,IWLIJK(1:10)
C
          COMMON/PEPITO/IWLIJK,IJK
C
          REAL SPACING,EXTENT,CNTX,CNTY,PEXTENT,PSPACING
C
          LOGICAL NOCOBSPSF
C
          COMMON/PSFCOBS/NOCOBSPSF
C
          LOGICAL ERR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          INTEGER SHTNM,IX,IY,II,I,J,NDAT,NDIM,
     1    MM,III,JJJ,IIX,IIY,ALLOERR,HI,MMM,IIIX,IIIY,DFLAG
C
!      LOGICAL ERRR,ERRFOB
C
          COMMON/PSFPLOTER/PEXTENT,PSPACING,CNTX,CNTY
C
          DEALLOCATE(FOEGRID1,STAT=ALLOERR)
          GPREG(101:110)=0.0D0
C
          PSFWV(1)=SYSTEM1(1)
          PSFWV(2)=SYSTEM1(2)
          PSFWV(3)=SYSTEM1(3)
          PSFWV(4)=SYSTEM1(4)
          PSFWV(5)=SYSTEM1(5)
          PSFWV(6)=SYSTEM1(71)
          PSFWV(7)=SYSTEM1(72)
          PSFWV(8)=SYSTEM1(73)
          PSFWV(9)=SYSTEM1(74)
          PSFWV(10)=SYSTEM1(75)
C
C     M IS THE DIMENSION OF THE REQUESTED GRID OVER THE PUPIL
C     THE TRANSFORM IS TWICE THIS BIG
          HI=0
          MM=TGR
          MMM=MM-1
          NDAT=MM*MM*2
          NDIM=2
C
C     HERE IS WHERE WE BUILD THE PSF
          IF(S1.EQ.1) DFLAG=1
          IF(S1.EQ.0) DFLAG=0
C
          IF(.NOT.CPFNEXT) THEN
              OUTLYNE='NO COMPLEX APERTURE FUNCTION EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO PSF CALCULATION IS POSSIBLE'
              CALL SHOWIT(1)
              CALL MACFAL
              CALL DELPSF
              RETURN
          END IF
C     CALC SPTOT
          SPTOT=0.0D0
          SPTOT=SPTOT+(SYSTEM1(31))
          SPTOT=SPTOT+(SYSTEM1(32))
          SPTOT=SPTOT+(SYSTEM1(33))
          SPTOT=SPTOT+(SYSTEM1(34))
          SPTOT=SPTOT+(SYSTEM1(35))
          SPTOT=SPTOT+(SYSTEM1(76))
          SPTOT=SPTOT+(SYSTEM1(77))
          SPTOT=SPTOT+(SYSTEM1(78))
          SPTOT=SPTOT+(SYSTEM1(79))
          SPTOT=SPTOT+(SYSTEM1(80))
C
C     NOW COMPUTE THE SHORTEST WAVELENGTH IN CURENT LENS UNITS
C     CALC THE SHRTWAVE
          SHRTWAVE=0.0D0
          IF(SYSTEM1(31).NE.0.0D0.AND.SYSTEM1(1).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(1)
              GO TO 314
          END IF
          IF(SYSTEM1(32).NE.0.0D0.AND.SYSTEM1(2).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(2)
              GO TO 314
          END IF
          IF(SYSTEM1(33).NE.0.0D0.AND.SYSTEM1(3).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(3)
              GO TO 314
          END IF
          IF(SYSTEM1(34).NE.0.0D0.AND.SYSTEM1(4).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(4)
              GO TO 314
          END IF
          IF(SYSTEM1(35).NE.0.0D0.AND.SYSTEM1(5).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(5)
              GO TO 314
          END IF
          IF(SYSTEM1(76).NE.0.0D0.AND.SYSTEM1(71).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(71)
              GO TO 314
          END IF
          IF(SYSTEM1(77).NE.0.0D0.AND.SYSTEM1(72).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(72)
              GO TO 314
          END IF
          IF(SYSTEM1(78).NE.0.0D0.AND.SYSTEM1(73).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(73)
              GO TO 314
          END IF
          IF(SYSTEM1(79).NE.0.0D0.AND.SYSTEM1(74).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(74)
              GO TO 314
          END IF
          IF(SYSTEM1(80).NE.0.0D0.AND.SYSTEM1(75).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(75)
              GO TO 314
          END IF
 314      CONTINUE
          IF(SHRTWAVE.EQ.0.0D0) THEN
              OUTLYNE='WAVELENGTHS ARE ALL ZERO'
              CALL SHOWIT(1)
              OUTLYNE='NO "PSF" CAN BE CALCULATED'
              CALL SHOWIT(1)
              CALL MACFAL
              CALL DELPSF
              RETURN
          END IF
C
          IF(SYSTEM1(31).NE.0.0D0.AND.SYSTEM1(1).LE.SHRTWAVE
     1    .AND.SYSTEM1(1).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(1)
              SHTNM=1
          END IF
          IF(SYSTEM1(32).NE.0.0D0.AND.SYSTEM1(2).LE.SHRTWAVE
     1    .AND.SYSTEM1(2).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(2)
              SHTNM=2
          END IF
          IF(SYSTEM1(33).NE.0.0D0.AND.SYSTEM1(3).LE.SHRTWAVE
     1    .AND.SYSTEM1(3).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(3)
              SHTNM=3
          END IF
          IF(SYSTEM1(34).NE.0.0D0.AND.SYSTEM1(4).LE.SHRTWAVE
     1    .AND.SYSTEM1(4).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(4)
              SHTNM=4
          END IF
          IF(SYSTEM1(35).NE.0.0D0.AND.SYSTEM1(5).LE.SHRTWAVE
     1    .AND.SYSTEM1(5).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(5)
              SHTNM=5
          END IF
          IF(SYSTEM1(76).NE.0.0D0.AND.SYSTEM1(71).LE.SHRTWAVE
     1    .AND.SYSTEM1(71).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(71)
              SHTNM=6
          END IF
          IF(SYSTEM1(77).NE.0.0D0.AND.SYSTEM1(72).LE.SHRTWAVE
     1    .AND.SYSTEM1(72).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(72)
              SHTNM=7
          END IF
          IF(SYSTEM1(78).NE.0.0D0.AND.SYSTEM1(73).LE.SHRTWAVE
     1    .AND.SYSTEM1(73).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(73)
              SHTNM=8
          END IF
          IF(SYSTEM1(79).NE.0.0D0.AND.SYSTEM1(74).LE.SHRTWAVE
     1    .AND.SYSTEM1(74).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(74)
              SHTNM=9
          END IF
          IF(SYSTEM1(80).NE.0.0D0.AND.SYSTEM1(75).LE.SHRTWAVE
     1    .AND.SYSTEM1(75).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(75)
              SHTNM=10
          END IF
C     SHRTWAVE IS IN MICROMETER, CHANGE TO LENS UNITS
          VALUE=SHRTWAVE
          IF(SYSTEM1(6).EQ.1.0D0) SHTVALUE=((VALUE*1.0D-3)/25.4D0)
          IF(SYSTEM1(6).EQ.2.0D0) SHTVALUE=(VALUE*1.0D-4)
          IF(SYSTEM1(6).EQ.3.0D0) SHTVALUE=(VALUE*1.0D-3)
          IF(SYSTEM1(6).EQ.4.0D0) SHTVALUE=(VALUE*1.0D-6)
C
C     WE NOW HAVE THE WAVELENGTH AND WAVELENGTH NUMBER OF THE
C     SHORTEST WAVELENGTH FOR WHICH THE SPECTRAL WEIGHTING FACTOR
C     IS NON-ZERO. THESE ARE SHTVALUE AND SHTNM
C
C     TGR ALWAYS RULES AND IS NOT CHANGED.
C
C     NOW COMPUTE THE PUPIL GRID SPACING AND EXTENT
C     THIS SETS THE GRID TO BE USED FOR THE PSF
C     IF THE MODE IS FOCAL OR UFOCAL
C     WE WANT THE ENTRANCE PUPIL DIAMETERS IN THE X AND Y DIRECTIONS
C     ENTRANCE PUPIL DIAMETERS IN THE X AND Y DIRECTION AT THE SHORT WAVELENGTH
C     THE LARGER OF THE TWO ENTRANCE PUPIL DIAMETERS WILL ACT AS THE
C     DIMENSION FOR COMPUTING THE PUPIL GRID SPACING AND EXTENT. THIS
C     IS CONSISTENT WITH THE WAY THE GRID IS SET IN THE CAPFN RAYTRACE
C     GRID IN COMPAP.FOR. THESE ARE THE UNVIGNETTED ENTRANCE PUPIL
C     VALUES. VIGNETTING IS ACCOUNTED FOR IN THE PSF RAY TRACE.
C
          IF(SYSTEM1(30).LE.2.0D0) THEN
              IF(REFEXT) THEN
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL FNUMX(VALVAL,ERR)
                  IF(.NOT.ERR) GRNX=VALVAL
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL FNUMY(VALVAL,ERR)
                  IF(.NOT.ERR) GRNY=VALVAL
              ELSE
                  GRNX=RBFNX
                  GRNY=RBFNY
              END IF
          ELSE
C     AFOCAL
              IF(REFEXT) THEN
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL EXPDIAX(VALVAL,ERR)
                  IF(.NOT.ERR) EXEX=VALVAL
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL EXPDIAY(VALVAL,ERR)
                  IF(.NOT.ERR) EXEY=VALVAL
              ELSE
                  EXEX=EXDIAX
                  EXEY=EXDIAY
              END IF
          END IF
C
          IF(SYSTEM1(30).LE.2.0D0) THEN
C     FOCAL
              DIAM=GRNX
              IF(GRNY.LT.GRNX) DIAM=GRNY
              IF(DIAM.EQ.0.0D0) THEN
                  OUTLYNE='F/NUMBER WAS ZERO, NO PSF CALCULATION POSSIBLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  CALL DELPSF
                  RETURN
              END IF
          ELSE
C     AFOCAL
              DIAM=EXEX
              IF(EXEY.GT.EXEX) DIAM=EXEY
              IF(DIAM.EQ.0.0D0) THEN
                  OUTLYNE=
     1            'EXIT PUPIL DIAMETER WAS ZERO, NO PSF CALCULATION POSSIBLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  CALL DELPSF
                  RETURN
              ELSE
C     MAKE THE SYSTEM HAVE A 100 UNIT FOCAL LENGTH
                  DIAM=1.0/DIAM
              END IF
          END IF

          IF(GRIFLG.EQ.1)
     1    OUTGRIDEXTENT=DIAM*(NNRD-1.0D0)*SHTVALUE
          IF(GRIFLG.EQ.0)
     1    OUTGRIDEXTENT=DIAM*(DBLE(NRD)-1.0D0)*SHTVALUE
          OUTGRIDSPACING=OUTGRIDEXTENT/DBLE(TGR-1)
          GRI=OUTGRIDSPACING
C
C     NOW PROCESS THE CAPFN DATA INTO PSF DATA
C
          DEALLOCATE(FOEGRID1,STAT=ALLOERR)
          ALLOCATE(FOEGRID1(MM,MM,1:10,1:2),STAT=ALLOERR)
C
          IWIW=IW**2
          I=2
C
          III=MM
          JJJ=MM
          FOEGRID1(1:MM,1:MM,1:10,1:2)=0.0D0
          DO J=1,NUMCOL
C     DOING A COLOR NOW
              IX=0
              IY=1
              II=0

C     WE WANT TO DO IWIW READS AND LOADS OF THE A ARRAY
 10           II=II+1
              IX=IX+1
              IF(IX.GT.IW) THEN
                  IY=IY+1
                  IX=1
              END IF
C     LOAD DSPOT(*) WITH DSPOTT(*,ID)
              ID=I-1
              CALL SPOTIT(4)
              SPTT=(DSPOT(17))
              I=I+1
C     LOAD AN ARRAY ELEMENT WITH REAL AND IMAGINARY CAPFN
C     FOR A COLOR
C     COL,ROW INDEXED AND ZERO PADDED, THAT IS WHY THE IIX AND IIY
C     ARE NEEDED

              IIIX=((MM/2)-((NRD)/2))
              IIIY=((MM/2)-((NRD)/2))
              IF(IIIX.LT.0) IIIX=0
              IF(IIIY.LT.0) IIIY=0
              IIX=IX+IIIX
              IIY=IY+IIIY
              IF(DSPOT(12).NE.0.0D0) THEN
              END IF
              IF(KKK.EQ.1) THEN
C       SAVE AIB
C     REAL PART
                  DSPOT(12)=DSPOT(12)*W3
                  DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
                  FOEGRID1(IIX,IIY,J,1)=
     1            DSQRT(DABS(DSPOT(12)))*DCOS(DSPOT(4))
C     IMAGINARY PART
                  FOEGRID1(IIX,IIY,J,2)=
     1            -DSQRT(DABS(DSPOT(12)))*DSIN(DSPOT(4))
                  WVNUM=DSPOT(16)
              ELSE
C       SAVE AS REITHETA
C     MODULUS
                  DSPOT(12)=DSPOT(12)*W3
                  DSPOT(37)=DSPOT(12)*RAYRAY(9,NEWIMG)
                  FOEGRID1(IIX,IIY,J,1)=DSQRT(DABS(DSPOT(12)))
C     PHASE IN WAVES
                  FOEGRID1(IIX,IIY,J,2)=DSPOT(4)
              END IF
C
              IF(II.LT.IWIW) GO TO 10
C     FELL THROUGH, FINISHED READING IWIW POINTS
              IF(SYSTEM1(30).LE.2.0D0) THEN
C       FOCAL
                  EXTENT=SNGL(OUTGRIDEXTENT)
                  SPACING=SNGL(OUTGRIDSPACING)
              ELSE
C       AFOCAL
                  EXTENT=SNGL(OUTGRIDEXTENT)
                  SPACING=SNGL(OUTGRIDSPACING)
              END IF
              PEXTENT=EXTENT*REAL(PGR)/REAL(TGR-1)
              PSPACING=SPACING
C     THE ARRAYS FOR A COLOR ARE WRITTEN, NOW SET UP THE X AND Y ARRAYS
          END DO
          RETURN
      END
C SUB NRDCALC.FOR

      SUBROUTINE NRDCALC(ERRORR)
C
          IMPLICIT NONE
C
C     THIS CALCULATED A NON-INTEGER NRD WHEN GRI IS SPECIFIED
C
          REAL*8 SHTVALUE,VALUE,FNX,FNY,FNN,VALVAL
     1    ,TNRD
C
          LOGICAL ERRORR,ERR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          INTEGER SHTNM
C
C     NOW COMPUTE THE SHORTEST WAVELENGTH IN CURENT LENS UNITS
C     CALC THE SHRTWAVE
          SHRTWAVE=0.0D0
          IF(SYSTEM1(31).NE.0.0D0.AND.SYSTEM1(1).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(1)
              GO TO 314
          END IF
          IF(SYSTEM1(32).NE.0.0D0.AND.SYSTEM1(2).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(2)
              GO TO 314
          END IF
          IF(SYSTEM1(33).NE.0.0D0.AND.SYSTEM1(3).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(3)
              GO TO 314
          END IF
          IF(SYSTEM1(34).NE.0.0D0.AND.SYSTEM1(4).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(4)
              GO TO 314
          END IF
          IF(SYSTEM1(35).NE.0.0D0.AND.SYSTEM1(5).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(5)
              GO TO 314
          END IF
          IF(SYSTEM1(76).NE.0.0D0.AND.SYSTEM1(71).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(71)
              GO TO 314
          END IF
          IF(SYSTEM1(77).NE.0.0D0.AND.SYSTEM1(72).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(72)
              GO TO 314
          END IF
          IF(SYSTEM1(78).NE.0.0D0.AND.SYSTEM1(73).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(73)
              GO TO 314
          END IF
          IF(SYSTEM1(79).NE.0.0D0.AND.SYSTEM1(74).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(74)
              GO TO 314
          END IF
          IF(SYSTEM1(80).NE.0.0D0.AND.SYSTEM1(75).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(75)
              GO TO 314
          END IF
 314      CONTINUE
          IF(SHRTWAVE.EQ.0.0D0) THEN
              OUTLYNE='WAVELENGTHS ARE ALL ZERO'
              CALL SHOWIT(1)
              OUTLYNE='NO "PSF" CAN BE CALCULATED'
              CALL SHOWIT(1)
              ERRORR=.TRUE.
              CALL MACFAL
              CALL DELPSF
              RETURN
          END IF
C
          IF(SYSTEM1(31).NE.0.0D0.AND.SYSTEM1(1).LE.SHRTWAVE
     1    .AND.SYSTEM1(1).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(1)
              SHTNM=1
          END IF
          IF(SYSTEM1(32).NE.0.0D0.AND.SYSTEM1(2).LE.SHRTWAVE
     1    .AND.SYSTEM1(2).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(2)
              SHTNM=2
          END IF
          IF(SYSTEM1(33).NE.0.0D0.AND.SYSTEM1(3).LE.SHRTWAVE
     1    .AND.SYSTEM1(3).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(3)
              SHTNM=3
          END IF
          IF(SYSTEM1(34).NE.0.0D0.AND.SYSTEM1(4).LE.SHRTWAVE
     1    .AND.SYSTEM1(4).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(4)
              SHTNM=4
          END IF
          IF(SYSTEM1(35).NE.0.0D0.AND.SYSTEM1(5).LE.SHRTWAVE
     1    .AND.SYSTEM1(5).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(5)
              SHTNM=5
          END IF
          IF(SYSTEM1(76).NE.0.0D0.AND.SYSTEM1(71).LE.SHRTWAVE
     1    .AND.SYSTEM1(71).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(71)
              SHTNM=6
          END IF
          IF(SYSTEM1(77).NE.0.0D0.AND.SYSTEM1(72).LE.SHRTWAVE
     1    .AND.SYSTEM1(72).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(72)
              SHTNM=7
          END IF
          IF(SYSTEM1(78).NE.0.0D0.AND.SYSTEM1(73).LE.SHRTWAVE
     1    .AND.SYSTEM1(73).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(73)
              SHTNM=8
          END IF
          IF(SYSTEM1(79).NE.0.0D0.AND.SYSTEM1(74).LE.SHRTWAVE
     1    .AND.SYSTEM1(74).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(74)
              SHTNM=9
          END IF
          IF(SYSTEM1(80).NE.0.0D0.AND.SYSTEM1(75).LE.SHRTWAVE
     1    .AND.SYSTEM1(75).NE.0.0D0) THEN
              SHRTWAVE=SYSTEM1(75)
              SHTNM=10
          END IF
C     SHRTWAVE IS IN MICROMETER, CHANGE TO LENS UNITS
          VALUE=SHRTWAVE
          IF(SYSTEM1(6).EQ.1.0D0) SHTVALUE=((VALUE*1.0D-3)/25.4D0)
          IF(SYSTEM1(6).EQ.2.0D0) SHTVALUE=(VALUE*1.0D-4)
          IF(SYSTEM1(6).EQ.3.0D0) SHTVALUE=(VALUE*1.0D-3)
          IF(SYSTEM1(6).EQ.4.0D0) SHTVALUE=(VALUE*1.0D-6)
C
C     WE NOW HAVE THE WAVELENGTH AND WAVELENGTH NUMBER OF THE
C     SHORTEST WAVELENGTH FOR WHICH THE SPECTRAL WEIGHTING FACTOR
C     IS NON-ZERO. THESE ARE SHTVALUE AND SHTNM
C
C     TGR ALWAYS RULES AND IS NOT CHANGED.
C
C     NOW COMPUTE THE NRD
C
          IF(SYSTEM1(30).LE.2.0D0) THEN
              IF(REFEXT) THEN
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL FNUMX(VALVAL,ERR)
                  IF(.NOT.ERR) FNX=VALVAL
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL FNUMY(VALVAL,ERR)
                  IF(.NOT.ERR) FNY=VALVAL
              ELSE
                  FNX=RBFNX
                  FNY=RBFNY
              END IF
              FNN=FNX
              IF(FNY.LT.FNX) FNN=FNY
          ELSE
C     AFOCAL
              IF(REFEXT) THEN
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL EXPDIAX(VALVAL,ERR)
                  IF(.NOT.ERR) FNX=VALVAL
                  ERR=.FALSE.
                  MSG=.FALSE.
                  CALL EXPDIAY(VALVAL,ERR)
                  IF(.NOT.ERR) FNY=VALVAL
              ELSE
                  FNX=EXDIAX
                  FNY=EXDIAY
              END IF
              FNN=FNX
              IF(FNY.GT.FNX) FNN=FNY
              FNN=1.0D0/FNN
          END IF
          FNN=DABS(FNN)
C
          NNRD=DABS(((GRI*DBLE(TGR-1))/(FNN*SHTVALUE))+1.0D0)
          IF(NNRD.GT.DBLE(TGR)) THEN
              NNRD=DABS(DBLE(NRD))
              OUTLYNE='THE NEW "NRD" FOR THE GIVEN "GRI" WAS UNREALISTIC'
              CALL SHOWIT(1)
              OUTLYNE='NO NEW "NRD" VALUE WAS COMPUTED OR WILL BE USED'
              CALL SHOWIT(1)
              OUTLYNE='"GRI" NO LONGER DETERMINES THE "NRD" VALUE'
              CALL SHOWIT(1)
          END IF
          TNRD=DINT(NNRD)
          IF(MOD(TNRD,2.0D0).NE.0.0D0) TNRD=TNRD-1.0D0
          IF(INT(TNRD).NE.0) THEN
              NRD=INT(DABS(TNRD))
              NRDFACTOR=DBLE(NRD-1)/NNRD
          END IF
          IF(INT(TNRD).EQ.0) THEN
              NNRD=DABS(DBLE(NRD))
              OUTLYNE='THE NEW "NRD" FOR THE GIVEN "GRI" WAS UNREALISTIC'
              CALL SHOWIT(1)
              OUTLYNE='NO NEW "NRD" VALUE WAS COMPUTED OR WILL BE USED'
              CALL SHOWIT(1)
              OUTLYNE='"GRI" NO LONGER DETERMINES THE "NRD" VALUE'
              CALL SHOWIT(1)
          END IF
          ERRORR=.FALSE.
          GRIFLG=0
          NRDFLG=1
          RETURN
      END
