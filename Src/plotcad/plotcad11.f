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


C       ELEVENTH FILE OF PLOT/CAD ROUTINES

C SUB PLTINTEN.FOR
      SUBROUTINE PLTINTEN
C
          IMPLICIT NONE
C
C       THIS PROGRAM CONTROLS THE PLOT INTEN COMMAND
C
!      INTEGER COLPAS
C
          INTEGER G,KKK,KVAL,I,KKV
C
          REAL REFHT
C
          REAL*8 WVAL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          LOGICAL PLOTIT(1:10)
          COMMON/ITPLOT/PLOTIT
C
C
          AUTOZSCALE=.TRUE.
          IF(STI.EQ.1) THEN
              OUTLYNE= '"PLOT INTEN" PLOTS THE EXISTING MULTI-RAY INTENSITY'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE= '"PLOT INTEN" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'INTEN') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE= '"PLOT INTEN" TAKES NO NUMERIC WORD'
                  CALL SHOWIT(1)
                  OUTLYNE='#2, #3, #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE= 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF

C     OPEN AND LOAD THE APPROPRIATE INTENSITY DATA AND THEN PLOT IT
          IF(.NOT.CPFNEXT) THEN
              OUTLYNE=
     1        'NO INTENSITY DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     DETERMINE THE REFERNCE APERTURE HEIGHT
C     OF THE REF SURFACE COORDINATES
          IF(DABS(ALENS(9,NEWREF)).GE.1.0D0.AND.DABS(ALENS(9,NEWREF))
     1    .LE.5.0D0) THEN
              IF(DABS(ALENS(9,NEWREF)).EQ.1.0D0) THEN
                  REFHT=SNGL(ALENS(10,NEWREF))
              END IF
C        RECT CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.2.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=SNGL(ALENS(10,NEWREF))
                  ELSE
                      REFHT=SNGL(ALENS(11,NEWREF))
                  END IF
              END IF
C        ELIP CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.3.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=SNGL(ALENS(10,NEWREF))
                  ELSE
                      REFHT=SNGL(ALENS(11,NEWREF))
                  END IF
              END IF
C        RCTK CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.4.0D0) THEN
                  IF(ALENS(10,NEWREF).GE.ALENS(11,NEWREF)) THEN
                      REFHT=SNGL(ALENS(10,NEWREF))
                  ELSE
                      REFHT=SNGL(ALENS(11,NEWREF))
                  END IF
              END IF
C        POLY CLAP
C
              IF(DABS(ALENS(9,NEWREF)).EQ.5.0D0) THEN
                  REFHT=SNGL(ALENS(10,NEWREF))
              END IF
C
          ELSE
C       NO CLAP ON REF SURF.
              IF(PXTRAY(1,NEWREF).GE.PXTRAX(1,NEWREF)) THEN
                  REFHT=SNGL((PXTRAY(1,NEWREF)))
              ELSE
                  REFHT=SNGL((PXTRAX(1,NEWREF)))
              END IF
          END IF
C
          WVAL=(W1)
          KKV=(ITOT-1)/NUMCOL
C     KKV IS THE TOTAL NUMBER OF POINTS AT THE DESIRED COLOR
C     AND IT IS AN EXACT SQUARE
          PLOTIT(1)=.FALSE.
          PLOTIT(2)=.FALSE.
          PLOTIT(3)=.FALSE.
          PLOTIT(4)=.FALSE.
          PLOTIT(5)=.FALSE.
          PLOTIT(6)=.FALSE.
          PLOTIT(7)=.FALSE.
          PLOTIT(8)=.FALSE.
          PLOTIT(9)=.FALSE.
          PLOTIT(10)=.FALSE.
          DO I=1,ITOT-1
C     LOAD DSPOTT(*,ID) INTO DSPOT(*)
              ID=I
              CALL SPOTIT(4)
C
              IF(DSPOT(16).EQ.WVAL) THEN
                  PLOTIT(INT(WVAL))=.TRUE.
                  KVAL=I
                  GO TO 10
              END IF
C
          END DO
 10       CONTINUE
          G=ITOT+1
C

          KKK=NINT(SQRT(FLOAT(KKV)))
          IF(.NOT.PLOTIT(INT(W1))) THEN
              OUTLYNE= 'CAPFN DATA FOR THE WAVELENGTH VALUE (NUMERIC WORD 1)'
              CALL SHOWIT(1)
              OUTLYNE= 'DOES NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE= 'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WQ.EQ.'CAPFNOPD') CALL CAPPLOT(1,REFHT,WVAL,KKV,KKK)
          IF(WQ.EQ.'CAPFNAPD') CALL CAPPLOT(2,REFHT,WVAL,KKV,KKK)
          RETURN
      END


C SUB INTENPLOT.FOR
      SUBROUTINE INTENPLOT(IJ,REFHT,WVAL,KKV,KKK)
C     KVAL IS THE FIRST RECORD OF THE SPOT FILE TO READ
C     REFHT IS THE NORMALIZING AP HT.
C     WVAL IS THE DESIRED WAVELENGTH NUMBER
C     KKV IS THE TOTAL NUMBER OF POINTS TO BE READ IN FROM DSPOTT
C
          IMPLICIT NONE
C
          LOGICAL ITSFLAT
C
          INTEGER IJ,I,J,KKI2,KKK,IQ,KKV
          INTEGER ALLOERR,WVNUMB
C
          REAL*8 WVAL

          REAL REFHT,ZMAX2,ZMIN2,RI,RII
          REAL XYVALUE,XPLT,YPLT,F1PLT,F2PLT,DELGRID
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          DIMENSION F1PLT(:,:),F2PLT(:,:),XPLT(:),YPLT(:)
C
          ALLOCATABLE :: F1PLT,F2PLT,YPLT,XPLT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          KKI2=NINT(SQRT(FLOAT(KKV)))
          DEALLOCATE(XPLT,YPLT,F1PLT,F2PLT,STAT=ALLOERR)
          ALLOCATE(XPLT(KKK),YPLT(KKK),F1PLT(KKK,KKK),
     1    F2PLT(KKK,KKK),STAT=ALLOERR)
C
          ZMAX1=-1.0E10
          ZMIN1=1.0E10
          ZMAX2=1.0E0
          ZMIN2=0.0E0
          I=1
          J=0
          DO IQ=1,ITOT-1
C     LOAD DSPOTT(*,ID) INTO DSPOT(*)
              ID=IQ
              CALL SPOTIT(4)
              IF(DSPOT(16).NE.WVAL) THEN
                  GO TO 101
              END IF
              J=J+1
              IF(J.GT.KKK) THEN
                  J=1
                  I=I+1
              END IF
              F1PLT(J,I)=SNGL(DSPOT(4)/(TWOPII))
              F2PLT(J,I)=SNGL(DSPOT(12))
              WVNUMB=INT(DSPOT(16))
              IF(F2PLT(J,I).NE.0.0) THEN
                  IF(F1PLT(J,I).GT.ZMAX1) ZMAX1=(F1PLT(J,I))
                  IF(F1PLT(J,I).LE.ZMIN1) ZMIN1=(F1PLT(J,I))
              END IF
 101          CONTINUE
          END DO
          ITSFLAT=.FALSE.
          IF(ABS(ZMIN1-ZMAX1).LT.1.0E-6) THEN
              IF(ABS(ZMIN1).LE.1.0E-6) ZMIN1=0.0
              ZMAX1=ZMIN1+1.0
              ITSFLAT=.TRUE.
              GO TO 5
          END IF
C
  5       DO I=-10000,10000,1
              RI=FLOAT(I)/100.0
              RII=FLOAT(I+1)/100.0
              IF(ZMAX1.GT.RI.AND.ZMAX1.LE.RII) THEN
                  ZMAX1=RII
                  GO TO 10
              END IF
          END DO
 10       CONTINUE
          DO I=-10000,10000,1
              RI=FLOAT(I)/100.0
              RII=FLOAT(I+1)/100.0
              IF(ZMIN1.GE.RI.AND.ZMIN1.LT.RII) THEN
                  ZMIN1=RI
                  GO TO 20
              END IF
          END DO
 20       CONTINUE
          IF(ZMIN1.EQ.ZMAX1) THEN
              ZMAX1=ZMIN1+0.01
              DO J=1,KKI2
                  DO I=1,KKI2
                      IF(F1PLT(I,J).LT.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1                F1PLT(I,J)=-0.0001
                      IF(F1PLT(I,J).GE.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1                F1PLT(I,J)= 0.0001
                  END DO
              END DO
          END IF
          CONTINUE
          DO J=1,KKI2
              DO I=1,KKI2
                  IF(F1PLT(I,J).LT.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1            F1PLT(I,J)=-0.0001
                  IF(F1PLT(I,J).GE.0.0.AND.ABS(F1PLT(I,J)).LT.0.0001)
     1            F1PLT(I,J)= 0.0001
              END DO
          END DO

          DELGRID=2.0/(FLOAT(KKI2)-1.0)
          XPLT(1)=-1.0
          YPLT(1)=-1.0
          XPLT(KKI2)=1.0
          YPLT(KKI2)=1.0
          XYVALUE=-1.0
          DO IQ=2,KKI2-1
              XYVALUE=XYVALUE+DELGRID
              XPLT(IQ)=XYVALUE
              YPLT(IQ)=XYVALUE
          END DO
          DO IQ=1,KKI2
              XPLT(IQ)=XPLT(IQ)*.99
              YPLT(IQ)=YPLT(IQ)*.99
          END DO
          IF(DF3.EQ.0.AND.DF4.EQ.0) THEN
              ZMAX1=SNGL(W4)
              ZMIN1=SNGL(W3)
          END IF
          DO J=1,KKI2
              DO I=1,KKI2
                  F1PLT(I,J)=(F1PLT(I,J)-ZMIN1)/(ZMAX1-ZMIN1)
                  IF(ABS(F1PLT(I,J)).LT.1E-6) F1PLT(I,J)=0.0
                  IF(ABS(F2PLT(I,J)).LT.1E-6) F2PLT(I,J)=0.0
              END DO
          END DO
          IF(ITSFLAT) THEN
              J=KKI2
              I=KKI2
              F1PLT(1:I,1:J)=0.01
          END IF
          CALL FFT3(KKI2,XPLT,YPLT,F1PLT,F2PLT,REFHT,IJ,WVNUMB)
          DEALLOCATE(XPLT,YPLT,F1PLT,F2PLT,STAT=ALLOERR)
          RETURN
      END



      SUBROUTINE PLOTINTEN(PCOUNT,XPLT,YPLT,F1PLT,F2PLT
     1,ROT,IJ,REFHT,WVNUMB)
          IMPLICIT NONE
C     F IS THE FUNTION FPLT(X,Y),XPLT AND YPLT ARE THE POINT COORDINATES
C     PCOUNT=NUMBER OF POINTS IN THE CAPFN FILE
C
          CHARACTER B*80,UNN*9,BLNOTE*80,BL20*20,NNTT1*99,CRANGE*15
     1    ,UNN1*9,TMY*8,DTY*10,CCRANGE*11
C
          INTEGER PCOUNT,ROT,WVWT,NT1ANG,NT1SIZ,ALLOERR
     1    ,K,COLPAS,II,IIX,IIY,IIXM1,IIYM1,IPN,H,WVNUMB

          REAL*8 XTEST1,XTEST2,YTEST1,YTEST2,SLOPE
C
          REAL F1PLT,F2PLT,XPLT,YPLT
     1    ,REFHT
C
          DIMENSION XPLT(PCOUNT),YPLT(PCOUNT),F1PLT(PCOUNT,PCOUNT),
     1    H(:),F2PLT(PCOUNT,PCOUNT)
C
          EXTERNAL WVWT
C
!      LOGICAL ERROR1
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          INTEGER I,J,IJ
          REAL*8 XV,YV,RANGE1
          REAL*8 XX,YY,XY2
          DIMENSION XX(:),YY(:),XY2(:)
          REAL F_JK,X_JK,Y_JK
          DIMENSION F_JK(:,:),X_JK(:),Y_JK(:)
          ALLOCATABLE :: XX,YY,F_JK,X_JK,Y_JK,XY2,H
          INCLUDE 'datmai.inc'
          INCLUDE 'dathgr.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          DEALLOCATE(XX,YY,F_JK,X_JK,Y_JK,XY2,H,STAT=ALLOERR)
          ALLOCATE(XX(PCOUNT),YY(PCOUNT),XY2(PCOUNT),
     1    F_JK(PCOUNT,PCOUNT),X_JK(PCOUNT),Y_JK(PCOUNT),H(2740:7260),
     2    STAT=ALLOERR)
          DO I=2740,7260
              H(I)=-10000
          END DO
          DO I=1,PCOUNT
              X_JK(I)=XPLT(I)
              Y_JK(I)=YPLT(I)
              XX(I)=0.0
              YY(I)=0.0
              DO J=1,PCOUNT
                  IF(IJ.EQ.1) F_JK(I,J)=F1PLT(I,J)
                  IF(IJ.EQ.2) F_JK(I,J)=F2PLT(I,J)
              END DO
          END DO
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
          IF(IJ.EQ.1) IIY=INT(2000.0+F1PLT(1,1))
          IF(IJ.EQ.2) IIY=INT(2500.0+F2PLT(1,1))
          CALL MY_PLOT(IIX,IIY,0,0,-10,10010,-10,7010)
          IPN=1

          DO J=1,PCOUNT
C     LOAD THE XX AND YY ARRAYS FOR A SPECIFIC J VALUE
              DO K=1,PCOUNT
                  XX(K)=XPLT(K)
                  IF(IJ.EQ.1) YY(K)=F1PLT(K,J)
                  IF(IJ.EQ.2) YY(K)=F2PLT(K,J)
              END DO
C     NOW PLOTTING STEPS A INTEGER 10 AND THERE ARE GOING TO BE
C     100 POINTS PER LINE
              DO I=1,3000
                  XV=DBLE(I-1)
                  YV=0.0D0
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
C     SLOPE IS VERTICAL, TAKE THE AVERAGE Y VALUE
                              YV=(YTEST2+YTEST1)/2.0D0
                          END IF
                          GO TO 2001
                      END IF
                  END DO
 2001             CONTINUE
                  IIXM1=IIX
                  IIYM1=IIY
                  IIX=INT((2750+XV)+(0.5*YPLT(J)))
                  IIY=INT((2000+YV)+(0.5*YPLT(J)))
C     IIX IS IN THE RANGE 2750 TO 7250
                  IPN=0
                  IF((IIY).GT.H(IIX)) THEN
                      IF(IPN.EQ.0.AND.I.NE.1.AND.IIYM1.LT.IIY.AND.H(IIX).GT.-10000
     1                .AND.IIYM1.LT.H(IIXM1))
     1                CALL MY_PLOT(IIX,H(IIX),0,0,-10,10010,-10,7010)
                      H(IIX)=(IIY)
                      IPN=1
                  ELSE
C     IIY LESS THAN OR EQUAL TO H(IIX)
                      IPN=0
                  END IF
                  IF(I.EQ.1) IPN=0
                  CALL MY_PLOT(IIX,IIY,IPN,0,-10,10010,-10,7010)
                  IF(IPN.EQ.0.AND.I.EQ.1) IPN=1
              END DO
              IPN=0
          END DO
          write(*,*) ROT
C
C
          IF(ROT.EQ.90) THEN
C     XAXIS

              CALL MY_PLOT(2550,1800,0,0,0,10000,0,7000)
              CALL MY_PLOT(2750,2000,1,0,0,10000,0,7000)
              CALL MY_PLOT(2750,2000,0,0,0,10000,0,7000)

C     XAXIS LABEL

              CALL MY_JUSTSTRING(2200,1910,'+X',0,2,3)
C     YAXIS

              CALL MY_PLOT(2750,2000,0,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2000,1,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2000,0,0,0,10000,0,7000)

C     YAXIS LABEL
              CALL MY_JUSTSTRING(6182,1910,'+Y',0,2,3)
          END IF
          IF(ROT.EQ.0) THEN
C     YAXIS

              CALL MY_PLOT(5750,2000,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,3700,1,0,0,10000,0,7000)
              CALL MY_PLOT(7450,3700,0,0,0,10000,0,7000)

C     YAXIS LABEL

              CALL MY_JUSTSTRING(7550,3800,'+Y',0,2,3)
C     XAXIS

              CALL MY_PLOT(2750,2000,0,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2000,1,0,0,10000,0,7000)
              CALL MY_PLOT(6032,2000,0,0,0,10000,0,7000)

C     XAXIS LABEL

              CALL MY_JUSTSTRING(6182,1910,'+X',0,2,3)
          END IF
C     ZAXIS

          CALL MY_PLOT(7250,3500,0,0,0,10000,0,7000)
          CALL MY_PLOT(7250,5500,1,0,0,10000,0,7000)
          CALL MY_PLOT(7250,5500,0,0,0,10000,0,7000)

C
          IF(IJ.EQ.1) THEN
C     Z AXIS TIC MARKS, OPD

              CALL MY_PLOT(7250,3500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,3500,1,0,0,10000,0,7000)

C
              RANGE1=ZMIN1
              WRITE(B,102) RANGE1
              READ(B,202) CCRANGE
              NNTT1=CCRANGE//' WAVES'
              CALL MY_JUSTSTRING(7550,3460,NNTT1(1:20),0,1,3)
              CALL MY_PLOT(7250,3700,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,3700,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,3900,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,3900,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4100,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4100,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4300,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4300,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,4500,1,0,0,10000,0,7000)

              RANGE1=(ZMAX1+ZMIN1)/2.0D0
              WRITE(B,102) RANGE1
              READ(B,202) CCRANGE
              NNTT1=CCRANGE//' WAVES'
              CALL MY_JUSTSTRING(7550,4460,NNTT1(1:20),0,1,3)

              CALL MY_PLOT(7250,4700,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4700,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4900,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4900,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5100,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,5100,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5300,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,5300,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,5500,1,0,0,10000,0,7000)

              RANGE1=ZMAX1
              WRITE(B,102) RANGE1
              READ(B,202) CCRANGE
              NNTT1=CCRANGE//' WAVES'

              CALL MY_JUSTSTRING(7550,5460,NNTT1(1:20),0,1,3)
          END IF
          IF(IJ.EQ.2) THEN
C     Z AXIS TIC MARKS, INTENSITY

              CALL MY_PLOT(7250,3500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,3500,1,0,0,10000,0,7000)


              CALL MY_JUSTSTRING(7550,3460,'0.0',0,1,3)

              CALL MY_PLOT(7250,3700,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,3700,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,3900,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,3900,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4100,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4100,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4300,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4300,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,4500,1,0,0,10000,0,7000)

              CALL MY_JUSTSTRING(7550,4460,'0.5',0,1,3)

              CALL MY_PLOT(7250,4700,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4700,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,4900,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,4900,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5100,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,5100,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5300,0,0,0,10000,0,7000)
              CALL MY_PLOT(7350,5300,1,0,0,10000,0,7000)
              CALL MY_PLOT(7250,5500,0,0,0,10000,0,7000)
              CALL MY_PLOT(7450,5500,1,0,0,10000,0,7000)


              CALL MY_JUSTSTRING(7550,5460,'1.0',0,1,3)
          END IF
C     Z AXIS LABLE
          IF(IJ.EQ.1)
     1    CALL MY_JUSTSTRING(6500,5750,' (Wavefront Error)',0,1,3)
          IF(IJ.EQ.2)
     1    CALL MY_JUSTSTRING(6500,5750,'(Relative Intensity)',0,1,3)
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
          IF(IJ.EQ.1) THEN
              IF(ROT.EQ.0) CALL MY_JUSTSTRING(5200,300,
     1        'Complex Aperture Function Wavefront Phase',
     1         0,2,3)
              IF(ROT.EQ.90) CALL MY_JUSTSTRING(5200,300,
     1        'Complex Aperture Function Wavefront Phase-ROTATED',
     1         0,2,3)
          END IF
          IF(IJ.EQ.2) THEN
              IF(ROT.EQ.0) CALL MY_JUSTSTRING(5200,300,
     1        'Complex Aperture Function Wavefront Intensity',
     1         0,2,3)
              IF(ROT.EQ.90) CALL MY_JUSTSTRING(5200,300,
     1        'Complex Aperture Function Wavefront Intensity-ROTATED',
     1         0,2,3)
          END IF
C
C     DO THE PLOTTING OF THE EXTENT
          IF(SYSTEM1(6).EQ.1.0D0) UNN='in(s)    '
          IF(SYSTEM1(6).EQ.2.0D0) UNN='cm(s)    '
          IF(SYSTEM1(6).EQ.3.0D0) UNN='mm(s)    '
          IF(SYSTEM1(6).EQ.4.0D0) UNN='meter(s) '
C     UNITS ARE NOW SET
C
C     NOW WRITE = "VALUE" UNN
          RANGE1=DBLE(2.0*REFHT)
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='REFERENCE APERTURE WIDTH = '//CRANGE//' '//UNN
C
          CALL MY_JUSTSTRING(200,1200,NNTT1(1:53),NT1ANG,NT1SIZ,3)
C
          IF(IJ.EQ.1) THEN
C     NOW WRITE = "VALUE" UNN
              RANGE1=PTOVOPD(WVNUMB)
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='P-V OPD = '//CRANGE//' WAVES'
C

              CALL MY_JUSTSTRING(200,5900,NNTT1(1:60),NT1ANG,NT1SIZ,3)
C
C     NOW WRITE = "VALUE" UNN
              RANGE1=RMSOP(WVNUMB)
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='RMS OPD = '//CRANGE//' WAVES'
C
              CALL MY_JUSTSTRING(200,5700,NNTT1(1:45),NT1ANG,NT1SIZ,3)
          END IF
C
          IF(WVNUMB.GE.1.AND.WVNUMB.LE.5) RANGE1=SYSTEM1(WVNUMB)
          IF(WVNUMB.GE.6.AND.WVNUMB.LE.10) RANGE1=SYSTEM1(WVNUMB+65)
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='WAVELENGTH = '//CRANGE//' MICRONS'
C
          CALL MY_JUSTSTRING(200,5500,NNTT1(1:40),NT1ANG,NT1SIZ,3)
C
C
C     NOW WRITE = "VALUE" UNN
          IF(WVNUMB.EQ.1) RANGE1=SYSTEM1(1)
          IF(WVNUMB.EQ.2) RANGE1=SYSTEM1(2)
          IF(WVNUMB.EQ.3) RANGE1=SYSTEM1(3)
          IF(WVNUMB.EQ.4) RANGE1=SYSTEM1(4)
          IF(WVNUMB.EQ.5) RANGE1=SYSTEM1(5)
          IF(WVNUMB.EQ.6) RANGE1=SYSTEM1(71)
          IF(WVNUMB.EQ.7) RANGE1=SYSTEM1(72)
          IF(WVNUMB.EQ.8) RANGE1=SYSTEM1(73)
          IF(WVNUMB.EQ.9) RANGE1=SYSTEM1(74)
          IF(WVNUMB.EQ.10) RANGE1=SYSTEM1(75)
          WRITE(B,101) RANGE1
          READ(B,200) CRANGE
          NNTT1='REFERENCE WAVELENGTH = '//CRANGE//' MICRON(S)'
C

          CALL MY_JUSTSTRING(200,1600,NNTT1(1:48),NT1ANG,NT1SIZ,3)
C
          IF(.NOT.SUMMOR) THEN
              IF(IJ.EQ.1) THEN
C     DO REF SPHERE SHIFTS IF NOT ZERO
                  IF(SYSTEM1(6).EQ.1.0D0) UNN='in(s)    '
                  IF(SYSTEM1(6).EQ.2.0D0) UNN='cm(s)    '
                  IF(SYSTEM1(6).EQ.3.0D0) UNN='mm(s)    '
                  IF(SYSTEM1(6).EQ.4.0D0) UNN='meter(s) '
                  IF(DABS(DLLX).GT.1.0D-14) THEN
                      WRITE(B,101) DLLX
                      READ(B,200) CRANGE
                      NNTT1='REF. SPHERE X-SHIFT = '//CRANGE//' '//UNN

                      CALL MY_JUSTSTRING(5200,1600,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  ELSE
                      NNTT1='REF. SPHERE CENTER NOT SHIFTED IN X'
                      CALL MY_JUSTSTRING(5200,1600,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  END IF
                  IF(DABS(DLLY).GT.1.0D-14) THEN
                      WRITE(B,101) DLLY
                      READ(B,200) CRANGE
                      NNTT1='REF. SPHERE Y-SHIFT = '//CRANGE//' '//UNN

                      CALL MY_JUSTSTRING(5200,1400,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  ELSE
                      NNTT1='REF. SPHERE CENTER NOT SHIFTED IN Y'
                      CALL MY_JUSTSTRING(5200,1400,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  END IF
                  IF(DABS(DLLZ).GT.1.0D-14) THEN
                      WRITE(B,101) DLLZ
                      READ(B,200) CRANGE
                      NNTT1='REF. SPHERE Z-SHIFT = '//CRANGE//' '//UNN

                      CALL MY_JUSTSTRING(5200,1200,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  ELSE
                      NNTT1='REF. SPHERE CENTER NOT SHIFTED IN Z'
                      CALL MY_JUSTSTRING(5200,1200,NNTT1(1:50),NT1ANG,NT1SIZ,3)
                  END IF
              END IF
C
C     NOT SUMMED CAPFN, DO FOV STUFF
C     FIELD OF VIEW DATA
C
              IF(SYSTEM1(19).EQ.1.0D0) THEN
C     SCX FANG
                  RANGE1=SYSTEM1(23)*LFOB(2)
                  UNN1='DEGREE(S)'
              ELSE
                  RANGE1=SYSTEM1(16)*LFOB(2)
                  IF(SYSTEM1(6).EQ.1.0D0) UNN1='in(s)    '
                  IF(SYSTEM1(6).EQ.2.0D0) UNN1='cm(s)    '
                  IF(SYSTEM1(6).EQ.3.0D0) UNN1='mm(s)    '
                  IF(SYSTEM1(6).EQ.4.0D0) UNN1='meter(s) '
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
              IF(SYSTEM1(18).EQ.1.0D0) THEN
C     SCY FANG
                  RANGE1=SYSTEM1(21)*LFOB(1)
                  UNN1='DEGREE(S)'
              ELSE
                  RANGE1=SYSTEM1(14)*LFOB(1)
                  IF(SYSTEM1(6).EQ.1.0D0) UNN1='in(s)    '
                  IF(SYSTEM1(6).EQ.2.0D0) UNN1='cm(s)    '
                  IF(SYSTEM1(6).EQ.3.0D0) UNN1='mm(s)    '
                  IF(SYSTEM1(6).EQ.4.0D0) UNN1='meter(s) '
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
          END IF
C
C
          IF(DF1.EQ.1) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='DRAW'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
101       FORMAT(1PG15.8)
102       FORMAT(1F11.4)
200       FORMAT(A15)
202       FORMAT(A11)
          DEALLOCATE(XX,YY,X_JK,Y_JK,F_JK,XY2,H,STAT=ALLOERR)
          RETURN
      END


      SUBROUTINE FFTINTEN(PCOUNT,XPLT,YPLT,F1PLT,F2PLT,REFHT,IJ
     1,WVNUMB)
          IMPLICIT NONE
!      REAL SPACER
          INTEGER ROT,I,J,ALLOERR,PCOUNT,IJ,WVNUMB
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          REAL F1PLT,F2PLT,XPLT,YPLT,FTF1,FTF2,REFHT
C
          DIMENSION FTF1(:,:),F1PLT(PCOUNT,PCOUNT),XPLT(PCOUNT),YPLT(PCOUNT)
     1    ,F2PLT(PCOUNT,PCOUNT),FTF2(:,:)
          ALLOCATABLE :: FTF1,FTF2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'dathgr.inc'
          DEALLOCATE(FTF1,FTF2,STAT=ALLOERR)
          ALLOCATE(FTF1(PCOUNT,PCOUNT),FTF2(PCOUNT,PCOUNT),STAT=ALLOERR)
C     FIX THE ARRAY SO IT PLOTS LIKE IN CODE-V
C     WE NEED TO REFLECT IN THE Y-DIRECTION
          ROT=0
          IF(ROTCAPFN) ROT=90
C
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  FTF1(I,J)=F1PLT(PCOUNT+1-I,J)
                  FTF2(I,J)=F2PLT(PCOUNT+1-I,J)
              END DO
          END DO
          DO J=1,PCOUNT
              DO I=1,PCOUNT
                  F1PLT(I,J)=FTF1(I,J)
                  F2PLT(I,J)=FTF2(I,J)
              END DO
          END DO
C     WE NEED TO REFLECT IN THE X-DIRECTION
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  FTF1(I,J)=F1PLT(PCOUNT+1-I,J)
                  FTF2(I,J)=F2PLT(PCOUNT+1-I,J)
              END DO
          END DO
          DO J=1,PCOUNT
              DO I=1,PCOUNT
                  F1PLT(I,J)=FTF1(I,J)
                  F2PLT(I,J)=FTF2(I,J)
              END DO
          END DO
          IF(ROT.EQ.90) THEN
C     ROTATE THE F ARRAY BY 90 DEGREES (CLOCKWISE LOOKING DOWN
C     FROM THE PLUS INTENSITY Z AXIS TOWARD THE MINUS INTENSITY
C     Z AXIS. STORE THE TEMPORARY FUNCTION IN ARRAY FTF
              DO J=1,PCOUNT
                  DO I=1,PCOUNT
                      FTF1(I,J)=F1PLT(PCOUNT+1-J,I)
                      FTF2(I,J)=F2PLT(PCOUNT+1-J,I)
                  END DO
              END DO
              DO J=1,PCOUNT
                  DO I=1,PCOUNT
                      F1PLT(I,J)=FTF1(I,J)
                      F2PLT(I,J)=FTF2(I,J)
                  END DO
              END DO
          END IF
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  F1PLT(I,J)=(F1PLT(I,J))
                  IF(F2PLT(I,J).EQ.0.0) THEN
                      F1PLT(I,J)=0.0
                  END IF
              END DO
          END DO
          DO I=1,PCOUNT
              XPLT(I)=((XPLT(I))*1500.0)+1500.0
              YPLT(I)=((YPLT(I))*1500.0)+1500.0
          END DO
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  IF(F2PLT(I,J).EQ.0.0D0) F1PLT(I,J)=0.0
                  F1PLT(I,J)=(F1PLT(I,J))*2000.0
                  F2PLT(I,J)=(F2PLT(I,J))*2000.0
              END DO
          END DO
C
C     NOW FUNCTION GOES FROM 0 TO 2000 AND XPLT AND YPLT GO FROM
C     0 TO +2000 EACH
C
C     THE PLOT WILL BE SEEN IN ORTHOGRAPHIC PROJECTION, XPLT ACROSS THE SCREEN,
C     YPLT INTO THE SCREEN AT 45 DEG EL AND AZ AND FPLT UP ON THE SCREEN
C

          CALL PLOTCAPFN(PCOUNT,XPLT,YPLT,F1PLT,F2PLT,ROT,IJ,REFHT,WVNUMB)
C
          DEALLOCATE(FTF1,FTF2,STAT=ALLOERR)
          RETURN
      END
