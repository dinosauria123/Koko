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

C       TENTH FILE OF PLOT/CAD ROUTINES

C SUB USER_CONTOUR.FOR
      SUBROUTINE USER_CONTOUR(IIU)
C
          IMPLICIT NONE
C
!        REAL REFHT
C
          REAL*8 WVAL
C
!       LOGICAL PLOTIT(1:10)
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          INTEGER KKV,KKK,IU,IIU
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
          PLOTCAPCON=.TRUE.
C
          AUTOZSCALE=.TRUE.
C
          IF(DF2.EQ.1) IIU=0
          IF(DF2.EQ.0) IIU=1
C
          IF(STI.EQ.1) THEN
              OUTLYNE= '"USERCONT" PLOTS THE EXISTING USER-DEFINED CONTOUR PLOT'
              CALL SHOWIT(1)
              OUTLYNE= '"AS A CONTOUR PLOT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE= '"USERCONT" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE= 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       OPEN DATA FILE, READ NUMBER OF DATA POINTS
C       READ X,Y,Z DATA SETS
C       READ VERTICAL AND HORIZONTAL PLOT AXIS LABELS
C       READ VERTICAL AND HORIZONTAL PLOT AXIS UNITS
C       READ Z-AXIS LABEL AND UNITS
C
C
C     NOW CALL ROUTINES TO DO THE CONTOUR PLOT
          CALL CON_USERPLOT(1,WVAL,KKV,KKK,IU)
C
          RETURN
      END



C SUB CON_USERPLOT.FOR
      SUBROUTINE CON_USERPLOT(IJ,WVAL,KKV,KKK,IU)
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
          INTEGER ALLOERR,WVNUMB,IU
C
          REAL*8 WVAL
          REAL ZMAX2,ZMIN2,RI,RII,ZSTEP
          REAL F1PLT,F2PLT,DELGRID
C
          REAL ZMAX1,ZMIN1
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          DIMENSION F1PLT(:,:),F2PLT(:,:)
C
          ALLOCATABLE :: F1PLT,F2PLT
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          KKI2=NINT(SQRT(FLOAT(KKV)))
          DEALLOCATE(F1PLT,F2PLT,STAT=ALLOERR)
          ALLOCATE(F1PLT(KKK,KKK),
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

          DELGRID=1.0/(FLOAT(KKI2)-1.0)
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
C     WE NOW HAVE PLOTABLE ARRAYS F1PLT AND F2PLT
C     THE ARRAYS ARE DIMENSIONED 1 BY KKI2
C     THE REFERENCE APERTURE WIDTH IS REFHT, MIN AND MAX Z VALUES
C     ARE 0 AND 1.0
C     IJ IS 1 FOR OPD AND 2 FOR INTENSITY
C     WAVLENGTH NUMBER IS WVNUMB
          ZSTEP=0.0
          IF(W3.GT.0.0D0.AND.IJ.EQ.1) ZSTEP=SNGL(W3)
          CALL PLTCONT(KKI2,F1PLT,F2PLT,IJ
     1    ,WVNUMB,ZSTEP,IU)
          DEALLOCATE(F1PLT,F2PLT,STAT=ALLOERR)
          RETURN
      END



      SUBROUTINE USERCONT(PCOUNT,F1PLT,F2PLT,IJ
     1,WVNUMB,ZSTEP,IU)
          IMPLICIT NONE
!      REAL SPACER
          INTEGER I,J,ALLOERR,PCOUNT,COLPAS,IJ,WVNUMB,NSTEP,IU
C
          CHARACTER B*10,WAVVAL*10
C
          REAL ZMAX1,ZMIN1,ZSTEP,OPDPEAK,OPDPIT
C
          LOGICAL AUTOZSCALE
C
          COMMON/AUTSCALE/ZMAX1,ZMIN1,AUTOZSCALE
C
          REAL F1PLT,F2PLT,FTF1,FTF2
C
          INTEGER NT1SIZ,NT1ANG,IF1PLT,IF2PLT
C
          DIMENSION IF1PLT(PCOUNT,PCOUNT)
     1    ,IF2PLT(PCOUNT,PCOUNT)
C
          DIMENSION FTF1(:,:),F1PLT(PCOUNT,PCOUNT)
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
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  F1PLT(I,J)=(F1PLT(I,J))
                  IF(F2PLT(I,J).EQ.0.0) THEN
                      F1PLT(I,J)=0.0
                  END IF
              END DO
          END DO
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  IF(F2PLT(I,J).EQ.0.0) F1PLT(I,J)=0.0
                  F1PLT(I,J)=(F1PLT(I,J))*5000.0
                  IF1PLT(I,J)=INT(F1PLT(I,J))
                  F2PLT(I,J)=(F2PLT(I,J))*5000.0
                  IF2PLT(I,J)=INT(F2PLT(I,J))
              END DO
          END DO
C
C
C     NSTEP IS DISTANCE IN Z BETWEEN CONTOURS IN D. I. U.
          IF(AUTOZSCALE.OR.ZSTEP.EQ.0.0) THEN
              NSTEP=500
          ELSE
              IF(IJ.EQ.1) NSTEP=NINT((ABS(ZSTEP/(ZMAX1-ZMIN1)))*5000.0)
              IF(IJ.EQ.2) NSTEP=500
          END IF
C     CALL PLOT CONTOUR HERE
          IF(NSTEP.LT.250) NSTEP=250
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
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT FRAME'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='PLOT LI'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          CALL MY_PLOT(5400,1500,0,0,0,10000,0,7000)
          CALL MY_PLOT(5400,2000,1,0,0,10000,0,7000)
          CALL MY_PLOT(5400,1500,0,0,0,10000,0,7000)
          CALL MY_PLOT(5900,1500,1,0,0,10000,0,7000)
          CALL MY_JUSTSTRING(5285,2040,'+Y',0,1,3)
          CALL MY_JUSTSTRING(5940,1470,'+X',0,1,3)
          IF(WVNUMB.GE.1.AND.WVNUMB.LE.5) THEN
              WRITE(B,180)REAL(SYSTEM1(WVNUMB))
          END IF
          IF(WVNUMB.GE.6.AND.WVNUMB.LE.10) THEN
              WRITE(B,180)REAL(SYSTEM1(65+WVNUMB))
          END IF
          READ(B,200) WAVVAL
180       FORMAT(G10.4)
200       FORMAT(A10)
          IF(IJ.EQ.1) THEN
              CALL MY_JUSTSTRING(100,250,
     1        'WAVEFRONT CONTOUR MAP AT WAVELENGTH = '
     2        //TRIM(WAVVAL)//' MICRON(S)',0,1,3)
              CALL MY_JUSTSTRING(6550,5400,
     1        'WAVEFRONT CONTOUR LEVELS'
     2        ,0,1,3)
          END IF
          IF(IJ.EQ.2) THEN
              CALL MY_JUSTSTRING(100,250,
     1        'PUPIL INTENSITY MAP AT WAVELENGTH = '
     2        //TRIM(WAVVAL)//' MICRON(S)',0,1,3)
              CALL MY_JUSTSTRING(6550,5400,
     1        'RELATIVE PUPIL INTENSITY'
     2        ,0,1,3)
          END IF
C     OPD
          OPDPEAK=ZMAX1
          OPDPIT=ZMIN1
          IF(IJ.EQ.1)
     1    CALL PLOT_CONTOUR_OPD(PCOUNT,IF1PLT,NSTEP,IU,OPDPEAK,OPDPIT)
C     APD
          IF(IJ.EQ.2)
     1    CALL PLOT_CONTOUR_APD(PCOUNT,IF2PLT,NSTEP,IU)
C
          DEALLOCATE(FTF1,FTF2,STAT=ALLOERR)
          RETURN
      END
