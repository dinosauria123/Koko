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

C       EIGHTH FILE OF CAPFN/SPOT ROUTINES

      SUBROUTINE MAPFIELDOPD
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          INTEGER I,J,ALLOERR,II,JJ,L,K
          REAL*8 DW1,DW2,DW3,DW4,DI,DJ
          REAL OPDARRAY
          DIMENSION OPDARRAY(:,:,:)
          ALLOCATABLE :: OPDARRAY
          CHARACTER*8 OWC
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'RMSMAP') THEN
                  OUTLYNE='"RMSMAP" PERFORMS FOV RMS OPD ANALYSIS'
                  CALL SHOWIT(1)
              END IF
              IF(WC.EQ.'STRLMAP') THEN
                  OUTLYNE='"STRLMAP" PERFORMS FOV, STREHL RATIO ANALYSIS'
                  CALL SHOWIT(1)
              END IF
              IF(WC.EQ.'PTVMAP') THEN
                  OUTLYNE='"PTVMAP" PERFORMS FOV PEAK TO VALLEY OPD ANALYSIS'
                  CALL SHOWIT(1)
              END IF
          END IF
          IF(WC.NE.'STRLMAP') THEN
              IF(SQ.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO QUALIFIER, STRING OR NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
              IF(SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' TAKES NO QUALIFIER, STRING OR NUMERIC WORD #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.NE.'STRLMAP') THEN
              IF(DF1.EQ.1) THEN
                  W1=INT(SYSTEM1(11))
              END IF
              IF(W1.LT.1.0D0.OR.W1.GT.10.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WAVELENGTH NUMBER MUST BE IN THE RANGE 1 TO 10'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1) THEN
                  W2=16.0D0
              END IF
              IF(W2.LT.16.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NRD MUST BE 16 OR GREATER'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.16.0D0.OR.W2.GT.512.0D0.OR.
     1        (W2-DBLE(INT(W2))).NE.0.0D0.OR.W2.GT.512.0D0.OR.
     2        ((W2/2.0D0)-DBLE(INT(W2/2.0D0))).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"NRD" REQUIERS NUMERIC INPUT TO BE A POSITIVE EVEN INTEGER'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'EVEN INTEGER VALUE FROM 16 TO 512'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) '"NRD" RESET TO 16'
                  CALL SHOWIT(1)
              END IF
              IF(DF3.EQ.1) THEN
                  W3=3.0D0
              END IF
              IF(W3.LT.3.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMBER OF FIELD POINT IN ONE DIMENSION MUST BE 3 OR GREATER'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
              IF(DF1.EQ.1) THEN
                  W1=INT(SYSTEM1(11))
              END IF
              IF(W1.LT.1.0D0.OR.W1.GT.10.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WAVELENGTH NUMBER MUST BE IN THE RANGE 1 TO 10'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1) THEN
                  W2=3.0D0
              END IF
              IF(W2.LT.2.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMBER OF FIELD POINT IN ONE DIMENSION MUST BE 3 OR GREATER'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.NE.'STRLMAP') THEN
              DW1=W1
              DW2=W2
              DW3=W3
              DW4=W4
              OWC=WC
              DEALLOCATE(OPDARRAY,STAT=ALLOERR)
              ALLOCATE(OPDARRAY(1:3,1:INT(DW3),1:INT(DW3)))
              INPUT='AIMRAY ON'
              CALL PROCES
C
              II=0
C
              DO I=-(INT(DW3)-1),(INT(DW3)-1),2
                  II=II+1
                  JJ=0
                  DI=DBLE(I)/DBLE(INT(DW3)-1)
                  DO J=-(INT(DW3)-1),(INT(DW3)-1),2
                      JJ=JJ+1
                      DJ=DBLE(J)/DBLE(INT(DW3)-1)
                      WRITE(INPUT,10) DI,DJ,INT(DW1)
 10                   FORMAT('FOB,',D23.15,',',D23.15,',0.0,',I2)
                      CALL PROCES
                      WRITE(INPUT,15) INT(DW2)
 15                   FORMAT('CAPFN SILENT',I3)
                      CALL PROCES
                      OPDARRAY(1,JJ,II)=SNGL(PTOVOPD(INT(DW1)))
                      OPDARRAY(2,JJ,II)=SNGL(RMSOP(INT(DW1)))
                  END DO
              END DO
              IF(OWC.EQ.'PTVMAP') K=1
              IF(OWC.EQ.'RMSMAP') K=2
              L=INT(DW1)
C       ARRAY LOADED. PLOT IT
              CALL PLTMAP(INT(DW3),OPDARRAY,K,L)
              DEALLOCATE(OPDARRAY,STAT=ALLOERR)
              RETURN
          ELSE
C       STRLMAP
              DW1=W1
              DW2=W2
              DW3=W3
              DW4=W4
              OWC=WC
              DEALLOCATE(OPDARRAY,STAT=ALLOERR)
              ALLOCATE(OPDARRAY(1:3,1:INT(DW2),1:INT(DW2)))
              INPUT='AIMRAY ON'
              CALL PROCES
C
              II=0
C
              INPUT='OUT NULL'
              CALL PROCES
              DO I=-(INT(DW2)-1),(INT(DW2)-1),2
                  II=II+1
                  JJ=0
                  DI=DBLE(I)/DBLE(INT(DW2)-1)
                  DO J=-(INT(DW2)-1),(INT(DW2)-1),2
                      JJ=JJ+1
                      DJ=DBLE(J)/DBLE(INT(DW2)-1)
                      WRITE(INPUT,11) DI,DJ,INT(DW1)
 11                   FORMAT('FOB,',D23.15,',',D23.15,',0.0,',I2)
                      CALL PROCES
                      WRITE(INPUT,16)
 16                   FORMAT('STREHL ACC')
                      CALL PROCES
                      IF(SNGL(REG(9)).LT.0.001) REG(9)=0.0D0
                      OPDARRAY(3,JJ,II)=SNGL(REG(9))
                  END DO
              END DO
              INPUT='OUT TP'
              CALL PROCES
              K=3
              L=INT(DW1)
C       ARRAY LOADED. PLOT IT
              CALL PLTMAP(INT(DW2),OPDARRAY,K,L)
              DEALLOCATE(OPDARRAY,STAT=ALLOERR)
          END IF
          RETURN
      END
      SUBROUTINE PLTMAP(PCOUNT,MY_ARRAY,K,L)
          IMPLICIT NONE
C
          CHARACTER B*10,WAVVAL*10
C
!      LOGICAL AUTOZSCALE
C
          REAL ZMAX,ZMIN,MY_ARRAY
C
          INTEGER NT1SIZ,NT1ANG,IF1PLT,K,IU,L,COLPAS,NSTEP,I,J,PCOUNT
C
          DIMENSION IF1PLT(PCOUNT,PCOUNT),MY_ARRAY(1:3,PCOUNT,PCOUNT)
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'dathgr.inc'

          ZMIN=1.0E30
          ZMAX=-1.0E30
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  IF(MY_ARRAY(K,I,J).GE.ZMAX) ZMAX=MY_ARRAY(K,I,J)
                  IF(MY_ARRAY(K,I,J).LT.ZMIN) ZMIN=MY_ARRAY(K,I,J)
              END DO
          END DO
          IF((ABS(ZMAX)-ABS(ZMIN)).EQ.0.0) THEN
              ZMAX=1.0
              ZMIN=0.0
          END IF
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  MY_ARRAY(K,I,J)=(MY_ARRAY(K,I,J)-ZMIN)/(ZMAX-ZMIN)
              END DO
          END DO
          DO I=1,PCOUNT
              DO J=1,PCOUNT
                  IF1PLT(I,J)=INT(MY_ARRAY(K,I,J)*5000.0)
              END DO
          END DO
C
C
          NSTEP=500
C     CALL PLOT CONTOUR HERE
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
          IF(L.GE.1.AND.L.LE.5) THEN
              WRITE(B,180)REAL(SYSTEM1(L))
          END IF
          IF(L.GE.6.AND.L.LE.10) THEN
              WRITE(B,180)REAL(SYSTEM1(65+L))
          END IF
          READ(B,200) WAVVAL
180       FORMAT(G10.4)
200       FORMAT(A10)
          IF(K.EQ.1) THEN
              CALL MY_JUSTSTRING(100,250,
     1        'PEAK TO VALLEY OPD MAP AT WAVELENGTH = '
     2        //TRIM(WAVVAL)//' MICROMETER',0,1,3)
              CALL MY_JUSTSTRING(6550,5400,
     1        'WAVEFRONT CONTOUR LEVELS'
     2        ,0,1,3)
          END IF
          IF(K.EQ.2) THEN
              CALL MY_JUSTSTRING(100,250,
     1        'RMS OPD MAP AT WAVELENGTH = '
     2        //TRIM(WAVVAL)//' MICROMETER',0,1,3)
              CALL MY_JUSTSTRING(6550,5400,
     1        'WAVEFRONT CONTOUR LEVELS'
     2        ,0,1,3)
          END IF
          IF(K.EQ.3) THEN
              CALL MY_JUSTSTRING(100,250,
     1        'PSF BASED STREHL RATIO AT WAVELENGTH = '
     2        //TRIM(WAVVAL)//' MICROMETER',0,1,3)
              CALL MY_JUSTSTRING(6550,5400,
     1        'STREHL RATIO CONTOUR LEVELS'
     2        ,0,1,3)
          END IF
          IU=1
          CALL PLOT_CONTOUR_OPD(PCOUNT,IF1PLT,NSTEP,IU,ZMAX,ZMIN)
          INPUT='DRAW'
          CALL PROCES
C
          RETURN
      END
