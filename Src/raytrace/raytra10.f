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

C      THIS IS THE TENTH FILE OF RAYTRACING ROUTINES

C SUB GBEAM.FOR
      SUBROUTINE GBEAM
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GBEAM. THIS SUBROUTINE IMPLEMENTS
C       "BEA" OR "BEAM" GAUSSIAN BEAM COMMAND
C
          INTEGER SF,I,ERROR
          REAL*8 OLDREF2,OLDSTOP
C
          SAVE OLDREF2,OLDSTOP
C
          REAL*8 PX,PY,PCX,PCY,PUX,PUY,PUCX,PUCY,DWORD2,DWORD3
     1    ,DWORD4,DWORD5,V1,V2,V3,V4,V5,V6,V7,V8,WRX,WRY,BDX,BDY
     2    ,VALVA,VAL1,VAL2
C
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          WRX=SYSTEM1(85)
          WRY=SYSTEM1(86)
          BDX=SYSTEM1(87)
          BDY=SYSTEM1(88)
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=SYSTEM1(11)
          DWORD2=W2
          DWORD3=W3
          DWORD4=W4
          DWORD5=W5
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"BEA" OR "BEAM" OUTPUTS GAUSSIAN BEAM PROPERTIES'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        'GAUSSIAN BEAM OUTPUT COMMANDS TAKE NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"BEA" AND "BEAM" TAKE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'QUALIFIER OR NUMERIC WORD #1 INPUT BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DWORD5.LT.1.0D0.OR.DWORD5.GT.10.0D0) THEN
              WRITE(OUTLYNE,*)'INVALID WAVELENGTH NUMBER ISSUED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO GAUSSIAN BEAM DATA EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO GAUSSIAN BEAM DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          OLDSTOP=SYSTEM1(26)
          OLDREF2=SYSTEM1(25)
          SYSTEM1(25)=1.0D0
          SYSTEM1(26)=1.0D0
C
C
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              CALL GNPRTGEN(0,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1        ,DWORD2,DWORD3,DWORD4,DWORD5,1)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DIFFERENTIAL RAY VALUES NOT CALCULABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  SYSTEM1(25)=OLDREF2
                  SYSTEM1(26)=OLDSTOP
                  RETURN
              END IF
              SF=INT(SYSTEM1(20))
              WRITE(OUTLYNE,6003)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6006) INT(DWORD5)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6007) DWORD2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6008) DWORD3
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6009) DWORD4
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6010) WRX
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6011) WRY
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6012) BDX
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6013) BDY
              CALL SHOWIT(0)
C
C     FIRST XZ-PLANE
C
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6000)
              CALL SHOWIT(0)
              DO I=0,SF
                  CALL GNPRTGEN(I,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1            ,DWORD2,DWORD3,DWORD4,DWORD5,1)
                  V1=DSQRT((PX**2)+(PCX**2))
                  IF(REAL(V1).EQ.0.0) THEN
                      VALVA=1.0D20
                  ELSE
                      VALVA=((PX*PUX)+(PCX*PUCX))/V1
                  END IF
                  IF(REAL(VALVA).EQ.0.0) THEN
                      V2=0.0D0
                  ELSE
                      V2=-V1/VALVA
                  END IF
                  IF(REAL((PUX**2)+(PUCX**2)).EQ.0.0) THEN
                      V3=1.0D20
                  ELSE
                      V3=-((PX*PUX)+(PCX*PUCX))/((PUX**2)+(PUCX**2))
                  END IF
                  VAL1=PX+(PUX*V3)
                  VAL2=PCX+(PUCX*V3)
                  V4=DSQRT((VAL1**2)+(VAL2**2))
                  WRITE(OUTLYNE,1500)I,V1,V2,V3,V4
                  CALL SHOWIT(0)
              END DO
C
C     SECOND YZ-PLANE
C
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6002) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6000)
              CALL SHOWIT(0)
              DO I=0,SF
                  CALL GNPRTGEN(I,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1            ,DWORD2,DWORD3,DWORD4,DWORD5,1)
                  V5=DSQRT((PY**2)+(PCY**2))
                  IF(REAL(V5).EQ.0.0) THEN
                      VALVA=1.0D20
                  ELSE
                      VALVA=((PY*PUY)+(PCY*PUCY))/V5
                  END IF
                  IF(REAL(VALVA).EQ.0.0) THEN
                      V6=0.0D0
                  ELSE
                      V6=-V5/VALVA
                  END IF
                  IF(REAL((PUY**2)+(PUCY**2)).EQ.0.0) THEN
                      V7=1.0D20
                  ELSE
                      V7=-((PY*PUY)+(PCY*PUCY))/((PUY**2)+(PUCY**2))
                  END IF
                  VAL1=PY+(PUY*V7)
                  VAL2=PCY+(PUCY*V7)
                  V8=DSQRT((VAL1**2)+(VAL2**2))
                  WRITE(OUTLYNE,1500)I,V5,V6,V7,V8
                  CALL SHOWIT(0)
              END DO
              SYSTEM1(25)=OLDREF2
              SYSTEM1(26)=OLDSTOP
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=0
              CALL GNPRTGEN(SF,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1        ,DWORD2,DWORD3,DWORD4,DWORD5,1)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DIFFERENTIAL RAY VALUES NOT CALCULABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  SYSTEM1(25)=OLDREF2
                  SYSTEM1(26)=OLDSTOP
                  RETURN
              END IF
              V1=DSQRT((PX**2)+(PCX**2))
              IF(REAL(V1).EQ.0.0) THEN
                  VALVA=1.0D20
              ELSE
                  VALVA=((PX*PUX)+(PCX*PUCX))/V1
              END IF
              IF(REAL(VALVA).EQ.0.0) THEN
                  V2=0.0D0
              ELSE
                  V2=-V1/VALVA
              END IF
              IF(REAL((PUX**2)+(PUCX**2)).EQ.0.0) THEN
                  V3=1.0D20
              ELSE
                  V3=-((PX*PUX)+(PCX*PUCX))/((PUX**2)+(PUCX**2))
              END IF
              VAL1=PX+(PUX*V3)
              VAL2=PCX+(PUCX*V3)
              V4=DSQRT((VAL1**2)+(VAL2**2))
              V5=DSQRT((PY**2)+(PCY**2))
              IF(REAL(V5).EQ.0.0) THEN
                  VALVA=1.0D20
              ELSE
                  VALVA=((PY*PUY)+(PCY*PUCY))/V5
              END IF
              IF(REAL(VALVA).EQ.0.0) THEN
                  V6=0.0D0
              ELSE
                  V6=-V5/VALVA
              END IF
              IF(REAL((PUY**2)+(PUCY**2)).EQ.0.0) THEN
                  V7=1.0D20
              ELSE
                  V7=-((PY*PUY)+(PCY*PUCY))/((PUY**2)+(PUCY**2))
              END IF
              VAL1=PY+(PUY*V7)
              VAL2=PCY+(PUCY*V7)
              V8=DSQRT((VAL1**2)+(VAL2**2))
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6003)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6006) INT(DWORD5)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6007) DWORD2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6008) DWORD3
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6009) DWORD4
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6010) WRX
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6011) WRY
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6012) BDX
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6013) BDY
              CALL SHOWIT(0)
C     XZ-PLANE
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1500) SF,V1,V2,V3,V4
              CALL SHOWIT(0)
C     YZ-PLANE
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6002) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1500) SF,V5,V6,V7,V8
              CALL SHOWIT(0)
              SYSTEM1(25)=OLDREF2
              SYSTEM1(26)=OLDSTOP
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              SYSTEM1(25)=OLDREF2
              SYSTEM1(26)=OLDSTOP
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=INT(SYSTEM1(20))
              CALL GNPRTGEN(SF,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1        ,DWORD2,DWORD3,DWORD4,DWORD5,1)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DIFFERENTIAL RAY VALUES NOT CALCULABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  SYSTEM1(25)=OLDREF2
                  SYSTEM1(26)=OLDSTOP
                  RETURN
              END IF
              V1=DSQRT((PX**2)+(PCX**2))
              IF(REAL(V1).EQ.0.0) THEN
                  VALVA=1.0D20
              ELSE
                  VALVA=((PX*PUX)+(PCX*PUCX))/V1
              END IF
              IF(REAL(VALVA).EQ.0.0) THEN
                  V2=0.0D0
              ELSE
                  V2=-V1/VALVA
              END IF
              IF(REAL((PUX**2)+(PUCX**2)).EQ.0.0) THEN
                  V3=1.0D20
              ELSE
                  V3=-((PX*PUX)+(PCX*PUCX))/((PUX**2)+(PUCX**2))
              END IF
              VAL1=PX+(PUX*V3)
              VAL2=PCX+(PUCX*V3)
              V4=DSQRT((VAL1**2)+(VAL2**2))
              V5=DSQRT((PY**2)+(PCY**2))
              IF(REAL(V5).EQ.0.0) THEN
                  VALVA=1.0D20
              ELSE
                  VALVA=((PY*PUY)+(PCY*PUCY))/V5
              END IF
              IF(REAL(VALVA).EQ.0.0) THEN
                  V6=0.0D0
              ELSE
                  V6=-V5/VALVA
              END IF
              IF(REAL((PUY**2)+(PUCY**2)).EQ.0.0) THEN
                  V7=1.0D20
              ELSE
                  V7=-((PY*PUY)+(PCY*PUCY))/((PUY**2)+(PUCY**2))
              END IF
              VAL1=PY+(PUY*V7)
              VAL2=PCY+(PUCY*V7)
              V8=DSQRT((VAL1**2)+(VAL2**2))
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6003)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6006) INT(DWORD5)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6007) DWORD2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6008) DWORD3
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6009) DWORD4
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6010) WRX
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6011) WRY
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6012) BDX
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6013) BDY
              CALL SHOWIT(0)
C     XZ-PLANE
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1500) SF,V1,V2,V3,V4
              CALL SHOWIT(0)
C     YZ-PLANE
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6002) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1500) SF,V5,V6,V7,V8
              CALL SHOWIT(0)
              SYSTEM1(25)=OLDREF2
              SYSTEM1(26)=OLDSTOP
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  SYSTEM1(25)=OLDREF2
                  SYSTEM1(26)=OLDSTOP
                  RETURN
              END IF
              CALL GNPRTGEN(I,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1        ,DWORD2,DWORD3,DWORD4,DWORD5,1)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DIFFERENTIAL RAY VALUES NOT CALCULABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  SYSTEM1(25)=OLDREF2
                  SYSTEM1(26)=OLDSTOP
                  RETURN
              END IF
              V1=DSQRT((PX**2)+(PCX**2))
              IF(REAL(V1).EQ.0.0) THEN
                  VALVA=1.0D20
              ELSE
                  VALVA=((PX*PUX)+(PCX*PUCX))/V1
              END IF
              IF(REAL(VALVA).EQ.0.0) THEN
                  V2=0.0D0
              ELSE
                  V2=-V1/VALVA
              END IF
              IF(REAL((PUX**2)+(PUCX**2)).EQ.0.0) THEN
                  V3=1.0D20
              ELSE
                  V3=-((PX*PUX)+(PCX*PUCX))/((PUX**2)+(PUCX**2))
              END IF
              VAL1=PX+(PUX*V3)
              VAL2=PCX+(PUCX*V3)
              V4=DSQRT((VAL1**2)+(VAL2**2))
              V5=DSQRT((PY**2)+(PCY**2))
              IF(REAL(V5).EQ.0.0) THEN
                  VALVA=1.0D20
              ELSE
                  VALVA=((PY*PUY)+(PCY*PUCY))/V5
              END IF
              IF(REAL(VALVA).EQ.0.0) THEN
                  V6=0.0D0
              ELSE
                  V6=-V5/VALVA
              END IF
              IF(REAL((PUY**2)+(PUCY**2)).EQ.0.0) THEN
                  V7=1.0D20
              ELSE
                  V7=-((PY*PUY)+(PCY*PUCY))/((PUY**2)+(PUCY**2))
              END IF
              VAL1=PY+(PUY*V7)
              VAL2=PCY+(PUCY*V7)
              V8=DSQRT((VAL1**2)+(VAL2**2))
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6003)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6006) INT(DWORD5)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6007) DWORD2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6008) DWORD3
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6009) DWORD4
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6010) WRX
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6011) WRY
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6012) BDX
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6013) BDY
              CALL SHOWIT(0)
C     XZ-PLANE
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6001) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)I,V1,V2,V3,V4
              CALL SHOWIT(0)
C     YZ-PLANE
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6002) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6000)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1500)I,V5,V6,V7,V8
              CALL SHOWIT(0)
          END IF
 1500     FORMAT(I3,7X,G13.6,3X,G13.6,4X,G13.6,5X,G13.6)
 6000     FORMAT('SURF',6X,'BEAM SEMI-DIA.',2X,'WAVFNT RADIUS'
     1    ,4X,'DIST TO WAIST',5X,'WAIST SEMI-DIA.')
 6001     FORMAT('GAUSSIAN BEAM DATA (XZ-PLANE)',
     1    ' - (CFG #',I2,')')
 6002     FORMAT('GAUSSIAN BEAM DATA (YZ-PLANE)',
     1    ' - (CFG #',I2,')')
C
 6003     FORMAT('GAUSSIAN BEAM ANALYSIS')
C
 2501     FORMAT(1X)
 6006     FORMAT('TRACED AT WAVELENGTH NUMBER = ',I2)
 6007     FORMAT('TRACED FROM Y-FOB =  ',G13.6)
 6008     FORMAT('TRACED FROM X-FOB =  ',G13.6)
 6009     FORMAT('WITH OBJ. Z-SHIFT =  ',G13.6,' LENS UNITS')
 6010     FORMAT(
     1    '      XZ-PLANE INPUT SEMI-DIA. =  ',G13.6,' LENS UNITS')
 6011     FORMAT(
     1    '      YZ-PLANE INPUT SEMI-DIA. =  ',G13.6,' LENS UNITS')
 6012     FORMAT(
     1    'XZ-PLANE DIVERGENCE HALF-ANGLE =  ',G13.6,' MILLIRADIANS')
 6013     FORMAT(
     1    'YZ-PLANE DIVERGENCE HALF-ANGLE =  ',G13.6,' MILLIRADIANS')
C
      END


C SUB GLPRY.FOR
      SUBROUTINE GLPRY
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GLPRY.FOR. THIS SUBROUTINE IMPLEMENTS
C       CREATION OF GLOBAL RAY DATA FOR THE LAST RAY TRACED.
C       FOR RAY PLOTTING IT IS CALLED BY RAYTRA.FOR
C
          INTEGER JK,I,N_HITS
C
          REAL*8 GRX,GRY,GRZ,GRL,GRM,GRN,GRLP,GRMP,GRNP,
     2    X1,Y1,Z1,L1,M1,N1,X00,Y00,Z0,LX0,LY0,LZ0,
     5    MX0,MY0,MZ0,NX0,NY0,NZ0,LO1,MO1,NO1
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(DABS(ALENS(3,NEWOBJ)).LE.1.0D10) THEN
C       FINITE OBJECT THICKNESS
              JK=NEWOBJ
          ELSE
C       INFINITE OBJECT THICKNESS
              JK=NEWOBJ+1
          END IF
C
          DO 10 I=JK,NEWIMG
C
C       NOW TRANSFORM TO THE GLOBAL COORDINATE SYSTEM.
C       THE GLOBAL COORDINATES OF EACH LOCAL COORDINATE
C       SYSTEM ARE:
C
              X00=VERTEX(1,I)
              Y00=VERTEX(2,I)
              Z0=VERTEX(3,I)
              LX0=VERTEX(4,I)
              MX0=VERTEX(5,I)
              NX0=VERTEX(6,I)
              LY0=VERTEX(7,I)
              MY0=VERTEX(8,I)
              NY0=VERTEX(9,I)
              LZ0=VERTEX(10,I)
              MZ0=VERTEX(11,I)
              NZ0=VERTEX(12,I)
C
              X1=X00+((LX0*(RAYRAY(1,I)))+(LY0*(RAYRAY(2,I)))
     1        +(LZ0*(RAYRAY(3,I))))
              Y1=Y00+((MX0*(RAYRAY(1,I)))+(MY0*(RAYRAY(2,I)))
     1        +(MZ0*(RAYRAY(3,I))))
              Z1=Z0+((NX0*(RAYRAY(1,I)))+(NY0*(RAYRAY(2,I)))
     1        +(NZ0*(RAYRAY(3,I))))
C
              L1=((LX0*(RAYRAY(4,I)))+(LY0*(RAYRAY(5,I)))
     1        +(LZ0*(RAYRAY(6,I))))
              M1=((MX0*(RAYRAY(4,I)))+(MY0*(RAYRAY(5,I)))
     1        +(MZ0*(RAYRAY(6,I))))
              N1=((NX0*(RAYRAY(4,I)))+(NY0*(RAYRAY(5,I)))
     1        +(NZ0*(RAYRAY(6,I))))
C
              LO1=((LX0*(RAYRAY(19,I)))+(LY0*(RAYRAY(20,I)))
     1        +(LZ0*(RAYRAY(21,I))))
              MO1=((MX0*(RAYRAY(19,I)))+(MY0*(RAYRAY(20,I)))
     1        +(MZ0*(RAYRAY(21,I))))
              NO1=((NX0*(RAYRAY(19,I)))+(NY0*(RAYRAY(20,I)))
     1        +(NZ0*(RAYRAY(21,I))))
C
              GLPRAY(1,I)=X1
              GLPRAY(2,I)=Y1
              GLPRAY(3,I)=Z1
              GLPRAY(4,I)=L1
              GLPRAY(5,I)=M1
              GLPRAY(6,I)=N1
              GLPRAY(7,I)=LO1
              GLPRAY(8,I)=MO1
              GLPRAY(9,I)=NO1
              GLVIRT(I)=.FALSE.
C     IF THE CURRENT SURFACE IS A TRUE DUMMY
              IF(DUM(I).AND.ALENS(34,I).EQ.0.0D0
     1        .AND.I.NE.NEWIMG) GLVIRT(I)=.TRUE.
              IF(I.EQ.NEWIMG) GLVIRT(I)=.FALSE.

C       THE CURRENT ENTRY FOR GLPRY REPRESENTS THE RAY AT THE BEGINNING
C       OF AN NSS OR GRIN ENVIRONMENT. NOW POPULATE THE ARRAY
C       GLOBAL_MULTIRAY_DATA STORED AT I TO REPRESENT THE INTERMEDIATE
C       RAY COORDINATES BETWEEN I AND I+1
              IF(I.GE.1.AND.NUMHITS(I-1).GT.1) THEN
                  DO N_HITS=1,NUMHITS(I-1)
                      X00=VERTEX(1,I-1)
                      Y00=VERTEX(2,I-1)
                      Z0=VERTEX(3,I-1)
                      LX0=VERTEX(4,I-1)
                      MX0=VERTEX(5,I-1)
                      NX0=VERTEX(6,I-1)
                      LY0=VERTEX(7,I-1)
                      MY0=VERTEX(8,I-1)
                      NY0=VERTEX(9,I-1)
                      LZ0=VERTEX(10,I-1)
                      MZ0=VERTEX(11,I-1)
                      NZ0=VERTEX(12,I-1)
                      GRX=MULTI_DATA(1,I-1,N_HITS)
                      GRY=MULTI_DATA(2,I-1,N_HITS)
                      GRZ=MULTI_DATA(3,I-1,N_HITS)
                      GRL=MULTI_DATA(4,I-1,N_HITS)
                      GRM=MULTI_DATA(5,I-1,N_HITS)
                      GRN=MULTI_DATA(6,I-1,N_HITS)
                      GRLP=MULTI_DATA(19,I-1,N_HITS)
                      GRMP=MULTI_DATA(20,I-1,N_HITS)
                      GRNP=MULTI_DATA(21,I-1,N_HITS)
                      X1=X00+((LX0*(GRX))+(LY0*(GRY))
     1                +(LZ0*(GRZ)))
                      Y1=Y00+((MX0*(GRX))+(MY0*(GRY))
     1                +(MZ0*(GRZ)))
                      Z1=Z0+((NX0*(GRX))+(NY0*(GRY))
     1                +(NZ0*(GRZ)))
C
                      L1=((LX0*(GRL))+(LY0*(GRM))
     1                +(LZ0*(GRN)))
                      M1=((MX0*(GRL))+(MY0*(GRM))
     1                +(MZ0*(GRN)))
                      N1=((NX0*(GRL))+(NY0*(GRM))
     1                +(NZ0*(GRN)))
C
                      LO1=((LX0*(GRLP))+(LY0*(GRMP))
     1                +(LZ0*(GRNP)))
                      MO1=((MX0*(GRLP))+(MY0*(GRMP))
     1                +(MZ0*(GRNP)))
                      NO1=((NX0*(GRLP))+(NY0*(GRMP))
     1                +(NZ0*(GRNP)))
C
                      GLOBAL_MULTIRAY_DATA(1,I-1,N_HITS)=X1
                      GLOBAL_MULTIRAY_DATA(2,I-1,N_HITS)=Y1
                      GLOBAL_MULTIRAY_DATA(3,I-1,N_HITS)=Z1
                      GLOBAL_MULTIRAY_DATA(4,I-1,N_HITS)=L1
                      GLOBAL_MULTIRAY_DATA(5,I-1,N_HITS)=M1
                      GLOBAL_MULTIRAY_DATA(6,I-1,N_HITS)=N1
                      GLOBAL_MULTIRAY_DATA(19,I-1,N_HITS)=LO1
                      GLOBAL_MULTIRAY_DATA(20,I-1,N_HITS)=MO1
                      GLOBAL_MULTIRAY_DATA(21,I-1,N_HITS)=NO1
                      GLOBAL_MULTIREF_DATA(1,I-1,N_HITS)=X1
                      GLOBAL_MULTIREF_DATA(2,I-1,N_HITS)=Y1
                      GLOBAL_MULTIREF_DATA(3,I-1,N_HITS)=Z1
                      GLOBAL_MULTIREF_DATA(4,I-1,N_HITS)=L1
                      GLOBAL_MULTIREF_DATA(5,I-1,N_HITS)=M1
                      GLOBAL_MULTIREF_DATA(6,I-1,N_HITS)=N1
                      GLOBAL_MULTIREF_DATA(19,I-1,N_HITS)=LO1
                      GLOBAL_MULTIREF_DATA(20,I-1,N_HITS)=MO1
                      GLOBAL_MULTIREF_DATA(21,I-1,N_HITS)=NO1
                  END DO
              END IF

 10       CONTINUE
C       ALL GLOBAL TRANSFORMATIONS ARE COMPLETE
C
          RETURN
      END
C SUB GLOBAL.FOR
      SUBROUTINE GLOBAL
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GLOBAL. THIS SUBROUTINE IMPLEMENTS
C       THE CMD COMMANDS "GLOBAL", "OFFSET" AND "VERTEX".
C
          CHARACTER UNIT*11
C
          INTEGER SF,I,JK
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          IF(SYSTEM1(6).EQ.1.0D0) UNIT='INCH'
          IF(SYSTEM1(6).EQ.2.0D0) UNIT='CENTIMETER'
          IF(SYSTEM1(6).EQ.3.0D0) UNIT='MILLIMETER'
          IF(SYSTEM1(6).EQ.4.0D0) UNIT='METER'
C
          IF(WC.EQ.'GLOBAL') THEN
C
              IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"',WC(1:6),
     2            '" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'OFF') THEN
                  WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "',
     1            WC(1:6),'" COMMAND'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"',WC(1:6),
     1            '" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.1.AND.INT(W1).LT.NEWOBJ.OR.S1.EQ.1.AND.
     1        INT(W1).GT.NEWIMG) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.SQ.EQ.0.AND.
     1        DF4.EQ.1.AND.DF5.EQ.1) THEN
C       PRINT CURRENT GLOBAL STATUS
                  IF(.NOT.GLOBE) THEN
                      WRITE(OUTLYNE,100)
                      CALL SHOWIT(0)
 100                  FORMAT('"GLOBAL" IS CURRENTLY SET TO "OFF"')
C                       RETURN
                  END IF
                  IF(GLOBE) THEN
                      WRITE(OUTLYNE,101) GLSURF
                      CALL SHOWIT(0)
 101                  FORMAT(
     1                '"GLOBAL" IS CURRENTLY "ON", REFERENCED TO SURFACE '
     2                ,I3)
                      WRITE(OUTLYNE,102) OFFX,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,103) OFFY,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,104) OFFZ,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,105) OFFA
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,106) OFFB
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,107) OFFC
                      CALL SHOWIT(0)
 102                  FORMAT('WITH X-OFFSET   = ',G18.10,1X,A11)
 103                  FORMAT('WITH Y-OFFSET   = ',G18.10,1X,A11)
 104                  FORMAT('WITH Z-OFFSET   = ',G18.10,1X,A11)
 105                  FORMAT('WITH ALPHA TILT = ',G18.10,' DEGREES')
 106                  FORMAT('WITH BETA  TILT = ',G18.10,' DEGREES')
 107                  FORMAT('WITH GAMMA TILT = ',G18.10,' DEGREES')
C                       RETURN
                  END IF
              END IF
              IF(WQ.EQ.'OFF') THEN
                  IF(GLOBE) THEN
C       SET REFERENCE AND OTHER RAY STATUS TO FALSE
                      FOBYES=.FALSE.
                      REFEXT=.FALSE.
                      SPDEXT=.FALSE.
                      GSPDEXT=.FALSE.
                      CPFNEXT=.FALSE.
                      CALL DELPSF
                      RAYEXT=.FALSE.
                      GLOBE=.FALSE.
                      GLSURF=0
                      OFFX=0.0D0
                      OFFY=0.0D0
                      OFFZ=0.0D0
                      OFFA=0.0D0
                      OFFB=0.0D0
                      OFFC=0.0D0
                      RETURN
                  END IF
                  IF(.NOT.GLOBE) THEN
                      RETURN
                  END IF
              END IF
              IF(S1.EQ.1) THEN
                  IF(DABS(ALENS(3,INT(W1))).GT.1.0D10.AND.INT(W1)
     1            .NE.NEWOBJ) THEN
                      WRITE(OUTLYNE,*)
     1                'THE GLOBAL SURFACE SPECIFIED ,SURFACE NUMBER ',INT(W1)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'HAS AN INFINITE THICKNESS ( MAGNITUDE GREATER THAN 1.0D+10)'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                '"GLOBAL" WILL YIELD UNCERTAIN RESULTS IN THIS CASE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'ADJUST THICKNESSES AND RE-ENTER THE "GLOBAL" COMMAND'
                      CALL SHOWIT(1)
                      REFEXT=.FALSE.
                      SPDEXT=.FALSE.
                      GSPDEXT=.FALSE.
                      CPFNEXT=.FALSE.
                      CALL DELPSF
                      FOBYES=.FALSE.
                      RAYEXT=.FALSE.
                      GLOBE=.FALSE.
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DABS(ALENS(3,INT(W1))).GT.1.0D10.AND.INT(W1)
     1            .EQ.NEWOBJ) THEN
                      WRITE(OUTLYNE,*)
     1                'GLOBAL SURFACE SELECTED WAS THE CURRENT OBJECT SURFACE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'IT HAS AN INFINITE THICKNESS ( MAGNITUDE GREATER THAN 1.0D+10)'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                '"GLOBAL" WILL YIELD UNCERTAIN RESULTS IN THIS CASE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'RE-ENTER "GLOBAL" WITH A DIFFERENT SURFACE NUMBER'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'OR A REDUCED OBJECT THICKNESS'
                      CALL SHOWIT(1)
                      REFEXT=.FALSE.
                      SPDEXT=.FALSE.
                      GSPDEXT=.FALSE.
                      CPFNEXT=.FALSE.
                      CALL DELPSF
                      FOBYES=.FALSE.
                      RAYEXT=.FALSE.
                      GLOBE=.FALSE.
                      CALL MACFAL
                      RETURN
                  END IF
C       CHECK FOR AND THICKNESSES GT 1.0D10
                  DO 1010 I=NEWOBJ+1,NEWIMG
                      IF(DABS(ALENS(3,I)).GT.1.0D10) THEN
                          WRITE(OUTLYNE,*)
     1                    'SURFACE NUMBER ',I
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'HAS AN INFINITE THICKNESS ( MAGNITUDE GREATER THAN 1.0D+10)'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    '"GLOBAL" WILL YIELD UNCERTAIN RESULTS IN THIS CASE'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'ADJUST THICKNESSES AND RE-ENTER THE "GLOBAL" COMMAND'
                          CALL SHOWIT(1)
                          REFEXT=.FALSE.
                          SPDEXT=.FALSE.
                          GSPDEXT=.FALSE.
                          CPFNEXT=.FALSE.
                          CALL DELPSF
                          FOBYES=.FALSE.
                          RAYEXT=.FALSE.
                          GLOBE=.FALSE.
                          CALL MACFAL
                          RETURN
                      END IF
 1010             CONTINUE
                  REFEXT=.FALSE.
                  SPDEXT=.FALSE.
                  GSPDEXT=.FALSE.
                  CPFNEXT=.FALSE.
                  CALL DELPSF
                  FOBYES=.FALSE.
                  RAYEXT=.FALSE.
                  GLOBE=.TRUE.
                  GLSURF=INT(W1)
                  OFFX=0.0D0
                  OFFY=0.0D0
                  OFFZ=0.0D0
                  OFFA=0.0D0
                  OFFB=0.0D0
                  OFFC=0.0D0
                  CALL GLVERT
                  RETURN
              END IF
          END IF
C
          IF(WC.EQ.'OFFSET') THEN
C
              IF(.NOT.GLOBE) THEN
                  WRITE(OUTLYNE,198)
                  CALL SHOWIT(0)
 199              FORMAT('"GLOBAL" IS CURRENTLY SET TO "OFF"')
                  WRITE(OUTLYNE,199)WC,WQ
                  CALL SHOWIT(0)
 198              FORMAT(A6,' ',A4,
     1            ' IS NOT A VALID COMMAND  WHEN "GLOBAL" IS SET TO "OFF"')
                  RETURN
              END IF
              IF(WQ.NE.'DEC'.AND.WQ.NE.'TILT') THEN
                  WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "',
     1            WC(1:6),'" COMMAND'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"',WC(1:6),' ',WQ(1:4),
     1            '" TAKES NO STRING OR NUMERIC WORD 4 OR 5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.SQ.EQ.0.AND.
     1        DF4.EQ.1.AND.DF5.EQ.1) THEN
C       PRINT CURRENT OFFSET STATUS
                  IF(.NOT.GLOBE) THEN
                      WRITE(OUTLYNE,200)
                      CALL SHOWIT(0)
 200                  FORMAT(
     1                '"GLOBAL" IS CURRENTLY SET TO "OFF", NO OFFSETS CAN EXIST')
                      RETURN
                  END IF
                  IF(GLOBE) THEN
                      WRITE(OUTLYNE,201) GLSURF
                      CALL SHOWIT(0)
 201                  FORMAT(
     1                '"GLOBAL" IS CURRENTLY "ON", REFERENCED TO SURFACE '
     2                ,I3)
                      WRITE(OUTLYNE,220)
                      CALL SHOWIT(0)
 220                  FORMAT('CURRENT OFFSET STATUS IS:')
                      WRITE(OUTLYNE,202) OFFX,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,203) OFFY,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,204) OFFZ,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,205) OFFA
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,206) OFFB
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,207) OFFC
                      CALL SHOWIT(0)
 202                  FORMAT('WITH X-OFFSET   = ',G18.10,1X,A11)
 203                  FORMAT('WITH Y-OFFSET   = ',G18.10,1X,A11)
 204                  FORMAT('WITH Z-OFFSET   = ',G18.10,1X,A11)
 205                  FORMAT('WITH ALPHA TILT = ',G18.10,' DEGREES')
 206                  FORMAT('WITH BETA  TILT = ',G18.10,' DEGREES')
 207                  FORMAT('WITH GAMMA TILT = ',G18.10,' DEGREES')
C                       RETURN
                  END IF
              END IF
              IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.SQ.EQ.0.AND.
     1        DF4.EQ.1.AND.DF5.EQ.1) THEN
C      OFFSET REQUIRES A QUALIFIER WITH NUMERIC INPUT
                  WRITE(OUTLYNE,*)
     1            '"OFFSET" REQUIRES A QUALIFIER WITH NUMERIC INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'DEC') THEN
                  RAYEXT=.FALSE.
                  REFEXT=.FALSE.
                  SPDEXT=.FALSE.
                  GSPDEXT=.FALSE.
                  CPFNEXT=.FALSE.
                  CALL DELPSF
                  FOBYES=.FALSE.
                  IF(DF1.EQ.1) W1=0.0D0
                  IF(DF2.EQ.1) W2=0.0D0
                  IF(DF3.EQ.1) W3=0.0D0
                  OFFX=W1
                  OFFY=W2
                  OFFZ=W3
C       GLVERT.FOR CALCULATES THE X,Y,Z OF EACH SURFACE
C       VERTEX IN THE COORDINATE SYSTEM OF THE GLOBAL REFERENCE
C       COORDINATE SYSTEM AND ALSO THE L,M AND N OF THE X,Y AND Z
C       AXES AT EACH SURFACE VERTEX IN THE COORDINATE SYSTEM
C       OF THE GLOBAL REFERENCE POINT.
                  CALL GLVERT
                  RETURN
              END IF
              IF(WQ.EQ.'TILT') THEN
                  RAYEXT=.FALSE.
                  REFEXT=.FALSE.
                  SPDEXT=.FALSE.
                  GSPDEXT=.FALSE.
                  CPFNEXT=.FALSE.
                  CALL DELPSF
                  FOBYES=.FALSE.
                  IF(DF1.EQ.1) W1=0.0D0
                  IF(DF2.EQ.1) W2=0.0D0
                  IF(DF3.EQ.1) W3=0.0D0
                  OFFA=W1
                  OFFB=W2
                  OFFC=W3
C       GLVERT.FOR CALCULATES THE X,Y,Z OF EACH SURFACE
C       VERTEX IN THE COORDINATE SYSTEM OF THE GLOBAL REFERENCE
C       COORDINATE SYSTEM AND ALSO THE L,M AND N OF THE X,Y AND Z
C       AXES AT EACH SURFACE VERTEX IN THE COORDINATE SYSTEM
C       OF THE GLOBAL REFERENCE POINT.
                  CALL GLVERT
                  RETURN
              END IF
C
          END IF

          IF(WC.EQ.'VERTEX') THEN
              IF(.NOT.GLOBE) THEN
C       NO VERTEX DATA TO OUTPUT
                  WRITE(OUTLYNE,*)
     1            'NO CURRENT GLOBAL VERTEX DATA EXISTS TO OUTPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"VERTEX" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"VERTEX" TAKES EITHER QUALIFIER OR NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'BUT NOT BOTH'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SYSTEM1(20).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO GLOBAL VERTEX DATA CAN EXIST'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
C       PRINT WARNING FOR OBJECT THICKNESS INFINITE
                  IF(DABS(ALENS(3,NEWOBJ)).GT.1.0D10)THEN
                      WRITE(OUTLYNE,*)'WARNING:'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'INFINITE OBJECT THICKNESS (MAGNITUDE GREATER THAN 1.0D+10'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'OBJECT THICKNESS WAS IGNORED IN ALL GLOBAL CALCULATIONS'
                      CALL SHOWIT(1)
                      JK=NEWOBJ+1
                  ELSE
                      JK=NEWOBJ
                  END IF
                  SF=NEWIMG
                  WRITE(OUTLYNE,5001) INT(F12)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5002) GLSURF
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) OFFX,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,103) OFFY,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,104) OFFZ,UNIT
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,105) OFFA
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,106) OFFB
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,107) OFFC
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5000)
                  CALL SHOWIT(0)
                  DO 10 I=JK,SF
                      WRITE(OUTLYNE,2000)I,VERTEX(1,I),VERTEX(2,I),VERTEX(3,I)
                      CALL SHOWIT(0)
 10               CONTINUE
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5003)
                  CALL SHOWIT(0)
                  DO 11 I=JK,SF
                      WRITE(OUTLYNE,2000)I,VERTEX(4,I),VERTEX(5,I),VERTEX(6,I)
                      CALL SHOWIT(0)
 11               CONTINUE
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5004)
                  CALL SHOWIT(0)
                  DO 12 I=JK,SF
                      WRITE(OUTLYNE,2000)I,VERTEX(7,I),VERTEX(8,I),VERTEX(9,I)
                      CALL SHOWIT(0)
 12               CONTINUE
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,5005)
                  CALL SHOWIT(0)
                  DO 13 I=JK,SF
                      WRITE(OUTLYNE,2000)I,VERTEX(10,I),VERTEX(11,I),VERTEX(12,I)
                      CALL SHOWIT(0)
 13               CONTINUE
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
                  SF=NEWOBJ
                  IF(DABS(ALENS(3,SF)).GT.1.0D10) THEN
                      WRITE(OUTLYNE,*)
     1                'NO GLOBAL RAY DATA EXISTS FOR THE OBJECT SURFACE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'WHICH HAS INFINTE THICKNESS (GREATER THAN 1.0D10)'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(HEADIN) THEN
                      WRITE(OUTLYNE,5001) INT(F12)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,5002) GLSURF
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,102) OFFX,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,103) OFFY,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,104) OFFZ,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,105) OFFA
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,106) OFFB
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,107) OFFC
                      CALL SHOWIT(0)
                  END IF
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)SF,VERTEX(1,SF),VERTEX(2,SF),VERTEX(3,SF)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5003)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)SF,VERTEX(4,SF),VERTEX(5,SF),VERTEX(6,SF)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5004)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)SF,VERTEX(7,SF),VERTEX(8,SF),VERTEX(9,SF)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5005)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)SF,VERTEX(10,SF),VERTEX(11,SF),VERTEX(12,SF)
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1        WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB') THEN
                  WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
                  SF=NEWIMG
                  IF(HEADIN) THEN
                      WRITE(OUTLYNE,5001) INT(F12)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,5002) GLSURF
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,102) OFFX,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,103) OFFY,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,104) OFFZ,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,105) OFFA
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,106) OFFB
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,107) OFFC
                      CALL SHOWIT(0)
                  END IF
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)SF,VERTEX(1,SF),VERTEX(2,SF),VERTEX(3,SF)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5003)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)SF,VERTEX(4,SF),VERTEX(5,SF),VERTEX(6,SF)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5004)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)SF,VERTEX(7,SF),VERTEX(8,SF),VERTEX(9,SF)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5005)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)SF,VERTEX(10,SF),VERTEX(11,SF),VERTEX(12,SF)
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(SQ.EQ.0.AND.DF1.NE.1) THEN
                  I=INT(W1)
                  IF(DABS(ALENS(3,I)).GT.1.0D10) THEN
                      WRITE(OUTLYNE,*)
     1                'NO GLOBAL RAY DATA EXISTS FOR THE OBJECT SURFACE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'WHICH HAS INFINTE THICKNESS (GREATER THAN 1.0D10)'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  SF=NEWIMG
                  IF(I.GT.SF.OR.I.LT.NEWOBJ) THEN
                      WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(HEADIN) THEN
                      WRITE(OUTLYNE,5001) INT(F12)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,5002) GLSURF
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,102) OFFX,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,103) OFFY,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,104) OFFZ,UNIT
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,105) OFFA
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,106) OFFB
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,107) OFFC
                      CALL SHOWIT(0)
                  END IF
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)I,VERTEX(1,I),VERTEX(2,I),VERTEX(3,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5003)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)I,VERTEX(4,I),VERTEX(5,I),VERTEX(6,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5004)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)I,VERTEX(7,I),VERTEX(8,I),VERTEX(9,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2501)
                  CALL SHOWIT(0)
                  IF(HEADIN) WRITE(OUTLYNE,5005)
                  IF(HEADIN) CALL SHOWIT(0)

                  WRITE(OUTLYNE,1500)I,VERTEX(10,I),VERTEX(11,I),VERTEX(12,I)
                  CALL SHOWIT(0)
                  RETURN
              END IF
 1500         FORMAT(I3,1X,G23.15,1X,G23.15,1X,G23.15)
 2000         FORMAT(I3,1X,G23.15,1X,G23.15,1X,G23.15)
 5000         FORMAT('SURF',7X,'X',23X,'Y',23X,'Z')
 5003         FORMAT('SURF',7X,'L(X)',20X,'M(X)',20X,'N(X)')
 5004         FORMAT('SURF',7X,'L(Y)',20X,'M(Y)',20X,'N(Y)')
 5005         FORMAT('SURF',7X,'L(Z)',20X,'M(Z)',20X,'N(Z)')
 5001         FORMAT('CURRENT GLOBAL VERTEX DATA ','
     1  - (CFG #',I2,')')
 5002         FORMAT(
     1        'GLOBAL VERTEX DATA REFERENCED TO SURFACE NUMBER ',I3)
 2501         FORMAT(1X)
              RETURN
          END IF
          RETURN
      END

C SUB GLBRAY.FOR
      SUBROUTINE GLBRAY
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GLBRAY.FOR. THIS SUBROUTINE IMPLEMENTS
C       CREATION OF GLOBAL RAY DATA FOR THE LAST RAY TRACED.
C       IT IS CALLED BY RAYTRA.FOR
C
          INTEGER JK,I
C
          REAL*8
     2    X1,Y1,Z1,L1,M1,N1,X00,Y00,Z0,LX0,LY0,LZ0,
     5    MX0,MY0,MZ0,NX0,NY0,NZ0,LO1,MO1,NO1,LN1,MN1,NN1
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(DABS(ALENS(3,0)).LE.1.0D10) THEN
C       FINITE OBJECT THICKNESS
              JK=0
          ELSE
C       INFINITE OBJECT THICKNESS
              JK=1
          END IF
C
          DO 10 I=JK,NEWIMG
C
C       NOW TRANSFORM TO THE GLOBAL COORDINATE SYSTEM.
C       THE GLOBAL COORDINATES OF EACH LOCAL COORDINATE
C       SYSTEM ARE:
C
              X00=VERTEX(1,I)
              Y00=VERTEX(2,I)
              Z0=VERTEX(3,I)
              LX0=VERTEX(4,I)
              MX0=VERTEX(5,I)
              NX0=VERTEX(6,I)
              LY0=VERTEX(7,I)
              MY0=VERTEX(8,I)
              NY0=VERTEX(9,I)
              LZ0=VERTEX(10,I)
              MZ0=VERTEX(11,I)
              NZ0=VERTEX(12,I)
C
              X1=X00+((LX0*(RAYRAY(1,I)))+(LY0*(RAYRAY(2,I)))
     1        +(LZ0*(RAYRAY(3,I))))
              Y1=Y00+((MX0*(RAYRAY(1,I)))+(MY0*(RAYRAY(2,I)))
     1        +(MZ0*(RAYRAY(3,I))))
              Z1=Z0+((NX0*(RAYRAY(1,I)))+(NY0*(RAYRAY(2,I)))
     1        +(NZ0*(RAYRAY(3,I))))
C
              L1=((LX0*(RAYRAY(4,I)))+(LY0*(RAYRAY(5,I)))
     1        +(LZ0*(RAYRAY(6,I))))
              M1=((MX0*(RAYRAY(4,I)))+(MY0*(RAYRAY(5,I)))
     1        +(MZ0*(RAYRAY(6,I))))
              N1=((NX0*(RAYRAY(4,I)))+(NY0*(RAYRAY(5,I)))
     1        +(NZ0*(RAYRAY(6,I))))
C
              LO1=((LX0*(RAYRAY(19,I)))+(LY0*(RAYRAY(20,I)))
     1        +(LZ0*(RAYRAY(21,I))))
              MO1=((MX0*(RAYRAY(19,I)))+(MY0*(RAYRAY(20,I)))
     1        +(MZ0*(RAYRAY(21,I))))
              NO1=((NX0*(RAYRAY(19,I)))+(NY0*(RAYRAY(20,I)))
     1        +(NZ0*(RAYRAY(21,I))))
C
              LN1=((LX0*(RAYRAY(13,I)))+(LY0*(RAYRAY(14,I)))
     1        +(LZ0*(RAYRAY(15,I))))
              MN1=((MX0*(RAYRAY(13,I)))+(MY0*(RAYRAY(14,I)))
     1        +(MZ0*(RAYRAY(15,I))))
              NN1=((NX0*(RAYRAY(13,I)))+(NY0*(RAYRAY(14,I)))
     1        +(NZ0*(RAYRAY(15,I))))
C
              GLRAY(1,I)=X1
              GLRAY(2,I)=Y1
              GLRAY(3,I)=Z1
              GLRAY(4,I)=L1
              GLRAY(5,I)=M1
              GLRAY(6,I)=N1
              GLRAY(7,I)=LO1
              GLRAY(8,I)=MO1
              GLRAY(9,I)=NO1
              GLRAY(10,I)=LN1
              GLRAY(11,I)=MN1
              GLRAY(12,I)=NN1
 10       CONTINUE
C       ALL GLOBAL TRANSFORMATIONS ARE COMPLETE
C
          RETURN
      END
C SUB GETOPD.FOR
      SUBROUTINE GETOPD(LEN,LENW,OPDERROR)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GETOPD WHICH IMPLEMENTS THE GET OPD
C       AND GET OPDW
C       COMMANDS AT THE CMD LEVEL.
C
          INTEGER J,JJ,WWRF
C
          REAL*8 LEN,LENW,WW,WAVE
C
          LOGICAL OPDERROR
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          IF(INT(CURLAM).EQ.1) WWVN=46
          IF(INT(CURLAM).EQ.2) WWVN=47
          IF(INT(CURLAM).EQ.3) WWVN=48
          IF(INT(CURLAM).EQ.4) WWVN=49
          IF(INT(CURLAM).EQ.5) WWVN=50
          IF(INT(CURLAM).EQ.6) WWVN=71
          IF(INT(CURLAM).EQ.7) WWVN=72
          IF(INT(CURLAM).EQ.8) WWVN=73
          IF(INT(CURLAM).EQ.9) WWVN=74
          IF(INT(CURLAM).EQ.10) WWVN=75
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
C               CHECK FOR PRESENCE OF STRING,QUALIFIER,OR NUMERIC
C               INPUT AND
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(RAYEXT.AND.REFEXT) THEN
              LEN=0.0D0
              RCOR=0.0D0
              OCOR=0.0D0
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) JJ=NEWOBJ+2
              IF(DABS(ALENS(3,NEWOBJ)).LT.1.0D10) JJ=NEWOBJ+1
              DO J=JJ,NEWIMG
                  LEN=LEN+RAYRAY(7,J)
     1            -(REFRY(7,J)*(ALENS(WWVN,J-1)/ALENS(WWRF,J-1)))
              END DO
              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       MODE FOCAL
                  RCOR=0.0D0
                  OCOR=0.0D0
                  CALL FOPD
                  LEN=LEN-(OCOR*(ALENS(WWVN,NEWOBJ)))
     1            +(RCOR*(ALENS(WWVN,NEWOBJ)))
                  RCOR=0.0D0
                  OCOR=0.0D0
                  CENCEN=.FALSE.
                  CALL LOPD
                  LEN=LEN-(OCOR*(ALENS(WWVN,NEWIMG-1)))
     1            +(RCOR*(ALENS(WWVN,NEWIMG-1)))
              ELSE
C       MODE AFOCAL
                  RCOR=0.0D0
                  OCOR=0.0D0
                  CALL FOPD
                  LEN=LEN-((OCOR*ALENS(WWVN,NEWOBJ)))
     1            +(RCOR*(ALENS(WWVN,NEWOBJ)))
                  RCOR=0.0D0
                  OCOR=0.0D0
                  CENCEN=.FALSE.
                  CALL LOPD
                  LEN=LEN-(OCOR*(ALENS(WWVN,NEWIMG-1)))
     1            +(RCOR*(ALENS(WWVN,NEWIMG-1)))
              END IF
C     CALCULATE LEN IN WAVES AT THE REFERENCE RAY WAVELENGTH
              IF(INT(CURLAM).GE.1.AND.INT(CURLAM).LE.5) THEN
                  WW=SYSTEM1(INT(CURLAM))
              END IF
              IF(INT(CURLAM).GE.6.AND.INT(CURLAM).LE.10) THEN
                  WW=SYSTEM1(INT(CURLAM)+65)
              END IF
              IF(SYSTEM1(6).EQ.1.0) WAVE=(WW*1.0D-3)/(25.4D0)
              IF(SYSTEM1(6).EQ.2.0) WAVE=WW*1.0D-4
              IF(SYSTEM1(6).EQ.3.0) WAVE=WW*1.0D-3
              IF(SYSTEM1(6).EQ.4.0) WAVE=WW*1.0D-6
              LEN=-LEN
              IF(REVSTR) LEN=-LEN
              LENW=LEN/WAVE
              OPDERROR=.FALSE.
              RETURN
          ELSE
C       NO RAY EXITS OR NO REF RAY
              IF(.NOT.REFEXT) THEN
                  OUTLYNE=
     1            'NO REFERENCE RAY DATA EXISTS, "OPD" CAN NOT BE CALCULATED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  OPDERROR=.TRUE.
              END IF
              IF(.NOT.RAYEXT) THEN
                  OUTLYNE=
     1            'NO RAY DATA EXISTS, "OPD" CAN NOT BE CALCULATED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  OPDERROR=.TRUE.
              END IF
          END IF
          RETURN
      END
C SUB LOPDS.FOR
      SUBROUTINE LOPDS(REFERR,TPT)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE LOPD.FOR. THIS SUBROUTINE IS
C       CALLED BY SPD AND COMPAP ROUTINES. IT CALCULATES ADJUSTMENTS TO THE
C       OPD DUE TO THE IMAGE REFERENCE SPHERE. (FOR AFOCAL
C       SYSTEMS THIS WILL BE A FLAT REFERENCE SURFACE)
C
          REAL*8 XREFI,XO,XOOY,XOOX,YO,YOOX,YOOY,ZO,ZOOX,ZOOY,
     1    YREFI,RAD,AL,BE,GA,A,B,C,LEN,LEN1,LO,LOOX,LOOY,MO,MOOX,MOOY,
     2    LEN2,Q,ARG,ZREFI,SIGNB,RL0,RM0,RN0,NO,NOOX,NOOY,THYNUM,THXNUM,
     3    RRX0,RRZ0,T,RX0,RY0,RZ0,RRY0,M0,L0,N0,XA,THXDEN,THYDEN,
     4    YA,ZA,LLL,MMM,NNN
C
          INTEGER TPT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
C
          LOGICAL REFERR
C
          RCOR=0.0D0
          OCOR=0.0D0
          REFERR=.FALSE.
C
C       CASE OF A PERFECT LENS AT SYSTEM1(20)-1,OR IDEAL
          IF(GLANAM(INT(SYSTEM1(20))-1,2).EQ.'PERFECT      '
     1    .OR.GLANAM(INT(SYSTEM1(20))-1,2).EQ.'IDEAL        ') THEN
C     ALWAYS ASSUME FOCAL MODE BUT THERE IS NOTHING TO SUBTRACT OFF
C     SINCE NOTHING WAS ADDED
C     EXCEPT FOR OFF AXIS IMAGE POINTS WHERE WAVEFRONT TILT MUST
C     BE REMOVED AS IN THE CASE OF AFOCAL SYSTEMS. HERE HOWEVER, THE TILTED
C     PLANE IS AT NEWING-1 RATHER THAN AT NEWIMG
C       FLAT REFERENCE PLANE
C       THE SO-CALLED FLAT REFERENCE PLANE IS A PLANE
C       PERPENDICULAR TO THE REF RAY. THE INTERSECTION
C       OF THIS REFERENCE PLANE WITH THE REFERENCE RAY OCCURRS
C       AT THE FINAL SURFACE (NEWIMG-1). WITH THIS DEFINITION,
C       THE CORRECTION TERM FOR THE REFERENCE RAY IS ALWAYS
C       ZERO.
              RCOR=0.0D0
              RX0=REFRY(1,NEWIMG-1)
              RY0=REFRY(2,NEWIMG-1)
              RZ0=REFRY(3,NEWIMG-1)
              RL0=REFRY(19,NEWIMG-1)
              RM0=REFRY(20,NEWIMG-1)
              RN0=REFRY(21,NEWIMG-1)
C       NOW THE OTHER RAY:
              RRX0=DSPOT(27)
              RRY0=DSPOT(28)
              RRZ0=DSPOT(29)
              L0=DSPOT(30)
              M0=DSPOT(31)
              N0=DSPOT(32)
C       THE LINE DIRECTION NUMBERS ARE:
C
C       THE PLANE WHICH HAS ITS ORIGIN WHERE THE
C       REFERENCE RAY INTERSECTS THE IMAGE PLANE-1
C       AND IS PERPENDICULAR TO THE REFERENCE RAY HAS THE FORM:
C
C       (RL0*(X-RX0))+(RM0*(Y-RY0))+(RN0*(Z-RZ0))=0
C       X,Y AND Z ARE ANY POINTS ON THE PLANE.
C
C       NOW DETERMINE THE INTERSECTION OF AN "OTHER" RAY
C       WITH THIS REFERENCE PLANE.
C
C       THE EQUATION OF THE OTHER RAY IS
C               XA=RRX0+(T*L0)
C               YA=RRY0+(T*M0)
C               ZA=RRZ0+(T*N0)
C THE PARAMETER T IS DETERMINED BY SUBSTITUTING THIS LINE
C       EQUATION INTO THE REFERENCE SURAFCE EQUATION
C       AND SOLVING FOR T.
C
              T=((RL0*(RX0-RRX0))+(RM0*(RY0-RRY0))+(RN0*(RZ0-RRZ0)))/
     1        ((RL0*L0)+(RM0*M0)+(RN0*N0))
C
C       NOW X,Y AND Z ARE:
              XA=RRX0+(T*L0)
              YA=RRY0+(T*M0)
              ZA=RRZ0+(T*N0)
              OCOR=DSQRT(
     1        ((DSPOT(27)-XA)**2)+
     2        ((DSPOT(28)-YA)**2)+
     3        ((DSPOT(29)-ZA)**2))
C     IF T IS LESS THAN ZERO, THE RAY INTERSECTION WITH THE
C     REFERENCE PLANE LIES ON THE NEGATIVE SIDE OF THE IMAGE SURFACE
C     AND ZA LT DSPOT(29) AND OCOR MUST BE POSITIVE AND VICE VERSA.
C       THE CORRECT SIGN IS:
              IF(ZA.GT.DSPOT(29)) OCOR=-OCOR
              IF(ZA.LE.DSPOT(29)) OCOR=OCOR
              REFERR=.FALSE.
              GO TO 900
          END IF
C
C       CASE OF AFOCAL SYSTEMS
C
          IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
C
C       FLAT REFERENCE PLANE
C       THE SO-CALLED FLAT REFERENCE PLANE IS A PLANE
C       PERPENDICULAR TO THE REF RAY. THE INTERSECTION
C       OF THIS REFERENCE PLANE WITH THE REFERENCE RAY OCCURRS
C       AT THE FINAL SURFACE (NEWIMG). WITH THIS DEFINITION,
C       THE CORRECTION TERM FOR THE REFERENCE RAY IS ALWAYS
C       ZERO.
              RCOR=0.0D0
              RX0=REFRY(1,NEWIMG)
              RY0=REFRY(2,NEWIMG)
              RZ0=REFRY(3,NEWIMG)
              RL0=REFRY(19,NEWIMG)
              RM0=REFRY(20,NEWIMG)
              RN0=REFRY(21,NEWIMG)
C       NOW THE OTHER RAY:
              RRX0=DSPOT(1)
              RRY0=DSPOT(2)
              RRZ0=DSPOT(3)
              L0=DSPOT(22)
              M0=DSPOT(23)
              N0=DSPOT(24)
C       THE LINE DIRECTION NUMBERS ARE:
C
C       THE PLANE WHICH HAS ITS ORIGIN WHERE THE
C       REFERENCE RAY INTERSECTS THE IMAGE PLANE
C       AND IS PERPENDICULAR TO THE REFERENCE RAY HAS THE FORM:
C
C       (RL0*(X-RX0))+(RM0*(Y-RY0))+(RN0*(Z-RZ0))=0
C       X,Y AND Z ARE ANY POINTS ON THE PLANE.
C
C       NOW DETERMINE THE INTERSECTION OF AN "OTHER" RAY
C       WITH THIS REFERENCE PLANE.
C
C       THE EQUATION OF THE OTHER RAY IS
C               XA=RRX0+(T*L0)
C               YA=RRY0+(T*M0)
C               ZA=RRZ0+(T*N0)
C THE PARAMETER T IS DETERMINED BY SUBSTITUTING THIS LINE
C       EQUATION INTO THE REFERENCE SURAFCE EQUATION
C       AND SOLVING FOR T.
C
              T=((RL0*(RX0-RRX0))+(RM0*(RY0-RRY0))+(RN0*(RZ0-RRZ0)))/
     1        ((RL0*L0)+(RM0*M0)+(RN0*N0))
C
C       NOW X,Y AND Z ARE:
              XA=RRX0+(T*L0)
              YA=RRY0+(T*M0)
              ZA=RRZ0+(T*N0)
              OCOR=DSQRT(
     1        ((DSPOT(1)-XA)**2)+
     2        ((DSPOT(2)-YA)**2)+
     3        ((DSPOT(3)-ZA)**2))
C     IF T IS LESS THAN ZERO, THE RAY INTERSECTION WITH THE
C     REFERENCE PLANE LIES ON THE NEGATIVE SIDE OF THE IMAGE SURFACE
C     AND ZA LT DSPOT(3) AND OCOR MUST BE POSITIVE AND VICE VERSA.
C       THE CORRECT SIGN IS:
              IF(ZA.GT.DSPOT(3)) OCOR=-OCOR
              IF(ZA.LE.DSPOT(3)) OCOR=OCOR
              REFERR=.FALSE.
              GO TO 900
          END IF
C
C     FOCAL SYSTEMS
C
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       SYSTEM FOCAL.
              IF(.NOT.EXPAUT.OR.EXPAUT.AND..NOT.LDIF2) THEN
C       THE REFERENCE SPHERE IS CENTERED WHERE THE REAL CHIEF
C       RAY CROSSES THE FINAL SURFACE.
C       THE RADIUS IS EQUAL
C       TO THE DISTANCE FROM THE FINAL IMAGE SURFACE TO THE SURFACE
C       PRECEEDING THE IMAGE SURFACE IF THE I-1 THICKNESS IS NON-ZERO
C       AND IS THE DISTANCE TO THE PARAXIAL EXIT PUPIL IF TH (I-1)=0
C       THIS IS JUST AS IN ACCOS V AND
C       HEXAGON AND GIVES THE USER THE CHOICE FOR RADIUS. BY USING
C       PIKUPS (NOT AVAILABLE IN CODE V), A WIDE VARIETY OF REFERENCE
C       SURFACES MAY BE SELECTED.
C
C     IF EXPAUT AND NOT LDIF2, AN ASTOP EX IS DONE EVEN IF THE I-1
C     THICKNESS IS NOT ZERO.
C
C       COORDINATES OF THE REAL CHIEF RAY AT THE IMAGE PLANE ARE:
C       THIS IS THE REFERENCE SPHERE CENTER LOCATION.
                  XREFI=REFRY(1,NEWIMG)
                  YREFI=REFRY(2,NEWIMG)
                  ZREFI=REFRY(3,NEWIMG)
                  IF(ALENS(3,(NEWIMG-1)).EQ.0.0D0.OR.ALENS(3,(NEWIMG-1)).NE.
     1            0.0D0.AND..NOT.LDIF2) THEN
C       DIST FROM I-1 TO I IS ZERO OF LDIF2 IS FALSE AND DIST NOT 0
C       DO AN INTERNAL "ASTOP EX" ADJUSTMENT
                      IF(DABS(PXTRAY(5,NEWIMG)*1.0D-15).LT.
     1                DABS(PXTRAY(6,NEWIMG))) THEN
                          RAD=(PXTRAY(5,NEWIMG)/PXTRAY(6,NEWIMG))
                          RREF=RAD
                      END IF
                      IF(DABS(PXTRAY(5,NEWIMG)*1.0D-15).GE.
     1                DABS(PXTRAY(6,NEWIMG))) THEN
C       REF RAY IS TELECENTRIC, SET RAD INFINITY
                          RAD=1.0D20
                          RREF=RAD
                      END IF
C       USE EXISTING THICKNESS OF NEWIMG-1 AS RAD
                      RAD=ALENS(3,NEWIMG-1)
                      RREF=RAD
                  END IF
C
C       NOW WE HAVE LOCATION OF REFERENCE SPHERE CENTER
C       AND ITS RADIUS
C
C       THE CHIEF RAY INTERSECTS THE REFERENCE SPHERE
C       THE LENGTH FROM THIS INTERSECTION TO THE INTERSECTION
C       OF THE REFERENCE RAY WITH THE FINAL SURFACE IS RCOR=RAD
C       REFERENCE RAY INTERSECTION WITH THE TANGENT PLANE IS:
C
                  RCOR=RAD
C
C       THE LENGTH FROM THE REFERENCE SPHERE
C       TO THE FINAL SURFACE ALONG THE "OTHER" RAY IS:
C
                  AL=DSPOT(1)-XREFI
                  BE=DSPOT(2)-YREFI
                  GA=DSPOT(3)-ZREFI
                  C=(AL**2)+(BE**2)+(GA**2)-(RAD**2)
                  B=2.0D0*((DSPOT(22)*AL)+
     1              (DSPOT(23)*BE)+
     2              (DSPOT(24)*GA))
                  A=1.0D0
C       SOLVE FOR LEN
                  IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
                  IF(B.EQ.0.0D0) SIGNB=1.0D0
                  IF(A.EQ.0.0D0) THEN
                      LEN=-(C/B)
                  ELSE
                      ARG=((B**2)-(4.0D0*A*C))
                      IF(ARG.LT.0.0D0) THEN
                          IF(TPT.EQ.1) THEN
                              OUTLYNE='RAY(S) COULD NOT INTERSECT REFERENCE SPHERE'
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'TRY USING AN "ASTOP EX" OR ASTOP "EN/EX" ADJUSTMENT'
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'OPD VALUES MAY BE SUSPECT'
                              CALL SHOWIT(1)
                          END IF
                          OCOR=0.0D0
                          RCOR=0.0D0
                          REFERR=.TRUE.
                          RETURN
C       REFERENCE SPHERE INTERSECTED
                      END IF
                      Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
                      LEN1=C/Q
                      LEN2=Q/A
                      IF(REFRY(6,NEWIMG).GT.0.0D0) THEN
                          IF(RAD.GT.0.0D0) THEN
C     THEN THE INTERSECTION TO USE IS THE NEGATIVE INTERSECTION
                              IF(LEN1.LT.0.0D0) LEN=LEN1
                              IF(LEN2.LT.0.0D0) LEN=LEN2
                          ELSE
C       RAD NEG
C     THEN THE INTERSECTION TO USE IS THE POSITIVE INTERSECTION
                              IF(LEN1.GT.0.0D0) LEN=LEN1
                              IF(LEN2.GT.0.0D0) LEN=LEN2
                          END IF
                      END IF
                      IF(REFRY(6,NEWIMG).LT.0.0D0) THEN
                          IF(RAD.GT.0.0D0) THEN
C     THEN THE INTERSECTION TO USE IS THE NEGATIVE INTERSECTION
                              IF(LEN1.LT.0.0D0) LEN=-LEN2
                              IF(LEN2.LT.0.0D0) LEN=-LEN1
                          ELSE
C       RAD NEG
C     THEN THE INTERSECTION TO USE IS THE POSITIVE INTERSECTION
                              IF(LEN1.GT.0.0D0) LEN=-LEN2
                              IF(LEN2.GT.0.0D0) LEN=-LEN1
                          END IF
                      END IF
                  END IF
C       NEGATIVE SIGN SINCE LEN1 AND LEN2 MEASURES DIRECTED
C       DISTANCE FROM IMAGE TO SPHERE INTERSECTION AND
C       LEN IS DIRECTED FROM SPHERE INTERSECTION TO IMAGE

                  OCOR=-LEN
                  REFERR=.FALSE.
                  GO TO 900
C     EXPAUT NOT FALSE
              END IF
              IF(EXPAUT) THEN
C       THE REFERENCE SPHERE IS CENTERED WHERE THE REAL CHIEF
C       RAY CROSSES THE FINAL SURFACE.
C       THE RADIUS IS EQUAL
C       TO THE DISTANCE FROM POINT TO WHERE THE CHIEF RAY CROSSES THE REAL
C     EXIT PUPIL
C       THIS IS JUST AS IN CODE-V AND IS THE DEFAULT WHEN THE PROGRAM
C     BEGINS
C
C       COORDINATES OF THE REAL CHIEF RAY AT THE IMAGE PLANE ARE:
C       THIS IS THE REFERENCE SPHERE CENTER LOCATION.
                  XREFI=REFRY(1,NEWIMG)
                  YREFI=REFRY(2,NEWIMG)
                  ZREFI=REFRY(3,NEWIMG)
C
C       DO A REAL RAY INTERNAL EXIT PUPIL ADJUSTMENT
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
                  THXNUM=((XO-XOOX)*(LO-LOOX))+((YO-YOOX)*(MO-MOOX))
     1            +((ZO-ZOOX)*(NO-NOOX))
                  THYNUM=((XO-XOOY)*(LO-LOOY))+((YO-YOOY)*(MO-MOOY))
     1            +((ZO-ZOOY)*(NO-NOOY))
                  THXDEN=((LO-LOOX)**2)+((MO-MOOX)**2)
     1            +((NO-NOOX)**2)
                  THYDEN=((LO-LOOY)**2)+((MO-MOOY)**2)
     1            +((NO-NOOY)**2)
                  IF(DABS(THXNUM*1.0D-15).LT.DABS(THXDEN).AND.
     1            DABS(THYNUM*1.0D-15).LT.
     1            DABS(THYDEN)) THEN
                      RAD=-((-THXNUM/THXDEN)+(-THYNUM/THYDEN))/2.0D0
                      RREF=RAD
C     THE EXIT PUPIL IS LOCATED AT:
                      XA=XO+(-RAD*LO)
                      YA=YO+(-RAD*MO)
                      ZA=ZO+(-RAD*NO)
                      RAD=DSQRT(
     1                ((XA-XREFI)**2)+((YA-YREFI)**2)+((ZA-ZREFI)**2)
     1                )
                      RREF=RAD
                      IF(ZA.LE.ZO) THEN
C     PUPIL LIES ON THE NEGATIVE SIDE OF NEWIMG SO RAD IS POS
                          RAD=RAD
                          RREF=RAD
                      ELSE
                          RAD=-RAD
                          RREF=RAD
                      END IF
                  END IF
                  IF(DABS(THXNUM*1.0D-15).GE.DABS(THXDEN).OR.
     1            DABS(THYNUM*1.0D-15).GE.
     1            DABS(THYDEN)) THEN
C       REF RAY IS TELECENTRIC, SET RAD INFINITY
C       FLAT REFERENCE PLANE
                      RAD=1.0D20
                      RREF=RAD
                  END IF
C
C       NOW WE HAVE LOCATION OF REFERENCE SPHERE CENTER
C       AND ITS RADIUS MAGNITUDE
C
C       THE CHIEF RAY INTERSECTS THE REFERENCE SPHERE
C       THE LENGTH FROM THIS INTERSECTION TO THE INTERSECTION
C       OF THE REFERENCE RAY WITH THE FINAL SURFACE IS RCOR=RAD
C       REFERENCE RAY INTERSECTION WITH THE TANGENT PLANE IS:
C
                  RCOR=RAD
C
C       THE LENGTH FROM THE REFERENCE SPHERE
C       TO THE FINAL SURFACE ALONG THE "OTHER" RAY IS:
C
                  LLL=DSPOT(22)
                  MMM=DSPOT(23)
                  NNN=DSPOT(24)
                  AL=DSPOT(1)-XREFI
                  BE=DSPOT(2)-YREFI
                  GA=DSPOT(3)-ZREFI
                  C=(AL**2)+(BE**2)+(GA**2)-(RAD**2)
                  B=2.0D0*((LLL*AL)+
     1              (MMM*BE)+
     2              (NNN*GA))
                  A=1.0D0
C       SOLVE FOR LEN
                  IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
                  IF(B.EQ.0.0D0) SIGNB=1.0D0
                  IF(A.EQ.0.0D0) THEN
                      LEN=-(C/B)
                  ELSE
                      ARG=((B**2)-(4.0D0*A*C))
                      IF(ARG.LT.0.0D0) THEN
                          IF(TPT.EQ.1) THEN
                              OUTLYNE='RAY(S) COULD NOT INTERSECT REFERENCE SPHERE'
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'THE CURRENT SYSTEM MAY HAVE EXCESSIVE ABERRATIONS'
                              CALL SHOWIT(1)
                              OUTLYNE=
     1                        'OPD VALUES MAY BE SUSPECT'
                              CALL SHOWIT(1)
                          END IF
                          OCOR=0.0D0
                          RCOR=0.0D0
                          REFERR=.TRUE.
                          RETURN
C       REFERENCE SPHERE INTERSECTED
                      END IF
                      Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
                      LEN1=C/Q
                      LEN2=Q/A
                      IF(REFRY(6,NEWIMG).GT.0.0D0) THEN
                          IF(RAD.GT.0.0D0) THEN
C     THEN THE INTERSECTION TO USE IS THE NEGATIVE INTERSECTION
                              IF(LEN1.LT.0.0D0) LEN=LEN1
                              IF(LEN2.LT.0.0D0) LEN=LEN2
                          ELSE
C       RAD NEG
C     THEN THE INTERSECTION TO USE IS THE POSITIVE INTERSECTION
                              IF(LEN1.GT.0.0D0) LEN=LEN1
                              IF(LEN2.GT.0.0D0) LEN=LEN2
                          END IF
                      END IF
                      IF(REFRY(6,NEWIMG).LT.0.0D0) THEN
                          IF(RAD.GT.0.0D0) THEN
C     THEN THE INTERSECTION TO USE IS THE NEGATIVE INTERSECTION
                              IF(LEN1.LT.0.0D0) LEN=-LEN2
                              IF(LEN2.LT.0.0D0) LEN=-LEN1
                          ELSE
C       RAD NEG
C     THEN THE INTERSECTION TO USE IS THE POSITIVE INTERSECTION
                              IF(LEN1.GT.0.0D0) LEN=-LEN2
                              IF(LEN2.GT.0.0D0) LEN=-LEN1
                          END IF
                      END IF
                  END IF
C       NEGATIVE SIGN SINCE LEN1 AND LEN2 MEASURES DIRECTED
C       DISTANCE FROM IMAGE TO SPHERE INTERSECTION AND
C       LEN IS DIRECTED FROM SPHERE INTERSECTION TO IMAGE
                  OCOR=-LEN
                  REFERR=.FALSE.
                  GO TO 900
C     EXPAUT NOT TRUE
              END IF
              REFERR=.FALSE.
C     NOT FOCAL
              GO TO 900
          END IF
          REFERR=.FALSE.
 900      CONTINUE
          IF(DABS(RCOR).GT.1.0D10.OR.DABS(OCOR).GT.1.0D10) THEN
C     INFINITE REFERENCE SPHERE
              RCOR=0.0D0
              IF(DABS(RCOR).GT.1.0D10.OR.DABS(OCOR).GT.1.0D10) THEN
C     INFINITE REFERENCE SPHERE
                  RCOR=0.0D0
C     OCOR IS THE DISTANCE ALONG THE RAY BETWEEN AN INTERSECTION
C     WITH A PLANE PERPENDICULAR TO A PARALLEL RAY WHICH PASSES
C     THROUGH A POINT ON THE IMAGE SURFACE WHERE THE CHIEF RAY
C     HITS
                  RCOR=0.0D0
                  RCOR=0.0D0
                  RX0=REFRY(1,NEWIMG-1)
                  RY0=REFRY(2,NEWIMG-1)
                  RZ0=REFRY(3,NEWIMG-1)
                  RL0=DSPOT(30)
                  RM0=DSPOT(31)
                  RN0=DSPOT(32)
C       NOW THE OTHER RAY:
                  RRX0=DSPOT(27)
                  RRY0=DSPOT(28)
                  RRZ0=DSPOT(29)
                  L0=DSPOT(30)
                  M0=DSPOT(31)
                  N0=DSPOT(32)
C       THE LINE DIRECTION NUMBERS ARE:
C
C       THE PLANE WHICH HAS ITS ORIGIN WHERE THE
C       REFERENCE RAY INTERSECTS THE IMAGE PLANE
C       AND IS PERPENDICULAR TO THE REGULAR RAY HAS THE FORM:
C
C       (RL0*(X-RX0))+(RM0*(Y-RY0))+(RN0*(Z-RZ0))=0
C       X,Y AND Z ARE ANY POINTS ON THE PLANE.
C       NOW DETERMINE THE INTERSECTION OF AN "OTHER" RAY
C       WITH THIS REFERENCE PLANE.
C
C       THE EQUATION OF THE OTHER RAY IS
C               XA=RRX0+(T*L0)
C               YA=RRY0+(T*M0)
C               ZA=RRZ0+(T*N0)
C THE PARAMETER T IS DETERMINED BY SUBSTITUTING THIS LINE
C       EQUATION INTO THE REFERENCE SURAFCE EQUATION
C       AND SOLVING FOR T.
C
                  T=((RL0*(RX0-RRX0))+(RM0*(RY0-RRY0))+(RN0*(RZ0-RRZ0)))/
     1            ((RL0*L0)+(RM0*M0)+(RN0*N0))
C
C       NOW X,Y AND Z ARE:
                  XA=RRX0+(T*L0)
                  YA=RRY0+(T*M0)
                  ZA=RRZ0+(T*N0)
                  OCOR=DSQRT(
     1            ((DSPOT(27)-XA)**2)+
     2            ((DSPOT(28)-YA)**2)+
     3            ((DSPOT(29)-ZA)**2))
C     IF T IS LESS THAN ZERO, THE RAY INTERSECTION WITH THE
C     REFERENCE PLANE LIES ON THE NEGATIVE SIDE OF THE IMAGE SURFACE
C     AND ZA LT DSPOT(29) AND OCOR MUST BE POSITIVE AND VICE VERSA.
C       THE CORRECT SIGN IS:
                  IF(ZA.GT.DSPOT(29)) OCOR=-OCOR
                  IF(ZA.LE.DSPOT(29)) OCOR=OCOR
              ELSE
              END IF
          ELSE
          END IF
          RETURN
      END
C  SUB LOPD.FOR
      SUBROUTINE LOPD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE LOPD.FOR. THIS SUBROUTINE IS
C       CALLED BY FANS AND OTHER ROUTINES. IT CALCULATES ADJUSTMENTS TO THE
C       OPD DUE TO THE IMAGE REFERENCE SPHERE. (FOR AFOCAL
C       SYSTEMS THIS WILL BE A FLAT REFERENCE SURFACE)
C
          REAL*8 XREFI,XO,XOOY,XOOX,YO,YOOX,YOOY,ZO,ZOOX,ZOOY,
     1    YREFI,RAD,AL,BE,GA,A,B,C,LEN,LEN1,LO,LOOX,LOOY,MO,MOOX,MOOY,
     2    LEN2,Q,ARG,ZREFI,SIGNB,RL0,RM0,RN0,NO,NOOX,NOOY,THYNUM,THXNUM,
     3    RRX0,RRZ0,T,RX0,RY0,RZ0,RRY0,M0,L0,N0,XA,THXDEN,THYDEN,
     4    YA,ZA,LLL,MMM,NNN
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datspd.inc'
C
          RCOR=0.0D0
          OCOR=0.0D0
C
C       CASE OF A PERFECT LENS AT SYSTEM1(20)-1
C     OR IDEAL LENS
          IF(GLANAM(INT(SYSTEM1(20))-1,2).EQ.'PERFECT      '
     1    .OR.GLANAM(INT(SYSTEM1(20))-1,2).EQ.'IDEAL        ') THEN
C     ALWAYS ASSUME FOCAL MODE BUT THERE IS NOTHING TO SUBTRACT OFF
C     SINCE NOTHING WAS ADDED
C     EXCEPT FOR OFF AXIS IMAGE POINTS WHERE WAVEFRONT TILT MUST
C     BE REMOVED AS IN THE CASE OF AFOCAL SYSTEMS. hERE HOWEVER, THE TILTED
C     PLANE IS AT NEWING-1 RATHER THAN AT NEWIMG
C       FLAT REFERENCE PLANE
C       THE SO-CALLED FLAT REFERENCE PLANE IS A PLANE
C       PERPENDICULAR TO THE REF RAY. THE INTERSECTION
C       OF THIS REFERENCE PLANE WITH THE REFERENCE RAY OCCURRS
C       AT THE FINAL SURFACE-1 (NEWIMG-1). WITH THIS DEFINITION,
C       THE CORRECTION TERM FOR THE REFERENCE RAY IS ALWAYS
C       ZERO.
              RCOR=0.0D0
              RX0=REFRY(1,NEWIMG-1)
              RY0=REFRY(2,NEWIMG-1)
              RZ0=REFRY(3,NEWIMG-1)
              RL0=REFRY(19,NEWIMG-1)
              RM0=REFRY(20,NEWIMG-1)
              RN0=REFRY(21,NEWIMG-1)
C       NOW THE OTHER RAY:
              RRX0=RAYRAY(1,NEWIMG-1)
              RRY0=RAYRAY(2,NEWIMG-1)
              RRZ0=RAYRAY(3,NEWIMG-1)
              L0=RAYRAY(19,NEWIMG-1)
              M0=RAYRAY(20,NEWIMG-1)
              N0=RAYRAY(21,NEWIMG-1)
C       THE LINE DIRECTION NUMBERS ARE:
C
C       THE PLANE WHICH HAS ITS ORIGIN WHERE THE
C       REFERENCE RAY INTERSECTS THE IMAGE PLANE
C       AND IS PERPENDICULAR TO THE REFERENCE RAY HAS THE FORM:
C
C       (RL0*(X-RX0))+(RM0*(Y-RY0))+(RN0*(Z-RZ0))=0
C       X,Y AND Z ARE ANY POINTS ON THE PLANE.
C
C       NOW DETERMINE THE INTERSECTION OF AN "OTHER" RAY
C       WITH THIS REFERENCE PLANE.
C
C       THE EQUATION OF THE OTHER RAY IS
C               XA=RRX0+(T*L0)
C               YA=RRY0+(T*M0)
C               ZA=RRZ0+(T*N0)
C THE PARAMETER T IS DETERMINED BY SUBSTITUTING THIS LINE
C       EQUATION INTO THE REFERENCE SURAFCE EQUATION
C       AND SOLVING FOR T.
C
              T=((RL0*(RX0-RRX0))+(RM0*(RY0-RRY0))+(RN0*(RZ0-RRZ0)))/
     1        ((RL0*L0)+(RM0*M0)+(RN0*N0))
C
C       NOW X,Y AND Z ARE:
              XA=RRX0+(T*L0)
              YA=RRY0+(T*M0)
              ZA=RRZ0+(T*N0)
              OCOR=DSQRT(
     1        ((RAYRAY(1,NEWIMG-1)-XA)**2)+
     2        ((RAYRAY(2,NEWIMG-1)-YA)**2)+
     3        ((RAYRAY(3,NEWIMG-1)-ZA)**2))
C     IF T IS LESS THAN ZERO, THE RAY INTERSECTION WITH THE
C     REFERENCE PLANE LIES ON THE NEGATIVE SIDE OF THE IMAGE SURFACE
C     AND ZA LT RAYRAY(3,NEWIMG-1) AND OCOR MUST BE POSITIVE AND VICE VERSA.
C       THE CORRECT SIGN IS:
              IF(ZA.GT.RAYRAY(3,NEWIMG-1)) OCOR=-OCOR
              IF(ZA.LE.RAYRAY(3,NEWIMG-1)) OCOR=OCOR
              GO TO 900
          END IF
C
C       CASE OF AFOCAL SYSTEMS
C
          IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
C
C       FLAT REFERENCE PLANE
C       THE SO-CALLED FLAT REFERENCE PLANE IS A PLANE
C       PERPENDICULAR TO THE REF RAY. THE INTERSECTION
C       OF THIS REFERENCE PLANE WITH THE REFERENCE RAY OCCURRS
C       AT THE FINAL SURFACE (NEWIMG). WITH THIS DEFINITION,
C       THE CORRECTION TERM FOR THE REFERENCE RAY IS ALWAYS
C       ZERO.
              RCOR=0.0D0
              RX0=REFRY(1,NEWIMG)
              RY0=REFRY(2,NEWIMG)
              RZ0=REFRY(3,NEWIMG)
              RL0=REFRY(19,NEWIMG)
              RM0=REFRY(20,NEWIMG)
              RN0=REFRY(21,NEWIMG)
C       NOW THE OTHER RAY:
              RRX0=RAYRAY(1,NEWIMG)
              RRY0=RAYRAY(2,NEWIMG)
              RRZ0=RAYRAY(3,NEWIMG)
              L0=RAYRAY(19,NEWIMG)
              M0=RAYRAY(20,NEWIMG)
              N0=RAYRAY(21,NEWIMG)
C
C       THE PLANE WHICH HAS ITS ORIGIN WHERE THE
C       REFERENCE RAY INTERSECTS THE IMAGE PLANE
C       AND IS PERPENDICULAR TO THE REFERENCE RAY HAS THE FORM:
C
C       (RL0*(X-RX0))+(RM0*(Y-RY0))+(RN0*(Z-RZ0))=0
C       X,Y AND Z ARE ANY POINTS ON THE PLANE.
C
C       NOW DETERMINE THE INTERSECTION OF AN "OTHER" RAY
C       WITH THIS REFERENCE PLANE.
C
C       THE EQUATION OF THE OTHER RAY IS
C               XA=RRX0+(T*L0)
C               YA=RRY0+(T*M0)
C               ZA=RRZ0+(T*N0)
C THE PARAMETER T IS DETERMINED BY SUBSTITUTING THIS LINE
C       EQUATION INTO THE REFERENCE SURAFCE EQUATION
C       AND SOLVING FOR T.
C
              T=((RL0*(RX0-RRX0))+(RM0*(RY0-RRY0))+(RN0*(RZ0-RRZ0)))/
     1        ((RL0*L0)+(RM0*M0)+(RN0*N0))
C
C       NOW X,Y AND Z ARE:
              XA=RRX0+(T*L0)
              YA=RRY0+(T*M0)
              ZA=RRZ0+(T*N0)
              OCOR=DSQRT(
     1        ((RAYRAY(1,NEWIMG)-XA)**2)+
     2        ((RAYRAY(2,NEWIMG)-YA)**2)+
     3        ((RAYRAY(3,NEWIMG)-ZA)**2))
C     IF T IS LESS THAN ZERO, THE RAY INTERSECTION WITH THE
C     REFERENCE PLANE LIES ON THE NEGATIVE SIDE OF THE IMAGE SURFACE
C     AND ZA LT RAYRAY(3,NEWIMG) AND OCOR MUST BE POSITIVE AND VICE VERSA.
C       THE CORRECT SIGN IS:
              IF(ZA.GT.RAYRAY(3,NEWIMG)) OCOR=-OCOR
              IF(ZA.LE.RAYRAY(3,NEWIMG)) OCOR=OCOR
              GO TO 900
          END IF
C
C     FOCAL SYSTEMS
C
          IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       SYSTEM FOCAL.
              IF(.NOT.EXPAUT.OR.EXPAUT.AND..NOT.LDIF2) THEN
C       THE REFERENCE SPHERE IS CENTERED WHERE THE REAL CHIEF
C       RAY CROSSES THE FINAL SURFACE.
C       THE RADIUS IS EQUAL
C       TO THE DISTANCE FROM THE FINAL IMAGE SURFACE TO THE SURFACE
C       PRECEEDING THE IMAGE SURFACE IF THE I-1 THICKNESS IS NON-ZERO
C       AND IS THE DISTANCE TO THE PARAXIAL EXIT PUPIL IF TH (I-1)=0
C       THIS IS JUST AS IN ACCOS V AND
C       HEXAGON AND GIVES THE USER THE CHOICE FOR RADIUS. BY USING
C       PIKUPS, A WIDE VARIETY OF REFERENCE
C       SURFACES MAY BE SELECTED.
C
C     IF EXPAUT AND NOT LDIF2, AN ASTOP EX IS DONE EVEN IF THE I-1
C     THICKNESS IS NOT ZERO.
C
C       COORDINATES OF THE REAL CHIEF RAY AT THE IMAGE PLANE ARE:
C       THIS IS THE REFERENCE SPHERE CENTER LOCATION.
                  XREFI=REFRY(1,NEWIMG)
                  YREFI=REFRY(2,NEWIMG)
                  ZREFI=REFRY(3,NEWIMG)
                  IF(ALENS(3,(NEWIMG-1)).EQ.0.0D0.OR.ALENS(3,(NEWIMG-1)).NE.
     1            0.0D0.AND..NOT.LDIF2) THEN
C       DIST FROM I-1 TO I IS ZERO OF LDIF2 IS FALSE AND DIST NOT 0
C       DO AN INTERNAL "ASTOP EX" ADJUSTMENT
                      IF(DABS(PXTRAY(5,NEWIMG)*1.0D-15).LT.
     1                DABS(PXTRAY(6,NEWIMG))) THEN
                          RAD=(PXTRAY(5,NEWIMG)/PXTRAY(6,NEWIMG))
                      END IF
                      IF(DABS(PXTRAY(5,NEWIMG)*1.0D-15).GE.
     1                DABS(PXTRAY(6,NEWIMG))) THEN
C       REF RAY IS TELECENTRIC, SET RAD = 1.0D20
                          RAD=1.0D20
                      END IF
C       USE EXISTING THICKNESS OF NEWIMG-1 AS RAD
                      RAD=ALENS(3,NEWIMG-1)
                  END IF
C
C       NOW WE HAVE LOCATION OF REFERENCE SPHERE CENTER
C       AND ITS RADIUS
C
C       THE CHIEF RAY INTERSECTS THE REFERENCE SPHERE
C       THE LENGTH FROM THIS INTERSECTION TO THE INTERSECTION
C       OF THE REFERENCE RAY WITH THE FINAL SURFACE IS RCOR=RAD
C       REFERENCE RAY INTERSECTION WITH THE TANGENT PLANE IS:
C
                  RCOR=RAD
C
C       THE LENGTH FROM THE REFERENCE SPHERE
C       TO THE FINAL SURFACE ALONG THE "OTHER" RAY IS:
C
                  AL=RAYRAY(1,NEWIMG)-XREFI
                  BE=RAYRAY(2,NEWIMG)-YREFI
                  GA=RAYRAY(3,NEWIMG)-ZREFI
                  C=(AL**2)+(BE**2)+(GA**2)-(RAD**2)
                  B=2.0D0*((RAYRAY(19,NEWIMG)*AL)+
     1              (RAYRAY(20,NEWIMG)*BE)+
     2              (RAYRAY(21,NEWIMG)*GA))
                  A=1.0D0
C       SOLVE FOR LEN
                  IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
                  IF(B.EQ.0.0D0) SIGNB=1.0D0
                  IF(A.EQ.0.0D0) THEN
                      LEN=-(C/B)
                  ELSE
                      ARG=((B**2)-(4.0D0*A*C))
                      IF(ARG.LT.0.0D0) THEN
                          OUTLYNE='RAY(S) COULD NOT INTERSECT REFERENCE SPHERE'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'TRY USING AN "ASTOP EX" OR ASTOP "EN/EX" ADJUSTMENT'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'OPD VALUES MAY BE SUSPECT'
                          CALL SHOWIT(1)
                          OCOR=0.0D0
                          RCOR=0.0D0
                          RETURN
C       REFERENCE SPHERE INTERSECTED
                      END IF
                      Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
                      LEN1=C/Q
                      LEN2=Q/A
                      IF(REFRY(6,NEWIMG).GT.0.0D0) THEN
                          IF(RAD.GT.0.0D0) THEN
C     THEN THE INTERSECTION TO USE IS THE NEGATIVE INTERSECTION
                              IF(LEN1.LT.0.0D0) LEN=LEN1
                              IF(LEN2.LT.0.0D0) LEN=LEN2
                          ELSE
C       RAD NEG
C     THEN THE INTERSECTION TO USE IS THE POSITIVE INTERSECTION
                              IF(LEN1.GT.0.0D0) LEN=LEN1
                              IF(LEN2.GT.0.0D0) LEN=LEN2
                          END IF
                      END IF
                      IF(REFRY(6,NEWIMG).LT.0.0D0) THEN
                          IF(RAD.GT.0.0D0) THEN
C     THEN THE INTERSECTION TO USE IS THE NEGATIVE INTERSECTION
                              IF(LEN1.LT.0.0D0) LEN=-LEN2
                              IF(LEN2.LT.0.0D0) LEN=-LEN1
                          ELSE
C       RAD NEG
C     THEN THE INTERSECTION TO USE IS THE POSITIVE INTERSECTION
                              IF(LEN1.GT.0.0D0) LEN=-LEN2
                              IF(LEN2.GT.0.0D0) LEN=-LEN1
                          END IF
                      END IF
                  END IF
C       NEGATIVE SIGN SINCE LEN1 AND LEN2 MEASURES DIRECTED
C       DISTANCE FROM IMAGE TO SPHERE INTERSECTION AND
C       LEN IS DIRECTED FROM SPHERE INTERSECTION TO IMAGE

                  OCOR=-LEN
                  GO TO 900
C     EXPAUT NOT FALSE
              END IF
              IF(EXPAUT) THEN
C       THE REFERENCE SPHERE IS CENTERED WHERE THE REAL CHIEF
C       RAY CROSSES THE FINAL SURFACE.
C       THE RADIUS IS EQUAL
C       TO THE DISTANCE FROM POINT TO WHERE THE CHIEF RAY CROSSES THE REAL
C     EXIT PUPIL
C       THIS IS JUST AS IN CODE-V AND IS THE DEFAULT WHEN THE PROGRAM
C     BEGINS
C
C       COORDINATES OF THE REAL CHIEF RAY AT THE IMAGE PLANE ARE:
C       THIS IS THE REFERENCE SPHERE CENTER LOCATION.
                  XREFI=REFRY(1,NEWIMG)
                  YREFI=REFRY(2,NEWIMG)
                  ZREFI=REFRY(3,NEWIMG)
C
C       DO A REAL RAY INTERNAL EXIT PUPIL ADJUSTMENT
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
                  THXNUM=((XO-XOOX)*(LO-LOOX))+((YO-YOOX)*(MO-MOOX))
     1            +((ZO-ZOOX)*(NO-NOOX))
                  THYNUM=((XO-XOOY)*(LO-LOOY))+((YO-YOOY)*(MO-MOOY))
     1            +((ZO-ZOOY)*(NO-NOOY))
                  THXDEN=((LO-LOOX)**2)+((MO-MOOX)**2)
     1            +((NO-NOOX)**2)
                  THYDEN=((LO-LOOY)**2)+((MO-MOOY)**2)
     1            +((NO-NOOY)**2)
                  IF(DABS(THXNUM*1.0D-15).LT.DABS(THXDEN).AND.
     1            DABS(THYNUM*1.0D-15).LT.
     1            DABS(THYDEN)) THEN
                      RAD=-((-THXNUM/THXDEN)+(-THYNUM/THYDEN))/2.0D0
C     THE EXIT PUPIL IS LOCATED AT:
                      XA=XO+(-RAD*LO)
                      YA=YO+(-RAD*MO)
                      ZA=ZO+(-RAD*NO)
                      RAD=DSQRT(
     1                ((XA-XREFI)**2)+((YA-YREFI)**2)+((ZA-ZREFI)**2)
     1                )
                      IF(ZA.LE.ZO) THEN
C     PUPIL LIES ON THE NEGATIVE SIDE OF NEWIMG SO RAD IS POS
                          RAD=RAD
                      ELSE
                          RAD=-RAD
                      END IF
                  END IF
                  IF(DABS(THXNUM*1.0D-15).GE.DABS(THXDEN).OR.
     1            DABS(THYNUM*1.0D-15).GE.
     1            DABS(THYDEN)) THEN
C       REF RAY IS TELECENTRIC, SET RAD =1.0D20
                      RAD=1.0D20
                  END IF
C
C       NOW WE HAVE LOCATION OF REFERENCE SPHERE CENTER
C       AND ITS RADIUS MAGNITUDE
C
C       THE CHIEF RAY INTERSECTS THE REFERENCE SPHERE
C       THE LENGTH FROM THIS INTERSECTION TO THE INTERSECTION
C       OF THE REFERENCE RAY WITH THE FINAL SURFACE IS RCOR=RAD
C       REFERENCE RAY INTERSECTION WITH THE TANGENT PLANE IS:
C
                  RCOR=RAD
C
C       THE LENGTH FROM THE REFERENCE SPHERE
C       TO THE FINAL SURFACE ALONG THE "OTHER" RAY IS:
C
                  LLL=RAYRAY(19,NEWIMG)
                  MMM=RAYRAY(20,NEWIMG)
                  NNN=RAYRAY(21,NEWIMG)
                  AL=RAYRAY(1,NEWIMG)-XREFI
                  BE=RAYRAY(2,NEWIMG)-YREFI
                  GA=RAYRAY(3,NEWIMG)-ZREFI
                  C=(AL**2)+(BE**2)+(GA**2)-(RAD**2)
                  B=2.0D0*((LLL*AL)+
     1              (MMM*BE)+
     2              (NNN*GA))
                  A=1.0D0
C       SOLVE FOR LEN
                  IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
                  IF(B.EQ.0.0D0) SIGNB=1.0D0
                  IF(A.EQ.0.0D0) THEN
                      LEN=-(C/B)
                  ELSE
                      ARG=((B**2)-(4.0D0*A*C))
                      IF(ARG.LT.0.0D0) THEN
                          OUTLYNE='RAY(S) COULD NOT INTERSECT REFERENCE SPHERE'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'THE CURRENT SYSTEM MAY HAVE EXCESSIVE ABERRATIONS'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'OPD VALUES MAY BE SUSPECT'
                          CALL SHOWIT(1)
                          OCOR=0.0D0
                          RCOR=0.0D0
                          RETURN
C       REFERENCE SPHERE INTERSECTED
                      END IF
                      Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
                      LEN1=C/Q
                      LEN2=Q/A
                      IF(REFRY(6,NEWIMG).GT.0.0D0) THEN
                          IF(RAD.GT.0.0D0) THEN
C     THEN THE INTERSECTION TO USE IS THE NEGATIVE INTERSECTION
                              IF(LEN1.LT.0.0D0) LEN=LEN1
                              IF(LEN2.LT.0.0D0) LEN=LEN2
                          ELSE
C       RAD NEG
C     THEN THE INTERSECTION TO USE IS THE POSITIVE INTERSECTION
                              IF(LEN1.GT.0.0D0) LEN=LEN1
                              IF(LEN2.GT.0.0D0) LEN=LEN2
                          END IF
                      END IF
                      IF(REFRY(6,NEWIMG).LT.0.0D0) THEN
                          IF(RAD.GT.0.0D0) THEN
C     THEN THE INTERSECTION TO USE IS THE NEGATIVE INTERSECTION
                              IF(LEN1.LT.0.0D0) LEN=-LEN2
                              IF(LEN2.LT.0.0D0) LEN=-LEN1
                          ELSE
C       RAD NEG
C     THEN THE INTERSECTION TO USE IS THE POSITIVE INTERSECTION
                              IF(LEN1.GT.0.0D0) LEN=-LEN2
                              IF(LEN2.GT.0.0D0) LEN=-LEN1
                          END IF
                      END IF
                  END IF
C       NEGATIVE SIGN SINCE LEN1 AND LEN2 MEASURES DIRECTED
C       DISTANCE FROM IMAGE TO SPHERE INTERSECTION AND
C       LEN IS DIRECTED FROM SPHERE INTERSECTION TO IMAGE
                  OCOR=-LEN
                  GO TO 900
C     EXPAUT NOT TRUE
              END IF
              GO TO 900
C     NOT FOCAL
          END IF
 900      CONTINUE
          IF(DABS(RCOR).GT.1.0D10.OR.DABS(OCOR).GT.1.0D10) THEN
C     INFINITE REFERENCE SPHERE
              RCOR=0.0D0
C     OCOR IS THE DISTANCE ALONG THE RAY BETWEEN AN INTERSECTION
C     WITH A PLANE PERPENDICULAR TO A PARALLEL RAY WHICH PASSES
C     THROUGH A POINT ON THE IMAGE SURFACE WHERE THE CHIEF RAY
C     HITS
              RCOR=0.0D0
              RX0=REFRY(1,NEWIMG-1)
              RY0=REFRY(2,NEWIMG-1)
              RZ0=REFRY(3,NEWIMG-1)
              RL0=RAYRAY(19,NEWIMG-1)
              RM0=RAYRAY(20,NEWIMG-1)
              RN0=RAYRAY(21,NEWIMG-1)
C       NOW THE OTHER RAY:
              RRX0=RAYRAY(1,NEWIMG-1)
              RRY0=RAYRAY(2,NEWIMG-1)
              RRZ0=RAYRAY(3,NEWIMG-1)
              L0=RAYRAY(19,NEWIMG-1)
              M0=RAYRAY(20,NEWIMG-1)
              N0=RAYRAY(21,NEWIMG-1)
C       THE LINE DIRECTION NUMBERS ARE:
C
C       THE PLANE WHICH HAS ITS ORIGIN WHERE THE
C       REFERENCE RAY INTERSECTS THE IMAGE PLANE
C       AND IS PERPENDICULAR TO THE REGULAR RAY HAS THE FORM:
C
C       (RL0*(X-RX0))+(RM0*(Y-RY0))+(RN0*(Z-RZ0))=0
C       X,Y AND Z ARE ANY POINTS ON THE PLANE.
C
C       NOW DETERMINE THE INTERSECTION OF AN "OTHER" RAY
C       WITH THIS REFERENCE PLANE.
C
C       THE EQUATION OF THE OTHER RAY IS
C               XA=RRX0+(T*L0)
C               YA=RRY0+(T*M0)
C               ZA=RRZ0+(T*N0)
C THE PARAMETER T IS DETERMINED BY SUBSTITUTING THIS LINE
C       EQUATION INTO THE REFERENCE SURAFCE EQUATION
C       AND SOLVING FOR T.
C
              T=((RL0*(RX0-RRX0))+(RM0*(RY0-RRY0))+(RN0*(RZ0-RRZ0)))/
     1        ((RL0*L0)+(RM0*M0)+(RN0*N0))
C
C       NOW X,Y AND Z ARE:
              XA=RRX0+(T*L0)
              YA=RRY0+(T*M0)
              ZA=RRZ0+(T*N0)
              OCOR=DSQRT(
     1        ((RAYRAY(1,NEWIMG-1)-XA)**2)+
     2        ((RAYRAY(2,NEWIMG-1)-YA)**2)+
     3        ((RAYRAY(3,NEWIMG-1)-ZA)**2))
C     IF T IS LESS THAN ZERO, THE RAY INTERSECTION WITH THE
C     REFERENCE PLANE LIES ON THE NEGATIVE SIDE OF THE IMAGE SURFACE
C     AND ZA LT RAYRAY(3,NEWIMG-1) AND OCOR MUST BE POSITIVE AND VICE VERSA.
C       THE CORRECT SIGN IS:
              IF(ZA.GT.RAYRAY(3,NEWIMG-1)) OCOR=-OCOR
              IF(ZA.LE.RAYRAY(3,NEWIMG-1)) OCOR=OCOR
          ELSE
          END IF
          RETURN
      END
C SUB GPXT.FOR
      SUBROUTINE GPXT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GPXT. THIS SUBROUTINE IMPLEMENTS
C       THE GENERALIZED PARAXIAL RAY TRACE PRINTOUT COMMANDS
C       "GPXTX" AND "GPXTY"
C
          INTEGER SF,I,ERROR
C
          REAL*8 PX,PY,PCX,PCY,PUX,PUY,PUCX,PUCY,DWORD2,DWORD3
     1    ,DWORD4,DWORD5
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          GPRAYEXT=.FALSE.
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=SYSTEM1(11)
          DWORD2=W2
          DWORD3=W3
          DWORD4=W4
          DWORD5=W5
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"GPXT(XorY)" OUTPUT GENERALIZED PARAXIAL DATA'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        'GENERALIZED PARAXIAL RAYTRACE OUTPUT COMMANDS TAKE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        'GENERALIZED PARAXIAL RAYTRACE OUTPUT COMMANDS TAKE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'QUALIFIER OR NUMERIC WORD #1 INPUT BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DWORD5.LT.1.0D0.OR.DWORD5.GT.10.0D0) THEN
              WRITE(OUTLYNE,*)'INVALID WAVELENGTH NUMBER ISSUED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO GENERALIZED PARAXIAL DATA EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO GENERALIZED PARAXIAL DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ALL') THEN
              CALL GNPRTGEN(0,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1        ,DWORD2,DWORD3,DWORD4,DWORD5,0)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DIFFERENTIAL RAY VALUES NOT CALCULABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  GPRAYEXT=.TRUE.
              END IF
              SF=INT(SYSTEM1(20))
              IF(WC.EQ.'GPXTX') WRITE(OUTLYNE,6001) INT(F12)
              IF(WC.EQ.'GPXTY') WRITE(OUTLYNE,6004) INT(F12)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6006) INT(DWORD5)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6007) DWORD2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6008) DWORD3
              CALL SHOWIT(0)
              WRITE(OUTLYNE,6009) DWORD4
              CALL SHOWIT(0)
              IF(WC.EQ.'GPXTX') WRITE(OUTLYNE,6002)
              IF(WC.EQ.'GPXTY') WRITE(OUTLYNE,6005)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2501)
              CALL SHOWIT(0)
              IF(WC.EQ.'GPXTX') WRITE(OUTLYNE,6000)
              IF(WC.EQ.'GPXTY') WRITE(OUTLYNE,6003)
              CALL SHOWIT(0)
              DO 10 I=0,SF
                  CALL GNPRTGEN(I,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1            ,DWORD2,DWORD3,DWORD4,DWORD5,0)
                  IF(ERROR.EQ.1) THEN
                      WRITE(OUTLYNE,*)'RAY COULD NOT BE TRACED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'DIFFERENTIAL RAY VALUES NOT CALCULABLE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      GPRAYEXT=.TRUE.
                  END IF
                  GPXY(1,I)=PX
                  GPXY(2,I)=PY
                  GPXY(3,I)=PCX
                  GPXY(4,I)=PCY
                  IF(WC.EQ.'GPXTX') WRITE(OUTLYNE,1500)I,PX,PUX,PCX,PUCX
                  IF(WC.EQ.'GPXTY') WRITE(OUTLYNE,1500)I,PY,PUY,PCY,PUCY
                  CALL SHOWIT(0)
 10           CONTINUE
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1.AND.WQ.EQ.'OB') THEN
              SF=0
              CALL GNPRTGEN(SF,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1        ,DWORD2,DWORD3,DWORD4,DWORD5,0)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DIFFERENTIAL RAY VALUES NOT CALCULABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  GPRAYEXT=.TRUE.
              END IF
              IF(WC.EQ.'GPXTX')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PX,PUX,PCX,PUCX
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'GPXTY')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6003)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PY,PUY,PCY,PUCY
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'OBJ'.OR.SQ.EQ.1.AND.
     1    WQ.NE.'ALL'.OR.SQ.EQ.1.AND.WQ.NE.'OB') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
C       OUTPUT IMAGE SURFACE
              SF=INT(SYSTEM1(20))
              CALL GNPRTGEN(SF,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1        ,DWORD2,DWORD3,DWORD4,DWORD5,0)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DIFFERENTIAL RAY VALUES NOT CALCULABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  GPRAYEXT=.TRUE.
              END IF
              IF(WC.EQ.'GPXTX')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PX,PUX,PCX,PUCX
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'GPXTY')THEN
                  IF(HEADIN) WRITE(OUTLYNE,6003)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500) SF,PY,PUY,PCY,PUCY
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(SQ.EQ.0.AND.DF1.NE.1) THEN
              I=INT(W1)
              SF=INT(SYSTEM1(20))
              IF(I.GT.SF.OR.I.LT.0) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL GNPRTGEN(I,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR
     1        ,DWORD2,DWORD3,DWORD4,DWORD5,0)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DIFFERENTIAL RAY VALUES NOT CALCULABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  GPRAYEXT=.TRUE.
              END IF
              IF(WC.EQ.'GPXTX') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6000)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)I,PX,PUX,PCX,PUCX
                  CALL SHOWIT(0)
              END IF
              IF(WC.EQ.'GPXTY') THEN
                  IF(HEADIN) WRITE(OUTLYNE,6003)
                  IF(HEADIN) CALL SHOWIT(0)
                  WRITE(OUTLYNE,1500)I,PY,PUY,PCY,PUCY
                  CALL SHOWIT(0)
              END IF
          END IF
 1500     FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6)
C
C       GPXTX
C
 6000     FORMAT('SURF',6X,'PX',12X,'PUX',13X,'PCX',11X,'PUCX')
 6001     FORMAT('GENERALIZED PARAXIAL RAYTRACE DATA (XZ-PLANE)',
     1    ' - (CFG #',I2,')')
 6002     FORMAT(
     1    '(PUX AND PUCX) MEASURED WITH RESPECT TO THE Z-AXIS')
C
C       GPXTY
C
 6003     FORMAT('SURF',6X,'PY',12X,'PUY',13X,'PCY',11X,'PUCY')
 6004     FORMAT('GENERALIZED PARAXIAL RAYTRACE DATA (YZ-PLANE)',
     1    ' - (CFG #',I2,')')
 6005     FORMAT(
     1    '(PUY AND PUCY) MEASURED WITH RESPECT TO THE Z-AXIS')
 2501     FORMAT(1X)
 6006     FORMAT('TRACED AT WAVELENGTH NUMBER = ',I2)
 6007     FORMAT('TRACED FROM Y-FOB =  ',G13.6)
 6008     FORMAT('TRACED FROM X-FOB =  ',G13.6)
 6009     FORMAT('WITH OBJ. Z-SHIFT =  ',G13.6,' LENS UNITS')
C
      END
C SUB PGLPRY.FOR
      SUBROUTINE PGLPRY
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE GLPRAY.FOR. THIS SUBROUTINE IMPLEMENTS
C       CREATION OF GLOBAL RAY DATA FOR THE LAST RAY TRACED.
C       FOR RAY PLOTTING IT IS CALLED BY RAYTRA.FOR
C
          INTEGER JK,I
C
          REAL*8 X,Y,Z,
     2    X1,Y1,Z1,X00,Y00,Z0,LX0,LY0,LZ0,
     5    MX0,MY0,MZ0,NX0,NY0,NZ0
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(DABS(ALENS(3,NEWOBJ)).LE.1.0D10) THEN
C       FINITE OBJECT THICKNESS
              JK=NEWOBJ
          ELSE
C       INFINITE OBJECT THICKNESS
              JK=NEWOBJ+1
          END IF
C
          DO 10 I=JK,NEWIMG
C
C       NOW TRANSFORM TO THE GLOBAL COORDINATE SYSTEM.
C       THE GLOBAL COORDINATES OF EACH LOCAL COORDINATE
C       SYSTEM ARE:
C
              X00=VERTEX(1,I)
              Y00=VERTEX(2,I)
              Z0=VERTEX(3,I)
              LX0=VERTEX(4,I)
              MX0=VERTEX(5,I)
              NX0=VERTEX(6,I)
              LY0=VERTEX(7,I)
              MY0=VERTEX(8,I)
              NY0=VERTEX(9,I)
              LZ0=VERTEX(10,I)
              MZ0=VERTEX(11,I)
              NZ0=VERTEX(12,I)
C
              X=GPXY(1,I)
              Y=0.0D0
              Z=0.0D0
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(X)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              PGLPRAY(1,I)=X1
              PGLPRAY(2,I)=Y1
              PGLPRAY(3,I)=Z1
              X=0.0D0
              Y=GPXY(2,I)
              Z=0.0D0
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(X)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              PGLPRAY(4,I)=X1
              PGLPRAY(5,I)=Y1
              PGLPRAY(6,I)=Z1
              X=GPXY(3,I)
              Y=0.0D0
              Z=0.0D0
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(X)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              PGLPRAY(7,I)=X1
              PGLPRAY(8,I)=Y1
              PGLPRAY(9,I)=Z1
              X=0.0D0
              Y=GPXY(4,I)
              Z=0.0D0
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(X)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              PGLPRAY(10,I)=X1
              PGLPRAY(11,I)=Y1
              PGLPRAY(12,I)=Z1
 10       CONTINUE
C       ALL GLOBAL TRANSFORMATIONS ARE COMPLETE
          RETURN
      END
