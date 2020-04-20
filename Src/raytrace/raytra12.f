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

C      THIS IS THE TWELFTH FILE OF RAYTRACING ROUTINES
C
C SUB REFRAY.FOR
      SUBROUTINE REFRAY(WPAS)
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE REFRAY.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE TRACING OF THE GUT OR REFERENCE RAY. THIS IS INITIATED
C       BY THE FOB COMMAND.
C
          INTEGER KKK,ISYS20,I,KKKK,N_HITS

          INTEGER IISURF,CAERAS,COERAS
C
          REAL*8 X,Y,Z,L,M,N,SNINDX,LOLD,MOLD,NOLD,WPAS
     1    ,D21,D22,LS,SNIND2,OXSTRT,OYSTRT,OLDCX,OLDCY,
     2    D11,D12,STX,STY,STZ,MAG,
     3    LSTART,MSTART,NSTART,YANG,XANG,X1LAST,LARGE,
     4    Y1LAST,X1ONE,Y1ONE,RXLAST,RYLAST,RXONE,RYONE,DDELX,DDELY,
     5    YYANG,XXANG,MF1,MF2,TARX,TARY,XFOBB0,YFOBB0,ZFOBB0
     6    ,ANGJK1,ANGJK2,XC1,YC1,ZC1,WWWW1,WWWW2,WWWW3,XCHIEF_TARGET
     7    ,ZSAG,SCLFACY,SCLFACX,AWW1,AWW2,YCHIEF_TARGET,CX,CY
     8    ,CX1LAST,CY1LAST,CX1ONE,CY1ONE,CRXLAST,CRYLAST,CRXONE,CRYONE,
     9    CD11,CD12,CD21,CD22,CDET,CDDELX,CDDELY,CMF1,CMF2
C
          REAL*8 XRAYER,YRAYER,ZRAYER,STEPL,STEPL1,ERRORVEC
          COMMON/RAYERPASS/XRAYER,YRAYER,ZRAYER
C
          COMMON/STRSTR/STX,STY,STZ
C
          COMMON/OHFOB/XFOBB0,YFOBB0,ZFOBB0
C
          COMMON/CACO/CAERAS,COERAS,LS
C
C       FOBB0=TRUE FOR ON AXIS AND FALSE FOR OFF AXIS
          LOGICAL SAGERR,FOBB0,FOBB0X,FOBB0Y,CDELFAIL,DELFAIL
C
          COMMON/FFOOBB/FOBB0,FOBB0X,FOBB0Y
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          KKKK=0
          KKK=0
          YAIMOL=0.0D0
          XAIMOL=0.0D0
          ZAIMOL=0.0D0
          DDELX=0.001D0
          DDELY=0.001D0
          CDDELX=0.0001D0*SYSTEM1(16)
          CDDELY=0.0001D0*SYSTEM1(14)
          IF(WC.EQ.'FOB') THEN
              IF(SYSTEM1(21).EQ.0.0D0) THEN
                  SCLFACY=1.0D0
              ELSE
                  SCLFACY=(1.0D0/SYSTEM1(21))
              END IF
              IF(SYSTEM1(23).EQ.0.0D0) THEN
                  SCLFACX=1.0D0
              ELSE
                  SCLFACX=(1.0D0/SYSTEM1(23))
              END IF
              AWW1=WW1/SCLFACY
              AWW2=WW2/SCLFACX
          END IF
          IF(WC.EQ.'FOBH') THEN
              SCLFACY=1.0D0
              SCLFACX=1.0D0
              AWW1=WW1/SCLFACY
              AWW2=WW2/SCLFACX
          END IF
C
          WWWW1=WW1
          WWWW2=WW2
          WWWW3=WW3
C
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
C       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
C       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
C       CHIEF RAY AIMING.
C
C     THE CHECK FOR TILTED AND DECENTERD OBJECT SURFACE HERE WAS REMOVED
C     ON 10/11/94
          LARGE=999999.99D0
          X1ONE=LARGE
          Y1ONE=LARGE
          X1LAST=LARGE
          Y1LAST=LARGE
          CX1ONE=LARGE
          CY1ONE=LARGE
          CX1LAST=LARGE
          CY1LAST=LARGE
C
C       SET INITIAL VALUES FOR RXONE, RYONE, RXLAST AND RYLAST
          RXONE=LARGE
          RYONE=LARGE
          RXLAST=LARGE
          RYLAST=LARGE
          CRXONE=LARGE
          CRYONE=LARGE
          CRXLAST=LARGE
          CRYLAST=LARGE
C
C       THE FIRST THING TO DO IS TO DETERMINE THE STARTING
C       COORDINATES OF THE RAY AT THE OBJECT SURFACE. FIRST
C       CONSIDER THAT WW5 IS ZERO AND THERE IS NO RE-ASSIGNMENT
C       OF OBJECT,REFERENCE OR IMAGE.
C
C       THE RELATIVE OBJECT HEIGHTS ARE TRANSLATED INTO REAL
C       X,Y AND Z POSITIONS WITH RESPECT TO THE OBJECT SURFACE HERE.
C
C       WW1 IS THE FRACTIONAL Y- OBJECT HEIGHT
C       WW2 IS THE FRACTIONAL X- OBJECT HEIGHT
C       WW3 IS THE FRACTIONAL OBJECT DISPLACEMENT FROM THE OBJECT SURFACE
C               PLANE. (ALLOWS FOR DEFINITION OF CURVED OBJECTS)
C       EXPRESSED AS THE WW3 FRACTION OF THE OBJECT SURFACE THICKNESS
C       WW4 IS THE WAVELENGTH NUMBER (1 TO 5)
C       WW5 IS CURRENTLY ASSUMED TO BE ZERO.
C
C       THE STARTING RAY COORDINATES FOR THE RAY TRACE ARE:
C       (AT THE OBJECT SURFACE)
C
          IF(WC.EQ.'FOBH') THEN
C       OBJ ANGLES CANT BE INPUT
              SYSTEM1(18)=0.0D0
              SYSTEM1(19)=0.0D0
          END IF
          IF(SYSTEM1(18).EQ.1.AND.NEWOBJ.EQ.0.OR.
     1    SYSTEM1(19).EQ.1.AND.NEWOBJ.EQ.0) THEN
C     OBJECT ANGLES INPUT
              ANGIN=.TRUE.
C     REF OBJ HT INPUT AS ANGLE
C     IN THIS CASE, FOB INDICATES FRACTIONS OF FIELD ANGLES
C     BUT ONLY IF THE NEWOBJ SURFACE IS SURFACE 0
C
C     THIS NEEDS MODIFICATION FOR FRACTIONS OF FIELD ANGLES
C
C     ANGLES IN RADIANS ARE:
              ANGJK1=AWW1
              ANGJK2=AWW2

              IF(ANGJK1.GT.89.9999D0.AND.
     1        ANGJK1.LT.90.0001D0)  ANGJK1=89.9999D0

              IF(ANGJK1.LT.-89.9999D0.AND.
     1        ANGJK1.GT.-90.0001D0) ANGJK1=-89.9999D0

              IF(ANGJK1.GT.-270.0001D0.AND.
     1        ANGJK1.LT.-269.9999D0) ANGJK1=89.9999D0

              IF(ANGJK1.GT.269.9999D0.AND.
     1        ANGJK1.LT.270.0001D0) ANGJK1=-89.9999D0

              IF(ANGJK2.GT.89.9999D0.AND.
     1        ANGJK2.LT.90.0001D0)  ANGJK2=89.9999D0

              IF(ANGJK2.LT.-89.9999D0.AND.
     1        ANGJK2.GT.-90.0001D0) ANGJK2=-89.9999D0

              IF(ANGJK2.GT.-270.0001D0.AND.
     1        ANGJK2.LT.-269.9999D0) ANGJK2=89.9999D0

              IF(ANGJK2.GT.269.9999D0.AND.
     1        ANGJK2.LT.270.0001D0) ANGJK2=-89.9999D0

C
              YYANG=ANGJK1*PII/180.0D0
              XXANG=ANGJK2*PII/180.0D0
C
              IF(DABS(ANGJK1).GT.90.0D0) YSTRT=DTAN(YYANG)*ALENS(3,NEWOBJ)
              IF(DABS(ANGJK1).LE.90.0D0) YSTRT=-DTAN(YYANG)*ALENS(3,NEWOBJ)
C
              IF(DABS(ANGJK2).GT.90.0D0) XSTRT=DTAN(XXANG)*ALENS(3,NEWOBJ)
              IF(DABS(ANGJK2).LE.90.0D0) XSTRT=-DTAN(XXANG)*ALENS(3,NEWOBJ)

              ZSTRT=2.0D0*ALENS(3,NEWOBJ)

              IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1        ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
C
                  IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1            ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
                      ZSTRT=0.0D0
                  END IF
              END IF
              IF(ANGJK2.GT.90.0D0.AND.ANGJK2.LT.270.0D0.OR.
     1        ANGJK2.LT.-90.0D0.AND.ANGJK2.GT.-270.0D0) THEN
C
                  IF(ANGJK1.GT.90.0D0.AND.ANGJK1.LT.270.0D0.OR.
     1            ANGJK1.LT.-90.0D0.AND.ANGJK1.GT.-270.0D0) THEN
                      ZSTRT=0.0D0
                  END IF
              END IF
              IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1        ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1        ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1        ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
C
                  IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1            ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1            ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1            ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
                      ZSTRT=0.0D0
                  END IF
              END IF
              IF(ANGJK2.GE.0.0D0.AND.ANGJK2.LT.90.0D0.OR.
     1        ANGJK2.LE.0.0D0.AND.ANGJK2.GT.-90.0D0.OR.
     1        ANGJK2.GT.270.0D0.AND.ANGJK2.LE.0.0D0.OR.
     1        ANGJK2.LT.-270.0D0.AND.ANGJK2.GE.0.0D0) THEN
C
                  IF(ANGJK1.GE.0.0D0.AND.ANGJK1.LT.90.0D0.OR.
     1            ANGJK1.LE.0.0D0.AND.ANGJK1.GT.-90.0D0.OR.
     1            ANGJK1.GT.270.0D0.AND.ANGJK1.LE.0.0D0.OR.
     1            ANGJK1.LT.-270.0D0.AND.ANGJK1.GE.0.0D0) THEN
                      ZSTRT=0.0D0
                  END IF
              END IF
              IF(SYSTEM1(18).EQ.1.0D0.AND.SYSTEM1(21).EQ.0.0D0) YSTRT=0.0D0
              IF(SYSTEM1(19).EQ.1.0D0.AND.SYSTEM1(23).EQ.0.0D0) XSTRT=0.0D0
              IF(XSTRT.EQ.0.0D0.AND.YSTRT.EQ.0.0D0) ZSTRT=0.0D0
          ELSE
C     OBJECT ANGLES NOT INPUT
              ANGIN=.FALSE.
C
              IF(ALENS(9,NEWOBJ).EQ.0.OR.SYSTEM1(98).NE.0.0D0
     1        .OR.SYSTEM1(99).NE.0.0D0.OR.ALENS(127,NEWOBJ).NE.0.0D0) THEN
C     THE STARTING RAY COORDINATES ON THE OBJECT SURAFCE
C     ARE NOT DETERMINED BY THE CLAP ON THE OBJECT SURFACE
C     USING SCY AND SCX, FOB REPRESENTS FRACTIONS OF IMAGE HEIGHT
C     AND DEPTH.
                  IF(SYSTEM1(16).NE.0.0D0) XSTRT=WW2*PXTRAX(5,NEWOBJ)
                  IF(SYSTEM1(16).EQ.0.0D0) XSTRT=0.0D0
                  IF(SYSTEM1(14).NE.0.0D0) YSTRT=WW1*PXTRAY(5,NEWOBJ)
                  IF(SYSTEM1(14).EQ.0.0D0) YSTRT=0.0D0
                  IF(SYSTEM1(98).NE.0.0D0.OR.SYSTEM1(99).NE.0.0D0) THEN
                      XSTRT=XSTRT+XRAYER
                      YSTRT=YSTRT+YRAYER
                  END IF
                  IISURF=NEWOBJ
                  CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
                  IF(SAGERR) THEN
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
                          CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
                          CALL SHOWIT(1)
                      END IF
                      STOPP=1
                      RAYCOD(1)=16
                      RAYCOD(2)=NEWOBJ
                      REFEXT=.FALSE.
                      CALL MACFAL
                      RETURN
                  END IF
                  ZSTRT=(WW3*ALENS(3,NEWOBJ))+ZSAG
              ELSE
C     USE Y HEIGHT OF CLAP WITH OFFSETS
                  IF(ALENS(9,NEWOBJ).EQ.1.0D0.AND.
     1            ALENS(127,NEWOBJ).EQ.0.0D0) THEN
                      IF(ALENS(10,NEWOBJ).LE.ALENS(11,NEWOBJ)) THEN
                          XSTRT=WW2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
                          YSTRT=WW1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
                      ELSE
                          XSTRT=WW2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
                          YSTRT=WW1*(ALENS(11,NEWOBJ)+ALENS(12,NEWOBJ))
                      END IF
                      IISURF=NEWOBJ
                      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
                      IF(SAGERR) THEN
                          IF(MSG) THEN
                              WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
                              CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
                              CALL SHOWIT(1)
                          END IF
                          STOPP=1
                          RAYCOD(1)=16
                          RAYCOD(2)=NEWOBJ
                          REFEXT=.FALSE.
                          CALL MACFAL
                          RETURN
                      END IF
                      ZSTRT=(WW3*ALENS(3,NEWOBJ))+ZSAG
                  END IF
                  IF(ALENS(9,NEWOBJ).EQ.5.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
                      XSTRT=WW2*(ALENS(10,NEWOBJ)+ALENS(13,NEWOBJ))
                      YSTRT=WW1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
                      IISURF=NEWOBJ
                      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
                      IF(SAGERR) THEN
                          IF(MSG) THEN
                              WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
                              CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
                              CALL SHOWIT(1)
                          END IF
                          STOPP=1
                          RAYCOD(1)=16
                          RAYCOD(2)=NEWOBJ
                          REFEXT=.FALSE.
                          CALL MACFAL
                          RETURN
                      END IF
                      ZSTRT=(WW3*ALENS(3,NEWOBJ))+ZSAG
                  END IF
                  IF(ALENS(9,NEWOBJ).EQ.6.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
                      XSTRT=WW2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
                      YSTRT=WW1*(ALENS(11,NEWOBJ)+ALENS(12,NEWOBJ))
                      IISURF=NEWOBJ
                      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
                      IF(SAGERR) THEN
                          IF(MSG) THEN
                              WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
                              CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
                              CALL SHOWIT(1)
                          END IF
                          STOPP=1
                          RAYCOD(1)=16
                          RAYCOD(2)=NEWOBJ
                          REFEXT=.FALSE.
                          CALL MACFAL
                          RETURN
                      END IF
                      ZSTRT=(WW3*ALENS(3,NEWOBJ))+ZSAG
                  END IF
                  IF(ALENS(9,NEWOBJ).GT.1.0D0.AND.ALENS(9,NEWOBJ).LE.4.0D0.AND.
     1            ALENS(127,NEWOBJ).EQ.0.0D0) THEN
                      XSTRT=WW2*(ALENS(11,NEWOBJ)+ALENS(13,NEWOBJ))
                      YSTRT=WW1*(ALENS(10,NEWOBJ)+ALENS(12,NEWOBJ))
                      IISURF=NEWOBJ
                      CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
                      IF(SAGERR) THEN
                          IF(MSG) THEN
                              WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
                              CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
                              CALL SHOWIT(1)
                          END IF
                          STOPP=1
                          RAYCOD(1)=16
                          RAYCOD(2)=NEWOBJ
                          REFEXT=.FALSE.
                          CALL MACFAL
                          RETURN
                      END IF
                      ZSTRT=(WW3*ALENS(3,NEWOBJ))+ZSAG
                  END IF
              END IF
          END IF
C
C               XSTRT,YSTRT AND ZSTRT HAVE BEEN INITIALLY SET
C
C               NOW CALCULATE THE TARGET FOR THE CHIEF RAY IF RXIM AND RYIM
C               ARE IN EFFECT.
C
C       THE WAVELENGTH NUMBER FOR THE REFERENCE RAY TRACE IS:
C       INT(WW4) OR INT(SYSTEM1(11))
C     CALC AIM POINTS IF RYIM OR RXIM ARE IN EFFECT
          IF(SYSTEM1(99).NE.SYSTEM1(98))
     1    SYSTEM1(98)=SYSTEM1(99)
          IF(SYSTEM1(99).NE.SYSTEM1(98))
     1    SYSTEM1(96)=SYSTEM1(97)
          XCHIEF_TARGET=0.0D0
          YCHIEF_TARGET=0.0D0
          IF(SYSTEM1(98).NE.0.0D0) THEN
              XCHIEF_TARGET=(WW2*SYSTEM1(96))
          END IF
          IF(SYSTEM1(99).NE.0.0D0) THEN
              YCHIEF_TARGET=(WW1*SYSTEM1(97))
          END IF
C
C                       THE CHIEF RAY TARGETS HAVE BEEN COMPUTED
C
 9898     CONTINUE
          KKKK=KKKK+1
          KKK=0
C     THE ABOVE CONTINUATION IS THE RE-ENTRY POINT FOR CHIEF RAY
C     ITERATION
C
C       NOW WHAT ARE THE STARTING DIRECTION COSINES AND HOW ARE
C       THEY DETERMINED?
C
C     STARTING POINTS AT SURFACE NEWOBJ+1 ARE THE PARAXIAL VALUES
C     MODULATED BY ANY DECENTER OR TILT ON NEWOBJ+1
C

          IF(SYSTEM1(62).EQ.0.0D0) THEN
C     RAY AIMING IS OFF
              IF(SYSTEM1(63).EQ.0.0D0) THEN
C     TEL OFF
                  X1AIM=PXTRAX(5,(NEWOBJ+1))
                  X1AIM=(X1AIM*WW2)-ALENS(31,NEWOBJ+1)+SYSTEM1(81)
                  Y1AIM=PXTRAY(5,(NEWOBJ+1))
                  Y1AIM=(Y1AIM*WW1)-ALENS(30,NEWOBJ+1)+SYSTEM1(82)
                  Z1AIM=SYSTEM1(89)
              ELSE
C     TEL ON,ALL RAYS ARE PARALLEL
                  IF(SYSTEM1(16).NE.0.0D0) X1AIM=XSTRT
                  IF(SYSTEM1(16).EQ.0.0D0) X1AIM=0.0D0
                  IF(SYSTEM1(14).NE.0.0D0) Y1AIM=YSTRT
                  IF(SYSTEM1(14).EQ.0.0D0) Y1AIM=0.0D0
                  Z1AIM=0.0D0
              END IF
          ELSE
C     RAY AIMING IS ON
              X1AIM=TRYX+XFOBB0
              Y1AIM=TRYY+YFOBB0
              Z1AIM=TRYZ+ZFOBB0

          END IF

          XC=X1AIM
          YC=Y1AIM
          ZC=Z1AIM
C     SAVE VERY FIRST X1AIM,Y1AIM, Z1AIM
          STX=X1AIM
          STY=Y1AIM
          STZ=Z1AIM
C     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
          XAIMOL=XC
          YAIMOL=YC
          ZAIMOL=ZC
          R_TX=X1AIM
          R_TY=Y1AIM
          R_TZ=Z1AIM
          CALL BAKONE
          X1AIM=R_TX
          Y1AIM=R_TY
          Z1AIM=R_TZ

C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
C
C       FOR NOW, IGNORE THE FOLLOWING:
C               1) CLAPS/COBS AND ERASES
C       NOW THE STARTING DIRECTION COSINES ARE RELATIVELY SIMPLE.
C
C       THEY ARE CALLED LSTART,MSTART AND NSTART
C
C       FIRST CONVERT THE SURFACE 0 AND 1 COORDINATES INTO YANG AND XANG
C       ANGULAR VALUES AS IN HEXAGON AND CODE V.
C       YANG AND XANG ANGLES IN RADIANS.
          STOPP=0
          REFEXT=.TRUE.
C
C       THE STARTING COORDINATES AND DIRECTION COSINES FOR THE VERY
C       FIRST PART OF THE CHIEF OR "GUT" RAY TRACE HAVE BEEN CALCULATED
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
C       CALL TRANSF
C
C       2. INTERSECT AND INTERACT
C
C       CALL HITSUR
C
C       3. REPEAT WITH A DO LOOP
C
C       STORE THE REFERENCE RAY DATA FOR SURFACE 0 IN THE ARRAY
C       REFRY(1:50,0:MAXSUR)
C
C       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
C       SURFACE ZERO POINT INTERSECTION AND LABEL 9 IS THE RE-ENTRY POINT
C
C
C       THIS IS THE RE-ENTRY POINT FOR RAY AIMING. RE-ENTRY
C       IS PERFORMED AFTER NEW VALUES OF X1AIM AND Y1AIM WERE
C       CALCULATED JUST BEFORE ENCOUNTERING THE (GO TO 9)
C       STATEMENT NEAR THE END OF THIS ROUTINE.
 9        CONTINUE
          RV=.FALSE.
          KKK=KKK+1
C       IF KKK EXCEEDS NRAITR TRIES, PRINT RAY ITERRATION ERROR
C       AND STOP SEARCHING.
          IF(KKK.GT.NRAITR) THEN

              GO TO 14
          ELSE
              STOPP=0
              REFEXT=.TRUE.
C        PROCEED
          END IF
C
C     COMPUTE DIR COS DIRECTLY FROM POSITIONS
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
              IF(DABS(NSTART).EQ.0.0D0) THEN
                  YANG=0.0D0
              ELSE
                  YANG=DATAN2(MSTART,NSTART)
              END IF
              IF(DABS(NSTART).EQ.0.0D0) THEN
                  XANG=0.0D0
              ELSE
                  XANG=DATAN2(LSTART,NSTART)
              END IF
          END IF
          IF(NSTART.EQ.0.0D0) THEN
              YYANG=PII/2.0D0
              XXANG=PII/2.0D0
          ELSE
              IF(DABS(NSTART).EQ.0.0D0) THEN
                  YYANG=0.0D0
              ELSE
                  YYANG=DATAN2(MSTART,NSTART)
              END IF
              IF(DABS(NSTART).EQ.0.0D0) THEN
                  XXANG=0.0D0
              ELSE
                  XXANG=DATAN2(LSTART,NSTART)
              END IF
          END IF
C
C       FOR SURFACE NEWOBJ
C       REFRY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
C       REFRY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
C       REFRY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
C       REFRY(4,SURF)=L(LOCAL)- DIRECTION COSINE AT SURFACE I
C       REFRY(5,SURF)=M(LOCAL)- DIRECTION COSINE AT SURFACE I
C       REFRY(6,SURF)=N(LOCAL)- DIRECTION COSINE AT SURFACE I
C       REFRY(8,SURF)=LEN - PHYSICAL LENGTH ALONG RAY FROM SURF I-1 TO I
C       REFRY(7,SURF)=OPL - OPL FROM SURFACE I-1 TO I
C       REFRY(9,SURF)=COSINE(I)
C       REFRY(10,SURF)=COSINE(IP)
C       REFRY(11,SURF)=UX - (RADIAN) MARGINAL XZ SLOPE ANGLE
C       REFRY(12,SURF)=UY - (RADIAN) MARGINAL YZ SLOPE ANGLE
C       REFRY(13,SURF)=LN - SURFACE NORMAL DIRECTION COSINE
C       REFRY(14,SURF)=MN - SURFACE NORMAL DIRECTION COSINE
C       REFRY(15,SURF)=NN - SURFACE NORMAL DIRECTION COSINE
C       REFRY(16,SURF)=XOLD
C       REFRY(17,SURF)=YOLD
C       REFRY(18,SURF)=ZOLD
C       REFRY(19,SURF)=LOLD
C       REFRY(20,SURF)=MOLD
C       REFRY(21,SURF)=NOLD
C       REFRY(22,SURF)=OPL - OPL FROM SURFACE NEWOBJ TO I
C       REFRY(23,SURF)=1.00 FOR NOT REV, -1.00 FOR REV
C       REFRY(24,SURF)=1.0D0 FOR POSITIVE RAY, -1 FOR NEGATIVE RAY
C       REFRY(25,SURF)=FOB INTENSITY
C       REFRY(26 TO 31) ARE THE L,M AND N DIR COS OF THE RAY X AND Y
C       POINTER DIRECTIONS
C       REFRY(32,SURF) = WAVELENGTH NUMBER
C       REFRY(33,SURF) TO REFRY(35,SURF) RESERVED
          REFRY(25,NEWOBJ)=WPAS
          REFRY(32,NEWOBJ)=WW4
C       TRACK INTENSITY FOR ILLUMINATION RAY TRACING
          IF(WC.EQ.'IFOB') REFRY(25,NEWOBJ)=WW5
          REFRY(1,NEWOBJ)=XSTRT
          REFRY(2,NEWOBJ)=YSTRT
          REFRY(3,NEWOBJ)=ZSTRT
          REFRY(4,NEWOBJ)=LSTART
          REFRY(5,NEWOBJ)=MSTART
          REFRY(6,NEWOBJ)=NSTART
          REFRY(7,NEWOBJ)=0.0D0
          REFRY(8,NEWOBJ)=0.0D0
          IF(INT(WW4).GE.1.AND.INT(WW4).LE.5) THEN
              SNIND2=DABS(ALENS(45+INT(WW4),NEWOBJ))/ALENS(45+INT(WW4),NEWOBJ)
          END IF
          IF(INT(WW4).GE.6.AND.INT(WW4).LE.10) THEN
              SNIND2=DABS(ALENS(65+INT(WW4),NEWOBJ))/ALENS(65+INT(WW4),NEWOBJ)
          END IF
          IF(SNIND2.GT.0.0D0) REFRY(24,NEWOBJ)=1.0D0
          IF(SNIND2.LT.0.0D0) REFRY(24,NEWOBJ)=-1.0D0
          IF(SNIND2.GT.0.0D0) POSRAY=.TRUE.
          IF(SNIND2.LT.0.0D0) POSRAY=.FALSE.
          REFRY(9,NEWOBJ)=NSTART
          REFRY(10,NEWOBJ)=NSTART
          REFRY(11,NEWOBJ)=XXANG
          IF((REFRY(11,NEWOBJ)).LT.0.0D0)
     1    REFRY(11,NEWOBJ)=REFRY(11,NEWOBJ)+(TWOPII)
          REFRY(12,NEWOBJ)=YYANG
          IF((REFRY(12,NEWOBJ)).LT.0.0D0)
     1    REFRY(12,NEWOBJ)=REFRY(12,NEWOBJ)+(TWOPII)
          REFRY(13,NEWOBJ)=0.0D0
          REFRY(14,NEWOBJ)=0.0D0
          REFRY(15,NEWOBJ)=1.0D0
          REFRY(16,NEWOBJ)=XSTRT
          REFRY(17,NEWOBJ)=YSTRT
          REFRY(18,NEWOBJ)=ZSTRT
          REFRY(19,NEWOBJ)=LSTART
          REFRY(20,NEWOBJ)=MSTART
          REFRY(21,NEWOBJ)=NSTART
          REFRY(22,NEWOBJ)=0.0D0
          IF(ALENS(3,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
          IF(ALENS(3,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
C
          X=XSTRT
          Y=YSTRT
          Z=ZSTRT
          L=LSTART
          M=MSTART
          N=NSTART
          ISYS20=NEWIMG
          I=0
!                if (XSTRT.LT.-1000) stop
          GO TO 13
C       GO TO 14 IS THE ESCAPE FOR A FAILED RAY
C       GO TO 16 IS THE NON-ESCAPE ROUTE
 14       CONTINUE
C     NO REFERENCE RAY SOLUTION, REF RAY FAILURE
          STOPP=1
          RAYCOD(1)=8
          RAYCOD(2)=I
          REFEXT=.FALSE.
          IF(.NOT.NULL) FOBYES=.FALSE.
          IF(.NOT.NULL.AND.F34.EQ.0) CALL MACFAL
          RETURN
 13       CONTINUE
          STOPP=0
          REFEXT=.TRUE.
C     HERE BEGINS A LOOP TO FIND VALUES OF TRYX AND TRYY
C     WHICH ARE BETTER THAN THE ONES BASED ON THE PARAXIAL TRACE
C     TEMP SET NOREF TO .TRUE. SO ERROR MESSAGE IS ISSUED
C
          DO 10 I=(NEWOBJ+1),ISYS20
C
C       WE ARE CURRENTLY AT SURFACE I-1 AND KNOW WHERE
C       WE ARE AND WHICH DIRECTION WE WANT TO GO.
C       CALLING TRANSFS.FOR TRANSFORMS THE X,Y,Z,L,M AND N INTO
C       THE COORDINATE SYSTEM OF SURFACE I,FOB0 TRACKS IF FOB 0 IS
C       USED OR FOB (SOMETHING NON-ZERO)
              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              CALL TRANSF
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(NSTART.EQ.0.0D0) THEN
                  YANG=PII/2.0D0
                  XANG=PII/2.0D0
              ELSE
                  IF(DABS(NSTART).EQ.0.0D0)THEN
                      YANG=0.0D0
                  ELSE
                      YANG=DATAN2(MSTART,NSTART)
                  END IF
                  IF(DABS(NSTART).EQ.0.0D0)THEN
                      XANG=0.0D0
                  ELSE
                      XANG=DATAN2(LSTART,NSTART)
                  END IF
              END IF
              STOPP=0
              REFEXT=.TRUE.
C       NOW INTERSECT THE SURFACE I
C       THE COORDINATES OF THE CURRENT RAY AT SURF I-1 IN THE COORDINATE
C       SYSTEM OF SURFACE I ARE XOLD,YOLD AND ZOLD.
C
              XOLD=X
              YOLD=Y
              ZOLD=Z
              LOLD=L
              MOLD=M
              NOLD=N
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
              IF(REFRY(24,R_I-1).GT.0.0D0) POSRAY=.TRUE.
              IF(REFRY(24,R_I-1).LT.0.0D0) POSRAY=.FALSE.
              MSG=.FALSE.
              CALL HITSUR
              MSG=.TRUE.
              IF(STOPP.EQ.1) THEN
                  REFEXT=.FALSE.
                  RETURN
              END IF
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(NSTART.EQ.0.0D0) THEN
                  YANG=PII/2.0D0
                  XANG=PII/2.0D0
              ELSE
                  IF(DABS(NSTART).EQ.0.0D0) THEN
                      YANG=0.0D0
                  ELSE
                      YANG=DATAN2(MSTART,NSTART)
                  END IF
                  IF(DABS(NSTART).EQ.0.0D0) THEN
                      XANG=0.0D0
                  ELSE
                      XANG=DATAN2(LSTART,NSTART)
                  END IF
              END IF
              IF(RV) REFRY(23,I)=-1.0D0
              IF(.NOT.RV) REFRY(23,I)=1.0D0
C       LOAD REF RAY REGISTERS
C       FOR SURFACE NEWOBJ
C       REFRY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
C       REFRY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
C       REFRY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
C       REFRY(4,SURF)=L(LOCAL)- DIRECTION COSINE AT SURFACE I
C       REFRY(5,SURF)=M(LOCAL)- DIRECTION COSINE AT SURFACE I
C       REFRY(6,SURF)=N(LOCAL)- DIRECTION COSINE AT SURFACE I
C       REFRY(8,SURF)=LEN - PHYSICAL LENGTH ALONG RAY FROM SURF I-1 TO I
C       REFRY(7,SURF)=OPL - OPL FROM SURFACE I-1 TO I
C       REFRY(9,SURF)=COSINE(I)
C       REFRY(10,SURF)=COSINE(IP)
C       REFRY(11,SURF)=UX - (RADIAN) MARGINAL XZ SLOPE ANGLE
C       REFRY(12,SURF)=UY - (RADIAN) MARGINAL YZ SLOPE ANGLE
C       REFRY(13,SURF)=L DIRECTION COSINE OF SURFACE NORMAL
C       REFRY(14,SURF)=M DIRECTION COSINE OF SURFACE NORMAL
C       REFRY(15,SURF)=N DIRECTION COSINE OF SURFACE NORMAL
C       REFRY(16,SURF)=XOLD
C       REFRY(17,SURF)=YOLD
C       REFRY(18,SURF)=ZOLD
C       REFRY(19,SURF)=LOLD
C       REFRY(20,SURF)=MOLD
C       REFRY(21,SURF)=NOLD
C     THESE LAST 6 ITEMS ARE THE COORDINATES OF THE RAY AT
C     SURFACE I-1 IN THE COORDINATE SYSTEM OF SURFACE I
C       REFRY(22,SURF)=OPL - OPL FROM SURFACE NEWOBJ TO I
C       REFRY(23,SURF)=1.00 FOR NOT REV, -1.00 FOR REV
C       REFRY(24,SURF)=1.0D0 FOR POSITIVE RAY, -1 FOR NEGATIVE RAY
C       REFRY(25,SURF)=FOB INTENSITY
              REFRY(32,I)=WW4
C       REFRY(33,SURF) TO REFRY(35,SURF) RESERVED
              REFRY(25,I)=REFRY(25,I-1)
              REFRY(1,I)=X
              REFRY(2,I)=Y
              REFRY(3,I)=Z
              REFRY(4,I)=L
              REFRY(5,I)=M
              REFRY(6,I)=N
C
C     WHAT IS THE SIGN OF THE INDEX IN THE I-1 SPACE
              IF(INT(WW4).GE.1.AND.INT(WW4).LE.5) THEN
                  SNINDX=DABS(ALENS(45+INT(WW4),I-1))/ALENS(45+INT(WW4),I-1)
                  SNIND2=DABS(ALENS(45+INT(WW4),I))/ALENS(45+INT(WW4),I)
              END IF
              IF(INT(WW4).GE.6.AND.INT(WW4).LE.10) THEN
                  SNINDX=DABS(ALENS(65+INT(WW4),I-1))/ALENS(65+INT(WW4),I-1)
                  SNIND2=DABS(ALENS(65+INT(WW4),I))/ALENS(65+INT(WW4),I)
              END IF
C
C
C     PHYSICAL LENGTH OF THE RAY FROM I-1 TO I IS THE MAG OF THE
C     DIST TRAVELED TIME THE SNINDX (REVERSED IN SIGN IF RV) IN THE SPACE
C     FROM I-1 TO I
              IF(NUMHITS(I-1).EQ.1.AND.I.GE.1) THEN
                  REFRY(8,I)=DSQRT(
     1            ((REFRY(3,I)-ZOLD)**2)+((REFRY(2,I)-YOLD)**2)
     2            +((REFRY(1,I)-XOLD)**2))
              ELSE
C       NUMHITS STORED ON LAST SURFACE WAS GREATER THAN 1
                  REFRY(8,I)=0.0D0
C       I-1 WAS AN NSS
                  XOLD=0.0D0
                  YOLD=0.0D0
                  ZOLD=0.0D0
                  DO N_HITS=1,NUMHITS(I-1)
                      STEPL=DSQRT(
     2                (((MULTIREF_DATA(3,I-1,N_HITS))-ZOLD)**2)+
     2                (((MULTIREF_DATA(2,I-1,N_HITS))-YOLD)**2)+
     2                (((MULTIREF_DATA(1,I-1,N_HITS))-XOLD)**2)
     3                )
                      IF(N_HITS.EQ.1) STEPL1=STEPL
                      REFRY(8,I)=RAYRAY(8,I)+STEPL
C       CREATE NEW XOLD,YOLD,ZOLD
                      XOLD=MULTIREF_DATA(1,I-1,N_HITS)
                      YOLD=MULTIREF_DATA(2,I-1,N_HITS)
                      ZOLD=MULTIREF_DATA(3,I-1,N_HITS)
                  END DO
                  REFRY(8,I)=REFRY(8,I)-STEPL1
              END IF
C
C     IF THE RAY HAS NO Z-COMPONENT THEN ITS SIGN IS ASSUMED TO BE
C     NEGATIVE IF THE INDEX IS NEGATIVE UNLESS IT IS A REVERSING RAY
              IF(RV) REFRY(8,I)=-REFRY(8,I)
              IF(.NOT.RV) REFRY(8,I)=REFRY(8,I)
C
              IF(SNIND2.GT.0.0D0) REFRY(24,I)=1.0D0
              IF(SNIND2.LT.0.0D0) REFRY(24,I)=-1.0D0
C
              IF(GLANAM(I-1,2).EQ.'PERFECT      ') THEN
                  REFRY(7,I)=0.0D0
                  REFRY(8,I)=0.0D0
              END IF
              IF(GLANAM(I-1,2).EQ.'IDEAL        ') THEN
                  REFRY(8,I)=-(ALENS(121,I-1)-ALENS(3,I-1))*REFRY(6,I-1)
              END IF
              IF(INT(WW4).GE.1.AND.INT(WW4).LE.5)
     1        REFRY(7,I)=REFRY(8,I)*DABS(ALENS(45+INT(WW4),(I-1)))
              IF(INT(WW4).GE.6.AND.INT(WW4).LE.10)
     1        REFRY(7,I)=REFRY(8,I)*DABS(ALENS(65+INT(WW4),(I-1)))
              IF(.NOT.RV) REFRY(7,I)=REFRY(7,I)+PHASE
              IF(RV) REFRY(7,I)=REFRY(7,I)-PHASE
C
              REFRY(9,I)=COSI
              REFRY(10,I)=COSIP
              IF(L.EQ.0.0D0) THEN
                  IF(N.GE.0.0D0) REFRY(11,I)=0.0D0
                  IF(N.LT.0.0D0) REFRY(11,I)=PII
              ELSE
                  IF(DABS(L).GE.DABS(1.0D35*N)) THEN
                      IF(L.GE.0.0D0) REFRY(11,I)=PII/2.0D0
                      IF(L.LT.0.0D0) REFRY(11,I)=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(N).EQ.0.0D0) THEN
                          REFRY(11,I)=0.0D0
                      ELSE
                          REFRY(11,I)=DATAN2(L,N)
                      END IF
                      IF((REFRY(11,I)).LT.0.0D0)
     1                REFRY(11,I)=REFRY(11,I)+(TWOPII)
                  END IF
              END IF
              IF(M.EQ.0.0D0) THEN
                  IF(N.GE.0.0D0) REFRY(12,I)=0.0D0
                  IF(N.LT.0.0D0) REFRY(12,I)=PII
              ELSE
                  IF(DABS(M).GE.DABS(1.0D35*N)) THEN
                      IF(M.GE.0.0D0) REFRY(12,I)=PII/2.0D0
                      IF(M.LT.0.0D0) REFRY(12,I)=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(N).EQ.0.0D0) THEN
                          REFRY(12,I)=0.0D0
                      ELSE
                          REFRY(12,I)=DATAN2(M,N)
                      END IF
                      IF((REFRY(12,I)).LT.0.0D0)
     1                REFRY(12,I)=REFRY(12,I)+(TWOPII)
                  END IF
              END IF
              REFRY(13,I)=LN
              REFRY(14,I)=MN
              REFRY(15,I)=NN
              REFRY(16,I)=XOLD
              REFRY(17,I)=YOLD
              REFRY(18,I)=ZOLD
              REFRY(19,I)=LOLD
              REFRY(20,I)=MOLD
              REFRY(21,I)=NOLD
              REFRY(22,I)=REFRY(22,(I-1))+REFRY(7,I)
              IF(STOPP.EQ.1.AND.KKKK.NE.0.AND.KKKK.GE.NRAITR) THEN
                  GO TO 14
              ELSE
                  STOPP=0
                  REFEXT=.TRUE.
              END IF
C
C       CHECK THE RAY HEIGHT AT
C       NEWR AND ADJUST THE Y1AIM AND X1AIM AT SURFACE
C       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWR IS WITHIN
C       AIMTOL . MAXIMUN NUMBER OF ITERRATIONS IS
C       NRAITR. (DEFAULT IS 100). DEFAULT AIMTOL IS 1.0D-6.
C
              IF(I.EQ.NEWREF) THEN
C       CALCULATE TARX AND TARY
                  IF(ALENS(9,I).GE.1.0D0.AND.ALENS(9,I).LE.6.0D0.AND.
     1            ALENS(127,I).EQ.0.0D0) THEN
C       REF SURF HAS CLAP ON IT
C       SET TARGET TO CENTER OF DECENTERED CLAP,
C
C       ALENS(12,I) AND ALENS(13,I) ARE CLAP DECENTRATIONS
C       HERE IS WERE THE TARGET FOR RAY AIMING IS SET FOR THE
C       CHIEF RAY. A SIMILAR BUT MORE COMPLEX SETTING IS REQUIRED
C       FOR NON-CHIEF RAYS IN THE SUBROUTINE RAYTRA.FOR
C
                      TARY=ALENS(12,I)
                      TARX=ALENS(13,I)
                  ELSE
C       NO CLAP OF REF SURF.
                      TARX=0.0D0
                      TARY=0.0D0
                  END IF
C
                  IF(DSQRT(((TARX-X)**2)+((TARY-Y)**2)).LE.AIMTOL
     1            .OR.SYSTEM1(62).EQ.0.0D0) THEN
C       AIM IS GOOD ENOUGH, PROCEED
                      GO TO 100
                  ELSE
C       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                  END IF
C
C       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
                  X1ONE=X1LAST
                  Y1ONE=Y1LAST
C       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
                  X1LAST=XAIMOL
                  Y1LAST=YAIMOL
C       SET RXONE AND RYONE TO RXLAST AND RYLAST
                  RXONE=RXLAST
                  RYONE=RYLAST
C       SET RXLAST AND RYLAST TO X AND Y
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
                      Y1AIM=YAIMOL+DDELY
                      X1AIM=XAIMOL+DDELX
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
C       CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                      XAIMOL=XC1
                      YAIMOL=YC1
                      ZAIMOL=ZC1
                      R_TX=X1AIM
                      R_TY=Y1AIM
                      R_TZ=Z1AIM
                      CALL BAKONE
                      X1AIM=R_TX
                      Y1AIM=R_TY
                      Z1AIM=R_TZ
C       NOW THE COORDINATES ARE IN THE NEWOBJ COORD SYSTEM
                      GO TO 9
                  END IF
C       THIS IS NOT THE FIRST REFINEMENT, KKK NOT = 1
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
C
C       CALL NEWDEL
                  DELFAIL=.FALSE.
                  CALL NEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL)
                  IF(DELFAIL) THEN
                      GO TO 14
                  ELSE
                      GO TO 9
                  END IF
C       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
              END IF
C
 100          CONTINUE
C
 10       CONTINUE
C     NOW IF RYIM OR RXIM ARE IN EFFECT, ITERATE THE CHEIF RAY
C     TO HIT THE IMAGE SURFACE AT THE CORRECT POINT
          IF(SYSTEM1(98).LT.0.0D0) THEN
              CX=REFRY(1,NEWIMG)
              CY=REFRY(2,NEWIMG)
          ELSE
              CX=REFRY(11,NEWIMG)*180.0D0/PII
              CY=REFRY(12,NEWIMG)*180.0D0/PII
          END IF
          ERRORVEC=DSQRT(((CX-XCHIEF_TARGET)**2)+((CY-YCHIEF_TARGET)**2))
          IF(SYSTEM1(98).NE.0.0D0.AND.SYSTEM1(99).NE.0.0D0.AND.
     1    ERRORVEC.GT.CAIMTOL) THEN
              IF(KKKK.EQ.1) THEN
                  IF(SYSTEM1(98).LT.0.0D0) THEN
                      OLDCX=REFRY(1,NEWIMG)
                      OLDCY=REFRY(2,NEWIMG)
                  ELSE
                      OLDCX=REFRY(11,NEWIMG)*180.0D0/PII
                      OLDCY=REFRY(12,NEWIMG)*180.0D0/PII
                  END IF
C     FIRST TIME
                  OXSTRT=XSTRT
                  OYSTRT=YSTRT
                  XSTRT=XSTRT+CDDELX
                  YSTRT=YSTRT+CDDELY

C     COMPUTE THE APROPRIATE ZSTRT VALUE
                  IISURF=NEWOBJ
                  CALL SAGRET(IISURF,XSTRT,YSTRT,ZSAG,SAGERR)
                  IF(SAGERR) THEN
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
                          CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
                          CALL SHOWIT(1)
                      END IF
                      STOPP=1
                      RAYCOD(1)=16
                      RAYCOD(2)=NEWOBJ
                      REFEXT=.FALSE.
                      CALL MACFAL
                      RETURN
                  END IF
                  ZSTRT=ZSAG
                  GO TO 9898
              ELSE
C     NOT FIRST TIME
C     SET DEPENDENT VARIABLE VALUES
C
C     PREVIOUS X AND Y VALUES ON IMAGE SURFACE
                  CRXONE=OLDCX
                  CRYONE=OLDCY
C     CURRENT X AND Y VALUES ON IMAGE SURFACE
                  IF(SYSTEM1(98).EQ.-1.0D0)
     1            CRXLAST=REFRY(1,NEWIMG)
                  IF(SYSTEM1(98).EQ.1.0D0)
     1            CRXLAST=REFRY(11,NEWIMG)*180.0D0/PII
                  IF(SYSTEM1(99).EQ.-1.0D0)
     1            CRYLAST=REFRY(2,NEWIMG)
                  IF(SYSTEM1(99).EQ.1.0D0)
     1            CRYLAST=REFRY(12,NEWIMG)*180.0D0/PII
                  OLDCX=CRXLAST
                  OLDCY=CRYLAST
C     SET INDEPENDENT VARIABLE VALUES
                  CX1ONE=OXSTRT
                  CY1ONE=OYSTRT
                  CX1LAST=XSTRT
                  CY1LAST=YSTRT
                  CD11=0.0D0
                  CD12=0.0D0
                  CD21=0.0D0
                  CD22=0.0D0
                  CDET=0.0D0
                  CALL CRAYDERIV(CX1LAST,CY1LAST,CX1ONE,CY1ONE
     1            ,CRXONE,CRYONE,CRXLAST,CRYLAST,CD11,CD12,CD21,CD22)
              END IF
              IF(KKKK.GT.NRAITR) THEN
C     MAXIMUM CYCLE
C     NO REFERENCE RAY SOLUTION, REF RAY FAILURE
                  STOPP=1
                  RAYCOD(1)=8
                  RAYCOD(2)=I
                  REFEXT=.FALSE.
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
                      CALL SHOWIT(1)
C     WRITE(OUTLYNE,*)'AIMED CHIEF RAY COULD NOT HIT THE DEFINED TARGET'
                      CALL SHOWIT(1)
                      IF(.NOT.NULL) FOBYES=.FALSE.
                      IF(.NOT.NULL.AND.F34.EQ.0) CALL MACFAL
                  END IF
                  RETURN
              END IF
C     REFINE THE XSTRT,YSTRT VALUE
              CDELFAIL=.FALSE.
C       CMF1=XCHIEF_TARGET-CRXLAST, CMF2=YCHIEF_TARGET-CRYLAST
C
              CMF1=XCHIEF_TARGET-CRXLAST
              CMF2=YCHIEF_TARGET-CRYLAST
              CALL CNEWDEL(CMF1,CMF2,CD11,CD12,CD21,CD22,CDELFAIL,
     1        CDDELX,CDDELY)
              IF(CDELFAIL) THEN
                  GO TO 14
              ELSE
                  GO TO 9898
              END IF
          ELSE
C     TARGET MET!
          END IF
          RETURN
      END
