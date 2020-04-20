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

      SUBROUTINE DXFNSSSFS
          USE NSSMOD
          INCLUDE 'datmai.inc'
          INTEGER I
          REAL*8 SFI
          IF(.NOT.NEXISTN) THEN
              WRITE(OUTLYNE,*) 'NO NSS DATABASE EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'NO DXF OUTPUT POSSIBLE'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          CALL NSS_VERTEX_SILENT
C       NOW VERTEX DATA EXISTS IN GLOBAL COORDINATES
          DO I=1,MAXS
C       PLOT THE CLAP FOR ACTIVE SURFACES ONLY
              IF(NSSALENS(19,I).NE.0.0D0) THEN
                  SFI=1.0D0
C        IF(NSSALENS(1,I).EQ.1.0D0.OR.NSSALENS(1,I).EQ.2.0D0.OR.
C     1  NSSALENS(1,I).EQ.3.0D0.OR.NSSALENS(1,I).EQ.4.0D0)
C     1  CALL DXFNSSCLP(I,SFI)
              END IF
          END DO
          DO I=1,MAXS
C       PLOT THE HOLES FOR ACTIVE SURFACES ONLY
              IF(NSSALENS(25,I).NE.0.0D0) THEN
C        IF(NSSALENS(1,I).EQ.1.0D0.OR.NSSALENS(1,I).EQ.2.0D0.OR.
C     1  NSSALENS(1,I).EQ.3.0D0.OR.NSSALENS(1,I).EQ.4.0D0)
C     1  CALL DXFNSSHOL(I)
              END IF
          END DO
          DO I=1,MAXS
C       PLOT THE PROFX FOR ACTIVE SURFACES ONLY
              IF(NSSALENS(19,I).NE.0.0D0) THEN
C        IF(NSSALENS(1,I).EQ.1.0D0.OR.NSSALENS(1,I).EQ.2.0D0.OR.
C     1  NSSALENS(1,I).EQ.3.0D0.OR.NSSALENS(1,I).EQ.4.0D0)
C     1  CALL DXFNSSPRO(I,0.0D0)
              END IF
          END DO
          DO I=1,MAXS
C       PLOT THE PROFY FOR ACTIVE SURFACES ONLY
              IF(NSSALENS(19,I).NE.0.0D0) THEN
C        IF(NSSALENS(1,I).EQ.1.0D0.OR.NSSALENS(1,I).EQ.2.0D0.OR.
C     1  NSSALENS(1,I).EQ.3.0D0.OR.NSSALENS(1,I).EQ.4.0D0)
C     1  CALL DXFNSSPRO(I,90.0D0)
              END IF
          END DO
          DO I=1,MAXS
C       PLOT THE INDIVIDUAL CLAPS OF ALL MEM PIXELS FOR A MEM SURFACE
              IF(NSSALENS(1,I).EQ.5.0D0) THEN
C        CALL DXFNSSMEMPLT(I)
              END IF
          END DO
          DO I=1,MAXS
C       PLOT THE TUBE CLAPS
              IF(NSSALENS(1,I).EQ.6.0D0) THEN
C        CALL DXFNSSCLPTUB(I)
              END IF
          END DO
          DO I=1,MAXS
C       PLOT THE TUBE PROFILES
              IF(NSSALENS(1,I).EQ.6.0D0) THEN
C         CALL DXFNSSPROTUB(I,0.0D0)
C         CALL DXFNSSPROTUB(I,45.0D0)
C         CALL DXFNSSPROTUB(I,90.0D0)
C         CALL DXFNSSPROTUB(I,135.0D0)
C         CALL DXFNSSPROTUB(I,180.0D0)
C         CALL DXFNSSPROTUB(I,225.0D0)
C         CALL DXFNSSPROTUB(I,270.0D0)
C         CALL DXFNSSPROTUB(I,315.0D0)
              END IF
          END DO
          RETURN
      END



      SUBROUTINE DXFNSSRY
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT RAY COMMAND AT THE CMD LEVEL
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,XMAXI,XMINI
     2    ,XNEW,YNEW,LKG,VIEPH,VIEAL,L,M,N
C
          INTEGER CN
C
!      LOGICAL GGO
C
          INTEGER IX,IY,I,II,IPST,COLPAS
          INTEGER NSSP1ARRAY,ALLOERR,ICOUNT
          DIMENSION NSSP1ARRAY(:,:)
          ALLOCATABLE :: NSSP1ARRAY
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C     LOOK.VIEW TRANSFORMS

          ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
          ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
          ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
          ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
          IF(.NOT.NEXISTN) THEN
              WRITE(OUTLYNE,*) 'NO NSS DATABASE EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'NO NSS RAY CAN BE DRAWN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          CALL NSS_VERTEX_SILENT
C
          DEALLOCATE(NSSP1ARRAY,STAT=ALLOERR)
          X=0.0D0
          Y=0.0D0
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C       NOW WE HAVE THE REAL WORLD COORDINATES OF THE RAY TO BE PLOTTED
C       THEY ARE STORED IN THE FILE NSSRHIST.DAT
C
C     HERE IS WHERE REAL WORK BEGINS. FIRST PROJECT THE 3D WORLD COORDINATES
C     OF THE VERTICIES ONTO THE 2D PLANE INDICATED BY THE PLOT LOOK/ PLOT VIEW
C     DATA. THE DIRECTION COSINES OF THE LOOK VECTOR ARE LOOKX, LOOKY AND
C     LOOKZ. THE TRANSFORMATIONS ARE:
C
          CALL NSSROT
C
C     AFTER THIS TRANSFORMATION, THE WORLD X COORDINATE IS PLOTTED IN THE
C     HORIZONTAL X-AXIS OF THE DISPLAY AND THE Y WORLD COORDINATE IS PLOTTED
C     IN THE VERTICAL Y-AXIS OF THE DISPLAY AFTER A CONVERSION TO
C     DEVICE INDEPENDENT COORDINATES ARE MADE.
C
          CALL NSSPLTSC
C
          ICOUNT=0
          OPEN(UNIT=92,FILE=trim(HOME)//'NSSHT.DAT',STATUS='UNKNOWN')
          REWIND(UNIT=92)
 5        CONTINUE
          CN=NSSWAVNUM
          READ(UNIT=92,FMT=*,END=6) I,X,Y,Z,L,M,N,CN
          ICOUNT=ICOUNT+1
          GO TO 5
 6        CONTINUE
          CALL CLOSE_FILE(92,1)
          IF(ICOUNT.EQ.0) THEN
              WRITE(OUTLYNE,*) 'NO RAYS EXIST TO BE PLOTTED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          ALLOCATE(NSSP1ARRAY(1:ICOUNT,1:3),STAT=ALLOERR)
          OPEN(UNIT=92,FILE=trim(HOME)//'NSSHT.DAT',STATUS='UNKNOWN')
C     HERE IS THE LOOP WHICH PLOTS THE RAYS USING CALLS TO
C     PENMV2(IX,IY,IPST)
          REWIND(UNIT=92)
          DO II=1,ICOUNT
              READ(UNIT=92,FMT=*) I,X,Y,Z,L,M,N,CN
              X=X-XROT
              Y=Y-YROT
              Z=Z-ZROT
              XN=ROT1X(X,Z,VIEPH)
              YN=Y
              ZN=ROT1Z(X,Z,VIEPH)
              X=XN
              Y=YN
              Z=ZN
C
              ZN=ROT2Z(Z,Y,VIEAL)
              YN=ROT2Y(Z,Y,VIEAL)
              XN=X
              X=XN
              Y=YN
              Z=ZN
              XMAXI=XN                  ! Add by ENDO
              XMINI=XN                  ! Add by ENDO
C
C     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
C     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
C     TWO STEPS.
C
C     THE WORLD X PLOTS TO THE PLOTTER X
C     THE WORLD Y PLOTS TO THE PLOTTER Y
C
C     STEP 1: CONVERT USING AN APPROPRIATE SCALE FACTOR
C               CALCULATING AN APPROPRIATE FACTOR IF NECESSARY
C
              Y=(Y/NSSSCFA)*1000.0D0
C
              X=(X/NSSSCFA)*1000.0D0
C
C     STEP 2: APPLY THE SCREEN Y-OFFSET AND SCREEN GAMMA ROTATION
C
C     APPLY THE PLOT LOOK/VIEW Y-OFFSET VALUE
C
              IF(LORIENT) CALL ORSHIFT
              Y=Y+3500.0D0+DBLE(PYSHFT)
              X=X+DBLE(PXSHFT)
C
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
              IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
                  JUSOFF=500.0D0-((XMINI/NSSSCFA)*1000.0D0)
                  RCL=-1
              ELSE
              END IF
              IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
                  RCL=-2
                  JUSOFF=5000.0D0
              ELSE
              END IF
              IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
                  JUSOFF=9500.0D0-((XMAXI/NSSSCFA)*1000.0D0)
                  RCL=-3
              ELSE
              END IF
C
              X=X+JUSOFF
C
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
              X=X-5000.0D0
              Y=Y-3500.0D0
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

              IF(DBLE(PGAMMA).NE.0.0D0) THEN
                  LKG=(PII/180.0D0)*DBLE(PGAMMA)
                  XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                  YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                  X=XNEW
                  Y=YNEW
              ELSE
              END IF

C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
              X=X+5000.0D0
              Y=Y+3500.0D0
C
C     PUT INSTRUCTIONS IN NSSP1ARRAY TO DROP PEN AND DRAW
              IPST=1
              IF(X.LT.-2.0D9) X=-2.0D9
              IF(Y.LT.-2.0D9) Y=-2.0D9
              IF(X.GT.2.0D9) X=2.0D9
              IF(Y.GT.2.0D9) Y=2.0D9
              IX=INT(X)
              IY=INT(Y)
C     MOVE TO NEW POSITION WITH LOWERED PEN
              NSSP1ARRAY(II,1)=IX
              NSSP1ARRAY(II,2)=IY
              IF(I.EQ.0) THEN
                  NSSP1ARRAY(II,3)=0
              ELSE
                  NSSP1ARRAY(II,3)=1
              END IF
              IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              CONTINUE
          END DO
          CALL CLOSE_FILE(92,1)
C
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARRAY ARRAY
C
          COLPAS=COLRAY
          CALL MY_COLTYP(COLPAS)
          NSSP1ARRAY(1,3)=0
          FIXUP=.FALSE.
          DO I=1,ICOUNT
              CALL PENMV2(NSSP1ARRAY(I,1),NSSP1ARRAY(I,2),NSSP1ARRAY(I,3))
          END DO
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
          DEALLOCATE(NSSP1ARRAY,STAT=ALLOERR)
          RETURN
      END
