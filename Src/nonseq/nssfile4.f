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

C       FOURTH COLLECTION OF NSS FILES
C
      SUBROUTINE NSSNR4(J)
          USE NSSMOD
C
          IMPLICIT NONE
C
          LOGICAL XODD,YODD,EXISTMEM,NSSLPERROR,PERR,RAYERR
C
C       THIS IS SUBROUTINE NSSNR4.INC. THIS SUBROUTINE IMPLEMENTS
C       INTERSECTION CALCULATION TO A MEM SURFACE
C
          INTEGER I,J,K,ALLOERR,NX,NY,MEMNUM,XSTATE,YSTATE
C
          INTEGER XADD,YADD
C
          CHARACTER AMEMNUM*3,MEMFILE*10
C
          REAL*8 NSSDELX,NSSDELY
C
          REAL*8 CENTERX,CENTERY,CENTERZ,IX,IY
C
          REAL*8 XCORR,YCORR,NEWRRX,NEWRRY,NEWRRZ,XERROR,YERROR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       ARE THE MEM DIMENSIONS ODD OR EVEN
C       ODD OR EVEN
          NX=INT(NSSALENS(3,J))
          NY=INT(NSSALENS(4,J))
          XODD=.FALSE.
          IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) XODD=.TRUE.
          YODD=.FALSE.
          IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) YODD=.TRUE.
C
C       WHAT ARE THE FULL EXTENTS OF THE MEM PIXEL?
          NSSDELX=NSSALENS(6,J)
          NSSDELY=NSSALENS(7,J)
C
C       ALLOCATE THE ARRAYS THAT STORE THE MEM PIXEL CENTER LOCATIONS
C       AND INITIALIZE THEM TO ZERO
          ALLOCATE(MEMX(1:NX,1:NY),MEMY(1:NX,1:NY),STAT=ALLOERR)
          MEMX(1:NX,1:NY)=0.0D0
          MEMY(1:NX,1:NY)=0.0D0
C
C       ESTABLISH THE MEM PIXEL CENTER LOCATION VALUES
          IF(XODD) THEN
              DO I=0,NX-1
                  DO K=1,NY
                      MEMX(I+1,K)=-(NSSDELX*DBLE((NX-1)/2))+(DBLE(I)*NSSDELX)
                  END DO
              END DO
          END IF
          IF(.NOT.XODD) THEN
              DO I=1,NX
                  DO K=1,NY
                      MEMX(I,K)=-(NSSDELX*(DBLE(NX+1)/2.0D0))+(DBLE(I)*NSSDELX)
                  END DO
              END DO
          END IF
          IF(YODD) THEN
              DO I=1,NX
                  DO K=0,NY-1
                      MEMY(I,K+1)=-(NSSDELY*DBLE((NY-1)/2))+(DBLE(K)*NSSDELY)
                  END DO
              END DO
          END IF
          IF(.NOT.YODD) THEN
              DO I=1,NX
                  DO K=1,NY
                      MEMY(I,K)=-(NSSDELY*(DBLE(NY+1)/2.0D0))+(DBLE(K)*NSSDELY)
                  END DO
              END DO
          END IF
C
C       OPEN THE EXISTING MEMxxx.DAT FILE OR CREATE A NEW ONE.
C
C       MEM FILE NAME
          MEMNUM=INT(NSSALENS(5,J))
          IF(MEMNUM.LT.1.OR.MEMNUM.GT.999) THEN
              WRITE(OUTLYNE,*) 'MEM FILE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              NSS_INTERSECT=.FALSE.
              RETURN
          END IF
          CALL ITOAA(MEMNUM,AMEMNUM)
          MEMFILE=trim(HOME)//'MEM'//AMEMNUM//'.DAT'
          EXISTMEM=.FALSE.
          INQUIRE(FILE=MEMFILE,EXIST=EXISTMEM)
C
C       ALLOCATE AND INITIALIZE MEM STATE AND MEM ERROR ARRAYS
          ALLOCATE (MEMXSTATE(1:NX,1:NY),MEMYSTATE(1:NX,1:NY)
     1    ,STAT=ALLOERR)
          ALLOCATE (MEMXERROR(1:NX,1:NY),MEMYERROR(1:NX,1:NY)
     1    ,STAT=ALLOERR)
          MEMXSTATE(1:NX,1:NY)=0
          MEMYSTATE(1:NX,1:NY)=0
          MEMXERROR(1:NX,1:NY)=0.0D0
          MEMYERROR(1:NX,1:NY)=0.0D0
C
          IF(EXISTMEM) THEN
              OPEN(UNIT=82,ACCESS='SEQUENTIAL',BLANK='NULL',
     1        FORM='FORMATTED',FILE=MEMFILE,
     2        STATUS='UNKNOWN')
              DO I=1,(NX*NY)
                  READ(82,*) XADD,YADD,XSTATE,YSTATE,XERROR,YERROR
                  MEMXSTATE(XADD,YADD)=XSTATE
                  MEMYSTATE(XADD,YADD)=YSTATE
                  MEMXERROR(XADD,YADD)=XERROR
                  MEMYERROR(XADD,YADD)=YERROR
                  IF(MEMXSTATE(XADD,YADD).NE.0.AND.MEMYSTATE(XADD,YADD).NE.0) THEN
                      NSS_INTERSECT=.FALSE.
                      WRITE(OUTLYNE,*)
     1                'EITHER THE X OR THE Y MEM STATE MAY BE NON-ZERO, BUT NOT BOTH'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'RAY TRACE STOPPED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END DO
C       CLOSE AND SAVE THE MEMFILE
              CALL CLOSE_FILE(82,1)
          ELSE
C       CREATE AND LOAD FILE
              OPEN(UNIT=82,ACCESS='SEQUENTIAL',BLANK='NULL',
     1        FORM='FORMATTED',FILE=MEMFILE,
     2        STATUS='UNKNOWN')
              DO I=1,NY
                  DO K=1,NX
C       THE DEFAULT OPTICAL STATE IS REFLECT
 10                   FORMAT(I5,I5,I2,I2,D23.15,D23.15)
                      WRITE(82,10) K,I,0,0,0.0D0,0.0D0
                      MEMXSTATE(K,I)=0
                      MEMYSTATE(K,I)=0
                      MEMXERROR(K,I)=0.0D0
                      MEMYERROR(K,I)=0.0D0
                  END DO
              END DO
C       CLOSE AND SAVE THE MEMFILE
              CALL CLOSE_FILE(82,1)
          END IF
C
C       WHAT CELL IS HIT BY THE RAY WITH COORDINATES RRX,RRY
          XCORR=MEMX(1,1)
          YCORR=MEMY(1,1)
          IX=NINT(((RRX-XCORR)/NSSDELX))+1
          IY=NINT(((RRY-YCORR)/NSSDELY))+1
          IF(IX.LT.1.OR.IX.GT.NX.OR.IY.LT.1.OR.IY.GT.NY) THEN
              NSS_INTERSECT=.FALSE.
              RETURN
          END IF
C
C       IS THAT CELL TILTED
          IF(MEMXSTATE(INT(IX),INT(IY)).EQ.0.AND.
     +    MEMYSTATE(INT(IX),INT(IY)).EQ.0.AND.
     1    MEMXERROR(INT(IX),INT(IY)).EQ.0.0D0.AND.
     +    MEMYERROR(INT(IX),INT(IY)).EQ.0.0D0) THEN
C       NO TILT
              RRZ=0.0D0
              CALL MEMPOWER(IX,IY,J,PERR)
              IF(PERR) THEN
                  NSS_INTERSECT=.FALSE.
                  RETURN
              END IF
              NSSLN=0.0D0
              NSSMN=0.0D0
              NSSNN=1.0D0
              NSS_INTERSECT=.TRUE.
          ELSE
C       MEM TILTED
              NSSLN=DSIN(
     1        ((DBLE(MEMXSTATE(INT(IX),INT(IY)))*NSSALENS(201,J))
     +        +MEMXERROR(INT(IX),INT(IY)))
     2        *PII/180.0D0)
C
              NSSMN=-DSIN(
     1        ((DBLE(MEMYSTATE(INT(IX),INT(IY)))*NSSALENS(202,J))
     +        +MEMYERROR(INT(IX),INT(IY)))
     2        *PII/180.0D0)
C
              NSSNN=DSQRT(1.0D0-(NSSLN**2)-(NSSMN**2))
C
C       IN THE PLANE OF THE UNTILTED PLANE, THE RAY COORDINATES
C       THE RAY HAS INITIAL COORDINATES RRX,RRY AND RRX AND DIRECTION COSINES
C       RRL,RRM AND RRN FOR A PLANE GOING THROUGH THE CENTER OF THE IX, IY PIXEL WITH DIRECTION COSINES
C       0,0 1. COMPUTE THE INTERSECTION WITH A PLANE GOING THROUGH THE CENTER OF THE IX, IY PIXEL WITH
C       NEW TILTED DIRECTION COSINES
C       PIXEL CENTER IS AT CENTERX,CENTERY AND CENTERZ
              CENTERX=XCORR+(DBLE(IX-1)*NSSDELX)
              CENTERY=YCORR+(DBLE(IY-1)*NSSDELY)
              CENTERZ=0.0D0
              RRZ=0.0D0
              CALL LINE_TO_PLANE
     1        (NEWRRX,NEWRRY,NEWRRZ,RRX,RRY,RRZ,RRL,RRM,RRN,
     2        CENTERX,CENTERY,CENTERZ
     3        ,NSSLN,NSSMN,NSSNN,NSSLPERROR)
              IF(.NOT.NSSLPERROR) THEN
                  RRX=NEWRRX
                  RRY=NEWRRY
                  RRZ=NEWRRZ
                  PERR=.FALSE.
                  CALL MEMPOWER(NEWRRX,NEWRRY,J,PERR)
                  IF(PERR) THEN
                      NSS_INTERSECT=.FALSE.
                      RETURN
                  END IF
                  NSS_INTERSECT=.TRUE.
                  IF(NSSALENS(210,J).NE.0.0D0) THEN
C       CODE GOES HERE TO KILL RAY IF IT IS OUTSIDE THE REFLECTIVE PART OF THE MEM
C       MEM PIXEL CENTER IS AT CENTERX,CENTERY,CENTERZ
C       MEM PIXEL REFLECTING EDGES ARE AT XMEMCOR(1:4),YMEMCOR(1:4)
C       RAY IS AT RRX,RRY,RRZ
                      RAYERR=.FALSE.
                      NSS_INTERSECT=.TRUE.
                      CENTERX=XCORR+(DBLE(IX-1)*NSSDELX)
                      CENTERY=YCORR+(DBLE(IY-1)*NSSDELY)
                      XMEMCOR(1)=CENTERX-DABS(NSSALENS(208,J)/2.0D0)
                      XMEMCOR(2)=CENTERX-DABS(NSSALENS(208,J)/2.0D0)
                      XMEMCOR(3)=CENTERX+DABS(NSSALENS(208,J)/2.0D0)
                      XMEMCOR(4)=CENTERX+DABS(NSSALENS(208,J)/2.0D0)
                      YMEMCOR(1)=CENTERY+DABS(NSSALENS(209,J)/2.0D0)
                      YMEMCOR(2)=CENTERY-DABS(NSSALENS(209,J)/2.0D0)
                      YMEMCOR(3)=CENTERY-DABS(NSSALENS(209,J)/2.0D0)
                      YMEMCOR(4)=CENTERY+DABS(NSSALENS(209,J)/2.0D0)
                      CALL CHECKCLAP(RAYERR)
                      IF(RAYERR) THEN
                          NSS_INTERSECT=.FALSE.
                      END IF
                  END IF
                  RETURN
              ELSE
                  NSS_INTERSECT=.FALSE.
                  RETURN
              END IF
          END IF
          RETURN
      END
      SUBROUTINE MEMPOWER(X,Y,J,PERR)
C       THIS APPLIES A SURFACE SLOPE ERROR TO THE MEM PIXEL WITH FUNCTIONAL FORM
C       R**2, R**4 R**6 R**8 AND R**10 BUT ASSUMES THE SURFACE DEFLECTION IS SMALL AND CAN BE IGNORED
C       IN ORDER TO KEEP RAY TRACING SPEED FAST. ADDED 4/22/2004
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 X,Y,A2,A4,A6,A8,A10,LN,MN,NX,NY,SGN,Q
          INTEGER J
          LOGICAL PERR
          INCLUDE 'datmai.inc'
C       COMPUTE NEW SURFACE SLOPE BASED ON MEMX AND MEMY MEASURED FROM THE CENTER
C       OF THE MEM
C       F(X,Y,Z)=A2*(X**2+Y**2)
C       +A4*(X**2+Y**2)**2
C       +A6*(X**2+Y**2)**3
C       +A8*(X**2+Y**2)**4
C       +A10*(X**2+Y**2)**5
C
C       SURFACE X DERIVATIVE FUNCTION
          NX(X,Y,A2,A4,A6,A8,A10)=(
     1    ((2.0D0*X)*A2)
     1    +((2.0D0*(((X**2)+(Y**2))**2)*2.0D0*X)*A4)
     1    +((3.0D0*(((X**2)+(Y**2))**2)*2.0D0*X)*A6)
     1    +((4.0D0*(((X**2)+(Y**2))**3)*2.0D0*X)*A8)
     1    +((5.0D0*(((X**2)+(Y**2))**4)*2.0D0*X)*A10)
     1    )
C       SURFACE Y DERIVATIVE FUNCTION
          NY(X,Y,A2,A4,A6,A8,A10)=(
     1    ((2.0D0*Y)*A2)
     1    +((2.0D0*(((X**2)+(Y**2))**2)*2.0D0*Y)*A4)
     1    +((3.0D0*(((X**2)+(Y**2))**2)*2.0D0*Y)*A6)
     1    +((4.0D0*(((X**2)+(Y**2))**3)*2.0D0*Y)*A8)
     1    +((5.0D0*(((X**2)+(Y**2))**4)*2.0D0*Y)*A10)
     1    )
          PERR=.FALSE.
          A2=NSSALENS(203,J)
          A4=NSSALENS(204,J)
          A6=NSSALENS(205,J)
          A8=NSSALENS(206,J)
          A10=NSSALENS(207,J)
          IF(A2.NE.0.0D0.OR.A4.NE.0.0D0.OR.A6.NE.0.0D0.OR.A8.NE.0.0D0
     1    .OR.A10.NE.0.0D0) THEN
              LN=-NX(X,Y,A2,A4,A6,A8,A10)
              MN=-NY(X,Y,A2,A4,A6,A8,A10)
          ELSE
              RETURN
          END IF
          NSSLN=NSSLN+LN
          NSSMN=NSSMN+MN
          IF(NSSNN.EQ.0.0D0) THEN
              SGN=1.0D0
          ELSE
              SGN=NSSNN/DABS(NSSNN)
          END IF
          Q=1.0D0-(NSSLN**2)-(NSSMN**2)
          IF(Q.LT.0.0D0) THEN
              PERR=.TRUE.
              RETURN
          ELSE
              NSSNN=SGN*DSQRT(Q)
          END IF
          RETURN
      END

      SUBROUTINE NSSSPOTSETUP
          USE NSSMOD
          USE GLOBALS
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
C
C       CREATE A NEW SPOT DISGRAM FILE OR PREPARE AN EXISTING FILE FOR
C       ADDITIONAL INPUT
          IF(NSSSPOTTYPE.EQ.0) THEN
              NSPOT=0
C       ERASE SPOT DIAGRAM FILE
              OPEN(UNIT=100,BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'NSSSPOT.DAT'
     2        ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(100,0)
              RETURN
          END IF
          IF(NSSSPOTTYPE.EQ.1) THEN
              NSPOT=0
C       ERASE AND CREATE A NEW SPOT DIAGRAM FILE
              OPEN(UNIT=100,BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'NSSSPOT.DAT'
     2        ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(100,0)
              OPEN(UNIT=100,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'NSSSPOT.DAT'
     2        ,STATUS='UNKNOWN')
              RETURN
          END IF
          IF(NSSSPOTTYPE.EQ.2) THEN
              OPEN(UNIT=100,BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'NSSSPOT.DAT'
     2        ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(100,0)
C       OPEN EXISTING SPOT DIAGRAM FILE SPOT DIAGRAM FILE
              OPEN(UNIT=100,ACCESS='APPEND',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'NSSSPOT.DAT'
     2        ,STATUS='UNKNOWN')
              RETURN
          END IF
      END

      SUBROUTINE NSS_SAVE_LAST_RAY
          USE NSSMOD
          IMPLICIT NONE
          NSS_OGRAY(1:50)=NSS_GRAY(1:50)
          NSS_OLRAY(1:50)=NSS_LRAY(1:50)
          NSS_OGRAYP(1:50)=NSS_GRAYP(1:50)
          NSS_OLRAYP(1:50)=NSS_LRAYP(1:50)
          RETURN
      END
      SUBROUTINE NSS_RAY_LENGTH(OX,OY,OZ,NX,NY,NZ,INDEX)
          USE NSSMOD
          IMPLICIT NONE
          REAL*8 OX,OY,OZ,NX,NY,NZ,INDEX
          NSS_RAYLENGTH=
     1    DSQRT(((NX-OX)**2)
     1    +((NY-OY)**2)
     1    +((NZ-OZ)**2))
          NSS_OPL=NSS_RAYLENGTH*INDEX
          RETURN
      END
      SUBROUTINE NSSSTATS
          USE NSSMOD
          USE GLOBALS
          IMPLICIT NONE
          REAL*8 SUMLX,SUMLY,SUMLR,OX,OY,OZ,NX,NY,NZ,INDEX
          REAL*8 SUML2X,SUML2Y,SUML2R
          REAL*8 NSSOPL,NSSLEN
          INTEGER I,K,J,RNUMBER,OLDRNUMBER
          LOGICAL EXIST48,OPEN48
          INCLUDE 'datmai.inc'
          SUMLX=0.0D0
          SUMLY=0.0D0
          SUMLR=0.0D0
          SUML2X=0.0D0
          SUML2Y=0.0D0
          SUML2R=0.0D0
          NSSMEANX=0.0D0
          NSSMEANY=0.0D0
          NSSMEANR=0.0D0
          NSSRMSX=0.0D0
          NSSRMSY=0.0D0
          NSSRMSR=0.0D0
          IF(NSSSPOTTYPE.EQ.0) THEN
              WRITE(OUTLYNE,*) 'SPOT DIAGRAM FILE TYPE HAS NOT BEEN SPECIFIED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       DOES A RAY HISTORY FILE EXIST?
          INQUIRE(FILE=trim(HOME)//'NSSRHIST.DAT',EXIST=EXIST48)
          INQUIRE(FILE=trim(HOME)//'NSSRHIST.DAT',OPENED=OPEN48)
          IF(.NOT.EXIST48) THEN
              WRITE(OUTLYNE,*) 'NO RAY HISTORY FILE EXISTS TO PROCESS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'NO SPOT DIAGRAM STATISTICS POSSIBLE'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       RAY HISTORY EXISTS
              IF(.NOT.OPEN48) THEN
                  OPEN(UNIT=48,FILE=trim(HOME)//'NSSRHIST.DAT')
                  REWIND(UNIT=48)
              ELSE
C       RAY HISTORY FILE IS OPEN, REWIND IT
                  REWIND(UNIT=48)
              END IF
          END IF
C       READ THE APPROPRIATE DATA, THAT IS THE X,Y,Z,L,M,N OF ALL RAYS
C       WHICH RESIDE ON THE NSSSPOTSURF SURFACE
C       AND WRITE IT TO THE EXISTING SPOT DIAGRAM FILE
          NSPOT=0
          OLDRNUMBER=0
          DO I=1,99999
              READ(48,*,ERR=998,END=999) RNUMBER
              IF(OLDRNUMBER.NE.RNUMBER) THEN
                  NSSOPL=0.0D0
                  NSSLEN=0.0D0
                  OLDRNUMBER=RNUMBER
              END IF
              READ(48,*,ERR=998,END=999) J,NSSRAYFAILCODE(1),
     1        NSSRAYFAILCODE(2)
C       READ RAYDATA
              READ(48,*,ERR=998,END=999)
     1        (NSS_LRAY(K),K=1,17)
              READ(48,*,ERR=998,END=999)
     1        (NSS_LRAYP(K),K=1,17)
              READ(48,*,ERR=998,END=999)
     1        (NSS_GRAY(K),K=1,17)
              READ(48,*,ERR=998,END=999)
     1        (NSS_GRAYP(K),K=1,17)
C       READ RAY ENERGY AND NUMBER OF HITS
              READ(48,*,ERR=998,END=999) NSSRAY_ENERGY,NSSRAY_HITS
C       READ RAY POLARIZATION DATA
              READ(48,*,ERR=998,END=999) (NSSRAY_RPOL(K),K=1,20)
              READ(48,*,ERR=998,END=999) (NSSRAY_IPOL(K),K=1,20)
C       READ CURRENT WAVELENGTH VALUE
C       READ PREVIOUS AND CURRENT INDEX VALUES
              READ(48,*,ERR=998,END=999) CURWAVELENGTH
              READ(48,*,ERR=998,END=999) PREVINDEXR,PREVINDEXI
              READ(48,*,ERR=998,END=999) CURINDEXR,CURINDEXI
              IF(J.EQ.0) THEN
                  OX=NSS_GRAY(1)
                  OY=NSS_GRAY(2)
                  OZ=NSS_GRAY(3)
              END IF
              IF(NSSSPOTHITS.EQ.0) THEN
C       DO ALL THE RAY HITS
                  IF(J.GT.0.AND.J.LE.NSSSPOTSURF) THEN
                      NX=NSS_GRAY(1)
                      NY=NSS_GRAY(2)
                      NZ=NSS_GRAY(3)
                      INDEX=CURINDEXR
                      CALL NSS_RAY_LENGTH(OX,OY,OZ,NX,NY,NZ,INDEX)
                      OX=NX
                      OY=NY
                      OZ=NZ
                      NSSOPL=NSSOPL+NSS_OPL
                      NSSLEN=NSSLEN+NSS_RAYLENGTH
                  END IF
                  IF(J.EQ.NSSSPOTSURF) THEN
                      NSPOT=NSPOT+1
C       WRITE THE DATA TO THE SPOT DIAGRAM FILE
                      WRITE(100,*) (NSS_LRAY(K),K=1,6),NSS_LRAY(16),NSSLEN,NSSOPL
                      SUMLX=SUMLX+NSS_LRAY(1)
                      SUMLY=SUMLY+NSS_LRAY(2)
                      SUMLR=SUMLR+DSQRT((NSS_LRAY(1)**2)+(NSS_LRAY(2)**2))
                      SUML2X=SUML2X+(NSS_LRAY(1)**2)
                      SUML2Y=SUML2Y+(NSS_LRAY(2)**2)
                      SUML2R=SUML2R+((NSS_LRAY(1)**2)+(NSS_LRAY(2)**2))
                  END IF
              ELSE
C       ONLY DO RAY HIT NUMBER NSSSPOTHITS
                  IF(J.GT.0.AND.J.LE.NSSSPOTSURF.AND.NSSSPOTHITS.EQ.
     1            NSSRAY_HITS) THEN
                      NX=NSS_GRAY(1)
                      NY=NSS_GRAY(2)
                      NZ=NSS_GRAY(3)
                      INDEX=CURINDEXR
                      CALL NSS_RAY_LENGTH(OX,OY,OZ,NX,NY,NZ,INDEX)
                      OX=NX
                      OY=NY
                      OZ=NZ
                      NSSOPL=NSSOPL+NSS_OPL
                      NSSLEN=NSSLEN+NSS_RAYLENGTH
                  END IF
                  IF(J.EQ.NSSSPOTSURF.AND.NSSSPOTHITS.EQ.NSSRAY_HITS) THEN
                      NSPOT=NSPOT+1
C       WRITE THE DATA TO THE SPOT DIAGRAM FILE
                      WRITE(100,*) (NSS_LRAY(K),K=1,6),NSS_LRAY(16),NSSLEN,NSSOPL
                      SUMLX=SUMLX+NSS_LRAY(1)
                      SUMLY=SUMLY+NSS_LRAY(2)
                      SUMLR=SUMLR+DSQRT((NSS_LRAY(1)**2)+(NSS_LRAY(2)**2))
                      SUML2X=SUML2X+(NSS_LRAY(1)**2)
                      SUML2Y=SUML2Y+(NSS_LRAY(2)**2)
                      SUML2R=SUML2R+((NSS_LRAY(1)**2)+(NSS_LRAY(2)**2))
                  END IF
              END IF
          END DO
 999      CALL CLOSE_FILE(48,1)
          CALL CLOSE_FILE(100,1)
          NSSSPOTEXIST=.TRUE.

C       FINAL STATS
          IF(NSPOT.GT.0) THEN
              NSSMEANX=SUMLX/DBLE(NSPOT)
              NSSMEANY=SUMLY/DBLE(NSPOT)
              NSSMEANR=SUMLR/DBLE(NSPOT)
          ELSE
              NSSMEANX=0.0D0
              NSSMEANY=0.0D0
              NSSMEANR=0.0D0
          END IF
          IF(NSPOT.GT.1) THEN
              NSSRMSX=DSQRT((SUML2X-((SUMLX**2)/NSPOT))/(NSPOT-1.0D0))
              NSSRMSY=DSQRT((SUML2Y-((SUMLY**2)/NSPOT))/(NSPOT-1.0D0))
              NSSRMSR=DSQRT((SUML2R-((SUMLR**2)/NSPOT))/(NSPOT-1.0D0))
          ELSE
              NSSRMSX=0.0D0
              NSSRMSY=0.0D0
              NSSRMSR=0.0D0
          END IF
C       USE SHOW OR GET TO DISPLAT THE MEANS AND STANDARD DEVIATIONS
          NSSSPOTEXIST=.TRUE.
          RETURN
 998      WRITE(OUTLYNE,*) 'ERROR READING RAY HISTORY'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'NO SPOT STATISTICS POSSIBLE'
          CALL SHOWIT(1)
          CALL CLOSE_FILE(48,0)
          CALL CLOSE_FILE(100,0)
          RETURN
      END



      SUBROUTINE NSSMEMPLT(JJ)
          USE NSSMOD
          USE GLOBALS
C
          IMPLICIT NONE
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y
     1    ,ROT2Z,AX,AY,AZ,AALF,APHI,XMAXI,XMINI
     2    ,XNEW,YNEW,LKG,VIEPH,VIEAL,Z1,NSSDELX,NSSDELY
     3    ,X00,Y00,Z0,LX0,LY0,LZ0,XCEN,YCEN,XERROR,YERROR
     4    ,X1,Y1,MX0,MY0,MZ0,NX0,NY0,NZ0,ZSTEPX,ZSTEPY
C
          INTEGER J,IK,JJ,ALLOERR
C
          INTEGER IX,IY,I,IPST,K

          INTEGER IJ,IL,XADD,YADD,XSTATE,YSTATE
C
          LOGICAL XODD,YODD,EXISTMEM
C
          INTEGER COLPAS,NX,NY,MEMNUM
C
          CHARACTER AMEMNUM*3,MEMFILE*10
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          REAL*8 CLPDAT
          DIMENSION CLPDAT(:,:)
          ALLOCATABLE :: CLPDAT
C
C     LOOK.VIEW TRANSFORMS

          ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
          ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
          ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
          ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
C
          DEALLOCATE(CLPDAT,STAT=ALLOERR)
          ALLOCATE(CLPDAT(1:4,1:3),STAT=ALLOERR)
C
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C       WE NEED TO GO THROUGH THE MEM IN A DOUBLE LOOP AND DRAW THE
C       BOUNDARY OF EVERY PIXEL
C       ARE THE MEM DIMENSIONS ODD OR EVEN
C       ODD OR EVEN
          NX=INT(NSSALENS(3,JJ))
          NY=INT(NSSALENS(4,JJ))
          XODD=.FALSE.
          IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) XODD=.TRUE.
          YODD=.FALSE.
          IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) YODD=.TRUE.
C
C       WHAT ARE THE FULL EXTENTS OF THE MEM PIXEL?
          NSSDELX=NSSALENS(6,JJ)
          NSSDELY=NSSALENS(7,JJ)
C
C       ALLOCATE THE ARRAYS THAT STORE THE MEM PIXEL CENTER LOCATIONS
C       AND INITIALIZE THEM TO ZERO
          ALLOCATE(MEMX(1:NX,1:NY),MEMY(1:NX,1:NY),STAT=ALLOERR)
          MEMX(1:NX,1:NY)=0.0D0
          MEMY(1:NX,1:NY)=0.0D0
C
C       ESTABLISH THE MEM PIXEL CENTER LOCATION VALUES
          IF(XODD) THEN
              DO I=0,NX-1
                  DO K=1,NY
                      MEMX(I+1,K)=-(NSSDELX*DBLE((NX-1)/2))+(DBLE(I)*NSSDELX)
                  END DO
              END DO
          END IF
          IF(.NOT.XODD) THEN
              DO I=1,NX
                  DO K=1,NY
                      MEMX(I,K)=-(NSSDELX*(DBLE(NX+1)/2.0D0))+(DBLE(I)*NSSDELX)
                  END DO
              END DO
          END IF
          IF(YODD) THEN
              DO I=1,NX
                  DO K=0,NY-1
                      MEMY(I,K+1)=-(NSSDELY*DBLE((NY-1)/2))+(DBLE(K)*NSSDELY)
                  END DO
              END DO
          END IF
          IF(.NOT.YODD) THEN
              DO I=1,NX
                  DO K=1,NY
                      MEMY(I,K)=-(NSSDELY*(DBLE(NY+1)/2.0D0))+(DBLE(K)*NSSDELY)
                  END DO
              END DO
          END IF
C       OPEN THE EXISTING MEMxxx.DAT FILE OR CREATE A NEW ONE.
C
C       MEM FILE NAME
          MEMNUM=INT(NSSALENS(5,JJ))
          IF(MEMNUM.LT.1.OR.MEMNUM.GT.999) THEN
              WRITE(OUTLYNE,*) 'MEM FILE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              RETURN
          END IF
          CALL ITOAA(MEMNUM,AMEMNUM)
          MEMFILE=trim(HOME)//'MEM'//AMEMNUM//'.DAT'
          EXISTMEM=.FALSE.
          INQUIRE(FILE=MEMFILE,EXIST=EXISTMEM)
C
C       ALLOCATE AND INITIALIZE MEM STATE AND ERROR ARRAYS
          ALLOCATE (MEMXSTATE(1:NX,1:NY),MEMYSTATE(1:NX,1:NY)
     1    ,STAT=ALLOERR)
          MEMXSTATE(1:NX,1:NY)=0
          MEMYSTATE(1:NX,1:NY)=0
          ALLOCATE (MEMXERROR(1:NX,1:NY),MEMYERROR(1:NX,1:NY)
     1    ,STAT=ALLOERR)
          MEMXERROR(1:NX,1:NY)=0.0D0
          MEMYERROR(1:NX,1:NY)=0.0D0
C
          IF(EXISTMEM) THEN
              OPEN(UNIT=82,ACCESS='SEQUENTIAL',BLANK='NULL',
     1        FORM='FORMATTED',FILE=MEMFILE,
     2        STATUS='UNKNOWN')
              DO I=1,(NX*NY)
                  READ(82,*) XADD,YADD,XSTATE,YSTATE,XERROR,YERROR
                  MEMXSTATE(XADD,YADD)=XSTATE
                  MEMYSTATE(XADD,YADD)=YSTATE
                  MEMXERROR(XADD,YADD)=XERROR
                  MEMYERROR(XADD,YADD)=YERROR
                  IF(MEMXSTATE(XADD,YADD).NE.0.AND.MEMYSTATE(XADD,YADD).NE.0) THEN
                      WRITE(OUTLYNE,*)
     1                'EITHER THE X OR THE Y MEM STATE MAY BE NON-ZERO, BUT NOT BOTH'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'BOTH STATES ASSUMED TO BE ZERO'
                      CALL SHOWIT(1)
                      MEMXSTATE(XADD,YADD)=0
                      MEMYSTATE(XADD,YADD)=0
                      MEMXERROR(XADD,YADD)=0.0D0
                      MEMYERROR(XADD,YADD)=0.0D0
                  END IF
              END DO
          ELSE
C       CREATE AND LOAD FILE
              OPEN(UNIT=82,ACCESS='SEQUENTIAL',BLANK='NULL',
     1        FORM='FORMATTED',FILE=MEMFILE,
     2        STATUS='UNKNOWN')
              DO I=1,NY
                  DO K=1,NX
C       THE DEFAULT OPTICAL STATE IS REFLECT
 10                   FORMAT(I5,I5,I2,I2,D23.15,D23.15)
                      WRITE(82,10) K,I,0,0,0.0D0,0.0D0
                      MEMXSTATE(K,I)=0
                      MEMYSTATE(K,I)=0
                      MEMXERROR(K,I)=0.0D0
                      MEMYERROR(K,I)=0.0D0
                  END DO
              END DO
          END IF
C       CLOSE AND SAVE THE MEMFILE
          CALL CLOSE_FILE(82,1)
C       DO THE LINES DIVIDING THE MEM ELEMENTS
          DO IJ=1,NY
              DO IL=1,NX
C
                  CLPDAT(1:4,1:3)=0.0
C
C       ALL INPUT IS OK, KEEP GOING
C     THE ARRAY CONTAINING SURFACE CLAP DATA IS:
C     CLPDAT(1:4,1:3)
C
C     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
C     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
C     THE THIRD IS THE SURFACE NUMBER
C
C     WE NEED TO LOAD THE ARRAY BEFORE PLOTTING
C
C     THE PROCEDURE IS:
C
C     CYCLE THROUGH ALL THE SURFACES
C
C     1. WE WILL CLOCK AROUND THE CLEAR APERTURE FROM THE LOCAL +X
C     TOWARD THE LOCAL +Y AXIS,
C     4.0 DEGREE INCREMENTS AS MEASURED
C     BY AN OBSERVER AT THE SURFACE VERTEX, IN THE LOCAL COORDINATE
C     SYSTEM OF THE SURFACE, WITH THE OBSERVER FACING THE -Z AXIS
C     DIRECTION
C
C     WRITE DOWN THE LOCAL X,Y,Z COORDINATES OF THE CORNERS OF A PIXEL
C     IN THE LOCAL COORDINATES OF THE MEM START AT MIN X, MIN Y AS BOTTOM LEFT
C     LOOKING TOWARD THE LOCAL +Z DIRECTION AND GO COUNTER CLOCKWISE
C       THE CENTER OF THE EXISTING MEM PIXEL IS
                  XCEN=MEMX(IJ,IL)
                  YCEN=MEMY(IJ,IL)
                  CLPDAT(1,1)=XCEN-(NSSDELX/2.0D0)
                  CLPDAT(1,2)=YCEN-(NSSDELY/2.0D0)
                  CLPDAT(2,1)=XCEN+(NSSDELX/2.0D0)
                  CLPDAT(2,2)=YCEN-(NSSDELY/2.0D0)
                  CLPDAT(3,1)=XCEN+(NSSDELX/2.0D0)
                  CLPDAT(3,2)=YCEN+(NSSDELY/2.0D0)
                  CLPDAT(4,1)=XCEN-(NSSDELX/2.0D0)
                  CLPDAT(4,2)=YCEN+(NSSDELY/2.0D0)
                  ZSTEPX=-DTAN(
     1              ((DBLE(MEMXSTATE(IL,IJ))*NSSALENS(201,JJ))+MEMXERROR(IL,IJ))
     2              *PII/180.0D0)*
     3              (NSSDELX/2.0D0)
                  ZSTEPY=DTAN(
     1              ((DBLE(MEMYSTATE(IL,IJ))*NSSALENS(202,JJ))+MEMYERROR(IL,IJ))
     2              *PII/180.0D0)*
     3              (NSSDELY/2.0D0)
                  IF(MEMXSTATE(IL,IJ).NE.0) THEN
C       XSTATE SET
                      CLPDAT(1,3)=ZSTEPX
                      CLPDAT(2,3)=-ZSTEPX
                      CLPDAT(3,3)=-ZSTEPX
                      CLPDAT(4,3)=ZSTEPX
                  END IF
                  IF(MEMYSTATE(IL,IJ).NE.0) THEN
C       XSTATE SET
                      CLPDAT(1,3)=-ZSTEPY
                      CLPDAT(2,3)=-ZSTEPY
                      CLPDAT(3,3)=ZSTEPY
                      CLPDAT(4,3)=ZSTEPY
                  END IF
C
C     3. THE ARRAYS NOW HAVE LOCAL X,Y AND Z VALUES STORED IN THEM
C     CONVERT THE LOCAL X ANY Y CLAPS TO GLOBAL NUMBERS
C     GLOBAL VERTEX DATA IS
                  DO I=1,4
                      X00=NSSVERTEX(1,JJ)
                      Y00=NSSVERTEX(2,JJ)
                      Z0 =NSSVERTEX(3,JJ)
                      LX0=NSSVERTEX(4,JJ)
                      MX0=NSSVERTEX(5,JJ)
                      NX0=NSSVERTEX(6,JJ)
                      LY0=NSSVERTEX(7,JJ)
                      MY0=NSSVERTEX(8,JJ)
                      NY0=NSSVERTEX(9,JJ)
                      LZ0=NSSVERTEX(10,JJ)
                      MZ0=NSSVERTEX(11,JJ)
                      NZ0=NSSVERTEX(12,JJ)
                      X=CLPDAT(I,1)
                      Y=CLPDAT(I,2)
                      Z=CLPDAT(I,3)
C
                      X1=X00+((LX0*(X))+(LY0*(Y))
     1                +(LZ0*(Z)))
                      Y1=Y00+((MX0*(X))+(MY0*(Y))
     1                +(MZ0*(Z)))
                      Z1=Z0+((NX0*(X))+(NY0*(Y))
     1                +(NZ0*(Z)))
                      CLPDAT(I,1)=X1
                      CLPDAT(I,2)=Y1
                      CLPDAT(I,3)=Z1
                  END DO
C
C     4. NOW DETERMINE THE BEST PLACE FOR THE ROTATION POINT FOR
C               PLOT LOOK/VIEW
C
                  CALL NSSROT
C
C     5.  CONVERT THE GLOBAL X AND Y CLAP VALUES
C               USING THE LOOK/VIEW VALUES
                  DO I=1,4
                      X=CLPDAT(I,1)
                      Y=CLPDAT(I,2)
                      Z=CLPDAT(I,3)
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
                      CLPDAT(I,1)=XN
                      CLPDAT(I,2)=YN
                      CLPDAT(I,3)=ZN
                  END DO
C
C     THE ARRAYS NOW HAVE GLOBAL SURFACE CLAP DATA IN THEM
C
C     6.IF NEEDED, DETERMINE SCALE FACTORS AND PLOT RANGE
C
                  CALL NSSPLTSC
C
C     7.CYCLE THROUGH THE THE ARRAYS, APPLY SCALE FACTORS
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
                  DO I=1,4
                      CLPDAT(I,1)=(CLPDAT(I,1)/NSSSCFA)*1000.0D0
                      CLPDAT(I,2)=(CLPDAT(I,2)/NSSSCFA)*1000.0D0
                  END DO
C
C     8. APPLY THE XSHIFT AND YSHIFT VALUES
                  DO I=1,4
                      IF(LORIENT) CALL ORSHIFT
                      CLPDAT(I,1)=CLPDAT(I,1)+DBLE(PXSHFT)
                      CLPDAT(I,2)=CLPDAT(I,2)+3500.0D0+DBLE(PYSHFT)
                  END DO
C
C     9. SET THE PLOT JUSTIFICATION IF NEEDED
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
C     NOW
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
                  DO I=1,4
                      CLPDAT(I,1)=CLPDAT(I,1)+JUSOFF
                  END DO
C     9. PLOT GAMMA
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
                  DO I=1,4
                      CLPDAT(I,1)=CLPDAT(I,1)-5000.0D0
                      CLPDAT(I,2)=CLPDAT(I,2)-3500.0D0
                  END DO
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

                  IF(DBLE(PGAMMA).NE.0.0D0) THEN
                      LKG=(PII/180.0D0)*DBLE(PGAMMA)

                      DO I=1,4
                          X=CLPDAT(I,1)
                          Y=CLPDAT(I,2)
                          XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                          YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                          CLPDAT(I,1)=XNEW
                          CLPDAT(I,2)=YNEW
                      END DO
                  ELSE
                  END IF
C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
                  DO I=1,4
                      CLPDAT(I,1)=CLPDAT(I,1)+5000.0D0
                      CLPDAT(I,2)=CLPDAT(I,2)+3500.0D0
                  END DO
C
C     NOW DRAW THE CLAP
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                  DO J=1,4
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
                      IF(J.EQ.1) IPST=0
                      IF(J.NE.1) IPST=1
                      IF(CLPDAT(J,1).GT.1.0D6) CLPDAT(J,1)=1.0D6
                      IF(CLPDAT(J,2).GT.1.0D6) CLPDAT(J,2)=1.0D6
                      IF(CLPDAT(J,1).LT.-1.0D6) CLPDAT(J,1)=-1.0D6
                      IF(CLPDAT(J,2).LT.-1.0D6) CLPDAT(J,2)=-1.0D6
                      IX=INT(CLPDAT(J,1))
                      IY=INT(CLPDAT(J,2))
                      P1ARAY(J,1,1)=IX
                      P1ARAY(J,2,1)=IY
                      P1ARAY(J,3,1)=IPST
                      IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                  END DO
C     FINISHED WITH THAT CLAP, LIFT PEN
                  IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
C     LINE TYPE SETTING
                  COLPAS=COLCLP
                  CALL MY_COLTYP(COLPAS)
                  OLLNTP=LNTYPE
                  LNTYPE=0
                  FIXUP=.FALSE.
                  DO IK=1,5
                      IF(IK.LE.4) THEN
                          CALL PENMV2(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
                      END IF
                      IF(IK.EQ.5) THEN
                          CALL PENMV2(P1ARAY(1,1,1),P1ARAY(1,2,1),1)
                      END IF
                  END DO
C
                  LNTYPE=OLLNTP
C     NEST MEM PIXEL
              END DO
          END DO
C       DO THE MEM SUBPIXELS
          DO IJ=1,NY
              DO IL=1,NX
C
                  CLPDAT(1:4,1:3)=0.0
C
C       ALL INPUT IS OK, KEEP GOING
C     THE ARRAY CONTAINING SURFACE CLAP DATA IS:
C     CLPDAT(1:4,1:3)
C
C     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
C     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
C     THE THIRD IS THE SURFACE NUMBER
C
C     WE NEED TO LOAD THE ARRAY BEFORE PLOTTING
C
C     THE PROCEDURE IS:
C
C     CYCLE THROUGH ALL THE SURFACES
C
C     1. WE WILL CLOCK AROUND THE CLEAR APERTURE FROM THE LOCAL +X
C     TOWARD THE LOCAL +Y AXIS,
C     4.0 DEGREE INCREMENTS AS MEASURED
C     BY AN OBSERVER AT THE SURFACE VERTEX, IN THE LOCAL COORDINATE
C     SYSTEM OF THE SURFACE, WITH THE OBSERVER FACING THE -Z AXIS
C     DIRECTION
C
C     WRITE DOWN THE LOCAL X,Y,Z COORDINATES OF THE CORNERS OF A PIXEL
C     IN THE LOCAL COORDINATES OF THE MEM START AT MIN X, MIN Y AS BOTTOM LEFT
C     LOOKING TOWARD THE LOCAL +Z DIRECTION AND GO COUNTER CLOCKWISE
C       THE CENTER OF THE EXISTING MEM PIXEL IS
                  XCEN=MEMX(IJ,IL)
                  YCEN=MEMY(IJ,IL)
                  NSSDELX=NSSALENS(208,JJ)
                  NSSDELY=NSSALENS(209,JJ)
                  CLPDAT(1,1)=XCEN-(NSSDELX/2.0D0)
                  CLPDAT(1,2)=YCEN-(NSSDELY/2.0D0)
                  CLPDAT(2,1)=XCEN+(NSSDELX/2.0D0)
                  CLPDAT(2,2)=YCEN-(NSSDELY/2.0D0)
                  CLPDAT(3,1)=XCEN+(NSSDELX/2.0D0)
                  CLPDAT(3,2)=YCEN+(NSSDELY/2.0D0)
                  CLPDAT(4,1)=XCEN-(NSSDELX/2.0D0)
                  CLPDAT(4,2)=YCEN+(NSSDELY/2.0D0)
                  ZSTEPX=-DTAN(
     1              ((DBLE(MEMXSTATE(IL,IJ))*NSSALENS(201,JJ))+MEMXERROR(IL,IJ))
     2              *PII/180.0D0)*
     3              (NSSDELX/2.0D0)
                  ZSTEPY=DTAN(
     1              ((DBLE(MEMYSTATE(IL,IJ))*NSSALENS(202,JJ))+MEMYERROR(IL,IJ))
     2              *PII/180.0D0)*
     3              (NSSDELY/2.0D0)
                  IF(MEMXSTATE(IL,IJ).NE.0) THEN
C       XSTATE SET
                      CLPDAT(1,3)=ZSTEPX
                      CLPDAT(2,3)=-ZSTEPX
                      CLPDAT(3,3)=-ZSTEPX
                      CLPDAT(4,3)=ZSTEPX
                  END IF
                  IF(MEMYSTATE(IL,IJ).NE.0) THEN
C       XSTATE SET
                      CLPDAT(1,3)=-ZSTEPY
                      CLPDAT(2,3)=-ZSTEPY
                      CLPDAT(3,3)=ZSTEPY
                      CLPDAT(4,3)=ZSTEPY
                  END IF
C
C     3. THE ARRAYS NOW HAVE LOCAL X,Y AND Z VALUES STORED IN THEM
C     CONVERT THE LOCAL X ANY Y CLAPS TO GLOBAL NUMBERS
C     GLOBAL VERTEX DATA IS
                  DO I=1,4
                      X00=NSSVERTEX(1,JJ)
                      Y00=NSSVERTEX(2,JJ)
                      Z0 =NSSVERTEX(3,JJ)
                      LX0=NSSVERTEX(4,JJ)
                      MX0=NSSVERTEX(5,JJ)
                      NX0=NSSVERTEX(6,JJ)
                      LY0=NSSVERTEX(7,JJ)
                      MY0=NSSVERTEX(8,JJ)
                      NY0=NSSVERTEX(9,JJ)
                      LZ0=NSSVERTEX(10,JJ)
                      MZ0=NSSVERTEX(11,JJ)
                      NZ0=NSSVERTEX(12,JJ)
                      X=CLPDAT(I,1)
                      Y=CLPDAT(I,2)
                      Z=CLPDAT(I,3)
C
                      X1=X00+((LX0*(X))+(LY0*(Y))
     1                +(LZ0*(Z)))
                      Y1=Y00+((MX0*(X))+(MY0*(Y))
     1                +(MZ0*(Z)))
                      Z1=Z0+((NX0*(X))+(NY0*(Y))
     1                +(NZ0*(Z)))
                      CLPDAT(I,1)=X1
                      CLPDAT(I,2)=Y1
                      CLPDAT(I,3)=Z1
                  END DO
C
C     4. NOW DETERMINE THE BEST PLACE FOR THE ROTATION POINT FOR
C               PLOT LOOK/VIEW
C
                  CALL NSSROT
C
C     5.  CONVERT THE GLOBAL X AND Y CLAP VALUES
C               USING THE LOOK/VIEW VALUES
                  DO I=1,4
                      X=CLPDAT(I,1)
                      Y=CLPDAT(I,2)
                      Z=CLPDAT(I,3)
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
                      CLPDAT(I,1)=XN
                      CLPDAT(I,2)=YN
                      CLPDAT(I,3)=ZN
                  END DO
C
C     THE ARRAYS NOW HAVE GLOBAL SURFACE CLAP DATA IN THEM
C
C     6.IF NEEDED, DETERMINE SCALE FACTORS AND PLOT RANGE
C
                  CALL NSSPLTSC
C
C     7.CYCLE THROUGH THE THE ARRAYS, APPLY SCALE FACTORS
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
                  DO I=1,4
                      CLPDAT(I,1)=(CLPDAT(I,1)/NSSSCFA)*1000.0D0
                      CLPDAT(I,2)=(CLPDAT(I,2)/NSSSCFA)*1000.0D0
                  END DO
C
C     8. APPLY THE XSHIFT AND YSHIFT VALUES
                  DO I=1,4
                      IF(LORIENT) CALL ORSHIFT
                      CLPDAT(I,1)=CLPDAT(I,1)+DBLE(PXSHFT)
                      CLPDAT(I,2)=CLPDAT(I,2)+3500.0D0+DBLE(PYSHFT)
                  END DO
C
C     9. SET THE PLOT JUSTIFICATION IF NEEDED
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
C     NOW
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
                  DO I=1,4
                      CLPDAT(I,1)=CLPDAT(I,1)+JUSOFF
                  END DO
C     9. PLOT GAMMA
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
                  DO I=1,4
                      CLPDAT(I,1)=CLPDAT(I,1)-5000.0D0
                      CLPDAT(I,2)=CLPDAT(I,2)-3500.0D0
                  END DO
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

                  IF(DBLE(PGAMMA).NE.0.0D0) THEN
                      LKG=(PII/180.0D0)*DBLE(PGAMMA)

                      DO I=1,4
                          X=CLPDAT(I,1)
                          Y=CLPDAT(I,2)
                          XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                          YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                          CLPDAT(I,1)=XNEW
                          CLPDAT(I,2)=YNEW
                      END DO
                  ELSE
                  END IF
C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
                  DO I=1,4
                      CLPDAT(I,1)=CLPDAT(I,1)+5000.0D0
                      CLPDAT(I,2)=CLPDAT(I,2)+3500.0D0
                  END DO
C
C     NOW DRAW THE CLAP
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                  DO J=1,4
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
                      IF(J.EQ.1) IPST=0
                      IF(J.NE.1) IPST=1
                      IF(CLPDAT(J,1).GT.1.0D6) CLPDAT(J,1)=1.0D6
                      IF(CLPDAT(J,2).GT.1.0D6) CLPDAT(J,2)=1.0D6
                      IF(CLPDAT(J,1).LT.-1.0D6) CLPDAT(J,1)=-1.0D6
                      IF(CLPDAT(J,2).LT.-1.0D6) CLPDAT(J,2)=-1.0D6
                      IX=INT(CLPDAT(J,1))
                      IY=INT(CLPDAT(J,2))
                      P1ARAY(J,1,1)=IX
                      P1ARAY(J,2,1)=IY
                      P1ARAY(J,3,1)=IPST
                      IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                  END DO
C     FINISHED WITH THAT CLAP, LIFT PEN
                  IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
C     LINE TYPE SETTING
                  COLPAS=COLCLP
                  CALL MY_COLTYP(COLPAS)
                  OLLNTP=LNTYPE
                  LNTYPE=0
                  FIXUP=.FALSE.
                  DO IK=1,5
                      IF(IK.LE.4) THEN
                          CALL PENMV2(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
                      END IF
                      IF(IK.EQ.5) THEN
                          CALL PENMV2(P1ARAY(1,1,1),P1ARAY(1,2,1),1)

                      END IF
                  END DO
C
                  LNTYPE=OLLNTP
C     NEST MEM PIXEL
              END DO
          END DO
C
          DEALLOCATE(CLPDAT,STAT=ALLOERR)
          RETURN
      END


      SUBROUTINE CHECKCLAP(RAYERR)
          USE NSSMOD
          IMPLICIT NONE
          LOGICAL RAYERR,INSID1,INS
!      REAL*8 CENTERY
          EXTERNAL INSID1
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INS=.TRUE.
          XT(1:4)=XMEMCOR(1:4)
          YT(1:4)=YMEMCOR(1:4)
          NP=4
          X0=RRX
          Y0=RRY
          IF(X0.LE.XT(1).OR.X0.LE.XT(2).OR.
     1    X0.GE.XT(3).OR.X0.GE.XT(4).OR.
     1    Y0.LE.YT(2).OR.Y0.LE.YT(3).OR.
     1    Y0.GE.XT(1).OR.Y0.GE.YT(4)) INS=.FALSE.
          IF(.NOT.INS) RAYERR=.TRUE.
          RETURN
      END
