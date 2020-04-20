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

C SUB DEGRAD.FOR
      SUBROUTINE DEGRAD
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO SET RAYTRACE MODE TO
C       DEGRRES, RADIANS OR TANGENTS
C
          LOGICAL RAD,DEG,TANG
C
          COMMON/ANGMOD/DEG,RAD,TANG
C
          INCLUDE 'datmai.inc'
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"DEG","RAD","TANGENT" AND "ANGMODE" COMMANDS'
              CALL SHOWIT(1)
              OUTLYNE=
     1          'TAKE NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'RAD') THEN
              RAD=.TRUE.
              DEG=.FALSE.
              TANG=.FALSE.
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'DEG') THEN
              DEG=.TRUE.
              RAD=.FALSE.
              TANG=.FALSE.
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'TANGENT') THEN
              TANG=.TRUE.
              DEG=.FALSE.
              RAD=.FALSE.
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'ANGMODE') THEN
              IF(DEG) THEN
                  OUTLYNE='RAYTRACE ANGULAR MODE SET TO "DEGREES"'
                  CALL SHOWIT(1)
                  RETURN
              ELSE
              END IF
              IF(RAD) THEN
                  OUTLYNE='RAYTRACE ANGULAR MODE SET TO "RADIANS"'
                  CALL SHOWIT(1)
                  RETURN
              ELSE
              END IF
              IF(TANG) THEN
                  OUTLYNE='RAYTRACE ANGULAR MODE SET TO "TANGENT"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB FOPD.FOR
      SUBROUTINE FOPD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FOPD.FOR. THIS SUBROUTINE IS
C       CALLED BY FANS AND AND OTHER ROUTINES. IT CALCULATES
C       ADJUSTMENTS TO THE
C       OPD DUE TO THE OBJECT REFERENCE SPHERE.
C     OCOR IS ALWAYS ADDED WHERE AS RCOR IS ALWAYS SUBTRACTED
C
          REAL*8 X1,Y1,Z1,RX0,RY0,RZ0,RLOLD,RMOLD,
     1    RNOLD,L,M,N,T,D,X2,Y2,Z2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          RCOR=0.0D0
          OCOR=0.0D0
C
C       IS THE BEGINNING REFERENCE SPHERE A SHERE OR A FLAT?
          D=DSQRT(((REFRY(16,NEWOBJ+1)-REFRY(1,NEWOBJ+1))**2)
     1    +((REFRY(17,NEWOBJ+1)-REFRY(2,NEWOBJ+1))**2)
     1    +((REFRY(18,NEWOBJ+1)-REFRY(3,NEWOBJ+1))**2))
C
C     D IS THE MAGNITUDE OF THE REFERENCE SPHERE RADIUS
C
          IF(D.GE.1.0D10) THEN
C       SURFACE IS FLAT, CORRECTION FROM NEWOBJ TO THE PLANE IS ZERO.
C       THE REFERENCE PLANE IS PERPENDICULAR TO THE REFERENCE RAY
C       AND HAS ITS ORIGIN WHERE THE CHIEF RAY INTERSECTS THE SURFACE
C       NEWOBJ+1
C       THE EQUATION OF THIS PLANE IS:
C       (X-RX0)*RLOLD + (Y-RY0)*RMOLD +(Z-RZ0)*RNOLD = 0
C       WHERE:
              RX0=REFRY(1,NEWOBJ+1)
              RY0=REFRY(2,NEWOBJ+1)
              RZ0=REFRY(3,NEWOBJ+1)
              RLOLD=REFRY(19,NEWOBJ+1)
              RMOLD=REFRY(20,NEWOBJ+1)
              RNOLD=REFRY(21,NEWOBJ+1)
C
C       WITH THIS DEFINITION, RCOR IS ALWAYS 0.0D0
              RCOR=0.0D0
C       OCOR IS THE SIGNED LENGTH ALONG THE "OTHER" RAY
C       BETWEEN THE INTERSECTION WITH THIS PLANE AND THE INTERSECTION
C       WITH THE SURFACE NEWOBJ+1.
C       THE "OTHER" RAY HITS SURFACE NEWOBJ+1 AT
              X1=RAYRAY(1,NEWOBJ+1)
              Y1=RAYRAY(2,NEWOBJ+1)
              Z1=RAYRAY(3,NEWOBJ+1)
C       ITS DIRECTION COSINES ARE
              L=RAYRAY(19,NEWOBJ+1)
              M=RAYRAY(20,NEWOBJ+1)
              N=RAYRAY(21,NEWOBJ+1)
C
C       THE OTHER RAY CAN BE REPRESENTED AS:
C
C       X=X1+(L*T)
C       Y=Y1+(M*T)
C       Z=Y1+(N*T)
C       T IS THE DISTANCE FROM THE NEWOBJ+1 SURFACE INTERSECTION TO THE
C       PLANE INTERSECTION AND IS:
C
              T=((RLOLD*(RX0-X1))+(RMOLD*(RY0-Y1))+(RNOLD*(RZ0-Z1)))/
     1        ((L*RLOLD)+(M*RMOLD)+(N*RNOLD))
C       THE OTHER RAY INTERSECTS THE PLANE AT:
C
              X2=X1+(L*T)
              Y2=Y1+(M*T)
              Z2=Z1+(N*T)
C       THE CORRECTION TERM IS JUST:
              OCOR=DSQRT(((X2-X1)**2)+((Y2-Y1)**2)+((Z2-Z1)**2))
              IF(.NOT.REVSTR) THEN
C     FORWARD RAY
C     IF IT HITS THE PLANE BEFORE IT HITS NEWOBJ+1
                  IF(Z1.GE.Z2) OCOR=-OCOR
C     IF IT HITS THE PLANE AFTER IT HITS NEWOBJ+1
                  IF(Z1.LT.Z2) OCOR=OCOR
              ELSE
C     REVERSING RAY, FLIP SIGN
C     IF IT HITS THE PLANE BEFORE IT HITS NEWOBJ+1
                  IF(Z1.GE.Z2) OCOR=OCOR
C     IF IT HITS THE PLANE AFTER IT HITS NEWOBJ+1
                  IF(Z1.LT.Z2) OCOR=-OCOR
              END IF
              IF(REFRY(6,NEWOBJ).LT.0.0D0.AND..NOT.REVSTR.OR.
     1        REFRY(6,NEWOBJ).GT.0.0D0.AND.REVSTR) THEN
                  OCOR=-OCOR
                  RCOR=-RCOR
              END IF
              GO TO 900
          ELSE
C       NOT FLAT, RADIUS IS ROBJ AND CENTER IS AT
C       COORDINATES OF REFERENCE RAY AT NEWOBJ.
C
              RCOR=D
              OCOR=D
C     NOW FOR THE SIGN OF RCOR AND OCOR
C
              IF(.NOT.REVSTR) THEN
                  OCOR=OCOR
                  RCOR=RCOR
              ELSE
C     FLIP SIGN
                  OCOR=-OCOR
                  RCOR=-RCOR
              END IF
          END IF
          IF(REFRY(6,NEWOBJ).LT.0.0D0.AND..NOT.REVSTR.OR.
     1    REFRY(6,NEWOBJ).GT.0.0D0.AND.REVSTR) THEN
              OCOR=-OCOR
              RCOR=-RCOR
          END IF
 900      CONTINUE
          IF(DABS(RCOR).GT.1.0D10.OR.DABS(OCOR).GT.1.0D10) THEN
C     INFINITE REFERENCE SPHERE
          ELSE
          END IF
          RETURN
      END
C SUB FOPDS.FOR
      SUBROUTINE FOPDS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE FOPDS.FOR. THIS SUBROUTINE IS
C       CALLED BY SPD AND COMPAP ROUTINES. IT CALCULATES
C       ADJUSTMENTS TO THE
C       OPD DUE TO THE OBJECT REFERENCE SPHERE.
C     OCOR IS ALWAYS ADDED WHERE AS RCOR IS ALWAYS SUBTRACTED
C
          REAL*8 X1,Y1,Z1,RX0,RY0,RZ0,RLOLD,RMOLD,
     1    RNOLD,L,M,N,T,D,X2,Y2,Z2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsp1.inc'
C
          RCOR=0.0D0
          OCOR=0.0D0
C
C       IS THE BEGINNING REFERENCE SPHERE A SHERE OR A FLAT?
          D=DSQRT(((REFRY(16,NEWOBJ+1)-REFRY(1,NEWOBJ+1))**2)
     1    +((REFRY(17,NEWOBJ+1)-REFRY(2,NEWOBJ+1))**2)
     1    +((REFRY(18,NEWOBJ+1)-REFRY(3,NEWOBJ+1))**2))
C
C     D IS THE MAGNITUDE OF THE REFERENCE SPHERE RADIUS
C
          IF(D.GE.1.0D10) THEN
C       SURFACE IS FLAT, CORRECTION FROM NEWOBJ TO THE PLANE IS ZERO.
C       THE REFERENCE PLANE IS PERPENDICULAR TO THE REFERENCE RAY
C       AND HAS ITS ORIGIN WHERE THE CHIEF RAY INTERSECTS THE SURFACE
C       NEWOBJ+1
C       THE EQUATION OF THIS PLANE IS:
C       (X-RX0)*RLOLD + (Y-RY0)*RMOLD +(Z-RZ0)*RNOLD = 0
C       WHERE:
              RX0=REFRY(1,NEWOBJ+1)
              RY0=REFRY(2,NEWOBJ+1)
              RZ0=REFRY(3,NEWOBJ+1)
              RLOLD=REFRY(19,NEWOBJ+1)
              RMOLD=REFRY(20,NEWOBJ+1)
              RNOLD=REFRY(21,NEWOBJ+1)
C
C       WITH THIS DEFINITION, RCOR IS ALWAYS 0.0D0
              RCOR=0.0D0
C       OCOR IS THE SIGNED LENGTH ALONG THE "OTHER" RAY
C       BETWEEN THE INTERSECTION WITH THIS PLANE AND THE INTERSECTION
C       WITH THE SURFACE NEWOBJ+1.
C       THE "OTHER" RAY HITS SURFACE NEWOBJ+1 AT
              X1=DSPOT(14)
              Y1=DSPOT(15)
              Z1=DSPOT(18)
C       ITS DIRECTION COSINES ARE
              L=DSPOT(19)
              M=DSPOT(20)
              N=DSPOT(21)
C
C       THE OTHER RAY CAN BE REPRESENTED AS:
C
C       X=X1+(L*T)
C       Y=Y1+(M*T)
C       Z=Y1+(N*T)
C       T IS THE DISTANCE FROM THE NEWOBJ+1 SURFACE INTERSECTION TO THE
C       PLANE INTERSECTION AND IS:
C
              T=((RLOLD*(RX0-X1))+(RMOLD*(RY0-Y1))+(RNOLD*(RZ0-Z1)))/
     1        ((L*RLOLD)+(M*RMOLD)+(N*RNOLD))
C       THE OTHER RAY INTERSECTS THE PLANE AT:
C
              X2=X1+(L*T)
              Y2=Y1+(M*T)
              Z2=Z1+(N*T)
C       THE CORRECTION TERM IS JUST:
              OCOR=DSQRT(((X2-X1)**2)+((Y2-Y1)**2)+((Z2-Z1)**2))
              IF(.NOT.REVSTR) THEN
C     FORWARD RAY
C     IF IT HITS THE PLANE BEFORE IT HITS NEWOBJ+1
                  IF(Z1.GE.Z2) OCOR=-OCOR
C     IF IT HITS THE PLANE AFTER IT HITS NEWOBJ+1
                  IF(Z1.LT.Z2) OCOR=OCOR
              ELSE
C     REVERSING RAY, FLIP SIGN
C     IF IT HITS THE PLANE BEFORE IT HITS NEWOBJ+1
                  IF(Z1.GE.Z2) OCOR=OCOR
C     IF IT HITS THE PLANE AFTER IT HITS NEWOBJ+1
                  IF(Z1.LT.Z2) OCOR=-OCOR
              END IF
              IF(REFRY(6,NEWOBJ).LT.0.0D0.AND..NOT.REVSTR.OR.
     1        REFRY(6,NEWOBJ).GT.0.0D0.AND.REVSTR) THEN
                  OCOR=-OCOR
                  RCOR=-RCOR
              END IF
              GO TO 900
          ELSE
C       NOT FLAT, RADIUS IS ROBJ AND CENTER IS AT
C       COORDINATES OF REFERENCE RAY AT NEWOBJ.
C
              RCOR=D
              OCOR=D
C     NOW FOR THE SIGN OF RCOR AND OCOR
C
              IF(.NOT.REVSTR) THEN
                  OCOR=OCOR
                  RCOR=RCOR
              ELSE
C     FLIP SIGN
                  OCOR=-OCOR
                  RCOR=-RCOR
              END IF
          END IF
          IF(REFRY(6,NEWOBJ).LT.0.0D0.AND..NOT.REVSTR.OR.
     1    REFRY(6,NEWOBJ).GT.0.0D0.AND.REVSTR) THEN
              OCOR=-OCOR
              RCOR=-RCOR
          END IF
 900      CONTINUE
          IF(DABS(RCOR).GT.1.0D10.OR.DABS(OCOR).GT.1.0D10) THEN
C     INFINITE REFERENCE SPHERE
          ELSE
          END IF
          RETURN
      END
C SUB FOBDMP.FOR
      SUBROUTINE FOBDMP
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE CONTROLS THE OPERATION OF THE "FOBDUMP"
C       COMMAND
C
          CHARACTER *17 POS,REV
C
          INTEGER I
C
          REAL*8 ACLENG
C
          COMMON/ACLEN/ACLENG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)'"FOBDUMP" TAKES NO INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
C     FIRST PRINT A MESSAGE AS TO WHETHER OR NOT THE LAST RAY
C     FAILED OR NOT OR WAS NEVER TRACED.
C     IF RAYCOD(1)=0 AND REFEXT FALSE
          IF(RAYCOD(1).EQ.0.AND..NOT.REFEXT) THEN
C       NO FOBRAY WAS TRACED, NO DATA EXISTS TO DUMP
              WRITE(OUTLYNE,5)
     1        'NO CURRENT FOB DATA EXISTS TO DUMP'
              CALL SHOWIT(0)
 5            FORMAT(A34)
              RETURN
          END IF
C     IF RAYCOD(1)=0 AND RAYCOD(2).EQ.NEWIMG.AND.REFEXT.TRUE
C     FOB WAS TRACED SUCCESSFULLY
          IF(RAYCOD(1).EQ.0.AND.REFEXT) THEN
C       FOB WAS TRACED SUCCESSFULLY
              WRITE(OUTLYNE,10)
     1        'CURRENT FOB WAS TRACED SUCCESSFULLY'
              CALL SHOWIT(0)
 10           FORMAT(A35)
C
C     HERE IS WHERE OUTPUT STATEMENTS GO FOR RAY DUMP
              DO I=NEWOBJ,NEWIMG
                  WRITE(OUTLYNE,100) I,REFRY(1,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,101) I,REFRY(2,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) I,REFRY(3,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,103) I,REFRY(4,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,104) I,REFRY(5,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,105) I,REFRY(6,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,106) I,REFRY(7,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,107) I,REFRY(8,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,108) I,REFRY(9,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,109) I,REFRY(10,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,110) I,REFRY(11,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,111) I,REFRY(12,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,112) I,REFRY(13,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,113) I,REFRY(14,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,114) I,REFRY(15,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,115) I,REFRY(16,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,116) I,REFRY(17,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,117) I,REFRY(18,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,118) I,REFRY(19,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,119) I,REFRY(20,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,120) I,REFRY(21,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,121) I,REFRY(22,I)
                  CALL SHOWIT(0)
                  IF(REFRY(23,I).EQ.1.0D0) REV= 'RAY NOT REVERSED '
                  IF(REFRY(23,I).EQ.-1.0D0) REV='RAY REVERSED     '
                  WRITE(OUTLYNE,122) I,REV
                  CALL SHOWIT(0)
                  IF(REFRY(24,I).EQ.1.0D0) POS= 'RAY IS POSRAY    '
                  IF(REFRY(24,I).EQ.-1.0D0) POS='RAY IS NOT POSRAY'
                  WRITE(OUTLYNE,122) I,POS
                  CALL SHOWIT(0)
 100              FORMAT('SURF=',I3,1X,'        X-COORDINATE=',D23.15)
 101              FORMAT('SURF=',I3,1X,'        Y-COORDINATE=',D23.15)
 102              FORMAT('SURF=',I3,1X,'        Z-COORDINATE=',D23.15)
 103              FORMAT('SURF=',I3,1X,'       L-DIR. COSINE=',D23.15)
 104              FORMAT('SURF=',I3,1X,'       M-DIR. COSINE=',D23.15)
 105              FORMAT('SURF=',I3,1X,'       N-DIR. COSINE=',D23.15)
 106              FORMAT('SURF=',I3,1X,'     PHYSICAL LENGTH=',D23.15)
 107              FORMAT('SURF=',I3,1X,'OPL-(SURF-1 TO SURF)=',D23.15)
 108              FORMAT('SURF=',I3,1X,'           COSINE(I)=',D23.15)
 109              FORMAT('SURF=',I3,1X,'          COSINE(IP)=',D23.15)
 110              FORMAT('SURF=',I3,1X,'   XZ-SLOPE(RADIANS)=',D23.15)
 111              FORMAT('SURF=',I3,1X,'   YZ-SLOPE(RADIANS)=',D23.15)
 112              FORMAT('SURF=',I3,1X,'  L-(SURFACE NORMAL)=',D23.15)
 113              FORMAT('SURF=',I3,1X,'  M-(SURFACE NORMAL)=',D23.15)
 114              FORMAT('SURF=',I3,1X,'  N-(SURFACE NORMAL)=',D23.15)
 115              FORMAT('SURF=',I3,1X,' X-(PRE-INTERACTION)=',D23.15)
 116              FORMAT('SURF=',I3,1X,' Y-(PRE-INTERACTION)=',D23.15)
 117              FORMAT('SURF=',I3,1X,' Z-(PRE-INTERACTION)=',D23.15)
 118              FORMAT('SURF=',I3,1X,' L-(PRE-INTERACTION)=',D23.15)
 119              FORMAT('SURF=',I3,1X,' M-(PRE-INTERACTION)=',D23.15)
 120              FORMAT('SURF=',I3,1X,' N-(PRE-INTERACTION)=',D23.15)
 121              FORMAT('SURF=',I3,1X,'OPL-(NEWOBJ TO SURF)=',D23.15)
 122              FORMAT('SURF=',I3,1X,A17)
              END DO
C
              RETURN
          END IF
C     IF RAYCOD(1)NOT=0 RAY WAS TRACED UNSUCCESSFULLY TO SURFACE
C     RAYCOD(2)
          IF(RAYCOD(1).NE.0) THEN
C       RAY WAS TRACED UNSUCCESSFULLY TO SURFACE RAYCOD(2)
              WRITE(OUTLYNE,15)
     1        'CURRENT RAY WAS TRACED UNSUCCESSFULLY AND FAILED AT SURFACE # '
     2        ,RAYCOD(2)
              CALL SHOWIT(0)
 15           FORMAT(A62,I3)
C
              IF(RAYCOD(1).EQ.1) THEN
                  WRITE(OUTLYNE,*)'RAY DID NOT INTERSECT THE SURFACE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.2) THEN
                  WRITE(OUTLYNE,*)
     1            'CONVERGENCE FAILURE OCCURRED DURING RAY INTERSECTION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'WITH ASPHERIC, TORIC OR SPECIAL SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'ACCURACY OF INTERSECTION WAS = ',ACLENG
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.3) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY FAILED TO CONVERGE TO REFERENCE SURFACE RAY-AIM POINT'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.4) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'TOTAL INTERNAL REFLECTION'
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(RAYCOD(1).EQ.5) THEN
                  WRITE(OUTLYNE,*)
     1            'ANGLE OF DIFFRACTION AT THE GRATING IS PHYSICALLY UNREALIZABLE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.6) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY BLOCKED BY CLEAR APERTURE ERASE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.7) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY BLOCKED BY OBSCURATION ERASE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.8) THEN
                  WRITE(OUTLYNE,*)
     1            'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*) '11',RAYCOD(1),RAYCOD(2)
                  CALL SHOWIT(1)
              END IF
              IF(RAYCOD(1).EQ.9) THEN
                  WRITE(OUTLYNE,*)
     1            'ANGLE OF DIFFRACTION AT THE HOE IS PHYSICALLY UNREALIZABLE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.10) THEN
                  WRITE(OUTLYNE,*)
     1            'HOE CONSTRUCTION POINTS CONFLICT WITH TRANSMISSION/REFLECTION'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'MODE OR CONSTRUCTION POINTS ARE NOT DEFINED'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.11) THEN
                  WRITE(OUTLYNE,*)
     1            'REFERENCE RAY CAN NOT BE TRACED AT ZERO WAVELENGTH '
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.12) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY CAN NOT BE TRACED AT ZERO WAVELENGTH '
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.13) THEN
                  WRITE(OUTLYNE,*)
     1            'RAY MISSED GRAZING INCIDENCE SURFACE SECTION'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.14) THEN
                  WRITE(OUTLYNE,*)
     1            'ILLUMINATION RAY BLOCKED BY CLEAR APERTURE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.15) THEN
                  WRITE(OUTLYNE,*)
     1            'NO GRID FILE EXISTS FOR THIS SURFACE'
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,*)
     1            'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
                  CALL SHOWIT(0)
              END IF
              IF(RAYCOD(1).EQ.16) THEN
                  WRITE(OUTLYNE,*)
     1            'SPECIFIED OBJECT POINT DOES NOT EXIST'
                  CALL SHOWIT(0)
              END IF
C
C     HERE IS WHERE OUTPUT STATEMENTS GO FOR RAY DUMP
              DO I=NEWOBJ,RAYCOD(2)
                  IF(I.EQ.RAYCOD(2))WRITE(OUTLYNE,*)
     1            'DATA FOR THIS LAST SURFACE IS SUSPECT'
                  IF(I.EQ.RAYCOD(2))CALL SHOWIT(0)
                  WRITE(OUTLYNE,100) I,REFRY(1,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,101) I,REFRY(2,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) I,REFRY(3,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,103) I,REFRY(4,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,104) I,REFRY(5,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,105) I,REFRY(6,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,106) I,REFRY(7,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,107) I,REFRY(8,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,108) I,REFRY(9,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,109) I,REFRY(10,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,110) I,REFRY(11,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,111) I,REFRY(12,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,112) I,REFRY(13,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,113) I,REFRY(14,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,114) I,REFRY(15,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,115) I,REFRY(16,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,116) I,REFRY(17,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,117) I,REFRY(18,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,118) I,REFRY(19,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,119) I,REFRY(20,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,120) I,REFRY(21,I)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,121) I,REFRY(22,I)
                  CALL SHOWIT(0)
                  IF(REFRY(23,I).EQ.1.0D0) REV= 'RAY NOT REVERSED '
                  IF(REFRY(23,I).EQ.-1.0D0) REV='RAY REVERSED     '
                  WRITE(OUTLYNE,122) I,REV
                  CALL SHOWIT(0)
                  IF(REFRY(24,I).EQ.1.0D0) POS= 'RAY IS POSRAY    '
                  IF(REFRY(24,I).EQ.-1.0D0) POS='RAY IS NOT POSRAY'
                  WRITE(OUTLYNE,122) I,POS
                  CALL SHOWIT(0)
              END DO
C
              RETURN
          END IF
          RETURN
      END
