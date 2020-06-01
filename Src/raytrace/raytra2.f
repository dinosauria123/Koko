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

C       THIS IS THE SECOND FILE OF RAYTRACING ROUTINES

C SUB HOE_TRACE.FOR
      SUBROUTINE HOE_TRACE
          IMPLICIT NONE
          INTEGER CURFIG,SOUFIG,REFFIG
          CHARACTER A*3,AFIG*3
          REAL*8 HOE_L,HOE_M,HOE_N,XO,YO,ZO
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER CURSURF
          SAVE CURSURF
          LOGICAL CFGQUIET
          COMMON/QUIETCFG/CFGQUIET
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
C       THIS ROUTINE DETERMINES THE DIRECTION COSINES FOR A TYPE 13
C       SPECIAL SURFACE
C     1.FIRST THE CURRENT LENS ARRAYS
C       ARE WRITTEN TO TEMPORARY ARRAY AREAS
C     2.THE RAY IS TRACED IN EACH OF THE TWO CONFIGS SPECIFIED IN THE
C       HOE-R DEFINITION IN RAY ARRAYS DIFFERENT FROM THE REGULAR ARRAYS
C       AND THE RAY DIRECTION COSINES ARE REMEMBERED AT THE FINAL SURFACE
C     3.THE CURRENT LENS ARRAYS ARE RESTORED.
C     4.HOE_DO_IT IS SET TO 3 IN HITSUR AFTER A RETURN IS MADE FROM
C       THIS ROUTINE
C     5.THE REGULAR RAY TRACE PROCEEDS AS USUAL
C     SINCE HOE-R IS ONLY VALID AT CONFIG 1, WHICH IS THE PERMANENT LENS
C     WE DON'T NEED TO SAVE THE CURRENT CONFIG, JUST REMEMBER WHERE THE RAY

C     Initialize to 0, not clear why they are needed (U.G.)
          HOE_L = 0.d0
          HOE_M = 0.d0
          HOE_N = 0.d0

C     TRACE SURFACE POINTER IS
          CURFIG=1
C     WHAT IS THE SOURCE POINT CONFIG NUMBER?
          SOUFIG=INT(FTFL01(13,R_I))
C     WHAT IS THE PLAYBACK CONFIG NUMBER?
          REFFIG=INT(FTFL01(14,R_I))
C     SAVE THE RAY TRACE SURFACE POINTER
          CURSURF=R_I
          CFGQUIET=.TRUE.
          XO=FTFL01(3,R_I)
          YO=FTFL01(4,R_I)
          ZO=FTFL01(5,R_I)
          RAYCLEAR=.FALSE.
C     SWITCH TO SOUFIG
          WRITE(A,10) SOUFIG
          READ(A,20) AFIG
 10       FORMAT(I3)
 20       FORMAT(A3)
          SAVE_KDP(16)=SAVEINPT(16)
          INPUT='CFG '//AFIG
          CALL PROCES
          REST_KDP(16)=RESTINPT(16)
C     TRACE SOUCE RAY
          CALL TRACE_HOERAY(XO,YO,ZO)
          IF(STOPP.EQ.1) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                  CALL SHOWIT(1)
                  OUTLYNE='RAY FAILED IN HOE-R SOURCE CONFIGURATION RAY TRACE'
                  CALL SHOWIT(1)
              END IF
              RAYCOD(1)=21
              RAYCOD(2)=R_I
              SPDCD1=RAYCOD(1)
              SPDCD2=R_I
              SAVE_KDP(16)=SAVEINPT(16)
              INPUT='CFG 1'
              CALL PROCES
              REST_KDP(16)=RESTINPT(16)
              CFGQUIET=.FALSE.
              RAYCLEAR=.TRUE.
              CALL MACFAL
              RETURN
          END IF
          L1HOE=HOE_L
          M1HOE=HOE_M
          N1HOE=HOE_N
          XO=FTFL01(8,R_I)
          YO=FTFL01(9,R_I)
          ZO=FTFL01(10,R_I)
C     SWITCH TO REFFIG
          WRITE(A,10) REFFIG
          READ(A,20) AFIG
          SAVE_KDP(16)=SAVEINPT(16)
          INPUT='CFG '//AFIG
          CALL PROCES
          REST_KDP(16)=RESTINPT(16)
C     TRACE REFERENCE RAY
          CALL TRACE_HOERAY(XO,YO,ZO)
          IF(STOPP.EQ.1) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                  CALL SHOWIT(1)
                  OUTLYNE='RAY FAILED IN HOE-R REFERENCE CONFIGURATION RAY TRACE'
                  CALL SHOWIT(1)
              END IF
              RAYCOD(1)=22
              RAYCOD(2)=R_I
              SPDCD1=RAYCOD(1)
              SPDCD2=R_I
              SAVE_KDP(16)=SAVEINPT(16)
              INPUT='CFG 1'
              CALL PROCES
              REST_KDP(16)=RESTINPT(16)
              CFGQUIET=.FALSE.
              RAYCLEAR=.TRUE.
              CALL MACFAL
              RETURN
          END IF
          L2HOE=HOE_L
          M2HOE=HOE_M
          N2HOE=HOE_N
C     SWITCH BACK TO ORIGINAL CURFIG
C     SWITCH TO SOUFIG
          SAVE_KDP(16)=SAVEINPT(16)
          INPUT='CFG 1'
          CALL PROCES
          REST_KDP(16)=RESTINPT(16)
          CFGQUIET=.FALSE.
          RAYCLEAR=.TRUE.
          R_I=CURSURF
          RETURN
      END
C SUB HIT17.FOR
      SUBROUTINE HIT17
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE HIT17.FOR. THIS SUBROUTINE IMPLEMENTS
C       TYPE 17 SPECIAL SURFACE RAYTRACING.
C
          REAL*8 SIGNNU,RR_N,RR_Z,
     1    T0,NUSUBV,TESTLEN,NORM,SNINDX,SNIND2
     3    ,MAG,J,ARG,NUSUBS
C
          LOGICAL NOHITMES
          COMMON/HITMES/NOHITMES
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          PHASE=0.0D0
          IF(WVN.EQ.1) WWVN=46
          IF(WVN.EQ.2) WWVN=47
          IF(WVN.EQ.3) WWVN=48
          IF(WVN.EQ.4) WWVN=49
          IF(WVN.EQ.5) WWVN=50
          IF(WVN.EQ.6) WWVN=71
          IF(WVN.EQ.7) WWVN=72
          IF(WVN.EQ.8) WWVN=73
          IF(WVN.EQ.9) WWVN=74
          IF(WVN.EQ.10) WWVN=75
          SNINDX=DABS(ALENS(WWVN,R_I-1))/ALENS(WWVN,R_I-1)
          SNIND2=DABS(ALENS(WWVN,R_I))/ALENS(WWVN,R_I)
C
          RR_N=R_N
          RR_Z=R_Z
C
C       FIRST CALCULATE THE INTERSECTION TO THE SIMPLE PLANE
C
          IF(R_I.EQ.(NEWOBJ+1)) THEN
              R_X=R_XAIM
              R_Y=R_YAIM
              R_Z=R_ZAIM
          ELSE
              T0=R_Z
C       T0/N IS THE DISTANCE ALONG THE RAY FROM
C       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
C       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
C       SYSTEM OF THE CURRENT SURFACE.
C
              IF(DABS(R_N).NE.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
              ELSE
                  NUSUBV=-T0/R_N
                  R_X=R_X+NUSUBV*R_L
                  R_Y=R_Y+NUSUBV*R_M
                  R_Z=0.0D0
              END IF
          END IF
          REG(40)=REG(9)
          REG(9)=R_X
          REG(10)=R_Y
          REG(11)=0.0D0
          REG(30)=REG(13)
          REG(13)=0.0
          REG(14)=0.0
          REG(15)=1.0
C
C       NOW REFINE TO INTERSECT WITH THE TYPE 17
C
C       SUBROUTINE NR4 INTERSECTS
C
          INR=ALENS(76,R_I)
          CALL NR4
C
          R_X=REG(9)
          R_Y=REG(10)
          R_Z=REG(11)
          NORM=DSQRT((REG(13)**2)+(REG(14)**2)+(REG(15)**2))
          LN=REG(13)/NORM
          MN=REG(14)/NORM
          NN=REG(15)/NORM
C
          IF(STOPP.EQ.1) RETURN
C
          NUSUBS=((ALENS(WWVN,(R_I-1)))/
     1    (ALENS(WWVN,R_I)))
          SIGNNU=DABS(NUSUBS)/NUSUBS
          NUSUBS=DABS(NUSUBS)
C
          COSI=((R_L*LN)+(R_M*MN)+(R_N*NN))
          IF(COSI.LT.-1.0D0) COSI=-1.0D0
          IF(COSI.GT.+1.0D0) COSI=+1.0D0
C
          IF(.NOT.DUM(R_I)) THEN
              IF(SIGNNU.LT.0.0D0) THEN
C       REFLECTION
                  R_L=(R_L-((2.0D0*COSI)*LN))
                  R_M=(R_M-((2.0D0*COSI)*MN))
                  R_N=(R_N-((2.0D0*COSI)*NN))
                  MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
                  R_L=R_L/MAG
                  R_M=R_M/MAG
                  R_N=R_N/MAG
C
                  COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
                  IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
                  IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
              ELSE
C       NOT REFLECTION, PROCEED
                  ARG=(1.0D0-(NUSUBS**2)*(1.0D0-(COSI**2)))
                  IF(ARG.LT.0.0D0) THEN
C       TIR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE='TOTAL INTERNAL REFLECTION'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=4
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO TIR
                  END IF
                  IF(COSI.LE.0.0D0) COSIP=-DSQRT(ARG)
                  IF(COSI.GT.0.0D0) COSIP=DSQRT(ARG)
                  J=((ALENS(WWVN,R_I)*COSIP)
     1            -(ALENS(WWVN,(R_I-1))*COSI))/
     2            (ALENS(WWVN,R_I))
                  R_L=((NUSUBS*R_L)+(J*LN))
                  R_M=((NUSUBS*R_M)+(J*MN))
                  R_N=((NUSUBS*R_N)+(J*NN))
                  MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
                  R_L=R_L/MAG
                  R_M=R_M/MAG
                  R_N=R_N/MAG
C
              END IF
C     DUM
          END IF
          COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
          IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
          IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
C     IF AT A DUMMY SURFACE, THE THICKNESS GOES FROM POS TO NEG
C     OR NEG TO POS, THEN IF RV, THEN SET .NOT.RV AND IF
C     NOT RV SET TO RV
C     OR AT A DUMMY SURFACE, N POS INTO NEG TH OR N NEG INTO POS TH
C     THEN IF RV, THEN SET .NOT.RV AND IF
          TESTLEN=R_Z-RR_Z
C     NOT RV SET TO RV
C     IF AT A DUMMY SURFACE, THE THICKNESS CHANGES SIGN, THEN THE
C     RAY GETS "REVERSED"
          IF(ALENS(3,R_I).GT.0.0D0.AND.ALENS(3,R_I-1).LT
     1    .0.0D0.AND.DUM(R_I).OR.ALENS(3,R_I).LT.0.0D0.AND.ALENS(3,R_I-1)
     2    .GT.0.0D0.AND.DUM(R_I)) THEN
              IF(RV) THEN
                  RV=.FALSE.
              ELSE
                  RV=.TRUE.
              END IF
          END IF
C     IF THE RAY HAS TRAVELED A NEG DIST WITH A POS DIRCOS
C     OR THE RAY HAS TRAVELED A POS DIST WITH A NEG DIR COS
C     AND THE RAY WAS NOT REVERESED, THEN SET IT TO "REVERSED"
C     RAY GETS "REVERSED"
          IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     2    RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.
     3    RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.
     4    SNINDX.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     5    SNINDX.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     6    SNINDX.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.
     7    SNINDX.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR) THEN
              RV=.TRUE.
          ELSE
              RV=.FALSE.
          END IF
          IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.RVSTART.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.RVSTART) THEN
              RV=.FALSE.
          END IF
          IF(RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     2    RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.
     3    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR) THEN
              RV=.FALSE.
              RVSTART=.FALSE.
          END IF
C       RAY TRACE AT SURFACE R_I COMPLETE.
          IF(R_I.EQ.NEWIMG) THEN
              R_L=OLDL
              R_M=OLDM
              R_N=OLDN
          END IF
          COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
          IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
          IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
          STOPP=0
          RETURN
      END
C SUB HITSUR.FOR
      SUBROUTINE HITSUR
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE HITSUR.FOR. THIS SUBROUTINE IMPLEMENTS
C       SURFACE INTERSECTIONS IN RAYTRACING. IT IS THE MAIN TRACING
C       ROUTINE. INTERSECTION TO SPECIAL SURFACES IS DONE BY
C       SUBROUTINE XYZSP.FOR FOR IF SPECIAL SURFACES ARE
C       PRESENT AND SET TO "ON" AND IF THEY ARE OF A RECOGNIZED TYPE.
C
          INTEGER N_X,N_Y
C
          REAL*8 OR_N,OR_Z
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C
          PHASE=0.0D0
C
          IF(ALENS(46,R_I).EQ.ALENS(46,(R_I-1)).AND.ALENS(47,R_I).EQ.
     1    ALENS(47,(R_I-1)).AND.ALENS(48,R_I).EQ.ALENS(48,(R_I-1)).AND.
     2    ALENS(49,R_I).EQ.ALENS(49,(R_I-1)).AND.ALENS(50,R_I).EQ.
     3    ALENS(50,(R_I-1)).AND.
     4    ALENS(71,R_I).EQ.ALENS(71,(R_I-1)).AND.ALENS(72,R_I).EQ.
     5    ALENS(72,(R_I-1)).AND.ALENS(73,R_I).EQ.ALENS(73,(R_I-1)).AND.
     6    ALENS(74,R_I).EQ.ALENS(74,(R_I-1)).AND.ALENS(75,R_I).EQ.
     7    ALENS(75,(R_I-1))) THEN
C       SURFACE IS A DUMMY
              IF(ALENS(68,R_I).EQ.0.0D0) DUM(R_I)=.TRUE.
              IF(ALENS(68,R_I).EQ.1.0D0) DUM(R_I)=.FALSE.
          ELSE
              DUM(R_I)=.FALSE.
          END IF
          IF(.NOT.DUMMMY(R_I)) DUM(R_I)=.FALSE.
C       INTERS KEEPS TRACK OF SINGLE OR MULTIPLE SURFACE
C       INTERSECTIONS
          INTERS=0
          SEC=0
C
          OLDL=R_L
          OLDM=R_M
          OLDN=R_N
C       SET STOPP TO 0
          STOPP=0
C       IF STOPP GETS SET TO 1, THE REFRAY TRACE IS STOPPED
C
C**********************************************************************
C
C                               NOW
C
C                   INTERSECT USER-DEFINED TYPE 17 SURFACES
C
          IF(ALENS(34,R_I).EQ.17.0D0.AND.ALENS(124,R_I).EQ.0.0D0) THEN
              CALL HIT17
              RETURN
          END IF
C
C**********************************************************************
C
C                               NOW
C
C                   INTERSECT FRESNEL-1 SURFACES
C
          IF(ALENS(34,R_I).EQ.16.0D0.AND.ALENS(1,R_I).EQ.0.0D0.AND.
     1    ALENS(23,R_I).EQ.0.0D0.OR.
     1    ALENS(34,R_I).EQ.16.0D0.AND.ALENS(23,R_I).NE.0.0D0.AND.
     1    ALENS(24,R_I).EQ.0.0D0) THEN
              CALL HITFRZFL
              RETURN
          END IF
          IF(ALENS(34,R_I).EQ.16.0D0.AND.ALENS(1,R_I).NE.0.0D0
     1    .AND.ALENS(23,R_I).EQ.0.0D0.OR.
     1    ALENS(34,R_I).EQ.16.0D0.AND.ALENS(23,R_I).NE.0.0D0.AND.
     2    ALENS(24,R_I).NE.0.0D0) THEN
              CALL HITFRZCV
              RETURN
          END IF
C
C**********************************************************************
C
C                               NOW
C
C                   INTERSECT GRAZING INCIDENCE SURFACES
C
          IF(ALENS(34,R_I).EQ.18.0D0) THEN
              CALL HITGRAZ
              RETURN
          END IF
C
C**********************************************************************
C                               NOW
C
C                   INTERSECT VARIOUS SURFACE TYPES
C
C**********************************************************************
C       PARAXIAL SURFACE
C
          IF(ALENS(124,R_I).EQ.1.0D0) THEN
              CALL HITPARAX(OR_N,OR_Z)
              IF(STOPP.EQ.0) THEN
                  IF(ALENS(34,R_I).NE.13.0D0.OR.ALENS(34,R_I).EQ.13.0D0
     1            .AND.F12.NE.1) THEN
                      CALL INTERACK(OR_N,OR_Z)
                      HOE_DO_IT=0
                  END IF
                  IF(ALENS(34,R_I).EQ.13.0D0.AND.F12.EQ.1) THEN
C     SET THE TARGET POSITIONS ON THE HOE AND RETURN
                      XHOE=R_X
                      YHOE=R_Y
                      ZHOE=R_Z
                      HOE_WAV_NUM=INT(FTFL01(2,R_I))
                      HOE_DO_IT=0
                      CALL HOE_TRACE
                      IF(STOPP.EQ.1) RETURN
                      HOE_DO_IT=3
                      CALL INTERACK(OR_N,OR_Z)
                      HOE_DO_IT=0
                      RETURN
                  END IF
              END IF
              RETURN
C       SURFACE NOT PARAXIAL
          END IF
C
C**********************************************************************
C       SURFACE R_I IS PLANO AND MAY CONTAIN 2ND, 4TH, 6TH, 8TH AND 10TH ORDER
C       ASPHERIC TERMS AND SPECIAL STUFF
C
          IF(ALENS(1,R_I).EQ.0.0D0.AND.ALENS(23,R_I)
     1    .EQ.0.0D0) THEN
C
              IF(ALENS(133,R_I).NE.0.0D0) THEN
C
C       CODE GOES HERE FOR ARRAY SURFACE IN SIDE
C
                  A_R_X=R_X
                  A_R_Y=R_Y
                  A_R_Z=R_Z
                  A_R_L=R_L
                  A_R_M=R_M
                  A_R_N=R_N
                  CALL ARRAYIN_FIX(N_X,N_Y)
              END IF
              CALL HITFLA(OR_N,OR_Z)
              IF(STOPP.EQ.0) THEN
                  IF(ALENS(34,R_I).NE.13.0D0.OR.ALENS(34,R_I).EQ.13.0D0
     1            .AND.F12.NE.1) THEN
                      CALL INTERACK(OR_N,OR_Z)
                      HOE_DO_IT=0
C
C       CODE GOES HERE FOR ARRAY SURFACE OUT SIDE
C
                      IF(ALENS(133,R_I).NE.0.0D0) THEN
                          CALL ARRAYOUT_FIX(N_X,N_Y)
                      END IF
                  END IF
                  IF(ALENS(34,R_I).EQ.13.0D0.AND.F12.EQ.1) THEN
C     SET THE TARGET POSITIONS ON THE HOE AND RETURN
                      XHOE=R_X
                      YHOE=R_Y
                      ZHOE=R_Z
                      HOE_WAV_NUM=INT(FTFL01(2,R_I))
                      HOE_DO_IT=0
                      CALL HOE_TRACE
                      IF(STOPP.EQ.1) RETURN
                      HOE_DO_IT=3
                      RETURN
                  END IF
              END IF
              RETURN
C       SURFACE NOT PLANO WITH ASPHERICS
          END IF
C
C**********************************************************************
C       SURFACE R_I IS SPHERICAL OR CONIC AND MAY HAVE ROTATIONALLY
C       SYMMETRIC ASPHERIC DEFORMATION COEFFICIENTS
C
          XOLD=R_X
          YOLD=R_Y
          ZOLD=R_Z
          IF(ALENS(1,R_I).NE.0.0D0.AND.ALENS(23,R_I).EQ.0.0D0) THEN
              IF(ALENS(133,R_I).NE.0.0D0) THEN
C
C       CODE GOES HERE FOR ARRAY SURFACE IN SIDE
C
                  A_R_X=R_X
                  A_R_Y=R_Y
                  A_R_Z=R_Z
                  A_R_L=R_L
                  A_R_M=R_M
                  A_R_N=R_N
                  CALL ARRAYIN_FIX(N_X,N_Y)
              END IF
              CALL HITASP(OR_N,OR_Z)
              IF(STOPP.EQ.0) THEN
                  IF(ALENS(34,R_I).NE.13.0D0.OR.ALENS(34,R_I).EQ.13.0D0
     1            .AND.F12.NE.1) THEN
                      CALL INTERACK(OR_N,OR_Z)
                      HOE_DO_IT=0
C
C       CODE GOES HERE FOR ARRAY SURFACE OUT SIDE
C
                      IF(ALENS(133,R_I).NE.0.0D0) THEN
                          CALL ARRAYOUT_FIX(N_X,N_Y)
                      END IF
                  END IF
                  IF(ALENS(34,R_I).EQ.13.0D0.AND.F12.EQ.1) THEN
C     SET THE TARGET POSITIONS ON THE HOE AND RETURN
                      XHOE=R_X
                      YHOE=R_Y
                      ZHOE=R_Z
                      HOE_WAV_NUM=INT(FTFL01(2,R_I))
                      HOE_DO_IT=0
                      CALL HOE_TRACE
                      IF(STOPP.EQ.1) RETURN
                      HOE_DO_IT=3
                      CALL INTERACK(OR_N,OR_Z)
                      HOE_DO_IT=0
                      RETURN
                  END IF
              END IF
              RETURN
C       SURFACE NOT SPHERICAL OR CONIC, MAY BE ROTATIONALLY SYMMETRIC
C       ASPHERIC OR A SPECIAL SURFACE SHAPE
          END IF
C**********************************************************************
C       SURFACE R_I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
C
          IF(ALENS(23,R_I).NE.0.0D0) THEN
C
              IF(ALENS(133,R_I).NE.0.0D0) THEN
C
C       CODE GOES HERE FOR ARRAY SURFACE IN SIDE
C
                  A_R_X=R_X
                  A_R_Y=R_Y
                  A_R_Z=R_Z
                  A_R_L=R_L
                  A_R_M=R_M
                  A_R_N=R_N
                  CALL ARRAYIN_FIX(N_X,N_Y)
              END IF
              CALL HITANA(OR_N,OR_Z)
              IF(STOPP.EQ.0) THEN
                  IF(ALENS(34,R_I).NE.13.0D0.OR.ALENS(34,R_I).EQ.13.0D0
     1            .AND.F12.NE.1) THEN
                      CALL INTERACK(OR_N,OR_Z)
                      HOE_DO_IT=0
C
C       CODE GOES HERE FOR ARRAY SURFACE OUT SIDE
C
                      IF(ALENS(133,R_I).NE.0.0D0) THEN
                          CALL ARRAYOUT_FIX(N_X,N_Y)
                      END IF
                  END IF
                  IF(ALENS(34,R_I).EQ.13.0D0.AND.F12.EQ.1) THEN
C     SET THE TARGET POSITIONS ON THE HOE AND RETURN
                      XHOE=R_X
                      YHOE=R_Y
                      ZHOE=R_Z
                      HOE_WAV_NUM=INT(FTFL01(2,R_I))
                      HOE_DO_IT=0
                      CALL HOE_TRACE
                      IF(STOPP.EQ.1) RETURN
                      HOE_DO_IT=3
                      CALL INTERACK(OR_N,OR_Z)
                      HOE_DO_IT=0
                      RETURN
                  END IF
              END IF
              RETURN
C       SURFACE NOT ANAMORPHIC
          END IF
          OUTLYNE='SURFACE TYPE NOT YET IN PROGRAM'
          CALL SHOWIT(1)
          RETURN
      END
C**********************************************************************
      SUBROUTINE APLANA(I,WWWW1,WWWW2,WWWWW1,WWWWW2)
          IMPLICIT NONE
          REAL*8 WX,WY,PARTX,PARTY,WWWW1,WWWW2,RD,HGT,FULL
     1    ,WWWWW1,WWWWW2
          INTEGER I
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(WWWW1.GE.0.0D0) WY= 1.0D0
          IF(WWWW1.LT.0.0D0) WY=-1.0D0
          IF(WWWW2.GE.0.0D0) WX= 1.0D0
          IF(WWWW2.LT.0.0D0) WX=-1.0D0
C     DETERMINE THE NEW WW1 AND WW2 VALUES
          IF(ALENS(127,I).EQ.0.0D0) THEN
              HGT=ALENS(9,I)
          ELSE
              HGT=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
          END IF
          RD=1.0D0/ALENS(1,I)
C     DETERMINE THE ANGLE OF THE FULL RAY
          FULL=DASIN(HGT/RD)
          PARTX=FULL*DABS(WWWW2)
          PARTY=FULL*DABS(WWWW1)
          WWWWW1=DSIN(PARTY)*RD*WY
          WWWWW2=DSIN(PARTX)*RD*WX
          RETURN
      END
C SUB HITASP.FOR
      SUBROUTINE HITASP(OR_N,OR_Z)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE HITASP.FOR. THIS SUBROUTINE IMPLEMENTS
C       ROTATIONALLY SYMMETRIC ASPHERIC SURFACE INTERSECTIONS IN RAYTRACING.
C       INTERSECTION TO SPECIAL SURFACES IS DONE BY
C       SUBROUTINE XYZSP.FOR FOR IF SPECIAL SURFACES ARE
C       PRESENT AND SET TO "ON" AND IF THEY ARE OF A RECOGNIZED TYPE.
C
          REAL*8 A,B,C,QQ,SIGNNU,CV,CC,OR_Z,OR_N,
     1    TEST1,TEST2,NUSUBS,RR_N,RR_Z
     3    ,Q,SIGNB,MAG,ARG,ZTEST,SNIND2
     4    ,FPX,FPY,FPZ,HV0,HV1,HV2,ZMIN,ZMAX,
     5    X1,X2,Y1,Y2,Z1,Z2,LN1,LN2,MN1,MN2,NN1,NN2
     6    ,SNINDX
C
          INTEGER ZPMIN,ZPMAX
C
C     HOE STUFF
C
          LOGICAL ERR
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          PHASE=0.0D0
          HV0=0.0D0
          HV1=0.0D0
          HV2=0.0D0
          IF(WVN.EQ.1) WWVN=46
          IF(WVN.EQ.2) WWVN=47
          IF(WVN.EQ.3) WWVN=48
          IF(WVN.EQ.4) WWVN=49
          IF(WVN.EQ.5) WWVN=50
          IF(WVN.EQ.6) WWVN=71
          IF(WVN.EQ.7) WWVN=72
          IF(WVN.EQ.8) WWVN=73
          IF(WVN.EQ.9) WWVN=74
          IF(WVN.EQ.10) WWVN=75
          SNINDX=DABS(ALENS(WWVN,R_I-1))/ALENS(WWVN,R_I-1)
          SNIND2=DABS(ALENS(WWVN,R_I))/ALENS(WWVN,R_I)
C
C       SURFACE IS CONIC
          CV=ALENS(1,R_I)
          CC=ALENS(2,R_I)
          IF(CC.EQ.-1.0D0.AND.CV.GT.0.0D0) ZTEST=1.0D20
          IF(CC.EQ.-1.0D0.AND.CV.LT.0.0D0) ZTEST=-1.0D20
          IF(CC.NE.-1.0D0) ZTEST=1.0D0/((CC+1.0D0)*CV)
C
          RR_N=R_N
          RR_Z=R_Z
          OR_N=RR_N
          OR_Z=RR_Z
C
C       SURFACE IS ROTATIONALLY SYMMETRIC ASPHERIC WITH NON-ZERO
C       CURVATURE
C
          IF(R_I.EQ.(NEWOBJ+1)) THEN
              R_X=R_XAIM
              R_Y=R_YAIM
              R_Z=R_ZAIM
C       JUST PROCEED WITH THE DIRECT INTERSECTION TO THE CONIC
          END IF
          NUSUBS=((ALENS((WWVN),(R_I-1)))/
     1    (ALENS((WWVN),R_I)))
          SIGNNU=DABS(NUSUBS)/NUSUBS
          NUSUBS=DABS(NUSUBS)
C
C       NOW INTERSECT THE SPHERE OR CONIC.
C       THE FOLLOWING CALCULATIONS ARE INTERMEDIATE STEPS:
C
          A=-(CV*((R_L**2)+(R_M**2)+((R_N**2)*
     1    (CC+1.0D0))))
          B=R_N-(CV*R_X*R_L)-(CV*R_Y*R_M)-
     1    ((CV*R_Z*R_N)
     1    *(CC+1.0D0))
          B=2.0D0*B
          C=-CV*((R_X**2)+(R_Y**2)+((R_Z**2)*
     1    (CC+1.0D0)))
          C=(C+(2.0D0*R_Z))
          IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
          IF(B.EQ.0.0D0) SIGNB=1.0D0
          IF(A.EQ.0.0D0) THEN
              HV0=-(C/B)
              INTERS=1
          ELSE
C       A NOT ZERO
              ARG=((B**2)-(4.0D0*A*C))
              IF(ARG.LT.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
C       ARG NOT ZERO
              END IF
              Q=(-0.5D0*(B+(SIGNB*(DSQRT(ARG)))))
              HV1=C/Q
              HV2=Q/A
              INTERS=2
          END IF
          IF(INTERS.EQ.1) THEN
C       CASE OF ONLY ONE INTERSECTION POINT
              X1=(R_X+(HV0*R_L))
              Y1=(R_Y+(HV0*R_M))
              Z1=(R_Z+(HV0*R_N))
          END IF
          IF(INTERS.EQ.2) THEN
C       CASE OF TWO INTERSECTION POINTS
              X1=(R_X+(HV1*R_L))
              Y1=(R_Y+(HV1*R_M))
              Z1=(R_Z+(HV1*R_N))
              X2=(R_X+(HV2*R_L))
              Y2=(R_Y+(HV2*R_M))
              Z2=(R_Z+(HV2*R_N))
          END IF
C     FIX FOR HYPERBOLAS
          IF(CC.LT.-1.0D0) THEN
              ZMIN=Z2
              ZMAX=Z2
              ZPMIN=2
              ZPMAX=2
              IF(Z1.LT.Z2) ZMIN=Z1
              IF(Z1.LT.Z2) ZPMIN=1
              IF(Z1.GT.Z2) ZMAX=Z1
              IF(Z1.GT.Z2) ZPMAX=1
C     HYPERBOLA
C     CV POSITIVE
              IF(CV.GT.0.0D0) THEN
                  IF(ZMIN.LT.0.0D0) THEN
C     ONLY ONE INTERSECTION POINT
                      INTERS=1
                      IF(ZPMIN.EQ.1) THEN
                          X1=X2
                          Y1=Y2
                          Z1=Z2
C     LEAVE X1,Y1,Z1 ALONE
                      END IF
C     TWO POINTS
                  END IF
              ELSE
C    CV NEGATIVE
                  IF(ZMAX.GT.0.0D0) THEN
C     ONLY ONE INTERSECTION POINT
                      INTERS=1
                      IF(ZPMAX.EQ.1) THEN
                          X1=X2
                          Y1=Y2
                          Z1=Z2
C     LEAVE X1,Y1,Z1 ALONE
                      END IF
C     THERE ARE TWO POINTS
                  END IF
              END IF
C     NOT HYPERBOLA
          END IF
C
C
C       CALCULATE THE DIRECTION COSINES OF THE FIRST POINT
          QQ=1.0D0-((CC+1.0D0)*(CV**2)*((X1**2)+(Y1**2)))
          IF(QQ.LT.0.0D0) THEN
              QQ=-QQ
C       QQ NOT NEGATIVE
          END IF
          QQ=DSQRT(QQ)
          QQ=1.0D0+QQ
          IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                  CALL SHOWIT(1)
                  OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                  CALL SHOWIT(1)
              END IF
              RAYCOD(1)=1
              RAYCOD(2)=R_I
              SPDCD1=RAYCOD(1)
              SPDCD2=R_I
              STOPP=1
              RETURN
          END IF
          FPX=-((2.0D0*CV*X1*QQ*(QQ-1.0D0))+
     1    ((CV**3)*X1*((X1**2)+(Y1**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
          FPY=-((2.0D0*CV*Y1*QQ*(QQ-1.0D0))+
     1    ((CV**3)*Y1*((X1**2)+(Y1**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
          IF(CV.GT.0.0D0) THEN
C     USE INWARD NORMAL
              IF(CC.LT.-1.0D0) THEN
                  FPZ=1.0D0
              ELSE
                  IF(Z1.GT.ZTEST) FPZ=-1.0D0
                  IF(Z1.LT.ZTEST) FPZ=1.0D0
                  IF(Z1.EQ.ZTEST) FPZ=0.0D0
              END IF
          ELSE
C     CV NEG
C     USE OUTWARD NORMAL
              IF(CC.LT.-1.0D0) THEN
                  FPZ=1.0D0
              ELSE
                  IF(Z1.LT.ZTEST) FPZ=-1.0D0
                  IF(Z1.GT.ZTEST) FPZ=1.0D0
                  IF(Z1.EQ.ZTEST) FPZ=0.0D0
              END IF
          END IF
C
          MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
          LN1=FPX/MAG
          MN1=FPY/MAG
          NN1=FPZ/MAG
C       NOW THE DIRECTION COSINES OF THE SECOND POINT IF IT EXISTS
          IF(INTERS.EQ.2) THEN
C       FIRST THE DIRECTION COSINES OF THE SURFACE NORMAL
              QQ=1.0D0-((CC+1.0D0)*(CV**2)*((X2**2)+(Y2**2)))
              IF(QQ.LT.0.0D0) THEN
                  QQ=-QQ
C       QQ NOT NEG
              END IF
              QQ=DSQRT(QQ)
              QQ=1.0D0+QQ
              IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
              END IF
              FPX=-((2.0D0*CV*X2*QQ*(QQ-1.0D0))+
     1        ((CV**3)*X2*((X2**2)+(Y2**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
              FPY=-((2.0D0*CV*Y2*QQ*(QQ-1.0D0))+
     1        ((CV**3)*Y2*((X2**2)+(Y2**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
              IF(CV.GT.0.0D0) THEN
C     USE INWARD NORMAL
                  IF(CC.LT.-1.0D0) THEN
                      FPZ=1.0D0
                  ELSE
                      IF(Z2.GT.ZTEST) FPZ=-1.0D0
                      IF(Z2.LT.ZTEST) FPZ=1.0D0
                      IF(Z2.EQ.ZTEST) FPZ=0.0D0
                  END IF
              ELSE
C     CV NEG
C     USE OUTWARD NORMAL
                  IF(CC.LT.-1.0D0) THEN
                      FPZ=1.0D0
                  ELSE
                      IF(Z2.LT.ZTEST) FPZ=-1.0D0
                      IF(Z2.GT.ZTEST) FPZ=1.0D0
                      IF(Z2.EQ.ZTEST) FPZ=0.0D0
                  END IF
              END IF
C
              MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
              LN2=FPX/MAG
              MN2=FPY/MAG
              NN2=FPZ/MAG
C       NOT TWO INTERSECTION POINTS
          END IF
C       NOW FIGURE OUT WHICH INTERSECTION POINT TO USE
C       DOT THE INCOMMING DIR COS WITH EACH SURFACE NORMAL
C
          IF(INTERS.EQ.2) THEN
C       THERE ARE TWO INTERSECTION POINTS
              TEST1=((R_L*LN1)+(R_M*MN1)+(R_N*NN1))
              TEST2=((R_L*LN2)+(R_M*MN2)+(R_N*NN2))
              IF(POSRAY.AND..NOT.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  ELSE
C       TEST1 NOT >0
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  END IF
                  GO TO 200
C     NOT POSRAY
              END IF
              IF(POSRAY.AND.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  ELSE
C       TEST1 NOT >0
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  END IF
                  GO TO 200
C     NOT POSRAY
              END IF
              IF(.NOT.POSRAY.AND..NOT.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  ELSE
C       TEST1 NOT >0
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  END IF
                  GO TO 200
C     POSRAY
              END IF
              IF(.NOT.POSRAY.AND.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  ELSE
C       TEST1 NOT >0
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  END IF
                  GO TO 200
C     POSRAY
              END IF
          ELSE
C       NOT TWO INTERSECTIONS
C       JUST ONE INTERSECTION
              R_X=X1
              R_Y=Y1
              R_Z=Z1
              LN=LN1
              MN=MN1
              NN=NN1
          END IF
 200      CONTINUE
C
C       NOW WE KNOW THE INTERSECTION POINT IF THE ASPHERIC TERMS
C       ARE ALL ZERO. WE ALSO HAVE THE SURFACE NORMALS
C
C       NOW CALCULATE THE RAY INTERSECTION WITH THE DEFORMED ASPHERIC
C       USING A CALL TO SUBROUTINE NR2. THIS ALSO GIVE THE DIRECTION
C       COSINES OF THE SURFACE NORMAL TO THE ASPHERIC.
C       NR2 ALSO DEALS WITH SPECIAL SURFACE TYPES:
C
          INR=ALENS(76,R_I)
          IF(ALENS(8,R_I).NE.0.0D0.OR.ALENS(34,R_I).GT.0.0D0.AND.
     1    DABS(ALENS(34,R_I)).NE.19.0D0.AND.
     1    DABS(ALENS(34,R_I)).NE.20.0D0.OR.ALENS(103,R_I).EQ.1.0D0) THEN
              ERR=.FALSE.
              CALL NR2(ERR)
              IF(ERR) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB GETZEE1.FOR
      SUBROUTINE GETZEE1
C
          IMPLICIT NONE
C
C     CALCULATES BEST INTERSECTION POINT TO FIRST SURFACE
C     WHEN SCY FANG OR SCX FANG IS SPECIFIED. RECOGNIZES UP TO
C     CONIC PROFILE ON SURFACE 1
C     USED FOR BETTER RAY AIMING
C
          REAL*8 A,B,C,CV,CC,ZTEST,MAG,
     1    Q,SIGNB,ARG,HV0,HV1,HV2,XA,YA,ZA
C
          INTEGER JIM
C
          LOGICAL ZEEERR
          COMMON/ERRZEE/ZEEERR
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          ZEEERR=.FALSE.
          HV0=0.0D0
          HV1=0.0D0
          HV2=0.0D0
C
C       SURFACE IS CONIC OR SPHERE
          CV=ALENS(1,1)
          CC=ALENS(2,1)
          IF(CC.EQ.-1.0D0.AND.CV.GT.0.0D0) ZTEST=1.0D20
          IF(CC.EQ.-1.0D0.AND.CV.LT.0.0D0) ZTEST=-1.0D20
          IF(CC.NE.-1.0D0) ZTEST=1.0D0/((CC+1.0D0)*CV)
C     COMPUTE DIR COS DIRECTLY FROM POSITIONS IN ALL CASES
C
          XA=XC
          YA=YC
          ZA=ZC
C     THESE ARE IN LOCAL COORDINATE SYSTEM OF NEWOBJ+1
C     CONVERT TO NEWOBJ COORD SYSTEM
          R_TX=XA
          R_TY=YA
          R_TZ=ZA
          CALL BAKONE
          XA=R_TX
          YA=R_TY
          ZA=R_TZ
          MAG=DSQRT(((XSTRT-XA)**2)+((YSTRT-YA)**2)
     1    +((ZSTRT-ZA)**2))
          R_L=(XA-XSTRT)/MAG
          R_M=(YA-YSTRT)/MAG
          R_N=(ZA-ZSTRT)/MAG
C     NOW PUT DIR COS INTO NEWOBJ+1 COORD SYSTEM
          R_TX=R_L
          R_TY=R_M
          R_TZ=R_N
          CALL FORONEL
C
          XA=XC
          YA=YC
          ZA=ZC
          DO JIM=1,2
              IF(JIM.EQ.1) THEN
                  R_L=0.0D0
                  R_M=0.0D0
                  R_N=1.0D0
              ELSE
                  R_L=R_TX
                  R_M=R_TY
                  R_N=R_TZ
              END IF
C
              R_X=XA
              R_Y=YA
              R_Z=ZA
C
C       NOW INTERSECT THE SPHERE OR CONIC AT NEWOBJ+1
C       THE FOLLOWING CALCULATIONS ARE INTERMEDIATE STEPS:
C
              A=-(CV*((R_L**2)+(R_M**2)+((R_N**2)*
     1        (CC+1.0D0))))
              B=R_N-(CV*R_X*R_L)-(CV*R_Y*R_M)-
     1        ((CV*R_Z*R_N)
     1        *(CC+1.0D0))
              B=2.0D0*B
              C=-CV*((R_X**2)+(R_Y**2)+((R_Z**2)*
     1        (CC+1.0D0)))
              C=(C+(2.0D0*R_Z))
              IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
              IF(B.EQ.0.0D0) SIGNB=1.0D0
              IF(A.EQ.0.0D0) THEN
                  HV0=-(C/B)
                  INTERS=1
              ELSE
C       A NOT ZERO
                  ARG=((B**2)-(4.0D0*A*C))
                  IF(ARG.LT.0.0D0) THEN
                      ZEEERR=.TRUE.
                      RETURN
C       ARG NOT ZERO
                  END IF
                  Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
                  HV1=C/Q
                  HV2=Q/A
                  INTERS=2
              END IF
              IF(INTERS.EQ.1) THEN
C       CASE OF ONLY ONE INTERSECTION POINT
                  XC=(R_X+(HV0*R_L))
                  YC=(R_Y+(HV0*R_M))
                  ZC=(R_Z+(HV0*R_N))
              END IF
              IF(INTERS.EQ.2) THEN
C       CASE OF TWO INTERSECTION POINTS
                  IF(DABS(HV1).LE.DABS(HV2)) THEN
                      XC=(R_X+(HV1*R_L))
                      YC=(R_Y+(HV1*R_M))
                      ZC=(R_Z+(HV1*R_N))
                  ELSE
                      XC=(R_X+(HV2*R_L))
                      YC=(R_Y+(HV2*R_M))
                      ZC=(R_Z+(HV2*R_N))
                  END IF
              END IF
C     COORDINATES ARE IN COORDINATE SYSTEM OF THE NEWOBJ+1 SURFACE
          END DO
          RETURN
      END
C SUB HITFLA.FOR
      SUBROUTINE HITFLA(OR_N,OR_Z)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE HITFLA.FOR. THIS SUBROUTINE IMPLEMENTS
C       FLAT ASPHERIC SURFACE INTERSECTIONS IN RAYTRACING.
C       INTERSECTION TO SPECIAL SURFACES IS DONE BY
C       SUBROUTINE XYZSP.FOR FOR IF SPECIAL SURFACES ARE
C       PRESENT AND SET TO "ON" AND IF THEY ARE OF A RECOGNIZED TYPE.
C
          REAL*8 SIGNNU,RR_N,RR_Z,OR_N,OR_Z
     1    ,T0,NUSUBV,SNINDX,SNIND2,NUSUBS
C
C     HOE STUFF
C
          LOGICAL ERR
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          PHASE=0.0D0
          IF(WVN.EQ.1) WWVN=46
          IF(WVN.EQ.2) WWVN=47
          IF(WVN.EQ.3) WWVN=48
          IF(WVN.EQ.4) WWVN=49
          IF(WVN.EQ.5) WWVN=50
          IF(WVN.EQ.6) WWVN=71
          IF(WVN.EQ.7) WWVN=72
          IF(WVN.EQ.8) WWVN=73
          IF(WVN.EQ.9) WWVN=74
          IF(WVN.EQ.10) WWVN=75
          SNINDX=DABS(ALENS(WWVN,R_I-1))/ALENS(WWVN,R_I-1)
          SNIND2=DABS(ALENS(WWVN,R_I))/ALENS(WWVN,R_I)
          NUSUBS=((ALENS(WWVN,(R_I-1)))/
     1    (ALENS(WWVN,R_I)))
          SIGNNU=DABS(NUSUBS)/NUSUBS
          NUSUBS=DABS(NUSUBS)
C
          RR_N=R_N
          RR_Z=R_Z
          OR_N=RR_N
          OR_Z=RR_Z
          LN=0.0D0
          MN=0.0D0
          NN=1.0D0
C
C       SURFACE R_I IS PLANO AND CONTAINS 2ND, 4TH, 6TH, 8TH AND 10TH ORDER
C       ASPHERIC TERMS.
C
C       FIRST CALCULATE THE INTERSECTION TO THE SIMPLE PLANE
C
          IF(R_I.EQ.(NEWOBJ+1)) THEN
              R_X=R_XAIM
              R_Y=R_YAIM
              R_Z=R_ZAIM
          ELSE
              T0=R_Z
C       T0/N IS THE DISTANCE ALONG THE RAY FROM
C       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
C       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
C       SYSTEM OF THE CURRENT SURFACE.
C
              IF(((R_L*LN)+(R_M*MN)+(R_N*NN)).EQ.0.0D0) THEN
                  IF(DUM(R_I)) THEN
                      R_Z=0.0D0
C       SURFACE IS A FLAT DUMMY SURFACE
C       ASSIGN SOME VALUES TO X,Y AND Z AND RETURN
C       THE X,Y AND Z VALUES DO NOT CHANGE. THEY REMAIN THE COORDINATES
C       OF THE RAY AT THE PREVIOUS SURFACE IN THE COORDINATE SYSTEM OF
C       THE CURRENT SURFACE
                      IF(R_L.EQ.0.0D0) THEN
C       RAY IS VERTICAL
                          R_Y=0.0D0
                      END IF
                      IF(R_M.EQ.0.0D0) THEN
C       RAY IS HORIZONTAL
                          R_X=0.0D0
                      END IF
                      IF(R_M.NE.0.0D0.AND.R_L.NE.0.0D0) THEN
C       RAY IS GENERALLY IN XY PLANE
C       GO TO A NEW R_X=0
                          R_Y=(-(R_M/R_L)*R_X)+R_Y
                          R_X=0.0D0
                      END IF
                      COSI=(R_L*LN)+(R_M*MN)+(R_N*NN)
                      IF(COSI.LT.-1.0D0) COSI=-1.0D0
                      IF(COSI.GT.+1.0D0) COSI=+1.0D0
                      COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
                      IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
                      IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
                      STOPP=0
                      RAYCOD(1)=0
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      RETURN
C       RAY DIRECTION COSINES REMAIN THE SAME
                  ELSE
C       NOT A DUMMY SURFACE, RAY FAILED
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=1
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
                  END IF
              ELSE
                  NUSUBV=-T0/R_N
                  R_X=R_X+NUSUBV*R_L
                  R_Y=R_Y+NUSUBV*R_M
                  R_Z=0.0D0
              END IF
          END IF
C
C       NOW REFINE TO INTERSECT WITH THE ASPHERIC
C
C       SUBROUTINE NR1 INTERSECTS
C       A PLANO WITH ASPERIC TERMS GIVEN THE INTERSECTION
C       WITH THE SIMPLE PLANO AS A STARTING POINT. IT
C       ALSO TAKES CARE OF SPECIAL SURFACES OF TYPES:
C
          INR=ALENS(76,R_I)
          IF(ALENS(8,R_I).NE.0.0D0.OR.ALENS(34,R_I).GT.0.0D0.AND.
     1    DABS(ALENS(34,R_I)).NE.19.0D0.AND.
     1    DABS(ALENS(34,R_I)).NE.20.0D0
     1    .OR.ALENS(103,R_I).EQ.1.0D0) THEN
              ERR=.FALSE.
              CALL NR1(ERR)
              IF(ERR) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB DPHASE.FOR
      SUBROUTINE DPHASE
C
          IMPLICIT NONE
C
C     THIS ADDS PHASE FOR GRATINGS AND HOES
C
          REAL*8 T1
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C     INTERSECTION OF NON-DIFFRACTED RAY PLANE WITH SURFACE NORMAL
          T1=((((R_L0*(R_X))
     1    +(R_M0*(R_Y))
     2    +(R_N0*(R_Z)))
     4    -((R_L*(R_X))
     1    +(R_M*(R_Y))
     2    +(R_N*(R_Z)))))
          PHASE=PHASE-T1

          RETURN
      END
C SUB HITPARAX.FOR
      SUBROUTINE HITPARAX(OR_N,OR_Z)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE HITPARAX.FOR. THIS SUBROUTINE IMPLEMENTS
C       INTERSECTIONS TO PARAXIAL SURFACES
C
          REAL*8 SIGNNU,RR_N,RR_Z,OR_N,OR_Z
     1    ,T0,NUSUBV,SNINDX,SNIND2,NUSUBS
C
C     HOE STUFF
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          PHASE=0.0D0
          IF(WVN.EQ.1) WWVN=46
          IF(WVN.EQ.2) WWVN=47
          IF(WVN.EQ.3) WWVN=48
          IF(WVN.EQ.4) WWVN=49
          IF(WVN.EQ.5) WWVN=50
          IF(WVN.EQ.6) WWVN=71
          IF(WVN.EQ.7) WWVN=72
          IF(WVN.EQ.8) WWVN=73
          IF(WVN.EQ.9) WWVN=74
          IF(WVN.EQ.10) WWVN=75
          SNINDX=DABS(ALENS(WWVN,R_I-1))/ALENS(WWVN,R_I-1)
          SNIND2=DABS(ALENS(WWVN,R_I))/ALENS(WWVN,R_I)
          NUSUBS=((ALENS(WWVN,(R_I-1)))/
     1    (ALENS(WWVN,R_I)))
          SIGNNU=DABS(NUSUBS)/NUSUBS
          NUSUBS=DABS(NUSUBS)
C
          RR_N=R_N
          RR_Z=R_Z
          OR_N=RR_N
          OR_Z=RR_Z
          LN=0.0D0
          MN=0.0D0
          NN=1.0D0
C
C       SURFACE R_I IS PLANO AND CONTAINS 2ND, 4TH, 6TH, 8TH AND 10TH ORDER
C       ASPHERIC TERMS.
C
C       FIRST CALCULATE THE INTERSECTION TO THE SIMPLE PLANE
C
          IF(R_I.EQ.(NEWOBJ+1)) THEN
              R_X=R_XAIM
              R_Y=R_YAIM
              R_Z=R_ZAIM
          ELSE
              T0=R_Z
C       T0/N IS THE DISTANCE ALONG THE RAY FROM
C       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
C       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
C       SYSTEM OF THE CURRENT SURFACE.
C
              IF(DABS(R_N).NE.0.0D0) THEN
                  IF(DUM(R_I)) THEN
                      R_Z=0.0D0
C       SURFACE IS A FLAT DUMMY SURFACE
C       ASSIGN SOME VALUES TO X,Y AND Z AND RETURN
C       THE X,Y AND Z VALUES DO NOT CHANGE. THEY REMAIN THE COORDINATES
C       OF THE RAY AT THE PREVIOUS SURFACE IN THE COORDINATE SYSTEM OF
C       THE CURRENT SURFACE
                      IF(R_L.EQ.0.0D0) THEN
C       RAY IS VERTICAL
                          R_Y=0.0D0
                      END IF
                      IF(R_M.EQ.0.0D0) THEN
C       RAY IS HORIZONTAL
                          R_X=0.0D0
                      END IF
                      IF(R_M.NE.0.0D0.AND.R_L.NE.0.0D0) THEN
C       RAY IS GENERALLY IN XY PLANE
C       GO TO A NEW R_X=0
                          R_Y=(-(R_M/R_L)*R_X)+R_Y
                          R_X=0.0D0
                      END IF
                      COSI=(R_L*LN)+(R_M*MN)+(R_N*NN)
                      IF(COSI.LT.-1.0D0) COSI=-1.0D0
                      IF(COSI.GT.+1.0D0) COSI=+1.0D0
                      COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
                      IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
                      IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
                      STOPP=0
                      RAYCOD(1)=0
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      RETURN
C       RAY DIRECTION COSINES REMAIN THE SAME
                  ELSE
C       NOT A DUMMY SURFACE, RAY FAILED
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=1
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
                  END IF
              ELSE
                  NUSUBV=-T0/R_N
                  R_X=R_X+NUSUBV*R_L
                  R_Y=R_Y+NUSUBV*R_M
                  R_Z=0.0D0
              END IF
          END IF
C
C       NOW REFINE TO INTERSECT WITH THE ASPHERIC
C
C       SUBROUTINE NRPARAX COMPUTES THE SURFACE NORMAL
C
          INR=ALENS(76,R_I)
          CALL NRPARAX
          RETURN
      END
C SUB HITFRZFL.FOR
      SUBROUTINE HITFRZFL
C
          IMPLICIT NONE
C
C     FLAT FREZNEL-1 SURFACE
C
          REAL*8 T0,NUSUBV,NUSUBS,TESTLEN
     3    ,MAG,J,ARG,RR_N,RR_Z
     4    ,FPX,FPY,FPZ,SIGNNU,C3,C4,C5,C6,C7,C8,C9,C10,C11
     5    ,Q,C1,C2,SNINDX,SNIND2,RRXX,RRYY
C
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          PHASE=0.0D0
          IF(WVN.EQ.1) WWVN=46
          IF(WVN.EQ.2) WWVN=47
          IF(WVN.EQ.3) WWVN=48
          IF(WVN.EQ.4) WWVN=49
          IF(WVN.EQ.5) WWVN=50
          IF(WVN.EQ.6) WWVN=71
          IF(WVN.EQ.7) WWVN=72
          IF(WVN.EQ.8) WWVN=73
          IF(WVN.EQ.9) WWVN=74
          IF(WVN.EQ.10) WWVN=75
          SNINDX=DABS(ALENS(WWVN,R_I-1))/ALENS(WWVN,R_I-1)
          SNIND2=DABS(ALENS(WWVN,R_I))/ALENS(WWVN,R_I)
          RR_N=R_N
          RR_Z=R_Z
C
          IF(R_I.EQ.(NEWOBJ+1)) THEN
              R_X=R_XAIM
              R_Y=R_YAIM
              R_Z=R_ZAIM
          ELSE
              T0=R_Z
              IF(DABS(R_N).NE.0.0D0) THEN
                  IF(DUM(R_I)) THEN
                      R_Z=0.0D0
C       SURFACE IS A FLAT DUMMY SURFACE
C       ASSIGN SOME VALUES TO X,Y AND Z AND RETURN
C       THE X,Y AND Z VALUES DO NOT CHANGE. THEY REMAIN THE COORDINATES
C       OF THE RAY AT THE PREVIOUS SURFACE IN THE COORDINATE SYSTEM OF
C       THE CURRENT SURFACE
                      IF(R_L.EQ.0.0D0) THEN
C       RAY IS VERTICAL
                          R_Y=0.0D0
                      END IF
                      IF(R_M.EQ.0.0D0) THEN
C       RAY IS HORIZONTAL
                          R_X=0.0D0
                      END IF
                      IF(R_M.NE.0.0D0.AND.R_L.NE.0.0D0) THEN
C       RAY IS GENERALLY IN XY PLANE
C       GO TO A NEW R_X=0
                          R_Y=(-(R_M/R_L)*R_X)+R_Y
                          R_X=0.0D0
                      END IF
                      RAYCOD(1)=0
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=0
                      RETURN
C       RAY DIRECTION COSINES REMAIN THE SAME
                  ELSE
C       NOT A DUMMY SURFACE, RAY FAILED
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=1
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                  END IF
                  RETURN
              ELSE
C     R_N NOT NEAR ZERO, PROCEED
C       KEEP GOING
                  NUSUBV=-T0/R_N
C       T0/N IS THE DISTANCE ALONG THE RAY FROM
C       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
C       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
C       SYSTEM OF THE CURRENT SURFACE.
C
                  R_X=R_X+NUSUBV*R_L
                  R_Y=R_Y+NUSUBV*R_M
                  R_Z=0.0D0
              END IF
          END IF
          NUSUBS=((ALENS((WWVN),(R_I-1)))/
     1    (ALENS((WWVN),R_I)))
          SIGNNU=DABS(NUSUBS)/NUSUBS
          NUSUBS=DABS(NUSUBS)
C
          IF(ALENS(23,R_I).EQ.0.0D0) THEN
              RRXX=R_X
              RRYY=R_Y
          END IF
          IF(ALENS(23,R_I).EQ.1.0D0) THEN
              RRXX=R_X
              RRYY=0.0D0
          END IF
          IF(ALENS(23,R_I).EQ.2.0D0) THEN
              RRXX=0.0D0
              RRYY=R_Y
          END IF
C
C     NOW HOW DO THE SPECIAL SURFACE TERMS CHANGE THIS ?
          C1=FTFL01(1,R_I)
          C2=FTFL01(2,R_I)
          C3=FTFL01(3,R_I)
          C4=FTFL01(4,R_I)
          C5=FTFL01(5,R_I)
          C6=FTFL01(6,R_I)
          C7=FTFL01(7,R_I)
          C8=FTFL01(8,R_I)
          C9=FTFL01(9,R_I)
          C10=FTFL01(10,R_I)
          C11=FTFL01(11,R_I)
          IF(C1.EQ.0.0D0.OR.C2.EQ.-1.0D0) THEN
              Q=1.0D0
          ELSE
              Q=(1.0D0-((C2+1.0D0)*(C1**2)*
     1        ((RRXX**2)+(RRYY**2))))
          END IF
          IF(Q.LE.0.0D0) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                  CALL SHOWIT(1)
                  OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                  CALL SHOWIT(1)
              END IF
              STOPP=1
              RAYCOD(1)=1
              RAYCOD(2)=R_I
              SPDCD1=RAYCOD(1)
              SPDCD2=R_I
              RETURN
          END IF
          IF(Q.GT.0.0D0) Q=DSQRT(Q)+1.0D0
C
          IF(C1.NE.0.0D0) THEN
              FPX=-(
     1        (((2.0D0*C1*RRXX*Q*(Q-1.0D0))+
     1        ((C1**3)*RRXX*((RRXX**2)+(RRYY**2))*(C2+1.0D0)))
     1        /((Q-1.0D0)*(Q**2)))
     1        +((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRXX)*C3)
     1        +((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRXX)*C4)
     1        +((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRXX)*C5)
     1        +((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRXX)*C6)
     1        +((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRXX)*C7)
     1        +((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRXX)*C8)
     1        +((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRXX)*C9)
     1        +((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRXX)*C10)
     1        +((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRXX)*C11)
     1        )
              FPY=-(
     1        (((2.0D0*C1*RRYY*Q*(Q-1.0D0))+
     1        ((C1**3)*RRYY*((RRXX**2)+(RRYY**2))*(C1+1.0D0)))
     1        /((Q-1.0D0)*(Q**2)))
     1        +((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRYY)*C3)
     1        +((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRYY)*C4)
     1        +((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRYY)*C5)
     1        +((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRYY)*C6)
     1        +((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRYY)*C7)
     1        +((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRYY)*C8)
     1        +((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRYY)*C9)
     1        +((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRYY)*C10)
     1        +((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRYY)*C11)
     1        )
          ELSE
              FPX=-(
     1        +((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRXX)*C3)
     1        +((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRXX)*C4)
     1        +((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRXX)*C5)
     1        +((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRXX)*C6)
     1        +((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRXX)*C7)
     1        +((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRXX)*C8)
     1        +((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRXX)*C9)
     1        +((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRXX)*C10)
     1        +((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRXX)*C11)
     1        )
              FPY=-(
     1        +((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRYY)*C3)
     1        +((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRYY)*C4)
     1        +((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRYY)*C5)
     1        +((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRYY)*C6)
     1        +((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRYY)*C7)
     1        +((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRYY)*C8)
     1        +((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRYY)*C9)
     1        +((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRYY)*C10)
     1        +((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRYY)*C11)
     1        )
          END IF
C
          FPZ=1.0D0
C
          MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
          LN=FPX/MAG
          MN=FPY/MAG
          NN=FPZ/MAG
C
          COSI=((R_L*LN)+(R_M*MN)+(R_N*NN))
          IF(COSI.LT.-1.0D0) COSI=-1.0D0
          IF(COSI.GT.+1.0D0) COSI=+1.0D0
          IF(.NOT.DUM(R_I)) THEN
              IF(SIGNNU.LT.0.0D0) THEN
C       REFLECTION
                  R_L=(R_L-((2.0D0*COSI)*LN))
                  R_M=(R_M-((2.0D0*COSI)*MN))
                  R_N=(R_N-((2.0D0*COSI)*NN))
                  MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
                  R_L=R_L/MAG
                  R_M=R_M/MAG
                  R_N=R_N/MAG
              ELSE
C       NOT REFLECTION, PROCEED
                  ARG=(1.0D0-(NUSUBS**2)*(1.0D0-(COSI**2)))
                  IF(ARG.LT.0.0D0) THEN
C       TIR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE='TOTAL INTERNAL REFLECTION'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=4
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO TIR
                  END IF
                  IF(COSI.LE.0.0D0) COSIP=-DSQRT(ARG)
                  IF(COSI.GT.0.0D0) COSIP=DSQRT(ARG)
                  J=((ALENS((WWVN),R_I)*COSIP)
     1            -(ALENS((WWVN),(R_I-1))*COSI))/
     2            (ALENS((WWVN),R_I))
                  R_L=((NUSUBS*R_L)+(J*LN))
                  R_M=((NUSUBS*R_M)+(J*MN))
                  R_N=((NUSUBS*R_N)+(J*NN))
                  MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
                  R_L=R_L/MAG
                  R_M=R_M/MAG
                  R_N=R_N/MAG
              END IF
C     DUM
          END IF
          COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
          IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
          IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
C     IF AT A DUMMY SURFACE, THE THICKNESS GOES FROM POS TO NEG
C     OR NEG TO POS, THEN IF RV, THEN SET .NOT.RV AND IF
C     NOT RV SET TO RV
C     OR AT A DUMMY SURFACE, N POS INTO NEG TH OR N NEG INTO POS TH
C     THEN IF RV, THEN SET .NOT.RV AND IF
          TESTLEN=R_Z-RR_Z
C     NOT RV SET TO RV
C     IF AT A DUMMY SURFACE, THE THICKNESS CHANGES SIGN, THEN THE
C     RAY GETS "REVERSED"
          IF(ALENS(3,R_I).GT.0.0D0.AND.ALENS(3,R_I-1).LT
     1    .0.0D0.AND.DUM(R_I).OR.ALENS(3,R_I).LT.0.0D0.AND.ALENS(3,R_I-1)
     2    .GT.0.0D0.AND.DUM(R_I)) THEN
              IF(RV) THEN
                  RV=.FALSE.
              ELSE
                  RV=.TRUE.
              END IF
          END IF
C     IF THE RAY HAS TRAVELED A NEG DIST WITH A POS DIRCOS
C     OR THE RAY HAS TRAVELED A POS DIST WITH A NEG DIR COS
C     AND THE RAY WAS NOT REVERESED, THEN SET IT TO "REVERSED"
C     RAY GETS "REVERSED"
          IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     2    RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.
     3    RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.
     4    SNINDX.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     5    SNINDX.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     6    SNINDX.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.
     7    SNINDX.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR) THEN
              RV=.TRUE.
          ELSE
              RV=.FALSE.
          END IF
          IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.RVSTART.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.RVSTART) THEN
              RV=.FALSE.
          END IF
          IF(RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     2    RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.
     3    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR) THEN
              RV=.FALSE.
              RVSTART=.FALSE.
          END IF
C       RAY TRACE AT FLAT SURFACE R_I COMPLETE.
          IF(R_I.EQ.NEWIMG) THEN
              R_L=OLDL
              R_M=OLDM
              R_N=OLDN
          END IF
          COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
          IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
          IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
          RAYCOD(1)=0
          RAYCOD(2)=R_I
          SPDCD1=RAYCOD(1)
          SPDCD2=R_I
          STOPP=0
          RETURN
      END
C SUB HITFRZCV.FOR
      SUBROUTINE HITFRZCV
C
          IMPLICIT NONE
C
C     CURVED FRESNEL-1
C
          REAL*8 A,B,C,QQ,SIGNNU,CV,CC,
     1    RR_Z,TEST1,TEST2,NUSUBS,RR_N,TESTLEN
     3    ,Q,SIGNB,MAG,J,ARG,C1,C2,C3,C4,ZTEST,SNIND2
     4    ,FPX,FPY,FPZ,C5,C6,HV0,HV1,HV2,ZMIN,ZMAX,
     5    X1,X2,Y1,Y2,Z1,Z2,LN1,LN2,MN1,MN2,NN1,NN2
     6    ,C7,C8,C9,C10,C11,SNINDX,RRXX,RRYY
C
          INTEGER ZPMIN,ZPMAX
C
C     HOE STUFF
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          PHASE=0.0D0
          HV0=0.0D0
          HV1=0.0D0
          HV2=0.0D0
          IF(WVN.EQ.1) WWVN=46
          IF(WVN.EQ.2) WWVN=47
          IF(WVN.EQ.3) WWVN=48
          IF(WVN.EQ.4) WWVN=49
          IF(WVN.EQ.5) WWVN=50
          IF(WVN.EQ.6) WWVN=71
          IF(WVN.EQ.7) WWVN=72
          IF(WVN.EQ.8) WWVN=73
          IF(WVN.EQ.9) WWVN=74
          IF(WVN.EQ.10) WWVN=75
          SNINDX=DABS(ALENS(WWVN,R_I-1))/ALENS(WWVN,R_I-1)
          SNIND2=DABS(ALENS(WWVN,R_I))/ALENS(WWVN,R_I)
C
          IF(ALENS(23,R_I).EQ.0.0D0) THEN
              CV=ALENS(1,R_I)
              CC=ALENS(2,R_I)
          END IF
          IF(ALENS(23,R_I).NE.0.0D0) THEN
              CV=ALENS(24,R_I)
              CC=ALENS(41,R_I)
          END IF
          IF(CC.EQ.-1.0D0.AND.CV.GT.0.0D0) ZTEST=1.0D20
          IF(CC.EQ.-1.0D0.AND.CV.LT.0.0D0) ZTEST=-1.0D20
          IF(CC.NE.-1.0D0) ZTEST=1.0D0/((CC+1.0D0)*CV)
C
          RR_N=R_N
          RR_Z=R_Z
C
          IF(R_I.EQ.(NEWOBJ+1)) THEN
              R_X=R_XAIM
              R_Y=R_YAIM
              R_Z=R_ZAIM
C       JUST PROCEED WITH THE DIRECT INTERSECTION TO THE CONIC
          END IF
          NUSUBS=((ALENS((WWVN),(R_I-1)))/
     1    (ALENS((WWVN),R_I)))
          SIGNNU=DABS(NUSUBS)/NUSUBS
          NUSUBS=DABS(NUSUBS)
C
C       NOW INTERSECT THE SURFACE
C       THE FOLLOWING CALCULATIONS ARE INTERMEDIATE STEPS:
C
          IF(ALENS(23,R_I).EQ.0.0D0) THEN
              RRXX=R_X
              RRYY=R_Y
          END IF
          IF(ALENS(23,R_I).EQ.1.0D0) THEN
              RRXX=R_X
              RRYY=0.0D0
          END IF
          IF(ALENS(23,R_I).EQ.2.0D0) THEN
              RRXX=0.0D0
              RRYY=R_Y
          END IF
          A=-(CV*((R_L**2)+(R_M**2)+((R_N**2)*
     1    (CC+1.0D0))))
          B=R_N-(CV*RRXX*R_L)-(CV*RRYY*R_M)-
     1    ((CV*R_Z*R_N)
     1    *(CC+1.0D0))
          B=2.0D0*B
          C=-CV*((RRXX**2)+(RRYY**2)+((R_Z**2)*
     1    (CC+1.0D0)))
          C=(C+(2.0D0*R_Z))
          IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
          IF(B.EQ.0.0D0) SIGNB=1.0D0
          IF(A.EQ.0.0D0) THEN
              HV0=-(C/B)
              INTERS=1
          ELSE
C       A NOT ZERO
              ARG=((B**2)-(4.0D0*A*C))
              IF(ARG.LT.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
C       ARG NOT ZERO
              END IF
              Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
              HV1=C/Q
              HV2=Q/A
              INTERS=2
          END IF
          IF(INTERS.EQ.1) THEN
C       CASE OF ONLY ONE INTERSECTION POINT
              X1=(R_X+(HV0*R_L))
              Y1=(R_Y+(HV0*R_M))
              Z1=(R_Z+(HV0*R_N))
          END IF
          IF(INTERS.EQ.2) THEN
C       CASE OF TWO INTERSECTION POINTS
              X1=(R_X+(HV1*R_L))
              Y1=(R_Y+(HV1*R_M))
              Z1=(R_Z+(HV1*R_N))
              X2=(R_X+(HV2*R_L))
              Y2=(R_Y+(HV2*R_M))
              Z2=(R_Z+(HV2*R_N))
          END IF
C     FIX FOR HYPERBOLAS
          IF(CC.LT.-1.0D0) THEN
              ZMIN=Z2
              ZMAX=Z2
              ZPMIN=2
              ZPMAX=2
              IF(Z1.LT.Z2) ZMIN=Z1
              IF(Z1.LT.Z2) ZPMIN=1
              IF(Z1.GT.Z2) ZMAX=Z1
              IF(Z1.GT.Z2) ZPMAX=1
C     HYPERBOLA
C     CV POSITIVE
              IF(CV.GT.0.0D0) THEN
                  IF(ZMIN.LT.0.0D0) THEN
C     ONLY ONE INTERSECTION POINT
                      INTERS=1
                      IF(ZPMIN.EQ.1) THEN
                          X1=X2
                          Y1=Y2
                          Z1=Z2
C     LEAVE X1,Y1,Z1 ALONE
                      END IF
C     TWO POINTS
                  END IF
              ELSE
C    CV NEGATIVE
                  IF(ZMAX.GT.0.0D0) THEN
C     ONLY ONE INTERSECTION POINT
                      INTERS=1
                      IF(ZPMAX.EQ.1) THEN
                          X1=X2
                          Y1=Y2
                          Z1=Z2
C     LEAVE X1,Y1,Z1 ALONE
                      END IF
C     THERE ARE TWO POINTS
                  END IF
              END IF
C     NOT HYPERBOLA
          END IF
C
C
          IF(ALENS(23,R_I).EQ.0.0D0) THEN
              RRXX=X1
              RRYY=Y1
          END IF
          IF(ALENS(23,R_I).EQ.1.0D0) THEN
              RRXX=X1
              RRYY=0.0D0
          END IF
          IF(ALENS(23,R_I).EQ.2.0D0) THEN
              RRXX=0.0D0
              RRYY=Y1
          END IF
C       CALCULATE THE DIRECTION COSINES OF THE FIRST POINT
          QQ=1.0D0-((CC+1.0D0)*(CV**2)*((RRXX**2)+(RRYY**2)))
          IF(QQ.LT.0.0D0) THEN
              QQ=-QQ
C       QQ NOT NEGATIVE
          END IF
          QQ=DSQRT(QQ)
          QQ=1.0D0+QQ
          IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                  CALL SHOWIT(1)
                  OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                  CALL SHOWIT(1)
              END IF
              RAYCOD(1)=1
              RAYCOD(2)=R_I
              SPDCD1=RAYCOD(1)
              SPDCD2=R_I
              STOPP=1
              RETURN
          END IF
          FPX=-((2.0D0*CV*RRXX*QQ*(QQ-1.0D0))+
     1    ((CV**3)*RRXX*((RRXX**2)+(RRYY**2))*
     1    (CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
          FPY=-((2.0D0*CV*RRYY*QQ*(QQ-1.0D0))+
     1    ((CV**3)*RRYY*((RRXX**2)+(RRYY**2))
     1    *(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
          IF(CV.GT.0.0D0) THEN
C     USE INWARD NORMAL
              IF(CC.LT.-1.0D0) THEN
                  FPZ=1.0D0
              ELSE
                  IF(Z1.GT.ZTEST) FPZ=-1.0D0
                  IF(Z1.LT.ZTEST) FPZ=1.0D0
                  IF(Z1.EQ.ZTEST) FPZ=0.0D0
              END IF
          ELSE
C     CV NEG
C     USE OUTWARD NORMAL
              IF(CC.LT.-1.0D0) THEN
                  FPZ=1.0D0
              ELSE
                  IF(Z1.LT.ZTEST) FPZ=-1.0D0
                  IF(Z1.GT.ZTEST) FPZ=1.0D0
                  IF(Z1.EQ.ZTEST) FPZ=0.0D0
              END IF
          END IF
C
          MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
          LN1=FPX/MAG
          MN1=FPY/MAG
          NN1=FPZ/MAG
C       NOW THE DIRECTION COSINES OF THE SECOND POINT IF IT EXISTS
          IF(INTERS.EQ.2) THEN
C       FIRST THE DIRECTION COSINES OF THE SURFACE NORMAL
              IF(ALENS(23,R_I).EQ.0.0D0) THEN
                  RRXX=X2
                  RRYY=Y2
              END IF
              IF(ALENS(23,R_I).EQ.1.0D0) THEN
                  RRXX=X2
                  RRYY=0.0D0
              END IF
              IF(ALENS(23,R_I).EQ.2.0D0) THEN
                  RRXX=0.0D0
                  RRYY=Y2
              END IF
              QQ=1.0D0-((CC+1.0D0)*(CV**2)*((RRXX**2)+(RRYY**2)))
              IF(QQ.LT.0.0D0) THEN
                  QQ=-QQ
C       QQ NOT NEG
              END IF
              QQ=DSQRT(QQ)
              QQ=1.0D0+QQ
              IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
              END IF
              FPX=-((2.0D0*CV*RRXX*QQ*(QQ-1.0D0))+
     1        ((CV**3)*RRXX*((RRXX**2)+(RRYY**2))*
     1        (CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
              FPY=-((2.0D0*CV*RRYY*QQ*(QQ-1.0D0))+
     1        ((CV**3)*RRYY*((RRXX**2)+(RRYY**2))
     1        *(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
              IF(CV.GT.0.0D0) THEN
C     USE INWARD NORMAL
                  IF(CC.LT.-1.0D0) THEN
                      FPZ=1.0D0
                  ELSE
                      IF(Z2.GT.ZTEST) FPZ=-1.0D0
                      IF(Z2.LT.ZTEST) FPZ=1.0D0
                      IF(Z2.EQ.ZTEST) FPZ=0.0D0
                  END IF
              ELSE
C     CV NEG
C     USE OUTWARD NORMAL
                  IF(CC.LT.-1.0D0) THEN
                      FPZ=1.0D0
                  ELSE
                      IF(Z2.LT.ZTEST) FPZ=-1.0D0
                      IF(Z2.GT.ZTEST) FPZ=1.0D0
                      IF(Z2.EQ.ZTEST) FPZ=0.0D0
                  END IF
              END IF
C
              MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
              LN2=FPX/MAG
              MN2=FPY/MAG
              NN2=FPZ/MAG
C       NOT TWO INTERSECTION POINTS
          END IF
C       NOW FIGURE OUT WHICH INTERSECTION POINT TO USE
C       DOT THE INCOMMING DIR COS WITH EACH SURFACE NORMAL
C
          IF(INTERS.EQ.2) THEN
C       THERE ARE TWO INTERSECTION POINTS
              TEST1=((R_L*LN1)+(R_M*MN1)+(R_N*NN1))
              TEST2=((R_L*LN2)+(R_M*MN2)+(R_N*NN2))
              IF(POSRAY.AND..NOT.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  ELSE
C       TEST1 NOT >0
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  END IF
                  GO TO 200
C     NOT POSRAY
              END IF
              IF(POSRAY.AND.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  ELSE
C       TEST1 NOT >0
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  END IF
                  GO TO 200
C     NOT POSRAY
              END IF
              IF(.NOT.POSRAY.AND..NOT.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  ELSE
C       TEST1 NOT >0
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  END IF
                  GO TO 200
C     POSRAY
              END IF
              IF(.NOT.POSRAY.AND.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  ELSE
C       TEST1 NOT >0
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  END IF
                  GO TO 200
C     POSRAY
              END IF
          ELSE
C       NOT TWO INTERSECTIONS
C       JUST ONE INTERSECTION
              R_X=X1
              R_Y=Y1
              R_Z=Z1
              LN=LN1
              MN=MN1
              NN=NN1
          END IF
 200      CONTINUE
C
C       NOW WE KNOW THE INTERSECTION POINT IF THE ASPHERIC TERMS
C       ARE ALL ZERO. WE ALSO HAVE THE SURFACE NORMALS
C
C       NOW CALCULATE THE RAY REAL SURFACE NORMALS DUE TO THE
C       FRESNEL SURFACE
C
          C1=FTFL01(1,R_I)
          C2=FTFL01(2,R_I)
          C3=FTFL01(3,R_I)
          C4=FTFL01(4,R_I)
          C5=FTFL01(5,R_I)
          C6=FTFL01(6,R_I)
          C7=FTFL01(7,R_I)
          C8=FTFL01(8,R_I)
          C9=FTFL01(9,R_I)
          C10=FTFL01(10,R_I)
          C11=FTFL01(11,R_I)
          IF(ALENS(23,R_I).EQ.0.0D0) THEN
              RRXX=R_X
              RRYY=R_Y
          END IF
          IF(ALENS(23,R_I).EQ.1.0D0) THEN
              RRXX=R_X
              RRYY=0.0D0
          END IF
          IF(ALENS(23,R_I).EQ.1.0D0) THEN
              RRXX=0.0D0
              RRYY=R_Y
          END IF
C
          IF(C1.EQ.0.0D0.OR.C2.EQ.-1.0D0) THEN
              Q=1.0D0
          ELSE
              Q=(1.0D0-((C2+1.0D0)*(C1**2)*
     1        ((RRXX**2)+(RRYY**2))))
          END IF
          IF(Q.LE.0.0D0) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                  CALL SHOWIT(1)
                  OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                  CALL SHOWIT(1)
              END IF
              STOPP=1
              RAYCOD(1)=1
              RAYCOD(2)=R_I
              SPDCD1=RAYCOD(1)
              SPDCD2=R_I
              RETURN
          END IF
C
          IF(Q.GT.0.0D0) Q=DSQRT(Q)+1.0D0
C
          IF(C1.NE.0.0D0) THEN
              FPX=-(
     1        (((2.0D0*C1*RRXX*Q*(Q-1.0D0))+
     1        ((C1**3)*RRXX*((RRXX**2)+(RRYY**2))*(C2+1.0D0)))
     1        /((Q-1.0D0)*(Q**2)))
     1        +((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRXX)*C3)
     1        +((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRXX)*C4)
     1        +((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRXX)*C5)
     1        +((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRXX)*C6)
     1        +((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRXX)*C7)
     1        +((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRXX)*C8)
     1        +((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRXX)*C9)
     1        +((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRXX)*C10)
     1        +((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRXX)*C11)
     1        )
              FPY=-(
     1        (((2.0D0*C1*RRYY*Q*(Q-1.0D0))+
     1        ((C1**3)*RRYY*((RRXX**2)+(RRYY**2))*(C1+1.0D0)))
     1        /((Q-1.0D0)*(Q**2)))
     1        +((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRYY)*C3)
     1        +((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRYY)*C4)
     1        +((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRYY)*C5)
     1        +((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRYY)*C6)
     1        +((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRYY)*C7)
     1        +((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRYY)*C8)
     1        +((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRYY)*C9)
     1        +((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRYY)*C10)
     1        +((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRYY)*C11)
     1        )
          ELSE
              FPX=-(
     1        +((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRXX)*C3)
     1        +((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRXX)*C4)
     1        +((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRXX)*C5)
     1        +((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRXX)*C6)
     1        +((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRXX)*C7)
     1        +((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRXX)*C8)
     1        +((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRXX)*C9)
     1        +((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRXX)*C10)
     1        +((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRXX)*C11)
     1        )
              FPY=-(
     1        +((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRYY)*C3)
     1        +((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRYY)*C4)
     1        +((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRYY)*C5)
     1        +((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRYY)*C6)
     1        +((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRYY)*C7)
     1        +((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRYY)*C8)
     1        +((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRYY)*C9)
     1        +((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRYY)*C10)
     1        +((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRYY)*C11)
     1        )
          END IF
          FPZ=1.0D0
C
C     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
C
          MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
          LN=FPX/MAG
          MN=FPY/MAG
          NN=FPZ/MAG
C
C       THE COSINE SQUARED OF THE ANGLE OF INCIDENCE IS JUST
          COSI=(LN*R_L)+(MN*R_M)+(NN*R_N)
          IF(COSI.LT.-1.0D0) COSI=-1.0D0
          IF(COSI.GT.+1.0D0) COSI=+1.0D0
          IF(.NOT.DUM(R_I)) THEN
              IF(SIGNNU.LT.0.0D0) THEN
C       REFLECTION
                  R_L=(R_L-((2.0D0*COSI)*LN))
                  R_M=(R_M-((2.0D0*COSI)*MN))
                  R_N=(R_N-((2.0D0*COSI)*NN))
                  MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
                  R_L=R_L/MAG
                  R_M=R_M/MAG
                  R_N=R_N/MAG
              ELSE
C       NOT REFLECTION, PROCEED
                  ARG=(1.0D0-(NUSUBS**2)*(1.0D0-(COSI**2)))
                  IF(ARG.LT.0.0D0) THEN
C       TIR OCCURRED, PRINT MESSAGE AND STOP TRACING
                      IF(MSG) THEN
                          WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                          CALL SHOWIT(1)
                          OUTLYNE='TOTAL INTERNAL REFLECTION'
                          CALL SHOWIT(1)
                      END IF
                      RAYCOD(1)=4
                      RAYCOD(2)=R_I
                      SPDCD1=RAYCOD(1)
                      SPDCD2=R_I
                      STOPP=1
                      RETURN
C       NO TIR
                  END IF
                  IF(COSI.LE.0.0D0) COSIP=-DSQRT(ARG)
                  IF(COSI.GT.0.0D0) COSIP=DSQRT(ARG)
                  J=((ALENS((WWVN),R_I)*COSIP)
     1            -(ALENS((WWVN),(R_I-1))*COSI))/
     2            (ALENS((WWVN),R_I))
                  R_L=((NUSUBS*R_L)+(J*LN))
                  R_M=((NUSUBS*R_M)+(J*MN))
                  R_N=((NUSUBS*R_N)+(J*NN))
                  MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
                  R_L=R_L/MAG
                  R_M=R_M/MAG
                  R_N=R_N/MAG
              END IF
C     DUM
          END IF
          COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
          IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
          IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
C     IF AT A DUMMY SURFACE, THE THICKNESS GOES FROM POS TO NEG
C     OR NEG TO POS, THEN IF RV, THEN SET .NOT.RV AND IF
C     NOT RV SET TO RV
C     OR AT A DUMMY SURFACE, N POS INTO NEG TH OR N NEG INTO POS TH
C     THEN IF RV, THEN SET .NOT.RV AND IF
          TESTLEN=R_Z-RR_Z
C     NOT RV SET TO RV
C     IF AT A DUMMY SURFACE, THE THICKNESS CHANGES SIGN, THEN THE
C     RAY GETS "REVERSED"
          IF(ALENS(3,R_I).GT.0.0D0.AND.ALENS(3,R_I-1).LT
     1    .0.0D0.AND.DUM(R_I).OR.ALENS(3,R_I).LT.0.0D0.AND.ALENS(3,R_I-1)
     2    .GT.0.0D0.AND.DUM(R_I)) THEN
              IF(RV) THEN
                  RV=.FALSE.
              ELSE
                  RV=.TRUE.
              END IF
          END IF
C     IF THE RAY HAS TRAVELED A NEG DIST WITH A POS DIRCOS
C     OR THE RAY HAS TRAVELED A POS DIST WITH A NEG DIR COS
C     AND THE RAY WAS NOT REVERESED, THEN SET IT TO "REVERSED"
C     RAY GETS "REVERSED"
          IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     2    RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.
     3    RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.
     4    SNINDX.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     5    SNINDX.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     6    SNINDX.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.
     7    SNINDX.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR) THEN
              RV=.TRUE.
          ELSE
              RV=.FALSE.
          END IF
          IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.RVSTART.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.RVSTART) THEN
              RV=.FALSE.
          END IF
          IF(RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     2    RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.
     3    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR) THEN
              RV=.FALSE.
              RVSTART=.FALSE.
          END IF
C       RAYTRACE THROUGH ASPHERE COMPLETED
          IF(R_I.EQ.NEWIMG) THEN
              R_L=OLDL
              R_M=OLDM
              R_N=OLDN
          END IF
          COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
          IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
          IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
          RAYCOD(1)=0
          RAYCOD(2)=R_I
          SPDCD1=RAYCOD(1)
          SPDCD2=R_I
          STOPP=0
          RETURN
      END
C SUB HITGRAZ.FOR
      SUBROUTINE HITGRAZ
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE HITGRAZ.FOR. THIS SUBROUTINE IMPLEMENTS
C       A GRAZING INCIDENCE RAY TRACE FOR GRAZING INCIDENCE,
C       REFLECTIVE SURFACES
C
          REAL*8 A,B,C,QQ,SIGNNU,CV,CC,
     1    TEST1,TEST2,NUSUBS,RR_N,TESTLEN
     3    ,Q,SIGNB,MAG,ARG,ZTEST,SNIND2
     4    ,FPX,FPY,FPZ,HV0,HV1,HV2,ZMIN,ZMAX,
     5    X1,X2,Y1,Y2,Z1,Z2,LN1,LN2,MN1,MN2,NN1,NN2
     6    ,SNINDX,HV_JK,RR_Z
C
          INTEGER ZPMIN,ZPMAX
C
C     HOE STUFF
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          PHASE=0.0D0
          RR_N=R_N
          HV0=0.0D0
          HV1=0.0D0
          HV2=0.0D0
          IF(WVN.EQ.1) WWVN=46
          IF(WVN.EQ.2) WWVN=47
          IF(WVN.EQ.3) WWVN=48
          IF(WVN.EQ.4) WWVN=49
          IF(WVN.EQ.5) WWVN=50
          IF(WVN.EQ.6) WWVN=71
          IF(WVN.EQ.7) WWVN=72
          IF(WVN.EQ.8) WWVN=73
          IF(WVN.EQ.9) WWVN=74
          IF(WVN.EQ.10) WWVN=75
          SNINDX=DABS(ALENS(WWVN,R_I-1))/ALENS(WWVN,R_I-1)
          SNIND2=DABS(ALENS(WWVN,R_I))/ALENS(WWVN,R_I)
C
C       SURFACE IS CONIC BY DEFINITION
          CV=ALENS(1,R_I)
          CC=ALENS(2,R_I)
          IF(CC.EQ.-1.0D0.AND.CV.GT.0.0D0) ZTEST=1.0D20
          IF(CC.EQ.-1.0D0.AND.CV.LT.0.0D0) ZTEST=-1.0D20
          IF(CC.NE.-1.0D0) ZTEST=1.0D0/((CC+1.0D0)*CV)
C
          RR_N=R_N
          RR_Z=R_Z
C
          IF(R_I.EQ.(NEWOBJ+1)) THEN
              R_X=R_XAIM
              R_Y=R_YAIM
              R_Z=R_ZAIM
C       JUST PROCEED WITH THE DIRECT INTERSECTION TO THE CONIC
          END IF
          NUSUBS=((ALENS((WWVN),(R_I-1)))/
     1    (ALENS((WWVN),R_I)))
          SIGNNU=DABS(NUSUBS)/NUSUBS
          NUSUBS=DABS(NUSUBS)
C
C       NOW INTERSECT THE SPHERE OR CONIC.
C       THE FOLLOWING CALCULATIONS ARE INTERMEDIATE STEPS:
C
          A=-(CV*((R_L**2)+(R_M**2)+((R_N**2)*
     1    (CC+1.0D0))))
          B=R_N-(CV*R_X*R_L)-(CV*R_Y*R_M)-
     1    ((CV*R_Z*R_N)
     1    *(CC+1.0D0))
          B=2.0D0*B
          C=-CV*((R_X**2)+(R_Y**2)+((R_Z**2)*
     1    (CC+1.0D0)))
          C=(C+(2.0D0*R_Z))
          IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
          IF(B.EQ.0.0D0) SIGNB=1.0D0
          IF(A.EQ.0.0D0) THEN
              HV0=-(C/B)
              INTERS=1
          ELSE
C       A NOT ZERO
              ARG=((B**2)-(4.0D0*A*C))
              IF(ARG.LT.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
C       ARG NOT ZERO
              END IF
              Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
              HV1=C/Q
              HV2=Q/A
              INTERS=2
              IF(HV2.EQ.0.0D0.OR.HV1.EQ.0.0D0) THEN
                  INTERS=1
                  IF(HV1.EQ.0.0D0.AND.HV2.NE.0.0D0) HV_JK=HV2
                  IF(HV1.NE.0.0D0.AND.HV2.EQ.0.0D0) HV_JK=HV1
                  IF(HV1.EQ.0.0D0.AND.HV2.EQ.0.0D0) HV_JK=0.0D0
                  HV1=HV_JK
                  HV2=0.0D0
              END IF
C
          END IF
          IF(INTERS.EQ.1) THEN
C       CASE OF ONLY ONE INTERSECTION POINT
              X1=(R_X+(HV0*R_L))
              Y1=(R_Y+(HV0*R_M))
              Z1=(R_Z+(HV0*R_N))
          END IF
          IF(INTERS.EQ.2) THEN
C       CASE OF TWO INTERSECTION POINTS
              X1=(R_X+(HV1*R_L))
              Y1=(R_Y+(HV1*R_M))
              Z1=(R_Z+(HV1*R_N))
              X2=(R_X+(HV2*R_L))
              Y2=(R_Y+(HV2*R_M))
              Z2=(R_Z+(HV2*R_N))
          END IF
C     FIX FOR HYPERBOLAS
          IF(CC.LT.-1.0D0) THEN
              ZMIN=Z2
              ZMAX=Z2
              ZPMIN=2
              ZPMAX=2
              IF(Z1.LT.Z2) ZMIN=Z1
              IF(Z1.LT.Z2) ZPMIN=1
              IF(Z1.GT.Z2) ZMAX=Z1
              IF(Z1.GT.Z2) ZPMAX=1
C     HYPERBOLA
C     CV POSITIVE
              IF(CV.GT.0.0D0) THEN
                  IF(ZMIN.LT.0.0D0) THEN
C     ONLY ONE INTERSECTION POINT
                      INTERS=1
                      IF(ZPMIN.EQ.1) THEN
                          X1=X2
                          Y1=Y2
                          Z1=Z2
C     LEAVE X1,Y1,Z1 ALONE
                      END IF
C     TWO POINTS
                  END IF
              ELSE
C    CV NEGATIVE
                  IF(ZMAX.GT.0.0D0) THEN
C     ONLY ONE INTERSECTION POINT
                      INTERS=1
                      IF(ZPMAX.EQ.1) THEN
                          X1=X2
                          Y1=Y2
                          Z1=Z2
C     LEAVE X1,Y1,Z1 ALONE
                      END IF
C     THERE ARE TWO POINTS
                  END IF
              END IF
C     NOT HYPERBOLA
          END IF
C
C
C       CALCULATE THE DIRECTION COSINES OF THE FIRST POINT
          QQ=1.0D0-((CC+1.0D0)*(CV**2)*((X1**2)+(Y1**2)))
          IF(QQ.LT.0.0D0) THEN
              QQ=-QQ
C       QQ NOT NEGATIVE
          END IF
          QQ=DSQRT(QQ)
          QQ=1.0D0+QQ
          IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                  CALL SHOWIT(1)
                  OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                  CALL SHOWIT(1)
              END IF
              RAYCOD(1)=1
              RAYCOD(2)=R_I
              SPDCD1=RAYCOD(1)
              SPDCD2=R_I
              STOPP=1
              RETURN
          END IF
          FPX=-((2.0D0*CV*X1*QQ*(QQ-1.0D0))+
     1    ((CV**3)*X1*((X1**2)+(Y1**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
          FPY=-((2.0D0*CV*Y1*QQ*(QQ-1.0D0))+
     1    ((CV**3)*Y1*((X1**2)+(Y1**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
          IF(CV.GT.0.0D0) THEN
C     USE INWARD NORMAL
              IF(CC.LT.-1.0D0) THEN
                  FPZ=1.0D0
              ELSE
                  IF(Z1.GT.ZTEST) FPZ=-1.0D0
                  IF(Z1.LT.ZTEST) FPZ=1.0D0
                  IF(Z1.EQ.ZTEST) FPZ=0.0D0
              END IF
          ELSE
C     CV NEG
C     USE OUTWARD NORMAL
              IF(CC.LT.-1.0D0) THEN
                  FPZ=1.0D0
              ELSE
                  IF(Z1.LT.ZTEST) FPZ=-1.0D0
                  IF(Z1.GT.ZTEST) FPZ=1.0D0
                  IF(Z1.EQ.ZTEST) FPZ=0.0D0
              END IF
          END IF
C
          MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
          LN1=FPX/MAG
          MN1=FPY/MAG
          NN1=FPZ/MAG
C       NOW THE DIRECTION COSINES OF THE SECOND POINT IF IT EXISTS
          IF(INTERS.EQ.2) THEN
C       FIRST THE DIRECTION COSINES OF THE SURFACE NORMAL
              QQ=1.0D0-((CC+1.0D0)*(CV**2)*((X2**2)+(Y2**2)))
              IF(QQ.LT.0.0D0) THEN
                  QQ=-QQ
C       QQ NOT NEG
              END IF
              QQ=DSQRT(QQ)
              QQ=1.0D0+QQ
              IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
              END IF
              FPX=-((2.0D0*CV*X2*QQ*(QQ-1.0D0))+
     1        ((CV**3)*X2*((X2**2)+(Y2**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
              FPY=-((2.0D0*CV*Y2*QQ*(QQ-1.0D0))+
     1        ((CV**3)*Y2*((X2**2)+(Y2**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
              IF(CV.GT.0.0D0) THEN
C     USE INWARD NORMAL
                  IF(CC.LT.-1.0D0) THEN
                      FPZ=1.0D0
                  ELSE
                      IF(Z2.GT.ZTEST) FPZ=-1.0D0
                      IF(Z2.LT.ZTEST) FPZ=1.0D0
                      IF(Z2.EQ.ZTEST) FPZ=0.0D0
                  END IF
              ELSE
C     CV NEG
C     USE OUTWARD NORMAL
                  IF(CC.LT.-1.0D0) THEN
                      FPZ=1.0D0
                  ELSE
                      IF(Z2.LT.ZTEST) FPZ=-1.0D0
                      IF(Z2.GT.ZTEST) FPZ=1.0D0
                      IF(Z2.EQ.ZTEST) FPZ=0.0D0
                  END IF
              END IF
C
              MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
              LN2=FPX/MAG
              MN2=FPY/MAG
              NN2=FPZ/MAG
C       NOT TWO INTERSECTION POINTS
          END IF
C       NOW FIGURE OUT WHICH INTERSECTION POINT TO USE
C       DOT THE INCOMMING DIR COS WITH EACH SURFACE NORMAL
C
          IF(INTERS.EQ.2) THEN
C       THERE ARE TWO INTERSECTION POINTS
              TEST1=((R_L*LN1)+(R_M*MN1)+(R_N*NN1))
              TEST2=((R_L*LN2)+(R_M*MN2)+(R_N*NN2))
              IF(POSRAY.AND..NOT.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  ELSE
C       TEST1 NOT >0
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  END IF
                  GO TO 200
C     NOT POSRAY
              END IF
              IF(POSRAY.AND.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  ELSE
C       TEST1 NOT >0
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  END IF
                  GO TO 200
C     NOT POSRAY
              END IF
              IF(.NOT.POSRAY.AND..NOT.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  ELSE
C       TEST1 NOT >0
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  END IF
                  GO TO 200
C     POSRAY
              END IF
              IF(.NOT.POSRAY.AND.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  ELSE
C       TEST1 NOT >0
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  END IF
                  GO TO 200
C     POSRAY
              END IF
          ELSE
C       NOT TWO INTERSECTIONS
C       JUST ONE INTERSECTION
              R_X=X1
              R_Y=Y1
              R_Z=Z1
              LN=LN1
              MN=MN1
              NN=NN1
C
          END IF
 200      CONTINUE
C
C       NOW WE KNOW THE INTERSECTION POINT IF THE FOURIER LEGENDRE TERMS
C       ARE ALL ZERO. WE ALSO HAVE THE SURFACE NORMALS
C
C       NOW CALCULATE THE RAY INTERSECTION WITH THE DEFORMED SURFACE
C       USING A CALL TO SUBROUTINE NR5. THIS ALSO GIVES THE DIRECTION
C       COSINES OF THE SURFACE NORMAL TO THE DEFORMED SURFACE.
C       ONLY IF THERE ARE NON-ZERO FOURIER-LEGENDRE COEFFS
C
          IF(FTFL01(4,R_I).NE.0.0D0.OR.FTFL01(5,R_I).NE.0.0D0
     1    .OR.FTFL01(6,R_I).NE.0.0D0.OR.FTFL01(7,R_I).NE.0.0D0
     1    .OR.FTFL01(8,R_I).NE.0.0D0.OR.FTFL01(9,R_I).NE.0.0D0
     1    .OR.FTFL01(10,R_I).NE.0.0D0.OR.FTFL01(11,R_I).NE.0.0D0
     1    .OR.FTFL01(12,R_I).NE.0.0D0.OR.FTFL01(13,R_I).NE.0.0D0
     1    .OR.FTFL01(14,R_I).NE.0.0D0.OR.FTFL01(15,R_I).NE.0.0D0
     1    .OR.FTFL01(16,R_I).NE.0.0D0.OR.FTFL01(17,R_I).NE.0.0D0
     1    .OR.FTFL01(18,R_I).NE.0.0D0) THEN
              INR=ALENS(76,R_I)
              CALL NR5
          END IF
C
          IF(R_Z.LT.FTFL01(1,R_I).OR.R_Z.GT.FTFL01(2,R_I)) THEN
C     MISSED SURFACE
              RAYCOD(1)=13
              RAYCOD(2)=R_I
              SPDCD1=RAYCOD(1)
              SPDCD2=R_I
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                  CALL SHOWIT(1)
                  OUTLYNE='RAY MISSED GRAZING INCIDENCE SURFACE SECTION'
                  CALL SHOWIT(1)
              END IF
              STOPP=1
          END IF
          IF(STOPP.EQ.1) RETURN
C
C       THE RETURNED VALUES OF X,Y,Z ARE THE CORRECT INTERSECTIONS
C       TO WITHIN THE 1.0D-25 OR SURTOL WHICHEVER IS THE SMALLER VALUE
C
C       THE COSINE SQUARED OF THE ANGLE OF INCIDENCE IS JUST
          COSI=(LN*R_L)+(MN*R_M)+(NN*R_N)
          IF(COSI.LT.-1.0D0) COSI=-1.0D0
          IF(COSI.GT.+1.0D0) COSI=+1.0D0
C
C       REFLECTION ALWAYS
          R_L=(R_L-((2.0D0*COSI)*LN))
          R_M=(R_M-((2.0D0*COSI)*MN))
          R_N=(R_N-((2.0D0*COSI)*NN))
C       RE-NORMALIZE
          MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
          R_L=R_L/MAG
          R_M=R_M/MAG
          R_N=R_N/MAG
          COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
          IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
          IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
C     IF THE RAY HAS TRAVELED A NEG DIST WITH A POS DIRCOS
C     OR THE RAY HAS TRAVELED A POS DIST WITH A NEG DIR COS
C     AND THE RAY WAS NOT REVERESED, THEN SET IT TO "REVERSED"
C     RAY GETS "REVERSED"
          TESTLEN=R_Z-RR_Z
          IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     2    RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.
     3    RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.
     4    SNINDX.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     5    SNINDX.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     6    SNINDX.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.
     7    SNINDX.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR) THEN
              RV=.TRUE.
          ELSE
              RV=.FALSE.
          END IF
          IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.RVSTART.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.RVSTART) THEN
              RV=.FALSE.
          END IF
          IF(RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.
     1    RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.
     2    RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.
     3    RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR) THEN
              RV=.FALSE.
              RVSTART=.FALSE.
          END IF
C       RAYTRACE OFF A GRAZING SURFACE COMPLETED
          IF(R_I.EQ.NEWIMG) THEN
              R_L=OLDL
              R_M=OLDM
              R_N=OLDN
          END IF
          COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
          IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
          IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
          STOPP=0
          RAYCOD(1)=0
          RAYCOD(2)=R_I
          SPDCD1=RAYCOD(1)
          SPDCD2=R_I
          RETURN
      END
C**********************************************************************
      SUBROUTINE ARRAYIN_FIX(N_X,N_Y)
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          REAL*8 X,Y
          INTEGER I,N_X,N_Y
          I=R_I
          X=R_X
          Y=R_Y
          CALL POSARRAY1(I,X,Y,N_X,N_Y)
          R_X=X
          R_Y=Y

          RETURN
      END
C**********************************************************************
      SUBROUTINE ARRAYOUT_FIX(N_X,N_Y)
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER I,N_X,N_Y
          REAL*8 X,Y
          I=R_I
          X=R_X
          Y=R_Y
          CALL POSARRAY2(I,X,Y,N_X,N_Y)
          R_X=X
          R_Y=Y
          RETURN
      END
C**********************************************************************
      SUBROUTINE POSARRAY1(I,X,Y,N_X,N_Y)
          IMPLICIT NONE
          REAL*8 X,Y,DX,DY,XWORKING,YWORKING,SGNX,SGNY
          INTEGER I,N_X,N_Y
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C       X AND Y  PASS IN AS THE X AND Y COORDINATES AND PASS BACK AS THE X AND Y
C       COORDINATES AT A SINGLE LENSLET
          DX=ALENS(131,I)
          DY=ALENS(132,I)
          IF(ALENS(133,I).EQ.-1.0D0) THEN
C       ODD
!        N_X=DBLE(NINT(X/DX))
!        N_Y=DBLE(NINT(Y/DY))
              N_X=NINT(X/DX)
              N_Y=NINT(Y/DY)

              XWORKING=X-(N_X*DX)
              YWORKING=Y-(N_Y*DY)
              X=XWORKING
              Y=YWORKING
          END IF
          IF(ALENS(133,I).EQ.1.0D0) THEN
C       EVEN
              IF(X.EQ.0.0D0) THEN
                  SGNX=1.0D0
              ELSE
                  SGNX=X/DABS(X)
              END IF
              IF(Y.EQ.0.0D0) THEN
                  SGNY=1.0D0
              ELSE
                  SGNY=Y/DABS(Y)
              END IF
              N_X=INT((DBLE(INT(X/DX))*2.0D0)+SGNX)
              N_Y=INT((DBLE(INT(Y/DY))*2.0D0)+SGNY)
              XWORKING=X-(N_X*DX/2.0D0)
              YWORKING=Y-(N_Y*DY/2.0D0)
              X=XWORKING
              Y=YWORKING
          END IF
          RETURN
      END
C**********************************************************************
      SUBROUTINE POSARRAY2(I,X,Y,N_X,N_Y)
          IMPLICIT NONE
          REAL*8 X,Y,DX,DY,XWORKING,YWORKING
          INTEGER I,N_X,N_Y
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C       INVERSE OF POSARRAY1
          DX=ALENS(131,I)
          DY=ALENS(132,I)
          IF(ALENS(133,I).EQ.-1.0D0) THEN
C       ODD
              XWORKING=X+(N_X*DX)
              YWORKING=Y+(N_Y*DY)
              X=XWORKING
              Y=YWORKING
          END IF
          IF(ALENS(133,I).EQ.1.0D0) THEN
C       EVEN
              XWORKING=X+(N_X*DX/2.0D0)
              YWORKING=Y+(N_Y*DY/2.0D0)
              X=XWORKING
              Y=YWORKING
          END IF
          RETURN
      END
C**********************************************************************
C SUB HITANA_old.FOR
      SUBROUTINE HITANA_old(OR_N,OR_Z)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE HITANA.FOR. THIS SUBROUTINE IMPLEMENTS
C       SURFACE INTERSECTIONS TO ANAMORPHICS IN RAYTRACING.
C
          REAL*8 A,B,C,QQ,SIGNNU,CV,NUSUBS,OR_N,OR_Z,
     1    TEST1,TEST2,RR_N,ZTEST,RR_Z
     3    ,Q,SIGNB,MAG,ARG,SNINDX,SNIND2
     4    ,FPX,FPY,FPZ,HV0,HV1,HV2,
     5    X1,X2,Y1,Y2,Z1,Z2,LN1,LN2,MN1,MN2,NN1,NN2
C
C     DIFFRACTION GRATING STUFF
C
C     HOE STUFF
          REAL*8 RD,R1,R2

          LOGICAL ERR
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          PHASE=0.0D0
          HV0=0.0D0
          HV1=0.0D0
          HV2=0.0D0
          IF(WVN.EQ.1) WWVN=46
          IF(WVN.EQ.2) WWVN=47
          IF(WVN.EQ.3) WWVN=48
          IF(WVN.EQ.4) WWVN=49
          IF(WVN.EQ.5) WWVN=50
          IF(WVN.EQ.6) WWVN=71
          IF(WVN.EQ.7) WWVN=72
          IF(WVN.EQ.8) WWVN=73
          IF(WVN.EQ.9) WWVN=74
          IF(WVN.EQ.10) WWVN=75
          SNINDX=DABS(ALENS(WWVN,R_I-1))/ALENS(WWVN,R_I-1)
          SNIND2=DABS(ALENS(WWVN,R_I))/ALENS(WWVN,R_I)
          R1=0.0D0
          R2=0.0D0
          IF(ALENS(1,R_I).NE.0.0D0) R1=1.0D0/ALENS(1,R_I)
          IF(ALENS(24,R_I).NE.0.0D0) R2=1.0D0/ALENS(24,R_I)
          RD=(R1+R2)/2.0D0
          CV=0.0D0
          IF(RD.NE.0.0D0) CV=1.0D0/RD
C
          IF(CV.NE.0.0D0) ZTEST=1.0D0/CV
C
          RR_N=R_N
          RR_Z=R_Z
          OR_Z=RR_Z
          OR_N=RR_N
C
C       SURFACE IS AN ANAMORPH
C
          IF(R_I.EQ.(NEWOBJ+1)) THEN
              R_X=R_XAIM
              R_Y=R_YAIM
              R_Z=R_ZAIM
C       JUST PROCEED WITH THE DIRECT INTERSECTION TO THE SPHERE
          END IF
          NUSUBS=((ALENS((WWVN),(R_I-1)))/
     1    (ALENS((WWVN),R_I)))
          SIGNNU=DABS(NUSUBS)/NUSUBS
          NUSUBS=DABS(NUSUBS)
C
C       NOW INTERSECT THE LARGEST SPHERE.
C       THE FOLLOWING CALCULATIONS ARE INTERMEDIATE STEPS:
C
          A=-(CV*((R_L**2)+(R_M**2)+((R_N**2))))
          B=R_N-(CV*R_X*R_L)-(CV*R_Y*R_M)-
     1    ((CV*R_Z*R_N))
          B=2.0D0*B
          C=-CV*((R_X**2)+(R_Y**2)+((R_Z**2)))
          C=(C+(2.0D0*R_Z))
          IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
          IF(B.EQ.0.0D0) SIGNB=1.0D0
          IF(A.EQ.0.0D0) THEN
              HV0=-(C/B)
              INTERS=1
          ELSE
C       A NOT ZERO
              ARG=((B**2)-(4.0D0*A*C))
              IF(ARG.LT.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
C       ARG NOT ZERO
              END IF
              Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
              HV1=C/Q
              HV2=Q/A
              INTERS=2
          END IF
          IF(INTERS.EQ.1) THEN
C       CASE OF ONLY ONE INTERSECTION POINT
              X1=(R_X+(HV0*R_L))
              Y1=(R_Y+(HV0*R_M))
              Z1=(R_Z+(HV0*R_N))
          END IF
          IF(INTERS.EQ.2) THEN
C       CASE OF TWO INTERSECTION POINTS
              X1=(R_X+(HV1*R_L))
              Y1=(R_Y+(HV1*R_M))
              Z1=(R_Z+(HV1*R_N))
              X2=(R_X+(HV2*R_L))
              Y2=(R_Y+(HV2*R_M))
              Z2=(R_Z+(HV2*R_N))
          END IF
C
C       CALCULATE THE DIRECTION COSINES OF THE FIRST POINT
          QQ=1.0D0-((CV**2)*((X1**2)+(Y1**2)))
          IF(QQ.LT.0.0D0) THEN
              QQ=-QQ
C       QQ NOT NEGATIVE
          END IF
          QQ=DSQRT(QQ)
          QQ=1.0D0+QQ
          IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                  CALL SHOWIT(1)
                  OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                  CALL SHOWIT(1)
              END IF
              RAYCOD(1)=1
              RAYCOD(2)=R_I
              SPDCD1=RAYCOD(1)
              SPDCD2=R_I
              STOPP=1
              RETURN
          END IF
          FPX=-((2.0D0*CV*X1*QQ*(QQ-1.0D0))+
     1    ((CV**3)*X1*((X1**2)+(Y1**2))))/((QQ-1.0D0)*(QQ**2))
          FPY=-((2.0D0*CV*Y1*QQ*(QQ-1.0D0))+
     1    ((CV**3)*Y1*((X1**2)+(Y1**2))))/((QQ-1.0D0)*(QQ**2))
C
          IF(CV.GT.0.0D0) THEN
C     USE INWARD NORMAL
              IF(Z1.GT.ZTEST) FPZ=-1.0D0
              IF(Z1.LT.ZTEST) FPZ=1.0D0
              IF(Z1.EQ.ZTEST) FPZ=0.0D0
          ELSE
C     CV NEG
C     USE OUTWARD NORMAL
              IF(Z1.LT.ZTEST) FPZ=-1.0D0
              IF(Z1.GT.ZTEST) FPZ=1.0D0
              IF(Z1.EQ.ZTEST) FPZ=0.0D0
          END IF
C
          MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
          LN1=FPX/MAG
          MN1=FPY/MAG
          NN1=FPZ/MAG
C       NOW THE DIRECTION COSINES OF THE SECOND POINT IF IT EXISTS
          IF(INTERS.EQ.2) THEN
C       FIRST THE DIRECTION COSINES OF THE SURFACE NORMAL
              QQ=1.0D0-((CV**2)*((X2**2)+(Y2**2)))
              IF(QQ.LT.0.0D0) THEN
                  QQ=-QQ
C       QQ NOT NEG
              END IF
              QQ=DSQRT(QQ)
              QQ=1.0D0+QQ
              IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
                  IF(MSG) THEN
                      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                      CALL SHOWIT(1)
                      OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                      CALL SHOWIT(1)
                  END IF
                  RAYCOD(1)=1
                  RAYCOD(2)=R_I
                  SPDCD1=RAYCOD(1)
                  SPDCD2=R_I
                  STOPP=1
                  RETURN
              END IF
              FPX=-((2.0D0*CV*X2*QQ*(QQ-1.0D0))+
     1        ((CV**3)*X2*((X2**2)+(Y2**2))))/((QQ-1.0D0)*(QQ**2))
              FPY=-((2.0D0*CV*Y2*QQ*(QQ-1.0D0))+
     1        ((CV**3)*Y2*((X2**2)+(Y2**2))))/((QQ-1.0D0)*(QQ**2))
              IF(CV.GT.0.0D0) THEN
C     USE INWARD NORMAL
                  IF(Z2.GT.ZTEST) FPZ=-1.0D0
                  IF(Z2.LT.ZTEST) FPZ=1.0D0
                  IF(Z2.EQ.ZTEST) FPZ=0.0D0
              ELSE
C     CV NEG
C     USE OUTWARD NORMAL
                  IF(Z2.LT.ZTEST) FPZ=-1.0D0
                  IF(Z2.GT.ZTEST) FPZ=1.0D0
                  IF(Z2.EQ.ZTEST) FPZ=0.0D0
              END IF
C
              write(outlyne,*) fpx,fpy,fpz
              call showit(1)
              MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
              LN2=FPX/MAG
              MN2=FPY/MAG
              NN2=FPZ/MAG
C       NOT TWO INTERSECTION POINTS
          END IF
C       NOW FIGURE OUT WHICH INTERSECTION POINT TO USE
C       DOT THE INCOMMING DIR COS WITH EACH SURFACE NORMAL
C
          IF(INTERS.EQ.2) THEN
C       THERE ARE TWO INTERSECTION POINTS
              TEST1=((R_L*LN1)+(R_M*MN1)+(R_N*NN1))
              TEST2=((R_L*LN2)+(R_M*MN2)+(R_N*NN2))
              IF(POSRAY.AND..NOT.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  ELSE
C       TEST1 NOT >0
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  END IF
                  GO TO 200
C     NOT POSRAY
              END IF
              IF(POSRAY.AND.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  ELSE
C       TEST1 NOT >0
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  END IF
                  GO TO 200
C     NOT POSRAY
              END IF
              IF(.NOT.POSRAY.AND..NOT.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  ELSE
C       TEST1 NOT >0
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  END IF
                  GO TO 200
C     POSRAY
              END IF
              IF(.NOT.POSRAY.AND.REVSTR) THEN
C     FIRST  INTERSECTION POINT = TEST()>0
C     SECOND INTERSECTION POINT = TEST()<0
                  IF(TEST1.GT.0.0D0) THEN
                      R_X=X1
                      R_Y=Y1
                      R_Z=Z1
                      LN=LN1
                      MN=MN1
                      NN=NN1
                      SEC=1
                  ELSE
C       TEST1 NOT >0
                      R_X=X2
                      R_Y=Y2
                      R_Z=Z2
                      LN=LN2
                      MN=MN2
                      NN=NN2
                      SEC=2
                  END IF
                  GO TO 200
C     POSRAY
              END IF
          ELSE
C       NOT TWO INTERSECTIONS
C       JUST ONE INTERSECTION
              R_X=X1
              R_Y=Y1
              R_Z=Z1
              LN=LN1
              MN=MN1
              NN=NN1
          END IF
 200      CONTINUE
C
C       NOW CALCULATE THE RAY INTERSECTION WITH THE DEFORMED TORIC
C       USING A CALL TO SUBROUTINE NR3
C       THIS ALSO DEALS WITH SPECIAL SURFACE TYPES:
C                       1, 2 AND 3
C
          INR=ALENS(76,R_I)
          ERR=.FALSE.
          CALL NR3(ERR)
          IF(ERR) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                  CALL SHOWIT(1)
                  OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                  CALL SHOWIT(1)
              END IF
              RAYCOD(1)=1
              RAYCOD(2)=R_I
              SPDCD1=RAYCOD(1)
              SPDCD2=R_I
              STOPP=1
              RETURN
          END IF
          RETURN
C
      END
C**********************************************************************
C SUB HITANA.FOR
      SUBROUTINE HITANA(OR_N,OR_Z)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE HITANA.FOR. THIS SUBROUTINE IMPLEMENTS
C       SURFACE INTERSECTIONS TO ANAMORPHICS IN RAYTRACING.
C
          REAL*8 SIGNNU,CV,
     1    NUSUBS,OR_N,OR_Z,
     1    RR_N,ZTEST,RR_Z
     3    ,SNINDX,SNIND2
     4    ,HV0,HV1,HV2
C
C     HOE STUFF
          REAL*8 RD,R1,R2
C
          LOGICAL ERR
C
          INTEGER SPDCD1,SPDCD2
          COMMON/SPRA2/SPDCD1,SPDCD2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER WWVN
          COMMON/WVPASS/WWVN
          PHASE=0.0D0
          HV0=0.0D0
          HV1=0.0D0
          HV2=0.0D0
          IF(WVN.EQ.1) WWVN=46
          IF(WVN.EQ.2) WWVN=47
          IF(WVN.EQ.3) WWVN=48
          IF(WVN.EQ.4) WWVN=49
          IF(WVN.EQ.5) WWVN=50
          IF(WVN.EQ.6) WWVN=71
          IF(WVN.EQ.7) WWVN=72
          IF(WVN.EQ.8) WWVN=73
          IF(WVN.EQ.9) WWVN=74
          IF(WVN.EQ.10) WWVN=75
          SNINDX=DABS(ALENS(WWVN,R_I-1))/ALENS(WWVN,R_I-1)
          SNIND2=DABS(ALENS(WWVN,R_I))/ALENS(WWVN,R_I)
          R1=0.0D0
          R2=0.0D0
          IF(ALENS(1,R_I).NE.0.0D0) R1=1.0D0/ALENS(1,R_I)
          IF(ALENS(24,R_I).NE.0.0D0) R2=1.0D0/ALENS(24,R_I)
          RD=(R1+R2)/2.0D0
          CV=0.0D0
          IF(RD.NE.0.0D0) CV=1.0D0/RD
C
          IF(CV.NE.0.0D0) ZTEST=1.0D0/CV
C
          RR_N=R_N
          RR_Z=R_Z
          OR_Z=RR_Z
          OR_N=RR_N
C
C       SURFACE IS AN ANAMORPH
C
          IF(R_I.EQ.(NEWOBJ+1)) THEN
              R_X=R_XAIM
              R_Y=R_YAIM
              R_Z=R_ZAIM
C       JUST PROCEED WITH THE DIRECT INTERSECTION TO THE ANAMORPH
          END IF
          NUSUBS=((ALENS((WWVN),(R_I-1)))/
     1    (ALENS((WWVN),R_I)))
          SIGNNU=DABS(NUSUBS)/NUSUBS
          NUSUBS=DABS(NUSUBS)
C
C       NOW CALCULATE THE RAY INTERSECTION WITH THE DEFORMED TORIC
C       USING A CALL TO SUBROUTINE NR3
C       THIS ALSO DEALS WITH SPECIAL SURFACE TYPES:
C                       1, 2 AND 3
C
          INR=ALENS(76,R_I)
          ERR=.FALSE.
          CALL NR3(ERR)
          IF(ERR) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',R_I
                  CALL SHOWIT(1)
                  OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
                  CALL SHOWIT(1)
              END IF
              RAYCOD(1)=1
              RAYCOD(2)=R_I
              SPDCD1=RAYCOD(1)
              SPDCD2=R_I
              STOPP=1
              RETURN
          END IF
          RETURN
C
      END
