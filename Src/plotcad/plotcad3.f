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

C       THIRD FILE OF PLOT/CAD ROUTINES

C SUB PLTORG.FOR
      SUBROUTINE PLTORG
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT ORIGIN COMMAND AT THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(WC.EQ.'PLOT'.AND.WQ.EQ.'ORIGIN') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT ORIGIN" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,873)ORX,ORY
                  CALL SHOWIT(1)
 873              FORMAT('CURRENT PLOT ORIGIN IS AT:'/
     1                   '                                           '/
     1                   '        X = ',I5,' DEVICE COORDINATE UNITS'/
     2                   '        Y = ',I5,' DEVICE COORDINATE UNITS')
                  RETURN
              ELSE
              END IF
              IF(DF1.EQ.1) W1=0.0D0
              IF(DF2.EQ.1) W2=0.0D0
              IF(INT(W1).GT.10000) THEN
                  OUTLYNE=
     1            'THE MAXIMUM X-COORDINATE VALUE OF 10000 WAS EXCEEDED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W2).GT.7000) THEN
                  OUTLYNE=
     1            'THE MAXIMUM Y-COORDINATE VALUE OF 7000 WAS EXCEEDED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              ORX=INT(W1)
              ORY=INT(W2)
C       MOVE PEN TO ORIGIN WITH PEN UP
              PENSTA=0
              XPEN=ORX
              YPEN=ORY
C     ARGUMENTS ARE PASSED TO PENMV VIA NAMED COMMONS IN DATLEN.FOR
              CALL PENMV
              RETURN
          ELSE
C       NOT PLOT ORIGIN
          END IF
          RETURN
      END


C SUB PLTNAM.FOR
      SUBROUTINE PLTNAM
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE "PLOT NAME" COMMAND AT THE CMD LEVEL
C
          CHARACTER BL10*10
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          BL10='          '
C
          IF(WQ.EQ.'NAME') THEN
C       SET PLOT NAME
C       CHECK SYNTAX
              IF(SN.EQ.1) THEN
                  OUTLYNE='"PLOT NAME" TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       STI
              IF(STI.EQ.1) THEN
                  IF(PPLI(1:10).EQ.BL10) THEN
                      OUTLYNE='THE CURRENT PLOT NAME IS BLANK'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C       PRINT OUT THE CURRENT PLOT NAME
                      WRITE(OUTLYNE,16) PPLI(1:60)
                      CALL SHOWIT(0)
 16                   FORMAT(A60)
                  END IF
                  RETURN
              ELSE
C       EXPLICT INPUT EXISTS
              END IF
C     SET THE PLOT NAME
C
              PPLI(1:60)=WS(1:60)
C
          ELSE
          END IF
          RETURN
      END


C SUB PLTDOTF.FOR
      SUBROUTINE PLTDOTF
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO PLOT DOTF AT THE CMD LEVEL
C
          CHARACTER BL20*20,NNTT1*99,CRANGE*8,LABX*40,LABY*40
     1    ,DTY*10,TMY*8,UNN1*9,BLNOTE*80,B*80
C
          REAL DDTAF(1:101),DDTAM(1:101,0:21),DDTAP(1:101,0:21)
     1    ,ADDTAF(1:101)
     2    ,RDELA,RDELB,RDELC,LLIM,ULIM
     3    ,DELX1,ADDTAM1(1:101),ADDTAM2(1:101)
C
          REAL LDTAF(1:11),LDTAM1(1:11),LDTAM2(1:11)
C
          REAL*8 FLDCODE(1:2,0:10),MAXFREQ
          CHARACTER FLDUNIT(0:10)*3
          COMMON/FLDLAB/FLDCODE,FLDUNIT
C
          REAL*8 RANGE1,CUTFRX,CUTFRY
C
          LOGICAL ERROR1,EXTDMTF1,EXTDMTF2
C
          LOGICAL MULTIOTF
          COMMON/OTFMULTI/MULTIOTF
C
          COMMON/DMTFEXT/EXTDMTF1,EXTDMTF2
C
          INTEGER COLPAS,IV,WVWT,I,N,DORI1,DORI2,NT1ANG,NT1SIZ,DFLAG
C
          INTEGER KK,JK_NN,MYJK,NX,NY,J,K,L,CT,LNTP
C
          EXTERNAL WVWT
C
          COMMON/DOTFPAS/DDTAF,DDTAM,DDTAP,N,DORI1,CUTFRX,CUTFRY,
     1    DORI2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
          INCLUDE 'datspd.inc'
C
          MYJK=0

C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"PLTDOTF" PLOTS EXISTING DOTF VALUES'
              CALL SHOWIT(1)
              RETURN
          ELSE
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"PLTDOTF" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'LEICA') THEN
              IF(DF1.EQ.0) THEN
                  DF1=1
                  S1=0
                  W1=0.0D0
                  OUTLYNE=
     1            '"PLTDOTF LEICA" ONLY TAKES NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='"MAX-FREQUENCY" INPUT IGNORED'
                  CALL SHOWIT(1)
              END IF
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'LEICA') THEN
              OUTLYNE=
     1        '"PLTDOTF" ONLY TAKES THE OPTIONAL QUALIFIER WORD "LEICA"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'LEICA') THEN
              IF(DF1.EQ.0) THEN
                  DF1=1
                  S1=0
                  W1=0.0D0
              END IF
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLTDOTF" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C
          IF(.NOT.EXTDMTF1) THEN
              OUTLYNE=
     1        'DOTF VALUES DO NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(N.LT.10) THEN
              OUTLYNE=
     1        'LESS THAN 10 DOTF VALUES IN TABLE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(CFLDCNT.LT.3.AND.WQ.EQ.'LEICA') THEN
              OUTLYNE=
     1        '"LEICA" TYPE PLOTS REQUIRE A MINIMUM OF 3 FOV POSITIONS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(N.GT.10.AND.WQ.EQ.'LEICA') THEN
              OUTLYNE=
     1        'WARNING:'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"LEICA" TYPE PLOTS WILL ONLY DISPLAY DATA FOR THE FIRST 10'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"NON-ZERO FREQUENCIES DISPLAYED WITH THE LAST "DOTF" COMMAND'
              CALL SHOWIT(1)
          END IF
          IF(DF1.EQ.0) THEN
C     EXPLICIT ENDING FREQUENCY
              IF(REAL(W1).GT.DDTAF(N+1)) ULIM=DDTAF(N+1)
              IF(W1.LE.0.0D0) THEN
                  OUTLYNE=
     1            'ENDING FREQUENCY MUST BE GREATER THAN 0.0'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     USE REAL(W1) AS ULIM
              ULIM=REAL(W1)
          END IF
          IF(DF1.EQ.1) ULIM=DDTAF(N+1)
C
          IF(DF2.EQ.1.OR.DF2.EQ.0.AND.W2.EQ.0.0D0) DFLAG=0
          IF(DF2.EQ.0.AND.W2.NE.0.0D0) DFLAG=1
C     GENERATE GRAPHIC
C     DO A PLOT NEW
          IF(SQ.EQ.1) THEN
              LLIM=REAL(DSQRT((CFLDS(2,1)**2)+(CFLDS(1,1)**2)))
              ULIM=REAL(DSQRT((CFLDS(2,CFLDCNT)**2)+(CFLDS(1,CFLDCNT)**2)))
              JK_NN=CFLDCNT
          END IF
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
          CALL PLTDEV
          GRASET=.TRUE.
          PLEXIS=.TRUE.
C     SET LETTER SIZE AND ANGLE
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
          CALL PLOTBOX
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
          NT1SIZ=1
          NT1ANG=0
          IF(STMPT) CALL MYTIME(TMY)
          IF(STMPD) CALL MYDATE(DTY)
          IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
          IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
          IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
          IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_JUSTSTRING(200,500,NNTT1(1:80),NT1ANG,NT1SIZ,3)
          ELSE
C     LI BLANK, NOT ACTION
          END IF
C     DO THE PLOTTING OF THE RSPH STATUS
C     AT X=200, Y=250
          NT1SIZ=1
          NT1ANG=0
          IF(SYSTEM1(30).LE.2.0D0) THEN
              IF(REFLOC.EQ.1)
     1        NNTT1(1:80)='IMAGE POINT LOCATED AT SPOT CENTROID         '
              IF(REFLOC.EQ.3)
     1        NNTT1(1:80)='IMAGE POINT LOCATED TO REMOVE TILT           '
              IF(REFLOC.EQ.4)
     1        NNTT1(1:80)='IMAGE POINT LOCATED TO REMOVE TILT AND FOCUS '
          ELSE
              NNTT1(1:80)='IMAGE POINT LOCATED ON THE CHIEF RAY (AFOCAL)'
          END IF
          CALL MY_JUSTSTRING(200,250,NNTT1(1:48),NT1ANG,NT1SIZ,3)
C
          IF(.NOT.MULTIOTF) THEN
              CALL MY_PLOT(200,6725,0,0,0,10000,0,7000)
              CALL MY_PLOT(1200,6725,1,0,0,10000,0,7000)
              IF(DORI1.EQ.0) THEN
                  NNTT1=
     1            'VERTICAL TARGET BARS'
              END IF
              IF(DORI1.EQ.1) THEN
                  NNTT1=
     1            'HORIZONTAL TARGET BARS'
              END IF
C
              CALL MY_JUSTSTRING(1400,6700,NNTT1(1:22),NT1ANG,NT1SIZ,3)
C

              IF(EXTDMTF2) THEN
                  CALL MY_PLOT(200,6525,0,0,0,10000,0,7000)
                  CALL MY_PLOT(1200,6525,1,3,0,10000,0,7000)
                  IF(DORI2.EQ.0) THEN
                      NNTT1=
     1                'VERTICAL TARGET BARS'
                  END IF
                  IF(DORI2.EQ.1) THEN
                      NNTT1=
     1                'HORIZONTAL TARGET BARS'
                  END IF
C
                  CALL MY_JUSTSTRING(1400,6500,NNTT1(1:22),NT1ANG,NT1SIZ,3)
              END IF
          END IF
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
C
C       LOWER AND LIMITS OF FREQ ARE 0 AND DDTA(N+1)
C       NOW DETERMINE THE LOWER AND UPPER X LIMITS OF THE PLOT
          LLIM=0.0
C       PROCEED
C
C       PLOT THE AXES
C
C       CORRECT SCALING IS IMPORTANT
C
          RDELA=ULIM
          RDELC=1.0
          RDELB=0.10
C
          IF(SQ.EQ.0) THEN
C     NON-LEICA PLOTS
              IF(SPACEBALL.EQ.1.AND.NEAR.OR.
     1        SPACEBALL.EQ.2.AND.SYSTEM1(30).LE.2.0D0) THEN
                  LABX='SPATIAL FREQUENCY (lp/mm)             '
                  NX=25
              END IF
              IF(SPACEBALL.EQ.1.AND..NOT.NEAR.OR.
     1        SPACEBALL.EQ.2.AND.SYSTEM1(30).GT.2.0D0) THEN
                  LABX='SPATIAL FREQUENCY (lp/mrad)             '
                  NX=27
              END IF
          ELSE
C     LEICA TYPE PLOTS
              IF(NEAR) THEN
                  IF(SYSTEM1(6).EQ.1.0D0) LABX='OBJECT HEIGHT (in)'
                  IF(SYSTEM1(6).EQ.2.0D0) LABX='OBJECT HEIGHT (cm)'
                  IF(SYSTEM1(6).EQ.3.0D0) LABX='OBJECT HEIGHT (mm)'
                  IF(SYSTEM1(6).EQ.4.0D0) LABX='OBJECT HEIGHT (m) '
                  NX=18
              ELSE
                  LABX='OBJECT FIELD OF VIEW (degrees)'
                  NX=36
              END IF
          END IF

          LABY='DIFFRACTION MTF                         '
          NY=15

C     PLOT THE AXES AND TICS
          CALL PLOTAXES
C     PLOT THE HORIZONTAL AXIS NAME
          CALL PLOTHNAME2(LABX,NX)
C     PLOT THE VERTICAL AXIS NAME
          CALL PLOTVNAME(LABY,NY)

C     PLOT THE VALUES FOR THE TIC MARKS
          CALL PLOTVVAL(0.0,0.1)
          DELX1=(ULIM-LLIM)/5.0
          CALL PLOTHVAL1(LLIM,DELX1)

C
          COLPAS=COLFRM
          IF(.NOT.MULTIOTF) THEN
              DO I=1,101
                  ADDTAF(I)=DDTAF(I)
                  ADDTAM1(I)=DDTAM(I,0)
                  ADDTAM2(I)=DDTAM(I,1)
              END DO
              CALL PLOTFUNC1(ADDTAF,ADDTAM1,N+1,LLIM,ULIM,0.0,1.005,150,0)
              CALL PLOTFUNC1(ADDTAF,ADDTAM2,N+1,LLIM,ULIM,0.0,1.005,150,3)
          ELSE
              IF(SQ.EQ.0) THEN
C     NON-LEICA TYPE PLOTS
C     MULTIOTF
                  CT=0
                  LNTP=0
                  DO J=0,OTFPAIR
                      IF(J.EQ.0)  K=0
                      IF(J.EQ.0)  L=1
                      IF(J.EQ.1)  K=2
                      IF(J.EQ.1)  L=3
                      IF(J.EQ.2)  K=4
                      IF(J.EQ.2)  L=5
                      IF(J.EQ.3)  K=6
                      IF(J.EQ.3)  L=7
                      IF(J.EQ.4)  K=8
                      IF(J.EQ.4)  L=9
                      IF(J.EQ.5)  K=10
                      IF(J.EQ.5)  L=11
                      IF(J.EQ.6)  K=12
                      IF(J.EQ.6)  L=13
                      IF(J.EQ.7)  K=14
                      IF(J.EQ.7)  L=15
                      IF(J.EQ.8)  K=16
                      IF(J.EQ.8)  L=17
                      IF(J.EQ.9)  K=18
                      IF(J.EQ.9)  L=19
                      IF(J.EQ.10) K=20
                      IF(J.EQ.10) L=21
                      DO I=1,101
                          ADDTAF(I)=DDTAF(I)
                          ADDTAM1(I)=DDTAM(I,K)
                          ADDTAM2(I)=DDTAM(I,L)
                      END DO
                      CT=CT+1
                      CALL PLOTFUNCB(ADDTAF,ADDTAM1,N+1,LLIM,ULIM,0.0,1.005,150,
     1                LNTP,CT,J)
                      CT=CT+1
                      CALL PLOTFUNCB(ADDTAF,ADDTAM2,N+1,LLIM,ULIM,0.0,1.005,150,
     1                LNTP,CT,J)
                  END DO
              ELSE
C     LEICA TYPE PLOTS
                  CT=0
                  LNTP=0
                  IF(N.GT.10) N=10
                  MAXFREQ=DBLE(DDTAF(N))/DBLE(N-1)
                  DO J=1,CFLDCNT
                      LDTAF(J)=REAL(DSQRT((CFLDS(2,J)**2)+(CFLDS(1,J)**2)))
                  END DO
                  DO I=1,N+1
                      DO J=1,CFLDCNT
C     FREQUENCY I, HORIZONTAL BARS
                          LDTAM1(J)=DDTAM(I,(2*J))
                      END DO
                      IF(CT.EQ.0.OR.CT.EQ.1) KK=0
                      IF(CT.EQ.2.OR.CT.EQ.3) KK=1
                      IF(CT.EQ.4.OR.CT.EQ.5) KK=2
                      IF(CT.EQ.6.OR.CT.EQ.7) KK=3
                      IF(CT.EQ.8.OR.CT.EQ.9) KK=4
                      IF(CT.EQ.10.OR.CT.EQ.11) KK=5
                      IF(CT.EQ.12.OR.CT.EQ.13) KK=6
                      IF(CT.EQ.14.OR.CT.EQ.15) KK=7
                      IF(CT.EQ.16.OR.CT.EQ.17) KK=8
                      IF(CT.EQ.18.OR.CT.EQ.19) KK=9
                      IF(CT.EQ.20.OR.CT.EQ.21) KK=10
                      CT=CT+1

                      IF(DLEICA(KK)) THEN
                          CALL PLOTFUNCC(LDTAF,LDTAM1,JK_NN,LLIM,ULIM,0.0,1.005,150,
     2                    LNTP,CT,MAXFREQ)
                      END IF

                      DO J=1,CFLDCNT
                          LDTAM2(J)=DDTAM(I,(2*J)+1)
                      END DO
                      IF(CT.EQ.0.OR.CT.EQ.1) KK=0
                      IF(CT.EQ.2.OR.CT.EQ.3) KK=1
                      IF(CT.EQ.4.OR.CT.EQ.5) KK=2
                      IF(CT.EQ.6.OR.CT.EQ.7) KK=3
                      IF(CT.EQ.8.OR.CT.EQ.9) KK=4
                      IF(CT.EQ.10.OR.CT.EQ.11) KK=5
                      IF(CT.EQ.12.OR.CT.EQ.13) KK=6
                      IF(CT.EQ.14.OR.CT.EQ.15) KK=7
                      IF(CT.EQ.16.OR.CT.EQ.17) KK=8
                      IF(CT.EQ.18.OR.CT.EQ.19) KK=9
                      IF(CT.EQ.20.OR.CT.EQ.21) KK=10
                      CT=CT+1
                      IF(DLEICA(KK)) THEN
                          CALL PLOTFUNCC(LDTAF,LDTAM2,JK_NN,LLIM,ULIM,0.0,1.005,150,
     2                    LNTP,CT,MAXFREQ)
                      END IF
                  END DO
              END IF
          END IF
C
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)

          IF(.NOT.MULTIOTF) THEN

              IF(.NOT.SUMMOR) THEN
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
101               FORMAT(F8.3)
200               FORMAT(A8)
                  NNTT1='OBJECT POINT-X'
C
                  CALL MY_JUSTSTRING(1400,6300,NNTT1(1:14),NT1ANG,NT1SIZ,3)
                  NNTT1=' = '//CRANGE//' '//UNN1
C
                  CALL MY_JUSTSTRING(2700,6300,NNTT1(1:21),NT1ANG,NT1SIZ,3)
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
                  CALL MY_JUSTSTRING(1400,6100,NNTT1(1:14),NT1ANG,NT1SIZ,3)
                  NNTT1=' = '//CRANGE//' '//UNN1
C
                  CALL MY_JUSTSTRING(2700,6100,NNTT1(1:21),NT1ANG,NT1SIZ,3)
              END IF
          END IF
C
          IF(.NOT.SUMMOR) THEN

C     SPCTRAL WEIGHTS
              IV=WVWT(7050,6900,ERROR1)

          END IF

          call settwocolors
C
          IF(DFLAG.EQ.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='DRAW'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF

          RETURN
      END


C SUB PLTGOTF.FOR
      SUBROUTINE PLTGOTF
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO PLOT GOTF AT THE CMD LEVEL
C
          CHARACTER BL20*20,NNTT1*99,CRANGE*8,LABX*40,LABY*40
     1    ,UNN1*9,BLNOTE*80,B*80,DTY*10,TMY*8
C
          REAL*4 GDTAF(1:101),GDTAM(1:101,1:20),GDTAP(1:101,1:20)
     1    ,AGDTAF(1:101),AGDTAM1(1:101),AGDTAM2(1:101)
     2    ,RDELA,RDELB,RDELC,LLIM,ULIM
     3    ,DELX1
C
          REAL*4 LGTAF(1:11),LGTAM1(1:11),LGTAM2(1:11)
C
          REAL*8 FLDCODE(1:2,0:10)
          CHARACTER FLDUNIT(0:10)*3
          COMMON/FLDLAB/FLDCODE,FLDUNIT
C
          REAL*8 RANGE1,MTHETA1,DELFREQ,DELZ,MAXFREQ,MTHETA2
C
          INTEGER COLPAS,IV,WVWT,ORTAG,I,ENN,ENNL,NT1ANG,NT1SIZ,DFLAG
          INTEGER NX,NY,CT,LNTP,JK_NN
C
          INTEGER KK,MYJK,J,K,L
C
          LOGICAL MULTIOTF
          COMMON/OTFMULTI/MULTIOTF
C
          EXTERNAL WVWT
C
          COMMON/GMTFPASS/ENNL,GDTAF,GDTAM,GDTAP,MTHETA1,
     1    DELZ,DELFREQ,MAXFREQ,MTHETA2
C
          LOGICAL ERROR1,EXTGMTF1,EXTGMTF2
C
          COMMON/GMTFEXT/EXTGMTF1,EXTGMTF2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
          INCLUDE 'datspd.inc'
C
          MYJK=0
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"PLTGOTF" PLOTS EXISTING GOTF VALUES'
              CALL SHOWIT(1)
              RETURN
          ELSE
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"PLTGOTF" TAKES NO QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'LEICA') THEN
              OUTLYNE=
     1        '"PLTGOTF" ONLY TAKES THE OPTIONAL QUALIFIER WORD "LEICA"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'LEICA') THEN
              IF(DF1.EQ.0) THEN
                  DF1=1
                  S1=0
                  W1=0.0D0
              END IF
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLTGOTF" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C
          IF(.NOT.EXTGMTF1) THEN
              OUTLYNE=
     1        'GOTF VALUES DO NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ENNL.LT.10) THEN
              OUTLYNE=
     1        'LESS THAN 10 GOTF VALUES IN TABLE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(CFLDCNT.LT.3.AND.WQ.EQ.'LEICA') THEN
              OUTLYNE=
     1        '"LEICA" TYPE PLOTS REQUIRE A MINIMUM OF 3 FOV POSITIONS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ENNL.GT.10.AND.WQ.EQ.'LEICA') THEN
              OUTLYNE=
     1        'WARNING:'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"LEICA" TYPE PLOTS WILL ONLY DISPLAY DATA FOR THE FIRST 10'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"NON-ZERO FREQUENCIES DISPLAYED WITH THE LAST "GOTF" COMMAND'
              CALL SHOWIT(1)
          END IF
          IF(DF1.EQ.1.OR.DF1.EQ.0.AND.W1.EQ.0.0D0) DFLAG=0
          IF(DF1.EQ.0.AND.W1.NE.0.0D0) DFLAG=1
C     GENERATE GRAPHIC
C     DO A PLOT NEW
          IF(SQ.EQ.1) THEN
              LLIM=REAL(DSQRT((CFLDS(2,1)**2)+(CFLDS(1,1)**2)))
              ULIM=REAL(DSQRT((CFLDS(2,CFLDCNT)**2)+(CFLDS(1,CFLDCNT)**2)))
              JK_NN=CFLDCNT
          END IF
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
          CALL PLTDEV
          GRASET=.TRUE.
          PLEXIS=.TRUE.
C     SET LETTER SIZE AND ANGLE
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
          CALL PLOTBOX
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
          NT1SIZ=1
          NT1ANG=0
          IF(STMPT) CALL MYTIME(TMY)
          IF(STMPD) CALL MYDATE(DTY)
          IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
          IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
          IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
          IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_JUSTSTRING(200,500,NNTT1(1:80),NT1ANG,NT1SIZ,3)
          ELSE
C     LI BLANK, NOT ACTION
          END IF
C     DO THE PLOTTING OF THE RSPH STATUS
C     AT X=200, Y=250
          NT1SIZ=1
          NT1ANG=0
          IF(SYSTEM1(30).LE.2.0D0) THEN
              IF(REFLOC.EQ.1)
     1        NNTT1(1:80)='IMAGE POINT LOCATED AT SPOT CENTROID         '
              IF(REFLOC.EQ.3)
     1        NNTT1(1:80)='IMAGE POINT LOCATED TO REMOVE TILT           '
              IF(REFLOC.EQ.4)
     1        NNTT1(1:80)='IMAGE POINT LOCATED TO REMOVE TILT AND FOCUS '
          ELSE
              NNTT1(1:80)='IMAGE POINT LOCATED ON THE CHIEF RAY (AFOCAL)'
          END IF
          CALL MY_JUSTSTRING(200,250,NNTT1(1:34),NT1ANG,NT1SIZ,3)
C
          IF(.NOT.MULTIOTF) THEN
C     FIRST TARGET ORIEMNTATION LEGEND
c
              CALL MY_PLOT(200,6725,0,0,0,10000,0,7000)
              CALL MY_PLOT(1200,6725,1,0,0,10000,0,7000)
C
              WRITE(B,101) MTHETA1
              READ(B,200) CRANGE
              ORTAG=0
101           FORMAT(F8.3)
200           FORMAT(A8)
              IF(MTHETA1.EQ.0.0D0.OR.MTHETA1.EQ.180.0D0.OR.MTHETA1.EQ.360.0D0)
     1         THEN
                  NNTT1=
     1            'VERTICAL TARGET BARS'
                  ORTAG=1
              END IF
              IF(MTHETA1.EQ.90.0D0.OR.MTHETA1.EQ.270.0D0) THEN
                  NNTT1=
     1            'HORIZONTAL TARGET BARS'
                  ORTAG=1
              END IF
              IF(ORTAG.EQ.0) NNTT1='TARGET ORIENTATION = '
C
              CALL MY_JUSTSTRING(1400,6700,NNTT1(1:21),NT1ANG,NT1SIZ,3)

              IF(ORTAG.EQ.0) THEN
                  NNTT1=CRANGE//' '//'DEGREES'
                  CALL MY_JUSTSTRING(2700,6700,NNTT1(1:16),NT1ANG,NT1SIZ,3)
              END IF
              ORTAG=0
C
              IF(EXTGMTF2) THEN
                  COLPAS=COLLBL
                  CALL MY_COLTYP(COLPAS)
C     SECOND TARGET ORIEMNTATION LEGEND
                  CALL MY_PLOT(200,6525,0,0,0,10000,0,7000)
                  CALL MY_PLOT(1200,6525,1,3,0,10000,0,7000)
C
                  WRITE(B,101) MTHETA2
                  READ(B,200) CRANGE
                  ORTAG=0
                  IF(MTHETA2.EQ.0.0D0.OR.MTHETA2.EQ.180.0D0.OR.MTHETA2.EQ.360.0D0)
     1             THEN
                      NNTT1=
     1                'VERTICAL TARGET BARS'
                      ORTAG=1
                  END IF
                  IF(MTHETA2.EQ.90.0D0.OR.MTHETA2.EQ.270.0D0) THEN
                      NNTT1=
     1                'HORIZONTAL TARGET BARS'
                      ORTAG=1
                  END IF
                  IF(ORTAG.EQ.0) NNTT1='TARGET ORIENTATION = '
                  CALL MY_JUSTSTRING(1400,6500,NNTT1(1:21),NT1ANG,NT1SIZ,3)

                  IF(ORTAG.EQ.0) THEN
                      NNTT1=CRANGE//' '//'DEGREES'
                      CALL MY_JUSTSTRING(2700,6500,NNTT1(1:16),NT1ANG,NT1SIZ,3)
                  END IF
                  ORTAG=0
              END IF
          END IF
C
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
C
C       LOWER AND UPPER LIMITS OF FREQ ARE 0 AND GDTAF(ENN+1)
C       NOW DETERMINE THE LOWER AND UPPER X LIMITS OF THE PLOT
          IF(SQ.EQ.0) THEN
              LLIM=0.0
              ULIM=GDTAF(ENNL+1)
          END IF
C       PROCEED
C
C       PLOT THE AXES
C
C       CORRECT SCALING IS IMPORTANT
C

          IF(SQ.EQ.0) THEN
              RDELA=ULIM
              RDELC=1.0
              RDELB=0.10
          END IF
C
          IF(SQ.EQ.0) THEN
C     NON-LEICA PLOTS
              IF(SPACEBALL.EQ.1.AND.NEAR.OR.
     1        SPACEBALL.EQ.2.AND.SYSTEM1(30).LE.2.0D0) THEN
                  LABX='SPATIAL FREQUENCY (lp/mm)             '
                  NX=25
              END IF
              IF(SPACEBALL.EQ.1.AND..NOT.NEAR.OR.
     1        SPACEBALL.EQ.2.AND.SYSTEM1(30).GT.2.0D0) THEN
                  LABX='SPATIAL FREQUENCY (lp/mrad)             '
                  NX=27
              END IF
          ELSE
C     LEICA TYPE PLOTS
              IF(NEAR) THEN
                  IF(SYSTEM1(6).EQ.1.0D0) LABX='OBJECT HEIGHT (in)'
                  IF(SYSTEM1(6).EQ.2.0D0) LABX='OBJECT HEIGHT (cm)'
                  IF(SYSTEM1(6).EQ.3.0D0) LABX='OBJECT HEIGHT (mm)'
                  IF(SYSTEM1(6).EQ.4.0D0) LABX='OBJECT HEIGHT (m) '
                  NX=18
              ELSE
                  LABX='OBJECT FIELD OF VIEW (degrees)'
                  NX=36
              END IF
          END IF

          LABY='GEOMETRICAL MTF                         '
          NY=15
C     PLOT THE AXES AND TICS
          CALL PLOTAXES
C     PLOT THE HORIZONTAL AXIS NAME
          CALL PLOTHNAME2(LABX,NX)
C     PLOT THE VERTICAL AXIS NAME
          CALL PLOTVNAME(LABY,NY)

C     PLOT THE VALUES FOR THE TIC MARKS
          CALL PLOTVVAL(0.0,0.1)
          DELX1=(ULIM-LLIM)/5.0
          CALL PLOTHVAL1(LLIM,DELX1)
C
          COLPAS=COLFRM
          IF(.NOT.MULTIOTF) THEN
              DO I=1,101
                  AGDTAF(I)=GDTAF(I)
                  AGDTAM1(I)=GDTAM(I,1)
                  AGDTAM2(I)=GDTAM(I,2)
              END DO
              ENN=ENNL
              CALL PLOTFUNC1(AGDTAF,AGDTAM1,ENN+1,LLIM,ULIM,0.0,1.005,150,0)
              ENN=ENNL
              CALL PLOTFUNC1(AGDTAF,AGDTAM2,ENN+1,LLIM,ULIM,0.0,1.005,150,3)
          ELSE
C     MULTIPLE FIELDS OF VIEW
              IF(SQ.EQ.0) THEN
C     NON-LEICA PLOTS
                  CT=0
                  LNTP=0
                  DO J=1,OTFPAIR
                      IF(J.EQ.1)  K=1
                      IF(J.EQ.1)  L=2
                      IF(J.EQ.2)  K=3
                      IF(J.EQ.2)  L=4
                      IF(J.EQ.3)  K=5
                      IF(J.EQ.3)  L=6
                      IF(J.EQ.4)  K=7
                      IF(J.EQ.4)  L=8
                      IF(J.EQ.5)  K=9
                      IF(J.EQ.5)  L=10
                      IF(J.EQ.6)  K=11
                      IF(J.EQ.6)  L=12
                      IF(J.EQ.7)  K=13
                      IF(J.EQ.7)  L=14
                      IF(J.EQ.8)  K=15
                      IF(J.EQ.8)  L=16
                      IF(J.EQ.9)  K=17
                      IF(J.EQ.9)  L=18
                      IF(J.EQ.10) K=19
                      IF(J.EQ.10) L=20
                      DO I=1,101
                          AGDTAF(I)=GDTAF(I)
                          AGDTAM1(I)=GDTAM(I,K)
                          AGDTAM2(I)=GDTAM(I,L)
                      END DO
                      ENN=ENNL
                      CT=CT+1
                      CALL PLOTFUNCA(AGDTAF,AGDTAM1,ENN+1,LLIM,ULIM,0.0,1.005,150,
     1                LNTP,CT,J)
                      ENN=ENNL
                      CT=CT+1
                      CALL PLOTFUNCA(AGDTAF,AGDTAM2,ENN+1,LLIM,ULIM,0.0,1.005,150,
     1                LNTP,CT,J)

                  END DO
              ELSE
C     LEICA TYPE PLOTS
                  CT=0
                  LNTP=0
                  ENN=ENNL
                  IF(ENNL.GT.10) ENN=10
                  JK_NN=CFLDCNT
                  MAXFREQ=DBLE(GDTAF(ENN))/DBLE(ENN-1)
                  DO J=1,CFLDCNT
                      LGTAF(J)=REAL(DSQRT((CFLDS(2,J)**2)+(CFLDS(1,J)**2)))
                  END DO
                  DO I=1,ENN+1
                      DO J=1,CFLDCNT
C     FREQUENCY I, HORIZONTAL BARS
                          LGTAM1(J)=GDTAM(I,(2*(J)-1))
                      END DO
                      CT=CT+1
                      IF(CT.EQ.1.OR.CT.EQ.2) KK=1
                      IF(CT.EQ.3.OR.CT.EQ.4) KK=2
                      IF(CT.EQ.5.OR.CT.EQ.6) KK=3
                      IF(CT.EQ.7.OR.CT.EQ.8) KK=4
                      IF(CT.EQ.9.OR.CT.EQ.10) KK=5
                      IF(CT.EQ.11.OR.CT.EQ.12) KK=6
                      IF(CT.EQ.13.OR.CT.EQ.14) KK=7
                      IF(CT.EQ.15.OR.CT.EQ.16) KK=8
                      IF(CT.EQ.17.OR.CT.EQ.18) KK=9
                      IF(CT.EQ.19.OR.CT.EQ.20) KK=10
                      IF(GLEICA(KK))
     1                CALL PLOTFUNCC(LGTAF,LGTAM1,JK_NN,LLIM,ULIM,0.0,1.005,150,
     2                LNTP,CT,MAXFREQ)
                      DO J=1,CFLDCNT
                          LGTAM2(J)=GDTAM(I,(2*J))
                      END DO
                      CT=CT+1
                      IF(CT.EQ.1.OR.CT.EQ.2) KK=1
                      IF(CT.EQ.3.OR.CT.EQ.4) KK=2
                      IF(CT.EQ.5.OR.CT.EQ.6) KK=3
                      IF(CT.EQ.7.OR.CT.EQ.8) KK=4
                      IF(CT.EQ.9.OR.CT.EQ.10) KK=5
                      IF(CT.EQ.11.OR.CT.EQ.12) KK=6
                      IF(CT.EQ.13.OR.CT.EQ.14) KK=7
                      IF(CT.EQ.15.OR.CT.EQ.16) KK=8
                      IF(CT.EQ.17.OR.CT.EQ.18) KK=9
                      IF(CT.EQ.19.OR.CT.EQ.20) KK=10
                      IF(GLEICA(KK))
     1                CALL PLOTFUNCC(LGTAF,LGTAM2,JK_NN,LLIM,ULIM,0.0,1.005,150,
     2                LNTP,CT,MAXFREQ)
                  END DO
              END IF
C
          END IF
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
C
          IF(.NOT.MULTIOTF) THEN
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
              CALL MY_JUSTSTRING(1400,6300,NNTT1(1:14),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN1
C
              CALL MY_JUSTSTRING(2700,6300,NNTT1(1:21),NT1ANG,NT1SIZ,3)
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
              CALL MY_JUSTSTRING(1400,6100,NNTT1(1:14),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN1
C
              CALL MY_JUSTSTRING(2700,6100,NNTT1(1:21),NT1ANG,NT1SIZ,3)
C
              IF(SYSTEM1(30).LE.2.0D0) THEN
C     FOCAL OR UFOCAL, PRINT THE DEFOCUS
                  IF(DELZ.NE.0.0D0) THEN
                      RANGE1=DELZ
                      IF(SYSTEM1(6).EQ.1.0D0) UNN1='in(s)    '
                      IF(SYSTEM1(6).EQ.2.0D0) UNN1='cm(s)    '
                      IF(SYSTEM1(6).EQ.3.0D0) UNN1='mm(s)    '
                      IF(SYSTEM1(6).EQ.4.0D0) UNN1='meter(s) '
                      WRITE(B,101) RANGE1
                      READ(B,200) CRANGE
                      NNTT1='DE-FOCUS IN  Z'
C

                      CALL MY_JUSTSTRING(1400,5900,NNTT1(1:14),NT1ANG,NT1SIZ,3)
                      NNTT1=' = '//CRANGE//' '//UNN1
C
                      CALL MY_JUSTSTRING(2700,5900,NNTT1(1:21),NT1ANG,NT1SIZ,3)
                  END IF
              END IF
          END IF
C     SPCTRAL WEIGHTS
          IV=WVWT(7050,6900,ERROR1)

          call settwocolors
C
          IF(DFLAG.EQ.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='DRAW'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF

          RETURN
      END


C SUB PLTLSF.FOR
      SUBROUTINE PLTLSF
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO PLOT GEO LSF AT THE CMD LEVEL
C
          CHARACTER BL20*20,NNTT1*99,CRANGE*8
     1    ,LABX*40,LABY*40,TMY*8,DTY*10,UNN1*9,BLNOTE*80,B*80
C
          REAL GDTAP(1:102),GDTAV(1:102),AGDTAP(1:102),AGDTAV(1:102)
     1    ,DELXX1,LLIMM,RDELA,RDELB,RDELC,LLIM,ULIM,DELX1
C
          REAL*8 SPDELX,SPDELY,MTHETA,DELZ,RANGE1
     1    ,SGNU,SGNL
C
          INTEGER ENN,COLPAS,ORTAG,IV,WVWT,I,ENNL,NT1ANG,NT1SIZ,DFLAG,MYJK
     1    ,NX,NY
C
          EXTERNAL WVWT
C
          COMMON/GLSFPASS/ENNL,GDTAP,GDTAV,MTHETA,SPDELX,SPDELY,DELZ
C
          LOGICAL ERROR1,EXTGLSF,LSFCENT
C
          COMMON/GLSFEXT/EXTGLSF,LSFCENT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          MYJK=0
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"PLTLSF" PLOTS EXISTING GEOMETRICAL LSF VALUES'
              CALL SHOWIT(1)
              RETURN
          ELSE
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"PLTLSF" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLTLSF" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C
          IF(.NOT.EXTGLSF) THEN
              OUTLYNE=
     1        'GEOMETRICAL LSF VALUES DO NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ENNL.LT.10) THEN
              OUTLYNE=
     1        'LESS THAN 10 LSF VALUES IN TABLE'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1.OR.DF2.EQ.0.AND.W2.EQ.0.0D0) DFLAG=0
          IF(DF2.EQ.0.AND.W2.NE.0.0D0) DFLAG=1
C     GENERATE GRAPHIC
C     DO A PLOT NEW
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
          CALL PLTDEV
          GRASET=.TRUE.
          PLEXIS=.TRUE.
C     SET LETTER SIZE AND ANGLE
          BL20='                    '
          BLNOTE=BL20//BL20//BL20//BL20
          NT1SIZ=1
          NT1ANG=0
          CALL MY_SETCHARASPECT(1.5,1.5)
          CALL PLOTBOX
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
          NT1SIZ=1
          NT1ANG=0
          IF(STMPT) CALL MYTIME(TMY)
          IF(STMPD) CALL MYDATE(DTY)
          IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
          IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
          IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
          IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
          IF(NNTT1.NE.BLNOTE) THEN
              CALL MY_JUSTSTRING(200,500,NNTT1(1:80),NT1ANG,NT1SIZ,3)
          ELSE
C     LI BLANK, NOT ACTION
          END IF
C     DO THE PLOTTING OF THE LSFCENT STATUS
C     AT X=200, Y=250
          NT1SIZ=1
          NT1ANG=0
          IF(LSFCENT.AND.SPDELX.EQ.0.0D0.AND.SPDELY.EQ.0.0D0)
     1    NNTT1(1:80)='LSF CENTER LOCATED AT SPOT CENTROID'
          IF(.NOT.LSFCENT.AND.SPDELX.EQ.0.0D0.AND.SPDELY.EQ.0.0D0)
     1    NNTT1(1:80)='LSF CENTER LOCATED ON THE CHIEF RAY'
          IF(.NOT.LSFCENT.AND.SPDELX.NE.0.0D0.OR.
     1    .NOT.LSFCENT.AND.SPDELY.NE.0.0D0)
     1    NNTT1(1:80)='LSF CENTER LOCATED ON THE CHIEF RAY WITH OFFSETS'
          CALL MY_JUSTSTRING(200,250,NNTT1(1:50),NT1ANG,NT1SIZ,3)
C
C     TARGET ORIEMNTATION LEGEND
c
          CALL MY_PLOT(200,6725,0,0,0,10000,0,7000)
          CALL MY_PLOT(1200,6725,1,0,0,10000,0,7000)
C
          WRITE(B,101) MTHETA
          READ(B,200) CRANGE
          ORTAG=0
101       FORMAT(F8.3)
200       FORMAT(A8)
          IF(MTHETA.EQ.0.0D0.OR.MTHETA.EQ.180.0D0.OR.MTHETA.EQ.360.0D0)
     1     THEN
              NNTT1=
     1        'YZ-LINE SPREAD FUNCTION'
              ORTAG=1
          END IF
          IF(MTHETA.EQ.90.0D0.OR.MTHETA.EQ.270.0D0) THEN
              NNTT1=
     1        'XZ-LINE SPREAD FUNCTION'
              ORTAG=1
          END IF
          IF(ORTAG.EQ.0) NNTT1='LSF ORIENTATION = '
C
          CALL MY_JUSTSTRING(1400,6700,NNTT1(1:18),NT1ANG,NT1SIZ,3)

          IF(ORTAG.EQ.0) THEN
              NNTT1=CRANGE//' '//'DEGREES'
              CALL MY_JUSTSTRING(2700,6700,NNTT1(1:18),NT1ANG,NT1SIZ,3)
          END IF
          ORTAG=0
C
C     LINE SPREAD FUNCTION LIMITS ARE 0 AND 1.0 JUST LIKE MTF
C
C       NOW DETERMINE THE LOWER AND UPPER X LIMITS OF THE PLOT
          ULIM=-1.0E20
          LLIM=1.0E20
          DO I=0,ENNL
              IF(GDTAP(I+1).GE.ULIM) ULIM=GDTAP(I+1)
              IF(GDTAP(I+1).LE.LLIM) LLIM=GDTAP(I+1)
          END DO
          SGNU=ULIM/DABS(DBLE(ULIM))
          SGNL=LLIM/DABS(DBLE(LLIM))
          IF(DABS(DBLE(ULIM)).GT.DABS(DBLE(LLIM))) THEN
              LLIM=REAL(-DABS(DBLE(ULIM)))
              ULIM=REAL(DABS(DBLE(ULIM)))
C     ULIM REMAIMS UNCHANGED
          ELSE
              ULIM=REAL(DABS(DBLE(LLIM)))
              LLIM=REAL(-DABS(DBLE(LLIM)))
C     LLIM REMAINS UNCHANGED
          END IF
          IF(DF1.EQ.0) THEN
              LLIM=REAL(-DABS(W1))
              ULIM=REAL(DABS(W1))
          END IF
C       PROCEED
C
C       PLOT THE AXES
C
C       CORRECT SCALING IS IMPORTANT
C

          RDELA=ULIM
          RDELC=1.0
          RDELB=0.10
C
C     Y-AXIS LABELS
C
          LABY='Geometrical LSF                         '
          NY=15
C     X-AXIS LABELS RANGE
C
          IF(SYSTEM1(30).GE.3.0D0) THEN
              LABX='LSF Extent (mrad)                       '
              NX=17
          ELSE
              COLPAS=COLFRM
              CALL MY_COLTYP(COLPAS)
              IF(SYSTEM1(6).EQ.1.0D0)
     1        LABX='LSF Extent (in)                         '
              IF(SYSTEM1(6).EQ.2.0D0)
     1        LABX='LSF Extent (cm)                         '
              IF(SYSTEM1(6).EQ.3.0D0)
     1        LABX='LSF Extent (mm)                         '
              IF(SYSTEM1(6).EQ.4.0D0)
     1        LABX='LSF Extent (meter)                      '
              IF(SYSTEM1(6).EQ.1.0D0)
     1        NX=15
              IF(SYSTEM1(6).EQ.2.0D0)
     1        NX=15
              IF(SYSTEM1(6).EQ.3.0D0)
     1        NX=15
              IF(SYSTEM1(6).EQ.4.0D0)
     1        NX=18
          END IF
C
C
C     PLOT THE AXES AND TICS
          CALL PLOTAXES2
C     PLOT THE HORIZONTAL AXIS NAME
          CALL PLOTHNAME(LABX,NX)
C     PLOT THE VERTICAL AXIS NAME
          CALL PLOTVNAME(LABY,NY)

C     PLOT THE VALUES FOR THE TIC MARKS
          LLIMM=0.0
          DELXX1=0.5
          CALL PLOTVVAL2(DELXX1)
          DELX1=(ULIM-LLIM)/4.0
          CALL PLOTHVAL2(LLIM,DELX1)

C
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          DO I=1,102
              AGDTAP(I)=GDTAP(I)
              AGDTAV(I)=GDTAV(I)
          END DO
          ENN=ENNL
          CALL PLOTFUNC3(AGDTAP,AGDTAV,ENN+1,LLIM,ULIM,0.0,1.0,150)
C
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
          COLPAS=COLLBL
          CALL MY_COLTYP(COLPAS)
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
          CALL MY_JUSTSTRING(1400,6500,NNTT1(1:14),NT1ANG,NT1SIZ,3)
          NNTT1=' = '//CRANGE//' '//UNN1
C
          CALL MY_JUSTSTRING(2700,6500,NNTT1(1:21),NT1ANG,NT1SIZ,3)
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
          CALL MY_JUSTSTRING(1400,6300,NNTT1(1:14),NT1ANG,NT1SIZ,3)
          NNTT1=' = '//CRANGE//' '//UNN1
C
          CALL MY_JUSTSTRING(2700,6300,NNTT1(1:21),NT1ANG,NT1SIZ,3)
C
          IF(SPDELX.NE.0.0D0) THEN
              RANGE1=SPDELX
              IF(SYSTEM1(6).EQ.1.0D0) UNN1='in(s)    '
              IF(SYSTEM1(6).EQ.2.0D0) UNN1='cm(s)    '
              IF(SYSTEM1(6).EQ.3.0D0) UNN1='mm(s)    '
              IF(SYSTEM1(6).EQ.4.0D0) UNN1='meter(s) '
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='OFFSET  IN   X'
C
              CALL MY_JUSTSTRING(1400,5900,NNTT1(1:14),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN1
C
              CALL MY_JUSTSTRING(2700,5900,NNTT1(1:21),NT1ANG,NT1SIZ,3)
          END IF
          IF(SPDELY.NE.0.0D0) THEN
              RANGE1=SPDELY
              IF(SYSTEM1(6).EQ.1.0D0) UNN1='in(s)    '
              IF(SYSTEM1(6).EQ.2.0D0) UNN1='cm(s)    '
              IF(SYSTEM1(6).EQ.3.0D0) UNN1='mm(s)    '
              IF(SYSTEM1(6).EQ.4.0D0) UNN1='meter(s) '
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='OFFSET  IN   Y'
C
              CALL MY_JUSTSTRING(1400,5700,NNTT1(1:14),NT1ANG,NT1SIZ,3)
              NNTT1=' = '//CRANGE//' '//UNN1
C
              CALL MY_JUSTSTRING(2700,5700,NNTT1(1:21),NT1ANG,NT1SIZ,3)
          END IF
          IF(SYSTEM1(30).LE.2.0D0) THEN
C     FOCAL OR UFOCAL, PRINT THE DEFOCUS
              IF(DELZ.NE.0.0D0) THEN
                  RANGE1=DELZ
                  IF(SYSTEM1(6).EQ.1.0D0) UNN1='in(s)    '
                  IF(SYSTEM1(6).EQ.2.0D0) UNN1='cm(s)    '
                  IF(SYSTEM1(6).EQ.3.0D0) UNN1='mm(s)    '
                  IF(SYSTEM1(6).EQ.4.0D0) UNN1='meter(s) '
                  WRITE(B,101) RANGE1
                  READ(B,200) CRANGE
                  NNTT1='DE-FOCUS IN  Z'
C

                  CALL MY_JUSTSTRING(1400,6100,NNTT1(1:14),NT1ANG,NT1SIZ,3)
                  NNTT1=' = '//CRANGE//' '//UNN1
C
                  CALL MY_JUSTSTRING(2700,6100,NNTT1(1:21),NT1ANG,NT1SIZ,3)
              END IF
          END IF
C     SPCTRAL WEIGHTS
          IV=WVWT(7000,6900,ERROR1)

          call setonecolors
C
          IF(DFLAG.EQ.0) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='DRAW'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          RETURN
      END


C SUB PLTLOK.FOR
      SUBROUTINE PLTLOK
          USE GLOBALS
C
          IMPLICIT NONE
C
          REAL*8 MAG
C
C       THIS ROUTINE DOES THE "PLOT LOOK" COMMAND AT THE CMD LEVEL
C
          REAL*8 SINA,COSA,SINF,COSF,RLOOKX,RLOOKY
     1    ,RLOOKZ
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       PLOT LOOK
C
          IF(WQ.EQ.'LOOK') THEN
C       CHECK SYNTAX
              IF(STI.EQ.1) THEN
                  RLOOKX=LOOKX
                  RLOOKY=LOOKY
                  RLOOKZ=LOOKZ
                  IF(DABS(RLOOKX).LT.1.0D-10) RLOOKX=0.0D0
                  IF(DABS(RLOOKY).LT.1.0D-10) RLOOKY=0.0D0
                  IF(DABS(RLOOKZ).LT.1.0D-10) RLOOKZ=0.0D0
                  WRITE(OUTLYNE,100)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,200)RLOOKX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,300)RLOOKY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,400)RLOOKZ
                  CALL SHOWIT(1)
                  RETURN
              ELSE
              END IF
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT LOOK" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT LOOK" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF4.EQ.0) THEN
                  OUTLYNE=
     1            '"PLOT LOOK" TAKES NO NUMERIC WORD #4 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF4.EQ.1) W4=0.0D0
              IF(DF5.EQ.1) W5=3500.0D0
              LOOKX=W1
              LOOKY=W2
              LOOKZ=W3
              MAG=DSQRT((LOOKX**2)+(LOOKY**2)+(LOOKZ**2))
              LOOKX=LOOKX/MAG
              LOOKY=LOOKY/MAG
              LOOKZ=LOOKZ/MAG
C       CALCULATE VIEALF AND VIEPHI
              SINA=LOOKY
              COSA=DSQRT((LOOKX**2)+(LOOKZ**2))
              IF(DABS(SINA).LE.1.0D-15.AND.DABS(COSA).LE.1.0D-15) THEN
                  VIEALF=0.0D0
              ELSE
                  VIEALF=DATAN2(SINA,COSA)
              END IF
              VIEALF=(180.0D0/PII)*VIEALF
              IF(VIEALF.GT.0.0D0) THEN
                  IF(VIEALF.GT.90.0D0) VIEALF=180.0D0-VIEALF
                  GO TO 1000
              ELSE
              END IF
              IF(VIEALF.LT.0.0D0) THEN
                  IF(VIEALF.LT.-90.0D0) VIEALF=-180.0D0-VIEALF
              ELSE
              END IF
 1000         CONTINUE
              IF(DABS(COSA).GT.1.0D-10) THEN
                  SINF=LOOKX/COSA
                  COSF=LOOKZ/COSA
                  IF(DABS(SINF).LE.1.0D-15.AND.DABS(COSF).LE.1.0D-15) THEN
                      VIEPHI=0.0D0
                  ELSE
                      VIEPHI=DATAN2(SINF,COSF)
                  END IF
                  VIEPHI=(180.0D0/PII)*VIEPHI
                  IF(VIEPHI.LT.0.0D0) VIEPHI=360.0D0+VIEPHI
              ELSE
                  VIEPHI=270.0D0
              END IF
          ELSE
C       NOT PLOT LOOK
          END IF
C
C       PLOT YESLOOK
C
          IF(WQ.EQ.'YESLOOK') THEN
C       CHECK SYNTAX
              IF(STI.EQ.0) THEN
                  IF(SN.EQ.1.OR.SST.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOT YESLOOK" TAKES NO STRING OR NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(STI.EQ.1) THEN
                  RLOOKX=LOOKX
                  RLOOKY=LOOKY
                  RLOOKZ=LOOKZ
                  IF(DABS(RLOOKX).LT.1.0D-10) RLOOKX=0.0D0
                  IF(DABS(RLOOKY).LT.1.0D-10) RLOOKY=0.0D0
                  IF(DABS(RLOOKZ).LT.1.0D-10) RLOOKZ=0.0D0
                  WRITE(OUTLYNE,100)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,200)RLOOKX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,300)RLOOKY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,400)RLOOKZ
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DRALOK=.TRUE.
              DRAVUE=.FALSE.
              CALL DOLOK
          ELSE
C       NOT PLOT YESLOOK
          END IF
C
C       PLOT NOLOOK
C
          IF(WQ.EQ.'NOLOOK') THEN
C       CHECK SYNTAX
              IF(STI.EQ.0) THEN
                  IF(SN.EQ.1.OR.SST.EQ.1) THEN
                      OUTLYNE=
     1                '"PLOT NOLOOK" TAKES NO STRING OR NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(STI.EQ.1) THEN
                  RLOOKX=LOOKX
                  RLOOKY=LOOKY
                  RLOOKZ=LOOKZ
                  IF(DABS(RLOOKX).LT.1.0D-10) RLOOKX=0.0D0
                  IF(DABS(RLOOKY).LT.1.0D-10) RLOOKY=0.0D0
                  IF(DABS(RLOOKZ).LT.1.0D-10) RLOOKZ=0.0D0
                  WRITE(OUTLYNE,100)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,200)RLOOKX
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,300)RLOOKY
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,400)RLOOKZ
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DRALOK=.FALSE.
              DRAVUE=.FALSE.
          END IF
          RETURN
100       FORMAT('CURRENT "PLOT LOOK" VALUES ARE:')
200       FORMAT('X-VECTOR DIRECTION COSINE = ',G15.8)
300       FORMAT('Y-VECTOR DIRECTION COSINE = ',G15.8)
400       FORMAT('Z-VECTOR DIRECTION COSINE = ',G15.8)
      END
C SUB PLTLI.FOR
      SUBROUTINE PLTLI
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE "PLOT LI" AND "PLOT NOLI" COMMANDS
C       AT THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       PLOT LI
C
          IF(WQ.EQ.'LI'.OR.WC.EQ.'NOLI') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  IF(WQ.EQ.'LI')
     1            OUTLYNE='"PLOT LI" TAKES NO NUMERIC OR STRING INPUT'
                  IF(WQ.EQ.'NOLI')
     1            OUTLYNE='"PLOT NOLI" TAKES NO NUMERIC OR STRING INPUT'
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(STI.EQ.1) THEN
                  IF(PLTLLI) OUTLYNE= 'LI PLOTTING IS CURRENTLY ENABLED'
                  IF(.NOT.PLTLLI)
     1            OUTLYNE= 'LI PLOTTING IS NOT CURRENTLY ENABLED'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(WQ.EQ.'LI')PLTLLI=.TRUE.
              IF(WQ.EQ.'LI') CALL DOLI
              IF(WQ.EQ.'NOLI')PLTLLI=.FALSE.
              RETURN
          ELSE
C       NOT PLOT LI OR NOLI
          END IF
          RETURN
      END


C SUB PLTLBLL.FOR
      SUBROUTINE PLTLBLL
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE "PLOT LBL"
C       AT THE CMD LEVEL
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       PLOT LBL
C
C       CHECK SYNTAX
          IF(STI.EQ.1) THEN
              IF(PLTLBL) OUTLYNE= 'LBL PLOTTING IS CURRENTLY ENABLED'
              IF(.NOT.PLTLBL)
     1        OUTLYNE= 'LBL PLOTTING IS NOT CURRENTLY ENABLED'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.AND.WQ.EQ.'LBL') THEN
              IF(WQ.EQ.'LBL')
     1        OUTLYNE='"PLOT LBL" TAKES NO STRING INPUT'
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(SST.EQ.1.AND.WQ.EQ.'NOLBL'.OR.SN.EQ.1.AND.WQ.EQ.'NOLBL')THEN
              IF(WQ.EQ.'LBL')
     1        OUTLYNE='"PLOT NOLBL" TAKES NO STRING OR NUMERIC INPUT'
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(WQ.EQ.'LBL') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  IF(WQ.EQ.'LBL')
     1            OUTLYNE='"PLOT LBL" ONLY TAKES NUMERIC WORD #1 INPUT'
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  IF(WQ.EQ.'LBL')
     1            OUTLYNE='"PLOT LBL" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
                  IF(WQ.EQ.'LBL')
     1            OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE FOR "PLOT LBL"'
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(WQ.EQ.'LBL')PLTLBL=.TRUE.
          IF(WQ.EQ.'LBL')LBLSURF=INT(W1)
          IF(WQ.EQ.'LBL') CALL DOLBL
          IF(WQ.EQ.'NOLBL')PLTLBL=.FALSE.
          IF(WQ.EQ.'NOLBL')LBLSURF=-99
          RETURN
      END
C SUB PLTJUS.FOR
      SUBROUTINE PLTJUS
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT RIGHT, LEFT AND CENTER COMMANDS
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(WQ.EQ.'RIGHT') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT RIGHT" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  IF(RCL.EQ.1.OR.RCL.EQ.-1) WRITE(OUTLYNE,801)
                  IF(RCL.EQ.2.OR.RCL.EQ.-2) WRITE(OUTLYNE,802)
                  IF(RCL.EQ.3.OR.RCL.EQ.-3) WRITE(OUTLYNE,803)
                  CALL SHOWIT(1)
 801              FORMAT('PLOTTING IS CURRENTLY "LEFT" JUSTIFIED')
 802              FORMAT('PLOTTING IS CURRENTLY "CENTER" JUSTIFIED')
 803              FORMAT('PLOTTING IS CURRENTLY "RIGHT" JUSTIFIED')
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'CENTER') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT CENTER" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  IF(RCL.EQ.1.OR.RCL.EQ.-1) WRITE(OUTLYNE,801)
                  IF(RCL.EQ.2.OR.RCL.EQ.-2) WRITE(OUTLYNE,802)
                  IF(RCL.EQ.3.OR.RCL.EQ.-3) WRITE(OUTLYNE,803)
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'LEFT') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT LEFT" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
                  IF(RCL.EQ.1.OR.RCL.EQ.-1) WRITE(OUTLYNE,801)
                  IF(RCL.EQ.2.OR.RCL.EQ.-2) WRITE(OUTLYNE,802)
                  IF(RCL.EQ.3.OR.RCL.EQ.-3) WRITE(OUTLYNE,803)
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
C
          IF(WQ.EQ.'LEFT') RCL=1
          IF(WQ.EQ.'CENTER') THEN
              RCL=-2
              JUSOFF=5000.0D0
          ELSE
          END IF
          IF(WQ.EQ.'RIGHT') RCL=3
C
C       ALL INPUT IS OK, KEEP GOING
C
          RETURN
      END
C SUB PLTFRM.FOR
      SUBROUTINE PLTFRM
          USE GLOBALS
C
          IMPLICIT NONE
C
!        REAL*8 MAG
C
C     THIS ROUTINE DOES THE "PLOT FRAME" COMMAND AT THE CMD LEVEL
C     DEFAULT RESULTS IN PLOT FRAME 0 0 10000 7000
C
          INTEGER COLPAS,IV1,IV2,IV3,IV4
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
C       CHECK SYNTAX
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,100)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,200)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,300)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,400)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,500)
              CALL SHOWIT(1)
              RETURN
          ELSE
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT FRAME" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(S5.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT FRAME" TAKES NO NUMERIC WORD #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C     SET VALUES AND DRAW RECTANGLE
C
          IF(DF1.EQ.1) W1=0.0D0
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=10000.0D0
          IF(DF4.EQ.1) W4=7000.0D0
          IV1=INT(W1)
          IV2=INT(W2)
          IV3=INT(W3)
          IV4=INT(W4)
C
C     LIFT PEN, MOVE TO BOX START
          COLPAS=COLFRM
          CALL MY_COLTYP(COLPAS)
          CALL MY_PLOT(IV1,IV2,0,0,-10,10010,-10,10010)
C     DROP PEN, DRAW BOX
          CALL MY_PLOT(IV1,IV4,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(IV3,IV4,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(IV3,IV2,1,0,-10,10010,-10,10010)
          CALL MY_PLOT(IV1,IV2,1,0,-10,10010,-10,10010)
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
 100      FORMAT('"PLOT FRAME" DRAWS A RECTANGLE USING COORDINATES')
 200      FORMAT('OF THE LOWER LEFT AND UPPER RIGHT CORNERS')
 300      FORMAT('NUMERIC WORDS #1 AND #2 ARE THE X AND Y LOWER LEFT')
 400      FORMAT('NUMERIC WORDS #3 AND #4 ARE THE X AND Y UPPER RIGHT')
 500      FORMAT('VALUES ENTERED ARE IN DEVICE INDEPENDENT COORDINATES')
C
          RETURN
      END
C SUB FOOTSANG.FOR
      SUBROUTINE FOOTSANG
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE FOOTSANG COMMAND
C
C
          LOGICAL EXIS94,OPEN94
C
          INTEGER*1 IFTVAL
C
          INTEGER SFNUMFT,KSF,IR,ORSF,RAYTOT
C
          COMMON/FOOTNUM/SFNUMFT
C
          COMMON/SFOR/ORSF
C
          REAL*8 TOTAREA,A,B,C,SFNN
C
          REAL XF,YF,ZF,LF,MF,NF,SFL,SFM,SFN,AREA
C
!      INTEGER NO
C
!      INTEGER M1,M2,M3,M4,M5
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C
          OPEN94=.FALSE.
          EXIS94=.FALSE.
          INQUIRE(FILE=trim(HOME)//'FOOT1.DAT',OPENED=OPEN94)
          INQUIRE(FILE=trim(HOME)//'FOOT1.DAT',EXIST=EXIS94)
          IF(.NOT.EXIS94) THEN
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(OPEN94) CALL CLOSE_FILE(94,1)
C
C
C       CHECK SYNTAX
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        'FOOTAREA" CALCULATES THE SOLID ANGLE SUBTENDED BY THE'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'CURRENT BEAM FOOTPRINT FROM POINT X,Y,Z'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        'FOOTSANG" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ACC') THEN
              OUTLYNE=
     1        'FOOTSANG" ONLY TAKES "ACC" AS OPTIONAL QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0.OR.S2.EQ.0.OR.S3.EQ.0) THEN
              OUTLYNE=
     1        'FOOTSANG" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W3.EQ.0.0D0) THEN
              OUTLYNE=
     1        'FOOTSANG" REQUIRES NONZERO NUMERIC WORD #3 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(((W1**2)+(W2**2)+(W3**2)).EQ.0.0D0) THEN
              OUTLYNE=
     1        'FOOTSANG" REQUIRES A NONZERO DISTANCE FROM THE LOCAL SURFACE'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'VERTEX TO THE POINT FROM WHICH THE SOLID ANGLE IS COMPUTED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C
          OPEN(UNIT=94,ACCESS='DIRECT'
     1    ,FORM='UNFORMATTED',FILE=trim(HOME)//'FOOT1.DAT',RECL=(80*NRECL)
     2    ,STATUS='UNKNOWN')
C                       READ THE FILE HEADER
          TOTAREA=0.0D0
          READ(UNIT=94,REC=1,ERR=776) RAYTOT,KSF
          GO TO 774
 776      CONTINUE
          OUTLYNE='UNEXPECTED EOF HIT IN FOOT1.DAT'
          CALL SHOWIT(1)
          OUTLYNE='FOOT1.DAT DELETED'
          CALL SHOWIT(1)
          CALL CLOSE_FILE(94,0)
          RETURN
 774      CONTINUE
C
          DO IR=2,RAYTOT
              GO TO 778
 777          CONTINUE
              OUTLYNE='UNEXPECTED EOF HIT IN FOOT1.DAT'
              CALL SHOWIT(1)
              OUTLYNE='FOOT1.DAT DELETED'
              CALL SHOWIT(1)
              CALL CLOSE_FILE(94,0)
              RETURN
 778          CONTINUE
              READ(UNIT=94,REC=IR,ERR=777)
     1        XF,YF,ZF,LF,MF,NF,SFL,SFM,SFN,IFTVAL,AREA
              A=(W1-DBLE(XF))**2
              B=(W2-DBLE(YF))**2
              C=(W3-DBLE(ZF))**2
              IF((A+B+C).EQ.0.0D0) THEN
                  OUTLYNE=
     1            'THE CURRENT REFERENCE POINT FOR THE SOLID ANGLE CALCULATION'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'IS LOCATED ON THE FOOTPRINT SURFACE. THE CALCULATION CAN NOT'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'PROCEED'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              SFNN=(DBLE(ZF)-(W3))/DSQRT(A+B+C)
              IF(IFTVAL.EQ.1) TOTAREA=TOTAREA+
     1        DABS((DBLE(AREA)/SFNN)
     2        /(4.0D0*PII*(A+B+C)))
          END DO
          IF(WQ.EQ.'ACC') THEN
              REG(40)=REG(9)
              REG(9)=TOTAREA
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              REG(40)=REG(9)
              REG(9)=TOTAREA
              WRITE(OUTLYNE,80) KSF
              CALL SHOWIT(0)
              WRITE(OUTLYNE,81) W1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,82) W2
              CALL SHOWIT(0)
              WRITE(OUTLYNE,83) W3
              CALL SHOWIT(0)
              WRITE(OUTLYNE,84) TOTAREA
              CALL SHOWIT(0)
 80           FORMAT('BEAM FOOTPRINT ON SURFACE # ',I3)
 81           FORMAT('FROM X = ',G23.15)
 82           FORMAT('FROM Y = ',G23.15)
 83           FORMAT('FROM Z = ',G23.15)
 84           FORMAT('CURRENT BEAM FOOTPRINT SOLID ANGLE = ',G23.15,' STER.')
              RETURN
          END IF
      END
C SUB FOOTAREA.FOR
      SUBROUTINE FOOTAREA
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE FOOTAREA COMMAND
C
C
          LOGICAL EXIS94,OPEN94
C
          INTEGER*1 IFTVAL
C
          INTEGER SFNUMFT,KSF,IR,ORSF,RAYTOT
C
          COMMON/FOOTNUM/SFNUMFT
C
          COMMON/SFOR/ORSF
C
          REAL*8 TOTAREA

C
          REAL XF,YF,ZF,LF,MF,NF,SFL,SFM,SFN,AREA
C
C
!      INTEGER M1,M2,M3,M4,M5
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C
          OPEN94=.FALSE.
          EXIS94=.FALSE.
          INQUIRE(FILE=trim(HOME)//'FOOT1.DAT',OPENED=OPEN94)
          INQUIRE(FILE=trim(HOME)//'FOOT1.DAT',EXIST=EXIS94)
          IF(.NOT.EXIS94) THEN
              OUTLYNE=
     1        'NO FOOT PRINT DATA EXISTS'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(OPEN94) CALL CLOSE_FILE(94,1)
C
C
C       CHECK SYNTAX
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        'FOOTAREA" CALCULATES THE AREA OF THE CURRENT BEAM FOOTPRINT'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        'FOOTAREA" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'ACC') THEN
              OUTLYNE=
     1        'FOOTAREA" ONLY TAKES "ACC" AS OPTIONAL QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C
          OPEN(UNIT=94,ACCESS='DIRECT'
     1    ,FORM='UNFORMATTED',FILE=trim(HOME)//'FOOT1.DAT',RECL=(80*NRECL)
     2    ,STATUS='UNKNOWN')
C                       READ THE FILE HEADER
          TOTAREA=0.0D0
          READ(UNIT=94,REC=1,ERR=776) RAYTOT,KSF
          GO TO 774
 776      CONTINUE
          OUTLYNE='UNEXPECTED EOF HIT IN FOOT1.DAT'
          CALL SHOWIT(1)
          OUTLYNE='FOOT1.DAT DELETED'
          CALL SHOWIT(1)
          CALL CLOSE_FILE(94,0)
          RETURN
 774      CONTINUE
C
          DO IR=2,RAYTOT
              GO TO 778
 777          CONTINUE
              OUTLYNE='UNEXPECTED EOF HIT IN FOOT1.DAT'
              CALL SHOWIT(1)
              OUTLYNE='FOOT1.DAT DELETED'
              CALL SHOWIT(1)
              CALL CLOSE_FILE(94,0)
              RETURN
 778          CONTINUE
              READ(UNIT=94,REC=IR,ERR=777)
     1        XF,YF,ZF,LF,MF,NF,SFL,SFM,SFN,IFTVAL,AREA
              IF(IFTVAL.EQ.1) TOTAREA=TOTAREA+(DBLE(AREA/SFN))
          END DO
          IF(WQ.EQ.'ACC') THEN
              REG(40)=REG(9)
              REG(9)=TOTAREA
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              REG(40)=REG(9)
              REG(9)=TOTAREA
              WRITE(OUTLYNE,80) KSF
              CALL SHOWIT(0)
              IF(SYSTEM1(6).EQ.1) WRITE(OUTLYNE,81) TOTAREA
              IF(SYSTEM1(6).EQ.2) WRITE(OUTLYNE,82) TOTAREA
              IF(SYSTEM1(6).EQ.3) WRITE(OUTLYNE,83) TOTAREA
              IF(SYSTEM1(6).EQ.4) WRITE(OUTLYNE,84) TOTAREA
              CALL SHOWIT(0)
 80           FORMAT('BEAM FOOTPRINT ON SURFACE # ',I3)
 81           FORMAT('CURRENT BEAM FOOTPRINT AREA = ',G23.15,' SQUARE IN')
 82           FORMAT('CURRENT BEAM FOOTPRINT AREA = ',G23.15,' SQUARE CM')
 83           FORMAT('CURRENT BEAM FOOTPRINT AREA = ',G23.15,' SQUARE MM')
 84           FORMAT('CURRENT BEAM FOOTPRINT AREA = ',G23.15,' SQUARE METERS')
              RETURN
          END IF
      END


C SUB PLTFOOT.FOR
      SUBROUTINE PLTFOOT
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT FOOT COMMAND
C
C     IT WAS MODELLED AFTER THE SUBROUTINE PLTCLP.FOR
C
!      LOGICAL OPEN94
C
          INTEGER*1 IFTVAL
C
          INTEGER COLPAS,SFNUMFT,KSF,IR,ORSF,RAYTOT,ALLOERR
C
          COMMON/FOOTNUM/SFNUMFT
C
          COMMON/SFOR/ORSF
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI
     2    ,XNEW,YNEW,LKG,VIEPH,VIEAL,Z1,ANGLE,AN2
     3    ,X00,Y00,Z0,LX0,LY0,LZ0,XFTVAL,YFTVAL,ZFTVAL
     4    ,X1,Y1,MX0,MY0,MZ0,NX0,NY0,NZ0,XID,YID
C
          REAL XF,YF,ZF,LF,MF,NF,SFL,SFM,SFN,AREA
C
          INTEGER JJ,J,III,NO
C
          INTEGER M1,M2,M3,M4,M5,IX,IY,I,II
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          REAL*8 CLPDAT
          DIMENSION CLPDAT(:,:,:,:)
          ALLOCATABLE :: CLPDAT
C
C     LOOK.VIEW TRANSFORMS

          ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
          ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
          ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
          ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
C
          M1=0
          M2=360
          M3=3
          M4=INT(SYSTEM1(20))
          M5=2
          DEALLOCATE(CLPDAT,STAT=ALLOERR)
          ALLOCATE(CLPDAT(M1:M2,M3,M1:M4,M5),STAT=ALLOERR)
          CLPDAT(0:360,1:3,0:M4,1:2)=0.0
C
          JJ=1
C
          X=0.0D0
          Y=0.0D0
C
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
          GLSURF=-99
          DO I=NEWIMG,NEWOBJ,-1
              IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
          END DO
          IF(GLSURF.EQ.-99) THEN
              GLOBE=.FALSE.
              OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
              CALL SHOWIT(1)
              OUTLYNE='NO OPTICAL SYSTEM PLOT COULD BE MADE'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(CLPDAT,STAT=ALLOERR)
              RETURN
          END IF
          GLOBE=.TRUE.
          OFFX=0.0D0
          OFFY=0.0D0
          OFFZ=0.0D0
          OFFA=0.0D0
          OFFB=0.0D0
          OFFC=0.0D0
          CALL GLVERT
          GLOBE=.FALSE.
C
C       CHECK SYNTAX
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"PLOT FOOT" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(CLPDAT,STAT=ALLOERR)
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,800)
              CALL SHOWIT(1)
 800          FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT FOOT"')
              DEALLOCATE(CLPDAT,STAT=ALLOERR)
              RETURN
          ELSE
          END IF

C       ALL INPUT IS OK, KEEP GOING
C     THE ARRAY CONTAINING SURFACE CLAP DATA IS:
C     CLPDAT(0:360,1:3,0:MAXSUR,1:2)
C
C     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
C     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
C     THE THIRD IS THE SURFACE NUMBER
C
C     WE NEED TO LOAD THE ARRAY BEFORE PLOTTING
C
C     THE PROCEDURE IS:
C
          II=SFNUMFT
C
C     1. WE WILL CLOCK AROUND THE CLEAR APERTURE FROM THE LOCAL +X
C     TOWARD THE LOCAL +Y AXIS,
C     2.0 DEGREE INCREMENTS AS MEASURED
C     BY AN OBSERVER AT THE SURFACE VERTEX, IN THE LOCAL COORDINATE
C     SYSTEM OF THE SURFACE, WITH THE OBSERVER FACING THE -Z AXIS
C     DIRECTION
C
          DO J=0,360
              ANGLE=(DBLE(J)*PII)/180.0D0
              AN2=(DBLE(J+1)*PII)/180.0D0
C     NOW ALONG THIS ANGLED LINE, WHAT ARE THE X AND Y COORDINATES
C     OF THE CLEAR APERTURE
              III=II
C
              CALL CAO3(X,Y,ANGLE,III,AN2,JJ,XID,YID,0.0D0,0.0D0)
C
C     THE RETURNED X AND Y ARE WHERE THE SAG IS TO BE CALCULATED
C
C     2. USE APPROPRIATE CALLS TO THE SAGPLT.FOR ROUTINE
C               TO CALCULATE THE SAG AND MAKE
C               CERTAIN THE SIGN IS CORRECT FOR A LOCAL Z COORDINATE
C
C
C     CALLS TO SAGPLT GO HERE
              III=II
              CALL SAGPLT(III,X,Y,Z,NO)
C
C     ASSIGN ARRAY VALUES BASED ON J VALUE
              CLPDAT(J,1,II,JJ)=X
              CLPDAT(J,2,II,JJ)=Y
              CLPDAT(J,3,II,JJ)=Z
C
C               CYCLE THROUGH THE NEXT DATA PAIR
          END DO
C
C     3. THE ARRAYS NOW HAVE LOCAL X,Y AND Z VALUES STORED IN THEM
C     CONVERT THE LOCAL X ANY Y CLAPS TO GLOBAL NUMBERS
C     GLOBAL VERTEX DATA IS
          II=SFNUMFT
          DO I=0,360
              X00=VERTEX(1,II)
              Y00=VERTEX(2,II)
              Z0=VERTEX(3,II)
              LX0=VERTEX(4,II)
              MX0=VERTEX(5,II)
              NX0=VERTEX(6,II)
              LY0=VERTEX(7,II)
              MY0=VERTEX(8,II)
              NY0=VERTEX(9,II)
              LZ0=VERTEX(10,II)
              MZ0=VERTEX(11,II)
              NZ0=VERTEX(12,II)
              X=CLPDAT(I,1,II,JJ)
              Y=CLPDAT(I,2,II,JJ)
              Z=CLPDAT(I,3,II,JJ)
C
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(Z)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              CLPDAT(I,1,II,JJ)=X1
              CLPDAT(I,2,II,JJ)=Y1
              CLPDAT(I,3,II,JJ)=Z1
          END DO
C
C     4. NOW DETERMINE THE BEST PLACE FOR THE ROTATION POINT FOR
C               PLOT LOOK/VIEW
C
C     ROT 3 IS LIKE ROT2 BUT KEYS OFF CLAP DATA NOT PROF DATA
          CALL ROT3(JJ,CLPDAT,M1,M2,M3,M4,M5)
C
C     5.  CONVERT THE GLOBAL X AND Y CLAP VALUES
C               USING THE LOOK/VIEW VALUES
          II=SFNUMFT
          DO I=0,360
              X=CLPDAT(I,1,II,JJ)
              Y=CLPDAT(I,2,II,JJ)
              Z=CLPDAT(I,3,II,JJ)
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
              CLPDAT(I,1,II,JJ)=XN
              CLPDAT(I,2,II,JJ)=YN
              CLPDAT(I,3,II,JJ)=ZN
          END DO
C
C     THE ARRAYS NOW HAVE GLOBAL SURFACE CLAP DATA IN THEM
C
C     6.IF NEEDED, DETERMINE SCALE FACTORS AND PLOT RANGE
C
C     PLTSC3 IS LIKE PLTSC2 BUT KEYS OFF CLAP DATA NOT PROF DATA
C
          CALL PLTSC5(XMINI,XMAXI,YMINI,YMAXI,JJ,CLPDAT,M1,M2,M3,M4,M5)
C
C     THE ABOVE WAS USED TO GET SCALE FACTORS JUST LIKE THEY ARE
C     GOTTEN IN A PLOT CLAP
C
C                    WE ARE GOING TO READ THE FOOT1.DAT
C     FILE AND PLOT EACH POINT, THEN LOOP AROUND FOR THE NEXT POINT
C     UNTIL WE ARE DONE.
C
          OPEN(UNIT=94,ACCESS='DIRECT'
     1    ,FORM='UNFORMATTED',FILE=trim(HOME)//'FOOT1.DAT',RECL=(80*NRECL)
     2    ,STATUS='UNKNOWN')
C                       READ THE FILE HEADER
          READ(UNIT=94,REC=1,ERR=776) RAYTOT,KSF
          GO TO 774
 776      CONTINUE
          OUTLYNE='UNEXPECTED EOF HIT IN FOOT1.DAT'
          CALL SHOWIT(1)
          OUTLYNE='FOOT1.DAT DELETED'
          CALL SHOWIT(1)
          CALL CLOSE_FILE(94,0)
          RETURN
 774      CONTINUE
C
          DO IR=2,RAYTOT
              GO TO 778
 777          CONTINUE
              OUTLYNE='UNEXPECTED EOF HIT IN FOOT1.DAT'
              CALL SHOWIT(1)
              OUTLYNE='FOOT1.DAT DELETED'
              CALL SHOWIT(1)
              CALL CLOSE_FILE(94,0)
              RETURN
 778          CONTINUE
              READ(UNIT=94,REC=IR,ERR=777)
     1        XF,YF,ZF,LF,MF,NF,SFL,SFM,SFN,IFTVAL,AREA
C
              XFTVAL=DBLE(XF)
              YFTVAL=DBLE(YF)
              ZFTVAL=DBLE(ZF)
C
C     THESE ARE THE VALUES READ
C                       XFTVAL
C                       YFTVAL
C                       ZFTVAL
C                       IFTVAL
C
              IF(IFTVAL.EQ.1) THEN
C     WE HAVE A POINT TO BE PLOTTED
C
C
C     NOW HAVE LOCAL X,Y AND Z VALUES
C     CONVERT THE LOCAL X ANY Y VALUES TO GLOBAL NUMBERS
C     GLOBAL VERTEX DATA IS
                  II=SFNUMFT
                  X00=VERTEX(1,II)
                  Y00=VERTEX(2,II)
                  Z0=VERTEX(3,II)
                  LX0=VERTEX(4,II)
                  MX0=VERTEX(5,II)
                  NX0=VERTEX(6,II)
                  LY0=VERTEX(7,II)
                  MY0=VERTEX(8,II)
                  NY0=VERTEX(9,II)
                  LZ0=VERTEX(10,II)
                  MZ0=VERTEX(11,II)
                  NZ0=VERTEX(12,II)
                  X=XFTVAL
                  Y=YFTVAL
                  Z=ZFTVAL
C
                  X1=X00+((LX0*(X))+(LY0*(Y))
     1            +(LZ0*(Z)))
                  Y1=Y00+((MX0*(X))+(MY0*(Y))
     1            +(MZ0*(Z)))
                  Z1=Z0+((NX0*(X))+(NY0*(Y))
     1            +(NZ0*(Z)))
                  XFTVAL=X1
                  YFTVAL=Y1
                  ZFTVAL=Z1
C
C     NOW DETERMINE THE BEST PLACE FOR THE ROTATION POINT FOR
C               PLOT LOOK/VIEW
C
C     ROT 3 IS LIKE ROT2 BUT KEYS OFF CLAP DATA NOT PROF DATA
                  CALL ROT3(JJ,CLPDAT,M1,M2,M3,M4,M5)
C
C     CONVERT THE GLOBAL X AND Y CLAP VALUES
C               USING THE LOOK/VIEW VALUES
                  II=SFNUMFT
                  X=XFTVAL
                  Y=YFTVAL
                  Z=ZFTVAL
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
                  XFTVAL=XN
                  YFTVAL=YN
                  ZFTVAL=ZN
C
C     NOW HAVE GLOBAL SURFACE DATA
C
C     APPLY SCALE FACTORS
C
C     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
C     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
C     TWO STEPS.
C
C     THE WORLD X PLOTS TO THE PLOTTER X
C     THE WORLD Y PLOTS TO THE PLOTTER Y
C
C     CONVERT USING AN APPROPRIATE SCALE FACTOR
C
                  XFTVAL=(XFTVAL/SCFAX)*1000.0D0
                  YFTVAL=(YFTVAL/SCFAY)*1000.0D0
C
C     APPLY THE XSHIFT AND YSHIFT VALUES
                  IF(LORIENT) CALL ORSHIFT
                  XFTVAL=XFTVAL+DBLE(PXSHFT)
                  YFTVAL=YFTVAL+3500.0D0+DBLE(PYSHFT)
C
C     SET THE PLOT JUSTIFICATION IF NEEDED
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
C     NOW
                  IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
                      JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
                      RCL=-1
                  ELSE
                  END IF
                  IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
                      RCL=-2
                      JUSOFF=5000.0D0
                  ELSE
                  END IF
                  IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
                      JUSOFF=9500.0D0-((XMAXI/SCFAX)*1000.0D0)
                      RCL=-3
                  ELSE
                  END IF
C
                  II=SFNUMFT
                  XFTVAL=XFTVAL+JUSOFF
C
C     APPLY PLOT GAMMA
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
                  XFTVAL=XFTVAL-5000.0D0
                  YFTVAL=YFTVAL-3500.0D0
C
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

                  IF(DBLE(PGAMMA).NE.0.0D0) THEN
                      LKG=(PII/180.0D0)*DBLE(PGAMMA)

                      X=XFTVAL
                      Y=YFTVAL
                      XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                      YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                      XFTVAL=XNEW
                      YFTVAL=YNEW
                  END IF
C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
C
                  XFTVAL=XFTVAL+5000.0D0
                  YFTVAL=YFTVAL+3500.0D0
C
                  I=SFNUMFT
C     NOW DRAW THE FOOT PRINT MARK AT SURFACE
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                  IF(XFTVAL.GT.1.0D6) XFTVAL=1.0D6
                  IF(YFTVAL.GT.1.0D6) YFTVAL=1.0D6
                  IF(XFTVAL.LT.-1.0D6) XFTVAL=-1.0D6
                  IF(YFTVAL.LT.-1.0D6) YFTVAL=-1.0D6
                  IX=INT(XFTVAL)
                  IY=INT(YFTVAL)
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
C     NOW ISSUE THE PLOTTING COMMANDS
C
C     LINE TYPE SETTING
C     SAME COLOR AS AXIS PLOTTING IN OPTICAL LAYOUT PLOTS
                  COLPAS=COLAXS
                  CALL MY_COLTYP(COLPAS)
                  OLLNTP=LNTYPE
                  LNTYPE=0
C     MOVE TO POINT WITH PEN UP
                  CALL PENMV1A(IX,IY,0)
C     MOVE TO Y-START POINT WITH PEN UP
                  CALL PENMV1A(IX,IY-10,0)
C     DROP THE PEN
                  CALL PENMV1A(IX,IY-10,1)
C     MOVE TO Y-END POINT WITH PEN DOWN
                  CALL PENMV1A(IX,IY+10,1)
C     RAISE THE PEN
                  CALL PENMV1A(IX,IY+10,0)
C     MOVE TO X-START POINT WITH PEN UP
                  CALL PENMV1A(IX-10,IY,0)
C     DROP THE PEN
                  CALL PENMV1A(IX-10,IY,1)
C     MOVE TO X-END POINT WITH PEN DOWN
                  CALL PENMV1A(IX+10,IY,1)
C     RAISE THE PEN
                  CALL PENMV1A(IX+10,IY,0)
              ELSE
C     THE POINT IS NOT TO BE PLOTTED
              END IF
          END DO
C
C     ENDING STUFF
C
          LNTYPE=OLLNTP
C
C     HERE WE DO THE PLOT LI AND PLOT AXIS DRAWING
          IF(.NOT.LBLFLG.AND.PLTLBL) THEN
              CALL DOLBL
              LBLFLG=.TRUE.
          ELSE
          END IF
          IF(.NOT.LIFLG.AND.PLTLLI) THEN
              CALL DOLI
              LIFLG=.TRUE.
          ELSE
          END IF
          IF(.NOT.SCFLG.AND.DRASCL) THEN
              CALL DOSC
              SCFLG=.TRUE.
              SZFLG=.TRUE.
          ELSE
          END IF
          IF(.NOT.SZFLG.AND.DRASZZ) THEN
              CALL DOSZ
              SZFLG=.TRUE.
              SCFLG=.TRUE.
          ELSE
          END IF
          IF(.NOT.LOKFLG.AND.DRALOK) THEN
              CALL DOLOK
              LOKFLG=.TRUE.
              VUEFLG=.TRUE.
          ELSE
          END IF
          IF(.NOT.VUEFLG.AND.DRAVUE) THEN
              CALL DOVUE
              LOKFLG=.TRUE.
              VUEFLG=.TRUE.
          ELSE
          END IF
          IF(.NOT.VIGFLG.AND.PLTVIG) THEN
              CALL VIGSHO
              VIGFLG=.TRUE.
          ELSE
          END IF
          DEALLOCATE(CLPDAT,STAT=ALLOERR)
          RETURN
      END


C SUB PLTFIELD.FOR
      SUBROUTINE PLTFIELD
          USE GLOBALS
C
          IMPLICIT NONE
C
C     THIS PLOTS FIELD ABERATIONS
C
          CHARACTER NNTT1*99,BLNOTE*80,BL20*20
     1    ,CRANGE*8,B*80,DTY*10,TMY*8,LABX*40,LABY*40
C
          REAL*8 WOR1(0:50),WOR2(0:50),RANGE1
     1    ,FACTY,ORI,DTA11(0:50),DTA22(0:50),DDTA(0:50),ADTA(0:50)
C
          REAL LLIM,ULIM,UFLIM,LFLIM,DELX1,FLDAN(0:50)
C
          COMMON/FIFI/FLDAN
C
          REAL X1(1:51),Y(1:51),XRAN1,YRAN,YMINJK,XMINJK1,XMAXJK1,YMAXJK
     1    ,X2(1:51),XMINJK2,XMAXJK2,XRAN2,XRAN,XMAXJK,XMINJK,X(1:51)
C
          INTEGER NX,NY,COLPAS,MYJK,DFLAG,I,PNTNUM,NT1ANG,NT1SIZ
C
          COMMON/NUMPNT/PNTNUM,ORI,FACTY
C
          LOGICAL ASTEXT,FLDEXT,DISEXT,FDISEXT
          COMMON/FIELDEXT/ASTEXT,FLDEXT,DISEXT,FDISEXT
C
          COMMON/ABSSS/WOR1,WOR2,DTA11,DTA22,DDTA,ADTA
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          MYJK=0
C
          IF(WC.EQ.'PLTDIST') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTDIST" PLOTS EXISTING DISTORTION VALUES'
                  CALL SHOWIT(1)
                  RETURN
              ELSE
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTDIST" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTDIST" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
C
C     PLT DIST
              IF(.NOT.DISEXT) THEN
                  OUTLYNE=
     1            'DISTORTION VALUES DO NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PNTNUM.LT.10) THEN
                  OUTLYNE=
     1            'LESS THAN 10 DISTORION VALUES IN TABLE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1.OR.DF2.EQ.0.AND.W2.EQ.0.0D0) DFLAG=0
              IF(DF2.EQ.0.AND.W2.NE.0.0D0) DFLAG=1
C     GENERATE GRAPHIC
C     DO A PLOT NEW
              DEVTYP=1
              LOOKY=0.0D0
              LOOKX=-1.0D0
              LOOKZ=0.0D0
              CALL PLTDEV
              GRASET=.TRUE.
              PLEXIS=.TRUE.
C     SET LETTER SIZE AND ANGLE
              BL20='                    '
              BLNOTE=BL20//BL20//BL20//BL20
              NT1SIZ=1
              NT1ANG=0
              CALL MY_SETCHARASPECT(1.5,1.5)
C     LIFT PEN, MOVE TO FRAME START
C
              CALL PLOTBOX
C
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
              NT1SIZ=1
              NT1ANG=0
              IF(STMPT) CALL MYTIME(TMY)
              IF(STMPD) CALL MYDATE(DTY)
              IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
              IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
              IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
              IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
              IF(NNTT1.NE.BLNOTE) THEN

                  CALL MY_JUSTSTRING(200,500,NNTT1(1:80),NT1ANG,NT1SIZ,3)
              ELSE
C     LI BLANK, NOT ACTION
              END IF
C
              RANGE1=ORI
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
101           FORMAT(F8.3)
200           FORMAT(A8)
              NNTT1='ORIENTATION ANGLE = '
C
              CALL MY_JUSTSTRING(200,250,NNTT1(1:20),NT1ANG,NT1SIZ,3)

              NNTT1=CRANGE//' '//'DEGREES'
C
              CALL MY_JUSTSTRING(1700,250,NNTT1(1:16),NT1ANG,NT1SIZ,3)
C
C     HERE GO THE REAL PLOTTING COMMANDS
C     **********************************
C     THE FUNCTIONAL VALUES ARE IN THE ARRAYS DDTA
C     THE INDEPENDENT VARIABLE IS THE LIST OF
C     RADIAL FIELD POSITIONS WHICH WILL BE GENERATED AND PLACED
C     IN THE 'Y' ARRAY
C     LOAD Y
              XMINJK1=1.0E20
              XMAXJK1=-1.0E20
              DO I=0,PNTNUM
                  Y(I+1)=FLDAN(I)
                  X1(I+1)=REAL(DDTA(I))
                  IF(REAL(DDTA(I)).GT.XMAXJK1) XMAXJK1=REAL(DDTA(I))
                  IF(REAL(DDTA(I)).LT.XMINJK1) XMINJK1=REAL(DDTA(I))
                  X(I+1)=X1(I+1)
              END DO
              YMAXJK=Y(PNTNUM+1)
              YMINJK=Y(1)
C
              YRAN=ABS(YMAXJK-YMINJK)
              XRAN1=ABS(XMAXJK1-XMINJK1)
              XRAN=XRAN1
              IF(DF1.EQ.0) XRAN=REAL(DABS(W1))
              XMINJK=XMINJK1
              XMAXJK=XMAXJK1
C
C       PROCEED
C
C       PLOT THE AXES
C
C       CORRECT SCALING IS IMPORTANT
C

C     Y-AXIS LABELS
C
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                  LABY='F.O.V.(deg)                             '
                  NY=11
              ELSE
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            LABY='OBJECT HT.(in)                          '
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            LABY='OBJECT HT.(cm)                          '
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            LABY='OBJECT HT.(mm)                          '
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            LABY='OBJECT HT.(meter)                       '
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            NY=17
              END IF

C     X-AXIS LABELS RANGE
C
              LABX='% Distortion                            '
              NX=12
C
C     PLOT THE AXES AND TICS
              CALL PLOTAXES2
C     PLOT THE HORIZONTAL AXIS NAME
              CALL PLOTHNAME(LABX,NX)
C     PLOT THE VERTICAL AXIS NAME
              CALL PLOTVNAME(LABY,NY)

C     PLOT THE VALUES FOR THE TIC MARKS
              DELX1=(YMAXJK)/2.0
              LLIM=0.0
              CALL PLOTVVAL2(DELX1)
              IF(DABS(DBLE(XMAXJK)).GT.DABS(DBLE(XMINJK))) THEN
                  XMAXJK=REAL(DABS(DBLE(XMAXJK)))
                  XMINJK=REAL(-DABS(DBLE(XMAXJK)))
              ELSE
                  XMAXJK=REAL(DABS(DBLE(XMINJK)))
                  XMINJK=REAL(-DABS(DBLE(XMINJK)))
              END IF
              DELX1=(XMAXJK-XMINJK)/4.0
              CALL PLOTHVAL2(XMINJK,DELX1)
C
              LLIM=XMINJK
              ULIM=XMAXJK
              UFLIM=YMAXJK
              LFLIM=0.0
              CALL PLOTFUNC4(X,Y,PNTNUM+1,XMINJK,XMAXJK,YMINJK,YMAXJK,0)
              IF(.NOT.PLEXIS) PLEXIS=.TRUE.

C
C     **********************************
C     DATA PLOTTING DONE
C
              call setonecolors
              IF(DFLAG.EQ.0) THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DRAW'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
              END IF
              RETURN
          END IF

          IF(WC.EQ.'PLTFDIST') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTFDIST" PLOTS EXISTING DISTORTION VALUES'
                  CALL SHOWIT(1)
                  RETURN

              ELSE
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTFDIST" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF

              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTFDIST" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
C
C     PLTFDIST
              IF(.NOT.FDISEXT) THEN
                  OUTLYNE=
     1            'FISHEYE DISTORTION VALUES DO NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN

              END IF
              IF(PNTNUM.LT.10) THEN
                  OUTLYNE=
     1            'LESS THAN 10 FISHEYE DISTORION VALUES IN TABLE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF

              IF(DF2.EQ.1.OR.DF2.EQ.0.AND.W2.EQ.0.0D0) DFLAG=0
              IF(DF2.EQ.0.AND.W2.NE.0.0D0) DFLAG=1
C     GENERATE GRAPHIC
C     DO A PLOT NEW
              DEVTYP=1
              LOOKY=0.0D0
              LOOKX=-1.0D0
              LOOKZ=0.0D0
              CALL PLTDEV
              GRASET=.TRUE.
              PLEXIS=.TRUE.
C     SET LETTER SIZE AND ANGLE
              BL20='                    '
              BLNOTE=BL20//BL20//BL20//BL20
              NT1SIZ=1
              NT1ANG=0
              CALL MY_SETCHARASPECT(1.5,1.5)
C     LIFT PEN, MOVE TO FRAME START
C
              CALL PLOTBOX
C
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
              NT1SIZ=1
              NT1ANG=0
              IF(STMPT) CALL MYTIME(TMY)
              IF(STMPD) CALL MYDATE(DTY)
              IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
              IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
              IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
              IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
              IF(NNTT1.NE.BLNOTE) THEN
                  CALL MY_JUSTSTRING(200,500,NNTT1(1:80),NT1ANG,NT1SIZ,3)
              ELSE
C     LI BLANK, NOT ACTION
              END IF
C
              RANGE1=ORI
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='ORIENTATION ANGLE = '
C
              CALL MY_JUSTSTRING(200,250,NNTT1(1:20),NT1ANG,NT1SIZ,3)

              NNTT1=CRANGE//' '//'DEGREES'
C
              CALL MY_JUSTSTRING(1700,250,NNTT1(1:16),NT1ANG,NT1SIZ,3)
C
C
C     HERE GO THE REAL PLOTTING COMMANDS
C     **********************************
C     THE FUNCTIONAL VALUES ARE IN THE ARRAYS DDTA
C     THE INDEPENDENT VARIABLE IS THE LIST OF
C     RADIAL FIELD POSITIONS WHICH WILL BE GENERATED AND PLACED
C     IN THE 'Y' ARRAY
C     LOAD Y
              XMINJK1=1.0E20
              XMAXJK1=-1.0E20
              DO I=0,PNTNUM
                  Y(I+1)=FLDAN(I)
                  X1(I+1)=REAL(DDTA(I))
                  IF(REAL(DDTA(I)).GT.XMAXJK1) XMAXJK1=REAL(DDTA(I))
                  IF(REAL(DDTA(I)).LT.XMINJK1) XMINJK1=REAL(DDTA(I))
                  X(I+1)=X1(I+1)
              END DO
              YMAXJK=Y(PNTNUM+1)
              YMINJK=Y(1)
C
              YRAN=ABS(YMAXJK-YMINJK)
              XRAN1=ABS(XMAXJK1-XMINJK1)
              XRAN=XRAN1
              IF(DF1.EQ.0) XRAN=REAL(DABS(W1))
              XMINJK=XMINJK1
              XMAXJK=XMAXJK1
C       PROCEED
C
C       PLOT THE AXES
C
C       CORRECT SCALING IS IMPORTANT
C

C     Y-AXIS LABELS
C
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                  LABY='F.O.V.(deg)                             '
                  NY=25
              ELSE
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            LABY='OBJECT HT.(in)                          '
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            LABY='OBJECT HT.(cm)                          '
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            LABY='OBJECT HT.(mm)                          '
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            LABY='OBJECT HT.(meter)                       '
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            NY=17
              END IF

C     X-AXIS LABELS RANGE
C
              LABX='% Fisheye Distortion                    '
              NX=20
C
C     PLOT THE AXES AND TICS
              CALL PLOTAXES2
C     PLOT THE HORIZONTAL AXIS NAME
              CALL PLOTHNAME(LABX,NX)
C     PLOT THE VERTICAL AXIS NAME
              CALL PLOTVNAME(LABY,NY)

C     PLOT THE VALUES FOR THE TIC MARKS
              DELX1=(YMAXJK)/2.0
              LLIM=0.0
              CALL PLOTVVAL2(DELX1)
              IF(DABS(DBLE(XMAXJK)).GT.DABS(DBLE(XMINJK))) THEN
                  XMAXJK=REAL(DABS(DBLE(XMAXJK)))
                  XMINJK=REAL(-DABS(DBLE(XMAXJK)))
              ELSE
                  XMAXJK=REAL(DABS(DBLE(XMINJK)))
                  XMINJK=REAL(-DABS(DBLE(XMINJK)))
              END IF
              DELX1=(XMAXJK-XMINJK)/4.0
              CALL PLOTHVAL2(XMINJK,DELX1)
C
              LLIM=XMINJK
              ULIM=XMAXJK
              UFLIM=YMAXJK
              LFLIM=0.0
              CALL PLOTFUNC4(X,Y,PNTNUM+1,XMINJK,XMAXJK,YMINJK,YMAXJK,0)
C
              IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
C
C     **********************************
C     DATA PLOTTING DONE
C
              call setonecolors
              IF(DFLAG.EQ.0) THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DRAW'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
              END IF
              RETURN
          END IF

          IF(WC.EQ.'PLTAST') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTAST" PLOTS EXISTING ASTIGMATISM VALUES'
                  CALL SHOWIT(1)
                  RETURN
              ELSE
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTAST" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTAST" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
C
C     PLT AST
              IF(.NOT.ASTEXT) THEN
                  OUTLYNE=
     1            'ASTIGMATISM VALUES DO NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PNTNUM.LT.10) THEN
                  OUTLYNE=
     1            'LESS THAN 10 ASTIGMATISM VALUES IN TABLE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1.OR.DF2.EQ.0.AND.W2.EQ.0.0D0) DFLAG=0
              IF(DF2.EQ.0.AND.W2.NE.0.0D0) DFLAG=1
C     GENERATE GRAPHIC
C     DO A PLOT NEW
              DEVTYP=1
              LOOKY=0.0D0
              LOOKX=-1.0D0
              LOOKZ=0.0D0
              CALL PLTDEV
              GRASET=.TRUE.
              PLEXIS=.TRUE.
C     SET LETTER SIZE AND ANGLE
              BL20='                    '
              BLNOTE=BL20//BL20//BL20//BL20
              NT1SIZ=1
              NT1ANG=0
              CALL MY_SETCHARASPECT(1.5,1.5)
C     LIFT PEN, MOVE TO FRAME START
C
              CALL PLOTBOX
C
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
              NT1SIZ=1
              NT1ANG=0
              IF(STMPT) CALL MYTIME(TMY)
              IF(STMPD) CALL MYDATE(DTY)
              IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
              IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
              IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
              IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
              IF(NNTT1.NE.BLNOTE) THEN
                  CALL MY_JUSTSTRING(200,500,NNTT1(1:80),NT1ANG,NT1SIZ,3)
              ELSE
C     LI BLANK, NOT ACTION
              END IF
C
              RANGE1=ORI
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='ORIENTATION ANGLE = '
C
              CALL MY_JUSTSTRING(200,250,NNTT1(1:20),NT1ANG,NT1SIZ,3)

              NNTT1=CRANGE//' '//'DEGREES'
C
              CALL MY_JUSTSTRING(1700,250,NNTT1(1:16),NT1ANG,NT1SIZ,3)
C
C
C     HERE GO THE REAL PLOTTING COMMANDS
C     **********************************
C     THE FUNCTIONAL VALUES ARE IN THE ARRAYS ADTA
C     THE INDEPENDENT VARIABLE IS THE LIST OF
C     RADIAL FIELD POSITIONS WHICH WILL BE GENERATED AND PLACED
C     IN THE 'Y' ARRAY
C     LOAD Y
              XMINJK1=1.0E20
              XMAXJK1=-1.0E20
              DO I=0,PNTNUM
                  Y(I+1)=FLDAN(I)
                  X1(I+1)=REAL(ADTA(I))
                  IF(REAL(ADTA(I)).GT.XMAXJK1) XMAXJK1=REAL(ADTA(I))
                  IF(REAL(ADTA(I)).LT.XMINJK1) XMINJK1=REAL(ADTA(I))
                  X(I+1)=X1(I+1)
              END DO
              YMAXJK=Y(PNTNUM+1)
              YMINJK=Y(1)
C
              YRAN=ABS(YMAXJK-YMINJK)
              XRAN1=ABS(XMAXJK1-XMINJK1)
              XRAN=XRAN1
              IF(DF1.EQ.0) XRAN=REAL(DABS(W1))
              XMINJK=XMINJK1
              XMAXJK=XMAXJK1
C       PROCEED
C
C       PLOT THE AXES
C
C       CORRECT SCALING IS IMPORTANT
C

C     Y-AXIS LABELS
C
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                  LABY='F.O.V.(deg)                             '
                  NY=11
              ELSE
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            LABY='OBJECT HT.(in)                          '
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            LABY='OBJECT HT.(cm)                          '
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            LABY='OBJECT HT.(mm)                          '
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            LABY='OBJECT HT.(meter)                       '
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            NY=17
              END IF

C     X-AXIS LABELS RANGE
C
              IF(SYSTEM1(30).GE.3.0D0) THEN
                  LABX='ASTIGMATISM [YZ-XZ] (diopter)           '
                  NX=29
              ELSE
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            LABX='ASTIGMATISM [YZ-XZ] (in)                '
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            LABX='ASTIGMATISM [YZ-XZ] (cm)                '
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            LABX='ASTIGMATISM [YZ-XZ] (mm)                '
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            LABX='ASTIGMATISM [YZ-XZ] (meter)             '
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            NX=24
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            NX=24
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            NX=24
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            NX=26
              END IF
              COLPAS=COLFRM
              CALL MY_COLTYP(COLPAS)
C
C     PLOT THE AXES AND TICS
              CALL PLOTAXES2
C     PLOT THE HORIZONTAL AXIS NAME
              CALL PLOTHNAME(LABX,NX)
C     PLOT THE VERTICAL AXIS NAME
              CALL PLOTVNAME(LABY,NY)

C     PLOT THE VALUES FOR THE TIC MARKS
              DELX1=(YMAXJK)/2.0
              LLIM=0.0
              CALL PLOTVVAL2(DELX1)
              IF(DABS(DBLE(XMAXJK)).GT.DABS(DBLE(XMINJK))) THEN
                  XMAXJK=REAL(DABS(DBLE(XMAXJK)))
                  XMINJK=REAL(-DABS(DBLE(XMAXJK)))
              ELSE
                  XMAXJK=REAL(DABS(DBLE(XMINJK)))
                  XMINJK=REAL(-DABS(DBLE(XMINJK)))
              END IF
              DELX1=(XMAXJK-XMINJK)/4.0
              CALL PLOTHVAL2(XMINJK,DELX1)
C
              LLIM=XMINJK
              ULIM=XMAXJK
              UFLIM=YMAXJK
              LFLIM=0.0
              CALL PLOTFUNC4(X,Y,PNTNUM+1,XMINJK,XMAXJK,YMINJK,YMAXJK,0)
C
              IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
C
C     X-AXIS LABELS (ASTIGMATISM)
C
C
C     **********************************
C     DATA PLOTTING DONE
C
              call setonecolors
              IF(DFLAG.EQ.0) THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DRAW'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
              END IF
              RETURN
          END IF

          IF(WC.EQ.'PLTFLDCV') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTFLDCV" PLOTS EXISTING FIELD CURVATURE VALUES'
                  CALL SHOWIT(1)
                  RETURN
              ELSE
              END IF
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTFLDCV" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"PLTFLDCV" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
C
C     PLT FILEDCV
              IF(.NOT.FLDEXT) THEN
                  OUTLYNE=
     1            'FIELD CURVATURE VALUES DO NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(PNTNUM.LT.10) THEN
                  OUTLYNE=
     1            'LESS THAN 10 FIELD CURVATURE VALUES IN TABLE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1.OR.DF2.EQ.0.AND.W2.EQ.0.0D0) DFLAG=0
              IF(DF2.EQ.0.AND.W2.NE.0.0D0) DFLAG=1
C     GENERATE GRAPHIC
C     DO A PLOT NEW
              DEVTYP=1
              LOOKY=0.0D0
              LOOKX=-1.0D0
              LOOKZ=0.0D0
              CALL PLTDEV
              GRASET=.TRUE.
              PLEXIS=.TRUE.
C     SET LETTER SIZE AND ANGLE
              BL20='                    '
              BLNOTE=BL20//BL20//BL20//BL20
              NT1SIZ=1
              NT1ANG=0
              CALL MY_SETCHARASPECT(1.5,1.5)
C     LIFT PEN, MOVE TO FRAME START
C
              CALL PLOTBOX
C
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
C     DO THE PLOTTING OF THE LENS IDENTIFIER
C     AT X=200, Y=500
              NT1SIZ=1
              NT1ANG=0
              IF(STMPT) CALL MYTIME(TMY)
              IF(STMPD) CALL MYDATE(DTY)
              IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
              IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
              IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
              IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
              IF(NNTT1.NE.BLNOTE) THEN
                  CALL MY_JUSTSTRING(200,500,NNTT1(1:20),NT1ANG,NT1SIZ,3)
              ELSE
C     LI BLANK, NOT ACTION
              END IF
C
              RANGE1=ORI
              WRITE(B,101) RANGE1
              READ(B,200) CRANGE
              NNTT1='ORIENTATION ANGLE = '

              CALL MY_JUSTSTRING(200,250,NNTT1(1:20),NT1ANG,NT1SIZ,3)

              NNTT1=CRANGE//' '//'DEGREES'
C
              CALL MY_JUSTSTRING(1600,250,NNTT1(1:16),NT1ANG,NT1SIZ,3)
C
C
C     HERE GO THE REAL PLOTTING COMMANDS
C     **********************************
C     THE FUNCTIONAL VALUES ARE IN THE ARRAYS DTA11 (AND DTA22 FOR
C     FIELD CURVATURE) THE INDEPENDENT VARIABLE IS THE LIST OF
C     RADIAL FIELD POSITIONS WHICH WILL BE GENERATED AND PLACED
C     IN THE 'Y' ARRAY
C     LOAD Y
              DO I=0,PNTNUM
                  Y(I+1)=FLDAN(I)
              END DO
              YMAXJK=Y(PNTNUM+1)
              YMINJK=Y(1)
              XMINJK1=1.0E20
              XMAXJK1=-1.0E20
              XMINJK2=1.0E20
              XMAXJK2=-1.0E20
              DO I=0,PNTNUM
                  X1(I+1)=REAL(DTA11(I))
                  X2(I+1)=REAL(DTA22(I))
                  IF(REAL(DTA11(I)).GT.XMAXJK1) XMAXJK1=REAL(DTA11(I))
                  IF(REAL(DTA22(I)).GT.XMAXJK2) XMAXJK2=REAL(DTA22(I))
                  IF(REAL(DTA11(I)).LT.XMINJK1) XMINJK1=REAL(DTA11(I))
                  IF(REAL(DTA22(I)).LT.XMINJK2) XMINJK2=REAL(DTA22(I))
              END DO
C
              YRAN=ABS(YMAXJK-YMINJK)
              XRAN1=ABS(XMAXJK1-XMINJK1)
              XRAN2=ABS(XMAXJK2-XMINJK2)
C     FIELD CURVATURE
              IF(XMAXJK1.GT.XMAXJK2) XMAXJK=XMAXJK1
              IF(XMAXJK1.LE.XMAXJK2) XMAXJK=XMAXJK2
              IF(XMINJK1.GT.XMINJK2) XMINJK=XMINJK1
              IF(XMINJK1.LE.XMINJK2) XMINJK=XMINJK2
              IF(XRAN1.GT.XRAN2) XRAN=XRAN1
              IF(XRAN1.LE.XRAN2) XRAN=XRAN2
              IF(DF1.EQ.0) XRAN=REAL(DABS(W1))
C     DRAW BOTH X1 AND X2
C       PROCEED
C
C       PLOT THE AXES
C
C       CORRECT SCALING IS IMPORTANT
C

C     Y-AXIS LABELS
C
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                  LABY='F.O.V.(deg)                             '
                  NY=11
              ELSE
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            LABY='OBJECT HT.(in)                          '
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            LABY='OBJECT HT.(cm)                          '
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            LABY='OBJECT HT.(mm)                          '
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            LABY='OBJECT HT.(meter)                       '
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            NY=14
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            NY=17
              END IF

C     X-AXIS LABELS RANGE
C
              IF(SYSTEM1(30).GE.3.0D0) THEN
                  LABX='FIELD CURVATURE [YZ-XZ] (diopter)       '
                  NX=33
              ELSE
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            LABX='FIELD CURVATURE [YZ-XZ] (in)            '
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            LABX='FIELD CURVATURE [YZ-XZ] (cm)            '
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            LABX='FIELD CURVATURE [YZ-XZ] (mm)            '
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            LABX='FIELD CURVATURE[YZ-XZ] (meter)          '
                  IF(SYSTEM1(6).EQ.1.0D0)
     1            NX=28
                  IF(SYSTEM1(6).EQ.2.0D0)
     1            NX=28
                  IF(SYSTEM1(6).EQ.3.0D0)
     1            NX=28
                  IF(SYSTEM1(6).EQ.4.0D0)
     1            NX=29
              END IF
              COLPAS=COLFRM
              CALL MY_COLTYP(COLPAS)
C
C     PLOT THE AXES AND TICS
              CALL PLOTAXES2
C     PLOT THE HORIZONTAL AXIS NAME
              CALL PLOTHNAME(LABX,NX)
C     PLOT THE VERTICAL AXIS NAME
              CALL PLOTVNAME(LABY,NY)

C     PLOT THE VALUES FOR THE TIC MARKS
              DELX1=(YMAXJK)/2.0
              LLIM=0.0
              CALL PLOTVVAL2(DELX1)
              IF(DABS(DBLE(XMAXJK)).GT.DABS(DBLE(XMINJK))) THEN
                  XMAXJK=REAL(DABS(DBLE(XMAXJK)))
                  XMINJK=REAL(-DABS(DBLE(XMAXJK)))
              ELSE
                  XMAXJK=REAL(DABS(DBLE(XMINJK)))
                  XMINJK=REAL(-DABS(DBLE(XMINJK)))
              END IF
              DELX1=(XMAXJK-XMINJK)/4.0
              CALL PLOTHVAL2(XMINJK,DELX1)
C
              LLIM=XMINJK
              ULIM=XMAXJK
              UFLIM=YMAXJK
              LFLIM=0.0
              CALL PLOTFUNC4(X1,Y,PNTNUM+1,XMINJK,XMAXJK,YMINJK,YMAXJK,0)
              CALL PLOTFUNC4(X2,Y,PNTNUM+1,XMINJK,XMAXJK,YMINJK,YMAXJK,3)
C
              IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
              COLPAS=COLLBL
              CALL MY_COLTYP(COLPAS)
C
C     NOW THE LEGEND
              CALL MY_PLOTC(200,6500,0,0,0,10000,0,7000)
              CALL MY_PLOTC(1200,6500,1,0,0,10000,0,7000)
              CALL MY_PLOTC(1300,6500,0,0,0,10000,0,7000)
              NNTT1=' = YZ-PLANE FIELD CURVATURE'
              CALL MY_JUSTSTRING(1300,6500,NNTT1(1:27),NT1ANG,NT1SIZ,3)
C
              CALL MY_PLOTC(200,6300,0,3,0,10000,0,7000)
              CALL MY_PLOTC(1200,6300,1,3,0,10000,0,7000)
              CALL MY_PLOTC(1300,6300,0,3,0,10000,0,7000)

              NNTT1=' = XZ-PLANE FIELD CURVATURE'
              CALL MY_JUSTSTRING(1300,6300,NNTT1(1:27),NT1ANG,NT1SIZ,3)
C
C
C     **********************************
C     DATA PLOTTING DONE

              call settwocolors2
              IF(DFLAG.EQ.0) THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DRAW'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
              END IF
              RETURN
          END IF
      END


C SUB PLTDSH.FOR
      SUBROUTINE PLTDSH
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT DASH COMMAND AT THE CMD LEVEL
C       AND PLOT NODASH
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       PLOT DASH
C
          IF(WQ.EQ.'DASH') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT DASH" TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
C     QUERRY
                  IF(DASHH) THEN
                      OUTLYNE='"PLOT DASH" IS CURRENTLY IN EFFECT FOR PLOTTING'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(.NOT.DASHH) THEN
                      OUTLYNE='"PLOT DASH" IS NOT EFFECT FOR PLOTTING'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
              END IF
              DASHH=.TRUE.
          ELSE
C       NOT PLOT DASH
          END IF
C       PLOT NODASH
C
          IF(WQ.EQ.'NODASH') THEN
C       CHECK SYNTAX
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"PLOT NODASH" TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(STI.EQ.1) THEN
C     QUERRY
                  IF(.NOT.DASHH) THEN
                      OUTLYNE=
     1                '"PLOT DASH" IS NOT IN EFFECT FOR PLOTTING'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(DASHH) THEN
                      OUTLYNE=
     1                '"PLOT DASH" IS CURRENTLY IN EFFECT FOR PLOTTING'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
              END IF
              DASHH=.FALSE.
          ELSE
C       NOT PLOT NODASH
          END IF
          RETURN
      END


C SUB PLTDEV.FOR
      SUBROUTINE PLTDEV
          USE GLOBALS
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE "PLOT NEW" COMMAND AT THE CMD
C       AND SPECT PROGRAM LEVELS
C
C
          LOGICAL USCALEX,USCALEY
          COMMON/SCALEU/USCALEX,USCALEY
          REAL YL,YU
          COMMON/YLYU/YL,YU
          REAL XL,XU
          COMMON/XLXU/XL,XU
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
C       THIS ROUTINE SETS GRASET AND DEVTYP
          GLOBE=.FALSE.
          CALL PLTRST
          XL=0.0
          XU=0.0
          YL=0.0
          YU=0.0
          DEVTYP=1
          LOOKY=0.0D0
          LOOKX=-1.0D0
          LOOKZ=0.0D0
          GRASET=.TRUE.
          USCALEX=.FALSE.
          USCALEY=.FALSE.
          SCFAX=0.0D0
          SCFAY=0.0D0
          NSSSCFA=0.0D0
          FIXUP=.FALSE.
          POLDX=0.0D0
          POLDY=0.0D0
          POLDZ=0.0D0
          PCURX=0.0D0
          PCURY=0.0D0
          PCURZ=0.0D0
          PNEWX=0.0D0
          PNEWY=0.0D0
          PNEWZ=0.0D0
          CALL PSTART
          RETURN
      END

C SUB PLTCOB.FOR
      SUBROUTINE PLTCOB(SURFACEI,MDX,MDY,GAMGAM)
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT COBS COMMAND
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,GAMGAM,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI
     2    ,XNEW,YNEW,LKG,VIEPH,VIEAL,Z1,ANGLE,AN2
     3    ,X00,Y00,Z0,LX0,LY0,LZ0,MDX,MDY
     4    ,X1,Y1,MX0,MY0,MZ0,NX0,NY0,NZ0
C
          INTEGER COLPAS,JJ,J,IK,III,NO,CLRR,ALLOERR,SURFACEI
C
          INTEGER M1,M2,M3,M4,M5,IX,IY,I,II,IPST
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          REAL*8 CLPDAT
          DIMENSION CLPDAT(:,:,:,:)
          ALLOCATABLE :: CLPDAT
C
C     LOOK.VIEW TRANSFORMS

          ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
          ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
          ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
          ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
C
          M1=0
          M2=360
          M3=3
          M4=INT(SYSTEM1(20))
          M5=2
          DEALLOCATE(CLPDAT,STAT=ALLOERR)
          ALLOCATE(CLPDAT(M1:M2,M3,M1:M4,M5),STAT=ALLOERR)
          CLPDAT(0:360,1:3,0:M4,1:2)=0.0
C
          JJ=1
C
          X=0.0D0
          Y=0.0D0
C
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
          GLSURF=-99
          DO I=NEWIMG,0,-1
              IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
          END DO
          IF(GLSURF.EQ.-99) THEN
              GLOBE=.FALSE.
              OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
              CALL SHOWIT(1)
              OUTLYNE='NO OPTICAL SYSTEM PLOT COULD BE MADE'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(CLPDAT,STAT=ALLOERR)
              RETURN
          END IF
          GLOBE=.TRUE.
          OFFX=0.0D0
          OFFY=0.0D0
          OFFZ=0.0D0
          OFFA=0.0D0
          OFFB=0.0D0
          OFFC=0.0D0
          CALL GLVERT
          GLOBE=.FALSE.
C
C       ALL INPUT IS OK, KEEP GOING
C     THE ARRAY CONTAINING SURFACE COBS DATA IS:
C     CLPDAT(0:360,1:3,0:MAXSUR,1:2)
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
          II=SURFACEI
          IF(ALENS(16,II).NE.0.0D0) THEN
C
C
C     1. WE WILL CLOCK AROUND THE OBSCURATION FROM THE LOCAL +X
C     TOWARD THE LOCAL +Y AXIS,
C     2.0 DEGREE INCREMENTS AS MEASURED
C     BY AN OBSERVER AT THE SURFACE VERTEX, IN THE LOCAL COORDINATE
C     SYSTEM OF THE SURFACE, WITH THE OBSERVER FACING THE -Z AXIS
C     DIRECTION
C
              DO J=0,360
                  ANGLE=(DBLE(J)*PII)/180.0D0
                  AN2=(DBLE(J+1)*PII)/180.0D0
C     NOW ALONG THIS ANGLED LINE, WHAT ARE THE X AND Y COORDINATES
C     OF THE CLEAR APERTURE
                  III=II
C
                  CALL CAO2(X,Y,ANGLE,III,AN2,MDX,MDY,GAMGAM)
C
C     THE RETURNED X AND Y ARE WHERE THE SAG IS TO BE CALCULATED
C
C     2. USE APPROPRIATE CALLS TO THE SAGPLT.FOR ROUTINE
C               TO CALCULATE THE SAG AND MAKE
C               CETRAIN THE SIGN IS CORRECT FOR A LOCAL Z COORDINATE
C
C
C     CALLS TO SAGPLT GO HERE
                  III=II
                  CALL SAGPLT(III,X,Y,Z,NO)
C
C     ASSIGN ARRAY VALUES BASED ON J VALUE
                  CLPDAT(J,1,II,JJ)=X
                  CLPDAT(J,2,II,JJ)=Y
                  CLPDAT(J,3,II,JJ)=Z
C
C               CYCLE THROUGH THE NEXT DATA PAIR
              END DO
          ELSE
          END IF
C
C     3. THE ARRAYS NOW HAVE LOCAL X,Y AND Z VALUES STORED IN THEM
C     CONVERT THE LOCAL X ANY Y COBS TO GLOBAL NUMBERS
C     GLOBAL VERTEX DATA IS
          II=SURFACEI
          IF(ALENS(16,II).NE.0.0D0) THEN
              DO I=0,360
                  X00=VERTEX(1,II)
                  Y00=VERTEX(2,II)
                  Z0=VERTEX(3,II)
                  LX0=VERTEX(4,II)
                  MX0=VERTEX(5,II)
                  NX0=VERTEX(6,II)
                  LY0=VERTEX(7,II)
                  MY0=VERTEX(8,II)
                  NY0=VERTEX(9,II)
                  LZ0=VERTEX(10,II)
                  MZ0=VERTEX(11,II)
                  NZ0=VERTEX(12,II)
                  X=CLPDAT(I,1,II,JJ)
                  Y=CLPDAT(I,2,II,JJ)
                  Z=CLPDAT(I,3,II,JJ)
C
                  X1=X00+((LX0*(X))+(LY0*(Y))
     1            +(LZ0*(Z)))
                  Y1=Y00+((MX0*(X))+(MY0*(Y))
     1            +(MZ0*(Z)))
                  Z1=Z0+((NX0*(X))+(NY0*(Y))
     1            +(NZ0*(Z)))
                  CLPDAT(I,1,II,JJ)=X1
                  CLPDAT(I,2,II,JJ)=Y1
                  CLPDAT(I,3,II,JJ)=Z1
              END DO
          ELSE
          END IF
C
C     4. NOW DETERMINE THE BEST PLACE FOR THE ROTATION POINT FOR
C               PLOT LOOK/VIEW
C
C     ROT 4 IS LIKE ROT2 BUT KEYS OFF COBS DATA NOT PROF DATA
          CALL ROT4(JJ,CLPDAT,M1,M2,M3,M4,M5)
C
C     5.  CONVERT THE GLOBAL X ANY Y COBS VALUES
C               USING THE LOOK/VIEW VALUES
          II=SURFACEI
          IF(ALENS(16,II).NE.0.0D0) THEN
              DO I=0,360
                  X=CLPDAT(I,1,II,JJ)
                  Y=CLPDAT(I,2,II,JJ)
                  Z=CLPDAT(I,3,II,JJ)
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
                  CLPDAT(I,1,II,JJ)=XN
                  CLPDAT(I,2,II,JJ)=YN
                  CLPDAT(I,3,II,JJ)=ZN
              END DO
          ELSE
          END IF
C
C     THE ARRAYS NOW HAVE GLOBAL SURFACE COBS DATA IN THEM
C
C     6.IF NEEDED, DETERMINE SCALE FACTORS AND PLOT RANGE
C
C     PLTSC4 IS LIKE PLTSC3 BUT KEYS OFF COBS DATA NOT CLAP DATA
C
          CALL PLTSC4(XMINI,XMAXI,YMINI,YMAXI,JJ,CLPDAT,M1,M2,M3,M4,M5)
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
          II=SURFACEI
          IF(ALENS(16,II).NE.0.0D0) THEN
              DO I=0,360
                  CLPDAT(I,1,II,JJ)=(CLPDAT(I,1,II,JJ)/SCFAX)*1000.0D0
                  CLPDAT(I,2,II,JJ)=(CLPDAT(I,2,II,JJ)/SCFAY)*1000.0D0
              END DO
          ELSE
          END IF
C
C     8. APPLY THE XSHIFT AND YSHIFT VALUES
          DO I=0,360
              II=SURFACEI
              IF(ALENS(16,II).NE.0.0D0) THEN
                  IF(LORIENT) CALL ORSHIFT
                  CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)+DBLE(PXSHFT)
                  CLPDAT(I,2,II,JJ)=CLPDAT(I,2,II,JJ)+3500.0D0+DBLE(PYSHFT)
              ELSE
              END IF
          END DO
C
C     9. SET THE PLOT JUSTIFICATION IF NEEDED
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
C     NOW
          IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
              JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
              RCL=-1
          ELSE
          END IF
          IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
              RCL=-2
              JUSOFF=5000.0D0
          ELSE
          END IF
          IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
              JUSOFF=9500.0D0-((XMAXI/SCFAX)*1000.0D0)
              RCL=-3
          ELSE
          END IF
C
          DO I=0,360
              II=SURFACEI
              IF(ALENS(16,II).NE.0.0D0) THEN
                  CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)+JUSOFF
              ELSE
              END IF
          END DO
C     9. PLOT GAMMA
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
          DO I=0,360
              II=SURFACEI
              IF(ALENS(16,II).NE.0.0D0) THEN
                  CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)-5000.0D0
                  CLPDAT(I,2,II,JJ)=CLPDAT(I,2,II,JJ)-3500.0D0
              ELSE
              END IF
          END DO
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

          IF(DBLE(PGAMMA).NE.0.0D0) THEN
              LKG=(PII/180.0D0)*DBLE(PGAMMA)

              DO I=0,360
                  II=SURFACEI
                  IF(ALENS(16,II).NE.0.0D0) THEN
                      X=CLPDAT(I,1,II,JJ)
                      Y=CLPDAT(I,2,II,JJ)
                      XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                      YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                      CLPDAT(I,1,II,JJ)=XNEW
                      CLPDAT(I,2,II,JJ)=YNEW
                  ELSE
                  END IF
              END DO
          ELSE
          END IF
C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
          DO I=0,360
              II=SURFACEI
              IF(ALENS(16,II).NE.0.0D0) THEN
                  CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)+5000.0D0
                  CLPDAT(I,2,II,JJ)=CLPDAT(I,2,II,JJ)+3500.0D0
              ELSE
              END IF
          END DO
C
          I=SURFACEI
          IF(ALENS(16,I).NE.0.0D0) THEN
C     NOW DRAW THE COBS AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
              IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              DO J=0,360
C     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
                  IF(J.EQ.0) IPST=0
                  IF(J.NE.0) IPST=1
                  IF(CLPDAT(J,1,I,JJ).GT.1.0D6) CLPDAT(J,1,I,JJ)=1.0D6
                  IF(CLPDAT(J,2,I,JJ).GT.1.0D6) CLPDAT(J,2,I,JJ)=1.0D6
                  IF(CLPDAT(J,1,I,JJ).LT.-1.0D6) CLPDAT(J,1,I,JJ)=-1.0D6
                  IF(CLPDAT(J,2,I,JJ).LT.-1.0D6) CLPDAT(J,2,I,JJ)=-1.0D6
                  IX=INT(CLPDAT(J,1,I,JJ))
                  IY=INT(CLPDAT(J,2,I,JJ))
                  P1ARAY(J,1,1)=IX
                  P1ARAY(J,2,1)=IY
                  P1ARAY(J,3,1)=IPST
                  IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              END DO
C     FINISHED WITH THAT COBS, LIFT PEN
              IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
C     LINE TYPE SETTING
              COLPAS=COLCOB
              CALL MY_COLTYP(COLPAS)
              OLLNTP=LNTYPE
              LNTYPE=0
C     SOLID OR INVISIBLE (NO DASHES FOR COBS AS OF 4/21/00)
              CLRR=0
              IF(DUMMMY(I).AND.
     1        ALENS(16,I).EQ.0.0D0) CLRR=-1
              IF(DUMMMY(I).AND.ALENS(16,I).NE.0.0D0.OR.
     1        DUMMMY(I).AND.ALENS(16,I).NE.0.0D0) THEN
                  LNTYPE=0
              ELSE
C     LEAVE LINE ALONE
              END IF
              IF(CLRR.NE.-1) THEN
                  FIXUP=.FALSE.
                  DO IK=0,360
                      IF(IK.EQ.0) P1ARAY(IK,3,1)=0
                      IF(IK.GT.0) THEN
                          IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0
     1                    .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                      END IF
                      CALL PENMV1(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
                  END DO
                  CALL FILCOB
              ELSE
                  CLRR=0
              END IF
              LNTYPE=OLLNTP
          ELSE
          END IF
C
C     HERE WE DO THE PLOT LI AND PLOT AXIS DRAWING
          IF(.NOT.VIGFLG.AND.PLTVIG) THEN
              CALL VIGSHO
              VIGFLG=.TRUE.
          ELSE
          END IF
          DEALLOCATE(CLPDAT,STAT=ALLOERR)
          RETURN
      END


C SUB PLTDEV1.FOR
      SUBROUTINE PLTDEV1
          USE GLOBALS
          USE NSSMOD
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE "PLOT NEW" COMMAND AT THE CMD
C       AND SPECT PROGRAM LEVELS
C
C
          LOGICAL USCALEX,USCALEY
          COMMON/SCALEU/USCALEX,USCALEY
          REAL YL,YU
          COMMON/YLYU/YL,YU
          REAL XL,XU
          COMMON/XLXU/XL,XU
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
C       THIS ROUTINE SETS GRASET AND DEVTYP
          GLOBE=.FALSE.
          CALL PLTRST1
          XL=0.0
          XU=0.0
          YL=0.0
          YU=0.0
          DEVTYP=1
          GRASET=.TRUE.
          USCALEX=.FALSE.
          USCALEY=.FALSE.
          SCFAX=0.0D0
          SCFAY=0.0D0
          NSSSCFA=0.0D0
          FIXUP=.FALSE.
          POLDX=0.0D0
          POLDY=0.0D0
          POLDZ=0.0D0
          PCURX=0.0D0
          PCURY=0.0D0
          PCURZ=0.0D0
          PNEWX=0.0D0
          PNEWY=0.0D0
          PNEWZ=0.0D0
          CALL PSTART
          RETURN

      END


C SUB PLTEDG.FOR
      SUBROUTINE PLTEDG
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE PLOT EDGEX/EDGEY COMMAND AT THE CMD LEVEL
C
          REAL*8 X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,XX1,XX2,YY1,
     1    ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI
     2    ,XNEW,YNEW,LKG,VIEPH,VIEAL,Z1,YY2
     3    ,X00,Y00,Z0,LX0,LY0,LZ0
     4    ,X1,X2,Y1,Y2,MX0,MY0,MZ0,NX0,NY0,NZ0
C
          INTEGER M1,M2,M3,CAFLG,COFLG,IK,III,NO,ALLOERR
C
          REAL*8 XLFT,YLFT,XRHT,YRHT,XTOP,YTOP,XBOT,YBOT
     1    ,XLFTO,YLFTO,XRHTO,YRHTO,XTOPO,YTOPO,XBOTO,YBOTO
     2    ,YLFT2,XLFT2,YRHT2,XRHT2,XTOP2,YTOP2,XBOT2,YBOT2,ZDELZ
C
          INTEGER COLPAS,IX,IY,I,II,IPST,J
C
          LOGICAL NOPLOT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          REAL*8 EDGE
          DIMENSION EDGE(:,:,:)
          ALLOCATABLE :: EDGE

C
C     LOOK.VIEW TRANSFORMS

          ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
          ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
C
          ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
          ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
          LNTYPE=0
C
          M1=4
          M2=0
          M3=INT(SYSTEM1(20))
          DEALLOCATE (EDGE,STAT=ALLOERR)
          ALLOCATE (EDGE(M1,M1,M2:M3),STAT=ALLOERR)
          X=0.0D0
          Y=0.0D0
          EDGE(1:4,1:4,0:M3)=0.0
C
          VIEPH=(PII/180.0D0)*VIEPHI
          VIEAL=(PII/180.0D0)*VIEALF
C
C     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
C     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
          GLSURF=-99
          DO I=NEWIMG,0,-1
              IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
          END DO
          IF(GLSURF.EQ.-99) THEN
              GLOBE=.FALSE.
              OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
              CALL SHOWIT(1)
              OUTLYNE='NO OPTICAL SYSTEM1 PLOT COULD BE MADE'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(EDGE,STAT=ALLOERR)
              RETURN
          END IF
          GLOBE=.TRUE.
          OFFX=0.0D0
          OFFY=0.0D0
          OFFZ=0.0D0
          OFFA=0.0D0
          OFFB=0.0D0
          OFFC=0.0D0
          CALL GLVERT
          GLOBE=.FALSE.
          IF(MSG) THEN
              OUTLYNE='GENERATING SURFACE EDGE DATA...'
              CALL SHOWIT(1)
          END IF
C
C       CHECK SYNTAX
          IF(SST.EQ.1) THEN
              IF(WQ.EQ.'EDGEX')OUTLYNE=
     1        '"PLOT EDGEX" TAKES NO STRING INPUT'
              IF(WQ.EQ.'EDGEY')OUTLYNE=
     1        '"PLOT EDGEY" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(EDGE,STAT=ALLOERR)
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1) THEN
              IF(WQ.EQ.'EDGEX')OUTLYNE=
     1        '"PLOT EDGEX" ONLY TAKES NUMERIC WORDS #1, #2 AND #5 INPUT'
              IF(WQ.EQ.'EDGEY')OUTLYNE=
     1        '"PLOT EDGEY" ONLY TAKES NUMERIC WORDS #1, #2 AND #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(EDGE,STAT=ALLOERR)
              RETURN
          END IF
          NOPLOT=.FALSE.
          IF(DF5.EQ.0) NOPLOT=.TRUE.
          IF(STI.EQ.1) THEN
              IF(WQ.EQ.'EDGEX')WRITE(OUTLYNE,800)
              IF(WQ.EQ.'EDGEY')WRITE(OUTLYNE,801)
              CALL SHOWIT(1)
 800          FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT EDGEX"')
 801          FORMAT('QUERRY (?) HAS NO MEANING WITH "PLOT EDGEY"')
              DEALLOCATE(EDGE,STAT=ALLOERR)
              RETURN
          ELSE
          END IF
          IF(DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
          IF(DF2.EQ.0.AND.W2.LT.0.0D0) W2=SYSTEM1(20)+W2
C       DEFAULT VALUES
          IF(DF1.EQ.1) THEN
              IF(DABS(ALENS(3,0)).GT.1.0D10) THEN
                  W1=DBLE(1)
              ELSE
                  W1=DBLE(0)
              END IF
          ELSE
C       DF1 NOT 1, W1 EXPLICITLY ENTERED
          END IF
          STASUR=INT(W1)
          IF(DF2.EQ.1) THEN
              W2=DBLE(NEWIMG)
          ELSE
C       DF2 NOT 1, W2 EXPLICITLY ENTERED
          END IF
          STPSUR=INT(W2)
          IF(INT(W1).LT.0) THEN
C       INVALID NUMERIC WORD #1
              OUTLYNE=
     1        'SURFACE NUMBER (NUMERIC WORD #1) IS BEYOND LEGAL RANGE1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(EDGE,STAT=ALLOERR)
              RETURN
          END IF
          IF(INT(W2).GT.NEWIMG) THEN
C       INVALID NUMERIC WORD #2
              OUTLYNE=
     1        'SURFACE NUMBER (NUMERIC WORD #2) IS BEYOND LEGAL RANGE1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(EDGE,STAT=ALLOERR)
              RETURN
          END IF
          IF(INT(W2).LE.INT(W1)) THEN
C       W2 LESS THAN OR EQUAL TO W1
              OUTLYNE=
     1        'NUMERIC WORD #2 MUST BE GREATER THAN NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(EDGE,STAT=ALLOERR)
              RETURN
          END IF
C
C       ALL INPUT IS OK, KEEP GOING
C     THE ARRAY CONTAINING SURFACE EDGE DATA ARE:
C
C     EDGE(1:4,1:3,0:MAXSUR)
C
C     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
C     FOR YEDGE
C               1=BOT   (CLAP)
C               2=TOP   (CLAP)
C               3=BOT   (COBS)
C               4=TOP   (COBS)
C     FOR YEDGE
C               1=LFT   (CLAP)
C               2=RHT   (CLAP)
C               3=LFT   (COBS)
C               4=RHT   (COBS)
C     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
C
C     WE NEED TO LOAD THE ARRAY BEFORE PLOTTING
C
C     THE PROCEDURE IS:
C     1. DETERMINE THE MAXIMUM AND MINIMUM LOCAL X AND Y
c               COORDINATES FOR A SURFACE FOR WHICH THE EDGE
C               IS TO BE CALCULATED.
C
C     CYCLE THROUGH ALL THE SURFACES
C
          DO II=STASUR,STPSUR
              X1=0.0D0
              X2=0.0D0
              Y1=0.0D0
              Y2=0.0D0
              XX1=0.0D0
              XX2=0.0D0
              YY1=0.0D0
              YY2=0.0D0
              XLFT=0.0D0
              YLFT=0.0D0
              XRHT=0.0D0
              YRHT=0.0D0
              XBOT=0.0D0
              YBOT=0.0D0
              XTOP=0.0D0
              YTOP=0.0D0
              XLFTO=0.0D0
              YLFTO=0.0D0
              XRHTO=0.0D0
              YRHTO=0.0D0
              XBOTO=0.0D0
              YBOTO=0.0D0
              XTOPO=0.0D0
              YTOPO=0.0D0
              CAFLG=0
              COFLG=0
              III=II
C
C     NOW FOR THE II SURFACE WE CALCULATE ALL THE END POINTS
C
              CALL CAO(YLFT,XLFT,YRHT,XRHT,XTOP,YTOP,XBOT,YBOT,
     1        YLFTO,XLFTO,YRHTO,XRHTO,XTOPO,YTOPO,XBOTO,YBOTO,CAFLG,
     2        COFLG,III
     3        ,YLFT2,XLFT2,YRHT2,XRHT2,XTOP2,YTOP2,XBOT2,YBOT2,ZDELZ)
C
C
              IF(WQ.EQ.'EDGEY') THEN
                  EDGE(1,1,II)=XBOT
                  EDGE(1,2,II)=YBOT
                  EDGE(2,1,II)=XTOP
                  EDGE(2,2,II)=YTOP
                  EDGE(3,1,II)=XBOTO
                  EDGE(3,2,II)=YBOTO
                  EDGE(4,1,II)=XTOPO
                  EDGE(4,2,II)=YTOPO
              ELSE
              END IF
C
              IF(WQ.EQ.'EDGEX') THEN
                  EDGE(1,1,II)=XLFT
                  EDGE(1,2,II)=YLFT
                  EDGE(2,1,II)=XRHT
                  EDGE(2,2,II)=YRHT
                  EDGE(3,1,II)=XLFTO
                  EDGE(3,2,II)=YLFTO
                  EDGE(4,1,II)=XRHTO
                  EDGE(4,2,II)=YRHTO
              ELSE
              END IF
C
C     NOW WE HAVE THE END POINTS, CYCLE THROUGH ALL FOUR PAIRS
C
C     2. USE APPROPRIATE CALLS TO THE SAGPLT.FOR ROUTINE
C               TO CALCULATE THE SAG AND MAKE
C               CETRAIN THE SIGN IS CORRECT FOR A LOCAL Z COORDINATE
C
              IF(WQ.EQ.'EDGEX') THEN
                  X1=XLFT2
                  Y1=YLFT2
                  X2=XRHT2
                  Y2=YRHT2
                  XX1=XLFTO
                  YY1=YLFTO
                  XX2=XRHTO
                  YY2=YRHTO
              ELSE
              END IF
              IF(WQ.EQ.'EDGEY') THEN
                  X1=XBOT2
                  Y1=YBOT2
                  X2=XTOP2
                  Y2=YTOP2
                  XX1=XBOTO
                  YY1=YBOTO
                  XX2=XTOPO
                  YY2=YTOPO
              ELSE
              END IF
              CALL SAGPLT(III,X1,Y1,Z,NO)
              EDGE(1,3,II)=Z+ZDELZ
              CALL SAGPLT(III,X2,Y2,Z,NO)
              EDGE(2,3,II)=Z+ZDELZ
              CALL SAGPLT(III,XX1,YY1,Z,NO)
              EDGE(3,3,II)=Z+ZDELZ
              CALL SAGPLT(III,XX2,YY2,Z,NO)
              EDGE(4,3,II)=Z+ZDELZ
C
C               CYCLE THROUGH THE NEXT SURFACE
          END DO
C
C     3. THE ARRAYS NOW HAVE LOCAL X,Y AND Z VALUES STORED IN THEM
C     CONVERT THE LOCAL X ANY Y EDGES TO GLOBAL NUMBERS
C     GLOBAL VERTEX DATA IS
          DO II=STASUR,STPSUR
              X00=VERTEX(1,II)
              Y00=VERTEX(2,II)
              Z0=VERTEX(3,II)
              LX0=VERTEX(4,II)
              MX0=VERTEX(5,II)
              NX0=VERTEX(6,II)
              LY0=VERTEX(7,II)
              MY0=VERTEX(8,II)
              NY0=VERTEX(9,II)
              LZ0=VERTEX(10,II)
              MZ0=VERTEX(11,II)
              NZ0=VERTEX(12,II)
              X=EDGE(1,1,II)
              Y=EDGE(1,2,II)
              Z=EDGE(1,3,II)
C
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(Z)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              EDGE(1,1,II)=X1
              EDGE(1,2,II)=Y1
              EDGE(1,3,II)=Z1
              X=EDGE(2,1,II)
              Y=EDGE(2,2,II)
              Z=EDGE(2,3,II)
C
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(Z)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              EDGE(2,1,II)=X1
              EDGE(2,2,II)=Y1
              EDGE(2,3,II)=Z1
              X=EDGE(3,1,II)
              Y=EDGE(3,2,II)
              Z=EDGE(3,3,II)
C
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(Z)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              EDGE(3,1,II)=X1
              EDGE(3,2,II)=Y1
              EDGE(3,3,II)=Z1
              X=EDGE(4,1,II)
              Y=EDGE(4,2,II)
              Z=EDGE(4,3,II)
C
              X1=X00+((LX0*(X))+(LY0*(Y))
     1        +(LZ0*(Z)))
              Y1=Y00+((MX0*(X))+(MY0*(Y))
     1        +(MZ0*(Z)))
              Z1=Z0+((NX0*(X))+(NY0*(Y))
     1        +(NZ0*(Z)))
              EDGE(4,1,II)=X1
              EDGE(4,2,II)=Y1
              EDGE(4,3,II)=Z1
          END DO
C
C     4. NOW DETERMINE THE BEST PLACE FOR THE ROTATION POINT FOR
C               PLOT LOOK/VIEW
C
          CALL ROT1
C
C     5.  CONVERT THE GLOBAL X ANY Y EDGES
C               USING THE LOOK/VIEW VALUES
          DO II=STASUR,STPSUR
              X=EDGE(1,1,II)
              Y=EDGE(1,2,II)
              Z=EDGE(1,3,II)
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
              EDGE(1,1,II)=XN
              EDGE(1,2,II)=YN
              EDGE(1,3,II)=ZN
              X=EDGE(2,1,II)
              Y=EDGE(2,2,II)
              Z=EDGE(2,3,II)
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
              EDGE(2,1,II)=XN
              EDGE(2,2,II)=YN
              EDGE(2,3,II)=ZN
              X=EDGE(3,1,II)
              Y=EDGE(3,2,II)
              Z=EDGE(3,3,II)
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
              EDGE(3,1,II)=XN
              EDGE(3,2,II)=YN
              EDGE(3,3,II)=ZN
              X=EDGE(4,1,II)
              Y=EDGE(4,2,II)
              Z=EDGE(4,3,II)
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
              EDGE(4,1,II)=XN
              EDGE(4,2,II)=YN
              EDGE(4,3,II)=ZN
          END DO
C
C     THE ARRAYS NOW HAVE GLOBAL SURFACE EDGE DATA IN THEM
C
C     6.IF NEEDED, DETERMINE SCALE FACTORS AND PLOT RANGE1
C
          CALL PLTSC1(XMINI,XMAXI,YMINI,YMAXI)
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
          DO II=STASUR,STPSUR
              DO I=1,4
                  EDGE(I,1,II)=(EDGE(I,1,II)/SCFAX)*1000.0D0
                  EDGE(I,2,II)=(EDGE(I,2,II)/SCFAY)*1000.0D0
              END DO
          END DO
C
C     8. APPLY THE PLOT XSHIFT AND YSHIFT VALUES
          DO I=1,4
              DO II=STASUR,STPSUR
                  IF(LORIENT) CALL ORSHIFT
                  EDGE(I,1,II)=EDGE(I,1,II)+DBLE(PXSHFT)
                  EDGE(I,2,II)=EDGE(I,2,II)+3500.0D0+DBLE(PYSHFT)
              END DO
          END DO
C
C     9. SET THE PLOT JUSTIFICATION IF NEEDED
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
C     NOW
          IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
              JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
              RCL=-1
          ELSE
          END IF
          IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
              RCL=-2
              JUSOFF=5000.0D0
          ELSE
          END IF
          IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
              JUSOFF=9500.0D0-((XMAXI/SCFAX)*1000.0D0)
              RCL=-3
          ELSE
          END IF
C
          DO I=1,4
              DO II=STASUR,STPSUR
                  EDGE(I,1,II)=EDGE(I,1,II)+JUSOFF
              END DO
          END DO
C     9. PLOT GAMMA
C     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
C     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
C     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CETER OF THE DISPLAY
C
          DO I=1,4
              DO II=STASUR,STPSUR
                  EDGE(I,1,II)=EDGE(I,1,II)-5000.0D0
                  EDGE(I,2,II)=EDGE(I,2,II)-3500.0D0
              END DO
          END DO
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

          IF(DBLE(PGAMMA).NE.0.0D0) THEN
              LKG=(PII/180.0D0)*DBLE(PGAMMA)

              DO I=1,4
                  DO II=STASUR,STPSUR
                      X=EDGE(I,1,II)
                      Y=EDGE(I,2,II)
                      XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                      YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                      EDGE(I,1,II)=XNEW
                      EDGE(I,2,II)=YNEW
                  END DO
              END DO
          ELSE
          END IF
C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
          DO I=1,4
              DO II=STASUR,STPSUR
                  EDGE(I,1,II)=EDGE(I,1,II)+5000.0D0
                  EDGE(I,2,II)=EDGE(I,2,II)+3500.0D0
              END DO
          END DO
C
          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
          IF(WQ.EQ.'EDGEX') THEN
C     FIRST DO THE EDGES OF THE CLAPS
C     DRAW THE X EDGES AND RETURN
              DO J=1,4
                  IF(J.EQ.1.OR.J.EQ.2) THEN
                      DO I=STASUR,STPSUR
                          IF(I.EQ.0) THEN
                              IPST=0
                          ELSE
C                       NOT OBJECT
C
C     NOW DRAW THE X EDGES OF THE CLAP AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
C     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
C     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
C     DRAW THE EDGE. IF THE CURRENT SURFACE MATERIAL IS AIR, DON'T
C     DRAW THE EDGE.
C     IN ALL OTHER CASES, DRAW THE EDGE.
                              IF(GLANAM(I-1,2).EQ.'AIR          '.OR.
     2                        GLANAM(I-1,2).EQ.'PERFECT      '.OR.
     3                        GLANAM(I-1,2).EQ.'IDEAL        '.OR.
     3                        GLANAM(I-1,2).EQ.'REFLTIR      '.OR.
     3                        GLANAM(I-1,2).EQ.'REFLTIRO     '.OR.
     4                        GLANAM(I-1,2).EQ.'REFL         '.AND.
     5                        DABS(ALENS(46,I-1)).EQ.1.0D0.AND.
     6                        DABS(ALENS(47,I-1)).EQ.1.0D0.AND.
     7                        DABS(ALENS(48,I-1)).EQ.1.0D0.AND.
     8                        DABS(ALENS(49,I-1)).EQ.1.0D0.AND.
     9                        DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                                  IPST=0
                              ELSE
                                  IPST=1
                                  IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                              END IF
                          END IF
                          IF(GLANAM(I-1,1).EQ.'MYGLASS') THEN
                              IPST=1
                          END IF
C
                          IF(EDGE(J,1,I).GT.1.0D6) EDGE(J,1,I)=1.0D6
                          IF(EDGE(J,2,I).GT.1.0D6) EDGE(J,2,I)=1.0D6
                          IF(EDGE(J,1,I).LT.-1.0D6) EDGE(J,1,I)=-1.0D6
                          IF(EDGE(J,2,I).LT.-1.0D6) EDGE(J,2,I)=-1.0D6
                          IX=INT(EDGE(J,1,I))
                          IY=INT(EDGE(J,2,I))
                          P1ARAY(I,1,1)=IX
                          P1ARAY(I,2,1)=IY
                          P1ARAY(I,3,1)=IPST
                          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                      END DO
C     FINISHED WITH THAT EDGE, LIFT PEN
                      IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
                      COLPAS=COLEDG
                      CALL MY_COLTYP(COLPAS)
                      FIXUP=.FALSE.
                      DO IK=STASUR,STPSUR
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
                              IF(IK.EQ.0) P1ARAY(IK,3,1)=0
                              IF(IK.GT.0) THEN
                                  IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0
     1                            .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                              END IF
                              IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,IK).NE.0.0D0)
     1                        CALL drawdatasave(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1),1)
                          END IF
                      END DO
C     NOW DO THE EDGES OF THE COBS
                  ELSE
C     J NOT 1 OR 2
                  END IF
C
                  IF(J.EQ.3.OR.J.EQ.4) THEN

C     DRAW THE X EDGES AND RETURN
                      DO I=STASUR,STPSUR
C
C     NOW DRAW THE X EDGES OF THE COBS AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
                          IF(I.EQ.0) THEN
                              IPST=0
                          ELSE
C                       NOT OBJECT
C
C     NOW DRAW THE X EDGES OF THE CLAP AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
C     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
C     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
C     DRAW THE EDGE. IN ALL OTHER CASES, DRAW THE EDGE.
                              IF(GLANAM(I-1,2).EQ.'AIR          '.OR.
     2                        GLANAM(I-1,2).EQ.'PERFECT      '.OR.
     3                        GLANAM(I-1,2).EQ.'IDEAL        '.OR.
     3                        GLANAM(I-1,2).EQ.'REFLTIRO     '.OR.
     3                        GLANAM(I-1,2).EQ.'REFLTIR      '.OR.
     4                        GLANAM(I-1,2).EQ.'REFL         '.AND.
     5                        DABS(ALENS(46,I-1)).EQ.1.0D0.AND.
     6                        DABS(ALENS(47,I-1)).EQ.1.0D0.AND.
     7                        DABS(ALENS(48,I-1)).EQ.1.0D0.AND.
     8                        DABS(ALENS(49,I-1)).EQ.1.0D0.AND.
     9                        DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                                  IPST=0
                              ELSE
                                  IF(ALENS(16,I).NE.0.0D0.AND.ALENS(16,I-1).NE.0.0D0) THEN
                                      IPST=1
                                      IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                                  ELSE
                                      IPST=0
                                  END IF
                              END IF
                          END IF
                          IF(GLANAM(I-1,1).EQ.'MYGLASS') THEN
                              IPST=1
                          END IF
                          IF(EDGE(J,1,I).GT.1.0D6) EDGE(J,1,I)=1.0D6
                          IF(EDGE(J,2,I).GT.1.0D6) EDGE(J,2,I)=1.0D6
                          IF(EDGE(J,1,I).LT.-1.0D6) EDGE(J,1,I)=-1.0D6
                          IF(EDGE(J,2,I).LT.-1.0D6) EDGE(J,2,I)=-1.0D6
                          IX=INT(EDGE(J,1,I))
                          IY=INT(EDGE(J,2,I))
                          P1ARAY(I,1,1)=IX
                          P1ARAY(I,2,1)=IY
                          P1ARAY(I,3,1)=IPST
                          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                      END DO
C     FINISHED WITH THAT EDGE, LIFT PEN
                      IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
                      COLPAS=COLEDG
                      CALL MY_COLTYP(COLPAS)
                      FIXUP=.FALSE.
                      DO IK=STASUR,STPSUR
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
                              IF(IK.EQ.0) P1ARAY(IK,3,1)=0
                              IF(IK.GT.0) THEN
                                  IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0
     1                            .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                              END IF
                              IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,IK).NE.0.0D0)
     1                        CALL drawdatasave(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1),1)
                          END IF
                      END DO
                  ELSE
C     J NOT 3 OR 4
                  END IF
              END DO
          ELSE
C     NOT EDGEX
          END IF
C
          IF(WQ.EQ.'EDGEY') THEN
C     FIRST DO THE EDGES OF THE CLAPS
C     DRAW THE Y EDGES AND RETURN
              DO J=1,4
                  IF(J.EQ.1.OR.J.EQ.2) THEN
                      DO I=STASUR,STPSUR
                          IF(I.EQ.0) THEN
                              IPST=0
                          ELSE
C                       NOT OBJECT
C
C     NOW DRAW THE Y EDGES OF THE CLAP AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
C     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
C     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
C     DRAW THE EDGE. IN ALL OTHER CASES, DRAW THE EDGE.
                              IF(GLANAM(I-1,2).EQ.'AIR          '.OR.
     2                        GLANAM(I-1,2).EQ.'PERFECT      '.OR.
     3                        GLANAM(I-1,2).EQ.'IDEAL        '.OR.
     3                        GLANAM(I-1,2).EQ.'REFLTIRO     '.OR.
     3                        GLANAM(I-1,2).EQ.'REFLTIR      '.OR.
     4                        GLANAM(I-1,2).EQ.'REFL         '.AND.
     5                        DABS(ALENS(46,I-1)).EQ.1.0D0.AND.
     6                        DABS(ALENS(47,I-1)).EQ.1.0D0.AND.
     7                        DABS(ALENS(48,I-1)).EQ.1.0D0.AND.
     8                        DABS(ALENS(49,I-1)).EQ.1.0D0.AND.
     9                        DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                                  IPST=0
                              ELSE
                                  IPST=1
                                  IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                              END IF
                          END IF
                          IF(GLANAM(I-1,1).EQ.'MYGLASS') THEN
                              IPST=1
                          END IF
C
                          IF(EDGE(J,1,I).GT.1.0D6) EDGE(J,1,I)=1.0D6
                          IF(EDGE(J,2,I).GT.1.0D6) EDGE(J,2,I)=1.0D6
                          IF(EDGE(J,1,I).LT.-1.0D6) EDGE(J,1,I)=-1.0D6
                          IF(EDGE(J,2,I).LT.-1.0D6) EDGE(J,2,I)=-1.0D6
                          IX=INT(EDGE(J,1,I))
                          IY=INT(EDGE(J,2,I))
                          P1ARAY(I,1,1)=IX
                          P1ARAY(I,2,1)=IY
                          P1ARAY(I,3,1)=IPST
                          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                      END DO
C     FINISHED WITH THAT EDGE, LIFT PEN
                      IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
                      COLPAS=COLEDG
                      CALL MY_COLTYP(COLPAS)
                      FIXUP=.FALSE.
                      DO IK=STASUR,STPSUR
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
                              IF(IK.EQ.0) P1ARAY(IK,3,1)=0
                              IF(IK.GT.0) THEN
                                  IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0
     1                            .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                              END IF
                              IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,IK).NE.0.0D0)
     1                        CALL drawdatasave(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1),1)
                          END IF
                      END DO
C     NOW DO THE EDGES OF THE COBS
                  ELSE
C     J NOT 1 OR 2
                  END IF
C
                  IF(J.EQ.3.OR.J.EQ.4) THEN

C     DRAW THE Y EDGES AND RETURN
                      DO I=STASUR,STPSUR
C
C     NOW DRAW THE Y EDGES OF THE COBS AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
                          IF(I.EQ.0) THEN
                              IPST=0
                          ELSE
C                       NOT OBJECT
C
C     NOW DRAW THE X EDGES OF THE CLAP AT SURFACE I
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
C
C     IF THE CURRENT SURFACE IS PRECEEDED BY AIR OR REFL
C     AND ALL THE REFRACTIVE INDICES ARE 1 OR -1, DON'T
C     DRAW THE EDGE. IN ALL OTHER CASES, DRAW THE EDGE.
                              IF(GLANAM(I-1,2).EQ.'AIR          '.OR.
     2                        GLANAM(I-1,2).EQ.'PERFECT      '.OR.
     3                        GLANAM(I-1,2).EQ.'IDEAL        '.OR.
     3                        GLANAM(I-1,2).EQ.'REFLTIR      '.OR.
     3                        GLANAM(I-1,2).EQ.'REFLTIRO     '.OR.
     4                        GLANAM(I-1,2).EQ.'REFL         '.AND.
     5                        DABS(ALENS(46,I-1)).EQ.1.0D0.AND.
     6                        DABS(ALENS(47,I-1)).EQ.1.0D0.AND.
     7                        DABS(ALENS(48,I-1)).EQ.1.0D0.AND.
     8                        DABS(ALENS(49,I-1)).EQ.1.0D0.AND.
     9                        DABS(ALENS(50,I-1)).EQ.1.0D0) THEN
                                  IPST=0
                              ELSE
                                  IF(ALENS(16,I).NE.0.0D0.AND.ALENS(16,I-1).NE.0.0D0) THEN
                                      IPST=1
                                      IF(I.EQ.STASUR.AND.GLANAM(I,2).EQ.'AIR') IPST=0
                                  ELSE
                                      IPST=0
                                  END IF
                              END IF
                          END IF
                          IF(GLANAM(I-1,1).EQ.'MYGLASS') THEN
                              IPST=1
                          END IF
                          IF(EDGE(J,1,I).GT.1.0D6) EDGE(J,1,I)=1.0D6
                          IF(EDGE(J,2,I).GT.1.0D6) EDGE(J,2,I)=1.0D6
                          IF(EDGE(J,1,I).LT.-1.0D6) EDGE(J,1,I)=-1.0D6
                          IF(EDGE(J,2,I).LT.-1.0D6) EDGE(J,2,I)=-1.0D6
                          IX=INT(EDGE(J,1,I))
                          IY=INT(EDGE(J,2,I))
                          P1ARAY(I,1,1)=IX
                          P1ARAY(I,2,1)=IY
                          P1ARAY(I,3,1)=IPST
                          IF(.NOT.PLEXIS) PLEXIS=.TRUE.
                      END DO
C     FINISHED WITH THAT EDGE, LIFT PEN
                      IPST=0
C     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
C
                      COLPAS=COLEDG
                      CALL MY_COLTYP(COLPAS)
                      FIXUP=.FALSE.
                      DO IK=STASUR,STPSUR
                          IF(ALENS(127,IK).NE.0.0D0.OR.IK.NE.STASUR.AND.
     1                    ALENS(127,IK-1).NE.0.0D0) THEN
                          ELSE
C
                              IF(IK.EQ.0) P1ARAY(IK,3,1)=0
                              IF(IK.GT.0) THEN
                                  IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0
     1                            .OR.P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) P1ARAY(IK,3,1)=0
                              END IF
C
                              IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,IK).NE.0.0D0)
     1                        CALL drawdatasave(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1),1)
                          END IF
                      END DO
                  ELSE
C     J NOT 3 OR 4
                  END IF
              END DO
          ELSE
C     NOT EDGEY
          END IF
C
C
C     HERE WE DO THE PLOT LI AND PLOT AXIS DRAWING
          IF(.NOT.VIGFLG.AND.PLTVIG) THEN
              CALL VIGSHO
              VIGFLG=.TRUE.
          ELSE
          END IF
          DEALLOCATE(EDGE,STAT=ALLOERR)
          RETURN
      END

C SUB PLTRHFOOT.FOR
      SUBROUTINE PLTRHFOOT
          USE GLOBALS
C
          IMPLICIT NONE
          LOGICAL EXIS103
          REAL*8 XFTVAL,YFTVAL,XNEW,YNEW,X,Y,LKG
          DIMENSION XFTVAL(:),YFTVAL(:)
          ALLOCATABLE :: XFTVAL,YFTVAL
          INTEGER ALLOERR,NUMBER_OF_RAYS,IX,IY,I,COLPAS
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PLOT RHFOOT" PLOTS THE RAY HISTORY FOOT PRINT FILE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'GENERATED BY THE RHFOOT COMMAND FROM THE SHORT RAYHIST.DAT FILE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PLOT RHFOOT" TAKES NO STRING OR NUMERIC IMPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
C
C       DOES AN RHFOOT.DAT FILE EXIST TO PLOT
          EXIS103=.FALSE.
          INQUIRE(FILE=trim(HOME)//'RHFOOT.DAT', EXIST=EXIS103)
          IF(.NOT.EXIS103) THEN
              WRITE(OUTLYNE,*) 'NO RHFOOT.DAT FILE EXISTS TO BE PLOTTED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'NO ACTON TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          OPEN(UNIT=103,FILE=trim(HOME)//'RHFOOT.DAT')
          READ(UNIT=103,FMT=*) NUMBER_OF_RAYS
          ALLOCATE(XFTVAL(1:NUMBER_OF_RAYS),YFTVAL(1:NUMBER_OF_RAYS),
     1    STAT=ALLOERR)
          DO I=1,NUMBER_OF_RAYS
              READ(UNIT=103,FMT=*) XFTVAL(I),YFTVAL(I)
          END DO
          CALL CLOSE_FILE(103,1)


C
C     APPLY SCALE FACTORS
C
C     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
C     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
C     TWO STEPS.
C
C     THE WORLD X PLOTS TO THE PLOTTER X
C     THE WORLD Y PLOTS TO THE PLOTTER Y
C
C     CONVERT USING AN APPROPRIATE SCALE FACTOR
          DO I=1,NUMBER_OF_RAYS
C
              XFTVAL(I)=(XFTVAL(I)/SCFAX)*1000.0D0
              YFTVAL(I)=(YFTVAL(I)/SCFAY)*1000.0D0
C
C     APPLY THE XSHIFT AND YSHIFT VALUES
              IF(LORIENT) CALL ORSHIFT
              XFTVAL(I)=XFTVAL(I)+DBLE(PXSHFT)
              YFTVAL(I)=YFTVAL(I)+3500.0D0+DBLE(PYSHFT)
C
C     SET THE PLOT JUSTIFICATION IF NEEDED
C     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
C
              XFTVAL(I)=XFTVAL(I)+5000.0D0
C
C     FIRST SHIFT THE COORDINATE ORIGIN TO THE CENTER OF THE DISPLAY
C
              XFTVAL(I)=XFTVAL(I)-5000.0D0
              YFTVAL(I)=YFTVAL(I)-3500.0D0
C
C     THE SCREEN COORDINATE IN REAL*8 IN THE SHIFTED COORDINATE
C     FRAME IS NOW X AND Y

              IF(DBLE(PGAMMA).NE.0.0D0) THEN
                  LKG=(PII/180.0D0)*DBLE(PGAMMA)

                  X=XFTVAL(I)
                  Y=YFTVAL(I)
                  XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                  YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                  XFTVAL(I)=XNEW
                  YFTVAL(I)=YNEW
              END IF
C     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
C     LEFT HAND CORNER
C
              XFTVAL(I)=XFTVAL(I)+5000.0D0
              YFTVAL(I)=YFTVAL(I)+3500.0D0
C
C     NOW DRAW THE FOOT PRINT MARK AT SURFACE
C     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
              IF(.NOT.PLEXIS) PLEXIS=.TRUE.
              IF(XFTVAL(I).GT.1.0D6) XFTVAL(I)=1.0D6
              IF(YFTVAL(I).GT.1.0D6) YFTVAL(I)=1.0D6
              IF(XFTVAL(I).LT.-1.0D6) XFTVAL(I)=-1.0D6
              IF(YFTVAL(I).LT.-1.0D6) YFTVAL(I)=-1.0D6
              IX=INT(XFTVAL(I))
              IY=INT(YFTVAL(I))
              IF(.NOT.PLEXIS) PLEXIS=.TRUE.
C
C     NOW ISSUE THE PLOTTING COMMANDS
C
C     LINE TYPE SETTING
C     SAME COLOR AS AXIS PLOTTING IN OPTICAL LAYOUT PLOTS
              COLPAS=COLAXS
              CALL MY_COLTYP(COLPAS)
              OLLNTP=LNTYPE
              LNTYPE=0
C     MOVE TO POINT WITH PEN UP
              CALL PENMV1A(IX,IY,0)
C     MOVE TO Y-START POINT WITH PEN UP
              CALL PENMV1A(IX,IY-10,0)
C     DROP THE PEN
              CALL PENMV1A(IX,IY-10,1)
C     MOVE TO Y-END POINT WITH PEN DOWN
              CALL PENMV1A(IX,IY+10,1)
C     RAISE THE PEN
              CALL PENMV1A(IX,IY+10,0)
C     MOVE TO X-START POINT WITH PEN UP
              CALL PENMV1A(IX-10,IY,0)
C     DROP THE PEN
              CALL PENMV1A(IX-10,IY,1)
C     MOVE TO X-END POINT WITH PEN DOWN
              CALL PENMV1A(IX+10,IY,1)
C     RAISE THE PEN
              CALL PENMV1A(IX+10,IY,0)
          END DO
C
          LNTYPE=OLLNTP
C
          DEALLOCATE(XFTVAL,YFTVAL,STAT=ALLOERR)
          RETURN
      END
