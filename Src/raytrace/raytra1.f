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

C SUB IRAY.FOR
      SUBROUTINE IRAY
          USE GLOBALS

C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE IRAY.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CMD LEVEL COMMAND IRAYA.
C
!        INTEGER I,J
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"IRAYA" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       DEFAULT QUALIFIER IS NOTHING (NO QUALIFIER)
          CACOCH=0
          IF(WQ.EQ.'CAOB') CACOCH=1
C
C       SET DEFAULT NUMERICS
          IF(DF1.EQ.1) W1=0.0D0
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=SYSTEM1(11)
          IF(DF3.EQ.1) WW3=W3
          IF(DF4.EQ.1) W4=1.0D0
          IF(DF4.EQ.1) WW4=1.0D0
          IF(W1.LT.0.0D0.OR.W1.GT.180.0D0) THEN
              OUTLYNE='NUMERIC WORD 1, THE THETA ANGLE,,'
              CALL SHOWIT(1)
              OUTLYNE='CAN RANGE FROM 0 TO 180 DEGREES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LT.0.0D0.OR.W2.GT.360D0) THEN
              OUTLYNE='NUMERIC WORD 2, THE PHI ANGLE,,'
              CALL SHOWIT(1)
              OUTLYNE='CAN RANGE FROM 0 TO 360 DEGREES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W4.GT.1.0D0.OR.W4.LE.0.0D0) THEN
              OUTLYNE='NUMERIC WORD 4, THE STARTING RAY INTENSITY,'
              CALL SHOWIT(1)
              OUTLYNE='MUST BE GREATER THE ZERO AND NO GREATER THAN 1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          WVN=W3
C       CURLAM TRACK CURRENT WAVELENGTH TRACED VIA COMMON
C       KURLAM IN DATLEN.FOR
          CURLAM=W3
C       THE CONTROL WAVELENGTH
C       NUMERIC WORD 5 IS NOT USED IN THE CMD LEVEL
C       "IRAYA" COMMAND
C       CHECK FOR NATURE OF QUALIFIER WORD
          IF(SQ.EQ.1) THEN
              IF(WC.EQ.'IRAYA'.AND.WQ.NE.'CAOB') THEN
                  OUTLYNE='INVALID QUALIFIER USED WITH "IRAYA"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       NO QUALIFIER,PROCEED
          END IF
C       CHECK FOR CORRECT WAVELENGTH BOUNDS
          IF(INT(W3).NE.1.AND.
     1    INT(W3).NE.2.AND.
     1    INT(W3).NE.3.AND.
     1    INT(W3).NE.4.AND.
     1    INT(W3).NE.5.AND.
     1    INT(W3).NE.6.AND.
     1    INT(W3).NE.7.AND.
     1    INT(W3).NE.8.AND.
     1    INT(W3).NE.9.AND.
     1    INT(W3).NE.10) THEN
              OUTLYNE='WAVELENGTH NUMBER BEYOND LEGAL BOUNDS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CHECK IF REFEXT=.FALSE. IF SO STOP AND PRINT  MESSAGE
C
          IF(.NOT.REFEXT) THEN
C       NO CHIEF RAY EXISTS, STOP
              IF(MSG) THEN
                  OUTLYNE=
     1            'A CHIEF RAY DOES NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='A RAY WILL BE TRACED WITHOUT A CHIEF RAY'
                  CALL SHOWIT(1)
                  OUTLYNE='CHECK YOUR RESULTS'
                  CALL SHOWIT(1)
              END IF
          END IF
C       SET RAYCODS
          RAYCOD(1)=0
          RAYCOD(2)=-1
C
C       SET STOPP=0
          STOPP=0
C       SET RAYEXT TO TRUE
          RAYEXT=.TRUE.
C       SET FAIL TO FALSE
          FAIL=.FALSE.
C
C       NOW THE DEFAULTS HAVE BEEN SET
C       NOW CALL THE SUBROUTINE WHICH STARTS THE
C       RAY TRACE. THIS IS RAYTRA.
C
          WWQ=WQ
          WW1=W1
          WW2=W2
          WW3=W3
          WW4=W4
          WVN=W3
C       THE CALL TO RAYTRA HAS INPUTS:
C               QUALIFIER
C               WW1
C               WW2
C               WW3
C               WW4
C               CACOCH
C               FAIL
C       TRACE RAY AND RETURN
          IF(GRASET.OR.DXFSET) PLTRAY=.TRUE.
          ITRACE=.TRUE.
          CALL RAYTRA
          ITRACE=.FALSE.
          IF(GRASET.OR.DXFSET) PLTRAY=.FALSE.
          IF(STOPP.EQ.1) THEN
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              IF(F34.NE.1) CALL MACFAL
              RETURN
          ELSE
              STOPP=0
              RAYEXT=.TRUE.
          END IF
C
C       THE RETURN WILL SEND BACK VIA THE COMMON/RAYCMN COMMON
C       THE FULL RAY DATA FOR THE FULLY AIMMED AND TRACED
C       RAY.
C
          RETURN
      END
C SUB SIZES.FOR
      SUBROUTINE SIZES
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SIZES
C
          INTEGER ALLOERR,FWARN,RWARN,J,I,K,L,M,N,KK
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          REAL*8 HY,HX,YF,XF,YR,XR,HXMAX,HXMIN,HYMAX,HYMIN
     1    ,XFFIX,YFFIX,XRFIX,YRFIX,XCENPOS,YCENPOS,RMAX,XLO,XHI,YLO,YHI
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsub.inc'
          REAL*8 VERARRAY
          DIMENSION VERARRAY(:,:)
          ALLOCATABLE :: VERARRAY
          DEALLOCATE(VERARRAY,STAT=ALLOERR)
          ALLOCATE(VERARRAY(1:220,0:MAXSUR),STAT=ALLOERR)
C
C       CHECK FOR STRING INPUT
          IF(STI.EQ.1) THEN
              OUTLYNE='"LIMRAYS" CALCULATES YZ AND XZ-PLANE LIMIT RAYS'
              CALL SHOWIT(1)
              OUTLYNE='QUALIFIERS ARE "REAL", "VREAL" OR "PARAX"'
              CALL SHOWIT(1)
              OUTLYNE='DEFAULT QUALIFIER IS "REAL"'
              CALL SHOWIT(1)
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"LIMRAYS" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
C       CHECK FOR NUMERIC INPUT
          IF(S1.EQ.1.AND.
     1    WQ.NE.'VREAL') THEN
              OUTLYNE=
     1        'ONLY "LIMRAYS VREAL" CAN TAKE NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"LIMRAYS" TAKE NO NUMERIC WORD #2 TO #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
          IF(S1.EQ.1.AND.WQ.EQ.'VREAL') THEN
              IF(W1.LT.10.0D0.OR.W1.GT.100.0D0) THEN
                  OUTLYNE=
     1            '"LIMRAYS VREAL" CAN TAKE NUMERIC WORD #1 FROM 10.0 TO 100.0'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  DEALLOCATE(VERARRAY,STAT=ALLOERR)
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'VREAL'.AND.S1.EQ.0) THEN
              S1=1
              DF1=0
              W1=25.0D0
              N=INT(W1)
          END IF
          IF(SQ.EQ.0) THEN
              SQ=1
              WQ='REAL'
          END IF
          IF(WQ.NE.'REAL'.AND.WQ.NE.'PARAX'.AND.WQ.NE.'VREAL') THEN
              OUTLYNE=
     1        '"LIMRAYS" ONLY TAKES "REAL", "VREAL" OR "PARAX" AS QUALIFIERS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
          IF(WQ.EQ.'PARAX') THEN
              FWARN=0
              RWARN=0
              WRITE(OUTLYNE,100)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2002)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2003) F12
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
              DO I=0,INT(SYSTEM1(20))
                  IF(I.EQ.0) THEN
                      IF(SYSTEM1(16).NE.0.0D0) HX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                      IF(SYSTEM1(16).EQ.0.0D0) HX=DABS(PXTRAX(1,I))
                      IF(SYSTEM1(14).NE.0.0D0) HY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                      IF(SYSTEM1(14).EQ.0.0D0) HY=DABS(PXTRAY(1,I))
                  ELSE
                      HX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                      HY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                  END IF
                  WRITE(OUTLYNE,200) I,HX,HY
                  CALL SHOWIT(0)
              END DO
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
          WRITE(OUTLYNE,100)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,2002)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,2003) F12
          CALL SHOWIT(0)
          WRITE(OUTLYNE,1001)
          CALL SHOWIT(0)
          IF(WQ.EQ.'REAL'.OR.WQ.EQ.'VREAL') THEN
              RWARN=0
              FWARN=0
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              LDIF2=.TRUE.
              LDIF=.TRUE.
C     EIGHT PLACES AROUND THE FULL FOV
              KK=0
              DO K=0,8
                  IF(ALENS(9,NEWOBJ).NE.2.0D0.AND.ALENS(9,NEWOBJ).NE.4.0D0.AND.
     1            ALENS(127,NEWOBJ).EQ.0.0D0) THEN
                      IF(K.EQ.0) THEN
                          YF=0.0D0
                          XF=1.0D0
                      END IF
                      IF(K.EQ.1) THEN
                          YF=DSQRT(2.0D0)/2.0D0
                          XF=DSQRT(2.0D0)/2.0D0
                      END IF
                      IF(K.EQ.2) THEN
                          YF=1.0D0
                          XF=0.0D0
                      END IF
                      IF(K.EQ.3) THEN
                          YF=DSQRT(2.0D0)/2.0D0
                          XF=-DSQRT(2.0D0)/2.0D0
                      END IF
                      IF(K.EQ.4) THEN
                          YF=0.0D0
                          XF=-1.0D0
                      END IF
                      IF(K.EQ.5) THEN
                          YF=-DSQRT(2.0D0)/2.0D0
                          XF=-DSQRT(2.0D0)/2.0D0
                      END IF
                      IF(K.EQ.6) THEN
                          YF=-1.0D0
                          XF=0.0D0
                      END IF
                      IF(K.EQ.7) THEN
                          YF=-DSQRT(2.0D0)/2.0D0
                          XF=DSQRT(2.0D0)/2.0D0
                      END IF
                      IF(K.EQ.8) THEN
                          YF=0.0D0
                          XF=0.0D0
                      END IF
                  ELSE
                      IF(ALENS(9,NEWOBJ).EQ.2.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
                          IF(K.EQ.0) THEN
                              YF=0.0D0
                              XF=1.0D0
                          END IF
                          IF(K.EQ.1) THEN
                              YF=1.0D0
                              XF=1.0D0
                          END IF
                          IF(K.EQ.2) THEN
                              YF=1.0D0
                              XF=0.0D0
                          END IF
                          IF(K.EQ.3) THEN
                              YF=1.0D0
                              XF=-1.0D0
                          END IF
                          IF(K.EQ.4) THEN
                              YF=0.0D0
                              XF=-1.0D0
                          END IF
                          IF(K.EQ.5) THEN
                              YF=-1.0D0
                              XF=-1.0D0
                          END IF
                          IF(K.EQ.6) THEN
                              YF=-1.0D0
                              XF=0.0D0
                          END IF
                          IF(K.EQ.7) THEN
                              YF=-1.0D0
                              XF=1.0D0
                          END IF
                          IF(K.EQ.8) THEN
                              YF=0.0D0
                              XF=0.0D0
                          END IF
                      END IF
                      IF(ALENS(9,NEWOBJ).EQ.4.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
                          YFFIX=DABS(ALENS(14,NEWOBJ)/ALENS(10,NEWOBJ))
                          XFFIX=DABS(ALENS(14,NEWOBJ)/ALENS(11,NEWOBJ))
                          IF(K.EQ.0) THEN
                              YF=0.0D0
                              XF=1.0D0
                          END IF
                          IF(K.EQ.1) THEN
                              YF=1.0D0-(0.02928D0*YFFIX)
                              XF=1.0D0-(0.02928D0*XFFIX)
                          END IF
                          IF(K.EQ.2) THEN
                              YF=1.0D0
                              XF=0.0D0
                          END IF
                          IF(K.EQ.3) THEN
                              YF=1.0D0-(0.02928D0*YFFIX)
                              XF=-1.0D0+(0.02928D0*XFFIX)
                          END IF
                          IF(K.EQ.4) THEN
                              YF=0.0D0
                              XF=-1.0D0
                          END IF
                          IF(K.EQ.5) THEN
                              YF=-1.0D0+(0.02928D0*YFFIX)
                              XF=-1.0D0+(0.02928D0*XFFIX)
                          END IF
                          IF(K.EQ.6) THEN
                              YF=-1.0D0
                              XF=0.0D0
                          END IF
                          IF(K.EQ.7) THEN
                              YF=-1.0D0+(0.02928D0*YFFIX)
                              XF=1.0D0-(0.02928D0*XFFIX)
                          END IF
                          IF(K.EQ.8) THEN
                              YF=0.0D0
                              XF=0.0D0
                          END IF
                      END IF
                  END IF
C     EIGHT PLACES AROUND THE APERTURE
C     DO THE FOB
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC='FOB     '
                  WQ='        '
                  SQ=0
                  SST=0
                  STI=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=1
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=0
                  SN=1
                  W1=YF
                  W2=XF
                  W3=0.0D0
                  W4=SYSTEM1(11)
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  CALL FFOB
                  IF(.NOT.REFEXT) FWARN=1
                  REST_KDP(1)=RESTINPT(1)
C
                  IF(WQ.NE.'VREAL') THEN
                      XLO=-1.0D0
                      XHI=1.0D0
                      YLO=-1.0D0
                      YHI=1.0D0
                  END IF
                  IF(WQ.EQ.'VREAL') THEN
                      CALL VIGCAL(N,XLO,XHI,1)
                      CALL VIGCAL(N,YLO,YHI,2)
                      MSG=.FALSE.
                  END IF
                  DO L=0,7
                      KK=KK+1
                      M=(3*KK)-2
                      IF(ALENS(9,NEWREF).NE.2.0D0.AND.ALENS(9,NEWREF).NE.4.0D0
     1                .AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
                          IF(L.EQ.0) THEN
                              YR=0.0D0
                              XR=XHI
                          END IF
                          IF(L.EQ.1) THEN
                              YR=YHI*DSQRT(2.0D0)/2.0D0
                              XR=XHI*DSQRT(2.0D0)/2.0D0
                          END IF
                          IF(L.EQ.2) THEN
                              YR=YHI
                              XR=0.0D0
                          END IF
                          IF(L.EQ.3) THEN
                              YR=YHI*DSQRT(2.0D0)/2.0D0
                              XR=XLO*DSQRT(2.0D0)/2.0D0
                          END IF
                          IF(L.EQ.4) THEN
                              YR=0.0D0
                              XR=XLO
                          END IF
                          IF(L.EQ.5) THEN
                              YR=YLO*DSQRT(2.0D0)/2.0D0
                              XR=XLO*DSQRT(2.0D0)/2.0D0
                          END IF
                          IF(L.EQ.6) THEN
                              YR=YLO
                              XR=0.0D0
                          END IF
                          IF(L.EQ.7) THEN
                              YR=YLO*DSQRT(2.0D0)/2.0D0
                              XR=XHI*DSQRT(2.0D0)/2.0D0
                          END IF
                      ELSE
                          IF(ALENS(9,NEWREF).EQ.2.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
                              IF(L.EQ.0) THEN
                                  YR=0.0D0
                                  XR=XHI
                              END IF
                              IF(L.EQ.1) THEN
                                  YR=YHI
                                  XR=XHI
                              END IF
                              IF(L.EQ.2) THEN
                                  YR=YHI
                                  XR=0.0D0
                              END IF
                              IF(L.EQ.3) THEN
                                  YR=YHI
                                  XR=XLO
                              END IF
                              IF(L.EQ.4) THEN
                                  YR=0.0D0
                                  XR=XLO
                              END IF
                              IF(L.EQ.5) THEN
                                  YR=YLO
                                  XR=XLO
                              END IF
                              IF(L.EQ.6) THEN
                                  YR=YLO
                                  XR=0.0D0
                              END IF
                              IF(L.EQ.7) THEN
                                  YR=YLO
                                  XR=XHI
                              END IF
                          END IF
                          IF(ALENS(9,NEWREF).EQ.4.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
                              YRFIX=DABS(ALENS(14,NEWREF)/ALENS(10,NEWREF))
                              XRFIX=DABS(ALENS(14,NEWREF)/ALENS(11,NEWREF))
                              IF(L.EQ.0) THEN
                                  YR=0.0D0
                                  XR=XHI
                              END IF
                              IF(L.EQ.1) THEN
                                  YR=YHI-(0.02928D0*YRFIX*YHI)
                                  XR=XHI-(0.02928D0*XRFIX*XHI)
                              END IF
                              IF(L.EQ.2) THEN
                                  YR=YHI
                                  XR=0.0D0
                              END IF
                              IF(L.EQ.3) THEN
                                  YR=YHI-(0.02928D0*YRFIX*YHI)
                                  XR=XLO-(0.02928D0*XRFIX*XLO)
                              END IF
                              IF(L.EQ.4) THEN
                                  YR=0.0D0
                                  XR=-XLO
                              END IF
                              IF(L.EQ.5) THEN
                                  YR=YLO-(0.02928D0*YRFIX*YLO)
                                  XR=XLO-(0.02928D0*XRFIX*XLO)
                              END IF
                              IF(L.EQ.6) THEN
                                  YR=-YLO
                                  XR=0.0D0
                              END IF
                              IF(L.EQ.7) THEN
                                  YR=YLO-(0.02928D0*YFFIX*YLO)
                                  XR=XHI-(0.02928D0*XFFIX*XHI)
                              END IF
                          END IF
                      END IF
C     TRACE THE RAY
                      SAVE_KDP(1)=SAVEINPT(1)
                      WQ='        '
                      SQ=0
                      SST=0
                      STI=0
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=1
                      DF5=1
                      S1=1
                      S2=1
                      S3=1
                      S4=0
                      S5=0
                      SN=1
                      W1=YR
                      W2=XR
                      W3=SYSTEM1(11)
                      WC='RAY     '
                      CALL RRAY
                      IF(.NOT.RAYEXT) RWARN=1
                      REST_KDP(1)=RESTINPT(1)
C     SAVE RAY DATA
                      DO I=0,INT(SYSTEM1(20))
                          VERARRAY(M,I)=RAYRAY(1,I)
                          VERARRAY(M+1,I)=RAYRAY(2,I)
                          VERARRAY(M+2,I)=DSQRT((RAYRAY(1,I)**2)+(RAYRAY(2,I)**2))
                      END DO
                  END DO
              END DO
              LDIF2=OLDLDIF2
              LDIF=OLDLDIF
C
C     PROCESS DATA
              DO I=0,INT(SYSTEM1(20))
                  HXMAX=-1.0D10
                  HYMAX=-1.0D10
                  HXMIN=1.0D10
                  HYMIN=1.0D10
                  DO J=1,214,3
                      IF(VERARRAY(J,I).GT.HXMAX) HXMAX=VERARRAY(J,I)
                      IF(VERARRAY(J,I).LT.HXMIN) HXMIN=VERARRAY(J,I)
                  END DO
                  DO J=2,215,3
                      IF(VERARRAY(J,I).GT.HYMAX) HYMAX=VERARRAY(J,I)
                      IF(VERARRAY(J,I).LT.HYMIN) HYMIN=VERARRAY(J,I)
                  END DO
                  WRITE(OUTLYNE,2000) I,HXMIN,HXMAX,HYMIN,HYMAX
                  CALL SHOWIT(0)
              END DO
C
              WRITE(OUTLYNE,100)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1002)
              CALL SHOWIT(0)
              DO I=0,INT(SYSTEM1(20))
                  RMAX=-1.0D10
                  HXMAX=-1.0D10
                  HYMAX=-1.0D10
                  HXMIN=1.0D10
                  HYMIN=1.0D10
                  DO J=1,214,3
                      IF(VERARRAY(J,I).GT.HXMAX) HXMAX=VERARRAY(J,I)
                      IF(VERARRAY(J,I).LT.HXMIN) HXMIN=VERARRAY(J,I)
                      XCENPOS=(HXMAX+HXMIN)/2.0D0
                  END DO
                  DO J=2,215,3
                      IF(VERARRAY(J,I).GT.HYMAX) HYMAX=VERARRAY(J,I)
                      IF(VERARRAY(J,I).LT.HYMIN) HYMIN=VERARRAY(J,I)
                      YCENPOS=(HYMAX+HYMIN)/2.0D0
                  END DO
                  DO J=3,216,3
                      IF(VERARRAY(J,I).GT.RMAX) RMAX=VERARRAY(J,I)
                  END DO
                  WRITE(OUTLYNE,2001) I,RMAX,XCENPOS,YCENPOS
                  CALL SHOWIT(0)
              END DO
              IF(FWARN.NE.0) WRITE(OUTLYNE,2004)
              IF(RWARN.NE.0) WRITE(OUTLYNE,2005)
              CALL SHOWIT(0)
              IF(FWARN.NE.0.OR.RWARN.NE.0) THEN
                  WRITE(OUTLYNE,2006)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2007)
                  CALL SHOWIT(0)
              END IF
 100          FORMAT(1X)
 101          FORMAT('SURF#',2X,'X-LIMIT RAY HT ',2X,'Y-LIMIT RAY HT ')
 200          FORMAT(I3,2X,D15.8,2X,D15.8)
 1001         FORMAT('SURF#',1X,' X-MIN  RAY HT ',2X,' X-MAX  RAY HT ',
     1        2X,' Y-MIN  RAY HT ',2X,' Y-MAX  RAY HT ')
 1002         FORMAT('SURF#',2X,'MAX RAD. DIST.',2X,' X-CENTER POS.',
     1        2X,' Y-CENTER POS.')
 2000         FORMAT(I3,2X,D15.8,2X,D15.8,2X,D15.8,2X,D15.8)
 2001         FORMAT(I3,2X,D15.8,2X,D15.8,2X,D15.8)
 2002         FORMAT('LIMIT RAY DATA AT THE CONTROL WAVELENGTH')
 2003         FORMAT('AND AT ALTERNATE CONFIGURATION ',I2)
 2004         FORMAT('WARNING: SOME CHIEF RAYS COULD NOT BE TRACED')
 2005         FORMAT('WARNING: SOME MARGINAL RAYS COULD NOT BE TRACED')
 2006         FORMAT('LIMIT RAY DATA MAY BE IN ERROR')
 2007         FORMAT('CHANGE OBJECT HT. OR REF. AP. HT. AND RE-RUN')
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
      END
C SUB SETCLAP.FOR
      SUBROUTINE SETCLAP
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SETCLAP
C
          INTEGER FWARN,RWARN,J,I,K,L,M,N,KK,ALLOERR
C
          LOGICAL OLDLDIF,OLDLDIF2
C
          REAL*8 HY,HX,YF,XF,YR,XR,HXMAX,HXMIN,HYMAX,HYMIN
     1    ,XFFIX,YFFIX,XRFIX,YRFIX,XCENPOS,YCENPOS,RMAX,XLO,XHI,YLO,YHI
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsub.inc'
          REAL*8 VERARRAY,XRAD,YRAD,RRAD
          DIMENSION VERARRAY(:,:)
          ALLOCATABLE :: VERARRAY
          DEALLOCATE(VERARRAY,STAT=ALLOERR)
          ALLOCATE(VERARRAY(1:220,0:MAXSUR),STAT=ALLOERR)
C
C       CHECK FOR STRING INPUT
          IF(STI.EQ.1) THEN
              OUTLYNE='"SETCLAP" SETS CLAPS ON SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='QUALIFIERS ARE "REAL", "VREAL" OR "PARAX"'
              CALL SHOWIT(1)
              OUTLYNE='DEFAULT QUALIFIER IS "REAL"'
              CALL SHOWIT(1)
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"SETCLAP" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
          IF(S4.EQ.1.or.S5.EQ.1) THEN
              OUTLYNE='"SETCLAP" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
          IF(DF1.EQ.1) W1=1.0
          IF(W1.EQ.0.0) THEN
              W1=1.0
          END IF
          IF(DF2.EQ.1) W2=SYSTEM1(20)-1.0D0
          IF(W2.EQ.0.0) W2=SYSTEM1(20)
          IF(W1.LT.0.0D0.OR.W2.GT.SYSTEM1(20).OR.
     1    W2.LT.W1) THEN
              WRITE(OUTLYNE,*)'SURFACE NUMBER(S) BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF

C       HANDLE NO SURFACES
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO PARAXIAL DATA EXISTS'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
C       CHECK FOR NUMERIC INPUT
          IF(S3.EQ.1.AND.
     1    WQ.NE.'VREAL') THEN
              OUTLYNE=
     1        'ONLY "SETCLAP VREAL" CAN TAKE NUMERIC WORD #3 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
          IF(S3.EQ.1.AND.WQ.EQ.'VREAL') THEN
              IF(W3.LT.10.0D0.OR.W3.GT.100.0D0) THEN
                  OUTLYNE=
     1            '"SETCLAP VREAL" CAN TAKE NUMERIC WORD #1 FROM 10.0 TO 100.0'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  DEALLOCATE(VERARRAY,STAT=ALLOERR)
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'VREAL'.AND.S1.EQ.0) THEN
              S3=1
              DF3=0
              W3=25.0D0
              N=INT(W3)
          END IF
          IF(SQ.EQ.0) THEN
              SQ=1
              WQ='REAL'
          END IF
          IF(WQ.NE.'REAL'.AND.WQ.NE.'PARAX'.AND.WQ.NE.'VREAL') THEN
              OUTLYNE=
     1        '"SETCLAP" ONLY TAKES "REAL", "VREAL" OR "PARAX" AS QUALIFIERS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
          IF(WQ.EQ.'PARAX') THEN
              FWARN=0
              RWARN=0
              DO I=0,INT(SYSTEM1(20))
                  IF(I.GE.INT(W1).AND.I.LE.INT(W2)) THEN
                      IF(.NOT.DUMMMY(I).OR.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          IF(ALENS(9,I).EQ.0.0D0.AND.ALENS(9,I).NE.5.0D0.AND.
     1                    ALENS(127,I).EQ.0.0D0) THEN
                              ALENS(9,I)=1.0D0
                              IF(I.EQ.0) THEN
                                  IF(SYSTEM1(16).NE.0.0D0) HX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                                  IF(SYSTEM1(16).EQ.0.0D0) HX=DABS(PXTRAX(1,I))
                                  IF(SYSTEM1(14).NE.0.0D0) HY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                                  IF(SYSTEM1(14).EQ.0.0D0) HY=DABS(PXTRAY(1,I))
                              ELSE
                                  HX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                                  HY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                              END IF
                              IF(HX.GT.HY) ALENS(10,I)=HX
                              IF(HX.LE.HY) ALENS(10,I)=HY
                              IF(ALENS(9,I).EQ.1.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
                                  ALENS(11,I)=ALENS(10,I)
                              ELSE
                                  ALENS(11,I)=0.0D0
                              END IF
                              ALENS(12,I)=0.0D0
                              ALENS(13,I)=0.0D0
                              ALENS(14,I)=0.0D0
                              ALENS(15,I)=0.0D0
                              WRITE(OUTLYNE,10) I
                              CALL SHOWIT(0)
 10                           FORMAT('          CLEAR APERTURE ASSIGNED TO SURFACE ',I3)
 15                           FORMAT('CLEAR APERTURE DIMENSIONS CHANGED AT SURFACE ',I3)
                          END IF
                          IF(ALENS(9,I).EQ.5.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
                              ALENS(9,I)=5.0D0
                              IF(I.EQ.0) THEN
                                  IF(SYSTEM1(16).NE.0.0D0) HX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                                  IF(SYSTEM1(16).EQ.0.0D0) HX=DABS(PXTRAX(1,I))
                                  IF(SYSTEM1(14).NE.0.0D0) HY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                                  IF(SYSTEM1(14).EQ.0.0D0) HY=DABS(PXTRAY(1,I))
                              ELSE
                                  HX=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                                  HY=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                              END IF
                              IF(HX.GT.HY) ALENS(10,I)=HX
                              IF(HX.LE.HY) ALENS(10,I)=HY
                              ALENS(12,I)=0.0D0
                              ALENS(13,I)=0.0D0
                              ALENS(14,I)=0.0D0
                              ALENS(15,I)=0.0D0
                              WRITE(OUTLYNE,10) I
                              CALL SHOWIT(0)
                          END IF
                      END IF
                  END IF
              END DO
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
          IF(WQ.EQ.'REAL'.OR.WQ.EQ.'VREAL') THEN
              RWARN=0
              FWARN=0
              OLDLDIF2=LDIF2
              OLDLDIF=LDIF
              LDIF2=.TRUE.
              LDIF=.TRUE.
C     EIGHT PLACES AROUND THE FULL FOV
              KK=0
              DO K=0,8
                  IF(ALENS(9,NEWOBJ).NE.2.0D0.AND.ALENS(9,NEWOBJ).NE.4.0D0
     1            .AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
                      IF(K.EQ.0) THEN
                          YF=0.0D0
                          XF=1.0D0
                      END IF
                      IF(K.EQ.1) THEN
                          YF=DSQRT(2.0D0)/2.0D0
                          XF=DSQRT(2.0D0)/2.0D0
                      END IF
                      IF(K.EQ.2) THEN
                          YF=1.0D0
                          XF=0.0D0
                      END IF
                      IF(K.EQ.3) THEN
                          YF=DSQRT(2.0D0)/2.0D0
                          XF=-DSQRT(2.0D0)/2.0D0
                      END IF
                      IF(K.EQ.4) THEN
                          YF=0.0D0
                          XF=-1.0D0
                      END IF
                      IF(K.EQ.5) THEN
                          YF=-DSQRT(2.0D0)/2.0D0
                          XF=-DSQRT(2.0D0)/2.0D0
                      END IF
                      IF(K.EQ.6) THEN
                          YF=-1.0D0
                          XF=0.0D0
                      END IF
                      IF(K.EQ.7) THEN
                          YF=-DSQRT(2.0D0)/2.0D0
                          XF=DSQRT(2.0D0)/2.0D0
                      END IF
                      IF(K.EQ.8) THEN
                          YF=0.0D0
                          XF=0.0D0
                      END IF
                  ELSE
                      IF(ALENS(9,NEWOBJ).EQ.2.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
                          IF(K.EQ.0) THEN
                              YF=0.0D0
                              XF=1.0D0
                          END IF
                          IF(K.EQ.1) THEN
                              YF=1.0D0
                              XF=1.0D0
                          END IF
                          IF(K.EQ.2) THEN
                              YF=1.0D0
                              XF=0.0D0
                          END IF
                          IF(K.EQ.3) THEN
                              YF=1.0D0
                              XF=-1.0D0
                          END IF
                          IF(K.EQ.4) THEN
                              YF=0.0D0
                              XF=-1.0D0
                          END IF
                          IF(K.EQ.5) THEN
                              YF=-1.0D0
                              XF=-1.0D0
                          END IF
                          IF(K.EQ.6) THEN
                              YF=-1.0D0
                              XF=0.0D0
                          END IF
                          IF(K.EQ.7) THEN
                              YF=-1.0D0
                              XF=1.0D0
                          END IF
                          IF(K.EQ.8) THEN
                              YF=0.0D0
                              XF=0.0D0
                          END IF
                      END IF
                      IF(ALENS(9,NEWOBJ).EQ.4.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
                          YFFIX=DABS(ALENS(14,NEWOBJ)/ALENS(10,NEWOBJ))
                          XFFIX=DABS(ALENS(14,NEWOBJ)/ALENS(11,NEWOBJ))
                          IF(K.EQ.0) THEN
                              YF=0.0D0
                              XF=1.0D0
                          END IF
                          IF(K.EQ.1) THEN
                              YF=1.0D0-(0.02928D0*YFFIX)
                              XF=1.0D0-(0.02928D0*XFFIX)
                          END IF
                          IF(K.EQ.2) THEN
                              YF=1.0D0
                              XF=0.0D0
                          END IF
                          IF(K.EQ.3) THEN
                              YF=1.0D0-(0.02928D0*YFFIX)
                              XF=-1.0D0+(0.02928D0*XFFIX)
                          END IF
                          IF(K.EQ.4) THEN
                              YF=0.0D0
                              XF=-1.0D0
                          END IF
                          IF(K.EQ.5) THEN
                              YF=-1.0D0+(0.02928D0*YFFIX)
                              XF=-1.0D0+(0.02928D0*XFFIX)
                          END IF
                          IF(K.EQ.6) THEN
                              YF=-1.0D0
                              XF=0.0D0
                          END IF
                          IF(K.EQ.7) THEN
                              YF=-1.0D0+(0.02928D0*YFFIX)
                              XF=1.0D0-(0.02928D0*XFFIX)
                          END IF
                          IF(K.EQ.8) THEN
                              YF=0.0D0
                              XF=0.0D0
                          END IF
                      END IF
                  END IF
C     EIGHT PLACES AROUND THE APERTURE
C     DO THE FOB
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC='FOB     '
                  WQ='        '
                  SQ=0
                  SST=0
                  STI=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=0
                  DF5=1
                  S1=1
                  S2=1
                  S3=1
                  S4=1
                  S5=0
                  SN=1
                  W1=YF
                  W2=XF
                  W3=0.0D0
                  W4=SYSTEM1(11)
C     SET MSG TO FALSE
                  MSG=.FALSE.
                  CALL FFOB
                  IF(.NOT.REFEXT) FWARN=1
                  REST_KDP(1)=RESTINPT(1)
C
                  IF(WQ.NE.'VREAL') THEN
                      XLO=-1.0D0
                      XHI=1.0D0
                      YLO=-1.0D0
                      YHI=1.0D0
                  END IF
                  IF(WQ.EQ.'VREAL') THEN
                      CALL VIGCAL(N,XLO,XHI,1)
                      CALL VIGCAL(N,YLO,YHI,2)
                      MSG=.FALSE.
                  END IF
                  DO L=0,7
                      KK=KK+1
                      M=(3*KK)-2
                      IF(ALENS(9,NEWREF).NE.2.0D0.AND.ALENS(9,NEWREF).NE.4.0D0
     1                .AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
                          IF(L.EQ.0) THEN
                              YR=0.0D0
                              XR=XHI
                          END IF
                          IF(L.EQ.1) THEN
                              YR=YHI*DSQRT(2.0D0)/2.0D0
                              XR=XHI*DSQRT(2.0D0)/2.0D0
                          END IF
                          IF(L.EQ.2) THEN
                              YR=YHI
                              XR=0.0D0
                          END IF
                          IF(L.EQ.3) THEN
                              YR=YHI*DSQRT(2.0D0)/2.0D0
                              XR=XLO*DSQRT(2.0D0)/2.0D0
                          END IF
                          IF(L.EQ.4) THEN
                              YR=0.0D0
                              XR=XLO
                          END IF
                          IF(L.EQ.5) THEN
                              YR=YLO*DSQRT(2.0D0)/2.0D0
                              XR=XLO*DSQRT(2.0D0)/2.0D0
                          END IF
                          IF(L.EQ.6) THEN
                              YR=YLO
                              XR=0.0D0
                          END IF
                          IF(L.EQ.7) THEN
                              YR=YLO*DSQRT(2.0D0)/2.0D0
                              XR=XHI*DSQRT(2.0D0)/2.0D0
                          END IF
                      ELSE
                          IF(ALENS(9,NEWREF).EQ.2.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
                              IF(L.EQ.0) THEN
                                  YR=0.0D0
                                  XR=XHI
                              END IF
                              IF(L.EQ.1) THEN
                                  YR=YHI
                                  XR=XHI
                              END IF
                              IF(L.EQ.2) THEN
                                  YR=YHI
                                  XR=0.0D0
                              END IF
                              IF(L.EQ.3) THEN
                                  YR=YHI
                                  XR=XLO
                              END IF
                              IF(L.EQ.4) THEN
                                  YR=0.0D0
                                  XR=XLO
                              END IF
                              IF(L.EQ.5) THEN
                                  YR=YLO
                                  XR=XLO
                              END IF
                              IF(L.EQ.6) THEN
                                  YR=YLO
                                  XR=0.0D0
                              END IF
                              IF(L.EQ.7) THEN
                                  YR=YLO
                                  XR=XHI
                              END IF
                          END IF
                          IF(ALENS(9,NEWREF).EQ.4.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
                              YRFIX=DABS(ALENS(14,NEWREF)/ALENS(10,NEWREF))
                              XRFIX=DABS(ALENS(14,NEWREF)/ALENS(11,NEWREF))
                              IF(L.EQ.0) THEN
                                  YR=0.0D0
                                  XR=XHI
                              END IF
                              IF(L.EQ.1) THEN
                                  YR=YHI-(0.02928D0*YRFIX*YHI)
                                  XR=XHI-(0.02928D0*XRFIX*XHI)
                              END IF
                              IF(L.EQ.2) THEN
                                  YR=YHI
                                  XR=0.0D0
                              END IF
                              IF(L.EQ.3) THEN
                                  YR=YHI-(0.02928D0*YRFIX*YHI)
                                  XR=XLO-(0.02928D0*XRFIX*XLO)
                              END IF
                              IF(L.EQ.4) THEN
                                  YR=0.0D0
                                  XR=-XLO
                              END IF
                              IF(L.EQ.5) THEN
                                  YR=YLO-(0.02928D0*YRFIX*YLO)
                                  XR=XLO-(0.02928D0*XRFIX*XLO)
                              END IF
                              IF(L.EQ.6) THEN
                                  YR=-YLO
                                  XR=0.0D0
                              END IF
                              IF(L.EQ.7) THEN
                                  YR=YLO-(0.02928D0*YFFIX*YLO)
                                  XR=XHI-(0.02928D0*XFFIX*XHI)
                              END IF
                          END IF
                      END IF
C     TRACE THE RAY
                      SAVE_KDP(1)=SAVEINPT(1)
                      WQ='        '
                      SQ=0
                      SST=0
                      STI=0
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=1
                      DF5=1
                      S1=1
                      S2=1
                      S3=1
                      S4=0
                      S5=0
                      SN=1
                      W1=YR
                      W2=XR
                      W3=SYSTEM1(11)
                      WC='RAY     '
                      CALL RRAY
                      IF(.NOT.RAYEXT) RWARN=1
                      REST_KDP(1)=RESTINPT(1)
C     SAVE RAY DATA
                      DO I=0,INT(SYSTEM1(20))
                          IF(.NOT.DUMMMY(I).OR.DF1.EQ.0.AND.DF2.EQ.0) THEN
                              VERARRAY(M,I)=RAYRAY(1,I)
                              VERARRAY(M+1,I)=RAYRAY(2,I)
                              VERARRAY(M+2,I)=DSQRT((RAYRAY(1,I)**2)+(RAYRAY(2,I)**2))
                          END IF
                      END DO
                  END DO
              END DO
              LDIF2=OLDLDIF2
              LDIF=OLDLDIF
C
C     PROCESS DATA
              DO I=0,INT(SYSTEM1(20))
                  IF(I.GE.INT(W1).AND.I.LE.INT(W2)) THEN
                      IF(.NOT.DUMMMY(I).OR.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HXMAX=-1.0D10
                          HYMAX=-1.0D10
                          HXMIN=1.0D10
                          HYMIN=1.0D10
                          DO J=1,214,3
                              IF(VERARRAY(J,I).GT.HXMAX) HXMAX=VERARRAY(J,I)
                              IF(VERARRAY(J,I).LT.HXMIN) HXMIN=VERARRAY(J,I)
                          END DO
                          DO J=2,215,3
                              IF(VERARRAY(J,I).GT.HYMAX) HYMAX=VERARRAY(J,I)
                              IF(VERARRAY(J,I).LT.HYMIN) HYMIN=VERARRAY(J,I)
                          END DO
                      END IF
                  END IF
              END DO
C
              DO I=0,INT(SYSTEM1(20))
                  IF(I.GE.INT(W1).AND.I.LE.INT(W2)) THEN
                      IF(.NOT.DUMMMY(I).OR.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          RMAX=-1.0D10
                          HXMAX=-1.0D10
                          HYMAX=-1.0D10
                          HXMIN=1.0D10
                          HYMIN=1.0D10
                          DO J=1,214,3
                              IF(VERARRAY(J,I).GT.HXMAX) HXMAX=VERARRAY(J,I)
                              IF(VERARRAY(J,I).LT.HXMIN) HXMIN=VERARRAY(J,I)
                              XCENPOS=(HXMAX+HXMIN)/2.0D0
                          END DO
                          DO J=2,215,3
                              IF(VERARRAY(J,I).GT.HYMAX) HYMAX=VERARRAY(J,I)
                              IF(VERARRAY(J,I).LT.HYMIN) HYMIN=VERARRAY(J,I)
                              YCENPOS=(HYMAX+HYMIN)/2.0D0
                          END DO
                          DO J=3,216,3
                              IF(VERARRAY(J,I).GT.RMAX) RMAX=VERARRAY(J,I)
                          END DO
                          YRAD=DABS((HYMAX-HYMIN)/2.0D0)
                          XRAD=DABS((HXMAX-HXMIN)/2.0D0)
                          RRAD=YRAD
                          IF(XRAD.GT.RRAD) RRAD=XRAD
                          IF(ALENS(9,I).EQ.0.0D0.AND.FWARN.EQ.0.AND.RWARN.EQ.0
     1                    .AND.ALENS(127,I).EQ.0.0D0) THEN
                              ALENS(9,I)=1.0D0
                              ALENS(10,I)=RRAD
                              ALENS(11,I)=RRAD
                              ALENS(12,I)=0.0D0
                              ALENS(13,I)=0.0D0
                              IF(DABS(YCENPOS).GT.1.0D-6) ALENS(12,I)=YCENPOS
                              IF(DABS(XCENPOS).GT.1.0D-6) ALENS(13,I)=XCENPOS
                              ALENS(14,I)=0.0D0
                              ALENS(15,I)=0.0D0
                              WRITE(OUTLYNE,10) I
                              CALL SHOWIT(0)
                          END IF
                          IF(ALENS(9,I).EQ.1.0D0.AND.FWARN.EQ.0.AND.RWARN.EQ.0
     1                    .AND.ALENS(127,I).EQ.0.0D0) THEN
                              ALENS(10,I)=RRAD
                              ALENS(11,I)=RRAD
                              ALENS(12,I)=0.0D0
                              ALENS(13,I)=0.0D0
                              IF(DABS(YCENPOS).GT.1.0D-6) ALENS(12,I)=YCENPOS
                              IF(DABS(XCENPOS).GT.1.0D-6) ALENS(13,I)=XCENPOS
                              ALENS(14,I)=0.0D0
                              ALENS(15,I)=0.0D0
                              WRITE(OUTLYNE,15) I
                              CALL SHOWIT(0)
                          END IF
                          IF(ALENS(9,I).EQ.2.0D0.AND.FWARN.EQ.0.AND.RWARN.EQ.0
     1                    .AND.ALENS(127,I).EQ.0.0D0) THEN
                              ALENS(10,I)=YRAD
                              ALENS(11,I)=XRAD
                              ALENS(12,I)=0.0D0
                              ALENS(13,I)=0.0D0
                              IF(DABS(YCENPOS).GT.1.0D-6) ALENS(12,I)=YCENPOS
                              IF(DABS(XCENPOS).GT.1.0D-6) ALENS(13,I)=XCENPOS
                              ALENS(14,I)=0.0D0
                              WRITE(OUTLYNE,15) I
                              CALL SHOWIT(0)
                          END IF
                          IF(ALENS(9,I).EQ.3.0D0.AND.FWARN.EQ.0.AND.RWARN.EQ.0
     1                    .AND.ALENS(127,I).EQ.0.0D0) THEN
                              ALENS(10,I)=YRAD
                              ALENS(11,I)=XRAD
                              ALENS(12,I)=0.0D0
                              ALENS(13,I)=0.0D0
                              IF(DABS(YCENPOS).GT.1.0D-6) ALENS(12,I)=YCENPOS
                              IF(DABS(XCENPOS).GT.1.0D-6) ALENS(13,I)=XCENPOS
                              ALENS(14,I)=0.0D0
                              WRITE(OUTLYNE,15) I
                              CALL SHOWIT(0)
                          END IF
                          IF(ALENS(9,I).EQ.4.0D0.AND.FWARN.EQ.0.AND.RWARN.EQ.0
     1                    .AND.ALENS(127,I).EQ.0.0D0) THEN
                              ALENS(10,I)=YRAD
                              ALENS(11,I)=XRAD
                              ALENS(12,I)=0.0D0
                              ALENS(13,I)=0.0D0
                              IF(DABS(YCENPOS).GT.1.0D-6) ALENS(12,I)=YCENPOS
                              IF(DABS(XCENPOS).GT.1.0D-6) ALENS(13,I)=XCENPOS
                              ALENS(14,I)=0.0D0
                              WRITE(OUTLYNE,15) I
                              CALL SHOWIT(0)
                          END IF
                          IF(ALENS(9,I).EQ.5.0D0.AND.FWARN.EQ.0.AND.RWARN.EQ.0
     1                    .AND.ALENS(127,I).EQ.0.0D0) THEN
                              ALENS(10,I)=XRAD
                              IF(YRAD.GT.XRAD) ALENS(10,I)=YRAD
                              ALENS(12,I)=0.0D0
                              ALENS(13,I)=0.0D0
                              IF(DABS(YCENPOS).GT.1.0D-6) ALENS(12,I)=YCENPOS
                              IF(DABS(XCENPOS).GT.1.0D-6) ALENS(13,I)=XCENPOS
                              ALENS(14,I)=0.0D0
                              WRITE(OUTLYNE,15) I
                              CALL SHOWIT(0)
                          END IF
                      END IF
                  END IF
              END DO
              IF(FWARN.NE.0) WRITE(OUTLYNE,2004)
              IF(RWARN.NE.0) WRITE(OUTLYNE,2005)
              CALL SHOWIT(0)
              IF(FWARN.NE.0.OR.RWARN.NE.0) THEN
                  WRITE(OUTLYNE,2006)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2007)
                  CALL SHOWIT(0)
              END IF
 2004         FORMAT('WARNING: SOME CHIEF RAYS COULD NOT BE TRACED')
 2005         FORMAT('WARNING: SOME MARGINAL RAYS COULD NOT BE TRACED')
 2006         FORMAT('LIMIT RAY DATA MAY BE IN ERROR, CLAPS NOT ASSIGNED')
 2007         FORMAT('CHANGE OBJECT HT. OR REF. AP. HT. AND RE-RUN')
              DEALLOCATE(VERARRAY,STAT=ALLOERR)
              RETURN
          END IF
      END
      SUBROUTINE RAYDOC
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          REAL*8 LENG
          COMMON/PASSLENG/LENG
          IF(RAYCOD(1).EQ.1) THEN
              WRITE(OUTLYNE,*) 'RAY DID NOT INTERSECT THE SURFACE'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.2) THEN
              WRITE(OUTLYNE,*)
     1        'CONVERGENCE FAILURE OCCURRED DURING RAY INTERSECTION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'WITH ASPHERIC, TORIC OR SPECIAL SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'ACCURACY OF INTERSECTION WAS = ',LENG
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.3) THEN
              WRITE(OUTLYNE,*)
     1        'RAY FAILED TO CONVERGE TO REFERENCE SURFACE RAY-AIM POINT'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.4) THEN
              WRITE(OUTLYNE,*) 'TOTAL INTERNAL REFLECTION'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.5) THEN
              WRITE(OUTLYNE,*)
     1        'ANGLE OF DIFFRACTION AT THE GRATING AT SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'IS PHYSICALLY UNREALIZABLE'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.6) THEN
              WRITE(OUTLYNE,*) 'RAY BLOCKED BY CIRCULAR CLEAR APERTURE'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.7) THEN
              WRITE(OUTLYNE,*) 'RAY BLOCKED BY CIRCULAR OBSCURATION'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.8) THEN
              WRITE(OUTLYNE,*)
     1        'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) '1',RAYCOD(1),RAYCOD(2)
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.9) THEN
              WRITE(OUTLYNE,*)
     1        'ANGLE OF DIFFRACTION AT THE HOE SURFACE IS PHYSICALLY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'UNREALIZABLE'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.11) THEN
              WRITE(OUTLYNE,*)
     1        'REFERENCE RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
          END IF
C
          IF(RAYCOD(1).EQ.12) THEN
              WRITE(OUTLYNE,*) 'RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.13) THEN
              WRITE(OUTLYNE,*) 'RAY MISSED GRAZING INCIDENCE SURFACE SECTION'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.14) THEN
              WRITE(OUTLYNE,*) 'ILLUMINATION RAY BLOCKED BY CLEAR APERTURE'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.15) THEN
              WRITE(OUTLYNE,*) 'NO GRID FILE EXISTS FOR THIS SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.16) THEN
              WRITE(OUTLYNE,*) 'SPECIFIED OBJECT POINT DOES NOT EXIST'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.17) THEN
              WRITE(OUTLYNE,*) 'NO DEFORM FILE EXISTS FOR THIS SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.18) THEN
              WRITE(OUTLYNE,*)
     1        'RAY COORDINATE BEYOND DEFORMABLE SURFACE BOUNDS'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.19) THEN
              WRITE(OUTLYNE,*)
     1        'AIMED CHIEF RAY COULD NOT HIT DEFINED TARGET POINT'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.20) THEN
              WRITE(OUTLYNE,*) 'TIR CONDITION NOT MET'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.21) THEN
              WRITE(OUTLYNE,*) 'HOE-R SOURCE CONFIGURATION RAY FAILURE'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(RAYCOD(1).EQ.22) THEN
              WRITE(OUTLYNE,*) 'HOE-R REFERENCE CONFIGURATION RAY FAILURE'
              CALL SHOWIT(1)
              RETURN
          END IF
      END
      SUBROUTINE IPLANE_TILT
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
!      REAL*8 ANGLE_A,ANGLE_B
          REAL*8 JK_TEMP
          COS_A_ANG=1.0D0
          COS_B_ANG=1.0D0
          IF(CARTMAN) THEN
              IPLANE_X=REFRY(1,NEWIMG)
              IPLANE_Y=REFRY(2,NEWIMG)
              IPLANE_Z=REFRY(3,NEWIMG)
              IPLANE_L=REFRY(4,NEWIMG)
              IPLANE_M=REFRY(5,NEWIMG)
              IPLANE_N=REFRY(6,NEWIMG)
              IPLANE_LN=REFRY(13,NEWIMG)
              IPLANE_MN=REFRY(14,NEWIMG)
              IPLANE_NN=REFRY(15,NEWIMG)
              IF(DABS(IPLANE_NN).EQ.0.0D0) THEN
                  JK_TEMP=0.0D0
              ELSE
                  JK_TEMP=DATAN2(IPLANE_MN,IPLANE_NN)
              END IF
              COS_A_ANG=DABS(DCOS(JK_TEMP))
              IF(DABS(IPLANE_NN).EQ.0.0D0) THEN
                  JK_TEMP=0.0D0
              ELSE
                  JK_TEMP=DATAN2(IPLANE_LN,IPLANE_NN)
              END IF
              COS_B_ANG=DABS(DCOS(JK_TEMP))
          END IF
          RETURN
      END


      SUBROUTINE SAVE_CHIEF_RAY_DATA
          IMPLICIT NONE
!      INTEGER I,J
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          OLREFRY=REFRY
          OLRFDIFF=RFDIFF
          RETURN
      END


      SUBROUTINE REST_CHIEF_RAY_DATA
          IMPLICIT NONE
!      INTEGER I,J
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          REFRY=OLREFRY
          RFDIFF=OLRFDIFF
          RETURN
      END
C SUB TRACE_HOERAY.FOR

      SUBROUTINE TRACE_HOERAY(XO,YO,ZO)

          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE HOERAY.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE TRACING OF A CONSTRUCTION OR PLAYBACK RAY IN TYPE 13
C       SPECIAL SURFACE HOE-R RAYTRACING. THE DIRECTION COSINES OF THE
C       RESULTANT RAY ARE PASSED BACK VIA THE ARGUMENTS.
C       THE RAY IS TRACED AT WAVELENGTH NUMBER HOE_WAV_NUM FROM POSITION
C       XO,YO,ZO ON THE OBJECT SURFACE TO XHOE,YHOE,ZHOE ON THE FINAL SURFACE.
          REAL*8 XO,YO,ZO
C
          INTEGER I,ISYS20,KKK

          INTEGER CAERAS,COERAS
C
          REAL*8 X,Y,Z,L,M,N,IA,IAP
     1    ,D21,D22,XL,XM,XN,YL,YM,YN,RN1,RN2,
     2    D11,D12,LS,SNINDX,SNIND2,
     3    LSTART,MSTART,NSTART,YANG,XANG,X1LAST,LARGE,
     4    Y1LAST,X1ONE,Y1ONE,RXLAST,RYLAST,RXONE,RYONE,DDELX,DDELY,
     5    XC1,YC1,ZC1,MF1,MF2,TARX,TARY,
     6    TEST,MAG,LOLD,MOLD,NOLD
C
!      REAL*8 HOE_L,HOE_M,HOE_N
C
          COMMON/CACO/CAERAS,COERAS,LS
C
          LOGICAL AIMOK,OLDPASS,DELFAIL
          COMMON/PASSOLD/OLDPASS
C
          REAL*8 XPASS,YPASS,ZPASS
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'

C
C     SET RAY AIMING TARGET VALUES
          TARY=YHOE
          TARX=XHOE
          KKK=0
C                       DDELX=0.001D0*SYSTEM1(12)
C                       DDELY=0.001D0*SYSTEM1(13)
          DDELX=0.001D0
          DDELY=0.001D0
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
C       RAY AIMING.
C
          WW1=YO
          WW2=XO
          WW3=DBLE(HOE_WAV_NUM)
          WW4=1.0D0
C
C
C       SET RAYCOD DEFAULTS
          RAYCOD(1)=-1
          RAYCOD(2)=-1
C
          AIMOK=.FALSE.
C
          RELY=YO
          RELX=X0
C
C       SET INITIAL VALUES FOR X1ONE, Y1ONE, X1LAST AND Y1LAST
C
          LARGE=-99999.9D0
          X1ONE=LARGE
          Y1ONE=LARGE
          X1LAST=LARGE
          Y1LAST=LARGE
C
C       SET INITIAL VALUES FOR RXONE, RYONE, RXLAST AND RYLAST
          RXONE=LARGE
          RYONE=LARGE
          RXLAST=LARGE
          RYLAST=LARGE
C
C       THE FIRST THING TO DO IS TO DETERMINE THE STARTING
C       COORDINATES OF THE RAY AT THE OBJECT SURFACE.
C       THE STARTING RAY COORDINATES AT SURFACE 0
C       ARE JUST THE RAY COORDINATES AT THIS SURFACE
C
C
          XSTRT=XO
          YSTRT=YO
          ZSTRT=ZO
C
C       THE WAVELENGTH NUMBER FOR THE RAY TRACE IS HOE_WAV_NUM
C
C
 989      CONTINUE
          IF(RAYCOD(1).EQ.1.AND.KKK.GT.1) THEN
              STOPP=1
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              FAIL=.TRUE.
              RETURN
          END IF
          X1AIM=0.0D0
          Y1AIM=0.0D0
          Z1AIM=0.0D0
          XC=X1AIM
          YC=Y1AIM
          ZC=Z1AIM
          XC1=XC
          YC1=YC
          ZC1=ZC
          IF(XC.LT.0.0D0) DDELX=-DDELX
          IF(YC.LT.0.0D0) DDELY=-DDELY
          XAIMOL=XC1
          YAIMOL=YC1
          ZAIMOL=ZC1
C     CALL BAKONE TO CONVERT TO 0 COORDINATES
          R_TX=X1AIM
          R_TY=Y1AIM
          R_TZ=Z1AIM
          CALL BAKONE
          X1AIM=R_TX
          Y1AIM=R_TY
          Z1AIM=R_TZ
C
C       NOW THE STARTING DIRECTION COSINES ARE RELATIVELY SIMPLE.
C       THEY ARE CALLED LSTART,MSTART AND NSTART
C
C       FIRST CONVERT THE SURFACE 0 AND 1 COORDINATES INTO YANG AND XANG
C       ANGULAR VALUES AS IN HEXAGON AND CODE V.
C       YANG AND XANG ANGLES IN RADIANS.
          STOPP=0
          RAYEXT=.TRUE.
          FAIL=.FALSE.
C
 9        CONTINUE
          IF(ALENS(3,0).LT.0.0D0) REVSTR=.TRUE.
          IF(ALENS(3,0).GE.0.0D0) REVSTR=.FALSE.
          RV=.FALSE.
          KKK=KKK+1
C       IF KKK EXCEEDS 100 TRIES, PRINT RAY ITERRATION ERROR
C       AND STOP SEARCHING.
          IF(KKK.GT.NRAITR) THEN
              IF(MSG) THEN
                  WRITE(OUTLYNE,*)
     1              'RAY FAILURE OCCURRED AT SURFACE ',INT(SYSTEM1(20))
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'RAY FAILED TO CONVERGE TO REFERENCE SURFACE RAY-AIM POINT'
                  CALL SHOWIT(1)
              END IF
              RAYCOD(1)=3
              RAYCOD(2)=INT(SYSTEM1(20))
              STOPP=1
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              FAIL=.TRUE.
              RETURN
          ELSE
              STOPP=0
              RAYEXT=.TRUE.
              FAIL=.FALSE.
C        PROCEED
          END IF
C
C       THIS IS THE RE-ENTRY POINT FOR RAY AIMING. RE-ENTRY
C       IS PERFORMED AFTER NEW VALUES OF X1AIM AND Y1AIM WERE
C       CALCULATED JUST BEFORE ENCOUNTERING THE (GO TO 9)
C       STATEMENT NEAR THE END OF THIS ROUTINE.
C     COMPUTE DIR COS DIRECTLY FROM POSITIONS
C
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
          H_RAY(32,0)=WW3
          H_RAY(1,0)=XSTRT
          H_RAY(2,0)=YSTRT
          H_RAY(3,0)=ZSTRT
          H_RAY(4,0)=LSTART
          H_RAY(5,0)=MSTART
          H_RAY(6,0)=NSTART
          H_RAY(7,0)=0.0D0
          H_RAY(8,0)=0.0D0
          H_RAY(25,0)=WW4
          IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) THEN
              SNIND2=DABS(ALENS(45+INT(WW3),0))/ALENS(45+INT(WW3),0)
              RN1=(ALENS(45+INT(WW3),0))
              RN2=(ALENS(45+INT(WW3),0))
          END IF
          IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) THEN
              SNIND2=DABS(ALENS(65+INT(WW3),0))/ALENS(65+INT(WW3),0)
              RN1=(ALENS(65+INT(WW3),0))
              RN2=(ALENS(65+INT(WW3),0))
          END IF
          IF(SNIND2.GT.0.0D0) H_RAY(24,0)=1.0D0
          IF(SNIND2.LT.0.0D0) H_RAY(24,0)=-1.0D0
          IF(SNIND2.GT.0.0D0) POSRAY=.TRUE.
          IF(SNIND2.LT.0.0D0) POSRAY=.FALSE.
          H_RAY(9,0)=DCOS(DSQRT((XANG**2)+(YANG**2)))
          H_RAY(10,0)=DCOS(DSQRT((XANG**2)+(YANG**2)))
          IA=DACOS(H_RAY(9,0))
          IAP=DACOS(H_RAY(10,0))
          H_RAY(11,0)=XANG
          IF((H_RAY(11,0)).LT.0.0D0) H_RAY(11,0)=
     1    H_RAY(11,0)+(TWOPII)
          H_RAY(12,0)=YANG
          IF((H_RAY(12,0)).LT.0.0D0) H_RAY(12,0)=
     1    H_RAY(12,0)+(TWOPII)
          H_RAY(13,0)=0.0D0
          H_RAY(14,0)=0.0D0
          H_RAY(15,0)=1.0D0
          H_RAY(16,0)=XSTRT
          H_RAY(17,0)=YSTRT
          H_RAY(18,0)=ZSTRT
          H_RAY(19,0)=LSTART
          H_RAY(20,0)=MSTART
          H_RAY(21,0)=NSTART
          H_RAY(22,0)=0.0D0
          H_RAY(26,0)=(1.0D0*DCOS(XANG))
          H_RAY(27,0)=0.0D0
          H_RAY(28,0)=-(1.0D0*DSIN(XANG))
          H_RAY(29,0)=0.0D0
          H_RAY(30,0)=(1.0D0*DCOS(XANG))
          H_RAY(31,0)=-(1.0D0*DSIN(YANG))
C
C
          X=XSTRT
          Y=YSTRT
          Z=ZSTRT
          L=LSTART
          M=MSTART
          N=NSTART
          XL=(1.0D0*DCOS(XANG))
          XM=0.0D0
          XN=-(1.0D0*DSIN(XANG))
          YL=0.0D0
          YM=(1.0D0*DCOS(XANG))
          YN=-(1.0D0*DSIN(YANG))
C
          ISYS20=INT(SYSTEM1(20))
          I=0
          DO 10 I=(1),ISYS20
              R_X=X
              R_Y=Y
              R_Z=Z
              R_L=L
              R_M=M
              R_N=N
              R_I=I
              CALL TRNSF2
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              XOLD=X
              YOLD=Y
              ZOLD=Z
              LOLD=L
              MOLD=M
              NOLD=N
C       XOLD,YOLD AND ZOLD ARE THE COORDINATES OF THE CURRENT RAY
C       AT SURFACE I-1 IN THE COORDINATE SYSTEM OF SURFACE I
C       NOW INTERSECT THE SURFACE
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
              IF(H_RAY(24,R_I-1).GT.0.0D0) POSRAY=.TRUE.
              IF(H_RAY(24,R_I-1).LT.0.0D0) POSRAY=.FALSE.
              CALL HITSUR
              IF(STOPP.EQ.1) THEN
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  RETURN
              END IF
              X=R_X
              Y=R_Y
              Z=R_Z
              L=R_L
              M=R_M
              N=R_N
              IF(RV) H_RAY(23,I)=-1.0D0
              IF(.NOT.RV) H_RAY(23,I)=1.0D0
C       LOAD REF RAY REGISTERS
C       FOR SURFACE 0
C       H_RAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
C       H_RAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
C       H_RAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
C       H_RAY(4,SURF)=L(LOCAL)- DIRECTION COSINE AT SURFACE I
C       H_RAY(5,SURF)=M(LOCAL)- DIRECTION COSINE AT SURFACE I
C       H_RAY(6,SURF)=N(LOCAL)- DIRECTION COSINE AT SURFACE I
C       H_RAY(8,SURF)=LEN - PHYSICAL LENGTH ALONG RAY FROM SURF I-1 TO I
C       H_RAY(7,SURF)=OPL - OPL FROM SURFACE I-1 TO I
C     ZERO IF I-1 IS PERFECT
C       H_RAY(9,SURF)=COSINE(I)
C       H_RAY(10,SURF)=COSINE(IP)
C       H_RAY(11,SURF)=UX - (RADIAN) MARGINAL XZ SLOPE ANGLE
C       H_RAY(12,SURF)=UY - (RADIAN) MARGINAL YZ SLOPE ANGLE
C       H_RAY(13,SURF)=L DIRECTION COSINE OF SURFACE NORMAL
C       H_RAY(14,SURF)=M DIRECTION COSINE OF SURFACE NORMAL
C       H_RAY(15,SURF)=N DIRECTION COSINE OF SURFACE NORMAL
C       H_RAY(16,SURF)=XOLD
C       H_RAY(17,SURF)=YOLD
C       H_RAY(18,SURF)=ZOLD
C       H_RAY(19,SURF)=LOLD
C       H_RAY(20,SURF)=MOLD
C       H_RAY(21,SURF)=NOLD
C       H_RAY(22,SURF)=OPL - OPL FROM SURFACE 0 TO I
C     OR TO I-1 IF I-1 IS "PERFECT'
C       H_RAY(23,SURF)=1 FOR NOT RV, -1 FOR RV
C       H_RAY(24,SURF)=1.0D0 FOR POS RAY, -1 FOR NEG RAY
C       H_RAY(25,SURF)=RAY ENERGY TERM
C       H_RAY(26,SURF)=RAY XL DIR COS
C       H_RAY(27,SURF)=RAY XM DIR COS
C       H_RAY(28,SURF)=RAY XN DIR COS
C       H_RAY(29,SURF)=RAY YL DIR COS
C       H_RAY(30,SURF)=RAY YM DIR COS
C       H_RAY(31,SURF)=RAY YN DIR COS

C       H_RAY(33,SURF) TO H_RAY(35,SURF) RESERVED
              H_RAY(32,I)=WW3
              H_RAY(1,I)=X
              H_RAY(2,I)=Y
              H_RAY(3,I)=Z
              H_RAY(4,I)=L
              H_RAY(5,I)=M
              H_RAY(6,I)=N
              H_RAY(26,I)=(M*YN)-(N*YM)
              H_RAY(27,I)=-((L*YN)-(N*YL))
              H_RAY(28,I)=(L*YM)-(M*YL)
              H_RAY(9,I)=COSI
              H_RAY(10,I)=COSIP
C
C     WHAT IS THE SIGN OF THE INDEX IN THE I-1 SPACE
              IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) THEN
                  SNINDX=DABS(ALENS(45+INT(WW3),I-1))/ALENS(45+INT(WW3),I-1)
                  SNIND2=DABS(ALENS(45+INT(WW3),I))/ALENS(45+INT(WW3),I)
                  RN1=(ALENS(45+INT(WW3),I-1))
                  RN2=(ALENS(45+INT(WW3),I))
              END IF
              IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) THEN
                  SNINDX=DABS(ALENS(65+INT(WW3),I-1))/ALENS(65+INT(WW3),I-1)
                  SNIND2=DABS(ALENS(65+INT(WW3),I))/ALENS(65+INT(WW3),I)
                  RN1=(ALENS(65+INT(WW3),I-1))
                  RN2=(ALENS(65+INT(WW3),I))
              END IF
              H_RAY(29,I)=YL
              H_RAY(30,I)=YM
              H_RAY(31,I)=YN
              H_RAY(25,I)=H_RAY(25,I-1)
C
C     PHYSICAL LENGTH OF THE RAY FROM I-1 TO I IS THE MAG OF THE
C     DIST TRAVELED TIME THE SNINDX TIMES -1 IF RV IN THE SPACE
C     FROM I-1 TO I
C
C     THE NEXT LINES FIX THE RAY DIRECTION WHEN THERE IS
C     NO Z COMPONENT OF MOTION.
C     THE MAGNITUDE OF THE DISTANCE THE RAY TRAVELED FROM I-1 TO I
C     IS ALWAYS JUST:
C
              H_RAY(8,I)=DSQRT(
     1        ((H_RAY(3,I)-ZOLD)**2)+((H_RAY(2,I)-YOLD)**2)
     2        +((H_RAY(1,I)-XOLD)**2))
C
              IF(RV) H_RAY(8,I)=-H_RAY(8,I)
              IF(.NOT.RV) H_RAY(8,I)=H_RAY(8,I)
              IF(DABS(H_RAY(8,I)).GE.1.0D10) H_RAY(8,I)=0.0D0
C
              IF(SNIND2.GT.0.0D0) H_RAY(24,I)=1.0D0
              IF(SNIND2.LT.0.0D0) H_RAY(24,I)=-1.0D0
C
              IF(GLANAM(I-1,2).EQ.'PERFECT      ') THEN
                  H_RAY(7,I)=0.0D0
                  H_RAY(8,I)=0.0D0
              END IF
              IF(GLANAM(I-1,2).EQ.'IDEAL        ') THEN
                  H_RAY(8,I)=-(ALENS(121,I-1)-ALENS(3,I-1))*H_RAY(6,I-1)
              END IF

              IF(INT(WW3).GE.1.AND.INT(WW3).LE.5)
     1        H_RAY(7,I)=H_RAY(8,I)*DABS(ALENS(45+INT(WW3),(I-1)))
              IF(INT(WW3).GE.6.AND.INT(WW3).LE.10)
     1        H_RAY(7,I)=H_RAY(8,I)*DABS(ALENS(65+INT(WW3),(I-1)))
              IF(.NOT.RV) H_RAY(7,I)=H_RAY(7,I)+PHASE
              IF(RV) H_RAY(7,I)=H_RAY(7,I)-PHASE
C
              IF(L.EQ.0.0D0) THEN
                  IF(N.GE.0.0D0) H_RAY(11,I)=0.0D0
                  IF(N.LT.0.0D0) H_RAY(11,I)=PII
              ELSE
                  IF(DABS(L).GE.DABS(1.0D35*N)) THEN
                      IF(L.GE.0.0D0) H_RAY(11,I)=PII/2.0D0
                      IF(L.LT.0.0D0) H_RAY(11,I)=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(N).EQ.0.0D0) THEN
                          H_RAY(11,I)=0.0D0
                      ELSE
                          H_RAY(11,I)=DATAN2(L,N)
                      END IF
                      IF((H_RAY(11,I)).LT.0.0D0) H_RAY(11,I)=
     1                H_RAY(11,I)+(TWOPII)
                  END IF
              END IF
              IF(M.EQ.0.0D0) THEN
                  IF(N.GE.0.0D0) H_RAY(12,I)=0.0D0
                  IF(N.LT.0.0D0) H_RAY(12,I)=PII
              ELSE
                  IF(DABS(M).GE.DABS(1.0D35*N)) THEN
                      IF(M.GE.0.0D0) H_RAY(12,I)=PII/2.0D0
                      IF(M.LT.0.0D0) H_RAY(12,I)=(3.0D0*PII)/2.0D0
                  ELSE
                      IF(DABS(N).EQ.0.0D0) THEN
                          H_RAY(12,I)=0.0D0
                      ELSE
                          H_RAY(12,I)=DATAN2(M,N)
                      END IF
                      IF((H_RAY(12,I)).LT.0.0D0) H_RAY(12,I)=
     1                H_RAY(12,I)+(TWOPII)
                  END IF
              END IF
              H_RAY(13,I)=LN
              H_RAY(14,I)=MN
              H_RAY(15,I)=NN
              H_RAY(16,I)=XOLD
              H_RAY(17,I)=YOLD
              H_RAY(18,I)=ZOLD
              H_RAY(19,I)=LOLD
              H_RAY(20,I)=MOLD
              H_RAY(21,I)=NOLD
              H_RAY(22,I)=H_RAY(22,(I-1))+H_RAY(7,I)
              IF(STOPP.EQ.1) THEN
C NEW STUFF 6/2/94
                  IF(KKK.EQ.1.AND.RAYCOD(1).EQ.1) THEN
                      STOPP=0
                      KKK=KKK+1
                      GO TO 989
                  END IF
                  FAIL=.TRUE.
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  STOPP=1
                  RETURN
              ELSE
                  FAIL=.FALSE.
                  STOPP=0
                  RAYEXT=.TRUE.
              END IF
C
C       CHECK THE RAY HEIGHT AT
C       NEWR AND ADJUST THE Y1AIM AND X1AIM AT SURFACE
C       1 SO THAT THE RAY HEIGHT AT NEWR IS WITHIN
C       AIMTOL . MAXIMUN NUMBER OF ITERRATIONS IS
C       NRAITR. (DEFAULT IS 100). DEFAULT AIMTOL IS 1.0D-6.
C
              IF(I.EQ.INT(SYSTEM1(20))) THEN
C
                  TEST=DSQRT(((TARX-X)**2)+((TARY-Y)**2))
                  IF(TEST.LE.AIMTOL) THEN
C       AIM IS GOOD ENOUGH, PROCEED
                      AIMOK=.TRUE.
                      GO TO 100
                  ELSE
                      AIMOK=.FALSE.
C       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                  END IF
C
C       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
C       X1ONE AND Y1ONE ARE THE FIRST SET OF RAY
C       COORDINATES AT SURFACE 1
                  X1ONE=X1LAST
                  Y1ONE=Y1LAST
C       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
C       X1LAST AND Y1LAST ARE THE LAST RAY COORDINATES
C       AT SURFACE 1
                  X1LAST=XAIMOL
                  Y1LAST=YAIMOL
C       SET RXONE AND RYONE TO RXLAST AND RYLAST
C       RYONE ANE RXONE ARE THE FIRST SET OF RAY
C       COORDINATES AT THE REFERENCE SURFACE
                  RXONE=RXLAST
                  RYONE=RYLAST
C       SET RXLAST AND RYLAST TO X AND Y
C       RXLAST AND RYLAST ARE THE LAST X AND Y RAY COORDINATES
C       ON THE REFERENCE SURFACE.
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
                      X1AIM=XAIMOL+DDELX
                      Y1AIM=YAIMOL+DDELY
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
                      XAIMOL=XC1
                      YAIMOL=YC1
                      ZAIMOL=ZC1
C     CALL BAKONE TO CONVERT TO 0 COORDINATES
                      R_TX=X1AIM
                      R_TY=Y1AIM
                      R_TZ=Z1AIM
                      CALL BAKONE
                      X1AIM=R_TX
                      Y1AIM=R_TY
                      Z1AIM=R_TZ
                      GO TO 9
C       THIS IS NOT THE FIRST REFINEMENT, KKK NOT = 1
                  END IF
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
                  DELFAIL=.FALSE.
                  CALL NEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL)
                  IF(DELFAIL) THEN
                      RETURN
                  ELSE
                      GO TO 9
                  END IF
C       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
              END IF
C
 100          CONTINUE
C
 10       CONTINUE
          RETURN
      END
C SUB RRAY2.FOR
      SUBROUTINE RRAY2
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE RRAY.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CMD LEVEL COMMAND RAY FROM CALCPRE
C
          INTEGER J
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          REAL*8 ZZEMAX,ZZEMIN
          COMMON/KENMOOR/ZZEMIN,ZZEMAX
C
C       WHEN RAY IS CALLED FROM OTHER PROGRAM LEVELS,
C       SOMETIMES CLEAR APERTURE/OBSCURATION CHECKING
C       IS RESET.
C       DEFAULT QUALIFIER IS NOTHING (NO QUALIFIER)
          CACOCH=0
C
C       SET DEFAULT NUMERICS
          IF(DF1.EQ.1) W1=0.0D0
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=SYSTEM1(11)
          IF(DF3.EQ.1) WW3=W3
          IF(DF4.EQ.1) W4=1.0D0
          IF(DF5.EQ.1) W5=0.0D0
          IF(DF5.EQ.0) MSG=.FALSE.
          WW4=W4
          WVN=W3
C       CURLAM TRACK CURRENT WAVELENGTH TRACED VIA COMMON
C       KURLAM IN DATLEN.FOR
          CURLAM=W3
C       THE CONTROL WAVELENGTH
C       NUMERIC WORD 5 ARE NOT USED IN THE CMD LEVEL
C       "RAY" COMMAND
C       CHECK FOR NATURE OF QUALIFIER WORD
C       CHECK FOR CORRECT WAVELENGTH BOUNDS
C
C       CHECK IF REFEXT=.FALSE. IF SO STOP AND PRINT  MESSAGE
C
          IF(.NOT.REFEXT) THEN
C       NO CHIEF RAY EXISTS, STOP
          END IF
C       SET RAYCODS
          RAYCOD(1)=0
          RAYCOD(2)=-1
C
C       SET STOPP=0
          STOPP=0
C       SET RAYEXT TO TRUE
          RAYEXT=.TRUE.
C       SET FAIL TO FALSE
          FAIL=.FALSE.
C
C       NOW THE DEFAULTS HAVE BEEN SET
C       NOW CALL THE SUBROUTINE WHICH STARTS THE
C       RAY TRACE. THIS IS RAYTRA2
C
          WWQ=WQ
          WW1=W1
          WW2=W2
          WW3=W3
          WW4=W4
          WVN=W3
C       THE CALL TO RAYTRA2 HAS INPUTS:
C               QUALIFIER
C               WW1
C               WW2
C               WW3
C               WW4
C               CACOCH
C               FAIL
C       TRACE RAY AND RETURN
          PLTRAY=.FALSE.
          CALL RAYTRA2
          PLTRAY=.FALSE.
          IF(STOPP.EQ.1) THEN
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              IF(F34.NE.1) CALL MACFAL
              IF(DF5.EQ.0) MSG=.TRUE.
              RETURN
          ELSE
              STOPP=0
              RAYEXT=.TRUE.
          END IF
C
C       THE RETURN WILL SEND BACK VIA THE COMMON/RAYCMN COMMON
C       THE FULL RAY DATA FOR THE FULLY AIMMED AND TRACED
C       RAY.
          DO J=0,INT(SYSTEM1(20))
              IF(ALENS(34,J).EQ.18.0D0) LDIF2=.FALSE.
              IF(ALENS(34,J).EQ.18.0D0) LDIF=.FALSE.
          END DO
          IF(LDIF) THEN
C       RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
C       DATA
C       SAVE RAYRAY DATA
              SAVE=RAYRAY
C
              SRAYDIFEXT=.FALSE.
              DIFRAYTRACE=.TRUE.
              CALL DIFRAY
              DIFRAYTRACE=.FALSE.
              IF(STOPP.EQ.1) THEN
                  STOPP=0
                  RAYRAY=SAVE
                  CALL MACFAL
                  IF(DF5.EQ.0) MSG=.TRUE.
                  RETURN
              ELSE
                  STOPP=0
                  RAYEXT=.TRUE.
              END IF
C
C       ALL DIFFERENTIAL RAY DATA WILL BE STORED IN ARRAYS
C       AND SET TO COMMON FROM WITHIN THE DIFRAY SUBROUTINE
C       RESTORE RAYRAY DATA
              RAYRAY=SAVE
C       NO DIF TRACE
          END IF
C
          IF(DF5.EQ.0) MSG=.TRUE.
          RETURN
      END
C SUB SIZES2.FOR
      SUBROUTINE SIZES2
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SIZES2
C
          INTEGER I,N
C
          REAL*8 X,Y,DX,DY
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(STI.EQ.1) THEN
              OUTLYNE='"BLKRAYS" CALCULATES YZ AND XZ-PLANE RAY BLOCKAGE'
              CALL SHOWIT(1)
              OUTLYNE='FOR THE CURRENT "FOB"'
              CALL SHOWIT(1)
              RETURN
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"BLKRAYS" TAKES NO STRING OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.REFEXT) THEN
              OUTLYNE='A CHIEF RAY DOES NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE='"BLKRAYS" DID NOT EXECUTE'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK FOR NUMERIC INPUT
          IF(W1.LT.10.0D0) W1=10.0D0
          IF(W1.GT.100.0D0) W1=100.0D0
          IF(DF1.EQ.1) W1=25.0D0
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"BLKRAYS" TAKE NO NUMERIC WORD #2 TO #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          N=INT(W1)
          IF(N.EQ.0) N=25
          X=-1.0D0
          Y=0.0D0
          DX=2.0D0/DBLE(N-1)
          WRITE(OUTLYNE,9)
          CALL SHOWIT(1)
 9        FORMAT('X-FAN RAY BLOCKAGES')
          WRITE(OUTLYNE,10)
          CALL SHOWIT(0)
 10       FORMAT(2X,'X-REF HT.',8X,'YREF HT.',5X,'FAIL',1X,'SURF#')
          DO I=1,N
              WRITE(INPUT,*) 'RAY CAOB,',X,Y,',,,1'
              CALL PROCES
              IF(RAYCOD(1).EQ.0) THEN
                  WRITE(OUTLYNE,20) X,Y
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,21) X,Y,RAYCOD(2)
                  CALL SHOWIT(0)
              END IF
              X=X+DX
          END DO
          X=0.0D0
          Y=-1.0D0
          DY=2.0D0/DBLE(N-1)
          WRITE(OUTLYNE,*) '  '
          CALL SHOWIT(0)
          WRITE(OUTLYNE,11)
          CALL SHOWIT(0)
 11       FORMAT('Y-FAN RAY BLOCKAGES')
          WRITE(OUTLYNE,10)
          CALL SHOWIT(0)
          DO I=1,N
              WRITE(INPUT,*) 'RAY CAOB,',X,Y,',,,1'
              CALL PROCES
              IF(RAYCOD(1).EQ.0) THEN
                  WRITE(OUTLYNE,20) X,Y
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,21) X,Y,RAYCOD(2)
                  CALL SHOWIT(0)
              END IF
              Y=Y+DY
          END DO
          X=-DSQRT(2.0D0)/2.0D0
          Y=-DSQRT(2.0D0)/2.0D0
          DX=DSQRT(2.0D0)/DBLE(N-1)
          DY=DSQRT(2.0D0)/DBLE(N-1)
          WRITE(OUTLYNE,*) '  '
          CALL SHOWIT(0)
          WRITE(OUTLYNE,12)
          CALL SHOWIT(0)
 12       FORMAT('P-FAN RAY BLOCKAGES')
          WRITE(OUTLYNE,10)
          CALL SHOWIT(0)
          DO I=1,N
              WRITE(INPUT,*) 'RAY CAOB,',X,Y,',,,1'
              CALL PROCES
              IF(RAYCOD(1).EQ.0) THEN
                  WRITE(OUTLYNE,20) X,Y
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,21) X,Y,RAYCOD(2)
                  CALL SHOWIT(0)
              END IF
              Y=Y+DY
              X=X+DX
          END DO
          X=DSQRT(2.0D0)/2.0D0
          Y=-DSQRT(2.0D0)/2.0D0
          DX=DSQRT(2.0D0)/DBLE(N-1)
          DY=DSQRT(2.0D0)/DBLE(N-1)
          WRITE(OUTLYNE,*) '  '
          CALL SHOWIT(0)
          WRITE(OUTLYNE,13)
          CALL SHOWIT(0)
 13       FORMAT('N-FAN RAY BLOCKAGES')
          WRITE(OUTLYNE,10)
          CALL SHOWIT(0)
          DO I=1,N
              WRITE(INPUT,*) 'RAY CAOB,',X,Y,',,,1'
              CALL PROCES
              IF(RAYCOD(1).EQ.0) THEN
                  WRITE(OUTLYNE,20) X,Y
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,21) X,Y,RAYCOD(2)
                  CALL SHOWIT(0)
              END IF
              Y=Y+DY
              X=X-DX
          END DO
 20       FORMAT(G15.7,1X,G15.7,1X,' NO  ')
 21       FORMAT(G15.7,1X,G15.7,1X,' YES ',I3)
          RETURN
      END
      SUBROUTINE HIST_RAY_SAVE
          USE GLOBALS
          INTEGER J,K
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER ALLOERR
          REAL*8 RHIST_TEMP
          DIMENSION RHIST_TEMP(:,:,:)
          ALLOCATABLE :: RHIST_TEMP
C       SAVES RAY DATA FOR RAY HISTORIES
          IF(RAY_HIST_NUM.GE.0) RAY_HIST_NUM=RAY_HIST_NUM+1
          IF(RAY_HIST_NUM.GE.RHIST_MAXRAYS) THEN
C       MAKE STORAGE ARRAY TWICE AS BIG
              ALLOCATE(RHIST_TEMP(1:93,1:RAY_HIST_NUM,0:INT(SYSTEM1(20)))
     1        ,STAT=ALLOERR)
              RHIST_TEMP(1:93,1:RAY_HIST_NUM,0:INT(SYSTEM1(20)))=0.0D0
              RHIST_TEMP(1:93,1:RAY_HIST_NUM,0:INT(SYSTEM1(20)))=
     1        RHIST(1:93,1:RAY_HIST_NUM,0:INT(SYSTEM1(20)))
              DEALLOCATE (RHIST, STAT=ALLOERR)
              RHIST_MAXRAYS=RHIST_MAXRAYS*2
              ALLOCATE (RHIST(1:93,1:RHIST_MAXRAYS,0:INT(SYSTEM1(20)))
     1        ,STAT=ALLOERR)
              RHIST(1:93,1:RAY_HIST_NUM,0:INT(SYSTEM1(20)))=0.0D0
              RHIST(1:93,1:RAY_HIST_NUM,0:INT(SYSTEM1(20)))=
     1        RHIST_TEMP(1:93,1:RAY_HIST_NUM,0:INT(SYSTEM1(20)))
              DEALLOCATE (RHIST_TEMP, STAT=ALLOERR)
          END IF
          IF(RAYEXT) THEN
              DO J=1,50
                  DO K=0,INT(SYSTEM1(20))
                      RHIST(J,RAY_HIST_NUM,K)=RAYRAY(J,K)
                  END DO
              END DO
              DO J=1,18
                  DO K=0,INT(SYSTEM1(20))
                      RHIST(J+50,RAY_HIST_NUM,K)=RFDIFF(J,K)
                  END DO
              END DO
              DO J=1,18
                  DO K=0,INT(SYSTEM1(20))
                      RHIST(J+68,RAY_HIST_NUM,K)=DIFF(J,K)
                  END DO
              END DO
          ELSE
C       RAY FAINED BUT WRITE ZEROS AND RAYCOD
              DO J=1,50
                  DO K=0,INT(SYSTEM1(20))
                      RHIST(J,RAY_HIST_NUM,K)=0.0D0
                  END DO
              END DO
              DO J=1,18
                  DO K=0,INT(SYSTEM1(20))
                      RHIST(J+50,RAY_HIST_NUM,K)=0.0D0
                  END DO
              END DO
              DO J=1,18
                  DO K=0,INT(SYSTEM1(20))
                      RHIST(J+68,RAY_HIST_NUM,K)=0.0D0
                  END DO
              END DO
          END IF
          DO K=0,INT(SYSTEM1(20))
              RHIST(J+87,RAY_HIST_NUM,K)=REFRY(1,NEWOBJ)
              RHIST(J+88,RAY_HIST_NUM,K)=REFRY(2,NEWOBJ)
              RHIST(J+89,RAY_HIST_NUM,K)=REFRY(11,NEWOBJ)
              RHIST(J+90,RAY_HIST_NUM,K)=REFRY(12,NEWOBJ)
              RHIST(J+91,RAY_HIST_NUM,K)=FOBNUMBER
              RHIST(J+92,RAY_HIST_NUM,K)=RAYCOD(1)
              RHIST(J+93,RAY_HIST_NUM,K)=RAYCOD(2)
          END DO
          RETURN
      END
C SUB MRRAYS.FOR
      SUBROUTINE MRRAYS
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MRRAYS.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CMD LEVEL COMMAND "MRAYS".
C
!        INTEGER I,J,N
!        REAL*8 XSTEP,YSTEP,SW1,SW2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(STI.NE.1) THEN
C       SET DEFAULT NUMERICS
              IF(DF1.EQ.1) W1=0.0D0
              IF(DF2.EQ.1) W2=0.0D0
              IF(DF3.EQ.1) W3=SYSTEM1(11)
              IF(DF3.EQ.1) WW3=W3
              IF(DF4.EQ.1) W4=1.0D0
              IF(DF5.EQ.1) W5=1.0D0
              IF(W5.LE.1.0D0) W5=1.0D0
              IF(DF1.EQ.1) RW1=0.0D0
              IF(DF2.EQ.1) RW2=0.0D0
              IF(DF3.EQ.1) RW3=SYSTEM1(11)
              IF(DF3.EQ.1) WW3=W3
              IF(DF4.EQ.1) RW4=1.0D0
              IF(DF5.EQ.1) RW5=1.0D0
              IF(RW5.LE.1.0D0) RW5=1.0D0
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'MRAYS',RW1,RW2,RW3
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)RW4,RW5
              CALL SHOWIT(1)
              RETURN
          END IF
C
          MRAYSSET=.FALSE.
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"MRAYS" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              IF(DF5.EQ.0) MSG=.TRUE.
              RETURN
          END IF
C
C
C       DEFAULT QUALIFIER IS NOTHING (NO QUALIFIER)
          RCACOCH=0
          IF(WQ.EQ.'CAOB') RCACOCH=1
C
C       THE CONTROL WAVELENGTH
C       "MRAYS" COMMAND
C       CHECK FOR NATURE OF QUALIFIER WORD
          IF(SQ.EQ.1) THEN
              IF(WC.EQ.'MRAYS'.AND.WQ.NE.'CAOB') THEN
                  OUTLYNE='INVALID QUALIFIER USED WITH "MRAYS"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       NO QUALIFIER,PROCEED
          END IF
C       CHECK FOR CORRECT WAVELENGTH BOUNDS
          IF(INT(W3).NE.1.AND.
     1    INT(W3).NE.2.AND.
     1    INT(W3).NE.3.AND.
     1    INT(W3).NE.4.AND.
     1    INT(W3).NE.5.AND.
     1    INT(W3).NE.6.AND.
     1    INT(W3).NE.7.AND.
     1    INT(W3).NE.8.AND.
     1    INT(W3).NE.9.AND.
     1    INT(W3).NE.10) THEN
              OUTLYNE='WAVELENGTH NUMBER BEYOND LEGAL BOUNDS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CHECK IF REFEXT=.FALSE. IF SO STOP AND PRINT  MESSAGE

          RW1=W1
          RW2=W2
          RW3=W3
          RW4=W4
          RW5=W5
          MRAYSSET=.TRUE.
C
          RETURN
      END
C SUB MTRACER.FOR
      SUBROUTINE MTRACER
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MTRACER IT DOES THE MTRACE COMMAND
C
          INTEGER I,J,FN,RN,K,L
          REAL*8 RXSTEP,RYSTEP,RSW1,RSW2,TRW1,TRW2,TFW1,TFW2
          REAL*8 FXSTEP,FYSTEP,FSW1,FSW2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          FOBNUMBER=0
C
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"MTRACER" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.MRAYSSET) THEN
              OUTLYNE='NO MRAYS DEFINED WITH "MRAYS"'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.MFOBSSET) THEN
              OUTLYNE='NO MFOBS DEFINED WITH "MFOBS"'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          FN=INT(FW4)
          RN=INT(RW5)
          WRITE(OUTLYNE,*) 'TRACING ',RN*RN,' RAYS'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'FROM EACH OF ',FN*FN,' OBJECT POSITIONS'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'PLEASE WAIT...'
          CALL SHOWIT(1)
C
C       CASE OF ONLY ONE FOB
C
C       CALCULATE X AND Y STEPS
          IF(FN.EQ.1) THEN
              DF1=0
              DF2=0
              DF3=0
              DF4=0
              DF5=1
              SQ=0
              SST=0
              W1=FW1
              W2=FW2
              W3=0.0D0
              W4=FW3
              WC='FOB'
              REFEXT=.FALSE.
              CALL FFOB
              IF(.NOT.REFEXT) THEN
                  WRITE(OUTLYNE,*) 'CHIEF RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO MRAYS WERE TRACED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  FOBNUMBER=FOBNUMBER+1
              END IF

C
C       THIS IS THE INNER RRAY LOOP
C
C       CALCULATE X AND Y STEPS
              IF(RN.EQ.1) THEN
                  RDF5=1
                  W1=RW1
                  W2=RW2
                  W3=RW3
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  WW4=1.0D0
                  WVN=W3
                  CURLAM=W3
                  DF5=RDF5
                  WC='RAY'
                  WQ='CAOB'
                  SQ=1
                  RAYEXT=.FALSE.
                  IF(RCACOCH.EQ.1) THEN
                      CACOCH=1
                      WQ='CAOB'
                      SQ=1
                  ELSE
                      CACOCH=0
                      SQ=0
                  END IF
                  CALL RRAY
                  SQ=0
                  CACOCH=0
                  IF(RAY_HIST_FLAG)
     1            CALL HIST_RAY_SAVE
              ELSE
                  RXSTEP=2.0D0*DABS(RW2)/(DABS(DBLE(INT(RW5)))-1.0D0)
                  RYSTEP=2.0D0*DABS(RW1)/(DABS(DBLE(INT(RW5)))-1.0D0)
                  RSW2=-DABS(RW2)
                  RSW1=-DABS(RW1)
                  TRW2=RSW2
                  TRW1=RSW1
                  DO I=1,RN
                      DO J=1,RN
                          RDF5=1
                          W5=0.0D0
                          DF5=RDF5
                          W1=TRW1
                          W2=TRW2
                          W3=RW3
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=RDF5
                          WC='RAY'
                          DF4=1
                          WW4=1.0D0
                          WVN=W3
                          CURLAM=W3
                          RAYEXT=.FALSE.
                          IF(RCACOCH.EQ.1) THEN
                              CACOCH=1
                              WQ='CAOB'
                              SQ=1
                          ELSE
                              CACOCH=0
                              SQ=0
                          END IF
                          CALL RRAY
                          SQ=0
                          CACOCH=0
                          IF(RAY_HIST_FLAG)
     1                    CALL HIST_RAY_SAVE
C
                          TRW2=TRW2+RXSTEP
                      END DO
                      TRW1=TRW1+RYSTEP
                      TRW2=RSW2
                  END DO
                  WRITE(OUTLYNE,*) 'MULTI-TRACE COMPLETE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          ELSE
C       CASE OF MULTIFOBS
C
C       CALCULATE X AND Y STEPS

              FXSTEP=2.0D0*DABS(FW2)/(DABS(DBLE(INT(FW4)))-1.0D0)
              FYSTEP=2.0D0*DABS(FW1)/(DABS(DBLE(INT(FW4)))-1.0D0)
              FSW2=-DABS(FW2)
              FSW1=-DABS(FW1)
              TFW2=FSW2
              TFW1=FSW1
              DO K=1,FN
                  DO L=1,FN
                      FW5=0.0D0
                      DF5=1
                      W1=TFW1
                      W2=TFW2
                      W3=0.0D0
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=0
                      FW4=INT(SYSTEM1(11))
                      FW4=FW3
                      W4=FW3
                      WC='FOB'
                      REFEXT=.FALSE.
                      CALL FFOB
                      IF(.NOT.REFEXT) THEN
                          WRITE(OUTLYNE,*) 'CHIEF RAY COULD NOT BE TRACED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'NO MRAYS FOR CURRENT FIELD POINT WERE TRACED'
                          CALL SHOWIT(1)
                      ELSE
                          FOBNUMBER=FOBNUMBER+1
                      END IF

C
                      TFW2=TFW2+FXSTEP
C
C       THIS IS THE INNER RRAY LOOP
C
C       CALCULATE X AND Y STEPS
                      IF(RN.EQ.1) THEN
                          RW2=0.0D0
                          RW1=0.0D0
                          RDF5=1
                          W1=RW1
                          W2=RW2
                          W3=RW3
                          DF1=0
                          DF2=0
                          DF3=0
                          DF4=0
                          DF5=RDF5
                          WC='RAY'
                          DF4=1
                          WW4=1.0D0
                          WVN=W3
                          CURLAM=W3
                          RAYEXT=.FALSE.
                          IF(RCACOCH.EQ.1) THEN
                              CACOCH=1
                              WQ='CAOB'
                              SQ=1
                          ELSE
                              CACOCH=0
                              SQ=0
                          END IF
                          CALL RRAY
                          SQ=0
                          CACOCH=0
                          IF(RAY_HIST_FLAG)
     1                    CALL HIST_RAY_SAVE
                      ELSE
                          RXSTEP=2.0D0*DABS(RW2)/(DABS(DBLE(INT(RW5)))-1.0D0)
                          RYSTEP=2.0D0*DABS(RW1)/(DABS(DBLE(INT(RW5)))-1.0D0)
                          RSW2=-DABS(RW2)
                          RSW1=-DABS(RW1)
                          TRW2=RSW2
                          TRW1=RSW1
                          DO I=1,RN
                              DO J=1,RN
                                  RDF5=1
                                  W5=0.0D0
                                  DF5=RDF5
                                  W1=TRW1
                                  W2=TRW2
                                  W3=RW3
                                  DF1=0
                                  DF2=0
                                  DF3=0
                                  DF5=RDF5
                                  WC='RAY'
                                  DF4=1
                                  WW4=1.0D0
                                  WVN=W3
                                  CURLAM=W3
                                  RAYEXT=.FALSE.
                                  IF(RCACOCH.EQ.1) THEN
                                      CACOCH=1
                                      WQ='CAOB'
                                      SQ=1
                                  ELSE
                                      CACOCH=0
                                      SQ=0
                                  END IF
                                  CALL RRAY
                                  SQ=0
                                  CACOCH=0
                                  IF(RAY_HIST_FLAG)
     1                            CALL HIST_RAY_SAVE
C
                                  TRW2=TRW2+RXSTEP
                              END DO
                              TRW1=TRW1+RYSTEP
                              TRW2=RSW2
                          END DO
                      END IF
C
                  END DO
                  TFW1=TFW1+FYSTEP
                  TFW2=FSW2
              END DO
              WRITE(OUTLYNE,*) 'MULTI-TRACE COMPLETE'
              CALL SHOWIT(1)
              RETURN
          END IF
      END
C SUB RRAY.FOR
      SUBROUTINE RRAY
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE RRAY.FOR. THIS SUBROUTINE IMPLEMENTS
C       THE CMD LEVEL COMMAND "RAY".
C
          INTEGER J
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          REAL*8 ZZEMAX,ZZEMIN
          COMMON/KENMOOR/ZZEMIN,ZZEMAX
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"RAY" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              IF(DF5.EQ.0) MSG=.TRUE.
              RETURN
          END IF
          IF(DF4.EQ.1) W4=1.0D0
          IF(DF5.EQ.1) W5=0.0D0
C
C       WHEN RAY IS CALLED FROM OTHER PROGRAM LEVELS,
C       SOMETIMES CLEAR APERTURE/OBSCURATION CHECKING
C       IS RESET.
C
C       DEFAULT QUALIFIER IS NOTHING (NO QUALIFIER)
          CACOCH=0
          IF(WQ.EQ.'CAOB') CACOCH=1
C
C       SET DEFAULT NUMERICS
          IF(DF1.EQ.1) W1=0.0D0
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=SYSTEM1(11)
          IF(DF3.EQ.1) WW3=W3
          IF(DF4.EQ.1) W4=1.0D0
          IF(DF5.EQ.1) W5=0.0D0
          IF(DF5.EQ.0) MSG=.FALSE.
          WW4=W4
          WVN=W3
C       CURLAM TRACK CURRENT WAVELENGTH TRACED VIA COMMON
C       KURLAM IN DATLEN.FOR
          CURLAM=W3
C       THE CONTROL WAVELENGTH
C       NUMERIC WORD 5 ARE NOT USED IN THE CMD LEVEL
C       "RAY" COMMAND
C       CHECK FOR NATURE OF QUALIFIER WORD
          IF(SQ.EQ.1) THEN
              IF(WC.EQ.'RAY'.AND.WQ.NE.'CAOB') THEN
                  OUTLYNE='INVALID QUALIFIER USED WITH "RAY"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  IF(DF5.EQ.0) MSG=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'IRAY'.AND.WQ.NE.'CAOB') THEN
                  OUTLYNE='INVALID QUALIFIER USED WITH "IRAY"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  IF(DF5.EQ.0) MSG=.TRUE.
                  RETURN
              END IF
C       NO QUALIFIER,PROCEED
          END IF
C       CHECK FOR CORRECT WAVELENGTH BOUNDS
          IF(INT(W3).NE.1.AND.
     1    INT(W3).NE.2.AND.
     1    INT(W3).NE.3.AND.
     1    INT(W3).NE.4.AND.
     1    INT(W3).NE.5.AND.
     1    INT(W3).NE.6.AND.
     1    INT(W3).NE.7.AND.
     1    INT(W3).NE.8.AND.
     1    INT(W3).NE.9.AND.
     1    INT(W3).NE.10) THEN
              OUTLYNE='WAVELENGTH NUMBER BEYOND LEGAL BOUNDS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              IF(DF5.EQ.0) MSG=.TRUE.
              RETURN
          END IF
          IF(DF4.EQ.1) W4=1.0D0
C
C       CHECK IF REFEXT=.FALSE. IF SO STOP AND PRINT  MESSAGE
C
          IF(.NOT.REFEXT) THEN
C       NO CHIEF RAY EXISTS, STOP
              IF(MSG) THEN
                  OUTLYNE=
     1            'A CHIEF RAY DOES NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='A RAY WILL BE TRACED WITHOUT A CHIEF RAY'
                  CALL SHOWIT(1)
                  OUTLYNE='CHECK YOUR RESULTS'
                  CALL SHOWIT(1)
              END IF
          END IF
C       SET RAYCODS
          RAYCOD(1)=0
          RAYCOD(2)=-1
C
C       SET STOPP=0
          STOPP=0
C       SET RAYEXT TO TRUE
          RAYEXT=.TRUE.
C       SET FAIL TO FALSE
          FAIL=.FALSE.
C
C       NOW THE DEFAULTS HAVE BEEN SET
C       NOW CALL THE SUBROUTINE WHICH STARTS THE
C       RAY TRACE. THIS IS RAYTRA.
C
          WWQ=WQ
          WW1=W1
          WW2=W2
          WW3=W3
          WW4=W4
          WVN=W3
C       THE CALL TO RAYTRA HAS INPUTS:
C               QUALIFIER
C               WW1
C               WW2
C               WW3
C               WW4
C               CACOCH
C               FAIL
C       TRACE RAY AND RETURN
          IF(GRASET.OR.DXFSET) PLTRAY=.TRUE.
          CALL RAYTRA
          IF(GRASET.OR.DXFSET) PLTRAY=.FALSE.
          IF(STOPP.EQ.1) THEN
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              IF(DF5.EQ.0) MSG=.TRUE.
              RETURN
          ELSE
              STOPP=0
              RAYEXT=.TRUE.
          END IF
C
C       THE RETURN WILL SEND BACK VIA THE COMMON/RAYCMN COMMON
C       THE FULL RAY DATA FOR THE FULLY AIMMED AND TRACED
C       RAY.
          DO J=0,INT(SYSTEM1(20))
              IF(ALENS(34,J).EQ.18.0D0) LDIF2=.FALSE.
              IF(ALENS(34,J).EQ.18.0D0) LDIF=.FALSE.
          END DO
          IF(LDIF) THEN
C       RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
C       DATA
C       SAVE RAYRAY DATA
              SAVE=RAYRAY
C
              SRAYDIFEXT=.FALSE.
              DIFRAYTRACE=.TRUE.
              CALL DIFRAY
              DIFRAYTRACE=.FALSE.
              IF(STOPP.EQ.1) THEN
                  STOPP=0
                  RAYRAY=SAVE
                  RETURN
              ELSE
                  STOPP=0
                  RAYEXT=.TRUE.
              END IF
C
C       ALL DIFFERENTIAL RAY DATA WILL BE STORED IN ARRAYS
C       AND SET TO COMMON FROM WITHIN THE DIFRAY SUBROUTINE
C       RESTORE RAYRAY DATA
              RAYRAY=SAVE
C       NO DIF TRACE
          END IF
C
          RETURN
      END
C SUB MTRACERI_NOGRID.FOR
      SUBROUTINE MTRACERI_NOGRID
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MTRACERI IT DOES THE MTRACEI (NO QUALIFIER WORD) COMMAND
C
          INTEGER FN,RN,K,L
          REAL*8 TFW1,TFW2
          REAL*8 FXSTEP,FYSTEP,FSW1,FSW2,TT1,TT2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsp1.inc'
C
          FOBNUMBER=0
C
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"MTRACERI" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(.NOT.MFOBSSET) THEN
              OUTLYNE='NO MFOBS DEFINED WITH "MFOBS"'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          OPEN(UNIT=104,FILE=trim(HOME)//'MTRACEI.DAT')
          CALL CLOSE_FILE(104,0)
          OPEN(UNIT=104,FILE=trim(HOME)//'MTRACEI.DAT')
C
          FN=INT(FW4)
          RN=INT(RW5)
          WRITE(OUTLYNE,*) 'TRACING SPOT DIAGRAMS'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'FROM EACH OF ',FN*FN,' OBJECT POSITIONS'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'PLEASE WAIT...'
          CALL SHOWIT(1)
C
C       CASE OF ONLY ONE FOB
C
C       CALCULATE X AND Y STEPS
          IF(FN.EQ.1) THEN
              DF1=0
              DF2=0
              DF3=0
              DF4=0
              DF5=1
              SQ=0
              SST=0
              W1=FW1
              W2=FW2
              TT1=FW1
              TT2=FW2
              W3=0.0D0
              W4=FW3
              WC='FOB'
              REFEXT=.FALSE.
              CALL FFOB
              IF(.NOT.REFEXT) THEN
C       WRITE(OUTLYNE,*) 'CHIEF RAY COULD NOT BE TRACED'
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*) 'NO SPOT DIAGRAM WAS TRACED'
C       CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  FOBNUMBER=FOBNUMBER+1
              END IF
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='SPD ACC'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              IF(SPDEXT) THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='FAILACC 2 2'
                  CALL PROCES
                  GPREG(2)=REG(9)
                  INPUT='FAILACC 3 3'
                  CALL PROCES
                  GPREG(3)=REG(9)
                  INPUT='FAILACC 4 4'
                  CALL PROCES
                  GPREG(4)=REG(9)
                  INPUT='FAILACC 7 7'
                  CALL PROCES
                  GPREG(7)=REG(9)
                  REST_KDP(1)=RESTINPT(1)
              ELSE
                  GPREG(1:7)=0.0D0
              END IF
              IF(SYSTEM1(18).EQ.0.0D0) THEN
                  WRITE(104,10)
     1            SYSTEM1(14)*TT1,SYSTEM1(16)*TT2,INT(RTOT),INTTOT,DSPOT(38)
     2            ,INT(GPREG(2)),INT(GPREG(3)),INT(GPREG(4)),INT(GPREG(7))
                  WRITE(OUTLYNE,10)
     1            SYSTEM1(14)*TT1,SYSTEM1(16)*TT2,INT(RTOT)
              ELSE
                  WRITE(104,10)
     1            SYSTEM1(21)*TT1,-SYSTEM1(23)*TT2,INT(RTOT),INTTOT,DSPOT(38)
     2            ,INT(GPREG(2)),INT(GPREG(3)),INT(GPREG(4)),INT(GPREG(7))
                  WRITE(OUTLYNE,10)
     1            SYSTEM1(21)*TT1,-SYSTEM1(23)*TT2,INT(RTOT)
              END IF
              CALL SHOWIT(0)
 10           FORMAT(1X,F8.3,1X,F8.3,1X,I6,1X,G15.7,1X,G15.7,4(1X,I6))
C
              CALL CLOSE_FILE(104,1)
              WRITE(OUTLYNE,*) 'TRACE COMPLETE'
              CALL SHOWIT(1)
              RETURN
          ELSE
C       CASE OF MULTIFOBS
C
C       CALCULATE X AND Y STEPS

              FXSTEP=2.0D0*DABS(FW2)/(DABS(DBLE(INT(FW4)))-1.0D0)
              FYSTEP=2.0D0*DABS(FW1)/(DABS(DBLE(INT(FW4)))-1.0D0)
              FSW2=-DABS(FW2)
              FSW1=-DABS(FW1)
              TFW2=FSW2
              TFW1=FSW1
              DO K=1,FN
                  DO L=1,FN
                      FW5=0.0D0
                      DF5=1
                      W1=TFW1
                      W2=TFW2
                      TT1=TFW1
                      TT2=TFW2
                      W3=0.0D0
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=0
                      FW4=INT(SYSTEM1(11))
                      FW4=FW3
                      W4=FW3
                      WC='FOB'
                      REFEXT=.FALSE.
                      CALL FFOB
                      IF(.NOT.REFEXT) THEN
C       WRITE(OUTLYNE,*) 'CHIEF RAY COULD NOT BE TRACED'
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)
C    1  'NO SPOT DIAGRAM WAS TRACED FOR FOB# ',FOBNUMBER
C       CALL SHOWIT(1)
                      ELSE
                          FOBNUMBER=FOBNUMBER+1
                      END IF

C
                      TFW2=TFW2+FXSTEP
C
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='SPD ACC'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      IF(SPDEXT) THEN
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT='FAILACC 2'
                          CALL PROCES
                          GPREG(2)=REG(9)
                          INPUT='FAILACC 3'
                          CALL PROCES
                          GPREG(3)=REG(9)
                          INPUT='FAILACC 4'
                          CALL PROCES
                          GPREG(4)=REG(9)
                          INPUT='FAILACC 7'
                          CALL PROCES
                          GPREG(7)=REG(9)
                          REST_KDP(1)=RESTINPT(1)
                      ELSE
                          GPREG(1:7)=0.0D0
                      END IF
                      IF(SYSTEM1(18).EQ.0.0D0) THEN
                          WRITE(104,10)
     1                    SYSTEM1(14)*TT1,SYSTEM1(16)*TT2,INT(RTOT),INTTOT,DSPOT(38)
     2                    ,INT(GPREG(2)),INT(GPREG(3)),INT(GPREG(4)),INT(GPREG(7))
                          WRITE(OUTLYNE,10)
     1                    SYSTEM1(14)*TT1,SYSTEM1(16)*TT2,INT(RTOT)
                      ELSE
                          WRITE(104,10)
     1                    SYSTEM1(21)*TT1,-SYSTEM1(23)*TT2,INT(RTOT),INTTOT,DSPOT(38)
     2                    ,INT(GPREG(2)),INT(GPREG(3)),INT(GPREG(4)),INT(GPREG(7))
                          WRITE(OUTLYNE,10)
     1                    SYSTEM1(21)*TT1,-SYSTEM1(23)*TT2,INT(RTOT)
                      END IF
                      CALL SHOWIT(0)
C
                  END DO
                  TFW1=TFW1+FYSTEP
                  TFW2=FSW2
              END DO
          END IF
          CALL CLOSE_FILE(104,1)
          WRITE(OUTLYNE,*) 'TRACE COMPLETE'
          CALL SHOWIT(1)
          RETURN
      END
      SUBROUTINE READIRAD
          IMPLICIT NONE
          INTEGER I,J,L
          REAL*8 DKK,DIARRAY_DIM,DFN
          REAL*8 YFOV,XFOV,RTOT,INTEN,AOI,AREA_FILL
          REAL*8 IGRID1,IGRID2,IGRID3
          INTEGER KK,IARRAY_DIM,FN,K
          INCLUDE 'datmai.inc'
C       NRECL IS ONE 4 BYTE WORD
          OPEN(UNIT=105,ACCESS='DIRECT',FILE=trim(HOME)//'IRAD.DAT',
     1    CONVERT='LITTLE_ENDIAN',
     2    FORM='UNFORMATTED',RECL=(6*NRECL),STATUS='UNKNOWN')
C       READ MAIN HEADER
          K=1
          READ(UNIT=105,REC=K) DKK,DIARRAY_DIM,DFN
          KK=INT(DKK)
          IARRAY_DIM=INT(DIARRAY_DIM)
          FN=INT(DFN)
          WRITE(OUTLYNE,*) KK,IARRAY_DIM,FN
          CALL SHOWIT(1)
C
          DO L=1,FN*FN
C       THERE ARE FN*FN FIELD OF VIEW POSITIONS TO READ
C       READ THE FOV HEADER
              K=K+1
              READ(UNIT=105,REC=K)
     1        YFOV,XFOV,RTOT
              WRITE(OUTLYNE,*) YFOV,XFOV,RTOT
              CALL SHOWIT(1)
              K=K+1
              READ(UNIT=105,REC=K)
     1        INTEN,AOI,AREA_FILL
              WRITE(OUTLYNE,*) INTEN,AOI,AREA_FILL
              CALL SHOWIT(1)
C       READ THE INTENSITY GRID DATA
              DO J=1,IARRAY_DIM
                  DO I=1,IARRAY_DIM
                      K=K+1
                      READ(UNIT=105,REC=K)IGRID1,IGRID2,IGRID3
                      WRITE(OUTLYNE,*) I,J,IGRID1,IGRID2,IGRID3
                      CALL SHOWIT(1)
                  END DO
              END DO
          END DO
          RETURN
      END
C SUB MTRACERI_GRID1.FOR
      SUBROUTINE MTRACERI_GRID1
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MTRACERI IT DOES THE MTRACEI1 (GRID) COMMAND
C
          INTEGER I,J,FN,RN,K,L,ALLOERR,NX,NY,ID1,KK
          REAL*8 TFW1,TFW2
          REAL*8 FXSTEP,FYSTEP,FSW1,FSW2,TT1,TT2,ARRAYLENGTH
          REAL*8 XDEL,YDEL,XCORRI,YCORRI,X,Y,Z,RRXI,RRYI
          REAL*8 FAREA,TAREA,AREA_FILL
          REAL*8 XSPAN,YSPAN,SPSURF
          LOGICAL XODD,YODD,ISITIN
          REAL*8 IX,IY
          REAL*8 DSP38
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
C       COPY OF THE ASSOCIATED VARIABLES IN GLOBALS.FOR
C       MTRACEI GRID VARIABLES
C       REAL*8, IGRID,IXARRAY,IYARRAY
C       INTEGER IARRAY_DIM
C       DIMENSION IGRID(:,:,:),IXARRAY(:,:),IYARRAY(:,:)
C       IGRID(IX,IY,ICODE)
C       ICODE=1 IF 0, GRID CELL DOES NOT COUNT AS IT IS OUTSIDE THE ASSOCIATED CLAP
C                  1, GRID CELL COUNTS
C       ICODE=2    , SUMMED RAY INTENSITY
C       ALLOCATABLE :: IGRID,IXARRAY,IYARRAY
C       END MULTI-RAY INTENSITY VARIABLES
C
          FOBNUMBER=0
C
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"MTRACERI GRID" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"MTRACERI GRID" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              OUTLYNE=
     1        '"MTRACERI GRID" REQUIRES EXPLICIT NUMERIC WORDS #1 AND #2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.1.0D0) THEN
              OUTLYNE=
     1        '"MTRACERI GRID" REQUIRES A POSITIVE GRID DIMENSIONALITY, (NW1)'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'EQUAL TO 2 OR GREATER'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LE.0.0D0) THEN
              OUTLYNE=
     1        '"MTRACERI GRID" REQUIRES A POSITIVE, NON-ZERO GRID SIZE (NW2)'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF3.EQ.1) THEN
              DF3=0
              W3=DBLE(NEWIMG)
              SPSURF=W3
          ELSE
              SPSURF=W3
          END IF

          IARRAY_DIM=INT(W1)
          ARRAYLENGTH=W2
          IF(.NOT.MFOBSSET) THEN
              OUTLYNE='NO MFOBS DEFINED WITH "MFOBS"'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       SET UP THE GRID ARRAY AND DETERMINE WHICH GRID POINTS ARE
C       INSIDE ANY CLEAR APERTURE ON THE SPOT DIAGRAM SURFACE
          DEALLOCATE(IXARRAY,
     1    IYARRAY,IGRID
     1    ,STAT=ALLOERR)
          ALLOCATE(IXARRAY(1:IARRAY_DIM,1:IARRAY_DIM),
     1    IYARRAY(1:IARRAY_DIM,1:IARRAY_DIM),IGRID(
     1    1:IARRAY_DIM,1:IARRAY_DIM,1:10),STAT=ALLOERR)
          IXARRAY(1:IARRAY_DIM,1:IARRAY_DIM)=0.0D0
          IYARRAY(1:IARRAY_DIM,1:IARRAY_DIM)=0.0D0
          IGRID(1:IARRAY_DIM,1:IARRAY_DIM,1:10)=0.0D0
          NX=IARRAY_DIM
          NY=IARRAY_DIM
C       ARE THE MEM DIMENSIONS ODD OR EVEN
C       ODD OR EVEN
          XDEL=ARRAYLENGTH/DBLE(NX)
          YDEL=ARRAYLENGTH/DBLE(NY)
          XODD=.FALSE.
          IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) XODD=.TRUE.
          YODD=.FALSE.
          IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) YODD=.TRUE.
C
C       ESTABLISH THE PIXEL CENTER LOCATION VALUES
          IF(XODD) THEN
              DO I=0,NX-1
                  DO K=1,NY
                      IXARRAY(I+1,K)=-(XDEL*DBLE((NX-1)/2))+(DBLE(I)*XDEL)
                  END DO
              END DO
          END IF
          IF(.NOT.XODD) THEN
              DO I=1,NX
                  DO K=1,NY
                      IXARRAY(I,K)=-(XDEL*(DBLE(NX+1)/2.0D0))+(DBLE(I)*XDEL)
                  END DO
              END DO
          END IF
          IF(YODD) THEN
              DO I=1,NX
                  DO K=0,NY-1
                      IYARRAY(I,K+1)=-(YDEL*DBLE((NY-1)/2))+(DBLE(K)*YDEL)
                  END DO
              END DO
          END IF
          IF(.NOT.YODD) THEN
              DO I=1,NX
                  DO K=1,NY
                      IYARRAY(I,K)=-(YDEL*(DBLE(NY+1)/2.0D0))+(DBLE(K)*YDEL)
                  END DO
              END DO
          END IF
          XCORRI=IXARRAY(1,1)
          YCORRI=IYARRAY(1,1)
C       IS THERE A CLAP ASSIGNED TO THE SPOT SURFACE
C       LIMITED TO CIRCULAR, ELLIPSE,RECTANGULAR OF RACETRACE,POLY OR IPOLY, NO MULTI-POLYS
C       THEN SEE IF ANY GRID POINTS ARE OUTSIDE THE CLAP AND ARE TO BE DIS-ALLOWED
C       CALLCULATE TOTAL AREA OF THE CLAP ON THE SPOT DIAGRAM SURFACE
          IF(ALENS(9,NEWIMG).EQ.1.0D0.OR.
     1    ALENS(9,NEWIMG).EQ.2.0D0.OR.
     1    ALENS(9,NEWIMG).EQ.3.0D0.OR.
     1    ALENS(9,NEWIMG).EQ.4.0D0) THEN
              IF(ALENS(9,NEWIMG).EQ.1.0D0) THEN
                  TAREA=(ALENS(10,NEWIMG)**2)*PII
              END IF
              IF(ALENS(9,NEWIMG).EQ.2.0D0) THEN
                  TAREA=(4.0D0*ALENS(10,NEWIMG)*ALENS(11,NEWIMG))
              END IF
              IF(ALENS(9,NEWIMG).EQ.3.0D0) THEN
                  TAREA=ALENS(11,NEWIMG)*ALENS(10,NEWIMG)*PII
              END IF
              IF(ALENS(9,NEWIMG).EQ.4.0D0) THEN
                  TAREA=(4.0D0*ALENS(10,NEWIMG)*ALENS(11,NEWIMG))
     1            -(
     1            ((2.0D0*ALENS(14,NEWIMG))**2)
     1            -(PII*(ALENS(14,NEWIMG)**2)))
              END IF
C       DIS-ALLOW POINTS
              DO I=1,NX
                  DO J=1,NY
                      X=IXARRAY(I,J)
                      Y=IYARRAY(I,J)
                      Z=0.0D0
                      ISITIN=.FALSE.
                      CALL SAGCACO(NEWIMG,X,Y,Z,ISITIN)
                      IF(.NOT.ISITIN) THEN
                          IGRID(I,J,1)=0.0D0
C       CAN'T WRITE INTO THAT SQUARE, IT IS OUTSIDE THE CLAP
                      ELSE
C       CAN WRITE INTO THAT SQUARE, IT IS INSIDE THE CLAP
                          IGRID(I,J,1)=1.0D0
                      END IF
                  END DO
              END DO
          ELSE
C       ALL POINTS ON GRID ARE TO BE USED.
              TAREA=DABS(ARRAYLENGTH**2)
              IGRID(1:NX,1:NY,1)=1.0D0
          END IF

          OPEN(UNIT=104,FILE=trim(HOME)//'MTRACEI.DAT')
          CALL CLOSE_FILE(104,0)
          OPEN(UNIT=104,FILE=trim(HOME)//'MTRACEI.DAT')
C
C       OPEN AND CLOSE IRRADIANCE FILE
          OPEN(UNIT=105,ACCESS='DIRECT',FILE=trim(HOME)//'IRAD.DAT',
     1    CONVERT='LITTLE_ENDIAN',
     2    FORM='UNFORMATTED',RECL=(6*NRECL),STATUS='UNKNOWN')
          CALL CLOSE_FILE(105,0)
C       OPEN IRRADIANCE FILE
          OPEN(UNIT=105,ACCESS='DIRECT',FILE=trim(HOME)//'IRAD.DAT',
     1    CONVERT='LITTLE_ENDIAN',
     2    FORM='UNFORMATTED',RECL=(6*NRECL),STATUS='UNKNOWN')
C
          FN=INT(FW4)
          RN=INT(RW5)
          WRITE(OUTLYNE,*) 'TRACING SPOT DIAGRAMS'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'FROM EACH OF ',FN*FN,' OBJECT POSITIONS'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'PLEASE WAIT...'
          CALL SHOWIT(1)
C
C       CASE OF ONLY ONE FOB
C
C       CALCULATE X AND Y STEPS
          IF(FN.EQ.1) THEN
              DF1=0
              DF2=0
              DF3=0
              DF4=0
              DF5=1
              SQ=0
              WQ='        '
              SST=0
              W1=FW1
              W2=FW2
              TT1=FW1
              TT2=FW2
              W3=0.0D0
              W4=FW3
              WC='FOB'
              REFEXT=.FALSE.
              CALL FFOB
              IF(.NOT.REFEXT) THEN
                  WRITE(OUTLYNE,*) 'CHIEF RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO SPOT DIAGRAM WAS TRACED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  FOBNUMBER=FOBNUMBER+1
              END IF
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='SPD ACC'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              KK=1
              IF(SPDEXT) THEN
C     NOW CALC TAREA
                  DO I=ID1+1,ITOT-1 
C     LOAD DSPOTT(I,*) INTO DSPOT(*)
                      ID=I
                      CALL SPOTIT(4)
                      IF(DSPOT(38).NE.0.0D0) THEN
                          DSP38=DSPOT(38)
                          XSPAN=24.0D0/99.0D0
                          YSPAN=24.0D0/99.0D0
                          EXIT
                      ELSE
C       SKIP, NO ENERGY
                      END IF
                  END DO
C
                  IF(dsp38.LT.0.03d0) THEN
                      farea=0.0d0
                  ELSE
                      IF(DSP38.NE.0.0D0) THEN
                          FAREA=REFRY(9,1)*IRTOT*XSPAN*YSPAN/DSP38
                      ELSE
                          FAREA=0.0D0
                      END IF
                  END IF
                  IF(tarea.EQ.0.0d0) THEN
                      area_fill=0.0d0
                  ELSE
                      area_fill=farea/tarea
                  END IF
              ELSE
C       NO SPOT EXISTS
                  area_fill=0.0d0
                  WRITE(OUTLYNE,*) 'NO SPOT DIAGRAM EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO FILL AREA OR IGRID DATA WAS WRITTEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              end if
              if(area_fill.eq.0.0d0) inttot=0.0d0

              IGRID(1:IARRAY_DIM,1:IARRAY_DIM,2:10)=0.0D0
              DO I=1,ITOT-1
C     LOAD DSPOTT(I,*) INTO DSPOT(*)
                  ID=I
                  CALL SPOTIT(4)
C       LOAD THE IGRID ARRAY
                  RRXI=DSPOT(45)
                  RRYI=DSPOT(46)
                  IX=NINT(((RRXI-XCORRI)/XDEL))+1
                  IY=NINT(((RRYI-YCORRI)/YDEL))+1
                  IF(IX.LT.1.OR.IX.GT.NX.OR.IY.LT.1.OR.IY.GT.NY) THEN
C       RAY WAS OUTSIDE THE INTENSITY ARRAY GRID AND WILL NOT BE FURTHER PROCESSED
                  ELSE
C       RAY WAS INSIDE THE ARRAY GRID
                      IF(IGRID(INT(IX),INT(IY),1).NE.0.0D0) THEN
C
C       ADD IRRADIANCE OF RAY
                          IGRID(INT(IX),INT(IY),2)=IGRID(INT(IX),INT(IY),2)+DSPOT(12)
C
C       ADD IRRADIANCE OF RAY TIMES THE ANGLE OF INCIDENCE AT NEWIMG SURFACE
                          IGRID(INT(IX),INT(IY),3)=IGRID(INT(IX),INT(IY),2)+
     &                    (DSPOT(12)*DSPOT(39))
C
C       INCREMENT THE NUMBER OF RAYS IN THE IX,IY BOX
                          IGRID(INT(IX),INT(IY),4)=IGRID(INT(IX),INT(IY),3)+1.0D0
C
C       TOTAL SUM OF COS(I)S IN BOX ON SPOT SURFACE
                          IGRID(INT(IX),INT(IY),5)=IGRID(INT(IX),INT(IY),4)+DSPOT(51)
C
C       AVERAGE OF COS(I)S IN BOX ON SPOT SURFACE
                          IGRID(INT(IX),INT(IY),6)=dspot(38)
C
                      ELSE
C       DON'T ADD ANYTHING
                      END IF
                  END IF
              END DO
C       NOW WRITE OUT THE CURRENT CONTENTS OF THE IGRID ARRAY
C       TO THE IRRADIANCE FILE

              WRITE(104,10)
     1        SYSTEM1(21)*TT1,-SYSTEM1(23)*TT2,INT(RTOT),INTTOT,DSP38
     3        ,AREA_FILL
              WRITE(OUTLYNE,11)
     1        SYSTEM1(21)*TT1,-SYSTEM1(23)*TT2,AREA_FILL,int(rtot)
     2        ,DACOS(DSP38)*180.0D0/PII
              CALL SHOWIT(0)
 11           FORMAT(1X,F8.3,1X,F8.3,1X,F8.3,1X,I6,1X,F8.3)
 10           FORMAT(1X,F8.3,1X,F8.3,1X,I6,1X,G15.7,1X,G15.7,
     1        1X,G15.7)
C       WRITE OUT THE IRAD.DAT FILE HEADER, THEN THE DATA
C       FOR THE CURRENT FOB
              KK=KK+1
              WRITE(UNIT=105,REC=KK)
     1        SYSTEM1(21)*TT1,SYSTEM1(23)*TT2,RTOT
              KK=KK+1
              WRITE(UNIT=105,REC=KK)
     1        INTTOT,DSP38,AREA_FILL
              DO J=1,NX
                  DO I=1,NY
                      KK=KK+1
                      WRITE(UNIT=105,REC=KK)IGRID(I,J,1),
     1                IGRID(I,J,2),IGRID(I,J,6)
                  END DO
              END DO
              FN=1
              WRITE(UNIT=105,REC=1) DBLE(KK),DBLE(IARRAY_DIM),
     1        DBLE(FN)
              CALL CLOSE_FILE(104,1)
              CALL CLOSE_FILE(105,1)
              WRITE(OUTLYNE,*) 'TRACE COMPLETE'
              CALL SHOWIT(1)
              RETURN
          ELSE
              KK=1
C       CASE OF MULTIFOBS
C
C       CALCULATE X AND Y STEPS

              FXSTEP=2.0D0*DABS(FW2)/(DABS(DBLE(INT(FW4)))-1.0D0)
              FYSTEP=2.0D0*DABS(FW1)/(DABS(DBLE(INT(FW4)))-1.0D0)
              FSW2=-DABS(FW2)
              FSW1=-DABS(FW1)
              TFW2=FSW2
              TFW1=FSW1
              DO K=1,FN
                  DO L=1,FN
                      FW5=0.0D0
                      DF5=1
                      W1=TFW1
                      W2=TFW2
                      TT1=TFW1
                      TT2=TFW2
                      W3=0.0D0
                      SQ=0
                      WQ='        '
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=0
                      FW4=INT(SYSTEM1(11))
                      FW4=FW3
                      W4=FW3
                      WC='FOB'
                      REFEXT=.FALSE.
                      CALL FFOB
                      IF(.NOT.REFEXT) THEN
C       WRITE(OUTLYNE,*) 'CHIEF RAY COULD NOT BE TRACED'
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)
C    1  'NO SPOT DIAGRAM WAS TRACED FOR FOB# ',FOBNUMBER
C       CALL SHOWIT(1)
                      ELSE
                          FOBNUMBER=FOBNUMBER+1
                      END IF

C
                      TFW2=TFW2+FXSTEP
C
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='SPD ACC'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      IF(SPDEXT) THEN
C       NOW CALC TAREA
                          DO I=ID1+1,ITOT-1
C     LOAD DSPOTT(I,*) INTO DSPOT(*)
                              ID=I
                              CALL SPOTIT(4)
                              IF(DSPOT(38).NE.0.0D0) THEN
                                  DSP38=DSPOT(38)
                                  XSPAN=24.0D0/99.0D0
                                  YSPAN=24.0D0/99.0D0
                                  EXIT
                              ELSE
C       SKIP, NO ENERGY
                              END IF
                          END DO
C
                          if(dsp38.lt.0.03d0) then
                              farea=0.0d0
                          else
                              IF(DSP38.NE.0.0D0) THEN
                                  FAREA=REFRY(9,1)*IRTOT*XSPAN*YSPAN/DSP38
                              ELSE
                                  FAREA=0.0D0
                              END IF
                          end if
                          if(tarea.eq.0.0d0) then
                              area_fill=0.0d0
                          else
                              area_fill=farea/tarea
                          end if
                      else
C       NO SPOT EXISTS
                          area_fill=0.0d0
                      end if
                      if(area_fill.eq.0.0d0) inttot=0.0d0
C
C       GREATE THE IGRID ARRAY
                      IGRID(1:IARRAY_DIM,1:IARRAY_DIM,2:10)=0.0D0
                      IF(SPDEXT) THEN
C       SPOT EXISTS
                          DO I=1,ITOT-1
C     LOAD DSPOTT(I,*) INTO DSPOT(*)
                              ID=I
                              CALL SPOTIT(4)
C       LOAD THE IGRID ARRAY
                              RRXI=DSPOT(45)
                              RRYI=DSPOT(46)
                              IX=NINT(((RRXI-XCORRI)/XDEL))+1
                              IY=NINT(((RRYI-YCORRI)/YDEL))+1
                              IF(IX.LT.1.OR.IX.GT.NX.OR.IY.LT.1.OR.IY.GT.NY) THEN
C       RAY WAS OUTSIDE THE INTENSITY ARRAY GRID AND WILL NOT BE FURTHER PROCESSED
                              ELSE
C       RAY WAS INSIDE THE ARRAY GRID
                                  IF(IGRID(INT(IX),INT(IY),1).NE.0.0D0) THEN
C
C       ADD IRRADIANCE OF RAY
                                      IGRID(INT(IX),INT(IY),2)=IGRID(INT(IX),INT(IY),2)+DSPOT(12)
C
C       ADD IRRADIANCE OF RAY TIMES THE ANGLE OF INCIDENCE AT NEWIMG SURFACE
                                      IGRID(INT(IX),INT(IY),3)=IGRID(INT(IX),INT(IY),2)+
     1                                (DSPOT(12)*DSPOT(39))
C
C       INCREMENT THE NUMBER OF RAYS IN THE IX,IY BOX
                                      IGRID(INT(IX),INT(IY),4)=IGRID(INT(IX),INT(IY),3)+1.0D0
C
C       TOTAL SUM OF COS(I)S IN BOX ON SPOT SURFACE
                                      IGRID(INT(IX),INT(IY),5)=IGRID(INT(IX),INT(IY),4)+DSPOT(51)
C
C       AVERAGE OF COS(I)S IN BOX ON SPOT SURFACE
                                      IGRID(INT(IX),INT(IY),6)=dspot(38)
C
                                  ELSE
C       DON'T ADD ANYTHING
                                  END IF
                              END IF
                          END DO
                      ELSE
C       NO SPOT, IGRID IS ALL ZERO
                      END IF
C       NOW WRITE OUT THE CURRENT CONTENTS OF THE IGRID ARRAY
C       TO THE IRRADIANCE FILE
                      WRITE(104,10)
     1                SYSTEM1(21)*TT1,-SYSTEM1(23)*TT2,INT(RTOT),INTTOT,DSP38
     3                ,AREA_FILL
                      WRITE(OUTLYNE,11)
     1                SYSTEM1(21)*TT1,-SYSTEM1(23)*TT2,AREA_FILL,int(rtot)
     2                ,DACOS(DSP38)*180.0D0/PII
                      CALL SHOWIT(0)
C       WRITE OUT THE IRAD.DAT FILE HEADER, THEN THE DATA
C       FOR THE CURRENT FOB
                      KK=KK+1
                      WRITE(UNIT=105,REC=KK)
     1                SYSTEM1(21)*TT1,SYSTEM1(23)*TT2,RTOT
                      KK=KK+1
                      WRITE(UNIT=105,REC=KK)
     1                INTTOT,DSP38,AREA_FILL
                      DO J=1,NX
                          DO I=1,NY
                              KK=KK+1
                              WRITE(UNIT=105,REC=KK)IGRID(I,J,1),
     1                        IGRID(I,J,2),IGRID(I,J,6)
                          END DO
                      END DO
C
                  END DO
                  TFW1=TFW1+FYSTEP
                  TFW2=FSW2
              END DO
          END IF
          WRITE(UNIT=105,REC=1) DBLE(KK),DBLE(IARRAY_DIM),DBLE(FN)
          CALL CLOSE_FILE(104,1)
          CALL CLOSE_FILE(105,1)
          WRITE(OUTLYNE,*) 'TRACE COMPLETE'
          CALL SHOWIT(1)
          RETURN
      END
C SUB MTRACERI_GRID2.FOR
      SUBROUTINE MTRACERI_GRID2
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE MTRACERI IT DOES THE MTRACEI2 (GRID) COMMAND
C
          INTEGER I,J,FN,RN,K,L,ALLOERR,NX,NY,KK
          REAL*8 TFW1,TFW2
          REAL*8 FXSTEP,FYSTEP,FSW1,FSW2,TT1,TT2,ARRAYLENGTH
          REAL*8 XDEL,YDEL,XCORRI,YCORRI,X,Y,Z,RRXI,RRYI
          REAL*8 FAREA,TAREA,AREA_FILL
          REAL*8 SPSURF
          LOGICAL XODD,YODD,ISITIN
          REAL*8 IX,IY
          REAL*8 DSP38

C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
C       COPY OF THE ASSOCIATED VARIABLES IN GLOBALS.FOR
C       MTRACEI GRID VARIABLES
C       REAL*8, IGRID,IXARRAY,IYARRAY
C       INTEGER IARRAY_DIM
C       DIMENSION IGRID(:,:,:),IXARRAY(:,:),IYARRAY(:,:)
C       IGRID(IX,IY,ICODE)
C       ICODE=1 IF 0, GRID CELL DOES NOT COUNT AS IT IS OUTSIDE THE ASSOCIATED CLAP
C                  1, GRID CELL COUNTS
C       ICODE=2    , SUMMED RAY INTENSITY
C       ALLOCATABLE :: IGRID,IXARRAY,IYARRAY
C       END MULTI-RAY INTENSITY VARIABLES
C
          FOBNUMBER=0
C
C
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1) THEN
              OUTLYNE='"MTRACERI GRID" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"MTRACERI GRID" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              OUTLYNE=
     1        '"MTRACERI GRID" REQUIRES EXPLICIT NUMERIC WORDS #1 AND #2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.1.0D0) THEN
              OUTLYNE=
     1        '"MTRACERI GRID" REQUIRES A POSITIVE GRID DIMENSIONALITY, (NW1)'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'EQUAL TO 2 OR GREATER'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.LE.0.0D0) THEN
              OUTLYNE=
     1        '"MTRACERI GRID" REQUIRES A POSITIVE, NON-ZERO GRID SIZE (NW2)'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF3.EQ.1) THEN
              DF3=0
              W3=DBLE(NEWIMG)
              SPSURF=W3
          ELSE
              SPSURF=W3
          END IF

          IARRAY_DIM=INT(W1)
          ARRAYLENGTH=W2
          IF(.NOT.MFOBSSET) THEN
              OUTLYNE='NO MFOBS DEFINED WITH "MFOBS"'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       SET UP THE GRID ARRAY AND DETERMINE WHICH GRID POINTS ARE
C       INSIDE ANY CLEAR APERTURE ON THE SPOT DIAGRAM SURFACE
          DEALLOCATE(IXARRAY,
     1    IYARRAY,IGRID
     1    ,STAT=ALLOERR)
          ALLOCATE(IXARRAY(1:IARRAY_DIM,1:IARRAY_DIM),
     1    IYARRAY(1:IARRAY_DIM,1:IARRAY_DIM),IGRID(
     1    1:IARRAY_DIM,1:IARRAY_DIM,1:10),STAT=ALLOERR)
          IXARRAY(1:IARRAY_DIM,1:IARRAY_DIM)=0.0D0
          IYARRAY(1:IARRAY_DIM,1:IARRAY_DIM)=0.0D0
          IGRID(1:IARRAY_DIM,1:IARRAY_DIM,1:10)=0.0D0
          NX=IARRAY_DIM
          NY=IARRAY_DIM
C       ARE THE MEM DIMENSIONS ODD OR EVEN
C       ODD OR EVEN
          XDEL=ARRAYLENGTH/DBLE(NX)
          YDEL=ARRAYLENGTH/DBLE(NY)
          XODD=.FALSE.
          IF((DBLE(NX)/2.0D0)-DBLE(NX/2).NE.0.0D0) XODD=.TRUE.
          YODD=.FALSE.
          IF((DBLE(NY)/2.0D0)-DBLE(NY/2).NE.0.0D0) YODD=.TRUE.
C
C       ESTABLISH THE PIXEL CENTER LOCATION VALUES
          IF(XODD) THEN
              DO I=0,NX-1
                  DO K=1,NY
                      IXARRAY(I+1,K)=-(XDEL*DBLE((NX-1)/2))+(DBLE(I)*XDEL)
                  END DO
              END DO
          END IF
          IF(.NOT.XODD) THEN
              DO I=1,NX
                  DO K=1,NY
                      IXARRAY(I,K)=-(XDEL*(DBLE(NX+1)/2.0D0))+(DBLE(I)*XDEL)
                  END DO
              END DO
          END IF
          IF(YODD) THEN
              DO I=1,NX
                  DO K=0,NY-1
                      IYARRAY(I,K+1)=-(YDEL*DBLE((NY-1)/2))+(DBLE(K)*YDEL)
                  END DO
              END DO
          END IF
          IF(.NOT.YODD) THEN
              DO I=1,NX
                  DO K=1,NY
                      IYARRAY(I,K)=-(YDEL*(DBLE(NY+1)/2.0D0))+(DBLE(K)*YDEL)
                  END DO
              END DO
          END IF
          XCORRI=IXARRAY(1,1)
          YCORRI=IYARRAY(1,1)
C       IS THERE A CLAP ASSIGNED TO THE SPOT SURFACE
C       LIMITED TO CIRCULAR, ELLIPSE,RECTANGULAR OF RACETRACE,POLY OR IPOLY, NO MULTI-POLYS
C       THEN SEE IF ANY GRID POINTS ARE OUTSIDE THE CLAP AND ARE TO BE DIS-ALLOWED
C       DIS-ALLOW POINTS
          IF(ALENS(9,NEWIMG).EQ.1.0D0.OR.
     1    ALENS(9,NEWIMG).EQ.2.0D0.OR.
     1    ALENS(9,NEWIMG).EQ.3.0D0.OR.
     1    ALENS(9,NEWIMG).EQ.4.0D0) THEN
              DO I=1,NX
                  DO J=1,NY
                      X=IXARRAY(I,J)
                      Y=IYARRAY(I,J)
                      Z=0.0D0
                      ISITIN=.FALSE.
                      CALL SAGCACO(SPDSURF,X,Y,Z,ISITIN)
                      IF(.NOT.ISITIN) THEN
                          IGRID(I,J,1)=0.0D0
C       CAN'T WRITE INTO THAT SQUARE, IT IS OUTSIDE THE CLAP
                      ELSE
C       CAN WRITE INTO THAT SQUARE, IT IS INSIDE THE CLAP
                          IGRID(I,J,1)=1.0D0
                      END IF
                  END DO
              END DO
          ELSE
C       ALL POINTS ON GRID ARE TO BE USED.
              TAREA=DABS(ARRAYLENGTH**2)
              IGRID(1:NX,1:NY,1)=1.0D0
          END IF

          OPEN(UNIT=104,FILE=trim(HOME)//'MTRACEI.DAT')
          CALL CLOSE_FILE(104,0)
C
C       OPEN AND CLOSE IRRADIANCE FILE
          OPEN(UNIT=105,ACCESS='DIRECT',FILE=trim(HOME)//'IRAD.DAT',
     1    CONVERT='LITTLE_ENDIAN',
     2    FORM='UNFORMATTED',RECL=(6*NRECL),STATUS='UNKNOWN')
          CALL CLOSE_FILE(105,0)
C       OPEN IRRADIANCE FILE
          OPEN(UNIT=105,ACCESS='DIRECT',FILE=trim(HOME)//'IRAD.DAT',
     1    CONVERT='LITTLE_ENDIAN',
     2    FORM='UNFORMATTED',RECL=(6*NRECL),STATUS='UNKNOWN')
C
          FN=INT(FW4)
          RN=INT(RW5)
          WRITE(OUTLYNE,*) 'TRACING SPOT DIAGRAMS'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'FROM EACH OF ',FN*FN,' OBJECT POSITIONS'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'PLEASE WAIT...'
          CALL SHOWIT(1)
C
C       CASE OF ONLY ONE FOB
C
C       CALCULATE X AND Y STEPS
          IF(FN.EQ.1) THEN
              DF1=0
              DF2=0
              DF3=0
              DF4=0
              DF5=1
              SQ=0
              WQ='        '
              SST=0
              W1=FW1
              W2=FW2
              TT1=FW1
              TT2=FW2
              W3=0.0D0
              W4=FW3
              WC='FOB'
              REFEXT=.FALSE.
              CALL FFOB
              IF(.NOT.REFEXT) THEN
                  WRITE(OUTLYNE,*) 'CHIEF RAY COULD NOT BE TRACED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO SPOT DIAGRAM WAS TRACED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE

                  FOBNUMBER=FOBNUMBER+1
              END IF
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='SPD ACC,,,3'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              KK=1
              IF(SPDEXT) THEN
                  area_fill=0.0D0
              else
C       NO SPOT EXISTS
                  area_fill=0.0d0
                  WRITE(OUTLYNE,*) 'NO SPOT DIAGRAM EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO FILL AREA OR IGRID DATA WAS WRITTEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              end if
              if(area_fill.eq.0.0d0) inttot=0.0d0

              IGRID(1:IARRAY_DIM,1:IARRAY_DIM,2:10)=0.0D0
              DO I=1,ITOT-1
C     LOAD DSPOTT(I,*) INTO DSPOT(*)
                  ID=I
                  CALL SPOTIT(4)
C       LOAD THE IGRID ARRAY
                  RRXI=DSPOT(45)
                  RRYI=DSPOT(46)
                  IX=NINT(((RRXI-XCORRI)/XDEL))+1
                  IY=NINT(((RRYI-YCORRI)/YDEL))+1
                  IF(IX.LT.1.OR.IX.GT.NX.OR.IY.LT.1.OR.IY.GT.NY) THEN
C       RAY WAS OUTSIDE THE INTENSITY ARRAY GRID AND WILL NOT BE FURTHER PROCESSED
                  ELSE
C       RAY WAS INSIDE THE ARRAY GRID
                      IF(IGRID(INT(IX),INT(IY),1).NE.0.0D0) THEN
C
C       ADD IRRADIANCE OF RAY
                          IGRID(INT(IX),INT(IY),2)=IGRID(INT(IX),INT(IY),2)+DSPOT(12)
C
C       ADD IRRADIANCE OF RAY TIMES THE ANGLE OF INCIDENCE AT NEWIMG SURFACE
                          IGRID(INT(IX),INT(IY),3)=IGRID(INT(IX),INT(IY),2)+(DSPOT(12)
     &                    *DSPOT(39))
C
C       INCREMENT THE NUMBER OF RAYS IN THE IX,IY BOX
                          IGRID(INT(IX),INT(IY),4)=IGRID(INT(IX),INT(IY),3)+1.0D0
C
C       TOTAL SUM OF COS(I)S IN BOX ON SPOT SURFACE
                          IGRID(INT(IX),INT(IY),5)=IGRID(INT(IX),INT(IY),4)+DSPOT(51)
C
C       AVERAGE OF COS(I)S IN BOX ON SPOT SURFACE
                          IGRID(INT(IX),INT(IY),6)=dspot(38)
C
                      ELSE
C       DON'T ADD ANYTHING
                      END IF
                  END IF
              END DO
! 11     FORMAT(1X,F8.3,1X,F8.3,1X,F8.3,1X,I6,1X,F8.3)
! 10     FORMAT(1X,F8.3,1X,F8.3,1X,I6,1X,G15.7,1X,G15.7,
!     1  1X,G15.7)
C       WRITE OUT THE IRAD.DAT FILE HEADER, THEN THE DATA
C       FOR THE CURRENT FOB
              KK=KK+1
              WRITE(UNIT=105,REC=KK)
     1        SYSTEM1(14)*TT1,SYSTEM1(16)*TT2,RTOT
              KK=KK+1
              WRITE(UNIT=105,REC=KK)
     1        INTTOT,DSP38,AREA_FILL
              DO J=1,NX
                  DO I=1,NY
                      KK=KK+1
                      WRITE(UNIT=105,REC=KK)IGRID(I,J,1),
     1                IGRID(I,J,2),IGRID(I,J,6)
                  END DO
              END DO
              FN=1
              WRITE(UNIT=105,REC=1) DBLE(KK),DBLE(IARRAY_DIM),
     1        DBLE(FN)
              CALL CLOSE_FILE(105,1)
              WRITE(OUTLYNE,*) 'TRACE COMPLETE'
              CALL SHOWIT(1)
              RETURN
          ELSE
              KK=1
C       CASE OF MULTIFOBS
C
C       CALCULATE X AND Y STEPS

              FXSTEP=2.0D0*DABS(FW2)/(DABS(DBLE(INT(FW4)))-1.0D0)
              FYSTEP=2.0D0*DABS(FW1)/(DABS(DBLE(INT(FW4)))-1.0D0)
              FSW2=-DABS(FW2)
              FSW1=-DABS(FW1)
              TFW2=FSW2
              TFW1=FSW1
              DO K=1,FN
                  DO L=1,FN
                      FW5=0.0D0
                      DF5=1
                      W1=TFW1
                      W2=TFW2
                      TT1=TFW1
                      TT2=TFW2
                      W3=0.0D0
                      SQ=0
                      WQ='        '
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=0
                      FW4=INT(SYSTEM1(11))
                      FW4=FW3
                      W4=FW3
                      WC='FOB'
                      REFEXT=.FALSE.
                      CALL FFOB
                      IF(.NOT.REFEXT) THEN
C       WRITE(OUTLYNE,*) 'CHIEF RAY COULD NOT BE TRACED'
C       CALL SHOWIT(1)
C       WRITE(OUTLYNE,*)
C    1  'NO SPOT DIAGRAM WAS TRACED FOR FOB# ',FOBNUMBER
C       CALL SHOWIT(1)
                      ELSE
                          FOBNUMBER=FOBNUMBER+1
                      END IF

C
                      TFW2=TFW2+FXSTEP
C
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='SPD ACC,,,3'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      IF(SPDEXT) THEN
                          FAREA=0.0D0
                          AREA_FILL=0.0D0
                      END IF
C
C       GREATE THE IGRID ARRAY
                      IGRID(1:IARRAY_DIM,1:IARRAY_DIM,2:10)=0.0D0
                      IF(SPDEXT) THEN
C       SPOT EXISTS
                          DO I=1,ITOT-1
C     LOAD DSPOTT(I,*) INTO DSPOT(*)
                              ID=I
                              CALL SPOTIT(4)
C       LOAD THE IGRID ARRAY
                              RRXI=DSPOT(45)
                              RRYI=DSPOT(46)
                              IX=NINT(((RRXI-XCORRI)/XDEL))+1
                              IY=NINT(((RRYI-YCORRI)/YDEL))+1
                              IF(IX.LT.1.OR.IX.GT.NX.OR.IY.LT.1.OR.IY.GT.NY) THEN
C       RAY WAS OUTSIDE THE INTENSITY ARRAY GRID AND WILL NOT BE FURTHER PROCESSED
                              ELSE
C       RAY WAS INSIDE THE ARRAY GRID
                                  IF(IGRID(INT(IX),INT(IY),1).NE.0.0D0) THEN
C
C       ADD IRRADIANCE OF RAY
                                      IGRID(INT(IX),INT(IY),2)=IGRID(INT(IX),INT(IY),2)+DSPOT(12)
C
C       ADD IRRADIANCE OF RAY TIMES THE ANGLE OF INCIDENCE AT NEWIMG SURFACE
                                      IGRID(INT(IX),INT(IY),3)=IGRID(INT(IX),INT(IY),2)+(DSPOT(12)
     &                                *DSPOT(39))
C
C       INCREMENT THE NUMBER OF RAYS IN THE IX,IY BOX
                                      IGRID(INT(IX),INT(IY),4)=IGRID(INT(IX),INT(IY),3)+1.0D0
C
C       TOTAL SUM OF COS(I)S IN BOX ON SPOT SURFACE
                                      IGRID(INT(IX),INT(IY),5)=IGRID(INT(IX),INT(IY),4)+DSPOT(51)
C
C       AVERAGE OF COS(I)S IN BOX ON SPOT SURFACE
                                      IGRID(INT(IX),INT(IY),6)=dspot(38)
C
                                  ELSE
C       DON'T ADD ANYTHING
                                  END IF
                              END IF
                          END DO
                      ELSE
C       NO SPOT, IGRID IS ALL ZERO
                      END IF
C       WRITE OUT THE IRAD.DAT FILE HEADER, THEN THE DATA
C       FOR THE CURRENT FOB
                      KK=KK+1
                      WRITE(UNIT=105,REC=KK)
     1                SYSTEM1(14)*TT1,SYSTEM1(16)*TT2,RTOT
                      KK=KK+1
                      WRITE(UNIT=105,REC=KK)
     1                INTTOT,DSP38,AREA_FILL
                      WRITE(OUTLYNE,*)
                      DO J=1,NX
                          DO I=1,NY
                              KK=KK+1
                              WRITE(UNIT=105,REC=KK)IGRID(I,J,1),
     1                        IGRID(I,J,2),IGRID(I,J,6)
                          END DO
                      END DO
C
                  END DO
                  TFW1=TFW1+FYSTEP
                  TFW2=FSW2
              END DO
          END IF
          WRITE(UNIT=105,REC=1) DBLE(KK),DBLE(IARRAY_DIM),DBLE(FN)
          CALL CLOSE_FILE(105,1)
          WRITE(OUTLYNE,*) 'TRACE COMPLETE'
          CALL SHOWIT(1)
          RETURN
      END
