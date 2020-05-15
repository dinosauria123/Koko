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

C SUB CMDER.FOR
          SUBROUTINE CMDER

          USE opsys
          USE GLOBALS
C
          IMPLICIT NONE
C
          CHARACTER DDATE*10,TTIME*8,FILNM*10,NM*8,TTTIM*8,DDDAT*10
     1    ,OWC*8,OWQ*8,HNAM*8,AI4*4
C
          LOGICAL LPASS1,LPASS2,EXTDMTF1,EXTDMTF2,PERF

          LOGICAL ITERROR
C
          REAL*8 X1,Y1,Z1,XL1,XM1,XN1,YL1,YM1,YN1,ZL1,ZM1,ZN1
          REAL*8 X2,Y2,Z2,XL2,XM2,XN2,YL2,YM2,YN2,ZL2,ZM2,ZN2
          REAL*8 X3,Y3,Z3,XL3,XM3,XN3,YL3,YM3,YN3,ZL3,ZM3,ZN3
          REAL*8 ALPHA,BETA,GAMMA
C
          COMMON/DMTFEXT/EXTDMTF1,EXTDMTF2,PERF
C
          LOGICAL EXTGMTF1,EXTGMTF2,RTGERROR
C
          COMMON/GMTFEXT/EXTGMTF1,EXTGMTF2
C
          CHARACTER LUNI*3
C
          REAL*8 XPFOB,YPFOB
          COMMON/PFOB/XPFOB,YPFOB,LUNI
C
          REAL*8 FLDCODE(1:2,0:10)
          CHARACTER FLDUNIT(0:10)*3
          COMMON/FLDLAB/FLDCODE,FLDUNIT
C
          COMMON/MANH/HNAM
C
          LOGICAL FIELDER,RAYER,IS
     1    ,LBT,EXIS51,OPEN51,LERROR,ERRORPSF,OLDLDIF2,OLDLDIF
     2    ,REFERR,PDOK
C
          LOGICAL MULTIOTF
          COMMON/OTFMULTI/MULTIOTF
C
          REAL*8 OS62,OS63

!      REAL OLDWAITER
C     COMMON/WAITEROLD/OLDWAITER
C
          INTEGER II,I,ITNUM,ITCT,OLDSTOP,OLDSTOP2,KKKEY,CCODE,ERROR
     1    ,OREFLOC,IIG,OCOLDEF,OCOLBAC,IU,CACOCHVIE
     2    ,MMM,NUMINLIST,N,ALLOERR

!      CHARACTER(LEN=1024) COMMAND,MESSAGE

          COMMON/CCPAC/OCOLBAC,OCOLDEF
          COMMON/GII/IIG
C
          COMMON/CODECC/CCODE,KKKEY
C
          COMMON/STOPPER_2/OLDSTOP,OLDSTOP2
C
          COMMON/STRNGR/DDDAT,TTTIM,NM,FILNM
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'dathgr.inc'
          REAL*8 TESTA,TESTB,TESTC,TESTD,TESTE
C
C     REMEMBER ORIGINAL REFLOC SETTING FOR MULTIPLE FOV DIFFRACTION
C     CALCULATIONS

          OREFLOC=REFLOC
          LASTCOMWRD=WC

          COS_A_ANG=1.0D0
          COS_B_ANG=1.0D0

C
C ******************************************************************************
C
C       CMD LEVEL COMMANDS F1=1
C
C*******************************************************************************
C
          IF(F1.EQ.1) THEN
              IF(WC.EQ.'?') THEN

                  CALL QUERRYY
                  RETURN
              END IF

              IF(WC.EQ.''.AND.F50.EQ.1) THEN
                  CALL BLANK0
                  F50=0
                  RETURN
              END IF

              IF(WC.EQ.' '.AND.F50.NE.1) THEN
                  RETURN
              END IF
C       CHECK FOR SPECT SUB-LEVEL
C
              IF(F17.EQ.0.AND.WC.EQ.'ITF'.OR.F17.EQ.0.AND.WC.EQ.'ILF'
     1        .OR.F17.EQ.0.AND.WC.EQ.'IMF'.OR.F17.EQ.0.AND.WC.EQ.'PROCEED'
     2        .OR.F17.EQ.0.AND.WC.EQ.'IPF') THEN
C       ILF
                  IF(WC.EQ.'ILF') THEN
                      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
C
                          OUTLYNE='"ILF" TAKES NO EXPLICIT INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF

                      F48=1
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      OUTLYNE='IF THE NEXT COMMAND IS "PROCEED"
     1YOUR LENS LIBRARY'
                      CALL SHOWIT(1)
                      OUTLYNE='WILL BE ERASED'
                      CALL SHOWIT(1)
                      RETURN
                  END IF

                  IF(WC.EQ.'PROCEED'.AND.F48.EQ.1) THEN
                      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                          OUTLYNE='"PROCEED" TAKES NO EXPLICIT INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          F48=0
                          CALL MACFAL
                          RETURN
                      END IF

                      CALL ILF
                      F48=0
                      RETURN
                  END IF

                  IF(WC.NE.'PROCEED'.AND.F48.EQ.1) THEN
                      OUTLYNE='ILF CANCELLED'
                      CALL SHOWIT(1)
                      F48=0
                      RETURN
                  END IF
C       ITF
                  IF(WC.EQ.'ITF') THEN
                      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                          OUTLYNE='"ITF" TAKES NO EXPLICIT INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF

                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      OUTLYNE='IF THE NEXT COMMAND IS "PROCEED" YOUR TRANSMISSION'
                      CALL SHOWIT(1)
                      OUTLYNE='LIBRARY WILL BE ERASED'
                      CALL SHOWIT(1)
                      F45=1
                      RETURN
                  END IF

                  IF(WC.EQ.'PROCEED'.AND.F45.EQ.1) THEN
                      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                          OUTLYNE='"PROCEED" TAKES NO EXPLICIT INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          F45=0
                          CALL MACFAL
                          RETURN
                      END IF

                      CALL ITF
                      F45=0
                      RETURN
                  END IF

                  IF(WC.NE.'PROCEED'.AND.F45.EQ.1) THEN
                      OUTLYNE='ITF CANCELLED'
                      CALL SHOWIT(1)
                      F45=0
                      RETURN
                  END IF
C       IPF
                  IF(WC.EQ.'IPF') THEN
                      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                          OUTLYNE='"IPF" TAKES NO EXPLICIT INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF

                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      OUTLYNE='IF THE NEXT COMMAND IS "PROCEED" YOUR PLOT'
                      CALL SHOWIT(1)
                      OUTLYNE='LIBRARY WILL BE ERASED'
                      CALL SHOWIT(1)
                      F33=1
                      RETURN
                  END IF

                  IF(WC.EQ.'PROCEED'.AND.F33.EQ.1) THEN
                      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                          OUTLYNE='"PROCEED" TAKES NO EXPLICIT INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          F33=0
                          CALL MACFAL
                          RETURN
                      END IF

                      CALL IPF
                      F33=0
                      RETURN
                  END IF

                  IF(WC.NE.'PROCEED'.AND.F33.EQ.1) THEN
                      OUTLYNE='IPF CANCELLED'
                      CALL SHOWIT(1)
                      F33=0
                      RETURN
                  END IF
C       IMF
                  IF(WC.EQ.'IMF') THEN
                      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                          OUTLYNE='"IMF" TAKES NO EXPLICIT INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF

                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      OUTLYNE='IF THE NEXT COMMAND IS "PROCEED" YOUR MACRO LIBRARY
     1'
                      CALL SHOWIT(1)
                      OUTLYNE='WILL BE ERASED'
                      CALL SHOWIT(1)
                      F49=1
                      RETURN
                  END IF

                  IF(WC.EQ.'PROCEED'.AND.F49.EQ.1) THEN
                      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                          OUTLYNE='"PROCEED" TAKES NO EXPLICIT INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          F49=0
                          CALL MACFAL
                          RETURN
                      END IF

                      CALL IMF
                      F49=0
                      RETURN
                  END IF

                  IF(WC.NE.'PROCEED'.AND.F49.EQ.1) THEN
                      OUTLYNE='IMF CANCELLED'
                      CALL SHOWIT(1)
                      F49=0
                      RETURN
                  END IF

                  IF(WC.EQ.'PROCEED'.AND.F45.EQ.0) THEN
                      OUTLYNE='NO ITF ACTION TAKEN'
                      CALL SHOWIT(1)
                      RETURN
                  END IF

                  IF(WC.EQ.'PROCEED'.AND.F33.EQ.0) THEN
                      OUTLYNE='NO IPF ACTION TAKEN'
                      CALL SHOWIT(1)
                      RETURN
                  END IF

                  IF(WC.EQ.'PROCEED'.AND.F48.EQ.0) THEN
                      OUTLYNE='NO ILF ACTION TAKEN'
                      CALL SHOWIT(1)
                      RETURN
                  END IF

                  IF(WC.EQ.'PROCEED'.AND.F49.EQ.0) THEN
                      OUTLYNE='NO IMF ACTION TAKEN'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
              END IF

              IF(F17.EQ.1.AND.WC.EQ.'ITF'.OR.F17.EQ.1.AND.WC.EQ.'ILF'
     1          .OR.F17.EQ.1.AND.WC.EQ.'IMF'.OR.F17.EQ.1.AND.WC.EQ.
     2          'PROCEED'.OR.F17.EQ.1.AND.WC.EQ.'IPF') THEN
C       WE ARE AT SPECT
                  OUTLYNE=
     1            '"ILF", "IMF", "ITF" AND "IPF" NOT VALID AT "SPECT" LEVEL'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF

              IF(F17.EQ.1) THEN
C               WE ARE IN THE SPECT PROGRAM LEVEL
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THE SPECT LEVEL.
C               WE ARE IN SPECT MODE. THE SUBROUTINE
C               SPECIN CONTAINS RULES FOR RESPONDING TO SPECIAL
C       SPECT SPECIFIC INPUT. MANY CMD LEVEL COMMANDS ARE ALSO ALLOWED.
C       DURING THIS MODE SPECIFIC SPECT COMMANDS ARE PERFORMED
C       BY SPECIN.FOR
C
                  IS=.FALSE.

                  IF(WC.EQ.'START')       IS=.TRUE.
                  IF(WC.EQ.'WAVLN')       IS=.TRUE.
                  IF(WC.EQ.'INT')         IS=.TRUE.
                  IF(WC.EQ.'TABLE')       IS=.TRUE.
C           IF(WC.EQ.'LOADPHOT')    IS=.TRUE.
C           IF(WC.EQ.'LOADSCOT')    IS=.TRUE.
                  IF(WC.EQ.'DIRECT')      IS=.TRUE.
                  IF(WC.EQ.'INSERT')      IS=.TRUE.
                  IF(WC.EQ.'DROP')        IS=.TRUE.
                  IF(WC.EQ.'DELETE')      IS=.TRUE.
                  IF(WC.EQ.'GETFILE')     IS=.TRUE.
                  IF(WC.EQ.'BLACKBDY')    IS=.TRUE.
                  IF(WC.EQ.'PHOTOPIC')    IS=.TRUE.
                  IF(WC.EQ.'SCOTOPIC')    IS=.TRUE.
                  IF(WC.EQ.'PUT')         IS=.TRUE.
                  IF(WC.EQ.'LIST')        IS=.TRUE.
                  IF(WC.EQ.'RENAME')      IS=.TRUE.
                  IF(WC.EQ.'SPRINT')      IS=.TRUE.
                  IF(WC.EQ.'PUNCH')       IS=.TRUE.
                  IF(WC.EQ.'INTER')       IS=.TRUE.
                  IF(WC.EQ.'NARCIN')      IS=.FALSE.
                  IF(WC.EQ.'FLNAME')      IS=.FALSE.
                  IF(WC.EQ.'FILE')        IS=.TRUE.
                  IF(WC.EQ.'ENDTABLE')    IS=.TRUE.
                  IF(WC.EQ.'NARC')        IS=.FALSE.
                  IF(WC.EQ.'EOS')         IS=.TRUE.
                  IF(WC.EQ.'DATA')        IS=.TRUE.
                  IF(WC.EQ.'CUME')        IS=.TRUE.
                  IF(WC.EQ.'WFACTOR')     IS=.TRUE.
                  IF(WC.EQ.'WORK')        IS=.TRUE.
                  IF(WC.EQ.'PTABLE')      IS=.TRUE.
                  IF(WC.EQ.'DIR')         IS=.TRUE.
                  IF(WC.EQ.'C')           IS=.TRUE.
                  IF(WC.EQ.'C')           IS=.TRUE.
                  IF(WC.EQ.'M')           IS=.TRUE.
                  IF(WC.EQ.'NAME')        IS=.TRUE.
                  IF(WC.EQ.'PLOTR')       IS=.TRUE.
                  IF(WC.EQ.'PLOTT')       IS=.TRUE.
                  IF(IS) THEN
                      CALL SPECIN
                      RETURN
C       PROCEED AS A CHECK FOR INVALIDS WAS ALREADY MADE.
                  END IF
C       NOT AT SPECT LEVEL, PROCEED
              END IF
C************************************************************
              PDOK=.FALSE.
              IF(WC.EQ.'PARTDRAW') PDOK=.TRUE.
              IF(WC.EQ.'OD      ') PDOK=.TRUE.
              IF(WC.EQ.'DIATOL  ') PDOK=.TRUE.
              IF(WC.EQ.'RADTOL  ') PDOK=.TRUE.
              IF(WC.EQ.'RADTLF  ') PDOK=.TRUE.
              IF(WC.EQ.'FRNG    ') PDOK=.TRUE.
              IF(WC.EQ.'THITOL  ') PDOK=.TRUE.
              IF(WC.EQ.'CLERAP  ') PDOK=.TRUE.
              IF(WC.EQ.'SURFQUAL') PDOK=.TRUE.
              IF(WC.EQ.'FNGDIA  ') PDOK=.TRUE.
              IF(WC.EQ.'CENTIR  ') PDOK=.TRUE.
              IF(WC.EQ.'BRKEDG  ') PDOK=.TRUE.
              IF(WC.EQ.'SURFCOAT') PDOK=.TRUE.
              IF(WC.EQ.'SAGTOL  ') PDOK=.TRUE.
              IF(WC.EQ.'PRPNTL  ') PDOK=.TRUE.
              IF(WC.EQ.'TITLE   ') PDOK=.TRUE.
              IF(WC.EQ.'DWGNO   ') PDOK=.TRUE.
              IF(WC.EQ.'SURFMATL') PDOK=.TRUE.
              IF(WC.EQ.'GLSCD   ') PDOK=.TRUE.
              IF(WC.EQ.'CONAME  ') PDOK=.TRUE.
              IF(WC.EQ.'WAVEL   ') PDOK=.TRUE.
              IF(WC.EQ.'PARTGO  ') PDOK=.TRUE.
              IF(WC.EQ.'PARTQUIT') PDOK=.TRUE.
              IF(PDOK) THEN
C       PART DRAWINGS
                  CALL PART_DRAW
                  PDOK=.FALSE.
                  RETURN
              END IF

              IF(WC.EQ.'ABSORB') THEN
                  CALL ABSORB
                  RETURN
              END IF

              IF(WC.EQ.'LENO')THEN
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='CFG 1'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  CALL LENOUT
                  RETURN
              END IF

              IF(WC.EQ.'LENOCSV')THEN
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='CFG 1'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  CALL LENOCSV
                  RETURN
              END IF

              IF(WC.EQ.'CV2PRG')THEN
                  CALL CV2PRG
                  RETURN
              END IF

              IF(WC.EQ.'ZMX2PRG')THEN
                  CALL ZMX2PRG
                  RETURN
              END IF

              IF(WC.EQ.'ECHO')THEN
                  CALL ECHO
                  RETURN
              END IF

              IF(WC.EQ.'SPSRF'.AND.WQ.NE.'ON'.AND.
     1        WC.EQ.'SPSRF'.AND.WQ.NE.'OFF')THEN
                  IF(RAYCLEAR) THEN
                      RAYEXT=.FALSE.
                      POLEXT=.FALSE.
                      REFEXT=.FALSE.
                      SPDEXT=.FALSE.
                      GSPDEXT=.FALSE.
                      CPFNEXT=.FALSE.
                      CALL DELPSF
                  END IF

                  II=INT(SYSTEM1(20))
                  ALENS(88,1:II)=0.0D0
                  LPASS1=.FALSE.
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL GRIDS(1,0,LPASS1)
c       CALL DEROFF
c       CALL AUTOFF
                  CALL SPSIN
                  RETURN
              END IF

              IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'ON'.OR.
     1          WC.EQ.'SPSRF'.AND.WQ.EQ.'OFF')THEN
                  IF(RAYCLEAR) THEN
                      RAYEXT=.FALSE.
                      POLEXT=.FALSE.
                      REFEXT=.FALSE.
                      SPDEXT=.FALSE.
                      GSPDEXT=.FALSE.
                      CPFNEXT=.FALSE.
                      CALL DELPSF
                  END IF

                  II=INT(SYSTEM1(20))
                  ALENS(88,1:II)=0.0D0
                  LPASS1=.FALSE.
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL GRIDS(1,0,LPASS1)
                  CALL SPONOF
                  RETURN
              END IF
              IF(WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'SPSRF'.OR.
     1        WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'SP'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'SPSRF'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'SP') THEN

                  IF(RAYCLEAR) THEN
                      RAYEXT=.FALSE.
                      POLEXT=.FALSE.
                      REFEXT=.FALSE.
                      SPDEXT=.FALSE.
                      GSPDEXT=.FALSE.
                      CPFNEXT=.FALSE.
                      CALL DELPSF
                  END IF

                  II=INT(SYSTEM1(20))
                  ALENS(88,1:II)=0.0D0
                  LPASS1=.FALSE.
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL GRIDS(1,0,LPASS1)
                  CALL SPSUP
                  RETURN
              END IF

              IF(WC.EQ.'SPFIT')THEN
                  CALL SPFIT
                  RETURN
              END IF

              IF(WC.EQ.'CONFIGS'.OR.WC.EQ.'CONFIG') THEN
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL DEROFF
                  CALL AUTOFF
                  CALL CFGIN
                  RETURN
              END IF

              IF(WC.EQ.'SPECT')THEN
                  CALL SPECTR
                  RETURN
              END IF

              IF(WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'CONFIGS'.OR.
     1        WQ.EQ.'UPDATE'.AND.WQ.EQ.'CONFIG'.OR.
     1        WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'CF'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'CONFIGS'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'CONFIG'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'CF') THEN
C       DO A FORCED RETURN TO CFG1
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL FRCCF1(1)
                  CALL CFGUP
                  RETURN
              END IF

              IF(WC.EQ.'CFG') THEN
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL CFGCHG
                  RETURN
              END IF

              IF(WC.EQ.'CF') THEN
                  CALL CFGPRT
                  RETURN
              END IF

              IF(WC.EQ.'DELCFG') THEN
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL DELCFG
                  RETURN
              END IF

              IF(WC.EQ.'REMOVE') THEN
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL REMOVE
                  RETURN
              END IF

              IF(WC.EQ.'DEZOOM') THEN
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL DEROFF
                  CALL AUTOFF
                  CALL DEZOOM
                  RETURN
              END IF

C       CALL TO LSTAT( LENS LIBRARY STATUS)
              IF(WC.EQ.'LSTAT')  THEN
                  IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
                      OUTLYNE=
     1                '"LSTAT" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF

                  CALL LSTAT1
                  RETURN
              END IF

C               LENS LIBRARY COMMANDS
              IF(WC.EQ.'LIB') THEN
                  OPTMES=.FALSE.
                  CALL LLIB
                  OPTMES=.TRUE.
                  RETURN
              END IF
C
C       CALL TO PSTAT( PLOT LIBRARY STATUS)
              IF(WC.EQ.'PSTAT')  THEN
                  IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
                      OUTLYNE=
     1                '"PSTAT" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL PLSTAT
                  RETURN
              END IF
C                PLOT LIBRARY COMMANDS
              IF(WC.EQ.'PLIB') THEN
                  IF(NEUTFILE) THEN
                      CALL PLLIB
                  ELSE
                      OUTLYNE='AUTOMATIC STORAGE OF GRAPHICS DATA IS "OFF"'
                      CALL SHOWIT(1)
                      OUTLYNE='NO PLOT LIBRARY COMMANDS MAY BE ISSUED'
                      CALL SHOWIT(1)
                      OUTLYNE='USE "NEUTFILE (YES or ON or NO or OFF)"'
                      CALL SHOWIT(1)
                      OUTLYNE='TO CONTROL ACCESS TO THE PLOT LIBRARY FEATURE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                  END IF
                  RETURN
              END IF
C
              IF(WC.EQ.'PRSPR') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PPRSPR
                  RETURN
              END IF
              IF(WC.EQ.'GCONVERT') THEN
                  CALL GCONVERT
                  RETURN
              END IF
              IF(WC.EQ.'LI'.OR.WC.EQ.'LIC') THEN
                  CALL SLI
                  RETURN
              END IF
              IF(WC.EQ.'INI') THEN
                  CALL SINI
                  RETURN
              END IF
              IF(WC.EQ.'LTYPE') THEN
                  CALL SLTYPE
                  RETURN
              END IF
              IF(WC.EQ.'WV') THEN
                  CALL SWV
                  RETURN
              END IF
              IF(WC.EQ.'WV2') THEN
                  CALL SWV2
                  RETURN
              END IF
              IF(WC.EQ.'UNITS') THEN
                  CALL SUNITS
                  RETURN
              END IF
              IF(WC.EQ.'CW'.OR.WC.EQ.'PCW'.OR.
     1        WC.EQ.'SCW') THEN
                  CALL SCW
                  RETURN
              END IF
              IF(WC.EQ.'SAY'.OR.WC.EQ.'SAX') THEN
                  CALL SSA
                  RETURN
              END IF
              IF(WC.EQ.'WRY'.OR.WC.EQ.'WRX') THEN
                  CALL SWR
                  RETURN
              END IF
              IF(WC.EQ.'NAOY'.OR.WC.EQ.'NAOX') THEN
                  CALL SNAO
                  RETURN
              END IF
              IF(WC.EQ.'FNOY'.OR.WC.EQ.'FNOX') THEN
                  CALL SFNO
                  RETURN
              END IF
              IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
                  CALL SSC
                  RETURN
              END IF
              IF(WC.EQ.'PXIM'.OR.WC.EQ.'PYIM') THEN
                  CALL PXYIM
                  RETURN
              END IF
              IF(WC.EQ.'RXIM'.OR.WC.EQ.'RYIM') THEN
                  CALL RXYIM
                  RETURN
              END IF
              IF(WC.EQ.'BDX'.OR.WC.EQ.'BDY') THEN
                  CALL SBD
                  RETURN
              END IF
              IF(WC.EQ.'TESTRD'.OR.WC.EQ.'TESTCYL') THEN
                  CALL TEST_PLATE_IT
                  RETURN
              END IF
              IF(WC.EQ.'TPLATE') THEN
                  CALL TSTPLATE
                  RETURN
              END IF

              IF(WC.EQ.'CTG'.OR.WC.EQ.'RTG') THEN
                  testa=3.0d0
                  testb=0.0d0
                  testc=1.0d-200
                  testd=1.0d200
                  TESTE=-4.0D0

                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  LBT=.FALSE.
                  RTGERROR=.FALSE.
                  CALL SRTG(LBT,RTGERROR)
                  OWC=WC
                  WC='ARRAY'
                  IF(.NOT.RTGERROR) CALL SARRAY(.FALSE.)
                  WC=OWC
                  RETURN
              END IF

              IF(WC.EQ.'CTGLBL'.OR.WC.EQ.'RTGLBL') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  LBT=.TRUE.
                  CALL SRTG(LBT,RTGERROR)
                  OWC=WC
                  WC='ARRAY'
                  IF(.NOT.RTGERROR) CALL SARRAY(.FALSE.)
                  WC=OWC
                  RETURN
                  LBT=.FALSE.
                  RETURN
              END IF

              IF(WC.EQ.'LENADD') THEN
                  CALL DEROFF
                  CALL AUTOFF
                  CALL LENADD
                  RETURN
              END IF
              IF(WC.EQ.'ASTOP') THEN
                  CALL SASTOP
                  RETURN
              END IF
              IF(WC.EQ.'ZERNREPT') THEN
                  CALL ZERNREPT
                  RETURN
              END IF
              IF(WC.EQ.'MODE') THEN
                  CALL SMODE
                  RETURN
              END IF
              IF(WC.EQ.'GLASSP') THEN
                  IF(WQ.EQ.'GLA'.AND.SST.EQ.0.OR.
     1            WQ.EQ.'GLCAT'.AND.SST.EQ.0) THEN
                      WQ='SCHOTT'
                      CALL GLASSP
                      WQ='SCH2000'
                      CALL GLASSP
                      WQ='OHARA'
                      CALL GLASSP
                      WQ='HOYA'
                      CALL GLASSP
                      WQ='HIKARI'
                      CALL GLASSP
                      WQ='CORNIN'
                      CALL GLASSP
                      WQ='CHANCE'
                      CALL GLASSP
                      WQ='MATL'
                      CALL GLASSP
                      WQ='RUSSIAN'
                      CALL GLASSP
                      WQ='RADHARD'
                      CALL GLASSP
                      WQ='GLA'
                  ELSE
                      IF(WS(1:1).EQ.':') WS(1:80)=WS(2:80)
                      CALL GLASSP
                  END IF
                  RETURN
              END IF
              IF(WC.EQ.'FINDGLAS') THEN
                  CALL FNDGLS
                  RETURN
              END IF
              IF(WC.EQ.'COLORSET') THEN
                  CALL COLORS
                  RETURN
              END IF
              IF(WC.EQ.'DEG'.OR.WC.EQ.'RAD'.OR.WC.EQ.'TANGENT'
     1        .OR.WC.EQ.'ANGMODE') THEN
                  CALL DEGRAD
                  RETURN
              END IF
              IF(WC.EQ.'ASPH') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL SASPH
                  WC='ASPH2'
                  CALL SASPH
                  WC='ASPH'
                  RETURN
              END IF
              IF(WC.EQ.'ASPH2') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL SASPH
                  RETURN
              END IF
              IF(WC.EQ.'SURTYPE') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL SURFTYPE
                  RETURN
              END IF
              IF(WC.EQ.'FOOTBLOK') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL BLKOUT
                  RETURN
              END IF
              IF(WC.EQ.'DUMOUT') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL DUMOUT
                  RETURN
              END IF
              IF(WC.EQ.'GREYSPOT') THEN
                  CALL GREYSPOT
                  RETURN
              END IF
              IF(WC.EQ.'TASPH') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL STASPH
                  RETURN
              END IF
              IF(WC.EQ.'REF') THEN
                  CALL SREF
                  RETURN
              END IF
              IF(WC.EQ.'REFS') THEN
                  WRITE(OUTLYNE,100) INT(SYSTEM1(25))
 100              FORMAT('THE CURRENT REFERENCE SURFACE I SURFACE # ',I3)
                  CALL SHOWIT(0)
                  RETURN
              END IF
              IF(WC.EQ.'CAOB') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL SCAOB
                  RETURN
              END IF
              IF(WC.EQ.'SPIDER') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL SPIDEROUT
                  RETURN
              END IF
              IF(WC.EQ.'ARRAY') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL SARRAY(.TRUE.)
                  RETURN
              END IF
              IF(WC.EQ.'TAD') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL STAD
                  RETURN
              END IF
              IF(WC.EQ.'SPIDER') THEN
C        CALL SPIDER
                  RETURN
              END IF
              IF(WC.EQ.'COATING') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL COATING
                  RETURN
              END IF
              IF(WC.EQ.'PIVOT') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL SPIV
                  RETURN
              END IF
              IF(WC.EQ.'PIVAXIS') THEN
                  CALL PIVAXOUT
                  RETURN
              END IF
              IF(WC.EQ.'DISP') THEN
                  CALL HEXDISP
                  RETURN
              END IF
              IF(WC.EQ.'STILT') THEN
                  CALL HEXSTILT
                  RETURN
              END IF
              IF(WC.EQ.'BTILT') THEN
                  CALL HEXBTILT
                  RETURN
              END IF
              IF(WC.EQ.'ROLL') THEN
                  CALL HEXROLL
                  RETURN
              END IF
              IF(WC.EQ.'FLIP') THEN
                  CALL CVFLIP
                  RETURN
              END IF
              IF(WC.EQ.'RIN'.OR.WC.EQ.'RIN2') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL SRIN
                  RETURN
              END IF
              IF(WC.EQ.'GRT') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL SGRATT
                  RETURN
              END IF
              IF(WC.EQ.'NDEX'.OR.WC.EQ.'NDEX2') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL SNDEX
                  RETURN
              END IF
              IF(WC.EQ.'TORIC'.OR.WC.EQ.'RTORIC'.OR.
     1        WC.EQ.'TR'.OR.WC.EQ.'TC'.OR.WC.EQ.'CTORIC') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PTORIC
                  RETURN
              END IF
              IF(WC.EQ.'SPTWT'.OR.WC.EQ.'SPTWT2') THEN
                  CALL SSPTWT
                  RETURN
              END IF
              IF(WC.EQ.'MAGY'.OR.WC.EQ.'MAGX') THEN
                  CALL SMAG
                  RETURN
              END IF
              IF(WC.EQ.'FNBY'.OR.WC.EQ.'FNBX') THEN
                  CALL SFNB
                  RETURN
              END IF
              IF(WC.EQ.'ERY'.OR.WC.EQ.'ERX') THEN
                  CALL SER
                  RETURN
              END IF
              IF(WC.EQ.'SPC') THEN
                  CALL SSPC
                  RETURN
              END IF
              IF(WC.EQ.'SLV') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRSLV
                  RETURN
              END IF
              IF(WC.EQ.'PIK') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRPIK
                  RETURN
              END IF
              IF(WC.EQ.'PXTX'.OR.WC.EQ.'PXTY'.OR.WC.EQ.'PITX'
     1        .OR.WC.EQ.'PITY'.OR.WC.EQ.'PRTX'.OR.WC.EQ.'PRTY') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PAROUT
                  RETURN
              END IF
              IF(WC.EQ.'PRREF') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRREF
                  RETURN
              END IF
              IF(WC.EQ.'PRDIFFXM'.OR.WC.EQ.'PRDIFFYM'.OR.WC.EQ.'PRDIFFXR'
     1        .OR.WC.EQ.'PRDIFFYR') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRDIFF
                  RETURN
              END IF
              IF(WC.EQ.'INVAR') THEN
                  CALL INVAR
                  RETURN
              END IF
              IF(WC.EQ.'MAB3'.OR.WC.EQ.'XMAB3') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRC
                  CALL MMAB3
                  RETURN
              END IF
              IF(WC.EQ.'MAB5'.OR.WC.EQ.'XMAB5') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRC
                  CALL MMAB5
                  RETURN
              END IF
              IF(WC.EQ.'MABX5'.OR.WC.EQ.'XMABX5') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRC
                  CALL MMABX5
                  RETURN
              END IF
              IF(WC.EQ.'SA357'.OR.WC.EQ.'XSA357') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRC
                  CALL SA357
                  RETURN
              END IF
              IF(WC.EQ.'MABP3'.OR.WC.EQ.'XMABP3') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRC
                  CALL MMABP3
                  RETURN
              END IF
              IF(WC.EQ.'SA357I'.OR.WC.EQ.'XSA357I') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRC
                  CALL A357I
                  RETURN
              END IF
              IF(WC.EQ.'MAB5I'.OR.WC.EQ.'XMAB5I') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRC
                  CALL AB5I
                  RETURN
              END IF
              IF(WC.EQ.'MABX5I'.OR.WC.EQ.'XMABX5I') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRC
                  CALL ABX5I
                  RETURN
              END IF
              IF(WC.EQ.'PCD3'.OR.WC.EQ.'XPCD3'.OR.WC.EQ.
     1        'SCD3'.OR.WC.EQ.'XSCD3') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRD
                  CALL PCD3
                  RETURN
              END IF
              IF(WC.EQ.'PCD5'.OR.WC.EQ.'XPCD5'.OR.WC.EQ.
     1        'SCD5'.OR.WC.EQ.'XSCD5') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRD
                  CALL PCD5
                  RETURN
              END IF
              IF(WC.EQ.'PCDX5'.OR.WC.EQ.'XPCDX5'.OR.WC.EQ.
     1        'SCDX5'.OR.WC.EQ.'XSCDX5') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRD
                  CALL PCDX5
                  RETURN
              END IF
              IF(WC.EQ.'PCDP3'.OR.WC.EQ.'XPCDP3'.OR.WC.EQ.
     1        'SCDP3'.OR.WC.EQ.'XSCDP3') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRD
                  CALL PCDP3
                  RETURN
              END IF
              IF(WC.EQ.'PCDSA'.OR.WC.EQ.'XPCDSA'.OR.WC.EQ.
     1        'SCDSA'.OR.WC.EQ.'XSCDSA') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRD
                  CALL PCDSA
                  RETURN
              END IF
              IF(WC.EQ.'FCHY'.OR.WC.EQ.'FCHX') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRTRB
                  CALL FCH
                  RETURN
              END IF
              IF(WC.EQ.'CHRSHIFT') THEN
                  CALL CHRSHIFT
                  RETURN
              END IF
              IF(WC.EQ.'FIRD') THEN
                  CALL FIRD
                  RETURN
              END IF
              IF(WC.EQ.'OBJLEV') THEN
                  CALL OBJLEV
                  RETURN
              END IF
              IF(WC.EQ.'LOADMAC') THEN
                  CALL MACARRAY_LOAD(NUMINLIST)
                  RETURN
              END IF
              IF(WC.EQ.'OCDY'.OR.WC.EQ.'OCDX') THEN
                  CALL OCD
                  RETURN
              END IF
              IF(WC.EQ.'SC'.OR.WC.EQ.'WSC') THEN
                  CALL SCALLE
                  RETURN
              END IF

              IF(WC.EQ.'C'.OR.WC.EQ.'M') THEN
                  CALL MESCOM
                  RETURN
              END IF

              IF(WC.EQ.'FIGURE') THEN
                  CALL FIGURE
                  RETURN
              END IF

              IF(WC.EQ.'EXI'.OR.WC.EQ.'EXIT') THEN
                  CALL EXITT(0)
                  RETURN
              END IF

              IF(WC.EQ.'X1'.OR.WC.EQ.'X2'.OR.WC.EQ.'Y1'.OR.
     1        WC.EQ.'Y2'.OR.WC.EQ.'Z1'.OR.WC.EQ.'Z2') THEN
                  CALL THREEDEESET
                  RETURN
              END IF
              IF(WC.EQ.'X1Y1='.OR.WC.EQ.'X2Y2='.OR.
     1        WC.EQ.'X3Y3='.OR.WC.EQ.'X4Y4='.OR.
     2        WC.EQ.'INTERP') THEN
                  CALL INTRPP
                  RETURN
              END IF
              IF(WC.EQ.'TABLE'.OR.WC.EQ.'COLHD'.OR.
     1        WC.EQ.'COLHD2'.OR.WC.EQ.'ROWHD'.OR.
     2        WC.EQ.'ROWHD2') THEN
                  CALL TABLE
                  RETURN
              END IF
              IF(WC.EQ.'NEWCMD'.OR.WC.EQ.'STWORD'.OR.WC.EQ.
     1        'CWORD'.OR.WC.EQ.'QWORD'.OR.WC.EQ.'N1WORD'.OR.WC.EQ.
     2        'N2WORD'.OR.WC.EQ.'N3WORD'.OR.WC.EQ.'N4WORD'.OR.WC
     3        .EQ.'N5WORD') THEN
                  CALL CWRITE
                  RETURN
              END IF
              IF(WC.EQ.'INCR') THEN
                  CALL INCR
                  RETURN
              END IF
              IF(WC.EQ.'ATAN2') THEN
                  CALL ATANN2
                  RETURN
              END IF
              IF(WC.EQ.'MACRO') THEN
                  HNAM=WQ
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='MDEL '//TRIM(WQ)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  IF(SST.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE=
     1                '"MACRO" ONLY TAKES QUALIFIER INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  F1=0
                  F2=1
                  F16=0
C               F1 SET BACK TO 1 AND F2 SET BACK TO 0 AT THE
C               END OF SUBROUTINE EEOM
                  CALL NEWMAC
                  RETURN
              END IF
              IF(WC.EQ.'LENS') THEN
                  PFAC=1.0D0
                  IF(RAYCLEAR) THEN
                      RAYEXT=.FALSE.
                      POLEXT=.FALSE.
                      REFEXT=.FALSE.
                      SPDEXT=.FALSE.
                      GSPDEXT=.FALSE.
                      CPFNEXT=.FALSE.
                      CALL DELPSF
                  END IF

                  II=INT(SYSTEM1(20))
                  ALENS(88,1:II)=0.0D0
                  ALENS(109,1:II)=0.0D0
                  LPASS1=.FALSE.
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL GRIDS(1,0,LPASS1)
                  LPASS1=.FALSE.
                  LPASS2=.FALSE.
                  CALL DEFGRIDS(1,0,LPASS1,LPASS2)
                  CALL DEROFF
                  CALL AUTOFF
                  CALL LENNS
                  RETURN
              END IF
              IF(WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'LENS'.OR.
     1        WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'L'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'LENS'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'L') THEN
                  CALL ULENNS
                  RETURN
              END IF

              IF(WC.EQ.'CLS') THEN
                  CALL shell_command('clear')
                  RETURN
              END IF

              IF(WC.EQ.'MRENAME') THEN
                  CALL MRENAME
                  RETURN
              END IF

              IF(WC.EQ.'MCOPY') THEN
                  CALL MCOPY
                  RETURN
              END IF

              IF(WC.EQ.'MREFRESH') THEN
                  CALL MREFRESH
                  RETURN
              END IF

              IF(WC.EQ.'LMEDIT') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='REPLACE'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  CALL MMEDIT
                  RETURN
              END IF

              IF(WC.EQ.'MEDIT') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='REPLACE'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  CALL FSMEDIT
                  RETURN
              END IF

              IF(WC.EQ.'MAC_EDIT') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='REPLACE'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  CALL MAC_EDITOR
                  RETURN
              END IF

              IF(WC.EQ.'REWIND') THEN
                  CALL REWIND
                  RETURN
              END IF
              IF(WC.EQ.'MFLN') THEN
                  CALL MMFLN
                  RETURN
              END IF

              IF(WC.EQ.'MACSAVE') THEN
                  CALL MACSAV
                  RETURN
              END IF

              IF(WC.EQ.'MACREST') THEN
                  CALL MACRES
                  RETURN
              END IF

              IF(WC.EQ.'LIBSAVE') THEN
                  CALL LIBSAV
                  RETURN
              END IF

              IF(WC.EQ.'LIBREST') THEN
                  CALL LIBRES
                  RETURN
              END IF

              IF(WC.EQ.'MDEL') THEN
                  CALL MMDEL
                  RETURN
              END IF

              IF(WC.EQ.'MFL') THEN
                  CALL MMFL
                  RETURN
              END IF

              IF(WC.EQ.'MFLC') THEN
                  CALL MMFLC
                  RETURN
              END IF

              IF(WC.EQ.'MFLC1') THEN
                  CALL MMFLC1
                  RETURN
              END IF

              IF(WC.EQ.'EJECT') THEN
                  CALL EJECT
                  RETURN
              END IF

              IF(WC.EQ.'FLAG') THEN
                  CALL FLAG
                  RETURN
              END IF

              IF(WC.EQ.'OUTPUT'.OR.WC.EQ.'OUT') THEN
C       OUT T IS THE SAME AS OUT TP
                  IF(WQ.EQ.'T       '.AND.SST.EQ.0) WQ='TP      '
              END IF
C
              IF(WC.EQ.'OUTPUT'.OR.WC.EQ.'OUT') THEN
                  IF(WQ(1:8).EQ.'T       '.OR.WQ.EQ.'FILE    ') THEN
                      CALL OUTPUT2
                      RETURN
                  ELSE
                      CALL OUTPUT
                      RETURN
                  END IF
              END IF

              IF(WC.EQ.'INPUT'.OR.WC.EQ.'IN') THEN
                  CALL INPUTT
                  RETURN
              END IF

              IF(WC.EQ.'MSTAT') THEN
                  CALL MSTAT
                  RETURN
              END IF

              IF(WC.EQ.'SAVE') THEN
                  CALL MSAVE
                  RETURN
              END IF

              IF(WC.EQ.'RELOAD') THEN
                  CALL RELOAD
                  RETURN
              END IF

              IF(WC.EQ.'ENT'.OR.
     1        WC.EQ.'ENTI'.OR.
     2        WC.EQ.'ENTC'.OR.WC.EQ.'PULL'.OR.
     3        WC.EQ.'IPULL'.OR.WC.EQ.'CPULL'.OR.
     4        WC.EQ.'RUP'.OR.WC.EQ.'IRUP'.OR.WC
     5        .EQ.'CRUP'.OR.WC.EQ.'RDN'.OR.WC.EQ.
     6        'IRDN'.OR.WC.EQ.'CRDN'.OR.WC.EQ.'LASTX'.OR.WC.EQ.
     7        'X-Y'.OR.
     1        WC.EQ.'LASTIX'.OR.WC.EQ.'IX-IY'.OR.WC.EQ.'RE-IM'.OR.
     1        WC.EQ.'CLX'.OR.WC.EQ.'CLIX'.OR.WC.EQ.
     2        'CLSTK'.OR.WC.EQ.'CLSTKI'.OR.WC.EQ.'CLSTKC'.OR.WC.EQ.
     3        '+'.OR.WC.EQ.'-'.OR.WC.EQ.'*'.OR.WC.EQ.'/'.OR.WC.EQ.
     5        'C+'.OR.WC.EQ.'C-'.OR.WC.EQ.'C*'.OR.WC.EQ.'C/'.OR.WC.EQ.
     6        'I+'.OR.WC.EQ.'I-'.OR.WC.EQ.'I*'.OR.WC.EQ.'I/'.OR.
     7        WC.EQ.'Y**X'.OR.WC.EQ.'CY**CX'.OR.
     8        WC.EQ.'IY**IX') THEN
                  CALL STACK
                  RETURN
              END IF

              IF(WC.EQ.'PRSTK'.OR.WC.EQ.'PRSTKC'.OR.
     1        WC.EQ.'PRSTKI'.OR.WC.EQ.'PRLSTX'.OR.WC.EQ.'PRLSTIX') THEN
                  CALL STACK
                  RETURN
              END IF

              IF(WC.EQ.'R-P'.OR.WC.EQ.'P-R'.OR.WC.EQ.'R-SP'.OR.
     1        WC.EQ.'SP-R'.OR.WC.EQ.'R-CYL'.OR.WC.EQ.'CYL-R'.OR.
     2        WC.EQ.'H-HMS'.OR.WC.EQ.'HMS-H') THEN
                  CALL COORD
                  RETURN
              END IF

              IF(WC.EQ.'IN-MM'.OR.WC.EQ.'IN-CM'.OR.WC.EQ.'IN-M'.OR.
     1        WC.EQ.'MM-IN'.OR.WC.EQ.'CM-IN'.OR.WC.EQ.'M-IN') THEN
                  CALL COORD
                  RETURN
              END IF

              IF(WC.EQ.'CLGREG'.OR.WC.EQ.'CLSTREG'
     1        .OR.WC.EQ.'STADD'.OR.WC.EQ.'STSUB'.OR.
     2        WC.EQ.'STDEV'.OR.WC.EQ.'MEAN'
     3        ) THEN
                  CALL GGPREG
                  RETURN
              END IF

C     THESE ARE 400 GENERAL PURPOSE CHARACTER*80 REGISTERS ADDED
C     IN 3/93
              IF(WC.EQ.'CLASTO'.OR.WC.EQ.'ASTO'.OR.WC.EQ.'ARCL'.OR.
     1        WC.EQ.'AWRITE') THEN
                  CALL GPRGA
                  RETURN
              END IF

              IF(WC.EQ.'MOD') THEN
                  CALL MMOD
                  RETURN
              END IF

              IF(WC.EQ.'INTGR'.OR.WC.EQ.'FRAC'.OR.WC.EQ.'FACT'
     1        .OR.WC.EQ.'CHS'.OR.WC.EQ.'RTD'.OR.WC.EQ.'DTR'
     2        .OR.WC.EQ.'ASIN'.OR.WC.EQ.'ACOS'.OR.WC.EQ.'PLUS'
     3        .OR.WC.EQ.'MINUS'.OR.WC.EQ.'DIV'.OR.WC.EQ.'MPY'
     4        .OR.WC.EQ.'MOVE'.OR.WC.EQ.'ATAN'.OR.WC.EQ.'PI'
     5        .OR.WC.EQ.'RAND'.OR.WC.EQ.'SIN'.OR.WC.EQ.'COS'
     6        .OR.WC.EQ.'TAN'.OR.WC.EQ.'TANH'.OR.WC.EQ.'SINH'
     7        .OR.WC.EQ.'COSH'.OR.WC.EQ.'SQRT'.OR.WC.EQ.'ABS'
     8        .OR.WC.EQ.'EXP'.OR.WC.EQ.'RECIP'.OR.WC.EQ.'POW'
     9        .OR.WC.EQ.'LOG10'.OR.WC.EQ.'LN'.OR.WC.EQ.'STORE'
     1        .OR.WC.EQ.'SGN'.OR.WC.EQ.'CLREG'.OR.WC.EQ.'PRIREG'.OR.WC.EQ.
     2        'WRITE'.OR.WC.EQ.'MAXVAL'.OR.WC.EQ.'MINVAL')
     3        THEN
                  CALL RGMATH
                  RETURN
              END IF

              IF(WC.EQ.'STOREMIN'.OR.WC.EQ.'STOREMAX'.OR.
     1        WC.EQ.'RESETMIN'.OR.WC.EQ.'RESETMAX') THEN
                  CALL MINMAXREG
                  RETURN
              END IF

              IF(WC.EQ.'LFORMAT')  THEN
                  CALL LFORMER
                  RETURN
              END IF

              IF(WC.EQ.'LWRITE')  THEN
                  CALL LWRITE
                  RETURN
              END IF

              IF(WC.EQ.'FORMAT')  THEN
                  CALL FORMER
                  RETURN
              END IF
C     SPECIAL PFAC OPERATION

              IF(WC.EQ.'PFAC')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='PFAC    '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='PFAC    '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL DAYS OPERATION
              IF(WC.EQ.'DAYS')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='DAYS    '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='DAYS    '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL SHORT OPERATION
              IF(WC.EQ.'SHORT')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='SHORT   '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='SHORT   '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL MEDIUM OPERATION
              IF(WC.EQ.'MEDIUM')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='MEDIUM  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='MEDIUM  '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL LONG OPERATION
              IF(WC.EQ.'LONG')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='LONG    '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='LONG    '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL BUYDELAY OPERATION
              IF(WC.EQ.'BUYDELAY')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='BUYDELAY'
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='BUYDELAY'
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL SELDEALY OPERATION
              IF(WC.EQ.'SELDELAY')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='SELDELAY'
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='SELDELAY'
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL DINMUL OPERATION
              IF(WC.EQ.'DINMUL')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='DINMUL  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='DINMUL  '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL LINTOL OPERATION
              IF(WC.EQ.'LINTOL')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='LINTOL  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='LINTOL  '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL ONTOL OPERATION
              IF(WC.EQ.'ONTOL')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='ONTOL   '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='ONTOL   '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL SINGTOL OPERATION
              IF(WC.EQ.'SINGTOL')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='SINGTOL '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='SINGTOL '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL SURTOL OPERATION
              IF(WC.EQ.'SURTOL')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='SURTOL  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='SURTOL  '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL SAGDEL OPERATION
              IF(WC.EQ.'SAGDEL')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='SAGDEL  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='SAGDEL  '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL AIMTOL OPERATION
              IF(WC.EQ.'AIMTOL')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='AIMTOL  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='AIMTOL  '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL CAIMTOL OPERATION
              IF(WC.EQ.'CAIMTOL')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='CAIMTOL '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='CAIMTOL '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL MAXOPT OPERATION
              IF(WC.EQ.'MAXOPT')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='MAXOPT  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='MAXOPT  '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL MAXREG OPERATION
              IF(WC.EQ.'MAXREG')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='MAXREG  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='MAXREG  '
                          SQ=1
                      END IF
                  END IF
              END IF

              IF(WC.EQ.'SERINC')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='SERINC  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='SERINC  '
                          SQ=1
                      END IF
                  END IF
              END IF

              IF(WC.EQ.'SERLIM')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='SERLIM  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='SERLIM  '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL NRAITR OPERATION
              IF(WC.EQ.'NRAITR')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='NRAITR  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='NRAITR  '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL DFTOL OPERATION
              IF(WC.EQ.'DIFTOL')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='DIFTOL  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='DIFTOL  '
                          SQ=1
                      END IF
                  END IF
              END IF

C     SPECIAL DELSUR OPERATION
              IF(WC.EQ.'DELSUR')  THEN
                  IF(SQ.EQ.0.AND.SN.EQ.0.OR.STI.EQ.1) THEN
                      WC='PMP     '
                      WQ='DELSUR  '
                      SQ=1
                  ELSE
                      IF(S1.EQ.1) THEN
                          WC='PM      '
                          WQ='DELSUR  '
                          SQ=1
                      END IF
                  END IF
              END IF

              IF(WC.EQ.'PM'.OR.WC.EQ.'PMP'.OR.WC.EQ.'OPCON')  THEN
                  CALL PM
                  RETURN
              END IF

              IF(WC.EQ.'WSYS'.OR.WC.EQ.'WSYSTEM')  THEN
                  CALL MYSYS
                  RETURN
              END IF

              IF(WC.EQ.'SYS'.OR.WC.EQ.'SYSTEM')  THEN
                  CALL MYSYS
                  RETURN
              END IF

              IF(WC.EQ.'COFACTOR')  THEN
                  CALL COFACTOR
                  RETURN
              END IF

              IF(WC.EQ.'DATE')  THEN
                  CALL MYDATE(DDATE)
                  DDDAT=DDATE
                  WRITE(OUTLYNE,1919) DDATE
                  CALL SHOWIT(0)
 1919             FORMAT(A10)
                  RETURN
              END IF

              IF(WC.EQ.'TIME')  THEN
                  CALL MYTIME(TTIME)
                  TTTIM=TTIME
                  WRITE(OUTLYNE,1918) TTIME
                  CALL SHOWIT(0)
 1918             FORMAT(A8)
                  RETURN
              END IF

              IF(WC.EQ.'SETTIMER')  THEN
                  CALL SETTIM
                  RETURN
              END IF
              IF(WC.EQ.'SEETIMER')  THEN
                  CALL SEETIM
                  RETURN
              END IF

              IF(WC.EQ.'RAY')  THEN
                  IF(WQ.EQ.'CAOB') CACOCH=1
C     IF NOT OPTIMIZATION OR TOLERANCING, MESSAGE ON
                  IF(F28.EQ.0.AND.F31.EQ.0) MSG=.TRUE.
C     IF VIE, MSG OFF
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  NOCOAT=.FALSE.
                  IF(.NOT.GLOBE) THEN
                      GRASET=.TRUE.
                      DXFSET=.TRUE.
                  END IF
                  CALL RRAY
C     DONE, MSG BACK ON
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'MRAYS')  THEN
                  IF(WQ.EQ.'CAOB') CACOCH=1
                  CALL MRRAYS
                  RETURN
              END IF

              IF(WC.EQ.'MFOBS')  THEN
                  CALL MFFOBS
                  RETURN
              END IF

              IF(WC.EQ.'MTRACE')  THEN
                  MSG=.FALSE.
                  CALL MTRACER
C     DONE, MSG BACK ON
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'READIRAD') THEN
                  CALL READIRAD
                  RETURN
              END IF

              IF(WC.EQ.'MTRACEI1') THEN
                  MSG=.FALSE.
C       SHUT OFF DIFFERENTIAL TRACING
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DIFFOB OFF'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DIFRAY OFF'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  CALL MTRACERI_GRID1
C       TURN ON DIFFERENTIAL TRACING
C     DONE, MSG BACK ON
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DIFFOB ON'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DIFRAY ON'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'MTRACEI2') THEN
                  MSG=.FALSE.
C       SHUT OFF DIFFERENTIAL TRACING
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DIFFOB OFF'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DIFRAY OFF'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  CALL MTRACERI_GRID2
C       TURN ON DIFFERENTIAL TRACING
C     DONE, MSG BACK ON
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DIFFOB ON'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='DIFRAY ON'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'INTEN')  THEN
C       RHIST_INTENSITY CALCULATION FROM SHORT RAYHIST.DAT
                  CALL RHIST_INTENSITY
                  RETURN
              END IF

              IF(WC.EQ.'IRAY')  THEN
                  IF(WQ.EQ.'CAOB') CACOCH=1
                  OLDIF=LDIF
                  OLDIF2=LDIF2
                  LDIF=.FALSE.
                  LDIF2=.TRUE.
                  OLDREF=NEWREF
                  NEWREF=1
                  OLDSTOP=INT(SYSTEM1(26))
                  OLDSTOP2=INT(SYSTEM1(27))
                  SYSTEM1(25)=1.0D0
                  SYSTEM1(26)=1.0D0
                  SYSTEM1(27)=0.0D0
C     IF VIE, MSG OFF
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  NOCOAT=.FALSE.
                  IF(.NOT.GLOBE) THEN
                      GRASET=.TRUE.
                      DXFSET=.TRUE.
                  END IF
                  CALL RRAY
C     DONE, MSG BACK ON
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  NEWREF=OLDREF
                  SYSTEM1(25)=DBLE(NEWREF)
                  SYSTEM1(26)=DBLE(OLDSTOP)
                  SYSTEM1(27)=DBLE(OLDSTOP2)
                  LDIF=OLDIF
                  LDIF2=OLDIF2
                  RETURN
              END IF

              IF(WC.EQ.'IRAYA')  THEN
                  OS62=SYSTEM1(62)
                  OS63=SYSTEM1(63)
                  SYSTEM1(62)=0.0D0
                  SYSTEM1(63)=0.0D0
                  OLDIF=LDIF
                  OLDIF2=LDIF2
                  LDIF=.FALSE.
                  LDIF2=.TRUE.
                  OLDREF=NEWREF
                  NEWREF=1
                  OLDSTOP=INT(SYSTEM1(26))
                  OLDSTOP2=INT(SYSTEM1(27))
                  SYSTEM1(25)=1.0D0
                  SYSTEM1(26)=1.0D0
                  SYSTEM1(27)=0.0D0
                  ITRACE=.TRUE.
C     IF VIE, MSG OFF
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  CALL IRAY
                  LDIF=OLDIF
                  LDIF2=OLDIF2
C     DONE, MSG BACK ON
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  NEWREF=OLDREF
                  SYSTEM1(25)=DBLE(NEWREF)
                  SYSTEM1(26)=DBLE(OLDSTOP)
                  SYSTEM1(27)=DBLE(OLDSTOP2)
                  LDIF=OLDIF
                  LDIF2=OLDIF2
                  SYSTEM1(62)=OS62
                  SYSTEM1(63)=OS63
                  ITRACE=.FALSE.
                  RETURN
              END IF

              IF(WC.EQ.'FOBA') THEN
C     FOBA CREATES THE INPUT FOR A REGULAR FOB COMMAND
C     AND IF SUCCESSFUL, IT EXECUTES THE FOB, ELSE IT DOES NOT
                  CALL FOBA
                  NEWOBJ=0
                  NEWREF=INT(SYSTEM1(25))
                  NEWIMG=INT(SYSTEM1(20))
                  RETURN
              END IF
              IF(WC.EQ.'FOB'.OR.WC.EQ.'FOBH')  THEN
C     NO LINE SPREAD FUNCTIONS MAY EXIST NOW
                  RSTREHL_EXIST=.FALSE.
                  LSF=.FALSE.
C     NO SPOTS EXIST
                  SPDEXT=.FALSE.
                  GSPDEXT=.FALSE.
                  CPFNEXT=.FALSE.
                  CALL DELPSF

C     IF NOT OPTIMIZATION OR TOLERANCING, MESSAGE ON
                  IF(F28.EQ.0.AND.F31.EQ.0) MSG=.TRUE.
C     IF VIE, MSG OFF
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  NEWOBJ=0
                  NEWREF=INT(SYSTEM1(25))
                  NEWIMG=INT(SYSTEM1(20))
                  IF(WC.EQ.'FOB     ') THEN
                      CALL FFOB
                  END IF
                  IF(WC.EQ.'FOBH    ')CALL FFOBH
                  IF(STI.EQ.1) RETURN
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  IF(.NOT.REFEXT) RETURN
                  EXIS51=.FALSE.
                  INQUIRE(FILE=trim(HOME)//'PSF.DAT',EXIST=EXIS51)
                  IF(EXIS51) THEN
                      OPEN51=.FALSE.
                      INQUIRE(FILE=trim(HOME)//'PSF.DAT',OPENED=OPEN51)
                      IF(.NOT.OPEN51) THEN
C     OPEN FILE
                          OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                      ,FORM='FORMATTED',FILE=trim(HOME)//'PSF.DAT'
     2                      ,STATUS='UNKNOWN')
                      END IF
                      CALL CLOSE_FILE(51,0)
                  END IF
                  EXIS51=.FALSE.
                  INQUIRE(FILE=trim(HOME)//'SPSF.DAT',EXIST=EXIS51)
                  IF(EXIS51) THEN
                      OPEN51=.FALSE.
                      INQUIRE(FILE=trim(HOME)//'SPSF.DAT',OPENED=OPEN51)
                      IF(.NOT.OPEN51) THEN
C     OPEN FILE
                          OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                      ,FORM='FORMATTED',FILE=trim(HOME)//'SPSF.DAT'
     2                      ,STATUS='UNKNOWN')
                      END IF
                      CALL CLOSE_FILE(51,0)
                  END IF
                  OLDIF=LDIF
C     IF DIFFERENTIAL CHIEF RAYS ARE TRACED, TRACE REQULAR DIFFERENTIAL
C     RAYS AS WELL ELSE DON'T
                  IF(LDIF2) LDIF=.TRUE.
                  SAVE_KDP(3)=SAVEINPT(3)
                  SQ=0
                  SST=0
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
                  W1=0.0D0
                  W2=0.0D0
                  W3=LFOB(4)
                  WC='RAY     '
                  NOCOAT=.FALSE.
                  IF(.NOT.GLOBE) THEN
                      GRASET=.TRUE.
                      DXFSET=.TRUE.
                  END IF
                  CALL RRAY
                  NOCOAT=.TRUE.
                  LDIF=OLDIF
                  REST_KDP(3)=RESTINPT(3)
C     NOW CALL AUXFOB TO CALCULATE DIFFERENTIAL RAY BASED FFL,BFL AND MAGS
C     AND PUPIL DIAMETERS
                  CALL LASTRAY(1)
                  IF(LDIF2.AND.RAYEXT.AND.REFEXT) THEN
                      CALL AUXFOB()
                  END IF
                  CALL LASTRAY(2)
                  RETURN
              END IF

              IF(WC.EQ.'IFOB')  THEN
                  OLDIF=LDIF
                  OLDIF2=LDIF2
                  LDIF=.FALSE.
                  LDIF2=.TRUE.
                  OLDREF=NEWREF
                  NEWREF=1
                  OLDSTOP=INT(SYSTEM1(26))
                  OLDSTOP2=INT(SYSTEM1(27))
                  SYSTEM1(25)=1.0D0
                  SYSTEM1(26)=1.0D0
                  SYSTEM1(27)=0.0D0
C     NO LINE SPREAD FUNCTIONS MAY EXIST NOW
                  LSF=.FALSE.
C     NO SPOTS EXIST
                  SPDEXT=.FALSE.
                  GSPDEXT=.FALSE.
                  CPFNEXT=.FALSE.
                  CALL DELPSF

C     IF VIE, MSG OFF
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  CALL FFOB
                  IF(STI.EQ.1) RETURN
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  NEWREF=OLDREF
                  SYSTEM1(25)=DBLE(NEWREF)
                  SYSTEM1(26)=DBLE(OLDSTOP)
                  SYSTEM1(27)=DBLE(OLDSTOP2)
                  LDIF=OLDIF
                  LDIF2=OLDIF2
                  RETURN
              END IF

              IF(WC.EQ.'VB'.OR.WC.EQ.'VBA')  THEN
                  OPTMES=.FALSE.
                  CALL VARBLL
                  OPTMES=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'TVB')  THEN
                  IF(F12.NE.1) THEN
                      OUTLYNE=
     1                'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'THE "TVB" COMMAND MAY BE ISSUED'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF

                  OPTMES=.FALSE.
                  CALL TVARBLL
                  OPTMES=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'COMPS')  THEN
                  IF(F12.NE.1) THEN
                      OUTLYNE=
     1                'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'THE "COMPS" COMMAND MAY BE ISSUED'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  OPTMES=.FALSE.
                  CALL CVARBLL
                  OPTMES=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'MR'.OR.WC.EQ.'MRA'
     1        .OR.WC.EQ.'OP'.OR.WC.EQ.'OPA')  THEN
C       DO A FORCED RETURN TO CFG1
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL FRCCF1(1)
                  OPTMES=.FALSE.
                  CALL MAROUT
                  OPTMES=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'TOPS') THEN
                  OPTMES=.FALSE.
                  CALL TOPOUT
                  OPTMES=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'CRITS') THEN
                  OPTMES=.FALSE.
                  CALL CRITOUT
                  OPTMES=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'AIMRAY'.OR.WC.EQ.'RAYAIM')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL NOAIMM
                  RETURN
              END IF

              IF(WC.EQ.'FLIPREFX'.OR.WC.EQ.'FLIPREFY')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL FLIPREF
                  RETURN
              END IF

              IF(WC.EQ.'SCREEN')  THEN
                  IF(SQ.EQ.0) WQ='ON'
                  IF(SQ.EQ.0) SQ=1
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL SCREENIT
                  RETURN
              END IF

              IF(WC.EQ.'RHIST')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL RHISTORY
                  RETURN
              END IF

              IF(WC.EQ.'FRAME')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL FFRAME
                  RETURN
              END IF

              IF(WC.EQ.'NEUTFILE')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  IF(WQ.EQ.'ON') NEUTFILE=.TRUE.
                  IF(WQ.EQ.'OFF') NEUTFILE=.FALSE.
                  RETURN
              END IF

              IF(WC.EQ.'MACFAIL')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL MACFAIL
                  RETURN
              END IF

              IF(WC.EQ.'USEOLREF')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL OLDREFDAT
                  RETURN
              END IF

              IF(WC.EQ.'SAVEREF')  THEN
                  CALL SAVEREFDATA
                  RETURN
              END IF

              IF(WC.EQ.'SAVERAY')  THEN
                  CALL SAVE_RAY_DATA
                  RETURN
              END IF

              IF(WC.EQ.'RESTRAY')  THEN
                  CALL REST_RAY_DATA
                  RETURN
              END IF

              IF(WC.EQ.'CLEARRAY')  THEN
                  CALL CLEAR_RAY_DATA
                  RETURN
              END IF

              IF(WC.EQ.'CARTMAN')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO')  WQ='OFF'
                  IF(WQ.EQ.'ON')  CARTMAN=.TRUE.
                  IF(WQ.EQ.'OFF') CARTMAN=.FALSE.
                  IF(STI.EQ.1.AND.CARTMAN) OUTLYNE='CARTMAN IS TRUE'
                  IF(STI.EQ.1.AND..NOT.CARTMAN) OUTLYNE='CARTMAN IS FALSE'
                  IF(STI.EQ.1) CALL SHOWIT(1)
                  RETURN
              END IF

              IF(WC.EQ.'REVRAY')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL SET_REVRAY
                  RETURN
              END IF

              IF(WC.EQ.'AIMAPL')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL NOAIMAPL
                  RETURN
              END IF

              IF(WC.EQ.'ALL')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL ALLDEF
                  RETURN
              END IF

              IF(WC.EQ.'NODRAW'.OR.WC.EQ.'YESDRAW')  THEN
                  CALL NODRAWW
                  RETURN
              END IF

              IF(WC.EQ.'NOWMF'.OR.WC.EQ.'YESWMF')  THEN
                  CALL NOWMFF
                  RETURN
              END IF

              IF(WC.EQ.'PSFROT')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL PSFROT
                  RETURN
              END IF

              IF(WC.EQ.'PSFLIN'.OR.WC.EQ.'PSFLOG')  THEN
                  CALL PSFLINLOG
                  RETURN
              END IF

              IF(WC.EQ.'PSFTAG'.OR.WC.EQ.'PSFLI')  THEN
                  CALL PSFTAGG
                  RETURN
              END IF

              IF(WC.EQ.'CAPFNROT')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL CAPFNROT
                  RETURN
              END IF

              IF(WC.EQ.'PLOT'.AND.WQ.EQ.'CAPFNOPD'.OR.
     1        WC.EQ.'PLOT'.AND.WQ.EQ.'CAPFNAPD') THEN
                  CALL PLTCAPFN
                  PLOTCAPCON=.FALSE.
                  RETURN
              END IF

              IF(WC.EQ.'PLOTCON'.AND.WQ.EQ.'CAPFNOPD'.OR.
     1        WC.EQ.'PLOTCON'.AND.WQ.EQ.'CAPFNAPD') THEN
                  OCOLBAC=COLBAC
                  COLBAC=0
                  OCOLDEF=COLDEF
                  COLBAC=0
                  COLDEF=14
                  IU=0
                  CALL PLTCAPCO(IU)
                  IF(IU.EQ.0) THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='DRAW'
                      CALL PROCES
                  END IF
                  REST_KDP(1)=RESTINPT(1)
                  COLBAC=OCOLBAC
                  COLDEF=OCOLDEF
                  RETURN
              END IF

              IF(WC.EQ.'USERCONT') THEN
                  OCOLBAC=COLBAC
                  COLBAC=0
                  OCOLDEF=COLDEF
                  COLBAC=0
                  COLDEF=14
                  IU=0
                  CALL USER_CONTOUR(IU)
                  IF(IU.EQ.0) THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='DRAW'
                      CALL PROCES
                  END IF
                  REST_KDP(1)=RESTINPT(1)
                  COLBAC=OCOLBAC
                  COLDEF=OCOLDEF
                  RETURN
              END IF

              IF(WC.EQ.'IOBJECT'.OR.WC.EQ.'IOBJECTN'.OR.WC.EQ.'COLOR'
     1        .OR.WC.EQ.'OBJVAL'.OR.WC.EQ.'OTOBMP'.OR.WC.EQ.'ITOBMP'
     2        .OR.WC.EQ.'IIMAGE'.OR.WC.EQ.'IIMAGEN'.OR.WC.EQ.'IMTRACE1'
     3        .OR.WC.EQ.'IMTRACE2'.OR.WC.EQ.'IMTRACE3'.OR.WC.EQ.'BUILDIMG'
     4        .OR.WC.EQ.'PLTOBJ'.OR.WC.EQ.'OBJVAL'.OR.WC.EQ.'OFROMBMP'
     5        .OR.WC.EQ.'IFROMBMP'.OR.WC.EQ.'PLTIMG'.OR.WC.EQ.'BMPREADR'
     6        .OR.WC.EQ.'PSFTOIMG'.OR.WC.EQ.'IIMAGED'.OR.WC.EQ.'IMGSLICE'
     7        .OR.WC.EQ.'LMINUSR'.OR.WC.EQ.'IOBJECTD'.OR.WC.EQ.'IMTRACE4'
     8        .OR.WC.EQ.'IMTRACE5'.OR.WC.EQ.'INTTOPSF') THEN
                  CALL FULLIMAGING
                  RETURN
              END IF

              IF(WC.EQ.'PLOT'.AND.WQ.EQ.'SAGFILE') THEN
                  PLOTCAPCON=.FALSE.
                  CALL PLT_SAGFILE
                  RETURN
              END IF

              IF(WC.EQ.'SAGFLROT')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL SSAGFLROT
                  RETURN
              END IF

              IF(WC.EQ.'RMSMAP'.OR.WC.EQ.'PTVMAP'.OR.WC.EQ.'STRLMAP') THEN
                  CALL MAPFIELDOPD
                  RETURN
              END IF

              IF(WC.EQ.'SAGFLROT')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL SSAGFLROT
                  RETURN
              END IF

              IF(WC.EQ.'CAPFNTAG'.OR.WC.EQ.'CAPFNLI')  THEN
                  CALL CAPFNTAGG
                  RETURN
              END IF

              IF(WC.EQ.'PSFWRITE')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL PSFWRTE
                  RETURN
              END IF

              IF(WC.EQ.'PSFBIN')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL PSFBN
                  RETURN
              END IF

              IF(WC.EQ.'PSFPLOT')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL PSFPLT
                  RETURN
              END IF

              IF(WC.EQ.'TEL')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL TELAIM
                  RETURN
              END IF

              IF(WC.EQ.'GEOLEICA')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL GEOLEICA
                  RETURN
              END IF

              IF(WC.EQ.'DIFLEICA')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL DIFLEICA
                  RETURN
              END IF

              IF(WC.EQ.'OVERBOSE')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL OVERBOSE
                  RETURN
              END IF

              IF(WC.EQ.'OPTMINIT')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL OPTMINIT
                  RETURN
              END IF

              IF(WC.EQ.'NEAR'.OR.WC.EQ.'FAR')  THEN
                  CALL NEARFARNEAR
                  RETURN
              END IF

              IF(WC.EQ.'LENSREST')  THEN
                  CALL LENSREST
                  RETURN
              END IF

              IF(WC.EQ.'LSAVE')  THEN
                  CALL LENSSAVE_NOOPT
                  RETURN
              END IF

              IF(WC.EQ.'LENSSAVE')  THEN
                  CALL LENSSAVE
                  RETURN
              END IF

              IF(WC.EQ.'LENSLOC')  THEN
                  CALL LENSLOC
                  RETURN
              END IF

              IF(WC.EQ.'LENSDIR')  THEN
                  CALL LENSDIR
                  RETURN
              END IF

              IF(WC.EQ.'DIFRAY')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL LLDDFF
                  RETURN
              END IF

              IF(WC.EQ.'LINK'.OR.WC.EQ.'LINKD'.OR.WC.EQ.'TLINK') THEN
                  IF(WC.EQ.'LINK'.OR.WC.EQ.'LINKD') THEN
                      LERROR=.FALSE.
                      CALL LINKIT(LERROR)
                      RETURN
                  END IF

                  IF(WC.EQ.'TLINK') THEN
                      LERROR=.FALSE.
                      SAVE_KDP(3)=SAVEINPT(3)
                      WC='LINK'
                      CALL LINKIT(LERROR)
                      SAVE_KDP(3)=SAVEINPT(3)
                      IF(.NOT.LERROR) THEN
                          SAVE_KDP(3)=SAVEINPT(3)
                          WC='LINKD'
                          S2=0
                          S3=0
                          S4=0
                          S5=0
                          SN=1
                          DF2=1
                          DF3=1
                          DF4=1
                          DF5=1
                          W2=0.0D0
                          W3=0.0D0
                          W4=0.0D0
                          W5=0.0D0
                          CALL LINKIT(LERROR)
                          REST_KDP(3)=RESTINPT(3)
                      END IF
                      RETURN
                  END IF
              END IF

              IF(WC.EQ.'DIFFOB')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL LLDDF2
                  RETURN
              END IF

              IF(WC.EQ.'OPDIF')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL OOPDIF
                  RETURN
              END IF

              IF(WC.EQ.'VIRTRAY')  THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL LLDDF3
                  RETURN
              END IF

              IF(WC.EQ.'XFAN'.OR.WC.EQ.'YFAN'.OR.WC.EQ.
     1        'NFAN'.OR.WC.EQ.'PFAN')  THEN
                  CACOCH=1
C       SET SO NO DIFFERENTIAL RAYS ARE TRACED
C       AND NO INDEPENDENT ERROR MESSAAGES ARE PRINTED
                  GRASET=.FALSE.
                  OLDIF=LDIF
                  LDIF=.FALSE.
                  MSG=.FALSE.
                  CALL FANS
                  LDIF=OLDIF
                  MSG=.TRUE.
                  CACOCH=0
                  RETURN
              END IF

              IF(WC.EQ.'VERTS') THEN
                  X1=GPREG(1)
                  Y1=GPREG(2)
                  Z1=GPREG(3)
                  XL1=GPREG(4)
                  XM1=GPREG(5)
                  XN1=GPREG(6)
                  YL1=GPREG(7)
                  YM1=GPREG(8)
                  YN1=GPREG(9)
                  ZL1=GPREG(10)
                  ZM1=GPREG(11)
                  ZN1=GPREG(12)
                  X2=GPREG(13)
                  Y2=GPREG(14)
                  Z2=GPREG(15)
                  XL2=GPREG(16)
                  XM2=GPREG(17)
                  XN2=GPREG(18)
                  YL2=GPREG(19)
                  YM2=GPREG(20)
                  YN2=GPREG(21)
                  ZL2=GPREG(22)
                  ZM2=GPREG(23)
                  ZN2=GPREG(24)
                  CALL VERT123(
     1            X1,Y1,Z1,XL1,XM1,XN1,YL1,YM1,YN1,ZL1,ZM1,ZN1,
     2            X2,Y2,Z2,XL2,XM2,XN2,YL2,YM2,YN2,ZL2,ZM2,ZN2,
     3            X3,Y3,Z3,XL3,XM3,XN3,YL3,YM3,YN3,ZL3,ZM3,ZN3,
     4            ALPHA,BETA,GAMMA)
                  RETURN
              END IF

              IF(WC.EQ.'GLOBAL'.OR.WC.EQ.'OFFSET'.OR.WC.EQ.'VERTEX')
     1        THEN
                  IF(WC.EQ.'GLOBAL') GRASET=.FALSE.
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL GLOBAL
                  RETURN
              END IF

              IF(WC.EQ.'PRGLOBAL')
     1        THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRGLBL
                  RETURN
              END IF

              IF(WC.EQ.'PRXYZ'.OR.WC.EQ.'PRXYI'.OR.
     1        WC.EQ.'PRXYIP') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRRAY
                  RETURN
              END IF

              IF(WC.EQ.'PRNSS') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRNSS
                  RETURN
              END IF

              IF(WC.EQ.'PRFLUX') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRFLUX
                  RETURN
              END IF

              IF(WC.EQ.'PRPOL') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRPOL
                  RETURN
              END IF

              IF(WC.EQ.'PRLMN') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRLMN
                  RETURN
              END IF

              IF(WC.EQ.'PRX'.OR.WC.EQ.'PRY') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRXY
                  RETURN
              END IF

              IF(WC.EQ.'PRZ') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRZ
                  RETURN
              END IF

              IF(WC.EQ.'PRR') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRR
                  RETURN
              END IF

              IF(WC.EQ.'PRXYD') THEN
                  IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  CALL PRXYD
                  RETURN
              END IF

              IF(WC.EQ.'FOBDUMP') THEN
                  CALL FOBDMP
                  RETURN
              END IF

              IF(WC.EQ.'RAYDUMP') THEN
                  CALL RAYDMP
                  RETURN
              END IF

              IF(WC.EQ.'VARIABLE'.OR.WC.EQ.'VARI') THEN
                  TVBCNT=0
                  PFAC=1.0D0
                  ISCOMP(1:MAXCMP)=.FALSE.
                  CALL VRBL1
                  RETURN
              END IF

              IF(WC.EQ.'TVAR') THEN
                  IF(F12.NE.1) THEN
                      OUTLYNE=
     1                'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'THE "TVAR" COMMAND MAY BE ISSUED'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL TVRBL1
                  RETURN
              END IF

              IF(WC.EQ.'COMPVAR') THEN
                  IF(F12.NE.1) THEN
                      OUTLYNE=
     1                'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'THE "COMPVAR" COMMAND MAY BE ISSUED'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL CVRBL1
                  RETURN
              END IF

              IF(WC.EQ.'MERIT') THEN
                  PFAC=1.0D0
C
                  FCCNT=0
                  TOPCNT=0
                  ISCRIT(1:MAXFOCRIT)=.FALSE.
                  ISTOP(1:MAXTOP)=.FALSE.
                  CALL MERIT
                  JK_CHMODE=.FALSE.
                  CORMOD=1
                  CURFIG=1
                  RETURN
              END IF

              IF(WC.EQ.'TOPER') THEN
                  CALL TOPER
                  JK_CHMODE=.FALSE.
                  RETURN
              END IF

              IF(WC.EQ.'FOCRIT') THEN
                  CALL FOCRIT
                  JK_CHMODE=.FALSE.
                  RETURN
              END IF

              IF(WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'VARIABLE'.OR.
     1        WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'VB'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'VARIABLE'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'VB') THEN
                  CALL DEROFF
                  CALL UVRBL1
                  RETURN
              END IF

              IF(WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'TVAR'.OR.
     1        WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'TVB'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'TVAR'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'TVB') THEN
                  IF(F12.NE.1) THEN
                      OUTLYNE=
     1                'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'THE "UPDATE TVAR" COMMAND MAY BE ISSUED'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL DEROFF
                  CALL TUVRBL1
                  RETURN
              END IF

              IF(WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'COMPVAR'.OR.
     1        WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'CMP'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'COMPVAR'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'CMP') THEN
                  IF(F12.NE.1) THEN
                      OUTLYNE=
     1                'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'THE "UPDATE COMPVAR" COMMAND MAY BE ISSUED'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'NO ACTION TAKEN'
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL DEROFF
                  CALL UCVRBL1
                  RETURN
              END IF

              IF(WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'MERIT'.OR.
     1        WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'M'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'MERIT'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'M') THEN
                  JK_CHMODE=.FALSE.
                  CORMOD=1
                  CURFIG=1
                  CALL DEROFF
                  CALL UMERIT
                  RETURN
              END IF

              IF(WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'TOPER'.OR.
     1        WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'TOP'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'TOPER'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'TOP') THEN
                  CALL DEROFF
                  CALL UTOPER
                  RETURN
              END IF

              IF(WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'FOCRIT'.OR.
     1        WC.EQ.'UPDATE'.AND.
     1        WQ.EQ.'FC'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'FOCRIT'.OR.
     1        WC.EQ.'U'.AND.
     1        WQ.EQ.'FC') THEN
                  JK_CHMODE=.FALSE.
                  CALL DEROFF
                  CALL UFOCRIT
                  RETURN
              END IF

              IF(WC.EQ.'OPD') THEN
                  CALL PROPD
                  RETURN
              END IF

              IF(WC.EQ.'AUTO') THEN
                  CALL AUTO
                  RETURN
              END IF

              IF(WC.EQ.'HEADINGS') THEN
                  CALL SETHED
                  RETURN
              END IF

              IF(WC.EQ.'PLOT'.OR.WC.EQ.'PNOTE') THEN
                  IF(DEVTYP.NE.1) CALL PLTDEV
                  PLOTCAPCON=.FALSE.
                  CALL PPLOTT
                  RETURN
              END IF

              IF(WC.EQ.'DXF') THEN
                  CALL DDXFF
                  RETURN
              END IF

              IF(WC.EQ.'PLTXFAN'.AND.F17.EQ.0.OR.
     1        WC.EQ.'PLTYFAN'.AND.F17.EQ.0.OR.
     1        WC.EQ.'PLTPFAN'.AND.F17.EQ.0.OR.
     1        WC.EQ.'PLTNFAN'.AND.F17.EQ.0.OR.
     1        WC.EQ.'PLOTFANS'.AND.F17.EQ.0) THEN
                  PLOTCAPCON=.FALSE.
                  CALL PPLOTT
                  RETURN
              END IF

              IF(WC.EQ.'PFANAXIS'.AND.F17.EQ.0.OR.
     2        WC.EQ.'PFANLBL'.AND.F17.EQ.0.OR.
     3        WC.EQ.'PFANCOMP'.AND.F17.EQ.0.OR.
     4        WC.EQ.'PFANCAP'.AND.F17.EQ.0.OR.
     5        WC.EQ.'PFANSSI'.AND.F17.EQ.0) THEN
                  PLOTCAPCON=.FALSE.
                  CALL PPLOTT
                  RETURN
              END IF

              IF(WC.EQ.'PLTXYFAN'.AND.F17.EQ.0.OR.
     1        WC.EQ.'PLTYXFAN'.AND.F17.EQ.0) THEN
                  PLOTCAPCON=.FALSE.
                  CALL PPLOTT
                  RETURN
              END IF

              IF(WC.EQ.'FANS') THEN
                  CALL RIMS
                  RETURN
              END IF

              IF(WC.EQ.'VIEOFF') THEN
                  CALL VIEOFF
                  RETURN
              END IF

              IF(WC.EQ.'SHOWNSS') THEN
                  CALL SHOWNSS
                  RETURN
              END IF

              IF(WC.EQ.'VIEVIG') THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL VIEVIG
                  RETURN
              END IF

              IF(WC.EQ.'COATINGS') THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL SETCOAT
                  RETURN
              END IF

              IF(WC.EQ.'VIESYM') THEN
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO') WQ='OFF'
                  CALL VIESYM
                  RETURN
              END IF

              IF(WC.EQ.'VIE'.OR.WC.EQ.'VIECO') THEN
                  CACOCHVIE=0
                  IF(WC.EQ.'VIECO') CACOCHVIE=1
                  F34=1
                  MSG=.FALSE.
                  CALL VIE(CACOCHVIE)
                  F34=0
                  MSG=.TRUE.
                  CACOCH=0
                  CACOCHVIE=0
                  RETURN
              END IF

              IF(WC.EQ.'SPDSSI') THEN
                  CALL SPDSSI
                  RETURN
              END IF

              IF(WC.EQ.'DET') THEN
                  CALL DETECTOR
                  RETURN
              END IF

              IF(WC.EQ.'PLTSPD') THEN
                  CALL PLTSPD
                  RETURN
              END IF

              IF(WC.EQ.'FANFIELD') THEN
                  CALL FANFOV
                  RETURN
              END IF

              IF(WC.EQ.'ORIENT'.OR.WC.EQ.'NORIENT') THEN
                  PLOTCAPCON=.FALSE.
                  CALL PPLOTT
                  RETURN
              END IF

              IF(WC.EQ.'DRAW'.OR.WC.EQ.'LISTDRAW') THEN
                  CALL PPLOTT
                  RETURN
              END IF

              IF(WC.EQ.'REPLAY') THEN
                  CALL REPLAYFILE
                  RETURN
              END IF

              IF(WC.EQ.'DRAWFAN') THEN
                  CALL PPLOTT
                  PLOTCAPCON=.FALSE.
                  RETURN
              END IF

              IF(WC.EQ.'PLT_FAN') THEN
                  CALL PPLOTT
                  PLOTCAPCON=.FALSE.
                  RETURN
              END IF

              IF(WC.EQ.'GRAOUT') THEN
                  CALL PPLOTT
                  RETURN
              END IF

              IF(WC.EQ.'PLOT'.AND.WQ.NE.'DEV'.AND.F17.EQ.1) THEN
                  OUTLYNE=
     1            'THIS PLOT COMMAND NOT OPERATIONAL AT THE "SPECT" LEVEL'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF

              IF(WC.EQ.'SAG') THEN
                  SAGCODE=0
                  CALL SSAAGG
                  RETURN
              END IF

              IF(WC.EQ.'SPD'.OR.WC.EQ.'FAIL'.OR.WC.EQ.'FAILACC'
     1        .OR.WC.EQ.'SPDADD'.OR.WC.EQ.'SPDSAVE'.OR.WC.EQ.'SPDSTATS') THEN
                  GRASET=.FALSE.
                  IF(WC.EQ.'SPDSTATS') SPDEXT=.FALSE.
                  MSGSPD=.TRUE.
                  CALL SPOT
                  RETURN
              END IF

              IF(WC.EQ.'ISPD')THEN
                  OLDIF=LDIF
                  OLDIF2=LDIF2
                  LDIF=.FALSE.
                  LDIF2=.TRUE.
                  OLDREF=NEWREF
                  NEWREF=1
                  OLDSTOP=INT(SYSTEM1(26))
                  OLDSTOP2=INT(SYSTEM1(27))
                  SYSTEM1(25)=1.0D0
                  SYSTEM1(26)=1.0D0
                  SYSTEM1(27)=0.0D0
                  GRASET=.FALSE.
                  SPDEXT=.FALSE.
                  MSGSPD=.TRUE.
C     IF VIE, MSG OFF
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  CALL SPOT
C     DONE, MSG BACK ON
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  NEWREF=OLDREF
                  SYSTEM1(25)=DBLE(NEWREF)
                  SYSTEM1(26)=DBLE(OLDSTOP)
                  SYSTEM1(27)=DBLE(OLDSTOP2)
                  LDIF=OLDIF
                  LDIF2=OLDIF2
                  RETURN
              END IF

              IF(WC.EQ.'ISPDA')THEN
                  GRASET=.FALSE.
                  SPDEXT=.FALSE.
                  MSGSPD=.TRUE.
                  OS62=SYSTEM1(62)
                  OS63=SYSTEM1(63)
                  SYSTEM1(62)=0.0D0
                  SYSTEM1(63)=0.0D0
                  OLDIF=LDIF
                  OLDIF2=LDIF2
                  LDIF=.FALSE.
                  LDIF2=.TRUE.
                  OLDREF=NEWREF
                  NEWREF=1
                  OLDSTOP=INT(SYSTEM1(26))
                  OLDSTOP2=INT(SYSTEM1(27))
                  SYSTEM1(25)=1.0D0
                  SYSTEM1(26)=1.0D0
                  SYSTEM1(27)=0.0D0
                  ITRACE=.TRUE.
C     IF VIE, MSG OFF
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  CALL SPOT
                  LDIF=OLDIF
                  LDIF2=OLDIF2
C     DONE, MSG BACK ON
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  NEWREF=OLDREF
                  SYSTEM1(25)=DBLE(NEWREF)
                  SYSTEM1(26)=DBLE(OLDSTOP)
                  SYSTEM1(27)=DBLE(OLDSTOP2)
                  LDIF=OLDIF
                  LDIF2=OLDIF2
                  SYSTEM1(62)=OS62
                  SYSTEM1(63)=OS63
                  ITRACE=.FALSE.
C     DONE, MSG BACK ON
                  IF(F34.EQ.1) MSG=.FALSE.
                  IF(F34.EQ.0) MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'RING'.OR.WC.EQ.'RINGS'.OR.WC.EQ.'RANNUM'
     1        .OR.WC.EQ.'SPDRESET'.OR.WC.EQ.'APOD'.OR.
     2        WC.EQ.'SPOT'.OR.WC.EQ.'RECT') THEN
                  CALL SPOTSET
                  RETURN
              END IF

              IF(WC.EQ.'OPRING'.OR.WC.EQ.'OPRINGS'.OR.WC.EQ.'OPRANNUM'
     1        .OR.WC.EQ.'OPSPDRST'.OR.
     2        WC.EQ.'OPSPOT'.OR.WC.EQ.'OPRECT') THEN
                  GRSPT=0
                  CALL OPSPOTSET
                  RETURN
              END IF

              IF(WC.EQ.'OPNRD'
     1        .OR.WC.EQ.'TOLNRD'.OR.WC.EQ.'NRD'.OR.WC.EQ.'TGR'
     1        .OR.WC.EQ.'PGR'.OR.WC.EQ.'GRI'.OR.WC.EQ.'CAPFNNRD'.OR.
     1        WC.EQ.'EXTENT') THEN
                  CALL MERIT2
                  RETURN
              END IF

              IF(WC.EQ.'REDSQ'.OR.WC.EQ.'REDSUMSQ'.OR.WC.EQ.'SREDSQ') THEN
                  IF(WC.EQ.'REDSUMSQ') THEN
                      GRASET=.FALSE.
                      CALL SUMSPREDSQ
                  ELSE
                      CALL SPREDSQ
                  END IF
                  RETURN
              END IF

              IF(WC.EQ.'DRED') THEN
                  ERRORPSF=.FALSE.
                  CALL PSFSPOT(ERRORPSF,MMM)
                  IF(ERRORPSF) THEN
                      OUTLYNE=
     1                'NO DIFFRACTION PSF EXISTS, NO PSF SPOT FILE WAS CREATED'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL DSPRED(MMM)
                  RETURN
              END IF

              IF(WC.EQ.'DREDSQ') THEN
                  ERRORPSF=.FALSE.
                  CALL PSFSPOT(ERRORPSF,MMM)
                  IF(ERRORPSF) THEN
                      OUTLYNE=
     1                'NO DIFFRACTION PSF EXISTS, "DREDSQ" CAN NOT PROCEED'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL DSPREDSQ(MMM)
                  RETURN
              END IF

              IF(WC.EQ.'PSFSPOT') THEN
                  ERRORPSF=.FALSE.
                  CALL PSFSPOT(ERRORPSF,MMM)
                  RETURN
              END IF

              IF(WC.EQ.'RED'.OR.WC.EQ.'ESEDX'.OR.WC.EQ.'ESEDY'
     1        .OR.WC.EQ.'ESED'.OR.WC.EQ.'REDSUM'.OR.WC.EQ.'SRED'.OR.
     2        WC.EQ.'SESED'.OR.WC.EQ.'SESEDX'.OR.WC.EQ.'SESEDY') THEN
                  IF(WC.EQ.'REDSUM') THEN
                      GRASET=.FALSE.
                      CALL SUMSPRED
                  ELSE
                      CALL SPRED
                  END IF
                  RETURN
              END IF

              IF(WC.EQ.'LSF') THEN
                  GRASET=.FALSE.
                  CALL LSFLSF
                  RETURN
              END IF

              IF(WC.EQ.'OLSF') THEN
                  GRASET=.FALSE.
                  CALL OLDLSF
                  RETURN
              END IF

              IF(WC.EQ.'EDIT') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='REPLACE'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  CALL EDITOR
                  RETURN
              END IF

              IF(WC.EQ.'RESTORE') THEN
                  GRASET=.FALSE.
                  F28=1
                  MSG=.FALSE.
                  OPTMES=.FALSE.
                  CALL RESTOR(.TRUE.)
                  OPTMES=.TRUE.
                  F28=0
                  MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'ROBB') THEN
                  GRASET=.FALSE.
                  F28=1
                  MSG=.FALSE.
                  OPTMES=.FALSE.
                  CALL ROBB
                  OPTMES=.TRUE.
                  F28=0
                  MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'ITER'.OR.WC.EQ.'IT') THEN
                  OWC=WC
                  IF(SQ.EQ.1) OWQ=WQ
                  IF(SQ.EQ.0) OWQ='        '
                  GRASET=.FALSE.
                  F28=1
                  KILOPT=.FALSE.
                  MSG=.FALSE.
                  OPTMES=.FALSE.
                  IF(SQ.EQ.0.OR.WQ.EQ.'POWL'.OR.WQ.EQ.'P'.OR.WQ.EQ.'FULL'
     1            .OR.WQ.EQ.'F'.OR.WQ.EQ.'DIR'.OR.WQ.EQ.'D'.OR.WQ.EQ.'POWELL') THEN
                      ITCT=INT(W1)
                      IF(ITCT.EQ.0) ITCT=1
                      DO ITNUM=1,ITCT
                          DEREXT=.FALSE.
                          WC=OWC
                          WQ=OWQ
                          SQ=0
                          IF(WQ.NE.'        ') SQ=1
                          DF1=1
                          S1=0
                          SN=0
                          ITERROR=.FALSE.
                          CALL ITER(1,1,ITERROR)
                          IF(ITERROR) THEN
                              ITERROR=.FALSE.
                              EXIT
                          END IF
                          IF(SYSTEM1(101).NE.0.0D0) THEN
                              SAVE_KDP(1)=SAVEINPT(1)
                              INPUT='VB'
                              CALL PROCES
                              REST_KDP(1)=RESTINPT(1)
                              SAVE_KDP(1)=SAVEINPT(1)
                              INPUT='VBA'
                              CALL PROCES
                              REST_KDP(1)=RESTINPT(1)
                              SAVE_KDP(1)=SAVEINPT(1)
                              INPUT='OP'
                              CALL PROCES
                              REST_KDP(1)=RESTINPT(1)
                              SAVE_KDP(1)=SAVEINPT(1)
                              INPUT='OPA'
                              CALL PROCES
                              REST_KDP(1)=RESTINPT(1)
                              SAVE_KDP(1)=SAVEINPT(1)
                              INPUT='OPRD'
                              CALL PROCES
                              REST_KDP(1)=RESTINPT(1)
                          END IF
                          IF(F28.EQ.0.OR.KILOPT) GO TO 8769
                      END DO
 8769                 CONTINUE
                  ELSE
                      WC=OWC
                      WQ=OWQ
                      SQ=0
                      IF(WQ.NE.'        ') SQ=1
                      CALL ITER(1,1,ITERROR)
                      IF(ITERROR) THEN
                          ITERROR=.FALSE.
                      END IF
                      IF(SYSTEM1(101).NE.0.0D0) THEN
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT='VB'
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT='VBA'
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT='OP'
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT='OPA'
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT='OPRD'
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                  END IF
                  OPTMES=.TRUE.
                  F28=0
                  MSG=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'RSV') THEN
C       RESOLVE THE EXISTING MATRIX BUT FIRST DO A RESTORE
                  GRASET=.FALSE.
                  F28=1
                  KILOPT=.FALSE.
                  MSG=.FALSE.
                  OPTMES=.FALSE.
                  WC='RSV'
                  SQ=0
                  CALL ITER(1,1,ITERROR)
                  IF(ITERROR) THEN
                      ITERROR=.FALSE.
                  END IF
                  IF(SYSTEM1(101).NE.0.0D0) THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='VB'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='VBA'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='OP'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='OPA'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='OPRD'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  OPTMES=.TRUE.
                  F28=0
                  MSG=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'SV') THEN
C       RESOLVE THE EXISTING MATRIX WITHOUT FIRST DOING A RESTORE
                  GRASET=.FALSE.
                  F28=1
                  KILOPT=.FALSE.
                  MSG=.FALSE.
                  OPTMES=.FALSE.
                  WC='SV'
                  SQ=0
                  CALL ITER(1,1,ITERROR)
                  IF(ITERROR) THEN
                      ITERROR=.FALSE.
                  END IF
                  IF(SYSTEM1(101).NE.0.0D0) THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='VB'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='VBA'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='OP'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='OPA'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='OPRD'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  OPTMES=.TRUE.
                  F28=0
                  MSG=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'OPRD') THEN
                  IF(RAYCLEAR) THEN
                      RAYEXT=.FALSE.
                      POLEXT=.FALSE.
                      REFEXT=.FALSE.
                  END IF
C       DO A FORCED RETURN TO CFG1
                  GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
                  GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
                  CALL FRCCF1(1)
                  GRASET=.FALSE.
                  F28=1
                  KILOPT=.FALSE.
                  MSG=.FALSE.
                  OPTMES=.FALSE.
                  CALL OPRD
                  OPTMES=.TRUE.
                  F28=0
                  MSG=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'FMT') THEN
                  GRASET=.FALSE.
                  F28=1
                  MSG=.FALSE.
                  OPTMES=.FALSE.
                  CALL FMT
                  OPTMES=.TRUE.
                  F28=0
                  MSG=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'J1') THEN
                  CALL BESS
                  RETURN
              END IF
              IF(WC.EQ.'GOTF') THEN
                  GRASET=.FALSE.
                  IF(LASTWASFOB) THEN
                      MULTIOTF=.FALSE.
                      GRASET=.FALSE.
                      OTFPAIR=1
                      SAVE_KDP(1)=SAVEINPT(1)
                      OLDLDIF2=LDIF2
                      OLDLDIF=LDIF
                      LDIF2=.TRUE.
                      LDIF=.TRUE.
                      WQ='        '
                      SQ=0
                      SST=0
                      STI=0
                      DF1=1
                      DF2=1
                      DF3=1
                      DF4=1
                      DF5=1
                      S1=0
                      S2=0
                      S3=0
                      S4=0
                      S5=0
                      SN=0
C     SET MSG TO FALSE
                      MSG=.FALSE.
                      MSGSPD=.FALSE.
                      ERROR=0
                      WC='SPD     '
                      CALL SPOT
                      IF(.NOT.SPDEXT) EXTGMTF1=.FALSE.
                      IF(.NOT.SPDEXT) EXTGMTF2=.FALSE.
                      IF(.NOT.SPDEXT) RETURN
                      REST_KDP(1)=RESTINPT(1)
                      IF(SQ.EQ.0) THEN
                          IIG=2
                          CALL GOTF
                          IF(.NOT.REFEXT.OR..NOT.SPDEXT) EXTGMTF1=.FALSE.
                          IF(.NOT.REFEXT.OR..NOT.SPDEXT) EXTGMTF2=.FALSE.
                          IF(.NOT.REFEXT.OR..NOT.SPDEXT) RETURN
                      ELSE
                          CALL GOTF
                          IF(.NOT.REFEXT.OR..NOT.SPDEXT) EXTGMTF1=.FALSE.
                          IF(.NOT.REFEXT.OR..NOT.SPDEXT) EXTGMTF2=.FALSE.
                          IF(.NOT.REFEXT.OR..NOT.SPDEXT) RETURN
                          IIG=1
                      END IF
                      RETURN
                  ELSE
C     DO MULTIPLE GOTF FOV'S
                      IF(CFLDCNT.EQ.0) THEN
                          OUTLYNE=
     1                    'MULTIPLE FIELDS OF VIEW ARE NOT DEFINED'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'NO ACTION TAKEN'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      MULTIOTF=.TRUE.
                      IIG=2
                      IF(SQ.EQ.0) THEN
                          IF(CFLDCNT.GT.0) THEN
                              GRASET=.FALSE.
                              OTFPAIR=0
                              DO I=1,CFLDCNT
                                  OTFPAIR=OTFPAIR+1
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  OLDLDIF2=LDIF2
                                  OLDLDIF=LDIF
                                  LDIF2=.TRUE.
                                  LDIF=.TRUE.
                                  WQ='P       '
                                  SQ=1
                                  SST=0
                                  STI=0
                                  W1=CFLDS(2,I)
                                  W2=CFLDS(1,I)
                                  DF1=0
                                  DF2=0
                                  DF3=1
                                  DF4=1
                                  DF5=1
                                  S1=1
                                  S2=1
                                  S3=0
                                  S4=0
                                  S5=0
                                  SN=1
C     SET MSG TO FALSE
                                  MSG=.FALSE.
                                  ERROR=0
                                  WC='FOB     '
                                  CALL FFOB
                                  IF(.NOT.REFEXT) EXTGMTF1=.FALSE.
                                  IF(.NOT.REFEXT) EXTGMTF2=.FALSE.
                                  IF(.NOT.REFEXT) RETURN
                                  FLDCODE(1,I)=XPFOB
                                  FLDCODE(2,I)=YPFOB
                                  FLDUNIT(I)=LUNI
                                  REST_KDP(1)=RESTINPT(1)
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  OLDLDIF2=LDIF2
                                  OLDLDIF=LDIF
                                  LDIF2=.TRUE.
                                  LDIF=.TRUE.
                                  WQ='        '
                                  SQ=0
                                  SST=0
                                  STI=0
                                  DF1=1
                                  DF2=1
                                  DF3=1
                                  DF4=1
                                  DF5=1
                                  S1=0
                                  S2=0
                                  S3=0
                                  S4=0
                                  S5=0
                                  SN=0
C     SET MSG TO FALSE
                                  MSG=.FALSE.
                                  MSGSPD=.FALSE.
                                  ERROR=0
                                  WC='SPD     '
                                  CALL SPOT
                                  IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF1=.FALSE.
                                  IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF2=.FALSE.
                                  IF(.NOT.SPDEXT.OR..NOT.REFEXT) RETURN
                                  REST_KDP(1)=RESTINPT(1)
                                  CALL GOTF
                                  IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF1=.FALSE.
                                  IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF2=.FALSE.
                                  IF(.NOT.SPDEXT.OR..NOT.REFEXT) RETURN
                              END DO
                              RETURN
                          END IF
                      ELSE
C     SQ=1
                      END IF
                      OTFPAIR=1
                      CALL GOTF
                      IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF1=.FALSE.
                      IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF2=.FALSE.
                      IF(.NOT.SPDEXT.OR..NOT.REFEXT) RETURN
                  END IF
                  RETURN
              END IF
              IF(WC.EQ.'DOTF') THEN
                  IF(LASTWASFOB.OR.CPFNEXT) THEN
                      MULTIOTF=.FALSE.
                      GRASET=.FALSE.
                      OTFPAIR=0
                      IF(SQ.EQ.0) THEN
                          IIG=2
                          CALL DOTF
                          IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
                          IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
                          IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
                      ELSE
                          IIG=1
                          CALL DOTF
                          IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
                          IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
                          IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
                      END IF
                      CPFNEXT=.FALSE.
                      LASTWASFOB=.FALSE.
                      RETURN
                  ELSE
                      IF(CFLDCNT.EQ.0) THEN
                          OUTLYNE=
     1                    'MULTIPLE FIELDS OF VIEW ARE NOT DEFINED'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'NO ACTION TAKEN'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      MULTIOTF=.TRUE.
C     DO MULTIPLE DOTF FOV'S
                      IIG=2
                      IF(SQ.EQ.0) THEN
                          IF(REFLOC.EQ.4) THEN
                              OUTLYNE='"RSPH" TEMPORARILY CHANGED FROM "BEST" TO "NOTILT"'
                              CALL SHOWIT(1)
                              OREFLOC=4
                              REFLOC=3
                          END IF
                          IF(CFLDCNT.GT.0) THEN
                              GRASET=.FALSE.
                              OTFPAIR=0
                              SAVE_KDP(1)=SAVEINPT(1)
                              OLDLDIF2=LDIF2
                              OLDLDIF=LDIF
                              LDIF2=.TRUE.
                              LDIF=.TRUE.
                              WQ='P       '
                              SQ=1
                              SST=0
                              STI=0
                              W1=0.0D0
                              W2=0.0D0
                              DF1=0
                              DF2=0
                              DF3=1
                              DF4=1
                              DF5=1
                              S1=1
                              S2=1
                              S3=0
                              S4=0
                              S5=0
                              SN=1
C     SET MSG TO FALSE
                              MSG=.FALSE.
                              ERROR=0
                              WC='FOBA    '
                              CALL FOBA
                              IF(.NOT.REFEXT) EXTDMTF1=.FALSE.
                              IF(.NOT.REFEXT) EXTDMTF2=.FALSE.
                              IF(.NOT.REFEXT) RETURN
                              FLDCODE(1,OTFPAIR)=XPFOB
                              FLDCODE(2,OTFPAIR)=YPFOB
                              FLDUNIT(OTFPAIR)=LUNI
                              REST_KDP(1)=RESTINPT(1)
                              SAVE_KDP(1)=SAVEINPT(1)
                              WC='CAPFN   '
                              WQ='PERFECT '
                              DF1=0
                              DF2=1
                              DF3=1
                              DF4=1
                              DF5=1
                              S1=1
                              S2=0
                              S3=0
                              S4=0
                              S5=0
                              SN=1
                              W1=CAPDEF
                              REFERR=.FALSE.
                              NRDFACTOR=1.0D0
                              PERFECT=.TRUE.
                              CALL COMPAP(REFERR,1)
                              PERFECT=.FALSE.
                              IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
                              IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
                              IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
                              REST_KDP(1)=RESTINPT(1)
                              CALL DOTF
                              IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
                              IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
                              IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
                              CPFNEXT=.FALSE.
                              DO I=1,CFLDCNT
                                  OTFPAIR=OTFPAIR+1
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  OLDLDIF2=LDIF2
                                  OLDLDIF=LDIF
                                  LDIF2=.TRUE.
                                  LDIF=.TRUE.
                                  WQ='P       '
                                  SQ=1
                                  SST=0
                                  STI=0
                                  W1=CFLDS(2,I)
                                  W2=CFLDS(1,I)
                                  DF1=0
                                  DF2=0
                                  DF3=1
                                  DF4=1
                                  DF5=1
                                  S1=1
                                  S2=1
                                  S3=0
                                  S4=0
                                  S5=0
                                  SN=1
C     SET MSG TO FALSE
                                  MSG=.FALSE.
                                  ERROR=0
                                  WC='FOB     '
                                  CALL FFOB
                                  IF(.NOT.REFEXT) EXTDMTF1=.FALSE.
                                  IF(.NOT.REFEXT) EXTDMTF2=.FALSE.
                                  IF(.NOT.REFEXT) RETURN
                                  FLDCODE(1,OTFPAIR)=XPFOB
                                  FLDCODE(2,OTFPAIR)=YPFOB
                                  FLDUNIT(OTFPAIR)=LUNI
                                  REST_KDP(1)=RESTINPT(1)
                                  CALL DOTF
                                  IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
                                  IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
                                  IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
                              END DO
                              IF(REFLOC.EQ.3.AND.OREFLOC.EQ.4) THEN
                                  OUTLYNE='"RSPH" RESET FROM "NOTILT" TO "BEST"'
                                  CALL SHOWIT(1)
                                  REFLOC=3
                              END IF
                              CPFNEXT=.FALSE.
                              LASTWASFOB=.FALSE.
                              RETURN
                          END IF
                      ELSE
C     SQ=1
                      END IF
                      OTFPAIR=0
                      CALL DOTF
                      IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
                      IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
                      IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
                  END IF
                  IF(REFLOC.EQ.3.AND.OREFLOC.EQ.4) THEN
                      OUTLYNE='"RSPH" RESET FROM "NOTILT" TO "BEST"'
                      CALL SHOWIT(1)
                      REFLOC=3
                  END IF
                  CPFNEXT=.FALSE.
                  LASTWASFOB=.FALSE.
                  RETURN
              END IF
C
              IF(WC.EQ.'TFDOTF') THEN
                  GRASET=.FALSE.
                  CALL TFDOTF
                  RETURN
              END IF

              IF(WC.EQ.'BEST'.OR.WC.EQ.'NOTILT') THEN
                  CALL CAPFIX
                  RETURN
              END IF

              IF(WC.EQ.'GRID') THEN
                  CALL MTFGRID
                  RETURN
              END IF

              IF(WC.EQ.'VAR'.OR.WC.EQ.'APSTREHL'.OR.WC.EQ.'STREHL') THEN
                  CALL APSTREHL
                  LSF=.FALSE.
                  SPDEXT=.FALSE.
                  GSPDEXT=.FALSE.
                  CPFNEXT=.FALSE.
                  LDIF=.TRUE.
                  LDIF2=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'SPACE') THEN
                  CALL SPACER
                  RETURN
              END IF

              IF(WC.EQ.'CUTOFF') THEN
                  CALL CUTOFF
                  RETURN
              END IF

              IF(WC.EQ.'WAMAP') THEN
                  CALL WAMAP
                  RETURN
              END IF

              IF(WC.EQ.'AMAP') THEN
                  CALL AMAP
                  RETURN
              END IF

              IF(WC.EQ.'DR/FR') THEN
                  CALL DLRPFR
                  RETURN
              END IF

              IF(WC.EQ.'USERFUNC') THEN
                  CALL CALL_USERFUNC
                  RETURN
              END IF

              IF(WC.EQ.'USERSUBR') THEN
                  CALL CALL_USERSUBR
                  RETURN
              END IF

              IF(WC.EQ.'RAYLEIGH') THEN
                  CALL RAYLEIGH
                  RETURN
              END IF

              IF(WC.EQ.'WEIGHT') THEN
                  CALL WEIGHT
                  RETURN
              END IF

              IF(WC.EQ.'COST') THEN
                  CALL COST
                  RETURN
              END IF

              IF(WC.EQ.'DEFORM') THEN
                  CALL DEFIT
                  RETURN
              END IF

              IF(WC.EQ.'OUTFLAT') THEN
                  CALL OUTFLT
                  RETURN
              END IF

              IF(WC.EQ.'EXPUP') THEN
                  CALL EXPUP
                  RETURN
              END IF

              IF(WC.EQ.'RSPH') THEN
                  CALL RSPH
                  RETURN
              END IF

              IF(WC.EQ.'PRINT') THEN
                  CALL PRNLP
                  RETURN
              END IF

              IF(WC.EQ.'FITZERN') THEN
                  CALL OPDLOD
                  RETURN
              END IF

              IF(WC.EQ.'LISTOPD') THEN
                  CALL OPDLIS
                  RETURN
              END IF

              IF(WC.EQ.'LISTZERN') THEN
                  CALL WRTCOEFS
                  RETURN
              END IF

              IF(WC.EQ.'LISTREPT') THEN
                  CALL WRTREPORT
                  RETURN
              END IF

              IF(WC.EQ.'OIF') THEN
                  CALL OIF
                  RETURN
              END IF

              IF(WC.EQ.'XXF') THEN
                  CALL XXF
                  RETURN
              END IF

              IF(WC.EQ.'XXFF') THEN
                  CALL XXFF
                  RETURN
              END IF

              IF(WC.EQ.'IMAGEDIR') THEN
                  CALL IMAGEDIR
                  RETURN
              END IF

              IF(WC.EQ.'CAPFN') THEN
                  GRASET=.FALSE.
                  MSGSPD=.TRUE.
                  IF(DOSTREHL) MSGSPD=.FALSE.
                  CALL SPOT
                  RETURN
              END IF

              IF(WC.EQ.'CAPFNOUT') THEN
                  CALL OPDOUT
                  RETURN
              END IF

              IF(WC.EQ.'CAPGRID') THEN
                  CALL CAPGRID
                  RETURN
              END IF

              IF(WC.EQ.'CAPFNIN'.OR.WC.EQ.'CAPFNADD'.OR.WC.EQ.
     1        'CAPFNCLR') THEN
                  CALL OPDIN
                  RETURN
              END IF

              IF(WC.EQ.'FUNNAME') THEN
                  CALL FUNNAME
                  RETURN
              END IF

              IF(WC.EQ.'GLASSWV') THEN
                  CALL GLSWVL
                  RETURN
              END IF

              IF(WC.EQ.'LEPRT'.OR.WC.EQ.'LIS') THEN
                  CALL LEPRT
                  RETURN
              END IF

              IF(WC.EQ.'DO') THEN
                  CALL DODODO
                  RETURN
              END IF

              IF(WC.EQ.'THERM') THEN
                  IF(WQ.NE.'AIR'.AND.WQ.NE.'OXYGEN'.AND.WQ.NE.'NITROGEN'
     1            .AND.WQ.NE.'ETHANE'.AND.WQ.NE.'METHANE'.AND.WQ.NE.'HYDROGEN'
     2            .AND.WQ.NE.'ARGON'.AND.WQ.NE.'HELIUM'.AND.WQ.NE.'GAS') THEN
                      CALL THERM
                  ELSE
                      CALL THERMGAS
                  END IF
                  RETURN
              END IF

              IF(WC.EQ.'PRES') THEN
                  CALL PRES
                  RETURN
              END IF

              IF(WC.EQ.'STATS') THEN
                  CALL STATT
                  RETURN
              END IF

              IF(WC.EQ.'FOOT'.AND.WQ.NE.'GRID') THEN
                  GRASET=.FALSE.
                  MSG=.FALSE.
                  CALL QRRAY
                  MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'FOOTAREA') THEN
                  GRASET=.FALSE.
                  MSG=.FALSE.
                  CALL FOOTAREA
                  MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'FOOTSANG') THEN
                  GRASET=.FALSE.
                  MSG=.FALSE.
                  CALL FOOTSANG
                  MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'FOOT'.AND.WQ.EQ.'GRID') THEN
                  GRASET=.FALSE.
                  MSG=.FALSE.
                  CALL FTGRID
                  MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'RADUNITS'.OR.WC.EQ.'PLANK'.OR.WC.EQ.'WIEN'
     1        .OR.WC.EQ.'STEFBOLT') THEN
                  CALL SCHWARTZ
                  RETURN
              END IF

              IF(WC.EQ.'K0'.OR.WC.EQ.'CVG') THEN
                  CALL APOVERT
                  RETURN
              END IF

              IF(WC.EQ.'SCHOTT'.OR.WC.EQ.'OHARA'.OR.WC.EQ.'HOYA'.OR.
     1        WC.EQ.'CORNIN'.OR.WC.EQ.'CHANCE'.OR.WC.EQ.'GLCAT'.OR.WC.EQ.
     2        'MATL'.OR.WC.EQ.'RADHARD'.OR.WC.EQ.'USER'.OR.WC.EQ.'HIKARI'
     3        .OR.WC.EQ.'RUSSIAN'.OR.WC.EQ.'GLA'.OR.WC.EQ.'AIR'.OR.WC.EQ.
     4        'SCH2000') THEN

                  IF(WS(1:1).EQ.':') WS(1:80)=WS(2:80)
                  CALL GLSRIN
                  RETURN
              END IF

              IF(WC.EQ.'SPGR') THEN
                  CALL SPGR
                  RETURN
              END IF

              IF(WC.EQ.'PRICE') THEN
                  CALL PPRICE
                  RETURN
              END IF

              IF(WC.EQ.'AUTOFUNC') THEN
                  CALL AUTOFUNC
                  RETURN
              END IF

              IF(WC.EQ.'THM') THEN
                  CALL TTHM
                  RETURN
              END IF

              IF(WC.EQ.'MAKEAUTO'.OR.WC.EQ.'DFTYPE'.OR.WC.EQ.'DFDEL'
     1        .OR.WC.EQ.'FP'.OR.WC.EQ.'DFP'.OR.WC.EQ.'MONO'.OR.
     2        WC.EQ.'POLY'.OR.WC.EQ.'DFGRID'.OR.WC.EQ.'DFHEX') THEN
                  CALL MAKE_DEF_AUTO
                  RETURN
              END IF

              IF(WC.EQ.'LENDIR'.OR.WC.EQ.'MACDIR'.OR.WC.EQ.'TRADIR'
     1        .OR.WC.EQ.'PLTDIR'.OR.WC.EQ.'CHGMAC') THEN
                  CALL CHADIR
                  RETURN
              END IF

              IF(WC.EQ.'PFIND') THEN
                  GRASET=.FALSE.
                  F28=1
                  KILOPT=.FALSE.
                  MSG=.FALSE.
                  OPTMES=.FALSE.
                  CALL PFIND
                  OPTMES=.TRUE.
                  F28=0
                  MSG=.TRUE.
                  RETURN
              END IF

              IF(WC.EQ.'MFG') THEN
                  WRITE(OUTLYNE,6661) MFG(1:20)
                  CALL SHOWIT(1)
                  RETURN
              END IF

              IF(WC.EQ.'CATNUM') THEN
                  WRITE(OUTLYNE,6662) CATNUM(1:20)
                  CALL SHOWIT(1)
                  RETURN
              END IF

 6661         FORMAT('THE CURRENT MFG IS: ',A20)
 6662         FORMAT('THE CURRENT CATNUM IS: ',A20)

              IF(WC.EQ.'INR') THEN
                  CALL INRINR
                  RETURN
              END IF

              IF(WC.EQ.'INRD') THEN
                  CALL INRINRD
                  RETURN
              END IF

              IF(WC.EQ.'AST'.OR.WC.EQ.'DIST'.OR.WC.EQ.'FLDCV'
     1        .OR.WC.EQ.'FISHDIST') THEN
                  CALL FIELDABS
                  RETURN
              END IF

              IF(WC.EQ.'PLTAST'.OR.WC.EQ.'PLTDIST'.OR.WC.EQ.'PLTFLDCV'
     1        .OR.WC.EQ.'PLTFDIST') THEN
                  CALL PLTFIELD
                  RETURN
              END IF

              IF(WC.EQ.'PLTGOTF')  THEN
                  CALL PLTGOTF
                  RETURN
              END IF

              IF(WC.EQ.'PLTDOTF')  THEN
                  CALL PLTDOTF
                  RETURN
              END IF

              IF(WC.EQ.'PLTLSF')  THEN
                  CALL PLTLSF
                  RETURN
              END IF

              IF(WC.EQ.'PLTCHRSH')  THEN
                  CALL PLTCHRSH
                  RETURN
              END IF

              IF(WC.EQ.'PLTRED'.OR.WC.EQ.'PLTESED')  THEN
                  CALL PLTRED
                  RETURN
              END IF

              IF(WC.EQ.'GPXTX'.OR.WC.EQ.'GPXTY') THEN
                  CALL GPXT
                  RETURN
              END IF

              IF(WC.EQ.'BEA'.OR.WC.EQ.'BEAM') THEN
                  CALL GBEAM
                  RETURN
              END IF

              IF(WC.EQ.'RAYS'.OR.WC.EQ.'FIELDS') THEN
                  CALL QRRYFL
                  RETURN
              END IF

              IF(WC.EQ.'FLDS') THEN
                  CALL FLDS
                  RETURN
              END IF

              IF(WC.EQ.'VIEOVER') THEN
                  CALL VIEOVER
                  RETURN
              END IF

              IF(WC.EQ.'TFMOTION') THEN
                  CALL TFMOTION
                  RETURN
              END IF

              IF(WC.EQ.'FLDSARE') THEN
                  CALL FLDSARE
                  RETURN
              END IF

              IF(WC.EQ.'MACDMP') THEN
C       MACDMP LIVES IN AUTO.FOR
                  CALL MACDMP
                  RETURN
              END IF

              IF(WC.EQ.'TOLDMP') THEN
C       TOLDMP LIVES IN AUTO.FOR
                  CALL TOLDMP
                  RETURN
              END IF

              IF(WC.EQ.'FOCI') THEN
C       CALCULATES FOCI POSITIONS FOR CONICS
                  CALL FOCII
                  RETURN
              END IF

              IF(WC.EQ.'RTOD') THEN
C       CALCULATES R TO D CONVERSION FOR GRAZING SUPPORT
                  CALL RTOD
                  RETURN
              END IF

              IF(WC.EQ.'DTOR') THEN
C       CALCULATES D TO R CONVERSION FOR GRAZING SUPPORT
                  CALL DTOR
                  RETURN
              END IF

              IF(WC.EQ.'ETOCC') THEN
C       CALCULATES E TO CC CONVERSION FOR GRAZING SUPPORT
                  CALL ETOCC
                  RETURN
              END IF

              IF(WC.EQ.'CCTOE') THEN
C       CALCULATES CC TO E CONVERSION FOR GRAZING SUPPORT
                  CALL CCTOE
                  RETURN
              END IF

              IF(WC.EQ.'RHO') THEN
C       CALCULATES RHO POSITIONS FOR CONICS
                  CALL RRHHOO
                  RETURN
              END IF

              IF(WC.EQ.'STAMPD'.OR.WC.EQ.'STAMPT') THEN
C       SETS TIME AND DATE STAMPING FOR LI
                  CALL STAMPER
                  RETURN
              END IF

              IF(WC.EQ.'DINCR') THEN
C       SETS DEFAULT DINCRS FOR CLASSES OF VARIABLES
                  CALL DINCIT
                  RETURN
              END IF

              IF(WC.EQ.'NEWSEED') THEN
C       RESETS THE RANDOM NUMBER SEED
                  CALL NEWSEED
                  RETURN
              END IF

              IF(WC.EQ.'Z0') THEN
C       CALCULATES Z POSITIONS FOR CONICS
                  CALL ZZEEOO
                  RETURN
              END IF

              IF(WC.EQ.'PROMPT') THEN
C       SETS PROMPT FOR PROMPTED INPUT
                  CALL SETPMT
                  RETURN
              END IF

              IF(WC.EQ.'PREAD') THEN
C       DOES PROMPTED INPUT
                  CALL PREAD
                  RETURN
              END IF

              IF(WC.EQ.'CLEARREG') THEN
C       CLEARS GENERAL PURPOSE STORAGE REGISTERS (SETS THEM TO 0.0)
                  CALL CLEARREG
                  RETURN
              END IF

              IF(WC.EQ.'STOAX') THEN
C       STORE PROMPT READ VALUE INTO AN ALPH STORAGE REGISTER
                  CALL STOAX
                  RETURN
              END IF

              IF(WC.EQ.'ATON') THEN
C       CONVERTS STRING TO NUMBER
                  CALL MACATON
                  RETURN
              END IF

              IF(WC.EQ.'ID1')  RETURN
              IF(WC.EQ.'ID2')  RETURN
              IF(WC.EQ.'ID3')  RETURN
              IF(WC.EQ.'ID4')  RETURN
              IF(WC.EQ.'ID5')  RETURN
              IF(WC.EQ.'ID6')  RETURN
              IF(WC.EQ.'ID7')  RETURN
              IF(WC.EQ.'ID8')  RETURN
              IF(WC.EQ.'ID9')  RETURN
              IF(WC.EQ.'ID10') RETURN
              IF(WC.EQ.'IDTAG') THEN
C       CREATES THE LENS IDTAG
                  IF(IN.NE.5) CALL DOGTAG
                  RETURN
              END IF

              IF(WC.EQ.'APPEND'.OR.WC.EQ.'REPLACE') THEN
C       SETS APPEND/REPLACE FOR EDITTEXT, CARDTEXT AND PUNCHFILE.DAT
                  CALL APPREP
                  RETURN
              END IF

              IF(WC.EQ.'LIMRAYS') THEN
C       CALCULATES YZ AND XZ PLANE RAY EXTENTS
                  CALL DEROFF
                  CALL SIZES
                  RETURN
              END IF

              IF(WC.EQ.'BLKRAYS') THEN
C       CALCULATES YZ AND XZ PLANE RAY BLOCKAGES
                  CALL DEROFF
                  CALL SIZES2
                  REFEXT=.FALSE.
                  RETURN
              END IF

              IF(WC.EQ.'PSF') THEN
                  GRASET=.FALSE.
                  MSGSPD=.TRUE.
                  IF(DOSTREHL) MSGSPD=.FALSE.
                  CALL SPOT
                  RETURN
              END IF

              IF(WC.EQ.'PUPIL') THEN
                  GRASET=.FALSE.
                  MSGSPD=.TRUE.
                  IF(DOSTREHL) MSGSPD=.FALSE.
                  CALL SPOT
                  RETURN
              END IF

              IF(WC.EQ.'PIXEL'.OR.WC.EQ.'CENTROID') THEN
                  CALL PIXAR
                  RETURN
              END IF

              IF(WC.EQ.'STREAK') THEN
                  IF(WS(1:3).EQ.'YES') WS(1:3)='ON '
                  IF(WS(1:2).EQ.'NO')  WQ(1:3)='OFF'
                  CALL STREAK
                  RETURN

              END IF
              IF(WC.EQ.'PSFINT'.OR.WC.EQ.'PSFINTS') THEN
                  CALL PSFINT
                  RETURN
              END IF

              IF(WC.EQ.'GAUSS'.OR.WC.EQ.'NOGAUSS'.OR.
     1        WC.EQ.'OPWTA'.OR.WC.EQ.'OPWTAIM') THEN
                  RETURN
              END IF

              IF(WC.EQ.'SETCLAP') THEN
C       CALCULATES YZ AND XZ PLANE RAY EXTENTS AND SETS CLAPS
                  CALL DEROFF
                  CALL SETCLAP
                  RETURN
              END IF

              IF(WC.EQ.'BWRTSPOT') THEN
C       WRITES THE SPOT DIAGRAM TO THE FILE NAMED BY THE
C       QUALIFIER WORD.SPD
                  CALL BWRTSPOT
                  RETURN
              END IF

              IF(WC.EQ.'AWRTSPOT') THEN
C       WRITES THE SPOT DIAGRAM TO THE FILE NAMED BY THE
C       QUALIFIER WORD.SPD
                  CALL AWRTSPOT
                  RETURN
              END IF

              IF(WC.EQ.'BWRTSUM') THEN
C       WRITES THE SUMMED SPOT DIAGRAM TO THE FILE NAMED BY THE
C       QUALIFIER WORD.SPD
                  CALL BWRTSUM
                  RETURN
              END IF

              IF(WC.EQ.'AWRTSUM') THEN
C       WRITES THE SUMMED SPOT DIAGRAM TO THE FILE NAMED BY THE
C       QUALIFIER WORD.SPD
                  CALL AWRTSUM
                  RETURN
              END IF

              IF(WC.EQ.'TOMODEL') THEN
C       CONVERTS A CATALOG GLASS TO A MODEL GLASS
                  I=INT(W1)
                  CALL MODEL(I)
                  RETURN
              END IF

              IF(WC.EQ.'TOMYGLAS') THEN
C       CONVERTS A CATALOG GLASS TO A MYGLASS
                  I=INT(W1)
                  CALL MYGLASS(I)
                  RETURN
              END IF

              IF(WC.EQ.'VIG') THEN
C       TURN VIG SWITCH ON,OFF,YES NO
                  IF(WQ.EQ.'YES') WQ='ON'
                  IF(WQ.EQ.'NO')  WQ='OFF'
                  CALL VIGGER
                  RETURN
              END IF

              IF(WC.EQ.'SENSI') THEN
C       INITIATE SENSITIVITY ANALYSIS
                  F31=1
                  CALL SENSI(0)
                  F31=0
                  RETURN
              END IF

              IF(WC.EQ.'MXT'.OR.WC.EQ.'MNT'
     1        .OR.WC.EQ.'MPR'.OR.WC.EQ.'MNR') THEN
C       SET MIN AND MAX THICKNESS/RADIUS LIMITS FOR VARIABLES
                  CALL THRDLIM
                  RETURN
              END IF

              IF(WC.EQ.'INVSENSI') THEN
C       INITIATE INVERSE SENSITIVITY ANALYSIS
                  F31=1
                  CALL SENSI(1)
                  F31=0
                  RETURN
              END IF

              IF(WC.EQ.'MONTE') THEN
C       INITIATE MONTE-CARLO ANALYSIS
                  F31=1
                  CALL MONTE
                  F31=0
                  RETURN
              END IF

              IF(WC.EQ.'AVEC'.OR.WC.EQ.'BVEC'.OR.WC.EQ.'DOT'
     1        .OR.WC.EQ.'CROSS') THEN
                  CALL VECTOROP
                  RETURN
              END IF

              IF(WC.EQ.'PROGSIZE') THEN
                  CALL PROGSIZE
                  RETURN
              END IF

              IF(WC.EQ.'RAYERROR') THEN
                  CALL RERROR
                  RETURN
              END IF

              IF(WC.EQ.'SHUFFLE') THEN
                  IF(W1.LE.0.0D0) W1=20.0D0
                  N=INT(W1)
                  ALLOCATE (DARR(1:N,1:2),STAT=ALLOERR)
                  CALL SHUFFLE(N,DARR)
                  DEALLOCATE (DARR)
                  RETURN
              END IF

              FIELDER=.FALSE.
              DO II=1,200
                  CALL ITOAAA(II,AI4)
                  IF(II.LE.9.AND.WC(1:2).EQ.'F'//AI4(1:1).OR.
     1            II.GE.10.AND.I.LE.99.AND.WC(1:3).EQ.'F'//AI4(1:2).OR.
     2            II.GE.100.AND.I.LE.999.AND.WC(1:4).EQ.'F'//AI4(1:3)) THEN
                      FIELDER =.TRUE.
                      GO TO 945
                  END IF
              END DO
 945          CONTINUE
              IF(FIELDER) THEN
                  CALL FIELDS
                  RETURN
              END IF

              RAYER=.FALSE.
              DO II=1,5000
                  CALL ITOAAA(II,AI4)
                  IF(II.LE.9.AND.WC(1:8).EQ.'R'//AI4(1:1)//'      '.OR.
     1            II.GE.10.AND.II.LE.99.AND.WC(1:8).EQ.'R'//AI4(1:2)//'     '.OR.
     2            II.GE.100.AND.II.LE.999.AND.WC(1:8).EQ.'R'//AI4(1:3)//'    '.OR.
     3            II.GE.1000.AND.II.LE.5000.AND.WC(1:8).EQ.'R'//AI4(1:4)//
     4            '   ') THEN
                      RAYER=.TRUE.
                      GO TO 946
                  END IF
              END DO
 946          CONTINUE
              IF(RAYER) THEN
                  CALL RAYS
                  RETURN
              END IF

              IF(WC.EQ.'NSSDEL'.OR.WC.EQ.'NSSNEW'.OR.WC.EQ.'NSSUNITS'.OR.
     1        WC.EQ.'NSSWV'.OR.WC.EQ.'NSSWT'.OR.WC.EQ.'UNIVERSE'.OR.WC.EQ.
     2        'OBJECT'.OR.WC.EQ.'ONAME'.OR.WC.EQ.'NSSVERT'.OR.WC.EQ.'NSSDET'
     3        .OR.WC.EQ.'SNAME'.OR.WC.EQ.'NSSCOAT1'.OR.WC.EQ.'SGRT'.OR.WC.EQ.
     4        'SURFACE'.OR.WC.EQ.'SPROFILE'.OR.WC.EQ.'SGRTD'.OR.WC.EQ.
     5        'SPARAM'.OR.WC.EQ.'SPOS'.OR.WC.EQ.'SROT'.OR.WC.EQ.'NSSCOAT2'.OR.
     6        WC.EQ.'MEDIA1'.OR.WC.EQ.'MEDIA2'.OR.WC.EQ.'NSSN'.OR.WC.EQ.
     7        'NSSSAVE'.OR.WC.EQ.'NSSREST'.OR.WC.EQ.'NSSMINE'.OR.WC.EQ.
     8        'NSSMHIT'.OR.WC.EQ.'NSSSPLIT'.OR.WC.EQ.'NSSOBJ'.OR.WC.EQ.
     9        'NSSGRIDS'.OR.WC.EQ.'NSSGRIDR'.OR.WC.EQ.
     1        'NSSTRACE'.OR.WC.EQ.'SCLAP'.OR.WC.EQ.
     2        'SHOLE'.OR.WC.EQ.'NSSLIST'.OR.WC.EQ.'OBJMEDIA'.OR.WC.EQ.
     3        'NSSLENO'.OR.WC.EQ.'SCLEAR'.OR.WC.EQ.'SUNCLEAR'.OR.WC.EQ.
     4        'SBOUNDX'.OR.WC.EQ.'SBOUNDY'.OR.WC.EQ.'SBOUNDZ'.OR.WC.EQ.
     5        'NSSPOL'.OR.WC.EQ.'NSSINTER'.OR.WC.EQ.'NSSAPODR'.OR.WC.EQ.
     6        'NSSSPOT'.OR.WC.EQ.'NSSORINT'.OR.WC.EQ.'NSSLINK'.OR.
     7        WC.EQ.'NSSEOS'.OR.WC.EQ.'NSSREF') THEN
                  CALL NSSCALL
                  RETURN
              END IF
              OUTLYNE= 'INVALID CMD LEVEL COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
          END IF
          RETURN
      END


      SUBROUTINE DELPSF
          USE opsys
          USE GLOBALS
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          INTEGER ALLOERR
          PSFEXT=.FALSE.

          call os_delete('PSF.DAT')
          call os_delete('SPDPSF.DAT')

          DEALLOCATE(SPDPSF1,SPDPSF2,SPDPSF3,STAT=ALLOERR)
          DEALLOCATE(IPSF1,IPSF2,IPSF3,STAT=ALLOERR)
          RETURN
      END
