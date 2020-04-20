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


C       EIGHTEENTH FILE FOR LENS DATABASE MANAGER FILES

      SUBROUTINE FLUSHNEXT
          USE GLOBALS
          IMPLICIT NONE
C     ALL THE ARRAY VALUES FOR THE NEXT SURFACE ARE SET TO THEIR
C     PROPER STARTING DEFAULT VALUES
!      INTEGER I,J,K
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       INITIALIZE THE ARRAYS
          NUMHITS(SURF)=1
          SOLVE(1:9,SURF)=0.0D0
          PIKUP(1:6,SURF,1:PSIZ)=0.0D0
          ALENS(1:LSIZ,SURF)=0.0D0
          ALENS(46:50,SURF)=1.0D0
          ALENS(71:75,SURF)=1.0D0
          ALENS(76:85,SURF)=0.0D0
          ALENS(51:70,SURF)=0.0D0
          ALENS(127:128,SURF)=0.0D0
          ALENS(134:137,SURF)=0.0D0
          MULTCLAP(1:1000,1:3,SURF)=0.0D0
          MULTCOBS(1:1000,1:3,SURF)=0.0D0
          GLANAM(SURF,1)='             '
          GLANAM(SURF,2)='AIR          '
          LBL(SURF)(1:80)=' '
          RETURN
      END


C SUB LENIN.FOR
      SUBROUTINE LENIN
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE LENIN. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE SETUP OF A NEW LENS AFTER INITIALIZATION BY
C       LENNS.
C
!        CHARACTER WCOLD*8
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          REAL*8 RAL,RBE,CGAM,SGAM,RGAM
C
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          END IF
C       HERE IS WERE THE ? FOLLOWING A LENS ULPATE COMMAND
C       IS HANDLED
          IF(STI.EQ.1) THEN
              CALL LQUERY
              RETURN
          END IF
C
C       FIRST HANDLE EOS (END OF SUBFILE) IN THIS CASE THE LENS
C       SUBFILE.
C
          IF(WC.EQ.'EOS') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"EOS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              LNSTYP=1
              CALL LNSEOS
              RETURN
          ELSE
C       DON'T TERMINATE THE INPUT ROUTINE. ACCEPT INPUT DATA STARTING
C       AT THE CURRENT SURFACE OR ACCEPT SYSTEM AND TILE DATA.
C       HERE IS WHERE THE BRANCHING TO SUBROUTINES WHICH HANDLE
C       INPUT TO THE LENS MATRICES IS HANDELED.
C       FIRST THE NON-SURFACE DEPENDENT DATA IS STORED.
C
C       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
C       SPECIFIC STORAGE LOCATIONS FOR LENS DATA ITEMS.
C
C       COMMAND (LI) OR (LIC) AT 'LENS' LEVEL
              IF(WC.EQ.'LI'.OR.WC.EQ.'LIC') THEN
                  CALL SLI
                  RETURN
              END IF
C       COMMAND (INI) AT 'LENS' LEVEL
              IF(WC.EQ.'INI') THEN
                  CALL SINI
                  RETURN
              END IF
C       COMMAND (LTYPE) AT 'LENS' LEVEL
              IF(WC.EQ.'LTYPE') THEN
                  CALL SLTYPE
                  RETURN
              END IF
C       COMMAND (LABEL) OR (LBL) AT 'LENS' LEVEL
              IF(WC.EQ.'LBL'.OR.WC.EQ.'LABEL') THEN
                  CALL SLABEL
                  RETURN
              END IF
C       COMMAND (WV,W1 W2 W3 W4 W5)
              IF(WC.EQ.'WV') THEN
                  CALL SWV
                  RETURN
              END IF
C       COMMAND (WV2,W1 W2 W3 W4 W5)
              IF(WC.EQ.'WV2') THEN
                  CALL SWV2
                  RETURN
              END IF
C       COMMAND (UNITS)
              IF(WC.EQ.'UNITS') THEN
                  CALL SUNITS
                  RETURN
              END IF
C       COMMAND (CW,PCW OR SCW)
              IF(WC.EQ.'CW'.OR.WC.EQ.'PCW'.OR.
     1        WC.EQ.'SCW') THEN
                  CALL SCW
                  RETURN
              END IF
C       COMMAND (SAY OR SAX)
              IF(WC.EQ.'SAY'.OR.WC.EQ.'SAX') THEN
                  CALL SSA
                  RETURN
              END IF
C       COMMAND (WRY OR WRX)
              IF(WC.EQ.'WRY'.OR.WC.EQ.'WRX') THEN
                  CALL SWR
                  RETURN
              END IF
C       COMMAND (BDY OR BDX)
              IF(WC.EQ.'BDY'.OR.WC.EQ.'BDX') THEN
                  CALL SBD
                  RETURN
              END IF
C       COMMAND (NAOY OR NAOX)
              IF(WC.EQ.'NAOY'.OR.WC.EQ.'NAOX') THEN
                  CALL SNAO
                  RETURN
              END IF
C       COMMAND (FNOY OR FNOX)
              IF(WC.EQ.'FNOY'.OR.WC.EQ.'FNOX') THEN
                  CALL SFNO
                  RETURN
              END IF
C       COMMAND (SCY,Y0 Y1 AND SCY FANG,U0 U1)
              IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
                  CALL SSC
                  RETURN
              END IF
C       COMMAND (PXIM AND PYIM)
              IF(WC.EQ.'PXIM'.OR.WC.EQ.'PYIM') THEN
                  CALL PXYIM
                  RETURN
              END IF
C       COMMAND (RXIM AND RYIM)
              IF(WC.EQ.'RXIM'.OR.WC.EQ.'RYIM') THEN
                  CALL RXYIM
                  RETURN
              END IF
C       COMMAND (RD) OR (CV)
              IF(WC.EQ.'RD'.OR.WC.EQ.'CV') THEN
                  CALL SCV
                  RETURN
              END IF
C       COMMAND (INR)
              IF(WC.EQ.'INR') THEN
                  CALL INRINR
                  RETURN
              END IF
C       COMMAND (MFG)
              IF(WC.EQ.'MFG') THEN
                  MFG=WS
                  RETURN
              END IF
C       COMMAND (CATNUM)
              IF(WC.EQ.'CATNUM') THEN
                  CATNUM=WS
                  RETURN
              END IF
C       COMMAND (TH)
              IF(WC.EQ.'TH') THEN
                  CALL STH
                  IF(SYSTEM1(63).NE.0.0D0.AND.DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                      SYSTEM1(63)=0.0D0
                      OUTLYNE='OBJECT THICKNESS EQUALS OR EXCEEDS 1.0D+10 LENS UNITS'
                      CALL SHOWIT(1)
                      OUTLYNE='TELECENTRIC AIMING HAS BEEN SHUT OFF'
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF
              IF(WC.EQ.'THM') THEN
                  CALL STHM
                  RETURN
              END IF
              IF(WC.EQ.'PRICE') THEN
                  CALL SPRICE
                  RETURN
              END IF
              IF(WC.EQ.'AUTOFUNC') THEN
                  CALL SAUTOFUNC
                  RETURN
              END IF
C       COMMAND (CC OR CCTOR)
              IF(WC.EQ.'CC'.OR.WC.EQ.'CCTOR') THEN
                  CALL SCC
                  RETURN
              END IF
C
C       COMMAND (DEFORM)
              IF(WC.EQ.'DEFORM') THEN
                  IF(ALENS(34,SURF).NE.0.0D0) THEN
                      OUTLYNE=
     1                'A SPECIAL SURFACE MAY NOT BE DEFINED AS A DEFORMABLE SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL DEFIT
                  RETURN
              END IF
C
C       COMMAND (AIR)
              IF(WC.EQ.'AIR') THEN
                  CALL SAIR
                  CALL FLUSHNEXT
                  RETURN
              END IF
C       COMMAND (GRT,GRO,GRS,GRX,GRY,GRZ)
              IF(WC.EQ.'GRT'.OR.WC.EQ.'GRO'.OR.WC.EQ.'GRS'.OR.
     1        WC.EQ.'GRX'.OR.WC.EQ.'GRY'.OR.WC.EQ.'GRZ') THEN
                  CALL SGRAT
                  RETURN
              END IF
C       COMMAND (NODUM)
              IF(WC.EQ.'NODUM') THEN
                  CALL SNODUM
                  RETURN
              END IF
C       COMMAND (MODEL [INDENTIFICATION],N V])
              IF(WC.EQ.'MODEL') THEN
                  IF(STI.EQ.1) THEN
                      WRITE(OUTLYNE,100) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
                      CALL SHOWIT(0)
                      RETURN
                  END IF
                  CALL SGLASS
                  CALL FLUSHNEXT
                  RETURN
              END IF
C       COMMAND (GLASS [INDENTIFICATION],N1 N2 N3 N4 N5])
              IF(WC.EQ.'GLASS') THEN
                  IF(STI.EQ.1) THEN
                      WRITE(OUTLYNE,100) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
                      CALL SHOWIT(0)
 100                  FORMAT('GLASS TYPE FOR SURFACE # ',I3,' IS: ',
     1                A8,1X,A8)
                      RETURN
                  END IF
                  CALL SGLASS
                  CALL FLUSHNEXT
                  RETURN
              END IF
C       COMMAND (N6,W1, TO N10,W1)
              IF(WC.EQ.'N6'.OR.WC.EQ.'N7'.OR.WC.EQ.'N1'.OR.WC.EQ.'N2'.OR.
     2        WC.EQ.'N3'.OR.WC.EQ.'N8'.OR.WC.EQ.'N9'.OR.WC.EQ.'N10'.OR.WC
     3        .EQ.'N4'.OR.WC.EQ.'N5') THEN
                  CALL RNCHG
                  RETURN
              END IF
C       COMMAND ((GLASS CAT NAME) [INDENTIFICATION])
              IF(WC.EQ.'SCHOTT'.OR.WC.EQ.'SCH2000'.OR.
     1        WC.EQ.'OHARA'.OR.WC.EQ.'CHANCE'.OR.
     2        WC.EQ.'CORNIN'.OR.WC.EQ.'MATL'.OR.
     6        WC.EQ.'HOYA'.OR.WC.EQ.'GLCAT'.OR.WC.EQ.'RADHARD'
     7        .OR.WC.EQ.'USER'.OR.WC.EQ.'RUSSIAN'.OR.WC.EQ.'HIKARI'
     7        .OR.WC.EQ.'GLA')THEN
                  CALL GLSCAT
                  CALL FLUSHNEXT
                  RETURN
              END IF
C       COMMAND (MULTCLAP)
              IF(WC.EQ.'MULTCLAP') THEN
                  CALL MULT_CLAP
                  RETURN
              END IF
C       COMMAND (MULTCOBS)
              IF(WC.EQ.'MULTCOBS') THEN
                  CALL MULT_COBS
                  RETURN
              END IF
C       COMMAND (SPIDER)
              IF(WC.EQ.'SPIDER') THEN
                  CALL SSPIDER
                  RETURN
              END IF
C       COMMAND (INDEX OR VNUM OR DPART)
              IF(WC.EQ.'INDEX'.OR.WC.EQ.'VNUM'.OR.WC.EQ.'DPART') THEN
                  CALL FICTCHG
                  RETURN
              END IF
C       COMMAND ((GLASS CAT NAME) [INDENTIFICATION])
              IF(WC.EQ.'SCHOTT'.OR.WC.EQ.'SCH2000'.OR.
     1        WC.EQ.'OHARA'.OR.WC.EQ.'CHANCE'.OR.
     2        WC.EQ.'CORNIN'.OR.WC.EQ.'MATL'.OR.
     6        WC.EQ.'HOYA'.OR.WC.EQ.'GLCAT'.OR.WC.EQ.'RADHARD'
     7        .OR.WC.EQ.'USER'.OR.WC.EQ.'RUSSIAN'.OR.WC.EQ.'HIKARI'
     7        .OR.WC.EQ.'GLA')THEN
                  CALL GLSCAT
                  CALL FLUSHNEXT
                  RETURN
              END IF
C       COMMAND (ARRAY)
              IF(WC.EQ.'ARRAY') THEN
                  CALL SARRAY(.FALSE.)
                  RETURN
              END IF
C       COMMAND (ASPH,D E F G AC)
              IF(WC.EQ.'ASPH'.OR.WC.EQ.'ASPH2') THEN
                  CALL SASPH
                  RETURN
              END IF
C       COMMAND (TASPH,D E F G)
              IF(WC.EQ.'TASPH') THEN
                  CALL STASPH
                  RETURN
              END IF
C       COMMAND (REFS)
              IF(WC.EQ.'REFS') THEN
                  CALL SREFS
                  RETURN
              END IF
C       COMMAND (ASTOP)
              IF(WC.EQ.'ASTOP') THEN
                  CALL SASTOP
                  RETURN
              END IF
C       COMMAND (REFL)
              IF(WC.EQ.'REFL') THEN
                  CALL SREFL
                  CALL FLUSHNEXT
                  RETURN
              END IF
C       COMMAND (REFLTIRO)
              IF(WC.EQ.'REFLTIRO') THEN
                  CALL SREFL
                  CALL FLUSHNEXT
                  RETURN
              END IF
C       COMMAND (REFLTIR)
              IF(WC.EQ.'REFLTIR') THEN
                  CALL SREFL
                  CALL FLUSHNEXT
                  RETURN
              END IF
C       COMMAND (PERFECT)
              IF(WC.EQ.'PERFECT') THEN
                  CALL SPERFECT
                  CALL FLUSHNEXT
                  RETURN
              END IF
C       COMMAND (REAL OR PARAX)
              IF(WC.EQ.'REAL'.OR.WC.EQ.'PARAX') THEN
                  CALL SURF_TYPE
                  RETURN
              END IF
C       COMMAND (IDEAL)
              IF(WC.EQ.'IDEAL') THEN
                  CALL SIDEAL
                  CALL FLUSHNEXT
                  RETURN
              END IF
C       COMMAND (MODE)
              IF(WC.EQ.'MODE') THEN
                  CALL SMODE
                  RETURN
              END IF
C       COMMAND (PIVAXIS)
              IF(WC.EQ.'PIVAXIS') THEN
                  CALL PIVAXIS
                  RETURN
              END IF
C       COMMAND (SPTWT)
              IF(WC.EQ.'SPTWT'.OR.WC.EQ.'SPTWT2') THEN
                  CALL SSPTWT
                  RETURN
              END IF
C       COMMAND (CLAP OR COBS)
              IF(WC.EQ.'CLAP'.OR.WC.EQ.'COBS') THEN
                  CALL SAPE
C       CLAP
                  IF(ALENS(9,SURF).EQ.6.0D0)  CALL LOADIPOLY(1,SURF)
C       CLAP ERASE
                  IF(ALENS(51,SURF).EQ.6.0D0) CALL LOADIPOLY(2,SURF)
C       COBS
                  IF(ALENS(16,SURF).EQ.6.0D0) CALL LOADIPOLY(3,SURF)
C       COBS ERASE
                  IF(ALENS(61,SURF).EQ.6.0D0) CALL LOADIPOLY(4,SURF)
                  RETURN
              END IF
C       COMMAND (AC,AD,AE,AF,AG,ADTOR,AETOR,AFTOR,AGTOR)
C       COMMAND (AH,AI,AJ,AK,AL)
              IF(WC.EQ.'AC'.OR.WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.
     1          'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'
     2          .OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR'.OR.WC.EQ.'AH'.OR.
     3        WC.EQ.'AJ'.OR.WC.EQ.'AI'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL') THEN
                  CALL SDEFG
                  RETURN
              END IF
C       COMMAND (PIVX.OR.PIVY.OR.PIVZ)
              IF(WC.EQ.'PIVX'.OR.WC.EQ.'PIVY'.OR.WC.EQ.'PIVZ') THEN
                  CALL SPIVXYD
                  RETURN
              END IF
C       COMMAND (XD.OR.YD.OR.ZD)
              IF(WC.EQ.'XD'.OR.WC.EQ.'YD'.OR.WC.EQ.'ZD') THEN
                  CALL SXYD
                  RETURN
              END IF
C       COMMAND (ALPHA, BETA OR GAMMA)
              IF(WC.EQ.'ALPHA'.OR.WC.EQ.'BETA'.OR.WC.EQ.
     1        'GAMMA') THEN
                  CALL SANGLE
C     USE THE CODE-V DEFAULT GAMMA ONLY IF IT IS NOT EXPLICITLY INPUT
                  IF(ALENS(25,SURF).EQ.4.0D0) THEN
C       TILT BEN
                      RAL=(PII/180.0D0)*ALENS(26,SURF)
                      RBE=(PII/180.0D0)*ALENS(27,SURF)
                      CGAM=(DCOS(RAL)+DCOS(RBE))/(1.0D0+(DCOS(RAL)*DCOS(RBE)))
                      SGAM=-(DSIN(RAL)*DSIN(RBE))/(1.0D0+(DCOS(RAL)*DCOS(RBE)))
                      IF(CGAM.LT.-1.0D0) CGAM=-1.0D0
                      IF(CGAM.GT.+1.0D0) CGAM=+1.0D0
                      IF(SGAM.GE.0.0D0) RGAM=DABS(DACOS(CGAM))
                      IF(SGAM.LT.0.0D0) RGAM=-DABS(DACOS(CGAM))
                      ALENS(28,SURF)=RGAM*180.0D0/PII
                      ALENS(120,SURF)=RGAM*180.0D0/PII
                  END IF
                  RETURN
              END IF
C       COMMAND (GDX.OR.GDY.OR.GDZ)
              IF(WC.EQ.'GDX'.OR.WC.EQ.'GDY'.OR.WC.EQ.'GDZ') THEN
                  CALL SGXYD
                  RETURN
              END IF
C       COMMAND (GALPHA, GBETA OR GGAMMA)
              IF(WC.EQ.'GALPHA'.OR.WC.EQ.'GBETA'.OR.WC.EQ.
     1        'GGAMMA') THEN
                  CALL SGANGLE
                  RETURN
              END IF
C       COMMAND (DEC,YD XD ZD)
              IF(WC.EQ.'DEC') THEN
                  CALL SDEC
                  RETURN
              END IF
C       COMMAND (PIVOT,X Y Z)
              IF(WC.EQ.'PIVOT') THEN
                  CALL SPIVOT
                  RETURN
              END IF
C       COMMAND (PIVAXIS)
              IF(WC.EQ.'PIVAXIS') THEN
                  CALL SPIVAX
                  RETURN
              END IF
C       COMMAND (TILT,ALPHA BETA GAMMA OR RTILT,ALPHA BETA GAMMA
C               OR TILT AUTO,ALPHA BETA GAMMA OR TILT AUTOM,ALPHA
C               BETA GAMMA)
              IF(WC.EQ.'TILT'.AND.WQ.NE.'AUTOD'.OR.
     1        WC.EQ.'TILT'.AND.WQ.NE.'AUTOD'
     1        .OR.WC.EQ.'RTILT') THEN
                  CALL STILT
                  RETURN
              END IF
C       COMMAND (YTORIC OR XTORIC)
              IF(WC.EQ.'YTORIC'.OR.WC.EQ.'XTORIC') THEN
                  CALL STORIC
                  RETURN
              END IF
C       COMMAND (CVTOR OR RDTOR)
              IF(WC.EQ.'CVTOR'.OR.WC.EQ.'RDTOR') THEN
                  CALL SCVR
                  RETURN
              END IF
C       COMMAND (RAYERROR) THEN
              IF(WC.EQ.'RAYERROR') THEN
                  CALL RAYERROR
                  RETURN
              END IF
C       COMMAND (ROO) THEN
              IF(WC.EQ.'ROO') THEN
                  CALL ROO
                  RETURN
              END IF
C       COMMAND (CCR) THEN
              IF(WC.EQ.'CCR') THEN
                  CALL CCR
                  RETURN
              END IF
C       COMMAND (COATING)
              IF(WC.EQ.'COATING') THEN
                  CALL SCOATING
                  RETURN
              END IF
C       COMMAND (SPGR)
              IF(WC.EQ.'SPGR') THEN
                  CALL SPGR
                  RETURN
              END IF
C       COMMAND (FOOTBLOK)
              IF(WC.EQ.'FOOTBLOK') THEN
                  CALL BLKHEAD
                  RETURN
              END IF
C       COMMANDS (PY,PX,PCY,PCX,CAY,CAX)
              IF(WC.EQ.'PY'.OR.WC.EQ.'PX'.OR.WC.EQ.'PCY'
     1        .OR.WC.EQ.'PCX'.OR.WC.EQ.'CAY'.OR.WC.EQ.'CAX') THEN
                  CALL THSOLV
                  IF(SYSTEM1(63).NE.0.0D0.AND.DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                      SYSTEM1(63)=0.0D0
                      OUTLYNE='OBJECT THICKNESS EQUALS OR EXCEEDS 1.0D+10 LENS UNITS'
                      CALL SHOWIT(1)
                      OUTLYNE='TELECENTRIC AIMING HAS BEEN SHUT OFF'
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF
C       COMMANDS (APY,PIY,PUY,APCY,PICY,PUCY,COCY,OR
C                 APX,PIX,PUX,APCX,PICX,PUCX AND COCX)
              IF(WC.EQ.'APY'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PUY'
     1        .OR.WC.EQ.'APCY'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY'.OR.
     2        WC.EQ.'COCY'.OR.WC.EQ.'APX'.OR.WC.EQ.'PIX'.OR.
     3        WC.EQ.'PUX'.OR.WC.EQ.'APCX'.OR.WC.EQ.'PICX'.OR.
     4        WC.EQ.'PUCX'.OR.WC.EQ.'COCX') THEN
                  CALL CVSOLV
                  RETURN
              END IF
C       COMMAND (PIKUP)
              IF(WC.EQ.'PIKUP') THEN
                  CALL SPIKUP
                  IF(SYSTEM1(63).NE.0.0D0.AND.DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                      SYSTEM1(63)=0.0D0
                      OUTLYNE='OBJECT THICKNESS EQUALS OR EXCEEDS 1.0D+10 LENS UNITS'
                      CALL SHOWIT(1)
                      OUTLYNE='TELECENTRIC AIMING HAS BEEN SHUT OFF'
                      CALL SHOWIT(1)
                  END IF
                  IF(WC.EQ.'PIKUP'.AND.WQ.EQ.'GLASS') CALL FLUSHNEXT
                  RETURN
              END IF
          END IF
C
          RETURN
      END
C SUB LENADD.FOR
      SUBROUTINE LENADD
C
          IMPLICIT NONE
C
C       IT IS WITHIN THIS SUBROUTINE THAT MAKE LENADD WORK
C
          CHARACTER WCOLD*8,DATA*80,FN*10,AN*3,AI*3,AI4*4
C
          INTEGER N3,FLASTP,FLREFS,LIBNUM,II,N,J,I,SUR,KPIK
     1    ,ALLOERR,JK,TOTSUR,IS,OLDOUT,OLDIN,IREND
C
          REAL*8 ASTOP2,REFS2,
     1    IMAGE1,IMAGE2,UNI1,UNI2
C
          LOGICAL EXISJK

          LOGICAL ITERROR
C
          INTEGER AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
C
          CHARACTER LICA*80,LIA*80,GLANMA*13,ALBL*80,LLTYPEA*80,INNIA*80
C
          REAL*8 SYSA,ALENA,SLVA,PIKA,FT01A,MULTCLAPA,MULTCOBSA
C
          REAL*8 AIPOLYX,AIPOLYY
C
          DIMENSION SYSA(:),ALENA(:,:),SLVA(:,:),PIKA(:,:,:),
     1    FT01A(:,:),LICA(:),GLANMA(:,:),ALBL(:),MULTCLAPA(:,:,:)
     2    ,MULTCOBSA(:,:,:),AIPOLYX(:,:,:),AIPOLYY(:,:,:)
C
          ALLOCATABLE :: SYSA,ALENA,SLVA,PIKA,FT01A,LICA,GLANMA,ALBL
     1    ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
C       RESTORE TO OLD OBJ,REF AND IMAGE SURFACES
          CALL RESSUR
C
C       LENADD TAKES NO STRING OR QUALIFIER INPUT
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"LENADD" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       LENADD TAKES NO NUMERIC WORD #4 OR #5 INPUT
          IF(S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"LENADD" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       LENADD REQUIRES EXPLICIT NUMERIC WORD #1 INPUT
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"LENADD" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       LENADD REQUIRES POSITIVE, NON-ZERO NUMERIC WORD #1 INPUT
          IF(W1.LE.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        '"LENADD" REQUIRES POSITIVE, NON-ZERO NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       IF NW2 OR NW3 ARE NOT DEFAULT
          FLASTP=1
          IF(DF2.EQ.0) FLASTP=2
          FLREFS=1
          IF(DF3.EQ.0) FLREFS=2
C       NOW FIND OUT IF THE LENS REFERED TO IN NW1 EXISTS
C       IN THE LENS LIBRARY
          LIBNUM=INT(W1)
C
C       DOES THE LIBRARY EXIST?
          EXISJK=.FALSE.
          INQUIRE(FILE=LIBLEN//'LIB.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"LENS LIBRARY DOES NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ILF" AND "PROCEED"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'BEFORE USING "LENADD"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          OPEN(UNIT=22,ACCESS='DIRECT',FILE=LIBLEN//'LIB.DAT',
     1    FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
          READ(UNIT=22,REC=INT(W1)) II,DATA
          IF(II.EQ.0) THEN
C       FOUND AN EMPTY SLOT
              WRITE(OUTLYNE,*)'LENS LIBRARY FILE NO. ',INT(W1),' IS EMPTY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'"LENADD" CAN NOT BE USED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     ALLOCATE
          AM1=SSIZ
          AM2=LSIZ
          AM3=MAXSUR
          AM4=PSIZ
          AM5=1
          AM6=9
          AM7=6
          AM8=96
          AM9=2
          AM10=0
          DEALLOCATE(SYSA,ALENA,SLVA
     1    ,PIKA,FT01A,LICA,AIPOLYX,AIPOLYY,
     2    GLANMA,ALBL,MULTCLAPA,MULTCOBSA,STAT=ALLOERR)
          ALLOCATE(SYSA(AM1),ALENA(AM2,AM10:AM3),SLVA(AM5:AM6,AM10:AM3)
     1    ,PIKA(AM7,AM10:AM3,AM4),FT01A(AM8,AM10:AM3),LICA(AM6),
     2    GLANMA(AM10:AM3,AM9),ALBL(AM10:AM3),MULTCLAPA(1:1000,1:3,0:AM3),
     3    AIPOLYX(1:200,AM10:AM3,1:4),AIPOLYY(1:200,AM10:AM3,1:4),
     4    mULTCOBSA(1:1000,1:3,0:AM3),STAT=ALLOERR)
C       BEFORE READING IN THE LENS LIBRARY, WE MUST DO SOME
C       THINGS WITH THE CURRENT LENS.
          SYSA(1:AM1)=0.0D0
          LICA(1:AM6)='                  '
          ALBL(AM10:AM3)='                               '
          ALENA(1:AM2,AM10:AM3)=0.0D0
          AIPOLYX(1:200,AM10:AM3,1:4)=0.0D0
          AIPOLYY(1:200,AM10:AM3,1:4)=0.0D0
          MULTCLAPA(1:1000,1:3,0:AM3)=0.0D0
          MULTCOBSA(1:1000,1:3,0:AM3)=0.0D0
          SLVA(AM5:AM6,AM10:AM3)=0.0D0
          PIKA(1:AM7,AM10:AM3,1:AM4)=0.0D0
          FT01A(1:AM8,AM10:AM3)=0.0D0
          GLANMA(AM10:AM3,1:AM9)='             '
C
          W2=0.0D0
          W3=0.0D0
          SN=0
          SST=0
          DF2=1
          DF3=1
C       NOW DELETE ALL BUT THE MAIN CFG
          SYSTEM1(50)=1.0D0
          SYSTEM1(56)=1.0D0
C
          IMAGE1=SYSTEM1(20)
          UNI1=SYSTEM1(6)
C
C       NOW SAVE LENS 1, CURRENT CONFIG, TO THE ACHIEVE LENS STORAGE
          CALL CTOA(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1    ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA,
     2    MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
C
C       OK WE ARE DONE WITH THE CURRENT LENS, IT IS IN ARCHIEVE STORAGE.
C
C     GET RID OF "LAST SURFACE" GLASS NAMES
          IS=INT(SYSTEM1(20))
          GLANAM(IS,1:2)(1:13)=GLANAM(IS-1,1:2)(1:13)
          GLANMP(IS,1:2)(1:13)=GLANMP(IS-1,1:2)(1:13)
          GLANMA(IS,1:2)(1:13)=GLANMA(IS-1,1:2)(1:13)
C
C       NOW GET THE LIBRARY
C       LENS
          J=INT(SYSTEM1(20))
C
C       NOW DETERMINE WHICH OF THE 999 LENS LIBRARY FILES
C       TO USE
          N=LIBNUM
          CALL CCOONN(N,AN)
          IF(N.GT.0.AND.N.LE.9) FN='LIB00'//AN(3:3)//'.DAT'
          IF(N.GT.9.AND.N.LE.99) FN='LIB0'//AN(2:3)//'.DAT'
          IF(N.GT.99.AND.N.LE.999) FN='LIB'//AN(1:3)//'.DAT'
          OPEN(UNIT=22,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=LIBLEN//FN
     2    ,STATUS='UNKNOWN')
C**********************************************************
C       RE-INITIALIZE OPERATING CONDITIONS
          IF(OPTM_INIT.EQ.1) THEN
              WCOLD=WC
              WC='OPCON'
              CALL PM
              WC=WCOLD
          END IF
C
C     SET GLOBE TO FALSE
          GLOBE=.FALSE.
C     SET OPCNT,VBCNT AND DEREXT TO CORRECT VALUES
          ISTOP(1:5)=.FALSE.
          ISCOMP(1:5)=.FALSE.
          ISCRIT(1:5)=.FALSE.
          TVBCNT=0
          IF(OPTM_INIT.EQ.1) THEN
              OPCNT=0
              VBCNT=0
              DEREXT=.FALSE.
              ITERROR=.FALSE.
              CALL ITER(0,0,ITERROR)
              FMTEXT=.FALSE.
              FMTFLG=.FALSE.
          END IF
C
C**********************************************************
C       NOW INPUT LENS FROM LIBXXX.DAT
C
C       LOAD INITIAL LENS DATA
          OLDIN=IN
          OLDOUT=OUT
          IN=22
          OUT=22
          REWIND(UNIT=22)
 3141     READ(UNIT=22,FMT=100,END=8888,ERR=8888) INPUT
          IF(INPUT(1:3).EQ.'OUT') GO TO 3141
          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
          IF(OPTM_INIT.EQ.0) THEN
              IF(INPUT(1:3).EQ.'VAR'.OR.INPUT(1:3).EQ.'MER') GO TO 8889
          END IF
          IF(INPUT(1:8).EQ.'FLDSRAYS') THEN
              IREND=500
              IF(INPUT(10:13).EQ.' 500') IREND=500
              IF(INPUT(10:13).EQ.'1000') IREND=1000
              IF(INPUT(10:13).EQ.'1500') IREND=1500
              IF(INPUT(10:13).EQ.'2000') IREND=2000
              IF(INPUT(10:13).EQ.'2500') IREND=2500
              IF(INPUT(10:13).EQ.'3000') IREND=3000
              IF(INPUT(10:13).EQ.'3500') IREND=3500
              IF(INPUT(10:13).EQ.'4000') IREND=4000
              IF(INPUT(10:13).EQ.'4500') IREND=4500
              IF(INPUT(10:13).EQ.'5000') IREND=5000
              DO I=1,200
                  READ(22,*) AI,FIELDY(I),FIELDX(I),
     1            FIELDZ(I),N3
                  FIELDW(I)=DBLE(N3)
                  IF(FIELDW(I).EQ.0.0D0) THEN
                      FIELDW(I)=SYSTEM1(11)
                  END IF
              END DO
              DO I=1,IREND
                  READ(22,*,ERR=8888,END=8888) AI4,RAYY(I),RAYX(I),N3
                  RAYW(I)=DBLE(N3)
                  IF(RAYW(I).EQ.0.0D0) THEN
                      IF(I.GE.1.AND.I.LE.41) RAYW(I)=SYSTEM1(11)
                      IF(I.GE.42.AND.I.LE.82) RAYW(I)=SYSTEM1(7)
                      IF(I.GE.83.AND.I.LE.123) RAYW(I)=SYSTEM1(8)
                  END IF
              END DO
          ELSE
          END IF
          IF(INPUT(1:1).EQ.'.'.OR.
     1    INPUT(1:1).EQ.'1'.OR.
     1    INPUT(1:1).EQ.'2'.OR.
     1    INPUT(1:1).EQ.'3'.OR.
     1    INPUT(1:1).EQ.'4'.OR.
     1    INPUT(1:1).EQ.'5'.OR.
     1    INPUT(1:1).EQ.'6'.OR.
     1    INPUT(1:1).EQ.'7'.OR.
     1    INPUT(1:1).EQ.'8'.OR.
     1    INPUT(1:1).EQ.'9'.OR.
     1    INPUT(1:1).EQ.'0'
     1    .OR.INPUT(1:8).EQ.'FLDSRAYS') THEN
          ELSE
              CALL PROCES
          END IF
          GO TO 3141
C     LIBRARY LENS HAS BEEN LOADED
 8888     BACKSPACE(UNIT=22)
          REWIND (UNIT=22)
          CLOSE (UNIT=22,STATUS='KEEP')
 8889     CALL CLOSE_FILE(22,1)
          OUT=OLDOUT
          IN=OLDIN
          LIBGET=.FALSE.
C
C       CURRENT LENS IS LOADED INTO THE LENS DATA STORAGE.
C
          CALL CLOSE_FILE(22,1)
C
          UNI2=SYSTEM1(6)
          ASTOP2=SYSTEM1(26)
          REFS2=SYSTEM1(25)
          IMAGE2=SYSTEM1(20)
C
C       ARE THE LENSES OF SIMILAR UNITS
          IF(UNI1.NE.UNI2)THEN
              OUTLYNE='LENSES 1 AND 2 HAVE DIFFERENT UNITS'
              CALL SHOWIT(1)
              OUTLYNE='"LENADD" NOT PERFORMED'
              CALL SHOWIT(1)
              CALL MACFAL
C       HERE WE FIX THE LENS, PUT ARCHIEVE LENS BACK TO CURRENT LENS
              CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10,
     1        SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA,
     2        MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
C       HERE WE FIX THE LENS, PUT PERM LENS BACK TO CURRENT LENS
              CALL CTOP
              DEALLOCATE(SYSA,ALENA,SLVA
     1        ,PIKA,FT01A,LICA,AIPOLYX,AIPOLYY,
     2        GLANMA,ALBL,MULTCLAPA,MULTCOBSA,STAT=ALLOERR)
              RETURN
          END IF
C       ARE THE LENSES OF SIMILAR SURFACE ANGLE MODES
C       NOW CHECK THAT THE COMBINED LENS IS NOT TOO BIG
          TOTSUR=INT(IMAGE2)+INT(IMAGE1)
          IF(TOTSUR.GT.MAXSUR) THEN
              OUTLYNE='NEW LENS WILL HAVE TOO MANY SURFACES'
              CALL SHOWIT(1)
              OUTLYNE='"LENADD" NOT PERFORMED'
              CALL SHOWIT(1)
              CALL MACFAL
C       HERE WE FIX THE LENS, PUT ARCHIEVE LENS BACK TO CURRENT LENS
              CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1        ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2        ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
C       HERE WE FIX THE LENS, PUT PERM LENS BACK TO CURRENT LENS
              CALL CTOP
              DEALLOCATE(SYSA,ALENA,SLVA
     1        ,PIKA,FT01A,LICA,AIPOLYX,AIPOLYY,
     2        GLANMA,ALBL,MULTCLAPA,MULTCOBSA,STAT=ALLOERR)
              RETURN
          END IF
C       PROCEED WITH THE ADD
C
C       TAKE CARE OF LENS 1 CONDITIONING
C       DELETE LENS 1 CONFIGS
C       NOW DELETE ALL BUT THE MAIN CFG
          SYSA(50)=1.0D0
          SYSA(56)=1.0D0
C       GET RID OF LENS 1 SOLVES
          ALENA(33,0:MAXSUR)=0.0D0
C
C       FOR LENS 2
C       DELETE LENS 2 CONFIGS
C       NOW DELETE ALL BUT THE MAIN CFG
          SYSTEM1(50)=1.0D0
          SYSTEM1(56)=1.0D0
C       GET RID OF LENS 1 SOLVES
          ALENS(33,0:MAXSUR)=0.0D0
C
C       IF THERE ARE PIKUPS, CHANGE THE TARGET SURFACES
C       BY ADDING TO THE TARGET SURFACE NUMBERS,THE VALUE IMAGE1
C
          DO KPIK=1,(PSIZ),5
              J=J+1
              PIKUP(2,0:SUR,KPIK)=PIKUP(2,0:SUR,KPIK)+DBLE(IMAGE1+1)
              PIKUP(2,0:SUR,KPIK+1)=PIKUP(2,0:SUR,KPIK+1)+DBLE(IMAGE1+1)
              PIKUP(2,0:SUR,KPIK+2)=PIKUP(2,0:SUR,KPIK+2)+DBLE(IMAGE1+1)
              PIKUP(2,0:SUR,KPIK+3)=PIKUP(2,0:SUR,KPIK+3)+DBLE(IMAGE1+1)
              PIKUP(2,0:SUR,KPIK+4)=PIKUP(2,0:SUR,KPIK+4)+DBLE(IMAGE1+1)
          END DO
C     NOW SAVE THE SECOND LENS TO PERM LENS
          CALL CTOP
C       NOW WRITE THE SECOND LENS TO THE BOTTOM OF THE FIRST LENS
C       MODE IS FROM THE SECOND LENS

C       SET F/# AND EP HOLDS FROM FIRST LENS
          SYSP(44)=SYSA(44)
          SYSP(45)=SYSA(45)
          SYSP(46)=SYSA(46)
          SYSP(47)=SYSA(47)

C       NOW RESET THE THE NEW FINAL SURFACE NUMBER
          SYSA(20)=IMAGE1+IMAGE2+1.0D0
C       NOW COPY ALL SURFACE DATA FROM LENS 2 TO THE END OF LENS1
          DO I=0,INT(SYSP(20))
              J=INT(IMAGE1)+1+I
              ALENA(1:LSIZ,J)=ALENP(1:LSIZ,I)
              AIPOLYX(1:200,J,1:4)=AIPOLYX(1:200,I,1:4)
              AIPOLYY(1:200,J,1:4)=AIPOLYY(1:200,I,1:4)
          END DO
          DO I=0,INT(SYSP(20))
              J=INT(IMAGE1)+1+I
              MULTCLAPA(1:1000,1:3,J)=MULTCLAPP(1:1000,1:3,I)
              MULTCOBSA(1:1000,1:3,J)=MULTCOBSP(1:1000,1:3,I)
          END DO
C     LENS SURFACE LABELS
          DO I=0,INT(SYSP(20))
              J=INT(IMAGE1)+1+I
              ALBL(J)(1:80)=PLBL(I)(1:80)
          END DO
          DO I=0,INT(SYSP(20))
              J=INT(IMAGE1)+1+I
              GLANMA(J,1)=GLANMP(I,1)
              GLANMA(J,2)=GLANMP(I,2)
          END DO
          DO SUR=0,INT(SYSP(20))
              J=INT(IMAGE1)+1+SUR
              DO JK=1,PSIZ
                  PIKA(1:6,J,JK)=PIKP(1:6,SUR,JK)
              END DO
          END DO
C       WHERE IS THE REFERENCE SURFACE AND ASTOP ?
          IF(FLASTP.EQ.2) SYSA(26)=ASTOP2+IMAGE1+1.0D0
          IF(FLREFS.EQ.2) SYSA(25)=REFS2+IMAGE1+1.0D0
C       NOW COPY THE ARCH LENS TO CURRENT LENS
          CALL ATOC(AM1,AM2,AM3,AM4,AM5,AM6,AM7,AM8,AM9,AM10
     1    ,SYSA,ALENA,SLVA,PIKA,FT01A,LIA,LICA,GLANMA,ALBL,LLTYPEA,INNIA
     2    ,MULTCLAPA,MULTCOBSA,AIPOLYX,AIPOLYY)
C     FIX THE PERM LENS
          CALL CTOP
C     SET NEW OBJ,REF AND IMAGE FLAGE.
          NEWOBJ=0
          NEWREF=INT(SYSTEM1(25))
          NEWIMG=INT(SYSTEM1(20))
C       I DO BELIEVE WE JUST ADDED 2 LENSES CORRECTLY !
C     FINAL UPDATE
          F1=0
          F6=1
          F22=1
          LNSTYP=1
          CALL LNSEOS
          ISTOP(1:5)=.FALSE.
          ISCOMP(1:5)=.FALSE.
          ISCRIT(1:5)=.FALSE.
          VBCNT=0
          TVBCNT=0
          IF(OPTM_INIT.EQ.1) THEN
              OPCNT=0
              VBCNT=0
              DEREXT=.FALSE.
              ITERROR=.FALSE.
              CALL ITER(0,0,ITERROR)
              FMTEXT=.FALSE.
              FMTFLG=.FALSE.
          END IF
C     FINAL UPDATE
          F1=0
          F6=1
          F22=1
          LNSTYP=1
          CALL LNSEOS
          DEALLOCATE(SYSA,ALENA,SLVA
     1    ,PIKA,FT01A,LICA,AIPOLYX,AIPOLYY,
     2    GLANMA,ALBL,MULTCLAPA,MULTCOBSA,STAT=ALLOERR)
          RETURN
 100      FORMAT(A140)
      END
      SUBROUTINE LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
C
          IMPLICIT NONE
C
          CHARACTER NAME1*13,NAME2*13,FLNAME*12
C
          LOGICAL LOWER
C
          REAL*8 LAMLOW,LAMUPP
C
          LAMLOW=0.0D0
          LAMUPP=1.0D10
          IF(NAME1.EQ.'CHANCE       '
     1    .OR.NAME1.EQ.'CORNIN       '
     2    .OR.NAME1.EQ.'CORNIN       '
     3    .OR.NAME1.EQ.'HIKARI       '
     4    ) THEN
              LAMLOW=0.365D0
              LAMUPP=1.100D0
          END IF
          IF(NAME1.EQ.'SCHOTT       ') THEN
              LAMLOW=0.3126D0
              LAMUPP=2.3254D0
          END IF
          IF(NAME1.EQ.'SCH2000      ') THEN
              LAMLOW=0.3126D0
              LAMUPP=2.3254D0
          END IF
          IF(NAME1.EQ.'MODEL        ') THEN
              LAMLOW=0.3341D0
              LAMUPP=2.3254D0
          END IF
C     MATL CAT LIMITS
          IF(NAME2.EQ.'GERMSC       ') THEN
              LAMLOW=2.0581D0
              LAMUPP=16.0D0
          END IF
          IF(NAME2.EQ.'DIAMOND      ') THEN
              LAMLOW=0.23D0
              LAMUPP=20.0D0
          END IF
          IF(NAME2.EQ.'YAG          ') THEN
              LAMLOW=0.4D0
              LAMUPP=4.0D0
          END IF
          IF(NAME2.EQ.'GERMPC       ') THEN
              LAMLOW=2.0581D0
              LAMUPP=13.02D0
          END IF
          IF(NAME2.EQ.'SILICON      ') THEN
              LAMLOW=1.3570D0
              LAMUPP=11.04D0
          END IF
          IF(NAME2.EQ.'IRG100       ') THEN
              LAMLOW=1.0D0
              LAMUPP=14.0D0
          END IF
          IF(NAME2.EQ.'B270         ') THEN
              LAMLOW=0.4358D0
              LAMUPP=0.6563D0
          END IF
          IF(NAME2.EQ.'IRG2         ') THEN
              LAMLOW=0.365D0
              LAMUPP=4.586D0
          END IF
          IF(NAME2.EQ.'IRG3         ') THEN
              LAMLOW=0.4047D0
              LAMUPP=4.586D0
          END IF
          IF(NAME2.EQ.'IRGN6        ') THEN
              LAMLOW=0.4047D0
              LAMUPP=4.258D0
          END IF
          IF(NAME2.EQ.'IRG7         ') THEN
              LAMLOW=0.365D0
              LAMUPP=3.303D0
          END IF
          IF(NAME2.EQ.'IRG9         ') THEN
              LAMLOW=0.365D0
              LAMUPP=3.303D0
          END IF
          IF(NAME2.EQ.'IRG11        ') THEN
              LAMLOW=0.4800D0
              LAMUPP=4.586D0
          END IF
          IF(NAME2.EQ.'IRG15        ') THEN
              LAMLOW=0.365D0
              LAMUPP=3.303D0
          END IF
          IF(NAME2.EQ.'ZNSE         ') THEN
              LAMLOW=0.54D0
              LAMUPP=18.2D0
          END IF
          IF(NAME2.EQ.'IRTRAN4      ') THEN
              LAMLOW=0.54D0
              LAMUPP=18.2D0
          END IF
          IF(NAME2.EQ.'ZNS          ') THEN
              LAMLOW=0.42D0
              LAMUPP=18.2D0
          END IF
          IF(NAME2.EQ.'IRTRAN2      ') THEN
              LAMLOW=0.42D0
              LAMUPP=18.2D0
          END IF
          IF(NAME2.EQ.'CLRTRAN      ') THEN
              LAMLOW=0.4047D0
              LAMUPP=13.0D0
          END IF
          IF(NAME2.EQ.'SUPRASIL     ') THEN
              LAMLOW=0.19D0
              LAMUPP=3.5D0
          END IF
          IF(NAME2.EQ.'ZNS-MS       ') THEN
              LAMLOW=0.42D0
              LAMUPP=18.2D0
          END IF
          IF(NAME2.EQ.'HOMOSIL      ') THEN
              LAMLOW=0.340D0
              LAMUPP=3.5D0
          END IF
          IF(NAME2.EQ.'SILICA       ') THEN
              LAMLOW=0.213856D0
              LAMUPP=3.7067D0
          END IF
          IF(NAME2.EQ.'SIO2         ') THEN
              LAMLOW=0.213856D0
              LAMUPP=3.7067D0
          END IF
          IF(NAME2.EQ.'SAPPHIRE     '
     1    .OR.NAME2.EQ.'SAPHIR       ') THEN
              LAMLOW=0.2652D0
              LAMUPP=5.577D0
          END IF
          IF(NAME2.EQ.'DYNASIL      ') THEN
              LAMLOW=0.213856D0
              LAMUPP=3.7067D0
          END IF
          IF(NAME2.EQ.'AMTIR1       ') THEN
              LAMLOW=1.0D0
              LAMUPP=14.0D0
          END IF
          IF(NAME2.EQ.'AMTIR3       ') THEN
              LAMLOW=3.0D0
              LAMUPP=14.0D0
          END IF
          IF(NAME2.EQ.'AS2S3        ') THEN
              LAMLOW=0.560D0
              LAMUPP=12.0D0
          END IF
          IF(NAME2.EQ.'GAAS         ') THEN
              LAMLOW=2.5D0
              LAMUPP=14.0D0
          END IF
          IF(NAME2.EQ.'CDTE         ') THEN
              LAMLOW=3.0D0
              LAMUPP=11.0D0
          END IF
          IF(NAME2.EQ.'IRTRAN6      ') THEN
              LAMLOW=3.0D0
              LAMUPP=11.0D0
          END IF
          IF(NAME2.EQ.'MGF2(O)      '
     1    .OR.NAME2.EQ.'MGF2         ') THEN
              LAMLOW=0.2D0
              LAMUPP=7.04D0
          END IF
          IF(NAME2.EQ.'IRTRAN1      ') THEN
              LAMLOW=0.2D0
              LAMUPP=7.04D0
          END IF
          IF(NAME2.EQ.'MGF2(E)      ') THEN
              LAMLOW=0.2D0
              LAMUPP=7.04D0
          END IF
          IF(NAME2.EQ.'CAF2         ') THEN
              LAMLOW=0.2288D0
              LAMUPP=9.724D0
          END IF
          IF(NAME2.EQ.'IRTRAN3      ') THEN
              LAMLOW=0.2288D0
              LAMUPP=9.724D0
          END IF
          IF(NAME2.EQ.'MGO          ') THEN
              LAMLOW=0.36117D0
              LAMUPP=5.35D0
          END IF
          IF(NAME2.EQ.'IRTRAN5      ') THEN
              LAMLOW=0.36117D0
              LAMUPP=5.35D0
          END IF
          IF(NAME2.EQ.'BAF2         ') THEN
              LAMLOW=0.2652D0
              LAMUPP=10.346D0
          END IF
          IF(NAME2.EQ.'KBR          ') THEN
              LAMLOW=0.405D0
              LAMUPP=25.0D0
          END IF
          IF(NAME2.EQ.'CSI          ') THEN
              LAMLOW=0.297D0
              LAMUPP=53.12D0
          END IF
          IF(NAME2.EQ.'CSBR         ') THEN
              LAMLOW=0.365D0
              LAMUPP=39.22D0
          END IF
          IF(NAME2.EQ.'KRS5         ') THEN
              LAMLOW=0.577D0
              LAMUPP=39.38D0
          END IF
          IF(NAME2.EQ.'LIF          ') THEN
              LAMLOW=0.1935D0
              LAMUPP=9.79D0
          END IF
          IF(NAME2.EQ.'VIR3         ') THEN
              LAMLOW=0.4047D0
              LAMUPP=6.0D0
          END IF
          IF(NAME2.EQ.'CALAL        ') THEN
              LAMLOW=0.4861D0
              LAMUPP=4.5D0
          END IF
          IF(NAME2.EQ.'9754         ') THEN
              LAMLOW=0.400D0
              LAMUPP=5.5D0
          END IF
          IF(NAME2.EQ.'ALON         ') THEN
              LAMLOW=0.365D0
              LAMUPP=5.0D0
          END IF
          IF(NAME2.EQ.'SPINEL       ') THEN
              LAMLOW=0.4047D0
              LAMUPP=5.5D0
          END IF
          IF(NAME2.EQ.'NACL         ') THEN
              LAMLOW=0.589D0
              LAMUPP=22.3D0
          END IF
          IF(NAME2.EQ.'SIO2O        ') THEN
              LAMLOW=0.185D0
              LAMUPP=7.0D0
          END IF
          IF(NAME2.EQ.'SIO2E        ') THEN
              LAMLOW=0.185D0
              LAMUPP=2.0531D0
          END IF
          IF(NAME2.EQ.'ACRYLIC      ') THEN
              LAMLOW=0.36501D0
              LAMUPP=1.013980D0
          END IF
          IF(NAME2.EQ.'PLYSTY       ') THEN
              LAMLOW=0.36501D0
              LAMUPP=1.013980D0
          END IF
          IF(NAME2.EQ.'POLYCARB     ') THEN
              LAMLOW=0.36501D0
              LAMUPP=1.013980D0
          END IF
          IF(NAME2.EQ.'SAN          ') THEN
              LAMLOW=0.36501D0
              LAMUPP=1.013980D0
          END IF
          IF(NAME2.EQ.'VAC          ') THEN
              LAMLOW=0.001D0
              LAMUPP=1.0D100
          END IF
          IF(NAME2.EQ.'H2O          ') THEN
              LAMLOW=0.1829D0
              LAMUPP=1.968D0
          END IF
c
          IF(FLNAME.EQ.'OHARA.BIN   ') THEN
              LAMLOW=0.3126D0
              LAMUPP=2.3254D0
          END IF
          IF(FLNAME.EQ.'OHARA-O.BIN ') THEN
              LAMLOW=0.365D0
              LAMUPP=1.100D0
          END IF
          LOWER=.FALSE.
          IF(FLNAME.EQ.'OHARA.BIN   ') THEN
              IF(NAME2.EQ.'FPL51Y') LOWER=.TRUE.
              IF(NAME2.EQ.'FSL5Y')  LOWER=.TRUE.
              IF(NAME2.EQ.'BSL7Y')  LOWER=.TRUE.
              IF(NAME2.EQ.'BAL15Y') LOWER=.TRUE.
              IF(NAME2.EQ.'BAL35Y') LOWER=.TRUE.
              IF(NAME2.EQ.'BSM51Y') LOWER=.TRUE.
              IF(NAME2.EQ.'PBL1Y')  LOWER=.TRUE.
              IF(NAME2.EQ.'PBL6Y')  LOWER=.TRUE.
              IF(NAME2.EQ.'PBL25Y') LOWER=.TRUE.
              IF(NAME2.EQ.'PBL26Y') LOWER=.TRUE.
              IF(NAME2.EQ.'PBM2Y')  LOWER=.TRUE.
              IF(NAME2.EQ.'PBM8Y')  LOWER=.TRUE.
              IF(NAME2.EQ.'i-FPL51Y') LOWER=.TRUE.
              IF(NAME2.EQ.'i-FSL5Y')  LOWER=.TRUE.
              IF(NAME2.EQ.'i-BSL7Y')  LOWER=.TRUE.
              IF(NAME2.EQ.'i-BAL15Y') LOWER=.TRUE.
              IF(NAME2.EQ.'i-BAL35Y') LOWER=.TRUE.
              IF(NAME2.EQ.'i-BSM51Y') LOWER=.TRUE.
              IF(NAME2.EQ.'i-PBL1Y')  LOWER=.TRUE.
              IF(NAME2.EQ.'i-PBL6Y')  LOWER=.TRUE.
              IF(NAME2.EQ.'i-PBL25Y') LOWER=.TRUE.
              IF(NAME2.EQ.'i-PBL26Y') LOWER=.TRUE.
              IF(NAME2.EQ.'i-PBM2Y')  LOWER=.TRUE.
              IF(NAME2.EQ.'i-PBM8Y')  LOWER=.TRUE.
              IF(LOWER) THEN
                  LAMLOW=0.2500D0
                  LAMUPP=1.100D0
              END IF
          END IF
          IF(NAME1.EQ.'SCHOTT') THEN
              LAMLOW=0.3126D0
              LAMUPP=2.3254D0
          END IF
          IF(NAME1.EQ.'SCH2000') THEN
              LAMLOW=0.3126D0
              LAMUPP=2.3254D0
          END IF
C RAD HARD LIMITS
          IF( NAME2.EQ.'BK7G18       '
     1    .OR.NAME2.EQ.'520637       '
     1    .OR.NAME2.EQ.'GG375G34     '
     1    .OR.NAME2.EQ.'525564       '
     1    .OR.NAME2.EQ.'BK7G25       '
     1    .OR.NAME2.EQ.'521632       '
     1    .OR.NAME2.EQ.'K5G20        '
     1    .OR.NAME2.EQ.'523568       ')THEN
              LAMLOW=0.365D0
              LAMUPP=2.3254D0
          END IF
C
          IF( NAME2.EQ.'BAK1G12      '
     1    .OR.NAME2.EQ.'572576       '
     1    .OR.NAME2.EQ.'SK4G13       '
     1    .OR.NAME2.EQ.'612582       '
     1    .OR.NAME2.EQ.'SK5G06       '
     1    .OR.NAME2.EQ.'590611       ')THEN
              LAMLOW=0.4047D0
              LAMUPP=2.3254D0
          END IF
          IF( NAME2.EQ.'SK10G10      '
     1    .OR.NAME2.EQ.'622572       '
     1    .OR.NAME2.EQ.'SSK5G06      '
     1    .OR.NAME2.EQ.'658510       ')THEN
              LAMLOW=0.4047D0
              LAMUPP=2.3254D0
          END IF
          IF( NAME2.EQ.'LAK9G15      '
     1    .OR.NAME2.EQ.'691547       '
     1    .OR.NAME2.EQ.'LF5G15       '
     1    .OR.NAME2.EQ.'584408       ')THEN
              LAMLOW=0.4047D0
              LAMUPP=2.3254D0
          END IF
          IF( NAME2.EQ.'L2G12        '
     1    .OR.NAME2.EQ.'621366       '
     1    .OR.NAME2.EQ.'SF5G10       '
     1    .OR.NAME2.EQ.'673323       '
     1    .OR.NAME2.EQ.'SF8G07       '
     1    .OR.NAME2.EQ.'695307       ')THEN
              LAMLOW=0.4047D0
              LAMUPP=2.3254D0
          END IF
          IF( NAME2.EQ.'SF6G05       '
     1    .OR.NAME2.EQ.'809253       '
     1    .OR.NAME2.EQ.'KZFS4G20     '
     1    .OR.NAME2.EQ.'615447       ')THEN
              LAMLOW=0.4358D0
              LAMUPP=2.3254D0
          END IF
          IF(NAME1.EQ.'RUSSIAN') THEN
              LAMLOW=0.3126D0
              LAMUPP=2.3254D0
              IF(NAME2.EQ.'LK8          '.OR.
     1           NAME2.EQ.'FK11         '.OR.
     2           NAME2.EQ.'FK14         '.OR.
     3           NAME2.EQ.'K100         '.OR.
     4           NAME2.EQ.'STK20        '.OR.
     5           NAME2.EQ.'TBF25        '.OR.
     6           NAME2.EQ.'TF12         '.OR.
     7           NAME2.EQ.'TF13         ') THEN
                  LAMLOW=0.40466D0
                  LAMUPP=2.3254D0
              END IF
              IF(NAME2.EQ.'ST2          '.OR.
     1           NAME2.EQ.'ST11         '.OR.
     2           NAME2.EQ.'LK4          '.OR.
     1           NAME2.EQ.'KY1          '.OR.
     1           NAME2.EQ.'KY2          '.OR.
     1           NAME2.EQ.'KB           '.OR.
     1           NAME2.EQ.'KI           '.OR.
     7           NAME2.EQ.'KB-P         ') THEN
                  LAMLOW=0.3650D0
                  LAMUPP=1.9701D0
              END IF
          END IF
          RETURN
      END


      SUBROUTINE LOADIPOLY(J,SUR)
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INTEGER J,SUR,N,I,M
          REAL*8 X,Y
          CHARACTER AN*2,FILE_NAME*11
          LOGICAL FEXIST,FOPEN
C       J=1 = CLAP
C       J=2 = CLAP ERASE
C       J=3 = COBS
C       J=4 = COBS ERASE
C       open appropriate file, read data and load appropriate IPOLYX and IPOLYY arrays.
C
          IF(J.EQ.1) THEN
              N=INT(ALENS(10,SURF))
              CALL ITOA2(N,AN)
              FILE_NAME=trim(HOME)//'IPOLY'//AN//'.DAT'
C
C       CHECK FOR FILE EXISTENCE
              FEXIST=.FALSE.
              CALL EXIST_FILE(FILE_NAME,FEXIST)
              IF(FEXIST) THEN
C
C       CHECK FOR FILE ALREADY OPENED
                  FOPEN=.FALSE.
                  CALL OPEN_FILE(FILE_NAME,FOPEN)
                  IF(FOPEN) THEN
                      CALL CLOSE_FILE(67,1)
                  END IF
              ELSE
C       FILE DOES NOT EXIST,QUIT
                  CALL MACFAL
                  RETURN
              END IF
C       NOW OPEN FILE AND LOAD APPROPRIATE ARRAY
              OPEN(UNIT=67,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=FILE_NAME
     2        ,STATUS='UNKNOWN')
              REWIND(UNIT=67)
              DO I=1,INT(ALENS(11,SURF))
                  READ(UNIT=67,FMT=*,END=9991,ERR=9991) M,X,Y
                  IPOLYX(I,SUR,J)=X
                  IPOLYY(I,SUR,J)=Y
              END DO
          END IF
C
          IF(J.EQ.2) THEN
              N=INT(ALENS(52,SURF))
              CALL ITOA2(N,AN)
              FILE_NAME='IPOLY'//AN//'.DAT'
C
C       CHECK FOR FILE EXISTENCE
              FEXIST=.FALSE.
              CALL EXIST_FILE(FILE_NAME,FEXIST)
              IF(FEXIST) THEN
C
C       CHECK FOR FILE ALREADY OPENED
                  FOPEN=.FALSE.
                  CALL OPEN_FILE(FILE_NAME,FOPEN)
                  IF(FOPEN) THEN
                      CALL CLOSE_FILE(67,1)
                  END IF
              ELSE
C       FILE DOES NOT EXIST,QUIT
                  CALL MACFAL
                  RETURN
              END IF
C       NOW OPEN FILE AND LOAD APPROPRIATE ARRAY
              OPEN(UNIT=67,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=FILE_NAME
     2        ,STATUS='UNKNOWN')
              REWIND(UNIT=67)
              DO I=1,INT(ALENS(53,SURF))
                  READ(UNIT=67,FMT=*,END=9991,ERR=9991) M,X,Y
                  IPOLYX(I,SUR,J)=X
                  IPOLYY(I,SUR,J)=Y
              END DO
          END IF
C
C
          IF(J.EQ.3) THEN
              N=INT(ALENS(17,SURF))
              CALL ITOA2(N,AN)
              FILE_NAME='IPOLY'//AN//'.DAT'
C
C       CHECK FOR FILE EXISTENCE
              FEXIST=.FALSE.
              CALL EXIST_FILE(FILE_NAME,FEXIST)
              IF(FEXIST) THEN
C
C       CHECK FOR FILE ALREADY OPENED
                  FOPEN=.FALSE.
                  CALL OPEN_FILE(FILE_NAME,FOPEN)
                  IF(FOPEN) THEN
                      CALL CLOSE_FILE(67,1)
                  END IF
              ELSE
C       FILE DOES NOT EXIST,QUIT
                  CALL MACFAL
                  RETURN
              END IF
C       NOW OPEN FILE AND LOAD APPROPRIATE ARRAY
              OPEN(UNIT=67,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=FILE_NAME
     2        ,STATUS='UNKNOWN')
              REWIND(UNIT=67)
              DO I=1,INT(ALENS(18,SURF))
                  READ(UNIT=67,FMT=*,END=9991,ERR=9991) M,X,Y
                  IPOLYX(I,SUR,J)=X
                  IPOLYY(I,SUR,J)=Y
              END DO
          END IF
C
C
          IF(J.EQ.4) THEN
              N=INT(ALENS(62,SURF))
              CALL ITOA2(N,AN)
              FILE_NAME=trim(HOME)//'IPOLY'//AN//'.DAT'
C
C       CHECK FOR FILE EXISTENCE
              FEXIST=.FALSE.
              CALL EXIST_FILE(FILE_NAME,FEXIST)
              IF(FEXIST) THEN
C
C       CHECK FOR FILE ALREADY OPENED
                  FOPEN=.FALSE.
                  CALL OPEN_FILE(FILE_NAME,FOPEN)
                  IF(FOPEN) THEN
                      CALL CLOSE_FILE(67,1)
                  END IF
              ELSE
C       FILE DOES NOT EXIST,QUIT
                  CALL MACFAL
                  RETURN
              END IF
C       NOW OPEN FILE AND LOAD APPROPRIATE ARRAY
              OPEN(UNIT=67,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=FILE_NAME
     2        ,STATUS='UNKNOWN')
              REWIND(UNIT=67)
              DO I=1,INT(ALENS(63,SURF))
                  READ(UNIT=67,FMT=*,END=9991,ERR=9991) M,X,Y
                  IPOLYX(I,SUR,J)=X
                  IPOLYY(I,SUR,J)=Y
              END DO
          END IF
C
 9991     CONTINUE
          CALL CLOSE_FILE(67,1)
          RETURN
      END

