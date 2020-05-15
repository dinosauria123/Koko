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

C SUB LSTAT.FOR
      SUBROUTINE LSTAT1
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE RETURNS THE NUMBER OF LENSES
C       CURRENTLY ON FILE IN THE LENS LIBRARAY
C
C
          INTEGER I,J,K,II,JJ
C
          LOGICAL EXISJK,EXISTAG
C
          CHARACTER DATA*80
C
          COMMON/DATA/I,J,K,II
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       OPEN UNIT 20 FOR I/O
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBLEN)//'LIB.DAT',EXIST=EXISJK)
          EXISTAG=.FALSE.
          INQUIRE(FILE=trim(LIBLEN)//'LIBTAG.DAT',EXIST=EXISTAG)
          IF(.NOT.EXISJK) THEN
              IF(trim(LIBLEN).NE.trim(HOME)//'LIBLEN/') THEN
                  WRITE(OUTLYNE,*)
     1            'THE LENS LIBRARY IN LIBRARY DIRECTORY = ',trim(LIBLEN)
                  CALL SHOWIT(1)
                  OUTLYNE='DOES NOT EXIST. BEFORE USING "ILF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  OUTLYNE='MAKE SURE YOU CHANGE TO THE DESIRED LENS LIBRARY'
                  CALL SHOWIT(1)
              ELSE
                  OUTLYNE='"THE DEFAULT LENS LIBRARY DOES NOT YET EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='TO INITIALIZE IT, USE "ILF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  OUTLYNE='BE CAREFUL, YOU MIGHT BE DELEATING YOUR LENS LIBRARY'
                  CALL SHOWIT(1)
                  OUTLYNE='BACK UP THE LENS LIBRARY FIRST WITH'
                  CALL SHOWIT(1)
                  OUTLYNE='THE "LIBSAVE" COMMAND'
                  CALL SHOWIT(1)
              END IF
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=22,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIB.DAT',
     1    FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
          IF(EXISTAG)
     1    OPEN(UNIT=27,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIBTAG.DAT',
     1    FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
          J=0
          JJ=0
          K=0
          DO 10 I=1,999
              READ(UNIT=22,REC=I) II,DATA
              IF(EXISTAG) THEN
                  DO J=1,10
                      READ(UNIT=27,REC=I-1+J) IDTAG(J)(1:75)
                  END DO
              END IF
              IF(II.NE.0) THEN
C       FOUND A STORED LENS
                  J=J+1
                  JJ=JJ+1
              END IF
 10       CONTINUE
C
          K=999-JJ
C       CLOSE UNIT 20 TO I/O
C
          CALL CLOSE_FILE(22,1)
          IF(EXISTAG) CALL CLOSE_FILE(27,1)
C
          WRITE(OUTLYNE,*) JJ,' LENSES ON FILE.'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'ROOM FOR',K,' MORE LENSES IN LENS LIBRARY'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'LENS DIRECTORY IS CURRENTLY: ',LIBLEN
          CALL SHOWIT(1)
          RETURN
      END

C SUB LLIB.FOR
      SUBROUTINE LLIB
          USE GLOBALS
C
          IMPLICIT NONE
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C               LENS LIBRARY COMMANDS
          IF(SQ.EQ.1.AND.
     1    WQ.NE.'P'.AND.WQ.NE.'PUT'.AND.WQ.NE.'GET'.AND.WQ.NE.'DEL'
     1    .AND.WQ.NE.'GETES'.AND.WQ.NE.'GETMG'.AND.WQ.NE.'GETNC'.AND.
     1    WQ.NE.'GETSH'.AND.WQ.NE.'PNC'.AND.WQ.NE.
     1    'PMG'.AND.WQ.NE.'PES'.AND.WQ.NE.'PSH'.AND.
     1    WQ.NE.'PRO'.AND.WQ.NE.'GETRO') THEN
              OUTLYNE=
     1        'INVALID QUALIFIER WORD USED WITH THE "LIB" COMMAND'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          ELSE
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE=
     1        '"LIB" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          ELSE
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"LIB" TAKES NO STRING OR NUMERIC WORD #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WQ.EQ.'P'.OR.WQ.EQ.'PMG'.OR.WQ.EQ.'PES'.OR.WQ.EQ.'PNC'
     1    .OR.WQ.EQ.'PSH'.OR.WQ.EQ.'GETMG'.OR.WQ.EQ.
     2    'GETES'.OR.WQ.EQ.'GETNC'.OR.WQ.EQ.'GETSH'.OR.
     1    WQ.EQ.'PUT'.OR.WQ.EQ.'PRO'.OR.WQ.EQ.'GETRO'
     2    .OR.WQ.EQ.'GET'.OR.
     3    WQ.EQ.'DEL') THEN
              CALL LLIBRY
              CALL FIXCURLENS
              RETURN
          ELSE
          END IF
          RETURN
      END
C SUB OOPDIF.FOR
      SUBROUTINE OOPDIF
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE OOPDIF COMMAND
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              IF(OPDIF) WRITE(OUTLYNE,100)
              IF(.NOT.OPDIF) WRITE(OUTLYNE,200)
              CALL SHOWIT(0)
 100          FORMAT(
     1        'DIFFERNTIAL RAY TRACING "ON" IN PRE-DEFINED OPERANDS')
 200          FORMAT(
     1        'DIFFERNTIAL RAY TRACING "OFF" IN PRE-DEFINED OPERANDS')
              RETURN
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"OPDIF" TAKES NO STRING OR NUMERICAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       QUALIFIERS
          IF(SQ.EQ.1.AND.WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
              OUTLYNE='"ON" AND "OFF" ARE THE ONLY VALID QUALIFIERS'
              CALL SHOWIT(1)
              OUTLYNE='USED WITH "OPDIF"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"OPDIF" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON') OPDIF=.TRUE.
          IF(WQ.EQ.'OFF') OPDIF=.FALSE.
          RETURN
      END
C SUB LLDDFF.FOR
      SUBROUTINE LLDDFF
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE LDIF SETTING VIA THE DIFRAY COMMAND
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              IF(LDIF) WRITE(OUTLYNE,100)
              IF(.NOT.LDIF) WRITE(OUTLYNE,200)
              CALL SHOWIT(0)
 100          FORMAT(
     1        'DIFFERNTIAL RAY TRACING IS CURRENTLY ACTIVE')
 200          FORMAT(
     1        'DIFFERNTIAL RAY TRACING IS CURRENTLY "NOT" ACTIVE')
              RETURN
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"DIFRAY" TAKES NO STRING OR NUMERICAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       QUALIFIERS
          IF(SQ.EQ.1.AND.WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
              OUTLYNE='"ON" AND "OFF" ARE THE ONLY VALID QUALIFIERS'
              CALL SHOWIT(1)
              OUTLYNE='USED WITH "DIFRAY"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"DIFRAY" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON') LDIF=.TRUE.
          IF(WQ.EQ.'OFF') LDIF=.FALSE.
          RETURN
      END
C SUB LLDDF2.FOR
      SUBROUTINE LLDDF2
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE LDIF2 SETTING VIA THE DIFFOB COMMAND
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              IF(LDIF2) WRITE(OUTLYNE,100)
              IF(.NOT.LDIF2) WRITE(OUTLYNE,200)
              CALL SHOWIT(0)
 100          FORMAT(
     1        'DIFFERNTIAL REFERENCE RAY TRACING IS CURRENTLY ACTIVE')
 200          FORMAT(
     1        'DIFFERNTIAL REFERENCE RAY TRACING IS CURRENTLY "NOT" ACTIVE')
              RETURN
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"DIFFOB" TAKES NO STRING OR NUMERICAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       QUALIFIERS
          IF(SQ.EQ.1.AND.WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
              OUTLYNE='"ON" AND "OFF" ARE THE ONLY VALID QUALIFIERS'
              CALL SHOWIT(1)
              OUTLYNE='USED WITH "DIFFOB"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"DIFFOB" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON') LDIF2=.TRUE.
          IF(WQ.EQ.'OFF') LDIF2=.FALSE.
          RETURN
      END
C SUB LLDDF3.FOR
      SUBROUTINE LLDDF3
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES THE SETTING OR NOVIRT VIA THE VIRTRAY COMMAND
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(STI.EQ.1) THEN
              IF(NOVIRT) WRITE(OUTLYNE,100)
              IF(.NOT.NOVIRT) WRITE(OUTLYNE,200)
              CALL SHOWIT(0)
 100          FORMAT(
     1        'VIRTUAL RAYS WILL NOT BE PLOTTED (PROGRAM DEFAULT)')
 200          FORMAT(
     1        'VIRTUAL RAYS WILL BE PLOTTED')
              RETURN
          END IF
C       CHECK FOR STRING INPUT
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"VIRTRAY" TAKES NO STRING OR NUMERICAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       QUALIFIERS
          IF(SQ.EQ.1.AND.WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
              OUTLYNE='"ON" AND "OFF" ARE THE ONLY VALID QUALIFIERS'
              CALL SHOWIT(1)
              OUTLYNE='USED WITH "VIRTRAY"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"VIRTRAY" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON') NOVIRT=.FALSE.
          IF(WQ.EQ.'OFF') NOVIRT=.TRUE.
          RETURN
      END
C SUB LEPRT.FOR
      SUBROUTINE LEPRT
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE LEPRT. THIS SUBROUTINE IMPLEMENTS
C       THE CMD COMMAND "LEPRT" OR "LIS"
C
          INTEGER IW3
C
          LOGICAL OLDHEAD
C
          CHARACTER IAW3*3
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsub.inc'
C
          IF(S2.EQ.1.OR.SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.
     1    S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'LEPRT') WRITE(OUTLYNE,*)
     1        '"LEPRT" TAKES NO QUALIFIER, STRING OR NUMERIC WORD'
              IF(WC.EQ.'LIS') WRITE(OUTLYNE,*)
     1        '"LIS" TAKES NO QUALIFIER, STRING OR NUMERIC WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '"#2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"LEPRT/LIS" LISTS ALL LENS DATA'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.0) THEN
              IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)
     1            '"SURFACE NUMBER BEYOND LEGAL BOUNDS"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          OLDHEAD=HEADIN
C
          IF(DF1.EQ.0) THEN
              IW3=INT(W1)
              CALL ITOAA(IW3,IAW3)
              IF(.NOT.HEADIN) HEADIN=.TRUE.
          END IF
C
          IF(DF1.EQ.1) THEN
C
C     OUTPUT NON-SURFACE STUFF
C
C     PROCEED
              WRITE(OUTLYNE,10)
              CALL SHOWIT(0)
 10           FORMAT('CURRENT LENS DATA LISTING')
              INPUT='LI'
              CALL PROCES
              INPUT='LIC'
              CALL PROCES
              INPUT='INI'
              CALL PROCES
              INPUT='LTYPE'
              CALL PROCES
              INPUT='UNITS ?'
              CALL PROCES
              INPUT='REF'
              CALL PROCES
              INPUT='FLDSARE'
              CALL PROCES
              INPUT='OCDY'
              CALL PROCES
              INPUT='OCDX'
              CALL PROCES
              INPUT='SAY'
              CALL PROCES
              INPUT='SAX'
              CALL PROCES
              INPUT='AUTOFUNC'
              CALL PROCES
              INPUT='SPTWT'
              CALL PROCES
              INPUT='MODE'
              CALL PROCES
              INPUT='ASTOP'
              CALL PROCES
              INPUT='SPC'
              CALL PROCES
              INPUT='PIVAXIS'
              CALL PROCES
          END IF
C
          IF(DF1.EQ.1) INPUT='RTGLBL ALL'
          IF(DF1.EQ.0) INPUT='RTGLBL '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='ASPH ALL'
          IF(DF1.EQ.0) INPUT='ASPH '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='TASPH ALL'
          IF(DF1.EQ.0) INPUT='TASPH '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='DEFORM ALL'
          IF(DF1.EQ.0) INPUT='DEFORM '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='TAD ALL'
          IF(DF1.EQ.0) INPUT='TAD '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='TR ALL'
          IF(DF1.EQ.0) INPUT='TR '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='CAOB ALL'
          IF(DF1.EQ.0) INPUT='CAOB '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='RIN ALL'
          IF(DF1.EQ.0) INPUT='RIN '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='RIN2 ALL'
          IF(DF1.EQ.0) INPUT='RIN2 '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='SPGR ALL'
          IF(DF1.EQ.0) INPUT='SPGR '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='GRT ALL'
          IF(DF1.EQ.0) INPUT='GRT '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='SLV ALL'
          IF(DF1.EQ.0) INPUT='SLV '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='PIK ALL'
          IF(DF1.EQ.0) INPUT='PIK '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='SURTYPE ALL'
          IF(DF1.EQ.0) INPUT='SURTYPE '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='FOOTBLOK ALL'
          IF(DF1.EQ.0) INPUT='FOOTBLOK '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='PIVAXIS ALL'
          IF(DF1.EQ.0) INPUT='PIVAXIS '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='DUMOUT ALL'
          IF(DF1.EQ.0) INPUT='DUMOUT '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='INR ALL'
          IF(DF1.EQ.0) INPUT='INR '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='THM ALL'
          IF(DF1.EQ.0) INPUT='THM '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='PRICE ALL'
          IF(DF1.EQ.0) INPUT='PRICE '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='COATING ALL'
          IF(DF1.EQ.0) INPUT='COATING '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) INPUT='PRSPR ALL'
          IF(DF1.EQ.0) INPUT='PRSPR '//IAW3
          CALL PROCES
          IF(DF1.EQ.1) THEN
              INPUT='CF'
              CALL PROCES
              INPUT='CFG'
              CALL PROCES
          END IF
          HEADIN=OLDHEAD
          RETURN
      END


C SUB LENUP.FOR
      SUBROUTINE LENUP
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE LENUP. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE CHANGES AN EXISTING LENS.
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          REAL*8 RAL,RBE,CGAM,SGAM,RGAM
C
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          END IF
C       HERE IS WERE THE ? FOLLOWING A LENS ULPATE COMMAND
C       IS HANDLED
          IF(STI.EQ.1) THEN
              CALL ULQUER
              RETURN
          END IF
C       CERTAIN COMMANDS ARE NOT ALLOWED IF SURF=SYSTEM1(20)
C       THEY ARE TH,PY,PCY,PX,PCX,CAY,CAX,PIKUP TH,
          IF(SURF.EQ.INT(SYSTEM1(20))) THEN
              IF(WC.EQ.'TH'.OR.WC.EQ.'PY'.OR.WC.EQ.'PCY'.OR.
     1        WC.EQ.'PX'.OR.WC.EQ.'PCX'.OR.WC.EQ.'CAY'.OR.WC.EQ.'CAX'
     2        .OR.WC.EQ.'PIKUP'.AND.WQ.EQ.'TH') THEN
C       THESE ARE NOT VALID AT THE IMAGE SURFACE
C
                  IF(WC.EQ.'TH') THEN
                      OUTLYNE=
     1                'THE "TH" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE='IN THE UPDATE LENS MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C
                  IF(WC.EQ.'PY') THEN
                      OUTLYNE=
     1                'THE "PY" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE='IN THE UPDATE LENS MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C
                  IF(WC.EQ.'PCY') THEN
                      OUTLYNE=
     1                'THE "PCY" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE='IN THE UPDATE LENS MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C
                  IF(WC.EQ.'CAY') THEN
                      OUTLYNE=
     1                'THE "CAY" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE='IN THE UPDATE LENS MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C
                  IF(WC.EQ.'PX') THEN
                      OUTLYNE=
     1                'THE "PX" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE='IN THE UPDATE LENS MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C
                  IF(WC.EQ.'PCX') THEN
                      OUTLYNE=
     1                'THE "PCX" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE='IN THE UPDATE LENS MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C
                  IF(WC.EQ.'CAX') THEN
                      OUTLYNE=
     1                'THE "CAX" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE='IN THE UPDATE LENS MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C
                  IF(WC.EQ.'PIKUP'.AND.WQ.EQ.'TH') THEN
                      OUTLYNE=
     1                'THE "PIKUP TH" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
                      CALL SHOWIT(1)
                      OUTLYNE='IN THE UPDATE LENS MODE'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  RETURN
C       NOT A COMMAND THAT WAS DISALLOWED
              END IF
C       NOT IMAGE SURF, PROCEED
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
              LNSTYP=1
              CALL LNSEOS
C     REBUILD VARIABLES AND TOL VARIABLES AS NECESSARY
              IF(F28.EQ.0.AND.F31.EQ.0) THEN
                  CALL VCHECK
                  CALL CVCHECK
                  CALL TVCHECK
              END IF
              RETURN
          ELSE
C       DON'T TERMINATE THE INPUT ROUTINE. ACCEPT INPUT DATA STARTING
C       AT THE CURRENT SURFACE OR ACCEPT SYSTEM AND TILE DATA OR CHANGE
C       CURRENT SURFACE. DEFAULT CURRENT SURFACE IS SURF=0
C       HERE IS WHERE THE BRANCHING TO SUBROUTINES WHICH HANDLE
C       CHANGES TO THE LENS MATRICES ARE HANDELED.
C
C
C       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
C       SPECIFIC STORAGE LOCATIONS FOR LENS DATA ITEMS.
C
C       COMMAND (LI) OR (LIC) AT 'U L' LEVEL
              IF(WC.EQ.'LI'.OR.WC.EQ.'LIC') THEN
                  CALL SLI
                  RETURN
              END IF
C       COMMAND (INI) AT 'U L' LEVEL
              IF(WC.EQ.'INI') THEN
                  CALL SINI
                  RETURN
              END IF
C       COMMAND (LTYPE) AT 'U L' LEVEL
              IF(WC.EQ.'LTYPE') THEN
                  CALL SLTYPE
                  RETURN
              END IF
C       COMMAND (LABEL) OR (LBL) AT 'U L' LEVEL
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
C       COMMAND (INRD)
              IF(WC.EQ.'INRD') THEN
                  CALL INRINRD
                  RETURN
              END IF
C       COMMAND (ZERO)
              IF(WC.EQ.'ZERO') THEN
                  CALL ZERO
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
C       COMMAND (DELDEFOR)
              IF(WC.EQ.'DELDEFOR') THEN
                  IF(ALENS(103,SURF).EQ.0.0D0) THEN
                      OUTLYNE=
     1                'NO DEFORMABLE SURFACE DEFINITION EXISTS TO BE DELETED'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL DELDEFIT
                  RETURN
              END IF
C       COMMAND (AIR)
              IF(WC.EQ.'AIR') THEN
                  CALL SAIR
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
                  RETURN
              END IF
C       COMMAND ((GLASS CAT NAME) [INDENTIFICATION])
              IF(WC.EQ.'SCHOTT'.OR.
     1        WC.EQ.'OHARA'.OR.WC.EQ.'CHANCE'.OR.
     2        WC.EQ.'CORNIN'.OR.WC.EQ.'RADHARD'.OR.
     3        WC.EQ.'MATL'.OR.WC.EQ.'GLA'.OR.
     4        WC.EQ.'USER'.OR.WC.EQ.'RUSSIAN'.OR.
     4        WC.EQ.'HIKARI'.OR.WC.EQ.'SCH2000'.OR.
     6        WC.EQ.'HOYA'.OR.WC.EQ.'GLCAT') THEN
                  CALL GLSCAT
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
C       COMMAND (N1,W1, TO N10,W1)
              IF(WC.EQ.'N1'.OR.WC.EQ.'N2'.OR.WC.EQ.'N3'.OR.
     1        WC.EQ.'N4'.OR.WC.EQ.'N5'.OR.WC.EQ.'N6'.OR.WC.EQ.'N7'
     2        .OR.WC.EQ.'N8'.OR.WC.EQ.'N9'.OR.WC.EQ.'N10') THEN
                  CALL RNCHG
                  RETURN
              END IF
C       COMMAND (INDEX OR VNUM OR DPART)
              IF(WC.EQ.'INDEX'.OR.WC.EQ.'VNUM'.OR.WC.EQ.'DPART') THEN
                  CALL FICTCHG
                  RETURN
              END IF
C       COMMAND (ASPH,D E F G AC)
              IF(WC.EQ.'ASPH'.OR.WC.EQ.'ASPH2') THEN
                  CALL SASPH
                  RETURN
              END IF
C       COMMAND (ASPHD OR TASPHD)
              IF(WC.EQ.'ASPHD'.OR.WC.EQ.'TASPHD') THEN
                  CALL SASPHD
                  RETURN
              END IF
C       COMMAND (ARRAYD)
              IF(WC.EQ.'ARRAYD') THEN
                  CALL SARRAYD
                  RETURN
              END IF
C       COMMAND (ARRAY)
              IF(WC.EQ.'ARRAY') THEN
                  CALL SARRAY(.FALSE.)
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
                  RETURN
              END IF
C       COMMAND (REFLTIRO)
              IF(WC.EQ.'REFLTIRO') THEN
                  CALL SREFL
                  RETURN
              END IF
C       COMMAND (REFLTIR)
              IF(WC.EQ.'REFLTIR') THEN
                  CALL SREFL
                  RETURN
              END IF
C       COMMAND (PERFECT)
              IF(WC.EQ.'PERFECT') THEN
                  CALL SPERFECT
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
                  RETURN
              END IF
C       COMMAND (CHG,W1)
              IF(WC.EQ.'CHG') THEN
                  CALL SCHG
                  RETURN
              END IF
C       COMMAND (INS,W1)
              IF(WC.EQ.'INS') THEN
                  IF(F12.NE.1) THEN
                      OUTLYNE='"INS" ONLY WORKS IN CONFIGURATION #1 (MAIN CONFIG)'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL SINS
                  RETURN
              END IF
C       COMMAND (DEL,W1 W2)
              IF(WC.EQ.'DEL') THEN
                  IF(F12.NE.1) THEN
                      OUTLYNE='"DEL" ONLY WORKS IN CONFIGURATION #1 (MAIN CONFIG)'
                      CALL SHOWIT(1)
                      OUTLYNE='NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  CALL SDEL
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
C       COMMAND (CLAP.OR.COBS)
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
C       COMMAND (CLAPD OR COBSD)
              IF(WC.EQ.'CLAPD'.OR.WC.EQ.'COBSD') THEN
                  CALL SAPED
                  RETURN
              END IF
C       COMMAND (AC,AD,AE,AF,AG,ADTOR,AETOR,AFTOR,AGTOR)
C       COMMAND (AH,AI,AJ,AK,AL)
              IF(WC.EQ.'AC'.OR.WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.
     1          'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'
     2        .OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'
     3        .OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL') THEN
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
C       COMMAND (GDX.OR.GDX.OR.GDZ)
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
C       COMMAND (DEC)
              IF(WC.EQ.'DEC') THEN
                  CALL SDEC
                  RETURN
              END IF
C       COMMAND (PIVOT)
              IF(WC.EQ.'PIVOT') THEN
                  CALL SPIVOT
                  RETURN
              END IF
C       COMMAND (PIVOTD)
              IF(WC.EQ.'PIVOTD') THEN
                  CALL SPIVOTD
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
     1        WC.EQ.'TILT'.AND.WQ.NE.'AUTOD'.OR.
     1        WC.EQ.'RTILT') THEN
                  CALL STILT
                  RETURN
              END IF
C       COMMAND (TILT AUTOD)
              IF(WC.EQ.'TILT'.AND.WQ.EQ.'AUTOD') THEN
                  CALL STILTAD
                  RETURN
              END IF
C       COMMAND (TILTD)
              IF(WC.EQ.'TILTD') THEN
                  CALL STILTD
                  RETURN
              END IF
C       COMMAND (YTORIC OR XTORIC)
              IF(WC.EQ.'YTORIC'.OR.WC.EQ.'XTORIC') THEN
                  CALL STORIC
                  RETURN
              END IF
C       COMMAND (RDTOR,CVTOR)
              IF(WC.EQ.'RDTOR'.OR.WC.EQ.'CVTOR') THEN
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
C       COMMAND (TORD)
              IF(WC.EQ.'TORD') THEN
                  CALL STORD
                  RETURN
              END IF
C       COMMAND (GRTD)
              IF(WC.EQ.'GRTD') THEN
                  CALL SGRTD
                  RETURN
              END IF
C       COMMANDS (PY,PX,PCY,PCX,CAY AND CAX)
C       AND DELETIONS TSD
              IF(WC.EQ.'PY'.OR.WC.EQ.'PX'.OR.WC.EQ.
     1        'PCY'.OR.WC.EQ.'TSD'.OR.
     1        WC.EQ.'PCX'.OR.WC.EQ.'CAY'.OR.WC.EQ.'CAX') THEN
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
C       COMMANDS (APY,PIY,PUY,APCY,PICY,PUCY,COCY,
C                 APX,PIX,PUX,APCX,PICX,PUCX, AND COCX)
C       AND DELETIONS CSD,CSDX,CSDY
              IF(WC.EQ.'APY'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PUY'.OR.
     1        WC.EQ.'APCY'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY'.OR.WC.EQ.
     2        'COCY'.OR.WC.EQ.'APX'.OR.WC.EQ.'PIX'.OR.WC.EQ.'PUX'
     3        .OR.WC.EQ.'APCX'.OR.WC.EQ.'PICX'.OR.WC.EQ.'PUCX'.OR.WC.EQ.
     4        'COCX'.OR.WC.EQ.'CSD'.OR.WC.EQ.'CSDX'.OR.
     5        WC.EQ.'CSDY') THEN
                  CALL CVSOLV
                  RETURN
              END IF
C       COMMANDS (PIKUP)
              IF(WC.EQ.'PIKUP') THEN
                  CALL SPIKUP
                  IF(SYSTEM1(63).NE.0.0D0.AND.DABS(ALENS(3,NEWOBJ)).GE.1.0D10) THEN
                      SYSTEM1(63)=0.0D0
                      OUTLYNE='OBJECT THICKNESS EQUALS OR EXCEEDS 1.0D+10 LENS UNITS'
                      CALL SHOWIT(1)
                      OUTLYNE='TELECENTRIC AIMING HAS BEEN SHUT OFF'
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF
C       COMMANDS (PIKD)
              IF(WC.EQ.'PIKD') THEN
                  CALL SPIKD
                  RETURN
              END IF
          END IF
          RETURN
      END

      SUBROUTINE RE_DISPLAY_LENS(YESEOS)
          USE opsys
          USE GLOBALS
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INTEGER SAVESURF
          LOGICAL YESEOS
          SAVE SAVESURF
          SAVESURF=SURF
          SAVE_KDP(21)=SAVEINPT(21)
          LNSTYP=1
          CALL LNSEOS
          REST_KDP(21)=RESTINPT(21)
          SAVE_KDP(21)=SAVEINPT(21)
          CALL shell_command('clear')
          REST_KDP(21)=RESTINPT(21)
          IF(.NOT.YESEOS) THEN
              SAVE_KDP(21)=SAVEINPT(21)
              INPUT='RTG ALL'
              CALL PROCES
              REST_KDP(21)=RESTINPT(21)
              SAVE_KDP(21)=SAVEINPT(21)
              INPUT='U L'
              CALL PROCES
              REST_KDP(21)=RESTINPT(21)
              SURF=SAVESURF
          END IF
          RETURN
      END

C SUB LLIBRY.FOR
      SUBROUTINE LLIBRY
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS LENS LIBRARY COMMANDS
C       LIB P, LIB PUT AND LIB GET.
C     ALSO GETMG, PMG (MELLES GRIOT)       DIRECTORY \LIBLMG
C     ALSO GETES, PES (EDMUND SCIENTIFIC)  DIRECTORY \LIBLES
C     ALSO GETNC, PNC (NEWPORT)            DIRECTORY \LIBLNC
C     ALSO GETSH, PSH (SPINDLER & HOYER)   DIRECTORY \LIBLSH
C     ALSO GETRO, PRO (ROLAN)              DIRECTORY \LIBLRO
C
C                       DEFINE VARIABLES
C
          CHARACTER DATA*80,AI4*4,AI*3,
     1    LLIP*80,WCOLD*8,DDATE*10,TTIME*8,AN*3
     2    ,FN*10,NM*8,TTTIM*8,DDDAT*10,LNDIR*90
C
          LOGICAL EXISJK,EXISTAG,GETERROR
C
          COMMON/ERRORGET/GETERROR
C
          COMMON/STRNGR/DDDAT,TTTIM,NM,FN
C
          INTEGER SING,LCNT,I,II,J,N,OLDOUT,
     1    OLDSQ,OLDSN,OLDIN,IREND,N3,JJ,OLDOUT1
C
          INTEGER FLG(0:20),ALL
C
          LOGICAL ITERROR

          COMMON/FFL/FLG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsub.inc'
C
          EXISTAG=.FALSE.
          INQUIRE(FILE=trim(LIBLEN)//'LIBTAG.DAT',EXIST=EXISTAG)
C
C       SET ALL=0
          ALL=0
C       SET SING=0
          SING=0
C
C*************************************************************
C       SPECIFIC CHECKS FOR LIB P
C
C       LIB P CAN TAKE NUMERIC INPUT FROM NW1 AND NW2
C       NW1 CAN BE 1 TO 999 OR BLANK
C       NW2 CAN BE 2 TO 999 OR BLANK
C       NW1 MUST BE LESS THAN NW2
C
          IF(WQ.EQ.'P'.OR.WQ.EQ.'PMG'.OR.WQ.EQ.'PES'.OR.WQ.EQ.'PNC'
     1    .OR.WQ.EQ.'PSH'.OR.WQ.EQ.'PRO') THEN
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"LIB" TAKES NO NUMERIC WORD #3 TO #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.1.0.AND.DF1.NE.1.OR.
     1        W1.GT.999.0.AND.DF1.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 1 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.2.0.AND.DF2.NE.1.OR.
     1        W2.GT.999.0.AND.DF2.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 2 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1) THEN
C
                  IF(W1.GE.W2) THEN
                      OUTLYNE=
     1                'NUMERIC WORD 1 MUST BE LESS THAN NUMERIC WORD 2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  SING=0
              ELSE
C       NW2 IS BLANK
C       SET SING=1
                  SING=1
              END IF
C       IS NW1 AND NW2 DEFAULT
              IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
C       NO NUMERIC INPUT, SET THE ALL VARIABLE TO 1
                  ALL=1
              END IF
          END IF
C**********************************************************
C       SPECIFIC CHECKS FOR LIB PUT
C
C       LIB PUT CAN TAKE NUMERIC INPUT FROM NW1 ONLY
C       NW1 CAN BE 1 TO 999 OR BLANK
C
          IF(WQ.EQ.'PUT') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"LIB PUT" TAKES NO NUMERIC WORD #2 TO #5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.1.0.AND.DF1.NE.1.OR.
     1        W1.GT.999.0.AND.DF1.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 1 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       IS NW1 DEFAULT
              IF(DF1.EQ.1) THEN
C       NO NUMERIC INPUT, SET THE ALL VARIABLE TO 1
                  ALL=1
              END IF
          END IF

C**********************************************************
C       SPECIFIC CHECKS FOR LIB GET
C
C       LIB GET CAN TAKE NUMERIC INPUT FROM NW1 ONLY
C       NW1 CAN BE 1 TO 999 OR BLANK
C
          IF(WQ.EQ.'GET'.OR.WQ.EQ.'GETMG'.OR.WQ.EQ.'GETES'.OR.
     1    WQ.EQ.'GETNC'.OR.WQ.EQ.'GETSH'
     2    .OR.WQ.EQ.'GETRO') THEN
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  IF(WQ.EQ.'GET') THEN
                      OUTLYNE='"LIB '//WQ(1:3)//
     1                '" ONLY TAKES NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  ELSE
                      OUTLYNE='"LIB '//WQ(1:5)//
     1                '" ONLY TAKES NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                  END IF
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.1.0.AND.DF1.NE.1.OR.
     1        W1.GT.999.0.AND.DF1.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 1 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       IS NW1 DEFAULT
              IF(WQ.EQ.'GET') THEN
                  IF(DF1.EQ.1) THEN
                      OUTLYNE='"LIB GET" REQUIRES EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              ELSE
                  IF(DF1.EQ.1) THEN
                      OUTLYNE='"LIB '//WQ(1:5)//
     1                '" REQUIRES EXPLICIT NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C*************************************************************
C       SPECIFIC CHECKS FOR LIB DEL
C
C       LIB DEL CAN TAKE NUMERIC INPUT FROM NW1 AND NW2
C       NW1 CAN BE 1 TO 999
C       NW2 CAN BE 2 TO 999
C       NW1 MUST BE LESS THAN NW2 IF NW2 NOT DEFAULT
C
          IF(WQ.EQ.'DEL') THEN
              IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE='"LIB DEL" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.1.0.AND.DF1.NE.1.OR.
     1        W1.GT.999.0.AND.DF1.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 1 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.2.0.AND.DF2.NE.1.OR.
     1        W2.GT.999.0.AND.DF2.NE.1) THEN
                  OUTLYNE='NUMERIC WORD 2 BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1) THEN
C
                  IF(W1.GE.W2) THEN
                      OUTLYNE=
     1                'NUMERIC WORD 1 MUST BE LESS THAN NUMERIC WORD 2'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  SING=0
              ELSE
C       NW2 IS BLANK
C       SET SING=1
                  SING=1
              END IF
C       IS NW1 AND NW2 DEFAULT
              IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
                  OUTLYNE='"LIB DEL" REQUIRES EXPLICIT NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C***********************************************************
C       IF YOU GOT HERE, INPUT WAS OK
C       PROCEED WITH PROCESSING
C
          IF(WQ.EQ.'PUT') THEN
C       FIRST LIB PUT
C       WHEN LIB PUT IS USED, AN ENTRY IS MADE INTO AN AVALIABLE
C       SLOT IN LIB.DAT (THE LENS LIBRARY DIRECTORY)
C     AND ALSO INTO LIBTAG.DAT (HOLDS THE LENS IDENTIFIER TAG)
C     WHICH IS UNIT 27
C       DEPENDING ON WHICH SLOT WAS USED, A FILE
C       LIB001.DAT TO LIB999.DAT IS USED TO THEN STORE THE
C       LENS DATA IN BINARY FORMAT. LIB.DAT IS UNIT 22
C
C       IF ALL=1 THEN WE MUST SEARCH FOR AN AVALIABLE SLOT
C
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=trim(LIBLEN)//'LIB.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  IF(trim(LIBLEN).NE.trim(HOME)//'LIBLEN/') THEN
                      WRITE(OUTLYNE,*)
     1                'THE LENS LIBRARY IN LIBRARY DIRECTORY = ',trim(LIBLEN)
                      CALL SHOWIT(1)
                      OUTLYNE='DOES NOT EXIST. BEFORE USING "ILF" AND "PROCEED"'
                      CALL SHOWIT(1)
                      OUTLYNE='MAKE SURE YOU CHANGE TO THE DESIRED LENS LIBRARY'
                      CALL SHOWIT(1)
                  ELSE
                      OUTLYNE='"THE DEFAULT LENS LIBRARY DOES NOT YET EXIST'
                      CALL SHOWIT(1)
                      OUTLYNE='TO INITIALIZE IT, USE "ILF" AND "PROCEED"'
                      CALL SHOWIT(1)
                      OUTLYNE='BE CAREFUL, YOU MIGHT BE DELEATING YOUR LENS LIBRARY'
                      CALL SHOWIT(1)
                      OUTLYNE='BACK UP THE LENS LIBRARY FIRST WITH'
                      CALL SHOWIT(1)
                      OUTLYNE='THE "LIBSAVE" COMMAND'
                      CALL SHOWIT(1)
                  END IF
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************

              OPEN(UNIT=22,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIB.DAT',
     1        FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
              IF(EXISTAG)
     1        OPEN(UNIT=27,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIBTAG.DAT',
     1        FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
              IF(ALL.EQ.1) THEN
                  DO 10 I=1,999
                      READ(UNIT=22,REC=I) II,DATA
                      IF(EXISTAG) THEN
                          DO J=1,10
                              READ(UNIT=27,REC=I-1+J) IDTAG(J)(1:75)
                          END DO
                      END IF
                      IF(II.EQ.0) THEN
C       FOUND AN EMPTY SLOT
                          II=I
                          GO TO 11
                      END IF
 10               CONTINUE
C       IF YOU GOT HERE, LIBRARY WAS FULL
                  OUTLYNE='LENS LIBRARY FULL, LENS NOT FILED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
 11               CONTINUE
C       PROCEED WITH STORAGE
              ELSE
C       ALL NOT 1 SET II=INT(W1)
                  II=INT(W1)
                  READ(UNIT=22,REC=II) J,DATA
                  IF(EXISTAG) THEN
                      DO JJ=1,10
                          READ(UNIT=27,REC=II-1+JJ) IDTAG(JJ)(1:75)
                      END DO
                  END IF
                  IF(J.NE.0) THEN
C       SLOT OCCUPIED
                      WRITE(OUTLYNE,*)'LENS LIBRARY FILE NO. ',J,' OCCUPIED'
                      CALL SHOWIT(1)
                      OUTLYNE='LENS NOT FILED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C       FILE THE NUMBER AND NAME IN THE LIBRARY
              CALL MYDATE(DDATE)
              CALL MYTIME(TTIME)
              DDDAT=DDATE
              TTTIM=TTIME
              IF(SST.EQ.1) THEN
                  LI=WS
                  LIP=WS
              END IF
              LLIP=DDATE//' '//TTIME//' '//LIP(1:59)
              WRITE(UNIT=22,REC=II) II,LLIP
              CALL DOGTAG
              IF(EXISTAG) THEN
                  DO J=1,10
                      WRITE(UNIT=27,REC=II-1+J) IDTAG(J)(1:75)
                  END DO
              END IF
              CALL CLOSE_FILE(22,1)
              IF(EXISTAG) CALL CLOSE_FILE(27,1)
C
C       NOW DETERMINE WHICH OF THE 999 LENS LIBRARY FILES
C       TO USE
C
              N=II
              CALL CCOONN(N,AN)
              IF(N.GT.0.AND.N.LE.9) FN='LIB00'//AN(3:3)//'.DAT'
              IF(N.GT.9.AND.N.LE.99) FN='LIB0'//AN(2:3)//'.DAT'
              IF(N.GT.99.AND.N.LE.999) FN='LIB'//AN(1:3)//'.DAT'
              OPEN(UNIT=33,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(LIBLEN)//FN
     2        ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(33,0)
              OPEN(UNIT=33,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(LIBLEN)//FN
     2        ,STATUS='UNKNOWN')
C
C**********************************************************
C       NOW OUTPUT LENS TO LIBXXX.DAT
C     DO A FORCED RETURN TO CONFIG 1
              IF(F15.EQ.0.AND.F12.NE.1) CALL FRCCF1(1)
C
              OLDOUT1=OUT
              OLDSQ=SQ
              OUT=33
C     SET SQ=0
              SQ=0
              OLDSN=SN
              SN=0
              SST=0
              WQ='        '
              WCOLD=WC
              WC='LENO'
              CALL LENOUT
              WC=WCOLD
              SQ=1
              SN=OLDSN
              WQ='PUT'
              OUT=OLDOUT1
C
              REWIND(UNIT=33)
C
              CALL CLOSE_FILE(33,1)
              WRITE(OUTLYNE,*)'LENS ID=',LIP(1:71)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'LENS STORED IN LIBRARY FILE NO. ',II
              CALL SHOWIT(1)
              RETURN
          END IF
C
C       NOW LIB GET
C
          IF(WQ.EQ.'GET'.OR.WQ.EQ.'GETMG'.OR.WQ.EQ.'GETES'.OR.
     1    WQ.EQ.'GETNC'.OR.WQ.EQ.'GETSH'
     2    .OR.WQ.EQ.'GETRO') THEN
C
C       WHEN LIB GET IS USED, A CHECK TO SEE IF THE SPECIFIC
C       LIBRARY ENTRY IN VACANT IS MADE. IF NOT VACANT,
C       LENS DATA IS READ IN FROM A SPECIFIC FILE.
C       IN IT IS VACANT, PRINT MESAGE AND STOP
C
C       ***************************************************************
              IF(WQ.EQ.'GET')   LNDIR= trim(LIBLEN)
              IF(WQ.EQ.'GETMG') LNDIR='LIBLMG/'
              IF(WQ.EQ.'GETES') LNDIR='LIBLES/'
              IF(WQ.EQ.'GETNC') LNDIR='LIBLNC/'
              IF(WQ.EQ.'GETSH') LNDIR='LIBLSH/'
              IF(WQ.EQ.'GETRO') LNDIR='LIBLRO/'
              IF(WC(1:3).EQ.'LIB'.AND.WQ(1:3).EQ.'GET') LIBGET=.TRUE.
              EXISJK=.FALSE.
              INQUIRE(FILE=trim(LNDIR)//'LIB.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  WRITE(OUTLYNE,*)
     1            'THE LENS LIBRARY IN LIBRARY DIRECTORY = ',trim(LNDIR)
                  CALL SHOWIT(1)
                  OUTLYNE='DOES NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='USE THE "LODLENS" MACRO TO RELOAD THE OFF-THE-SHELF'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS LIBRARIES'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=22,ACCESS='DIRECT',FILE=trim(LNDIR)//'LIB.DAT',
     1        FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
              IF(EXISTAG)
     1        OPEN(UNIT=27,ACCESS='DIRECT',FILE=trim(LNDIR)//'LIBTAG.DAT',
     1          FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
              READ(UNIT=22,REC=INT(W1)) II,DATA
              IF(EXISTAG) THEN
                  DO J=1,10
                      READ(UNIT=27,REC=INT(W1)-1+J) IDTAG(J)(1:75)
                  END DO
              END IF
              CALL CLOSE_FILE(22,1)
              IF(EXISTAG) CALL CLOSE_FILE(27,1)
              GETERROR=.FALSE.
              IF(II.EQ.0) THEN
                  GETERROR=.TRUE.
C       FOUND AN EMPTY SLOT
                  WRITE(OUTLYNE,*)'LENS LIBRARY FILE NO. ',INT(W1),' IS EMPTY'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       NOT EMPTY, READ DATA
                  II=INT(W1)
              END IF
C
C       NOW DETERMINE WHICH OF THE 999 LENS LIBRARY FILES
C       TO USE
              N=II
              CALL CCOONN(N,AN)
              IF(N.GT.0.AND.N.LE.9) FN='LIB00'//AN(3:3)//'.DAT'
              IF(N.GT.9.AND.N.LE.99) FN='LIB0'//AN(2:3)//'.DAT'
              IF(N.GT.99.AND.N.LE.999) FN='LIB'//AN(1:3)//'.DAT'
              OPEN(UNIT=33,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(LNDIR)//FN
     2        ,STATUS='UNKNOWN')
C**********************************************************
              REFEXT=.FALSE.
C
C     SET GLOBE TO FALSE
              GLOBE=.FALSE.
C     SET OPCNT,VBCNT AND DEREXT TO CORRECT VALUES
              ISTOP(1)=.FALSE.
              ISTOP(2)=.FALSE.
              ISTOP(3)=.FALSE.
              ISTOP(4)=.FALSE.
              ISTOP(5)=.FALSE.
              ISCOMP(1)=.FALSE.
              ISCOMP(2)=.FALSE.
              ISCOMP(3)=.FALSE.
              ISCOMP(4)=.FALSE.
              ISCOMP(5)=.FALSE.
              ISCRIT(1)=.FALSE.
              ISCRIT(2)=.FALSE.
              ISCRIT(3)=.FALSE.
              ISCRIT(4)=.FALSE.
              ISCRIT(5)=.FALSE.
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
              REWIND(UNIT=33)
              OLDIN=IN
              OLDOUT=OUT
              IN=33
              OUT=33
 3141         READ(UNIT=33,FMT=100,END=8888,ERR=8888) INPUT
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
                      READ(33,*) AI,FIELDY(I),FIELDX(I),
     1                FIELDZ(I),N3
                      FIELDW(I)=DBLE(N3)
                      IF(FIELDW(I).EQ.0.0D0) THEN
                          FIELDW(I)=SYSTEM1(11)
                      END IF
                  END DO
                  DO I=1,IREND
                      READ(33,*,ERR=8888,END=8888) AI4,RAYY(I),RAYX(I),N3
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
     1        INPUT(1:1).EQ.'1'.OR.
     1        INPUT(1:1).EQ.'2'.OR.
     1        INPUT(1:1).EQ.'3'.OR.
     1        INPUT(1:1).EQ.'4'.OR.
     1        INPUT(1:1).EQ.'5'.OR.
     1        INPUT(1:1).EQ.'6'.OR.
     1        INPUT(1:1).EQ.'7'.OR.
     1        INPUT(1:1).EQ.'8'.OR.
     1        INPUT(1:1).EQ.'9'.OR.
     1        INPUT(1:1).EQ.'0'
     1        .OR.INPUT(1:8).EQ.'FLDSRAYS') THEN
              ELSE
                  CALL PROCES
              END IF
              GO TO 3141
C     LIBRARY LENS HAS BEEN LOADED
 8888         BACKSPACE(UNIT=33)
              REWIND (UNIT=33)
 8889         CALL CLOSE_FILE(33,1)
              IN=OLDIN
              OUT=OLDOUT
              LIBGET=.FALSE.
C
C       CURRENT LENS IS LOADED INTO THE LENS DATA STORAGE.
C
              WRITE(OUTLYNE,*)'LENS ID=',LIP(1:71)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RESTORED LIBRARY LENS NO. ',II
              CALL SHOWIT(1)
              RETURN
          END IF
C
C       NOW LIB P
          IF(WQ.EQ.'P'.OR.WQ.EQ.'PMG'.OR.WQ.EQ.'PES'.OR.WQ.EQ.'PNC'
     1    .OR.WQ.EQ.'PSH'.OR.WQ.EQ.'PRO') THEN
C
C       WHEN LIB P IS USED, PRINT OUT A PORTION
C       OF THE LENS LIBRARY DIRECTORY LIB.DAT
C
C       ***************************************************************
              IF(WQ.EQ.'P')   LNDIR= trim(LIBLEN)
              IF(WQ.EQ.'PMG') LNDIR='LIBLMG/'
              IF(WQ.EQ.'PES') LNDIR='LIBLES/'
              IF(WQ.EQ.'PNC') LNDIR='LIBLNC/'
              IF(WQ.EQ.'PSH') LNDIR='LIBLSH/'
              IF(WQ.EQ.'PRO') LNDIR='LIBLRO/'
              EXISJK=.FALSE.
              INQUIRE(FILE=trim(LNDIR)//'LIB.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  WRITE(OUTLYNE,*)
     1            'THE LENS LIBRARY IN LIBRARY DIRECTORY = ',trim(LNDIR)
                  CALL SHOWIT(1)
                  OUTLYNE='DOES NOT EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='USE THE "LODLENS" MACRO TO RELOAD THE OFF-THE-SHELF'
                  CALL SHOWIT(1)
                  OUTLYNE='LENS LIBRARIES'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=22,ACCESS='DIRECT',FILE=trim(LNDIR)//'LIB.DAT',
     1        FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
              IF(EXISTAG)
     1        OPEN(UNIT=27,ACCESS='DIRECT',FILE=trim(LNDIR)//'LIBTAG.DAT',
     1        FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
              IF(ALL.EQ.1) THEN
                  LCNT=0
 123              FORMAT(1X)
                  WRITE(OUTLYNE,123)
                  CALL SHOWIT(0)
                  DO 817 I=1,999
                      READ(UNIT=22,REC=I) II,DATA
                      IF(EXISTAG) THEN
                          DO J=1,10
                              READ(UNIT=27,REC=I-1+J) IDTAG(J)(1:75)
                          END DO
                      END IF
                      IF(II.EQ.0) THEN
C       FOUND AN EMPTY SLOT
C       NO OUTPUT
                          GO TO 817
                      ELSE
C       NOT EMPTY, WRITE OUTPUT
                          LCNT=LCNT+1
                          IF(I.LT.10) THEN
                              WRITE(OUTLYNE,3994)I,DATA(1:75)
                              CALL SHOWIT(0)
                          END IF
                          IF(I.GE.10.AND.I.LE.99) THEN
                              WRITE(OUTLYNE,3996)I,DATA(1:75)
                              CALL SHOWIT(0)
                          END IF
                          IF(I.GE.100) THEN
                              WRITE(OUTLYNE,3997)I,DATA(1:75)
                              CALL SHOWIT(0)
                          END IF
 3994                     FORMAT('00',I1,1X,A75)
 3996                     FORMAT('0',I2,1X,A75)
 3997                     FORMAT(I3,1X,A75)
                      END IF
 817              CONTINUE
                  CALL CLOSE_FILE(22,1)
                  IF(EXISTAG) CALL CLOSE_FILE(27,1)
                  IF(LCNT.EQ.0) THEN
                      WRITE(OUTLYNE,3995)
                      CALL SHOWIT(0)
 3995                 FORMAT('LENS LIBRARY EMPTY')
                      RETURN
                  END IF
                  RETURN
              END IF
              IF(SING.EQ.1) THEN
                  READ(UNIT=22,REC=INT(W1))II,DATA
                  IF(EXISTAG) THEN
                      DO J=1,10
                          READ(UNIT=27,REC=INT(W1)-1+J) IDTAG(J)(1:75)
                      END DO
                  END IF
                  CALL CLOSE_FILE(22,1)
                  IF(EXISTAG) CALL CLOSE_FILE(27,1)
                  IF(II.NE.0) THEN
                      I=INT(W1)
                      IF(I.LT.10) THEN
                          WRITE(OUTLYNE,3994)I,DATA(1:75)
                          CALL SHOWIT(0)
                      END IF
                      IF(I.GE.10.AND.I.LE.99) THEN
                          WRITE(OUTLYNE,3996)I,DATA(1:75)
                          CALL SHOWIT(0)
                      END IF
                      IF(I.GE.100) THEN
                          WRITE(OUTLYNE,3997)I,DATA(1:75)
                          CALL SHOWIT(0)
                      END IF
                      RETURN
 9934                 FORMAT('LENS LIBRARY ENTRY ',I3,' IS CURRENTLY EMPTY')
                  ELSE
                      WRITE(OUTLYNE,9934) INT(W1)
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF
C
C       PRINT SEVERAL ENTRIES
              DO 819 I=INT(W1),INT(W2)
                  READ(UNIT=22,REC=I) II,DATA
                  IF(EXISTAG) THEN
                      DO J=1,10
                          READ(UNIT=27,REC=I-1+J) IDTAG(J)(1:75)
                      END DO
                  END IF
                  IF(II.EQ.0) THEN
C       FOUND AN EMPTY SLOT
C       NO OUTPUT
                      GO TO 819
                  ELSE
C       NOT EMPTY, WRITE OUTPUT
                      IF(I.LT.10) THEN
                          WRITE(OUTLYNE,3994)I,DATA(1:75)
                          CALL SHOWIT(0)
                      END IF
                      IF(I.GE.10.AND.I.LE.99) THEN
                          WRITE(OUTLYNE,3996)I,DATA(1:75)
                          CALL SHOWIT(0)
                      END IF
                      IF(I.GE.100) THEN
                          WRITE(OUTLYNE,3997)I,DATA(1:75)
                          CALL SHOWIT(0)
                      END IF
                  END IF
 819          CONTINUE
              CALL CLOSE_FILE(22,1)
              IF(EXISTAG) CALL CLOSE_FILE(27,1)
              RETURN
C
          END IF
C***********************************************************
C       NOW LIB DEL
          IF(WQ.EQ.'DEL') THEN
C
C       WHEN LIB DEL IS USED,DELETE SPECIFIC ENTRIES IN THE
C       DIRECTORY AND DELETE SPECIFIC FILES
C
              IF(SING.EQ.1) THEN
C       ***************************************************************
                  EXISJK=.FALSE.
                  INQUIRE(FILE=trim(LIBLEN)//'LIB.DAT',EXIST=EXISJK)
                  IF(.NOT.EXISJK) THEN
                      IF(trim(LIBLEN).NE.trim(HOME)//'LIBLEN/') THEN
                          WRITE(OUTLYNE,*)
     1                    'THE LENS LIBRARY IN LIBRARY DIRECTORY = ',trim(LIBLEN)
                          CALL SHOWIT(1)
                          OUTLYNE='DOES NOT EXIST. BEFORE USING "ILF" AND "PROCEED"'
                          CALL SHOWIT(1)
                          OUTLYNE='MAKE SURE YOU CHANGE TO THE DESIRED LENS LIBRARY'
                          CALL SHOWIT(1)
                      ELSE
                          OUTLYNE='"THE DEFAULT LENS LIBRARY DOES NOT YET EXIST'
                          CALL SHOWIT(1)
                          OUTLYNE='TO INITIALIZE IT, USE "ILF" AND "PROCEED"'
                          CALL SHOWIT(1)
                          OUTLYNE='BE CAREFUL, YOU MIGHT BE DELEATING YOUR LENS LIBRARY'
                          CALL SHOWIT(1)
                          OUTLYNE='BACK UP THE LENS LIBRARY FIRST WITH'
                          CALL SHOWIT(1)
                          OUTLYNE='THE "LIBSAVE" COMMAND'
                          CALL SHOWIT(1)
                      END IF
                      CALL MACFAL
                      RETURN
                  END IF
C       ***************************************************************
                  OPEN(UNIT=22,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIB.DAT',
     1            FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
                  IF(EXISTAG)
     1            OPEN(UNIT=27,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIBTAG.DAT',
     1            FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
                  READ(UNIT=22,REC=INT(W1))II,DATA
                  IF(EXISTAG) THEN
                      DO J=1,10
                          READ(UNIT=27,REC=INT(W1)-1+J) IDTAG(J)(1:75)
                      END DO
                  END IF
                  IF(II.EQ.0) THEN
                      WRITE(OUTLYNE,*)'LENS LIBRARY FILE NO. ',INT(W1),' ALREADY EMPTY'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C       PROCEED WITH DELETION
                      II=0
                      DATA=AA//AA//AA//AA
                      WRITE(UNIT=22,REC=INT(W1))II,DATA
                      IF(EXISTAG) WRITE(UNIT=27,REC=INT(W1)) AA//AA//AA//AA(1:15)
                      CALL CLOSE_FILE(22,1)
                      IF(EXISTAG)  CALL CLOSE_FILE(27,1)
                      II=INT(W1)
C       DUMP THE LIBRARY FILE
                      N=II
                      CALL CCOONN(N,AN)
                      IF(N.GT.0.AND.N.LE.9) FN='LIB00'//AN(3:3)//'.DAT'
                      IF(N.GT.9.AND.N.LE.99) FN='LIB0'//AN(2:3)//'.DAT'
                      IF(N.GT.99.AND.N.LE.999) FN='LIB'//AN(1:3)//'.DAT'
                      OPEN(UNIT=33,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                ,FORM='FORMATTED',FILE=trim(LIBLEN)//FN
     2                ,STATUS='UNKNOWN')
                      CALL CLOSE_FILE(33,0)
                      WRITE(OUTLYNE,*)'LENS LIBRARY FILE NO. ',INT(W1),' DELETED'
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF
C
C       DELETE SEVERAL LIBRARY FILES
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=trim(LIBLEN)//'LIB.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  IF(trim(LIBLEN).NE.trim(HOME)//'LIBLEN/') THEN
                      WRITE(OUTLYNE,*)
     1                'THE LENS LIBRARY IN LIBRARY DIRECTORY = ',trim(LIBLEN)
                      CALL SHOWIT(1)
                      OUTLYNE='DOES NOT EXIST. BEFORE USING "ILF" AND "PROCEED"'
                      CALL SHOWIT(1)
                      OUTLYNE='MAKE SURE YOU CHANGE TO THE DESIRED LENS LIBRARY'
                      CALL SHOWIT(1)
                  ELSE
                      OUTLYNE='"THE DEFAULT LENS LIBRARY DOES NOT YET EXIST'
                      CALL SHOWIT(1)
                      OUTLYNE='TO INITIALIZE IT, USE "ILF" AND "PROCEED"'
                      CALL SHOWIT(1)
                      OUTLYNE='BE CAREFUL, YOU MIGHT BE DELEATING YOUR LENS LIBRARY'
                      CALL SHOWIT(1)
                      OUTLYNE='BACK UP THE LENS LIBRARY FIRST WITH'
                      CALL SHOWIT(1)
                      OUTLYNE='THE "LIBSAVE" COMMAND'
                      CALL SHOWIT(1)
                  END IF
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              DO 8191 I=INT(W1),INT(W2)
                  OPEN(UNIT=22,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIB.DAT',
     1            FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
                  IF(EXISTAG)
     1            OPEN(UNIT=27,ACCESS='DIRECT',FILE=trim(LIBLEN)//'LIBTAG.DAT',
     1            FORM='UNFORMATTED',RECL=(84*NRECL),STATUS='UNKNOWN')
                  READ(UNIT=22,REC=I) II,DATA
                  IF(EXISTAG) THEN
                      DO J=1,10
                          READ(UNIT=27,REC=I-1+J) IDTAG(J)(1:75)
                      END DO
                  END IF
                  IF(II.EQ.0) THEN
C       FOUND AN EMPTY SLOT
C       NO DELETION
                      WRITE(OUTLYNE,*)'LENS LIBRARY FILE NO. ',I,' ALREADY EMPTY'
                      CALL SHOWIT(1)
                      CALL CLOSE_FILE(22,1)
                      IF(EXISTAG) CALL CLOSE_FILE(27,1)
                      CALL MACFAL
                      GO TO 8191
                  ELSE
C       NOT EMPTY, DELETE ENTRY
                      II=0
                      DATA=AA//AA//AA//AA
                      WRITE(UNIT=22,REC=I)II,DATA
                      CALL DOGTAG
                      IF(EXISTAG) THEN
                          DO J=1,10
                              WRITE(UNIT=27,REC=I-1+J) IDTAG(J)(1:75)
                          END DO
                      END IF
                      CALL CLOSE_FILE(22,1)
                      IF(EXISTAG) CALL CLOSE_FILE(27,1)
                      II=I
C       DUMP THE LIBRARY FILE
                      N=II
                      CALL CCOONN(N,AN)
                      IF(N.GT.0.AND.N.LE.9) FN='LIB00'//AN(3:3)//'.DAT'
                      IF(N.GT.9.AND.N.LE.99) FN='LIB0'//AN(2:3)//'.DAT'
                      IF(N.GT.99.AND.N.LE.999) FN='LIB'//AN(1:3)//'.DAT'
                      OPEN(UNIT=33,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                ,FORM='FORMATTED',FILE=trim(LIBLEN)//FN
     2                ,STATUS='UNKNOWN')
                      CALL CLOSE_FILE(33,0)
                      WRITE(OUTLYNE,*)'LENS LIBRARY FILE NO. ',I,' DELETED'
                      CALL SHOWIT(1)
                  END IF
 8191         CONTINUE
              RETURN
          END IF
 100      FORMAT(A140)
      END
