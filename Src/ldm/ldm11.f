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

C       ELEVENTH FILE FOR LENS DATABASE MANAGER FILES

C SUB AUXCFG.FOR
      SUBROUTINE AUXCFG
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE AUXCFG. THIS IS THE SUBROUTINE WHICH
C       SETS UP THE FAST ACCESS CFG ARRAYS AT THE END OF THE EOS
C       PROCESS FROM LENS MODIFICATIONS. IT IS CALLED FROM LNSEOS.
C
          CHARACTER AN1*25
C
          INTEGER I,J,K,ALLOERR
C
          REAL*8 N1,SURFNM
C
          COMMON/CAUX1/N1,AN1
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datsub.inc'
C
          CHARACTER SCRATH*140,BLANK*140
          DIMENSION SCRATH(:)
          ALLOCATABLE :: SCRATH
          INTEGER NANA
          NANA=2000
          DEALLOCATE (SCRATH,STAT=ALLOERR)
          ALLOCATE (SCRATH(NANA),STAT=ALLOERR)
          BLANK=AA//AA//AA//AA//AA//AA//AA
          SCRATH(1:2000)=BLANK
          AUXMAX=0
C
          LASCFG=INT(SYSTEM1(56))
C       LASCFG IS THE NUMBER OF THE LAST NON ZERO CONFIGURATION
C       IF LASTCFG=1, JUST RETURN, THERE IS NOTHING TO DO
          IF(LASCFG.LE.1) THEN
              DEALLOCATE(SCRATH,STAT=ALLOERR)
              RETURN
          END IF
          IF(LASCFG.GT.1) THEN
C       PROPERLY LOAD THE FAST ARRAYS AND THEN RETURN
              K=0
              AUXMAX=K
              DO I=2,LASCFG
C       SET ULON AND USON BOTH TO FALSE
C       THESE TRACK IF U L OR U SP EXISTS IN CONFIG FILE
                  ULON=.FALSE.
                  USON=.FALSE.
C       LOAD ARRAYS FOR CFG I
C       THERE IS AN ADDRESS ARRAY AND A VALUE ARRAY
C       CFADD AND CFVAL
C       CFADD(ENTRY#,DATA TYPE#)
C       DATA TYPE = 1 (USE SAME CODE AS IN VARIABLES SUBFILE)
C       SURFACE # = 2
C       CFG #     = 3
C       ADD1      = 4
C       ADD2      = 5
C       ADD3      = 6
C       ADD4      = 7
C       PIKSOL    = 8
C       J(ENTRY NUMBER IN THE ITH CONFIG)=9
C     ENTRY# IS A CHRONOLOGICAL LISTING NUMBER IN THE ARRAY
C     ADD1 AND ADD2 ARE THE STARTING AND ENDING FIELDS FOR
C     NW1 DATA, ADD3 AND ADD4 ARE STARTING AND ENDING FIELDS
C     FOR NW2 DATA.
C     PIKSOL IS SET TO NON-ZERO, IF THE VALUE TYPE IS BEING
C     SET USING A PIKUP OR A SOLVE SINCE IT IS IMPORTANT
C     TO KNOW THAT WHEN TRYING TO OPTIMIZE.
C     SOLVE AND PIKUP TYPES ARE
C               NON-ZERO PIKSOL VALUES REPRESENT:
C     PIKUP RD    = 1
C     PIKUP CV    = 2
C     PIKUP CC    = 3
C     PIKUP TH    = 4
C     PIKUP AD    = 5
C     PIKUP AE    = 6
C     PIKUP AF    = 7
C     PIKUP AG    = 8
C     PIKUP RDTOR = 9
C     PIKUP CVTOR = 10
C     PIKUP CCTOR = 11
C     PIKUP ADTOR = 12
C     PIKUP AETOR = 13
C     PIKUP AFTOR = 14
C     PIKUP AGTOR = 15
C     PIKUP ALPHA = 16
C     PIKUP BETA  = 17
C     PIKUP GAMMA = 18
C     PIKUP XD    = 19
C     PIKUP YD    = 20
C     PIKUP GLASS = 21
C     PIKUP PRO   = 22
C     PIKUP NPRO  = 23
C     SOLVE PY    = 24
C     SOLVE PX    = 25
C     SOLVE PCY   = 26
C     SOLVE PCX   = 27
C     SOLVE PUY   = 28
C     SOLVE PUX   = 29
C     SOLVE PUCY  = 30
C     SOLVE PUCX  = 31
C     SOLVE COCY  = 32
C     SOLVE COCX  = 33
C     PIKUP ZD    = 34
C     PIKUP PIVX  = 35
C     PIKUP PIVY  = 36
C     PIKUP PIVZ  = 37
C     PIKUP GDX   = 38
C     PIKUP GDY   = 39
C     PIKUP GDZ   = 40
C     PIKUP GALPHA= 41
C     PIKUP GBETA = 42
C     PIKUP GGAMMA= 43
C     PIKUP GRT   = 44
C
C     IF AN ITEM IS SET USING A PIKUP OR SOLVE, IT CAN'T BE
C     USED IN THE VARIABLES SUBFILE AND A MESSAGE TO THAT
C     EFFECT WILL BE ISSUED WHEN AN ATTEMP IS MADE TO ADD THAT
C     ITEM TO THE VARIABLES OR TVARIABLES SUBFILE.
C
C     CFVAL(ENTRY#,DATA TYPE#)
C     ENTRY# IS A CRONOLOGICAL LISTING NUMBER IN THE ARRAY (SAME
C     AS FOR CFADD
C     DATA TYPE#=1 FOR NW1 VALUE
C     DATA TYPE#=2 FOR NW2 VALUE
C     CFCHAR(ENTRY#,DATA TYPE#)
C     ENTRY# IS A CRONOLOGICAL LISTING NUMBER IN THE ARRAY (SAME
C     AS FOR CFADD
C     DATA TYPE#=1 > CHARACTER REPRESENTATION OF NW1 VALUE
C     DATA TYPE#=2 > CHARACTER REPRESENTATION OF NW2 VALUE
C     THERE COULD BE A NEED FOR (MAXCFG-1)*198 ENTRY NUMBERS
C     IF ALL ALTERNATE CONFIGS HAD ONLY VARIABLE TYPE CHANGES
C
                  SCRATH(1:CFGCNT(I))=CONFG(I,1:CFGCNT(I))
                  DO J=1,CFGCNT(I)
C     WE ARE INSIDE CONFIG #I NOW, THERE ARE CFGCNT(I) ENTRIES
C     FOR CONFIG(I)
                      IF(SCRATH(J)(1:8).EQ.'U       '.AND.
     1                   SCRATH(J)(10:17).EQ.'L       '.OR.
     2                   SCRATH(J)(1:8).EQ.'UPDATE  '.AND.
     1                   SCRATH(J)(10:17).EQ.'LENS    '.OR.
     1                   SCRATH(J)(1:8).EQ.'UPDATE  '.AND.
     1                   SCRATH(J)(10:17).EQ.'L       '.OR.
     2                   SCRATH(J)(1:8).EQ.'U       '.AND.
     1                   SCRATH(J)(10:17).EQ.'LENS    ') ULON=.TRUE.
                      IF(SCRATH(J)(1:8).EQ.'U       '.AND.
     1                   SCRATH(J)(10:17).EQ.'SP      '.OR.
     2                   SCRATH(J)(1:8).EQ.'UPDATE  '.AND.
     1                   SCRATH(J)(10:17).EQ.'SPSRF   '.OR.
     1                   SCRATH(J)(1:8).EQ.'UPDATE  '.AND.
     1                   SCRATH(J)(10:17).EQ.'SP      '.OR.
     2                   SCRATH(J)(1:8).EQ.'U       '.AND.
     1                   SCRATH(J)(10:17).EQ.'SPSRF   ') USON=.TRUE.
                      IF(SCRATH(J)(1:8).EQ.'EOS     '.AND.ULON) ULON=.FALSE.
                      IF(SCRATH(J)(1:8).EQ.'EOS     '.AND.USON) USON=.FALSE.
C
C     NO WE ONLY NEED TO DO MORE BEFORE GOING TO THE NEXT LINE IN
C     SCRATH, IF ULON OR USON ARE .TRUE.
                      IF(USON.OR.ULON) THEN
C     CONTINUE TO PROCESS
C     FIRST USON, SINCE THAT SEEMS EASIER TO DO
                          IF(USON) THEN
C     CHECK FOR C1 THROUGH C96 AND PROCESS APPROPRIATELY
                              IF(SCRATH(J)(1:8).EQ.'C1') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=27
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C2') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=28
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C3') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=32
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C4') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=30
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C5') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=34
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C6') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=32
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C7') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=41
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C8') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=34
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C9') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=35
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C10') THEN
                                  K=K+1
                                  AUXMAX=K
                                  AN1=SCRATH(J)(10:34)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C11') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=37
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C12') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=41
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C13') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=39
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C14') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=56
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C15') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=41
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C16') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=42
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C17') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=43
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C18') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=44
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C19') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=45
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C20') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=46
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C21') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=47
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C22') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=48
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C23') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=65
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C24') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=56
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C25') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=51
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C26') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=52
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C27') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=53
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C28') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=54
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C29') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=55
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C30') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=56
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C31') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=57
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C32') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=58
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C33') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=65
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C34') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=60
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C35') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=61
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C36') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=62
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C37') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=63
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C38') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=64
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C39') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=65
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C40') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=66
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C41') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=67
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C42') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=68
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C43') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=69
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C44') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=70
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C45') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=71
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C46') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=72
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C47') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=73
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C48') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=74
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C49') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=76
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C50') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=77
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C51') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=78
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C52') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=79
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C53') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=80
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C54') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=81
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C55') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=82
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C56') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=83
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C57') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=84
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C58') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=85
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C59') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=86
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C60') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=87
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C61') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=88
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C62') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=89
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C63') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=90
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C64') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=91
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C65') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=92
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C66') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=93
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C67') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=94
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C68') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=95
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C69') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=96
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C70') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=97
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C71') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=98
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C72') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=99
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C73') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=100
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C74') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=101
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C75') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=102
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C76') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=103
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C77') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=104
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C78') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=105
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C79') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=106
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C80') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=107
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C81') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=108
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C82') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=109
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C83') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=110
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C84') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=111
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C85') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=112
                                  AN1=SCRATH(J)(10:34)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C86') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=113
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C87') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=114
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C88') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=115
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C89') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=116
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C90') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=117
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C91') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=118
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C92') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=119
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C93') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=120
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C94') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=121
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C95') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=122
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'C96') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=123
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFADD(K,2)=INT(N1)
                                  CFADD(K,3)=I
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                              ELSE
C     PROCEED
                              END IF
                          ELSE
C     USON FALSE
                          END IF
C     NOW CHECK FOR CASES OF ULON TRUE
                          IF(ULON) THEN
C               SET THE CURRENT SURFACE NUMBER IF NECESSARY
                              IF(SCRATH(J)(1:8).EQ.'CHG     ') THEN
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  SURFNM=N1
                              ELSE
C     PROCEED, SURFNM ALREADY SET
                              END IF
C     THE REGULAR LENS FILE VARIABLES
                              IF(SCRATH(J)(1:8).EQ.'RD      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=1
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                  END IF
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'INDEX   '.OR.
     1                        SCRATH(J)(1:8).EQ.'VNUM    '.OR.SCRATH(J)(1:8).EQ.'DPART   ')THEN
                                  K=K+1
                                  AUXMAX=K
                                  IF(SCRATH(J)(1:8).EQ.'INDEX   ') CFADD(K,1)=135
                                  IF(SCRATH(J)(1:8).EQ.'VNUM    ') CFADD(K,1)=136
                                  IF(SCRATH(J)(1:8).EQ.'DPART   ') CFADD(K,1)=140
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                  END IF
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(34:56)
                                      CALL AUXATN
                                      CFVAL(K,2)=N1
                                      CFCHAR(K,2)=SCRATH(J)(34:56)
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(43:65)
                                      CALL AUXATN
                                      CFVAL(K,2)=N1
                                      CFCHAR(K,2)=SCRATH(J)(43:65)
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'CV      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=2
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                  END IF
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'TH      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=3
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'GRS     ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=149
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  CFCHAR(K,2)=' 0.000000000000000D+000'
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'CLAP    '.AND.
     1                        SCRATH(J)(10:17).NE.'RECT     '.AND.SCRATH(J)(10:17).NE.'RCTK    '
     1                        .AND.
     1                        SCRATH(J)(10:17).NE.'ERASE    '.AND.SCRATH(J)(10:17).NE.'ELIPE   '
     1                        .AND.
     1                        SCRATH(J)(10:17).NE.'RECTE    '.AND.SCRATH(J)(10:17).NE.'RCTKE   '
     1                        ) THEN
C     WE HAVE A CIRCULAR CLAP OR POLY CLAP
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=142
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(10:32)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFCHAR(K,1)=SCRATH(J)(10:32)
                                  CFADD(K,4)=10
                                  CFADD(K,5)=32
                                  AN1=SCRATH(J)(34:56)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,2)=SCRATH(J)(34:56)
                                  CFADD(K,6)=34
                                  CFADD(K,7)=56
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'CLAP    '.AND.
     1                        SCRATH(J)(10:17).EQ.'RECT     '.OR.
     1                        SCRATH(J)(1:8).EQ.'CLAP    '.AND.
     1                        SCRATH(J)(10:17).EQ.'ELIP     '.OR.
     1                        SCRATH(J)(1:8).EQ.'CLAP    '.AND.
     1                        SCRATH(J)(10:17).EQ.'RCTK     ') THEN
C     WE HAVE A NON CIRCULAR CLAP
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=141
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  AN1=SCRATH(J)(19:41)
                                  CALL AUXATN
                                  CFVAL(K,1)=N1
                                  CFCHAR(K,1)=SCRATH(J)(19:41)
                                  CFADD(K,4)=19
                                  CFADD(K,5)=41
                                  AN1=SCRATH(J)(43:65)
                                  CALL AUXATN
                                  CFVAL(K,2)=N1
                                  CFCHAR(K,2)=SCRATH(J)(43:65)
                                  CFADD(K,6)=43
                                  CFADD(K,7)=65
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'CC      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=4
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AD      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=5
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AE      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=6
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AF      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=7
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AG      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=8
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AH      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=129
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AI      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=130
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AJ      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=131
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AK      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=132
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AL      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=133
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'RDTOR   ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=9
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'CVTOR') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=10
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'CCTOR   ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=11
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'ADTOR   ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=12
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AETOR   ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=13
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AFTOR   ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=14
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'AGTOR   ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=15
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'ALPHA   ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=16
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'BETA    ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=17
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'GAMMA   ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=18
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'GALPHA  ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=146
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'GBETA   ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=147
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'GGAMMA  ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=148
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'XD      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=19
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'ZD      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=134
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'GDX     ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=143
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'GDY     ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=144
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'GDZ     ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=145
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'PIVX    ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=137
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'PIVY    ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=138
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'PIVZ    ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=139
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'YD      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=20
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'N1      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=21
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'N2      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=22
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'N3      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=23
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'N4      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=32
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'N5      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=25
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'N6      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=124
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'N7      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=125
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'N8      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=126
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'N9      ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=127
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
                              IF(SCRATH(J)(1:8).EQ.'N10     ') THEN
                                  K=K+1
                                  AUXMAX=K
                                  CFADD(K,1)=128
                                  CFADD(K,2)=INT(SURFNM)
                                  CFADD(K,3)=I
                                  CFADD(K,8)=0
                                  CFADD(K,9)=J
                                  IF(SCRATH(J)(10:17).NE.'DELT    '.AND.SCRATH(J)(10:17)
     1                            .NE.'CENT    ') THEN
                                      AN1=SCRATH(J)(10:32)
                                      CALL AUXATN
                                      CFVAL(K,1)=N1
                                      CFCHAR(K,1)=SCRATH(J)(10:32)
                                      CFADD(K,4)=10
                                      CFADD(K,5)=32
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=34
                                      CFADD(K,7)=56
                                  ELSE
                                      AN1=SCRATH(J)(19:41)
                                      CALL AUXATN
                                      CFCHAR(K,1)=SCRATH(J)(19:41)
                                      CFADD(K,4)=19
                                      CFADD(K,5)=41
                                      CFCHAR(K,2)=' 0.000000000000000D+000'
                                      CFADD(K,6)=43
                                      CFADD(K,7)=65
                                  END IF
                                  CFVAL(K,1)=N1
                                  CFVAL(K,2)=0.0D0
                              ELSE
C     PROCEED
                              END IF
C     PROCEED
                          ELSE
C     ULON FALSE
                          END IF
C                       ELSE
C     BOTH ULON AND USON FALSE
                      END IF
                  END DO
C     WE JUST LEFT CONFIG I, LOOP BACK FOR THE NEXT CONFIG
C
              END DO
C     NEXT LINES WERE USED DURING DEBUGGING OF VARIABLES SUBFILE
C     ARRAYS LOADED, ALL DONE
              DEALLOCATE(SCRATH,STAT=ALLOERR)
              RETURN
          ELSE
          END IF
          DEALLOCATE(SCRATH,STAT=ALLOERR)
          RETURN
      END
C SUB BLKHEAD.FOR
      SUBROUTINE BLKHEAD
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE BLKHEAD WHICH IMPLEMENTS THE
C       FOOTBLOK DEFINITION
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
C               OR NUMERIC INPUT.
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"FOOTBLOK" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"FOOTBLOK" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.NE.'YES'.AND.WQ.NE.'NO'.AND.WQ.NE.'ON'.AND.WQ.NE.'OFF')THEN
              OUTLYNE='"FOOTBLOK" REQUIRES "ON","OFF","YES" OR "NO"'
              CALL SHOWIT(1)
              OUTLYNE='AS VALID QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WQ.EQ.'YES'.OR.WQ.EQ.'ON') ALENS(58,SURF)=1.0D0
          IF(WQ.EQ.'NO'.OR.WQ.EQ.'OFF') ALENS(58,SURF)=0.0D0
          RETURN
      END
C SUB CVFLIP.FOR
      SUBROUTINE CVFLIP
C
          IMPLICIT NONE
C
C     THIS DOES THE CMD LEVEL COMMAND "FLIP"
C
C       FORM OF THE "FLIP" COMMAND IS:
C
C       FLIP, I J
C
          INTEGER I,J,ALLOERR,K,BCNT,JJ
          REAL*8 TEMPLEN
          CHARACTER TEMPGLA*13
          DIMENSION TEMPLEN(:,:),TEMPGLA(:,:)
          ALLOCATABLE :: TEMPLEN,TEMPGLA
C
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'"FLIP" TAKES THE FOLLOWING INPUT:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 IS STARTING SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #2 IS ENDING SURFACE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"FLIP" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"FLIP" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"FLIP" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.GT.W2) THEN
              WRITE(OUTLYNE,*)'FOR THE "FLIP" COMMAND THE STARTING SURFACE MUST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'COME BEFORE ENDING SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.0.0D0) THEN
              WRITE(OUTLYNE,*)'STARTING SURFACE MUST COME AFTER SURFACE ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          I=INT(W1)
          J=INT(W2)
C
C     DO A FORCED RETURN TO CFG 1
          CALL FRCCF1(1)
C
C     LOAD THE LENS INTO THE TEMPLEN
          DEALLOCATE(TEMPLEN,
     1    TEMPGLA,STAT=ALLOERR)
          ALLOCATE(TEMPLEN(1:LSIZ,0:MAXSUR),
     1    TEMPGLA(0:MAXSUR,1:2),STAT=ALLOERR)
          TEMPLEN(1:LSIZ,0:MAXSUR)=ALENS(1:LSIZ,0:MAXSUR)
          TEMPGLA(0:MAXSUR,1:2)=GLANAM(0:MAXSUR,1:2)
C     STEPS
C
C     1. IF THERE IS ALTERNATE CONFIG STUFF, PRINT WARNING MESSAGE
C     THAT "FLIP" ONLY AFFECTS THE MAIN CFG #1
          IF(SYSTEM1(56).GT.1.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'WARNING:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'THE CURRENT LENS HAS ALTERNATE CONFIGURATIONS DEFINED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'ONLY THE MAIN CONFIGURATION DATA WILL BE FLIPPED'
              CALL SHOWIT(1)
          END IF
C     2. IF THERE ARE TILTS OR DECENTERS IN THE I TO J RANGE
C     PRINT MESSAGE THAT THEY WILL BE DELETED
          DO K=I,J
              IF(ALENS(25,K).NE.0.0D0.OR.ALENS(29,K).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'THERE ARE TILTS AND/OR DECENTERS IN THE SURFACE NUMBER RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'ALL TILTS AND/OR DECENTERS IN THE SURFACE RANGE WILL BE DELETED'
                  CALL SHOWIT(1)
                  GO TO 20
              END IF
          END DO
 20       CONTINUE
          DO K=I,J
              IF(ALENS(90,K).NE.0.0D0.OR.ALENS(91,K).NE.0.0D0.OR.
     1        ALENS(92,K).NE.0.0D0.OR.ALENS(93,K).NE.0.0D0.OR.
     1        ALENS(94,K).NE.0.0D0.OR.ALENS(95,K).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'THERE ARE GLOBAL TILTS AND/OR DECENTERS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'IN THE SURFACE NUMBER RANGE WHICH WILL BE DELETED'
                  CALL SHOWIT(1)
                  GO TO 21
              END IF
          END DO
 21       CONTINUE
          DO K=I,J
              IF(ALENS(32,K).NE.0.0D0.OR.ALENS(33,K).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'THERE ARE PIKUPS OR TILTS IN THE SURFACE NUMBER RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'WHICH WILL BE DELETED'
                  CALL SHOWIT(1)
                  GO TO 22
              END IF
          END DO
 22       CONTINUE
C
          DO K=I,J
              IF(ALENS(96,K).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'AFTER "FLIP", CHECK DIFFRACTION GRATING DIRECTION NUMBERS'
                  CALL SHOWIT(1)
                  GO TO 23
              END IF
          END DO
 23       CONTINUE
          DO K=I,J
              IF(ALENS(34,K).NE.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'NO SPECIAL SURFACE DATA IS FLIPED BY "FLIP"'
                  CALL SHOWIT(1)
                  GO TO 24
              END IF
          END DO
 24       CONTINUE
C
C     3. APPLY FLIP SURFACES IGNORING ALL TILTS AND DECENTERS
C     SOME THINGS JUST FLIP EXACTLY
          DO K=I,J
              BCNT=I+(J-K)
              ALENS(1:2,K)=TEMPLEN(1:2,BCNT)
              ALENS(4:45,K)=TEMPLEN(4:45,BCNT)
              ALENS(51:70,K)=TEMPLEN(51:70,BCNT)
              ALENS(75:105,K)=TEMPLEN(75:105,BCNT)
              ALENS(25:31,K)=0.0D0
              ALENS(33:34,K)=0.0D0
              ALENS(90:95,K)=0.0D0
              ALENS(59,K)=0.0D0
              ALENS(116,K)=0.0D0
              ALENS(70,K)=0.0D0
              ALENS(77:80,K)=0.0D0
C     FLIP CURVATURE SIGNS
              ALENS(1,K)=-ALENS(1,K)
              ALENS(4:7,K)=-ALENS(4:7,K)
              ALENS(24,K)=-ALENS(24,K)
              ALENS(37:43,K)=-ALENS(37:43,K)
              ALENS(38:43,K)=-ALENS(38:43,K)
              ALENS(81:85,K)=-ALENS(81:85,K)
          END DO
          JJ=J-1
          DO K=I,JJ
              BCNT=I+(JJ-K)
              ALENS(3,K)=TEMPLEN(3,BCNT)
              ALENS(46:50,K)=1.0D0
              ALENS(71:75,K)=1.0D0
              GLANAM(K,1:2)=TEMPGLA(BCNT,1:2)
          END DO
          F1=0
          F6=1
          F22=1
          LNSTYP=1
          CALL LNSEOS
C

C     4. PRINT MESSGE THAT SURFACES HAVE BEEN FLIPPED
          WRITE(OUTLYNE,*)
     1    'THE REQUESTED "FLIP" HAS BEEN PERFORMED'
          CALL SHOWIT(1)
          DEALLOCATE(TEMPLEN,TEMPGLA,STAT=ALLOERR)
          RETURN
      END
C SUB HEXROLL.FOR
      SUBROUTINE HEXROLL
C
          IMPLICIT NONE
C
C     THIS DOES THE HEXAGON-LIKE CMD LEVEL COMMAND "ROLL"
C
C       FORM OF THE "ROLL" COMMAND IS:
C
C       ROLL, I J DELTAX DELTAY K
C
          INTEGER I,J,SURFN,K,L
C
          COMMON/REMSURF/SURFN
C
          REAL*8 D11,D12,D13,D21,D22,D23,D31,D32,D33,DX1,DY1
     1    ,DZ1,ALPHA,BETA,GAMMA,GALPHA,GBETA,GGAMMA,GDECX,GDECY,GDECZ,
     2    COSB,SINB,DXER,DYER,NEWALPHA,NEWBETA,TH
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)'"ROLL" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'"ROLL" TAKES THE FOLLOWING INPUT:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 IS STARTING SURFACE NUMBER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #2 IS ENDING SURFACE NUMBER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #3 IS X-DISPLACEMENT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #4 IS Y-DISPLACEMENT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #5 IS OPTIONAL SURFACE # FOR ROLL'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(DF1.EQ.1.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"ROLL" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"ROLL" REQUIRES EXPLICIT NUMERIC WORD #2'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.0.0D0.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'STARTING SURFACE MUST COME AFTER SURFACE ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DABS(ALENS(3,INT(W1-1.0D0))).GE.1.0D10) THEN
              WRITE(OUTLYNE,*)'SURFACE I-1 MUST HAVE A FINITE THICKNESS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF((W2).GE.SYSTEM1(20)) THEN
              WRITE(OUTLYNE,*)
     1        'ENDING SURFACE MUST COME BEFORE THE FINAL SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.GT.W2) THEN
              WRITE(OUTLYNE,*)
     1        'STARTING SURFACE MUST COME BEFORE ENDING SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF5.EQ.1) THEN
              W5=W1
          END IF
          I=INT(W1)
          J=INT(W2)
          K=INT(W5)
          IF(K.NE.J) K=I
          IF(DF3.EQ.1) W3=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          DXER=W3
          DYER=W4
C
C     STEPS
C
C
          IF(I.EQ.K) THEN
C     1. GET THE LOCATION OF SURFACE J+1 IN THE COORDINATE SYSTEM OF
C     SURFACE I-1. WRITE IT DOWN
C
              SAVE_KDP(1)=SAVEINPT(1)
              WC='GLOBAL'
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I-1)
              CALL GLOBAL
              REST_KDP(1)=RESTINPT(1)
C     NOW GET ALL THE GOODIES
              DX1=VERTEX(1,J+1)
              DY1=VERTEX(2,J+1)
              DZ1=VERTEX(3,J+1)
              D11=VERTEX(4,J+1)
              D12=VERTEX(5,J+1)
              D13=VERTEX(6,J+1)
              D21=VERTEX(7,J+1)
              D22=VERTEX(8,J+1)
              D23=VERTEX(9,J+1)
              D31=VERTEX(10,J+1)
              D32=VERTEX(11,J+1)
              D33=VERTEX(12,J+1)
C     CALCULATE GLOBAL DECENTERS AND TILTS
              GDECX=DX1
              GDECY=DY1
              GDECZ=DZ1
C     CALCULATE GLOBAL ALPHA, BETA AND GAMMA
C     NOW CALCULATE ALPHA,BETA AND GAMMA AND ASSIGN THEM TO SURFACE I
C     CALCULATE BETA
              BETA=DASIN(-D31)
              COSB=DCOS(BETA)
              IF(COSB.NE.0.0D0) THEN
C     COSINE OF BETA IS NOT ZERO
                  IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).NE.0.0D0) ALPHA=0.0D0
                  IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=0.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=PII/2.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).NE.0.0D0)
     1            ALPHA=DATAN2((D32/COSB),(D33/COSB))
                  IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).NE.0.0D0) GAMMA=0.0D0
                  IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=0.0D0
                  IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=PII/2.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D11/COSB).NE.0.0D0)
     1            GAMMA=DATAN2((-D21/COSB),(D11/COSB))
                  ALPHA=(180.0D0/PII)*ALPHA
                  BETA=(180.0D0/PII)*BETA
                  GAMMA=(180.0D0/PII)*GAMMA
              END IF
              IF(COSB.EQ.0.0D0) THEN
C     COSINE OF BETA IS ZERO
                  IF(D31.EQ.-1.0D0) SINB=1
                  IF(D31.EQ.1.0D0) SINB=-1
                  IF(SINB.EQ.1) BETA=90.0D0
                  IF(SINB.EQ.-1) BETA=-90.0D0
                  GAMMA=0.0D0
                  IF(SINB.EQ.1) THEN
                      IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=PII/2.0D0
                      IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=DATAN2((D12),(D13))
                  END IF
                  IF(SINB.EQ.-1) THEN
                      IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=PII/2.0D0
                      IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=DATAN2((-D12),(-D13))
                  END IF
                  ALPHA=(180.0D0/PII)*ALPHA
              END IF
              GALPHA=ALPHA
              GBETA=BETA
              GGAMMA=GAMMA
C
C     2. APPLY A TILT TO SURFACE I CORRESPONDING TO THE OFFSETS IF
C     THE SURFACE IS NOT FLAT. iF IT IS FLAT, APPLY A DECENTER
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I)
              CALL CONTRO
              IF(ALENS(1,I).EQ.0.0D0) THEN
C     FLAT, DO A DECENTER
                  WC='XD'
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  STI=0
                  W1=DXER+ALENS(114,I)
                  CALL CONTRO
                  WC='YD'
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  STI=0
                  W1=DYER+ALENS(115,I)
                  CALL CONTRO
              ELSE
C     CALCULATE ALPHA AND BETA TILTS AND APPLY THEM
                  NEWALPHA=-(180.0D0/PII)*DATAN2(DYER,DABS(1.0D0/ALENS(1,I)))
                  NEWBETA= (180.0D0/PII)*DATAN2(DXER,DABS(1.0D0/ALENS(1,I)))
                  IF(NEWALPHA.GT.180.0D0) NEWALPHA=NEWALPHA-360.0D0
                  IF(NEWBETA.GT.180.0D0) NEWBETA=NEWBETA-360.0D0
                  IF(ALENS(1,I).LT.0.0D0) THEN
                      NEWALPHA=-NEWALPHA
                      NEWBETA =-NEWBETA
                  END IF
                  WC='ALPHA'
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  STI=0
                  W1=NEWALPHA+ALENS(118,I)
                  CALL CONTRO
                  WC='BETA'
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  STI=0
                  W1=NEWBETA+ALENS(119,I)
                  CALL CONTRO
              END IF
C     DO A PIVOT AT THE CENTER OF CURVATURE ASSUMING NO INITIAL TILTS
              IF(ALENS(1,I).NE.0.0D0) THEN
                  WC='PIVOT'
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  STI=0
                  W1=0.0D0
                  W2=0.0D0
                  W3=1.0D0/ALENS(1,I)
                  CALL CONTRO
              END IF
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
C
C     3. APPLY A RETURN ON SURFACE J+1 TO RETURN TO I-1
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(J+1)
              CALL CONTRO
              WC='TILT'
              WQ='RET'
              SQ=1
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I-1)
              CALL CONTRO
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
C
C     4. ASSIGN GLOBAL DECENTERS AND TILTS TO SURFACE J+1 SO THAT
C     IT RETURNS TO ITS ORIGINAL POSITION WITH RESPECT TO I-1
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(J+1)
              CALL CONTRO
              WC='GDX'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECX
              CALL CONTRO
              WC='GDY'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECY
              CALL CONTRO
              WC='GDZ'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECZ
              CALL CONTRO
              WC='GALPHA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GALPHA
              CALL CONTRO
              WC='GBETA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GBETA
              CALL CONTRO
              WC='GGAMMA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GGAMMA
              CALL CONTRO
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          ELSE
C     J=K
C     1. GET THE LOCATION OF SURFACE J+1 IN THE COORDINATE SYSTEM OF
C     SURFACE I-1. WRITE IT DOWN
C
              SAVE_KDP(1)=SAVEINPT(1)
              WC='GLOBAL'
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I-1)
              CALL GLOBAL
              REST_KDP(1)=RESTINPT(1)
C     NOW GET ALL THE GOODIES
              DX1=VERTEX(1,J+1)
              DY1=VERTEX(2,J+1)
              DZ1=VERTEX(3,J+1)
              D11=VERTEX(4,J+1)
              D12=VERTEX(5,J+1)
              D13=VERTEX(6,J+1)
              D21=VERTEX(7,J+1)
              D22=VERTEX(8,J+1)
              D23=VERTEX(9,J+1)
              D31=VERTEX(10,J+1)
              D32=VERTEX(11,J+1)
              D33=VERTEX(12,J+1)
C     CALCULATE GLOBAL DECENTERS AND TILTS
              GDECX=DX1
              GDECY=DY1
              GDECZ=DZ1
C     CALCULATE GLOBAL ALPHA, BETA AND GAMMA
C     NOW CALCULATE ALPHA,BETA AND GAMMA AND ASSIGN THEM TO SURFACE I
C     CALCULATE BETA
              BETA=DASIN(-D31)
              COSB=DCOS(BETA)
              IF(COSB.NE.0.0D0) THEN
C     COSINE OF BETA IS NOT ZERO
                  IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).NE.0.0D0) ALPHA=0.0D0
                  IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=0.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=PII/2.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).NE.0.0D0)
     1            ALPHA=DATAN2((D32/COSB),(D33/COSB))
                  IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).NE.0.0D0) GAMMA=0.0D0
                  IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=0.0D0
                  IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=PII/2.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D11/COSB).NE.0.0D0)
     1            GAMMA=DATAN2((-D21/COSB),(D11/COSB))
                  ALPHA=(180.0D0/PII)*ALPHA
                  BETA=(180.0D0/PII)*BETA
                  GAMMA=(180.0D0/PII)*GAMMA
              END IF
              IF(COSB.EQ.0.0D0) THEN
C     COSINE OF BETA IS ZERO
                  IF(D31.EQ.-1.0D0) SINB=1
                  IF(D31.EQ.1.0D0) SINB=-1
                  IF(SINB.EQ.1) BETA=90.0D0
                  IF(SINB.EQ.-1) BETA=-90.0D0
                  GAMMA=0.0D0
                  IF(SINB.EQ.1) THEN
                      IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=PII/2.0D0
                      IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=DATAN2((D12),(D13))
                  END IF
                  IF(SINB.EQ.-1) THEN
                      IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=PII/2.0D0
                      IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=DATAN2((-D12),(-D13))
                  END IF
                  ALPHA=(180.0D0/PII)*ALPHA
              END IF
              GALPHA=ALPHA
              GBETA=BETA
              GGAMMA=GAMMA
C
C     2. APPLY A TILT TO SURFACE I CORRESPONDING TO THE OFFSETS IF
C     THE SURFACE IS NOT FLAT. iF IT IS FLAT, APPLY A DECENTER
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I)
              CALL CONTRO
              IF(ALENS(1,K).EQ.0.0D0) THEN
C     FLAT, DO A DECENTER
                  WC='XD'
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  STI=0
                  W1=DXER+ALENS(114,I)
                  CALL CONTRO
                  WC='YD'
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  STI=0
                  W1=DYER+ALENS(115,I)
                  CALL CONTRO
              ELSE
C     CALCULATE ALPHA AND BETA TILTS AND APPLY THEM
                  NEWALPHA=(180.0D0/PII)*DATAN2(DYER,DABS(1.0D0/ALENS(1,K)))
                  NEWBETA= -(180.0D0/PII)*DATAN2(DXER,DABS(1.0D0/ALENS(1,K)))
                  IF(NEWALPHA.GT.180.0D0) NEWALPHA=NEWALPHA-360.0D0
                  IF(NEWBETA.GT.180.0D0) NEWBETA=NEWBETA-360.0D0
                  IF(ALENS(1,K).GT.0.0D0) THEN
                      NEWALPHA=-NEWALPHA
                      NEWBETA =-NEWBETA
                  END IF
                  WC='ALPHA'
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  STI=0
                  W1=NEWALPHA+ALENS(118,I)
                  CALL CONTRO
                  WC='BETA'
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  STI=0
                  W1=NEWBETA+ALENS(119,I)
                  CALL CONTRO
              END IF
C     DO A PIVOT AT THE CENTER OF CURVATURE ASSUMING NO INITIAL TILTS
              IF(ALENS(1,K).NE.0.0D0) THEN
C     ASSUME NO TILTS OR DECENTERS BETWEEN I AND J
                  TH=0.0D0
                  DO L=I,J-1
                      TH=TH+ALENS(3,L)
                  END DO
                  WC='PIVOT'
                  WQ='        '
                  SQ=0
                  S1=1
                  S2=1
                  S3=1
                  S4=0
                  S5=0
                  DF1=0
                  DF2=0
                  DF3=0
                  DF4=1
                  DF5=1
                  SN=1
                  SST=0
                  STI=0
                  W1=0.0D0
                  W2=0.0D0
                  W3=TH+(1.0D0/ALENS(1,K))
                  CALL CONTRO
              END IF
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
C
C     3. APPLY A RETURN ON SURFACE J+1 TO RETURN TO I-1
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(J+1)
              CALL CONTRO
              WC='TILT'
              WQ='RET'
              SQ=1
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I-1)
              CALL CONTRO
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
C
C     4. ASSIGN GLOBAL DECENTERS AND TILTS TO SURFACE J+1 SO THAT
C     IT RETURNS TO ITS ORIGINAL POSITION WITH RESPECT TO I-1
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(J+1)
              CALL CONTRO
              WC='GDX'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECX
              CALL CONTRO
              WC='GDY'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECY
              CALL CONTRO
              WC='GDZ'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECZ
              CALL CONTRO
              WC='GALPHA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GALPHA
              CALL CONTRO
              WC='GBETA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GBETA
              CALL CONTRO
              WC='GGAMMA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GGAMMA
              CALL CONTRO
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
C     NOW DELETE ALL RET FROM J+1
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='UPDATE LENS'
          CALL PROCES
          WC='CHG'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=DBLE(J+1)
          CALL CONTRO
          WC='TILT'
          WQ='RETD'
          SQ=1
          S1=0
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=1
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=0
          SST=0
          STI=0
          CALL CONTRO
C     NOW DELETE ALL PIVOT FROM I
          WC='CHG'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=DBLE(I)
          CALL CONTRO
          WC='PIVOTD'
          WQ='        '
          SQ=0
          S1=0
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=1
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=0
          SST=0
          STI=0
          CALL CONTRO
          INPUT='EOS'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)

          RETURN
      END
      SUBROUTINE HEXSTILT
C
          IMPLICIT NONE
C
C     THIS DOES THE HEXAGON-LIKE CMD LEVEL COMMAND "STILT"
C
C       FORM OF THE "STILT" COMMAND IS:
C
C       STILT, I ALPHA BETA GAMMA
CC     OR
C        STILT PIVOT X Y Z
C      OR
C        STILT PIVOT AUTO
C
          INTEGER I,SURFN
C
          COMMON/REMSURF/SURFN
C
          REAL*8 D11,D12,D13,D21,D22,D23,D31,D32,D33,DX1,DY1
     1    ,DZ1,ALPHA,BETA,GAMMA,GALPHA,GBETA,GGAMMA,GDECX,GDECY,GDECZ,
     2    COSB,SINB,DALPHA,DBETA,DGAMMA
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     STILT PIVOT
          IF(SQ.EQ.1.AND.WQ.NE.'PIVOT'.AND.WQ.NE.'PIVAUTO') THEN
              WRITE(OUTLYNE,*)
     1        '"STILT" ONLY TAKES "PIVOT" AND "PIVAUTO" AS AN OPTIONAL'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'"QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
          END IF
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"STILT" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"STILT" TAKES THE FOLLOWING INPUT:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 IS SURFACE NUMBER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #2 IS ALPHA TILT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #3 IS BETA TILT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #4 IS GAMMA TILT'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(STI.EQ.1.AND.WQ.EQ.'PIVOT') THEN
              WRITE(OUTLYNE,*)'"STILT PIVOT" TAKES THE FOLLOWING INPUT:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 IS X-PIVOT LOCATION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #2 IS Y-PIVOT LOCATION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #3 IS Z-PIVOT LOCATION'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(STI.EQ.1.AND.WQ.EQ.'PIVAUTO') THEN
              WRITE(OUTLYNE,*)
     1        '"STILT PIVAUTO" TAKES THE FOLLOWING OPTIONAL INPUT:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 IS X-PIVOT OFFSET'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #2 IS Y-PIVOT OFFSET'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #3 IS Z-PIVOT OFFSET'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(DF1.EQ.1.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"STILT" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.0.0D0.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'SURFACE MUST COME AFTER SURFACE ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              IF(DABS(ALENS(3,INT(W1-1.0D0))).GE.1.0D10) THEN
                  WRITE(OUTLYNE,*)'SURFACE I-1 MUST HAVE A FINITE THICKNESS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF((W1).GE.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)
     1            'SURFACE MUST COME BEFORE THE FINAL SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF5.EQ.0.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"STILT" TAKES NO NUMERIC WORD #5'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.DF4.EQ.0.OR.SQ.EQ.1.AND.DF5.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"STILT PIVOT and PIVAUTO" TAKE NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
C     JUST STILT
              I=INT(W1)
              IF(DF2.EQ.1) W2=0.0D0
              IF(DF3.EQ.1) W3=0.0D0
              IF(DF4.EQ.1) W4=0.0D0
              DALPHA=W2
              DBETA =W3
              DGAMMA=W4
C
C     STEPS
C
C
C     1. GET THE LOCATION OF SURFACE I+1 IN THE COORDINATE SYSTEM OF
C     SURFACE I-1. WRITE IT DOWN
C
              SAVE_KDP(1)=SAVEINPT(1)
              WC='GLOBAL'
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I-1)
              CALL GLOBAL
              REST_KDP(1)=RESTINPT(1)
C     NOW GET ALL THE GOODIES
              DX1=VERTEX(1,I+1)
              DY1=VERTEX(2,I+1)
              DZ1=VERTEX(3,I+1)
              D11=VERTEX(4,I+1)
              D12=VERTEX(5,I+1)
              D13=VERTEX(6,I+1)
              D21=VERTEX(7,I+1)
              D22=VERTEX(8,I+1)
              D23=VERTEX(9,I+1)
              D31=VERTEX(10,I+1)
              D32=VERTEX(11,I+1)
              D33=VERTEX(12,I+1)
C     CALCULATE GLOBAL DECENTERS AND TILTS
              GDECX=DX1
              GDECY=DY1
              GDECZ=DZ1
C     CALCULATE GLOBAL ALPHA, BETA AND GAMMA
C     NOW CALCULATE ALPHA,BETA AND GAMMA AND ASSIGN THEM TO SURFACE I
C     CALCULATE BETA
              BETA=DASIN(-D31)
              COSB=DCOS(BETA)
              IF(COSB.NE.0.0D0) THEN
C     COSINE OF BETA IS NOT ZERO
                  IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).NE.0.0D0) ALPHA=0.0D0
                  IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=0.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=PII/2.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).NE.0.0D0)
     1            ALPHA=DATAN2((D32/COSB),(D33/COSB))
                  IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).NE.0.0D0) GAMMA=0.0D0
                  IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=0.0D0
                  IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=PII/2.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D11/COSB).NE.0.0D0)
     1            GAMMA=DATAN2((-D21/COSB),(D11/COSB))
                  ALPHA=(180.0D0/PII)*ALPHA
                  BETA=(180.0D0/PII)*BETA
                  GAMMA=(180.0D0/PII)*GAMMA
              END IF
              IF(COSB.EQ.0.0D0) THEN
C     COSINE OF BETA IS ZERO
                  IF(D31.EQ.-1.0D0) SINB=1
                  IF(D31.EQ.1.0D0) SINB=-1
                  IF(SINB.EQ.1) BETA=90.0D0
                  IF(SINB.EQ.-1) BETA=-90.0D0
                  GAMMA=0.0D0
                  IF(SINB.EQ.1) THEN
                      IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=PII/2.0D0
                      IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=DATAN2((D12),(D13))
                  END IF
                  IF(SINB.EQ.-1) THEN
                      IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=PII/2.0D0
                      IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=DATAN2((-D12),(-D13))
                  END IF
                  ALPHA=(180.0D0/PII)*ALPHA
              END IF
              GALPHA=ALPHA
              GBETA=BETA
              GGAMMA=GAMMA
C
C     2. APPLY A TILT TO SURFACE I EQUAL TO THE STILT VALUES
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I)
              SURFN=I
              CALL CONTRO
              WC='ALPHA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DALPHA+ALENS(118,I)
              CALL CONTRO
              WC='BETA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBETA+ALENS(119,I)
              CALL CONTRO
              WC='GAMMA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DGAMMA+ALENS(120,I)
              CALL CONTRO
              IF(.NOT.STILTAUTO) THEN
                  IF(STILTXP.NE.0.0D0.OR.STILTYP.NE.0.0D0.OR.
     1            STILTZP.NE.0.0D0) THEN
                      WC='PIVOT'
                      WQ='        '
                      SQ=0
                      S1=1
                      S2=1
                      S3=1
                      S4=0
                      S5=0
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=1
                      DF5=1
                      SN=1
                      SST=0
                      STI=0
                      W1=STILTXP
                      W2=STILTYP
                      W3=STILTZP
                      CALL CONTRO
                  END IF
              ELSE
                  IF(STILTXP.NE.0.0D0.OR.STILTYP.NE.0.0D0.OR.
     1            STILTZP.NE.0.0D0.OR.REFRY(1,SURFN).NE.0.0D0.OR.
     2            REFRY(2,SURFN).NE.0.0D0.OR.REFRY(3,SURFN).NE.0.0D0) THEN
                      WC='PIVOT'
                      WQ='        '
                      SQ=0
                      S1=1
                      S2=1
                      S3=1
                      S4=0
                      S5=0
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=1
                      DF5=1
                      SN=1
                      SST=0
                      STI=0
                      W1=REFRY(1,SURFN)+STILTXP
                      W2=REFRY(2,SURFN)+STILTYP
                      W3=REFRY(3,SURFN)+STILTZP
                      CALL CONTRO
                  END IF
              END IF
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
C
C     3. APPLY A RETURN ON SURFACE I+1 TO RETURN TO I-1
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I+1)
              CALL CONTRO
              WC='TILT'
              WQ='RET'
              SQ=1
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I-1)
              CALL CONTRO
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
C
C     4. ASSIGN GLOBAL DECENTERS AND TILTS TO SURFACE I+1 SO THAT
C     IT RETURNS TO ITS ORIGINAL POSITION WITH RESPECT TO I-1
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I+1)
              CALL CONTRO
              WC='GDX'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECX
              CALL CONTRO
              WC='GDY'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECY
              CALL CONTRO
              WC='GDZ'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECZ
              CALL CONTRO
              WC='GALPHA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GALPHA
              CALL CONTRO
              WC='GBETA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GBETA
              CALL CONTRO
              WC='GGAMMA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GGAMMA
              CALL CONTRO
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              STILTXP=0.0D0
              STILTYP=0.0D0
              STILTZP=0.0D0
              STILTAUTO=.FALSE.
              REFEXT=.FALSE.
          ELSE
          END IF
          IF(WQ.EQ.'PIVOT') THEN
              IF(S1.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1) THEN
                  STILTXP=W1
                  STILTYP=W2
                  STILTZP=W3
              ELSE
                  STILTXP=0.0D0
                  STILTYP=0.0D0
                  STILTZP=0.0D0
              END IF
          ELSE
          END IF
          IF(WQ.EQ.'PIVAUTO') THEN
              IF(REFEXT) THEN
                  STILTXP=W1
                  STILTYP=W2
                  STILTZP=W3
                  STILTAUTO=.TRUE.
              ELSE
                  STILTXP=0.0D0
                  STILTYP=0.0D0
                  STILTZP=0.0D0
                  STILTAUTO=.FALSE.
                  WRITE(OUTLYNE,*)
     1            '"STILT PIVAUTO" REQUIRES REFERENCE RAY DATA TO EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TRACE A CHIEF RAY WITH THE "FOB" COMMAND'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'AND THEN ISSUE "STILT PIVAUTO" AGAIN'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
          IF(SQ.EQ.0) THEN
C     NOW DELETE ALL RET AND PIVOTS
C     NOW DELETE ALL RET FROM I+1
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I+1)
              CALL CONTRO
              WC='TILT'
              WQ='RETD'
              SQ=1
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=0
              SST=0
              STI=0
              CALL CONTRO
C     NOW DELETE ALL PIVOT FROM I
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I)
              CALL CONTRO
              WC='PIVOTD'
              WQ='        '
              SQ=0
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=0
              SST=0
              STI=0
              CALL CONTRO
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          RETURN
      END
C SUB HEXDISP.FOR
      SUBROUTINE HEXDISP
C
          IMPLICIT NONE
C
C       THIS DOES THE HEXAGON-LIKE CMD LEVEL COMMAND "DISP"
C
C       FORM OF THE "DISP" COMMAND IS:
C
C       DISP, I J XD YD ZD
C
          INTEGER I,J
          REAL*8 D11,D12,D13,D21,D22,D23,D31,D32,D33,DX1,DY1
     1    ,DZ1,ALPHA,BETA,GAMMA,GALPHA,GBETA,GGAMMA,GDECX,GDECY,GDECZ,
     2    COSB,SINB,XDEC,YDEC,ZDEC
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C
          IF(SQ.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"DISP" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'"DISP" TAKES THE FOLLOWING INPUT:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 IS STARTING SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #2 IS ENDING SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #3 IS X-DISPLACEMENT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #4 IS Y-DISPLACEMENT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #5 IS Z-DISPLACEMENT'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)'"DISP" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1) THEN
              WRITE(OUTLYNE,*)'"DISP" REQUIRES EXPLICIT NUMERIC WORD #2'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.GT.W2) THEN
              WRITE(OUTLYNE,*)'FOR THE "DISP" COMMAND THE STARTING SURFACE MUST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'COME BEFORE ENDING SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.0.0D0) THEN
              WRITE(OUTLYNE,*)'STARTING SURFACE MUST COME AFTER SURFACE ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DABS(ALENS(3,INT(W1-1.0D0))).GE.1.0D10) THEN
              WRITE(OUTLYNE,*)'SURFACE I-1 MUST HAVE A FINITE THICKNESS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.GE.SYSTEM1(20)) THEN
              WRITE(OUTLYNE,*)
     1        'ENDING SURFACE MUST COME BEFORE THE FINAL SURFACE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          I=INT(W1)
          J=INT(W2)
          IF(DF3.EQ.1) W3=0.0D0
          IF(DF4.EQ.1) W4=0.0D0
          IF(DF5.EQ.1) W5=0.0D0
          XDEC=W3
          YDEC=W4
          ZDEC=W5
C
C     STEPS
C
C
C     1. GET THE LOCATION OF SURFACE J+1 IN THE COORDINATE SYSTEM OF
C     SURFACE I-1. WRITE IT DOWN
C
          SAVE_KDP(1)=SAVEINPT(1)
          WC='GLOBAL'
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=DBLE(I-1)
          CALL GLOBAL
          REST_KDP(1)=RESTINPT(1)
C     NOW GET ALL THE GOODIES
          DX1=VERTEX(1,J+1)
          DY1=VERTEX(2,J+1)
          DZ1=VERTEX(3,J+1)
          D11=VERTEX(4,J+1)
          D12=VERTEX(5,J+1)
          D13=VERTEX(6,J+1)
          D21=VERTEX(7,J+1)
          D22=VERTEX(8,J+1)
          D23=VERTEX(9,J+1)
          D31=VERTEX(10,J+1)
          D32=VERTEX(11,J+1)
          D33=VERTEX(12,J+1)
C     CALCULATE GLOBAL DECENTERS AND TILTS
          GDECX=DX1
          GDECY=DY1
          GDECZ=DZ1
C     CALCULATE GLOBAL ALPHA, BETA AND GAMMA
C     NOW CALCULATE ALPHA,BETA AND GAMMA AND ASSIGN THEM TO SURFACE I
C     CALCULATE BETA
          BETA=DASIN(-D31)
          COSB=DCOS(BETA)
          IF(COSB.NE.0.0D0) THEN
C     COSINE OF BETA IS NOT ZERO
              IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).NE.0.0D0) ALPHA=0.0D0
              IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=0.0D0
              IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=PII/2.0D0
              IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).NE.0.0D0)
     1        ALPHA=DATAN2((D32/COSB),(D33/COSB))
              IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).NE.0.0D0) GAMMA=0.0D0
              IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=0.0D0
              IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=PII/2.0D0
              IF((D32/COSB).NE.0.0D0.AND.(D11/COSB).NE.0.0D0)
     1        GAMMA=DATAN2((-D21/COSB),(D11/COSB))
              ALPHA=(180.0D0/PII)*ALPHA
              BETA=(180.0D0/PII)*BETA
              GAMMA=(180.0D0/PII)*GAMMA
          END IF
          IF(COSB.EQ.0.0D0) THEN
C     COSINE OF BETA IS ZERO
              IF(D31.EQ.-1.0D0) SINB=1
              IF(D31.EQ.1.0D0) SINB=-1
              IF(SINB.EQ.1) BETA=90.0D0
              IF(SINB.EQ.-1) BETA=-90.0D0
              GAMMA=0.0D0
              IF(SINB.EQ.1) THEN
                  IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1            ALPHA=0.0D0
                  IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1            ALPHA=0.0D0
                  IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1            ALPHA=PII/2.0D0
                  IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1            ALPHA=DATAN2((D12),(D13))
              END IF
              IF(SINB.EQ.-1) THEN
                  IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1            ALPHA=0.0D0
                  IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1            ALPHA=0.0D0
                  IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1            ALPHA=PII/2.0D0
                  IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1            ALPHA=DATAN2((-D12),(-D13))
              END IF
              ALPHA=(180.0D0/PII)*ALPHA
          END IF
          GALPHA=ALPHA
          GBETA=BETA
          GGAMMA=GAMMA
C
C     2. APPLY A DEC TO SURFACE I EQUAL TO THE DISP VALUES
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='UPDATE LENS'
          CALL PROCES
          WC='CHG'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=DBLE(I)
          CALL CONTRO
          WC='XD'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=XDEC+ALENS(114,I)
          CALL CONTRO
          WC='YD'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=YDEC+ALENS(115,I)
          CALL CONTRO
          WC='ZD'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=ZDEC+ALENS(116,I)
          CALL CONTRO
          INPUT='EOS'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
C
C     3. APPLY A RETURN ON SURFACE J+1 TO RETURN TO I-1
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='UPDATE LENS'
          CALL PROCES
          WC='CHG'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=DBLE(J+1)
          CALL CONTRO
          WC='TILT'
          WQ='RET'
          SQ=1
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=DBLE(I-1)
          CALL CONTRO
          INPUT='EOS'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
C
C     4. ASSIGN GLOBAL DECENTERS AND TILTS TO SURFACE J+1 SO THAT
C     IT RETURNS TO ITS ORIGINAL POSITION WITH RESPECT TO I-1
          INPUT='UPDATE LENS'
          CALL PROCES
          WC='CHG'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=DBLE(J+1)
          CALL CONTRO
          WC='GDX'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=GDECX
          CALL CONTRO
          WC='GDY'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=GDECY
          CALL CONTRO
          WC='GDZ'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=GDECZ
          CALL CONTRO
          WC='GALPHA'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=GALPHA
          CALL CONTRO
          WC='GBETA'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=GBETA
          CALL CONTRO
          WC='GGAMMA'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=GGAMMA
          CALL CONTRO
          INPUT='EOS'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
C     NOW DELETE ALL RET FROM J+1
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='UPDATE LENS'
          CALL PROCES
          WC='CHG'
          WQ='        '
          SQ=0
          S1=1
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=0
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=1
          SST=0
          STI=0
          W1=DBLE(J+1)
          CALL CONTRO
          WC='TILT'
          WQ='RETD'
          SQ=1
          S1=0
          S2=0
          S3=0
          S4=0
          S5=0
          DF1=1
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          SN=0
          SST=0
          STI=0
          CALL CONTRO
          INPUT='EOS'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          RETURN
      END
C SUB HEXBTILT.FOR
      SUBROUTINE HEXBTILT
C
          IMPLICIT NONE
C
C     THIS DOES THE HEXAGON-LIKE CMD LEVEL COMMAND "BTILT"
C
C       FORM OF THE "BTILT" COMMAND IS:
C
C       BTILT, I J, ALPHA BETA GAMMA
CC     OR
C        BTILT PIVOT X Y Z
C      OR
C        BTILT PIVOT AUTO
C
          INTEGER I,J,SURFN
C
          COMMON/REMSURF/SURFN
C
          REAL*8 D11,D12,D13,D21,D22,D23,D31,D32,D33,DX1,DY1
     1    ,DZ1,ALPHA,BETA,GAMMA,GALPHA,GBETA,GGAMMA,GDECX,GDECY,GDECZ,
     2    COSB,SINB,DALPHA,DBETA,DGAMMA
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C     BTILT PIVOT
          IF(SQ.EQ.1.AND.WQ.NE.'PIVOT'.AND.WQ.NE.'PIVAUTO') THEN
              WRITE(OUTLYNE,*)
     1        '"BTILT" ONLY TAKES "PIVOT" AND "PIVAUTO" AS AN OPTIONAL'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'"QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
          END IF
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"BTILT" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"BTILT" TAKES THE FOLLOWING INPUT:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 IS STARTING SURFACE NUMBER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #2 IS ENDING SURFACE NUMBER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #3 IS ALPHA TILT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #4 IS BETA TILT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #5 IS GAMMA TILT'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(STI.EQ.1.AND.WQ.EQ.'PIVOT') THEN
              WRITE(OUTLYNE,*)'"BTILT PIVOT" TAKES THE FOLLOWING INPUT:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 IS X-PIVOT LOCATION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #2 IS Y-PIVOT LOCATION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #3 IS Z-PIVOT LOCATION'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(STI.EQ.1.AND.WQ.EQ.'PIVAUTO') THEN
              WRITE(OUTLYNE,*)
     1        '"BTILT PIVAUTO" TAKES THE FOLLOWING OPTIONAL INPUT:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 IS X-PIVOT OFFSET'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #2 IS Y-PIVOT OFFSET'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #3 IS Z-PIVOT OFFSET'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(DF1.EQ.1.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"BTILT" REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF2.EQ.1.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"BTILT" REQUIRES EXPLICIT NUMERIC WORD #2'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.0.0D0.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'STARTING SURFACE MUST COME AFTER SURFACE ZERO'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              IF(DABS(ALENS(3,INT(W1-1.0D0))).GE.1.0D10) THEN
                  WRITE(OUTLYNE,*)'SURFACE I-1 MUST HAVE A FINITE THICKNESS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF((W2).GE.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)
     1            'ENDING SURFACE MUST COME BEFORE THE FINAL SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.GT.W2) THEN
                  WRITE(OUTLYNE,*)
     1            'STARTING SURFACE MUST COME BEFORE ENDING SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.1.AND.DF4.EQ.0.OR.SQ.EQ.1.AND.DF5.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"BTILT PIVOT and PIVAUTO" TAKE NO NUMERIC WORD #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
C     JUST BTILT
              I=INT(W1)
              J=INT(W2)
              IF(DF3.EQ.1) W3=0.0D0
              IF(DF4.EQ.1) W4=0.0D0
              IF(DF5.EQ.1) W5=0.0D0
              DALPHA=W3
              DBETA =W4
              DGAMMA=W5
C
C     STEPS
C
C
C     1. GET THE LOCATION OF SURFACE J+1 IN THE COORDINATE SYSTEM OF
C     SURFACE I-1. WRITE IT DOWN
C
              SAVE_KDP(1)=SAVEINPT(1)
              WC='GLOBAL'
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I-1)
              CALL GLOBAL
              REST_KDP(1)=RESTINPT(1)
C     NOW GET ALL THE GOODIES
              DX1=VERTEX(1,J+1)
              DY1=VERTEX(2,J+1)
              DZ1=VERTEX(3,J+1)
              D11=VERTEX(4,J+1)
              D12=VERTEX(5,J+1)
              D13=VERTEX(6,J+1)
              D21=VERTEX(7,J+1)
              D22=VERTEX(8,J+1)
              D23=VERTEX(9,J+1)
              D31=VERTEX(10,J+1)
              D32=VERTEX(11,J+1)
              D33=VERTEX(12,J+1)
C     CALCULATE GLOBAL DECENTERS AND TILTS
              GDECX=DX1
              GDECY=DY1
              GDECZ=DZ1
C     CALCULATE GLOBAL ALPHA, BETA AND GAMMA
C     NOW CALCULATE ALPHA,BETA AND GAMMA AND ASSIGN THEM TO SURFACE I
C     CALCULATE BETA
              BETA=DASIN(-D31)
              COSB=DCOS(BETA)
              IF(COSB.NE.0.0D0) THEN
C     COSINE OF BETA IS NOT ZERO
                  IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).NE.0.0D0) ALPHA=0.0D0
                  IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=0.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=PII/2.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).NE.0.0D0)
     1            ALPHA=DATAN2((D32/COSB),(D33/COSB))
                  IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).NE.0.0D0) GAMMA=0.0D0
                  IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=0.0D0
                  IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=PII/2.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D11/COSB).NE.0.0D0)
     1            GAMMA=DATAN2((-D21/COSB),(D11/COSB))
                  ALPHA=(180.0D0/PII)*ALPHA
                  BETA=(180.0D0/PII)*BETA
                  GAMMA=(180.0D0/PII)*GAMMA
              END IF
              IF(COSB.EQ.0.0D0) THEN
C     COSINE OF BETA IS ZERO
                  IF(D31.EQ.-1.0D0) SINB=1
                  IF(D31.EQ.1.0D0) SINB=-1
                  IF(SINB.EQ.1) BETA=90.0D0
                  IF(SINB.EQ.-1) BETA=-90.0D0
                  GAMMA=0.0D0
                  IF(SINB.EQ.1) THEN
                      IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=PII/2.0D0
                      IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=DATAN2((D12),(D13))
                  END IF
                  IF(SINB.EQ.-1) THEN
                      IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=PII/2.0D0
                      IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=DATAN2((-D12),(-D13))
                  END IF
                  ALPHA=(180.0D0/PII)*ALPHA
              END IF
              GALPHA=ALPHA
              GBETA=BETA
              GGAMMA=GAMMA
C
C     2. APPLY A TILT TO SURFACE I EQUAL TO THE BTILT VALUES
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I)
              SURFN=I
              CALL CONTRO
              WC='ALPHA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DALPHA+ALENS(118,I)
              CALL CONTRO
              WC='BETA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBETA+ALENS(119,I)
              CALL CONTRO
              WC='GAMMA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DGAMMA+ALENS(120,I)
              CALL CONTRO
              IF(.NOT.BTILTAUTO) THEN
                  IF(BTILTXP.NE.0.0D0.OR.BTILTYP.NE.0.0D0.OR.
     1            BTILTZP.NE.0.0D0) THEN
                      WC='PIVOT'
                      WQ='        '
                      SQ=0
                      S1=1
                      S2=1
                      S3=1
                      S4=0
                      S5=0
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=1
                      DF5=1
                      SN=1
                      SST=0
                      STI=0
                      W1=BTILTXP
                      W2=BTILTYP
                      W3=BTILTZP
                      CALL CONTRO
                  END IF
              ELSE
                  IF(BTILTXP.NE.0.0D0.OR.BTILTYP.NE.0.0D0.OR.
     1            BTILTZP.NE.0.0D0.OR.REFRY(1,SURFN).NE.0.0D0.OR.
     2            REFRY(2,SURFN).NE.0.0D0.OR.REFRY(3,SURFN).NE.0.0D0) THEN
                      WC='PIVOT'
                      WQ='        '
                      SQ=0
                      S1=1
                      S2=1
                      S3=1
                      S4=0
                      S5=0
                      DF1=0
                      DF2=0
                      DF3=0
                      DF4=1
                      DF5=1
                      SN=1
                      SST=0
                      STI=0
                      W1=REFRY(1,SURFN)+BTILTXP
                      W2=REFRY(2,SURFN)+BTILTYP
                      W3=REFRY(3,SURFN)+BTILTZP
                      CALL CONTRO
                  END IF
              END IF
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
C
C     3. APPLY A RETURN ON SURFACE J+1 TO RETURN TO I-1
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(J+1)
              CALL CONTRO
              WC='TILT'
              WQ='RET'
              SQ=1
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I-1)
              CALL CONTRO
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
C
C     4. ASSIGN GLOBAL DECENTERS AND TILTS TO SURFACE J+1 SO THAT
C     IT RETURNS TO ITS ORIGINAL POSITION WITH RESPECT TO I-1
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(J+1)
              CALL CONTRO
              WC='GDX'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECX
              CALL CONTRO
              WC='GDY'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECY
              CALL CONTRO
              WC='GDZ'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GDECZ
              CALL CONTRO
              WC='GALPHA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GALPHA
              CALL CONTRO
              WC='GBETA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GBETA
              CALL CONTRO
              WC='GGAMMA'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=GGAMMA
              CALL CONTRO
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              BTILTXP=0.0D0
              BTILTYP=0.0D0
              BTILTZP=0.0D0
              BTILTAUTO=.FALSE.
              REFEXT=.FALSE.
          ELSE
          END IF
          IF(WQ.EQ.'PIVOT') THEN
              IF(S1.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1) THEN
                  BTILTXP=W1
                  BTILTYP=W2
                  BTILTZP=W3
              ELSE
                  BTILTXP=0.0D0
                  BTILTYP=0.0D0
                  BTILTZP=0.0D0
              END IF
          ELSE
          END IF
          IF(WQ.EQ.'PIVAUTO') THEN
              IF(REFEXT) THEN
                  BTILTXP=W1
                  BTILTYP=W2
                  BTILTZP=W3
                  BTILTAUTO=.TRUE.
              ELSE
                  BTILTXP=0.0D0
                  BTILTYP=0.0D0
                  BTILTZP=0.0D0
                  BTILTAUTO=.FALSE.
                  WRITE(OUTLYNE,*)
     1            '"BTILT PIVAUTO" REQUIRES REFERENCE RAY DATA TO EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TRACE A CHIEF RAY WITH THE "FOB" COMMAND'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'AND THEN ISSUE "BTILT PIVAUTO" AGAIN'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
          END IF
C     NOW DELETE ALL RET AND PIVOTS
          IF(SQ.EQ.0) THEN
C     NOW DELETE ALL RET FROM J+1
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='UPDATE LENS'
              CALL PROCES
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(J+1)
              CALL CONTRO
              WC='TILT'
              WQ='RETD'
              SQ=1
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=0
              SST=0
              STI=0
              CALL CONTRO
C     NOW DELETE ALL PIVOT FROM I
              WC='CHG'
              WQ='        '
              SQ=0
              S1=1
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=0
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=1
              SST=0
              STI=0
              W1=DBLE(I)
              CALL CONTRO
              WC='PIVOTD'
              WQ='        '
              SQ=0
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=0
              SST=0
              STI=0
              CALL CONTRO
              INPUT='EOS'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          RETURN
      END
