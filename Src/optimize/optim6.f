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

C       SIXTH SET OF OPTIMIZATION ROUTINES

C ROUTINE DEROFF.FOR
C
      SUBROUTINE DEROFF
C
          IMPLICIT NONE

          LOGICAL ITERROR
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
C
          IF(OPTM_INIT.EQ.1) THEN
              DEREXT=.FALSE.
              SOLEXT=.FALSE.
              ITERROR=.FALSE.
              CALL ITER(0,0,ITERROR)
          END IF
C
          RETURN
      END
C SUB FMT.FOR
      SUBROUTINE FMT
C
          IMPLICIT NONE
C
          INTEGER I,TAGER,CFGCHK
C
          REAL*8 CFMTFMT,FMTFMT1
C
          LOGICAL NOP,ALLER,CFGER
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
          CFGER=.FALSE.
          TAGER=0
          ALLER=.TRUE.
C
C       THIS IS SUBROUTINE FMT. THIS IS THE SUBROUTINE WHICH
C       HANDLES CMD LEVEL COMMAND "FMT"
C
          NOP=.FALSE.
          IF(SQ.EQ.1.AND.WQ.EQ.'NP') THEN
              NOP=.TRUE.
              SQ=0
              WQ='        '
          END IF
C
C
          IF(OPCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'THERE ARE NO OPERANDS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO FIGURE OF MERIT CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       "FMT" TAKES NO INPUT WORDS
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"FMT" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
              WRITE(OUTLYNE,*)
     1        '"FMT" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"FMT" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(S1.EQ.1.AND.SQ.EQ.0) THEN
C     DO FMT FOR A SINGLE OPERAND
              ALLER=.FALSE.
              TAGER=INT(W1)
              CFGER=.FALSE.
          END IF
          IF(S1.EQ.1.AND.SQ.EQ.1) THEN
C     DO FMT FOR A SINGLE CFG
              ALLER=.FALSE.
              TAGER=INT(W1)
              CFGER=.TRUE.
          END IF
C
C
          IF(.NOT.CFGER.AND..NOT.ALLER) THEN
C     CHECK FOR VALID TAGER
              IF(TAGER.GT.OPCNT) THEN
C     OPERAND DOES NOT EXIST
                  WRITE(OUTLYNE,*)
     1            'OPERAND NUMBER ',TAGER,'DOES NOT EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(CFGER) THEN
C     CHECK FOR TAGER GREATER THAN MAXCFG
              IF(TAGER.GT.MAXCFG) THEN
C     CFG NOT EXISTANT
                  WRITE(OUTLYNE,*)
     1            'CONFIG NUMBER BEYOND CURRENT PROGRAM LIMIT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     CHECK FOR OPERANDS ACTIVE IN CFG TAGER
              CFGCHK=0
              DO I=1,OPCNT
                  IF(INT(OPERND(I,16)).EQ.TAGER) CFGCHK=1
              END DO

              IF(CFGCHK.EQ.0) THEN
C     CFG OP DATA NOT EXISTANT
                  WRITE(OUTLYNE,*)
     1            'NO OPERANDS ARE ACTIVE IN CONFIG # ',TAGER
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
          END IF
C     NOW PROCEED WITH THE OUTPUT
C
          OPCALC_TYPE=3
          CALL OPCALC
          IF(F28.EQ.0) RETURN
          CALL OPLOAD
          IF(F28.EQ.0) RETURN
          IF(KILOPT) THEN
              OUTLYNE='SOME OPERANDS ARE NOT CALCULABLE'
              CALL SHOWIT(1)
              OUTLYNE='THE CURRENT FIGURE OF MERIT IS MEANINGLESS.'
              CALL SHOWIT(1)
              RETURN
          END IF
          FMTFLG=.TRUE.
C       PROCEED WITH ACTION FOR COMMAND
          IF(ALLER) THEN
              CFMTFMT=0.0D0
              DO I=1,OPCNT
                  CFMTFMT=CFMTFMT+(OPERND(I,14)**2)
              END DO
          END IF
          IF(.NOT.ALLER.AND..NOT.CFGER) THEN
              FMTFMT1=0.0D0
              FMTFMT1=(OPERND(TAGER,14)**2)
          END IF
          IF(.NOT.ALLER.AND.CFGER) THEN
              FMTFMT1=0.0D0
              DO I=1,OPCNT
                  IF(INT(OPERND(I,16)).EQ.TAGER)
     1                      FMTFMT1=FMTFMT1+(OPERND(I,14)**2)
              END DO
          END IF
 10       FORMAT('CURRENT FMT = ',G23.15)
 101      FORMAT('FOR OPERAND NUMBER = ',I3)
 102      FORMAT('  FMT CONTRIBUTION = ',G23.15)
 201      FORMAT('FOR CONFIG. NUMBER = ',I2)
          IF(.NOT.NOP) THEN
C
              IF(ALLER) THEN
C     PRINT MESSAGE
                  WRITE(OUTLYNE,10) CFMTFMT
                  CALL SHOWIT(0)
              END IF
              IF(.NOT.ALLER.AND..NOT.CFGER) THEN
C     PRINT MESSAGE
                  WRITE(OUTLYNE,101) TAGER
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) FMTFMT1
                  CALL SHOWIT(0)
              END IF
              IF(.NOT.ALLER.AND.CFGER) THEN
C     PRINT MESSAGE
                  WRITE(OUTLYNE,201) TAGER
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) FMTFMT1
                  CALL SHOWIT(0)
              END IF
C
          END IF
          RETURN
C       ALL DONE
      END


C SUB FMT2.FOR
      SUBROUTINE FMT2
C
          IMPLICIT NONE
C
          INTEGER I,TAGER,CFGCHK
C
          REAL*8 FMTFMT1,DELFMT
C
          LOGICAL NOP,ALLER,CFGER
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
          CFGER=.FALSE.
          TAGER=0
          ALLER=.TRUE.
C
          NOP=.FALSE.
          IF(SQ.EQ.1.AND.WQ.EQ.'NP') THEN
              NOP=.TRUE.
              SQ=0
              WQ='        '
          END IF
C
C
          IF(OPCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'THERE ARE NO OPERANDS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO FIGURE OF MERIT CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       "FMT" TAKES NO INPUT WORDS
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"FMT" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
              WRITE(OUTLYNE,*)
     1        '"FMT" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"FMT" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.1.AND.SQ.EQ.0) THEN
C     DO FMT FOR A SINGLE OPERAND
              ALLER=.FALSE.
              TAGER=INT(W1)
              CFGER=.FALSE.
          END IF
          IF(S1.EQ.1.AND.SQ.EQ.1) THEN
C     DO FMT FOR A SINGLE CFG
              ALLER=.FALSE.
              TAGER=INT(W1)
              CFGER=.TRUE.
          END IF
C
C
          IF(.NOT.CFGER.AND..NOT.ALLER) THEN
C     CHECK FOR VALID TAGER
              IF(TAGER.GT.OPCNT) THEN
C     OPERAND DOES NOT EXIST
                  WRITE(OUTLYNE,*)
     1            'OPERAND NUMBER ',TAGER,'DOES NOT EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(CFGER) THEN
C     CHECK FOR TAGER GREATER THAN MAXCFG
              IF(TAGER.GT.MAXCFG) THEN
C     CFG NOT EXISTANT
                  WRITE(OUTLYNE,*)
     1            'CONFIG NUMBER BEYOND CURRENT PROGRAM LIMIT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     CHECK FOR OPERANDS ACTIVE IN CFG TAGER
              CFGCHK=0
              DO I=1,OPCNT
                  IF(INT(OPERND(I,16)).EQ.TAGER) CFGCHK=1
              END DO

              IF(CFGCHK.EQ.0) THEN
C     CFG OP DATA NOT EXISTANT
                  WRITE(OUTLYNE,*)
     1            'NO OPERANDS ARE ACTIVE IN CONFIG # ',TAGER
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
          END IF
C     NOW PROCEED WITH THE OUTPUT
C
          IF(ALLER) OLDFMT=FMTFMT
          OPCALC_TYPE=3
          CALL OPCALC
          IF(F28.EQ.0) RETURN
          CALL OPLOAD
          IF(F28.EQ.0) RETURN
          FMTFLG=.TRUE.
C       PROCEED WITH ACTION FOR COMMAND
          IF(ALLER) THEN
              FMTFMT=0.0D0
              DO I=1,OPCNT
                  FMTFMT=FMTFMT+(OPERND(I,14)**2)
              END DO
          END IF
          IF(.NOT.ALLER.AND..NOT.CFGER) THEN
              FMTFMT1=0.0D0
              FMTFMT1=(OPERND(TAGER,14)**2)
          END IF
          IF(.NOT.ALLER.AND.CFGER) THEN
              FMTFMT1=0.0D0
              DO I=1,OPCNT
                  IF(INT(OPERND(I,16)).EQ.TAGER)
     1                      FMTFMT1=FMTFMT1+(OPERND(I,14)**2)
              END DO
          END IF
          IF(ALLER) DELFMT=FMTFMT-OLDFMT
 10       FORMAT('    NEW FMT = ',G23.15,1X,'CHANGE = ',G23.15)
 101      FORMAT('FOR OPERAND NUMBER = ',I3)
 102      FORMAT('  FMT CONTRIBUTION = ',G23.15)
 201      FORMAT('FOR CONFIG. NUMBER = ',I2)
          IF(.NOT.NOP) THEN
C
              IF(ALLER) THEN
C     PRINT MESSAGE
                  WRITE(OUTLYNE,10) FMTFMT,(FMTFMT-OLDFMT)
                  CALL SHOWIT(0)

                  IF(.NOT.FMTEXT) OLDFMT=FMTFMT
                  IF(.NOT.FMTEXT) FMTEXT=.TRUE.
              END IF
              IF(.NOT.ALLER.AND..NOT.CFGER) THEN
C     PRINT MESSAGE
                  WRITE(OUTLYNE,101) TAGER
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) FMTFMT1
                  CALL SHOWIT(0)
              END IF
              IF(.NOT.ALLER.AND.CFGER) THEN
C     PRINT MESSAGE
                  WRITE(OUTLYNE,201) TAGER
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) FMTFMT1
                  CALL SHOWIT(0)
              END IF
C
          END IF
          RETURN
C       ALL DONE
      END
C SUB FMT4.FOR
      SUBROUTINE FMT4
C
          IMPLICIT NONE
C
          INTEGER I,TAGER,CFGCHK
C
          REAL*8 FMTFMT1,DELFMT
C
          LOGICAL NOP,ALLER,CFGER
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
          CFGER=.FALSE.
          TAGER=0
          ALLER=.TRUE.
C
          NOP=.FALSE.
          IF(SQ.EQ.1.AND.WQ.EQ.'NP') THEN
              NOP=.TRUE.
              SQ=0
              WQ='        '
          END IF
C
C
          IF(OPCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'THERE ARE NO OPERANDS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO FIGURE OF MERIT CAN EXIST'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       "FMT" TAKES NO INPUT WORDS
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"FMT" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
              WRITE(OUTLYNE,*)
     1        '"FMT" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"FMT" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.1.AND.SQ.EQ.0) THEN
C     DO FMT FOR A SINGLE OPERAND
              ALLER=.FALSE.
              TAGER=INT(W1)
              CFGER=.FALSE.
          END IF
          IF(S1.EQ.1.AND.SQ.EQ.1) THEN
C     DO FMT FOR A SINGLE CFG
              ALLER=.FALSE.
              TAGER=INT(W1)
              CFGER=.TRUE.
          END IF
C
C
          IF(.NOT.CFGER.AND..NOT.ALLER) THEN
C     CHECK FOR VALID TAGER
              IF(TAGER.GT.OPCNT) THEN
C     OPERAND DOES NOT EXIST
                  WRITE(OUTLYNE,*)
     1            'OPERAND NUMBER ',TAGER,'DOES NOT EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(CFGER) THEN
C     CHECK FOR TAGER GREATER THAN MAXCFG
              IF(TAGER.GT.MAXCFG) THEN
C     CFG NOT EXISTANT
                  WRITE(OUTLYNE,*)
     1            'CONFIG NUMBER BEYOND CURRENT PROGRAM LIMIT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     CHECK FOR OPERANDS ACTIVE IN CFG TAGER
              CFGCHK=0
              DO I=1,OPCNT
                  IF(INT(OPERND(I,16)).EQ.TAGER) CFGCHK=1
              END DO

              IF(CFGCHK.EQ.0) THEN
C     CFG OP DATA NOT EXISTANT
                  WRITE(OUTLYNE,*)
     1            'NO OPERANDS ARE ACTIVE IN CONFIG # ',TAGER
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
          END IF
C     NOW PROCEED WITH THE OUTPUT
C
          IF(ALLER) OLDFMT=FMTFMT
          OPCALC_TYPE=3
          CALL OPCALC
          IF(F28.EQ.0) RETURN
          CALL OPLOAD
          IF(F28.EQ.0) RETURN
          FMTFLG=.TRUE.
C       PROCEED WITH ACTION FOR COMMAND
          IF(ALLER) THEN
              FMTFMT=0.0D0
              DO I=1,OPCNT
                  FMTFMT=FMTFMT+(OPERND(I,14)**2)
              END DO
          END IF
          IF(.NOT.ALLER.AND..NOT.CFGER) THEN
              FMTFMT1=0.0D0
              FMTFMT1=(OPERND(TAGER,14)**2)
          END IF
          IF(.NOT.ALLER.AND.CFGER) THEN
              FMTFMT1=0.0D0
              DO I=1,OPCNT
                  IF(INT(OPERND(I,16)).EQ.TAGER)
     1                      FMTFMT1=FMTFMT1+(OPERND(I,14)**2)
              END DO
          END IF
          IF(ALLER) DELFMT=FMTFMT-OLDFMT
! 10   FORMAT('    NEW FMT = ',G23.15,1X,'CHANGE = ',G23.15)
! 101  FORMAT('FOR OPERAND NUMBER = ',I3)
! 102  FORMAT('  FMT CONTRIBUTION = ',G23.15)
! 201  FORMAT('FOR CONFIG. NUMBER = ',I2)
! 20   FORMAT('FMT(CHANGE) = ',G23.15)
          IF(.NOT.NOP) THEN
C
              IF(ALLER) THEN
C     PRINT MESSAGE
                  IF(.NOT.FMTEXT) OLDFMT=FMTFMT
                  IF(.NOT.FMTEXT) FMTEXT=.TRUE.
              END IF
C
          END IF
          RETURN
C       ALL DONE
      END
C SUB FIXCVAR.FOR
      SUBROUTINE FIXCVAR
C
          IMPLICIT NONE
C
          INTEGER II,I,VALT,VBSURF
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
          DO I=1,MAXCMP
              IF(ISCOMP(I)) THEN
                  II=I
                  VBSURF=INT(VARABL(II,3))
                  VALT=INT(VARABL(II,1))
                  IF(VALT.EQ.2.OR.VALT.EQ.1) THEN
                      VARABL(II,4)=ALENS(1,VBSURF)
                      VARABL(II,5)=ALENS(1,VBSURF)
                      VARABL(II,13)=ALENS(1,VBSURF)
                      VARABL(II,6)=0.0D0
                  END IF
C
C     DO TH VARIABLE
                  IF(VALT.EQ.3) THEN
C
                      VARABL(II,4)=ALENS(3,VBSURF)
                      VARABL(II,5)=ALENS(3,VBSURF)
                      VARABL(II,13)=ALENS(3,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 3, NOT A THICKNESS VARIABLE
                  END IF
C     DO CC
                  IF(VALT.EQ.4) THEN
C
                      VARABL(II,4)=ALENS(2,VBSURF)
                      VARABL(II,5)=ALENS(2,VBSURF)
                      VARABL(II,13)=ALENS(2,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 4, CC VARIABLE
                  END IF
                  IF(VALT.EQ.5) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C
                      VARABL(II,4)=ALENS(4,VBSURF)
                      VARABL(II,5)=ALENS(4,VBSURF)
                      VARABL(II,13)=ALENS(4,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 5, AD VARIABLE
                  END IF
C
                  IF(VALT.EQ.6) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(II,4)=ALENS(5,VBSURF)
                      VARABL(II,5)=ALENS(5,VBSURF)
                      VARABL(II,13)=ALENS(5,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 6, AE VARIABLE
                  END IF
C
                  IF(VALT.EQ.7) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(II,4)=ALENS(6,VBSURF)
                      VARABL(II,5)=ALENS(6,VBSURF)
                      VARABL(II,13)=ALENS(6,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 7, AF VARIABLE
                  END IF
C
                  IF(VALT.EQ.8) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(II,4)=ALENS(7,VBSURF)
                      VARABL(II,5)=ALENS(7,VBSURF)
                      VARABL(II,13)=ALENS(7,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 8, AG VARIABLE
                  END IF
                  IF(VALT.EQ.129) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(II,4)=ALENS(81,VBSURF)
                      VARABL(II,5)=ALENS(81,VBSURF)
                      VARABL(II,13)=ALENS(81,VBSURF)
                      VARABL(II,6)=0.0D0
                  END IF
                  IF(VALT.EQ.130) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(II,4)=ALENS(82,VBSURF)
                      VARABL(II,5)=ALENS(82,VBSURF)
                      VARABL(II,13)=ALENS(82,VBSURF)
                      VARABL(II,6)=0.0D0
                  END IF
                  IF(VALT.EQ.131) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(II,4)=ALENS(83,VBSURF)
                      VARABL(II,5)=ALENS(83,VBSURF)
                      VARABL(II,13)=ALENS(83,VBSURF)
                      VARABL(II,6)=0.0D0
                  END IF
                  IF(VALT.EQ.132) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(II,4)=ALENS(84,VBSURF)
                      VARABL(II,5)=ALENS(84,VBSURF)
                      VARABL(II,13)=ALENS(84,VBSURF)
                      VARABL(II,6)=0.0D0
                  END IF
                  IF(VALT.EQ.133) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(II,4)=ALENS(85,VBSURF)
                      VARABL(II,5)=ALENS(85,VBSURF)
                      VARABL(II,13)=ALENS(85,VBSURF)
                      VARABL(II,6)=0.0D0
                  END IF
C     DO ZD
                  IF(VALT.EQ.134) THEN
                      VARABL(II,4)=ALENS(116,VBSURF)
                      VARABL(II,5)=ALENS(116,VBSURF)
                      VARABL(II,13)=ALENS(116,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 134, ZD VARIABLE
                  END IF
C     DO INDEX
                  IF(VALT.EQ.135) THEN
                      VARABL(II,4)=ALENS(86,VBSURF)
                      VARABL(II,5)=ALENS(86,VBSURF)
                      VARABL(II,13)=ALENS(86,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 135, INDEX VARIABLE
                  END IF
C     DO VNUM
                  IF(VALT.EQ.136) THEN
                      VARABL(II,4)=ALENS(87,VBSURF)
                      VARABL(II,5)=ALENS(87,VBSURF)
                      VARABL(II,13)=ALENS(87,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 136, VNUM VARIABLE
                  END IF
C     DO PIVX
                  IF(VALT.EQ.137) THEN
                      VARABL(II,4)=ALENS(78,VBSURF)
                      VARABL(II,5)=ALENS(78,VBSURF)
                      VARABL(II,13)=ALENS(78,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 137, PIVX VARIABLE
                  END IF
C     DO PIVY
                  IF(VALT.EQ.138) THEN
                      VARABL(II,4)=ALENS(79,VBSURF)
                      VARABL(II,5)=ALENS(79,VBSURF)
                      VARABL(II,13)=ALENS(79,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 138, PIVY VARIABLE
                  END IF
C     DO PIVZ
                  IF(VALT.EQ.139) THEN
                      VARABL(II,4)=ALENS(80,VBSURF)
                      VARABL(II,5)=ALENS(80,VBSURF)
                      VARABL(II,13)=ALENS(80,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 139, PIVZ VARIABLE
                  END IF
C     DO DPART
                  IF(VALT.EQ.140) THEN
                      VARABL(II,4)=ALENS(89,VBSURF)
                      VARABL(II,5)=ALENS(89,VBSURF)
                      VARABL(II,13)=ALENS(89,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 140, DPART VARIABLE
                  END IF
C     DO CLPX
                  IF(VALT.EQ.141) THEN
                      VARABL(II,4)=ALENS(11,VBSURF)
                      VARABL(II,5)=ALENS(11,VBSURF)
                      VARABL(II,13)=ALENS(11,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 141, CLPX VARIABLE
                  END IF
C     DO CLPY
                  IF(VALT.EQ.142) THEN
                      VARABL(II,4)=ALENS(10,VBSURF)
                      VARABL(II,5)=ALENS(10,VBSURF)
                      VARABL(II,13)=ALENS(10,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 142, CLPY VARIABLE
                  END IF
C     DO GDX
                  IF(VALT.EQ.143) THEN
                      VARABL(II,4)=ALENS(90,VBSURF)
                      VARABL(II,5)=ALENS(90,VBSURF)
                      VARABL(II,13)=ALENS(90,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 143, GDX VARIABLE
                  END IF
C     DO GDY
                  IF(VALT.EQ.144) THEN
                      VARABL(II,4)=ALENS(91,VBSURF)
                      VARABL(II,5)=ALENS(91,VBSURF)
                      VARABL(II,13)=ALENS(91,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 144, GDY VARIABLE
                  END IF
C     DO GDZ
                  IF(VALT.EQ.145) THEN
                      VARABL(II,4)=ALENS(92,VBSURF)
                      VARABL(II,5)=ALENS(92,VBSURF)
                      VARABL(II,13)=ALENS(92,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 145, GDZ VARIABLE
                  END IF
C     DO GALPHA
                  IF(VALT.EQ.146) THEN
                      VARABL(II,4)=ALENS(93,VBSURF)
                      VARABL(II,5)=ALENS(93,VBSURF)
                      VARABL(II,13)=ALENS(93,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 146, GALPHA VARIABLE
                  END IF
C     DO GBETA
                  IF(VALT.EQ.147) THEN
                      VARABL(II,4)=ALENS(94,VBSURF)
                      VARABL(II,5)=ALENS(94,VBSURF)
                      VARABL(II,13)=ALENS(94,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 147, GBETA VARIABLE
                  END IF
C     DO GGAMMA
                  IF(VALT.EQ.148) THEN
                      VARABL(II,4)=ALENS(95,VBSURF)
                      VARABL(II,5)=ALENS(95,VBSURF)
                      VARABL(II,13)=ALENS(95,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 148, GGAMMA VARIABLE
                  END IF
C     DO GRS
                  IF(VALT.EQ.149) THEN
                      VARABL(II,4)=ALENS(98,VBSURF)
                      VARABL(II,5)=ALENS(98,VBSURF)
                      VARABL(II,13)=ALENS(98,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 149, GRS VARIABLE
                  END IF
C
C     DO CVTOR
                  IF(VALT.EQ.10.OR.VALT.EQ.9) THEN
                      VARABL(II,4)=ALENS(24,VBSURF)
                      VARABL(II,5)=ALENS(24,VBSURF)
                      VARABL(II,13)=ALENS(24,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 10, NOT A RDTOR OR CVTOR VARIABLE
                  END IF
C
C     DO CCTOR
                  IF(VALT.EQ.11) THEN
                      VARABL(II,4)=ALENS(41,VBSURF)
                      VARABL(II,5)=ALENS(41,VBSURF)
                      VARABL(II,13)=ALENS(41,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 11, CCTOR VARIABLE
                  END IF
C
C     DO ADTOR
                  IF(VALT.EQ.12) THEN
                      VARABL(II,4)=ALENS(37,VBSURF)
                      VARABL(II,5)=ALENS(37,VBSURF)
                      VARABL(II,13)=ALENS(37,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 12, ADTOR VARIABLE
                  END IF
C
C     DO AETOR
                  IF(VALT.EQ.13) THEN
                      VARABL(II,4)=ALENS(38,VBSURF)
                      VARABL(II,5)=ALENS(38,VBSURF)
                      VARABL(II,13)=ALENS(38,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 13, AETOR VARIABLE
                  END IF
C
C     DO AFTOR
                  IF(VALT.EQ.14) THEN
                      VARABL(II,4)=ALENS(39,VBSURF)
                      VARABL(II,5)=ALENS(39,VBSURF)
                      VARABL(II,13)=ALENS(39,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 14, AFTOR VARIABLE
                  END IF
C
C     DO AGTOR
                  IF(VALT.EQ.15) THEN
                      VARABL(II,4)=ALENS(40,VBSURF)
                      VARABL(II,5)=ALENS(40,VBSURF)
                      VARABL(II,13)=ALENS(40,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 15, AGTOR VARIABLE
                  END IF
C
C     DO ALPHA
                  IF(VALT.EQ.16) THEN
                      VARABL(II,4)=ALENS(118,VBSURF)
                      VARABL(II,5)=ALENS(118,VBSURF)
                      VARABL(II,13)=ALENS(118,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 16, ALPHA VARIABLE
                  END IF
C
C     DO BETA
                  IF(VALT.EQ.17) THEN
                      VARABL(II,4)=ALENS(119,VBSURF)
                      VARABL(II,5)=ALENS(119,VBSURF)
                      VARABL(II,13)=ALENS(119,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 17, BETA VARIABLE
                  END IF
C
C     DO GAMMA
                  IF(VALT.EQ.18) THEN
                      VARABL(II,4)=ALENS(120,VBSURF)
                      VARABL(II,5)=ALENS(120,VBSURF)
                      VARABL(II,13)=ALENS(120,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 18, GAMMA VARIABLE
                  END IF
C
C     DO YD
                  IF(VALT.EQ.20) THEN
                      VARABL(II,4)=ALENS(115,VBSURF)
                      VARABL(II,5)=ALENS(115,VBSURF)
                      VARABL(II,13)=ALENS(115,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 20, YD VARIABLE
                  END IF
C
C     DO N1
                  IF(VALT.EQ.21) THEN
                      VARABL(II,4)=ALENS(46,VBSURF)
                      VARABL(II,5)=ALENS(46,VBSURF)
                      VARABL(II,13)=ALENS(46,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 21, N1 VARIABLE
                  END IF
C
C     DO N2
                  IF(VALT.EQ.22) THEN
                      VARABL(II,4)=ALENS(47,VBSURF)
                      VARABL(II,5)=ALENS(47,VBSURF)
                      VARABL(II,13)=ALENS(47,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 22, N2 VARIABLE
                  END IF
C
C     DO N3
                  IF(VALT.EQ.23) THEN
                      VARABL(II,4)=ALENS(48,VBSURF)
                      VARABL(II,5)=ALENS(48,VBSURF)
                      VARABL(II,13)=ALENS(48,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 23, N3 VARIABLE
                  END IF
C
C     DO N4
                  IF(VALT.EQ.24) THEN
                      VARABL(II,4)=ALENS(49,VBSURF)
                      VARABL(II,5)=ALENS(49,VBSURF)
                      VARABL(II,13)=ALENS(49,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 24, N4 VARIABLE
                  END IF
C
C     DO N5
                  IF(VALT.EQ.25) THEN
                      VARABL(II,4)=ALENS(50,VBSURF)
                      VARABL(II,5)=ALENS(50,VBSURF)
                      VARABL(II,13)=ALENS(50,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 25, N5 VARIABLE
                  END IF
C
C     NOW DO THE COEFFS
C
C     DO C1 TO C48
                  IF(VALT.GE.27.AND.VALT.LE.74) THEN
                      VARABL(II,4)=FTFL01((VALT-26),VBSURF)
                      VARABL(II,5)=FTFL01((VALT-26),VBSURF)
                      VARABL(II,13)=FTFL01((VALT-26),VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 27 TO 74, C1 - C48 VARIABLE
                  END IF
C     DO C49 TO C96
                  IF(VALT.GE.76.AND.VALT.LE.123) THEN
                      VARABL(II,4)=FTFL01((VALT-27),VBSURF)
                      VARABL(II,5)=FTFL01((VALT-27),VBSURF)
                      VARABL(II,13)=FTFL01((VALT-27),VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 76 TO 123, C49 - C96 VARIABLE
                  END IF
C
C     DO AC
                  IF(VALT.EQ.75) THEN
                      VARABL(II,4)=ALENS(43,VBSURF)
                      VARABL(II,5)=ALENS(43,VBSURF)
                      VARABL(II,13)=ALENS(43,VBSURF)
                      VARABL(II,6)=0.0D0
C     VALT NOT 75, AC VARIABLE
                  END IF
C
                  CONTINUE
              END IF
          END DO
C       ALL DONE
          RETURN
      END
C SUB FIXTVAR.FOR
      SUBROUTINE FIXTVAR
C
          IMPLICIT NONE
C
          INTEGER II,I,VALT,VBSURF
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
C     NOW BUILD THE VARIABLE ENTRY
C       J=1  > 1 THROUGH 164, A VARIABLE TYPE DESIGNATOR
C       J=3  > 0 THROUGH MAXSUR, THE SURFACE NUMBER DESIGNATOR
C       J=4  > VARIABLE CURRENT VALUE
C       J=5  > VARIABLE PREVIOUS VALUE
C       J=6  > LAST VARIABLE CHANGE VALUE
C
C
          IF(TVBCNT.EQ.0) RETURN
          DO I=1,TVBCNT
              II=I+MAXCMP
              VBSURF=INT(VARABL(II,3))
              VALT=INT(VARABL(II,1))
              IF(VALT.EQ.1.OR.VALT.EQ.134) THEN
                  IF(ALENS(1,VBSURF).EQ.0.0D0) THEN
                      VARABL(II,4)=0.0D0
                      VARABL(II,5)=0.0D0
                      VARABL(II,13)=0.0D0
                  ELSE
                      VARABL(II,4)=1.0D0/ALENS(1,VBSURF)
                      VARABL(II,5)=1.0D0/ALENS(1,VBSURF)
                      VARABL(II,13)=1.0D0/ALENS(1,VBSURF)
                  END IF
                  VARABL(II,6)=0.0D0
              END IF
              IF(VALT.EQ.2.OR.VALT.EQ.135) THEN
                  VARABL(II,4)=ALENS(1,VBSURF)
                  VARABL(II,5)=ALENS(1,VBSURF)
                  VARABL(II,13)=ALENS(1,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C
C     DO TH VARIABLE
              IF(VALT.EQ.3) THEN
C
                  VARABL(II,4)=ALENS(3,VBSURF)
                  VARABL(II,5)=ALENS(3,VBSURF)
                  VARABL(II,13)=ALENS(3,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 3, NOT A THICKNESS VARIABLE
              END IF
C     DO CC
              IF(VALT.EQ.4) THEN
C
                  VARABL(II,4)=ALENS(2,VBSURF)
                  VARABL(II,5)=ALENS(2,VBSURF)
                  VARABL(II,13)=ALENS(2,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 4, CC VARIABLE
              END IF
              IF(VALT.EQ.5) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C
                  VARABL(II,4)=ALENS(4,VBSURF)
                  VARABL(II,5)=ALENS(4,VBSURF)
                  VARABL(II,13)=ALENS(4,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 5, AD VARIABLE
              END IF
C
              IF(VALT.EQ.6) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                  VARABL(II,4)=ALENS(5,VBSURF)
                  VARABL(II,5)=ALENS(5,VBSURF)
                  VARABL(II,13)=ALENS(5,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 6, AE VARIABLE
              END IF
C
              IF(VALT.EQ.7) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                  VARABL(II,4)=ALENS(6,VBSURF)
                  VARABL(II,5)=ALENS(6,VBSURF)
                  VARABL(II,13)=ALENS(6,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 7, AF VARIABLE
              END IF
C
              IF(VALT.EQ.8) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                  VARABL(II,4)=ALENS(7,VBSURF)
                  VARABL(II,5)=ALENS(7,VBSURF)
                  VARABL(II,13)=ALENS(7,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 8, AG VARIABLE
              END IF
              IF(VALT.EQ.129) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                  VARABL(II,4)=ALENS(81,VBSURF)
                  VARABL(II,5)=ALENS(81,VBSURF)
                  VARABL(II,13)=ALENS(81,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
              IF(VALT.EQ.130) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                  VARABL(II,4)=ALENS(82,VBSURF)
                  VARABL(II,5)=ALENS(82,VBSURF)
                  VARABL(II,13)=ALENS(82,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
              IF(VALT.EQ.131) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                  VARABL(II,4)=ALENS(83,VBSURF)
                  VARABL(II,5)=ALENS(83,VBSURF)
                  VARABL(II,13)=ALENS(83,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
              IF(VALT.EQ.132) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                  VARABL(II,4)=ALENS(84,VBSURF)
                  VARABL(II,5)=ALENS(84,VBSURF)
                  VARABL(II,13)=ALENS(84,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
              IF(VALT.EQ.133) THEN
                  IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                  VARABL(II,4)=ALENS(85,VBSURF)
                  VARABL(II,5)=ALENS(85,VBSURF)
                  VARABL(II,13)=ALENS(85,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C     DO ZD
              IF(VALT.EQ.138) THEN
                  VARABL(II,4)=ALENS(116,VBSURF)
                  VARABL(II,5)=ALENS(116,VBSURF)
                  VARABL(II,13)=ALENS(116,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 138, ZD VARIABLE
              END IF
C     DO INDEX
              IF(VALT.EQ.139) THEN
                  VARABL(II,4)=ALENS(86,VBSURF)
                  VARABL(II,5)=ALENS(86,VBSURF)
                  VARABL(II,13)=ALENS(86,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 139, INDEX VARIABLE
              END IF
C     DO VNUM
              IF(VALT.EQ.140) THEN
                  VARABL(II,4)=ALENS(87,VBSURF)
                  VARABL(II,5)=ALENS(87,VBSURF)
                  VARABL(II,13)=ALENS(87,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 140, VNUM VARIABLE
              END IF
C     DO PIVX
              IF(VALT.EQ.141) THEN
                  VARABL(II,4)=ALENS(78,VBSURF)
                  VARABL(II,5)=ALENS(78,VBSURF)
                  VARABL(II,13)=ALENS(78,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 141, PIVX VARIABLE
              END IF
C     DO PIVY
              IF(VALT.EQ.142) THEN
                  VARABL(II,4)=ALENS(79,VBSURF)
                  VARABL(II,5)=ALENS(79,VBSURF)
                  VARABL(II,13)=ALENS(79,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 142, PIVY VARIABLE
              END IF
C     DO PIVZ
              IF(VALT.EQ.143) THEN
                  VARABL(II,4)=ALENS(80,VBSURF)
                  VARABL(II,5)=ALENS(80,VBSURF)
                  VARABL(II,13)=ALENS(80,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 143, PIVZ VARIABLE
              END IF
C     DO DPART
              IF(VALT.EQ.144) THEN
                  VARABL(II,4)=ALENS(89,VBSURF)
                  VARABL(II,5)=ALENS(89,VBSURF)
                  VARABL(II,13)=ALENS(89,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 144, DPART VARIABLE
              END IF
C     DO CLPX
              IF(VALT.EQ.145) THEN
                  VARABL(II,4)=ALENS(11,VBSURF)
                  VARABL(II,5)=ALENS(11,VBSURF)
                  VARABL(II,13)=ALENS(11,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 145, CLPX VARIABLE
              END IF
C     DO CLPY
              IF(VALT.EQ.146) THEN
                  VARABL(II,4)=ALENS(10,VBSURF)
                  VARABL(II,5)=ALENS(10,VBSURF)
                  VARABL(II,13)=ALENS(10,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 146, CLPY VARIABLE
              END IF
C     DO GDX
              IF(VALT.EQ.147) THEN
                  VARABL(II,4)=ALENS(90,VBSURF)
                  VARABL(II,5)=ALENS(90,VBSURF)
                  VARABL(II,13)=ALENS(90,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 147, GDX VARIABLE
              END IF
C     DO GDY
              IF(VALT.EQ.148) THEN
                  VARABL(II,4)=ALENS(91,VBSURF)
                  VARABL(II,5)=ALENS(91,VBSURF)
                  VARABL(II,13)=ALENS(91,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 148, GDY VARIABLE
              END IF
C     DO GDZ
              IF(VALT.EQ.149) THEN
                  VARABL(II,4)=ALENS(92,VBSURF)
                  VARABL(II,5)=ALENS(92,VBSURF)
                  VARABL(II,13)=ALENS(92,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 149, GDZ VARIABLE
              END IF
C     DO GALPHA
              IF(VALT.EQ.150) THEN
                  VARABL(II,4)=ALENS(93,VBSURF)
                  VARABL(II,5)=ALENS(93,VBSURF)
                  VARABL(II,13)=ALENS(93,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 150, GALPHA VARIABLE
              END IF
C     DO GBETA
              IF(VALT.EQ.151) THEN
                  VARABL(II,4)=ALENS(94,VBSURF)
                  VARABL(II,5)=ALENS(94,VBSURF)
                  VARABL(II,13)=ALENS(94,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 151, GBETA VARIABLE
              END IF
C     DO GGAMMA
              IF(VALT.EQ.152) THEN
                  VARABL(II,4)=ALENS(95,VBSURF)
                  VARABL(II,5)=ALENS(95,VBSURF)
                  VARABL(II,13)=ALENS(95,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 152, GGAMMA VARIABLE
              END IF
C     DO GRS
              IF(VALT.EQ.153) THEN
                  VARABL(II,4)=ALENS(98,VBSURF)
                  VARABL(II,5)=ALENS(98,VBSURF)
                  VARABL(II,13)=ALENS(98,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 153, GRS VARIABLE
              END IF
C     DO DISPX
              IF(VALT.EQ.154) THEN
                  VARABL(II,4)=TOLER(1,VBSURF)
                  VARABL(II,5)=TOLER(1,VBSURF)
                  VARABL(II,13)=TOLER(1,VBSURF)
                  VARABL(II,7)=TOLER(2,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C     DO DISPY
              IF(VALT.EQ.155) THEN
                  VARABL(II,4)=TOLER(3,VBSURF)
                  VARABL(II,5)=TOLER(3,VBSURF)
                  VARABL(II,13)=TOLER(3,VBSURF)
                  VARABL(II,7)=TOLER(4,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C     DO DISPZ
              IF(VALT.EQ.156) THEN
                  VARABL(II,4)=TOLER(5,VBSURF)
                  VARABL(II,5)=TOLER(5,VBSURF)
                  VARABL(II,13)=TOLER(5,VBSURF)
                  VARABL(II,7)=TOLER(6,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C     DO STILTA
              IF(VALT.EQ.157) THEN
                  VARABL(II,4)=TOLER(7,VBSURF)
                  VARABL(II,5)=TOLER(7,VBSURF)
                  VARABL(II,13)=TOLER(7,VBSURF)
                  VARABL(II,9)=TOLER(8,VBSURF)
                  VARABL(II,10)=TOLER(9,VBSURF)
                  VARABL(II,11)=TOLER(10,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C     DO STILTB
              IF(VALT.EQ.158) THEN
                  VARABL(II,4)=TOLER(11,VBSURF)
                  VARABL(II,5)=TOLER(11,VBSURF)
                  VARABL(II,13)=TOLER(11,VBSURF)
                  VARABL(II,9)=TOLER(12,VBSURF)
                  VARABL(II,10)=TOLER(13,VBSURF)
                  VARABL(II,11)=TOLER(14,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C     DO STILTG
              IF(VALT.EQ.159) THEN
                  VARABL(II,4)=TOLER(15,VBSURF)
                  VARABL(II,5)=TOLER(15,VBSURF)
                  VARABL(II,13)=TOLER(15,VBSURF)
                  VARABL(II,9)=TOLER(16,VBSURF)
                  VARABL(II,10)=TOLER(17,VBSURF)
                  VARABL(II,11)=TOLER(18,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C     DO BTILTA
              IF(VALT.EQ.160) THEN
                  VARABL(II,4)=TOLER(19,VBSURF)
                  VARABL(II,5)=TOLER(19,VBSURF)
                  VARABL(II,13)=TOLER(19,VBSURF)
                  VARABL(II,9)=TOLER(21,VBSURF)
                  VARABL(II,10)=TOLER(22,VBSURF)
                  VARABL(II,11)=TOLER(23,VBSURF)
                  VARABL(II,7)=TOLER(20,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C     DO BTILTB
              IF(VALT.EQ.161) THEN
                  VARABL(II,4)=TOLER(24,VBSURF)
                  VARABL(II,5)=TOLER(24,VBSURF)
                  VARABL(II,13)=TOLER(24,VBSURF)
                  VARABL(II,9)=TOLER(26,VBSURF)
                  VARABL(II,10)=TOLER(27,VBSURF)
                  VARABL(II,11)=TOLER(28,VBSURF)
                  VARABL(II,7)=TOLER(25,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C     DO BTILTG
              IF(VALT.EQ.162) THEN
                  VARABL(II,4)=TOLER(29,VBSURF)
                  VARABL(II,5)=TOLER(29,VBSURF)
                  VARABL(II,13)=TOLER(29,VBSURF)
                  VARABL(II,9)=TOLER(31,VBSURF)
                  VARABL(II,10)=TOLER(32,VBSURF)
                  VARABL(II,11)=TOLER(33,VBSURF)
                  VARABL(II,7)=TOLER(30,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C     DO ROLLX
              IF(VALT.EQ.163) THEN
                  VARABL(II,4)=TOLER(34,VBSURF)
                  VARABL(II,5)=TOLER(34,VBSURF)
                  VARABL(II,13)=TOLER(34,VBSURF)
                  VARABL(II,7)=TOLER(35,VBSURF)
                  VARABL(II,12)=TOLER(36,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C     DO ROLLY
              IF(VALT.EQ.164) THEN
                  VARABL(II,4)=TOLER(37,VBSURF)
                  VARABL(II,5)=TOLER(37,VBSURF)
                  VARABL(II,13)=TOLER(37,VBSURF)
                  VARABL(II,7)=TOLER(38,VBSURF)
                  VARABL(II,12)=TOLER(39,VBSURF)
                  VARABL(II,6)=0.0D0
              END IF
C
C     DO RDTOR
              IF(VALT.EQ.9.OR.VALT.EQ.136) THEN
                  IF(ALENS(24,VBSURF).NE.0.0D0) THEN
                      VARABL(II,4)=1.0D0/ALENS(24,VBSURF)
                      VARABL(II,5)=1.0D0/ALENS(24,VBSURF)
                      VARABL(II,13)=1.0D0/ALENS(24,VBSURF)
                  ELSE
                      VARABL(II,4)=0.0D0
                      VARABL(II,5)=0.0D0
                      VARABL(II,13)=0.0D0
                  END IF
                  VARABL(II,6)=0.0D0
C     VALT NOT 9 01 136 , NOT A RDTOR VARIABLE
              END IF
C     DO CVTOR
              IF(VALT.EQ.10.OR.VALT.EQ.137) THEN
                  VARABL(II,4)=ALENS(24,VBSURF)
                  VARABL(II,5)=ALENS(24,VBSURF)
                  VARABL(II,13)=ALENS(24,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 10 OR 137, NOT CVTOR VARIABLE
              END IF
C
C     DO CCTOR
              IF(VALT.EQ.11) THEN
                  VARABL(II,4)=ALENS(41,VBSURF)
                  VARABL(II,5)=ALENS(41,VBSURF)
                  VARABL(II,13)=ALENS(41,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 11, CCTOR VARIABLE
              END IF
C
C     DO ADTOR
              IF(VALT.EQ.12) THEN
                  VARABL(II,4)=ALENS(37,VBSURF)
                  VARABL(II,5)=ALENS(37,VBSURF)
                  VARABL(II,13)=ALENS(37,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 12, ADTOR VARIABLE
              END IF
C
C     DO AETOR
              IF(VALT.EQ.13) THEN
                  VARABL(II,4)=ALENS(38,VBSURF)
                  VARABL(II,5)=ALENS(38,VBSURF)
                  VARABL(II,13)=ALENS(38,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 13, AETOR VARIABLE
              END IF
C
C     DO AFTOR
              IF(VALT.EQ.14) THEN
                  VARABL(II,4)=ALENS(39,VBSURF)
                  VARABL(II,5)=ALENS(39,VBSURF)
                  VARABL(II,13)=ALENS(39,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 14, AFTOR VARIABLE
              END IF
C
C     DO AGTOR
              IF(VALT.EQ.15) THEN
                  VARABL(II,4)=ALENS(40,VBSURF)
                  VARABL(II,5)=ALENS(40,VBSURF)
                  VARABL(II,13)=ALENS(40,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 15, AGTOR VARIABLE
              END IF
C
C     DO ALPHA
              IF(VALT.EQ.16) THEN
                  VARABL(II,4)=ALENS(118,VBSURF)
                  VARABL(II,5)=ALENS(118,VBSURF)
                  VARABL(II,13)=ALENS(118,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 16, ALPHA VARIABLE
              END IF
C
C     DO BETA
              IF(VALT.EQ.17) THEN
                  VARABL(II,4)=ALENS(119,VBSURF)
                  VARABL(II,5)=ALENS(119,VBSURF)
                  VARABL(II,13)=ALENS(119,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 17, BETA VARIABLE
              END IF
C
C     DO GAMMA
              IF(VALT.EQ.18) THEN
                  VARABL(II,4)=ALENS(120,VBSURF)
                  VARABL(II,5)=ALENS(120,VBSURF)
                  VARABL(II,13)=ALENS(120,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 18, GAMMA VARIABLE
              END IF
C
C     DO XD
              IF(VALT.EQ.19) THEN
                  VARABL(II,4)=ALENS(114,VBSURF)
                  VARABL(II,5)=ALENS(114,VBSURF)
                  VARABL(II,13)=ALENS(114,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 19, XD VARIABLE
              END IF
C
C     DO YD
              IF(VALT.EQ.20) THEN
                  VARABL(II,4)=ALENS(115,VBSURF)
                  VARABL(II,5)=ALENS(115,VBSURF)
                  VARABL(II,13)=ALENS(115,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 20, YD VARIABLE
              END IF
C
C     DO N1
              IF(VALT.EQ.21) THEN
                  VARABL(II,4)=ALENS(46,VBSURF)
                  VARABL(II,5)=ALENS(46,VBSURF)
                  VARABL(II,13)=ALENS(46,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 21, N1 VARIABLE
              END IF
C
C     DO N2
              IF(VALT.EQ.22) THEN
                  VARABL(II,4)=ALENS(47,VBSURF)
                  VARABL(II,5)=ALENS(47,VBSURF)
                  VARABL(II,13)=ALENS(47,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 22, N2 VARIABLE
              END IF
C
C     DO N3
              IF(VALT.EQ.23) THEN
                  VARABL(II,4)=ALENS(48,VBSURF)
                  VARABL(II,5)=ALENS(48,VBSURF)
                  VARABL(II,13)=ALENS(48,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 23, N3 VARIABLE
              END IF
C
C     DO N4
              IF(VALT.EQ.24) THEN
                  VARABL(II,4)=ALENS(49,VBSURF)
                  VARABL(II,5)=ALENS(49,VBSURF)
                  VARABL(II,13)=ALENS(49,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 24, N4 VARIABLE
              END IF
C
C     DO N5
              IF(VALT.EQ.25) THEN
                  VARABL(II,4)=ALENS(50,VBSURF)
                  VARABL(II,5)=ALENS(50,VBSURF)
                  VARABL(II,13)=ALENS(50,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 25, N5 VARIABLE
              END IF
C
C     NOW DO THE COEFFS
C
C     DO C1 TO C48
              IF(VALT.GE.27.AND.VALT.LE.74) THEN
                  VARABL(II,4)=FTFL01((VALT-26),VBSURF)
                  VARABL(II,5)=FTFL01((VALT-26),VBSURF)
                  VARABL(II,13)=FTFL01((VALT-26),VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 27 TO 74, C1 - C48 VARIABLE
              END IF
C     DO C49 TO C96
              IF(VALT.GE.76.AND.VALT.LE.123) THEN
                  VARABL(II,4)=FTFL01((VALT-27),VBSURF)
                  VARABL(II,5)=FTFL01((VALT-27),VBSURF)
                  VARABL(II,13)=FTFL01((VALT-27),VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 76 TO 123, C49 - C96 VARIABLE
              END IF
C
C     DO AC
              IF(VALT.EQ.75) THEN
                  VARABL(II,4)=ALENS(43,VBSURF)
                  VARABL(II,5)=ALENS(43,VBSURF)
                  VARABL(II,13)=ALENS(43,VBSURF)
                  VARABL(II,6)=0.0D0
C     VALT NOT 75, AC VARIABLE
              END IF
C
              CONTINUE
          END DO
C       ALL DONE
          RETURN
      END



C SUB FIXVAR.FOR
      SUBROUTINE FIXVAR
          USE NSSMOD
C
          IMPLICIT NONE
C
          INTEGER I,J,VALT,VBSURF,VADD
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
C     NOW BUILD THE VARIABLE ENTRY
C       J=1  > 2 THROUGH 133, A VARIABLE TYPE DESIGNATOR
C       J=2  > 1 THROUGH MAXCFG, THE CONFIGURATION DESIGNATOR
C       J=3  > 0 THROUGH MAXSUR, THE SURFACE NUMBER DESIGNATOR
C       J=4  > VARIABLE CURRENT VALUE
C       J=5  > VARIABLE PREVIOUS VALUE
C       J=6  > LAST VARIABLE CHANGE VALUE
C
C
          IF(VBCNT.EQ.0) RETURN
          DO I=1,VBCNT
              VBSURF=INT(VARABL(I,3))
              VBCFG=INT(VARABL(I,2))
              VALT=INT(VARABL(I,1))
              IF(VBCFG.EQ.1) THEN
C     CONFIG 1 VARIABLES
                  IF(VALT.EQ.2.OR.VALT.EQ.9) THEN
                      VARABL(I,4)=ALENS(1,VBSURF)
                      VARABL(I,5)=ALENS(1,VBSURF)
                      VARABL(I,13)=ALENS(1,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C
C     DO TH VARIABLE
                  IF(VALT.EQ.3) THEN
C
                      VARABL(I,4)=ALENS(3,VBSURF)
                      VARABL(I,5)=ALENS(3,VBSURF)
                      VARABL(I,13)=ALENS(3,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 3, NOT A THICKNESS VARIABLE
                  END IF
C     DO CC
                  IF(VALT.EQ.4) THEN
C
                      VARABL(I,4)=ALENS(2,VBSURF)
                      VARABL(I,5)=ALENS(2,VBSURF)
                      VARABL(I,13)=ALENS(2,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 4, CC VARIABLE
                  END IF
                  IF(VALT.EQ.5) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
C
                      VARABL(I,4)=ALENS(4,VBSURF)
                      VARABL(I,5)=ALENS(4,VBSURF)
                      VARABL(I,13)=ALENS(4,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 5, AD VARIABLE
                  END IF
C
                  IF(VALT.EQ.6) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(I,4)=ALENS(5,VBSURF)
                      VARABL(I,5)=ALENS(5,VBSURF)
                      VARABL(I,13)=ALENS(5,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 6, AE VARIABLE
                  END IF
C
                  IF(VALT.EQ.7) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(I,4)=ALENS(6,VBSURF)
                      VARABL(I,5)=ALENS(6,VBSURF)
                      VARABL(I,13)=ALENS(6,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 7, AF VARIABLE
                  END IF
C
                  IF(VALT.EQ.8) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(I,4)=ALENS(7,VBSURF)
                      VARABL(I,5)=ALENS(7,VBSURF)
                      VARABL(I,13)=ALENS(7,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 8, AG VARIABLE
                  END IF
                  IF(VALT.EQ.129) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(I,4)=ALENS(81,VBSURF)
                      VARABL(I,5)=ALENS(81,VBSURF)
                      VARABL(I,13)=ALENS(81,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
                  IF(VALT.EQ.130) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(I,4)=ALENS(82,VBSURF)
                      VARABL(I,5)=ALENS(82,VBSURF)
                      VARABL(I,13)=ALENS(82,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
                  IF(VALT.EQ.131) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(I,4)=ALENS(83,VBSURF)
                      VARABL(I,5)=ALENS(83,VBSURF)
                      VARABL(I,13)=ALENS(83,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
                  IF(VALT.EQ.132) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(I,4)=ALENS(84,VBSURF)
                      VARABL(I,5)=ALENS(84,VBSURF)
                      VARABL(I,13)=ALENS(84,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
                  IF(VALT.EQ.133) THEN
                      IF(ALENS(8,VBSURF).NE.1.0D0) ALENS(8,VBSURF)=1.0D0
                      VARABL(I,4)=ALENS(85,VBSURF)
                      VARABL(I,5)=ALENS(85,VBSURF)
                      VARABL(I,13)=ALENS(85,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO ZD
                  IF(VALT.EQ.134) THEN
                      VARABL(I,4)=ALENS(116,VBSURF)
                      VARABL(I,5)=ALENS(116,VBSURF)
                      VARABL(I,13)=ALENS(116,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 134, ZD VARIABLE
                  END IF
C     DO INDEX
                  IF(VALT.EQ.135) THEN
                      VARABL(I,4)=ALENS(86,VBSURF)
                      VARABL(I,5)=ALENS(86,VBSURF)
                      VARABL(I,13)=ALENS(86,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 135, INDEX VARIABLE
                  END IF
C     DO VNUM
                  IF(VALT.EQ.136) THEN
                      VARABL(I,4)=ALENS(87,VBSURF)
                      VARABL(I,5)=ALENS(87,VBSURF)
                      VARABL(I,13)=ALENS(87,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 134, ZD VARIABLE
                  END IF
C     DO PIVX
                  IF(VALT.EQ.137) THEN
                      VARABL(I,4)=ALENS(78,VBSURF)
                      VARABL(I,5)=ALENS(78,VBSURF)
                      VARABL(I,13)=ALENS(78,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 137, PIVX VARIABLE
                  END IF
C     DO PIVY
                  IF(VALT.EQ.138) THEN
                      VARABL(I,4)=ALENS(79,VBSURF)
                      VARABL(I,5)=ALENS(79,VBSURF)
                      VARABL(I,13)=ALENS(79,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 138, PIVY VARIABLE
                  END IF
C     DO PIVZ
                  IF(VALT.EQ.139) THEN
                      VARABL(I,4)=ALENS(80,VBSURF)
                      VARABL(I,5)=ALENS(80,VBSURF)
                      VARABL(I,13)=ALENS(80,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 139, PIVZ VARIABLE
                  END IF
C     DO DPART
                  IF(VALT.EQ.140) THEN
                      VARABL(I,4)=ALENS(89,VBSURF)
                      VARABL(I,5)=ALENS(89,VBSURF)
                      VARABL(I,13)=ALENS(89,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 140, DPART VARIABLE
                  END IF
C     DO CLPX
                  IF(VALT.EQ.141) THEN
                      VARABL(I,4)=ALENS(11,VBSURF)
                      VARABL(I,5)=ALENS(11,VBSURF)
                      VARABL(I,13)=ALENS(11,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 141, CLPX VARIABLE
                  END IF
C     DO CLPY
                  IF(VALT.EQ.142) THEN
                      VARABL(I,4)=ALENS(10,VBSURF)
                      VARABL(I,5)=ALENS(10,VBSURF)
                      VARABL(I,13)=ALENS(10,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 142, CLPY VARIABLE
                  END IF
C     DO GDX
                  IF(VALT.EQ.143) THEN
                      VARABL(I,4)=ALENS(90,VBSURF)
                      VARABL(I,5)=ALENS(90,VBSURF)
                      VARABL(I,13)=ALENS(90,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 143, GDX VARIABLE
                  END IF
C     DO GDY
                  IF(VALT.EQ.144) THEN
                      VARABL(I,4)=ALENS(91,VBSURF)
                      VARABL(I,5)=ALENS(91,VBSURF)
                      VARABL(I,13)=ALENS(91,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 144, GDY VARIABLE
                  END IF
C     DO GDZ
                  IF(VALT.EQ.145) THEN
                      VARABL(I,4)=ALENS(92,VBSURF)
                      VARABL(I,5)=ALENS(92,VBSURF)
                      VARABL(I,13)=ALENS(92,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 145, GDZ VARIABLE
                  END IF
C     DO GALPHA
                  IF(VALT.EQ.146) THEN
                      VARABL(I,4)=ALENS(93,VBSURF)
                      VARABL(I,5)=ALENS(93,VBSURF)
                      VARABL(I,13)=ALENS(93,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 146, GALPHA VARIABLE
                  END IF
C     DO GBETA
                  IF(VALT.EQ.147) THEN
                      VARABL(I,4)=ALENS(94,VBSURF)
                      VARABL(I,5)=ALENS(94,VBSURF)
                      VARABL(I,13)=ALENS(94,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 147, GBETA VARIABLE
                  END IF
C     DO GGAMMA
                  IF(VALT.EQ.148) THEN
                      VARABL(I,4)=ALENS(95,VBSURF)
                      VARABL(I,5)=ALENS(95,VBSURF)
                      VARABL(I,13)=ALENS(95,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 148, GGAMMA VARIABLE
                  END IF
C     DO GRS
                  IF(VALT.EQ.149) THEN
                      VARABL(I,4)=ALENS(98,VBSURF)
                      VARABL(I,5)=ALENS(98,VBSURF)
                      VARABL(I,13)=ALENS(98,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 149, GRS VARIABLE
                  END IF
C     DO MACVAR
                  IF(VALT.EQ.150) THEN
                      VARABL(I,4)=GPREG(VBSURF)
                      VARABL(I,5)=GPREG(VBSURF)
                      VARABL(I,13)=GPREG(VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 150, MACVAR VARIABLE
                  END IF
C     DO NSSXPOS
                  IF(VALT.EQ.4219) THEN
                      VARABL(I,4)=NSSALENS(34,VBSURF)
                      VARABL(I,5)=NSSALENS(34,VBSURF)
                      VARABL(I,13)=NSSALENS(34,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO NSSYPOS
                  IF(VALT.EQ.4220) THEN
                      VARABL(I,4)=NSSALENS(35,VBSURF)
                      VARABL(I,5)=NSSALENS(35,VBSURF)
                      VARABL(I,13)=NSSALENS(35,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO NSSZPOS
                  IF(VALT.EQ.4221) THEN
                      VARABL(I,4)=NSSALENS(36,VBSURF)
                      VARABL(I,5)=NSSALENS(36,VBSURF)
                      VARABL(I,13)=NSSALENS(36,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO NSSALPH
                  IF(VALT.EQ.4222) THEN
                      VARABL(I,4)=NSSALENS(40,VBSURF)
                      VARABL(I,5)=NSSALENS(40,VBSURF)
                      VARABL(I,13)=NSSALENS(40,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO NSSBETA
                  IF(VALT.EQ.4223) THEN
                      VARABL(I,4)=NSSALENS(41,VBSURF)
                      VARABL(I,5)=NSSALENS(41,VBSURF)
                      VARABL(I,13)=NSSALENS(41,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO NSSGAMM
                  IF(VALT.EQ.4224) THEN
                      VARABL(I,4)=NSSALENS(42,VBSURF)
                      VARABL(I,5)=NSSALENS(42,VBSURF)
                      VARABL(I,13)=NSSALENS(42,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO V1
                  IF(VALT.EQ.4225) THEN
                      VARABL(I,4)=NSSALENS(3,VBSURF)
                      VARABL(I,5)=NSSALENS(3,VBSURF)
                      VARABL(I,13)=NSSALENS(3,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO V2
                  IF(VALT.EQ.4226) THEN
                      VARABL(I,4)=NSSALENS(4,VBSURF)
                      VARABL(I,5)=NSSALENS(4,VBSURF)
                      VARABL(I,13)=NSSALENS(4,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO V3
                  IF(VALT.EQ.4227) THEN
                      VARABL(I,4)=NSSALENS(5,VBSURF)
                      VARABL(I,5)=NSSALENS(5,VBSURF)
                      VARABL(I,13)=NSSALENS(5,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO V4
                  IF(VALT.EQ.4228) THEN
                      VARABL(I,4)=NSSALENS(6,VBSURF)
                      VARABL(I,5)=NSSALENS(6,VBSURF)
                      VARABL(I,13)=NSSALENS(6,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO V5
                  IF(VALT.EQ.4229) THEN
                      VARABL(I,4)=NSSALENS(7,VBSURF)
                      VARABL(I,5)=NSSALENS(7,VBSURF)
                      VARABL(I,13)=NSSALENS(7,VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C     DO P001 TO P200
                  IF(VALT.GE.4230.AND.VALT.LE.4429) THEN
                      VARABL(I,4)=NSSALENS((VALT-4229),VBSURF)
                      VARABL(I,5)=NSSALENS((VALT-4229),VBSURF)
                      VARABL(I,13)=NSSALENS((VALT-4229),VBSURF)
                      VARABL(I,6)=0.0D0
                  END IF
C
C     DO CVTOR
                  IF(VALT.EQ.10.OR.VALT.EQ.9) THEN
                      VARABL(I,4)=ALENS(24,VBSURF)
                      VARABL(I,5)=ALENS(24,VBSURF)
                      VARABL(I,13)=ALENS(24,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 10, NOT A OR CVTOR VARIABLE
                  END IF
C
C     DO CCTOR
                  IF(VALT.EQ.11) THEN
                      VARABL(I,4)=ALENS(41,VBSURF)
                      VARABL(I,5)=ALENS(41,VBSURF)
                      VARABL(I,13)=ALENS(41,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 11, CCTOR VARIABLE
                  END IF
C
C     DO ADTOR
                  IF(VALT.EQ.12) THEN
                      VARABL(I,4)=ALENS(37,VBSURF)
                      VARABL(I,5)=ALENS(37,VBSURF)
                      VARABL(I,13)=ALENS(37,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 12, ADTOR VARIABLE
                  END IF
C
C     DO AETOR
                  IF(VALT.EQ.13) THEN
                      VARABL(I,4)=ALENS(38,VBSURF)
                      VARABL(I,5)=ALENS(38,VBSURF)
                      VARABL(I,13)=ALENS(38,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 13, AETOR VARIABLE
                  END IF
C
C     DO AFTOR
                  IF(VALT.EQ.14) THEN
                      VARABL(I,4)=ALENS(39,VBSURF)
                      VARABL(I,5)=ALENS(39,VBSURF)
                      VARABL(I,13)=ALENS(39,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 14, AFTOR VARIABLE
                  END IF
C
C     DO AGTOR
                  IF(VALT.EQ.15) THEN
                      VARABL(I,4)=ALENS(40,VBSURF)
                      VARABL(I,5)=ALENS(40,VBSURF)
                      VARABL(I,13)=ALENS(40,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 15, AGTOR VARIABLE
                  END IF
C
C     DO ALPHA
                  IF(VALT.EQ.16) THEN
                      VARABL(I,4)=ALENS(118,VBSURF)
                      VARABL(I,5)=ALENS(118,VBSURF)
                      VARABL(I,13)=ALENS(118,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 16, ALPHA VARIABLE
                  END IF
C
C     DO BETA
                  IF(VALT.EQ.17) THEN
                      VARABL(I,4)=ALENS(119,VBSURF)
                      VARABL(I,5)=ALENS(119,VBSURF)
                      VARABL(I,13)=ALENS(119,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 17, BETA VARIABLE
                  END IF
C
C     DO GAMMA
                  IF(VALT.EQ.18) THEN
                      VARABL(I,4)=ALENS(120,VBSURF)
                      VARABL(I,5)=ALENS(120,VBSURF)
                      VARABL(I,13)=ALENS(120,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 18, GAMMA VARIABLE
                  END IF
C
C     DO XD
                  IF(VALT.EQ.19) THEN
                      VARABL(I,4)=ALENS(114,VBSURF)
                      VARABL(I,5)=ALENS(114,VBSURF)
                      VARABL(I,13)=ALENS(114,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 19, XD VARIABLE
                  END IF
C
C     DO YD
                  IF(VALT.EQ.20) THEN
                      VARABL(I,4)=ALENS(115,VBSURF)
                      VARABL(I,5)=ALENS(115,VBSURF)
                      VARABL(I,13)=ALENS(115,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 20, YD VARIABLE
                  END IF
C
C     DO N1
                  IF(VALT.EQ.21) THEN
                      VARABL(I,4)=ALENS(46,VBSURF)
                      VARABL(I,5)=ALENS(46,VBSURF)
                      VARABL(I,13)=ALENS(46,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 21, N1 VARIABLE
                  END IF
C
C     DO N2
                  IF(VALT.EQ.22) THEN
                      VARABL(I,4)=ALENS(47,VBSURF)
                      VARABL(I,5)=ALENS(47,VBSURF)
                      VARABL(I,13)=ALENS(47,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 22, N2 VARIABLE
                  END IF
C
C     DO N3
                  IF(VALT.EQ.23) THEN
                      VARABL(I,4)=ALENS(48,VBSURF)
                      VARABL(I,5)=ALENS(48,VBSURF)
                      VARABL(I,13)=ALENS(48,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 23, N3 VARIABLE
                  END IF
C
C     DO N4
                  IF(VALT.EQ.24) THEN
                      VARABL(I,4)=ALENS(49,VBSURF)
                      VARABL(I,5)=ALENS(49,VBSURF)
                      VARABL(I,13)=ALENS(49,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 24, N4 VARIABLE
                  END IF
C
C     DO N5
                  IF(VALT.EQ.25) THEN
                      VARABL(I,4)=ALENS(50,VBSURF)
                      VARABL(I,5)=ALENS(50,VBSURF)
                      VARABL(I,13)=ALENS(50,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 25, N5 VARIABLE
                  END IF
C
C     NOW DO THE COEFFS
C
C     DO C1 TO C48
                  IF(VALT.GE.27.AND.VALT.LE.74) THEN
                      VARABL(I,4)=FTFL01((VALT-26),VBSURF)
                      VARABL(I,5)=FTFL01((VALT-26),VBSURF)
                      VARABL(I,13)=FTFL01((VALT-26),VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 27 TO 74, C1 - C48 VARIABLE
                  END IF
C     DO C49 TO C96
                  IF(VALT.GE.76.AND.VALT.LE.123) THEN
                      VARABL(I,4)=FTFL01((VALT-27),VBSURF)
                      VARABL(I,5)=FTFL01((VALT-27),VBSURF)
                      VARABL(I,13)=FTFL01((VALT-27),VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 76 TO 123, C49 - C96 VARIABLE
                  END IF
C
C     DO AC
                  IF(VALT.EQ.75) THEN
                      VARABL(I,4)=ALENS(43,VBSURF)
                      VARABL(I,5)=ALENS(43,VBSURF)
                      VARABL(I,13)=ALENS(43,VBSURF)
                      VARABL(I,6)=0.0D0
C     VALT NOT 75, AC VARIABLE
                  END IF
C
C     NOW BOUNDS CHECKER

                  IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                      VARABL(I,4)=VARABL(I,9)
                      WRITE(OUTLYNE,*)
     1                'WARNING: '
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
                      CALL SHOWIT(1)
                  END IF
                  IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                      VARABL(I,4)=VARABL(I,10)
                      WRITE(OUTLYNE,*)
     1                'WARNING: '
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                      CALL SHOWIT(1)
                  END IF
                  VARABL(I,5)=VARABL(I,4)
                  VARABL(I,13)=VARABL(I,4)
C
              ELSE
C     NOT CFG 1
                  VADD=INT(VARABL(I,14))
                  VBSURF=INT(VARABL(I,3))
                  VBCFG=INT(VARABL(I,2))
                  VALT=INT(VARABL(I,1))
                  IF(VBCFG.EQ.0.OR.CFGCNT(VBCFG).EQ.0) THEN
                      GO TO 200
                  END IF
C     USE AUXCFG ARRAY
C     FIRST WE SEE IF THE CONFIG CALLED FOR IS AN ACTIVE (I.E.
C     SOMETHING IN THAT CONFIG) CONFIGURATION.
C
C     THEN WE CHECK TO SEE IF THE VARIABLE IS IN THE
C     CONFIG SUBFILE DEFINITION FOR THAT CONFIGURATION
C
                  J=INT(VARABL(I,11))
C
C     J IS THE LOCATION IN THE AUXCFG ARRAYS WHERE THIS VARIABLE
C     IS LOCATED
C
                  IF(VALT.GE.27.AND.VALT.LE.74.OR.VALT.GE.76.AND.VALT.LE.123
     1            .OR.VALT.EQ.141) THEN
                      VARABL(I,4) =CFVAL(J,2)
                      VARABL(I,5) =CFVAL(J,2)
                      VARABL(I,13) =CFVAL(J,2)
                      VARABL(I,6) =0.0D0
C
C     BOUNDS CHECKER

                      IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                          VARABL(I,4)=VARABL(I,9)
                          WRITE(OUTLYNE,*)
     1                    'WARNING: '
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
                          CALL SHOWIT(1)
                      END IF
                      IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                          VARABL(I,4)=VARABL(I,10)
                          WRITE(OUTLYNE,*)
     1                    'WARNING: '
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                          CALL SHOWIT(1)
                      END IF
                      VARABL(I,5)=VARABL(I,4)
                      VARABL(I,13)=VARABL(I,4)
C
                  ELSE
                      IF(CFADD(J,1).EQ.1.AND.CFVAL(J,1).NE.0.0D0.OR.
     1                CFADD(J,1).EQ.9.AND.CFVAL(J,1).NE.0.0D0) THEN
                          VARABL(I,4) =1.0D0/CFVAL(J,1)
                          VARABL(I,5) =1.0D0/CFVAL(J,1)
                          VARABL(I,13) =1.0D0/CFVAL(J,1)
                          VARABL(I,6) =0.0D0
                      ELSE
                          VARABL(I,4) =CFVAL(J,1)
                          VARABL(I,5) =CFVAL(J,1)
                          VARABL(I,13) =CFVAL(J,1)
                          VARABL(I,6) =0.0D0
                      END IF
C
C     BOUNDS CHECKER

                      IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                          VARABL(I,4)=VARABL(I,9)
                          WRITE(OUTLYNE,*)
     1                    'WARNING: '
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
                          CALL SHOWIT(1)
                      END IF
                      IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                          VARABL(I,4)=VARABL(I,10)
                          WRITE(OUTLYNE,*)
     1                    'WARNING: '
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                          CALL SHOWIT(1)
                      END IF
                      VARABL(I,5)=VARABL(I,4)
                      VARABL(I,13)=VARABL(I,4)
C
                  END IF
                  VARABL(I,6)=0.0D0
              END IF
 200          CONTINUE
          END DO
C       ALL DONE
          RETURN
      END



      SUBROUTINE FIELDS
          IMPLICIT NONE
          CHARACTER AVAL3*3
          INTEGER I
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          AVAL3(1:3)=WC(2:4)
          CALL ATOII(AVAL3,I)
          IF(STI.EQ.1) THEN
              OUTLYNE='"'//WC//'" SETS A FIELD OF VIEW FOR OPTIMIZATION'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
              OUTLYNE='CURRENT SETTINGS FOR "'//WC//'" ARE:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,110) FIELDY(I)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,111) FIELDX(I)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,112) FIELDZ(I)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,113) INT(FIELDW(I))
              CALL SHOWIT(1)
 110          FORMAT(' Y-FOB = ',D23.15)
 111          FORMAT(' X-FOB = ',D23.15)
 112          FORMAT(' Z-POS = ',D23.15)
 113          FORMAT(' WAV#  = ',I2)
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              OUTLYNE='"'//WC//'" TAKES NO QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S5.EQ.1) THEN
              OUTLYNE='"'//WC//'" TAKES NO NUMERIC WORD #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE='"'//WC//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF4.EQ.1) THEN
              W4=SYSTEM1(11)
              DF4=0
          END IF
          IF(W4.NE.1.0D0.AND.W4.NE.2.0D0.AND.W4.NE.3.0D0.AND.W4.NE.4.0D0
     1    .AND.W4.NE.5.0D0.AND.W4.NE.6.0D0.AND.W4.NE.7.0D0.AND.W4.NE.
     2    8.0D0.AND.W4.NE.9.0D0.AND.W4.NE.10.0D0) THEN
              OUTLYNE='WAVELENGTH # MUST BE 1,2,3,4,5,6,7,8,9 OR 10'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) W1=0.0D0
          IF(DF2.EQ.1) W2=0.0D0
          IF(DF3.EQ.1) W3=0.0D0
          FIELDY(I)=W1
          FIELDX(I)=W2
          FIELDZ(I)=W3
          FIELDW(I)=DBLE(INT(W4))
          RETURN
      END
      SUBROUTINE MAKE_DEF_AUTO
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datsp1.inc'
          INTEGER IVAL,I,J,K,DFNRD,L,LL,LLL,LLLL
          REAL*8 JK_WW1,JK_WW2,VL,XPOS,YPOS,STEP,WT1,WT2,THETA
          REAL*8 TESTLENGTH,LAST_TESTLENGTH,YSTART_POS,XSTART_POS
          REAL*8 THETA_VAL1
          CHARACTER AV1*23,AV2*23,AVL*23,ACFG*3,AI4*4,AV0*4,AV4*23
          CHARACTER AXPOS*23,AYPOS*23
          COMMON/JK_ATD/AVL,VL
          REAL*8 RADIUS_VAL(1:20)
          COMMON/VALRAD/RADIUS_VAL
C
          IF(WC.NE.'DFGRID') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"'//TRIM(WC)//'" TAKES NO STRING OR QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"'//TRIM(WC)//'" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C     WC=MONO OR POLY
C
          IF(WC.EQ.'MONO') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"MONO" SETS AUTOMATIC MERIT FUNCTION BUILDING TO BE RESTRICTED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO THE CONTROL WAVELENGTH ONLY. IT IS THE DEFAULT'
                  CALL SHOWIT(1)
                  IF(CHROMATIC) WRITE(OUTLYNE,*)
     1            '"POLY" IS CURRENTLY IN EFFECT'
                  IF(.NOT.CHROMATIC) WRITE(OUTLYNE,*)
     1            '"MONO" IS CURRENTLY IN EFFECT'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"MONO" ONLY TAKES NO INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CHROMATIC=.FALSE.
              RETURN
          END IF
          IF(WC.EQ.'POLY') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"POLY" SETS AUTOMATIC MERIT FUNCTION BUILDING TO BE RESTRICTED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'TO POLYCHROMATIC MODE ONLY'
                  CALL SHOWIT(1)
                  IF(CHROMATIC) WRITE(OUTLYNE,*)
     1            '"POLY" IS CURRENTLY IN EFFECT'
                  IF(.NOT.CHROMATIC) WRITE(OUTLYNE,*)
     1            '"MONO" IS CURRENTLY IN EFFECT'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"POLY" ONLY TAKES NO INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CHROMATIC=.TRUE.
              RETURN
          END IF
C
C     WC=DFDEL
C
          IF(WC.EQ.'DFDEL') THEN
              IF(DFGRID.EQ.1) THEN
                  WRITE(OUTLYNE,*) 'CURRENT GRID SETTING IS "HEX"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '"DFDEL" REQUIRES THE GRID TYPE TO BE SET TO "RECT"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DFDEL" SETS DEFAULT AUTO MERIT FUNCTION RAY GRID SPACING'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DFDEL CURRENTLY SET TO ',DFDEL
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"DFDEL" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)'"DFDEL" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.GT.1.414213D0.OR.W1.LT.0.1054D0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DFDEL" MAX ALLOWED VALUE IS: 1.414213'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '"DFDEL" MIN ALLOWED VALUE IS: 0.1054'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DFDEL=W1
              RETURN
          END IF
C
C
C     WC=DFHEX
C
          IF(WC.EQ.'DFHEX') THEN
              IF(DFGRID.EQ.2) THEN
                  WRITE(OUTLYNE,*) 'CURRENT GRID SETTING IS "RECT"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '"DFHEX" REQUIRES THE GRID TYPE TO BE SET TO "HEX"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     SYNTAX CHECK
              IF(STI.EQ.1.OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
                  WRITE(OUTLYNE,*)      'NUMBER OF RINGS IS SET TO ',DFRIN
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NUMBER OF PIE SECTORS IS SET TO ',DFSEC
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"DFHEX" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.1) THEN
                  IF(W1.LT.1.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'MINIMUN NUMBER OF RADIAL RINGS IS 1'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W1.GT.20.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'MAXIMUM NUMBER OF RADIAL RINGS IS 20'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(S2.EQ.1) THEN
                  IF(W2.LT.4.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'MINIMUN NUMBER OF PIE SECTORS IS 4'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W2.GT.32.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'MAXIMUM NUMBER OF PIE SECTORS IS 32'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(DF1.EQ.0) DFRIN=INT(W1)
              IF(DF2.EQ.0) DFSEC=INT(W2)
              RETURN
          END IF
C
C
C     WC=DFGRID
C
          IF(WC.EQ.'DFGRID') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1.OR.SQ.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"DFGRID" SETS DEFAULT AUTO MERIT FUNCTION RAY GRID SHAPE'
                  CALL SHOWIT(1)
                  IF(DFGRID.EQ.1)
     1            WRITE(OUTLYNE,*)'DFGRID CURRENTLY SET TO "HEX"'
                  IF(DFGRID.EQ.2)
     1            WRITE(OUTLYNE,*)'DFGRID CURRENTLY SET TO "RECT"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"DFGRID" ONLY TAKES QUALIFIER WORD INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.NE.'HEX'.AND.WQ.NE.'RECT') THEN
                  WRITE(OUTLYNE,*)'"DFGRID" REQUIRES "HEX" OR "RECT"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'AS QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'HEX') DFGRID=1
              IF(WQ.EQ.'RECT') DFGRID=2
              RETURN
          END IF
C
C     WC=DFP
C
          IF(WC.EQ.'DFP') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DFP" SETS DEFAULT AUTO MERIT FUNCTION NUMBERS OF FIELD OF VIEW'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DFP CURRENTLY SET TO ',DFPNUMB
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"DFP" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)'"DFP" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.1.OR.INT(W1).GT.25) THEN
                  WRITE(OUTLYNE,*)
     1            '"DFP" MUST BE AN INTEGER FROM 1 TO 25'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DFPNUMB=INT(W1)
              RETURN
          END IF
C
C     WC=DFTYPE
C
          IF(WC.EQ.'DFTYPE') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"DFTYPE" SETS DEFAULT AUTO MERIT FUNCTION OPERAND TYPE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'DFTYPE CURRENTLY SET TO ',DFTYPENUMB
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S5.EQ.1.OR.S4.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"DFTYPE" TAKES NO NUMERIC WORDS #4 OR #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) THEN
                  WRITE(OUTLYNE,*)'"DFTYPE" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.1.OR.INT(W1).GT.2) THEN
                  WRITE(OUTLYNE,*)
     1            '"DFTYPE" MUST BE 1(TRANSVERSE ONLY) OR '
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '2(TRANSVERSE PLUS OPDS)'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1) W2=1.0D0
              IF(DF3.EQ.1) W3=1.0D0
              W2=DABS(W2)
              W3=DABS(W3)
              DFWT1=W2
              DFWT2=W3
              DFTYPENUMB=INT(W1)
              RETURN
          END IF
C
C     WC=FP
C
          IF(WC.EQ.'FP') THEN
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"FP" DEFINES FOBS FOR THE DEFAULT AUTO MERIT FUNCTION'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"FP" REQUIRES EXPLICIT NUMERIC WORDS 1, 2 AND 3'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.1.OR.INT(W1).GT.DFPNUMB) THEN
                  WRITE(OUTLYNE,*)
     1            'FIELD OF VIEW POSITION MUST BE FROM 1 TO ',DFPNUMB
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF5.EQ.1) W5=SYSTEM1(11)
              IF(INT(W5).LT.1.OR.INT(W5).GT.10) THEN
                  WRITE(OUTLYNE,*)
     1            'WAVELENGTH NUMBER MUST BE FROM 1 TO 10'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              W4=DABS(W4)
              IF(DF4.EQ.1) W4=1.0D0
              IF(DF5.EQ.1) W5=1.0D0
              DEFAULT_FOB(1,INT(W1))=W2
              DEFAULT_FOB(2,INT(W1))=W3
              DEFAULT_FOB(3,INT(W1))=W4
              DEFAULT_FOB(4,INT(W1))=W5
              RETURN
          END IF
C
C     WC=MAKEAUTO
C
          IF(WC.EQ.'MAKEAUTO') THEN
              IF(DFGRID.EQ.2) THEN
C       GRID SHAPE RECTANGULAR
C     SYNTAX CHECK
                  IF(STI.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                '"MAKEAUTO" CREATES DEFAULT AUTO MERIT FUNCTION'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                '"MAKEAUTO" ONLY TAKES NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(S1.EQ.0) W1=1.0D0
                  IF(INT(W1).LT.1.OR.INT(W1).GT.MAXCFG) THEN
                      WRITE(OUTLYNE,*)
     1                'CONFIGURATION NUMBER MUST BE FROM 1 TO ',MAXCFG
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  DF_CFG=INT(W1)
C
C     RECTANGULAR GRID OF RAYS
C     CALCULATE DFNRD
                  LAST_TESTLENGTH=0.0D0
                  TESTLENGTH=0.0D0
                  DO I=0,200
                      TESTLENGTH=
     1                DSQRT(((DFDEL/2.0D0)**2)+(((DFDEL/2.0D0)+
     2                ((DFDEL)*DBLE(I)))**2))
                      IF(LAST_TESTLENGTH.LE.1.0D0.AND.TESTLENGTH.GT.1.0D0) THEN
                          DFNRD=((I))*2
                          GO TO 10
                      ELSE
C     PROCEED WITH NEXT CYCLE
                          LAST_TESTLENGTH=TESTLENGTH
                      END IF
                  END DO
 10               CONTINUE
                  XSTART_POS=(DFDEL/2.0D0)+(DFDEL*DBLE((DFNRD/2)-1))
                  YSTART_POS=(DFDEL/2.0D0)+(DFDEL*DBLE((DFNRD/2)-1))
C     RESET FIELDS AND RAYS TO THE DEFAULT VALUES
                  SAVE_KDP(14)=SAVEINPT(14)
                  INPUT='FIELDS RESET'
                  CALL PROCES
                  REST_KDP(14)=RESTINPT(14)
                  SAVE_KDP(14)=SAVEINPT(14)
                  INPUT='RAYS RESET'
                  CALL PROCES
                  REST_KDP(14)=RESTINPT(14)
C     REDEFINE THE FIELD OF VIEW POSITIONS
                  DO I=1,DFPNUMB
                      CALL ITOAAA(I,AI4)
                      AV0=AI4
                      VL=DEFAULT_FOB(1,I)
                      CALL DTOACV
                      AV1=AVL
                      VL=DEFAULT_FOB(2,I)
                      CALL DTOACV
                      AV2=AVL
                      VL=DEFAULT_FOB(4,I)
                      CALL DTOACV
                      AV4=AVL
                      INPUT ='F'//AV0//','//AV1//','//AV2//','//AV4
                      CALL PROCES
                      INPUT ='F'//AV0//','//AV1//','//AV2//','//AV4
                  END DO
C     REDEFINE THE RAY POSITIONS
                  IF(CHROMATIC) THEN
                      LLLL=0
                      K=0
                      DO L=1,10
                          IF(L.EQ.1)  LL=31
                          IF(L.EQ.2)  LL=32
                          IF(L.EQ.3)  LL=33
                          IF(L.EQ.4)  LL=34
                          IF(L.EQ.5)  LL=35
                          IF(L.EQ.6)  LL=76
                          IF(L.EQ.7)  LL=77
                          IF(L.EQ.8)  LL=78
                          IF(L.EQ.9)  LL=79
                          IF(L.EQ.10) LL=80
                          IF(SYSTEM1(LL).NE.0.0D0) THEN
                              LLL=L
                              LLLL=LLLL+1
C     DO A WAVELENGTH
                              STEP=DFDEL
                              YPOS=-XSTART_POS
                              DO I=1,DFNRD
                                  XPOS=-YSTART_POS
                                  DO J=1,DFNRD
                                      K=K+1
                                      CALL ITOAAA(K,AI4)
                                      AV0=AI4
                                      VL=XPOS
                                      CALL DTOACV
                                      AXPOS=AVL
                                      VL=YPOS
                                      CALL DTOACV
                                      AYPOS=AVL
                                      CALL ITOAAA(LLL,AI4)
                                      AV4=AI4
                                      INPUT ='R'//AV0//','//AYPOS//','//AXPOS//','//AV4
                                      CALL PROCES
                                      XPOS=XPOS+STEP
                                  END DO
                                  YPOS=YPOS+STEP
                              END DO
                          END IF
                      END DO
                  ELSE
C       JUST DO THE CONTROL WAVELENGTH
                      LLLL=1
                      K=0
                      L=INT(SYSTEM1(11))
                      IF(L.EQ.1)  LL=31
                      IF(L.EQ.2)  LL=32
                      IF(L.EQ.3)  LL=33
                      IF(L.EQ.4)  LL=34
                      IF(L.EQ.5)  LL=35
                      IF(L.EQ.6)  LL=76
                      IF(L.EQ.7)  LL=77
                      IF(L.EQ.8)  LL=78
                      IF(L.EQ.9)  LL=79
                      IF(L.EQ.10) LL=80
                      LLL=L
C     DO A WAVELENGTH
                      STEP=DFDEL
                      YPOS=-XSTART_POS
                      DO I=1,DFNRD
                          XPOS=-YSTART_POS
                          DO J=1,DFNRD
                              K=K+1
                              CALL ITOAAA(K,AI4)
                              AV0=AI4
                              VL=XPOS
                              CALL DTOACV
                              AXPOS=AVL
                              VL=YPOS
                              CALL DTOACV
                              AYPOS=AVL
                              CALL ITOAAA(LLL,AI4)
                              AV4=AI4
                              INPUT ='R'//AV0//','//AYPOS//','//AXPOS//','//AV4
                              CALL PROCES
                              XPOS=XPOS+STEP
                          END DO
                          YPOS=YPOS+STEP
                      END DO
                  END IF
C     SET UP THE OPERANDS AND SKIP BLOCKED OPERANDS
                  WRITE(OUTLYNE,*) 'SETTING UP OPERAND DEFINITIONS...'
                  CALL SHOWIT(1)
C     UPDATE MERIT
                  SAVE_KDP(14)=SAVEINPT(14)
                  INPUT='UPDATE MERIT'
                  CALL PROCES
                  REST_KDP(14)=RESTINPT(14)
C     CFG AS NECESSARY
                  IF(DF_CFG.NE.1) THEN
                      SAVE_KDP(14)=SAVEINPT(14)
                      IVAL=DF_CFG
                      CALL NTOAN1(IVAL,ACFG)
                      INPUT='CFG '//ACFG
                      CALL PROCES
                      REST_KDP(14)=RESTINPT(14)
                  END IF
                  DO I=1,DFPNUMB
                      DO J=1,(DFNRD**2)*LLLL
                          JK_WW1=RAYY(J)
                          JK_WW2=RAYX(J)
                          WT1=DFWT1*DEFAULT_FOB(3,I)
                          WT2=DFWT2*DEFAULT_FOB(3,I)
C     TYPE ONE MERIT FUNCTION
                          IF(DFTYPENUMB.EQ.1) THEN
                              IF(ALENS(9,NEWREF).EQ.2.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
C     SQUARE OPERAND PATTERN
                                  IF(SYSTEM1(30).LE.2) THEN
C     FOCAL
                                      WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                  ELSE
C     AFOCAL
                                      WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                  END IF
                              ELSE
C     CIRCULAR OPERAND PATTERN
                                  IF(DSQRT(((JK_WW1)**2)+((JK_WW2)**2)).LE.1.0D0) THEN
C     ADD OPERAND
                                      IF(SYSTEM1(30).LE.2) THEN
C     FOCAL
                                          WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                                          CALL PROCES
                                          WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                                          CALL PROCES
                                      ELSE
C     AFOCAL
                                          WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                                          CALL PROCES
                                          WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                                          CALL PROCES
                                      END IF
                                  ELSE
C     DON'T ADD OPERAND
                                  END IF
                              END IF
                          END IF
C     TYPE TWO MERIT FUNCTION
                          IF(DFTYPENUMB.EQ.2) THEN
                              IF(ALENS(9,NEWREF).EQ.2.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
C     SQUARE OPERAND PATTERN
                                  IF(SYSTEM1(30).LE.2) THEN
C     FOCAL
                                      WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                                      CALL PROCES
                                  ELSE
C     AFOCAL
                                      WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                                      CALL PROCES
                                  END IF
                              ELSE
C     CIRCULAR OPERAND PATTERN
                                  IF(DSQRT(((JK_WW1)**2)+((JK_WW2)**2)).LE.1.0D0) THEN
C     ADD OPERAND
                                      IF(SYSTEM1(30).LE.2) THEN
C     FOCAL
                                          WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                                          CALL PROCES
                                          WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                                          CALL PROCES
                                          WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                                          CALL PROCES
                                      ELSE
C     AFOCAL
                                          WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                                          CALL PROCES
                                          WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                                          CALL PROCES
                                          WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                                          CALL PROCES
                                      END IF
                                  ELSE
C     DON'T ADD OPERAND
                                  END IF
                              END IF
                          END IF
                      END DO
                  END DO
C
C     EOS
                  SAVE_KDP(14)=SAVEINPT(14)
                  INPUT='EOS'
                  CALL PROCES
                  WRITE(OUTLYNE,*) 'MERIT FUNCTION DEFINITION COMPLETED'
                  CALL SHOWIT(1)
                  REST_KDP(14)=RESTINPT(14)
                  RETURN
              END IF
          END IF
C
          IF(DFGRID.EQ.1) THEN
C     HEXAPOLAR GRID
C     SYNTAX CHECK
              IF(STI.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"MAKEAUTO" CREATES DEFAULT AUTO MERIT FUNCTION'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"MAKEAUTO" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.EQ.0) W1=1.0D0
              IF(INT(W1).LT.1.OR.INT(W1).GT.MAXCFG) THEN
                  WRITE(OUTLYNE,*)
     1            'CONFIGURATION NUMBER MUST BE FROM 1 TO ',MAXCFG
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              DF_CFG=INT(W1)
C       COMPUTE AND SAVE RAY RING RADII
C
              DO I=1,DFRIN
                  RADIUS_VAL(I)=1.0D0/DSQRT(DBLE(I))
              END DO
C       RAYS START AT Y=YMAX, X=0 AND PROCEED CLOCKWISE AROUNT EACH
C       RING
C
              THETA_VAL1=2.0D0*PII/DBLE(DFSEC)
C     RESET FIELDS AND RAYS TO THE DEFAULT VALUES
              SAVE_KDP(14)=SAVEINPT(14)
              INPUT='FIELDS RESET'
              CALL PROCES
              REST_KDP(14)=RESTINPT(14)
              SAVE_KDP(14)=SAVEINPT(14)
              INPUT='RAYS RESET'
              CALL PROCES
              REST_KDP(14)=RESTINPT(14)
C     REDEFINE THE FIELD OF VIEW POSITIONS
              DO I=1,DFPNUMB
                  CALL ITOAAA(I,AI4)
                  AV0=AI4
                  VL=DEFAULT_FOB(1,I)
                  CALL DTOACV
                  AV1=AVL
                  VL=DEFAULT_FOB(2,I)
                  CALL DTOACV
                  AV2=AVL
                  VL=DEFAULT_FOB(4,I)
                  CALL DTOACV
                  AV4=AVL
                  INPUT ='F'//AV0//','//AV1//','//AV2//','//AV4
                  CALL PROCES
                  INPUT ='F'//AV0//','//AV1//','//AV2//','//AV4
              END DO
C     REDEFINE THE RAY POSITIONS
              IF(CHROMATIC) THEN
                  LLLL=0
                  K=0
                  DO L=1,10
                      IF(L.EQ.1)  LL=31
                      IF(L.EQ.2)  LL=32
                      IF(L.EQ.3)  LL=33
                      IF(L.EQ.4)  LL=34
                      IF(L.EQ.5)  LL=35
                      IF(L.EQ.6)  LL=76
                      IF(L.EQ.7)  LL=77
                      IF(L.EQ.8)  LL=78
                      IF(L.EQ.9)  LL=79
                      IF(L.EQ.10) LL=80
                      IF(SYSTEM1(LL).NE.0.0D0) THEN
                          LLL=L
                          LLLL=LLLL+1
C     DO A WAVELENGTH

C       ZERO ANGLE STARTS AT NOON AND PROCEEDS COUNTER-CLOCKWISE
                          DO I=1,DFRIN
                              DO J=1,DFSEC
C       COMPUTE XPOS AND YPOS
                                  THETA=DBLE(J-1)*THETA_VAL1
                                  XPOS=RADIUS_VAL(I)*DCOS(THETA)
                                  YPOS=RADIUS_VAL(I)*DSIN(THETA)
                                  K=K+1
                                  CALL ITOAAA(K,AI4)
                                  AV0=AI4
                                  VL=XPOS
                                  CALL DTOACV
                                  AXPOS=AVL
                                  VL=YPOS
                                  CALL DTOACV
                                  AYPOS=AVL
                                  CALL ITOAAA(LLL,AI4)
                                  AV4=AI4
                                  INPUT ='R'//AV0//','//AYPOS//','//AXPOS//','//AV4
                                  CALL PROCES
                              END DO
                          END DO
                      END IF
                  END DO
              ELSE
C       JUST DO THE CONTROL WAVELENGTH
                  LLLL=1
                  K=0
                  L=INT(SYSTEM1(11))
                  IF(L.EQ.1)  LL=31
                  IF(L.EQ.2)  LL=32
                  IF(L.EQ.3)  LL=33
                  IF(L.EQ.4)  LL=34
                  IF(L.EQ.5)  LL=35
                  IF(L.EQ.6)  LL=76
                  IF(L.EQ.7)  LL=77
                  IF(L.EQ.8)  LL=78
                  IF(L.EQ.9)  LL=79
                  IF(L.EQ.10) LL=80
                  LLL=L

C     DO A WAVELENGTH

C       ZERO ANGLE STARTS AT 3 pm, PROCEEDS COUNTER-CLOCKWISE
                  DO I=1,DFRIN
                      DO J=1,DFSEC
C       COMPUTE XPOS AND YPOS
                          THETA=DBLE(J-1)*THETA_VAL1
                          XPOS=RADIUS_VAL(I)*DCOS(THETA)
                          YPOS=RADIUS_VAL(I)*DSIN(THETA)
                          K=K+1
                          CALL ITOAAA(K,AI4)
                          AV0=AI4
                          VL=XPOS
                          CALL DTOACV
                          AXPOS=AVL
                          VL=YPOS
                          CALL DTOACV
                          AYPOS=AVL
                          CALL ITOAAA(LLL,AI4)
                          AV4=AI4
                          INPUT ='R'//AV0//','//AYPOS//','//AXPOS//','//AV4
                          CALL PROCES
                      END DO
                  END DO
              END IF

C     SET UP THE OPERANDS AND SKIP BLOCKED OPERANDS
              WRITE(OUTLYNE,*) 'SETTING UP OPERAND DEFINITIONS...'
              CALL SHOWIT(1)
C     UPDATE MERIT
              SAVE_KDP(14)=SAVEINPT(14)
              INPUT='UPDATE MERIT'
              CALL PROCES
              REST_KDP(14)=RESTINPT(14)
C     CFG AS NECESSARY
              IF(DF_CFG.NE.1) THEN
                  SAVE_KDP(14)=SAVEINPT(14)
                  IVAL=DF_CFG
                  CALL NTOAN1(IVAL,ACFG)
                  INPUT='CFG '//ACFG
                  CALL PROCES
                  REST_KDP(14)=RESTINPT(14)
              END IF
              DO I=1,DFPNUMB

                  DO J=1,(DFGRID*DFSEC)*LLLL
                      JK_WW1=RAYY(J)
                      JK_WW2=RAYX(J)
                      WT1=DFWT1*DEFAULT_FOB(3,I)
                      WT2=DFWT2*DEFAULT_FOB(3,I)
C     TYPE ONE MERIT FUNCTION
                      IF(DFTYPENUMB.EQ.1) THEN
                          IF(ALENS(9,NEWREF).EQ.2.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
C     SQUARE OPERAND PATTERN
                              IF(SYSTEM1(30).LE.2) THEN
C     FOCAL
                                  WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                                  CALL PROCES
                                  WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                                  CALL PROCES
                              ELSE
C     AFOCAL
                                  WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                                  CALL PROCES
                                  WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                                  CALL PROCES
                              END IF
                          ELSE
C     CIRCULAR OPERAND PATTERN
                              IF(DSQRT(((JK_WW1)**2)+((JK_WW2)**2)).LE.1.0D0) THEN
C     ADD OPERAND
                                  IF(SYSTEM1(30).LE.2) THEN
C     FOCAL
                                      WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                  ELSE
C     AFOCAL
                                      WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                  END IF
                              ELSE
C     DON'T ADD OPERAND
                              END IF
                          END IF
                      END IF
C     TYPE TWO MERIT FUNCTION
                      IF(DFTYPENUMB.EQ.2) THEN
                          IF(ALENS(9,NEWREF).EQ.2.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
C     SQUARE OPERAND PATTERN
                              IF(SYSTEM1(30).LE.2) THEN
C     FOCAL
                                  WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                                  CALL PROCES
                                  WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                                  CALL PROCES
                                  WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                                  CALL PROCES
                              ELSE
C     AFOCAL
                                  WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                                  CALL PROCES
                                  WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                                  CALL PROCES
                                  WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                                  CALL PROCES
                              END IF
                          ELSE
C     CIRCULAR OPERAND PATTERN
                              IF(DSQRT(((JK_WW1)**2)+((JK_WW2)**2)).LE.1.0D0) THEN
C     ADD OPERAND
                                  IF(SYSTEM1(30).LE.2) THEN
C     FOCAL
                                      WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                                      CALL PROCES
                                  ELSE
C     AFOCAL
                                      WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                                      CALL PROCES
                                      WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                                      CALL PROCES
                                  END IF
                              ELSE
C     DON'T ADD OPERAND
                              END IF
                          END IF
                      END IF
                  END DO
              END DO
C
C     EOS
              SAVE_KDP(14)=SAVEINPT(14)
              INPUT='EOS'
              CALL PROCES
              WRITE(OUTLYNE,*) 'MERIT FUNCTION DEFINITION COMPLETED'
              CALL SHOWIT(1)
              REST_KDP(14)=RESTINPT(14)
              RETURN
          END IF
          RETURN
      END
C SUB TOPER.FOR
      SUBROUTINE TOPER
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          INTEGER I
C
C       THIS IS SUBROUTINE TOPER. THIS IS THE SUBROUTINE WHICH
C       INITIALLY STARTS THE SETUP OF A NEW TOPER SUBFILE
C       THE CMD LEVEL IS DISABLED AND FLAG F53 IS SET TO 1.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"',WC(1:5),'" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F53=1
C
          DO I=1,MAXTOP
              ISTOP(I)=.FALSE.
              OPERND(MAXFOCRIT+I,1)=0.0D0
              OPERDESC(MAXFOCRIT+I)(1:80)=' '
              OPERND(MAXFOCRIT+I,1:20)=0.0D0
          END DO
          TOPCNT=0
          OPCNT=0
          FMTEXT=.FALSE.
C
          CORMOD=1
C       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
          CURFIG=1
C
C       THE MAXIMUM NUMBER OF OPERANDS ALLOWABLE
C       IS SET IN PROGRAM.FOR WITH THE VALUE MAXTOP WHICH IS
C       PASSED IN A COMMON IN THE INCLUDE FILE DATSUB.FOR
          RETURN
      END


C SUB FOCRIT.FOR
      SUBROUTINE FOCRIT
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          INTEGER I
C
C       THIS IS SUBROUTINE FOCRIT. THIS IS THE SUBROUTINE WHICH
C       INITIALLY STARTS THE SETUP OF A NEW FOCRIT SUBFILE
C       THE CMD LEVEL IS DISABLED AND FLAG F54 IS SET TO 1.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"',WC(1:6),'" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F54=1
C
          DO I=1,MAXFOCRIT
              ISCRIT(I)=.FALSE.
              OPERND(I,1)=0.0D0
              OPERDESC(I)(1:80)=' '
              OPERND(I,1:20)=0.0D0
          END DO
          FCCNT=0
          OPCNT=0
          FMTEXT=.FALSE.
C
C     TARGETS ARE AUTOMATICALLY SET TO THE ORIGINAL VALUES FOR FOCRITS
C
          CORMOD=1
C       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
          CURFIG=1
C
C       THE MAXIMUM NUMBER OF OPERANDS ALLOWABLE
C       IS SET IN PROGRAM.FOR WITH THE VALUE MAXFOCRIT WHICH IS
C       PASSED IN A COMMON IN THE INCLUDE FILE DATSUB.FOR
          RETURN
      END


C SUB MERIT.FOR
      SUBROUTINE MERIT
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
!        INTEGER I,J
C
C       THIS IS SUBROUTINE MERIT. THIS IS THE SUBROUTINE WHICH
C       INITIALLY STARTS THE SETUP OF A NEW MERIT SUBFILE
C       THE CMD LEVEL IS DISABLED AND FLAG F27 IS SET TO 1.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"',WC(1:5),'" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          F1=0
          F27=1
          OPERND(1:MAXOPT,1)=0.0D0
          OPERDESC(1:MAXOPT)(1:80)=' '
          OPERND(1:MAXOPT,1:20)=0.0D0
C
C       SET THE COUNTER TO THE TOP OF THE MERIT ARRAY STRUCTURE.
          OPCNT=0
          FCCNT=0
          TOPCNT=0
          FMTEXT=.FALSE.
C
          CORMOD=1
C       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
          CURFIG=1
C
C       THE MAXIMUM NUMBER OF OPERANDS ALLOWABLE
C       IS SET IN PROGRAM.FOR WITH THE VALUE MAXOP WHICH IS
C       PASSED IN A COMMON IN THE INCLUDE FILE DATSUB.FOR
          RETURN
      END
C SUB MDUMP.FOR
      SUBROUTINE MDUMP(IID,JJD,MDERIV)
C
          IMPLICIT NONE
C
          INTEGER I,J,IID,JJD
C
          REAL*8 MDERIV
C
          DIMENSION MDERIV(1:IID,1:JJD)
C
          REAL*8 VAL
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          CHARACTER AI1*3,AI2*3,AI3*3,AI4*3,AVAL1*13,AVAL2*13,AVAL3*13,
     1    AVAL4*13
C
C       THIS IS SUBROUTINE MDUMP. THIS IS THE SUBROUTINE WHICH
C       HANDLES AN "ITER MDUMP" COMMAND.
C
C     THE ARRAY MDERIV(I,J) CONTAINS THE PARTIAL DERIVATIVES OF
C     ALL DEFINED OPERANDS WITH RESPECT TO CHANGES IN ALL DEFINED
C     OPERANDS.
C
          IF(.NOT.DEREXT) THEN
              WRITE(OUTLYNE,*)
     1        '"ITER (MDUMP, MDP, MDUMPA AND MDPA)"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'REQUIRE A DERIVATIVE MATRIX TO EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'BEFORE THEY CAN FUNCTION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.NE.'MDUMPA'.AND.WQ.NE.'MDPA') THEN
C     ITER MDUMP OR ITER MDP
              DO I=1,VBCNT,4
                  CALL I3TOA3(I,AI1)
                  CALL I3TOA3(I+1,AI2)
                  CALL I3TOA3(I+2,AI3)
                  CALL I3TOA3(I+3,AI4)

                  IF(I.LE.VBCNT) OUTLYNE='         '//AI1
                  IF((I+1).LE.VBCNT)
     1            OUTLYNE='         '//AI1//'           '//AI2
                  IF((I+2).LE.VBCNT)
     1            OUTLYNE='         '//AI1//'           '//AI2//'           '//AI3
                  IF((I+3).LE.VBCNT)
     1            OUTLYNE='         '//AI1//'           '//AI2//'           '//AI3
     2            //'         '//AI4
                  CALL SHOWIT(0)

                  DO J=1,OPCNT
                      IF(I.LE.VBCNT) THEN
                          CALL I3TOA3(J,AI1)
                          VAL=MDERIV(J,I)*DINMUL*VARABL(I,8)
                          CALL DTOA(VAL,AVAL1)
                          OUTLYNE= AI1//' '//AVAL1
                      END IF
                      IF(I+1.LE.VBCNT) THEN
                          VAL=MDERIV(J,I+1)*DINMUL*VARABL(I+1,8)
                          CALL DTOA(VAL,AVAL2)
                          OUTLYNE=AI1//' '//AVAL1//' '//AVAL2
                      END IF
                      IF(I+2.LE.VBCNT) THEN
                          VAL=MDERIV(J,I+2)*DINMUL*VARABL(I+2,8)
                          CALL DTOA(VAL,AVAL3)
                          OUTLYNE= AI1//' '//AVAL1//' '//AVAL2//' '//AVAL3
                      END IF
                      IF(I+3.LE.VBCNT) THEN
                          VAL=MDERIV(J,I+3)*DINMUL*VARABL(I+3,8)
                          CALL DTOA(VAL,AVAL4)
                          OUTLYNE= AI1//' '//AVAL1//' '//AVAL2//' '//AVAL3//' '//AVAL4
                      END IF
                      CALL SHOWIT(0)
                  END DO
              END DO
C     FINISHED DUMPING THE DIFFERENCE MATRIX
          ELSE
C     WQ=MDUMPA OR MDPA
              DO J=1,VBCNT
                  DO I=1,OPCNT
                      WRITE(OUTLYNE,10)
                      CALL SHOWIT(0)
 10                   FORMAT('**************************************************')
                      WRITE(OUTLYNE,20) J,I
                      CALL SHOWIT(0)
 20                   FORMAT('FOR VARIABLE # = ',I3,' AND OPERAND # = ',I3)
                      WRITE(OUTLYNE,30) MDERIV(I,J)*DINMUL*VARABL(J,8)
                      CALL SHOWIT(0)
 30                   FORMAT('THE CURRENT DIFFERENCE MATRIX VALUE = ',D23.15)
                      WRITE(OUTLYNE,10)
                      CALL SHOWIT(0)
                  END DO
              END DO
          END IF
          RETURN
      END
C SUB FMT3.FOR
      SUBROUTINE FMT3
C       SILENT VERSION OF FMT2
C
          IMPLICIT NONE
C
          INTEGER I,TAGER,CFGCHK
C
          REAL*8 FMTFMT1,DELFMT
C
          LOGICAL NOP,ALLER,CFGER
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
          CFGER=.FALSE.
          TAGER=0
          ALLER=.TRUE.
C
          NOP=.FALSE.
          IF(SQ.EQ.1.AND.WQ.EQ.'NP') THEN
              NOP=.TRUE.
              SQ=0
              WQ='        '
          END IF
          IF(S1.EQ.1.AND.SQ.EQ.0) THEN
C     DO FMT FOR A SINGLE OPERAND
              ALLER=.FALSE.
              TAGER=INT(W1)
              CFGER=.FALSE.
          END IF
          IF(S1.EQ.1.AND.SQ.EQ.1) THEN
C     DO FMT FOR A SINGLE CFG
              ALLER=.FALSE.
              TAGER=INT(W1)
              CFGER=.TRUE.
          END IF
C
C
          IF(.NOT.CFGER.AND..NOT.ALLER) THEN
C     CHECK FOR VALID TAGER
              IF(TAGER.GT.OPCNT) THEN
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(CFGER) THEN
C     CHECK FOR TAGER GREATER THAN MAXCFG
              IF(TAGER.GT.MAXCFG) THEN
C     CFG NOT EXISTANT
                  CALL MACFAL
                  RETURN
              END IF
C     CHECK FOR OPERANDS ACTIVE IN CFG TAGER
              CFGCHK=0
              DO I=1,OPCNT
                  IF(INT(OPERND(I,16)).EQ.TAGER) CFGCHK=1
              END DO

              IF(CFGCHK.EQ.0) THEN
C     CFG OP DATA NOT EXISTANT
                  CALL MACFAL
                  RETURN
              END IF
C
          END IF
C     NOW PROCEED WITH THE OUTPUT
C
          IF(ALLER) OLDFMT=FMTFMT
          OPCALC_TYPE=3
          CALL OPCALC
          IF(F28.EQ.0) RETURN
          CALL OPLOAD
          IF(F28.EQ.0) RETURN
          FMTFLG=.TRUE.
C       PROCEED WITH ACTION FOR COMMAND
          IF(ALLER) THEN
              FMTFMT=0.0D0
              DO I=1,OPCNT
                  FMTFMT=FMTFMT+(OPERND(I,14)**2)
              END DO
          END IF
          IF(.NOT.ALLER.AND..NOT.CFGER) THEN
              FMTFMT1=0.0D0
              FMTFMT1=(OPERND(TAGER,14)**2)
          END IF
          IF(.NOT.ALLER.AND.CFGER) THEN
              FMTFMT1=0.0D0
              DO I=1,OPCNT
                  IF(INT(OPERND(I,16)).EQ.TAGER)
     1                      FMTFMT1=FMTFMT1+(OPERND(I,14)**2)
              END DO
          END IF
          IF(ALLER) DELFMT=FMTFMT-OLDFMT
          RETURN
C       ALL DONE
      END
C SUB ITERADJUST.FOR
      SUBROUTINE ITERADJUST(IID,JJD,MDERIV,JA)
C
          IMPLICIT NONE
C
          INTEGER I,J,IID,JJD,SUM1,IIID,JJJD,MAXCNT,ALLOERR
C
          INTEGER JA
C
          REAL*8 MDERIV,CHGS1,CHGS2,TESTMAX1,TESTMAX2,VALUE1,VALUE2,VALUE3
C
          DIMENSION MDERIV(1:IID,1:JJD),CHGS1(1:IID)
C
          !     REAL*8 VAL

          REAL*8 DERIV
          DIMENSION DERIV(:,:)
          ALLOCATABLE :: DERIV
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
C
C       THIS IS SUBROUTINE MDUMP. THIS IS THE SUBROUTINE WHICH
C       HANDLES AN "ITER ADJUST" COMMAND.
C
C     THE ARRAY MDERIV(I,J) CONTAINS THE PARTIAL DERIVATIVES OF
C     ALL DEFINED OPERANDS WITH RESPECT TO CHANGES IN ALL DEFINED
C     OPERANDS.
C
C
C     CREATE THE CHANGE MATRIX FROM THE DERIVATIVE MATRIX
C
          DO J=1,VBCNT
              DO I=1,OPCNT
                  MDERIV(I,J)=MDERIV(I,J)*DINMUL*VARABL(J,8)
              END DO
          END DO
C     CHANGE MATRIX COMPLETE
C
C     FOR EACH VARIABLE, FORM A SUM OF THE SQUARES OF THE CHANGE VALUES FOR ALL THE
C     OPERANDS
          CHGS1(1:IID)=0.0D0
          DO J=1,VBCNT
              DO I=1,OPCNT
                  CHGS1(J)=CHGS1(J)+DABS(MDERIV(I,J))
              END DO
          END DO
C     NOW IF THE CHGS1(J) ARE LESS THAN ONTOL, ADD THEM TO THE SUM ACROSS ALL VBS
          CHGS2=0.0D0
          SUM1=0
          DO J=1,VBCNT
              IF(CHGS1(J).GE.ONTOL) THEN
                  CHGS2=CHGS2+CHGS1(J)
                  SUM1=SUM1+1
              END IF
          END DO
C     NOW DIVIDE BY THE NUMBER OF ENTRIES SUMMED
          CHGS2=CHGS2/DBLE(SUM1)
          ADJUST_VAL1=CHGS2
C     DO THE ADJUST
          DO J=1,VBCNT
              IF(CHGS1(J).NE.0.0D0) THEN
                  VARABL(J,8)=VARABL(J,8)*CHGS2/CHGS1(J)
              ELSE
C       NO VARIABLE SCALING
              END IF
          END DO
C     RECOMPUTE DERIVATIVES USING NEW BARABL(J,8) VALUES
          DEALLOCATE(DERIV,STAT=ALLOERR)
          IIID=OPCNT
          JJJD=VBCNT
          IF(IIID.GE.JJJD)MAXCNT=IIID+1
          IF(IIID.LT.JJJD)MAXCNT=JJJD+1
          ALLOCATE(DERIV(1:MAXCNT,1:MAXCNT),STAT=ALLOERR)
          DERIV(1:MAXCNT,1:MAXCNT)=0.0D0
          CALL DERIVATIVES(MAXCNT,DERIV)
C
C
C     IF JA = 1, AUTOSCALE DINMUL
          IF(JA.EQ.1) THEN
              TESTMAX2=0.0D0
              DO I=1,OPCNT
C     VALUE1 = ABSOLUTE VALUE OF CURRENT VALUE OF OPERND I
                  VALUE1=DABS(OPERND(I,4))
                  TESTMAX1=0.0D0
                  DO J=1,VBCNT
C     VALUE2 = ABSOLUTE VALUE OF CURRENT CHANGE VALUE OF OPERAND I
C              WITH RESECT TO VARIABLE J
                      VALUE2=DABS(DERIV(I,J))
                      IF(VALUE1.NE.0.0D0) THEN
                          VALUE3=VALUE2/VALUE1
                      ELSE
                          VALUE3=0.0D0
                      END IF
                      IF(VALUE3.GT.LINTOL.AND.VALUE3.GT.TESTMAX1) TESTMAX1=VALUE3
                  END DO
                  IF(TESTMAX1.GT.TESTMAX2) TESTMAX2=TESTMAX1
              END DO
              TESTMAX2=TESTMAX2/LINTOL
              DINMUL=DINMUL/TESTMAX2
              IF(DINMUL.LT.1.0D-8) DINMUL=1.0D-8
C     RECOMPUTE DERIVATIVES USING NEW BARABL(J,8) VALUES
              DEALLOCATE(DERIV,STAT=ALLOERR)
              IIID=OPCNT
              JJJD=VBCNT
              IF(IIID.GE.JJJD)MAXCNT=IIID+1
              IF(IIID.LT.JJJD)MAXCNT=JJJD+1
              ALLOCATE(DERIV(1:MAXCNT,1:MAXCNT),STAT=ALLOERR)
              DERIV(1:MAXCNT,1:MAXCNT)=0.0D0
              CALL DERIVATIVES(MAXCNT,DERIV)
          END IF
          RETURN
      END
