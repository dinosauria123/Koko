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

C       FIRST SET OF OPTIMIZATION ROUTINES
C SUB TVARCLN.FOR
C
      SUBROUTINE TVARCLN
C
C     THIS REMOVES DUPLICATE ENTRIES, KEEPING ONLY THE LAST ENTRY
C
          IMPLICIT NONE
C
          INTEGER I,J,L,II,JJ
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
 30       CONTINUE
          DO I=1,TVBCNT
              II=I+MAXCMP
C     WE ARE AT THE ITH ENTRY IN THE VARAIBLES LIST
              DO J=1,TVBCNT
                  JJ=J+MAXCMP
                  IF(VARABL(II,1).EQ.VARABL(JJ,1).AND.
     1               VARABL(II,2).EQ.VARABL(JJ,2).AND.
     2               VARABL(II,3).EQ.VARABL(JJ,3).AND.
     3               VARNAM(II).EQ.VARNAM(JJ).AND.
     4               II.NE.JJ) THEN
                      GO TO 20
                  ELSE
C     KEEP GOING
                  END IF
              END DO
          END DO
          RETURN
 20       CONTINUE
C     REPLACE II ENTRY WITH JJ ENTRY
          VARABL(II,1:17)=VARABL(JJ,1:17)
C     REMOVE ENTRY JJ AND MOVE ENTRIES FOLLOWING JJ UP I
C     THEN REDUCE VBCNT BY 1
          IF(JJ.LT.TVBCNT) THEN
              DO L=JJ,TVBCNT+MAXCMP
                  VARABL(L,1:17)=VARABL(L+1,1:17)
              END DO
          ELSE
C     NOTHING TO COPY UP
          END IF
          TVBCNT=TVBCNT-1
          IF(II.LT.TVBCNT) GO TO 30
C
          RETURN
      END
C SUB VARCLN.FOR
      SUBROUTINE VARCLN
C
C     THIS REMOVES DUPLICATE ENTRIES, KEEPING ONLY THE LAST ENTRY
C
          IMPLICIT NONE
C
          INTEGER I,J,K,L,II,JJ
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
 30       CONTINUE
          DO I=1,VBCNT
              II=I
C     WE ARE AT THE ITH ENTRY IN THE VARAIBLES LIST
              DO J=1,VBCNT
                  JJ=J
                  IF(VARABL(II,1).EQ.VARABL(JJ,1).AND.
     1               VARABL(II,2).EQ.VARABL(JJ,2).AND.
     2               VARABL(II,3).EQ.VARABL(JJ,3).AND.
     3               II.NE.JJ.AND.VARNAM(II).EQ.VARNAM(JJ)) THEN
                      GO TO 20
                  ELSE
C     KEEP GOING
                  END IF
              END DO
          END DO
          DO I=1,VBCNT
          END DO
          RETURN
 20       CONTINUE
C     REPLACE II ENTRY WITH JJ ENTRY
          DO K=1,17
              VARABL(II,1:K)=VARABL(JJ,1:K)
          END DO
C     REMOVE ENTRY JJ AND MOVE ENTRIES FOLLOWING JJ UP I
C     THEN REDUCE VBCNT BY 1
          IF(JJ.LT.VBCNT) THEN
              DO L=JJ,VBCNT
                  DO K=1,17
                      VARABL(L,1:K)=VARABL(L+1,1:K)
                  END DO
              END DO
          ELSE
C     NOTHING TO COPY UP
          END IF
          VBCNT=VBCNT-1
          IF(II.LT.VBCNT) GO TO 30
          RETURN
      END

C SUB PFIND.FOR
      SUBROUTINE PFIND
C
          IMPLICIT NONE
C
          INTEGER FLAGGER,JKFAIL,I
C
          REAL*8 OFMT,NEWFMT,OLDPFAC,LASTPFAC
          REAL*8 BESTFMT,BESTPFAC
          COMMON/BESTCHOICE/BESTFMT,BESTPFAC
          LOGICAL ITERROR
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
C
C       THIS IS SUBROUTINE PFIND. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE "PFIND" COMMAND.
C
C
          IF(DF1.EQ.0.AND.INT(W1).LT.2) W1=2.0D0
          IF(DF2.EQ.0) PFDELM=W2
          IF(DF1.EQ.0) PFNUM=INT(W1)
C     PROCEED
          IF(OPCNT.EQ.0.OR.VBCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        'THERE ARE NO OPERANDS AND/OR VARIABLES DEFINED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '"PFIND" CAN NOT PROCEED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          OPCALC_TYPE=3
          CALL OPCALC
          IF(F28.EQ.0) RETURN
C     OPLOAD LOADS APPROPRIATE OPERND ARRAY AREAS FROM THE
C     GENERAL PURPOSE REGISTERS
          CALL OPLOAD
          IF(F28.EQ.0) RETURN
          IF(KILOPT) THEN
              F28=0
              CALL MACFAL
              RETURN
          END IF
C
          F32=1
          SAVE_KDP(1)=SAVEINPT(1)
          WC='RSV'
          WQ='        '
          WS=' '
          W1=0.0D0
          W2=0.0D0
          W3=0.0D0
          W4=0.0D0
          W5=0.0D0
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
          STI=0
          SQ=0
          SST=0
          ITERROR=.FALSE.
          CALL ITER(2,0,ITERROR)
          REST_KDP(1)=RESTINPT(1)
          IF(FMTFMT.LE.BESTFMT) THEN
              BESTFMT=FMTFMT
              BESTPFAC=PFAC
          END IF
          IF(KILOPT) THEN
              F28=0
              CALL MACFAL
              RETURN
          END IF
          IF(F32.EQ.0) RETURN
          F32=0
C
          OFMT=FMTFMT
          OLDPFAC=PFAC
          BESTFMT=FMTFMT
          BESTPFAC=PFAC
          PFAC=PFAC*PFDELM
          FLAGGER=1
C     LOOP STARTS
          JKFAIL=0
          IF(DABS(FMTFMT).LE.1.0D-50) THEN
              RETURN
          END IF
C
          DO I=1,PFNUM
              OFMT=FMTFMT
C
              F32=1
              SAVE_KDP(1)=SAVEINPT(1)
              WC='RSV'
              WQ='        '
              WS=' '
              W1=0.0D0
              W2=0.0D0
              W3=0.0D0
              W4=0.0D0
              W5=0.0D0
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
              STI=0
              SQ=0
              SST=0
              ITERROR=.FALSE.
              CALL ITER(2,0,ITERROR)
              REST_KDP(1)=RESTINPT(1)
              IF(FMTFMT.LE.BESTFMT) THEN
                  BESTFMT=FMTFMT
                  BESTPFAC=PFAC
              END IF
              IF(KILOPT) THEN
                  F28=0
                  CALL MACFAL
                  RETURN
              END IF
              IF(F32.EQ.0) RETURN
              F32=0
              IF(DABS(FMTFMT).LE.1.0D-50) THEN
                  RETURN
              END IF
C
              NEWFMT=FMTFMT
              IF(NEWFMT.GE.OFMT.AND.FLAGGER.EQ.2) GO TO 80
              IF(NEWFMT.LT.OFMT.AND.FLAGGER.EQ.1) GO TO 80
              IF(NEWFMT.GE.OFMT.AND.FLAGGER.EQ.1) GO TO 90
              IF(NEWFMT.LT.OFMT.AND.FLAGGER.EQ.2) GO TO 90
 80           IF(FLAGGER.EQ.2) THEN
                  LASTPFAC=PFAC
                  PFAC=OLDPFAC
                  JKFAIL=JKFAIL+1
                  IF(JKFAIL.GE.MAXFAIL) THEN
                      GO TO 100
                  END IF
              END IF
              FLAGGER=1
              OLDPFAC=PFAC
              LASTPFAC=PFAC
              PFAC=PFAC*PFDELM
              GO TO 20
 90           IF(FLAGGER.EQ.1) THEN
                  LASTPFAC=PFAC
                  PFAC=OLDPFAC
                  JKFAIL=JKFAIL+1
                  IF(JKFAIL.GE.MAXFAIL) THEN
                      GO TO 100
                  END IF
              END IF
              FLAGGER=2
              OLDPFAC=PFAC
              LASTPFAC=PFAC
              PFAC=PFAC/PFDELM
 20           CONTINUE
          END DO
 100      CONTINUE
          IF(JKFAIL.GE.MAXFAIL) THEN
              PFAC=OLDPFAC
              FMTFMT=OFMT
          END IF
          SAVE_KDP(1)=SAVEINPT(1)
          WC='RSV'
          WQ='        '
          WS=' '
          W1=0.0D0
          W2=0.0D0
          W3=0.0D0
          W4=0.0D0
          W5=0.0D0
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
          STI=0
          SQ=0
          SST=0
          ITERROR=.FALSE.
          CALL ITER(2,0,ITERROR)
          REST_KDP(1)=RESTINPT(1)
          IF(FMTFMT.LE.BESTFMT) THEN
              BESTFMT=FMTFMT
              BESTPFAC=PFAC
          END IF
          WRITE(OUTLYNE,300) BESTPFAC,BESTFMT
          CALL SHOWIT(0)
          RETURN
 300      FORMAT('   BEST PFAC = ',D18.10,' :     BEST FMT = ',D18.10)
          RETURN
      END
C SUB OPRD.FOR
      SUBROUTINE OPRD
C
C       THIS IS SUBROUTINE OPRD. THIS IS THE SUBROUTINE WHICH
C       HANDLES CMD LEVEL COMMAND "OPRD"
C
          IMPLICIT NONE
C
          CHARACTER CNAM*3,LWQ*8
C
          INTEGER CFGCHK,TAGER,I,ITAG,LW1,LS1,LSQ
C
          LOGICAL DESYES,ALLER,CFGER

          REAL*8 LDF1
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
          ALLER=.TRUE.
          CFGER=.FALSE.
          TAGER=0
C
C     SAVE NUMERIC WORD #1
          LW1=INT(W1)
          LDF1=DF1
          LS1=S1
          LSQ=SQ
          LWQ=WQ
C
C       RECALC VALUES AND RELOAD VALUES
C       INTO THE OPERND ARRAY
C     THIS PROCESS IS MODULARIZED
C     OPCALC CAUSES THE APPROPRIATE MACRO FUNCTIONS TO EXECUTE
C     IT DOES ABSOLUTLY NOTHING ELSE
          OPCALC_TYPE=3
          CALL OPCALC
          IF(F28.EQ.0) RETURN
C     OPLOAD LOADS APPROPRIATE OPERND ARRAY AREAS FROM THE
C     GENERAL PURPOSE REGISTERS
          CALL OPLOAD
          IF(F28.EQ.0) RETURN
          FMTFLG=.TRUE.
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"OPRD" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(LSQ.EQ.1.AND.LWQ.NE.'CFG') THEN
              WRITE(OUTLYNE,*)
     1        '"OPRD" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(LWQ.EQ.'CFG'.AND.LDF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"OPRD CFG" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       "OPRD" TAKES NO OTHER INPUT WORDS
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.
     1    S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"OPRD" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(LWQ.EQ.'CFG') THEN
              TAGER=LW1
              ALLER=.FALSE.
              CFGER=.TRUE.
          END IF
          IF(LWQ.NE.'CFG'.AND.LDF1.EQ.0) THEN
              TAGER=LW1
              ALLER=.FALSE.
              CFGER=.FALSE.
          END IF
          IF(LWQ.NE.'CFG'.AND.LDF1.EQ.1) THEN
              ALLER=.TRUE.
              CFGER=.FALSE.
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
C     PROCEED
              CFGCHK=0
              DO I=0,OPCNT
                  IF(INT(OPERND(I,16)).EQ.TAGER) CFGCHK=1
              END DO
C     CHECK FOR OPERANDS ACTIVE IN CFG TAGER

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
          END IF
C     NOW TAGER, ALLER AND CFGER ARE PROBERLY SET


          IF(LDF1.EQ.0.AND.LWQ.NE.'CFG') THEN
              ITAG=LW1
              IF(LWQ.NE.'CFG') THEN
                  IF(ITAG.LT.1.OR.ITAG.GT.OPCNT) THEN
                      WRITE(OUTLYNE,*)'OPERAND NUMBER BEYOND DEFINED BOUNDS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C       PROCEED WITH ACTION FOR COMMAND
          IF(OPCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'THE MERIT SUBFILE IS EMPTY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO OPERAND DATA VALUES EXISTS TO DISPLAY'
              CALL SHOWIT(1)
          ELSE
C     OPCNT NOT ZERO
              IF(LDF1.EQ.1.OR.LDF1.EQ.0.AND.LWQ.EQ.'CFG') THEN
C     NOT JUST A SINGLE OPERAND
C     PRINT HEADING
 20               FORMAT('OPERAND LIST')
 21               FORMAT(
     1            'ACTIVE IN CONFIGURATION # ',I2)

                  WRITE(OUTLYNE,20)
                  CALL SHOWIT(0)

                  IF(LWQ.EQ.'CFG') WRITE(OUTLYNE,21)
                  IF(LWQ.EQ.'CFG') CALL SHOWIT(0)
 201              FORMAT(
     1            'OP#',2X,'OP NAME',4X,'VALUE',9X,'TARGET',8X,'WEIGHT',7X,
     2            'FMT CONT',4X,'MODE')

                  WRITE(OUTLYNE,201)
                  CALL SHOWIT(0)
 202              FORMAT(I4,1X,A8,1X,G13.6,1X,G13.6,1X,G12.5,1X,G13.6,1X,A3)
 203              FORMAT(I4,1X,A8,1X,'***********',1X,
     1            'OPERAND NOT CURRENTLY CALCULABLE',1X,'**********',1X,A3)
                  DO I=1,OPCNT
C     OUTPUT THE OPRD DATA FOR OPERAND #OPCNT
                      IF(OPERND(I,13).EQ.1.0D0) CNAM ='COR'
                      IF(OPERND(I,13).EQ.0.0D0) CNAM ='BYP'
                      IF(OPERND(I,13).EQ.-2.0D0) CNAM ='GTE'
                      IF(OPERND(I,13).EQ.2.0D0) CNAM ='LTE'
                      IF(OPERND(I,13).EQ.10.0D0) CNAM='HLD'
                      IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                          IF(OPERND(I,19).EQ.0.0D0)
     1                    WRITE(OUTLYNE,202) I,OPNAM(I),OPERND(I,4),OPERND(I,2)
     1                    ,OPERND(I,7),(OPERND(I,14)**2),CNAM
                          IF(OPERND(I,19).EQ.1.0D0)
     1                    WRITE(OUTLYNE,203) I,OPNAM(I),CNAM
                          CALL SHOWIT(0)
                      END IF
                  END DO
                  DESYES=.FALSE.
                  DO I=1,OPCNT
                      IF(OPERDESC(I)(1:8).NE.'        ') THEN
                          DESYES=.TRUE.
                      END IF
                  END DO
                  IF(DESYES) WRITE(OUTLYNE,1703)
                  IF(DESYES) CALL SHOWIT(0)
                  IF(DESYES) WRITE(OUTLYNE,1702)
                  IF(DESYES) CALL SHOWIT(0)
                  IF(DESYES) WRITE(OUTLYNE,1703)
                  IF(DESYES) CALL SHOWIT(0)
 1702             FORMAT('CURRENT OPERAND DESCRIPTIONS')
 1703             FORMAT(1X)
                  DO I=1,OPCNT
                      IF(OPERDESC(I)(1:8).NE.'        ') THEN
                          WRITE(OUTLYNE,1701) OPNAM(I),OPERDESC(I)(1:69)
                          CALL SHOWIT(0)
                      END IF
                  END DO
                  RETURN
              ELSE
C     JUST A SINGLE OPERAND
C     JUST DO ITAG
C     PRINT HEADING
                  WRITE(OUTLYNE,20)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,201)
                  CALL SHOWIT(0)
                  IF(OPERND(ITAG,13).EQ.1.0D0) CNAM ='COR'
                  IF(OPERND(ITAG,13).EQ.0.0D0) CNAM ='BYP'
                  IF(OPERND(ITAG,13).EQ.-2.0D0) CNAM ='GTE'
                  IF(OPERND(ITAG,13).EQ.2.0D0) CNAM ='LTE'
                  IF(OPERND(ITAG,13).EQ.10.0D0) CNAM='HLD'
                  IF(OPERND(ITAG,19).EQ.0.0D0)
     1            WRITE(OUTLYNE,202) ITAG,OPNAM(ITAG),OPERND(ITAG,4),OPERND(ITAG,2)
     1            ,OPERND(ITAG,7),(OPERND(ITAG,14)**2),CNAM
                  IF(OPERND(ITAG,19).EQ.1.0D0)
     1            WRITE(OUTLYNE,203) ITAG,OPNAM(ITAG),CNAM
                  CALL SHOWIT(0)
                  IF(OPERDESC(ITAG)(1:8).NE.'        ')
     1            WRITE(OUTLYNE,1701) OPNAM(ITAG),OPERDESC(ITAG)(1:69)
                  IF(OPERDESC(ITAG)(1:8).NE.'        ')
     1            CALL SHOWIT(0)
 1701             FORMAT(A8,'::',A69)
              END IF
          END IF
          RETURN
      END
C SUB OPRCLN.FOR
      SUBROUTINE OPRCLN
C
          IMPLICIT NONE
C
          INTEGER I,J,L,II,JJ
C
!      LOGICAL OPGO
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
 30       CONTINUE
          DO I=1,OPCNT
              II=I
C     WE ARE AT THE ITH ENTRY IN THE OPERAND LIST
              DO J=1,OPCNT
                  JJ=J
                  IF(OPNAM(II).EQ.OPNAM(JJ).AND.II.NE.JJ.AND.
     1            OPERND(II,1).EQ.OPERND(JJ,1).AND.
     2            OPERND(II,8).EQ.OPERND(JJ,8).AND.
     3            OPERND(II,9).EQ.OPERND(JJ,9).AND.
     4            OPERND(II,10).EQ.OPERND(JJ,10).AND.
     5            OPERND(II,13).EQ.OPERND(JJ,13).AND.
     6            OPERND(II,16).EQ.OPERND(JJ,16)) THEN
                      GO TO 20
                  ELSE
C     KEEP GOING
                  END IF
              END DO
          END DO
          RETURN
 20       CONTINUE
C     REPLACE II ENTRY WITH JJ ENTRY
          OPNAM(II)=OPNAM(JJ)
          OPERND(II,1:20)=OPERND(JJ,1:20)
          OPERND(II,15)=0.0D0
C     LEAVE THE CORMOD SETTING ALONE UNLESS MODFLAG TRUE
          IF(MODEFLAG) OPERND(II,13)=OPERND(JJ,13)
          OPERND(II,14:20)=OPERND(JJ,14:20)
          OPERND(II,15)=0.0D0
C     REMOVE ENTRY JJ AND MOVE ENTRIES FOLLOWING JJ UP I
C     THEN REDUCE OPCNT BY 1
          IF(JJ.LT.OPCNT) THEN
              DO L=JJ,OPCNT
                  OPERND(L,1:20)=OPERND(L+1,1:20)
                  OPERND(L,15)=0.0D0
                  OPNAM(L)=OPNAM(L+1)
              END DO
          ELSE
C     NOTHING TO COPY UP
          END IF
          OPCNT=OPCNT-1
          IF(II.LT.OPCNT) GO TO 30
C
          RETURN
      END
C SUB OPCALC.FOR
      SUBROUTINE OPCALC
C
          IMPLICIT NONE
C
          INTEGER I,STRT,CNTER,KLI,PREDEFI,OF28,OF31
C
          COMMON/PREPRE/PREDEFI
C
          COMMON/OLDFS/OF28,OF31
C
          LOGICAL JK_OPTM,SENSERR,JK_TOLR,FUN
C
          INTEGER IAUTO
          COMMON/AUTOI/IAUTO
C
          COMMON/ERRSENS/SENSERR
C
          COMMON/NEFER/KLI
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsub.inc'
          OF28=F28
          OF31=F31
          SENSERR=.FALSE.
C
C
          FUNCL1=.FALSE.
          FUNCL2=.FALSE.
          FUNCL3=.FALSE.
          FUNCL4=.FALSE.
          FUNCL5=.FALSE.
          FUNCL6=.FALSE.
          FUNCL7=.FALSE.
          FUNCL8=.FALSE.
          FUNCL9=.FALSE.
          FUNCL10=.FALSE.
C     THESE NEXT 6 VARIABLES TRACK STATUS OF FIELD,RAY,CFG,SPDEXT
C     CPFNEXT AND ZERNIKE FIT
C     FOR PREDEFINED OPERANDS TO MINIMIZE EXCESSIVE RAY TRACING
          OLDF=-100
          OLDLAMM=-100
          OLDR=-100
          OLDCFG=-100
          OLDSPD=.FALSE.
          OLDGSPD=.FALSE.
          OLDCSPD=.FALSE.
          OLDCPFN=.FALSE.
          OLDZRNFT=.FALSE.
          SELECT CASE(OPCALC_TYPE)
            CASE(1)
C     FOCRITS
              STRT=1
              CNTER=MAXFOCRIT
            CASE(2)
C     TOLOPS
              STRT=MAXFOCRIT+1
              CNTER=MAXFOCRIT+MAXTOP
            CASE(3)
C     OPTIMIZATION OPERANDS
              CNTER=OPCNT
              STRT=1
          END SELECT
C
          DO I=STRT,CNTER
C
C      PREDEFINED OPERANDS
C
              IF(INT(OPERND(I,1)).EQ.0) THEN
C     EVALUATE A PREDEFINED OPERAND AND DROP TO THE END
C     FOR PREDEFINED OPERANDS, THE VALUE IS LOADED INTO
C     THE OPERAND ARRAY WITHOUT THE NEED TO USE "OPLOAD".
                  PREDEFI=I
                  JK_OPTM=.FALSE.
                  JK_TOLR=.FALSE.
                  IF(F28.EQ.1) JK_OPTM=.TRUE.
                  IF(F28.EQ.0) JK_OPTM=.FALSE.
                  IF(F31.EQ.1) JK_TOLR=.TRUE.
                  BAAD=0
C     AIMRFDIF AND AIMRYDIF ONLY HAVE AN EFFECT IF THE PROGRAM
C     IS CHANGED BACK TO CHIEF RAY AIMING WHICH WAS CIRCUMVENTED
C     ON 2/10/2006.
                  AIMRFDIF=.FALSE.
                  AIMRYDIF=.FALSE.
                  CALL CALCPRE
                  AIMRFDIF=.TRUE.
                  AIMRYDIF=.TRUE.
C     RETURN TO CFG 1
                  IAUTO=1
                  IF(F12.NE.1) CALL CFGCHG2
                  MSG=.TRUE.
                  IF(BAAD.EQ.0) OPERND(I,19)=0.0D0
                  IF(BAAD.EQ.1) THEN
                      OPERND(I,19)=1.0D0
C       COMMENTED OUT ON 12/6/2002
C     KILOPT=.TRUE.
                  END IF
                  IF(F31.EQ.0.AND.JK_TOLR) THEN
                      JK_TOLR=.FALSE.
C               FORCE TO CFG 1
                      IF(OPCALC_TYPE.EQ.3) CALL FRCCF1(0)
                      RETURN
                  END IF
                  GO TO 666
              END IF
C
              IF(INT(OPERND(I,1)).EQ.1.AND..NOT.FUNCL1) THEN
                  IF(.NOT.FUNEXT(1)) THEN
                      WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN01 DOES NOT EXIST'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'OPERANDS COULD NOT BE CALCULATED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      OPCNT=0
                      FMTEXT=.FALSE.
                      RETURN
                  END IF
C     RUN FUN01
                  SAVE_KDP(2)=SAVEINPT(2)
                  WC='FUN01'
                  WQ='        '
                  SQ=0
                  SN=1
                  SST=0
                  W1=OPERND(I,9)
                  W2=OPERND(I,10)
                  DF1=INT(OPERND(I,11))
                  DF2=INT(OPERND(I,12))
                  FMWQ='        '
                  FMSQ=0
                  FMNW(1)=W1
                  FMNW(2)=W2
                  FMNW(3)=0.0D0
                  FMNW(4)=0.0D0
                  FMNW(5)=0.0D0
                  FMDF(1)=DF1
                  FMDF(2)=DF2
                  FMDF(3)=1
                  FMDF(4)=1
                  FMDF(5)=1
C
                  NESFUN=.FALSE.
                  KLI=1
                  FUN=.TRUE.
                  F26=1
                  IF(F28.EQ.1.OR.F31.EQ.1) CALL FUNEXC
                  IF(F28.EQ.0.AND.OF28.EQ.1) CALL HALTER
                  IF(F28.EQ.0.AND.OF28.EQ.1) RETURN
                  IF(F31.EQ.0.AND.OF31.EQ.1) THEN
                      F31=1
                      OF31=1
C     SET ALL REGISTERS TO 0.0
                      SENSERR=.TRUE.
                      GPREG=0.0D0
                  END IF
                  FUNCL1=.TRUE.
                  F26=0
                  REST_KDP(2)=RESTINPT(2)
              END IF
              IF(INT(OPERND(I,1)).EQ.2.AND..NOT.FUNCL2) THEN
                  IF(.NOT.FUNEXT(2)) THEN
                      WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN02 DOES NOT EXIST'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'OPERANDS COULD NOT BE CALCULATED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      OPCNT=0
                      FMTEXT=.FALSE.
                      RETURN
                  END IF
C     RUN FUN02
                  SAVE_KDP(2)=SAVEINPT(2)
                  WC='FUN02'
                  WQ='        '
                  SQ=0
                  SN=1
                  SST=0
                  W1=OPERND(I,9)
                  W2=OPERND(I,10)
                  DF1=INT(OPERND(I,11))
                  DF2=INT(OPERND(I,12))
                  FMWQ='        '
                  FMSQ=0
                  FMNW(1)=W1
                  FMNW(2)=W2
                  FMNW(3)=0.0D0
                  FMNW(4)=0.0D0
                  FMNW(5)=0.0D0
                  FMDF(1)=DF1
                  FMDF(2)=DF2
                  FMDF(3)=1
                  FMDF(4)=1
                  FMDF(5)=1
                  NESFUN=.FALSE.
                  KLI=2
                  FUN=.TRUE.
                  F26=1
                  IF(F28.EQ.1.OR.F31.EQ.1) CALL FUNEXC
                  IF(F28.EQ.0.AND.OF28.EQ.1) CALL HALTER
                  IF(F28.EQ.0.AND.OF28.EQ.1) RETURN
                  IF(F31.EQ.0.AND.OF31.EQ.1) THEN
                      F31=1
                      OF31=1
C     SET ALL REGISTERS TO 0.0
                      SENSERR=.TRUE.
                      GPREG=0.0D0
                  END IF
                  FUNCL2=.TRUE.
                  F26=0
                  REST_KDP(2)=RESTINPT(2)
              END IF
              IF(INT(OPERND(I,1)).EQ.3.AND..NOT.FUNCL3) THEN
                  IF(.NOT.FUNEXT(3)) THEN
                      WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN03 DOES NOT EXIST'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'OPERANDS COULD NOT BE CALCULATED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      OPCNT=0
                      FMTEXT=.FALSE.
                      RETURN
                  END IF
C     RUN FUN03
                  SAVE_KDP(2)=SAVEINPT(2)
                  WC='FUN03'
                  WQ='        '
                  SQ=0
                  SN=1
                  SST=0
                  W1=OPERND(I,9)
                  W2=OPERND(I,10)
                  DF1=INT(OPERND(I,11))
                  DF2=INT(OPERND(I,12))
                  FMWQ='        '
                  FMSQ=0
                  FMNW(1)=W1
                  FMNW(2)=W2
                  FMNW(3)=0.0D0
                  FMNW(4)=0.0D0
                  FMNW(5)=0.0D0
                  FMDF(1)=DF1
                  FMDF(2)=DF2
                  FMDF(3)=1
                  FMDF(4)=1
                  FMDF(5)=1
                  NESFUN=.FALSE.
                  KLI=3
                  FUN=.TRUE.
                  F26=1
                  IF(F28.EQ.1.OR.F31.EQ.1) CALL FUNEXC
                  IF(F28.EQ.0.AND.OF28.EQ.1) CALL HALTER
                  IF(F28.EQ.0.AND.OF28.EQ.1) RETURN
                  IF(F31.EQ.0.AND.OF31.EQ.1) THEN
                      F31=1
                      OF31=1
C     SET ALL REGISTERS TO 0.0
                      SENSERR=.TRUE.
                      GPREG=0.0D0
                  END IF
                  FUNCL3=.TRUE.
                  F26=0
                  REST_KDP(2)=RESTINPT(2)
              END IF
              IF(INT(OPERND(I,1)).EQ.4.AND..NOT.FUNCL4) THEN
                  IF(.NOT.FUNEXT(4)) THEN
                      WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN04 DOES NOT EXIST'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'OPERANDS COULD NOT BE CALCULATED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      OPCNT=0
                      FMTEXT=.FALSE.
                      RETURN
                  END IF
C     RUN FUN04
                  SAVE_KDP(2)=SAVEINPT(2)
                  WC='FUN04'
                  WQ='        '
                  SQ=0
                  SN=1
                  SST=0
                  W1=OPERND(I,9)
                  W2=OPERND(I,10)
                  DF1=INT(OPERND(I,11))
                  DF2=INT(OPERND(I,12))
                  FMWQ='        '
                  FMSQ=0
                  FMNW(1)=W1
                  FMNW(2)=W2
                  FMNW(3)=0.0D0
                  FMNW(4)=0.0D0
                  FMNW(5)=0.0D0
                  FMDF(1)=DF1
                  FMDF(2)=DF2
                  FMDF(3)=1
                  FMDF(4)=1
                  FMDF(5)=1
                  NESFUN=.FALSE.
                  KLI=4
                  FUN=.TRUE.
                  F26=1
                  IF(F28.EQ.1.OR.F31.EQ.1) CALL FUNEXC
                  IF(F28.EQ.0.AND.OF28.EQ.1) CALL HALTER
                  IF(F28.EQ.0.AND.OF28.EQ.1) RETURN
                  IF(F31.EQ.0.AND.OF31.EQ.1) THEN
                      F31=1
                      OF31=1
C     SET ALL REGISTERS TO 0.0
                      SENSERR=.TRUE.
                      GPREG=0.0D0
                  END IF
                  FUNCL4=.TRUE.
                  F26=0
                  REST_KDP(2)=RESTINPT(2)
              END IF
              IF(INT(OPERND(I,1)).EQ.5.AND..NOT.FUNCL5) THEN
                  IF(.NOT.FUNEXT(5)) THEN
                      WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN05 DOES NOT EXIST'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'OPERANDS COULD NOT BE CALCULATED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      OPCNT=0
                      FMTEXT=.FALSE.
                      RETURN
                  END IF
C     RUN FUN05
                  SAVE_KDP(2)=SAVEINPT(2)
                  WC='FUN05'
                  WQ='        '
                  SQ=0
                  SN=1
                  SST=0
                  W1=OPERND(I,9)
                  W2=OPERND(I,10)
                  DF1=INT(OPERND(I,11))
                  DF2=INT(OPERND(I,12))
                  FMWQ='        '
                  FMSQ=0
                  FMNW(1)=W1
                  FMNW(2)=W2
                  FMNW(3)=0.0D0
                  FMNW(4)=0.0D0
                  FMNW(5)=0.0D0
                  FMDF(1)=DF1
                  FMDF(2)=DF2
                  FMDF(3)=1
                  FMDF(4)=1
                  FMDF(5)=1
                  NESFUN=.FALSE.
                  KLI=5
                  FUN=.TRUE.
                  F26=1
                  IF(F28.EQ.1.OR.F31.EQ.1) CALL FUNEXC
                  IF(F28.EQ.0.AND.OF28.EQ.1) CALL HALTER
                  IF(F28.EQ.0.AND.OF28.EQ.1) RETURN
                  IF(F31.EQ.0.AND.OF31.EQ.1) THEN
                      F31=1
                      OF31=1
C     SET ALL REGISTERS TO 0.0
                      SENSERR=.TRUE.
                      GPREG=0.0D0
                  END IF
                  FUNCL5=.TRUE.
                  F26=0
                  REST_KDP(2)=RESTINPT(2)
              END IF
              IF(INT(OPERND(I,1)).EQ.6.AND..NOT.FUNCL6) THEN
                  IF(.NOT.FUNEXT(6)) THEN
                      WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN06 DOES NOT EXIST'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'OPERANDS COULD NOT BE CALCULATED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      OPCNT=0
                      FMTEXT=.FALSE.
                      RETURN
                  END IF
C     RUN FUN06
                  SAVE_KDP(2)=SAVEINPT(2)
                  WC='FUN06'
                  WQ='        '
                  SQ=0
                  SN=1
                  SST=0
                  W1=OPERND(I,9)
                  W2=OPERND(I,10)
                  DF1=INT(OPERND(I,11))
                  DF2=INT(OPERND(I,12))
                  FMWQ='        '
                  FMSQ=0
                  FMNW(1)=W1
                  FMNW(2)=W2
                  FMNW(3)=0.0D0
                  FMNW(4)=0.0D0
                  FMNW(5)=0.0D0
                  FMDF(1)=DF1
                  FMDF(2)=DF2
                  FMDF(3)=1
                  FMDF(4)=1
                  FMDF(5)=1
                  NESFUN=.FALSE.
                  KLI=6
                  FUN=.TRUE.
                  F26=1
                  IF(F28.EQ.1.OR.F31.EQ.1) CALL FUNEXC
                  IF(F28.EQ.0.AND.OF28.EQ.1) CALL HALTER
                  IF(F28.EQ.0.AND.OF28.EQ.1) RETURN
                  IF(F31.EQ.0.AND.OF31.EQ.1) THEN
                      F31=1
                      OF31=1
C     SET ALL REGISTERS TO 0.0
                      SENSERR=.TRUE.
                      GPREG=0.0D0
                  END IF
                  FUNCL6=.TRUE.
                  F26=0
                  REST_KDP(2)=RESTINPT(2)
              END IF
              IF(INT(OPERND(I,1)).EQ.7.AND..NOT.FUNCL7) THEN
                  IF(.NOT.FUNEXT(7)) THEN
                      WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN07 DOES NOT EXIST'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'OPERANDS COULD NOT BE CALCULATED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      OPCNT=0
                      FMTEXT=.FALSE.
                      RETURN
                  END IF
C     RUN FUN07
                  SAVE_KDP(2)=SAVEINPT(2)
                  WC='FUN07'
                  WQ='        '
                  SQ=0
                  SN=1
                  SST=0
                  W1=OPERND(I,9)
                  W2=OPERND(I,10)
                  DF1=INT(OPERND(I,11))
                  DF2=INT(OPERND(I,12))
                  FMWQ='        '
                  FMSQ=0
                  FMNW(1)=W1
                  FMNW(2)=W2
                  FMNW(3)=0.0D0
                  FMNW(4)=0.0D0
                  FMNW(5)=0.0D0
                  FMDF(1)=DF1
                  FMDF(2)=DF2
                  FMDF(3)=1
                  FMDF(4)=1
                  FMDF(5)=1
                  NESFUN=.FALSE.
                  KLI=7
                  FUN=.TRUE.
                  F26=1
                  IF(F28.EQ.1.OR.F31.EQ.1) CALL FUNEXC
                  IF(F28.EQ.0.AND.OF28.EQ.1) CALL HALTER
                  IF(F28.EQ.0.AND.OF28.EQ.1) RETURN
                  IF(F31.EQ.0.AND.OF31.EQ.1) THEN
                      F31=1
                      OF31=1
C     SET ALL REGISTERS TO 0.0
                      SENSERR=.TRUE.
                      GPREG=0.0D0
                  END IF
                  FUNCL7=.TRUE.
                  F26=0
                  REST_KDP(2)=RESTINPT(2)
              END IF
              IF(INT(OPERND(I,1)).EQ.8.AND..NOT.FUNCL8) THEN
                  IF(.NOT.FUNEXT(8)) THEN
                      WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN08 DOES NOT EXIST'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'OPERANDS COULD NOT BE CALCULATED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      OPCNT=0
                      FMTEXT=.FALSE.
                      RETURN
                  END IF
C     RUN FUN08
                  SAVE_KDP(2)=SAVEINPT(2)
                  WC='FUN08'
                  WQ='        '
                  SQ=0
                  SN=1
                  SST=0
                  W1=OPERND(I,9)
                  W2=OPERND(I,10)
                  DF1=INT(OPERND(I,11))
                  DF2=INT(OPERND(I,12))
                  FMWQ='        '
                  FMSQ=0
                  FMNW(1)=W1
                  FMNW(2)=W2
                  FMNW(3)=0.0D0
                  FMNW(4)=0.0D0
                  FMNW(5)=0.0D0
                  FMDF(1)=DF1
                  FMDF(2)=DF2
                  FMDF(3)=1
                  FMDF(4)=1
                  FMDF(5)=1
                  NESFUN=.FALSE.
                  KLI=8
                  FUN=.TRUE.
                  F26=1
                  IF(F28.EQ.1.OR.F31.EQ.1) CALL FUNEXC
                  IF(F28.EQ.0.AND.OF28.EQ.1) CALL HALTER
                  IF(F28.EQ.0.AND.OF28.EQ.1) RETURN
                  IF(F31.EQ.0.AND.OF31.EQ.1) THEN
                      F31=1
                      OF31=1
C     SET ALL REGISTERS TO 0.0
                      SENSERR=.TRUE.
                      GPREG=0.0D0
                  END IF
                  FUNCL8=.TRUE.
                  F26=0
                  REST_KDP(2)=RESTINPT(2)
              END IF
              IF(INT(OPERND(I,1)).EQ.9.AND..NOT.FUNCL9) THEN
                  IF(.NOT.FUNEXT(9)) THEN
                      WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN09 DOES NOT EXIST'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'OPERANDS COULD NOT BE CALCULATED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      OPCNT=0
                      FMTEXT=.FALSE.
                      RETURN
                  END IF
C     RUN FUN09
                  SAVE_KDP(2)=SAVEINPT(2)
                  WC='FUN09'
                  WQ='        '
                  SQ=0
                  SN=1
                  SST=0
                  W1=OPERND(I,9)
                  W2=OPERND(I,10)
                  DF1=INT(OPERND(I,11))
                  DF2=INT(OPERND(I,12))
                  FMWQ='        '
                  FMSQ=0
                  FMNW(1)=W1
                  FMNW(2)=W2
                  FMNW(3)=0.0D0
                  FMNW(4)=0.0D0
                  FMNW(5)=0.0D0
                  FMDF(1)=DF1
                  FMDF(2)=DF2
                  FMDF(3)=1
                  FMDF(4)=1
                  FMDF(5)=1
                  NESFUN=.FALSE.
                  KLI=9
                  FUN=.TRUE.
                  F26=1
                  IF(F28.EQ.1.OR.F31.EQ.1) CALL FUNEXC
                  IF(F28.EQ.0.AND.OF28.EQ.1) CALL HALTER
                  IF(F28.EQ.0.AND.OF28.EQ.1) RETURN
                  IF(F31.EQ.0.AND.OF31.EQ.1) THEN
                      F31=1
                      OF31=1
C     SET ALL REGISTERS TO 0.0
                      SENSERR=.TRUE.
                      GPREG=0.0D0
                  END IF
                  FUNCL9=.TRUE.
                  F26=0
                  REST_KDP(2)=RESTINPT(2)
              END IF
              IF(INT(OPERND(I,1)).EQ.10.AND..NOT.FUNCL10) THEN
                  IF(.NOT.FUNEXT(10)) THEN
                      WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN10 DOES NOT EXIST'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'OPERANDS COULD NOT BE CALCULATED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      OPCNT=0
                      FMTEXT=.FALSE.
                      RETURN
                  END IF
C     RUN FUN10
                  SAVE_KDP(2)=SAVEINPT(2)
                  WC='FUN10'
                  WQ='        '
                  SQ=0
                  SN=1
                  SST=0
                  W1=OPERND(I,9)
                  W2=OPERND(I,10)
                  DF1=INT(OPERND(I,11))
                  DF2=INT(OPERND(I,12))
                  FMWQ='        '
                  FMSQ=0
                  FMNW(1)=W1
                  FMNW(2)=W2
                  FMNW(3)=0.0D0
                  FMNW(4)=0.0D0
                  FMNW(5)=0.0D0
                  FMDF(1)=DF1
                  FMDF(2)=DF2
                  FMDF(3)=1
                  FMDF(4)=1
                  FMDF(5)=1
                  NESFUN=.FALSE.
                  KLI=10
                  FUN=.TRUE.
                  F26=1
                  IF(F28.EQ.1.OR.F31.EQ.1) CALL FUNEXC
                  IF(F28.EQ.0.AND.OF28.EQ.1) CALL HALTER
                  IF(F28.EQ.0.AND.OF28.EQ.1) RETURN
                  IF(F31.EQ.0.AND.OF31.EQ.1) THEN
                      F31=1
                      OF31=1
C     SET ALL REGISTERS TO 0.0
                      SENSERR=.TRUE.
                      GPREG=0.0D0
                  END IF
                  FUNCL10=.TRUE.
                  F26=0
                  REST_KDP(2)=RESTINPT(2)
              END IF
 666          CONTINUE
          END DO
C               FORCE TO CFG 1
          IF(OPCALC_TYPE.EQ.3) CALL FRCCF1(0)
          RETURN
C       ALL DONE
      END
C SUB TVBA.FOR
      SUBROUTINE TVBA
C
          IMPLICIT NONE
C
          CHARACTER VNAME*6
C
          REAL*8 CURVALV
C
          INTEGER I,II
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
C       THIS IS SUBROUTINE TVBA. THIS IS THE SUBROUTINE WHICH DOES TVAR
C       OUTPUT COMMANDS AT THE CMD LEVEL AND FROM INSIDE TVARIABLE AND
C       UPDATE TVARIABLE
          IF(SQ.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"TVB" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"TVB" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(TVBCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'NO TOLERANCE VARIABLE DATA EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO-ACTION TAKEN'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(DF1.EQ.0) THEN
C     CHECK FOR A VALID NUMERIC WORD #1 INPUT VALUE
C     AND PERFORM THE OPERATION
              IF(W1.LE.0.0D0.OR.W1.GT.TVBCNT) THEN
                  WRITE(OUTLYNE,*)'TOLERANCE VARIABLE NUMBER BEYOND DEFINED BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     HERE DO THE OUTPUT FOR VARIABLE NUMBER INT(W1)
C     MAKE TVAR DATA CURRENT
C
C     ESTABLISH NAMES
          IF(DF1.EQ.0) THEN
C
              I=INT(W1)+MAXCMP
              IF(VARABL(I,1).EQ.1.0D0)   VNAME='RD    '
              IF(VARABL(I,1).EQ.2.0D0)   VNAME='CV    '
              IF(VARABL(I,1).EQ.3.0D0)   VNAME='TH    '
              IF(VARABL(I,1).EQ.4.0D0)   VNAME='CC    '
              IF(VARABL(I,1).EQ.5.0D0)   VNAME='AD    '
              IF(VARABL(I,1).EQ.6.0D0)   VNAME='AE    '
              IF(VARABL(I,1).EQ.7.0D0)   VNAME='AF    '
              IF(VARABL(I,1).EQ.8.0D0)   VNAME='AG    '
              IF(VARABL(I,1).EQ.9.0D0)   VNAME='RDTOR '
              IF(VARABL(I,1).EQ.10.0D0)  VNAME='CVTOR '
              IF(VARABL(I,1).EQ.11.0D0)  VNAME='CCTOR '
              IF(VARABL(I,1).EQ.12.0D0)  VNAME='ADTOR '
              IF(VARABL(I,1).EQ.13.0D0)  VNAME='AETOR '
              IF(VARABL(I,1).EQ.14.0D0)  VNAME='AFTOR '
              IF(VARABL(I,1).EQ.15.0D0)  VNAME='AGTOR '
              IF(VARABL(I,1).EQ.16.0D0)  VNAME='ALPHA '
              IF(VARABL(I,1).EQ.17.0D0)  VNAME='BETA  '
              IF(VARABL(I,1).EQ.18.0D0)  VNAME='GAMMA '
              IF(VARABL(I,1).EQ.19.0D0)  VNAME='XD    '
              IF(VARABL(I,1).EQ.20.0D0)  VNAME='YD    '
              IF(VARABL(I,1).EQ.21.0D0)  VNAME='N1    '
              IF(VARABL(I,1).EQ.22.0D0)  VNAME='N2    '
              IF(VARABL(I,1).EQ.23.0D0)  VNAME='N3    '
              IF(VARABL(I,1).EQ.24.0D0)  VNAME='N4    '
              IF(VARABL(I,1).EQ.25.0D0)  VNAME='N5    '
              IF(VARABL(I,1).EQ.27.0D0)  VNAME='C1    '
              IF(VARABL(I,1).EQ.28.0D0)  VNAME='C2    '
              IF(VARABL(I,1).EQ.29.0D0)  VNAME='C3    '
              IF(VARABL(I,1).EQ.30.0D0)  VNAME='C4    '
              IF(VARABL(I,1).EQ.31.0D0)  VNAME='C5    '
              IF(VARABL(I,1).EQ.32.0D0)  VNAME='C6    '
              IF(VARABL(I,1).EQ.33.0D0)  VNAME='C7    '
              IF(VARABL(I,1).EQ.34.0D0)  VNAME='C8    '
              IF(VARABL(I,1).EQ.35.0D0)  VNAME='C9    '
              IF(VARABL(I,1).EQ.36.0D0)  VNAME='C10   '
              IF(VARABL(I,1).EQ.37.0D0)  VNAME='C11   '
              IF(VARABL(I,1).EQ.38.0D0)  VNAME='C12   '
              IF(VARABL(I,1).EQ.39.0D0)  VNAME='C13   '
              IF(VARABL(I,1).EQ.40.0D0)  VNAME='C14   '
              IF(VARABL(I,1).EQ.41.0D0)  VNAME='C15   '
              IF(VARABL(I,1).EQ.42.0D0)  VNAME='C16   '
              IF(VARABL(I,1).EQ.43.0D0)  VNAME='C17   '
              IF(VARABL(I,1).EQ.44.0D0)  VNAME='C18   '
              IF(VARABL(I,1).EQ.45.0D0)  VNAME='C19   '
              IF(VARABL(I,1).EQ.46.0D0)  VNAME='C20   '
              IF(VARABL(I,1).EQ.47.0D0)  VNAME='C21   '
              IF(VARABL(I,1).EQ.48.0D0)  VNAME='C22   '
              IF(VARABL(I,1).EQ.49.0D0)  VNAME='C23   '
              IF(VARABL(I,1).EQ.50.0D0)  VNAME='C24   '
              IF(VARABL(I,1).EQ.51.0D0)  VNAME='C25   '
              IF(VARABL(I,1).EQ.52.0D0)  VNAME='C26   '
              IF(VARABL(I,1).EQ.53.0D0)  VNAME='C27   '
              IF(VARABL(I,1).EQ.54.0D0)  VNAME='C28   '
              IF(VARABL(I,1).EQ.55.0D0)  VNAME='C29   '
              IF(VARABL(I,1).EQ.56.0D0)  VNAME='C30   '
              IF(VARABL(I,1).EQ.57.0D0)  VNAME='C31   '
              IF(VARABL(I,1).EQ.58.0D0)  VNAME='C32   '
              IF(VARABL(I,1).EQ.59.0D0)  VNAME='C33   '
              IF(VARABL(I,1).EQ.60.0D0)  VNAME='C34   '
              IF(VARABL(I,1).EQ.61.0D0)  VNAME='C35   '
              IF(VARABL(I,1).EQ.62.0D0)  VNAME='C36   '
              IF(VARABL(I,1).EQ.63.0D0)  VNAME='C37   '
              IF(VARABL(I,1).EQ.64.0D0)  VNAME='C38   '
              IF(VARABL(I,1).EQ.65.0D0)  VNAME='C39   '
              IF(VARABL(I,1).EQ.66.0D0)  VNAME='C40   '
              IF(VARABL(I,1).EQ.67.0D0)  VNAME='C41   '
              IF(VARABL(I,1).EQ.68.0D0)  VNAME='C42   '
              IF(VARABL(I,1).EQ.69.0D0)  VNAME='C43   '
              IF(VARABL(I,1).EQ.70.0D0)  VNAME='C44   '
              IF(VARABL(I,1).EQ.71.0D0)  VNAME='C45   '
              IF(VARABL(I,1).EQ.72.0D0)  VNAME='C46   '
              IF(VARABL(I,1).EQ.73.0D0)  VNAME='C47   '
              IF(VARABL(I,1).EQ.74.0D0)  VNAME='C48   '
              IF(VARABL(I,1).EQ.75.0D0)  VNAME='AC    '
              IF(VARABL(I,1).EQ.76.0D0)  VNAME='C49   '
              IF(VARABL(I,1).EQ.77.0D0)  VNAME='C50   '
              IF(VARABL(I,1).EQ.78.0D0)  VNAME='C51   '
              IF(VARABL(I,1).EQ.79.0D0)  VNAME='C52   '
              IF(VARABL(I,1).EQ.80.0D0)  VNAME='C53   '
              IF(VARABL(I,1).EQ.81.0D0)  VNAME='C54   '
              IF(VARABL(I,1).EQ.82.0D0)  VNAME='C55   '
              IF(VARABL(I,1).EQ.83.0D0)  VNAME='C56   '
              IF(VARABL(I,1).EQ.84.0D0)  VNAME='C57   '
              IF(VARABL(I,1).EQ.85.0D0)  VNAME='C58   '
              IF(VARABL(I,1).EQ.86.0D0)  VNAME='C59   '
              IF(VARABL(I,1).EQ.87.0D0)  VNAME='C60   '
              IF(VARABL(I,1).EQ.88.0D0)  VNAME='C61   '
              IF(VARABL(I,1).EQ.89.0D0)  VNAME='C62   '
              IF(VARABL(I,1).EQ.90.0D0)  VNAME='C63   '
              IF(VARABL(I,1).EQ.91.0D0)  VNAME='C64   '
              IF(VARABL(I,1).EQ.92.0D0)  VNAME='C65   '
              IF(VARABL(I,1).EQ.93.0D0)  VNAME='C66   '
              IF(VARABL(I,1).EQ.94.0D0)  VNAME='C67   '
              IF(VARABL(I,1).EQ.95.0D0)  VNAME='C68   '
              IF(VARABL(I,1).EQ.96.0D0)  VNAME='C69   '
              IF(VARABL(I,1).EQ.97.0D0)  VNAME='C70   '
              IF(VARABL(I,1).EQ.98.0D0)  VNAME='C71   '
              IF(VARABL(I,1).EQ.99.0D0)  VNAME='C72   '
              IF(VARABL(I,1).EQ.100.0D0) VNAME='C73   '
              IF(VARABL(I,1).EQ.101.0D0) VNAME='C74   '
              IF(VARABL(I,1).EQ.102.0D0) VNAME='C75   '
              IF(VARABL(I,1).EQ.103.0D0) VNAME='C76   '
              IF(VARABL(I,1).EQ.104.0D0) VNAME='C77   '
              IF(VARABL(I,1).EQ.105.0D0) VNAME='C78   '
              IF(VARABL(I,1).EQ.106.0D0) VNAME='C79   '
              IF(VARABL(I,1).EQ.107.0D0) VNAME='C80   '
              IF(VARABL(I,1).EQ.108.0D0) VNAME='C81   '
              IF(VARABL(I,1).EQ.109.0D0) VNAME='C82   '
              IF(VARABL(I,1).EQ.110.0D0) VNAME='C83   '
              IF(VARABL(I,1).EQ.111.0D0) VNAME='C84   '
              IF(VARABL(I,1).EQ.112.0D0) VNAME='C85   '
              IF(VARABL(I,1).EQ.113.0D0) VNAME='C86   '
              IF(VARABL(I,1).EQ.114.0D0) VNAME='C87   '
              IF(VARABL(I,1).EQ.115.0D0) VNAME='C88   '
              IF(VARABL(I,1).EQ.116.0D0) VNAME='C89   '
              IF(VARABL(I,1).EQ.117.0D0) VNAME='C90   '
              IF(VARABL(I,1).EQ.118.0D0) VNAME='C91   '
              IF(VARABL(I,1).EQ.119.0D0) VNAME='C92   '
              IF(VARABL(I,1).EQ.120.0D0) VNAME='C93   '
              IF(VARABL(I,1).EQ.121.0D0) VNAME='C94   '
              IF(VARABL(I,1).EQ.122.0D0) VNAME='C95   '
              IF(VARABL(I,1).EQ.123.0D0) VNAME='C96   '
              IF(VARABL(I,1).EQ.124.0D0) VNAME='N6    '
              IF(VARABL(I,1).EQ.125.0D0) VNAME='N7    '
              IF(VARABL(I,1).EQ.126.0D0) VNAME='N8    '
              IF(VARABL(I,1).EQ.127.0D0) VNAME='N9    '
              IF(VARABL(I,1).EQ.128.0D0) VNAME='N10   '
              IF(VARABL(I,1).EQ.129.0D0) VNAME='AH    '
              IF(VARABL(I,1).EQ.130.0D0) VNAME='AI    '
              IF(VARABL(I,1).EQ.131.0D0) VNAME='AJ    '
              IF(VARABL(I,1).EQ.132.0D0) VNAME='AK    '
              IF(VARABL(I,1).EQ.133.0D0) VNAME='AL    '
              IF(VARABL(I,1).EQ.134.0D0) VNAME='RD_FR '
              IF(VARABL(I,1).EQ.135.0D0) VNAME='CV_FR '
              IF(VARABL(I,1).EQ.136.0D0) VNAME='RDTFR '
              IF(VARABL(I,1).EQ.137.0D0) VNAME='CVTFR '
              IF(VARABL(I,1).EQ.138.0D0) VNAME='ZD    '
              IF(VARABL(I,1).EQ.139.0D0) VNAME='INDEX '
              IF(VARABL(I,1).EQ.140.0D0) VNAME='VNUM  '
              IF(VARABL(I,1).EQ.141.0D0) VNAME='PIVX  '
              IF(VARABL(I,1).EQ.142.0D0) VNAME='PIVY  '
              IF(VARABL(I,1).EQ.143.0D0) VNAME='PIVZ  '
              IF(VARABL(I,1).EQ.144.0D0) VNAME='DPART '
              IF(VARABL(I,1).EQ.145.0D0) VNAME='CLPX  '
              IF(VARABL(I,1).EQ.146.0D0) VNAME='CLPY  '
              IF(VARABL(I,1).EQ.147.0D0) VNAME='GDX   '
              IF(VARABL(I,1).EQ.148.0D0) VNAME='GDY   '
              IF(VARABL(I,1).EQ.149.0D0) VNAME='GDZ   '
              IF(VARABL(I,1).EQ.150.0D0) VNAME='GALPHA'
              IF(VARABL(I,1).EQ.151.0D0) VNAME='GBETA '
              IF(VARABL(I,1).EQ.152.0D0) VNAME='GGAMMA'
              IF(VARABL(I,1).EQ.153.0D0) VNAME='GRS   '
              IF(VARABL(I,1).EQ.154.0D0) VNAME='DISPX '
              IF(VARABL(I,1).EQ.155.0D0) VNAME='DISPY '
              IF(VARABL(I,1).EQ.156.0D0) VNAME='DISPZ '
              IF(VARABL(I,1).EQ.157.0D0) VNAME='STILTA'
              IF(VARABL(I,1).EQ.158.0D0) VNAME='STILTB'
              IF(VARABL(I,1).EQ.159.0D0) VNAME='STILTG'
              IF(VARABL(I,1).EQ.160.0D0) VNAME='BTILTA'
              IF(VARABL(I,1).EQ.161.0D0) VNAME='BTILTB'
              IF(VARABL(I,1).EQ.162.0D0) VNAME='BTILTG'
              IF(VARABL(I,1).EQ.163.0D0) VNAME='ROLLX '
              IF(VARABL(I,1).EQ.164.0D0) VNAME='ROLLY '
              CURVALV=VARABL(I,4)
C     DO TVB HEADER PRINTING
C     PRINT TVB DATA
              WRITE(OUTLYNE,100) INT(W1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
              IF(VARABL(I,7).EQ.0.0D0.AND.VARABL(I,12).EQ.0.0D0)
     1        WRITE(OUTLYNE,102) INT(W1),VNAME,
     1        INT(VARABL(I,3)),
     2        CURVALV,VARABL(I,8)
              IF(VARABL(I,7).NE.0.0D0.AND.VARABL(I,12).EQ.0.0D0)
     1        WRITE(OUTLYNE,1022) INT(W1),VNAME,
     1        INT(VARABL(I,3)),
     2        CURVALV,VARABL(I,8),INT(VARABL(I,7))
              IF(VARABL(I,7).NE.0.0D0.AND.VARABL(I,12).NE.0.0D0)
     1        WRITE(OUTLYNE,1023) INT(W1),VNAME,
     1        INT(VARABL(I,3)),
     2        CURVALV,VARABL(I,8),INT(VARABL(I,7)),INT(VARABL(I,12))
              IF(VARABL(I,7).EQ.0.0D0.AND.VARABL(I,12).NE.0.0D0)
     1        WRITE(OUTLYNE,1025) INT(W1),VNAME,
     1        INT(VARABL(I,3)),
     2        CURVALV,VARABL(I,8),INT(VARABL(I,12))
              CALL SHOWIT(0)
              IF(VNAME(1:5).EQ.'STILT'.OR.VNAME(1:5).EQ.'BTILT') THEN
                  WRITE(OUTLYNE,1024) VARABL(I,9),VARABL(I,10),VARABL(I,11)
                  CALL SHOWIT(0)
              END IF
 100          FORMAT(
     1        'CURRENT TOLERANCE VARIABLE DATA (TVB) FOR TVAR #',I6)
 101          FORMAT('TVAR #',1X,'VARIABLE',1X,'SURF#',1X,
     1        'CURRENT VALUE',2X,'DELTA VALUE',2X,'2ND SURF#',2X,'ROLL SURF#')
 102          FORMAT(I6,3X,A6,1X,I6,1X,G13.6,2X,G13.6)
 1022         FORMAT(I6,3X,A6,1X,I6,1X,G13.6,2X,G13.6,5X,I3)
 1023         FORMAT(I6,3X,A6,1X,I6,1X,G13.6,2X,G13.6,5X,I3,9X,I3)
 1025         FORMAT(I6,3X,A6,1X,I6,1X,G13.6,2X,G13.6,8X,9X,I3)
 1024         FORMAT('(X-PIVOT=',G13.6,' Y-PIVOT=',G13.6,
     1        ' Z-PIVOT=',G13.6,')')
              RETURN
          ELSE
C     DF1=1, NO NUMERIC INPUT OF CONFIG OUTPUT, DO ALL OUTPUT
C     HERE DO THE VARIABLE OUTPUT FOR ALL VARIABLES
              WRITE(OUTLYNE,400)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
 400          FORMAT(
     1        'CURRENT TOLERANCE VARIABLE DATA (TVB)')
C     DO TVB HEADER PRINTING
              DO I=1,TVBCNT
                  II=I+MAXCMP
                  IF(VARABL(II,1).EQ.1.0D0)   VNAME='RD    '
                  IF(VARABL(II,1).EQ.2.0D0)   VNAME='CV    '
                  IF(VARABL(II,1).EQ.3.0D0)   VNAME='TH    '
                  IF(VARABL(II,1).EQ.4.0D0)   VNAME='CC    '
                  IF(VARABL(II,1).EQ.5.0D0)   VNAME='AD    '
                  IF(VARABL(II,1).EQ.6.0D0)   VNAME='AE    '
                  IF(VARABL(II,1).EQ.7.0D0)   VNAME='AF    '
                  IF(VARABL(II,1).EQ.8.0D0)   VNAME='AG    '
                  IF(VARABL(II,1).EQ.9.0D0)   VNAME='RDTOR '
                  IF(VARABL(II,1).EQ.10.0D0)  VNAME='CVTOR '
                  IF(VARABL(II,1).EQ.11.0D0)  VNAME='CCTOR '
                  IF(VARABL(II,1).EQ.12.0D0)  VNAME='ADTOR '
                  IF(VARABL(II,1).EQ.13.0D0)  VNAME='AETOR '
                  IF(VARABL(II,1).EQ.14.0D0)  VNAME='AFTOR '
                  IF(VARABL(II,1).EQ.15.0D0)  VNAME='AGTOR '
                  IF(VARABL(II,1).EQ.16.0D0)  VNAME='ALPHA '
                  IF(VARABL(II,1).EQ.17.0D0)  VNAME='BETA  '
                  IF(VARABL(II,1).EQ.18.0D0)  VNAME='GAMMA '
                  IF(VARABL(II,1).EQ.19.0D0)  VNAME='XD    '
                  IF(VARABL(II,1).EQ.20.0D0)  VNAME='YD    '
                  IF(VARABL(II,1).EQ.21.0D0)  VNAME='N1    '
                  IF(VARABL(II,1).EQ.22.0D0)  VNAME='N2    '
                  IF(VARABL(II,1).EQ.23.0D0)  VNAME='N3    '
                  IF(VARABL(II,1).EQ.24.0D0)  VNAME='N4    '
                  IF(VARABL(II,1).EQ.25.0D0)  VNAME='N5    '
                  IF(VARABL(II,1).EQ.27.0D0)  VNAME='C1    '
                  IF(VARABL(II,1).EQ.28.0D0)  VNAME='C2    '
                  IF(VARABL(II,1).EQ.29.0D0)  VNAME='C3    '
                  IF(VARABL(II,1).EQ.30.0D0)  VNAME='C4    '
                  IF(VARABL(II,1).EQ.31.0D0)  VNAME='C5    '
                  IF(VARABL(II,1).EQ.32.0D0)  VNAME='C6    '
                  IF(VARABL(II,1).EQ.33.0D0)  VNAME='C7    '
                  IF(VARABL(II,1).EQ.34.0D0)  VNAME='C8    '
                  IF(VARABL(II,1).EQ.35.0D0)  VNAME='C9    '
                  IF(VARABL(II,1).EQ.36.0D0)  VNAME='C10   '
                  IF(VARABL(II,1).EQ.37.0D0)  VNAME='C11   '
                  IF(VARABL(II,1).EQ.38.0D0)  VNAME='C12   '
                  IF(VARABL(II,1).EQ.39.0D0)  VNAME='C13   '
                  IF(VARABL(II,1).EQ.40.0D0)  VNAME='C14   '
                  IF(VARABL(II,1).EQ.41.0D0)  VNAME='C15   '
                  IF(VARABL(II,1).EQ.42.0D0)  VNAME='C16   '
                  IF(VARABL(II,1).EQ.43.0D0)  VNAME='C17   '
                  IF(VARABL(II,1).EQ.44.0D0)  VNAME='C18   '
                  IF(VARABL(II,1).EQ.45.0D0)  VNAME='C19   '
                  IF(VARABL(II,1).EQ.46.0D0)  VNAME='C20   '
                  IF(VARABL(II,1).EQ.47.0D0)  VNAME='C21   '
                  IF(VARABL(II,1).EQ.48.0D0)  VNAME='C22   '
                  IF(VARABL(II,1).EQ.49.0D0)  VNAME='C23   '
                  IF(VARABL(II,1).EQ.50.0D0)  VNAME='C24   '
                  IF(VARABL(II,1).EQ.51.0D0)  VNAME='C25   '
                  IF(VARABL(II,1).EQ.52.0D0)  VNAME='C26   '
                  IF(VARABL(II,1).EQ.53.0D0)  VNAME='C27   '
                  IF(VARABL(II,1).EQ.54.0D0)  VNAME='C28   '
                  IF(VARABL(II,1).EQ.55.0D0)  VNAME='C29   '
                  IF(VARABL(II,1).EQ.56.0D0)  VNAME='C30   '
                  IF(VARABL(II,1).EQ.57.0D0)  VNAME='C31   '
                  IF(VARABL(II,1).EQ.58.0D0)  VNAME='C32   '
                  IF(VARABL(II,1).EQ.59.0D0)  VNAME='C33   '
                  IF(VARABL(II,1).EQ.60.0D0)  VNAME='C34   '
                  IF(VARABL(II,1).EQ.61.0D0)  VNAME='C35   '
                  IF(VARABL(II,1).EQ.62.0D0)  VNAME='C36   '
                  IF(VARABL(II,1).EQ.63.0D0)  VNAME='C37   '
                  IF(VARABL(II,1).EQ.64.0D0)  VNAME='C38   '
                  IF(VARABL(II,1).EQ.65.0D0)  VNAME='C39   '
                  IF(VARABL(II,1).EQ.66.0D0)  VNAME='C40   '
                  IF(VARABL(II,1).EQ.67.0D0)  VNAME='C41   '
                  IF(VARABL(II,1).EQ.68.0D0)  VNAME='C42   '
                  IF(VARABL(II,1).EQ.69.0D0)  VNAME='C43   '
                  IF(VARABL(II,1).EQ.70.0D0)  VNAME='C44   '
                  IF(VARABL(II,1).EQ.71.0D0)  VNAME='C45   '
                  IF(VARABL(II,1).EQ.72.0D0)  VNAME='C46   '
                  IF(VARABL(II,1).EQ.73.0D0)  VNAME='C47   '
                  IF(VARABL(II,1).EQ.74.0D0)  VNAME='C48   '
                  IF(VARABL(II,1).EQ.75.0D0)  VNAME='AC    '
                  IF(VARABL(II,1).EQ.76.0D0)  VNAME='C49   '
                  IF(VARABL(II,1).EQ.77.0D0)  VNAME='C50   '
                  IF(VARABL(II,1).EQ.78.0D0)  VNAME='C51   '
                  IF(VARABL(II,1).EQ.79.0D0)  VNAME='C52   '
                  IF(VARABL(II,1).EQ.80.0D0)  VNAME='C53   '
                  IF(VARABL(II,1).EQ.81.0D0)  VNAME='C54   '
                  IF(VARABL(II,1).EQ.82.0D0)  VNAME='C55   '
                  IF(VARABL(II,1).EQ.83.0D0)  VNAME='C56   '
                  IF(VARABL(II,1).EQ.84.0D0)  VNAME='C57   '
                  IF(VARABL(II,1).EQ.85.0D0)  VNAME='C58   '
                  IF(VARABL(II,1).EQ.86.0D0)  VNAME='C59   '
                  IF(VARABL(II,1).EQ.87.0D0)  VNAME='C60   '
                  IF(VARABL(II,1).EQ.88.0D0)  VNAME='C61   '
                  IF(VARABL(II,1).EQ.89.0D0)  VNAME='C62   '
                  IF(VARABL(II,1).EQ.90.0D0)  VNAME='C63   '
                  IF(VARABL(II,1).EQ.91.0D0)  VNAME='C64   '
                  IF(VARABL(II,1).EQ.92.0D0)  VNAME='C65   '
                  IF(VARABL(II,1).EQ.93.0D0)  VNAME='C66   '
                  IF(VARABL(II,1).EQ.94.0D0)  VNAME='C67   '
                  IF(VARABL(II,1).EQ.95.0D0)  VNAME='C68   '
                  IF(VARABL(II,1).EQ.96.0D0)  VNAME='C69   '
                  IF(VARABL(II,1).EQ.97.0D0)  VNAME='C70   '
                  IF(VARABL(II,1).EQ.98.0D0)  VNAME='C71   '
                  IF(VARABL(II,1).EQ.99.0D0)  VNAME='C72   '
                  IF(VARABL(II,1).EQ.100.0D0) VNAME='C73   '
                  IF(VARABL(II,1).EQ.101.0D0) VNAME='C74   '
                  IF(VARABL(II,1).EQ.102.0D0) VNAME='C75   '
                  IF(VARABL(II,1).EQ.103.0D0) VNAME='C76   '
                  IF(VARABL(II,1).EQ.104.0D0) VNAME='C77   '
                  IF(VARABL(II,1).EQ.105.0D0) VNAME='C78   '
                  IF(VARABL(II,1).EQ.106.0D0) VNAME='C79   '
                  IF(VARABL(II,1).EQ.107.0D0) VNAME='C80   '
                  IF(VARABL(II,1).EQ.108.0D0) VNAME='C81   '
                  IF(VARABL(II,1).EQ.109.0D0) VNAME='C82   '
                  IF(VARABL(II,1).EQ.110.0D0) VNAME='C83   '
                  IF(VARABL(II,1).EQ.111.0D0) VNAME='C84   '
                  IF(VARABL(II,1).EQ.112.0D0) VNAME='C85   '
                  IF(VARABL(II,1).EQ.113.0D0) VNAME='C86   '
                  IF(VARABL(II,1).EQ.114.0D0) VNAME='C87   '
                  IF(VARABL(II,1).EQ.115.0D0) VNAME='C88   '
                  IF(VARABL(II,1).EQ.116.0D0) VNAME='C89   '
                  IF(VARABL(II,1).EQ.117.0D0) VNAME='C90   '
                  IF(VARABL(II,1).EQ.118.0D0) VNAME='C91   '
                  IF(VARABL(II,1).EQ.119.0D0) VNAME='C92   '
                  IF(VARABL(II,1).EQ.120.0D0) VNAME='C93   '
                  IF(VARABL(II,1).EQ.121.0D0) VNAME='C94   '
                  IF(VARABL(II,1).EQ.122.0D0) VNAME='C95   '
                  IF(VARABL(II,1).EQ.123.0D0) VNAME='C96   '
                  IF(VARABL(II,1).EQ.124.0D0) VNAME='N6    '
                  IF(VARABL(II,1).EQ.125.0D0) VNAME='N7    '
                  IF(VARABL(II,1).EQ.126.0D0) VNAME='N8    '
                  IF(VARABL(II,1).EQ.127.0D0) VNAME='N9    '
                  IF(VARABL(II,1).EQ.128.0D0) VNAME='N10   '
                  IF(VARABL(II,1).EQ.129.0D0) VNAME='AH    '
                  IF(VARABL(II,1).EQ.130.0D0) VNAME='AI    '
                  IF(VARABL(II,1).EQ.131.0D0) VNAME='AJ    '
                  IF(VARABL(II,1).EQ.132.0D0) VNAME='AK    '
                  IF(VARABL(II,1).EQ.133.0D0) VNAME='AL    '
                  IF(VARABL(II,1).EQ.134.0D0) VNAME='RD_FR '
                  IF(VARABL(II,1).EQ.135.0D0) VNAME='CV_FR '
                  IF(VARABL(II,1).EQ.136.0D0) VNAME='RDTFR '
                  IF(VARABL(II,1).EQ.137.0D0) VNAME='CVTFR '
                  IF(VARABL(II,1).EQ.138.0D0) VNAME='ZD    '
                  IF(VARABL(II,1).EQ.139.0D0) VNAME='INDEX '
                  IF(VARABL(II,1).EQ.140.0D0) VNAME='VNUM  '
                  IF(VARABL(II,1).EQ.141.0D0) VNAME='PIVX  '
                  IF(VARABL(II,1).EQ.142.0D0) VNAME='PIVY  '
                  IF(VARABL(II,1).EQ.143.0D0) VNAME='PIVZ  '
                  IF(VARABL(II,1).EQ.144.0D0) VNAME='DPART '
                  IF(VARABL(II,1).EQ.145.0D0) VNAME='CLPX  '
                  IF(VARABL(II,1).EQ.146.0D0) VNAME='CLPY  '
                  IF(VARABL(II,1).EQ.147.0D0) VNAME='GDX   '
                  IF(VARABL(II,1).EQ.148.0D0) VNAME='GDY   '
                  IF(VARABL(II,1).EQ.149.0D0) VNAME='GDZ   '
                  IF(VARABL(II,1).EQ.150.0D0) VNAME='GALPHA'
                  IF(VARABL(II,1).EQ.151.0D0) VNAME='GBETA '
                  IF(VARABL(II,1).EQ.152.0D0) VNAME='GGAMMA'
                  IF(VARABL(II,1).EQ.153.0D0) VNAME='GRS   '
                  IF(VARABL(II,1).EQ.154.0D0) VNAME='DISPX '
                  IF(VARABL(II,1).EQ.155.0D0) VNAME='DISPY '
                  IF(VARABL(II,1).EQ.156.0D0) VNAME='DISPZ '
                  IF(VARABL(II,1).EQ.157.0D0) VNAME='STILTA'
                  IF(VARABL(II,1).EQ.158.0D0) VNAME='STILTB'
                  IF(VARABL(II,1).EQ.159.0D0) VNAME='STILTG'
                  IF(VARABL(II,1).EQ.160.0D0) VNAME='BTILTA'
                  IF(VARABL(II,1).EQ.161.0D0) VNAME='BTILTB'
                  IF(VARABL(II,1).EQ.162.0D0) VNAME='BTILTG'
                  IF(VARABL(II,1).EQ.163.0D0) VNAME='ROLLX '
                  IF(VARABL(II,1).EQ.164.0D0) VNAME='ROLLY '
                  CURVALV=VARABL(II,4)
C     PRINT VB DATA
                  IF(VARABL(II,7).EQ.0.0D0.AND.VARABL(II,12).EQ.0.0D0)
     1            WRITE(OUTLYNE,102) I,VNAME,
     1            INT(VARABL(II,3)),
     2            CURVALV,VARABL(II,8)
                  IF(VARABL(II,7).NE.0.0D0.AND.VARABL(II,12).EQ.0.0D0)
     1            WRITE(OUTLYNE,1022) I,VNAME,
     1            INT(VARABL(II,3)),
     2            CURVALV,VARABL(II,8),INT(VARABL(II,7))
                  IF(VARABL(II,7).NE.0.0D0.AND.VARABL(II,12).NE.0.0D0)
     1            WRITE(OUTLYNE,1023) I,VNAME,
     1            INT(VARABL(II,3)),
     2            CURVALV,VARABL(II,8),INT(VARABL(II,7)),INT(VARABL(II,12))
                  IF(VARABL(II,7).EQ.0.0D0.AND.VARABL(II,12).NE.0.0D0)
     1            WRITE(OUTLYNE,1025) I,VNAME,
     1            INT(VARABL(II,3)),
     2            CURVALV,VARABL(II,8),INT(VARABL(II,12))
                  CALL SHOWIT(0)
                  IF(VNAME(1:5).EQ.'STILT'.OR.VNAME(1:5).EQ.'BTILT') THEN
                      WRITE(OUTLYNE,1024) VARABL(II,9),VARABL(II,10),VARABL(II,11)
                      CALL SHOWIT(0)
                  END IF
              END DO
          END IF
          RETURN
      END
C SUB CVBA.FOR
      SUBROUTINE CVBA
C
          IMPLICIT NONE
C
          LOGICAL YES
C
          CHARACTER VNAME*6
C
          LOGICAL ALLER
C
          INTEGER I
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
          IF(DF1.EQ.1) ALLER=.TRUE.
          IF(DF1.EQ.0) ALLER=.FALSE.
C
C       THIS IS SUBROUTINE CVBA. THIS IS THE SUBROUTINE WHICH DOES VARIABLE
C       (COMPENSATION)
C       OUTPUT COMMANDS AT THE CMD LEVEL AND FROM INSIDE VARIABLE AND
C       UPDATE VARIABLE
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"COMPS" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"COMPS" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          YES=.FALSE.
          DO I=1,MAXCMP
              IF(ISCOMP(I)) YES=.TRUE.
          END DO
          IF(.NOT.YES) CMPCNT=0
          IF(YES) CMPCNT=1
          IF(CMPCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'NO COMPENSATION VARIABLE DATA EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO-ACTION TAKEN'
              CALL SHOWIT(1)
              RETURN
          END IF
C     NOW ALLER IS PROBERLY SET
          IF(DF1.EQ.0) THEN
C     CHECK FOR A VALID NUMERIC WORD #1 INPUT VALUE
C     AND PERFORM THE OPERATION
              IF(W1.LE.0.0D0.OR.W1.GT.MAXCMP) THEN
                  WRITE(OUTLYNE,*)
     1            'COMPENSATION VARIABLE NUMBER BEYOND DEFINED BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(.NOT.ISCOMP(INT(W1))) THEN
                  WRITE(OUTLYNE,*)
     1            'COMPENSATION VARIABLE NUMBER ',INT(W1),' NOT CURRENTLY DEFINED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     HERE DO THE OUTPUT FOR VARIABLE NUMBER INT(W1)
C       CHECK IF VARIABLE DATA EXISTS. IF NOT PRINT MESSAGE
C     ESTABLISH NAMES
C
              I=INT(W1)
              IF(VARABL(I,1).EQ.1.0D0)   VNAME='CV    '
              IF(VARABL(I,1).EQ.2.0D0)   VNAME='CV    '
              IF(VARABL(I,1).EQ.3.0D0)   VNAME='TH    '
              IF(VARABL(I,1).EQ.4.0D0)   VNAME='CC    '
              IF(VARABL(I,1).EQ.5.0D0)   VNAME='AD    '
              IF(VARABL(I,1).EQ.6.0D0)   VNAME='AE    '
              IF(VARABL(I,1).EQ.7.0D0)   VNAME='AF    '
              IF(VARABL(I,1).EQ.8.0D0)   VNAME='AG    '
              IF(VARABL(I,1).EQ.9.0D0)   VNAME='CVTOR '
              IF(VARABL(I,1).EQ.10.0D0)  VNAME='CVTOR '
              IF(VARABL(I,1).EQ.11.0D0)  VNAME='CCTOR '
              IF(VARABL(I,1).EQ.12.0D0)  VNAME='ADTOR '
              IF(VARABL(I,1).EQ.13.0D0)  VNAME='AETOR '
              IF(VARABL(I,1).EQ.14.0D0)  VNAME='AFTOR '
              IF(VARABL(I,1).EQ.15.0D0)  VNAME='AGTOR '
              IF(VARABL(I,1).EQ.16.0D0)  VNAME='ALPHA '
              IF(VARABL(I,1).EQ.17.0D0)  VNAME='BETA  '
              IF(VARABL(I,1).EQ.18.0D0)  VNAME='GAMMA '
              IF(VARABL(I,1).EQ.19.0D0)  VNAME='XD    '
              IF(VARABL(I,1).EQ.20.0D0)  VNAME='YD    '
              IF(VARABL(I,1).EQ.21.0D0)  VNAME='N1    '
              IF(VARABL(I,1).EQ.22.0D0)  VNAME='N2    '
              IF(VARABL(I,1).EQ.23.0D0)  VNAME='N3    '
              IF(VARABL(I,1).EQ.24.0D0)  VNAME='N4    '
              IF(VARABL(I,1).EQ.25.0D0)  VNAME='N5    '
              IF(VARABL(I,1).EQ.27.0D0)  VNAME='C1    '
              IF(VARABL(I,1).EQ.28.0D0)  VNAME='C2    '
              IF(VARABL(I,1).EQ.29.0D0)  VNAME='C3    '
              IF(VARABL(I,1).EQ.30.0D0)  VNAME='C4    '
              IF(VARABL(I,1).EQ.31.0D0)  VNAME='C5    '
              IF(VARABL(I,1).EQ.32.0D0)  VNAME='C6    '
              IF(VARABL(I,1).EQ.33.0D0)  VNAME='C7    '
              IF(VARABL(I,1).EQ.34.0D0)  VNAME='C8    '
              IF(VARABL(I,1).EQ.35.0D0)  VNAME='C9    '
              IF(VARABL(I,1).EQ.36.0D0)  VNAME='C10   '
              IF(VARABL(I,1).EQ.37.0D0)  VNAME='C11   '
              IF(VARABL(I,1).EQ.38.0D0)  VNAME='C12   '
              IF(VARABL(I,1).EQ.39.0D0)  VNAME='C13   '
              IF(VARABL(I,1).EQ.40.0D0)  VNAME='C14   '
              IF(VARABL(I,1).EQ.41.0D0)  VNAME='C15   '
              IF(VARABL(I,1).EQ.42.0D0)  VNAME='C16   '
              IF(VARABL(I,1).EQ.43.0D0)  VNAME='C17   '
              IF(VARABL(I,1).EQ.44.0D0)  VNAME='C18   '
              IF(VARABL(I,1).EQ.45.0D0)  VNAME='C19   '
              IF(VARABL(I,1).EQ.46.0D0)  VNAME='C20   '
              IF(VARABL(I,1).EQ.47.0D0)  VNAME='C21   '
              IF(VARABL(I,1).EQ.48.0D0)  VNAME='C22   '
              IF(VARABL(I,1).EQ.49.0D0)  VNAME='C23   '
              IF(VARABL(I,1).EQ.50.0D0)  VNAME='C24   '
              IF(VARABL(I,1).EQ.51.0D0)  VNAME='C25   '
              IF(VARABL(I,1).EQ.52.0D0)  VNAME='C26   '
              IF(VARABL(I,1).EQ.53.0D0)  VNAME='C27   '
              IF(VARABL(I,1).EQ.54.0D0)  VNAME='C28   '
              IF(VARABL(I,1).EQ.55.0D0)  VNAME='C29   '
              IF(VARABL(I,1).EQ.56.0D0)  VNAME='C30   '
              IF(VARABL(I,1).EQ.57.0D0)  VNAME='C31   '
              IF(VARABL(I,1).EQ.58.0D0)  VNAME='C32   '
              IF(VARABL(I,1).EQ.59.0D0)  VNAME='C33   '
              IF(VARABL(I,1).EQ.60.0D0)  VNAME='C34   '
              IF(VARABL(I,1).EQ.61.0D0)  VNAME='C35   '
              IF(VARABL(I,1).EQ.62.0D0)  VNAME='C36   '
              IF(VARABL(I,1).EQ.63.0D0)  VNAME='C37   '
              IF(VARABL(I,1).EQ.64.0D0)  VNAME='C38   '
              IF(VARABL(I,1).EQ.65.0D0)  VNAME='C39   '
              IF(VARABL(I,1).EQ.66.0D0)  VNAME='C40   '
              IF(VARABL(I,1).EQ.67.0D0)  VNAME='C41   '
              IF(VARABL(I,1).EQ.68.0D0)  VNAME='C42   '
              IF(VARABL(I,1).EQ.69.0D0)  VNAME='C43   '
              IF(VARABL(I,1).EQ.70.0D0)  VNAME='C44   '
              IF(VARABL(I,1).EQ.71.0D0)  VNAME='C45   '
              IF(VARABL(I,1).EQ.72.0D0)  VNAME='C46   '
              IF(VARABL(I,1).EQ.73.0D0)  VNAME='C47   '
              IF(VARABL(I,1).EQ.74.0D0)  VNAME='C48   '
              IF(VARABL(I,1).EQ.75.0D0)  VNAME='AC    '
              IF(VARABL(I,1).EQ.76.0D0)  VNAME='C49   '
              IF(VARABL(I,1).EQ.77.0D0)  VNAME='C50   '
              IF(VARABL(I,1).EQ.78.0D0)  VNAME='C51   '
              IF(VARABL(I,1).EQ.79.0D0)  VNAME='C52   '
              IF(VARABL(I,1).EQ.80.0D0)  VNAME='C53   '
              IF(VARABL(I,1).EQ.81.0D0)  VNAME='C54   '
              IF(VARABL(I,1).EQ.82.0D0)  VNAME='C55   '
              IF(VARABL(I,1).EQ.83.0D0)  VNAME='C56   '
              IF(VARABL(I,1).EQ.84.0D0)  VNAME='C57   '
              IF(VARABL(I,1).EQ.85.0D0)  VNAME='C58   '
              IF(VARABL(I,1).EQ.86.0D0)  VNAME='C59   '
              IF(VARABL(I,1).EQ.87.0D0)  VNAME='C60   '
              IF(VARABL(I,1).EQ.88.0D0)  VNAME='C61   '
              IF(VARABL(I,1).EQ.89.0D0)  VNAME='C62   '
              IF(VARABL(I,1).EQ.90.0D0)  VNAME='C63   '
              IF(VARABL(I,1).EQ.91.0D0)  VNAME='C64   '
              IF(VARABL(I,1).EQ.92.0D0)  VNAME='C65   '
              IF(VARABL(I,1).EQ.93.0D0)  VNAME='C66   '
              IF(VARABL(I,1).EQ.94.0D0)  VNAME='C67   '
              IF(VARABL(I,1).EQ.95.0D0)  VNAME='C68   '
              IF(VARABL(I,1).EQ.96.0D0)  VNAME='C69   '
              IF(VARABL(I,1).EQ.97.0D0)  VNAME='C70   '
              IF(VARABL(I,1).EQ.98.0D0)  VNAME='C71   '
              IF(VARABL(I,1).EQ.99.0D0)  VNAME='C72   '
              IF(VARABL(I,1).EQ.100.0D0) VNAME='C73   '
              IF(VARABL(I,1).EQ.101.0D0) VNAME='C74   '
              IF(VARABL(I,1).EQ.102.0D0) VNAME='C75   '
              IF(VARABL(I,1).EQ.103.0D0) VNAME='C76   '
              IF(VARABL(I,1).EQ.104.0D0) VNAME='C77   '
              IF(VARABL(I,1).EQ.105.0D0) VNAME='C78   '
              IF(VARABL(I,1).EQ.106.0D0) VNAME='C79   '
              IF(VARABL(I,1).EQ.107.0D0) VNAME='C80   '
              IF(VARABL(I,1).EQ.108.0D0) VNAME='C81   '
              IF(VARABL(I,1).EQ.109.0D0) VNAME='C82   '
              IF(VARABL(I,1).EQ.110.0D0) VNAME='C83   '
              IF(VARABL(I,1).EQ.111.0D0) VNAME='C84   '
              IF(VARABL(I,1).EQ.112.0D0) VNAME='C85   '
              IF(VARABL(I,1).EQ.113.0D0) VNAME='C86   '
              IF(VARABL(I,1).EQ.114.0D0) VNAME='C87   '
              IF(VARABL(I,1).EQ.115.0D0) VNAME='C88   '
              IF(VARABL(I,1).EQ.116.0D0) VNAME='C89   '
              IF(VARABL(I,1).EQ.117.0D0) VNAME='C90   '
              IF(VARABL(I,1).EQ.118.0D0) VNAME='C91   '
              IF(VARABL(I,1).EQ.119.0D0) VNAME='C92   '
              IF(VARABL(I,1).EQ.120.0D0) VNAME='C93   '
              IF(VARABL(I,1).EQ.121.0D0) VNAME='C94   '
              IF(VARABL(I,1).EQ.122.0D0) VNAME='C95   '
              IF(VARABL(I,1).EQ.123.0D0) VNAME='C96   '
              IF(VARABL(I,1).EQ.124.0D0) VNAME='N6    '
              IF(VARABL(I,1).EQ.125.0D0) VNAME='N7    '
              IF(VARABL(I,1).EQ.126.0D0) VNAME='N8    '
              IF(VARABL(I,1).EQ.127.0D0) VNAME='N9    '
              IF(VARABL(I,1).EQ.128.0D0) VNAME='N10   '
              IF(VARABL(I,1).EQ.129.0D0) VNAME='AH    '
              IF(VARABL(I,1).EQ.130.0D0) VNAME='AI    '
              IF(VARABL(I,1).EQ.131.0D0) VNAME='AJ    '
              IF(VARABL(I,1).EQ.132.0D0) VNAME='AK    '
              IF(VARABL(I,1).EQ.133.0D0) VNAME='AL    '
              IF(VARABL(I,1).EQ.134.0D0) VNAME='ZD    '
              IF(VARABL(I,1).EQ.135.0D0) VNAME='INDEX '
              IF(VARABL(I,1).EQ.136.0D0) VNAME='VNUM  '
              IF(VARABL(I,1).EQ.137.0D0) VNAME='PIVX  '
              IF(VARABL(I,1).EQ.138.0D0) VNAME='PIVY  '
              IF(VARABL(I,1).EQ.139.0D0) VNAME='PIVZ  '
              IF(VARABL(I,1).EQ.140.0D0) VNAME='DPART '
              IF(VARABL(I,1).EQ.141.0D0) VNAME='CLPX  '
              IF(VARABL(I,1).EQ.142.0D0) VNAME='CLPY  '
              IF(VARABL(I,1).EQ.143.0D0) VNAME='GDX   '
              IF(VARABL(I,1).EQ.144.0D0) VNAME='GDY   '
              IF(VARABL(I,1).EQ.145.0D0) VNAME='GDZ   '
              IF(VARABL(I,1).EQ.146.0D0) VNAME='GALPHA'
              IF(VARABL(I,1).EQ.147.0D0) VNAME='GBETA '
              IF(VARABL(I,1).EQ.148.0D0) VNAME='GGAMMA'
              IF(VARABL(I,1).EQ.149.0D0) VNAME='GRS   '
C     DO COMPS HEADER PRINTING
C     PRINT COMPS DATA
              WRITE(OUTLYNE,100) INT(W1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,102) INT(W1),VNAME,
     1        INT(VARABL(INT(W1),3)),
     2        VARABL(INT(W1),4),VARABL(INT(W1),8)
              CALL SHOWIT(0)
 100          FORMAT(
     1        'CURRENT COMPENSATION VARIABLE DATA (COMPS) FOR VARIABLE #',I6)
 101          FORMAT('COMPVAR #',1X,'VARIABLE',2X,'SURF#',3X,
     1        'CURRENT VALUE',2X,'DINCR VALUE')
 102          FORMAT(3X,I6,3X,A6,1X,I6,3X,G13.6,2X,G13.6)
              RETURN
          ELSE
C     DF1=1, NO NUMERIC INPUT OF CONFIG OUTPUT, DO ALL OUTPUT
C     HERE DO THE VARIABLE OUTPUT FOR ALL VARIABLES
              WRITE(OUTLYNE,400)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
 400          FORMAT(
     1        'CURRENT COMPENSATION VARIABLE DATA (COMPS)')
C     DO COMPS HEADER PRINTING
              DO I=1,MAXCMP
                  IF(ISCOMP(I)) THEN
                      IF(VARABL(I,1).EQ.1.0D0)   VNAME='CV    '
                      IF(VARABL(I,1).EQ.2.0D0)   VNAME='CV    '
                      IF(VARABL(I,1).EQ.3.0D0)   VNAME='TH    '
                      IF(VARABL(I,1).EQ.4.0D0)   VNAME='CC    '
                      IF(VARABL(I,1).EQ.5.0D0)   VNAME='AD    '
                      IF(VARABL(I,1).EQ.6.0D0)   VNAME='AE    '
                      IF(VARABL(I,1).EQ.7.0D0)   VNAME='AF    '
                      IF(VARABL(I,1).EQ.8.0D0)   VNAME='AG    '
                      IF(VARABL(I,1).EQ.9.0D0)   VNAME='CVTOR '
                      IF(VARABL(I,1).EQ.10.0D0)  VNAME='CVTOR '
                      IF(VARABL(I,1).EQ.11.0D0)  VNAME='CCTOR '
                      IF(VARABL(I,1).EQ.12.0D0)  VNAME='ADTOR '
                      IF(VARABL(I,1).EQ.13.0D0)  VNAME='AETOR '
                      IF(VARABL(I,1).EQ.14.0D0)  VNAME='AFTOR '
                      IF(VARABL(I,1).EQ.15.0D0)  VNAME='AGTOR '
                      IF(VARABL(I,1).EQ.16.0D0)  VNAME='ALPHA '
                      IF(VARABL(I,1).EQ.17.0D0)  VNAME='BETA  '
                      IF(VARABL(I,1).EQ.18.0D0)  VNAME='GAMMA '
                      IF(VARABL(I,1).EQ.19.0D0)  VNAME='XD    '
                      IF(VARABL(I,1).EQ.20.0D0)  VNAME='YD    '
                      IF(VARABL(I,1).EQ.21.0D0)  VNAME='N1    '
                      IF(VARABL(I,1).EQ.22.0D0)  VNAME='N2    '
                      IF(VARABL(I,1).EQ.23.0D0)  VNAME='N3    '
                      IF(VARABL(I,1).EQ.24.0D0)  VNAME='N4    '
                      IF(VARABL(I,1).EQ.25.0D0)  VNAME='N5    '
                      IF(VARABL(I,1).EQ.27.0D0)  VNAME='C1    '
                      IF(VARABL(I,1).EQ.28.0D0)  VNAME='C2    '
                      IF(VARABL(I,1).EQ.29.0D0)  VNAME='C3    '
                      IF(VARABL(I,1).EQ.30.0D0)  VNAME='C4    '
                      IF(VARABL(I,1).EQ.31.0D0)  VNAME='C5    '
                      IF(VARABL(I,1).EQ.32.0D0)  VNAME='C6    '
                      IF(VARABL(I,1).EQ.33.0D0)  VNAME='C7    '
                      IF(VARABL(I,1).EQ.34.0D0)  VNAME='C8    '
                      IF(VARABL(I,1).EQ.35.0D0)  VNAME='C9    '
                      IF(VARABL(I,1).EQ.36.0D0)  VNAME='C10   '
                      IF(VARABL(I,1).EQ.37.0D0)  VNAME='C11   '
                      IF(VARABL(I,1).EQ.38.0D0)  VNAME='C12   '
                      IF(VARABL(I,1).EQ.39.0D0)  VNAME='C13   '
                      IF(VARABL(I,1).EQ.40.0D0)  VNAME='C14   '
                      IF(VARABL(I,1).EQ.41.0D0)  VNAME='C15   '
                      IF(VARABL(I,1).EQ.42.0D0)  VNAME='C16   '
                      IF(VARABL(I,1).EQ.43.0D0)  VNAME='C17   '
                      IF(VARABL(I,1).EQ.44.0D0)  VNAME='C18   '
                      IF(VARABL(I,1).EQ.45.0D0)  VNAME='C19   '
                      IF(VARABL(I,1).EQ.46.0D0)  VNAME='C20   '
                      IF(VARABL(I,1).EQ.47.0D0)  VNAME='C21   '
                      IF(VARABL(I,1).EQ.48.0D0)  VNAME='C22   '
                      IF(VARABL(I,1).EQ.49.0D0)  VNAME='C23   '
                      IF(VARABL(I,1).EQ.50.0D0)  VNAME='C24   '
                      IF(VARABL(I,1).EQ.51.0D0)  VNAME='C25   '
                      IF(VARABL(I,1).EQ.52.0D0)  VNAME='C26   '
                      IF(VARABL(I,1).EQ.53.0D0)  VNAME='C27   '
                      IF(VARABL(I,1).EQ.54.0D0)  VNAME='C28   '
                      IF(VARABL(I,1).EQ.55.0D0)  VNAME='C29   '
                      IF(VARABL(I,1).EQ.56.0D0)  VNAME='C30   '
                      IF(VARABL(I,1).EQ.57.0D0)  VNAME='C31   '
                      IF(VARABL(I,1).EQ.58.0D0)  VNAME='C32   '
                      IF(VARABL(I,1).EQ.59.0D0)  VNAME='C33   '
                      IF(VARABL(I,1).EQ.60.0D0)  VNAME='C34   '
                      IF(VARABL(I,1).EQ.61.0D0)  VNAME='C35   '
                      IF(VARABL(I,1).EQ.62.0D0)  VNAME='C36   '
                      IF(VARABL(I,1).EQ.63.0D0)  VNAME='C37   '
                      IF(VARABL(I,1).EQ.64.0D0)  VNAME='C38   '
                      IF(VARABL(I,1).EQ.65.0D0)  VNAME='C39   '
                      IF(VARABL(I,1).EQ.66.0D0)  VNAME='C40   '
                      IF(VARABL(I,1).EQ.67.0D0)  VNAME='C41   '
                      IF(VARABL(I,1).EQ.68.0D0)  VNAME='C42   '
                      IF(VARABL(I,1).EQ.69.0D0)  VNAME='C43   '
                      IF(VARABL(I,1).EQ.70.0D0)  VNAME='C44   '
                      IF(VARABL(I,1).EQ.71.0D0)  VNAME='C45   '
                      IF(VARABL(I,1).EQ.72.0D0)  VNAME='C46   '
                      IF(VARABL(I,1).EQ.73.0D0)  VNAME='C47   '
                      IF(VARABL(I,1).EQ.74.0D0)  VNAME='C48   '
                      IF(VARABL(I,1).EQ.75.0D0)  VNAME='AC    '
                      IF(VARABL(I,1).EQ.76.0D0)  VNAME='C49   '
                      IF(VARABL(I,1).EQ.77.0D0)  VNAME='C50   '
                      IF(VARABL(I,1).EQ.78.0D0)  VNAME='C51   '
                      IF(VARABL(I,1).EQ.79.0D0)  VNAME='C52   '
                      IF(VARABL(I,1).EQ.80.0D0)  VNAME='C53   '
                      IF(VARABL(I,1).EQ.81.0D0)  VNAME='C54   '
                      IF(VARABL(I,1).EQ.82.0D0)  VNAME='C55   '
                      IF(VARABL(I,1).EQ.83.0D0)  VNAME='C56   '
                      IF(VARABL(I,1).EQ.84.0D0)  VNAME='C57   '
                      IF(VARABL(I,1).EQ.85.0D0)  VNAME='C58   '
                      IF(VARABL(I,1).EQ.86.0D0)  VNAME='C59   '
                      IF(VARABL(I,1).EQ.87.0D0)  VNAME='C60   '
                      IF(VARABL(I,1).EQ.88.0D0)  VNAME='C61   '
                      IF(VARABL(I,1).EQ.89.0D0)  VNAME='C62   '
                      IF(VARABL(I,1).EQ.90.0D0)  VNAME='C63   '
                      IF(VARABL(I,1).EQ.91.0D0)  VNAME='C64   '
                      IF(VARABL(I,1).EQ.92.0D0)  VNAME='C65   '
                      IF(VARABL(I,1).EQ.93.0D0)  VNAME='C66   '
                      IF(VARABL(I,1).EQ.94.0D0)  VNAME='C67   '
                      IF(VARABL(I,1).EQ.95.0D0)  VNAME='C68   '
                      IF(VARABL(I,1).EQ.96.0D0)  VNAME='C69   '
                      IF(VARABL(I,1).EQ.97.0D0)  VNAME='C70   '
                      IF(VARABL(I,1).EQ.98.0D0)  VNAME='C71   '
                      IF(VARABL(I,1).EQ.99.0D0)  VNAME='C72   '
                      IF(VARABL(I,1).EQ.100.0D0) VNAME='C73   '
                      IF(VARABL(I,1).EQ.101.0D0) VNAME='C74   '
                      IF(VARABL(I,1).EQ.102.0D0) VNAME='C75   '
                      IF(VARABL(I,1).EQ.103.0D0) VNAME='C76   '
                      IF(VARABL(I,1).EQ.104.0D0) VNAME='C77   '
                      IF(VARABL(I,1).EQ.105.0D0) VNAME='C78   '
                      IF(VARABL(I,1).EQ.106.0D0) VNAME='C79   '
                      IF(VARABL(I,1).EQ.107.0D0) VNAME='C80   '
                      IF(VARABL(I,1).EQ.108.0D0) VNAME='C81   '
                      IF(VARABL(I,1).EQ.109.0D0) VNAME='C82   '
                      IF(VARABL(I,1).EQ.110.0D0) VNAME='C83   '
                      IF(VARABL(I,1).EQ.111.0D0) VNAME='C84   '
                      IF(VARABL(I,1).EQ.112.0D0) VNAME='C85   '
                      IF(VARABL(I,1).EQ.113.0D0) VNAME='C86   '
                      IF(VARABL(I,1).EQ.114.0D0) VNAME='C87   '
                      IF(VARABL(I,1).EQ.115.0D0) VNAME='C88   '
                      IF(VARABL(I,1).EQ.116.0D0) VNAME='C89   '
                      IF(VARABL(I,1).EQ.117.0D0) VNAME='C90   '
                      IF(VARABL(I,1).EQ.118.0D0) VNAME='C91   '
                      IF(VARABL(I,1).EQ.119.0D0) VNAME='C92   '
                      IF(VARABL(I,1).EQ.120.0D0) VNAME='C93   '
                      IF(VARABL(I,1).EQ.121.0D0) VNAME='C94   '
                      IF(VARABL(I,1).EQ.122.0D0) VNAME='C95   '
                      IF(VARABL(I,1).EQ.123.0D0) VNAME='C96   '
                      IF(VARABL(I,1).EQ.124.0D0) VNAME='N6    '
                      IF(VARABL(I,1).EQ.125.0D0) VNAME='N7    '
                      IF(VARABL(I,1).EQ.126.0D0) VNAME='N8    '
                      IF(VARABL(I,1).EQ.127.0D0) VNAME='N9    '
                      IF(VARABL(I,1).EQ.128.0D0) VNAME='N10   '
                      IF(VARABL(I,1).EQ.129.0D0) VNAME='AH    '
                      IF(VARABL(I,1).EQ.130.0D0) VNAME='AI    '
                      IF(VARABL(I,1).EQ.131.0D0) VNAME='AJ    '
                      IF(VARABL(I,1).EQ.132.0D0) VNAME='AK    '
                      IF(VARABL(I,1).EQ.133.0D0) VNAME='AL    '
                      IF(VARABL(I,1).EQ.134.0D0) VNAME='ZD    '
                      IF(VARABL(I,1).EQ.135.0D0) VNAME='INDEX '
                      IF(VARABL(I,1).EQ.136.0D0) VNAME='VNUM  '
                      IF(VARABL(I,1).EQ.137.0D0) VNAME='PIVX  '
                      IF(VARABL(I,1).EQ.138.0D0) VNAME='PIVY  '
                      IF(VARABL(I,1).EQ.139.0D0) VNAME='PIVZ  '
                      IF(VARABL(I,1).EQ.140.0D0) VNAME='DPART '
                      IF(VARABL(I,1).EQ.141.0D0) VNAME='CLPX  '
                      IF(VARABL(I,1).EQ.142.0D0) VNAME='CLPY  '
                      IF(VARABL(I,1).EQ.143.0D0) VNAME='GDX   '
                      IF(VARABL(I,1).EQ.144.0D0) VNAME='GDY   '
                      IF(VARABL(I,1).EQ.145.0D0) VNAME='GDZ   '
                      IF(VARABL(I,1).EQ.146.0D0) VNAME='GALPHA'
                      IF(VARABL(I,1).EQ.147.0D0) VNAME='GBETA '
                      IF(VARABL(I,1).EQ.148.0D0) VNAME='GGAMMA'
                      IF(VARABL(I,1).EQ.149.0D0) VNAME='GRS   '
C     PRINT COMPS DATA
                      IF(ALLER) THEN
                          WRITE(OUTLYNE,102) I,VNAME,
     1                    INT(VARABL(I,3)),
     2                    VARABL(I,4),VARABL(I,8)
                          CALL SHOWIT(0)
                      END IF
                  END IF
              END DO
          END IF
          RETURN
      END
C SUB VBA.FOR
      SUBROUTINE VBA
C
          IMPLICIT NONE
C
          CHARACTER VNAME*7,TAGNAM*4
C
          LOGICAL CFGER,ALLER
C
          INTEGER I,CFGCHK,TAGER,TAG4
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
          ALLER=.TRUE.
          CFGER=.FALSE.
          TAGER=0
C
C       THIS IS SUBROUTINE VBA. THIS IS THE SUBROUTINE WHICH DOES VARIABLE
C       OUTPUT COMMANDS AT THE CMD LEVEL AND FROM INSIDE VARIABLE AND
C       UPDATE VARIABLE
          IF(WC.EQ.'VB') THEN
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"VB" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
                  WRITE(OUTLYNE,*)
     1            '"VB" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'CFG'.AND.DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"VB CFG" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'VBA') THEN
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"VBA" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
                  WRITE(OUTLYNE,*)
     1            '"VBA" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'CFG'.AND.DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"VBA CFG" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'VB') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"VB" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'VBA') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"VBA" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(VBCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'NO VARIABLE DATA EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO-ACTION TAKEN'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WQ.EQ.'CFG') THEN
              TAGER=INT(W1)
              ALLER=.FALSE.
              CFGER=.TRUE.
          END IF
          IF(WQ.NE.'CFG'.AND.DF1.EQ.0) THEN
              TAGER=INT(W1)
              ALLER=.FALSE.
              CFGER=.FALSE.
          END IF
          IF(WQ.NE.'CFG'.AND.DF1.EQ.1) THEN
              ALLER=.TRUE.
              CFGER=.FALSE.
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
C     PROCEED
              CFGCHK=0
              DO I=0,VBCNT
                  IF(INT(VARABL(I,2)).EQ.TAGER) CFGCHK=1
              END DO
C     CHECK FOR VARIABLES ACTIVE IN CFG TAGER

              IF(CFGCHK.EQ.0) THEN
C     CFG VB DATA NOT EXISTANT
                  WRITE(OUTLYNE,*)
     1            'NO VARIABLES ARE ACTIVE IN CONFIG # ',TAGER
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     NOW CFGIS, TAGER, ALLER AND CFGER ARE PROBERLY SET
          IF(DF1.EQ.0.AND.WQ.NE.'CFG') THEN
C     CHECK FOR A VALID NUMERIC WORD #1 INPUT VALUE
C     AND PERFORM THE OPERATION
              IF(W1.LE.0.0D0.OR.W1.GT.VBCNT) THEN
                  WRITE(OUTLYNE,*)'VARIABLE NUMBER BEYOND DEFINED BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     HERE DO THE OUTPUT FOR VARIABLE NUMBER INT(W1)
C       CHECK IF VARIABLE DATA EXISTS. IF NOT PRINT MESSAGE
C     ESTABLISH NAMES
C
              I=INT(W1)
              VNAME='       '
              IF(VARABL(I,1).EQ.1.0D0)   VNAME='CV     '
              IF(VARABL(I,1).EQ.2.0D0)   VNAME='CV     '
              IF(VARABL(I,1).EQ.3.0D0)   VNAME='TH     '
              IF(VARABL(I,1).EQ.4.0D0)   VNAME='CC     '
              IF(VARABL(I,1).EQ.5.0D0)   VNAME='AD     '
              IF(VARABL(I,1).EQ.6.0D0)   VNAME='AE     '
              IF(VARABL(I,1).EQ.7.0D0)   VNAME='AF     '
              IF(VARABL(I,1).EQ.8.0D0)   VNAME='AG     '
              IF(VARABL(I,1).EQ.9.0D0)   VNAME='CVTOR  '
              IF(VARABL(I,1).EQ.10.0D0)  VNAME='CVTOR  '
              IF(VARABL(I,1).EQ.11.0D0)  VNAME='CCTOR  '
              IF(VARABL(I,1).EQ.12.0D0)  VNAME='ADTOR  '
              IF(VARABL(I,1).EQ.13.0D0)  VNAME='AETOR  '
              IF(VARABL(I,1).EQ.14.0D0)  VNAME='AFTOR  '
              IF(VARABL(I,1).EQ.15.0D0)  VNAME='AGTOR  '
              IF(VARABL(I,1).EQ.16.0D0)  VNAME='ALPHA  '
              IF(VARABL(I,1).EQ.17.0D0)  VNAME='BETA   '
              IF(VARABL(I,1).EQ.18.0D0)  VNAME='GAMMA  '
              IF(VARABL(I,1).EQ.19.0D0)  VNAME='XD     '
              IF(VARABL(I,1).EQ.20.0D0)  VNAME='YD     '
              IF(VARABL(I,1).EQ.21.0D0)  VNAME='N1     '
              IF(VARABL(I,1).EQ.22.0D0)  VNAME='N2     '
              IF(VARABL(I,1).EQ.23.0D0)  VNAME='N3     '
              IF(VARABL(I,1).EQ.24.0D0)  VNAME='N4     '
              IF(VARABL(I,1).EQ.25.0D0)  VNAME='N5     '
              IF(VARABL(I,1).EQ.27.0D0)  VNAME='C1     '
              IF(VARABL(I,1).EQ.28.0D0)  VNAME='C2     '
              IF(VARABL(I,1).EQ.29.0D0)  VNAME='C3     '
              IF(VARABL(I,1).EQ.30.0D0)  VNAME='C4     '
              IF(VARABL(I,1).EQ.31.0D0)  VNAME='C5     '
              IF(VARABL(I,1).EQ.32.0D0)  VNAME='C6     '
              IF(VARABL(I,1).EQ.33.0D0)  VNAME='C7     '
              IF(VARABL(I,1).EQ.34.0D0)  VNAME='C8     '
              IF(VARABL(I,1).EQ.35.0D0)  VNAME='C9     '
              IF(VARABL(I,1).EQ.36.0D0)  VNAME='C10    '
              IF(VARABL(I,1).EQ.37.0D0)  VNAME='C11    '
              IF(VARABL(I,1).EQ.38.0D0)  VNAME='C12    '
              IF(VARABL(I,1).EQ.39.0D0)  VNAME='C13    '
              IF(VARABL(I,1).EQ.40.0D0)  VNAME='C14    '
              IF(VARABL(I,1).EQ.41.0D0)  VNAME='C15    '
              IF(VARABL(I,1).EQ.42.0D0)  VNAME='C16    '
              IF(VARABL(I,1).EQ.43.0D0)  VNAME='C17    '
              IF(VARABL(I,1).EQ.44.0D0)  VNAME='C18    '
              IF(VARABL(I,1).EQ.45.0D0)  VNAME='C19    '
              IF(VARABL(I,1).EQ.46.0D0)  VNAME='C20    '
              IF(VARABL(I,1).EQ.47.0D0)  VNAME='C21    '
              IF(VARABL(I,1).EQ.48.0D0)  VNAME='C22    '
              IF(VARABL(I,1).EQ.49.0D0)  VNAME='C23    '
              IF(VARABL(I,1).EQ.50.0D0)  VNAME='C24    '
              IF(VARABL(I,1).EQ.51.0D0)  VNAME='C25    '
              IF(VARABL(I,1).EQ.52.0D0)  VNAME='C26    '
              IF(VARABL(I,1).EQ.53.0D0)  VNAME='C27    '
              IF(VARABL(I,1).EQ.54.0D0)  VNAME='C28    '
              IF(VARABL(I,1).EQ.55.0D0)  VNAME='C29    '
              IF(VARABL(I,1).EQ.56.0D0)  VNAME='C30    '
              IF(VARABL(I,1).EQ.57.0D0)  VNAME='C31    '
              IF(VARABL(I,1).EQ.58.0D0)  VNAME='C32    '
              IF(VARABL(I,1).EQ.59.0D0)  VNAME='C33    '
              IF(VARABL(I,1).EQ.60.0D0)  VNAME='C34    '
              IF(VARABL(I,1).EQ.61.0D0)  VNAME='C35    '
              IF(VARABL(I,1).EQ.62.0D0)  VNAME='C36    '
              IF(VARABL(I,1).EQ.63.0D0)  VNAME='C37    '
              IF(VARABL(I,1).EQ.64.0D0)  VNAME='C38    '
              IF(VARABL(I,1).EQ.65.0D0)  VNAME='C39    '
              IF(VARABL(I,1).EQ.66.0D0)  VNAME='C40    '
              IF(VARABL(I,1).EQ.67.0D0)  VNAME='C41    '
              IF(VARABL(I,1).EQ.68.0D0)  VNAME='C42    '
              IF(VARABL(I,1).EQ.69.0D0)  VNAME='C43    '
              IF(VARABL(I,1).EQ.70.0D0)  VNAME='C44    '
              IF(VARABL(I,1).EQ.71.0D0)  VNAME='C45    '
              IF(VARABL(I,1).EQ.72.0D0)  VNAME='C46    '
              IF(VARABL(I,1).EQ.73.0D0)  VNAME='C47    '
              IF(VARABL(I,1).EQ.74.0D0)  VNAME='C48    '
              IF(VARABL(I,1).EQ.75.0D0)  VNAME='AC     '
              IF(VARABL(I,1).EQ.76.0D0)  VNAME='C49    '
              IF(VARABL(I,1).EQ.77.0D0)  VNAME='C50    '
              IF(VARABL(I,1).EQ.78.0D0)  VNAME='C51    '
              IF(VARABL(I,1).EQ.79.0D0)  VNAME='C52    '
              IF(VARABL(I,1).EQ.80.0D0)  VNAME='C53    '
              IF(VARABL(I,1).EQ.81.0D0)  VNAME='C54    '
              IF(VARABL(I,1).EQ.82.0D0)  VNAME='C55    '
              IF(VARABL(I,1).EQ.83.0D0)  VNAME='C56    '
              IF(VARABL(I,1).EQ.84.0D0)  VNAME='C57    '
              IF(VARABL(I,1).EQ.85.0D0)  VNAME='C58    '
              IF(VARABL(I,1).EQ.86.0D0)  VNAME='C59    '
              IF(VARABL(I,1).EQ.87.0D0)  VNAME='C60    '
              IF(VARABL(I,1).EQ.88.0D0)  VNAME='C61    '
              IF(VARABL(I,1).EQ.89.0D0)  VNAME='C62    '
              IF(VARABL(I,1).EQ.90.0D0)  VNAME='C63    '
              IF(VARABL(I,1).EQ.91.0D0)  VNAME='C64    '
              IF(VARABL(I,1).EQ.92.0D0)  VNAME='C65    '
              IF(VARABL(I,1).EQ.93.0D0)  VNAME='C66    '
              IF(VARABL(I,1).EQ.94.0D0)  VNAME='C67    '
              IF(VARABL(I,1).EQ.95.0D0)  VNAME='C68    '
              IF(VARABL(I,1).EQ.96.0D0)  VNAME='C69    '
              IF(VARABL(I,1).EQ.97.0D0)  VNAME='C70    '
              IF(VARABL(I,1).EQ.98.0D0)  VNAME='C71    '
              IF(VARABL(I,1).EQ.99.0D0)  VNAME='C72    '
              IF(VARABL(I,1).EQ.100.0D0) VNAME='C73    '
              IF(VARABL(I,1).EQ.101.0D0) VNAME='C74    '
              IF(VARABL(I,1).EQ.102.0D0) VNAME='C75    '
              IF(VARABL(I,1).EQ.103.0D0) VNAME='C76    '
              IF(VARABL(I,1).EQ.104.0D0) VNAME='C77    '
              IF(VARABL(I,1).EQ.105.0D0) VNAME='C78    '
              IF(VARABL(I,1).EQ.106.0D0) VNAME='C79    '
              IF(VARABL(I,1).EQ.107.0D0) VNAME='C80    '
              IF(VARABL(I,1).EQ.108.0D0) VNAME='C81    '
              IF(VARABL(I,1).EQ.109.0D0) VNAME='C82    '
              IF(VARABL(I,1).EQ.110.0D0) VNAME='C83    '
              IF(VARABL(I,1).EQ.111.0D0) VNAME='C84    '
              IF(VARABL(I,1).EQ.112.0D0) VNAME='C85    '
              IF(VARABL(I,1).EQ.113.0D0) VNAME='C86    '
              IF(VARABL(I,1).EQ.114.0D0) VNAME='C87    '
              IF(VARABL(I,1).EQ.115.0D0) VNAME='C88    '
              IF(VARABL(I,1).EQ.116.0D0) VNAME='C89    '
              IF(VARABL(I,1).EQ.117.0D0) VNAME='C90    '
              IF(VARABL(I,1).EQ.118.0D0) VNAME='C91    '
              IF(VARABL(I,1).EQ.119.0D0) VNAME='C92    '
              IF(VARABL(I,1).EQ.120.0D0) VNAME='C93    '
              IF(VARABL(I,1).EQ.121.0D0) VNAME='C94    '
              IF(VARABL(I,1).EQ.122.0D0) VNAME='C95    '
              IF(VARABL(I,1).EQ.123.0D0) VNAME='C96    '
              IF(VARABL(I,1).EQ.124.0D0) VNAME='N6     '
              IF(VARABL(I,1).EQ.125.0D0) VNAME='N7     '
              IF(VARABL(I,1).EQ.126.0D0) VNAME='N8     '
              IF(VARABL(I,1).EQ.127.0D0) VNAME='N9     '
              IF(VARABL(I,1).EQ.128.0D0) VNAME='N10    '
              IF(VARABL(I,1).EQ.129.0D0) VNAME='AH     '
              IF(VARABL(I,1).EQ.130.0D0) VNAME='AI     '
              IF(VARABL(I,1).EQ.131.0D0) VNAME='AJ     '
              IF(VARABL(I,1).EQ.132.0D0) VNAME='AK     '
              IF(VARABL(I,1).EQ.133.0D0) VNAME='AL     '
              IF(VARABL(I,1).EQ.134.0D0) VNAME='ZD     '
              IF(VARABL(I,1).EQ.135.0D0) VNAME='INDEX  '
              IF(VARABL(I,1).EQ.136.0D0) VNAME='VNUM   '
              IF(VARABL(I,1).EQ.137.0D0) VNAME='PIVX   '
              IF(VARABL(I,1).EQ.138.0D0) VNAME='PIVY   '
              IF(VARABL(I,1).EQ.139.0D0) VNAME='PIVZ   '
              IF(VARABL(I,1).EQ.140.0D0) VNAME='DPART  '
              IF(VARABL(I,1).EQ.141.0D0) VNAME='CLPX   '
              IF(VARABL(I,1).EQ.142.0D0) VNAME='CLPY   '
              IF(VARABL(I,1).EQ.143.0D0) VNAME='GDX    '
              IF(VARABL(I,1).EQ.144.0D0) VNAME='GDY    '
              IF(VARABL(I,1).EQ.145.0D0) VNAME='GDZ    '
              IF(VARABL(I,1).EQ.146.0D0) VNAME='GALPHA '
              IF(VARABL(I,1).EQ.147.0D0) VNAME='GBETA  '
              IF(VARABL(I,1).EQ.148.0D0) VNAME='GGAMMA '
              IF(VARABL(I,1).EQ.149.0D0) VNAME='GRS    '
              IF(VARABL(I,1).EQ.150.0D0) VNAME='MACVAR '
              IF(VARABL(I,1).GT.249.0D0.AND.VARABL(I,1).LT.4219.0D0) THEN
C     VNAME ACT0001 TO ACT3969 HAVE NUMBERS 250 TO 4218
                  TAG4=INT(VARABL(I,1))-249
                  CALL ITOA4(TAGNAM,TAG4)
                  VNAME='ACT'//TAGNAM
              END IF
C       NOW NSS DATABASE VARIABLES
              IF(VARABL(I,1).EQ.4219.0D0) VNAME='NSSXPOS'
              IF(VARABL(I,1).EQ.4220.0D0) VNAME='NSSYPOS'
              IF(VARABL(I,1).EQ.4221.0D0) VNAME='NSSXPOS'
              IF(VARABL(I,1).EQ.4222.0D0) VNAME='NSSALPH'
              IF(VARABL(I,1).EQ.4223.0D0) VNAME='NSSBETA'
              IF(VARABL(I,1).EQ.4224.0D0) VNAME='NSSGAMM'
              IF(VARABL(I,1).EQ.4225.0D0) VNAME='V1     '
              IF(VARABL(I,1).EQ.4226.0D0) VNAME='V2     '
              IF(VARABL(I,1).EQ.4227.0D0) VNAME='V3     '
              IF(VARABL(I,1).EQ.4228.0D0) VNAME='V4     '
              IF(VARABL(I,1).EQ.4229.0D0) VNAME='V5     '
              IF(VARABL(I,1).GT.4229.0D0.AND.VARABL(I,1).LT.4430.0D0) THEN
C     VNAME P0001 TO P0200 HAVE NUMBERS 4230 TO 4439
                  TAG4=INT(VARABL(I,1))-4229
                  CALL ITOA4(TAGNAM,TAG4)
                  IF(TAG4.LT.10) VNAME='PAR'//TAGNAM(4:4)//' '
                  IF(TAG4.GE.10.AND.TAG4.LT.100) VNAME='PAR'//TAGNAM(3:4)//' '
                  IF(TAG4.GE.100.AND.TAG4.LT.999) VNAME='PAR'//TAGNAM(2:4)//' '
              END IF
              IF(WC.EQ.'VBA') THEN
C     DO VBA HEADER PRINTING
C     PRINT VBA DATA
                  WRITE(OUTLYNE,200)INT(W1)
                  CALL SHOWIT(0)
 200              FORMAT(
     1            'CURRENT AUXILLIARY VARIABLE DATA (VBA) FOR VARIABLE #',I6)
                  WRITE(OUTLYNE,201)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2011)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,202) VNAME,
     1            INT(VARABL(INT(W1),3)),INT(VARABL(INT(W1),2)),
     2            VARABL(INT(W1),9),VARABL(INT(W1),10)
     2            ,VARABL(INT(W1),7),(VARABL(INT(W1),8)*DINMUL)
                  CALL SHOWIT(0)
 201              FORMAT('VARIABLE',2X,'SURF#',2X,'CFG#',2X,
     1            'MIN. LIMIT',4X,'MAX. LIMIT',6X,'WEIGHT',7X,'DINCR*DINMUL')
 2011             FORMAT(9X,'(REG#)')
 202              FORMAT(A7,2X,I6,3X,I2,3X,G12.5,2X,G12.5,2X,G12.5,2X,G12.5)
                  RETURN
              END IF
              IF(WC.EQ.'VB') THEN
C     DO VB HEADER PRINTING
C     PRINT VB DATA
                  WRITE(OUTLYNE,100) INT(W1)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1011)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102) INT(W1),VNAME,
     1            INT(VARABL(INT(W1),3)),INT(VARABL(INT(W1),2)),
     2            VARABL(INT(W1),4),VARABL(INT(W1),6)
                  CALL SHOWIT(0)
 100              FORMAT(
     1            'CURRENT VARIABLE DATA (VB) FOR VARIABLE #',I6)
 101              FORMAT('VAR #',1X,'VARIABLE',2X,'SURF#',1X,'CFG#',
     1            3X,'CURRENT VALUE',2X,'LAST CHANGE')
 1011             FORMAT(15X,'(REG#)')
 102              FORMAT(I6,1X,A7,1X,I6,1X,I2,5X,G13.6,2X,G13.6)
                  RETURN
              END IF
          ELSE
C     DF1=1, NO NUMERIC INPUT OF CONFIG OUTPUT, DO ALL OUTPUT
C     HERE DO THE VARIABLE OUTPUT FOR ALL VARIABLES
C
              IF(WC.EQ.'VBA') THEN
C     DO VBA HEADER PRINTING
                  WRITE(OUTLYNE,300)
                  CALL SHOWIT(0)
 300              FORMAT(
     1            'CURRENT AUXILLIARY VARIABLE DATA (VBA)')
                  IF(CFGER) WRITE(OUTLYNE,301) TAGER
                  IF(CFGER) CALL SHOWIT(0)
 301              FORMAT(
     1            'ACTIVE IN CONFIGURATION # ',I2)
                  WRITE(OUTLYNE,201)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2011)
                  CALL SHOWIT(0)
                  DO I=1,VBCNT
                      IF(VARABL(I,1).EQ.1.0D0)   VNAME='CV    '
                      IF(VARABL(I,1).EQ.2.0D0)   VNAME='CV    '
                      IF(VARABL(I,1).EQ.3.0D0)   VNAME='TH    '
                      IF(VARABL(I,1).EQ.4.0D0)   VNAME='CC    '
                      IF(VARABL(I,1).EQ.5.0D0)   VNAME='AD    '
                      IF(VARABL(I,1).EQ.6.0D0)   VNAME='AE    '
                      IF(VARABL(I,1).EQ.7.0D0)   VNAME='AF    '
                      IF(VARABL(I,1).EQ.8.0D0)   VNAME='AG    '
                      IF(VARABL(I,1).EQ.9.0D0)   VNAME='CVTOR '
                      IF(VARABL(I,1).EQ.10.0D0)  VNAME='CVTOR '
                      IF(VARABL(I,1).EQ.11.0D0)  VNAME='CCTOR '
                      IF(VARABL(I,1).EQ.12.0D0)  VNAME='ADTOR '
                      IF(VARABL(I,1).EQ.13.0D0)  VNAME='AETOR '
                      IF(VARABL(I,1).EQ.14.0D0)  VNAME='AFTOR '
                      IF(VARABL(I,1).EQ.15.0D0)  VNAME='AGTOR '
                      IF(VARABL(I,1).EQ.16.0D0)  VNAME='ALPHA '
                      IF(VARABL(I,1).EQ.17.0D0)  VNAME='BETA  '
                      IF(VARABL(I,1).EQ.18.0D0)  VNAME='GAMMA '
                      IF(VARABL(I,1).EQ.19.0D0)  VNAME='XD    '
                      IF(VARABL(I,1).EQ.20.0D0)  VNAME='YD    '
                      IF(VARABL(I,1).EQ.21.0D0)  VNAME='N1    '
                      IF(VARABL(I,1).EQ.22.0D0)  VNAME='N2    '
                      IF(VARABL(I,1).EQ.23.0D0)  VNAME='N3    '
                      IF(VARABL(I,1).EQ.24.0D0)  VNAME='N4    '
                      IF(VARABL(I,1).EQ.25.0D0)  VNAME='N5    '
                      IF(VARABL(I,1).EQ.27.0D0)  VNAME='C1    '
                      IF(VARABL(I,1).EQ.28.0D0)  VNAME='C2    '
                      IF(VARABL(I,1).EQ.29.0D0)  VNAME='C3    '
                      IF(VARABL(I,1).EQ.30.0D0)  VNAME='C4    '
                      IF(VARABL(I,1).EQ.31.0D0)  VNAME='C5    '
                      IF(VARABL(I,1).EQ.32.0D0)  VNAME='C6    '
                      IF(VARABL(I,1).EQ.33.0D0)  VNAME='C7    '
                      IF(VARABL(I,1).EQ.34.0D0)  VNAME='C8    '
                      IF(VARABL(I,1).EQ.35.0D0)  VNAME='C9    '
                      IF(VARABL(I,1).EQ.36.0D0)  VNAME='C10   '
                      IF(VARABL(I,1).EQ.37.0D0)  VNAME='C11   '
                      IF(VARABL(I,1).EQ.38.0D0)  VNAME='C12   '
                      IF(VARABL(I,1).EQ.39.0D0)  VNAME='C13   '
                      IF(VARABL(I,1).EQ.40.0D0)  VNAME='C14   '
                      IF(VARABL(I,1).EQ.41.0D0)  VNAME='C15   '
                      IF(VARABL(I,1).EQ.42.0D0)  VNAME='C16   '
                      IF(VARABL(I,1).EQ.43.0D0)  VNAME='C17   '
                      IF(VARABL(I,1).EQ.44.0D0)  VNAME='C18   '
                      IF(VARABL(I,1).EQ.45.0D0)  VNAME='C19   '
                      IF(VARABL(I,1).EQ.46.0D0)  VNAME='C20   '
                      IF(VARABL(I,1).EQ.47.0D0)  VNAME='C21   '
                      IF(VARABL(I,1).EQ.48.0D0)  VNAME='C22   '
                      IF(VARABL(I,1).EQ.49.0D0)  VNAME='C23   '
                      IF(VARABL(I,1).EQ.50.0D0)  VNAME='C24   '
                      IF(VARABL(I,1).EQ.51.0D0)  VNAME='C25   '
                      IF(VARABL(I,1).EQ.52.0D0)  VNAME='C26   '
                      IF(VARABL(I,1).EQ.53.0D0)  VNAME='C27   '
                      IF(VARABL(I,1).EQ.54.0D0)  VNAME='C28   '
                      IF(VARABL(I,1).EQ.55.0D0)  VNAME='C29   '
                      IF(VARABL(I,1).EQ.56.0D0)  VNAME='C30   '
                      IF(VARABL(I,1).EQ.57.0D0)  VNAME='C31   '
                      IF(VARABL(I,1).EQ.58.0D0)  VNAME='C32   '
                      IF(VARABL(I,1).EQ.59.0D0)  VNAME='C33   '
                      IF(VARABL(I,1).EQ.60.0D0)  VNAME='C34   '
                      IF(VARABL(I,1).EQ.61.0D0)  VNAME='C35   '
                      IF(VARABL(I,1).EQ.62.0D0)  VNAME='C36   '
                      IF(VARABL(I,1).EQ.63.0D0)  VNAME='C37   '
                      IF(VARABL(I,1).EQ.64.0D0)  VNAME='C38   '
                      IF(VARABL(I,1).EQ.65.0D0)  VNAME='C39   '
                      IF(VARABL(I,1).EQ.66.0D0)  VNAME='C40   '
                      IF(VARABL(I,1).EQ.67.0D0)  VNAME='C41   '
                      IF(VARABL(I,1).EQ.68.0D0)  VNAME='C42   '
                      IF(VARABL(I,1).EQ.69.0D0)  VNAME='C43   '
                      IF(VARABL(I,1).EQ.70.0D0)  VNAME='C44   '
                      IF(VARABL(I,1).EQ.71.0D0)  VNAME='C45   '
                      IF(VARABL(I,1).EQ.72.0D0)  VNAME='C46   '
                      IF(VARABL(I,1).EQ.73.0D0)  VNAME='C47   '
                      IF(VARABL(I,1).EQ.74.0D0)  VNAME='C48   '
                      IF(VARABL(I,1).EQ.75.0D0)  VNAME='AC    '
                      IF(VARABL(I,1).EQ.76.0D0)  VNAME='C49   '
                      IF(VARABL(I,1).EQ.77.0D0)  VNAME='C50   '
                      IF(VARABL(I,1).EQ.78.0D0)  VNAME='C51   '
                      IF(VARABL(I,1).EQ.79.0D0)  VNAME='C52   '
                      IF(VARABL(I,1).EQ.80.0D0)  VNAME='C53   '
                      IF(VARABL(I,1).EQ.81.0D0)  VNAME='C54   '
                      IF(VARABL(I,1).EQ.82.0D0)  VNAME='C55   '
                      IF(VARABL(I,1).EQ.83.0D0)  VNAME='C56   '
                      IF(VARABL(I,1).EQ.84.0D0)  VNAME='C57   '
                      IF(VARABL(I,1).EQ.85.0D0)  VNAME='C58   '
                      IF(VARABL(I,1).EQ.86.0D0)  VNAME='C59   '
                      IF(VARABL(I,1).EQ.87.0D0)  VNAME='C60   '
                      IF(VARABL(I,1).EQ.88.0D0)  VNAME='C61   '
                      IF(VARABL(I,1).EQ.89.0D0)  VNAME='C62   '
                      IF(VARABL(I,1).EQ.90.0D0)  VNAME='C63   '
                      IF(VARABL(I,1).EQ.91.0D0)  VNAME='C64   '
                      IF(VARABL(I,1).EQ.92.0D0)  VNAME='C65   '
                      IF(VARABL(I,1).EQ.93.0D0)  VNAME='C66   '
                      IF(VARABL(I,1).EQ.94.0D0)  VNAME='C67   '
                      IF(VARABL(I,1).EQ.95.0D0)  VNAME='C68   '
                      IF(VARABL(I,1).EQ.96.0D0)  VNAME='C69   '
                      IF(VARABL(I,1).EQ.97.0D0)  VNAME='C70   '
                      IF(VARABL(I,1).EQ.98.0D0)  VNAME='C71   '
                      IF(VARABL(I,1).EQ.99.0D0)  VNAME='C72   '
                      IF(VARABL(I,1).EQ.100.0D0) VNAME='C73   '
                      IF(VARABL(I,1).EQ.101.0D0) VNAME='C74   '
                      IF(VARABL(I,1).EQ.102.0D0) VNAME='C75   '
                      IF(VARABL(I,1).EQ.103.0D0) VNAME='C76   '
                      IF(VARABL(I,1).EQ.104.0D0) VNAME='C77   '
                      IF(VARABL(I,1).EQ.105.0D0) VNAME='C78   '
                      IF(VARABL(I,1).EQ.106.0D0) VNAME='C79   '
                      IF(VARABL(I,1).EQ.107.0D0) VNAME='C80   '
                      IF(VARABL(I,1).EQ.108.0D0) VNAME='C81   '
                      IF(VARABL(I,1).EQ.109.0D0) VNAME='C82   '
                      IF(VARABL(I,1).EQ.110.0D0) VNAME='C83   '
                      IF(VARABL(I,1).EQ.111.0D0) VNAME='C84   '
                      IF(VARABL(I,1).EQ.112.0D0) VNAME='C85   '
                      IF(VARABL(I,1).EQ.113.0D0) VNAME='C86   '
                      IF(VARABL(I,1).EQ.114.0D0) VNAME='C87   '
                      IF(VARABL(I,1).EQ.115.0D0) VNAME='C88   '
                      IF(VARABL(I,1).EQ.116.0D0) VNAME='C89   '
                      IF(VARABL(I,1).EQ.117.0D0) VNAME='C90   '
                      IF(VARABL(I,1).EQ.118.0D0) VNAME='C91   '
                      IF(VARABL(I,1).EQ.119.0D0) VNAME='C92   '
                      IF(VARABL(I,1).EQ.120.0D0) VNAME='C93   '
                      IF(VARABL(I,1).EQ.121.0D0) VNAME='C94   '
                      IF(VARABL(I,1).EQ.122.0D0) VNAME='C95   '
                      IF(VARABL(I,1).EQ.123.0D0) VNAME='C96   '
                      IF(VARABL(I,1).EQ.124.0D0) VNAME='N6    '
                      IF(VARABL(I,1).EQ.125.0D0) VNAME='N7    '
                      IF(VARABL(I,1).EQ.126.0D0) VNAME='N8    '
                      IF(VARABL(I,1).EQ.127.0D0) VNAME='N9    '
                      IF(VARABL(I,1).EQ.128.0D0) VNAME='N10   '
                      IF(VARABL(I,1).EQ.129.0D0) VNAME='AH    '
                      IF(VARABL(I,1).EQ.130.0D0) VNAME='AI    '
                      IF(VARABL(I,1).EQ.131.0D0) VNAME='AJ    '
                      IF(VARABL(I,1).EQ.132.0D0) VNAME='AK    '
                      IF(VARABL(I,1).EQ.133.0D0) VNAME='AL    '
                      IF(VARABL(I,1).EQ.134.0D0) VNAME='ZD    '
                      IF(VARABL(I,1).EQ.135.0D0) VNAME='INDEX '
                      IF(VARABL(I,1).EQ.136.0D0) VNAME='VNUM  '
                      IF(VARABL(I,1).EQ.137.0D0) VNAME='PIVX  '
                      IF(VARABL(I,1).EQ.138.0D0) VNAME='PIVY  '
                      IF(VARABL(I,1).EQ.139.0D0) VNAME='PIVZ  '
                      IF(VARABL(I,1).EQ.140.0D0) VNAME='DPART '
                      IF(VARABL(I,1).EQ.141.0D0) VNAME='CLPX  '
                      IF(VARABL(I,1).EQ.142.0D0) VNAME='CLPY  '
                      IF(VARABL(I,1).EQ.143.0D0) VNAME='GDX   '
                      IF(VARABL(I,1).EQ.144.0D0) VNAME='GDY   '
                      IF(VARABL(I,1).EQ.145.0D0) VNAME='GDZ   '
                      IF(VARABL(I,1).EQ.146.0D0) VNAME='GALPHA'
                      IF(VARABL(I,1).EQ.147.0D0) VNAME='GBETA '
                      IF(VARABL(I,1).EQ.148.0D0) VNAME='GGAMMA'
                      IF(VARABL(I,1).EQ.149.0D0) VNAME='GRS   '
                      IF(VARABL(I,1).EQ.150.0D0) VNAME='MACVAR'
                      IF(VARABL(I,1).GT.249.0D0.AND.VARABL(I,1).LT.4219.0D0) THEN
C     VNAME ACT0001 TO ACT3969 HAVE NUMBERS 250 TO 4218
                          TAG4=INT(VARABL(I,1))-249
                          CALL ITOA4(TAGNAM,TAG4)
                          VNAME='ACT'//TAGNAM
                      END IF
C       NOW NSS DATABASE VARIABLES
                      IF(VARABL(I,1).EQ.4219.0D0) VNAME='NSSXPOS'
                      IF(VARABL(I,1).EQ.4220.0D0) VNAME='NSSYPOS'
                      IF(VARABL(I,1).EQ.4221.0D0) VNAME='NSSXPOS'
                      IF(VARABL(I,1).EQ.4222.0D0) VNAME='NSSALPH'
                      IF(VARABL(I,1).EQ.4223.0D0) VNAME='NSSBETA'
                      IF(VARABL(I,1).EQ.4224.0D0) VNAME='NSSGAMM'
                      IF(VARABL(I,1).EQ.4225.0D0) VNAME='V1     '
                      IF(VARABL(I,1).EQ.4226.0D0) VNAME='V2     '
                      IF(VARABL(I,1).EQ.4227.0D0) VNAME='V3     '
                      IF(VARABL(I,1).EQ.4228.0D0) VNAME='V4     '
                      IF(VARABL(I,1).EQ.4229.0D0) VNAME='V5     '
                      IF(VARABL(I,1).GT.4229.0D0.AND.VARABL(I,1).LT.4430.0D0) THEN
C     VNAME P0001 TO P0200 HAVE NUMBERS 4230 TO 4439
                          TAG4=INT(VARABL(I,1))-4229
                          CALL ITOA4(TAGNAM,TAG4)
                          IF(TAG4.LT.10) VNAME='PAR'//TAGNAM(4:4)//' '
                          IF(TAG4.GE.10.AND.TAG4.LT.100) VNAME='PAR'//TAGNAM(3:4)//' '
                          IF(TAG4.GE.100.AND.TAG4.LT.999) VNAME='PAR'//TAGNAM(2:4)//' '
                      END IF
                      IF(ALLER.OR.CFGER.AND.INT(VARABL(I,2)).EQ.TAGER) THEN
                          WRITE(OUTLYNE,202) VNAME,
     1                    INT(VARABL(I,3)),INT(VARABL(I,2)),
     2                    VARABL(I,9),VARABL(I,10)
     2                    ,VARABL(I,7),(VARABL(I,8)*DINMUL)
                          CALL SHOWIT(0)
                      END IF
                  END DO
                  RETURN
              END IF
              IF(WC.EQ.'VB') THEN
                  WRITE(OUTLYNE,400)
                  IF(CFGER) WRITE(OUTLYNE,301) TAGER
                  IF(CFGER) CALL SHOWIT(0)
                  WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1011)
                  CALL SHOWIT(0)
 400              FORMAT(
     1            'CURRENT VARIABLE DATA (VB)')
C     DO VB HEADER PRINTING
                  DO I=1,VBCNT
                      IF(VARABL(I,1).EQ.1.0D0)   VNAME='CV    '
                      IF(VARABL(I,1).EQ.2.0D0)   VNAME='CV    '
                      IF(VARABL(I,1).EQ.3.0D0)   VNAME='TH    '
                      IF(VARABL(I,1).EQ.4.0D0)   VNAME='CC    '
                      IF(VARABL(I,1).EQ.5.0D0)   VNAME='AD    '
                      IF(VARABL(I,1).EQ.6.0D0)   VNAME='AE    '
                      IF(VARABL(I,1).EQ.7.0D0)   VNAME='AF    '
                      IF(VARABL(I,1).EQ.8.0D0)   VNAME='AG    '
                      IF(VARABL(I,1).EQ.9.0D0)   VNAME='CVTOR '
                      IF(VARABL(I,1).EQ.10.0D0)  VNAME='CVTOR '
                      IF(VARABL(I,1).EQ.11.0D0)  VNAME='CCTOR '
                      IF(VARABL(I,1).EQ.12.0D0)  VNAME='ADTOR '
                      IF(VARABL(I,1).EQ.13.0D0)  VNAME='AETOR '
                      IF(VARABL(I,1).EQ.14.0D0)  VNAME='AFTOR '
                      IF(VARABL(I,1).EQ.15.0D0)  VNAME='AGTOR '
                      IF(VARABL(I,1).EQ.16.0D0)  VNAME='ALPHA '
                      IF(VARABL(I,1).EQ.17.0D0)  VNAME='BETA  '
                      IF(VARABL(I,1).EQ.18.0D0)  VNAME='GAMMA '
                      IF(VARABL(I,1).EQ.19.0D0)  VNAME='XD    '
                      IF(VARABL(I,1).EQ.20.0D0)  VNAME='YD    '
                      IF(VARABL(I,1).EQ.21.0D0)  VNAME='N1    '
                      IF(VARABL(I,1).EQ.22.0D0)  VNAME='N2    '
                      IF(VARABL(I,1).EQ.23.0D0)  VNAME='N3    '
                      IF(VARABL(I,1).EQ.24.0D0)  VNAME='N4    '
                      IF(VARABL(I,1).EQ.25.0D0)  VNAME='N5    '
                      IF(VARABL(I,1).EQ.27.0D0)  VNAME='C1    '
                      IF(VARABL(I,1).EQ.28.0D0)  VNAME='C2    '
                      IF(VARABL(I,1).EQ.29.0D0)  VNAME='C3    '
                      IF(VARABL(I,1).EQ.30.0D0)  VNAME='C4    '
                      IF(VARABL(I,1).EQ.31.0D0)  VNAME='C5    '
                      IF(VARABL(I,1).EQ.32.0D0)  VNAME='C6    '
                      IF(VARABL(I,1).EQ.33.0D0)  VNAME='C7    '
                      IF(VARABL(I,1).EQ.34.0D0)  VNAME='C8    '
                      IF(VARABL(I,1).EQ.35.0D0)  VNAME='C9    '
                      IF(VARABL(I,1).EQ.36.0D0)  VNAME='C10   '
                      IF(VARABL(I,1).EQ.37.0D0)  VNAME='C11   '
                      IF(VARABL(I,1).EQ.38.0D0)  VNAME='C12   '
                      IF(VARABL(I,1).EQ.39.0D0)  VNAME='C13   '
                      IF(VARABL(I,1).EQ.40.0D0)  VNAME='C14   '
                      IF(VARABL(I,1).EQ.41.0D0)  VNAME='C15   '
                      IF(VARABL(I,1).EQ.42.0D0)  VNAME='C16   '
                      IF(VARABL(I,1).EQ.43.0D0)  VNAME='C17   '
                      IF(VARABL(I,1).EQ.44.0D0)  VNAME='C18   '
                      IF(VARABL(I,1).EQ.45.0D0)  VNAME='C19   '
                      IF(VARABL(I,1).EQ.46.0D0)  VNAME='C20   '
                      IF(VARABL(I,1).EQ.47.0D0)  VNAME='C21   '
                      IF(VARABL(I,1).EQ.48.0D0)  VNAME='C22   '
                      IF(VARABL(I,1).EQ.49.0D0)  VNAME='C23   '
                      IF(VARABL(I,1).EQ.50.0D0)  VNAME='C24   '
                      IF(VARABL(I,1).EQ.51.0D0)  VNAME='C25   '
                      IF(VARABL(I,1).EQ.52.0D0)  VNAME='C26   '
                      IF(VARABL(I,1).EQ.53.0D0)  VNAME='C27   '
                      IF(VARABL(I,1).EQ.54.0D0)  VNAME='C28   '
                      IF(VARABL(I,1).EQ.55.0D0)  VNAME='C29   '
                      IF(VARABL(I,1).EQ.56.0D0)  VNAME='C30   '
                      IF(VARABL(I,1).EQ.57.0D0)  VNAME='C31   '
                      IF(VARABL(I,1).EQ.58.0D0)  VNAME='C32   '
                      IF(VARABL(I,1).EQ.59.0D0)  VNAME='C33   '
                      IF(VARABL(I,1).EQ.60.0D0)  VNAME='C34   '
                      IF(VARABL(I,1).EQ.61.0D0)  VNAME='C35   '
                      IF(VARABL(I,1).EQ.62.0D0)  VNAME='C36   '
                      IF(VARABL(I,1).EQ.63.0D0)  VNAME='C37   '
                      IF(VARABL(I,1).EQ.64.0D0)  VNAME='C38   '
                      IF(VARABL(I,1).EQ.65.0D0)  VNAME='C39   '
                      IF(VARABL(I,1).EQ.66.0D0)  VNAME='C40   '
                      IF(VARABL(I,1).EQ.67.0D0)  VNAME='C41   '
                      IF(VARABL(I,1).EQ.68.0D0)  VNAME='C42   '
                      IF(VARABL(I,1).EQ.69.0D0)  VNAME='C43   '
                      IF(VARABL(I,1).EQ.70.0D0)  VNAME='C44   '
                      IF(VARABL(I,1).EQ.71.0D0)  VNAME='C45   '
                      IF(VARABL(I,1).EQ.72.0D0)  VNAME='C46   '
                      IF(VARABL(I,1).EQ.73.0D0)  VNAME='C47   '
                      IF(VARABL(I,1).EQ.74.0D0)  VNAME='C48   '
                      IF(VARABL(I,1).EQ.75.0D0)  VNAME='AC    '
                      IF(VARABL(I,1).EQ.76.0D0)  VNAME='C49   '
                      IF(VARABL(I,1).EQ.77.0D0)  VNAME='C50   '
                      IF(VARABL(I,1).EQ.78.0D0)  VNAME='C51   '
                      IF(VARABL(I,1).EQ.79.0D0)  VNAME='C52   '
                      IF(VARABL(I,1).EQ.80.0D0)  VNAME='C53   '
                      IF(VARABL(I,1).EQ.81.0D0)  VNAME='C54   '
                      IF(VARABL(I,1).EQ.82.0D0)  VNAME='C55   '
                      IF(VARABL(I,1).EQ.83.0D0)  VNAME='C56   '
                      IF(VARABL(I,1).EQ.84.0D0)  VNAME='C57   '
                      IF(VARABL(I,1).EQ.85.0D0)  VNAME='C58   '
                      IF(VARABL(I,1).EQ.86.0D0)  VNAME='C59   '
                      IF(VARABL(I,1).EQ.87.0D0)  VNAME='C60   '
                      IF(VARABL(I,1).EQ.88.0D0)  VNAME='C61   '
                      IF(VARABL(I,1).EQ.89.0D0)  VNAME='C62   '
                      IF(VARABL(I,1).EQ.90.0D0)  VNAME='C63   '
                      IF(VARABL(I,1).EQ.91.0D0)  VNAME='C64   '
                      IF(VARABL(I,1).EQ.92.0D0)  VNAME='C65   '
                      IF(VARABL(I,1).EQ.93.0D0)  VNAME='C66   '
                      IF(VARABL(I,1).EQ.94.0D0)  VNAME='C67   '
                      IF(VARABL(I,1).EQ.95.0D0)  VNAME='C68   '
                      IF(VARABL(I,1).EQ.96.0D0)  VNAME='C69   '
                      IF(VARABL(I,1).EQ.97.0D0)  VNAME='C70   '
                      IF(VARABL(I,1).EQ.98.0D0)  VNAME='C71   '
                      IF(VARABL(I,1).EQ.99.0D0)  VNAME='C72   '
                      IF(VARABL(I,1).EQ.100.0D0) VNAME='C73   '
                      IF(VARABL(I,1).EQ.101.0D0) VNAME='C74   '
                      IF(VARABL(I,1).EQ.102.0D0) VNAME='C75   '
                      IF(VARABL(I,1).EQ.103.0D0) VNAME='C76   '
                      IF(VARABL(I,1).EQ.104.0D0) VNAME='C77   '
                      IF(VARABL(I,1).EQ.105.0D0) VNAME='C78   '
                      IF(VARABL(I,1).EQ.106.0D0) VNAME='C79   '
                      IF(VARABL(I,1).EQ.107.0D0) VNAME='C80   '
                      IF(VARABL(I,1).EQ.108.0D0) VNAME='C81   '
                      IF(VARABL(I,1).EQ.109.0D0) VNAME='C82   '
                      IF(VARABL(I,1).EQ.110.0D0) VNAME='C83   '
                      IF(VARABL(I,1).EQ.111.0D0) VNAME='C84   '
                      IF(VARABL(I,1).EQ.112.0D0) VNAME='C85   '
                      IF(VARABL(I,1).EQ.113.0D0) VNAME='C86   '
                      IF(VARABL(I,1).EQ.114.0D0) VNAME='C87   '
                      IF(VARABL(I,1).EQ.115.0D0) VNAME='C88   '
                      IF(VARABL(I,1).EQ.116.0D0) VNAME='C89   '
                      IF(VARABL(I,1).EQ.117.0D0) VNAME='C90   '
                      IF(VARABL(I,1).EQ.118.0D0) VNAME='C91   '
                      IF(VARABL(I,1).EQ.119.0D0) VNAME='C92   '
                      IF(VARABL(I,1).EQ.120.0D0) VNAME='C93   '
                      IF(VARABL(I,1).EQ.121.0D0) VNAME='C94   '
                      IF(VARABL(I,1).EQ.122.0D0) VNAME='C95   '
                      IF(VARABL(I,1).EQ.123.0D0) VNAME='C96   '
                      IF(VARABL(I,1).EQ.124.0D0) VNAME='N6    '
                      IF(VARABL(I,1).EQ.125.0D0) VNAME='N7    '
                      IF(VARABL(I,1).EQ.126.0D0) VNAME='N8    '
                      IF(VARABL(I,1).EQ.127.0D0) VNAME='N9    '
                      IF(VARABL(I,1).EQ.128.0D0) VNAME='N10   '
                      IF(VARABL(I,1).EQ.129.0D0) VNAME='AH    '
                      IF(VARABL(I,1).EQ.130.0D0) VNAME='AI    '
                      IF(VARABL(I,1).EQ.131.0D0) VNAME='AJ    '
                      IF(VARABL(I,1).EQ.132.0D0) VNAME='AK    '
                      IF(VARABL(I,1).EQ.133.0D0) VNAME='AL    '
                      IF(VARABL(I,1).EQ.134.0D0) VNAME='ZD    '
                      IF(VARABL(I,1).EQ.135.0D0) VNAME='INDEX '
                      IF(VARABL(I,1).EQ.136.0D0) VNAME='VNUM  '
                      IF(VARABL(I,1).EQ.137.0D0) VNAME='PIVX  '
                      IF(VARABL(I,1).EQ.138.0D0) VNAME='PIVY  '
                      IF(VARABL(I,1).EQ.139.0D0) VNAME='PIVZ  '
                      IF(VARABL(I,1).EQ.140.0D0) VNAME='DPART '
                      IF(VARABL(I,1).EQ.141.0D0) VNAME='CLPX  '
                      IF(VARABL(I,1).EQ.142.0D0) VNAME='CLPY  '
                      IF(VARABL(I,1).EQ.143.0D0) VNAME='GDX   '
                      IF(VARABL(I,1).EQ.144.0D0) VNAME='GDY   '
                      IF(VARABL(I,1).EQ.145.0D0) VNAME='GDZ   '
                      IF(VARABL(I,1).EQ.146.0D0) VNAME='GALPHA'
                      IF(VARABL(I,1).EQ.147.0D0) VNAME='GBETA '
                      IF(VARABL(I,1).EQ.148.0D0) VNAME='GGAMMA'
                      IF(VARABL(I,1).EQ.149.0D0) VNAME='GRS   '
                      IF(VARABL(I,1).EQ.150.0D0) VNAME='MACVAR'
                      IF(VARABL(I,1).GT.249.0D0.AND.VARABL(I,1).LT.4219.0D0) THEN
C     VNAME ACT0001 TO ACT3969 HAVE NUMBERS 250 TO 4218
                          TAG4=INT(VARABL(I,1))-249
                          CALL ITOA4(TAGNAM,TAG4)
                          VNAME='ACT'//TAGNAM
                      END IF
C       NOW NSS DATABASE VARIABLES
                      IF(VARABL(I,1).EQ.4219.0D0) VNAME='NSSXPOS'
                      IF(VARABL(I,1).EQ.4220.0D0) VNAME='NSSYPOS'
                      IF(VARABL(I,1).EQ.4221.0D0) VNAME='NSSXPOS'
                      IF(VARABL(I,1).EQ.4222.0D0) VNAME='NSSALPH'
                      IF(VARABL(I,1).EQ.4223.0D0) VNAME='NSSBETA'
                      IF(VARABL(I,1).EQ.4224.0D0) VNAME='NSSGAMM'
                      IF(VARABL(I,1).EQ.4225.0D0) VNAME='V1     '
                      IF(VARABL(I,1).EQ.4226.0D0) VNAME='V2     '
                      IF(VARABL(I,1).EQ.4227.0D0) VNAME='V3     '
                      IF(VARABL(I,1).EQ.4228.0D0) VNAME='V4     '
                      IF(VARABL(I,1).EQ.4229.0D0) VNAME='V5     '
                      IF(VARABL(I,1).GT.4229.0D0.AND.VARABL(I,1).LT.4430.0D0) THEN
C     VNAME P0001 TO P0200 HAVE NUMBERS 4230 TO 4439
                          TAG4=INT(VARABL(I,1))-4229
                          CALL ITOA4(TAGNAM,TAG4)
                          IF(TAG4.LT.10) VNAME='PAR'//TAGNAM(4:4)//' '
                          IF(TAG4.GE.10.AND.TAG4.LT.100) VNAME='PAR'//TAGNAM(3:4)//' '
                          IF(TAG4.GE.100.AND.TAG4.LT.999) VNAME='PAR'//TAGNAM(2:4)//' '
                      END IF
C     PRINT VB DATA
                      IF(ALLER.OR.CFGER.AND.INT(VARABL(I,2)).EQ.TAGER) THEN
                          WRITE(OUTLYNE,102) I,VNAME,
     1                    INT(VARABL(I,3)),INT(VARABL(I,2)),
     2                    VARABL(I,4),VARABL(I,6)
                          CALL SHOWIT(0)
                      END IF
                  END DO
                  RETURN
              END IF
          END IF
          RETURN
      END
