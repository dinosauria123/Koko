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

C       SEVENTH SET OF OPTIMIZATION ROUTINES

C SUB TOPOUT.FOR
      SUBROUTINE TOPOUT
C
          IMPLICIT NONE
C
          INTEGER I,II,TAGER
C
          LOGICAL DESYES,ALLER
C
          LOGICAL YES
C
          CHARACTER DASHER*3
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
C
          ALLER=.TRUE.
          TAGER=0
C
          DASHER='---'
C
          FUNNAM(0)='FUNC00'
          FUNNAM(1)='FUNC01'
          FUNNAM(2)='FUNC02'
          FUNNAM(3)='FUNC03'
          FUNNAM(4)='FUNC04'
          FUNNAM(5)='FUNC05'
          FUNNAM(6)='FUNC06'
          FUNNAM(7)='FUNC07'
          FUNNAM(8)='FUNC08'
          FUNNAM(9)='FUNC09'
          FUNNAM(10)='FUNC10'
C
C       THIS IS SUBROUTINE TOPOUT. THIS IS THE SUBROUTINE WHICH DOES OPERAND
C       OUTPUT COMMANDS AT THE CMD LEVEL AND FROM INSIDE MERIT AND
C       UPDATE MERIT
          IF(WC.EQ.'TOPS') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"TOPS" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'TOPS') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"TOPS" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.0) THEN
              TAGER=INT(W1)
              ALLER=.FALSE.
          END IF
          IF(DF1.EQ.1) THEN
              ALLER=.TRUE.
          END IF
C     NOW TAGER AND ALLER ARE PROBERLY SET
C
          IF(DF1.EQ.0) THEN
C     CHECK FOR A VALID NUMERIC WORD #1 INPUT VALUE
C     AND PERFORM THE OPERATION
              IF(W1.LE.0.0D0.OR.W1.GE.MAXTOP) THEN
                  WRITE(OUTLYNE,*)'TOPER NUMBER BEYOND DEFINED BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
C     W1 IS A VALID NUMBER, PROCEED
              END IF
              IF(.NOT.ISTOP(INT(W1))) THEN
                  WRITE(OUTLYNE,*)'TOPER NUMBER NOT CURRENTLY DEFINED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
C     W1 IS A VALID NUMBER, PROCEED
              END IF
C
C     HERE DO THE OUTPUT FOR OPERAND NUMBER INT(W1)
C       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
              YES=.FALSE.
              DO I=1,MAXTOP
                  IF(ISTOP(I)) YES=.TRUE.
              END DO
              IF(.NOT.YES) TOPCNT=0
              IF(YES) TOPCNT=1
              IF(TOPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO TOPER DATA EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(.NOT.ISTOP(INT(W1))) THEN
                  WRITE(OUTLYNE,*)'TOPER DATA NOT DEFINED FOR TOPER #',INT(W1)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
              IF(WC.EQ.'TOPS') THEN
C     DO TOPS HEADER PRINTING
C     PRINT CRIT DATA
 100              FORMAT(
     1            'CURRENT TOLERANCE OPERAND DATA FOR TOPER #',I2)
                  WRITE(OUTLYNE,100) INT(W1)
                  CALL SHOWIT(0)
                  II=INT(W1)+MAXFOCRIT
C
 101              FORMAT(
     1            'TOPER # ',2X,'TOPER NAME ',6X,
     2            ' NW2 ',8X,' NW3 ',8X,' NW4 ',8X,' NW5')
 401              FORMAT(
     1            27X,
     2            ' (I) ',8X,' (J) ',8X,' (K) ')
                  WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,401)
                  CALL SHOWIT(0)
C
C     NO DEFAULT
 102              FORMAT(I3,11X,A8,2X,G11.4,2X,G11.4,2X,G11.4,2X,G11.4)
C
C     JUST K DEFAULT
 103              FORMAT(I3,11X,A8,2X,G11.4,2X,G11.4,2X,
     1            3X,'-----',5X,G11.4)
C
C     J AND K DEFAULT
 104              FORMAT(I3,11X,A8,2X,G11.4,2X,3X,
     1            '-----',3X,2X,3X,'-----',5X,G11.4)
C
C     I,J AND K DEFAULT
 105              FORMAT(I3,11X,A8,2X,3X,'-----',3X,2X,
     1            3X,'-----',3X,2X,3X,'-----',5X,G11.4)

C     JUST J DEFAULT
 106              FORMAT(I3,11X,A8,2X,G11.4,2X,3X,
     1            '-----',3X,2X,G11.4,2X,G11.4)

C     JUST I DEFAULT
 107              FORMAT(I3,11X,A8,2X,3X,'-----',3X,
     1            2X,G11.4,2X,G11.4,2X,G11.4)

C     I AND K DEFAULT
 108              FORMAT(I3,11X,A8,2X,G11.4,2X,3X,
     1            '-----',3X,2X,3X,'-----',5X,G11.4)

C     I AND J DEFAULT
 109              FORMAT(I3,11X,A8,2X,3X,'-----',3X,
     1            2X,G11.4,2X,3X,'-----',5X,G11.4)
C
C     NO DEFAULTS
                  IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))
     1            .EQ.0.AND.INT(OPERND(II,18)).EQ.0) THEN
                      WRITE(OUTLYNE,102) INT(W1),OPNAM(II),
     1                OPERND(II,8),OPERND(II,9),
     1                OPERND(II,10),OPERND(II,20)
                      CALL SHOWIT(0)
                  END IF
C
C     K DEFAULT
                  IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))
     1            .EQ.1.AND.INT(OPERND(II,18)).EQ.0) THEN
                      WRITE(OUTLYNE,103) INT(W1),OPNAM(II),
     1                OPERND(II,8),OPERND(II,9),OPERND(II,20)
                      CALL SHOWIT(0)
                  END IF
C
C     J AND K DEFAULTS
                  IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))
     1            .EQ.1.AND.INT(OPERND(II,18)).EQ.0) THEN
                      WRITE(OUTLYNE,104) INT(W1),OPNAM(II),
     1                OPERND(II,8),OPERND(II,20)
                      CALL SHOWIT(0)
                  END IF
C
C     I J K DEFAULTS
                  IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))
     1            .EQ.1.AND.INT(OPERND(II,18)).EQ.1) THEN
                      WRITE(OUTLYNE,105) INT(W1),OPNAM(II),OPERND(II,20)
                      CALL SHOWIT(0)
                  END IF
C
C     J DEFAULT
                  IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))
     1            .EQ.0.AND.INT(OPERND(II,18)).EQ.0) THEN
                      WRITE(OUTLYNE,106) INT(W1),OPNAM(II),
     1                OPERND(II,8),
     1                OPERND(II,10),OPERND(II,20)
                      CALL SHOWIT(0)
                  END IF
C
C     I DEFAULT
                  IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))
     1            .EQ.0.AND.INT(OPERND(II,18)).EQ.1) THEN
                      WRITE(OUTLYNE,107) INT(W1),OPNAM(II),
     1                OPERND(II,9),
     1                OPERND(II,10),OPERND(II,20)
                      CALL SHOWIT(0)
                  END IF
C     I AND K DEFAULT
                  IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))
     1            .EQ.1.AND.INT(OPERND(II,18)).EQ.1) THEN
                      WRITE(OUTLYNE,108) INT(W1),OPNAM(II),
     1                OPERND(II,9),OPERND(II,20)
                      CALL SHOWIT(0)
                  END IF
C     I AND J DEFAULT
                  IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))
     1            .EQ.0.AND.INT(OPERND(II,18)).EQ.1) THEN
                      WRITE(OUTLYNE,109) INT(W1),OPNAM(II),
     1                OPERND(II,10),OPERND(II,20)
                      CALL SHOWIT(0)
                  END IF
                  IF(OPERDESC(II)(1:8).NE.'        ')
     1            WRITE(OUTLYNE,1701) OPNAM(II),OPERDESC(II)(1:69)
                  CALL SHOWIT(0)
 1701             FORMAT(A8,'::',A69)
                  RETURN
              END IF
          ELSE
C     DF1=1, NO NUMERIC INPUT, DO ALL OUTPUT
C     HERE DO THE MERIT OUTPUT FOR ALL OPERANDS
C       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
              YES=.FALSE.
              DO I=1,MAXTOP
                  IF(ISTOP(I)) YES=.TRUE.
              END DO
              IF(.NOT.YES) TOPCNT=0
              IF(YES) TOPCNT=1
              IF(TOPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO TOPER DATA EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
              IF(WC.EQ.'TOPS') THEN
                  WRITE(OUTLYNE,400)
                  CALL SHOWIT(0)
 400              FORMAT(
     1            'CURRENT TOPER DATA')
                  WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,401)
                  CALL SHOWIT(0)
C     DO MERIT HEADER PRINTING
                  DO I=1,MAXTOP
                      II=I+MAXFOCRIT
                      IF(ISTOP(I)) THEN
C     WRITE DATA
C
C     NO DEFAULTS
                          IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))
     1                    .EQ.0.AND.INT(OPERND(II,18)).EQ.0) THEN
                              WRITE(OUTLYNE,102) I,OPNAM(II),
     1                        OPERND(II,8),OPERND(II,9),
     1                        OPERND(II,10),OPERND(II,20)
                              CALL SHOWIT(0)
                          END IF
C
C     K DEFAULT
                          IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))
     1                    .EQ.1.AND.INT(OPERND(II,18)).EQ.0) THEN
                              WRITE(OUTLYNE,103) I,OPNAM(II),
     1                        OPERND(II,8),OPERND(II,9),OPERND(II,20)
                              CALL SHOWIT(0)
                          END IF
C
C     J AND K DEFAULTS
                          IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))
     1                    .EQ.1.AND.INT(OPERND(II,18)).EQ.0) THEN
                              WRITE(OUTLYNE,104) I,OPNAM(II),
     1                        OPERND(II,8),OPERND(II,20)
                              CALL SHOWIT(0)
                          END IF
C
C     I J K DEFAULTS
                          IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))
     1                    .EQ.1.AND.INT(OPERND(II,18)).EQ.1) THEN
                              WRITE(OUTLYNE,105) I,OPNAM(II),OPERND(II,20)
                              CALL SHOWIT(0)
                          END IF
C
C     J DEFAULT
                          IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))
     1                    .EQ.0.AND.INT(OPERND(II,18)).EQ.0) THEN
                              WRITE(OUTLYNE,106) I,OPNAM(II),
     1                        OPERND(II,8),
     1                        OPERND(II,10),OPERND(II,20)
                              CALL SHOWIT(0)
                          END IF
C
C     I DEFAULT
                          IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))
     1                    .EQ.0.AND.INT(OPERND(II,18)).EQ.1) THEN
                              WRITE(OUTLYNE,107) I,OPNAM(II),
     1                        OPERND(II,9),
     1                        OPERND(II,10),OPERND(II,20)
                              CALL SHOWIT(0)
                          END IF
C     I AND K DEFAULT
                          IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))
     1                    .EQ.1.AND.INT(OPERND(II,18)).EQ.1) THEN
                              WRITE(OUTLYNE,108) I,OPNAM(II),
     1                        OPERND(II,9),OPERND(II,20)
                              CALL SHOWIT(0)
                          END IF
C     I AND J DEFAULT
                          IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))
     1                    .EQ.0.AND.INT(OPERND(II,18)).EQ.1) THEN
                              WRITE(OUTLYNE,109) I,OPNAM(II),
     1                        OPERND(II,10),OPERND(II,20)
                              CALL SHOWIT(0)
                          END IF
                      END IF
                  END DO
                  DESYES=.FALSE.
                  DO I=1,MAXTOP
                      II=I+MAXFOCRIT
                      IF(OPERDESC(II)(1:8).NE.'        ') THEN
                          DESYES=.TRUE.
                      END IF
                  END DO
                  IF(DESYES) WRITE(OUTLYNE,1703)
                  IF(DESYES) CALL SHOWIT(0)
                  IF(DESYES) WRITE(OUTLYNE,1702)
                  IF(DESYES) CALL SHOWIT(0)
                  IF(DESYES) WRITE(OUTLYNE,1703)
                  IF(DESYES) CALL SHOWIT(0)
 1702             FORMAT('CURRENT TOERANCE OPERAND DESCRIPTIONS')
 1703             FORMAT(1X)
                  DO I=1,MAXTOP
                      II=I+MAXFOCRIT
                      IF(OPERDESC(II)(1:8).NE.'        ')
     1                WRITE(OUTLYNE,1701) OPNAM(II),OPERDESC(II)(1:69)
                      IF(OPERDESC(II)(1:8).NE.'        ')
     1                CALL SHOWIT(0)
                  END DO
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB CRITOUT.FOR
      SUBROUTINE CRITOUT
C
          IMPLICIT NONE
C
          INTEGER I,TAGER
C
          LOGICAL DESYES,ALLER
C
          LOGICAL YES
C
          CHARACTER DASHER*3
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
C
          ALLER=.TRUE.
          TAGER=0
C
          DASHER='---'
C
          FUNNAM(0)='FUNC00'
          FUNNAM(1)='FUNC01'
          FUNNAM(2)='FUNC02'
          FUNNAM(3)='FUNC03'
          FUNNAM(4)='FUNC04'
          FUNNAM(5)='FUNC05'
          FUNNAM(6)='FUNC06'
          FUNNAM(7)='FUNC07'
          FUNNAM(8)='FUNC08'
          FUNNAM(9)='FUNC09'
          FUNNAM(10)='FUNC10'
C
C       THIS IS SUBROUTINE CRITOUT. THIS IS THE SUBROUTINE WHICH DOES OPERAND
C       OUTPUT COMMANDS AT THE CMD LEVEL AND FROM INSIDE MERIT AND
C       UPDATE MERIT
          IF(WC.EQ.'CRITS') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"CRITS" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'CRITS') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"CRITS" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(DF1.EQ.0) THEN
              TAGER=INT(W1)
              ALLER=.FALSE.
          END IF
          IF(DF1.EQ.1) THEN
              ALLER=.TRUE.
          END IF
C     NOW TAGER AND ALLER ARE PROBERLY SET
C
          IF(DF1.EQ.0) THEN
C     CHECK FOR A VALID NUMERIC WORD #1 INPUT VALUE
C     AND PERFORM THE OPERATION
              IF(W1.LE.0.0D0.OR.W1.GT.MAXFOCRIT) THEN
                  WRITE(OUTLYNE,*)'FOCRIT NUMBER BEYOND DEFINED BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
C     W1 IS A VALID NUMBER, PROCEED
              END IF
              IF(.NOT.ISCRIT(INT(W1))) THEN
                  WRITE(OUTLYNE,*)'FOCRIT NUMBER NOT CURRENTLY DEFINED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
C     W1 IS A VALID NUMBER, PROCEED
              END IF
C
C     HERE DO THE OUTPUT FOR OPERAND NUMBER INT(W1)
C       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
              YES=.FALSE.
              DO I=1,MAXFOCRIT
                  IF(ISCRIT(I)) YES=.TRUE.
              END DO
              IF(.NOT.YES) FCCNT=0
              IF(YES) FCCNT=1
              IF(FCCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO FOCRIT DATA EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(.NOT.ISCRIT(INT(W1))) THEN
                  WRITE(OUTLYNE,*)'FOCRIT DATA NOT DEFINED FOR FOCRIT #',INT(W1)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
              IF(WC.EQ.'CRITS') THEN
C     DO CRITS HEADER PRINTING
C     PRINT CRIT DATA
 100              FORMAT(
     1            'CURRENT FOCRIT DATA FOR FOCRIT #',I2)
                  WRITE(OUTLYNE,100) INT(W1)
                  CALL SHOWIT(0)
C
 101              FORMAT(
     1            'FOCRIT #',2X,'FOCRIT NAME',6X,
     2            ' NW2 ',8X,' NW3 ',8X,' NW4 ')
 401              FORMAT(
     1            27X,
     2            ' (I) ',8X,' (J) ',8X,' (K) ')
                  WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,401)
                  CALL SHOWIT(0)
C
C     NO DEFAULT
 102              FORMAT(I3,11X,A8,2X,G11.4,2X,G11.4,2X,G11.4)
C
C     JUST K DEFAULT
 103              FORMAT(I3,11X,A8,2X,G11.4,2X,G11.4,2X,
     1            3X,'-----')
C
C     J AND K DEFAULT
 104              FORMAT(I3,11X,A8,2X,G11.4,2X,3X,
     1            '-----',3X,2X,3X,'-----')
C
C     I,J AND K DEFAULT
 105              FORMAT(I3,11X,A8,2X,3X,'-----',3X,2X,
     1            3X,'-----',3X,2X,3X,'-----')

C     JUST J DEFAULT
 106              FORMAT(I3,11X,A8,2X,G11.4,2X,3X,
     1            '-----',3X,2X,G11.4)

C     JUST I DEFAULT
 107              FORMAT(I3,11X,A8,2X,3X,'-----',3X,
     1            2X,G11.4,2X,G11.4)

C     I AND K DEFAULT
 108              FORMAT(I3,11X,A8,2X,G11.4,2X,3X,
     1            '-----',3X,2X,3X,'-----')

C     I AND J DEFAULT
 109              FORMAT(I3,11X,A8,2X,3X,'-----',3X,
     1            2X,G11.4,2X,3X,'-----')
C
C     NO DEFAULTS
                  IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))
     1            .EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
                      WRITE(OUTLYNE,102) INT(W1),OPNAM(INT(W1)),
     1                OPERND(INT(W1),8),OPERND(INT(W1),9),
     1                OPERND(INT(W1),10)
                      CALL SHOWIT(0)
                  END IF
C
C     K DEFAULT
                  IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))
     1            .EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
                      WRITE(OUTLYNE,103) INT(W1),OPNAM(INT(W1)),
     1                OPERND(INT(W1),8),OPERND(INT(W1),9)
                      CALL SHOWIT(0)
                  END IF
C
C     J AND K DEFAULTS
                  IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))
     1            .EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
                      WRITE(OUTLYNE,104) INT(W1),OPNAM(INT(W1)),
     1                OPERND(INT(W1),8)
                      CALL SHOWIT(0)
                  END IF
C
C     I J K DEFAULTS
                  IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))
     1            .EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
                      WRITE(OUTLYNE,105) INT(W1),OPNAM(INT(W1))
                      CALL SHOWIT(0)
                  END IF
C
C     J DEFAULT
                  IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))
     1            .EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
                      WRITE(OUTLYNE,106) INT(W1),OPNAM(INT(W1)),
     1                OPERND(INT(W1),8),
     1                OPERND(INT(W1),10)
                      CALL SHOWIT(0)
                  END IF
C
C     I DEFAULT
                  IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))
     1            .EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
                      WRITE(OUTLYNE,107) INT(W1),OPNAM(INT(W1)),
     1                OPERND(INT(W1),9),
     1                OPERND(INT(W1),10)
                      CALL SHOWIT(0)
                  END IF
C     I AND K DEFAULT
                  IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))
     1            .EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
                      WRITE(OUTLYNE,108) INT(W1),OPNAM(INT(W1)),
     1                OPERND(INT(W1),9)
                      CALL SHOWIT(0)
                  END IF
C     I AND J DEFAULT
                  IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))
     1            .EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
                      WRITE(OUTLYNE,109) INT(W1),OPNAM(INT(W1)),
     1                OPERND(INT(W1),10)
                      CALL SHOWIT(0)
                  END IF
                  IF(OPERDESC(INT(W1))(1:8).NE.'        ')
     1            WRITE(OUTLYNE,1701) OPNAM(INT(W1)),OPERDESC(INT(W1))(1:69)
                  IF(OPERDESC(INT(W1))(1:8).NE.'        ')
     1            CALL SHOWIT(1)
 1701             FORMAT(A8,'::',A69)
                  RETURN
              END IF
          ELSE
C     DF1=1, NO NUMERIC INPUT, DO ALL OUTPUT
C     HERE DO THE MERIT OUTPUT FOR ALL OPERANDS
C       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
              YES=.FALSE.
              DO I=1,MAXFOCRIT
                  IF(ISCRIT(I)) YES=.TRUE.
              END DO
              IF(.NOT.YES) FCCNT=0
              IF(YES) FCCNT=1
              IF(FCCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO FOCRIT DATA EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
              IF(WC.EQ.'CRITS') THEN
                  WRITE(OUTLYNE,400)
                  CALL SHOWIT(0)
 400              FORMAT(
     1            'CURRENT FOCRIT DATA')
                  WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,401)
                  CALL SHOWIT(0)
C     DO MERIT HEADER PRINTING
                  DO I=1,MAXFOCRIT
                      IF(ISCRIT(I)) THEN
C     WRITE DATA
C
C     NO DEFAULTS
                          IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))
     1                    .EQ.0.AND.INT(OPERND(I,18)).EQ.0) THEN
                              WRITE(OUTLYNE,102) I,OPNAM(I),
     1                        OPERND(I,8),OPERND(I,9),
     1                        OPERND(I,10)
                              CALL SHOWIT(0)
                          END IF
C
C     K DEFAULT
                          IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))
     1                    .EQ.1.AND.INT(OPERND(I,18)).EQ.0) THEN
                              WRITE(OUTLYNE,103) I,OPNAM(I),
     1                        OPERND(I,8),OPERND(I,9)
                              CALL SHOWIT(0)
                          END IF
C
C     J AND K DEFAULTS
                          IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))
     1                    .EQ.1.AND.INT(OPERND(I,18)).EQ.0) THEN
                              WRITE(OUTLYNE,104) I,OPNAM(I),
     1                        OPERND(I,8)
                              CALL SHOWIT(0)
                          END IF
C
C     I J K DEFAULTS
                          IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))
     1                    .EQ.1.AND.INT(OPERND(I,18)).EQ.1) THEN
                              WRITE(OUTLYNE,105) I,OPNAM(I)
                              CALL SHOWIT(0)
                          END IF
C
C     J DEFAULT
                          IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))
     1                    .EQ.0.AND.INT(OPERND(I,18)).EQ.0) THEN
                              WRITE(OUTLYNE,106) I,OPNAM(I),
     1                        OPERND(I,8),
     1                        OPERND(I,10)
                              CALL SHOWIT(0)
                          END IF
C
C     I DEFAULT
                          IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))
     1                    .EQ.0.AND.INT(OPERND(I,18)).EQ.1) THEN
                              WRITE(OUTLYNE,107) I,OPNAM(I),
     1                        OPERND(I,9),
     1                        OPERND(I,10)
                              CALL SHOWIT(0)
                          END IF
C     I AND K DEFAULT
                          IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))
     1                    .EQ.1.AND.INT(OPERND(I,18)).EQ.1) THEN
                              WRITE(OUTLYNE,108) I,OPNAM(I),
     1                        OPERND(I,9)
                              CALL SHOWIT(0)
                          END IF
C     I AND J DEFAULT
                          IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))
     1                    .EQ.0.AND.INT(OPERND(I,18)).EQ.1) THEN
                              WRITE(OUTLYNE,109) I,OPNAM(I),
     1                        OPERND(I,10)
                              CALL SHOWIT(0)
                          END IF
                      END IF
                  END DO
                  DESYES=.FALSE.
                  DO I=1,MAXFOCRIT
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
 1702             FORMAT('CURRENT FOCRIT OPERAND DESCRIPTIONS')
 1703             FORMAT(1X)
                  DO I=1,MAXTOP
                      IF(OPERDESC(I)(1:8).NE.'        ')
     1                WRITE(OUTLYNE,1701) OPNAM(I),OPERDESC(I)(1:69)
                      IF(OPERDESC(I)(1:8).NE.'        ')
     1                CALL SHOWIT(0)
                  END DO
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB MAROUT.FOR
      SUBROUTINE MAROUT
C
          IMPLICIT NONE
C
          INTEGER I,CFGCHK,TAGER
C
          LOGICAL DESYES,ALLER,CFGER
C
          CHARACTER CNAM*3,FNAMER*6,DASHER*3
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
C
          ALLER=.TRUE.
          CFGER=.FALSE.
          TAGER=0
C
          DASHER='---'
C
          FUNNAM(0)='FUNC00'
          FUNNAM(1)='FUNC01'
          FUNNAM(2)='FUNC02'
          FUNNAM(3)='FUNC03'
          FUNNAM(4)='FUNC04'
          FUNNAM(5)='FUNC05'
          FUNNAM(6)='FUNC06'
          FUNNAM(7)='FUNC07'
          FUNNAM(8)='FUNC08'
          FUNNAM(9)='FUNC09'
          FUNNAM(10)='FUNC10'
C
C       THIS IS SUBROUTINE MAROUT. THIS IS THE SUBROUTINE WHICH DOES OPERAND
C       OUTPUT COMMANDS AT THE CMD LEVEL AND FROM INSIDE MERIT AND
C       UPDATE MERIT
          IF(WC.EQ.'MR'.OR.WC.EQ.'OP') THEN
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"',WC(1:2),'" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
                  WRITE(OUTLYNE,*)
     1            '"',WC(1:2),'" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'CFG'.AND.DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"',WC(1:2),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'MRA'.OR.WC.EQ.'OPA') THEN
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"',WC(1:3),'" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
                  WRITE(OUTLYNE,*)
     1            '"',WC(1:3),'" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'CFG'.AND.DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"',WC(1:3),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'MR'.OR.WC.EQ.'OP') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"',WC(1:2),'" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'MRA'.OR.WC.EQ.'OPA') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"',WC(1:3),'" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
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
C
          IF(DF1.EQ.0.AND.WQ.NE.'CFG') THEN
C     CHECK FOR A VALID NUMERIC WORD #1 INPUT VALUE
C     AND PERFORM THE OPERATION
              IF(W1.LE.0.0D0.OR.W1.GT.OPCNT) THEN
                  WRITE(OUTLYNE,*)'OPERAND NUMBER BEYOND DEFINED BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     HERE DO THE OUTPUT FOR OPERAND NUMBER INT(W1)
C       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
              IF(OPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO MERIT DATA EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
              IF(WC.EQ.'MRA'.OR.WC.EQ.'OPA') THEN
C     DO MRA/OPA HEADER PRINTING
C     PRINT MRA/OPA DATA
                  WRITE(OUTLYNE,200)INT(W1)
                  CALL SHOWIT(0)
 200              FORMAT(
     1            'CURRENT AUXILLIARY MERIT DATA (MRA/OPA) FOR OPERAND #',I3)
 201              FORMAT(
     1            'OP #',2X,'OP NAME',6X,'TARGET',9X,'WEIGHT',6X,
     2            'FUNC.',2X,'REG #',2X,'MODE',2X,'CFG')
 301              FORMAT(53X,'(NW3)')
                  WRITE(OUTLYNE,201)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,301)
                  CALL SHOWIT(0)
 202              FORMAT(I3,3X,A8,2X,G13.6,2X,G13.6,2X,A6,1X,I3,4X,A3,3X,I3)
 802              FORMAT(I3,3X,A8,2X,G13.6,2X,G13.6,2X,A6,1X,I3,4X,A3,3X,A3)
 1202             FORMAT(I3,3X,A8,2X,G13.6,2X,G13.6,2X,A6,1X,'---',4X,A3,3X,I3)
 1802             FORMAT(I3,3X,A8,2X,G13.6,2X,G13.6,2X,A6,1X,'---',4X,A3,3X,A3)
                  IF(OPERND(INT(W1),13).EQ.1.0D0) CNAM ='COR'
                  IF(OPERND(INT(W1),13).EQ.0.0D0) CNAM ='BYP'
                  IF(OPERND(INT(W1),13).EQ.-2.0D0) CNAM ='GTE'
                  IF(OPERND(INT(W1),13).EQ.2.0D0) CNAM ='LTE'
                  IF(OPERND(INT(W1),13).EQ.10.0D0) CNAM='HLD'
                  IF(FUNNAM(INT(OPERND(INT(W1),1))).EQ.'FUNC00') THEN
                      FNAMER='------'
                  ELSE
                      FNAMER=FUNNAM(INT(OPERND(INT(W1),1)))
                  END IF
                  IF(INT(OPERND(INT(W1),16)).NE.0) THEN
                      IF(INT(OPERND(INT(W1),18)).EQ.0)
     1                WRITE(OUTLYNE,202) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7),FNAMER,
     2                INT(OPERND(INT(W1),8)),CNAM,INT(OPERND(INT(W1),16))
                      IF(INT(OPERND(INT(W1),18)).EQ.0) CALL SHOWIT(0)
                      IF(INT(OPERND(INT(W1),18)).EQ.1)
     1                WRITE(OUTLYNE,1202) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7),FNAMER,
     2                CNAM,INT(OPERND(INT(W1),16))
                      IF(INT(OPERND(INT(W1),18)).EQ.1) CALL SHOWIT(0)
                  ELSE
                      IF(INT(OPERND(INT(W1),18)).EQ.0)
     1                WRITE(OUTLYNE,802) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7),FNAMER,
     2                INT(OPERND(INT(W1),8)),CNAM,DASHER
                      IF(INT(OPERND(INT(W1),18)).EQ.0) CALL SHOWIT(0)
                      IF(INT(OPERND(INT(W1),18)).EQ.1)
     1                WRITE(OUTLYNE,1802) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7),FNAMER,
     2                CNAM,DASHER
                      IF(INT(OPERND(INT(W1),18)).EQ.1) CALL SHOWIT(0)
                  END IF
C     WRITE DATA ITEMS
                  IF(OPERDESC(INT(W1))(1:8).NE.'        ')
     1            WRITE(OUTLYNE,1701) OPNAM(INT(W1)),OPERDESC(INT(W1))(1:69)
                  IF(OPERDESC(INT(W1))(1:8).NE.'        ')
     1            CALL SHOWIT(0)
 1701             FORMAT(A8,'::',A69)
                  RETURN
              END IF
              IF(WC.EQ.'MR'.OR.WC.EQ.'OP') THEN
C     DO MR/OP HEADER PRINTING
C     PRINT MR.OP DATA
 100              FORMAT(
     1            'CURRENT MERIT FUNCTION DATA (MR/OP) FOR OPERAND #',I2)
                  WRITE(OUTLYNE,100) INT(W1)
                  CALL SHOWIT(0)
C
 101              FORMAT(
     1            'OP #',2X,'OP NAME',6X,'TARGET',7X,'WEIGHT',7X,
     2            ' NW3 ',8X,' NW4 ',8X,' NW5 ')
 401              FORMAT(
     1            45X,
     2            ' (I) ',8X,' (J) ',8X,' (K) ')
                  WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,401)
                  CALL SHOWIT(0)
C
C     NO DEFAULT
 102              FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5)
C
C     JUST K DEFAULT
 103              FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5,2X,
     1            3X,'-----')
C
C     J AND K DEFAULT
 104              FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,G12.5,1X,3X,
     1            '-----',3X,2X,3X,'-----')
C
C     I,J AND K DEFAULT
 105              FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,3X,'-----',3X,2X,
     1            3X,'-----',3X,2X,3X,'-----')

C     JUST J DEFAULT
 106              FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,G12.5,2X,3X,
     1            '-----',3X,1X,G12.5)

C     JUST I DEFAULT
 107              FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,3X,'-----',3X,
     1            1X,G12.5,1X,G12.5)

C     I AND K DEFAULT
 108              FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,G12.5,2X,3X,
     1            '-----',3X,2X,3X,'-----')

C     I AND J DEFAULT
 109              FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,2X,3X,'-----',3X,
     1            1X,G12.4,2X,3X,'-----')
C
C     NO DEFAULTS
                  IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))
     1            .EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
                      WRITE(OUTLYNE,102) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7),OPERND(INT(W1),8),OPERND(INT(W1),9),
     1                OPERND(INT(W1),10)
                      CALL SHOWIT(0)
                  END IF
C
C     K DEFAULT
                  IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))
     1            .EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
                      WRITE(OUTLYNE,103) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7),OPERND(INT(W1),8),OPERND(INT(W1),9)
                      CALL SHOWIT(0)
                  END IF
C
C     J AND K DEFAULTS
                  IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))
     1            .EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
                      WRITE(OUTLYNE,104) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7),OPERND(INT(W1),8)
                      CALL SHOWIT(0)
                  END IF
C
C     I J K DEFAULTS
                  IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))
     1            .EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
                      WRITE(OUTLYNE,105) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7)
                      CALL SHOWIT(0)
                  END IF
C
C     J DEFAULT
                  IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))
     1            .EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
                      WRITE(OUTLYNE,106) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7),OPERND(INT(W1),8),
     1                OPERND(INT(W1),10)
                      CALL SHOWIT(0)
                  END IF
C
C     I DEFAULT
                  IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))
     1            .EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
                      WRITE(OUTLYNE,107) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7),OPERND(INT(W1),9),
     1                OPERND(INT(W1),10)
                      CALL SHOWIT(0)
                  END IF
C     I AND K DEFAULT
                  IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))
     1            .EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
                      WRITE(OUTLYNE,108) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7),OPERND(INT(W1),9)
                      CALL SHOWIT(0)
                  END IF
C     I AND J DEFAULT
                  IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))
     1            .EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
                      WRITE(OUTLYNE,109) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),
     1                OPERND(INT(W1),7),OPERND(INT(W1),10)
                      CALL SHOWIT(0)
                  END IF
                  IF(OPERDESC(INT(W1))(1:8).NE.'        ')
     1            WRITE(OUTLYNE,1701) OPNAM(INT(W1)),OPERDESC(INT(W1))(1:69)
                  IF(OPERDESC(INT(W1))(1:8).NE.'        ')
     1            CALL SHOWIT(0)
                  RETURN
              END IF
          ELSE
C     DF1=1, NO NUMERIC INPUT, DO ALL OUTPUT
C     HERE DO THE MERIT OUTPUT FOR ALL OPERANDS
C       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
              IF(OPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO MERIT DATA EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
              IF(WC.EQ.'MRA'.OR.WC.EQ.'OPA') THEN
C     DO MRA.OPA HEADER PRINTING
                  WRITE(OUTLYNE,300)
                  CALL SHOWIT(0)
                  IF(CFGER) WRITE(OUTLYNE,3011) TAGER
                  IF(CFGER) CALL SHOWIT(0)
 300              FORMAT(
     1            'CURRENT AUXILLIARY MERIT DATA (MRA/OPA)')
 3011             FORMAT('ACTIVE IN CONFIGURATION # ',I2)
                  WRITE(OUTLYNE,201)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,301)
                  CALL SHOWIT(0)
                  DO I=1,OPCNT
C     WRITE DATA ITEMS
                      IF(OPERND(I,13).EQ.1.0D0) CNAM ='COR'
                      IF(OPERND(I,13).EQ.0.0D0) CNAM ='BYP'
                      IF(OPERND(I,13).EQ.-2.0D0) CNAM ='GTE'
                      IF(OPERND(I,13).EQ.2.0D0) CNAM ='LTE'
                      IF(OPERND(I,13).EQ.10.0D0) CNAM ='HLD'
                      IF(FUNNAM(INT(OPERND(I,1))).EQ.'FUNC00') THEN
                          FNAMER='-----'
                      ELSE
                          FNAMER=FUNNAM(INT(OPERND(I,1)))
                      END IF
                      IF(INT(OPERND(I,16)).NE.0) THEN
                          IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                              IF(INT(OPERND(I,18)).EQ.0)
     1                        WRITE(OUTLYNE,202) I,OPNAM(I),OPERND(I,2),
     1                        OPERND(I,7),FNAMER,
     2                        INT(OPERND(I,8)),CNAM,INT(OPERND(I,16))
                              IF(INT(OPERND(I,18)).EQ.0) CALL SHOWIT(0)
                              IF(INT(OPERND(I,18)).EQ.1)
     1                        WRITE(OUTLYNE,1202) I,OPNAM(I),OPERND(I,2),
     1                        OPERND(I,7),FNAMER,
     2                        CNAM,INT(OPERND(I,16))
                              IF(INT(OPERND(I,18)).EQ.1) CALL SHOWIT(0)
                          END IF
                      ELSE
                          IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                              IF(INT(OPERND(I,18)).EQ.0)
     1                        WRITE(OUTLYNE,802) I,OPNAM(I),OPERND(I,2),
     1                        OPERND(I,7),FNAMER,
     2                        INT(OPERND(I,8)),CNAM,DASHER
                              IF(INT(OPERND(I,18)).EQ.0) CALL SHOWIT(0)
                              IF(INT(OPERND(I,18)).EQ.1)
     1                        WRITE(OUTLYNE,1802) I,OPNAM(I),OPERND(I,2),
     1                        OPERND(I,7),FNAMER,
     2                        CNAM,DASHER
                              IF(INT(OPERND(I,18)).EQ.1) CALL SHOWIT(0)
                          END IF
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
                      IF(OPERDESC(I)(1:8).NE.'        ')
     1                WRITE(OUTLYNE,1701) OPNAM(I),OPERDESC(I)(1:69)
                      IF(OPERDESC(I)(1:8).NE.'        ')
     1                CALL SHOWIT(0)
                  END DO
                  RETURN
              END IF
              IF(WC.EQ.'MR'.OR.WC.EQ.'OP') THEN
                  WRITE(OUTLYNE,400)
                  CALL SHOWIT(0)
                  IF(CFGER) WRITE(OUTLYNE,3011) TAGER
                  IF(CFGER) CALL SHOWIT(0)
 400              FORMAT(
     1            'CURRENT MERIT FUNCTION DATA (MR/OP)')
                  WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,401)
                  CALL SHOWIT(0)
C     DO MERIT HEADER PRINTING
                  DO I=1,OPCNT
C     WRITE DATA
C
C     NO DEFAULTS
                      IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))
     1                .EQ.0.AND.INT(OPERND(I,18)).EQ.0) THEN
                          IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                              WRITE(OUTLYNE,102) I,OPNAM(I),OPERND(I,2),
     1                        OPERND(I,7),OPERND(I,8),OPERND(I,9),
     1                        OPERND(I,10)
                              CALL SHOWIT(0)
                          END IF
                      END IF
C
C     K DEFAULT
                      IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))
     1                .EQ.1.AND.INT(OPERND(I,18)).EQ.0) THEN
                          IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                              WRITE(OUTLYNE,103) I,OPNAM(I),OPERND(I,2),
     1                        OPERND(I,7),OPERND(I,8),OPERND(I,9)
                              CALL SHOWIT(0)
                          END IF
                      END IF
C
C     J AND K DEFAULTS
                      IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))
     1                .EQ.1.AND.INT(OPERND(I,18)).EQ.0) THEN
                          IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                              WRITE(OUTLYNE,104) I,OPNAM(I),OPERND(I,2),
     1                        OPERND(I,7),OPERND(I,8)
                              CALL SHOWIT(0)
                          END IF
                      END IF
C
C     I J K DEFAULTS
                      IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))
     1                .EQ.1.AND.INT(OPERND(I,18)).EQ.1) THEN
                          WRITE(OUTLYNE,105) I,OPNAM(I),OPERND(I,2),
     1                    OPERND(I,7)
                          CALL SHOWIT(0)
                      END IF
C
C     J DEFAULT
                      IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))
     1                .EQ.0.AND.INT(OPERND(I,18)).EQ.0) THEN
                          IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                              WRITE(OUTLYNE,106) I,OPNAM(I),OPERND(I,2),
     1                        OPERND(I,7),OPERND(I,8),
     1                        OPERND(I,10)
                              CALL SHOWIT(0)
                          END IF
                      END IF
C
C     I DEFAULT
                      IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))
     1                .EQ.0.AND.INT(OPERND(I,18)).EQ.1) THEN
                          IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                              WRITE(OUTLYNE,107) I,OPNAM(I),OPERND(I,2),
     1                        OPERND(I,7),OPERND(I,9),
     1                        OPERND(I,10)
                              CALL SHOWIT(0)
                          END IF
                      END IF
C     I AND K DEFAULT
                      IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))
     1                .EQ.1.AND.INT(OPERND(I,18)).EQ.1) THEN
                          IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                              WRITE(OUTLYNE,108) I,OPNAM(I),OPERND(I,2),
     1                        OPERND(I,7),OPERND(I,9)
                              CALL SHOWIT(0)
                          END IF
                      END IF
C     I AND J DEFAULT
                      IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))
     1                .EQ.0.AND.INT(OPERND(I,18)).EQ.1) THEN
                          IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                              WRITE(OUTLYNE,109) I,OPNAM(I),OPERND(I,2),
     1                        OPERND(I,7),OPERND(I,10)
                              CALL SHOWIT(0)
                          END IF
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
                  DO I=1,OPCNT
                      IF(OPERDESC(I)(1:8).NE.'        ') THEN
                          WRITE(OUTLYNE,1701) OPNAM(I),OPERDESC(I)(1:69)
                          CALL SHOWIT(0)
                      END IF
                  END DO
                  RETURN
              END IF
          END IF
          RETURN
      END
