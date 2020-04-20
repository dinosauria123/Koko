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

C       FIFTEENTH FILE FOR LENS DATABASE MANAGER FILES

C SUB LNSEOS.FOR
      SUBROUTINE LNSEOS
          USE GLOBALS
C
C     CALLS LNSEOS1. THIS IS HERE TO SUPPORT AUTOFUNC
C
          IMPLICIT NONE
C
!        LOGICAL SAV2,RES2,EXIS49
C
          INTEGER I,MACYES,ICASE,OF5,OF6,ALLOERR
          COMMON/OLDFVAL/OF5,OF6
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsub.inc'
          OF5=F5
          OF6=F6
C
          IF(CFLDCNT.EQ.0) THEN
C       SET UP THREE FIELD OF VIEW POSITIONS AT 0 .7 AND 1.0
              CFLDCNT=3
              CFLDS(1,1)=0.0D0
              CFLDS(1,2)=0.0D0
              CFLDS(1,3)=0.0D0
              CFLDS(2,1)=0.0D0
              CFLDS(2,2)=0.7D0
              CFLDS(2,3)=1.0D0
          END IF
C     RESTORE DECENTERS TO CONVER PIVOTS
          IF(SYSTEM1(20).GT.DBLE(MAXSUR)) SYSTEM1(20)=DBLE(MAXSUR)
          I=INT(SYSTEM1(20))
          ALENS(29,1:I)=0.0D0
          ALENS(31,1:I)=ALENS(114,1:I)
          ALENS(30,1:I)=ALENS(115,1:I)
          ALENS(69,1:I)=ALENS(116,1:I)
          ALENS(26,1:I)=ALENS(118,1:I)
          ALENS(27,1:I)=ALENS(119,1:I)
          ALENS(28,1:I)=ALENS(120,1:I)
          DO I=0,INT(SYSTEM1(20))
              IF(ALENS(31,I).NE.0.0D0.OR.ALENS(30,I).NE.0.0D0.OR.
     1        ALENS(69,I).NE.0.0D0) ALENS(29,I)=1.0D0
          END DO
C
          IF(INT(SYSTEM1(91)).EQ.0) F57=0
C
          IF(F57.EQ.0) THEN
C       ALLOCATE THE MULTIHIT ARRAYS AND P1ARAY
              DEALLOCATE(GLOBAL_MULTIRAY_DATA,GLOBAL_MULTIREF_DATA,
     1        MULTIRAY_DATA,MULTIREF_DATA,MULTI_DATA,P1ARAY,STAT=ALLOERR)
              IF(.NOT.ALLOCATED(MULTIRAY_DATA)) THEN
                  IF(SYSTEM1(102).EQ.1.0D0) THEN
C       AN NSS TUBE SURFACE IS PRESENT, ALLOCATE MEMORY
                      DEALLOCATE(GLOBAL_MULTIRAY_DATA,GLOBAL_MULTIREF_DATA,
     1                MULTIRAY_DATA,MULTIREF_DATA,MULTI_DATA,P1ARAY,STAT=ALLOERR)
                      ALLOCATE(
     1                GLOBAL_MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                GLOBAL_MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                MULTI_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                P1ARAY(0:360,1:3,1:MRAYS),
     1                STAT=ALLOERR)
                      GLOBAL_MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                      GLOBAL_MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                      MULTI_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                      MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                      MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                  ELSE
                      DEALLOCATE(P1ARAY,STAT=ALLOERR)
                      ALLOCATE(P1ARAY(0:360,1:3,1:MRAYS),STAT=ALLOERR)
                  END IF
              END IF
              CALL LNSEOS1
              IF(OF5.EQ.1) THEN
C     SET PARAXIAL FLAGS TO FALSE
                  CALL PRTRB
                  CALL PRTRC
                  CALL PRTRD
              END IF
              RETURN
          END IF
          IF(F57.EQ.1) THEN
              F57=0
C       ALLOCATE THE MULTIHIT ARRAYS AND P1ARAY
              DEALLOCATE(GLOBAL_MULTIRAY_DATA,GLOBAL_MULTIREF_DATA,
     1        MULTIRAY_DATA,MULTIREF_DATA,MULTI_DATA,P1ARAY,STAT=ALLOERR)
              IF(.NOT.ALLOCATED(MULTIRAY_DATA)) THEN
                  IF(SYSTEM1(102).EQ.1.0D0) THEN
C       AN NSS TUBE SURFACE IS PRESENT, ALLOCATE MEMORY
                      DEALLOCATE(GLOBAL_MULTIRAY_DATA,GLOBAL_MULTIREF_DATA,
     1                MULTIRAY_DATA,MULTIREF_DATA,P1ARAY,MULTI_DATA,STAT=ALLOERR)
                      ALLOCATE(
     1                GLOBAL_MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                GLOBAL_MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                MULTI_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                P1ARAY(0:360,1:3,1:MRAYS),
     1                STAT=ALLOERR)
                      GLOBAL_MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                      GLOBAL_MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                      MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                      MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                      MULTI_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                  ELSE
                      DEALLOCATE(P1ARAY,STAT=ALLOERR)
                      ALLOCATE(P1ARAY(0:360,1:3,1:MRAYS),STAT=ALLOERR)
                  END IF
              END IF
              CALL LNSEOS1
              IF(OF5.EQ.1) THEN
C     SET PARAXIAL FLAGS TO FALSE
                  CALL PRTRB
                  CALL PRTRC
                  CALL PRTRD
              END IF
              F57=1
              F1=0
              F6=1
              F22=1
              LNSTYP=2
          END IF
C
          IF(F6.EQ.1) THEN
              IF(F57.EQ.1) THEN
                  F57=0
C       ALLOCATE THE MULTIHIT ARRAYS AND P1ARAY
                  DEALLOCATE(GLOBAL_MULTIRAY_DATA,GLOBAL_MULTIREF_DATA,
     1            MULTIRAY_DATA,MULTIREF_DATA,MULTI_DATA,P1ARAY,STAT=ALLOERR)
                  IF(.NOT.ALLOCATED(MULTIRAY_DATA)) THEN
                      IF(SYSTEM1(102).EQ.1.0D0) THEN
C       AN NSS TUBE SURFACE IS PRESENT, ALLOCATE MEMORY
                          DEALLOCATE(GLOBAL_MULTIRAY_DATA,GLOBAL_MULTIREF_DATA,
     1                    MULTIRAY_DATA,MULTIREF_DATA,P1ARAY,MULTI_DATA,STAT=ALLOERR)
                          ALLOCATE(
     1                    GLOBAL_MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                    GLOBAL_MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                    MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                    MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                    MULTI_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                    P1ARAY(0:360,1:3,1:MRAYS),
     1                    STAT=ALLOERR)
                          GLOBAL_MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                          GLOBAL_MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                          MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                          MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                          MULTI_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                      ELSE
                          DEALLOCATE(P1ARAY,STAT=ALLOERR)
                          ALLOCATE(P1ARAY(0:360,1:3,1:MRAYS),STAT=ALLOERR)
                      END IF
                  END IF
                  CALL LNSEOS1
C       REFRESH 357 ABERRATIONS
                  IF(OF5.EQ.1) THEN
C     SET PARAXIAL FLAGS TO FALSE
                      CALL PRTRB
                      CALL PRTRC
                      CALL PRTRD
                  END IF
C     NOW EXECUTE THE MACRO FUNCTION DESIGNATED BY AUTOFUNC
C
                  ICASE=INT(SYSTEM1(91))
                  SELECT CASE(ICASE)
                    CASE(1)
                      IF(.NOT.FUNEXT(1)) THEN
                          WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN01 DOES NOT EXIST'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AND COULD NOT BE EXECUTED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AUTOFUNC OPERATION WAS NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          GO TO 5000
                      END IF
C               RUN MACRO FUN01
                      SAVE_KDP(1)=SAVEINPT(1)
                      NEST=0
                      NESTI(0:20)=0
                      NESTIJ(0:20)=0
                      MWQ(0:20)='        '
                      MWS(0:20)='        '
                      MSQ(0:20)=0
                      MST(0:20)=0
                      MNW(0:20,1:5)=0.0D0
                      MDF(0:20,1:5)=1
                      NESFUN(0:10)=.FALSE.
                      TF(0:20)=0
                      WC='FUN01'
                      CALL MACRUN(MACYES)
                      REST_KDP(1)=RESTINPT(1)
C
                    CASE(2)
                      IF(.NOT.FUNEXT(2)) THEN
                          WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN02 DOES NOT EXIST'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AND COULD NOT BE EXECUTED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AUTOFUNC OPERATION WAS NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          GO TO 5000
                      END IF
C               RUN MACRO FUN02
                      SAVE_KDP(1)=SAVEINPT(1)
                      NEST=0
                      NESTI(0:20)=0
                      NESTIJ(0:20)=0
                      MWQ(0:20)='        '
                      MWS(0:20)='        '
                      MSQ(0:20)=0
                      MST(0:20)=0
                      MNW(0:20,1:5)=0.0D0
                      MDF(0:20,1:5)=1
                      NESFUN(0:10)=.FALSE.
                      TF(0:20)=0
                      WC='FUN02'
                      CALL MACRUN(MACYES)
                      REST_KDP(1)=RESTINPT(1)
C
                    CASE(3)
                      IF(.NOT.FUNEXT(3)) THEN
                          WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN03 DOES NOT EXIST'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AND COULD NOT BE EXECUTED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AUTOFUNC OPERATION WAS NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          GO TO 5000
                      END IF
C               RUN MACRO FUN03
                      SAVE_KDP(1)=SAVEINPT(1)
                      NEST=0
                      NESTI(0:20)=0
                      NESTIJ(0:20)=0
                      MWQ(0:20)='        '
                      MWS(0:20)='        '
                      MSQ(0:20)=0
                      MST(0:20)=0
                      MNW(0:20,1:5)=0.0D0
                      MDF(0:20,1:5)=1
                      NESFUN(0:10)=.FALSE.
                      TF(0:20)=0
                      WC='FUN03'
                      CALL MACRUN(MACYES)
                      REST_KDP(1)=RESTINPT(1)
C
                    CASE(4)
                      IF(.NOT.FUNEXT(4)) THEN
                          WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN04 DOES NOT EXIST'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AND COULD NOT BE EXECUTED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AUTOFUNC OPERATION WAS NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          GO TO 5000
                      END IF
C               RUN MACRO FUN04
                      SAVE_KDP(1)=SAVEINPT(1)
                      NEST=0
                      NESTI(0:20)=0
                      NESTIJ(0:20)=0
                      MWQ(0:20)='        '
                      MWS(0:20)='        '
                      MSQ(0:20)=0
                      MST(0:20)=0
                      MNW(0:20,1:5)=0.0D0
                      MDF(0:20,1:5)=1
                      NESFUN(0:10)=.FALSE.
                      TF(0:20)=0
                      WC='FUN04'
                      CALL MACRUN(MACYES)
                      REST_KDP(1)=RESTINPT(1)
C
                    CASE(5)
                      IF(.NOT.FUNEXT(5)) THEN
                          WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN05 DOES NOT EXIST'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AND COULD NOT BE EXECUTED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AUTOFUNC OPERATION WAS NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          GO TO 5000
                      END IF
C               RUN MACRO FUN05
                      SAVE_KDP(1)=SAVEINPT(1)
                      NEST=0
                      NESTI(0:20)=0
                      NESTIJ(0:20)=0
                      MWQ(0:20)='        '
                      MWS(0:20)='        '
                      MSQ(0:20)=0
                      MST(0:20)=0
                      MNW(0:20,1:5)=0.0D0
                      MDF(0:20,1:5)=1
                      NESFUN(0:10)=.FALSE.
                      TF(0:20)=0
                      WC='FUN05'
                      CALL MACRUN(MACYES)
                      REST_KDP(1)=RESTINPT(1)

                    CASE(6)
                      IF(.NOT.FUNEXT(6)) THEN
                          WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN06 DOES NOT EXIST'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AND COULD NOT BE EXECUTED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AUTOFUNC OPERATION WAS NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          GO TO 5000
                      END IF
C               RUN MACRO FUN06
                      SAVE_KDP(1)=SAVEINPT(1)
                      NEST=0
                      NESTI(0:20)=0
                      NESTIJ(0:20)=0
                      MWQ(0:20)='        '
                      MWS(0:20)='        '
                      MSQ(0:20)=0
                      MST(0:20)=0
                      MNW(0:20,1:5)=0.0D0
                      MDF(0:20,1:5)=1
                      NESFUN(0:10)=.FALSE.
                      TF(0:20)=0
                      WC='FUN06'
                      CALL MACRUN(MACYES)
                      REST_KDP(1)=RESTINPT(1)
C
                    CASE(7)
                      IF(.NOT.FUNEXT(7)) THEN
                          WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN07 DOES NOT EXIST'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AND COULD NOT BE EXECUTED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AUTOFUNC OPERATION WAS NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          GO TO 5000
                      END IF
C               RUN MACRO FUN07
                      SAVE_KDP(1)=SAVEINPT(1)
                      NEST=0
                      NESTI(0:20)=0
                      NESTIJ(0:20)=0
                      MWQ(0:20)='        '
                      MWS(0:20)='        '
                      MSQ(0:20)=0
                      MST(0:20)=0
                      MNW(0:20,1:5)=0.0D0
                      MDF(0:20,1:5)=1
                      NESFUN(0:10)=.FALSE.
                      TF(0:20)=0
                      WC='FUN07'
                      CALL MACRUN(MACYES)
                      REST_KDP(1)=RESTINPT(1)
C
                    CASE(8)
                      IF(.NOT.FUNEXT(8)) THEN
                          WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN08 DOES NOT EXIST'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AND COULD NOT BE EXECUTED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AUTOFUNC OPERATION WAS NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          GO TO 5000
                      END IF
C               RUN MACRO FUN08
                      SAVE_KDP(1)=SAVEINPT(1)
                      NEST=0
                      NESTI(0:20)=0
                      NESTIJ(0:20)=0
                      MWQ(0:20)='        '
                      MWS(0:20)='        '
                      MSQ(0:20)=0
                      MST(0:20)=0
                      MNW(0:20,1:5)=0.0D0
                      MDF(0:20,1:5)=1
                      NESFUN(0:10)=.FALSE.
                      TF(0:20)=0
                      WC='FUN08'
                      CALL MACRUN(MACYES)
                      REST_KDP(1)=RESTINPT(1)
C
                    CASE(9)
                      IF(.NOT.FUNEXT(9)) THEN
                          WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN09 DOES NOT EXIST'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AND COULD NOT BE EXECUTED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AUTOFUNC OPERATION WAS NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          GO TO 5000
                      END IF
C               RUN MACRO FUN09
                      SAVE_KDP(1)=SAVEINPT(1)
                      NEST=0
                      NESTI(0:20)=0
                      NESTIJ(0:20)=0
                      MWQ(0:20)='        '
                      MWS(0:20)='        '
                      MSQ(0:20)=0
                      MST(0:20)=0
                      MNW(0:20,1:5)=0.0D0
                      MDF(0:20,1:5)=1
                      NESFUN(0:10)=.FALSE.
                      TF(0:20)=0
                      WC='FUN09'
                      CALL MACRUN(MACYES)
                      REST_KDP(1)=RESTINPT(1)
C
                    CASE(10)
                      IF(.NOT.FUNEXT(10)) THEN
                          WRITE(OUTLYNE,*) 'MACRO FUNCTION FUN10 DOES NOT EXIST'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AND COULD NOT BE EXECUTED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'WARNING:'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*) 'AUTOFUNC OPERATION WAS NOT PERFORMED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          GO TO 5000
                      END IF
C               RUN MACRO FUN10
                      SAVE_KDP(1)=SAVEINPT(1)
                      NEST=0
                      NESTI(0:20)=0
                      NESTIJ(0:20)=0
                      MWQ(0:20)='        '
                      MWS(0:20)='        '
                      MSQ(0:20)=0
                      MST(0:20)=0
                      MNW(0:20,1:5)=0.0D0
                      MDF(0:20,1:5)=1
                      NESFUN(0:10)=.FALSE.
                      TF(0:20)=0
                      WC='FUN10'
                      CALL MACRUN(MACYES)
                      REST_KDP(1)=RESTINPT(1)
                  END SELECT
                  F1=0
                  F6=1
                  F22=1
                  LNSTYP=2
C       ALLOCATE THE MULTIHIT ARRAYS AND P1ARAY
                  DEALLOCATE(GLOBAL_MULTIRAY_DATA,GLOBAL_MULTIREF_DATA,
     1            MULTIRAY_DATA,MULTIREF_DATA,MULTI_DATA,P1ARAY,STAT=ALLOERR)
C       DO WE NEED TO ALLOCATE THE MULTIHIT ARRAYS
                  IF(.NOT.ALLOCATED(MULTIRAY_DATA)) THEN
                      IF(SYSTEM1(102).EQ.1.0D0) THEN
C       AN NSS TUBE SURFACE IS PRESENT, ALLOCATE MEMORY
                          DEALLOCATE(GLOBAL_MULTIRAY_DATA,GLOBAL_MULTIREF_DATA,
     1                    MULTIRAY_DATA,MULTIREF_DATA,MULTI_DATA,P1ARAY,STAT=ALLOERR)
                          ALLOCATE(
     1                    GLOBAL_MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                    GLOBAL_MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                    MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                    MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                    MULTI_DATA(1:50,0:MAXSUR,1:MRAYS),
     1                    P1ARAY(0:360,1:3,1:MRAYS),
     1                    STAT=ALLOERR)
                          GLOBAL_MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                          GLOBAL_MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                          MULTIRAY_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                          MULTIREF_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                          MULTI_DATA(1:50,0:MAXSUR,1:MRAYS)=0.0D0
                      ELSE
                          DEALLOCATE(P1ARAY,STAT=ALLOERR)
                          ALLOCATE(P1ARAY(0:360,1:3,1:MRAYS),STAT=ALLOERR)
                      END IF
                  END IF
                  CALL LNSEOS1
                  IF(OF5.EQ.1) THEN
C     SET PARAXIAL FLAGS TO FALSE
                      CALL PRTRB
                      CALL PRTRC
                      CALL PRTRD
                  END IF
                  F57=1
              END IF
          END IF
 5000     CONTINUE
          RETURN
      END
      SUBROUTINE TILT_RETURN(ERCODE,TRYES)
          IMPLICIT NONE
          INTEGER I,J
          LOGICAL ERCODE,TRYES
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C     TEST FOR NESTED TILT RETS
          DO I=0,INT(SYSTEM1(20))
              IF(ALENS(25,I).EQ.6.0D0) THEN
                  TRYES=.TRUE.
C     FOUND A TILT RET ON SURFACE I
C     THAT TILT RET MUST NOT REFER TO A SURFACE WITH A TILT RET ON IT
                  J=INT(ALENS(70,I))
                  IF(ALENS(25,J).EQ.6.0D0) THEN
C     TILT RET REFERS TO A SURFACE WHICH HAS A TILT RET ON IT, DROP THE
C     TILT RETURN
                      ALENS(25:28,I)=0.0D0
                      ALENS(118:120,I)=0.0D0
                      ALENS(70,I)=0.0D0
                      ALENS(77:80,I)=0.0D0
                      ALENS(59,I)=0.0D0
                      ALENS(90:95,I)=0.0D0
                      ERCODE=.TRUE.
                      WRITE(OUTLYNE,*)'"TILT RET" ON SURFACE ',I
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'REFERS TO SURFACE ',J,' WHICH ITSELF HAS'
                      CALL SHOWIT(1)
                      OUTLYNE='A "TILT RET" ASSIGNED TO IT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'THE "TILT RET" ON SURFACE ',I,' IS BEING DELETED'
                      CALL SHOWIT(1)
                  END IF
              ELSE
C     NO TILT RET ON SURFACE I
              END IF
          END DO
          DO I=0,INT(SYSTEM1(20))
              IF(ALENS(25,I).EQ.6.0D0) THEN
C     TILT RET
                  DO J=0,I
                      IF(ALENS(25,J).EQ.2.0D0.OR.ALENS(25,J).EQ.3.0D0) THEN
C     PREVIOUS TILT AUTO FOUND
C     DELETE THE TILT RET ON I
                          ALENS(25:28,I)=0.0D0
                          ALENS(118:120,I)=0.0D0
                          ALENS(70,I)=0.0D0
                          ALENS(77:80,I)=0.0D0
                          ALENS(59,I)=0.0D0
                          ALENS(90:95,I)=0.0D0
                          ERCODE=.TRUE.
                          WRITE(OUTLYNE,*)'"TILT RET" ON SURFACE ',I
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'IS PRECEEDED BY A "TILT AUTO(M)" ON SURFACE',J
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'THE "TILT RET" ON SURFACE ',I,' IS BEING DELETED'
                          CALL SHOWIT(1)
                      ELSE
C     TO PREV TILT AUTO FOUND
                      END IF
                  END DO
              ELSE
C     NO TILT RET
              END IF
          END DO
          DO I=0,INT(SYSTEM1(20))
              IF(ALENS(25,I).EQ.6.0D0) THEN
C     TILT RET
                  IF(INT(ALENS(70,I)).GE.I) THEN
                      ERCODE=.TRUE.
                      WRITE(OUTLYNE,*)'"TILT RET" ON SURFACE ',I
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'REFERS TO A FOLLOWING SURFACE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'THE "TILT RET" ON SURFACE ',I,' IS BEING DELETED'
                      CALL SHOWIT(1)
                  ELSE
C     REFERS TO EARLIER SURFACE
                  END IF
              ELSE
C     NO TILT RET
              END IF
          END DO
          RETURN
      END
C SUB LNSEOS1.FOR
      SUBROUTINE LNSEOS1
          USE GLOBALS
C     NOTE ON 11/04/96 THIS ROUTINE ONLY HAS ONE SUCCESSFUL RETURN
C     AND IT IS AT THE BOTTOM OF THE ROUTINE
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE LNSEOS. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE COMMAND EOS WHEN ISSUED FROM THE NEW LENS INPUT
C       OR LENS UPDATE MODE.
C
          LOGICAL REDOLNSEOS,BADGLS,ERCODE,TRYES,LPASS1
C
          COMMON/GLSBAD/BADGLS
C
          INTEGER KIL,ITYPEP,CFI,CFJ,CFF12,IPASS1,IPASS2
     6    ,I,II,III,SLVMRK,ITT,IFF,TESTIT,J,K,ZZTOP
C
          COMMON/DPIKER/CFI,CFJ,CFF12
C
          COMMON/PTYPER/ITYPEP
C
          REAL*8 YCLAP,XCLAP,XTERM,YTERM,A1,A2,JK_TEMP
C
          LOGICAL NOSTOP,NOREF,SKIP
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C       IF F28 OR F31 ARE 1, WE ARE IN OPTIM OR TOLER
C       AND SOME THINGS GET SKIPPED
          SKIP=.FALSE.
          IF(F28.EQ.1.OR.F31.EQ.1.OR.LNSTYP.NE.1) SKIP=.TRUE.
C
          SELECT CASE (SKIP)
            CASE (.TRUE.)
C       FIX SECOND NUMERIC WORD IN SCY/SCX/SCY FANG/SCX FANG
C       AND ASTOP ASSIGNMENT
              NOSTOP=.TRUE.
              NOREF=.TRUE.
              IF(INT(SYSTEM1(25)).GE.0.AND.INT(SYSTEM1(25)).LE.
     1        INT(SYSTEM1(20)))
     1        NOREF=.FALSE.
              IF(INT(SYSTEM1(26)).GE.0.AND.INT(SYSTEM1(26)).LE.
     1        INT(SYSTEM1(20)))
     1        NOSTOP=.FALSE.
C       IF NO REF SURF AND NO STOP, MAKE REF SURF = SURFACE 1
              IF(NOREF.AND.NOSTOP) THEN
                  SYSTEM1(25)=1.0D0
              END IF
C       IF NO REF SURF BUT THERE IS A STOP, MAKE REF SURF = STOP SURF
              IF(.NOT.NOSTOP.AND.NOREF) THEN
                  SYSTEM1(25)=SYSTEM1(26)
              END IF
              IF(.NOT.NOSTOP) THEN
C       THERE IS A STOP SURFACE, ZERO OUT SECOND NUMERIC WORDS OF
C       SCY,SCX,SCY FANG AND SCX FANG
                  SYSTEM1(15)=0.0D0
                  SYSTEM1(17)=0.0D0
                  SYSTEM1(22)=0.0D0
                  SYSTEM1(24)=0.0D0
              END IF
C
          END SELECT
C
          REDOLNSEOS=.FALSE.
C       999 IS A RE-ENTRY POINT
 999      CONTINUE
          SELECT CASE (SKIP)
            CASE (.TRUE.)
C       CHECK FOR OBJECT THICKNESS ZERO
              IF(DABS(ALENS(3,0)).EQ.0.0D0) THEN
                  OUTLYNE=
     1            'WARNING: OBJECT THICKNESS IS ZERO'
                  CALL SHOWIT(1)
                  OUTLYNE='IS CHANGED TO 1.0D20'
                  CALL SHOWIT(1)
                  ALENS(3,0)=1.0D20
              END IF
C       CHECK FOR OBJECT THICKNESS LESS THAN SAY OR SAX
              IF(DABS(ALENS(3,0)).LT.0.01D0) THEN
                  OUTLYNE=
     1            'WARNING: OBJECT THICKNESS IS LESS THAN 0.01 LENS UNITS.'
                  CALL SHOWIT(1)
                  OUTLYNE='INCREASE THE OBJECT THICKNESS IN ORDER TO AVOID OPD'
                  CALL SHOWIT(1)
                  OUTLYNE='INACCURACIES'
                  CALL SHOWIT(1)
              END IF
C       CHECK FOR SURFACE THICKNESS MAG GREATER THAN 1E200
C     OR LESS THAN 1.0D-20
C       THIS HELPS OVERFLOWING ON MACHINES.
              DO KIL=0,INT(SYSTEM1(20))
                  IF(ALENS(3,KIL).GT.1.0D200)
     1            ALENS(3,KIL)=1.0D200
                  IF(ALENS(3,KIL).LT.-1.0D200)
     1            ALENS(3,KIL)=-1.0D200
                  IF(DABS(ALENS(3,KIL)).LT.1.0D-20)
     1            ALENS(3,KIL)=0.0D0
              END DO
C       RESET NEWOBJ,NEWREF AND NEWIMG
              CALL RESSUR
C
              IF(SYSTEM1(18).EQ.1.0D0) SYSTEM1(19)=1.0D0
              IF(SYSTEM1(18).EQ.0.0D0) SYSTEM1(19)=0.0D0
C
C       SET REFEXT TO FALSE AND NULL TO TRUE
              IF(RAYCLEAR) THEN
                  FOBYES=.FALSE.
                  NULL=.TRUE.
                  FAIL=.TRUE.
                  RAYEXT=.FALSE.
                  POLEXT=.FALSE.
                  REFEXT=.FALSE.
                  SPDEXT=.FALSE.
                  GSPDEXT=.FALSE.
                  CPFNEXT=.FALSE.
                  CALL DELPSF
              END IF
          END SELECT
          II=INT(SYSTEM1(20))
          ALENS(109,1:II)=0.0D0
          IF(F5.EQ.1) THEN
C       LENS INPUT, UNLOAD THE GRID FILES
              ALENS(88,1:II)=0.0D0
              IPASS1=1
              IPASS2=0
              LPASS1=.FALSE.
              GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
              GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
              GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
              CALL GRIDS(1,0,LPASS1)
          END IF
C
C     RESOLVE FICT GLASS TYPE IN OPTIM AND TOLER

          IF(LNSTYP.EQ.2.OR.LNSTYP.EQ.3) THEN
C       NOW RESOLVE THE CALCULATION OF REFRACTIVE INDICES VIA
C       GLASS CATALOG SEARCHES
C
C     FIX LEADING COLON IN GLASS NAME
              DO I=0,INT(SYSTEM1(20))
                  IF(GLANAM(I,2)(1:1).EQ.':') GLANAM(I,2)(1:13)=GLANAM(I,2)(2:13)
                  IF(GLANMP(I,2)(1:1).EQ.':') GLANMP(I,2)(1:13)=GLANMP(I,2)(2:13)
              END DO
              CALL GLSRES
          END IF
C
          IF(LNSTYP.EQ.1) THEN
C
C       AS VARIOUS FEATURES ARE ADDED TO THE LENS SUBFILE, THINGS
C       SUCH AS THE PARAXIAL SOLVES OR THE PIKUPS, THEY WILL
C       REQUIRE RESOLUTION PRIOR TO A RETURN TO THE CMD LEVEL.
C
C       NOW RESOLVE THE CALCULATION OF REFRACTIVE INDICES VIA
C       GLASS CATALOG SEARCHES IF F22=1. DONE FOR THE CURRENT
C       CONFIGURATION ONLY.
C
C       PROCEED TO CHECK CATALOGS
C
C     FIX LEADING COLON IN GLASS NAME
              DO I=0,INT(SYSTEM1(20))
                  IF(GLANAM(I,2)(1:1).EQ.':') GLANAM(I,2)(1:13)=GLANAM(I,2)(2:13)
                  IF(GLANMP(I,2)(1:1).EQ.':') GLANMP(I,2)(1:13)=GLANMP(I,2)(2:13)
              END DO
C
              BADGLS=.FALSE.
              IF(F22.EQ.1) THEN
                  CALL GLSRES
                  IF(BADGLS) THEN
C     DO THE MESSAGE
                      OUTLYNE=
     1                'WARNING: CURRENT LENS HAS GLASS INPUT ERRORS'
                      CALL SHOWIT(1)
                      OUTLYNE= 'LENS PRESCRIPTION SHOULD BE CHECKED'
                      CALL SHOWIT(1)
                  END IF
C       NO CATALOG SEARCH IS REQUIRED
              END IF
C
C
C       NOW AT LENS INPUT LEVEL, THE LAST SURFACE ENTERED INCREMENTED THE
C       SURFACE COUNT. TO CORRECT THIS, ONLY AT F5=1, DECREASE THE SURFACE COUNT
C       BY 1.0 AND ONLY IF THERE ARE NO SOLVES OR THICKNESS
C     PIKUPS ON THIS LAST SURFACE
              IF(F5.EQ.1) THEN
C     IS THERE A THICKNESS SOLVE OR PIKUP ON SURF INT(SYSTEM1(20))-1?
                  IF(ALENS(32,INT(SYSTEM1(20))-1).NE.0.0D0.OR.
     1            ALENS(33,INT(SYSTEM1(20))-1).NE.0.0D0.OR.ALENS(3,INT(SYSTEM1(20)))
     1            .NE.0.0D0) THEN
C     HAS SOLVES OR PIKUPS OR THICKNESS ON I-1, DON'T REDUCE THE COUNT
                  ELSE
C     NO SOLVES, PIKUPS OR THICKNESS ON I-1, DELETE THE SURFACE I
                      SYSTEM1(20)=SYSTEM1(20)-1.0D0
                  END IF
              END IF
C
              NEWIMG=INT(SYSTEM1(20))
              IF(SYSTEM1(20).LT.0.0D0) SYSTEM1(20)=0.0D0
              NEWIMG=INT(SYSTEM1(20))
              IF(F5.EQ.1.AND.SYSTEM1(20).EQ.0.0D0) THEN
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  OUTLYNE='RETURNED TO CMD LEVEL'
                  CALL SHOWIT(1)
                  F1=1
                  F5=0
                  F22=0
                  SYSTEM1(25)=0.0D0
                  NEWREF=INT(SYSTEM1(25))
                  CALL MACFAL
                  RETURN
              END IF
              IF(F6.EQ.1.AND.SYSTEM1(20).EQ.0.0D0) THEN
                  OUTLYNE='LENS SYSTEM HAS NO SURFACES'
                  CALL SHOWIT(1)
                  OUTLYNE='RETURNED TO CMD LEVEL'
                  CALL SHOWIT(1)
                  F1=1
                  F6=0
                  F22=0
                  SYSTEM1(25)=0.0D0
                  NEWREF=INT(SYSTEM1(25))
                  CALL MACFAL
                  RETURN
              END IF
C***************************************************************
C       ON MAY 11 1990 IT WAS DISCOVERED THAT IF THE RAYTRACE
C       TRACED INTO THE LAST SURFACE (NEWIMG). IF THE INDEX OF
C       NEWIMG WAS NOT = TO NEWIMG-1, THE DIRECTION COSINES WERE
C       CHANGED. THIS WAS FIXED INSIDE HITSUR.FOR AND THE
C       APPROPRIATE PARAXIAL TRACES. THE REFRACIVE INDEX
C       OF SURFACE NEWIMG IS NOW NOT USED IN ANY CALCULATION OF
C       PARAXIAL OR REAL RAY TRACING. FOR NEATNESS, THE NAME OF THE
C       GLASS TYPE OF SURFACE SYSTEM1(20) IS SET TO "IMAGE SURFACE"
C       AND THE INDEX OF REFRACTION IS SET = TO SURFACE SYSTEM1(20)-1
              ALENS(46,INT(SYSTEM1(20)))=ALENS(46,(INT(SYSTEM1(20))-1))
              ALENS(47,INT(SYSTEM1(20)))=ALENS(47,(INT(SYSTEM1(20))-1))
              ALENS(48,INT(SYSTEM1(20)))=ALENS(48,(INT(SYSTEM1(20))-1))
              ALENS(49,INT(SYSTEM1(20)))=ALENS(49,(INT(SYSTEM1(20))-1))
              ALENS(50,INT(SYSTEM1(20)))=ALENS(50,(INT(SYSTEM1(20))-1))
              ALENS(71,INT(SYSTEM1(20)))=ALENS(71,(INT(SYSTEM1(20))-1))
              ALENS(72,INT(SYSTEM1(20)))=ALENS(72,(INT(SYSTEM1(20))-1))
              ALENS(73,INT(SYSTEM1(20)))=ALENS(73,(INT(SYSTEM1(20))-1))
              ALENS(74,INT(SYSTEM1(20)))=ALENS(74,(INT(SYSTEM1(20))-1))
              ALENS(75,INT(SYSTEM1(20)))=ALENS(75,(INT(SYSTEM1(20))-1))
              GLANAM(INT(SYSTEM1(20)),1)=' '
              GLANAM(INT(SYSTEM1(20)),2)='LAST SURFACE'
          ELSE
C       LNSTYP NOT 1
          END IF
C
          IF(LNSTYP.EQ.1) THEN
C
C***************************************************************
C
C       DOES SAX GET THE VALUE OF SAY?
              IF(SYSTEM1(49).NE.1.0D0.AND.SYSTEM1(49).NE.3.0D0) THEN
C               YES IT DOES
                  SYSTEM1(13)=SYSTEM1(12)
C               DON'T RESET SAX EQUAL TO SAY
              END IF
              IF(SYSTEM1(13).EQ.0.0D0.AND.SYSTEM1(12).NE.0.0D0) THEN
C               YES IT DOES
                  SYSTEM1(13)=SYSTEM1(12)
C               DON'T RESET SAX EQUAL TO SAY
              END IF
C     NOW CALCULATE SAY AND SAX FROM NAO OR FNO IF NECESSARY
C
              IF(SYSTEM1(64).EQ.1.0D0) THEN
                  IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                      YTERM=(SYSTEM1(65))/(ALENS(45+INT(SYSTEM1(11)),0))
                  END IF
                  IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                      YTERM=(SYSTEM1(65))/(ALENS(65+INT(SYSTEM1(11)),0))
                  END IF
                  IF(DABS(YTERM).GT.1.0D0) THEN
                      OUTLYNE=
     1                'NEW "NAOY" VALUE WAS NOT REALISTIC'
                      CALL SHOWIT(1)
                      OUTLYNE= '"NAOY" SETTING TURNED OFF'
                      CALL SHOWIT(1)
                      YTERM=1.0D0
                      SYSTEM1(12)=ALENS(3,0)*DTAN(DASIN(YTERM))
                      SYSTEM1(64)=0.0D0
                  ELSE
                      SYSTEM1(12)=ALENS(3,0)*DTAN(DASIN(YTERM))
                  END IF
              END IF
              IF(SYSTEM1(64).EQ.2.0D0) THEN
                  IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                      XTERM=(SYSTEM1(66))/(ALENS(45+INT(SYSTEM1(11)),0))
                  END IF
                  IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                      XTERM=(SYSTEM1(66))/(ALENS(65+INT(SYSTEM1(11)),0))
                  END IF
                  IF(DABS(XTERM).GT.1.0D0) THEN
                      OUTLYNE=
     1                'NEW "NAOX" VALUE WAS NOT REALISTIC'
                      CALL SHOWIT(1)
                      OUTLYNE= '"NAOX" SETTING TURNED OFF'
                      CALL SHOWIT(1)
                      XTERM=1.0D0
                      SYSTEM1(13)=ALENS(3,0)*DTAN(DASIN(XTERM))
                      SYSTEM1(64)=0.0D0
                  ELSE
                      SYSTEM1(13)=ALENS(3,0)*DTAN(DASIN(XTERM))
                  END IF
              END IF
              IF(SYSTEM1(64).EQ.3.0D0) THEN
                  IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                      YTERM=(SYSTEM1(65))/(ALENS(45+INT(SYSTEM1(11)),0))
                      XTERM=(SYSTEM1(66))/(ALENS(45+INT(SYSTEM1(11)),0))
                  END IF
                  IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                      YTERM=(SYSTEM1(65))/(ALENS(65+INT(SYSTEM1(11)),0))
                      XTERM=(SYSTEM1(66))/(ALENS(65+INT(SYSTEM1(11)),0))
                  END IF
                  IF(DABS(YTERM).GT.1.0D0) THEN
                      OUTLYNE=
     1                'NEW "NAOY" VALUE WAS NOT REALISTIC'
                      CALL SHOWIT(1)
                      OUTLYNE= '"NAOY" SETTING TURNED OFF'
                      CALL SHOWIT(1)
                      YTERM=1.0D0
                      SYSTEM1(12)=ALENS(3,0)*DTAN(DASIN(YTERM))
                      SYSTEM1(64)=0.0D0
                  ELSE
                      SYSTEM1(12)=ALENS(3,0)*DTAN(DASIN(YTERM))
                  END IF
                  IF(DABS(XTERM).GT.1.0D0) THEN
                      OUTLYNE=
     1                'NEW "NAOX" VALUE WAS NOT REALISTIC'
                      CALL SHOWIT(1)
                      OUTLYNE= '"NAOX" SETTING TURNED OFF'
                      CALL SHOWIT(1)
                      XTERM=1.0D0
                      SYSTEM1(13)=ALENS(3,0)*DTAN(DASIN(XTERM))
                      SYSTEM1(64)=0.0D0
                  ELSE
                      SYSTEM1(13)=ALENS(3,0)*DTAN(DASIN(XTERM))
                  END IF
              END IF

              IF(SYSTEM1(67).EQ.1.0D0) THEN
                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
              END IF
              IF(SYSTEM1(67).EQ.2.0D0) THEN
                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
              END IF
              IF(SYSTEM1(67).EQ.3.0D0) THEN
                  SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                  SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
              END IF
C
              IF(SYSTEM1(49).NE.2.0D0.AND.SYSTEM1(49).NE.3.0D0) THEN
C     SCX NOT ENTERED EXPLICITLY
                  IF(SYSTEM1(18).EQ.1.0D0) SYSTEM1(19)=1.0D0
                  IF(SYSTEM1(18).EQ.0.0D0) SYSTEM1(19)=0.0D0
C       NO FIX IS NECESARRY
              END IF
C
              IF(DABS(ALENS(3,NEWOBJ)).GE.1.0D10.AND.SYSTEM1(60).EQ.0.0D0
     1        .AND.SYSTEM1(61).EQ.0.0D0) THEN
C     OBJECT AT INFINITY, NO SCY (FANG) OR SCX (FANG) COMMANDS ISSED
C     SET SCY FANG AND SCX FANG TO 1.0 DEGREES
                  SYSTEM1(14)=0.0D0
                  SYSTEM1(15)=0.0D0
                  SYSTEM1(16)=0.0D0
                  SYSTEM1(17)=0.0D0
                  SYSTEM1(21)=1.0D0
                  SYSTEM1(22)=0.0D0
                  SYSTEM1(23)=1.0D0
                  SYSTEM1(24)=0.0D0
                  SYSTEM1(18)=1.0D0
                  SYSTEM1(19)=1.0D0
                  SYSTEM1(51)=0.0D0
                  SYSTEM1(52)=0.0D0
                  SYSTEM1(53)=1.0D0
                  SYSTEM1(54)=1.0D0
              END IF
              IF(DABS(ALENS(3,NEWOBJ)).LT.1.0D10.AND.SYSTEM1(60).EQ.0.0D0
     1        .AND.SYSTEM1(61).EQ.0.0D0) THEN
C     OBJECT NEAR, NO SCY (FANG) OR SCX (FANG) COMMANDS ISSED
C     SET SCY AND SCX TO 1.0 LENS UNIT
                  SYSTEM1(14)=1.0D0
                  SYSTEM1(15)=0.0D0
                  SYSTEM1(16)=1.0D0
                  SYSTEM1(17)=0.0D0
                  SYSTEM1(21)=0.0D0
                  SYSTEM1(22)=0.0D0
                  SYSTEM1(23)=0.0D0
                  SYSTEM1(24)=0.0D0
                  SYSTEM1(18)=0.0D0
                  SYSTEM1(19)=0.0D0
                  SYSTEM1(51)=1.0D0
                  SYSTEM1(52)=1.0D0
                  SYSTEM1(53)=0.0D0
                  SYSTEM1(54)=0.0D0
              END IF
C
              IF(SYSTEM1(18).EQ.0.0D0) THEN
C       CALCULATE SCY FANG FROM SCY BUT SINCE THE DENOMINATOR IN
C       THE CALCULATION IS THE OBJECT THICKNESS, IF IT IS ZERO,
C       SKIP THE CALCULATION AND SET SCY FANG TO 1.0. PRINT
C       A WARNING MESSAGE AS WELL.
C
                  I=0
                  IF(ALENS(3,0).EQ.0.0D0) THEN
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      OUTLYNE=' OBJECT THICKNESS ZERO. SCY FANG SET TO 1.0'
                      CALL SHOWIT(1)
                      OUTLYNE=' OBJECT THICKNESS RESET TO 1.0D20'
                      CALL SHOWIT(1)
                      SYSTEM1(21)=1.0D0
                      ALENS(3,0)=1.0D20
                  ELSE
C     OBJECT THICKNESS NOT ZERO
C       PROCEED WITH SCY FANG CALCULATION
C
                      IF(SYSTEM1(26).NE.-99.0D0) THEN
                          IF(DABS(SYSTEM1(14)).LE.1.0D-15.AND.
     1                    DABS(ALENS(3,0)).LE.1.0D-15) THEN
                              JK_TEMP=0.0D0
                          ELSE
                              JK_TEMP=DATAN2(SYSTEM1(14),ALENS(3,0))
                          END IF
                          SYSTEM1(21)=JK_TEMP

                          IF(ALENS(3,0).GT.0.0D0) THEN
                              IF(SYSTEM1(14).GT.0.0D0) SYSTEM1(21)=-DABS(SYSTEM1(21))
                              IF(SYSTEM1(14).LT.0.0D0) SYSTEM1(21)=DABS(SYSTEM1(21))
                              IF(SYSTEM1(14).EQ.0.0D0) SYSTEM1(21)=0.0D0
                          END IF
                          IF(ALENS(3,0).LT.0.0D0) THEN
                              IF(SYSTEM1(14).GT.0.0D0) SYSTEM1(21)=DABS(SYSTEM1(21))
                              IF(SYSTEM1(14).LT.0.0D0) SYSTEM1(21)=-DABS(SYSTEM1(21))
                              IF(SYSTEM1(14).EQ.0.0D0) SYSTEM1(21)=0.0D0
                          END IF
                      END IF
C
                      IF(SYSTEM1(26).EQ.-99.0D0) THEN
                          IF(DABS(SYSTEM1(14)-SYSTEM1(15)).LE.1.0D-15.AND.
     1                    DABS(ALENS(3,0)).LE.1.0D-15) THEN
                              JK_TEMP=0.0D0
                          ELSE
                              JK_TEMP=DATAN2((SYSTEM1(14)-SYSTEM1(15)),ALENS(3,0))
                          END IF
                          SYSTEM1(21)=JK_TEMP
                          IF(ALENS(3,0).GT.0.0D0) THEN
                              IF((SYSTEM1(14)-SYSTEM1(15)).GT.0.0D0) SYSTEM1(21)=
     1                          -DABS(SYSTEM1(21))
                              IF((SYSTEM1(14)-SYSTEM1(15)).LT.0.0D0) SYSTEM1(21)=
     1                          DABS(SYSTEM1(21))
                              IF((SYSTEM1(14)-SYSTEM1(15)).EQ.0.0D0) SYSTEM1(21)=0.0D0
                          END IF
                          IF(ALENS(3,0).LT.0.0D0) THEN
                              IF((SYSTEM1(14)-SYSTEM1(15)).GT.0.0D0) SYSTEM1(21)=
     1                          DABS(SYSTEM1(21))
                              IF((SYSTEM1(14)-SYSTEM1(15)).LT.0.0D0) SYSTEM1(21)=
     1                          -DABS(SYSTEM1(21))
                              IF((SYSTEM1(14)-SYSTEM1(15)).EQ.0.0D0) SYSTEM1(21)=0.0D0
                          END IF
                      END IF
C SYSTEM1(18)=0 AND OBJECT THICKNESS NOT ZERO
                      SYSTEM1(21)=(180.0D0/PII)*SYSTEM1(21)
C     END OF LOOP FOR OBJECT THICKNESS TEST
                  END IF
              ELSE
C       SYSTEM1(18) NOT EQUAL TO 0.0
              END IF
CC
              IF(SYSTEM1(19).EQ.0.0D0) THEN
C       CALCULATE SCX FANG FROM SCX BUT SINCE THE DENOMINATOR IN
C       THE CALCULATION IS THE OBJECT THICKNESS, IF IT IS ZERO,
C       SKIP THE CALCULATION AND SET SCX FANG TO 1.0. PRINT
C       A WARNING MESSAGE AS WELL.
C
C       SHOULD SCX EQUAL SCY?
                  IF(SYSTEM1(49).NE.2.0D0.AND.SYSTEM1(49).NE.3.0D0) THEN
C       YES IT SHOULD
                      SYSTEM1(16)=SYSTEM1(14)
                      SYSTEM1(17)=SYSTEM1(15)
C       NO, DON'T SET SCX = SCY
                  END IF
C
                  I=0
                  IF(ALENS(3,0).EQ.0.0D0) THEN
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      OUTLYNE=' OBJECT THICKNESS ZERO. SCX FANG SET TO 1.0'
                      CALL SHOWIT(1)
                      OUTLYNE=' OBJECT THICKNESS RESET TO 1.0D20'
                      CALL SHOWIT(1)
                      SYSTEM1(23)=1.0D0
                      ALENS(3,0)=1.0D20
                  ELSE
C       PROCEED WITH SCX FANG CALCULATION
C
                      IF(SYSTEM1(26).NE.-99.0D0)THEN
                          IF(DABS(SYSTEM1(16)).LE.1.0D-15.AND.
     1                    DABS(ALENS(3,0)).LE.1.0D-15) THEN
                              JK_TEMP=0.0D0
                          ELSE
                              JK_TEMP=DATAN2(SYSTEM1(16),ALENS(3,0))
                          END IF
                          SYSTEM1(23)=JK_TEMP
                          IF(ALENS(3,0).GT.0.0D0) THEN
                              IF(SYSTEM1(16).GT.0.0D0) SYSTEM1(23)=-DABS(SYSTEM1(23))
                              IF(SYSTEM1(16).LT.0.0D0) SYSTEM1(23)=DABS(SYSTEM1(23))
                              IF(SYSTEM1(16).EQ.0.0D0) SYSTEM1(23)=0.0D0
                          END IF
                          IF(ALENS(3,0).LT.0.0D0) THEN
                              IF(SYSTEM1(16).GT.0.0D0) SYSTEM1(23)=DABS(SYSTEM1(23))
                              IF(SYSTEM1(16).LT.0.0D0) SYSTEM1(23)=-DABS(SYSTEM1(23))
                              IF(SYSTEM1(16).EQ.0.0D0) SYSTEM1(23)=0.0D0
                          END IF
                      END IF
                      IF(SYSTEM1(26).EQ.-99.0D0)THEN
                          IF(DABS(SYSTEM1(16)-SYSTEM1(17)).LE.1.0D-15.AND.
     1                    DABS(ALENS(3,0)).LE.1.0D-15) THEN
                              JK_TEMP=0.0D0
                          ELSE
                              JK_TEMP=DATAN2((SYSTEM1(16)-SYSTEM1(17)),ALENS(3,0))
                          END IF
                          SYSTEM1(23)=JK_TEMP
                          IF(ALENS(3,0).GT.0.0D0) THEN
                              IF((SYSTEM1(16)-SYSTEM1(17)).GT.0.0D0) SYSTEM1(23)=
     1                         -DABS(SYSTEM1(23))
                              IF((SYSTEM1(16)-SYSTEM1(17)).LT.0.0D0) SYSTEM1(23)=
     1                         DABS(SYSTEM1(23))
                              IF((SYSTEM1(16)-SYSTEM1(17)).EQ.0.0D0) SYSTEM1(23)=0.0D0
                          END IF
                          IF(ALENS(3,0).LT.0.0D0) THEN
                              IF((SYSTEM1(16)-SYSTEM1(17)).GT.0.0D0) SYSTEM1(23)=
     1                         DABS(SYSTEM1(23))
                              IF((SYSTEM1(16)-SYSTEM1(17)).LT.0.0D0) SYSTEM1(23)=
     1                         -DABS(SYSTEM1(23))
                              IF((SYSTEM1(16)-SYSTEM1(17)).EQ.0.0D0) SYSTEM1(23)=0.0D0
                          END IF
                      END IF
                      SYSTEM1(23)=(180.0D0/PII)*SYSTEM1(23)
                  END IF
              ELSE
C       SYSTEM1(19) NOT EQUAL TO 0.0
              END IF
C
              IF(SYSTEM1(18).EQ.1.0D0) THEN
C
C*****************************************************************
C
C       CALCULATE SCY FROM SCY FANG IF REQUIRED
C
                  I=0
                  SYSTEM1(14)=-ALENS(3,0)*
     1            DTAN((PII/180.0D0)*SYSTEM1(21))
                  IF(SYSTEM1(26).EQ.-99.0D0) SYSTEM1(14)=SYSTEM1(14)+SYSTEM1(15)
              END IF
C
C******************************************************************
C
              IF(SYSTEM1(19).EQ.1.0D0) THEN
C
C       CALCULATE SCX FROM SCX FANG IF REQUIRED
C
C       SHOULD SCX FANG EQUAL SCY FANG?
                  IF(SYSTEM1(49).NE.2.0D0.AND.SYSTEM1(49).NE.3.0D0) THEN
C       YES IT SHOULD
                      SYSTEM1(23)=SYSTEM1(21)
                      SYSTEM1(24)=SYSTEM1(22)
C       NO, DON'T SET SCX FANG = SCY FANG
                  END IF

                  I=0
                  SYSTEM1(16)=-ALENS(3,0)*
     1            DTAN((PII/180.0D0)*SYSTEM1(23))
                  IF(SYSTEM1(26).EQ.-99.0D0) SYSTEM1(16)=SYSTEM1(16)+SYSTEM1(17)
              END IF
          ELSE
C     LNSTYP NOT 1
          END IF
C
C
C       SET FLAGS FOR A RETURN TO CMD LEVEL
C
          IF(F5.EQ.1) THEN
              TFSURF=INT(SYSTEM1(20))
              TFTMIN=-0.005D0
              TFTMAX=0.005D0
              TFDELT=0.001D0
              TFDIRECTION= 2
          END IF
          IF(F5.EQ.1) F5=0
          IF(F6.EQ.1) F6=0
          F1=1
          F22=0
C       NOW FLAGS ARE SET TO CMD LEVEL
C
          IF(LNSTYP.EQ.1) THEN
C
C
C********************************************************************
C********************************************************************
C
C       CHECK THAT THERE ARE NO SOLVE RECORDS WHICH OCCUR BEFORE THE
C       APERTURE STOP SURFACE. SOLVES INFRONT OF THE APERTURE
C       STOP SURFACE ARE NOT ALLOWED. THE ASTOP SURFACE IS THE
C       SURFACE FOR WHICH PARAXIAL CHIEF RAYS ARE AIMED. IN THE
C       ABSENCE OF AN APERTURE STOP DEFINITION, SURFACE
C       1 IS THE AIMING SURFACE AND NO SOLVES MAY COME BEFORE SURFACE
C       1.
C**********************************************************************
C
              SLVMRK=INT(SYSTEM1(26))-1
C       IF NO ASTOP DEFINED, SLVMRK WILL BE LESS THAN ZERO
C       SET IT TO 1
              IF(SLVMRK.LT.0) SLVMRK=1
              DO 56 I=0,SLVMRK-1
C       ARE THERE SOLVES?
C
                  IF(ALENS(33,I).NE.0.0D0) THEN
C       FOUND A SOLVE INFRONT OF THE ASTOP SURFACE, REMOVE IT
C       AND PRINT MESSAGE IN SOME CASES
C     NO CHIEF RAY SOLVES IN FRONT OF STOP INCLUDING APY AND APX SOLVES
C     THEN RESOLVE THE VALUE OF ALENS(33,I)

C     CHIEF RAY SOLVES OF ANY TYPE ARE NOT ALLOWED IN FRONT OF THE STOP
                      IF(SOLVE(6,I).EQ.2.0D0) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                     ' :SURFACE PRECEEDS ASTOP SURFACE - PCY-SOLVE DELETED'
                          CALL SHOWIT(1)
                          SOLVE(6,I)=0.0D0
                      END IF
                      IF(SOLVE(4,I).EQ.5.0D0) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                     ' :SURFACE PRECEEDS ASTOP SURFACE - PCX-SOLVE DELETED'
                          CALL SHOWIT(1)
                          SOLVE(4,I)=0.0D0
                      END IF
                      IF(SOLVE(8,I).EQ.4.0D0) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                     ' :SURFACE PRECEEDS ASTOP SURFACE - APCY-SOLVE DELETED'
                          CALL SHOWIT(1)
                          SOLVE(8,I)=0.0D0
                      END IF
                      IF(SOLVE(2,I).EQ.11.0D0) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                     ' :SURFACE PRECEEDS ASTOP SURFACE - APCX-SOLVE DELETED'
                          CALL SHOWIT(1)
                          SOLVE(2,I)=0.0D0
                      END IF
                      IF(SOLVE(8,I).EQ.5.0D0) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                     ' :SURFACE PRECEEDS ASTOP SURFACE - PICY-SOLVE DELETED'
                          CALL SHOWIT(1)
                          SOLVE(8,I)=0.0D0
                      END IF
                      IF(SOLVE(2,I).EQ.12.0D0) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                     ' :SURFACE PRECEEDS ASTOP SURFACE - PICX-SOLVE DELETED'
                          CALL SHOWIT(1)
                          SOLVE(2,I)=0.0D0
                      END IF
                      IF(SOLVE(8,I).EQ.6.0D0) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                     ' :SURFACE PRECEEDS ASTOP SURFACE - PUCY-SOLVE DELETED'
                          CALL SHOWIT(1)
                          SOLVE(8,I)=0.0D0
                      END IF
                      IF(SOLVE(2,I).EQ.13.0D0) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                     ' :SURFACE PRECEEDS ASTOP SURFACE - PUCX-SOLVE DELETED'
                          CALL SHOWIT(1)
                          SOLVE(2,I)=0.0D0
                      END IF
C     IF SAY FLOAT IS IN OPERATION, NO MARGINAL RAY SOLVES ARE ALLOWED
C     IN FRONT OF THE STOP
                      IF(SYSTEM1(83).EQ.1.0D0) THEN
C     SAY FLOAT IN EFFECT
                          IF(SOLVE(6,I).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,*)'SAY FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - PY-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(6,I)=0.0D0
                          END IF
                          IF(SOLVE(6,I).EQ.3.0D0) THEN
                              WRITE(OUTLYNE,*)'SAY FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SAY FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - CAY-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(6,I)=0.0D0
                          END IF
                          IF(SOLVE(8,I).EQ.1.0D0) THEN
                              WRITE(OUTLYNE,*)'SAY FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - APY-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(8,I)=0.0D0
                          END IF
                          IF(SOLVE(8,I).EQ.2.0D0) THEN
                              WRITE(OUTLYNE,*)'SAY FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - PIY-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(8,I)=0.0D0
                          END IF
                          IF(SOLVE(8,I).EQ.3.0D0) THEN
                              WRITE(OUTLYNE,*)'SAY FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - PUY-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(8,I)=0.0D0
                          END IF
                          IF(SOLVE(8,I).EQ.7.0D0) THEN
                              WRITE(OUTLYNE,*)'SAY FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - COCY-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(8,I)=0.0D0
                          END IF
                      ELSE
C     NOT SAY FLOAT
                      END IF
C     IF SAX FLOAT IS IN OPERATION, NO MARGINAL RAY SOLVES ARE ALLOWED
C     IN FRONT OF THE STOP
                      IF(SYSTEM1(84).EQ.1.0D0) THEN
C     SAX FLOAT IN EFFECT
                          IF(SOLVE(4,I).EQ.4.0D0) THEN
                              WRITE(OUTLYNE,*)'SAX FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - PX-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(4,I)=0.0D0
                          END IF
                          IF(SOLVE(4,I).EQ.6.0D0) THEN
                              WRITE(OUTLYNE,*)'SAX FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - CAX-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(6,I)=0.0D0
                          END IF
                          IF(SOLVE(2,I).EQ.8.0D0) THEN
                              WRITE(OUTLYNE,*)'SAX FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - APX-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(2,I)=0.0D0
                          END IF
                          IF(SOLVE(2,I).EQ.9.0D0) THEN
                              WRITE(OUTLYNE,*)'SAX FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - PIX-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(2,I)=0.0D0
                          END IF
                          IF(SOLVE(2,I).EQ.10.0D0) THEN
                              WRITE(OUTLYNE,*)'SAX FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - PUX-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(2,I)=0.0D0
                          END IF
                          IF(SOLVE(2,I).EQ.14.0D0) THEN
                              WRITE(OUTLYNE,*)'SAX FLOAT IS IN EFFECT'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                         ' :SURFACE PRECEEDS ASTOP SURFACE - COCX-SOLVE DELETED'
                              CALL SHOWIT(1)
                              SOLVE(2,I)=0.0D0
                          END IF
                      ELSE
C     NOT SAX FLOAT
                      END IF
C       CALCULATE THE CORRECT VALUE OF ALENS(33,SURF)
C
                      ALENS(33,SURF)=0.0D0
                      IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
                      IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
                      IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
                      IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
C
                      IF(ALENS(33,I).EQ.0.0D0) THEN
                          WRITE(OUTLYNE,*)'SURFACE',SURF,
     1                     'NO SOLVES REMAIN ON SURFACE'
                          CALL SHOWIT(1)
                          ALENS(33,I)=0.0D0
C       TESTED SURFACE DOES NOT HAVE SOLVE, PROCEED
                      END IF
                  END IF
 56           CONTINUE
C
C       SOLVE/ASTOP INCONSISTENCIES REMOVED
C
C
C********************************************************************
C     THIS WAS MOVED HERE AFTER PIKUPS TO MAKE REFL WORK WITH
C     PIKUP GLASS
C
C       THE NEXT ITEM TO REQUIRE RESOLUTION IS THE PSUDO-PIKUP
C       OF THE REFRACTIVE INDICES FOR REFLECTIVE SURFACES.
C       A REFLECTIVE SURFACE ALWAYS REFLECTS BACK INTO THE
C       MEDIA IT IS EMERSED IN AND THUS IF SURFACE I+1
C       IS DEFINED AS "REFL" THEN ITS REFRACTIVE INDICES
C       WILL ALWAYS BE EQUAL IN MAGNITUDE BUT OPPOSITE IN
C       ALGEBRAIC SIGN TO THE REFRACTIVE INDICES OF SUFACE
C       I. AT THIS POINT THE LENS IS SEARCHED FROM SURFACE 0
C       TO THE (IMAGE SURFACE NUMBER )
C       (SURFACE NUMBER = INT(SYSTEM1(20))-1).
C       IF A SURFACE IS FOUND TO HAVE A GLANAM(SURF,1)='REFL'
C       THEN FOR THAT SURFACE THE INDICES STORED IN
C       ALENS(46,SURF) THROUGH ALENS(50,SURF) ARE SET EQUAL TO BUT
C     (AND 71 TO 75 FOR WAVELENGTHS 6 TO 10)
C       OPPOSITE IN SIGN TO THE CORRESPONDING VALUES ON SURFACE
C       (SURF-1). THEN A RETURN TO THE CMD LEVEL IS PERFORMED.
C       RESOLUTION OF REFRACTIVE INDICES IN PRESENTS OF
C       REFLECTIONS

C       ON 8/8/89 IT WAS DISCOVERED THAT IF A SYSTEM HAD
C       ORIGINALLY HAD REFLECTIVE SURFACES AND SOME OR ALL WERE
C       CHANGED TO NONE REFLECTIVE, THE NEGATIVE INDICES
C       AFTER THAT REFLECTIVE SURFACE WERE NOT CORRECTLY MADE
C       POSITIVE. BY RE-SETTING ALL INDICES POSITIVE, THE SUBROUTINE
C       LNSEOS COULD THEN GIVE THE CORRECT SIGNS TO ALL REFRACTIVE
C       INDICES.
              K=INT(SYSTEM1(20))
              ALENS(46:50,1:K)=DABS(ALENS(46:50,1:K))
              ALENS(71:75,1:K)=DABS(ALENS(71:75,1:K))
C
              F21=0
              DO 10 I=1,INT(SYSTEM1(20))
                  IF(GLANAM(I,2).EQ.'REFL'.OR.GLANAM(I,2).EQ.'REFLTIRO') THEN
                      IF(F21.EQ.1.OR.F21.EQ.0) THEN
                          F21=-1
                          ALENS(46:50,I)=ALENS(46:50,(I-1))
                          ALENS(71:75,I)=ALENS(71:75,(I-1))
                      ELSE
                          IF(F21.EQ.-1)THEN
                              F21=1
                              ALENS(46:50,I)=ALENS(46:50,(I-1))
                              ALENS(71:75,I)=ALENS(71:75,(I-1))
                          END IF
                      END IF
                  END IF
                  IF(F21.EQ.0) GO TO 10
                  IF(F21.EQ.-1) THEN
                      IF(ALENS(46,I).GT.0.0D0)  ALENS(46,I)=-ALENS(46,I)
                      IF(ALENS(47,I).GT.0.0D0)  ALENS(47,I)=-ALENS(47,I)
                      IF(ALENS(48,I).GT.0.0D0)  ALENS(48,I)=-ALENS(48,I)
                      IF(ALENS(49,I).GT.0.0D0)  ALENS(49,I)=-ALENS(49,I)
                      IF(ALENS(50,I).GT.0.0D0)  ALENS(50,I)=-ALENS(50,I)
                      IF(ALENS(71,I).GT.0.0D0)  ALENS(71,I)=-ALENS(71,I)
                      IF(ALENS(72,I).GT.0.0D0)  ALENS(72,I)=-ALENS(72,I)
                      IF(ALENS(73,I).GT.0.0D0)  ALENS(73,I)=-ALENS(73,I)
                      IF(ALENS(74,I).GT.0.0D0)  ALENS(74,I)=-ALENS(74,I)
                      IF(ALENS(75,I).GT.0.0D0)  ALENS(75,I)=-ALENS(75,I)
                  END IF
                  IF(F21.EQ.1) THEN
                      IF(ALENS(46,I).LT.0.0D0)ALENS(46,I)=DABS(ALENS(46,I))
                      IF(ALENS(47,I).LT.0.0D0)ALENS(47,I)=DABS(ALENS(47,I))
                      IF(ALENS(48,I).LT.0.0D0)ALENS(48,I)=DABS(ALENS(48,I))
                      IF(ALENS(49,I).LT.0.0D0)ALENS(49,I)=DABS(ALENS(49,I))
                      IF(ALENS(50,I).LT.0.0D0)ALENS(50,I)=DABS(ALENS(50,I))
                      IF(ALENS(71,I).LT.0.0D0)ALENS(71,I)=DABS(ALENS(71,I))
                      IF(ALENS(72,I).LT.0.0D0)ALENS(72,I)=DABS(ALENS(72,I))
                      IF(ALENS(73,I).LT.0.0D0)ALENS(73,I)=DABS(ALENS(73,I))
                      IF(ALENS(74,I).LT.0.0D0)ALENS(74,I)=DABS(ALENS(74,I))
                      IF(ALENS(75,I).LT.0.0D0)ALENS(75,I)=DABS(ALENS(75,I))
                  END IF
 10           CONTINUE
C
C********************************************************************
C
C       NOW CHECK FOR THE PRESENCE OF COCY OR COCX SOLVES WHICH
C       REFERENCE A SURFACE NUMBER LARGER (FARTHER DOWN THE LENS)
C       THAN ITS OWN NUMBER. IF FOUND, CHECK THAT THERE ARE NO INTERVINING
C       PAXAXIAL THICKNESS SOLVES. IF ANY ARE FOUND, REMOVE THE
C       COCY OF COCX SOLVE AND PRINT A WARNING MESSAGE.
C
              DO 803 I=1,INT(SYSTEM1(20))
                  IF(SOLVE(8,I).EQ.7.0D0.OR.
     1            SOLVE(2,I).EQ.14.0D0) THEN
C       FOUND A COCY OR COCX SOLVE
C       DOES IT REFER TO A FOLLOWING SURFACE?
                      IF(SOLVE(9,I).GT.I.OR.
     1                SOLVE(1,I).GT.I) THEN
C       YES IT DOES,PROCEED WITH PROCESSING
C       ARE THERE THICKNESS SOLVES BETWEEN THE COCY/COCX SOLVE
C       AND THE REFERENCED SURFACE?
                          ITT=I
                          IF(SOLVE(8,I).EQ.7.0D0) IFF=INT(SOLVE(9,I))-1
                          IF(SOLVE(2,I).EQ.14.0D0) IFF=INT(SOLVE(1,I))-1
                          TESTIT=0
                          DO 804 J=ITT,IFF
                              IF(SOLVE(6,J).NE.0.0D0.OR.
     1                        SOLVE(4,J).NE.0.0D0) THEN
                                  TESTIT=TESTIT+1
C       NO THICKNESS SOLVE FOUND,CONTINUE
                              END IF
 804                      CONTINUE
C       IF TESTIT NOT ZERO, THEN THERE WAS AN INTERVINING
C       THICKNESS SOLVE. IN THIS CASE THE COCY OR COCX SOLVE MUST GO.
                          IF(TESTIT.NE.0) THEN
C       REMOVE THE SOLVE AND PRINT AN ERROR MESSAGE
                              IF(SOLVE(8,I).EQ.7.0D0) THEN
                                  OUTLYNE=
     1                            '(COCY) SOLVE CONFLICTS WITH A FOLLOWING THICKNESS SOLVE'
                                  CALL SHOWIT(1)
                                  OUTLYNE='(COCY) SOLVE DELETED'
                                  CALL SHOWIT(1)
                                  SOLVE(8,I)=0.0D0
                                  SOLVE(9,I)=0.0D0
                                  ALENS(33,I)=ALENS(33,I)-2.0D0
C       NOT A COCY SOLVE,PROCEED
                              END IF
                              IF(SOLVE(2,I).EQ.14.0D0) THEN
                                  OUTLYNE=
     1                            '(COCX) SOLVE CONFLICTS WITH A FOLLOWING THICKNESS SOLVE'
                                  CALL SHOWIT(1)
                                  OUTLYNE='(COCX) SOLVE DELETED'
                                  CALL SHOWIT(1)
                                  SOLVE(2,I)=0.0D0
                                  SOLVE(1,I)=0.0D0
                                  ALENS(33,I)=ALENS(33,I)-2.0D0
C       NOT A COCX SOLVE,PROCEED
                              END IF
C       TESTIT IS ZERO, NO SOLVE TO REMOVE, PROCEED
                          END IF
C       NO TO DOES NOT,CONTINUE
                      END IF
C       NO COCY OR COCX SOLVE FOUND, CONTINUE
                  END IF
 803          CONTINUE
          ELSE
C     NOT LNSTYP=1
          END IF
C
C       NOW CHECK FOR FNBY AND FNBX HOLDS AND CHECK THE MODE.
C       IF NOT FOCAL OR UFOCAL, PRINT MESSAGE THAT HOLD WAS IGNORED.
C       CHECK FOR FNBY OR FNBX HOLD
          IF(SYSTEM1(44).EQ.-1.0D0.AND.SYSTEM1(83).EQ.0.0D0.AND.
     1    SYSTEM1(84).EQ.0.0D0) THEN
C               THERE IS AN F NUMBER HOLD (YZ PLANE)
C       CHECK LENS MODE, MUST BE FOCAL OR UFOCAL
              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       HOLD PERFORMED. FIRST PERFORM A PARAXIAL TRACE TO
C       GET STARTING VARLUES
                  ITYPEP=1
C       TELECENTRIC STUFF, 11/12/2000
                  IF(SYSTEM1(63).EQ.1.0D0) THEN
                      IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                          OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                          CALL SHOWIT(1)
                          OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                          CALL SHOWIT(1)
                          OUTLYNE='VALUES'
                          CALL SHOWIT(1)
                          OUTLYNE='PARAXIAL TRACE STOPPED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                          IF(SYSTEM1(64).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                          END IF
                          IF(SYSTEM1(64).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(64).EQ.2.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(67).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                          END IF
                          IF(SYSTEM1(67).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                          IF(SYSTEM1(67).EQ.3.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                      END IF
                  END IF
C
                  CALL PRTRA
                  ITYPEP=1
                  CALL FADJ
              ELSE
                  IF(LNSTYP.EQ.1) THEN
C       MODE NOT CORRECT
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "FNBY HLD" IS IN EFFECT.'
                      CALL SHOWIT(1)
                      OUTLYNE='"FNBY HLD" REQUIRES THE FOCAL OR UFOCAL MODES.'
                      CALL SHOWIT(1)
                      IF(SYSTEM1(30).EQ.3.0D0) THEN
                          OUTLYNE='CURRENT MODE IS AFOCAL.'
                          CALL SHOWIT(1)
                      END IF
                      IF(SYSTEM1(30).EQ.4.0D0) THEN
                          OUTLYNE='CURRENT MODE IS UAFOCAL'
                          CALL SHOWIT(1)
                      END IF
                      OUTLYNE='"FNBY HLD" NOT PERFORMED.'
                      CALL SHOWIT(1)
                  END IF
              END IF
C       NO FNBY HLD IN EFFECT
          END IF
          IF(SYSTEM1(45).EQ.-1.0D0.AND.SYSTEM1(83).EQ.0.0D0.AND.
     1    SYSTEM1(84).EQ.0.0D0) THEN
C               THERE IS AN F NUMBER HOLD (XZ PLANE)
C       CHECK LENS MODE, MUST BE FOCAL OR UFOCAL
              IF(SYSTEM1(30).EQ.1.0D0.OR.SYSTEM1(30).EQ.2.0D0) THEN
C       HOLD PERFORMED. FIRST PERFORM A PARAXIAL TRACE TO
C       GET STARTING VALUES.
                  ITYPEP=3
C       TELECENTRIC STUFF, 11/12/2000
                  IF(SYSTEM1(63).EQ.1.0D0) THEN
                      IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                          OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                          CALL SHOWIT(1)
                          OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                          CALL SHOWIT(1)
                          OUTLYNE='VALUES'
                          CALL SHOWIT(1)
                          OUTLYNE='PARAXIAL TRACE STOPPED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                          IF(SYSTEM1(64).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                          END IF
                          IF(SYSTEM1(64).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(64).EQ.3.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(67).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                          END IF
                          IF(SYSTEM1(67).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                          IF(SYSTEM1(67).EQ.3.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                      END IF
                  END IF
C
                  CALL PRTRA
                  ITYPEP=2
                  CALL FADJ
              ELSE
C       MODE NOT CORRECT
                  IF(LNSTYP.EQ.1) THEN
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "FNBX HLD" IS IN EFFECT.'
                      CALL SHOWIT(1)
                      OUTLYNE='"FNBX HLD" REQUIRES THE FOCAL OR UFOCAL MODES.'
                      CALL SHOWIT(1)
                      IF(SYSTEM1(30).EQ.3.0D0) THEN
                          OUTLYNE='CURRENT MODE IS AFOCAL.'
                          CALL SHOWIT(1)
                      END IF
                      IF(SYSTEM1(30).EQ.4.0D0) THEN
                          OUTLYNE='CURRENT MODE IS UAFOCAL'
                          CALL SHOWIT(1)
                      END IF
                      OUTLYNE='"FNBX HLD" NOT PERFORMED.'
                      CALL SHOWIT(1)
                  END IF
              END IF
C       NO FNBX HLD IN EFFECT
          END IF
C       NOW CHECK FOR ERY AND ERX HOLDS AND CHECK THE MODE.
C       IF NOT FOCAL OR UFOCAL, PRINT MESSAGE THAT HOLD WAS IGNORED.
C       CHECK FOR FNBY OR FNBX HOLD
          IF(SYSTEM1(44).EQ.-2.0D0.AND.SYSTEM1(83).EQ.0.0D0.AND.
     1    SYSTEM1(84).EQ.0.0D0) THEN
C               THERE IS AN EXIT PUPIL RADIUS HOLD (YZ PLANE)
C       CHECK LENS MODE, MUST BE AFOCAL OR UAFOCAL
              IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
C       HOLD PERFORMED. FIRST PERFORM A PARAXIAL TRACE TO GET
C       STATRING VALUES.
                  ITYPEP=1
C       TELECENTRIC STUFF, 11/12/2000
                  IF(SYSTEM1(63).EQ.1.0D0) THEN
                      IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                          OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                          CALL SHOWIT(1)
                          OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                          CALL SHOWIT(1)
                          OUTLYNE='VALUES'
                          CALL SHOWIT(1)
                          OUTLYNE='PARAXIAL TRACE STOPPED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                          IF(SYSTEM1(64).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                          END IF
                          IF(SYSTEM1(64).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(64).EQ.3.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(67).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                          END IF
                          IF(SYSTEM1(67).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                          IF(SYSTEM1(67).EQ.3.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                      END IF
                  END IF
C
                  CALL PRTRA
                  ITYPEP=1
                  CALL ERADJ
              ELSE
                  IF(LNSTYP.EQ.1) THEN
C       MODE NOT CORRECT
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "ERY HLD" IS IN EFFECT.'
                      CALL SHOWIT(1)
                      OUTLYNE='"ERY HLD" REQUIRES THE AFOCAL OR UAFOCAL MODES.'
                      CALL SHOWIT(1)
                      IF(SYSTEM1(30).EQ.1.0D0) THEN
                          OUTLYNE='CURRENT MODE IS FOCAL.'
                          CALL SHOWIT(1)
                      END IF
                      IF(SYSTEM1(30).EQ.2.0D0) THEN
                          OUTLYNE='CURRENT MODE IS UFOCAL'
                          CALL SHOWIT(1)
                      END IF
                      OUTLYNE='"ERY HLD" NOT PERFORMED.'
                      CALL SHOWIT(1)
                  END IF
              END IF
C       NO ERY HLD IN EFFECT
          END IF
          IF(SYSTEM1(45).EQ.-2.0D0.AND.SYSTEM1(83).EQ.0.0D0.AND.
     1    SYSTEM1(84).EQ.0.0D0) THEN
C               THERE IS AN EXIT PUPIL RADIUS HOLD (XZ PLANE)
C       CHECK LENS MODE, MUST BE AFOCAL OR UAFOCAL
              IF(SYSTEM1(30).EQ.3.0D0.OR.SYSTEM1(30).EQ.4.0D0) THEN
C       HOLD PERFORMED. FIRST PERFORM A PARAXIAL TRACE TO GET
C       STARTING VALUES.
                  ITYPEP=3
C       TELECENTRIC STUFF, 11/12/2000
                  IF(SYSTEM1(63).EQ.1.0D0) THEN
                      IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                          OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                          CALL SHOWIT(1)
                          OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                          CALL SHOWIT(1)
                          OUTLYNE='VALUES'
                          CALL SHOWIT(1)
                          OUTLYNE='PARAXIAL TRACE STOPPED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                          IF(SYSTEM1(64).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                          END IF
                          IF(SYSTEM1(64).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(64).EQ.3.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(67).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                          END IF
                          IF(SYSTEM1(67).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                          IF(SYSTEM1(67).EQ.3.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                      END IF
                  END IF
C
                  CALL PRTRA
                  ITYPEP=2
                  CALL ERADJ
              ELSE
                  IF(LNSTYP.EQ.1) THEN
C       MODE NOT CORRECT
                      OUTLYNE='WARNING:'
                      CALL SHOWIT(1)
                      OUTLYNE='AN "ERX HLD" IS IN EFFECT.'
                      CALL SHOWIT(1)
                      OUTLYNE='"ERX HLD" REQUIRES THE AFOCAL OR UAFOCAL MODES.'
                      CALL SHOWIT(1)
                      IF(SYSTEM1(30).EQ.1.0D0) THEN
                          OUTLYNE='CURRENT MODE IS AFOCAL.'
                          CALL SHOWIT(1)
                      END IF
                      IF(SYSTEM1(30).EQ.2.0D0) THEN
                          OUTLYNE='CURRENT MODE IS UAFOCAL'
                          CALL SHOWIT(1)
                      END IF
                      OUTLYNE='"ERX HLD" NOT PERFORMED.'
                      CALL SHOWIT(1)
                  END IF
              END IF
C       NO ERX HLD IN EFFECT
          END IF

C
C       NOW WE GO TO THE PARAXIAL RAYTRACE AND
C       PERFORM ENOUGH PARAXIAL RAYTRACING TO AIM
C       RAYS TO THE STOP, RESOLVE SOLVES, AND RESOLVE
C       STOP ADJUSTMENTS AND PARAXIAL BASED HOLDS.
C       WE START BY BRANCHING OUT TO THE PAXAXIAL TRACE
C       BY CALLING PRTRA1. ALL CURRANT VARIABLES ARE
C       PASSED VIA COMMONS TO PRTRA1 AND PRTRA2.
C
C       PRTRA1 DOES THE YZ PLANE TRACE
C       PRTRA2 DOES THE XZ PLANE TRACE
C       AN XZ TRACE IS ALWAYS PERFORMED AND IS AVALIABLE
C       EVEN WHEN TORICS ARE NOT PRESENT
C
C
C       PIKUPS ARE RESOLVED BY (PIKRES) FOR EACH LENS SURFACE
C       GOING FROM SURFACE 0 TO IMMAGE SURFACE. THIS ACTION
C       IS TAKEN FROM WITHIN THE PARAXIAL RAYTRACE SUBROUTINES.
          ITYPEP=3
C       TELECENTRIC STUFF, 11/12/2000
          IF(SYSTEM1(63).EQ.1.0D0) THEN
              IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                  OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                  CALL SHOWIT(1)
                  OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                  CALL SHOWIT(1)
                  OUTLYNE='VALUES'
                  CALL SHOWIT(1)
                  OUTLYNE='PARAXIAL TRACE STOPPED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  IF(SYSTEM1(64).EQ.1.0D0) THEN
                      SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                  END IF
                  IF(SYSTEM1(64).EQ.2.0D0) THEN
                      SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                  END IF
                  IF(SYSTEM1(64).EQ.3.0D0) THEN
                      SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                      SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                  END IF
                  IF(SYSTEM1(67).EQ.1.0D0) THEN
                      SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                  END IF
                  IF(SYSTEM1(67).EQ.2.0D0) THEN
                      SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                  END IF
                  IF(SYSTEM1(67).EQ.3.0D0) THEN
                      SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                      SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                  END IF
              END IF
          END IF
C
          CALL PRTRA
C
          IF(SYSTEM1(83).NE.0.0D0.AND.SYSTEM1(26).NE.-99.0D0.OR.
     1    SYSTEM1(84).NE.0.0D0.AND.SYSTEM1(26).NE.-99.0D0) THEN
              IF(ALENS(9,INT(SYSTEM1(26))).EQ.0.0D0.OR.
     1        ALENS(127,INT(SYSTEM1(26))).NE.0.0D0) THEN
C     NO ADJUSTMENTS TO SAY AND SAX ARE DONE, NO CLAP IS ON
C     STOP SURFACE
              ELSE
                  IF(ALENS(9,INT(SYSTEM1(26))).EQ.1.0D0.AND.
     1            ALENS(127,INT(SYSTEM1(26))).EQ.0.0D0) THEN
                      IF(ALENS(10,INT(SYSTEM1(26))).LE.
     1                ALENS(11,INT(SYSTEM1(26)))) THEN
                          YCLAP=ALENS(10,INT(SYSTEM1(26)))/DABS(PXTRAY(1,INT(SYSTEM1(26))))
                          XCLAP=ALENS(10,INT(SYSTEM1(26)))/DABS(PXTRAX(1,INT(SYSTEM1(26))))
                      ELSE
                          YCLAP=ALENS(11,INT(SYSTEM1(26)))/DABS(PXTRAY(1,INT(SYSTEM1(26))))
                          XCLAP=ALENS(11,INT(SYSTEM1(26)))/DABS(PXTRAX(1,INT(SYSTEM1(26))))
                      END IF
                  END IF
                  IF(ALENS(9,INT(SYSTEM1(26))).EQ.5.0D0.AND.
     1            ALENS(127,INT(SYSTEM1(26))).EQ.0.0D0) THEN
                      YCLAP=ALENS(10,INT(SYSTEM1(26)))/DABS(PXTRAY(1,INT(SYSTEM1(26))))
                      XCLAP=ALENS(10,INT(SYSTEM1(26)))/DABS(PXTRAX(1,INT(SYSTEM1(26))))
                  END IF
                  IF(ALENS(9,INT(SYSTEM1(26))).GT.1.0D0.AND.
     1            ALENS(9,INT(SYSTEM1(26))).LE.4.0D0.AND.
     1            ALENS(127,INT(SYSTEM1(26))).EQ.0.0D0) THEN
                      YCLAP=ALENS(10,INT(SYSTEM1(26)))/DABS(PXTRAY(1,INT(SYSTEM1(26))))
                      XCLAP=ALENS(11,INT(SYSTEM1(26)))/DABS(PXTRAX(1,INT(SYSTEM1(26))))
                  END IF
                  IF(SYSTEM1(83).NE.0.0D0) THEN
                      SYSTEM1(12)=SYSTEM1(12)*YCLAP
                  END IF
                  IF(SYSTEM1(83).NE.0.0D0) THEN
                      SYSTEM1(13)=SYSTEM1(13)*XCLAP
                  END IF
                  ITYPEP=3
C       TELECENTRIC STUFF, 11/12/2000
                  IF(SYSTEM1(63).EQ.1.0D0) THEN
                      IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                          OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                          CALL SHOWIT(1)
                          OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                          CALL SHOWIT(1)
                          OUTLYNE='VALUES'
                          CALL SHOWIT(1)
                          OUTLYNE='PARAXIAL TRACE STOPPED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                          IF(SYSTEM1(64).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                          END IF
                          IF(SYSTEM1(64).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(64).EQ.3.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(67).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                          END IF
                          IF(SYSTEM1(67).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                          IF(SYSTEM1(67).EQ.3.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                      END IF
                  END IF
C
                  CALL PRTRA

                  IF(ALENS(9,INT(SYSTEM1(26))).EQ.1.0D0.AND.
     1            ALENS(127,INT(SYSTEM1(26))).EQ.0.0D0) THEN
                      IF(ALENS(10,INT(SYSTEM1(26))).LE.
     1                ALENS(11,INT(SYSTEM1(26)))) THEN
                          YCLAP=ALENS(10,INT(SYSTEM1(26)))/DABS(PXTRAY(1,INT(SYSTEM1(26))))
                          XCLAP=ALENS(10,INT(SYSTEM1(26)))/DABS(PXTRAX(1,INT(SYSTEM1(26))))
                      ELSE
                          YCLAP=ALENS(11,INT(SYSTEM1(26)))/DABS(PXTRAY(1,INT(SYSTEM1(26))))
                          XCLAP=ALENS(11,INT(SYSTEM1(26)))/DABS(PXTRAX(1,INT(SYSTEM1(26))))
                      END IF
                  END IF
                  IF(ALENS(9,INT(SYSTEM1(26))).EQ.5.0D0.AND.
     1            ALENS(127,INT(SYSTEM1(26))).EQ.0.0D0) THEN
                      YCLAP=ALENS(10,INT(SYSTEM1(26)))/DABS(PXTRAY(1,INT(SYSTEM1(26))))
                      XCLAP=ALENS(10,INT(SYSTEM1(26)))/DABS(PXTRAX(1,INT(SYSTEM1(26))))
                  END IF
                  IF(ALENS(9,INT(SYSTEM1(26))).GT.1.0D0.AND.
     1            ALENS(9,INT(SYSTEM1(26))).LE.4.0D0.AND.
     1            ALENS(127,INT(SYSTEM1(26))).EQ.0.0D0) THEN
                      YCLAP=ALENS(10,INT(SYSTEM1(26)))/DABS(PXTRAY(1,INT(SYSTEM1(26))))
                      XCLAP=ALENS(11,INT(SYSTEM1(26)))/DABS(PXTRAX(1,INT(SYSTEM1(26))))
                  END IF
                  IF(SYSTEM1(83).NE.0.0D0) THEN
                      SYSTEM1(12)=SYSTEM1(12)*YCLAP
                  END IF
                  IF(SYSTEM1(83).NE.0.0D0) THEN
                      SYSTEM1(13)=SYSTEM1(13)*XCLAP
                  END IF
                  ITYPEP=3
C       TELECENTRIC STUFF, 11/12/2000
                  IF(SYSTEM1(63).EQ.1.0D0) THEN
                      IF(SYSTEM1(64).EQ.0.0D0.AND.SYSTEM1(67).EQ.0.0D0) THEN
                          OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                          CALL SHOWIT(1)
                          OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                          CALL SHOWIT(1)
                          OUTLYNE='VALUES'
                          CALL SHOWIT(1)
                          OUTLYNE='PARAXIAL TRACE STOPPED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
                          IF(SYSTEM1(64).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                          END IF
                          IF(SYSTEM1(64).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(64).EQ.3.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)*SYSTEM1(65)
                              SYSTEM1(13)=ALENS(3,0)*SYSTEM1(66)
                          END IF
                          IF(SYSTEM1(67).EQ.1.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                          END IF
                          IF(SYSTEM1(67).EQ.2.0D0) THEN
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                          IF(SYSTEM1(67).EQ.3.0D0) THEN
                              SYSTEM1(12)=ALENS(3,0)/(2.0D0*SYSTEM1(68))
                              SYSTEM1(13)=ALENS(3,0)/(2.0D0*SYSTEM1(69))
                          END IF
                      END IF
                  END IF
C
                  CALL PRTRA
              END IF
          END IF
C
C       NOW ALL PARAXIAL BASED CHANGES TO THE LENS HAVE BEEN
C       MADE.
C
C***************************************************************
C       NOW RESOLVE THE  SYMMETRY SWITCH SETTING
C     XZ-PLANE
C       SYSTEM1(28)=1.0D0 MEANS XZ SYMMETRY
C       SYSTEM1(28)=0.0D0 MEANS NO XZ SYMMETRY
          SYSTEM1(28)=1.0D0
C       A SETTING OF 1.0 MEANS ROTATIONAL SYMMETRY. 0 MEAN
C       NO ROTATIONAL SYMMETRY.
          DO III=0,INT(SYSTEM1(20))
C
C       IF THERE IS AN XD, BETA OR GAMMA NON-ZERO
C       SET SYSTEM1(28)=0.0D0
              IF(ALENS(27,III).NE.0.0D0.OR.
     1        ALENS(28,III).NE.0.0D0.OR.
     2        ALENS(31,III).NE.0.0D0) THEN
                  SYSTEM1(28)=0.0D0
                  GO TO 628
              END IF
C       IF A NON-ROTATIONALLY SYMMETRIC SPECIAL SURFACE EXISTS
C       AT THIS TIME (SEPT 1989) ONLY TYPE 0 (NONE) AND TYPE 1
C       (ROTATIONAL POLYNOMIAL) ARE ROTATIONALLY SYMMETRIC.
              IF(DABS(ALENS(34,III)).NE.0.0D0.AND.
     1        DABS(ALENS(34,III)).NE.1.0D0) THEN
                  SYSTEM1(28)=0.0D0
                  GO TO 628
              END IF
          END DO
C       REFS ORIENTATION NOT 0.0 OR CLAP TILT EXITS ON THE
C       REFERENCE SURFACE
          IF(ALENS(9,NEWREF).NE.0.0D0.AND.ALENS(15,NEWREF).NE.0.0D0
     1    .AND.ALENS(127,NEWREF).EQ.0.0D0.OR.
     1    ALENS(127,NEWREF).EQ.0.0D0
     1    .AND.SYSTEM1(59).NE.0.0D0) SYSTEM1(28)=0.0D0
 628      CONTINUE
C     YZ-PLANE
C       SYSTEM1(48)=1.0D0 MEANS YZ SYMMETRY
C       SYSTEM1(48)=0.0D0 MEANS NO YZ SYMMETRY
          SYSTEM1(48)=1.0D0
C       A SETTING OF 1.0 MEANS ROTATIONAL SYMMETRY. 0 MEAN
C       NO ROTATIONAL SYMMETRY.
          DO III=0,INT(SYSTEM1(20))
C
C       IF THERE IS AN YD, ALPHA OR GAMMA NON-ZERO
C       SET SYSTEM1(48)=0.0D0
              IF(ALENS(118,III).NE.0.0D0.OR.
     1        ALENS(120,III).NE.0.0D0.OR.
     2        ALENS(115,III).NE.0.0D0) THEN
                  SYSTEM1(48)=0.0D0
                  GO TO 629
              END IF
C       IF A NON-ROTATIONALLY SYMMETRIC SPECIAL SURFACE EXISTS
C       AT THIS TIME (SEPT 1989) ONLY TYPE 0 (NONE) AND TYPE 1
C       (ROTATIONAL POLYNOMIAL) ARE ROTATIONALLY SYMMETRIC.
              IF(DABS(ALENS(34,III)).NE.0.0D0.AND.
     1        DABS(ALENS(34,III)).NE.1.0D0) THEN
                  SYSTEM1(48)=0.0D0
                  GO TO 629
              END IF
          END DO
 629      CONTINUE
C*******************************************************************
C
C       NOW IF F12=1, MAIN CONFIG, THEN MAKE THE PERMANENT LENS RECORD
C       EQUAL TO THE CURRENT DATA UNLESS LNSTYP = 3
          IF(LNSTYP.EQ.1.OR.LNSTYP.EQ.2) THEN
              IF(F12.EQ.1) THEN
                  CALL CTOP
C************************************************************
C               PERMANENT LENS UPDATED
C       F12 NOT 1, PROCEED
              END IF
          END IF
C     NOW UPDATE DUMMM
C     SURFACES 0 AND INT(SYSTEM1(20)) ARE NEVER DUMMIES
          DUMMMY(0)=.FALSE.
          DUMMMY(INT(SYSTEM1(20)))=.FALSE.
C
          DO ZZTOP=1,INT(SYSTEM1(20))-1
              DUMMMY(ZZTOP)=.FALSE.
              IF(GLANAM(ZZTOP,1).EQ.GLANAM((ZZTOP-1),1).AND.
     1        GLANAM(ZZTOP,2).EQ.GLANAM((ZZTOP-1),2).AND.
     2        GLANAM(ZZTOP,2).NE.'REFL'.AND.
     2        GLANAM(ZZTOP,2).NE.'REFLTIRO'.AND.
     2        GLANAM(ZZTOP,2).NE.'IDEAL'.AND.
     3        GLANAM(ZZTOP,2).NE.'PERFECT      ') DUMMMY(ZZTOP)=.TRUE.
              IF(GLANAM(ZZTOP,1).EQ.'MODEL        ') DUMMMY(ZZTOP)=.FALSE.
              IF(GLANAM(ZZTOP-1,1).EQ.'MODEL        ') DUMMMY(ZZTOP)=.FALSE.
              IF(ALENS(46,ZZTOP).EQ.ALENS(46,ZZTOP-1).AND.
     1           ALENS(47,ZZTOP).EQ.ALENS(47,ZZTOP-1).AND.
     1           ALENS(48,ZZTOP).EQ.ALENS(48,ZZTOP-1).AND.
     1           ALENS(49,ZZTOP).EQ.ALENS(49,ZZTOP-1).AND.
     1           ALENS(71,ZZTOP).EQ.ALENS(71,ZZTOP-1).AND.
     1           ALENS(72,ZZTOP).EQ.ALENS(72,ZZTOP-1).AND.
     1           ALENS(73,ZZTOP).EQ.ALENS(73,ZZTOP-1).AND.
     1           ALENS(74,ZZTOP).EQ.ALENS(74,ZZTOP-1).AND.
     1           ALENS(75,ZZTOP).EQ.ALENS(75,ZZTOP-1).AND.
     1           ALENS(50,ZZTOP).EQ.ALENS(50,ZZTOP-1)) DUMMMY(ZZTOP)=.TRUE.
              IF(GLANAM(ZZTOP,1).EQ.'MODEL        '.AND.
     1        GLANAM(ZZTOP-1,1).EQ.'MODEL        '.AND.
     1        GLANAM(ZZTOP-1,2).EQ.GLANAM(ZZTOP,2)) THEN
                  DUMMMY(ZZTOP)=.FALSE.
                  IF(ALENS(86,ZZTOP).EQ.ALENS(86,ZZTOP-1).AND.
     1               ALENS(87,ZZTOP).EQ.ALENS(87,ZZTOP-1).AND.
     1               ALENS(89,ZZTOP).EQ.ALENS(89,ZZTOP-1)) DUMMMY(ZZTOP)=.TRUE.
              END IF
              IF(ALENS(68,ZZTOP).EQ.1.0D0.AND.DUMMMY(ZZTOP))
     1        DUMMMY(ZZTOP)=.FALSE.
C
          END DO
C
C     RESOLVE CHARACTERISTICS OF PERFECT LENSES
          I=INT(SYSTEM1(20))-1
          IF(GLANAM(I,2).EQ.'PERFECT      ') THEN
              ALENS(1,I)=0.0D0
              ALENS(2,I)=0.0D0
              ALENS(4,I)=0.0D0
              ALENS(5,I)=0.0D0
              ALENS(6,I)=0.0D0
              ALENS(7,I)=0.0D0
              ALENS(8,I)=0.0D0
              ALENS(9,I)=0.0D0
              ALENS(127,I)=0.0D0
              ALENS(23,I)=0.0D0
              ALENS(24,I)=0.0D0
              ALENS(29,I)=0.0D0
              ALENS(30,I)=0.0D0
              ALENS(31,I)=0.0D0
              ALENS(32,I)=0.0D0
              ALENS(33,I)=0.0D0
              ALENS(34,I)=0.0D0
              ALENS(35,I)=0.0D0
              ALENS(36,I)=0.0D0
              ALENS(37,I)=0.0D0
              ALENS(38,I)=0.0D0
              ALENS(39,I)=0.0D0
              ALENS(40,I)=0.0D0
              ALENS(41,I)=0.0D0
              ALENS(43,I)=0.0D0
              ALENS(45,I)=0.0D0
              ALENS(51,I)=0.0D0
              ALENS(59,I)=0.0D0
              ALENS(61,I)=0.0D0
              ALENS(68,I)=1.0D0
              ALENS(69,I)=0.0D0
              ALENS(70,I)=0.0D0
              ALENS(77,I)=0.0D0
              ALENS(78,I)=0.0D0
              ALENS(79,I)=0.0D0
              ALENS(80,I)=0.0D0
              ALENS(80,I)=0.0D0
              ALENS(81,I)=0.0D0
              ALENS(82,I)=0.0D0
              ALENS(83,I)=0.0D0
              ALENS(84,I)=0.0D0
              ALENS(85,I)=0.0D0
              ALENS(86,I)=0.0D0
              ALENS(87,I)=0.0D0
              ALENS(88,I)=0.0D0
              ALENS(89,I)=0.0D0
              ALENS(90,I)=0.0D0
              ALENS(91,I)=0.0D0
              ALENS(92,I)=0.0D0
              ALENS(93,I)=0.0D0
              ALENS(94,I)=0.0D0
              ALENS(95,I)=0.0D0
              ALENS(96,I)=0.0D0
              ALENS(97,I)=0.0D0
              ALENS(98,I)=0.0D0
              ALENS(99,I)=0.0D0
              ALENS(100,I)=0.0D0
              ALENS(101,I)=0.0D0
              ALENS(102,I)=0.0D0
              ALENS(103,I)=0.0D0
              ALENS(104,I)=0.0D0
              ALENS(105,I)=0.0D0
              ALENS(106,I)=0.0D0
              ALENS(107,I)=0.0D0
              ALENS(108,I)=0.0D0
              ALENS(109,I)=0.0D0
              ALENS(110,I)=0.0D0
              ALENS(25,I)=0.0D0
              ALENS(26,I)=0.0D0
              ALENS(27,I)=0.0D0
              ALENS(28,I)=0.0D0
              ALENS(114,I)=0.0D0
              ALENS(115,I)=0.0D0
              ALENS(116,I)=0.0D0
              ALENS(118,I)=0.0D0
              ALENS(119,I)=0.0D0
              ALENS(120,I)=0.0D0
              I=INT(SYSTEM1(20))
              ALENS(1,I)=0.0D0
              ALENS(2,I)=0.0D0
              ALENS(4,I)=0.0D0
              ALENS(5,I)=0.0D0
              ALENS(6,I)=0.0D0
              ALENS(7,I)=0.0D0
              ALENS(8,I)=0.0D0
              ALENS(9,I)=0.0D0
              ALENS(127,I)=0.0D0
              ALENS(23,I)=0.0D0
              ALENS(24,I)=0.0D0
              ALENS(29,I)=0.0D0
              ALENS(30,I)=0.0D0
              ALENS(114,I)=0.0D0
              ALENS(115,I)=0.0D0
              ALENS(116,I)=0.0D0
              ALENS(31,I)=0.0D0
              ALENS(32,I)=0.0D0
              ALENS(33,I)=0.0D0
              ALENS(34,I)=0.0D0
              ALENS(35,I)=0.0D0
              ALENS(36,I)=0.0D0
              ALENS(37,I)=0.0D0
              ALENS(38,I)=0.0D0
              ALENS(39,I)=0.0D0
              ALENS(40,I)=0.0D0
              ALENS(41,I)=0.0D0
              ALENS(43,I)=0.0D0
              ALENS(45,I)=0.0D0
              ALENS(51,I)=0.0D0
              ALENS(59,I)=0.0D0
              ALENS(61,I)=0.0D0
              ALENS(68,I)=1.0D0
              ALENS(69,I)=0.0D0
              ALENS(70,I)=0.0D0
              ALENS(77,I)=0.0D0
              ALENS(78,I)=0.0D0
              ALENS(79,I)=0.0D0
              ALENS(80,I)=0.0D0
              ALENS(80,I)=0.0D0
              ALENS(81,I)=0.0D0
              ALENS(82,I)=0.0D0
              ALENS(83,I)=0.0D0
              ALENS(84,I)=0.0D0
              ALENS(85,I)=0.0D0
              ALENS(86,I)=0.0D0
              ALENS(87,I)=0.0D0
              ALENS(88,I)=0.0D0
              ALENS(89,I)=0.0D0
              ALENS(90,I)=0.0D0
              ALENS(91,I)=0.0D0
              ALENS(92,I)=0.0D0
              ALENS(93,I)=0.0D0
              ALENS(94,I)=0.0D0
              ALENS(95,I)=0.0D0
              ALENS(96,I)=0.0D0
              ALENS(97,I)=0.0D0
              ALENS(98,I)=0.0D0
              ALENS(99,I)=0.0D0
              ALENS(100,I)=0.0D0
              ALENS(101,I)=0.0D0
              ALENS(102,I)=0.0D0
              ALENS(103,I)=0.0D0
              ALENS(104,I)=0.0D0
              ALENS(105,I)=0.0D0
              ALENS(106,I)=0.0D0
              ALENS(107,I)=0.0D0
              ALENS(108,I)=0.0D0
              ALENS(109,I)=0.0D0
              ALENS(110,I)=0.0D0
              ALENS(25,I)=0.0D0
              ALENS(26,I)=0.0D0
              ALENS(27,I)=0.0D0
              ALENS(28,I)=0.0D0
              ALENS(118,I)=0.0D0
              ALENS(119,I)=0.0D0
              ALENS(120,I)=0.0D0
          END IF
          I=INT(SYSTEM1(20))-1
          IF(GLANAM(I,2).EQ.'PERFECT      ') THEN
              IF(SYSTEM1(30).GE.3.0D0) THEN
                  IF(SYSTEM1(30).EQ.3.0D0) SYSTEM1(30)=1.0D0
                  IF(SYSTEM1(30).EQ.4.0D0) SYSTEM1(30)=2.0D0
                  OUTLYNE='"PERFECT" SURFACE IN USE. LENS MODE HAS BEEN'
                  CALL SHOWIT(1)
                  IF(SYSTEM1(30).EQ.1.0D0) OUTLYNE='RESET TO THE "FOCAL" MODE'
                  IF(SYSTEM1(30).EQ.2.0D0) OUTLYNE='RESET TO THE "UFOCAL" MODE'
                  CALL SHOWIT(1)
              END IF
          END IF
C     RESOLVE CHARACTERISTICS OF IDEAL LENSES
          I=INT(SYSTEM1(20))-1
          IF(GLANAM(I,2).EQ.'IDEAL        ') THEN
              ALENS(1:2,I)=0.0D0
              ALENS(4:9,I)=0.0D0
              ALENS(127,I)=0.0D0
              ALENS(23:24,I)=0.0D0
              ALENS(29,I)=0.0D0
              ALENS(30:43,I)=0.0D0
              ALENS(45,I)=0.0D0
              ALENS(51,I)=0.0D0
              ALENS(59,I)=0.0D0
              ALENS(61,I)=0.0D0
              ALENS(68,I)=1.0D0
              ALENS(69,I)=0.0D0
              ALENS(70,I)=0.0D0
              ALENS(77:110,I)=0.0D0
              ALENS(25:28,I)=0.0D0
              ALENS(114:120,I)=0.0D0
              I=INT(SYSTEM1(20))
              ALENS(1:2,I)=0.0D0
              ALENS(4:9,I)=0.0D0
              ALENS(127,I)=0.0D0
              ALENS(23:24,I)=0.0D0
              ALENS(29:30,I)=0.0D0
              ALENS(114:116,I)=0.0D0
              ALENS(31:43,I)=0.0D0
              ALENS(45,I)=0.0D0
              ALENS(51,I)=0.0D0
              ALENS(59,I)=0.0D0
              ALENS(61,I)=0.0D0
              ALENS(68,I)=1.0D0
              ALENS(69,I)=0.0D0
              ALENS(70,I)=0.0D0
              ALENS(77:110,I)=0.0D0
              ALENS(25:28,I)=0.0D0
              ALENS(118:120,I)=0.0D0
          END IF
          I=INT(SYSTEM1(20))-1
          IF(GLANAM(I,2).EQ.'IDEAL        ') THEN
              IF(SYSTEM1(30).GE.3.0D0) THEN
                  IF(SYSTEM1(30).EQ.3.0D0) SYSTEM1(30)=1.0D0
                  IF(SYSTEM1(30).EQ.4.0D0) SYSTEM1(30)=2.0D0
                  OUTLYNE='"IDEAL" SURFACE IN USE. LENS MODE HAS BEEN'
                  CALL SHOWIT(1)
                  IF(SYSTEM1(30).EQ.1.0D0) OUTLYNE='RESET TO THE "FOCAL" MODE'
                  IF(SYSTEM1(30).EQ.2.0D0) OUTLYNE='RESET TO THE "UFOCAL" MODE'
                  CALL SHOWIT(1)
              END IF
          END IF
C
C
C     ADDED ON 3/17/93
C     SET UP THE FAST ACCESS CFG ARRAYS FOR OPTIMIZATION
C     IF THERE ARE CONFIGS

          IF(INT(SYSTEM1(56)).GT.1.AND.LNSTYP.NE.3) CALL AUXCFG
C
C     INITIALIZE THE FRONT FOCAL LENGTH, BACK FOCAL LENGTH
C     AND ENTRANCE TO EXIT PUPIL MAG VARIABLES BASED ON
C     PARAXIAL DATA
C
          RFFLX=0.0D0
          RFFLY=0.0D0
          RBFLX=0.0D0
          RBFLY=0.0D0
          RMAGX=0.0D0
          RMAGY=0.0D0
          MAGXOR=0.0D0
          MAGYOR=0.0D0
C
          IF(PXTRAX(6,NEWREF).NE.0.0D0) THEN
              MAGXOR=PXTRAX(6,NEWOBJ)/PXTRAX(6,NEWREF)
          ELSE
              IF(PXTRAX(6,NEWOBJ).GE.0.0D0) MAGXOR=1.0D10
              IF(PXTRAX(6,NEWOBJ).LT.0.0D0) MAGXOR=-1.0D10
          END IF
          IF(PXTRAY(6,NEWREF).NE.0.0D0) THEN
              MAGYOR=PXTRAY(6,NEWOBJ)/PXTRAY(6,NEWREF)
          ELSE
              IF(PXTRAY(6,NEWOBJ).GE.0.0D0) MAGYOR=1.0D10
              IF(PXTRAY(6,NEWOBJ).LT.0.0D0) MAGYOR=-1.0D10
          END IF
C
C     RFFLX,RFFLY,RBFLX,RBFLY
C
          I=NEWOBJ
          J=NEWIMG-1
          IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J)))
     1    .NE.0.0D0) THEN
              REFLY=-(((PXTRAY(2,I)*PXTRAY(5,I+1))-(PXTRAY(1,I+1)*PXTRAY(6,I
     1          )))/((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
          ELSE
              REFLY=1.0D20
          END IF
          IF(((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J)))
     1    .NE.0.0D0) THEN
              REFLX=-(((PXTRAX(2,I)*PXTRAX(5,I+1))-(PXTRAX(1,I+1)*PXTRAX(6,I
     1          )))/((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J))))
          ELSE
              REFLX=1.0D20
          END IF
          IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J)))
     1    .NE.0.0D0) THEN
              RBFLY=-(((PXTRAY(2,I)*PXTRAY(5,J))-(PXTRAY(6,I)*PXTRAY(1,J)))/
     1          ((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
          ELSE
              RBFLY=1.0D20
          END IF
          IF(((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J)))
     1    .NE.0.0D0) THEN
              RBFLX=-(((PXTRAX(2,I)*PXTRAX(5,J))-(PXTRAX(6,I)*PXTRAX(1,J)))/
     1          ((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J))))
          ELSE
              RBFLX=1.0D20
          END IF
          IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J)))
     1    .NE.0.0D0) THEN
              RFFLY=-(((PXTRAY(1,I+1)*PXTRAY(6,J))-(PXTRAY(2,J)*PXTRAY(5,I+1
     1          )))/((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
          ELSE
              RFFLY=1.0D20
          END IF
          IF(((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J)))
     1    .NE.0.0D0) THEN
              RFFLX=-(((PXTRAX(1,I+1)*PXTRAX(6,J))-(PXTRAX(2,J)*PXTRAX(5,I+1
     1          )))/((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J))))
          ELSE
              RFFLX=1.0D20
          END IF
C
C     F-NUMBERS
C
          IF(PXTRAX(2,NEWOBJ).NE.0.0D0) THEN
              RFFNX=-0.5D0/PXTRAX(2,NEWOBJ)
          ELSE
              IF(RFFLX.GE.0.0D0) RFFNX=1.0D10
              IF(RFFLX.LT.0.0D0) RFFNX=-1.0D10
          END IF
          IF(PXTRAY(2,NEWOBJ).NE.0.0D0) THEN
              RFFNY=-0.5D0/PXTRAY(2,NEWOBJ)
          ELSE
              IF(RFFLY.GE.0.0D0) RFFNY=1.0D10
              IF(RFFLY.LT.0.0D0) RFFNY=-1.0D10
          END IF
          IF(PXTRAX(2,NEWIMG).NE.0.0D0) THEN
              RBFNX=-0.5D0/PXTRAX(2,NEWIMG)
          ELSE
              IF(RBFLX.GE.0.0D0) RBFNX=1.0D10
              IF(RBFLX.LT.0.0D0) RBFNX=-1.0D10
          END IF
          IF(PXTRAY(2,NEWIMG).NE.0.0D0) THEN
              RBFNY=-0.5D0/PXTRAY(2,NEWIMG)
          ELSE
              IF(RBFLY.GE.0.0D0) RBFNY=1.0D10
              IF(RBFLY.LT.0.0D0) RBFNY=-1.0D10
          END IF
C     RMAGX AND RMAGY
          IF(PXTRAX(6,NEWOBJ).NE.0.0D0) THEN
              RMAGX=PXTRAX(6,NEWIMG)/PXTRAX(6,NEWOBJ)
          ELSE
              IF(PXTRAX(6,NEWOBJ).GE.0.0D0) RMAGX=1.0D10
              IF(PXTRAX(6,NEWOBJ).LT.0.0D0) RMAGX=-1.0D10
          END IF
          IF(PXTRAY(6,NEWOBJ).NE.0.0D0) THEN
              RMAGY=PXTRAY(6,NEWIMG)/PXTRAY(6,NEWOBJ)
          ELSE
              IF(PXTRAY(6,NEWOBJ).GE.0.0D0) RMAGY=1.0D10
              IF(PXTRAY(6,NEWOBJ).LT.0.0D0) RMAGY=-1.0D10
          END IF
C     PARAXIAL DEFAULT PUPIL DATA
C     NOW THE ENTRANCE AND EXIT PUPIL POSITIONS
          IF(DABS(PXTRAY(6,1)).EQ.0.0) THEN
              ENPOSY=-(ALENS(3,0))
          ELSE
              ENPOSY=(-PXTRAY(5,1)/PXTRAY(6,1))
          END IF
          IF(DABS(PXTRAX(6,1)).EQ.0.0D0) THEN
              ENPOSX=-(ALENS(3,0))
          ELSE
              ENPOSX=(-PXTRAX(5,1)/PXTRAX(6,1))
          END IF
          ENPUX=0.0D0
          ENPUY=0.0D0
          ENPUZ=(ENPOSX+ENPOSY)/2.0D0
C     NOW THE EXIT PUPIL POSITIONS
          IF(DABS(PXTRAY(6,INT(SYSTEM1(20)))).EQ.0.0D0) THEN
              EXPOSY=ALENS(3,0)
          ELSE
              EXPOSY=(-PXTRAY(5,INT(SYSTEM1(20)))
     1        /PXTRAY(6,INT(SYSTEM1(20))))
          END IF
          IF(DABS(PXTRAX(6,INT(SYSTEM1(20)))).EQ.0.0D0) THEN
              EXPOSX=ALENS(3,0)
          ELSE
              EXPOSX=(-PXTRAX(5,INT(SYSTEM1(20)))
     1        /PXTRAX(6,INT(SYSTEM1(20))))
          END IF
          EXPUX=0.0D0
          EXPUY=0.0D0
          EXPUZ=(EXPOSX+EXPOSY)/2.0D0
C     NOW THE ENTRANCE AND EXIT PUPIL DIAMETERS
          ENDIAX=2.0D0*DABS(PXTRAX(1,1)+(ENPUZ*PXTRAX(2,0)))
          ENDIAY=2.0D0*DABS(PXTRAY(1,1)+(ENPUZ*PXTRAY(2,0)))
          EXDIAX=2.0D0*DABS(PXTRAX(1,INT(SYSTEM1(20)))+(EXPUZ
     1    *PXTRAX(2,INT(SYSTEM1(20)))))
          EXDIAY=2.0D0*DABS(PXTRAY(1,INT(SYSTEM1(20)))+(EXPUZ
     1    *PXTRAY(2,INT(SYSTEM1(20)))))
C
C     NOW SET THE INR FOR PHASE SURFACE RAY TRACES
C     THE INR IS THE LARGER OF THE ABSOLUTE VALUE OF THE
C     X-CLAP PLUS X-CLAP OFFSET OR THE Y-CLAP PLUS Y-CLAP OFFSET
C     IF THERE IS A CLAP. IF NO CLAP, IT IS THE SUM OF THE ABSOLUTE
C     VALUES OF THE PY PLUS PCY AT THAT SURFACE
          DO I=0,INT(SYSTEM1(20))
              IF(ALENS(143,I).EQ.0.0D0) THEN
C     MAKE A DEFAULT ASSIGNMENT
                  IF(ALENS(9,I).NE.0.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
C     USE CLAP DATA
                      IF(ALENS(9,I).NE.5.0D0) THEN
                          A1=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                          A2=DABS(ALENS(11,I))+DABS(ALENS(13,I))
                      ELSE
                          A1=DABS(ALENS(10,I))+DABS(ALENS(12,I))
                          A2=DABS(ALENS(10,I))+DABS(ALENS(13,I))
                      END IF
                      IF(A1.GE.A2) THEN
                          ALENS(76,I)=A1
                      ELSE
                          ALENS(76,I)=A2
                      END IF
                  ELSE
C     USE PARAX DATA
                      A1=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                      A2=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                      IF(A1.GE.A2) THEN
                          ALENS(76,I)=A1
                      ELSE
                          ALENS(76,I)=A2
                      END IF
                  END IF
C     NO ASSIGNMENT IS NECESSARY
              END IF
          END DO
C     11/04/96
C     RESOLVE ALL TILT RETURNS AND APPLY ALL TILT RETURN RULES
C     BY CALLING TILT_RETURN
          SYSTEM1(90)=0.0D0
          DO I=0,INT(SYSTEM1(20))
C     IF THE TILT RET FLAG RES WAS ON, SET THE TILT TYPE TO TILT RET
C     THEN SET RESOLUTION FLAG TO OFF OR ZERO
              IF(ALENS(77,I).EQ.1.0D0) ALENS(25,I)=6.0D0
              ALENS(77,I)=0.0D0
          END DO
C     RESOLVE ALL PIVOTS WHICH BY THEIR NATURE DEFINE NEW DECENTERS
C     ON THE AFFECTED SURFACES
          DO I=0,INT(SYSTEM1(20))
              IF(ALENS(25,I).EQ.1.0D0.OR.ALENS(25,I).EQ.-1.0D0.OR.
     1        ALENS(25,I).EQ.5.0D0) THEN
                  IF(ALENS(59,I).EQ.1.0D0) CALL PIVDEC(I)
              END IF
          END DO
          ERCODE=.FALSE.
          TRYES=.FALSE.
C     THIS CALL FINDS ILLEGAL TILT RETS AND TILT AUTOS WHICH PRECEED
C     TILT RETS AND FIXES THEM
          CALL TILT_RETURN(ERCODE,TRYES)
          IF(ERCODE) THEN
C     THERE WERE ERRORS RESOLVING TILT RETURN, CALL MACFAL
C     AND SET ERCODE TO .FALSE.
              ERCODE=.FALSE.
C     STOP A MACRO IF ONE IS RUNNING, THEN PROCEED
              CALL MACFAL
          END IF
          IF(TRYES) THEN
C     RESOLVE TILT RETURNS INTO TILTS AND DECENTER VALUES
C     ON SURFACES
C
              CALL RETRES
C
          END IF
C     FORCE IMAGE SURFACE TO HAVE ZERO THICKNESS
C       CHECK FOR LAST SURFACE THICKNESS NOT ZERO
C     THE FINAL SURFACE ALWAYS HAS ZERO THICKNESS
          IF(ALENS(3,INT(SYSTEM1(20))).NE.0.0D0) THEN
              ALENS(3,INT(SYSTEM1(20)))=0.0D0
              OUTLYNE=
     1        'WARNING: FINAL SURFACE THICKNESS WAS RESET TO ZERO'
              CALL SHOWIT(1)
          END IF
          DO I=1,INT(SYSTEM1(20))
              ALENS(29,I)=0.0D0
              IF(ALENS(30,I).NE.0.0D0.OR.ALENS(31,I).NE.0.0D0.OR.
     1        ALENS(69,I).NE.0.0D0) ALENS(29,I)=1.0D0
          END DO
          IF(.NOT.REDOLNSEOS) THEN
              IF(SYSTEM1(94).NE.0.0D0.OR.SYSTEM1(98).NE.0.0D0) THEN
                  CALL REDOXOBJ(REDOLNSEOS)
              END IF
              IF(SYSTEM1(95).NE.0.0D0.OR.SYSTEM1(99).NE.0.0D0) THEN
                  CALL REDOYOBJ(REDOLNSEOS)
              END IF
          ELSE
C     DONE
              RETURN
          END IF
          IF(REDOLNSEOS) THEN
              F1=0
              F6=1
              F22=0
              GO TO 999
          END IF

          RETURN
      END
      SUBROUTINE REDOXOBJ(REDOLNSEOS)
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          REAL*8 B,JK_FACTOR
          LOGICAL REDOLNSEOS
C
          JK_FACTOR=1.0D0
          IF(SYSTEM1(94).EQ.1.0D0.OR.SYSTEM1(98).EQ.1.0D0) THEN
C     IMAGE ANGLES SCALE
              B=PXTRAX(6,INT(SYSTEM1(20)))
              IF(B.NE.0.0D0) THEN
                  IF(SYSTEM1(94).EQ.1.0D0)
     1            JK_FACTOR=DTAN(PII*SYSTEM1(92)/180.0D0)/B
                  IF(SYSTEM1(98).EQ.1.0D0)
     1            JK_FACTOR=DTAN(PII*SYSTEM1(96)/180.0D0)/B
              END IF
          ELSE
C     IMAGE HT SCALE
              B=PXTRAX(5,INT(SYSTEM1(20)))
              IF(B.NE.0.0D0) THEN
                  IF(SYSTEM1(94).EQ.-1.0D0)
     1            JK_FACTOR=SYSTEM1(92)/B
                  IF(SYSTEM1(98).EQ.-1.0D0)
     1            JK_FACTOR=SYSTEM1(96)/B
              END IF
          END IF
C     SET SCX
          SYSTEM1(16)=SYSTEM1(16)*JK_FACTOR
          SYSTEM1(17)=0.0D0
          SYSTEM1(19)=0.0D0
          SYSTEM1(52)=1.0D0
          SYSTEM1(54)=0.0D0
          SYSTEM1(61)=1.0D0
          REDOLNSEOS=.TRUE.
          RETURN
      END
      SUBROUTINE REDOYOBJ(REDOLNSEOS)
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          REAL*8 B,JK_FACTOR
          LOGICAL REDOLNSEOS
C
          JK_FACTOR=1.0D0
          IF(SYSTEM1(95).EQ.1.0D0.OR.SYSTEM1(99).EQ.1.0D0) THEN
C     IMAGE ANGLES SCALE
              B=PXTRAY(6,INT(SYSTEM1(20)))
              IF(B.NE.0.0D0) THEN
                  IF(SYSTEM1(95).EQ.1.0D0)
     1            JK_FACTOR=DTAN(PII*SYSTEM1(93)/180.0D0)/B
                  IF(SYSTEM1(99).EQ.1.0D0)
     1            JK_FACTOR=DTAN(PII*SYSTEM1(97)/180.0D0)/B
              END IF
          ELSE
C     IMAGE HT SCALE
              B=PXTRAY(5,INT(SYSTEM1(20)))
              IF(B.NE.0.0D0) THEN
                  IF(SYSTEM1(95).EQ.-1.0D0)
     1            JK_FACTOR=SYSTEM1(93)/B
                  IF(SYSTEM1(99).EQ.-1.0D0)
     1            JK_FACTOR=SYSTEM1(97)/B
              END IF
          END IF
C     SET SCY
          SYSTEM1(14)=SYSTEM1(14)*JK_FACTOR
          SYSTEM1(15)=0.0D0
          SYSTEM1(18)=0.0D0
          SYSTEM1(51)=1.0D0
          SYSTEM1(53)=0.0D0
          SYSTEM1(60)=1.0D0
          REDOLNSEOS=.TRUE.
          RETURN
      END
      SUBROUTINE RERROR
          IMPLICIT NONE
          INTEGER I
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          DO I=1,INT(SYSTEM1(20))
              IF(ALENS(144,I).NE.0.0D0) THEN
                  WRITE(OUTLYNE,10) ALENS(144,I),I
                  CALL SHOWIT(0)
 10               FORMAT('RAYERROR = ',G23.15,' FOR SURFACE # ',I3)
              END IF
          END DO
      END
