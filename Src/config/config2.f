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

C       SECOND FILE OF CONFIGS FILES

C SUB CFSC2.FOR
      SUBROUTINE CFSC2
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE CFSC2 DOES SCALING OF CONFIG
C       DATA WHEN SAY,SAX,SCY,SCX,SCY FANG AND SCX FANG
C       ARE NOT SCALED WITH SURFACE DATA.
C
          CHARACTER HOLD*140,AVAL1*23,AN1*23,AV1*23,
     2    AVAL2*23,AVAL3*23,AVAL4*23,AVAL5*23,SNAME*9,
     3    LNAME*18
C
          INTEGER
     6    ULINE,ELINE,IEND,CHG1,ISTART,ISTOP,STARL,STOPL,ISTA,
     7    ISTO,I,J
C
          REAL*8 VAL1,VAL2,N1,V1,MM1,
     2    VAL3,VAL4,VAL5
C
          COMMON/CAUX1/N1,AN1
C
          COMMON/JK_NTA3/V1,AV1
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       STRAIGHT SCALING (SC,FACT,I,J)
C       W1=FACTOR
C       W2=STARTING SURFACE
C       W3=ENDING SURFACE
C
C       I TRACKS THE CONFIGURATION BEING HANDLED
C
C       J TRACKS THE ENTRY NUMBER
C
          IEND=INT(SYSTEM1(56))
          DO 10 I=2,IEND
C       LOOP THROUGH ALL NON BLANK CONFIGS
              ELINE=1
              ULINE=0
C
C       REMOVE ALL FNBY/ER/MAG ENTRIES BY CALLIN FNBDE
              CALL FNBDE(I)
C
C       DETERMINE LOCATION OF U L,EOS AND FIRST CHG
              DO 15 J=1,CFGCNT(I)
                  EE12=CONFG(I,J)
                  HOLDER=EE12
                  IF((HOLDER(1:10)).EQ.'U        L'.OR.
     1            (HOLDER(1:13)).EQ.'UPDATE   LENS'.OR.
     1            (HOLDER(1:10)).EQ.'UPDATE   L'.OR.
     1            (HOLDER(1:13)).EQ.'U        LENS') ULINE=J
                  IF((HOLDER(1:3)).EQ.'EOS'.AND.J.GT.ULINE)THEN
                      ELINE=J
                      GO TO 16
                  ELSE
                  END IF
 15           CONTINUE
 16           CONTINUE
              CHG1=ELINE
              DO 20 J=(ULINE+1),(ELINE-1)
                  EE12=CONFG(I,J)
                  HOLDER=EE12
                  IF((HOLDER(1:3)).EQ.'CHG') THEN
                      CHG1=J
                      GO TO 21
                  ELSE
                  END IF
 20           CONTINUE
 21           CONTINUE
C
C       NOW SURFACE DATA. WE SCALE ALL SURFACE DATA BETWEEN
C       SURFACE W2 AND SURFACE W3 AS INPUT IN THE SCALING COMMAND
C
              ISTART=INT(W2)
              ISTOP= INT(W3)
              ISTA=0
              ISTO=0
C
C       THE LINES BETWEEN WHICH SCALING WILL OCCUR ARE
C       NOW DETERMINED.
C       SEARCH FOR THE STARTING LINE.(STARL)
              DO 100 J=CHG1,(ELINE-1)
                  EE12=CONFG(I,J)
                  HOLDER=EE12
                  IF((HOLDER(1:3)).EQ.'CHG') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      IF(INT(VAL1).LT.ISTART) GO TO 100
                      IF(INT(VAL1).GE.ISTART) THEN
                          IF(ISTA.EQ.0) THEN
                              ISTA=1
                              STARL=J+1
C       STARTING LINE STARL ASSIGNED
                              GO TO 101
                          ELSE
C       ISTA NOT 0, STARL ALREADY ASSIGNED
                          END IF
                      ELSE
                      END IF
                  ELSE
C       NOT CHG, PROCEED
                  END IF
C
 100          CONTINUE
C       IF GOT HERE, DID NOT FIND STARTING LINE AND THEREFORE
C       NO ENDING LINE EXISTS SO NOTHING TO SCALE. GO TO 10
C       AND CHECK NEXT CONFIG
              GO TO 10
 101          CONTINUE
C       SEARCH FOR THE ENDING LINE.(ENDL)
              DO 200 J=STARL,(ELINE-1)
                  EE12=CONFG(I,J)
                  HOLDER=EE12
                  IF((HOLDER(1:3)).EQ.'CHG') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      IF(INT(VAL1).LE.ISTOP) GO TO 200
                      IF(INT(VAL1).GT.ISTOP) THEN
                          IF(ISTO.EQ.0) THEN
                              ISTO=1
                              STOPL=J-1
C       HALTING LINE STOPL ASSIGNED
                              GO TO 201
                          ELSE
C       ISTO NOT 0
                          END IF
                      ELSE
                      END IF
                  ELSE
C       NOT CHG, PROCEED
                  END IF
C
 200          CONTINUE
C       IF GOT HERE, DID NOT FIND HALTING LINE AND THEREFORE
C       HALTING LINE IS ELINE-1
C                       ISTO=1
              STOPL=(ELINE-1)
 201          CONTINUE
C
C       PROCEED TO SCALE NOW
              DO 300 J=STARL,STOPL
                  EE12=CONFG(I,J)
                  HOLDER=EE12
C
C       SCALE ALL APPROPRIATE SURFACE ENTRYS AND REMEMBER TO SKIP
C       INTERMEDIATE CHG STATMENTS.
C               SKIP CHG STATMENTS
                  IF((HOLDER(1:3)).EQ.'CHG') GO TO 300
C
C       SCALE (CV)
                  IF((HOLDER(1:2)).EQ.'CV') THEN
                      AVAL1=(HOLDER(10:32))
                      HOLD(1:140)=HOLDER(33:140)
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='CV      ,'//AVAL1//HOLD(1:100)
                      GO TO 299
                  ELSE
C       NOT CV
                  END IF
C       SCALE (RD)
                  IF((HOLDER(1:2)).EQ.'RD') THEN
                      AVAL1=(HOLDER(10:32))
                      HOLD(1:140)=HOLDER(33:140)
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='RD      ,'//AVAL1//HOLD(1:100)
                      GO TO 299
                  ELSE
C       NOT RD
                  END IF
C       SCALE (TH)
                  IF((HOLDER(1:2)).EQ.'TH') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='TH      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C       SCALE (THM)
                  IF((HOLDER(1:3)).EQ.'THM') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='THM     ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (AC)
                  IF((HOLDER(1:2)).EQ.'AC') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AC      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (AD)
                  IF((HOLDER(1:2)).EQ.'AD') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**3)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AD      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C       SCALE (ADTOR)
                  IF((HOLDER(1:5)).EQ.'ADTOR') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**3)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='ADTOR   ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (AE)
                  IF((HOLDER(1:2)).EQ.'AE') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**5)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AE      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C       SCALE (AETOR)
                  IF((HOLDER(1:5)).EQ.'AETOR') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**5)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AETOR   ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (AF)
                  IF((HOLDER(1:2)).EQ.'AF') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**7)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AF      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C       SCALE (AFTOR)
                  IF((HOLDER(1:5)).EQ.'AFTOR') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**7)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AFTOR   ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (AG)
                  IF((HOLDER(1:2)).EQ.'AG') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**9)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AG      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C       SCALE (AGTOR)
                  IF((HOLDER(1:5)).EQ.'AGTOR') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**9)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AGTOR   ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (AH)
                  IF((HOLDER(1:2)).EQ.'AH') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**11)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AH      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (AI)
                  IF((HOLDER(1:2)).EQ.'AI') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**13)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AI      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (AJ)
                  IF((HOLDER(1:2)).EQ.'AJ') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**15)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AJ      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (AK)
                  IF((HOLDER(1:2)).EQ.'AK') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**17)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AK      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (AL)
                  IF((HOLDER(1:2)).EQ.'AL') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/(W1**19)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='AL      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C       SCALE (ASPH)
                  IF((HOLDER(1:4)).EQ.'ASPH') THEN
                      HOLDER=HOLDER(10:140)
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL1=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL1=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL2=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL2=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL3=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL3=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL4=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL4=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL5=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL5=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF(AVAL1.NE.',') THEN
                          AV1=AVAL1
                          CALL ATON3
                          VAL1=V1
                          VAL1=VAL1/(W1**3)
                          V1=VAL1
                          CALL NTOA3
                          AVAL1=AV1
                      ELSE
                      END IF
C
                      IF(AVAL2.NE.',') THEN
                          AV1=AVAL2
                          CALL ATON3
                          VAL2=V1
                          VAL2=VAL2/(W1**5)
                          V1=VAL2
                          CALL NTOA3
                          AVAL2=AV1
                      ELSE
                      END IF
C
                      IF(AVAL3.NE.',') THEN
                          AV1=AVAL3
                          CALL ATON3
                          VAL3=V1
                          VAL3=VAL3/(W1**7)
                          V1=VAL3
                          CALL NTOA3
                          AVAL3=AV1
                      ELSE
                      END IF
C
                      IF(AVAL4.NE.',') THEN
                          AV1=AVAL4
                          CALL ATON3
                          VAL4=V1
                          VAL4=VAL4/(W1**9)
                          V1=VAL4
                          CALL NTOA3
                          AVAL4=AV1
                      ELSE
                      END IF
C
                      IF(AVAL5.NE.',') THEN
                          AV1=AVAL5
                          CALL ATON3
                          VAL5=V1
                          VAL5=VAL5/(W1)
                          V1=VAL5
                          CALL NTOA3
                          AVAL5=AV1
                      ELSE
                      END IF
C       RE-CONSTRUCT HOLDER
                      HOLDER='ASPH    ,'
                      IF(AVAL1.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL1//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL2.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL2//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL3.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL3//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL4.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL4//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL5.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL5//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (ASPH2)
                  IF((HOLDER(1:4)).EQ.'ASPH2') THEN
                      HOLDER=HOLDER(10:140)
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL1=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL1=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL2=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL2=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL3=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL3=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL4=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL4=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL5=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL5=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF(AVAL1.NE.',') THEN
                          AV1=AVAL1
                          CALL ATON3
                          VAL1=V1
                          VAL1=VAL1/(W1**11)
                          V1=VAL1
                          CALL NTOA3
                          AVAL1=AV1
                      ELSE
                      END IF
C
                      IF(AVAL2.NE.',') THEN
                          AV1=AVAL2
                          CALL ATON3
                          VAL2=V1
                          VAL2=VAL2/(W1**13)
                          V1=VAL2
                          CALL NTOA3
                          AVAL2=AV1
                      ELSE
                      END IF
C
                      IF(AVAL3.NE.',') THEN
                          AV1=AVAL3
                          CALL ATON3
                          VAL3=V1
                          VAL3=VAL3/(W1**15)
                          V1=VAL3
                          CALL NTOA3
                          AVAL3=AV1
                      ELSE
                      END IF
C
                      IF(AVAL4.NE.',') THEN
                          AV1=AVAL4
                          CALL ATON3
                          VAL4=V1
                          VAL4=VAL4/(W1**17)
                          V1=VAL4
                          CALL NTOA3
                          AVAL4=AV1
                      ELSE
                      END IF
C
                      IF(AVAL5.NE.',') THEN
                          AV1=AVAL5
                          CALL ATON3
                          VAL5=V1
                          VAL5=VAL5/(W1**19)
                          V1=VAL5
                          CALL NTOA3
                          AVAL5=AV1
                      ELSE
                      END IF
C       RE-CONSTRUCT HOLDER
                      HOLDER='ASPH2   ,'
                      IF(AVAL1.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL1//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL2.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL2//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL3.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL3//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL4.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL4//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL5.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL5//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      GO TO 299
                  ELSE
                  END IF
C       SCALE (TASPH)
                  IF((HOLDER(1:5)).EQ.'TASPH') THEN
                      HOLDER=HOLDER(10:140)
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL1=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL1=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL2=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL2=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL3=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL3=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL4=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL4=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL5=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL5=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF(AVAL1.NE.',') THEN
                          AV1=AVAL1
                          CALL ATON3
                          VAL1=V1
                          VAL1=VAL1/(W1**3)
                          V1=VAL1
                          CALL NTOA3
                          AVAL1=AV1
                      ELSE
                      END IF
C
                      IF(AVAL2.NE.',') THEN
                          AV1=AVAL2
                          CALL ATON3
                          VAL2=V1
                          VAL2=VAL2/(W1**5)
                          V1=VAL2
                          CALL NTOA3
                          AVAL2=AV1
                      ELSE
                      END IF
C
                      IF(AVAL3.NE.',') THEN
                          AV1=AVAL3
                          CALL ATON3
                          VAL3=V1
                          VAL3=VAL3/(W1**7)
                          V1=VAL3
                          CALL NTOA3
                          AVAL3=AV1
                      ELSE
                      END IF
C
                      IF(AVAL4.NE.',') THEN
                          AV1=AVAL4
                          CALL ATON3
                          VAL4=V1
                          VAL4=VAL4/(W1**9)
                          V1=VAL4
                          CALL NTOA3
                          AVAL4=AV1
                      ELSE
                      END IF
C
C       RE-CONSTRUCT HOLDER
                      HOLDER='TASPH   ,'
                      IF(AVAL1.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL1//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL2.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL2//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL3.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL3//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL4.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL4//',,'
                      ELSE
                          HOLDER=trim(HOLDER)//',,'
                      END IF
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (CLAP OR COBS)
                  MM1=DABS(W1)
C
                  IF((HOLDER(1:4)).EQ.'CLAP'.OR.
     1            (HOLDER(1:4)).EQ.'COBS') THEN
C
                      IF((HOLDER(9:9)).EQ.',') THEN
                          SNAME=HOLDER(1:9)
                          LNAME='                  '
                          HOLDER=HOLDER(10:140)
                      ELSE
                          SNAME='         '
                          LNAME=HOLDER(1:18)
                          HOLDER=HOLDER(19:140)
                      END IF
C       RESOLVE NUMERIC VALUES
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL1=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL1=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL2=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL2=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL3=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL3=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL4=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL4=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL5=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL5=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
C       NOW RESOLVE SCALING (DEPENDING ON QUALIFIER VALUE)
C
C       NO QUALIFIER JUST CLAP OR COBS
C
                      IF(LNAME(1:6).EQ.'      ') THEN
C       JUST CLAP OR COBS, SCALE ALL VALUES
                          IF(AVAL1.NE.',') THEN
                              AV1=AVAL1
                              CALL ATON3
                              VAL1=V1
                              VAL1=VAL1*MM1
                              V1=VAL1
                              CALL NTOA3
                              AVAL1=AV1
                          ELSE
                          END IF
                          IF(AVAL2.NE.',') THEN
                              AV1=AVAL2
                              CALL ATON3
                              VAL2=V1
                              VAL2=VAL2*MM1
                              V1=VAL2
                              CALL NTOA3
                              AVAL2=AV1
                          ELSE
                          END IF
                          IF(AVAL3.NE.',') THEN
                              AV1=AVAL3
                              CALL ATON3
                              VAL3=V1
                              VAL3=VAL3*MM1
                              V1=VAL3
                              CALL NTOA3
                              AVAL3=AV1
                          ELSE
                          END IF
                          AVAL4=','
                          AVAL5=','
C
C       RE-CONSTRUCT HOLDER
C
                          HOLDER=SNAME
                          IF(AVAL1.NE.',') THEN
                              HOLDER=trim(HOLDER)//AVAL1//','
                          ELSE
                              HOLDER=trim(HOLDER)//','
                          END IF
                          IF(AVAL2.NE.',') THEN
                              HOLDER=trim(HOLDER)//AVAL2//','
                          ELSE
                              HOLDER=trim(HOLDER)//','
                          END IF
                          IF(AVAL3.NE.',') THEN
                              HOLDER=trim(HOLDER)//AVAL3//','
                          ELSE
                              HOLDER=trim(HOLDER)//','
                          END IF
                          IF(AVAL4.NE.',') THEN
                              HOLDER=trim(HOLDER)//AVAL4//','
                          ELSE
                              HOLDER=trim(HOLDER)//','
                          END IF
                          IF(AVAL5.NE.',') THEN
                              HOLDER=trim(HOLDER)//AVAL5//','
                          ELSE
                              HOLDER=trim(HOLDER)//','
                          END IF
                      ELSE
C       LNAME WAS NOT BLANK, THERE MUST HAVE BEEN A QUALIFIER
C       HANDEL IT
C       BREAK OUT THE QUALIFIER PART OR LNAME
C       WHICH IS LNAME(10:17) AND DECIDE WHAT TO DO IN TERMS
C       OF SCALING. THEN RE-CONSTRUCT HOLDER
C
C       LNAME(10:17)= 'ERASE   '
                          IF(LNAME(10:17).EQ.'ERASE   ') THEN
C       SAME SCALING AS CLAP OR COBS.
                              IF(AVAL1.NE.',') THEN
                                  AV1=AVAL1
                                  CALL ATON3
                                  VAL1=V1
                                  VAL1=VAL1*MM1
                                  V1=VAL1
                                  CALL NTOA3
                                  AVAL1=AV1
                              ELSE
                              END IF
                              IF(AVAL2.NE.',') THEN
                                  AV1=AVAL2
                                  CALL ATON3
                                  VAL2=V1
                                  VAL2=VAL2*MM1
                                  V1=VAL2
                                  CALL NTOA3
                                  AVAL2=AV1
                              ELSE
                              END IF
                              IF(AVAL3.NE.',') THEN
                                  AV1=AVAL3
                                  CALL ATON3
                                  VAL3=V1
                                  VAL3=VAL3*MM1
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
                              ELSE
                              END IF
                              AVAL4=','
                              AVAL5=','
                          ELSE
C       NOT 'ERASE'
                          END IF
C
C       LNAME(10:17)= 'RECT    ' OR 'RECTE'
                          IF(LNAME(10:17).EQ.'RECT    '.OR.LNAME(10:17)
     1                    .EQ.'RECTE   ') THEN
                              IF(AVAL1.NE.',') THEN
                                  AV1=AVAL1
                                  CALL ATON3
                                  VAL1=V1
                                  VAL1=VAL1*MM1
                                  V1=VAL1
                                  CALL NTOA3
                                  AVAL1=AV1
                              ELSE
                              END IF
                              IF(AVAL2.NE.',') THEN
                                  AV1=AVAL2
                                  CALL ATON3
                                  VAL2=V1
                                  VAL2=VAL2*MM1
                                  V1=VAL2
                                  CALL NTOA3
                                  AVAL2=AV1
                              ELSE
                              END IF
                              IF(AVAL3.NE.',') THEN
                                  AV1=AVAL3
                                  CALL ATON3
                                  VAL3=V1
                                  VAL3=VAL3*MM1
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
                              ELSE
                              END IF
                              IF(AVAL4.NE.',') THEN
                                  AV1=AVAL4
                                  CALL ATON3
                                  VAL4=V1
                                  VAL4=VAL4*MM1
                                  V1=VAL4
                                  CALL NTOA3
                                  AVAL4=AV1
                              ELSE
                              END IF
                              AVAL5=','
                          ELSE
C       NOT 'RECT'
                          END IF
C
C       LNAME(10:17)= 'ELIP    ' OR 'ELIPE'
                          IF(LNAME(10:17).EQ.'ELIP    '.OR.LNAME(10:17)
     1                    .EQ.'ELIPE   ') THEN
                              IF(AVAL1.NE.',') THEN
                                  AV1=AVAL1
                                  CALL ATON3
                                  VAL1=V1
                                  VAL1=VAL1*MM1
                                  V1=VAL1
                                  CALL NTOA3
                                  AVAL1=AV1
                              ELSE
                              END IF
                              IF(AVAL2.NE.',') THEN
                                  AV1=AVAL2
                                  CALL ATON3
                                  VAL2=V1
                                  VAL2=VAL2*MM1
                                  V1=VAL2
                                  CALL NTOA3
                                  AVAL2=AV1
                              ELSE
                              END IF
                              IF(AVAL3.NE.',') THEN
                                  AV1=AVAL3
                                  CALL ATON3
                                  VAL3=V1
                                  VAL3=VAL3*MM1
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
                              ELSE
                              END IF
                              IF(AVAL4.NE.',') THEN
                                  AV1=AVAL4
                                  CALL ATON3
                                  VAL4=V1
                                  VAL4=VAL4*MM1
                                  V1=VAL4
                                  CALL NTOA3
                                  AVAL4=AV1
                              ELSE
                              END IF
                              AVAL5=','
                          ELSE
C       NOT 'ELIP'
                          END IF
C
C       LNAME(10:17)=RCTK OR RCTKE
                          IF(LNAME(10:17).EQ.'RCTK    '.OR.
     1                    LNAME(10:17).EQ.'RCTKE   ') THEN
                              IF(AVAL1.NE.',') THEN
                                  AV1=AVAL1
                                  CALL ATON3
                                  VAL1=V1
                                  VAL1=VAL1*MM1
                                  V1=VAL1
                                  CALL NTOA3
                                  AVAL1=AV1
                                  V1=VAL1
                                  CALL NTOA3
                                  AVAL1=AV1
                              ELSE
                              END IF
                              IF(AVAL2.NE.',') THEN
                                  AV1=AVAL2
                                  CALL ATON3
                                  VAL2=V1
                                  VAL2=VAL2*MM1
                                  V1=VAL2
                                  CALL NTOA3
                                  AVAL2=AV1
                              ELSE
                              END IF
                              IF(AVAL3.NE.',') THEN
                                  AV1=AVAL3
                                  CALL ATON3
                                  VAL3=V1
                                  VAL3=VAL3*MM1
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
                              ELSE
                              END IF
                              IF(AVAL4.NE.',') THEN
                                  AV1=AVAL4
                                  CALL ATON3
                                  VAL4=V1
                                  VAL4=VAL4*MM1
                                  V1=VAL4
                                  CALL NTOA3
                                  AVAL4=AV1
                              ELSE
                              END IF
                              IF(AVAL5.NE.',') THEN
                                  AV1=AVAL5
                                  CALL ATON3
                                  VAL5=V1
                                  VAL5=VAL5*MM1
                                  V1=VAL5
                                  CALL NTOA3
                                  AVAL5=AV1
                              ELSE
                              END IF
                          ELSE
C       NOT RCTK OR RCTKE
                          END IF
C
C       LNAME(10:17)=POLY OR POLYE
                          IF(LNAME(10:17).EQ.'POLY    '.OR.
     1                    LNAME(10:17).EQ.'POLYE   ') THEN
                              IF(AVAL1.NE.',') THEN
                                  AV1=AVAL1
                                  CALL ATON3
                                  VAL1=V1
                                  VAL1=VAL1*MM1
                                  V1=VAL1
                                  CALL NTOA3
                                  AVAL1=AV1
                                  V1=VAL1
                                  CALL NTOA3
                                  AVAL1=AV1
                              ELSE
                              END IF
                              IF(AVAL2.NE.',') THEN
                                  AV1=AVAL2
                                  CALL ATON3
                                  VAL2=V1
                                  VAL2=VAL2
                                  V1=VAL2
                                  CALL NTOA3
                                  AVAL2=AV1
                              ELSE
                              END IF
                              IF(AVAL3.NE.',') THEN
                                  AV1=AVAL3
                                  CALL ATON3
                                  VAL3=V1
                                  VAL3=VAL3*MM1
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
                              ELSE
                              END IF
                              IF(AVAL4.NE.',') THEN
                                  AV1=AVAL4
                                  CALL ATON3
                                  VAL4=V1
                                  VAL4=VAL4*MM1
                                  V1=VAL4
                                  CALL NTOA3
                                  AVAL4=AV1
                              ELSE
                              END IF
                              IF(AVAL5.NE.',') THEN
                                  AV1=AVAL5
                                  CALL ATON3
                                  VAL5=V1
                                  VAL5=VAL5
                                  V1=VAL5
                                  CALL NTOA3
                                  AVAL5=AV1
                              ELSE
                              END IF
                          ELSE
C       NOT POLY OR POLYE
                          END IF
C
C       LNAME(10:17)=IPOLY OR IPOLYE
                          IF(LNAME(10:17).EQ.'IPOLY   '.OR.
     1                    LNAME(10:17).EQ.'IPOLYE  ') THEN
                              IF(AVAL1.NE.',') THEN
                                  AV1=AVAL1
                                  CALL ATON3
                                  VAL1=V1
                                  VAL1=VAL1
                                  V1=VAL1
                                  CALL NTOA3
                                  AVAL1=AV1
                                  V1=VAL1
                                  CALL NTOA3
                                  AVAL1=AV1
                              ELSE
                              END IF
                              IF(AVAL2.NE.',') THEN
                                  AV1=AVAL2
                                  CALL ATON3
                                  VAL2=V1
                                  VAL2=VAL2
                                  V1=VAL2
                                  CALL NTOA3
                                  AVAL2=AV1
                              ELSE
                              END IF
                              IF(AVAL3.NE.',') THEN
                                  AV1=AVAL3
                                  CALL ATON3
                                  VAL3=V1
                                  VAL3=VAL3*MM1
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
                              ELSE
                              END IF
                              IF(AVAL4.NE.',') THEN
                                  AV1=AVAL4
                                  CALL ATON3
                                  VAL4=V1
                                  VAL4=VAL4*MM1
                                  V1=VAL4
                                  CALL NTOA3
                                  AVAL4=AV1
                              ELSE
                              END IF
                              IF(AVAL5.NE.',') THEN
                                  AV1=AVAL5
                                  CALL ATON3
                                  VAL5=V1
                                  VAL5=VAL5*MM1
                                  V1=VAL5
                                  CALL NTOA3
                                  AVAL5=AV1
                              ELSE
                              END IF
                          ELSE
C       NOT IPOLY OR IPOLYE
                          END IF
C
C       RE-CONSTRUCT HOLDER
C
                          HOLDER=LNAME
                          IF(AVAL1.NE.',') THEN
                              HOLDER=trim(HOLDER)//AVAL1//','
                          ELSE
                              HOLDER=trim(HOLDER)//','
                          END IF
                          IF(AVAL2.NE.',') THEN
                              HOLDER=trim(HOLDER)//AVAL2//','
                          ELSE
                              HOLDER=trim(HOLDER)//','
                          END IF
                          IF(AVAL3.NE.',') THEN
                              HOLDER=trim(HOLDER)//AVAL3//','
                          ELSE
                              HOLDER=trim(HOLDER)//','
                          END IF
                          IF(AVAL4.NE.',') THEN
                              HOLDER=trim(HOLDER)//AVAL4//','
                          ELSE
                              HOLDER=trim(HOLDER)//','
                          END IF
                          IF(AVAL5.NE.',') THEN
                              HOLDER=trim(HOLDER)//AVAL5//','
                          ELSE
                              HOLDER=trim(HOLDER)//','
                          END IF
                      END IF
                      GO TO 299
                  ELSE
C       NOT CLAP OR COBS, PROCEED
                  END IF
C
C       NOW TORIC CURVATUURE AND RADIUS
C       SCALE (CVTOR)
                  IF((HOLDER(1:5)).EQ.'CVTOR') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1/W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='CVTOR   ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C       SCALE (RDTOR)
                  IF((HOLDER(1:5)).EQ.'RDTOR') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='RDTOR   ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C       SCALE (DEC)
                  IF((HOLDER(1:3)).EQ.'DEC') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      AVAL2=(HOLDER(34:56))
                      AV1=AVAL2
                      CALL ATON3
                      VAL2=V1
                      VAL2=VAL2*W1
                      V1=VAL2
                      CALL NTOA3
                      AVAL2=AV1
                      AVAL3=(HOLDER(58:80))
                      AV1=AVAL3
                      CALL ATON3
                      VAL3=V1
                      VAL3=VAL3*W1
                      V1=VAL3
                      CALL NTOA3
                      AVAL3=AV1
                      HOLDER='DEC     ,'//AVAL1//','//AVAL2//','//AVAL3//',,,'
                      GO TO 299
                  ELSE
                  END IF
C       SCALE (PIVOT)
                  IF((HOLDER(1:5)).EQ.'PIVOT') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      AVAL2=(HOLDER(34:56))
                      AV1=AVAL2
                      CALL ATON3
                      VAL2=V1
                      VAL2=VAL2*W1
                      V1=VAL2
                      CALL NTOA3
                      AVAL2=AV1
                      AVAL3=(HOLDER(58:80))
                      AV1=AVAL3
                      CALL ATON3
                      VAL3=V1
                      VAL3=VAL3*W1
                      V1=VAL3
                      CALL NTOA3
                      AVAL3=AV1
                      HOLDER='PIVOT   ,'//AVAL1//','//AVAL2//','//AVAL3//',,,'
                      GO TO 299
                  ELSE
                  END IF
C       SCALE (PY)
                  IF((HOLDER(1:2)).EQ.'PY') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='PY      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (PX)
                  IF((HOLDER(1:2)).EQ.'PX') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='PX      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (PCY)
                  IF((HOLDER(1:3)).EQ.'PCY') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='PCY     ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (PCX)
                  IF((HOLDER(1:3)).EQ.'PCX') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='PCX     ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (CAY)
                  IF((HOLDER(1:3)).EQ.'CAY') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='CAY     ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (CAX)
                  IF((HOLDER(1:3)).EQ.'CAX') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='CAX     ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (XD)
                  IF((HOLDER(1:2)).EQ.'XD') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*DABS(W1)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='XD      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (YD)
                  IF((HOLDER(1:2)).EQ.'YD') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*DABS(W1)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='YD      ,'//AVAL1//',,,,,'
                  ELSE
                  END IF
C
C       SCALE (ZD)
                  IF((HOLDER(1:2)).EQ.'ZD') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='ZD      ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (GDX)
                  IF((HOLDER(1:3)).EQ.'GDX') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*DABS(W1)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='GDX     ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (GDY)
                  IF((HOLDER(1:3)).EQ.'GDY') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*DABS(W1)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='GDY     ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (GDZ)
                  IF((HOLDER(1:3)).EQ.'GDZ') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='GDZ     ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (ALPHA,BETA,GAMMA,GALPHA,GBETA,GGAMMA)
                  IF((HOLDER(1:5)).EQ.'ALPHA'.OR.
     1            (HOLDER(1:4)).EQ.'BETA'.OR.
     1            (HOLDER(1:5)).EQ.'GAMMA'.OR.
     1            (HOLDER(1:6)).EQ.'GALPHA'.OR.
     1            (HOLDER(1:5)).EQ.'GBETA'.OR.
     1            (HOLDER(1:6)).EQ.'GGAMMA') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=-VAL1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      IF(HOLDER(1:5).EQ.'ALPHA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
                      IF(HOLDER(1:4).EQ.'BETA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
                      IF(HOLDER(1:5).EQ.'GAMMA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
                      IF(HOLDER(1:6).EQ.'GALPHA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
                      IF(HOLDER(1:5).EQ.'GBETA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
                      IF(HOLDER(1:6).EQ.'GGAMMA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (PIVX)
                  IF((HOLDER(1:4)).EQ.'PIVX') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*DABS(W1)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='PIVX    ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (PIVY)
                  IF((HOLDER(1:4)).EQ.'PIVY') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*DABS(W1)
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='PIVY    ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       SCALE (PIVZ)
                  IF((HOLDER(1:4)).EQ.'PIVZ') THEN
                      AVAL1=(HOLDER(10:32))
                      AV1=AVAL1
                      CALL ATON3
                      VAL1=V1
                      VAL1=VAL1*W1
                      V1=VAL1
                      CALL NTOA3
                      AVAL1=AV1
                      HOLDER='PIVZ    ,'//AVAL1//',,,,,'
                      GO TO 299
                  ELSE
                  END IF
C
C       NOW WE MUST SCALE ALL PIKUPS CORRECTLY
C
C       SCALE (PIKUP)S AS APPROPRIATE
C
                  IF((HOLDER(1:5)).EQ.'PIKUP')THEN
                      IF((HOLDER(10:17)).EQ.'RD      '.OR.
     1                (HOLDER(10:17)).EQ.'CV      '.OR.
     2                (HOLDER(10:17)).EQ.'TH      '.OR.
     4                (HOLDER(10:17)).EQ.'AC      '.OR.
     4                (HOLDER(10:17)).EQ.'AD      '.OR.
     5                (HOLDER(10:17)).EQ.'AE      '.OR.
     6                (HOLDER(10:17)).EQ.'AF      '.OR.
     7                (HOLDER(10:17)).EQ.'AG      '.OR.
     4                (HOLDER(10:17)).EQ.'AH      '.OR.
     4                (HOLDER(10:17)).EQ.'AI      '.OR.
     5                (HOLDER(10:17)).EQ.'AJ      '.OR.
     6                (HOLDER(10:17)).EQ.'AK      '.OR.
     7                (HOLDER(10:17)).EQ.'AL      ') THEN
                          GO TO 1000
                      ELSE
                      END IF
                      IF((HOLDER(10:17)).EQ.'ADTOR   '.OR.
     1                (HOLDER(10:17)).EQ.'AETOR   '.OR.
     2                (HOLDER(10:17)).EQ.'AFTOR   '.OR.
     3                (HOLDER(10:17)).EQ.'AGTOR   '.OR.
     4                (HOLDER(10:17)).EQ.'YD      '.OR.
     4                (HOLDER(10:17)).EQ.'ZD      '.OR.
     4                (HOLDER(10:17)).EQ.'PIVX    '.OR.
     4                (HOLDER(10:17)).EQ.'PIVY    '.OR.
     4                (HOLDER(10:17)).EQ.'PIVZ    '.OR.
     4                (HOLDER(10:17)).EQ.'GDX     '.OR.
     4                (HOLDER(10:17)).EQ.'GDY     '.OR.
     5                (HOLDER(10:17)).EQ.'GDZ     '.OR.
     5                (HOLDER(10:17)).EQ.'ALPHA   '.OR.
     5                (HOLDER(10:17)).EQ.'BETA    '.OR.
     5                (HOLDER(10:17)).EQ.'GAMMA   '.OR.
     5                (HOLDER(10:17)).EQ.'GALPHA  '.OR.
     5                (HOLDER(10:17)).EQ.'GBETA   '.OR.
     5                (HOLDER(10:17)).EQ.'GGAMMA  '.OR.
     5                (HOLDER(10:17)).EQ.'XD      ')THEN
                          GO TO 1000
                      ELSE
                      END IF
                      IF((HOLDER(10:17)).EQ.'CLAP    '.OR.
     1                (HOLDER(10:17)).EQ.'COBS    '.OR.
     2                (HOLDER(10:17)).EQ.'RDTOR   '.OR.
     3                (HOLDER(10:17)).EQ.'CVTOR   ')THEN
C
C       FOUND PIKUP TO SCALE, JUMP TO 1000
                          GO TO 1000
                      ELSE
C       DON'T SCALE PIKUP JUST JUMP TO 300
C
                      END IF
 1000                 CONTINUE
                      LNAME=HOLDER(1:18)
                      HOLDER=HOLDER(19:140)
C
C       RESOLVE NUMERIC VALUES
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL1=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL1=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL2=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL2=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL3=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL3=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL4=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL4=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
                      IF((HOLDER(1:1)).NE.',') THEN
                          AVAL5=(HOLDER(1:23))
                          HOLDER=HOLDER(24:140)
                      ELSE
                          AVAL5=','
                          HOLDER=HOLDER(2:140)
                      END IF
C
C       THE ALPHA STRING REPRESENTATIONS ARE NOW BROKEN OUT
C       AS AVAL1 TO AVAL5
C
                      IF(AVAL4.NE.',') THEN
C       MAYBE AVAL4 IS 1.0 AND WE HAVE 'SPECIAL' PIKUP OPTION
C       IN WHICH CASE NO SCALEING OCCURS.
                          AV1=AVAL4
                          CALL ATON3
                          VAL4=V1
                          IF(VAL4.EQ.1.0D0) THEN
C       CASE OF 'SPECIAL' PIKUP OPTION, NO SCALING, JUMP TO 300
                              GO TO 300
C               ELSE
C       VAL4 NOT 1.0, JUST CONTINUE
                          END IF
                      ELSE
C       AVAL4 IS DEFAULT, CONTINUE
                      END IF

C       WE ONLY SCALE THE THIRD NUMERIC WORD IF ANY
C
C       LNAME(10:17)= RD,TH,XD,YD,RDTOR,PIVX,PIVY,PIVZ
C       LNAME(10:17)= OR GLOBAL TILTS AND DECENTERS
                      IF(LNAME(10:17).EQ.'RD      '.OR.
     1                LNAME(10:17).EQ.'RDTOR   '.OR.
     2                LNAME(10:17).EQ.'XD      '.OR.
     2                LNAME(10:17).EQ.'ZD      '.OR.
     3                LNAME(10:17).EQ.'YD      '.OR.
     2                LNAME(10:17).EQ.'GDX     '.OR.
     2                LNAME(10:17).EQ.'GDY     '.OR.
     3                LNAME(10:17).EQ.'GDZ     '.OR.
     3                LNAME(10:17).EQ.'ALPHA   '.OR.
     3                LNAME(10:17).EQ.'BETA    '.OR.
     3                LNAME(10:17).EQ.'GAMMA   '.OR.
     3                LNAME(10:17).EQ.'GALPHA  '.OR.
     3                LNAME(10:17).EQ.'GBETA   '.OR.
     3                LNAME(10:17).EQ.'GGAMMA  '.OR.
     3                LNAME(10:17).EQ.'PIVX    '.OR.
     3                LNAME(10:17).EQ.'PIVY    '.OR.
     3                LNAME(10:17).EQ.'PIVZ    '.OR.
     4                LNAME(10:17).EQ.'TH      ') THEN
C       LINEAR SCALING
                          IF(AVAL4.NE.',') THEN
                              AV1=AVAL4
                              CALL ATON3
                              VAL4=V1
                              IF(LNAME(10:17).EQ.'XD      '.OR.
     1                           LNAME(10:17).EQ.'YD      '.OR.
     1                           LNAME(10:17).EQ.'PIVX    '.OR.
     1                           LNAME(10:17).EQ.'PIVY    '.OR.
     1                           LNAME(10:17).EQ.'GDX     '.OR.
     1                           LNAME(10:17).EQ.'GDY     ') THEN
                                  VAL4=VAL4*DABS(W1)
                                  GO TO 1001
                              END IF
                              IF(LNAME(10:17).EQ.'ALPHA   '.OR.
     1                           LNAME(10:17).EQ.'BETA    '.OR.
     1                           LNAME(10:17).EQ.'GAMMA   '.OR.
     1                           LNAME(10:17).EQ.'GALPHA  '.OR.
     1                           LNAME(10:17).EQ.'GBETA   '.OR.
     1                           LNAME(10:17).EQ.'GGAMMA  ') THEN
                                  VAL4=-VAL4
                                  GO TO 1001
                              END IF
                              VAL4=VAL4*(W1)
 1001                         V1=VAL4
                              V1=VAL4
                              CALL NTOA3
                              AVAL4=AV1
                          ELSE
                          END IF
                      ELSE
                      END IF
C       LNAME(10:17)= CLAP OR COBS
                      IF(LNAME(10:17).EQ.'CLAP    '.OR.
     1                LNAME(10:17).EQ.'COBS    ') THEN
C       LINEAR SCALING
                          IF(AVAL4.NE.',') THEN
                              AV1=AVAL4
                              CALL ATON3
                              VAL4=V1
                              VAL4=VAL4*DABS(W1)
                              V1=VAL4
                              CALL NTOA3
                              AVAL4=AV1
                          ELSE
                          END IF
                      ELSE
                      END IF
C       LNAME(10:17)= CV,CVTOR
                      IF(LNAME(10:17).EQ.'CV      '.OR.
     1                LNAME(10:17).EQ.'CVTOR   ')THEN
C       RECIPROCAL SCALING
                          IF(AVAL4.NE.',') THEN
                              AV1=AVAL4
                              CALL ATON3
                              VAL4=V1
                              VAL4=VAL4/W1
                              V1=VAL4
                              CALL NTOA3
                              AVAL4=AV1
                          ELSE
                          END IF
                      ELSE
                      END IF
C       LNAME(10:17)= AD OR ADTOR
                      IF(LNAME(10:17).EQ.'AD      '.OR.
     1                LNAME(10:17).EQ.'AC      '.OR.
     1                LNAME(10:17).EQ.'AE      '.OR.
     2                LNAME(10:17).EQ.'AF      '.OR.
     3                LNAME(10:17).EQ.'AG      '.OR.
     1                LNAME(10:17).EQ.'AH      '.OR.
     1                LNAME(10:17).EQ.'AI     '.OR.
     1                LNAME(10:17).EQ.'AJ      '.OR.
     2                LNAME(10:17).EQ.'AK      '.OR.
     3                LNAME(10:17).EQ.'AL      '.OR.
     4                LNAME(10:17).EQ.'ADTOR   '.OR.
     5                LNAME(10:17).EQ.'AETOR   '.OR.
     6                LNAME(10:17).EQ.'AFTOR   '.OR.
     7                LNAME(10:17).EQ.'AGTOR   ') THEN
C       SPECIAL SCALING
                          IF(AVAL4.NE.',') THEN
                              AV1=AVAL4
                              CALL ATON3
                              VAL4=V1
                              IF(LNAME(10:17).EQ.'AC     ')THEN
                                  VAL4=VAL4/W1
                              ELSE
                              END IF
                              IF(LNAME(10:17).EQ.'AD      '.OR.
     1                        LNAME(10:17).EQ.'ADTOR   ') THEN
                                  VAL4=VAL4/(W1**3)
                              ELSE
                              END IF
                              IF(LNAME(10:17).EQ.'AE      '.OR.
     1                        LNAME(10:17).EQ.'AETOR   ') THEN
                                  VAL4=VAL4/(W1**5)
                              ELSE
                              END IF
                              IF(LNAME(10:17).EQ.'AF      '.OR.
     1                        LNAME(10:17).EQ.'AFTOR   ') THEN
                                  VAL4=VAL4/(W1**7)
                              ELSE
                              END IF
                              IF(LNAME(10:17).EQ.'AG      '.OR.
     1                        LNAME(10:17).EQ.'AGTOR   ') THEN
                                  VAL4=VAL4/(W1**9)
                              ELSE
                              END IF
                              IF(LNAME(10:17).EQ.'AH      ') THEN
                                  VAL4=VAL4/(W1**11)
                              ELSE
                              END IF
                              IF(LNAME(10:17).EQ.'AI      ') THEN
                                  VAL4=VAL4/(W1**13)
                              ELSE
                              END IF
                              IF(LNAME(10:17).EQ.'AJ      ') THEN
                                  VAL4=VAL4/(W1**15)
                              ELSE
                              END IF
                              IF(LNAME(10:17).EQ.'AK      ') THEN
                                  VAL4=VAL4/(W1**17)
                              ELSE
                              END IF
                              IF(LNAME(10:17).EQ.'AL      ') THEN
                                  VAL4=VAL4/(W1**19)
                              ELSE
                              END IF
                              V1=VAL4
                              CALL NTOA3
                              AVAL4=AV1
                          ELSE
                          END IF
                      ELSE
                      END IF
C
C       RE-CONSTRUCT HOLDER
C
                      HOLDER=LNAME
                      IF(AVAL1.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL1//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL2.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL2//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL3.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL3//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL4.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL4//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      IF(AVAL5.NE.',') THEN
                          HOLDER=trim(HOLDER)//AVAL5//','
                      ELSE
                          HOLDER=trim(HOLDER)//','
                      END IF
                      GO TO 299
                  ELSE
C       NOT PIKUP
                  END IF
                  GO TO 300
C************************************************************
 299              CONFG(I,J)=HOLDER(1:140)
 300          CONTINUE
C
C       FINISHED SCALING OF SURFACE DATA
              DO J=1,CFGCNT(I)
C     I IS THE CONFIG NUMBER, J IS THE ENTRY NUMBER
                  IF(CONFG(I,J)(1:2).EQ.'C1') THEN
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.4.OR.SPECF2(I,J).EQ.4) THEN
C     TYPE 4
                          VAL2=VAL2*W1
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
C
                  IF(CONFG(I,J)(1:2).EQ.'C2') THEN
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.4.OR.SPECF2(I,J).EQ.4) THEN
C     TYPE 4
                          VAL2=VAL2*W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
C
                  IF(CONFG(I,J)(1:2).EQ.'C3') THEN
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.4.OR.SPECF2(I,J).EQ.4) THEN
C     TYPE 4
                          VAL2=VAL2*W1
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2*W1
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2*W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
C
                  IF(CONFG(I,J)(1:2).EQ.'C4') THEN
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/W1
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2*W1
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2*W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
C
                  IF(CONFG(I,J)(1:2).EQ.'C5') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/W1
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2*W1
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2*W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:2).EQ.'C6') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:2).EQ.'C7') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**2)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2*W1
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2*W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:2).EQ.'C8') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**2)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2*W1
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2*W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:2).EQ.'C9') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2*W1
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**2)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2*W1
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2*W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C10') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**2)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C11') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/W1
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**3)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2*W1
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2*W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C12') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**2)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**3)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C13') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**3)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**3)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2/W1
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C14') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**4)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**3)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2/(W1**2)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C15') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**5)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**3)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2/(W1**3)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C16') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**6)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**4)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2/(W1**4)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/W1
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C17') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**7)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**4)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2/(W1**5)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**2)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C18') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**4)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2/(W1**6)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**2)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C19') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**4)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2/(W1**7)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**2)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C20') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**4)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**2)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C21') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**4)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**3)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C22') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**12)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**5)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**3)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C23') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**13)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**5)
                      END IF
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**3)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C24') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**14)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**5)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**3)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C25') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**15)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**5)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**3)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C26') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**16)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**5)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**4)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C27') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**17)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**5)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**4)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C28') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**18)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**5)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**4)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C29') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**19)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**6)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**4)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C30') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**20)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**6)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**4)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C31') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**21)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**6)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**4)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C32') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**22)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**6)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**5)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C33') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**23)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**6)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**5)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C34') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**24)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**6)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**5)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C35') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**25)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**6)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**5)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C36') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**26)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**6)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**5)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C37') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**27)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**7)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**5)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C38') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**28)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**7)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**5)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C39') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**29)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**7)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**6)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C40') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**30)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**7)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**6)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C41') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**31)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**7)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**6)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C42') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**32)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**7)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**6)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C43') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**33)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**7)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**6)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C44') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**34)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**7)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**6)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C45') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**35)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**7)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**6)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C46') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**36)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**6)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C47') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**37)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**7)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C48') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.
     1                SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
C     TYPE 1 OR 6
                          VAL2=VAL2/(W1**38)
                      END IF
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**7)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C49') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**7)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C50') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**7)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C51') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**7)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C52') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**7)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C53') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**7)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C54') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**7)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C55') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**8)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**7)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C56') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**8)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C57') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**8)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C58') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**8)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C59') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**8)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C61') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**8)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C62') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**8)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C63') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**8)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C64') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**8)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C65') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**8)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C66') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**9)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**9)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C67') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**9)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C68') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**9)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C69') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**9)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C70') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**9)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C71') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**9)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C72') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**9)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C73') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**9)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C74') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**9)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C76') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**9)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C77') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C78') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**10)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C79') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C80') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C81') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C82') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C83') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C84') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C85') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C86') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C87') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C88') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
                      IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
C     TYPE 13
                          VAL2=VAL2/(W1**10)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C89') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C90') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
                  IF(CONFG(I,J)(1:3).EQ.'C91') THEN
C                     SCALE IT
C     BREAK OUT SURF NUMBER AND VALUE
                      AN1=CONFG(I,J)(10:23)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.
     1                SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
C     TYPE 7 OR 8
                          VAL2=VAL2/(W1**11)
                      END IF
C     CONVERT THE SCALED VALUE
                      N1=VAL2
                      CALL AUXNTA
                      AVAL2=AN1
C     STORE THE SCALED VALUE
                      CONFG(I,J)(34:56)=AVAL2
                  END IF
              END DO
C
 10       CONTINUE
          RETURN
      END
C SUB CFPIKD.FOR
      SUBROUTINE CFPIKD
C
          IMPLICIT NONE
C
C       WHEN A PARTICULAR CONFIGURATION BECOMES THE MAIN OR
C       CURRENT LENS, IF INDIRECT OR ILLEGAL PIKUPS ARE FOUND,
C       THIS SUBROUTINE REMOVES THEM FROM THAT CONFIGURATUIONS
C       DATA IN THE CONFIGS ARRAY.
C
          CHARACTER AVAL1*23,LINE1*32,LINE2*17
C
          INTEGER F12,I,J,CHGLIN,PIKLIN,K,CFI,CFJ,CFF12
C
          COMMON/DPIKER/CFI,CFJ,CFF12
C
          COMMON/DPIKER2/PIKLIN
C
          REAL*8 VAL1
C
          COMMON/JK_NTA3/VAL1,AVAL1
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
C
          F12=CFF12
          I=CFI
          J=CFJ
C
C       FOR CONFIGURATION F12, AFTER THE LINE
C       CHG     , (I REPRESENTED AS A REAL*8 NUMBER IN
C       D23.15) THERE IS A PIKUP WHICH MUST BE REMOVED.
C
C       THE TYPE OF PIKUP IS DESIGNATED BY THE INTEGER VALUE OF
C                               J
C                       ENCODED AS:
C                       1=RD
C                       2=CV
C                       3=TH
C                       4=CC
C                       5=AD
C                       6=AE
C                       7=AF
C                       8=AG
C                       9=CVTOR
C                       10=RDTOR
C                       11=PRO
C                       12=NPRO
C                       13=YD
C                       14=XD
C                       15=ALPHA
C                       16=BETA
C                       17=GAMMA
C                       18=CLAP
C                       19=COBS
C                       20=GLASS
C                       21=CCTOR
C                       22=ADTOR
C                       23=AETOR
C                       24=AFTOR
C                       25=AGTOR
C                       26=AC
C                       27=AH
C                       28=AI
C                       29=AJ
C                       30=AK
C                       31=AL
C                       32=THOAL
C                       33=ZD
C                       34=PIVX
C                       35=PIVY
C                       36=PIVZ
C                       37=GDX
C                       38=GDX
C                       39=GDX
C                       40=GALPHA
C                       41=GBETA
C                       42=GGAMMA
C                       43=GRT
C       DETERMINE THE VALUE OF LINE2
          LINE2='                 '
          IF(J.EQ.1)  LINE2='PIKUP    RD      '
          IF(J.EQ.2)  LINE2='PIKUP    CV      '
          IF(J.EQ.3)  LINE2='PIKUP    TH      '
          IF(J.EQ.4)  LINE2='PIKUP    CC      '
          IF(J.EQ.5)  LINE2='PIKUP    AD      '
          IF(J.EQ.6)  LINE2='PIKUP    AE      '
          IF(J.EQ.7)  LINE2='PIKUP    AF      '
          IF(J.EQ.8)  LINE2='PIKUP    AG      '
          IF(J.EQ.9)  LINE2='PIKUP    CVTOR   '
          IF(J.EQ.10) LINE2='PIKUP    RDTOR   '
          IF(J.EQ.11) LINE2='PIKUP    PRO     '
          IF(J.EQ.12) LINE2='PIKUP    NPRO    '
          IF(J.EQ.13) LINE2='PIKUP    YD      '
          IF(J.EQ.14) LINE2='PIKUP    XD      '
          IF(J.EQ.15) LINE2='PIKUP    ALPHA   '
          IF(J.EQ.16) LINE2='PIKUP    BETA    '
          IF(J.EQ.17) LINE2='PIKUP    GAMMA   '
          IF(J.EQ.18) LINE2='PIKUP    CLAP    '
          IF(J.EQ.19) LINE2='PIKUP    COBS    '
          IF(J.EQ.20) LINE2='PIKUP    GLASS   '
          IF(J.EQ.21) LINE2='PIKUP    CCTOR   '
          IF(J.EQ.22) LINE2='PIKUP    ADTOR   '
          IF(J.EQ.23) LINE2='PIKUP    AETOR   '
          IF(J.EQ.24) LINE2='PIKUP    AFTOR   '
          IF(J.EQ.25) LINE2='PIKUP    AGTOR   '
          IF(J.EQ.26) LINE2='PIKUP    AC      '
          IF(J.EQ.27) LINE2='PIKUP    AH      '
          IF(J.EQ.28) LINE2='PIKUP    AI      '
          IF(J.EQ.29) LINE2='PIKUP    AJ      '
          IF(J.EQ.30) LINE2='PIKUP    AK      '
          IF(J.EQ.31) LINE2='PIKUP    AL      '
          IF(J.EQ.32) LINE2='PIKUP    THOAL   '
          IF(J.EQ.33) LINE2='PIKUP    ZD      '
          IF(J.EQ.34) LINE2='PIKUP    PIVX    '
          IF(J.EQ.35) LINE2='PIKUP    PIVY    '
          IF(J.EQ.36) LINE2='PIKUP    PIVZ    '
          IF(J.EQ.37) LINE2='PIKUP    GDX     '
          IF(J.EQ.38) LINE2='PIKUP    GDY     '
          IF(J.EQ.39) LINE2='PIKUP    GDZ     '
          IF(J.EQ.40) LINE2='PIKUP    GALPHA  '
          IF(J.EQ.41) LINE2='PIKUP    GBETA   '
          IF(J.EQ.42) LINE2='PIKUP    GGAMMA  '
          IF(J.EQ.43) LINE2='PIKUP    GRT     '
C       CONSTRUCT THE CHG LINE WHICH MUST BE SEARCHED FOR AS
C       LINE1
          VAL1=DBLE(I)
          CALL NTOA3
          LINE1='CHG     ,'//AVAL1
C
C       CHGLIN IS THE LINE WERE THE CORRECT CHG IS LOCATED
C
          DO 100 K=1,CFGCNT(F12)
              EE12=CONFG(F12,K)
              HOLDER=EE12
              IF((HOLDER(1:32)).EQ.LINE1) THEN
                  CHGLIN = K
                  GO TO 101
              ELSE
C       KEEP LOOKING, WE KNOW IT IS THERE.
              END IF
 100      CONTINUE
 101      CONTINUE
C
C       NOW SEARCH FROM LINE CHGLIN TO CFGCNT(F12) FOR THE
C       PIKUP DESIGNATED BY LINE2
          DO 200 K=CHGLIN,CFGCNT(F12)
              EE12=CONFG(F12,K)
              HOLDER=EE12
              IF((HOLDER(1:17)).EQ.LINE2) THEN
                  PIKLIN=K
              ELSE
C       KEEP GOING IT MUST BE THERE
              END IF
 200      CONTINUE
C
C       NOW REMOVE LINE PIKLIN FROM CFG F12 USING SUBROUTINE
C       REMPIK
          CALL REMPIK
C
          RETURN
C
      END
