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

C       THIRD FILE OF CONFIGS FILES

C SUB CFGUP.FOR
      SUBROUTINE CFGUP
C
          IMPLICIT NONE
C
          INTEGER I,J
C
C       THIS SUBROUTINE IS CALLED TO UPDATE THE
C       CONFIGURATION DATA USING THE COMMAND
C       (UPDATE CONFIGS) FROM THE CMD LEVEL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datcfg.inc'
C
C       CONFIGS TAKES NO INPUT
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"UPDATE CONFIGS" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       INITIALIZE REFERENCE RAY LOGICAL
          IF(RAYCLEAR) THEN
              FOBYES=.FALSE.
              REFEXT=.FALSE.
              NULL = .FALSE.
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              SPDEXT=.FALSE.
              GSPDEXT=.FALSE.
              CPFNEXT=.FALSE.
              CALL DELPSF
              FAIL=.TRUE.
          END IF
C
C                       SET F1=0 AND F11=1
          F1=0
          F11=1
          F12=2
          DO J=2,MAXCFG
              DO I=0,INT(SYSTEM1(20))
                  SPECF2(J,I)=INT(DABS(ALENS(34,I)))
              END DO
          END DO
C       IF THIS CONFIG IS EMPTY, THE STARTING ADDRESS IS 0
C                       RETURN
      END
C SUB CFGPRT.FOR.FOR
      SUBROUTINE CFGPRT
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED FROM THE CMD LEVEL
C       IT IS USED TO LIST THE CONFIGURATION DATA
C       DESIGNATED BY THE CONFIGURATION NUMBER INT(W1)
C       IF DF1 NOT 1. PRINT ALL THE NON-BLANK COFIGURATION
C       DATA WHEN DF1 = 1
C       CURRENT CONFIGURATION NUMBER IS NOT AFFECTED
C
          CHARACTER BL1*58
C
          INTEGER ALL,IEND,I,IC,J
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
C
          BL1=AA//AA//'                  '
C
C       IEND IS THE LAST NON-BLANK CONFIG
          IEND=INT(SYSTEM1(56))
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"CF" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       DEFAULT NUMERIC INPUT
          IF(DF1.EQ.1) THEN
C       ALL IS A FLAG USED TO TELL THIS ROUTINE TO PRINT ALL
C       CONFIGS DATA
              ALL=1
              DF1=0
              GO TO 56
          END IF
C
C       CFG OUT OF RANGE
          IF(INT(W1).LT.2.OR.INT(W1).GT.MAXCFG) THEN
              WRITE(OUTLYNE,*)'NUMERIC INPUT BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'VALID CFG NUMBERS ARE 2 TO ',MAXCFG
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       MUST BE A VALID ADDRESS
C       ONLY ONE SURFACE TO BE PRINTED
C
C       CASE OF EMPTY CFG
          IF(CFGCNT(INT(W1)).EQ.0) THEN
              WRITE(OUTLYNE,100) INT(W1)
              CALL SHOWIT(0)
              RETURN
          ELSE
C       WRITE OUT CONFIGS DATA
              WRITE(OUTLYNE,1000) INT(W1)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1100)
              CALL SHOWIT(0)
              DO I=1,CFGCNT(INT(W1))
                  EE12=CONFG(INT(W1),I)
                  HOLDER=EE12
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      IF((HOLDER(75:140)).EQ.BL1) THEN
C     JUST DO ONE LINE OF OUTPUT
                          WRITE(OUTLYNE,1200) I,HOLDER(1:74)
                          CALL SHOWIT(0)
                      ELSE
                          WRITE(OUTLYNE,1200) I,HOLDER(1:74)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,1201) HOLDER(75:140)
                          CALL SHOWIT(0)
                      END IF
                  ELSE
C       OUT NOT 6 OR 7
                      WRITE(OUTLYNE,1202) I,HOLDER(1:126)
                      CALL SHOWIT(0)
                  END IF
              END DO
          END IF
          RETURN
 56       CONTINUE
          IF(ALL.EQ.1) THEN
C
C       PRINT ALL NON-BLANK DATA
C
              IC=0
              DO 20 I=2,IEND
                  IF(CFGCNT(I).GT.0)  THEN
C       DISPLAY THIS CONFIG
                      IC=IC+1
C       PRINT THIS CONFIG
                      WRITE(OUTLYNE,1000) I
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,1100)
                      CALL SHOWIT(0)
                      DO 30 J=1,CFGCNT(I)
                          EE12=CONFG(I,J)
                          HOLDER=EE12
C       WRITE OUT CONFIGS DATA
                          IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                              IF((HOLDER(75:140)).EQ.BL1) THEN
                                  WRITE(OUTLYNE,1200) J,HOLDER(1:74)
                                  CALL SHOWIT(0)
                              ELSE
                                  WRITE(OUTLYNE,1200) J,HOLDER(1:74)
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,1201) HOLDER(75:140)
                                  CALL SHOWIT(0)
                              END IF
                          ELSE
C       OUT NOT 6,7
                              WRITE(OUTLYNE,1202) J,HOLDER(1:126)
                              CALL SHOWIT(0)
                          END IF
 30                   CONTINUE
                  END IF
 20           CONTINUE
          END IF
          IF(IC.EQ.0) WRITE(OUTLYNE,110)
          CALL SHOWIT(0)
          RETURN
 110      FORMAT('NO CONFIG DATA')
 100      FORMAT('CFG NUMBER ',I3,' :NO CONFIG. DATA')
 1000     FORMAT('CONFIG. DATA FOR CONFIG. NUMBER',I3)
 1100     FORMAT(2X)
 1200     FORMAT(I3,1X,A75)
 1201     FORMAT(5X,A74)
 1202     FORMAT(I3,1X,A127)
      END
C SUB CFGOUT.FOR
      SUBROUTINE CFGOUT(LINE1,LINE2,LINE3,LINE4,
     1LINE5,LINE6,LNCNT)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED BY CFGIN2 AND
C       CAUSES THE CURRENT PROGRAM INSTRUCTION TO BE
C       RETURNED AS A PURE ALPHANUMERIC STRING WHICH
C       IS STORED IN THE CONFIG ARRAY BY SUBROUTINE
C       CFGIN2
C
          INTEGER LENG,LNCNT,RETRET,RET,LEOS,SPEOS
C
          COMMON/RETIT/RET,RETRET,LEOS,SPEOS
C
          CHARACTER AN1*23,AN2*23,AN3*23,AN4*23,
     2    AN5*23,BN1*11,BN2*11,BN3*11,BN4*11,BN5*11,AAN1*1,AAN2*1,
     3    AAN3*1,AAN4*1,AAN5*1,
     4    LINE1*140,LINE2*140,LINE3*140,LINE4*140,BLANK*140,
     5    LINE5*140,LINE6*140
C
          COMMON/BLAAA/BLANK
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          RETRET=0
          BLANK=AA//AA//AA//AA//AA//AA//AA
          LINE1(1:140)=BLANK
          LINE2(1:140)=BLANK
          LINE3(1:140)=BLANK
          LINE4(1:140)=BLANK
          LINE5(1:140)=BLANK
          LINE6(1:140)=BLANK
          LNCNT=1
C
C       SPECIAL DEFAULT SETTINGS
          CALL CFDFLT
          IF(RETRET.EQ.1) RETURN
C
C       SPECIAL DEFAULT VALUES MUST BE ASSIGNED
C       FOR THE FOLLOWING CASES
          IF(WC.EQ.'GLASS') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)= WC//WQ//',,,,,,'
              LINE2(1:140)='N1      ,'//AN1//',,,,,'
              LINE3(1:140)='N2      ,'//AN2//',,,,,'
              LINE4(1:140)='N3      ,'//AN3//',,,,,'
              LINE5(1:140)='N4      ,'//AN4//',,,,,'
              LINE6(1:140)='N5      ,'//AN5//',,,,,'
              LNCNT=5
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'ASPH') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH,,,,,,'
              LINE2(1:140)='AD      ,'//AN1//',,,,,'
              LINE3(1:140)='AE      ,'//AN2//',,,,,'
              LINE4(1:140)='AF      ,'//AN3//',,,,,'
              LINE5(1:140)='AG      ,'//AN4//',,,,,'
              LINE6(1:140)='AC      ,'//AN5//',,,,,'
              LNCNT=5
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'ASPH2') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH2,,,,,,'
              LINE2(1:140)='AH      ,'//AN1//',,,,,'
              LINE3(1:140)='AI      ,'//AN2//',,,,,'
              LINE4(1:140)='AJ      ,'//AN3//',,,,,'
              LINE5(1:140)='AK      ,'//AN4//',,,,,'
              LINE6(1:140)='AL      ,'//AN5//',,,,,'
              LNCNT=5
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AD') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH,,,,,,'
              LINE2(1:140)='AD      ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AE') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH,,,,,,'
              LINE2(1:140)='AE      ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AF') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH,,,,,,'
              LINE2(1:140)='AF      ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AG') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH,,,,,,'
              LINE2(1:140)='AG      ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AH') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH2,,,,,,'
              LINE2(1:140)='AH      ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AI') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH2,,,,,,'
              LINE2(1:140)='AI      ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AJ') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH2,,,,,,'
              LINE2(1:140)='AJ      ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AK') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH2,,,,,,'
              LINE2(1:140)='AK      ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AL') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH2,,,,,,'
              LINE2(1:140)='AL      ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AC') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPH,,,,,,'
              LINE2(1:140)='AC      ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'ASPHD') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ASPHD,,,,,,'
              LINE2(1:140)=BLANK
              LINE3(1:140)=BLANK
              LINE4(1:140)=BLANK
              LINE5(1:140)=BLANK
              LNCNT=5
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'ARRAY'.AND.WQ.EQ.'ODD') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ARRAY ODD,'//AN1//','//AN2//','//AN3//',,,'
              LNCNT=1
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'ARRAYD') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='ARRAYD,,,,,,'
              LINE2(1:140)=BLANK
              LINE3(1:140)=BLANK
              LINE4(1:140)=BLANK
              LINE5(1:140)=BLANK
              LNCNT=5
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'TASPH') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='TASPH,,,,,,'
              LINE2(1:140)='ADTOR   ,'//AN1//',,,,,'
              LINE3(1:140)='AETOR   ,'//AN2//',,,,,'
              LINE4(1:140)='AFTOR   ,'//AN3//',,,,,'
              LINE5(1:140)='AGTOR   ,'//AN4//',,,,,'
              LNCNT=5
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'ADTOR') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='TASPH,,,,,,'
              LINE2(1:140)='ADTOR   ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AETOR') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='TASPH,,,,,,'
              LINE2(1:140)='AETOR   ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AFTOR') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='TASPH,,,,,,'
              LINE2(1:140)='AFTOR   ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'AGTOR') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='TASPH,,,,,,'
              LINE2(1:140)='AGTOR   ,'//AN1//',,,,,'
              LNCNT=2
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'TASPHD') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='TASPHD,,,,,,'
              LINE2(1:140)=BLANK
              LINE3(1:140)=BLANK
              LINE4(1:140)=BLANK
              LINE5(1:140)=BLANK
              LNCNT=5
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'TILT'.OR.WC.EQ.'RTILT') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)= WC//WQ//',,,,,,'
              LINE2(1:140)='ALPHA   ,'//AN1//',,,,,'
              LINE3(1:140)='BETA    ,'//AN2//',,,,,'
              LINE4(1:140)='GAMMA   ,'//AN3//',,,,,'
              LNCNT=4
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'TILTD') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='TILTD,,,,,,'
              LINE2(1:140)=BLANK
              LINE3(1:140)=BLANK
              LINE4(1:140)=BLANK
              LINE5(1:140)=BLANK
              LNCNT=1
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'PIVOTD') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='PIVOTD,,,,,,'
              LINE2(1:140)=BLANK
              LINE3(1:140)=BLANK
              LINE4(1:140)=BLANK
              LINE5(1:140)=BLANK
              LNCNT=1
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'DEC') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='YD      ,'//AN1//',,,,,'
              LINE2(1:140)='XD      ,'//AN2//',,,,,'
              LINE3(1:140)='ZD      ,'//AN3//',,,,,'
              LNCNT=3
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'PIVOT') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)='PIVX    ,'//AN1//',,,,,'
              LINE2(1:140)='PIVY    ,'//AN2//',,,,,'
              LINE3(1:140)='PIVZ    ,'//AN3//',,,,,'
              LNCNT=3
              RETURN
          ELSE
          END IF
          IF(WC.EQ.'MODEL') THEN
              CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
     1        BN1,BN2,BN3,BN4,BN5)
C
              LINE1(1:140)= WC//WQ//',,,,,,'
              LINE2(1:140)='INDEX   ,'//AN1//',,,,,'
              LINE3(1:140)='VNUM    ,'//AN2//',,,,,'
              LINE4(1:140)='DPART   ,'//AN3//',,,,,'
              LNCNT=4
              RETURN
          ELSE
          END IF
C       CHECK FOR PRESECE OF A QUALIFIER
          IF(SQ.EQ.1) THEN
C       THERE IS A QUALIFIER WORD
C       NOW CHECK FOR PRESENCE OF A STRING
              IF(SST.EQ.1) THEN
C       THERE IS A QUALIFIER WORD AND A STRING  AND
C       NO NUMERIC WORDS SINCE STRINGS AND NUMERIC WORDS
C       ARE MUTUALLY EXCLUSIVE.
C       PRODUCE OUTPUT
C
C
                  LINE1(1:140)=WC//CHAR(32)//WQ//CHAR(32)//WS
                  GO TO 40
              ELSE
C       THERE IS NO STRING,THERE ARE NUMERIC WORDS AND A QUALIFIER.
C       BEFORE WRITING THE NUMERIC DATA,IT MUST BE CONVERTED
C       INTO A STRING VARIABLE. OTHERWISE HANDELING THE DEFAULT
C       VALUES BECOMES AN S.O.B..
C       CONVERT THE NUMERIC VALES TO STRING VALUES WITH
C       SUBROUTINE NWTOAWB.  THE CALL IS :
C
C               CALL NWTOAWB(W1,W2,W3,W4,W5,AN1,AN2,AN3,AN4,AN5,
C     1  BN1,BN2,BN3,BN4,BN5)
C       THW RETURNED VALUES ARE AN1,AN2,AN3,AN4,AN5
C
                  CALL NWTOAWB(W1,W2,W3,W4
     1            ,W5,AN1,AN2,AN3,AN4,AN5,BN1,BN2,BN3,BN4,BN5)
C
C       DEAL WITH THE DEFAULT VALUES
                  AAN1=','
                  AAN2=','
                  AAN3=','
                  AAN4=','
                  AAN5=','
C
C       PRODUCE OUTPUT. WRITE NUMERIC WORDS EVEN IF ALL ZERO.
C
C       SPECIAL CASES:
C
C
                  LINE1(1:140)=WC//CHAR(32)//WQ//','
                  IF(DF1.EQ.1) THEN
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AAN1
                  ELSE
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AN1//','
                  END IF
                  IF(DF2.EQ.1) THEN
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AAN2
                  ELSE
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AN2//','
                  END IF
                  IF(DF3.EQ.1) THEN
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AAN3
                  ELSE
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AN3//','
                  END IF
                  IF(DF4.EQ.1) THEN
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AAN4
                  ELSE
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AN4//','
                  END IF
                  IF(DF5.EQ.1) THEN
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AAN5
                  ELSE
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AN5//','
                  END IF
                  GO TO 40
              END IF
          ELSE
C       NO QUALIFIER WORD
C       CHECK FOR THE PRESENCE OF A STRING
              IF(SST.EQ.1) THEN
C       THERE IS A STRING WITHOUT A QUALIFIER AND NO NUMERIC INPUT
C       PRODUCE OUTPUT
C
                  LINE1(1:140)=WC//CHAR(32)//WS
                  GO TO 40
              ELSE
C       THERE IS NO QUALIFIER AND NO STRING
C       THERE IS NUMERIC INPUT.
C       BEFORE WRITING THE NUMERIC DATA,IT MUST BE CONVERTED
C       INTO A STRING VARIABLE. OTHERWISE HANDELING THE DEFAULT
C       VALUES BECOMES AN S.O.B..
C       CONVERT THE NUMERIC VALES TO STRING VALUES WITH
C       SUBROUTINE NWTOAWB.  THE CALL IS :
C
C               CALL NWTOAWB(W1,W2,W3,W4,W5,,AN1,AN2,AN3,AN4,AN5)
C       THW RETURNED VALUES ARE AN1,AN2,AN3,AN4,AN5,BN1,BN2,BN3,
C     1  BN4,BN5
C
                  CALL NWTOAWB(W1,W2,W3,W4
     1            ,W5,AN1,AN2,AN3,AN4,AN5,BN1,BN2,BN3,BN4,BN5)
C
C       DEAL WITH THE DEFAULT VALUES
C       AAN1=','
                  AAN2=','
                  AAN3=','
                  AAN4=','
                  AAN5=','
C
C       PRODUCE OUTPUT. WRITE NUMERIC WORDS EVEN IF ALL ZERO.
                  LINE1(1:140)=WC//','
                  IF(DF1.EQ.1) THEN
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AAN1
                  ELSE
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AN1//','
                  END IF
                  IF(DF2.EQ.1) THEN
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AAN2
                  ELSE
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AN2//','
                  END IF
                  IF(DF3.EQ.1) THEN
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AAN3
                  ELSE
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AN3//','
                  END IF
                  IF(DF4.EQ.1) THEN
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AAN4
                  ELSE
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AN4//','
                  END IF
                  IF(DF5.EQ.1) THEN
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AAN5
                  ELSE
                      CALL LTH140(LINE1,LENG)
                      LINE1(1:140)=LINE1(1:LENG)//AN5//','
                  END IF
              END IF
          END IF
C
 40       CONTINUE
          LINE2(1:140)=BLANK
          LINE3(1:140)=BLANK
          LINE4(1:140)=BLANK
          LINE5(1:140)=BLANK
          LINE6(1:140)=BLANK
          LNCNT=1
          RETURN
      END
C SUB CFGIN3.FOR
      SUBROUTINE CFGIN3
C
          IMPLICIT NONE
C
C       SPECIAL VERSION OF CFGIN2 FOR USE BY "REMOVE"
C
          INTEGER RET,KEND,K,RETRET,LEOS,SPEOS
C
          COMMON/RETIT/RET,RETRET,LEOS,SPEOS
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          RET=0
          KEND=1
          DO K=2,MAXCFG
              IF(CFGCNT(K).GT.0) KEND=K
          END DO
          SYSTEM1(56)=DBLE(KEND)
          SYSP(56)=SYSTEM1(56)
C
          CALL CFGCLN
          CALL LCLEAN
          CALL CFGCLN
C               RETURN TO CMD LEVEL
          F1=1
          IF(F10.EQ.1) F10=0
          IF(F11.EQ.1) F11=0
C               RETURN CFG COUNTER TO MAIN CFG NUMBER 1
          F12=1
          F1=0
          F6=1
          F22=1
          LNSTYP=1
          CALL LNSEOS
          IF(F28.EQ.0) CALL VCHECK
          RETURN
      END
C SUB CFGIN2.FOR
      SUBROUTINE CFGIN2
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED FROM THE CONFIGS INPUT OR
C       CONFIGS UPDATE LEVEL. IT WORKS INSIDE THE CONFIGS
C       LEVEL TO SET UP THE CONFIGS DATA IN THE CONFIGS ARRAY
C       CONFIGS. IF RET=1, ISSUE AN EOS AND RETURN, USED WITH
C       THE CMD LEVEL REMOVE COMMAND
C
          CHARACTER
     1    LINE1*140,LINE2*140,LINE3*140,LINE4*140,LINE5*140,
     2    BLANK*140,LINE6*140
C
          LOGICAL CEE
C
          COMMON/BLAAA/BLANK
C
          INTEGER KEND
          INTEGER LEOS,SPEOS,RET,LNCNT,RETRET,K
          COMMON/RETIT/RET,RETRET,LEOS,SPEOS
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
C
          IF(WC.EQ.'M'.OR.WC.EQ.'C') THEN
              CALL MESCOM
              RETURN
          END IF
          BLANK=AA//AA//AA//AA//AA//AA//AA
C
          IF(RET.EQ.1) THEN
              LEOS=0
              SPEOS=0
              RET=0
              WC='EOS'
          END IF
C
C       IN ORDER TO CLOSE THE CONFIGS OR UPDATE CONFIGS
C       ENTRY THE COMMAND
C                       (EOS) IS USED,BUT THERE IS
C       A PROBLEM. AS IT IS PERMISSIBLE TO ALSO USE
C       UPDATE LENS AND UPDATE SPSRF LEVELS INSIDE
C       CONFIGS OR UPDATE CONFIGS, WE MUST PROPERLY
C       HANDLE NESTING OF (EOS) COMMANDS
C       IF UPDATE LENS IS ENCOUNTERED, THE LEOS FLAG IS SET TO 1,
C       ALSO F13 IS SET TO 1 FOR USE BY QUERRY
C       THE NEXT EOS COMMAND ISSUED WILL BE USED TO CLOSE THAT LEVEL,
C       NOT TO EXIT FROM CONFIGS OR UPDATE CONFIGS.
C
C       FOR UPDATE SPSRF THE SPEOS FLAG WILL BE USED ANALOGOUSLY
C       ALSO F14 IS USED FOR QUERRY
C
C       FIRST CHECK IF THE CURRENT CONFIGURATION HAS A FULL
C       ARRAY
C
          IF(F12.NE.1.AND.CFGCNT(F12).EQ.2000) THEN
              WRITE(OUTLYNE,*)'CONFIGURATION',F12,
     1        ' :CONFIGURATION STORAGE NOW FULL'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ADDITIONAL ENTRIES ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CURRENT ARRAY IS NOT FULL, WE CAN ACCEPT COMMANDS
C*******************************************************************
C       HANDEL QUERRY OF CURRENT CFG NUMBER
          IF(WC.EQ.'CFG'.AND.DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1
     1    .AND.DF4.EQ.1.AND.DF5.EQ.1) THEN
              WRITE(OUTLYNE,567)F12
              CALL SHOWIT(0)
! 566    FORMAT(1X)
 567          FORMAT('CURRENT CONFIGURATION NUMBER IS ',I3)
              RETURN
          END IF
C*******************************************************************
C       HANDEL INPUT TO CFG BEYOND LEGAL RANGE
          IF(WC.EQ.'CFG'.AND.INT(W1).LT.1.OR.
     1    WC.EQ.'CFG'.AND.INT(W1).GT.MAXCFG) THEN
              WRITE(OUTLYNE,*)'CONFIGURATION NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C*******************************************************************
C       HANDEL 'EOS'
C
          IF(WC.EQ.'EOS') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"EOS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(LEOS.EQ.1) THEN
C       WRITE 'EOS' TO THE CONFIGS DATA FILE THEN
                  LEOS=0
                  F13=0
                  CFGCNT(F12)=CFGCNT(F12)+1
                  HOLDER(1:140)=BLANK
                  HOLDER(1:3)='EOS'
                  CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  IF(CFGCNT(F12).EQ.2000) THEN
                      WRITE(OUTLYNE,*)'CONFIGURATION',F12,
     1                ' :CONFIGURATION STORAGE NOW FULL'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'ADDITIONAL DATA ENTRY NOT ALLOWED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  RETURN
              END IF
              IF(SPEOS.EQ.1) THEN
C       WRITE 'EOS' TO THE CONFIGS DATA FILE THEN
                  CFGCNT(F12)=CFGCNT(F12)+1
                  HOLDER(1:140)=BLANK
                  HOLDER(1:3)='EOS'
                  CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  SPEOS=0
                  F14=0
                  IF(CFGCNT(F12).EQ.2000) THEN
                      WRITE(OUTLYNE,*)
     1                'CONFIGURATION',F12,' :CONFIGURATION STORAGE FULL'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'ADDITIONAL DATA ENTRY NOT ALLOWED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  RETURN
              END IF
C       NEITHER LEOS OR SPEOS
C       EXIT CONFIGS OR EXIT UPDATE CONFIGS AND RETURN TO THE CMD
C       LEVEL
C               IS EOS W/O LEOS OR SPEOS
C       RETURN TO CMD LEVEL
C
C       CALCULATE THE CORRECT VALUE FOR SYSTEM1(56)
              KEND=1
              DO K=2,MAXCFG
                  IF(CFGCNT(K).GT.0) KEND=K
              END DO
              SYSTEM1(56)=DBLE(KEND)
              SYSP(56)=SYSTEM1(56)
C
C       THIS IS SERIOUS, WE ARE LEAVING THE CONFIG SUBFILE!
C       WE MUST CLEAN UP FIRST
C
              CALL CFGCLN
              CALL LCLEAN
              CALL CFGCLN
C
C
C       THIS REMOVES REDUNDANT SETS OF U L/EOS AND U SP/EOS
C       IF NO U L/EOS WAS USED.
C
              IF(F10.EQ.1) F10=0
              IF(F11.EQ.1) F11=0
              IF(F13.EQ.1) F13=0
              IF(F14.EQ.1) F14=0
C               RETURN CFG COUNTER TO MAIN CFG NUMBER 1
              F12=1
              F1=0
              F6=1
              LNSTYP=1
              CALL LNSEOS
              IF(F28.EQ.0) CALL VCHECK
              RETURN
          END IF
C**********************************************************************
C       THE COMMAND CFG,W1 IS USED TO CHANGE TO A DIFFERENT
C       CONFIG OR TO INTERROGATE THE CURRENT CONFIG BEING INPUT
C       OR CHANGED. SINCE CONFIG 1 CAN NOT BE CHANGED, ENTRY TO THE
C       CONFIGS LEVEL VIA CONFIGS OR UPDATE CONFIGS LEAVES THE USER AT
C       CFG=2 AS A DEFAULT
          IF(WC.EQ.'CFG') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"CFG" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"CFG" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C****************************************************************
C       SN NOT 0,NUMERIC INPUT TO CFG WAS GIVEN
              IF(LEOS.EQ.1.) THEN
C       CFG NOT AN ALLOWED COMMAND
                  WRITE(OUTLYNE,*)'COMMAND "CFG" NOT ALLOWED IN LENS UPDATE'
                  CALL SHOWIT(1)
                  IF(F10.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS INPUT LEVEL'
                  IF(F11.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS UPDATE LEVEL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SPEOS.EQ.1.) THEN
C       CFG NOT AN ALLOWED COMMAND
                  WRITE(OUTLYNE,*)
     1            'COMMAND "CFG" NOT ALLOWED IN SPECIAL SURFACE UPDATE'
                  CALL SHOWIT(1)
                  IF(F10.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS INPUT LEVEL'
                  IF(F11.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS UPDATE LEVEL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(DF1.EQ.0) THEN
C       CHANGE TO CONFIG NUMBER INT(W1)
                  IF(INT(W1).EQ.1) THEN
                      WRITE(OUTLYNE,*)'CONFIGURATION #1 NOT AVAILABLE FOR CHANGE'
                      CALL SHOWIT(1)
                      IF(F10.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS INPUT LEVEL'
                      IF(F11.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS UPDATE LEVEL'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  F12=INT(W1)
                  RETURN
              END IF
          END IF
          IF(F12.EQ.1) THEN
              WRITE(OUTLYNE,*)'CONFIGURATION #1 NOT AVAILABLE FOR CHANGES'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       NOW WHAT ABOUT ALL THE OTHER COMMANDS WHICH GOT THROUGH
C       TO THIS POINT. FOR THEM, THE CHARACTER REPRESENTATION OF
C       EACH IS STORED IN THE APPROPRIATE LOCATION IN THE
C       CONFIGS ARRAY. THEN THE CONFIG DATA COUNTER CFGCNT(F12)
C       IS INCREMENTED BY 1 PRIOR TO THE STORAGE
C
C       CHECK FOR VALID CMD,LENS UPDATE OR SPSRF UPDATE COMMANDS
C       IN THIS SUBROUTINE USING SOME INTERNAL FLAGS.
C       USE TECHNIQUE FROM MFL OF MACROS TO WRITE OUT
C       CONFIG DATA TO STORAGE.
C       ADD U L COMMANS FOR REFRACTIVE INDICES
C       ADD CMD COMMAND TO CHANGE IMMAGE SURFACE NUMBER
C       GOT TO FINISH THIS BY FRIDAY INCLUDING DEBUG!
C
C
C       IF LEOS IS SET TO 1, ONLY UPDATE LENS COMMANDS AND EOS
C       ARE LEGAL. THIS IS AN INPUT FILE AND ALL IS RESOLVED AT
C       EOS. IF SPEOS=1 THEN ONLY SPSRF UPDATE COMMANDS ARE VALID
C
C       FOR LEOS=0 AND SPEOS=0 ONLY A FEW CMD LEVEL COMMANDS
C       AND THE CONFIGS PROCESSOR COMMANDS ARE VALID
C
          IF(LEOS.EQ.1) THEN
C       TEST FOR VALID UPDATE LENS COMMANDS
C
              IF(WC.EQ.'LI'.OR.WC.EQ.'LIC'.OR.WC.EQ.'WV'.OR.WC.EQ.'UNITS'
     1        .OR.WC.EQ.'PCW'.OR.WC.EQ.'SCW'.OR.WC.EQ.'CW'.OR.WC.EQ.'LBL'
     2        .OR.WC.EQ.'SAY'.OR.WC.EQ.'SAX'.OR.WC.EQ.'SCY'.OR.WC.EQ.'SCX'
     3        .OR.WC.EQ.'CV'.OR.WC.EQ.'RD'.OR.WC.EQ.'CC'.OR.WC.EQ.'ASPH'
     5        .OR.WC.EQ.'APY'.OR.WC.EQ.'MODE'.OR.WC.EQ.'SPTWT'.OR.WC.EQ.
     5        'LABEL'.OR.WC.EQ.'APX'.OR.WC.EQ.'CCTOR'.OR.WC.EQ.'TILTD'
     6        .OR.WC.EQ.'NAOX'.OR.WC.EQ.'NAOY'.OR.WC.EQ.'WV2'.OR.WC.EQ.'INR'
     6        .OR.WC.EQ.'SPTWT2'.OR.WC.EQ.'FNOX'.OR.WC.EQ.'FNOY'.OR.
     7        WC.EQ.'ASPH2'.OR.WC.EQ.'WRX'.OR.WC.EQ.'WRY'.OR.WC.EQ.
     8        'BDX'.OR.WC.EQ.'BDY'.OR.WC.EQ.'PIVX'.OR.WC.EQ.'PIVY'
     9        .OR.WC.EQ.'PIVZ'.OR.WC.EQ.'INI'.OR.WC.EQ.'LTYPE') THEN
                  GO TO 55
C       ALL IS WELL,PROCEED
              ELSE
C       CONTINUE TESTING
                  IF(WC.EQ.'PIY'.OR.WC.EQ.'PIX'.OR.WC.EQ.'PUY'.OR.WC.EQ.'PUX'
     1            .OR.WC.EQ.'APCY'.OR.WC.EQ.'APCX'.OR.WC.EQ.'PICY'.OR.WC.EQ.
     2            'PICX'.OR.WC.EQ.'PUCY'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'COCY'.OR.
     3            WC.EQ.'COCX'.OR.WC.EQ.'PIKUP'.OR.WC.EQ.'DEC'.OR.WC.EQ.'TILT'
     4            .OR.WC.EQ.'RTILT'.OR.WC.EQ.'ASTOP'.OR.WC.EQ.
     5            'REFS'.OR.WC.EQ.'TH'.OR.WC.EQ.'PY'.OR.WC.EQ.'PCY'.OR.WC.EQ.
     6            'CAY'.OR.WC.EQ.'PX'.OR.WC.EQ.'PCX'.OR.WC.EQ.'CAX'.OR.WC.EQ.
     7            'CLAP'.OR.WC.EQ.'COBS'.OR.WC.EQ.'GLA'.OR.WC.EQ.'SCHOTT'.OR.
     8            WC.EQ.'INRD'.OR.WC.EQ.'NODUM'.OR.WC.EQ.'SCH2000'.OR.
     9            WC.EQ.'GLASS'.OR.WC.EQ.'MODEL'.OR.WC.EQ.'GRT'.OR.WC.EQ.'GRO'
     1            .OR.WC.EQ.'GRS'.OR.WC.EQ.'GRX'.OR.WC.EQ.'GRY'.OR.WC.EQ.'GRZ'
     2            .OR.WC.EQ.'SPGR'.OR.WC.EQ.'THM'.OR.WC.EQ.'PRICE'.OR.WC.EQ.
     3            'AUTOFUNC'.OR.WC.EQ.'COATING'.OR.WC.EQ.'MULTCLAP'.OR.
     4            WC.EQ.'SPIDER'.OR.WC.EQ.'ZERO') THEN
                      GO TO 55
C       ALL IS WELL,PROCEED
                  ELSE
C       CONTINUE TESTING
                      IF( WC.EQ.'OHARA'.OR.WC.EQ.'HOYA'.OR.WC.EQ.'CHANCE'.OR.WC.EQ.
     1                'CORNIN'.OR.WC.EQ.'MATL'.OR.WC.EQ.'ZD'.OR.WC.EQ.'IDEAL'.OR.
     2                WC.EQ.'RADHARD'.OR.WC.EQ.'AIR'.OR.WC.EQ.'REFL'.OR.
     3                WC.EQ.'EOS'.OR.WC.EQ.'CHG'.OR.WC.EQ.'CVTOR'.OR.WC.EQ.'RDTOR'.OR.
     4                WC.EQ.'XD'.OR.WC.EQ.'YD'.OR.WC.EQ.'ALPHA'.OR.WC.EQ.'TASPH'
     5                .OR.WC.EQ.'TASPHD'.OR.WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.
     6                WC.EQ.'AFTOR'.OR.WC.EQ.'GLCAT'.OR.WC.EQ.'VISUAL'.OR.
     7                WC.EQ.'AGTOR'.OR.WC.EQ.'N1'.OR.WC.EQ.'N2'.OR.WC.EQ.'N3'
     8                .OR.WC.EQ.'N4'.OR.WC.EQ.'N5'.OR.WC.EQ.'USER'.OR.WC.EQ.'N6'
     9                .OR.WC.EQ.'N7'.OR.WC.EQ.'N8'.OR.WC.EQ.'N9'.OR.WC.EQ.'N10'.OR.
     1                WC.EQ.'GDX'.OR.WC.EQ.'GDY'.OR.WC.EQ.'GDZ'.OR.WC.EQ.'GALPHA'.OR.
     2                WC.EQ.'GBETA'.OR.WC.EQ.'GGAMMA'.OR.WC.EQ.'PERFECT'
     3                .OR.WC.EQ.'REAL'.OR.WC.EQ.'PARAX'.OR.WC.EQ.'PERFECT'.OR.
     4                WC.EQ.'MULTCOBS'.OR.WC.EQ.'YTORIC'.OR.WC.EQ.'XTORIC') THEN
                          GO TO 55
C       ALL IS WELL, PROCEED
                      ELSE
C               CONTINUE TESTING
                          IF(WC.EQ.'BETA'.OR.WC.EQ.'GAMMA'.OR.WC.EQ.'AD'.OR.WC.EQ.'GRTD'
     2                    .OR.WC.EQ.'AE'.OR.WC.EQ.'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'ASPHD'
     3                    .OR.WC.EQ.'CSD'.OR.WC.EQ.'AC'.OR.WQ.EQ.'AH'.OR.WC.EQ.'AI'.OR.WQ
     4                    .EQ.'CSDX'.OR.WC.EQ.'CSDY'.OR.WC.EQ.'TSD'.OR.WC.EQ.'PIKD'.OR.
     5                    WC.EQ.'CLAPD'.OR.WC.EQ.'COBSD'.OR.WC.EQ.'AJ'
     6                    .OR.WC.EQ.'AK'.OR.WC.EQ.'AL'.OR.WC.EQ.'INDEX'.OR.WC.EQ.'PXIM'.OR.
     7                    WC.EQ.'VNUM'.OR.WC.EQ.'FOOTBLOK'.OR.WC.EQ.'PIVOT'.OR.WC.EQ.
     8                    'DPART'.OR.WC.EQ.'PIVOTD'.OR.WC.EQ.'DEFORM'.OR.WC.EQ.'PYIM'.OR.
     9                    WC.EQ.'RXIM'.OR.WC.EQ.'RYIM'.OR.WC.EQ.'DELDEFOR'.OR.WC.EQ.
     1                    'PIVAXIS'.OR.WC.EQ.'RUSSIAN'.OR.WC.EQ.'REFLTIR'.OR.WC.EQ.
     2                    'REFLTIRO'.OR.WC.EQ.'HIKARI'.OR.WC.EQ.'RAYERROR'.OR.WC.EQ.'ROO'
     3                    .OR.WC.EQ.'CCR') THEN
                              GO TO 55
C       ALL IS WELL,PROCEED
                          ELSE
                              IF(WC.EQ.'ARRAY'.OR.WC.EQ.'ARRAYD'.OR.
     1                        WC.EQ.'ROO'.OR.WC.EQ.'CCR') THEN
                                  GO TO 55
C       ALL IS WELL,PROCEED
                              ELSE
C       DID NOT FIND A MATCH
                                  WRITE(OUTLYNE,*)'UPDATE LENS COMMAND ',WC,' IS NOT ALLOWED'
                                  CALL SHOWIT(1)
                                  IF(F10.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS INPUT LEVEL'
                                  IF(F11.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS UPDATE LEVEL'
                                  CALL SHOWIT(1)
                                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                                  CALL SHOWIT(1)
                                  CALL MACFAL
                                  RETURN
                              END IF
                          END IF
                      END IF
                  END IF
              END IF
          END IF
C
C       CHECK SPEOS STATUS
C
          IF(SPEOS.EQ.1) THEN
C               WE ARE USING SPSRF UPDATE COMMANDS IN CONFIGS
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       USED
C
              CEE=.FALSE.
              IF(WC.EQ.'C1') CEE=.TRUE.
              IF(WC.EQ.'C2') CEE=.TRUE.
              IF(WC.EQ.'C3') CEE=.TRUE.
              IF(WC.EQ.'C4') CEE=.TRUE.
              IF(WC.EQ.'C5') CEE=.TRUE.
              IF(WC.EQ.'C6') CEE=.TRUE.
              IF(WC.EQ.'C7') CEE=.TRUE.
              IF(WC.EQ.'C8') CEE=.TRUE.
              IF(WC.EQ.'C9') CEE=.TRUE.
              IF(WC.EQ.'C10') CEE=.TRUE.
              IF(WC.EQ.'C11') CEE=.TRUE.
              IF(WC.EQ.'C12') CEE=.TRUE.
              IF(WC.EQ.'C13') CEE=.TRUE.
              IF(WC.EQ.'C14') CEE=.TRUE.
              IF(WC.EQ.'C15') CEE=.TRUE.
              IF(WC.EQ.'C16') CEE=.TRUE.
              IF(WC.EQ.'C17') CEE=.TRUE.
              IF(WC.EQ.'C18') CEE=.TRUE.
              IF(WC.EQ.'C19') CEE=.TRUE.
              IF(WC.EQ.'C20') CEE=.TRUE.
              IF(WC.EQ.'C21') CEE=.TRUE.
              IF(WC.EQ.'C22') CEE=.TRUE.
              IF(WC.EQ.'C23') CEE=.TRUE.
              IF(WC.EQ.'C24') CEE=.TRUE.
              IF(WC.EQ.'C25') CEE=.TRUE.
              IF(WC.EQ.'C26') CEE=.TRUE.
              IF(WC.EQ.'C27') CEE=.TRUE.
              IF(WC.EQ.'C28') CEE=.TRUE.
              IF(WC.EQ.'C29') CEE=.TRUE.
              IF(WC.EQ.'C30') CEE=.TRUE.
              IF(WC.EQ.'C31') CEE=.TRUE.
              IF(WC.EQ.'C32') CEE=.TRUE.
              IF(WC.EQ.'C33') CEE=.TRUE.
              IF(WC.EQ.'C34') CEE=.TRUE.
              IF(WC.EQ.'C35') CEE=.TRUE.
              IF(WC.EQ.'C36') CEE=.TRUE.
              IF(WC.EQ.'C37') CEE=.TRUE.
              IF(WC.EQ.'C38') CEE=.TRUE.
              IF(WC.EQ.'C39') CEE=.TRUE.
              IF(WC.EQ.'C40') CEE=.TRUE.
              IF(WC.EQ.'C41') CEE=.TRUE.
              IF(WC.EQ.'C42') CEE=.TRUE.
              IF(WC.EQ.'C43') CEE=.TRUE.
              IF(WC.EQ.'C44') CEE=.TRUE.
              IF(WC.EQ.'C45') CEE=.TRUE.
              IF(WC.EQ.'C46') CEE=.TRUE.
              IF(WC.EQ.'C47') CEE=.TRUE.
              IF(WC.EQ.'C48') CEE=.TRUE.
              IF(WC.EQ.'C49') CEE=.TRUE.
              IF(WC.EQ.'C50') CEE=.TRUE.
              IF(WC.EQ.'C51') CEE=.TRUE.
              IF(WC.EQ.'C52') CEE=.TRUE.
              IF(WC.EQ.'C53') CEE=.TRUE.
              IF(WC.EQ.'C54') CEE=.TRUE.
              IF(WC.EQ.'C55') CEE=.TRUE.
              IF(WC.EQ.'C56') CEE=.TRUE.
              IF(WC.EQ.'C57') CEE=.TRUE.
              IF(WC.EQ.'C58') CEE=.TRUE.
              IF(WC.EQ.'C59') CEE=.TRUE.
              IF(WC.EQ.'C60') CEE=.TRUE.
              IF(WC.EQ.'C61') CEE=.TRUE.
              IF(WC.EQ.'C62') CEE=.TRUE.
              IF(WC.EQ.'C63') CEE=.TRUE.
              IF(WC.EQ.'C64') CEE=.TRUE.
              IF(WC.EQ.'C65') CEE=.TRUE.
              IF(WC.EQ.'C66') CEE=.TRUE.
              IF(WC.EQ.'C67') CEE=.TRUE.
              IF(WC.EQ.'C68') CEE=.TRUE.
              IF(WC.EQ.'C69') CEE=.TRUE.
              IF(WC.EQ.'C70') CEE=.TRUE.
              IF(WC.EQ.'C71') CEE=.TRUE.
              IF(WC.EQ.'C72') CEE=.TRUE.
              IF(WC.EQ.'C73') CEE=.TRUE.
              IF(WC.EQ.'C74') CEE=.TRUE.
              IF(WC.EQ.'C75') CEE=.TRUE.
              IF(WC.EQ.'C76') CEE=.TRUE.
              IF(WC.EQ.'C77') CEE=.TRUE.
              IF(WC.EQ.'C78') CEE=.TRUE.
              IF(WC.EQ.'C79') CEE=.TRUE.
              IF(WC.EQ.'C80') CEE=.TRUE.
              IF(WC.EQ.'C81') CEE=.TRUE.
              IF(WC.EQ.'C82') CEE=.TRUE.
              IF(WC.EQ.'C83') CEE=.TRUE.
              IF(WC.EQ.'C84') CEE=.TRUE.
              IF(WC.EQ.'C85') CEE=.TRUE.
              IF(WC.EQ.'C86') CEE=.TRUE.
              IF(WC.EQ.'C87') CEE=.TRUE.
              IF(WC.EQ.'C88') CEE=.TRUE.
              IF(WC.EQ.'C89') CEE=.TRUE.
              IF(WC.EQ.'C90') CEE=.TRUE.
              IF(WC.EQ.'C91') CEE=.TRUE.
              IF(WC.EQ.'C92') CEE=.TRUE.
              IF(WC.EQ.'C93') CEE=.TRUE.
              IF(WC.EQ.'C94') CEE=.TRUE.
              IF(WC.EQ.'C95') CEE=.TRUE.
              IF(WC.EQ.'C96') CEE=.TRUE.
              IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'ON')  CEE=.TRUE.
              IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'OFF') CEE=.TRUE.
              IF(WC.EQ.'SPDEL'.OR.WC.EQ.'SPECIAL'.OR.CEE.OR.WC.EQ.'C'
     1        .OR.WC.EQ.'M'.OR.WC.EQ.'EOS'.OR.WC.EQ.'GENL') THEN
                  GO TO 55
C       ALL IS WELL,PROCEED
              ELSE
C       DID NOT FIND A MATCH
                  WRITE(OUTLYNE,*)'UPDATE SPSRF COMMAND ',WC,' IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  IF(F10.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS INPUT LEVEL'
                  IF(F11.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS UPDATE LEVEL'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
C       NOT SPEOS=1,NOT SPSRF UPDATE COMMANDS,MUST BE ONE
C       OF THE FEW LEGAL CMD LEVEL COMMANDS, PROCEED
C       WITH CHECKING
          END IF
          IF(LEOS.EQ.0.AND.SPEOS.EQ.0) THEN
C
C       AT CMD LEVEL (MUST BE)
              IF(WC.EQ.'MODE'.OR.WC.EQ.'PRES'.AND.WQ.EQ.'GAS'.OR.WC.EQ.'PRES'
     1        .AND.WQ.EQ.'AIR'.OR.WC.EQ.'PRES'.AND.WQ.EQ.'HYDROGEN'.OR.WC.EQ.
     1        'PRES'.AND.WQ.EQ.'NITROGEN'.OR.WC.EQ.'UPDATE'.AND.WQ.EQ.'LENS'
     1        .OR.WC.EQ.'UPDATE'.AND.WQ.EQ.'L'.OR.WC.EQ.'U'.AND.WQ.EQ.'LENS'
     1        .OR.WC.EQ.'U'.AND.WQ.EQ.'L'.OR.WC.EQ.'UPDATE'.AND.WQ.EQ.'SPSRF'
     1        .OR.WC.EQ.'U'.AND.WQ.EQ.'SPSRF'.OR.WC.EQ.'UPDATE'.AND.WQ.EQ.'SP'
     2        .OR.WC.EQ.'PRES'.AND.WQ.EQ.'ARGON'.OR.WC.EQ.'PRES'.AND.WQ.EQ.
     1        'METHANE'.OR.WC.EQ.'PRES'.AND.WQ.EQ.'ETHANE'.OR.WC.EQ.'PRES'
     1        .AND.WQ.EQ.'HELIUM'.OR.WC.EQ.'PRES'.AND.WQ.EQ.'OXYGEN'
     3        .OR.WC.EQ.'U'.AND.WQ.EQ.'SP'.OR.WC.EQ.'FNBY'.OR.WC.EQ.'FNBX')
     1         THEN
C
C       ALL IS WELL,PROCEED
                  GO TO 55
              END IF
C
              IF(WC.EQ.'ERY'.OR.WC.EQ.'ERX'.OR.WC.EQ.'SPTWT'.OR.WC.EQ.'MAGY'
     5        .OR.WC.EQ.'MAGX'.OR.WC.EQ.'SPSRF'.AND.WQ.EQ.'ON'.OR.WC.EQ.'SPSRF'
     7        .AND.WQ.EQ.'OFF'.OR.WC.EQ.'THERM'.AND.WQ.EQ.'GLASS'.OR.WC
     1        .EQ.'SPTWT2'.OR.WC.EQ.'FLDS'
     8        .OR.WC.EQ.'THERM'.AND.WQ.EQ.'SPACE'.OR.WC.EQ.'THERM'
     9        .AND.WQ.EQ.'THICK'.OR.WC.EQ.'THERM'.AND.WQ.EQ.'SHAPE'
     7        .OR.WC.EQ.'THERM'.AND.WQ.EQ.'GAS'.OR.WC.EQ.'THERM'
     8        .AND.WQ.EQ.'OXYGEN'.OR.WC.EQ.'THERM'.AND.WQ.EQ.'AIR'.OR.
     9        WC.EQ.'THERM'.AND.WQ.EQ.'HELIUM'
     7        .OR.WC.EQ.'THERM'.AND.WQ.EQ.'NITROGEN'.OR.WC.EQ.'THERM'
     8        .AND.WQ.EQ.'HYDROGEN'.OR.WC.EQ.'THERM'.AND.WQ.EQ.'ARGON'.OR.
     9        WC.EQ.'THERM'.AND.WQ.EQ.'ETHANE'.OR.WC.EQ.'THERM'.AND.
     1        WQ.EQ.'METHANE') THEN
C
C       ALL IS WELL,PROCEED
                  GO TO 55
              ELSE
                  WRITE(OUTLYNE,*)'CMD LEVEL COMMAND ',WC,' IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  IF(F10.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS INPUT LEVEL'
                  IF(F11.EQ.1) WRITE(OUTLYNE,*)'AT CONFIGS UPDATE LEVEL'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
              WRITE(OUTLYNE,*)'SERIOUS ERROR AT END OF CFGIN2.FOR'
              CALL SHOWIT(1)
          END IF
 55       IF(WC.EQ.'UPDATE'.AND.WQ.EQ.'LENS'.OR.
     1    WC.EQ.'U'.AND.WQ.EQ.'L'.OR.
     1    WC.EQ.'U'.AND.WQ.EQ.'LENS'.OR.
     1    WC.EQ.'UPDATE'.AND.WQ.EQ.'L') THEN
              LEOS=1
              F13=1
          END IF
          IF(WC.EQ.'UPDATE'.AND.WQ.EQ.'SPSRF'.OR.
     1    WC.EQ.'U'.AND.WQ.EQ.'SP'.OR.
     1    WC.EQ.'UPDATE'.AND.WQ.EQ.'SP'.OR.
     1    WC.EQ.'U'.AND.WQ.EQ.'SPSRF') THEN
              SPEOS=1
              F14=1
          END IF
C
          CALL CFGOUT(LINE1,LINE2,LINE3,LINE4,LINE5,
     1    LINE6,LNCNT)
          IF(RETRET.EQ.1) THEN
              LINE1=BLANK
              LINE2=BLANK
              LINE3=BLANK
              LINE4=BLANK
              LINE5=BLANK
              LINE6=BLANK
              LNCNT=1
          END IF
C
C       WRITE THIS LINE TO THE CORRECT LOCATION
C       IN THE CONFIG DATA ARRAY
          IF(LINE1.NE.BLANK) THEN
              CFGCNT(F12)=CFGCNT(F12)+1
              HOLDER(1:140)=LINE1(1:140)
              CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
              IF(LINE1(1:10).EQ.'U        L'.OR.LINE1(1:13).EQ.
     1        'UPDATE   LENS'.OR.
     1        LINE1(1:10).EQ.'UPDATE   L'.OR.LINE1(1:13).EQ.
     1        'U        LENS') THEN
C       ADD A CHG,0 AFTER IT
                  CFGCNT(F12)=CFGCNT(F12)+1
                  HOLDER(1:140)=BLANK
                  HOLDER(1:34)=
     1            'CHG     , 0.000000000000000D+000,,,,,'
                  CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
              END IF
          END IF
          IF(LNCNT.GT.1) THEN
              IF(LINE2.NE.BLANK) THEN
                  CFGCNT(F12)=CFGCNT(F12)+1
                  HOLDER(1:140)=LINE2(1:140)
                  CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  IF(LINE2(1:10).EQ.'U        L'.OR.LINE2(1:13).EQ.
     1            'UPDATE   LENS'.OR.
     1            LINE2(1:10).EQ.'UPDATE   L'.OR.LINE2(1:13).EQ.
     1            'U        LENS') THEN
C       ADD A CHG,0 AFTER IT
                      CFGCNT(F12)=CFGCNT(F12)+1
                      HOLDER(1:140)=BLANK
                      HOLDER(1:34)=
     1                'CHG     , 0.000000000000000D+000,,,,,'
                      CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  END IF
              END IF
          END IF
          IF(LNCNT.GT.2) THEN
              IF(LINE3.NE.BLANK) THEN
                  CFGCNT(F12)=CFGCNT(F12)+1
                  HOLDER(1:140)=LINE3(1:140)
                  CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  IF(LINE3(1:10).EQ.'U        L'.OR.LINE3(1:13).EQ.
     1            'UPDATE   LENS'.OR.
     1             LINE3(1:10).EQ.'UPDATE   L'.OR.LINE3(1:13).EQ.
     1            'U        LENS') THEN
C       ADD A CHG,0 AFTER IT
                      CFGCNT(F12)=CFGCNT(F12)+1
                      HOLDER(1:140)=BLANK
                      HOLDER(1:34)=
     1                'CHG     , 0.000000000000000D+000,,,,,'
                      CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  END IF
              END IF
          END IF
          IF(LNCNT.GT.3) THEN
              IF(LINE4.NE.BLANK) THEN
                  CFGCNT(F12)=CFGCNT(F12)+1
                  HOLDER(1:140)=LINE4(1:140)
                  CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  IF(LINE4(1:10).EQ.'U        L'.OR.LINE4(1:13).EQ.
     1            'UPDATE   LENS'.OR.
     1             LINE4(1:10).EQ.'UPDATE   L'.OR.LINE4(1:13).EQ.
     1            'U        LENS') THEN
C       ADD A CHG,0 AFTER IT
                      CFGCNT(F12)=CFGCNT(F12)+1
                      HOLDER(1:140)=BLANK
                      HOLDER(1:34)=
     1                'CHG     , 0.000000000000000D+000,,,,,'
                      CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  END IF
              END IF
          END IF
          IF(LNCNT.GT.4) THEN
              IF(LINE5.NE.BLANK) THEN
                  CFGCNT(F12)=CFGCNT(F12)+1
                  HOLDER(1:140)=LINE5(1:140)
                  CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  IF(LINE5(1:10).EQ.'U        L'.OR.LINE5(1:13).EQ.
     1            'UPDATE   LENS'.OR.
     1             LINE5(1:10).EQ.'UPDATE   L'.OR.LINE5(1:13).EQ.
     1            'U        LENS') THEN
C       ADD A CHG,0 AFTER IT
                      CFGCNT(F12)=CFGCNT(F12)+1
                      HOLDER(1:140)=BLANK
                      HOLDER(1:34)=
     1                'CHG     , 0.000000000000000D+000,,,,,'
                      CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  END IF
              END IF
          END IF
          IF(LNCNT.GT.5) THEN
              IF(LINE6.NE.BLANK) THEN
                  CFGCNT(F12)=CFGCNT(F12)+1
                  HOLDER(1:140)=LINE6(1:140)
                  CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  IF(LINE6(1:10).EQ.'U        L'.OR.LINE6(1:13).EQ.
     1            'UPDATE   LENS'.OR.
     1             LINE6(1:10).EQ.'UPDATE   L'.OR.LINE6(1:13).EQ.
     1            'U        LENS') THEN
C       ADD A CHG,0 AFTER IT
                      CFGCNT(F12)=CFGCNT(F12)+1
                      HOLDER(1:140)=BLANK
                      HOLDER(1:34)=
     1                'CHG     , 0.000000000000000D+000,,,,,'
                      CONFG(F12,CFGCNT(F12))=HOLDER(1:140)
                  END IF
              END IF
          END IF
C
          RETURN
      END
C SUB CFGFIX.FOR
      SUBROUTINE CFGFX1(I)
C
          IMPLICIT NONE
C
          INTEGER K,I,J
C
          LOGICAL CIS
C
          REAL*8 VL
C
          CHARACTER AVL*23
C
          COMMON/JK_ATD/AVL,VL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datcfg.inc'
C
C       THIS IS SUBROUTINE CFGFX1. IT FIXES CONFIGS DATA
C     AFTER SURFACE INSERTION AT SURFACE I (OLD SURFACE I BECOMES
C     SURFACE I+1)
          IF(SYSTEM1(56).LE.1.0D0) RETURN
C     SINCE NO CONFIGS DATA EXISTED
C     SYSTEM1(56) NOT 1, PROCEED
          DO K=2,INT(SYSTEM1(56))
C     FIX CONFIG K
              DO J=1,CFGCNT(K)
                  IF(CONFG(K,J)(1:3).EQ.'CHG') THEN
C     CONVERT CONFG(K,J)(10:32) TO REAL*8, ADD 1.0D0 TO IT,
C     IF IT IS IN THE CORRECT RANGE,
C     CONVERT IT BACK TO CHARACTER *23 AND STUFF IT BACK INTO
C     CONFG(K,J)(10:32)
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).GE.I) THEN
                          VL=VL+1.0D0
                          CALL DTOACV
                          CONFG(K,J)(10:32)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT CHG
                  END IF
                  IF(CONFG(K,J)(1:7).EQ.'SPECIAL'.OR.
     1            CONFG(K,J)(1:4).EQ.'GENL') THEN
C     CONVERT CONFG(K,J)(10:32) TO REAL*8, ADD 1.0D0 TO IT,
C     IF IT IS IN THE CORRECT RANGE,
C     CONVERT IT BACK TO CHARACTER *23 AND STUFF IT BACK INTO
C     CONFG(K,J)(10:32)
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).GE.I) THEN
                          VL=VL+1.0D0
                          CALL DTOACV
                          CONFG(K,J)(10:32)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT SPECIAL OR GENL
                  END IF
                  IF(CONFG(K,J)(1:5).EQ.'SPDEL') THEN
C     CONVERT CONFG(K,J)(10:32) TO REAL*8, ADD 1.0D0 TO IT,
C     IF IT IS IN THE CORRECT RANGE,
C     CONVERT IT BACK TO CHARACTER *23 AND STUFF IT BACK INTO
C     CONFG(K,J)(10:32)
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).GE.I) THEN
                          VL=VL+1.0D0
                          CALL DTOACV
                          CONFG(K,J)(10:32)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT SPDEL
                  END IF
                  IF(CONFG(K,J)(1:5).EQ.'PIKUP') THEN
C     CONVERT CONFG(K,J)(19:41) TO REAL*8, ADD 1.0D0 TO IT,
C     IF IT IS IN THE CORRECT RANGE,
C     CONVERT IT BACK TO CHARACTER *23 AND STUFF IT BACK INTO
C     CONFG(K,J)(19:41)
                      AVL=CONFG(K,J)(19:41)
                      CALL ATODCV
                      IF(INT(VL).GE.I) THEN
                          VL=VL+1.0D0
                          CALL DTOACV
                          CONFG(K,J)(19:41)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT PIKUP
                  END IF
                  CIS=.FALSE.
                  IF(CONFG(K,J)(1:7).EQ.'MAGX'.OR.
     1            CONFG(K,J)(1:3).EQ.'MAGY') THEN
C     SPECIAL STUFF, TWO WORDS TO FIX
C     WORD #2
C     CONVERT CONFG(K,J)(34:56) TO REAL*8, ADD 1.0D0 TO IT,
C     IF IT IS IN THE CORRECT RANGE,
C     CONVERT IT BACK TO CHARACTER *23 AND STUFF IT BACK INTO
C     CONFG(K,J)(10:32)
                      AVL=CONFG(K,J)(34:56)
                      CALL ATODCV
                      IF(INT(VL).GE.I) THEN
                          VL=VL+1.0D0
                          CALL DTOACV
                          CONFG(K,J)(34:56)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
C     WORD #3
C     CONVERT CONFG(K,J)(52:71) TO REAL*8, ADD 1.0D0 TO IT,
C     IF IT IS IN THE CORRECT RANGE,
C     CONVERT IT BACK TO CHARACTER *23 AND STUFF IT BACK INTO
C     CONFG(K,J)(52:74)
                      AVL=CONFG(K,J)(52:74)
                      CALL ATODCV
                      IF(INT(VL).GE.I) THEN
                          VL=VL+1.0D0
                          CALL DTOACV
                          CONFG(K,J)(52:74)=AVL(1:23)
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT MAGX OR MAGY
                  END IF
                  CIS=.FALSE.
                  IF(CONFG(K,J)(1:2).EQ.'C1') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C2') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C3') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C4') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C5') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C6') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C7') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C8') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C9') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C10') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C11') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C12') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C13') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C14') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C15') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C16') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C17') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C18') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C19') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C10') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C21') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C22') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C23') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C24') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C25') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C26') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C27') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C28') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C29') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C30') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C31') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C32') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C33') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C34') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C35') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C36') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C37') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C38') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C39') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C40') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C41') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C42') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C43') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C44') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C45') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C46') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C47') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C48') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C49') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C50') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C51') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C52') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C53') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C54') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C55') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C56') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C57') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C58') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C59') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C60') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C61') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C62') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C63') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C64') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C65') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C66') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C67') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C68') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C69') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C70') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C71') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C72') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C73') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C74') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C75') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C76') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C77') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C78') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C79') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C80') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C81') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C82') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C83') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C84') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C85') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C86') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C87') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C88') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C89') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C90') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C91') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C92') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C93') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C94') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C95') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C96') CIS=.TRUE.
                  IF(CIS) THEN
C     CONVERT CONFG(K,J)(10:32) TO REAL*8, ADD 1.0D0 TO IT,
C     IF IT IS IN THE CORRECT RANGE,
C     CONVERT IT BACK TO CHARACTER *23 AND STUFF IT BACK INTO
C     CONFG(K,J)(10:32)
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).GE.I) THEN
                          VL=VL+1.0D0
                          CALL DTOACV
                          CONFG(K,J)(10:32)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT C1 TO C96
                  END IF
              END DO
          END DO
C     ALL CONFIGS FIXED, RETURN
          RETURN
      END
      SUBROUTINE CFGFX2(I)
C
          IMPLICIT NONE
C
          INTEGER K,I,J,L,CNTER,LL
C
          LOGICAL CIS
C
          CHARACTER BLANK*140,AADF*20
C
          REAL*8 VL
C
          CHARACTER AVL*23
C
          COMMON/JK_ATD/AVL,VL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datcfg.inc'
C
          AADF='                    '
          BLANK=AADF//AADF//AADF//AADF//AADF//AADF//AADF
C
C       THIS IS SUBROUTINE CFGFX2. IT FIXES CONFIGS DATA
C     AFTER SURFACE DELETION OF SURFACE I (PART 1)
          IF(SYSTEM1(56).LE.1.0D0) RETURN
C     SINCE NO CONFIGS DATA EXISTED
C     SYSTEM1(56) NOT 1, PROCEED
          DO K=2,INT(SYSTEM1(56))
C     FIX CONFIG K
              DO J=1,CFGCNT(K)
                  IF(CONFG(K,J)(1:3).EQ.'CHG') THEN
C     CONVERT CONFG(K,J)(10:32) TO REAL*8
C     IF IT IS = I THEN BLANK ALL CONFIGS ENTRIES FROM HERE
C     TO BUT NOT INCLUDING AN EOS
C     CONFG(K,J)(10:32)
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).EQ.I) THEN
C     HERE IS THE BLANKING LOOP





C     BLANK THE CURRENT LINE AND ALL OTHER LINES UNTIL ANOTHER "CFG"
C     OR EOS IS ENCOUNTERED
                          CONFG(K,J)=BLANK
                          DO L=J+1,CFGCNT(K)
                              IF(CONFG(K,L).NE.'CHG'.AND.CONFG(K,L).NE.'EOS') THEN
                                  CONFG(K,L)=BLANK
                              ELSE
                                  GO TO 666
                              END IF
                          END DO
 666                      CONTINUE
                      ELSE
C     INT(VL) NOT EQUAL TO I, NOTHING TO DO
                      END IF
                  ELSE
C     NOT CHG
                  END IF
                  IF(CONFG(K,J)(1:7).EQ.'SPECIAL'.OR.
     1            CONFG(K,J)(1:4).EQ.'GENL') THEN
C     CONVERT CONFG(K,J)(10:32) TO REAL*8
C     IF IT IS = I, BLANK THE LINE
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).EQ.I) THEN
C     BLANK THE LINE
                          CONFG(K,J)=BLANK
                      ELSE
C     INT(VL) NOT = I
                      END IF
                  ELSE
C     NOT SPECIAL
                  END IF
                  IF(CONFG(K,J)(1:5).EQ.'SPDEL') THEN
C     CONVERT CONFG(K,J)(10:32) TO REAL*8
C     IF IT IS = I, BLANK THE LINE
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).EQ.I) THEN
C     BLANK THE LINE
                          CONFG(K,J)=BLANK
                      ELSE
C     INT(VL) NOT = I
                      END IF
                  ELSE
C     NOT SPDEL
                  END IF
                  IF(CONFG(K,J)(1:5).EQ.'PIKUP') THEN
C     CONVERT CONFG(K,J)(19:41) TO REAL*8
C     IF IT IS = I, BLANK THE LINE
                      AVL=CONFG(K,J)(19:41)
                      CALL ATODCV
                      IF(INT(VL).EQ.I) THEN
C     BLANK THE LINE
                          CONFG(K,J)=BLANK
                      ELSE
C     INT(VL) NOT = I
                      END IF
                  ELSE
C     NOT PIKUP
                  END IF
                  CIS=.FALSE.
                  IF(CONFG(K,J)(1:7).EQ.'MAGX'.OR.
     1            CONFG(K,J)(1:3).EQ.'MAGY') THEN
C     SPECIAL STUFF, TWO WORDS TO FIX
C     WORD #2
C     CONVERT CONFG(K,J)(34:56) TO REAL*8
C     IF IT IS = I, BLANK THE LINE
                      AVL=CONFG(K,J)(34:56)
                      CALL ATODCV
                      IF(INT(VL).EQ.I) THEN
C     BLANK THE LINE
                          CONFG(K,J)=BLANK
                      ELSE
C     INT(VL) NOT = I
                      END IF
C     WORD #3
C     CONVERT CONFG(K,J)(52:74) TO REAL*8
C     IF IT IS = I, BLANK THE LINE
                      AVL=CONFG(K,J)(52:74)
                      CALL ATODCV
                      IF(INT(VL).EQ.I) THEN
C     BLANK THE LINE
                          CONFG(K,J)=BLANK
                      ELSE
C     INT(VL) NOT = I
                      END IF
                  ELSE
C     NOT MAGX OR MAGY
                  END IF
                  CIS=.FALSE.
                  IF(CONFG(K,J)(1:2).EQ.'C1') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C2') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C3') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C4') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C5') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C6') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C7') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C8') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C9') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C10') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C11') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C12') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C13') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C14') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C15') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C16') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C17') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C18') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C19') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C10') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C21') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C22') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C23') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C24') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C25') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C26') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C27') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C28') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C29') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C30') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C31') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C32') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C33') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C34') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C35') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C36') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C37') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C38') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C39') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C40') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C41') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C42') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C43') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C44') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C45') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C46') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C47') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C48') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C49') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C50') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C51') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C52') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C53') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C54') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C55') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C56') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C57') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C58') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C59') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C60') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C61') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C62') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C63') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C64') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C65') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C66') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C67') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C68') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C69') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C70') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C71') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C72') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C73') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C74') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C75') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C76') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C77') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C78') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C79') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C80') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C81') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C82') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C83') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C84') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C85') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C86') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C87') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C88') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C89') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C90') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C91') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C92') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C93') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C94') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C95') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C96') CIS=.TRUE.
                  IF(CIS) THEN
C     CONVERT CONFG(K,J)(10:32) TO REAL*8
C     IF IT IS = I, BLANK THE LINE
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).EQ.I) THEN
C     BLANK THE LINE
                          CONFG(K,J)=BLANK
                      ELSE
C     INT(VL) NOT = I
                      END IF
                  ELSE
C     NOT C1 TO C96
                  END IF
              END DO
C     NOW REMOVE BLANK LINES
C     REMEMBER ORIGINAL LENGTH
              CNTER=CFGCNT(K)
 999          CONTINUE
C
              DO L=1,CFGCNT(K)
                  IF(CONFG(K,L).EQ.BLANK) THEN
C     FOUND A BLANK LINE AT L
                      DO LL=L,CNTER-1
                          CONFG(K,LL)=CONFG(K,LL+1)
                      END DO
                      CNTER=CNTER-1
                  ELSE
C     LINE WAS NOT BLANK, CONTINUE SEACHING
                  END IF
              END DO
              IF(CNTER.NE.CFGCNT(K)) THEN
C     A LINE WAS DELETED, KEEP GOING
                  CFGCNT(K)=CNTER
                  GO TO 999
              ELSE
C     CNTER=CFGCNT(K), DONE
              END IF
C
          END DO
C     ALL CONFIGS FIXED, RETURN
          RETURN
      END
      SUBROUTINE CFGFX3(I)
C
          IMPLICIT NONE
C
          INTEGER K,I,J
C
          LOGICAL CIS
C
          REAL*8 VL
C
          CHARACTER AVL*23
C
          COMMON/JK_ATD/AVL,VL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datcfg.inc'
C
C       THIS IS SUBROUTINE CFGFX3. IT FIXES CONFIGS DATA
C     AFTER SURFACE DELETION OF SURFACE I (PART 2)
          IF(SYSTEM1(56).LE.1.0D0) RETURN
C     SINCE NO CONFIGS DATA EXISTED
C     SYSTEM1(56) NOT 1, PROCEED
          DO K=2,INT(SYSTEM1(56))
C     FIX CONFIG K
              DO J=1,CFGCNT(K)
                  IF(CONFG(K,J)(1:3).EQ.'CHG') THEN
C     CONVERT CONFG(K,J)(10:32) TO REAL*8, MINUS 1.0D0 FROM IT,
C     IF IT IS GREATER THAN OF EQUAL TO I+1,
C     CONVERT IT BACK TO CHARACTER *23 AND STUFF IT BACK INTO
C     CONFG(K,J)(10:32)
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).GT.I) THEN
                          VL=VL-1.0D0
                          CALL DTOACV
                          CONFG(K,J)(10:32)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT CHG
                  END IF
                  IF(CONFG(K,J)(1:7).EQ.'SPECIAL'.OR.
     1            CONFG(K,J)(1:7).EQ.'GENL') THEN
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).GT.I) THEN
                          VL=VL-1.0D0
                          CALL DTOACV
                          CONFG(K,J)(10:32)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT SPECIAL
                  END IF
                  IF(CONFG(K,J)(1:5).EQ.'SPDEL') THEN
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).GT.I) THEN
                          VL=VL-1.0D0
                          CALL DTOACV
                          CONFG(K,J)(10:32)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT SPDEL
                  END IF
                  IF(CONFG(K,J)(1:5).EQ.'PIKUP') THEN
                      AVL=CONFG(K,J)(19:41)
                      CALL ATODCV
                      IF(INT(VL).GT.I) THEN
                          VL=VL-1.0D0
                          CALL DTOACV
                          CONFG(K,J)(19:41)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT PIKUP
                  END IF
                  CIS=.FALSE.
                  IF(CONFG(K,J)(1:7).EQ.'MAGX'.OR.
     1            CONFG(K,J)(1:3).EQ.'MAGY') THEN
C     SPECIAL STUFF, TWO WORDS TO FIX
C     WORD #2
C     CONVERT CONFG(K,J)(34:56) TO REAL*8, MINUS 1.0D0 FROM IT,
C     IF IT IS EQUAL TO OR GREATER THAN OR EQUAL TO,
C     CONVERT IT BACK TO CHARACTER *23 AND STUFF IT BACK INTO
C     CONFG(K,J)(10:32)
                      AVL=CONFG(K,J)(34:56)
                      CALL ATODCV
                      IF(INT(VL).GT.I) THEN
                          VL=VL-1.0D0
                          CALL DTOACV
                          CONFG(K,J)(34:56)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
C     WORD #3
C     CONVERT CONFG(K,J)(52:71) TO REAL*8, MINUS 1.0D0 FROM IT,
C     IF IT IS GREATER THAN OR EQUAL TO I+1,
C     CONVERT IT BACK TO CHARACTER *23 AND STUFF IT BACK INTO
C     CONFG(K,J)(52:71)
                      AVL=CONFG(K,J)(52:74)
                      CALL ATODCV
                      IF(INT(VL).GT.I) THEN
                          VL=VL-1.0D0
                          CALL DTOACV
                          CONFG(K,J)(52:74)=AVL(1:23)
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT MAGX OR MAGY
                  END IF
                  CIS=.FALSE.
                  IF(CONFG(K,J)(1:2).EQ.'C1') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C2') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C3') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C4') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C5') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C6') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C7') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C8') CIS=.TRUE.
                  IF(CONFG(K,J)(1:2).EQ.'C9') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C10') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C11') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C12') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C13') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C14') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C15') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C16') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C17') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C18') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C19') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C10') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C21') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C22') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C23') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C24') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C25') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C26') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C27') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C28') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C29') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C30') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C31') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C32') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C33') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C34') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C35') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C36') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C37') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C38') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C39') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C40') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C41') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C42') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C43') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C44') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C45') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C46') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C47') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C48') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C49') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C50') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C51') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C52') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C53') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C54') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C55') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C56') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C57') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C58') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C59') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C60') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C61') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C62') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C63') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C64') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C65') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C66') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C67') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C68') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C69') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C70') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C71') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C72') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C73') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C74') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C75') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C76') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C77') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C78') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C79') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C80') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C81') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C82') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C83') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C84') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C85') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C86') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C87') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C88') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C89') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C90') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C91') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C92') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C93') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C94') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C95') CIS=.TRUE.
                  IF(CONFG(K,J)(1:3).EQ.'C96') CIS=.TRUE.
                  IF(CIS) THEN
                      AVL=CONFG(K,J)(10:32)
                      CALL ATODCV
                      IF(INT(VL).GT.I) THEN
                          VL=VL-1.0D0
                          CALL DTOACV
                          CONFG(K,J)(10:32)=AVL
                      ELSE
C     NOT IN CORRECT RANGE
                      END IF
                  ELSE
C     NOT C1 TO C96
                  END IF
              END DO
          END DO
C     ALL CONFIGS FIXED, RETURN
          RETURN
      END
C SUB CFGCLN.FOR
      SUBROUTINE CFGCLN
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE CLEANS UP AND STREAMLINES
C       THE CONFIGURATION DATA STORAGE MAKING CERTAIN
C       THAT THE FILE IS AS ECONOMICAL AS POSSIBLE.
C       IT RE-ORDERS THE STORAGE SO THAT CMD LEVEL
C       LENS CHANGES COME LAST FOLLOWING  SPECIAL SURFACE
C       CHANGES AND UPDATE LENS CHANGES.
C
          CHARACTER
     1    BLANK*140,HOLD2*140
C
          COMMON/BLAAA/BLANK
C
          INTEGER ULTEST,ALLOERR,
     6    SCRCNT,SPTEST,IL,I,J,K,JJ,JJJ,JK,
     7    UL,SP,UL1,SP1,UUL,IEND,STOP,REF,CFCN,TESTV1
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
C
          CHARACTER*140 SCRATH
          DIMENSION SCRATH(:)
          ALLOCATABLE :: SCRATH
          INTEGER NANA
          NANA=2000
          DEALLOCATE(SCRATH,STAT=ALLOERR)
          ALLOCATE(SCRATH(NANA),STAT=ALLOERR)
C
          BLANK=AA//AA//AA//AA//AA//AA//AA
          DO IL=1,2000
              SCRATH(IL)=BLANK
          END DO
C       IEND IS THE LAST NON-BLANK CONFIG
          IEND=INT(SYSTEM1(56))
C
C       BLANK OUT ALL BUT THE LAST ASTOP AND REFS IN THE CONFIG
C
C
          DO 13 I=2,IEND
              CFCN=CFGCNT(I)
              STOP=0
              DO 11 J=CFGCNT(I),1,-1
                  EE12=CONFG(I,J)
                  HOLDER=EE12
                  IF((HOLDER(1:5)).EQ.'ASTOP') THEN
                      IF(STOP.EQ.0) THEN
C       SET STOP TO 1 AND PROCEED
                          STOP=1
                          GO TO 11
                      END IF
                      IF(STOP.EQ.1) THEN
                          DO 14 K=J,(CFGCNT(I)-1)
                              EE12=CONFG(I,(K+1))
                              HOLDER=EE12
                              CONFG(I,K)=HOLDER(1:140)
 14                       CONTINUE
                          CFCN=CFCN-1
                      END IF
                  END IF
 11           CONTINUE
              REF=0
              DO 12 J=CFGCNT(I),1,-1
                  EE12=CONFG(I,J)
                  HOLDER=EE12
                  IF(HOLDER(1:4).EQ.'REFS') THEN
                      IF(REF.EQ.0) THEN
C       SET REF TO 1 AND PROCEED
                          REF=1
                          GO TO 12
                      END IF
                      IF(REF.EQ.1) THEN
                          DO 16 K=J,(CFGCNT(I)-1)
                              EE12=CONFG(I,(K+1))
                              HOLDER=EE12
                              CONFG(I,K)=HOLDER(1:140)
 16                       CONTINUE
                          CFCN=CFCN-1
                          CONFG(I,J)=BLANK(1:140)
                      END IF
                  END IF
 12           CONTINUE
              CFGCNT(I)=CFCN
 13       CONTINUE
C
C
C       CHECK EACH CONFIG FOR WHICH THERE IS CONFIG DATA
C
          DO 10 I=2,IEND
              ULTEST=0
              SPTEST=0
              UL=0
              SP=0
C       COUNT THE NUMBER OF U L AND U SP IN THIS CONFIG
              DO 80 K=1,CFGCNT(I)
                  EE12=CONFG(I,K)
                  HOLDER=EE12
                  IF((HOLDER(1:10)).EQ.'U        L'.OR.
     1            (HOLDER(1:13)).EQ.'UPDATE   LENS'.OR.
     2            (HOLDER(1:10)).EQ.'UPDATE   L'.OR.
     3            (HOLDER(1:13)).EQ.'U        LENS') UL=UL+1
                  IF((HOLDER(1:11)).EQ.'U        SP'.OR.
     1            (HOLDER(1:14)).EQ.'UPDATE   SPSRF'.OR.
     2            (HOLDER(1:11)).EQ.'UPDATE  SP'.OR.
     3            (HOLDER(1:14)).EQ.'U        SPSRF') SP=SP+1
 80           CONTINUE
C
C       INITIALIZE THE SCRATCH AREA FOR CLEANUP PROCESSING
              IF(CFGCNT(I).NE.0) THEN
C       THERE IS CONFIG DATA TO CHECK, DO IT HERE
C                       THE CLEANUP PROCESS
                  SCRCNT=0
C       SCRATCH MEMORY INITIALIZED
C
C       IS THERE UPDATE SPSRF DATA TO BE PUT INTO THE SCRATCH
C       ARRAY?
C       IF SP NOT 0, THEN THERE MAY BE
C
C       SP NON-ZERO MEANS THERE ARE SP U SP/EOS PAIRS TO PROCESS
C       SP1 SET TO ZERO MEANS WE ARE IN AN U SP/EOS PAIR.
C
C       ANY COMMANDS WHICH LIE BETWEEN U SP OR UPDATE SPSRF
C       AND ANY FOLLOWING EOS SHOULD BE WRITTEN BETWEEN ONE
C       U SP/EOS PAIR IN THE SCRATH ARRAY
                  IF(SP.EQ.0) GO TO 32
                  SP1=1
                  DO 30 J=1,CFGCNT(I)
                      EE12=CONFG(I,J)
                      HOLDER=EE12
                      IF(HOLDER.EQ.BLANK) GO TO 30
C       SCAN THE CONFIG DATA, LOOK FOR THE FIRST
C       U SP OR UPDATE SPSRF
                      IF(HOLDER(1:11).EQ.'U        SP'.OR.
     2                HOLDER(1:14).EQ.'UPDATE   SPSRF'.OR.
     3                HOLDER(1:11).EQ.'UPDATE   SP'.OR.
     4                HOLDER(1:14).EQ.'U        SPSRF') THEN
                          SP1=0
C       IF THE NEXT ENTRY IS EOS, MAYBE THROW OUT THE PAIR
C       UNLESS THE EOS REPRESENTS THE LAST EOS
                          EE12=CONFG(I,(J+1))
                          HOLDER=EE12
                          IF((HOLDER(1:3)).EQ.'EOS') THEN
C       DECREMENT SP BY 1
                              SP=SP-1
                              IF(SP.EQ.0) THEN
C       JUST THROW OUT THE U SP, NOT THE EOS
C       FOUND LAST 'EOS'
                                  SCRCNT=SCRCNT+1
                                  SCRATH(SCRCNT)=HOLDER
                                  CONFG(I,(J+1))=BLANK(1:140)
C       JUMP OUT OF LOOP
                                  GO TO 31
                              ELSE
C       SP NOT 0, DUMP THE PAIR.
                                  CONFG(I,J)=BLANK(1:140)
                                  CONFG(I,(J+1))=BLANK(1:140)
                                  SP1=1
                                  GO TO 30
                              END IF
                          ELSE
                              SP1=0
C       DONT THROW OUT THE PAIR, NEXT ENTRY WAS
C       NOT 'EOS' PROCEED TO PROCESS
                          END IF
C       CHECK IF U SP WAS FIRST ONE, IF SO WRITE IT.
                          IF(SPTEST.EQ.0) THEN
C       WRITE THE U SP TO THE FILE AND SET SPTEST=1
                              SCRCNT=SCRCNT+1
                              EE12=CONFG(I,J)
                              HOLDER=EE12
                              SCRATH(SCRCNT)=HOLDER
                              SPTEST=1
C       DECREMENT THE U SP COUNTER
                              SP=SP-1
C
C       NOW PROCEED TO CHECK THE NEXT CONFIG ENTRY
                              GO TO 30
                          ELSE
C       SPTEST=1, DON'T WRITE THE U SP, IT IS NOT THE FIRST ONE.
C       DECREMENT THE U SP COUNTER
                              SP=SP-1
                              CONFG(I,J)=BLANK(1:140)
C       NOW PROCEED TO CHECK THE NEXT CONFIG ENTRY
                          END IF
                          GO TO 30
                      ELSE
C       THE CURRENT ENTRY IS NOT U SP. IF SP1=0, PROCEED TO PROCESS
C       IF SP1=1, GO TO 30 AND LOOK FOR U SP
                          IF(SP1.EQ.1) GO TO 30
C       SP1 NOT 1
C       THE CONFIG ENTRY IS NOT U SP, PROCEED WITH SEARCH
C       THE CURRENT ENTRY IS EITHER NOT AN U SP COMMAND OR IT IS
C       IF SPTEST=1, IT IS AN SPSRF COMMAND, CHECK IT AND PROCESS IT.
C       ALSO SEE IF THE COMMAND FOLLOWING IT IS AN 'EOS' AND HANDEL IT.
                          IF(SPTEST.EQ.1) THEN
C       THE CURRENT ENTRY IS AN U SP COMMAND, WRITE IT
                              SCRCNT=SCRCNT+1
                              EE12=CONFG(I,J)
                              HOLDER=EE12
                              SCRATH(SCRCNT)=HOLDER
                              CONFG(I,J)=BLANK(1:140)
C       IS THE COMMAND FOLLOWED BY AN 'EOS'
                              EE12=CONFG(I,(J+1))
                              HOLDER=EE12
                              IF(HOLDER(1:3).EQ.'EOS') THEN
C       NEXT COMMAND IS 'EOS'. IF SP=0, IT IS THE FINAL EOS,
C       IF SP NOT 0, SKIP IT, BLANK IT AND PROCEED
                                  IF(SP.EQ.0) THEN
C       FOUND LAST 'EOS'
                                      SCRCNT=SCRCNT+1
                                      SCRATH(SCRCNT)=HOLDER
                                      CONFG(I,(J+1))=BLANK(1:140)
C       JUMP OUT OF LOOP
                                      GO TO 31
                                  ELSE
C       SP NOT 0
C       THIS WAS NOT THE LAST EOS, SEARCH FOR ANOTHER U SP
                                      SP1=1
                                      CONFG(I,(J+1))=BLANK(1:140)
C
                                  END IF
C       BLANK EOS AND PROCEED WITH SEARCH
                                  GO TO 30
                              ELSE
C       NEXT COMMAND WAS NOT 'EOS', PROCEED
                              END IF
                              GO TO 30
                          ELSE
C       THE CURRENT COMMAND WAS NOT AN U SP COMMAND
C       AND IT OCCURED WITH SP1=0
                              OUTLYNE='SERIOUS ERROR IN CFGCLN.FOR (1)'
                              CALL SHOWIT(1)
                              DEALLOCATE(SCRATH,STAT=ALLOERR)
                              RETURN
                          END IF
                      END IF
C
 30               CONTINUE
 31               CONTINUE
 32               CONTINUE
C
C       NOW DO THE SAME THING FOR U L/EOS
C
C       IS THERE UPDATE LENS DATA TO BE PUT INTO THE SCRATCH
C       ARRAY?
C       ANY COMMANDS WHICH LIE BETWEEN U L OR UPDATE LENS
C       AND ANY FOLLOWING EOS SHOULD BE WRITTEN BETWEEN ONE
C       U L/EOS PAIR IN THE SCRATH ARRAY
C
C       AVOID THE SEARCH
                  IF(UL.EQ.0) GO TO 50
                  UL1=1
                  DO 40 J=1,CFGCNT(I)
                      EE12=CONFG(I,J)
C
                      HOLDER=EE12
                      IF(HOLDER.EQ.BLANK) GO TO 40
C       SCAN THE CONFIG DATA, LOOK FOR THE FIRST
C       U L OR UPDATE LENS
                      IF(HOLDER(1:10).EQ.'U        L'.OR.
     2                HOLDER(1:13).EQ.'UPDATE   LENS'.OR.
     3                HOLDER(1:10).EQ.'UPDATE   L'.OR.
     4                HOLDER(1:13).EQ.'U        LENS') THEN
C       IF THE NEXT ENTRY IS EOS, THROW OUT THE PAIR
C       OR IF NEXT TWO COMMANDS ARE CHG AND EOS THROW OUT PAIR
                          EE12=CONFG(I,(J+1))
                          HOLDER=EE12
                          IF((HOLDER(1:3)).EQ.'EOS') THEN
C       THROW OUT THE PAIR AND DECREMENT UL BY 1
                              UL=UL-1
                              CONFG(I,J)=BLANK(1:140)
                              CONFG(I,(J+1))=BLANK(1:140)
C       WAS THAT THE END OF THE CONFIG FILE?
C       IF SO ADD AN EOS TO THE END OF SCATCH FILE
                              IF((J+1).EQ.CFGCNT(I)) THEN
                                  SCRCNT=SCRCNT+1
                                  SCRATH(SCRCNT)='EOS'
                              ELSE
C       NOT END
                              END IF
                              GO TO 40
                          END IF
                          EE12=CONFG(I,(J+1))
                          HOLDER=EE12
                          EE12=CONFG(I,(J+2))
                          HOLD2=EE12
                          IF((HOLDER(1:3)).EQ.'CHG'.AND.
     1                    (HOLD2(1:3)).EQ.'EOS') THEN
C       THROW OUT THE THREE AND DECREMENT UL BY 1
                              UL=UL-1
                              CONFG(I,J)=BLANK(1:140)
                              CONFG(I,(J+1))=BLANK(1:140)
                              CONFG(I,(J+2))=BLANK(1:140)
C       WAS THAT THE END OF THE CONFIG FILE?
C       IF SO ADD AN EOS TO THE END OF SCATCH FILE
                              IF((J+2).EQ.CFGCNT(I)) THEN
                                  SCRCNT=SCRCNT+1
                                  SCRATH(SCRCNT)='EOS'
                              ELSE
C       NOT END
                              END IF
                              GO TO 40
                          ELSE
C       FOUND AN U L NOT FOLLOWED BY EOS
C       OR CHG AND EOS, SET UL1=0
                              UL1=0
C       DONT THROW OUT, NEXT ENTRY WAS
C       NOT 'EOS'  OR 'CHG' AND 'EOS' PROCEED TO PROCESS
                          END IF
C       CHECK IF U L WAS FIRST ONE, IF SO WRITE IT.
                          IF(UL.NE.0) THEN
                              IF(ULTEST.EQ.0) THEN
C       WRITE THE U L TO THE FILE AND SET ULTEST=1
                                  SCRCNT=SCRCNT+1
                                  EE12=CONFG(I,J)
                                  HOLDER=EE12
                                  SCRATH(SCRCNT)=HOLDER
                                  ULTEST=1
C NOW LOAD IN A DEFAULT CHG,0
                                  EE12=CONFG(I,(J+1))
                                  HOLDER=EE12
                                  IF((HOLDER(1:29)).NE.
     1                            'CHG     ,  0.000000000000D+00')THEN
                                      SCRCNT=SCRCNT+1
                                      SCRATH(SCRCNT)='CHG     ,  0.000000000000D+00,,,,,'
                                  ELSE
C       DON'T ADD CHG,0
                                  END IF
C                       ULTEST=1
C       DECREMENT THE U L COUNTER
                                  UL=UL-1
C       NOW PROCEED TO CHECK THE NEXT CONFIG ENTRY
                                  GO TO 40
                              ELSE
C       ULTEST=1, DON'T WRITE THE U L, IT IS NOT THE FIRST ONE.
C       DECREMENT THE U L COUNTER
                                  UL=UL-1
                                  CONFG(I,J)=BLANK(1:140)
C       NOW PROCEED TO CHECK THE NEXT CONFIG ENTRY
                              END IF
                          END IF
                          GO TO 40
                      ELSE
C       THE CURRENT ENTRY IS NOT U L. IF UL1=0, PROCEED TO PROCESS
C       IF UL1=1, GO TO 40 AND LOOK FOR U L
                          IF(UL1.EQ.1) GO TO 40
C       UL1 NOT 1
C       THE CONFIG  ENTRY IS NOT U L, PROCEED WITH SEARCH
C       THE CURRENT ENTRY IS EITHER NOT AN U L COMMAND OR IT IS
C       IF ULTEST=1, IT IS AN LENS COMMAND, CHECK IT AND PROCESS IT.
C       ALSO SEE IF THE COMMAND FOLLOWING IT IS AN 'EOS' AND HANDEL IT.
                          IF(ULTEST.EQ.1) THEN
C       THE CURRENT ENTRY IS AN U L COMMAND, WRITE IT
                              SCRCNT=SCRCNT+1
                              EE12=CONFG(I,J)
                              HOLDER=EE12
                              SCRATH(SCRCNT)=HOLDER
                              CONFG(I,J)=BLANK(1:140)
C       IS THE COMMAND FOLLOWED BY AN 'EOS'
                              EE12=CONFG(I,(J+1))
                              HOLDER=EE12
                              IF((HOLDER(1:3)).EQ.'EOS') THEN
C       NEXT COMMAND IS 'EOS'. IF UL=0, IT IS THE FINAL EOS,
C       IF UL NOT 0, SKIP IT, BLANK IT AND PROCEED
                                  IF(UL.EQ.0) THEN
C       FOUND LAST 'EOS'
                                      SCRCNT=SCRCNT+1
                                      SCRATH(SCRCNT)=HOLDER
                                      CONFG(I,(J+1))=BLANK(1:140)
C       JUMP OUT OF LOOP
                                      GO TO 41
                                  ELSE
C       UL NOT 0
C       THIS WAS NOT THE LAST EOS, SEARCH FOR ANOTHER U L
                                      UL1=1
                                      CONFG(I,(J+1))=BLANK(1:140)
C
                                  END IF
C       BLANK EOS AND PROCEED WITH SEARCH
                                  GO TO 40
                              ELSE
C       NEXT COMMAND WAS NOT 'EOS', PROCEED
                              END IF
                              GO TO 40
                          ELSE
C       THE CURRENT COMMAND WAS NOT AN U L COMMAND
C       AND IT OCCURED WITH UL1=0
                              OUTLYNE= 'SERIOUS ERROR IN CFGCLN.FOR (2)'
                              CALL SHOWIT(1)
                              DEALLOCATE(SCRATH,STAT=ALLOERR)
                              RETURN
                          END IF
                      END IF
C
C
 40               CONTINUE
 41               CONTINUE
C       CODE WHICH ADDED A BLANK U L/EOS PAIR WAS REMOVED
C       FROM HERE ON 9/12/88 AND IS NOT NEEDED ANYMORE.
C
 50               CONTINUE
C
C***************************************************************
C       START CHECKING CONFIGS FOR DATA
C       CHECK FOR NON- U L OR U SP COMMANDS.
C       IF FOUND WRITE THEM TO SCRATH(1:2000) AND INCREMENT
C       SCRCNT, THE SCRATCH COUNTER.
C       THE COMMANDS WE ARE LOOKING FOR ARE:
C       MODE (FOCAL,AFOCAL,UFOCAL OR UAFOCAL)
C       FNBY AND FNBX
C       ERY AND ERX
C       MAGY AND MAGX
C       SPTWT AND SPTWT2. (ALSO DO SPSRF ON AND SPSRF OFF WITH
C       SURFACE NUMBER ENTRIES)
C       START CHECKING FROM END OF DATA LIST
                  DO 21 JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:16).EQ.'MODE     FOCAL  '.OR.
     1                HOLDER(1:16).EQ.'MODE     AFOCAL '.OR.
     2                HOLDER(1:16).EQ.'MODE     UAFOCAL'.OR.
     3                HOLDER(1:16).EQ.'MODE     UFOCAL ') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
C       NOW IF THERE ARE ANY MODE COMMANDS BETWEEN THIS MODE
C       COMMAND AND THE START OF THE CONFIG DATA, REMOVE THEM
                          DO 22 JJJ=(JJ-1),1,-1
                              EE12=CONFG(I,JJJ)
                              HOLDER=EE12
                              IF(HOLDER(1:16).EQ.'MODE     FOCAL  '.OR.
     1                        HOLDER(1:16).EQ.'MODE     AFOCAL '.OR.
     2                        HOLDER(1:16).EQ.'MODE     UAFOCAL'.OR.
     3                        HOLDER(1:16).EQ.'MODE     UFOCAL ') THEN
                                  CONFG(I,JJJ)=BLANK(1:140)
                              ELSE
C       PROCEED WITH CHECKING
                              END IF
 22                       CONTINUE
                      ELSE
C       FIRST OCCURENCE OF MODE NOT FOUND, CHECK NEXT ENTRY
                      END IF
 21               CONTINUE
C
C       NOW ALL MODE COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       NOW CHECK 'MAGY' OR 'MAGX'
C
C       START CHECKING FROM END OF DATA LIST
                  DO 23 JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:4).EQ.'MAGY'.OR.
     1                HOLDER(1:4).EQ.'MAGX') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
C       NOW IF THERE ARE ANY MAGY OR MAGX
C       COMMANDS BETWEEN THIS MAGX OR MAGY
C       COMMAND AND THE START OF THE CONFIG DATA, REMOVE THEM
                          DO 24 JJJ=(JJ-1),1,-1
                              EE12=CONFG(I,JJJ)
                              HOLDER=EE12
                              IF(HOLDER.EQ.'MAGY'.OR.
     1                        HOLDER(1:4).EQ.'MAGX') THEN
                                  CONFG(I,JJJ)=BLANK(1:140)
                              ELSE
C       PROCEED WITH CHECKING
                              END IF
 24                       CONTINUE
                      ELSE
C       FIRST OCCURENCE OF MAGY OR MAGX NOT FOUND, CHECK NEXT ENTRY
                      END IF
 23               CONTINUE
C       NOW ALL MAGY/MAGX COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       NOW CHECK 'FNBY' OR 'ERY'. ALL FNBY AND ERY COMMANDS
C       TAKE PRECIDENCE OVER ALL OTHERS, ONLY ONE CAN BE
C       ALLOWED TO REMAIN!
C
C       START CHECKING FROM END OF DATA LIST
                  DO 25 JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:4).EQ.'FNBY'.OR.
     1                HOLDER(1:3).EQ.'ERY') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
C       NOW IF THERE ARE ANY FNBY OR ERY
C       COMMANDS BETWEEN THIS FNBY OR ERY
C       COMMAND AND THE START OF THE CONFIG DATA, REMOVE THEM
                          DO 26 JJJ=(JJ-1),1,-1
                              EE12=CONFG(I,JJJ)
                              HOLDER=EE12
                              IF(HOLDER(1:4).EQ.'FNBY'.OR.
     1                        HOLDER(1:3).EQ.'ERY') THEN
                                  CONFG(I,JJJ)=BLANK(1:140)
                              ELSE
C       PROCEED WITH CHECKING
                              END IF
 26                       CONTINUE
                      ELSE
C       FIRST OCCURENCE OF FNBY OR ERY NOT FOUND, CHECK NEXT ENTRY
                      END IF
 25               CONTINUE
C       NOW ALL FNBY/ERY COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       NOW CHECK 'FNBX' OR 'ERX'. ALL FNBX AND ERX COMMANDS
C       TAKE PRECIDENCE OVER ALL OTHERS, ONLY ONE CAN BE
C       ALLOWED TO REMAIN!
C
C       START CHECKING FROM END OF DATA LIST
                  DO 27 JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:4).EQ.'FNBX'.OR.
     1                HOLDER(1:3).EQ.'ERX') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
C       NOW IF THERE ARE ANY FNBX OR ERX
C       COMMANDS BETWEEN THIS FNBX OR ERX
C       COMMAND AND THE START OF THE CONFIG DATA, REMOVE THEM
                          DO 28 JJJ=(JJ-1),1,-1
                              EE12=CONFG(I,JJJ)
                              HOLDER=EE12
                              IF(HOLDER(1:4).EQ.'FNBX'.OR.
     1                        HOLDER(1:3).EQ.'ERX') THEN
                                  CONFG(I,JJJ)=BLANK(1:140)
                              ELSE
C       PROCEED WITH CHECKING
                              END IF
 28                       CONTINUE
                      ELSE
C       FIRST OCCURENCE OF FNBX OR ERX NOT FOUND, CHECK NEXT ENTRY
                      END IF
 27               CONTINUE
C       NOW SPTWT
C       NOW CHECK 'SPTWT'
C
C       START CHECKING FROM END OF DATA LIST
                  DO 231 JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:5).EQ.'SPTWT') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
C       NOW IF THERE ARE ANY SPTWT
C       COMMANDS BETWEEN THIS SPTWT
C       COMMAND AND THE START OF THE CONFIG DATA, REMOVE THEM
                          DO 241 JJJ=(JJ-1),1,-1
                              EE12=CONFG(I,JJJ)
                              HOLDER=EE12
                              IF(HOLDER(1:5).EQ.'SPTWT') THEN
                                  CONFG(I,JJJ)=BLANK(1:140)
                              ELSE
C       PROCEED WITH CHECKING
                              END IF
 241                      CONTINUE
                      ELSE
C       FIRST OCCURENCE OF SPTWT NOT FOUND, CHECK NEXT ENTRY
                      END IF
 231              CONTINUE
C       NOW ALL SPTWT COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       SPTWT DONE
C       NOW SPTWT2
C       NOW CHECK 'SPTWT2'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:6).EQ.'SPTWT2') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
C       NOW IF THERE ARE ANY SPTWT2
C       COMMANDS BETWEEN THIS SPTWT2
C       COMMAND AND THE START OF THE CONFIG DATA, REMOVE THEM
                          DO JJJ=(JJ-1),1,-1
                              EE12=CONFG(I,JJJ)
                              HOLDER=EE12
                              IF(HOLDER(1:6).EQ.'SPTWT2') THEN
                                  CONFG(I,JJJ)=BLANK(1:140)
                              ELSE
C       PROCEED WITH CHECKING
                              END IF
                          END DO
                      ELSE
C       FIRST OCCURENCE OF SPTWT2 NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL SPTWT2 COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       SPTWT2 DONE
C       NOW THERM SHAPE
C       NOW CHECK 'THERM SHAPE'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:14).EQ.'THERM    SHAPE') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM SHAPE NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM SHAPE COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM SHAPE DONE
C       NOW THERM THICK
C       NOW CHECK 'THERM THICK'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:14).EQ.'THERM    THICK') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM THICK NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM THICK COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM THICK DONE
C       NOW THERM SPACE
C       NOW CHECK 'THERM SPACE'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:14).EQ.'THERM    SPACE') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM SPACE NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM SPACE COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM SPACE DONE
C       NOW THERM GLASS
C       NOW CHECK 'THERM GLASS'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:14).EQ.'THERM    GLASS') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM GLASS NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM GLASS COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM GLASS DONE
C
C       NOW THERM GAS
C       NOW CHECK 'THERM GAS'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:12).EQ.'THERM    GAS') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM GAS NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM GAS COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM GAS DONE
C
C       NOW THERM AIR
C       NOW CHECK 'THERM AIR'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:12).EQ.'THERM    AIR') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM AIR NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM AIR COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM AIR DONE
C
C       NOW THERM OXYGEN
C       NOW CHECK 'THERM OXYGEN'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:15).EQ.'THERM    OXYGEN') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM OXYGEN NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM OXYGEN COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM OXYGEN DONE
C
C       NOW THERM HYDROGEN
C       NOW CHECK 'THERM HYDROGEN'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      IF(HOLDER(1:17).EQ.'THERM    HYDROGEN') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM HYDROGEN NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM HYDROGEN COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM HYDROGEN DONE
C
C       NOW THERM NITROGEN
C       NOW CHECK 'THERM NITROGEN'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:17).EQ.'THERM    NITROGEN') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM NITROGEN NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM NITROGEN COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM NITROGEN DONE
C
C       NOW THERM ARGON
C       NOW CHECK 'THERM ARGON'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:14).EQ.'THERM    ARGON') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM ARGON NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM ARGON COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM ARGON DONE
C
C       NOW THERM METHANE
C       NOW CHECK 'THERM METHANE'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:16).EQ.'THERM    METHANE') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM METHANE NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM METHANE COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM METHANE DONE
C
C       NOW THERM ETHANE
C       NOW CHECK 'THERM ETHANE'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:15).EQ.'THERM    ETHANE') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF THERM ETHANE NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL THERM ETHANE COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       THERM ETHANE DONE
C
C       NOW PRES GAS
C       NOW CHECK 'PRES GAS'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:12).EQ.'PRES     GAS') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF PRES GAS NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL PRES GAS COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       PRES GAS DONE
C
C       NOW PRES AIR
C       NOW CHECK 'PRES AIR'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:12).EQ.'PRES     AIR') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF PRES AIR NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL PRES AIR COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       PRES AIR DONE
C
C       NOW PRES OXYGEN
C       NOW CHECK 'PRES OXYGEN'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:15).EQ.'PRES     OXYGEN') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF PRES OXYGEN NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL PRES OXYGEN COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       PRES OXYGEN DONE
C
C       NOW PRES HYDROGEN
C       NOW CHECK 'PRES HYDROGEN'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:17).EQ.'PRES     HYDROGEN') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF PRES HYDROGEN NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL PRES HYDROGEN COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       PRES HYDROGEN DONE
C
C       NOW PRES NITROGEN
C       NOW CHECK 'PRES NITROGEN'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:17).EQ.'PRES     NITROGEN') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF PRES NITROGEN NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL PRES NITROGEN COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       PRES NITROGEN DONE
C
C       NOW PRES ARGON
C       NOW CHECK 'PRES ARGON'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:14).EQ.'PRES     ARGON') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF PRES ARGON NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL PRES ARGON COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       PRES ARGON DONE
C
C       NOW PRES METHANE
C       NOW CHECK 'PRES METHANE'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:16).EQ.'PRES     METHANE') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF PRES METHANE NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL PRES METHANE COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       PRES METHANE DONE
C
C       NOW PRES ETHANE
C       NOW CHECK 'PRES ETHANE'
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:15).EQ.'PRES     ETHANE') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE OF PRES ETHANE NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       NOW ALL PRES ETHANE COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       PRES ETHANE DONE
C
C       NOW FLDS MAX
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:12).EQ.'FLDS     MAX') THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       FLDS MAX DONE
C
C       NOW FLDS (NO QUAL)
C
C       START CHECKING FROM END OF DATA LIST
                  DO JJ=CFGCNT(I),1,-1
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:4).EQ.'FLDS'.AND.HOLDER(1:12).NE.'FLDS     MAX'
     1                ) THEN
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
                      ELSE
C       FIRST OCCURENCE NOT FOUND, CHECK NEXT ENTRY
                      END IF
                  END DO
C       FLDS (NO QUAL) DONE
C
C       NOW SPSRF (ON OR OFF)
C       NOW CHECK 'SPSRF'
C
C       START CHECKING FROM END OF DATA LIST FOR EACH VALID
C       SURFACE NUMBER FROM 1 TO INT(STSEM(20)
                  DO 1131 JK=1,INT(SYSTEM1(20))
C
                      DO 1231 JJ=CFGCNT(I),1,-1
                          EE12=CONFG(I,JJ)
                          HOLDER=EE12
                          IF(HOLDER(1:5).EQ.'SPSRF') THEN
C       CONVERT NUMERIC ENTRY TO INTEGER AND CONPARE WITH
C       VALUE OF JK. IF NO MATCH, GO TO 1131
                              CALL CONVR1(HOLDER(1:140),TESTV1)
                              IF(TESTV1.NE.JK) GO TO 1231
C       TESTV1=JK, PROCEED.
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                              SCRCNT=SCRCNT+1
                              SCRATH(SCRCNT)=HOLDER
                              CONFG(I,JJ)=BLANK(1:140)
                              GO TO 1131
                          ELSE
C       FIRST OCCURENCE OF SPSRF NOT FOUND, CHECK NEXT ENTRY
                          END IF
 1231                 CONTINUE
C
C       NOW DECREMENT SURFACE COUNTER AND CHECK NEXT SURFACE NUMBER
 1131             CONTINUE
C       NOW ALL SPSRF ARE RE-WRITTEN TO SCRATH ARRAY
C       SPSRF DONE
C
C       ALL CMD LEVEL COMMANDS WHICH WERE DOMINANT, NOW LIE IN THE
C       LAST SPACES OF THE SCRATH ARRAY
C
C       NOW WRITE THE SCRATH ARRAY INTO THE CONFIG ARRAY
                  CONFG(I,1:CFGCNT(I))=BLANK(1:140)
                  CFGCNT(I)=SCRCNT
                  CONFG(I,1:CFGCNT(I))=SCRATH(1:CFGCNT(I))(1:140)
                  UUL=0
                  DO 1212 J=1,CFGCNT(I)
                      EE12=CONFG(I,J)
                      HOLDER=EE12
                      IF((HOLDER(1:10)).EQ.'U        L'.OR.
     1                (HOLDER(1:13)).EQ.'UPDATE   LENS'.OR.
     2                (HOLDER(1:10)).EQ.'UPDATE   L'.OR.
     3                (HOLDER(1:13)).EQ.'U        LENS') THEN
                          UUL=1
                          GO TO 1222
                      END IF
 1212             CONTINUE
 1222             CONTINUE
C       NOW REORDER THE SPSRF DATA USING A CALL TO SORDER.FOR
                  CALL SORDER(I)
                  IF(UUL.EQ.1) THEN
C       NOW REORDER THE LENS DATA USING A CALL TO
C                       LORDER.FOR
                      CALL LORDER(I)
                  ELSE
C       NO CALL TO LORDER
                  END IF
              ELSE
C       NO CONFIG DATA FOR CONFIG I
              END IF
C
 10       CONTINUE
          DEALLOCATE(SCRATH,STAT=ALLOERR)
          RETURN
C
      END
C SUB CFGIN.FOR
      SUBROUTINE CFGIN
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO CREATE THE
C       CONFIGURATION DATA USING THE COMMAND
C       (CONFIGS) FROM THE CMD LEVEL
C
          INTEGER I,J
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       CONFIGS TAKES NO INPUT
          IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE=
     1        '"CONFIGS" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       INITIALIZE REFERENCE RAY LOGICAL
          IF(RAYCLEAR) THEN
              FOBYES=.FALSE.
              REFEXT=.FALSE.
              NULL=.FALSE.
              RAYEXT=.FALSE.
              POLEXT=.FALSE.
              SPDEXT=.FALSE.
              GSPDEXT=.FALSE.
              FAIL=.TRUE.
          END IF

C       INITIALIZE SYSTEM1(50) AND SYSP(50) AND SYTEM(56) AND SYSP(56)
          SYSTEM1(50)=1.0D0
          SYSP(50)=1.0D0
          SYSTEM1(56)=1.0D0
          SYSP(56)=1.0D0
C
C       INITIALIZE THE ARRAY CONFIGS
C       INITIALIZE THE ARRAY CONFIGS
C                      DO I=2,MAXCFG
          CFGCNT(2:MAXCFG)=0
C                      END DO
C     INITIALIZE THE ARRAY THAT KEEPS TRACK OF SPECIAL SURFACES
          DO J=2,MAXCFG
              I=INT(SYSTEM1(20))
              SPECFF(J,0:I)=0
              SPECF2(J,0:I)=INT(DABS(ALENS(34,0:I)))
          END DO
C                       SET F1=0 AND F10=1
          F1=0
          F10=1
          F12=2
C                       RETURN
      END
C SUB CFGCHG2.FOR
      SUBROUTINE CFGCHG2
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE BEHAVIOR OF THE CFG
C       COMMAND FROM INSIDE OPTIMIZATION
          CHARACTER DUMPY*140
          COMMON/LUMPY/DUMPY
C
          INTEGER IEND,IC,I
C
          COMMON/AUTOI/I
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
          IF(I.GE.1.AND.I.LE.MAXCFG) THEN
              IF(I.EQ.1) GO TO 7687
C
              IF(I.NE.1) THEN
C
C     UPPRLN COPIES THE CURRENT LENS TO THE PERMANENT LENS IF F12
C     IS 1, IF THE CURRENT LENS IS CFG 1
                  CALL UPPRLN
                  F12=I
              END IF

C       DECIDE IF THE INTEGER PART OF SYSTEM1(56) SHOULD
C       BE INCREMENTED.
              IEND=INT(SYSTEM1(56))
              IF(I.GT.IEND) THEN
C       CHANGE IEND AND FRAC SYSTEM1(50)
                  SYSTEM1(56)=SYSTEM1(56)+1.0D0
                  SYSP(56)=SYSTEM1(56)
              END IF
C
C       RETURN TO CFG 1
C
 7687         IF(I.EQ.1) THEN
C
                  IF(F12.NE.1) THEN
                      CALL PTOC
C************************************************************
C               FINISHED RETURNING TO CFG 1
C
                      F12=1
                      F1=0
                      F6=1
                      F22=1
                      LNSTYP=1
                      CALL LNSEOS
                  END IF
                  RETURN
              END IF
C
C       HERE IS WERE THE CHANGES OCCUR
C       CHANGE TO CFG F12
C
C     NEXT COPY THE PERMANENT LENS TO THE ACTIVE LENS ARRAYS
C
              LI=LIP
              LLTYPE=LLTYPEP
              INNI=INNIP
              LIC=LICP
              SYSTEM1=SYSP
              ALENS=ALENP
              PIKP=PIKUP
              SOLVE=SLVP
              FTFL01=FT01P
              GLANAM=GLANMP
C
              IF(F12.NE.1) THEN
C       NOW THE ORIGINAL PERMANENT LENS IS IN
C       WORKING LENS STORAGE
C
C       NOW THE MAGIC. FOR THE PARTICULAR CFG
C       DESIGNATED BY THE CURRENT VALUE OF FLAG F12.
C       DURING THIS PROCESS, FLAG F15 IS SET TO 1
C       TO HELP THE UPDATE LENS AND UPDATE SPSRF
C       LEVELS KNOW HOW TO PROERLY RESPOND
                  F15=1
C       READ THE CONTENTS OF
C               CONFIG(F12,IC) FROM IC=1 TO IC=CFGCNT(F12)
C       AND ONE BY ONE RUN THE STRING VALUES FOUND BACK
C       THROUGH THE PROCESS SUBROUTINE
                  CFCH=.TRUE.
                  DO 14 IC=1,CFGCNT(F12)
                      EE12=CONFG(F12,IC)
                      DUMPY=EE12
                      CALL CFCHG1
 14               CONTINUE
                  CFCH=.FALSE.
C
C       PROCESS DONE, SET F15 TO 0
                  F15=0
              END IF
          END IF
          RETURN
      END
C SUB CFGCHG.FOR
      SUBROUTINE CFGCHG
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE BEHAVIOR OF THE CFG
C       COMMAND FROM THE CMD LEVEL
          CHARACTER DUMPY*140
          COMMON/LUMPY/DUMPY
          LOGICAL CFGQUIET
          COMMON/QUIETCFG/CFGQUIET
C
          INTEGER IEND,I
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
C       CFG TAKES ONLY NUMERIC WORD #1 INPUT
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE='"CFG" ONLY TAKES NUMERIC WORD #1 INPUT'
              IF(.NOT.CFGQUIET) CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              IF(.NOT.CFGQUIET) CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'CFG'.AND.DF1.EQ.1) THEN
C       JUST PRINT OUT THE CURRENT CONFIGURATION NUMBER
 765          FORMAT(1X)
              IF(OPTMES) THEN
                  IF(F4.EQ.0) THEN
                  END IF
                  IF(F4.EQ.0) WRITE(OUTLYNE,765)
                  IF(.NOT.CFGQUIET.AND.F4.EQ.0) CALL SHOWIT(0)
                  IF(F4.EQ.0) THEN
                  END IF
                  IF(F4.EQ.0) WRITE(OUTLYNE,955) F12
                  IF(.NOT.CFGQUIET.AND.F4.EQ.0) CALL SHOWIT(0)
              END IF
 955          FORMAT('CURRENT CONFIGURATION NUMBER IS '
     1        ,I3)
              RETURN
          END IF
          IF(WC.EQ.'CFG'.AND.S1.EQ.1) THEN
              IF(W1.GE.1.0.AND.W1.LE.DBLE(MAXCFG)) THEN
                  IF(INT(W1).EQ.1) GO TO 7687
                  IF(CFGCNT(INT(W1)).EQ.0.AND.INT(W1).NE.1) THEN
                      WRITE(OUTLYNE,*)'NO CONFIGS DATA DEFINED FOR CFG ',INT(W1)
                      IF(.NOT.CFGQUIET) CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'CURRENT CONFIGURATION IS NUMBER ',F12
                      IF(.NOT.CFGQUIET) CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  IF(INT(W1).NE.1) THEN
                      IF(INT(W1).EQ.F12) THEN
                          IF(OPTMES) THEN
                              IF(F4.EQ.0) THEN
                              END IF
                              IF(F4.EQ.0) WRITE(OUTLYNE,998) INT(W1)
                              IF(.NOT.CFGQUIET.AND.F4.EQ.0) CALL SHOWIT(1)
 998                          FORMAT('CURRENT CONFIGURATION IS ',I3,
     1                        ' NO CHANGE IS REQUIRED')
                          END IF
                          RETURN
                      END IF
                      IF(OPTMES) THEN
                          IF(F4.EQ.0) THEN
                          END IF
                          IF(F4.EQ.0) WRITE(OUTLYNE,996) INT(W1)
                          IF(.NOT.CFGQUIET.AND.F4.EQ.0) CALL SHOWIT(1)
                      END IF
 996                  FORMAT('CURRENT CONFIGURATION CHANGED TO ',I3)
C
C     UPPRLN COPIES THE CURRENT LENS TO THE PERMANENT LENS IF F12
C     IS 1, IF THE CURRENT LENS IS CFG 1
                      CALL UPPRLN
                      F12=INT(W1)
                  END IF

C       DECIDE IF THE INTEGER PART OF SYSTEM1(56) SHOULD
C       BE INCREMENTED.
                  IEND=INT(SYSTEM1(56))
                  IF(INT(W1).GT.IEND) THEN
C       CHANGE IEND AND FRAC SYSTEM1(50)
                      SYSTEM1(56)=SYSTEM1(56)+1.0D0
                      SYSP(56)=SYSTEM1(56)
                  END IF
C
C       RETURN TO CFG 1
C
 7687             IF(INT(W1).EQ.1) THEN
                      IF(OPTMES) THEN
                          IF(F4.EQ.0) THEN
                          END IF
                          IF(OUT.EQ.6) THEN
                              IF(F12.EQ.1.AND.F4.EQ.0) WRITE(OUTLYNE,997)
                              IF(.NOT.CFGQUIET.AND.F12.EQ.1.AND.F4.EQ.0) CALL SHOWIT(1)
                              IF(F12.NE.1.AND.F4.EQ.0) WRITE(OUTLYNE,799)
                              IF(.NOT.CFGQUIET.AND.F12.NE.1.AND.F4.EQ.0) CALL SHOWIT(1)
                          END IF
                      END IF
 799                  FORMAT('RETURNED TO CONFIG NUMBER 1')
 997                  FORMAT('CURRENT CONFIGURATION REMAINS CFG NUMBER 1')
C       NOW COPY THE PERMANENT LENS BACK TO THE CURRENT LENS
C       DATA
C
                      IF(F12.NE.1) THEN
                          CALL PTOC
C************************************************************
C               FINISHED RETURNING TO CFG 1
C
                          F12=1
                          F1=0
                          F6=1
                          F22=1
                          LNSTYP=1
                          CALL LNSEOS
C
                      END IF
                      RETURN
                  END IF
C
C       HERE IS WERE THE CHANGES OCCUR
C       CHANGE TO CFG F12
C
C     NEXT COPY THE PERMANENT LENS TO THE ACTIVE LENS ARRAYS
C
                  LI=LIP
                  LLTYPE=LLTYPEP
                  INNI=INNIP
                  LIC=LICP
                  SYSTEM1=SYSP
                  ALENS=ALENP
                  PIKP=PIKUP
                  SOLVE=SLVP
                  FTFL01=FT01P
                  GLANAM=GLANMP
C
                  IF(F12.NE.1) THEN
C       NOW THE ORIGINAL PERMANENT LENS IS IN
C       WORKING LENS STORAGE
C
C       NOW THE MAGIC. FOR THE PARTICULAR CFG
C       DESIGNATED BY THE CURRENT VALE OF FLAG F12.
C       DURING THIS PROCESS, FLAG F15 IS SET TO 1
C       TO HELP THE UPDATE LENS AND UPDATE SPSRF
C       LEVELS KNOW HOW TO PROERLY RESPOND
                      F15=1
C       READ THE CONTENTS OF
C               CONFIG(F12,I) FROM I=1 TO I=CFGCNT(F12)
C       AND ONE BY ONE RUN THE STRING VALUES FOUND BACK
C       THROUGH THE PROCESS SUBROUTINE
                      CFCH=.TRUE.
                      DO I=1,CFGCNT(F12)
                          EE12=CONFG(F12,I)
                          DUMPY=EE12
                          CALL CFCHG1
                      END DO
                      CFCH=.FALSE.
C
C       PROCESS DONE, SET F15 TO 0
                      F15=0
                  END IF
              ELSE
                  OUTLYNE='CONFIGURATION NUMBER BEYOND LEGAL RANGE'
                  IF(.NOT.CFGQUIET) CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  IF(.NOT.CFGQUIET) CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB DEZOOM.FOR
      SUBROUTINE DEZOOM
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE BEHAVIOR OF THE DEZOOM
C       COMMAND FROM THE CMD LEVEL
C
C       DEZOOM CHANGES THE DESIGNATED CONFIGURATION
C       INTO THE MAIN PERMANENT LENS AND THE DELETES
C       ALL CONFIGURATION DATA AND SETS CFG = 1
C
          CHARACTER
     1    DUMPY*140,BLANK*140
C
          COMMON/LUMPY/DUMPY
C
          COMMON/BLAAA/BLANK
C
          INTEGER I
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
C       DEZOOM TAKES ONLY NUMERIC INPUT
C
          BLANK=AA//AA//AA//AA//AA//AA//AA
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"DEZOOM" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'DEZOOM'.AND.DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"DEZOOM" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'DEZOOM'.AND.S1.EQ.1) THEN
C
C       HERE GOES
C
              IF(W1.GE.1.0.AND.W1.LE.DBLE(MAXCFG)) THEN
C       VALID CFG NUMBER
C
                  IF(CFGCNT(INT(W1)).EQ.0.AND.INT(W1).NE.1) THEN
C       CFG DESIGNATED HAS NO DATA, NODEZOOM
                      WRITE(OUTLYNE,*)'NO CONFIGS DATA DEFINED FOR CFG ',INT(W1)
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO DEZOOMING PERFORMED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  IF(INT(W1).NE.1) THEN
                      WRITE(OUTLYNE,*)'DEZOOMING FROM CFG NUMBER',INT(W1)
                      CALL SHOWIT(1)
                  END IF
C
                  IF(INT(W1).EQ.1) THEN
                      WRITE(OUTLYNE,*)'DEZOOMING FROM CFG #1 IS MEANINGLESS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C       FIRST COPY THE PERMANENT LENS TO THE ACTIVE LENS ARRAYS
C
                  LI=LIP
                  LLTYPE=LLTYPEP
                  INNI=INNIP
                  LIC=LICP
                  SYSTEM1=SYSP
                  ALENP=ALENS
                  PIKUP=PIKP
                  SLVP=SOLVE
                  FT01P=FTFL01
                  GLANMP=GLANAM
                  LBL=PLBL
C
C       NOW CHANGE TO THE CFG INDICATED BY INT(W1)
C
                  F12=INT(W1)
C
C       NOW THE MAGIC. FOR THE PARTICULAR CFG
C       DESIGNATED BY THE CURRENT VALE OF FLAG F12.
C       DURING THIS PROCESS, FLAG F15 IS SET TO 1
C       TO HELP THE UPDATE LENS AND UPDATE SPSRF
C       LEVELS KNOW HOW TO PROERLY RESPOND
                  F15=1
C       READ THE CONTENTS OF
C               CONFG(F12,I)
C       FROM I=1 TO I=CFGCNT(F12)
C       AND ONE BY ONE RUN THE STRING VALUES FOUND BACK
C       THROUGH THE PROCESS SUBROUTINE
                  DO 14 I=1,CFGCNT(F12)
                      EE12=CONFG(F12,I)
                      DUMPY=EE12
                      CALL CFCHG1
 14               CONTINUE
C
C       PROCESS DONE, SET F15 TO 0
                  F15=0
C
C       NOW SET F12 TO 1 AND DELETE ALL THE CONFIGS DATA
                  F12=1
                  CFGCNT(2:MAXCFG)=0
C
C       NOW COPY THIS INTO THE PERM DATA
C
                  CALL CTOP
                  LNSTYP=1
                  CALL LNSEOS
                  IF(F28.EQ.0) CALL VCHECK
C
              ELSE
                  WRITE(OUTLYNE,*)'CONFIGURATION NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB DELCFG.FOR.FOR
      SUBROUTINE DELCFG
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED FROM THE CMD LEVEL
C       IT IS USED TO DELETE THE CONFIGURATION DATA
C       DESIGNATER BY THE CONFIGURATION NUMBER INT(W1)
C
C       CURRENT CONFIGURATION NUMBER IS NOT AFFECTED
C
          CHARACTER BLANK*140
C
          COMMON/BLAAA/BLANK
C
!        INTEGER J
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"DELCFG" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       DEFAULT NUMERIC INPUT
          IF(DF1.EQ.1) THEN
              WRITE(OUTLYNE,*)'"DELCFG" MUST HAVE EXPLICIT NUMERIC INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CFG OUT OF RANGE
          IF(INT(W1).LT.2.OR.INT(W1).GT.MAXCFG) THEN
              WRITE(OUTLYNE,*)'NUMERIC INPUT BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'VALID CFG NUMBERS ARE 2 TO ',MAXCFG
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).GE.2.AND.INT(W1).LE.MAXCFG) THEN
C       VALID ADDRESS
C       CASE OF EMPTY CFG
              IF(CFGCNT(INT(W1)).LT.1) THEN
                  WRITE(OUTLYNE,*)'CFG',INT(W1),' :ALREADY EMPTY'
                  CALL SHOWIT(1)
                  RETURN
              ELSE
C       NOT EMPTY
                  BLANK=AA//AA//AA//AA//AA//AA//AA
                  CONFG(INT(W1),1:MAXCFG)=BLANK(1:140)
                  CFGCNT(INT(W1))=0
                  LNSTYP=1
                  CALL LNSEOS
                  IF(F28.EQ.0) CALL VCHECK
                  WRITE(OUTLYNE,*)'CFG',INT(W1),' :CONFIG DATA DELETED'
                  CALL SHOWIT(1)
              END IF
          END IF
          RETURN
      END
C SUB FRCCF1.FOR
      SUBROUTINE FRCCF1(I)
C
C     DOES AN UNCONDITIONAL FORCED RETURN TO CFG1
C
          IMPLICIT NONE
          INTEGER I
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
C
C       NOW FORCE THE LENS TO BE IN CONFIG #1
C       NOW COPY THE PERMANENT LENS BACK TO THE CURRENT LENS
C       DATA

          IF(F15.EQ.0.AND.F12.NE.1) THEN
              CALL PTOC
          END IF
C               FINISHED RETURNING TO CFG 1
          F12=1
          F6=1
          F1=0
          F22=I
          LNSTYP=2
          CALL LNSEOS
          RETURN
      END
