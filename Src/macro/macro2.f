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

C       SECOND FILE OF MACRO FILES

C SUB LTH80.FOR
      SUBROUTINE LTH80(IPT80,LENG)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE DETERMINES THE LAST POSITION IN
C       THE CHARACTER VARIABLE INPT80 WHICH IS NOT BLANK.
C       IT PASSES THIS LAST NON-BLANK POSITION BACK TO THE
C       CALLING ROUTINE IN THE INTEGER VARIABLE LENG
C
          CHARACTER IPT80*80
C
          INTEGER LENG,QBVAL,I
C
          LENG=80
          DO 10 I=80,1,-1
              QBVAL=ICHAR(IPT80(I:I))
              IF(QBVAL.EQ.32) THEN
                  LENG=LENG-1
                  GO TO 10
              ELSE
C       QBVAL IS NOT 32. FOUND END OF INPT80
                  RETURN
              END IF
 10       CONTINUE
C       IF YOU GOT HERE THEN INPT80 WAS ALL BLANK
          RETURN
      END
C SUB LTH140.FOR
      SUBROUTINE LTH140(IPT140,LENG)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE DETERMINES THE LAST POSITION IN
C       THE CHARACTER VARIABLE INPT140 WHICH IS NOT BLANK.
C       IT PASSES THIS LAST NON-BLANK POSITION BACK TO THE
C       CALLING ROUTINE IN THE INTEGER VARIABLE LENG
C
          CHARACTER IPT140*140
C
          INTEGER LENG,QBVAL,I
C
          LENG=140
          DO 10 I=140,1,-1
              QBVAL=ICHAR(IPT140(I:I))
              IF(QBVAL.EQ.32) THEN
                  LENG=LENG-1
                  GO TO 10
              ELSE
C       QBVAL IS NOT 32. FOUND END OF IPT140
                  RETURN
              END IF
 10       CONTINUE
C       IF YOU GOT HERE THEN IPT140 WAS ALL BLANK
          RETURN
      END
C SUB MSTAT.FOR
      SUBROUTINE MSTAT
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE RETURNS THE NUMBER OF MACROS
C       CURRENTLY ON FILE IN THE MACRO DIRECTORY
C
          INTEGER LK,LN,OC,J,I,K
C
          LOGICAL EXISJK
C
          CHARACTER NM*8,STAMP*20
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C       OPEN UNIT 20 FOR I/O
C
          IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"MSTAT" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"MACRO FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
C
          J=0
C       J COUNTS THE NUMBER OF NON-BLANK ENTRIES IN THE DIRECTORY.
          DO 10 I=1,MAXMAC
              READ(UNIT=20,REC=I,ERR=10) NM,LK,LN,OC,STAMP
              IF(OC.EQ.1) J=J+1
 10       CONTINUE
C
          K=(MAXMAC)-J
C       CLOSE UNIT 20 TO I/O
C
          CALL CLOSE_FILE(20,1)
          WRITE(OUTLYNE,*)J,' MACROS ON FILE.'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'ROOM FOR',K,' MORE MACROS'
          CALL SHOWIT(1)
          RETURN
      END
C SUB MSAVE.FOR
      SUBROUTINE MSAVE
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO SAVE THE CONTENTS OF THE NAMED
C       REGISTERS REG(1:50) TO THE ARRAY SREG(0:20,1:50).
C       SREG CAN BE PASSED VIA COMMON SMEMRY IF NEEDED.
C       THE FIRST INDEX IN SREG REFERS TO THE CURRENT MACRO
C       NESTING LEVEL.
C
          REAL*8 SREG(0:20,1:50)
C
          COMMON/SMEMRY/SREG
C
!        INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
          IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)'"SAVE" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          SREG(NEST,1:50)=REG(1:50)
          WRITE(OUTLYNE,*)'NAMED REGISTERS SAVED AT NEST LEVEL=',NEST
          CALL SHOWIT(1)
          RETURN
      END
C SUB MREFRESH.FOR
      SUBROUTINE MREFRESH
C
          IMPLICIT NONE
C
C     USED TO REFRESH A MACRO BEING EDITTED
C
          LOGICAL EXISMED
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          CHARACTER*8 TO,FROM
          COMMON/TOFROM/TO,FROM
C
C       "MREFRESH" COMMAND
          IF(STI.EQ.1) THEN
              OUTLYNE='"MREFRESH" REFRESHES A MACRO BEING EDITTED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SQ.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE='"MREFRESH" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          EXISMED=.FALSE.
          INQUIRE(FILE='MAC_EDIT.DAT',EXIST=EXISMED)
          IF(EXISMED) THEN
              SAVE_KDP(17)=SAVEINPT(17)
              INPUT='IN FILE MAC_EDIT.DAT'
              CALL PROCES
              REST_KDP(17)=RESTINPT(17)
          END IF
          RETURN
      END
C SUB MRENAME.FOR
      SUBROUTINE MRENAME
C
          IMPLICIT NONE
C
C     USED TO RENAME A MACRO
C
!      LOGICAL SAVE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          CHARACTER*8 TO,FROM
          COMMON/TOFROM/TO,FROM
C
C       "RENAME" COMMAND
          IF(SN.EQ.1) THEN

              OUTLYNE='"MRENAME" TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'MRENAME'.AND.SQ.EQ.0.OR.WC.EQ.'MRENAME'
     1    .AND.SST.EQ.0) THEN
              OUTLYNE='"MRENAME" REQUIRES BOTH QUALIFIER AND STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     LMEDIT THE NEW NAME MACRO
          TO=WS(1:8)
          FROM=WQ(1:8)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='LMEDIT '//TO
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          IF(NOMEDIT) THEN
              NOMEDIT=.FALSE.
              OUTLYNE=
     1        'TARGET MACRO NAME INVALID. IT MAY ALREADY EXIST'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='EX '//FROM
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          IF(NOEXTRACT) THEN
              NOMEDIT=.FALSE.
              OUTLYNE=
     1        'SOURCE MACRO NAME INVALID. IT MIGHT NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FL'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='FL'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='MDEL '//FROM
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
C                       RETURN
      END


C SUB MCOPY.FOR
      SUBROUTINE MCOPY
C
          IMPLICIT NONE
C
C     USED TO RENAME A MACRO
C
          !     LOGICAL SAVE

          INTEGER OOUT
          COMMON/OUTO/OOUT
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          CHARACTER*8 TO,FROM
          COMMON/TOFROM/TO,FROM
C
C       "MCOPY" COMMAND
          IF(SN.EQ.1) THEN
              OUTLYNE='"MCOPY" TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'MCOPY'.AND.SQ.EQ.0.OR.WC.EQ.'MCOPY'
     1    .AND.SST.EQ.0) THEN
              OUTLYNE='"MCOPY" REQUIRES BOTH QUALIFIER AND STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          TO=WS(1:8)
          FROM=WQ(1:8)
C     LMEDIT THE NEW NAME MACRO
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='LMEDIT '//TO
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          IF(NOMEDIT) THEN
              NOMEDIT=.FALSE.
              OUTLYNE=
     1        'TARGET MACRO NAME INVALID. IT MAY ALREADY EXIST'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='EX '//FROM
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          IF(NOEXTRACT) THEN
              NOMEDIT=.FALSE.
              OUTLYNE=
     1        'SOURCE MACRO NAME INVALID. IT MIGHT NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
C
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='FL'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              CALL MACFAL
              RETURN
          END IF
C
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='FL'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
C                       RETURN
      END


C SUB MREA.FOR
      SUBROUTINE MREA
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED BY PROCESS IF F47 = 1
C       WHICH DESIGNATES LINE REPLACEMENT MODE IN MACRO
C       EDIT.

          INTEGER MMIJ,I
C
          COMMON/MIJ/MMIJ
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          IF(F47.EQ.-1) THEN
              F47=0
              OUTLYNE='MACRO LINE REPLACEMENT NOT PERFORMED'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
C       FIRST CLEAR FLAG F47
          F47=0
C       NOW PERFORM THE REPLACEMENT AT THE CURRENT LINE.
C
          I=CURLIN
          MACCW(I)=WC
          MACQW(I)=WQ
          MACSTR(I)=WS
          MACNW(1,I)=W1
          MACNW(2,I)=W2
          MACNW(3,I)=W3
          MACNW(4,I)=W4
          MACNW(5,I)=W5
          MACSTA(1,I)=SB1
          MACSTA(2,I)=SB2
          MACSTA(3,I)=SC1
          MACSTA(4,I)=SC2
          MACSTA(5,I)=SQ
          MACSTA(6,I)=SST
          MACSTA(7,I)=S1
          MACSTA(8,I)=S2
          MACSTA(9,I)=S3
          MACSTA(10,I)=S4
          MACSTA(11,I)=S5
          MACSTA(12,I)=DF1
          MACSTA(13,I)=DF2
          MACSTA(14,I)=DF3
          MACSTA(15,I)=DF4
          MACSTA(16,I)=DF5
          MACSTA(17,I)=SN
          MACSTA(18,I)=STI
C       MACSTA(19,I) AND MACSTA(20,I) NOT YET USED
          MACSTA(19,I)=0
          MACSTA(20,I)=0
C       THE CURRENT LINE HAS BEEN REPLACED.
          RETURN
      END
C SUB MPR.FOR
      SUBROUTINE MPR
C
          IMPLICIT NONE
C
C       THIS MACRO CONTROLS PRINTING OF LINES DURING THE MEDIT
C       MODE
C
!        CHARACTER ALN1*80
C
          INTEGER LENG,NUMLIN,COUNT,MWW1
C
          INTEGER MACLOC,IMACEN,
     6    MMIJ,I,LINBOT,LINCUR

          CHARACTER
     2    BN1*10,BN2*10,BN3*10,BN4*10,BN5*10,
     3    BBN1*1,BBN2*1,BBN3*1,BBN4*1,BBN5*1,
     4    LINE1*98,MACNAM*8
C
          COMMON/MIJ/MMIJ
C
          COMMON/MACID/MACNAM
C
          COMMON/MACID2/MACLOC,IMACEN
C
          COMMON/STAT13/LINCUR,LINBOT
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C               HANDEL TOP AND BOTTOM
C
          IF(CURLIN.EQ.0) THEN
              CURLIN=0
          ELSE
          END IF
C
C               TOP AND BOTTOM HANDLED
C
C       EACH TIME A SINGLE LINE IS PRINTED, THE CURRENT
C       LINE IS INCREMENTED BY 1 UNLESS ONLY ONE LINE WAS
C       REQUESTED. PR OR PR,-1 WILL REQUEST THIS.
C
C       THE PR COMMAND IN MEDIT ONLY ALLOWS OUTPUT TO THE
C       TERMINAL OR SCREEN (OUT=6) AND THE PRINTER FILE
C       (OUT=7). AND OTHER OUT VALUE YIELDS AN ERROR MESSAGE
C       AND AN AUTOMATIC RETURN TO OUT=6.
C
C       HOW MANY LINES ARE TO BE PRINTED?
C               W1 LINES
C
          COUNT=1
          NUMLIN=1
          MWW1=0
          IF(W1.LE.0.0D0) THEN
              W1=1.0D0
              MWW1=1
          ELSE
          END IF
          NUMLIN=INT(W1)
          COUNT=NUMLIN
 41       IF(COUNT.EQ.0) GO TO 2000
          IF(MWW1.EQ.1.AND.CURLIN.EQ.0) CURLIN=1
          IF(COUNT.LE.NUMLIN.AND.MWW1.NE.1) CURLIN=CURLIN+1
          IF(CURLIN.GT.(MCDIR2(2,(MMIJ))-2))
     1    CURLIN=MCDIR2(2,(MMIJ))-2
          I=CURLIN
C       CHECK FOR PRESECE OF A QUALIFIER
          IF(MACSTA(5,I).EQ.1) THEN
C       THERE IS A QUALIFIER WORD
C       NOW CHECK FOR PRESENCE OF A STRING
              IF(MACSTA(6,I).EQ.1) THEN
C       THERE IS A QUALIFIER WORD AND A STRING  AND
C       NO NUMERIC WORDS SINCE STRINGS AND NUMERIC WORDS
C       ARE MUTUALLY EXCLUSIVE.
C       PRODUCE OUTPUT
C
C       HERE OUT ALWAYS = 6 SO WRITE COMPACT 80 COLUMN OUTPUT.
C
                  LINE1=MACCW(I)//CHAR(32)//MACQW(I)//CHAR(32)//MACSTR(I)
                  GO TO 40
              ELSE
C       THERE IS NO STRING,THERE ARE NUMERIC WORDS AND A QUALIFIER.
C       BEFORE WRITING THE NUMERIC DATA,IT MUST BE CONVERTED
C       INTO A STRING VARIABLE. OTHERWISE HANDELING THE DEFAULT
C       VALUES BECOMES AN S.O.B..
C       CONVERT THE NUMERIC VALES TO STRING VALUES WITH
C       SUBROUTINE NTOA2.  THE CALL IS :
C
C               CALL NTOA2(N1,N2,N3,N4,N5,BN1,BN2,BN3,BN4,BN5)
C
                  CALL NTOA2(MACNW(1,I),MACNW(2,I),MACNW(3,I),MACNW(4,I)
     1            ,MACNW(5,I),BN1,BN2,BN3,BN4,BN5)
C
C       DEAL WITH THE DEFAULT VALUES
                  BBN1=','
                  BBN2=','
                  BBN3=','
                  BBN4=','
                  BBN5=','
C
                  LINE1=MACCW(I)//CHAR(32)//MACQW(I)//','
                  IF(MACSTA(12,I).EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN1
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN1//','
                  END IF
                  IF(MACSTA(13,I).EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN2
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN2//','
                  END IF
                  IF(MACSTA(14,I).EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN3
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN3//','
                  END IF
                  IF(MACSTA(15,I).EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN4
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN4//','
                  END IF
                  IF(MACSTA(16,I).EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN5
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN5//','
                  END IF
              END IF
              GO TO 40
          ELSE
C       NO QUALIFIER WORD
C       CHECK FOR THE PRESENCE OF A STRING
              IF(MACSTA(6,I).EQ.1) THEN
C       THERE IS A STRING WITHOUT A QUALIFIER AND NO NUMERIC INPUT
C       PRODUCE OUTPUT
C
                  LINE1=MACCW(I)//CHAR(32)//MACSTR(I)
                  GO TO 40
              ELSE
C       THERE IS NO QUALIFIER AND NO STRING
C       THERE IS NUMERIC INPUT.
C       BEFORE WRITING THE NUMERIC DATA,IT MUST BE CONVERTED
C       INTO A STRING VARIABLE. OTHERWISE HANDELING THE DEFAULT
C       VALUES BECOMES AN S.O.B..
C       CONVERT THE NUMERIC VALES TO STRING VALUES WITH
C       SUBROUTINE NTOA2.  THE CALL IS :
C
                  CALL NTOA2(MACNW(1,I),MACNW(2,I),MACNW(3,I),MACNW(4,I)
     1            ,MACNW(5,I),BN1,BN2,BN3,BN4,BN5)
C
C       DEAL WITH THE DEFAULT VALUES
                  BBN1=','
                  BBN2=','
                  BBN3=','
                  BBN4=','
                  BBN5=','
C
C       PRODUCE OUTPUT. WRITE NUMERIC WORDS EVEN IF ALL ZERO.
                  LINE1=MACCW(I)//','
                  IF(MACSTA(12,I).EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN1
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN1//','
                  END IF
                  IF(MACSTA(13,I).EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN2
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN2//','
                  END IF
                  IF(MACSTA(14,I).EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN3
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN3//','
                  END IF
                  IF(MACSTA(15,I).EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN4
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN4//','
                  END IF
                  IF(MACSTA(16,I).EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN5
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN5//','
                  END IF
              END IF
          END IF
 40       WRITE(OUTLYNE,500)I,LINE1
          CALL SHOWIT(3)
          IF(CURLIN.EQ.(MCDIR2(2,(MMIJ))-2)) THEN
              OUTLYNE='LINE POINTER AT MACRO BOTTOM'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(MWW1.EQ.1) THEN
              MWW1=0
              RETURN
          END IF
          COUNT=COUNT-1
          GO TO 41
 500      FORMAT(I3,A76)
! 501    FORMAT(I3,A76)
 2000     RETURN
      END
C SUB MOD2.FOR
      SUBROUTINE MOD2
C
          IMPLICIT NONE
C
C       THIS DOES BT,NEXT,DE,EX,FL,GO,QUIT,RE,TP,AND INS
C       AT THE MEDIT LEVEL
C
          CHARACTER WCN*8,WQN*8,WSN*80,
     2    FILNAM*10,TTIME*8,DDATE*10
C
          LOGICAL EXISJK
C
          COMMON/NXT/WCN,WQN,WSN
C
          INTEGER NDEL,
     6    MMIJ,NCRLIN,OCRLIN,NF,I,J,JK,L,K,KK
     8    ,LLTEST,NW2,NW1,MAXLL,KLI
C
          LOGICAL EXST
C
          COMMON/MIJ/MMIJ
C
          COMMON/NEFER/NF
C
          COMMON/COMNAM/FILNAM
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          IF(WC.EQ.'BT') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"BT" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              OCRLIN=CURLIN
              NCRLIN=(MCDIR2(2,(MMIJ))-2)
C
              CURLIN=(MCDIR2(2,(MMIJ))-2)
              OUTLYNE='LINE POINTER AT MACRO BOTTOM'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WC.EQ.'NEXT') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"NEXT" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              WC=WCN
              WQ=WQN
              SQ=1
              WS=WSN
              SST=1
              IF(WC.EQ.'LO') THEN
                  CALL MLO
              ELSE
                  OUTLYNE='NEXT CAN ONLY BE USED AFTER THE "LO" COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'DE') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"DE" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       IF THE CURRENT LINE IS 0 THEN NO DELETION OCCURS
C       JUST PRINT LINE POINTER AT MACRO TOP
C
              IF(CURLIN.EQ.0) THEN
                  OUTLYNE='LINE POINTER AT MACRO TOP'
                  CALL SHOWIT(1)
                  OUTLYNE='NO DELETION OCCURED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C               WHAT IF W1=0.0D0
              IF(W1.EQ.0.0D0) W1=1.0D0
              NDEL=INT(W1)
C
              OCRLIN=CURLIN
C       BEFORE DELETION, I=CURLIN
C       WHEN THE CURRENT LINE IS DELETED,ALL OF THE MACRO
C       DATA STORAGE IS REFILED ONE POSITION CLOSER TO
C       THE FIRST POSITION.
C
C
 10           I=CURLIN
C
              IF(CURLIN.EQ.1.AND.MCDIR2(2,(MMIJ)).EQ.3) THEN
                  OUTLYNE='MACRO NOW HAS ONE BLANK LINE'
                  CALL SHOWIT(1)
                  MACCW(I)=' '
                  MACQW(I)=' '
                  MACSTR(I)=' '
                  MACNW(1,I)=0.0D0
                  MACNW(2,I)=0.0D0
                  MACNW(3,I)=0.0D0
                  MACNW(4,I)=0.0D0
                  MACNW(5,I)=0.0D0
                  MACSTA(1,I)=1
                  MACSTA(2,I)=1
                  MACSTA(3,I)=0
                  MACSTA(4,I)=0
                  MACSTA(5,I)=0
                  MACSTA(6,I)=0
                  MACSTA(7,I)=0
                  MACSTA(8,I)=0
                  MACSTA(9,I)=0
                  MACSTA(10,I)=0
                  MACSTA(11,I)=0
                  MACSTA(12,I)=1
                  MACSTA(13,I)=1
                  MACSTA(14,I)=1
                  MACSTA(15,I)=1
                  MACSTA(16,I)=1
                  MACSTA(17,I)=0
                  MACSTA(18,I)=0
C       MACSTA(19,I) AND MACSTA(20,I) NOT YET USED
                  MACSTA(19,I)=0
                  MACSTA(20,I)=0
                  RETURN
              ELSE
C       MACRO HAS MORE THAN ONE BLANK LINE, PROCEED
              END IF
C
              IF(CURLIN.EQ.(MCDIR2(2,(MMIJ))-2)) THEN
C       YOU ARE AT THE LAST MACRO LINE. DELETION IS SIMPLE
C       JUST DECREMENT MCDIR2(2,(MMIJ)) BY 1 AND DECREMENT CURLIN BY
C       1 AND RETURN. YOU RETURN EVEN IF MORE THAN ONE LINE WAS
C       REQUESTED TO BE DELETED BECAUSE YOU CAN ONLY DELETE ONE
C       LINE IN THIS CASE.
                  MCDIR2(2,(MMIJ))=MCDIR2(2,(MMIJ))-1
                  CURLIN=CURLIN-1
                  CALL MPR
                  RETURN
              ELSE
C       PROCEED, YOU ARE NOT ON THE LAST LINE
              END IF
              DO J=I,(MCDIR2(2,(MMIJ))-3)
                  MACCW(J)=MACCW(J+1)
                  MACQW(J)=MACQW(J+1)
                  MACSTR(J)=MACSTR(J+1)
                  MACNW(1,J)=MACNW(1,(J+1))
                  MACNW(2,J)=MACNW(2,(J+1))
                  MACNW(3,J)=MACNW(3,(J+1))
                  MACNW(4,J)=MACNW(4,(J+1))
                  MACNW(5,J)=MACNW(5,(J+1))
                  MACSTA(1,J)=MACSTA(1,(J+1))
                  MACSTA(2,J)=MACSTA(2,(J+1))
                  MACSTA(3,J)=MACSTA(3,(J+1))
                  MACSTA(4,J)=MACSTA(4,(J+1))
                  MACSTA(5,J)=MACSTA(5,(J+1))
                  MACSTA(6,J)=MACSTA(6,(J+1))
                  MACSTA(7,J)=MACSTA(7,(J+1))
                  MACSTA(8,J)=MACSTA(8,(J+1))
                  MACSTA(9,J)=MACSTA(9,(J+1))
                  MACSTA(10,J)=MACSTA(10,(J+1))
                  MACSTA(11,J)=MACSTA(11,(J+1))
                  MACSTA(12,J)=MACSTA(12,(J+1))
                  MACSTA(13,J)=MACSTA(13,(J+1))
                  MACSTA(14,J)=MACSTA(14,(J+1))
                  MACSTA(15,J)=MACSTA(15,(J+1))
                  MACSTA(16,J)=MACSTA(16,(J+1))
                  MACSTA(17,J)=MACSTA(17,(J+1))
                  MACSTA(18,J)=MACSTA(18,(J+1))
                  MACSTA(19,J)=0
                  MACSTA(20,J)=0
C       MACSTA(19,IMAC) AND MACSTA(20,IMAC) NOT YET USED
              END DO
C       DECREASE THE MACRO LENGTH COUNTER BY ONE
              MCDIR2(2,(MMIJ))=MCDIR2(2,(MMIJ))-1
C       BUT CURRENT LINE REMAINS THE SAME
C
              NDEL=NDEL-1
              IF(NDEL.EQ.0) THEN
C       YOU HAVE DELETED ALL THAT WAS REQUESTED
                  W1=0.0D0
                  CALL MPR
                  RETURN
              ELSE
                  GO TO 10
              END IF
          ELSE
C        NOT DE
          END IF
          IF(WC.EQ.'DEQUIET') THEN
C
C       IF THE CURRENT LINE IS 0 THEN NO DELETION OCCURS
C       JUST PRINT LINE POINTER AT MACRO TOP
C
              IF(CURLIN.EQ.0) THEN
                  CALL MACFAL
                  RETURN
              END IF
C
C               WHAT IF W1=0.0D0
              IF(W1.EQ.0.0D0) W1=1.0D0
              NDEL=INT(W1)
C
              OCRLIN=CURLIN
C       BEFORE DELETION, I=CURLIN
C       WHEN THE CURRENT LINE IS DELETED,ALL OF THE MACRO
C       DATA STORAGE IS REFILED ONE POSITION CLOSER TO
C       THE FIRST POSITION.
C
C
 11           I=CURLIN
C
              IF(CURLIN.EQ.1.AND.MCDIR2(2,(MMIJ)).EQ.3) THEN
                  MACCW(I)=' '
                  MACQW(I)=' '
                  MACSTR(I)=' '
                  MACNW(1,I)=0.0D0
                  MACNW(2,I)=0.0D0
                  MACNW(3,I)=0.0D0
                  MACNW(4,I)=0.0D0
                  MACNW(5,I)=0.0D0
                  MACSTA(1,I)=1
                  MACSTA(2,I)=1
                  MACSTA(3,I)=0
                  MACSTA(4,I)=0
                  MACSTA(5,I)=0
                  MACSTA(6,I)=0
                  MACSTA(7,I)=0
                  MACSTA(8,I)=0
                  MACSTA(9,I)=0
                  MACSTA(10,I)=0
                  MACSTA(11,I)=0
                  MACSTA(12,I)=1
                  MACSTA(13,I)=1
                  MACSTA(14,I)=1
                  MACSTA(15,I)=1
                  MACSTA(16,I)=1
                  MACSTA(17,I)=0
                  MACSTA(18,I)=0
C       MACSTA(19,I) AND MACSTA(20,I) NOT YET USED
                  MACSTA(19,I)=0
                  MACSTA(20,I)=0
                  RETURN
              ELSE
C       MACRO HAS MORE THAN ONE BLANK LINE, PROCEED
              END IF
C
              IF(CURLIN.EQ.(MCDIR2(2,(MMIJ))-2)) THEN
C       YOU ARE AT THE LAST MACRO LINE. DELETION IS SIMPLE
C       JUST DECREMENT MCDIR2(2,(MMIJ)) BY 1 AND DECREMENT CURLIN BY
C       1 AND RETURN. YOU RETURN EVEN IF MORE THAN ONE LINE WAS
C       REQUESTED TO BE DELETED BECAUSE YOU CAN ONLY DELETE ONE
C       LINE IN THIS CASE.
                  MCDIR2(2,(MMIJ))=MCDIR2(2,(MMIJ))-1
                  CURLIN=CURLIN-1
                  RETURN
              ELSE
C       PROCEED, YOU ARE NOT ON THE LAST LINE
              END IF
              DO J=I,(MCDIR2(2,(MMIJ))-3)
                  MACCW(J)=MACCW(J+1)
                  MACQW(J)=MACQW(J+1)
                  MACSTR(J)=MACSTR(J+1)
                  MACNW(1,J)=MACNW(1,(J+1))
                  MACNW(2,J)=MACNW(2,(J+1))
                  MACNW(3,J)=MACNW(3,(J+1))
                  MACNW(4,J)=MACNW(4,(J+1))
                  MACNW(5,J)=MACNW(5,(J+1))
                  MACSTA(1,J)=MACSTA(1,(J+1))
                  MACSTA(2,J)=MACSTA(2,(J+1))
                  MACSTA(3,J)=MACSTA(3,(J+1))
                  MACSTA(4,J)=MACSTA(4,(J+1))
                  MACSTA(5,J)=MACSTA(5,(J+1))
                  MACSTA(6,J)=MACSTA(6,(J+1))
                  MACSTA(7,J)=MACSTA(7,(J+1))
                  MACSTA(8,J)=MACSTA(8,(J+1))
                  MACSTA(9,J)=MACSTA(9,(J+1))
                  MACSTA(10,J)=MACSTA(10,(J+1))
                  MACSTA(11,J)=MACSTA(11,(J+1))
                  MACSTA(12,J)=MACSTA(12,(J+1))
                  MACSTA(13,J)=MACSTA(13,(J+1))
                  MACSTA(14,J)=MACSTA(14,(J+1))
                  MACSTA(15,J)=MACSTA(15,(J+1))
                  MACSTA(16,J)=MACSTA(16,(J+1))
                  MACSTA(17,J)=MACSTA(17,(J+1))
                  MACSTA(18,J)=MACSTA(18,(J+1))
                  MACSTA(19,J)=0
                  MACSTA(20,J)=0
C       MACSTA(19,IMAC) AND MACSTA(20,IMAC) NOT YET USED
              END DO
C       DECREASE THE MACRO LENGTH COUNTER BY ONE
              MCDIR2(2,(MMIJ))=MCDIR2(2,(MMIJ))-1
C       BUT CURRENT LINE REMAINS THE SAME
C
              NDEL=NDEL-1
              IF(NDEL.EQ.0) THEN
C       YOU HAVE DELETED ALL THAT WAS REQUESTED
                  W1=0.0D0
                  RETURN
              ELSE
                  GO TO 11
              END IF
          ELSE
C        NOT DEQUIET
          END IF
          IF(WC.EQ.'EX') THEN
              IF(SST.EQ.1) THEN
                  OUTLYNE=
     1            '"EX" TAKES NO STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            '"EX" TAKES NO NUMERIC WORD #3, 4 OR 5 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       IF THE MACRO THAT IS BEING EXTRACTED INTO (TARGET MACRO)
C       CONSISTS OF ONE BLANK LINE ONLY, THEN EXTRACT STARTING
C       AT CURLIN AND NOT CURLIN+1
C
C       THE FORMAT OF THE COMMAND IS:
C
C       EX (MACRO NAME),FIRST LINE EXTRACTED , LAST LINE EXTRACTED
C
C       IF NO NUMERIC INPUT IS PROVIDED (DEFAULT VALUES) THEN
C
C       FIRST LINE EXTRACTED DEFAULT = LINE 1
C       LAST LINE EXTRACTED DEFAULT = LAST LINE
C
C       MAKE SURE YOU DON'T WRITE BEYOND THE EOM
C
C       CHECK FOR BLANK MACRO NAME
C
              IF(WQ.EQ.' ') THEN
                  OUTLYNE='SOURCE MACRO NAME MISSING'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(CURLIN.EQ.(MCDIR2(2,(MMIJ))-1)) THEN
                  CURLIN=MCDIR2(2,(MMIJ))-2
              ELSE
              END IF
C
C       WE MUST SEE IF THE SOURCE MACRO IS ON FILE
C       IF IT IS IT IS IN THE DIRECTORY.
C
              DO 1 I=1,MAXMAC
                  IF(WQ.EQ.MCDIR1(I)) THEN
                      JK=I
                      GO TO 50
                  ELSE
                  END IF
 1            CONTINUE
C       IF YOU GOT HERE THE SOURCE MACRO WAS NOT ON FILE
              WRITE(OUTLYNE,*)'SOURCE MACRO ',WQ,' NOT ON FILE'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
 50           IF(JK.EQ.MMIJ) THEN
                  OUTLYNE=
     1            'A MACRO MAY NOT BE EXTRACTED INTO ITSELF'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              LLTEST=MCDIR2(2,(JK))-2
C
C       THE ABOVE ARE THE BOUNDS OF THE SOURCE MACRO
C               NOW HANDEL THE DEFAULT VALUES
              IF(DF1.EQ.1) NW1=1
              IF(DF2.EQ.1) NW2=(MCDIR2(2,(JK))-2)
C               DEFAULTS HANDLED
C
              IF(DF1.EQ.0) NW1=INT(W1)
              IF(DF2.EQ.0) NW2=INT(W2)
C
C       CHECK FOR INVALID LINE NUMBERS
C
              CALL MAXLNN(MAXLL,MAXLIN)
              IF(NW1.LT.1.OR.NW2.LT.NW1.OR.NW2.LT.1.OR.
     1        NW1.GT.(MAXLL-2)
     2        .OR.NW2.GT.(MAXLL-2)) THEN
                  OUTLYNE=
     1            'INVALID LINE NUMBERS REQUESTED FOR EXTRACTION'
                  CALL SHOWIT(1)
              END IF

C       TEST IF REQUESTED LINES ARE BEYOND THE LIMITS OF THE
C       SOURCE MACRO.
              IF((NW2-NW1+1).GT.LLTEST) THEN
                  OUTLYNE=
     1            'NUMBER OF LINES REQUESTED FOR EXTRACTION'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'ARE BEYOND NUMBER OF LINES AVALIABLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(NW2.GT.LLTEST) THEN
                  OUTLYNE=
     1            'TARGET LINE BEYOND END OF SOURCE MACRO'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       NOW CHECK THAT THE CURRENT MACRO PLUS THE REQUESTED
C       EXTRACTED LINES DON'T ADD UP TO A MACRO OVER MAXLL TOTAL
C       LINES. REMEMBER THE CURRENT MACRO IS (MMIJ)
              CALL MAXLNN(MAXLL,MAXLIN)
              IF((MCDIR2(2,(MMIJ))+(NW2-NW1+1)).GT.MAXLL) THEN
                  WRITE(OUTLYNE,*)
     1              'EXTRACTION NOT ALLOWED AS MACRO',MCDIR1(MMIJ)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1              'WOULD EXCEED ',MAXLL,' LINES'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       DOES THE TARGET MACRO HAVE JUST ONE BLANK LINE?
              IF(MCDIR2(2,(MMIJ)).EQ.3.AND.MACCW(1).EQ.' '.
     1        OR.F46.EQ.1) THEN
C***************************************************************
C       NOW PROCEED WITH THE EXTRACTION.
C       THIS IS THE CASE OF THE TARGET MACRO WITH ONE BLANK LINE
C       OR AN EMPTY MACRO.
C       FIRST WE MUST ADD ONE LESS THAN THE NUMBER OF LINES TO BE
C       EXTRACTED TO THE ONE LINE MACRO.
C
C       THE NUMBER OF LINES TO BE EXTRACTED
C       WILL BE (NW2-NW1+1). LET US NOW ADD (NW2-NW1+1-1) LINES.
C       THIS IS EASY TO DO BECAUSE ALL THE LINES ARE BLANK.
C       INCREASE THE LENGTH BY (NW2-NW1)
C
C       FOR THE EMPTY MACRO STRAIGHT FROM MMEDIT WE MUST
C       CHECK F46 AND RESET F46 AND SET INITIAL MCDIR2(2,(MMIJ)) TO 3.
C
                  IF(F46.EQ.1) THEN
                      MCDIR2(2,(MMIJ))=3
                      F46=0
                  ELSE
                  END IF
C       NOW
                  MCDIR2(2,(MMIJ))=MCDIR2(2,(MMIJ))+(NW2-NW1)
C
C       NOW READ THE APPROPRIATE LINES OF THE SOURCE MACRO
C       AND WRITE THEM INTO THE SPACE PROVIDED STARTING
C       AT POSITION (CURLIN) WHICH IN THIS CASE = 1 AND PROCEEDING TO
C       POSITION (CURLIN+ NUMBER OF LINES EXTRACTED)
C       OR CURLIN+(NW2-NW1+1) OR NW2-NW1+2
C
C       OPEN UNIT 20 FOR I/O
C       READ FROM MACRO (JK)
C
                  NF=JK
                  CALL MACFIL
                  EXST=.FALSE.
                  INQUIRE(FILE=trim(LIBMAC)//FILNAM,EXIST=EXST)
                  IF(.NOT.EXST) THEN
                      WRITE(OUTLYNE,*)
     1                  'MACRO BODY FILE ',FILNAM,' MISSING'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'MACRO FILE DAMAGED, SEE REFERENCE MANUAL FOR'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'REPAIR PROCEDURE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1            'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
C
C       THE FIRST RECORD OF THE SOURCE MACRO IS AT RECORD 1
C       THAT WILL BE LINE 0
C       THEN IF WE WANT TO START READING FROM THE NW1 ST LINE
C       WE START BY READING FROM RECORD:
C               INT(NW1)+1
C       THE LAST LINE TO READ IS:
C               INT(NW2)+1
C       WE READ THIS DATA INTO ARRAY CELLS STARTIN AT
C               L=CURLIN
                  L=CURLIN
                  J=(1+NW1)
                  DO 300 K=J,(1+NW2)
C
                      READ(UNIT=30,REC=K) MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),
     1                MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),
     2                MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),
     3                MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),
     4                MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L)
     5                ,MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)
C
                      L=L+1
C
 300              CONTINUE
                  CALL CLOSE_FILE(30,1)
C       THE CURRENT LINE SHOULD NOW BE ADVANCED BY THE NUMBER OF
C       LINES EXTRACTED.
                  CURLIN=(NW2-NW1+2)
C       WE JUST FINISHED THE EXTRACTION INTO A MACRO THAT
C       CONSISTED OF ONE BLANK LINE ONLY
C***************************************************************
C
              ELSE
C
C       TARGET MACRO IS NOT BLANK OR MADE OF ONE BLANK LINE
C
C       NOW PROCEED WITH THE EXTRACTION. FIRST WE MUST
C       SHIFT LINES CURLIN+1 THROUGH MCDIR2(2,(MMIJ))-2
C       DOWN IN THE MACRO STORAGE ARRAY BY THE NUMBER OF LINES
C       TO BE EXTRACTED. THE NUMBER OF LINES TO BE EXTRACTED
C       WILL BE (NW2-NW1+1). LET US NOW SHIFT THE DATA AS WE DID
C       IN SUBROUTINE INSLIN
C
                  KK=(NW2-NW1+1)
                  I=(MCDIR2(2,(MMIJ))-2)
                  DO 201 J=I,(CURLIN+1),-1
                      MACCW(J+KK)=MACCW(J)
                      MACQW(J+KK)=MACQW(J)
                      MACSTR(J+KK)=MACSTR(J)
                      MACNW(1,J+KK)=MACNW(1,(J))
                      MACNW(2,J+KK)=MACNW(2,(J))
                      MACNW(3,J+KK)=MACNW(3,(J))
                      MACNW(4,J+KK)=MACNW(4,(J))
                      MACNW(5,J+KK)=MACNW(5,(J))
                      MACSTA(1,J+KK)=MACSTA(1,(J))
                      MACSTA(2,J+KK)=MACSTA(2,(J))
                      MACSTA(3,J+KK)=MACSTA(3,(J))
                      MACSTA(4,J+KK)=MACSTA(4,(J))
                      MACSTA(5,J+KK)=MACSTA(5,(J))
                      MACSTA(6,J+KK)=MACSTA(6,(J))
                      MACSTA(7,J+KK)=MACSTA(7,(J))
                      MACSTA(8,J+KK)=MACSTA(8,(J))
                      MACSTA(9,J+KK)=MACSTA(9,(J))
                      MACSTA(10,J+KK)=MACSTA(10,(J))
                      MACSTA(11,J+KK)=MACSTA(11,(J))
                      MACSTA(12,J+KK)=MACSTA(12,(J))
                      MACSTA(13,J+KK)=MACSTA(13,(J))
                      MACSTA(14,J+KK)=MACSTA(14,(J))
                      MACSTA(15,J+KK)=MACSTA(15,(J))
                      MACSTA(16,J+KK)=MACSTA(16,(J))
                      MACSTA(17,J+KK)=MACSTA(17,(J))
                      MACSTA(18,J+KK)=MACSTA(18,(J))
                      MACSTA(19,J+KK)=0
                      MACSTA(20,J+KK)=0
C       MACSTA(19,J) AND MACSTA(20,J) NOT YET USED
 201              CONTINUE
C       NOW INCREMENT THE LENGTH
                  MCDIR2(2,(MMIJ))=MCDIR2(2,(MMIJ))+KK
C
C       NOW READ THE APPROPRIATE LINES OF THE SOURCE MACRO
C       AND WRITE THEM INTO THE SPACE PROVIDED STARTING
C       AT POSITION (CURLIN+1) AND PROCEEDING TO
C       POSITION (CURLIN+1)+KK
C
C       OPEN UNIT 30 FOR I/O
C
                  NF=JK
                  CALL MACFIL
                  EXST=.FALSE.
                  INQUIRE(FILE=trim(LIBMAC)//FILNAM,EXIST=EXST)
                  IF(.NOT.EXST) THEN
                      WRITE(OUTLYNE,*)
     1                 'MACRO BODY FILE ',FILNAM,' MISSING'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'MACRO FILE DAMAGED, SEE REFERENCE MANUAL FOR'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'REPAIR PROCEDURE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1            'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
C
C       THE FIRST RECORD OF THE SOURCE MACRO IS AT RECORD 1
C       THAT WILL BE LINE 0
C       THEN IF WE WANT TO START READING FROM THE NW1 ST LINE
C       WE START BY READING FROM RECORD:
C               (1+NW1)
C       THE LAST LINE TO READ IS:
C       (1+NW2)
C       WE READ THIS DATA INTO ARRAY CELLS STARTIN AT
C               L=CURLIN+1
                  L=CURLIN+1
                  J=(1+NW1)
                  DO 30 K=J,(1+NW2)
C
                      READ(UNIT=30,REC=K) MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),
     1                MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),
     2                MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),
     3                MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),
     4                MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L)
     5                ,MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)
C
                      L=L+1
C
 30               CONTINUE
                  CALL CLOSE_FILE(30,1)
C       THE CURRENT LINE SHOULD NOW BE ADVANCED BY THE NUMBER OF
C       LINES EXTRACTED.
                  CURLIN=CURLIN+(NW2-NW1+1)
C       WE JUST FINISHED THE EXTRACTION
              END IF
              RETURN
          ELSE
C       NOT EX
          END IF
          IF(WC.EQ.'FL') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"FL" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       THE FL COMMAND IS THE ONLY WAY OUT OF THE MACRO CREATION
C       MODE. THIS SUBROUTINE CLOSES OFF MACRO EDITING,
C       SETS FLAG F3=0 AND F1=1 (REENABLING THE CMD LEVEL,
C       AND APPROPRIATELY FILING THE MACRO INTO THE MACRO LIBRARY
C       FILE MAC.DAT.
C
C       THERE IS ALWAYS A MACRO TO STORE
C
C       NOW STORE MACRO DIRECTORY INFORMATION
C       SET OCCUPANCY TO 1
              MCDIR2(3,(MMIJ))=1
              IF(MCDIR2(2,MMIJ).LE.3) THEN
                  MCDIR2(2,MMIJ)=3
                  MACCW(1)='C'
                  MACQW(1)=' '
                  MACSTR(1)='(COMMENT PLACE HOLDER)'
                  MACNW(1,1)=0.0D0
                  MACNW(2,1)=0.0D0
                  MACNW(3,1)=0.0D0
                  MACNW(4,1)=0.0D0
                  MACNW(5,1)=0.0D0
                  MACSTA(1,1)=0
                  MACSTA(2,1)=1
                  MACSTA(3,1)=1
                  MACSTA(4,1)=0
                  MACSTA(5,1)=0
                  MACSTA(6,1)=1
                  MACSTA(7,1)=0
                  MACSTA(8,1)=0
                  MACSTA(9,1)=0
                  MACSTA(10,1)=0
                  MACSTA(11,1)=0
                  MACSTA(12,1)=1
                  MACSTA(13,1)=1
                  MACSTA(14,1)=1
                  MACSTA(15,1)=1
                  MACSTA(16,1)=1
                  MACSTA(17,1)=0
                  MACSTA(18,1)=0
C       MACSTA(19,1) AND MACSTA(20,1) NOT YET USED
                  MACSTA(19,1)=0
                  MACSTA(20,1)=0
              END IF
C
C       GET A TIME AND DATE STAMP
              CALL MYDATE(DDATE)
              CALL MYTIME(TTIME)
              MCDIR3(MMIJ)=TTIME//'  '//DDATE
C
C       OPEN UNIT 20 FOR I/O
C

C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  OUTLYNE='"MACRO FILES DO NOT YET EXIST'
                  CALL SHOWIT(1)
                  OUTLYNE='TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1        'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
C
              WRITE(UNIT=20,REC=MMIJ) MCDIR1(MMIJ),MCDIR2(1,(MMIJ))
     1        ,MCDIR2(2,(MMIJ)),MCDIR2(3,(MMIJ)),MCDIR3(MMIJ)
C
C       FIND A FILE FOR THE MACRO BODY
C
              NF=MMIJ
              CALL MACFIL
C
              OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1        'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
C
C       NOW WRITE THE BODY OF THE MACRO STARTING AT RECORD 1
C
              DO 301 K=1,(MCDIR2(2,(MMIJ)))
                  L=K-1
                  IF(L.EQ.(MCDIR2(2,(MMIJ))-1)) THEN
                      MACCW(L)='EOM'
                      MACQW(L)=' '
                      MACSTR(L)=' '
                      MACNW(1,L)=0.0D0
                      MACNW(2,L)=0.0D0
                      MACNW(3,L)=0.0D0
                      MACNW(4,L)=0.0D0
                      MACNW(5,L)=0.0D0
                      MACSTA(1,L)=0
                      MACSTA(2,L)=1
                      MACSTA(3,L)=1
                      MACSTA(4,L)=0
                      MACSTA(5,L)=0
                      MACSTA(6,L)=0
                      MACSTA(7,L)=0
                      MACSTA(8,L)=0
                      MACSTA(9,L)=0
                      MACSTA(10,L)=0
                      MACSTA(11,L)=0
                      MACSTA(12,L)=1
                      MACSTA(13,L)=1
                      MACSTA(14,L)=1
                      MACSTA(15,L)=1
                      MACSTA(16,L)=1
                      MACSTA(17,L)=0
                      MACSTA(18,L)=0
C       MACSTA(19,L) AND MACSTA(20,L) NOT YET USED
                      MACSTA(19,L)=0
                      MACSTA(20,L)=0
                  ELSE
C       NOT AT EOM
                  END IF
                  WRITE(UNIT=30,REC=K) MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),
     1            MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),
     2            MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),
     3            MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),
     4            MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L)
     5            ,MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)
 301          CONTINUE
              CALL CLOSE_FILE(30,1)
              CALL CLOSE_FILE(20,1)
              KLI=0
              IF(MCDIR1(MMIJ).EQ.'FUN01   ')  KLI=1
              IF(MCDIR1(MMIJ).EQ.'FUN02   ')  KLI=2
              IF(MCDIR1(MMIJ).EQ.'FUN03   ')  KLI=3
              IF(MCDIR1(MMIJ).EQ.'FUN04   ')  KLI=4
              IF(MCDIR1(MMIJ).EQ.'FUN05   ')  KLI=5
              IF(MCDIR1(MMIJ).EQ.'FUN06   ')  KLI=6
              IF(MCDIR1(MMIJ).EQ.'FUN07   ')  KLI=7
              IF(MCDIR1(MMIJ).EQ.'FUN08   ')  KLI=8
              IF(MCDIR1(MMIJ).EQ.'FUN09   ')  KLI=9
              IF(MCDIR1(MMIJ).EQ.'FUN10   ')  KLI=10
C
              IF(KLI.NE.0) THEN
                  FUNEXT(KLI)=.TRUE.
                  FCDIR1(KLI)=MCDIR1(MMIJ)
                  FCDIR2(1,KLI)=MCDIR2(1,MMIJ)
                  FCDIR2(2,KLI)=MCDIR2(2,MMIJ)
                  FCDIR2(3,KLI)=MCDIR2(3,MMIJ)
                  DO 3010 K=1,(FCDIR2(2,KLI))
                      L=K-1
                      FUNCW(KLI,L)=MACCW(L)
                      FUNQW(KLI,L)=MACQW(L)
                      FUNSTR(KLI,L)=MACSTR(L)
                      FUNNW(KLI,1,L)=MACNW(1,L)
                      FUNNW(KLI,2,L)=MACNW(2,L)
                      FUNNW(KLI,3,L)=MACNW(3,L)
                      FUNNW(KLI,4,L)=MACNW(4,L)
                      FUNNW(KLI,5,L)=MACNW(5,L)
                      FUNSTA(KLI,1,L)=MACSTA(1,L)
                      FUNSTA(KLI,2,L)=MACSTA(2,L)
                      FUNSTA(KLI,3,L)=MACSTA(3,L)
                      FUNSTA(KLI,4,L)=MACSTA(4,L)
                      FUNSTA(KLI,5,L)=MACSTA(5,L)
                      FUNSTA(KLI,6,L)=MACSTA(6,L)
                      FUNSTA(KLI,7,L)=MACSTA(7,L)
                      FUNSTA(KLI,8,L)=MACSTA(8,L)
                      FUNSTA(KLI,9,L)=MACSTA(9,L)
                      FUNSTA(KLI,10,L)=MACSTA(10,L)
                      FUNSTA(KLI,11,L)=MACSTA(11,L)
                      FUNSTA(KLI,12,L)=MACSTA(12,L)
                      FUNSTA(KLI,13,L)=MACSTA(13,L)
                      FUNSTA(KLI,14,L)=MACSTA(14,L)
                      FUNSTA(KLI,15,L)=MACSTA(15,L)
                      FUNSTA(KLI,16,L)=MACSTA(16,L)
                      FUNSTA(KLI,17,L)=MACSTA(17,L)
                      FUNSTA(KLI,18,L)=MACSTA(18,L)
                      FUNSTA(KLI,19,L)=MACSTA(19,L)
                      FUNSTA(KLI,20,L)=MACSTA(20,L)
C
 3010             CONTINUE
              ELSE
C       NOT A FUNCTION
              END IF
              WRITE(OUTLYNE,*)
     1          'MACRO ',MCDIR1(MMIJ),' FILED'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
C       NOW MACRO HAS BEEN FILED
C       RESET F1 AND F3 AND RETURN
              F1=1
              F3=0
              RETURN
          END IF
          IF(WC.EQ.'GO') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1
     1        .OR.SST.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE=
     1            '"GO" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  OUTLYNE=
     1            '"GO" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       MGO CAN ONLY SET THE CURLIN BETWEEN
C       THE MINIMUM VALUE OF 1.
C       THE MAXIMUM VALUE OF EITHER:
C               MCDIR2(2,(MMIJ))-2
C               OR
C               MAXLL-2
C       WHICHEVER IS LESS.
C
              OCRLIN=CURLIN
              NCRLIN=INT(W1)
C
              IF(NCRLIN.LT.0) THEN
                  OUTLYNE='LINE NUMBER LOWER THAN 0 IS NOT ALLOWED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       NEW POSITION NOT TOO SMALL
                  IF(NCRLIN.GT.(MCDIR2(2,(MMIJ))-2)) THEN
                      OUTLYNE='LINE NUMBER BEYOND MACRO BOTTOM NOT ALLOWED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C       NEW POSITION MUST BE OK
C
              CURLIN=INT(W1)
C       NOW PRINT THE NEW CURRENT LINE IF IT IS DIFFERENT
C       FROM THE OLD CURRENT LINE.
              IF(CURLIN.EQ.(MCDIR2(2,(MMIJ))-2)) THEN
                  CALL MPR
                  RETURN
              ELSE
              END IF
              IF(CURLIN.EQ.0) THEN
                  OUTLYNE='LINE POINTER AT MACRO TOP'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(CURLIN.NE.OCRLIN) THEN
                  W1=0.0D0
C       SETS THE NUMBER OF LINES PRINTED TO THE CURRENT LINE
C       WITH NO ADVANCE TO THE LINE COUNT
                  CALL MPR
              ELSE
                  WRITE(OUTLYNE,*)'POSITION UNCHANGED AT LINE= ',CURLIN
                  CALL SHOWIT(1)
              END IF
              RETURN
          ELSE
C       NOT GO
          END IF
          IF(WC.EQ.'GOQUIET') THEN
C       MGO CAN ONLY SET THE CURLIN BETWEEN
C       THE MINIMUM VALUE OF 1.
C       THE MAXIMUM VALUE OF EITHER:
C               MCDIR2(2,(MMIJ))-2
C               OR
C               MAXLL-2
C       WHICHEVER IS LESS.
C
              OCRLIN=CURLIN
              NCRLIN=INT(W1)
C
              IF(NCRLIN.LT.0) THEN
                  RETURN
              ELSE
C       NEW POSITION NOT TOO SMALL
                  IF(NCRLIN.GT.(MCDIR2(2,(MMIJ))-2)) THEN
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C       NEW POSITION MUST BE OK
C
              CURLIN=INT(W1)
C       NOW PRINT THE NEW CURRENT LINE IF IT IS DIFFERENT
C       FROM THE OLD CURRENT LINE.
              IF(CURLIN.EQ.(MCDIR2(2,(MMIJ))-2)) THEN
                  RETURN
              ELSE
              END IF
              IF(CURLIN.EQ.0) THEN
                  RETURN
              END IF
              IF(CURLIN.NE.OCRLIN) THEN
                  W1=0.0D0
C       SETS THE NUMBER OF LINES PRINTED TO THE CURRENT LINE
C       WITH NO ADVANCE TO THE LINE COUNT
              ELSE
              END IF
              RETURN
          ELSE
C       NOT GOQUIET
          END IF
          IF(WC.EQ.'QU'.OR.WC.EQ.'QUIT') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"QU"/"QUIT" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       THIS SUBROUTINE ABORTS THE MACRO EDIT PROCESS (MEDIT) AND
C       RETURNS TO THE CMD LEVEL WITHOUT FILING THE CURRENT
C       MACRO.
C               SET FLAG F1=1 AND F3=0
              F1=1
              F3=0
              IF(F46.EQ.1) F46=0
              OUTLYNE=
     1        'MACRO EDIT ABORTED. RETURNING TO CMD LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'RE') THEN
              IF(SQ.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=
     1            '"RE" TAKES NO QUALIFIER OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(SST.EQ.0) THEN
                  OUTLYNE=
     1            '"RE" REQUIRES NON-BLANK STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       IF THE CURRENT LINE IS 0 THEN NO REPLACEMENT OCCURS
C       JUST PRINT LINE POINTER AT MACRO TOP
C
              IF(CURLIN.EQ.0) THEN
                  OUTLYNE='LINE POINTER AT MACRO TOP'
                  CALL SHOWIT(1)
                  OUTLYNE='NO REPLACEMENT OCCURED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
C
C       THIS SUBROUTINE IS USED TO REPLACE THE CONTENTS OF THE
C       CURRENT MACRO LINE WITH THE NEW MACRO LINE REPRESENTED
C       BY THE ALPHANUMERIC STRING FOLLOWING RE OR RE,.
C
C       THEREFORE TO REPLACE THE CURRENT LINE IN A MACRO
C       WITH (SET A 2)
C       WE TYPE EITHER:
C               RE,:SET A 2
C       OR      RE :SET A 2
C
C       REMEMBER AN ALPHANUMERIC STRING MUST BE DELIMITED
C       BY A : UNLESS IT IS THE SPECIAL INTERROGATOR (?).
C
C       CURRENT INSRUCTION LINE'S WS STRING VALUE BECOMES
C       THE NEW MACRO LINE.
C       THIS SUBROUTINE PASSES WS BACK TO THE PROCESS
C       SUBROUTINE  AS THE NEW VALUE OF THE VARIABLE (INPUT)
C       FOR REPROCESSING.  FLAG F47 IS SET HERE
C       WITH FLAG 47 SET, SUBROUTINE PROCESS DOES NOT CALL
C       SUBROUTINE CONTROL BUT CALLS SUBROUTINE MREA
C       FOR FUTHER PROCESSING. AFTER REPLACEMENT
C       FLAG 47 IS CLEARED TO ZERO.
C
              F47=1
              INPUT=WS
              CALL PROCES
C       NOW DO THE REPLACEMENT AT THE CURRENT LINE.
C       THE REPLACEMENT IS DONE BY SUBROUTINE MREA
              RETURN
          ELSE
C       NO RE
          END IF
          IF(WC.EQ.'TP') THEN
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE=
     1            '"TP" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       TP IS A VERY SIMPLE ROUTINE WHICH
C       CHANGES THE CURRENT LINE (CURLIN) FROM
C       ITS OLD VALUE TO LINE 0 OR THE MACRO TOP.
C
              OCRLIN=CURLIN
              NCRLIN=0
C
              CURLIN=0
              OUTLYNE='LINE POINTER AT MACRO TOP'
              CALL SHOWIT(1)
          END IF
      END
C SUB MMOD.FOR
      SUBROUTINE MMOD
C
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
C
C       THIS SUBROUTINE IS CALLED TO CALCULATE THE MODULO FUNCTION
C       USING THE ARGUMENTS W1 AND W2.
C
C       THE RESULT IS LEFT IN THE ACCUMULATOR.
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)'"MOD" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"MOD" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(W2.EQ.0.0D0) THEN
              REG(40)=REG(9)
              REG(9)=0.0D0
          ELSE
              REG(40)=REG(9)
              REG(9)=DMOD(W1,W2)
          END IF
C
          RETURN
      END
C SUB LENSSAVE_NOOPT.FOR
      SUBROUTINE LENSSAVE_NOOPT
          USE GLOBALS
C
          IMPLICIT NONE
C
          CHARACTER LFILENAME*80
C
          INTEGER WSCNT
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
          LFILENAME=""
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"LSAVE" SAVES THE CURRENT LENS IN AN ASCII FILE'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'WITHOUT SAVING OPTIMIZATION OR RAY DATA'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"LSAVE" TAKES NO NUMERIC OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SST.EQ.0) WS='LENS'
          WSCNT=LEN(TRIM(WS))
          WRITE(OUTLYNE,*) WSCNT,TRIM(WS)
          CALL SHOWIT(1)
          LFILENAME(1:LEN(TRIM(WS))+4)=TRIM(WS)//'.PRG'
          WSCNT=LEN(TRIM(LFILENAME))
C
C       ***************************************************************
C     DELETE CURRENT FILE
          OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE=TRIM(DIRLEN)//TRIM(LFILENAME)
     2      ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(97,0)
C
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='OUT FILE '//TRIM(DIRLEN)//TRIM(LFILENAME)
          CALL PROCES
          INPUT='LENO NOOPT'
          CALL PROCES
          INPUT='IN TP'
          CALL PROCES
          INPUT='OUT TP'
          CALL PROCES
C     RESTORE I/0
          REST_KDP(1)=RESTINPT(1)
          OUTLYNE=
     1    'CURRENT LENS SAVED AS:'//TRIM(DIRLEN)//TRIM(LFILENAME)
          CALL SHOWIT(1)
          RETURN
      END
C SUB LENSSAVE.FOR
      SUBROUTINE LENSSAVE
          USE GLOBALS
C
          IMPLICIT NONE
C
          CHARACTER LFILENAME*80
C
          INTEGER WSCNT
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          LFILENAME=""
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"LENSSAVE" SAVES THE CURRENT LENS IN AN ASCII FILE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"LENSSAVE" TAKES NO NUMERIC OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SST.EQ.0) WS='LENS'
          WSCNT=LEN(TRIM(WS))
          WRITE(OUTLYNE,*) WSCNT,TRIM(WS)
          CALL SHOWIT(1)
          LFILENAME(1:LEN(TRIM(WS))+4)=TRIM(WS)//'.PRG'
          WSCNT=LEN(TRIM(LFILENAME))
C
C       ***************************************************************
C     DELETE CURRENT FILE
          OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE=TRIM(DIRLEN)//TRIM(LFILENAME)
     2      ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(97,0)
C
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='OUT FILE '//TRIM(DIRLEN)//TRIM(LFILENAME)
          CALL PROCES
!        WRITE(OUTLYNE,*) INPUT(1:79)
!        CALL SHOWIT(1)
          INPUT='LENO'
          CALL PROCES
          INPUT='IN TP'
          CALL PROCES
          INPUT='OUT TP'
          CALL PROCES
C     RESTORE I/0
          REST_KDP(1)=RESTINPT(1)
          OUTLYNE=
     1    'CURRENT LENS SAVED AS:'//TRIM(DIRLEN)//TRIM(LFILENAME)
          CALL SHOWIT(1)
          RETURN
      END
C SUB LENSREST.FOR
      SUBROUTINE LENSREST
C
          IMPLICIT NONE
C
          CHARACTER LFILENAME*80
C
          INTEGER WSCNT
C
          LOGICAL EXISJK
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'

          LFILENAME=""
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"LENSREST" RESTORES THE CURRENT LENS FROM AN ASCII FILE'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"LENSREST" TAKES NO NUMERIC OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.0) THEN
              OUTLYNE=
     1        '"LENSREST" REQUIRES A FILE NAME'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C
          WSCNT=LEN(TRIM(WS))
          WRITE(OUTLYNE,*) WSCNT,TRIM(WS)
          CALL SHOWIT(1)
          LFILENAME(1:LEN(TRIM(WS))+4)=TRIM(WS)//'.PRG'
          WSCNT=LEN(TRIM(LFILENAME))
C
C       ***************************************************************
C     DOES THE FILE EXIST?
          EXISJK=.FALSE.
          INQUIRE(FILE=TRIM(DIRLEN)//TRIM(LFILENAME),EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE=
     1        'LENS FILE NAMED '//TRIM(LFILENAME)//' DOES NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE='NO LENS RESTORATION WAS PERFORMED'
              CALL SHOWIT(1)

              CALL MACFAL
              RETURN
          END IF

C      INPUT LENS
C
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='IN TP'
          CALL PROCES
          INPUT='OUT TP'
          CALL PROCES
          INPUT='IN FILE '//TRIM(DIRLEN)//TRIM(LFILENAME)
          write(6,*) TRIM(DIRLEN)//TRIM(LFILENAME)
          CALL PROCES
          INPUT='IN TP'
          CALL PROCES
          INPUT='OUT TP'
          CALL PROCES
C     RESTORE I/0
          REST_KDP(1)=RESTINPT(1)
          OUTLYNE=
     1    'LENS SAVED AS: '//TRIM(LFILENAME)//' HAS BEEN RESTORED'
          CALL SHOWIT(1)
          RETURN
      END
C SUB MACSAV.FOR
      SUBROUTINE MACSAV
C
          IMPLICIT NONE
C
!      CHARACTER FILECONTENTS*140
!      INTEGER I
C
          LOGICAL EXISJK
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"MACSAVE" SAVES THE CURRENT MACRO LIBRARY FILES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"MACSAVE" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"MACRO FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
C
          OUTLYNE=
     1    'PLEASE WAIT WHILE THE CURRENT MACRO DIRECTORY IS SAVED...'
          CALL SHOWIT(1)
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='OUT FILE MACSAV.DAT'
          CALL PROCES
          INPUT='MFL'
          CALL PROCES
          INPUT='OUT TP'
          CALL PROCES
          OUTLYNE=
     1    'THE CURRENT MACRO DIRECTORY HAS BEEN SAVED TO MACSAV.DAT'
          CALL SHOWIT(1)
          RETURN
      END
C SUB MACRES.FOR
      SUBROUTINE MACRES
C
          IMPLICIT NONE
C
!      CHARACTER FILECONTENTS*140
!      INTEGER I
C
          LOGICAL EXISJK,OPENJK
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"MACREST" RESTORES THE CURRENT MACRO LIBRARY FILES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"MACREST" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       ***************************************************************
          EXISJK=.FALSE.
          OPENJK=.FALSE.
          INQUIRE(FILE=trim(HOME)//'MACSAV.DAT',EXIST=EXISJK)
          INQUIRE(FILE=trim(HOME)//'MACSAV.DAT',OPENED=OPENJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"MACSAV.DAT" DOES NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE='NO SAVED MACRO DIRCETORY EXISTS TO BE RESTORED'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(OPENJK) THEN
              OUTLYNE='"MACSAV.DAT" HAS NOT CLOSED YET'
              CALL SHOWIT(1)
              OUTLYNE='WAIT A FEW SECONDS FOR IT TO CLOSE, THEN RE-ISSUE'
              CALL SHOWIT(1)
              OUTLYNE='THE "MACREST" COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OUTLYNE=
     1    'PLEASE WAIT WHILE THE CURRENT MACRO DIRECTORY IS RESTORED...'
          CALL SHOWIT(1)
C
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='IMF'
          CALL PROCES
          INPUT='PROCEED'
          CALL PROCES
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='INPUT FILE MACSAV.DAT'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='INPUT TP'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          OUTLYNE=
     1    'CURRENT MACRO DIRECTORY RESTORED FROM "MACSAV.DAT"'
          CALL SHOWIT(1)
          RETURN
      END
C SUB LIBSAV.FOR
      SUBROUTINE LIBSAV
C
          IMPLICIT NONE
C
          CHARACTER AI*3
C
          INTEGER I,II
C
          LOGICAL EXISJK,GETERROR
C
          COMMON/ERRORGET/GETERROR
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"LIBSAVE" SAVES THE CURRENT LENS LIBRARY FILES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"LIBSAVE" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=LIBLEN//'LIB.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"LENS LIBRARY DO NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='TO INITIALIZE IT, USE "ILF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
C     DELETE CURRENT LIBSAV.DAT
          OPEN(UNIT=31,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'LIBSAV.DAT'
     2    ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(31,0)
          IF(ALLSTOP) THEN
              ALLSTOP=.FALSE.
              RETURN
          END IF
C
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='OUT FILE LIBSAV.DAT'
          CALL PROCES
          DO I=1,999
              II=I
              CALL NTOAN1(II,AI)
              INPUT='LIB GET '//AI
              CALL PROCES
              IF(.NOT.GETERROR) THEN
                  INPUT='LENO'
                  CALL PROCES
                  INPUT='M LIB PUT'
                  CALL PROCES
              END IF
          END DO
          CALL CLOSE_FILE(31,1)
          INPUT='OUT TP'
          CALL PROCES
C     RESTORE I/0
          REST_KDP(1)=RESTINPT(1)
          OUTLYNE=
     1    'CURRENT LENS LIBRARY DIRECTORY SAVED IN "LIBSAV.DAT"'
          CALL SHOWIT(1)
          RETURN
      END
C SUB LIBRES.FOR
      SUBROUTINE LIBRES
C
          IMPLICIT NONE
C
C
!      CHARACTER FILECONTENTS*140
C
!      INTEGER I
          LOGICAL EXISJK
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"LIBREST" SAVES THE CURRENT LENS LIBRARY FILES'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"LIBREST" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(HOME)//'LIBSAV.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"LIBSAV.DAT" DOES NOT EXIST'
              CALL SHOWIT(1)
              OUTLYNE='NO SAVED LENS LIBRARY DIRCETORY EXISTS TO BE RESTORED'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
C     DELETE CURRENT EDITTEXT.DAT
          OPEN(UNIT=9,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'EDITTEXT.DAT'
     2    ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(9,0)
          CALL CLOSE_FILE(31,1)
C
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='ILF'
          CALL PROCES
          INPUT='PROCEED'
          CALL PROCES
C     RESTORE I/I
          REST_KDP(1)=RESTINPT(1)
          OPEN(UNIT=31,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'LIBSAV.DAT'
     2    ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(31,1)
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='INPUT FILE LIBSAV.DAT'
          CALL PROCES
C     RESTORE I/I
          REST_KDP(1)=RESTINPT(1)
          OUTLYNE=
     1    'CURRENT LENS LIBRARY DIRECTORY RESTORED FROM "LIBSAV.DAT"'
          CALL SHOWIT(1)
          RETURN
      END
C SUB MMFLN.FOR
      SUBROUTINE MMFLN
C
          IMPLICIT NONE
C
          INTEGER NKEY,LINCUR,LINBOT,I,J
C
          LOGICAL EXISJK
C
          REAL*8 KEY
C
          CHARACTER COMM_ARRAY*40
C
          DIMENSION COMM_ARRAY(:)
          ALLOCATABLE :: COMM_ARRAY
C
          INTEGER AKK,ALLOERR
C
          COMMON/STAT13/LINCUR,LINBOT
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C       THIS SUBROUTINE IS USED TO LIST THE CONTENTS
C       OF THE MACRO DIRECTORY. THE MACRO NAMES ARE PRINTED
C       FOR EACH MACRO FOR WHICH THE MACRO KEY MATCHES THE MACRO LOCK.
C       IF THE SPECIAL KEY (-1) IS USED, ALL MACRO NAMES
C       ARE LISTED.
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"MFLC" ONLY TAKES QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C
C       OPEN UNIT 20 FOR I/O
C
          KEY=0.0D0
          NKEY=INT(KEY)
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"MACRO FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
C
C       INITIALIZE ASORT TO ALL BLANKS
C
          ASORT(1:MAXMAC)='        '
C
          J=0
C       J COUNTS THE NUMBER OF NON-BLANK ENTRIES IN THE DIRECTORY.
          DO 10 I=1,MAXMAC
              READ(UNIT=20,REC=I,ERR=10)MCDIR1(I),MCDIR2(1,I),MCDIR2(2,I),
     1        MCDIR2(3,I),MCDIR3(I)
              IF(MCDIR2(3,(I)).EQ.1) J=J+1
 10       CONTINUE
C
C       CLOSE UNIT 20 TO I/O
C
          CALL CLOSE_FILE(20,1)
C
C       LOOK FOR MCDIR2(3,(I))=1 AND MATCHING LOCKS OR LOCK = -1
C     NOW GET THE LEADING COMMENT IN EVERY MACRO AND STORE THEM IN
C     THE COMMENT ARRAY COM_ARRAY
          ALLOCATE(COMM_ARRAY(1:MAXMAC),STAT=ALLOERR)
C
          AKK=0
          DO 20 I=1,MAXMAC
              IF(MCDIR2(3,(I)).EQ.1.OR.
     1        MCDIR2(3,(I)).EQ.1) THEN
                  AKK=AKK+1
                  ASORT(AKK)=MCDIR1(I)
                  SAVE_KDP(18)=SAVEINPT(18)
                  WQ=TRIM(ASORT(AKK))
                  CALL MMFLC1
                  REST_KDP(18)=RESTINPT(18)
                  AISORT(AKK)=' '//MCDIR3(I)//' '//MAC_COM1
              END IF
 20       CONTINUE
C       NOW SORT THE MACRO NANES
          CALL SORTA(AKK)
C       NOW PRINT THEM
          WRITE(OUTLYNE,50)
          CALL SHOWIT(0)
  50      FORMAT('MACRO FILE CONTENTS')
          WRITE(OUTLYNE,100)
          CALL SHOWIT(0)
 100      FORMAT('NAME      TIME AND DATE        DESCRIPTION')
          DO I=1,AKK
              WRITE(OUTLYNE,200) ASORT(I)(1:8),AISORT(I)(1:70)
              CALL SHOWIT(0)
          END DO
 200      FORMAT(A8,1X,A70)
          DEALLOCATE(COMM_ARRAY,STAT=ALLOERR)
          RETURN
      END
C SUB MMFLC.FOR
      SUBROUTINE MMFLC
C
          IMPLICIT NONE
C
          INTEGER NKEY,NF,IFF,
     1    I
C
          COMMON/IFFY/IFF
C
          CHARACTER NAME*8
C
          LOGICAL EXISJK
C
          COMMON/NEFER/NF
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C       THIS SUBROUTINE IS USED TO OUTPUT THE LEADING COMMENT
C       LINES OF A MACRO.
C
C       THE MACRO IS READ FROM THE FILE MAC.DAT AND ITS COMMENTS
C       ARE WRITTEN TO THE CURRENT OUTPUT DEVICE.
C       MAC.DAT WILL ALWAYS BE UNIT=20.
C
          IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"MFLC" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE=
     1        '"MFLC" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          NAME=WQ
          NKEY=0
C
C       IF THE MACRO WITH ITS KEY IS NOT FOUND, AS MESSAGE WILL BE
C       PRINTED TO THE DISPLAY.
C
C
C       READ THE CURRENT MACRO DIRECTORY
C
C       OPEN UNIT 20 FOR I/O
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"MACRO FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
C
          DO 10 I=1,MAXMAC
              READ(UNIT=20,REC=I,ERR=10)MCDIR1(I),MCDIR2(1,I),MCDIR2(2,I),
     1        MCDIR2(3,I),MCDIR3(I)
C
C       THE INITIAL VALUES STORED IN MAC.DAT ARE:
C
C               MCDIR1(I) IS THE MACRO NAME
C               MCDIR2(1,I) IS THE MACRO LOCK
C               MCDIR2(2,I) IS THE MACRO LENGTH
C               MCDIR2(3,I) IS THE OCCUPANCY FLAG
C                               = 0 FOR BLANK MACRO
C                               = 1 FOR NON-BLANK MACRO
C
C

 10       CONTINUE
          CALL CLOSE_FILE(20,1)
C
C       SEARCHES OF THIS MACRO DIRECTORY ARE NOW MADE TO DETERMINE
C       IF THE MACRO REQUESTED WITH ITS KEY IS ON FILE.
C
C       NOW DETERMINE A LOCATION OF THE MACRO.
C       SEARCH THE DIRECTORY FOR THE MACRO TO BE OUTPUT
C
          IFF=0
C
C       IF NAME IS NON-BLANK WE CONTINUE.
C
          DO 20 I=1,MAXMAC
              IF(MCDIR1(I).EQ.NAME.AND.NKEY.EQ.MCDIR2(1,I).OR.
     1        MCDIR1(I).EQ.NAME) THEN
C
C       WE FOUND THE MACRO CALLED FOR. SET NF=I
                  NF=I
                  CALL MACCOM
                  IF(HT) RETURN
C       MACCOM IS THE SUBROUTINE WHICH ACTUALLY OUTPUTS
C       THE COMMENTS OF THE NF TH MACRO IN MAC.DAT.
C       MFLC ONLY WORKS ON THE FIRST OCCURENCE OF A MACRO
C       NAME AND KEY COMBINATION.
                  RETURN
              ELSE
              END IF
 20       CONTINUE
C
      END
C SUB MMFLC1.FOR
      SUBROUTINE MMFLC1
C
          IMPLICIT NONE
C
          INTEGER NKEY,NF,IFF,
     1    I
C
          COMMON/IFFY/IFF
C
          CHARACTER NAME*8
C
          LOGICAL EXISJK
C
          COMMON/NEFER/NF
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C       THIS SUBROUTINE IS USED TO OUTPUT THE LEADING COMMENT
C       LINES OF A MACRO.
C
C       THE MACRO IS READ FROM THE FILE MAC.DAT AND ITS COMMENTS
C       ARE WRITTEN TO THE CURRENT OUTPUT DEVICE.
C       MAC.DAT WILL ALWAYS BE UNIT=20.
C
C
          NAME=WQ
          NKEY=0
C
C       IF THE MACRO WITH ITS KEY IS NOT FOUND, AS MESSAGE WILL BE
C       PRINTED TO THE DISPLAY.
C
C
C       READ THE CURRENT MACRO DIRECTORY
C
C       OPEN UNIT 20 FOR I/O
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
C
          DO 10 I=1,MAXMAC
              READ(UNIT=20,REC=I,ERR=10)MCDIR1(I),MCDIR2(1,I),MCDIR2(2,I),
     1        MCDIR2(3,I),MCDIR3(I)
C
C       THE INITIAL VALUES STORED IN MAC.DAT ARE:
C
C               MCDIR1(I) IS THE MACRO NAME
C               MCDIR2(1,I) IS THE MACRO LOCK
C               MCDIR2(2,I) IS THE MACRO LENGTH
C               MCDIR2(3,I) IS THE OCCUPANCY FLAG
C                               = 0 FOR BLANK MACRO
C                               = 1 FOR NON-BLANK MACRO
C
C

 10       CONTINUE
          CALL CLOSE_FILE(20,1)
C
C       SEARCHES OF THIS MACRO DIRECTORY ARE NOW MADE TO DETERMINE
C       IF THE MACRO REQUESTED WITH ITS KEY IS ON FILE.
C
C       NOW DETERMINE A LOCATION OF THE MACRO.
C       SEARCH THE DIRECTORY FOR THE MACRO TO BE OUTPUT
C
          IFF=0
C
C       IF NAME IS NON-BLANK WE CONTINUE.
C
          DO 20 I=1,MAXMAC
              IF(MCDIR1(I).EQ.NAME.AND.NKEY.EQ.MCDIR2(1,I).OR.
     1        MCDIR1(I).EQ.NAME) THEN
C
C       WE FOUND THE MACRO CALLED FOR. SET NF=I
                  NF=I
                  CALL MACCOM1
                  IF(HT) RETURN
C       MACCOM IS THE SUBROUTINE WHICH ACTUALLY OUTPUTS
C       THE COMMENTS OF THE NF TH MACRO IN MAC.DAT.
C       MFLC ONLY WORKS ON THE FIRST OCCURENCE OF A MACRO
C       NAME AND KEY COMBINATION.
                  RETURN
              ELSE
              END IF
 20       CONTINUE
C
      END
C SUB MMFL.FOR
      SUBROUTINE MMFL
C
          IMPLICIT NONE
C
          LOGICAL EXISJK
C
          INTEGER NKEY,NF,I
C
          COMMON/NEFER/NF
C
          CHARACTER NAME*8
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'

C
C       THIS SUBROUTINE IS USED TO OUTPUT THE CONTENTS OF A
C       MACRO IN A FORMAT MUCH THE SAME AS THE FORMAT INWHICH
C       THE MACRO WAS INITIALLY INPUT AT THE KEYBOARD.
C       THE MACRO IS READ FROM THE FILE MAC.DAT AND IS
C       WRITTEN TO THE CURRENT OUTPUT DEVICE.
C       MAC.DAT WILL ALWAYS BE UNIT=20.
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"MFL" ONLY TAKES QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL

              RETURN
          END IF
C
          NAME=WQ
          NKEY=0
C
C       IF THE MACRO WITH ITS KEY IS NOT FOUND, AS MESSAGE WILL BE
C       PRINTED TO THE DISPLAY.
C
C
C       READ THE CURRENT MACRO DIRECTORY
C
C       OPEN UNIT 20 FOR I/O
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"MACRO FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
C
          DO 10 I=1,MAXMAC
              READ(UNIT=20,REC=I,ERR=10)MCDIR1(I),MCDIR2(1,I),MCDIR2(2,I),
     1        MCDIR2(3,I),MCDIR3(I)
C
C
C
C       THE INITIAL VALUES STORED IN MAC.DAT ARE:
C
C               MCDIR1(I) IS THE MACRO NAME
C               MCDIR2(1,I) IS THE MACRO LOCK
C               MCDIR2(2,I) IS THE MACRO LENGTH
C               MCDIR2(3,I) IS THE OCCUPANCY FLAG
C                               = 0 FOR BLANK MACRO
C                               = 1 FOR NON-BLANK MACRO
C
C

 10       CONTINUE
          CALL CLOSE_FILE(20,1)
C
C       SEARCHES OF THIS MACRO DIRECTORY ARE NOW MADE TO DETERMINE
C       IF THE MACRO REQUESTED WITH ITS KEY IS ON FILE.
C
C       NOW DETERMINE A LOCATION OF THE MACRO.
C       SEARCH THE DIRECTORY FOR THE MACRO TO BE OUTPUT
C
C       IF NAME= '        ' (BLANK) THEN ALL THE NON-BLANK
C       MACROS WILL BE OUTPUT
C
C       IF NAME IS NON-BLANK THEN THE KEY MUST MATCH THE MACRO LOCK
C
          IF(NAME.EQ.'        ') GO TO 1000
C       THAT IS WHERE THE ENTIRE CONTENTS OF MAC.DAT ARE OUTPUT
C       IF NAME IS NON-BLANK WE CONTINUE.
C
          DO 20 I=1,MAXMAC

              IF(MCDIR1(I).EQ.NAME.AND.NKEY.EQ.MCDIR2(1,I).OR.
     1        MCDIR1(I).EQ.NAME.AND.NKEY.EQ.-1) THEN
C
C       WE FOUND THE MACRO CALLED FOR. SET NF=I
                  NF=I
                  CALL MACOUT
C       MACOUT IS THE SUBROUTINE WHICH ACTUALLY OUTPUTS
C       THE CONTENTS OF THE NF TH MACRO IN MAC.DAT.
C       MFL ONLY WORKS ON THE FIRST OCCURENCE OF A MACRO
C       NAME AND KEY COMBINATION.
                  RETURN
              ELSE
              END IF
 20       CONTINUE
          WRITE(OUTLYNE,*) '"',TRIM(NAME),'" IS NOT A VALID MACRO NAME'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*) 'NO MACRO LISTING IS POSSIBLE'
          CALL SHOWIT(1)
          RETURN
C
C       HERE IS WHERE EVERYTHING GETS OUTPUT IF MFL(KEY) OR MFL(-1)
C       IS ISSUED.

 1000     DO 2000 I=1,MAXMAC
              IF(MCDIR2(3,I).EQ.1.AND.MCDIR2(1,I).EQ.NKEY.OR.
     1        MCDIR2(3,I).EQ.1.AND.MCDIR2(1,I).EQ.-1) THEN
                  NF=I
                  CALL MACOUT
              ELSE
C       DON'T CALL MACOUT
              END IF
 2000     CONTINUE
          RETURN
      END


C SUB MMEDIT.FOR
      SUBROUTINE MMEDIT
C
          IMPLICIT NONE
C
          CHARACTER STAMP*20
C
          LOGICAL EXISJK
C
          INTEGER I,J,HAPPY
          CHARACTER HAPPYNAME*8
          SAVE HAPPY
          SAVE HAPPYNAME
C
C       THIS IS THE SUBROUTINE WHICH CONTROLS THE MACRO EDIT OR
C       LMEDIT OPERATION.
C       THE OPERATION HERE IS SIMILAR TO THE MACRO INPUT MODE BUT
C       ALSO HAS THE CAPABILITY TO MOVE AROUND INSIDE A MACRO
C       AND MAKE CHANGES TO THAT MACRO.
C
C       SUBROUTINE FLMAC IS THE ONLY WAY BACK TO THE
C       MAIN PROGRAM CMD LEVEL. WQ IS THE NEW MACRO NAME.
C       MACRO NAMES MUST NOT BE THE SAME AS PROGRAM COMMAND
C       NAMES AS IF THEY ARE, THE MACRO WILL NEVER BE ABLE
C       TO BE EXECUTED OR DELETED WITHOUT EXTERNAL INTERVENTION.
C
          CHARACTER MNAME*8
C
C       W1 IS USED TO PASS THE MACRO LOCK IF ONE IS DESIRED
C
          INTEGER NKEY
     1    ,M1,M2,M3,NEXTM3,MMIJ,FLAGA

          COMMON/MIJ/MMIJ
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"MACRO FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
C       SET FLAGS
          F1=0
          F3=1
C
C       CHECK FOR NO MACRO NAME
C
          IF(WQ.EQ.' ') THEN
              OUTLYNE='LMEDIT MUST BE FOLLOWED BY A MACRO NAME'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              F1=1
              F3=0
              RETURN
          END IF
C
C       OPEN UNIT 20 FOR I/O
C
          NKEY=0
C
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
          J=0
          M3=0
          FLAGA=0
          DO 10 I=1,MAXMAC
              READ(UNIT=20,REC=I,ERR=10) MNAME,M1,M2,NEXTM3,STAMP
              M3=M3+NEXTM3
              IF(WQ.EQ.MNAME.AND.NKEY.EQ.M1) THEN
                  J=1
                  MMIJ=I
                  FLAGA=1
              ELSE
                  IF(WQ.EQ.MNAME.AND.NKEY.NE.M1) THEN
                      J=1
                      MMIJ=1
                      FLAGA=0
                  ELSE
                  END IF
              END IF
 10       CONTINUE
C               CLOSE UNIT 20
          CALL CLOSE_FILE(20,1)
C       CHECK FOR FULL DIRECTORY
C
          IF(M3.GE.MAXMAC) THEN
C       DIRECTORY FULL
              OUTLYNE='MACRO DIRECTORY FULL. RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              F1=1
              F3=0
              RETURN
          END IF
C
C       CHECK FOR MACRO ALREADY ON FILE.
          HAPPY=0
C
          IF(J.EQ.0) THEN
              MMIJ=0
              HAPPY=1
              HAPPYNAME=WQ
          ELSE
C       WE ARE CHANGING AN EXISTING MACRO AND IT IS THE MMIJ TH
C       MACRO IN THE DIRECTORY.
          END IF
C
C       DURING MACRO EDITING, EACH PROGRAM INSTRUCTION
C       ENTERED IS PROCESSED BY SUBROUTINE PROCESS
C       THEN PASSED BY SUBROUTINE CONTROL TO SUBROUTINE
C       MACMOD WHERE EACH COMMAND IS EITHER ACTED UPON IF IT IS
C       A MEDIT SPECIFIC COMMAND OR IS ENTERED AS A NEW LINE IN THE
C       MACRO IF IT IS NOT A MEDIT SPECIFIC COMMAND.
C
C       WHEN FL IS ENTERED, THE MACRO STORAGE ARRAYS
C       ARE WRITTEN TO THE MACRO FILE MAC.DAT.
C       THE RECORDS ARE WRITTEN DIRECT AND UNFORMATTED.
C       A DIRECTORY OF MACRO NAMES IS WRITTEN IN THE FIRST
C       MAXMAC ENRTIES OF THIS MACRO FILE.
C
          CALL MACEDT
          RETURN
      END


C SUB FSMEDIT.FOR
      SUBROUTINE FSMEDIT
C
          IMPLICIT NONE
C
!      INTEGER I
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          LOGICAL EXIS86,OPEN86
C
C       CHECK FOR NO MACRO NAME
C
          IF(WQ.EQ.' ') THEN
              OUTLYNE='MEDIT MUST BE FOLLOWED BY A MACRO NAME'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE='MEDIT TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='MEDIT TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          EXIS86=.FALSE.
          OPEN86=.FALSE.
          INQUIRE(FILE=trim(HOME)//'EDITTING.DAT',EXIST=EXIS86)
          INQUIRE(FILE=trim(HOME)//'EDITTING.DAT',OPENED=OPEN86)
          IF(EXIS86) THEN
              OUTLYNE=
     1        'THE FULL SCREEN MACRO EDITOR IS IN USE. FILE THE MACRO BEING'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'EDITTED BEFORE EDITTING ANOTHER MACRO IN FULL SCREEN MODE.'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          SAVE_KDP(8)=SAVEINPT(8)
          INPUT='OUT NULL'
          CALL PROCES
          REST_KDP(8)=RESTINPT(8)
C
          SAVE_KDP(8)=SAVEINPT(8)

          INPUT='LMEDIT '//TRIM(WQ)
          CALL PROCES
          REST_KDP(8)=RESTINPT(8)
C
          SAVE_KDP(8)=SAVEINPT(8)
          INPUT='FL'
          CALL PROCES
          REST_KDP(8)=RESTINPT(8)
C
          SAVE_KDP(8)=SAVEINPT(8)
          INPUT='OUT FILE MAC_EDIT.DAT'
          CALL PROCES
          REST_KDP(8)=RESTINPT(8)
C
          SAVE_KDP(8)=SAVEINPT(8)
          INPUT='MFL '//TRIM(WQ)
          CALL PROCES
          REST_KDP(8)=RESTINPT(8)
C
          SAVE_KDP(8)=SAVEINPT(8)
          INPUT='OUT TP'
          CALL PROCES
          REST_KDP(8)=RESTINPT(8)
C
          SAVE_KDP(8)=SAVEINPT(8)
          INPUT='MAC_EDIT MAC_EDIT.DAT'
          CALL PROCES
          REST_KDP(8)=RESTINPT(8)
C
          SAVE_KDP(8)=SAVEINPT(8)
          INPUT='OUT TP'
          CALL PROCES
          REST_KDP(8)=RESTINPT(8)
C
          SAVE_KDP(8)=SAVEINPT(8)
          INPUT='IN TP'
          CALL PROCES
          REST_KDP(8)=RESTINPT(8)

          RETURN
      END


C SUB MMDEL.FOR
      SUBROUTINE MMDEL
C
          IMPLICIT NONE
C
          CHARACTER STAMP*20
C
          INTEGER KLI,I
C
          LOGICAL EXST
C
C       THIS SUBROUTINE IS USED TO DELETE A MACRO
C
          INTEGER OCC,MACLOC,IMACEN,KEY,NF
C
          CHARACTER MACNAM*8,NAME*8,
     1    FILNAM*10
C
          LOGICAL EXISJK
C
          COMMON/NEFER/NF
C
          COMMON/COMNAM/FILNAM
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C       THIS SUBROUTINE IS USED TO DELETE A NEW
C       MACRO. THE MACRO IN THE FILE MAC.DAT IS REMOVED.
C       MAC.DAT WILL ALWAYS BE UNIT=20.
C       THE MACRO DIRECTORY WILL BE UPDATED WHEN A MACRO IS
C       DELETED.
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE=
     1        '"MDEL" ONLY TAKES QUALIFIER'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE=
     1        '"MDEL" REQUIRES A NON-BLANK MACRO NAME QUALIFIER WORD'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          NAME=WQ
          KEY=0
C       OPEN UNIT 20 FOR I/O
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"MACRO FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       PROCEED
          END IF
C       ***************************************************************
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
C
          DO 10 I=1,MAXMAC
              READ(UNIT=20,REC=I,ERR=10) MACNAM,MACLOC,IMACEN,OCC,STAMP
C
C       MACNAN IS THE MACRO NAME, MACLOC IS THE MACRO LOCK,
C       IMACEN IS THE MACRO LENGTH IN LINES, AND OCC IS A FLAG
C       =0 IF THE RECORD REPRESENTS A BLANK MACRO AND =1
C       REPRESENTS A NON-BLANK MACRO.
C
C       TEST IF THE ITH MACRO DIRECTORY ENTRY MATCHES THE
C       MACRO TO BE DELETED. THE MATCH MUST BE MADE FOR
C       THE MACRO NAME AND THE MACRO LOCK.
C
              IF(MACNAM.EQ.NAME.AND.MACLOC.EQ.KEY) THEN
C       THE MACRO TO BE DELETED HAS BEEN FOUND
C       WRITE THE ITH RECORD OF THE DIRECTORY WITH:
C       MACRO NAME = 'BLANK',LOCK=0,LENGTH=0,AND,OCC=0
C       THEN FILL THE APPROPRIATE MAXLL RECORDS WITH APPRORIATE
C       ZEROS AND BLANKS.
C
                  WRITE(UNIT=20,REC=I) 'BLANK',0,0,0
                  KLI=0
                  IF(MACNAM.EQ.'FUN01   ')  KLI=1
                  IF(MACNAM.EQ.'FUN02   ')  KLI=2
                  IF(MACNAM.EQ.'FUN03   ')  KLI=3
                  IF(MACNAM.EQ.'FUN04   ')  KLI=4
                  IF(MACNAM.EQ.'FUN05   ')  KLI=5
                  IF(MACNAM.EQ.'FUN06   ')  KLI=6
                  IF(MACNAM.EQ.'FUN07   ')  KLI=7
                  IF(MACNAM.EQ.'FUN08   ')  KLI=8
                  IF(MACNAM.EQ.'FUN09   ')  KLI=9
                  IF(MACNAM.EQ.'FUN10   ')  KLI=10
C
                  IF(KLI.NE.0) THEN
                      FUNEXT(KLI)=.FALSE.
                      FCDIR1(KLI)='BLANK'
                      FCDIR2(1:3,KLI)=0
                  ELSE
C       NOT A FUNCTION
                  END IF
C
C
C       NOW THE FILE CONTAINING THE MACRO BODY IS DELETED
                  NF=I
                  CALL MACFIL
                  EXST=.FALSE.
                  INQUIRE(FILE=trim(LIBMAC)//FILNAM,EXIST=EXST)
                  IF(.NOT.EXST) THEN
                      WRITE(OUTLYNE,*)'MACRO BODY FILE ',FILNAM,' MISSING'
                      CALL SHOWIT(1)
                      OUTLYNE='MACRO NAME ENTRY WILL BE DELETED'
                      CALL SHOWIT(1)
                  END IF
                  OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1            'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
                  CALL CLOSE_FILE(30,0)
                  WRITE(OUTLYNE,*)'MACRO ',NAME,' DELETED'
                  CALL SHOWIT(1)
                  CALL CLOSE_FILE(20,1)
                  RETURN
              END IF
 10       CONTINUE
          CALL CLOSE_FILE(20,1)
C
C       IF YOU GOT HERE YOU DID NOT FIND A MATCH.
          IF(IN.EQ.5.AND.OUT.EQ.6) THEN
              WRITE(OUTLYNE,*)
     1          'MACRO ',NAME,' NOT FOUND'
              CALL SHOWIT(1)
          END IF
          RETURN
      END
C SUB MLO.FOR
      SUBROUTINE MLO
C
          IMPLICIT NONE
C
C       THE FORMAT FOR THE PROGRAM COMMAND IS:
C
C               LO (C OR Q OR CQ OR COQ),: TARGET STRING
C       OR      LO (C OR Q OR CQ OR COQ) : TARGET STRING
C
C       THIS SUBROUTINE IS USED TO LOCATE A MACRO LINE
C       WITH A SPECIFIC COMMAND WORD AND OR QUALIFIER WORD
C       OR A COMMAND WORD OR A QUALIFIER WORD ALONE.
C       THE SEARCH IS PERFORMED FROM THE LINE JUST AFTER
C       THE CURRENT LINE
C       DOWN TO THE MACRO END. IF THE TARGET OF THE SEARCH
C       IS NOT FOUND, THE POINTER IS LEFT AT THE LAST
C       MACRO LINE AND A MESSAGE THAT THE TARGET WAS NOT
C       FOUND IS PRINTED.
C
          CHARACTER
     2    TARGET*80,TAR1*8,TAR2*8,WCN*8,WQN*8,WSN*80
C
          INTEGER MMIJ,QBVAL,L,I,J,K

          COMMON/MIJ/MMIJ
C
          COMMON/NXT/WCN,WQN,WSN
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C       SET UP FOR POSSIBLE USE OF SUBROUTINE (NEXT)
          WCN='LO'
          WQN=WQ
          WSN=WS
C
C       WHAT WAS TO QUALIFIER WORD FOR LO ?
C
C       WAS IT C OR Q OR CQ OR COQ.
C       IF IT WAS C SEARCH FOR TARGET = COMMAND WORD
C       IF IT WAS Q SEARCH FOR TARGET = QUALIFIER WORD
C       IF IT WAS COQ SEARCH FOR TARGET = COMMAND WORD OR
C                                       QUALIFIER WORD
C       IF IT WAS CQ SEARCH FOR FIRST PART OF TARGET
C                               = COMMAND WORD
C                               SECOND PART OF TARGET
C                               = QUALIFIER WORD.
C       THE TARGET STRING IS PASSED IN THE WS VARIABLE
C       HANDLE THE STRING AND DETERMINE IF IT HAS MORE THAN
C       ONE PART.
          TARGET=WS
C       ALL LEADING BLANKS ARE NOW TO BE REMOVED.
          L=0
 1        QBVAL=ICHAR(TARGET(1:1))
          IF(QBVAL.EQ.32) THEN
              TARGET=TARGET(2:80)
              L=L+1
              IF(L.EQ.80) GO TO 20
              GO TO 1
          ELSE
C       FOUND A NON-BLANK, PROCEED, LEADING BLANKS REMOVED
          END IF
C       NOW SEARCH FOR EITHER THE NEXT BLANK OR 8 CHARACTERS.
C       BREAK OFF THE FIRST PART OF TARGET TO TAR1.
C
          QBVAL=0
          DO 2 I=1,8
              QBVAL=ICHAR(TARGET(I:I))
              IF(QBVAL.EQ.32) THEN
                  TAR1=TARGET(1:(I-1))
                  TARGET=TARGET(I:80)
                  GO TO 3
              ELSE
C       CONTINUE SEARCH
              END IF
 2        CONTINUE
C       IF YOU GOT HERE THEN TAR1 = FIRST EIGHT CHARACTERS
          TAR1=TARGET(1:8)
          TARGET=TARGET(9:80)
 3        CONTINUE
C       NOW PROCESS OUT TAR2.
C       ARE THERE ANY NON-BLANKS TO BE REMOVED?
C       IF SO REMOVE THEM.
          L=0
          QBVAL=0
 30       QBVAL=ICHAR(TARGET(1:1))
          IF(QBVAL.NE.32) THEN
              TARGET=TARGET(2:80)
              L=L+1
              IF(L.EQ.80) THEN
C       TAR2 DID NOT EXIST
                  TAR2=' '
                  GO TO 100
              ELSE
              END IF
              GO TO 30
          ELSE
C       FOUND A BLANK, PROCEED
          END IF
C       AGAIN WE MUST REMOVE LEADING BLANKS
          L=0
          QBVAL=0
 40       QBVAL=ICHAR(TARGET(1:1))
          IF(QBVAL.EQ.32) THEN
              TARGET=TARGET(2:80)
              L=L+1
              IF(L.EQ.80) THEN
C       TAR2 WAS BLANK
                  TAR2=' '
                  GO TO 100
              ELSE
              END IF
              GO TO 40
          ELSE
C       ANOTHER NON-BLANK WAS FOUND
C       THIS IS THE START OF TAR2.
          END IF
          L=0
          QBVAL=0
          DO 50 I=1,8
              QBVAL=ICHAR(TARGET(I:I))
              IF(QBVAL.EQ.32) THEN
                  TAR2=TARGET(1:(I-1))
                  GO TO 100
              ELSE
              END IF
C               PROCEED
 50       CONTINUE
C       IF YOU GOT HERE THEN
          TAR2=TARGET(1:8)
C       NOW TARGET HAS BEEN PROCESSED
C               NOW SEARCH FO APPROPRIATE TARGETS THROUGH THE MACRO
C
 100      J=CURLIN+1
          DO 5000 I=J,(MCDIR2(2,(MMIJ))-2)
              IF(WQ.EQ.'C') GO TO 1000
              IF(WQ.EQ.'Q') GO TO 2000
              IF(WQ.EQ.'CQ') GO TO 3000
              IF(WQ.EQ.'COQ') GO TO 4000
 1000         IF(MACCW(I).EQ.TAR1) THEN
                  K=I
                  GO TO 6000
              ELSE
                  GO TO 5000
              END IF
 2000         IF(MACQW(I).EQ.TAR1) THEN
                  K=I
                  GO TO 6000
              ELSE
                  GO TO 5000
              END IF
 3000         IF(MACCW(I).EQ.TAR1.AND.MACQW(I).EQ.TAR2) THEN
                  K=I
                  GO TO 6000
              ELSE
                  GO TO 5000
              END IF
 4000         IF(MACCW(I).EQ.TAR1.OR.MACQW(I).EQ.TAR1) THEN
                  K=I
                  GO TO 6000
              ELSE
                  GO TO 5000
              END IF
 5000     CONTINUE
          CURLIN=(MCDIR2(2,(MMIJ))-2)
          OUTLYNE='SEARCH TARGET NOT FOUND'
          CALL SHOWIT(1)
          CALL MPR
          RETURN
 6000     CURLIN=K
          CALL MPR
          RETURN
C
 20       CONTINUE
          OUTLYNE='SEARCH TARGET WAS BLANK. NO SEARCH PERFORMED.'
          CALL SHOWIT(1)
          RETURN
      END
C SUB MCFLEX.FOR
      SUBROUTINE MCFLEX
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE ERASES ALL EXISTING MACRO FILES
C       MACXXX.FOR
C
          LOGICAL LXIST
C
          INTEGER NF
C
          CHARACTER FILNAM*10
C
          COMMON/NEFER/NF
C
          COMMON/COMNAM/FILNAM
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          DO 10 NF=1,MAXMAC
              CALL MACFIL
              LXIST=.FALSE.
              INQUIRE(FILE=trim(LIBMAC)//FILNAM,EXIST=LXIST)
              IF(LXIST) THEN
                  OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1            'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
                  CALL CLOSE_FILE(30,0)
              ELSE
C       CONTINUE SEARCH
              END IF
 10       CONTINUE
          RETURN
      END
C SUB MAXLNN.FOR
      SUBROUTINE MAXLNN(MAXLL,MAXLIN)
C
          IMPLICIT NONE
C
          INTEGER MAXLL,MAXLIN
C
!        CHARACTER NAME*8
C
C       THIS SUBROUTINE IS USED TO SET THE VALUE OF MAXLL
          MAXLL=MAXLIN
          RETURN
      END

C SUB MACX1.FOR
C
      SUBROUTINE MACX1
C
          IMPLICIT NONE
C
          INTEGER CT
C
          REAL*8 H(1:5,0:10),PASS1,PASS2
C
          COMMON/COMACC1/PASS1
C
          COMMON/COMACC2/PASS2
C
          COMMON/COMCT/CT
C
          COMMON/ACH/H
C
          INCLUDE 'datmac.inc'
C
          IF(INT(DABS(H(2,CT))).EQ.0) THEN
              PASS2=((((PASS1)*H(3,CT))+H(4,CT))
     1        **INT(H(5,CT)))
          ELSE
              PASS2=((((MNW(NESTER,INT(DABS(H(2,CT)))))*H(3,CT))+H(4,CT))
     1        **INT(H(5,CT)))
          END IF
          RETURN
      END
C SUB MACRUN.FOR
      SUBROUTINE MACRUN(MACYES)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS THE MAIN CONTROLING SUBROUTINE FOR
C       MACRO EXECUTION. IT IS CALLED BY SUBROUTINE CONTROL.
C       UPON RETURN TO CONTROL, THE VALUE OF MACYES TELLS
C       CONTROL IF A MACRO WAS FOUND AND EXECUTED OR IF
C       NO MACRO WAS FOUND AND AND INVALID COMMAND HAD BEEN
C       ENTERED
C
          INTEGER FLG(0:20)
     6    ,MACYES,NF,I,KLI
C
          LOGICAL EXISJK

          INTEGER ACCCNT,ACCSUB
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          COMMON/FFL/FLG
C
          COMMON/NEFER/NF
C
          LOGICAL FUN
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C       CHECK THE FUNCTION DIRECTORY IF REQUIRED
          KLI=0
          FUN=.FALSE.
          IF(WC.EQ.'FUN01   ') KLI=1
          IF(WC.EQ.'FUN02   ') KLI=2
          IF(WC.EQ.'FUN03   ') KLI=3
          IF(WC.EQ.'FUN04   ') KLI=4
          IF(WC.EQ.'FUN05   ') KLI=5
          IF(WC.EQ.'FUN06   ') KLI=6
          IF(WC.EQ.'FUN07   ') KLI=7
          IF(WC.EQ.'FUN08   ') KLI=8
          IF(WC.EQ.'FUN09   ') KLI=9
          IF(WC.EQ.'FUN10   ') KLI=10
C     ALLOW FUN NAMES IN MERIT AND UPDATE MERIT
          IF(F27.EQ.1.OR.F27.EQ.2) KLI=0
          IF(KLI.NE.0.AND.FUNEXT(KLI)) THEN
              FUN=.TRUE.
C       WE WANT TO RUN A FUNCTION, NOT A MACRO
C**********************************************************************
C       WE FOUND THE FUNCTION CALLED FOR.
              F4=1
C       FLAG F4 = 1 INDICATES INSTRUCTIONS RUNNING IN A FUNCTION
C       BY SETTING FLAG F4 HERE, WE GUARANTEE THAT IT IS ALWAYS
C       SET DURING FUNCTION EXECUTION EVEN IF THE MACROS ARE NESTED.
              NESFUN(0)=.TRUE.
              NEST=0
              NESTI(0)=1
              NESTIJ(0)=KLI
              NF=KLI
              ACCSUB=0
              CALL MACEXC
              KLI=NF
              F4=0
C       SETTING F4 = 0 INDICATES ALL FUNCTIONS FINISHED RUNNING
              MACYES=1
              RETURN
C       MACEXC IS THE SUBROUTINE WHICH ACTUALLY RUNS THE
C       KLI TH FUNCTION.
C**********************************************************************
          ELSE
C       PROCEED WITH MACRO RUNNING
          END IF
C       READ THE CURRENT MACRO DIRECTORY
C
C       OPEN UNIT 20 FOR I/O
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"MACRO FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
C
          DO 10 I=1,MAXMAC
              READ(UNIT=20,REC=I,ERR=10)MCDIR1(I),MCDIR2(1,I),MCDIR2(2,I),
     1        MCDIR2(3,I),MCDIR3(I)
C
C       THE INITIAL VALUES STORED IN MAC.DAT ARE:
C
C               MCDIR1(I) IS THE MACRO NAME
C               MCDIR2(1,I) IS THE MACRO LOCK
C               MCDIR2(2,I) IS THE MACRO LENGTH
C               MCDIR2(3,I) IS THE OCCUPANCY FLAG
C                               = 0 FOR BLANK MACRO
C                               = 1 FOR NON-BLANK MACRO
C

 10       CONTINUE
          CALL CLOSE_FILE(20,1)
C
C       SEARCHES OF THIS MACRO DIRECTORY ARE NOW MADE TO DETERMINE
C       IF THE MACRO REQUESTED IS ON FILE.
C
C       NOW DETERMINE A LOCATION OF THE MACRO.
C       SEARCH THE DIRECTORY FOR THE MACRO TO BE EXECUTED.
C
          DO 20 I=1,MAXMAC
C
C       TEST FOR A NAME MATCH
C
              IF(MCDIR1(I).EQ.WC) THEN
C
C       WE FOUND THE MACRO CALLED FOR. SET NF=I
                  NF=I
                  F4=1
C       FLAG F4 = 1 INDICATES INSTRUCTIONS RUNNING IN A MACRO
C       BY SETTING FLAG F4 HERE, WE GUARANTEE THAT IT IS ALWAYS
C       SET DURING MACRO EXECUTION EVEN IF THE MACROS ARE NESTED.
                  NESFUN(0)=.FALSE.
                  NEST=0
                  NESTI(0)=1
                  NESTIJ(0)=NF
                  ACCSUB=0
                  CALL MACEXC
                  F4=0
C       SETTING F4 = 0 INDICATES ALL MACROS FINISHED RUNNING
                  MACYES=1
                  RETURN
C       MACEXC IS THE SUBROUTINE WHICH ACTUALLY RUNS THE
C       NF TH MACRO IN MAC.DAT.
C
              ELSE
C       CONTINUE THE SEARCH
              END IF
 20       CONTINUE
C
C       IF YOU GOT HERE YOU COULD NOT FIND THE MACRO WITH THE CALLED
C       FOR LOCK.
C
          MACYES=0
          RETURN
C
      END
C SUB MACMOD.FOR
      SUBROUTINE MACMOD
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDLES THE MEDIT COMMANDS AND MEDIT MODE
C       MACRO INPUT
C
          CHARACTER WCN*8,WQN*8,WSN*80
C
          INTEGER MMIJ
C
          COMMON/MIJ/MMIJ
C
          COMMON/NXT/WCN,WQN,WSN
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C       FIRST CHECK THE COMMAND WORD WC FOR SPECIAL
C       MEDIT COMMANDS.
C       THEY ARE
C
C               PR -  PRINT LINE OR LINES
C               BT -  GO TO BOTTOM OF MACRO
C               DE -  DELETE  MACRO LINES
C               EX -  EXTRACT ANOTHER MACRO INTO THE
C                     CURRENT MACRO
C               FL -  STOP EDITING AND FILE THE MACRO
C               GO -  GO TO A SPECIFIED MACRO LINE
C             LO C -  LOCATE THE NEXT OCCURENCE OF A COMMAND
C                     WORD
C             LO Q -  LOCATE THE NEXT OCCURENCE OF A QUALIFIER
C                     WORD
C            LO CQ -  LOCATE THE NEXT OCCURENCE OF THE COMMAND
C                     AND QUALIFIER WORD.
C           LO COQ -  LOCATE THE NEXT OCCURENCE OT THE COMMAND
C                     OR QUALIFIER WORD.
C               NEXT- REPEAT SEARCH FURTHER DOWN MACRO.
C       QUIT OR QU -  QUIT WITHOUT CHANGING MACRO FILE(MAC.DAT)
C               RE -  REPLACE CURRENT LINE WITH WHAT FOLLOWS
C               TP -  GO TO TOP OF THE MACRO
C               PB -  PRINTS PAGES BACK IF SCROLLING (CMD LEVEL COM.)
C
C       THERE ARE THE SPECIAL COMMAND (BLANK WITH A RETURN
C       AND ?. A BLANK WITH A RETURN YIELDS THE MESSAGE.
C
C       MACRO (MACRO NAME),EDITING AT MACRO LINE # EXCEPT AT
C       LINE ZERO WHICH YIELDS A TOP OF MACRO MESSAGE.
C
C
C       THE BLANK FOLLOWED BY A RETURN YIELDS THE PROGRAM
C       LEVEL MESSAGE FROM SUBROUTINE BLANK
C
C       CHECK FOR BT,DE,EX,FL,GO,LO,QUIT,QU,RE,TP OR PR
C       AND BLANK OR ? AND NEXT
C
          IF(WC.EQ.'?') THEN
              CALL QUERRYY
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.' '.AND.F50.EQ.1) THEN
              CALL BLANK0
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.' '.AND.F50.NE.1) THEN
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'PR'.AND.F46.EQ.0) THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  OUTLYNE='"PR" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CALL MPR
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'PR'.AND.F46.EQ.1) THEN
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  OUTLYNE='"PR" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              OUTLYNE='MACRO HAS NO LINES'
              CALL SHOWIT(1)
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'BT'.AND.F46.EQ.0) THEN
              CALL MOD2
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'BT'.AND.F46.EQ.1) THEN
              OUTLYNE='MACRO HAS NO LINES'
              CALL SHOWIT(1)
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'NEXT'.AND.F46.EQ.0) THEN
              CALL MOD2
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'NEXT'.AND.F46.EQ.1) THEN
              OUTLYNE='MACRO HAS NO LINES'
              CALL SHOWIT(1)
              GO TO 1000
          ELSE
          END IF
          IF(WC(1:2).EQ.'DE'.AND.F46.EQ.0) THEN
              CALL MOD2
              GO TO 1000
          ELSE
          END IF
          IF(WC(1:2).EQ.'DE'.AND.F46.EQ.1) THEN
              OUTLYNE='MACRO HAS NO LINES'
              CALL SHOWIT(1)
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'EX') THEN
              CALL MOD2
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'FL') THEN
              CALL MOD2
              GO TO 1000
          ELSE
          END IF
          IF(WC(1:2).EQ.'GO'.AND.F46.EQ.0) THEN
              CALL MOD2
              GO TO 1000
          ELSE
          END IF
          IF(WC(1:2).EQ.'GO'.AND.F46.EQ.1) THEN
              OUTLYNE='MACRO HAS NO LINES'
              CALL SHOWIT(1)
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'LO'.AND.F46.EQ.0) THEN
              CALL MLO
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'LO'.AND.F46.EQ.1) THEN
              OUTLYNE='MACRO HAS NO LINES'
              CALL SHOWIT(1)
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'QUIT'.OR.WC.EQ.'QU') THEN
              CALL MOD2
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'RE'.AND.F46.EQ.0) THEN
              CALL MOD2
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'RE'.AND.F46.EQ.1) THEN
              OUTLYNE='MACRO HAS NO LINES'
              CALL SHOWIT(1)
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'TP'.AND.F46.EQ.0) THEN
              CALL MOD2
              GO TO 1000
          ELSE
          END IF
          IF(WC.EQ.'TP'.AND.F46.EQ.1) THEN
              OUTLYNE='MACRO HAS NO LINES'
              CALL SHOWIT(1)
              GO TO 1000
          ELSE
C       THE INSTRUCTION LINE MUST BE AN INPUT LINE. IF IT IS NOT
C       BLANK, INSERT IT INTO THE MACRO AT THE CURRENT POSITION
C
C**********************************************************************
C
C       HERE IS WHERE MACRO SPECIFIC OPERATING COMMANDS HAVE SOME
C       OR THEIR SYNTAX CHECKED.
C
              IF(WC.EQ.'QSUB'.OR.WC.EQ.'SSUB') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE='"'//WC(1:4)//'" TAKES NO NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'QRSUB') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE='"'//WC(1:5)//'" TAKES NO NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'ACCSUB') THEN
                  IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"ACCSUB" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF1.EQ.1) THEN
                      OUTLYNE=
     1                '"ACCSUB" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
C       NOT ACCSUB
              END IF
              IF(WC.EQ.'CSUB') THEN
                  IF(SST.EQ.1) THEN
                      OUTLYNE='"'//WC(1:4)//'" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'CRSUB') THEN
                  IF(SST.EQ.1) THEN
                      OUTLYNE='"'//WC(1:5)//'" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'SAVE') THEN
                  IF(SST.EQ.1) THEN
                      OUTLYNE='"SAVE" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'RELOAD') THEN
                  IF(SST.EQ.1) THEN
                      OUTLYNE='"RELOAD" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'NSUB') THEN
                  IF(SST.EQ.1) THEN
                      OUTLYNE='"'//WC(1:4)//'" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'IF(X>0)'.OR.WC.EQ.'IF(X<0)'.OR.WC.EQ.'IF(X=0)'.OR.
     1        WC.EQ.'IF(X=Y)'.OR.WC.EQ.'IF(X<Y)'.OR.WC.EQ.'IF(X>Y)') THEN
                  IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:6)//
     1                '" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:6)//'" TAKES EITHER QUALIFIER OR'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD #1 INPUT, BUT NOT BOTH'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'BPOS'.OR.WC.EQ.'BNEG') THEN
                  IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:4)//
     1                '" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:4)//'" TAKES EITHER QUALIFIER OR'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD #1 INPUT, BUT NOT BOTH'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'BRDF1'.OR.
     1        WC.EQ.'BRDF2'.OR.WC.EQ.'BRDF3'.OR.
     1        WC.EQ.'BRDF4'.OR.WC.EQ.'BRDF5') THEN
                  IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:5)//
     1                '" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:5)//'" TAKES EITHER QUALIFIER OR'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD #1 INPUT, BUT NOT BOTH'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'BRDQ') THEN
                  IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:4)//
     1                '" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:4)//'" TAKES EITHER QUALIFIER OR'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD #1 INPUT, BUT NOT BOTH'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'BRT'.OR.WC.EQ.'BRF') THEN
                  IF(SST.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:3)//'" TAKES NO STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'TRACE') THEN
                  IF(SST.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:5)//'" ONLY TAKES QUALIFIER WORD INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'SSTEP') THEN
                  IF(SST.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:5)//'" ONLY TAKES QUALIFIER WORD INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'PAUSE') THEN
                  IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:5)//'" TAKES NO ADDITIONAL INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'BRERR') THEN
                  IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:5)//
     1                '" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:5)//'" TAKES EITHER QUALIFIER OR'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD #1 INPUT, BUT NOT BOTH'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'BZE'.OR.WC.EQ.'BRI'.OR.WC.EQ.'BRJ'.OR.
     1        WC.EQ.'BRU'.OR.WC.EQ.'BRK'.OR.WC.EQ.'BRL'.OR.WC.EQ.'BRM'
     2        .OR.WC.EQ.'BRN') THEN
                  IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:3)//
     1                '" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.1.AND.S1.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:3)//'" TAKES EITHER QUALIFIER OR'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD #1 INPUT, BUT NOT BOTH'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'PUTR') THEN
                  IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"PUTR" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'BRANCH') THEN
                  IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"BRANCH" ONLY TAKES QUALIFIER AND '
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD #1, #2 AND #3 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(SQ.EQ.1.AND.S3.EQ.1) THEN
                      OUTLYNE=
     1                '"BRANCH" TAKES EITHER QUALIFIER OR'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD #3 INPUT, BUT NOT BOTH'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'MOVE'.AND.WQ.EQ.'NW') THEN
                  IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"MOVE NW" ONLY TAKES NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF1.EQ.1) THEN
                      OUTLYNE=
     1                '"MOVE NW" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
                  IF(INT(W1).LT.1.OR.INT(W1).GT.5) THEN
                      OUTLYNE=
     1                '"MOVE NW" REQUIRES 1, 2, 3, 4 OR 5 FOR NUMERIC INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
              ELSE
              END IF
              IF(WC.EQ.'BP') THEN
                  IF(SST.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE=
     1                '"BP" ONLY TAKES QUALIFIER WORD INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'BRQ') THEN
                  IF(SN.EQ.1) THEN
                      OUTLYNE=
     1                '"BRQ" ONLY TAKES QUALIFIER AND STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'STORE') THEN
                  IF(SST.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE=
     1                '"STORE" ONLY TAKES QUALIFIER WORD INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'M') THEN
                  IF(SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                      OUTLYNE=
     1                '"'//WC(1:1)//
     1                '" ONLY TAKES STRING AND NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'C') THEN
                  IF(SQ.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='"'//WC(1:1)//'" ONLY TAKES STRING INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'RETURN') THEN
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='"RETURN" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WC.EQ.'EOM') THEN
                  IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
                      OUTLYNE='"EOM" TAKES NO EXPLICIT INPUT'
                      CALL SHOWIT(1)
                      OUTLYNE='RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C**********************************************************************
C
              CALL INSLIN
              GO TO 1000
          END IF
 1000     CONTINUE
          RETURN
      END
