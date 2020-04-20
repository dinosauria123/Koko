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

      SUBROUTINE DMULTIPROCESS(WD,WDEXIS,INSTRING,N,CVERROR)
          IMPLICIT NONE
          CHARACTER AA23*23,WORD*23,VALA*23
          REAL*8 WD,VALV
          CHARACTER INSTRING*1024,BL1024*1024
          LOGICAL CVERROR
          INTEGER WDEXIS,N,I,K
          DIMENSION WD(1:N),WDEXIS(1:N)
          INCLUDE 'datmai.inc'
          AA23='                       '
          BL1024=AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//'    '
C     INITIALIZE RETURN ARRAYS
          DO I=1,N
              WD(I)=0.0D0
              WDEXIS(I)=0
          END DO
C     INSTRING HAS NO LEADING BLANKS
          DO K=1,N
C     REMOVE LEADING BLANKS FROM REBUILT STRING
              DO I=1,1024
                  IF(INSTRING(1:1).EQ.' ') THEN
                      INSTRING(1:1024)=INSTRING(2:1024)//' '
                  END IF
              END DO
              DO I=1,1024
                  IF(INSTRING(I:I).EQ.' ') THEN
                      IF(INSTRING(1:1024).EQ.BL1024(1:1024)) RETURN
C     STRIP OUT WORD
                      WORD(1:23)=INSTRING(1:I-1)
C     REBUILD INSTRING
                      INSTRING(1:1024)=INSTRING(I:1024)//BL1024(1:I-1)
C     PACK RIGHT AND TRANSLATE
                      VALA(1:23)=AA23(1:(23-(I-1)))//WORD(1:(I-1))
                      CALL ATODCODEV(VALA,VALV,CVERROR)
                      IF(CVERROR) THEN
                          GO TO 6666
                      END IF
C     STORE IN RETURN ARRAY
                      WD(K)=VALV
C     SET FLAG ARRAY
                      WDEXIS(K)=1
C     PROCESS NEXT NUMERIC WORD
                      GO TO 6666
                  ELSE
C     CHECK NEXT CHARACTER FOR A BLANK
                  END IF
              END DO
 6666         CONTINUE
          END DO
          RETURN
      END


      SUBROUTINE IMULTIPROCESS(IWD,WDEXIS,INSTRING,N,CVERROR)
          IMPLICIT NONE
          CHARACTER AA23*23,WORD*23,VALA*23
          INTEGER IWD,IVALV
          LOGICAL CVERROR
          CHARACTER INSTRING*1024,BL1024*1024
          INTEGER WDEXIS,N,I,K
          DIMENSION IWD(1:N),WDEXIS(1:N)
          INCLUDE 'datmai.inc'
          AA23='                       '
          BL1024=AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//'    '
C     INITIALIZE RETURN ARRAYS
          DO I=1,N
              IWD(I)=0
              WDEXIS(I)=0
          END DO
C     INSTRING HAS NO LEADING BLANKS
          DO K=1,N
C     REMOVE LEADING BLANKS FROM REBUILT STRING
              DO I=1,1024
                  IF(INSTRING(1:1).EQ.' ') THEN
                      INSTRING(1:1024)=INSTRING(2:1024)//' '
                  END IF
              END DO
              DO I=1,1024
                  IF(INSTRING(I:I).EQ.' ') THEN
                      IF(INSTRING(1:1024).EQ.BL1024(1:1024)) RETURN
C     STRIP OUT WORD
                      WORD(1:23)=INSTRING(1:I-1)
C     REBUILD INSTRING
                      INSTRING(1:1024)=INSTRING(I:1024)//BL1024(1:I-1)
C     PACK RIGHT AND TRANSLATE
                      VALA(1:23)=AA23(1:(23-(I-1)))//WORD(1:(I-1))
                      CALL ATOICODEV(VALA,IVALV,CVERROR)
                      IF(CVERROR) THEN
                          GO TO 6666
                      END IF
C     STORE IN RETURN ARRAY
                      IWD(K)=IVALV
C     SET FLAG ARRAY
                      WDEXIS(K)=1
C     PROCESS NEXT NUMERIC WORD
                      GO TO 6666
                  ELSE
C     CHECK NEXT CHARACTER FOR A BLANK
                  END IF
              END DO
 6666         CONTINUE
          END DO
          RETURN
      END


      SUBROUTINE ONEBLANK(N,STRING1024)
          IMPLICIT NONE
C
          INTEGER N
C
          CHARACTER STRING1024*1024
  10      CONTINUE
          IF(STRING1024(N+1:N+2).EQ.' ') THEN
              STRING1024(1:1024)=STRING1024(1:N+1)//STRING1024((N+3):1024)//' '
              GO TO 10
          ELSE
              RETURN
          END IF
      END


      SUBROUTINE RIGHTJUST(VALA)
          IMPLICIT NONE
          CHARACTER*23 VALA
          INTEGER I
          DO I=1,23
              IF(VALA(23:23).EQ.' ') THEN
                  VALA(1:23)=' '//VALA(1:22)
              ELSE
                  RETURN
              END IF
          END DO
          RETURN
      END


      SUBROUTINE LEFTJUST(TEMPER)
          IMPLICIT NONE
          CHARACTER*1024 TEMPER
          INTEGER I
          DO I=1,1024
              IF(TEMPER(1:1).EQ.' ') THEN
                  TEMPER(1:1024)=TEMPER(2:1024)//' '
              ELSE
                  RETURN
              END IF
          END DO
          RETURN
      END

C SUB CONTRO.FOR
      SUBROUTINE CONTRO
C
          IMPLICIT NONE
C
          CHARACTER WCC(1:16000)*8,WC1*8,WQ1*8,WS1*141
C
          LOGICAL YESEOS,STP,IS,FNYES,FNYES1,NOGO,OHFUN,OREFEXT,ORAYEXT
C
          INTEGER I,NNN,NNNN,OS1,OS2,OS3,OS4,ODF1,ODF2,ODF3,ODF4
     1    ,MACYES,RET,RETRET,SST1,FF29,FF51,FF52,SQ1,SPEOS,LEOS
C
          COMMON/RETIT/RET,RETRET,LEOS,SPEOS
C
          REAL*8 WWW1,OW1,OW2,OW3,OW4
C
          COMMON/COMWDS/WCC
C
          CHARACTER REMWQ*8
          COMMON/WQREM/REMWQ
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datlen.inc'

          IF(WC.EQ.'ANGLES') RETURN
          IF(WC.EQ.'MYGLASS') WC='GLASS'
          IF(WC.EQ.'EPD') THEN
              IF(DF1.EQ.0) W1=W1/2.0D0
              WC='SAY'
          END IF
          IF(WC.NE.'PLTCHRSH') CHRSHIFTEXT=.FALSE.
          IF(F3.NE.1.AND.F2.NE.1) THEN
              IF(WC(1:8).EQ.'SHO     '.AND.SQ.EQ.0.AND.S1.EQ.0) THEN
                  SQ=1
                  WQ='X       '
              END IF
          END IF
          IF(WC.EQ.'GLASS   '.AND.WS(1:4).EQ.'REFL') THEN
              WS=' '
              SST=0
              WC='REFL    '
          END IF
          IF(WC.EQ.'GLASS   '.AND.WS(1:4).EQ.'REFLTIRO') THEN
              WS=' '
              SST=0
              WC='REFLTIRO'
          END IF
          IF(WC.EQ.'GLASS   '.AND.WS(1:4).EQ.'REFLTIR') THEN
              WS=' '
              SST=0
              WC='REFLTIR'
          END IF
          IF(WC.EQ.'GLASS   '.AND.WS(1:3).EQ.'AIR') THEN
              WS=' '
              SST=0
              WC='AIR     '
          END IF

          REMWQ='        '
C
          IF(WS(1:40).EQ.'                                        ') THEN
              SST=0
          END IF
C
          OREFEXT=REFEXT
          ORAYEXT=RAYEXT
          LASTWASFOB=.FALSE.
          IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
          IF(LASTWASFOB) REFEXT=OREFEXT
          IF(LASTWASFOB) RAYEXT=ORAYEXT
C
          IF(ALLSET) THEN
              IF(SN.EQ.0.AND.STI.EQ.0.AND.SQ.EQ.0.AND.SST.EQ.0) THEN
                  IF(WC.EQ.'RTG') GO TO 5
                  IF(WC.EQ.'RTGLBL') GO TO 5
                  IF(WC.EQ.'CTG') GO TO 5
                  IF(WC.EQ.'CTGLBL') GO TO 5
                  IF(WC.EQ.'RIN') GO TO 5
                  IF(WC.EQ.'CAOB') GO TO 5
                  IF(WC.EQ.'TAD') GO TO 5
                  IF(WC.EQ.'ASPH') GO TO 5
                  IF(WC.EQ.'ASPH2') GO TO 5
                  IF(WC.EQ.'TASPH') GO TO 5
                  IF(WC.EQ.'TR') GO TO 5
                  IF(WC.EQ.'TC') GO TO 5
                  IF(WC.EQ.'SLV') GO TO 5
                  IF(WC.EQ.'PIK') GO TO 5
                  IF(WC.EQ.'RIN') GO TO 5
                  IF(WC.EQ.'RIN2') GO TO 5
                  IF(WC.EQ.'DUMOUT') GO TO 5
                  IF(WC.EQ.'INR') GO TO 5
                  IF(WC.EQ.'PRSPR') GO TO 5
                  IF(WC.EQ.'PIVOT') GO TO 5
                  IF(WC.EQ.'PIVAXIS') GO TO 5
                  IF(WC.EQ.'PXTY') GO TO 5
                  IF(WC.EQ.'PXTX') GO TO 5
                  IF(WC.EQ.'PITY') GO TO 5
                  IF(WC.EQ.'PITX') GO TO 5
                  IF(WC.EQ.'PRTY') GO TO 5
                  IF(WC.EQ.'PRTX') GO TO 5
                  IF(WC.EQ.'PRX') GO TO 5
                  IF(WC.EQ.'PRY') GO TO 5
                  IF(WC.EQ.'PRXYZ') GO TO 5
                  IF(WC.EQ.'PRXYI') GO TO 5
                  IF(WC.EQ.'PRXYIP') GO TO 5
                  IF(WC.EQ.'PRXYD') GO TO 5
                  IF(WC.EQ.'PRZ') GO TO 5
                  IF(WC.EQ.'PRR') GO TO 5
                  IF(WC.EQ.'PRREF') GO TO 5
                  IF(WC.EQ.'PRLMN') GO TO 5
                  IF(WC.EQ.'PRDIFFXR') GO TO 5
                  IF(WC.EQ.'PRDIFFYR') GO TO 5
                  IF(WC.EQ.'PRDIFFXM') GO TO 5
                  IF(WC.EQ.'PRDIFFYM') GO TO 5
                  IF(WC.EQ.'GPXTY') GO TO 5
                  IF(WC.EQ.'GPXTX') GO TO 5
                  IF(WC.EQ.'VERTEX') GO TO 5
                  IF(WC.EQ.'PRGLOBAL') GO TO 5
                  IF(WC.EQ.'BEAM') GO TO 5
                  IF(WC.EQ.'NDEX') GO TO 5
                  IF(WC.EQ.'NDEX2') GO TO 5
                  GO TO 6
 5                WQ='ALL'
                  SQ=1
 6                CONTINUE
              END IF
          END IF
C
          IF(WC(1:8).EQ.'END     ') WC='EOS     '
C
C       IT IS WITHIN THIS SUBROUTINE THAT THE DECISION (BASED
C       UPON INPUT) IS MADE AS TO WHAT TO DO OR CALCULATE.
C       FLAG STATUS AS TO WHICH LEVEL OF THE PROGRAM IS BEING
C       RUN IS ALWAYS PASSED VIA A (COMMON) BACK TO THIS
C       SUBROUTINE. THE COMMON IS NAMED FLAGS COMMON.
C
          IF(F3.NE.1.AND.F2.NE.1) THEN
              IF(WC(1:8).EQ.'SHO     '.AND.SQ.EQ.0.OR.
     1        WC.EQ.'SHOW'.AND.SQ.EQ.0) THEN
                  IF(SQ.EQ.0.AND.S1.EQ.0) THEN
                  ELSE
                      CALL SHOWREG
                  END IF
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
          END IF
          IF(F3.NE.1.AND.F2.NE.1) THEN
C     NOT MACRO EDITTING OR INPUT, DO A REPLACEMENT
              IF(WC(1:8).EQ.'SHO     '.AND.SQ.EQ.1.OR.
     1        WC.EQ.'SHOW'.AND.SQ.EQ.1) THEN
                  REMWQ=WQ
                  SHOW=.TRUE.
                  WC='GET'
              ELSE
                  SHOW=.FALSE.
              END IF
          END IF
          IF(F3.NE.1.AND.F2.NE.1) THEN
C     NOT MACRO EDITTING OR INPUT, DO A REPLACEMENT
              IF(WC(1:4).EQ.'ASHO'.AND.SQ.EQ.1.OR.
     1        WC.EQ.'ASHOW'.AND.SQ.EQ.1) THEN
                  REMWQ=WQ
                  ASHOW=.TRUE.
                  WC='AGET'
              ELSE
                  ASHOW=.FALSE.
              END IF
          END IF
C
C     3/8/94 HERE IS THE ACTION FOR "W1" THROUGH "W5"
C

          IF(F2.NE.1.AND.F3.NE.1) THEN
              IF(WC.NE.'W1'.AND.WC.NE.'W2'.AND.WC.NE.'W3'.AND.WC.NE.'W4'
     1        .AND.WC.NE.'W5'.AND.WC.NE.'GET'.AND.WC.NE.'AGET') THEN
                  IF(W1CODE.NE.0) THEN
                      IF(DF1.EQ.1) THEN
C     ASSIGN A VALUE
                          IF(W1CODE.LT.0.0D0) THEN
                              IF(W1CODE.EQ.-100) W1=REG(1)
                              IF(W1CODE.EQ.-99) W1=REG(2)
                              IF(W1CODE.EQ.-98) W1=REG(3)
                              IF(W1CODE.EQ.-97) W1=REG(4)
                              IF(W1CODE.EQ.-96) W1=REG(5)
                              IF(W1CODE.EQ.-95) W1=REG(6)
                              IF(W1CODE.EQ.-94) W1=REG(7)
                              IF(W1CODE.EQ.-93) W1=REG(8)
                              IF(W1CODE.EQ.-92) W1=REG(9)
                              IF(W1CODE.EQ.-91) W1=REG(10)
                              IF(W1CODE.EQ.-90) W1=REG(11)
                              IF(W1CODE.EQ.-89) W1=REG(12)
                              IF(W1CODE.EQ.-88) W1=REG(13)
                              IF(W1CODE.EQ.-87) W1=REG(14)
                              IF(W1CODE.EQ.-86) W1=REG(15)
                              IF(W1CODE.EQ.-85) W1=REG(16)
                              IF(W1CODE.EQ.-84) W1=REG(17)
                              IF(W1CODE.EQ.-83) W1=REG(18)
                              IF(W1CODE.EQ.-82) W1=REG(19)
                              IF(W1CODE.EQ.-81) W1=REG(20)
                              IF(W1CODE.EQ.-80) W1=REG(40)
                              IF(W1CODE.EQ.-79) W1=REG(30)
                              IF(W1CODE.EQ.-78) W1=REG(21)
                              IF(W1CODE.EQ.-77) W1=REG(22)
                              IF(W1CODE.EQ.-76) W1=REG(23)
                              IF(W1CODE.EQ.-75) W1=REG(24)
                              IF(W1CODE.EQ.-74) W1=REG(25)
                              IF(W1CODE.EQ.-73) W1=REG(26)
                              IF(W1CODE.EQ.-72) W1=REG(27)
                              IF(W1CODE.EQ.-71) W1=REG(28)
                          ELSE
                              W1=GPREG(W1CODE)
                          END IF
                          DF1=0
                          S1=1
                          SN=1
C     DON'T ASSIGN W1 A VALUE
                      END IF
C     W1CODE WAS ZERO
                  END IF
                  IF(W2CODE.NE.0) THEN
                      IF(DF2.EQ.1) THEN
                          IF(W2CODE.LT.0.0D0) THEN
                              IF(W2CODE.EQ.-100) W2=REG(1)
                              IF(W2CODE.EQ.-99) W2=REG(2)
                              IF(W2CODE.EQ.-98) W2=REG(3)
                              IF(W2CODE.EQ.-97) W2=REG(4)
                              IF(W2CODE.EQ.-96) W2=REG(5)
                              IF(W2CODE.EQ.-95) W2=REG(6)
                              IF(W2CODE.EQ.-94) W2=REG(7)
                              IF(W2CODE.EQ.-93) W2=REG(8)
                              IF(W2CODE.EQ.-92) W2=REG(9)
                              IF(W2CODE.EQ.-91) W2=REG(10)
                              IF(W2CODE.EQ.-90) W2=REG(11)
                              IF(W2CODE.EQ.-89) W2=REG(12)
                              IF(W2CODE.EQ.-88) W2=REG(13)
                              IF(W2CODE.EQ.-87) W2=REG(14)
                              IF(W2CODE.EQ.-86) W2=REG(15)
                              IF(W2CODE.EQ.-85) W2=REG(16)
                              IF(W2CODE.EQ.-84) W2=REG(17)
                              IF(W2CODE.EQ.-83) W2=REG(18)
                              IF(W2CODE.EQ.-82) W2=REG(19)
                              IF(W2CODE.EQ.-81) W2=REG(20)
                              IF(W2CODE.EQ.-80) W2=REG(40)
                              IF(W2CODE.EQ.-79) W2=REG(30)
                              IF(W2CODE.EQ.-78) W2=REG(21)
                              IF(W2CODE.EQ.-77) W2=REG(22)
                              IF(W2CODE.EQ.-76) W2=REG(23)
                              IF(W2CODE.EQ.-75) W2=REG(24)
                              IF(W2CODE.EQ.-74) W2=REG(25)
                              IF(W2CODE.EQ.-73) W2=REG(26)
                              IF(W2CODE.EQ.-72) W2=REG(27)
                              IF(W2CODE.EQ.-71) W2=REG(28)
                          ELSE
                              W2=GPREG(W2CODE)
                          END IF
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                  END IF
                  IF(W3CODE.NE.0) THEN
                      IF(DF3.EQ.1) THEN
                          IF(W3CODE.LT.0.0D0) THEN
                              IF(W3CODE.EQ.-100) W3=REG(1)
                              IF(W3CODE.EQ.-99) W3=REG(2)
                              IF(W3CODE.EQ.-98) W3=REG(3)
                              IF(W3CODE.EQ.-97) W3=REG(4)
                              IF(W3CODE.EQ.-96) W3=REG(5)
                              IF(W3CODE.EQ.-95) W3=REG(6)
                              IF(W3CODE.EQ.-94) W3=REG(7)
                              IF(W3CODE.EQ.-93) W3=REG(8)
                              IF(W3CODE.EQ.-92) W3=REG(9)
                              IF(W3CODE.EQ.-91) W3=REG(10)
                              IF(W3CODE.EQ.-90) W3=REG(11)
                              IF(W3CODE.EQ.-89) W3=REG(12)
                              IF(W3CODE.EQ.-88) W3=REG(13)
                              IF(W3CODE.EQ.-87) W3=REG(14)
                              IF(W3CODE.EQ.-86) W3=REG(15)
                              IF(W3CODE.EQ.-85) W3=REG(16)
                              IF(W3CODE.EQ.-84) W3=REG(17)
                              IF(W3CODE.EQ.-83) W3=REG(18)
                              IF(W3CODE.EQ.-82) W3=REG(19)
                              IF(W3CODE.EQ.-81) W3=REG(20)
                              IF(W3CODE.EQ.-80) W3=REG(40)
                              IF(W3CODE.EQ.-79) W3=REG(30)
                              IF(W3CODE.EQ.-78) W3=REG(21)
                              IF(W3CODE.EQ.-77) W3=REG(22)
                              IF(W3CODE.EQ.-76) W3=REG(23)
                              IF(W3CODE.EQ.-75) W3=REG(24)
                              IF(W3CODE.EQ.-74) W3=REG(25)
                              IF(W3CODE.EQ.-73) W3=REG(26)
                              IF(W3CODE.EQ.-72) W3=REG(27)
                              IF(W3CODE.EQ.-71) W3=REG(28)
                          ELSE
                              W3=GPREG(W3CODE)
                          END IF
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                  END IF
                  IF(W4CODE.NE.0) THEN
                      IF(DF4.EQ.1) THEN
                          IF(W1CODE.LT.0.0D0) THEN
                              IF(W4CODE.EQ.-100) W4=REG(1)
                              IF(W4CODE.EQ.-99) W4=REG(2)
                              IF(W4CODE.EQ.-98) W4=REG(3)
                              IF(W4CODE.EQ.-97) W4=REG(4)
                              IF(W4CODE.EQ.-96) W4=REG(5)
                              IF(W4CODE.EQ.-95) W4=REG(6)
                              IF(W4CODE.EQ.-94) W4=REG(7)
                              IF(W4CODE.EQ.-93) W4=REG(8)
                              IF(W4CODE.EQ.-92) W4=REG(9)
                              IF(W4CODE.EQ.-91) W4=REG(10)
                              IF(W4CODE.EQ.-90) W4=REG(11)
                              IF(W4CODE.EQ.-89) W4=REG(12)
                              IF(W4CODE.EQ.-88) W4=REG(13)
                              IF(W4CODE.EQ.-87) W4=REG(14)
                              IF(W4CODE.EQ.-86) W4=REG(15)
                              IF(W4CODE.EQ.-85) W4=REG(16)
                              IF(W4CODE.EQ.-84) W4=REG(17)
                              IF(W4CODE.EQ.-83) W4=REG(18)
                              IF(W4CODE.EQ.-82) W4=REG(19)
                              IF(W4CODE.EQ.-81) W4=REG(20)
                              IF(W4CODE.EQ.-80) W4=REG(40)
                              IF(W4CODE.EQ.-79) W4=REG(30)
                              IF(W4CODE.EQ.-78) W4=REG(21)
                              IF(W4CODE.EQ.-77) W4=REG(22)
                              IF(W4CODE.EQ.-76) W4=REG(23)
                              IF(W4CODE.EQ.-75) W4=REG(24)
                              IF(W4CODE.EQ.-74) W4=REG(25)
                              IF(W4CODE.EQ.-73) W4=REG(26)
                              IF(W4CODE.EQ.-72) W4=REG(27)
                              IF(W4CODE.EQ.-71) W4=REG(28)
                          ELSE
                              W4=GPREG(W4CODE)
                          END IF
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                  END IF
                  IF(W5CODE.NE.0) THEN
                      IF(DF5.EQ.1) THEN
                          IF(W5CODE.LT.0.0D0) THEN
                              IF(W5CODE.EQ.-100) W5=REG(1)
                              IF(W5CODE.EQ.-99) W5=REG(2)
                              IF(W5CODE.EQ.-98) W5=REG(3)
                              IF(W5CODE.EQ.-97) W5=REG(4)
                              IF(W5CODE.EQ.-96) W5=REG(5)
                              IF(W5CODE.EQ.-95) W5=REG(6)
                              IF(W5CODE.EQ.-94) W5=REG(7)
                              IF(W5CODE.EQ.-93) W5=REG(8)
                              IF(W5CODE.EQ.-92) W5=REG(9)
                              IF(W5CODE.EQ.-91) W5=REG(10)
                              IF(W5CODE.EQ.-90) W5=REG(11)
                              IF(W5CODE.EQ.-89) W5=REG(12)
                              IF(W5CODE.EQ.-88) W5=REG(13)
                              IF(W5CODE.EQ.-87) W5=REG(14)
                              IF(W5CODE.EQ.-86) W5=REG(15)
                              IF(W5CODE.EQ.-85) W5=REG(16)
                              IF(W5CODE.EQ.-84) W5=REG(17)
                              IF(W5CODE.EQ.-83) W5=REG(18)
                              IF(W5CODE.EQ.-82) W5=REG(19)
                              IF(W5CODE.EQ.-81) W5=REG(20)
                              IF(W5CODE.EQ.-80) W5=REG(40)
                              IF(W5CODE.EQ.-79) W5=REG(30)
                              IF(W5CODE.EQ.-78) W5=REG(21)
                              IF(W5CODE.EQ.-77) W5=REG(22)
                              IF(W5CODE.EQ.-76) W5=REG(23)
                              IF(W5CODE.EQ.-75) W5=REG(24)
                              IF(W5CODE.EQ.-74) W5=REG(25)
                              IF(W5CODE.EQ.-73) W5=REG(26)
                              IF(W5CODE.EQ.-72) W5=REG(27)
                              IF(W5CODE.EQ.-71) W5=REG(28)
                          ELSE
                              W5=GPREG(W5CODE)
                          END IF
                          DF5=0
                          S5=1
                          SN=1
                      END IF
                  END IF

C     NOW CLEAR THE CODES
                  W1CODE=0
                  W2CODE=0
                  W3CODE=0
                  W4CODE=0
                  W5CODE=0
              ELSE
C     COMMAND WAS W1 THROUGH W5, CALL WWORD AND RETURN
C     NO MATER WHAT PROGRAM LEVEL WE ARE AT
                  IF(WC.EQ.'W1'.OR.WC.EQ.'W2'.OR.WC.EQ.'W3'.OR.WC.EQ.'W4'
     1            .OR.WC.EQ.'W5') CALL WWORD
                  IF(WC.EQ.'GET') CALL GET
                  IF(WC.EQ.'AGET') CALL AGET
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'A='.OR.WC.EQ.'B='.OR.WC.EQ.'KTEST='.OR.WC.EQ.'LTEST='
     1        .OR.WC.EQ.'C='.OR.WC.EQ.'K='.OR.WC.EQ.'L='.OR.WC.EQ.'M='.OR.
     1        WC.EQ.'D='.OR.WC.EQ.'E='.OR.WC.EQ.'F='.OR.WC.EQ.'N='.OR.
     2        WC.EQ.'G='.OR.WC.EQ.'H='.OR.WC.EQ.'I='.OR.WC.EQ.'MTEST='.OR.
     3        WC.EQ.'J='.OR.WC.EQ.'ITEST='.OR.WC.EQ.'JTEST='.OR.
     4        WC.EQ.'X='.OR.WC.EQ.'Y='.OR.WC.EQ.'Z='.OR.WC.EQ.'NTEST='.OR.
     5        WC.EQ.'T='.OR.WC.EQ.'IX='.OR.WC.EQ.'IY='.OR.
     6        WC.EQ.'IZ='.OR.WC.EQ.'IT=') THEN
                  CALL AUXSET
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'SET') THEN
                  CALL SET
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'STO') THEN
                  CALL GGPREG
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'SAVEREG') THEN
                  CALL GGPREG_SAVE
                  RETURN
              END IF
              IF(WC.EQ.'RCL') THEN
                  CALL GGPREG
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'ARCL') THEN
                  CALL GPRGA
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'STOAX') THEN
                  CALL STOAX
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'PI      '.OR.WC.EQ.'INTGR   '.OR.WC.EQ.'FRAC    '.OR.
     1           WC.EQ.'CHS     '.OR.WC.EQ.'RTD     '.OR.WC.EQ.'DTR     '.OR.
     1           WC.EQ.'ASIN    '.OR.WC.EQ.'ACOS    '.OR.WC.EQ.'PLUS    '.OR.
     1           WC.EQ.'MINUS   '.OR.WC.EQ.'DIV     '.OR.WC.EQ.'MPY     '.OR.
     1           WC.EQ.'MOVE    '.OR.WC.EQ.'ATAN    '.OR.WC.EQ.'RAND    '.OR.
     1           WC.EQ.'SIN     '.OR.WC.EQ.'COS     '.OR.WC.EQ.'TAN     '.OR.
     1           WC.EQ.'TANH    '.OR.WC.EQ.'SINH    '.OR.WC.EQ.'COSH    '.OR.
     1           WC.EQ.'SQRT    '.OR.WC.EQ.'ABS     '.OR.WC.EQ.'EXP     '.OR.
     1           WC.EQ.'RECIP   '.OR.WC.EQ.'MINVAL  '.OR.WC.EQ.'LOG10   '.OR.
     1           WC.EQ.'LN      '.OR.WC.EQ.'POW     '.OR.WC.EQ.'STORE   '.OR.
     1           WC.EQ.'SGN     '.OR.WC.EQ.'CLREG   '.OR.WC.EQ.'MAXVAL  '.OR.
     1           WC.EQ.'WRITE   ') THEN
                  CALL RGMATH
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'ENT'.OR.
     1        WC.EQ.'ENTI'.OR.
     2        WC.EQ.'ENTC'.OR.WC.EQ.'PULL'.OR.
     3        WC.EQ.'IPULL'.OR.WC.EQ.'CPULL'.OR.
     4        WC.EQ.'RUP'.OR.WC.EQ.'IRUP'.OR.WC
     5        .EQ.'CRUP'.OR.WC.EQ.'RDN'.OR.WC.EQ.
     6        'IRDN'.OR.WC.EQ.'CRDN'.OR.WC.EQ.'LASTX'.OR.WC.EQ.
     7        'X-Y'.OR.
     1        WC.EQ.'LASTIX'.OR.WC.EQ.'IX-IY'.OR.WC.EQ.'RE-IM'.OR.
     1        WC.EQ.'CLX'.OR.WC.EQ.'CLIX'.OR.WC.EQ.
     2        'CLSTK'.OR.WC.EQ.'CLSTKI'.OR.WC.EQ.'CLSTKC'.OR.WC.EQ.
     3        '+'.OR.WC.EQ.'-'.OR.WC.EQ.'*'.OR.WC.EQ.'/'.OR.WC.EQ.
     5        'C+'.OR.WC.EQ.'C-'.OR.WC.EQ.'C*'.OR.WC.EQ.'C/'.OR.WC.EQ.
     6        'I+'.OR.WC.EQ.'I-'.OR.WC.EQ.'I*'.OR.WC.EQ.'I/'.OR.
     7        WC.EQ.'Y**X'.OR.WC.EQ.'CY**CX'.OR.
     8        WC.EQ.'IY**IX') THEN
                  CALL STACK
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'INCR    ') THEN
                  CALL INCR
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'MOD') THEN
                  CALL MMOD
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'CLASTO'.OR.WC.EQ.'ASTO'.OR.WC.EQ.'ARCL'.OR.
     1        WC.EQ.'AWRITE') THEN
                  CALL GPRGA
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'CLGREG'.OR.WC.EQ.'CLSTREG'
     1        .OR.WC.EQ.'STADD'.OR.WC.EQ.'STSUB'.OR.
     2        WC.EQ.'STDEV'.OR.WC.EQ.'MEAN'
     3        ) THEN
                  CALL GGPREG
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'ATAN2   ') THEN
                  CALL ATANN2
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'J1      ') THEN
                  CALL BESS
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'PREAD   ') THEN
                  CALL PREAD
                  LASTCOMWRD=WC
                  RETURN
              END IF
              IF(WC.EQ.'ATON') THEN
C       CONVERTS STRING TO NUMBER
                  CALL MACATON
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'CLASTO'.OR.WC.EQ.'ASTO'.OR.WC.EQ.'ARCL'.OR.
     1        WC.EQ.'AWRITE') THEN
                  CALL GPRGA
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'R-P'.OR.WC.EQ.'P-R'.OR.WC.EQ.'R-SP'.OR.
     1        WC.EQ.'SP-R'.OR.WC.EQ.'R-CYL'.OR.WC.EQ.'CYL-R'.OR.
     2        WC.EQ.'H-HMS'.OR.WC.EQ.'HMS-H') THEN
                  CALL COORD
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'IN-MM'.OR.WC.EQ.'IN-CM'.OR.WC.EQ.'IN-M'.OR.
     1        WC.EQ.'MM-IN'.OR.WC.EQ.'CM-IN'.OR.WC.EQ.'M-IN') THEN
                  CALL COORD
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
C     CRAETING A MACRO OR EDITING ONE
          END IF
C
C
C       HERE IS WHERE WE BRANCH TO VARIOUS SUBROUTINES DEPENDING
C       UPON THE COMMAND WORD.
C
C       INITIALIZE MACYES TO ZERO (NOT A MACRO)
C
          MACYES=0
C
          IF(F27.EQ.0.OR.F53.EQ.0.OR.F54.EQ.0) THEN
              FNYES=.FALSE.
              IF(WC.EQ.'FUN01') FNYES=.TRUE.
              IF(WC.EQ.'FUN02') FNYES=.TRUE.
              IF(WC.EQ.'FUN03') FNYES=.TRUE.
              IF(WC.EQ.'FUN04') FNYES=.TRUE.
              IF(WC.EQ.'FUN05') FNYES=.TRUE.
              IF(WC.EQ.'FUN06') FNYES=.TRUE.
              IF(WC.EQ.'FUN07') FNYES=.TRUE.
              IF(WC.EQ.'FUN08') FNYES=.TRUE.
              IF(WC.EQ.'FUN09') FNYES=.TRUE.
              IF(WC.EQ.'FUN10') FNYES=.TRUE.
          END IF
C
C     LOOK FOR RENAMED FUNCTIONS
          IF(WC.EQ.FNAMED(1))  WC='FUN01'
          IF(WC.EQ.FNAMED(2))  WC='FUN02'
          IF(WC.EQ.FNAMED(3))  WC='FUN03'
          IF(WC.EQ.FNAMED(4))  WC='FUN04'
          IF(WC.EQ.FNAMED(5))  WC='FUN05'
          IF(WC.EQ.FNAMED(6))  WC='FUN06'
          IF(WC.EQ.FNAMED(7))  WC='FUN07'
          IF(WC.EQ.FNAMED(8))  WC='FUN08'
          IF(WC.EQ.FNAMED(9))  WC='FUN09'
          IF(WC.EQ.FNAMED(10)) WC='FUN10'
C
C     IF F27 IS 1 OR 2 WE ARE WORKING THE MERIT FUNCTION
C     IF NOT A SPECIFIC ALLOWED COMMAND, INSERT FUNC01 FOR
C     WC AND MAKE OLD WC THE NEW WQ

          IF(F27.EQ.1.OR.F27.EQ.2) THEN
              NOGO=.FALSE.
              OHFUN=.FALSE.
              IF(WC.EQ.'FUNC01  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC02  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC03  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC04  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC05  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC06  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC07  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC08  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC09  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC10  ')   NOGO=.TRUE.
              IF(WC.EQ.'DEL     ')   NOGO=.TRUE.
              IF(WC.EQ.'COR     ')   NOGO=.TRUE.
              IF(WC.EQ.'BYP     ')   NOGO=.TRUE.
              IF(WC.EQ.'BLO     ')   NOGO=.TRUE.
              IF(WC.EQ.'BHI     ')   NOGO=.TRUE.
              IF(WC.EQ.'GTE     ')   NOGO=.TRUE.
              IF(WC.EQ.'LTE     ')   NOGO=.TRUE.
              IF(WC.EQ.'HLD     ')   NOGO=.TRUE.
              IF(WC.EQ.'GET     ')   NOGO=.TRUE.
              IF(WC.EQ.'AGET    ')   NOGO=.TRUE.
              IF(WC.EQ.'RCL     ')   NOGO=.TRUE.
              IF(WC.EQ.'STO     ')   NOGO=.TRUE.
              IF(WC.EQ.'WRITE   ')   NOGO=.TRUE.
              IF(WC.EQ.'MAXVAL  ')   NOGO=.TRUE.
              IF(WC.EQ.'MINVAL  ')   NOGO=.TRUE.
              IF(WC.EQ.'PI      ')   NOGO=.TRUE.
              IF(WC.EQ.'CHS     ')   NOGO=.TRUE.
              IF(WC.EQ.'MOVE    ')   NOGO=.TRUE.
              IF(WC.EQ.'LASTX   ')   NOGO=.TRUE.
              IF(WC.EQ.'LASTIX  ')   NOGO=.TRUE.
              IF(WC.EQ.'INCR    ')   NOGO=.TRUE.
              IF(WC.EQ.'PLUS    ')   NOGO=.TRUE.
              IF(WC.EQ.'MINUS   ')   NOGO=.TRUE.
              IF(WC.EQ.'MPY     ')   NOGO=.TRUE.
              IF(WC.EQ.'DIV     ')   NOGO=.TRUE.
              IF(WC.EQ.'SQRT    ')   NOGO=.TRUE.
              IF(WC.EQ.'SIN     ')   NOGO=.TRUE.
              IF(WC.EQ.'COS     ')   NOGO=.TRUE.
              IF(WC.EQ.'TAN     ')   NOGO=.TRUE.
              IF(WC.EQ.'SINH    ')   NOGO=.TRUE.
              IF(WC.EQ.'COSH    ')   NOGO=.TRUE.
              IF(WC.EQ.'TANH    ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTK   ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTKI  ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTKC  ')   NOGO=.TRUE.
              IF(WC.EQ.'CLREG   ')   NOGO=.TRUE.
              IF(WC.EQ.'CLGREG  ')   NOGO=.TRUE.
              IF(WC.EQ.'CLX     ')   NOGO=.TRUE.
              IF(WC.EQ.'CLIX    ')   NOGO=.TRUE.
              IF(WC.EQ.'ASIN    ')   NOGO=.TRUE.
              IF(WC.EQ.'ACOS    ')   NOGO=.TRUE.
              IF(WC.EQ.'ATAN    ')   NOGO=.TRUE.
              IF(WC.EQ.'ABS     ')   NOGO=.TRUE.
              IF(WC.EQ.'EXP     ')   NOGO=.TRUE.
              IF(WC.EQ.'LN      ')   NOGO=.TRUE.
              IF(WC.EQ.'LOG10   ')   NOGO=.TRUE.
              IF(WC.EQ.'FACT    ')   NOGO=.TRUE.
              IF(WC.EQ.'SGN     ')   NOGO=.TRUE.
              IF(WC.EQ.'RECIP   ')   NOGO=.TRUE.
              IF(WC.EQ.'INTGR   ')   NOGO=.TRUE.
              IF(WC.EQ.'FRAC    ')   NOGO=.TRUE.
              IF(WC.EQ.'POW     ')   NOGO=.TRUE.
              IF(WC.EQ.'STORE   ')   NOGO=.TRUE.
              IF(WC.EQ.'RAND    ')   NOGO=.TRUE.
              IF(WC.EQ.'MOD     ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTREG ')   NOGO=.TRUE.
              IF(WC.EQ.'STADD   ')   NOGO=.TRUE.
              IF(WC.EQ.'STSUB   ')   NOGO=.TRUE.
              IF(WC.EQ.'MEAN    ')   NOGO=.TRUE.
              IF(WC.EQ.'STDEV   ')   NOGO=.TRUE.
              IF(WC.EQ.'RTD     ')   NOGO=.TRUE.
              IF(WC.EQ.'DTR     ')   NOGO=.TRUE.
              IF(WC.EQ.'ATAN2   ')   NOGO=.TRUE.
              IF(WC.EQ.'J1      ')   NOGO=.TRUE.
              IF(WC.EQ.'PREAD   ')   NOGO=.TRUE.
              IF(WC.EQ.'ATON    ')   NOGO=.TRUE.
              IF(WC.EQ.'STOAX   ')   NOGO=.TRUE.
              IF(WC.EQ.'ARCL    ')   NOGO=.TRUE.
              IF(WC.EQ.'AWRITE  ')   NOGO=.TRUE.
              IF(WC.EQ.'ASTO    ')   NOGO=.TRUE.
              IF(WC.EQ.'CLASTO  ')   NOGO=.TRUE.
              IF(WC.EQ.'SHOW    ')   NOGO=.TRUE.
              IF(WC.EQ.'ENT     ')   NOGO=.TRUE.
              IF(WC.EQ.'ENTI    ')   NOGO=.TRUE.
              IF(WC.EQ.'ENTC    ')   NOGO=.TRUE.
              IF(WC.EQ.'PULL    ')   NOGO=.TRUE.
              IF(WC.EQ.'IPULL   ')   NOGO=.TRUE.
              IF(WC.EQ.'CPULL   ')   NOGO=.TRUE.
              IF(WC.EQ.'RUP     ')   NOGO=.TRUE.
              IF(WC.EQ.'IRUP    ')   NOGO=.TRUE.
              IF(WC.EQ.'CRUP    ')   NOGO=.TRUE.
              IF(WC.EQ.'RDN     ')   NOGO=.TRUE.
              IF(WC.EQ.'IRDN    ')   NOGO=.TRUE.
              IF(WC.EQ.'CRDN    ')   NOGO=.TRUE.
              IF(WC.EQ.'X-Y     ')   NOGO=.TRUE.
              IF(WC.EQ.'IX-IY   ')   NOGO=.TRUE.
              IF(WC.EQ.'+       ')   NOGO=.TRUE.
              IF(WC.EQ.'-       ')   NOGO=.TRUE.
              IF(WC.EQ.'*       ')   NOGO=.TRUE.
              IF(WC.EQ.'/       ')   NOGO=.TRUE.
              IF(WC.EQ.'I+      ')   NOGO=.TRUE.
              IF(WC.EQ.'I-      ')   NOGO=.TRUE.
              IF(WC.EQ.'I*      ')   NOGO=.TRUE.
              IF(WC.EQ.'I/      ')   NOGO=.TRUE.
              IF(WC.EQ.'C+      ')   NOGO=.TRUE.
              IF(WC.EQ.'C-      ')   NOGO=.TRUE.
              IF(WC.EQ.'C*      ')   NOGO=.TRUE.
              IF(WC.EQ.'C/      ')   NOGO=.TRUE.
              IF(WC.EQ.'Y**X    ')   NOGO=.TRUE.
              IF(WC.EQ.'IY**IX  ')   NOGO=.TRUE.
              IF(WC.EQ.'CY**CX  ')   NOGO=.TRUE.
              IF(WC.EQ.'P-R     ')   NOGO=.TRUE.
              IF(WC.EQ.'R-P     ')   NOGO=.TRUE.
              IF(WC.EQ.'CYL-R   ')   NOGO=.TRUE.
              IF(WC.EQ.'R-CYL   ')   NOGO=.TRUE.
              IF(WC.EQ.'SP-R    ')   NOGO=.TRUE.
              IF(WC.EQ.'R-SP    ')   NOGO=.TRUE.
              IF(WC.EQ.'RE-IM   ')   NOGO=.TRUE.
              IF(WC.EQ.'IM-RE   ')   NOGO=.TRUE.
              IF(WC.EQ.'H-HMS   ')   NOGO=.TRUE.
              IF(WC.EQ.'HMS-H   ')   NOGO=.TRUE.
              IF(WC.EQ.'IN-MM   ')   NOGO=.TRUE.
              IF(WC.EQ.'IN-CM   ')   NOGO=.TRUE.
              IF(WC.EQ.'IN-M    ')   NOGO=.TRUE.
              IF(WC.EQ.'MM-IN   ')   NOGO=.TRUE.
              IF(WC.EQ.'CM-IN   ')   NOGO=.TRUE.
              IF(WC.EQ.'M-IN    ')   NOGO=.TRUE.
              IF(WC.EQ.'SET     ')   NOGO=.TRUE.
              IF(WC.EQ.'W1      ')   NOGO=.TRUE.
              IF(WC.EQ.'W2      ')   NOGO=.TRUE.
              IF(WC.EQ.'W3      ')   NOGO=.TRUE.
              IF(WC.EQ.'W4      ')   NOGO=.TRUE.
              IF(WC.EQ.'W5      ')   NOGO=.TRUE.
              IF(WC.EQ.'MR      ')   NOGO=.TRUE.
              IF(WC.EQ.'MRA     ')   NOGO=.TRUE.
              IF(WC.EQ.'OP      ')   NOGO=.TRUE.
              IF(WC.EQ.'OPA     ')   NOGO=.TRUE.
              IF(WC.EQ.'CFG     ')   NOGO=.TRUE.
              IF(WC.EQ.'C       ')   NOGO=.TRUE.
              IF(WC.EQ.'M       ')   NOGO=.TRUE.
              IF(WC.EQ.'EOS     ')   NOGO=.TRUE.
              IF(WC.EQ.'?       ')   NOGO=.TRUE.
              IF(WC.EQ.'        ')   NOGO=.TRUE.
              IF(WC.EQ.'OP_DESC ')   NOGO=.TRUE.
C     PREDEFINED OPERAND NAMES SET OHFUN TO TRUE
              IF(WC.EQ.'MACOPT  ')   OHFUN=.TRUE.
              IF(WC.EQ.'X       ')   OHFUN=.TRUE.
              IF(WC.EQ.'Y       ')   OHFUN=.TRUE.
              IF(WC.EQ.'Z       ')   OHFUN=.TRUE.
              IF(WC.EQ.'DCL     ')   OHFUN=.TRUE.
              IF(WC.EQ.'DCM     ')   OHFUN=.TRUE.
              IF(WC.EQ.'DCN     ')   OHFUN=.TRUE.
              IF(WC.EQ.'K       ')   OHFUN=.TRUE.
              IF(WC.EQ.'L       ')   OHFUN=.TRUE.
              IF(WC.EQ.'M       ')   OHFUN=.TRUE.
              IF(WC.EQ.'DX      ') OHFUN=.TRUE.
              IF(WC.EQ.'DY      ') OHFUN=.TRUE.
              IF(WC.EQ.'DR      ') OHFUN=.TRUE.
              IF(WC.EQ.'DXA     ') OHFUN=.TRUE.
              IF(WC.EQ.'DYA     ') OHFUN=.TRUE.
              IF(WC.EQ.'DRA     ') OHFUN=.TRUE.
              IF(WC.EQ.'XANG    ') OHFUN=.TRUE.
              IF(WC.EQ.'YANG    ') OHFUN=.TRUE.
              IF(WC.EQ.'OPL     ') OHFUN=.TRUE.
              IF(WC.EQ.'OPD     ') OHFUN=.TRUE.
              IF(WC.EQ.'OPDW    ') OHFUN=.TRUE.
              IF(WC.EQ.'LOLD    ') OHFUN=.TRUE.
              IF(WC.EQ.'MOLD    ') OHFUN=.TRUE.
              IF(WC.EQ.'NOLD    ') OHFUN=.TRUE.
              IF(WC.EQ.'LEN     ') OHFUN=.TRUE.
              IF(WC.EQ.'AII     ') OHFUN=.TRUE.
              IF(WC.EQ.'AIP     ') OHFUN=.TRUE.
              IF(WC.EQ.'LN      ') OHFUN=.TRUE.
              IF(WC.EQ.'MN      ') OHFUN=.TRUE.
              IF(WC.EQ.'NN      ') OHFUN=.TRUE.
              IF(WC.EQ.'PXPX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PXPY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PYPX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PYPY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PXAPX   ') OHFUN=.TRUE.
              IF(WC.EQ.'PXAPY   ') OHFUN=.TRUE.
              IF(WC.EQ.'PYAPX   ') OHFUN=.TRUE.
              IF(WC.EQ.'PYAPY   ') OHFUN=.TRUE.
              IF(WC.EQ.'DXDX    ') OHFUN=.TRUE.
              IF(WC.EQ.'DXDY    ') OHFUN=.TRUE.
              IF(WC.EQ.'DYDX    ') OHFUN=.TRUE.
              IF(WC.EQ.'DYDY    ') OHFUN=.TRUE.
              IF(WC.EQ.'DXADX   ') OHFUN=.TRUE.
              IF(WC.EQ.'DXADY   ') OHFUN=.TRUE.
              IF(WC.EQ.'DYADX   ') OHFUN=.TRUE.
              IF(WC.EQ.'DYADY   ') OHFUN=.TRUE.
              IF(WC.EQ.'XREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'YREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'ZREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'LREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'MACOPT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'NREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'LREFOL  ') OHFUN=.TRUE.
              IF(WC.EQ.'MREFOL  ') OHFUN=.TRUE.
              IF(WC.EQ.'NREFOL  ') OHFUN=.TRUE.
              IF(WC.EQ.'IREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'IPREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'YAREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'LNREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'MNREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'NNREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'GLX     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLY     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLZ     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLL     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLM     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLN     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLLOLD  ') OHFUN=.TRUE.
              IF(WC.EQ.'GLMOLD  ') OHFUN=.TRUE.
              IF(WC.EQ.'GLNOLD  ') OHFUN=.TRUE.
              IF(WC.EQ.'LENREF  ') OHFUN=.TRUE.
              IF(WC.EQ.'OPLREF  ') OHFUN=.TRUE.
              IF(WC.EQ.'RD      ') OHFUN=.TRUE.
              IF(WC.EQ.'CV      ') OHFUN=.TRUE.
              IF(WC.EQ.'TH      ') OHFUN=.TRUE.
              IF(WC.EQ.'THM     ') OHFUN=.TRUE.
              IF(WC.EQ.'PRICE   ') OHFUN=.TRUE.
              IF(WC.EQ.'CC      ') OHFUN=.TRUE.
              IF(WC.EQ.'AC      ') OHFUN=.TRUE.
              IF(WC.EQ.'AD      ') OHFUN=.TRUE.
              IF(WC.EQ.'AE      ') OHFUN=.TRUE.
              IF(WC.EQ.'AF      ') OHFUN=.TRUE.
              IF(WC.EQ.'AG      ') OHFUN=.TRUE.
              IF(WC.EQ.'AH      ') OHFUN=.TRUE.
              IF(WC.EQ.'AI      ') OHFUN=.TRUE.
              IF(WC.EQ.'AJ      ') OHFUN=.TRUE.
              IF(WC.EQ.'AK      ') OHFUN=.TRUE.
              IF(WC.EQ.'AL      ') OHFUN=.TRUE.
              IF(WC.EQ.'RDTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'CVTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'GRS     ') OHFUN=.TRUE.
              IF(WC.EQ.'CCTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'ADTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'AETOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'AFTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'AGTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'ALPHA   ') OHFUN=.TRUE.
              IF(WC.EQ.'BETA    ') OHFUN=.TRUE.
              IF(WC.EQ.'GAMMA   ') OHFUN=.TRUE.
              IF(WC.EQ.'GDX     ') OHFUN=.TRUE.
              IF(WC.EQ.'GDY     ') OHFUN=.TRUE.
              IF(WC.EQ.'GDZ     ') OHFUN=.TRUE.
              IF(WC.EQ.'GALPHA  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBETA   ') OHFUN=.TRUE.
              IF(WC.EQ.'GGAMMA  ') OHFUN=.TRUE.
              IF(WC.EQ.'VNUM    ') OHFUN=.TRUE.
              IF(WC.EQ.'DPART   ') OHFUN=.TRUE.
              IF(WC.EQ.'ABBE    ') OHFUN=.TRUE.
              IF(WC.EQ.'PARTL   ') OHFUN=.TRUE.
              IF(WC.EQ.'INDEX   ') OHFUN=.TRUE.
              IF(WC.EQ.'N1      ') OHFUN=.TRUE.
              IF(WC.EQ.'N2      ') OHFUN=.TRUE.
              IF(WC.EQ.'N3      ') OHFUN=.TRUE.
              IF(WC.EQ.'N4      ') OHFUN=.TRUE.
              IF(WC.EQ.'N5      ') OHFUN=.TRUE.
              IF(WC.EQ.'N6      ') OHFUN=.TRUE.
              IF(WC.EQ.'N7      ') OHFUN=.TRUE.
              IF(WC.EQ.'N8      ') OHFUN=.TRUE.
              IF(WC.EQ.'N9      ') OHFUN=.TRUE.
              IF(WC.EQ.'N10     ') OHFUN=.TRUE.
              IF(WC.EQ.'XD      ') OHFUN=.TRUE.
              IF(WC.EQ.'YD      ') OHFUN=.TRUE.
              IF(WC.EQ.'ZD      ') OHFUN=.TRUE.
              IF(WC.EQ.'PIVX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIVY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIVZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'XVERT   ') OHFUN=.TRUE.
              IF(WC.EQ.'YVERT   ') OHFUN=.TRUE.
              IF(WC.EQ.'ZVERT   ') OHFUN=.TRUE.
              IF(WC.EQ.'LXVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MXVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'NXVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'LYVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MYVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'NYVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'LZVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MZVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'NZVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'LENGTH  ') OHFUN=.TRUE.
              IF(WC.EQ.'OAL     ') OHFUN=.TRUE.
              IF(WC.EQ.'MLENGTH ') OHFUN=.TRUE.
              IF(WC.EQ.'OPTLEN  ') OHFUN=.TRUE.
              IF(WC.EQ.'WEIGHT  ') OHFUN=.TRUE.
              IF(WC.EQ.'ET      ') OHFUN=.TRUE.
              IF(WC.EQ.'ETY     ') OHFUN=.TRUE.
              IF(WC.EQ.'ETX     ') OHFUN=.TRUE.
              IF(WC.EQ.'SHAPEFAC') OHFUN=.TRUE.
              IF(WC.EQ.'C1      ') OHFUN=.TRUE.
              IF(WC.EQ.'C2      ') OHFUN=.TRUE.
              IF(WC.EQ.'C3      ') OHFUN=.TRUE.
              IF(WC.EQ.'C4      ') OHFUN=.TRUE.
              IF(WC.EQ.'C5      ') OHFUN=.TRUE.
              IF(WC.EQ.'C6      ') OHFUN=.TRUE.
              IF(WC.EQ.'C7      ') OHFUN=.TRUE.
              IF(WC.EQ.'C8      ') OHFUN=.TRUE.
              IF(WC.EQ.'C9      ') OHFUN=.TRUE.
              IF(WC.EQ.'C10     ') OHFUN=.TRUE.
              IF(WC.EQ.'C11     ') OHFUN=.TRUE.
              IF(WC.EQ.'C12     ') OHFUN=.TRUE.
              IF(WC.EQ.'C13     ') OHFUN=.TRUE.
              IF(WC.EQ.'C14     ') OHFUN=.TRUE.
              IF(WC.EQ.'C15     ') OHFUN=.TRUE.
              IF(WC.EQ.'C16     ') OHFUN=.TRUE.
              IF(WC.EQ.'C17     ') OHFUN=.TRUE.
              IF(WC.EQ.'C18     ') OHFUN=.TRUE.
              IF(WC.EQ.'C19     ') OHFUN=.TRUE.
              IF(WC.EQ.'C20     ') OHFUN=.TRUE.
              IF(WC.EQ.'C21     ') OHFUN=.TRUE.
              IF(WC.EQ.'C22     ') OHFUN=.TRUE.
              IF(WC.EQ.'C23     ') OHFUN=.TRUE.
              IF(WC.EQ.'C24     ') OHFUN=.TRUE.
              IF(WC.EQ.'C25     ') OHFUN=.TRUE.
              IF(WC.EQ.'C26     ') OHFUN=.TRUE.
              IF(WC.EQ.'C27     ') OHFUN=.TRUE.
              IF(WC.EQ.'C28     ') OHFUN=.TRUE.
              IF(WC.EQ.'C29     ') OHFUN=.TRUE.
              IF(WC.EQ.'C30     ') OHFUN=.TRUE.
              IF(WC.EQ.'C31     ') OHFUN=.TRUE.
              IF(WC.EQ.'C32     ') OHFUN=.TRUE.
              IF(WC.EQ.'C33     ') OHFUN=.TRUE.
              IF(WC.EQ.'C34     ') OHFUN=.TRUE.
              IF(WC.EQ.'C35     ') OHFUN=.TRUE.
              IF(WC.EQ.'C36     ') OHFUN=.TRUE.
              IF(WC.EQ.'C37     ') OHFUN=.TRUE.
              IF(WC.EQ.'C38     ') OHFUN=.TRUE.
              IF(WC.EQ.'C39     ') OHFUN=.TRUE.
              IF(WC.EQ.'C40     ') OHFUN=.TRUE.
              IF(WC.EQ.'C41     ') OHFUN=.TRUE.
              IF(WC.EQ.'C42     ') OHFUN=.TRUE.
              IF(WC.EQ.'C43     ') OHFUN=.TRUE.
              IF(WC.EQ.'C44     ') OHFUN=.TRUE.
              IF(WC.EQ.'C45     ') OHFUN=.TRUE.
              IF(WC.EQ.'C46     ') OHFUN=.TRUE.
              IF(WC.EQ.'C47     ') OHFUN=.TRUE.
              IF(WC.EQ.'C48     ') OHFUN=.TRUE.
              IF(WC.EQ.'C49     ') OHFUN=.TRUE.
              IF(WC.EQ.'C50     ') OHFUN=.TRUE.
              IF(WC.EQ.'C51     ') OHFUN=.TRUE.
              IF(WC.EQ.'C52     ') OHFUN=.TRUE.
              IF(WC.EQ.'C53     ') OHFUN=.TRUE.
              IF(WC.EQ.'C54     ') OHFUN=.TRUE.
              IF(WC.EQ.'C55     ') OHFUN=.TRUE.
              IF(WC.EQ.'C56     ') OHFUN=.TRUE.
              IF(WC.EQ.'C57     ') OHFUN=.TRUE.
              IF(WC.EQ.'C58     ') OHFUN=.TRUE.
              IF(WC.EQ.'C59     ') OHFUN=.TRUE.
              IF(WC.EQ.'C60     ') OHFUN=.TRUE.
              IF(WC.EQ.'C61     ') OHFUN=.TRUE.
              IF(WC.EQ.'C62     ') OHFUN=.TRUE.
              IF(WC.EQ.'C63     ') OHFUN=.TRUE.
              IF(WC.EQ.'C64     ') OHFUN=.TRUE.
              IF(WC.EQ.'C65     ') OHFUN=.TRUE.
              IF(WC.EQ.'C66     ') OHFUN=.TRUE.
              IF(WC.EQ.'C67     ') OHFUN=.TRUE.
              IF(WC.EQ.'C68     ') OHFUN=.TRUE.
              IF(WC.EQ.'C69     ') OHFUN=.TRUE.
              IF(WC.EQ.'C70     ') OHFUN=.TRUE.
              IF(WC.EQ.'C71     ') OHFUN=.TRUE.
              IF(WC.EQ.'C72     ') OHFUN=.TRUE.
              IF(WC.EQ.'C73     ') OHFUN=.TRUE.
              IF(WC.EQ.'C74     ') OHFUN=.TRUE.
              IF(WC.EQ.'C75     ') OHFUN=.TRUE.
              IF(WC.EQ.'C76     ') OHFUN=.TRUE.
              IF(WC.EQ.'C77     ') OHFUN=.TRUE.
              IF(WC.EQ.'C78     ') OHFUN=.TRUE.
              IF(WC.EQ.'C79     ') OHFUN=.TRUE.
              IF(WC.EQ.'C80     ') OHFUN=.TRUE.
              IF(WC.EQ.'C81     ') OHFUN=.TRUE.
              IF(WC.EQ.'C82     ') OHFUN=.TRUE.
              IF(WC.EQ.'C83     ') OHFUN=.TRUE.
              IF(WC.EQ.'C84     ') OHFUN=.TRUE.
              IF(WC.EQ.'C85     ') OHFUN=.TRUE.
              IF(WC.EQ.'C86     ') OHFUN=.TRUE.
              IF(WC.EQ.'C87     ') OHFUN=.TRUE.
              IF(WC.EQ.'C88     ') OHFUN=.TRUE.
              IF(WC.EQ.'C89     ') OHFUN=.TRUE.
              IF(WC.EQ.'C90     ') OHFUN=.TRUE.
              IF(WC.EQ.'C91     ') OHFUN=.TRUE.
              IF(WC.EQ.'C92     ') OHFUN=.TRUE.
              IF(WC.EQ.'C93     ') OHFUN=.TRUE.
              IF(WC.EQ.'C94     ') OHFUN=.TRUE.
              IF(WC.EQ.'C95     ') OHFUN=.TRUE.
              IF(WC.EQ.'C96     ') OHFUN=.TRUE.
              IF(WC.EQ.'PWRY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PWRX    ') OHFUN=.TRUE.
              IF(WC.EQ.'FLCLTHY ') OHFUN=.TRUE.
              IF(WC.EQ.'FLCLTHX ') OHFUN=.TRUE.
              IF(WC.EQ.'FLCLTH  ') OHFUN=.TRUE.
              IF(WC.EQ.'PY      ') OHFUN=.TRUE.
              IF(WC.EQ.'PX      ') OHFUN=.TRUE.
              IF(WC.EQ.'PCY     ') OHFUN=.TRUE.
              IF(WC.EQ.'PCX     ') OHFUN=.TRUE.
              IF(WC.EQ.'PUY     ') OHFUN=.TRUE.
              IF(WC.EQ.'PUX     ') OHFUN=.TRUE.
              IF(WC.EQ.'PUCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PUCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIY     ') OHFUN=.TRUE.
              IF(WC.EQ.'PIX     ') OHFUN=.TRUE.
              IF(WC.EQ.'PICY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PICX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIYP    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIXP    ') OHFUN=.TRUE.
              IF(WC.EQ.'PICYP   ') OHFUN=.TRUE.
              IF(WC.EQ.'PICXP   ') OHFUN=.TRUE.
              IF(WC.EQ.'PACY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PACX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACY    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'IMDISX  ') OHFUN=.TRUE.
              IF(WC.EQ.'IMDISY  ') OHFUN=.TRUE.
              IF(WC.EQ.'CENTX   ') OHFUN=.TRUE.
              IF(WC.EQ.'CENTY   ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSX    ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSY    ') OHFUN=.TRUE.
              IF(WC.EQ.'RMS     ') OHFUN=.TRUE.
              IF(WC.EQ.'RSSX    ') OHFUN=.TRUE.
              IF(WC.EQ.'RSSY    ') OHFUN=.TRUE.
              IF(WC.EQ.'RSS     ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSOPD  ') OHFUN=.TRUE.
              IF(WC.EQ.'ZERN37  ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGX    ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGY    ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGXOR  ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGYOR  ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFLX    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFLY    ') OHFUN=.TRUE.
              IF(WC.EQ.'FFNX    ') OHFUN=.TRUE.
              IF(WC.EQ.'FFNY    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFNX    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFNY    ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
              IF(WC.EQ.'ENDIAX  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENDIAY  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXDIAX  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXDIAY  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPOSX  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPOSY  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPOSZ  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPOSX  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPOSY  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPOSZ  ') OHFUN=.TRUE.
              IF(WC.EQ.'FNUMX   ') OHFUN=.TRUE.
              IF(WC.EQ.'FNUMY   ') OHFUN=.TRUE.
              IF(WC.EQ.'OBFNUMX ') OHFUN=.TRUE.
              IF(WC.EQ.'ONFNUMY ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPDIAX ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPDIAY ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPDIAX ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPDIAY ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDIAX ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDIAY ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDISX ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDISY ') OHFUN=.TRUE.
              IF(WC.EQ.'CHFIMX  ') OHFUN=.TRUE.
              IF(WC.EQ.'CHFIMY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GPX     ') OHFUN=.TRUE.
              IF(WC.EQ.'GPY     ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUX    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUY    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUCX   ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUCY   ') OHFUN=.TRUE.
              IF(WC.EQ.'DIST    ') OHFUN=.TRUE.
              IF(WC.EQ.'FISHDIST') OHFUN=.TRUE.
              IF(WC.EQ.'XFOC    ') OHFUN=.TRUE.
              IF(WC.EQ.'YFOC    ') OHFUN=.TRUE.
              IF(WC.EQ.'AST     ') OHFUN=.TRUE.
              IF(WC.EQ.'SA3     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA3    ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA3   ') OHFUN=.TRUE.
              IF(WC.EQ.'AST3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST3   ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS3   ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ3   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5    ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5   ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5   ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5   ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5   ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSA  ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSA  ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMA  ') OHFUN=.TRUE.
              IF(WC.EQ.'TAS     ') OHFUN=.TRUE.
              IF(WC.EQ.'XTAS    ') OHFUN=.TRUE.
              IF(WC.EQ.'SAS     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSAS    ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7    ') OHFUN=.TRUE.
              IF(WC.EQ.'SA3P    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5P    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSAP  ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSAP ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSAP  ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSAP ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMAP  ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMAP ') OHFUN=.TRUE.
              IF(WC.EQ.'TASP    ') OHFUN=.TRUE.
              IF(WC.EQ.'XTASP   ') OHFUN=.TRUE.
              IF(WC.EQ.'SASP    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSASP   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7P    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7P   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA3S    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5S    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSAS  ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSAS ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSAS  ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSAS ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMAS  ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMAS ') OHFUN=.TRUE.
              IF(WC.EQ.'TASS    ') OHFUN=.TRUE.
              IF(WC.EQ.'XTASS   ') OHFUN=.TRUE.
              IF(WC.EQ.'SASS    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSASS   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7S    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7S   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5I    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSAI  ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSAI ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSAI ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMAI  ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMAI ') OHFUN=.TRUE.
              IF(WC.EQ.'TASI    ') OHFUN=.TRUE.
              IF(WC.EQ.'XTASI   ') OHFUN=.TRUE.
              IF(WC.EQ.'SASI    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSASI   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7I    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7I   ') OHFUN=.TRUE.
              IF(WC.EQ.'PSA3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XPSA3   ') OHFUN=.TRUE.
              IF(WC.EQ.'PCMA3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPCMA3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PAST3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPAST3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PDIS3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPDIS3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PPTZ3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPPTZ3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PSA3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPSA3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'PCMA3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPCMA3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PAST3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPAST3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PDIS3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPDIS3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PPTZ3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPPTZ3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PSA3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPSA3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'PCMA3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPCMA3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PAST3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPAST3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PDIS3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPDIS3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PPTZ3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPPTZ3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZCV   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZCV  ') OHFUN=.TRUE.
              IF(WC.EQ.'MGOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'PGOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'MDOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'PDOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'GOTFM   ') OHFUN=.TRUE.
              IF(WC.EQ.'GOTFP   ') OHFUN=.TRUE.
              IF(WC.EQ.'DOTFM  ') OHFUN=.TRUE.
              IF(WC.EQ.'DOTFP  ') OHFUN=.TRUE.
              IF(WC.EQ.'RED     ') OHFUN=.TRUE.
              IF(WC.EQ.'REDCEN  ') OHFUN=.TRUE.
              IF(WC.EQ.'SYMX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SYMY    ') OHFUN=.TRUE.
              IF(WC.EQ.'ASYMX   ') OHFUN=.TRUE.
              IF(WC.EQ.'ASYMY   ') OHFUN=.TRUE.
              IF(WC.EQ.'CTSX    ') OHFUN=.TRUE.
              IF(WC.EQ.'CTSY    ') OHFUN=.TRUE.
              IF(WC.EQ.'SCEX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SCEY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PACM    ') OHFUN=.TRUE.
              IF(WC.EQ.'PACZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACM    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCM    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCM    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'GREYS   ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRADX  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRADY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBDISX  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBDISY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRCVX  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRCVY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBWAISTX') OHFUN=.TRUE.
              IF(WC.EQ.'GBWAISTY') OHFUN=.TRUE.
              IF(WC.EQ.'CLPY    ') OHFUN=.TRUE.
              IF(WC.EQ.'CLPX    ') OHFUN=.TRUE.
              IF(WC.EQ.'DMINUSD ') OHFUN=.TRUE.
              IF(WC.EQ.'COST    ') OHFUN=.TRUE.
              IF(WC.EQ.'ACT     ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSYX   ') OHFUN=.TRUE.
              IF(WC.EQ.'CLEARX  ') OHFUN=.TRUE.
              IF(WC.EQ.'CLEARY  ') OHFUN=.TRUE.
              IF(.NOT.NOGO.AND..NOT.OHFUN) THEN
                  OUTLYNE='INVALID OPERAND ENTRY'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(.NOT.NOGO.AND.OHFUN) THEN
                  WQ=WC
                  SQ=1
                  WC='FUNC00'
C               NO REPLACEMENT DONE
                  NOGO=.FALSE.
                  OHFUN=.FALSE.
              END IF
C     NOT IN MERIT OR UPDATE MERIT
          END IF
C     IF F53 IS 1 OR 2 WE ARE WORKING THE TOPER FUNCTION
C     IF NOT A SPECIFIC ALLOWED COMMAND, INSERT FUNC01 FOR
C     WC AND MAKE OLD WC THE NEW WQ
          IF(F53.EQ.1.OR.F53.EQ.2) THEN
              NOGO=.FALSE.
              OHFUN=.FALSE.
              IF(WC.EQ.'FUNC01  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC02  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC03  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC04  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC05  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC06  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC07  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC08  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC09  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC10  ')   NOGO=.TRUE.
              IF(WC.EQ.'DEL     ')   NOGO=.TRUE.
              IF(WC.EQ.'GET     ')   NOGO=.TRUE.
              IF(WC.EQ.'AGET    ')   NOGO=.TRUE.
              IF(WC.EQ.'RCL     ')   NOGO=.TRUE.
              IF(WC.EQ.'STO     ')   NOGO=.TRUE.
              IF(WC.EQ.'WRITE   ')   NOGO=.TRUE.
              IF(WC.EQ.'MAXVAL  ')   NOGO=.TRUE.
              IF(WC.EQ.'MINVAL  ')   NOGO=.TRUE.
              IF(WC.EQ.'PI      ')   NOGO=.TRUE.
              IF(WC.EQ.'CHS     ')   NOGO=.TRUE.
              IF(WC.EQ.'MOVE    ')   NOGO=.TRUE.
              IF(WC.EQ.'LASTX   ')   NOGO=.TRUE.
              IF(WC.EQ.'LASTIX  ')   NOGO=.TRUE.
              IF(WC.EQ.'INCR    ')   NOGO=.TRUE.
              IF(WC.EQ.'PLUS    ')   NOGO=.TRUE.
              IF(WC.EQ.'MINUS   ')   NOGO=.TRUE.
              IF(WC.EQ.'MPY     ')   NOGO=.TRUE.
              IF(WC.EQ.'DIV     ')   NOGO=.TRUE.
              IF(WC.EQ.'SQRT    ')   NOGO=.TRUE.
              IF(WC.EQ.'SIN     ')   NOGO=.TRUE.
              IF(WC.EQ.'COS     ')   NOGO=.TRUE.
              IF(WC.EQ.'TAN     ')   NOGO=.TRUE.
              IF(WC.EQ.'SINH    ')   NOGO=.TRUE.
              IF(WC.EQ.'COSH    ')   NOGO=.TRUE.
              IF(WC.EQ.'TANH    ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTK   ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTKI  ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTKC  ')   NOGO=.TRUE.
              IF(WC.EQ.'CLREG   ')   NOGO=.TRUE.
              IF(WC.EQ.'CLGREG  ')   NOGO=.TRUE.
              IF(WC.EQ.'CLX     ')   NOGO=.TRUE.
              IF(WC.EQ.'CLIX    ')   NOGO=.TRUE.
              IF(WC.EQ.'ASIN    ')   NOGO=.TRUE.
              IF(WC.EQ.'ACOS    ')   NOGO=.TRUE.
              IF(WC.EQ.'ATAN    ')   NOGO=.TRUE.
              IF(WC.EQ.'ABS     ')   NOGO=.TRUE.
              IF(WC.EQ.'EXP     ')   NOGO=.TRUE.
              IF(WC.EQ.'LN      ')   NOGO=.TRUE.
              IF(WC.EQ.'LOG10   ')   NOGO=.TRUE.
              IF(WC.EQ.'FACT    ')   NOGO=.TRUE.
              IF(WC.EQ.'SGN     ')   NOGO=.TRUE.
              IF(WC.EQ.'RECIP   ')   NOGO=.TRUE.
              IF(WC.EQ.'INTGR   ')   NOGO=.TRUE.
              IF(WC.EQ.'FRAC    ')   NOGO=.TRUE.
              IF(WC.EQ.'POW     ')   NOGO=.TRUE.
              IF(WC.EQ.'STORE   ')   NOGO=.TRUE.
              IF(WC.EQ.'RAND    ')   NOGO=.TRUE.
              IF(WC.EQ.'MOD     ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTREG ')   NOGO=.TRUE.
              IF(WC.EQ.'STADD   ')   NOGO=.TRUE.
              IF(WC.EQ.'STSUB   ')   NOGO=.TRUE.
              IF(WC.EQ.'MEAN    ')   NOGO=.TRUE.
              IF(WC.EQ.'STDEV   ')   NOGO=.TRUE.
              IF(WC.EQ.'RTD     ')   NOGO=.TRUE.
              IF(WC.EQ.'DTR     ')   NOGO=.TRUE.
              IF(WC.EQ.'ATAN2   ')   NOGO=.TRUE.
              IF(WC.EQ.'J1      ')   NOGO=.TRUE.
              IF(WC.EQ.'PREAD   ')   NOGO=.TRUE.
              IF(WC.EQ.'ATON    ')   NOGO=.TRUE.
              IF(WC.EQ.'STOAX   ')   NOGO=.TRUE.
              IF(WC.EQ.'ARCL    ')   NOGO=.TRUE.
              IF(WC.EQ.'AWRITE  ')   NOGO=.TRUE.
              IF(WC.EQ.'ASTO    ')   NOGO=.TRUE.
              IF(WC.EQ.'CLASTO  ')   NOGO=.TRUE.
              IF(WC.EQ.'SHOW    ')   NOGO=.TRUE.
              IF(WC.EQ.'ENT     ')   NOGO=.TRUE.
              IF(WC.EQ.'ENTI    ')   NOGO=.TRUE.
              IF(WC.EQ.'ENTC    ')   NOGO=.TRUE.
              IF(WC.EQ.'PULL    ')   NOGO=.TRUE.
              IF(WC.EQ.'IPULL   ')   NOGO=.TRUE.
              IF(WC.EQ.'CPULL   ')   NOGO=.TRUE.
              IF(WC.EQ.'RUP     ')   NOGO=.TRUE.
              IF(WC.EQ.'IRUP    ')   NOGO=.TRUE.
              IF(WC.EQ.'CRUP    ')   NOGO=.TRUE.
              IF(WC.EQ.'RDN     ')   NOGO=.TRUE.
              IF(WC.EQ.'IRDN    ')   NOGO=.TRUE.
              IF(WC.EQ.'CRDN    ')   NOGO=.TRUE.
              IF(WC.EQ.'X-Y     ')   NOGO=.TRUE.
              IF(WC.EQ.'IX-IY   ')   NOGO=.TRUE.
              IF(WC.EQ.'+       ')   NOGO=.TRUE.
              IF(WC.EQ.'-       ')   NOGO=.TRUE.
              IF(WC.EQ.'*       ')   NOGO=.TRUE.
              IF(WC.EQ.'/       ')   NOGO=.TRUE.
              IF(WC.EQ.'I+      ')   NOGO=.TRUE.
              IF(WC.EQ.'I-      ')   NOGO=.TRUE.
              IF(WC.EQ.'I*      ')   NOGO=.TRUE.
              IF(WC.EQ.'I/      ')   NOGO=.TRUE.
              IF(WC.EQ.'C+      ')   NOGO=.TRUE.
              IF(WC.EQ.'C-      ')   NOGO=.TRUE.
              IF(WC.EQ.'C*      ')   NOGO=.TRUE.
              IF(WC.EQ.'C/      ')   NOGO=.TRUE.
              IF(WC.EQ.'Y**X    ')   NOGO=.TRUE.
              IF(WC.EQ.'IY**IX  ')   NOGO=.TRUE.
              IF(WC.EQ.'CY**CX  ')   NOGO=.TRUE.
              IF(WC.EQ.'P-R     ')   NOGO=.TRUE.
              IF(WC.EQ.'R-P     ')   NOGO=.TRUE.
              IF(WC.EQ.'CYL-R   ')   NOGO=.TRUE.
              IF(WC.EQ.'R-CYL   ')   NOGO=.TRUE.
              IF(WC.EQ.'SP-R    ')   NOGO=.TRUE.
              IF(WC.EQ.'R-SP    ')   NOGO=.TRUE.
              IF(WC.EQ.'RE-IM   ')   NOGO=.TRUE.
              IF(WC.EQ.'IM-RE   ')   NOGO=.TRUE.
              IF(WC.EQ.'H-HMS   ')   NOGO=.TRUE.
              IF(WC.EQ.'HMS-H   ')   NOGO=.TRUE.
              IF(WC.EQ.'IN-MM   ')   NOGO=.TRUE.
              IF(WC.EQ.'IN-CM   ')   NOGO=.TRUE.
              IF(WC.EQ.'IN-M    ')   NOGO=.TRUE.
              IF(WC.EQ.'MM-IN   ')   NOGO=.TRUE.
              IF(WC.EQ.'CM-IN   ')   NOGO=.TRUE.
              IF(WC.EQ.'M-IN    ')   NOGO=.TRUE.
              IF(WC.EQ.'SET     ')   NOGO=.TRUE.
              IF(WC.EQ.'W1      ')   NOGO=.TRUE.
              IF(WC.EQ.'W2      ')   NOGO=.TRUE.
              IF(WC.EQ.'W3      ')   NOGO=.TRUE.
              IF(WC.EQ.'W4      ')   NOGO=.TRUE.
              IF(WC.EQ.'W5      ')   NOGO=.TRUE.
              IF(WC.EQ.'TOPS    ')   NOGO=.TRUE.
              IF(WC.EQ.'C       ')   NOGO=.TRUE.
              IF(WC.EQ.'M       ')   NOGO=.TRUE.
              IF(WC.EQ.'EOS     ')   NOGO=.TRUE.
              IF(WC.EQ.'?       ')   NOGO=.TRUE.
              IF(WC.EQ.'        ')   NOGO=.TRUE.
              IF(WC.EQ.'OP_DESC ')   NOGO=.TRUE.
C     PREDEFINED TOL OPERAND NAMES SET OHFUN TO TRUE
              IF(WC.EQ.'X       ')   OHFUN=.TRUE.
              IF(WC.EQ.'Y       ')   OHFUN=.TRUE.
              IF(WC.EQ.'Z       ')   OHFUN=.TRUE.
              IF(WC.EQ.'DCL     ')   OHFUN=.TRUE.
              IF(WC.EQ.'DCM     ')   OHFUN=.TRUE.
              IF(WC.EQ.'DCN     ')   OHFUN=.TRUE.
              IF(WC.EQ.'K       ')   OHFUN=.TRUE.
              IF(WC.EQ.'L       ')   OHFUN=.TRUE.
              IF(WC.EQ.'M       ')   OHFUN=.TRUE.
              IF(WC.EQ.'DX      ') OHFUN=.TRUE.
              IF(WC.EQ.'DY      ') OHFUN=.TRUE.
              IF(WC.EQ.'DR      ') OHFUN=.TRUE.
              IF(WC.EQ.'DXA     ') OHFUN=.TRUE.
              IF(WC.EQ.'DYA     ') OHFUN=.TRUE.
              IF(WC.EQ.'DRA     ') OHFUN=.TRUE.
              IF(WC.EQ.'XANG    ') OHFUN=.TRUE.
              IF(WC.EQ.'YANG    ') OHFUN=.TRUE.
              IF(WC.EQ.'OPL     ') OHFUN=.TRUE.
              IF(WC.EQ.'OPD     ') OHFUN=.TRUE.
              IF(WC.EQ.'OPDW    ') OHFUN=.TRUE.
              IF(WC.EQ.'LOLD    ') OHFUN=.TRUE.
              IF(WC.EQ.'MOLD    ') OHFUN=.TRUE.
              IF(WC.EQ.'NOLD    ') OHFUN=.TRUE.
              IF(WC.EQ.'LEN     ') OHFUN=.TRUE.
              IF(WC.EQ.'AII     ') OHFUN=.TRUE.
              IF(WC.EQ.'AIP     ') OHFUN=.TRUE.
              IF(WC.EQ.'LN      ') OHFUN=.TRUE.
              IF(WC.EQ.'MN      ') OHFUN=.TRUE.
              IF(WC.EQ.'NN      ') OHFUN=.TRUE.
              IF(WC.EQ.'PXPX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PXPY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PYPX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PYPY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PXAPX   ') OHFUN=.TRUE.
              IF(WC.EQ.'PXAPY   ') OHFUN=.TRUE.
              IF(WC.EQ.'PYAPX   ') OHFUN=.TRUE.
              IF(WC.EQ.'PYAPY   ') OHFUN=.TRUE.
              IF(WC.EQ.'DXDX    ') OHFUN=.TRUE.
              IF(WC.EQ.'DXDY    ') OHFUN=.TRUE.
              IF(WC.EQ.'DYDX    ') OHFUN=.TRUE.
              IF(WC.EQ.'DYDY    ') OHFUN=.TRUE.
              IF(WC.EQ.'DXADX   ') OHFUN=.TRUE.
              IF(WC.EQ.'DXADY   ') OHFUN=.TRUE.
              IF(WC.EQ.'DYADX   ') OHFUN=.TRUE.
              IF(WC.EQ.'DYADY   ') OHFUN=.TRUE.
              IF(WC.EQ.'XREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'YREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'ZREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'LREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'MACOPT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'NREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'LREFOL  ') OHFUN=.TRUE.
              IF(WC.EQ.'MREFOL  ') OHFUN=.TRUE.
              IF(WC.EQ.'NREFOL  ') OHFUN=.TRUE.
              IF(WC.EQ.'IREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'IPREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'YAREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'LNREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'MNREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'NNREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'GLX     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLY     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLZ     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLL     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLM     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLN     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLLOLD  ') OHFUN=.TRUE.
              IF(WC.EQ.'GLMOLD  ') OHFUN=.TRUE.
              IF(WC.EQ.'GLNOLD  ') OHFUN=.TRUE.
              IF(WC.EQ.'LENREF  ') OHFUN=.TRUE.
              IF(WC.EQ.'OPLREF  ') OHFUN=.TRUE.
              IF(WC.EQ.'RD      ') OHFUN=.TRUE.
              IF(WC.EQ.'CV      ') OHFUN=.TRUE.
              IF(WC.EQ.'TH      ') OHFUN=.TRUE.
              IF(WC.EQ.'THM     ') OHFUN=.TRUE.
              IF(WC.EQ.'PRICE   ') OHFUN=.TRUE.
              IF(WC.EQ.'CC      ') OHFUN=.TRUE.
              IF(WC.EQ.'AC      ') OHFUN=.TRUE.
              IF(WC.EQ.'AD      ') OHFUN=.TRUE.
              IF(WC.EQ.'AE      ') OHFUN=.TRUE.
              IF(WC.EQ.'AF      ') OHFUN=.TRUE.
              IF(WC.EQ.'AG      ') OHFUN=.TRUE.
              IF(WC.EQ.'AH      ') OHFUN=.TRUE.
              IF(WC.EQ.'AI      ') OHFUN=.TRUE.
              IF(WC.EQ.'AJ      ') OHFUN=.TRUE.
              IF(WC.EQ.'AK      ') OHFUN=.TRUE.
              IF(WC.EQ.'AL      ') OHFUN=.TRUE.
              IF(WC.EQ.'RDTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'CVTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'GRS     ') OHFUN=.TRUE.
              IF(WC.EQ.'CCTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'ADTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'AETOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'AFTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'AGTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'ALPHA   ') OHFUN=.TRUE.
              IF(WC.EQ.'BETA    ') OHFUN=.TRUE.
              IF(WC.EQ.'GAMMA   ') OHFUN=.TRUE.
              IF(WC.EQ.'GDX     ') OHFUN=.TRUE.
              IF(WC.EQ.'GDY     ') OHFUN=.TRUE.
              IF(WC.EQ.'GDZ     ') OHFUN=.TRUE.
              IF(WC.EQ.'GALPHA  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBETA   ') OHFUN=.TRUE.
              IF(WC.EQ.'GGAMMA  ') OHFUN=.TRUE.
              IF(WC.EQ.'VNUM    ') OHFUN=.TRUE.
              IF(WC.EQ.'DPART   ') OHFUN=.TRUE.
              IF(WC.EQ.'ABBE    ') OHFUN=.TRUE.
              IF(WC.EQ.'PARTL   ') OHFUN=.TRUE.
              IF(WC.EQ.'INDEX   ') OHFUN=.TRUE.
              IF(WC.EQ.'N1      ') OHFUN=.TRUE.
              IF(WC.EQ.'N2      ') OHFUN=.TRUE.
              IF(WC.EQ.'N3      ') OHFUN=.TRUE.
              IF(WC.EQ.'N4      ') OHFUN=.TRUE.
              IF(WC.EQ.'N5      ') OHFUN=.TRUE.
              IF(WC.EQ.'N6      ') OHFUN=.TRUE.
              IF(WC.EQ.'N7      ') OHFUN=.TRUE.
              IF(WC.EQ.'N8      ') OHFUN=.TRUE.
              IF(WC.EQ.'N9      ') OHFUN=.TRUE.
              IF(WC.EQ.'N10     ') OHFUN=.TRUE.
              IF(WC.EQ.'XD      ') OHFUN=.TRUE.
              IF(WC.EQ.'YD      ') OHFUN=.TRUE.
              IF(WC.EQ.'ZD      ') OHFUN=.TRUE.
              IF(WC.EQ.'PIVX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIVY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIVZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'XVERT   ') OHFUN=.TRUE.
              IF(WC.EQ.'YVERT   ') OHFUN=.TRUE.
              IF(WC.EQ.'ZVERT   ') OHFUN=.TRUE.
              IF(WC.EQ.'LXVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MXVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'NXVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'LYVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MYVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'NYVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'LZVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MZVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'NZVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'LENGTH  ') OHFUN=.TRUE.
              IF(WC.EQ.'OAL     ') OHFUN=.TRUE.
              IF(WC.EQ.'MLENGTH ') OHFUN=.TRUE.
              IF(WC.EQ.'OPTLEN  ') OHFUN=.TRUE.
              IF(WC.EQ.'WEIGHT  ') OHFUN=.TRUE.
              IF(WC.EQ.'ET      ') OHFUN=.TRUE.
              IF(WC.EQ.'ETY     ') OHFUN=.TRUE.
              IF(WC.EQ.'ETX     ') OHFUN=.TRUE.
              IF(WC.EQ.'SHAPEFAC') OHFUN=.TRUE.
              IF(WC.EQ.'C1      ') OHFUN=.TRUE.
              IF(WC.EQ.'C2      ') OHFUN=.TRUE.
              IF(WC.EQ.'C3      ') OHFUN=.TRUE.
              IF(WC.EQ.'C4      ') OHFUN=.TRUE.
              IF(WC.EQ.'C5      ') OHFUN=.TRUE.
              IF(WC.EQ.'C6      ') OHFUN=.TRUE.
              IF(WC.EQ.'C7      ') OHFUN=.TRUE.
              IF(WC.EQ.'C8      ') OHFUN=.TRUE.
              IF(WC.EQ.'C9      ') OHFUN=.TRUE.
              IF(WC.EQ.'C10     ') OHFUN=.TRUE.
              IF(WC.EQ.'C11     ') OHFUN=.TRUE.
              IF(WC.EQ.'C12     ') OHFUN=.TRUE.
              IF(WC.EQ.'C13     ') OHFUN=.TRUE.
              IF(WC.EQ.'C14     ') OHFUN=.TRUE.
              IF(WC.EQ.'C15     ') OHFUN=.TRUE.
              IF(WC.EQ.'C16     ') OHFUN=.TRUE.
              IF(WC.EQ.'C17     ') OHFUN=.TRUE.
              IF(WC.EQ.'C18     ') OHFUN=.TRUE.
              IF(WC.EQ.'C19     ') OHFUN=.TRUE.
              IF(WC.EQ.'C20     ') OHFUN=.TRUE.
              IF(WC.EQ.'C21     ') OHFUN=.TRUE.
              IF(WC.EQ.'C22     ') OHFUN=.TRUE.
              IF(WC.EQ.'C23     ') OHFUN=.TRUE.
              IF(WC.EQ.'C24     ') OHFUN=.TRUE.
              IF(WC.EQ.'C25     ') OHFUN=.TRUE.
              IF(WC.EQ.'C26     ') OHFUN=.TRUE.
              IF(WC.EQ.'C27     ') OHFUN=.TRUE.
              IF(WC.EQ.'C28     ') OHFUN=.TRUE.
              IF(WC.EQ.'C29     ') OHFUN=.TRUE.
              IF(WC.EQ.'C30     ') OHFUN=.TRUE.
              IF(WC.EQ.'C31     ') OHFUN=.TRUE.
              IF(WC.EQ.'C32     ') OHFUN=.TRUE.
              IF(WC.EQ.'C33     ') OHFUN=.TRUE.
              IF(WC.EQ.'C34     ') OHFUN=.TRUE.
              IF(WC.EQ.'C35     ') OHFUN=.TRUE.
              IF(WC.EQ.'C36     ') OHFUN=.TRUE.
              IF(WC.EQ.'C37     ') OHFUN=.TRUE.
              IF(WC.EQ.'C38     ') OHFUN=.TRUE.
              IF(WC.EQ.'C39     ') OHFUN=.TRUE.
              IF(WC.EQ.'C40     ') OHFUN=.TRUE.
              IF(WC.EQ.'C41     ') OHFUN=.TRUE.
              IF(WC.EQ.'C42     ') OHFUN=.TRUE.
              IF(WC.EQ.'C43     ') OHFUN=.TRUE.
              IF(WC.EQ.'C44     ') OHFUN=.TRUE.
              IF(WC.EQ.'C45     ') OHFUN=.TRUE.
              IF(WC.EQ.'C46     ') OHFUN=.TRUE.
              IF(WC.EQ.'C47     ') OHFUN=.TRUE.
              IF(WC.EQ.'C48     ') OHFUN=.TRUE.
              IF(WC.EQ.'C49     ') OHFUN=.TRUE.
              IF(WC.EQ.'C50     ') OHFUN=.TRUE.
              IF(WC.EQ.'C51     ') OHFUN=.TRUE.
              IF(WC.EQ.'C52     ') OHFUN=.TRUE.
              IF(WC.EQ.'C53     ') OHFUN=.TRUE.
              IF(WC.EQ.'C54     ') OHFUN=.TRUE.
              IF(WC.EQ.'C55     ') OHFUN=.TRUE.
              IF(WC.EQ.'C56     ') OHFUN=.TRUE.
              IF(WC.EQ.'C57     ') OHFUN=.TRUE.
              IF(WC.EQ.'C58     ') OHFUN=.TRUE.
              IF(WC.EQ.'C59     ') OHFUN=.TRUE.
              IF(WC.EQ.'C60     ') OHFUN=.TRUE.
              IF(WC.EQ.'C61     ') OHFUN=.TRUE.
              IF(WC.EQ.'C62     ') OHFUN=.TRUE.
              IF(WC.EQ.'C63     ') OHFUN=.TRUE.
              IF(WC.EQ.'C64     ') OHFUN=.TRUE.
              IF(WC.EQ.'C65     ') OHFUN=.TRUE.
              IF(WC.EQ.'C66     ') OHFUN=.TRUE.
              IF(WC.EQ.'C67     ') OHFUN=.TRUE.
              IF(WC.EQ.'C68     ') OHFUN=.TRUE.
              IF(WC.EQ.'C69     ') OHFUN=.TRUE.
              IF(WC.EQ.'C70     ') OHFUN=.TRUE.
              IF(WC.EQ.'C71     ') OHFUN=.TRUE.
              IF(WC.EQ.'C72     ') OHFUN=.TRUE.
              IF(WC.EQ.'C73     ') OHFUN=.TRUE.
              IF(WC.EQ.'C74     ') OHFUN=.TRUE.
              IF(WC.EQ.'C75     ') OHFUN=.TRUE.
              IF(WC.EQ.'C76     ') OHFUN=.TRUE.
              IF(WC.EQ.'C77     ') OHFUN=.TRUE.
              IF(WC.EQ.'C78     ') OHFUN=.TRUE.
              IF(WC.EQ.'C79     ') OHFUN=.TRUE.
              IF(WC.EQ.'C80     ') OHFUN=.TRUE.
              IF(WC.EQ.'C81     ') OHFUN=.TRUE.
              IF(WC.EQ.'C82     ') OHFUN=.TRUE.
              IF(WC.EQ.'C83     ') OHFUN=.TRUE.
              IF(WC.EQ.'C84     ') OHFUN=.TRUE.
              IF(WC.EQ.'C85     ') OHFUN=.TRUE.
              IF(WC.EQ.'C86     ') OHFUN=.TRUE.
              IF(WC.EQ.'C87     ') OHFUN=.TRUE.
              IF(WC.EQ.'C88     ') OHFUN=.TRUE.
              IF(WC.EQ.'C89     ') OHFUN=.TRUE.
              IF(WC.EQ.'C90     ') OHFUN=.TRUE.
              IF(WC.EQ.'C91     ') OHFUN=.TRUE.
              IF(WC.EQ.'C92     ') OHFUN=.TRUE.
              IF(WC.EQ.'C93     ') OHFUN=.TRUE.
              IF(WC.EQ.'C94     ') OHFUN=.TRUE.
              IF(WC.EQ.'C95     ') OHFUN=.TRUE.
              IF(WC.EQ.'C96     ') OHFUN=.TRUE.
              IF(WC.EQ.'PWRY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PWRX    ') OHFUN=.TRUE.
              IF(WC.EQ.'FLCLTHY ') OHFUN=.TRUE.
              IF(WC.EQ.'FLCLTHX ') OHFUN=.TRUE.
              IF(WC.EQ.'FLCLTH  ') OHFUN=.TRUE.
              IF(WC.EQ.'PY      ') OHFUN=.TRUE.
              IF(WC.EQ.'PX      ') OHFUN=.TRUE.
              IF(WC.EQ.'PCY     ') OHFUN=.TRUE.
              IF(WC.EQ.'PCX     ') OHFUN=.TRUE.
              IF(WC.EQ.'PUY     ') OHFUN=.TRUE.
              IF(WC.EQ.'PUX     ') OHFUN=.TRUE.
              IF(WC.EQ.'PUCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PUCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIY     ') OHFUN=.TRUE.
              IF(WC.EQ.'PIX     ') OHFUN=.TRUE.
              IF(WC.EQ.'PICY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PICX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIYP    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIXP    ') OHFUN=.TRUE.
              IF(WC.EQ.'PICYP   ') OHFUN=.TRUE.
              IF(WC.EQ.'PICXP   ') OHFUN=.TRUE.
              IF(WC.EQ.'PACY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PACX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACY    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'IMDISX  ') OHFUN=.TRUE.
              IF(WC.EQ.'IMDISY  ') OHFUN=.TRUE.
              IF(WC.EQ.'CENTX   ') OHFUN=.TRUE.
              IF(WC.EQ.'CENTY   ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSX    ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSY    ') OHFUN=.TRUE.
              IF(WC.EQ.'RMS     ') OHFUN=.TRUE.
              IF(WC.EQ.'RSSX    ') OHFUN=.TRUE.
              IF(WC.EQ.'RSSY    ') OHFUN=.TRUE.
              IF(WC.EQ.'RSS     ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSOPD  ') OHFUN=.TRUE.
              IF(WC.EQ.'ZERN37  ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGX    ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGY    ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGXOR  ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGYOR  ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFLX    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFLY    ') OHFUN=.TRUE.
              IF(WC.EQ.'FFNX    ') OHFUN=.TRUE.
              IF(WC.EQ.'FFNY    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFNX    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFNY    ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
              IF(WC.EQ.'ENDIAX  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENDIAY  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXDIAX  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXDIAY  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPOSX  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPOSY  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPOSZ  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPOSX  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPOSY  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPOSZ  ') OHFUN=.TRUE.
              IF(WC.EQ.'FNUMX   ') OHFUN=.TRUE.
              IF(WC.EQ.'FNUMY   ') OHFUN=.TRUE.
              IF(WC.EQ.'OBFNUMX ') OHFUN=.TRUE.
              IF(WC.EQ.'ONFNUMY ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPDIAX ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPDIAY ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPDIAX ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPDIAY ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDIAX ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDIAY ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDISX ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDISY ') OHFUN=.TRUE.
              IF(WC.EQ.'CHFIMX  ') OHFUN=.TRUE.
              IF(WC.EQ.'CHFIMY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GPX     ') OHFUN=.TRUE.
              IF(WC.EQ.'GPY     ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUX    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUY    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUCX   ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUCY   ') OHFUN=.TRUE.
              IF(WC.EQ.'DIST    ') OHFUN=.TRUE.
              IF(WC.EQ.'FISHDIST') OHFUN=.TRUE.
              IF(WC.EQ.'XFOC    ') OHFUN=.TRUE.
              IF(WC.EQ.'YFOC    ') OHFUN=.TRUE.
              IF(WC.EQ.'AST     ') OHFUN=.TRUE.
              IF(WC.EQ.'SA3     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA3    ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA3   ') OHFUN=.TRUE.
              IF(WC.EQ.'AST3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST3   ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS3   ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ3   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5    ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5   ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5   ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5   ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5   ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSA  ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSA  ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMA  ') OHFUN=.TRUE.
              IF(WC.EQ.'TAS     ') OHFUN=.TRUE.
              IF(WC.EQ.'XTAS    ') OHFUN=.TRUE.
              IF(WC.EQ.'SAS     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSAS    ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7    ') OHFUN=.TRUE.
              IF(WC.EQ.'SA3P    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5P    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSAP  ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSAP ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSAP  ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSAP ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMAP  ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMAP ') OHFUN=.TRUE.
              IF(WC.EQ.'TASP    ') OHFUN=.TRUE.
              IF(WC.EQ.'XTASP   ') OHFUN=.TRUE.
              IF(WC.EQ.'SASP    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSASP   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7P    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7P   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA3S    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5S    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSAS  ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSAS ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSAS  ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSAS ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMAS  ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMAS ') OHFUN=.TRUE.
              IF(WC.EQ.'TASS    ') OHFUN=.TRUE.
              IF(WC.EQ.'XTASS   ') OHFUN=.TRUE.
              IF(WC.EQ.'SASS    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSASS   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7S    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7S   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5I    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSAI  ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSAI ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSAI ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMAI  ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMAI ') OHFUN=.TRUE.
              IF(WC.EQ.'TASI    ') OHFUN=.TRUE.
              IF(WC.EQ.'XTASI   ') OHFUN=.TRUE.
              IF(WC.EQ.'SASI    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSASI   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7I    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7I   ') OHFUN=.TRUE.
              IF(WC.EQ.'PSA3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XPSA3   ') OHFUN=.TRUE.
              IF(WC.EQ.'PCMA3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPCMA3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PAST3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPAST3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PDIS3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPDIS3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PPTZ3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPPTZ3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PSA3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPSA3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'PCMA3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPCMA3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PAST3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPAST3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PDIS3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPDIS3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PPTZ3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPPTZ3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PSA3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPSA3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'PCMA3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPCMA3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PAST3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPAST3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PDIS3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPDIS3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PPTZ3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPPTZ3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZCV   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZCV  ') OHFUN=.TRUE.
              IF(WC.EQ.'MGOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'PGOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'MDOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'PDOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'GOTFM   ') OHFUN=.TRUE.
              IF(WC.EQ.'GOTFP   ') OHFUN=.TRUE.
              IF(WC.EQ.'DOTFM   ') OHFUN=.TRUE.
              IF(WC.EQ.'DOTFP   ') OHFUN=.TRUE.
              IF(WC.EQ.'RED     ') OHFUN=.TRUE.
              IF(WC.EQ.'REDCEN  ') OHFUN=.TRUE.
              IF(WC.EQ.'SYMX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SYMY    ') OHFUN=.TRUE.
              IF(WC.EQ.'ASYMX   ') OHFUN=.TRUE.
              IF(WC.EQ.'ASYMY   ') OHFUN=.TRUE.
              IF(WC.EQ.'CTSX    ') OHFUN=.TRUE.
              IF(WC.EQ.'CTSY    ') OHFUN=.TRUE.
              IF(WC.EQ.'SCEX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SCEY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PACM    ') OHFUN=.TRUE.
              IF(WC.EQ.'PACZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACM    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCM    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCM    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'GREYS   ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRADX  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRADY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBDISX  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBDISY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRCVX  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRCVY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBWAISTX') OHFUN=.TRUE.
              IF(WC.EQ.'GBWAISTY') OHFUN=.TRUE.
              IF(WC.EQ.'CLPY    ') OHFUN=.TRUE.
              IF(WC.EQ.'CLPX    ') OHFUN=.TRUE.
              IF(WC.EQ.'DMINUSD ') OHFUN=.TRUE.
              IF(WC.EQ.'COST    ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSYX   ') OHFUN=.TRUE.
              IF(WC.EQ.'CLEARX  ') OHFUN=.TRUE.
              IF(WC.EQ.'CLEARY  ') OHFUN=.TRUE.
              IF(.NOT.NOGO.AND..NOT.OHFUN) THEN
                  OUTLYNE='INVALID OPERAND ENTRY'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(.NOT.NOGO.AND.OHFUN) THEN
                  WQ=WC
                  SQ=1
                  WC='FUNC00'
C               NO REPLACEMENT DONE
                  NOGO=.FALSE.
                  OHFUN=.FALSE.
              END IF
C     NOT IN TOP OR UPDATE TOPER
          END IF
C
C     IF F54 IS 1 OR 2 WE ARE WORKING THE FOCRIT FUNCTION
C     IF NOT A SPECIFIC ALLOWED COMMAND, INSERT FUNC01 FOR
C     WC AND MAKE OLD WC THE NEW WQ
          IF(F54.EQ.1.OR.F54.EQ.2) THEN
              NOGO=.FALSE.
              OHFUN=.FALSE.
              IF(WC.EQ.'FUNC01  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC02  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC03  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC04  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC05  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC06  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC07  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC08  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC09  ')   NOGO=.TRUE.
              IF(WC.EQ.'FUNC10  ')   NOGO=.TRUE.
              IF(WC.EQ.'DEL     ')   NOGO=.TRUE.
              IF(WC.EQ.'COR     ')   NOGO=.TRUE.
              IF(WC.EQ.'BYP     ')   NOGO=.TRUE.
              IF(WC.EQ.'BLO     ')   NOGO=.TRUE.
              IF(WC.EQ.'BHI     ')   NOGO=.TRUE.
              IF(WC.EQ.'GTE     ')   NOGO=.TRUE.
              IF(WC.EQ.'LTE     ')   NOGO=.TRUE.
              IF(WC.EQ.'HLD     ')   NOGO=.TRUE.
              IF(WC.EQ.'GET     ')   NOGO=.TRUE.
              IF(WC.EQ.'AGET    ')   NOGO=.TRUE.
              IF(WC.EQ.'RCL     ')   NOGO=.TRUE.
              IF(WC.EQ.'STO     ')   NOGO=.TRUE.
              IF(WC.EQ.'WRITE   ')   NOGO=.TRUE.
              IF(WC.EQ.'MAXVAL  ')   NOGO=.TRUE.
              IF(WC.EQ.'MINVAL  ')   NOGO=.TRUE.
              IF(WC.EQ.'PI      ')   NOGO=.TRUE.
              IF(WC.EQ.'CHS     ')   NOGO=.TRUE.
              IF(WC.EQ.'MOVE    ')   NOGO=.TRUE.
              IF(WC.EQ.'LASTX   ')   NOGO=.TRUE.
              IF(WC.EQ.'LASTIX  ')   NOGO=.TRUE.
              IF(WC.EQ.'INCR    ')   NOGO=.TRUE.
              IF(WC.EQ.'PLUS    ')   NOGO=.TRUE.
              IF(WC.EQ.'MINUS   ')   NOGO=.TRUE.
              IF(WC.EQ.'MPY     ')   NOGO=.TRUE.
              IF(WC.EQ.'DIV     ')   NOGO=.TRUE.
              IF(WC.EQ.'SQRT    ')   NOGO=.TRUE.
              IF(WC.EQ.'SIN     ')   NOGO=.TRUE.
              IF(WC.EQ.'COS     ')   NOGO=.TRUE.
              IF(WC.EQ.'TAN     ')   NOGO=.TRUE.
              IF(WC.EQ.'SINH    ')   NOGO=.TRUE.
              IF(WC.EQ.'COSH    ')   NOGO=.TRUE.
              IF(WC.EQ.'TANH    ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTK   ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTKI  ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTKC  ')   NOGO=.TRUE.
              IF(WC.EQ.'CLREG   ')   NOGO=.TRUE.
              IF(WC.EQ.'CLGREG  ')   NOGO=.TRUE.
              IF(WC.EQ.'CLX     ')   NOGO=.TRUE.
              IF(WC.EQ.'CLIX    ')   NOGO=.TRUE.
              IF(WC.EQ.'ASIN    ')   NOGO=.TRUE.
              IF(WC.EQ.'ACOS    ')   NOGO=.TRUE.
              IF(WC.EQ.'ATAN    ')   NOGO=.TRUE.
              IF(WC.EQ.'ABS     ')   NOGO=.TRUE.
              IF(WC.EQ.'EXP     ')   NOGO=.TRUE.
              IF(WC.EQ.'LN      ')   NOGO=.TRUE.
              IF(WC.EQ.'LOG10   ')   NOGO=.TRUE.
              IF(WC.EQ.'FACT    ')   NOGO=.TRUE.
              IF(WC.EQ.'SGN     ')   NOGO=.TRUE.
              IF(WC.EQ.'RECIP   ')   NOGO=.TRUE.
              IF(WC.EQ.'INTGR   ')   NOGO=.TRUE.
              IF(WC.EQ.'FRAC    ')   NOGO=.TRUE.
              IF(WC.EQ.'POW     ')   NOGO=.TRUE.
              IF(WC.EQ.'STORE   ')   NOGO=.TRUE.
              IF(WC.EQ.'RAND    ')   NOGO=.TRUE.
              IF(WC.EQ.'MOD     ')   NOGO=.TRUE.
              IF(WC.EQ.'CLSTREG ')   NOGO=.TRUE.
              IF(WC.EQ.'STADD   ')   NOGO=.TRUE.
              IF(WC.EQ.'STSUB   ')   NOGO=.TRUE.
              IF(WC.EQ.'MEAN    ')   NOGO=.TRUE.
              IF(WC.EQ.'STDEV   ')   NOGO=.TRUE.
              IF(WC.EQ.'RTD     ')   NOGO=.TRUE.
              IF(WC.EQ.'DTR     ')   NOGO=.TRUE.
              IF(WC.EQ.'ATAN2   ')   NOGO=.TRUE.
              IF(WC.EQ.'J1      ')   NOGO=.TRUE.
              IF(WC.EQ.'PREAD   ')   NOGO=.TRUE.
              IF(WC.EQ.'ATON    ')   NOGO=.TRUE.
              IF(WC.EQ.'STOAX   ')   NOGO=.TRUE.
              IF(WC.EQ.'ARCL    ')   NOGO=.TRUE.
              IF(WC.EQ.'AWRITE  ')   NOGO=.TRUE.
              IF(WC.EQ.'ASTO    ')   NOGO=.TRUE.
              IF(WC.EQ.'CLASTO  ')   NOGO=.TRUE.
              IF(WC.EQ.'SHOW    ')   NOGO=.TRUE.
              IF(WC.EQ.'ENT     ')   NOGO=.TRUE.
              IF(WC.EQ.'ENTI    ')   NOGO=.TRUE.
              IF(WC.EQ.'ENTC    ')   NOGO=.TRUE.
              IF(WC.EQ.'PULL    ')   NOGO=.TRUE.
              IF(WC.EQ.'IPULL   ')   NOGO=.TRUE.
              IF(WC.EQ.'CPULL   ')   NOGO=.TRUE.
              IF(WC.EQ.'RUP     ')   NOGO=.TRUE.
              IF(WC.EQ.'IRUP    ')   NOGO=.TRUE.
              IF(WC.EQ.'CRUP    ')   NOGO=.TRUE.
              IF(WC.EQ.'RDN     ')   NOGO=.TRUE.
              IF(WC.EQ.'IRDN    ')   NOGO=.TRUE.
              IF(WC.EQ.'CRDN    ')   NOGO=.TRUE.
              IF(WC.EQ.'X-Y     ')   NOGO=.TRUE.
              IF(WC.EQ.'IX-IY   ')   NOGO=.TRUE.
              IF(WC.EQ.'+       ')   NOGO=.TRUE.
              IF(WC.EQ.'-       ')   NOGO=.TRUE.
              IF(WC.EQ.'*       ')   NOGO=.TRUE.
              IF(WC.EQ.'/       ')   NOGO=.TRUE.
              IF(WC.EQ.'I+      ')   NOGO=.TRUE.
              IF(WC.EQ.'I-      ')   NOGO=.TRUE.
              IF(WC.EQ.'I*      ')   NOGO=.TRUE.
              IF(WC.EQ.'I/      ')   NOGO=.TRUE.
              IF(WC.EQ.'C+      ')   NOGO=.TRUE.
              IF(WC.EQ.'C-      ')   NOGO=.TRUE.
              IF(WC.EQ.'C*      ')   NOGO=.TRUE.
              IF(WC.EQ.'C/      ')   NOGO=.TRUE.
              IF(WC.EQ.'Y**X    ')   NOGO=.TRUE.
              IF(WC.EQ.'IY**IX  ')   NOGO=.TRUE.
              IF(WC.EQ.'CY**CX  ')   NOGO=.TRUE.
              IF(WC.EQ.'P-R     ')   NOGO=.TRUE.
              IF(WC.EQ.'R-P     ')   NOGO=.TRUE.
              IF(WC.EQ.'CYL-R   ')   NOGO=.TRUE.
              IF(WC.EQ.'R-CYL   ')   NOGO=.TRUE.
              IF(WC.EQ.'SP-R    ')   NOGO=.TRUE.
              IF(WC.EQ.'R-SP    ')   NOGO=.TRUE.
              IF(WC.EQ.'RE-IM   ')   NOGO=.TRUE.
              IF(WC.EQ.'IM-RE   ')   NOGO=.TRUE.
              IF(WC.EQ.'H-HMS   ')   NOGO=.TRUE.
              IF(WC.EQ.'HMS-H   ')   NOGO=.TRUE.
              IF(WC.EQ.'IN-MM   ')   NOGO=.TRUE.
              IF(WC.EQ.'IN-CM   ')   NOGO=.TRUE.
              IF(WC.EQ.'IN-M    ')   NOGO=.TRUE.
              IF(WC.EQ.'MM-IN   ')   NOGO=.TRUE.
              IF(WC.EQ.'CM-IN   ')   NOGO=.TRUE.
              IF(WC.EQ.'M-IN    ')   NOGO=.TRUE.
              IF(WC.EQ.'SET     ')   NOGO=.TRUE.
              IF(WC.EQ.'W1      ')   NOGO=.TRUE.
              IF(WC.EQ.'W2      ')   NOGO=.TRUE.
              IF(WC.EQ.'W3      ')   NOGO=.TRUE.
              IF(WC.EQ.'W4      ')   NOGO=.TRUE.
              IF(WC.EQ.'W5      ')   NOGO=.TRUE.
              IF(WC.EQ.'MR      ')   NOGO=.TRUE.
              IF(WC.EQ.'MRA     ')   NOGO=.TRUE.
              IF(WC.EQ.'OP      ')   NOGO=.TRUE.
              IF(WC.EQ.'OPA     ')   NOGO=.TRUE.
              IF(WC.EQ.'CFG     ')   NOGO=.TRUE.
              IF(WC.EQ.'C       ')   NOGO=.TRUE.
              IF(WC.EQ.'M       ')   NOGO=.TRUE.
              IF(WC.EQ.'EOS     ')   NOGO=.TRUE.
              IF(WC.EQ.'?       ')   NOGO=.TRUE.
              IF(WC.EQ.'        ')   NOGO=.TRUE.
              IF(WC.EQ.'OP_DESC ')   NOGO=.TRUE.
C     PREDEFINED FOCRIT OPERAND NAMES SET OHFUN TO TRUE
              IF(WC.EQ.'X       ')   OHFUN=.TRUE.
              IF(WC.EQ.'Y       ')   OHFUN=.TRUE.
              IF(WC.EQ.'Z       ')   OHFUN=.TRUE.
              IF(WC.EQ.'DCL     ')   OHFUN=.TRUE.
              IF(WC.EQ.'DCM     ')   OHFUN=.TRUE.
              IF(WC.EQ.'DCN     ')   OHFUN=.TRUE.
              IF(WC.EQ.'K       ')   OHFUN=.TRUE.
              IF(WC.EQ.'L       ')   OHFUN=.TRUE.
              IF(WC.EQ.'M       ')   OHFUN=.TRUE.
              IF(WC.EQ.'DX      ') OHFUN=.TRUE.
              IF(WC.EQ.'DY      ') OHFUN=.TRUE.
              IF(WC.EQ.'DR      ') OHFUN=.TRUE.
              IF(WC.EQ.'DXA     ') OHFUN=.TRUE.
              IF(WC.EQ.'DYA     ') OHFUN=.TRUE.
              IF(WC.EQ.'DRA     ') OHFUN=.TRUE.
              IF(WC.EQ.'XANG    ') OHFUN=.TRUE.
              IF(WC.EQ.'YANG    ') OHFUN=.TRUE.
              IF(WC.EQ.'OPL     ') OHFUN=.TRUE.
              IF(WC.EQ.'OPD     ') OHFUN=.TRUE.
              IF(WC.EQ.'OPDW    ') OHFUN=.TRUE.
              IF(WC.EQ.'LOLD    ') OHFUN=.TRUE.
              IF(WC.EQ.'MOLD    ') OHFUN=.TRUE.
              IF(WC.EQ.'NOLD    ') OHFUN=.TRUE.
              IF(WC.EQ.'LEN     ') OHFUN=.TRUE.
              IF(WC.EQ.'AII     ') OHFUN=.TRUE.
              IF(WC.EQ.'AIP     ') OHFUN=.TRUE.
              IF(WC.EQ.'LN      ') OHFUN=.TRUE.
              IF(WC.EQ.'MN      ') OHFUN=.TRUE.
              IF(WC.EQ.'NN      ') OHFUN=.TRUE.
              IF(WC.EQ.'PXPX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PXPY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PYPX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PYPY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PXAPX   ') OHFUN=.TRUE.
              IF(WC.EQ.'PXAPY   ') OHFUN=.TRUE.
              IF(WC.EQ.'PYAPX   ') OHFUN=.TRUE.
              IF(WC.EQ.'PYAPY   ') OHFUN=.TRUE.
              IF(WC.EQ.'DXDX    ') OHFUN=.TRUE.
              IF(WC.EQ.'DXDY    ') OHFUN=.TRUE.
              IF(WC.EQ.'DYDX    ') OHFUN=.TRUE.
              IF(WC.EQ.'DYDY    ') OHFUN=.TRUE.
              IF(WC.EQ.'DXADX   ') OHFUN=.TRUE.
              IF(WC.EQ.'DXADY   ') OHFUN=.TRUE.
              IF(WC.EQ.'DYADX   ') OHFUN=.TRUE.
              IF(WC.EQ.'DYADY   ') OHFUN=.TRUE.
              IF(WC.EQ.'XREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'YREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'ZREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'LREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'MACOPT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'NREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'LREFOL  ') OHFUN=.TRUE.
              IF(WC.EQ.'MREFOL  ') OHFUN=.TRUE.
              IF(WC.EQ.'NREFOL  ') OHFUN=.TRUE.
              IF(WC.EQ.'IREF    ') OHFUN=.TRUE.
              IF(WC.EQ.'IPREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'YAREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'LNREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'MNREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'NNREF   ') OHFUN=.TRUE.
              IF(WC.EQ.'GLX     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLY     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLZ     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLL     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLM     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLN     ') OHFUN=.TRUE.
              IF(WC.EQ.'GLLOLD  ') OHFUN=.TRUE.
              IF(WC.EQ.'GLMOLD  ') OHFUN=.TRUE.
              IF(WC.EQ.'GLNOLD  ') OHFUN=.TRUE.
              IF(WC.EQ.'LENREF  ') OHFUN=.TRUE.
              IF(WC.EQ.'OPLREF  ') OHFUN=.TRUE.
              IF(WC.EQ.'RD      ') OHFUN=.TRUE.
              IF(WC.EQ.'CV      ') OHFUN=.TRUE.
              IF(WC.EQ.'TH      ') OHFUN=.TRUE.
              IF(WC.EQ.'THM     ') OHFUN=.TRUE.
              IF(WC.EQ.'PRICE   ') OHFUN=.TRUE.
              IF(WC.EQ.'CC      ') OHFUN=.TRUE.
              IF(WC.EQ.'AC      ') OHFUN=.TRUE.
              IF(WC.EQ.'AD      ') OHFUN=.TRUE.
              IF(WC.EQ.'AE      ') OHFUN=.TRUE.
              IF(WC.EQ.'AF      ') OHFUN=.TRUE.
              IF(WC.EQ.'AG      ') OHFUN=.TRUE.
              IF(WC.EQ.'AH      ') OHFUN=.TRUE.
              IF(WC.EQ.'AI      ') OHFUN=.TRUE.
              IF(WC.EQ.'AJ      ') OHFUN=.TRUE.
              IF(WC.EQ.'AK      ') OHFUN=.TRUE.
              IF(WC.EQ.'AL      ') OHFUN=.TRUE.
              IF(WC.EQ.'RDTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'CVTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'GRS     ') OHFUN=.TRUE.
              IF(WC.EQ.'CCTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'ADTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'AETOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'AFTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'AGTOR   ') OHFUN=.TRUE.
              IF(WC.EQ.'ALPHA   ') OHFUN=.TRUE.
              IF(WC.EQ.'BETA    ') OHFUN=.TRUE.
              IF(WC.EQ.'GAMMA   ') OHFUN=.TRUE.
              IF(WC.EQ.'GDX     ') OHFUN=.TRUE.
              IF(WC.EQ.'GDY     ') OHFUN=.TRUE.
              IF(WC.EQ.'GDZ     ') OHFUN=.TRUE.
              IF(WC.EQ.'GALPHA  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBETA   ') OHFUN=.TRUE.
              IF(WC.EQ.'GGAMMA  ') OHFUN=.TRUE.
              IF(WC.EQ.'VNUM    ') OHFUN=.TRUE.
              IF(WC.EQ.'DPART   ') OHFUN=.TRUE.
              IF(WC.EQ.'ABBE    ') OHFUN=.TRUE.
              IF(WC.EQ.'PARTL   ') OHFUN=.TRUE.
              IF(WC.EQ.'INDEX   ') OHFUN=.TRUE.
              IF(WC.EQ.'N1      ') OHFUN=.TRUE.
              IF(WC.EQ.'N2      ') OHFUN=.TRUE.
              IF(WC.EQ.'N3      ') OHFUN=.TRUE.
              IF(WC.EQ.'N4      ') OHFUN=.TRUE.
              IF(WC.EQ.'N5      ') OHFUN=.TRUE.
              IF(WC.EQ.'N6      ') OHFUN=.TRUE.
              IF(WC.EQ.'N7      ') OHFUN=.TRUE.
              IF(WC.EQ.'N8      ') OHFUN=.TRUE.
              IF(WC.EQ.'N9      ') OHFUN=.TRUE.
              IF(WC.EQ.'N10     ') OHFUN=.TRUE.
              IF(WC.EQ.'XD      ') OHFUN=.TRUE.
              IF(WC.EQ.'YD      ') OHFUN=.TRUE.
              IF(WC.EQ.'ZD      ') OHFUN=.TRUE.
              IF(WC.EQ.'PIVX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIVY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIVZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'XVERT   ') OHFUN=.TRUE.
              IF(WC.EQ.'YVERT   ') OHFUN=.TRUE.
              IF(WC.EQ.'ZVERT   ') OHFUN=.TRUE.
              IF(WC.EQ.'LXVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MXVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'NXVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'LYVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MYVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'NYVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'LZVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'MZVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'NZVERT  ') OHFUN=.TRUE.
              IF(WC.EQ.'LENGTH  ') OHFUN=.TRUE.
              IF(WC.EQ.'OAL     ') OHFUN=.TRUE.
              IF(WC.EQ.'MLENGTH ') OHFUN=.TRUE.
              IF(WC.EQ.'OPTLEN  ') OHFUN=.TRUE.
              IF(WC.EQ.'WEIGHT  ') OHFUN=.TRUE.
              IF(WC.EQ.'ET      ') OHFUN=.TRUE.
              IF(WC.EQ.'ETY     ') OHFUN=.TRUE.
              IF(WC.EQ.'ETX     ') OHFUN=.TRUE.
              IF(WC.EQ.'SHAPEFAC') OHFUN=.TRUE.
              IF(WC.EQ.'C1      ') OHFUN=.TRUE.
              IF(WC.EQ.'C2      ') OHFUN=.TRUE.
              IF(WC.EQ.'C3      ') OHFUN=.TRUE.
              IF(WC.EQ.'C4      ') OHFUN=.TRUE.
              IF(WC.EQ.'C5      ') OHFUN=.TRUE.
              IF(WC.EQ.'C6      ') OHFUN=.TRUE.
              IF(WC.EQ.'C7      ') OHFUN=.TRUE.
              IF(WC.EQ.'C8      ') OHFUN=.TRUE.
              IF(WC.EQ.'C9      ') OHFUN=.TRUE.
              IF(WC.EQ.'C10     ') OHFUN=.TRUE.
              IF(WC.EQ.'C11     ') OHFUN=.TRUE.
              IF(WC.EQ.'C12     ') OHFUN=.TRUE.
              IF(WC.EQ.'C13     ') OHFUN=.TRUE.
              IF(WC.EQ.'C14     ') OHFUN=.TRUE.
              IF(WC.EQ.'C15     ') OHFUN=.TRUE.
              IF(WC.EQ.'C16     ') OHFUN=.TRUE.
              IF(WC.EQ.'C17     ') OHFUN=.TRUE.
              IF(WC.EQ.'C18     ') OHFUN=.TRUE.
              IF(WC.EQ.'C19     ') OHFUN=.TRUE.
              IF(WC.EQ.'C20     ') OHFUN=.TRUE.
              IF(WC.EQ.'C21     ') OHFUN=.TRUE.
              IF(WC.EQ.'C22     ') OHFUN=.TRUE.
              IF(WC.EQ.'C23     ') OHFUN=.TRUE.
              IF(WC.EQ.'C24     ') OHFUN=.TRUE.
              IF(WC.EQ.'C25     ') OHFUN=.TRUE.
              IF(WC.EQ.'C26     ') OHFUN=.TRUE.
              IF(WC.EQ.'C27     ') OHFUN=.TRUE.
              IF(WC.EQ.'C28     ') OHFUN=.TRUE.
              IF(WC.EQ.'C29     ') OHFUN=.TRUE.
              IF(WC.EQ.'C30     ') OHFUN=.TRUE.
              IF(WC.EQ.'C31     ') OHFUN=.TRUE.
              IF(WC.EQ.'C32     ') OHFUN=.TRUE.
              IF(WC.EQ.'C33     ') OHFUN=.TRUE.
              IF(WC.EQ.'C34     ') OHFUN=.TRUE.
              IF(WC.EQ.'C35     ') OHFUN=.TRUE.
              IF(WC.EQ.'C36     ') OHFUN=.TRUE.
              IF(WC.EQ.'C37     ') OHFUN=.TRUE.
              IF(WC.EQ.'C38     ') OHFUN=.TRUE.
              IF(WC.EQ.'C39     ') OHFUN=.TRUE.
              IF(WC.EQ.'C40     ') OHFUN=.TRUE.
              IF(WC.EQ.'C41     ') OHFUN=.TRUE.
              IF(WC.EQ.'C42     ') OHFUN=.TRUE.
              IF(WC.EQ.'C43     ') OHFUN=.TRUE.
              IF(WC.EQ.'C44     ') OHFUN=.TRUE.
              IF(WC.EQ.'C45     ') OHFUN=.TRUE.
              IF(WC.EQ.'C46     ') OHFUN=.TRUE.
              IF(WC.EQ.'C47     ') OHFUN=.TRUE.
              IF(WC.EQ.'C48     ') OHFUN=.TRUE.
              IF(WC.EQ.'C49     ') OHFUN=.TRUE.
              IF(WC.EQ.'C50     ') OHFUN=.TRUE.
              IF(WC.EQ.'C51     ') OHFUN=.TRUE.
              IF(WC.EQ.'C52     ') OHFUN=.TRUE.
              IF(WC.EQ.'C53     ') OHFUN=.TRUE.
              IF(WC.EQ.'C54     ') OHFUN=.TRUE.
              IF(WC.EQ.'C55     ') OHFUN=.TRUE.
              IF(WC.EQ.'C56     ') OHFUN=.TRUE.
              IF(WC.EQ.'C57     ') OHFUN=.TRUE.
              IF(WC.EQ.'C58     ') OHFUN=.TRUE.
              IF(WC.EQ.'C59     ') OHFUN=.TRUE.
              IF(WC.EQ.'C60     ') OHFUN=.TRUE.
              IF(WC.EQ.'C61     ') OHFUN=.TRUE.
              IF(WC.EQ.'C62     ') OHFUN=.TRUE.
              IF(WC.EQ.'C63     ') OHFUN=.TRUE.
              IF(WC.EQ.'C64     ') OHFUN=.TRUE.
              IF(WC.EQ.'C65     ') OHFUN=.TRUE.
              IF(WC.EQ.'C66     ') OHFUN=.TRUE.
              IF(WC.EQ.'C67     ') OHFUN=.TRUE.
              IF(WC.EQ.'C68     ') OHFUN=.TRUE.
              IF(WC.EQ.'C69     ') OHFUN=.TRUE.
              IF(WC.EQ.'C70     ') OHFUN=.TRUE.
              IF(WC.EQ.'C71     ') OHFUN=.TRUE.
              IF(WC.EQ.'C72     ') OHFUN=.TRUE.
              IF(WC.EQ.'C73     ') OHFUN=.TRUE.
              IF(WC.EQ.'C74     ') OHFUN=.TRUE.
              IF(WC.EQ.'C75     ') OHFUN=.TRUE.
              IF(WC.EQ.'C76     ') OHFUN=.TRUE.
              IF(WC.EQ.'C77     ') OHFUN=.TRUE.
              IF(WC.EQ.'C78     ') OHFUN=.TRUE.
              IF(WC.EQ.'C79     ') OHFUN=.TRUE.
              IF(WC.EQ.'C80     ') OHFUN=.TRUE.
              IF(WC.EQ.'C81     ') OHFUN=.TRUE.
              IF(WC.EQ.'C82     ') OHFUN=.TRUE.
              IF(WC.EQ.'C83     ') OHFUN=.TRUE.
              IF(WC.EQ.'C84     ') OHFUN=.TRUE.
              IF(WC.EQ.'C85     ') OHFUN=.TRUE.
              IF(WC.EQ.'C86     ') OHFUN=.TRUE.
              IF(WC.EQ.'C87     ') OHFUN=.TRUE.
              IF(WC.EQ.'C88     ') OHFUN=.TRUE.
              IF(WC.EQ.'C89     ') OHFUN=.TRUE.
              IF(WC.EQ.'C90     ') OHFUN=.TRUE.
              IF(WC.EQ.'C91     ') OHFUN=.TRUE.
              IF(WC.EQ.'C92     ') OHFUN=.TRUE.
              IF(WC.EQ.'C93     ') OHFUN=.TRUE.
              IF(WC.EQ.'C94     ') OHFUN=.TRUE.
              IF(WC.EQ.'C95     ') OHFUN=.TRUE.
              IF(WC.EQ.'C96     ') OHFUN=.TRUE.
              IF(WC.EQ.'PWRY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PWRX    ') OHFUN=.TRUE.
              IF(WC.EQ.'FLCLTHY ') OHFUN=.TRUE.
              IF(WC.EQ.'FLCLTHX ') OHFUN=.TRUE.
              IF(WC.EQ.'FLCLTH  ') OHFUN=.TRUE.
              IF(WC.EQ.'PY      ') OHFUN=.TRUE.
              IF(WC.EQ.'PX      ') OHFUN=.TRUE.
              IF(WC.EQ.'PCY     ') OHFUN=.TRUE.
              IF(WC.EQ.'PCX     ') OHFUN=.TRUE.
              IF(WC.EQ.'PUY     ') OHFUN=.TRUE.
              IF(WC.EQ.'PUX     ') OHFUN=.TRUE.
              IF(WC.EQ.'PUCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PUCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIY     ') OHFUN=.TRUE.
              IF(WC.EQ.'PIX     ') OHFUN=.TRUE.
              IF(WC.EQ.'PICY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PICX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIYP    ') OHFUN=.TRUE.
              IF(WC.EQ.'PIXP    ') OHFUN=.TRUE.
              IF(WC.EQ.'PICYP   ') OHFUN=.TRUE.
              IF(WC.EQ.'PICXP   ') OHFUN=.TRUE.
              IF(WC.EQ.'PACY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PACX    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACY    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'IMDISX  ') OHFUN=.TRUE.
              IF(WC.EQ.'IMDISY  ') OHFUN=.TRUE.
              IF(WC.EQ.'CENTX   ') OHFUN=.TRUE.
              IF(WC.EQ.'CENTY   ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSX    ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSY    ') OHFUN=.TRUE.
              IF(WC.EQ.'RMS     ') OHFUN=.TRUE.
              IF(WC.EQ.'RSSX    ') OHFUN=.TRUE.
              IF(WC.EQ.'RSSY    ') OHFUN=.TRUE.
              IF(WC.EQ.'RSS     ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSOPD  ') OHFUN=.TRUE.
              IF(WC.EQ.'ZERN37  ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGX    ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGY    ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGXOR  ') OHFUN=.TRUE.
              IF(WC.EQ.'MAGYOR  ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFLX    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFLY    ') OHFUN=.TRUE.
              IF(WC.EQ.'FFNX    ') OHFUN=.TRUE.
              IF(WC.EQ.'FFNY    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFNX    ') OHFUN=.TRUE.
              IF(WC.EQ.'BFNY    ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
              IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
              IF(WC.EQ.'ENDIAX  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENDIAY  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXDIAX  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXDIAY  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPOSX  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPOSY  ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPOSZ  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPOSX  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPOSY  ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPOSZ  ') OHFUN=.TRUE.
              IF(WC.EQ.'FNUMX   ') OHFUN=.TRUE.
              IF(WC.EQ.'FNUMY   ') OHFUN=.TRUE.
              IF(WC.EQ.'OBFNUMX ') OHFUN=.TRUE.
              IF(WC.EQ.'ONFNUMY ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPDIAX ') OHFUN=.TRUE.
              IF(WC.EQ.'ENPDIAY ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPDIAX ') OHFUN=.TRUE.
              IF(WC.EQ.'EXPDIAY ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDIAX ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDIAY ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDISX ') OHFUN=.TRUE.
              IF(WC.EQ.'PUPDISY ') OHFUN=.TRUE.
              IF(WC.EQ.'CHFIMX  ') OHFUN=.TRUE.
              IF(WC.EQ.'CHFIMY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GPX     ') OHFUN=.TRUE.
              IF(WC.EQ.'GPY     ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUX    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUY    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPCX    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPCY    ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUCX   ') OHFUN=.TRUE.
              IF(WC.EQ.'GPUCY   ') OHFUN=.TRUE.
              IF(WC.EQ.'DIST    ') OHFUN=.TRUE.
              IF(WC.EQ.'FISHDIST') OHFUN=.TRUE.
              IF(WC.EQ.'XFOC    ') OHFUN=.TRUE.
              IF(WC.EQ.'YFOC    ') OHFUN=.TRUE.
              IF(WC.EQ.'AST     ') OHFUN=.TRUE.
              IF(WC.EQ.'SA3     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA3    ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA3   ') OHFUN=.TRUE.
              IF(WC.EQ.'AST3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST3   ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS3   ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ3   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5    ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5   ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5   ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5   ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5    ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5   ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSA  ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSA  ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMA  ') OHFUN=.TRUE.
              IF(WC.EQ.'TAS     ') OHFUN=.TRUE.
              IF(WC.EQ.'XTAS    ') OHFUN=.TRUE.
              IF(WC.EQ.'SAS     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSAS    ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7     ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7    ') OHFUN=.TRUE.
              IF(WC.EQ.'SA3P    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5P    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5P  ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSAP  ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSAP ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSAP  ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSAP ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMAP  ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMAP ') OHFUN=.TRUE.
              IF(WC.EQ.'TASP    ') OHFUN=.TRUE.
              IF(WC.EQ.'XTASP   ') OHFUN=.TRUE.
              IF(WC.EQ.'SASP    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSASP   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7P    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7P   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA3S    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5S    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5S  ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSAS  ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSAS ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSAS  ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSAS ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMAS  ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMAS ') OHFUN=.TRUE.
              IF(WC.EQ.'TASS    ') OHFUN=.TRUE.
              IF(WC.EQ.'XTASS   ') OHFUN=.TRUE.
              IF(WC.EQ.'SASS    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSASS   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7S    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7S   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA5I    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'CMA5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XCMA5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'AST5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XAST5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'DIS5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XDIS5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZ5I   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZ5I  ') OHFUN=.TRUE.
              IF(WC.EQ.'TOBSAI  ') OHFUN=.TRUE.
              IF(WC.EQ.'XTOBSAI ') OHFUN=.TRUE.
              IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
              IF(WC.EQ.'XSOBSAI ') OHFUN=.TRUE.
              IF(WC.EQ.'ELCMAI  ') OHFUN=.TRUE.
              IF(WC.EQ.'XELCMAI ') OHFUN=.TRUE.
              IF(WC.EQ.'TASI    ') OHFUN=.TRUE.
              IF(WC.EQ.'XTASI   ') OHFUN=.TRUE.
              IF(WC.EQ.'SASI    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSASI   ') OHFUN=.TRUE.
              IF(WC.EQ.'SA7I    ') OHFUN=.TRUE.
              IF(WC.EQ.'XSA7I   ') OHFUN=.TRUE.
              IF(WC.EQ.'PSA3    ') OHFUN=.TRUE.
              IF(WC.EQ.'XPSA3   ') OHFUN=.TRUE.
              IF(WC.EQ.'PCMA3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPCMA3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PAST3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPAST3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PDIS3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPDIS3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PPTZ3   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPPTZ3  ') OHFUN=.TRUE.
              IF(WC.EQ.'PSA3P   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPSA3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'PCMA3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPCMA3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PAST3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPAST3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PDIS3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPDIS3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PPTZ3P  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPPTZ3P ') OHFUN=.TRUE.
              IF(WC.EQ.'PSA3S   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPSA3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'PCMA3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPCMA3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PAST3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPAST3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PDIS3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPDIS3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PPTZ3S  ') OHFUN=.TRUE.
              IF(WC.EQ.'XPPTZ3S ') OHFUN=.TRUE.
              IF(WC.EQ.'PTZCV   ') OHFUN=.TRUE.
              IF(WC.EQ.'XPTZCV  ') OHFUN=.TRUE.
              IF(WC.EQ.'MGOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'PGOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'MDOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'PDOTF   ') OHFUN=.TRUE.
              IF(WC.EQ.'GOTFM   ') OHFUN=.TRUE.
              IF(WC.EQ.'GOTFP   ') OHFUN=.TRUE.
              IF(WC.EQ.'DOTFM   ') OHFUN=.TRUE.
              IF(WC.EQ.'DOTFP   ') OHFUN=.TRUE.
              IF(WC.EQ.'RED     ') OHFUN=.TRUE.
              IF(WC.EQ.'REDCEN  ') OHFUN=.TRUE.
              IF(WC.EQ.'SYMX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SYMY    ') OHFUN=.TRUE.
              IF(WC.EQ.'ASYMX   ') OHFUN=.TRUE.
              IF(WC.EQ.'ASYMY   ') OHFUN=.TRUE.
              IF(WC.EQ.'CTSX    ') OHFUN=.TRUE.
              IF(WC.EQ.'CTSY    ') OHFUN=.TRUE.
              IF(WC.EQ.'SCEX    ') OHFUN=.TRUE.
              IF(WC.EQ.'SCEY    ') OHFUN=.TRUE.
              IF(WC.EQ.'PACM    ') OHFUN=.TRUE.
              IF(WC.EQ.'PACZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACM    ') OHFUN=.TRUE.
              IF(WC.EQ.'SACZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCM    ') OHFUN=.TRUE.
              IF(WC.EQ.'PLCZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCM    ') OHFUN=.TRUE.
              IF(WC.EQ.'SLCZ    ') OHFUN=.TRUE.
              IF(WC.EQ.'GREYS   ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRADX  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRADY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBDISX  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBDISY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRCVX  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBRCVY  ') OHFUN=.TRUE.
              IF(WC.EQ.'GBWAISTX') OHFUN=.TRUE.
              IF(WC.EQ.'GBWAISTY') OHFUN=.TRUE.
              IF(WC.EQ.'CLPY    ') OHFUN=.TRUE.
              IF(WC.EQ.'CLPX    ') OHFUN=.TRUE.
              IF(WC.EQ.'DMINUSD ') OHFUN=.TRUE.
              IF(WC.EQ.'COST    ') OHFUN=.TRUE.
              IF(WC.EQ.'RMSYX   ') OHFUN=.TRUE.
              IF(WC.EQ.'CLEARX  ') OHFUN=.TRUE.
              IF(WC.EQ.'CLEARY  ') OHFUN=.TRUE.
              IF(.NOT.NOGO.AND..NOT.OHFUN) THEN
                  OUTLYNE='INVALID OPERAND ENTRY'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(.NOT.NOGO.AND.OHFUN) THEN
                  WQ=WC
                  SQ=1
                  WC='FUNC00'
                  NOGO=.FALSE.
                  OHFUN=.FALSE.
C               NO REPLACEMENT DONE
              END IF
C     NOT IN FOCRIT OR UPDATE FOCRIT
          END IF
C
          DO 3000 NNN=1,16000
              NNNN=NNN
              IF(WC.NE.WCC(NNN)) GO TO 3000
              IF(WC.EQ.WCC(NNN).AND..NOT.FNYES) GO TO 3001
 3000     CONTINUE
          IF(NNNN.EQ.16000) THEN
C       NO MATCH WAS EVER FOUND (ONLY IF IN = 5)
C       HERE IS WHERE WE RUN OFF AND SEE IF THE COMMAND IS
C       REALLY A MACRO NAME WHICH IS TO BE EXECUTED.
C       IF IT WAS A MACRO NAME THE RETURN WILL PASS THE
C       VALUE OF 1 FOR VARRIABLE MACYES.
C       IF IT WAS NOT A MACRO NAME, THE RETURN WILL PASS
C       A VALUE OF MACYES=0.
C
              IF(F2.EQ.1.OR.F3.EQ.1) GO TO 3001
C       (JUST PROCEED.
C       ALLOW THE INSTRUCTION TO BE PUT INTO THE MACRO
C       EVEN WITHOUT A MATCH.)
C
C
              IF(IN.NE.5.AND.IN.NE.8.AND.IN.NE.9.AND.IN.NE.97.AND.IN.NE.96)
     1        GO TO 3001
              IF(F1.EQ.1) THEN
C     CMD LEVEL
                  IF(F4.NE.1) THEN
C       NOT COMMING FROM WITHIN A MACRO
C       SET INITIAL NESTING TRACKING PARAMETERS.
                      NEST=0
                      DO 10 I=0,20
                          NESTI(I)=0
                          NESTIJ(I)=0
 10                   CONTINUE
                  END IF
C
C       NOW SET THE SPECIAL QUALIFIER AND NUMERIC WORDS WHICH
C       ARE USED WITH THE CALL TO THE MACRO. NAMES ARE
C       MWQ,MWS,MNW1,MNW2,MNW3,MNW4,MNW5
C
                  MWQ(0)=WQ
                  MWS(0)=WS
                  MSQ(0)=SQ
                  MST(0)=SST
                  MNW(0,1)=W1
                  MNW(0,2)=W2
                  MNW(0,3)=W3
                  MNW(0,4)=W4
                  MNW(0,5)=W5
                  MDF(0,1)=DF1
                  MDF(0,2)=DF2
                  MDF(0,3)=DF3
                  MDF(0,4)=DF4
                  MDF(0,5)=DF5
C
                  DO I=0,10
                      NESFUN(I)=.FALSE.
                  END DO
                  DO I=0,20
                      TF(I)=0
                  END DO
C
C     CODE ADDED 5/17/2006 TO MAKE THE PERMANENT MACRO DIRECTORY
C     AUTO SEARCHED AFTER THER CURRENT DIRECTORY
C
C     SAVE THE "CURRENT" MACRO DIRECTORY NAME
                  OLIBMAC=LIBMAC
                  CALL MACRUN(MACYES)
C     IF MACYES=0, TRY THE PERMANENT MACRO DIRECTORY
                  IF(MACYES.EQ.0) THEN
                      LIBMAC=trim(HOME)//'PERMAC'
                      CALL MACRUN(MACYES)
                  END IF
C     SET THE MACRO DIRECTOR BACK TO THE ORIGINAL "CURRENT" VALUE
                  LIBMAC=OLIBMAC
C
C       OTHER VARIABLES ARE PASSED TO MACRUN VIA NAMED
C       COMMONS.
C
                  IF(MACYES.EQ.1) THEN
C
C       THE MACRO HAS BEEN EXECUTED FROM THE MACRO PROCESSOR
C       LEVEL SO JUST RETURN AND LOOK FOR THE NEXT INPUT COMMAND.
C       SET MACYES BACK TO ZERO.
C
                      MACYES=0
C
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
C
C       MACYES=0 THE COMMAND WAS INVALID. CHECK PROGRAM
C       LEVEL AND PRINT APPROPRIATE MESSAGE, THEN RETURN
C
                  END IF
C               F1 IS NOT 1, NO NEED TO CHECK FOR RUNNING A
C               MACRO
              END IF
              IF(F1.EQ.1.AND.F4.EQ.0.AND.F17.EQ.0) THEN
                  OUTLYNE='INVALID CMD LEVEL COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(F1.EQ.1.AND.F4.EQ.1.AND.F17.EQ.0) THEN
                  OUTLYNE='INVALID CMD LEVEL COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(F1.EQ.0.AND.F5.EQ.1) THEN
                  OUTLYNE='INVALID LENS INPUT COMMAND1'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(F1.EQ.0.AND.F6.EQ.1) THEN
                  OUTLYNE='INVALID LENS UPDATE COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(F1.EQ.0.AND.F7.EQ.1) THEN
                  OUTLYNE='INVALID SPSRF INPUT COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(F1.EQ.0.AND.F8.EQ.1) THEN
                  OUTLYNE='INVALID UPDATE SPSRF COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F9.EQ.1) THEN
                  OUTLYNE='INVALID SPFIT COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F10.EQ.1) THEN
                  OUTLYNE='INVALID CONFIGS INPUT COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F11.EQ.1) THEN
                  OUTLYNE='INVALID CONFIGS UPDATE COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.1.AND.F17.EQ.1) THEN
                  OUTLYNE='INVALID SPECT COMMAND'
                  CALL SHOWIT(1)
              END IF
              FNYES1=.FALSE.
              IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
              IF(WC.EQ.'COR     ') FNYES1=.TRUE.
              IF(WC.EQ.'BYP     ') FNYES1=.TRUE.
              IF(WC.EQ.'BLO     ') FNYES1=.TRUE.
              IF(WC.EQ.'BHI     ') FNYES1=.TRUE.
              IF(WC.EQ.'GTE     ') FNYES1=.TRUE.
              IF(WC.EQ.'LTE     ') FNYES1=.TRUE.
              IF(WC.EQ.'HLD     ') FNYES1=.TRUE.
              IF(WC.EQ.'GET     ') FNYES1=.TRUE.
              IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
              IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
              IF(WC.EQ.'STO     ') FNYES1=.TRUE.
              IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
              IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'PI      ') FNYES1=.TRUE.
              IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
              IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
              IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
              IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
              IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
              IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
              IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
              IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
              IF(WC.EQ.'COS     ') FNYES1=.TRUE.
              IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
              IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
              IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
              IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
              IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
              IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
              IF(WC.EQ.'LN      ') FNYES1=.TRUE.
              IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
              IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
              IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
              IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
              IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
              IF(WC.EQ.'POW     ') FNYES1=.TRUE.
              IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
              IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
              IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
              IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
              IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
              IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
              IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
              IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
              IF(WC.EQ.'J1      ') FNYES1=.TRUE.
              IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
              IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
              IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
              IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
              IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
              IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
              IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
              IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
              IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
              IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
              IF(WC.EQ.'+       ') FNYES1=.TRUE.
              IF(WC.EQ.'-       ') FNYES1=.TRUE.
              IF(WC.EQ.'*       ') FNYES1=.TRUE.
              IF(WC.EQ.'/       ') FNYES1=.TRUE.
              IF(WC.EQ.'I+      ') FNYES1=.TRUE.
              IF(WC.EQ.'I-      ') FNYES1=.TRUE.
              IF(WC.EQ.'I*      ') FNYES1=.TRUE.
              IF(WC.EQ.'I/      ') FNYES1=.TRUE.
              IF(WC.EQ.'C+      ') FNYES1=.TRUE.
              IF(WC.EQ.'C-      ') FNYES1=.TRUE.
              IF(WC.EQ.'C*      ') FNYES1=.TRUE.
              IF(WC.EQ.'C/      ') FNYES1=.TRUE.
              IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
              IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
              IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
              IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
              IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
              IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
              IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
              IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
              IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
              IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
              IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
              IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
              IF(WC.EQ.'SET     ') FNYES1=.TRUE.
              IF(WC.EQ.'W1      ') FNYES1=.TRUE.
              IF(WC.EQ.'W2      ') FNYES1=.TRUE.
              IF(WC.EQ.'W3      ') FNYES1=.TRUE.
              IF(WC.EQ.'W4      ') FNYES1=.TRUE.
              IF(WC.EQ.'W5      ') FNYES1=.TRUE.
              IF(WC.EQ.'MR      ') FNYES1=.TRUE.
              IF(WC.EQ.'MRA     ') FNYES1=.TRUE.
              IF(WC.EQ.'OP      ') FNYES1=.TRUE.
              IF(WC.EQ.'OPA     ') FNYES1=.TRUE.
              IF(WC.EQ.'CFG     ') FNYES1=.TRUE.
              IF(WC.EQ.'C       ') FNYES1=.TRUE.
              IF(WC.EQ.'M       ') FNYES1=.TRUE.
              IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
              IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
              IF(F1.EQ.0.AND.F27.EQ.1.AND..NOT.FNYES1) THEN
                  OUTLYNE='INVALID (MERIT INPUT) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F27.EQ.1.AND.FNYES1) GO TO 3001
              FNYES1=.FALSE.
              IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
              IF(WC.EQ.'GET     ') FNYES1=.TRUE.
              IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
              IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
              IF(WC.EQ.'STO     ') FNYES1=.TRUE.
              IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
              IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'PI      ') FNYES1=.TRUE.
              IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
              IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
              IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
              IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
              IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
              IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
              IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
              IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
              IF(WC.EQ.'COS     ') FNYES1=.TRUE.
              IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
              IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
              IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
              IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
              IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
              IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
              IF(WC.EQ.'LN      ') FNYES1=.TRUE.
              IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
              IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
              IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
              IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
              IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
              IF(WC.EQ.'POW     ') FNYES1=.TRUE.
              IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
              IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
              IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
              IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
              IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
              IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
              IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
              IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
              IF(WC.EQ.'J1      ') FNYES1=.TRUE.
              IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
              IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
              IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
              IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
              IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
              IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
              IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
              IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
              IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
              IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
              IF(WC.EQ.'+       ') FNYES1=.TRUE.
              IF(WC.EQ.'-       ') FNYES1=.TRUE.
              IF(WC.EQ.'*       ') FNYES1=.TRUE.
              IF(WC.EQ.'/       ') FNYES1=.TRUE.
              IF(WC.EQ.'I+      ') FNYES1=.TRUE.
              IF(WC.EQ.'I-      ') FNYES1=.TRUE.
              IF(WC.EQ.'I*      ') FNYES1=.TRUE.
              IF(WC.EQ.'I/      ') FNYES1=.TRUE.
              IF(WC.EQ.'C+      ') FNYES1=.TRUE.
              IF(WC.EQ.'C-      ') FNYES1=.TRUE.
              IF(WC.EQ.'C*      ') FNYES1=.TRUE.
              IF(WC.EQ.'C/      ') FNYES1=.TRUE.
              IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
              IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
              IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
              IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
              IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
              IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
              IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
              IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
              IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
              IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
              IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
              IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
              IF(WC.EQ.'SET     ') FNYES1=.TRUE.
              IF(WC.EQ.'W1      ') FNYES1=.TRUE.
              IF(WC.EQ.'W2      ') FNYES1=.TRUE.
              IF(WC.EQ.'W3      ') FNYES1=.TRUE.
              IF(WC.EQ.'W4      ') FNYES1=.TRUE.
              IF(WC.EQ.'W5      ') FNYES1=.TRUE.
              IF(WC.EQ.'MR      ') FNYES1=.TRUE.
              IF(WC.EQ.'MRA     ') FNYES1=.TRUE.
              IF(WC.EQ.'OP      ') FNYES1=.TRUE.
              IF(WC.EQ.'OPA     ') FNYES1=.TRUE.
              IF(WC.EQ.'CFG     ') FNYES1=.TRUE.
              IF(WC.EQ.'C       ') FNYES1=.TRUE.
              IF(WC.EQ.'M       ') FNYES1=.TRUE.
              IF(WC.EQ.'DEL     ') FNYES1=.TRUE.
              IF(WC.EQ.'COR     ') FNYES1=.TRUE.
              IF(WC.EQ.'BYP     ') FNYES1=.TRUE.
              IF(WC.EQ.'BLO     ') FNYES1=.TRUE.
              IF(WC.EQ.'BHI     ') FNYES1=.TRUE.
              IF(WC.EQ.'GTE     ') FNYES1=.TRUE.
              IF(WC.EQ.'LTE     ') FNYES1=.TRUE.
              IF(WC.EQ.'HLD     ') FNYES1=.TRUE.
              IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
              IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
              IF(F1.EQ.0.AND.F27.EQ.2.AND..NOT.FNYES1) THEN
                  OUTLYNE='INVALID (MERIT UPDATE) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F27.EQ.2.AND.FNYES1) GO TO 3001
              FNYES1=.FALSE.
              FNYES1=.FALSE.
              IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
              IF(WC.EQ.'GET     ') FNYES1=.TRUE.
              IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
              IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
              IF(WC.EQ.'STO     ') FNYES1=.TRUE.
              IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
              IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'PI      ') FNYES1=.TRUE.
              IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
              IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
              IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
              IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
              IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
              IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
              IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
              IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
              IF(WC.EQ.'COS     ') FNYES1=.TRUE.
              IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
              IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
              IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
              IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
              IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
              IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
              IF(WC.EQ.'LN      ') FNYES1=.TRUE.
              IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
              IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
              IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
              IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
              IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
              IF(WC.EQ.'POW     ') FNYES1=.TRUE.
              IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
              IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
              IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
              IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
              IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
              IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
              IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
              IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
              IF(WC.EQ.'J1      ') FNYES1=.TRUE.
              IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
              IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
              IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
              IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
              IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
              IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
              IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
              IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
              IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
              IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
              IF(WC.EQ.'+       ') FNYES1=.TRUE.
              IF(WC.EQ.'-       ') FNYES1=.TRUE.
              IF(WC.EQ.'*       ') FNYES1=.TRUE.
              IF(WC.EQ.'/       ') FNYES1=.TRUE.
              IF(WC.EQ.'I+      ') FNYES1=.TRUE.
              IF(WC.EQ.'I-      ') FNYES1=.TRUE.
              IF(WC.EQ.'I*      ') FNYES1=.TRUE.
              IF(WC.EQ.'I/      ') FNYES1=.TRUE.
              IF(WC.EQ.'C+      ') FNYES1=.TRUE.
              IF(WC.EQ.'C-      ') FNYES1=.TRUE.
              IF(WC.EQ.'C*      ') FNYES1=.TRUE.
              IF(WC.EQ.'C/      ') FNYES1=.TRUE.
              IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
              IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
              IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
              IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
              IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
              IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
              IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
              IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
              IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
              IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
              IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
              IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
              IF(WC.EQ.'SET     ') FNYES1=.TRUE.
              IF(WC.EQ.'W1      ') FNYES1=.TRUE.
              IF(WC.EQ.'W2      ') FNYES1=.TRUE.
              IF(WC.EQ.'W3      ') FNYES1=.TRUE.
              IF(WC.EQ.'W4      ') FNYES1=.TRUE.
              IF(WC.EQ.'W5      ') FNYES1=.TRUE.
              IF(WC.EQ.'TOPS    ') FNYES1=.TRUE.
              IF(WC.EQ.'C       ') FNYES1=.TRUE.
              IF(WC.EQ.'M       ') FNYES1=.TRUE.
              IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
              IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
              IF(F1.EQ.0.AND.F53.EQ.1.AND..NOT.FNYES1) THEN
                  OUTLYNE='INVALID (TOPER INPUT) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F53.EQ.1.AND.FNYES1) GO TO 3001
              FNYES1=.FALSE.
              IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
              IF(WC.EQ.'GET     ') FNYES1=.TRUE.
              IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
              IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
              IF(WC.EQ.'STO     ') FNYES1=.TRUE.
              IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
              IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'PI      ') FNYES1=.TRUE.
              IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
              IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
              IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
              IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
              IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
              IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
              IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
              IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
              IF(WC.EQ.'COS     ') FNYES1=.TRUE.
              IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
              IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
              IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
              IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
              IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
              IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
              IF(WC.EQ.'LN      ') FNYES1=.TRUE.
              IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
              IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
              IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
              IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
              IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
              IF(WC.EQ.'POW     ') FNYES1=.TRUE.
              IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
              IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
              IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
              IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
              IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
              IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
              IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
              IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
              IF(WC.EQ.'J1      ') FNYES1=.TRUE.
              IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
              IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
              IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
              IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
              IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
              IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
              IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
              IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
              IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
              IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
              IF(WC.EQ.'+       ') FNYES1=.TRUE.
              IF(WC.EQ.'-       ') FNYES1=.TRUE.
              IF(WC.EQ.'*       ') FNYES1=.TRUE.
              IF(WC.EQ.'/       ') FNYES1=.TRUE.
              IF(WC.EQ.'I+      ') FNYES1=.TRUE.
              IF(WC.EQ.'I-      ') FNYES1=.TRUE.
              IF(WC.EQ.'I*      ') FNYES1=.TRUE.
              IF(WC.EQ.'I/      ') FNYES1=.TRUE.
              IF(WC.EQ.'C+      ') FNYES1=.TRUE.
              IF(WC.EQ.'C-      ') FNYES1=.TRUE.
              IF(WC.EQ.'C*      ') FNYES1=.TRUE.
              IF(WC.EQ.'C/      ') FNYES1=.TRUE.
              IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
              IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
              IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
              IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
              IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
              IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
              IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
              IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
              IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
              IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
              IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
              IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
              IF(WC.EQ.'SET     ') FNYES1=.TRUE.
              IF(WC.EQ.'W1      ') FNYES1=.TRUE.
              IF(WC.EQ.'W2      ') FNYES1=.TRUE.
              IF(WC.EQ.'W3      ') FNYES1=.TRUE.
              IF(WC.EQ.'W4      ') FNYES1=.TRUE.
              IF(WC.EQ.'W5      ') FNYES1=.TRUE.
              IF(WC.EQ.'TOPS    ') FNYES1=.TRUE.
              IF(WC.EQ.'C       ') FNYES1=.TRUE.
              IF(WC.EQ.'M       ') FNYES1=.TRUE.
              IF(WC.EQ.'DEL     ') FNYES1=.TRUE.
              IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
              IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
              IF(F1.EQ.0.AND.F53.EQ.2.AND..NOT.FNYES1) THEN
                  OUTLYNE='INVALID (TOPER UPDATE) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F53.EQ.2.AND.FNYES1) GO TO 3001
              FNYES1=.FALSE.
              IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
              IF(WC.EQ.'COR     ') FNYES1=.TRUE.
              IF(WC.EQ.'BYP     ') FNYES1=.TRUE.
              IF(WC.EQ.'BLO     ') FNYES1=.TRUE.
              IF(WC.EQ.'BHI     ') FNYES1=.TRUE.
              IF(WC.EQ.'GTE     ') FNYES1=.TRUE.
              IF(WC.EQ.'LTE     ') FNYES1=.TRUE.
              IF(WC.EQ.'HLD     ') FNYES1=.TRUE.
              IF(WC.EQ.'GET     ') FNYES1=.TRUE.
              IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
              IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
              IF(WC.EQ.'STO     ') FNYES1=.TRUE.
              IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
              IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'PI      ') FNYES1=.TRUE.
              IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
              IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
              IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
              IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
              IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
              IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
              IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
              IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
              IF(WC.EQ.'COS     ') FNYES1=.TRUE.
              IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
              IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
              IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
              IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
              IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
              IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
              IF(WC.EQ.'LN      ') FNYES1=.TRUE.
              IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
              IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
              IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
              IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
              IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
              IF(WC.EQ.'POW     ') FNYES1=.TRUE.
              IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
              IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
              IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
              IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
              IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
              IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
              IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
              IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
              IF(WC.EQ.'J1      ') FNYES1=.TRUE.
              IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
              IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
              IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
              IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
              IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
              IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
              IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
              IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
              IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
              IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
              IF(WC.EQ.'+       ') FNYES1=.TRUE.
              IF(WC.EQ.'-       ') FNYES1=.TRUE.
              IF(WC.EQ.'*       ') FNYES1=.TRUE.
              IF(WC.EQ.'/       ') FNYES1=.TRUE.
              IF(WC.EQ.'I+      ') FNYES1=.TRUE.
              IF(WC.EQ.'I-      ') FNYES1=.TRUE.
              IF(WC.EQ.'I*      ') FNYES1=.TRUE.
              IF(WC.EQ.'I/      ') FNYES1=.TRUE.
              IF(WC.EQ.'C+      ') FNYES1=.TRUE.
              IF(WC.EQ.'C-      ') FNYES1=.TRUE.
              IF(WC.EQ.'C*      ') FNYES1=.TRUE.
              IF(WC.EQ.'C/      ') FNYES1=.TRUE.
              IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
              IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
              IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
              IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
              IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
              IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
              IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
              IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
              IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
              IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
              IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
              IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
              IF(WC.EQ.'SET     ') FNYES1=.TRUE.
              IF(WC.EQ.'W1      ') FNYES1=.TRUE.
              IF(WC.EQ.'W2      ') FNYES1=.TRUE.
              IF(WC.EQ.'W3      ') FNYES1=.TRUE.
              IF(WC.EQ.'W4      ') FNYES1=.TRUE.
              IF(WC.EQ.'W5      ') FNYES1=.TRUE.
              IF(WC.EQ.'CRITS   ') FNYES1=.TRUE.
              IF(WC.EQ.'C       ') FNYES1=.TRUE.
              IF(WC.EQ.'M       ') FNYES1=.TRUE.
              IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
              IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
              IF(F1.EQ.0.AND.F54.EQ.1.AND..NOT.FNYES1) THEN
                  OUTLYNE='INVALID (FOCRIT INPUT) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F54.EQ.1.AND.FNYES1) GO TO 3001
              FNYES1=.FALSE.
              IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
              IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
              IF(WC.EQ.'GET     ') FNYES1=.TRUE.
              IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
              IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
              IF(WC.EQ.'STO     ') FNYES1=.TRUE.
              IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
              IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
              IF(WC.EQ.'PI      ') FNYES1=.TRUE.
              IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
              IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
              IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
              IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
              IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
              IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
              IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
              IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
              IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
              IF(WC.EQ.'COS     ') FNYES1=.TRUE.
              IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
              IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
              IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
              IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
              IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
              IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
              IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
              IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
              IF(WC.EQ.'LN      ') FNYES1=.TRUE.
              IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
              IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
              IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
              IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
              IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
              IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
              IF(WC.EQ.'POW     ') FNYES1=.TRUE.
              IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
              IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
              IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
              IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
              IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
              IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
              IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
              IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
              IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
              IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
              IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
              IF(WC.EQ.'J1      ') FNYES1=.TRUE.
              IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
              IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
              IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
              IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
              IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
              IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
              IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
              IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
              IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
              IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
              IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
              IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
              IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
              IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
              IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
              IF(WC.EQ.'+       ') FNYES1=.TRUE.
              IF(WC.EQ.'-       ') FNYES1=.TRUE.
              IF(WC.EQ.'*       ') FNYES1=.TRUE.
              IF(WC.EQ.'/       ') FNYES1=.TRUE.
              IF(WC.EQ.'I+      ') FNYES1=.TRUE.
              IF(WC.EQ.'I-      ') FNYES1=.TRUE.
              IF(WC.EQ.'I*      ') FNYES1=.TRUE.
              IF(WC.EQ.'I/      ') FNYES1=.TRUE.
              IF(WC.EQ.'C+      ') FNYES1=.TRUE.
              IF(WC.EQ.'C-      ') FNYES1=.TRUE.
              IF(WC.EQ.'C*      ') FNYES1=.TRUE.
              IF(WC.EQ.'C/      ') FNYES1=.TRUE.
              IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
              IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
              IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
              IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
              IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
              IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
              IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
              IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
              IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
              IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
              IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
              IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
              IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
              IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
              IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
              IF(WC.EQ.'SET     ') FNYES1=.TRUE.
              IF(WC.EQ.'W1      ') FNYES1=.TRUE.
              IF(WC.EQ.'W2      ') FNYES1=.TRUE.
              IF(WC.EQ.'W3      ') FNYES1=.TRUE.
              IF(WC.EQ.'W4      ') FNYES1=.TRUE.
              IF(WC.EQ.'W5      ') FNYES1=.TRUE.
              IF(WC.EQ.'CRITS   ') FNYES1=.TRUE.
              IF(WC.EQ.'C       ') FNYES1=.TRUE.
              IF(WC.EQ.'M       ') FNYES1=.TRUE.
              IF(WC.EQ.'DEL     ') FNYES1=.TRUE.
              IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
              IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
              IF(F1.EQ.0.AND.F54.EQ.2.AND..NOT.FNYES1) THEN
                  FNYES1=.FALSE.
                  OUTLYNE='INVALID (FOCRIT UPDATE) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F54.EQ.2.AND.FNYES1) GO TO 3001
              IF(F1.EQ.0.AND.F29.EQ.1) THEN
                  OUTLYNE='INVALID (VARIABLE INPUT) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F29.EQ.2) THEN
                  OUTLYNE='INVALID (VARIABLE UPDATE) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F51.EQ.1) THEN
                  OUTLYNE='INVALID (TVAR INPUT) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F51.EQ.2) THEN
                  OUTLYNE='INVALID (TVAR UPDATE) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F52.EQ.1) THEN
                  OUTLYNE='INVALID (COMPVAR INPUT) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F52.EQ.2) THEN
                  OUTLYNE='INVALID (COMPVAR UPDATE) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F53.EQ.1) THEN
                  OUTLYNE='INVALID (TOPER INPUT) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F53.EQ.2) THEN
                  OUTLYNE='INVALID (TOPER UPDATE) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F54.EQ.1) THEN
                  OUTLYNE='INVALID (FOCRIT INPUT) COMMAND'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F54.EQ.2) THEN
                  OUTLYNE='INVALID (FOCRIT UPDATE) COMMAND'
                  CALL SHOWIT(1)
              END IF
C       SET F48 AND F49 TO ZERO
              F48=0
              F49=0
              CALL MACFAL
              RETURN
C       A MATCH WAS FOUND
          END IF
 3001     CONTINUE
C
C       AT THIS POINT A SEARCH IS PERFORMED FOR EACH LEVEL OF THE
C       PROGRAM TO DETERMINE IF THE VALID PROGRAM COMMAND IS VALID
C       WITHIN THAT LEVEL.
C
C       FOR THE CMD LEVEL FLAG F1=1
C
C       MANY COMMANDS MAY BE ISSUED WHEN F1=1. THE COMMAND WHICH
C       ARE NOT ALLOWED ARE:
C               EOM - END OF MACRO
C
C       AND THE LMEDITING COMMANDS:
C
C               PR -  PRINT A LINE OR LINES
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
C               NEXT- REPREAT PRIOR SEARCH FURTER DOWN MACRO
C       QUIT OR QU -  QUIT WITHOUT CHANGING MACRO FILE(MAC.DAT)
C               RE -  REPLACE CURRENT LINE WITH WHAT FOLLOWS
C               TP -  GO TO TOP OF THE MACRO
C
C       AND ALL MACRO EXECUTION SPECIFIC COMMANDS:
C
C       NSUB,SSUB,QSUB,MOVE (WITH NW AS QUALIFIER),CSUB,AND PUTR,
C       TRACE (ON OR OFF),ACCSUB,RETURN
C       AND ALL THE LENS INPUT AND LENS UPDATE COMMANDS ETC.
C       SO FOR CMD LEVEL WITHOUT MACRO EXECUTION;
C
          IF(F1.EQ.1.AND.F4.EQ.0.AND.F17.EQ.0) THEN
C       SPECT LEVEL IS NOT ACTIVE
C
              CALL CONT1(STP)
              IF(STP) RETURN
          END IF
C       IF THE SPECT LEVEL IS ACTIVE THEN
C
          IF(F1.EQ.1.AND.F4.EQ.0.AND.F17.EQ.1) THEN
C
              CALL CONT2(STP)
              IF(STP) RETURN
C
          END IF
C
C       IF F1=1 AND F4=1 (MACRO EXECUTION) FEWER COMMAND ARE
C       DISALLOWED.
C
          IF(F1.EQ.1.AND.F4.EQ.1.AND.F17.EQ.0.AND.F10.EQ.0
     1    .OR.F1.EQ.1.AND.F4.EQ.1.AND.F17.EQ.0.AND.F11.EQ.0) THEN
C
C       THESE ARE THE COMMANDS NOT ALLOWED AT THE CMD LEVEL
C       DURING MACRO EXECUTION
C
              WC1=WC
              CALL CONTA(WC1,IS)
              IF(IS) THEN
                  OUTLYNE='INVALID CMD LEVEL COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C       COMMANDS NOT ALLOWED DURING SPECT OPERATION
          IF(F1.EQ.1.AND.F4.EQ.1.AND.F17.EQ.1) THEN
C
              WC1=WC
              CALL CONTB(WC1,IS)
              IF(IS) THEN
                  OUTLYNE='INVALID SPECT LEVEL COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       FOR MACRO CREATION MODE, MANNY COMMANDS MAY BE ISSUED.
C       THE COMMANDS WHICH MAY NOT BE ISSUED ARE:
C
C               MACRO - CREATE A NEW MACRO
C               IMF   - INITIALIZE MACRO FILE
C               MDEL  - DELETE A MACRO
C               MKEY  - CHANGE A MACRO KEY
C
C       AND ALL MACRO LMEDIT COMMANDS:
C
C       LMEDIT,FL,BT,EX,FL,GO,LO,RE,TP
C       ,QU,QUIT, AND NEXT
C
          IF(F2.EQ.1) THEN
              WC1=WC
              WQ1=WQ
              CALL CONTC(WC1,WQ1,IS)
              IF(IS) THEN
                  OUTLYNE='INVALID COMMAND USED IN MACRO CREATION'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       FOR THE MACRO EDIT LEVEL A FEW COMMANDS
C       ARE EXECUTABLE. THEY ARE:
C
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
C               NEXT- PROCEED WITH SEARCH FURTHER DOWN MACRO.
C       QUIT OR QU -  QUIT WITHOUT CHANGING MACRO FILE(MAC.DAT)
C               RE -  REPLACE CURRENT LINE WITH WHAT FOLLOWS
C               TP -  GO TO TOP OF THE MACRO
C
C       AN INPUT LINE IN THE LMEDIT MODE BEGINNING WITH ANY
C       OTHER WORD IS CONSIDERED THE NEXT LINE TO BE INPUT
C       AS IN MACRO INPUT MODE. IT IS EITHER APPENDED OR INSERTED
C       DEPENDING ON WHETHER WE ARE AT THE END OF THE CURRENT
C       MACRO OR NOT. THE TESTING FOR WHETHER OR NOT TO
C       EXECUTE A COMMAND OR NOT IS DONE IN THE MEDIT SUBROUTINE.
C
C       THE ONLY FORBIDDEN COMMANDS IN THE MEDIT MODE ARE:
C
C               MACRO,EOM,IMF,AND LMEDIT
C
          IF(F3.EQ.1) THEN
              WC1=WC
              CALL CONTD(WC1,IS)
              IF(IS) THEN
                  OUTLYNE='INVALID LMEDIT COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C       THE ONLY FORBIDDEN COMMANDS IN THE MACRO INPUT MODE ARE:
C
C               MACRO,IMF,LMEDIT AND FL
C
          IF(F2.EQ.1) THEN
              WC1=WC
              CALL CONTE(WC1,IS)
              IF(IS) THEN
                  OUTLYNE='INVALID MACRO INPUT COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       OTHER PROGRAM LEVELS MAY ONLY ALLOW A SMALL
C       NUMBER OF COMMANDS. TESTS AT THESE LEVELS WILL
C       TEST FOR ALLOWED VICE DISALLOWED COMMANDS.
C
          IF(F3.EQ.1) THEN
C               WE ARE IN MACRO EDITING (LMEDIT). THE SUBROUTINE
C               MMEDIT AND MACEDT AND MACMOD CONTAIN THE RULES FOR
C               RESPONDING TO INPUT DURING THIS MODE.SUBROUTINE
C               MMEDIT WAS CALLED WHEN F1=1. IT CALLED SUBROUTINE
C               MACEDT.
C       HERE WE CALL SUBROUTINE MACMOD
              CALL MACMOD
              LASTCOMWRD=WC
              LASTWASFOB=.FALSE.
              IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
              RETURN
          END IF
C
          IF(F5.EQ.1.AND.F10.EQ.0.OR.F5.EQ.1.AND.F11.EQ.0) THEN

C               WE ARE IN THE LENS INPUT ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THE LENS INPUT LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN LENS INPUT MODE. THE SUBROUTINE
C               LENIN AND LENNS CONTAIN RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
              WC1=WC
              WS1=WS
              SST1=SST
              CALL CONTF(WC1,IS)

              IF(IS) THEN
                  CALL LENIN
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'?') THEN
                  CALL QUERRYY
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                  CALL BLANK0
                  F50=0
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.' '.AND.F50.NE.1) THEN
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
              END IF
              OUTLYNE='INVALID LENS INPUT COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F6.EQ.1.AND.F10.EQ.0.OR.F6.EQ.1.AND.F11.EQ.0) THEN
C
C               WE ARE IN THE LENS UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THE LENS UPDATE LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C
C               WE ARE IN LENS UPDATE MODE. THE SUBROUTINE
C               LENUP AND ULENNS CONTAIN RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
C
C       THIS IS THE LIST OF VALID LENS UPDATE COMMANDS
              WC1=WC
              WS1=WS
              SST1=SST
              CALL CONTG(WC1,IS)
              IF(IS) THEN
                  CALL LENUP
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  IF(WC.EQ.'EOS') YESEOS=.TRUE.
                  IF(WC.NE.'EOS') YESEOS=.FALSE.
C     CALL RE_DISPLAY_LENS(YESEOS)
                  RETURN
              END IF
              IF(WC.EQ.'?') THEN
                  CALL QUERRYY
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                  CALL BLANK0
                  F50=0
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.' '.AND.F50.NE.1) THEN
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
              END IF
              OUTLYNE='INVALID LENS UPDATE COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F7.EQ.1.AND.F10.EQ.0.OR.F7.EQ.1.AND.F11.EQ.0) THEN
C               WE ARE IN THE SPSRF INPUT ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN SPSRF INPUT MODE. THE SUBROUTINE
C               SPSIN AND SPIN2 AND SUBROUTINES THEY CALL
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
              WC1=WC
              WQ1=WQ
              CALL CONTH(WC1,WQ1,IS)
              IF(IS) THEN
                  CALL SPSRF2()
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'?') THEN
                  CALL QUERRYY
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                  CALL BLANK0
                  F50=0
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.' '.AND.F50.NE.1) THEN
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
              END IF
              OUTLYNE='INVALID SPSRF INPUT COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F8.EQ.1.AND.F10.EQ.0.OR.F8.EQ.1.AND.F11.EQ.0) THEN
C               WE ARE IN THE SPSRF UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN SPSRF UPDATE MODE. THE SUBROUTINE
C               SPSUP,SPUP2 AND AND SUBROUTINES THEY CALL
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
              WC1=WC
              WQ1=WQ
              CALL CONTI(WC1,WQ1,IS)
              IF(IS) THEN
                  CALL SPSRF2()
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'?') THEN
                  CALL QUERRYY
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                  CALL BLANK0
                  F50=0
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.' '.AND.F50.NE.1) THEN
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
              END IF
              OUTLYNE='INVALID SPSRF UPDATE COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F9.EQ.1.AND.F10.EQ.0.OR.F9.EQ.1.AND.F11.EQ.0) THEN
C               WE ARE IN THE SPFIT INPUT ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN SPFIT  MODE. THE SUBROUTINE
C               SPFIT AND SPFIT2 AND THE SUBROUTINES THEY CALL
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
              WC1=WC
              CALL CONTJ(WC1,IS)
              IF(IS) THEN
                  CALL SPFIT2
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.'?') THEN
                      CALL QUERRYY
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
                  IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                      CALL BLANK0
                      F50=0
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  ELSE
                      IF(WC.EQ.' '.AND.F50.NE.1) THEN
                          LASTCOMWRD=WC
                          LASTWASFOB=.FALSE.
                          IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                          RETURN
                      END IF
                  END IF
                  OUTLYNE='INVALID SPFIT INPUT COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(F27.EQ.1.OR.F27.EQ.2) THEN
C               WE ARE IN THE MERIT INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN MERIT INPUT OR UPDATE MODE. THE SUBROUTINE
C               MERIT1.FOR
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
              WC1=WC
              CALL CONTK(WC1,IS)
              IF(IS) THEN
                  CALL MERIT1
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.'?') THEN
                      CALL QUERRYY
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
                  IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                      CALL BLANK0
                      F50=0
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  ELSE
                      IF(WC.EQ.' '.AND.F50.NE.1) THEN
                          LASTCOMWRD=WC
                          LASTWASFOB=.FALSE.
                          IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                          RETURN
                      END IF
                  END IF
                  IF(F27.EQ.1) THEN
                      OUTLYNE='INVALID (MERIT INPUT) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(F27.EQ.2) THEN
                      OUTLYNE='INVALID (MERIT UPDATE) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
          IF(F53.EQ.1.OR.F53.EQ.2) THEN
C               WE ARE IN THE TOPER INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN TOPER INPUT OR UPDATE MODE. THE SUBROUTINE
C               MCOMP.FOR
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
              WC1=WC
              CALL CONTL(WC1,IS)
              IF(IS) THEN
                  CORMOD=1
C       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
                  CURFIG=1

                  CALL TOPER1
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.'?') THEN
                      CALL QUERRYY
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
                  IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                      CALL BLANK0
                      F50=0
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  ELSE
                      IF(WC.EQ.' '.AND.F50.NE.1) THEN
                          LASTCOMWRD=WC
                          LASTWASFOB=.FALSE.
                          IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                          RETURN
                      END IF
                  END IF
                  IF(F53.EQ.1) THEN
                      OUTLYNE='INVALID (TOPER INPUT) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(F53.EQ.2) THEN
                      OUTLYNE='INVALID (TOPER UPDATE) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
          IF(F54.EQ.1.OR.F54.EQ.2) THEN
C               WE ARE IN THE FOCRIT INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               FOCRIT1.FOR
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
              WC1=WC
              CALL CONTM(WC1,IS)
              IF(IS) THEN
C
                  CORMOD=1
C       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
                  CURFIG=1
                  CALL FOCRIT1
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.'?') THEN
                      CALL QUERRYY
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
                  IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                      CALL BLANK0
                      F50=0
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  ELSE
                      IF(WC.EQ.' '.AND.F50.NE.1) THEN
                          LASTCOMWRD=WC
                          LASTWASFOB=.FALSE.
                          IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                          RETURN
                      END IF
                  END IF
                  IF(F54.EQ.1) THEN
                      OUTLYNE='INVALID (FOCRIT INPUT) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(F54.EQ.2) THEN
                      OUTLYNE='INVALID (FOCRIT UPDATE) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C
          IF(F29.EQ.1.OR.F29.EQ.2) THEN
C               WE ARE IN THE VARIABLE INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN VARIABLE INPUT OR UPDATE MODE. THE SUBROUTINE
C               VARBLE.FOR
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
              WC1=WC
              FF29=F29
              CALL CONTN(WC1,FF29,IS)
              IF(IS) THEN
                  IF(WQ.EQ.'ALL'.AND.SQ.EQ.1) THEN
                      WQ='        '
                      SQ=0
                      OW4=W4
                      OW3=W3
                      OW2=W2
                      OW1=W1
                      OS4=S4
                      OS3=S3
                      OS2=S2
                      OS1=S1
                      ODF4=DF4
                      ODF3=DF3
                      ODF2=DF2
                      ODF1=DF1
                      DO I=0,INT(SYSTEM1(20))
                          IF(WC.EQ.'CV'.AND.I.EQ.0.OR.WC.EQ.'CVTOR'.AND.I.EQ.0) GO TO 67
                          IF(WC.EQ.'RD'.AND.I.EQ.0) GO TO 67
                          IF(WC.EQ.'RDTOR'.AND.I.EQ.0) GO TO 67
                          IF(WC.EQ.'CV'.AND.I.EQ.INT(SYSTEM1(20))
     1                    .OR.WC.EQ.'CVTOR'.AND.I.EQ.INT(SYSTEM1(20))) GO TO 67
                          IF(WC.EQ.'RD'.AND.I.EQ.INT(SYSTEM1(20))
     3                    .OR.WC.EQ.'RDTOR'.AND.I.EQ.INT(SYSTEM1(20))) GO TO 67
                          IF(DUMMMY(I).AND.WC.EQ.'TH') GO TO 67
                          IF(DUMMMY(I).AND.WC.EQ.'CV') GO TO 67
                          IF(DUMMMY(I).AND.WC.EQ.'RD') GO TO 67
                          IF(DUMMMY(I).AND.WC.EQ.'CVTOR') GO TO 67
                          IF(DUMMMY(I).AND.WC.EQ.'RDTOR') GO TO 67
                          IF(WC.EQ.'TH'.AND.I.EQ.INT(SYSTEM1(20))) GO TO 67
                          IF(WC.EQ.'TH'.AND.I.EQ.0) GO TO 67
                          IF(WC.EQ.'N1'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
                          IF(WC.EQ.'N2'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
                          IF(WC.EQ.'N3'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
                          IF(WC.EQ.'N4'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
                          IF(WC.EQ.'N5'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
                          IF(WC.EQ.'N6'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
                          IF(WC.EQ.'N7'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
                          IF(WC.EQ.'N8'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
                          IF(WC.EQ.'N9'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
                          IF(WC.EQ.'N10'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
                          IF(WC.EQ.'INDEX'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 67
                          IF(WC.EQ.'VNUM'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 67
                          IF(WC.EQ.'DPART'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 67
                          IF(SYSTEM1(27).EQ.1.0D0.OR.SYSTEM1(27).EQ.2.0D0) THEN
                              IF(I.EQ.1.OR.I.EQ.2) THEN
                                  IF(WC.EQ.'TH') GO TO 67
                              END IF
                          END IF
                          IF(SYSTEM1(27).EQ.-1.0D0.OR.SYSTEM1(27).EQ.2.0D0) THEN
                              IF(I.EQ.INT(SYSTEM1(20))-1.OR.I.EQ.INT(SYSTEM1(20))-2) THEN
                                  IF(WC.EQ.'TH') GO TO 67
                              END IF
                          END IF
                          W5=OW4
                          W4=OW3
                          W3=OW2
                          W2=OW1
                          S5=OS4
                          S4=OS3
                          S3=OS2
                          S2=OS1
                          DF5=ODF4
                          DF4=ODF3
                          DF3=ODF2
                          DF2=ODF1
                          W1=DBLE(I)
                          S1=1
                          DF1=0
                          CALL VARBLL
 67                       CONTINUE
                      END DO
                  ELSE
                      CALL VARBLL
                  END IF
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
C
              WC1=WC
              CALL CONTO(WC1,IS)
              IF(IS) THEN
                  IF(WQ.EQ.'ALL'.AND.SQ.EQ.1) THEN
                      WQ='        '
                      SQ=0
                      OW4=W4
                      OW3=W3
                      OW2=W2
                      OW1=W1
                      OS4=S4
                      OS3=S3
                      OS2=S2
                      OS1=S1
                      ODF4=DF4
                      ODF3=DF3
                      ODF2=DF2
                      ODF1=DF1
                      DO I=0,INT(SYSTEM1(20))
                          IF(WC.EQ.'CV'.AND.I.EQ.0.OR.WC.EQ.'CVTOR'.AND.I.EQ.0) GO TO 68
                          IF(WC.EQ.'RD'.AND.I.EQ.0) GO TO 68
                          IF(WC.EQ.'RDTOR'.AND.I.EQ.0) GO TO 68
                          IF(WC.EQ.'CV'.AND.I.EQ.INT(SYSTEM1(20))
     1                    .OR.WC.EQ.'CVTOR'.AND.I.EQ.INT(SYSTEM1(20))) GO TO 68
                          IF(WC.EQ.'RD'.AND.I.EQ.INT(SYSTEM1(20))
     3                    .OR.WC.EQ.'RDTOR'.AND.I.EQ.INT(SYSTEM1(20))) GO TO 68
                          IF(DUMMMY(I).AND.WC.EQ.'TH') GO TO 68
                          IF(DUMMMY(I).AND.WC.EQ.'CV') GO TO 68
                          IF(DUMMMY(I).AND.WC.EQ.'RD') GO TO 68
                          IF(DUMMMY(I).AND.WC.EQ.'CVTOR') GO TO 68
                          IF(DUMMMY(I).AND.WC.EQ.'RDTOR') GO TO 68
                          IF(WC.EQ.'TH'.AND.I.EQ.0) GO TO 68
                          IF(WC.EQ.'TH'.AND.I.EQ.INT(SYSTEM1(20))) GO TO 68
                          IF(WC.EQ.'N1'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
                          IF(WC.EQ.'N2'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
                          IF(WC.EQ.'N3'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
                          IF(WC.EQ.'N4'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
                          IF(WC.EQ.'N5'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
                          IF(WC.EQ.'N6'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
                          IF(WC.EQ.'N7'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
                          IF(WC.EQ.'N8'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
                          IF(WC.EQ.'N9'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
                          IF(WC.EQ.'N10'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
                          IF(WC.EQ.'INDEX'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 68
                          IF(WC.EQ.'VNUM'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 68
                          IF(WC.EQ.'DPART'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 68
                          IF(SYSTEM1(27).EQ.1.0D0.OR.SYSTEM1(27).EQ.2.0D0) THEN
                              IF(I.EQ.1.OR.I.EQ.2) THEN
                                  IF(WC.EQ.'TH') GO TO 68
                              END IF
                          END IF
                          IF(SYSTEM1(27).EQ.-1.0D0.OR.SYSTEM1(27).EQ.2.0D0) THEN
                              IF(I.EQ.INT(SYSTEM1(20))-1.OR.I.EQ.INT(SYSTEM1(20))-2) THEN
                                  IF(WC.EQ.'TH') GO TO 68
                              END IF
                          END IF
                          W5=OW4
                          W4=OW3
                          W3=OW2
                          W2=OW1
                          S5=OS4
                          S4=OS3
                          S3=OS2
                          S2=OS1
                          DF5=ODF4
                          DF4=ODF3
                          DF3=ODF2
                          DF2=ODF1
                          W1=DBLE(I)
                          S1=1
                          DF1=0
                          CALL VARBLL
 68                       CONTINUE
                      END DO
                  ELSE
                      CALL VARBLL
                  END IF
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.'?') THEN
                      CALL QUERRYY
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
                  IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                      CALL BLANK0
                      F50=0
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  ELSE
                      IF(WC.EQ.' '.AND.F50.NE.1) THEN
                          LASTCOMWRD=WC
                          LASTWASFOB=.FALSE.
                          IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                          RETURN
                      END IF
                  END IF
                  IF(F29.EQ.1) THEN
                      OUTLYNE='INVALID (VARIABLE INPUT) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(F29.EQ.2) THEN
                      OUTLYNE='INVALID (VARIABLE UPDATE) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C
          IF(F51.EQ.1.OR.F51.EQ.2) THEN
C               WE ARE IN THE TVAR INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN VARIABLE INPUT OR UPDATE MODE. THE SUBROUTINE
C               VARBLE.FOR
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
              WC1=WC
              FF51=F51
              CALL CONTP(WC1,FF51,IS)
              IF(IS) THEN
                  CALL TVARBLL
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.'?') THEN
                      CALL QUERRYY
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
                  IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                      CALL BLANK0
                      F50=0
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  ELSE
                      IF(WC.EQ.' '.AND.F50.NE.1) THEN
                          LASTCOMWRD=WC
                          LASTWASFOB=.FALSE.
                          IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                          RETURN
                      END IF
                  END IF
                  IF(F51.EQ.1) THEN
                      OUTLYNE='INVALID (TVAR INPUT) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(F51.EQ.2) THEN
                      OUTLYNE='INVALID (TVAR UPDATE) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
          IF(F52.EQ.1.OR.F52.EQ.2) THEN
C               WE ARE IN THE COMPVAR INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN VARIABLE INPUT OR UPDATE MODE.
              WC1=WC
              FF52=F52
              CALL CONTQ(WC1,FF52,IS)
              IF(IS) THEN
                  CALL CVARBLL
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.'?') THEN
                      CALL QUERRYY
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
                  IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                      CALL BLANK0
                      F50=0
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  ELSE
                      IF(WC.EQ.' '.AND.F50.NE.1) THEN
                          LASTCOMWRD=WC
                          LASTWASFOB=.FALSE.
                          IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                          RETURN
                      END IF
                  END IF
                  IF(F52.EQ.1) THEN
                      OUTLYNE='INVALID (COMPVAR INPUT) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(F52.EQ.2) THEN
                      OUTLYNE='INVALID (COMPVAR UPDATE) COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C
          IF(F10.EQ.1.OR.F11.EQ.1) THEN
C
C               WE ARE IN THE CONFIGS INPUT (F10=1) OR
C       CONFIGS UPDATE (F11=0) ROUTINE
C
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THE CONFIGS INPUT OR UPDATE LEVEL.
C       TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C
C               WE ARE IN CONFIGS INPUT LEVEL. THE SUBROUTINE
C        CFGIN,CFGUP  AND CFGIN2 CONTAIN RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
C       ALL OF THE COMMAND VALID AT THE UPDATE LENS LEVEL ARE
C       VALID IN CONFIGS AND UPDATE CONFIGS
C       THIS IS THE LIST OF VALID LENS UPDATE COMMANDS
              WC1=WC
              WQ1=WQ
              SQ1=SQ
              CALL CONTR(WC1,WQ1,IS)
              IF(IS) THEN
                  RET=0
                  CALL CFGIN2
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
                  IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'?') THEN
                  CALL QUERRYY
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.' '.AND.F50.EQ.1) THEN
                  CALL BLANK0
                  F50=0
                  LASTCOMWRD=WC
                  LASTWASFOB=.FALSE.
                  IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                  RETURN
              ELSE
                  IF(WC.EQ.' '.AND.F50.NE.1) THEN
                      LASTCOMWRD=WC
                      LASTWASFOB=.FALSE.
                      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                  END IF
              END IF
              IF(F1.EQ.0.AND.F10.EQ.1) THEN
                  OUTLYNE='INVALID CONFIGS INPUT COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(F1.EQ.0.AND.F11.EQ.1) THEN
                  OUTLYNE='INVALID CONFIGS UPDATE COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C       CHECK FOR VALID QUALIFIERS FOR UPDATE OR U
C       ISSUED AT THE CMD LEVEL
          IF(F1.EQ.1.AND.F17.EQ.0.AND.WC.EQ.'U') THEN
              IF(WQ.NE.'LENS'.AND.WQ.NE.'SPSRF'.AND.
     1        WQ.NE.'CONFIGS'.AND.WQ.NE.'CONFIG'.AND.WQ
     2        .NE.'VARIABLE'.AND.WQ.NE.'MERIT'.AND.WQ.NE.
     3        'TVAR'.AND.WQ.NE.'COMPVAR'.AND.WQ.NE.'TOPER'.AND.WQ.NE.'FOCRIT'
     4        .AND.WQ.NE.'L'.AND.WQ.NE.'SP'.AND.WQ.NE.'VB'.AND.
     5        WQ.NE.'CF'.AND.WQ.NE.'RS'.AND.WQ.NE.'M'.AND.WQ.NE.'TVB'
     6        .AND.WQ.NE.'CMP'.AND.WQ.NE.'TOP'.AND.WQ.NE.'FC') THEN
                  OUTLYNE='INVALID CMD LEVEL COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(F1.EQ.1.AND.F17.EQ.1.AND.WC.EQ.'U') THEN
              OUTLYNE='INVALID SPECT LEVEL COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(F1.EQ.1..AND.F17.EQ.0.AND.WC.EQ.'UPDATE') THEN
              IF(WQ.NE.'LENS'.AND.WQ.NE.'SPSRF'.AND.
     1        WQ.NE.'CONFIGS'.AND.WQ.NE.'CONFIG'.AND.WQ
     2        .NE.'VARIABLE'.AND.WQ.NE.'MERIT'.AND.WQ.NE.
     3        'TVAR'.AND.WQ.NE.'COMPVAR'.AND.WQ.NE.'TOPER'.AND.WQ.NE.'FOCRIT'
     4        .AND.WQ.NE.'L'.AND.WQ.NE.'SP'.AND.WQ.NE.'VB'.AND.
     5        WQ.NE.'CF'.AND.WQ.NE.'RS'.AND.WQ.NE.'M'.AND.WQ.NE.'TVB'
     6        .AND.WQ.NE.'CMP'.AND.WQ.NE.'TOP'.AND.WQ.NE.'FC') THEN
                  OUTLYNE='INVALID CMD LEVEL COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(F1.EQ.1.AND.F17.EQ.1.AND.WC.EQ.'UPDATE') THEN
              OUTLYNE='INVALID SPECT LEVEL COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(F2.EQ.1) THEN
C               WE ARE IN MACRO CREATION MODE. THE SUBROUTINE
C               MACIN CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
              WWW1=0.0D0
              CALL MACIN(WWW1)
              LASTCOMWRD=WC
              LASTWASFOB=.FALSE.
              IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
              RETURN
C       F2 NOT 1,PROCEED
          END IF
C
C       FOR THE CMD LEVEL COMMANDS F1=1, SO NOW CALL CMDER

          CALL CMDER

          LASTCOMWRD=WC
          LASTWASFOB=.FALSE.
          IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.

          RETURN
      END


      SUBROUTINE CONTA(WC,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'START')               IS=.TRUE.
          IF(WC.EQ.'WAVLN')               IS=.TRUE.
          IF(WC.EQ.'INT')                 IS=.TRUE.
          IF(WC.EQ.'DIRECT')              IS=.TRUE.
          IF(WC.EQ.'INSERT')              IS=.TRUE.
C               IF(WC.EQ.'LOADPHOT')            IS=.TRUE.
C               IF(WC.EQ.'LOADSCOT')            IS=.TRUE.
          IF(WC.EQ.'DROP')                IS=.TRUE.
          IF(WC.EQ.'DELETE')              IS=.TRUE.
          IF(WC.EQ.'GETFILE')             IS=.TRUE.
          IF(WC.EQ.'BLACKBDY')            IS=.TRUE.
          IF(WC.EQ.'PHOTOPIC')            IS=.TRUE.
          IF(WC.EQ.'SCOTOPIC')            IS=.TRUE.
          IF(WC.EQ.'PUT')                 IS=.TRUE.
          IF(WC.EQ.'LIST')                IS=.TRUE.
          IF(WC.EQ.'RENAME')              IS=.TRUE.
          IF(WC.EQ.'PUNCH')               IS=.TRUE.
          IF(WC.EQ.'INTER')               IS=.TRUE.
          IF(WC.EQ.'NARCIN')              IS=.TRUE.
          IF(WC.EQ.'FLNAME')              IS=.TRUE.
          IF(WC.EQ.'ENDTABLE')            IS=.TRUE.
          IF(WC.EQ.'NARC')                IS=.TRUE.
          IF(WC.EQ.'EOS')                 IS=.TRUE.
          IF(WC.EQ.'DATA')                IS=.TRUE.
          IF(WC.EQ.'CUME')                IS=.TRUE.
          IF(WC.EQ.'WFACTOR')             IS=.TRUE.
          IF(WC.EQ.'WORK')                IS=.TRUE.
          IF(WC.EQ.'PTABLE')              IS=.TRUE.
          IF(WC.EQ.'DIR')                 IS=.TRUE.
          IF(WC.EQ.'FILE')                IS=.TRUE.
          IF(WC.EQ.'NAME')                IS=.TRUE.
          IF(WC.EQ.'PLOTR')               IS=.TRUE.
          IF(WC.EQ.'PLOTT')               IS=.TRUE.
          RETURN
      END


      SUBROUTINE CONTB(WC,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'EXIT')                IS=.TRUE.
          IF(WC.EQ.'EXI')                 IS=.TRUE.
          IF(WC.EQ.'LENS')                IS=.TRUE.
          IF(WC.EQ.'CONFIGS')             IS=.TRUE.
          IF(WC.EQ.'CONFIG')              IS=.TRUE.
          IF(WC.EQ.'SPSRF')               IS=.TRUE.
          IF(WC.EQ.'SPFIT')               IS=.TRUE.
          IF(WC.EQ.'SPECT')               IS=.TRUE.
          IF(WC.EQ.'MACRO')               IS=.TRUE.
          IF(WC.EQ.'LMEDIT')              IS=.TRUE.
          IF(WC.EQ.'UPDATE')              IS=.TRUE.
          IF(WC.EQ.'U')                   IS=.TRUE.
          IF(WC.EQ.'TOLER')               IS=.TRUE.
          IF(WC.EQ.'FOE')                 IS=.TRUE.
          IF(WC.EQ.'VARIABLE')            IS=.TRUE.
          IF(WC.EQ.'VARI')                IS=.TRUE.
          IF(WC.EQ.'MERIT')               IS=.TRUE.
          IF(WC.EQ.'TVAR')                IS=.TRUE.
          IF(WC.EQ.'COMPVAR')             IS=.TRUE.
          IF(WC.EQ.'LAYOUT')              IS=.TRUE.
          RETURN
      END
      SUBROUTINE CONTC(WC,WQ,IS)
          IMPLICIT NONE
          CHARACTER WC*8,WQ*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'IMF')         IS=.TRUE.
          IF(WC.EQ.'MACRO')       IS=.TRUE.
          IF(WC.EQ.'MDEL')        IS=.TRUE.
          IF(WC.EQ.'MKEY')        IS=.TRUE.
          IF(WC.EQ.'LMEDIT')      IS=.TRUE.
          IF(WC.EQ.'FL')          IS=.TRUE.
          IF(WC.EQ.'BT')          IS=.TRUE.
          IF(WC.EQ.'EX')          IS=.TRUE.
          IF(WC.EQ.'GO')          IS=.TRUE.
          IF(WC.EQ.'GOQUIET')     IS=.TRUE.
          IF(WC.EQ.'LO'.AND.WQ.EQ.'Q')      IS=.TRUE.
          IF(WC.EQ.'LO'.AND.WQ.EQ.'C')      IS=.TRUE.
          IF(WC.EQ.'LO'.AND.WQ.EQ.'CQ')     IS=.TRUE.
          IF(WC.EQ.'LO'.AND.WQ.EQ.'COQ')    IS=.TRUE.
          IF(WC.EQ.'RE')          IS=.TRUE.
          IF(WC.EQ.'TP')          IS=.TRUE.
          IF(WC.EQ.'QU')          IS=.TRUE.
          IF(WC.EQ.'QUIT')        IS=.TRUE.
          IF(WC.EQ.'NEXT')        IS=.TRUE.
          RETURN
      END
      SUBROUTINE CONTD(WC,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'MACRO')             IS=.TRUE.
          IF(WC.EQ.'EOM')               IS=.TRUE.
          IF(WC.EQ.'IMF')               IS=.TRUE.
          IF(WC.EQ.'LMEDIT')            IS=.TRUE.
          RETURN
      END

      SUBROUTINE CONTE(WC,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'MACRO')             IS=.TRUE.
          IF(WC.EQ.'FL')                IS=.TRUE.
          IF(WC.EQ.'IMF')               IS=.TRUE.
          IF(WC.EQ.'LMEDIT')            IS=.TRUE.
          RETURN
      END

      SUBROUTINE CONTF(WC,IS)
          IMPLICIT NONE
          CHARACTER WC*8
!      INTEGER SST
          LOGICAL IS
          IS=.FALSE.
C       THIS IS THE LIST OF VALID LENS INPUT COMMANDS
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'SET')         IS=.TRUE.
          IF(WC.EQ.'W1')          IS=.TRUE.
          IF(WC.EQ.'W2')          IS=.TRUE.
          IF(WC.EQ.'W3')          IS=.TRUE.
          IF(WC.EQ.'W4')          IS=.TRUE.
          IF(WC.EQ.'W5')          IS=.TRUE.
          IF(WC.EQ.'LI')          IS=.TRUE.
          IF(WC.EQ.'MFG')         IS=.TRUE.
          IF(WC.EQ.'CATNUM')      IS=.TRUE.
          IF(WC.EQ.'LABEL')       IS=.TRUE.
          IF(WC.EQ.'LBL')         IS=.TRUE.
          IF(WC.EQ.'LIC')         IS=.TRUE.
          IF(WC.EQ.'INI')         IS=.TRUE.
          IF(WC.EQ.'LTYPE')       IS=.TRUE.
          IF(WC.EQ.'MODE')        IS=.TRUE.
          IF(WC.EQ.'SPTWT')       IS=.TRUE.
          IF(WC.EQ.'NODUM')       IS=.TRUE.
          IF(WC.EQ.'SPTWT2')      IS=.TRUE.
          IF(WC.EQ.'WV')          IS=.TRUE.
          IF(WC.EQ.'WV2')         IS=.TRUE.
          IF(WC.EQ.'UNITS')       IS=.TRUE.
          IF(WC.EQ.'PCW')         IS=.TRUE.
          IF(WC.EQ.'SCW')         IS=.TRUE.
          IF(WC.EQ.'CW')          IS=.TRUE.
          IF(WC.EQ.'SAY')         IS=.TRUE.
          IF(WC.EQ.'SAX')         IS=.TRUE.
          IF(WC.EQ.'WRX')         IS=.TRUE.
          IF(WC.EQ.'WRY')         IS=.TRUE.
          IF(WC.EQ.'BDX')         IS=.TRUE.
          IF(WC.EQ.'BDY')         IS=.TRUE.
          IF(WC.EQ.'NAOY')        IS=.TRUE.
          IF(WC.EQ.'NAOX')        IS=.TRUE.
          IF(WC.EQ.'FNOY')        IS=.TRUE.
          IF(WC.EQ.'FNOX')        IS=.TRUE.
          IF(WC.EQ.'SCY')         IS=.TRUE.
          IF(WC.EQ.'SCX')         IS=.TRUE.
          IF(WC.EQ.'PYIM')        IS=.TRUE.
          IF(WC.EQ.'PXIM')        IS=.TRUE.
          IF(WC.EQ.'RYIM')        IS=.TRUE.
          IF(WC.EQ.'RXIM')        IS=.TRUE.
          IF(WC.EQ.'CV')          IS=.TRUE.
          IF(WC.EQ.'CVTOR')       IS=.TRUE.
          IF(WC.EQ.'GRT')         IS=.TRUE.
          IF(WC.EQ.'GRO')         IS=.TRUE.
          IF(WC.EQ.'GRS')         IS=.TRUE.
          IF(WC.EQ.'GRX')         IS=.TRUE.
          IF(WC.EQ.'GRY')         IS=.TRUE.
          IF(WC.EQ.'GRZ')         IS=.TRUE.
          IF(WC.EQ.'RD')          IS=.TRUE.
          IF(WC.EQ.'RDTOR')       IS=.TRUE.
          IF(WC.EQ.'REAL')        IS=.TRUE.
          IF(WC.EQ.'PARAX')       IS=.TRUE.
          IF(WC.EQ.'FOOTBLOK')    IS=.TRUE.
          IF(WC.EQ.'PIVOT')       IS=.TRUE.
          IF(WC.EQ.'PIVAXIS')     IS=.TRUE.
          IF(WC.EQ.'CC')          IS=.TRUE.
          IF(WC.EQ.'CCTOR')       IS=.TRUE.
          IF(WC.EQ.'ARRAY')       IS=.TRUE.
          IF(WC.EQ.'ASPH')        IS=.TRUE.
          IF(WC.EQ.'ASPH2')       IS=.TRUE.
          IF(WC.EQ.'TASPH')       IS=.TRUE.
          IF(WC.EQ.'AC')          IS=.TRUE.
          IF(WC.EQ.'AD')          IS=.TRUE.
          IF(WC.EQ.'AE')          IS=.TRUE.
          IF(WC.EQ.'AF')          IS=.TRUE.
          IF(WC.EQ.'AG')          IS=.TRUE.
          IF(WC.EQ.'AH')          IS=.TRUE.
          IF(WC.EQ.'AI')          IS=.TRUE.
          IF(WC.EQ.'AJ')          IS=.TRUE.
          IF(WC.EQ.'AK')          IS=.TRUE.
          IF(WC.EQ.'AL')          IS=.TRUE.
          IF(WC.EQ.'ADTOR')       IS=.TRUE.
          IF(WC.EQ.'AETOR')       IS=.TRUE.
          IF(WC.EQ.'AFTOR')       IS=.TRUE.
          IF(WC.EQ.'AGTOR')       IS=.TRUE.
          IF(WC.EQ.'YTORIC')      IS=.TRUE.
          IF(WC.EQ.'XTORIC')      IS=.TRUE.
          IF(WC.EQ.'NORMAL')      IS=.TRUE.
          IF(WC.EQ.'SPIDER')      IS=.TRUE.
          IF(WC.EQ.'APY')         IS=.TRUE.
          IF(WC.EQ.'APX')         IS=.TRUE.
          IF(WC.EQ.'PIY')         IS=.TRUE.
          IF(WC.EQ.'PIX')         IS=.TRUE.
          IF(WC.EQ.'PUY')         IS=.TRUE.
          IF(WC.EQ.'PUX')         IS=.TRUE.
          IF(WC.EQ.'APCY')        IS=.TRUE.
          IF(WC.EQ.'APCX')        IS=.TRUE.
          IF(WC.EQ.'PICY')        IS=.TRUE.
          IF(WC.EQ.'PICX')        IS=.TRUE.
          IF(WC.EQ.'PUCY')        IS=.TRUE.
          IF(WC.EQ.'PUCX')        IS=.TRUE.
          IF(WC.EQ.'COCY')        IS=.TRUE.
          IF(WC.EQ.'COCX')        IS=.TRUE.
          IF(WC.EQ.'PIKUP')       IS=.TRUE.
          IF(WC.EQ.'DEC')         IS=.TRUE.
          IF(WC.EQ.'YD')          IS=.TRUE.
          IF(WC.EQ.'XD')          IS=.TRUE.
          IF(WC.EQ.'ZD')          IS=.TRUE.
          IF(WC.EQ.'PIVY')        IS=.TRUE.
          IF(WC.EQ.'PIVX')        IS=.TRUE.
          IF(WC.EQ.'PIVZ')        IS=.TRUE.
          IF(WC.EQ.'TILT')        IS=.TRUE.
          IF(WC.EQ.'RTILT')       IS=.TRUE.
          IF(WC.EQ.'ALPHA')       IS=.TRUE.
          IF(WC.EQ.'BETA')        IS=.TRUE.
          IF(WC.EQ.'GAMMA')       IS=.TRUE.
          IF(WC.EQ.'GDX     ')    IS=.TRUE.
          IF(WC.EQ.'GDY     ')    IS=.TRUE.
          IF(WC.EQ.'GDZ     ')    IS=.TRUE.
          IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
          IF(WC.EQ.'GBETA   ')    IS=.TRUE.
          IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
          IF(WC.EQ.'ASTOP')       IS=.TRUE.
          IF(WC.EQ.'REFS')        IS=.TRUE.
          IF(WC.EQ.'TH')          IS=.TRUE.
          IF(WC.EQ.'THM')         IS=.TRUE.
          IF(WC.EQ.'PRICE')       IS=.TRUE.
          IF(WC.EQ.'AUTOFUNC')    IS=.TRUE.
C     BEGINNING OF NON-SEQUENTIAL COMMANDS
          IF(WC.EQ.'CCR')         IS=.TRUE.
          IF(WC.EQ.'ROO')         IS=.TRUE.
C     END OF NON-SEQUENTIAL COMMANDS
          IF(WC.EQ.'PY')          IS=.TRUE.
          IF(WC.EQ.'RAYERROR')    IS=.TRUE.
          IF(WC.EQ.'PCY')         IS=.TRUE.
          IF(WC.EQ.'CAY')         IS=.TRUE.
          IF(WC.EQ.'PX')          IS=.TRUE.
          IF(WC.EQ.'PCX')         IS=.TRUE.
          IF(WC.EQ.'CAX')         IS=.TRUE.
          IF(WC.EQ.'CLAP')        IS=.TRUE.
          IF(WC.EQ.'COBS')        IS=.TRUE.
          IF(WC.EQ.'GLA')         IS=.TRUE.
          IF(WC.EQ.'DEFORM')      IS=.TRUE.
          IF(WC.EQ.'DELDEFOR')    IS=.TRUE.
          IF(WC.EQ.'GLASS')       IS=.TRUE.
          IF(WC.EQ.'N6')          IS=.TRUE.
          IF(WC.EQ.'N7')          IS=.TRUE.
          IF(WC.EQ.'N8')          IS=.TRUE.
          IF(WC.EQ.'N9')          IS=.TRUE.
          IF(WC.EQ.'N10')         IS=.TRUE.
          IF(WC.EQ.'N1')          IS=.TRUE.
          IF(WC.EQ.'N2')          IS=.TRUE.
          IF(WC.EQ.'N3')          IS=.TRUE.
          IF(WC.EQ.'N4')          IS=.TRUE.
          IF(WC.EQ.'N5')          IS=.TRUE.
          IF(WC.EQ.'INDEX')       IS=.TRUE.
          IF(WC.EQ.'VNUM')        IS=.TRUE.
          IF(WC.EQ.'DPART')       IS=.TRUE.
          IF(WC.EQ.'SCHOTT')      IS=.TRUE.
          IF(WC.EQ.'SCH2000')     IS=.TRUE.
          IF(WC.EQ.'MULTCLAP')    IS=.TRUE.
          IF(WC.EQ.'MULTCOBS')    IS=.TRUE.
          IF(WC.EQ.'SPIDER')      IS=.TRUE.
          IF(WC.EQ.'SPGR')        IS=.TRUE.
          IF(WC.EQ.'MODEL')       IS=.TRUE.
          IF(WC.EQ.'RADHARD')     IS=.TRUE.
          IF(WC.EQ.'MATL')        IS=.TRUE.
          IF(WC.EQ.'RUSSIAN')     IS=.TRUE.
          IF(WC.EQ.'USER')        IS=.TRUE.
          IF(WC.EQ.'GLCAT')       IS=.TRUE.
          IF(WC.EQ.'OHARA')       IS=.TRUE.
          IF(WC.EQ.'HOYA')        IS=.TRUE.
          IF(WC.EQ.'HIKARI')      IS=.TRUE.
          IF(WC.EQ.'CHANCE')      IS=.TRUE.
          IF(WC.EQ.'CORNIN')      IS=.TRUE.
          IF(WC.EQ.'AIR')         IS=.TRUE.
          IF(WC.EQ.'COATING ')    IS=.TRUE.
          IF(WC.EQ.'REFL')        IS=.TRUE.
          IF(WC.EQ.'REFLTIRO')    IS=.TRUE.
          IF(WC.EQ.'REFLTIR')     IS=.TRUE.
          IF(WC.EQ.'PERFECT')     IS=.TRUE.
          IF(WC.EQ.'IDEAL')       IS=.TRUE.
          IF(WC.EQ.'EOS     ')    IS=.TRUE.
          IF(WC.EQ.'TASPH')       IS=.TRUE.
          IF(WC.EQ.'C')           IS=.TRUE.
          IF(WC.EQ.'M')           IS=.TRUE.
          IF(WC.EQ.'INR')         IS=.TRUE.
          RETURN
      END

      SUBROUTINE CONTG(WC,IS)
C     LENS UPDATE
          IMPLICIT NONE
          CHARACTER WC*8
!      INTEGER SST
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'ARCL')        IS=.TRUE.
          IF(WC.EQ.'STOAX')       IS=.TRUE.
          IF(WC.EQ.'SET')         IS=.TRUE.
          IF(WC.EQ.'W1')          IS=.TRUE.
          IF(WC.EQ.'W2')          IS=.TRUE.
          IF(WC.EQ.'W3')          IS=.TRUE.
          IF(WC.EQ.'W4')          IS=.TRUE.
          IF(WC.EQ.'W5')          IS=.TRUE.
          IF(WC.EQ.'LI')          IS=.TRUE.
          IF(WC.EQ.'MFG')         IS=.TRUE.
          IF(WC.EQ.'CATNUM')      IS=.TRUE.
          IF(WC.EQ.'LABEL')       IS=.TRUE.
          IF(WC.EQ.'LBL')         IS=.TRUE.
          IF(WC.EQ.'LIC')         IS=.TRUE.
          IF(WC.EQ.'INI')         IS=.TRUE.
          IF(WC.EQ.'LTYPE')       IS=.TRUE.
          IF(WC.EQ.'MODE')        IS=.TRUE.
          IF(WC.EQ.'SPTWT')       IS=.TRUE.
          IF(WC.EQ.'NODUM')       IS=.TRUE.
          IF(WC.EQ.'SPTWT2')      IS=.TRUE.
          IF(WC.EQ.'WV')          IS=.TRUE.
          IF(WC.EQ.'WV2')         IS=.TRUE.
          IF(WC.EQ.'UNITS')       IS=.TRUE.
          IF(WC.EQ.'PCW')         IS=.TRUE.
          IF(WC.EQ.'SCW')         IS=.TRUE.
          IF(WC.EQ.'CW')          IS=.TRUE.
          IF(WC.EQ.'SAY')         IS=.TRUE.
          IF(WC.EQ.'SAX')         IS=.TRUE.
          IF(WC.EQ.'WRX')         IS=.TRUE.
          IF(WC.EQ.'WRY')         IS=.TRUE.
          IF(WC.EQ.'BDX')         IS=.TRUE.
          IF(WC.EQ.'BDY')         IS=.TRUE.
          IF(WC.EQ.'NAOY')        IS=.TRUE.
          IF(WC.EQ.'NAOX')        IS=.TRUE.
          IF(WC.EQ.'FNOY')        IS=.TRUE.
          IF(WC.EQ.'FNOX')        IS=.TRUE.
          IF(WC.EQ.'SCY')         IS=.TRUE.
          IF(WC.EQ.'SCX')         IS=.TRUE.
          IF(WC.EQ.'PYIM')        IS=.TRUE.
          IF(WC.EQ.'PXIM')        IS=.TRUE.
          IF(WC.EQ.'RYIM')        IS=.TRUE.
          IF(WC.EQ.'RXIM')        IS=.TRUE.
          IF(WC.EQ.'CV')          IS=.TRUE.
          IF(WC.EQ.'RD')          IS=.TRUE.
          IF(WC.EQ.'CC')          IS=.TRUE.
          IF(WC.EQ.'ASPH')        IS=.TRUE.
          IF(WC.EQ.'ASPH2')       IS=.TRUE.
          IF(WC.EQ.'YTORIC')      IS=.TRUE.
          IF(WC.EQ.'XTORIC')      IS=.TRUE.
          IF(WC.EQ.'NORMAL')      IS=.TRUE.
          IF(WC.EQ.'SPIDER')      IS=.TRUE.
          IF(WC.EQ.'APY')         IS=.TRUE.
          IF(WC.EQ.'APX')         IS=.TRUE.
          IF(WC.EQ.'PIY')         IS=.TRUE.
          IF(WC.EQ.'PIX')         IS=.TRUE.
          IF(WC.EQ.'PUY')         IS=.TRUE.
          IF(WC.EQ.'PUX')         IS=.TRUE.
          IF(WC.EQ.'APCY')        IS=.TRUE.
          IF(WC.EQ.'APCX')        IS=.TRUE.
          IF(WC.EQ.'PICY')        IS=.TRUE.
          IF(WC.EQ.'PICX')        IS=.TRUE.
          IF(WC.EQ.'PUCY')        IS=.TRUE.
          IF(WC.EQ.'PUCX')        IS=.TRUE.
          IF(WC.EQ.'COCY')        IS=.TRUE.
          IF(WC.EQ.'COCX')        IS=.TRUE.
          IF(WC.EQ.'PIKUP')       IS=.TRUE.
          IF(WC.EQ.'DEC')         IS=.TRUE.
          IF(WC.EQ.'TILT')        IS=.TRUE.
          IF(WC.EQ.'RTILT')       IS=.TRUE.
          IF(WC.EQ.'ASTOP')       IS=.TRUE.
          IF(WC.EQ.'REFS')        IS=.TRUE.
          IF(WC.EQ.'TH')          IS=.TRUE.
          IF(WC.EQ.'THM')         IS=.TRUE.
          IF(WC.EQ.'PRICE')       IS=.TRUE.
          IF(WC.EQ.'AUTOFUNC')    IS=.TRUE.
C     BEGINNING OF NON-SEQUENTIAL COMMANDS
          IF(WC.EQ.'CCR')         IS=.TRUE.
          IF(WC.EQ.'ROO')         IS=.TRUE.
C     END OF NON-SEQUENTIAL COMMANDS
          IF(WC.EQ.'RAYERROR')    IS=.TRUE.
          IF(WC.EQ.'PY')          IS=.TRUE.
          IF(WC.EQ.'PCY')         IS=.TRUE.
          IF(WC.EQ.'CAY')         IS=.TRUE.
          IF(WC.EQ.'PX')          IS=.TRUE.
          IF(WC.EQ.'PCX')         IS=.TRUE.
          IF(WC.EQ.'CAX')         IS=.TRUE.
          IF(WC.EQ.'CLAP')        IS=.TRUE.
          IF(WC.EQ.'COBS')        IS=.TRUE.
          IF(WC.EQ.'GLA')         IS=.TRUE.
          IF(WC.EQ.'GLASS')       IS=.TRUE.
          IF(WC.EQ.'DEFORM')      IS=.TRUE.
          IF(WC.EQ.'DELDEFOR')    IS=.TRUE.
          IF(WC.EQ.'SCHOTT')      IS=.TRUE.
          IF(WC.EQ.'SCH2000')     IS=.TRUE.
          IF(WC.EQ.'MULTCLAP')    IS=.TRUE.
          IF(WC.EQ.'MULTCOBS')    IS=.TRUE.
          IF(WC.EQ.'SPIDER')      IS=.TRUE.
          IF(WC.EQ.'SPGR')        IS=.TRUE.
          IF(WC.EQ.'MODEL')       IS=.TRUE.
          IF(WC.EQ.'RADHARD')     IS=.TRUE.
          IF(WC.EQ.'MATL')        IS=.TRUE.
          IF(WC.EQ.'RUSSIAN')     IS=.TRUE.
          IF(WC.EQ.'USER')        IS=.TRUE.
          IF(WC.EQ.'GLCAT')       IS=.TRUE.
          IF(WC.EQ.'OHARA')       IS=.TRUE.
          IF(WC.EQ.'HOYA')        IS=.TRUE.
          IF(WC.EQ.'HIKARI')      IS=.TRUE.
          IF(WC.EQ.'CHANCE')      IS=.TRUE.
          IF(WC.EQ.'CORNIN')      IS=.TRUE.
          IF(WC.EQ.'AIR')         IS=.TRUE.
          IF(WC.EQ.'COATING ')    IS=.TRUE.
          IF(WC.EQ.'REFL')        IS=.TRUE.
          IF(WC.EQ.'REFLTIRO')    IS=.TRUE.
          IF(WC.EQ.'REFLTIR')     IS=.TRUE.
          IF(WC.EQ.'PERFECT')     IS=.TRUE.
          IF(WC.EQ.'IDEAL')       IS=.TRUE.
          IF(WC.EQ.'EOS')         IS=.TRUE.
          IF(WC.EQ.'TASPH')       IS=.TRUE.
          IF(WC.EQ.'C')           IS=.TRUE.
          IF(WC.EQ.'M')           IS=.TRUE.
          IF(WC.EQ.'CHG')         IS=.TRUE.
          IF(WC.EQ.'CVTOR')       IS=.TRUE.
          IF(WC.EQ.'GRT')         IS=.TRUE.
          IF(WC.EQ.'GRO')         IS=.TRUE.
          IF(WC.EQ.'GRS')         IS=.TRUE.
          IF(WC.EQ.'GRX')         IS=.TRUE.
          IF(WC.EQ.'GRY')         IS=.TRUE.
          IF(WC.EQ.'GRZ')         IS=.TRUE.
          IF(WC.EQ.'RDTOR')       IS=.TRUE.
          IF(WC.EQ.'REAL')        IS=.TRUE.
          IF(WC.EQ.'PARAX')       IS=.TRUE.
          IF(WC.EQ.'FOOTBLOK')    IS=.TRUE.
          IF(WC.EQ.'PIVOT')       IS=.TRUE.
          IF(WC.EQ.'PIVAXIS')     IS=.TRUE.
          IF(WC.EQ.'PIVOTD')      IS=.TRUE.
          IF(WC.EQ.'XD')          IS=.TRUE.
          IF(WC.EQ.'YD')          IS=.TRUE.
          IF(WC.EQ.'ZD')          IS=.TRUE.
          IF(WC.EQ.'PIVY')        IS=.TRUE.
          IF(WC.EQ.'PIVX')        IS=.TRUE.
          IF(WC.EQ.'PIVZ')        IS=.TRUE.
          IF(WC.EQ.'ALPHA')       IS=.TRUE.
          IF(WC.EQ.'GDX     ')    IS=.TRUE.
          IF(WC.EQ.'GDY     ')    IS=.TRUE.
          IF(WC.EQ.'GDZ     ')    IS=.TRUE.
          IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
          IF(WC.EQ.'GBETA   ')    IS=.TRUE.
          IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
          IF(WC.EQ.'TASPHD')      IS=.TRUE.
          IF(WC.EQ.'ARRAYD')      IS=.TRUE.
          IF(WC.EQ.'ARRAY')       IS=.TRUE.
          IF(WC.EQ.'CCTOR')       IS=.TRUE.
          IF(WC.EQ.'ADTOR')       IS=.TRUE.
          IF(WC.EQ.'AETOR')       IS=.TRUE.
          IF(WC.EQ.'AFTOR')       IS=.TRUE.
          IF(WC.EQ.'AGTOR')       IS=.TRUE.
          IF(WC.EQ.'AC')          IS=.TRUE.
          IF(WC.EQ.'AD')          IS=.TRUE.
          IF(WC.EQ.'AE')          IS=.TRUE.
          IF(WC.EQ.'AF')          IS=.TRUE.
          IF(WC.EQ.'AG')          IS=.TRUE.
          IF(WC.EQ.'AH')          IS=.TRUE.
          IF(WC.EQ.'AI')          IS=.TRUE.
          IF(WC.EQ.'AJ')          IS=.TRUE.
          IF(WC.EQ.'AK')          IS=.TRUE.
          IF(WC.EQ.'AL')          IS=.TRUE.
          IF(WC.EQ.'N1')          IS=.TRUE.
          IF(WC.EQ.'N2')          IS=.TRUE.
          IF(WC.EQ.'N3')          IS=.TRUE.
          IF(WC.EQ.'N4')          IS=.TRUE.
          IF(WC.EQ.'N5')          IS=.TRUE.
          IF(WC.EQ.'N6')          IS=.TRUE.
          IF(WC.EQ.'N7')          IS=.TRUE.
          IF(WC.EQ.'N8')          IS=.TRUE.
          IF(WC.EQ.'N9')          IS=.TRUE.
          IF(WC.EQ.'N10')         IS=.TRUE.
          IF(WC.EQ.'INDEX')       IS=.TRUE.
          IF(WC.EQ.'VNUM')        IS=.TRUE.
          IF(WC.EQ.'DPART')       IS=.TRUE.
          IF(WC.EQ.'BETA')        IS=.TRUE.
          IF(WC.EQ.'GAMMA')       IS=.TRUE.
          IF(WC.EQ.'ASPHD')       IS=.TRUE.
          IF(WC.EQ.'TORD')        IS=.TRUE.
          IF(WC.EQ.'TILTD')       IS=.TRUE.
          IF(WC.EQ.'CSD')         IS=.TRUE.
          IF(WC.EQ.'CSDX')        IS=.TRUE.
          IF(WC.EQ.'CSDY')        IS=.TRUE.
          IF(WC.EQ.'TSD')         IS=.TRUE.
          IF(WC.EQ.'GRTD')        IS=.TRUE.
          IF(WC.EQ.'PIKD')        IS=.TRUE.
          IF(WC.EQ.'CLAPD')       IS=.TRUE.
          IF(WC.EQ.'COBSD')       IS=.TRUE.
          IF(WC.EQ.'INS')         IS=.TRUE.
          IF(WC.EQ.'DEL')         IS=.TRUE.
          IF(WC.EQ.'INR')         IS=.TRUE.
          IF(WC.EQ.'INRD')        IS=.TRUE.
          IF(WC.EQ.'ZERO')        IS=.TRUE.
          RETURN
      END
      SUBROUTINE CONTH(WC,WQ,IS)
          IMPLICIT NONE
          CHARACTER WC*8,WQ*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'ARCL')        IS=.TRUE.
          IF(WC.EQ.'STOAX')       IS=.TRUE.
          IF(WC.EQ.'SET')         IS=.TRUE.
          IF(WC.EQ.'W1')          IS=.TRUE.
          IF(WC.EQ.'W2')          IS=.TRUE.
          IF(WC.EQ.'W3')          IS=.TRUE.
          IF(WC.EQ.'W4')          IS=.TRUE.
          IF(WC.EQ.'W5')          IS=.TRUE.
          IF(WC.EQ.'EOS')                         IS=.TRUE.
          IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'ON')        IS=.TRUE.
          IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'OFF')       IS=.TRUE.
          IF(WC.EQ.'C')                           IS=.TRUE.
          IF(WC.EQ.'M')                           IS=.TRUE.
          IF(WC.EQ.'SPDEL')                       IS=.TRUE.
          IF(WC.EQ.'SPECIAL')                     IS=.TRUE.
          IF(WC.EQ.'GENL')                        IS=.TRUE.
          IF(WC.EQ.'C1')                          IS=.TRUE.
          IF(WC.EQ.'C2')                          IS=.TRUE.
          IF(WC.EQ.'C3')                          IS=.TRUE.
          IF(WC.EQ.'C4')                          IS=.TRUE.
          IF(WC.EQ.'C5')                          IS=.TRUE.
          IF(WC.EQ.'C6')                          IS=.TRUE.
          IF(WC.EQ.'C7')                          IS=.TRUE.
          IF(WC.EQ.'C8')                          IS=.TRUE.
          IF(WC.EQ.'C9')                          IS=.TRUE.
          IF(WC.EQ.'C10')                         IS=.TRUE.
          IF(WC.EQ.'C11')                         IS=.TRUE.
          IF(WC.EQ.'C12')                         IS=.TRUE.
          IF(WC.EQ.'C13')                         IS=.TRUE.
          IF(WC.EQ.'C14')                         IS=.TRUE.
          IF(WC.EQ.'C15')                         IS=.TRUE.
          IF(WC.EQ.'C16')                         IS=.TRUE.
          IF(WC.EQ.'C17')                         IS=.TRUE.
          IF(WC.EQ.'C18')                         IS=.TRUE.
          IF(WC.EQ.'C19')                         IS=.TRUE.
          IF(WC.EQ.'C20')                         IS=.TRUE.
          IF(WC.EQ.'C21')                         IS=.TRUE.
          IF(WC.EQ.'C22')                         IS=.TRUE.
          IF(WC.EQ.'C23')                         IS=.TRUE.
          IF(WC.EQ.'C24')                         IS=.TRUE.
          IF(WC.EQ.'C25')                         IS=.TRUE.
          IF(WC.EQ.'C26')                         IS=.TRUE.
          IF(WC.EQ.'C27')                         IS=.TRUE.
          IF(WC.EQ.'C28')                         IS=.TRUE.
          IF(WC.EQ.'C29')                         IS=.TRUE.
          IF(WC.EQ.'C30')                         IS=.TRUE.
          IF(WC.EQ.'C31')                         IS=.TRUE.
          IF(WC.EQ.'C32')                         IS=.TRUE.
          IF(WC.EQ.'C33')                         IS=.TRUE.
          IF(WC.EQ.'C34')                         IS=.TRUE.
          IF(WC.EQ.'C35')                         IS=.TRUE.
          IF(WC.EQ.'C36')                         IS=.TRUE.
          IF(WC.EQ.'C37')                         IS=.TRUE.
          IF(WC.EQ.'C38')                         IS=.TRUE.
          IF(WC.EQ.'C39')                         IS=.TRUE.
          IF(WC.EQ.'C40')                         IS=.TRUE.
          IF(WC.EQ.'C41')                         IS=.TRUE.
          IF(WC.EQ.'C42')                         IS=.TRUE.
          IF(WC.EQ.'C43')                         IS=.TRUE.
          IF(WC.EQ.'C44')                         IS=.TRUE.
          IF(WC.EQ.'C45')                         IS=.TRUE.
          IF(WC.EQ.'C46')                         IS=.TRUE.
          IF(WC.EQ.'C47')                         IS=.TRUE.
          IF(WC.EQ.'C48')                         IS=.TRUE.
          IF(WC.EQ.'C49')                         IS=.TRUE.
          IF(WC.EQ.'C50')                         IS=.TRUE.
          IF(WC.EQ.'C51')                         IS=.TRUE.
          IF(WC.EQ.'C52')                         IS=.TRUE.
          IF(WC.EQ.'C53')                         IS=.TRUE.
          IF(WC.EQ.'C54')                         IS=.TRUE.
          IF(WC.EQ.'C55')                         IS=.TRUE.
          IF(WC.EQ.'C56')                         IS=.TRUE.
          IF(WC.EQ.'C57')                         IS=.TRUE.
          IF(WC.EQ.'C58')                         IS=.TRUE.
          IF(WC.EQ.'C59')                         IS=.TRUE.
          IF(WC.EQ.'C60')                         IS=.TRUE.
          IF(WC.EQ.'C61')                         IS=.TRUE.
          IF(WC.EQ.'C62')                         IS=.TRUE.
          IF(WC.EQ.'C63')                         IS=.TRUE.
          IF(WC.EQ.'C64')                         IS=.TRUE.
          IF(WC.EQ.'C65')                         IS=.TRUE.
          IF(WC.EQ.'C66')                         IS=.TRUE.
          IF(WC.EQ.'C67')                         IS=.TRUE.
          IF(WC.EQ.'C68')                         IS=.TRUE.
          IF(WC.EQ.'C69')                         IS=.TRUE.
          IF(WC.EQ.'C70')                         IS=.TRUE.
          IF(WC.EQ.'C71')                         IS=.TRUE.
          IF(WC.EQ.'C72')                         IS=.TRUE.
          IF(WC.EQ.'C73')                         IS=.TRUE.
          IF(WC.EQ.'C74')                         IS=.TRUE.
          IF(WC.EQ.'C75')                         IS=.TRUE.
          IF(WC.EQ.'C76')                         IS=.TRUE.
          IF(WC.EQ.'C77')                         IS=.TRUE.
          IF(WC.EQ.'C78')                         IS=.TRUE.
          IF(WC.EQ.'C79')                         IS=.TRUE.
          IF(WC.EQ.'C80')                         IS=.TRUE.
          IF(WC.EQ.'C81')                         IS=.TRUE.
          IF(WC.EQ.'C82')                         IS=.TRUE.
          IF(WC.EQ.'C83')                         IS=.TRUE.
          IF(WC.EQ.'C84')                         IS=.TRUE.
          IF(WC.EQ.'C85')                         IS=.TRUE.
          IF(WC.EQ.'C86')                         IS=.TRUE.
          IF(WC.EQ.'C87')                         IS=.TRUE.
          IF(WC.EQ.'C88')                         IS=.TRUE.
          IF(WC.EQ.'C89')                         IS=.TRUE.
          IF(WC.EQ.'C90')                         IS=.TRUE.
          IF(WC.EQ.'C91')                         IS=.TRUE.
          IF(WC.EQ.'C92')                         IS=.TRUE.
          IF(WC.EQ.'C93')                         IS=.TRUE.
          IF(WC.EQ.'C94')                         IS=.TRUE.
          IF(WC.EQ.'C95')                         IS=.TRUE.
          IF(WC.EQ.'C96')                         IS=.TRUE.
          RETURN
      END
      SUBROUTINE CONTI(WC,WQ,IS)
          IMPLICIT NONE
          CHARACTER WC*8,WQ*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'ARCL')        IS=.TRUE.
          IF(WC.EQ.'STOAX')       IS=.TRUE.
          IF(WC.EQ.'SET')         IS=.TRUE.
          IF(WC.EQ.'W1')          IS=.TRUE.
          IF(WC.EQ.'W2')          IS=.TRUE.
          IF(WC.EQ.'W3')          IS=.TRUE.
          IF(WC.EQ.'W4')          IS=.TRUE.
          IF(WC.EQ.'W5')          IS=.TRUE.
          IF(WC.EQ.'EOS')                         IS=.TRUE.
          IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'ON')        IS=.TRUE.
          IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'OFF')       IS=.TRUE.
          IF(WC.EQ.'C')                           IS=.TRUE.
          IF(WC.EQ.'M')                           IS=.TRUE.
          IF(WC.EQ.'SPDEL')                       IS=.TRUE.
          IF(WC.EQ.'SPECIAL')                     IS=.TRUE.
          IF(WC.EQ.'GENL')                        IS=.TRUE.
          IF(WC.EQ.'C1')                          IS=.TRUE.
          IF(WC.EQ.'C2')                          IS=.TRUE.
          IF(WC.EQ.'C3')                          IS=.TRUE.
          IF(WC.EQ.'C4')                          IS=.TRUE.
          IF(WC.EQ.'C5')                          IS=.TRUE.
          IF(WC.EQ.'C6')                          IS=.TRUE.
          IF(WC.EQ.'C7')                          IS=.TRUE.
          IF(WC.EQ.'C8')                          IS=.TRUE.
          IF(WC.EQ.'C9')                          IS=.TRUE.
          IF(WC.EQ.'C10')                         IS=.TRUE.
          IF(WC.EQ.'C11')                         IS=.TRUE.
          IF(WC.EQ.'C12')                         IS=.TRUE.
          IF(WC.EQ.'C13')                         IS=.TRUE.
          IF(WC.EQ.'C14')                         IS=.TRUE.
          IF(WC.EQ.'C15')                         IS=.TRUE.
          IF(WC.EQ.'C16')                         IS=.TRUE.
          IF(WC.EQ.'C17')                         IS=.TRUE.
          IF(WC.EQ.'C18')                         IS=.TRUE.
          IF(WC.EQ.'C19')                         IS=.TRUE.
          IF(WC.EQ.'C20')                         IS=.TRUE.
          IF(WC.EQ.'C21')                         IS=.TRUE.
          IF(WC.EQ.'C22')                         IS=.TRUE.
          IF(WC.EQ.'C23')                         IS=.TRUE.
          IF(WC.EQ.'C24')                         IS=.TRUE.
          IF(WC.EQ.'C25')                         IS=.TRUE.
          IF(WC.EQ.'C26')                         IS=.TRUE.
          IF(WC.EQ.'C27')                         IS=.TRUE.
          IF(WC.EQ.'C28')                         IS=.TRUE.
          IF(WC.EQ.'C29')                         IS=.TRUE.
          IF(WC.EQ.'C30')                         IS=.TRUE.
          IF(WC.EQ.'C31')                         IS=.TRUE.
          IF(WC.EQ.'C32')                         IS=.TRUE.
          IF(WC.EQ.'C33')                         IS=.TRUE.
          IF(WC.EQ.'C34')                         IS=.TRUE.
          IF(WC.EQ.'C35')                         IS=.TRUE.
          IF(WC.EQ.'C36')                         IS=.TRUE.
          IF(WC.EQ.'C37')                         IS=.TRUE.
          IF(WC.EQ.'C38')                         IS=.TRUE.
          IF(WC.EQ.'C39')                         IS=.TRUE.
          IF(WC.EQ.'C40')                         IS=.TRUE.
          IF(WC.EQ.'C41')                         IS=.TRUE.
          IF(WC.EQ.'C42')                         IS=.TRUE.
          IF(WC.EQ.'C43')                         IS=.TRUE.
          IF(WC.EQ.'C44')                         IS=.TRUE.
          IF(WC.EQ.'C45')                         IS=.TRUE.
          IF(WC.EQ.'C46')                         IS=.TRUE.
          IF(WC.EQ.'C47')                         IS=.TRUE.
          IF(WC.EQ.'C48')                         IS=.TRUE.
          IF(WC.EQ.'C49')                         IS=.TRUE.
          IF(WC.EQ.'C50')                         IS=.TRUE.
          IF(WC.EQ.'C51')                         IS=.TRUE.
          IF(WC.EQ.'C52')                         IS=.TRUE.
          IF(WC.EQ.'C53')                         IS=.TRUE.
          IF(WC.EQ.'C54')                         IS=.TRUE.
          IF(WC.EQ.'C55')                         IS=.TRUE.
          IF(WC.EQ.'C56')                         IS=.TRUE.
          IF(WC.EQ.'C57')                         IS=.TRUE.
          IF(WC.EQ.'C58')                         IS=.TRUE.
          IF(WC.EQ.'C59')                         IS=.TRUE.
          IF(WC.EQ.'C60')                         IS=.TRUE.
          IF(WC.EQ.'C61')                         IS=.TRUE.
          IF(WC.EQ.'C62')                         IS=.TRUE.
          IF(WC.EQ.'C63')                         IS=.TRUE.
          IF(WC.EQ.'C64')                         IS=.TRUE.
          IF(WC.EQ.'C65')                         IS=.TRUE.
          IF(WC.EQ.'C66')                         IS=.TRUE.
          IF(WC.EQ.'C67')                         IS=.TRUE.
          IF(WC.EQ.'C68')                         IS=.TRUE.
          IF(WC.EQ.'C69')                         IS=.TRUE.
          IF(WC.EQ.'C70')                         IS=.TRUE.
          IF(WC.EQ.'C71')                         IS=.TRUE.
          IF(WC.EQ.'C72')                         IS=.TRUE.
          IF(WC.EQ.'C73')                         IS=.TRUE.
          IF(WC.EQ.'C74')                         IS=.TRUE.
          IF(WC.EQ.'C75')                         IS=.TRUE.
          IF(WC.EQ.'C76')                         IS=.TRUE.
          IF(WC.EQ.'C77')                         IS=.TRUE.
          IF(WC.EQ.'C78')                         IS=.TRUE.
          IF(WC.EQ.'C79')                         IS=.TRUE.
          IF(WC.EQ.'C80')                         IS=.TRUE.
          IF(WC.EQ.'C81')                         IS=.TRUE.
          IF(WC.EQ.'C82')                         IS=.TRUE.
          IF(WC.EQ.'C83')                         IS=.TRUE.
          IF(WC.EQ.'C84')                         IS=.TRUE.
          IF(WC.EQ.'C85')                         IS=.TRUE.
          IF(WC.EQ.'C86')                         IS=.TRUE.
          IF(WC.EQ.'C87')                         IS=.TRUE.
          IF(WC.EQ.'C88')                         IS=.TRUE.
          IF(WC.EQ.'C89')                         IS=.TRUE.
          IF(WC.EQ.'C90')                         IS=.TRUE.
          IF(WC.EQ.'C91')                         IS=.TRUE.
          IF(WC.EQ.'C92')                         IS=.TRUE.
          IF(WC.EQ.'C93')                         IS=.TRUE.
          IF(WC.EQ.'C94')                         IS=.TRUE.
          IF(WC.EQ.'C95')                         IS=.TRUE.
          IF(WC.EQ.'C96')                         IS=.TRUE.
          RETURN
      END


      SUBROUTINE CONTJ(WC,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'ARCL')        IS=.TRUE.
          IF(WC.EQ.'STOAX')       IS=.TRUE.
          IF(WC.EQ.'SET')         IS=.TRUE.
          IF(WC.EQ.'W1')          IS=.TRUE.
          IF(WC.EQ.'W2')          IS=.TRUE.
          IF(WC.EQ.'W3')          IS=.TRUE.
          IF(WC.EQ.'W4')          IS=.TRUE.
          IF(WC.EQ.'W5')          IS=.TRUE.
          IF(WC.EQ.'EOS')         IS=.TRUE.
          IF(WC.EQ.'C')           IS=.TRUE.
          IF(WC.EQ.'M')           IS=.TRUE.
          IF(WC.EQ.'SURF')        IS=.TRUE.
          IF(WC.EQ.'COEF')        IS=.TRUE.
          IF(WC.EQ.'DATA')        IS=.TRUE.
          IF(WC.EQ.'FIT')         IS=.TRUE.
          IF(WC.EQ.'FITGLASS')    IS=.TRUE.
          IF(WC.EQ.'GDATA')       IS=.TRUE.
          IF(WC.EQ.'LIST')        IS=.TRUE.
          IF(WC.EQ.'COEFS')       IS=.TRUE.
          IF(WC.EQ.'LISTCOEF')    IS=.TRUE.
          IF(WC.EQ.'EVAL')        IS=.TRUE.
          IF(WC.EQ.'FILE')        IS=.TRUE.
          IF(WC.EQ.'FETCH')       IS=.TRUE.
          IF(WC.EQ.'TYPE')        IS=.TRUE.
          IF(WC.EQ.'READ')        IS=.TRUE.
          RETURN
      END


      SUBROUTINE CONTK(WC,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'SET     ')       IS=.TRUE.
          IF(WC.EQ.'W1      ')       IS=.TRUE.
          IF(WC.EQ.'W2      ')       IS=.TRUE.
          IF(WC.EQ.'W3      ')       IS=.TRUE.
          IF(WC.EQ.'W4      ')       IS=.TRUE.
          IF(WC.EQ.'W5      ')       IS=.TRUE.
          IF(WC.EQ.'EOS     ')       IS=.TRUE.
          IF(WC.EQ.'MR      ')       IS=.TRUE.
          IF(WC.EQ.'MRA     ')       IS=.TRUE.
          IF(WC.EQ.'OP      ')       IS=.TRUE.
          IF(WC.EQ.'OPA     ')       IS=.TRUE.
          IF(WC.EQ.'CFG     ')       IS=.TRUE.
          IF(WC.EQ.'C       ')       IS=.TRUE.
          IF(WC.EQ.'M       ')       IS=.TRUE.
          IF(WC.EQ.'DEL     ')       IS=.TRUE.
          IF(WC.EQ.'FUNC01  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC02  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC03  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC04  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC05  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC06  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC07  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC08  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC09  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC10  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC00  ')       IS=.TRUE.
          IF(WC.EQ.'COR     ')       IS=.TRUE.
          IF(WC.EQ.'BYP     ')       IS=.TRUE.
          IF(WC.EQ.'BLO     ')       IS=.TRUE.
          IF(WC.EQ.'BHI     ')       IS=.TRUE.
          IF(WC.EQ.'HLD     ')       IS=.TRUE.
          IF(WC.EQ.'GTE     ')       IS=.TRUE.
          IF(WC.EQ.'LTE     ')       IS=.TRUE.
          IF(WC.EQ.'OP_DESC ')       IS=.TRUE.
          RETURN
      END


      SUBROUTINE CONTL(WC,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'SET     ')       IS=.TRUE.
          IF(WC.EQ.'W1      ')       IS=.TRUE.
          IF(WC.EQ.'W2      ')       IS=.TRUE.
          IF(WC.EQ.'W3      ')       IS=.TRUE.
          IF(WC.EQ.'W4      ')       IS=.TRUE.
          IF(WC.EQ.'W5      ')       IS=.TRUE.
          IF(WC.EQ.'EOS     ')       IS=.TRUE.
          IF(WC.EQ.'TOPS    ')       IS=.TRUE.
          IF(WC.EQ.'C       ')       IS=.TRUE.
          IF(WC.EQ.'M       ')       IS=.TRUE.
          IF(WC.EQ.'DEL     ')       IS=.TRUE.
          IF(WC.EQ.'FUNC01  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC02  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC03  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC04  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC05  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC06  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC07  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC08  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC09  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC10  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC00  ')       IS=.TRUE.
          IF(WC.EQ.'OP_DESC ')       IS=.TRUE.
          RETURN
      END


      SUBROUTINE CONTM(WC,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'SET     ')       IS=.TRUE.
          IF(WC.EQ.'W1      ')       IS=.TRUE.
          IF(WC.EQ.'W2      ')       IS=.TRUE.
          IF(WC.EQ.'W3      ')       IS=.TRUE.
          IF(WC.EQ.'W4      ')       IS=.TRUE.
          IF(WC.EQ.'W5      ')       IS=.TRUE.
          IF(WC.EQ.'EOS     ')       IS=.TRUE.
          IF(WC.EQ.'CRITS   ')       IS=.TRUE.
          IF(WC.EQ.'C       ')       IS=.TRUE.
          IF(WC.EQ.'M       ')       IS=.TRUE.
          IF(WC.EQ.'DEL     ')       IS=.TRUE.
          IF(WC.EQ.'FUNC01  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC02  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC03  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC04  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC05  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC06  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC07  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC08  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC09  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC10  ')       IS=.TRUE.
          IF(WC.EQ.'FUNC00  ')       IS=.TRUE.
          IF(WC.EQ.'OP_DESC ')       IS=.TRUE.
          RETURN
      END


      SUBROUTINE CONTN(WC,F29,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          INTEGER F29
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'ARCL')        IS=.TRUE.
          IF(WC.EQ.'STOAX')       IS=.TRUE.
          IF(WC.EQ.'SET')         IS=.TRUE.
          IF(WC.EQ.'W1')          IS=.TRUE.
          IF(WC.EQ.'W2')          IS=.TRUE.
          IF(WC.EQ.'W3')          IS=.TRUE.
          IF(WC.EQ.'W4')          IS=.TRUE.
          IF(WC.EQ.'W5')          IS=.TRUE.
          IF(WC.EQ.'EOS')         IS=.TRUE.
          IF(WC.EQ.'VB')          IS=.TRUE.
          IF(WC.EQ.'VBA')         IS=.TRUE.
          IF(WC.EQ.'C')           IS=.TRUE.
          IF(WC.EQ.'M')           IS=.TRUE.
          IF(WC.EQ.'CFG')         IS=.TRUE.
          IF(WC.EQ.'DEL'.AND.F29.EQ.2)            IS=.TRUE.
          RETURN
      END


      SUBROUTINE CONTO(WC,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'MACVAR')      IS=.TRUE.
          IF(WC.EQ.'RD')          IS=.TRUE.
          IF(WC.EQ.'CV')          IS=.TRUE.
          IF(WC.EQ.'TH')          IS=.TRUE.
          IF(WC.EQ.'CC')          IS=.TRUE.
          IF(WC.EQ.'AC')          IS=.TRUE.
          IF(WC.EQ.'AD')          IS=.TRUE.
          IF(WC.EQ.'AE')          IS=.TRUE.
          IF(WC.EQ.'AF')          IS=.TRUE.
          IF(WC.EQ.'AG')          IS=.TRUE.
          IF(WC.EQ.'AH')          IS=.TRUE.
          IF(WC.EQ.'AI')          IS=.TRUE.
          IF(WC.EQ.'AJ')          IS=.TRUE.
          IF(WC.EQ.'AK')          IS=.TRUE.
          IF(WC.EQ.'AL')          IS=.TRUE.
          IF(WC.EQ.'RDTOR')       IS=.TRUE.
          IF(WC.EQ.'FOOTBLOK')    IS=.TRUE.
          IF(WC.EQ.'PIVOT')       IS=.TRUE.
          IF(WC.EQ.'PIVAXIS')     IS=.TRUE.
          IF(WC.EQ.'PIVOTD')      IS=.TRUE.
          IF(WC.EQ.'CVTOR')       IS=.TRUE.
          IF(WC.EQ.'GRS')         IS=.TRUE.
          IF(WC.EQ.'CCTOR')       IS=.TRUE.
          IF(WC.EQ.'ADTOR')       IS=.TRUE.
          IF(WC.EQ.'AETOR')       IS=.TRUE.
          IF(WC.EQ.'AFTOR')       IS=.TRUE.
          IF(WC.EQ.'AGTOR')       IS=.TRUE.
          IF(WC.EQ.'ALPHA')       IS=.TRUE.
          IF(WC.EQ.'BETA')        IS=.TRUE.
          IF(WC.EQ.'GAMMA')       IS=.TRUE.
          IF(WC.EQ.'GDX     ')    IS=.TRUE.
          IF(WC.EQ.'GDY     ')    IS=.TRUE.
          IF(WC.EQ.'GDZ     ')    IS=.TRUE.
          IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
          IF(WC.EQ.'GBETA   ')    IS=.TRUE.
          IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
          IF(WC.EQ.'XD')          IS=.TRUE.
          IF(WC.EQ.'YD')          IS=.TRUE.
          IF(WC.EQ.'ZD')          IS=.TRUE.
          IF(WC.EQ.'PIVY')        IS=.TRUE.
          IF(WC.EQ.'PIVX')        IS=.TRUE.
          IF(WC.EQ.'PIVZ')        IS=.TRUE.
          IF(WC.EQ.'N1')          IS=.TRUE.
          IF(WC.EQ.'N2')          IS=.TRUE.
          IF(WC.EQ.'N3')          IS=.TRUE.
          IF(WC.EQ.'N4')          IS=.TRUE.
          IF(WC.EQ.'N5')          IS=.TRUE.
          IF(WC.EQ.'N6')          IS=.TRUE.
          IF(WC.EQ.'N7')          IS=.TRUE.
          IF(WC.EQ.'N8')          IS=.TRUE.
          IF(WC.EQ.'N9')          IS=.TRUE.
          IF(WC.EQ.'N10')         IS=.TRUE.
          IF(WC.EQ.'INDEX')       IS=.TRUE.
          IF(WC.EQ.'VNUM')        IS=.TRUE.
          IF(WC.EQ.'DPART')       IS=.TRUE.
          IF(WC.EQ.'CLPX')        IS=.TRUE.
          IF(WC.EQ.'CLPY')        IS=.TRUE.
          IF(WC.EQ.'C1')          IS=.TRUE.
          IF(WC.EQ.'C2')          IS=.TRUE.
          IF(WC.EQ.'C3')          IS=.TRUE.
          IF(WC.EQ.'C4')          IS=.TRUE.
          IF(WC.EQ.'C5')          IS=.TRUE.
          IF(WC.EQ.'C6')          IS=.TRUE.
          IF(WC.EQ.'C7')          IS=.TRUE.
          IF(WC.EQ.'C8')          IS=.TRUE.
          IF(WC.EQ.'C9')          IS=.TRUE.
          IF(WC.EQ.'C10')         IS=.TRUE.
          IF(WC.EQ.'C11')         IS=.TRUE.
          IF(WC.EQ.'C12')         IS=.TRUE.
          IF(WC.EQ.'C13')         IS=.TRUE.
          IF(WC.EQ.'C14')         IS=.TRUE.
          IF(WC.EQ.'C15')         IS=.TRUE.
          IF(WC.EQ.'C16')         IS=.TRUE.
          IF(WC.EQ.'C17')         IS=.TRUE.
          IF(WC.EQ.'C18')         IS=.TRUE.
          IF(WC.EQ.'C19')         IS=.TRUE.
          IF(WC.EQ.'C20')         IS=.TRUE.
          IF(WC.EQ.'C21')         IS=.TRUE.
          IF(WC.EQ.'C22')         IS=.TRUE.
          IF(WC.EQ.'C23')         IS=.TRUE.
          IF(WC.EQ.'C24')         IS=.TRUE.
          IF(WC.EQ.'C25')         IS=.TRUE.
          IF(WC.EQ.'C26')         IS=.TRUE.
          IF(WC.EQ.'C27')         IS=.TRUE.
          IF(WC.EQ.'C28')         IS=.TRUE.
          IF(WC.EQ.'C29')         IS=.TRUE.
          IF(WC.EQ.'C30')         IS=.TRUE.
          IF(WC.EQ.'C31')         IS=.TRUE.
          IF(WC.EQ.'C32')         IS=.TRUE.
          IF(WC.EQ.'C33')         IS=.TRUE.
          IF(WC.EQ.'C34')         IS=.TRUE.
          IF(WC.EQ.'C35')         IS=.TRUE.
          IF(WC.EQ.'C36')         IS=.TRUE.
          IF(WC.EQ.'C37')         IS=.TRUE.
          IF(WC.EQ.'C38')         IS=.TRUE.
          IF(WC.EQ.'C39')         IS=.TRUE.
          IF(WC.EQ.'C40')         IS=.TRUE.
          IF(WC.EQ.'C41')         IS=.TRUE.
          IF(WC.EQ.'C42')         IS=.TRUE.
          IF(WC.EQ.'C43')         IS=.TRUE.
          IF(WC.EQ.'C44')         IS=.TRUE.
          IF(WC.EQ.'C45')         IS=.TRUE.
          IF(WC.EQ.'C46')         IS=.TRUE.
          IF(WC.EQ.'C47')         IS=.TRUE.
          IF(WC.EQ.'C48')         IS=.TRUE.
          IF(WC.EQ.'C49')                         IS=.TRUE.
          IF(WC.EQ.'C50')                         IS=.TRUE.
          IF(WC.EQ.'C51')                         IS=.TRUE.
          IF(WC.EQ.'C52')                         IS=.TRUE.
          IF(WC.EQ.'C53')                         IS=.TRUE.
          IF(WC.EQ.'C54')                         IS=.TRUE.
          IF(WC.EQ.'C55')                         IS=.TRUE.
          IF(WC.EQ.'C56')                         IS=.TRUE.
          IF(WC.EQ.'C57')                         IS=.TRUE.
          IF(WC.EQ.'C58')                         IS=.TRUE.
          IF(WC.EQ.'C59')                         IS=.TRUE.
          IF(WC.EQ.'C60')                         IS=.TRUE.
          IF(WC.EQ.'C61')                         IS=.TRUE.
          IF(WC.EQ.'C62')                         IS=.TRUE.
          IF(WC.EQ.'C63')                         IS=.TRUE.
          IF(WC.EQ.'C64')                         IS=.TRUE.
          IF(WC.EQ.'C65')                         IS=.TRUE.
          IF(WC.EQ.'C66')                         IS=.TRUE.
          IF(WC.EQ.'C67')                         IS=.TRUE.
          IF(WC.EQ.'C68')                         IS=.TRUE.
          IF(WC.EQ.'C69')                         IS=.TRUE.
          IF(WC.EQ.'C70')                         IS=.TRUE.
          IF(WC.EQ.'C71')                         IS=.TRUE.
          IF(WC.EQ.'C72')                         IS=.TRUE.
          IF(WC.EQ.'C73')                         IS=.TRUE.
          IF(WC.EQ.'C74')                         IS=.TRUE.
          IF(WC.EQ.'C75')                         IS=.TRUE.
          IF(WC.EQ.'C76')                         IS=.TRUE.
          IF(WC.EQ.'C77')                         IS=.TRUE.
          IF(WC.EQ.'C78')                         IS=.TRUE.
          IF(WC.EQ.'C79')                         IS=.TRUE.
          IF(WC.EQ.'C80')                         IS=.TRUE.
          IF(WC.EQ.'C81')                         IS=.TRUE.
          IF(WC.EQ.'C82')                         IS=.TRUE.
          IF(WC.EQ.'C83')                         IS=.TRUE.
          IF(WC.EQ.'C84')                         IS=.TRUE.
          IF(WC.EQ.'C85')                         IS=.TRUE.
          IF(WC.EQ.'C86')                         IS=.TRUE.
          IF(WC.EQ.'C87')                         IS=.TRUE.
          IF(WC.EQ.'C88')                         IS=.TRUE.
          IF(WC.EQ.'C89')                         IS=.TRUE.
          IF(WC.EQ.'C90')                         IS=.TRUE.
          IF(WC.EQ.'C91')                         IS=.TRUE.
          IF(WC.EQ.'C92')                         IS=.TRUE.
          IF(WC.EQ.'C93')                         IS=.TRUE.
          IF(WC.EQ.'C94')                         IS=.TRUE.
          IF(WC.EQ.'C95')                         IS=.TRUE.
          IF(WC.EQ.'C96')                         IS=.TRUE.
          IF(WC(1:3).EQ.'ACT')                    IS=.TRUE.
          IF(WC(1:3).EQ.'PAR')                     IS=.TRUE.
          IF(WC.EQ.'V1      ')                    IS=.TRUE.
          IF(WC.EQ.'V2      ')                    IS=.TRUE.
          IF(WC.EQ.'V3      ')                    IS=.TRUE.
          IF(WC.EQ.'V4      ')                    IS=.TRUE.
          IF(WC.EQ.'V5      ')                    IS=.TRUE.
          IF(WC.EQ.'NSSXPOS ')                    IS=.TRUE.
          IF(WC.EQ.'NSSYPOS ')                    IS=.TRUE.
          IF(WC.EQ.'NSSZPOS ')                    IS=.TRUE.
          IF(WC.EQ.'NSSALPH ')                    IS=.TRUE.
          IF(WC.EQ.'NSSBETA ')                    IS=.TRUE.
          IF(WC.EQ.'NSSGAMM ')                    IS=.TRUE.
          RETURN
      END


      SUBROUTINE CONTP(WC,F51,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          INTEGER F51
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'ARCL')        IS=.TRUE.
          IF(WC.EQ.'STOAX')       IS=.TRUE.
          IF(WC.EQ.'SET')         IS=.TRUE.
          IF(WC.EQ.'W1')          IS=.TRUE.
          IF(WC.EQ.'W2')          IS=.TRUE.
          IF(WC.EQ.'W3')          IS=.TRUE.
          IF(WC.EQ.'W4')          IS=.TRUE.
          IF(WC.EQ.'W5')          IS=.TRUE.
          IF(WC.EQ.'EOS')         IS=.TRUE.
          IF(WC.EQ.'TVB')         IS=.TRUE.
          IF(WC.EQ.'C')           IS=.TRUE.
          IF(WC.EQ.'M')           IS=.TRUE.
          IF(WC.EQ.'RD')          IS=.TRUE.
          IF(WC.EQ.'CV')          IS=.TRUE.
          IF(WC.EQ.'TH')          IS=.TRUE.
          IF(WC.EQ.'CC')          IS=.TRUE.
          IF(WC.EQ.'AC')          IS=.TRUE.
          IF(WC.EQ.'AD')          IS=.TRUE.
          IF(WC.EQ.'AE')          IS=.TRUE.
          IF(WC.EQ.'AF')          IS=.TRUE.
          IF(WC.EQ.'AG')          IS=.TRUE.
          IF(WC.EQ.'AH')          IS=.TRUE.
          IF(WC.EQ.'AI')          IS=.TRUE.
          IF(WC.EQ.'AJ')          IS=.TRUE.
          IF(WC.EQ.'AK')          IS=.TRUE.
          IF(WC.EQ.'AL')          IS=.TRUE.
          IF(WC.EQ.'RDTOR')       IS=.TRUE.
          IF(WC.EQ.'CVTOR')       IS=.TRUE.
          IF(WC.EQ.'GRS')         IS=.TRUE.
          IF(WC.EQ.'CCTOR')       IS=.TRUE.
          IF(WC.EQ.'ADTOR')       IS=.TRUE.
          IF(WC.EQ.'AETOR')       IS=.TRUE.
          IF(WC.EQ.'AFTOR')       IS=.TRUE.
          IF(WC.EQ.'AGTOR')       IS=.TRUE.
          IF(WC.EQ.'ALPHA')       IS=.TRUE.
          IF(WC.EQ.'BETA')        IS=.TRUE.
          IF(WC.EQ.'GAMMA')       IS=.TRUE.
          IF(WC.EQ.'GDX     ')    IS=.TRUE.
          IF(WC.EQ.'GDY     ')    IS=.TRUE.
          IF(WC.EQ.'GDZ     ')    IS=.TRUE.
          IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
          IF(WC.EQ.'GBETA   ')    IS=.TRUE.
          IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
          IF(WC.EQ.'XD')          IS=.TRUE.
          IF(WC.EQ.'YD')          IS=.TRUE.
          IF(WC.EQ.'ZD')          IS=.TRUE.
          IF(WC.EQ.'PIVY')        IS=.TRUE.
          IF(WC.EQ.'PIVX')        IS=.TRUE.
          IF(WC.EQ.'PIVZ')        IS=.TRUE.
          IF(WC.EQ.'N1')          IS=.TRUE.
          IF(WC.EQ.'N2')          IS=.TRUE.
          IF(WC.EQ.'N3')          IS=.TRUE.
          IF(WC.EQ.'N4')          IS=.TRUE.
          IF(WC.EQ.'N5')          IS=.TRUE.
          IF(WC.EQ.'N6')          IS=.TRUE.
          IF(WC.EQ.'N7')          IS=.TRUE.
          IF(WC.EQ.'N8')          IS=.TRUE.
          IF(WC.EQ.'N9')          IS=.TRUE.
          IF(WC.EQ.'N10')         IS=.TRUE.
          IF(WC.EQ.'INDEX')       IS=.TRUE.
          IF(WC.EQ.'VNUM')        IS=.TRUE.
          IF(WC.EQ.'DPART')       IS=.TRUE.
          IF(WC.EQ.'CLPX')        IS=.TRUE.
          IF(WC.EQ.'CLPY')        IS=.TRUE.
          IF(WC.EQ.'C1')          IS=.TRUE.
          IF(WC.EQ.'C2')          IS=.TRUE.
          IF(WC.EQ.'C3')          IS=.TRUE.
          IF(WC.EQ.'C4')          IS=.TRUE.
          IF(WC.EQ.'C5')          IS=.TRUE.
          IF(WC.EQ.'C6')          IS=.TRUE.
          IF(WC.EQ.'C7')          IS=.TRUE.
          IF(WC.EQ.'C8')          IS=.TRUE.
          IF(WC.EQ.'C9')          IS=.TRUE.
          IF(WC.EQ.'C10')         IS=.TRUE.
          IF(WC.EQ.'C11')         IS=.TRUE.
          IF(WC.EQ.'C12')         IS=.TRUE.
          IF(WC.EQ.'C13')         IS=.TRUE.
          IF(WC.EQ.'C14')         IS=.TRUE.
          IF(WC.EQ.'C15')         IS=.TRUE.
          IF(WC.EQ.'C16')         IS=.TRUE.
          IF(WC.EQ.'C17')         IS=.TRUE.
          IF(WC.EQ.'C18')         IS=.TRUE.
          IF(WC.EQ.'C19')         IS=.TRUE.
          IF(WC.EQ.'C20')         IS=.TRUE.
          IF(WC.EQ.'C21')         IS=.TRUE.
          IF(WC.EQ.'C22')         IS=.TRUE.
          IF(WC.EQ.'C23')         IS=.TRUE.
          IF(WC.EQ.'C24')         IS=.TRUE.
          IF(WC.EQ.'C25')         IS=.TRUE.
          IF(WC.EQ.'C26')         IS=.TRUE.
          IF(WC.EQ.'C27')         IS=.TRUE.
          IF(WC.EQ.'C28')         IS=.TRUE.
          IF(WC.EQ.'C29')         IS=.TRUE.
          IF(WC.EQ.'C30')         IS=.TRUE.
          IF(WC.EQ.'C31')         IS=.TRUE.
          IF(WC.EQ.'C32')         IS=.TRUE.
          IF(WC.EQ.'C33')         IS=.TRUE.
          IF(WC.EQ.'C34')         IS=.TRUE.
          IF(WC.EQ.'C35')         IS=.TRUE.
          IF(WC.EQ.'C36')         IS=.TRUE.
          IF(WC.EQ.'C37')         IS=.TRUE.
          IF(WC.EQ.'C38')         IS=.TRUE.
          IF(WC.EQ.'C39')         IS=.TRUE.
          IF(WC.EQ.'C40')         IS=.TRUE.
          IF(WC.EQ.'C41')         IS=.TRUE.
          IF(WC.EQ.'C42')         IS=.TRUE.
          IF(WC.EQ.'C43')         IS=.TRUE.
          IF(WC.EQ.'C44')         IS=.TRUE.
          IF(WC.EQ.'C45')         IS=.TRUE.
          IF(WC.EQ.'C46')         IS=.TRUE.
          IF(WC.EQ.'C47')         IS=.TRUE.
          IF(WC.EQ.'C48')         IS=.TRUE.
          IF(WC.EQ.'C49')                         IS=.TRUE.
          IF(WC.EQ.'C50')                         IS=.TRUE.
          IF(WC.EQ.'C51')                         IS=.TRUE.
          IF(WC.EQ.'C52')                         IS=.TRUE.
          IF(WC.EQ.'C53')                         IS=.TRUE.
          IF(WC.EQ.'C54')                         IS=.TRUE.
          IF(WC.EQ.'C55')                         IS=.TRUE.
          IF(WC.EQ.'C56')                         IS=.TRUE.
          IF(WC.EQ.'C57')                         IS=.TRUE.
          IF(WC.EQ.'C58')                         IS=.TRUE.
          IF(WC.EQ.'C59')                         IS=.TRUE.
          IF(WC.EQ.'C60')                         IS=.TRUE.
          IF(WC.EQ.'C61')                         IS=.TRUE.
          IF(WC.EQ.'C62')                         IS=.TRUE.
          IF(WC.EQ.'C63')                         IS=.TRUE.
          IF(WC.EQ.'C64')                         IS=.TRUE.
          IF(WC.EQ.'C65')                         IS=.TRUE.
          IF(WC.EQ.'C66')                         IS=.TRUE.
          IF(WC.EQ.'C67')                         IS=.TRUE.
          IF(WC.EQ.'C68')                         IS=.TRUE.
          IF(WC.EQ.'C69')                         IS=.TRUE.
          IF(WC.EQ.'C70')                         IS=.TRUE.
          IF(WC.EQ.'C71')                         IS=.TRUE.
          IF(WC.EQ.'C72')                         IS=.TRUE.
          IF(WC.EQ.'C73')                         IS=.TRUE.
          IF(WC.EQ.'C74')                         IS=.TRUE.
          IF(WC.EQ.'C75')                         IS=.TRUE.
          IF(WC.EQ.'C76')                         IS=.TRUE.
          IF(WC.EQ.'C77')                         IS=.TRUE.
          IF(WC.EQ.'C78')                         IS=.TRUE.
          IF(WC.EQ.'C79')                         IS=.TRUE.
          IF(WC.EQ.'C80')                         IS=.TRUE.
          IF(WC.EQ.'C81')                         IS=.TRUE.
          IF(WC.EQ.'C82')                         IS=.TRUE.
          IF(WC.EQ.'C83')                         IS=.TRUE.
          IF(WC.EQ.'C84')                         IS=.TRUE.
          IF(WC.EQ.'C85')                         IS=.TRUE.
          IF(WC.EQ.'C86')                         IS=.TRUE.
          IF(WC.EQ.'C87')                         IS=.TRUE.
          IF(WC.EQ.'C88')                         IS=.TRUE.
          IF(WC.EQ.'C89')                         IS=.TRUE.
          IF(WC.EQ.'C90')                         IS=.TRUE.
          IF(WC.EQ.'C91')                         IS=.TRUE.
          IF(WC.EQ.'C92')                         IS=.TRUE.
          IF(WC.EQ.'C93')                         IS=.TRUE.
          IF(WC.EQ.'C94')                         IS=.TRUE.
          IF(WC.EQ.'C95')                         IS=.TRUE.
          IF(WC.EQ.'C96')                         IS=.TRUE.
          IF(WC.EQ.'CV_FR')                       IS=.TRUE.
          IF(WC.EQ.'RD_FR')                       IS=.TRUE.
          IF(WC.EQ.'RDTFR')                       IS=.TRUE.
          IF(WC.EQ.'CVTFR')                       IS=.TRUE.
          IF(WC.EQ.'STILTA') IS=.TRUE.
          IF(WC.EQ.'STILTB') IS=.TRUE.
          IF(WC.EQ.'STILTG') IS=.TRUE.
          IF(WC.EQ.'BTILTA') IS=.TRUE.
          IF(WC.EQ.'BTILTB') IS=.TRUE.
          IF(WC.EQ.'BTILTG') IS=.TRUE.
          IF(WC.EQ.'DISPX')  IS=.TRUE.
          IF(WC.EQ.'DISPY')  IS=.TRUE.
          IF(WC.EQ.'DISPZ')  IS=.TRUE.
          IF(WC.EQ.'ROLLX')  IS=.TRUE.
          IF(WC.EQ.'ROLLY')  IS=.TRUE.
          IF(WC.EQ.'DEL'.AND.F51.EQ.2)            IS=.TRUE.
          RETURN
      END


      SUBROUTINE CONTQ(WC,F52,IS)
          IMPLICIT NONE
          CHARACTER WC*8
          INTEGER F52
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'ARCL')        IS=.TRUE.
          IF(WC.EQ.'STOAX')       IS=.TRUE.
          IF(WC.EQ.'SET')         IS=.TRUE.
          IF(WC.EQ.'W1')          IS=.TRUE.
          IF(WC.EQ.'W2')          IS=.TRUE.
          IF(WC.EQ.'W3')          IS=.TRUE.
          IF(WC.EQ.'W4')          IS=.TRUE.
          IF(WC.EQ.'W5')          IS=.TRUE.
          IF(WC.EQ.'EOS')         IS=.TRUE.
          IF(WC.EQ.'COMPS')       IS=.TRUE.
          IF(WC.EQ.'C')           IS=.TRUE.
          IF(WC.EQ.'M')           IS=.TRUE.
          IF(WC.EQ.'RD')          IS=.TRUE.
          IF(WC.EQ.'CV')          IS=.TRUE.
          IF(WC.EQ.'TH')          IS=.TRUE.
          IF(WC.EQ.'CC')          IS=.TRUE.
          IF(WC.EQ.'AC')          IS=.TRUE.
          IF(WC.EQ.'AD')          IS=.TRUE.
          IF(WC.EQ.'AE')          IS=.TRUE.
          IF(WC.EQ.'AF')          IS=.TRUE.
          IF(WC.EQ.'AG')          IS=.TRUE.
          IF(WC.EQ.'AH')          IS=.TRUE.
          IF(WC.EQ.'AI')          IS=.TRUE.
          IF(WC.EQ.'AJ')          IS=.TRUE.
          IF(WC.EQ.'AK')          IS=.TRUE.
          IF(WC.EQ.'AL')          IS=.TRUE.
          IF(WC.EQ.'RDTOR')       IS=.TRUE.
          IF(WC.EQ.'CVTOR')       IS=.TRUE.
          IF(WC.EQ.'GRS')         IS=.TRUE.
          IF(WC.EQ.'CCTOR')       IS=.TRUE.
          IF(WC.EQ.'ADTOR')       IS=.TRUE.
          IF(WC.EQ.'AETOR')       IS=.TRUE.
          IF(WC.EQ.'AFTOR')       IS=.TRUE.
          IF(WC.EQ.'AGTOR')       IS=.TRUE.
          IF(WC.EQ.'ALPHA')       IS=.TRUE.
          IF(WC.EQ.'BETA')        IS=.TRUE.
          IF(WC.EQ.'GAMMA')       IS=.TRUE.
          IF(WC.EQ.'GDX     ')    IS=.TRUE.
          IF(WC.EQ.'GDY     ')    IS=.TRUE.
          IF(WC.EQ.'GDZ     ')    IS=.TRUE.
          IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
          IF(WC.EQ.'GBETA   ')    IS=.TRUE.
          IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
          IF(WC.EQ.'XD')          IS=.TRUE.
          IF(WC.EQ.'YD')          IS=.TRUE.
          IF(WC.EQ.'ZD')          IS=.TRUE.
          IF(WC.EQ.'PIVY')        IS=.TRUE.
          IF(WC.EQ.'PIVX')        IS=.TRUE.
          IF(WC.EQ.'PIVZ')        IS=.TRUE.
          IF(WC.EQ.'N1')          IS=.TRUE.
          IF(WC.EQ.'N2')          IS=.TRUE.
          IF(WC.EQ.'N3')          IS=.TRUE.
          IF(WC.EQ.'N4')          IS=.TRUE.
          IF(WC.EQ.'N5')          IS=.TRUE.
          IF(WC.EQ.'N6')          IS=.TRUE.
          IF(WC.EQ.'N7')          IS=.TRUE.
          IF(WC.EQ.'N8')          IS=.TRUE.
          IF(WC.EQ.'N9')          IS=.TRUE.
          IF(WC.EQ.'N10')         IS=.TRUE.
          IF(WC.EQ.'INDEX')       IS=.TRUE.
          IF(WC.EQ.'VNUM')        IS=.TRUE.
          IF(WC.EQ.'DPART')       IS=.TRUE.
          IF(WC.EQ.'CLPX')        IS=.TRUE.
          IF(WC.EQ.'CLPY')        IS=.TRUE.
          IF(WC.EQ.'C1')          IS=.TRUE.
          IF(WC.EQ.'C2')          IS=.TRUE.
          IF(WC.EQ.'C3')          IS=.TRUE.
          IF(WC.EQ.'C4')          IS=.TRUE.
          IF(WC.EQ.'C5')          IS=.TRUE.
          IF(WC.EQ.'C6')          IS=.TRUE.
          IF(WC.EQ.'C7')          IS=.TRUE.
          IF(WC.EQ.'C8')          IS=.TRUE.
          IF(WC.EQ.'C9')          IS=.TRUE.
          IF(WC.EQ.'C10')         IS=.TRUE.
          IF(WC.EQ.'C11')         IS=.TRUE.
          IF(WC.EQ.'C12')         IS=.TRUE.
          IF(WC.EQ.'C13')         IS=.TRUE.
          IF(WC.EQ.'C14')         IS=.TRUE.
          IF(WC.EQ.'C15')         IS=.TRUE.
          IF(WC.EQ.'C16')         IS=.TRUE.
          IF(WC.EQ.'C17')         IS=.TRUE.
          IF(WC.EQ.'C18')         IS=.TRUE.
          IF(WC.EQ.'C19')         IS=.TRUE.
          IF(WC.EQ.'C20')         IS=.TRUE.
          IF(WC.EQ.'C21')         IS=.TRUE.
          IF(WC.EQ.'C22')         IS=.TRUE.
          IF(WC.EQ.'C23')         IS=.TRUE.
          IF(WC.EQ.'C24')         IS=.TRUE.
          IF(WC.EQ.'C25')         IS=.TRUE.
          IF(WC.EQ.'C26')         IS=.TRUE.
          IF(WC.EQ.'C27')         IS=.TRUE.
          IF(WC.EQ.'C28')         IS=.TRUE.
          IF(WC.EQ.'C29')         IS=.TRUE.
          IF(WC.EQ.'C30')         IS=.TRUE.
          IF(WC.EQ.'C31')         IS=.TRUE.
          IF(WC.EQ.'C32')         IS=.TRUE.
          IF(WC.EQ.'C33')         IS=.TRUE.
          IF(WC.EQ.'C34')         IS=.TRUE.
          IF(WC.EQ.'C35')         IS=.TRUE.
          IF(WC.EQ.'C36')         IS=.TRUE.
          IF(WC.EQ.'C37')         IS=.TRUE.
          IF(WC.EQ.'C38')         IS=.TRUE.
          IF(WC.EQ.'C39')         IS=.TRUE.
          IF(WC.EQ.'C40')         IS=.TRUE.
          IF(WC.EQ.'C41')         IS=.TRUE.
          IF(WC.EQ.'C42')         IS=.TRUE.
          IF(WC.EQ.'C43')         IS=.TRUE.
          IF(WC.EQ.'C44')         IS=.TRUE.
          IF(WC.EQ.'C45')         IS=.TRUE.
          IF(WC.EQ.'C46')         IS=.TRUE.
          IF(WC.EQ.'C47')         IS=.TRUE.
          IF(WC.EQ.'C48')         IS=.TRUE.
          IF(WC.EQ.'C49')                         IS=.TRUE.
          IF(WC.EQ.'C50')                         IS=.TRUE.
          IF(WC.EQ.'C51')                         IS=.TRUE.
          IF(WC.EQ.'C52')                         IS=.TRUE.
          IF(WC.EQ.'C53')                         IS=.TRUE.
          IF(WC.EQ.'C54')                         IS=.TRUE.
          IF(WC.EQ.'C55')                         IS=.TRUE.
          IF(WC.EQ.'C56')                         IS=.TRUE.
          IF(WC.EQ.'C57')                         IS=.TRUE.
          IF(WC.EQ.'C58')                         IS=.TRUE.
          IF(WC.EQ.'C59')                         IS=.TRUE.
          IF(WC.EQ.'C60')                         IS=.TRUE.
          IF(WC.EQ.'C61')                         IS=.TRUE.
          IF(WC.EQ.'C62')                         IS=.TRUE.
          IF(WC.EQ.'C63')                         IS=.TRUE.
          IF(WC.EQ.'C64')                         IS=.TRUE.
          IF(WC.EQ.'C65')                         IS=.TRUE.
          IF(WC.EQ.'C66')                         IS=.TRUE.
          IF(WC.EQ.'C67')                         IS=.TRUE.
          IF(WC.EQ.'C68')                         IS=.TRUE.
          IF(WC.EQ.'C69')                         IS=.TRUE.
          IF(WC.EQ.'C70')                         IS=.TRUE.
          IF(WC.EQ.'C71')                         IS=.TRUE.
          IF(WC.EQ.'C72')                         IS=.TRUE.
          IF(WC.EQ.'C73')                         IS=.TRUE.
          IF(WC.EQ.'C74')                         IS=.TRUE.
          IF(WC.EQ.'C75')                         IS=.TRUE.
          IF(WC.EQ.'C76')                         IS=.TRUE.
          IF(WC.EQ.'C77')                         IS=.TRUE.
          IF(WC.EQ.'C78')                         IS=.TRUE.
          IF(WC.EQ.'C79')                         IS=.TRUE.
          IF(WC.EQ.'C80')                         IS=.TRUE.
          IF(WC.EQ.'C81')                         IS=.TRUE.
          IF(WC.EQ.'C82')                         IS=.TRUE.
          IF(WC.EQ.'C83')                         IS=.TRUE.
          IF(WC.EQ.'C84')                         IS=.TRUE.
          IF(WC.EQ.'C85')                         IS=.TRUE.
          IF(WC.EQ.'C86')                         IS=.TRUE.
          IF(WC.EQ.'C87')                         IS=.TRUE.
          IF(WC.EQ.'C88')                         IS=.TRUE.
          IF(WC.EQ.'C89')                         IS=.TRUE.
          IF(WC.EQ.'C90')                         IS=.TRUE.
          IF(WC.EQ.'C91')                         IS=.TRUE.
          IF(WC.EQ.'C92')                         IS=.TRUE.
          IF(WC.EQ.'C93')                         IS=.TRUE.
          IF(WC.EQ.'C94')                         IS=.TRUE.
          IF(WC.EQ.'C95')                         IS=.TRUE.
          IF(WC.EQ.'C96')                         IS=.TRUE.
          IF(WC.EQ.'DEL'.AND.F52.EQ.2)            IS=.TRUE.
          RETURN
      END


      SUBROUTINE CONTR(WC,WQ,IS)
C       CONFIGS INPUT AND UPDATE
          IMPLICIT NONE
          CHARACTER WC*8,WQ*8
          !     INTEGER SQ
          LOGICAL IS
          IS=.FALSE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'ARCL')        IS=.TRUE.
          IF(WC.EQ.'STOAX')       IS=.TRUE.
          IF(WC.EQ.'SET')         IS=.TRUE.
          IF(WC.EQ.'W1')          IS=.TRUE.
          IF(WC.EQ.'W2')          IS=.TRUE.
          IF(WC.EQ.'W3')          IS=.TRUE.
          IF(WC.EQ.'W4')          IS=.TRUE.
          IF(WC.EQ.'W5')          IS=.TRUE.
          IF(WC.EQ.'LI')          IS=.TRUE.
          IF(WC.EQ.'LABEL')       IS=.TRUE.
          IF(WC.EQ.'LBL')         IS=.TRUE.
          IF(WC.EQ.'LIC')         IS=.TRUE.
          IF(WC.EQ.'INI')         IS=.TRUE.
          IF(WC.EQ.'LTYPE')       IS=.TRUE.
          IF(WC.EQ.'MODE')        IS=.TRUE.
          IF(WC.EQ.'SPTWT')       IS=.TRUE.
          IF(WC.EQ.'NODUM')       IS=.TRUE.
          IF(WC.EQ.'SPTWT2')      IS=.TRUE.
          IF(WC.EQ.'WV')          IS=.TRUE.
          IF(WC.EQ.'WV2')         IS=.TRUE.
          IF(WC.EQ.'UNITS')       IS=.TRUE.
          IF(WC.EQ.'PCW')         IS=.TRUE.
          IF(WC.EQ.'SCW')         IS=.TRUE.
          IF(WC.EQ.'CW')          IS=.TRUE.
          IF(WC.EQ.'SAY')         IS=.TRUE.
          IF(WC.EQ.'SAX')         IS=.TRUE.
          IF(WC.EQ.'WRX')         IS=.TRUE.
          IF(WC.EQ.'WRY')         IS=.TRUE.
          IF(WC.EQ.'BDX')         IS=.TRUE.
          IF(WC.EQ.'BDY')         IS=.TRUE.
          IF(WC.EQ.'NAOY')        IS=.TRUE.
          IF(WC.EQ.'NAOX')        IS=.TRUE.
          IF(WC.EQ.'FNOY')        IS=.TRUE.
          IF(WC.EQ.'FNOX')        IS=.TRUE.
          IF(WC.EQ.'SCY')         IS=.TRUE.
          IF(WC.EQ.'SCX')         IS=.TRUE.
          IF(WC.EQ.'PYIM')        IS=.TRUE.
          IF(WC.EQ.'PXIM')        IS=.TRUE.
          IF(WC.EQ.'RYIM')        IS=.TRUE.
          IF(WC.EQ.'RXIM')        IS=.TRUE.
          IF(WC.EQ.'CV')          IS=.TRUE.
          IF(WC.EQ.'RD')          IS=.TRUE.
          IF(WC.EQ.'CC')          IS=.TRUE.
          IF(WC.EQ.'ASPH')        IS=.TRUE.
          IF(WC.EQ.'ASPH2')       IS=.TRUE.
          IF(WC.EQ.'YTORIC')      IS=.TRUE.
          IF(WC.EQ.'XTORIC')      IS=.TRUE.
          IF(WC.EQ.'NORMAL')      IS=.TRUE.
          IF(WC.EQ.'SPIDER')      IS=.TRUE.
          IF(WC.EQ.'APY')         IS=.TRUE.
          IF(WC.EQ.'APX')         IS=.TRUE.
          IF(WC.EQ.'PIY')         IS=.TRUE.
          IF(WC.EQ.'PIX')         IS=.TRUE.
          IF(WC.EQ.'PUY')         IS=.TRUE.
          IF(WC.EQ.'PUX')         IS=.TRUE.
          IF(WC.EQ.'APCY')        IS=.TRUE.
          IF(WC.EQ.'APCX')        IS=.TRUE.
          IF(WC.EQ.'PICY')        IS=.TRUE.
          IF(WC.EQ.'PICX')        IS=.TRUE.
          IF(WC.EQ.'PUCY')        IS=.TRUE.
          IF(WC.EQ.'PUCX')        IS=.TRUE.
          IF(WC.EQ.'COCY')        IS=.TRUE.
          IF(WC.EQ.'COCX')        IS=.TRUE.
          IF(WC.EQ.'PIKUP')       IS=.TRUE.
          IF(WC.EQ.'DEC')         IS=.TRUE.
          IF(WC.EQ.'TILT')        IS=.TRUE.
          IF(WC.EQ.'RTILT')       IS=.TRUE.
          IF(WC.EQ.'ASTOP')       IS=.TRUE.
          IF(WC.EQ.'REFS')        IS=.TRUE.
          IF(WC.EQ.'TH')          IS=.TRUE.
          IF(WC.EQ.'THM')         IS=.TRUE.
          IF(WC.EQ.'PRICE')       IS=.TRUE.
          IF(WC.EQ.'AUTOFUNC')    IS=.TRUE.
C     BEGINNING OF NON-SEQUENTIAL COMMANDS
          IF(WC.EQ.'CCR')         IS=.TRUE.
          IF(WC.EQ.'ROO')         IS=.TRUE.
C     END OF NON-SEQUENTIAL COMMANDS
          IF(WC.EQ.'RAYERROR')    IS=.TRUE.
          IF(WC.EQ.'PY')          IS=.TRUE.
          IF(WC.EQ.'PCY')         IS=.TRUE.
          IF(WC.EQ.'CAY')         IS=.TRUE.
          IF(WC.EQ.'PX')          IS=.TRUE.
          IF(WC.EQ.'PCX')         IS=.TRUE.
          IF(WC.EQ.'CAX')         IS=.TRUE.
          IF(WC.EQ.'CLAP')        IS=.TRUE.
          IF(WC.EQ.'COBS')        IS=.TRUE.
          IF(WC.EQ.'GLA')         IS=.TRUE.
          IF(WC.EQ.'GLASS')       IS=.TRUE.
          IF(WC.EQ.'DEFORM')      IS=.TRUE.
          IF(WC.EQ.'DELDEFOR')    IS=.TRUE.
          IF(WC.EQ.'SCHOTT')      IS=.TRUE.
          IF(WC.EQ.'SCH2000')     IS=.TRUE.
          IF(WC.EQ.'MULTCLAP')    IS=.TRUE.
          IF(WC.EQ.'MULTCOBS')    IS=.TRUE.
          IF(WC.EQ.'SPIDER')      IS=.TRUE.
          IF(WC.EQ.'SPGR')        IS=.TRUE.
          IF(WC.EQ.'MODEL')       IS=.TRUE.
          IF(WC.EQ.'RADHARD')     IS=.TRUE.
          IF(WC.EQ.'MATL')        IS=.TRUE.
          IF(WC.EQ.'RUSSIAN')     IS=.TRUE.
          IF(WC.EQ.'USER')        IS=.TRUE.
          IF(WC.EQ.'GLCAT')       IS=.TRUE.
          IF(WC.EQ.'OHARA')       IS=.TRUE.
          IF(WC.EQ.'HOYA')        IS=.TRUE.
          IF(WC.EQ.'HIKARI')      IS=.TRUE.
          IF(WC.EQ.'CHANCE')      IS=.TRUE.
          IF(WC.EQ.'CORNIN')      IS=.TRUE.
          IF(WC.EQ.'AIR')         IS=.TRUE.
          IF(WC.EQ.'COATING ')    IS=.TRUE.
          IF(WC.EQ.'REFL')        IS=.TRUE.
          IF(WC.EQ.'REFLTIRO')    IS=.TRUE.
          IF(WC.EQ.'REFLTIR')     IS=.TRUE.
          IF(WC.EQ.'PERFECT')     IS=.TRUE.
          IF(WC.EQ.'IDEAL')       IS=.TRUE.
          IF(WC.EQ.'EOS')         IS=.TRUE.
          IF(WC.EQ.'TASPH')       IS=.TRUE.
          IF(WC.EQ.'C')           IS=.TRUE.
          IF(WC.EQ.'M')           IS=.TRUE.
          IF(WC.EQ.'CHG')         IS=.TRUE.
          IF(WC.EQ.'CVTOR')       IS=.TRUE.
          IF(WC.EQ.'REAL')        IS=.TRUE.
          IF(WC.EQ.'PARAX')       IS=.TRUE.
          IF(WC.EQ.'GRT')         IS=.TRUE.
          IF(WC.EQ.'GRO')         IS=.TRUE.
          IF(WC.EQ.'GRS')         IS=.TRUE.
          IF(WC.EQ.'GRX')         IS=.TRUE.
          IF(WC.EQ.'GRY')         IS=.TRUE.
          IF(WC.EQ.'GRZ')         IS=.TRUE.
          IF(WC.EQ.'RDTOR')       IS=.TRUE.
          IF(WC.EQ.'YTORIC')      IS=.TRUE.
          IF(WC.EQ.'XTORIC')      IS=.TRUE.
          IF(WC.EQ.'NORMAL')      IS=.TRUE.
          IF(WC.EQ.'FOOTBLOK')    IS=.TRUE.
          IF(WC.EQ.'PIVOT')       IS=.TRUE.
          IF(WC.EQ.'PIVAXIS')     IS=.TRUE.
          IF(WC.EQ.'PIVOTD')      IS=.TRUE.
          IF(WC.EQ.'XD')          IS=.TRUE.
          IF(WC.EQ.'YD')          IS=.TRUE.
          IF(WC.EQ.'ZD')          IS=.TRUE.
          IF(WC.EQ.'PIVY')        IS=.TRUE.
          IF(WC.EQ.'PIVX')        IS=.TRUE.
          IF(WC.EQ.'PIVZ')        IS=.TRUE.
          IF(WC.EQ.'ALPHA')       IS=.TRUE.
          IF(WC.EQ.'GDX     ')    IS=.TRUE.
          IF(WC.EQ.'GDY     ')    IS=.TRUE.
          IF(WC.EQ.'GDZ     ')    IS=.TRUE.
          IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
          IF(WC.EQ.'GBETA   ')    IS=.TRUE.
          IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
          IF(WC.EQ.'TASPHD')      IS=.TRUE.
          IF(WC.EQ.'ARRAYD')      IS=.TRUE.
          IF(WC.EQ.'ARRAY')       IS=.TRUE.
          IF(WC.EQ.'CCTOR')       IS=.TRUE.
          IF(WC.EQ.'ADTOR')       IS=.TRUE.
          IF(WC.EQ.'AETOR')       IS=.TRUE.
          IF(WC.EQ.'AFTOR')       IS=.TRUE.
          IF(WC.EQ.'AGTOR')       IS=.TRUE.
          IF(WC.EQ.'AC')          IS=.TRUE.
          IF(WC.EQ.'AD')          IS=.TRUE.
          IF(WC.EQ.'AE')          IS=.TRUE.
          IF(WC.EQ.'AF')          IS=.TRUE.
          IF(WC.EQ.'AG')          IS=.TRUE.
          IF(WC.EQ.'AH')          IS=.TRUE.
          IF(WC.EQ.'AI')          IS=.TRUE.
          IF(WC.EQ.'AJ')          IS=.TRUE.
          IF(WC.EQ.'AK')          IS=.TRUE.
          IF(WC.EQ.'AL')          IS=.TRUE.
          IF(WC.EQ.'N1')          IS=.TRUE.
          IF(WC.EQ.'N2')          IS=.TRUE.
          IF(WC.EQ.'N3')          IS=.TRUE.
          IF(WC.EQ.'N4')          IS=.TRUE.
          IF(WC.EQ.'N5')          IS=.TRUE.
          IF(WC.EQ.'N6')          IS=.TRUE.
          IF(WC.EQ.'N7')          IS=.TRUE.
          IF(WC.EQ.'N8')          IS=.TRUE.
          IF(WC.EQ.'N9')          IS=.TRUE.
          IF(WC.EQ.'N10')         IS=.TRUE.
          IF(WC.EQ.'INDEX')       IS=.TRUE.
          IF(WC.EQ.'BETA')        IS=.TRUE.
          IF(WC.EQ.'GAMMA')       IS=.TRUE.
          IF(WC.EQ.'ASPHD')       IS=.TRUE.
          IF(WC.EQ.'TORD')        IS=.TRUE.
          IF(WC.EQ.'TILTD')       IS=.TRUE.
          IF(WC.EQ.'CSD')         IS=.TRUE.
          IF(WC.EQ.'CSDX')        IS=.TRUE.
          IF(WC.EQ.'CSDY')        IS=.TRUE.
          IF(WC.EQ.'TSD')         IS=.TRUE.
          IF(WC.EQ.'GRTD')        IS=.TRUE.
          IF(WC.EQ.'PIKD')        IS=.TRUE.
          IF(WC.EQ.'CLAPD')       IS=.TRUE.
          IF(WC.EQ.'COBSD')       IS=.TRUE.
          IF(WC.EQ.'INR')         IS=.TRUE.
          IF(WC.EQ.'INRD')        IS=.TRUE.
          IF(WC.EQ.'ZERO')        IS=.TRUE.
C
C       THE FOLLOWING SPSRF COMMANDS
C        ARE VALID AS WELL
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'ARCL')        IS=.TRUE.
          IF(WC.EQ.'STOAX')       IS=.TRUE.
          IF(WC.EQ.'SET')         IS=.TRUE.
          IF(WC.EQ.'W1')          IS=.TRUE.
          IF(WC.EQ.'W2')          IS=.TRUE.
          IF(WC.EQ.'W3')          IS=.TRUE.
          IF(WC.EQ.'W4')          IS=.TRUE.
          IF(WC.EQ.'W5')          IS=.TRUE.
          IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'ON')        IS=.TRUE.
          IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'OFF')       IS=.TRUE.
          IF(WC.EQ.'SPDEL')                       IS=.TRUE.
          IF(WC.EQ.'SPECIAL')                     IS=.TRUE.
          IF(WC.EQ.'GENL')                        IS=.TRUE.
          IF(WC.EQ.'C1')                          IS=.TRUE.
          IF(WC.EQ.'C2')                          IS=.TRUE.
          IF(WC.EQ.'C3')                          IS=.TRUE.
          IF(WC.EQ.'C4')                          IS=.TRUE.
          IF(WC.EQ.'C5')                          IS=.TRUE.
          IF(WC.EQ.'C6')                          IS=.TRUE.
          IF(WC.EQ.'C7')                          IS=.TRUE.
          IF(WC.EQ.'C8')                          IS=.TRUE.
          IF(WC.EQ.'C9')                          IS=.TRUE.
          IF(WC.EQ.'C10')                         IS=.TRUE.
          IF(WC.EQ.'C11')                         IS=.TRUE.
          IF(WC.EQ.'C12')                         IS=.TRUE.
          IF(WC.EQ.'C13')                         IS=.TRUE.
          IF(WC.EQ.'C14')                         IS=.TRUE.
          IF(WC.EQ.'C15')                         IS=.TRUE.
          IF(WC.EQ.'C16')                         IS=.TRUE.
          IF(WC.EQ.'C17')                         IS=.TRUE.
          IF(WC.EQ.'C18')                         IS=.TRUE.
          IF(WC.EQ.'C19')                         IS=.TRUE.
          IF(WC.EQ.'C20')                         IS=.TRUE.
          IF(WC.EQ.'C21')                         IS=.TRUE.
          IF(WC.EQ.'C22')                         IS=.TRUE.
          IF(WC.EQ.'C23')                         IS=.TRUE.
          IF(WC.EQ.'C24')                         IS=.TRUE.
          IF(WC.EQ.'C25')                         IS=.TRUE.
          IF(WC.EQ.'C26')                         IS=.TRUE.
          IF(WC.EQ.'C27')                         IS=.TRUE.
          IF(WC.EQ.'C28')                         IS=.TRUE.
          IF(WC.EQ.'C29')                         IS=.TRUE.
          IF(WC.EQ.'C30')                         IS=.TRUE.
          IF(WC.EQ.'C31')                         IS=.TRUE.
          IF(WC.EQ.'C32')                         IS=.TRUE.
          IF(WC.EQ.'C33')                         IS=.TRUE.
          IF(WC.EQ.'C34')                         IS=.TRUE.
          IF(WC.EQ.'C35')                         IS=.TRUE.
          IF(WC.EQ.'C36')                         IS=.TRUE.
          IF(WC.EQ.'C37')                         IS=.TRUE.
          IF(WC.EQ.'C38')                         IS=.TRUE.
          IF(WC.EQ.'C39')                         IS=.TRUE.
          IF(WC.EQ.'C40')                         IS=.TRUE.
          IF(WC.EQ.'C41')                         IS=.TRUE.
          IF(WC.EQ.'C42')                         IS=.TRUE.
          IF(WC.EQ.'C43')                         IS=.TRUE.
          IF(WC.EQ.'C44')                         IS=.TRUE.
          IF(WC.EQ.'C45')                         IS=.TRUE.
          IF(WC.EQ.'C46')                         IS=.TRUE.
          IF(WC.EQ.'C47')                         IS=.TRUE.
          IF(WC.EQ.'C48')                         IS=.TRUE.
          IF(WC.EQ.'C49')                         IS=.TRUE.
          IF(WC.EQ.'C50')                         IS=.TRUE.
          IF(WC.EQ.'C51')                         IS=.TRUE.
          IF(WC.EQ.'C52')                         IS=.TRUE.
          IF(WC.EQ.'C53')                         IS=.TRUE.
          IF(WC.EQ.'C54')                         IS=.TRUE.
          IF(WC.EQ.'C55')                         IS=.TRUE.
          IF(WC.EQ.'C56')                         IS=.TRUE.
          IF(WC.EQ.'C57')                         IS=.TRUE.
          IF(WC.EQ.'C58')                         IS=.TRUE.
          IF(WC.EQ.'C59')                         IS=.TRUE.
          IF(WC.EQ.'C60')                         IS=.TRUE.
          IF(WC.EQ.'C61')                         IS=.TRUE.
          IF(WC.EQ.'C62')                         IS=.TRUE.
          IF(WC.EQ.'C63')                         IS=.TRUE.
          IF(WC.EQ.'C64')                         IS=.TRUE.
          IF(WC.EQ.'C65')                         IS=.TRUE.
          IF(WC.EQ.'C66')                         IS=.TRUE.
          IF(WC.EQ.'C67')                         IS=.TRUE.
          IF(WC.EQ.'C68')                         IS=.TRUE.
          IF(WC.EQ.'C69')                         IS=.TRUE.
          IF(WC.EQ.'C70')                         IS=.TRUE.
          IF(WC.EQ.'C71')                         IS=.TRUE.
          IF(WC.EQ.'C72')                         IS=.TRUE.
          IF(WC.EQ.'C73')                         IS=.TRUE.
          IF(WC.EQ.'C74')                         IS=.TRUE.
          IF(WC.EQ.'C75')                         IS=.TRUE.
          IF(WC.EQ.'C76')                         IS=.TRUE.
          IF(WC.EQ.'C77')                         IS=.TRUE.
          IF(WC.EQ.'C78')                         IS=.TRUE.
          IF(WC.EQ.'C79')                         IS=.TRUE.
          IF(WC.EQ.'C80')                         IS=.TRUE.
          IF(WC.EQ.'C81')                         IS=.TRUE.
          IF(WC.EQ.'C82')                         IS=.TRUE.
          IF(WC.EQ.'C83')                         IS=.TRUE.
          IF(WC.EQ.'C84')                         IS=.TRUE.
          IF(WC.EQ.'C85')                         IS=.TRUE.
          IF(WC.EQ.'C86')                         IS=.TRUE.
          IF(WC.EQ.'C87')                         IS=.TRUE.
          IF(WC.EQ.'C88')                         IS=.TRUE.
          IF(WC.EQ.'C89')                         IS=.TRUE.
          IF(WC.EQ.'C90')                         IS=.TRUE.
          IF(WC.EQ.'C91')                         IS=.TRUE.
          IF(WC.EQ.'C92')                         IS=.TRUE.
          IF(WC.EQ.'C93')                         IS=.TRUE.
          IF(WC.EQ.'C94')                         IS=.TRUE.
          IF(WC.EQ.'C95')                         IS=.TRUE.
          IF(WC.EQ.'C96')                         IS=.TRUE.
C
C      ALSO   THE FOLLOWING CMD LEVEL COMMANDS
C
          IF(WC.EQ.'UPDATE'.OR.WC.EQ.'U') THEN
              IF(WQ.EQ.'LENS'.OR.WQ.EQ.'L')           IS=.TRUE.
              IF(WQ.EQ.'SP'.OR.WQ.EQ.'SPSRF')         IS=.TRUE.
          END IF
          IF(WC.EQ.'MODE')                        IS=.TRUE.
          IF(WC.EQ.'FNBX')                        IS=.TRUE.
          IF(WC.EQ.'FNBY')                        IS=.TRUE.
          IF(WC.EQ.'ERX')                         IS=.TRUE.
          IF(WC.EQ.'ERY')                         IS=.TRUE.
          IF(WC.EQ.'SPTWT')                       IS=.TRUE.
          IF(WC.EQ.'NODUM')       IS=.TRUE.
          IF(WC.EQ.'SPTWT2')                      IS=.TRUE.
          IF(WC.EQ.'THERM')                       IS=.TRUE.
          IF(WC.EQ.'PRES')                        IS=.TRUE.
          IF(WC.EQ.'MAGY')                        IS=.TRUE.
          IF(WC.EQ.'MAGX')                        IS=.TRUE.
          IF(WC.EQ.'FLDS')                        IS=.TRUE.
          IF(WC.EQ.'CFG')                         IS=.TRUE.
          IF(WC.EQ.'GET     ') IS=.TRUE.
          IF(WC.EQ.'AGET    ') IS=.TRUE.
          IF(WC.EQ.'RCL     ') IS=.TRUE.
          IF(WC.EQ.'STO     ') IS=.TRUE.
          IF(WC.EQ.'WRITE   ') IS=.TRUE.
          IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
          IF(WC.EQ.'MINVAL  ') IS=.TRUE.
          IF(WC.EQ.'PI      ') IS=.TRUE.
          IF(WC.EQ.'CHS     ') IS=.TRUE.
          IF(WC.EQ.'MOVE    ') IS=.TRUE.
          IF(WC.EQ.'LASTX   ') IS=.TRUE.
          IF(WC.EQ.'LASTIX  ') IS=.TRUE.
          IF(WC.EQ.'INCR    ') IS=.TRUE.
          IF(WC.EQ.'PLUS    ') IS=.TRUE.
          IF(WC.EQ.'MINUS   ') IS=.TRUE.
          IF(WC.EQ.'MPY     ') IS=.TRUE.
          IF(WC.EQ.'DIV     ') IS=.TRUE.
          IF(WC.EQ.'SQRT    ') IS=.TRUE.
          IF(WC.EQ.'SIN     ') IS=.TRUE.
          IF(WC.EQ.'COS     ') IS=.TRUE.
          IF(WC.EQ.'TAN     ') IS=.TRUE.
          IF(WC.EQ.'SINH    ') IS=.TRUE.
          IF(WC.EQ.'COSH    ') IS=.TRUE.
          IF(WC.EQ.'TANH    ') IS=.TRUE.
          IF(WC.EQ.'CLSTK   ') IS=.TRUE.
          IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
          IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
          IF(WC.EQ.'CLREG   ') IS=.TRUE.
          IF(WC.EQ.'CLGREG  ') IS=.TRUE.
          IF(WC.EQ.'CLX     ') IS=.TRUE.
          IF(WC.EQ.'CLIX    ') IS=.TRUE.
          IF(WC.EQ.'ASIN    ') IS=.TRUE.
          IF(WC.EQ.'ACOS    ') IS=.TRUE.
          IF(WC.EQ.'ATAN    ') IS=.TRUE.
          IF(WC.EQ.'ABS     ') IS=.TRUE.
          IF(WC.EQ.'EXP     ') IS=.TRUE.
          IF(WC.EQ.'LN      ') IS=.TRUE.
          IF(WC.EQ.'LOG10   ') IS=.TRUE.
          IF(WC.EQ.'FACT    ') IS=.TRUE.
          IF(WC.EQ.'SGN     ') IS=.TRUE.
          IF(WC.EQ.'RECIP   ') IS=.TRUE.
          IF(WC.EQ.'INTGR   ') IS=.TRUE.
          IF(WC.EQ.'FRAC    ') IS=.TRUE.
          IF(WC.EQ.'POW     ') IS=.TRUE.
          IF(WC.EQ.'STORE   ') IS=.TRUE.
          IF(WC.EQ.'RAND    ') IS=.TRUE.
          IF(WC.EQ.'MOD     ') IS=.TRUE.
          IF(WC.EQ.'CLSTREG ') IS=.TRUE.
          IF(WC.EQ.'STADD   ') IS=.TRUE.
          IF(WC.EQ.'STSUB   ') IS=.TRUE.
          IF(WC.EQ.'MEAN    ') IS=.TRUE.
          IF(WC.EQ.'STDEV   ') IS=.TRUE.
          IF(WC.EQ.'RTD     ') IS=.TRUE.
          IF(WC.EQ.'DTR     ') IS=.TRUE.
          IF(WC.EQ.'ATAN2   ') IS=.TRUE.
          IF(WC.EQ.'J1      ') IS=.TRUE.
          IF(WC.EQ.'PREAD   ') IS=.TRUE.
          IF(WC.EQ.'ATON    ') IS=.TRUE.
          IF(WC.EQ.'STOAX   ') IS=.TRUE.
          IF(WC.EQ.'ARCL    ') IS=.TRUE.
          IF(WC.EQ.'AWRITE  ') IS=.TRUE.
          IF(WC.EQ.'ASTO    ') IS=.TRUE.
          IF(WC.EQ.'CLASTO  ') IS=.TRUE.
          IF(WC.EQ.'SHOW    ') IS=.TRUE.
          IF(WC.EQ.'ENT     ') IS=.TRUE.
          IF(WC.EQ.'ENTI    ') IS=.TRUE.
          IF(WC.EQ.'ENTC    ') IS=.TRUE.
          IF(WC.EQ.'PULL    ') IS=.TRUE.
          IF(WC.EQ.'IPULL   ') IS=.TRUE.
          IF(WC.EQ.'CPULL   ') IS=.TRUE.
          IF(WC.EQ.'RUP     ') IS=.TRUE.
          IF(WC.EQ.'IRUP    ') IS=.TRUE.
          IF(WC.EQ.'CRUP    ') IS=.TRUE.
          IF(WC.EQ.'RDN     ') IS=.TRUE.
          IF(WC.EQ.'IRDN    ') IS=.TRUE.
          IF(WC.EQ.'CRDN    ') IS=.TRUE.
          IF(WC.EQ.'X-Y     ') IS=.TRUE.
          IF(WC.EQ.'IX-IY   ') IS=.TRUE.
          IF(WC.EQ.'+       ') IS=.TRUE.
          IF(WC.EQ.'-       ') IS=.TRUE.
          IF(WC.EQ.'*       ') IS=.TRUE.
          IF(WC.EQ.'/       ') IS=.TRUE.
          IF(WC.EQ.'I+      ') IS=.TRUE.
          IF(WC.EQ.'I-      ') IS=.TRUE.
          IF(WC.EQ.'I*      ') IS=.TRUE.
          IF(WC.EQ.'I/      ') IS=.TRUE.
          IF(WC.EQ.'C+      ') IS=.TRUE.
          IF(WC.EQ.'C-      ') IS=.TRUE.
          IF(WC.EQ.'C*      ') IS=.TRUE.
          IF(WC.EQ.'C/      ') IS=.TRUE.
          IF(WC.EQ.'Y**X    ') IS=.TRUE.
          IF(WC.EQ.'IY**IX  ') IS=.TRUE.
          IF(WC.EQ.'CY**CX  ') IS=.TRUE.
          IF(WC.EQ.'P-R     ') IS=.TRUE.
          IF(WC.EQ.'R-P     ') IS=.TRUE.
          IF(WC.EQ.'CYL-R   ') IS=.TRUE.
          IF(WC.EQ.'R-CYL   ') IS=.TRUE.
          IF(WC.EQ.'SP-R    ') IS=.TRUE.
          IF(WC.EQ.'R-SP    ') IS=.TRUE.
          IF(WC.EQ.'RE-IM   ') IS=.TRUE.
          IF(WC.EQ.'IM-RE   ') IS=.TRUE.
          IF(WC.EQ.'H-HMS   ') IS=.TRUE.
          IF(WC.EQ.'HMS-H   ') IS=.TRUE.
          IF(WC.EQ.'IN-MM   ') IS=.TRUE.
          IF(WC.EQ.'IN-CM   ') IS=.TRUE.
          IF(WC.EQ.'IN-M    ') IS=.TRUE.
          IF(WC.EQ.'MM-IN   ') IS=.TRUE.
          IF(WC.EQ.'CM-IN   ') IS=.TRUE.
          IF(WC.EQ.'M-IN    ') IS=.TRUE.
          IF(WC.EQ.'ARCL')        IS=.TRUE.
          IF(WC.EQ.'STOAX')       IS=.TRUE.
          IF(WC.EQ.'SET')         IS=.TRUE.
          IF(WC.EQ.'W1')          IS=.TRUE.
          IF(WC.EQ.'W2')          IS=.TRUE.
          IF(WC.EQ.'W3')          IS=.TRUE.
          IF(WC.EQ.'W4')          IS=.TRUE.
          IF(WC.EQ.'W5')          IS=.TRUE.
          RETURN
      END


C SUB CONT1.FOR
      SUBROUTINE CONT1(STP)
C
C       THIS CHECKS FOR INVALID COMMANDS ISSUED FROM THE
C       CMD LEVEL WHEN SPECT IS NOT BEING USED AND THE COMMANDS
C       ARE NOT COMMING FROM WITHIN A MACRO.
C
          IMPLICIT NONE
C
          LOGICAL IS,STP
C
          INCLUDE 'datmai.inc'
C
          IS=.FALSE.
          IF(WC.EQ.'START')       IS=.TRUE.
          IF(WC.EQ.'WAVLN')       IS=.TRUE.
          IF(WC.EQ.'INT')         IS=.TRUE.
          IF(WC.EQ.'DIRECT')      IS=.TRUE.
C       IF(WC.EQ.'LOADPHOT')    IS=.TRUE.
C       IF(WC.EQ.'LOADSCOT')    IS=.TRUE.
          IF(WC.EQ.'INSERT')      IS=.TRUE.
          IF(WC.EQ.'DROP')        IS=.TRUE.
          IF(WC.EQ.'DELETE')      IS=.TRUE.
          IF(WC.EQ.'GETFILE')     IS=.TRUE.
          IF(WC.EQ.'BLACKBDY')    IS=.TRUE.
          IF(WC.EQ.'PHOTOPIC')    IS=.TRUE.
          IF(WC.EQ.'SCOTOPIC')    IS=.TRUE.
          IF(WC.EQ.'PUT')         IS=.TRUE.
          IF(WC.EQ.'LIST')        IS=.TRUE.
          IF(WC.EQ.'RENAME')      IS=.TRUE.
          IF(WC.EQ.'PUNCH')       IS=.TRUE.
          IF(WC.EQ.'SPRINT')      IS=.TRUE.
          IF(WC.EQ.'INTER')       IS=.TRUE.
          IF(WC.EQ.'NARCIN')      IS=.TRUE.
          IF(WC.EQ.'FLNAME')      IS=.TRUE.
          IF(WC.EQ.'ENDTABLE')    IS=.TRUE.
          IF(WC.EQ.'NARC')        IS=.TRUE.
          IF(WC.EQ.'EOS')         IS=.TRUE.
          IF(WC.EQ.'DATA')        IS=.TRUE.
          IF(WC.EQ.'CUME')        IS=.TRUE.
          IF(WC.EQ.'WFACTOR')     IS=.TRUE.
          IF(WC.EQ.'WORK')        IS=.TRUE.
          IF(WC.EQ.'PTABLE')      IS=.TRUE.
          IF(WC.EQ.'DIR')         IS=.TRUE.
          IF(WC.EQ.'FILE')        IS=.TRUE.
          IF(WC.EQ.'NAME')        IS=.TRUE.
          IF(WC.EQ.'NSUB')        IS=.TRUE.
          IF(WC.EQ.'QSUB')        IS=.TRUE.
          IF(WC.EQ.'SSUB')        IS=.TRUE.
          IF(WC.EQ.'QRSUB')       IS=.TRUE.
          IF(WC.EQ.'CSUB')        IS=.TRUE.
          IF(WC.EQ.'CRSUB')       IS=.TRUE.
          IF(WC.EQ.'PUTR')        IS=.TRUE.
          IF(WC.EQ.'TRACE')       IS=.TRUE.
          IF(WC.EQ.'ACCSUB')      IS=.TRUE.
          IF(WC.EQ.'RETURN')      IS=.TRUE.
          IF(WC.EQ.'BP')          IS=.TRUE.
          IF(WC.EQ.'BRQ')         IS=.TRUE.
          IF(WC.EQ.'BPOS')        IS=.TRUE.
          IF(WC.EQ.'BRDQ')        IS=.TRUE.
          IF(WC.EQ.'BRDF1')       IS=.TRUE.
          IF(WC.EQ.'BRDF2')       IS=.TRUE.
          IF(WC.EQ.'BRDF3')       IS=.TRUE.
          IF(WC.EQ.'BRDF4')       IS=.TRUE.
          IF(WC.EQ.'BRDF5')       IS=.TRUE.
          IF(WC.EQ.'BNEG')        IS=.TRUE.
          IF(WC.EQ.'BZE')         IS=.TRUE.
          IF(WC.EQ.'IF(X>0)')     IS=.TRUE.
          IF(WC.EQ.'IF(X<0)')     IS=.TRUE.
          IF(WC.EQ.'IF(X=0)')     IS=.TRUE.
          IF(WC.EQ.'IF(X>Y)')     IS=.TRUE.
          IF(WC.EQ.'IF(X<Y)')     IS=.TRUE.
          IF(WC.EQ.'IF(X=Y)')     IS=.TRUE.
          IF(WC.EQ.'BRI')         IS=.TRUE.
          IF(WC.EQ.'BRJ')         IS=.TRUE.
          IF(WC.EQ.'BRK')         IS=.TRUE.
          IF(WC.EQ.'BRL')         IS=.TRUE.
          IF(WC.EQ.'BRM')         IS=.TRUE.
          IF(WC.EQ.'BRM')         IS=.TRUE.
          IF(WC.EQ.'BRU')         IS=.TRUE.
          IF(WC.EQ.'BRANCH')      IS=.TRUE.
          IF(WC.EQ.'BRT')         IS=.TRUE.
          IF(WC.EQ.'BRF')         IS=.TRUE.
          IF(WC.EQ.'BRERR')       IS=.TRUE.
          IF(WC.EQ.'PAUSE')       IS=.TRUE.
          IF(WC.EQ.'SSTEP')       IS=.TRUE.
          IF(IS) THEN
              OUTLYNE='INVALID CMD LEVEL COMMAND'
              CALL SHOWIT(1)
              STP=.TRUE.
              CALL MACFAL
              RETURN
          ELSE
              STP=.FALSE.
          END IF
          RETURN
      END


C SUB CONT2.FOR
      SUBROUTINE CONT2(STP)
C
C       THIS CHECKS FOR INVALID SPECT COMMANDS GIVEN FROM WITHIN THE
C       SPECTRAL SUBFILE WHEN NOT COMMING FROM WITHIN A MACRO.
C
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
C
          LOGICAL STP,IS
C
          IS=.FALSE.
          IF(WC.EQ.'EXIT')        IS=.TRUE.
          IF(WC.EQ.'EXI')         IS=.TRUE.
          IF(WC.EQ.'LENS')        IS=.TRUE.
          IF(WC.EQ.'CONFIGS')     IS=.TRUE.
          IF(WC.EQ.'CONFIG ')     IS=.TRUE.
          IF(WC.EQ.'SPSRF')       IS=.TRUE.
          IF(WC.EQ.'SPFIT')       IS=.TRUE.
          IF(WC.EQ.'SPECT')       IS=.TRUE.
          IF(WC.EQ.'MACRO')       IS=.TRUE.
          IF(WC.EQ.'LMEDIT')      IS=.TRUE.
          IF(WC.EQ.'UPDATE')      IS=.TRUE.
          IF(WC.EQ.'U')           IS=.TRUE.
          IF(WC.EQ.'RAYSET')      IS=.TRUE.
          IF(WC.EQ.'DEP')         IS=.TRUE.
          IF(WC.EQ.'FOE')         IS=.TRUE.
          IF(WC.EQ.'VARIABLE')    IS=.TRUE.
          IF(WC.EQ.'VARI')        IS=.TRUE.
          IF(WC.EQ.'TVAR')        IS=.TRUE.
          IF(WC.EQ.'TOPER')       IS=.TRUE.
          IF(WC.EQ.'FOCRIT')      IS=.TRUE.
          IF(WC.EQ.'COMPVAR')     IS=.TRUE.
          IF(WC.EQ.'MERIT')       IS=.TRUE.
          IF(WC.EQ.'LAYOUT')      IS=.TRUE.
          IF(WC.EQ.'NSUB')        IS=.TRUE.
          IF(WC.EQ.'QSUB')        IS=.TRUE.
          IF(WC.EQ.'SSUB')        IS=.TRUE.
          IF(WC.EQ.'QRSUB')       IS=.TRUE.
          IF(WC.EQ.'CSUB')        IS=.TRUE.
          IF(WC.EQ.'CRSUB')       IS=.TRUE.
          IF(WC.EQ.'PUTR')        IS=.TRUE.
          IF(WC.EQ.'TRACE')       IS=.TRUE.
          IF(WC.EQ.'ACCSUB')      IS=.TRUE.
          IF(WC.EQ.'RETURN')      IS=.TRUE.
          IF(WC.EQ.'BP')          IS=.TRUE.
          IF(WC.EQ.'BRQ')         IS=.TRUE.
          IF(WC.EQ.'BPOS')        IS=.TRUE.
          IF(WC.EQ.'BRDQ')        IS=.TRUE.
          IF(WC.EQ.'BRDF1')       IS=.TRUE.
          IF(WC.EQ.'BRDF2')       IS=.TRUE.
          IF(WC.EQ.'BRDF3')       IS=.TRUE.
          IF(WC.EQ.'BRDF4')       IS=.TRUE.
          IF(WC.EQ.'BRDF5')       IS=.TRUE.
          IF(WC.EQ.'BNEG')        IS=.TRUE.
          IF(WC.EQ.'BZE')         IS=.TRUE.
          IF(WC.EQ.'IF(X>0)')     IS=.TRUE.
          IF(WC.EQ.'IF(X<0)')     IS=.TRUE.
          IF(WC.EQ.'IF(X=0)')     IS=.TRUE.
          IF(WC.EQ.'IF(X>Y)')     IS=.TRUE.
          IF(WC.EQ.'IF(X<Y)')     IS=.TRUE.
          IF(WC.EQ.'IF(X=Y)')     IS=.TRUE.
          IF(WC.EQ.'BRI')         IS=.TRUE.
          IF(WC.EQ.'BRJ')         IS=.TRUE.
          IF(WC.EQ.'BRK')         IS=.TRUE.
          IF(WC.EQ.'BRL')         IS=.TRUE.
          IF(WC.EQ.'BRM')         IS=.TRUE.
          IF(WC.EQ.'BRN')         IS=.TRUE.
          IF(WC.EQ.'BRU')         IS=.TRUE.
          IF(WC.EQ.'BRANCH')      IS=.TRUE.
          IF(WC.EQ.'BRT')         IS=.TRUE.
          IF(WC.EQ.'BRF')         IS=.TRUE.
          IF(WC.EQ.'BRERR')       IS=.TRUE.
          IF(WC.EQ.'PAUSE')       IS=.TRUE.
          IF(WC.EQ.'SSTEP')       IS=.TRUE.
          IF(IS) THEN
              OUTLYNE='INVALID SPECT LEVEL COMMAND'
              CALL SHOWIT(1)
              STP=.TRUE.
              CALL MACFAL
              RETURN
          ELSE
              STP=.FALSE.
          END IF
          RETURN
      END
C       FIFTH SET OF UTILTIY ROUTINES GO HERE

C SUB CV2PRG.FOR
      SUBROUTINE CV2PRG
C
          IMPLICIT NONE
C
          CHARACTER CVFILENAME*141,KDPFILENAME*80,CV_INPUT_STRING*1024
C
          CHARACTER TEMPA*1024,TEMPB*1024,CVA*23,THA*23,AINDEX*8
     1    ,GLASSA*40,VALA*23,AA23*23,SUMSTRING*1024,OUTLYNE2*132
     2    ,TEMPC*1024,BL1024*1024,TOR*2,TIL*1,CYL*1,TEMPER*1024
     3    ,FIELDTYPE*3,AAS*1,CLTYPE*1,COTYPE*1,TEMPCC*1024,AVNUMB*7
C
          LOGICAL EXIS37,SEMI,ADD,RADON,OLDADD
C
          LOGICAL CVERROR
C
          LOGICAL DOWV,DOWV2,CLAP1,CLAP2,CLDX,CLDY,CLTILT
C
          LOGICAL COBS1,COBS2,COBX,COBY,COTILT
C
          INTEGER I,CVFILENAMELENGTH,KDPFILENAMELENGTH,N,NPERIOD,ALLOERR
C
          INTEGER J,K,L,NADD,II,STRINGEND,WDEXIS(1:100),M
C
          INTEGER J1,J2,IVALV,IWD(1:100),SURFER
C
          REAL*8 CV,TH,VALV,WD(1:100),MAX,CL1,CL2,CL3,CL4,CL5
C
          REAL*8 CO1,CO2,CO3,CO4,CO5
C
          DIMENSION TEMPA(:),TEMPB(:),TEMPC(:)
C
          ALLOCATABLE :: TEMPA,TEMPB,TEMPC
C
          LOGICAL HOE
          INTEGER HOESUR
          CHARACTER*1 HV1,HV2
          REAL*8 HX1,HY1,HZ1,HX2,HY2,HZ2,HWL,HOR
          DIMENSION HV1(:),HV2(:),HX1(:),HY1(:),HZ1(:),HX2(:),HY2(:),
     1    HZ2(:),HOESUR(:),HOE(:),HWL(:),HOR(:)
          ALLOCATABLE :: HWL,HV1,HV2,HX1,HY1,HZ1,HX2,HY2,
     1    HZ2,HOESUR,HOE,HOR
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          SURFER=0
C
          TOR='? '
          TIL='?'
          CYL='?'
          AAS='?'
          CLTYPE='?'
          CLAP1=.FALSE.
          CLAP2=.FALSE.
          CLDX=.FALSE.
          CLDY=.FALSE.
          CLTILT=.FALSE.
          CL1=0.0D0
          CL2=0.0D0
          CL3=0.0D0
          CL4=0.0D0
          CL5=0.0D0
          COTYPE='?'
          COBS1=.FALSE.
          COBS2=.FALSE.
          COBX=.FALSE.
          COBY=.FALSE.
          COTILT=.FALSE.
          CO1=0.0D0
          CO2=0.0D0
          CO3=0.0D0
          CO4=0.0D0
          CO5=0.0D0
C
C       THIS SUBROUTINE IS CALLED CONVERT A FILE FROM CODE V TO PRG
C     LENS INPUT FORMAT
C
          AA23='                       '
          BL1024=AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1         //AA//'    '
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"CV2PRG" CONVERTS THE NAMED FILE (STRING INPUT)'
              CALL SHOWIT(1)
              OUTLYNE='FROM CODE V TO PROGRAM FORMAT'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"CV2PRG" TAKES NO NUMERIC OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.0) THEN
              OUTLYNE='"CV2PRG" REQUIRES EXPLICIT STRING (FILE NAME) INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          CVFILENAME=trim(HOME)//"LENSES/"//WS
          DO I=80,1,-1
              IF(CVFILENAME(I:I).NE.' ') THEN
                  N=I
                  GO TO 10
              END IF
          END DO
          OUTLYNE='INVALID (ZERO LENGTH) CODE V FILE NAME'
          CALL SHOWIT(1)
          OUTLYNE='NO ACTION TAKEN'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
 10       CONTINUE
          CVFILENAMELENGTH=N
          EXIS37=.FALSE.
          INQUIRE(FILE=CVFILENAME(1:N),EXIST=EXIS37)
          IF(.NOT.EXIS37) THEN
              OUTLYNE='NO CODE V INPUT FILE EXISTS TO READ'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     FILES EXISTS, CREATE PROGRAM FILE NAME
          NPERIOD=9
          DO I=1,80
              IF(CVFILENAME(I:I).EQ.'.') THEN
                  NPERIOD=I
                  GO TO 20
              END IF
          END DO
 20       CONTINUE
          IF(NPERIOD.GT.9) NPERIOD=9
          KDPFILENAME=CVFILENAME(1:NPERIOD)//'DAT'
          KDPFILENAMELENGTH=NPERIOD+4
C     OPEN THE CVFILENAME TO READ IT AS ASCII
C
          DEALLOCATE (TEMPA,TEMPB,TEMPC
     1    ,STAT=ALLOERR)
          DEALLOCATE (
     1    HWL,
     1    HV1,
     1    HV2,
     1    HX1,
     1    HY1,
     1    HZ1,
     1    HX2,
     1    HY2,
     1    HZ2,
     1    HOE,
     1    HOR,
     1    HOESUR,STAT=ALLOERR)
          ALLOCATE (TEMPA(1:5000),TEMPB(1:5000),TEMPC(1:5000)
     1    ,STAT=ALLOERR)
          ALLOCATE (
     1    HWL(0:MAXSUR),
     1    HV1(0:MAXSUR),
     1    HV2(0:MAXSUR),
     1    HX1(0:MAXSUR),
     1    HY1(0:MAXSUR),
     1    HZ1(0:MAXSUR),
     1    HX2(0:MAXSUR),
     1    HY2(0:MAXSUR),
     1    HZ2(0:MAXSUR),
     1    HOE(0:MAXSUR),
     1    HOR(0:MAXSUR),
     1    HOESUR(0:MAXSUR),STAT=ALLOERR)
          I=MAXSUR
          HOE(0:I)=.FALSE.
          HOESUR(0:I)=-1
          TEMPA(1:5000)=BL1024
          TEMPB(1:5000)=BL1024
          TEMPC(1:5000)=BL1024
C
C     CLOSE IT IF OPEN AND KEEP IY
          CALL CLOSE_FILE(37,1)
          CALL CLOSE_FILE(38,0)
          OPEN(UNIT=37,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=CVFILENAME(1:CVFILENAMELENGTH)
     2    ,STATUS='UNKNOWN')
          OPEN(UNIT=38,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'CONVERT.ERR'
     2    ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(38,0)
          OPEN(UNIT=38,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'CONVERT.ERR'
     2    ,STATUS='UNKNOWN')
          II=1
          ADD=.FALSE.
          OLDADD=.FALSE.
          DO I=1,5000
C     READ A LINE OF THE FILE AS A 132 CHARACTER, CHARACTER
              CV_INPUT_STRING=BL1024
              READ(37,100,ERR=75,END=76) CV_INPUT_STRING(1:132)
C     IS THERE A CONTINUATION MARK AT THE END ?
C     REMEMBER IF ADD WAS ON LAST TIME
              OLDADD=ADD
              DO J=1024,1,-1
                  ADD=.FALSE.
                  IF(CV_INPUT_STRING(J:J).EQ.'&') THEN
C     YES
                      ADD=.TRUE.
                      NADD=J
                      GO TO 11
                  ELSE
                  END IF
              END DO
 11           CONTINUE
              IF(.NOT.ADD.AND..NOT.OLDADD) THEN
C     NOTHING TO ADD, WRITE A TEMPA VALUE
                  TEMPA(II)(1:132)=CV_INPUT_STRING(1:132)
C     BLANK OUT ALL &
                  DO J=1024,1,-1
                      IF(TEMPA(II)(J:J).EQ.'&') TEMPA(II)(J:J)=' '
                  END DO
                  II=II+1
                  ADD=.FALSE.
                  OLDADD=.FALSE.
              END IF
C
              IF(ADD.AND..NOT.OLDADD) THEN
C     THEN WRITE AN INITIAL SUMSTRING AND GET ITS LENGTH
                  SUMSTRING(1:1024)=CV_INPUT_STRING(1:1024)
                  DO J=1024,1,-1
                      IF(SUMSTRING(J:J).NE.' ') THEN
                          STRINGEND=J
                          GO TO 15
                      END IF
                  END DO
 15               CONTINUE
              END IF
              IF(.NOT.ADD.AND.OLDADD) THEN
C     ADD, THEN WRITE A TEMPA VALUE
                  IF(STRINGEND.LT.1024) THEN
                      SUMSTRING(1:1024)=SUMSTRING(1:STRINGEND)//
     1                CV_INPUT_STRING(1:(1024-STRINGEND))
                  END IF
C     GET THE NEW STRINGEND
                  DO J=1024,1,-1
                      IF(SUMSTRING(J:J).NE.' ') THEN
                          STRINGEND=J
                          GO TO 25
                      END IF
                  END DO
 25               CONTINUE
                  TEMPA(II)(1:1024)=SUMSTRING(1:1024)
C     BLANK OUT ALL &
                  DO J=1024,1,-1
                      IF(TEMPA(II)(J:J).EQ.'&') TEMPA(II)(J:J)=' '
                  END DO
                  II=II+1
                  ADD=.FALSE.
                  OLDADD=.FALSE.
              END IF
              IF(ADD.AND.OLDADD) THEN
C     ADD, TO MAKE A NEW SUMSTRING
                  IF(STRINGEND.LT.1024) THEN
                      SUMSTRING(1:1024)=SUMSTRING(1:STRINGEND)//
     1                CV_INPUT_STRING(1:(1024-STRINGEND))
                  END IF
C     GET THE NEW STRINGEND
                  DO J=1024,1,-1
                      IF(SUMSTRING(J:J).NE.' ') THEN
                          STRINGEND=J
                          GO TO 30
                      END IF
                  END DO
 30               CONTINUE
              END IF
          END DO
          GO TO 76
 75       CONTINUE
          OUTLYNE='ERROR READING CODE V INPUT FILE'
          CALL SHOWIT(1)
          OUTLYNE='NO FILE CONVERSION PERFORMED'
          CALL SHOWIT(1)
          CALL MACFAL
          CALL CLOSE_FILE(37,1)
          CALL CLOSE_FILE(38,0)
          DEALLOCATE (TEMPA,TEMPB,TEMPC,STAT=ALLOERR)
          DEALLOCATE (HOE,HOESUR,HV1,HV2,HX1,HY1,HZ1
     1    ,HWL,HX2,HY2,HZ2,HOR,STAT=ALLOERR)
          RETURN
 76       CONTINUE
C     NOW REMOVE ALL VIRTUAL RETURNS (SEMI-COLONS) AND LOAD TEMPB
C     CYCLE THROUGH THE II-1 ENTRIES IN TEMPA
          K=1
          DO I=1,II-1
C     ARE THERE ANY SEMICOLONS
              L=1
              SEMI=.FALSE.
              DO J=1,1024
                  IF(TEMPA(I)(J:J).EQ.';') SEMI=.TRUE.
              END DO
              IF(SEMI) THEN
C     MULTIPLE COMMANDS PER LINE
                  DO J=1,1024
                      IF(TEMPA(I)(J:J).EQ.';') THEN
                          TEMPB(K)=TEMPA(I)(L:J-1)
                          L=J+1
                          K=K+1
                      END IF
                      IF(J.EQ.1024) THEN
                          TEMPB(K)=TEMPA(I)(L:1024)
                          K=K+1
                          L=J+1
                      END IF
                  END DO
              ELSE
C     ONLY ONE INSTRUCTION ON THE LINE
                  TEMPB(K)=TEMPA(I)
                  K=K+1
              END IF
          END DO
C     NOW REMOVE ALL EVIL CHARACTERS AND LEADING BLANKS
          DO I=1,K-1
              DO J=1,1024
                  IF(ICHAR(TEMPB(I)(J:J)).LT.32.OR.
     1            ICHAR(TEMPB(I)(J:J)).GT.126) TEMPB(I)(J:J)=' '
              END DO
              DO J=1,1024
                  IF(TEMPB(I)(1:1).EQ.' ') TEMPB(I)(1:1024)=TEMPB(I)(2:1024)//' '
              END DO
          END DO
          L=1
          DO I=1,K-1
              IF(TEMPB(I)(1:20).NE.'                    ') THEN
                  TEMPC(L)(1:1024)=TEMPB(I)(1:1024)
                  L=L+1
              END IF
          END DO
          L=L-1
C
C     INSTRUCTION TRASNLATION
C
 4000     FORMAT(' THE FOLLOWING CODE-V COMMAND DID NOT TRANSLATE:')
          WRITE(38,4000)
          DO I=1,L
              TEMPCC(1:1024)=TEMPC(I)(1:1024)
C     RDM
              IF(TEMPC(I)(1:3).EQ.'RDM') THEN
                  IF(TEMPC(I)(1:5).EQ.'RDM N') RADON=.FALSE.
                  IF(TEMPC(I)(1:5).NE.'RDM N') RADON=.TRUE.
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     SPH OR CON
              IF(TEMPC(I)(1:3).EQ.'SPH'.OR.
     1        TEMPC(I)(1:3).EQ.'CON'.OR.
     1        TEMPC(I)(1:3).EQ.'ASP') THEN
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     LEN
              IF(TEMPC(I)(1:3).EQ.'LEN') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='LENS'
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     TITLE
              IF(TEMPC(I)(1:5).EQ.'TITLE'.OR.TEMPC(I)(1:3).EQ.'TIT') THEN
                  DO J=1024,1,-1
                      IF(TEMPC(I)(J:J).EQ.'''') THEN
                          J2=J
                          GO TO 201
                      END IF
                  END DO
 201              CONTINUE
                  DO J=1,1024
                      IF(TEMPC(I)(J:J).EQ.'''') THEN
                          J1=J
                          GO TO 102
                      END IF
                  END DO
 102              CONTINUE
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='LI '//TEMPC(I)(J1+1:J2-1)
                  IF(TEMPC(I)(J1+1:J2-1).NE.BL1024(J1+1:J2-1)) THEN
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                  END IF
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     INI
              IF(TEMPC(I)(1:3).EQ.'INI') THEN
                  DO J=1024,1,-1
                      IF(TEMPC(I)(J:J).EQ.'''') THEN
                          J2=J
                          GO TO 202
                      END IF
                  END DO
 202              CONTINUE
                  DO J=1,1024
                      IF(TEMPC(I)(J:J).EQ.'''') THEN
                          J1=J
                          GO TO 121
                      END IF
                  END DO
 121              CONTINUE
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='INI '//TEMPC(I)(J1+1:J2-1)
                  IF(TEMPC(I)(J1+1:J2-1).NE.BL1024(J1+1:J2-1)) THEN
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                  END IF
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     EPD
              IF(TEMPC(I)(1:3).EQ.'EPD') THEN
                  TEMPER=TEMPC(I)(4:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  CALL ATODCODEV(VALA,VALV,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE2,2001) VALV/2.0D0
 2001             FORMAT('SAY,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     NAO
              IF(TEMPC(I)(1:3).EQ.'NAO') THEN
                  TEMPER=TEMPC(I)(4:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  CALL ATODCODEV(VALA,VALV,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE2,2002) VALV
 2002             FORMAT('NAO,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     REF
              IF(TEMPC(I)(1:3).EQ.'REF') THEN
                  TEMPER=TEMPC(I)(4:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  CALL ATOICODEV(VALA,IVALV,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE2,2003) IVALV
 2003             FORMAT('CW,',I2)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     YAN,XAN,YOB,XOB,XIM,YIM,XRI,YRI
              IF(TEMPC(I)(1:3).EQ.'YAN'
     1        .OR.TEMPC(I)(1:3).EQ.'XAN'
     1        .OR.TEMPC(I)(1:3).EQ.'XIM'
     1        .OR.TEMPC(I)(1:3).EQ.'YIM'
     1        .OR.TEMPC(I)(1:3).EQ.'XRI'
     1        .OR.TEMPC(I)(1:3).EQ.'YRI'
     1        .OR.TEMPC(I)(1:3).EQ.'YOB'
     1        .OR.TEMPC(I)(1:3).EQ.'XOB') THEN
                  FIELDTYPE='   '
                  IF(TEMPC(I)(1:3).EQ.'XAN') FIELDTYPE='XAN'
                  IF(TEMPC(I)(1:3).EQ.'YAN') FIELDTYPE='YAN'
                  IF(TEMPC(I)(1:3).EQ.'XOB') FIELDTYPE='XOB'
                  IF(TEMPC(I)(1:3).EQ.'YOB') FIELDTYPE='YOB'
                  IF(TEMPC(I)(1:3).EQ.'XIM') FIELDTYPE='XIM'
                  IF(TEMPC(I)(1:3).EQ.'YIM') FIELDTYPE='YIM'
                  IF(TEMPC(I)(1:3).EQ.'XRI') FIELDTYPE='XRI'
                  IF(TEMPC(I)(1:3).EQ.'YRI') FIELDTYPE='YRI'
                  CALL ONEBLANK(3,TEMPC(I)(1:1024))
                  IF(FIELDTYPE.EQ.'XAN'.OR.FIELDTYPE.EQ.'YAN'.OR.
     1            FIELDTYPE.EQ.'XOB'.OR.FIELDTYPE.EQ.'YOB') THEN
C     MAX FIELD POS CONVERT TO SCY. SCY FANG, SCX AND SCX FANG
C     BREAK OUT UP TO 25 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
C     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
C     THEN FILD LARGEST VALUE
C     AS NEEDED.
C     STRIP OFF 3 CHARACTERS
                      TEMPC(I)(1:1024)=TEMPC(I)(4:1024)//'   '
C     STRIP LEADING BLANKS
                      DO J=1,1024
                          IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                      END DO
                      CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),25,CVERROR)
                      IF(CVERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
                      MAX=-1000.0D0
                      DO J=1,25
                          IF(DABS(WD(J)).GT.MAX) MAX=DABS(WD(J))
                      END DO
                      IF(FIELDTYPE.EQ.'YAN') THEN
                          WRITE(OUTLYNE2,2004) MAX
 2004                     FORMAT('SCY FANG,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          TEMPC(I)(1:1024)=BL1024(1:1024)
                      END IF
                      IF(FIELDTYPE.EQ.'XAN') THEN
                          WRITE(OUTLYNE2,2005) MAX
 2005                     FORMAT('SCX FANG,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          TEMPC(I)(1:1024)=BL1024(1:1024)
                      END IF
                      IF(FIELDTYPE.EQ.'YOB') THEN
                          WRITE(OUTLYNE2,2006) MAX
 2006                     FORMAT('SCY,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          TEMPC(I)(1:1024)=BL1024(1:1024)
                      END IF
                      IF(FIELDTYPE.EQ.'XOB') THEN
                          WRITE(OUTLYNE2,2007) MAX
 2007                     FORMAT('SCX,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          TEMPC(I)(1:1024)=BL1024(1:1024)
                      END IF
                      GO TO 8888
                  END IF
              END IF
              IF(FIELDTYPE.EQ.'XIM'.OR.FIELDTYPE.EQ.'YIM'.OR.
     1        FIELDTYPE.EQ.'XRI'.OR.FIELDTYPE.EQ.'YRI') THEN
C     IMAGE HT SPEC
C     STRIP OFF 3 CHARACTERS
                  TEMPC(I)(1:1024)=TEMPC(I)(4:1024)//'   '
C     STRIP LEADING BLANKS
                  DO J=1,1024
                      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                  END DO
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),25,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  MAX=-1000.0D0
                  DO J=1,25
                      IF(DABS(WD(J)).GT.MAX) MAX=DABS(WD(J))
                  END DO
                  IF(FIELDTYPE.EQ.'XIM') THEN
                      WRITE(OUTLYNE2,4004) MAX
 4004                 FORMAT('PXIM,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  IF(FIELDTYPE.EQ.'YIM') THEN
                      WRITE(OUTLYNE2,4005) MAX
 4005                 FORMAT('PYIM,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  IF(FIELDTYPE.EQ.'XRI') THEN
                      WRITE(OUTLYNE2,4006) MAX
 4006                 FORMAT('RXIM,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  IF(FIELDTYPE.EQ.'YRI') THEN
                      WRITE(OUTLYNE2,4007) MAX
 4007                 FORMAT('RYIM,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  GO TO 8888
              END IF
C     WL
              IF(TEMPC(I)(1:2).EQ.'WL') THEN
                  CALL ONEBLANK(2,TEMPC(I)(1:1024))
C     BREAK OUT UP TO 10 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
C     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
C     THEN CONVERT THEM TO NUMERIC WORDS AND ISSUE THE WV AND WV2 COMMANDS
C     AS NEEDED.
C     STRIP OFF WD
                  TEMPC(I)(1:1024)=TEMPC(I)(3:1024)//'  '
C     STRIP LEADING BLANKS
                  DO J=1,1024
                      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                  END DO
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),10,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  DOWV=.FALSE.
                  DOWV2=.FALSE.
                  DO J=1,5
                      IF(WDEXIS(J).NE.0) DOWV =.TRUE.
                  END DO
                  DO J=6,10
                      IF(WDEXIS(J).NE.0) DOWV2 =.TRUE.
                  END DO
                  IF(DOWV) THEN
                      WRITE(OUTLYNE2,2008) WD(1)/1000.0D0,WD(2)/1000.0D0,WD(3)/1000.0D0
     1                ,WD(4)/1000.0D0,WD(5)/1000.0D0
 2008                 FORMAT('WV,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1                ,D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  IF(DOWV2) THEN
                      WRITE(OUTLYNE2,2009) WD(6)/1000.0D0,WD(7)/1000.0D0,WD(8)/1000.0D0
     1                ,WD(9)/1000.0D0,WD(10)/1000.0D0
 2009                 FORMAT('WV2,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1                ,D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  GO TO 8888
              END IF
C     WTW
              IF(TEMPC(I)(1:3).EQ.'WTW') THEN
                  CALL ONEBLANK(3,TEMPC(I)(1:1024))
C     BREAK OUT UP TO 10 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
C     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
C     THEN CONVERT THEM TO NUMERIC WORDS AND ISSUE THE SPTWT/SPTWT2 COMMANDS
C     AS NEEDED.
C     STRIP OFF WTW
                  TEMPC(I)(1:1024)=TEMPC(I)(4:1024)//'   '
C     STRIP LEADING BLANKS
                  DO J=1,1024
                      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                  END DO
                  CALL IMULTIPROCESS(IWD,WDEXIS,TEMPC(I)(1:1024),10,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  DOWV=.FALSE.
                  DOWV2=.FALSE.
                  DO J=1,5
                      IF(WDEXIS(J).NE.0) DOWV =.TRUE.
                  END DO
                  DO J=6,10
                      IF(WDEXIS(J).NE.0) DOWV2 =.TRUE.
                  END DO
                  IF(DOWV) THEN
                      WRITE(OUTLYNE2,2010) WD(1),WD(2),WD(3),WD(4),WD(5)
 2010                 FORMAT('SPTWT,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1                ,D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  IF(DOWV2) THEN
                      WRITE(OUTLYNE2,2011) WD(6),WD(7),WD(8),WD(9),WD(10)
 2011                 FORMAT('SPTWT2,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1                ,D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  GO TO 8888
              END IF
C     TEL Y
              IF(TEMPC(I)(1:5).EQ.'TEL Y') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='TEL YES'
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     STO
              IF(TEMPC(I)(1:3).EQ.'STO') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='ASTOP'
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='REFS'
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CYL
C     CYL IS JUST AN INTERNAL SETTING
              IF(TEMPC(I)(1:3).EQ.'CYL') THEN
                  TOR='? '
                  CYL='C'
                  AAS='?'
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     YTO OR AAS
              IF(TEMPC(I)(1:3).EQ.'YTO'.OR.TEMPC(I)(1:3).EQ.'AAS') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='YTORIC'
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TOR='YT'
                  AAS='?'
                  CYL='?'
                  IF(TEMPC(I)(1:3).EQ.'AAS') AAS='Y'
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     XTO
              IF(TEMPC(I)(1:3).EQ.'XTO') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='XTORIC'
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TOR='XT'
                  AAS='?'
                  CYL='?'
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     DIM
              IF(TEMPC(I)(1:3).EQ.'DIM') THEN
                  IF(TEMPC(I)(7:7).EQ.'M') THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='UNITS MM'
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(7:7).EQ.'C') THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='UNITS CM'
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(7:7).EQ.'I') THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='UNITS IN'
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     PROCESS EACH LINE INTO A KDP COMMAND AND THEN DISPLAY IT
              IF(TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.' '.OR.
     1        TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.'I'.OR.
     1        TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.'O'.OR.
     1        TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.'0') THEN
                  IF(TEMPC(I)(1:2).NE.'SO'.AND.TEMPC(I)(1:2).NE.'S0') THEN
C
C     WRITE CLAP DATA, COBS DATA, THEN GLASS DATA
C     CLAP DATA
                      IF(CLTYPE.EQ.'C') THEN
                          WRITE(OUTLYNE2,2012) CL1,CL4,CL3,CL2
 2012                     FORMAT('CLAP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          CLTYPE='?'
                          CLAP1=.FALSE.
                          CLAP2=.FALSE.
                          CLDX=.FALSE.
                          CLDY=.FALSE.
                          CLTILT=.FALSE.
                          CL1=0.0D0
                          CL2=0.0D0
                          CL3=0.0D0
                          CL4=0.0D0
                          CL5=0.0D0
                      END IF
                      IF(CLTYPE.EQ.'R') THEN
                          WRITE(OUTLYNE2,2013) CL2,CL1,CL4,CL3
 2013                     FORMAT('CLAP RECT,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          CLTYPE='?'
                          CLAP1=.FALSE.
                          CLAP2=.FALSE.
                          CLDX=.FALSE.
                          CLDY=.FALSE.
                          CLTILT=.FALSE.
                          CL1=0.0D0
                          CL2=0.0D0
                          CL3=0.0D0
                          CL4=0.0D0
                          CL5=0.0D0
                      END IF
                      IF(CLTYPE.EQ.'E') THEN
                          WRITE(OUTLYNE2,2014) CL2,CL1,CL4,CL3
 2014                     FORMAT('CLAP ELIP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          CLTYPE='?'
                          CLAP1=.FALSE.
                          CLAP2=.FALSE.
                          CLDX=.FALSE.
                          CLDY=.FALSE.
                          CLTILT=.FALSE.
                          CL1=0.0D0
                          CL2=0.0D0
                          CL3=0.0D0
                          CL4=0.0D0
                          CL5=0.0D0
                      END IF
                      IF(CLTILT) THEN
                          WRITE(OUTLYNE2,2015) CL5
 2015                     FORMAT('CLAP TILT,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          CLTILT=.FALSE.
                      END IF
C     COBS DATA
                      IF(COTYPE.EQ.'C') THEN
                          WRITE(OUTLYNE2,4012) CO1,CO4,CO3
 4012                     FORMAT('COBS,',D23.15,',',D23.15,',',D23.15,',,,')
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          COTYPE='?'
                          COBS1=.FALSE.
                          COBS2=.FALSE.
                          COBX=.FALSE.
                          COBY=.FALSE.
                          CO1=0.0D0
                          CO2=0.0D0
                          CO3=0.0D0
                          CO4=0.0D0
                          CO5=0.0D0
                      END IF
                      IF(COTYPE.EQ.'R') THEN
                          WRITE(OUTLYNE2,4013) CO2,CO1,CO4,CO3
 4013                     FORMAT('COBS RECT,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          COTYPE='?'
                          COBS1=.FALSE.
                          COBS2=.FALSE.
                          COBX=.FALSE.
                          COBY=.FALSE.
                          CO1=0.0D0
                          CO2=0.0D0
                          CO3=0.0D0
                          CO4=0.0D0
                          CO5=0.0D0
                      END IF
                      IF(COTYPE.EQ.'E') THEN
                          WRITE(OUTLYNE2,4014) CO2,CO1,CO4,CO3
 4014                     FORMAT('COBS ELIP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          COTYPE='?'
                          COBS1=.FALSE.
                          COBS2=.FALSE.
                          COBX=.FALSE.
                          COBY=.FALSE.
                          CO1=0.0D0
                          CO2=0.0D0
                          CO3=0.0D0
                          CO4=0.0D0
                          CO5=0.0D0
                      END IF
                      IF(COTILT) THEN
                          WRITE(OUTLYNE2,4015) CO5
 4015                     FORMAT('COBS TILT,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          COTILT=.FALSE.
                      END IF
C     GLASS DATA
                      WRITE(OUTLYNE2,2016) GLASSA
 2016                 FORMAT(A40)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      SURFER=SURFER+1
                      HOESUR(SURFER)=-1
                      TOR='? '
                      TIL='?'
                      CYL='?'
                      AAS='?'
                  END IF
C     A SURFACE LINE, BREAK IT OUT
C     STRIP OFF TO THE START OF THE CURVATURE
                  IF(TEMPC(I)(1:2).EQ.'SO'.OR.
     1            TEMPC(I)(1:2).EQ.'S0'.OR.
     2            TEMPC(I)(1:2).EQ.'S '.OR.
     3            TEMPC(I)(1:2).EQ.'SI')
     4            TEMPC(I)(1:1024)=TEMPC(I)(3:1024)//'     '
C     REMOVE MORE LEADING BLANKS
                  DO M=1,1024
                      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                  END DO
C     STRIP OFF THE CURVATURE
                  DO J=1,1024
                      IF(TEMPC(I)(J:J).EQ.' ') THEN
                          CVA=AA(1:(23-J-1))//TEMPC(I)(1:J-1)
                          CALL ATODCODEV(CVA,CV,CVERROR)
                          IF(CVERROR) THEN
                              WRITE(38,4001) TEMPCC(1:78)
                              GO TO 8888
                          END IF
C     IF RADON THEN CODE V REPORTED RADIUS, CONVERT TO
C     CURVATURE ELSE DON'T AND DON'T CONVERT IF RADON AND RAD=0
                          IF(RADON.AND.CV.NE.0.0D0) CV=1.0D0/CV
                          TEMPC(I)(1:1024)=TEMPC(I)(J+1:1024)//BL1024(1:J)
                          GO TO 50
                      END IF
                  END DO
C     REMOVE SOME MORE LEADING SPACES
 50               DO J=1,1024
                      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                  END DO
C     STRIP OFF THE THICKNESS
                  DO J=1,1024
                      IF(TEMPC(I)(J:J).EQ.' ') THEN
                          THA=AA(1:(23-J-1))//TEMPC(I)(1:J-1)
                          CALL ATODCODEV(THA,TH,CVERROR)
                          IF(CVERROR) THEN
                              WRITE(38,4001) TEMPCC(1:78)
                              GO TO 8888
                          END IF
                          TEMPC(I)(1:1024)=TEMPC(I)(J+1:1024)//BL1024(1:J)
                          DO M=1,1024
                              IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                          END DO
                          CALL ATODCODEV(THA,TH,CVERROR)
                          IF(CVERROR) THEN
                              WRITE(38,4001) TEMPCC(1:78)
                              GO TO 8888
                          END IF
                          GO TO 60
                      END IF
                  END DO
 60               CONTINUE
C     NOW THE GLASS TYPE
C     ALL BLANKS, GLASS WAS AIR
                  IF(TEMPC(I)(1:1024).EQ.BL1024) THEN
                      GLASSA='AIR'
                      GO TO 7777
                  END IF
C     REMOVE BLANKS
                  DO M=1,1024
                      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                  END DO
C     DETERMINE GLASS NAME
                  DO J=1,1024
                      IF(TEMPC(I)(J:J).EQ.'_') THEN
                          IF(TEMPC(I)(J+1:J+1).EQ.'S'.AND.TEMPC(I)(1:4).EQ.'BAF2') THEN
                              GLASSA='MATL '//TEMPC(I)(1:J-1)
                          ELSE
                              GLASSA='GLA '//TEMPC(I)(1:J-1)
                          END IF
                          TEMPC(I)(1:1024)=BL1024(1:1024)
                          GO TO 7777
                      END IF
                  END DO
                  IF(TEMPC(I)(1:4).EQ.'AIR ') THEN
                      GLASSA='AIR'
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 7777
                  END IF
                  IF(TEMPC(I)(1:4).EQ.'REFL') THEN
                      GLASSA='REFL'
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 7777
                  END IF
C
C     CODE-V MODEL GLASS
C
                  IF(TEMPC(I)(1:1).EQ.'0'.OR.TEMPC(I)(1:1).EQ.'1'.OR.
     1            TEMPC(I)(1:1).EQ.'2'.OR.TEMPC(I)(1:1).EQ.'3'.OR.
     1            TEMPC(I)(1:1).EQ.'4'.OR.TEMPC(I)(1:1).EQ.'5'.OR.
     1            TEMPC(I)(1:1).EQ.'6'.OR.TEMPC(I)(1:1).EQ.'7'.OR.
     1            TEMPC(I)(1:1).EQ.'8'.OR.TEMPC(I)(1:1).EQ.'9') THEN
                      AINDEX='1.'//TEMPC(I)(1:6)
                      AVNUMB=TEMPC(I)(8:9)//'.'//TEMPC(I)(10:13)
                      GLASSA='MODEL G'//TEMPC(I)(1:3)//
     1                TEMPC(I)(8:10)//','//AINDEX//','//AVNUMB
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 7777
                  END IF
C       FALL THROUGH
                  GLASSA='GLASS '//TEMPC(I)(1:20)
 7777             CONTINUE
                  IF(.NOT.RADON) THEN
                      WRITE(OUTLYNE2,2017) CV
                  ELSE
                      IF(CV.EQ.0.0D0) THEN
                          WRITE(OUTLYNE2,2041) CV
                      ELSE
                          WRITE(OUTLYNE2,2041) 1.0D0/CV
                      END IF
                  END IF
 2017             FORMAT('CV,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  WRITE(OUTLYNE2,2018) TH
 2018             FORMAT('TH,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
              ELSE
C     NOT A SURFACE COMMAND
C     RESOLVE NON-SURFACE COMMANDS
              END IF
C
C     TILTS AND DECENTERS GO HERE
              IF(TEMPC(I)(1:3).EQ.'DAR') THEN
                  WRITE(OUTLYNE2,2019)
 2019             FORMAT('TILT DAR')
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TIL='T'
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
              IF(TEMPC(I)(1:3).EQ.'REV') THEN
                  WRITE(OUTLYNE2,9019)
 9019             FORMAT('TILT REV')
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TIL='T'
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
              IF(TEMPC(I)(1:3).EQ.'BEN') THEN
                  WRITE(OUTLYNE2,2020)
 2020             FORMAT('TILT BEN')
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TIL='T'
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C
C     ADE,BDE,CDE,XDE,YDE,ZDE
              IF(TEMPC(I)(1:3).EQ.'ADE'.OR.
     1        TEMPC(I)(1:3).EQ.'BDE'.OR.
     1        TEMPC(I)(1:3).EQ.'CDE'.OR.
     1        TEMPC(I)(1:3).EQ.'XDE'.OR.
     1        TEMPC(I)(1:3).EQ.'YDE'.OR.
     1        TEMPC(I)(1:3).EQ.'ZDE') THEN
                  TEMPER=TEMPC(I)(4:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  CALL ATODCODEV(VALA,VALV,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  IF(TIL.EQ.'?') THEN
                      IF(TEMPC(I)(1:3).EQ.'ADE'.OR.
     1                TEMPC(I)(1:3).EQ.'BDE'.OR.
     1                TEMPC(I)(1:3).EQ.'CDE') THEN
                          TIL='T'
                          WRITE(OUTLYNE2,2022)
 2022                     FORMAT('TILT')
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                  END IF
                  IF(TEMPC(I)(1:3).EQ.'ADE') THEN
                      WRITE(OUTLYNE2,2023) VALV
 2023                 FORMAT('ALPHA,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:3).EQ.'BDE') THEN
                      WRITE(OUTLYNE2,2024) VALV
 2024                 FORMAT('BETA,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:3).EQ.'CDE') THEN
                      WRITE(OUTLYNE2,2025) VALV
 2025                 FORMAT('GAMMA,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:3).EQ.'XDE') THEN
                      WRITE(OUTLYNE2,2026) VALV
 2026                 FORMAT('XD,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:3).EQ.'YDE') THEN
                      WRITE(OUTLYNE2,2027) VALV
 2027                 FORMAT('YD,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:3).EQ.'ZDE') THEN
                      WRITE(OUTLYNE2,2028) VALV
 2028                 FORMAT('ZD,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C
C     RET
              IF(TEMPC(I)(1:3).EQ.'RET') THEN
                  TEMPER=TEMPC(I)(4:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TRIM(TEMPER(1:23))
                  VALA=TEMPER(2:23)//' '
                  CALL RIGHTJUST(VALA)
                  CALL ATOICODEV(VALA,IVALV,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE2,2029) IVALV
 2029             FORMAT('TILT RET,',I3)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     TILTS AND DECENTERD DONE
C
C     THICKNESS THI WITHOUT A SOLVE
C
C     NOT THI HMY, THI HMX, THI HCY THI HCX
              IF(TEMPC(I)(1:3).EQ.'THI') THEN
                  IF(TEMPC(I)(1:7).NE.'THI HMY'.AND.
     1            TEMPC(I)(1:7).NE.'THI HMX'.AND.
     1            TEMPC(I)(1:7).NE.'THI HCY'.AND.
     1            TEMPC(I)(1:7).NE.'THI HCX') THEN
C     JUST THI
                      TEMPER=TEMPC(I)(4:1024)
                      CALL LEFTJUST(TEMPER)
                      VALA=TEMPER(1:23)
                      CALL RIGHTJUST(VALA)
                      CALL ATODCODEV(VALA,VALV,CVERROR)
                      IF(CVERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
                      WRITE(OUTLYNE2,2030) VALV
 2030                 FORMAT('TH,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  END IF
              END IF
C     THICKNESS DONE
C
C
C     CLAP DIMENSIONS
C
              IF(TEMPC(I)(1:3).EQ.'CIR'.OR.
     1        TEMPC(I)(1:3).EQ.'REX'.OR.
     1        TEMPC(I)(1:3).EQ.'REY'.OR.
     1        TEMPC(I)(1:3).EQ.'ELX'.OR.
     1        TEMPC(I)(1:3).EQ.'ELY') THEN
                  IF(TEMPC(I)(1:7).NE.'CIR OBS'.AND.
     1            TEMPC(I)(1:7).NE.'CIR EDG'.AND.
     2            TEMPC(I)(1:7).NE.'CIR HOL') THEN
C     PROCEED
                      TEMPER=TEMPC(I)(4:1024)
                      CALL LEFTJUST(TEMPER)
                      VALA=TEMPER(1:23)
                      CALL RIGHTJUST(VALA)
                      CALL ATODCODEV(VALA,VALV,CVERROR)
                      IF(CVERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'CIR') THEN
                          CLTYPE='C'
                          CL1=VALV
                          CL2=VALV
                          CLAP1=.TRUE.
                          CLAP2=.TRUE.
                      ELSE
                          IF(CLTYPE.EQ.'?'.AND.TEMPC(I)(1:1).EQ.'E') CLTYPE='E'
                          IF(CLTYPE.EQ.'?'.AND.TEMPC(I)(1:1).EQ.'R') CLTYPE='R'
                          IF(TEMPC(I)(3:3).EQ.'X') THEN
C     X VALUE
                              CL1=VALV
                              CLAP1=.TRUE.
                          ELSE
C     Y VALUE
                              CL2=VALV
                              CLAP2=.TRUE.
                          END IF
                      END IF
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  ELSE
C     HOL OR EDGE OR OBS
                      IF(TEMPC(I)(5:7).NE.'OBS') THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
                  END IF
              END IF
C     CLAP DIMENSIONS DONE
C
C
C     CLAP DECENTERS  AND TILTS
C
              IF(TEMPC(I)(1:3).EQ.'ADX'.OR.
     1        TEMPC(I)(1:3).EQ.'ADY'.OR.
     1        TEMPC(I)(1:3).EQ.'ARO') THEN
                  TEMPER=TEMPC(I)(4:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  CALL ATODCODEV(VALA,VALV,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  IF(.NOT.CLDX.AND.TEMPC(I)(3:3).EQ.'X') THEN
                      CLDX=.TRUE.
                      CL3=VALV
                  END IF
                  IF(.NOT.CLDY.AND.TEMPC(I)(3:3).EQ.'Y') THEN
                      CLDY=.TRUE.
                      CL4=VALV
                  END IF
                  IF(.NOT.CLTILT.AND.TEMPC(I)(1:3).EQ.'ARO') THEN
                      CLTILT=.TRUE.
                      CL5=VALV
                  END IF
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CLAP DECENTERS AND TILTS DONE
C
C     COBS DIMENSIONS
C
              IF(TEMPC(I)(1:7).EQ.'CIR OBS'.OR.
     1        TEMPC(I)(1:7).EQ.'REX OBS'.OR.
     1        TEMPC(I)(1:7).EQ.'REY OBS'.OR.
     1        TEMPC(I)(1:7).EQ.'ELX OBS'.OR.
     1        TEMPC(I)(1:7).EQ.'ELY OBS') THEN
C     PROCEED
                  TEMPER=TEMPC(I)(8:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  CALL ATODCODEV(VALA,VALV,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  IF(TEMPC(I)(1:3).EQ.'CIR') THEN
                      COTYPE='C'
                      CO1=VALV
                      CO2=VALV
                      COBS1=.TRUE.
                      COBS2=.TRUE.
                  ELSE
                      IF(COTYPE.EQ.'?'.AND.TEMPC(I)(1:1).EQ.'E') COTYPE='E'
                      IF(COTYPE.EQ.'?'.AND.TEMPC(I)(1:1).EQ.'R') COTYPE='R'
                      IF(TEMPC(I)(3:3).EQ.'X') THEN
C     X VALUE
                          CO1=VALV
                          COBS1=.TRUE.
                      ELSE
C     Y VALUE
                          CO2=VALV
                          COBS2=.TRUE.
                      END IF
                  END IF
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     COBS DIMENSIONS DONE
C
C
C     COBS DECENTERS  AND TILTS
C
              IF(TEMPC(I)(1:7).EQ.'ADX OBS'.OR.
     1        TEMPC(I)(1:7).EQ.'ADY OBS '.OR.
     1        TEMPC(I)(1:7).EQ.'ARO OBS') THEN
                  TEMPER=TEMPC(I)(8:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  CALL ATODCODEV(VALA,VALV,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  IF(.NOT.COBX.AND.TEMPC(I)(3:3).EQ.'X') THEN
                      COBX=.TRUE.
                      CO3=VALV
                  END IF
                  IF(.NOT.COBY.AND.TEMPC(I)(3:3).EQ.'Y') THEN
                      COBY=.TRUE.
                      CO4=VALV
                  END IF
                  IF(.NOT.COTILT.AND.TEMPC(I)(1:3).EQ.'ARO') THEN
                      COTILT=.TRUE.
                      CO5=VALV
                  END IF
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     COBS DECENTERS AND TILTS DONE
C
C     DIFFRACTION GRATING COMMANDS
C
C     GRT, GRO, GRS, GRX, GRY, GRX
              IF(TEMPC(I)(1:3).EQ.'GRT'.OR.
     1        TEMPC(I)(1:3).EQ.'GRS'.OR.
     1        TEMPC(I)(1:3).EQ.'GRO'.OR.
     1        TEMPC(I)(1:3).EQ.'GRX'.OR.
     1        TEMPC(I)(1:3).EQ.'GRY'.OR.
     1        TEMPC(I)(1:3).EQ.'GRZ') THEN
                  IF(TEMPC(I)(1:3).EQ.'GRT') THEN
                      WRITE(OUTLYNE2,2031)
 2031                 FORMAT('GRT')
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:3).NE.'GRT') THEN
                      IF(TEMPC(I)(1:3).NE.'GRO') THEN
C     BREAK OUT A VALUE
                          TEMPER=TEMPC(I)(4:1024)
                          CALL LEFTJUST(TEMPER)
                          VALA=TEMPER(1:23)
                          CALL RIGHTJUST(VALA)
                          CALL ATODCODEV(VALA,VALV,CVERROR)
                          IF(CVERROR) THEN
                              WRITE(38,4001) TEMPCC(1:78)
                              GO TO 8888
                          END IF
                      ELSE
C     BREAK OUT A VALUE
                          TEMPER=TEMPC(I)(4:1024)
                          CALL LEFTJUST(TEMPER)
                          VALA=TEMPER(1:23)
                          CALL RIGHTJUST(VALA)
                          CALL ATOICODEV(VALA,IVALV,CVERROR)
                          IF(CVERROR) THEN
                              WRITE(38,4001) TEMPCC(1:78)
                              GO TO 8888
                          END IF
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'GRO') THEN
                          WRITE(OUTLYNE2,2032) DBLE(IVALV)
 2032                     FORMAT('GRO,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'GRS') THEN
                          WRITE(OUTLYNE2,2033) VALV
 2033                     FORMAT('GRS,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'GRX') THEN
                          WRITE(OUTLYNE2,2034) VALV
 2034                     FORMAT('GRX,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'GRY') THEN
                          WRITE(OUTLYNE2,2035) VALV
 2035                     FORMAT('GRY,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'GRZ') THEN
                          WRITE(OUTLYNE2,2036) VALV
 2036                     FORMAT('GRZ,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  END IF
              END IF
C     GRATING COMMANDS DONE
C
C     HOE COMMANDS
C
C     HOE, HTO, HOR, HWL, HX1,HY1,HZ1,HX2,HY2,HZ2
              IF(TEMPC(I)(1:3).EQ.'HOE'.OR.
     1        TEMPC(I)(1:3).EQ.'HOR'.OR.
     1        TEMPC(I)(1:3).EQ.'HWL'.OR.
     1        TEMPC(I)(1:3).EQ.'HX1'.OR.
     1        TEMPC(I)(1:3).EQ.'HY1'.OR.
     1        TEMPC(I)(1:3).EQ.'HZ1'.OR.
     1        TEMPC(I)(1:3).EQ.'HX2'.OR.
     1        TEMPC(I)(1:3).EQ.'HY2'.OR.
     1        TEMPC(I)(1:3).EQ.'HZ2'.OR.
     1        TEMPC(I)(1:3).EQ.'HV1'.OR.
     1        TEMPC(I)(1:3).EQ.'HV2') THEN
                  IF(TEMPC(I)(1:3).NE.'HOE'.AND.
     1            TEMPC(I)(1:3).NE.'HV1'.AND.
     1            TEMPC(I)(1:3).NE.'HV2') THEN
C     BREAK OUT A VALUE
                      TEMPER=TEMPC(I)(4:1024)
                      CALL LEFTJUST(TEMPER)
                      VALA=TEMPER(1:23)
                      CALL RIGHTJUST(VALA)
                      CALL ATODCODEV(VALA,VALV,CVERROR)
                      IF(CVERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'HOE')
     1                HOE(SURFER)=.TRUE.
                      IF(TEMPC(I)(1:7).EQ.'HV1 REA')
     1                HV1(SURFER)='R'
                      IF(TEMPC(I)(1:7).EQ.'HV1 VIR')
     1                HV1(SURFER)='V'
                      IF(TEMPC(I)(1:7).EQ.'HV2 REA')
     1                HV2(SURFER)='R'
                      IF(TEMPC(I)(1:7).EQ.'HV2 VIR')
     1                HV2(SURFER)='V'
                      IF(TEMPC(I)(1:3).EQ.'HX1')
     1                HX1(SURFER)=VALV
                      IF(TEMPC(I)(1:3).EQ.'HY1')
     1                HY1(SURFER)=VALV
                      IF(TEMPC(I)(1:3).EQ.'HZ1')
     1                HZ1(SURFER)=VALV
                      IF(TEMPC(I)(1:3).EQ.'HX2')
     1                HX2(SURFER)=VALV
                      IF(TEMPC(I)(1:3).EQ.'HY2')
     1                HY2(SURFER)=VALV
                      IF(TEMPC(I)(1:3).EQ.'HZ2')
     1                HZ2(SURFER)=VALV
                      IF(TEMPC(I)(1:3).EQ.'HWL')
     1                HWL(SURFER)=VALV
                      IF(TEMPC(I)(1:3).EQ.'HOR')
     1                HOR(SURFER)=VALV
                      HOESUR(SURFER)=SURFER
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  END IF
              END IF
C     GRATING COMMANDS DONE
C
C     THICKNESS SOLVES GO HERE
C
C     THI HMY, THI HMX, THI HCY THI HCX
              IF(TEMPC(I)(1:7).EQ.'THI HMY'.OR.
     1        TEMPC(I)(1:7).EQ.'THI HMX'.OR.
     1        TEMPC(I)(1:7).EQ.'THI HCY'.OR.
     1        TEMPC(I)(1:7).EQ.'THI HCX') THEN
                  TEMPER=TEMPC(I)(8:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  CALL ATODCODEV(VALA,VALV,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'THI HMX') THEN
                      WRITE(OUTLYNE2,2037) VALV
 2037                 FORMAT('PX,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'THI HMY') THEN
                      WRITE(OUTLYNE2,2038) VALV
 2038                 FORMAT('PY,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'THI HCX') THEN
                      WRITE(OUTLYNE2,2039) VALV
 2039                 FORMAT('PCX,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'THI HCY') THEN
                      WRITE(OUTLYNE2,2040) VALV
 2040                 FORMAT('PCY,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     THICKNESS SOLVES DONE
C
C     CURVATURE/RADII GO HERE
C
              IF(TEMPC(I)(1:3).EQ.'CUX'.OR.
     1        TEMPC(I)(1:3).EQ.'CUY'.OR.
     2        TEMPC(I)(1:3).EQ.'RDX'.OR.
     3        TEMPC(I)(1:3).EQ.'RDY') THEN
                  IF(TEMPC(I)(1:7).NE.'CUX UMX'.AND.
     1               TEMPC(I)(1:7).NE.'CUY UMY'.AND.
     1               TEMPC(I)(1:7).NE.'CUX UCX'.AND.
     1               TEMPC(I)(1:7).NE.'CUY UCY'.AND.
     1               TEMPC(I)(1:7).NE.'CUX IMX'.AND.
     1               TEMPC(I)(1:7).NE.'CUY IMY'.AND.
     1               TEMPC(I)(1:7).NE.'CUX ICX'.AND.
     1               TEMPC(I)(1:7).NE.'CUY ICY') THEN
C     PROCEED, CURVATURE OR RADII
                      TEMPER=TEMPC(I)(4:1024)
                      CALL LEFTJUST(TEMPER)
                      VALA=TEMPER(1:23)
                      CALL RIGHTJUST(VALA)
                      CALL ATODCODEV(VALA,VALV,CVERROR)
                      IF(CVERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
C     CONVERT TO RADII IF NECESSARY
                      IF(TEMPC(I)(1:1).EQ.'R'.AND.VALV.NE.0.0D0) VALV=1.0D0/VALV
C     FIRST IF ROTATIONALLY SYMMETRIC
                      IF(TOR.EQ.'? '.AND.AAS.EQ.'?'.AND.CYL.EQ.'?') THEN
                          IF(TEMPC(I)(3:3).EQ.'Y') THEN
C     YZ PLANE
                              IF(TEMPC(I)(1:1).EQ.'R') THEN
                                  WRITE(OUTLYNE2,2041) VALV
 2041                             FORMAT('RD,',D23.15)
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  INPUT(1:132)=OUTLYNE2(1:132)
                                  IF(INPUT(1:20).NE.AA) CALL PROCES
                                  REST_KDP(1)=RESTINPT(1)
                              END IF
                              IF(TEMPC(I)(1:1).EQ.'C') THEN
                                  WRITE(OUTLYNE2,2042) VALV
 2042                             FORMAT('CV,',D23.15)
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  INPUT(1:132)=OUTLYNE2(1:132)
                                  IF(INPUT(1:20).NE.AA) CALL PROCES
                                  REST_KDP(1)=RESTINPT(1)
                              END IF
                          ELSE
C     NOTHING TO DO, RDX AND CUX NOT RECOGNIZED
                          END IF
                      END IF
C
C     SET TOR IF CYL NOT ?
                      IF(CYL.EQ.'C') THEN
                          IF(TEMPC(I)(3:3).EQ.'Y') TOR='YT'
                          IF(TEMPC(I)(3:3).EQ.'Y') THEN
                              WRITE(OUTLYNE2,2043)
 2043                         FORMAT('YTORIC')
                              SAVE_KDP(1)=SAVEINPT(1)
                              INPUT(1:132)=OUTLYNE2(1:132)
                              IF(INPUT(1:20).NE.AA) CALL PROCES
                              REST_KDP(1)=RESTINPT(1)
                          END IF
                          IF(TEMPC(I)(3:3).EQ.'X') TOR='XT'
                          IF(TEMPC(I)(3:3).EQ.'X') THEN
                              WRITE(OUTLYNE2,2044)
 2044                         FORMAT('XTORIC')
                              SAVE_KDP(1)=SAVEINPT(1)
                              INPUT(1:132)=OUTLYNE2(1:132)
                              IF(INPUT(1:20).NE.AA) CALL PROCES
                              REST_KDP(1)=RESTINPT(1)
                          END IF
                      END IF
C
                      IF(TOR.EQ.'YT') THEN
C     Y TORIC
                          IF(TEMPC(I)(3:3).EQ.'Y') THEN
C     YZ PLANE
                              IF(TEMPC(I)(1:1).EQ.'R') THEN
                                  WRITE(OUTLYNE2,2045) VALV
 2045                             FORMAT('RDTOR,'D23.15)
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  INPUT(1:132)=OUTLYNE2(1:132)
                                  IF(INPUT(1:20).NE.AA) CALL PROCES
                                  REST_KDP(1)=RESTINPT(1)
                              END IF
                              IF(TEMPC(I)(1:1).EQ.'C') THEN
                                  WRITE(OUTLYNE2,2046) VALV
 2046                             FORMAT('CVTOR,'D23.15)
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  INPUT(1:132)=OUTLYNE2(1:132)
                                  IF(INPUT(1:20).NE.AA) CALL PROCES
                                  REST_KDP(1)=RESTINPT(1)
                              END IF
                          ELSE
C     XZ PLANE
                              IF(TEMPC(I)(1:1).EQ.'R') THEN
                                  WRITE(OUTLYNE2,2047) VALV
 2047                             FORMAT('RD,'D23.15)
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  INPUT(1:132)=OUTLYNE2(1:132)
                                  IF(INPUT(1:20).NE.AA) CALL PROCES
                                  REST_KDP(1)=RESTINPT(1)
                              END IF
                              IF(TEMPC(I)(1:1).EQ.'C') THEN
                                  WRITE(OUTLYNE2,2048) VALV
 2048                             FORMAT('CV,'D23.15)
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  INPUT(1:132)=OUTLYNE2(1:132)
                                  IF(INPUT(1:20).NE.AA) CALL PROCES
                                  REST_KDP(1)=RESTINPT(1)
                              END IF
                          END IF
                      END IF
                      IF(TOR.EQ.'XT') THEN
C     X TORIC
                          IF(TEMPC(I)(3:3).EQ.'Y') THEN
C     YZ PLANE
                              IF(TEMPC(I)(1:1).EQ.'R') THEN
                                  WRITE(OUTLYNE2,2049) VALV
 2049                             FORMAT('RD,'D23.15)
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  INPUT(1:132)=OUTLYNE2(1:132)
                                  IF(INPUT(1:20).NE.AA) CALL PROCES
                                  REST_KDP(1)=RESTINPT(1)
                              END IF
                              IF(TEMPC(I)(1:1).EQ.'C') THEN
                                  WRITE(OUTLYNE2,2050) VALV
 2050                             FORMAT('CV,'D23.15)
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  INPUT(1:132)=OUTLYNE2(1:132)
                                  IF(INPUT(1:20).NE.AA) CALL PROCES
                                  REST_KDP(1)=RESTINPT(1)
                              END IF
                          ELSE
C     XZ PLANE
                              IF(TEMPC(I)(1:1).EQ.'R') THEN
                                  WRITE(OUTLYNE2,2051) VALV
 2051                             FORMAT('RDTOR,'D23.15)
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  INPUT(1:132)=OUTLYNE2(1:132)
                                  IF(INPUT(1:20).NE.AA) CALL PROCES
                                  REST_KDP(1)=RESTINPT(1)
                              END IF
                              IF(TEMPC(I)(1:1).EQ.'C') THEN
                                  WRITE(OUTLYNE2,2052) VALV
 2052                             FORMAT('CVTOR,'D23.15)
                                  SAVE_KDP(1)=SAVEINPT(1)
                                  INPUT(1:132)=OUTLYNE2(1:132)
                                  IF(INPUT(1:20).NE.AA) CALL PROCES
                                  REST_KDP(1)=RESTINPT(1)
                              END IF
                          END IF
                      END IF
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  END IF
              END IF
C     CURVATURE/RADIUS DONE
C
C     CONICS AND ASPHERICS GO HERE IF TOR='? '
              IF(TOR.EQ.'? ') THEN
C     ROTATIONALLY SYMMETRIC SURFACES
                  IF(TEMPC(I)(1:2).EQ.'K '.OR.
     1               TEMPC(I)(1:2).EQ.'A '.OR.
     2               TEMPC(I)(1:2).EQ.'B '.OR.
     2               TEMPC(I)(1:2).EQ.'C '.OR.
     2               TEMPC(I)(1:2).EQ.'D '.OR.
     2               TEMPC(I)(1:2).EQ.'E '.OR.
     2               TEMPC(I)(1:2).EQ.'F '.OR.
     2               TEMPC(I)(1:2).EQ.'G '.OR.
     2               TEMPC(I)(1:2).EQ.'H '.OR.
     3               TEMPC(I)(1:2).EQ.'J ') THEN
C     PROCEED
                      TEMPER=TEMPC(I)(3:1024)
                      CALL LEFTJUST(TEMPER)
                      VALA=TEMPER(1:23)
                      CALL RIGHTJUST(VALA)
                      CALL ATODCODEV(VALA,VALV,CVERROR)
                      IF(CVERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
                      IF(TEMPC(I)(1:1).EQ.'K') THEN
                          WRITE(OUTLYNE2,2053) VALV
 2053                     FORMAT('CC,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:1).EQ.'A') THEN
                          WRITE(OUTLYNE2,2054) VALV
 2054                     FORMAT('AD,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:1).EQ.'B') THEN
                          WRITE(OUTLYNE2,2154) VALV
 2154                     FORMAT('AE,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:1).EQ.'C') THEN
                          WRITE(OUTLYNE2,2055) VALV
 2055                     FORMAT('AF,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:1).EQ.'D') THEN
                          WRITE(OUTLYNE2,2056) VALV
 2056                     FORMAT('AG,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:1).EQ.'E') THEN
                          WRITE(OUTLYNE2,2057) VALV
 2057                     FORMAT('AH,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:1).EQ.'F') THEN
                          WRITE(OUTLYNE2,2058) VALV
 2058                     FORMAT('AI,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:1).EQ.'G') THEN
                          WRITE(OUTLYNE2,2059) VALV
 2059                     FORMAT('AJ,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:1).EQ.'H') THEN
                          WRITE(OUTLYNE2,2060) VALV
 2060                     FORMAT('AK,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:1).EQ.'J') THEN
                          WRITE(OUTLYNE2,2061) VALV
 2061                     FORMAT('AL,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  END IF
              END IF
C     ROTATIONALLY SYMMETRIC SURFACES ASPHERICS DONE
C
C     CONICS AND ASPHERICS GO HERE IF TOR NOT '?'
              IF(TOR.NE.'? '.AND.AAS.EQ.'?') THEN
C     TORICS
                  IF(TEMPC(I)(1:2).EQ.'K '.OR.
     1               TEMPC(I)(1:2).EQ.'A '.OR.
     2               TEMPC(I)(1:2).EQ.'B '.OR.
     2               TEMPC(I)(1:2).EQ.'C '.OR.
     3               TEMPC(I)(1:2).EQ.'D ') THEN
                      TEMPER=TEMPC(I)(3:1024)
                      CALL LEFTJUST(TEMPER)
                      VALA=TEMPER(1:23)
                      CALL RIGHTJUST(VALA)
                      CALL ATODCODEV(VALA,VALV,CVERROR)
                      IF(CVERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
                      IF(TEMPC(I)(1:1).EQ.'K') THEN
                          WRITE(OUTLYNE2,2062) VALV
 2062                     FORMAT('CCTOR,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(2:2).EQ.'A ') THEN
                          WRITE(OUTLYNE2,2063) VALV
 2063                     FORMAT('ADTOR,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(2:2).EQ.'B ') THEN
                          WRITE(OUTLYNE2,2064) VALV
 2064                     FORMAT('AETOR,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(2:2).EQ.'C ') THEN
                          WRITE(OUTLYNE2,2065) VALV
 2065                     FORMAT('AFTOR,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(2:2).EQ.'D ') THEN
                          WRITE(OUTLYNE2,2066) VALV
 2066                     FORMAT('AGTOR,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      GO TO 213
 213                  CONTINUE
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  END IF
              END IF
C     NON AAS TORICS ASPHERICS DONE
C
C     ANAMORPHIC ASPHERICS
              IF(AAS.EQ.'Y') THEN
C     ANAMORPHIC ASPHERICS
                  IF(TEMPC(I)(1:3).EQ.'KY '.OR.
     1               TEMPC(I)(1:3).EQ.'KX '.OR.
     2               TEMPC(I)(1:3).EQ.'AR '.OR.
     2               TEMPC(I)(1:3).EQ.'AP '.OR.
     2               TEMPC(I)(1:3).EQ.'BR '.OR.
     2               TEMPC(I)(1:3).EQ.'BP '.OR.
     2               TEMPC(I)(1:3).EQ.'CR '.OR.
     2               TEMPC(I)(1:3).EQ.'CP '.OR.
     2               TEMPC(I)(1:3).EQ.'DR '.OR.
     3               TEMPC(I)(1:3).EQ.'DP ') THEN
                      TEMPER=TEMPC(I)(4:1024)
                      CALL LEFTJUST(TEMPER)
                      VALA=TEMPER(1:23)
                      CALL RIGHTJUST(VALA)
                      CALL ATODCODEV(VALA,VALV,CVERROR)
                      IF(CVERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
                      IF(TEMPC(I)(1:2).EQ.'KY') THEN
                          WRITE(OUTLYNE2,2067) VALV
 2067                     FORMAT('CC,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:2).EQ.'KX') THEN
                          WRITE(OUTLYNE2,2068) VALV
 2068                     FORMAT('CCTOR,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'AR ') THEN
                          WRITE(OUTLYNE2,2069) VALV
 2069                     FORMAT('AD,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'AP ') THEN
                          WRITE(OUTLYNE2,2070) VALV
 2070                     FORMAT('ADTOR,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'BR ') THEN
                          WRITE(OUTLYNE2,2071) VALV
 2071                     FORMAT('AE,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'BP ') THEN
                          WRITE(OUTLYNE2,2072) VALV
 2072                     FORMAT('AETOR,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'CR ') THEN
                          WRITE(OUTLYNE2,2073) VALV
 2073                     FORMAT('AF,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'CP ') THEN
                          WRITE(OUTLYNE2,2074) VALV
 2074                     FORMAT('AFTOR,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'DR ') THEN
                          WRITE(OUTLYNE2,2075) VALV
 2075                     FORMAT('AG,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(TEMPC(I)(1:3).EQ.'DP ') THEN
                          WRITE(OUTLYNE2,2076) VALV
 2076                     FORMAT('AGTOR,'D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      GO TO 216
 216                  CONTINUE
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  END IF
              END IF
C     AAS TORICS ASPHERICS DONE
C
C     CURVATURE SOLVES GO HERE
C
C     CUY AND CUX WITH UMY/UMX,UCY/UCX,IMY/IMX AND ICY/ICX
              IF(TEMPC(I)(1:7).EQ.'CUX UMX'.OR.
     1           TEMPC(I)(1:7).EQ.'CUY UMY'.OR.
     1           TEMPC(I)(1:7).EQ.'CUX UCX'.OR.
     1           TEMPC(I)(1:7).EQ.'CUY UCY'.OR.
     1           TEMPC(I)(1:7).EQ.'CUX IMX'.OR.
     1           TEMPC(I)(1:7).EQ.'CUY IMY'.OR.
     1           TEMPC(I)(1:7).EQ.'CUX ICX'.OR.
     1           TEMPC(I)(1:7).EQ.'CUY ICY') THEN
                  TEMPER=TEMPC(I)(8:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  CALL ATODCODEV(VALA,VALV,CVERROR)
                  IF(CVERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  IF(TEMPC(I)(3:3).EQ.'X') THEN
                      IF(CYL.EQ.'C') THEN
                          WRITE(OUTLYNE2,2077)
 2077                     FORMAT('XTORIC')
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(CYL.EQ.'C') TOR='XT'
                  END IF
                  IF(TEMPC(I)(3:3).EQ.'Y') THEN
                      IF(CYL.EQ.'C') THEN
                          WRITE(OUTLYNE2,2078)
 2078                     FORMAT('YTORIC')
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE2(1:132)
                          IF(INPUT(1:20).NE.AA) CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(CYL.EQ.'C') TOR='YT'
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'CUX UMX') THEN
                      WRITE(OUTLYNE2,2079) VALV
 2079                 FORMAT('PUX,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'CUY UMY') THEN
                      WRITE(OUTLYNE2,2080) VALV
 2080                 FORMAT('PUY,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'CUX UCX') THEN
                      WRITE(OUTLYNE2,2081) VALV
 2081                 FORMAT('PUCX,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'CUY UCY') THEN
                      WRITE(OUTLYNE2,2082) VALV
 2082                 FORMAT('PUCY,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'CUX IMX') THEN
                      WRITE(OUTLYNE2,2083) VALV
 2083                 FORMAT('PIX,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'CUY IMY') THEN
                      WRITE(OUTLYNE2,2084) VALV
 2084                 FORMAT('PIY,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'CUX ICX') THEN
                      WRITE(OUTLYNE2,2085) VALV
 2085                 FORMAT('PICX,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:7).EQ.'CUY ICY') THEN
                      WRITE(OUTLYNE2,2086) VALV
 2086                 FORMAT('PICY,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CURVATURE SOLVES DONE
C
              IF(TEMPC(I)(1:2).EQ.'GO') THEN
                  TEMPC(I)(1:1024)=BL1024(1:1024)
              END IF
C     IF WE ARE HERE, THE COMMAND DID NOT TRANSLATE
              IF(TEMPC(I)(1:75).NE.BL1024(1:78)) THEN
                  WRITE(38,4001) TEMPCC(1:78)
              END IF
 4001         FORMAT(' ',A78)
C     DO NEXT LINE IN THE CODE V SEQUENCE FILE
 8888         CONTINUE
          END DO
          WRITE(OUTLYNE2,2016) GLASSA
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT(1:132)=OUTLYNE2(1:132)
          IF(INPUT(1:20).NE.AA) CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SURFER=SURFER+1
          HOESUR(SURFER)=-1
          TOR='? '
          TIL='?'
          CYL='?'
          AAS='?'
          CLAP1=.FALSE.
          CLAP2=.FALSE.
          CLDX=.FALSE.
          CLDY=.FALSE.
          CLTILT=.FALSE.
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT(1:132)=OUTLYNE2(1:132)
          IF(INPUT(1:20).NE.AA) CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          WRITE(OUTLYNE2,2088)
 2088     FORMAT('EOS')
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT(1:132)=OUTLYNE2(1:132)
          IF(INPUT(1:20).NE.AA) CALL PROCES
          REST_KDP(1)=RESTINPT(1)
C     NOW HOE STUFF IF NEEDED
          DO I=0,MAXSUR
              IF(HOE(I)) GO TO 220
          END DO
          CALL CLOSE_FILE(37,1)
          CALL CLOSE_FILE(38,1)
          RETURN
 220      CONTINUE
          WRITE(OUTLYNE2,2089)
 2089     FORMAT('SPSRF')
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT(1:132)=OUTLYNE2(1:132)
          IF(INPUT(1:20).NE.AA) CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          DO I=0,MAXSUR
              IF(HOE(I)) THEN
                  WRITE(OUTLYNE2,2090) I
 2090             FORMAT('SPECIAL,',I3,', 12')
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  WRITE(OUTLYNE2,2091) I,HOR(I)
 2091             FORMAT('C1,',I3,',',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  WRITE(OUTLYNE2,2092) I,HWL(I)
 2092             FORMAT('C2,',I3,',',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  WRITE(OUTLYNE2,2093) I,HX1(I)
 2093             FORMAT('C3,',I3,',',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  WRITE(OUTLYNE2,2094) I,HY1(I)
 2094             FORMAT('C4,',I3,',',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  WRITE(OUTLYNE2,2095) I,HZ1(I)
 2095             FORMAT('C5,',I3,',',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  IF(HV1(I).EQ.'R') THEN
                      WRITE(OUTLYNE2,2097) I
 2097                 FORMAT('C5,',I3,', +1.0D0')
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(HV1(I).EQ.'V') THEN
                      WRITE(OUTLYNE2,2098) I
 2098                 FORMAT('C5,',I3,', -1.0D0')
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  WRITE(OUTLYNE2,2099) I,HX2(I)
 2099             FORMAT('C7,',I3,',',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  WRITE(OUTLYNE2,2100) I,HY2(I)
 2100             FORMAT('C8,',I3,',',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  WRITE(OUTLYNE2,2101) I,HZ2(I)
 2101             FORMAT('C9,',I3,',',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE2(1:132)
                  IF(INPUT(1:20).NE.AA) CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  IF(HV2(I).EQ.'R') THEN
                      WRITE(OUTLYNE2,2102) I
 2102                 FORMAT('C10,',I3,', +1.0D0')
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(HV2(I).EQ.'V') THEN
                      WRITE(OUTLYNE2,2103) I
 2103                 FORMAT('C10,',I3,', -1.0D0')
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE2(1:132)
                      IF(INPUT(1:20).NE.AA) CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
              END IF
          END DO
          WRITE(OUTLYNE2,2104)
 2104     FORMAT('EOS')
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT(1:132)=OUTLYNE2(1:132)
          IF(INPUT(1:20).NE.AA) CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          CALL CLOSE_FILE(37,1)
          CALL CLOSE_FILE(38,1)
          DEALLOCATE (TEMPA,TEMPB,TEMPC,STAT=ALLOERR)
          DEALLOCATE (HOE,HOESUR,HV1,HV2,HX1,HY1,HZ1
     1    ,HWL,HX2,HY2,HZ2,HOR,STAT=ALLOERR)
          RETURN
 100      FORMAT(A132)
      END
