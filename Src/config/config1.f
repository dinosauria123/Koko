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

C       FIRST FILE OF CONFIGS FILES

C SUB SORDER.FOR
      SUBROUTINE SORDER(I)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SORDER. IT REMOVES INCONSISTENCIES
C       IN SPECIAL SURFACE DATA WITHIN
C       CONFIGURATION DATA FOR CONFIGURATION (I).

          INTEGER SCRCNT,ULINE,ELINE,
     1    TESTV1,TESTV2,EXSST(1:2000),I,J,III,
     2    ALLOERR,JJ,KKK,JJJ
C
          CHARACTER BLANK*140,CSTUFF*3
C
          COMMON/BLAAA/BLANK
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          CHARACTER*140 SCRATH
          DIMENSION SCRATH(:)
          ALLOCATABLE :: SCRATH
          INTEGER NANA
          NANA=2000
          DEALLOCATE (SCRATH,STAT=ALLOERR)
          ALLOCATE (SCRATH(NANA),STAT=ALLOERR)
C
C
          BLANK=AA//AA//AA//AA//AA//AA//AA
          SCRATH(1:2000)=BLANK
C
C       FIND THE LOCATION OF THE U SP AND ITS EOS LINE
C
          ULINE=0
          ELINE=1
          SCRCNT=0
C       CHECK FOR U SP
          DO 20 J=1,CFGCNT(I)
C
              EE12=CONFG(I,J)
              HOLDER=EE12
              IF((HOLDER(1:11)).EQ.'U        SP'.OR.
     1        (HOLDER(1:14)).EQ.'UPDATE   SPSRF'.OR.
     1        (HOLDER(1:11)).EQ.'UPDATE   SP'.OR.
     1        (HOLDER(1:14)).EQ.'U        SPSRF') THEN
C       FOUND U SP, SET ULINE TO J
                  ULINE=J
                  GO TO 21
              END IF
 20       CONTINUE
 21       CONTINUE
          IF(ULINE.EQ.0) THEN
C       NOTHING TO CLEAN UP, JUST RETURN
              DEALLOCATE(SCRATH,STAT=ALLOERR)
              RETURN
          ELSE
C       LOOK FOR EOS
          END IF
          DO 30 J=(ULINE+1),CFGCNT(I)
              EE12=CONFG(I,J)
              HOLDER=EE12
              IF((HOLDER(1:3)).EQ.'EOS') THEN
                  ELINE=J
                  GO TO 31
              ELSE
C       CONTINUE SEARCH
              END IF
 30       CONTINUE
 31       CONTINUE
          IF(ELINE.EQ.0) THEN
              WRITE(OUTLYNE,*)'SERIOUS ERROR IN SCLEAN SUBROUTINE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'REPORT THIS ERROR!'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'SCLEAN TOOK NO ACTION'
              CALL SHOWIT(1)
              CALL MACFAL
              DEALLOCATE(SCRATH,STAT=ALLOERR)
              RETURN
          END IF
C       WRITE THE FIRST LINE
          SCRCNT=SCRCNT+1
          EE12=CONFG(I,ULINE)
          HOLDER=EE12
          SCRATH(SCRCNT)=HOLDER
C
C       NOW WE HAVE ULINE AND ELINE. NOW FIND ALL THE "SPECIAL" LINES
C       , "SPDEL", "SPSRF ON", "SPSRF OFF" LINES AND "C1" TO C96" LINES
C       THAT LIE BETWEEN ULINE AND ELINE. ORDER THEM
C       IN ASSENDING SURFACE ORDER.
C       NOW SPECIAL/SPDEL
C       NOW CHECK 'SPECIAL'/'SPDEL', ONLY ONE OR THE OTHER IS
C       ALLOWED PER SURFACE NUMBER
C
C       START CHECKING FROM END OF DATA LIST FOR EACH VALID
C       SURFACE NUMBER FROM 1 TO INT(SYSTEM1(20)
          DO 110 III=1,INT(SYSTEM1(20))
C
              DO 120 JJ=ELINE-1,ULINE+1,-1
C
                  EE12=CONFG(I,JJ)
                  HOLDER=EE12
                  IF(HOLDER(1:7).EQ.'SPECIAL'.OR.
     1            HOLDER(1:5).EQ.'SPDEL'.OR.HOLDER(1:4).EQ.'GENL') THEN
C       CONVERT NUMERIC ENTRY TO INTEGER AND CONPARE WITH
C       VALUE OF III. IF NO MATCH, GO TO 120
                      CALL CONVR2(HOLDER(1:140),TESTV1)
                      IF(TESTV1.NE.III) GO TO 120
C       TESTV1=III, PROCEED.
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                      SCRCNT=SCRCNT+1
                      SCRATH(SCRCNT)=HOLDER
                      CONFG(I,JJ)=BLANK(1:140)
                      GO TO 110
                  ELSE
C       FIRST OCCURENCE OF SPECIAL/SPDEL NOT FOUND, CHECK NEXT ENTRY
                  END IF
 120          CONTINUE
C
C       NOW DECREMENT SURFACE COUNTER AND CHECK NEXT SURFACE NUMBER
 110      CONTINUE
C       NOW ALL SPECIAL/SPDEL COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       SPECIAL DONE
C
C       NOW SPSRF ON/SPSRF OFF
C       NOW CHECK 'SPSRF ON/SPSRF OFF', ONLY ONE OR THE OTHER IS
C       ALLOWED PER SURFACE NUMBER
C
C       START CHECKING FROM END OF DATA LIST FOR EACH VALID
C       SURFACE NUMBER FROM 1 TO INT(SYSTEM1(20)
          DO 1105 III=1,INT(SYSTEM1(20))
C
              DO 1205 JJ=ELINE-1,ULINE+1,-1
C
                  EE12=CONFG(I,JJ)
                  HOLDER=EE12
                  IF(HOLDER(1:11).EQ.'SPSRF    ON'.OR.
     1            HOLDER(1:12).EQ.'SPSRF    OFF') THEN
C       CONVERT NUMERIC ENTRY TO INTEGER AND CONPARE WITH
C       VALUE OF III. IF NO MATCH, GO TO 1205
                      CALL CONVR1(HOLDER(1:140),TESTV1)
                      IF(TESTV1.NE.III) GO TO 1205
C       TESTV1=III, PROCEED.
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                      SCRCNT=SCRCNT+1
                      SCRATH(SCRCNT)=HOLDER
                      CONFG(I,JJ)=BLANK(1:40)
                      GO TO 1105
                  ELSE
C       FIRST OCCURENCE OF SPECIAL/SPDEL NOT FOUND, CHECK NEXT ENTRY
                  END IF
 1205         CONTINUE
C
C       NOW DECREMENT SURFACE COUNTER AND CHECK NEXT SURFACE NUMBER
 1105     CONTINUE
C       NOW ALL SPECIAL/SPDEL COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       SPECIAL DONE
C
C       CHECK EXISTENCE OF SPECIAL SURFACE DEFINITIONS ON
C       EACH SURFACE FOR THE CURRENT CFG AND SET THE EXISTENCE
C       FLAG EXSTT(SURF) TO 1 IF THERE IS A DEFINITION AND 0
C       IF THERE IS NOT
          KKK=INT(SYSTEM1(20))
          EXSST(0:KKK)=0
          DO 1100 KKK=1,INT(SYSTEM1(20))
C       FIRST IF SURFACE IS SPECIAL IN LENS DATA
              IF(ALENS(34,KKK).NE.0.0D0) THEN
C       SURFACE IS SPECIAL IN LENS DATA.
                  EXSST(KKK)=1
              ELSE
C       SURFACE NOT SPECIAL, CONTINUE
              END IF
              DO 1101 JJ=1,SCRCNT
C       ARE THERE SPECIAL OR SPDEL FOR SURFACE I?
                  IF(SCRATH(JJ)(1:5).EQ.'SPDEL') THEN
                      CALL CONVR2(SCRATH(JJ)(1:140),TESTV1)
                      IF(TESTV1.EQ.KKK) EXSST(KKK)=0
                  ELSE
C       NOT SPDEL, CONTINUE
                  END IF
                  IF(SCRATH(JJ)(1:7).EQ.'SPECIAL'.OR.
     1            SCRATH(JJ)(1:4).EQ.'GENL') THEN
                      CALL CONVR2(SCRATH(JJ)(1:140),TESTV1)
                      IF(TESTV1.EQ.KKK) EXSST(KKK)=1
                  ELSE
C       NOT SPECIAL,CONTINUE
                  END IF
                  IF(SCRATH(JJ)(1:12).EQ.'SPSRF    OFF') THEN
                      CALL CONVR1(SCRATH(JJ)(1:140),TESTV1)
                      IF(TESTV1.EQ.KKK) EXSST(KKK)=1
                  ELSE
C       NOT SPSRF OFF,CONTINUE
                  END IF
                  IF(SCRATH(JJ)(1:11).EQ.'SPSRF    ON') THEN
                      CALL CONVR1(SCRATH(JJ)(1:140),TESTV1)
                      IF(TESTV1.EQ.KKK) EXSST(KKK)=1
                  ELSE
C       NOT SPSRF OFF,CONTINUE
                  END IF
 1101         CONTINUE
 1100     CONTINUE
C
C       NOW THE COEFFICIENTS STARTING FROM SURFACE 1 TO SYSTEM1(20)
C
C       START CHECKING FROM END OF DATA LIST FOR EACH VALID
C       SURFACE NUMBER FROM 1 TO INT(SYSTEM1(20)
C
          DO 310 III=1,INT(SYSTEM1(20))
C
              DO 400 JJJ=1,96
                  IF(JJJ.EQ.1) CSTUFF='C1 '
                  IF(JJJ.EQ.2) CSTUFF='C2 '
                  IF(JJJ.EQ.3) CSTUFF='C3 '
                  IF(JJJ.EQ.4) CSTUFF='C4 '
                  IF(JJJ.EQ.5) CSTUFF='C5 '
                  IF(JJJ.EQ.6) CSTUFF='C6 '
                  IF(JJJ.EQ.7) CSTUFF='C7 '
                  IF(JJJ.EQ.8) CSTUFF='C8 '
                  IF(JJJ.EQ.9) CSTUFF='C9 '
                  IF(JJJ.EQ.10) CSTUFF='C10'
                  IF(JJJ.EQ.11) CSTUFF='C11'
                  IF(JJJ.EQ.12) CSTUFF='C12'
                  IF(JJJ.EQ.13) CSTUFF='C13'
                  IF(JJJ.EQ.14) CSTUFF='C14'
                  IF(JJJ.EQ.15) CSTUFF='C15'
                  IF(JJJ.EQ.16) CSTUFF='C16'
                  IF(JJJ.EQ.17) CSTUFF='C17'
                  IF(JJJ.EQ.18) CSTUFF='C18'
                  IF(JJJ.EQ.19) CSTUFF='C19'
                  IF(JJJ.EQ.20) CSTUFF='C20'
                  IF(JJJ.EQ.21) CSTUFF='C21'
                  IF(JJJ.EQ.22) CSTUFF='C22'
                  IF(JJJ.EQ.23) CSTUFF='C23'
                  IF(JJJ.EQ.24) CSTUFF='C24'
                  IF(JJJ.EQ.25) CSTUFF='C25'
                  IF(JJJ.EQ.26) CSTUFF='C26'
                  IF(JJJ.EQ.27) CSTUFF='C27'
                  IF(JJJ.EQ.28) CSTUFF='C28'
                  IF(JJJ.EQ.29) CSTUFF='C29'
                  IF(JJJ.EQ.30) CSTUFF='C30'
                  IF(JJJ.EQ.31) CSTUFF='C31'
                  IF(JJJ.EQ.32) CSTUFF='C32'
                  IF(JJJ.EQ.33) CSTUFF='C33'
                  IF(JJJ.EQ.34) CSTUFF='C34'
                  IF(JJJ.EQ.35) CSTUFF='C35'
                  IF(JJJ.EQ.36) CSTUFF='C36'
                  IF(JJJ.EQ.37) CSTUFF='C37'
                  IF(JJJ.EQ.38) CSTUFF='C38'
                  IF(JJJ.EQ.39) CSTUFF='C39'
                  IF(JJJ.EQ.40) CSTUFF='C40'
                  IF(JJJ.EQ.41) CSTUFF='C41'
                  IF(JJJ.EQ.42) CSTUFF='C42'
                  IF(JJJ.EQ.43) CSTUFF='C43'
                  IF(JJJ.EQ.44) CSTUFF='C44'
                  IF(JJJ.EQ.45) CSTUFF='C45'
                  IF(JJJ.EQ.46) CSTUFF='C46'
                  IF(JJJ.EQ.47) CSTUFF='C47'
                  IF(JJJ.EQ.48) CSTUFF='C48'
                  IF(JJJ.EQ.49) CSTUFF='C49'
                  IF(JJJ.EQ.50) CSTUFF='C50'
                  IF(JJJ.EQ.51) CSTUFF='C51'
                  IF(JJJ.EQ.52) CSTUFF='C52'
                  IF(JJJ.EQ.53) CSTUFF='C53'
                  IF(JJJ.EQ.54) CSTUFF='C54'
                  IF(JJJ.EQ.55) CSTUFF='C55'
                  IF(JJJ.EQ.56) CSTUFF='C56'
                  IF(JJJ.EQ.57) CSTUFF='C57'
                  IF(JJJ.EQ.58) CSTUFF='C58'
                  IF(JJJ.EQ.59) CSTUFF='C59'
                  IF(JJJ.EQ.60) CSTUFF='C60'
                  IF(JJJ.EQ.61) CSTUFF='C61'
                  IF(JJJ.EQ.62) CSTUFF='C62'
                  IF(JJJ.EQ.63) CSTUFF='C63'
                  IF(JJJ.EQ.64) CSTUFF='C64'
                  IF(JJJ.EQ.65) CSTUFF='C65'
                  IF(JJJ.EQ.66) CSTUFF='C66'
                  IF(JJJ.EQ.67) CSTUFF='C67'
                  IF(JJJ.EQ.68) CSTUFF='C68'
                  IF(JJJ.EQ.69) CSTUFF='C69'
                  IF(JJJ.EQ.70) CSTUFF='C70'
                  IF(JJJ.EQ.71) CSTUFF='C71'
                  IF(JJJ.EQ.72) CSTUFF='C72'
                  IF(JJJ.EQ.73) CSTUFF='C73'
                  IF(JJJ.EQ.74) CSTUFF='C74'
                  IF(JJJ.EQ.75) CSTUFF='C75'
                  IF(JJJ.EQ.76) CSTUFF='C76'
                  IF(JJJ.EQ.77) CSTUFF='C77'
                  IF(JJJ.EQ.78) CSTUFF='C78'
                  IF(JJJ.EQ.79) CSTUFF='C79'
                  IF(JJJ.EQ.80) CSTUFF='C80'
                  IF(JJJ.EQ.81) CSTUFF='C81'
                  IF(JJJ.EQ.82) CSTUFF='C82'
                  IF(JJJ.EQ.83) CSTUFF='C83'
                  IF(JJJ.EQ.84) CSTUFF='C84'
                  IF(JJJ.EQ.85) CSTUFF='C85'
                  IF(JJJ.EQ.86) CSTUFF='C86'
                  IF(JJJ.EQ.87) CSTUFF='C87'
                  IF(JJJ.EQ.88) CSTUFF='C88'
                  IF(JJJ.EQ.89) CSTUFF='C89'
                  IF(JJJ.EQ.90) CSTUFF='C90'
                  IF(JJJ.EQ.91) CSTUFF='C91'
                  IF(JJJ.EQ.92) CSTUFF='C92'
                  IF(JJJ.EQ.93) CSTUFF='C93'
                  IF(JJJ.EQ.94) CSTUFF='C94'
                  IF(JJJ.EQ.95) CSTUFF='C95'
                  IF(JJJ.EQ.96) CSTUFF='C96'
C
C       NOW C#
C       NOW CHECK 'C#'
C
                  DO 320 JJ=ELINE-1,ULINE+1,-1
C
                      EE12=CONFG(I,JJ)
                      HOLDER=EE12
                      IF(HOLDER(1:3).EQ.CSTUFF) THEN
                          CALL CONVR2(HOLDER(1:140),TESTV2)
C
                          IF(TESTV2.EQ.III.AND.EXSST(III).EQ.0) THEN
C       DON'T PROCESS COEFS JUST
C       GO TO NEXT SURFACE
                              GO TO 310
                          ELSE
C       PROCEED PROCESSING THE COEFFICIENTS
                          END IF
C

C       CONVERT NUMERIC ENTRY TO INTEGER AND CONPARE WITH
C       VALUE OF III. IF NO MATCH, GO TO 320
                          CALL CONVR2(HOLDER(1:140),TESTV2)
                          IF(TESTV2.NE.III) GO TO 320
C       TESTV2=III, PROCEED.
C       FOUND AND ENTRY FOR THE SCRATH ARRAY
                          SCRCNT=SCRCNT+1
                          SCRATH(SCRCNT)=HOLDER
                          CONFG(I,JJ)=BLANK(1:140)
C       CHECK FOR NEXT COEFFICIENT
                          GO TO 400
                      ELSE
C       FIRST OCCURENCE OF 'C#' NOT FOUND, CHECK NEXT ENTRY
                      END IF
 320              CONTINUE
C
C       NOW ALL 'C#' COMMANDS ARE RE-WRITTEN TO SCRATH ARRAY
C       'C#' DONE
 400          CONTINUE
C       NOW DECREMENT SURFACE COUNTER AND CHECK NEXT SURFACE NUMBER
 310      CONTINUE
C       NOW WRITE THE REST OF THE CONFIGS ARRAY TO THE SCRATH ARRAY
C
          DO 135 J=ELINE,CFGCNT(I)
              SCRCNT=SCRCNT+1
              EE12=CONFG(I,J)
              HOLDER=EE12
              SCRATH(SCRCNT)=HOLDER
 135      CONTINUE
C
C       NOW BLANK THE LINES ULINE TO CFGCNT(I)
C       IN THE CONFIGS ARRAY
          CONFG(I,ULINE:CFGCNT(I))=BLANK(1:140)
C
C       NOW WRITE SCRATH BACK INTO CONFIG ARRAY
          JJ=ULINE
          DO 16 J=1,SCRCNT

              CONFG(I,JJ)=SCRATH(J)(1:140)
              JJ=JJ+1
 16       CONTINUE
C
C       RESET CFGCNT(I)
          CFGCNT(I)=SCRCNT
C
          DEALLOCATE(SCRATH,STAT=ALLOERR)
          RETURN
      END
C SUB REMOVE.FOR
      SUBROUTINE REMOVE
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED FROM THE CMD LEVEL.
C       IT WORKS BY REMOVING THE JTH THROUGH THE KTH
C       ENTRIES IN THE CONFIGS SUBFILE FOR THE ITH CONFIG
C
C       THE COMMAND IS: REMOVE,I J K
C
C       EXPLICIT NUMERIC INPUT FOR ALL THREE NUMERIC WORDS
C       IS REQUIRED. K MUST BE GREATER THAN J
C       I IS VALID FOR I=2 TO K=100
C
          CHARACTER
     1    A*17,B*17,C*17,D*17,E*17,F*17,G*17,H*17,AI*3
C
          INTEGER DELCNT,RET,I,J,RETRET,LEOS,SPEOS
C
          COMMON/RETIT/RET,RETRET,LEOS,SPEOS
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmai.inc'
C
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"REMOVE" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"REMOVE" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W2.GT.W3) THEN
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD (2) MUST BE < OR = NUMERIC WORD (3)'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).LT.2.OR.INT(W1).GT.2000) THEN
              WRITE(OUTLYNE,*)'REQUESTED CONFIGURATION  BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(CFGCNT(INT(W1)).EQ.0) THEN
              WRITE(OUTLYNE,*)'NO CONFIGURATION DATA FOR CFG ',INT(W1)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W3).GT.CFGCNT(INT(W1))) THEN
C       REQUEST BEYOND LINES EXISTING
              WRITE(OUTLYNE,*)'REQUESTED DELETION BEYOND EXISTING DATA'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       ALL INPUT OK, DELETE THE REQUESTED DATA
C       THE FIRST DATA LINE TO GO IS
C       INT(W2), THE LAST TO GO IS INT(W3)
C       ANY ENTRY AHEAD OF INT(W2) IS NOT AFFECTED
C
C       THE FOLLOWING STINGS MAY NOT BE DIRECTLY REMOVED:
C
C       'U        L       ,,,,,,'
C       'UPDATE   LENS    ,,,,,,'
C       'U        SP      ,,,,,,'
C       'UPDATE   SPSRF   ,,,,,,'
C       'EOS'
          A='U        L      '
          B='UPDATE   LENS    '
          C='U        SP      '
          D='UPDATE   SPSRF   '
          E='UPDATE   L      '
          F='U        LENS    '
          G='UPDATE   SP      '
          H='U        SPSRF   '
          AI='EOS'
C
C       IN THE ABSENCE OF THESE FORBIDDEN STRINGS
C       HOW MANY LINES ARE REQUESTED TO BE DELETED
C       INT(W3)-INT(W2)+1
C       SET THE DELETION COUNTER
          DELCNT=INT(W3)-INT(W2)+1
C       START AT LINE INT(W2)
  5       I=INT(W2)
 10       CONTINUE
          EE12=CONFG(INT(W1),I)
          HOLDER=EE12
          IF(HOLDER(1:17).NE.A.AND.
     1    HOLDER(1:17).NE.B.AND.
     2    HOLDER(1:17).NE.C.AND.
     3    HOLDER(1:17).NE.D.AND.
     1    HOLDER(1:17).NE.E.AND.
     2    HOLDER(1:17).NE.F.AND.
     3    HOLDER(1:17).NE.G.AND.
     1    HOLDER(1:17).NE.H.AND.
     4    HOLDER(1:3).NE.AI) THEN
C       DELETE THE LINE
              DO 20 J=I,(CFGCNT(INT(W1))-1)
                  EE12=CONFG(INT(W1),(J+1))
                  HOLDER=EE12
                  CONFG(INT(W1),(J))=HOLDER(1:140)
 20           CONTINUE
C       ONE LINE DELETED
C       DECREMENT THE DELETION COUNTER
              DELCNT=DELCNT-1
C       DECREMENT THE CFGCNT COUNTER
              CFGCNT(INT(W1))=CFGCNT(INT(W1))-1
C       IS THE DELETION COUNTER ZERO
              IF(DELCNT.LE.0) THEN
C       DELETIONS COMPLETE,RETURN
                  GO TO 30
              ELSE
C       DELETIONS NOT ZERO, CONTINUE
                  GO TO 10
              END IF
          ELSE
C       FOUND A STRING NOT TO DELETE
              I=I+1
              W2=W2+1.0
C       DECREMENT THE DELETION  COUNTER
              DELCNT=DELCNT-1
              IF(DELCNT.LE.0) GO TO 30
C       PROCEED
              GO TO 5
          END IF
C
 30       CONTINUE
C
          RET=1
          F1=0
          F11=1
          F12=INT(W1)
          CALL CFGIN3
          F1=1
          F11=0
          F12=1
          RETURN
      END
C SUB CFSC1.FOR
      SUBROUTINE CFSC1
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE CFSC1 DOES SCALING OF CONFIG
C       DATA WHEN SAY,SAX,SCY,SCX,SCY FANG, SCX FANG
C       WRX,WRY (BDX AND BDY GET SCALED BY THE INVERSE OF THE SCALE FACTOR)
C       ARE SCALED WITH SURFACE DATA.
C
          CHARACTER HOLD*140,AVAL1*23,
     2    AVAL2*23,AVAL3*23,AVAL4*23,AVAL5*23,SNAME*9,
     3    LNAME*18,AN1*23,AV1*23
C
          INTEGER
     6    ULINE,ELINE,IEND,CHG1,ISTART,ISTOP,STARL,STOPL,ISTA,
     7    ISTO,I,J
C
          REAL*8 VAL1,VAL2,MM1,
     2    VAL3,VAL4,VAL5,N1,V1
C
          COMMON/JK_NTA3/V1,AV1
C
          COMMON/CAUX1/N1,AN1
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       STRAIGHT SCALING (SC,FACT,I,J)
C       W1=FACTOR
C       W2=STARTING SURFACE
C       W3=ENDING SURFACE
C       SAY,SAX,SCY,SCX STARTING VALUES ALSO SCALED
C
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
C     UNLESS WRX,WRY,BDX OR BDY ENTERED
              IF(WC.NE.'BDX'.AND.WC.NE.'BDY'.AND.WC.NE.'WRX'.AND.
     1        WC.NE.'WRY') CALL FNBDE(I)
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
C       ANYTHING BETWEEN ULINE+1 AND CHG1-1 IS NON SURFACE DATA
C       MAYBE TO SCALE
              IF(CHG1.GT.(ULINE+1)) THEN
C       THERE IS NON SURFACE DATA TO PROCESS
C       CHECK FOR SAY, SAX, WRX AND WRY
                  DO 30 J=(ULINE+1),(CHG1-1)
                      EE12=CONFG(I,J)
                      HOLDER=EE12
C       SET DEFAULT VALUES
                      VAL1=0.0D0
                      VAL2=0.0D0
                      AVAL1='                    '
                      AVAL2='                    '
                      IF((HOLDER(1:3)).EQ.'SAY'.OR.
     1                (HOLDER(1:3)).EQ.'SAX'.OR.HOLDER(1:3).EQ.'WRX'.OR.
     1                HOLDER(1:3).EQ.'WRY') THEN
C       FOUND SAY, SAX, WRX OR WRY TO PROCESS
                          AVAL1=(HOLDER(10:32))
                          AV1=AVAL1
                          CALL ATON3
                          VAL1=V1
C       SCALE VAL1
                          VAL1=VAL1*W1
                          V1=VAL1
                          CALL NTOA3
                          AVAL1=AV1
C       REBUILD CONFIG ENTRY
                          HOLDER=HOLDER(1:9)//AVAL1//',,,,,'
                          CONFG(I,J)=HOLDER(1:140)
C       CHECK NEXT CONFIG LINE
                          GO TO 30
                      ELSE
C       NOT SAY OR SAX, PROCEED
                      END IF
                      HOLDER=EE12
C       SET DEFAULT VALUES
                      VAL1=0.0D0
                      VAL2=0.0D0
                      AVAL1='                    '
                      AVAL2='                    '
                      IF((HOLDER(1:3)).EQ.'BDY'.OR.
     1                (HOLDER(1:3)).EQ.'BDX') THEN
C       BDX OR BDY TO PROCESS
                          AVAL1=(HOLDER(10:32))
                          AV1=AVAL1
                          CALL ATON3
                          VAL1=V1
C       SCALE VAL1
                          VAL1=VAL1/W1
                          V1=VAL1
                          CALL NTOA3
                          AVAL1=AV1
C       REBUILD CONFIG ENTRY
                          HOLDER=HOLDER(1:9)//AVAL1//',,,,,'
                          CONFG(I,J)=HOLDER(1:140)
C       CHECK NEXT CONFIG LINE
                          GO TO 30
                      ELSE
C       NOT BDY OR BDX, PROCEED
                      END IF
C       NOW DO SCY AND SCX
                      IF((HOLDER(1:3)).EQ.'SCY'.OR.
     1                (HOLDER(1:3)).EQ.'SCX') THEN
C       SET DEFAULT VALUES
                          VAL1=0.0D0
                          VAL2=0.0D0
                          AVAL1='                    '
                          AVAL2='                    '
                          IF((HOLDER(1:17)).NE.'SCY      FANG    '.AND.
     1                       (HOLDER(1:17)).NE.'SCX      FANG    ')THEN
C       FOUND SCY OR SCX TO PROCESS
                              AVAL1=(HOLDER(10:32))
                              AV1=AVAL1
                              CALL ATON3
                              VAL1=V1
C       SCALE VAL1
                              VAL1=VAL1*W1
                              V1=VAL1
                              CALL NTOA3
                              AVAL1=AV1
                              AVAL2=(HOLDER(34:56))
                              AV1=AVAL2
                              CALL ATON3
                              VAL2=V1
C       SCALE VAL2
                              VAL2=VAL2*W1
                              V1=VAL2
                              CALL NTOA3
                              AVAL2=AV1
C       REBUILD CONFIG ENTRY
                              HOLDER=HOLDER(1:9)//AVAL1//','//AVAL2//',,,,'
                              CONFG(I,J)=HOLDER(1:140)
C       CHECK NEXT CONFIG LINE
                              GO TO 30
                          ELSE
C       NOT SCY OR SCX, PROCEED
                          END IF
C       CHECK FOR SCY FANG AND SCX FANG
C       SET DEFAULT VALUES
                          VAL1=0.0D0
                          VAL2=0.0D0
                          AVAL1='                    '
                          AVAL2='                    '
                          IF((HOLDER(1:17)).EQ.'SCY      FANG    '.OR.
     1                       (HOLDER(1:17)).EQ.'SCX      FANG    ')THEN
C       FOUND SCY FANG OR SCX FANG TO PROCESS
                              AVAL1=(HOLDER(19:41))
                              AV1=AVAL1
                              CALL ATON3
                              VAL1=V1
C       SCALE VAL1
C     THE ANGLE DOES NOT GET SCALED !
                              VAL1=VAL1
                              V1=VAL1
                              CALL NTOA3
                              AVAL1=AV1
                              AVAL2=(HOLDER(43:65))
                              AV1=AVAL2
                              CALL ATON3
                              VAL2=V1
C       SCALE VAL2
                              VAL2=VAL2*W1
                              V1=VAL2
                              CALL NTOA3
                              AVAL2=AV1
C       REBUILD CONFIG ENTRY
                              HOLDER=HOLDER(1:18)//AVAL1//','//AVAL2//',,,,'
                              CONFG(I,J)=HOLDER(1:140)
C       CHECK NEXT CONFIG ENTRY
                          ELSE
C       NOT SCY FANG OR SCX FANG, PROCEED
                          END IF
                      ELSE
                      END IF
C       NOW DO PXIM AND PYIM AND RXIM AND RYIM
                      IF((HOLDER(1:4)).EQ.'PXIM'.OR.
     1                (HOLDER(1:4)).EQ.'PYIM'.OR.
     1                (HOLDER(1:4)).EQ.'RXIM'.OR.
     1                (HOLDER(1:4)).EQ.'RYIM') THEN
C       SET DEFAULT VALUES
                          VAL1=0.0D0
                          AVAL1='                    '
                          IF((HOLDER(1:17)).NE.'PXIM     FANG    '.AND.
     1                       (HOLDER(1:17)).NE.'PYIM     FANG    '.OR.
     1                       (HOLDER(1:17)).NE.'RXIM     FANG    '.OR.
     1                       (HOLDER(1:17)).NE.'RYIM     FANG    ')THEN
C       PROCESS
                              AVAL1=(HOLDER(10:32))
                              AV1=AVAL1
                              CALL ATON3
                              VAL1=V1
C       SCALE VAL1
                              VAL1=VAL1*W1
                              V1=VAL1
                              CALL NTOA3
                              AVAL1=AV1
C       REBUILD CONFIG ENTRY
                              HOLDER=HOLDER(1:9)//AVAL1//',,,,,'
                              CONFG(I,J)=HOLDER(1:140)
C       CHECK NEXT CONFIG LINE
                              GO TO 30
                          ELSE
C       PROCEED
                          END IF
C       CHECK FOR PXIM FANG AND PYIM FANG
C       CHECK FOR RXIM FANG AND RYIM FANG
C       SET DEFAULT VALUES
                          VAL1=0.0D0
                          AVAL1='                    '
                          IF((HOLDER(1:17)).EQ.'PXIM     FANG    '.OR.
     1                       (HOLDER(1:17)).EQ.'PYIM     FANG    '.OR.
     1                       (HOLDER(1:17)).EQ.'RXIM     FANG    '.OR.
     1                       (HOLDER(1:17)).EQ.'RYIM     FANG    ')THEN
C       PROCESS
                              AVAL1=(HOLDER(19:41))
                              AV1=AVAL1
                              CALL ATON3
                              VAL1=V1
C       SCALE VAL1
C     THE ANGLE DOES NOT GET SCALED !
                              VAL1=VAL1
                              V1=VAL1
                              CALL NTOA3
                              AVAL1=AV1
C       REBUILD CONFIG ENTRY
                              HOLDER=HOLDER(1:18)//AVAL1//',,,,,'
                              CONFG(I,J)=HOLDER(1:140)
C       CHECK NEXT CONFIG ENTRY
                          ELSE
C       PROCEED
                          END IF
                      ELSE
                      END IF
 30               CONTINUE
              ELSE
C       NO NON SURFACE DATA,PROCEED WITH SURFACE DATA
              END IF
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
                      HOLDER='CV      ,'//AVAL1//HOLD(1:103)
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
                      HOLDER='RD      ,'//AVAL1//HOLD(1:103)
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
C
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
                  IF((HOLDER(1:5)).EQ.'ASPH2') THEN
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
                          VAL1=VAL1/(W1)
                          V1=VAL1
                          CALL NTOA3
                          AVAL1=AV1
                      ELSE
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
                              V1=VAL3
                              CALL NTOA3
                              AVAL3=AV1
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
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
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
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
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
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
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
                              ELSE
                              END IF
                              IF(AVAL2.NE.',') THEN
                                  AV1=AVAL2
                                  CALL ATON3
                                  VAL2=V1
                                  VAL2=VAL2*MM1
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
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
                              ELSE
                              END IF
                              IF(AVAL2.NE.',') THEN
                                  AV1=AVAL2
                                  CALL ATON3
                                  VAL2=V1
                                  VAL2=VAL2
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
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
                              ELSE
                              END IF
                              IF(AVAL2.NE.',') THEN
                                  AV1=AVAL2
                                  CALL ATON3
                                  VAL2=V1
                                  VAL2=VAL2
                                  V1=VAL3
                                  CALL NTOA3
                                  AVAL3=AV1
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
                      GO TO 299
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
     2                (HOLDER(10:17)).EQ.'AC      '.OR.
     4                (HOLDER(10:17)).EQ.'AD      '.OR.
     5                (HOLDER(10:17)).EQ.'AE      '.OR.
     6                (HOLDER(10:17)).EQ.'AF      '.OR.
     7                (HOLDER(10:17)).EQ.'AG      '.OR.
     4                (HOLDER(10:17)).EQ.'AH      '.OR.
     5                (HOLDER(10:17)).EQ.'AI      '.OR.
     6                (HOLDER(10:17)).EQ.'AJ      '.OR.
     7                (HOLDER(10:17)).EQ.'AK      '.OR.
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
     5                (HOLDER(10:17)).EQ.'XD      '.OR.
     4                (HOLDER(10:17)).EQ.'GDX     '.OR.
     4                (HOLDER(10:17)).EQ.'GDY     '.OR.
     5                (HOLDER(10:17)).EQ.'GDZ     '.OR.
     5                (HOLDER(10:17)).EQ.'ALPHA   '.OR.
     5                (HOLDER(10:17)).EQ.'BETA    '.OR.
     5                (HOLDER(10:17)).EQ.'GAMMA   '.OR.
     5                (HOLDER(10:17)).EQ.'GALPHA  '.OR.
     5                (HOLDER(10:17)).EQ.'GBETA   '.OR.
     5                (HOLDER(10:17)).EQ.'GGAMMA  '.OR.
     5                (HOLDER(10:17)).EQ.'PIVX    '.OR.
     5                (HOLDER(10:17)).EQ.'PIVY    '.OR.
     5                (HOLDER(10:17)).EQ.'PIVZ    ') THEN
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
     1                LNAME(10:17).EQ.'AI      '.OR.
     2                LNAME(10:17).EQ.'AO      '.OR.
     3                LNAME(10:17).EQ.'AK      '.OR.
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
                              IF(LNAME(10:17).EQ.'AC      ')THEN
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
                      AN1=CONFG(I,J)(10:32)
                      CALL AUXATN
                      VAL1=N1
                      AN1=CONFG(I,J)(34:56)
                      CALL AUXATN
                      VAL2=N1
C     WHAT KIND OF SPECIAL SURFACE IS IT ?
                      IF(SPECFF(I,J).EQ.4.OR.SPECF2(I,J).EQ.4) THEN
C     TYPE 4
                          VAL2=VAL2*W2
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
                      IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
C     TYPE 12
                          VAL2=VAL2*W1
                      END IF
                      IF(SPECFF(I,J).EQ.4.OR.SPECF2(I,J).EQ.4) THEN
C     TYPE 4
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
                  IF(CONFG(I,J)(1:3).EQ.'C60') THEN
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
                  IF(CONFG(I,J)(1:3).EQ.'C75') THEN
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
C       FINISHED SCALING OF SURFACE DATA
C
 10       CONTINUE
          RETURN
      END
