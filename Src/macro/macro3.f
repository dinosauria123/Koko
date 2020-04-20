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

C       THIRD FILE OF MACRO FILES

C SUB MACLOD.FOR
      SUBROUTINE MACLOD
C
          IMPLICIT NONE
C
          LOGICAL EXST
C
C       THIS IS CALLED BY MACEXC TO RUN A MACRO OR FUNCTION
C       INSIDE A MACRO OR FUNCTION.
C       THE FUNCTION OF THIS SUBROUTINE IS TO LOAD INTO THE
C       MACRO ARRAYS THE TARGET MACRO OR FUNCTION DESIGNATED BY ITS ADDRESS
C       IN THE MACRO OR FUNCTION DIRECTORY.
C
          CHARACTER FILNAM*10
C
          INTEGER NF,L,K
          INTEGER FLG(0:20)
C
          COMMON/NEFER/NF
C
          COMMON/COMNAM/FILNAM
C
          COMMON/FFL/FLG
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
          IF(.NOT.NESFUN(NEST)) THEN
              CALL MACFIL
              EXST=.FALSE.
              INQUIRE(FILE=trim(LIBMAC)//FILNAM,EXIST=EXST)
              IF(.NOT.EXST) THEN
                  OUTLYNE=
     1            'MACRO BODY FILE '//FILNAM//' MISSING'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'MACRO FILE DAMAGED, SEE REFERENCE MANUAL FOR'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'REPAIR PROCEDURE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1        'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
C
C       NOW LETS LOAD THE MACRO ARRAYS WITH THE CONTENTS OF
C       THE NF TH MACRO. WE DO THIS FROM MAC.DAT (UNIT=20)
C
C       NOW READ THE BODY OF THE MACRO STARTING AT RECORD 1
C
              DO 30 K=1,(MCDIR2(2,NF))
                  L=K-1
                  READ(UNIT=30,REC=K) MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),
     1            MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),
     2            MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),
     3            MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),
     4            MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L)
     5            ,MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)
C
                  L=L+1
C
 30           CONTINUE
              CALL CLOSE_FILE(30,1)
C
          ELSE
C       THE CALLED ITEM WAS A FUNCTION, LOAD IT
C
C       NOW LETS LOAD THE MACRO ARRAYS WITH THE CONTENTS OF
C       THE NF TH FUNCTION.
C       INITIALIZE MACRO FUNCTIONS
              MCDIR1(NF)=FCDIR1(NF)
              MCDIR2(1,NF)=FCDIR2(1,NF)
              MCDIR2(2,NF)=FCDIR2(2,NF)
              MCDIR2(3,NF)=FCDIR2(3,NF)
              MCDIR3(NF)=FCDIR3(NF)
C
              DO K=1,(MCDIR2(2,NF))
                  L=K-1
                  MACCW(L)=FUNCW(NF,L)
                  MACQW(L)=FUNQW(NF,L)
                  MACSTR(L)=FUNSTR(NF,L)
                  MACNW(1,L)=FUNNW(NF,1,L)
                  MACNW(2,L)=FUNNW(NF,2,L)
                  MACNW(3,L)=FUNNW(NF,3,L)
                  MACNW(4,L)=FUNNW(NF,4,L)
                  MACNW(5,L)=FUNNW(NF,5,L)
                  MACSTA(1,L)=FUNSTA(NF,1,L)
                  MACSTA(2,L)=FUNSTA(NF,2,L)
                  MACSTA(3,L)=FUNSTA(NF,3,L)
                  MACSTA(4,L)=FUNSTA(NF,4,L)
                  MACSTA(5,L)=FUNSTA(NF,5,L)
                  MACSTA(6,L)=FUNSTA(NF,6,L)
                  MACSTA(7,L)=FUNSTA(NF,7,L)
                  MACSTA(8,L)=FUNSTA(NF,8,L)
                  MACSTA(9,L)=FUNSTA(NF,9,L)
                  MACSTA(10,L)=FUNSTA(NF,10,L)
                  MACSTA(11,L)=FUNSTA(NF,11,L)
                  MACSTA(12,L)=FUNSTA(NF,12,L)
                  MACSTA(13,L)=FUNSTA(NF,13,L)
                  MACSTA(14,L)=FUNSTA(NF,14,L)
                  MACSTA(15,L)=FUNSTA(NF,15,L)
                  MACSTA(16,L)=FUNSTA(NF,16,L)
                  MACSTA(17,L)=FUNSTA(NF,17,L)
                  MACSTA(18,L)=FUNSTA(NF,18,L)
                  MACSTA(19,L)=FUNSTA(NF,19,L)
                  MACSTA(20,L)=FUNSTA(NF,20,L)
              END DO
C       FUNCTION LOADED INTO MAC ARRAYS
          END IF
          RETURN
      END
C SUB MACIN.FOR
      SUBROUTINE MACIN(WWW1)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS ONLY CALLED WHEN FLAG F2=1 (MACRO
C       CREATION MODE. HEREIN ALL COMMANDS ARE RESOLVED CORRECTLY
C       FOR THE MACRO CREATION MODE OF THE PROGRAM.
C
C       THE EOM COMMAND IS THE ONLY WAY OUT OF THE MACRO CREATION
C       MODE. THIS SUBROUTINE PROCESSES THE EOM COMMAND BY CALLING
C       SUBROUTINE EEOM. SUBROUTINE EEOM CLOSES OFF MACRO CREATION,
C       SETS FLAG F2=0 AND F1=1 (REENABLING THE CMD LEVEL,
C       AND APPROPRIATELY FILING THE MACRO INTO THE MACRO LIBRARY
C       FILE MAC.DAT.
C
          INTEGER IMAC,MAXLL
C
          CHARACTER MACNAM*8
C
C       MACCW,MACQW,AND MACSTR HOLD COMMWD,QUALWD AND STRINGS
C       FOR MAXLL MACRO LINES. MACSTA HOLDS VARIOUS STATUS INDICATORS.
C       MACNW HOLDS NUMERIC WORDS.
C
          DOUBLE PRECISION WWW1
C
          INTEGER MACLOC,IMACEN
C
C       IMAC TRACKS THE CURRENT MACRO LINE NUMBER.
C       MAXIMUM VALUE ALLOWED FOR IMAC IS MAXLL-1.
C
          COMMON/IMAC/IMAC
C
          COMMON/MACID/MACNAM
C
          COMMON/MACID2/MACLOC,IMACEN
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C       TEST FOR MACRO INPUT COMMANDS. IF THERE ARE NONE, GO TO
C       CREATION PHASE IE COMMANDS WHICH EXECUTE IMMEDIATELY AND
C       ARE NOT STORED IN THE MACRO. THE COMMAND ARE '?' AND ' '.
C
          IF(WC.EQ.'?'.OR.WC.EQ.' ') THEN
              IF(WC.EQ.'?') CALL QUERRYY
              IF(WC.EQ.' '.AND.F50.EQ.1) CALL BLANK0
          ELSE
C
C       HERE IS WHERE ALL OF THE PASSED COMMAND WORDS,QUALIFIER WORDS,
C       AND OTHER INPUT ARE STORED IN THE MACRO ARRAYS.  THE FIRST LINE OF
C       EACH MACRO WILL BE AUTOMATICALLY WRITTEN AS:
C
C       MACRO (NAME),(LOCK)
C
C       THE LAST LINE (MAXLL-1) CAN ONLY CONTAIN 'EOM'.
C       (END OF MACRO). WHEN EOM IS ISSUED FROM THE KEYBOARD OR
C       READ IN FROM FILES EDITTEXT,CARDTEXT OR PUNCH
C       IT CAUSES 'EOM' TO BE WRITTEN IN THE CURRENT MACRO LINE
C       AND IT ENDS MACRO CREATION. IF 'EOM' IS ENCOUNTERED IN A MACRO
C       DURING MACRO EXECUTION THEN
C       IT SIMPLY INDICATES THE END OF THE MACRO.
C
C       AFTER EACH STORAGE
C       OPERATION, IMAC  THE COUNTER IS INCREMENTED. WHEN IMAC REACHES
C       MAXLL-2 A MESSAGE ' THAT WAS THE MAXLL-2 TH LINE,
C       THE LINE MAXLL TH LINE
C       IS RESERVED
C       FOR THE AUTOMATIC EOM COMMAND.'
C       IF THE MACRO IS LESS THAN MAXLL LINES LONG, THE LAST LINE OF
C       THE MACRO WILL AUTOMATICALLY BE WRITTEN AS EOM PRIOR TO STORAGE.
C
C       TEST FOR THE FIRST LINE OF THE MACRO
              IF(IMAC.EQ.0) THEN
                  MACCW(IMAC)='MACRO'
C       TEST FOR A BLANK MACRO NAME (BLANK NAMES ARE NOT ALLOWED)
                  IF(WQ.EQ.'        ') THEN
                      OUTLYNE=
     1                'A BLANK MACRO NAME IS NOT ALLOWED.'
                      CALL SHOWIT(1)
                      OUTLYNE=
     1                'RETURNED TO CMD LEVEL'
                      CALL SHOWIT(1)
                      F2=0
                      F1=1
                      CALL MACFAL
                      RETURN
                  ELSE
                  END IF
C       THE MACRO NAME IS NOT BLANK, CONTINUE PROCESSING.
C
C       STORE THE MACRO NAME AND THE MACRO LOCK IN ARRAYS AND IN SPECIAL
C       VARIABLES.
C
                  MACNAM=WQ
                  MACLOC=INT(WWW1)
                  MACCW(IMAC)=WC
                  MACQW(IMAC)=WQ
                  MACSTR(IMAC)=WS
                  MACNW(1,IMAC)=W1
                  MACNW(2,IMAC)=W2
                  MACNW(3,IMAC)=W3
                  MACNW(4,IMAC)=W4
                  MACNW(5,IMAC)=W5
                  MACSTA(1,IMAC)=SB1
                  MACSTA(2,IMAC)=SB2
                  MACSTA(3,IMAC)=SC1
                  MACSTA(4,IMAC)=SC2
                  MACSTA(5,IMAC)=SQ
                  MACSTA(6,IMAC)=SST
                  MACSTA(7,IMAC)=S1
                  MACSTA(8,IMAC)=S2
                  MACSTA(9,IMAC)=S3
                  MACSTA(10,IMAC)=S4
                  MACSTA(11,IMAC)=S5
                  MACSTA(12,IMAC)=DF1
                  MACSTA(13,IMAC)=DF2
                  MACSTA(14,IMAC)=DF3
                  MACSTA(15,IMAC)=DF4
                  MACSTA(16,IMAC)=DF5
                  MACSTA(17,IMAC)=SN
                  MACSTA(18,IMAC)=STI
C       MACSTA(19,IMAC) AND MACSTA(20,IMAC) NOT YET USED
                  MACSTA(19,IMAC)=0
                  MACSTA(20,IMAC)=0
C       INCREMENT THE COUNTER
                  IMAC=IMAC+1
                  RETURN
              ELSE
C       IMAC NOT EQUAL TO 0
              END IF
C       PROCEED WITH PROCESSING
C       ARE WE AT LINE MAXLL-2?
              CALL MAXLNN(MAXLL,MAXLIN)
              IF(IMAC.EQ.(MAXLL-2)) THEN
                  WRITE(OUTLYNE,*)
     1              'THAT WAS LINE ',MAXLL,'. LINE ',
     2              MAXLL,' IS RESERVED FOR (EOM)'
                  CALL SHOWIT(1)
C       INCREMENT THE COUTER
                  IMAC=IMAC+1
                  CALL MACFAL
                  RETURN
              END IF
C       CONTINUE PROCESSING
C       ARE WE AT MAXLL-1 WITH NON- (EOM) INPUT?
              CALL MAXLNN(MAXLL,MAXLIN)
              IF(IMAC.EQ.(MAXLL-1).AND.WC.NE.'EOM') THEN
                  OUTLYNE=
     1            'NO INPUT OTHER THAN (EOM) IS ALLOWED'
                  CALL SHOWIT(1)
C       IMAC NOT INCREMENTED HERE
                  CALL MACFAL
                  RETURN
              END IF
C       IS THE NEXT INPUT (EOM)?
C
              IF(WC.EQ.'EOM') THEN
                  MACCW(IMAC)=WC
                  MACQW(IMAC)=WQ
                  MACSTR(IMAC)=WS
                  MACNW(1,IMAC)=W1
                  MACNW(2,IMAC)=W2
                  MACNW(3,IMAC)=W3
                  MACNW(4,IMAC)=W4
                  MACNW(5,IMAC)=W5
                  MACSTA(1,IMAC)=SB1
                  MACSTA(2,IMAC)=SB2
                  MACSTA(3,IMAC)=SC1
                  MACSTA(4,IMAC)=SC2
                  MACSTA(5,IMAC)=SQ
                  MACSTA(6,IMAC)=SST
                  MACSTA(7,IMAC)=S1
                  MACSTA(8,IMAC)=S2
                  MACSTA(9,IMAC)=S3
                  MACSTA(10,IMAC)=S4
                  MACSTA(11,IMAC)=S5
                  MACSTA(12,IMAC)=DF1
                  MACSTA(13,IMAC)=DF2
                  MACSTA(14,IMAC)=DF3
                  MACSTA(15,IMAC)=DF4
                  MACSTA(16,IMAC)=DF5
                  MACSTA(17,IMAC)=SN
                  MACSTA(18,IMAC)=STI
C       MACSTA(19,IMAC) AND MACSTA(20,IMAC) NOT YET USED
                  MACSTA(19,IMAC)=0
                  MACSTA(20,IMAC)=0
C       VARIABLE IMACEN KEEPS TRACK OF THE NUMBER OF LINES IN EACH
C       FINISHED MACRO
C
                  IMACEN=IMAC+1
C       IMAC NOT INCREMENTED HERE. NOW CALL EEOM TO FILE THE MACRO
C       IN MAC.DAT
                  CALL EEOM
C       WE ARE AT THE CMD LEVEL NOW SO JUST RETURN
                  RETURN
              ELSE
              END IF
C       IMAC .NE. MAXLL-2 OR MAXLL-1 AND WC .NE. 'EOM'
C       PROCEED WITH PROCESSING WHEN IMAC NE 0,MAXLL-2,OR
C       MAXLL-1.
C
C       BEFORE PUTING ING THE LINE IN THE MACRO, CHECK SOME SYNTAX
C       JUST AS IN MACMOD.FOR
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
                  END IF
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
              IF(WC.EQ.'BPOS'.OR.WC.EQ.'BNEG'.OR.WC.EQ.'BRDQ') THEN
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
              IF(WC.EQ.'BRDF1'.OR.WC.EQ.'BRDF2'.OR.WC.EQ.'BRDF3'.OR.
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
              IF(WC.EQ.'BZE'.OR.WC.EQ.'BRI'.OR.WC.EQ.'BRJ'.OR.
     1        WC.EQ.'BRU'.OR.WC.EQ.'BRK'.OR.WC.EQ.'BRL'.OR.WC.EQ.'BRM'
     1        .OR.WC.EQ.'BRN') THEN
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
                  ELSE
                  END IF
              ELSE
C       NOT BREER
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
     1                '"'//WC(1:1)//'
     1" ONLY TAKES STRING AND NUMERIC WORD #1 INPUT'
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
              MACCW(IMAC)=WC
              MACQW(IMAC)=WQ
              MACSTR(IMAC)=WS
              MACNW(1,IMAC)=W1
              MACNW(2,IMAC)=W2
              MACNW(3,IMAC)=W3
              MACNW(4,IMAC)=W4
              MACNW(5,IMAC)=W5
              MACSTA(1,IMAC)=SB1
              MACSTA(2,IMAC)=SB2
              MACSTA(3,IMAC)=SC1
              MACSTA(4,IMAC)=SC2
              MACSTA(5,IMAC)=SQ
              MACSTA(6,IMAC)=SST
              MACSTA(7,IMAC)=S1
              MACSTA(8,IMAC)=S2
              MACSTA(9,IMAC)=S3
              MACSTA(10,IMAC)=S4
              MACSTA(11,IMAC)=S5
              MACSTA(12,IMAC)=DF1
              MACSTA(13,IMAC)=DF2
              MACSTA(14,IMAC)=DF3
              MACSTA(15,IMAC)=DF4
              MACSTA(16,IMAC)=DF5
              MACSTA(17,IMAC)=SN
              MACSTA(18,IMAC)=STI
C       MACSTA(19,IMAC) AND MACSTA(20,IMAC) NOT YET USED
              MACSTA(19,IMAC)=0
              MACSTA(20,IMAC)=0
              IMAC=IMAC+1
          END IF
          RETURN
      END
C SUB MACCOM.FOR
      SUBROUTINE MACCOM
C
          IMPLICIT NONE
C
          LOGICAL EXST,EXISJK
C
C       THIS SUBROUTINE IS CALLED BY MMFLC AND
C       CAUSES THE ITH MACRO'S LEADING COMMENTS TO BE OUTPUT
C       TO THE DEFAULT OUTPUT DEVICE.
C
          INTEGER OCC,MCLOC1,IMCEN1
     1    ,NF,K,L,I
C
          CHARACTER MCNAM1*8,FILNAM*10,STAMP*20
C
          COMMON/NEFER/NF
C
          COMMON/COMNAM/FILNAM
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
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
          READ(UNIT=20,REC=NF,ERR=10) MCNAM1,MCLOC1,IMCEN1,OCC,STAMP

C
C       MCNAM1 IS THE MACRO NAME, MCLOC1 IS THE MACRO LOCK,
C       IMCEN1 IS THE MACRO LENGTH IN LINES, AND OCC IS A FLAG
C       =0 IF THE RECORD REPRESENTS A BLANK MACRO AND =1
C       FOR A NON-BLANK MACRO.
C
C       MACRO RECORDS (MAXIMUM OF MAXLL NON-BLANK RECL=55*NRECL RECORDS)
C       ARE USED TO STORE A MACRO.
C
C       NOW DETERMINE THE LOCATION OF THE MACRO BODY.
C
          CALL MACFIL
          INQUIRE(FILE=trim(LIBMAC)//FILNAM,EXIST=EXST)
          IF(.NOT.EXST) THEN
              OUTLYNE=
     1        'MACRO BODY FILE '//FILNAM//' MISSING'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'MACRO FILE DAMAGED, SEE REFERENCE MANUAL FOR'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'REPAIR PROCEDURE'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       OPEN THE FILE
          OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1    'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')

C       NOW READ THE BODY OF THE MACRO STARTING AT RECORD 1
C
          DO 30 K=1,(IMCEN1)
              L=K-1
              READ(UNIT=30,REC=K) MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),
     1        MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),
     2        MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),
     3        MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),
     4        MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L)
     5        ,MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)
C
 30       CONTINUE
          CALL CLOSE_FILE(30,1)
          CALL CLOSE_FILE(20,1)
C
C       THE MACRO ARRAYS ARE NOW LOADED. NOW REFORMAT AND OUTPUT.
C
C       THE OUTPUT MAY HAVE UP TO IMCEN1 LINES IN IT.
C
C       WE READ FROM I=1, NOT I=0 SINCE I=0 HAS COMMAND WORD="MACRO"
C
          DO 40 I=0,IMCEN1
C       ONLY LINES WHICH BEGIN WITH A COMMAND WORD (C)
C       WILL BE PRINTED UNLESS MFLC IS ISSUED WITHOUT A MACRO NAME.
C       IF THAT HAPPENS, THE MACRO NAME WILL BE PRINTED.
              IF(I.EQ.0) THEN
                  GO TO 40
              ELSE
                  GO TO 41
              END IF
C
C       IF OUT = 6 (SCREEN) OR OUT = 7 (PRINTER)
C       THEN WRITE COMPACT 80 COLUMN OUTPUT.
C
C       FOR ED,CR,AND PU WRITE EXTENDED COMPLETE OUTPUT
C
 41           IF(MACCW(I)(1:2).EQ.'C '.OR.MACCW(I)(1:2).EQ.'C,') THEN
C
                  IF(OUT.EQ.8.OR.OUT.EQ.9.OR.OUT.EQ.10) THEN
                      WRITE(OUTLYNE,100) MACCW(I),(MACSTR(I))
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,200) MACCW(I),(MACSTR(I))
                      CALL SHOWIT(0)
                  END IF
                  GO TO 40
              ELSE
                  WRITE(OUTLYNE,51)
                  CALL SHOWIT(0)
                  RETURN
              END IF
C
 40       CONTINUE
C
!  50    FORMAT(A8,1X,A8)
  51      FORMAT('END OF LEADING COMMENTS')
 100      FORMAT(A8,1X,A80)
 200      FORMAT(A8,1X,A61)
C
 10       RETURN
      END
C SUB MACCOM1.FOR
      SUBROUTINE MACCOM1
C
          IMPLICIT NONE
C
          LOGICAL EXST,EXISJK
C
C       THIS SUBROUTINE IS CALLED BY MMFLC AND
C       CAUSES THE ITH MACRO'S LEADING COMMENTS TO BE OUTPUT
C       TO THE DEFAULT OUTPUT DEVICE.
C
          INTEGER OCC,MCLOC1,IMCEN1
     1    ,NF,K,L,I
C
          CHARACTER MCNAM1*8,FILNAM*10,STAMP*20
C
          COMMON/NEFER/NF
C
          COMMON/COMNAM/FILNAM
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
C       OPEN UNIT 20 FOR I/O
C
C       ***************************************************************
          MAC_COM1='                                        '
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
          READ(UNIT=20,REC=NF,ERR=10) MCNAM1,MCLOC1,IMCEN1,OCC,STAMP

C
C       MCNAM1 IS THE MACRO NAME, MCLOC1 IS THE MACRO LOCK,
C       IMCEN1 IS THE MACRO LENGTH IN LINES, AND OCC IS A FLAG
C       =0 IF THE RECORD REPRESENTS A BLANK MACRO AND =1
C       FOR A NON-BLANK MACRO.
C
C       MACRO RECORDS (MAXIMUM OF MAXLL NON-BLANK RECL=55*NRECL RECORDS)
C       ARE USED TO STORE A MACRO.
C
C       NOW DETERMINE THE LOCATION OF THE MACRO BODY.
C
          CALL MACFIL
          INQUIRE(FILE=trim(LIBMAC)//FILNAM,EXIST=EXST)
          IF(.NOT.EXST) THEN
              CALL MACFAL
              RETURN
          END IF
C
C       OPEN THE FILE
          OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1    'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')

C       NOW READ THE BODY OF THE MACRO STARTING AT RECORD 1
C
          DO 30 K=1,(IMCEN1)
              L=K-1
              READ(UNIT=30,REC=K) MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),
     1        MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),
     2        MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),
     3        MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),
     4        MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L)
     5        ,MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)
C
 30       CONTINUE
          CALL CLOSE_FILE(30,1)
          CALL CLOSE_FILE(20,1)
C
C       THE MACRO ARRAYS ARE NOW LOADED. NOW REFORMAT AND OUTPUT.
C
C       THE OUTPUT MAY HAVE UP TO IMCEN1 LINES IN IT.
C
C       WE READ FROM I=1, NOT I=0 SINCE I=0 HAS COMMAND WORD="MACRO"
C
          DO I=0,IMCEN1
C       ONLY LINES WHICH BEGIN WITH A COMMAND WORD (C)
C       WILL BE PRINTED UNLESS MFLC IS ISSUED WITHOUT A MACRO NAME.
C       IF THAT HAPPENS, THE MACRO NAME WILL BE PRINTED.
C
              IF(MACCW(I)(1:2).EQ.'C '.OR.MACCW(I)(1:2).EQ.'C,') THEN
                  MAC_COM1=MACSTR(I)(1:40)
                  RETURN
              END IF
          END DO
 10       RETURN
      END
C SUB MACDEF.FOR
C
      SUBROUTINE MACDEF
C
C       THIS SUBROUTINE HELPS MACEXC BE SMALLER
C
          IMPLICIT NONE
C
          INTEGER CT
C
          COMMON/COMCT/CT
C
          REAL*8 H(1:5,0:10)
C
          COMMON/ACH/H
C
          IF(H(3,CT).EQ.0.0D0) H(3,CT)=1.0D0
          IF(H(4,CT).EQ.0.0D0) H(4,CT)=0.0D0
          IF(H(5,CT).EQ.0.0D0) H(5,CT)=1.0D0
          RETURN
      END
C SUB MACEDT.FOR
      SUBROUTINE MACEDT
C
          IMPLICIT NONE

          INTEGER I,K,L,MAXLL
C
          LOGICAL EXST,EXISJK
C
          INTEGER MMIJ,NF
C
          CHARACTER FILNAM*10
C
          COMMON/MIJ/MMIJ
C
          COMMON/NEFER/NF
C
          COMMON/COMNAM/FILNAM
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
C       THIS SUBROUTINE IS CALLED INITIALLY BY MMEDIT
C       THE VARIABLE MMIJ TRACKS THE LOCATIONS WHERE THE FINAL
C       STORAGE WILL OCCUR. AFTER THIS SUBROUTINE LOADS
C       THE MACRO STORAGE. FURTHER WORK ON THE MACRO WILL
C       OCCUR THROUGH CALLS FROM SUBROUTINE CONTOL THROUGH
C       SUBROUTINE MACMOD.
C       IF MMIJ = 0 DESIGNATES A NEW MACRO. IF MMIJ LIES BETWEEN
C       1 AND MAXMAC IT DESIGNATES THE LOCATION IN THE DIRECTORY
C       WHERE THE MEDITED MACRO IS AND IS TO BE STORED.
C
C       FL,QU AND QUIT ARE THE ONLY WAYS OUT OF THE MACRO EDIT
C       MODE. THIS SUBROUTINE PROCESSES THE FL COMMAND BY CALLING
C       SUBROUTINE FLMAC. SUBROUTINE FLMAC CLOSES OFF MACRO EDITING,
C       SETS FLAG F3=0 AND F1=1 (REENABLING THE CMD LEVEL,
C       AND APPROPRIATELY FILING THE MACRO INTO THE MACRO LIBRARY
C       FILE MAC.DAT. QUIT OR QU GETS OUT WITHOUT FILING.
C
C               INITIALIZE OLDIJ
          OLDIJ=MAXMAC+1
C
C       NOW LOAD THE CURRENT MACRO DIRECTORY INTO MEMORY
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
          ELSE
C       PROCEED
          END IF
C       ***************************************************************
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
C
          DO 10 I=1,MAXMAC
              READ(UNIT=20,REC=I,ERR=10)MCDIR1(I),MCDIR2(1,I),MCDIR2(2,I)
     1        ,MCDIR2(3,I),MCDIR3(I)
 10       CONTINUE
          CALL CLOSE_FILE(20,1)
C
C       IF MMIJ = 0 THEN FIND A BLANK SPOT IN THE DIRECTORY
C       ELSE MMIJ DESIGNATES THE MACRO TO BE MEDITED.
C
          IF(MMIJ.EQ.0) THEN
              OLDIJ=0
              DO 11 I=1,MAXMAC
                  IF(MCDIR2(3,(I)).EQ.0) THEN
                      MMIJ=I
                      GO TO 12
                  ELSE
                  END IF
 11           CONTINUE
          ELSE
C       MMIJ NOT = 0, PROCEED
          END IF
C
C       NOW WE HAVE A NON-ZERO MMIJ- A REAL PLACE TO GET AND STORE
C       A MACRO. IF OLDIJ = 0 DON'T BOTHER TO RETREIVE A MACRO FROM
C       POSITION MMIJ. IF OLDIJ = MAXMAC+1, THEN RETRIEVE A MACRO
C       NOW DEFINE THE ARRAYS WHICH HOLD THE MACRO.
 12       CONTINUE
C       WAS THERE AN EXISTING MACRO TO EDIT?
C
          IF(OLDIJ.EQ.(MAXMAC+1)) THEN
C
C               YES THERE WAS
C
              NF=MMIJ
              CALL MACFIL
              EXST=.FALSE.
              INQUIRE(FILE=trim(LIBMAC)//FILNAM,EXIST=EXST)
              IF(.NOT.EXST) THEN
                  OUTLYNE=
     1            'MACRO BODY FILE '//FILNAM//' MISSING'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'MACRO FILE DAMAGED, SEE REFERENCE MANUAL FOR'
                  CALL SHOWIT(1)
                  OUTLYNE=
     1            'REPAIR PROCEDURE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1        'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
C
C       NOW READ THE BODY OF THE MACRO STARTING AT RECORD 1
C
              DO 30 K=1,(MCDIR2(2,(MMIJ)))
                  L=K-1
                  READ(UNIT=30,REC=K) MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),
     1            MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),
     2            MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),
     3            MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),
     4            MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L)
     5            ,MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)
C
  30          CONTINUE
              CALL CLOSE_FILE(30,1)
C               SET CURRENT LINE
              CURLIN=0
          ELSE
C
C       THERE WAS NO EXISTING MACRO TO EDIT SO DON'T TRY
C       TO READ IN A MACRO.
C       THE MACRO DIRECTORY AND MACRO MEMORY ARE THEREFORE INITIALIZED.
              MCDIR1(MMIJ)=WQ
              MCDIR2(1,(MMIJ))=INT(W1)
C       LEAVE MCDIR2(3,(MMIJ))=0 TILL MACRO IS SUCCESFULLY STORED.
              MCDIR2(3,(MMIJ))=1
C
              MCDIR2(2,(MMIJ))=3
C       THE MACRO STORAGE IS NOW INITIALIZED.
              CALL MAXLNN(MAXLL,MAXLIN)
              I=(MAXLL-1)
              MACCW(0:I)=' '
              MACQW(0:I)=' '
              MACSTR(0:I)=' '
              MACNW(1:5,0:I)=0.0D0
              MACSTA(1:20,0:I)=0
C       SET UP THE FIRST LINE OF THE NEW MACRO
              MACCW(0)='MACRO'
              MACQW(0)=MCDIR1(MMIJ)
              MACSTA(4,0)=1
              MACSTA(5,0)=1
              MACSTA(12,0)=1
              MACSTA(13,0)=1
              MACSTA(14,0)=1
              MACSTA(15,0)=1
              MACSTA(16,0)=1
C
C
C       SET CURRENT LINE
              CURLIN=1
C       SET FLAG F46 = 1 TO INDICATE A BLANK MACRO
              F46=1
          END IF
          RETURN
      END


C SUB MACEXC.FOR
      SUBROUTINE MACEXC
C
          IMPLICIT NONE
C
C       THIS IS THE SUBROUTINE THAT REALLY DOES IT. YES BOYS AND GIRLS
C       THIS SUBROUTINE RUNS A MACRO AS IF IT WERE A VALID
C       PROGRAM COMMAND.
C
C       THE MACRO THAT IT EXECUTES IS THE NF TH MACRO. PROVISION
C       HAS BEEN MADE FOR MACRO NESTING AT UP TO 20 LEVELS.
C
C       FOR EACH LEVEL THE PROGRAM KEEPS TRACK OF THE FOLLOWING
C       ITEMS IN SEVERAL ARRAYS DIMENSIONED (0:20).
C       VARIABLE NEST TRACKS THE CURRENT NEST LEVEL
C
C       ARRAY NESTI  TRACKS THE LINE NUMBER TO WHICH THE
C               I+1 TH MACRO RETURNS WHEN RETURNING TO THE
C               I TH MACRO.
C       ARRAY NESTIJ TRACK THE MACROS IN THE NEST.
C
          LOGICAL READERR,EXST,FUN,NSUBDV
C
          COMMON/ERRREAD/READERR
C
          CHARACTER FILNAM*10,OMWQ(0:20)*8,OMWS(0:20)*80,WQOLD*8
     1    ,ACCWRD*8,EE1*8,EE2*80,WCOLD*8,STROLD*80
C
          INTEGER FLG(0:20),SNOLD,SQOLD,LINCUR,LINBOT
     1    ,STOLD,S1OLD,S2OLD,S3OLD,S4OLD,S5OLD,OMSQ(0:20),OMDF(0:20,1:5)
     2    ,DF1OLD,DF2OLD,DF3OLD,DF4OLD,DF5OLD,OMST(0:20)
C
          COMMON/STAT13/LINCUR,LINBOT
C
          INTEGER NF,LLL,LLK
C
          LOGICAL HAP1,MOVEYES,QRSUB,CRSUB
C
          COMMON/NEFER/NF
C
          INTEGER CT,
     6    NSUB,CSUB,QSUB,SSUB,Q(0:20),SPOINT,
     7    CTF,K,L,KK,LL,I
C
          COMMON/COMCT/CT
C
          REAL*8 H(1:5,0:10),HOLDER,OMNW(0:20,1:5),
     1    PASS1,PASS2,W1OLD,W2OLD,W3OLD,W4OLD,W5OLD
C
          COMMON/COMACC1/PASS1
C
          COMMON/COMACC2/PASS2
C
          COMMON/FFL/FLG
C
          COMMON/ACCSB/ACCWRD
C
          INTEGER ACCCNT,ACCSUB
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          COMMON/ACH/H
C
          COMMON/COMNAM/FILNAM
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       THE MACRO INFORMATION IS STORED IN A PRE-PROCESSED
C       FORM AND THUS CALLS NEED NOT BE PASSED TO
C       THE PROCESS ROUTINE BUT CAN GO DIRECTLY TO SUBROUTINE
C       CONTROL.
C
C       MACCW,MACQW,AND MACSTR HOLD COMMWD,QUALWD AND STRINGS
C       FOR MAXLL MACRO LINES. MACSTA HOLDS VARIOUS STATUS INDICATORS.
C       MACNW HOLDS NUMERIC WORDS.
C
          F57=0
C     OR WE HAVE PROBLEMS WITH AUTOFUNC
          NEST=0
          NESTER=0
C
C       THE INITIAL VALUES STORED IN THE MACRO DIRECTORY ARE:
C     SAVE THE REAL ORIGINAL INPUT VALUES FOR THE CALL FROM THE
C     CMD LEVEL
          OMWQ(NESTER)=MWQ(NESTER)
          OMWS(NESTER)=MWS(NESTER)
          OMSQ(NESTER)=MSQ(NESTER)
          OMST(NESTER)=MST(NESTER)
          OMNW(NESTER,1)=MNW(NESTER,1)
          OMNW(NESTER,2)=MNW(NESTER,2)
          OMNW(NESTER,3)=MNW(NESTER,3)
          OMNW(NESTER,4)=MNW(NESTER,4)
          OMNW(NESTER,5)=MNW(NESTER,5)
          OMDF(NESTER,1)=MDF(NESTER,1)
          OMDF(NESTER,2)=MDF(NESTER,2)
          OMDF(NESTER,3)=MDF(NESTER,3)
          OMDF(NESTER,4)=MDF(NESTER,4)
          OMDF(NESTER,5)=MDF(NESTER,5)
C
C       SET TF(NEST)=0 AS THE DEFAULT WHEN BEGINNING MACRO EXECUTION
          TF(NEST)=0
C       SET SSTEP(NEST)=0 AS THE DEFAULT WHEN BEGINNING MACRO EXECUTION
          SSTEP(NEST)=0
C
C       INITIALIZE NSUB,QSUB,SSUB AND CSUB
          NSUB=0
          QRSUB=.FALSE.
          CRSUB=.FALSE.
          QSUB=0
          SSUB=0
          CSUB=0
          CT=0
C       CT COUNTS THE NUMBER OF STACKED NSUBS
C
C               MCDIR1(NF) IS THE MACRO NAME
C               MCDIR2(1,NF) IS THE MACRO LOCK
C               MCDIR2(2,NF) IS THE MACRO LENGTH
C               MCDIR2(3,NF) IS THE OCCUPANCY FLAG
C                               = 0 FOR BLANK MACRO
C                               = 1 FOR NON-BLANK MACRO
C
          IF(.NOT.FUN) THEN
              NESFUN(NEST)=.FALSE.
              CALL MACFIL
              EXST=.FALSE.
              INQUIRE(FILE=trim(LIBMAC)//FILNAM,EXIST=EXST)
              IF(.NOT.EXST) THEN
                  OUTLYNE='MACRO BODY FILE '//FILNAM//' MISSING'
                  CALL SHOWIT(1)
                  OUTLYNE='MACRO FILE DAMAGED, SEE REFERENCE MANUAL FOR'
                  CALL SHOWIT(1)
                  OUTLYNE='REPAIR PROCEDURE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              NESTIJ(0)=NF
C
              OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1        'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
C
C       NOW LETS LOAD THE MACRO ARRAYS WITH THE CONTENTS OF
C       THE NF TH MACRO. WE DO THIS FROM MAC.DAT (UNIT=20)
C
C       NOW READ THE BODY OF THE MACRO STARTING AT RECORD 1
C
              DO 30 K=1,(MCDIR2(2,NF))
                  L=K-1
                  READ(UNIT=30,REC=K) MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),
     1            MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),
     2            MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),
     3            MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),
     4            MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L)
     5            ,MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)
 30           CONTINUE
              CALL CLOSE_FILE(30,1)
          ELSE
              NESFUN(NEST)=.TRUE.
C       FUN WAS TRUE, EXECUTE THE FUNCTION DESIGNATED BY NF
C***********************************************************************
C       INITIALIZE MACRO FUNCTIONS
              MCDIR1(NF)=FCDIR1(NF)
              MCDIR2(1,NF)=FCDIR2(1,NF)
              MCDIR2(2,NF)=FCDIR2(2,NF)
              MCDIR2(3,NF)=FCDIR2(3,NF)
              MCDIR3(NF)=FCDIR3(NF)
C
              DO K=1,(MCDIR2(2,NF))
                  L=K-1
                  MACCW(L)=FUNCW(NF,L)
                  MACQW(L)=FUNQW(NF,L)
                  MACSTR(L)=FUNSTR(NF,L)
                  MACNW(1,L)=FUNNW(NF,1,L)
                  MACNW(2,L)=FUNNW(NF,2,L)
                  MACNW(3,L)=FUNNW(NF,3,L)
                  MACNW(4,L)=FUNNW(NF,4,L)
                  MACNW(5,L)=FUNNW(NF,5,L)
                  MACSTA(1,L)=FUNSTA(NF,1,L)
                  MACSTA(2,L)=FUNSTA(NF,2,L)
                  MACSTA(3,L)=FUNSTA(NF,3,L)
                  MACSTA(4,L)=FUNSTA(NF,4,L)
                  MACSTA(5,L)=FUNSTA(NF,5,L)
                  MACSTA(6,L)=FUNSTA(NF,6,L)
                  MACSTA(7,L)=FUNSTA(NF,7,L)
                  MACSTA(8,L)=FUNSTA(NF,8,L)
                  MACSTA(9,L)=FUNSTA(NF,9,L)
                  MACSTA(10,L)=FUNSTA(NF,10,L)
                  MACSTA(11,L)=FUNSTA(NF,11,L)
                  MACSTA(12,L)=FUNSTA(NF,12,L)
                  MACSTA(13,L)=FUNSTA(NF,13,L)
                  MACSTA(14,L)=FUNSTA(NF,14,L)
                  MACSTA(15,L)=FUNSTA(NF,15,L)
                  MACSTA(16,L)=FUNSTA(NF,16,L)
                  MACSTA(17,L)=FUNSTA(NF,17,L)
                  MACSTA(18,L)=FUNSTA(NF,18,L)
                  MACSTA(19,L)=FUNSTA(NF,19,L)
                  MACSTA(20,L)=FUNSTA(NF,20,L)
              END DO
C       FUNCTION LOADED INTO MAC ARRAYS
C
C***********************************************************************
          END IF
C
C       THE REQUESTED MACRO IS NOW LOADED INTO THE MACRO ARRAYS.
C
C       NOW PROCESS THE COMMAND THROUGH THE SUBROUTINE CONTROL
C       STARTING WITH THE SECOND ENTRY AND CEASE EXECUTION AT THE
C       IMACEN-1 TH ENTRY.( THIS AVOIDS PROCESSING THE WORDS
C       MACRO OR EOM)
C
C       BEFORE CALLING CONTROL, HOWEVER, CHECK THAT NONE OF
C       THE COMMANDS STORED IN THE MACRO ARRAYS ARE THEMSELVES
C       MACRO NAMES. IF THEY ARE THEN THE NEST HANDLEING FEATURES
C       IN THIS SUBROUTINE TAKE CARE OF THEM. MACROS THAT
C       CALL MACROS NEVER PASS THE NESTED MACRO CALL BACK
C       TO SUBROUTINE CONTROL.
C
C       SET THE STARTING LINE NUMBER HERE (SPOINT)
          SPOINT=1
 85       CONTINUE
          I=SPOINT
          GO TO 54
 5111     IF(CT.NE.0) CT=CT-1
          IF(CT.EQ.0) NSUB=0
          I=I+1
 54       CONTINUE
          W1=0.0D0
          W2=0.0D0
          W3=0.0D0
          W4=0.0D0
          W5=0.0D0
          S1=0
          S2=0
          S3=0
          S4=0
          S5=0
          SN=0
          DF1=1
          DF2=1
          DF3=1
          DF4=1
          DF5=1
          IF(MACCW(I).EQ.'EOM') THEN
              NEST=NEST-1
C     RESTORE THE REAL ORIGINAL INPUT VALUES FROM THE CALL FROM THE
C     CMD LEVEL
              IF(NEST.EQ.-1) THEN
                  NESTER=0
              ELSE
                  NESTER=NEST
              END IF
C
              MWQ(NESTER)  =OMWQ(NESTER)
              MWS(NESTER)  =OMWS(NESTER)
              MSQ(NESTER)  =OMSQ(NESTER)
              MST(NESTER)  =OMST(NESTER)
              MNW(NESTER,1)=OMNW(NESTER,1)
              MNW(NESTER,2)=OMNW(NESTER,2)
              MNW(NESTER,3)=OMNW(NESTER,3)
              MNW(NESTER,4)=OMNW(NESTER,4)
              MNW(NESTER,5)=OMNW(NESTER,5)
              MDF(NESTER,1)=OMDF(NESTER,1)
              MDF(NESTER,2)=OMDF(NESTER,2)
              MDF(NESTER,3)=OMDF(NESTER,3)
              MDF(NESTER,4)=OMDF(NESTER,4)
              MDF(NESTER,5)=OMDF(NESTER,5)

              IF(NEST.EQ.-1) THEN
                  NESTER=0
                  NEST=0
C       STOP RUNNING MACROS AND RETURN SETTING F4=0
                  F4=0
                  WC=WCOLD
                  WQ=WQOLD
                  WS=STROLD
                  SQ=SQOLD
                  SN=SNOLD
                  SST=STOLD
                  S1=S1OLD
                  S2=S2OLD
                  S3=S3OLD
                  S4=S4OLD
                  S5=S5OLD
                  DF1=DF1OLD
                  DF2=DF2OLD
                  DF3=DF3OLD
                  DF4=DF4OLD
                  DF5=DF5OLD
                  W1=W1OLD
                  W2=W2OLD
                  W3=W3OLD
                  W4=W4OLD
                  W5=W5OLD
                  RETURN
              ELSE
                  NESTER=NEST
              END IF
C       NEST NOT -1, PROCEED
              IF(.NOT.NESFUN(NEST)) THEN
C     'RETURNING EXECUTION TO ',MCDIR1(NESTIJ(NEST))
                  NF=NESTIJ(NEST)
                  CALL MACLOD
                  SPOINT=NESTI(NEST)
                  GO TO 85
              ELSE
C       RETURNING TO A FUNCTION
C     'RETURNING EXECUTION TO ',FCDIR1(NESTIJ(NEST))
                  NF=NESTIJ(NEST)
                  CALL MACLOD
                  SPOINT=NESTI(NEST)
                  GO TO 85
              END IF
C       COMMAND WAS NOT AN EOM, PROCEED
          END IF
C
C       IF YOU GOT HERE THEN
C       THE MACRO DID NOT CALL A MACRO OR A FUNCTION. THE COMMAND IS
C       EITHER A VALID PROGRAM COMMAND OR AN INVALID
C       PROGRAM COMMAND.
C***********************************************************************
C       NOW IF A COMMENT LINE IS ENCOUNTERED WE WANT TO DO NOTHING
C       IF WE DON'T HANDLE THE COMMENT HERE WE RISK THE POSSIBILITY
C       OF CSUB,QSUB,SSUB OR NSUB SUBING INTO A COMMENT. THIS NEVER NEEDS
C       TO OCCUR.
C
          IF(MACCW(I).EQ.'C') THEN
C       SKIP TO THE NEXT MACRO LINE BY A GO TO 50
              GO TO 50
C       JUST PROCEED
          END IF
C***********************************************************************
C       NOW IF A MESSAGE LINE IS ENCOUNTERED WE WANT HANDLE IT
C       IMMEDIATELY.
C       IF WE DON'T HANDLE THE MESSAGE HERE WE RISK THE POSSIBILITY
C       OF CSUB,QSUB,SSUB OR NSUB SUBING INTO A MESSAGE. THIS NEVER NEEDS
C       TO OCCUR.
C
          IF(MACCW(I).EQ.'M') THEN
C       HANDLE MESSAGE NOW THEN SKIP TO NEXT MACRO LINE
              WCOLD=WC
              WQOLD=WQ
              STROLD=WS
              SNOLD=SN
              SQOLD=SQ
              STOLD=SST
              S1OLD=S1
              S2OLD=S2
              S3OLD=S3
              S4OLD=S4
              S5OLD=S5
              DF1OLD=DF1
              DF2OLD=DF2
              DF3OLD=DF3
              DF4OLD=DF4
              DF5OLD=DF5
              W1OLD=W1
              W2OLD=W2
              W3OLD=W3
              W4OLD=W4
              W5OLD=W5
              WC=MACCW(I)
              WS=MACSTR(I)
              SQ=MACSTA(5,I)
              SN=MACSTA(17,I)
              W1=MACNW(1,I)
              W2=MACNW(2,I)
              W3=MACNW(3,I)
              W4=MACNW(4,I)
              W5=MACNW(5,I)
              SST=MACSTA(6,I)
              S1=MACSTA(7,I)
              S2=MACSTA(8,I)
              S3=MACSTA(9,I)
              S4=MACSTA(10,I)
              S5=MACSTA(11,I)
              DF1=MACSTA(12,I)
              DF2=MACSTA(13,I)
              DF3=MACSTA(14,I)
              DF4=MACSTA(15,I)
              DF5=MACSTA(16,I)
              CALL MESCOM
              WC=WCOLD
              WQ=WQOLD
              WS=STROLD
              SQ=SQOLD
              SN=SNOLD
              SST=STOLD
              S1=S1OLD
              S2=S2OLD
              S3=S3OLD
              S4=S4OLD
              S5=S5OLD
              DF1=DF1OLD
              DF2=DF2OLD
              DF3=DF3OLD
              DF4=DF4OLD
              DF5=DF5OLD
              W1=W1OLD
              W2=W2OLD
              W3=W3OLD
              W4=W4OLD
              W5=W5OLD
              GO TO 50
C       JUST PROCEED
          END IF
C
          IF(MACCW(I).EQ.'MOVE'.AND.MACQW(I).NE.'NW') THEN
              WCOLD=WC
              WQOLD=WQ
              STROLD=WS
              SNOLD=SN
              SQOLD=SQ
              STOLD=SST
              S1OLD=S1
              S2OLD=S2
              S3OLD=S3
              S4OLD=S4
              S5OLD=S5
              DF1OLD=DF1
              DF2OLD=DF2
              DF3OLD=DF3
              DF4OLD=DF4
              DF5OLD=DF5
              W1OLD=W1
              W2OLD=W2
              W3OLD=W3
              W4OLD=W4
              W5OLD=W5
              WC=MACCW(I)
              WQ=MACQW(I)
              WS=MACSTR(I)
              SQ=MACSTA(5,I)
              SN=MACSTA(17,I)
              W1=MACNW(1,I)
              W2=MACNW(2,I)
              W3=MACNW(3,I)
              W4=MACNW(4,I)
              W5=MACNW(5,I)
              SST=MACSTA(6,I)
              S1=MACSTA(7,I)
              S2=MACSTA(8,I)
              S3=MACSTA(9,I)
              S4=MACSTA(10,I)
              S5=MACSTA(11,I)
              DF1=MACSTA(12,I)
              DF2=MACSTA(13,I)
              DF3=MACSTA(14,I)
              DF4=MACSTA(15,I)
              DF5=MACSTA(16,I)
C     DO MOVES HERE
              MOVEYES=.FALSE.
              IF(WQ.EQ.'A') MOVEYES=.TRUE.
              IF(WQ.EQ.'B') MOVEYES=.TRUE.
              IF(WQ.EQ.'C') MOVEYES=.TRUE.
              IF(WQ.EQ.'D') MOVEYES=.TRUE.
              IF(WQ.EQ.'E') MOVEYES=.TRUE.
              IF(WQ.EQ.'F') MOVEYES=.TRUE.
              IF(WQ.EQ.'G') MOVEYES=.TRUE.
              IF(WQ.EQ.'H') MOVEYES=.TRUE.
              IF(WQ.EQ.'I') MOVEYES=.TRUE.
              IF(WQ.EQ.'J') MOVEYES=.TRUE.
              IF(WQ.EQ.'K') MOVEYES=.TRUE.
              IF(WQ.EQ.'L') MOVEYES=.TRUE.
              IF(WQ.EQ.'M') MOVEYES=.TRUE.
              IF(WQ.EQ.'N') MOVEYES=.TRUE.
              IF(WQ.EQ.'IX') MOVEYES=.TRUE.
              IF(WQ.EQ.'IY') MOVEYES=.TRUE.
              IF(WQ.EQ.'IZ') MOVEYES=.TRUE.
              IF(WQ.EQ.'IT') MOVEYES=.TRUE.
              IF(WQ.EQ.'Y') MOVEYES=.TRUE.
              IF(WQ.EQ.'Z') MOVEYES=.TRUE.
              IF(WQ.EQ.'T') MOVEYES=.TRUE.
              IF(WQ.EQ.'ITEST') MOVEYES=.TRUE.
              IF(WQ.EQ.'JTEST') MOVEYES=.TRUE.
              IF(WQ.EQ.'KTEST') MOVEYES=.TRUE.
              IF(WQ.EQ.'LTEST') MOVEYES=.TRUE.
              IF(WQ.EQ.'MTEST') MOVEYES=.TRUE.
              IF(WQ.EQ.'NTEST') MOVEYES=.TRUE.
              IF(.NOT.MOVEYES) THEN
                  OUTLYNE='INVALID REGISTER NAME USED WITH THE "MOVE" COMMAND'
                  CALL SHOWIT(1)
                  OUTLYNE='MACRO EXECUTION ABORTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
                  REG(40)=REG(9)
                  IF(WQ.EQ.'A')REG(9)=REG(1)
                  IF(WQ.EQ.'B')REG(9)=REG(2)
                  IF(WQ.EQ.'C')REG(9)=REG(3)
                  IF(WQ.EQ.'D')REG(9)=REG(4)
                  IF(WQ.EQ.'E')REG(9)=REG(5)
                  IF(WQ.EQ.'F')REG(9)=REG(6)
                  IF(WQ.EQ.'G')REG(9)=REG(7)
                  IF(WQ.EQ.'H')REG(9)=REG(8)
                  IF(WQ.EQ.'Y')REG(9)=REG(10)
                  IF(WQ.EQ.'Z')REG(9)=REG(11)
                  IF(WQ.EQ.'T')REG(9)=REG(12)
                  IF(WQ.EQ.'IX')REG(9)=REG(13)
                  IF(WQ.EQ.'IY')REG(9)=REG(14)
                  IF(WQ.EQ.'IZ')REG(9)=REG(15)
                  IF(WQ.EQ.'IT')REG(9)=REG(16)
                  IF(WQ.EQ.'I') REG(9)=REG(17)
                  IF(WQ.EQ.'ITEST')REG(9)=REG(18)
                  IF(WQ.EQ.'J')REG(9)=REG(19)
                  IF(WQ.EQ.'JTEST')REG(9)=REG(20)
                  IF(WQ.EQ.'K')REG(9)=REG(21)
                  IF(WQ.EQ.'L')REG(9)=REG(22)
                  IF(WQ.EQ.'M')REG(9)=REG(23)
                  IF(WQ.EQ.'N')REG(9)=REG(24)
                  IF(WQ.EQ.'KTEST')REG(9)=REG(25)
                  IF(WQ.EQ.'LTEST')REG(9)=REG(26)
                  IF(WQ.EQ.'MTEST')REG(9)=REG(27)
                  IF(WQ.EQ.'NTEST')REG(9)=REG(28)
                  IF(TF(NEST).EQ.1) THEN
C               TRACE IS ON. DON'T PRINT MACRO COMMANDS HOWEVER
                      CALL TRACER
C
C       F41 NE 1, TRACE IS OFF, PROCEED AS NORMAL
                  END IF
                  WC=WCOLD
                  WQ=WQOLD
                  WS=STROLD
                  SQ=SQOLD
                  SN=SNOLD
                  SST=STOLD
                  S1=S1OLD
                  S2=S2OLD
                  S3=S3OLD
                  S4=S4OLD
                  S5=S5OLD
                  DF1=DF1OLD
                  DF2=DF2OLD
                  DF3=DF3OLD
                  DF4=DF4OLD
                  DF5=DF5OLD
                  W1=W1OLD
                  W2=W2OLD
                  W3=W3OLD
                  W4=W4OLD
                  W5=W5OLD
                  GO TO 50
              END IF
          END IF
C
C***********************************************************************
C       IF A (BP) COMMAND IS FOUND IN THE NORMAL PROCESS OF MACRO
C       EXECUTION, JUST INCREMENT THE LINE COUNTER I BY 1 AND
C       PROCEED BY JUMPING TO 54
          IF(MACCW(I).EQ.'BP') THEN
              I=I+1
              GO TO 54
C       DON'T DO ANYTHING
          END IF
C***********************************************************************
C       NOW SET THE PARAMETERS UP TO PASS BACK TO CONTROL
C       IN THE SAME WAY PARAMETERS WERE PASSED TO CONTROL
C       FROM PROCESS.
C
C       NOW WHAT IF WE ENCOUNTER A RETURN COMMAND.
C       IN THAT CASE WE STOP EXECUTION OF THE CURRENT MACRO.
C       THEN DECREMENT NEST BY 1,RELOAD THE MACRO DESIGNATED
C       BY NESTIJ(NEST) AND BEGIN EXECUTING IT FROM LINE NUMBER
C       NESTI(NEST).
C
          IF(MACCW(I).EQ.'RETURN') THEN
              NEST=NEST-1
              IF(NEST.EQ.-1) THEN
                  NESTER=0
              ELSE
                  NESTER=NEST
              END IF
C     RESTORE THE REAL ORIGINAL INPUT VALUES FROM THE CALL FROM THE
C     CMD LEVEL
              IF(NEST.EQ.-1) THEN
                  NEST=0
                  NESTER=0
C       STOP RUNNING MACROS AND RETURN SETTING F4=0
                  F4=0
                  WC=WCOLD
                  WQ=WQOLD
                  WS=STROLD
                  SQ=SQOLD
                  SN=SNOLD
                  SST=STOLD
                  S1=S1OLD
                  S2=S2OLD
                  S3=S3OLD
                  S4=S4OLD
                  S5=S5OLD
                  DF1=DF1OLD
                  DF2=DF2OLD
                  DF3=DF3OLD
                  DF4=DF4OLD
                  DF5=DF5OLD
                  W1=W1OLD
                  W2=W2OLD
                  W3=W3OLD
                  W4=W4OLD
                  W5=W5OLD
                  MWQ(NESTER)  =OMWQ(NESTER)
                  MWS(NESTER)  =OMWS(NESTER)
                  MSQ(NESTER)  =OMSQ(NESTER)
                  MST(NESTER)  =OMST(NESTER)
                  MNW(NESTER,1)=OMNW(NESTER,1)
                  MNW(NESTER,2)=OMNW(NESTER,2)
                  MNW(NESTER,3)=OMNW(NESTER,3)
                  MNW(NESTER,4)=OMNW(NESTER,4)
                  MNW(NESTER,5)=OMNW(NESTER,5)
                  MDF(NESTER,1)=OMDF(NESTER,1)
                  MDF(NESTER,2)=OMDF(NESTER,2)
                  MDF(NESTER,3)=OMDF(NESTER,3)
                  MDF(NESTER,4)=OMDF(NESTER,4)
                  MDF(NESTER,5)=OMDF(NESTER,5)
                  RETURN
              ELSE
                  NESTER=NEST
                  MWQ(NESTER)  =OMWQ(NESTER)
                  MWS(NESTER)  =OMWS(NESTER)
                  MSQ(NESTER)  =OMSQ(NESTER)
                  MST(NESTER)  =OMST(NESTER)
                  MNW(NESTER,1)=OMNW(NESTER,1)
                  MNW(NESTER,2)=OMNW(NESTER,2)
                  MNW(NESTER,3)=OMNW(NESTER,3)
                  MNW(NESTER,4)=OMNW(NESTER,4)
                  MNW(NESTER,5)=OMNW(NESTER,5)
                  MDF(NESTER,1)=OMDF(NESTER,1)
                  MDF(NESTER,2)=OMDF(NESTER,2)
                  MDF(NESTER,3)=OMDF(NESTER,3)
                  MDF(NESTER,4)=OMDF(NESTER,4)
                  MDF(NESTER,5)=OMDF(NESTER,5)
              END IF
C       NEST NOT -1, PROCEED
              IF(.NOT.NESFUN(NEST)) THEN
C      'RETURNING EXECUTION TO ',MCDIR1(NESTIJ(NEST))
                  NF=NESTIJ(NEST)
                  CALL MACLOD
                  SPOINT=NESTI(NEST)
                  GO TO 85
              ELSE
C       RETURNING TO A FUNCTION

C     'RETURNING EXECUTION TO ',FCDIR1(NESTIJ(NEST))
                  NF=NESTIJ(NEST)
                  CALL MACLOD
                  SPOINT=NESTI(NEST)
                  GO TO 85
              END IF
C       COMMAND WAS NOT A RETURN, PROCEED
          END IF
C       NOW WHAT IF WE ENCOUNTER A PAUSE COMMAND.
C       IN THAT CASE WE TEMPORARILY STOP EXECUTION OF THE CURRENT MACRO.
C       UNTIL THE RETURN KEY IS PRESSED
C
          IF(MACCW(I).EQ.'PAUSE') THEN
              CALL MACPAUSE
              GO TO 50
          END IF
C       SET CSUB FOR CSUBS
          IF(MACCW(I).EQ.'CSUB'.OR.MACCW(I).EQ.'CRSUB') THEN
              IF(MACCW(I).EQ.'CRSUB') CRSUB=.TRUE.
              CSUB=1
              GO TO 50
          ELSE
              IF(CSUB.EQ.1) THEN
                  CSUB=1
              ELSE
                  CSUB=0
              END IF
          END IF
C       HANDLE QSUB DV
          IF(MACCW(I).EQ.'QSUB'.AND.MACQW(I).EQ.'DV'.AND.
     1    MSQ(NESTER).EQ.0) THEN
              MWQ(NESTER)=(MACSTR(I)(1:8))
              GO TO 50
          ELSE
              IF(MACCW(I).EQ.'QSUB'.AND.MACQW(I).EQ.'DV'.AND.
     1        MSQ(NESTER).NE.0) GO TO 50
          END IF
C       HANDLE SSUB DV
          IF(MACCW(I).EQ.'SSUB'.AND.MACQW(I).EQ.'DV'.AND.
     1    MST(NESTER).EQ.0) THEN
              MWS(NESTER)=(MACSTR(I)(1:80))
              GO TO 50
          ELSE
              IF(MACCW(I).EQ.'SSUB'.AND.MACQW(I).EQ.'DV'.AND.
     1        MST(NESTER).NE.0) GO TO 50
          END IF
C       SET QSUB OR QSUBS
          IF(MACCW(I).EQ.'QSUB'.AND.MACQW(I).EQ.' '.OR.
     1    MACCW(I).EQ.'QRSUB'.AND.MACQW(I).EQ.' ') THEN
              IF(MACCW(I).EQ.'QSUB') QRSUB=.FALSE.
              IF(MACCW(I).EQ.'QRSUB') QRSUB=.TRUE.
              QSUB=1
              GO TO 50
          ELSE
              IF(QSUB.EQ.1) THEN
                  QSUB=1
              ELSE
                  QSUB=0
              END IF
          END IF
C       SET SSUB
          IF(MACCW(I).EQ.'SSUB'.AND.MACQW(I).EQ.' ') THEN
              SSUB=1
              GO TO 50
          ELSE
          END IF
C
C       NOW SET UP THE VARIABLES TO TRACK THE OPERATION OF
C       ACCSUB. ACCSUB SET TO 1 MEANS ACCSUB IN EFFECT.
C       ACCSUB=0 MEANS ACCSUB NOT IN EFFECT.
C       ACCWRD IS THE REGISTER NAME TO USE.
C       ACCCNT IS THE COUNTER TRACKING THE NUMBER OF TIMES
C       ACCSUB WILL BE USED BEFORE ACCSUB IS SET BACK TO 0.
C       THE VALUES OF ACCSUB,ACCWRD, AND ACCCNT WILL BE PASSED
C       TO APPROPRIATE COMMANDS OUTSIDE OF THIS SUBROUTINE
C       USING NAMED COMMON COMMON/ACCSB
C
          IF(MACCW(I).EQ.'ACCSUB') THEN
              ACCSUB=1
              ACCWRD=MACQW(I)
              HOLDER=MACNW(1,I)
              ACCCNT=INT(HOLDER)
          ELSE
C       MACCW(I) NOT ACCSUB
              IF(ACCSUB.EQ.1) THEN
                  ACCSUB=1
              ELSE
                  ACCSUB=0
              END IF
          END IF
C
C       HANDLE NSUB DV
          NSUBDV=.FALSE.
          IF(MACCW(I).EQ.'NSUB'.AND.MACQW(I).EQ.'DV') THEN
C       MACNW'S ARE THE NUMERIC WORDS OF THE NSUB DV STATEMENT.
C       MNW'S ARE THE MACRO COMMAND LINE NUMERIC WORDS
C       MDF'S TRACK WHETHER THE MACRO COMMAND LINE NUMERIC
C       WORDS ARE DEFAULT BLANK OR EXIT IN A NON-DEFAULT WAY.
              IF(MDF(NESTER,1).EQ.1) MNW(NESTER,1)=MACNW(1,I)
              IF(MDF(NESTER,2).EQ.1) MNW(NESTER,2)=MACNW(2,I)
              IF(MDF(NESTER,3).EQ.1) MNW(NESTER,3)=MACNW(3,I)
              IF(MDF(NESTER,4).EQ.1) MNW(NESTER,4)=MACNW(4,I)
              IF(MDF(NESTER,5).EQ.1) MNW(NESTER,5)=MACNW(5,I)
              NSUBDV=.TRUE.
              GO TO 50
          END IF
C       ************************************************************
C       SET VARIOUS NSUBS
C               FIRST JUST NSUB
          IF(MACCW(I).EQ.'NSUB') THEN
              IF(NSUB.EQ.0) THEN
                  NSUB=1
                  CT=1
C       THE H ARRAY CONTAINS NUMERIC WORD VALUES WHICH ARE
C       FOR NUMERIC WORD 1 AND 2 ACT AS ADDRESSES FOR THE
C       EXECUTABLE STATEMENT WHICH FOLLOWS THE NSUBS.
                  H(1,CT)=MACNW(1,I)
                  H(2,CT)=MACNW(2,I)
                  IF(H(2,CT).EQ.0.0D0) REG9(CT)=REG(9)
                  H(3,CT)=MACNW(3,I)
                  H(4,CT)=MACNW(4,I)
                  H(5,CT)=MACNW(5,I)
              ELSE
                  CT=CT+1
                  IF(CT.GT.10) THEN
                      OUTLYNE='MAXIMUM NUMBER OF CONSECUTIVE NSUBS = 10'
                      CALL SHOWIT(1)
                      OUTLYNE='HAS BEEN REACHED, MACRO EXECUTION ABORTED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      H(1,CT)=MACNW(1,I)
                      H(2,CT)=MACNW(2,I)
                      IF(H(2,CT).EQ.0.0D0) REG9(CT)=REG(9)
                      H(3,CT)=MACNW(3,I)
                      H(4,CT)=MACNW(4,I)
                      H(5,CT)=MACNW(5,I)
                  END IF
              END IF
              IF(MACQW(I).EQ.' ') THEN
                  Q(CT)=1
C       THE CURRENT NSUB REFERENCED BY J IS A SIMPLE NSUB
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
C
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='MACRO EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=MNW(NESTER,1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=MNW(NESTER,2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=MNW(NESTER,3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=MNW(NESTER,4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=MNW(NESTER,5)
                      CALL ACC1(CT)
                      CALL MACDEF
                      CALL MACX1
                      CALL ACC2
                      W1=0.0D0
                      W2=0.0D0
                      W3=0.0D0
                      W4=0.0D0
                      W5=0.0D0
                      S1=0
                      S2=0
                      S3=0
                      S4=0
                      S5=0
                      SN=0
                      DF1=1
                      DF2=1
                      DF3=1
                      DF4=1
                      DF5=1
                      GO TO 5111
                  END IF
              END IF
              IF(MACQW(I).EQ.'RA') THEN
                  Q(CT)=2
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RA QUALIFIER.
C       THIS IS JUST LIKE NSUB EXCEPT THAT THE MULTIPLIER IS NOW
C                       MNW(NESTER,INT(DABS(H(3,CT)))) INSTEAD OF
C       JUST            H(3,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE MACRO USING
C       NSUB DV.
C
                  H(3,CT)=MNW(NESTER,INT(DABS(H(3,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='MACRO EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=MNW(NESTER,1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=MNW(NESTER,2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=MNW(NESTER,3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=MNW(NESTER,4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=MNW(NESTER,5)
                      CALL ACC1(CT)
                      CALL MACDEF
                      CALL MACX1
                      CALL ACC2
                      GO TO 5111
                  END IF
              END IF
              IF(MACQW(I).EQ.'RB') THEN
                  Q(CT)=3
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RB QUALIFIER.
C       THIS IS JUST LIKE NSUB EXCEPT THAT THE MULTIPLIER IS NOW
C                       MNW(NESTER,INT(DABS(H(4,CT)))) INSTEAD OF
C       JUST            H(4,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE MACRO USING
C       NSUB DV.
C
                  H(4,CT)=MNW(NESTER,INT(DABS(H(4,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='MACRO EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=MNW(NESTER,1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=MNW(NESTER,2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=MNW(NESTER,3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=MNW(NESTER,4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=MNW(NESTER,5)
                      CALL ACC1(CT)
                      CALL MACDEF
                      CALL MACX1
                      CALL ACC2
                      GO TO 5111
                  END IF
              END IF
              IF(MACQW(I).EQ.'RC') THEN
                  Q(CT)=4
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RC QUALIFIER.
C       THIS IS JUST LIKE NSUB EXCEPT THAT THE MULTIPLIER IS NOW
C                       MNW(NESTER,INT(DABS(H(5,CT)))) INSTEAD OF
C       JUST            H(5,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE MACRO USING
C       NSUB DV.
C
                  H(5,CT)=MNW(NESTER,INT(DABS(H(5,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='MACRO EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=MNW(NESTER,1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=MNW(NESTER,2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=MNW(NESTER,3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=MNW(NESTER,4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=MNW(NESTER,5)
                      CALL ACC1(CT)
                      CALL MACDEF
                      CALL MACX1
                      CALL ACC2
                      GO TO 5111
                  END IF
              END IF
              IF(MACQW(I).EQ.'RAB') THEN
                  Q(CT)=5
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RAB QUALIFIER.
C       THIS IS RA AND RB
C
                  H(3,CT)=MNW(NESTER,INT(DABS(H(3,CT))))
                  H(4,CT)=MNW(NESTER,INT(DABS(H(4,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='MACRO EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=MNW(NESTER,1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=MNW(NESTER,2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=MNW(NESTER,3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=MNW(NESTER,4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=MNW(NESTER,5)
                      CALL ACC1(CT)
                      CALL MACDEF
                      CALL MACX1
                      CALL ACC2
                      GO TO 5111
                  END IF
              END IF
              IF(MACQW(I).EQ.'RAC') THEN
                  Q(CT)=6
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RAC QUALIFIER.
C       THIS IS RA AND RC
C
                  H(3,CT)=MNW(NESTER,INT(DABS(H(3,CT))))
                  H(5,CT)=MNW(NESTER,INT(DABS(H(5,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='MACRO EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=MNW(NESTER,1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=MNW(NESTER,2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=MNW(NESTER,3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=MNW(NESTER,4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=MNW(NESTER,5)
                      CALL ACC1(CT)
                      CALL MACDEF
                      CALL MACX1
                      CALL ACC2
                      GO TO 5111
                  END IF
              END IF
              IF(MACQW(I).EQ.'RBC') THEN
                  Q(CT)=7
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RA QUALIFIER.
C       THIS IS RB AND RC
C
                  H(4,CT)=MNW(NESTER,INT(DABS(H(4,CT))))
                  H(5,CT)=MNW(NESTER,INT(DABS(H(5,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='MACRO EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=MNW(NESTER,1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=MNW(NESTER,2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=MNW(NESTER,3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=MNW(NESTER,4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=MNW(NESTER,5)
                      CALL ACC1(CT)
                      CALL MACDEF
                      CALL MACX1
                      CALL ACC2
                      GO TO 5111
                  END IF
              END IF
              IF(MACQW(I).EQ.'RABC') THEN
                  Q(CT)=8
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RABC QUALIFIER.
C       THIS IS RA RB AND RC
C
                  H(3,CT)=MNW(NESTER,INT(DABS(H(3,CT))))
                  H(4,CT)=MNW(NESTER,INT(DABS(H(4,CT))))
                  H(5,CT)=MNW(NESTER,INT(DABS(H(5,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='MACRO EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=MNW(NESTER,1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=MNW(NESTER,2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=MNW(NESTER,3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=MNW(NESTER,4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=MNW(NESTER,5)
                      CALL ACC1(CT)
                      CALL MACDEF
                      CALL MACX1
                      CALL ACC2
                      GO TO 5111
                  END IF
              END IF
              IF(MACQW(I).NE.' '.AND.MACQW(I).NE.'RA'.AND.
     1        MACQW(I).NE.'RB'.AND.MACQW(I).NE.'RC'.AND.MACQW(I).NE.
     2        'RAB'.AND.MACQW(I).NE.'RAC'.AND.MACQW(I).NE.'RBC'.AND.
     3        MACQW(I).NE.'RABC') Q(CT)=99
              GO TO 50
          ELSE
C       MACCW(I) NOT NSUB BUT
              IF(NSUB.EQ.1) THEN
                  NSUB=1
              ELSE
                  NSUB=0
              END IF
C               PROCEED
          END IF
C
C       ************************************************************
C               HANDEL MOVE NW
C
          IF(MACCW(I).EQ.'MOVE'.AND.MACQW(I).EQ.'NW') THEN
              IF((INT(DABS(MACNW(1,I)))).EQ.0) MACNW(1,I)=INT(REG(17))
              IF((INT(DABS(MACNW(1,I)))).EQ.0) THEN
                  OUTLYNE='VALUE OF I REGISTER EQUAL TO 0'
                  CALL SHOWIT(1)
                  OUTLYNE='FOR (MOVE NW,0) COMMAND'
                  CALL SHOWIT(1)
                  OUTLYNE='DEFAULT: CONTENTS OF NW1 MOVED INTO ACCUMULATOR'
                  CALL SHOWIT(1)
                  REG(40)=REG(9)
                  REG(9)=MNW(NESTER,1)
                  GO TO 50
              END IF
              IF(INT(DABS(MACNW(1,I))).EQ.0)
     1                MNW(NESTER,INT(DABS(MACNW(1,I))))=REG(9)
              IF(INT(DABS(MACNW(1,I))).GT.5) THEN
                  OUTLYNE='VALUE OF I REGISTER GREATER THAN 5'
                  CALL SHOWIT(1)
                  OUTLYNE='FOR (MOVE NW,0) COMMAND'
                  CALL SHOWIT(1)
                  OUTLYNE='DEFAULT: CONTENTS OF NW1 MOVED INTO ACCUMULATOR'
                  CALL SHOWIT(1)
                  REG(40)=REG(9)
                  REG(9)=MNW(NESTER,1)
                  GO TO 50
              END IF
              REG(40)=REG(9)
              REG(9)=MNW(NESTER,INT(MACNW(1,I)))
              GO TO 50
          END IF
C       ************************************************************
C                       HANDLE PUTR
C
C       PUTR IS ONLY VALID FROM INSIDE A MACRO.
C       THIS COMMAND REPLACES INT(MACNW(1,I)) TH NUMERIC WORD
C       OF THE INPUT MACRO COMMAND RECORD BY THE
C       NUMBER IN THE NAMED REGISTER. IF MACNW(1,I) IS ZERO, THE INTEGER
C       VALUE OF THE INDEX REGISTER I WILL BE USED INSTEAD.
C       IF IT IS ZERO ALSO THE THE FIRST NUMERIC WORD WILL BE
C       THE DEFAULT. IF THE INTEGER VALUE OF THE I REGISTER IS GREATER
C       THAN 5  THEN THE FIRST NUMERIC WORD WILL ALSO BE THE DEFAULT.
C       VALID REG NAMES ARE: A,B,C,D,E,F,G,X,Y,Z,T,
C       IX,IY,IZ,IT
C
          IF(MACCW(I).EQ.'PUTR') THEN
              IF(MACQW(I).NE.'A'.AND.MACQW(I).NE.'B'
     1        .AND.MACQW(I).NE.'C'.AND.MACQW(I).NE.'D'.AND.
     2        MACQW(I).NE.'E'.AND.MACQW(I).NE.'F'.AND.
     3        MACQW(I).NE.'G'.AND.MACQW(I).NE.'H'.AND.MACQW(I).NE.'Y'
     4        .AND.MACQW(I).NE.'Z'.AND.MACQW(I).NE.'T'.AND.
     5        MACQW(I).NE.'IX'.AND.MACQW(I).NE.'IY'.AND.MACQW(I).NE.'IZ'
     6        .AND.MACQW(I).NE.'IT'.AND.MACQW(I).NE.' '
     7        .AND.MACQW(I).NE.'X') THEN
                  OUTLYNE='INVALID REGISTER NAME FOR (PUTR)'
                  CALL SHOWIT(1)
                  GO TO 50
              ELSE
                  IF((INT(DABS(MACNW(1,I))).EQ.0)) MACNW(1,I)=REG(17)
                  IF((INT(DABS(MACNW(1,I))).EQ.0)) THEN
                      OUTLYNE='VALUE OF I REGISTER EQUAL TO 0'
                      CALL SHOWIT(1)
                      OUTLYNE='FOR THE (PUTR,0) COMMAND'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD 1 USED AS THE DEFAULT'
                      CALL SHOWIT(1)
                      MACNW(1,I)=1.0D0
                  END IF
C
                  IF(INT(DABS(MACNW(1,I))).GT.5) THEN
                      OUTLYNE='VALUE OF I REGISTER GREATER THAN 5'
                      CALL SHOWIT(1)
                      OUTLYNE='FOR THE (PUTR,0) COMMAND'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD 1 USED AS THE DEFAULT'
                      CALL SHOWIT(1)
                      MACNW(1,I)=1.0
                  END IF
                  KK=INT(DABS(MACNW(1,I)))
                  IF(ACCSUB.EQ.1) THEN
                      IF(MACQW(I).EQ.'ACC'.OR.MACQW(I).EQ.'X'.OR.
     1                MACQW(I).EQ.' ') THEN
                          MACQW(I)=ACCWRD
                          ACCCNT=ACCCNT-1
                          IF(ACCCNT.EQ.0) ACCSUB=0
                      END IF
C       ACCSUB WAS NOT IN EFFECT
                  END IF
C
                  IF(MACQW(I).EQ.'A')  MNW(NESTER,KK)=REG(1)
                  IF(MACQW(I).EQ.'B')  MNW(NESTER,KK)=REG(2)
                  IF(MACQW(I).EQ.'C')  MNW(NESTER,KK)=REG(3)
                  IF(MACQW(I).EQ.'D')  MNW(NESTER,KK)=REG(4)
                  IF(MACQW(I).EQ.'E')  MNW(NESTER,KK)=REG(5)
                  IF(MACQW(I).EQ.'F')  MNW(NESTER,KK)=REG(6)
                  IF(MACQW(I).EQ.'G')  MNW(NESTER,KK)=REG(7)
                  IF(MACQW(I).EQ.'H')  MNW(NESTER,KK)=REG(8)
                  IF(MACQW(I).EQ.'X')  MNW(NESTER,KK)=REG(9)
                  IF(MACQW(I).EQ.' ')  MNW(NESTER,KK)=REG(9)
                  IF(MACQW(I).EQ.'Y')  MNW(NESTER,KK)=REG(10)
                  IF(MACQW(I).EQ.'Z')  MNW(NESTER,KK)=REG(11)
                  IF(MACQW(I).EQ.'T')  MNW(NESTER,KK)=REG(12)
                  IF(MACQW(I).EQ.'IX') MNW(NESTER,KK)=REG(13)
                  IF(MACQW(I).EQ.'IY') MNW(NESTER,KK)=REG(14)
                  IF(MACQW(I).EQ.'IZ') MNW(NESTER,KK)=REG(15)
                  IF(MACQW(I).EQ.'IT') MNW(NESTER,KK)=REG(16)
                  GO TO 50
              END IF
          END IF
C       ************************************************************
          WCOLD=WC
          WQOLD=WQ
          STROLD=WS
          SNOLD=SN
          SQOLD=SQ
          STOLD=SST
          S1OLD=S1
          S2OLD=S2
          S3OLD=S3
          S4OLD=S4
          S5OLD=S5
          DF1OLD=DF1
          DF2OLD=DF2
          DF3OLD=DF3
          DF4OLD=DF4
          DF5OLD=DF5
          W1OLD=W1
          W2OLD=W2
          W3OLD=W3
          W4OLD=W4
          W5OLD=W5
          EE1=MACCW(I)
          WC=EE1
          EE1=MACQW(I)
          WQ=EE1
          EE2= MACSTR(I)
          WS=EE2
          W1=MACNW(1,I)
          W2=MACNW(2,I)
          W3=MACNW(3,I)
          W4=MACNW(4,I)
          W5=MACNW(5,I)
          SB1=MACSTA(1,I)
          SB2=MACSTA(2,I)
          SC1=MACSTA(3,I)
          SC2=MACSTA(4,I)
          SQ=MACSTA(5,I)
          SST=MACSTA(6,I)
          S1=MACSTA(7,I)
          S2=MACSTA(8,I)
          S3=MACSTA(9,I)
          S4=MACSTA(10,I)
          S5=MACSTA(11,I)
          DF1=MACSTA(12,I)
          DF2=MACSTA(13,I)
          DF3=MACSTA(14,I)
          DF4=MACSTA(15,I)
          DF5=MACSTA(16,I)
          SN=MACSTA(17,I)
          STI=MACSTA(18,I)
C       MACSTA(19,I) AND MACSTA(20,I) NOT YET USED
          MACSTA(19,I)=0
          MACSTA(20,I)=0
C
C       IMPLEMENT SUBSTITUTIONS HERE
C       HANDLE CSUB
          IF(CSUB.EQ.1) THEN
              IF(.NOT.CRSUB) WC=MWQ(NESTER)
              IF(CRSUB) WC=AGPREG(0)(1:8)
              IF(CRSUB) CRSUB=.FALSE.
          END IF
C       HANDLE QSUB
          IF(QSUB.EQ.1) THEN
              IF(.NOT.QRSUB) WQ=MWQ(NESTER)
              IF(QRSUB) WQ=AGPREG(0)(1:8)
              IF(QRSUB) QRSUB=.FALSE.
C       IF WQ IS THE NAME OF THE ACCUMULATOR THEN
              IF(ACCSUB.EQ.1) THEN
                  IF(WQ.EQ.'ACC'.OR.WQ.EQ.'X'.OR.WQ.EQ.' ') THEN
                      WQ=ACCWRD
                      ACCCNT=ACCCNT-1
                      IF(ACCCNT.EQ.0) ACCSUB=0
                  END IF
C       ACCSUB WAS NOT IN EFFECT
              END IF
              SQ=1
          END IF
C       HANDLE SSUB
          IF(SSUB.EQ.1) THEN
              WS=MWS(NESTER)
              SST=1
          END IF
C
C       HANDLE NSUB
C
          IF(NSUB.EQ.1) THEN
              CTF=CT
              IF(CT.GT.10) THEN
                  OUTLYNE='MAXIMUM NUMBER OF CONSECUTIVE NSUBS = 10'
                  CALL SHOWIT(1)
                  OUTLYNE='HAS BEEN REACHED, MACRO EXECUTION ABORTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
C       PROCEED
              END IF
              DO 400 CT=1,CTF
                  CALL MACDEF
                  IF(Q(CT).EQ.1) THEN
C       THE CURRENT NSUB REFERENCED BY J IS A SIMPLE NSUB
C                IF(INT(DABS(H(1,CT))).EQ.0) THEN
C                THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W1
                          CALL MACX1
                          W1=PASS2
                          CALL ACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W2
                          CALL MACX1
                          W2=PASS2

                          CALL ACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W3
                          CALL MACX1
                          W3=PASS2
                          CALL ACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W4
                          CALL MACX1
                          W4=PASS2
                          CALL ACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W5
                          CALL MACX1
                          W5=PASS2
                          CALL ACC3
                          DF5=0
                          S5=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE, NSUB IGNORED'
                          CALL SHOWIT(1)
                          GO TO 400
                      END IF
                  END IF
                  IF(Q(CT).EQ.2) THEN
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RA QUALIFIER.
C       THIS IS JUST LIKE NSUB EXCEPT THAT THE MULTIPLIER IS NOW
C                       MNW(NESTER,INT(DABS(H(3,CT)))) INSTEAD OF
C       JUST            H(3,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE MACRO USING
C       NSUB DV.
C
                      H(3,CT)=MNW(NESTER,INT(DABS(H(3,CT))))
C
C                IF(INT(DABS(H(1,CT))).EQ.0) THEN
C                THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W1
                          CALL MACX1
                          W1=PASS2
                          CALL ACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W2
                          CALL MACX1
                          W2=PASS2
                          CALL ACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W3
                          CALL MACX1
                          W3=PASS2
                          CALL ACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W4
                          CALL MACX1
                          W4=PASS2
                          CALL ACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W5
                          CALL MACX1
                          W5=PASS2
                          CALL ACC3
                          DF5=0
                          S5=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE, NSUB IGNORED'
                          CALL SHOWIT(1)
                          GO TO 400
                      END IF
                  END IF
                  IF(Q(CT).EQ.3) THEN
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RB QUALIFIER.
C       THIS IS JUST LIKE NSUB EXCEPT THAT THE MULTIPLIER IS NOW
C                       MNW(NESTER,INT(DABS(H(4,CT)))) INSTEAD OF
C       JUST            H(4,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE MACRO USING
C       NSUB DV.
C
                      H(4,CT)=MNW(NESTER,INT(DABS(H(4,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C                THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W1
                          CALL MACX1
                          W1=PASS2
                          CALL ACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W2
                          CALL MACX1
                          W2=PASS2
                          CALL ACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W3
                          CALL MACX1
                          W3=PASS2
                          CALL ACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W4
                          CALL MACX1
                          W4=PASS2
                          CALL ACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W5
                          CALL MACX1
                          W5=PASS2
                          CALL ACC3
                          DF5=0
                          S5=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE, NSUB IGNORED'
                          CALL SHOWIT(1)
                          GO TO 400
                      END IF
                  END IF
                  IF(Q(CT).EQ.4) THEN
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RC QUALIFIER.
C       THIS IS JUST LIKE NSUB EXCEPT THAT THE MULTIPLIER IS NOW
C                       MNW(NESTER,INT(DABS(H(5,CT)))) INSTEAD OF
C       JUST            H(5,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE MACRO USING
C       NSUB DV.
C
                      H(5,CT)=MNW(NESTER,INT(DABS(H(5,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C               THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W1
                          CALL MACX1
                          W1=PASS2
                          CALL ACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W2
                          CALL MACX1
                          W2=PASS2
                          CALL ACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W3
                          CALL MACX1
                          W3=PASS2
                          CALL ACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W4
                          CALL MACX1
                          W4=PASS2
                          CALL ACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W5
                          CALL MACX1
                          W5=PASS2
                          CALL ACC3
                          DF5=0
                          S5=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE, NSUB IGNORED'
                          CALL SHOWIT(1)
                          GO TO 400
                      END IF
                  END IF
                  IF(Q(CT).EQ.5) THEN
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RAB QUALIFIER.
C       THIS IS RA AND RB
C
                      H(3,CT)=MNW(NESTER,INT(DABS(H(3,CT))))
                      H(4,CT)=MNW(NESTER,INT(DABS(H(4,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C               THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W1
                          CALL MACX1
                          W1=PASS2
                          CALL ACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W2
                          CALL MACX1
                          W2=PASS2
                          CALL ACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W3
                          CALL MACX1
                          W3=PASS2
                          CALL ACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W4
                          CALL MACX1
                          W4=PASS2
                          CALL ACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W5
                          CALL MACX1
                          W5=PASS2
                          CALL ACC3
                          DF5=0
                          S5=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE, NSUB IGNORED'
                          CALL SHOWIT(1)
                          GO TO 400
                      END IF
                  END IF
                  IF(Q(CT).EQ.6) THEN
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RAC QUALIFIER.
C       THIS IS RA AND RC
C
                      H(3,CT)=MNW(NESTER,INT(DABS(H(3,CT))))
                      H(5,CT)=MNW(NESTER,INT(DABS(H(5,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C               THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W1
                          CALL MACX1
                          W1=PASS2
                          CALL ACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W2
                          CALL MACX1
                          W2=PASS2
                          CALL ACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W3
                          CALL MACX1
                          W3=PASS2
                          CALL ACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W4
                          CALL MACX1
                          W4=PASS2
                          CALL ACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W5
                          CALL MACX1
                          W5=PASS2
                          CALL ACC3
                          DF5=0
                          S5=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE, NSUB IGNORED'
                          CALL SHOWIT(1)
                          GO TO 400
                      END IF
                  END IF
                  IF(Q(CT).EQ.7) THEN
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RA QUALIFIER.
C       THIS IS RB AND RC
C
                      H(4,CT)=MNW(NESTER,INT(DABS(H(4,CT))))
                      H(5,CT)=MNW(NESTER,INT(DABS(H(5,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C               THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W1
                          CALL MACX1
                          W1=PASS2
                          CALL ACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W2
                          CALL MACX1
                          W2=PASS2
                          CALL ACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W3
                          CALL MACX1
                          W3=PASS2
                          CALL ACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W4
                          CALL MACX1
                          W4=PASS2
                          CALL ACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W5
                          CALL MACX1
                          W5=PASS2
                          CALL ACC3
                          DF5=0
                          S5=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE, NSUB IGNORED'
                          CALL SHOWIT(1)
                          GO TO 400
                      END IF
                  END IF
                  IF(Q(CT).EQ.8) THEN
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RABC QUALIFIER.
C       THIS IS RA RB AND RC
C
                      H(3,CT)=MNW(NESTER,INT(DABS(H(3,CT))))
                      H(4,CT)=MNW(NESTER,INT(DABS(H(4,CT))))
                      H(5,CT)=MNW(NESTER,INT(DABS(H(5,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C               THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W1
                          CALL MACX1
                          W1=PASS2
                          CALL ACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W2
                          CALL MACX1
                          W2=PASS2
                          CALL ACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W3
                          CALL MACX1
                          W3=PASS2
                          CALL ACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W4
                          CALL MACX1
                          W4=PASS2
                          CALL ACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='MACRO EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL ACC1(CT)
                          PASS2=W5
                          CALL MACX1
                          W5=PASS2
                          CALL ACC3
                          DF5=0
                          S5=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE, NSUB IGNORED'
                          CALL SHOWIT(1)
                          GO TO 400
                      END IF
                  END IF
                  IF(Q(CT).EQ.99) THEN
                      OUTLYNE='INVALID NSUB QUALIFIER, NSUB IGNORED'
                      CALL SHOWIT(1)
                  END IF
 400          CONTINUE
          END IF
C     NOW RESET THE H(X,CT) TO 0.0D0
          H(1:5,1:10)=0.0D0
          Q(1:10)=99
          CT=0
C               NSUB NOT IN EFFECT
C
C       HERE WE HANDEL BRANCHING COMMANDS
C       ARE THERE A BRANCHING COMMANDS BRU,BPOS,BREER,
C       BNEG,BZE,BRI,BRJ,BRK,BRL,BRM OR BRN  NEXT?
C     AND ALSO BRDQ, BRDF1 TO BRDF5
C     ALSO IF(X=0), IF(X>0) AND IF(X<0)
C     ALSO IF(X=Y), IF(X>Y) AND IF(X<Y)
C
          HAP1=.FALSE.
          IF(MACCW(I).EQ.'BRU') HAP1=.TRUE.
          IF(MACCW(I).EQ.'BPOS'.AND.
     1    REAL(REG(9)).GT.0.0) HAP1=.TRUE.
          IF(MACCW(I).EQ.'BNEG'.AND.
     1    REAL(REG(9)).LT.0.0) HAP1=.TRUE.
          IF(MACCW(I).EQ.'BZE'.AND.
     1    REAL(REG(9)).EQ.0.0) HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRI'.AND.
     1    REAL(REG(17)).EQ.REAL(REG(18))) HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRJ'.AND.
     1    REAL(REG(19)).EQ.REAL(REG(20))) HAP1=.TRUE.
          IF(MACCW(I).EQ.'BREER') HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRK'.AND.REAL(REG(21)).EQ.REAL(REG(25)))
     1    HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRL'.AND.REAL(REG(22)).EQ.REAL(REG(26)))
     1    HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRM'.AND.REAL(REG(23)).EQ.REAL(REG(27)))
     1    HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRN'.AND.REAL(REG(24)).EQ.REAL(REG(28)))
     1    HAP1=.TRUE.
          IF(MACCW(I).EQ.'IF(X>0)'.AND.REAL(REG(9)).GT.0.0) HAP1=.TRUE.
          IF(MACCW(I).EQ.'IF(X<0)'.AND.REAL(REG(9)).LT.0.0) HAP1=.TRUE.
          IF(MACCW(I).EQ.'IF(X=0)'.AND.REAL(REG(9)).EQ.0.0) HAP1=.TRUE.
          IF(MACCW(I).EQ.'IF(X>Y)'.AND.REAL(REG(9)).GT.REAL(REG(10)))
     1    HAP1=.TRUE.
          IF(MACCW(I).EQ.'IF(X<Y)'.AND.REAL(REG(9)).LT.REAL(REG(10)))
     1    HAP1=.TRUE.
          IF(MACCW(I).EQ.'IF(X=Y)'.AND.REAL(REG(9)).EQ.REAL(REG(10)))
     1    HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRDQ'.AND.MSQ(NESTER).EQ.0) HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRDF1'.AND.MDF(NESTER,1).EQ.1) HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRDF2'.AND.MDF(NESTER,2).EQ.1) HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRDF3'.AND.MDF(NESTER,3).EQ.1) HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRDF4'.AND.MDF(NESTER,4).EQ.1) HAP1=.TRUE.
          IF(MACCW(I).EQ.'BRDF5'.AND.MDF(NESTER,5).EQ.1) HAP1=.TRUE.
          IF(HAP1) THEN
              HAP1=.FALSE.
C               (YES THERE ARE AND A JUMP IS TO BE MADE)
C       WE BEGIN SEARCHING FOR THE BRANCH POINT OR WE JUMP
C       BY THE DESIGNATED NUMBER OF LINES. THE INDENTIFIER
C       OF THE BRANCH POINT TO JUMP TO IS PASSED VIA MACQW(I).
C       THE LC OR LINE COUNT IS PASSED VIA MACNW(1,I)
C       THE LC ALWAYS TAKES PRECIDENCE OVER THE IDENTIFIER.
C       IF THE IDENTIFIER IS BLANK AND THE LC IS BLANK
C       AN ERROR MESSAGE IS PRINTED AND MACRO EXECUTION
C       TERMINATES.
              IF(MACQW(I).EQ.' '.AND.MACNW(1,I).EQ.0) THEN
                  OUTLYNE='BRANCH CALL MISSING IDENTIFIER/LC'
                  CALL SHOWIT(1)
                  OUTLYNE='MACRO EXECUTION TERMINATING'
                  CALL SHOWIT(1)
              END IF
              IF(INT(MACNW(1,I)).NE.0) THEN
C       JUMP LINES
                  I=I+INT(MACNW(1,I))
                  GO TO 54
              ELSE
C       SEARCH FOR A BRANCH POINT
C       HERE WE SEARCH FROM THE FIRST LINE OF THE MACRO TO
C       THE END FOR THE TARGET BRANCH POINT. DO THIS WITHOUT
C       INCREMENTING I.
                  DO 53 LL=1,(MCDIR2(2,NESTIJ(NEST))-2)
                      IF(MACCW(LL).EQ.'BP'.AND.MACQW(LL).EQ.MACQW(I)) THEN
C       FOUND BRANCH POINT
                          I=LL+1
                          GO TO 54
                      END IF
 53               CONTINUE
C       IF YOU GOT HERE YOU NEVER FOUND THE BRANCH POINT
C       PRINT ERROR AND TERMINATE MACRO EXECUTION.
                  OUTLYNE='BRANCH POINT '//MACQW(I)//' NOT FOUND'
                  CALL SHOWIT(1)
                  OUTLYNE='MACRO EXECUTION TERMINATING'
                  CALL SHOWIT(1)
              END IF
          ELSE
C WAS A BRANCHING COMMAND FOUND BUT NO JUMP WAS TO BE MADE?
              IF(MACCW(I).EQ.'BREER') THEN
                  IF(.NOT.READERR) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BPOS'.OR.MACCW(I).EQ.'IF(X>0)') THEN
                  IF(REAL(REG(9)).LE.0.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BNEG'.OR.MACCW(I).EQ.'IF(X<0)') THEN
                  IF(REAL(REG(9)).GE.0.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BZE'.OR.MACCW(I).EQ.'IF(X=0)') THEN
                  IF(REAL(REG(9)).NE.0.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'IF(X>Y)') THEN
                  IF(REAL(REG(9)).LE.REAL(REG(10))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'IF(X<Y)') THEN
                  IF(REAL(REG(9)).GE.REAL(REG(10))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'IF(X=Y)') THEN
                  IF(REAL(REG(9)).NE.REAL(REG(10))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRI') THEN
                  IF(REAL(REG(17)).NE.REAL(REG(18)))THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRJ') THEN
                  IF(REAL(REG(19)).NE.REAL(REG(20))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRK') THEN
                  IF(REAL(REG(21)).NE.REAL(REG(25))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRL') THEN
                  IF(REAL(REG(22)).NE.REAL(REG(26))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRM') THEN
                  IF(REAL(REG(23)).NE.REAL(REG(27))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRN') THEN
                  IF(REAL(REG(24)).NE.REAL(REG(28))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRDQ') THEN
                  IF(MSQ(NESTER).EQ.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRDF1') THEN
                  IF(MDF(NESTER,1).EQ.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRDF2') THEN
                  IF(MDF(NESTER,2).EQ.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRDF3') THEN
                  IF(MDF(NESTER,3).EQ.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRDF4') THEN
                  IF(MDF(NESTER,4).EQ.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(MACCW(I).EQ.'BRDF5') THEN
                  IF(MDF(NESTER,5).EQ.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
          END IF
C     SET READERROR TO .FALSE.
          READERR=.FALSE.
C***********************************************************************
C***********************************************************************
C       IS THERE THE BRANCHING COMMAND (BRANCH)?
C
          IF(MACCW(I).EQ.'BRANCH') THEN
              IF(INT(DABS(MACNW(1,I))).GT.5) THEN
                  OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='MACRO EXECUTION ABORTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(MNW(NESTER,INT(DABS(MACNW(1,I)))).EQ.MACNW(2,I)) THEN
C
C
C               (YES THERE IS)
C       WE BEGIN SEARCHING FOR THE BRANCH POINT OR WE JUMP
C       BY THE DESIGNATED NUMBER OF LINES. THE INDENTIFIER
C       OF THE BRANCH POINT TO JUMP TO IS PASSED VIA MACQW(I).
C       THE LC OR LINE COUNT IS PASSED VIA MACNW(1,I)
C       THE LC ALWAYS TAKES PRECIDENCE OVER THE IDENTIFIER.
C       IF THE IDENTIFIER IS BLANK AND THE LC IS BLANK
C       AN ERROR MESSAGE IS PRINTED AND MACRO EXECUTION
C       TERMINATES.
                  IF(MACQW(I).EQ.' '.AND.MACNW(3,I).EQ.0) THEN
                      OUTLYNE='BRANCH CALL MISSING IDENTIFIER/LC'
                      CALL SHOWIT(1)
                      OUTLYNE='MACRO EXECUTION TERMINATING'
                      CALL SHOWIT(1)
                  END IF
                  IF(INT(MACNW(3,I)).NE.0) THEN
C       JUMP LINES
                      I=I+INT(MACNW(1,I))
                      GO TO 54
                  ELSE
C       SEARCH FOR A BRANCH POINT
C       HERE WE SEARCH FROM THE FIRST LINE OF THE MACRO TO
C       THE END FOR THE TARGET BRANCH POINT. DO THIS WITHOUT
C       INCREMENTING I.
                      DO 56 LL=1,(MCDIR2(2,NESTIJ(NEST))-2)
                          IF(MACCW(LL).EQ.'BP'.AND.MACQW(LL).EQ.MACQW(I)) THEN
C       FOUND BRANCH POINT
                              I=LL+1
                              GO TO 54
                          END IF
 56                   CONTINUE
C       IF YOU GOT HERE YOU NEVER FOUND THE BRANCH POINT
C       PRINT ERROR AND TERMINATE MACRO EXECUTION.
                      OUTLYNE='BRANCH POINT '//MACQW(I)//' NOT FOUND'
                      CALL SHOWIT(1)
                      OUTLYNE='MACRO EXECUTION TERMINATING'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              ELSE
C       BRANCH COMMAND FOUND BUT NO JUMP TO OCCUR
                  I=I+1
                  GO TO 54
              END IF
C       COMMAND WAS NOT THE (BRANCH) COMMAND, DO NOTHING
          END IF
C***********************************************************************
C***********************************************************************
C       IS THERE THE BRANCHING COMMAND (BRQ)?
C
          IF(MACCW(I).EQ.'BRQ'.AND.MWQ(NESTER).EQ.(MACSTR(I)(1:8))) THEN
C
C               (YES THERE IS AND A JUMP SHOULD BE MADE)
C       WE BEGIN SEARCHING FOR THE BRANCH POINT. THERE IS NO
C       JUMP USED IN THIS COMMAND.
C       THE INDENTIFIER
C       OF THE BRANCH POINT TO JUMP TO IS PASSED VIA MACQW(I).
C       IF THE IDENTIFIER IS BLANK
C       AN ERROR MESSAGE IS PRINTED AND MACRO EXECUTION
C       TERMINATES.
              IF(MACQW(I).EQ.' ') THEN
                  OUTLYNE='BRANCH CALL MISSING IDENTIFIER'
                  CALL SHOWIT(1)
                  OUTLYNE='MACRO EXECUTION TERMINATING'
                  CALL SHOWIT(1)
              END IF
C       SEARCH FOR A BRANCH POINT
C       HERE WE SEARCH FROM THE FIRST LINE OF THE MACRO TO
C       THE END FOR THE TARGET BRANCH POINT. DO THIS WITHOUT
C       INCREMENTING I.
              DO 57 LL=1,(MCDIR2(2,NESTIJ(NEST))-2)
                  IF(MACCW(LL).EQ.'BP'.AND.MACQW(LL).EQ.MACQW(I)) THEN
C       FOUND BRANCH POINT
                      I=LL+1
                      GO TO 54
                  END IF
 57           CONTINUE
C       IF YOU GOT HERE YOU NEVER FOUND THE BRANCH POINT
C       PRINT ERROR AND TERMINATE MACRO EXECUTION.
              OUTLYNE='BRANCH POINT '//MACQW(I)//' NOT FOUND'
              CALL SHOWIT(1)
              OUTLYNE='MACRO EXECUTION TERMINATING'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       IS THE BRQ COMMAND HERE WITH NO JUMP TO BE MADE?
              IF(MACCW(I).EQ.'BRQ') THEN
                  IF(MWQ(NESTER).NE.(MACSTR(I)(1:8))) THEN
                      I=I+1
                      GO TO 54
                  END IF
C       NO BRQ, JUST PROCEED
              END IF
          END IF
C***********************************************************************
C***********************************************************************
C       ARE THERE THE BRANCHING COMMAND (BRT OR BRF)?
C
          IF(MACCW(I).EQ.'BRT'.OR.MACCW(I).EQ.'BRF') THEN
              IF(MACNW(1,I).LT.0.0D0.OR.
     1           MACNW(2,I).LT.0.0D0.OR.
     2           MACNW(3,I).LT.0.0D0.OR.
     3           MACNW(4,I).LT.0.0D0.OR.
     4           MACNW(5,I).LT.0.0D0) THEN
                  OUTLYNE='INPUT TO BRT OR BRF MUST BE POSITIVE'
                  CALL SHOWIT(1)
                  OUTLYNE='MACRO EXECUTION TERMINATING'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       INPUT NOT NEGATIVE,PROCEED
          END IF
          IF(MACCW(I).EQ.'BRT'.OR.MACCW(I).EQ.'BRF') THEN
              IF(MACCW(I).EQ.'BRT'.AND.
     1           FLG(INT(MACNW(1,I))).GT.0.AND.
     2           FLG(INT(MACNW(2,I))).GT.0.AND.
     3           FLG(INT(MACNW(3,I))).GT.0.AND.
     4           FLG(INT(MACNW(4,I))).GT.0.AND.
     5           FLG(INT(MACNW(5,I))).GT.0.OR.
     6           MACCW(I).EQ.'BRF'.AND.FLG(INT(MACNW(1,I))).LT.0.OR.
     7           MACCW(I).EQ.'BRF'.AND.FLG(INT(MACNW(2,I))).LT.0.OR.
     8           MACCW(I).EQ.'BRF'.AND.FLG(INT(MACNW(3,I))).LT.0.OR.
     9           MACCW(I).EQ.'BRF'.AND.FLG(INT(MACNW(4,I))).LT.0.OR.
     1           MACCW(I).EQ.'BRF'.AND.FLG(INT(MACNW(5,I))).LT.0) THEN
C
C               (YES THERE ARE AND BRANCHING SHOULD OCCUR)
C       WE BEGIN SEARCHING FOR THE BRANCH POINT. THERE IS NO
C       JUMP USED IN THIS COMMAND.
C       THE INDENTIFIER
C       OF THE BRANCH POINT TO JUMP TO IS PASSED VIA MACQW(I).
C       IF THE IDENTIFIER IS BLANK
C       AN ERROR MESSAGE IS PRINTED AND MACRO EXECUTION
C       TERMINATES.
                  IF(MACQW(I).EQ.' ') THEN
                      OUTLYNE='BRANCH CALL MISSING IDENTIFIER'
                      CALL SHOWIT(1)
                      OUTLYNE='MACRO EXECUTION TERMINATING'
                      CALL SHOWIT(1)
                  END IF
C       SEARCH FOR A BRANCH POINT
C       HERE WE SEARCH FROM THE FIRST LINE OF THE MACRO TO
C       THE END FOR THE TARGET BRANCH POINT. DO THIS WITHOUT
C       INCREMENTING I.
                  DO 58 LL=1,(MCDIR2(2,NESTIJ(NEST))-2)
                      IF(MACCW(LL).EQ.'BP'.AND.MACQW(LL).EQ.MACQW(I)) THEN
C       FOUND BRANCH POINT
                          I=LL+1
                          GO TO 54
                      END IF
 58               CONTINUE
C       IF YOU GOT HERE YOU NEVER FOUND THE BRANCH POINT
C       PRINT ERROR AND TERMINATE MACRO EXECUTION.
                  OUTLYNE='BRANCH POINT '//MACQW(I)//' NOT FOUND'
                  CALL SHOWIT(1)
                  OUTLYNE='MACRO EXECUTION TERMINATING'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       BRANCHING DID NOT OCCUR,WHY
                  IF(MACCW(I).EQ.'BRT') THEN
                      IF( FLG(INT(MACNW(1,I))).GT.0..AND.
     2                   FLG(INT(MACNW(2,I))).GT.0.AND.
     3                   FLG(INT(MACNW(3,I))).GT.0.AND.
     4                   FLG(INT(MACNW(4,I))).GT.0.AND.
     5                   FLG(INT(MACNW(5,I))).GT.0) THEN
C               ACTION ALREADY TAKEN
                      ELSE
C       NO JUMP TO OCCUR
                          I=I+1
                          GO TO 54
                      END IF
C       NOT BRT
                  END IF
                  IF(MACCW(I).EQ.'BRF') THEN
                      IF( FLG(INT(MACNW(1,I))).LT.0..AND.
     2                   FLG(INT(MACNW(2,I))).LT.0.AND.
     3                   FLG(INT(MACNW(3,I))).LT.0.AND.
     4                   FLG(INT(MACNW(4,I))).LT.0.AND.
     5                   FLG(INT(MACNW(5,I))).LT.0) THEN
C               ACTION ALREADY TAKEN
                      ELSE
C       NO JUMP TO OCCUR
                          I=I+1
                          GO TO 54
                      END IF
C       NOT BRT
                  END IF
              END IF
C       NOT BRT OR BRF, CONTINUE
          END IF
C***********************************************************************
C                       HERE IS HANDELED THE TRACE ON/OFF FEATURE
          IF(MACCW(I).EQ.'TRACE') THEN
C
C       THE ONLY VALID QUALIFIERS ARE: ON OR OFF
C
              IF(MACQW(I).EQ.'ON'.OR.MACQW(I).EQ.'OFF') THEN
C       PROCESS
                  IF(MACQW(I).EQ.'ON') TF(NEST)=1
                  IF(MACQW(I).EQ.'OFF') TF(NEST)=0
                  GO TO 50
              ELSE
                  OUTLYNE='INVALID QUALIFIER WORD FOR (TRACE).'
                  CALL SHOWIT(1)
                  GO TO 50
              END IF
          END IF
C***********************************************************************
C***********************************************************************
C                       HERE IS HANDELED THE SSTEP ON/OFF FEATURE
          IF(MACCW(I).EQ.'SSTEP') THEN
C
C       THE ONLY VALID QUALIFIERS ARE: ON OR OFF
C
              IF(MACQW(I).EQ.'ON'.OR.MACQW(I).EQ.'OFF') THEN
C       PROCESS
                  IF(MACQW(I).EQ.'ON') SSTEP(NEST)=1
                  IF(MACQW(I).EQ.'OFF') SSTEP(NEST)=0
                  GO TO 50
              ELSE
                  OUTLYNE='INVALID QUALIFIER WORD FOR (SSTEP).'
                  CALL SHOWIT(1)
                  GO TO 50
              END IF
          END IF
C***********************************************************************
C
          IF(TF(NEST).EQ.1) THEN
C               TRACE IS ON. DON'T PRINT MACRO COMMANDS HOWEVER
              CALL TRACER
C
C       F41 NE 1, TRACE IS OFF, PROCEED AS NORMAL
          END IF
C     IF NSUBDV IS FALSE MEANING THERE WAS NO NSUBDV COMMAND AND
C     IF THERE WAS NO NUMERIC INPUT GIVEN WITH THE MACRO, THEN IF
C     DF1 THROUGH DF5 ARE 1, THEN SET THE CORRESPONDING S AND W VALUES
C     TO 0 AND 0.0
          IF(S1.EQ.0.AND.S2.EQ.0.AND.S3.EQ.0.AND.S4.EQ.0.AND.S5.EQ.0) THEN
              SN=0
          ELSE
              SN=1
          END IF
          IF(F27.EQ.0) THEN
C     NOT IN MERIT OR UPDATE MERIT
C       DOES THE MACRO OR FUNCTION CALL A FUNCTION ?
              DO K=1,10
                  IF(MACCW(I).EQ.FCDIR1(K)) THEN
C
C       WE JUST CALLED A FUNCTION
C       THERE IS NESTING. NESTIJ(0) IS THE BASE OF
C       THE NEST. HERE WE DO SEVERAL THINGS. WE SET NESTI(NEST)
C       EQUAL TO I+1, THE LINE NUMBER IN THE CALLING MACRO OR FUNCTION TO
C       WHICH THE EVENTUAL RETURN WILL BE MADE. INCREMENT
C       NEST BY ONE AND THEN SET
C       NESTIJ(NEST+1)= K, THE ADDRESS OF THE CALLED FUNCTION
C       IN THE DIRECTORY.
C
                      OMWQ(NESTER)  =MWQ(NESTER)
                      OMWS(NESTER)  =MWS(NESTER)
                      OMSQ(NESTER)  =MSQ(NESTER)
                      OMST(NESTER)  =MST(NESTER)
                      OMNW(NESTER,1)=MNW(NESTER,1)
                      OMNW(NESTER,2)=MNW(NESTER,2)
                      OMNW(NESTER,3)=MNW(NESTER,3)
                      OMNW(NESTER,4)=MNW(NESTER,4)
                      OMNW(NESTER,5)=MNW(NESTER,5)
                      OMDF(NESTER,1)=MDF(NESTER,1)
                      OMDF(NESTER,2)=MDF(NESTER,2)
                      OMDF(NESTER,3)=MDF(NESTER,3)
                      OMDF(NESTER,4)=MDF(NESTER,4)
                      OMDF(NESTER,5)=MDF(NESTER,5)
                      NESTI(NEST)=(I+1)
                      NEST=NEST+1
                      NESTER=NEST
C       INITIALIZE NSUB,QSUB,SSUB AND CSUB
                      NSUB=0
                      QRSUB=.FALSE.
                      CRSUB=.FALSE.
                      QSUB=0
                      SSUB=0
                      CSUB=0
                      CT=0
                      NESTIJ(NEST)=K
                      NESFUN(NEST)=.TRUE.
C
C       NOW WE MUST LOAD IN THE CALLED FUNCTION INTO THE MACRO ARRAYS
C       AND THEN BEGIN EXECUTING IT FROM LINE 1. WHEN A RETURN IS
C       FOUND AT ANY POINT, THE RULE WILL BE TO STOP EXECUTION OF
C       THE CURRENT FUNCTION,DECREMENT NEST BY 1 AND THEN RELOAD
C       THE MACRO OR FUNCTION DESIGNATED BY THE ADDRESS NESTIJ. AFTER RELOAD
C       WE BEGIN EXECUTION OF THAT MACRO OR FUNCTION AT LINE NESTI(NEST).
C       IF WE ENCOUNTER A RETURN WHILE AT LEVEL NEST=0, WE WILL
C       STOP EXECUTION OF THE BASE IN THE NEST AND RETURN
C       TO THE CURRENT LEVEL OF THE PROGRAM.
C
                      IF(NESFUN(NEST).AND.NESFUN(NEST-1).AND.
     1                NESTIJ(NEST).EQ.NF) THEN
                          OUTLYNE='A FUNCTION IS NOT ALLOWED TO CALL ITSELF'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          CALL MACFAL
                          RETURN
                      END IF
C       NOW SET THE SPECIAL QUALIFIER AND NUMERIC WORDS WHICH
C       ARE USED WITH THE CALL TO THE MACRO. NAMES ARE
C       MWQ,MWS,MNW1,MNW2,MNW3,MNW4,MNW5
C
                      MWQ(NESTER)=WQ
                      MWS(NESTER)=WS
                      MSQ(NESTER)=SQ
                      MST(NESTER)=SQ
                      MNW(NESTER,1)=W1
                      MNW(NESTER,2)=W2
                      MNW(NESTER,3)=W3
                      MNW(NESTER,4)=W4
                      MNW(NESTER,5)=W5
                      MDF(NESTER,1)=DF1
                      MDF(NESTER,2)=DF2
                      MDF(NESTER,3)=DF3
                      MDF(NESTER,4)=DF4
                      MDF(NESTER,5)=DF5
                      NF=NESTIJ(NEST)
                      CALL MACLOD
                      SPOINT=1
                      GO TO 85
                  END IF
              END DO
              DO 40 K=1,MAXMAC
                  IF(MACCW(I).EQ.MCDIR1(K)) THEN
C
C       THE MACRO CALLED A MACRO
C       THERE IS NESTING. NESTIJ(0) IS THE FIRST MACRO OR BASE OF
C       THE NEST. HERE WE DO SEVERAL THINGS. WE SET NESTI(NEST)
C       EQUAL TO I+1, THE LINE NUMBER IN THE CALLING MACRO TO
C       WHICH THE EVENTUAL RETURN WILL BE MADE. INCREMENT
C       NEST BY ONE AND THEN SET
C       NESTIJ(NEST+1)= K, THE ADDRESS OF THE CALLED MACRO
C       IN THE DIRECTORY.
C
                      OMWQ(NESTER)  =MWQ(NESTER)
                      OMWS(NESTER)  =MWS(NESTER)
                      OMSQ(NESTER)  =MSQ(NESTER)
                      OMST(NESTER)  =MST(NESTER)
                      OMNW(NESTER,1)=MNW(NESTER,1)
                      OMNW(NESTER,2)=MNW(NESTER,2)
                      OMNW(NESTER,3)=MNW(NESTER,3)
                      OMNW(NESTER,4)=MNW(NESTER,4)
                      OMNW(NESTER,5)=MNW(NESTER,5)
                      OMDF(NESTER,1)=MDF(NESTER,1)
                      OMDF(NESTER,2)=MDF(NESTER,2)
                      OMDF(NESTER,3)=MDF(NESTER,3)
                      OMDF(NESTER,4)=MDF(NESTER,4)
                      OMDF(NESTER,5)=MDF(NESTER,5)
                      NESTI(NEST)=(I+1)
                      NEST=NEST+1
                      NESTER=NEST
C       INITIALIZE NSUB,QSUB,SSUB AND CSUB
                      NSUB=0
                      QRSUB=.FALSE.
                      CRSUB=.FALSE.
                      QSUB=0
                      SSUB=0
                      CSUB=0
                      CT=0
                      NESTIJ(NEST)=K
                      NESFUN(NEST)=.FALSE.
C
C       NOW WE MUST LOAD IN THE CALLED MACRO INTO THE MACRO ARRAYS
C       AND THEN BEGIN EXECUTING IT FROM LINE 1. WHEN A RETURN IS
C       FOUND AT ANY POINT, THE RULE WILL BE TO STOP EXECUTION OF
C       THE CURRENT MACRO,DECREMENT NEST BY 1 AND THEN RELOAD
C       THE MACRO DESIGNATED BY THE ADDRESS NESTIJ. AFTER RELOAD
C       WE BEGIN EXECUTION OF THAT MACRO AT LINE NESTI(NEST).
C       IF WE ENCOUNTER A RETURN WHILE AT LEVEL NEST=0, WE WILL
C       STOP EXECUTION OF THE BASE MACRO IN THE NEST AND RETURN
C       TO THE CURRENT LEVEL OF THE PROGRAM.
                      IF(.NOT.NESFUN(NEST)) LLK=1
                      IF(.NOT.NESFUN(NEST-1)) LLL=1
                      IF(LLK.EQ.1.AND.LLL.EQ.1.AND.
     1                NESTIJ(NEST).EQ.NF) THEN
                          OUTLYNE='A MACRO IS NOT ALLOWED TO CALL ITSELF'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C       NOW SET THE SPECIAL QUALIFIER AND NUMERIC WORDS WHICH
C       ARE USED WITH THE CALL TO THE MACRO. NAMES ARE
C       MWQ,MWS,MNW1,MNW2,MNW3,MNW4,MNW5
C
                      MWQ(NESTER)=WQ
                      MWS(NESTER)=WS
                      MSQ(NESTER)=SQ
                      MST(NESTER)=SQ
                      MNW(NESTER,1)=W1
                      MNW(NESTER,2)=W2
                      MNW(NESTER,3)=W3
                      MNW(NESTER,4)=W4
                      MNW(NESTER,5)=W5
                      MDF(NESTER,1)=DF1
                      MDF(NESTER,2)=DF2
                      MDF(NESTER,3)=DF3
                      MDF(NESTER,4)=DF4
                      MDF(NESTER,5)=DF5
                      NF=NESTIJ(NEST)
                      CALL MACLOD
                      SPOINT=1
                      GO TO 85
                  END IF
 40           CONTINUE
C     F27 WAS 1 OR 2, PROCEED
          END IF
          CALL CONTRO
          IF(F4.EQ.0) THEN
              OUTLYNE='MACRO EXECUTION ABORTED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       RESET ALL SUB FLAGS AND COUNTERS
          CT=0
          CSUB=0
          QSUB=0
          SSUB=0
          QRSUB=.FALSE.
          CRSUB=.FALSE.
          NSUB=0
 50       I=I+1
          IF(I.LE.(MCDIR2(2,NESTIJ(NEST))-2)) GO TO 54
C       REACHED END OF MACRO
          NEST=NEST-1
          IF(NEST.EQ.-1) THEN
              NESTER=0
          ELSE
              NESTER=NEST
          END IF
C     RESTORE THE REAL ORIGINAL INPUT VALUES FROM THE CALL FROM THE
C     CMD LEVEL
          MWQ(NESTER)  =OMWQ(NESTER)
          MWS(NESTER)  =OMWS(NESTER)
          MSQ(NESTER)  =OMSQ(NESTER)
          MST(NESTER)  =OMST(NESTER)
          MNW(NESTER,1)=OMNW(NESTER,1)
          MNW(NESTER,2)=OMNW(NESTER,2)
          MNW(NESTER,3)=OMNW(NESTER,3)
          MNW(NESTER,4)=OMNW(NESTER,4)
          MNW(NESTER,5)=OMNW(NESTER,5)
          MDF(NESTER,1)=OMDF(NESTER,1)
          MDF(NESTER,2)=OMDF(NESTER,2)
          MDF(NESTER,3)=OMDF(NESTER,3)
          MDF(NESTER,4)=OMDF(NESTER,4)
          MDF(NESTER,5)=OMDF(NESTER,5)
          IF(NEST.EQ.-1) THEN
              NEST=0
              NESTER=0
C       STOP RUNNING MACROS AND RETURN SETTING F4=0
              WC=WCOLD
              WQ=WQOLD
              WS=STROLD
              SQ=SQOLD
              SN=SNOLD
              SST=STOLD
              S1=S1OLD
              S2=S2OLD
              S3=S3OLD
              S4=S4OLD
              S5=S5OLD
              DF1=DF1OLD
              DF2=DF2OLD
              DF3=DF3OLD
              DF4=DF4OLD
              DF5=DF5OLD
              W1=W1OLD
              W2=W2OLD
              W3=W3OLD
              W4=W4OLD
              W5=W5OLD
              F4=0
              IF(INT(SYSTEM1(91)).NE.0) F57=1
              RETURN
C       NEST NOT -1, PROCEED
          ELSE
              NESTER=NEST
          END IF
          IF(.NOT.NESFUN(NEST)) THEN

C     'RETURNING EXECUTION TO ',MCDIR1(NESTIJ(NEST))
              NF=NESTIJ(NEST)
              CALL MACLOD
              SPOINT=NESTI(NEST)
              GO TO 85
          ELSE
C       RETURNING TO A FUNCTION

C        OUTLYNE='RETURNING EXECUTION TO ',FCDIR1(NESTIJ(NEST))
              CALL SHOWIT(1)
              NF=NESTIJ(NEST)
              CALL MACLOD
              SPOINT=NESTI(NEST)
              GO TO 85
          END IF
      END


C SUB MACFAL.FOR
      SUBROUTINE MACFAL
          USE GLOBALS
C
          IMPLICIT NONE

          LOGICAL ITERROR
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsub.inc'

C
C       THIS SUBROUTINE STOPS MACRO EXECUTION IF AN ERROR OCCURS
C       AND RETURNS THE PROGRAM TO THE CMD LEVEL.
          FOBRUN=.FALSE.
          ITERROR=.TRUE.
          GUIERROR=.TRUE.
          ALLSTOP=.TRUE.
C     IT ALSO STOPS OPTIMIZATION CALCULATIONS IF THEY ARE IN PROGRESS

          IF(.NOT.MACFAILURE) RETURN
C
          IF(F26.EQ.1.AND.F28.EQ.0) THEN
C       MACRO FUNCTION RUNNING, STOP, RESET FLAGS AND ISSUE A MESSAGE
C
              OUTLYNE='MACRO FUNCTION EXECUTION ABNORMALLY TERMINATED'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
C                       RESET FLAG STATUS
              BADOPS=.TRUE.
              F1=1
              F2=0
              F3=0
              F4=0
              F5=0
              F6=0
              F7=0
              F8=0
              F9=0
              F10=0
              F11=0
              F12=1
              F13=0
              F14=0
              F15=0
              F16=0
              F17=0
              F18=0
              F26=0
              F27=0
              F29=0
              F30=0
              F31=0
              F32=0
              F35=0
              F36=0
              F37=0
              F38=0
              F39=0
              F40=0
              F41=0
              F42=0
              F43=0
              F44=0
              F45=0
              F46=0
              F47=0
              F48=0
              F49=0
              F50=0
              F51=0
              F52=0
              F53=0
              F54=0
              F55=0
              IF(OUT.NE.98) THEN
                  OFILN='            '
                  LASTFIL=OFILN
                  SAVE_KDP(1)=SAVEINPT(1)
C     RESET I/O TO SAFE VALUES
                  INPUT='OUT TP'
                  CALL PROCES
                  INPUT='IN TP'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  IN=5
                  OUT=6
              END IF
C                        NEST=0
C                        NESTER=0
              RETURN
          ELSE
          END IF
          IF(F26.EQ.1.AND.F28.EQ.1) THEN
C       MACRO FUNCTION RUNNING IN OPTIMIZATION, STOP,
C       RESET FLAGS AND ISSUE A MESSAGE
C
              OUTLYNE='OPERAND(S) NOT CALCULABLE (MACRO FUNCTION FAILURE)'
              CALL SHOWIT(1)
              OUTLYNE='CURRENT OPTIMIZATION OPERATION TERMINATED'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
C                       RESET FLAG STATUS
              KILOPT=.TRUE.
              BADOPS=.TRUE.
              F1=1
              F2=0
              F3=0
              F4=0
              F5=0
              F6=0
              F7=0
              F8=0
              F9=0
              F10=0
              F11=0
              F12=1
              F13=0
              F14=0
              F15=0
              F16=0
              F17=0
              F18=0
              F26=0
              F27=0
              F29=0
              F30=0
              F31=0
              F32=0
              F35=0
              F36=0
              F37=0
              F38=0
              F39=0
              F40=0
              F41=0
              F42=0
              F43=0
              F44=0
              F45=0
              F46=0
              F47=0
              F48=0
              F49=0
              F50=0
              F51=0
              F52=0
              F53=0
              F54=0
              F55=0
              IF(OUT.NE.98) THEN
                  OFILN='            '
                  LASTFIL=OFILN
                  SAVE_KDP(1)=SAVEINPT(1)
C     RESET I/O TO SAFE VALUES
                  INPUT='OUT TP'
                  CALL PROCES
                  INPUT='IN TP'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  IN=5
                  OUT=6
              END IF
C                        NEST=0
C                        NESTER=0
              RETURN
          ELSE
          END IF
          IF(F4.EQ.1) THEN
C       MACRO RUNNING, STOP, RESET FLAGS AND ISSUE A MESSAGE
C
              OUTLYNE='MACRO EXECUTION ABNORMALLY TERMINATED'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO THE CMD LEVEL'
              CALL SHOWIT(1)
C                       RESET FLAG STATUS
              BADOPS=.TRUE.
              F1=1
              F2=0
              F3=0
              F4=0
              F5=0
              F6=0
              F7=0
              F8=0
              F9=0
              F10=0
              F11=0
              F12=1
              F13=0
              F14=0
              F15=0
              F16=0
              F17=0
              F18=0
              F26=0
              F27=0
              F29=0
              F30=0
              F31=0
              F32=0
              F35=0
              F36=0
              F37=0
              F38=0
              F39=0
              F40=0
              F41=0
              F42=0
              F43=0
              F44=0
              F45=0
              F46=0
              F47=0
              F48=0
              F49=0
              F50=0
              F51=0
              F52=0
              F53=0
              F54=0
              F55=0
              IF(OUT.NE.98) THEN
                  OFILN='            '
                  LASTFIL=OFILN
                  SAVE_KDP(1)=SAVEINPT(1)
C     RESET I/O TO SAFE VALUES
                  INPUT='OUT TP'
                  CALL PROCES
                  INPUT='IN TP'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  IN=5
                  OUT=6
              END IF
          END IF
C                        NEST=0
C                        NESTER=0
          RETURN
      END


C SUB MACFIL.FOR
      SUBROUTINE MACFIL
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS USED TO DETERMIN THE CORRECT FILE
C       NAME GIVEN A FILE NUMBER
C
          INTEGER N
C
          CHARACTER FILNAM*10,AN*3
C
          COMMON/NEFER/N
C
          COMMON/COMNAM/FILNAM
C
          CALL CCOONN(N,AN)
C
          IF(N.GT.0.AND.N.LE.9) FILNAM='MAC00'//AN(3:3)//'.DAT'
          IF(N.GT.9.AND.N.LE.99) FILNAM='MAC0'//AN(2:3)//'.DAT'
          IF(N.GT.99.AND.N.LE.999) FILNAM='MAC'//AN(1:3)//'.DAT'
          RETURN
      END
