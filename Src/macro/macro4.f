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

C       FOURTH FILE OF MACRO FILES

C SUB MACOUT.FOR
      SUBROUTINE MACOUT
C
          IMPLICIT NONE
C
          LOGICAL EXST,EXISJK
C
C       THIS SUBROUTINE IS CALLED BY MMFL AND
C       CAUSES THE ITH MACRO TO BE OUTPUT
C       TO THE DEFAULT OUTPUT DEVICE IN THE SAME
C       FORMAT AS IF IT WERE BEING INPUT FROM THE KEYBOARD
C
          INTEGER OCC,MCLOC1,IMCEN1,LENG,NF,
     1    K,L,I
C
          CHARACTER MCNAM1*8,AN1*23,AN2*23,AN3*23,AN4*23,
     2    AN5*23,BN1*11,BN2*11,BN3*11,BN4*11,BN5*11,AAN1*1,AAN2*1,
     3    AAN3*1,AAN4*1,AAN5*1,BBBN1*1,BBBN2*1,BBBN3*1,BBBN4*1,BBBN5*1,
     4    LINE1*80,LINE2*140,FILNAM*10,CM*1,STAMP*20,BLIN80*80,
     5    BLIN13*140
C
          COMMON/NEFER/NF
C
          COMMON/COMNAM/FILNAM
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
          BLIN80=AA//AA//AA//AA
          BLIN13=AA//AA//AA//AA//AA//AA//AA
C
C       OPEN UNIT 20 FOR I/O
          CM=','
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
C       MCNAN1 IS THE MACRO NAME, MCLOC1 IS THE MACRO LOCK,
C       IMCEN1 IS THE MACRO LENGTH IN LINES, AND OCC IS A FLAG
C       =0 IF THE RECORD REPRESENTS A BLANK MACRO AND =1
C       FOR A NON-BLANK MACRO.
C
C       MACRO RECORDS (MAXIMUM OF MAXLL NON-BLANK RECL=55*NRECL RECORDS)
C       ARE USED TO STORE A MACRO.
C
          CALL MACFIL
          EXST=.FALSE.
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
          OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1    'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
C
C       NOW DETERMINE THE LOCATION OF THE MACRO BODY.
C
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
C       THE OUTPUT WILL HAVE IMCEN1 LINES IN IT.
C
          DO I=0,(IMCEN1-1)
C
C       CHECK FOR PRESECE OF A QUALIFIER
              IF(MACSTA(5,I).EQ.1) THEN
C       THERE IS A QUALIFIER WORD
C       PRODUCE OUTPUT
C
C       IF OUT = 6 (SCREEN) OR OUT = 7 (PRINTER)
C       THEN WRITE COMPACT 80 COLUMN OUTPUT.
C
C       FOR ED,CR,AND PU WRITE EXTENDED COMPLETE OUTPUT
C
                  IF(OUT.EQ.8.OR.OUT.EQ.9.OR.OUT.EQ.10.OR.OUT.EQ.97) THEN
                      LINE2=MACCW(I)//CHAR(32)//MACQW(I)
                  ELSE
                      LINE1=MACCW(I)//CHAR(32)//MACQW(I)
                  END IF

C       THERE IS NO STRING,THERE ARE NUMERIC WORDS AND A QUALIFIER.
C       BEFORE WRITING THE NUMERIC DATA,IT MUST BE CONVERTED
C       INTO A STRING VARIABLE. OTHERWISE HANDELING THE DEFAULT
C       VALUES BECOMES AN S.O.B..
C       CONVERT THE NUMERIC VALES TO STRING VALUES WITH
C       SUBROUTINE NWTOAW.  THE CALL IS :
C
C               CALL NWTOAW(N1,N2,N3,N4,N5,AN1,AN2,AN3,AN4,AN5,
C     1  BN1,BN2,BN3,BN4,BN5)
C       THW RETURNED VALUES ARE AN1,AN2,AN3,AN4,AN5
C
                  CALL NWTOAW(MACNW(1,I),MACNW(2,I),MACNW(3,I),MACNW(4,I)
     1            ,MACNW(5,I),AN1,AN2,AN3,AN4,AN5,BN1,BN2,BN3,BN4,BN5)
C
C       DEAL WITH THE DEFAULT VALUES
                  AAN1=','
                  AAN2=','
                  AAN3=','
                  AAN4=','
                  AAN5=','
                  BBBN1=','
                  BBBN2=','
                  BBBN3=','
                  BBBN4=','
                  BBBN5=','
C
C       PRODUCE OUTPUT. WRITE NUMERIC WORDS EVEN IF ALL ZERO.
C
                  IF(OUT.EQ.8.OR.OUT.EQ.9.OR.OUT.EQ.10.OR.OUT.EQ.97) THEN
                      LINE2=MACCW(I)//CHAR(32)//MACQW(I)//CM
                      IF(MACSTA(12,I).EQ.1) THEN
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AAN1
                      ELSE
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AN1//CM
                      END IF
                      IF(MACSTA(13,I).EQ.1) THEN
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AAN2
                      ELSE
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AN2//CM
                      END IF
                      IF(MACSTA(14,I).EQ.1) THEN
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AAN3
                      ELSE
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AN3//CM
                      END IF
                      IF(MACSTA(15,I).EQ.1) THEN
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AAN4
                      ELSE
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AN4//CM
                      END IF
                      IF(MACSTA(16,I).EQ.1) THEN
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AAN5
                      ELSE
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AN5//CM
                      END IF
                  ELSE
                      LINE1=MACCW(I)//CHAR(32)//MACQW(I)//CM
                      IF(MACSTA(12,I).EQ.1) THEN
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BBBN1
                      ELSE
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BN1//CM
                      END IF
                      IF(MACSTA(13,I).EQ.1) THEN
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BBBN2
                      ELSE
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BN2//CM
                      END IF
                      IF(MACSTA(14,I).EQ.1) THEN
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BBBN3
                      ELSE
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BN3//CM
                      END IF
                      IF(MACSTA(15,I).EQ.1) THEN
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BBBN4
                      ELSE
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BN4//CM
                      END IF
                      IF(MACSTA(16,I).EQ.1) THEN
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BBBN5
                      ELSE
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BN5//CM
                      END IF
                  END IF
              ELSE
C       NO QUALIFIER WORD
C       THERE IS NUMERIC INPUT.
C       BEFORE WRITING THE NUMERIC DATA,IT MUST BE CONVERTED
C       INTO A STRING VARIABLE. OTHERWISE HANDELING THE DEFAULT
C       VALUES BECOMES AN S.O.B..
C       CONVERT THE NUMERIC VALES TO STRING VALUES WITH
C       SUBROUTINE NWTOAW.  THE CALL IS :
C
C               CALL NWTOAW(N1,N2,N3,N4,N5,AN1,AN2,AN3,AN4,AN5)
C       THW RETURNED VALUES ARE AN1,AN2,AN3,AN4,AN5,BN1,BN2,BN3,
C     1  BN4,BN5
C
                  CALL NWTOAW(MACNW(1,I),MACNW(2,I),MACNW(3,I),MACNW(4,I)
     1            ,MACNW(5,I),AN1,AN2,AN3,AN4,AN5,BN1,BN2,BN3,BN4,BN5)
C
C       DEAL WITH THE DEFAULT VALUES
C       AAN1=','
                  AAN2=','
                  AAN3=','
                  AAN4=','
                  AAN5=','
                  BBBN1=','
                  BBBN2=','
                  BBBN3=','
                  BBBN4=','
                  BBBN5=','
C
C       PRODUCE OUTPUT. WRITE NUMERIC WORDS EVEN IF ALL ZERO.
                  IF(OUT.EQ.8.OR.OUT.EQ.9.OR.OUT.EQ.10.OR.OUT.EQ.97) THEN
                      LINE2=MACCW(I)//CM

                      IF(MACSTA(12,I).EQ.1) THEN
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AAN1
                      ELSE
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AN1//CM
                      END IF
                      IF(MACSTA(13,I).EQ.1) THEN
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AAN2
                      ELSE
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AN2//CM
                      END IF
                      IF(MACSTA(14,I).EQ.1) THEN
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AAN3
                      ELSE
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AN3//CM
                      END IF
                      IF(MACSTA(15,I).EQ.1) THEN
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AAN4
                      ELSE
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AN4//CM
                      END IF
                      IF(MACSTA(16,I).EQ.1) THEN
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AAN5
                      ELSE
                          CALL LTH140(LINE2,LENG)
                          LINE2=LINE2(1:LENG)//AN5//CM
                      END IF
                  ELSE
                      LINE1=MACCW(I)//CM
                      IF(MACSTA(12,I).EQ.1) THEN
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BBBN1
                      ELSE
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BN1//CM
                      END IF
                      IF(MACSTA(13,I).EQ.1) THEN
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BBBN2
                      ELSE
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BN2//CM
                      END IF
                      IF(MACSTA(14,I).EQ.1) THEN
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BBBN3
                      ELSE
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BN3//CM
                      END IF
                      IF(MACSTA(15,I).EQ.1) THEN
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BBBN4
                      ELSE
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BN4//CM
                      END IF
                      IF(MACSTA(16,I).EQ.1) THEN
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BBBN5
                      ELSE
                          CALL LTH80(LINE1,LENG)
                          LINE1=LINE1(1:LENG)//BN5//CM
                      END IF
                  END IF
              END IF
C       NOW CHECK FOR PRESENCE OF A STRING
              IF(MACSTA(6,I).EQ.1) THEN
                  IF(OUT.EQ.8.OR.OUT.EQ.9.OR.OUT.EQ.10.OR.OUT.EQ.97) THEN
                      LINE2=TRIM(LINE2)//TRIM(MACSTR(I))
                  ELSE
                      LINE1=TRIM(LINE1)//TRIM(MACSTR(I))
                  END IF
              END IF
C
              IF(OUT.EQ.7.OR.OUT.EQ.6) THEN
                  IF(LINE1.NE.BLIN80) CALL NO_BLANK(LINE1,79)
                  IF(LINE1.NE.BLIN80) CALL NO_ZEROS(LINE1,79)
                  IF(LINE1.NE.BLIN80) WRITE(OUTLYNE,*) TRIM(LINE1)
                  IF(LINE1.NE.BLIN80) CALL SHOWIT(0)
              END IF
              IF(OUT.EQ.8.OR.OUT.EQ.9.OR.OUT.EQ.10.OR.OUT.EQ.97) THEN
                  IF(LINE2.NE.BLIN13) CALL NO_BLANK(LINE2,139)
                  IF(LINE2.NE.BLIN13) CALL NO_ZEROS(LINE2,139)
                  IF(LINE2.NE.BLIN13) WRITE(OUTLYNE,*) TRIM(LINE2)
                  IF(LINE2.NE.BLIN13) CALL SHOWIT(2)
              END IF
          END DO

C
          CALL CLOSE_FILE(20,1)
          RETURN
 10       RETURN
      END


      SUBROUTINE NO_BLANK(STRING,N)
          INTEGER N,I,J
          CHARACTER STRING*(*)
C     STRIP LEADING BLANKS

          DO I=1,N
              IF(STRING(1:1).EQ.' ')
     1        STRING(1:N)=STRING(2:140)//' '
          END DO
C     IS LINE BLANK? IF YES RETURN
          IF(STRING(1:1).EQ.' ') RETURN
          J=2
          DO I=1,N-1
              IF(STRING(J:J+1).EQ.'  ') THEN
                  STRING(1:N)=STRING(1:J)//STRING(J+2:N)//' '
              ELSE
                  J=J+1
              END IF
          END DO
          J=2
          DO I=1,N-1
              IF(STRING(J:J+1).EQ.' ,') THEN
                  STRING(1:N)=STRING(1:J-1)//STRING(J+1:N)//' '
              ELSE
                  J=J+1
              END IF
          END DO
          J=2
          DO I=1,N-1
              IF(STRING(J:J+1).EQ.', ') THEN
                  STRING(1:N)=STRING(1:J)//STRING(J+2:N)//' '
              ELSE
                  J=J+1
              END IF
          END DO
          RETURN
      END


      SUBROUTINE ONE_BLANK(STRING,N)
          INTEGER N,I,J
          CHARACTER STRING*(*)
C     STRIP LEADING BLANKS

          DO I=1,N
              IF(STRING(1:1).EQ.' ')
     1        STRING(1:N)=STRING(2:140)//' '
          END DO
C     IS LINE BLANK? IF YES RETURN
          IF(STRING(1:1).EQ.' ') RETURN
          J=2
          DO I=1,N
              IF(STRING(J:J+1).EQ.'  ') THEN
                  STRING(1:N)=STRING(1:J-1)//STRING(J+1:N)//' '
              ELSE
                  J=J+1
              END IF
          END DO
          RETURN
      END


      SUBROUTINE NO_ZEROS(STRING,N)
          INTEGER N,I,J
          CHARACTER STRING*(*)
          INCLUDE 'datmai.inc'
C     STRIP LEADING BLANKS

20        CONTINUE
          J=1
          DO I=1,N
              IF(N.EQ.79.AND.J.GE.77) GO TO 30
              IF(N.EQ.139.AND.J.GE.137) GO TO 30
              IF(STRING(J:J+2).EQ.'00D'.OR.STRING(J:J+2).EQ.'00E'.OR.
     1        STRING(J:J+2).EQ.'00,'.OR.STRING(J:J+2).EQ.'00 ') THEN
                  STRING(1:N)=STRING(1:J-1)//STRING(J+1:N)//' '
                  IF(STRING(J:J+1).EQ.' 0')
     1            STRING(1:N)=STRING(1:J-1)//STRING(J+1:N)//' '
                  GO TO 20
              ELSE
                  J=J+1
                  IF(N.EQ.79.AND.J.GE.77) GO TO 30
                  IF(N.EQ.139.AND.J.GE.137) GO TO 30
              END IF
          END DO
 30       CONTINUE
          RETURN
      END


C SUB NEWMAC.FOR
      SUBROUTINE NEWMAC
C
          IMPLICIT NONE
C
          CHARACTER STAMP*20
C
          LOGICAL EXISJK
C
          INTEGER
     1    I,J,MAXLL,KKKEY,CCODE
C
          COMMON/CODECC/CCODE,KKKEY
C
          REAL*8 DWW1
C
C       THIS IS THE SUBROUTINE RESPONSIBLE FOR CREATION OF A
C       NEW MACRO. SUBROUTINE EOM IS THE ONLY WAY BACK TO THE
C       MAIN PROGRAM CMD LEVEL. WQ IS THE NEW MACRO NAME.
C       MACRO NAMES MUST NOT BE THE SAME AS PROGRAM COMMAND
C       NAMES AS IF THEY ARE, THE MACRO WILL NEVER BE ABLE
C       TO BE EXECUTED OR DELETED WITHOUT EXTERNAL INTERVENTION.
C
          CHARACTER MNAME*8

          INTEGER M1,M2,M3,NEXTM3
C
C       MACCW,MACQW,AND MACSTR STORE THE COMMAND,QUALIFIER,AND
C       ALPHA-NUMERIC STRING.
C
C       MACNW STORES THE 5 NUMERIC WORDS
C
          INTEGER IMAC
C
C       MACSTA STORE UP TO 20 STATUS INDICATORS
C
C       ALL STORAGE IS FOR MACROS UP TO MAXLL LINES
C
          COMMON/IMAC/IMAC
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              OUTLYNE='"MACRO FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              OUTLYNE='TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
              CALL SHOWIT(1)
              F1=1
              F2=0
              CALL MACFAL
              RETURN
          ELSE
C       PROCEED
          END IF
C
          CALL MAXLNN(MAXLL,MAXLIN)
          I=(MAXLL-1)
          MACCW(0:I)=BB
          MACQW(0:I)=BB
          MACSTR(0:I)=AA//AA//AA//AA
          MACSTA(1:20,0:I)=0
          MACNW(1:5,0:I)=0.0D0

C       OPEN UNIT 20 FOR I/O
C
C       CHECK FOR A BLANK MACRO NAME, IF FOUND SEND MESSAGE
C       AND RETURN
          IF(WQ.EQ.'        ') THEN
              OUTLYNE=
     1        '"MACRO" MUST BE FOLLOWED BY A NON-BLANK MACRO NAME'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              F1=1
              F2=0
              CALL MACFAL
              RETURN
          ELSE
C       NOT A BLANK NAME
          END IF
C
C       ***************************************************************
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
          J=0
          M3=0
          DO 10 I=1,MAXMAC
              READ(UNIT=20,REC=I,ERR=10) MNAME,M1,M2,NEXTM3,STAMP
              M3=M3+NEXTM3
              IF(WQ.EQ.MNAME) J=J+1
 10       CONTINUE
C               CLOSE UNIT 20
          CALL CLOSE_FILE(20,1)
          IF(J.GT.0) THEN
C     OVERWRITE
              F16=1
              F1=1
              F2=0
              CCODE=1
              KKKEY=M1
              CALL MACFAL
              RETURN
          ELSE
          END IF
C       CHECK FOR NO ROOM IN THE DIRECTORY
          IF(M3.GE.MAXMAC) THEN
C       NO MORE ROOM
              OUTLYNE='MACRO DIRECTORY FULL. RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
          END IF
C
          IMAC=0
C
C       IMAC IS THE COUNTER FOR MACRO LINES. IT IS INITIALIZED
C       HERE TO 0 AND IS PASSED IN MACRO COMMON
C
C       DURING MACRO CREATION, THE CMD LEVEL IS DISABLED
C       AND THE MACRO INPUT LEVEL IN ENABLED.
C
C       DURING MACRO INPUT, EACH PROGRAM INSTRUCTION
C       ENTERED IS PROCESSED BY SUBROUTINE PROCESS
C       THEN PASSED BY SUBROUTINE CONTROL TO SUBROUTINE
C       MACIN WHERE EACH COMMAND IS
C       STORED IN THE MACRO CREATION ARRAYS.
C       WHEN EOM IS ENTERED, THE MACRO CREATION ARRAYS
C       ARE WRITTEN TO THE MACRO FILE MAC.DAT.
C       THE RECORDS ARE WRITTEN DIRECT AND UNFORMATTED.
C       A DIRECTORY OF MACRO NAMES IS WRITTEN IN THE FIRST
C       MAXMAC ENRTIES OF THIS MACRO FILE.
C
          DWW1=W1
          CALL MACIN(DWW1)
          RETURN
      END
      SUBROUTINE MACARRAY_LOAD(NUMINLIST)
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INTEGER OLDOUT,OLDIN,I,NUMINLIST
          CHARACTER*132 LINE
          MACARRAY(1:1024)='        '
          CALL CLOSE_FILE(9,0)
          OLDOUT=OUT
          OLDIN=IN
          SAVE_KDP(24)=SAVEINPT(24)
          INPUT='OUT ED'
          CALL PROCES
          INPUT='MFLN'
          CALL PROCES
          REST_KDP(24)=RESTINPT(24)
          OUT=OLDOUT
          IN=OLDIN
          CALL CLOSE_FILE(9,1)
          OPEN(UNIT=9,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'EDITTEXT.DAT'
     2    ,STATUS='UNKNOWN')
          REWIND(9)
          I=1
 1        READ(9,25,ERR=90,END=90) LINE
          IF(I.GE.3) MACARRAY(I-2)=LINE(1:8)
          I=I+1
          GO TO 1
 90       CONTINUE
          CALL CLOSE_FILE(9,0)
 25       FORMAT(A132)
          NUMINLIST=I-2
          RETURN
      END
