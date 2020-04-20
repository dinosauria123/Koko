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

C       FIRST FILE OF MACRO FILES

C SUB SORTA.FOR
      SUBROUTINE SORTA(AKK)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE SORTS AKK EIGHT CHARACTER STRING VARIABLES
C       ALPHA-NUMERICALLY. THESE AKK CHARACTERS ARE PASSED IN ARRAY
C       ASORT(I). AKK CAN RANGE FROM 0 TO MAXMAC. THE SORTED ARRAY
C       IS LEFT IN ASORT AND IS PASSED BACK TO THE CALLING PROGRAM
C       VIA COMMON ASRT1 AND ASRT2
C
C       IMPLICIT NONE
C
          CHARACTER NEXT*8,NEXT1*80
C
          INTEGER II,JK,AKK
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'dathgr.inc'
C
C       THE ARRAY BSORT IS A WORKING BUFFER FOR INTERMEDIATE
C       RESULTS.
C
C       INITIALIZE BSORT ARRAY TO ALL BLANKS
          BSORT(1:MAXMAC)='        '
          BISORT(1:MAXMAC)='                    '
C
C       IF AKK=0 NO SORT,RETURN
          IF(AKK.EQ.0) THEN
              RETURN
          ELSE
C       IF AKK=1 SORT IS TRIVIAL
              IF(AKK.EQ.1) THEN
C       NO SORT NEEDS BE PERFORMED
                  RETURN
              ELSE
C       IF AKK LT 0 ERROR
                  IF(AKK.LT.0) THEN
                      OUTLYNE='ERROR- NEGATIVE ITEMS TO SORT'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
C       IF AKK GT MAXMAC ERROR
                      IF(AKK.GT.MAXMAC) THEN
                          WRITE(OUTLYNE,*)
     1                      'ERROR- CAN NOT SORT MORE THAN ',MAXMAC,' ITEMS'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      ELSE
C       AKK IS A VALUE FROM 2 TO MAXMAC
C       PERFORM TO SORT.
C********************************************************************
C
C                       ALPHANUMERIC SORT
C                       BEGINS HERE
C       START UP
C
                          BSORT(1)=ASORT(1)
                          BISORT(1)=AISORT(1)
                          NEXT=ASORT(2)
                          NEXT1=AISORT(2)
                          IF(NEXT.GE.BSORT(1)) THEN
                              BSORT(2)=NEXT
                              BISORT(2)=NEXT1
                          ELSE
                              BSORT(2)=BSORT(1)
                              BISORT(2)=BISORT(1)
                              BSORT(1)=NEXT
                              BISORT(1)=NEXT1
                          END IF
C       WE NOW HAVE AKK-2 ITEMS TO RANK AGAINST THE FIRST AND SECOND
C       ITEM.
C
C       FOR ITEM 3 THERE ARE TWO COMPARISON PASSES
C       FOR ITEM 4 THERE ARE THREE COMPARISON PASSES
C       AND FOR ITEM AKK THERE WILL BE AK-1 COMPARISON PASSES.
C
C
                          DO 10 II=3,AKK
C       THIS IS THE MAIN COUNTING LOOP TO SEE WHEN TO STOP
C       SEARCHING FOR NEW DATA.
C
                              NEXT=ASORT(II)
                              NEXT1=AISORT(II)
C
                              DO 20 JK=II-1,1,-1
C       THIS IS THE COUNTING LOOP TO COUNT OFF EACH COMPARISON
C       PASS.
C
                                  IF(NEXT.LT.BSORT(JK)) THEN
                                      BSORT(JK+1)=BSORT(JK)
                                      BISORT(JK+1)=BISORT(JK)
                                      BSORT(JK)=NEXT
                                      BISORT(JK)=NEXT1
                                  ELSE
                                      BSORT(JK+1)=NEXT
                                      BISORT(JK+1)=NEXT1
                                      GO TO 10
                                  END IF
 20                           CONTINUE
 10                       CONTINUE
C       LOAD BSORT INTO ASORT
C
                          DO 30 II=1,AKK
                              ASORT(II)=BSORT(II)
                              AISORT(II)=BISORT(II)
 30                       CONTINUE
                      END IF
                  END IF
              END IF
          END IF
          RETURN
      END
      SUBROUTINE SORT_JK1(N,RA,RA1,RA2)
C     USED IN FIELD SORTING OR MULTIPLE FIELDS OF VIEW
          IMPLICIT NONE
          REAL*8 RA,RRA,RRA1,RRA2,RA1,RA2
          INTEGER N,L,IR,J,I
          DIMENSION RA(N),RA1(N),RA2(N)
          INCLUDE 'datmai.inc'
          L=N/2+1
          IR=N
 10       CONTINUE
          IF(L.GT.1) THEN
              L=L-1
              RRA=RA(L)
              RRA1=RA1(L)
              RRA2=RA2(L)
          ELSE
              RRA=RA(IR)
              RRA1=RA1(IR)
              RRA2=RA2(IR)
              RA(IR)=RA(1)
              RA1(IR)=RA1(1)
              RA2(IR)=RA2(1)
              IR=IR-1
              IF(IR.EQ.1) THEN
                  RA(1)=RRA
                  RA1(1)=RRA1
                  RA2(1)=RRA2
                  RETURN
              END IF
          END IF
          I=L
          J=L+L
 20       IF(J.LE.IR) THEN
              IF(J.LT.IR) THEN
                  IF(RA(J).LT.RA(J+1)) J=J+1
              END IF
              IF(RRA.LT.RA(J)) THEN
                  RA(I)=RA(J)
                  RA1(I)=RA1(J)
                  RA2(I)=RA2(J)
                  I=J
                  J=J+1
              ELSE
                  J=IR+1
              END IF
              GO TO 20
          END IF
          RA(I)=RRA
          RA1(I)=RRA1
          RA2(I)=RRA2
          GO TO 10
      END
C SUB RELOAD.FOR
      SUBROUTINE RELOAD
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO RELOAD THE CONTENTS OF THE NAMED
C       REGISTERS REG(1:50) FROM THE ARRAY SREG(1:20,1:50).
C       SREG CAN BE PASSED VIA COMMON SMEMRY IF NEEDED.
C
          REAL*8 SREG(0:20,1:50)
C
!        INTEGER I
C
          COMMON/SMEMRY/SREG
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
          IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE='"RELOAD" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          REG(1:25)=SREG(NEST,1:25)
          WRITE(OUTLYNE,*)
     1    'NAMED REGISTERS RELOADED AT NEST LEVEL=',NEST
          CALL SHOWIT(1)
          RETURN
      END
C SUB ACC1.FOR
      SUBROUTINE ACC1(CT)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE MAKES ACCSUB AVALIABLE TO NSUB.
C       IT IS THE LOADER.
C       THE SAVER IS SUBROUTINE ACC2
C
          CHARACTER ACCWRD*8
C
          INTEGER ACCSUB,ACCCNT,CT
C
          REAL*8 PASS1,REG9(1:10)
C
          COMMON/REGINALD/REG9
C
          COMMON/COMACC1/PASS1
C
          COMMON/ACCSB/ACCWRD
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          INCLUDE 'datmai.inc'
C
          IF(ACCSUB.EQ.1) THEN
              IF(ACCWRD.EQ.'A') PASS1=REG(1)
              IF(ACCWRD.EQ.'B') PASS1=REG(2)
              IF(ACCWRD.EQ.'C') PASS1=REG(3)
              IF(ACCWRD.EQ.'D') PASS1=REG(4)
              IF(ACCWRD.EQ.'E') PASS1=REG(5)
              IF(ACCWRD.EQ.'F') PASS1=REG(6)
              IF(ACCWRD.EQ.'G') PASS1=REG(7)
              IF(ACCWRD.EQ.'H') PASS1=REG(8)
              IF(ACCWRD.EQ.'X') PASS1=REG(9)
              IF(ACCWRD.EQ.'Y') PASS1=REG(10)
              IF(ACCWRD.EQ.'Z') PASS1=REG(11)
              IF(ACCWRD.EQ.'T') PASS1=REG(12)
              IF(ACCWRD.EQ.'IX') PASS1=REG(13)
              IF(ACCWRD.EQ.'IY') PASS1=REG(14)
              IF(ACCWRD.EQ.'IZ') PASS1=REG(15)
              IF(ACCWRD.EQ.'IT') PASS1=REG(16)
          ELSE
              PASS1=REG9(CT)
          END IF
          RETURN
      END
C SUB ACC2.FOR
      SUBROUTINE ACC2
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE MAKES ACCSUB AVALIABLE TO NSUB.
C       IT IS THE SAVER
C       THE LOADER IS SUBROUTINE ACC1
C
          CHARACTER ACCWRD*8
C
          INTEGER ACCSUB,ACCCNT
C
          REAL*8 PASS2
C
          COMMON/COMACC2/PASS2
C
          COMMON/ACCSB/ACCWRD
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          INCLUDE 'datmai.inc'
C
          IF(ACCSUB.EQ.1) THEN
              IF(ACCWRD.EQ.'A')  REG(1)=PASS2
              IF(ACCWRD.EQ.'B')  REG(2)=PASS2
              IF(ACCWRD.EQ.'C')  REG(3)=PASS2
              IF(ACCWRD.EQ.'D')  REG(4)=PASS2
              IF(ACCWRD.EQ.'E')  REG(5)=PASS2
              IF(ACCWRD.EQ.'F')  REG(6)=PASS2
              IF(ACCWRD.EQ.'G')  REG(7)=PASS2
              IF(ACCWRD.EQ.'H')  REG(8)=PASS2
              IF(ACCWRD.EQ.'X') THEN
                  REG(40)=REG(9)
                  REG(9)=PASS2
              END IF
              IF(ACCWRD.EQ.'Y')  REG(10)=PASS2
              IF(ACCWRD.EQ.'Z')  REG(11)=PASS2
              IF(ACCWRD.EQ.'T')  REG(12)=PASS2
              IF(ACCWRD.EQ.'IX') THEN
                  REG(30)=REG(13)
                  REG(13)=PASS2
              END IF
              IF(ACCWRD.EQ.'IY') REG(14)=PASS2
              IF(ACCWRD.EQ.'IZ') REG(15)=PASS2
              IF(ACCWRD.EQ.'IT') REG(16)=PASS2
              ACCCNT=ACCCNT-1
              IF(ACCCNT.EQ.0) ACCSUB=0
              RETURN
          ELSE
              REG(40)=REG(9)
              REG(9)=PASS2
          END IF
          RETURN
      END
C SUB ACC3.FOR
      SUBROUTINE ACC3
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE MAKES ACCSUB AVALIABLE TO NSUB.
C       THIS IS A CLOSER
C       THE LOADER IS SUBROUTINE ACC1
C       THE SAVER IS SUBROUTINE ACC2
C
          CHARACTER ACCWRD*8
C
          INTEGER ACCSUB,ACCCNT
C
          COMMON/ACCSB/ACCWRD
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          INCLUDE 'datmai.inc'
C
          IF(ACCSUB.EQ.1) THEN
              ACCCNT=ACCCNT-1
              IF(ACCCNT.EQ.0) ACCSUB=0
          ELSE
          END IF
          RETURN
      END
C SUB EEOM.FOR
      SUBROUTINE EEOM
C
          IMPLICIT NONE
C
          INTEGER OCC,MACLOC
     5    ,IMACEN,MCLOC1,IMCEN1,
     6    NF,I,K,L,KLI
C
          LOGICAL EXISJK
C
          CHARACTER MACNAM*8,MCNAM1*8,FILNAM*10,STAMP*20,
     1    DMNAM*10,DDATE*10,TTIME*8,TTTIM*8,DDDAT*10
C
          COMMON/STRNGR/DDDAT,TTTIM,MCNAM1,DMNAM
C
          COMMON/MACID/MACNAM
C
          COMMON/MACID2/MACLOC,IMACEN
C
          COMMON/NEFER/NF
C
          COMMON/COMNAM/FILNAM
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
C       THIS SUBROUTINE IS USED TO END CREATION OF A NEW
C       MACRO. IT ALSO FILES THE MACRO DIRECTORY
C       INFORMATION IN THE FILE
C       MAC.DATA AND THE MACRO BODY IN ONE OF THE
C       MAXMAC FILES MACXXX.DAT. MACRO OUTPUT WILL ALWAYS
C       VIA BE UNIT=20.
C       THE FILES WILL BE UNFORMATED AND DIRECT FOR QUICK
C       ACCESS. A DIRECTORY WILL BE WRITTEN INTO THE
C       FILE MAC.DAT WHICH WILL BE UPDATED WHENEVER
C       A MACRO IS ADDED,SUBTRACTED OR MODIFIED.
C
C       IF THERE ARE NO LINES IN THE MACRO, PRINT A
C       MEESAGE TO THAT EFFECT AND RETURN TO CMD LEVEL
C
          IF(SST.EQ.1.OR.SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"EOM" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C
          IF(IMACEN.GT.2) THEN
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
              DO 10 I=1,MAXMAC
                  READ(UNIT=20,REC=I,ERR=10) MCNAM1,MCLOC1,IMCEN1,OCC,STAMP
C
C       MCNAN1 IS THE MACRO NAME, MCLOC1 IS THE MACRO LOCK,
C       IMCEN1 IS THE MACRO LENGTH IN LINES, AND OCC IS A FLAG
C       =0 IF THE RECORD REPRESENTS A BLANK MACRO AND =1
C       FOR A NON-BLANK MACRO.
C
C
C       THE INITIAL VALUES STORED IN MAC.DAT ARE:
C
C       BLANK,0,0,0,STAMP BEFORE THE FIRST MACRO STORAGE.
C
C       VALUES FOR THE NEW MACRO ARE READ INTO THE ARRAY MCDIR1 (TRACKING
C       THE MACRO NAME) AND MCDIR2 (TRACKING MACLOC,IMACEN, AND
C       OCC. THE INFORMATION IN THESE MACRO DIRECTORY ARRAYS
C       IS ALWAYS PASSED TO OTHER SUBROUTINES VIA DIRMAC COMMON.
C
                  MCDIR1(I)=MCNAM1
                  MCDIR2(1,I)=MCLOC1
                  MCDIR2(2,I)=IMCEN1
                  MCDIR2(3,I)=OCC
                  MCDIR3(I)=STAMP
 10           CONTINUE
              CALL CLOSE_FILE(20,1)
C
C       SEARCHES OF THIS MACRO DIRECTORY ARE NOW MADE TO DETERMINE
C       HOW AND WHERE TO STORE EACH NEW OR UPDATED MACRO
C
C       MACRO RECORDS (MAXIMUM OF MAXLL NON-BLANK RECL=55*NRECL RECORDS)
C       ARE USED TO STORE A MACRO. THE FIRST MACRO IS STORED IN
C       FILE MAC001.DAT. THE MAXMACTH MACRO IN MAC(MAXMAC).DAT.
C       THIS ALLOWS STORAGE
C       OF MAXMAC MAXLL LINE MACROS AND THE ASSOCIATED DIRECTORY.
C
C       SEARCH THE DIRECTORY FOR A MACRO OF THE SAME NAME AS THE MACRO
C       TO BE FILED.
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
              DO 20 I=1,MAXMAC
                  IF(MCDIR1(I).EQ.MACNAM) THEN
C
C       FILE THE NEW MACRO OVER WHERE THE OLD MACRO WAS LOCATED
C       FIRST UPDATE THE DIRECTORY INFORMATION
C
                      MCDIR1(I)=MACNAM
                      MCDIR2(1,I)=MACLOC
                      MCDIR2(2,I)=IMACEN
                      MCDIR2(3,I)=1
                      CALL MYTIME(TTIME)
                      CALL MYDATE(DDATE)
                      DDDAT=DDATE
                      TTTIM=TTIME
                      MCDIR3(I)=TTIME//'  '//DDATE
C
                      WRITE(UNIT=20,REC=I) MCDIR1(I),MCDIR2(1,I),
     1                MCDIR2(2,I),MCDIR2(3,I),MCDIR3(I)
                      CALL CLOSE_FILE(20,1)
C       THE MACRO FILE TO STORE IN IS DETERMINED BY THE VALUE OF I
C
C       SUBROUTINE MACFIL IS CALLED WHICH RETURNS THE STRING
C       NAME OF THE CORRECT FILE.
C
                      NF=I
                      CALL MACFIL
C
C       NOW OPEN THE CORRECT FILE
C
                      OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1                'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
C
C       NOW FILE THE BODY OF THE MACRO STARTING AT RECORD 1
C
                      DO 30 K=1,(IMACEN)
                          L=K-1
                          WRITE(UNIT=30,REC=K) MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),
     1                    MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),
     2                    MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),
     3                    MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),
     4                    MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L)
     5                    ,MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)
C
 30                   CONTINUE
                      CALL CLOSE_FILE(30,1)
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
                          FUNEXT(KLI)=.TRUE.
                          FCDIR1(KLI)=MACNAM
                          FCDIR2(1,KLI)=MACLOC
                          FCDIR2(2,KLI)=IMACEN
                          FCDIR2(3,KLI)=1
                          DO 301 K=1,(IMACEN)
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
 301                      CONTINUE
                      ELSE
C       NOT A FUNCTION
                      END IF
C
C       AFTER THE RETURN AND THE MACRO IS FILED AND
C       THE DIRECTORY IS UPDATED THEN
C       RETURN TO SUBROUTINE CONTROL AND THE
C       CMD LEVEL.
C       AND MACRO INPUT LEVEL IS DISABLED.
                      OUTLYNE=
     1                ' AMACRO '//MACNAM//' FILED - RETURNED TO CMD LEVEL'
                      CALL SHOWIT(1)
C                       RETURN TO CMD LEVEL
                      F1=1
                      F2=0
                      RETURN
                  ELSE
C       NAME WAS NOT FOUND AS AN OLD NAME.
                      IF(MCDIR2(3,I).EQ.0) THEN
C
C       FIRST UPDATE THE DIRECTORY INFORMATION
C
                          MCDIR1(I)=MACNAM
                          MCDIR2(1,I)=MACLOC
                          MCDIR2(2,I)=IMACEN
                          MCDIR2(3,I)=1
                          CALL MYTIME(TTIME)
                          CALL MYDATE(DDATE)
                          DDDAT=DDATE
                          TTTIM=TTIME
                          MCDIR3(I)=TTIME//'  '//DDATE
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
     1                    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
C
                          WRITE(UNIT=20,REC=I) MCDIR1(I),MCDIR2(1,I),MCDIR2(2,I),
     1                    MCDIR2(3,I),MCDIR3(I)
C
C
C       NOW FILE THE BODY OF THE MACRO STARTING AT RECORD 1 IN FILE
C       FILNAM AFTER DETERMINING FILNAM
                          NF=I
C
                          CALL MACFIL
                          OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FILNAM,FORM=
     1                    'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
                          DO 40 K=1,(IMACEN)
                              L=K-1
                              WRITE(UNIT=30,REC=K) MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),
     1                        MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),
     2                        MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),
     3                        MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),
     4                        MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L)
     5                        ,MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)
C
 40                       CONTINUE
                          CALL CLOSE_FILE(30,1)
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
                              FUNEXT(KLI)=.TRUE.
                              FCDIR1(KLI)=MACNAM
                              FCDIR2(1,KLI)=MACLOC
                              FCDIR2(2,KLI)=IMACEN
                              FCDIR2(3,KLI)=1
                              DO 302 K=1,(IMACEN)
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
 302                          CONTINUE
                          ELSE
C       NOT A FUNCTION
                          END IF
                          CALL CLOSE_FILE(20,1)
C
C       NOW THAT A MACRO IS FILED AND THE DIRECTORY IS UPDATED
C       IT IS TIME TO AUTOMATICALLY RETURN TO THE PROGRAM
C       CMD LEVEL.
C
                          F2=0
                          F1=1
C       THE CMD LEVEL IS NOW RE-ENABLED.
                          OUTLYNE=
     1                    ' MACRO '//MACNAM//' FILED - RETURNED TO CMD LEVEL'
                          CALL SHOWIT(1)
                          RETURN
                      ELSE
C       REPEAT THE SEARCH PROCESS FOR THE NEXT ENTRY IN THE MACRO
C       DIRECTORY

                      END IF
                  END IF
 20           CONTINUE
C
C       IF YOU GOT HERE, THEN THERE WAS NO OLD MACRO TO OVERWRITE
C       AND THERE WERE NO BLANK SPACES.
C       YOU SHOULD NEVER GET HERE
              OUTLYNE=
     1        'SERIOUS ERROR TRYING TO FILE CURRENT MACRO'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'THIS IS THE MACFILE BUG'
              CALL SHOWIT(1)
C       CLOSE UNIT 20
C
              CALL CLOSE_FILE(20,1)
C

              F2=0
              F1=1
              OUTLYNE='RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
          ELSE
C       MACRO HAD NO LINES
              OUTLYNE=
     1        'MACRO HAS NO LINES - RETURNED TO CMD LEVEL'
              CALL SHOWIT(1)
              F2=0
              F1=1
              CALL MACFAL
          END IF
          RETURN
      END
C SUB IMF.FOR
      SUBROUTINE IMF
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE INITIALIZES OR BLANKS OUT THE CURRENT
C       MACRO DIRECTORY.
C
          INTEGER I,ZERO,N
C
          CHARACTER BLANK*8,STAMP*20,FN*10,AN*3
C
          LOGICAL EXISJK
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
          ZERO=0
          BLANK='        '
          STAMP='                    '
C       OPEN UNIT 20 FOR I/O
C
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
          CALL CLOSE_FILE(20,0)
          DO N=1,MAXMAC
              CALL CCOONN(N,AN)
              IF(N.GT.0.AND.N.LE.9) FN='MAC00'//AN(3:3)//'.DAT'
              IF(N.GT.9.AND.N.LE.99) FN='MAC0'//AN(2:3)//'.DAT'
              IF(N.GT.99.AND.N.LE.999) FN='MAC'//AN(1:3)//'.DAT'
              EXISJK=.FALSE.
              INQUIRE(FILE=trim(LIBMAC)//FN,EXIST=EXISJK)
              IF(EXISJK) THEN
                  OPEN(UNIT=30,ACCESS='DIRECT',FILE=trim(LIBMAC)//FN,FORM=
     1            'UNFORMATTED',RECL=(55*NRECL),STATUS='UNKNOWN')
                  CALL CLOSE_FILE(30,0)
              END IF
          END DO
C
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
C
          DO 25 I=1,MAXMAC
              WRITE(UNIT=20,REC=I) BLANK,ZERO,ZERO,ZERO,STAMP
 25       CONTINUE
C
          CALL CLOSE_FILE(20,1)
C
C       CHECK EXISTANCE OF MACRO FILES MAC001 TO MAC(MAXMAC).DAT
C       IF FOUND. DELETE THEM
          CALL MCFLEX
          OUTLYNE=
     1    'MACRO FILE INITIALIZED'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
      END



C SUB INSLIN.FOR
      SUBROUTINE INSLIN
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS USED TO INSERT A PROCESSED COMMAND LINE
C       INTO A MACRO. THE NEW LINE IS INSERTED AT POSITION JUST
C       AFTER THE CURRENT LINE UNLESS THE CURRENT MACRO HAS NO LINES
C       OR HAS ONE BLANK LINE
C       AND IS BEING CREATED FROM SCRATCH IN MEDIT.
C       IF IT IS BEING CREATED FROM SCRATCH THEN INITIALLY FLAG
C       F46 = 1. IF SO THE FIRST LINE OF INPUT GOES INTO
C       LINE NUMBER 1. THEN F46 IS RESET TO 0 AND OTHER LINES ARE HANDELED
C       JUST AS IF THE MACRO WAS AN OLD MACRO.
C       THE NEW LINE IS THEN MADE TO BE THE
C       NEW CURRENT LINE. THE MCDIR2(2,(MMIJ)) OF MACRO MMIJ IS THEN
C       INCREMENTED. IF MCDIR2(2,(MMIJ)) = MAXLL. THE INSERTION OF LINES
C       IS NOT ALLOWED AND A MESSAGE IS PRINTED.
C
          INTEGER MMIJ,I,J,M,MAXLL
C
          COMMON/MIJ/MMIJ
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
C
C       IS THE MACRO A NEW ONE ?
C
          IF(F46.EQ.1) THEN
C                       YES
              MCDIR2(2,(MMIJ))=3
              MACCW(1)=WC
              MACQW(1)=WQ
              MACSTR(1)=WS
              MACNW(1,1)=W1
              MACNW(2,1)=W2
              MACNW(3,1)=W3
              MACNW(4,1)=W4
              MACNW(5,1)=W5
              MACSTA(1,1)=SB1
              MACSTA(2,1)=SB2
              MACSTA(3,1)=SC1
              MACSTA(4,1)=SC2
              MACSTA(5,1)=SQ
              MACSTA(6,1)=SST
              MACSTA(7,1)=S1
              MACSTA(8,1)=S2
              MACSTA(9,1)=S3
              MACSTA(10,1)=S4
              MACSTA(11,1)=S5
              MACSTA(12,1)=DF1
              MACSTA(13,1)=DF2
              MACSTA(14,1)=DF3
              MACSTA(15,1)=DF4
              MACSTA(16,1)=DF5
              MACSTA(17,1)=SN
              MACSTA(18,1)=STI
C       MACSTA(19,1) AND MACSTA(20,1) NOT YET USED
              MACSTA(19,1)=0
              MACSTA(20,1)=0
C       SET F46 TO 0
              F46=0
              RETURN
          ELSE
          END IF
C
C       DOES THE MACRO CONSIST OF ONE BLANK LINE? IF YES START BY
C       WRITING OVER IT.
C
          IF(MCDIR2(2,(MMIJ)).EQ.3.AND.MACCW(1).EQ.' ') THEN
C
              CURLIN=1
              MACCW(1)=WC
              MACQW(1)=WQ
              MACSTR(1)=WS
              MACNW(1,1)=W1
              MACNW(2,1)=W2
              MACNW(3,1)=W3
              MACNW(4,1)=W4
              MACNW(5,1)=W5
              MACSTA(1,1)=SB1
              MACSTA(2,1)=SB2
              MACSTA(3,1)=SC1
              MACSTA(4,1)=SC2
              MACSTA(5,1)=SQ
              MACSTA(6,1)=SST
              MACSTA(7,1)=S1
              MACSTA(8,1)=S2
              MACSTA(9,1)=S3
              MACSTA(10,1)=S4
              MACSTA(11,1)=S5
              MACSTA(12,1)=DF1
              MACSTA(13,1)=DF2
              MACSTA(14,1)=DF3
              MACSTA(15,1)=DF4
              MACSTA(16,1)=DF5
              MACSTA(17,1)=SN
              MACSTA(18,1)=STI
C       MACSTA(19,1) AND MACSTA(20,1) NOT YET USED
              MACSTA(19,1)=0
              MACSTA(20,1)=0
              RETURN
          ELSE
C       NOT ONE BLANK LINE, PROCEED
          END IF
C
C       MACRO IS NOT NEW
C
C       DON'T ADD LINES BEYOND THE EOM
C
          IF(CURLIN.EQ.(MCDIR2(2,(MMIJ))-1)) THEN
              CURLIN=MCDIR2(2,(MMIJ))-2
          ELSE
          END IF
C
          CALL MAXLNN(MAXLL,MAXLIN)
          IF(MCDIR2(2,(MMIJ)).EQ.(MAXLL-2)) THEN
              WRITE(OUTLYNE,*)
     1          'MACRO HAS ',(MAXLL-2),' LINES. NO INPUT IS ALLOWED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       NOW SHIFT ALL OF THE MACRO STORAGE STARTING AT
C       CURLIN+1 TO THE END OF THE MACRO. INCREMENT
C       MCDIR2(2,(MMIJ)) BY 1. THEN STORE THE NEW LINE INTO
C       THE CURLIN+1 POSITION AND THE REDEFINE CURLIN = CURLIN+1
C
C
          I=(MCDIR2(2,(MMIJ))-2)
          DO 20 J=I,(CURLIN+1),-1
              MACCW(J+1)=MACCW(J)
              MACQW(J+1)=MACQW(J)
              MACSTR(J+1)=MACSTR(J)
              MACNW(1,J+1)=MACNW(1,(J))
              MACNW(2,J+1)=MACNW(2,(J))
              MACNW(3,J+1)=MACNW(3,(J))
              MACNW(4,J+1)=MACNW(4,(J))
              MACNW(5,J+1)=MACNW(5,(J))
              MACSTA(1,J+1)=MACSTA(1,(J))
              MACSTA(2,J+1)=MACSTA(2,(J))
              MACSTA(3,J+1)=MACSTA(3,(J))
              MACSTA(4,J+1)=MACSTA(4,(J))
              MACSTA(5,J+1)=MACSTA(5,(J))
              MACSTA(6,J+1)=MACSTA(6,(J))
              MACSTA(7,J+1)=MACSTA(7,(J))
              MACSTA(8,J+1)=MACSTA(8,(J))
              MACSTA(9,J+1)=MACSTA(9,(J))
              MACSTA(10,J+1)=MACSTA(10,(J))
              MACSTA(11,J+1)=MACSTA(11,(J))
              MACSTA(12,J+1)=MACSTA(12,(J))
              MACSTA(13,J+1)=MACSTA(13,(J))
              MACSTA(14,J+1)=MACSTA(14,(J))
              MACSTA(15,J+1)=MACSTA(15,(J))
              MACSTA(16,J+1)=MACSTA(16,(J))
              MACSTA(17,J+1)=MACSTA(17,(J))
              MACSTA(18,J+1)=MACSTA(18,(J))
              MACSTA(19,J+1)=0
              MACSTA(20,J+1)=0
C       MACSTA(19,IMAC) AND MACSTA(20,IMAC) NOT YET USED
 20       CONTINUE
C       INCREMENT THE MACRO LENGTH COUNTER BY ONE
          MCDIR2(2,(MMIJ))=MCDIR2(2,(MMIJ))+1
C       NOW INSERT THE NEW INSTRUCTION AT CURLIN+1
          M=CURLIN+1
          MACCW(M)=WC
          MACQW(M)=WQ
          MACSTR(M)=WS
          MACNW(1,M)=W1
          MACNW(2,M)=W2
          MACNW(3,M)=W3
          MACNW(4,M)=W4
          MACNW(5,M)=W5
          MACSTA(1,M)=SB1
          MACSTA(2,M)=SB2
          MACSTA(3,M)=SC1
          MACSTA(4,M)=SC2
          MACSTA(5,M)=SQ
          MACSTA(6,M)=SST
          MACSTA(7,M)=S1
          MACSTA(8,M)=S2
          MACSTA(9,M)=S3
          MACSTA(10,M)=S4
          MACSTA(11,M)=S5
          MACSTA(12,M)=DF1
          MACSTA(13,M)=DF2
          MACSTA(14,M)=DF3
          MACSTA(15,M)=DF4
          MACSTA(16,M)=DF5
          MACSTA(17,M)=SN
          MACSTA(18,M)=STI
C       MACSTA(19,M) AND MACSTA(20,M) NOT YET USED
          MACSTA(19,M)=0
          MACSTA(20,M)=0
C       NOW INCREMENT CURLIN BY 1
          CURLIN=CURLIN+1
          RETURN
      END
C SUB FUNX1.FOR
C
      SUBROUTINE FUNX1
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
          COMMON/FACH/H
C
          INCLUDE 'datmac.inc'
C
          IF(INT(DABS(H(2,CT))).EQ.0) THEN
              PASS2=((((PASS1)*H(3,CT))+H(4,CT))
     1        **INT(H(5,CT)))
          ELSE
              PASS2=((((FMNW(INT(DABS(H(2,CT)))))*H(3,CT))+H(4,CT))
     1        **INT(H(5,CT)))
          END IF
          RETURN
      END
C SUB FUNNAME.FOR
      SUBROUTINE FUNNAME
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE DEFINES ALTERNAMTE MACRO FUNCTION NAMES
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          IF(SST.EQ.1) THEN
              OUTLYNE='"FUNNAME" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"FUNNAME" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) THEN
              OUTLYNE='"FUNNAME" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.NE.1.0D0.AND.W1.NE.2.0D0.AND.W1.NE.3.0D0.AND.W1.NE.4.0D0
     1    .AND.W1.NE.5.0D0.AND.W1.NE.6.0D0.AND.W1.NE.7.0D0.AND.W1.NE.8.0D0
     1    .AND.W1.NE.9.0D0.AND.W1.NE.10.0D0
     1    .AND.W1.NE.11.0D0.AND.W1.NE.12.0D0
     1    .AND.W1.NE.13.0D0.AND.W1.NE.14.0D0
     1    .AND.W1.NE.15.0D0.AND.W1.NE.16.0D0
     1    .AND.W1.NE.17.0D0.AND.W1.NE.18.0D0
     1    .AND.W1.NE.19.0D0.AND.W1.NE.20.0D0) THEN
              OUTLYNE='"VALID FUNCTION NUMBERS ARE 1 THROUGHT 20'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(STI.EQ.1) THEN
              OUTLYNE='NO INFORMATION AVLAILABLE'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE='"FUNNAME" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          FNAMED(INT(W1))=WQ(1:8)
          RETURN
      END
C SUB FUNEXC.FOR
      SUBROUTINE FUNEXC
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE RUNS A MACRO FUNCTION FOR USE IN
C       SPECIAL SURFACE DEFINITIONS AND MERIT FUNCTION CONSTRUCTION
C
C       THE FUNCTION THAT IT EXECUTES IS THE NF TH FUNCTION. PROVISION
C       NO NESTING IS ALLOWED HERE
C
          LOGICAL READERR,FUN,NSUBDV
     1    ,MOVEYES,HAP1
C
          COMMON/ERRREAD/READERR
C
          CHARACTER WQOLD*8
     1    ,ACCWRD*8,EE1*8,EE2*80,WCOLD*8,STROLD*80
C
          INTEGER FLG(0:20),SNOLD,SQOLD,LINCUR,LINBOT
     1    ,STOLD,S1OLD,S2OLD,S3OLD,S4OLD,S5OLD
     2    ,DF1OLD,DF2OLD,DF3OLD,DF4OLD,DF5OLD
C
!        REAL*8 XCJ,YCJ,ZCJ
C
          COMMON/STAT13/LINCUR,LINBOT
C
!        INTEGER LLL,LLK
C
          LOGICAL QRSUB,CRSUB
C
          INTEGER CT,
     6    NSUB,CSUB,QSUB,SSUB,Q(0:20),SPOINT,FTF,ACCSUB,
     7    CTF,NF,K,KK,LL,ACCCNT,I,SSSTEP
C
          REAL*8 H(1:5,0:10),HOLDER,
     1    PASS1,PASS2,W1OLD,W2OLD,W3OLD,W4OLD,W5OLD
C
          COMMON/COMACC1/PASS1
C
          COMMON/COMACC2/PASS2
C
          COMMON/COMCT/CT
C
          COMMON/FFL/FLG
C
          COMMON/FTRA/FTF,SSSTEP
C
          COMMON/ACCSB/ACCWRD
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          COMMON/FACH/H
C
          COMMON/NEFER/NF
C
          COMMON/COMFUN/FUN
C
          INCLUDE 'datmac.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datsp1.inc'
C
C       THE MACRO INFORMATION IS STORED IN A PRE-PROCESSED
C       FORM AND THUS CALLS NEED NOT BE PASSED TO
C       THE PROCESS ROUTINE BUT CAN GO DIRECTLY TO SUBROUTINE
C       CONTROL.
          F57=0
C     OR WE HAVE PROBLEMS WITH AUTOFUNC
C
C       FUNCW,FUNQW,AND FUNSTR HOLD COMMWD,QUALWD AND STRINGS
C       FOR MAXLL FUNCTION LINES. FUNSTA HOLDS VARIOUS STATUS INDICATORS.
C       FUNNW HOLDS NUMERIC WORDS.
C
C       THE INITIAL VALUES STORED IN THE FUNCTION DIRECTORY ARE:
C
C       SET FTF=0 AS THE DEFAULT WHEN BEGINNING FUNCTION EXECUTION
          FTF=0
C
C       INITIALIZE NSUB,QSUB,SSUB AND CSUB
          QRSUB=.FALSE.
          CRSUB=.FALSE.
          NSUB=0
          QSUB=0
          SSUB=0
          CSUB=0
          CT=0
C       CT COUNTS THE NUMBER OF STACKED NSUBS
C
C               FCDIR1(NF) IS THE FUNCTION NAME
C               FCDIR2(1,NF) IS THE LOCK
C               FCDIR2(2,NF) IS THE LENGTH
C               FCDIR2(3,NF) IS THE OCCUPANCY FLAG
C                               = 0 FOR BLANK FUNCTION
C                               = 1 FOR NON-BLANK FUNCTION
C
C       FUN WAS TRUE, EXECUTE THE FUNCTION DESIGNATED BY NF
C***********************************************************************
C        FCDIR1(NF)
C        FCDIR2(1,NF)
C        FCDIR2(2,NF)
C        FCDIR2(3,NF)
C        FCDIR3(NF)
C        FUNCW(NF,L)
C        FUNQW(NF,L)
C        FUNSTR(NF,L)
C        FUNNW(NF,1,L)
C        FUNNW(NF,2,L)
C        FUNNW(NF,3,L)
C        FUNNW(NF,4,L)
C        FUNNW(NF,5,L)
C        FUNSTA(NF,1,L)
C        FUNSTA(NF,2,L)
C        FUNSTA(NF,3,L)
C        FUNSTA(NF,4,L)
C        FUNSTA(NF,5,L)
C        FUNSTA(NF,6,L)
C        FUNSTA(NF,7,L)
C        FUNSTA(NF,8,L)
C        FUNSTA(NF,9,L)
C        FUNSTA(NF,10,L)
C        FUNSTA(NF,11,L)
C        FUNSTA(NF,12,L)
C        FUNSTA(NF,13,L)
C        FUNSTA(NF,14,L)
C        FUNSTA(NF,15,L)
C        FUNSTA(NF,16,L)
C        FUNSTA(NF,17,L)
C        FUNSTA(NF,18,L)
C        FUNSTA(NF,19,L)
C        FUNSTA(NF,20,L)
C
C***********************************************************************
C
C       NOW PROCESS THE COMMAND THROUGH THE SUBROUTINE CONTROL
C       STARTING WITH THE SECOND ENTRY AND CEASE EXECUTION AT THE
C       IMACEN-1 TH ENTRY.( THIS AVOIDS PROCESSING THE WORDS
C       MACRO OR EOM)
C
C       BEFORE CALLING CONTROL, HOWEVER, CHECK THAT NONE OF
C       THE COMMANDS STORED IN THE FUNCTION ARRAYS ARE THEMSELVES
C       MACRO OR FUNCTION NAMES. (NESTING IS NOT ALLOWED HERE)
C
C       SET THE STARTING LINE NUMBER HERE (SPOINT)
          SPOINT=1
          CONTINUE
          I=SPOINT
          GO TO 54
 5111     IF(CT.NE.0) CT=CT-1
          IF(CT.EQ.0) NSUB=0
          I=I+1
 54       CONTINUE
C
          IF(FUNCW(NF,I).EQ.'EOM') THEN
C       RAN INTO AN EOM SO ALL FUNCTION EXECUTION STOPS
              CALL MACFAL
              RETURN
          END IF
C       DOES THE FUNCTION CALL A FUNCTION ?
          DO K=1,10
              IF(FUNCW(NF,I).EQ.FCDIR1(K)) THEN
C
C       WE JUST CALLED A FUNCTION
                  OUTLYNE='A FUNCTION IS NOT ALLOWED TO CALL A FUNCTION'
                  CALL SHOWIT(1)
                  OUTLYNE='WHEN USED IN SPECIAL SURFACES OR OPTIMIZATION'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END DO
          DO 40 K=1,10
              IF(FUNCW(NF,I).EQ.MCDIR1(K)) THEN
C
C       THE FUNCTION CALLED A MACRO
                  OUTLYNE='A FUNCTION IS NOT ALLOWED TO CALL A MACRO'
                  CALL SHOWIT(1)
                  OUTLYNE='WHEN USED IN SPECIAL SURFACES OR OPTIMIZATION'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
 40       CONTINUE
C
C       IF YOU GOT HERE THEN
C       THE FUNCTION DID NOT CALL A MACRO OR A FUNCTION. THE COMMAND IS
C       EITHER A VALID PROGRAM COMMAND OR AN INVALID
C       PROGRAM COMMAND.
C***********************************************************************
C       NOW IF A COMMENT LINE IS ENCOUNTERED WE WANT TO DO NOTHING
C       IF WE DON'T HANDLE THE COMMENT HERE WE RISK THE POSSIBILITY
C       OF CSUB,QSUB,SSUB OR NSUB SUBING INTO A COMMENT. THIS NEVER NEEDS
C       TO OCCUR.
C
          IF(FUNCW(NF,I).EQ.'C') THEN
C       SKIP TO THE NEXT LINE BY A GO TO 50
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
          IF(FUNCW(NF,I).EQ.'M') THEN
C       HANDLE MESSAGE NOW THEN SKIP TO NEXT LINE
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
              WC=FUNCW(NF,I)
              WS=FUNSTR(NF,I)
              SQ=FUNSTA(NF,5,I)
              SN=FUNSTA(NF,17,I)
              W1=FUNNW(NF,1,I)
              W2=FUNNW(NF,2,I)
              W3=FUNNW(NF,3,I)
              W4=FUNNW(NF,4,I)
              W5=FUNNW(NF,5,I)
              SST=FUNSTA(NF,6,I)
              S1=FUNSTA(NF,7,I)
              S2=FUNSTA(NF,8,I)
              S3=FUNSTA(NF,9,I)
              S4=FUNSTA(NF,10,I)
              S5=FUNSTA(NF,11,I)
              DF1=FUNSTA(NF,12,I)
              DF2=FUNSTA(NF,13,I)
              DF3=FUNSTA(NF,14,I)
              DF4=FUNSTA(NF,15,I)
              DF5=FUNSTA(NF,16,I)
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
          IF(FUNCW(NF,I).EQ.'MOVE'.AND.FUNQW(NF,I).NE.'NW') THEN
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
              WC=FUNCW(NF,I)
              WQ=FUNQW(NF,I)
              WS=FUNSTR(NF,I)
              SQ=FUNSTA(NF,5,I)
              SN=FUNSTA(NF,17,I)
              W1=FUNNW(NF,1,I)
              W2=FUNNW(NF,2,I)
              W3=FUNNW(NF,3,I)
              W4=FUNNW(NF,4,I)
              W5=FUNNW(NF,5,I)
              SST=FUNSTA(NF,6,I)
              S1=FUNSTA(NF,7,I)
              S2=FUNSTA(NF,8,I)
              S3=FUNSTA(NF,9,I)
              S4=FUNSTA(NF,10,I)
              S5=FUNSTA(NF,11,I)
              DF1=FUNSTA(NF,12,I)
              DF2=FUNSTA(NF,13,I)
              DF3=FUNSTA(NF,14,I)
              DF4=FUNSTA(NF,15,I)
              DF5=FUNSTA(NF,16,I)
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
                  IF(FTF.EQ.1) THEN
C               TRACE IS ON. DON'T PRINT MACRO COMMANDS HOWEVER
                      CALL FTRACER
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
C       IF A (BP) COMMAND IS FOUND IN THE NORMAL PROCESS OF FUNCTION
C       EXECUTION, JUST INCREMENT THE LINE COUNTER I BY 1 AND
C       PROCEED BY JUMPING TO 54
          IF(FUNCW(NF,I).EQ.'BP') THEN
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
C       IN THAT CASE WE STOP EXECUTION OF THE FUNCTION.
C     AND RETURN TO THE CALLING SUBROUTINE
C
          IF(FUNCW(NF,I).EQ.'RETURN') THEN
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
          END IF
C       NOW WHAT IF WE ENCOUNTER A PAUSE COMMAND.
C       IN THAT CASE WE TEMPORARILY STOP EXECUTION OF THE CURRENT MACRO.
C       UNTIL THE RETURN KEY IS PRESSED
C
          IF(FUNCW(NF,I).EQ.'PAUSE') THEN
              CALL MACPAUSE
              GO TO 50
          END IF
C
C       SET CSUB FOR CSUBS
          IF(FUNCW(NF,I).EQ.'CSUB'.OR.FUNCW(NF,I).EQ.'CRSUB') THEN
              IF(FUNCW(NF,I).EQ.'CRSUB') CRSUB=.TRUE.
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
          IF(FUNCW(NF,I).EQ.'QSUB'.AND.FUNQW(NF,I).EQ.'DV'.AND.
     1    FMSQ.EQ.0) THEN
              FMWQ=(FUNSTR(NF,I)(1:8))
              GO TO 50
          ELSE
              IF(FUNCW(NF,I).EQ.'QSUB'.AND.FUNQW(NF,I).EQ.'DV'.AND.
     1        FMSQ.NE.0) GO TO 50
          END IF
C       HANDLE SSUB DV
          IF(FUNCW(NF,I).EQ.'SSUB'.AND.FUNQW(NF,I).EQ.'DV'.AND.
     1    FMST.EQ.0) THEN
              FMWS=(FUNSTR(NF,I)(1:80))
              GO TO 50
          ELSE
              IF(FUNCW(NF,I).EQ.'SSUB'.AND.FUNQW(NF,I).EQ.'DV'.AND.
     1        FMST.NE.0) GO TO 50
          END IF
C       SET QSUB FOR QSUBS
          IF(FUNCW(NF,I).EQ.'QSUB'.AND.FUNQW(NF,I).EQ.' '.OR.
     1    FUNCW(NF,I).EQ.'QRSUB'.AND.FUNQW(NF,I).EQ.' ') THEN
              IF(FUNCW(NF,I).EQ.'QSUB') QRSUB=.FALSE.
              IF(FUNCW(NF,I).EQ.'QRSUB') QRSUB=.TRUE.
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
          IF(FUNCW(NF,I).EQ.'SSUB'.AND.FUNQW(NF,I).EQ.' ') THEN
              SSUB=1
              GO TO 50
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
          IF(FUNCW(NF,I).EQ.'ACCSUB') THEN
              ACCSUB=1
              ACCWRD=FUNQW(NF,I)
              HOLDER=FUNNW(NF,1,I)
              ACCCNT=INT(HOLDER)
          ELSE
              IF(ACCSUB.EQ.1) THEN
                  ACCSUB=1
              ELSE
                  ACCSUB=0
              END IF
          END IF
C       HANDLE NSUB DV
          NSUBDV=.FALSE.
          IF(FUNCW(NF,I).EQ.'NSUB'.AND.FUNQW(NF,I).EQ.'DV') THEN
C       FUNNW'S ARE THE NUMERIC WORDS OF THE NSUB DV STATEMENT.
C       FMNW'S ARE THE MACRO COMMAND LINE NUMERIC WORDS
C       FMDF'S TRACK WHETHER THE MACRO COMMAND LINE NUMERIC
C       WORDS ARE DEFAULT BLANK OR EXIT IN A NON-DEFAULT WAY.
              IF(FMDF(1).EQ.1) FMNW(1)=FUNNW(NF,1,I)
              IF(FMDF(2).EQ.1) FMNW(2)=FUNNW(NF,2,I)
              IF(FMDF(3).EQ.1) FMNW(3)=FUNNW(NF,3,I)
              IF(FMDF(4).EQ.1) FMNW(4)=FUNNW(NF,4,I)
              IF(FMDF(5).EQ.1) FMNW(5)=FUNNW(NF,5,I)
              NSUBDV=.TRUE.
              GO TO 50
          END IF
C       ************************************************************
C       SET VARIOUS NSUBS
C               FIRST JUST NSUB
          IF(FUNCW(NF,I).EQ.'NSUB') THEN
              IF(NSUB.EQ.0) THEN
                  NSUB=1
                  CT=1
C       THE H ARRAY CONTAINS NUMERIC WORD VALUES WHICH ARE
C       FOR NUMERIC WORD 1 AND 2 ACT AS ADRESSES FOR THE
C       EXECUTABLE STATEMENT WHICH FOLLOWS THE NSUBS.
                  H(1,CT)=FUNNW(NF,1,I)
                  H(2,CT)=FUNNW(NF,2,I)
                  IF(H(2,CT).EQ.0.0D0) REG9(CT)=REG(9)
                  H(3,CT)=FUNNW(NF,3,I)
                  H(4,CT)=FUNNW(NF,4,I)
                  H(5,CT)=FUNNW(NF,5,I)
              ELSE
                  CT=CT+1
                  IF(CT.GT.10) THEN
                      OUTLYNE='MAXIMUM NUMBER OF CONSECUTIVE NSUBS = 10'
                      CALL SHOWIT(1)
                      OUTLYNE='HAS BEEN REACHED, FUNCTION EXECUTION ABORTED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  ELSE
                      H(1,CT)=FUNNW(NF,1,I)
                      H(2,CT)=FUNNW(NF,2,I)
                      IF(H(2,CT).EQ.0.0D0) REG9(CT)=REG(9)
                      H(3,CT)=FUNNW(NF,3,I)
                      H(4,CT)=FUNNW(NF,4,I)
                      H(5,CT)=FUNNW(NF,5,I)
                  END IF
              END IF
              IF(FUNQW(NF,I).EQ.' ') THEN
                  Q(CT)=1
C       THE CURRENT NSUB REFERENCED BY J IS A SIMPLE NSUB
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
C
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='FUNCTION EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=FMNW(1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=FMNW(2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=FMNW(3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=FMNW(4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=FMNW(5)
                      CALL FACC1(CT)
                      CALL FUNDEF
                      CALL FUNX1
                      CALL FACC2
                      GO TO 5111
                  END IF
              END IF
              IF(FUNQW(NF,I).EQ.'RA') THEN
                  Q(CT)=2
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RA QUALIFIER.
C       THIS IS JUST LIKE NSUB EXCEPT THAT THE MULTIPLIER IS NOW
C                       FMNW(INT(DABS(H(3,CT)))) INSTEAD OF
C       JUST            H(3,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE FUNCTION USING
C       NSUB DV.
C
                  H(3,CT)=FMNW(INT(DABS(H(3,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='FUNCTION EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=FMNW(1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=FMNW(2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=FMNW(3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=FMNW(4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=FMNW(5)
                      CALL FACC1(CT)
                      CALL FUNDEF
                      CALL FUNX1
                      CALL FACC2
                      GO TO 5111
                  END IF
              END IF
              IF(FUNQW(NF,I).EQ.'RB') THEN
                  Q(CT)=3
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RB QUALIFIER.
C       THIS IS JUST LIKE NSUB EXCEPT THAT THE MULTIPLIER IS NOW
C                       FMNW(INT(DABS(H(4,CT)))) INSTEAD OF
C       JUST            H(4,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE FUNCTION USING
C       NSUB DV.
C
                  H(4,CT)=FMNW(INT(DABS(H(4,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='FUNCTION EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=FMNW(1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=FMNW(2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=FMNW(3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=FMNW(4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=FMNW(5)
                      CALL FACC1(CT)
                      CALL FUNDEF
                      CALL FUNX1
                      CALL FACC2
                      GO TO 5111
                  END IF
              END IF
              IF(FUNQW(NF,I).EQ.'RC') THEN
                  Q(CT)=4
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RC QUALIFIER.
C       THIS IS JUST LIKE NSUB EXCEPT THAT THE MULTIPLIER IS NOW
C                       FMNW(INT(DABS(H(5,CT)))) INSTEAD OF
C       JUST            H(5,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE FUNCTION USING
C       NSUB DV.
C
                  H(5,CT)=FMNW(INT(DABS(H(5,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='FUNCTION EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=FMNW(1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=FMNW(2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=FMNW(3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=FMNW(4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=FMNW(5)
                      CALL FACC1(CT)
                      CALL FUNDEF
                      CALL FUNX1
                      CALL FACC2
                      GO TO 5111
                  END IF
              END IF
              IF(FUNQW(NF,I).EQ.'RAB') THEN
                  Q(CT)=5
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RAB QUALIFIER.
C       THIS IS RA AND RB
C
                  H(3,CT)=FMNW(INT(DABS(H(3,CT))))
                  H(4,CT)=FMNW(INT(DABS(H(4,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='FUNCTION EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=FMNW(1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=FMNW(2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=FMNW(3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=FMNW(4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=FMNW(5)
                      CALL FACC1(CT)
                      CALL FUNDEF
                      CALL FUNX1
                      CALL FACC2
                      GO TO 5111
                  END IF
              END IF
              IF(FUNQW(NF,I).EQ.'RAC') THEN
                  Q(CT)=6
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RAC QUALIFIER.
C       THIS IS RA AND RC
C
                  H(3,CT)=FMNW(INT(DABS(H(3,CT))))
                  H(5,CT)=FMNW(INT(DABS(H(5,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='FUNCTION EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=FMNW(1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=FMNW(2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=FMNW(3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=FMNW(4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=FMNW(5)
                      CALL FACC1(CT)
                      CALL FUNDEF
                      CALL FUNX1
                      CALL FACC2
                      GO TO 5111
                  END IF
              END IF
              IF(FUNQW(NF,I).EQ.'RBC') THEN
                  Q(CT)=7
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RA QUALIFIER.
C       THIS IS RB AND RC
C
                  H(4,CT)=FMNW(INT(DABS(H(4,CT))))
                  H(5,CT)=FMNW(INT(DABS(H(5,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='FUNCTION EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=FMNW(1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=FMNW(2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=FMNW(3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=FMNW(4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=FMNW(5)
                      CALL FACC1(CT)
                      CALL FUNDEF
                      CALL FUNX1
                      CALL FACC2
                      GO TO 5111
                  END IF
              END IF
              IF(FUNQW(NF,I).EQ.'RABC') THEN
                  Q(CT)=8
C       THE CURRENT NSUB REFERENCED BY J IS NSUB WITH RABC QUALIFIER.
C       THIS IS RA RB AND RC
C
                  H(3,CT)=FMNW(INT(DABS(H(3,CT))))
                  H(4,CT)=FMNW(INT(DABS(H(4,CT))))
                  H(5,CT)=FMNW(INT(DABS(H(5,CT))))
C
                  IF(INT(DABS(H(1,CT))).EQ.0) THEN
                      IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                          OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                          CALL SHOWIT(1)
                          OUTLYNE='FUNCTION EXECUTION ABORTED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(DABS(H(2,CT))).EQ.1) REG(9)=FMNW(1)
                      IF(INT(DABS(H(2,CT))).EQ.2) REG(9)=FMNW(2)
                      IF(INT(DABS(H(2,CT))).EQ.3) REG(9)=FMNW(3)
                      IF(INT(DABS(H(2,CT))).EQ.4) REG(9)=FMNW(4)
                      IF(INT(DABS(H(2,CT))).EQ.5) REG(9)=FMNW(5)
                      CALL FACC1(CT)
                      CALL FUNDEF
                      CALL FUNX1
                      CALL FACC2
                      GO TO 5111
                  END IF
              END IF
              IF(FUNQW(NF,I).NE.' '.AND.FUNQW(NF,I).NE.'RA'.AND.
     1        FUNQW(NF,I).NE.'RB'.AND.FUNQW(NF,I).NE.'RC'.AND.FUNQW(NF,I).NE.
     2        'RAB'.AND.FUNQW(NF,I).NE.'RAC'.AND.FUNQW(NF,I).NE.'RBC'.AND.
     3        FUNQW(NF,I).NE.'RABC') Q(CT)=99
              GO TO 50
          ELSE
C       FUNCW(I) NOT NSUB BUT
              IF(NSUB.EQ.1) THEN
                  NSUB=1
              ELSE
                  NSUB=0
              END IF
C               PROCEED
          END IF
C       ************************************************************
C               HANDEL MOVE NW
C
          IF(FUNCW(NF,I).EQ.'MOVE'.AND.FUNQW(NF,I).EQ.'NW') THEN
              IF((INT(DABS(FUNNW(NF,1,I)))).EQ.0) FUNNW(NF,1,I)=INT(REG(17))
              IF((INT(DABS(FUNNW(NF,1,I)))).EQ.0) THEN
                  OUTLYNE='VALUE OF I REGISTER EQUAL TO 0'
                  CALL SHOWIT(1)
                  OUTLYNE='FOR (MOVE NW,0) COMMAND'
                  CALL SHOWIT(1)
                  OUTLYNE='DEFAULT: CONTENTS OF NW1 MOVED INTO ACCUMULATOR'
                  CALL SHOWIT(1)
                  REG(40)=REG(9)
                  REG(9)=FMNW(1)
                  GO TO 50
              END IF
              IF(INT(DABS(FUNNW(NF,1,I))).EQ.0)
     1                FMNW(INT(DABS(FUNNW(NF,1,I))))=REG(9)
              IF(INT(DABS(FUNNW(NF,1,I))).GT.5) THEN
                  OUTLYNE='VALUE OF I REGISTER GREATER THAN 5'
                  CALL SHOWIT(1)
                  OUTLYNE='FOR (MOVE NW,0) COMMAND'
                  CALL SHOWIT(1)
                  OUTLYNE='DEFAULT: CONTENTS OF NW1 MOVED INTO ACCUMULATOR'
                  CALL SHOWIT(1)
                  REG(40)=REG(9)
                  REG(9)=FMNW(1)
                  GO TO 50
              END IF
              REG(40)=REG(9)
              REG(9)=FMNW(INT(FUNNW(NF,1,I)))
              GO TO 50
          END IF
C       ************************************************************
C                       HANDLE PUTR
C
C       PUTR IS ONLY VALID FROM INSIDE A FUNCTION.
C       THIS COMMAND REPLACES INT(FUNNW(NF,1,I)) TH NUMERIC WORD
C       OF THE INPUT MACRO COMMAND RECORD BY THE
C       NUMBER IN THE NAMED REGISTER. IF FUNNW(NF,1,I) IS ZERO, THE INTEGER
C       VALUE OF THE INDEX REGISTER I WILL BE USED INSTEAD.
C       IF IT IS ZERO ALSO THE THE FIRST NUMERIC WORD WILL BE
C       THE DEFAULT. IF THE INTEGER VALUE OF THE I REGISTER IS GREATER
C       THAN 5  THEN THE FIRST NUMERIC WORD WILL ALSO BE THE DEFAULT.
C       VALID REG NAMES ARE: A,B,C,D,E,F,G,X,Y,Z,T,
C       IX,IY,IZ,IT
C
          IF(FUNCW(NF,I).EQ.'PUTR') THEN
              IF(FUNQW(NF,I).NE.'A'.AND.FUNQW(NF,I).NE.'B'
     1        .AND.FUNQW(NF,I).NE.'C'.AND.FUNQW(NF,I).NE.'D'.AND.
     2        FUNQW(NF,I).NE.'E'.AND.FUNQW(NF,I).NE.'F'.AND.
     3        FUNQW(NF,I).NE.'G'.AND.FUNQW(NF,I).NE.'H'.AND.FUNQW(NF,I).NE.'Y'
     4        .AND.FUNQW(NF,I).NE.'Z'.AND.FUNQW(NF,I).NE.'T'.AND.
     5        FUNQW(NF,I).NE.'IX'.AND.FUNQW(NF,I).NE.'IY'.AND.
     5        FUNQW(NF,I).NE.'IZ'
     6        .AND.FUNQW(NF,I).NE.'IT'.AND.FUNQW(NF,I).NE.' '
     7        .AND.FUNQW(NF,I).NE.'X') THEN
                  OUTLYNE='INVALID REGISTER NAME FOR (PUTR)'
                  CALL SHOWIT(1)
                  GO TO 50
              ELSE
                  IF((INT(DABS(FUNNW(NF,1,I))).EQ.0)) FUNNW(NF,1,I)=REG(17)
                  IF((INT(DABS(FUNNW(NF,1,I))).EQ.0)) THEN
                      OUTLYNE='VALUE OF I REGISTER EQUAL TO 0'
                      CALL SHOWIT(1)
                      OUTLYNE='FOR THE (PUTR,0) COMMAND'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD 1 USED AS THE DEFAULT'
                      CALL SHOWIT(1)
                      FUNNW(NF,1,I)=1.0D0
                  END IF
C
                  IF(INT(DABS(FUNNW(NF,1,I))).GT.5) THEN
                      OUTLYNE='VALUE OF I REGISTER GREATER THAN 5'
                      CALL SHOWIT(1)
                      OUTLYNE='FOR THE (PUTR,0) COMMAND'
                      CALL SHOWIT(1)
                      OUTLYNE='NUMERIC WORD 1 USED AS THE DEFAULT'
                      CALL SHOWIT(1)
                      FUNNW(NF,1,I)=1.0
                  END IF
                  KK=INT(DABS(FUNNW(NF,1,I)))
                  IF(ACCSUB.EQ.1) THEN
                      IF(FUNQW(NF,I).EQ.'ACC'.OR.FUNQW(NF,I).EQ.'X'.OR.
     1                FUNQW(NF,I).EQ.' ') THEN
                          FUNQW(NF,I)=ACCWRD
                          ACCCNT=ACCCNT-1
                          IF(ACCCNT.EQ.0) ACCSUB=0
                      END IF
C       ACCSUB WAS NOT IN EFFECT
                  END IF
C
                  IF(FUNQW(NF,I).EQ.'A') FMNW(KK)=REG(1)
                  IF(FUNQW(NF,I).EQ.'B') FMNW(KK)=REG(2)
                  IF(FUNQW(NF,I).EQ.'C') FMNW(KK)=REG(3)
                  IF(FUNQW(NF,I).EQ.'D') FMNW(KK)=REG(4)
                  IF(FUNQW(NF,I).EQ.'E') FMNW(KK)=REG(5)
                  IF(FUNQW(NF,I).EQ.'F') FMNW(KK)=REG(6)
                  IF(FUNQW(NF,I).EQ.'G') FMNW(KK)=REG(7)
                  IF(FUNQW(NF,I).EQ.'H') FMNW(KK)=REG(8)
                  IF(FUNQW(NF,I).EQ.'X') FMNW(KK)=REG(9)
                  IF(FUNQW(NF,I).EQ.' ') FMNW(KK)=REG(9)
                  IF(FUNQW(NF,I).EQ.'Y') FMNW(KK)=REG(10)
                  IF(FUNQW(NF,I).EQ.'Z') FMNW(KK)=REG(11)
                  IF(FUNQW(NF,I).EQ.'T') FMNW(KK)=REG(12)
                  IF(FUNQW(NF,I).EQ.'IX') FMNW(KK)=REG(13)
                  IF(FUNQW(NF,I).EQ.'IY') FMNW(KK)=REG(14)
                  IF(FUNQW(NF,I).EQ.'IZ') FMNW(KK)=REG(15)
                  IF(FUNQW(NF,I).EQ.'IT') FMNW(KK)=REG(16)
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
          EE1=FUNCW(NF,I)
          WC=EE1
          EE1=FUNQW(NF,I)
          WQ=EE1
          EE2= FUNSTR(NF,I)
          WS=EE2
          W1=FUNNW(NF,1,I)
          W2=FUNNW(NF,2,I)
          W3=FUNNW(NF,3,I)
          W4=FUNNW(NF,4,I)
          W5=FUNNW(NF,5,I)
          SB1=FUNSTA(NF,1,I)
          SB2=FUNSTA(NF,2,I)
          SC1=FUNSTA(NF,3,I)
          SC2=FUNSTA(NF,4,I)
          SQ=FUNSTA(NF,5,I)
          SST=FUNSTA(NF,6,I)
          S1=FUNSTA(NF,7,I)
          S2=FUNSTA(NF,8,I)
          S3=FUNSTA(NF,9,I)
          S4=FUNSTA(NF,10,I)
          S5=FUNSTA(NF,11,I)
          DF1=FUNSTA(NF,12,I)
          DF2=FUNSTA(NF,13,I)
          DF3=FUNSTA(NF,14,I)
          DF4=FUNSTA(NF,15,I)
          DF5=FUNSTA(NF,16,I)
          SN=FUNSTA(NF,17,I)
          STI=FUNSTA(NF,18,I)
C       FUNSTA(NF,19,I) AND FUNSTA(NF,20,I) NOT YET USED
          FUNSTA(NF,19,I)=0
          FUNSTA(NF,20,I)=0
C
C       IMPLEMENT SUBSTITUTIONS HERE
C       HANDLE CSUB
          IF(CSUB.EQ.1) THEN
              IF(.NOT.CRSUB) WC=FMWQ
              IF(CRSUB) WC=AGPREG(0)(1:8)
              IF(CRSUB) CRSUB=.FALSE.
          END IF
C       HANDLE QSUB
          IF(QSUB.EQ.1) THEN
              IF(.NOT.QRSUB) WQ=FMWQ
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
              WS=FMWS
              SST=1
          END IF
C       HANDLE NSUB
          IF(NSUB.EQ.1) THEN
              CTF=CT
              IF(CT.GT.10) THEN
                  OUTLYNE='MAXIMUM NUMBER OF CONSECUTIVE NSUBS = 10'
                  CALL SHOWIT(1)
                  OUTLYNE='HAS BEEN REACHED, FUNCTION EXECUTION ABORTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
C       PROCEED
              END IF
              DO 400 CT=1,CTF
                  CALL FUNDEF
                  IF(Q(CT).EQ.1) THEN
C       THE CURRENT NSUB REFERENCED BY J IS A SIMPLE NSUB
C                IF(INT(DABS(H(1,CT))).EQ.0) THEN
C                THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W1
                          CALL FUNX1
                          W1=PASS2
                          CALL FACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W2
                          CALL FUNX1
                          W2=PASS2
                          CALL FACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W3
                          CALL FUNX1
                          W3=PASS2
                          CALL FACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W4
                          CALL FUNX1
                          W4=PASS2
                          CALL FACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W5
                          CALL FUNX1
                          W5=PASS2
                          CALL FACC3
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
C                       FMNW(INT(DABS(H(3,CT)))) INSTEAD OF
C       JUST            H(3,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE FUNCTION USING
C       NSUB DV.
C
                      H(3,CT)=FMNW(INT(DABS(H(3,CT))))
C
C                IF(INT(DABS(H(1,CT))).EQ.0) THEN
C                THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W1
                          CALL FUNX1
                          W1=PASS2
                          CALL FACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W2
                          CALL FUNX1
                          W2=PASS2
                          CALL FACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W3
                          CALL FUNX1
                          W3=PASS2
                          CALL FACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W4
                          CALL FUNX1
                          W4=PASS2
                          CALL FACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W5
                          CALL FUNX1
                          W5=PASS2
                          CALL FACC3
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
C                       FMNW(INT(DABS(H(4,CT)))) INSTEAD OF
C       JUST            H(4,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE MACRO USING
C       NSUB DV.
C
                      H(4,CT)=FMNW(INT(DABS(H(4,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C                THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W1
                          CALL FUNX1
                          W1=PASS2
                          CALL FACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W2
                          CALL FUNX1
                          W2=PASS2
                          CALL FACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W3
                          CALL FUNX1
                          W3=PASS2
                          CALL FACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W4
                          CALL FUNX1
                          W4=PASS2
                          CALL FACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W5
                          CALL FUNX1
                          W5=PASS2
                          CALL FACC3
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
C                       FMNW(INT(DABS(H(5,CT)))) INSTEAD OF
C       JUST            H(5,CT)
C       IF THIS VALUE HAPPENS TO BE ZERO THEN A CHANGE TO SOME
C       OTHER DEFAULT VALUE MUST BE DONE INSIDE THE MACRO USING
C       NSUB DV.
C
                      H(5,CT)=FMNW(INT(DABS(H(5,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C               THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W1
                          CALL FUNX1
                          W1=PASS2
                          CALL FACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W2
                          CALL FUNX1
                          W2=PASS2
                          CALL FACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W3
                          CALL FUNX1
                          W3=PASS2
                          CALL FACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W4
                          CALL FUNX1
                          W4=PASS2
                          CALL FACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W5
                          CALL FUNX1
                          W5=PASS2
                          CALL FACC3
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
                      H(3,CT)=FMNW(INT(DABS(H(3,CT))))
                      H(4,CT)=FMNW(INT(DABS(H(4,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C               THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W1
                          CALL FUNX1
                          W1=PASS2
                          CALL FACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W2
                          CALL FUNX1
                          W2=PASS2
                          CALL FACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W3
                          CALL FUNX1
                          W3=PASS2
                          CALL FACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W4
                          CALL FUNX1
                          W4=PASS2
                          CALL FACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W5
                          CALL FUNX1
                          W5=PASS2
                          CALL FACC3
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
                      H(3,CT)=FMNW(INT(DABS(H(3,CT))))
                      H(5,CT)=FMNW(INT(DABS(H(5,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C               THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W1
                          CALL FUNX1
                          W1=PASS2
                          CALL FACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W2
                          CALL FUNX1
                          W2=PASS2
                          CALL FACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W3
                          CALL FUNX1
                          W3=PASS2
                          CALL FACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W4
                          CALL FUNX1
                          W4=PASS2
                          CALL FACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W5
                          CALL FUNX1
                          W5=PASS2
                          CALL FACC3
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
                      H(4,CT)=FMNW(INT(DABS(H(4,CT))))
                      H(5,CT)=FMNW(INT(DABS(H(5,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C               THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W1
                          CALL FUNX1
                          W1=PASS2
                          CALL FACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W2
                          CALL FUNX1
                          W2=PASS2
                          CALL FACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W3
                          CALL FUNX1
                          W3=PASS2
                          CALL FACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W4
                          CALL FUNX1
                          W4=PASS2
                          CALL FACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W5
                          CALL FUNX1
                          W5=PASS2
                          CALL FACC3
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
                      H(3,CT)=FMNW(INT(DABS(H(3,CT))))
                      H(4,CT)=FMNW(INT(DABS(H(4,CT))))
                      H(5,CT)=FMNW(INT(DABS(H(5,CT))))
C
C               IF(INT(DABS(H(1,CT))).EQ.0) THEN
C               THIS CASE HANDELED EARLIER
                      IF(INT(DABS(H(1,CT))).EQ.1) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W1
                          CALL FUNX1
                          W1=PASS2
                          CALL FACC3
                          DF1=0
                          S1=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.2) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W2
                          CALL FUNX1
                          W2=PASS2
                          CALL FACC3
                          DF2=0
                          S2=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.3) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W3
                          CALL FUNX1
                          W3=PASS2
                          CALL FACC3
                          DF3=0
                          S3=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.4) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W4
                          CALL FUNX1
                          W4=PASS2
                          CALL FACC3
                          DF4=0
                          S4=1
                          SN=1
                      END IF
                      IF(INT(DABS(H(1,CT))).EQ.5) THEN
                          IF(INT(DABS(H(1,CT))).GT.5.OR.INT(DABS(H(2,CT))).GT.5) THEN
                              OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                              CALL SHOWIT(1)
                              OUTLYNE='FUNCTION EXECUTION ABORTED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              RETURN
                          END IF
                          CALL FACC1(CT)
                          PASS2=W5
                          CALL FUNX1
                          W5=PASS2
                          CALL FACC3
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
C               ARE THERE A BRANCHING COMMANDS BRU,BPOS
C       BNEG,BZE,BRI,BRJ,BRK,BRL,BRM OR BRN  NEXT?
C     ALSO IF(X=0), IF(X>0) AND IF(X<0)
C     ALSO IF(X=Y), IF(X>Y) AND IF(X<Y)
C     AND ALSO BRDQ, BRDF1 TO BRDF5
C
          HAP1=.FALSE.
          IF(FUNCW(NF,I).EQ.'BRU') HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BPOS'.AND.
     1    REAL(REG(9)).GT.0.0) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BNEG'.AND.
     1    REAL(REG(9)).LT.0.0) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BZE'.AND.
     1    REAL(REG(9)).EQ.0.0) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRI'.AND.
     1    REAL(REG(17)).EQ.REAL(REG(18))) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRJ'.AND.
     1    REAL(REG(19)).EQ.REAL(REG(20))) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BREER') HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRK'.AND.REAL(REG(21)).EQ.REAL(REG(25)))
     1    HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRL'.AND.REAL(REG(22)).EQ.REAL(REG(26)))
     1    HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRM'.AND.REAL(REG(23)).EQ.REAL(REG(27)))
     1    HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRN'.AND.REAL(REG(24)).EQ.REAL(REG(28)))
     1    HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'IF(X>0)'.AND.REAL(REG(9)).GT.0.0) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'IF(X<0)'.AND.REAL(REG(9)).LT.0.0) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'IF(X=0)'.AND.REAL(REG(9)).EQ.0.0) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'IF(X>Y)'.AND.REAL(REG(9)).GT.REAL(REG(10)))
     1    HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'IF(X<Y)'.AND.REAL(REG(9)).LT.REAL(REG(10)))
     1    HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'IF(X=Y)'.AND.REAL(REG(9)).EQ.REAL(REG(10)))
     1    HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRDQ'.AND.FMSQ.EQ.0) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRDF1'.AND.FMDF(1).EQ.1) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRDF2'.AND.FMDF(2).EQ.1) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRDF3'.AND.FMDF(3).EQ.1) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRDF4'.AND.FMDF(4).EQ.1) HAP1=.TRUE.
          IF(FUNCW(NF,I).EQ.'BRDF5'.AND.FMDF(5).EQ.1) HAP1=.TRUE.
          IF(HAP1) THEN
              HAP1=.FALSE.
C               (YES THERE ARE AND A JUMP IS TO BE MADE)
C       WE BEGIN SEARCHING FOR THE BRANCH POINT OR WE JUMP
C       BY THE DESIGNATED NUMBER OF LINES. THE INDENTIFIER
C       OF THE BRANCH POINT TO JUMP TO IS PASSED VIA FUNQW(NF,I).
C       THE LC OR LINE COUNT IS PASSED VIA FUNNW(NF,1,I)
C       THE LC ALWAYS TAKES PRECIDENCE OVER THE IDENTIFIER.
C       IF THE IDENTIFIER IS BLANK AND THE LC IS BLANK
C       AN ERROR MESSAGE IS PRINTED AND MACRO EXECUTION
C       TERMINATES.
              IF(FUNQW(NF,I).EQ.' '.AND.FUNNW(NF,1,I).EQ.0) THEN
                  OUTLYNE='BRANCH CALL MISSING IDENTIFIER/LC'
                  CALL SHOWIT(1)
                  OUTLYNE='FUNCTION EXECUTION TERMINATING'
                  CALL SHOWIT(1)
              END IF
              IF(INT(FUNNW(NF,1,I)).NE.0) THEN
C       JUMP LINES
                  I=I+INT(FUNNW(NF,1,I))
                  GO TO 54
              ELSE
C       SEARCH FOR A BRANCH POINT
C       HERE WE SEARCH FROM THE FIRST LINE OF THE MACRO TO
C       THE END FOR THE TARGET BRANCH POINT. DO THIS WITHOUT
C       INCREMENTING I.
                  DO 53 LL=1,(FCDIR2(2,NF)-2)
                      IF(FUNCW(NF,LL).EQ.'BP'.AND.FUNQW(NF,LL).EQ.FUNQW(NF,I)) THEN
C       FOUND BRANCH POINT
                          I=LL+1
                          GO TO 54
                      END IF
 53               CONTINUE
C       IF YOU GOT HERE YOU NEVER FOUND THE BRANCH POINT
C       PRINT ERROR AND TERMINATE MACRO EXECUTION.
                  WRITE(OUTLYNE,*)'BRANCH POINT ',FUNQW(NF,I),' NOT FOUND'
                  CALL SHOWIT(1)
                  OUTLYNE='FUNCTION EXECUTION TERMINATING'
                  CALL SHOWIT(1)
              END IF
          ELSE
C WAS A BRANCHING COMMAND FOUND BUT NO JUMP WAS TO BE MADE?
              IF(FUNCW(NF,I).EQ.'BREER') THEN
                  IF(.NOT.READERR) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BPOS'.OR.FUNCW(NF,I).EQ.'IF(X>0)') THEN
                  IF(REAL(REG(9)).LE.0.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BNEG'.OR.FUNCW(NF,I).EQ.'IF(X<0)') THEN
                  IF(REAL(REG(9)).GE.0.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BZE'.OR.FUNCW(NF,I).EQ.'IF(X=0)') THEN
                  IF(REAL(REG(9)).NE.0.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'IF(X>Y)') THEN
                  IF(REAL(REG(9)).LE.REAL(REG(10))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'IF(X<Y)') THEN
                  IF(REAL(REG(9)).GE.0.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'IF(X=Y)') THEN
                  IF(REAL(REG(9)).NE.0.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRI') THEN
                  IF(REAL(REG(17)).NE.REAL(REG(18)))THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRJ') THEN
                  IF(REAL(REG(19)).NE.REAL(REG(20))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRK') THEN
                  IF(REAL(REG(21)).NE.REAL(REG(25))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRL') THEN
                  IF(REAL(REG(22)).NE.REAL(REG(26))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRM') THEN
                  IF(REAL(REG(23)).NE.REAL(REG(27))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRN') THEN
                  IF(REAL(REG(24)).NE.REAL(REG(28))) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRDQ') THEN
                  IF(FMSQ.EQ.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRDF1') THEN
                  IF(FMDF(1).EQ.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRDF2') THEN
                  IF(FMDF(2).EQ.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRDF3') THEN
                  IF(FMDF(3).EQ.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRDF4') THEN
                  IF(FMDF(4).EQ.0) THEN
                      I=I+1
                      GO TO 54
                  END IF
              END IF
              IF(FUNCW(NF,I).EQ.'BRDF5') THEN
                  IF(FMDF(5).EQ.0) THEN
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
          IF(FUNCW(NF,I).EQ.'BRANCH') THEN
              IF(INT(DABS(FUNNW(NF,1,I))).GT.5) THEN
                  OUTLYNE='NUMERIC WORD NUMBER OUT OF RANGE'
                  CALL SHOWIT(1)
                  OUTLYNE='FUNCTION EXECUTION ABORTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(FMNW(INT(DABS(FUNNW(NF,1,I)))).EQ.FUNNW(NF,2,I)) THEN
C
C               (YES THERE IS)
C       WE BEGIN SEARCHING FOR THE BRANCH POINT OR WE JUMP
C       BY THE DESIGNATED NUMBER OF LINES. THE INDENTIFIER
C       OF THE BRANCH POINT TO JUMP TO IS PASSED VIA FUNQW(NF,I).
C       THE LC OR LINE COUNT IS PASSED VIA FUNNW(NF,1,I)
C       THE LC ALWAYS TAKES PRECIDENCE OVER THE IDENTIFIER.
C       IF THE IDENTIFIER IS BLANK AND THE LC IS BLANK
C       AN ERROR MESSAGE IS PRINTED AND MACRO EXECUTION
C       TERMINATES.
                  IF(FUNQW(NF,I).EQ.' '.AND.FUNNW(NF,3,I).EQ.0) THEN
                      OUTLYNE='BRANCH CALL MISSING IDENTIFIER/LC'
                      CALL SHOWIT(1)
                      OUTLYNE='FUNCTION EXECUTION TERMINATING'
                      CALL SHOWIT(1)
                  END IF
                  IF(INT(FUNNW(NF,3,I)).NE.0) THEN
C       JUMP LINES
                      I=I+INT(FUNNW(NF,1,I))
                      GO TO 54
                  ELSE
C       SEARCH FOR A BRANCH POINT
C       HERE WE SEARCH FROM THE FIRST LINE OF THE MACRO TO
C       THE END FOR THE TARGET BRANCH POINT. DO THIS WITHOUT
C       INCREMENTING I.
                      DO 56 LL=1,(FCDIR2(2,NF)-2)
                          IF(FUNCW(NF,LL).EQ.'BP'.AND.FUNQW(NF,LL).EQ.FUNQW(NF,I)) THEN
C       FOUND BRANCH POINT
                              I=LL+1
                              GO TO 54
                          END IF
 56                   CONTINUE
C       IF YOU GOT HERE YOU NEVER FOUND THE BRANCH POINT
C       PRINT ERROR AND TERMINATE MACRO EXECUTION.
                      WRITE(OUTLYNE,*)'BRANCH POINT ',FUNQW(NF,I),' NOT FOUND'
                      CALL SHOWIT(1)
                      OUTLYNE='FUNCTION EXECUTION TERMINATING'
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
          IF(FUNCW(NF,I).EQ.'BRQ'.AND.FMWQ.EQ.(FUNSTR(NF,I)(1:8))) THEN
C
C               (YES THERE IS AND A JUMP SHOULD BE MADE)
C       WE BEGIN SEARCHING FOR THE BRANCH POINT. THERE IS NO
C       JUMP USED IN THIS COMMAND.
C       THE INDENTIFIER
C       OF THE BRANCH POINT TO JUMP TO IS PASSED VIA FUNQW(NF,I).
C       IF THE IDENTIFIER IS BLANK
C       AN ERROR MESSAGE IS PRINTED AND MACRO EXECUTION
C       TERMINATES.
              IF(FUNQW(NF,I).EQ.' ') THEN
                  OUTLYNE='BRANCH CALL MISSING IDENTIFIER'
                  CALL SHOWIT(1)
                  OUTLYNE='FUNCTION EXECUTION TERMINATING'
                  CALL SHOWIT(1)
              END IF
C       SEARCH FOR A BRANCH POINT
C       HERE WE SEARCH FROM THE FIRST LINE OF THE MACRO TO
C       THE END FOR THE TARGET BRANCH POINT. DO THIS WITHOUT
C       INCREMENTING I.
              DO 57 LL=1,(FCDIR2(2,NF)-2)
                  IF(FUNCW(NF,LL).EQ.'BP'.AND.FUNQW(NF,LL).EQ.FUNQW(NF,I)) THEN
C       FOUND BRANCH POINT
                      I=LL+1
                      GO TO 54
                  END IF
 57           CONTINUE
C       IF YOU GOT HERE YOU NEVER FOUND THE BRANCH POINT
C       PRINT ERROR AND TERMINATE MACRO EXECUTION.
              WRITE(OUTLYNE,*)'BRANCH POINT ',FUNQW(NF,I),' NOT FOUND'
              CALL SHOWIT(1)
              OUTLYNE='FUNCTION EXECUTION TERMINATING'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       IS THE BRQ COMMAND HERE WITH NO JUMP TO BE MADE?
              IF(FUNCW(NF,I).EQ.'BRQ') THEN
                  IF(FMWQ.NE.(FUNSTR(NF,I)(1:8))) THEN
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
          IF(FUNCW(NF,I).EQ.'BRT'.OR.FUNCW(NF,I).EQ.'BRF') THEN
              IF(FUNNW(NF,1,I).LT.0.0D0.OR.
     1           FUNNW(NF,2,I).LT.0.0D0.OR.
     2           FUNNW(NF,3,I).LT.0.0D0.OR.
     3           FUNNW(NF,4,I).LT.0.0D0.OR.
     4           FUNNW(NF,5,I).LT.0.0D0) THEN
                  OUTLYNE='INPUT TO BRT OR BRF MUST BE POSITIVE'
                  CALL SHOWIT(1)
                  OUTLYNE='FUNCTION EXECUTION TERMINATING'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       INPUT NOT NEGATIVE,PROCEED
          END IF
          IF(FUNCW(NF,I).EQ.'BRT'.OR.FUNCW(NF,I).EQ.'BRF') THEN
              IF(FUNCW(NF,I).EQ.'BRT'.AND.
     1           FLG(INT(FUNNW(NF,1,I))).GT.0.AND.
     2           FLG(INT(FUNNW(NF,2,I))).GT.0.AND.
     3           FLG(INT(FUNNW(NF,3,I))).GT.0.AND.
     4           FLG(INT(FUNNW(NF,4,I))).GT.0.AND.
     5           FLG(INT(FUNNW(NF,5,I))).GT.0.OR.
     6           FUNCW(NF,I).EQ.'BRF'.AND.FLG(INT(FUNNW(NF,1,I))).LT.0.OR.
     7           FUNCW(NF,I).EQ.'BRF'.AND.FLG(INT(FUNNW(NF,2,I))).LT.0.OR.
     8           FUNCW(NF,I).EQ.'BRF'.AND.FLG(INT(FUNNW(NF,3,I))).LT.0.OR.
     9           FUNCW(NF,I).EQ.'BRF'.AND.FLG(INT(FUNNW(NF,4,I))).LT.0.OR.
     1           FUNCW(NF,I).EQ.'BRF'.AND.FLG(INT(FUNNW(NF,5,I))).LT.0) THEN
C
C               (YES THERE ARE AND BRANCHING SHOULD OCCUR)
C       WE BEGIN SEARCHING FOR THE BRANCH POINT. THERE IS NO
C       JUMP USED IN THIS COMMAND.
C       THE INDENTIFIER
C       OF THE BRANCH POINT TO JUMP TO IS PASSED VIA FUNQW(NF,I).
C       IF THE IDENTIFIER IS BLANK
C       AN ERROR MESSAGE IS PRINTED AND MACRO EXECUTION
C       TERMINATES.
                  IF(FUNQW(NF,I).EQ.' ') THEN
                      OUTLYNE='BRANCH CALL MISSING IDENTIFIER'
                      CALL SHOWIT(1)
                      OUTLYNE='FUNCTION EXECUTION TERMINATING'
                      CALL SHOWIT(1)
                  END IF
C       SEARCH FOR A BRANCH POINT
C       HERE WE SEARCH FROM THE FIRST LINE OF THE MACRO TO
C       THE END FOR THE TARGET BRANCH POINT. DO THIS WITHOUT
C       INCREMENTING I.
                  DO 58 LL=1,(FCDIR2(2,NF)-2)
                      IF(FUNCW(NF,LL).EQ.'BP'.AND.FUNQW(NF,LL).EQ.FUNQW(NF,I)) THEN
C       FOUND BRANCH POINT
                          I=LL+1
                          GO TO 54
                      END IF
 58               CONTINUE
C       IF YOU GOT HERE YOU NEVER FOUND THE BRANCH POINT
C       PRINT ERROR AND TERMINATE MACRO EXECUTION.
                  WRITE(OUTLYNE,*)'BRANCH POINT ',FUNQW(NF,I),' NOT FOUND'
                  CALL SHOWIT(1)
                  OUTLYNE='FUNCTION EXECUTION TERMINATING'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       BRANCHING DID NOT OCCUR,WHY
                  IF(FUNCW(NF,I).EQ.'BRT') THEN
                      IF( FLG(INT(FUNNW(NF,1,I))).GT.0..AND.
     2                   FLG(INT(FUNNW(NF,2,I))).GT.0.AND.
     3                   FLG(INT(FUNNW(NF,3,I))).GT.0.AND.
     4                   FLG(INT(FUNNW(NF,4,I))).GT.0.AND.
     5                   FLG(INT(FUNNW(NF,5,I))).GT.0) THEN
C               ACTION ALREADY TAKEN
                      ELSE
C       NO JUMP TO OCCUR
                          I=I+1
                          GO TO 54
                      END IF
C       NOT BRT
                  END IF
                  IF(FUNCW(NF,I).EQ.'BRF') THEN
                      IF( FLG(INT(FUNNW(NF,1,I))).LT.0..AND.
     2                   FLG(INT(FUNNW(NF,2,I))).LT.0.AND.
     3                   FLG(INT(FUNNW(NF,3,I))).LT.0.AND.
     4                   FLG(INT(FUNNW(NF,4,I))).LT.0.AND.
     5                   FLG(INT(FUNNW(NF,5,I))).LT.0) THEN
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
          IF(FUNCW(NF,I).EQ.'TRACE') THEN
C
C       THE ONLY VALID QUALIFIERS ARE: ON OR OFF
C
              IF(FUNQW(NF,I).EQ.'ON'.OR.FUNQW(NF,I).EQ.'OFF') THEN
C       PROCESS
                  IF(FUNQW(NF,I).EQ.'ON') FTF=1
                  IF(FUNQW(NF,I).EQ.'OFF') FTF=0
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
          IF(FUNCW(NF,I).EQ.'SSTEP') THEN
C
C       THE ONLY VALID QUALIFIERS ARE: ON OR OFF
C
              IF(FUNQW(NF,I).EQ.'ON'.OR.FUNQW(NF,I).EQ.'OFF') THEN
C       PROCESS
                  IF(FUNQW(NF,I).EQ.'ON') SSSTEP=1
                  IF(FUNQW(NF,I).EQ.'OFF') SSSTEP=0
                  GO TO 50
              ELSE
                  OUTLYNE='INVALID QUALIFIER WORD FOR (SSTEP).'
                  CALL SHOWIT(1)
                  GO TO 50
              END IF
          END IF
C***********************************************************************
C
          IF(FTF.EQ.1) THEN
C               TRACE IS ON. DON'T PRINT MACRO COMMANDS HOWEVER
              CALL FTRACER
C
C       F41 NE 1, TRACE IS OFF, PROCEED AS NORMAL
          END IF
          IF(S1.EQ.0.AND.S2.EQ.0.AND.S3.EQ.0.AND.S4.EQ.0.AND.S5.EQ.0) THEN
              SN=0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
          ELSE
              SN=1
          END IF
C     FIXES ERROR OF NSUB INTO LIKE A "MOVE A" 11/04/96
          IF(WC.EQ.'MOVE'.AND.WQ.NE.'NW') THEN
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
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              SN=0
          END IF
          CALL CONTRO
C       RESET ALL SUB FLAGS AND COUNTERS
          CT=0
          CSUB=0
          QSUB=0
          SSUB=0
          QRSUB=.FALSE.
          CRSUB=.FALSE.
          NSUB=0
 50       I=I+1
          IF(I.LE.(FCDIR2(2,NF)-2)) GO TO 54
C       REACHED END OF FUNCTION
C
C       MACRO HAS FINISHED RUNNING. SET TRACE OFF FTF=0
          FTF=0
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
          IF(INT(SYSTEM1(91)).NE.0) F57=1
          RETURN
      END
C SUB FUNDEF.FOR
C
      SUBROUTINE FUNDEF
C
C       THIS SUBROUTINE HELPS FUNEXC BE SMALLER
C
          IMPLICIT NONE
C
          INTEGER CT
C
          COMMON/COMCT/CT
C
          REAL*8 H(1:5,0:10)
C
          COMMON/FACH/H
C
          IF(H(3,CT).EQ.0.0D0) H(3,CT)=1.0D0
          IF(H(4,CT).EQ.0.0D0) H(4,CT)=0.0D0
          IF(H(5,CT).EQ.0.0D0) H(5,CT)=1.0D0
          RETURN
      END
C SUB FTRACER.FOR
      SUBROUTINE FTRACER
C
          IMPLICIT NONE
C
C       THIS CONTROLS PRINTING OF A LINE DURING OPTIM,SPSRF FUNCTIONS
C       TRACE ON MODE
C
          INTEGER MMIJ,LENG,FTF,SSSTEP
C
          CHARACTER BN1*10,BN2*10,BN3*10,BN4*10,BN5*10,
     3    BBN1*1,BBN2*1,BBN3*1,BBN4*1,BBN5*1,
     4    LINE1*180
C
          COMMON/FTRA/FTF,SSSTEP
C
          COMMON/MIJ/MMIJ
C
          INCLUDE 'datmai.inc'
C
C       THE TRACE COMMAND ONLY SENDS OUTPUT TO THE
C       TERMINAL OR SCREEN (OUT=6).
C
C       ONLY THE CURRENT PROGRAM INSTRUCTION WHICH IS GOING TO BE
C       EXECUTED BY THE SUBROUTINE CONTROL WILL BE TRACED. TRACE
C       ON DOES NOT TRACE MACRO SPECIFIC COMMANDS.

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
C       HERE OUT ALWAYS = 6 SO WRITE COMPACT 80 COLUMN OUTPUT.
C
                  LINE1=WC//CHAR(32)//WQ//CHAR(32)//WS
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
                  CALL NTOA2(W1,W2,W3,W4,W5,BN1,BN2,BN3,BN4,BN5)
C
C       DEAL WITH THE DEFAULT VALUES
                  BBN1=','
                  BBN2=','
                  BBN3=','
                  BBN4=','
                  BBN5=','
C
                  LINE1=WC//CHAR(32)//WQ//','
                  IF(DF1.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN1
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN1//','
                  END IF
                  IF(DF2.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN2
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN2//','
                  END IF
                  IF(DF3.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN3
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN3//','
                  END IF
                  IF(DF4.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN4
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN4//','
                  END IF
                  IF(DF5.EQ.1) THEN
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
              IF(SST.EQ.1) THEN
C       THERE IS A STRING WITHOUT A QUALIFIER AND NO NUMERIC INPUT
C       PRODUCE OUTPUT
C
                  LINE1=WC//CHAR(32)//WS
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
                  CALL NTOA2(W1,W2,W3,W4,W5,BN1,BN2,BN3,BN4,BN5)
C
C       DEAL WITH THE DEFAULT VALUES
                  BBN1=','
                  BBN2=','
                  BBN3=','
                  BBN4=','
                  BBN5=','
C
C       PRODUCE OUTPUT. WRITE NUMERIC WORDS EVEN IF ALL ZERO.
                  LINE1=WC//','
                  IF(DF1.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN1
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN1//','
                  END IF
                  IF(DF2.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN2
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN2//','
                  END IF
                  IF(DF3.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN3
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN3//','
                  END IF
                  IF(DF4.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN4
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN4//','
                  END IF
                  IF(DF5.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN5
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN5//','
                  END IF
              END IF
          END IF
 40       WRITE(OUTLYNE,500)LINE1
          CALL SHOWIT(3)
 500      FORMAT(A79)
          RETURN
      END



C SUB TRACER.FOR
      SUBROUTINE TRACER
C
          IMPLICIT NONE
C
C       THIS MACRO CONTROLS PRINTING OF A LINE DURING MACRO
C       TRACE ON MODE
C
          INTEGER MMIJ,LENG
C
          CHARACTER BN1*10,BN2*10,BN3*10,BN4*10,BN5*10,
     3    BBN1*1,BBN2*1,BBN3*1,BBN4*1,BBN5*1,
     4    LINE1*180
C
          COMMON/MIJ/MMIJ
C
          INCLUDE 'datmai.inc'
C
C       THE TRACE COMMAND ONLY SENDS OUTPUT TO THE
C       TERMINAL OR SCREEN (OUT=6).
C
C       ONLY THE CURRENT PROGRAM INSTRUCTION WHICH IS GOING TO BE
C       EXECUTED BY THE SUBROUTINE CONTROL WILL BE TRACED. TRACE
C       ON DOES NOT TRACE MACRO SPECIFIC COMMANDS.

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
C       HERE OUT ALWAYS = 6 SO WRITE COMPACT 80 COLUMN OUTPUT.
C
                  LINE1=WC//CHAR(32)//WQ//CHAR(32)//WS
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
                  CALL NTOA2(W1,W2,W3,W4,W5,BN1,BN2,BN3,BN4,BN5)
C
C       DEAL WITH THE DEFAULT VALUES
                  BBN1=','
                  BBN2=','
                  BBN3=','
                  BBN4=','
                  BBN5=','
C
                  LINE1=WC//CHAR(32)//WQ//','
                  IF(DF1.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN1
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN1//','
                  END IF
                  IF(DF2.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN2
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN2//','
                  END IF
                  IF(DF3.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN3
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN3//','
                  END IF
                  IF(DF4.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN4
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN4//','
                  END IF
                  IF(DF5.EQ.1) THEN
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
              IF(SST.EQ.1) THEN
C       THERE IS A STRING WITHOUT A QUALIFIER AND NO NUMERIC INPUT
C       PRODUCE OUTPUT
C
                  LINE1=WC//CHAR(32)//WS
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
                  CALL NTOA2(W1,W2,W3,W4,W5,BN1,BN2,BN3,BN4,BN5)
C
C       DEAL WITH THE DEFAULT VALUES
                  BBN1=','
                  BBN2=','
                  BBN3=','
                  BBN4=','
                  BBN5=','
C
C       PRODUCE OUTPUT. WRITE NUMERIC WORDS EVEN IF ALL ZERO.
                  LINE1=WC//','
                  IF(DF1.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN1
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN1//','
                  END IF
                  IF(DF2.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN2
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN2//','
                  END IF
                  IF(DF3.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN3
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN3//','
                  END IF
                  IF(DF4.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN4
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN4//','
                  END IF
                  IF(DF5.EQ.1) THEN
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BBN5
                  ELSE
                      CALL LTH80(LINE1,LENG)
                      LINE1=LINE1(1:LENG)//BN5//','
                  END IF
              END IF
          END IF
 40       WRITE(OUTLYNE,500)LINE1
          CALL SHOWIT(3)
 500      FORMAT(A79)
          RETURN
      END
C SUB FACC1.FOR
      SUBROUTINE FACC1(CT)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE MAKES ACCSUB AVALIABLE TO NSUB.
C       IT IS THE LOADER.
C       THE SAVER IS SUBROUTINE ACC2
C
          CHARACTER ACCWRD*8
C
          INTEGER ACCSUB,ACCCNT,CT
C
          REAL*8 PASS1,REG9(1:10)
C
          COMMON/REGINALD/REG9
C
          COMMON/COMACC1/PASS1
C
          COMMON/ACCSB/ACCWRD
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          INCLUDE 'datmai.inc'
C
          IF(ACCSUB.EQ.1) THEN
              IF(ACCWRD.EQ.'A') PASS1=REG(1)
              IF(ACCWRD.EQ.'B') PASS1=REG(2)
              IF(ACCWRD.EQ.'C') PASS1=REG(3)
              IF(ACCWRD.EQ.'D') PASS1=REG(4)
              IF(ACCWRD.EQ.'E') PASS1=REG(5)
              IF(ACCWRD.EQ.'F') PASS1=REG(6)
              IF(ACCWRD.EQ.'G') PASS1=REG(7)
              IF(ACCWRD.EQ.'H') PASS1=REG(8)
              IF(ACCWRD.EQ.'X') PASS1=REG(9)
              IF(ACCWRD.EQ.'Y') PASS1=REG(10)
              IF(ACCWRD.EQ.'Z') PASS1=REG(11)
              IF(ACCWRD.EQ.'T') PASS1=REG(12)
              IF(ACCWRD.EQ.'IX') PASS1=REG(13)
              IF(ACCWRD.EQ.'IY') PASS1=REG(14)
              IF(ACCWRD.EQ.'IZ') PASS1=REG(15)
              IF(ACCWRD.EQ.'IT') PASS1=REG(16)
          ELSE
              PASS1=REG9(CT)
          END IF
          RETURN
      END
C SUB FACC2.FOR
      SUBROUTINE FACC2
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE MAKES ACCSUB AVALIABLE TO NSUB.
C       IT IS THE SAVER
C       THE LOADER IS SUBROUTINE ACC1
C
          CHARACTER ACCWRD*8
C
          INTEGER ACCSUB,ACCCNT
C
          REAL*8 PASS2
C
          COMMON/COMACC2/PASS2
C
          COMMON/ACCSB/ACCWRD
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          INCLUDE 'datmai.inc'
C
          IF(ACCSUB.EQ.1) THEN
              IF(ACCWRD.EQ.'A')  REG(1)=PASS2
              IF(ACCWRD.EQ.'B')  REG(2)=PASS2
              IF(ACCWRD.EQ.'C')  REG(3)=PASS2
              IF(ACCWRD.EQ.'D')  REG(4)=PASS2
              IF(ACCWRD.EQ.'E')  REG(5)=PASS2
              IF(ACCWRD.EQ.'F')  REG(6)=PASS2
              IF(ACCWRD.EQ.'G')  REG(7)=PASS2
              IF(ACCWRD.EQ.'H')  REG(8)=PASS2
              IF(ACCWRD.EQ.'X') THEN
                  REG(40)=REG(9)
                  REG(9)=PASS2
              ELSE
              END IF
              IF(ACCWRD.EQ.'Y')  REG(10)=PASS2
              IF(ACCWRD.EQ.'Z')  REG(11)=PASS2
              IF(ACCWRD.EQ.'T')  REG(12)=PASS2
              IF(ACCWRD.EQ.'IX') THEN
                  REG(30)=REG(13)
                  REG(13)=PASS2
              ELSE
              END IF
              IF(ACCWRD.EQ.'IY') REG(14)=PASS2
              IF(ACCWRD.EQ.'IZ') REG(15)=PASS2
              IF(ACCWRD.EQ.'IT') REG(16)=PASS2
              ACCCNT=ACCCNT-1
              IF(ACCCNT.EQ.0) ACCSUB=0
              RETURN
          ELSE
              REG(40)=REG(9)
              REG(9)=PASS2
          END IF
          RETURN
      END
C SUB FACC3.FOR
      SUBROUTINE FACC3
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE MAKES ACCSUB AVALIABLE TO NSUB.
C       THIS IS A CLOSER
C       THE LOADER IS SUBROUTINE ACC1
C       THE SAVER IS SUBROUTINE ACC2
C
          CHARACTER ACCWRD*8
C
          INTEGER ACCSUB,ACCCNT
C
          COMMON/ACCSB/ACCWRD
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          INCLUDE 'datmai.inc'
C
          IF(ACCSUB.EQ.1) THEN
              ACCCNT=ACCCNT-1
              IF(ACCCNT.EQ.0) ACCSUB=0
          ELSE
          END IF
          RETURN
      END
