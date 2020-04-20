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

C       FIRST SET OF SPECT ROUTINES

C SUB PFILE.FOR
      SUBROUTINE PFILE
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS FILING THE CONTENTS OF THE
C       CUMULATIVE MEMORY AREA TO A SPECT DISK FILE
C
C                       DEFINE VARIABLES
C
          CHARACTER NM*8,FN*10,DDATE*10,
     1    TTIME*8,TTTIM*8,DDDAT*10
C
          COMMON/STRNGR/DDDAT,TTTIM,NM,FN
C
          INTEGER POINTS,OCC,NTOTAL,NF,I,J,II
C
          REAL*8
     1    DATA1,DATA2,LAMB1,LAMB2
C
          REAL*8 CUMULT(1:1001,1:3)
C
          LOGICAL EXISJK
C
          COMMON/CUM/CUMULT
C
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
          INCLUDE 'datmai.inc'
C
C       INITIALIZE II TO ZERO
          II=0
C
C       TABLE DATA IN BINARY FORMAT. TRA.DAT IS UNIT 35
C
C       DATA STORED IN TRA.DAT IS:
C               FILE NAME (WQ) CHARACTER *8
C               DATA1 STARTING WAVELENGTH (DOUBLE PRES.)
C               DATA2 ENDIG WAVLENGTH (DOUBLE PRES.)
C               POINTS - NUMBER OF DATA POINTS (INTEGER)
C               OCC- OCCUPANCY FLAG, 0=EMPTY, 1- FULL (INTEGER)
C               DDATE- CALENDER DATE FILE WAS STORED
C               TTIME-TIME FILED
C       WE MUST SEARCH FOR A FILE TO OVERWRITE
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1    FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
          DO 9 I=1,200
              READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              DDDAT=DDATE
              TTTIM=TTIME
              IF(NM.EQ.WQ) THEN
C       FOUND THE EARLIER VERSION OF THE TABLE FILE NAMED "WQ"
C       OVERWRITE IT.
                  II=I
                  GO TO 11
              END IF
C
 9        CONTINUE
C
C       NO OVERWRITE TO DO, FIND A BLANK SPOT TO FILE IN
C
          DO 10 I=1,200
              READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              DDDAT=DDATE
              TTTIM=TTIME
              IF(OCC.EQ.0) THEN
C       FOUND AN EMPTY SLOT
                  II=I
                  GO TO 11
              END IF
C
C       CHECK IF NM=WQ
C
 10       CONTINUE
C       IF YOU GOT HERE, LIBRARY WAS FULL
          WRITE(OUTLYNE,*)
     1    'SPECT DISK FILE WAS FULL, CUMULATIVE DATA NOT FILED'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
 11       CONTINUE
C       PROCEED WITH STORAGE
C       FILE THE DIRECTORY DATA IN TRA.DAT
          DATA1=LAMB1
          DATA2=LAMB2
          POINTS=NTOTAL
          OCC=1
          CALL MYDATE(DDATE)
          CALL MYTIME(TTIME)
          DDDAT=DDATE
          TTTIM=TTIME
          WRITE(UNIT=35,REC=II) WQ,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
          CALL CLOSE_FILE(35,1)
          NF=II
          CALL TRAFIL(NF,FN)
C
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1    FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
          CALL CLOSE_FILE(35,0)
C       OPEN AND WRITE TO FILE
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1    FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
C       NOW OUTPUT TABLE DATA TO TRAXXX.DAT
C
          DO 210 J=1,NTOTAL
              WRITE(UNIT=35,REC=J) CUMULT(J,1),
     1        CUMULT(J,2),CUMULT(J,3)
 210      CONTINUE
C
          CALL CLOSE_FILE(35,1)
          WRITE(OUTLYNE,*)
     1     'CUMULATIVE DATA STORED IN SPECT DISK FILE NO. ',II
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'AS FILE ( ',WQ,' )'
          CALL SHOWIT(1)
          RETURN
C
      END
C SUB TRFLEX.FOR
      SUBROUTINE TRFLEX
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE ERASES ALL EXISTING TRANSMISSION FILES
C       TRAXXX.FOR
C
          LOGICAL LXIST
C
          INTEGER NF
C
          CHARACTER FN*10
C
          INCLUDE 'datmai.inc'
C
          DO NF=1,999
              CALL TRAFIL(NF,FN)
              LXIST=.FALSE.
              INQUIRE(FILE=LIBTRA//FN,EXIST=LXIST)
              IF(LXIST) THEN
                  OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1            FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
                  CALL CLOSE_FILE(35,1)
              ELSE
C       CONTINUE SEARCH
              END IF
          END DO
          RETURN
      END
C SUB TRAFIL.FOR
      SUBROUTINE TRAFIL(N,FN)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS USED TO DETERMIN THE CORRECT FILE
C       NAME GIVEN A FILE NUMBER( FOR TRANSMISSION FILES USED
C       IN SPECT)
C
          INTEGER N
C
          CHARACTER FN*10,AN*3
C
          CALL CCOONN(N,AN)
C
          IF(N.GT.0.AND.N.LE.9) FN='TRA00'//AN(3:3)//'.DAT'
          IF(N.GT.9.AND.N.LE.99) FN='TRA0'//AN(2:3)//'.DAT'
          IF(N.GT.99.AND.N.LE.999) FN='TRA'//AN(1:3)//'.DAT'
          RETURN
      END
C SUB CCOONN.FOR
      SUBROUTINE CCOONN(N,AN)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT INTEGER NUMERIC WORDS
C       TO CHARACTER VARIABLES
C
          INTEGER N
C
          CHARACTER B*20,AN*3

          WRITE(B,100) N
          READ(B,200) AN
 100      FORMAT(I3)
 200      FORMAT(A3)
          RETURN
      END
C SUB TFILE.FOR
      SUBROUTINE TFILE(TNAME)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS FILING TABLE DATA TO DISK
C
C                       DEFINE VARIABLES
C
          CHARACTER TNAME*8,NM*8,FN*10,DDATE*10,
     1    TTIME*8,TTTIM*8,DDDAT*10
C
          LOGICAL EXISJK
C
          COMMON/STRNGR/DDDAT,TTTIM,NM,FN
C
          INTEGER CTAB,POINTS,OCC,NTOTAL,I,J,II
C
          REAL*8 TABLE(1:1001,1:3),
     1    DATA1,DATA2,LAMB1,LAMB2
C
          INTEGER NF
C
          COMMON/TABL/TABLE,CTAB
C
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
          INCLUDE 'datmai.inc'
C
C       INITIALIZE II TO ZERO
          II=0
C
C       IF NUMBER OF POINTS IS LESS THAN 2, DON'T FILE THE
C       FILE, GIVE MESSAGE AND RETURN.
C
          IF(CTAB.LT.2) THEN
              WRITE(OUTLYNE,*)'LEAVING THE "TABLE" INPUT MODE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'NO TABLE DATA EXISTED TO FILE AND NO FILE WAS CREATED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       TABLE DATA IN BINARY FORMAT. TRA.DAT IS UNIT 35
C
C       DATA STORED IN TRA.DAT IS:
C               FILE NAME (TNAME) CHARACTER *8
C               DATA1 STARTING WAVELENGTH (DOUBLE PRES.)
C               DATA2 ENDIG WAVLENGTH (DOUBLE PRES.)
C               POINTS - NUMBER OF DATA POINTS (INTEGER)
C               OCC- OCCUPANCY FLAG, 0=EMPTY, 1- FULL (INTEGER)
C               DDATE- CALENDER DATE FILE WAS STORED
C               TTIME-TIME FILED
C       WE MUST SEARCH FOR A FILE TO OVERWRITE
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1    FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
          DO 9 I=1,999
              READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              DDDAT=DDATE
              TTTIM=TTIME
              IF(NM.EQ.TNAME) THEN
C       FOUND THE EARLIER VERSION OF THE TABLE FILE NAMED "TNAME"
C       OVERWRITE IT.
                  II=I
                  GO TO 11
              END IF
C
 9        CONTINUE
C
C       NO OVERWRITE TO DO, FIND A BLANK SPOT TO FILE IN
C
          DO 10 I=1,999
              READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              DDDAT=DDATE
              TTTIM=TTIME
              IF(OCC.EQ.0) THEN
C       FOUND AN EMPTY SLOT
                  II=I
                  GO TO 11
              END IF
C
C       CHECK IF NM=TNAME
C
 10       CONTINUE
C       IF YOU GOT HERE, LIBRARY WAS FULL
          WRITE(OUTLYNE,*)
     1    'SPECT DISK FILE WAS FULL, TABLE DATA NOT FILED'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
 11       CONTINUE
C       PROCEED WITH STORAGE
C       FILE THE DIRECTORY DATA IN TRA.DAT
          DATA1=LAMB1
          DATA2=LAMB2
          IF(TABLE(1,2).GT.0.0D0) DATA1=TABLE(1,2)
          IF(TABLE(CTAB,2).GT.0.0D0) DATA2=TABLE(CTAB,2)
          POINTS=CTAB
          OCC=1
          CALL MYDATE(DDATE)
          CALL MYTIME(TTIME)
          DDDAT=DDATE
          TTTIM=TTIME
          WRITE(UNIT=35,REC=II) TNAME,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
          CALL CLOSE_FILE(35,1)
C
          NF=II
          CALL TRAFIL(NF,FN)
C
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1    FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
          CALL CLOSE_FILE(35,0)
C       OPEN AND WRITE TO FILE
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1    FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
C       NOW OUTPUT TABLE DATA TO TRAXXX.DAT
C
          DO 210 J=1,POINTS
              WRITE(UNIT=35,REC=J) TABLE(J,1),TABLE(J,2),TABLE(J,3)
 210      CONTINUE
C
          CALL CLOSE_FILE(35,1)
          WRITE(OUTLYNE,*)
     1     'TABLE ',TNAME,' STORED IN SPECT DISK FILE NO. ',II
          CALL SHOWIT(0)
          RETURN
C
      END
C SUB TABDIR.FOR
      SUBROUTINE TABDIR
C
          IMPLICIT NONE
C
          INTEGER I
C
C       THIS SUBROUTINE HANDELS PRINTING THE DIRECTORY OF
C       SPECT DISK FILES
C
          CHARACTER NM*8,DDATE*10,TTIME*8,FN*10,TTTIM*8,DDDAT*10
C
          LOGICAL EXISJK
C
          COMMON/STRNGR/DDDAT,TTTIM,NM,FN
C
          INTEGER POINTS,OCC,OTPUT
C
          REAL*8 DATA1,DATA2
C
          INCLUDE 'datmai.inc'
C
C       TABLE DATA IN BINARY FORMAT. TRA.DAT IS UNIT 35
C
C       DATA STORED IN TRA.DAT IS:
C               FILE NAME (TNAME) CHARACTER *8
C               DATA1 STARTING WAVELENGTH (DOUBLE PRES.)
C               DATA2 ENDIG WAVLENGTH (DOUBLE PRES.)
C               POINTS - NUMBER OF DATA POINTS (INTEGER)
C               OCC- OCCUPANCY FLAG, 0=EMPTY, 1- FULL (INTEGER)
C               DDATE-DATE FILED
C               TTIME=TIME FILED
          IF(DF1.EQ.1) THEN
C       LIST AFF FILES
C
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1        FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
              OTPUT=0
              DO 10 I=1,999
                  READ(UNIT=35,REC=I)
     1            NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
                  DDDAT=DDATE
                  TTTIM=TTIME
                  IF(OCC.EQ.1) THEN
                      OTPUT=1
                      GO TO 12
                  END IF
C
 10           CONTINUE
C
              IF(OTPUT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'SPECT DISK FILE IS EMPTY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO DIRECTORY PRINTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
 12           CONTINUE
              WRITE(OUTLYNE,100)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,106)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,102)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,101)
              CALL SHOWIT(0)
              DO 11 I=1,999
                  READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
                  DDDAT=DDATE
                  TTTIM=TTIME
                  IF(OCC.EQ.1) THEN
                      WRITE(OUTLYNE,103)I,NM,DATA1,DATA2,POINTS,DDATE,TTIME
                      CALL SHOWIT(0)
                  END IF
C
C
 11           CONTINUE
              CALL CLOSE_FILE(35,1)
          ELSE
C       JUST LIST ONE FILE IF IT EXISTS
C       ***************************************************************
              EXISJK=.FALSE.
              INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
              IF(.NOT.EXISJK) THEN
                  WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       ***************************************************************
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1        FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
              I=INT(W1)
              READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              DDDAT=DDATE
              TTTIM=TTIME
              CALL CLOSE_FILE(35,1)
              IF(OCC.EQ.1) THEN
C       OUTPUT
                  WRITE(OUTLYNE,105) INT(W1),NM
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,106)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,102)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,101)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,103)I,NM,DATA1,DATA2,POINTS,DDATE,TTIME
                  CALL SHOWIT(0)
                  RETURN
              ELSE
C       PRINT MESSAGE
                  WRITE(OUTLYNE,*)'FILE # ',INT(W1),' DOES NOT EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              RETURN
          END IF
 100      FORMAT('DIRECTORY OF SPECT FILES STORED ON DISK')
 105      FORMAT('DIRECTORY FOR SPECT FILE ',I3,1X,A8)
 101      FORMAT(1X)
 102      FORMAT(
     1    '           ',4X,'(MICRONS)',4X,'(MICRONS)')
 106      FORMAT(
     1    'FILE #/NAME',4X,'LAMBDA 1',5X,'LAMBDA 2',3X,
     2    'NO. OF POINTS',2X,'DATE/TIME FILED')
 103      FORMAT(I3,1X,A8,1X,G12.4,1X,G12.4,5X,I4,7X,A9,'/',A8)
          RETURN
C
      END
C SUB SSTART.FOR
      SUBROUTINE SSTART
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO ENTER THE SPECT LEVEL COMMAND
C       "START"
C
          INTEGER NTOTAL,I
C
          REAL*8 LAMB1,LAMB2,
     1    CUMULT(1:1001,1:3)
C
          COMMON/CUM/CUMULT
C
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
          INCLUDE 'datmai.inc'
C
          IF(NTOTAL.LT.0) NTOTAL=1001
          DO 5 I=1,NTOTAL
              CUMULT(I,1)=DBLE(I)
              IF(I.EQ.1) CUMULT(I,2)=LAMB1
              IF(I.NE.1) CUMULT(I,2)=CUMULT((I-1),2)+
     2        ((LAMB2-LAMB1)/(DBLE(NTOTAL-1)))
 5        CONTINUE
C       DEFINE AND SET THE CUMULATIVE AREA OF MEMORY
          DO 10 I=1,NTOTAL
              CUMULT(I,3)=1.0D0
 10       CONTINUE
C
C                       RETURN
      END
C SUB SPECTR.FOR
      SUBROUTINE SPECTR
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO ENTER THE SPECT LEVEL
C       FROM THE CMD LEVEL
C
          INTEGER NTOTAL
C
          REAL*8 LAMB1,LAMB2,
     1    CUMULT(1:1001,1:3)
C
          COMMON/CUM/CUMULT
C
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
          INCLUDE 'datmai.inc'
C
C       SPECT TAKES NO INPUT
          IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"SPECTR" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C                       SET F17=1, LEAVE F1=1
C       SET F18 AND F19 TO 0
C       F18 TRACKS TABLE INPUT LEVEL
C       F19 TRACKS EXISTENCE OF TABLE DATA
          IF(F1.NE.1) F1=1
          F18=0
          F19=0
          NTOTAL=-99
          LAMB1=0.05
          LAMB2=20.0
          F17=1
          CALL SSTART
C                       RETURN
      END
C SUB PWORK.FOR
      SUBROUTINE PWORK
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO ENTER THE SPECT LEVEL
C       COMMAND "WORK" AT THE SPECTR LEVEL
C
          INTEGER NTOTAL,COUNT,I
C
          REAL*8 LAMB1,LAMB2,WORK(1:1001,1:3)
C
          COMMON/WRK/WORK
C
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
          INCLUDE 'datmai.inc'
          WRITE(OUTLYNE,200)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,204) LAMB1
          CALL SHOWIT(0)
          WRITE(OUTLYNE,205) LAMB2
          CALL SHOWIT(0)
          WRITE(OUTLYNE,206) NTOTAL
          CALL SHOWIT(0)
          WRITE(OUTLYNE,201)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,202)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,203)
          CALL SHOWIT(0)
C       PROCEED
          DO 10 I=1,NTOTAL
              COUNT=INT(WORK(I,1))
              WRITE(OUTLYNE,100) COUNT,WORK(I,2),WORK(I,3)
              CALL SHOWIT(0)
 10       CONTINUE
 100      FORMAT(5X,I3,8X,G18.10,5X,G18.10)
 200      FORMAT(
     1    'LISTING OF THE "SPECT" WORK MEMORY AREA')
 201      FORMAT(1X)
 202      FORMAT('ENTRY NUMBER',4X,'WAVLENGTH-(MICRONS)',6X,
     1    'FUNCTION VALUE')
 203      FORMAT(
     1    '--------------------------------------------------------')
 204      FORMAT('LAMBDA(1) = ',G18.10)
 205      FORMAT('LAMBDA(2) = ',G18.10)
 206      FORMAT('TOTAL NUMBER OF DATA POINTS = ',I3)
C                       RETURN
      END
C SUB PTABLE.FOR
      SUBROUTINE PTABLE
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO ENTER THE SPECT LEVEL
C       COMMAND "LIST" AT THE SPECTR LEVEL
C
          CHARACTER TNAME*8
C
          INTEGER CTAB,COUNT,I
C
          REAL*8 LAMB1,LAMB2,TABLE(1:1001,1:3)
C
          COMMON/TABL/TABLE,CTAB
C
          COMMON/CURNAM/TNAME
C
          INCLUDE 'datmai.inc'
C
          LAMB1=TABLE(1,2)
          LAMB2=TABLE(CTAB,2)
          WRITE(OUTLYNE,200) TNAME
          CALL SHOWIT(0)
          WRITE(OUTLYNE,204) LAMB1
          CALL SHOWIT(0)
          WRITE(OUTLYNE,205) LAMB2
          CALL SHOWIT(0)
          WRITE(OUTLYNE,206) CTAB
          CALL SHOWIT(0)
          WRITE(OUTLYNE,201)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,202)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,203)
          CALL SHOWIT(0)
C       PROCEED
          DO 10 I=1,CTAB
              COUNT=INT(TABLE(I,1))
              WRITE(OUTLYNE,100) COUNT,TABLE(I,2),TABLE(I,3)
              CALL SHOWIT(0)
 10       CONTINUE
 100      FORMAT(5X,I3,8X,G18.10,5X,G18.10)
 200      FORMAT(
     1    'LISTING OF THE "SPECT" TABLE MEMORY AREA FOR FILE (',
     1     A8,' )')
 201      FORMAT(1X)
 202      FORMAT('ENTRY NUMBER',4X,'WAVLENGTH-(MICRONS)',6X,
     1    'FUNCTION VALUE')
 203      FORMAT(
     1    '--------------------------------------------------------')
 204      FORMAT(
     1    'SHORTEST WAVELENGTH VALUE CURRENTLY IN THE FILE = '
     2    ,G18.10,' MICRONS')
 205      FORMAT(
     1    'LONGEST  WAVELENGTH VALUE CURRENTLY IN THE FILE = '
     2    ,G18.10,' MICRONS')
 206      FORMAT('TOTAL NUMBER OF DATA POINTS = ',I3)
C                       RETURN
      END
C SUB BLCKBD.FOR
      SUBROUTINE BLCKBD
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE "BLACKBDY" SPECT COMMAND.
C       BLACK BODY RADIANT SPECTRAL EMMITTANCE OR
C       BLACK BODY RADIANT SPECTRAL PHOTON EMMITTANCE VALUES
C       ARE CALCULATED FOR THE VAVELENGTH VALUES IN THE CUMULATIVE
C       AREA AND THEN MULTIPLIED INTO THE FUNCTIONAL
C       AREA IN CUMULATIVE.
C
C                       DEFINE VARIABLES
C
          INTEGER I
C
          REAL*8 CUMULT(1:1001,1:3),LAMB1,LAMB2,VAL,
     1    TEST
C
          INTEGER NTOTAL
C
          COMMON/CUM/CUMULT
C
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
          INCLUDE 'datmai.inc'
C
          IF(WQ.EQ.'SRE') THEN
              DO 10 I=1,NTOTAL
                  IF(CUMULT(I,2).EQ.0.0D0) THEN
                      CUMULT(I,3)=0.0D0
                      GO TO 10
                  ELSE
                  END IF
C       CALCULATE BB VALUE
                  TEST=1.43879D4/(CUMULT(I,2)*W1)
                  IF(TEST.GE.10.0D0) THEN
                      CUMULT(I,3)=CUMULT(I,3)*(DEXP(-TEST))*(3.7415D4)
                      CUMULT(I,3)=CUMULT(I,3)/CUMULT(I,2)
                      CUMULT(I,3)=CUMULT(I,3)/CUMULT(I,2)
                      CUMULT(I,3)=CUMULT(I,3)/CUMULT(I,2)
                      CUMULT(I,3)=CUMULT(I,3)/CUMULT(I,2)
                      CUMULT(I,3)=CUMULT(I,3)/CUMULT(I,2)
                  ELSE
                      CUMULT(I,3)=CUMULT(I,3)*(3.7415D4/(CUMULT(I,2)**5))*
     1                (1.0D0/(DEXP(1.43879D4/(CUMULT(I,2)*W1))-1.0D0))
                  END IF

 10           CONTINUE
C       CUMULATIVE AREA HAS BEEM MULTIPLIED BY THE APPRORIATE
C       BLAKBODY VALUES.
C       PRINT MESSAGE AND RETURN
              WRITE(OUTLYNE,*)'FOR T = ',W1,' DEGREES KELVIN'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'BLACK BODY SPECTRAL RADIANT EMMITTANCE VALUES HAVE BEEN'
              CALL SHOWIT(1)
              OUTLYNE='MULTIPLIED INTO THE CUMULATIVE AREA'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'UNITS ARE: Watt/(cm2-micron)'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WQ.EQ.'SRPE') THEN
              DO 20 I=1,NTOTAL
                  IF(CUMULT(I,2).EQ.0.0D0) THEN
                      CUMULT(I,3)=0.0D0
                      GO TO 20
                  ELSE
                  END IF
                  TEST=1.43879D4/(CUMULT(I,2)*W1)
                  IF(TEST.GE.10.0D0) THEN
                      VAL=(DEXP(-TEST))*(1.88356D3)
                      VAL=VAL/CUMULT(I,2)
                      VAL=VAL/CUMULT(I,2)
                      VAL=VAL/CUMULT(I,2)
                      VAL=VAL/CUMULT(I,2)
                  ELSE
                      VAL=(1.88356D3/(CUMULT(I,2)**4))*
     1                (1.0D0/(DEXP(1.43879D4/(CUMULT(I,2)*W1))-1.0D0))
                  END IF
                  CUMULT(I,3)=CUMULT(I,3)*VAL*(1D20)

 20           CONTINUE
C       CUMULATIVE AREA HAS BEEM MULTIPLIED BY THE APPRORIATE
C       BLAKBODY VALUES.
C       PRINT MESSAGE AND RETURN
              WRITE(OUTLYNE,*)'FOR T = ',W1,' DEGREES KELVIN'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'BLACK BODY SPECTRAL RADIANT PHOTON EMMITTANCE VALUES HAVE BEEN'
              CALL SHOWIT(1)
              OUTLYNE='MULTIPLIED INTO THE CUMULATIVE AREA'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'UNITS ARE: Photon/(sec-cm2-micron)'
              CALL SHOWIT(1)
              RETURN
          END IF
      END
C SUB SINSERT.FOR
      SUBROUTINE SINSERT
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS INSERTING OF A RECORD INTO
C       SPECT DISK FILE WQ
C
          INTEGER III,I,J
C
          CHARACTER DDATE*10,TTIME*8,FN*10,NM*8,TTTIM*8,DDDAT*10
C
          INTEGER POINTS,OCC,CTAB,NF
C
          LOGICAL EXISJK
C
          REAL*8 DATA1,DATA2,TABLE(1:1001,1:3),
     1    LAMB1,LAMB2
C
          COMMON/STRNGR/DDDAT,TTTIM,NM,FN
C
          INTEGER NTOTAL
C
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
          COMMON/TABL/TABLE,CTAB
C
          INCLUDE 'datmai.inc'
C
          III=0
C
C       TABLE DATA IN BINARY FORMAT. TRA.DAT IS UNIT 35
C
C       DATA STORED IN TRA.DAT IS:
C               FILE NAME (TNAME) CHARACTER *8
C               DATA1 STARTING WAVELENGTH (DOUBLE PRES.)
C               DATA2 ENDIG WAVLENGTH (DOUBLE PRES.)
C               POINTS - NUMBER OF DATA POINTS (INTEGER)
C               OCC- OCCUPANCY FLAG, 0=EMPTY, 1- FULL (INTEGER)
C               DDATE-DATE FILED
C               TTIME=TIME FILED
C       FIND THE DESIRED FILE
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1    FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
          DO 200 I=1,999
              READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              DDDAT=DDATE
              TTTIM=TTIME
              IF(NM.EQ.WQ) THEN
C       FOUND FILE
                  GO TO 300
              END IF
 200      CONTINUE
C       IF YOU GOT HERE, FILE DID NOT EXIST
          WRITE(OUTLYNE,*)'FILE ',WQ,' DOES NOT EXIST'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL CLOSE_FILE(35,1)
          CALL MACFAL
          RETURN
 300      CONTINUE
C       LOAD FILE INTO TABLE ARRAY
C
          NF=I
          CALL TRAFIL(NF,FN)
C
C       OPEN AND READ FROM FILE
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1    FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
          DO 210 J=1,POINTS
              READ(UNIT=35,REC=J) TABLE(J,1),TABLE(J,2),TABLE(J,3)
 210      CONTINUE
          CALL CLOSE_FILE(35,1)
          IF(W1.LT.LAMB1.OR.W1.GT.LAMB2) THEN
              WRITE(OUTLYNE,*)
     1        'NEW WAVELENGTH IS BEYOND CURRENTLY DEFINED RANGE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       FIND A SPOT FOR THE NEW ENTRY
C

          DO 350 I=1,(POINTS-1)
              IF(TABLE(I,2).EQ.W1.OR.TABLE((I+1),2).EQ.W1) THEN
                  WRITE(OUTLYNE,*)
     1            'NEW WAVELENGTH ENTRY EQUALS AN EXISTING WAVELENGTH VALUE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(TABLE(I,2).LT.W1.AND.TABLE((I+1),2).GT.W1) THEN
C       NEW ENTRY GOES BETWEEN THE TWO TESTED RECORDS.
                  III=I+1
                  GO TO 360
              END IF
              IF(W1.LT.TABLE(1,2)) THEN
C       NEW ENTRY GOES AT BEGINNING OF LIST.
                  III=1
                  GO TO 360
              END IF
              IF(W1.GT.TABLE(POINTS,2)) THEN
C       NEW ENTRY GOES AT END OF LIST.
                  III=POINTS+1
                  GO TO 360
              END IF
 350      CONTINUE
          WRITE(OUTLYNE,*)'NO INSERTION POINT FOUND'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
 360      CONTINUE
C
C       INCREMENT TOTAL NUMBER OF POINTS
C
          POINTS=POINTS+1
C
C       PERFORM INSERTION HERE
C
C       MOVE EVERYTHING DOWN ONE
C
          DO 370 I=POINTS,(III+1),-1
              TABLE(I,1)=TABLE((I-1),1)+1.0D0
              TABLE(I,2)=TABLE((I-1),2)
              TABLE(I,3)=TABLE((I-1),3)
 370      CONTINUE
C
C       WRITE IN THE NEW DATA ITEM
C
          TABLE(III,1)=DBLE(III)
          TABLE(III,2)=W1
          TABLE(III,3)=W2
C
C       RESET PARAMETERS FOR DIRECTORY
C
          CTAB=POINTS
          CALL MYTIME(TTIME)
          CALL MYDATE(DDATE)
          DDDAT=DDATE
          TTTIM=TTIME
          IF(POINTS.EQ.0) THEN
              DATA1=0.0D0
              DATA2=0.0D0
              OCC=0
              DDATE='         '
              TTIME='        '
              DDDAT='         '
              TTTIM='        '
              NM='        '
          END IF
C
C       FILE THE FILE IN THE APPROPRIATE SPOT
C       OPEN AND WRITE TO FILE
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1    FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
          WRITE(UNIT=35,REC=NF) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
          CALL CLOSE_FILE(35,1)
          IF(OCC.EQ.0) THEN
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1        FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
              CALL CLOSE_FILE(35,0)
          END IF
C
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1    FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
          F19=1
          DO 220 J=1,POINTS
              WRITE(UNIT=35,REC=J) TABLE(J,1),TABLE(J,2),TABLE(J,3)
 220      CONTINUE
          CALL CLOSE_FILE(35,1)
C       PRINT MESSAGE TO THAT EFFECT
          WRITE(OUTLYNE,*)'ENTRY INSERTED INTO FILE ',WQ
          CALL SHOWIT(1)
          IF(OCC.EQ.0) THEN
              WRITE(OUTLYNE,*)'FILE ',WQ,' IS EMPTY'
              CALL SHOWIT(1)
              F19=0
          END IF
          RETURN
      END
C SUB INTSMP.FOR
      SUBROUTINE INTSMP
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED INTEGRATE THE CUMULATIVE
C       MEMORY AREA AND PLACE THE VALUE OF THE INTEGRAL
C       IN THE X- REGISTER
C
          INTEGER NTOTAL,I
C
          REAL*8 VALUE,CUMULT(1:1001,1:3),LAMB1,LAMB2,
     1    IVAL
C
          COMMON/CUM/CUMULT
C
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
          INCLUDE 'datmai.inc'
C
C       THERE ARE NTOTAL POINTS IN THE CUMULATIVE AREA INLUDING
C       THE END POINTS. THEY ARE EQUALLY SPACED. EACH POINT HAS A
C       FUNCTIONAL VALUE ASSIGNED TO IT.
C
C       THE VAULE OF EACH INTERVAL IS EXACTLY
C
C               (LAMB2-LAMB1)/DBLE(NTOTAL-1)=IVAL
C
          IVAL=(LAMB2-LAMB1)/(DBLE(NTOTAL-1))
          VALUE=0.0D0
          DO 10 I=1,(NTOTAL-1)
              VALUE=VALUE+
     1        IVAL*((CUMULT(I,3)+CUMULT((I+1),3))/2.0D0)
C
 10       CONTINUE
C
C       NOW SAVE THE VALUE IN THE X- REGISTER, PUSH THE STACK
C       AND UPDATE THE LASTX REGISTER.
          REG(40)=REG(9)
          REG(9)=VALUE
          WRITE(OUTLYNE,1000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,2000) VALUE
          CALL SHOWIT(0)
          WRITE(OUTLYNE,3000)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,4000)
          CALL SHOWIT(0)
 1000     FORMAT(
     1    'THE VALUE OF THE INTEGRAL OF THE "SPECT" CUMULATIVE')
 2000     FORMAT('MEMORY AREA = ',G18.10)
 3000     FORMAT(1X)
 4000     FORMAT('THIS VALUE HAS BEEN STORED IN THE X-REGISTER')
          RETURN
      END
C SUB ITF.FOR
      SUBROUTINE ITF
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE INITIALIZES OR BLANKS OUT THE CURRENT
C       TRANSMISSION FILE DIRECTORY.
C
          INTEGER I,OH,N
C
          CHARACTER BLANK*8,B1*9,FN*10,AN*3
C
          REAL*8 ZERO
C
          LOGICAL EXISJK
C
          INCLUDE 'datmai.inc'
C
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',FORM=
     1    'UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
          CALL CLOSE_FILE(35,0)
          DO N=1,999
              CALL CCOONN(N,AN)
              IF(N.GT.0.AND.N.LE.9) FN='TRA00'//AN(3:3)//'.DAT'
              IF(N.GT.9.AND.N.LE.99) FN='TRA0'//AN(2:3)//'.DAT'
              IF(N.GT.99.AND.N.LE.999) FN='TRA'//AN(1:3)//'.DAT'
              EXISJK=.FALSE.
              INQUIRE(FILE=LIBTRA//FN,EXIST=EXISJK)
              IF(EXISJK) THEN
                  OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1            FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
                  CALL CLOSE_FILE(35,0)
              END IF
          END DO
C
          ZERO=0.0D0
          OH=0
          BLANK='        '
          B1=   '         '
C
C       OPEN UNIT 35 FOR I/O
C
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',FORM=
     1    'UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
          DO 25 I=1,999
              WRITE(UNIT=35,REC=I)
     1        BLANK,ZERO,ZERO,OH,OH,B1,BLANK
 25       CONTINUE
C
          CALL CLOSE_FILE(35,1)
C
C       CHECK EXISTANCE OF MACRO FILESTRA001 TO TRA999.DAT
C       IF FOUND. DELETE THEM
          CALL TRFLEX
          WRITE(OUTLYNE,*)
     1    'TRANSMISSION/REFLECTION FILES INITIALIZED'
          CALL SHOWIT(1)
          RETURN
      END
C SUB CUME.FOR
      SUBROUTINE CUME
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO ENTER THE SPECT LEVEL
C       COMMAND "CUME" AT THE SPECTR LEVEL
C
          INTEGER NTOTAL,COUNT,I
C
          REAL*8 LAMB1,LAMB2,CUMULT(1:1001,1:3)
C
          COMMON/CUM/CUMULT
C
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
          INCLUDE 'datmai.inc'
          WRITE(OUTLYNE,200)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,204) LAMB1
          CALL SHOWIT(0)
          WRITE(OUTLYNE,205) LAMB2
          CALL SHOWIT(0)
          WRITE(OUTLYNE,206) NTOTAL
          CALL SHOWIT(0)
          WRITE(OUTLYNE,201)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,202)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,203)
          CALL SHOWIT(0)
C       PROCEED
          DO 10 I=1,NTOTAL
              COUNT=INT(CUMULT(I,1))
              WRITE(OUTLYNE,100) COUNT,CUMULT(I,2),CUMULT(I,3)
              CALL SHOWIT(0)
 10       CONTINUE
 100      FORMAT(5X,I3,8X,G18.10,5X,G18.10)
 200      FORMAT(
     1    'LISTING OF THE "SPECT" CUMULATIVE MEMORY AREA')
 201      FORMAT(1X)
 202      FORMAT('ENTRY NUMBER',4X,'WAVLENGTH-(MICRONS)',6X,
     1    'FUNCTION VALUE')
 203      FORMAT(
     1    '--------------------------------------------------------')
 204      FORMAT('LAMBDA(1) = ',G18.10)
 205      FORMAT('LAMBDA(2) = ',G18.10)
 206      FORMAT('TOTAL NUMBER OF DATA POINTS = ',I3)


          RETURN
      END
C SUB WFACTOR.FOR
      SUBROUTINE WFACTOR(NUM_JK)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO EXECUTE THE SPECT COMMAND "WFACTOR"
C       COMMAND "CUME" AT THE SPECTR LEVEL
C
          INTEGER I,NUM_JK,ALLOERR,J
C
          REAL*8 CUMULT(1:1001,1:3),WVDELT,SUBSPACE
     1    ,SPWTS,SPWV,CURVAL,TAREA,BIG1,WVLIM,TESTAREA,TEST2,TEST2OLD
     2    ,STARTWV,STOPWV
C
          DIMENSION SPWTS(:),SPWV(:),BIG1(:),WVLIM(:)
C
          COMMON/CUM/CUMULT
          INCLUDE 'datmai.inc'
C
          ALLOCATABLE :: SPWTS,SPWV,BIG1,WVLIM
          DEALLOCATE(BIG1,SPWTS,SPWV,WVLIM,STAT=ALLOERR)
          ALLOCATE(BIG1(1000),SPWTS(10),SPWV(10),WVLIM(11),STAT=ALLOERR)
C
          SPWTS(1:10)=0.0D0
          SPWV(1:10)=0.0D0
          WVDELT=DABS(CUMULT(1001,2)-CUMULT(1,2))/100.0D0
C
C     INTEGERATE THE FULL CUMULT AREA
          DO I=1,1000
              CURVAL=((CUMULT(I,3)+CUMULT(I+1,3))/2.0D0)*WVDELT
              BIG1(I)=CURVAL
          END DO
C     CALCULATE SUB-DIVISION SPACING
          SUBSPACE=DABS(CUMULT(1001,2)-CUMULT(1,2))/DBLE(NUM_JK)
C     CALCULATE ENDS OF SUB-DIVISIONS
          WVLIM(1)=CUMULT(1,2)
          DO I=2,NUM_JK+1
              WVLIM(I)=WVLIM(I-1)+SUBSPACE
          END DO
          DO I=1,1000
              DO J=1,NUM_JK
                  IF((CUMULT(I,2)+CUMULT(I+1,2))/2.0D0.GE.WVLIM(J).AND.
     1            (CUMULT(I,2)+CUMULT(I+1,2))/2.0D0.LE.WVLIM(J+1))
     1                       SPWTS(J)=SPWTS(J)+BIG1(I)
              END DO
          END DO
C     COMPUTE THE FULL CUMULT AREA AND NORMALIZE THE WEIGHTING FACTORS
          TAREA=0.0D0
          DO I=1,NUM_JK
              TAREA=TAREA+SPWTS(I)
          END DO
          DO I=1,NUM_JK
              SPWTS(I)=SPWTS(I)/TAREA
          END DO
C
C     NOW COMPUTE THE AREA CENTER WAVELENGTHS FOR EACH BAND
          DO I=1,NUM_JK
              STARTWV=WVLIM(I)
              STOPWV =WVLIM(I+1)
              TESTAREA=0.0D0
              DO J=1,1000
                  IF((CUMULT(J,2)+CUMULT(J+1,2))/2.0D0.GE.STARTWV.AND.
     1            (CUMULT(J,2)+CUMULT(I+J,2))/2.0D0.LE.STOPWV) THEN
                      TESTAREA=TESTAREA+BIG1(J)
                  END IF
              END DO
              TEST2=0.0D0
              TEST2OLD=0.0D0
              DO J=1,1000
                  IF((CUMULT(J,2)+CUMULT(J+1,2))/2.0D0.GE.STARTWV.AND.
     1            (CUMULT(J,2)+CUMULT(J+1,2))/2.0D0.LE.STOPWV) THEN
                      TEST2OLD=TEST2
                      TEST2=TEST2+BIG1(J)
                      IF(TEST2OLD.LE.(TESTAREA/2.0D0).AND.TEST2.GE.(TESTAREA/2.0D0))
     1                THEN
C     SET WAVELENGTH AND GO TO 25
                          SPWV(I)=(CUMULT(J,2)+CUMULT(J+1,2))/2.0D0
                          GO TO 25
                      END IF
                  END IF
              END DO
C
 25           CONTINUE
          END DO
          WRITE(OUTLYNE,202)
          CALL SHOWIT(0)
          WRITE(OUTLYNE,203)
          CALL SHOWIT(0)
C       PROCEED
          DO I=1,NUM_JK
              WRITE(OUTLYNE,100) SPWV(I),SPWTS(I)
              CALL SHOWIT(0)
          END DO
 100      FORMAT(2X,G18.10,1X,6X,2X,G18.10)
! 200    FORMAT(
!     1  'SPECTRAL WAVELENGTHS AND WEIGHTS')
! 201    FORMAT(1X)
 202      FORMAT(' WAVLENGTH-(MICRONS) ',6X,
     1    '   SPECTRAL WEIGHT   ')
 203      FORMAT(
     1    '--------------------------------------------------------')
          DEALLOCATE(BIG1,SPWTS,SPWV,WVLIM,STAT=ALLOERR)
          RETURN
      END
C SUB DROP.FOR
      SUBROUTINE DROP
C
          IMPLICIT NONE
C
          INTEGER I,J
C
C       THIS SUBROUTINE HANDELS DROPING OF RECORD W1 FROM
C       SPECT DISK FILE WQ
C
C                       DEFINE VARIABLES
C
          CHARACTER DDATE*10,TTIME*8,FN*10,NM*8,TTTIM*8,DDDAT*10
C
          INTEGER POINTS,OCC,CTAB
C
          REAL*8 DATA1,DATA2,TABLE(1:1001,1:3)
C
          LOGICAL EXISJK
C
          INTEGER NF
C
          COMMON/STRNGR/TTTIM,DDDAT,NM,FN
C
          COMMON/TABL/TABLE,CTAB
C
          INCLUDE 'datmai.inc'
C
C       TABLE DATA IN BINARY FORMAT. TRA.DAT IS UNIT 35
C
C       DATA STORED IN TRA.DAT IS:
C               FILE NAME (TNAME) CHARACTER *8
C               DATA1 STARTING WAVELENGTH (DOUBLE PRES.)
C               DATA2 ENDIG WAVLENGTH (DOUBLE PRES.)
C               POINTS - NUMBER OF DATA POINTS (INTEGER)
C               OCC- OCCUPANCY FLAG, 0=EMPTY, 1- FULL (INTEGER)
C               DDATE-DATE FILED
C               TTIME=TIME FILED
C       FIND THE DESIRED FILE
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1    FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
          DO 200 I=1,999
              READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              TTTIM=TTIME
              DDDAT=DDATE
              IF(NM.EQ.WQ) THEN
C       FOUND FILE
                  GO TO 300
              END IF
 200      CONTINUE
C       IF YOU GOT HERE, FILE DID NOT EXIST
          WRITE(OUTLYNE,*)'FILE ',WQ,' DOES NOT EXIST'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL CLOSE_FILE(35,1)
          CALL MACFAL
          RETURN
 300      CONTINUE
C       LOAD FILE INTO TABLE ARRAY
C
          NF=I
          CALL TRAFIL(NF,FN)
C
C       OPEN AND READ FROM FILE
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1    FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
          DO 210 J=1,POINTS
              READ(UNIT=35,REC=J) TABLE(J,1),TABLE(J,2),TABLE(J,3)
 210      CONTINUE
          CALL CLOSE_FILE(35,1)
          IF(INT(W1).LT.1.OR.INT(W1).GT.POINTS) THEN
              WRITE(OUTLYNE,*)'ENTRY # ',INT(W1),' IS BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C

C       DELETE RECORD INT(W1) IF FOUND
          DO 350 I=INT(W1),(POINTS-1)
              TABLE(I,1)=TABLE((I+1),1)-1.0D0
              TABLE(I,2)=TABLE((I+1),2)
              TABLE(I,3)=TABLE((I+1),3)
 350      CONTINUE
          POINTS=POINTS-1
          CTAB=POINTS
          CALL MYTIME(TTIME)
          CALL MYDATE(DDATE)
          TTTIM=TTIME
          DDDAT=DDATE
          IF(POINTS.EQ.0) THEN
              DATA1=0.0D0
              DATA2=0.0D0
              OCC=0
              DDATE='         '
              TTIME='        '
              DDDAT='         '
              TTTIM='        '
              NM='        '
          END IF
C
C       FILE THE FILE IN THE APPROPRIATE SPOT
C       OPEN AND WRITE TO FILE
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1    FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
          WRITE(UNIT=35,REC=NF) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
          CALL CLOSE_FILE(35,1)
          IF(OCC.EQ.0) THEN
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1        FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
              CALL CLOSE_FILE(35,0)
          END IF
C
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1    FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
          F19=1
          DO 220 J=1,POINTS
              WRITE(UNIT=35,REC=J) TABLE(J,1),TABLE(J,2),TABLE(J,3)
 220      CONTINUE
          CALL CLOSE_FILE(35,1)
C       PRINT MESSAGE TO THAT EFFECT
          WRITE(OUTLYNE,*)'ENTRY # ',INT(W1),' DELETED FROM FILE ',WQ
          CALL SHOWIT(1)
          IF(OCC.EQ.0) THEN
              WRITE(OUTLYNE,*)'FILE ',WQ,' IS NOW EMPTY'
              CALL SHOWIT(1)
              F19=0
          END IF
C
          RETURN
C
      END
C SUB DELETE.FOR
      SUBROUTINE DELETE
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS DELETING A SPECT DISK FILE
C
C                       DEFINE VARIABLES
C
          INTEGER NF,I
C
          CHARACTER NM*8,FN*10,DDATE*10,TTIME*8,DDDAT*10,TTTIM*8
C
          LOGICAL EXISJK
C
          INTEGER POINTS,OCC
C
          REAL*8 DATA1,DATA2
C
          COMMON/STRNGR/DDDAT,TTTIM,NM,FN
C
          INCLUDE 'datmai.inc'
C
C       TABLE DATA IN BINARY FORMAT. TRA.DAT IS UNIT 35
C
C       DATA STORED IN TRA.DAT IS:
C               FILE NAME (TNAME) CHARACTER *8
C               DATA1 STARTING WAVELENGTH (DOUBLE PRES.)
C               DATA2 ENDIG WAVLENGTH (DOUBLE PRES.)
C               POINTS - NUMBER OF DATA POINTS (INTEGER)
C               OCC- OCCUPANCY FLAG, 0=EMPTY, 1- FULL (INTEGER)
C               DDATE-DATE FILED
C               TTIME=TIME FILED
C       IF DF1=0, SPECIFIC NUMERIC FILE NUMBER TO BE DELETED
C
          IF(DF1.EQ.0) THEN
              WQ='        '
              SQ=1
              IF(W1.LE.0.0.OR.W1.GT.999.0) THEN
                  WRITE(OUTLYNE,*)
     1            'FILE # REQUESTED FOR DELETION IS BEYOND DEFINED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'FILE NUMBER RANGE. VALID RANGE IS 1 TO 999.'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
C       DF1=1, LOOK FOR A FILE BY NAME
              GO TO 100
          END IF
C       NOW TRY TO DELETE FILE INT(W1)
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1    FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
          I=INT(W1)
          READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
          TTTIM=TTIME
          DDDAT=DDATE
          IF(OCC.EQ.0) THEN
C       FILE ALREADY EMPTY
              WRITE(OUTLYNE,*)'FILE # ',INT(W1),'DOES NOT EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       FILE TO BE DELETED
              NM='        '
              DATA1=0.0D0
              DATA2=0.0D0
              POINTS=0
              OCC=0
              DDATE='         '
              TTIME='        '
              DDDAT='         '
              TTTIM='        '
              WRITE(UNIT=35,REC=I)NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              CALL CLOSE_FILE(35,1)
              NF=I
              CALL TRAFIL(NF,FN)
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1        FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
              CALL CLOSE_FILE(35,0)
              WRITE(OUTLYNE,*)'FILE # ',INT(W1),' DELETED'
              CALL SHOWIT(1)
          END IF
          RETURN
C
 100      CONTINUE
C
C       FILE FILE NAMED WQ
C
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1    FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
          DO 200 I=1,999
C
              READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              DDDAT=DDATE
              TTTIM=TTIME
C
              IF(NM.EQ.WQ) THEN
C       FOUND FILE TO DELETE
                  NM='        '
                  DATA1=0.0D0
                  DATA2=0.0D0
                  POINTS=0
                  OCC=0
                  DDATE='         '
                  TTIME='        '
                  DDDAT='         '
                  TTTIM='        '
                  WRITE(UNIT=35,REC=I)NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
                  CALL CLOSE_FILE(35,1)
                  NF=I
                  CALL TRAFIL(NF,FN)
                  OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1            FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
                  CALL CLOSE_FILE(35,0)
                  WRITE(OUTLYNE,*)'FILE NAMED ',WQ,' DELETED'
                  CALL SHOWIT(1)
                  RETURN
              END IF
 200      CONTINUE
C       NEVER FOUND FILE
          WRITE(OUTLYNE,*)'FILE NAMED ',WQ,'DOES NOT EXIST'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
C
      END
C SUB GFILE.FOR
      SUBROUTINE GFILE
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE "GETFILE" SPECT COMMAND.
C
C                       DEFINE VARIABLES
C
          INTEGER I,J,JK,KI
C
          CHARACTER NM*8,FN*10,DDATE*10,TTIME*8,TTTIM*8,DDDAT*10
C
          LOGICAL EXISJK
C
          REAL*8 WORK(1:1001,1:3),L1,L2,
     1    DATA1,DATA2,LAMB1,LAMB2,CUMULT(1:1001,1:3),DELLAM,WORKFC,
     2    SLOPE
C
          COMMON/STRNGR/DDDAT,TTTIM,NM,FN
C
          INTEGER OCC,POINTS,NTOTAL,NF
C
          COMMON/CUM/CUMULT
C
          COMMON/WRK/WORK
C
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
          INCLUDE 'datmai.inc'
C
C       TABLE DATA IN BINARY FORMAT. TRA.DAT IS UNIT 35
C
C       DATA STORED IN TRA.DAT IS:
C               FILE NAME (TNAME) CHARACTER *8
C               DATA1 STARTING WAVELENGTH (DOUBLE PRES.)
C               DATA2 ENDIG WAVLENGTH (DOUBLE PRES.)
C               POINTS - NUMBER OF DATA POINTS (INTEGER)
C               OCC- OCCUPANCY FLAG, 0=EMPTY, 1- FULL (INTEGER)
C               DDATE-DATE FILED
C               TTIME=TIME FILED
C       FIND THE DESIRED FILE
C       ***************************************************************
          EXISJK=.FALSE.
          INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       ***************************************************************
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1    FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
          DO 200 I=1,999
              READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
              DDDAT=DDATE
              TTTIM=TTIME
              IF(NM.EQ.WQ) THEN
C       FOUND FILE
                  GO TO 300
              END IF
 200      CONTINUE
C       IF YOU GOT HERE, FILE DID NOT EXIST
          WRITE(OUTLYNE,*)'FILE ',WQ,' DOES NOT EXIST'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL CLOSE_FILE(35,1)
          CALL MACFAL
          RETURN
 300      CONTINUE
C
C       ESTABLISH NEW WAVELENGTH BOUNDARIES. NO WAVELENGTH
C       VALUE MAY BE LESS THAN 0.0 MICRONS
C
          L1=(LAMB1*W1)+W2
          IF(L1.LT.0.0D0) L1=0.0D0
          L2=(LAMB2*W1)+W2
C
          IF(L1.LT.0.0D0.OR.W2.LT.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'A NEGATIVE WAVELENGTH VALUE IS NOT ALLOWED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(L2.LE.0.0D0.OR.L2.LE.L1) THEN
C       NO WAVELENGTH RANGE EXISTS, NO INTERPOLATION POSSIBLE
              WRITE(OUTLYNE,*)'WAVELENGTH LIMITS ARE NOT REALISTIC'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
C       REDEFINE LAMB1 AND LAMB2
              LAMB1=L1
              LAMB2=L2
          END IF
C
C       LOAD FILE INTO WORK ARRAY
C
          NF=I
          CALL TRAFIL(NF,FN)
C
C       OPEN AND READ FROM FILE
          OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1    FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
          DO 210 J=1,POINTS
              READ(UNIT=35,REC=J) WORK(J,1),WORK(J,2),WORK(J,3)
 210      CONTINUE
          CALL CLOSE_FILE(35,1)
C
C
C       INTERPOLATE TO THE POINTS DEFINED IN THE CUMULATIVE
C       AREA BY LAMB1,LAMB2 AND NTOTAL.
C
C       THE DIFFERENCE IN WAVELENGTH BETWEEN ENTRIES IN THE NEW
C       DATA WILL BE
C       (LAMB2-LAMB1)/(NTOTAL-1)
C
          DELLAM=(LAMB2-LAMB1)/(DBLE(NTOTAL)-1.0D0)
C
C       SCALE THE CURRENT WORK DATA BY THE REQUESTED
C       TRANSFORMATION.
C
          DO 505 J=1,POINTS
              WORK(J,3)=(((WORK(J,3)*W3)+W4)**INT(DABS(W5)))
 505      CONTINUE
          IF(INT(W5).LT.0.0D0) THEN
              DO 506 J=1,POINTS
                  IF(WORK(J,3).NE.0.0D0) WORK(J,3)=1.0D0/WORK(J,3)
                  IF(WORK(J,3).EQ.0.0D0) WORK(J,3)=0.0D0
 506          CONTINUE
          END IF
C
C       THESE NEW FUNCTIONAL VALUES ARE FOR THE NEW WAVELENGTH
C       VALUES :
          DO 500 J=1,POINTS
              WORK(J,2)=(WORK(J,2)*W1)+W2
              IF(WORK(J,2).LT.0.0D0) WORK(J,2)=0.0D0
 500      CONTINUE
C
          IF(WORK(POINTS,2).LE.WORK(1,2)) THEN
              WRITE(OUTLYNE,*)
     1        'NEW WAVELENGTHS IN WORK AREA ARE NOT IN ASCENDING ORDER AND'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'"GETFILE" WAS ABORTED.'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       THE NEW FUNCTIONAL VALUES ARE SPREAD OVER THE NEW LAMB1 TO
C       LAMB2 RANGE IN CUMULT, WITH SPACING DELLAM.
C
          CUMULT(1,2)=LAMB1
          CUMULT(NTOTAL,2)=LAMB2
          DO 10 J=2,(NTOTAL-1)
              CUMULT(J,2)=CUMULT((J-1),2)+DELLAM
 10       CONTINUE
C
C       NOW INTERPOLATE THE WORK AREA AND MULTIPLY INTO THE
C       CUMULATIVE AREA FUNCTIONAL VALUES.
C
          SLOPE=0.0D0
          JK=1
          DO 20 J=1,POINTS-1
C
C       CALCULATE A SLOPE FROM THE WORK DATA
C
              SLOPE=(((WORK((J+1),3))-(WORK(J,3)))/
     1        ((WORK((J+1),2))-(WORK(J,2))))
              KI=JK
C
C       RULES FOR INTERPOLATION (EXTRAPOLATION MAY OCCUR
C       AT END POINTS)
C
C       GET A WAVELENGTH VALUE FROM THE CUMULATIVE AREA
C       AS LONG AS THE WAVELENGTH VALUE FROM THE
C       CUMULATIVE AREA IS NOT GREATER THAN THE CURRENT
C       VALUE OF WORK((J+1),2), WE KEEP USING THE CURRENT SLOPE.
C
C       IF THE WAVELENGTH VALUE FROM CUMULATIVE AREA BECOMES
C       GREATER THAN THE CURRENT WORK((J+1),2), CALCULATE A NEW
C       SLOPE UNLESS J+1=POINTS. IF J+1=POINTS, KEEP USING
C       THE CURRENT SLOPE UNTIL THE CUMULATIVE AREA IS FILLED.
C
C       IF THE LAST WAVELENGTH VALUE IN WORK IS REACHED(J=POINTS),
C
              DO 30 I=KI,NTOTAL
                  IF(CUMULT(I,2).LE.WORK((J+1),2).AND.(J+1).LE.POINTS
     1            .OR.CUMULT(I,2).GT.WORK((J+1),2).AND.(J+1).GE.POINTS) THEN
                      WORKFC=(SLOPE*(CUMULT(I,2)-WORK(J,2)))+WORK(J,3)
                      CUMULT(I,3)=CUMULT(I,3)*(WORKFC)
                  ELSE
C       WE MUST
C       CALCULATE A NEW SLOPE. THE UNLESS OUT OF POINTS CASE WAS DONE
C       ABOVE.
                      JK=I
                      GO TO 20
                  END IF
 30           CONTINUE
C
 20       CONTINUE
C
C       CUMULATIVE AREA HAS BEEM MULTIPLIED BY THE TRANSFORMED,
C       INTERPOLATED WORK AREA. PRINT MESSAGE AND RETURN
          WRITE(OUTLYNE,*)
     1    'SPECT FILE ( ',WQ,' ) READ INTO WORK AREA MEMORY'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'TRANSFORMED, INTERPOLATED AND'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)
     1    'MULTIPLIED INTO THE CUMULATIVE MEMORY AREA'
          CALL SHOWIT(1)
          F19=1
C
          RETURN
C
      END
