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

C       FIRST SET OF UTILTIY ROUTINES GO HERE

C SUB PPRINT.FOR
      SUBROUTINE PPRINT
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE "PRINT" SPECT COMMAND.
C
C       DEFINE VARIABLES
C
          INTEGER I,J,JK,KI
          CHARACTER NM*8,FN*10,DDATE*9,TTIME*8

          LOGICAL EXISJK

          REAL*8 WORK(1:1001,1:3),L1,L2,
     1    DATA1,DATA2,LAMB1,LAMB2,CUMULT(1:1001,1:3),DELLAM,WORKFC,
     2    SLOPE,TEMP(1:1001,1:3)

          INTEGER OCC,POINTS,NTOTAL,TCOUNT,NF

          COMMON/CUM/CUMULT
          COMMON/WRK/WORK
          COMMON/WAVEL/LAMB1,LAMB2,NTOTAL

          INCLUDE 'datmai.inc'
C
C       IF SQ.EQ.0, DON'T LOOK FOR A FILE, JUST MOVE THE
C       CUMULATIVE AREA.
C
          IF(SQ.EQ.1) THEN
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
     1        FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
              DO 200 I=1,200
                  READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
                  IF(NM.EQ.WQ) THEN
C       FOUND FILE
                      GO TO 300
                  END IF
 200          CONTINUE
C       IF YOU GOT HERE, FILE DID NOT EXIST
              WRITE(OUTLYNE,*)'FILE ',WQ,' DOES NOT EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL CLOSE_FILE(35,1)
              CALL MACFAL
              RETURN
 300          CONTINUE
C
C
C       LOAD FILE INTO TEMP ARRAY
C
              NF=I
              CALL TRAFIL(NF,FN)
C
C       OPEN AND READ FROM FILE
              OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1        FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
              TCOUNT=POINTS
              DO 210 J=1,POINTS
                  READ(UNIT=35,REC=J) TEMP(J,1),TEMP(J,2),TEMP(J,3)
 210          CONTINUE
              CALL CLOSE_FILE(35,1)
          ELSE
C       QUALIFIER WAS BLANK, NO FILE TO LOAD
C       LOAD CUMULATIVE AREA INTO WORK
              TCOUNT=NTOTAL
              POINTS=NTOTAL
              J=NTOTAL
              TEMP(1:J,1:3)=CUMULT(1:J,1:3)
          END IF
C
C       ESTABLISH NEW WAVELENGTH BOUNDARIES. NO WAVELENGTH
C       VALUE MAY BE LESS THAN 0.0 MICRONS
C
          L1=(LAMB1*W1)+W2
          IF(L1.LT.0.0D0) L1=0.0D0
          L2=(LAMB2*W1)+W2
          IF(L1.LE.0.0D0.OR.L2.LE.0.0D0) THEN
              WRITE(OUTLYNE,*)
     1        'A ZERO OR NEGATIVE WAVELENGTH VALUE IS NOT ALLOWED'
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

C       INTERPOLATE TO THE POINTS DEFINED IN THE CUMULATIVE
C       AREA BY LAMB1,LAMB2 AND NTOTAL.
C
C       THE DIFFERENCE IN WAVELENGTH BETWEEN ENTRIES IN THE NEW
C       DATA WILL BE
C       (LAMB2-LAMB1)/(NTOTAL-1)
C
          DELLAM=(LAMB2-LAMB1)/(DBLE(NTOTAL)-1.0D0)
C
C       SCALE THE CURRENT TEMP DATA BY THE REQUESTED
C       TRANSFORMATION.
C
          DO 505 J=1,TCOUNT
              TEMP(J,3)=(((TEMP(J,3)*W3)+W4)**INT(DABS(W5)))
 505      CONTINUE
          IF(INT(W5).LT.0.0D0) THEN
              DO 506 J=1,TCOUNT
                  IF(TEMP(J,3).NE.0.0D0) TEMP(J,3)=1.0D0/TEMP(J,3)
                  IF(TEMP(J,3).EQ.0.0D0) TEMP(J,3)=0.0D0
 506          CONTINUE
          END IF
C
C       THESE NEW FUNCTIONAL VALUES ARE FOR THE NEW WAVELENGTH
C       VALUES :
          DO 500 J=1,TCOUNT
              TEMP(J,2)=(TEMP(J,2)*W1)+W2
              IF(TEMP(J,2).LT.0.0D0) TEMP(J,2)=0.0D0
 500      CONTINUE
C
          IF(TEMP(POINTS,2).LE.TEMP(1,2)) THEN
              WRITE(OUTLYNE,*)
     1        'NEW WAVELENGTHS ARE NOT IN ASCENDING ORDER AND'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'"PRINT" WAS ABORTED.'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       THE NEW FUNCTIONAL VALUES ARE SPREAD OVER THE NEW LAMB1 TO
C       LAMB2 RANGE IN WORK, WITH SPACING DELLAM.
C
          DO 11 J=1,NTOTAL
              WORK(J,1)=DBLE(J)
 11       CONTINUE
          WORK(1,2)=LAMB1
          WORK(NTOTAL,2)=LAMB2
          DO 10 J=2,(NTOTAL-1)
              WORK(J,2)=WORK((J-1),2)+DELLAM
 10       CONTINUE
C
C       NOW INTERPOLATE THE TEMP AREA AND STORE INTO THE
C       WORK AREA FUNCTIONAL VALUES.
C
          SLOPE=0.0D0
          JK=1
          DO 20 J=1,TCOUNT-1
C
C       CALCULATE A SLOPE FROM THE WORK DATA
C
              SLOPE=(((TEMP((J+1),3))-(TEMP(J,3)))/
     1        ((TEMP((J+1),2))-(TEMP(J,2))))
              KI=JK
C
C       RULES FOR INTERPOLATION (EXTRAPOLATION MAY OCCUR
C       AT END POINTS)
C
C       GET A WAVELENGTH VALUE FROM THE CUMULATIVE AREA
C       AS LONG AS THE WAVELENGTH VALUE FROM THE
C       CUMULATIVE AREA IS NOT GREATER THAN THE CURRENT
C       VALUE OF TEMP((J+1),2), WE KEEP USING THE CURRENT SLOPE.
C
C       IF THE WAVELENGTH VALUE FROM CUMULATIVE AREA BECOMES
C       GREATER THAN THE CURRENT TEMP((J+1),2), CALCULATE A NEW
C       SLOPE UNLESS J+1=TCOUNT. IF J+1=TCOUNT, KEEP USING
C       THE CURRENT SLOPE UNTIL THE CUMULATIVE AREA IS FILLED.
C
C       IF THE LAST WAVELENGTH VALUE IN WORK IS REACHED(J=TCOUNT),
C
              DO 30 I=KI,NTOTAL
                  IF(WORK(I,2).LE.TEMP((J+1),2).AND.(J+1).LE.TCOUNT
     1            .OR.WORK(I,2).GT.TEMP((J+1),2).AND.(J+1).GE.TCOUNT) THEN
                      WORKFC=(SLOPE*(WORK(I,2)-TEMP(J,2)))+TEMP(J,3)
                      WORK(I,3)=WORKFC
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
C       WORK AREA HAS HAD STORED IN IT THE TRANSFORMED,
C       INTERPOLATED TEMP AREA. PRINT MESSAGE AND RETURN
          WRITE(OUTLYNE,*)
     1    'THE NEW CONTENTS OF THE "WORK" AREA AFTER ACTION'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'BY THE "PRINT" COMMAND ARE:.'
          CALL SHOWIT(1)
          CALL PWORK
C
          RETURN
C
      END
C SUB PRNLP.FOR
      SUBROUTINE PRNLP
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO PRINT THE PRINTER FILE.
C
          LOGICAL EXIS7,OPEN7
C
!      CHARACTER PLINE*80
C
!      INTEGER I
C
          LOGICAL NOPRIN
C
          COMMON/PRINNO/NOPRIN
C
          INCLUDE 'datmai.inc'
C
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE='"PRINT" TAKES NO NUMERIC OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              SQ=1
              WQ='P       '
          END IF
          IF(SQ.EQ.1.AND.WQ(1:1).NE.'P'.AND.WQ(1:1).NE.'L') THEN
              OUTLYNE='"PRINT" ONLY TAKES "P" AND "L" AS OPTIONAL QUALIFIERS'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          EXIS7=.FALSE.
          OPEN7=.FALSE.
          INQUIRE(FILE=trim(HOME)//'PRINTER.TXT',EXIST=EXIS7)
          IF(EXIS7) THEN
              INQUIRE(FILE=trim(HOME)//'PRINTER.TXT',OPENED=OPEN7)
              CALL CLOSE_FILE(7,1)
!      CALL PREDITOR
              IF(OUT.EQ.7) THEN
C     REOPEN
                  IF(APPEND) THEN
                      OPEN(UNIT=7,ACCESS='APPEND',BLANK='NULL'
     1                  ,FORM='FORMATTED',FILE=trim(HOME)//'PRINTER.TXT'
     2                  ,STATUS='UNKNOWN')
                  END IF
                  IF(.NOT.APPEND) THEN
                      OPEN(UNIT=7,ACCESS='SEQUENTIAL',BLANK='NULL'
     1                  ,FORM='FORMATTED',FILE=trim(HOME)//'PRINTER.TXT'
     1                  ,STATUS='UNKNOWN')
                  END IF
              END IF
          ELSE
C     FILE DOES NOT EXIST
              OUTLYNE='NO PRINTER FILE WAS FOUND'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          RETURN
          CONTINUE
          CALL CLOSE_FILE(7,1)
          OUTLYNE='PRINTER FILE CAN NOT BE READ'
          CALL SHOWIT(1)
          OUTLYNE='NO PRINTING WILL BE PERFORMED'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
          CONTINUE
          OUTLYNE='PRINTER IS BUSY'
          CALL SHOWIT(1)
          OUTLYNE='TRY AGAIN AFTER THE PRINTER IS DONE'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
      END


C SUB NEWSEED.FOR
      SUBROUTINE NEWSEED
C
          IMPLICIT NONE

          INCLUDE 'datmai.inc'

          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"NEWSEED" re-initializes the random number generator'
              CALL SHOWIT(1)
              RETURN
          END IF

          call randset

          RETURN
      END


C SUB PM.FOR
      SUBROUTINE PM
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS DOES THE "PM" AND "PMP" CMD LEVEL COMMANDS
C       AND ALSO SETS THE DEFAULT OPCON OR OPERATING CONDITIONS.
C       USED FOR CHANGING, SETTING, AND QUERRYING
C       OPERATING CONDITIONS
C
          INTEGER IVAL,ALLOERR
C
          REAL*8 VAL
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
          IF(WC.EQ.'PM') THEN
C
              IF(WQ.NE.'SURTOL'.AND.WQ.NE.'AIMTOL'.AND.WQ.NE.'SERINC'
     1        .AND.WQ.NE.'PFAC'.AND.WQ.NE.'MRAYS'.AND.WQ.NE.'CAIMTOL'
     2        .AND.WQ.NE.'DINMUL'.AND.WQ.NE.'NRAITR'.AND.WQ.NE.'LINTOL'
     3        .AND.WQ.NE.'ONTOL'.AND.WQ.NE.'SINGTOL'.AND.
     4        WQ.NE.'DIFTOL'.AND.WQ.NE.'DELSUR'.AND.
     5        WQ.NE.'SERLIM'.AND.
     6        WQ.NE.'SAGDEL'.AND.WQ.NE.'MAXREG'.AND.WQ.NE.'MAXOPT') THEN
                  OUTLYNE='INVALID QUALIFIER USED WITH "PM" COMMAND'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  OUTLYNE=
     1            'PM '//WQ//'REQUIRES NUMERIC EXPLICIT WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  OUTLYNE=
     1            'PM '//WQ//'ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'NRAITR') THEN
                  IF(WQ.EQ.'NRAITR') NRAITR=INT(W1)
                  WRITE(OUTLYNE,1001) WQ,INT(W1)
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(WQ.EQ.'SURTOL'.OR.WQ.EQ.'AIMTOL'.OR.WQ.EQ.'SERINC'
     1        .OR.WQ.EQ.'PFAC'.OR.WQ.EQ.'MRAYS'.OR.WQ.EQ.'CAIMTOL'
     2        .OR.WQ.EQ.'DINMUL'.OR.WQ.EQ.'DIFTOL'.OR.WQ.EQ.'DELSUR'
     3        .OR.WQ.EQ.'ONTOL'.OR.WQ.EQ.'SINGTOL'.OR.WQ.EQ.'LINTOL'.OR.
     4        WQ.EQ.'SERLIM'.OR.WQ.EQ.'SAGDEL'.OR.WQ.EQ.'MAXREG'.OR.
     5        WQ.EQ.'MAXOPT') THEN
                  IF(WQ.EQ.'SURTOL') SURTOL=W1
C       LIMITS FOR AIMTOL
                  IF(WQ.EQ.'AIMTOL') THEN
                      IF(W1.LE.0.0D0) THEN
                          OUTLYNE=
     1                    '"PM AIMTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.LT.1.0D-25) THEN
                          OUTLYNE=
     1                    'THE "AIMTOL" VALUE MAY NOT BE LESS THAN 1.0D-25'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.GE.1.0D0) THEN
                          OUTLYNE=
     1                    'THE "AIMTOL" VALUE MUST BE LESS THAN 1.0'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      AIMTOL=W1
                  ELSE
                  END IF
C       NOT CAIMTOL
C       LIMITS FOR CAIMTOL
                  IF(WQ.EQ.'CAIMTOL') THEN
                      IF(W1.LE.0.000001D0) THEN
                          OUTLYNE=
     1                    '"PM CAIMTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT >= 0.000001'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.LT.0.000001D0) THEN
                          OUTLYNE=
     1                    'THE "CAIMTOL" VALUE MAY NOT BE LESS THAN 1.0D-6'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.GE.1.0D0) THEN
                          OUTLYNE=
     1                    'THE "CAIMTOL" VALUE MUST BE LESS THAN 1.0'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      CAIMTOL=W1
                  ELSE
                  END IF
C       NOT CAIMTOL
C       LIMITS FOR MRAYS
                  IF(WQ.EQ.'MRAYS') THEN
                      IF(W1.LT.1.0D0) THEN
                          OUTLYNE=
     1                    'THE "MRAYS" VALUE MUST BE LESS THAN GREATER THAN OR EQUAL TO 1'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      MRAYS=INT(W1)
                  ELSE
                  END IF
C       NOT MRAYS
C       LIMITS FOR MAXREG
                  IF(WQ.EQ.'MAXREG') THEN
                      IF(INT(W1).LT.4000) THEN
                          OUTLYNE=
     1                    '"PM MAXREG" REQUIRES NUMERIC WORD #1 INPUT GREATER THAN OR'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'EQUAL TO 4000'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(W1).GT.100000) THEN
                          OUTLYNE=
     1                    'THE "MAXREG" VALUE MAY NOT BE GREATER THAN 100000'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      MAXREG=INT(W1)
                  ELSE
                  END IF
C       NOT MAXREG
C       LIMITS FOR MAXOPT
                  IF(WQ.EQ.'MAXOPT') THEN
                      IF(INT(W1).LT.4000) THEN
                          OUTLYNE=
     1                    '"PM MAXOPT" REQUIRES NUMERIC WORD #1 INPUT GREATER THAN OR'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'EQUAL TO 4000'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(INT(W1).GT.100000) THEN
                          OUTLYNE=
     1                    'THE "MAXOPT" VALUE MAY NOT BE GREATER THAN 100000'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      MAXOPT=INT(W1)
                      MAXVB=INT(W1)
                      WRITE(OUTLYNE,*) MAXVB,INT(W1),'HI3'
                      CALL SHOWIT(1)
                      MAXTVB=MAXVB-MAXCMP
                  ELSE
                  END IF
C       NOT MAXOPT
C       LIMITS FOR SAGDEL
                  IF(WQ.EQ.'SAGDEL') THEN
                      IF(W1.LE.0.0D0) THEN
                          OUTLYNE=
     1                    '"PM SAGDEL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.LT.1.0D-15) THEN
                          OUTLYNE=
     1                    'THE "SAGDEL" VALUE MAY NOT BE LESS THAN 1.0D-15'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      SAGDEL=W1
                  ELSE
                  END IF
C       NOT SAGDEL
C       LIMITS FOR SERINC
                  IF(WQ.EQ.'SERINC') THEN
                      IF(W1.LE.0.0D0) THEN
                          OUTLYNE=
     1                    '"PM SERINC" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.LT.1.0D-15) THEN
                          OUTLYNE=
     1                    'THE "SERINC" VALUE MAY NOT BE LESS THAN 1.0D-15'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.GE.(0.5D0*SERLIM)) THEN
                          OUTLYNE=
     1                    'THE "SERINC" VALUE MUST BE LESS THAN ONE HALF OF SERLIM'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      SERINC=W1
                  ELSE
                  END IF
C       LIMITS FOR SERLIM
                  IF(WQ.EQ.'SERLIM') THEN
                      IF(W1.LE.0.0D0) THEN
                          OUTLYNE=
     1                    '"PM SERLIM" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.LE.(2.0D0*SERINC)) THEN
                          OUTLYNE=
     1                    'THE "SERLIM" VALUE MUST BE GREATER THAN 2 TIMES SERINC'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      SERLIM=W1
                  ELSE
                  END IF
C       NOT SERLIM
                  IF(WQ.EQ.'DELSUR') THEN
                      IF(W1.LE.0.0D0) THEN
                          OUTLYNE=
     1                    '"PM DELSUR" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.LT.1.0D-15) THEN
                          OUTLYNE=
     1                    'THE "DELSUR" VALUE MAY NOT BE LESS THAN 1.0D-15'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.GE.10.0D0) THEN
                          OUTLYNE=
     1                    'THE "DELSUR" VALUE MUST BE LESS THAN 10.0'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      DELSUR=W1
                  ELSE
C       NOT DELSUR
                  END IF
                  IF(WQ.EQ.'DINMUL') THEN
                      IF(W1.LE.0.0D0) THEN
                          OUTLYNE=
     1                    '"PM DINMUL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.LT.1.0D-15) THEN
                          OUTLYNE=
     1                    'THE "DINMUL" VALUE MAY NOT BE LESS THAN 1.0D-15'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      DINMUL=W1
                  ELSE
C       NOT DINMUL
                  END IF
                  IF(WQ.EQ.'PFAC') PFAC=W1
                  IF(WQ.EQ.'DIFTOL') THEN
                      IF(W1.LE.0.0D0) THEN
                          OUTLYNE=
     1                    '"PM DIFTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.GE.1.0D0) THEN
                          OUTLYNE=
     1                    'THE "DIFTOL" VALUE MUST BE LESS THAN 1.0'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      DIFTOL=W1
                  ELSE
C       NOT DIFTOL
                  END IF
                  IF(WQ.EQ.'LINTOL') THEN
                      IF(W1.LE.0.0D0.OR.W1.GT.1.0D0) THEN
                          OUTLYNE=
     1                    '"PM LINTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE=
     1                    'GREATER THAN 0.0 AND LESS THAN OR EQUAL TO 1.0'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      LINTOL=W1
                  ELSE
C       NOT LINTOL
                  END IF
                  IF(WQ.EQ.'ONTOL') THEN
                      IF(W1.LE.0.0D0) THEN
                          OUTLYNE=
     1                    '"PM ONTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.GE.1.0D0) THEN
                          OUTLYNE=
     1                    'THE "ONTOL" VALUE MUST BE LESS THAN 1.0'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      ONTOL=W1
                  ELSE
C       NOT ONTOL
                  END IF
                  IF(WQ.EQ.'SINGTOL') THEN
                      IF(W1.LE.0.0D0) THEN
                          OUTLYNE=
     1                    '"PM SINGTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(W1.GE.1.0D0) THEN
                          OUTLYNE=
     1                    'THE "SINGTOL" VALUE MUST BE LESS THAN 1.0'
                          CALL SHOWIT(1)
                          OUTLYNE='RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      SINGTOL=W1
C       NOT SINGTOL
                  END IF
                  WRITE(OUTLYNE,2001) WQ,W1
                  CALL SHOWIT(0)
                  RETURN
              END IF
              RETURN
 1001         FORMAT(A8,' RESET TO ',I5)
 2001         FORMAT(A8,' RESET TO ',G18.10)
          ELSE
C       NOT PM
          END IF
C
          IF(WC.EQ.'PMP') THEN
C
              IF(WQ.NE.'SURTOL'.AND.WQ.NE.'AIMTOL'.AND.WQ.NE.'SERINC'
     1        .AND.WQ.NE.'PFAC'.AND.WQ.NE.'MRAYS'.AND.WQ.NE.'CAIMTOL'
     2        .AND.WQ.NE.'DINMUL'.AND.WQ.NE.'NRAITR'
     3        .AND.WQ.NE.'DIFTOL'.AND.WQ.NE.'DELSUR'.AND.WQ.NE.'ONTOL'.AND.
     4        WQ.NE.'SINGTOL'.AND.WQ.NE.'LINTOL' .AND.
     5        WQ.NE.'SERLIM'.AND.WQ.NE.'SAGDEL'.AND.WQ.NE.'MAXREG'.AND.
     6        WQ.NE.'MAXOPT') THEN
                  OUTLYNE='INVALID QUALIFIER USED WITH "PMP" COMMAND'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SN.EQ.1.OR.SST.EQ.1) THEN
                  OUTLYNE='PMP '//WQ//'TAKES NO NUMERIC OR STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'NRAITR') THEN
                  IF(WQ.EQ.'NRAITR') IVAL=NRAITR
                  WRITE(OUTLYNE,1000) WQ,IVAL
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(WQ.EQ.'MRAYS') THEN
                  IF(WQ.EQ.'MRAYS') IVAL=MRAYS
                  WRITE(OUTLYNE,1000) WQ,IVAL
                  CALL SHOWIT(0)
                  RETURN
              ELSE
              END IF
              IF(WQ.EQ.'SURTOL'.OR.WQ.EQ.'AIMTOL'.OR.WQ.EQ.'SERINC'
     1        .OR.WQ.EQ.'PFAC'.OR.WQ.EQ.'CAIMTOL'
     2        .OR.WQ.EQ.'DINMUL'.OR.WQ.EQ.'DIFTOL'.OR.WQ.EQ.'LINTOL'
     3        .OR.WQ.EQ.'DELSUR'.OR.WQ.EQ.'ONTOL'.OR.WQ.EQ.'SINGTOL'
     4        .OR.WQ.EQ.'SERLIM'.OR.WQ.EQ.'SAGDEL'.OR.WQ.EQ.'MAXREG'.OR.
     5        WQ.EQ.'MAXOPT') THEN
                  IF(WQ.EQ.'LINTOL') VAL=LINTOL
                  IF(WQ.EQ.'SURTOL') VAL=SURTOL
                  IF(WQ.EQ.'AIMTOL') VAL=AIMTOL
                  IF(WQ.EQ.'CAIMTOL') VAL=CAIMTOL
                  IF(WQ.EQ.'MAXREG') VAL=MAXREG
                  IF(WQ.EQ.'MAXOPT') VAL=MAXOPT
                  IF(WQ.EQ.'SERINC') VAL=SERINC
                  IF(WQ.EQ.'SERLIM') VAL=SERLIM
                  IF(WQ.EQ.'PFAC') VAL=PFAC
                  IF(WQ.EQ.'DINMUL') VAL=DINMUL
                  IF(WQ.EQ.'DIFTOL') VAL=DIFTOL
                  IF(WQ.EQ.'DELSUR') VAL=DELSUR
                  IF(WQ.EQ.'SAGDEL') VAL=SAGDEL
                  IF(WQ.EQ.'ONTOL') VAL=ONTOL
                  IF(WQ.EQ.'SINGTOL') VAL=SINGTOL
                  WRITE(OUTLYNE,2000) WQ,VAL
                  CALL SHOWIT(0)
                  IF(WQ.EQ.'MAXOPT') THEN
                      WRITE(OUTLYNE,2000) 'MAXVB   ',MAXVB
                      CALL SHOWIT(0)
                  END IF
                  RETURN
              END IF
 1000         FORMAT(A8,' = ',I10)
 2000         FORMAT(A8,' = ',G18.10)

C       NOT PMP
          END IF
          IF(WC.EQ.'OPCON') THEN
              SURTOL=1.0D-10
              AIMTOL=1.0D-10
              CAIMTOL=0.001D0
              MRAYS=200
              DEALLOCATE(
     1        P1ARAY,STAT=ALLOERR)
              ALLOCATE(
     1        P1ARAY(0:360,1:3,1:MRAYS),
     1        STAT=ALLOERR)
              MAXOPT=4000
              MAXREG=50000
              MAXVB=4000
              MAXTVB=MAXVB-MAXCMP
              SAGDEL=1.0D-3
              SERINC=1.0D-2
              SERLIM=10.0D0
              PFAC=1.0D0
              DINMUL=1.0D0
              LINTOL=0.1D0
              DELSUR=0.0025D0
C       DIFTOL IS THE DEFAULT INCREMENT FOR THE REAL RAYTRACE
C       SIMULATION OF THE DIFFERENTIAL RAY TRACE. THE DEFAULT VALUE
C       IS SET AT 1.0D-3 OR THE REFERENCE APERTURE HT AT THE REFERENCE SURFACE
              DIFTOL=1.0D-3
              ONTOL=1.0D-10
              SINGTOL=1.0D-12
C
              NRAITR=100
              ASIAD=0
              ASITD=0
              RETURN
          ELSE
C       NOT OPCON
          END IF
      END



C SUB OUTPUT2.FOR
      SUBROUTINE OUTPUT2
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO SET THE CURRENT OUTPUT DEVICE.
C
C
C       VALID QUALIFIER WORDS ARE:
C
C               T = KEYBOARD + FILE IN STRING (UNIT 97) THIS IS OUT = 69
C               FILE = FILE IN STRING = 97 THIS IS OUT = 97
C
!        LOGICAL OPEN7,OPEN96,OPEN97,EXIS96,EXIS97
C
!        LOGICAL EXIS7,EXIS8,EXIS9,EXIS10
C
!        INTEGER OLD

          LOGICAL EXIS97
C
          INCLUDE 'datmai.inc'
C
          IF(SQ.EQ.1.AND.SST.EQ.0) THEN
              OUTLYNE='A FILE NAME IS REQUIRED FOR THIS FORM OF "OUTPUT"'
              CALL SHOWIT(5)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.SST.EQ.1) THEN
              LASTFIL=OFILN
              OFILN=WS(1:80)
          END IF
          IF(WQ.EQ.'T')   OUT=69
          IF(WQ.EQ.'FILE')OUT=97
C
C     OUTPUT T PLUS FILE NAME
C
          IF(WQ.EQ.'T'.OR.WQ.EQ.'FILE') THEN
              IF(IN.NE.8) CALL CLOSE_FILE(8,1)
              IF(IN.NE.9) CALL CLOSE_FILE(9,1)
              IF(IN.NE.10)CALL CLOSE_FILE(10,1)
              IF(IN.NE.40)CALL CLOSE_FILE(40,1)
              IF(WQ.EQ.'T')OUT=69
              IF(WQ.EQ.'FILE')OUT=97
              IF(WQ.EQ.'FILE    '.OR.WQ.EQ.'T       ') THEN
                  EXIS97=.FALSE.
                  INQUIRE(FILE=trim(HOME)//OFILN,EXIST=EXIS97)
                  IF(APPEND)
     1              OPEN(UNIT=97,ACCESS='APPEND',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//OFILN
     2              ,STATUS='UNKNOWN',ERR=314)
                  IF(.NOT.APPEND)
     1              OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=OFILN
     2              ,STATUS='UNKNOWN',ERR=314)
              END IF
              RETURN
          ELSE
          END IF
          RETURN
 314      CONTINUE
          OUTLYNE='WARNING:'
          CALL SHOWIT(1)
          OUTLYNE='FILE COULD NOT BE OPENED'
          CALL SHOWIT(1)
          OUTLYNE='CHECK YOUR TYPING'
          CALL SHOWIT(1)
      END


C SUB APPREP.FOR
      SUBROUTINE APPREP
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO SET FILES TO APPEND OR SEQ.
C
          LOGICAL OLDAPP
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'dathgr.inc'
C
          IF(WC.EQ.'REPLACE'.AND.STI.EQ.1) THEN
              OUTLYNE=
     1        '"REPLACE" MAKES INPUT/OUTPUT ASCII FILES SEQUENTIAL'
              CALL SHOWIT(5)
              IF(APPEND) OUTLYNE=
     1        '"APPEND" IS CURRENTLY IN EFFECT'
              IF(.NOT.APPEND) OUTLYNE=
     1        '"REPLACE" IS CURRENTLY IN EFFECT'
              CALL SHOWIT(5)
              RETURN
          END IF
          IF(WC.EQ.'APPEND'.AND.STI.EQ.1) THEN
              OUTLYNE=
     1        '"APPEND" MAKES INPUT/OUTPUT ASCII FILES APPENDED'
              CALL SHOWIT(5)
              IF(APPEND) OUTLYNE=
     1        '"APPEND" IS CURRENTLY IN EFFECT'
              IF(.NOT.APPEND) OUTLYNE=
     1        '"REPLACE" IS CURRENTLY IN EFFECT'
              CALL SHOWIT(5)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE=
     1        '"APPEND" AND "REPLACE" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(5)
              CALL MACFAL
              RETURN
          END IF
          OLDAPP=APPEND
          IF(WC.EQ.'APPEND') APPEND=.TRUE.
          IF(WC.EQ.'REPLACE') APPEND=.FALSE.
          IF(.NOT.OLDAPP.AND.APPEND.OR.OLDAPP.AND..NOT.APPEND) THEN
C     TRASH THE FILES
              OPEN(UNIT=7,BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'PRINTER.TXT'
     2        ,STATUS='UNKNOWN')
              IF(IN.NE.7) CALL CLOSE_FILE(7,0)
! 35   FORMAT(A2)
              IF(APPEND) OPEN(UNIT=8,BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2        ,STATUS='UNKNOWN')
              IF(APPEND.AND.IN.NE.8) CALL CLOSE_FILE(8,0)
              IF(APPEND) OPEN(UNIT=9,BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'EDITTEXT.DAT'
     2        ,STATUS='UNKNOWN')
              IF(APPEND.AND.IN.NE.9) CALL CLOSE_FILE(9,0)
              IF(APPEND.AND.IN.NE.97) CALL CLOSE_FILE(97,0)
              OPEN(UNIT=10,BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'PUNCH.DAT'
     2        ,STATUS='UNKNOWN')
              IF(APPEND.AND.IN.NE.10) CALL CLOSE_FILE(10,0)
          END IF
          RETURN
      END


C SUB OUTPUT.FOR
      SUBROUTINE OUTPUT
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO SET THE CURRENT OUTPUT DEVICE.
C
C
C       VALID QUALIFIER WORDS ARE:
C
C               TP = KEYBOARD = 6
C               TP = KEYBOARD + FILE = 69
C               LP = LINE PRINTER = PRINTER.TXT = 7
C               CP = CARDTEXT.DAT = 8
C               ED = EDITTEXT.DAT = 9
C               PU = PUNCH.DAT = 10
C               NAMED FILE = 97
C               NULL=NOWHERE.DAT=98
C
!        LOGICAL OPEN7,OPEN97
C
          LOGICAL EXIS7,EXIS8,EXIS9,EXIS10,EXIS97
C
!        INTEGER OLD
C
          INCLUDE 'datmai.inc'
C
          IF(WQ.NE.'LP'.AND.
     1    WQ.NE.'ED'.AND.WQ.NE.'CP'
     2    .AND.WQ.NE.'PU'.AND.WQ.NE.'TP'.AND.SQ.NE.0
     1    .AND.WQ.NE.'NULL') THEN
              OUTLYNE='INVALID OUTPUT DEVICE NAME'
              CALL SHOWIT(5)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0.OR.STI.EQ.1) THEN
C HANDEL PRINTING OF NAME OF CURRENT DEVICE
              IF(OUT.EQ.69)  THEN
                  OUTLYNE='OUTPUT IS "T" PLUS FILE NAME = '//OFILN
                  CALL SHOWIT(5)
                  RETURN
              END IF
              IF(OUT.EQ.97)  THEN
                  OUTLYNE='OUTPUT IS TO FILE NAME = '//OFILN
                  CALL SHOWIT(5)
                  RETURN
              END IF
              IF(OUT.EQ.6)  THEN
                  OUTLYNE='OUTPUT IS "TP"'
                  CALL SHOWIT(5)
                  RETURN
              END IF
              IF(OUT.EQ.98)  THEN
                  OUTLYNE='OUTPUT IS "NULL"'
                  CALL SHOWIT(5)
                  RETURN
              END IF
              IF(OUT.EQ.7)  THEN
                  OUTLYNE='OUTPUT IS "LP"'
                  CALL SHOWIT(5)
                  RETURN
              END IF
              IF(OUT.EQ.8)  THEN
                  OUTLYNE='OUTPUT IS "CP"'
                  CALL SHOWIT(5)
                  RETURN
              END IF
              IF(OUT.EQ.9)  THEN
                  OUTLYNE='OUTPUT IS "ED"'
                  CALL SHOWIT(5)
                  RETURN
              END IF
              IF(OUT.EQ.10) THEN
                  OUTLYNE='OUTPUT IS "PU"'
                  CALL SHOWIT(5)
                  RETURN
              END IF
          END IF
C
C     OUTPUT TP
C
C
          IF(WQ.EQ.'TP') THEN
              IF(CMDLINE(1:5).EQ.'BATCH') RETURN
              IF(IN.NE.8) CALL CLOSE_FILE(8,1)
              IF(IN.NE.9) CALL CLOSE_FILE(9,1)
              IF(IN.NE.10)CALL CLOSE_FILE(10,1)
              IF(IN.NE.40)CALL CLOSE_FILE(40,1)
              IF(IN.NE.96)CALL CLOSE_FILE(96,1)
              IF(IN.NE.97)CALL CLOSE_FILE(97,1)
              LASTFIL=OFILN
              OFILN='            '
              OUT=6
! 35   FORMAT(A2)
              RETURN
          ELSE
C     NOT OUT TP
          END IF
          IF(WQ.EQ.'LP') THEN
              IF(IN.NE.8) CALL CLOSE_FILE(8,1)
              IF(IN.NE.9) CALL CLOSE_FILE(9,1)
              IF(IN.NE.10)CALL CLOSE_FILE(10,1)
              IF(IN.NE.40)CALL CLOSE_FILE(40,1)
              IF(IN.NE.96)CALL CLOSE_FILE(96,1)
              IF(IN.NE.97)CALL CLOSE_FILE(97,1)
              LASTFIL=OFILN
              OFILN='            '
              OUT=7
              EXIS7=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PRINTER.TXT',EXIST=EXIS7)
              IF(APPEND)
     1          OPEN(UNIT=7,ACCESS='APPEND',BLANK='NULL'
     1          ,FORM='FORMATTED',FILE=trim(HOME)//'PRINTER.TXT'
     2          ,STATUS='UNKNOWN')
              IF(.NOT.APPEND)
     1          OPEN(UNIT=7,ACCESS='SEQUENTIAL',BLANK='NULL'
     1          ,FORM='FORMATTED',FILE=trim(HOME)//'PRINTER.TXT'
     2          ,STATUS='UNKNOWN')
              RETURN
          ELSE
          END IF
          IF(WQ.EQ.'FILE    '.OR.WQ.EQ.'T       ') THEN
              IF(IN.NE.8) CALL CLOSE_FILE(8,1)
              IF(IN.NE.9) CALL CLOSE_FILE(9,1)
              IF(IN.NE.10)CALL CLOSE_FILE(10,1)
              IF(IN.NE.40)CALL CLOSE_FILE(40,1)
              IF(IN.NE.96)CALL CLOSE_FILE(96,1)
              IF(IN.NE.97)CALL CLOSE_FILE(97,1)
              LASTFIL=OFILN
              OFILN='            '
              OUT=7
              EXIS97=.FALSE.
              OFILN=WS(1:80)
              INQUIRE(FILE=OFILN,EXIST=EXIS97)
              IF(APPEND)
     1          OPEN(UNIT=97,ACCESS='APPEND',BLANK='NULL'
     1          ,FORM='FORMATTED',FILE=trim(HOME)//'OFILN'
     2          ,STATUS='UNKNOWN')
              IF(.NOT.APPEND)
     1          OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'
     1          ,FORM='FORMATTED',FILE=trim(HOME)//OFILN
     2          ,STATUS='UNKNOWN')
              RETURN
          ELSE
          END IF
          IF(WQ.EQ.'CP') THEN
              IF(IN.NE.9) CALL CLOSE_FILE(9,1)
              IF(IN.NE.10)CALL CLOSE_FILE(10,1)
              IF(IN.NE.40)CALL CLOSE_FILE(40,1)
              IF(IN.NE.96)CALL CLOSE_FILE(96,1)
              IF(IN.NE.97)CALL CLOSE_FILE(97,1)
              LASTFIL=OFILN
              OFILN='            '
              OUT=8
              EXIS8=.FALSE.
 222          CONTINUE
              INQUIRE(FILE=trim(HOME)//'CARDTEXT.DAT',EXIST=EXIS8)
              IF(APPEND) OPEN(UNIT=8,ACCESS='APPEND',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2        ,STATUS='UNKNOWN',ERR=111)
              IF(.NOT.APPEND) OPEN(UNIT=8,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2        ,STATUS='UNKNOWN',ERR=111)
              RETURN
 111          IF(APPEND) OPEN(UNIT=8,FILE=trim(HOME)//'CARDTEXT.DAT',
     1        STATUS='UNKNOWN',ACCESS='APPEND')
              IF(.NOT.APPEND)
     1        OPEN(UNIT=8,FILE=trim(HOME)//'CARDTEXT.DAT',STATUS='UNKNOWN'
     1        ,ACCESS='SEQUENTIAL')
              IF(IN.NE.8) CALL CLOSE_FILE(8,0)
              GO TO 222
          ELSE
          END IF
          IF(WQ.EQ.'ED') THEN
              IF(IN.NE.8) CALL CLOSE_FILE(8,1)
              IF(IN.NE.10)CALL CLOSE_FILE(10,1)
              IF(IN.NE.40)CALL CLOSE_FILE(40,1)
              IF(IN.NE.96)CALL CLOSE_FILE(96,1)
              IF(IN.NE.97)CALL CLOSE_FILE(97,1)
              LASTFIL=OFILN
              OFILN='            '
              OUT=9
              EXIS9=.FALSE.
 444          CONTINUE
              INQUIRE(FILE=trim(HOME)//'EDITTEXT.DAT',EXIST=EXIS9)
              IF(APPEND) OPEN(UNIT=9,ACCESS='APPEND',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'EDITTEXT.DAT'
     2        ,STATUS='UNKNOWN',ERR=333)
              IF(.NOT.APPEND) OPEN(UNIT=9,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'EDITTEXT.DAT'
     2        ,STATUS='UNKNOWN',ERR=333)
              RETURN
 333          IF(APPEND) OPEN(UNIT=9,FILE=trim(HOME)//'EDITTEXT.DAT',
     1        STATUS='UNKNOWN',ACCESS='APPEND')
              IF(.NOT.APPEND)
     1        OPEN(UNIT=9,FILE=trim(HOME)//'EDITTEXT.DAT',STATUS='UNKNOWN'
     1        ,ACCESS='SEQUENTIAL')
              IF(IN.NE.9) CALL CLOSE_FILE(9,0)
              GO TO 444
          ELSE
          END IF
          IF(WQ.EQ.'NULL') THEN
              IF(IN.NE.8) CALL CLOSE_FILE(8,1)
              IF(IN.NE.9) CALL CLOSE_FILE(9,1)
              IF(IN.NE.10)CALL CLOSE_FILE(10,1)
              IF(IN.NE.40)CALL CLOSE_FILE(40,1)
              IF(IN.NE.96)CALL CLOSE_FILE(96,1)
              IF(IN.NE.97)CALL CLOSE_FILE(97,1)
              LASTFIL=OFILN
              OFILN='            '
              OUT=98
              RETURN
          ELSE
          END IF
          IF(WQ.EQ.'PU') THEN
              IF(IN.NE.8) CALL CLOSE_FILE(8,1)
              IF(IN.NE.9) CALL CLOSE_FILE(9,1)
              IF(IN.NE.40)CALL CLOSE_FILE(40,1)
              IF(IN.NE.96)CALL CLOSE_FILE(96,1)
              IF(IN.NE.97)CALL CLOSE_FILE(97,1)
              LASTFIL=OFILN
              OFILN='            '
              OUT=10
              EXIS10=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PUNCH.DAT',EXIST=EXIS10)
 666          IF(APPEND) OPEN(UNIT=10,ACCESS='APPEND',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'PUNCH.DAT'
     2        ,STATUS='UNKNOWN',ERR=555)
              IF(.NOT.APPEND) OPEN(UNIT=10,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'PUNCH.DAT'
     2        ,STATUS='UNKNOWN',ERR=555)
              RETURN
 555          IF(APPEND) OPEN(UNIT=9,FILE=trim(HOME)//'PUNCH.DAT',
     1        STATUS='UNKNOWN',ACCESS='APPEND')
              IF(.NOT.APPEND)
     1        OPEN(UNIT=9,FILE=trim(HOME)//'PUNCH.DAT',STATUS='UNKNOWN'
     1        ,ACCESS='SEQUENTIAL')
              IF(IN.NE.10) CALL CLOSE_FILE(10,0)
              GO TO 666
          ELSE
          END IF
          RETURN
      END


      SUBROUTINE PARAMOUT(IPAR,TEMPCC,ZMXERROR)
          IMPLICIT NONE
          INTEGER IPAR,J
          REAL*8 VALV
          LOGICAL ZMXERROR
          CHARACTER TEMPCC*1024,TEMPER*1024,VALA*23,AA23*23
          INCLUDE 'datmai.inc'
          AA23='                       '
C******************************************************************************
C     PARAMETERS
C     PULL OUT PARAMETER NUMBER
          TEMPER=TEMPCC(4:1024)//'    '
C
C     STRIP UP TO 10 LEFT BLANKS
          DO J=1,10
              IF(TEMPER(1:1).NE.' ') THEN
                  TEMPER(1:1024)=TEMPER(2:1024)//' '
              ELSE
              END IF
          END DO
C     FIND BLANK AT END OF PARAMETER NUMBER
          J=1
          DO WHILE (TEMPER(J:J).NE.' ')
              J=J+1
          END DO
C     J REPRESENTS A BLANK TO THE RIGHT OF THE PARAMETER VALUE
          VALA=AA23
          VALA=TEMPER(1:J-1)
          CALL RIGHTJUST(VALA)
          CALL ATOIZMX(VALA,IPAR,ZMXERROR)
          IF(ZMXERROR) RETURN
C     IVAL IS PARAMETER NUMBER, NOW STRIP OUT PARAMETER VALUE
C
C     STRIP UP TO 10 LEFT BLANKS
          DO J=1,10
              IF(TEMPER(1:1).NE.' ') THEN
                  TEMPER(1:1024)=TEMPER(2:1024)//' '
              ELSE
              END IF
          END DO
          VALA=TEMPER(1:23)
          CALL RIGHTJUST(VALA)
          CALL ATODZMX(VALA,VALV,ZMXERROR)
          IF(ZMXERROR) RETURN
C     VALV IS PARAMETER VALUE
C                  FINISHED BREAKING OUT PARAMETER NUMBER AND VALUES
C******************************************************************************
          RETURN
      END
C SUB WWORD.FOR
      SUBROUTINE WWORD
C
          IMPLICIT NONE
C
C       THIS DOES THE "W1" THROUGH "W5" CMD LEVEL COMMANDS
C
          INTEGER VALW,WWCODE
C
          LOGICAL QQ
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              IF(W1CODE.NE.0) WRITE(OUTLYNE,1000) W1CODE
              CALL SHOWIT(0)
              IF(W1CODE.EQ.0) WRITE(OUTLYNE,1001)
              CALL SHOWIT(0)
              IF(W2CODE.NE.0) WRITE(OUTLYNE,2000) W2CODE
              CALL SHOWIT(0)
              IF(W2CODE.EQ.0) WRITE(OUTLYNE,2001)
              CALL SHOWIT(0)
              IF(W3CODE.NE.0) WRITE(OUTLYNE,3000) W3CODE
              CALL SHOWIT(0)
              IF(W3CODE.EQ.0) WRITE(OUTLYNE,3001)
              CALL SHOWIT(0)
              IF(W4CODE.NE.0) WRITE(OUTLYNE,4000) W4CODE
              CALL SHOWIT(0)
              IF(W4CODE.EQ.0) WRITE(OUTLYNE,4001)
              CALL SHOWIT(0)
              IF(W5CODE.NE.0) WRITE(OUTLYNE,5000) W5CODE
              CALL SHOWIT(0)
              IF(W5CODE.EQ.0) WRITE(OUTLYNE,5001)
              CALL SHOWIT(0)
 1001         FORMAT('THE "W1" POINTER IS CURRENTLY NOT DEFINED')
 2001         FORMAT('THE "W2" POINTER IS CURRENTLY NOT DEFINED')
 3001         FORMAT('THE "W3" POINTER IS CURRENTLY NOT DEFINED')
 4001         FORMAT('THE "W4" POINTER IS CURRENTLY NOT DEFINED')
 5001         FORMAT('THE "W5" POINTER IS CURRENTLY NOT DEFINED')
 1000         FORMAT('THE "W1" POINTER IS CURRENTLY = ',I6)
 2000         FORMAT('THE "W2" POINTER IS CURRENTLY = ',I6)
 3000         FORMAT('THE "W3" POINTER IS CURRENTLY = ',I6)
 4000         FORMAT('THE "W4" POINTER IS CURRENTLY = ',I6)
 5000         FORMAT('THE "W5" POINTER IS CURRENTLY = ',I6)

C     DISPLAY CURRENT SETTINGS
              RETURN
          ELSE
          END IF
          IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0.OR.DF5.EQ.0) THEN
              OUTLYNE=
     1        '"'//WC(1:2)//'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"'//WC(1:2)//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              OUTLYNE=
     1        '"'//WC(1:2)//'" TAKES EITHER NUMERIC OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'BUT NOT BOTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SQ.EQ.1.AND.S1.EQ.0) THEN
C
              QQ=.FALSE.
              IF(WQ.EQ.'A') QQ=.TRUE.
              IF(WQ.EQ.'B') QQ=.TRUE.
              IF(WQ.EQ.'C') QQ=.TRUE.
              IF(WQ.EQ.'D') QQ=.TRUE.
              IF(WQ.EQ.'E') QQ=.TRUE.
              IF(WQ.EQ.'F') QQ=.TRUE.
              IF(WQ.EQ.'G') QQ=.TRUE.
              IF(WQ.EQ.'H') QQ=.TRUE.
              IF(WQ.EQ.'I') QQ=.TRUE.
              IF(WQ.EQ.'J') QQ=.TRUE.
              IF(WQ.EQ.'K') QQ=.TRUE.
              IF(WQ.EQ.'L') QQ=.TRUE.
              IF(WQ.EQ.'M') QQ=.TRUE.
              IF(WQ.EQ.'N') QQ=.TRUE.
              IF(WQ.EQ.'ITEST') QQ=.TRUE.
              IF(WQ.EQ.'JTEST') QQ=.TRUE.
              IF(WQ.EQ.'KTEST') QQ=.TRUE.
              IF(WQ.EQ.'LTEST') QQ=.TRUE.
              IF(WQ.EQ.'MTEST') QQ=.TRUE.
              IF(WQ.EQ.'NTEST') QQ=.TRUE.
              IF(WQ.EQ.'X') QQ=.TRUE.
              IF(WQ.EQ.'Y') QQ=.TRUE.
              IF(WQ.EQ.'Z') QQ=.TRUE.
              IF(WQ.EQ.'T') QQ=.TRUE.
              IF(WQ.EQ.'IX') QQ=.TRUE.
              IF(WQ.EQ.'IY') QQ=.TRUE.
              IF(WQ.EQ.'IZ') QQ=.TRUE.
              IF(WQ.EQ.'IT') QQ=.TRUE.
              IF(WQ.EQ.'ACC') QQ=.TRUE.
              IF(.NOT.QQ) THEN
                  OUTLYNE='INVALID QUALIFIER USED WITH "'//WC(1:2)//'" COMMAND'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          ELSE
C     NO QUALIFIER
          END IF
          IF(S1.EQ.1.AND.SQ.EQ.0) THEN
              VALW=INT(W1)
              IF(VALW.LT.0.OR.VALW.GT.400) THEN
                  WRITE(OUTLYNE,99) INT(VALW)
 99               FORMAT('GENERAL PURPOSE STORAGE REGISTER ',I6,' DOES NOT EXIST')
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          WWCODE=0
          IF(SQ.EQ.0.AND.DF1.EQ.1) WWCODE=-92
          IF(WQ.EQ.'A') WWCODE=-100
          IF(WQ.EQ.'B') WWCODE=-99
          IF(WQ.EQ.'C') WWCODE=-98
          IF(WQ.EQ.'D') WWCODE=-97
          IF(WQ.EQ.'E') WWCODE=-96
          IF(WQ.EQ.'F') WWCODE=-95
          IF(WQ.EQ.'G') WWCODE=-94
          IF(WQ.EQ.'H') WWCODE=-93
          IF(WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    WWCODE=-92
          IF(WQ.EQ.'Y') WWCODE=-91
          IF(WQ.EQ.'Z') WWCODE=-90
          IF(WQ.EQ.'T') WWCODE=-89
          IF(WQ.EQ.'IX') WWCODE=-88
          IF(WQ.EQ.'IY') WWCODE=-87
          IF(WQ.EQ.'IZ') WWCODE=-86
          IF(WQ.EQ.'IT') WWCODE=-85
          IF(WQ.EQ.'I') WWCODE=-84
          IF(WQ.EQ.'ITEST') WWCODE=-83
          IF(WQ.EQ.'J') WWCODE=-82
          IF(WQ.EQ.'JTEST') WWCODE=-81
          IF(WQ.EQ.'LASTX') WWCODE=-80
          IF(WQ.EQ.'LASTIX') WWCODE=-79
          IF(WQ.EQ.'K') WWCODE=-78
          IF(WQ.EQ.'L') WWCODE=-77
          IF(WQ.EQ.'M') WWCODE=-76
          IF(WQ.EQ.'N') WWCODE=-75
          IF(WQ.EQ.'KTEST') WWCODE=-74
          IF(WQ.EQ.'LTEST') WWCODE=-73
          IF(WQ.EQ.'MTEST') WWCODE=-72
          IF(WQ.EQ.'NTEST') WWCODE=-71
C
          IF(S1.EQ.1) THEN
              WWCODE=INT(W1)
          END IF
          IF(WC.EQ.'W1') W1CODE=WWCODE
          IF(WC.EQ.'W2') W2CODE=WWCODE
          IF(WC.EQ.'W3') W3CODE=WWCODE
          IF(WC.EQ.'W4') W4CODE=WWCODE
          IF(WC.EQ.'W5') W5CODE=WWCODE

          RETURN
      END
C SUB UPPRLN.FOR
      SUBROUTINE UPPRLN
C
C     THIS CAUSES THE CURRENT LENS TO BECOME THE PERMANENT LENS
C     IT IS ONLY CALLED FROM CFG1 JUST BEFORE GOING TO ANOTHER CFG
C
          IMPLICIT NONE
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
C
          IF(F12.EQ.1) THEN
              CALL PTOC
C
C               FINISHED COPYING CFG 1 CURLENS TO PERLENS
C
              F6=1
              F1=0
              F22=1
              LNSTYP=2
              CALL LNSEOS
          ELSE
          END IF
          RETURN
      END
C SUB TABLE.FOR
      SUBROUTINE TABLE
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS "TABLE" AND DOES ALL THE TABLE
C       COMMANDS FROM THE CMD LEVEL.
C
          CHARACTER*12 HCOL(0:9),HCOL2(0:9),
     1    HROW(0:100),HROW2(0:100)
          CHARACTER AI*3,
     1    BLANK*12,HEADL1*132,
     2    HEADL2*132,HEADL3*80,HEADL4*80,BL12*12

          INTEGER NROW,NCOL,
     6    I,J,FROW,FCOL
C
          REAL*8
     1    TABL(0:100,0:9)

          logical ex
C
          COMMON/HDTAB/HCOL,HCOL2,HROW,HROW2
C
          COMMON/TTABLE1/TABL
          LOGICAL TABEXIST
          COMMON/EXISTAB/TABEXIST
C
          INCLUDE 'datmai.inc'
C
          BLANK='            '
          BL12='            '
C
C       "ROWHD","ROWHD2","COLHD" AND "COLHD2" COMMANDS
C
C       CHECK FOR STRING INPUT
C
          IF(WC.EQ.'COLHD'.OR.WC.EQ.'COLHD2'.OR.WC.EQ.'ROWHD'
     1    .OR.WC.EQ.'ROWHD2') THEN
              IF(SST.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' COMMAND REQUIRES EXPLICIT STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C       CHECK FOR QUALIFIER INPUT
C
          IF(WC.EQ.'COLHD'.OR.WC.EQ.'COLHD2'.OR.WC.EQ.'ROWHD'
     1    .OR.WC.EQ.'ROWHD2') THEN
              IF(SQ.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' COMMAND REQUIRES EXPLICIT QUALIFIER WORD INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C       CHECK FOR QUALIFIER INPUT
C
          IF(WC.EQ.'COLHD'.OR.WC.EQ.'COLHD2'.OR.WC.EQ.'ROWDH'
     1    .OR.WC.EQ.'ROWHD2') THEN
              IF(SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC,' COMMAND TAKES NO NUMERIC WORD INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C       "TABLE" COMMAND
C       VALID QUALIFIERS ARE "CLEAR","SETUP","PUT","GET",
C       "PRINT1", "PRINT2","PRINT3", "SAVE" AND "RELOAD"
C
C       CHECK FOR STRING INPUT
C
          IF(WC.EQ.'TABLE'.AND.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"TABLE" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CHECK FOR NO QUALIFIER
C
          IF(WC.EQ.'TABLE'.AND.SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"TABLE" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       CHECK FOR INVALID QUALIFIER
C
          IF(WC.EQ.'TABLE'.AND.SQ.EQ.1) THEN
              IF(WQ.NE.'SETUP'.AND.WQ.NE.'CLEAR'.AND.WQ.NE.'PUT'.AND.
     1        WQ.NE.'GET'.AND.WQ.NE.'PRINT1'.AND.WQ.NE.'SAVE'.AND.WQ.NE.
     2        'RELOAD'.AND.WQ.NE.'PRINT2'.AND.WQ.NE.'PRINT3') THEN
                  WRITE(OUTLYNE,*)
     1            'INVALID QUALIFIER WORD USED WITH "TABLE" COMMAND'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       CHECK FOR NUMERIC INPUT
C
          IF(WC.EQ.'TABLE'.AND.SQ.EQ.1) THEN
              IF(WQ.EQ.'SETUP'.OR.WQ.EQ.'CLEAR'.OR.
     1        WQ.EQ.'PRINT1'.OR.WQ.EQ.'PRINT2'.OR.WQ.EQ.'SAVE'.OR.WQ.EQ.
     2        'RELOAD'.OR.WQ.EQ.'PRINT3') THEN
                  IF(SN.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                '"TABLE SETUP", "TABLE CLEAR", "TABLE PRINT1/2/3"'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                '"TABLE SAVE" AND "TABLE RELOAD" TAKE NO NUMERIC WORD INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C
C       EXTRA NW3,NW4 AND NW5 INPUT
C
          IF(WC.EQ.'TABLE'.AND.SQ.EQ.1) THEN
              IF(WQ.EQ.'GET') THEN
                  IF(DF3.EQ.0.OR.DF4.EQ.0.OR.DF5.EQ.0) THEN
                      WRITE(OUTLYNE,*)
     1                '"TABLE GET"'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'ONLY TAKES NUMERIC WORD 1 AND 2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WQ.EQ.'PUT') THEN
                  IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                      WRITE(OUTLYNE,*)
     1                '"TABLE PUT"'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'TAKES NO NUMERIC WORD 4 OR 5 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C
C       EXPLICIT NW1 AND NW2
C
          IF(WC.EQ.'TABLE'.AND.SQ.EQ.1) THEN
              IF(WQ.EQ.'PUT'.OR.WQ.EQ.'GET') THEN
                  IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                '"TABLE PUT" AND "TABLE GET"'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'REQUIRE EXPLICIT NUMERIC WORD 1 AND 2 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C
C
C       TABLE SETUP
          IF(WC.EQ.'TABLE'.AND.WQ.EQ.'SETUP') THEN
              NCOL=0
              NROW=0
              DO I=0,9
                  DO J=0,100
                      TABL(J,I)=0.0D0
                  END DO
              END DO
              DO I=0,9
                  CALL ITOAA(I,AI)
                  HCOL(I)=' COLUMN '//AI//' '
                  HCOL2(I)='            '
              END DO
              DO I=0,100
                  CALL ITOAA(I,AI)
                  HROW(I)='  ROW  '//AI//'  '
                  HROW2(I)='            '
              END DO
              WRITE(OUTLYNE,*)
     1        'THE NEW "TABLE" HAS BEEN SETUP'
              CALL SHOWIT(1)
              TABEXIST=.TRUE.
              RETURN
          END IF
C
C
C       TABLE CLEAR
          IF(WC.EQ.'TABLE'.AND.WQ.EQ.'CLEAR') THEN
              NCOL=0
              NROW=0
              DO I=0,9
                  DO J=0,100
                      TABL(J,I)=0.0D0
                  END DO
              END DO
              WRITE(OUTLYNE,*)
     1        'THE CURRENT "TABLE" HAS BEEN CLEARED'
              CALL SHOWIT(1)
              RETURN
          END IF
C
C       TABLE PUT
          IF(WC.EQ.'TABLE'.AND.WQ.EQ.'PUT') THEN
              IF(.NOT.TABEXIST) THEN
                  WRITE(OUTLYNE,*) '"TABLE SETUP" MUST BE ISSUED TO SET UP'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'THE TABLE BEFORE DATA MAY BE ENTERED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF

              IF(DF3.EQ.0) THEN
C       EXPLICIT NW3 INPUT, SET TABLE ENTRY TO THAT VALUE
                  TABL(INT(W1),INT(W2))=W3
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD 3 VALUE MOVED TO TABLE ROW ',INT(W1)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '                              TABLE COL ',INT(W2)
                  CALL SHOWIT(1)
              ELSE
C       USE ACCUMULATOR VALUE FOR TABLE ENTRY

                  TABL(INT(W1),INT(W2))=REG(9)
                  WRITE(OUTLYNE,*)
     1            'ACCUMULATOR CONTENTS MOVED TO TABLE ROW ',INT(W1)
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            '                              TABLE COL ',INT(W2)
                  CALL SHOWIT(1)
              END IF
C       SET OCCUPANCY FLAG TO TRUE
              IF(NROW.LT.INT(W1)) NROW=INT(W1)
              IF(NCOL.LT.INT(W2)) NCOL=INT(W2)
              RETURN
          END IF
C
C       TABLE GET
          IF(WC.EQ.'TABLE'.AND.WQ.EQ.'GET') THEN
              IF(.NOT.TABEXIST) THEN
                  WRITE(OUTLYNE,*) 'NO TABLE EXISTS YET'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              REG(40)=REG(9)
              REG(9)=TABL(INT(W1),INT(W2))
              WRITE(OUTLYNE,*)
     1        'CONTENTS OF TABLE ROW ',INT(W1)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        '            TABLE COL ',INT(W2)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'MOVED TO ACCUMULATOR'
              CALL SHOWIT(1)
              RETURN
          END IF
C
C       ROWHD OR ROWHD2
          IF(WC.EQ.'ROWHD') THEN
              I=FROW(WQ)
              IF(NROW.LT.I) NROW=I
              HROW(I)=WS(1:12)
              RETURN
          END IF
          IF(WC.EQ.'ROWHD2') THEN
              I=FROW(WQ)
              IF(NROW.LT.I) NROW=I
              HROW2(I)=WS(1:12)
              RETURN
          END IF
C
C       COLHD OR COLHD2
          IF(WC.EQ.'COLHD') THEN
              I=FCOL(WQ)
              IF(NCOL.LT.I) NCOL=I
              HCOL(I)=WS(1:12)
              RETURN
          END IF
          IF(WC.EQ.'COLHD2') THEN
              I=FCOL(WQ)
              IF(NCOL.LT.I) NCOL=I
              HCOL2(I)=WS(1:12)
              RETURN
          END IF
C
C       TABLE SAVE
          IF(WC.EQ.'TABLE'.AND.WQ.EQ.'SAVE') THEN
              IF(.NOT.TABEXIST) THEN
                  WRITE(OUTLYNE,*) 'NO TABLE EXISTS TO BE SAVED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       OPEN AND WRITE TO FILE
              OPEN(UNIT=36,ACCESS='DIRECT',FILE=trim(HOME)//'TAB.DAT',
     1        FORM='UNFORMATTED',RECL=(132*NRECL),STATUS='UNKNOWN')
C
              WRITE(UNIT=36,REC=1) NROW,NCOL
              WRITE(UNIT=36,REC=2) HCOL(0),HCOL2(0)
              WRITE(UNIT=36,REC=3) HCOL(1),HCOL2(1)
              WRITE(UNIT=36,REC=4) HCOL(2),HCOL2(2)
              WRITE(UNIT=36,REC=5) HCOL(3),HCOL2(3)
              WRITE(UNIT=36,REC=6) HCOL(4),HCOL2(4)
              WRITE(UNIT=36,REC=7) HCOL(5),HCOL2(5)
              WRITE(UNIT=36,REC=8) HCOL(6),HCOL2(6)
              WRITE(UNIT=36,REC=9) HCOL(7),HCOL2(7)
              WRITE(UNIT=36,REC=10) HCOL(8),HCOL2(8)
              WRITE(UNIT=36,REC=11) HCOL(9),HCOL2(9)

              DO 210 I=0,100
                  WRITE(UNIT=36,REC=I+12) HROW(I),HROW2(I),TABL(I,0),
     1            TABL(I,1),TABL(I,2),TABL(I,3),TABL(I,4),TABL(I,5),
     2            TABL(I,6),TABL(I,7),TABL(I,8),TABL(I,9)
 210          CONTINUE
              CALL CLOSE_FILE(36,1)
              WRITE(OUTLYNE,*)
     1        'THE CURRENT "TABLE" HAS BEEN SAVED IN "TAB.DAT"'
              CALL SHOWIT(1)
              RETURN
          END IF
C       TABLE RELOAD
          IF(WC.EQ.'TABLE'.AND.WQ.EQ.'RELOAD') THEN
              inquire(file=trim(HOME)//'TAB.DAT',EXIST=ex)
              if (ex) then
C       OPEN AND READ FROM FILE
                  OPEN(UNIT=36,ACCESS='DIRECT',FILE=trim(HOME)//'TAB.DAT',
     1            FORM='UNFORMATTED',RECL=(132*NRECL),STATUS='UNKNOWN')
C
                  READ(UNIT=36,REC=1) NROW,NCOL
                  READ(UNIT=36,REC=2) HCOL(0),HCOL2(0)
                  READ(UNIT=36,REC=3) HCOL(1),HCOL2(1)
                  READ(UNIT=36,REC=4) HCOL(2),HCOL2(2)
                  READ(UNIT=36,REC=5) HCOL(3),HCOL2(3)
                  READ(UNIT=36,REC=6) HCOL(4),HCOL2(4)
                  READ(UNIT=36,REC=7) HCOL(5),HCOL2(5)
                  READ(UNIT=36,REC=8) HCOL(6),HCOL2(6)
                  READ(UNIT=36,REC=9) HCOL(7),HCOL2(7)
                  READ(UNIT=36,REC=10) HCOL(8),HCOL2(8)
                  READ(UNIT=36,REC=11) HCOL(9),HCOL2(9)

                  DO 310 I=0,100
                      READ(UNIT=36,REC=I+12) HROW(I),HROW2(I),TABL(I,0),
     1                TABL(I,1),TABL(I,2),TABL(I,3),TABL(I,4),TABL(I,5),
     2                TABL(I,6),TABL(I,7),TABL(I,8),TABL(I,9)
 310              CONTINUE
                  CALL CLOSE_FILE(36,1)
                  WRITE(OUTLYNE,*)
     1            'THE CURRENT "TABLE" HAS BEEN RELOADED FROM "TAB.DAT"'
                  CALL SHOWIT(1)
                  TABEXIST=.TRUE.
              else
                  write(OUTLYNE,*) 'TABLE DATA NOT FOUND'
                  call SHOWIT(1)
                  return
              end if
              RETURN
          END IF
C       TABLE PRINT
          IF(WC.EQ.'TABLE'.AND.WQ.EQ.'PRINT1'.OR.
     1    WC.EQ.'TABLE'.AND.WQ.EQ.'PRINT2'.OR.
     3    WC.EQ.'TABLE'.AND.WQ.EQ.'PRINT3') THEN
              IF(.NOT.TABEXIST) THEN
                  WRITE(OUTLYNE,*) 'NO TABLE EXISTS TO BE PRINTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(NCOL.EQ.0.AND.NROW.EQ.0) THEN
                  WRITE(OUTLYNE,*) 'NO TABLE EXISTS TO BE PRINTED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PRINT TABLE DATA
C       HERE THE HEADINGS AND VALUES ARE OUTPUT. 3 FORMS ARE USED,
C       ONE FOR 80 COLS(PRINT1) AND ONE FOR 132 COLS(PRINT2) SPACE DELIMITED
C       AND ONE FOR 132 COLS(PRINT3) COMMA DELIMITED
C
              IF(WQ.EQ.'PRINT1') THEN
C       DATA DOES EXIST TO PRINT
C
C       PRINT FIRST 5 COLUMNS OF THE TABLE
C
C       PRINT COLUMN HEADING
                  HEADL3='             '//HCOL(1)//' '//HCOL(2)//' '//
     1            HCOL(3)//' '//HCOL(4)//' '//HCOL(5)
                  HEADL4='             '//HCOL2(1)//' '//HCOL2(2)//' '//
     1            HCOL2(3)//' '//HCOL2(4)//' '//HCOL2(5)
 1002             FORMAT(A79)
C       PRINT HEADING
                  WRITE(OUTLYNE,1002) HEADL3(1:79)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1002) HEADL4(1:79)
                  CALL SHOWIT(0)
                  DO I=1,NROW
                      WRITE(OUTLYNE,123) HROW(I),TABL(I,1),TABL(I,2)
     1                ,TABL(I,3),TABL(I,4),TABL(I,5)
                      CALL SHOWIT(0)
 123                  FORMAT(A12,5(1X,G12.5))
                      IF(HROW2(I).NE.BL12) THEN
                          WRITE(OUTLYNE,453) HROW2(I)
                          CALL SHOWIT(0)
 453                      FORMAT(A12)
                      END IF
                  END DO
C       PRINT LAST 4 COLUMNS IF NOT BLANK
                  IF(NCOL.GT.5) THEN
C
C       PRINT COLUMN HEADING
                      HEADL3='             '//HCOL(6)//' '//HCOL(7)//' '//
     1                HCOL(8)//' '//HCOL(9)
                      HEADL4='             '//HCOL2(6)//' '//HCOL2(7)//' '//
     1                HCOL2(8)//' '//HCOL2(9)
                      WRITE(OUTLYNE,2222)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,1002) HEADL3(1:79)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,1002) HEADL4(1:79)
                      CALL SHOWIT(0)
                      DO I=1,NROW
C       PRINT EACH ROW INCLUDING ROW HEADING. ZERO PRINTS AS BLANK
C       AND A LINE IS SKIPPED IN THE DATA IF A SECOND LINE OF
C       ROW HEADING DESCRIPTION IS USED.
C
C       PRINT THE LINE
                          WRITE(OUTLYNE,454) HROW(I),TABL(I,6),TABL(I,7),
     1                    TABL(I,8),TABL(I,9)
                          CALL SHOWIT(0)
 454                      FORMAT(A12,4(1X,G12.5))
                          IF(HROW2(I).NE.BL12) THEN
                              WRITE(OUTLYNE,455) HROW2(I)(1:12)
                              CALL SHOWIT(0)
 455                          FORMAT(A12)
                          END IF
                      END DO
                  END IF
                  RETURN
              END IF
C
C       PRINT2
              IF(WQ.EQ.'PRINT2') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      OUTLYNE='"TABLE PRINT2" IS FOR USE WHEN OUTPUTING TO A DISK'
                      CALL SHOWIT(1)
                      OUTLYNE='FILE ONLY'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C       DATA DOES EXIST TO PRINT
C
C       PRINT FIRST 5 COLUMNS OF THE TABLE
C
C       PRINT COLUMN HEADING
                  HEADL1='             '//HCOL(1)//'      '//HCOL(2)//'      '//
     1            HCOL(3)//'      '//HCOL(4)//'      '//HCOL(5)
                  HEADL2='             '//HCOL2(1)//'      '//HCOL2(2)//'      '//
     1            HCOL2(3)//'      '//HCOL2(4)//'      '//HCOL2(5)
 1012             FORMAT(A131)
C       PRINT HEADING
                  WRITE(OUTLYNE,1012) HEADL1(1:131)
                  CALL SHOWIT(2)
                  WRITE(OUTLYNE,1012) HEADL2(1:131)
                  CALL SHOWIT(2)
                  DO I=1,NROW
                      WRITE(OUTLYNE,124) HROW(I),TABL(I,1),TABL(I,2)
     1                ,TABL(I,3),TABL(I,4),TABL(I,5)
                      CALL SHOWIT(2)
 124                  FORMAT(A12,5(1X,G17.10))
                      IF(HROW2(I).NE.BL12) THEN
                          WRITE(OUTLYNE,453) HROW2(I)
                          CALL SHOWIT(2)
                      END IF
                  END DO
C       PRINT LAST 4 COLUMNS IF NOT BLANK
                  IF(NCOL.GT.5) THEN
                      WRITE(OUTLYNE,2222)
                      CALL SHOWIT(2)
C
C       PRINT COLUMN HEADING
                      HEADL1='             '//HCOL(6)//'      '//HCOL(7)//'      '//
     1                HCOL(8)//'      '//HCOL(9)
                      HEADL2='             '//HCOL2(6)//'      '//HCOL2(7)//'      '//
     1                HCOL2(8)//'      '//HCOL2(9)
C       PRINT HEADING
                      WRITE(OUTLYNE,1012) HEADL1(1:131)
                      CALL SHOWIT(2)
                      WRITE(OUTLYNE,1012) HEADL2(1:131)
                      CALL SHOWIT(2)
                      DO I=1,NROW
C       PRINT EACH ROW INCLUDING ROW HEADING. ZERO PRINTS AS BLANK
C       AND A LINE IS SKIPPED IN THE DATA IF A SECOND LINE OF
C       ROW HEADING DESCRIPTION IS USED.
C
C       PRINT THE LINE
                          WRITE(OUTLYNE,456) HROW(I),TABL(I,6),TABL(I,7),
     1                    TABL(I,8),TABL(I,9)
                          CALL SHOWIT(2)
 456                      FORMAT(A12,4(1X,G17.10))
                          IF(HROW2(I).NE.BL12) THEN
                              WRITE(OUTLYNE,454) HROW2(I)(1:12)
                              CALL SHOWIT(2)
                          END IF
                      END DO
                  END IF
                  RETURN
              END IF
C
C        PRINT 3
              IF(WQ.EQ.'PRINT3') THEN
                  IF(OUT.NE.97) THEN
                      OUTLYNE='"TABLE PRINT3" IS FOR USE WHEN OUTPUTING TO A'
                      CALL SHOWIT(1)
                      OUTLYNE='NAMED DISK FILE AS IN "OUT FILE (FILE NAME)"'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C       DATA DOES EXIST TO PRINT
                  DO I=1,NROW
                      WRITE(OUTLYNE_LONG,1124) TABL(I,1),TABL(I,2)
     1                ,TABL(I,3),TABL(I,4),TABL(I,5),TABL(I,6),TABL(I,7)
     2                ,TABL(I,8),TABL(I,9)
                      CALL SHOWIT(20)
 1124                 FORMAT(8(D23.15,','),D23.15)
                  END DO
              END IF
              RETURN
          END IF
 2222     FORMAT(1X)
      END
C SUB FCOL.FOR
      FUNCTION FCOL(WK)
C
          IMPLICIT NONE
C
          CHARACTER WK*8
C
          INTEGER FCOL
C
          INCLUDE 'datmai.inc'
C
          IF(
     1    WK.NE.'C1'.AND.WK.NE.'C2'.AND.WK.NE.'C3'.AND.WK.NE.'C4'.AND.
     2    WK.NE.'C5'.AND.WK.NE.'C6'.AND.WK.NE.'C7'.AND.WK.NE.'C8'.AND.
     3    WK.NE.'C9') THEN
              FCOL=0
              WRITE(OUTLYNE,*)
     1        'INVALID QUALIFIER USED WITH "COLHD" OR "COLHD2"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WK.EQ.'C1') FCOL=1
          IF(WK.EQ.'C2') FCOL=2
          IF(WK.EQ.'C3') FCOL=3
          IF(WK.EQ.'C4') FCOL=4
          IF(WK.EQ.'C5') FCOL=5
          IF(WK.EQ.'C6') FCOL=6
          IF(WK.EQ.'C7') FCOL=7
          IF(WK.EQ.'C8') FCOL=8
          IF(WK.EQ.'C9') FCOL=9
          RETURN
      END
C SUB FROW.FOR
      FUNCTION FROW(WK)
C
          IMPLICIT NONE
C
          CHARACTER WK*8
C
          INTEGER FROW
C
          INCLUDE 'datmai.inc'
C
          IF(WK.NE.'R1'.AND.WK.NE.'R2'.AND.WK.NE.'R3'.AND.WK.NE.'R4'.AND.
     2    WK.NE.'R5'.AND.WK.NE.'R6'.AND.WK.NE.'R7'.AND.WK.NE.'R8'.AND.
     2    WK.NE.'R9'.AND.WK.NE.'R10'.AND.WK.NE.'R11'.AND.WK.NE.'R12'.AND.
     2    WK.NE.'R13'.AND.WK.NE.'R14'.AND.WK.NE.'R15'.AND.WK.NE.'R16'.AND.
     2    WK.NE.'R17'.AND.WK.NE.'R18'.AND.WK.NE.'R19'.AND.WK.NE.'R16'.AND.
     2    WK.NE.'R21'.AND.WK.NE.'R22'.AND.WK.NE.'R23'.AND.WK.NE.'R24')THEN
              GO TO 4
          END IF
          GO TO 100
 4        IF(WK.NE.'R25'.AND.WK.NE.'R26'.AND.WK.NE.'R27'.AND.WK.NE.'R28'
     2     .AND.
     2     WK.NE.'R29'.AND.WK.NE.'R30'.AND.WK.NE.'R31'.AND.WK.NE.'R32'.AND.
     2     WK.NE.'R33'.AND.WK.NE.'R34'.AND.WK.NE.'R35'.AND.WK.NE.'R36'.AND.
     2     WK.NE.'R37'.AND.WK.NE.'R38'.AND.WK.NE.'R39'.AND.WK.NE.'R40')THEN
              GO TO 5
          END IF
          GO TO 100
 5        IF(WK.NE.'R41'.AND.WK.NE.'R42'.AND.WK.NE.'R43'.AND.WK.NE.'R44'
     1    .AND.
     2    WK.NE.'R45'.AND.WK.NE.'R46'.AND.WK.NE.'R47'.AND.WK.NE.'R48'.AND.
     2    WK.NE.'R49'.AND.WK.NE.'R50'.AND.WK.NE.'R51'.AND.WK.NE.'R52'.AND.
     2    WK.NE.'R53'.AND.WK.NE.'R54'.AND.WK.NE.'R55'.AND.WK.NE.'R56'.AND.
     2    WK.NE.'R57'.AND.WK.NE.'R58'.AND.WK.NE.'R59'.AND.WK.NE.'R60'.AND.
     2    WK.NE.'R61'.AND.WK.NE.'R62'.AND.WK.NE.'R63'.AND.WK.NE.'R64'.AND.
     2    WK.NE.'R65'.AND.WK.NE.'R66'.AND.WK.NE.'R67'.AND.WK.NE.'R68'.AND.
     2    WK.NE.'R69'.AND.WK.NE.'R70'.AND.WK.NE.'R71'.AND.WK.NE.'R72'.AND.
     2    WK.NE.'R73'.AND.WK.NE.'R74'.AND.WK.NE.'R75'.AND.WK.NE.'R76')THEN
              GO TO 10
          END IF
          GO TO 100
 10       IF(
     2    WK.NE.'R77'.AND.WK.NE.'R78'.AND.WK.NE.'R79'.AND.WK.NE.'R80'.AND.
     2    WK.NE.'R81'.AND.WK.NE.'R82'.AND.WK.NE.'R83'.AND.WK.NE.'R84'.AND.
     2    WK.NE.'R85'.AND.WK.NE.'R86'.AND.WK.NE.'R87'.AND.WK.NE.'R88'.AND.
     2    WK.NE.'R89'.AND.WK.NE.'R90'.AND.WK.NE.'R91'.AND.WK.NE.'R92'.AND.
     2    WK.NE.'R93'.AND.WK.NE.'R94'.AND.WK.NE.'R95'.AND.WK.NE.'R96'.AND.
     2    WK.NE.'R97'.AND.WK.NE.'R98'.AND.WK.NE.'R99'.AND.WK.NE.'R100'
     2    )THEN
              GO TO 15
          END IF
          GO TO 100
 15       WRITE(OUTLYNE,*)
     1    'INVALID QUALIFIER USED WITH "ROWHD" OR "ROWHD2"'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          frow=0
          RETURN

 100      CONTINUE
          FROW=0
C
          IF(WK.EQ.'R1')  FROW=1
          IF(WK.EQ.'R2')  FROW=2
          IF(WK.EQ.'R3')  FROW=3
          IF(WK.EQ.'R4')  FROW=4
          IF(WK.EQ.'R5')  FROW=5
          IF(WK.EQ.'R6')  FROW=6
          IF(WK.EQ.'R7')  FROW=7
          IF(WK.EQ.'R8')  FROW=8
          IF(WK.EQ.'R9')  FROW=9
          IF(WK.EQ.'R10') FROW=10
          IF(WK.EQ.'R11') FROW=11
          IF(WK.EQ.'R12') FROW=12
          IF(WK.EQ.'R13') FROW=13
          IF(WK.EQ.'R14') FROW=14
          IF(WK.EQ.'R15') FROW=15
          IF(WK.EQ.'R16') FROW=16
          IF(WK.EQ.'R17') FROW=17
          IF(WK.EQ.'R18') FROW=18
          IF(WK.EQ.'R19') FROW=19
          IF(WK.EQ.'R20') FROW=20
          IF(WK.EQ.'R21') FROW=21
          IF(WK.EQ.'R22') FROW=22
          IF(WK.EQ.'R23') FROW=23
          IF(WK.EQ.'R24') FROW=24
          IF(WK.EQ.'R25') FROW=25
          IF(WK.EQ.'R26') FROW=26
          IF(WK.EQ.'R27') FROW=27
          IF(WK.EQ.'R28') FROW=28
          IF(WK.EQ.'R29') FROW=29
          IF(WK.EQ.'R30') FROW=30
          IF(WK.EQ.'R31') FROW=31
          IF(WK.EQ.'R32') FROW=32
          IF(WK.EQ.'R33') FROW=33
          IF(WK.EQ.'R34') FROW=34
          IF(WK.EQ.'R35') FROW=35
          IF(WK.EQ.'R36') FROW=36
          IF(WK.EQ.'R37') FROW=37
          IF(WK.EQ.'R38') FROW=38
          IF(WK.EQ.'R39') FROW=39
          IF(WK.EQ.'R40') FROW=40
          IF(WK.EQ.'R41') FROW=41
          IF(WK.EQ.'R42') FROW=42
          IF(WK.EQ.'R43') FROW=43
          IF(WK.EQ.'R44') FROW=44
          IF(WK.EQ.'R45') FROW=45
          IF(WK.EQ.'R46') FROW=46
          IF(WK.EQ.'R47') FROW=47
          IF(WK.EQ.'R48') FROW=48
          IF(WK.EQ.'R49') FROW=49
          IF(WK.EQ.'R50') FROW=50
          IF(WK.EQ.'R51') FROW=51
          IF(WK.EQ.'R52') FROW=52
          IF(WK.EQ.'R53') FROW=53
          IF(WK.EQ.'R54') FROW=54
          IF(WK.EQ.'R55') FROW=55
          IF(WK.EQ.'R56') FROW=56
          IF(WK.EQ.'R57') FROW=57
          IF(WK.EQ.'R58') FROW=58
          IF(WK.EQ.'R59') FROW=59
          IF(WK.EQ.'R60') FROW=60
          IF(WK.EQ.'R61') FROW=61
          IF(WK.EQ.'R62') FROW=62
          IF(WK.EQ.'R63') FROW=63
          IF(WK.EQ.'R64') FROW=64
          IF(WK.EQ.'R65') FROW=65
          IF(WK.EQ.'R66') FROW=66
          IF(WK.EQ.'R67') FROW=67
          IF(WK.EQ.'R68') FROW=68
          IF(WK.EQ.'R69') FROW=69
          IF(WK.EQ.'R70') FROW=70
          IF(WK.EQ.'R71') FROW=71
          IF(WK.EQ.'R72') FROW=72
          IF(WK.EQ.'R73') FROW=73
          IF(WK.EQ.'R74') FROW=74
          IF(WK.EQ.'R75') FROW=75
          IF(WK.EQ.'R76') FROW=76
          IF(WK.EQ.'R77') FROW=77
          IF(WK.EQ.'R78') FROW=78
          IF(WK.EQ.'R89') FROW=89
          IF(WK.EQ.'R80') FROW=80
          IF(WK.EQ.'R81') FROW=81
          IF(WK.EQ.'R82') FROW=82
          IF(WK.EQ.'R83') FROW=83
          IF(WK.EQ.'R84') FROW=84
          IF(WK.EQ.'R85') FROW=85
          IF(WK.EQ.'R86') FROW=86
          IF(WK.EQ.'R87') FROW=87
          IF(WK.EQ.'R88') FROW=88
          IF(WK.EQ.'R89') FROW=89
          IF(WK.EQ.'R90') FROW=90
          IF(WK.EQ.'R91') FROW=91
          IF(WK.EQ.'R92') FROW=92
          IF(WK.EQ.'R93') FROW=93
          IF(WK.EQ.'R94') FROW=94
          IF(WK.EQ.'R95') FROW=95
          IF(WK.EQ.'R96') FROW=96
          IF(WK.EQ.'R97') FROW=97
          IF(WK.EQ.'R98') FROW=98
          IF(WK.EQ.'R99') FROW=99
          IF(WK.EQ.'R100') FROW=100
          RETURN
      END


C SUB STACK.FOR
      SUBROUTINE STACK
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS USED TO MANIPULATE THE REAL AND IMAGINARY
C       RPN STACK
C
          CHARACTER CSTRING*23
C
          INCLUDE 'datmai.inc'
!      INTEGER I
C
C       X = REG(9),Y=REG(10),Z=REG(11),T=REG(12)
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)'STACK COMMANDS TAKE NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'RUP') THEN
              REG(50)=REG(12)
              REG(12)=REG(11)
              REG(11)=REG(10)
              REG(10)=REG(9)
              REG(40)=REG(9)
              REG(9)=REG(50)
              RETURN
          END IF
          IF(WC.EQ.'IRUP') THEN
              REG(49)=REG(16)
              REG(16)=REG(15)
              REG(15)=REG(14)
              REG(14)=REG(13)
              REG(30)=REG(13)
              REG(13)=REG(49)
              RETURN
          END IF
          IF(WC.EQ.'CRUP') THEN
              REG(49)=REG(16)
              REG(16)=REG(15)
              REG(15)=REG(14)
              REG(14)=REG(13)
              REG(30)=REG(13)
              REG(13)=REG(49)
              REG(50)=REG(12)
              REG(12)=REG(11)
              REG(11)=REG(10)
              REG(10)=REG(9)
              REG(40)=REG(9)
              REG(9) =REG(50)
              RETURN
          END IF
          IF(WC.EQ.'RDN') THEN
              REG(50)=REG(9)
              REG(40)=REG(9)
              REG(9)=REG(10)
              REG(10)=REG(11)
              REG(11)=REG(12)
              REG(12)=REG(50)
              RETURN
          END IF
          IF(WC.EQ.'IRDN') THEN
              REG(49)=REG(13)
              REG(30)=REG(13)
              REG(13)=REG(14)
              REG(14)=REG(15)
              REG(15)=REG(16)
              REG(16)=REG(49)
              RETURN
          END IF
          IF(WC.EQ.'CRDN') THEN
              REG(49)=REG(13)
              REG(30)=REG(13)
              REG(13)=REG(14)
              REG(14)=REG(15)
              REG(15)=REG(16)
              REG(16)=REG(49)
              REG(50)=REG(9)
              REG(40)=REG(9)
              REG(9)=REG(10)
              REG(10)=REG(11)
              REG(11)=REG(12)
              REG(12)=REG(50)
              RETURN
          END IF
          IF(WC.EQ.'ENTR'.OR.WC.EQ.'ENT') THEN
              CALL PUSH_STACK
              RETURN
          END IF
          IF(WC.EQ.'ENTI') THEN
              REG(16)=REG(15)
              REG(15)=REG(14)
              REG(14)=REG(13)
              RETURN
          END IF
          IF(WC.EQ.'ENTC') THEN
              REG(16)=REG(15)
              REG(15)=REG(14)
              REG(14)=REG(13)
              CALL PUSH_STACK
              RETURN
          END IF
          IF(WC.EQ.'PULL') THEN
              REG(40)=REG(9)
              REG(9)=REG(10)
              CALL PULL_STACK
              RETURN
          END IF
          IF(WC.EQ.'IPULL') THEN
              REG(30)=REG(13)
              REG(13)=REG(14)
              REG(14)=REG(15)
              REG(15)=REG(16)
              RETURN
          END IF
          IF(WC.EQ.'CPULL') THEN
              REG(30)=REG(13)
              REG(13)=REG(14)
              REG(14)=REG(15)
              REG(15)=REG(16)
              REG(40)=REG(9)
              REG(9)=REG(10)
              CALL PULL_STACK
              RETURN
          END IF
          IF(WC.EQ.'LASTIX') THEN

              REG(31)=REG(13)
              REG(13)=REG(30)
              REG(30)=REG(31)
              RETURN
          END IF
          IF(WC.EQ.'LASTX') THEN
              REG(41)=REG(9)
              REG(9)=REG(40)
              REG(40)=REG(41)
              RETURN
          END IF
          IF(WC.EQ.'X-Y') THEN
              REG(40)=REG(9)
              REG(9)=REG(10)
              REG(10)=REG(40)
              RETURN
          END IF
          IF(WC.EQ.'IX-IY') THEN
              REG(30)=REG(13)
              REG(13)=REG(14)
              REG(14)=REG(30)
              RETURN
          END IF
          IF(WC.EQ.'RE-IM') THEN
C       UPDATE LASTX AND LASTIX
              REG(40)=REG(9)
              REG(30)=REG(13)
C
              REG(32)=REG(9)
              REG(9)=REG(13)
              REG(13)=REG(32)
C
              REG(32)=REG(10)
              REG(10)=REG(14)
              REG(14)=REG(32)
C
              REG(32)=REG(11)
              REG(11)=REG(15)
              REG(15)=REG(32)
C
              REG(32)=REG(12)
              REG(12)=REG(16)
              REG(16)=REG(32)
C
              REG(32)=0.0D0
              RETURN
          END IF
          IF(WC.EQ.'X-Y') THEN
              REG(40)=REG(9)
              REG(9)=REG(10)
              REG(10)=REG(40)
              RETURN
          END IF
          IF(WC.EQ.'+') THEN
              REG(40)=REG(9)
              REG(9)=REG(10)+REG(9)
              CALL PULL_STACK
              RETURN
          END IF
          IF(WC.EQ.'I+') THEN
              REG(30)=REG(13)
              REG(13)=REG(14)+REG(13)
              REG(14)=REG(15)
              REG(15)=REG(16)
              RETURN
          END IF
          IF(WC.EQ.'C+') THEN
              REG(40)=REG(9)
              REG(9)=REG(10)+REG(9)
              CALL PULL_STACK
              REG(30)=REG(13)
              REG(13)=REG(14)+REG(13)
              REG(14)=REG(15)
              REG(15)=REG(16)
              RETURN
          END IF
          IF(WC.EQ.'-') THEN
              REG(40)=REG(9)
              REG(9)=REG(10)-REG(9)
              CALL PULL_STACK
              RETURN
          END IF
          IF(WC.EQ.'I-') THEN
              REG(30)=REG(13)
              REG(13)=REG(14)-REG(13)
              REG(14)=REG(15)
              REG(15)=REG(16)
              RETURN
          END IF
          IF(WC.EQ.'C-') THEN
              REG(40)=REG(9)
              REG(9)=REG(10)-REG(9)
              CALL PULL_STACK
              REG(30)=REG(13)
              REG(13)=REG(14)-REG(13)
              REG(14)=REG(15)
              REG(15)=REG(16)
              RETURN
          END IF
          IF(WC.EQ.'*') THEN
              REG(40)=REG(9)
              REG(9)=REG(10)*REG(9)
              CALL PULL_STACK
          END IF
          IF(WC.EQ.'I*') THEN
              REG(30)=REG(13)
              REG(13)=REG(14)*REG(13)
              REG(14)=REG(15)
              REG(15)=REG(16)
          END IF
          IF(WC.EQ.'C*') THEN
              REG(40)=REG(9)
              REG(9)=REG(10)*REG(9)
              CALL PULL_STACK
              REG(30)=REG(13)
              REG(13)=REG(14)*REG(13)
              REG(14)=REG(15)
              REG(15)=REG(16)
          END IF
          IF(WC.EQ.'/') THEN
              IF(REG(9).NE.0.0D0) THEN
                  REG(40)=REG(9)
                  REG(9)=REG(10)/REG(9)
                  CALL PULL_STACK
                  RETURN
              ELSE
                  IF(REG(9).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)'REAL DIVISION BY ZERO NOT ALLOWED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                  END IF
              END IF
              RETURN
          END IF
          IF(WC.EQ.'I/') THEN
              IF(REG(13).NE.0.0D0) THEN
                  REG(30)=REG(13)
                  REG(13)=REG(14)/REG(13)
                  REG(14)=REG(15)
                  REG(15)=REG(16)
                  RETURN
              ELSE
                  IF(REG(13).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)'IMAGINARY DIVISION BY ZERO NOT ALLOWED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                  END IF
              END IF
              RETURN
          END IF
          IF(WC.EQ.'C/') THEN
              IF(REG(9).NE.0.0D0.AND.REG(13).NE.0.0D0) THEN
                  REG(40)=REG(9)
                  REG(9)=REG(10)/REG(9)
                  CALL PULL_STACK
                  REG(30)=REG(13)
                  REG(13)=REG(14)/REG(13)
                  REG(14)=REG(15)
                  REG(15)=REG(16)
                  RETURN
              ELSE
                  IF(REG(9).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)'REAL DIVISION BY ZERO NOT ALLOWED'
                      CALL SHOWIT(1)
                  END IF
                  IF(REG(13).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)'IMAGINARY DIVISION BY ZERO NOT ALLOWED'
                      CALL SHOWIT(1)
                  END IF
                  IF(REG(9).EQ.0.0D0.OR.REG(13).EQ.0.0D0
     1            .OR.REG(9).EQ.0.0D0.AND.REG(13).EQ.0.0D0) THEN
                      WRITE(OUTLYNE,*)'COMPLEX STACK NOT CHANGED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                  END IF
              END IF
              RETURN
          END IF
          IF(WC.EQ.'Y**X') THEN
              REG(40)=REG(9)
              REG(9)=REG(10)**REG(9)
              CALL PULL_STACK
              RETURN
          END IF
          IF(WC.EQ.'IY**IX') THEN
              REG(30)=REG(13)
              REG(13)=REG(14)**REG(13)
              REG(14)=REG(15)
              REG(15)=REG(16)
              RETURN
          END IF
          IF(WC.EQ.'CY**CX') THEN
              REG(40)=REG(9)
              REG(9)=REG(10)**REG(9)
              CALL PULL_STACK
              REG(30)=REG(13)
              REG(13)=REG(14)**REG(13)
              REG(14)=REG(15)
              REG(15)=REG(16)
              RETURN
          END IF
          IF(WC.EQ.'CLIX') THEN
              REG(30)=REG(13)
              REG(13)=0.0D0
              RETURN
          END IF
          IF(WC.EQ.'CLX') THEN
              REG(40)=REG(9)
              REG(9)=0.0D0
              RETURN
          END IF
          IF(WC.EQ.'CLSTK') THEN
              REG(40)=REG(9)
              REG(9)=0.0D0
              REG(10)=0.0D0
              REG(11)=0.0D0
              REG(12)=0.0D0
              RETURN
          END IF
          IF(WC.EQ.'CLSTKI') THEN
              REG(30)=REG(13)
              REG(13)=0.0D0
              REG(14)=0.0D0
              REG(15)=0.0D0
              REG(16)=0.0D0
              RETURN
          END IF
          IF(WC.EQ.'CLSTKC') THEN
              REG(30)=REG(13)
              REG(13)=0.0D0
              REG(14)=0.0D0
              REG(15)=0.0D0
              REG(16)=0.0D0
              REG(40)=REG(9)
              REG(9)=0.0D0
              REG(10)=0.0D0
              REG(11)=0.0D0
              REG(12)=0.0D0
              RETURN
          END IF
          IF(WC.EQ.'PRLSTX') THEN
C       THIS IS CALLED TO WRITE OUT THE CONTENTS
C       OF REGISTER REG(40). THIS IS THE LASTX REGISTER.
C       PRIOR TO REASSIGNMENT OF THE VALUE OF THE ACCUMULATOR
C       REG(9) THE CURRENT VALUE IS ALWAYS STORED IN REG(40)
              IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(40)
                  WS='LASTX ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,101) REG(40)
                  CALL SHOWIT(0)
              END IF
C       THE FOLLOWING ARE THE FORMAT STATEMENTS
 101          FORMAT(D23.15)
              RETURN
          END IF
          IF(WC.EQ.'PRLSTIX') THEN
C       THIS IS CALLED TO WRITE OUT THE CONTENTS
C       OF REGISTER REG(30). THIS IS THE LASTIX REGISTER.
C       PRIOR TO REASSIGNMENT OF THE VALUE OF THE IX RERGISTER
C       REG(13) THE CURRENT VALUE IS ALWAYS STORED IN REG(30)
              IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(30)
                  WS='LASTIX ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,102) REG(30)
                  CALL SHOWIT(0)
              END IF
C       THE FOLLOWING ARE THE FORMAT STATEMENTS
 102          FORMAT(D23.15)
              RETURN
          END IF
          IF(WC.EQ.'PRSTK') THEN
C       THIS IS CALLED TO WRITE OUT THE CONTENTS
C       OF A THE REAL STACK.
              IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(9)
                  WS='X ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,1001) REG(9)
                  CALL SHOWIT(0)
              END IF
              IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(10)
                  WS='Y ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,1001) REG(10)
                  CALL SHOWIT(0)
              END IF
              IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(11)
                  WS='Z ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,1001) REG(11)
                  CALL SHOWIT(0)
              END IF
              IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(12)
                  WS='T ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,1001) REG(12)
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(WC.EQ.'PRSTKI') THEN
C       THIS IS CALLED TO WRITE OUT THE CONTENTS
C       OF A THE IMAGINARY STACK.
              IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(13)
                  WS='IX ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,1001) REG(13)
                  CALL SHOWIT(0)
              END IF
              IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(14)
                  WS='IY ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,1001) REG(14)
                  CALL SHOWIT(0)
              END IF
              IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(15)
                  WS='IZ ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,1001) REG(15)
                  CALL SHOWIT(0)
              END IF
              IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(16)
                  WS='IT ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,1001) REG(16)
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
          IF(WC.EQ.'PRSTKC') THEN
C       THIS IS CALLED TO WRITE OUT THE CONTENTS
C       OF A THE COMPLEX STACK.
              IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(9)
                  WS='X ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(10)
                  WS='Y ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(11)
                  WS='Z ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(12)
                  WS='T ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(13)
                  WS='IX ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(14)
                  WS='IY ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(15)
                  WS='IZ ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
                  WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(16)
                  WS='IT ='//' '//CSTRING(1:23)
                  WRITE(OUTLYNE,1100) WS
                  CALL SHOWIT(0)
              ELSE
                  WRITE(OUTLYNE,1001) REG(9)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1001) REG(10)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1001) REG(11)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1001) REG(12)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1001) REG(13)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1001) REG(14)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1001) REG(15)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,1001) REG(16)
                  CALL SHOWIT(0)
              END IF
              RETURN
          END IF
C
C       THE FOLLOWING ARE THE FORMAT STATEMENTS
C
 1001     FORMAT(D23.15)
          RETURN
   69     CONTINUE
          WRITE(OUTLYNE,*)
     1    'INVALID FORMAT SPECIFICATION EXISTS'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)
     1    'RE-ISSUE THE "FORMAT" COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
 1100     FORMAT(A79)
      END
C SUB MINMAXREG.FOR
      SUBROUTINE MINMAXREG
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS USED TO MANIPULATE THE MIN/MAX REGISTERS
C
C
          INCLUDE 'datmai.inc'
!      INTEGER I
C
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              IF(WC.EQ.'STOREMIN')
     1        WRITE(OUTLYNE,*)'"STOREMIN" TAKES NO STRING OR QUALIFIER INPUT'
              IF(WC.EQ.'STOREMAX')
     1        WRITE(OUTLYNE,*)'"STOREMAX" TAKES NO STRING OR QUALIFIER INPUT'
              IF(WC.EQ.'RESETMIN')
     1        WRITE(OUTLYNE,*)'"RESETMIN" TAKES NO STRING OR QUALIFIER INPUT'
              IF(WC.EQ.'RESETMAX')
     1        WRITE(OUTLYNE,*)'"RESETMAX" TAKES NO STRING OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'STOREMIN'.OR.WC.EQ.'STOREMAX') THEN
              IF(DF1.EQ.1) THEN
                  W1=1.0D0
                  DF1=0
                  S1=1
              END IF
          END IF
          IF(WC.EQ.'RESETMIN') THEN
              IF(DF2.EQ.1) THEN
                  W2=1.0D300
                  DF2=0
                  S2=1
              END IF
          END IF
          IF(WC.EQ.'RESETMAX') THEN
              IF(DF2.EQ.1) THEN
                  W2=-1.0D300
                  DF2=0
                  S1=2
              END IF
          END IF
          IF(WC.EQ.'RESETMIN'.OR.WC.EQ.'RESETMAX') THEN
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  IF(WC.EQ.'RESETMIN')
     1            WRITE(OUTLYNE,*)
     2            '"RESETMIN" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
                  IF(WC.EQ.'RESETMAX')
     1            WRITE(OUTLYNE,*)
     2            '"RESETMAX" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'STOREMIN'.OR.WC.EQ.'STOREMAX') THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  IF(WC.EQ.'STOREMIN')
     1            WRITE(OUTLYNE,*)'"STOREMIN" ONLY TAKES NUMERIC WORD #1 INPUT'
                  IF(WC.EQ.'STOREMAX')
     1            WRITE(OUTLYNE,*)'"STOREMAX" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'RESETMIN') THEN
              IF(DF1.EQ.1) THEN
                  MIN_REG(1:100)=W2
                  RETURN
              ELSE
                  MIN_REG(INT(W1))=W2
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'RESETMAX') THEN
              IF(DF1.EQ.1) THEN
                  MAX_REG(1:100)=W2
                  RETURN
              ELSE
                  MAX_REG(INT(W1))=W2
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'STOREMAX') THEN
              IF(REG(9).GT.MAX_REG(INT(W1))) MAX_REG(INT(W1))=REG(9)
              RETURN
          END IF
          IF(WC.EQ.'STOREMIN') THEN
              IF(REG(9).LT.MIN_REG(INT(W1))) MIN_REG(INT(W1))=REG(9)
              RETURN
          END IF
      END
C SUB ZMX2PRG.FOR
      SUBROUTINE ZMX2PRG
C
          IMPLICIT NONE
C
          CHARACTER ZMXFILENAME*80,KDPFILENAME*80,ZMX_INPUT_STRING*1024
     1    ,SURFTYPE*8
C
          CHARACTER TEMPA*1024,TEMPB*1024
     1    ,GLASSA*27,VALA*23,AA23*23,SUMSTRING*1024
     2    ,TEMPC*1024,BL1024*1024,TIL*1,TEMPER*1024,PREV*80
     3    ,FIELDTYPE*3,CLTYPE*1,COTYPE*1,TEMPCC*1024,TEMPCCC*1024
C
          LOGICAL EXIS37,SEMI,ADD,OLDADD
C
          LOGICAL ZMXERROR
C
          LOGICAL DOWV,DOWV2,CLAP1,CLAP2,CLDX,CLDY
C
          LOGICAL SURFIT,COBS1,COBS2,COBX,COBY
C
          INTEGER I,ZMXFILENAMELENGTH,KDPFILENAMELENGTH,N,NPERIOD,ALLOERR
C
          INTEGER J,K,L,NADD,II,IC,STRINGEND,WDEXIS(1:100)
C
          INTEGER IVALV,IWD(1:100),SURFER,FLDTYP,IPAR
C
          REAL*8 VALV,WD(1:100),MAX,CL1,CL2,CL3,CL4,CL5
C
          REAL*8 CO1,CO2,CO3,CO4,CO5,VPAR
C
          DIMENSION TEMPA(:),TEMPB(:),TEMPC(:)
C
          ALLOCATABLE :: TEMPA,TEMPB,TEMPC
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          SURFER=0
          GLASSA='AIR                        '
          SURFIT=.FALSE.
C
          TIL='?'
          CLTYPE='?'
          CLAP1=.FALSE.
          CLAP2=.FALSE.
          CLDX=.FALSE.
          CLDY=.FALSE.
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
          CO1=0.0D0
          CO2=0.0D0
          CO3=0.0D0
          CO4=0.0D0
          CO5=0.0D0
C
C       THIS SUBROUTINE IS CALLED CONVERT A FILE FROM ZMX TO PROGRAM
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
              OUTLYNE='"ZMX2PRG" CONVERTS THE NAMED FILE (STRING INPUT)'
              CALL SHOWIT(1)
              OUTLYNE='FROM ZEMAX TO PROGRAM FORMAT'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SQ.EQ.1) THEN
              OUTLYNE='"ZMX2PRG" TAKES NO NUMERIC OR QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.0) THEN
              OUTLYNE='"ZMX2PRG" REQUIRES EXPLICIT STRING (FILE NAME) INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          call upper_case(WS)
          ZMXFILENAME=trim(HOME)//"LENSES/"//WS

          write(6,*) ZMXFILENAME

          DO I=80,1,-1
              IF(ZMXFILENAME(I:I).NE.' ') THEN
                  N=I
                  GO TO 10
              END IF
          END DO
          OUTLYNE='INVALID (ZERO LENGTH) ZEMAX FILE NAME'
          CALL SHOWIT(1)
          OUTLYNE='NO ACTION TAKEN'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
 10       CONTINUE
          ZMXFILENAMELENGTH=N
          EXIS37=.FALSE.
          INQUIRE(FILE=ZMXFILENAME(1:N),EXIST=EXIS37)
          IF(.NOT.EXIS37) THEN
              OUTLYNE='NO ZEMAX INPUT FILE EXISTS TO READ'
              CALL SHOWIT(1)
              OUTLYNE='NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     FILES EXISTS, CREATE PROGRAM FILE NAME
          NPERIOD=9
          DO I=1,80
              IF(ZMXFILENAME(I:I).EQ.'.') THEN
                  NPERIOD=I
                  GO TO 20
              END IF
          END DO
 20       CONTINUE
          IF(NPERIOD.GT.9) NPERIOD=9
          KDPFILENAME=ZMXFILENAME(1:NPERIOD)//'DAT'
          KDPFILENAMELENGTH=NPERIOD+4
C     OPEN THE ZMXFILENAME TO READ IT AS ASCII
C
          DEALLOCATE (TEMPA,TEMPB,TEMPC
     1    ,STAT=ALLOERR)
          ALLOCATE (TEMPA(1:5000),TEMPB(1:5000),TEMPC(1:5000)
     1    ,STAT=ALLOERR)
          TEMPA(1:5000)=BL1024
          TEMPB(1:5000)=BL1024
          TEMPC(1:5000)=BL1024
C
C     CLOSE IT IF OPEN AND KEEP IT
          CALL CLOSE_FILE(37,1)
          CALL CLOSE_FILE(38,0)
          OPEN(UNIT=37,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=ZMXFILENAME(1:ZMXFILENAMELENGTH)
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
              ZMX_INPUT_STRING=BL1024
              READ(37,100,ERR=75,END=76) ZMX_INPUT_STRING(1:132)
              GO TO 77
 76           ZMX_INPUT_STRING(1:132)='GO'
              GO TO 78
 77           CONTINUE
C     IS THERE A CONTINUATION MARK AT THE END ?
C     REMEMBER IF ADD WAS ON LAST TIME
              OLDADD=ADD
              DO J=1024,1,-1
                  ADD=.FALSE.
                  IF(ZMX_INPUT_STRING(J:J).EQ.'&') THEN
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
                  TEMPA(II)(1:132)=ZMX_INPUT_STRING(1:132)
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
                  SUMSTRING(1:1024)=ZMX_INPUT_STRING(1:1024)
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
     1                ZMX_INPUT_STRING(1:(1024-STRINGEND))
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
     1                ZMX_INPUT_STRING(1:(1024-STRINGEND))
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
          GO TO 78
 75       CONTINUE
          OUTLYNE='ERROR READING ZEMAX INPUT FILE'
          CALL SHOWIT(1)
          OUTLYNE='NO FILE CONVERSION PERFORMED'
          CALL SHOWIT(1)
          CALL MACFAL
          CALL CLOSE_FILE(37,1)
          CALL CLOSE_FILE(38,0)
          DEALLOCATE (TEMPA,TEMPB,TEMPC,STAT=ALLOERR)
          RETURN
 78       CONTINUE
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
 4000     FORMAT(' THE FOLLOWING ZEMAX DID NOT TRANSLATE:')
          WRITE(38,4000)
C******************************************************************************
C
          PREV=AA//AA//AA//AA
          INPUT='LENS'
          CALL PROCES

          DO I=1,L
              TEMPCC(1:1024)=TEMPC(I)(1:1024)
              TEMPCCC(1:1024)=TEMPC(I)(1:1024)
              IF(SURFTYPE.EQ.'EVENASPH') THEN
 2054             FORMAT('AD,',D23.15)
 2154             FORMAT('AE,',D23.15)
 2055             FORMAT('AF,',D23.15)
 2056             FORMAT('AG,',D23.15)
 2057             FORMAT('AH,',D23.15)
 2058             FORMAT('AI,',D23.15)
 2059             FORMAT('AJ,',D23.15)
 2060             FORMAT('AK,',D23.15)
                  IF(TEMPCC(1:4).EQ.'PARM') THEN
                      ZMXERROR=.FALSE.
                      TEMPCC(1:1024)=TEMPC(I)(1:1024)
                      CALL PARAMOUT(IPAR,TEMPCC,ZMXERROR)
                      IF(ZMXERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
C
C     PROCEED PROCESSING THE PARAMETER INPUT WITH COORDBRK SURFACE
                      IF(IPAR.EQ.1) THEN
                          WRITE(OUTLYNE,2054) VPAR
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.2) THEN
                          WRITE(OUTLYNE,2154) VPAR
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.3) THEN
                          WRITE(OUTLYNE,2055) VPAR
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.4) THEN
                          WRITE(OUTLYNE,2056) VPAR
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.5) THEN
                          WRITE(OUTLYNE,2057) VPAR
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.6) THEN
                          WRITE(OUTLYNE,2058) VPAR
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.7) THEN
                          WRITE(OUTLYNE,2059) VPAR
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.8) THEN
                          WRITE(OUTLYNE,2060) VPAR
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  ELSE
C     NOT PARM
                  END IF
              END IF
              IF(SURFTYPE.EQ.'DGRATING') THEN
 2091             FORMAT('GRT,',D23.15)
 2092             FORMAT('GRO,',D23.15)
                  IF(TEMPCC(1:4).EQ.'PARM') THEN
                      ZMXERROR=.FALSE.
                      TEMPCC(1:1024)=TEMPC(I)(1:1024)
                      CALL PARAMOUT(IPAR,TEMPCC,ZMXERROR)
                      IF(ZMXERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
C
C     PROCEED PROCESSING THE PARAMETER INPUT WITH COORDBRK SURFACE
                      IF(IPAR.EQ.1) THEN
                          IF(SYSTEM1(6).EQ.1.0D0) VPAR=(1.0D-3/VPAR)/25.4D0
                          IF(SYSTEM1(6).EQ.2.0D0) VPAR=(1.0D-4/VPAR)
                          IF(SYSTEM1(6).EQ.3.0D0) VPAR=(1.0D-3/VPAR)
                          IF(SYSTEM1(6).EQ.4.0D0) VPAR=1.0D-6/VPAR
                          WRITE(OUTLYNE,2091) VPAR
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.2) THEN
                          WRITE(OUTLYNE,2092) VPAR
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  ELSE
C     NOT PARM
                  END IF
              END IF
C     TYPE STANDARD OR TYPE EVENASPH
              IF(TEMPC(I)(1:13).EQ.'TYPE STANDARD'.OR.
     1        TEMPC(I)(1:13).EQ.'TYPE EVENASPH') THEN
                  IF(TEMPC(I)(1:13).EQ.'TYPE STANDARD') SURFTYPE='STANDARD'
                  IF(TEMPC(I)(1:13).EQ.'TYPE EVENASPH') SURFTYPE='EVENASPH'
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     TYPE DIFFRACTION GRATING
              IF(TEMPC(I)(1:13).EQ.'TYPE DGRATING') THEN
                  IF(TEMPC(I)(1:13).EQ.'TYPE DGRATING') SURFTYPE='DGRATING'
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     VERS
              IF(TEMPC(I)(1:4).EQ.'VERS') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='LENS'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
C     SET DEFAULT UNITS TO MM SINCE THAT IS HOW ZEMAX WAKES UP
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=BL1024(1:132)
                  INPUT(1:132)='UNITS MM'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     NAME
              IF(TEMPC(I)(1:4).EQ.'NAME') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='LI '//TEMPC(I)(5:80)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     COMM
              IF(TEMPC(I)(1:4).EQ.'COMM') THEN
                  INPUT='LBL'//TEMPC(I)(5:80)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     ENPD
              IF(TEMPC(I)(1:4).EQ.'ENPD') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  ZMXERROR=.FALSE.
                  CALL ATODZMX(VALA,VALV,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2001) VALV/2.0D0
 2001             FORMAT('SAY,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     FLOA
              IF(TEMPC(I)(1:4).EQ.'FLOA') THEN
                  WRITE(OUTLYNE,2085)
 2085             FORMAT('SAY FLOAT')
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     OBNA
              IF(TEMPC(I)(1:4).EQ.'OBNA') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  ZMXERROR=.FALSE.
                  CALL ATODCODEV(VALA,VALV,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2002) VALV
 2002             FORMAT('NAO,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     PWAV
              IF(TEMPC(I)(1:4).EQ.'PWAV') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  ZMXERROR=.FALSE.
                  CALL ATOIZMX(VALA,IVALV,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2003) IVALV
 2003             FORMAT('CW,',I2)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     FTYP
              IF(TEMPC(I)(1:4).EQ.'FTYP') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  ZMXERROR=.FALSE.
                  CALL ATOIZMX(VALA,IVALV,ZMXERROR)
                  FLDTYP=IVALV
C     0 = FIELD ANGLE
C     1 = OBJECT HT.
C     2 = PARAX IMAGE HT.
C     3 = REAL IMAGE HT.
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     XFLD OR YFLD
              IF(TEMPC(I)(1:4).EQ.'XFLD'
     1        .OR.TEMPC(I)(1:4).EQ.'YFLD') THEN
                  FIELDTYPE='   '
                  IF(TEMPC(I)(1:4).EQ.'XFLD') THEN
                      IF(FLDTYP.EQ.0) FIELDTYPE='XAN'
                      IF(FLDTYP.EQ.1) FIELDTYPE='XOB'
                      IF(FLDTYP.EQ.2) FIELDTYPE='XIM'
                      IF(FLDTYP.EQ.3) FIELDTYPE='XIR'
                  END IF
                  IF(TEMPC(I)(1:4).EQ.'YFLD') THEN
                      IF(FLDTYP.EQ.0) FIELDTYPE='YAN'
                      IF(FLDTYP.EQ.1) FIELDTYPE='YOB'
                      IF(FLDTYP.EQ.2) FIELDTYPE='YIM'
                      IF(FLDTYP.EQ.3) FIELDTYPE='YIR'
                  END IF
                  CALL ONEBLANK(4,TEMPC(I)(1:1024))
                  IF(FIELDTYPE.EQ.'XAN'.OR.FIELDTYPE.EQ.'YAN'.OR.
     1            FIELDTYPE.EQ.'XOB'.OR.FIELDTYPE.EQ.'YOB') THEN
C     STRIP OFF 4 CHARACTERS
                      TEMPC(I)(1:1024)=TEMPC(I)(5:1024)//'    '
C     STRIP LEADING BLANKS
                      DO J=1,1024
                          IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                      END DO
                      ZMXERROR=.FALSE.
                      CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),9,ZMXERROR)
                      IF(ZMXERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
                      MAX=-1000.0D0
                      DO J=1,9
                          IF(DABS(WD(J)).GT.MAX) MAX=DABS(WD(J))
                      END DO
                      IF(FIELDTYPE.EQ.'YAN') THEN
                          WRITE(OUTLYNE,2004) MAX
 2004                     FORMAT('SCY FANG,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          TEMPC(I)(1:1024)=BL1024(1:1024)
                      END IF
                      IF(FIELDTYPE.EQ.'XAN') THEN
                          WRITE(OUTLYNE,2005) MAX
 2005                     FORMAT('SCX FANG,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          TEMPC(I)(1:1024)=BL1024(1:1024)
                      END IF
                      IF(FIELDTYPE.EQ.'YOB') THEN
                          WRITE(OUTLYNE,2006) MAX
 2006                     FORMAT('SCY,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          TEMPC(I)(1:1024)=BL1024(1:1024)
                      END IF
                      IF(FIELDTYPE.EQ.'XOB') THEN
                          WRITE(OUTLYNE,2007) MAX
 2007                     FORMAT('SCX,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                          TEMPC(I)(1:1024)=BL1024(1:1024)
                      END IF
                      GO TO 8888
                  END IF
              END IF
              IF(FIELDTYPE.EQ.'XIM'.OR.FIELDTYPE.EQ.'YIM') THEN
C     IMAGE HT SPEC
C     STRIP OFF 4 CHARACTERS
                  TEMPC(I)(1:1024)=TEMPC(I)(5:1024)//'    '
C     STRIP LEADING BLANKS
                  DO J=1,1024
                      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                  END DO
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),9,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  MAX=-1000.0D0
                  DO J=1,25
                      IF(DABS(WD(J)).GT.MAX) MAX=DABS(WD(J))
                  END DO
                  IF(FIELDTYPE.EQ.'XIM') THEN
                      WRITE(OUTLYNE,4004) MAX
 4004                 FORMAT('PXIM,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  IF(FIELDTYPE.EQ.'YIM') THEN
                      WRITE(OUTLYNE,4005) MAX
 4005                 FORMAT('PYIM,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  IF(FIELDTYPE.EQ.'XIR') THEN
                      WRITE(OUTLYNE,4006) MAX
 4006                 FORMAT('RXIM,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  IF(FIELDTYPE.EQ.'YIR') THEN
                      WRITE(OUTLYNE,4007) MAX
 4007                 FORMAT('RYIM,',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  GO TO 8888
              END IF
C     WAVL
              IF(TEMPC(I)(1:4).EQ.'WAVL') THEN
C     BREAK OUT UP TO 10 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
C     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
C     THEN CONVERT THEM TO NUMERIC WORDS AND ISSUE THE WV AND WV2 COMMANDS
C     AS NEEDED.
C     STRIP OFF FOUR CHARACTERS
                  TEMPC(I)(1:1024)=TEMPC(I)(5:1024)//'    '
C     STRIP LEADING BLANKS
                  DO J=1,1024
                      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                  END DO
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),10,ZMXERROR)
                  IF(ZMXERROR) THEN
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
                      WRITE(OUTLYNE,2008) WD(1),WD(2),WD(3)
     1                ,WD(4),WD(5)
 2008                 FORMAT('WV,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1                ,D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  IF(DOWV2) THEN
                      WRITE(OUTLYNE,2009) WD(6),WD(7),WD(8)
     1                ,WD(9),WD(10)
 2009                 FORMAT('WV2,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1                ,D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  GO TO 8888
              END IF
C     WWGT
              IF(TEMPC(I)(1:4).EQ.'WWGT') THEN
C     BREAK OUT UP TO 10 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
C     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
C     THEN CONVERT THEM TO NUMERIC WORDS AND ISSUE THE SPTWT/SPTWT2 COMMANDS
C     AS NEEDED.
C     STRIP OFF WWGT
                  TEMPC(I)(1:1024)=TEMPC(I)(5:1024)//'    '
C     STRIP LEADING BLANKS
                  DO J=1,1024
                      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                  END DO
                  ZMXERROR=.FALSE.
                  CALL IMULTIPROCESS(IWD,WDEXIS,TEMPC(I)(1:1024),10,ZMXERROR)
                  IF(ZMXERROR) THEN
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
                      WRITE(OUTLYNE,2010) WD(1),WD(2),WD(3),WD(4),WD(5)
 2010                 FORMAT('SPTWT,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1                ,D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  IF(DOWV2) THEN
                      WRITE(OUTLYNE,2011) WD(6),WD(7),WD(8),WD(9),WD(10)
 2011                 FORMAT('SPTWT2,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1                ,D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                  END IF
                  GO TO 8888
              END IF
C     STOP
              IF(TEMPC(I)(1:4).EQ.'STOP') THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='ASTOP'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='REFS'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     UNIT
              IF(TEMPC(I)(1:4).EQ.'UNIT') THEN
                  TEMPC(I)(1:1024)=TEMPC(I)(5:1024)//'    '
                  DO IC=1,1024
                      IF(TEMPC(I)(1:1).EQ.' ') THEN
                          TEMPC(I)(1:1024)=TEMPC(IC)(2:1024)//' '
                      ELSE
                          GO TO 34
                      END IF
                  END DO
 34               CONTINUE
                  IF(TEMPC(I)(1:2).EQ.'MM') THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='UNITS MM'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:2).EQ.'CM') THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='UNITS CM'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:2).EQ.'IN') THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='UNITS IN'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  IF(TEMPC(I)(1:5).EQ.'METER') THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='UNITS M'
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                  END IF
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C
C     PROCESS EACH LINE INTO A KDP COMMAND AND THEN DISPLAY IT
              IF(TEMPC(I)(1:4).EQ.'SURF') THEN
                  IF(.NOT.SURFIT) THEN
                      SURFIT=.TRUE.
                      GO TO 8888
                  END IF
C
C     WRITE CLAP DATA, COBS DATA, THEN GLASS DATA
C     CLAP DATA
                  IF(CLTYPE.EQ.'C') THEN
                      WRITE(OUTLYNE,2012) CL1,CL4,CL3,CL2
 2012                 FORMAT('CLAP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      CLTYPE='?'
                      CLAP1=.FALSE.
                      CLAP2=.FALSE.
                      CLDX=.FALSE.
                      CLDY=.FALSE.
                      CL1=0.0D0
                      CL2=0.0D0
                      CL3=0.0D0
                      CL4=0.0D0
                      CL5=0.0D0
                  END IF
                  IF(CLTYPE.EQ.'R') THEN
                      WRITE(OUTLYNE,2013) CL2,CL1,CL4,CL3
 2013                 FORMAT('CLAP RECT,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      CLTYPE='?'
                      CLAP1=.FALSE.
                      CLAP2=.FALSE.
                      CLDX=.FALSE.
                      CLDY=.FALSE.
                      CL1=0.0D0
                      CL2=0.0D0
                      CL3=0.0D0
                      CL4=0.0D0
                      CL5=0.0D0
                  END IF
                  IF(CLTYPE.EQ.'E') THEN
                      WRITE(OUTLYNE,2014) CL2,CL1,CL4,CL3
 2014                 FORMAT('CLAP ELIP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      CLTYPE='?'
                      CLAP1=.FALSE.
                      CLAP2=.FALSE.
                      CLDX=.FALSE.
                      CLDY=.FALSE.
                      CL1=0.0D0
                      CL2=0.0D0
                      CL3=0.0D0
                      CL4=0.0D0
                      CL5=0.0D0
                  END IF
C     COBS DATA
                  IF(COTYPE.EQ.'C') THEN
                      WRITE(OUTLYNE,4012) CO1,CO4,CO3
 4012                 FORMAT('COBS,',D23.15,',',D23.15,',',D23.15,',,,')
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
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
                      WRITE(OUTLYNE,4013) CO2,CO1,CO4,CO3
 4013                 FORMAT('COBS RECT,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
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
                      WRITE(OUTLYNE,4014) CO2,CO1,CO4,CO3
 4014                 FORMAT('COBS ELIP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT(1:132)=OUTLYNE(1:132)
                      CALL PROCES
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
C     GLASS DATA
                  IF(GLASSA(1:3).EQ.'   ') GLASSA='AIR                        '
                  WRITE(OUTLYNE,2016) GLASSA
 2016             FORMAT(A27)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  SURFER=SURFER+1
                  GLASSA='AIR                        '
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  TIL='?'
                  GO TO 8888
              END IF
C
C     TILTS AND DECENTERS GO HERE
              IF(TEMPC(I)(1:13).EQ.'TYPE TILTSURF') THEN
                  WRITE(OUTLYNE,2019)
 2019             FORMAT('TILT DAR')
                  SURFTYPE='TILTSURF'
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TIL='T'
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
              IF(TEMPC(I)(1:13).EQ.'TYPE COORDBRK') THEN
                  SURFTYPE='COORDBRK'
                  WRITE(OUTLYNE,2021)
 2021             FORMAT('TILT')
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TIL='T'
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C
              IF(SURFTYPE.EQ.'COORDBRK') THEN
C
                  IF(TEMPCC(1:4).EQ.'PARM') THEN
                      ZMXERROR=.FALSE.
                      TEMPCC(1:1024)=TEMPC(I)(1:1024)
                      CALL PARAMOUT(IPAR,TEMPCC,ZMXERROR)
                      IF(ZMXERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
C
C     PROCEED PROCESSING THE PARAMETER INPUT WITH COORDBRK SURFACE
                      IF(IPAR.EQ.3) THEN
                          WRITE(OUTLYNE,2023) -VPAR
 2023                     FORMAT('ALPHA,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.4) THEN
                          WRITE(OUTLYNE,2024) -VPAR
 2024                     FORMAT('BETA,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.5) THEN
                          WRITE(OUTLYNE,2025) VPAR
 2025                     FORMAT('GAMMA,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.1) THEN
                          WRITE(OUTLYNE,2026) VPAR
 2026                     FORMAT('XD,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.2) THEN
                          WRITE(OUTLYNE,2027) VPAR
 2027                     FORMAT('YD,',D23.15)
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  ELSE
C     NOT PARM
                  END IF
              ELSE
C     NOT COORDBRK
              END IF
C
              IF(SURFTYPE.EQ.'TILTSURF') THEN
C
                  IF(TEMPCC(1:4).EQ.'PARM') THEN
                      ZMXERROR=.FALSE.
                      TEMPCC(1:1024)=TEMPC(I)(1:1024)
                      CALL PARAMOUT(IPAR,TEMPCC,ZMXERROR)
                      IF(ZMXERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
C
C     PROCEED PROCESSING THE PARAMETER INPUT WITH COORDBRK SURFACE
                      IF(IPAR.EQ.1) THEN
                          WRITE(OUTLYNE,2024) -(180.0D0*DATAN(VPAR))/PII
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      IF(IPAR.EQ.2) THEN
                          WRITE(OUTLYNE,2023) -(180.0D0*DATAN(VPAR))/PII
                          SAVE_KDP(1)=SAVEINPT(1)
                          INPUT(1:132)=OUTLYNE(1:132)
                          CALL PROCES
                          REST_KDP(1)=RESTINPT(1)
                      END IF
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  ELSE
C     NOT PARM
                  END IF
              ELSE
C     NOT TILTSURF
              END IF
C
C     TILTS AND DECENTERD DONE
C
C     THICKNESS DISZ
C
              IF(TEMPC(I)(1:4).EQ.'DISZ') THEN
                  TEMPER=TEMPC(I)(6:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  IF(VALA(1:8).EQ.'INFINITY') THEN
                      VALV=1.0D20
                  ELSE
                      CALL RIGHTJUST(VALA)
                      ZMXERROR=.FALSE.
                      CALL ATODZMX(VALA,VALV,ZMXERROR)
                  END IF
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2030) VALV
 2030             FORMAT('TH,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     THICKNESS DONE
C
C     CHIEF RAY THICKNESS SOLVE
C
              IF(TEMPC(I)(1:4).EQ.'CHZH') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  ZMXERROR=.FALSE.
                  CALL ATODZMX(VALA,VALV,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2040) VALV
 2040             FORMAT('PCY,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CHIEF RAY THICKNESS SOLVE DONE
C
C     MARGINAL RAY THICKNESS SOLVE
C
              IF(TEMPC(I)(1:4).EQ.'MAZH') THEN
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),2,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2038) WD(1)
 2038             FORMAT('PY,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     MARGINAL RAY THICKNESS SOLVE DONE
C
              IF(TEMPC(I)(1:4).EQ.'EDGE') THEN
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),2,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,3038) WD(1)
 3038             FORMAT('CAY,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     EDGE THICKNESS SOLVE DONE
C
C
C
C     CHIEF RAY CURVATURE SOLVE
C
              IF(TEMPC(I)(1:4).EQ.'CHIA') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  ZMXERROR=.FALSE.
                  CALL ATODZMX(VALA,VALV,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2082) VALV
 2082             FORMAT('PUCY,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CHIEF RAY CURVATURE SOLVE DONE
C
C
C     MARGINAL RAY CURVATURE SOLVE
C
              IF(TEMPC(I)(1:4).EQ.'MARA') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  ZMXERROR=.FALSE.
                  CALL ATODZMX(VALA,VALV,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2080) VALV
 2080             FORMAT('PUY,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C
C     CENTER OF CURVATURE SOLVE
C
              IF(TEMPC(I)(1:4).EQ.'TOCO') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  ZMXERROR=.FALSE.
                  CALL ATOIZMX(VALA,IVALV,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2083) IVALV
 2083             FORMAT('PUY,',I3)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CENTER OF CURVATURE SOLVE DONE
C
C     APLANATIC MARGINAL CURVATURE SOLVE
C
              IF(TEMPC(I)(1:4).EQ.'APLA') THEN
                  WRITE(OUTLYNE,2081)
 2081             FORMAT('APY')
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     MARGINAL RAY CURVATURE SOLVE DONE
C
C     CURVATURE CURV
C
              IF(TEMPC(I)(1:4).EQ.'CURV') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  ZMXERROR=.FALSE.
                  CALL ATODZMX(VALA,VALV,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2042) VALV
 2042             FORMAT('CV,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CURVATURE DONE
C
C     CURVATURE PIKUP
C
              IF(TEMPC(I)(1:4).EQ.'PCUP') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2086) INT(WD(1)),WD(2)
 2086             FORMAT('PIKUP CV,',I3,',',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CURVATURE PIKUP DONE
C
C     CONIC PIKUP
C
              IF(TEMPC(I)(1:4).EQ.'PKUP') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2087) INT(WD(1)),WD(2)
 2087             FORMAT('PIKUP CC,',I3,',',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CONIC PIKUP DONE
C
C     THICKNESS PIKUP
C
              IF(TEMPC(I)(1:4).EQ.'PZUP') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2088) INT(WD(1)),WD(2)
 2088             FORMAT('PIKUP TH,',I3,',',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     THICKNESS PIKUP DONE
C
C     RADII RADI
C
              IF(TEMPC(I)(1:4).EQ.'RADI') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  ZMXERROR=.FALSE.
                  CALL ATODZMX(VALA,VALV,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2084) VALV
 2084             FORMAT('RD,',D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     RADII DONE
C
C     CONIC CONI
C
              IF(TEMPC(I)(1:4).EQ.'CONI') THEN
                  TEMPER=TEMPC(I)(5:1024)
                  CALL LEFTJUST(TEMPER)
                  VALA=TEMPER(1:23)
                  CALL RIGHTJUST(VALA)
                  ZMXERROR=.FALSE.
                  CALL ATODZMX(VALA,VALV,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  WRITE(OUTLYNE,2053) VALV
 2053             FORMAT('CC,'D23.15)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT(1:132)=OUTLYNE(1:132)
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CONIC DONE
C
C     DETERMINE GLASS TYPE
C
              IF(TEMPC(I)(1:4).EQ.'GLAS') THEN
                  TEMPER=TEMPC(I)(5:1024)
C     REMOVE UP TO 10 LEADING BLANKS
                  DO J=1,10
                      IF(TEMPER(1:1).EQ.' ') TEMPER(1:1024)=TEMPER(2:1024)//' '
                  END DO
C     FIND NEXT BLANK
                  J=1
                  DO WHILE(TEMPER(J:J).NE.' ')
                      J=J+1
                  END DO
                  IF(TEMPER(1:3).EQ.'AIR') GLASSA='AIR                        '
                  IF(TEMPER(1:4).EQ.'REFL') GLASSA='REFL                       '
                  IF(TEMPER(1:6).EQ.'MIRROR') GLASSA='REFL                       '
                  IF(TEMPER(1:3).NE.'AIR'.AND.TEMPER(1:4).NE.'REFL'.AND.
     1            TEMPER(1:6).NE.'MIRROR')
     2            GLASSA='GLA '//TEMPER(1:J-1)
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              ELSE
C     NOT GLAS
              END IF
C     GLASSA DETERMINED
C
C     CIRCULAR CLAP DIMENSIONS
C
              IF(TEMPC(I)(1:4).EQ.'CLAP') THEN
C     PROCEED
                  TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  CLTYPE='C'
                  CL1=WD(2)
                  CL2=WD(2)
                  CLAP1=.TRUE.
                  CLAP2=.TRUE.
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CIRCULAR CLAP DIMENSIONS DONE
C
C     CIRCULAR DIAM DIMENSIONS
C
              IF(TEMPC(I)(1:4).EQ.'DIAM') THEN
C     PROCEED
                  TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),3,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  CLTYPE='C'
                  CL1=WD(1)
                  CL2=WD(1)
                  CLAP1=.TRUE.
                  CLAP2=.TRUE.
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CIRCULAR CLAP DIMENSIONS DONE
C
C     ELLIPTICAL CLAP DIMENSIONS
C
              IF(TEMPC(I)(1:4).EQ.'ELAP') THEN
C     PROCEED
                  TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  CLTYPE='E'
                  CL1=WD(2)
                  CL2=WD(1)
                  CLAP1=.TRUE.
                  CLAP2=.TRUE.
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     ELIPTICAL CLAP DIMENSIONS DONE
C
C     RECTANGULAR CLAP DIMENSIONS
C
              IF(TEMPC(I)(1:4).EQ.'SQAP') THEN
C     PROCEED
                  TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  CLTYPE='R'
                  CL1=WD(2)
                  CL2=WD(1)
                  CLAP1=.TRUE.
                  CLAP2=.TRUE.
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     RECTANGULAR CLAP DIMENSIONS DONE
C
C     APERTURE DECENTERS
C
              IF(PREV(1:4).EQ.'CLAP'.OR.
     1        PREV(1:4).EQ.'ELAP'.OR.
     2        PREV(1:4).EQ.'SQAP') THEN
                  IF(TEMPC(I)(1:4).EQ.'OBDC') THEN
C     PROCEED
                      TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
                      ZMXERROR=.FALSE.
                      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
                      IF(ZMXERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
                      CLDX=.TRUE.
                      CLDY=.TRUE.
                      CL3=WD(2)
                      CL4=WD(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  END IF
              END IF
C
C     COBS DIMENSIONS
C
              IF(TEMPC(I)(1:4).EQ.'OBSC') THEN
C     PROCEED
                  TEMPER=TEMPC(I)(5:1024)
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  COTYPE='C'
                  CO1=WD(2)
                  CO2=WD(2)
                  COBS1=.TRUE.
                  COBS2=.TRUE.
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     CIRCULAR COBS DIMENSIONS DONE
C
C     ELLIPTICAL CLAP DIMENSIONS
C
              IF(TEMPC(I)(1:4).EQ.'ELOB') THEN
C     PROCEED
                  TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  COTYPE='E'
                  CO1=WD(2)
                  CO2=WD(1)
                  COBS1=.TRUE.
                  COBS2=.TRUE.
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     ELIPTICAL COBS DIMENSIONS DONE
C
C     RECTANGULAR COBS DIMENSIONS
C
              IF(TEMPC(I)(1:4).EQ.'SQOB') THEN
C     PROCEED
                  TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
                  ZMXERROR=.FALSE.
                  CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
                  IF(ZMXERROR) THEN
                      WRITE(38,4001) TEMPCC(1:78)
                      GO TO 8888
                  END IF
                  COTYPE='R'
                  CO1=WD(2)
                  CO2=WD(1)
                  COBS1=.TRUE.
                  COBS2=.TRUE.
                  TEMPC(I)(1:1024)=BL1024(1:1024)
                  GO TO 8888
              END IF
C     RECTANGULAR COBS DIMENSIONS DONE
C
C     OBSCURATION DECENTERS
C
              IF(PREV.EQ.'OBSC'.OR.
     1        PREV(1:4).EQ.'ELOB'.OR.
     2        PREV(1:4).EQ.'SQOB') THEN
                  IF(TEMPC(I)(1:4).EQ.'OBDC') THEN
C     PROCEED
                      TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
                      ZMXERROR=.FALSE.
                      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
                      IF(ZMXERROR) THEN
                          WRITE(38,4001) TEMPCC(1:78)
                          GO TO 8888
                      END IF
                      COBX=.TRUE.
                      COBY=.TRUE.
                      CO3=WD(2)
                      CO4=WD(1)
                      TEMPC(I)(1:1024)=BL1024(1:1024)
                      GO TO 8888
                  END IF
              END IF
C
              IF(TEMPC(I)(1:2).EQ.'GO') THEN
                  TEMPC(I)(1:1024)=BL1024(1:1024)
              END IF
C     IF WE ARE HERE, THE COMMAND DID NOT TRANSLATE
              IF(TEMPC(I)(1:75).NE.BL1024(1:78)) THEN
                  WRITE(38,4001) TEMPCC(1:78)
              END IF
 4001         FORMAT(' ',A78)
C     DO NEXT LINE IN THE ZEMAX FILE
 8888         CONTINUE
              PREV(1:80)=TEMPCCC(1:80)
          END DO
          IF(GLASSA(1:3).EQ.'   ') GLASSA='AIR                        '
          WRITE(OUTLYNE,2016) GLASSA
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT(1:132)=OUTLYNE(1:132)
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          SURFER=SURFER+1
          WRITE(OUTLYNE,2089)
 2089     FORMAT('EOS')
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT(1:132)=OUTLYNE(1:132)
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          CALL CLOSE_FILE(37,1)
          CALL CLOSE_FILE(38,1)
          DEALLOCATE (TEMPA,TEMPB,TEMPC,STAT=ALLOERR)
          RETURN
 100      FORMAT(A132)
      END
C
      SUBROUTINE SAVEFONT(I)
          IMPLICIT NONE
          INTEGER I
          include 'datmai.inc'
          OPEN(UNIT=110,ACCESS='SEQUENTIAL',BLANK='NULL'
     1      ,FORM='FORMATTED',FILE=trim(HOME)//'FONTSAVE.DAT'
     2      ,STATUS='UNKNOWN')
          REWIND(UNIT=110)
          IF(I.LT.2.OR.I.GT.36) I=8
          WRITE(110,*) I
          CALL CLOSE_FILE(110,1)
      END
