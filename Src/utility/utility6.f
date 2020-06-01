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

C       SIXTH SET OF UTILTIY ROUTINES GO HERE

      SUBROUTINE SHOWREG
          IMPLICIT NONE
          INTEGER I
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
          IF(STI.EQ.1) THEN
              OUTLYNE='"SHOW" SHOWS A REGISTER VALUE'
              CALL SHOWIT(1)
              OUTLYNE='OR SHOWS A "GOTTEN" PARAMETER VALUE'
              CALL SHOWIT(1)
              OUTLYNE='IF A QUALIFIER WORD IS INCLUDED'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(S1.EQ.0) THEN
              OUTLYNE=
     1        '"SHOW" WITHOUT A QUALIFIER REQUIRES EXPLICIT NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"SHOW" WITHOUT A QUALIFIER ONLY TAKES NUMERIC WORD #1 AND #2'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE=
     1        '"SHOW" TAKES NO ALPHANUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.0.0D0.OR.INT(W1).GT.MAXREG) THEN
              OUTLYNE=
     1        'FOR "SHOW" WITH NO QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #1 MUST LIE BETWEEN 1 AND ',MAXREG
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.AND.W2.LE.0.0D0.OR.S2.EQ.1.AND.INT(W2).GT.MAXREG) THEN
              OUTLYNE=
     1        'FOR "SHOW" WITH NO QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #2 MUST LIE BETWEEN 1 AND ',MAXREG
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.AND.W2.LT.W1) THEN
              OUTLYNE=
     1        'FOR "SHOW" WITH NO QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'NUMERIC WORD #2 MUST BE GREATER THAN OR EQUAL TO NUMERIC WORD #1'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     INPUT OK
          IF(S2.EQ.0) W2=W1
C
  10      FORMAT('GENERAL PURPOSE REGISTER # ',I6,' = ',D23.15)
          DO I=INT(W1),INT(W2)
              WRITE(OUTLYNE,10)I,GPREG(I)
              CALL SHOWIT(0)
          END DO
          RETURN
      END


      SUBROUTINE PUSH_STACK
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
!      INTEGER I
C     COPY X TO LAST X
          REG(40)=REG(9)
          REG(12)=REG(11)
          REG(11)=REG(10)
          REG(10)=REG(9)
      END


      SUBROUTINE PULL_STACK
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
!      INTEGER I
C     COPY X TO LAST X
C
          REG(10)=REG(11)
          REG(11)=REG(12)
      END


      SUBROUTINE CHADIR

          USE opsys
          IMPLICIT NONE

          CHARACTER DIRNAMM*80

C       THIS IS SUBROUTINE CHADIR. IT IS USED TO CHANGE THE NAME
C       DIRECTORY OF THE LENS LIBRARY, MACRO LIBRARY, TRASMISSION FILES
C       AND PLOT LIBRARY.
C
C               COMANDS ARE LENDIR, MACDIR (CHGMAC), TRADIR AND PLTDIR
C
C               IF THE DIRECORIES DONT EXIST, THEY ARE CREATED
C               THE NEW NAMES ARE STORED IN THE VARIABLES
C
C                   LIBLEN,LIBMAC,LIBTRA AND LIBPLO
C
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'LENDIR') THEN
                  WRITE(OUTLYNE,10) LIBLEN
                  CALL SHOWIT(0)
              ELSE
              END IF
              IF(WC.EQ.'MACDIR'.OR.WC.EQ.'CHGMAC') THEN
                  WRITE(OUTLYNE,20) LIBMAC
                  CALL SHOWIT(0)
              ELSE
              END IF
              IF(WC.EQ.'TRADIR') THEN
                  WRITE(OUTLYNE,30) LIBTRA
                  CALL SHOWIT(0)
              ELSE
              END IF
              IF(WC.EQ.'PLTDIR') THEN
                  WRITE(OUTLYNE,40) LIBPLO
                  CALL SHOWIT(0)
              ELSE
              END IF
              RETURN
          ELSE
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1)THEN
              OUTLYNE=
     1        '"'//WC(1:6)//'" TAKES ONLY QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(SQ.EQ.0) THEN
              OUTLYNE=
     1        '"'//WC(1:6)//'" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(WQ(1:1).EQ.' '.OR.WQ(2:2).EQ.' '.OR.WQ(3:3).EQ.' '
     1    .OR.WQ(4:4).EQ.' '.OR.WQ(5:5).EQ.' '.OR.WQ(6:6).EQ.' ') THEN
              OUTLYNE=
     1        'NEW DIRECTORY NAME MUST BE EXACTLY 6 NON-BLANK CHARACTERS'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'IN LENGTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(WQ(7:7).NE.' '.OR.WQ(8:8).NE.' ') THEN
              OUTLYNE=
     1        'NEW DIRECTORY NAME MAY BE ONLY 6 NON-BLANK CHARACTERS'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'IN LENGTH'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
C       INPUT OK
          IF(WC.EQ.'LENDIR') LIBLEN = trim(HOME)//WQ(1:6)//'/'
          IF(WC.EQ.'MACDIR'.OR.WC.EQ.'CHGMAC')
     &    LIBMAC = trim(HOME)//WQ(1:6)//'/'
          IF(WC.EQ.'TRADIR') LIBTRA = trim(HOME)//WQ(1:6)//'/'
          IF(WC.EQ.'PLTDIR') LIBPLO = trim(HOME)//WQ(1:6)//'/'
          
C     DO THE DIRECTORIES EXIST ? IF NOT CREATE THEM
          DIRNAMM=trim(HOME)//TRIM(WQ)
          CALL os_newdir(DIRNAMM)
          IF(WC.EQ.'LENDIR') THEN
              WRITE(OUTLYNE,10) trim(LIBLEN)
              CALL SHOWIT(0)
          ELSE
          END IF
          IF(WC.EQ.'MACDIR'.OR.WC.EQ.'CHGMAC') THEN
              WRITE(OUTLYNE,20) trim(LIBMAC)
              CALL SHOWIT(0)
          ELSE
          END IF
          IF(WC.EQ.'TRADIR') THEN
              WRITE(OUTLYNE,30) trim(LIBTRA)
              CALL SHOWIT(0)
          ELSE
          END IF
          IF(WC.EQ.'PLTDIR') THEN
              WRITE(OUTLYNE,40) trim(LIBPLO)
              CALL SHOWIT(0)
          ELSE
          END IF
C
          RETURN
 10       FORMAT('CURRENT LENS LIBRARY DIRECTORY IS (',A40,')')
 20       FORMAT('CURRENT MACRO LIBRARY DIRECTORY IS (',A40,')')
 30       FORMAT('CURRENT TRANSMISSION FILE DIRECTORY IS (',A40,')')
 40       FORMAT('CURRENT PLOT LIBRARY DIRECTORY IS (',A40,')')
      END


      SUBROUTINE LENSLOC

          USE opsys
          IMPLICIT NONE

C     THIS IS SUBROUTINE LENSLOC. IT IS USED TO CHANGE THE NAME
C     DIRECTORY FOR LENSSAVE/LENSREST
C
          INCLUDE 'datmai.inc'
C
C     CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
C     PRINT ERROR AND RETURN IF DISCOVERED.
     
C     WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'LENSLOC') THEN
                  WRITE(OUTLYNE,10)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,11) DIRLEN
                  CALL SHOWIT(0)
              END IF
              RETURN
          ELSE
          END IF
          IF(SQ.EQ.1.OR.SN.EQ.1)THEN
              OUTLYNE=
     1        '"LENSLOC" TAKES ONLY STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.0) THEN
              OUTLYNE=
     1        '"LENSLOC" REQUIRES EXPLICIT STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
           END IF

           ! set lens directory
           IF (ws .EQ. ".") THEN
              CALL getcwd(DIRLEN)
           ELSE IF (ws .EQ. "~") THEN
              DIRLEN = trim(HOME)//'LENSES'
           ELSE
              DIRLEN = TRIM(WS)
           END IF

           CALL add_dir_slash( DIRLEN )
           RETURN
           
 10       FORMAT('CURRENT LENS DIRECTORY NAME IS:')
 11       FORMAT(A79)
      END


      SUBROUTINE LENSDIR
      
          USE opsys
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE LENSDIR. IT IS USED TO LIST THE
C       DIRECTORY FOR LENSSAVE/LENSREST
C
          INCLUDE 'datmai.inc'
C
C               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
C               PRINT ERROR AND RETURN IF DISCOVERED.
C
C               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!        IF(STI.EQ.1) THEN
          IF(WC.EQ.'LENSDIR') THEN
              WRITE(OUTLYNE,10)
              CALL SHOWIT(0)
              WRITE(OUTLYNE,11) DIRLEN
              CALL SHOWIT(0)
          END IF
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1)THEN
              OUTLYNE=
     1        '"LENSDIR" TAKES NO ADDITIONAL INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       INPUT OK
C
C     LIST THE DIRECTORY
C
          CALL os_listdir( DIRLEN )

          RETURN
 10       FORMAT('THE CURRENT LENS DIRECTORY NAME IS:')
 11       FORMAT(A79)
      END


C SUB DODODO.FOR
      SUBROUTINE DODODO
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO DO THE "DO" STATEMENT
          CHARACTER AAW1*23,DOWQ*8
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"DO" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'NO ADDITIONAL INFORMATION'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"DO" TAKES NO NUMERIC WORD #1, #2 AND #3 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)'"DO" REQUIRES EXPLICIT QUALIFIER INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0) THEN
              WRITE(OUTLYNE,*)'"DO" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.0) THEN
              WRITE(OUTLYNE,*)'"DO" REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C DO THE DO OPERATION
          DOWQ=WQ
C
          DO I=INT(W1),INT(W2)
C     NOTE:THIS WILL TAKE ANY QUALIFIER WORD
              CALL DONTOA(DBLE(I),AAW1)
              INPUT(1:80)=DOWQ(1:8)//','//AAW1(1:23)//',,,,,'
              CALL PROCES
          END DO
C
          RETURN
      END
C SUB DONTOA.FOR
      SUBROUTINE DONTOA(I,AI)
C
          IMPLICIT NONE
C
          REAL*8 I
C
          CHARACTER B*80,AI*23
C
          WRITE(B,100) I
          READ(B,200) AI
 100      FORMAT(D23.15)
 200      FORMAT(A23)
          RETURN
      END
C SUB FLAG.FOR
      SUBROUTINE FLAG
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS USED TO SET ONE OF 20 PROGRAM FLAGS
C       WHICH CAN THEN BE TESTED BY OTHER DECISION MAKING PROGRAM
C       COMMANDS. THE PROGRAM FLAGS ARE STORED IN THE INTERNAL FLAGS
C       ARRAY FLG(0:20) AND ARE PASSED FROM ONE SUBROUTINE TO THE
C       NEXT VIA THE FL COMMON.
C
          INTEGER W(1:5),IW(1:5),FLG(0:20),I
C
          REAL*8 AW1,AW2,AW3,AW4,AW5
C
          COMMON/FFL/FLG
C
          INCLUDE 'datmai.inc'
C
C       THE SPECIFIC FLAG TO BE SET IS DESIGNATED BY THE
C       VALUE1 OF W1 THROUGH W5. IF THE VALUE1 IS POSITIVE,
C       THE FLAG IS SET. IF THE VALUE1 IS NEGATIVE, THE FLAG
C       IS CLEARED. SET ASSIGNS A VALUE1 OF 1, CLEARED ASSIGNS
C       A VALUE1 OF -1 TO THE PARTICULAR FLAG. IF ALL THE NUMERIC
C       WORDS (W1 THROUGH W5) ARE ZERO, THE CURRENT SETTING
C       OF EACH FLAG IS PRINTED.
C
C       ARE ALL THE NUMERIC WORDS DEFAULT?
C
          IF(SST.EQ.1.OR.SQ.EQ.1) THEN
              IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1.AND.
     1        DF5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"FLAG" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       PRINT OUT THE FLAG VALUE1S TO THE DEFAULT OUTPUT DEVICE.
              WRITE(OUTLYNE,1000) FLG(1)*1,FLG(2)*2,FLG(3)*3,
     1        FLG(4)*4,FLG(5)*5
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000) FLG(6)*6,FLG(7)*7,FLG(8)*8,
     1        FLG(9)*9,FLG(10)*10
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000) FLG(11)*11,FLG(12)*12,FLG(13)*13,
     1        FLG(14)*14,FLG(15)*15
              CALL SHOWIT(0)
              WRITE(OUTLYNE,1000) FLG(16)*16,FLG(17)*17,FLG(18)*18,
     1        FLG(19)*19,FLG(20)*20
              CALL SHOWIT(0)
C
          ELSE
C       NOT ALL THE NUMERIC WORDS WERE ZERO
C       TO HANDLE DEFAULT INPUT, SHOULD IT OCCUR,DEFAULT
C       VALUE1S ARE CONSIDERED ZERO AND IGNORED.
C       ONLY VALUE1S FROM 1.0 TO 20.0  AND -1.0 TO -20.0
C       ARE CONSIDERED VALID. IF AND NUMERIC WORD IS BEYOND
C       THIS LEGAL RANGE, PROCESS NONE OF THE VALUE1S,
C       PRINT AN ERROR MESSAGE TO THE SCREEN AND RETURN.
C
              AW1=DABS(W1)
              AW2=DABS(W2)
              AW3=DABS(W3)
              AW4=DABS(W4)
              AW5=DABS(W5)
C
              IF(AW1.NE.0.0D0) THEN
                  IF(AW1.LT.1.0D0.OR.AW1.GT.20.0D0) THEN
                      WRITE(OUTLYNE,*)'FLAG VALUE1 BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(AW2.NE.0.0D0) THEN
                  IF(AW2.LT.1.0D0.OR.AW2.GT.20.0D0) THEN
                      WRITE(OUTLYNE,*)'FLAG VALUE1 BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(AW3.NE.0.0D0) THEN
                  IF(AW3.LT.1.0D0.OR.AW3.GT.20.0D0) THEN
                      WRITE(OUTLYNE,*)'FLAG VALUE1 BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(AW4.NE.0.0D0) THEN
                  IF(AW4.LT.1.0D0.OR.AW4.GT.20.0D0) THEN
                      WRITE(OUTLYNE,*)'FLAG VALUE1 BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(AW5.NE.0.0D0) THEN
                  IF(AW5.LT.1.0D0.OR.AW5.GT.20.0D0) THEN
                      WRITE(OUTLYNE,*)'FLAG VALUE1 BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
          END IF
C       NUMERIC WORDS ARE NOT ARE ZERO AND ARE ALL IN LEGAL RANGE
C
          W(1)=INT(W1)
          W(2)=INT(W2)
          W(3)=INT(W3)
          W(4)=INT(W4)
          W(5)=INT(W5)
          IW(1)=ABS(W(1))
          IW(2)=ABS(W(2))
          IW(3)=ABS(W(3))
          IW(4)=ABS(W(4))
          IW(5)=ABS(W(5))
C
          DO 350 I=1,5
              GOTO(21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38
     1        ,39,40) IW(I)
C
 21           IF(W(I).GT.0) FLG(1)=1
              IF(W(I).LT.0) FLG(1)=-1
              GO TO 350
 22           IF(W(I).GT.0) FLG(2)=1
              IF(W(I).LT.0) FLG(2)=-1
              GO TO 350
 23           IF(W(I).GT.0) FLG(3)=1
              IF(W(I).LT.0) FLG(3)=-1
              GO TO 350
 24           IF(W(I).GT.0) FLG(4)=1
              IF(W(I).LT.0) FLG(4)=-1
              GO TO 350
 25           IF(W(I).GT.0) FLG(5)=1
              IF(W(I).LT.0) FLG(5)=-1
              GO TO 350
 26           IF(W(I).GT.0) FLG(6)=1
              IF(W(I).LT.0) FLG(6)=-1
              GO TO 350
 27           IF(W(I).GT.0) FLG(7)=1
              IF(W(I).LT.0) FLG(7)=-1
              GO TO 350
 28           IF(W(I).GT.0) FLG(8)=1
              IF(W(I).LT.0) FLG(8)=-1
              GO TO 350
 29           IF(W(I).GT.0) FLG(9)=1
              IF(W(I).LT.0) FLG(9)=-1
              GO TO 350
 30           IF(W(I).GT.0) FLG(10)=1
              IF(W(I).LT.0) FLG(10)=-1
              GO TO 350
 31           IF(W(I).GT.0) FLG(11)=1
              IF(W(I).LT.0) FLG(11)=-1
              GO TO 350
 32           IF(W(I).GT.0) FLG(12)=1
              IF(W(I).LT.0) FLG(12)=-1
              GO TO 350
 33           IF(W(I).GT.0) FLG(13)=1
              IF(W(I).LT.0) FLG(13)=-1
              GO TO 350
 34           IF(W(I).GT.0) FLG(14)=1
              IF(W(I).LT.0) FLG(14)=-1
              GO TO 350
 35           IF(W(I).GT.0) FLG(15)=1
              IF(W(I).LT.0) FLG(15)=-1
              GO TO 350
 36           IF(W(I).GT.0) FLG(16)=1
              IF(W(I).LT.0) FLG(16)=-1
              GO TO 350
 37           IF(W(I).GT.0) FLG(17)=1
              IF(W(I).LT.0) FLG(17)=-1
              GO TO 350
 38           IF(W(I).GT.0) FLG(18)=1
              IF(W(I).LT.0) FLG(18)=-1
              GO TO 350
 39           IF(W(I).GT.0) FLG(19)=1
              IF(W(I).LT.0) FLG(19)=-1
              GO TO 350
 40           IF(W(I).GT.0) FLG(20)=1
              IF(W(I).LT.0) FLG(20)=-1
              GO TO 350
 350      CONTINUE
C
          RETURN
C
 1000     FORMAT(I3,2X,I3,2X,I3,2X,I3,2X,I3)
C
      END
C SUB SETPMT.FOR
      SUBROUTINE SETPMT
          IMPLICIT NONE

C       THIS SUBROUTINE IS USED TO SET THE USER PROMPT
C
          CHARACTER PMTVAL*80
C
          COMMON/TELPRM/PMTVAL
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PROMPT" IS USED TO SET THE PROMPT STRING FOR PROMPTED INPUT'
              CALL SHOWIT(1)
              IF(PMTVAL(1:10).EQ.'          ')
     1        WRITE(OUTLYNE,*) 'THE CURRENT PROMPT IS : (BLANK)'
              IF(PMTVAL(1:10).NE.'          ')
     1        WRITE(OUTLYNE,*) 'THE CURRENT PROMPT IS : ',PMTVAL
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"PROMPT" TAKES NO QUALIFIER OR NUMERIC INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"PROMPT" REQUIRES EXPLICIT STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          PMTVAL=WS(1:80)
          RETURN
      END


C SUB GPRGA.FOR
      SUBROUTINE GPRGA
          IMPLICIT NONE

C       THIS SUBROUTINE IS USED FOR GENERAL CHARACTER REGISTERS
C
          INTEGER I,IVL
C
          CHARACTER AAD*20,B*7
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"ASTO", "ARCL", "CLASTO" AND "AWRITE" MANIPULATE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'ALPHA STORAGE REGISTERS'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SN.EQ.1) THEN
              IF(WC.NE.'ARCL'.AND.WC.NE.'AWRITE') THEN
                  WRITE(OUTLYNE,*)
     1            '"ASTO" AND "CLASTO" COMMANDS TAKE NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'ASTO'.AND.SQ.EQ.0.OR.WC.EQ.'ASTO'.AND.
     1    SST.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"ASTO" REQUIRES EXPLICIT QUALIFIER AND STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'ARCL'.AND.S1.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"ARCL" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'AWRITE'.AND.S1.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"AWRITE" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'ARCL') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"ARCL" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.1.OR.INT(W1).GT.MAXREG) THEN
                  WRITE(OUTLYNE,*)
     1            '"ARCL" STORAGE REGISTER NUMBER BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VALID INPUT IS 1 TO ',MAXREG
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'AWRITE') THEN
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"AWRITE" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.1.0D0.OR.INT(W1).GT.MAXREG) THEN
                  WRITE(OUTLYNE,*)
     1            '"AWRITE" STORAGE REGISTER NUMBER BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VALID INPUT IS 1 TO ',MAXREG
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'CLASTO'.AND.SQ.EQ.1.OR.WC.EQ.'CLASTO'.AND.
     1    SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"CLASTO" TAKES NO QUALIFIER OR STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'ARCL') THEN
              AGPREG(0)(1:80)=AGPREG(INT(W1))(1:80)
              RETURN
          END IF
          IF(WC.EQ.'AWRITE') THEN
              IF(OUT.NE.6.AND.OUT.NE.7)
     1        WRITE(OUTLYNE,987) AGPREG(INT(W1))(1:80)
              IF(OUT.EQ.6.OR.OUT.EQ.7)
     1        WRITE(OUTLYNE,9871) AGPREG(INT(W1))(1:79)
              CALL SHOWIT(0)
 987          FORMAT(A80)
 9871         FORMAT(A79)
              RETURN
          END IF
          IF(WC.EQ.'CLASTO')THEN
C       THIS IS USED TO CLEAR GENERAL PURPOSE CHARACTER
C       REGISTERS AGPREG(1) TO AGPREG(MACREG)
              AAD='                    '
              DO I=1,MAXREG
                  AGPREG(I)=AAD//AAD//AAD//AAD//AAD//AAD//AAD
              END DO
              WRITE(OUTLYNE,*)
     1        'GENERAL PURPOSE CHARACTER REGISTERS HAVE BEEN CLEARED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WC.EQ.'ASTO') THEN
C     "ASTO"
C     CHECK FOR VALID QUALIFIER WORDS FOR "ASTO"
              IF(WQ(1:1).EQ.'A') THEN
                  WRITE(B,110,ERR=98) WQ(2:8)
                  READ(B,*,ERR=98) IVL
                  IF(IVL.GE.1.AND.IVL.LE.MAXREG) GO TO 20
 110              FORMAT(A7)
  98              CONTINUE
              END IF
              WRITE(OUTLYNE,*)
     1        'INVALID QUALIFIER WORD FOUND'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
 20           CONTINUE
              I=IVL
              AGPREG(I)(1:80)=WS(1:80)
          END IF
          RETURN
      END
C SUB AGET.FOR
      SUBROUTINE AGET
C
          IMPLICIT NONE
C
C
          CHARACTER AAVAL*80
C
          LOGICAL ONESY,TWOSY
C
          INTEGER NUM5
C
          CHARACTER REMWQ*8
          COMMON/WQREM/REMWQ
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datsub.inc'
C
          AAVAL=AA//AA//AA//AA
C
          IF(DF5.EQ.1) THEN
              W5=1.0D0
              S5=1
              DF1=0
              SN=1
          END IF
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"AGET" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK FOR GETABLE THINGS
C
          IF(WQ.NE.'LI'.AND.WQ.NE.'LIC1'.AND.WQ.NE.'LIC2'
     1    .AND.WQ.NE.'LIC3'.AND.WQ.NE.'LIC4'.AND.WQ.NE.'CATNAME'
     2    .AND.WQ.NE.'GLANAME'.AND.WQ.NE.'SURFLBL') THEN
C
C       NO MATCH WAS FOUND.
              WRITE(OUTLYNE,*)'PARAMETER ',WQ,' IS NOT "AGET"-ABLE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C
C     FIRST, GET COMMANDS WITH NO NUMBERIC INPUT USED
          ONESY=.FALSE.
          IF(WQ.EQ.'LI')     ONESY=.TRUE.
          IF(WQ.EQ.'LIC1')   ONESY=.TRUE.
          IF(WQ.EQ.'LIC2')   ONESY=.TRUE.
          IF(WQ.EQ.'LIC3')   ONESY=.TRUE.
          IF(WQ.EQ.'LIC4')   ONESY=.TRUE.
          IF(ONESY) THEN
              IF(S1.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"AGET ',WQ,'" ONLY USES NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C     FIRST, GET COMMANDS WITH ONE INPUT USED
          TWOSY=.FALSE.
          IF(WQ.EQ.'CATNAME')     TWOSY=.TRUE.
          IF(WQ.EQ.'GLANAME')     TWOSY=.TRUE.
          IF(WQ.EQ.'SURFLBL')     TWOSY=.TRUE.
          IF(TWOSY) THEN
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"AGET ',WQ,'" ONLY USES NUMERIC WORDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'#1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'CATNAME'.OR.WQ.EQ.'GLANAME'.OR.WQ.EQ.'SURFLBL') THEN
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(WQ.EQ.'LI') THEN
              AAVAL=LI
              GO TO 200
          END IF
          IF(WQ.EQ.'LIC1') THEN
              AAVAL=LIC(1)
              GO TO 200
          END IF
          IF(WQ.EQ.'LIC2') THEN
              AAVAL=LIC(2)
              GO TO 200
          END IF
          IF(WQ.EQ.'LIC3') THEN
              AAVAL=LIC(3)
              GO TO 200
          END IF
          IF(WQ.EQ.'LIC4') THEN
              AAVAL=LIC(4)
              GO TO 200
          END IF
          IF(WQ.EQ.'CATNAME') THEN
              AAVAL(1:13)=GLANAM(INT(W1),1)
              GO TO 200
          END IF
          IF(WQ.EQ.'GLANAME') THEN
              AAVAL(1:13)=GLANAM(INT(W1),2)
              GO TO 200
          END IF
          IF(WQ.EQ.'SURFLBL') THEN
              GO TO 200
          END IF
          OUTLYNE='PARAMETER NOT CURRENTLY "GET"-ABLE'
          CALL SHOWIT(1)
          RETURN
C
 200      CONTINUE
          IF(AAVAL.EQ.AA//AA//AA//AA) THEN
              OUTLYNE='PARAMETER RETURNED WAS A BLANK'
              CALL SHOWIT(1)
          END IF
          NUM5=INT(W5)
          AGPREG(NUM5)=AAVAL
          IF(ASHOW.AND.AAVAL.NE.AA//AA//AA//AA) THEN
              WRITE(OUTLYNE,10) AAVAL(1:79)
 10           FORMAT(A79)
              CALL SHOWIT(0)
          END IF
          RETURN
      END
C SUB GETA.FOR
      SUBROUTINE GETA
C
          IMPLICIT NONE
C
          INTEGER NUM5
C
          REAL*8 VALUE1
C
          COMMON/GV/VALUE1,NUM5
C
          INCLUDE 'datmai.inc'
C
          REG(40)=REG(9)
          REG(9)=VALUE1
          IF(NUM5.GE.1.AND.NUM5.LE.400) THEN
              GPREG(NUM5)=VALUE1
          END IF
          RETURN
      END

      FUNCTION EDGTHK(I,K)
C
          IMPLICIT NONE
C
          REAL*8 EDGTHK
C
          LOGICAL ETERROR
C
          INTEGER I,K
C
          EXTERNAL SAGIT
C
          REAL*8 CA1,CA2,CA,TH12,SAGIT,SAG1,SAG2
C
          INCLUDE 'datlen.inc'
C
          ETERROR=.FALSE.
          EDGTHK = 0.d0
C
          IF (K.EQ.1) THEN
C     YZ PLANE CALCULATION FROM SURFACE I TO I+1
C     DETERMINE HEIGHTS AND THICKNESS FOR CALCULATION
              IF(ALENS(9,I).EQ.0.0D0.AND.ALENS(9,I+1).EQ.0.0D0) THEN
C     NO CLAPS
                  CA1=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                  CA2=DABS(PXTRAY(1,I+1))+DABS(PXTRAY(5,I+1))
                  TH12=ALENS(3,I)
              ELSE
C     SOME CLAPS
                  IF(ALENS(9,I).EQ.0.0D0) THEN
                      CA1=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
                  ELSE
                      IF(ALENS(9,I).EQ.1.0D0) THEN
                          IF(ALENS(10,I).LE.ALENS(11,I))CA1=DABS(ALENS(10,I))
                          IF(ALENS(10,I).GT.ALENS(11,I))CA1=DABS(ALENS(11,I))
                      ELSE
                          CA1=DABS(ALENS(10,I))
                      END IF
                  END IF
                  IF(ALENS(9,I+1).EQ.0.0D0) THEN
                      CA1=DABS(PXTRAY(1,I+1))+DABS(PXTRAY(5,I+1))
                  ELSE
                      IF(ALENS(9,I+1).EQ.1.0D0) THEN
                          IF(ALENS(10,I+1).LE.ALENS(11,I+1))CA1=DABS(ALENS(10,I+1))
                          IF(ALENS(10,I+1).GT.ALENS(11,I+1))CA1=DABS(ALENS(11,I+1))
                      ELSE
                          CA1=DABS(ALENS(10,I+1))
                      END IF
                  END IF
                  TH12=ALENS(3,I)
              END IF
              IF(CA1.GT.CA2) CA=CA1
              IF(CA1.LE.CA2) CA=CA2
C     NOW CA AND TH12 HAVE BEEN DETERMINED
C     CALC SAG1 AND SAG2
              SAG1=SAGIT(I,CA,1,ETERROR)
              SAG2=SAGIT(I+1,CA,1,ETERROR)
              IF(ETERROR) THEN
                  EDGTHK=0.0D0
              ELSE
                  EDGTHK=-SAG1+SAG2+TH12
              END IF
          END IF
           
          IF(K.EQ.2) THEN
C     XZ PLANE CLACULATION
C     DETERMINE HEIGHTS AND THICKNESS FOR CALCULATION
              IF(ALENS(9,I).EQ.0.0D0.AND.ALENS(9,I+1).EQ.0.0D0) THEN
C     NO CLAPS
                  CA1=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                  CA2=DABS(PXTRAX(1,I+1))+DABS(PXTRAX(5,I+1))
                  TH12=ALENS(3,I)
              ELSE
C     SOME CLAPS
                  IF(ALENS(9,I).EQ.0.0D0) THEN
                      CA1=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
                  ELSE
                      IF(ALENS(9,I).EQ.1.0D0) CA1=DABS(ALENS(10,I))
                      IF(ALENS(9,I).GT.1.0D0) CA1=DABS(ALENS(11,I))
                  END IF
                  IF(ALENS(9,I+1).EQ.0.0D0) THEN
                      CA1=DABS(PXTRAX(1,I+1))+DABS(PXTRAX(5,I+1))
                  ELSE
                      IF(ALENS(9,I+1).EQ.1.0D0) CA1=DABS(ALENS(10,I+1))
                      IF(ALENS(9,I+1).GT.1.0D0) CA1=DABS(ALENS(11,I+1))
                  END IF
                  TH12=ALENS(3,I)
              END IF
              IF(CA1.GT.CA2) CA=CA1
              IF(CA1.LE.CA2) CA=CA2
C     NOW CA AND TH12 HAVE BEEN DETERMINED
C     CALC SAG1 AND SAG2
              SAG1=SAGIT(I,CA,2,ETERROR)
              SAG2=SAGIT(I+1,CA,2,ETERROR)
              IF(ETERROR) THEN
                  EDGTHK=0.0D0
              ELSE
                  EDGTHK=-SAG1+SAG2+TH12
              END IF
          END IF

      END FUNCTION EDGTHK

      
      FUNCTION SAGIT(I,CA,K,ETERROR)
C
          IMPLICIT NONE
C
          INTEGER I,K
C
          LOGICAL ETERROR
C
          REAL*8 CA,SAGIT,SAG
C
          INCLUDE 'datlen.inc'
C
          IF(K.EQ.1) THEN
C     YZ PLANE CALCULATION FROM SURFACE I TO I+1
C     DETERMINE SAG FOR SURF I AND I+1 IN YZ-PLANE
              CALL SAGITT(I,CA,K,SAG,ETERROR)
              SAGIT=SAG

          ELSE IF(K.EQ.2) THEN
C     XZ PLANE CLACULATION
C     DETERMINE SAG FOR SURF I AND I+1 IN YZ-PLANE
              CALL SAGITT(I,CA,K,SAG,ETERROR)
              SAGIT=SAG

          ELSE
              SAGIT = 0.0
              ETERROR = .TRUE.
          END IF
      END
C SUB GET.FOR
      SUBROUTINE GET
          USE GLOBALS
          USE NSSMOD
C
          IMPLICIT NONE
C
C       SUBROUTINE GET SERVES TO PASS ANY OF A LARGE NUMBER
C       OF NUMERICAL VALES, CALCULATED AT VARIOUS LEVELS
C       OF THE PROGRAM(ONE AT A TIME) TO THE MACRO ACCUMULATOR
C       REG(9). THE VALUE1S WHICH CAN BE (GOTTEN) ARE STORED
C       IN VARIOUS COMMONS AND GOTTEN INTO THE ACCUMULATOR
C       BY THIS ROUTINE.
C
C       (NOTE FROM 4/7/91) THINGS TO ADD
C                       GET ALL THE SPD STATISTICAL DATA
C                       GET ALL THE CURRENT RAY DATA AND
C       LOAD IT INTO NUMERICAL REGISTERS STARTING AT #100
C
          LOGICAL ONESY,GPR,TF357,NEG,OPMAP,ZN,COEFJK,ERRR,ERRER,ERRER2
     1    ,GETTER,GERROR1,GERROR2,OPDERROR
C
          REAL*8 DERVAL,VALL
C
          INTEGER I,N,ERROR,ORIEN,IGET,JGET,ISURF
C
          CHARACTER WQLOCAL*8
C
          REAL*8 W2LOCAL
C
          COMMON/LOCALWQ/WQLOCAL,W2LOCAL
C
          COMMON/VALSUR/I

          COMMON/RETTEG/GETTER,DERVAL
C
          COMMON/OPOPMP/OPMAP
C
          INTEGER NUM5,NF,ITYP
C
          REAL*8 VALUE1,WV,COSARG,DUMMY,TEMPR1,TEMPR2,TEMPDIF,
     1    INDEX,DISP,VNUM,V1,EDGTHK,PARTL,TEMPSUM,VALVAL,DWORD1,DWORD2
     2    ,VHI,VLO,NWN1,NWN2,NWN3,NWN4,JPX,JPY,JPCY,JPCX,JPUY,JPUX,JPUCY,
     3    JPUCX,V2,V9,JA,JB
C
          COMMON/GV/VALUE1,NUM5
C
          COMMON/LGV/NEG
C
          COMMON/NSIN/NF,INDEX,DISP,VNUM,PARTL
C
          COMMON/PRCOM/WV,ITYP
C
          REAL*8 X(1:96)
C
          COMMON/SOLU/X
C
          CHARACTER REMWQ*8
          COMMON/WQREM/REMWQ

          LOGICAL ITERROR
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datsub.inc'
C
          EXTERNAL EDGTHK
C
          JA=COS_A_ANG
          JB=COS_B_ANG
          TF357=.FALSE.
          GPR=.FALSE.
          NEG=.FALSE.
C
          IF(DF5.EQ.1) THEN
              NUM5=0
          ELSE
              IF(INT(W5).LT.0.OR.INT(W5).GT.400) THEN
                  WRITE(OUTLYNE,*)'NUMERIC WORD #5 BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"GET" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK FOR GETABLE THINGS
C
          IF(WQ.NE.'CFG'.AND.WQ.NE.'UNITS'.AND.WQ.NE.'INS'
     1    .AND.WQ.NE.'REFS'.AND.WQ.NE.'ASTOP'.AND.WQ.NE.'PIYP'
     2    .AND.WQ.NE.'PY'.AND.WQ.NE.'PCY'.AND.WQ.NE.'PUY'.AND.WQ.NE.
     3    'PIXP'.AND.WQ.NE.'PIY'.AND.WQ.NE.'PUCY'.AND.WQ.NE.'PICY'
     4    .AND.WQ.NE.'PX'.AND.WQ.NE.'PCX'.AND.WQ.NE.'PUX'.AND
     5    .WQ.NE.'PIX'.AND.WQ.NE.'PUCX'.AND.WQ.NE.'PICX'.AND
     6    .WQ.NE.'CV'.AND.WQ.NE.'CVTOR'.AND.WQ.NE.'TH'.AND.WQ.NE.
     7    'AD'.AND.WQ.NE.'AE'.AND.WQ.NE.'AF'.AND.WQ.NE.'AG'.AND.
     8    WQ.NE.'CC'.AND.WQ.NE.'ALPHA'.AND.WQ.NE.'BETA'.AND.WQ.NE.
     9    'GAMMA'.AND.WQ.NE.'TCODE'.AND.WQ.NE.'XD'.AND.WQ.NE.'YD'
     1    .AND.WQ.NE.'AC'.AND.WQ.NE.'PICYP'.AND.WQ.NE.'PICXP'.AND.WQ
     2    .NE.'INR'.AND.WQ.NE.'GPY'.AND.WQ.NE.'GPX'.AND.WQ.NE.'GPCX'
     3    .AND.WQ.NE.'GPCY'.AND.WQ.NE.'GPUX'.AND.WQ.NE.'GPUY'.AND.
     4    WQ.NE.'SHRTWAVE'.AND.WQ.NE.'GPUCX'.AND.WQ.NE.'GPUCY'.AND.WQ
     5    .NE.'ZD'.AND.WQ.NE.'PSF'.AND.WQ.NE.'ISN'.AND.WQ.NE.'RLEN'.AND.
     6    WQ.NE.'OSN'.AND.WQ.NE.'SPTWT'.AND.WQ.NE.'GDX'.AND.WQ.NE.'ORLEN'
     7    .AND.WQ.NE.'GDY'.AND.WQ.NE.'GDZ'.AND.WQ.NE.'GALPHA'.AND.
     8    WQ.NE.'GBETA'.AND.WQ.NE.'GGAMMA'.AND.WQ.NE.'THM'.AND.
     9    WQ.NE.'PRICE'.AND.WQ.NE.'RAYCOD1'.AND.WQ.NE.'RAYCOD2') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
          IF(WQ.NE.'ACT     ') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C
C       GLOBAL VERTEX AND RAY DATA
C
          IF(WQ.NE.'XVERT'.AND.WQ.NE.'YVERT'.AND.WQ.NE.'ZVERT'
     1    .AND.WQ.NE.'LXVERT'.AND.WQ.NE.'MXVERT'.AND.WQ.NE.'NXVERT'
     2    .AND.WQ.NE.'LYVERT'.AND.WQ.NE.'MYVERT'.AND.WQ.NE.'NYVERT'
     3    .AND.WQ.NE.'LZVERT'.AND.WQ.NE.'MZVERT'.AND.WQ.NE.'NZVERT'
     4    .AND.WQ.NE.'GLX'.AND.WQ.NE.'GLY'.AND.WQ.NE.'GLZ'
     5    .AND.WQ.NE.'GLL'.AND.WQ.NE.'GLM'.AND.WQ.NE.'GLN'
     6    .AND.WQ.NE.'GLLOLD'.AND.WQ.NE.'GLMOLD'.AND.WQ.NE.'FLDSX'.AND.
     7    WQ.NE.'GLNOLD'.AND.WQ.NE.'DYA'.AND.WQ.NE.'DXA'.AND.WQ.NE.
     8    'FLDSY'.AND.WQ.NE.'AH'.AND.WQ.NE.'AJ'.AND.WQ.NE.'AI'.AND.
     9    WQ.NE.'AK'.AND.WQ.NE.'AL'.AND.WQ.NE.'GLLN'.AND.WQ.NE.'GLMN'
     1    .AND.WQ.NE.'GLNN') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C
C       NSS DATABASE ITEMS
C
          IF(WQ.NE.'NSSMEANX'.AND.WQ.NE.'NSSMEANY'.AND.
     1    WQ.NE.'NSSMEANR'.AND.WQ.NE.'NSSRMSX'.AND.
     1    WQ.NE.'NSSRMSY'.AND.WQ.NE.'NSSRMSR') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C
          IF(WQ.NE.'RD'.AND.WQ.NE.'RDTOR'.AND.WQ.NE.'Y'.AND.WQ.NE.
     2    'DY'.AND.WQ.NE.'X'.AND.WQ.NE.'DX'.AND.WQ.NE.'XANG'.AND
     3    .WQ.NE.'YANG'.AND.WQ.NE.'AII'.AND.WQ.NE.'Z'.AND.WQ.NE.
     4    'DCL'.AND.WQ.NE.'DCM'.AND.WQ.NE.'DCN'.AND.WQ.NE.'OPL'.AND.
     5    WQ.NE.'WV'.AND.WQ.NE.'PFAC'.AND.WQ.NE.'K'.AND.WQ.NE.'L'.AND.WQ
     6    .NE.'DINMUL'.AND.WQ.NE.'PXPX'.AND.WQ.NE.'PXPY'.AND.WQ.NE.'PYPX'
     7    .AND.WQ.NE.'PYPY'.AND.WQ.NE.'DXDX'.AND.WQ.NE.'DXDY'
     8    .AND.WQ.NE.'DYDX'.AND.WQ.NE.'DYDY'.AND.WQ.NE.'DYADY'.AND.WQ.NE.
     9    'DYADX'.AND.WQ.NE.'DXADX'.AND.WQ.NE.'DXADY'
     1    .AND.WQ.NE.'CTSX'.AND.WQ.NE.'CTSY'.AND.WQ.NE.'SCEX'.AND.
     1    WQ.NE.'SCEY'.AND.WQ.NE.'GRS'.AND.WQ.NE.'GRO'.AND.WQ.NE.'GRX'
     2    .AND.WQ.NE.'GRY'.AND.WQ.NE.'GRZ'.AND.WQ.NE.'RENERGY'
     3    .AND.WQ.NE.'MAXREG'.AND.WQ.NE.'MINREG'.AND.WQ.NE.'NUMHITS'
     4    .AND.WQ.NE.'RTOT'.AND.WQ.NE.'ITOT'.AND.WQ.NE.'M'.AND.WQ.NE.'ONTOL'
     5    .AND.WQ.NE.'SINGTOL'.AND.WQ.NE.'LINTOL') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C
          IF(WQ.NE.'XREF'.AND.WQ.NE.'YREF'.AND.WQ.NE.'ZREF'.AND.WQ
     1    .NE.'LREF'.AND.WQ.NE.'MREF'.AND.WQ.NE.'NREF'.AND.WQ.NE.
     2    'LENREF'.AND.WQ.NE.'OPLREF'.AND.WQ.NE.'IREF'.AND.WQ.NE.
     3    'IPREF'.AND.WQ.NE.'LNREF'.AND.WQ.NE.'MNREF'.AND.WQ.NE.
     4    'NNREF'.AND.WQ.NE.'XAREF'.AND.WQ.NE.'YAREF'.AND.WQ.NE.
     5    'DIFTOL'.AND.WQ.NE.'LEN'.AND.WQ.NE.'AIP'.AND.WQ.NE.'LN'
     6    .AND.WQ.NE.'MN'.AND.WQ.NE.'NN'.AND.WQ.NE.'LOLD'.AND.
     7    WQ.NE.'MOLD'.AND.WQ.NE.'NOLD'.AND.WQ.NE.'LREFOL'.AND.WQ.NE.
     8    'MREFOL'.AND.WQ.NE.'NREFOL'.AND.WQ.NE.'COATING') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
          IF(WQ.NE.'PXAPX'.AND.WQ.NE.'PXAPY'.AND.WQ.NE.'PYAPY'.AND.WQ.NE.
     1    'PYAPX'.AND.WQ.NE.'VNUM'.AND.WQ.NE.'MCODE'.AND.WQ.NE.'CLAP'.AND.
     2    WQ.NE.'CLAPY'.AND.WQ.NE.'CLAPX'.AND.WQ.NE.'COBS'.AND.WQ.NE.
     3    'COBSY'.AND.WQ.NE.'COBSX'.AND.WQ.NE.'PARTL'
     6    .AND.WQ.NE.'CENTX'.AND.WQ.NE.'CENTY'.AND.WQ.
     7    NE.'MODE'.AND.WQ.NE.'MATRIX'.AND.WQ.NE.'NVAR'.AND.WQ.NE.'KDWA'
     8    .AND.WQ.NE.'FOCRIT'.AND.WQ.NE.'VB'.AND.WQ.NE.'DERIV'.AND.WQ.NE.
     9    'VWFC'.AND.WQ.NE.'LCV'.AND.WQ.NE.'SPS'.AND.WQ.NE.'TV'
     1    .AND.WQ.NE.'SAG'.AND.WQ.NE.'RMSOPD'
     2    .AND.WQ.NE.'RMSX'.AND.WQ.NE.'RMSY'.AND.WQ.NE.'RMS'.AND.WQ.NE.
     3    'SPDTRANS'.AND.WQ.NE.'RSSX'.AND.WQ.NE.'RSSY'.AND.WQ.NE.'RSS'.AND.
     4    WQ.NE.'PTOVOPD'.AND.WQ.NE.'ABBE'.AND.WQ.NE.'INDEX'.AND.
     5    WQ.NE.'FCSFT'.AND.WQ.NE.'FCSFTX'.AND.WQ.NE.'FCSFTY'.AND.WQ.NE.
     6    'TRANS'.AND.WQ.NE.'N1'.AND.WQ.NE.'N2'.AND.WQ.NE.'N3'.AND.WQ.NE.
     7    'N4'.AND.WQ.NE.'N5'.AND.WQ.NE.'N6'.AND.WQ.NE.'N7'.AND.WQ.NE.'N8'
     8    .AND.WQ.NE.'N9'.AND.WQ.NE.'N10'.AND.WQ.NE.'DPART'.AND.WQ.NE.
     9    'RMSASPCT'.AND.WQ.NE.'PSFSUM'.AND.WQ.NE.'PSFFWHMX'.AND.WQ.NE.
     1    'PSFFWHMY') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       3,5 AND 7 ABERRATIONS
          IF(WQ.NE.'SA3'.AND.WQ.NE.'XSA3'.AND.WQ.NE.'CMA3'.AND.WQ.NE.
     1    'XCMA3'.AND.WQ.NE.'DIS3'.AND.WQ.NE.'XDIS3'.AND.WQ.NE.'AST3'.AND.
     2    WQ.NE.'XAST3'.AND.WQ.NE.'PTZ3'.AND.WQ.NE.'XPTZ3'.AND.WQ.NE.
     3    'SA5'.AND.WQ.NE.'XSA5'.AND.WQ.NE.'CMA5'.AND.WQ.NE.'XCMA5'.AND.
     4    WQ.NE.'AST5'.AND.WQ.NE.'XAST5'.AND.WQ.NE.'DIS5'
     1    .AND.WQ.NE.'PTZCV'.AND.WQ.NE.'XPTZCV'
     4    .AND.WQ.NE.'XDIS5'.AND.WQ.NE.'PTZ5'.AND.WQ.NE.'XPTZ5'.AND.
     5    WQ.NE.'TOBSA'.AND.WQ.NE.'XTOBSA'.AND.WQ.NE.'SOBSA'.AND.
     6    WQ.NE.'XSOBSA'.AND.WQ.NE.'ELCMA'.AND.WQ.NE.'XELCMA'.AND.WQ
     7    .NE.'TAS'.AND.WQ.NE.'XTAS'.AND.WQ.NE.'SAS'.AND.WQ.NE.'XTAS'
     8    .AND.WQ.NE.'SA7'.AND.WQ.NE.'XSA7') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       3RD PUPIL ABERRATIONS
          IF(WQ.NE.'PSA3'.AND.WQ.NE.'XPSA3'.AND.WQ.NE.'PCMA3'.AND.WQ.NE.
     1    'XPCMA3'.AND.WQ.NE.'PDIS3'.AND.WQ.NE.'XPDIS3'.AND.
     1    WQ.NE.'PAST3'.AND.WQ.NE.'OPD'.AND.WQ.NE.'OPDW'
     2    .AND.WQ.NE.'XPAST3'.AND.WQ.NE.'PPTZ3'.AND.WQ.NE.'XPPTZ3'
     4    .AND.WQ.NE.'VIGY'.AND.WQ.NE.'VIGX'.AND.WQ.NE.'SAY'.AND.WQ
     5    .NE.'SAX'.AND.WQ.NE.'SCX'.AND.WQ.NE.'SCY'.AND.WQ.NE.'WRX'
     6    .AND.WQ.NE.'WRY'.AND.WQ.NE.'BDX'.AND.WQ.NE.'BDY'
     7    .AND.WQ.NE.'VIGSX'.AND.WQ.NE.'VIGSY'.AND.WQ.NE.'AUTOFUNC'.AND.
     8    WQ.NE.'PXIM'.AND.WQ.NE.'PYIM'.AND.WQ.NE.'RXIM'.AND.WQ.NE.
     9    'RYIM') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       3RD PUPIL ABERRATIONS (PRIMARY CHROMATIC DIFFERENCES)
          IF(WQ.NE.'PSA3P'.AND.WQ.NE.'XPSA3P'.AND.WQ.NE.'PCMA3P'
     1    .AND.WQ.NE.'XPCMA3P'.AND.WQ.NE.'PDIS3P'.AND.WQ.NE.'XPDIS3P'.AND.
     2    WQ.NE.'PAST3P'.AND.WQ.NE.'RSPHX'.AND.WQ.NE.'RSPHY'.AND.
     3    WQ.NE.'XPAST3P'.AND.WQ.NE.'PPTZ3P'.AND.WQ.NE.'XPPTZ3P'
     4    .AND.WQ.NE.'RSPHZ') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       3RD PUPIL ABERRATIONS (SECONDARY CHROMATIC DIFFERENCES)
          IF(WQ.NE.'PSA3S'.AND.WQ.NE.'XPSA3S'.AND.WQ.NE.'PCMA3S'
     1    .AND.WQ.NE.'XPCMA3S'.AND.WQ.NE.'PDIS3S'.AND.WQ.NE.'XPDIS3S'.AND.
     1    WQ.NE.'PAST3S'.AND.
     2    WQ.NE.'GLASSWV1'.AND.
     2    WQ.NE.'GLASSWV2'.AND.WQ.NE.'GLASSWV3'.AND.WQ.NE.'GLASSWV4'
     2    .AND.WQ.NE.'GLASSWV5'.AND.
     2    WQ.NE.'XPAST3S'.AND.WQ.NE.'PPTZ3S'.AND.WQ.NE.'XPPTZ3S'
     2    .AND.WQ.NE.'PACM'.AND.WQ.NE.'PACZ'.AND.WQ.NE.'SACM'.AND.WQ.NE.
     2    'SACZ'.AND.WQ.NE.'PLCM'.AND.WQ.NE.'PLCZ'.AND.WQ.NE.'SLCM'.AND.
     2    WQ.NE.'SLCZ') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       INTRINSIC ABERRATIONS
          IF(WQ.NE.'SA5I'.AND.WQ.NE.'XSA5I'.AND.WQ.NE.'CMA5I'.AND.
     4    WQ.NE.'AST5I'.AND.WQ.NE.'XAST5I'.AND.WQ.NE.'DIS5I'
     4    .AND.WQ.NE.'XDIS5I'.AND.WQ.NE.'PTZ5I'.AND.WQ.NE.'XPTZ5I'.AND.
     5    WQ.NE.'TOBSAI'.AND.WQ.NE.'XTOBSAI'.AND.WQ.NE.'SOBSAI'.AND.
     6    WQ.NE.'XSOBSAI'.AND.WQ.NE.'ELCMAI'.AND.WQ.NE.'XELCMAI'.AND.WQ.
     7    NE.'TASI'.AND.WQ.NE.'XTASI'.AND.WQ.NE.'SASI'.AND.WQ.NE.'XTASI'
     8    .AND.WQ.NE.'SA7I'.AND.WQ.NE.'XSA7I'.AND.WQ.NE.'XCMA5I') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       3,5 AND 7 PRIMARTY CHROMATIC DIFFERENCES (YZ)
          IF(WQ.NE.'SA3P'.AND.WQ.NE.'CMA3P'.AND.WQ.NE.
     1    'DIS3P'.AND.WQ.NE.'AST3P'.AND.
     2    WQ.NE.'PTZ3P'.AND.WQ.NE.
     3    'SA5P'.AND.WQ.NE.'CMA5P'.AND.
     4    WQ.NE.'AST5P'.AND.WQ.NE.'DIS5P'
     4    .AND.WQ.NE.'PTZ5P'.AND.
     5    WQ.NE.'TOBSAP'.AND.WQ.NE.'SOBSAP'.AND.
     6    WQ.NE.'ELCMAP'.AND.WQ.
     7    NE.'TASP'.AND.WQ.NE.'SASP'.AND.
     8    WQ.NE.'SA7P') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
          IF(WQ.NE.'ENDIAX'.AND.WQ.NE.'ENDIAY'.AND.WQ.NE.
     1    'EXDIAX'.AND.WQ.NE.'EXDIAY') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       REMAINER OF THINGS
          IF(WQ.NE.'PWRY'.AND.WQ.NE.'PWRX'.AND.WQ.NE.'MAGXOR'.AND.
     1    WQ.NE.'LENGTH'.AND.WQ.NE.'MLENGTH'.AND.WQ.NE.'MAGYOR'.AND.
     2    WQ.NE.'FFLY'.AND.WQ.NE.'FFLX'.AND.WQ.NE.'PUPDISX'.AND.WQ.NE.
     3    'IMDISX'.AND.WQ.NE.'BFLX'.AND.WQ.NE.'BFLY'
     4    .AND.WQ.NE.'FLCLTH'.AND.WQ.NE.'PUPDIAX'.AND.WQ.NE.'MAGX'
     4    .AND.WQ.NE.'CHFIMX'.AND.WQ.NE.'DR'.AND.WQ.NE.'DRA'.AND.
     6    WQ.NE.'MAGY'.AND.WQ.NE.'EFLX'.AND.WQ.NE.'EFLY'.AND.WQ.NE.
     7    'FFNX'.AND.WQ.NE.'FFNY'.AND.WQ.NE.'BFNX'.AND.WQ.NE.'BFNY'
     8    .AND.WQ.NE.'ENPOSX'.AND.WQ.NE.'ENPOSY'.AND.WQ.NE.'ENPOSZ'.AND.
     9    WQ.NE.'EXPOSX'.AND.WQ.NE.'EXPOSY'.AND.WQ.NE.'EXPOSZ'.AND.WQ.NE.
     1    'PUPDISY'.AND.WQ.NE.'PUPDIAY'.AND.WQ.NE.'IMDISY'.AND.WQ.NE.
     2    'CHFIMY'.AND.WQ.NE.'OAL'.AND.WQ.NE.'OPTLEN'.AND.WQ.NE.'WEIGHT'
     3    ) THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       3,5 AND 7 SECONDARY CHROMATIC DIFFERENCES  (YZ)
          IF(WQ.NE.'SA3S'.AND.WQ.NE.'CMA3S'.AND.WQ.NE.
     1    'DIS3S'.AND.WQ.NE.'AST3S'.AND.WQ.NE.'FLCLTHY'.AND.
     2    WQ.NE.'PTZ3S'.AND.WQ.NE.'FLCLTHX'.AND.WQ.NE.
     3    'SA5S'.AND.WQ.NE.'CMA5S'.AND.
     4    WQ.NE.'AST5S'.AND.WQ.NE.'DIS5S'
     4    .AND.WQ.NE.'PTZ5S'.AND.
     5    WQ.NE.'TOBSAS'.AND.WQ.NE.'SOBSAS'.AND.
     6    WQ.NE.'ELCMAP'.AND.WQ.
     7    NE.'TASS'.AND.WQ.NE.'SASS'.AND.
     8    WQ.NE.'SA7S') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       3,5 AND 7 PRIMARTY CHROMATIC DIFFERENCES (XZ)
          IF(WQ.NE.'XSA3P'.AND.WQ.NE.'XCMA3P'.AND.WQ.NE.
     1    'XDIS3P'.AND.WQ.NE.'XAST3P'.AND.
     2    WQ.NE.'XPTZ3P'.AND.WQ.NE.
     3    'XSA5P'.AND.WQ.NE.'XCMA5P'.AND.
     4    WQ.NE.'XAST5P'.AND.WQ.NE.'XDIS5P'
     4    .AND.WQ.NE.'XPTZ5P'.AND.
     5    WQ.NE.'XTOBSAP'.AND.WQ.NE.'XSOBSAP'.AND.
     6    WQ.NE.'XELCMAP'.AND.WQ.
     7    NE.'XTASP'.AND.WQ.NE.'XSASP'.AND.
     8    WQ.NE.'XSA7P') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
          IF(WQ.NE.'DIST'.AND.WQ.NE.'XFOC'.AND.WQ.NE.'FISHDIST'
     9    .AND.WQ.NE.'YFOC'.AND.WQ.NE.'AST') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       CHROMATIC ABERRATIONS
          IF(WQ.NE.'PACY'.AND.WQ.NE.'PACX'.AND.WQ.NE.'SAGDEL'.AND.WQ.NE.
     1    'SACY'.AND.WQ.NE.'SACX'.AND.WQ.NE.'CAIMTOL'
     2    .AND.WQ.NE.'SURTOL'.AND.WQ.NE.'PLCY'.AND.WQ.NE.'MRAYS'.AND.WQ.NE.
     3    'PLCX'.AND.WQ.NE.'SLCY'.AND.WQ.NE.'AIMTOL'.AND.WQ.NE.'NRAITR'
     4    .AND.WQ.NE.'DELSUR'.AND.WQ.NE.'SLCX'.AND.WQ.NE.'FNUMX'.AND.
     5    WQ.NE.'FNUMY'.AND.WQ.NE.'OBFNUMX'.AND.WQ.NE.'OBFNUMY'.AND.
     6    WQ.NE.'EXPDIAX'.AND.WQ.NE.'EXPDIAY'.AND.WQ.NE.'ENPDIAX'
     7    .AND.WQ.NE.'ENPDIAY'.AND.WQ.NE.'SERINC'.AND.WQ.NE.'SERLIM') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       3,5 AND 7 SECONDARY CHROMATIC DIFFERENCES  (XZ)
          IF(WQ.NE.'XSA3S'.AND.WQ.NE.'XCMA3S'.AND.WQ.NE.
     1    'XDIS3S'.AND.WQ.NE.'XAST3S'.AND.
     2    WQ.NE.'XPTZ3S'.AND.WQ.NE.'ZERN37'.AND.WQ.NE.
     3    'XSA5S'.AND.WQ.NE.'XCMA5S'.AND.
     4    WQ.NE.'XAST5S'.AND.WQ.NE.'XDIS5S'
     4    .AND.WQ.NE.'XPTZ5S'.AND.
     5    WQ.NE.'XTOBSAS'.AND.WQ.NE.'XSOBSAS'.AND.
     6    WQ.NE.'XELCMAP'.AND.WQ.NE.'OPWT'.AND.WQ.NE.'VARWT'.AND.WQ.
     7    NE.'XTASS'.AND.WQ.NE.'XSASS'.AND.WQ.NE.'VARDNC'.AND.WQ.NE.
     8    'OPTYPE'.AND.
     8    WQ.NE.'XSA7S') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       COBS AND CLAP DATA CONT'D AND GAUSSIAN BEAM PROP STUFF
          IF(WQ.NE.'COBSE'.AND.WQ.NE.'COBSY'.AND.WQ.NE.'COBSYE'.AND.
     1    WQ.NE.'COBSX'.AND.WQ.NE.'COBSXE'.AND.WQ.NE.'CLAPE'.AND.
     1    WQ.NE.'CLAPY'.AND.WQ.NE.'CLAPYE'.AND.WQ.NE.'CLAPX'.AND.
     1    WQ.NE.'CLAPXE'.AND.WQ.NE.'GBRADX'.AND.WQ.NE.'GBRADY'.AND.
     1    WQ.NE.'GBDISX'.AND.WQ.NE.'GBDISY'.AND.WQ.NE.'GBRCVX'.AND.
     1    WQ.NE.'GBRCVY'.AND.WQ.NE.'GBWAISTX'.AND.WQ.NE.'GBWAISTY') THEN
C
C       CONTINUE CHECKING
          ELSE
C       FOUND A MATCH, GO TO 1
              GO TO 1
          END IF
C       SPECIAL SURFACE COEFFICIENTS C1 TO C96
          IF(WQ.EQ.'C1') GO TO 1
          IF(WQ.EQ.'C2') GO TO 1
          IF(WQ.EQ.'C3') GO TO 1
          IF(WQ.EQ.'C4') GO TO 1
          IF(WQ.EQ.'C5') GO TO 1
          IF(WQ.EQ.'C6') GO TO 1
          IF(WQ.EQ.'C7') GO TO 1
          IF(WQ.EQ.'C8') GO TO 1
          IF(WQ.EQ.'C9') GO TO 1
          IF(WQ.EQ.'C10') GO TO 1
          IF(WQ.EQ.'C11') GO TO 1
          IF(WQ.EQ.'C12') GO TO 1
          IF(WQ.EQ.'C13') GO TO 1
          IF(WQ.EQ.'C14') GO TO 1
          IF(WQ.EQ.'C15') GO TO 1
          IF(WQ.EQ.'C16') GO TO 1
          IF(WQ.EQ.'C17') GO TO 1
          IF(WQ.EQ.'C18') GO TO 1
          IF(WQ.EQ.'C19') GO TO 1
          IF(WQ.EQ.'C20') GO TO 1
          IF(WQ.EQ.'C21') GO TO 1
          IF(WQ.EQ.'C22') GO TO 1
          IF(WQ.EQ.'C23') GO TO 1
          IF(WQ.EQ.'C24') GO TO 1
          IF(WQ.EQ.'C25') GO TO 1
          IF(WQ.EQ.'C26') GO TO 1
          IF(WQ.EQ.'C27') GO TO 1
          IF(WQ.EQ.'C28') GO TO 1
          IF(WQ.EQ.'C29') GO TO 1
          IF(WQ.EQ.'C30') GO TO 1
          IF(WQ.EQ.'C31') GO TO 1
          IF(WQ.EQ.'C32') GO TO 1
          IF(WQ.EQ.'C33') GO TO 1
          IF(WQ.EQ.'C34') GO TO 1
          IF(WQ.EQ.'C35') GO TO 1
          IF(WQ.EQ.'C36') GO TO 1
          IF(WQ.EQ.'C37') GO TO 1
          IF(WQ.EQ.'C38') GO TO 1
          IF(WQ.EQ.'C39') GO TO 1
          IF(WQ.EQ.'C40') GO TO 1
          IF(WQ.EQ.'C41') GO TO 1
          IF(WQ.EQ.'C42') GO TO 1
          IF(WQ.EQ.'C43') GO TO 1
          IF(WQ.EQ.'C44') GO TO 1
          IF(WQ.EQ.'C45') GO TO 1
          IF(WQ.EQ.'C46') GO TO 1
          IF(WQ.EQ.'C47') GO TO 1
          IF(WQ.EQ.'C48') GO TO 1
          IF(WQ.EQ.'C49') GO TO 1
          IF(WQ.EQ.'C50') GO TO 1
          IF(WQ.EQ.'C51') GO TO 1
          IF(WQ.EQ.'C52') GO TO 1
          IF(WQ.EQ.'C53') GO TO 1
          IF(WQ.EQ.'C54') GO TO 1
          IF(WQ.EQ.'C55') GO TO 1
          IF(WQ.EQ.'C56') GO TO 1
          IF(WQ.EQ.'C57') GO TO 1
          IF(WQ.EQ.'C58') GO TO 1
          IF(WQ.EQ.'C59') GO TO 1
          IF(WQ.EQ.'C60') GO TO 1
          IF(WQ.EQ.'C61') GO TO 1
          IF(WQ.EQ.'C62') GO TO 1
          IF(WQ.EQ.'C63') GO TO 1
          IF(WQ.EQ.'C64') GO TO 1
          IF(WQ.EQ.'C65') GO TO 1
          IF(WQ.EQ.'C66') GO TO 1
          IF(WQ.EQ.'C67') GO TO 1
          IF(WQ.EQ.'C68') GO TO 1
          IF(WQ.EQ.'C69') GO TO 1
          IF(WQ.EQ.'C70') GO TO 1
          IF(WQ.EQ.'C71') GO TO 1
          IF(WQ.EQ.'C72') GO TO 1
          IF(WQ.EQ.'C73') GO TO 1
          IF(WQ.EQ.'C74') GO TO 1
          IF(WQ.EQ.'C75') GO TO 1
          IF(WQ.EQ.'C76') GO TO 1
          IF(WQ.EQ.'C77') GO TO 1
          IF(WQ.EQ.'C78') GO TO 1
          IF(WQ.EQ.'C79') GO TO 1
          IF(WQ.EQ.'C80') GO TO 1
          IF(WQ.EQ.'C81') GO TO 1
          IF(WQ.EQ.'C82') GO TO 1
          IF(WQ.EQ.'C83') GO TO 1
          IF(WQ.EQ.'C84') GO TO 1
          IF(WQ.EQ.'C85') GO TO 1
          IF(WQ.EQ.'C86') GO TO 1
          IF(WQ.EQ.'C87') GO TO 1
          IF(WQ.EQ.'C88') GO TO 1
          IF(WQ.EQ.'C89') GO TO 1
          IF(WQ.EQ.'C90') GO TO 1
          IF(WQ.EQ.'C91') GO TO 1
          IF(WQ.EQ.'C92') GO TO 1
          IF(WQ.EQ.'C93') GO TO 1
          IF(WQ.EQ.'C94') GO TO 1
          IF(WQ.EQ.'C95') GO TO 1
          IF(WQ.EQ.'C96') GO TO 1
          IF(WQ.NE.'OPRD'.AND.WQ.NE.'WT'.AND.WQ.NE.'SCALEX'.AND.WQ.NE.
     2    'SCALEY'.AND.WQ.NE.
     3    'CCTOR'.AND.WQ.NE.'ADTOR'.AND.WQ.NE.'AETOR'.AND.
     3    WQ.NE.'AFTOR'.AND.WQ.NE.'SHAPEFAC'.AND.WQ.NE.
     4    'AGTOR'.AND.WQ.NE.'CW'.AND.WQ.NE.'CLDECY'.AND.WQ.NE.'CLDECX'
     5    .AND.WQ.NE.'CODECX'.AND.WQ.NE.'CODECY'.AND.WQ.NE.'CLTILT'.AND.
     6    WQ.NE.'COTILT'.AND.WQ.NE.'CLRAD'.AND.WQ.NE.'XPEN'
     7    .AND.WQ.NE.'YPEN'.AND.WQ.NE.'SCALX'.AND.WQ.NE.'SCALY'.AND.WQ.NE.
     8    'CORAD'.AND.WQ.NE.'ET'.AND.WQ.NE.'ETY'.AND.WQ.NE.'ETX'.AND.
     9    WQ.NE.'XPENOL'.AND.WQ.NE.
     1    'YPENOL'.AND.WQ.NE.'PENSTA') THEN
C
C       NO MATCH WAS FOUND.
              WRITE(OUTLYNE,*)'PARAMETER ',WQ,' IS NOT "GET"-ABLE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
 1        CONTINUE
C
C       FIRST, GET COMMANDS WITH NO NUMBERIC INPUT USED
          ONESY=.FALSE.
          IF(WQ.EQ.'RTOT')   ONESY=.TRUE.
          IF(WQ.EQ.'ITOT')   ONESY=.TRUE.
          IF(WQ.EQ.'CFG')    ONESY=.TRUE.
          IF(WQ.EQ.'UNITS')  ONESY=.TRUE.
          IF(WQ.EQ.'REFS')   ONESY=.TRUE.
          IF(WQ.EQ.'EFLX')   ONESY=.TRUE.
          IF(WQ.EQ.'ISN')    ONESY=.TRUE.
          IF(WQ.EQ.'ASTOP')  ONESY=.TRUE.
          IF(WQ.EQ.'CENTX')  ONESY=.TRUE.
          IF(WQ.EQ.'CENTY')  ONESY=.TRUE.
          IF(WQ.EQ.'MODE')   ONESY=.TRUE.
          IF(WQ.EQ.'MAGX')   ONESY=.TRUE.
          IF(WQ.EQ.'MAGY')   ONESY=.TRUE.
          IF(WQ.EQ.'NVAR')   ONESY=.TRUE.
          IF(WQ.EQ.'KDWA')   ONESY=.TRUE.
          IF(WQ.EQ.'FOCRIT') ONESY=.TRUE.
          IF(WQ.EQ.'FFLX')   ONESY=.TRUE.
          IF(WQ.EQ.'LCV')    ONESY=.TRUE.
          IF(WQ.EQ.'PFAC')   ONESY=.TRUE.
          IF(WQ.EQ.'ONTOL')  ONESY=.TRUE.
          IF(WQ.EQ.'SINGTOL')ONESY=.TRUE.
          IF(WQ.EQ.'DINMUL') ONESY=.TRUE.
          IF(WQ.EQ.'LINTOL') ONESY=.TRUE.
          IF(WQ.EQ.'FFLY')   ONESY=.TRUE.
          IF(WQ.EQ.'CW')     ONESY=.TRUE.
          IF(WQ.EQ.'XPEN')   ONESY=.TRUE.
          IF(WQ.EQ.'YPEN')   ONESY=.TRUE.
          IF(WQ.EQ.'SCALX')  ONESY=.TRUE.
          IF(WQ.EQ.'SCALY')  ONESY=.TRUE.
          IF(WQ.EQ.'MAGXOR') ONESY=.TRUE.
          IF(WQ.EQ.'DIFTOL') ONESY=.TRUE.
          IF(WQ.EQ.'XPENOL') ONESY=.TRUE.
          IF(WQ.EQ.'YPENOL') ONESY=.TRUE.
          IF(WQ.EQ.'PENSTA') ONESY=.TRUE.
          IF(WQ.EQ.'OSN')    ONESY=.TRUE.
          IF(WQ.EQ.'SURTOL') ONESY=.TRUE.
          IF(WQ.EQ.'SAGDEL') ONESY=.TRUE.
          IF(WQ.EQ.'AIMTOL') ONESY=.TRUE.
          IF(WQ.EQ.'CAIMTOL')ONESY=.TRUE.
          IF(WQ.EQ.'MRAYS')  ONESY=.TRUE.
          IF(WQ.EQ.'SERINC') ONESY=.TRUE.
          IF(WQ.EQ.'SERLIM') ONESY=.TRUE.
          IF(WQ.EQ.'NRAITR') ONESY=.TRUE.
          IF(WQ.EQ.'DELSUR') ONESY=.TRUE.
          IF(WQ.EQ.'OPD')    ONESY=.TRUE.
          IF(WQ.EQ.'VIGY')   ONESY=.TRUE.
          IF(WQ.EQ.'OPDW')   ONESY=.TRUE.
          IF(WQ.EQ.'MAGYOR') ONESY=.TRUE.
          IF(WQ.EQ.'BFLX')   ONESY=.TRUE.
          IF(WQ.EQ.'BFLY')   ONESY=.TRUE.
          IF(WQ.EQ.'BFNX')   ONESY=.TRUE.
          IF(WQ.EQ.'BFNY')   ONESY=.TRUE.
          IF(WQ.EQ.'FFNX')   ONESY=.TRUE.
          IF(WQ.EQ.'FFNY')   ONESY=.TRUE.
          IF(WQ.EQ.'GLASSWV1')  ONESY=.TRUE.
          IF(WQ.EQ.'GLASSWV2')  ONESY=.TRUE.
          IF(WQ.EQ.'GLASSWV3')  ONESY=.TRUE.
          IF(WQ.EQ.'GLASSWV4')  ONESY=.TRUE.
          IF(WQ.EQ.'GLASSWV5')  ONESY=.TRUE.
          IF(WQ.EQ.'EFLY')      ONESY=.TRUE.
          IF(WQ.EQ.'EXPOSX')    ONESY=.TRUE.
          IF(WQ.EQ.'EXPOSY')    ONESY=.TRUE.
          IF(WQ.EQ.'EXPOSZ')    ONESY=.TRUE.
          IF(WQ.EQ.'ENPOSX')    ONESY=.TRUE.
          IF(WQ.EQ.'ENPOSY')    ONESY=.TRUE.
          IF(WQ.EQ.'ENPOSZ')    ONESY=.TRUE.
          IF(WQ.EQ.'FNUMX')     ONESY=.TRUE.
          IF(WQ.EQ.'FNUMY')     ONESY=.TRUE.
          IF(WQ.EQ.'OBFNUMX')   ONESY=.TRUE.
          IF(WQ.EQ.'OBFNUMY')   ONESY=.TRUE.
          IF(WQ.EQ.'EXPDIAX')   ONESY=.TRUE.
          IF(WQ.EQ.'EXPDIAY')   ONESY=.TRUE.
          IF(WQ.EQ.'ENPDIAX')   ONESY=.TRUE.
          IF(WQ.EQ.'ENPDIAY')   ONESY=.TRUE.
          IF(WQ.EQ.'SHRTWAVE')  ONESY=.TRUE.
          IF(WQ.EQ.'ENDIAX')    ONESY=.TRUE.
          IF(WQ.EQ.'ENDIAY')    ONESY=.TRUE.
          IF(WQ.EQ.'RMSASPCT')  ONESY=.TRUE.
          IF(WQ.EQ.'RMS')       ONESY=.TRUE.
          IF(WQ.EQ.'RMSX')      ONESY=.TRUE.
          IF(WQ.EQ.'RMSY')      ONESY=.TRUE.
          IF(WQ.EQ.'EXDIAX')    ONESY=.TRUE.
          IF(WQ.EQ.'EXDIAY')    ONESY=.TRUE.
          IF(WQ.EQ.'RSS')       ONESY=.TRUE.
          IF(WQ.EQ.'RSSX')      ONESY=.TRUE.
          IF(WQ.EQ.'RSSY')      ONESY=.TRUE.
          IF(WQ.EQ.'FCSFT')     ONESY=.TRUE.
          IF(WQ.EQ.'VIGX')      ONESY=.TRUE.
          IF(WQ.EQ.'VIGSX')     ONESY=.TRUE.
          IF(WQ.EQ.'VIGSY')     ONESY=.TRUE.
          IF(WQ.EQ.'FCSFTX')    ONESY=.TRUE.
          IF(WQ.EQ.'FCSFTY')    ONESY=.TRUE.
          IF(WQ.EQ.'SAY')       ONESY=.TRUE.
          IF(WQ.EQ.'AUTOFUNC')  ONESY=.TRUE.
          IF(WQ.EQ.'SAX')       ONESY=.TRUE.
          IF(WQ.EQ.'SCY')       ONESY=.TRUE.
          IF(WQ.EQ.'SCX')       ONESY=.TRUE.
          IF(WQ.EQ.'PXIM')      ONESY=.TRUE.
          IF(WQ.EQ.'PYIM')      ONESY=.TRUE.
          IF(WQ.EQ.'RXIM')      ONESY=.TRUE.
          IF(WQ.EQ.'RYIM')      ONESY=.TRUE.
          IF(WQ.EQ.'WRX')       ONESY=.TRUE.
          IF(WQ.EQ.'WRY')       ONESY=.TRUE.
          IF(WQ.EQ.'BDX')       ONESY=.TRUE.
          IF(WQ.EQ.'BDY')       ONESY=.TRUE.
          IF(WQ.EQ.'PACM')      ONESY=.TRUE.
          IF(WQ.EQ.'PACZ')      ONESY=.TRUE.
          IF(WQ.EQ.'SACM')      ONESY=.TRUE.
          IF(WQ.EQ.'SACZ')      ONESY=.TRUE.
          IF(WQ.EQ.'PLCM')      ONESY=.TRUE.
          IF(WQ.EQ.'PLCZ')      ONESY=.TRUE.
          IF(WQ.EQ.'SLCM')      ONESY=.TRUE.
          IF(WQ.EQ.'SLCZ')      ONESY=.TRUE.
          IF(WQ.EQ.'RSPHX')     ONESY=.TRUE.
          IF(WQ.EQ.'RSPHY')     ONESY=.TRUE.
          IF(WQ.EQ.'RSPHZ')     ONESY=.TRUE.
          IF(WQ.EQ.'RAYCOD1')   ONESY=.TRUE.
          IF(WQ.EQ.'RAYCOD2')   ONESY=.TRUE.
          IF(WQ.EQ.'PSFFWHMX')  ONESY=.TRUE.
          IF(WQ.EQ.'PSFFWHMY')  ONESY=.TRUE.
          IF(WQ.EQ.'PSFSUM')    ONESY=.TRUE.
          IF(WQ.EQ.'NSSMEANX')    ONESY=.TRUE.
          IF(WQ.EQ.'NSSMEANY')    ONESY=.TRUE.
          IF(WQ.EQ.'NSSMEANR')    ONESY=.TRUE.
          IF(WQ.EQ.'NSSRMSX')    ONESY=.TRUE.
          IF(WQ.EQ.'NSSRMSY')    ONESY=.TRUE.
          IF(WQ.EQ.'NSSRMSR')    ONESY=.TRUE.
          IF(ONESY) THEN
              IF(WQ.EQ.'NSSMEANX') THEN
                  IF(NSSSPOTEXIST) THEN
                      VALUE1=NSSMEANX
                      GO TO 200
                  ELSE
                      WRITE(OUTLYNE,*) 'NO NSS SPOT EXISTS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WQ.EQ.'NSSMEANY') THEN
                  IF(NSSSPOTEXIST) THEN
                      VALUE1=NSSMEANY
                      GO TO 200
                  ELSE
                      WRITE(OUTLYNE,*) 'NO NSS SPOT EXISTS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WQ.EQ.'NSSMEANR') THEN
                  IF(NSSSPOTEXIST) THEN
                      VALUE1=NSSMEANR
                      GO TO 200
                  ELSE
                      WRITE(OUTLYNE,*) 'NO NSS SPOT EXISTS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WQ.EQ.'NSSRMSX') THEN
                  IF(NSSSPOTEXIST) THEN
                      VALUE1=NSSRMSX
                      GO TO 200
                  ELSE
                      WRITE(OUTLYNE,*) 'NO NSS SPOT EXISTS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WQ.EQ.'NSSRMSY') THEN
                  IF(NSSSPOTEXIST) THEN
                      VALUE1=NSSRMSY
                      GO TO 200
                  ELSE
                      WRITE(OUTLYNE,*) 'NO NSS SPOT EXISTS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(WQ.EQ.'NSSRMSR') THEN
                  IF(NSSSPOTEXIST) THEN
                      VALUE1=NSSRMSR
                      GO TO 200
                  ELSE
                      WRITE(OUTLYNE,*) 'NO NSS SPOT EXISTS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
              IF(WQ.EQ.'RSPHX'.OR.WQ.EQ.'RSPHY'.OR.WQ.EQ.'RSPHZ') THEN
                  IF(.NOT.CPFNEXT) THEN
                      OUTLYNE=
     1                'NO CAPFN EXISTS, "'//WQ(1:5)//'" IS NOT CURRENTLY GETABLE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WQ.EQ.'RSPHX') VALUE1=DLLX
                  IF(WQ.EQ.'RSPHY') VALUE1=DLLY
                  IF(WQ.EQ.'RSPHZ') VALUE1=DLLZ
                  GO TO 200
              END IF
              IF(WQ.EQ.'VIGX'.OR.WQ.EQ.'VIGY'.OR.WQ.EQ.'VIGSX'.OR.
     1        WQ.EQ.'VIGSY') THEN
                  IF(S1.EQ.0) THEN
                      N=20
                      SN=0
                      DF1=1
                  END IF
                  IF(S1.EQ.1) THEN
                      N=INT(W1)
                      SN=0
                      DF1=1
                  END IF
                  S1=0
              END IF
              IF(S1.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"GET ',WQ,'" ONLY USES NUMERIC WORD #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(WQ.EQ.'VIGX'.OR.WQ.EQ.'VIGY'.OR.WQ.EQ.'VIGSX'.OR.
     1        WQ.EQ.'VIGSY') THEN
C
                  IF(.NOT.REFEXT) THEN
C     NO REF RAY MESSAGE HERE
                      WRITE(OUTLYNE,*)'NO REFERENCE RAY DATA EXISTS.'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                WQ,' IS NOT GET-ABLE'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(WQ.EQ.'VIGX') CALL VIGCAL(N,VLO,VHI,1)
                  IF(WQ.EQ.'VIGY') CALL VIGCAL(N,VLO,VHI,2)
                  IF(WQ.EQ.'VIGSX') CALL VIGCAL(N,VLO,VHI,1)
                  IF(WQ.EQ.'VIGSY') CALL VIGCAL(N,VLO,VHI,2)
                  MSG=.TRUE.
                  IF(WQ.EQ.'VIGX'.OR.WQ.EQ.'VIGY') VALUE1=VHI
                  IF(WQ.EQ.'VIGSX'.OR.WQ.EQ.'VIGSY') VALUE1=1.0D0-VHI
                  IF(W5.GE.1.0.AND.W5.LE.400.0D0) THEN
                      NUM5=INT(W5)
                      GPREG(NUM5)=VALUE1
                  END IF
C
                  IF(WQ.EQ.'VIGX'.OR.WQ.EQ.'VIGY') THEN
                      REG(9)= VHI
                      REG(13)=VLO
                  END IF
                  IF(WQ.EQ.'VIGSX'.OR.WQ.EQ.'VIGSY') THEN
                      REG(9)= 1.0D0-VHI
                      REG(13)=1.0D0+VLO
                  END IF
                  IF(SHOW) THEN
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='WRITE X MAX-'//REMWQ
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      SAVE_KDP(1)=SAVEINPT(1)
                      INPUT='WRITE IX MIN-'//REMWQ
                      CALL PROCES
                      REST_KDP(1)=RESTINPT(1)
                      RETURN
                  END IF
              END IF
C
              IF(WQ.EQ.'FNUMX') THEN
                  ERRR=.FALSE.
                  MSG=.TRUE.
                  CACOCH=0
                  CALL FNUMX(VALVAL,ERRR)
                  MSG=.FALSE.
                  IF(ERRR) VALUE1=RBFNX
                  IF(.NOT.ERRR) VALUE1=VALVAL
                  GO TO 200
              END IF
              IF(WQ.EQ.'FNUMY') THEN
                  ERRR=.FALSE.
                  MSG=.TRUE.
                  CACOCH=0
                  CALL FNUMY(VALVAL,ERRR)
                  MSG=.FALSE.
                  IF(ERRR)VALUE1=RBFNY
                  IF(.NOT.ERRR) VALUE1=VALVAL
                  GO TO 200
              END IF
              IF(WQ.EQ.'OBFNUMX') THEN
                  ERRR=.FALSE.
                  MSG=.TRUE.
                  CACOCH=0
                  CALL OBFNUMX(VALVAL,ERRR)
                  MSG=.FALSE.
                  IF(ERRR) VALUE1=RFFNX
                  IF(.NOT.ERRR) VALUE1=VALVAL
                  GO TO 200
              END IF
              IF(WQ.EQ.'OBFNUMY') THEN
                  ERRR=.FALSE.
                  MSG=.TRUE.
                  CACOCH=0
                  CALL OBFNUMY(VALVAL,ERRR)
                  MSG=.FALSE.
                  IF(ERRR) VALUE1=RFFNY
                  IF(.NOT.ERRR) VALUE1=VALVAL
                  VALUE1=VALVAL
                  GO TO 200
              END IF
              IF(WQ.EQ.'EXPDIAX') THEN
                  ERRR=.FALSE.
                  MSG=.TRUE.
                  CACOCH=0
                  CALL EXPDIAX(VALVAL,ERRR)
                  MSG=.FALSE.
                  IF(.NOT.ERRR) VALUE1=VALVAL
                  IF(ERRR) VALUE1=EXDIAX
                  GO TO 200
              END IF
              IF(WQ.EQ.'EXPDIAY') THEN
                  ERRR=.FALSE.
                  MSG=.TRUE.
                  CACOCH=0
                  CALL EXPDIAY(VALVAL,ERRR)
                  MSG=.FALSE.
                  IF(.NOT.ERRR) VALUE1=VALVAL
                  IF(ERRR) VALUE1=EXDIAY
                  GO TO 200
              END IF
              IF(WQ.EQ.'ENPDIAX') THEN
                  ERRR=.FALSE.
                  MSG=.TRUE.
                  CACOCH=0
                  CALL ENPDIAX(VALVAL,ERRR)
                  MSG=.FALSE.
                  IF(.NOT.ERRR) VALUE1=VALVAL
                  IF(ERRR) VALUE1=ENDIAX
                  GO TO 200
              END IF
              IF(WQ.EQ.'ENPDIAY') THEN
                  ERRR=.FALSE.
                  MSG=.TRUE.
                  CACOCH=0
                  CALL ENPDIAY(VALVAL,ERRR)
                  MSG=.FALSE.
                  IF(.NOT.ERRR) VALUE1=VALVAL
                  IF(ERRR) VALUE1=ENDIAY
                  GO TO 200
              END IF
              IF(WQ.EQ.'MAGY') THEN
                  VALUE1=RMAGY
                  GO TO 200
              END IF
              IF(WQ.EQ.'MAGX') THEN
                  VALUE1=RMAGX
                  GO TO 200
              END IF
              IF(WQ.EQ.'MAGYOR') THEN
                  VALUE1=MAGYOR
                  GO TO 200
              END IF
              IF(WQ.EQ.'MAGXOR') THEN
                  VALUE1=MAGXOR
                  GO TO 200
              END IF
              IF(WQ.EQ.'EFLX') THEN
                  VALUE1=REFLX
                  GO TO 200
              END IF
              IF(WQ.EQ.'EFLY') THEN
                  VALUE1=REFLY
                  GO TO 200
              END IF
              IF(WQ.EQ.'FFLX') THEN
                  VALUE1=RFFLX
                  GO TO 200
              END IF
              IF(WQ.EQ.'FFLY') THEN
                  VALUE1=RFFLY
                  GO TO 200
              END IF
              IF(WQ.EQ.'BFLX') THEN
                  VALUE1=RBFLX
                  GO TO 200
              END IF
              IF(WQ.EQ.'BFLY') THEN
                  VALUE1=RBFLY
                  GO TO 200
              END IF
              IF(WQ.EQ.'FFNX') THEN
                  VALUE1=RFFNX
                  GO TO 200
              END IF
              IF(WQ.EQ.'FFNY') THEN
                  VALUE1=RFFNY
                  GO TO 200
              END IF
              IF(WQ.EQ.'BFNX') THEN
                  VALUE1=RBFNX
                  GO TO 200
              END IF
              IF(WQ.EQ.'BFNY') THEN
                  VALUE1=RBFNY
                  GO TO 200
              END IF
              IF(WQ.EQ.'EXPOSX') THEN
                  VALUE1=EXPUX
                  GO TO 200
              END IF
              IF(WQ.EQ.'EXPOSY') THEN
                  VALUE1=EXPUY
                  GO TO 200
              END IF
              IF(WQ.EQ.'EXPOSZ') THEN
                  VALUE1=EXPUZ
                  GO TO 200
              END IF
              IF(WQ.EQ.'ENPOSX') THEN
                  VALUE1=ENPUX
                  GO TO 200
              END IF
              IF(WQ.EQ.'ENPOSY') THEN
                  VALUE1=ENPUY
                  GO TO 200
              END IF
              IF(WQ.EQ.'ENPOSZ') THEN
                  VALUE1=ENPUZ
                  GO TO 200
              END IF
              IF(WQ.EQ.'ENDIAX') THEN
                  VALUE1=ENDIAX
                  GO TO 200
              END IF
              IF(WQ.EQ.'ENDIAY') THEN
                  VALUE1=ENDIAY
                  GO TO 200
              END IF
              IF(WQ.EQ.'EXDIAX') THEN
                  VALUE1=EXDIAX
                  GO TO 200
              END IF
              IF(WQ.EQ.'EXDIAY') THEN
                  VALUE1=EXDIAY
                  GO TO 200
              END IF
              IF(WQ.EQ.'RAYCOD1') THEN
                  VALUE1=RAYCOD(1)
                  GO TO 200
              END IF
              IF(WQ.EQ.'RAYCOD2') THEN
                  VALUE1=RAYCOD(2)
                  GO TO 200
              END IF
              IF(WQ.EQ.'GLASSWV1') THEN
                  VALUE1=GLSWV(1)
                  GO TO 200
              END IF
              IF(WQ.EQ.'GLASSWV2') THEN
                  VALUE1=GLSWV(2)
                  GO TO 200
              END IF
              IF(WQ.EQ.'GLASSWV3') THEN
                  VALUE1=GLSWV(3)
                  GO TO 200
              END IF
              IF(WQ.EQ.'GLASSWV4') THEN
                  VALUE1=GLSWV(4)
                  GO TO 200
              END IF
              IF(WQ.EQ.'GLASSWV5') THEN
                  VALUE1=GLSWV(5)
                  GO TO 200
              END IF
              IF(WQ.EQ.'CFG') THEN
                  VALUE1=DBLE(F12)
                  GO TO 200
              END IF
              IF(WQ.EQ.'RTOT') THEN
                  VALUE1=RTOT
                  GO TO 200
              END IF
              IF(WQ.EQ.'ITOT') THEN
                  VALUE1=INTTOT
                  GO TO 200
              END IF
              IF(WQ.EQ.'SHRTWAVE') THEN
                  VALUE1=0.0D0
                  IF(SYSTEM1(31).NE.0.0D0.AND.SYSTEM1(1).NE.0.0D0) THEN
                      VALUE1=SYSTEM1(1)
                      GO TO 314
                  END IF
                  IF(SYSTEM1(32).NE.0.0D0.AND.SYSTEM1(2).NE.0.0D0) THEN
                      VALUE1=SYSTEM1(2)
                      GO TO 314
                  END IF
                  IF(SYSTEM1(33).NE.0.0D0.AND.SYSTEM1(3).NE.0.0D0) THEN
                      VALUE1=SYSTEM1(3)
                      GO TO 314
                  END IF
                  IF(SYSTEM1(34).NE.0.0D0.AND.SYSTEM1(4).NE.0.0D0) THEN
                      VALUE1=SYSTEM1(4)
                      GO TO 314
                  END IF
                  IF(SYSTEM1(35).NE.0.0D0.AND.SYSTEM1(5).NE.0.0D0) THEN
                      VALUE1=SYSTEM1(5)
                      GO TO 314
                  END IF
                  IF(SYSTEM1(76).NE.0.0D0.AND.SYSTEM1(71).NE.0.0D0) THEN
                      VALUE1=SYSTEM1(71)
                      GO TO 314
                  END IF
                  IF(SYSTEM1(77).NE.0.0D0.AND.SYSTEM1(72).NE.0.0D0) THEN
                      VALUE1=SYSTEM1(72)
                      GO TO 314
                  END IF
                  IF(SYSTEM1(78).NE.0.0D0.AND.SYSTEM1(73).NE.0.0D0) THEN
                      VALUE1=SYSTEM1(73)
                      GO TO 314
                  END IF
                  IF(SYSTEM1(79).NE.0.0D0.AND.SYSTEM1(74).NE.0.0D0) THEN
                      VALUE1=SYSTEM1(74)
                      GO TO 314
                  END IF
                  IF(SYSTEM1(80).NE.0.0D0.AND.SYSTEM1(75).NE.0.0D0) THEN
                      VALUE1=SYSTEM1(75)
                      GO TO 314
                  END IF
 314              CONTINUE
                  IF(VALUE1.EQ.0.0D0) GO TO 200
C
                  IF(SYSTEM1(31).NE.0.0D0.AND.SYSTEM1(1).LT.VALUE1
     1            .AND.SYSTEM1(1).NE.0.0D0) VALUE1=SYSTEM1(1)
                  IF(SYSTEM1(32).NE.0.0D0.AND.SYSTEM1(2).LT.VALUE1
     1            .AND.SYSTEM1(2).NE.0.0D0) VALUE1=SYSTEM1(2)
                  IF(SYSTEM1(33).NE.0.0D0.AND.SYSTEM1(3).LT.VALUE1
     1            .AND.SYSTEM1(3).NE.0.0D0) VALUE1=SYSTEM1(3)
                  IF(SYSTEM1(34).NE.0.0D0.AND.SYSTEM1(4).LT.VALUE1
     1            .AND.SYSTEM1(4).NE.0.0D0) VALUE1=SYSTEM1(4)
                  IF(SYSTEM1(35).NE.0.0D0.AND.SYSTEM1(5).LT.VALUE1
     1            .AND.SYSTEM1(5).NE.0.0D0) VALUE1=SYSTEM1(5)
                  IF(SYSTEM1(76).NE.0.0D0.AND.SYSTEM1(71).LT.VALUE1
     1            .AND.SYSTEM1(71).NE.0.0D0) VALUE1=SYSTEM1(71)
                  IF(SYSTEM1(77).NE.0.0D0.AND.SYSTEM1(72).LT.VALUE1
     1            .AND.SYSTEM1(72).NE.0.0D0) VALUE1=SYSTEM1(72)
                  IF(SYSTEM1(78).NE.0.0D0.AND.SYSTEM1(73).LT.VALUE1
     1            .AND.SYSTEM1(73).NE.0.0D0) VALUE1=SYSTEM1(73)
                  IF(SYSTEM1(79).NE.0.0D0.AND.SYSTEM1(74).LT.VALUE1
     1            .AND.SYSTEM1(74).NE.0.0D0) VALUE1=SYSTEM1(74)
                  IF(SYSTEM1(80).NE.0.0D0.AND.SYSTEM1(75).LT.VALUE1
     1            .AND.SYSTEM1(75).NE.0.0D0) VALUE1=SYSTEM1(75)
                  IF(SYSTEM1(6).EQ.1.0D0) VALUE1=((VALUE1*1.0D-3)/25.4D0)
                  IF(SYSTEM1(6).EQ.2.0D0) VALUE1=(VALUE1*1.0D-4)
                  IF(SYSTEM1(6).EQ.3.0D0) VALUE1=(VALUE1*1.0D-3)
                  IF(SYSTEM1(6).EQ.4.0D0) VALUE1=(VALUE1*1.0D-6)
                  GO TO 200
              END IF
              IF(WQ.EQ.'PACM'.OR.WQ.EQ.'PACZ'.OR.WQ.EQ.'SACM'.OR.WQ.EQ.'SACZ'
     1        .OR.WQ.EQ.'PLCM'.OR.WQ.EQ.'PLCZ'.OR.WQ.EQ.'SLCM'.OR.WQ.EQ.'SLCZ')
     2         THEN
                  VALUE1=0.0D0
                  ERRR=.FALSE.
                  IF(WQ.EQ.'PACM') CACOCH=0
                  IF(WQ.EQ.'PACZ') CACOCH=0
                  IF(WQ.EQ.'SACM') CACOCH=0
                  IF(WQ.EQ.'SACZ') CACOCH=0
                  IF(WQ.EQ.'PLCM') CACOCH=0
                  IF(WQ.EQ.'PLCZ') CACOCH=0
                  IF(WQ.EQ.'SLCM') CACOCH=0
                  IF(WQ.EQ.'SLCZ') CACOCH=0
                  IF(WQ.EQ.'PACM') CALL REALCOLR(1,ERRR)
                  IF(WQ.EQ.'PACZ') CALL REALCOLR(2,ERRR)
                  IF(WQ.EQ.'SACM') CALL REALCOLR(3,ERRR)
                  IF(WQ.EQ.'SACZ') CALL REALCOLR(4,ERRR)
                  IF(WQ.EQ.'PLCM') CALL REALCOLR(5,ERRR)
                  IF(WQ.EQ.'PLCZ') CALL REALCOLR(6,ERRR)
                  IF(WQ.EQ.'SLCM') CALL REALCOLR(7,ERRR)
                  IF(WQ.EQ.'SLCZ') CALL REALCOLR(8,ERRR)
                  IF(ERRR) THEN
                      WRITE(OUTLYNE,*)
     1                'SOME RAYS COULD NOT BE TRACED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                '"',WQ(1:4),'" IS NOT GETABLE'
                      CALL SHOWIT(1)
                      ERRR=.FALSE.
                      CALL MACFAL
                      RETURN
                  END IF
                  GO TO 200
              END IF
              IF(WQ.EQ.'AUTOFUNC') THEN
                  VALUE1=SYSTEM1(91)
                  GO TO 200
              END IF
              IF(WQ.EQ.'SAY') THEN
                  VALUE1=SYSTEM1(12)
                  GO TO 200
              END IF
              IF(WQ.EQ.'SAX') THEN
                  VALUE1=SYSTEM1(13)
                  GO TO 200
              END IF
              IF(WQ.EQ.'SCY') THEN
                  VALUE1=SYSTEM1(14)
                  GO TO 200
              END IF
              IF(WQ.EQ.'SCX') THEN
                  VALUE1=SYSTEM1(16)
                  GO TO 200
              END IF
              IF(WQ.EQ.'PXIM') THEN
                  VALUE1=SYSTEM1(92)
                  GO TO 200
              END IF
              IF(WQ.EQ.'PYIM') THEN
                  VALUE1=SYSTEM1(93)
                  GO TO 200
              END IF
              IF(WQ.EQ.'RXIM') THEN
                  VALUE1=SYSTEM1(96)
                  GO TO 200
              END IF
              IF(WQ.EQ.'RYIM') THEN
                  VALUE1=SYSTEM1(97)
                  GO TO 200
              END IF
              IF(WQ.EQ.'WRX') THEN
                  VALUE1=SYSTEM1(85)
                  GO TO 200
              END IF
              IF(WQ.EQ.'WRY') THEN
                  VALUE1=SYSTEM1(86)
                  GO TO 200
              END IF
              IF(WQ.EQ.'BDX') THEN
                  VALUE1=SYSTEM1(87)
                  GO TO 200
              END IF
              IF(WQ.EQ.'BDY') THEN
                  VALUE1=SYSTEM1(88)
                  GO TO 200
              END IF
              IF(WQ.EQ.'UNITS') THEN
                  VALUE1=SYSTEM1(6)
                  GO TO 200
              END IF
              IF(WQ.EQ.'OSN') THEN
                  VALUE1=DBLE(NEWOBJ)
                  GO TO 200
              END IF
              IF(WQ.EQ.'ISN') THEN
                  VALUE1=DBLE(NEWIMG)
                  GO TO 200
              END IF
              IF(WQ.EQ.'REFS') THEN
                  VALUE1=DBLE(NEWREF)
                  GO TO 200
              END IF
              IF(WQ.EQ.'ASTOP') THEN
                  VALUE1=SYSTEM1(26)
                  GO TO 200
              END IF
              IF(WQ.EQ.'CW') THEN
                  VALUE1=SYSTEM1(11)
                  GO TO 200
              END IF
              IF(WQ.EQ.'CENTX') THEN
                  IF(.NOT.SPDEXT) THEN
                      WRITE(OUTLYNE,*)
     1                'NO SPOT DIAGRAM EXISTS, "CENTX" IS NOT CURRENTLY GETABLE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  VALUE1=CENTX
                  GO TO 200
              END IF
              IF(WQ.EQ.'CENTY') THEN
                  IF(.NOT.SPDEXT) THEN
                      WRITE(OUTLYNE,*)
     1                'NO SPOT DIAGRAM EXISTS, "CENTY" IS NOT CURRENTLY GETABLE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  VALUE1=CENTY
                  GO TO 200
              END IF
              IF(WQ.EQ.'RMS'.OR.WQ.EQ.'RMSX'.OR.WQ.EQ.'RMSY'.OR.WQ.EQ.
     1        'RSS'.OR.WQ.EQ.'RSSX'.OR.WQ.EQ.'RSSY'.OR.WQ.EQ.'FCSFT'.OR.
     2        WQ.EQ.'FCSFTX'.OR.WQ.EQ.'FCSFTY'.OR.WQ.EQ.'RMSASPCT') THEN
                  IF(WQ.EQ.'FCSFT'.OR.WQ.EQ.'FCSFTX'.OR.WQ.EQ.'FCSFTY') THEN
                      IF(.NOT.SPDEXT) THEN
                          WRITE(OUTLYNE,*)
     1                    'NO SPOT DIAGRAM EXISTS, ITEM IS NOT CURRENTLY GETABLE'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
C
                      IF(.NOT.STATSP) THEN
                          WRITE(OUTLYNE,*)
     1                    'NO FULL STATS EXIST, "',WQ(1:6),'" IS NOT CURRENTLY GETABLE'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                      IF(SYSTEM1(30).GE.3.0D0) THEN
                          WRITE(OUTLYNE,*)
     1                    WQ(1:6),'" IS NOT DEFINED IN MODES AFOCAL AND UAFOCAL'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          RETURN
                      END IF
                  END IF
                  IF(WQ.EQ.'RMS') VALUE1=RMS
                  IF(WQ.EQ.'RSS') VALUE1=RSS
                  IF(WQ.EQ.'RMSX') VALUE1=RMSX
                  IF(WQ.EQ.'RSSX') VALUE1=RSSX
                  IF(WQ.EQ.'RMSY') VALUE1=RMSY
                  IF(WQ.EQ.'RSSY') VALUE1=RSSY
                  IF(WQ.EQ.'FCSFT') VALUE1=FCSFT
                  IF(WQ.EQ.'FCSFTX') VALUE1=FCSFTX
                  IF(WQ.EQ.'FCSFTY') VALUE1=FCSFTY
                  IF(WQ.EQ.'RMSASPCT') THEN
                      IF(RMSX.NE.0.0D0) THEN
                          VALUE1=RMSY/RMSX
                      ELSE
                          WRITE(OUTLYNE,*) 'RMSASPCT NOR CALCULABLE'
                          CALL SHOWIT(1)
                          VALUE1=0.0D0
                      END IF
                  END IF
                  GO TO 200
              END IF
              IF(WQ.EQ.'KDWA') THEN
                  VALUE1=DBLE(OPCNT)
                  GO TO 200
              END IF
              IF(WQ.EQ.'NVAR') THEN
                  VALUE1=DBLE(VBCNT)
                  GO TO 200
              END IF
              IF(WQ.EQ.'FOCRIT') THEN
C                        VALUE1=DBLE(TOPCNT)
                  OUTLYNE='"FOCRIT" NOT YET GETABLE'
                  CALL SHOWIT(1)
                  VALUE1=0.0D0
                  GO TO 200
              END IF
              IF(WQ.EQ.'LCV') THEN
                  VALUE1=LCVLCV
                  GO TO 200
              END IF
              IF(WQ.EQ.'PFAC') THEN
                  VALUE1=PFAC
                  GO TO 200
              END IF
              IF(WQ.EQ.'SURTOL') THEN
                  VALUE1=SURTOL
                  GO TO 200
              END IF
              IF(WQ.EQ.'AIMTOL') THEN
                  VALUE1=AIMTOL
                  GO TO 200
              END IF
              IF(WQ.EQ.'CAIMTOL') THEN
                  VALUE1=CAIMTOL
                  GO TO 200
              END IF
              IF(WQ.EQ.'MRAYS') THEN
                  VALUE1=MRAYS
                  GO TO 200
              END IF
              IF(WQ.EQ.'SAGDEL') THEN
                  VALUE1=SAGDEL
                  GO TO 200
              END IF
              IF(WQ.EQ.'SERINC') THEN
                  VALUE1=SERINC
                  GO TO 200
              END IF
              IF(WQ.EQ.'SERLIM') THEN
                  VALUE1=SERLIM
                  GO TO 200
              END IF
              IF(WQ.EQ.'DELSUR') THEN
                  VALUE1=DELSUR
                  GO TO 200
              END IF
              IF(WQ.EQ.'NRAITR') THEN
                  VALUE1=DBLE(NRAITR)
                  GO TO 200
              END IF
              IF(WQ.EQ.'DINMUL') THEN
                  VALUE1=DINMUL
                  GO TO 200
              END IF
              IF(WQ.EQ.'LINTOL') THEN
                  VALUE1=LINTOL
                  GO TO 200
              END IF
              IF(WQ.EQ.'ONTOL') THEN
                  VALUE1=ONTOL
                  GO TO 200
              END IF
              IF(WQ.EQ.'SINGTOL') THEN
                  VALUE1=SINGTOL
                  GO TO 200
              END IF
              IF(WQ.EQ.'DIFTOL') THEN
                  VALUE1=DIFTOL
                  GO TO 200
              END IF
              IF(WQ.EQ.'MODE') THEN
                  VALUE1=SYSTEM1(30)
                  GO TO 200
              END IF
              IF(WQ.EQ.'XPEN') THEN
                  VALUE1=DBLE(XPEN)
                  GO TO 200
              END IF
              IF(WQ.EQ.'YPEN') THEN
                  VALUE1=DBLE(YPEN)
                  GO TO 200
              END IF
              IF(WQ.EQ.'XPENOL') THEN
                  VALUE1=DBLE(XPENOL)
                  GO TO 200
              END IF
              IF(WQ.EQ.'OPD') THEN
                  CALL GETOPD(VALL,DUMMY,OPDERROR)
                  IF(OPDERROR) THEN
                      CALL MACFAL
                      RETURN
                  ELSE
                      VALUE1=VALL
                      GO TO 200
                  END IF
              END IF
              IF(WQ.EQ.'OPDW') THEN
                  CALL GETOPD(DUMMY,VALL,OPDERROR)
                  IF(OPDERROR) THEN
                      CALL MACFAL
                      RETURN
                  ELSE
                      VALUE1=VALL
                      GO TO 200
                  END IF
              END IF
              IF(WQ.EQ.'YPENOL') THEN
                  VALUE1=DBLE(YPENOL)
                  GO TO 200
              END IF
              IF(WQ.EQ.'PENSTA') THEN
                  VALUE1=DBLE(PENSTA)
                  GO TO 200
              END IF
              IF(WQ.EQ.'SCALX'.OR.WQ.EQ.'SCALY') THEN
                  OUTLYNE='PARAMETER NOT CURRENTLY "GET"-ABLE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
C
C       NOW FOR THINGS THAT TAKE ONLY NUMERIC WORD
C       NUMBER 1 NUMERIC INPUT
C
          IF(WQ.EQ.'CV'.OR.WQ.EQ.'CVTOR'.OR.WQ.EQ.'TH'.OR.WQ.EQ.'INR'.OR.
     1    WQ.EQ.'AD'.OR.WQ.EQ.'AE'.OR.WQ.EQ.'AF'.OR.WQ.EQ.'SHAPEFAC'.OR.
     2    WQ.EQ.'AG'.OR.WQ.EQ.'CC'.OR.WQ.EQ.'ALPHA'.OR.WQ.EQ.'AH'.OR.WQ
     3    .EQ.'BETA'.OR.WQ.EQ.'GAMMA'.OR.WQ.EQ.'TCODE'.OR.WQ.EQ.'AI'.OR.
     4    WQ.EQ.'CCTOR'.OR.WQ.EQ.'ADTOR'.OR.WQ.EQ.'AETOR'.OR.WQ.EQ.'AJ'.OR.
     1    WQ.EQ.'GBDISX'.OR.WQ.EQ.'GBDISY'.OR.WQ.EQ.'GBWAISTX'.OR.WQ.EQ.
     1    'GBWAISTY'.OR.WQ.EQ.'ZD'.OR.WQ.EQ.'PTOVOPD'.OR.WQ
     5    .EQ.'AFTOR'.OR.WQ.EQ.'AGTOR'.OR.WQ.EQ.'XD'.OR.WQ.EQ.'AK'.OR.WQ.EQ.
     6    'YD'.OR.WQ.EQ.'RD'.OR.WQ.EQ.'RDTOR'.OR.WQ.EQ.'X'.OR.WQ.EQ.'AL'
     7    .OR.WQ.EQ.'Y'.OR.WQ.EQ.'Z'.OR.WQ.EQ.'DX'.OR.WQ.EQ.'RENERGY'.OR.WQ
     8    .EQ.'DY'.OR.WQ.EQ.'XANG'.OR.WQ.EQ.'YANG'.OR.WQ.EQ.'AII'.OR.WQ.EQ.
     9    'NUMHITS'.OR.WQ.EQ.'K'.OR.WQ.EQ.'M'.OR.WQ.EQ.'L'
     9    .OR.WQ.EQ.'DCL'.OR.WQ.EQ.'DCM'.OR.WQ.EQ.'DCN'.OR.WQ.EQ.'AC'.OR.
     1    WQ.EQ.'RMSOPD'.OR.WQ.EQ.'SPTWT'.OR.WQ.EQ.'GDX'.OR.WQ.EQ.'GDY'
     2    .OR.WQ.EQ.'GDZ'.OR.WQ.EQ.'GALPHA'.OR.WQ.EQ.'GBETA'.OR.WQ.EQ.
     3    'GGAMMA'.OR.WQ.EQ.'GRS'.OR.WQ.EQ.'GRO'.OR.WQ.EQ.'GRX'.OR.WQ
     4    .EQ.'GRY'.OR.WQ.EQ.'GRZ'.AND.WQ.EQ.'THM'.AND.WQ.EQ.'PRICE') THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
          IF(WQ.EQ.'SA3'.OR.WQ.EQ.'XSA3'.OR.WQ.EQ.'CMA3'.OR.
     1    WQ.EQ.'XCMA3'.OR.WQ.EQ.'AST3'.OR.WQ.EQ.'XAST3'.OR.WQ
     2    .EQ.'DIS3'.OR.WQ.EQ.'XDIS3'.OR.WQ.EQ.'PTZ3'.OR.WQ
     3    .EQ.'XPTZ3'.OR.WQ.EQ.'SA5'.OR.WQ.EQ.'XSA5'.OR.
     4    WQ.EQ.'CMA5'.OR.WQ.EQ.'XCMA5'.OR.WQ.EQ.'AST5'.OR.WQ
     5    .EQ.'XAST5'.OR.WQ.EQ.'DIS5'.OR.WQ.EQ.'XDIS5'.OR.WQ.EQ.
     6    'PTZ5'.OR.WQ.EQ.'XPTZ5'.OR.WQ.EQ.'TOBSA'.OR.WQ.EQ.'XTOBSA'
     7    .OR.WQ.EQ.'SOBSA'.OR.WQ.EQ.'XSOBSA'.OR.WQ.EQ.'ELCMA'.OR.WQ.EQ.
     8    'XELCMA'.OR.WQ.EQ.'TAS'.OR.WQ.EQ.'XTAS'.OR.WQ.EQ.'SAS'
     9    .OR.WQ.EQ.'XSAS'.OR.WQ.EQ.'SA7'.OR.WQ.EQ.'XSA7'
     1    .OR.WQ.EQ.'PTZCV'.OR.WQ.EQ.'MINREG'.OR.WQ.EQ.'MAXREG'.OR.WQ.EQ.
     2    'XPTZCV'.OR.WQ.EQ.'COATING') THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
          COEFJK=.FALSE.
          IF(WQ.EQ.'C1') COEFJK=.TRUE.
          IF(WQ.EQ.'C2') COEFJK=.TRUE.
          IF(WQ.EQ.'C3') COEFJK=.TRUE.
          IF(WQ.EQ.'C4') COEFJK=.TRUE.
          IF(WQ.EQ.'C5') COEFJK=.TRUE.
          IF(WQ.EQ.'C6') COEFJK=.TRUE.
          IF(WQ.EQ.'C7') COEFJK=.TRUE.
          IF(WQ.EQ.'C8') COEFJK=.TRUE.
          IF(WQ.EQ.'C9') COEFJK=.TRUE.
          IF(WQ.EQ.'C10') COEFJK=.TRUE.
          IF(WQ.EQ.'C11') COEFJK=.TRUE.
          IF(WQ.EQ.'C12') COEFJK=.TRUE.
          IF(WQ.EQ.'C13') COEFJK=.TRUE.
          IF(WQ.EQ.'C14') COEFJK=.TRUE.
          IF(WQ.EQ.'C15') COEFJK=.TRUE.
          IF(WQ.EQ.'C16') COEFJK=.TRUE.
          IF(WQ.EQ.'C17') COEFJK=.TRUE.
          IF(WQ.EQ.'C18') COEFJK=.TRUE.
          IF(WQ.EQ.'C19') COEFJK=.TRUE.
          IF(WQ.EQ.'C20') COEFJK=.TRUE.
          IF(WQ.EQ.'C21') COEFJK=.TRUE.
          IF(WQ.EQ.'C22') COEFJK=.TRUE.
          IF(WQ.EQ.'C23') COEFJK=.TRUE.
          IF(WQ.EQ.'C24') COEFJK=.TRUE.
          IF(WQ.EQ.'C25') COEFJK=.TRUE.
          IF(WQ.EQ.'C26') COEFJK=.TRUE.
          IF(WQ.EQ.'C27') COEFJK=.TRUE.
          IF(WQ.EQ.'C28') COEFJK=.TRUE.
          IF(WQ.EQ.'C29') COEFJK=.TRUE.
          IF(WQ.EQ.'C30') COEFJK=.TRUE.
          IF(WQ.EQ.'C31') COEFJK=.TRUE.
          IF(WQ.EQ.'C32') COEFJK=.TRUE.
          IF(WQ.EQ.'C33') COEFJK=.TRUE.
          IF(WQ.EQ.'C34') COEFJK=.TRUE.
          IF(WQ.EQ.'C35') COEFJK=.TRUE.
          IF(WQ.EQ.'C36') COEFJK=.TRUE.
          IF(WQ.EQ.'C37') COEFJK=.TRUE.
          IF(WQ.EQ.'C38') COEFJK=.TRUE.
          IF(WQ.EQ.'C39') COEFJK=.TRUE.
          IF(WQ.EQ.'C40') COEFJK=.TRUE.
          IF(WQ.EQ.'C41') COEFJK=.TRUE.
          IF(WQ.EQ.'C42') COEFJK=.TRUE.
          IF(WQ.EQ.'C43') COEFJK=.TRUE.
          IF(WQ.EQ.'C44') COEFJK=.TRUE.
          IF(WQ.EQ.'C45') COEFJK=.TRUE.
          IF(WQ.EQ.'C46') COEFJK=.TRUE.
          IF(WQ.EQ.'C47') COEFJK=.TRUE.
          IF(WQ.EQ.'C48') COEFJK=.TRUE.
          IF(WQ.EQ.'C49') COEFJK=.TRUE.
          IF(WQ.EQ.'C50') COEFJK=.TRUE.
          IF(WQ.EQ.'C51') COEFJK=.TRUE.
          IF(WQ.EQ.'C52') COEFJK=.TRUE.
          IF(WQ.EQ.'C53') COEFJK=.TRUE.
          IF(WQ.EQ.'C54') COEFJK=.TRUE.
          IF(WQ.EQ.'C55') COEFJK=.TRUE.
          IF(WQ.EQ.'C56') COEFJK=.TRUE.
          IF(WQ.EQ.'C57') COEFJK=.TRUE.
          IF(WQ.EQ.'C58') COEFJK=.TRUE.
          IF(WQ.EQ.'C59') COEFJK=.TRUE.
          IF(WQ.EQ.'C60') COEFJK=.TRUE.
          IF(WQ.EQ.'C61') COEFJK=.TRUE.
          IF(WQ.EQ.'C62') COEFJK=.TRUE.
          IF(WQ.EQ.'C63') COEFJK=.TRUE.
          IF(WQ.EQ.'C64') COEFJK=.TRUE.
          IF(WQ.EQ.'C65') COEFJK=.TRUE.
          IF(WQ.EQ.'C66') COEFJK=.TRUE.
          IF(WQ.EQ.'C67') COEFJK=.TRUE.
          IF(WQ.EQ.'C68') COEFJK=.TRUE.
          IF(WQ.EQ.'C69') COEFJK=.TRUE.
          IF(WQ.EQ.'C70') COEFJK=.TRUE.
          IF(WQ.EQ.'C71') COEFJK=.TRUE.
          IF(WQ.EQ.'C72') COEFJK=.TRUE.
          IF(WQ.EQ.'C73') COEFJK=.TRUE.
          IF(WQ.EQ.'C74') COEFJK=.TRUE.
          IF(WQ.EQ.'C75') COEFJK=.TRUE.
          IF(WQ.EQ.'C76') COEFJK=.TRUE.
          IF(WQ.EQ.'C77') COEFJK=.TRUE.
          IF(WQ.EQ.'C78') COEFJK=.TRUE.
          IF(WQ.EQ.'C79') COEFJK=.TRUE.
          IF(WQ.EQ.'C80') COEFJK=.TRUE.
          IF(WQ.EQ.'C81') COEFJK=.TRUE.
          IF(WQ.EQ.'C82') COEFJK=.TRUE.
          IF(WQ.EQ.'C83') COEFJK=.TRUE.
          IF(WQ.EQ.'C84') COEFJK=.TRUE.
          IF(WQ.EQ.'C85') COEFJK=.TRUE.
          IF(WQ.EQ.'C86') COEFJK=.TRUE.
          IF(WQ.EQ.'C87') COEFJK=.TRUE.
          IF(WQ.EQ.'C88') COEFJK=.TRUE.
          IF(WQ.EQ.'C89') COEFJK=.TRUE.
          IF(WQ.EQ.'C90') COEFJK=.TRUE.
          IF(WQ.EQ.'C91') COEFJK=.TRUE.
          IF(WQ.EQ.'C92') COEFJK=.TRUE.
          IF(WQ.EQ.'C93') COEFJK=.TRUE.
          IF(WQ.EQ.'C94') COEFJK=.TRUE.
          IF(WQ.EQ.'C95') COEFJK=.TRUE.
          IF(WQ.EQ.'C96') COEFJK=.TRUE.
          IF(COEFJK) THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
C
          IF(WQ.EQ.'SA3P'.OR.WQ.EQ.'XSA3P'.OR.WQ.EQ.'CMA3P'.OR.
     1    WQ.EQ.'XCMA3P'.OR.WQ.EQ.'AST3P'.OR.WQ.EQ.'XAST3P'.OR.WQ
     2    .EQ.'DIS3P'.OR.WQ.EQ.'XDIS3P'.OR.WQ.EQ.'PTZ3P'.OR.WQ
     3    .EQ.'XPTZ3P'.OR.WQ.EQ.'SA5P'.OR.WQ.EQ.'XSA5P'.OR.
     4    WQ.EQ.'CMA3P'.OR.WQ.EQ.'XCMA3P'.OR.WQ.EQ.'AST5P'.OR.WQ
     5    .EQ.'XAST5P'.OR.WQ.EQ.'DIS5P'.OR.WQ.EQ.'XDIS5P'.OR.WQ.EQ.
     6    'PTZ5P'.OR.WQ.EQ.'XPTZ5P'.OR.WQ.EQ.'TOBSAP'.OR.WQ.EQ.'XTOBSAP'
     7    .OR.WQ.EQ.'SOBSAP'.OR.WQ.EQ.'XSOBSAP'.OR.WQ.EQ.'ELCMAP'.OR.WQ
     8    .EQ.'XELCMAP'.OR.WQ.EQ.'TASP'.OR.WQ.EQ.'XTASP'.OR.WQ.EQ.'SASP'
     9    .OR.WQ.EQ.'XSASP'.OR.WQ.EQ.'SA7P'.OR.WQ.EQ.'XSA7P') THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
          IF(WQ.EQ.'SA3S'.OR.WQ.EQ.'XSA3S'.OR.WQ.EQ.'CMA3S'.OR.
     1    WQ.EQ.'XCMA3S'.OR.WQ.EQ.'AST3S'.OR.WQ.EQ.'XAST3S'.OR.WQ
     2    .EQ.'DIS3S'.OR.WQ.EQ.'XDIS3S'.OR.WQ.EQ.'PTZ3S'.OR.WQ
     3    .EQ.'XPTZ3S'.OR.WQ.EQ.'SA5S'.OR.WQ.EQ.'XSA5S'.OR.
     4    WQ.EQ.'CMA3S'.OR.WQ.EQ.'XCMA3S'.OR.WQ.EQ.'AST5S'.OR.WQ
     5    .EQ.'XAST5S'.OR.WQ.EQ.'DIS5S'.OR.WQ.EQ.'XDIS5S'.OR.WQ.EQ.
     6    'PTZ5S'.OR.WQ.EQ.'XPTZ5S'.OR.WQ.EQ.'TOBSAS'.OR.WQ.EQ.'XTOBSAS'
     7    .OR.WQ.EQ.'SOBSAS'.OR.WQ.EQ.'XSOBSAS'.OR.WQ.EQ.'ELCMAS'.OR.WQ
     8    .EQ.'XELCMAS'.OR.WQ.EQ.'TASS'.OR.WQ.EQ.'XTASS'.OR.WQ.EQ.'SASS'
     9    .OR.WQ.EQ.'XSASS'.OR.WQ.EQ.'SA7S'.OR.WQ.EQ.'XSA7S')THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
          IF(WQ.EQ.'SA5I'.OR.WQ.EQ.'XSA5I'.OR.
     4    WQ.EQ.'CMA5I'.OR.WQ.EQ.'XCMA5I'.OR.WQ.EQ.'AST5I'.OR.WQ
     5    .EQ.'XAST5I'.OR.WQ.EQ.'DIS5I'.OR.WQ.EQ.'XDIS5I'.OR.WQ.EQ.
     6    'PTZ5I'.OR.WQ.EQ.'XPTZ5I'.OR.WQ.EQ.'TOBSAI'.OR.WQ.EQ.'XTOBSAI'
     7    .OR.WQ.EQ.'SOBSAI'.OR.WQ.EQ.'XSOBSAI'.OR.WQ.EQ.'ELCMAI'.OR.WQ
     8    .EQ.'XELCMAI'.OR.WQ.EQ.'TASI'.OR.WQ.EQ.'XTASI'.OR.WQ.EQ.'SASI'
     9    .OR.WQ.EQ.'XSASI'.OR.WQ.EQ.'SA7I'.OR.WQ.EQ.'XSA7I'
     4    .OR.WQ.EQ.'CHFIMX'.OR.WQ.EQ.'PUPDIAX'.OR.WQ.EQ.'PUPDISX'
     4    .OR.WQ.EQ.'IMDISX'.OR.WQ.EQ.'PUPDIAY'.OR.WQ.EQ.'PUPDISY'.OR.WQ
     5    .EQ.'IMDISY'.OR.WQ.EQ.'CHFIMY') THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
          IF(WQ.EQ.'PSA3'.OR.WQ.EQ.'XPSA3'.OR.WQ.EQ.'PCMA3'.OR.
     1    WQ.EQ.'XPCMA3'.OR.WQ.EQ.'PAST3'.OR.WQ.EQ.'XPAST3'.OR.WQ
     2    .EQ.'PDIS3'.OR.WQ.EQ.'XPDIS3'.OR.WQ.EQ.'PPTZ3'.OR.WQ
     3    .EQ.'XPPTZ3'.OR.WQ.EQ.'DR'.OR.WQ.EQ.'DRA'
     4    .OR.WQ.EQ.'ZERN37'.OR.WQ.EQ.'OPWT'
     5    .OR.WQ.EQ.'OPTYPE'.OR.WQ.EQ.'VARDNC'.OR.WQ.EQ.'VARWT') THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
          IF(WQ.EQ.'PSA3P'.OR.WQ.EQ.'XPSA3P'.OR.WQ.EQ.'PCMA3P'.OR.
     1    WQ.EQ.'XPCMA3P'.OR.WQ.EQ.'PAST3P'.OR.WQ.EQ.'XPAST3P'.OR.WQ
     2    .EQ.'PDIS3P'.OR.WQ.EQ.'XPDIS3P'.OR.WQ.EQ.'PPTZ3P'.OR.WQ
     3    .EQ.'XPPTZ3P') THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
          IF(WQ.EQ.'PACY'.OR.WQ.EQ.'PACX'.OR.WQ.EQ.'PLCY'.OR.
     1    WQ.EQ.'PLCX'.OR.WQ.EQ.'SACY'.OR.WQ.EQ.'SACX'.OR.WQ
     2    .EQ.'SLCY'.OR.WQ.EQ.'SLCX'.OR.WQ.EQ.'ET'.OR.WQ.EQ.'ETY'.OR.
     3    WQ.EQ.'ETX') THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
          IF(WQ.EQ.'PSA3S'.OR.WQ.EQ.'PXSA3S'.OR.WQ.EQ.'PCMA3S'.OR.
     1    WQ.EQ.'XPCMA3S'.OR.WQ.EQ.'PAST3S'.OR.WQ.EQ.'XPAST3S'.OR.WQ
     2    .EQ.'PDIS3S'.OR.WQ.EQ.'XPDIS3S'.OR.WQ.EQ.'PPTZ3S'.OR.WQ
     3    .EQ.'XPPTZ3S') THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
C
C       GLOBAL RAY DATA
C
          IF(WQ.EQ.'GLX'.OR.WQ.EQ.'GLY'.OR.WQ.EQ.'GLZ'
     5    .OR.WQ.EQ.'GLL'.OR.WQ.EQ.'GLM'.OR.WQ.EQ.'GLN'
     6    .OR.WQ.EQ.'GLLOLD'.OR.WQ.EQ.'GLMOLD'.OR.WQ.EQ.'GLNOLD'
     7    .OR.WQ.EQ.'GLLN'.OR.WQ.EQ.'GLMN'.OR.WQ.EQ.'GLNN')THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
          IF(WQ.EQ.'INDEX'.OR.WQ.EQ.
     1    'OPL'.OR.WQ.EQ.'WV'.OR.WQ.EQ.'PXPX'.OR.WQ.EQ.'PXPY'
     2    .OR.WQ.EQ.'PXPY'.OR.WQ.EQ.'PYPY'.OR.WQ.EQ.'PYPX'
     3    .OR.WQ.EQ.'DXDX'.OR.WQ.EQ.'DXDY'.OR.WQ.EQ.'DYDY'.OR.
     4    WQ.EQ.'DYDX'.OR.WQ.EQ.'PXAPX'.OR.WQ.EQ.'PXAPY'.OR.
     5    WQ.EQ.'PYAPY'.OR.WQ.EQ.'PYAPX'.OR.WQ.EQ.'DXADX'.OR.
     6    WQ.EQ.'DXADY'.OR.WQ.EQ.'DYADY'.OR.WQ.EQ.'DYADX'.OR.
     7    WQ.EQ.'VNUM'.OR.WQ.EQ.'MCODE'.OR.WQ.EQ.'CLAP'.OR.WQ.EQ.
     8    'CLAPY'.OR.WQ.EQ.'CLAPX'.OR.WQ.EQ.'COBS'.OR.WQ.EQ.'COBSY'
     9    .OR.WQ.EQ.'COBSX'.OR.WQ.EQ.'PARTL'.OR.WQ.EQ.'ABBE'.OR.
     1    WQ.EQ.'N1'.OR.WQ.EQ.'N2'.OR.WQ.EQ.'N3'.OR.WQ.EQ.'N4'.OR.
     1    WQ.EQ.'N5'.OR.WQ.EQ.'N6'.OR.WQ.EQ.'N7'.OR.WQ.EQ.'N8'.OR.
     1    WQ.EQ.'N9'.OR.WQ.EQ.'N10'.OR.WQ.EQ.'DPART') THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
          IF(WQ.EQ.'VB'.OR.WQ.EQ.'WVFC'.OR.WQ.EQ.'GBDISX'.OR.WQ.EQ.
     1    'TV'.OR.WQ.EQ.'OPRD'.OR.WQ.EQ.'WT'.OR.WQ.EQ.'GBDISY'.OR.
     2    WQ.EQ.'CLDECX'.OR.WQ.EQ.'CLDECY'.OR.WQ.EQ.'GBWAISTX'.OR.
     3    WQ.EQ.'CODECX'.OR.WQ.EQ.'CODECY'.OR.WQ.EQ.'CLTILT'.OR.WQ
     4    .EQ.'COTILT'.OR.WQ.EQ.'LOLD'.OR.WQ.EQ.'MOLD'.OR.
     5    WQ.EQ.'XREF'.OR.WQ.EQ.'YREF'.OR.WQ.EQ.'ZREF'.OR.WQ
     6    .EQ.'LREF'.OR.WQ.EQ.'MREF'.OR.WQ.EQ.'NREF'.OR.WQ.EQ.
     7    'LENREF'.OR.WQ.EQ.'OPLREF'.OR.WQ.EQ.'IREF'.OR.WQ.EQ.
     8    'GBWAISTY'.OR.WQ.EQ.'FLDSX'.OR.WQ.EQ.'FLDSY'.OR.
     8    WQ.EQ.'IPREF'.OR.WQ.EQ.'LNREF'.OR.WQ.EQ.'MNREF')THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 21
          END IF
          IF(WQ.EQ.'NNREF'.OR.WQ.EQ.'XAREF'.OR.WQ.EQ.'YAREF'.OR.
     5    WQ.EQ.'AII'.OR.WQ.EQ.'LEN'.OR.WQ.EQ.'DXA'.OR.WQ.EQ.'DYA'
     8    .OR.WQ.EQ.'AIP'.OR.WQ.EQ.'LN'.OR.WQ.EQ.'MN'.OR.WQ.EQ.
     9    'NN'.OR.WQ.EQ.'CLRAD'.OR.WQ.EQ.
     8    'CORAD'.OR.
     9    WQ.EQ.'NOLD'.OR.WQ.EQ.'SCEX'.OR.WQ.EQ.'SCEY') THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       NOW DO SOMETHING, THEN GO TO 200 TO STORE, THEN RETURN
C
C       PROCESS THE GET REQUESTS WITH ONE NUMERIC WORD
          ELSE
C     WQ NOT MATCHED FOR ONE NUMERIC INPUT, GO TO 2121
C     AND DO THINGS WITH 2 NUMERIC WORDS PLUS NW5
              GO TO 2121
          END IF
 21       CONTINUE
C
C     DO SCEX AND SCEY
C
          IF(WQ.EQ.'SCEX'.OR.WQ.EQ.'SCEY') THEN
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY USES NUMERIC WORDS #1 AND #5'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH CALCULATIONS
              ERROR=0
              V1=W1
              IF(WQ.EQ.'SCEX') CACOCH=0
              IF(WQ.EQ.'SCEY') CACOCH=0
              IF(WQ.EQ.'SCEX') CALL SCE(VALVAL,1,V1,ERROR)
              IF(WQ.EQ.'SCEY') CALL SCE(VALVAL,2,V1,ERROR)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"',WQ(1:4),'" IS NOT CURRENTLY CALCULABLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              VALUE1=VALVAL
              GO TO 200
          END IF
C
C       ON 3/5/91 THE CODE TO GET THE 3RD,5TH AND 7TH ORDER ABERRATIONS
C       AND THEIR CHROMATIC DIFFERENCES WERE ADDED HERE VIA A
C       CHECK FOR THE CORRECT QUALIFIER WORD AND A CALL TO THE SUBROUTINE
C       G357.FOR WHICH RETURNS THE QUANTITY (VALUE1) AS ITS ONLY
C       ARGUMENT
C       3RD, 5TH AND 7TH ORDER
          IF(WQ.EQ.'SA3')          TF357=.TRUE.
          IF(WQ.EQ.'XSA3')         TF357=.TRUE.
          IF(WQ.EQ.'CMA3')         TF357=.TRUE.
          IF(WQ.EQ.'XCMA3')        TF357=.TRUE.
          IF(WQ.EQ.'AST3')         TF357=.TRUE.
          IF(WQ.EQ.'XAST3')        TF357=.TRUE.
          IF(WQ.EQ.'DIS3')         TF357=.TRUE.
          IF(WQ.EQ.'XDIS3')        TF357=.TRUE.
          IF(WQ.EQ.'PTZ3')         TF357=.TRUE.
          IF(WQ.EQ.'XPTZ3')        TF357=.TRUE.
          IF(WQ.EQ.'PTZCV')        TF357=.TRUE.
          IF(WQ.EQ.'XPTZCV')       TF357=.TRUE.
          IF(WQ.EQ.'SA5')          TF357=.TRUE.
          IF(WQ.EQ.'XSA5')         TF357=.TRUE.
          IF(WQ.EQ.'CMA5')         TF357=.TRUE.
          IF(WQ.EQ.'XCMA5')        TF357=.TRUE.
          IF(WQ.EQ.'AST5')         TF357=.TRUE.
          IF(WQ.EQ.'XAST5')        TF357=.TRUE.
          IF(WQ.EQ.'PTZ5')         TF357=.TRUE.
          IF(WQ.EQ.'XPTZ5')        TF357=.TRUE.
          IF(WQ.EQ.'TOBSA')        TF357=.TRUE.
          IF(WQ.EQ.'XTOBSA')       TF357=.TRUE.
          IF(WQ.EQ.'SOBSA')        TF357=.TRUE.
          IF(WQ.EQ.'XSOBSA')       TF357=.TRUE.
          IF(WQ.EQ.'ELCMA')        TF357=.TRUE.
          IF(WQ.EQ.'XELCMA')       TF357=.TRUE.
          IF(WQ.EQ.'TAS')          TF357=.TRUE.
          IF(WQ.EQ.'XTAS')         TF357=.TRUE.
          IF(WQ.EQ.'SAS')          TF357=.TRUE.
          IF(WQ.EQ.'XSAS')         TF357=.TRUE.
          IF(WQ.EQ.'SA7')          TF357=.TRUE.
          IF(WQ.EQ.'XSA7')         TF357=.TRUE.
C       PRIMARY 3RD, 5TH AND 7TH CHROMATIC DIFFERENCES
          IF(WQ.EQ.'SA3P')          TF357=.TRUE.
          IF(WQ.EQ.'XSA3P')         TF357=.TRUE.
          IF(WQ.EQ.'CMA3P')         TF357=.TRUE.
          IF(WQ.EQ.'XCMA3P')        TF357=.TRUE.
          IF(WQ.EQ.'AST3P')         TF357=.TRUE.
          IF(WQ.EQ.'XAST3P')        TF357=.TRUE.
          IF(WQ.EQ.'DIS3P')         TF357=.TRUE.
          IF(WQ.EQ.'XDIS3P')        TF357=.TRUE.
          IF(WQ.EQ.'PTZ3P')         TF357=.TRUE.
          IF(WQ.EQ.'XPTZ3P')        TF357=.TRUE.
          IF(WQ.EQ.'SA5P')          TF357=.TRUE.
          IF(WQ.EQ.'XSA5P')         TF357=.TRUE.
          IF(WQ.EQ.'CMA5P')         TF357=.TRUE.
          IF(WQ.EQ.'XCMA5P')        TF357=.TRUE.
          IF(WQ.EQ.'AST5P')         TF357=.TRUE.
          IF(WQ.EQ.'XAST5P')        TF357=.TRUE.
          IF(WQ.EQ.'PTZ5P')         TF357=.TRUE.
          IF(WQ.EQ.'XPTZ5P')        TF357=.TRUE.
          IF(WQ.EQ.'TOBSAP')        TF357=.TRUE.
          IF(WQ.EQ.'XTOBSAP')       TF357=.TRUE.
          IF(WQ.EQ.'SOBSAP')        TF357=.TRUE.
          IF(WQ.EQ.'XSOBSAP')       TF357=.TRUE.
          IF(WQ.EQ.'ELCMAP')        TF357=.TRUE.
          IF(WQ.EQ.'XELCMAP')       TF357=.TRUE.
          IF(WQ.EQ.'TASP')          TF357=.TRUE.
          IF(WQ.EQ.'XTASP')         TF357=.TRUE.
          IF(WQ.EQ.'SASP')          TF357=.TRUE.
          IF(WQ.EQ.'XSASP')         TF357=.TRUE.
          IF(WQ.EQ.'SA7P')          TF357=.TRUE.
          IF(WQ.EQ.'XSA7P')         TF357=.TRUE.
C       SECONDARY 3RD, 5TH AND 7TH ORDER CHROMATIC DIFFERENCES
          IF(WQ.EQ.'SA3S')          TF357=.TRUE.
          IF(WQ.EQ.'XSA3S')         TF357=.TRUE.
          IF(WQ.EQ.'CMA3S')         TF357=.TRUE.
          IF(WQ.EQ.'XCMA3S')        TF357=.TRUE.
          IF(WQ.EQ.'AST3S')         TF357=.TRUE.
          IF(WQ.EQ.'XAST3S')        TF357=.TRUE.
          IF(WQ.EQ.'DIS3S')         TF357=.TRUE.
          IF(WQ.EQ.'XDIS3S')        TF357=.TRUE.
          IF(WQ.EQ.'PTZ3S')         TF357=.TRUE.
          IF(WQ.EQ.'XPTZ3S')        TF357=.TRUE.
          IF(WQ.EQ.'SA5S')          TF357=.TRUE.
          IF(WQ.EQ.'XSA5S')         TF357=.TRUE.
          IF(WQ.EQ.'CMA5S')         TF357=.TRUE.
          IF(WQ.EQ.'XCMA5S')        TF357=.TRUE.
          IF(WQ.EQ.'AST5S')         TF357=.TRUE.
          IF(WQ.EQ.'XAST5S')        TF357=.TRUE.
          IF(WQ.EQ.'PTZ5S')         TF357=.TRUE.
          IF(WQ.EQ.'XPTZ5S')        TF357=.TRUE.
          IF(WQ.EQ.'TOBSAS')        TF357=.TRUE.
          IF(WQ.EQ.'XTOBSAS')       TF357=.TRUE.
          IF(WQ.EQ.'SOBSAS')        TF357=.TRUE.
          IF(WQ.EQ.'XSOBSAS')       TF357=.TRUE.
          IF(WQ.EQ.'ELCMAS')        TF357=.TRUE.
          IF(WQ.EQ.'XELCMAS')       TF357=.TRUE.
          IF(WQ.EQ.'TASS')          TF357=.TRUE.
          IF(WQ.EQ.'XTASS')         TF357=.TRUE.
          IF(WQ.EQ.'SASS')          TF357=.TRUE.
          IF(WQ.EQ.'XSASS')         TF357=.TRUE.
          IF(WQ.EQ.'SA7S')          TF357=.TRUE.
          IF(WQ.EQ.'XSA7S')         TF357=.TRUE.
C       THIRD ORDER PUPIL ABERRATIONS
          IF(WQ.EQ.'PSA3')          TF357=.TRUE.
          IF(WQ.EQ.'XPSA3')         TF357=.TRUE.
          IF(WQ.EQ.'PCMA3')         TF357=.TRUE.
          IF(WQ.EQ.'XPCMA3')        TF357=.TRUE.
          IF(WQ.EQ.'PAST3')         TF357=.TRUE.
          IF(WQ.EQ.'XPAST3')        TF357=.TRUE.
          IF(WQ.EQ.'PDIS3')         TF357=.TRUE.
          IF(WQ.EQ.'XPDIS3')        TF357=.TRUE.
          IF(WQ.EQ.'PPTZ3')         TF357=.TRUE.
          IF(WQ.EQ.'XPPTZ3')        TF357=.TRUE.
C       THIRD ORDER CHROMATIC PUPIL DIFFERENCES PRIMARY
          IF(WQ.EQ.'PSA3P')          TF357=.TRUE.
          IF(WQ.EQ.'XPSA3P')         TF357=.TRUE.
          IF(WQ.EQ.'PCMA3P')         TF357=.TRUE.
          IF(WQ.EQ.'XPCMA3P')        TF357=.TRUE.
          IF(WQ.EQ.'PAST3P')         TF357=.TRUE.
          IF(WQ.EQ.'XPAST3P')        TF357=.TRUE.
          IF(WQ.EQ.'PDIS3P')         TF357=.TRUE.
          IF(WQ.EQ.'XPDIS3P')        TF357=.TRUE.
          IF(WQ.EQ.'PPTZ3P')         TF357=.TRUE.
          IF(WQ.EQ.'XPPTZ3P')        TF357=.TRUE.
C       THIRD ORDER CHROMATIC PUPIL DIFFERENCES SECONDARY
          IF(WQ.EQ.'PSA3S')          TF357=.TRUE.
          IF(WQ.EQ.'XPSA3S')         TF357=.TRUE.
          IF(WQ.EQ.'PCMA3S')         TF357=.TRUE.
          IF(WQ.EQ.'XPCMA3S')        TF357=.TRUE.
          IF(WQ.EQ.'PAST3S')         TF357=.TRUE.
          IF(WQ.EQ.'XPAST3S')        TF357=.TRUE.
          IF(WQ.EQ.'PDIS3S')         TF357=.TRUE.
          IF(WQ.EQ.'XPDIS3S')        TF357=.TRUE.
          IF(WQ.EQ.'PPTZ3S')         TF357=.TRUE.
C       5TH AND 7TH INTRINSIC CONTRIBUTIONS
          IF(WQ.EQ.'SA5I')          TF357=.TRUE.
          IF(WQ.EQ.'XSA5I')         TF357=.TRUE.
          IF(WQ.EQ.'CMA5I')         TF357=.TRUE.
          IF(WQ.EQ.'XCMA5I')        TF357=.TRUE.
          IF(WQ.EQ.'AST5I')         TF357=.TRUE.
          IF(WQ.EQ.'XAST5I')        TF357=.TRUE.
          IF(WQ.EQ.'PTZ5I')         TF357=.TRUE.
          IF(WQ.EQ.'XPTZ5I')        TF357=.TRUE.
          IF(WQ.EQ.'TOBSAI')        TF357=.TRUE.
          IF(WQ.EQ.'XTOBSAI')       TF357=.TRUE.
          IF(WQ.EQ.'SOBSAI')        TF357=.TRUE.
          IF(WQ.EQ.'XSOBSAI')       TF357=.TRUE.
          IF(WQ.EQ.'ELCMAI')        TF357=.TRUE.
          IF(WQ.EQ.'XELCMAI')       TF357=.TRUE.
          IF(WQ.EQ.'TASI')          TF357=.TRUE.
          IF(WQ.EQ.'XTASI')         TF357=.TRUE.
          IF(WQ.EQ.'SASI')          TF357=.TRUE.
          IF(WQ.EQ.'XSASI')         TF357=.TRUE.
          IF(WQ.EQ.'SA7I')          TF357=.TRUE.
          IF(WQ.EQ.'XSA7I')         TF357=.TRUE.
C       CHROMATIC ABERRATIONS
          IF(WQ.EQ.'PACY')          TF357=.TRUE.
          IF(WQ.EQ.'PACX')          TF357=.TRUE.
          IF(WQ.EQ.'PLCY')          TF357=.TRUE.
          IF(WQ.EQ.'PLCX')          TF357=.TRUE.
          IF(WQ.EQ.'SACY')          TF357=.TRUE.
          IF(WQ.EQ.'SACX')          TF357=.TRUE.
          IF(WQ.EQ.'SLCY')          TF357=.TRUE.
          IF(WQ.EQ.'SLCX')          TF357=.TRUE.
          IF(WQ.EQ.'CHFIMX')        TF357=.TRUE.
          IF(WQ.EQ.'CHFIMY')        TF357=.TRUE.
          IF(WQ.EQ.'PUPDIAY')       TF357=.TRUE.
          IF(WQ.EQ.'PUPDIAX')       TF357=.TRUE.
          IF(WQ.EQ.'IMDISX')        TF357=.TRUE.
          IF(WQ.EQ.'IMDISY')        TF357=.TRUE.
          IF(WQ.EQ.'PUPDISX')       TF357=.TRUE.
          IF(WQ.EQ.'PUPDISY')       TF357=.TRUE.
          IF(TF357) THEN
              IF(W1.LT.0.0D0) NEG=.TRUE.
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(INT(W1).LT.0.OR.INT(W1).GT.INT(SYSTEM1(20))) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              CALL G357
              TF357=.FALSE.
              GO TO 200
          END IF
C
C       ON 1/10/94 THE CODE TO GET C1 TO C96 WAS ADDED
          COEFJK=.FALSE.
          IF(WQ.EQ.'C1') COEFJK=.TRUE.
          IF(WQ.EQ.'C2') COEFJK=.TRUE.
          IF(WQ.EQ.'C3') COEFJK=.TRUE.
          IF(WQ.EQ.'C4') COEFJK=.TRUE.
          IF(WQ.EQ.'C5') COEFJK=.TRUE.
          IF(WQ.EQ.'C6') COEFJK=.TRUE.
          IF(WQ.EQ.'C7') COEFJK=.TRUE.
          IF(WQ.EQ.'C8') COEFJK=.TRUE.
          IF(WQ.EQ.'C9') COEFJK=.TRUE.
          IF(WQ.EQ.'C10') COEFJK=.TRUE.
          IF(WQ.EQ.'C11') COEFJK=.TRUE.
          IF(WQ.EQ.'C12') COEFJK=.TRUE.
          IF(WQ.EQ.'C13') COEFJK=.TRUE.
          IF(WQ.EQ.'C14') COEFJK=.TRUE.
          IF(WQ.EQ.'C15') COEFJK=.TRUE.
          IF(WQ.EQ.'C16') COEFJK=.TRUE.
          IF(WQ.EQ.'C17') COEFJK=.TRUE.
          IF(WQ.EQ.'C18') COEFJK=.TRUE.
          IF(WQ.EQ.'C19') COEFJK=.TRUE.
          IF(WQ.EQ.'C20') COEFJK=.TRUE.
          IF(WQ.EQ.'C21') COEFJK=.TRUE.
          IF(WQ.EQ.'C22') COEFJK=.TRUE.
          IF(WQ.EQ.'C23') COEFJK=.TRUE.
          IF(WQ.EQ.'C24') COEFJK=.TRUE.
          IF(WQ.EQ.'C25') COEFJK=.TRUE.
          IF(WQ.EQ.'C26') COEFJK=.TRUE.
          IF(WQ.EQ.'C27') COEFJK=.TRUE.
          IF(WQ.EQ.'C28') COEFJK=.TRUE.
          IF(WQ.EQ.'C29') COEFJK=.TRUE.
          IF(WQ.EQ.'C30') COEFJK=.TRUE.
          IF(WQ.EQ.'C31') COEFJK=.TRUE.
          IF(WQ.EQ.'C32') COEFJK=.TRUE.
          IF(WQ.EQ.'C33') COEFJK=.TRUE.
          IF(WQ.EQ.'C34') COEFJK=.TRUE.
          IF(WQ.EQ.'C35') COEFJK=.TRUE.
          IF(WQ.EQ.'C36') COEFJK=.TRUE.
          IF(WQ.EQ.'C37') COEFJK=.TRUE.
          IF(WQ.EQ.'C38') COEFJK=.TRUE.
          IF(WQ.EQ.'C39') COEFJK=.TRUE.
          IF(WQ.EQ.'C40') COEFJK=.TRUE.
          IF(WQ.EQ.'C41') COEFJK=.TRUE.
          IF(WQ.EQ.'C42') COEFJK=.TRUE.
          IF(WQ.EQ.'C43') COEFJK=.TRUE.
          IF(WQ.EQ.'C44') COEFJK=.TRUE.
          IF(WQ.EQ.'C45') COEFJK=.TRUE.
          IF(WQ.EQ.'C46') COEFJK=.TRUE.
          IF(WQ.EQ.'C47') COEFJK=.TRUE.
          IF(WQ.EQ.'C48') COEFJK=.TRUE.
          IF(WQ.EQ.'C49') COEFJK=.TRUE.
          IF(WQ.EQ.'C50') COEFJK=.TRUE.
          IF(WQ.EQ.'C51') COEFJK=.TRUE.
          IF(WQ.EQ.'C52') COEFJK=.TRUE.
          IF(WQ.EQ.'C53') COEFJK=.TRUE.
          IF(WQ.EQ.'C54') COEFJK=.TRUE.
          IF(WQ.EQ.'C55') COEFJK=.TRUE.
          IF(WQ.EQ.'C56') COEFJK=.TRUE.
          IF(WQ.EQ.'C57') COEFJK=.TRUE.
          IF(WQ.EQ.'C58') COEFJK=.TRUE.
          IF(WQ.EQ.'C59') COEFJK=.TRUE.
          IF(WQ.EQ.'C60') COEFJK=.TRUE.
          IF(WQ.EQ.'C61') COEFJK=.TRUE.
          IF(WQ.EQ.'C62') COEFJK=.TRUE.
          IF(WQ.EQ.'C63') COEFJK=.TRUE.
          IF(WQ.EQ.'C64') COEFJK=.TRUE.
          IF(WQ.EQ.'C65') COEFJK=.TRUE.
          IF(WQ.EQ.'C66') COEFJK=.TRUE.
          IF(WQ.EQ.'C67') COEFJK=.TRUE.
          IF(WQ.EQ.'C68') COEFJK=.TRUE.
          IF(WQ.EQ.'C69') COEFJK=.TRUE.
          IF(WQ.EQ.'C70') COEFJK=.TRUE.
          IF(WQ.EQ.'C71') COEFJK=.TRUE.
          IF(WQ.EQ.'C72') COEFJK=.TRUE.
          IF(WQ.EQ.'C73') COEFJK=.TRUE.
          IF(WQ.EQ.'C74') COEFJK=.TRUE.
          IF(WQ.EQ.'C75') COEFJK=.TRUE.
          IF(WQ.EQ.'C76') COEFJK=.TRUE.
          IF(WQ.EQ.'C77') COEFJK=.TRUE.
          IF(WQ.EQ.'C78') COEFJK=.TRUE.
          IF(WQ.EQ.'C79') COEFJK=.TRUE.
          IF(WQ.EQ.'C80') COEFJK=.TRUE.
          IF(WQ.EQ.'C81') COEFJK=.TRUE.
          IF(WQ.EQ.'C82') COEFJK=.TRUE.
          IF(WQ.EQ.'C83') COEFJK=.TRUE.
          IF(WQ.EQ.'C84') COEFJK=.TRUE.
          IF(WQ.EQ.'C85') COEFJK=.TRUE.
          IF(WQ.EQ.'C86') COEFJK=.TRUE.
          IF(WQ.EQ.'C87') COEFJK=.TRUE.
          IF(WQ.EQ.'C88') COEFJK=.TRUE.
          IF(WQ.EQ.'C89') COEFJK=.TRUE.
          IF(WQ.EQ.'C90') COEFJK=.TRUE.
          IF(WQ.EQ.'C91') COEFJK=.TRUE.
          IF(WQ.EQ.'C92') COEFJK=.TRUE.
          IF(WQ.EQ.'C93') COEFJK=.TRUE.
          IF(WQ.EQ.'C94') COEFJK=.TRUE.
          IF(WQ.EQ.'C95') COEFJK=.TRUE.
          IF(WQ.EQ.'C96') COEFJK=.TRUE.
          IF(COEFJK) THEN
              IF(W1.LT.0.0D0) NEG=.TRUE.
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(INT(W1).LT.0.OR.INT(W1).GT.INT(SYSTEM1(20))) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(WQ.EQ.'C1') VALUE1=FTFL01(1,INT(W1))
              IF(WQ.EQ.'C2') VALUE1=FTFL01(2,INT(W1))
              IF(WQ.EQ.'C3') VALUE1=FTFL01(3,INT(W1))
              IF(WQ.EQ.'C4') VALUE1=FTFL01(4,INT(W1))
              IF(WQ.EQ.'C5') VALUE1=FTFL01(5,INT(W1))
              IF(WQ.EQ.'C6') VALUE1=FTFL01(6,INT(W1))
              IF(WQ.EQ.'C7') VALUE1=FTFL01(7,INT(W1))
              IF(WQ.EQ.'C8') VALUE1=FTFL01(8,INT(W1))
              IF(WQ.EQ.'C9') VALUE1=FTFL01(9,INT(W1))
              IF(WQ.EQ.'C10') VALUE1=FTFL01(10,INT(W1))
              IF(WQ.EQ.'C11') VALUE1=FTFL01(11,INT(W1))
              IF(WQ.EQ.'C12') VALUE1=FTFL01(12,INT(W1))
              IF(WQ.EQ.'C13') VALUE1=FTFL01(13,INT(W1))
              IF(WQ.EQ.'C14') VALUE1=FTFL01(14,INT(W1))
              IF(WQ.EQ.'C15') VALUE1=FTFL01(15,INT(W1))
              IF(WQ.EQ.'C16') VALUE1=FTFL01(16,INT(W1))
              IF(WQ.EQ.'C17') VALUE1=FTFL01(17,INT(W1))
              IF(WQ.EQ.'C18') VALUE1=FTFL01(18,INT(W1))
              IF(WQ.EQ.'C19') VALUE1=FTFL01(19,INT(W1))
              IF(WQ.EQ.'C20') VALUE1=FTFL01(20,INT(W1))
              IF(WQ.EQ.'C21') VALUE1=FTFL01(21,INT(W1))
              IF(WQ.EQ.'C22') VALUE1=FTFL01(22,INT(W1))
              IF(WQ.EQ.'C23') VALUE1=FTFL01(23,INT(W1))
              IF(WQ.EQ.'C24') VALUE1=FTFL01(24,INT(W1))
              IF(WQ.EQ.'C25') VALUE1=FTFL01(25,INT(W1))
              IF(WQ.EQ.'C26') VALUE1=FTFL01(26,INT(W1))
              IF(WQ.EQ.'C27') VALUE1=FTFL01(27,INT(W1))
              IF(WQ.EQ.'C28') VALUE1=FTFL01(28,INT(W1))
              IF(WQ.EQ.'C29') VALUE1=FTFL01(29,INT(W1))
              IF(WQ.EQ.'C30') VALUE1=FTFL01(30,INT(W1))
              IF(WQ.EQ.'C31') VALUE1=FTFL01(31,INT(W1))
              IF(WQ.EQ.'C32') VALUE1=FTFL01(32,INT(W1))
              IF(WQ.EQ.'C33') VALUE1=FTFL01(33,INT(W1))
              IF(WQ.EQ.'C34') VALUE1=FTFL01(34,INT(W1))
              IF(WQ.EQ.'C35') VALUE1=FTFL01(35,INT(W1))
              IF(WQ.EQ.'C36') VALUE1=FTFL01(36,INT(W1))
              IF(WQ.EQ.'C37') VALUE1=FTFL01(37,INT(W1))
              IF(WQ.EQ.'C38') VALUE1=FTFL01(38,INT(W1))
              IF(WQ.EQ.'C39') VALUE1=FTFL01(39,INT(W1))
              IF(WQ.EQ.'C40') VALUE1=FTFL01(40,INT(W1))
              IF(WQ.EQ.'C41') VALUE1=FTFL01(41,INT(W1))
              IF(WQ.EQ.'C42') VALUE1=FTFL01(42,INT(W1))
              IF(WQ.EQ.'C43') VALUE1=FTFL01(43,INT(W1))
              IF(WQ.EQ.'C44') VALUE1=FTFL01(44,INT(W1))
              IF(WQ.EQ.'C45') VALUE1=FTFL01(45,INT(W1))
              IF(WQ.EQ.'C46') VALUE1=FTFL01(46,INT(W1))
              IF(WQ.EQ.'C47') VALUE1=FTFL01(47,INT(W1))
              IF(WQ.EQ.'C48') VALUE1=FTFL01(48,INT(W1))
              IF(WQ.EQ.'C49') VALUE1=FTFL01(49,INT(W1))
              IF(WQ.EQ.'C50') VALUE1=FTFL01(50,INT(W1))
              IF(WQ.EQ.'C51') VALUE1=FTFL01(51,INT(W1))
              IF(WQ.EQ.'C52') VALUE1=FTFL01(52,INT(W1))
              IF(WQ.EQ.'C53') VALUE1=FTFL01(53,INT(W1))
              IF(WQ.EQ.'C54') VALUE1=FTFL01(54,INT(W1))
              IF(WQ.EQ.'C55') VALUE1=FTFL01(55,INT(W1))
              IF(WQ.EQ.'C56') VALUE1=FTFL01(56,INT(W1))
              IF(WQ.EQ.'C57') VALUE1=FTFL01(57,INT(W1))
              IF(WQ.EQ.'C58') VALUE1=FTFL01(58,INT(W1))
              IF(WQ.EQ.'C59') VALUE1=FTFL01(59,INT(W1))
              IF(WQ.EQ.'C60') VALUE1=FTFL01(60,INT(W1))
              IF(WQ.EQ.'C61') VALUE1=FTFL01(61,INT(W1))
              IF(WQ.EQ.'C62') VALUE1=FTFL01(62,INT(W1))
              IF(WQ.EQ.'C63') VALUE1=FTFL01(63,INT(W1))
              IF(WQ.EQ.'C64') VALUE1=FTFL01(64,INT(W1))
              IF(WQ.EQ.'C65') VALUE1=FTFL01(65,INT(W1))
              IF(WQ.EQ.'C66') VALUE1=FTFL01(66,INT(W1))
              IF(WQ.EQ.'C67') VALUE1=FTFL01(67,INT(W1))
              IF(WQ.EQ.'C68') VALUE1=FTFL01(68,INT(W1))
              IF(WQ.EQ.'C69') VALUE1=FTFL01(69,INT(W1))
              IF(WQ.EQ.'C70') VALUE1=FTFL01(70,INT(W1))
              IF(WQ.EQ.'C71') VALUE1=FTFL01(71,INT(W1))
              IF(WQ.EQ.'C72') VALUE1=FTFL01(72,INT(W1))
              IF(WQ.EQ.'C73') VALUE1=FTFL01(73,INT(W1))
              IF(WQ.EQ.'C74') VALUE1=FTFL01(74,INT(W1))
              IF(WQ.EQ.'C75') VALUE1=FTFL01(75,INT(W1))
              IF(WQ.EQ.'C76') VALUE1=FTFL01(76,INT(W1))
              IF(WQ.EQ.'C77') VALUE1=FTFL01(77,INT(W1))
              IF(WQ.EQ.'C78') VALUE1=FTFL01(78,INT(W1))
              IF(WQ.EQ.'C79') VALUE1=FTFL01(79,INT(W1))
              IF(WQ.EQ.'C80') VALUE1=FTFL01(80,INT(W1))
              IF(WQ.EQ.'C81') VALUE1=FTFL01(81,INT(W1))
              IF(WQ.EQ.'C82') VALUE1=FTFL01(82,INT(W1))
              IF(WQ.EQ.'C83') VALUE1=FTFL01(83,INT(W1))
              IF(WQ.EQ.'C84') VALUE1=FTFL01(84,INT(W1))
              IF(WQ.EQ.'C85') VALUE1=FTFL01(85,INT(W1))
              IF(WQ.EQ.'C86') VALUE1=FTFL01(86,INT(W1))
              IF(WQ.EQ.'C87') VALUE1=FTFL01(87,INT(W1))
              IF(WQ.EQ.'C88') VALUE1=FTFL01(88,INT(W1))
              IF(WQ.EQ.'C89') VALUE1=FTFL01(89,INT(W1))
              IF(WQ.EQ.'C90') VALUE1=FTFL01(90,INT(W1))
              IF(WQ.EQ.'C91') VALUE1=FTFL01(91,INT(W1))
              IF(WQ.EQ.'C92') VALUE1=FTFL01(92,INT(W1))
              IF(WQ.EQ.'C93') VALUE1=FTFL01(93,INT(W1))
              IF(WQ.EQ.'C94') VALUE1=FTFL01(94,INT(W1))
              IF(WQ.EQ.'C95') VALUE1=FTFL01(95,INT(W1))
              IF(WQ.EQ.'C96') VALUE1=FTFL01(96,INT(W1))
              COEFJK=.FALSE.
              GO TO 200
          END IF
          IF(WQ.EQ.'FLDSX') THEN
              IF(DF1.EQ.0.AND.W1.LT.1.0D0.OR.
     1        DF1.EQ.0.AND.INT(W1).GT.CFLDCNT) THEN
                  WRITE(OUTLYNE,*)
     1            'INVALID FIELD OF VIEW POSITION NUMBER ENTERED WITH "FLDSX"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              VALUE1=CFLDS(1,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'FLDSY') THEN
              IF(DF1.EQ.0.AND.W1.LT.1.0D0.OR.
     1        DF1.EQ.0.AND.INT(W1).GT.CFLDCNT) THEN
                  WRITE(OUTLYNE,*)
     1            'INVALID FIELD OF VIEW POSITION NUMBER ENTERED WITH "FLDSY"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              VALUE1=CFLDS(2,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'SPTWT') THEN
              IF(DF1.EQ.0.AND.W1.LT.1.0D0.OR.DF1.EQ.0.AND.W1.GT.10.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'INVALID WAVELENGTH NUMBER ENTERED WITH "SPTWT"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'WAVELENGTH NUMBER (1 TO 10) REQUIRED WITH " GET SPTWT"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.EQ.1.0D0) VALUE1=SYSTEM1(31)
              IF(W1.EQ.2.0D0) VALUE1=SYSTEM1(32)
              IF(W1.EQ.3.0D0) VALUE1=SYSTEM1(33)
              IF(W1.EQ.4.0D0) VALUE1=SYSTEM1(34)
              IF(W1.EQ.5.0D0) VALUE1=SYSTEM1(35)
              IF(W1.EQ.6.0D0) VALUE1=SYSTEM1(76)
              IF(W1.EQ.7.0D0) VALUE1=SYSTEM1(77)
              IF(W1.EQ.8.0D0) VALUE1=SYSTEM1(78)
              IF(W1.EQ.9.0D0) VALUE1=SYSTEM1(79)
              IF(W1.EQ.10.0D0) VALUE1=SYSTEM1(80)
              GO TO 200
          END IF
          IF(WQ.EQ.'RMSOPD') THEN
              IF(.NOT.CPFNEXT) THEN
                  WRITE(OUTLYNE,*)
     1            'NO CAPFN EXISTS, "RMSOPD" IS NOT CURRENTLY GETABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0.AND.W1.LT.1.0D0.OR.DF1.EQ.0.AND.W1.GT.10.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'INVALID WAVELENGTH NUMBER ENTERED WITH "RMSOPD"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) VALUE1=RMSOPD
              IF(W1.EQ.1.0D0) VALUE1=RMSOP(1)
              IF(W1.EQ.2.0D0) VALUE1=RMSOP(2)
              IF(W1.EQ.3.0D0) VALUE1=RMSOP(3)
              IF(W1.EQ.4.0D0) VALUE1=RMSOP(4)
              IF(W1.EQ.5.0D0) VALUE1=RMSOP(5)
              IF(W1.EQ.6.0D0) VALUE1=RMSOP(6)
              IF(W1.EQ.7.0D0) VALUE1=RMSOP(7)
              IF(W1.EQ.8.0D0) VALUE1=RMSOP(8)
              IF(W1.EQ.9.0D0) VALUE1=RMSOP(9)
              IF(W1.EQ.10.0D0) VALUE1=RMSOP(10)
              GO TO 200
          END IF
          IF(WQ.EQ.'PTOVOPD') THEN
              IF(.NOT.CPFNEXT) THEN
                  WRITE(OUTLYNE,*)
     1            'NO CAPFN EXISTS, "PTOVOPD" IS NOT CURRENTLY GETABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.0.AND.W1.LT.1.0D0.OR.DF1.EQ.0.AND.W1.GT.10.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'INVALID WAVELENGTH NUMBER ENTERED WITH "PTOVOPD"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) VALUE1=PTOVOPD(0)
              IF(W1.EQ.1.0D0) VALUE1=PTOVOPD(1)
              IF(W1.EQ.2.0D0) VALUE1=PTOVOPD(2)
              IF(W1.EQ.3.0D0) VALUE1=PTOVOPD(3)
              IF(W1.EQ.4.0D0) VALUE1=PTOVOPD(4)
              IF(W1.EQ.5.0D0) VALUE1=PTOVOPD(5)
              IF(W1.EQ.6.0D0) VALUE1=PTOVOPD(6)
              IF(W1.EQ.7.0D0) VALUE1=PTOVOPD(7)
              IF(W1.EQ.8.0D0) VALUE1=PTOVOPD(8)
              IF(W1.EQ.9.0D0) VALUE1=PTOVOPD(9)
              IF(W1.EQ.10.0D0) VALUE1=PTOVOPD(10)
              GO TO 200
          END IF







          IF(WQ.EQ.'OPWT'.OR.WQ.EQ.'WT') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"OPWT" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(OPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'THERE ARE CURRENTLY NO OPERANDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(INT(W1).LT.0.OR.INT(W1).GT.OPCNT) THEN
                  WRITE(OUTLYNE,*)
     1            '"OPERAND NUMBER DESIGNATOR BEYOND CURRENT BOUNDARY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              VALUE1=OPERND(INT(W1),7)
              GO TO 200
          END IF
          IF(WQ.EQ.'TV') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"TV" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(OPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'THERE ARE CURRENTLY NO OPERANDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(INT(W1).LT.0.OR.INT(W1).GT.OPCNT) THEN
                  WRITE(OUTLYNE,*)
     1            '"OPERAND NUMBER DESIGNATOR BEYOND CURRENT BOUNDARY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              VALUE1=OPERND(INT(W1),4)
              GO TO 200
          END IF
          IF(WQ.EQ.'OPTYPE') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"OPTYPE" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(OPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'THERE ARE CURRENTLY NO OPERANDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(INT(W1).LT.0.OR.INT(W1).GT.OPCNT) THEN
                  WRITE(OUTLYNE,*)
     1            '"OPERAND NUMBER DESIGNATOR BEYOND CURRENT BOUNDARY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(OPERND(INT(W1),13).EQ.1.0D0) VALUE1=1.0D0
              IF(OPERND(INT(W1),13).EQ.0.0D0) VALUE1=2.0D0
              IF(OPERND(INT(W1),13).EQ.10.0D0) VALUE1=3.0D0
              IF(OPERND(INT(W1),13).EQ.-2.0D0) VALUE1=4.0D0
              IF(OPERND(INT(W1),13).EQ.2.0D0) VALUE1=5.0D0
              GO TO 200
          END IF
          IF(WQ.EQ.'VB') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"VB" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(VBCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'THERE ARE CURRENTLY NO VARIABLES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(INT(W1).LT.0.OR.INT(W1).GT.VBCNT) THEN
                  WRITE(OUTLYNE,*)
     1            '"VARIABLE NUMBER DESIGNATOR BEYOND CURRENT BOUNDARY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              VALUE1=VARABL(INT(W1),4)
              GO TO 200
          END IF
          IF(WQ.EQ.'OPRD') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"OPRD" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(OPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'THERE ARE CURRENTLY NO OPERANDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(INT(W1).LT.0.OR.INT(W1).GT.OPCNT) THEN
                  WRITE(OUTLYNE,*)
     1            '"OPERAND NUMBER DESIGNATOR BEYOND CURRENT BOUNDARY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              VALUE1=OPERND(INT(W1),4)
              GO TO 200
          END IF
          IF(WQ.EQ.'VBT') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"VBT" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(TVBCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'THERE ARE CURRENTLY NO TOLERANCE VARIABLES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(INT(W1).LT.0.OR.INT(W1).GT.TVBCNT) THEN
                  WRITE(OUTLYNE,*)
     1            '"TOL VARIABLE NUMBER DESIGNATOR BEYOND CURRENT BOUNDARY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              VALUE1=0.0D0
              OUTLYNE='"VBT" NOT YET GETABLE'
              CALL SHOWIT(1)
              GO TO 200
          END IF
          IF(WQ.EQ.'VARWT'.OR.WQ.EQ.'VWFC') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"VARWT" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(VBCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'THERE ARE CURRENTLY NO VARIABLES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(INT(W1).LT.0.OR.INT(W1).GT.VBCNT) THEN
                  WRITE(OUTLYNE,*)
     1            '"VARIABLE NUMBER DESIGNATOR BEYOND CURRENT BOUNDARY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              VALUE1=VARABL(INT(W1),7)
              GO TO 200
          END IF
          IF(WQ.EQ.'VARDNC') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"VARDNC" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(VBCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            'THERE ARE CURRENTLY NO VARIABLES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(INT(W1).LT.0.OR.INT(W1).GT.VBCNT) THEN
                  WRITE(OUTLYNE,*)
     1            '"VARIABLE NUMBER DESIGNATOR BEYOND CURRENT BOUNDARY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              VALUE1=VARABL(INT(W1),8)
              GO TO 200
          END IF
          IF(WQ.EQ.'XREF'.OR.WQ.EQ.'YREF'.OR.WQ.EQ.'ZREF'.OR.
     1    WQ.EQ.'LREF'.OR.WQ.EQ.'MREF'.OR.WQ.EQ.'NREF'.OR.WQ.EQ.
     2    'LENREF'.OR.WQ.EQ.'OPLREF'.OR.WQ.EQ.'IREF'.OR.WQ.EQ.'IPREF'
     3    .OR.WQ.EQ.'LNREF'.OR.WQ.EQ.'MNREF'.OR.WQ.EQ.'NNREF'.OR.WQ
     4    .EQ.'XAREF'.OR.WQ.EQ.'YAREF') THEN
C       CHECK IF REFERENCE RAY DATA EXISTS
              IF(.NOT.REFEXT) THEN
                  WRITE(OUTLYNE,*)'NO REFERENCE RAY DATA EXISTS.'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            WQ,' AT SURFACE ',INT(W1),' IS NOT GET-ABLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=DBLE(NEWIMG)
              IF(WQ.EQ.'XREF') VALUE1=REFRY(1,INT(W1))
              IF(WQ.EQ.'YREF') VALUE1=REFRY(2,INT(W1))
              IF(WQ.EQ.'ZREF') VALUE1=REFRY(3,INT(W1))
              IF(WQ.EQ.'LREF') VALUE1=REFRY(4,INT(W1))
              IF(WQ.EQ.'MREF') VALUE1=REFRY(5,INT(W1))
              IF(WQ.EQ.'NREF') VALUE1=REFRY(6,INT(W1))
              IF(WQ.EQ.'LREFOL') VALUE1=REFRY(19,INT(W1))
              IF(WQ.EQ.'MREFOL') VALUE1=REFRY(20,INT(W1))
              IF(WQ.EQ.'NREFOL') VALUE1=REFRY(21,INT(W1))
              IF(WQ.EQ.'LENREF') VALUE1=REFRY(8,INT(W1))
              IF(WQ.EQ.'OPLREF') VALUE1=REFRY(7,INT(W1))
              IF(WQ.EQ.'IREF') VALUE1=REFRY(9,INT(W1))
              IF(WQ.EQ.'IPREF') VALUE1=REFRY(10,INT(W1))
              IF(WQ.EQ.'XAREF') VALUE1=REFRY(11,INT(W1))
              IF(WQ.EQ.'XAREF') THEN
                  IF(REFRY(11,INT(W1)).GT.PII) THEN
                      VALUE1=REFRY(11,INT(W1))-(TWOPII)
                  ELSE
                      VALUE1=REFRY(11,INT(W1))
                  END IF
              END IF
              IF(WQ.EQ.'YAREF') THEN
                  IF(REFRY(12,INT(W1)).GT.PII) THEN
                      VALUE1=REFRY(12,INT(W1))-(TWOPII)
                  ELSE
                      VALUE1=REFRY(12,INT(W1))
                  END IF
              END IF
              IF(WQ.EQ.'LNREF') VALUE1=REFRY(13,INT(W1))
              IF(WQ.EQ.'MNREF') VALUE1=REFRY(14,INT(W1))
              IF(WQ.EQ.'NNREF') VALUE1=REFRY(15,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'X'.OR.WQ.EQ.'Y'.OR.WQ.EQ.'Z'.OR.WQ.EQ.'RENERGY'.OR.
     1    WQ.EQ.'DCL'.OR.WQ.EQ.'DCM'.OR.WQ.EQ.'DCN'.OR.WQ.EQ.
     2    'LEN'.OR.WQ.EQ.'OPL'.OR.WQ.EQ.'AII'.OR.WQ.EQ.'AIP'
     3    .OR.WQ.EQ.'LN'.OR.WQ.EQ.'MN'.OR.WQ.EQ.'NN'.OR.WQ
     4    .EQ.'XANG'.OR.WQ.EQ.'YANG'.OR.WQ.EQ.'DX'.OR.WQ.EQ.'NUMHITS'
     5    .OR.WQ.EQ.'DY'.OR.WQ.EQ.'DXA'.OR.WQ.EQ.'DYA'.OR.WQ.EQ.
     6    'DYDY'.OR.WQ.EQ.'DYDX'.OR.WQ.EQ.'DXDY'.OR.WQ.EQ.'DXDX'
     7    .OR.WQ.EQ.'DYADY'.OR.WQ.EQ.'DYADX'.OR.WQ.EQ.'DXADY'.OR.
     8    WQ.EQ.'DXADX'.OR.WQ.EQ.'LOLD'.OR.WQ.EQ.'MOLD'.OR.WQ.EQ.
     9    'NOLD'.OR.WQ.EQ.'DR'.OR.WQ.EQ.'DRA'.OR.WQ.EQ.'LREFOL'
     1    .OR.WQ.EQ.'NREFOL'.OR.WQ.EQ.'MREFOL'.OR.WQ.EQ.'K'.OR.WQ.EQ.'L'
     2    .OR.WQ.EQ.'M') THEN
              IF(DF1.EQ.1) W1=DBLE(NEWIMG)
              GO TO 231
          END IF
          IF(WQ.EQ.'GLX'.OR.WQ.EQ.'GLY'.OR.WQ.EQ.'GLZ'.OR.
     4    WQ.EQ.'GLL'.OR.WQ.EQ.'GLM'.OR.WQ.EQ.'GLN'.OR.
     4    WQ.EQ.'GLLOLD'.OR.WQ.EQ.'GLMOLD'.OR.WQ.EQ.'GLNOLD'.OR.
     4    WQ.EQ.'PYPY'.OR.WQ.EQ.'PYPX'.OR.WQ.EQ.'PXPX'.OR.
     4    WQ.EQ.'PXPY'.OR.WQ.EQ.'PXAPX'.OR.WQ.EQ.'PXAPY'.OR.
     4    WQ.EQ.'PYAPX'.OR.WQ.EQ.'PYAPY'.OR.
     1    WQ.EQ.'GLLN'.OR.WQ.EQ.'GLMN'.OR.WQ.EQ.'GLNN') THEN
              IF(DF1.EQ.1) W1=DBLE(NEWIMG)
              GO TO 231
          ELSE
              GO TO 232
          END IF
 231      CONTINUE
C       CHECK IF REFERENCE RAY DATA EXISTS
          IF(WQ.EQ.'GLX'.OR.WQ.EQ.'GLY'.OR.WQ.EQ.'GLZ'.OR.
     4    WQ.EQ.'GLL'.OR.WQ.EQ.'GLM'.OR.WQ.EQ.'GLN'.OR.
     4    WQ.EQ.'GLLOLD'.OR.WQ.EQ.'GLMOLD'.OR.WQ.EQ.'GLNOLD'.OR.
     4    WQ.EQ.'PYPY'.OR.WQ.EQ.'PYPX'.OR.WQ.EQ.'PXPX'.OR.
     4    WQ.EQ.'PXPY'.OR.WQ.EQ.'PXAPX'.OR.WQ.EQ.'PXAPY'.OR.
     4    WQ.EQ.'PYAPX'.OR.WQ.EQ.'PYAPY'.OR.
     1    WQ.EQ.'GLLN'.OR.WQ.EQ.'GLMN'.OR.WQ.EQ.'GLNN') THEN
              IF(.NOT.RAYEXT) THEN
                  WRITE(OUTLYNE,*)'NO RAY DATA EXISTS.'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            WQ,' AT SURFACE ',INT(W1),' IS NOT GET-ABLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
              WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'X') VALUE1=RAYRAY(1,INT(W1))
          IF(WQ.EQ.'Y') VALUE1=RAYRAY(2,INT(W1))
          IF(WQ.EQ.'Z') VALUE1=RAYRAY(3,INT(W1))
          IF(WQ.EQ.'DCL') VALUE1=RAYRAY(4,INT(W1))
          IF(WQ.EQ.'DCM') VALUE1=RAYRAY(5,INT(W1))
          IF(WQ.EQ.'DCN') VALUE1=RAYRAY(6,INT(W1))
          IF(WQ.EQ.'K') VALUE1=RAYRAY(4,INT(W1))
          IF(WQ.EQ.'L') VALUE1=RAYRAY(5,INT(W1))
          IF(WQ.EQ.'M') VALUE1=RAYRAY(6,INT(W1))
          IF(WQ.EQ.'LOLD') VALUE1=RAYRAY(19,INT(W1))
          IF(WQ.EQ.'MOLD') VALUE1=RAYRAY(20,INT(W1))
          IF(WQ.EQ.'NOLD') VALUE1=RAYRAY(21,INT(W1))
          IF(WQ.EQ.'LEN') VALUE1=RAYRAY(8,INT(W1))
          IF(WQ.EQ.'OPL') VALUE1=RAYRAY(7,INT(W1))
          IF(WQ.EQ.'AII') VALUE1=RAYRAY(9,INT(W1))
          IF(WQ.EQ.'AIP') VALUE1=RAYRAY(10,INT(W1))
          IF(WQ.EQ.'RENERGY') VALUE1=RAYRAY(25,INT(W1))
          IF(WQ.EQ.'NUMHITS') THEN
              IF(.NOT.ALLOCATED(MULTIRAY_DATA)) THEN
                  VALUE1=1.0D0
              ELSE
                  VALUE1=NUMHITS(INT(W1))
              END IF
          END IF
          IF(WQ.EQ.'XANG') THEN
              IF(RAYRAY(11,INT(W1)).GT.PII) THEN
                  VALUE1=RAYRAY(11,INT(W1))-(TWOPII)
              END IF
              IF(RAYRAY(11,INT(W1)).LT.-PII) THEN
                  VALUE1=RAYRAY(11,INT(W1))+(TWOPII)
              END IF
              VALUE1=RAYRAY(11,INT(W1))
          END IF
          IF(WQ.EQ.'YANG') THEN
              IF(RAYRAY(12,INT(W1)).GT.PII) THEN
                  VALUE1=RAYRAY(12,INT(W1))-(TWOPII)
              END IF
              IF(RAYRAY(12,INT(W1)).LT.-PII) THEN
                  VALUE1=RAYRAY(12,INT(W1))+(TWOPII)
              END IF
              VALUE1=RAYRAY(12,INT(W1))
          END IF
          IF(WQ.EQ.'LN') VALUE1=RAYRAY(13,INT(W1))
          IF(WQ.EQ.'MN') VALUE1=RAYRAY(14,INT(W1))
          IF(WQ.EQ.'NN') VALUE1=RAYRAY(15,INT(W1))
          IF(WQ.EQ.'GLX') VALUE1=GLRAY(1,INT(W1))
          IF(WQ.EQ.'GLY') VALUE1=GLRAY(2,INT(W1))
          IF(WQ.EQ.'GLZ') VALUE1=GLRAY(3,INT(W1))
          IF(WQ.EQ.'GLL') VALUE1=GLRAY(4,INT(W1))
          IF(WQ.EQ.'GLM') VALUE1=GLRAY(5,INT(W1))
          IF(WQ.EQ.'GLN') VALUE1=GLRAY(6,INT(W1))
          IF(WQ.EQ.'GLLOLD') VALUE1=GLRAY(7,INT(W1))
          IF(WQ.EQ.'GLMOLD') VALUE1=GLRAY(8,INT(W1))
          IF(WQ.EQ.'GLNOLD') VALUE1=GLRAY(9,INT(W1))
          IF(WQ.EQ.'GLLN') VALUE1=GLRAY(10,INT(W1))
          IF(WQ.EQ.'GLMN') VALUE1=GLRAY(11,INT(W1))
          IF(WQ.EQ.'GLNN') VALUE1=GLRAY(12,INT(W1))
C       CALCULATE DX AND DY AND THEIR SLOPES
          IF(WQ.EQ.'DR') THEN
              VALUE1=(((RAYRAY(1,INT(W1))-REFRY(1,INT(W1)))/JB)**2)
              VALUE1=VALUE1+
     1        (((RAYRAY(2,INT(W1))-REFRY(2,INT(W1)))/JA)**2)
              VALUE1=DSQRT(VALUE1)
          END IF
          IF(WQ.EQ.'DRA') THEN
              COSARG=((RAYRAY(4,INT(W1))*REFRY(4,INT(W1)))+
     1        (RAYRAY(5,INT(W1))*REFRY(5,INT(W1)))+
     1        (RAYRAY(6,INT(W1))*REFRY(6,INT(W1))))
              IF(COSARG.LT.0.0D0) COSARG=-COSARG
              IF(COSARG.GT.1.0D0) COSARG=1.0D0
              VALUE1=DACOS(COSARG)
              IF(REAL(VALUE1).EQ.REAL(TWOPII)) VALUE1=0.0D0
              IF(REAL(VALUE1).GT.REAL(PII)) VALUE1=VALUE1-(TWOPII)
          END IF
          IF(WQ.EQ.'DX') THEN
              VALUE1=(RAYRAY(1,INT(W1))-REFRY(1,INT(W1)))/JB
          END IF
          IF(.NOT.LDIF) THEN
              IF(WQ.EQ.'DXDX'.OR.WQ.EQ.'DXDY'.OR.WQ.EQ.'DYDX'.OR.WQ.EQ.
     1        'DYDY'.OR.WQ.EQ.'DYADY'.OR.WQ.EQ.'DYADX'.OR.WQ.EQ.'DXADY'
     2        .OR.WQ.EQ.'DXADX'.OR.WQ.EQ.'PYPX'.OR.WQ.EQ.'PYPY'.OR.WQ.EQ.
     3        'PXPY'.OR.WQ.EQ.'PXPX'.OR.WQ.EQ.'PYAPY'.OR.WQ.EQ.'PYAPX'
     4        .OR.WQ.EQ.'PXAPY'.OR.WQ.EQ.'PXAPX') THEN
                  WRITE(OUTLYNE,*)
     1            'DIFFERENTIAL RAYTRACE IS "OFF"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'ALL RAY DERIVATIVES ARE ALL ZERO'
                  CALL SHOWIT(1)
                  VALUE1=0.0D0
                  GO TO 200
              END IF
          END IF
          IF(WQ.EQ.'DXDX') THEN
              VALUE1=(DIFF(1,INT(W1))-RAYRAY(1,INT(W1)))/(DELX*JB)
          END IF
          IF(WQ.EQ.'DXDY') THEN
              VALUE1=(DIFF(7,INT(W1))-RAYRAY(1,INT(W1)))/(DELY*JB)
          END IF
          IF(WQ.EQ.'DY') THEN
              VALUE1=(RAYRAY(2,INT(W1))-REFRY(2,INT(W1)))/JA
          END IF
          IF(WQ.EQ.'DYDY') THEN
              VALUE1=(DIFF(8,INT(W1))-RAYRAY(2,INT(W1)))/(DELY*JA)
          END IF
          IF(WQ.EQ.'DYDX') THEN
              VALUE1=(DIFF(2,INT(W1))-RAYRAY(2,INT(W1)))/(DELX*JA)
          END IF
C
C       CALCULATE DXA AND DYA AND THEIR SLOPES
          IF(WQ.EQ.'DXA') THEN
              VALUE1=RAYRAY(11,INT(W1))
     1        -REFRY(11,INT(W1))
              IF(REAL(VALUE1).GT.REAL(PII)) VALUE1=VALUE1-(TWOPII)
              IF(REAL(VALUE1).EQ.REAL(TWOPII)) VALUE1=0.0D0
          END IF
          IF(WQ.EQ.'DXADX') THEN
              IF(DABS(DIFF(4,INT(W1))).GE.
     1        (1.0D35*DABS(DIFF(6,INT(W1))))) THEN
                  IF(REAL(DIFF(4,INT(W1))).GE.0.0) V1=PII/2.0D0
                  IF(REAL(DIFF(4,INT(W1))).LT.0.0) V1=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(DIFF(4,INT(W1))).LE.1.0D-15.AND.
     1            DABS(DIFF(6,INT(W1))).LE.1.0D-15) THEN
                      V1=0.0D0
                  ELSE
                      V1=DATAN2(DIFF(4,INT(W1)),DIFF(6,INT(W1)))
                  END IF
                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
              END IF
              VALUE1=(V1-RAYRAY(11,INT(W1)))
              IF(REAL(VALUE1).GT.REAL(PII)) VALUE1=VALUE1-(TWOPII)
              IF(REAL(VALUE1).EQ.REAL(TWOPII)) VALUE1=0.0D0
              VALUE1=VALUE1/DELX
          END IF
          IF(WQ.EQ.'DXADY') THEN
              IF(DABS(DIFF(10,INT(W1))).GE.
     1        (1.0D35*DABS(DIFF(12,INT(W1))))) THEN
                  IF(REAL(DIFF(10,INT(W1))).GE.0.0) V1=PII/2.0D0
                  IF(REAL(DIFF(10,INT(W1))).LT.0.0) V1=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(DIFF(10,INT(W1))).LE.1.0D-15.AND.
     1            DABS(DIFF(12,INT(W1))).LE.1.0D-15) THEN
                      V1=0.0D0
                  ELSE
                      V1=DATAN2(DIFF(10,INT(W1)),DIFF(12,INT(W1)))
                  END IF
                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
              END IF
              VALUE1=(V1-RAYRAY(11,INT(W1)))
              IF(REAL(VALUE1).GT.REAL(PII)) VALUE1=VALUE1-(TWOPII)
              IF(REAL(VALUE1).EQ.REAL(TWOPII)) VALUE1=0.0D0
              VALUE1=VALUE1/DELY
          END IF
          IF(WQ.EQ.'DYA') THEN
              VALUE1=RAYRAY(12,INT(W1))
     1        -REFRY(12,INT(W1))
              IF(REAL(VALUE1).EQ.REAL(TWOPII)) VALUE1=0.0D0
              IF(REAL(VALUE1).GT.REAL(PII)) VALUE1=VALUE1-(TWOPII)
          END IF
          IF(WQ.EQ.'DYADX') THEN
              IF(DABS(DIFF(5,INT(W1))).GE.
     1        (1.0D35*DABS(DIFF(6,INT(W1))))) THEN
                  IF(REAL(DIFF(5,INT(W1))).GE.0.0) V1=PII/2.0D0
                  IF(REAL(DIFF(5,INT(W1))).LT.0.0) V1=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(DIFF(5,INT(W1))).LE.1.0D-15.AND.
     1            DABS(DIFF(6,INT(W1))).LE.1.0D-15) THEN
                      V1=0.0D0
                  ELSE
                      V1=DATAN2(DIFF(5,INT(W1)),DIFF(6,INT(W1)))
                  END IF
                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
              END IF
              VALUE1=(V1-RAYRAY(12,INT(W1)))
              IF(REAL(VALUE1).GT.REAL(PII)) VALUE1=VALUE1-(TWOPII)
              IF(REAL(VALUE1).EQ.REAL(TWOPII)) VALUE1=0.0D0
              VALUE1=VALUE1/DELX
          END IF
          IF(WQ.EQ.'DYADY') THEN
              IF(DABS(DIFF(11,INT(W1))).GE.
     1        (1.0D35*DABS(DIFF(12,INT(W1))))) THEN
                  IF(REAL(DIFF(11,INT(W1))).GE.0.0) V1=PII/2.0D0
                  IF(REAL(DIFF(11,INT(W1))).LT.0.0) V1=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(DIFF(11,INT(W1))).LE.1.0D-15.AND.
     1            DABS(DIFF(12,INT(W1))).LE.1.0D-15) THEN
                      V1=0.0D0
                  ELSE
                      V1=DATAN2(DIFF(11,INT(W1)),DIFF(12,INT(W1)))
                  END IF
                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
              END IF
              VALUE1=(V1-RAYRAY(12,INT(W1)))
              IF(REAL(VALUE1).GT.REAL(PII)) VALUE1=VALUE1-(TWOPII)
              IF(REAL(VALUE1).EQ.(TWOPII)) VALUE1=0.0D0
              VALUE1=VALUE1/DELY
          END IF
          IF(WQ.EQ.'PYPY') THEN
              VALUE1=(RFDIFF(8,INT(W1))-REFRY(2,INT(W1)))/(RFDELY*JA)
          END IF
          IF(WQ.EQ.'PYPX') THEN
              VALUE1=(RFDIFF(2,INT(W1))-REFRY(2,INT(W1)))/(RFDELX*JA)
          END IF
          IF(WQ.EQ.'PXPX') THEN
              VALUE1=(RFDIFF(1,INT(W1))-REFRY(1,INT(W1)))/(RFDELX*JB)
          END IF
          IF(WQ.EQ.'PXPY') THEN
              VALUE1=(RFDIFF(7,INT(W1))-REFRY(1,INT(W1)))/(RFDELY*JB)
          END IF
          IF(WQ.EQ.'PYAPX') THEN
              IF(DABS(RFDIFF(5,INT(W1))).GE.
     1        (1.0D35*DABS(RFDIFF(6,INT(W1))))) THEN
                  IF(REAL(RFDIFF(5,INT(W1))).GE.0.0) V1=PII/2.0D0
                  IF(REAL(RFDIFF(5,INT(W1))).LT.0.0) V1=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(RFDIFF(5,INT(W1))).LE.1.0D-15.AND.
     1            DABS(RFDIFF(6,INT(W1))).LE.1.0D-15) THEN
                      V1=0.0D0
                  ELSE
                      V1=DATAN2(RFDIFF(5,INT(W1)),RFDIFF(6,INT(W1)))
                  END IF
                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
              END IF
              VALUE1=(V1-REFRY(12,INT(W1)))
              IF(REAL(VALUE1).GT.REAL(PII)) VALUE1=VALUE1-(TWOPII)
              IF(REAL(VALUE1).EQ.REAL(TWOPII)) VALUE1=0.0D0
              VALUE1=VALUE1/RFDELX
          END IF
          IF(WQ.EQ.'PYAPY') THEN
              IF(DABS(RFDIFF(11,INT(W1))).GE.
     1        (1.0D35*DABS(RFDIFF(12,INT(W1))))) THEN
                  IF(REAL(RFDIFF(11,INT(W1))).GE.0.0) V1=PII/2.0D0
                  IF(REAL(RFDIFF(11,INT(W1))).LT.0.0) V1=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(RFDIFF(11,INT(W1))).LE.1.0D-15.AND.
     1            DABS(RFDIFF(12,INT(W1))).LE.1.0D-15) THEN
                      V1=0.0D0
                  ELSE
                      V1=DATAN2(RFDIFF(11,INT(W1)),RFDIFF(12,INT(W1)))
                  END IF
                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
              END IF
              VALUE1=(V1-REFRY(12,INT(W1)))
              IF(REAL(VALUE1).GT.REAL(PII)) VALUE1=VALUE1-(TWOPII)
              IF(REAL(VALUE1).EQ.REAL(TWOPII)) VALUE1=0.0D0
              VALUE1=VALUE1/RFDELY
          END IF
          IF(WQ.EQ.'PXAPX') THEN
              IF(DABS(RFDIFF(4,INT(W1))).GE.
     1        (1.0D35*DABS(RFDIFF(6,INT(W1))))) THEN
                  IF(REAL(RFDIFF(4,INT(W1))).GE.0.0) V1=PII/2.0D0
                  IF(REAL(RFDIFF(4,INT(W1))).LT.0.0) V1=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(RFDIFF(4,INT(W1))).LE.1.0D-15.AND.
     1            DABS(RFDIFF(6,INT(W1))).LE.1.0D-15) THEN
                      V1=0.0D0
                  ELSE
                      V1=DATAN2(RFDIFF(4,INT(W1)),RFDIFF(6,INT(W1)))
                  END IF
                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
              END IF
              VALUE1=(V1-REFRY(11,INT(W1)))
              IF(REAL(VALUE1).GT.REAL(PII)) VALUE1=VALUE1-(TWOPII)
              IF(REAL(VALUE1).EQ.REAL(TWOPII)) VALUE1=0.0D0
              VALUE1=VALUE1/RFDELX
          END IF
          IF(WQ.EQ.'PXAPY') THEN
              IF(DABS(RFDIFF(10,INT(W1))).GE.
     1        (1.0D35*DABS(RFDIFF(12,INT(W1))))) THEN
                  IF(REAL(RFDIFF(10,INT(W1))).GE.0.0) V1=PII/2.0D0
                  IF(REAL(RFDIFF(10,INT(W1))).LT.0.0) V1=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(RFDIFF(10,INT(W1))).LE.1.0D-15.AND.
     1            DABS(RFDIFF(12,INT(W1))).LE.1.0D-15) THEN
                      V1=0.0D0
                  ELSE
                      V1=DATAN2(RFDIFF(10,INT(W1)),RFDIFF(12,INT(W1)))
                  END IF
                  IF(REAL(V1).LT.0.0) V1=V1+(TWOPII)
              END IF
              VALUE1=(V1-REFRY(11,INT(W1)))
              IF(REAL(VALUE1).GT.REAL(PII)) VALUE1=VALUE1-(TWOPII)
              IF(REAL(VALUE1).EQ.REAL(TWOPII)) VALUE1=0.0D0
              VALUE1=VALUE1/RFDELY
          END IF
          GO TO 200
 232      CONTINUE
          IF(WQ.EQ.'ZERN37') THEN
              IF(.NOT.OPMAP) THEN
                  WRITE(OUTLYNE,*)
     1            'NO FITTED WAVEFRONT MAP EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO-ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              ZN=.FALSE.
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ZERN37" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.EQ.1.0D0) ZN=.TRUE.
              IF(W1.EQ.2.0D0) ZN=.TRUE.
              IF(W1.EQ.3.0D0) ZN=.TRUE.
              IF(W1.EQ.4.0D0) ZN=.TRUE.
              IF(W1.EQ.5.0D0) ZN=.TRUE.
              IF(W1.EQ.6.0D0) ZN=.TRUE.
              IF(W1.EQ.7.0D0) ZN=.TRUE.
              IF(W1.EQ.8.0D0) ZN=.TRUE.
              IF(W1.EQ.9.0D0) ZN=.TRUE.
              IF(W1.EQ.10.0D0) ZN=.TRUE.
              IF(W1.EQ.11.0D0) ZN=.TRUE.
              IF(W1.EQ.12.0D0) ZN=.TRUE.
              IF(W1.EQ.13.0D0) ZN=.TRUE.
              IF(W1.EQ.14.0D0) ZN=.TRUE.
              IF(W1.EQ.15.0D0) ZN=.TRUE.
              IF(W1.EQ.16.0D0) ZN=.TRUE.
              IF(W1.EQ.17.0D0) ZN=.TRUE.
              IF(W1.EQ.18.0D0) ZN=.TRUE.
              IF(W1.EQ.19.0D0) ZN=.TRUE.
              IF(W1.EQ.20.0D0) ZN=.TRUE.
              IF(W1.EQ.21.0D0) ZN=.TRUE.
              IF(W1.EQ.22.0D0) ZN=.TRUE.
              IF(W1.EQ.23.0D0) ZN=.TRUE.
              IF(W1.EQ.24.0D0) ZN=.TRUE.
              IF(W1.EQ.25.0D0) ZN=.TRUE.
              IF(W1.EQ.26.0D0) ZN=.TRUE.
              IF(W1.EQ.27.0D0) ZN=.TRUE.
              IF(W1.EQ.28.0D0) ZN=.TRUE.
              IF(W1.EQ.29.0D0) ZN=.TRUE.
              IF(W1.EQ.30.0D0) ZN=.TRUE.
              IF(W1.EQ.31.0D0) ZN=.TRUE.
              IF(W1.EQ.32.0D0) ZN=.TRUE.
              IF(W1.EQ.33.0D0) ZN=.TRUE.
              IF(W1.EQ.34.0D0) ZN=.TRUE.
              IF(W1.EQ.35.0D0) ZN=.TRUE.
              IF(W1.EQ.36.0D0) ZN=.TRUE.
              IF(W1.EQ.37.0D0) ZN=.TRUE.
              IF(.NOT.ZN) THEN
                  WRITE(OUTLYNE,*)'COEFFICIENT NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              VALUE1=X(INT(W1))
              GO TO 200
          END IF
C
C     SQUEEZE IN GAUSSIAN BEAM PARAMETERS OR WAIST POSITION
C     AND SEMI-DIAMETER
          IF(WQ.EQ.'GBDISX') THEN
              OLDREF=NEWREF
              OLDOBJ=NEWOBJ
              OLDIMG=NEWIMG
              NEWIMG=INT(SYSTEM1(20))
              NEWOBJ=0
              NEWREF=1
C     CALCULATE THE GPX,GPUX,GPCX AND GPUCX
              ERROR=0
              IF(REFEXT) THEN
                  NWN1=LFOB(1)
                  NWN2=LFOB(2)
                  NWN3=LFOB(3)
                  NWN4=LFOB(4)
              ELSE
                  NWN1=0.0D0
                  NWN2=0.0D0
                  NWN3=0.0D0
                  NWN4=SYSTEM1(11)
              END IF
              CALL GNPR2(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPX=VALUE1
              CALL GNPR4(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPUX=VALUE1
              CALL GNPR6(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPCX=VALUE1
              CALL GNPR8(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPUCX=VALUE1
              IF(REAL((JPUX**2)+(JPUCX**2)).EQ.0.0) THEN
                  VALUE1=1.0D20
              ELSE
                  VALUE1=-((JPX*JPUX)+(JPCX*JPUCX))/((JPUX**2)+(JPUCX**2))
              END IF
              NEWOBJ=OLDOBJ
              NEWREF=OLDREF
              NEWIMG=OLDIMG
              GO TO 200
          END IF
          IF(WQ.EQ.'GBDISY') THEN
              OLDREF=NEWREF
              OLDOBJ=NEWOBJ
              OLDIMG=NEWIMG
              NEWIMG=INT(SYSTEM1(20))
              NEWOBJ=0
              NEWREF=1
C     CALCULATE THE GPY,GPUY,GPCY AND GPUCY
              ERROR=0
              IF(REFEXT) THEN
                  NWN1=LFOB(1)
                  NWN2=LFOB(2)
                  NWN3=LFOB(3)
                  NWN4=LFOB(4)
              ELSE
                  NWN1=0.0D0
                  NWN2=0.0D0
                  NWN3=0.0D0
                  NWN4=SYSTEM1(11)
              END IF
              CALL GNPR1(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPY=VALUE1
              CALL GNPR3(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPUY=VALUE1
              CALL GNPR5(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPCY=VALUE1
              CALL GNPR7(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPUCY=VALUE1
              IF(REAL((JPUY**2)+(JPUCY**2)).EQ.0.0) THEN
                  VALUE1=1.0D20
              ELSE
                  VALUE1=-((JPY*JPUY)+(JPCY*JPUCY))/((JPUY**2)+(JPUCY**2))
              END IF
              NEWOBJ=OLDOBJ
              NEWREF=OLDREF
              NEWIMG=OLDIMG
              GO TO 200
          END IF
          IF(WQ.EQ.'GBWAISTX') THEN
              OLDREF=NEWREF
              OLDOBJ=NEWOBJ
              OLDIMG=NEWIMG
              NEWIMG=INT(SYSTEM1(20))
              NEWOBJ=0
              NEWREF=1
C     CALCULATE THE GPX,GPUX,GPCX AND GPUCX
              ERROR=0
              IF(REFEXT) THEN
                  NWN1=LFOB(1)
                  NWN2=LFOB(2)
                  NWN3=LFOB(3)
                  NWN4=LFOB(4)
              ELSE
                  NWN1=0.0D0
                  NWN2=0.0D0
                  NWN3=0.0D0
                  NWN4=SYSTEM1(11)
              END IF
              CALL GNPR2(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPX=VALUE1
              CALL GNPR4(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPUX=VALUE1
              CALL GNPR6(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPCX=VALUE1
              CALL GNPR8(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPUCX=VALUE1
C     DISTANCE TO WAIST IS JUST:
              IF(REAL((JPUX**2)+(JPUCX**2)).EQ.0.0) THEN
                  V1=1.0D20
              ELSE
                  V1=-((JPX*JPUX)+(JPCX*JPUCX))/((JPUX**2)+(JPUCX**2))
              END IF
C     SEMI-DIAMETER AT V1 IS JUST:
              VALUE1=DSQRT(((JPX+(JPUX*V1))**2)+((JPCX+(JPUCX*V1))**2))
              NEWOBJ=OLDOBJ
              NEWREF=OLDREF
              NEWIMG=OLDIMG
              GO TO 200
          END IF
          IF(WQ.EQ.'GBWAISTY') THEN
              OLDREF=NEWREF
              OLDOBJ=NEWOBJ
              OLDIMG=NEWIMG
              NEWIMG=INT(SYSTEM1(20))
              NEWOBJ=0
              NEWREF=1
C     CALCULATE THE GPY,GPUY,GPCY AND GPUCY
              ERROR=0
              IF(REFEXT) THEN
                  NWN1=LFOB(1)
                  NWN2=LFOB(2)
                  NWN3=LFOB(3)
                  NWN4=LFOB(4)
              ELSE
                  NWN1=0.0D0
                  NWN2=0.0D0
                  NWN3=0.0D0
                  NWN4=SYSTEM1(11)
              END IF
              CALL GNPR1(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPY=VALUE1
              CALL GNPR3(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPUY=VALUE1
              CALL GNPR5(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPCY=VALUE1
              CALL GNPR7(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
              JPUCY=VALUE1
C     DISTANCE TO WAIST IS JUST:
              IF(REAL((JPUY**2)+(JPUCY**2)).EQ.0.0) THEN
                  V1=1.0D20
              ELSE
                  V1=-((JPY*JPUY)+(JPCY*JPUCY))/((JPUY**2)+(JPUCY**2))
              END IF
C     SEMI-DIAMETER AT V1 IS JUST:
              VALUE1=DSQRT(((JPY+(JPUY*V1))**2)+((JPCY+(JPUCY*V1))**2))
              NEWOBJ=OLDOBJ
              NEWREF=OLDREF
              NEWIMG=OLDIMG
              GO TO 200
          END IF
C
          IF(WQ.EQ.'CV') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(1,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'ET'.OR.WQ.EQ.'ETY') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.(SYSTEM1(20)-1.0D0)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ET" AND "GET ETY" REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     CALCULATE EDGTHK (ET) VIA THE EDGTHK FUNCTION
              VALUE1=EDGTHK(INT(W1),1)
              GO TO 200
          END IF
          IF(WQ.EQ.'ETX') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.(SYSTEM1(20)-1.0D0)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ETX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     CALCULATE EDGTHK (ETX) VIA THE EDGTHK FUNCTION
              VALUE1=EDGTHK(INT(W1),2)
              GO TO 200
          END IF
          IF(WQ.EQ.'CVTOR') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(24,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'TH') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(3,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'THM') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(110,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'PRICE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(111,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GRS') THEN
              IF(ALENS(96,INT(W1)).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE IS NOT A LINEAR GRATING'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(98,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GRO') THEN
              IF(ALENS(96,INT(W1)).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE IS NOT A LINEAR GRATING'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(97,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GRX') THEN
              IF(ALENS(96,INT(W1)).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE IS NOT A LINEAR GRATING'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(99,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GRY') THEN
              IF(ALENS(96,INT(W1)).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE IS NOT A LINEAR GRATING'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(100,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GRZ') THEN
              IF(ALENS(96,INT(W1)).EQ.0.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE IS NOT A LINEAR GRATING'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(101,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'INR') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(76,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AC') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(43,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AD') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(4,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(5,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AF') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(6,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AG') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(7,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AH') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(81,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AI') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(82,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AJ') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(83,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AK') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(84,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AL') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(85,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'CC') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(2,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'ALPHA') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(118,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'BETA') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(119,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GAMMA') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(120,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'MCODE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       CALCULATE MCODE
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(INT(W1).EQ.0) THEN
C       AT OBJECT.
                  VALUE1=2.0D0
                  IF(GLANAM(INT(W1),2).EQ.'AIR') VALUE1=0.0D0
                  IF(GLANAM(INT(W1),1).EQ.'GLASS') VALUE1=3.0D0
                  IF(GLANAM(INT(W1),1).EQ.'MODEL') VALUE1=4.0D0
                  IF(GLANAM(INT(W1),2).EQ.'REFL') VALUE1=1.0D0
                  IF(GLANAM(INT(W1),2).EQ.'REFLTIRO') VALUE1=7.0D0
                  IF(GLANAM(INT(W1),2).EQ.'REFLTIR') VALUE1=8.0D0
                  IF(GLANAM(INT(W1),2).EQ.'PERFECT') VALUE1=5.0D0
                  IF(GLANAM(INT(W1),2).EQ.'IDEAL') VALUE1=6.0D0
              ELSE
C       NOT OBJECT SURFACE
                  VALUE1=2.0D0
                  IF(GLANAM(INT(W1),2).EQ.'AIR'.AND.
     1            GLANAM((INT(W1)-1),2).EQ.'AIR') VALUE1=0.0D0
                  IF(GLANAM(INT(W1),1).EQ.'GLASS') VALUE1=3.0D0
                  IF(GLANAM(INT(W1),1).EQ.'MODEL') VALUE1=4.0D0
                  IF(GLANAM(INT(W1),2).EQ.'REFL') VALUE1=1.0D0
                  IF(GLANAM(INT(W1),2).EQ.'REFLTIRO') VALUE1=7.0D0
                  IF(GLANAM(INT(W1),2).EQ.'REFLTIR') VALUE1=8.0D0
                  IF(GLANAM(INT(W1),2).EQ.'PERFECT') VALUE1=5.0D0
                  IF(GLANAM(INT(W1),2).EQ.'IDEAL') VALUE1=6.0D0
              END IF
C
              GO TO 200
          END IF
          IF(WQ.EQ.'TCODE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(DF1.EQ.1) W1=SYSTEM1(20)
C       CALCULATE TCODE
              VALUE1=0.0D0
              VALUE1=ALENS(25,INT(W1))
C
              GO TO 200
          END IF
          IF(WQ.EQ.'CCTOR') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(41,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'ADTOR') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(37,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AETOR') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(38,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AFTOR') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(39,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'AGTOR') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(40,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'XD') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(114,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'ZD') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(116,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'YD') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(115,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GDX') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(90,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GDY') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(91,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GDZ') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(92,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GALPHA') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(93,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GBETA') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(94,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'GGAMMA') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(95,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'RD') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(DABS(ALENS(1,INT(W1))).EQ.0.0D0) THEN
                  VALUE1=1.0D300
              ELSE
                  VALUE1=1.0D0/ALENS(1,INT(W1))
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'MINREG') THEN
              IF(DF1.EQ.1) W1=1.0D0
              IF(INT(W1).LT.1.OR.INT(W1).GT.100) THEN
                  WRITE(OUTLYNE,*)'MINREG NUMBER BEYOND LEGAL RANGE (1 TO 100)'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              VALUE1=MIN_REG(INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'MAXREG') THEN
              IF(DF1.EQ.1) W1=1.0D0
              IF(INT(W1).LT.1.OR.INT(W1).GT.100) THEN
                  WRITE(OUTLYNE,*)'MAXREG NUMBER BEYOND LEGAL RANGE (1 TO 100)'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              VALUE1=MAX_REG(INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'SHAPEFAC') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.(SYSTEM1(20)-1.0D0)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"SHAPEFAC" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DABS(ALENS(1,INT(W1))).LT.1D-30) THEN
                  IF(ALENS(1,INT(W1)).GT.0.0D0) TEMPR1=1.0D30
                  IF(ALENS(1,INT(W1)).LT.0.0D0) TEMPR1=-1.0D30
              ELSE
                  TEMPR1=1.0D0/ALENS(1,INT(W1))
              END IF
              IF(DABS(ALENS(1,INT(W1)+1)).LT.1D-30) THEN
                  IF(ALENS(1,INT(W1)+1).GT.0.0D0) TEMPR2=1.0D30
                  IF(ALENS(1,INT(W1)+1).LT.0.0D0) TEMPR2=-1.0D30
              ELSE
                  TEMPR2=1.0D0/ALENS(1,INT(W1)+1)
              END IF
              TEMPSUM=TEMPR2+TEMPR1
              TEMPDIF=TEMPR2-TEMPR1
              IF(DABS(TEMPDIF).LT.1.0D-30) THEN
                  IF(TEMPSUM.EQ.0.0D0) VALUE1=0.0D0
                  IF(TEMPSUM.GT.0.0D0.AND.TEMPDIF.GT.0.0D0) VALUE1=1.0D30
                  IF(TEMPSUM.LT.0.0D0.AND.TEMPDIF.LT.0.0D0) VALUE1=1.0D30
                  IF(TEMPSUM.GT.0.0D0.AND.TEMPDIF.LT.0.0D0) VALUE1=-1.0D30
                  IF(TEMPSUM.LT.0.0D0.AND.TEMPDIF.GT.0.0D0) VALUE1=-1.0D30
              ELSE
                  VALUE1=(TEMPR2+TEMPR1)/(TEMPR2-TEMPR1)
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'RDTOR') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(DABS(ALENS(24,INT(W1))).EQ.0.0D0) THEN
                  VALUE1=1.0D300
              ELSE
                  VALUE1=1.0D0/ALENS(24,INT(W1))
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'COATING') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              VALUE1=ALENS(112,INT(W1))
              GO TO 200
          END IF
C       ABBE AND WV AND PARTL
          IF(WQ.EQ.'WV') THEN
              IF(W1.LT.1.0.OR.W1.GT.10.0D0) THEN
                  WRITE(OUTLYNE,*)'WAVELENGTH NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(11)
              IF(INT(W1).EQ.1) VALUE1=SYSTEM1(1)
              IF(INT(W1).EQ.2) VALUE1=SYSTEM1(2)
              IF(INT(W1).EQ.3) VALUE1=SYSTEM1(3)
              IF(INT(W1).EQ.4) VALUE1=SYSTEM1(4)
              IF(INT(W1).EQ.5) VALUE1=SYSTEM1(5)
              IF(INT(W1).EQ.6) VALUE1=SYSTEM1(71)
              IF(INT(W1).EQ.7) VALUE1=SYSTEM1(72)
              IF(INT(W1).EQ.8) VALUE1=SYSTEM1(73)
              IF(INT(W1).EQ.9) VALUE1=SYSTEM1(74)
              IF(INT(W1).EQ.10) VALUE1=SYSTEM1(75)
              GO TO 200
          END IF
          IF(WQ.EQ.'ABBE'.OR.WQ.EQ.'PARTL'.OR.WQ.EQ.'INDEX'.OR.
     1    WQ.EQ.'VNUM'.OR.WQ.EQ.'DPART') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              NF=INT(W1)
              CALL SINDEX
              IF(WQ.EQ.'ABBE') VALUE1=VNUM
              IF(WQ.EQ.'PARTL') VALUE1=PARTL
              IF(WQ.EQ.'INDEX') VALUE1=ALENS(86,INT(W1))
              IF(WQ.EQ.'VNUM') VALUE1=ALENS(87,INT(W1))
              IF(WQ.EQ.'DPART') VALUE1=ALENS(89,INT(W1))
              GO TO 200
          END IF
          IF(WQ.EQ.'CLAP') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(9,INT(W1)).EQ.1.0D0) THEN
                  VALUE1=ALENS(10,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "CLAP" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  VALUE1=-1.0D0
                  CALL MACFAL
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'CLAPE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(51,INT(W1)).EQ.1.0D0) THEN
                  VALUE1=ALENS(52,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "CLAPE" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  VALUE1=-1.0D0
                  CALL MACFAL
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'CLRAD') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(9,INT(W1)).EQ.4.0D0) VALUE1=ALENS(14,INT(W1))
              IF(ALENS(9,INT(W1)).LT.4.0D0) THEN
              ELSE
                  WRITE(OUTLYNE,*)'NO "CLRAD" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'CLRADE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN

                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(51,INT(W1)).EQ.4.0D0) VALUE1=ALENS(56,INT(W1))
              IF(ALENS(51,INT(W1)).LT.4.0D0) THEN
              ELSE
                  WRITE(OUTLYNE,*)'NO "CLRADE" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'C0RAD') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(16,INT(W1)).EQ.4.0D0) VALUE1=ALENS(21,INT(W1))
              IF(ALENS(16,INT(W1)).LT.4.0D0)THEN
              ELSE
                  WRITE(OUTLYNE,*)'NO "CORAD" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'CORADE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(DABS(ALENS(61,INT(W1))).EQ.4.0D0) VALUE1=ALENS(66,INT(W1))
              IF(DABS(ALENS(16,INT(W1))).LT.4.0D0)THEN
              ELSE
                  WRITE(OUTLYNE,*)'NO "CORADE" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'CLDECX'.OR.WQ.EQ.'CLDECY'.OR.WQ.EQ.'CLTILT'
     1    .OR.WQ.EQ.'CODECX'.OR.WQ.EQ.'CODECY'.OR.WQ.EQ.'COTILT') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(ALENS(9,INT(W1)).NE.0.0D0.OR.ALENS(16,INT(W1)).NE.0.0D0) THEN
                  IF(DF1.EQ.1) W1=SYSTEM1(20)
                  IF(ALENS(9,INT(W1)).NE.0.0D0) THEN
                      IF(WQ.EQ.'CLDECX') VALUE1=ALENS(13,INT(W1))
                      IF(WQ.EQ.'CLDECY') VALUE1=ALENS(12,INT(W1))
                      IF(WQ.EQ.'CLTILT') VALUE1=ALENS(15,INT(W1))
                      GO TO 200
                  ELSE
                      IF(ALENS(16,INT(W1)).NE.0.0D0) THEN
                          IF(WQ.EQ.'CODECX') VALUE1=ALENS(20,INT(W1))
                          IF(WQ.EQ.'CODECY') VALUE1=ALENS(19,INT(W1))
                          IF(WQ.EQ.'COTILT') VALUE1=ALENS(22,INT(W1))
                          GO TO 200
                      END IF
                  END IF
              ELSE
C     BOTH ARE ZERO
                  WRITE(OUTLYNE,*)'NO "CLAP/COBS" DEFINED ON CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
                  GO TO 200
              END IF
          END IF
          IF(WQ.EQ.'CLDECXE'.OR.WQ.EQ.'CLDECYE'.OR.WQ.EQ.'CLTILTE'
     1    .OR.WQ.EQ.'CODECXE'.OR.WQ.EQ.'CODECYE'.OR.WQ.EQ.'COTILTE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(ALENS(51,INT(W1)).NE.0.0D0.AND.ALENS(61,INT(W1)).NE.0.0D0) THEN
                  IF(DF1.EQ.1) W1=SYSTEM1(20)
                  IF(ALENS(51,INT(W1)).NE.0.0D0) THEN
                      IF(WQ.EQ.'CLDECXE') VALUE1=ALENS(55,INT(W1))
                      IF(WQ.EQ.'CLDECYE') VALUE1=ALENS(54,INT(W1))
                      IF(WQ.EQ.'CLTILTE') VALUE1=ALENS(57,INT(W1))
                      GO TO 200
                  ELSE
                      IF(ALENS(61,INT(W1)).NE.0.0D0) THEN
                          IF(WQ.EQ.'CODECXE') VALUE1=ALENS(65,INT(W1))
                          IF(WQ.EQ.'CODECYE') VALUE1=ALENS(64,INT(W1))
                          IF(WQ.EQ.'COTILTE') VALUE1=ALENS(66,INT(W1))
                          GO TO 200
                      END IF
                  END IF
              ELSE
                  WRITE(OUTLYNE,*)
     1            'NO "CLAP/COBS ERASE" DEFINED ON CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
                  GO TO 200
              END IF
          END IF
          IF(WQ.EQ.'CLAPY') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(9,INT(W1)).EQ.2.0D0.OR.
     1        ALENS(9,INT(W1)).EQ.3.0D0.OR.ALENS(9,INT(W1))
     2        .EQ.4.0D0) THEN
                  VALUE1=ALENS(10,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "CLAPY" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'CLAPYE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(51,INT(W1)).EQ.2.0D0.OR.
     1        ALENS(9,INT(W1)).EQ.3.0D0.OR.ALENS(9,INT(W1))
     2        .EQ.4.0D0) THEN
                  VALUE1=ALENS(52,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "CLAPYE" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'CLAPX') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(9,INT(W1)).EQ.2.0D0.OR.
     1        ALENS(9,INT(W1)).EQ.3.0D0.OR.ALENS(9,INT(W1))
     1        .EQ.4.0D0) THEN
                  VALUE1=ALENS(11,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "CLAPX" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'CLAPXE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(51,INT(W1)).EQ.2.0D0.OR.
     1        ALENS(9,INT(W1)).EQ.3.0D0.OR.ALENS(9,INT(W1))
     1        .EQ.4.0D0) THEN
                  VALUE1=ALENS(53,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "CLAPXE" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'COBS') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(16,INT(W1)).EQ.1.0D0) THEN
                  VALUE1=ALENS(17,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "COBS" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'COBSE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(16,INT(W1)).EQ.1.0D0) THEN
                  VALUE1=ALENS(62,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "COBSE" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'COBSY') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(16,INT(W1)).EQ.2.0D0.OR.
     1        ALENS(16,INT(W1)).EQ.3.0D0.OR.ALENS(16,INT(W1))
     1        .EQ.4.0D0) THEN
                  VALUE1=ALENS(17,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "COBSY" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'COBSYE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(61,INT(W1)).EQ.2.0D0.OR.
     1        ALENS(16,INT(W1)).EQ.3.0D0.OR.ALENS(16,INT(W1))
     1        .EQ.4.0D0) THEN
                  VALUE1=ALENS(62,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "COBSYE" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'COBSX') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(16,INT(W1)).EQ.2.0D0.OR.
     1        ALENS(16,INT(W1)).EQ.3.0D0.OR.ALENS(16,INT(W1))
     1        .EQ.4.0D0) THEN
                  VALUE1=ALENS(18,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "COBSX" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'COBSXE') THEN
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CALCULATE VALUE1
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(61,INT(W1)).EQ.2.0D0.OR.
     1        ALENS(61,INT(W1)).EQ.3.0D0.OR.ALENS(61,INT(W1))
     1        .EQ.4.0D0) THEN
                  VALUE1=ALENS(63,INT(W1))
              ELSE
                  WRITE(OUTLYNE,*)'NO "COBSXE" TO "GET" FOR THE CURRENT SURFACE"'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  VALUE1=-1.0D0
              END IF
              GO TO 200
          END IF
 2121     CONTINUE
C     TWO NUMERIC WORDS
C     'ACT ' GOES HERE
          IF(WQ.EQ.'ACT     ') THEN
              IF(DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY USES NUMERIC WORDS #1, #2 AND #5'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.0.0D0) NEG=.TRUE.
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(INT(W1).LT.0.OR.INT(W1).GT.INT(SYSTEM1(20))) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(ALENS(103,INT(W1)).NE.1.0D0) THEN
                  WRITE(OUTLYNE,*)'SURFACE NOT DEFORMABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'VALUE1S NOT GETABLE'
                  CALL SHOWIT(1)

                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.1.0D0.OR.W2.GT.
     1        (ALENS(105,INT(W1))**2)) THEN
                  WRITE(OUTLYNE,*)'COEFFICIENT BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     VALUE1S GETABLE
              ISURF=INT(W1)
              WQLOCAL=WQ
              W2LOCAL=W2
              CALL DEFGRIDS(7,ISURF,GERROR1,GERROR2)
              GO TO 200
          END IF
C
C
C
C     TWO NUMERIC WORDS
C     'RLEN' AND 'ORLEN ' GOES HERE
          IF(WQ.EQ.'RLEN    '.OR.WQ.EQ.'ORLEN   ') THEN
              IF(DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY USES NUMERIC WORDS #1, #2 AND #5'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W1).LT.0.OR.INT(W1).GT.INT(SYSTEM1(20))) THEN
                  WRITE(OUTLYNE,*)'STARTING SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W2).LT.0.OR.INT(W2).GT.INT(SYSTEM1(20))) THEN
                  WRITE(OUTLYNE,*)'ENDING SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(INT(W2).LT.INT(W1)) THEN
                  WRITE(OUTLYNE,*)
     1            'ENDING SURFACE NUMBER MUST BE GREATER THAN'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'STARTING SURFACE NUMBER'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     VALUE1S GETABLE
              VALUE1=0.0D0
              DO I=INT(W1)+1,INT(W2)
                  IF(WQ.EQ.'RLEN')  VALUE1=VALUE1+RAYRAY(8,I)
                  IF(WQ.EQ.'ORLEN') VALUE1=VALUE1+RAYRAY(7,I)
              END DO
              GO TO 200
          END IF

          IF(WQ.EQ.'PSF') THEN
              IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET PSF" REQUIRES EXPLICIT NUMERIC WORDS #1 AND #2'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S3.EQ.1.OR.S4.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET PSF" TAKES NO NUMERIC WORD #3 OR #4'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IGET=INT(W1)
              JGET=INT(W2)
              ERRER=.FALSE.
              ERRER2=.FALSE.
              CALL GETPSF(IGET,JGET,V9,ERRER,ERRER2)
              IF(ERRER) THEN
                  WRITE(OUTLYNE,*)
     1            'ERROR, NO PSF.DAT DATA EXISTS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(ERRER2) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #1 OR #2 OR #1 AND #2 ARE OUTSIDE THE CURRENT PSF'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              VALUE1=V9
              GO TO 200
          ELSE
          END IF
          IF(WQ.EQ.'PSFSUM'.OR.WQ.EQ.'PSFFWHMX'.OR.WQ.EQ.'PSFFWHMY') THEN
              IF(SN.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'PSFSUM') THEN
                  CALL GETPSFSUM(V9,ERRER)
                  IF(ERRER) THEN
                      WRITE(OUTLYNE,*)
     1                'ERROR, NO PSF.DAT EXISTS, NO INTEGRATION CAN BE PERFORMED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  VALUE1=V9
                  GO TO 200
              ELSE
              END IF
C
              IF(WQ.EQ.'PSFFWHMX') THEN
                  CALL GETPSFFWHM(V9,ERRER,1)
                  IF(ERRER) THEN
                      WRITE(OUTLYNE,*)
     1                'ERROR, NO PSF.DAT EXISTS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  VALUE1=V9
                  GO TO 200
              ELSE
              END IF
C
              IF(WQ.EQ.'PSFFWHMY') THEN
                  CALL GETPSFFWHM(V9,ERRER,2)
                  IF(ERRER) THEN
                      WRITE(OUTLYNE,*)
     1                'ERROR, NO PSF.DAT EXISTS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  VALUE1=V9
                  GO TO 200
              ELSE
              END IF
C
          END IF
          IF(WQ.EQ.'N1'.OR.WQ.EQ.'N2'.OR.WQ.EQ.'N3'.OR.WQ.EQ.'N4'.OR.
     1    WQ.EQ.'N5'.OR.WQ.EQ.'N6'.OR.WQ.EQ.'N7'.OR.WQ.EQ.'N8'.OR.
     2    WQ.EQ.'N9'.OR.WQ.EQ.'N10') THEN
              IF(DF1.EQ.1) W1=0.0D0
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY USES NUMERIC WORDS #1 AND #5'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH CALCULATIONS
              IF(WQ.EQ.'N1') VALUE1=ALENS(46,(INT(W1)))
              IF(WQ.EQ.'N2') VALUE1=ALENS(47,(INT(W1)))
              IF(WQ.EQ.'N3') VALUE1=ALENS(48,(INT(W1)))
              IF(WQ.EQ.'N4') VALUE1=ALENS(49,(INT(W1)))
              IF(WQ.EQ.'N5') VALUE1=ALENS(50,(INT(W1)))
              IF(WQ.EQ.'N6') VALUE1=ALENS(71,(INT(W1)))
              IF(WQ.EQ.'N7') VALUE1=ALENS(72,(INT(W1)))
              IF(WQ.EQ.'N8') VALUE1=ALENS(73,(INT(W1)))
              IF(WQ.EQ.'N9') VALUE1=ALENS(74,(INT(W1)))
              IF(WQ.EQ.'N10') VALUE1=ALENS(75,(INT(W1)))
              GO TO 200
          END IF
          IF(WQ.EQ.'GBRADX'.OR.WQ.EQ.'GBRADY'.OR.WQ.EQ.'GBRCVX'.OR.
     1    WQ.EQ.'GBRCVY') THEN
C     GAUSSIAN BEAM STUFF THAT TAKES 2 NUMERIC WORDS
C     SURF NUMBER AND Z-OFFSET
              IF(DF1.EQ.1) W1=0.0D0
              IF(DF2.EQ.1) W2=0.0D0
              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY USES NUMERIC WORDS #1, #2 AND #5'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH CALCULATIONS
C
              IF(WQ.EQ.'GBRADX') THEN
                  OLDREF=NEWREF
                  OLDOBJ=NEWOBJ
                  OLDIMG=NEWIMG
                  NEWIMG=INT(SYSTEM1(20))
                  NEWOBJ=0
                  NEWREF=1
C     CALCULATE THE GPX,GPUX,GPCX AND GPUCX
                  ERROR=0
                  IF(REFEXT) THEN
                      NWN1=LFOB(1)
                      NWN2=LFOB(2)
                      NWN3=LFOB(3)
                      NWN4=LFOB(4)
                  ELSE
                      NWN1=0.0D0
                      NWN2=0.0D0
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                  END IF
                  CALL GNPR2(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPX=VALUE1
                  CALL GNPR4(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPUX=VALUE1
                  CALL GNPR6(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPCX=VALUE1
                  CALL GNPR8(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPUCX=VALUE1
                  VALUE1=(JPX**2)+(JPCX**2)+((JPUX**2)*(W2**2))
     1            +((JPUCX**2)*(W2**2))+
     2            (2.0D0*JPX*JPUX*W2)+(2.0D0*JPCX*JPUCX*W2)
                  VALUE1=DSQRT(VALUE1)
                  NEWOBJ=OLDOBJ
                  NEWREF=OLDREF
                  NEWIMG=OLDIMG
                  GO TO 200
              END IF
              IF(WQ.EQ.'GBRADY') THEN
                  OLDREF=NEWREF
                  OLDOBJ=NEWOBJ
                  OLDIMG=NEWIMG
                  NEWIMG=INT(SYSTEM1(20))
                  NEWOBJ=0
                  NEWREF=1
C     CALCULATE THE GPY,GPUY,GPCY AND GPUCY
                  ERROR=0
                  IF(REFEXT) THEN
                      NWN1=LFOB(1)
                      NWN2=LFOB(2)
                      NWN3=LFOB(3)
                      NWN4=LFOB(4)
                  ELSE
                      NWN1=0.0D0
                      NWN2=0.0D0
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                  END IF
                  CALL GNPR1(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPY=VALUE1
                  CALL GNPR3(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPUY=VALUE1
                  CALL GNPR5(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPCY=VALUE1
                  CALL GNPR7(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPUCY=VALUE1
                  VALUE1=(JPY**2)+(JPCY**2)+((JPUY**2)*(W2**2))
     1            +((JPUCY**2)*(W2**2))+
     2            (2.0D0*JPY*JPUY*W2)+(2.0D0*JPCY*JPUCY*W2)
                  VALUE1=DSQRT(VALUE1)
                  NEWOBJ=OLDOBJ
                  NEWREF=OLDREF
                  NEWIMG=OLDIMG
                  GO TO 200
              END IF
              IF(WQ.EQ.'GBRCVX') THEN
                  OLDREF=NEWREF
                  OLDOBJ=NEWOBJ
                  OLDIMG=NEWIMG
                  NEWIMG=INT(SYSTEM1(20))
                  NEWOBJ=0
                  NEWREF=1
C     CALCULATE THE GPX,GPUX,GPCX AND GPUCX
                  ERROR=0
                  IF(REFEXT) THEN
                      NWN1=LFOB(1)
                      NWN2=LFOB(2)
                      NWN3=LFOB(3)
                      NWN4=LFOB(4)
                  ELSE
                      NWN1=0.0D0
                      NWN2=0.0D0
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                  END IF
                  CALL GNPR2(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPX=VALUE1
                  CALL GNPR4(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPUX=VALUE1
                  CALL GNPR6(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPCX=VALUE1
                  CALL GNPR8(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPUCX=VALUE1
C     SEMI-DIAMETER AT Z IS JUST
                  VALUE1=(JPX**2)+(JPCX**2)+((JPUX**2)*(W2**2))
     1            +((JPUCX**2)*(W2**2))+
     2            (2.0D0*JPX*JPUX*W2)+(2.0D0*JPCX*JPUCX*W2)
                  V1=DSQRT(VALUE1)
                  VALVAL=V1
C     SLOPE AT Z IS :
                  IF(REAL(VALVAL).EQ.0.0) THEN
                      V1=1.0D20
                  ELSE
                      V1=(((JPUX**2)*W2)+((JPUCX**2)*W2)+(JPX*JPUX)+(JPCX*JPUCX))/V1
                  END IF
C     RADIUS OF CURVATURE IS:
                  IF(REAL(V1).EQ.0.0) THEN
                      VALUE1=1.0D20
                  ELSE
                      VALUE1=-VALVAL/V1
                  END IF
                  NEWOBJ=OLDOBJ
                  NEWREF=OLDREF
                  NEWIMG=OLDIMG
                  GO TO 200
              END IF
              IF(WQ.EQ.'GBRCVY') THEN
                  OLDREF=NEWREF
                  OLDOBJ=NEWOBJ
                  OLDIMG=NEWIMG
                  NEWIMG=INT(SYSTEM1(20))
                  NEWOBJ=0
                  NEWREF=1
C     CALCULATE THE GPY,GPUY,GPCY AND GPUCY
                  ERROR=0
                  IF(REFEXT) THEN
                      NWN1=LFOB(1)
                      NWN2=LFOB(2)
                      NWN3=LFOB(3)
                      NWN4=LFOB(4)
                  ELSE
                      NWN1=0.0D0
                      NWN2=0.0D0
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                  END IF
                  CALL GNPR1(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPY=VALUE1
                  CALL GNPR3(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPUY=VALUE1
                  CALL GNPR5(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPCY=VALUE1
                  CALL GNPR7(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,1)
                  JPUCY=VALUE1
C     SEMI-DIAMETER AT Z IS JUST
                  VALUE1=(JPY**2)+(JPCY**2)+((JPUY**2)*(W2**2))
     1            +((JPUCY**2)*(W2**2))+
     2            (2.0D0*JPY*JPUY*W2)+(2.0D0*JPCY*JPUCY*W2)
                  V1=DSQRT(VALUE1)
                  VALVAL=V1
C     SLOPE AT Z IS :
                  IF(REAL(VALVAL).EQ.0.0) THEN
                      V1=1.0D20
                  ELSE
                      V1=(((JPUY**2)*W2)+((JPUCY**2)*W2)+(JPY*JPUY)+(JPCY*JPUCY))/V1
                  END IF
C     RADIUS OF CURVATURE IS:
                  IF(REAL(V1).EQ.0.0) THEN
                      VALUE1=1.0D20
                  ELSE
                      VALUE1=-VALVAL/V1
                  END IF
                  NEWOBJ=OLDOBJ
                  NEWREF=OLDREF
                  NEWIMG=OLDIMG
                  GO TO 200
              END IF
C
          END IF
C
C       GLOBAL VERTEX DATA
C
          IF(WQ.EQ.'XVERT'.OR.WQ.EQ.'YVERT'.OR.WQ.EQ.'ZVERT'
     1    .OR.WQ.EQ.'LXVERT'.OR.WQ.EQ.'MXVERT'.OR.WQ.EQ.'NXVERT'
     2    .OR.WQ.EQ.'LYVERT'.OR.WQ.EQ.'MYVERT'.OR.WQ.EQ.'NYVERT'
     3    .OR.WQ.EQ.'LZVERT'.OR.WQ.EQ.'MZVERT'.OR.WQ.EQ.'NZVERT'
     7    )THEN
              IF(DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORDS #1, #2 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" REQUIRES EXPLICIT NUMERIC WORD #2 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.NEWOBJ.OR.W1.GT.NEWIMG) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" NW1, SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.NEWOBJ.OR.W2.GT.NEWIMG) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" NW2, SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              SAVE_KDP(30)=SAVEINPT(30)
              WRITE(INPUT,*)'GLOBAL,',W2
              CALL PROCES
              REST_KDP(30)=RESTINPT(30)
              IF(WQ.EQ.'XVERT') VALUE1=VERTEX(1,INT(W1))
              IF(WQ.EQ.'YVERT') VALUE1=VERTEX(2,INT(W1))
              IF(WQ.EQ.'ZVERT') VALUE1=VERTEX(3,INT(W1))
              IF(WQ.EQ.'LXVERT') VALUE1=VERTEX(4,INT(W1))
              IF(WQ.EQ.'MXVERT') VALUE1=VERTEX(5,INT(W1))
              IF(WQ.EQ.'NXVERT') VALUE1=VERTEX(6,INT(W1))
              IF(WQ.EQ.'LYVERT') VALUE1=VERTEX(7,INT(W1))
              IF(WQ.EQ.'MYVERT') VALUE1=VERTEX(8,INT(W1))
              IF(WQ.EQ.'NYVERT') VALUE1=VERTEX(9,INT(W1))
              IF(WQ.EQ.'LZVERT') VALUE1=VERTEX(10,INT(W1))
              IF(WQ.EQ.'MZVERT') VALUE1=VERTEX(11,INT(W1))
              IF(WQ.EQ.'NZVERT') VALUE1=VERTEX(12,INT(W1))
              GO TO 200
          END IF
C
          IF(WQ.EQ.'PWRY'.OR.WQ.EQ.'PWRX'.OR.WQ.EQ.
     1    'LENGTH'.OR.WQ.EQ.'MLENGTH'.OR.WQ.EQ.'OAL'.OR.
     3    WQ.EQ.'OPTLEN'.OR.WQ.EQ.'FLCLTHY'.OR.WQ.EQ.'FLCLTHX'.OR.
     4    WQ.EQ.'FLCLTH'.OR.WQ.EQ.'WEIGHT')THEN
              IF(DF1.EQ.1) W1=0.0D0
              IF(DF2.EQ.1) W2=SYSTEM1(20)
              IF(DF1.EQ.1) S1=1
              IF(DF2.EQ.1) S2=1
              IF(DF1.EQ.1) DF1=0
              IF(DF2.EQ.1) DF2=0

              IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
              IF(W2.LT.0.0D0) W2=SYSTEM1(20)+W2
              IF(W1.LT.0.0D0.OR.W2.GT.SYSTEM1(20)) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.GE.W2) THEN
                  WRITE(OUTLYNE,*)'SURFACE NUMBERS OUT OF ORDER'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY USES NUMERIC WORDS #1, #2 AND #5'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH CALCULATIONS
              IF(WQ.NE.'WEIGHT') THEN
                  CALL BASOP
              ELSE
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC='WEIGHT  '
                  WQ='ACC     '
                  SQ=1
                  SST=0
                  W3=0.0D0
                  W4=0.0D0
                  W5=0.0D0
                  DF3=1
                  DF4=1
                  DF5=1
                  S3=0
                  S4=0
                  S5=0
                  SN=1
                  CALL WEIGHT
                  VALUE1=REG(9)
                  REST_KDP(1)=RESTINPT(1)
              END IF
              GO TO 200
          END IF
C
          IF(WQ.EQ.'DIST'.OR.WQ.EQ.'FISHDIST') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1) W2=0.0D0
              IF(DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY USES NUMERIC WORDS #1, #2 AND #5'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH CALCULATIONS
              DWORD1=W1
              DWORD2=W2
              ERROR=0
              IF(WQ.EQ.'DIST') CACOCH=0
              IF(WQ.EQ.'FISHDIST') CACOCH=0
              IF(WQ.EQ.'DIST') CALL DISTOR(DWORD1,DWORD2,ERROR)
              IF(WQ.EQ.'FISHDIST') CALL FDISTOR(DWORD1,DWORD2,ERROR)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"DIST" IS NOT CURRENTLY CALCULABLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 200
          END IF
          IF(WQ.EQ.'XFOC'.OR.WQ.EQ.'YFOC'.OR.WQ.EQ.'AST') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1) W2=0.0D0
              IF(DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY USES NUMERIC WORDS #1, #2 AND #5'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH CALCULATIONS
              DWORD1=W1
              DWORD2=W2
              IF(WQ.EQ.'YFOC') ORIEN=0
              IF(WQ.EQ.'XFOC') ORIEN=1
              IF(WQ.EQ.'AST')  ORIEN=2
              ERROR=0
              CACOCH=0
              CALL FLDCRV(ORIEN,DWORD1,DWORD2,ERROR)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"',WQ(1:4),'" IS NOT CURRENTLY CALCULABLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              GO TO 200
          END IF
C
          IF(WQ.EQ.'CTSX'.OR.WQ.EQ.'CTSY') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF2.EQ.1) W2=0.0D0
              IF(DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY USES NUMERIC WORDS #1, #2 AND #5'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH CALCULATIONS
              ERROR=0
              V1=W1
              V2=W2
              IF(WQ.EQ.'CTSX') CACOCH=0
              IF(WQ.EQ.'CTSY') CACOCH=0
              IF(WQ.EQ.'CTSX') CALL CTS(VALVAL,1,V1,V2,ERROR)
              IF(WQ.EQ.'CTSY') CALL CTS(VALVAL,2,V1,V2,ERROR)
              IF(ERROR.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"',WQ(1:4),'" IS NOT CURRENTLY CALCULABLE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              VALUE1=VALVAL
              GO TO 200
          END IF
C
          IF(WQ.EQ.'DERIV'.OR.WQ.EQ.'MATRIX') THEN
              IF(.NOT.DEREXT) THEN
                  WRITE(OUTLYNE,*)
     1            'NO MATRIX EXISTS, DERIVATIVES ARE NOT AVAILABLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" REQUIRES EXPLICIT NUMERIC WORDS #1 AND #2'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W1.LT.1.0D0.OR.W1.GT.DBLE(VBCNT)) THEN
                  WRITE(OUTLYNE,*)
     1            '"VARIABLE NUMBER DESIGNATOR BEYOND CURRENT BOUNDARY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(W2.LT.1.0D0.OR.W2.GT.DBLE(OPCNT)) THEN
                  WRITE(OUTLYNE,*)
     1            '"OPERAND NUMBER DESIGNATOR BEYOND CURRENT BOUNDARY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY USES NUMERIC WORDS #1, #2 AND #5'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       PROCEED WITH CALCULATIONS
              GETTER=.TRUE.
              ITERROR=.FALSE.
              CALL ITER(0,0,ITERROR)
              VALUE1=DERVAL
              GETTER=.FALSE.
              GO TO 200
          END IF
C
          IF(WQ.EQ.'PY'.OR.WQ.EQ.'PUY'.OR.WQ.EQ.'PIY'.OR.WQ.EQ.'PCY'
     1    .OR.WQ.EQ.'PUCY'.OR.WQ.EQ.'PICY'.OR.WQ.EQ.'PX'.OR.WQ.EQ.'PUX'
     2    .OR.WQ.EQ.'PIX'.OR.WQ.EQ.'PCX'.OR.WQ.EQ.'PUCX'.OR.WQ.EQ.'PICX'
     3    .OR.WQ.EQ.'PICYP'.OR.WQ.EQ.'PICXP'.OR.WQ.EQ.'PIYP'.OR.WQ.EQ.
     4    'PIXP') THEN
              IF(DF1.EQ.1) W1=SYSTEM1(20)
              IF(DF2.EQ.1) W2=SYSTEM1(11)
              IF(DF3.EQ.0.OR.DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY USES NUMERIC WORDS #1, #2 AND #5'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'PY'.OR.WQ.EQ.'PUY'.OR.WQ.EQ.'PIY'.OR.WQ.EQ.'PCY'
     1        .OR.WQ.EQ.'PUCY'.OR.WQ.EQ.'PICY'.OR.WQ.EQ.'PX'.OR.WQ.EQ.'PUX'
     2        .OR.WQ.EQ.'PIX'.OR.WQ.EQ.'PCX'.OR.WQ.EQ.'PUCX'.OR.WQ.EQ.'PICX'
     3        .OR.WQ.EQ.'PICYP'.OR.WQ.EQ.'PICXP'.OR.WQ.EQ.'PIYP'.OR.WQ.EQ.
     4        'PIXP') THEN
                  IF(W1.LT.0.0D0) W1=SYSTEM1(20)+W1
                  IF(W1.LT.0.0.OR.W1.GT.SYSTEM1(20))THEN
                      WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W2.LT.1.0.OR.W2.GT.10.0)THEN
                      WRITE(OUTLYNE,*)'WAVELENGTH NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  IF(W2.EQ.SYSTEM1(11)) THEN
                      IF(WQ.EQ.'PY')VALUE1=PXTRAY(1,INT(W1))
                      IF(WQ.EQ.'PUY')VALUE1=PXTRAY(2,INT(W1))
                      IF(WQ.EQ.'PIY')VALUE1=PXTRAY(3,INT(W1))
                      IF(WQ.EQ.'PCY')VALUE1=PXTRAY(5,INT(W1))
                      IF(WQ.EQ.'PUCY')VALUE1=PXTRAY(6,INT(W1))
                      IF(WQ.EQ.'PICY')VALUE1=PXTRAY(7,INT(W1))
                      IF(WQ.EQ.'PX')VALUE1=PXTRAX(1,INT(W1))
                      IF(WQ.EQ.'PUX')VALUE1=PXTRAX(2,INT(W1))
                      IF(WQ.EQ.'PIX')VALUE1=PXTRAX(3,INT(W1))
                      IF(WQ.EQ.'PCX')VALUE1=PXTRAX(5,INT(W1))
                      IF(WQ.EQ.'PUCX')VALUE1=PXTRAX(6,INT(W1))
                      IF(WQ.EQ.'PICX')VALUE1=PXTRAX(7,INT(W1))
                      IF(WQ.EQ.'PIYP')VALUE1=PXTRAY(4,INT(W1))
                      IF(WQ.EQ.'PIXP')VALUE1=PXTRAX(4,INT(W1))
                      IF(WQ.EQ.'PICYP')VALUE1=PXTRAY(8,INT(W1))
                      IF(WQ.EQ.'PICXP')VALUE1=PXTRAX(8,INT(W1))
                      GO TO 200
                  END IF
                  IF(W2.NE.SYSTEM1(11)) THEN
                      WV=W2
                      ITYP=1
                      CALL PRCOL
                      WV=W2
                      ITYP=2
                      CALL PRCOL
                      IF(WQ.EQ.'PY')VALUE1=COLY(1,INT(W1))
                      IF(WQ.EQ.'PUY')VALUE1=COLY(2,INT(W1))
                      IF(WQ.EQ.'PIY')VALUE1=COLY(3,INT(W1))
                      IF(WQ.EQ.'PCY')VALUE1=COLY(5,INT(W1))
                      IF(WQ.EQ.'PUCY')VALUE1=COLY(6,INT(W1))
                      IF(WQ.EQ.'PICY')VALUE1=COLY(7,INT(W1))
                      IF(WQ.EQ.'PX')VALUE1=COLX(1,INT(W1))
                      IF(WQ.EQ.'PUX')VALUE1=COLX(2,INT(W1))
                      IF(WQ.EQ.'PIX')VALUE1=COLX(3,INT(W1))
                      IF(WQ.EQ.'PCX')VALUE1=COLX(5,INT(W1))
                      IF(WQ.EQ.'PUCX')VALUE1=COLX(6,INT(W1))
                      IF(WQ.EQ.'PICX')VALUE1=COLX(7,INT(W1))
                      IF(WQ.EQ.'PIYP')VALUE1=COLX(4,INT(W1))
                      IF(WQ.EQ.'PIXP')VALUE1=COLY(4,INT(W1))
                      IF(WQ.EQ.'PICYP')VALUE1=COLX(8,INT(W1))
                      IF(WQ.EQ.'PICXP')VALUE1=COLY(8,INT(W1))
                      GO TO 200
                  END IF
              END IF
          END IF
C       SEARCH FOR GETS USING THREE NUMERIC INPUT WORDS
          IF(WQ.EQ.'SPS'.OR.WQ.EQ.'SAG'.OR.WQ.EQ.'GPY'.OR.WQ.EQ.'GPX'
     1    .OR.WQ.EQ.'GPCX'.OR.WQ.EQ.'GPCY'.OR.WQ.EQ.'GPUY'.OR.WQ.EQ.
     2    'GPUX'.OR.WQ.EQ.'GPUCX'.OR.WQ.EQ.'GPUCY') THEN
              IF(DF4.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"GET ',WQ,'" ONLY TAKES NUMERIC WORD #1, #2, #3 AND #5 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'SPS') THEN

                  OUTLYNE='PARAMETER NOT CURRENTLY "GET"-ABLE'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(WQ.EQ.'SAG') THEN
                  CALL SSAAGG
                  RETURN
              END IF
              IF(WQ.EQ.'GPY')           GPR=.TRUE.
              IF(WQ.EQ.'GPX')           GPR=.TRUE.
              IF(WQ.EQ.'GPUY')          GPR=.TRUE.
              IF(WQ.EQ.'GPUX')          GPR=.TRUE.
              IF(WQ.EQ.'GPCY')          GPR=.TRUE.
              IF(WQ.EQ.'GPCX')          GPR=.TRUE.
              IF(WQ.EQ.'GPUCY')         GPR=.TRUE.
              IF(WQ.EQ.'GPUCX')         GPR=.TRUE.
              IF(GPR) THEN
                  IF(W1.LT.0.0D0) NEG=.TRUE.
                  IF(W1.LT.0.0D0) W1=DBLE(NEWIMG)+W1
                  IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
                      WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF1.EQ.1) W1=NEWIMG
                  ERROR=0
                  IF(WQ.EQ.'GPY') THEN
                      NWN1=W2
                      NWN2=W3
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                      CALL GNPR1(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,0)
                  END IF
                  ERROR=0
                  IF(WQ.EQ.'GPX') THEN
                      NWN1=W2
                      NWN2=W3
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                      CALL GNPR2(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,0)
                  END IF
                  ERROR=0
                  IF(WQ.EQ.'GPUY') THEN
                      NWN1=W2
                      NWN2=W3
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                      CALL GNPR3(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,0)
                  END IF
                  ERROR=0
                  IF(WQ.EQ.'GPUX') THEN
                      NWN1=W2
                      NWN2=W3
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                      CALL GNPR4(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,0)
                  END IF
                  ERROR=0
                  IF(WQ.EQ.'GPCY') THEN
                      NWN1=W2
                      NWN2=W3
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                      CALL GNPR5(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,0)
                  END IF
                  ERROR=0
                  IF(WQ.EQ.'GPCX') THEN
                      NWN1=W2
                      NWN2=W3
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                      CALL GNPR6(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,0)
                  END IF
                  ERROR=0
                  IF(WQ.EQ.'GPUCY') THEN
                      NWN1=W2
                      NWN2=W3
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                      CALL GNPR7(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,0)
                  END IF
                  ERROR=0
                  IF(WQ.EQ.'GPUCX') THEN
                      NWN1=W2
                      NWN2=W3
                      NWN3=0.0D0
                      NWN4=SYSTEM1(11)
                      CALL GNPR8(INT(W1),NWN1,NWN2,NWN3,NWN4,ERROR,0)
                  END IF
C
                  IF(ERROR.EQ.1) THEN
                      WRITE(OUTLYNE,*)'RAY COULD NOT BE TRACED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'GENERALIZED PARAXIAL RAY VALUE1S NOT CALCULABLE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  GPR=.FALSE.
                  GO TO 200
              END IF
          END IF
C
          RETURN
C
 200      CONTINUE
          NUM5=INT(W5)
          CALL GETA
          IF(SHOW) THEN
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='WRITE X '//REMWQ
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
          END IF
          RETURN
      END
