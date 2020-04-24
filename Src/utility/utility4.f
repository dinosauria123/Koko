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

C       FOURTH SET OF UTILTIY ROUTINES GO HERE

C SUB PUTAB.FOR
      SUBROUTINE PUTAB
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO ENTER THE SPECT LEVEL
C       COMMAND "PUNCH" AT THE SPECTR LEVEL
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
          LOGICAL OPENCR
C
          INCLUDE 'datmai.inc'
C
          OPENCR=.FALSE.
          INQUIRE(FILE=trim(HOME)//'CARDTEXT.DAT',OPENED=OPENCR)
          IF(.NOT.OPENCR) THEN
C     OPEN AS SEQUENTIAL, SET APPEND TO TRUE
              APPEND=.TRUE.
              IF(APPEND) OPEN(UNIT=8,ACCESS='APPEND',BLANK='NULL',
     1        FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT',
     2        STATUS='UNKNOWN')
              IF(.NOT.APPEND) OPEN(UNIT=8,ACCESS='SEQUENTIAL',BLANK='NULL',
     1        FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT',
     2        STATUS='UNKNOWN')
          ELSE
          END IF
          LAMB1=TABLE(1,2)
          LAMB2=TABLE(CTAB,2)
          WRITE(8,200) TNAME
          WRITE(8,204) LAMB1
          WRITE(8,205) LAMB2
          WRITE(8,206) CTAB
          WRITE(8,201)
          WRITE(8,202)
          WRITE(8,203)
C       PROCEED
          DO 10 I=1,CTAB
              COUNT=INT(TABLE(I,1))
              WRITE(8,100) COUNT,TABLE(I,2),TABLE(I,3)
 10       CONTINUE
          IF(OUT.NE.8) THEN
              CALL CLOSE_FILE(8,1)
          ELSE
          END IF
 100      FORMAT(6X,I3,8X,G18.10,5X,G18.10)
 200      FORMAT(
     1    'LISTING OF THE "SPECT" TABLE MEMORY AREA FOR FILE (',
     1     A8,' )')
 201      FORMAT(2X)
 202      FORMAT('ENTRY NUMBER',4X,'WAVLENGTH-(MICRONS)',6X,
     1    'FUNCTION VALUE')
 203      FORMAT(
     1    '-------------------------------------------------------')
 204      FORMAT(
     1    'SHORTEST WAVELENGTH VALUE CURRENTLY IN THE FILE = '
     2    ,G18.10,' MICRONS')
 205      FORMAT(
     1    'LONGEST  WAVELENGTH VALUE CURRENTLY IN THE FILE = '
     2    ,G18.10,' MICRONS')
 206      FORMAT('TOTAL NUMBER OF DATA POINTS = ',I3)
C                       RETURN
      END
C SUB AUXSET.FOR
      SUBROUTINE AUXSET
C       THIS IS A MORE SHORT CUT METHOD OF SETTING REGISTER VALUES
C       THIS SUBROUTINE IS USED TO SET A NAMED REGISTER (A THROUGH H)
C       THE ACCUMULATOR (NAMED 'BLANK' OR 'ACC' OR 'X')
C       OR THE 'Y','Z','T','IX','IY','IZ',AND 'IT' REGISTERS, AND
C       THE INDEXING REGISTERS I,ITEST,J AND JTEST.
C
          IMPLICIT NONE
C
          LOGICAL QSTRING
C
C       WC IS USED TO CALL THE REGISTER BY NAME.
C
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1) THEN
              OUTLYNE='NO STRING INPUT IS USED WHEN'
              CALL SHOWIT(1)
              OUTLYNE='SETTING THE REGISTER VALUES IN THIS WAY'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE=
     1        'NO NUMERIC WORD 2, 3, 4, OR 5 INPUT IS USED WHEN'
              CALL SHOWIT(1)
              OUTLYNE='SETTING THE REGISTER VALUES IN THIS WAY'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              DF1=1
              S1=0
              W1=0.0D0
              SN=0
          END IF
          IF(DF1.EQ.1) W1=0.0D0
          IF(SQ.EQ.1) THEN
              QSTRING=.FALSE.
              WS='                                                 '
              IF(WQ.EQ.'A      ') QSTRING=.TRUE.
              IF(WQ.EQ.'B      ') QSTRING=.TRUE.
              IF(WQ.EQ.'C      ') QSTRING=.TRUE.
              IF(WQ.EQ.'D      ') QSTRING=.TRUE.
              IF(WQ.EQ.'E      ') QSTRING=.TRUE.
              IF(WQ.EQ.'F      ') QSTRING=.TRUE.
              IF(WQ.EQ.'G      ') QSTRING=.TRUE.
              IF(WQ.EQ.'H      ') QSTRING=.TRUE.
              IF(WQ.EQ.'I      ') QSTRING=.TRUE.
              IF(WQ.EQ.'J      ') QSTRING=.TRUE.
              IF(WQ.EQ.'K      ') QSTRING=.TRUE.
              IF(WQ.EQ.'L      ') QSTRING=.TRUE.
              IF(WQ.EQ.'M      ') QSTRING=.TRUE.
              IF(WQ.EQ.'N      ') QSTRING=.TRUE.
              IF(WQ.EQ.'X      ') QSTRING=.TRUE.
              IF(WQ.EQ.'Y      ') QSTRING=.TRUE.
              IF(WQ.EQ.'Z      ') QSTRING=.TRUE.
              IF(WQ.EQ.'T      ') QSTRING=.TRUE.
              IF(WQ.EQ.'X      ') QSTRING=.TRUE.
              IF(WQ.EQ.'Y      ') QSTRING=.TRUE.
              IF(WQ.EQ.'Z      ') QSTRING=.TRUE.
              IF(WQ.EQ.'T      ') QSTRING=.TRUE.
              IF(WQ.EQ.'ITEST  ') QSTRING=.TRUE.
              IF(WQ.EQ.'JTEST  ') QSTRING=.TRUE.
              IF(WQ.EQ.'KTEST  ') QSTRING=.TRUE.
              IF(WQ.EQ.'LTEST  ') QSTRING=.TRUE.
              IF(WQ.EQ.'MTEST  ') QSTRING=.TRUE.
              IF(WQ.EQ.'NTEST  ') QSTRING=.TRUE.
              IF(WQ.EQ.'A      ') WS='A'
              IF(WQ.EQ.'B      ') WS='B'
              IF(WQ.EQ.'C      ') WS='C'
              IF(WQ.EQ.'D      ') WS='D'
              IF(WQ.EQ.'E      ') WS='E'
              IF(WQ.EQ.'F      ') WS='F'
              IF(WQ.EQ.'G      ') WS='G'
              IF(WQ.EQ.'H      ') WS='H'
              IF(WQ.EQ.'I      ') WS='I'
              IF(WQ.EQ.'J      ') WS='J'
              IF(WQ.EQ.'K      ') WS='K'
              IF(WQ.EQ.'L      ') WS='L'
              IF(WQ.EQ.'M      ') WS='M'
              IF(WQ.EQ.'N      ') WS='N'
              IF(WQ.EQ.'ITEST  ') WS='ITEST'
              IF(WQ.EQ.'JTEST  ') WS='JTEST'
              IF(WQ.EQ.'KTEST  ') WS='KTEST'
              IF(WQ.EQ.'LTEST  ') WS='LTEST'
              IF(WQ.EQ.'MTEST  ') WS='MTEST'
              IF(WQ.EQ.'NTEST  ') WS='NTEST'
              IF(WQ.EQ.'X') WS='X'
              IF(WQ.EQ.'Y') WS='Y'
              IF(WQ.EQ.'Z') WS='Z'
              IF(WQ.EQ.'T') WS='T'
              IF(WQ.EQ.'IX') WQ='IX'
              IF(WQ.EQ.'IY') WQ='IY'
              IF(WQ.EQ.'IZ') WQ='IZ'
              IF(WQ.EQ.'IT') WQ='IT'
              IF(QSTRING) SST=1
              IF(QSTRING) SQ=0
              IF(QSTRING) S1=0
              IF(QSTRING) DF1=1
          END IF
          IF(WC.EQ.'A=') WQ='A'
          IF(WC.EQ.'B=') WQ='B'
          IF(WC.EQ.'C=') WQ='C'
          IF(WC.EQ.'D=') WQ='D'
          IF(WC.EQ.'E=') WQ='E'
          IF(WC.EQ.'F=') WQ='F'
          IF(WC.EQ.'G=') WQ='G'
          IF(WC.EQ.'H=') WQ='H'
          IF(WC.EQ.'I=') WQ='I'
          IF(WC.EQ.'J=') WQ='J'
          IF(WC.EQ.'K=') WQ='K'
          IF(WC.EQ.'L=') WQ='L'
          IF(WC.EQ.'M=') WQ='M'
          IF(WC.EQ.'N=') WQ='N'
          IF(WC.EQ.'ITEST=') WQ='ITEST'
          IF(WC.EQ.'JTEST=') WQ='JTEST'
          IF(WC.EQ.'KTEST=') WQ='KTEST'
          IF(WC.EQ.'LTEST=') WQ='LTEST'
          IF(WC.EQ.'MTEST=') WQ='MTEST'
          IF(WC.EQ.'NTEST=') WQ='NTEST'
          IF(WC.EQ.'X=') WQ='X'
          IF(WC.EQ.'Y=') WQ='Y'
          IF(WC.EQ.'Z=') WQ='Z'
          IF(WC.EQ.'T=') WQ='T'
          IF(WC.EQ.'IX=') WQ='IX'
          IF(WC.EQ.'IY=') WQ='IY'
          IF(WC.EQ.'IZ=') WQ='IZ'
          IF(WC.EQ.'IT=') WQ='IT'
          CALL SET
          RETURN
      END
C SUB BASOP.FOR
      SUBROUTINE BASOP
C
          IMPLICIT NONE
C
C       SUBROUTINE GET SERVES TO GET LENGTH,MLENGTH,PWRY,PWRX
C       MAGNIF, (FLCLTH OR FLCLTHY) AND FLCLTHX VALUES.
C       IT IS CALLED BY GET.FOR AND RETURNS THE
C       REAL*8
C
          REAL*8 VALUE1,W1A,W1B,V1,V2
     1    ,EFLY,EFLX
C
          INTEGER CW,I,J,NUM5

          COMMON/GV/VALUE1,NUM5
C
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
C
          W1A=W1
          W1B=W1
          IF(W1.EQ.0.0D0) THEN
              W1B=W1
          ELSE
              W1B=W1-1.0D0
          END IF
C       GET MAGNIF
          IF(WQ.EQ.'MAGNIF') THEN
C       V1 IS DISTACE TO IMAGE BEFORE SURFACE W1
C       V2 IS IMAGE HEIGHT AT THAT SURFACE
              V1=((-PXTRAY(1,INT(W1A)))/(PXTRAY(2,INT(W1B))))
              V2=(V1*PXTRAY(6,INT(W1B)))+PXTRAY(5,INT(W1A))
C       V3 IS DISTACE TO IMAGE AFTER SURFACE W2
C       V2 IS IMAGE HEIGHT AT THAT SURFACE
              V1=((-PXTRAY(1,INT(W2)))/(PXTRAY(2,INT(W2))))
              V2=(V1*PXTRAY(6,INT(W2)))+PXTRAY(5,INT(W2))
C       VALUE=V1/V2
              VALUE1=V1/V2
              RETURN
          ELSE
C       PROCEED
          END IF
C
C       GET LENGTH
C
          IF(WQ.EQ.'MLENGTH'.OR.WQ.EQ.'OPTLEN') THEN
              VALUE1=0.0D0
              DO I=INT(W1),INT(W2)-1
                  IF(INT(SYSTEM1(11)).GE.1.AND.INT(SYSTEM1(11)).LE.5) THEN
                      CW=INT(SYSTEM1(11))+45
                  END IF
                  IF(INT(SYSTEM1(11)).GE.6.AND.INT(SYSTEM1(11)).LE.10) THEN
                      CW=INT(SYSTEM1(11))+65
                  END IF
                  VALUE1=VALUE1+(ALENS(3,I)*ALENS(CW,I))
              END DO
              RETURN
          ELSE
          END IF
C
C       GET LENGTH OR OAL
C
          IF(WQ.EQ.'LENGTH'.OR.WQ.EQ.'OAL') THEN
              VALUE1=0.0D0
              DO I=INT(W1),INT(W2)-1
                  VALUE1=VALUE1+ALENS(3,I)
              END DO
              RETURN
          ELSE
          END IF
C
          IF(WQ.EQ.'FLCLTH'.OR.WQ.EQ.'PWRY'.OR.WQ.EQ.'PWRX'.OR.WQ.EQ.
     1    'FLCLTHY'.OR.WQ.EQ.'FLCLTHX') THEN
C       YZ AND XZ PLANE EFL CALCULATION
C
              IF(W1.GT.0.0D0) THEN
                  I=INT(W1)-1
              ELSE
                  I=INT(W1)
              END IF
              J=INT(W2)
              EFLY=-(((PXTRAY(2,I)*PXTRAY(5,I+1))-(PXTRAY(1,I+1)*PXTRAY(6,I
     1        )))/((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
              EFLX=-(((PXTRAX(2,I)*PXTRAX(5,I+1))-(PXTRAX(1,I+1)*PXTRAX(6,I
     1        )))/((PXTRAX(2,I)*PXTRAX(6,J))-(PXTRAX(6,I)*PXTRAX(2,J))))
              IF(WQ.EQ.'FLCLTH') VALUE1=EFLY
              IF(WQ.EQ.'FLCLTHY') VALUE1=EFLY
              IF(WQ.EQ.'FLCLTHX') VALUE1=EFLX
              IF(WQ.EQ.'PWRY') THEN
                  IF(EFLY.NE.0.0D0) THEN
                      VALUE1=1.0D0/EFLY
                  ELSE
                      VALUE1=0.0D0
                  END IF
                  RETURN
              ELSE
              END IF
              IF(WQ.EQ.'PWRX') THEN
                  IF(EFLY.NE.0.0D0) THEN
                      VALUE1=1.0D0/EFLX
                  ELSE
                      VALUE1=0.0D0
                  END IF
                  RETURN
              ELSE
              END IF
          ELSE
C       PROCEED
          END IF
          RETURN
      END
C SUB BESS.FOR
      SUBROUTINE BESS
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS USED FOR BESSEL FUNCTIONS
C
          EXTERNAL BESSJ1
C
          REAL*8 VALUE1,BESSJ1
C
          INCLUDE 'datmai.inc'
C
          IF(WC.EQ.'J1') THEN
              IF(SST.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"J1" TAKES NO QUALIFIER OR STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"J1" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1) W1=0.0D0
              IF(DABS(W1).GT.1.0D15) THEN
                  WRITE(OUTLYNE,*)'"J1" MAXIMUM ARGUMENT IS +/- 1.0D15'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              VALUE1=BESSJ1(W1)
              REG(40)=REG(9)
              REG(9)=VALUE1
          ELSE
              OUTLYNE='ONLY J1(X) HAS BEEN IMPLEMENTED'
              CALL SHOWIT(1)
          END IF
          RETURN
      END
      FUNCTION BESSJ1(X)
C
          IMPLICIT NONE
C
          REAL*8 Y,P1,P2,P3,P4,P5,Q1,Q2,Q3,Q4,Q5,
     *        R1,R2,R3,R4,R5,R6,
     *        S1,S2,S3,S4,S5,S6,X,BESSJ1,XX,Z,AX
          DATA R1,R2,R3,R4,R5,R6/72362614232.D0,-7895059235.D0,242396853.1D0
     *    ,
     *        -2972611.439D0,15704.48260D0,-30.16036606D0/,
     *        S1,S2,S3,S4,S5,S6/144725228442.D0,2300535178.D0,
     *        18583304.74D0,99447.43394D0,376.9991397D0,1.D0/
          DATA P1,P2,P3,P4,P5/1.D0,.183105D-2,-.3516396496D-4,.2457520174D-5
     *    ,
     *        -.240337019D-6/, Q1,Q2,Q3,Q4,Q5/.04687499995D0,-.2002690873D-3
     *    ,
     *        .8449199096D-5,-.88228987D-6,.105787412D-6/
          IF(DABS(X).LT.8.0D0)THEN
              Y=X**2
              BESSJ1=X*(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6)))))
     *            /(S1+Y*(S2+Y*(S3+Y*(S4+Y*(S5+Y*S6)))))
          ELSE
              AX=DABS(X)
              Z=8.0D0/AX
              Y=Z**2
              XX=AX-2.356194491D0
              BESSJ1=DSQRT(.636619772D0/AX)*(DCOS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y
     *            *P5))))-Z*DSIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))
     *            *DSIGN(1.0D0,X)
          ENDIF
          RETURN
      END

C SUB BLANK.FOR
      SUBROUTINE BLANK0
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS RESPONSE TO BLANK INPUT
C
          INCLUDE 'datmai.inc'
C
          IF(IN.EQ.5) THEN
              IF(F1.EQ.1) THEN
                  OUTLYNE='CMD LEVEL'
                  CALL SHOWIT(1)
              END IF
              IF(F2.EQ.1.AND.F1.EQ.0) THEN
                  OUTLYNE='MACRO INPUT LEVEL'
                  CALL SHOWIT(1)
              END IF
              IF(F3.EQ.1.AND.F1.EQ.0) THEN
                  OUTLYNE='MACRO EDIT LEVEL'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F5.EQ.1) THEN
                  OUTLYNE='LENS INPUT LEVEL'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F6.EQ.1) THEN
                  OUTLYNE='LENS UPDATE LEVEL'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F7.EQ.1) THEN
                  OUTLYNE='SPSRF INPUT LEVEL'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F8.EQ.1) THEN
                  OUTLYNE='SPSRF UPDATE LEVEL'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F9.EQ.1) THEN
                  OUTLYNE='SPFIT LEVEL'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F10.EQ.1) THEN
                  OUTLYNE='CONFIGS INPUT LEVEL'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.0.AND.F11.EQ.1) THEN
                  OUTLYNE='CONFIGS UPDATE LEVEL'
                  CALL SHOWIT(1)
              END IF
              IF(F1.EQ.1.AND.F17.EQ.1.AND.F18.EQ.0) THEN
                  OUTLYNE='SPECT LEVEL'
              END IF
              IF(F1.EQ.1.AND.F17.EQ.1.AND.F18.EQ.1) THEN
                  OUTLYNE='SPECT-TABLE INPUT LEVEL'
                  CALL SHOWIT(1)
              END IF
              IF(F27.EQ.1) THEN
                  OUTLYNE='MERIT INPUT LEVEL (MERIT)'
                  CALL SHOWIT(1)
              END IF
              IF(F27.EQ.2) THEN
                  OUTLYNE='MERIT UPDATE LEVEL (UPDATE MERIT)'
                  CALL SHOWIT(1)
              END IF
              IF(F29.EQ.1) THEN
                  OUTLYNE='VARIABLE INPUT LEVEL (VARIABLE)'
                  CALL SHOWIT(1)
              END IF
              IF(F29.EQ.2) THEN
                  OUTLYNE='VARIABLE UPDATE LEVEL (UPDATE VARIABLE)'
                  CALL SHOWIT(1)
              END IF
              IF(F51.EQ.1) THEN
                  OUTLYNE='TVAR INPUT LEVEL (TVAR)'
                  CALL SHOWIT(1)
              END IF
              IF(F51.EQ.2) THEN
                  OUTLYNE='TVAR UPDATE LEVEL (UPDATE TVAR)'
                  CALL SHOWIT(1)
              END IF
              IF(F52.EQ.1) THEN
                  OUTLYNE='COMPVAR INPUT LEVEL (COMPVAR)'
                  CALL SHOWIT(1)
              END IF
              IF(F52.EQ.2) THEN
                  OUTLYNE='COMPVAR UPDATE LEVEL (UPDATE COMPVAR)'
                  CALL SHOWIT(1)
              END IF
              IF(F53.EQ.1) THEN
                  OUTLYNE='TOPER INPUT LEVEL (TOPER)'
                  CALL SHOWIT(1)
              END IF
              IF(F53.EQ.2) THEN
                  OUTLYNE='TOPER UPDATE LEVEL (UPDATE TOPER)'
                  CALL SHOWIT(1)
              END IF
              IF(F54.EQ.1) THEN
                  OUTLYNE='FOCRIT INPUT LEVEL (FOCRIT)'
                  CALL SHOWIT(1)
              END IF
              IF(F54.EQ.2) THEN
                  OUTLYNE='FOCRIT UPDATE LEVEL (UPDATE FOCRIT)'
                  CALL SHOWIT(1)
              END IF
          ELSE
C     IN NOT 5, NO PRINTED RESPONSE TO A BLANK LINE
          END IF
          RETURN
      END
C SUB ECHO.FOR
      SUBROUTINE ECHO
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO IMPLIMENT THE CMD
C       LEVEL COMMAND ECHO (ON/OFF/ OR A QUERRY AS TO ECHO STATUS)
C
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"ECHO" ONLY TAKES QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(SQ.EQ.0) THEN
              IF(ECH.EQ.0) THEN
                  OUTLYNE='ECHO IS OFF'
                  CALL SHOWIT(1)
              END IF
              IF(ECH.EQ.1) THEN
                  OUTLYNE='ECHO IS ON'
                  CALL SHOWIT(1)
              END IF
              RETURN
          ELSE
C       SQ NOT 0
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  OUTLYNE='INVALID QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WQ.EQ.'ON') ECH=1
              IF(WQ.EQ.'OFF') ECH=0
          END IF
          RETURN
      END
C SUB EJECT.FOR
      SUBROUTINE EJECT
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE PRODUCES A PAGE EJECT TO THE
C       TOP OF THE NEXT PAGE FOR THE CURRENT OUTPUT
C       DEVICE IF AND ONLY IF THE OUTPUT DEVICE IS
C       UNIT 7 (LINE PRINTER- PRINTER)
C
          INTEGER OLDOUT
C
          LOGICAL OPEN7
C
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"EJECT" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(OUT.EQ.7) THEN
              OPEN7=.FALSE.
              INQUIRE(FILE=trim(HOME)//'PRINTER.TXT',OPENED=OPEN7)
              IF(.NOT.OPEN7) THEN
                  OLDOUT=0
                  IF(APPEND)
     1               OPEN(UNIT=7,ACCESS='APPEND',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//'PRINTER.TXT'
     1              ,STATUS='UNKNOWN')
                  IF(.NOT.APPEND)
     1               OPEN(UNIT=7,ACCESS='SEQUENTIAL',BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//'PRINTER.TXT'
     1              ,STATUS='UNKNOWN')
              ELSE
C       UNIT 7 WAS OPEN
                  OLDOUT=1
C       PROCEED
              END IF
              WRITE(OUTLYNE,1000) CHAR(12)
              CALL SHOWIT(0)
              IF(OLDOUT.EQ.0) CALL CLOSE_FILE(7,1)
          ELSE
C       UNIT NOT 7, DON'T DO ANYTHING, JUST RETURN.
          END IF
 1000     FORMAT(A2)
          RETURN
      END

C SUB EXITT.FOR
      SUBROUTINE EXITT(CLSCODE)
          USE GLOBALS
C
          IMPLICIT NONE
C
          INTEGER CLSCODE,ALLOERR
C
          INTEGER IEND,OLDOUT,OLDSQ,IPASS1,IPASS2,I
C
          CHARACTER WCOLD*8,AI4*4
C
          REAL*8 SYS50,SYS56,RPASS1,RPASS2,RPASS3
C
          LOGICAL OPEN63
     1    ,OPEN32,EXIS32,EXIS63
     2    ,EXIS70,EXIS72,EXIS94,EXIS95,EXIS96,LPASS1,LPASS2

          LOGICAL ITERROR
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datmac.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"EXIT" OR "EXI" TAKES NO EXPLICIT INPUT'
              CALL SHOWIT(5)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(5)
              CALL MACFAL
              RETURN
          END IF
C       THIS SUBROUTINE EXITS TO THE COMPUTER OPERATING SYSTEM.
C       ANY ACTIONS REQUIRED SUCH AS CLOSING FILES,
C       REWINDING FILES,WRITING DATA ECT. OCCUR HERE.
C       AFTER THESE ACTIONS ARE TAKEN,
C       PROGRAM EXECUTION IS TERMINATED AND THE
C       USER IS PLACED BACK AT THE COMPUTER SYSYTEM
C       LEVEL.
C
C       EXIT CAN ONLY OCCUR FROM THE CMD LEVEL OF
C       THE PROGRAM. I.E. F1=1
C
C       PLACE COMMANDS OR SUBROUTINE CALLS HERE TO
C       PERFORM CMDS WHICH MUST BE ACCOMPLISHED
C       PRIOR TO PROGRAM TERMINATION.
C
          IF(PLEXIS) CALL PSTOP
          IF(DXFEXIS) THEN
              CALL DXC()
              LAYER='LAYER001'
              DXFEXIS=.FALSE.
              DXFSET=.FALSE.
              DEVTYP=0
          END IF
          OPEN32=.FALSE.
          EXIS32=.FALSE.
          EXIS72=.FALSE.
          EXIS70=.FALSE.
          EXIS95=.FALSE.
          EXIS94=.FALSE.
          EXIS96=.FALSE.
          INQUIRE(FILE=LIBSPO//'SPOTS.DAT',OPENED=OPEN32)
          INQUIRE(FILE=LIBSPO//'SPOTS.DAT',EXIST=EXIS32)
          INQUIRE(FILE=trim(HOME)//'APMAP.DAT',EXIST=EXIS72)
          INQUIRE(FILE=trim(HOME)//'OPDMAP.DAT',EXIST=EXIS70)
          INQUIRE(FILE=trim(HOME)//'FOOT1.DAT',EXIST=EXIS94)
          INQUIRE(FILE=trim(HOME)//'QUIET.DAT',EXIST=EXIS96)
          CALL CLOSE_FILE(7,1)
          CALL CLOSE_FILE(8,1)
          CALL CLOSE_FILE(9,1)
          CALL CLOSE_FILE(10,1)
          CALL CLOSE_FILE(30,1)
          CALL CLOSE_FILE(80,1)
          CALL CLOSE_FILE(81,1)
          IF(EXIS94) THEN
              OPEN(UNIT=94,ACCESS='DIRECT'
     1        ,FORM='UNFORMATTED',FILE=trim(HOME)//'FOOT1.DAT',RECL=(80*NRECL)
     2        ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(94,0)
          END IF
          OPEN32=.FALSE.
          OPEN63=.FALSE.
          EXIS32=.FALSE.
          EXIS63=.FALSE.
          IF(EXIS32) THEN
              OPEN(UNIT=32,ACCESS='DIRECT',FILE=LIBSPO//'SPOTS.DAT',
     1        FORM='UNFORMATTED',RECL=(110*NRECL),STATUS='UNKNOWN')
              CALL CLOSE_FILE(32,0)
          END IF
          IF(EXIS72) THEN
              OPEN(UNIT=72,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'APMAP.DAT'
     2        ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(72,0)
          END IF
          IF(EXIS70) THEN
              OPEN(UNIT=70,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'OPDMAP.DAT'
     2        ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(70,0)
          END IF
          IF(EXIS96) THEN
              OPEN(UNIT=96,
     1        FILE=trim(HOME)//'QUIET.DAT'
     2        ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(96,0)
          END IF
          CALL CLOSE_FILE(95,1)
          CALL CLOSE_FILE(97,1)
C
C
C       DO A FORCED RETURN TO CFG1
C
          IF(F15.EQ.0.AND.F12.NE.1) CALL FRCCF1(1)
C
C       SET THE INTEGER PART OF SYSTEM1(50) EQUAL TO 1
C       SET THE INTEGER PART OF system(56) EQUAL TO THE HIGHEST
C       NON-EMPTY CONFIG.
C       INITIALIZE IEND AND SYS56
          IEND=1
          SYS56=1.0D0
C       CHECK FOR HIGHEST NON-EMPTY CONFIG
          DO 5 I=2,MAXCFG
              IF(CFGCNT(I).GT.0) IEND=I
 5        CONTINUE
C       IEND IS THE NUMBER OF THE HIGHEST NUMBERED NON-EMPTY CONFIG
          SYS56=DBLE(IEND)
          SYS50=1.0
          SYSTEM1(50)=SYS50
          SYSP(50)=SYSTEM1(50)
          SYSTEM1(56)=SYS56
          SYSP(56)=SYSTEM1(56)
C
C       DUMP THE PERMANENT LENS INTO THE ASCII STORAGE FILE
C       LENSTEXT.DATA
C
C       NOW OUTPUT LENS TO LENSTEXT.DAT
C       IF SYSP(20) NOT 0.0D0
          IF(SYSP(20).NE.0.0D0) THEN
C
              OPEN(UNIT=89,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'CURLENS/LENSTEXT.DAT'
     2        ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(89,0)

              OPEN(UNIT=89,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'CURLENS/LENSTEXT.DAT'
     2        ,STATUS='UNKNOWN')
C
              OLDOUT=OUT
              OLDSQ=SQ
              OUT=89
C     SET SQ=0
              SQ=0
              SST=0
              SN=0
              WCOLD=WC
              WC='LENO'
C     CALL LENOUT
              CALL LENOUT
              WC=WCOLD
              OUT=OLDOUT
C
              REWIND(UNIT=89)
              CALL CLOSE_FILE(89,1)
          ELSE
C       NO LENS DATA WAS AVAILABLE TO OUTPUT
          END IF
C
C     SAVE RAY AND FIELD DATA
          OPEN(UNIT=16,ACCESS='SEQUENTIAL',
     1    BLANK='NULL',FORM='FORMATTED',FILE=trim(HOME)//'FIELDS.DAT',
     1    STATUS='UNKNOWN')
 100      FORMAT(A4,1X,D15.8,1X,D15.8,1X,D15.8,1X,I2)
          DO I=1,200
              CALL ITOAAA(I,AI4)
              WRITE(UNIT=16,FMT=100) AI4,FIELDY(I),FIELDX(I),
     1        FIELDZ(I),INT(FIELDW(I))
          END DO
          CALL CLOSE_FILE(16,1)
          OPEN(UNIT=16,ACCESS='SEQUENTIAL',
     1    BLANK='NULL',FORM='FORMATTED',FILE=trim(HOME)//'RAYS.DAT',
     1    STATUS='UNKNOWN')
 102      FORMAT(A4,1X,D15.8,1X,D15.8,1X,I2)
          DO I=1,5000
              CALL ITOAAA(I,AI4)
              WRITE(UNIT=16,FMT=102) AI4,RAYY(I),RAYX(I),
     1        INT(RAYW(I))
          END DO
          CALL CLOSE_FILE(16,1)
          CALL CLOSE_FILE(11,0)
          CALL CLOSE_FILE(12,1)
          CALL CLOSE_FILE(13,0)
          CALL CLOSE_FILE(27,1)
C
          OPEN(UNIT=78,ACCESS='SEQUENTIAL',FILE=trim(HOME)//'FASTLENS.DAT',
     1    FORM='FORMATTED',STATUS='UNKNOWN')
          CALL CLOSE_FILE(78,0)
C
C     CLOSE AND SAVE THE DXF3D.DXF FILE UNIT #39
          CALL CLOSE_FILE(39,1)
C     DEALLOCATE GRID SPECIAL SURFACE MEMORY (1/7/97)
          IPASS1=1
          IPASS2=0
          LPASS1=.FALSE.
          GRIDSUNLOADED19(0:499)=.TRUE.
          GRIDSUNLOADED20(0:499)=.TRUE.
          GRIDSUNLOADED22(0:499)=.TRUE.
          CALL GRIDS(1,0,LPASS1)
          IPASS1=0
          IPASS2=1
          RPASS1=0.0D0
          RPASS2=0.0D0
          RPASS3=0.0D0
          CALL SPL23(IPASS1,RPASS1,RPASS2,RPASS3,IPASS2)
C     DEALLOCATE DEFORMABLE SURFACE MEMORY (5/11/98)
          IPASS1=1
          IPASS2=0
          LPASS1=.FALSE.
          LPASS2=.FALSE.
          CALL DEFGRIDS(1,IPASS2,LPASS1,LPASS2)
          ITERROR=.FALSE.
          CALL ITER(0,0,ITERROR)
C     CLEAR THE ALLOCATED ARRAY CAPFNIN
          WC='CAPFNCLR'
          SQ=0
          SST=0
          SN=0
          CALL OPDIN
          IF(CLSCODE.EQ.1) THEN
          END IF
          CALL CLOSE_FILE(58,0)
C
! 10                    CONTINUE
          OPEN(UNIT=28
     1    ,FILE=trim(HOME)//'NEUTRAL.DAT',FORM='UNFORMATTED',ACCESS='DIRECT'
     2    ,RECL=(NRECL*42),STATUS='UNKNOWN')
          CALL CLOSE_FILE(28,0)
          OPEN(UNIT=27
     1    ,FILE=trim(HOME)//'NEUTRERP.DAT'
     2    ,FORM='UNFORMATTED',ACCESS='DIRECT'
     3    ,RECL=(NRECL*42),STATUS='UNKNOWN')
          CALL CLOSE_FILE(27,0)
          CALL MY_DELETE_FILE('REPLAY.WMF')
          STOP
      END


C SUB HALTER.FOR
      SUBROUTINE HALTER
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmac.inc'
C
          CHARACTER INSTRC*140
          INTEGER NSTRUC
C
          COMMON/PRO22/INSTRC,NSTRUC
C
          CHARACTER*2 HX
          CHARACTER*3 KKDP
          HX='  '
          CALL INTERFACER(HX)
          IF(HX.EQ.'HX'.OR.HX.EQ.'hx'.OR.HX.EQ.'hX'.OR.HX.EQ.'Hx') THEN
C     THIS IS 'HX' BEHAVIOR
              OUTLYNE='CURRENT INTERNAL PROGRAM PROCESS TERMINATED'
              CALL SHOWIT(1)
              OUTLYNE='RETURNED TO THE CMD LEVEL'
              CALL SHOWIT(1)
C                       RESET FLAG STATUS
              INSTRC=AA//AA//AA//AA//AA//AA//AA
              NSTRUC=0
              HT=.TRUE.
              F1=1
              F2=0
              F3=0
              F4=0
              F5=0
              IF(F6.EQ.1) THEN
                  LNSTYP=1
                  CALL LNSEOS
              END IF
              F6=0
              F7=0
              IF(F8.EQ.1) THEN
                  CALL SPSEOS
              END IF
              F8=0
              F9=0
              F10=0
              F11=0
              F13=0
              F14=0
              F15=0
              F16=0
              F17=0
              F18=0
              F26=0
              F27=0
              F28=0
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
              F56=0
              F57=0
              F58=0
              F59=0
              F60=0
              F61=0
              F62=0
              F63=0
              F64=0
              F65=0
              F66=0
              F67=0
              F68=0
              F69=0
              F70=0
              F71=0
              F72=0
              F73=0
              F74=0
              F75=0
              F76=0
              F77=0
              F78=0
              F79=0
              F80=0
              F81=0
              F82=0
              F83=0
              F84=0
              F85=0
              F86=0
              F87=0
              F88=0
              F89=0
              F90=0
              F91=0
              F92=0
              F93=0
              F94=0
              F95=0
              F96=0
              F97=0
              F98=0
              F99=0
              F100=0
              DEREXT=.FALSE.
              KILOPT=.TRUE.
              SOLEXT=.FALSE.

!      CALL PUT_PROMPT(KKDP,3)
              NEST=0
              NESTER=0
              RETURN
          END IF
          IF(HX.NE.'HX') THEN
C     THIS IS A Resume Typing (PRESSED ENTER ONLY)
              HX='  '
              HT=.FALSE.
              CALL SELECTKDP(KKDP)
!      CALL PUT_PROMPT(KKDP,3)
              RETURN
          END IF
          CALL SELECTKDP(KKDP)
!      CALL PUT_PROMPT(KKDP,3)

          RETURN
      END


      SUBROUTINE INTER_PROMPT
          IMPLICIT NONE
          CHARACTER*57 LINE
          INTEGER N
          N=43
          LINE='HX(stop process) or ENTER(resume)'
!       CALL PUT_PROMPT(LINE,N)
          RETURN
      END


      SUBROUTINE INTERFACER(HX)
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          CHARACTER*2 HX
          CALL INTER_PROMPT
          CALL INTER_RESPON(HX)
          RETURN
      END


      SUBROUTINE INTER_RESPON(HX)
          IMPLICIT NONE
          CHARACTER*2 HX
          CHARACTER*80 RESPONSE
          INCLUDE 'datmai.inc'
!       CALL GET_RESPONSE(RESPONSE,80)
          HX=RESPONSE(1:2)
          RETURN
      END


C SUB HMAT.FOR
      SUBROUTINE HMAT(N,DECIM,AN,INTEG,IB)
C
          IMPLICIT NONE
C
C       THIS ROUTINE IS USED BY THE "PLOT ACC" COMMAND AT THE CMD LEVEL
C
          INTEGER DECIM,I,IB
C
          LOGICAL INTEG
C
          REAL*8 N
C
          CHARACTER B*140,AN*40
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(INTEG) THEN
              IF(INT(N).GT.999999999.OR.INT(N).LT.-999999999) THEN
                  AN='**********'
                  GO TO 100
              ELSE
                  WRITE(B,201) INT(N)
                  READ(B,200) AN
              END IF
              GO TO 100
          ELSE
C       NOT INTEGER OUTPUT
          END IF
C
C       DECIMAL OUTPUT IN G FORMAT
C
          IF(DECIM.EQ.1) THEN
              WRITE(B,101) N
              READ(B,200) AN
              GO TO 100
          ELSE
          END IF
C
C
          IF(DECIM.EQ.2) THEN
              WRITE(B,102) N
              READ(B,200) AN
              GO TO 100
          ELSE
          END IF
C
C
          IF(DECIM.EQ.3) THEN
              WRITE(B,103) N
              READ(B,200) AN
              GO TO 100
          ELSE
          END IF
C
C
          IF(DECIM.EQ.4) THEN
              WRITE(B,104) N
              READ(B,200) AN
              GO TO 100
          ELSE
          END IF
C
C
          IF(DECIM.EQ.5) THEN
              WRITE(B,105) N
              READ(B,200) AN
              GO TO 100
          ELSE
          END IF
C
C
          IF(DECIM.EQ.6) THEN
              WRITE(B,106) N
              READ(B,200) AN
              GO TO 100
          ELSE
          END IF
C
C
          IF(DECIM.EQ.7) THEN
              WRITE(B,107) N
              READ(B,200) AN
              GO TO 100
          ELSE
          END IF
C
C
          IF(DECIM.EQ.8) THEN
              WRITE(B,108) N
              READ(B,200) AN
              GO TO 100
          ELSE
          END IF
C
C
          IF(DECIM.EQ.9) THEN
              WRITE(B,109) N
              READ(B,200) AN
              GO TO 100
          ELSE
          END IF
C
C
          IF(DECIM.EQ.10) THEN
              WRITE(B,110) N
              READ(B,200) AN
              GO TO 100
          ELSE
          END IF
C
! 10             CONTINUE
 101      FORMAT(G8.1)
 102      FORMAT(G9.2)
 103      FORMAT(G10.3)
 104      FORMAT(G11.4)
 105      FORMAT(G12.5)
 106      FORMAT(G13.6)
 107      FORMAT(G14.7)
 108      FORMAT(G15.8)
 109      FORMAT(G16.9)
 110      FORMAT(G17.10)
 201      FORMAT(I10)
 200      FORMAT(A40)
 100      IB=1
          DO I=40,1,-1
              IF(AN(I:I).NE.' ') THEN
                  IB=I
                  GO TO 300
              END IF
          END DO
 300      CONTINUE
          RETURN
      END
C SUB INCR.FOR
      SUBROUTINE INCR
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO INCREMENT OR DECREMENT
C       THE CONTENTS OF A NAMED REGISTER OR THE ACCUMULATOR
C       BY THE VALUE PASSED IN W1. IF W1 IS BLANK (DEFAULT)
C       AS OPPOSED TO BEING TYPED IN AS ZERO, THEN THE
C       INCREMENT VALUE IS ASSUMED TO BE 1.0D0. THIS
C       WILL OCCUR IF DF1 IS EQUAL TO 1.
C
          CHARACTER ACCWRD*8
C
          INTEGER ACCSUB,ACCCNT,I
C
          COMMON/ACCSB/ACCWRD
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          INCLUDE 'datmai.inc'
C
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"INCR" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WQ.NE.'A'.AND.WQ.NE.'B'.AND.WQ.NE.'C'.AND.WQ.NE.'D'
     1    .AND.WQ.NE.'E'.AND.WQ.NE.'F'.AND.WQ.NE.'G'.AND.WQ.NE.'H'
     2    .AND.WQ.NE.' '.AND.WQ.NE.'ACC'.AND.WQ.NE.'X'.AND.WQ.
     3    NE.'Y'.AND.WQ.NE.'Z'.AND.WQ.NE.'IX'.AND.WQ.NE.'IY'
     4    .AND.WQ.NE.'IZ'.AND.WQ.NE.'IT'.AND.WQ.NE.'I'.AND.WQ.NE.
     5    'ITEST'.AND.WQ.NE.'JTEST'.AND.WQ.NE.'K'.AND.WQ.NE.'L'
     6    .AND.WQ.NE.'M'.AND.WQ.NE.'N'.AND.WQ.NE.'KTEST'.AND.WQ.NE.'LTEST'
     7    .AND.WQ.NE.'MTEST'.AND.WQ.NE.'NTEST'.AND.WQ.NE.'J') THEN
              WRITE(OUTLYNE,*)'INVALID REGISTER NAME'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
              IF(ACCSUB.EQ.1) THEN
                  IF(WQ.EQ.'ACC'.OR.WQ.EQ.'X'.OR.WQ.EQ.' ') THEN
                      WQ=ACCWRD
                      ACCCNT=ACCCNT-1
                      IF(ACCCNT.EQ.0) ACCSUB=0
                  END IF
              END IF
              IF(DF1.EQ.1) W1=1.0D0
              IF(WQ.EQ.'A') REG(1)=REG(1)+W1
              IF(WQ.EQ.'B') REG(2)=REG(2)+W1
              IF(WQ.EQ.'C') REG(3)=REG(3)+W1
              IF(WQ.EQ.'D') REG(4)=REG(4)+W1
              IF(WQ.EQ.'E') REG(5)=REG(5)+W1
              IF(WQ.EQ.'F') REG(6)=REG(6)+W1
              IF(WQ.EQ.'G') REG(7)=REG(7)+W1
              IF(WQ.EQ.'H') REG(8)=REG(8)+W1
              IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X') THEN
                  REG(40)=REG(9)
                  REG(9)=REG(9)+W1
              END IF
              IF(WQ.EQ.'Y') REG(10)=REG(10)+W1
              IF(WQ.EQ.'Z') REG(11)=REG(11)+W1
              IF(WQ.EQ.'T') REG(12)=REG(12)+W1
              IF(WQ.EQ.'IX') THEN
                  REG(30)=REG(13)
                  REG(13)=REG(13)+W1
              END IF
              IF(WQ.EQ.'IY') REG(14)=REG(14)+W1
              IF(WQ.EQ.'IZ') REG(15)=REG(15)+W1
              IF(WQ.EQ.'IT') REG(16)=REG(16)+W1
              IF(WQ.EQ.'I') REG(17)=REG(17)+W1
              IF(WQ.EQ.'ITEST') REG(18)=REG(18)+W1
              IF(WQ.EQ.'J') REG(19)=REG(19)+W1
              IF(WQ.EQ.'JTEST') REG(20)=REG(20)+W1
              IF(WQ.EQ.'K') REG(21)=REG(21)+W1
              IF(WQ.EQ.'L') REG(22)=REG(22)+W1
              IF(WQ.EQ.'M') REG(23)=REG(23)+W1
              IF(WQ.EQ.'N') REG(24)=REG(24)+W1
              IF(WQ.EQ.'KTEST') REG(25)=REG(25)+W1
              IF(WQ.EQ.'LTEST') REG(26)=REG(26)+W1
              IF(WQ.EQ.'MTEST') REG(27)=REG(27)+W1
              IF(WQ.EQ.'NTEST') REG(28)=REG(28)+W1
          END IF
          DO I=1,28
              IF(DABS(REG(I)).LT.1.0D-14) REG(I)=0.0D0
          END DO
          RETURN
      END


C SUB INPUTT.FOR
      SUBROUTINE INPUTT
          USE GLOBALS
C
          IMPLICIT NONE
C
          LOGICAL EXIS8,EXIS9,OPEN8,OPEN9,EXIS97
C
          INTEGER I,IREND,J,N3
C
          CHARACTER AI*3,AI4*4
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datlen.inc'

C
C       THIS SUBROUTINE IS CALLED TO SET THE CURRENT INPUT DEVICE.
C
C
          IF(SST.EQ.1.AND.WQ.NE.'FILE    ') THEN
              OUTLYNE='"INPUT" ONLY TAKES QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              OUTLYNE='WHEN THE QUALIFIER WORD IS NOT "FILE"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.0.AND.WQ.EQ.'FILE    ') THEN
              OUTLYNE='"INPUT FILE" REQUIRES A FILE NAME AS STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.1) THEN
              OUTLYNE='"INPUT" TAKES NO NUMERIC INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C       VALID QUALIFIER WORDS ARE:
C
C               TP = KEYBOARD = 5 OR 96
C               CR = CARDTEXT.DAT = 8 (WAS CP ON OUTPUT)
C               ED = EDITTEXT.DAT = 9
C               ED = EDITTEXT.DAT = 9
C               FILE = OFILN = 97
C       THE TRAILING NUMBERS ARE THE DEVICE UNIT NUMBERS FOR EACH
C       DEVICE.
          IF(WQ.NE.'ED'.AND.WQ.NE.'CR'.AND.WQ.NE.'TP'.AND.
     1    SQ.NE.0.AND.WQ.NE.'FILE    ') THEN
              OUTLYNE='INVALID INPUT DEVICE NAME'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'FILE    ')
     1    LASTFIL=OFILN
          IF(WQ.EQ.'FILE    ')
     1    OFILN=WS(1:80)
          IF(WQ.NE.'FILE'.AND.SST.NE.0) THEN
              OUTLYNE='"INPUT FILE" REQUIRES A FILE NAME AS STRING INPUT'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C                IF SQ.EQ.0
C HANDEL PRINTING OF NAME OF CURRENT DEVICE
          IF(SQ.EQ.0.AND.IN.EQ.5.OR.IN.EQ.5.AND.STI.EQ.1) THEN
              OUTLYNE='INPUT IS "TP"'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.IN.EQ.96.OR.IN.EQ.96.AND.STI.EQ.1) THEN
              OUTLYNE='INPUT IS "TP"'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.IN.EQ.8.OR.IN.EQ.8.AND.STI.EQ.1) THEN
              OUTLYNE='INPUT IS "CP"'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.IN.EQ.9.OR.IN.EQ.9.AND.STI.EQ.1) THEN
              OUTLYNE='INPUT IS "ED"'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.IN.EQ.97.OR.IN.EQ.97.AND.STI.EQ.1) THEN
              OUTLYNE='INPUT IS "FILE" NAMED '//OFILN
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WQ.EQ.'TP') THEN
              IN=5
              RETURN
          END IF
          IF(WQ.EQ.'CR') THEN
              EXIS8=.FALSE.
              INQUIRE(FILE=trim(HOME)//'CARDTEXT.DAT',EXIST=EXIS8)
              IF(.NOT.EXIS8) THEN
                  OUTLYNE='NO CARDTEXT.DAT FILE EXISTS TO READ'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IN=8
              OPEN8=.FALSE.
              INQUIRE(FILE=trim(HOME)//'CARDTEXT.DAT',
     1        OPENED=OPEN8)
              IF(.NOT.OPEN8) THEN
                  IF(APPEND) OPEN(UNIT=8,ACCESS='APPEND',BLANK='NULL'
     1            ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2            ,STATUS='UNKNOWN')
                  IF(.NOT.APPEND) OPEN(UNIT=8,ACCESS='SEQUENTIAL',BLANK='NULL'
     1            ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2            ,STATUS='UNKNOWN')
              ELSE
              END IF
              IF(F4.EQ.1) THEN
C     IN A MACRO, READ THE DATA
                  REWIND(UNIT=IN)
                  DO I=1,99999
                      IF(F4.EQ.0) RETURN
                      READ(UNIT=IN,FMT=100,END=9999,ERR=9999) INPUT
                      IF(INPUT(1:3).NE.'OUT') THEN
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:8).EQ.'FLDSRAYS') THEN
                              IREND=500
                              IF(INPUT(10:13).EQ.' 500') IREND=500
                              IF(INPUT(10:13).EQ.'1000') IREND=1000
                              IF(INPUT(10:13).EQ.'1500') IREND=1500
                              IF(INPUT(10:13).EQ.'2000') IREND=2000
                              IF(INPUT(10:13).EQ.'2500') IREND=2500
                              IF(INPUT(10:13).EQ.'3000') IREND=3000
                              IF(INPUT(10:13).EQ.'3500') IREND=3500
                              IF(INPUT(10:13).EQ.'4000') IREND=4000
                              IF(INPUT(10:13).EQ.'4500') IREND=4500
                              IF(INPUT(10:13).EQ.'5000') IREND=5000
                              DO J=1,200
                                  READ(IN,*) AI,FIELDY(I),FIELDX(I),
     1                            FIELDZ(I),N3
                                  FIELDW(I)=DBLE(N3)
                                  IF(FIELDW(I).EQ.0.0D0) THEN
                                      FIELDW(I)=SYSTEM1(11)
                                  END IF
                              END DO
                              DO J=1,IREND
                                  READ(IN,*,ERR=9999,END=9999) AI4,RAYY(I),RAYX(I),N3
                                  RAYW(I)=DBLE(N3)
                                  IF(RAYW(I).EQ.0.0D0) THEN
                                      IF(I.GE.1.AND.I.LE.41) RAYW(I)=SYSTEM1(11)
                                      IF(I.GE.42.AND.I.LE.82) RAYW(I)=SYSTEM1(7)
                                      IF(I.GE.83.AND.I.LE.123) RAYW(I)=SYSTEM1(8)
                                  END IF
                              END DO
                          ELSE
                          END IF
                          IF(INPUT(1:1).EQ.'.'.OR.
     1                    INPUT(1:1).EQ.'1'.OR.
     1                    INPUT(1:1).EQ.'2'.OR.
     1                    INPUT(1:1).EQ.'3'.OR.
     1                    INPUT(1:1).EQ.'4'.OR.
     1                    INPUT(1:1).EQ.'5'.OR.
     1                    INPUT(1:1).EQ.'6'.OR.
     1                    INPUT(1:1).EQ.'7'.OR.
     1                    INPUT(1:1).EQ.'8'.OR.
     1                    INPUT(1:1).EQ.'9'.OR.
     1                    INPUT(1:1).EQ.'0'
     1                    .OR.INPUT(1:8).EQ.'FLDSRAYS') THEN
                          ELSE
                              IF(IN.NE.9) CALL PROCES
                              MULTICOM=.TRUE.
                              IF(IN.EQ.9) CALL PROCES
                              MULTICOM=.FALSE.
                          END IF
                          IF(F4.EQ.0) RETURN
                      END IF
                  END DO
 9999             BACKSPACE(UNIT=IN)
                  BACKSPACE(UNIT=IN)
                  REWIND (UNIT=IN)
                  CLOSE (UNIT=IN)
                  RETURN
              ELSE
C     NOT IN A MACRO
              END IF
              RETURN
          ELSE
C     NOT CR
          END IF
C
          IF(WQ.EQ.'FILE') THEN
              EXIS97=.FALSE.
              INQUIRE(FILE=trim(OFILN),EXIST=EXIS97)
              IF(.NOT.EXIS97) THEN
                  OUTLYNE='FILE '//OFILN//' DOES NOT EXIST TO READ'

                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  OFILN='            '
                  CALL MACFAL
                  RETURN
              END IF
              IN=97
              CALL CLOSE_FILE(96,1)
              CALL CLOSE_FILE(97,1)
              IF(APPEND) OPEN(UNIT=97,ACCESS='APPEND',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(OFILN)
     2        ,STATUS='UNKNOWN')
              IF(.NOT.APPEND) OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(OFILN)
     2        ,STATUS='UNKNOWN')
              IF(F4.EQ.1) THEN
C     IN A MACRO, READ THE DATA
                  REWIND(UNIT=IN)
                  DO I=1,99999
                      IF(F4.EQ.0) RETURN
                      READ(UNIT=IN,FMT=100,END=9991,ERR=9991) INPUT
                      IF(INPUT(1:3).NE.'OUT'.OR.F2.EQ.1.OR.F3.EQ.1.OR.F4.EQ.1) THEN
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:8).EQ.'FLDSRAYS') THEN
                              IREND=500
                              IF(INPUT(10:13).EQ.' 500') IREND=500
                              IF(INPUT(10:13).EQ.'1000') IREND=1000
                              IF(INPUT(10:13).EQ.'1500') IREND=1500
                              IF(INPUT(10:13).EQ.'2000') IREND=2000
                              IF(INPUT(10:13).EQ.'2500') IREND=2500
                              IF(INPUT(10:13).EQ.'3000') IREND=3000
                              IF(INPUT(10:13).EQ.'3500') IREND=3500
                              IF(INPUT(10:13).EQ.'4000') IREND=4000
                              IF(INPUT(10:13).EQ.'4500') IREND=4500
                              IF(INPUT(10:13).EQ.'5000') IREND=5000
                              DO J=1,200
                                  READ(IN,*) AI,FIELDY(I),FIELDX(I),
     1                            FIELDZ(I),N3
                                  FIELDW(I)=DBLE(N3)
                                  IF(FIELDW(I).EQ.0.0D0) THEN
                                      FIELDW(I)=SYSTEM1(11)
                                  END IF
                              END DO
                              DO J=1,IREND
                                  READ(IN,*,ERR=9991,END=9991) AI4,RAYY(I),RAYX(I),N3
                                  RAYW(I)=DBLE(N3)
                                  IF(RAYW(I).EQ.0.0D0) THEN
                                      IF(I.GE.1.AND.I.LE.41) RAYW(I)=SYSTEM1(11)
                                      IF(I.GE.42.AND.I.LE.82) RAYW(I)=SYSTEM1(7)
                                      IF(I.GE.83.AND.I.LE.123) RAYW(I)=SYSTEM1(8)
                                  END IF
                              END DO
                          ELSE
                          END IF
                          IF(INPUT(1:1).EQ.'.'.OR.
     1                    INPUT(1:1).EQ.'1'.OR.
     1                    INPUT(1:1).EQ.'2'.OR.
     1                    INPUT(1:1).EQ.'3'.OR.
     1                    INPUT(1:1).EQ.'4'.OR.
     1                    INPUT(1:1).EQ.'5'.OR.
     1                    INPUT(1:1).EQ.'6'.OR.
     1                    INPUT(1:1).EQ.'7'.OR.
     1                    INPUT(1:1).EQ.'8'.OR.
     1                    INPUT(1:1).EQ.'9'.OR.
     1                    INPUT(1:1).EQ.'0'
     1                    .OR.INPUT(1:8).EQ.'FLDSRAYS') THEN
                          ELSE
                              IF(IN.NE.9) CALL PROCES
                              MULTICOM=.TRUE.
                              IF(IN.EQ.9) CALL PROCES
                              MULTICOM=.FALSE.
                          END IF
                          IF(F4.EQ.0) RETURN
                          IF(IN.NE.9) CALL PROCES
                          MULTICOM=.TRUE.
                          IF(IN.EQ.9) CALL PROCES
                          MULTICOM=.FALSE.
                      END IF
                  END DO
 9991             BACKSPACE(UNIT=IN)
                  BACKSPACE(UNIT=IN)
                  REWIND (UNIT=IN)
                  CALL CLOSE_FILE(IN,1)
                  IN=5
                  INPUT='IN TP'
                  IF(IN.NE.9) CALL PROCES
                  MULTICOM=.TRUE.
                  IF(IN.EQ.9) CALL PROCES
                  MULTICOM=.FALSE.
                  RETURN
              ELSE
C     NOT IN A MACRO
C     NOT IN A MACRO, READ THE DATA
                  REWIND(UNIT=IN)
                  DO I=1,99999
                      READ(UNIT=IN,FMT=100,END=9992,ERR=9992) INPUT
                      IF(INPUT(1:3).NE.'OUT'.OR.F2.EQ.1.OR.F3.EQ.1.OR.F4.EQ.1) THEN
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
                          IF(INPUT(1:8).EQ.'FLDSRAYS') THEN
                              IREND=500
                              IF(INPUT(10:13).EQ.' 500') IREND=500
                              IF(INPUT(10:13).EQ.'1000') IREND=1000
                              IF(INPUT(10:13).EQ.'1500') IREND=1500
                              IF(INPUT(10:13).EQ.'2000') IREND=2000
                              IF(INPUT(10:13).EQ.'2500') IREND=2500
                              IF(INPUT(10:13).EQ.'3000') IREND=3000
                              IF(INPUT(10:13).EQ.'3500') IREND=3500
                              IF(INPUT(10:13).EQ.'4000') IREND=4000
                              IF(INPUT(10:13).EQ.'4500') IREND=4500
                              IF(INPUT(10:13).EQ.'5000') IREND=5000
                              DO J=1,200
                                  READ(IN,*) AI,FIELDY(I),FIELDX(I),
     1                            FIELDZ(I),N3
                                  FIELDW(I)=DBLE(N3)
                                  IF(FIELDW(I).EQ.0.0D0) THEN
                                      FIELDW(I)=SYSTEM1(11)
                                  END IF
                              END DO
                              DO J=1,IREND
                                  READ(IN,*,ERR=9992,END=9992) AI4,RAYY(I),RAYX(I),N3
                                  RAYW(I)=DBLE(N3)
                                  IF(RAYW(I).EQ.0.0D0) THEN
                                      IF(I.GE.1.AND.I.LE.41) RAYW(I)=SYSTEM1(11)
                                      IF(I.GE.42.AND.I.LE.82) RAYW(I)=SYSTEM1(7)
                                      IF(I.GE.83.AND.I.LE.123) RAYW(I)=SYSTEM1(8)
                                  END IF
                              END DO
                          ELSE
                          END IF
                          IF(INPUT(1:1).EQ.'.'.OR.
     1                    INPUT(1:1).EQ.'1'.OR.
     1                    INPUT(1:1).EQ.'2'.OR.
     1                    INPUT(1:1).EQ.'3'.OR.
     1                    INPUT(1:1).EQ.'4'.OR.
     1                    INPUT(1:1).EQ.'5'.OR.
     1                    INPUT(1:1).EQ.'6'.OR.
     1                    INPUT(1:1).EQ.'7'.OR.
     1                    INPUT(1:1).EQ.'8'.OR.
     1                    INPUT(1:1).EQ.'9'.OR.
     1                    INPUT(1:1).EQ.'0'
     1                    .OR.INPUT(1:8).EQ.'FLDSRAYS') THEN
                          ELSE
                              IF(IN.NE.9) CALL PROCES
                              MULTICOM=.TRUE.
                              IF(IN.EQ.9) CALL PROCES
                              MULTICOM=.FALSE.
                          END IF
                      END IF
                  END DO
 9992             BACKSPACE(UNIT=IN)
                  BACKSPACE(UNIT=IN)
                  REWIND (UNIT=IN)
                  CALL CLOSE_FILE(IN,1)
                  IN=5
                  INPUT='IN TP'
                  IF(IN.NE.9) CALL PROCES
                  MULTICOM=.TRUE.
                  IF(IN.EQ.9) CALL PROCES
                  MULTICOM=.FALSE.
              END IF
              RETURN
          ELSE
C     NOT FILE
          END IF
C
C
C
          IF(WQ.EQ.'ED') THEN
              EXIS9=.FALSE.
              INQUIRE(FILE=trim(HOME)//'EDITTEXT.DAT',EXIST=EXIS9)
              IF(.NOT.EXIS9) THEN
                  OUTLYNE='NO EDITTEXT.DAT FILE EXISTS TO READ'
                  CALL SHOWIT(1)
                  OUTLYNE='NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IN=9
              OPEN9=.FALSE.
              INQUIRE(FILE=trim(HOME)//'EDITTEXT.DAT',OPENED=OPEN9)
              IF(.NOT.OPEN9) THEN
                  IF(APPEND) OPEN(UNIT=9,ACCESS='APPEND',BLANK='NULL'
     1            ,FORM='FORMATTED',FILE=trim(HOME)//'EDITTEXT.DAT'
     2            ,STATUS='UNKNOWN')
                  IF(.NOT.APPEND) OPEN(UNIT=9,ACCESS='SEQUENTIAL',BLANK='NULL'
     1            ,FORM='FORMATTED',FILE=trim(HOME)//'EDITTEXT.DAT'
     2            ,STATUS='UNKNOWN')
              ELSE
              END IF
              IF(F4.EQ.1) THEN
C     IN A MACRO, READ THE DATA
                  REWIND(UNIT=IN)
                  DO I=1,99999
                      IF(F4.EQ.0) RETURN
                      READ(UNIT=IN,FMT=100,END=9998,ERR=9998) INPUT
                      IF(F4.EQ.0) RETURN
                      IF(IN.NE.9) CALL PROCES
                      MULTICOM=.TRUE.
                      IF(IN.EQ.9) CALL PROCES
                      MULTICOM=.FALSE.
                  END DO
 9998             BACKSPACE(UNIT=IN)
                  BACKSPACE(UNIT=IN)
                  REWIND (UNIT=IN)
                  CLOSE (UNIT=IN)
                  IN=5
                  INPUT='IN TP'
                  IF(IN.NE.9) CALL PROCES
                  MULTICOM=.TRUE.
                  IF(IN.EQ.9) CALL PROCES
                  MULTICOM=.FALSE.
                  RETURN
              ELSE
C     NOT IN A MACRO
              END IF
              RETURN
          ELSE
C     NOT ED
          END IF
          RETURN
 100      FORMAT(A140)
      END


C SUB STOAX.FOR
      SUBROUTINE STOAX
C
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE=
     1        '"STOAX" PLACES THE LAST PROMPT READ STRING INTO'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'THE ALPHANUMERIC STORAGE REGISTER DESIGNATED BY'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'NUMERIC WORD #1. VALID VALUES ARE 1 TO 400'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              OUTLYNE=
     1        '"STOAX" ONLY TAKES NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0) THEN
              OUTLYNE=
     1        '"STOAX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LT.1.0D0.OR.W1.GT.400.0D0) THEN
              OUTLYNE=
     1        '"STOAX" REQUIRES NUMERIC WORD #1 INPUT TO BE IN THE RANGE'
              CALL SHOWIT(1)
              OUTLYNE=
     1        '"1 TO 400'
              CALL SHOWIT(1)
              OUTLYNE=
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          AGPREG(INT(W1))(1:80)=AGPREG(0)(1:80)
          RETURN
      END
C SUB MACATON.FOR
      SUBROUTINE MACATON
C
          IMPLICIT NONE
C
          CHARACTER AVAL*23
C
          REAL*8 VAL
C
          LOGICAL ERROR1,READERR
C
          COMMON/ERRREAD/READERR
C
          INCLUDE 'datmai.inc'
C
          AVAL=AGPREG(0)(1:23)
          ERROR1=.FALSE.
          READERR=.FALSE.
          CALL ATODMC(AVAL,VAL,ERROR1)
          IF(ERROR1) THEN
              READERR=.TRUE.
              OUTLYNE='THE AX-REGISTER VALUE COULD NOT BE CONVERTED'
              CALL SHOWIT(1)
              OUTLYNE='TO A NUMBER.'
              CALL SHOWIT(1)
              OUTLYNE='THE X-REGISTER VALUE WAS NOT CHANGED.'
              CALL SHOWIT(1)
              RETURN
          ELSE
              REG(40)=REG(9)
              REG(9)=VAL
          END IF
          RETURN
      END
C SUB PREAD.FOR
      SUBROUTINE PREAD
C
          IMPLICIT NONE
C
          CHARACTER ASTRING*80,PMTVAL*80,KKDP*3
C
          INTEGER I,IV,J
C
          COMMON/TELPRM/PMTVAL
C
          LOGICAL LBLHT
C
          COMMON/JKA/LBLHT
C
          INCLUDE 'datmai.inc'
C
C       PROMPTED INPUT
C
          CALL CBREAK
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"PREAD" INITIATES A PROMPTED STRING READ'
              CALL SHOWIT(1)
              OUTLYNE='FROM THE KEYBOARD'
              CALL SHOWIT(1)
              RETURN
          END IF
          IV=0
          DO I=80,1,-1
              IF(PMTVAL(I:I).NE.' ') THEN
                  IV=I
                  GO TO 50
              END IF
          END DO
 50       CONTINUE
          IF(IV.GT.20) IV=20
          IF(IV.EQ.0.OR.IV.GT.20)  THEN

!      CALL PUT_PROMPT('INPUT ',6)
!      CALL GET_RESPONSE(ASTRING,80)
              CALL SELECTKDP(KKDP)
!      CALL PUT_PROMPT(KKDP,3)
          ENDIF
          IF(IV.LE.20.AND.IV.NE.0)  THEN
              IF(IV.LT.10) THEN
!      CALL PUT_PROMPT('INPUT '//PMTVAL(1:IV)//' ',(IV+7))
              ELSE
!      CALL PUT_PROMPT('INPUT '//PMTVAL(1:IV)//' ',(IV+7))
              END IF
!      CALL GET_RESPONSE(ASTRING,80)
              CALL SELECTKDP(KKDP)
!      CALL PUT_PROMPT(KKDP,3)

          END IF
          DO I=1,80
              J=ICHAR(ASTRING(I:I))
              IF(J.GE.97.AND.J.LE.122)
     1               ASTRING(I:I)=CHAR(J-32)
          END DO
          AGPREG(0)(1:80)=ASTRING(1:80)
          RETURN
      END
C SUB INTER.FOR
      SUBROUTINE INTER
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS INTERPOLATION OF A VALUE
C       IN SPECT.
C
          INTEGER I,J
C
          CHARACTER DDATE*10,TTIME*8,FN*10,NM*8,TTTIM*8,DDDAT*10
C
          INTEGER POINTS,OCC,CTAB
C
          REAL*8 DATA1,DATA2,TABLE(1:1001,1:3),
     1    VALUE1,SLOPE
C
          COMMON/STRNGR/DDDAT,TTTIM,NM,FN
C
          INTEGER NF
C
          LOGICAL EXISJK
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
C
C       INTERPOLATE OR EXTRAPOLATE AS APPROPRIATE.
C
C       DOES W1 EQUAL AN EXISTING WAVELENGTH VALUE?
          DO 100 I=1,POINTS
              IF(W1.EQ.TABLE(I,2)) THEN
                  VALUE1=TABLE(I,3)
                  REG(40)=REG(9)
                  REG(9)=VALUE1
                  WRITE(OUTLYNE,1000) W1
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) VALUE1
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000)
                  CALL SHOWIT(0)
                  RETURN
              END IF
 100      CONTINUE
C
C       KEEP GOING, IT WAS NOT AND EXISTING VALUE.
C       IS IT AN EXTRAPOLATION AT LOW END?
          IF(W1.LT.TABLE(1,2)) THEN
              SLOPE=(TABLE(2,3)-TABLE(1,3))/(TABLE(2,2)-TABLE(1,2))
              VALUE1=((SLOPE)*(W1-TABLE(1,2)))+TABLE(1,3)
              REG(40)=REG(9)
              REG(9)=VALUE1
              WRITE(OUTLYNE,1000) W1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000) VALUE1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,3000)
              CALL SHOWIT(0)
              RETURN
          END IF
C       IS IT AN EXTRAPOLATION AT HIGH END?
          IF(W1.GT.TABLE(POINTS,2)) THEN
              SLOPE=(TABLE(POINTS,3)-TABLE((POINTS-1),3))/
     1        (TABLE(POINTS,2)-TABLE((POINTS-1),2))
              VALUE1=((SLOPE)*(W1-TABLE(POINTS,2)))+TABLE(POINTS,3)
              REG(40)=REG(9)
              REG(9)=VALUE1
              WRITE(OUTLYNE,1000) W1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,2000) VALUE1
              CALL SHOWIT(0)
              WRITE(OUTLYNE,3000)
              CALL SHOWIT(0)
              RETURN
          END IF
C       IS IT AN INTERPOLATION
          DO 400 I=1,(POINTS-1)
              IF(TABLE(I,2).LT.W1.AND.TABLE((I+1),2).GT.W1) THEN
C       INTERPOLATE IN THIS SPACE
                  SLOPE=(TABLE((I+1),3)-TABLE(I,3))/
     1            (TABLE((I+1),2)-TABLE(I,2))
                  VALUE1=((SLOPE)*(W1-TABLE(I,2)))+TABLE(I,3)
                  REG(40)=REG(9)
                  REG(9)=VALUE1
                  WRITE(OUTLYNE,1000) W1
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,2000) VALUE1
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,3000)
                  CALL SHOWIT(0)
                  RETURN
              END IF
 400      CONTINUE
          RETURN
 1000     FORMAT('FOR A WAVELENGTH OF ( ',G18.10,' ) MICRONS')
 2000     FORMAT('THE FUNCTIONAL VALUE IS ( ',G18.10,' )')
 3000     FORMAT('THIS VALUE HAS BEEN STORED IN THE X-REGISTER')
      END
C SUB INTRPP.FOR
      SUBROUTINE INTRPP
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS INTERP AND DOES THE LINEAR AND LARGRANGE
C       THREE POINT INTERPOLATION FROM THE CMD LEVEL.
C
          INTEGER FLAG1,FLAG2,FLAG3,FLAG4
C
          REAL*8 X1,X2,X3,Y1,Y2,Y3,X4,Y4,VALUE1,L1,L2,L3,L4,
     1    XMIN,XMAX
C
          INCLUDE 'datmai.inc'

          FLAG1=0
          FLAG2=0
          FLAG3=0
          FLAG4=0
          X1=0
          X2=0
          X3=0
          X4=0
          Y1=0
          Y2=0
          Y3=0
          Y4=0

C       DATA INPUT
C
          IF(WC.EQ.'X1Y1='.OR.WC.EQ.'X2Y2='.OR.
     1    WC.EQ.'X3Y3='.OR.WC.EQ.'X4Y4=') THEN
C
C       CHECK FOR WRONG INPUT
              IF(SQ.EQ.1.OR.SST.EQ.1.OR.DF3.EQ.0.OR.DF4.EQ.4.OR.
     1        DF5.EQ.0) THEN
                  WRITE(OUTLYNE,*)WC(1:5),' ONLY USES NUMERIC WORDS 1 AND 2'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC(1:5),' REQUIRES EXPLICIT NUMERIC WORDS 1 AND 2'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       PROCEED PROCESSING DATA
              IF(WC.EQ.'X1Y1=') THEN
                  X1=W1
                  Y1=W2
                  FLAG1=1
                  FLAG2=0
                  FLAG3=0
                  FLAG4=0
                  RETURN
              END IF
              IF(WC.EQ.'X2Y2=') THEN
                  X2=W1
                  Y2=W2
                  FLAG2=1
                  FLAG3=0
                  FLAG4=0
                  RETURN
              END IF
              IF(WC.EQ.'X3Y3=') THEN
                  X3=W1
                  Y3=W2
                  FLAG3=1
                  FLAG4=0
                  RETURN
              END IF
              IF(WC.EQ.'X4Y4=') THEN
                  X4=W1
                  Y4=W2
                  FLAG4=1
                  RETURN
              END IF
C
          END IF
          IF(WC.EQ.'INTERP') THEN
C       INTERPOLATION
              IF(SST.EQ.1.OR.DF2.EQ.2.OR.DF3.EQ.0.OR.DF4.EQ.0
     1        .OR.DF5.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            WC(1:6),' TAKES NO ALPHANUMERIC STRING AND ONLY USES'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'QUALIFIER AND NUMERIC WORD 1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF

              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            WC(1:6),' REQUIRES EXPLICIT NUMERIC WORD 1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       CHECK FOR VALID QUALIFIER WORDS, EITHER (LIN),
C       (LAG), (PAR) OR (CUBIC)
              IF(WQ.NE.'LIN'.AND.WQ.NE.'LAG'.AND.WQ.NE.'PAR'
     1        .AND.WQ.NE.'CUBIC') THEN
                  WRITE(OUTLYNE,*)
     1            'INVALID QUALIFIER USED WITH THE "INTERP" COMMAND'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       WQ.EQ.LIN
              IF(WQ.EQ.'LIN') THEN
C       LINEAR INTERPOLATION
C
C       PROCEED WITH INTERPOLATION
C
C       ENOUGH POINTS?
                  IF(FLAG1.EQ.0) THEN
                      WRITE(OUTLYNE,*) 'NO INPUT DATA EXISTS FOR INTERPOLATION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'INPUT ADDITIONAL DATA WITH "X1Y1=" AND "X2Y2="'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'BEFORE USING "INTERP LIN"'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(FLAG1.EQ.1.AND.FLAG2.EQ.0) THEN
                      WRITE(OUTLYNE,*) 'ONLY 1 DATA PAIR EXISTS FOR INTERPOLATION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'INPUT ADDITIONAL DATA WITH "X2Y2="'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'BEFORE USING "INTERP LIN"'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C       INTERPOLATION
C       CALCULATE THE LINE SLOPE
                  IF(DABS(X2-X1).LE.1.0D-35) THEN
C       SLOPE INDETERMINATE, NO INTERP POSSIBLE
                      WRITE(OUTLYNE,*)
     1                'SLOPE INDETERMINATE, NO LINEAR INTERPOLATION POSSISBLE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'ENTER NEW DATA TO CONTINUE INTERPOLATING'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  VALUE1=(((X2-W1)*Y1)+((W1-X1)*Y2))/(X2-X1)
                  WRITE(OUTLYNE,200)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,305)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,400)W1,VALUE1
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,305)
                  CALL SHOWIT(0)
                  IF(FLAG3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'NOTICE: 3 DATA PAIRS WERE ENTERED PRIOR TO INTERPOLATION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'ONLY DATA PAIRS 1 AND 2 ARE USED FOR LINEAR INTERPOLATION'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C       EXTRAPOLATION WARNING MESSAGE?
                  IF(X1.LT.X2.AND.W1.LT.X1.OR.X1.LT.X2.AND.W1.GT.X2
     1            .OR.X2.LT.X1.AND.W1.LT.X2.OR.X2.LT.X1.AND.W1.GT.X1)THEN
C       EXTRAPOLATION
                      WRITE(OUTLYNE,*)
     1                'WARNING: CURRENT VALUE IS EXTRPOLATED, NOT INTERPOLATED'
                      CALL SHOWIT(1)
                  END IF
                  GO TO 100
              END IF
C       WQ.EQ.LAG OR PAR
              IF(WQ.EQ.'LAG'.OR.WQ.EQ.'PAR') THEN
C       LAGRANGIAN PARABOLIC 3-POINT INTERPOLATION
C       PROCEED WITH INTERPOLATION
C
C       ENOUGH POINTS?
                  IF(FLAG1.EQ.0) THEN
                      WRITE(OUTLYNE,*) 'NO INPUT DATA EXISTS FOR INTERPOLATION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'INPUT ADDITIONAL DATA WITH "X1Y1=", "X2Y2=" AND "X3Y3="'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'BEFORE USING "INTERP LAG" OR "INTERP PAR"'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(FLAG1.EQ.1.AND.FLAG2.EQ.0) THEN
                      WRITE(OUTLYNE,*)
     1                'ONLY 1 DATA PAIR EXISTS FOR INTERPOLATION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'INPUT ADDITIONAL DATA WITH "X2Y2=" AND "X3Y3="'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'BEFORE USING "INTERP LAG" OR "INTERP PAR"'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  IF(FLAG1.EQ.1.AND.FLAG2.EQ.1.AND.FLAG3.EQ.0) THEN
                      WRITE(OUTLYNE,*) 'ONLY 2 DATA PAIRS EXISTS FOR INTERPOLATION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'INPUT ADDITIONAL DATA WITH "X3Y3="'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'BEFORE USING "INTERP LAG" OR "INTERP PAR"'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C       INTERPOLATION
C       CALCULATE THE LINE SLOPE
                  IF(DABS(X2-X1).LE.1.0D-35.OR.DABS(X3-X2).LE.1.0D-35.OR.
     1            DABS(X1-X3).LE.1.0D-35) THEN
C       SLOPE INDETERMINATE, NO INTERP POSSIBLE
                      WRITE(OUTLYNE,*)
     1                'SLOPE INDETERMINATE, NO LAGRANGIAN INTERPOLATION POSSISBLE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'ENTER NEW DATA TO CONTINUE INTERPOLATING'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  L1=((W1-X2)/(X1-X2))*((W1-X3)/(X1-X3))
                  L2=((W1-X1)/(X2-X1))*((W1-X3)/(X2-X3))
                  L3=((W1-X1)/(X3-X1))*((W1-X2)/(X3-X2))
                  VALUE1=(L1*Y1)+(L2*Y2)+(L3*Y3)
                  WRITE(OUTLYNE,300)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,305)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,400)W1,VALUE1
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,305)
                  CALL SHOWIT(0)
                  IF(FLAG4.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                'NOTICE: 4 DATA PAIRS WERE ENTERED PRIOR TO INTERPOLATION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'ONLY DATA PAIRS 1, 2 AND 3 ARE USED FOR'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'PARABOLIC OR LAGRANGIAN INTERPOLATION'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C       EXTRAPOLATION WARNING MESSAGE?

C       CALCULATE MINIMUM X AND MAXIMUM X
                  IF(X1.LT.X2) THEN
                      XMIN=X1
                  ELSE
                      XMIN=X2
                  END IF
                  IF(X3.LT.XMIN) THEN
                      XMIN=X3
                  END IF
                  IF(X1.GT.X2) THEN
                      XMAX=X1
                  ELSE
                      XMAX=X2
                  END IF
                  IF(X3.GT.XMAX) THEN
                      XMAX=X3
                  END IF
                  IF(W1.LT.XMIN.OR.W1.GT.XMAX) THEN
C       EXTRAPOLATION
                      WRITE(OUTLYNE,*)
     1                'WARNING: CURRENT VALUE IS EXTRPOLATED, NOT INTERPOLATED'
                      CALL SHOWIT(1)
                  END IF
                  GO TO 100
              END IF
C       WQ.EQ.CUBIC
              IF(WQ.EQ.'CUBIC') THEN
C       CUBIC 4-POINT INTERPOLATION
C       PROCEED WITH INTERPOLATION
C
C       ENOUGH POINTS?
                  IF(FLAG1.EQ.0) THEN
                      WRITE(OUTLYNE,*) 'NO INPUT DATA EXISTS FOR INTERPOLATION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'INPUT ADDITIONAL DATA WITH "X1Y1=", "X2Y2="'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                '"X3Y3=" AND "X4Y4="'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'BEFORE USING "INTERP CUBIC"'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(FLAG1.EQ.1.AND.FLAG2.EQ.0) THEN
                      WRITE(OUTLYNE,*) 'ONLY 1 DATA PAIR EXISTS FOR INTERPOLATION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'INPUT ADDITIONAL DATA WITH "X2Y2=", "X3Y3=" AND "X4Y4="'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'BEFORE USING "INTERP CUBIC"'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  IF(FLAG1.EQ.1.AND.FLAG2.EQ.1.AND.FLAG3.EQ.0) THEN
                      WRITE(OUTLYNE,*) 'ONLY 2 DATA PAIRS EXISTS FOR INTERPOLATION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'INPUT ADDITIONAL DATA WITH "X3Y3=" AND "X4Y4="'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'BEFORE USING "INTERP CUBIC"'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(FLAG1.EQ.1.AND.FLAG2.EQ.1.AND.FLAG3.EQ.1.AND.
     1            FLAG4.EQ.0) THEN
                      WRITE(OUTLYNE,*) 'ONLY 3 DATA PAIRS EXISTS FOR INTERPOLATION'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'INPUT ADDITIONAL DATA WITH "X4Y4="'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*) 'BEFORE USING "INTERP CUBIC"'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
C       INTERPOLATION
C       CALCULATE THE LINE SLOPE
                  IF(DABS(X2-X1).LE.1.0D-35.OR.DABS(X3-X2).LE.1.0D-35.OR.
     1            DABS(X1-X3).LE.1.0D-35.OR.DABS(X1-X4).LE.1.0D-35.OR.
     2            DABS(X2-X4).LE.1.0D-35.OR.DABS(X3-X4).LE.1.0D-35) THEN
C       SLOPE INDETERMINATE, NO INTERP POSSIBLE
                      WRITE(OUTLYNE,*)
     1                'SLOPE INDETERMINATE, NO CUBIC INTERPOLATION POSSISBLE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'ENTER NEW DATA TO CONTINUE INTERPOLATING'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  L1=((W1-X2)/(X1-X2))*((W1-X3)/(X1-X3))*((W1-X4)/(X1-X4))
                  L2=((W1-X1)/(X2-X1))*((W1-X3)/(X2-X3))*((W1-X4)/(X2-X4))
                  L3=((W1-X1)/(X3-X1))*((W1-X2)/(X3-X2))*((W1-X4)/(X3-X4))
                  L4=((W1-X1)/(X4-X1))*((W1-X2)/(X4-X2))*((W1-X3)/(X4-X3))
                  VALUE1=(L1*Y1)+(L2*Y2)+(L3*Y3)+(L4*Y4)
                  WRITE(OUTLYNE,500)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,305)
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,400)W1,VALUE1
                  CALL SHOWIT(0)
                  WRITE(OUTLYNE,305)
                  CALL SHOWIT(0)
C       EXTRAPOLATION WARNING MESSAGE?

C       CALCULATE MINIMUM X AND MAXIMUM X
                  IF(X1.LT.X2) THEN
                      XMIN=X1
                  ELSE
                      XMIN=X2
                  END IF
                  IF(X3.LT.XMIN) THEN
                      XMIN=X3
                  END IF
                  IF(X4.LT.XMIN) THEN
                      XMIN=X4
                  END IF
                  IF(X1.GT.X2) THEN
                      XMAX=X1
                  ELSE
                      XMAX=X2
                  END IF
                  IF(X3.GT.XMAX) THEN
                      XMAX=X3
                  END IF
                  IF(X4.GT.XMAX) THEN
                      XMAX=X4
                  END IF
                  IF(W1.LT.XMIN.OR.W1.GT.XMAX) THEN
C       EXTRAPOLATION
                      WRITE(OUTLYNE,*)
     1                'WARNING: CURRENT VALUE IS EXTRPOLATED, NOT INTERPOLATED'
                      CALL SHOWIT(1)
                  END IF
                  GO TO 100
              END IF
C
C       PUT ANSWER IN ACCUMULATOR AND RETURN
 100          REG(40)=REG(9)
              REG(9)=VALUE1
              CONTINUE
              RETURN
C
          END IF
C
          RETURN
 200      FORMAT('USING 2-POINT LINEAR INTERPOLATION:')
 300      FORMAT('USING 3-POINT LAGRANGIAN PARABOLIC INTERPOLATION:')
 500      FORMAT('USING 4-POINT CUBIC INTERPOLATION:')
 305      FORMAT(1X)
 400      FORMAT('FOR X = ',D18.10,' Y = ',D18.10)
      END


C SUB CWRITE.FOR
      SUBROUTINE CWRITE
C
          IMPLICIT NONE
C
C       THIS IS THE CWRITE SUBROUTINE. IT PROVIDES A WAY OF
C       OUTPUTTING FORMATTED OUTPUT FROM WITHIN A MACRO OR
C       FROM THE CMD LEVEL.
C
          CHARACTER STWORD*80,QWORD*8,CWORD*8
     &    ,QBLANK*8,ANUM*80,AN1*23,AN2*23,
     &    AN3*23,AN4*23,AN5*23,BL80*80
C
          INTEGER FLAG1,FLAG2,
     &    FLAG3,FLAG4,FLAG5,QFLAG,CFLAG,STFLAG,ACNT
C
          REAL*8 N1WORD,
     &    N2WORD,N3WORD,N4WORD,N5WORD
C
          INCLUDE 'datmai.inc'
C
          QBLANK='        '
          BL80=AA//AA//AA//AA
C       STWORD
          IF(WC.EQ.'STWORD') THEN
C
C       CHECK FOR WRONG INPUT AND SET FLAG
C
              IF(SQ.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"STWORD" ONLY TAKES STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SST.NE.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"STWORD" REQUIRES EXPLICIT STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              STWORD=WS
              IF(WS.NE.BL80) STFLAG=1
          END IF
C
C       CWORD
          IF(WC.EQ.'CWORD') THEN
C
C       CHECK FOR WRONG INPUT AND SET FLAG
C
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"CWORD" ONLY TAKES QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.NE.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"CWORD" REQUIRES EXPLICIT QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              CWORD=WQ
              IF(WQ.NE.QBLANK) CFLAG=1
          END IF
C
C       QWORD
          IF(WC.EQ.'QWORD') THEN
C
C       CHECK FOR WRONG INPUT AND SET FLAG
C
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"QWORD" ONLY TAKES QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(SQ.NE.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"QWORD" REQUIRES EXPLICIT QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              QWORD=WQ
              IF(WQ.NE.QBLANK) QFLAG=1
          END IF
C
C       N1WORD TO N5WORD
          IF(WC.EQ.'N1WORD'.OR.WC.EQ.'N2WORD'.OR.WC.EQ.'N3WORD'
     1    .OR.WC.EQ.'N4WORD'.OR.WC.EQ.'N5WORD') THEN
C
C       CHECK FOR WRONG INPUT AND SET FLAG
C
              IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.
     1        S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"N1WORD" TO "N5WORD" ONLY TAKE NUMERIC WORD 1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S1.NE.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"N1WORD" TO "N5WORD" REQUIRES EXPLICIT NUMERIC WORD 1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'N1WORD')THEN
                  N1WORD=W1
                  FLAG1=1
              END IF
              IF(WC.EQ.'N2WORD')THEN
                  N2WORD=W1
                  FLAG2=1
              END IF
              IF(WC.EQ.'N3WORD')THEN
                  N3WORD=W1
                  FLAG3=1
              END IF
              IF(WC.EQ.'N4WORD')THEN
                  N4WORD=W1
                  FLAG4=1
              END IF
              IF(WC.EQ.'N5WORD')THEN
                  N5WORD=W1
                  FLAG5=1
              END IF
          END IF
C
C       WC IS NEWCMD
          IF(WC.EQ.'NEWCMD') THEN
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  WRITE(OUTLYNE,*)'"NEWCMD" TAKES NO STRING OR NUMERIC INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
C       VALID VERSIONS ARE WITH QUALIFIER EITHER BLANK
C       OR "CLEAR"
C       "CLEAR" DOES NO OUTPUT BUT RESETS ALL FLAGS TO 0
C       AND SETS CWORD,QWORD,STWORD TO BLANK AND N1WORD TO N5WORD
C       TO ZERO
          IF(WC.EQ.'NEWCMD'.AND.SQ.EQ.1.AND.WQ.NE.'CLEAR') THEN
C       INVALID QUALIFIER
              WRITE(OUTLYNE,*)'"NEWCMD" ISSUED WITH INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'NEWCMD'.AND.WQ.EQ.'CLEAR') THEN
              CWORD=QBLANK
              QWORD=QBLANK
              STWORD=AA//AA//AA//AA
              N1WORD=0.0D0
              N2WORD=0.0D0
              N3WORD=0.0D0
              N4WORD=0.0D0
              N5WORD=0.0D0
              CFLAG=0
              QFLAG=0
              STFLAG=0
              FLAG1=0
              FLAG2=0
              FLAG3=0
              FLAG4=0
              FLAG5=0
          END IF
          IF(WC.EQ.'NEWCMD'.AND.SQ.EQ.0) THEN
C
C       PERFORM THE OUTPUT UNLESS CFLAG=0

              IF (CFLAG.EQ.0) THEN

                  WRITE(OUTLYNE,*)'NEW COMMAND TO BE OUTPUT WAS ALL BLANK'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       IF NUMERIC AND STRING OUTPUT DATA EXISTS, NUMERIC TAKES
C       PRIORITY.
              IF(FLAG1.EQ.1.OR.FLAG2.EQ.1.OR.FLAG3.EQ.1.OR.FLAG4.EQ.1
     1        .OR.FLAG5.EQ.1) THEN
                  IF(STFLAG.EQ.1) THEN
C       BLANK THE STRING AND DO MESSAGE
                      STFLAG=0
                      STWORD=AA//AA//AA//AA
                      WRITE(OUTLYNE,*)
     1                'BOTH NUMERIC AND ALPANUMERIC STRING DATA WAS FOUND'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C       THERE IS NUMERIC DATA AND QUALIFIER, DO THAT OUTPUT HERE
C
C       NUMERIC OUTPUT GOES HERE
C
C       CONSTRUCT APPROPRIATE ALPHANUMERIC REPRESENTATION
C       OR NUMERIC WORDS SIMILAR TO WHAT IS DONE IN THE
C       OUTPUT OF A MACRO TO A DISK FILE DURING MFL.
C
C       CONVERT TO ALPHA FROM REAL*8
C
                  CALL NTA(N1WORD,N2WORD,N3WORD,N4WORD,N5WORD,AN1,AN2,
     1            AN3,AN4,AN5)
C
                  IF(FLAG1.EQ.1) THEN
                      ANUM=','//AN1
                      ACNT=24
                  ELSE
                      ANUM=',,'
                      ACNT=2
                  END IF
                  IF(FLAG2.EQ.1) THEN
                      ANUM=ANUM(1:ACNT)//','//AN2
                      ACNT=ACNT+24
                  ELSE
                      ANUM=ANUM(1:ACNT)//','
                      ACNT=ACNT+1
                  END IF
                  IF(FLAG3.EQ.1) THEN
                      ANUM=ANUM(1:ACNT)//','//AN3
                      ACNT=ACNT+24
                  ELSE
                      ANUM=ANUM(1:ACNT)//','
                      ACNT=ACNT+1
                  END IF
                  IF(FLAG4.EQ.1) THEN
                      ANUM=ANUM(1:ACNT)//','//AN4
                      ACNT=ACNT+24
                  ELSE
                      ANUM=ANUM(1:ACNT)//','
                      ACNT=ACNT+1
                  END IF
                  IF(FLAG5.EQ.1) THEN
                      ANUM=ANUM(1:ACNT)//','//AN5
                      ACNT=ACNT+24
                  ELSE
                      ANUM=ANUM(1:ACNT)//','
                      ACNT=ACNT+1
                  END IF
C
C       IS THERE A QUALIFIER WORD
                  IF(QFLAG.EQ.1) THEN
C       YES
                      INPUT=TRIM(CWORD)//' '//TRIM(QWORD)//TRIM(ANUM)
                  ELSE
C       NO QUALIFIER
                      INPUT=TRIM(CWORD)//TRIM(ANUM)
                  END IF
              ELSE
C       NO NUMERIC DATA EXISTS, ONLY QUALIFIER AND STRING
C
                  IF(QFLAG.EQ.0.AND.STFLAG.EQ.0) THEN
C       WRITE COMMAND WORD ONLY
                      INPUT=TRIM(CWORD)
                  END IF
                  IF(QFLAG.EQ.1.AND.STFLAG.EQ.0) THEN
C       WRITE COMMAND WORD AND QUALIFIER WORD
                      INPUT=TRIM(CWORD)//' '//TRIM(QWORD)
                  END IF
                  IF(QFLAG.EQ.0.AND.STFLAG.EQ.0) THEN
C       WRITE COMMAND WORD AND STRING
                      INPUT=TRIM(CWORD)//TRIM(STWORD)
                  END IF
                  IF(QFLAG.EQ.1.AND.STFLAG.EQ.1) THEN
C       WRITE COMMAND WORD ONLY
                      INPUT=TRIM(CWORD)//' '//TRIM(QWORD)//' '//TRIM(STWORD)
                  END IF
              END IF
C                       QUALIFIER NOT QBLANK
          END IF
          IF(WC.EQ.'NEWCMD'.AND.WQ.NE.'CLEAR') THEN
              SAVE_KDP(19)=SAVEINPT(19)
              CALL PROCES
              REST_KDP(19)=RESTINPT(19)
          END IF
          RETURN
      END
C SUB COORD.FOR
      SUBROUTINE COORD
C
          IMPLICIT NONE
C
C       THIS ROUTINE DOES COORDINATE, TIME/DATE AND ANGULAR
C       CONVERSIONS.
C
          REAL*8 R,THETA,R2,PHI,X,Y,Z,FAC,
     1    H,M,S,BUFFER,COSARG
C
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
C
              IF(WC.EQ.'R-P') THEN
                  WRITE(OUTLYNE,*)'"R-P" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'P-R') THEN
                  WRITE(OUTLYNE,*)'"P-R" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'R-CYL') THEN
                  WRITE(OUTLYNE,*)'"R-CYL" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'CYL-R') THEN
                  WRITE(OUTLYNE,*)'"CYL-R" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'R-SP') THEN
                  WRITE(OUTLYNE,*)'"R-SP" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'SP-R') THEN
                  WRITE(OUTLYNE,*)'"SP-R" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'H-HMS') THEN
                  WRITE(OUTLYNE,*)'"H-HMS" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'HMS-H') THEN
                  WRITE(OUTLYNE,*)'"HMS-H" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(WC.EQ.'IN-MM'.OR.WC.EQ.'IN-CM'.OR.WC.EQ.'IN-M'.OR.
     1        WC.EQ.'MM-IN'.OR.WC.EQ.'CM-IN'.OR.WC.EQ.'M-IN') THEN
                  WRITE(OUTLYNE,*)'"',WC(1:5),'" TAKES NO EXPLICIT INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'R-P') THEN
C       THIS CONVERTS THE RECTANGULAR COORDINATES
C       X AND Y (STORED IN THE STACK IN X AND Y) INTO
C       RIGHT HANDED POLAR COORDINATES R AND THETA.
C       AFTER THE CALCULATION, R REPLACES X AND THETA
C       REPLACES Y IN THE STACK. THETA IS EXPRESSED IN
C       DECIMAL DEGREES.
              IF(REG(9).EQ.0.0D0) THEN
                  THETA=90.0D0
                  GO TO 51
              END IF
              IF(DABS(REG(10)).GE.DABS(((1.0D35)*REG(9)))) THEN
                  IF(REG(10).GE.0.0D0) THETA=90.0D0
                  IF(REG(10).LT.0.0D0) THETA=-90.0D0
              ELSE
                  IF(DABS(REG(10)).LE.1.0D-15.AND.DABS(REG(9)).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(REG(10),REG(9))
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
                  THETA=THETA*(180.0D0/PII)
              END IF
 51           R=DSQRT(DABS(REG(9)*REG(9))+DABS(REG(10)*REG(10)))
              REG(40)=REG(9)
              REG(9)=R
              REG(10)=THETA
              RETURN
          END IF
          IF(WC.EQ.'P-R') THEN
C       THIS CONVERTS THE RIGHT HANDED
C       POLAR COORDINATES
C       R AND THETA (STORED IN THE STACK IN X AND Y) INTO
C       RECTANGULAR COORDINATES X AND Y.
C       AFTER THE CALCULATION, X REPLACES R AND Y
C       REPLACES THETA IN THE STACK. THETA IS ENTERED IN
C       DECIMAL DEGREES.
C
              X=REG(9)*(DCOS(REG(10)/(180.0D0/PII)))
              Y=REG(9)*(DSIN(REG(10)/(180.0D0/PII)))
              REG(40)=REG(9)
              REG(9)=X
              REG(10)=Y
              RETURN
          END IF
          IF(WC.EQ.'R-CYL') THEN
C       THIS CONVERTS THE RECTANGULAR COORDINATES
C       X,Y AND Z (STORED IN THE STACK IN X,Y AND Z) INTO
C       RIGHT HANDED CYLINDRICAL COORDINATES R AND THETA AND Z
C       AFTER THE CALCULATION, R REPLACES X AND THETA
C       REPLACES Y IN THE STACK. THETA IS EXPRESSED IN
C       DECIMAL DEGREES. Z REMAINS IN Z
              IF(REG(9).EQ.0.0D0) THEN
                  THETA=90.0D0
                  GO TO 52
              END IF
              IF(DABS(REG(10)).GE.DABS(((1.0D35)*REG(9)))) THEN
                  IF(REG(10).GE.0.0D0) THETA=90.0D0
                  IF(REG(10).LT.0.0D0) THETA=-90.0D0
              ELSE
                  IF(DABS(REG(10)).LE.1.0D-15.AND.DABS(REG(9)).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(REG(10),REG(9))
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
                  THETA=THETA*(180.0D0/PII)
              END IF
 52           R=DSQRT(DABS(REG(9)*REG(9))+DABS(REG(10)*REG(10)))
              REG(40)=REG(9)
              REG(9)=R
              REG(10)=THETA
              RETURN
          END IF
          IF(WC.EQ.'CYL-R') THEN
C       THIS CONVERTS THE RIGHT HANDED
C       CYLINDRICAL COORDINATES
C       R AND THETA AND Z (STORED IN THE STACK IN X,Y AND Z) INTO
C       RECTANGULAR COORDINATES X AND Y AND Z.
C       AFTER THE CALCULATION, X REPLACES R AND Y
C       REPLACES THETA IN THE STACK. THETA IS ENTERED IN
C       DECIMAL DEGREES. Z REMAINS IN Z
              FAC=(180.0D0/PII)
              X=REG(9)*(DCOS(REG(10)/FAC))
              Y=REG(9)*(DSIN(REG(10)/FAC))
              REG(40)=REG(9)
              REG(9)=X
              REG(10)=Y
              RETURN
          END IF
          IF(WC.EQ.'R-SP')THEN
C       THIS CONVERTS THE RECTANGULAR COORDINATES
C       X,Y AND Z (STORED IN THE STACK IN X,Y AND Z) INTO
C       RIGHT HANDED SPHERICAL COORDINATES R AND THETA AND PHI
C       AFTER THE CALCULATION, R REPLACES X AND THETA
C       REPLACES Y AND PHI REPLACES Z IN THE STACK.
C       THETA AND PHI ARE EXPRESSED IN
C       DECIMAL DEGREES.
              R2=(REG(9)*REG(9))+(REG(10)*REG(10))+(REG(11)*REG(11))
              IF(R2.EQ.0.0D0) THEN
                  REG(9)=0.0D0
                  REG(10)=0.0D0
                  REG(11)=0.0D0
                  RETURN
              END IF
              IF(REG(9).EQ.0.0D0) THEN
                  THETA=90.0D0
                  GO TO 53
              END IF
              IF(DABS(REG(10)).GE.DABS(((1.0D35)*REG(9)))) THEN
                  IF(REG(10).GE.0.0D0) THETA=90.0D0
                  IF(REG(10).LT.0.0D0) THETA=-90.0D0
              ELSE
                  IF(DABS(REG(10)).LE.1.0D-15.AND.DABS(REG(9)).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(REG(10),REG(9))
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
                  THETA=THETA*(180.0D0/PII)
              END IF
 53           R=DSQRT(R2)
              COSARG=REG(11)/R
              IF(COSARG.LT.0.0D0) COSARG=-COSARG
              IF(COSARG.GT.1.0D0) COSARG=1.0D0
              PHI=DACOS(COSARG)
              REG(40)=REG(9)
              REG(9)=R
              REG(10)=THETA
              REG(11)=PHI
              RETURN
          END IF
          IF(WC.EQ.'SP-R') THEN
C       THIS CONVERTS THE RIGHT HANDED
C       SPHERICAL COORDINATES
C       R THETA AND PHI (STORED IN THE STACK IN X,Y AND Z) INTO
C       RECTANGULAR COORDINATES X AND Y AND Z.
C       AFTER THE CALCULATION, X REPLACES R AND Y
C       REPLACES THETA AND Z REPLACES PHI IN THE STACK. THETA IS
C        ENTERED IN DECIMAL DEGREES.
              FAC=(180.0D0/PII)
              X=REG(9)*DCOS((REG(10)/(FAC)))*DSIN((REG(11)/(FAC)))
              Y=REG(9)*DSIN((REG(10)/(FAC)))*DSIN((REG(11)/(FAC)))
              Z=REG(9)*DCOS(REG(11)/(180.0D0/PII))
              REG(40)=REG(9)
              REG(9)=X
              REG(10)=Y
              REG(11)=Z
              RETURN
          END IF
          IF(WC.EQ.'H-HMS') THEN
C       THIS CONVERTS DECIMAL HOURS OR DEGREES TO
C       HOURS/MIN/SEC OR DEG/MIN/SEC
C
C       DECIMAL HOURS OR DEGREES ARE STORED IN THE X REGISTER
C       REG(9).
C       AFTER THE CALCULATION, H.MS OR D.MS REPLACES
C       DECIMAL HOURS OR DEGREES IN THE X REGISTER REG(9)
C
C       OUTPUT OF 3HRS-25MIN-34.5SEC IS STORED IN REG(9) AS:
C
C               3.25345
C
              BUFFER=DBLE(INT(REG(9)))
              H=BUFFER
              BUFFER=REG(9)-H
              IF(BUFFER.EQ.0.0D0) THEN
                  REG(40)=REG(9)
                  REG(9)=H
                  RETURN
              END IF
C               FRACTIONAL PART NOT ZERO
              M=DBLE(INT(60.0D0*BUFFER))
C       M IS THE NUMBER OF MINUTES LESS THAN 60
              BUFFER=(BUFFER*60)-M
              S=60.0D0*BUFFER
C       S IS THE NUMBER OF SECONDS AND FRACTION OF SECONDS
C
              H=H+(M/100.0D0)+(S/10000.0D0)
              REG(40)=REG(9)
              REG(9)=H
              RETURN
          END IF
          IF(WC.EQ.'HMS-H') THEN
C       THIS CONVERTS  HOURS.MUNUTES SECONDS OR DEGREES
C       .MINUTES SECONDS TO DECIMAL HOURS OR DEGREES.
C       INPUT IS:
C               HOURS.MINUETSSECONDS
C       OR
C               DEGREES.MINUTESSECONDS
C
C       3HRS-25MIN-34.5SECONDS IS STORED IN REG(9) AS:
C
C               3.25345
C       AFTER THE CALCULATION
C       DECIMAL HOURS OR DEGREES ARE STORED IN THE X REGISTER
C       REG(9).
              BUFFER=DBLE(INT(REG(9)))
              H=BUFFER
              BUFFER=REG(9)-H
              IF(BUFFER.EQ.0.0D0) THEN
                  REG(40)=REG(9)
                  REG(9)=H
                  RETURN
              END IF
C               FRACTIONAL PART NOT ZERO
              M=DBLE(INT(100.0D0*BUFFER))/60.0D0
C       M IS THE NUMBER OF MINUTES EXPRESSED AS A FRACTION OF A
C       DEGREE OR HOUR.
              BUFFER=(BUFFER*100.0D0)-(M*60.0D0)
              S=(100.0D0*BUFFER)/3600.0D0
C       S IS THE NUMBER OF SECONDS AND FRACTION OF SECONDS EXPRESSED
C       AS A FRACTION OF A DEGREE OR HOUR.
C
              H=H+M+S
              REG(40)=REG(9)
              REG(9)=H
              RETURN
          END IF
          IF(WC.EQ.'IN-CM') THEN
C       THIS CONVERTS IN TO CM
              REG(40)=REG(9)
              REG(9)=REG(9)*2.54D0
              RETURN
          END IF
          IF(WC.EQ.'IN-MM') THEN
C       THIS CONVERTS IN TO MM
              REG(40)=REG(9)
              REG(9)=REG(9)*25.4D0
              RETURN
          END IF
          IF(WC.EQ.'IN-M') THEN
C       THIS CONVERTS IN TO M
              REG(40)=REG(9)
              REG(9)=REG(9)/39.3700787402
              RETURN
          END IF
          IF(WC.EQ.'CM-IN') THEN
C       THIS CONVERTS CM TO IN
              REG(40)=REG(9)
              REG(9)=REG(9)/2.54D0
              RETURN
          END IF
          IF(WC.EQ.'MM-IN') THEN
C       THIS CONVERTS MM TO IN
              REG(40)=REG(9)
              REG(9)=REG(9)/25.4D0
              RETURN
          END IF
          IF(WC.EQ.'M-IN') THEN
C       THIS CONVERTS M TO IN
              REG(40)=REG(9)
              REG(9)=REG(9)*39.3700787402
              RETURN
          END IF
          RETURN
      END
C SUB GCONVERT
      SUBROUTINE GCONVERT
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          REAL*8 LOCAL_XVERT,LOCAL_YVERT,LOCAL_ZVERT
          REAL*8 LOCAL_LXVERT,LOCAL_MXVERT,LOCAL_NXVERT
          REAL*8 LOCAL_LYVERT,LOCAL_MYVERT,LOCAL_NYVERT
          REAL*8 LOCAL_LZVERT,LOCAL_MZVERT,LOCAL_NZVERT
          DIMENSION LOCAL_XVERT(:),LOCAL_YVERT(:),LOCAL_ZVERT(:)
          DIMENSION LOCAL_LXVERT(:),LOCAL_MXVERT(:),LOCAL_NXVERT(:)
          DIMENSION LOCAL_LYVERT(:),LOCAL_MYVERT(:),LOCAL_NYVERT(:)
          DIMENSION LOCAL_LZVERT(:),LOCAL_MZVERT(:),LOCAL_NZVERT(:)
          ALLOCATABLE :: LOCAL_XVERT,LOCAL_YVERT,LOCAL_ZVERT
          ALLOCATABLE LOCAL_LXVERT,LOCAL_MXVERT,LOCAL_NXVERT
          ALLOCATABLE LOCAL_LYVERT,LOCAL_MYVERT,LOCAL_NYVERT
          ALLOCATABLE LOCAL_LZVERT,LOCAL_MZVERT,LOCAL_NZVERT
          REAL*8 D11,D12,D13,D21,D22,D23,D31,D32,D33
          REAL*8 XDEC,YDEC,ZDEC,ALPHA,BETA,GAMMA,COSB,SINB
          CHARACTER*23 AALPHA,ABETA,AGAMMA,AXDEC,AYDEC,AZDEC
          INTEGER IV,ALLOERR,I
          CHARACTER*3 AIV1,AIV2
          ALLOCATE(LOCAL_XVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE(LOCAL_YVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE(LOCAL_ZVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE(LOCAL_LXVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE(LOCAL_LYVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE(LOCAL_LZVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE(LOCAL_MXVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE(LOCAL_MYVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE(LOCAL_MZVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE(LOCAL_NXVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE(LOCAL_NYVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          ALLOCATE(LOCAL_NZVERT(0:INT(SYSTEM1(20))),STAT=ALLOERR)
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"GCONVERT" CONVERTS THE CURRENT LENS INTO A "GLOBAL" LENS WITH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RESPECT TO THE SURFACE SPECIFIED IN NUMERIC WORD #1'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1    .OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"GCONVERT TAKES NO QUALIFIER, STRING OR'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #2, #3, #4 OR #5 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"GCONVERT REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(W1.LE.1) THEN
              WRITE(OUTLYNE,*)
     1        '"NUMERIC WORD #1 NUST BE 2 OR GREATER'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

          IV=INT(W1)
          CALL NTOAN1(IV,AIV2)
C
          SAVE_KDP(20)=SAVEINPT(20)
          INPUT='GLOBAL '//AIV2
          CALL PROCES
          REST_KDP(20)=RESTINPT(20)
C
C       GET GLOBAL VERTEX DATA FOR ALL SURFACES
C
          DO I=0,INT(SYSTEM1(20))
C
C       SET GLOBAL SURFACE TO INT(W1)
              IV=I
              CALL NTOAN1(IV,AIV1)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET XVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_XVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET YVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_YVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET ZVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_ZVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET LXVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_LXVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET MXVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_MXVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET NXVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_NXVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET LYVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_LYVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET MYVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_MYVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET NYVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_NYVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET LZVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_LZVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET MZVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_MZVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GET NZVERT '//AIV1//','//AIV2
              CALL PROCES
              LOCAL_NZVERT(I)=REG(9)
              REST_KDP(20)=RESTINPT(20)
          END DO
          DO I=INT(W1)+1,INT(SYSTEM1(20))
              IV=I
              CALL NTOAN1(IV,AIV1)
              XDEC=LOCAL_XVERT(I)
              YDEC=LOCAL_YVERT(I)
              ZDEC=LOCAL_ZVERT(I)
              D11=LOCAL_LXVERT(I)
              D12=LOCAL_MXVERT(I)
              D13=LOCAL_NXVERT(I)
              D21=LOCAL_LYVERT(I)
              D22=LOCAL_MYVERT(I)
              D23=LOCAL_NYVERT(I)
              D31=LOCAL_LZVERT(I)
              D32=LOCAL_MZVERT(I)
              D33=LOCAL_NZVERT(I)
C     NOW CALCULATE ALPHA,BETA AND GAMMA AND ASSIGN THEM TO SURFACE I
C     CALCULATE BETA
              BETA=DASIN(-D31)
              COSB=DCOS(BETA)
              IF(COSB.NE.0.0D0) THEN
C     COSINE OF BETA IS NOT ZERO
                  IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).NE.0.0D0) ALPHA=0.0D0
                  IF((D32/COSB).EQ.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=0.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).EQ.0.0D0) ALPHA=PII/2.0D0
                  IF((D32/COSB).NE.0.0D0.AND.(D33/COSB).NE.0.0D0)
     1            ALPHA=DATAN2((D32/COSB),(D33/COSB))
                  IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).NE.0.0D0) GAMMA=0.0D0
                  IF((D21/COSB).EQ.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=0.0D0
                  IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).EQ.0.0D0) GAMMA=PII/2.0D0
                  IF((D21/COSB).NE.0.0D0.AND.(D11/COSB).NE.0.0D0)
     1            GAMMA=DATAN2((-D21/COSB),(D11/COSB))
              END IF
              IF(COSB.EQ.0.0D0) THEN
C     COSINE OF BETA IS ZERO
                  IF(D31.EQ.-1.0D0) SINB=1
                  IF(D31.EQ.1.0D0) SINB=-1
                  IF(SINB.EQ.1) BETA=PII/2.0D0
                  IF(SINB.EQ.-1) BETA=-PII/2.0D0
                  GAMMA=0.0D0
                  IF(SINB.EQ.1) THEN
                      IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=PII/2.0D0
                      IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=DATAN2((D12),(D13))
                  END IF
                  IF(SINB.EQ.-1) THEN
                      IF(D12.EQ.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.EQ.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=0.0D0
                      IF(D12.NE.0.0D0.AND.D13.EQ.0.0D0)
     1                ALPHA=PII/2.0D0
                      IF(D12.NE.0.0D0.AND.D13.NE.0.0D0)
     1                ALPHA=DATAN2((-D12),(-D13))
                  END IF
              END IF
              ALPHA=ALPHA*(180.0D0/PII)
              BETA=BETA*(180.0D0/PII)
              GAMMA=GAMMA*(180.0D0/PII)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='U L'
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='CHG '//AIV1
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='DEC 0 0 0'
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='TILTD'
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='EOS'
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              INPUT='U L'
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='CHG '//AIV1
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='TILT RET '//AIV2
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='EOS'
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
C
              CALL CONVR4(AXDEC,XDEC)
              CALL CONVR4(AYDEC,YDEC)
              CALL CONVR4(AZDEC,ZDEC)
              CALL CONVR4(AALPHA,ALPHA)
              CALL CONVR4(ABETA,BETA)
              CALL CONVR4(AGAMMA,GAMMA)
C
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='U L'
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='CHG '//AIV1
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GDX '//AXDEC
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GDY '//AYDEC
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GDZ '//AZDEC
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GALPHA '//AALPHA
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GBETA '//ABETA
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='GGAMMA '//AGAMMA
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              ALENS(3,I)=0.0D0
              REST_KDP(20)=RESTINPT(20)
              SAVE_KDP(20)=SAVEINPT(20)
              INPUT='EOS'
              CALL PROCES
              REST_KDP(20)=RESTINPT(20)
          END DO
          DEALLOCATE(LOCAL_XVERT,STAT=ALLOERR)
          DEALLOCATE(LOCAL_YVERT,STAT=ALLOERR)
          DEALLOCATE(LOCAL_ZVERT,STAT=ALLOERR)
          DEALLOCATE(LOCAL_LXVERT,STAT=ALLOERR)
          DEALLOCATE(LOCAL_LYVERT,STAT=ALLOERR)
          DEALLOCATE(LOCAL_LZVERT,STAT=ALLOERR)
          DEALLOCATE(LOCAL_MXVERT,STAT=ALLOERR)
          DEALLOCATE(LOCAL_MYVERT,STAT=ALLOERR)
          DEALLOCATE(LOCAL_MZVERT,STAT=ALLOERR)
          DEALLOCATE(LOCAL_NXVERT,STAT=ALLOERR)
          DEALLOCATE(LOCAL_NYVERT,STAT=ALLOERR)
          DEALLOCATE(LOCAL_NZVERT,STAT=ALLOERR)
          RETURN
      END
C SUB CONVR1.FOR
      SUBROUTINE CONVR1(AVAL,IVAL)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT CHARACTER REPRESENTATION OF
C       A D23.15 NUMERIC WORD TO AN INTEGER
C       VALUE. CHARACTER VALUE IS 23 SPACES LONG
C
          REAL*8 VAL
C
          INTEGER IVAL
C
          COMMON/VL/VAL
C
          CHARACTER B*140,AVAL*140,ANVAL*23
C
          COMMON/CHVAL/B,ANVAL
C
          ANVAL=AVAL(19:41)
          WRITE(B,110) ANVAL
          READ(B,100) VAL
          IVAL=INT(VAL)
! 10             CONTINUE
 100      FORMAT(D23.15)
 110      FORMAT(A23)
          RETURN
      END
C SUB CONVR2.FOR
      SUBROUTINE CONVR2(AVAL,IVAL)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT CHARACTER REPRESENTATION OF
C       A D23.15 NUMERIC WORD TO AN INTEGER
C       VALUE. CHARACTER VALUE IS 23 SPACES LONG
C
          REAL*8 VAL
C
          INTEGER IVAL
C
          COMMON/VL/VAL
C
          CHARACTER B*140,AVAL*140,ANVAL*23
C
          COMMON/CHVAL/B,ANVAL
C
          ANVAL=AVAL(10:32)
          WRITE(B,110) ANVAL
          READ(B,100) VAL
          IVAL=INT(VAL)
! 10             CONTINUE
 100      FORMAT(D23.15)
 110      FORMAT(A23)
          RETURN
      END
C SUB CONVR3.FOR
      SUBROUTINE CONVR3(AVAL,VAL)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT CHARACTER REPRESENTATION OF
C       A D23.15 NUMERIC WORD TO AN REAL*8 VALUE
C
          REAL*8 VAL
C
          CHARACTER B*23,AVAL*23
C
          WRITE(B,110) AVAL
          READ(B,100) VAL
 100      FORMAT(D23.15)
 110      FORMAT(A23)
          RETURN
      END
C SUB CONVR4.FOR
      SUBROUTINE CONVR4(AVAL,VAL)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT
C       A D23.15 NUMERIC WORD TO
C       A CHARACTER VALUE IS 23 SPACES LONG
C
          REAL*8 VAL
C
          CHARACTER B*23,AVAL*23
C
          WRITE(B,100) VAL
          READ(B,110) AVAL
 100      FORMAT(D23.15)
 110      FORMAT(A23)
          RETURN
      END
C SUB ATODCV.FOR
      SUBROUTINE ATODCV
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT CHARACTER REPRESENTATION OF
C       A D23.15 NUMERIC WORD TO A REAL*8
C       VALUE. CHARACTER VALUE IS 23 SPACES LONG
C
          REAL*8 VL
C
          INTEGER I,DEC
C
          CHARACTER B*140,AVL*23
C
          COMMON/JK_ATD/AVL,VL
          DEC=0
          DO I=1,23
              IF(AVL(I:I).EQ.'.') DEC=1
          END DO
          IF(DEC.EQ.0) AVL(1:23)=AVL(2:23)//'.'
C
          WRITE(B,110) AVL
          READ(B,100) VL
! 10             CONTINUE
 100      FORMAT(D23.15)
 110      FORMAT(A23)
          RETURN
      END
C SUB DTOACV.FOR
      SUBROUTINE DTOACV
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT D23.15 TO A23
C
          REAL*8 VL
C
          CHARACTER B*140,AVL*23
C
          COMMON/JK_ATD/AVL,VL
C
          WRITE(B,100) VL
          READ(B,110) AVL
! 10             CONTINUE
 100      FORMAT(D23.15)
 110      FORMAT(A23)
          RETURN
      END
C SUB DTOA.FOR
      SUBROUTINE DTOA(VAL,AVAL)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 TO G13.6
C
          REAL*8 VAL
C
          CHARACTER B*140,AVAL*13
C
C
          WRITE(B,100) VAL
          READ(B,110) AVAL
! 10             CONTINUE
 100      FORMAT(G13.6)
 110      FORMAT(A13)
          RETURN
      END
C SUB DTOA23.FOR
      SUBROUTINE DTOA23(VAL,AVAL)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 TO G23.15
C
          REAL*8 VAL
C
          CHARACTER B*140,AVAL*23
C
C
          WRITE(B,100) VAL
          READ(B,110) AVAL
! 10             CONTINUE
 100      FORMAT(G23.15)
 110      FORMAT(A23)
          RETURN
      END
C SUB ATODMC.FOR
      SUBROUTINE ATODMC(AVL,VL,ERROR)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT CHARACTER REPRESENTATION OF
C       A D23.15 NUMERIC WORD TO A REAL*8
C       VALUE. CHARACTER VALUE IS 23 SPACES LONG
C
          REAL*8 VL
C
          LOGICAL ERROR,ED,PNT
C
          INTEGER I,J
C
          CHARACTER B*140,AVL*23
C
          ED=.FALSE.
          PNT=.FALSE.
          DO I=23,1,-1
              IF(AVL(I:I).EQ.'D'.OR.AVL(I:I).EQ.'E') ED=.TRUE.
              J=I
C     FOUND AN E OR D
              IF(AVL(I:I).EQ.'.') PNT=.TRUE.
C     FOUND A DECIMAL
          END DO
C
          IF(.NOT.PNT) THEN
C     NO DECIMAL EXISTS
              IF(.NOT.ED) THEN
C     NO E OR D EXISTS
                  DO I=23,1,-1
                      IF(AVL(I:I).NE.' ') THEN
C     I:I IS NOT A BLANK
C
                          IF(I.EQ.23) THEN
                              AVL(23:23)='.'
                          ELSE
C     I NOT 23
                              AVL(I+1:I+1)='.'
                          END IF
                          GO TO 66
                      END IF
                  END DO
                  AVL='0.0D0                  '
              ELSE
C     ED
C     AN E OR D EXISTS AT J, PLACE A DECIMAL INFRONT OF IT
                  AVL(1:23)=AVL(1:J-1)//'.'//AVL(J:22)
              END IF
          END IF
 66       CONTINUE
C
          ERROR=.FALSE.
          WRITE(B,110,ERR=98) AVL
          READ(B,100,ERR=98) VL
! 10             CONTINUE
 100      FORMAT(D23.15)
 110      FORMAT(A23)
          RETURN
 98       VL=0.0D0
          ERROR=.TRUE.
          RETURN
      END

C SUB NTOA3.FOR
      SUBROUTINE NTOA3
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 NUMERIC WORDS
C       TO CHARACTER VARIABLES
C
          REAL*8 VAL
C
          CHARACTER C*140,AVAL*23
C
          COMMON/JK_NTA3/VAL,AVAL
C
          WRITE(C,250) VAL
          READ(C,300) AVAL
! 10             CONTINUE
 250      FORMAT(D23.15)
 300      FORMAT(A23)
          RETURN
      END

C SUB NTOAN1.FOR
      SUBROUTINE NTOAN1(IVAL,AIVAL)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT THREE DIGIT INTEGER NUMERIC WORDS
C       TO THREE CHARACTER, CHARACTER VARIABLES
C
          INTEGER IVAL
C
          CHARACTER CEME*140,AIVAL*3
C
          WRITE(CEME,250) IVAL
          READ(CEME,300) AIVAL(1:3)
! 10             CONTINUE
 250      FORMAT(I3)
 300      FORMAT(A3)
          RETURN
      END

C SUB NTOAN2.FOR
      SUBROUTINE NTOAN2(DVAL,AVAL10)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT 10 DIGIT REAL*8 NUMERIC WORDS
C       TO ELEVEN CHARACTER,LEFT JUSTIFIED, CHARACTER VARIABLES
C
          REAL*8 DVAL
C
          CHARACTER C*140,AVAL10*10
C
          WRITE(C,250) DVAL
          READ(C,300) AVAL10(1:10)
 250      FORMAT(G10.3)
 300      FORMAT(A10)
          RETURN
      END
C SUB ATON3.FOR
      SUBROUTINE ATON3
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT CHARACTER REPRESENTATION OF
C       A D23.15 NUMERIC WORD TO A REAL*8
C       VALUE. CHARACTER VALUE IS 23 SPACES LONG
C
          REAL*8 VAL
C
          CHARACTER B*140,AVAL*23
C
          COMMON/JK_NTA3/VAL,AVAL
C
          WRITE(B,110) AVAL
          READ(B,100) VAL
! 10             CONTINUE
 100      FORMAT(D23.15)
 110      FORMAT(A23)
          RETURN
      END
C SUB NTA.FOR
      SUBROUTINE NTA(N1,N2,N3,N4,N5,AN1,AN2,AN3,AN4,AN5)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 NUMERIC WORDS
C       TO CHARACTER VARIABLES, THIS IS CALLED FROM "CWRITE"
C       DURING THE "COMMAND WRITING PROCESS"
C
          REAL*8 N1,N2,N3,N4,N5,N
C
          INTEGER I
C
          CHARACTER B*140,AN1*23,AN2*23,AN3*23,AN4*23,AN5*23,AN*23

C
          DO 10 I=1,5
              IF(I.EQ.1) N=N1
              IF(I.EQ.2) N=N2
              IF(I.EQ.3) N=N3
              IF(I.EQ.4) N=N4
              IF(I.EQ.5) N=N5
              WRITE(B,100) N
              READ(B,200) AN
              IF(I.EQ.1) AN1=AN
              IF(I.EQ.2) AN2=AN
              IF(I.EQ.3) AN3=AN
              IF(I.EQ.4) AN4=AN
              IF(I.EQ.5) AN5=AN
 10       CONTINUE
 100      FORMAT(D23.15)
 200      FORMAT(A23)
          RETURN
      END
C SUB NTA12.FOR
      SUBROUTINE NTA12(N,AN)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 NUMERIC WORDS
C       TO CHARACTER VARIABLES. THIS IS CALLED DURING THE TABLE
C       WRITER PRINT PROCESS.
C
          REAL*8 N
C
          CHARACTER B*140,AN*12

C
          WRITE(B,100) N
          READ(B,200) AN
 100      FORMAT(G12.5)
 200      FORMAT(A12)
          IF(N.EQ.0.0D0) AN='0.0000000000'
          RETURN
      END
C SUB AUXATN.FOR
      SUBROUTINE AUXATN
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT A CHARACTER*23 REPRESENTATION OF
C       A REAL*8 NUMERIC WORD
C       INTO A REAL*8 NUMERIC WORD
C
          REAL*8 N1
C
          CHARACTER B*140,AN1*23
C
          COMMON/CAUX1/N1,AN1
C
          WRITE(B,200) AN1
          READ(B,100) N1
 100      FORMAT(D23.15)
 200      FORMAT(A23)
          RETURN
      END
C SUB AUXNTA.FOR
      SUBROUTINE AUXNTA
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT A REAL*8 NUMERIC WORD
C       TO A CHARACTER*23, CHARACTER VARIABLE
C
          REAL*8 N1
C
          CHARACTER B*140,AN1*23
C
          COMMON/CAUX1/N1,AN1
C
          WRITE(B,100) N1
          READ(B,200) AN1
 100      FORMAT(D23.15)
 200      FORMAT(A23)
          RETURN
      END
C SUB ITOA.FOR
      SUBROUTINE ITOA(I,AI)
C
          IMPLICIT NONE
C
          INTEGER I
C
          CHARACTER B*80,AI*2
C
          WRITE(B,100) I
          READ(B,200) AI
 100      FORMAT(I2)
 200      FORMAT(A2)
          RETURN
      END
C SUB ITOA6.FOR
      SUBROUTINE ITOA6(I,AI)
C
          IMPLICIT NONE
C
          INTEGER I
C
          CHARACTER B*80,AI*6
C
          WRITE(B,100) I
          READ(B,200) AI
 100      FORMAT(I6)
 200      FORMAT(A6)
          RETURN
      END
C SUB ITOAA.FOR
      SUBROUTINE ITOAA(I,AI3)
C
          IMPLICIT NONE
C
          INTEGER I
C
          CHARACTER B*80,AI3*3,AI2*2,AI1*1
C
          IF(I.GE.0.AND.I.LT.10) THEN
              WRITE(B,101) I
              READ(B,201) AI1
              AI3(1:2)='00'
              AI3(3:3)=AI1(1:1)
 101          FORMAT(I1)
 201          FORMAT(A1)
              RETURN
          END IF
          IF(I.GE.10.AND.I.LT.100) THEN
              WRITE(B,102) I
              READ(B,202) AI2
              AI3(1:1)='0'
              AI3(2:3)=AI2(1:2)
 102          FORMAT(I2)
 202          FORMAT(A2)
              RETURN
          END IF
          IF(I.GE.100.AND.I.LT.999) THEN
              WRITE(B,103) I
              READ(B,203) AI3
 103          FORMAT(I3)
 203          FORMAT(A3)
              RETURN
          END IF
      END
C SUB ITOA2.FOR
      SUBROUTINE ITOA2(I,AI2)
C       0 TO 99 CONVERTED TO STRING '00' TO STRING '99'
C
          IMPLICIT NONE
C
          INTEGER I
C
          CHARACTER B*80,AI2*2,AI1*1
C
          IF(I.GE.0.AND.I.LT.10) THEN
              WRITE(B,101) I
              READ(B,201) AI1
              AI2(1:1)='0'
              AI2(2:2)=AI1(1:1)
 101          FORMAT(I1)
 201          FORMAT(A1)
              RETURN
          END IF
          IF(I.GE.10.AND.I.LT.100) THEN
              WRITE(B,102) I
              READ(B,202) AI2
 102          FORMAT(I2)
 202          FORMAT(A2)
              RETURN
          END IF
      END
C SUB ATOII.FOR
      SUBROUTINE ATOII(AI3,I)
C
          IMPLICIT NONE
C
          INTEGER I
C
          CHARACTER B*80,AI3*3
C
          WRITE(B,201) AI3
          READ(B,101) I
 101      FORMAT(I3)
 201      FORMAT(A3)
          RETURN
      END
C SUB ATOI4.FOR
      SUBROUTINE ATOI4(AI4,I)
C
          IMPLICIT NONE
C
          INTEGER I
C
          CHARACTER B*80,AI4*4
C
          WRITE(B,201) AI4
          READ(B,101) I
 101      FORMAT(I4)
 201      FORMAT(A4)
          RETURN
      END
C SUB ITOA4.FOR
      SUBROUTINE ITOA4(AI4,I)
C
          IMPLICIT NONE
C
          INTEGER I
C
          CHARACTER B*80,AI4*4,A1*1,A2*2,A3*3,A4*4
C
          AI4='****'
          IF(I.LT.10) THEN
              WRITE(B,101) I
              READ(B,201) A1
 101          FORMAT(I1)
 201          FORMAT(A1)
              AI4='000'//A1
              RETURN
          END IF
          IF(I.GE.10.AND.I.LT.100) THEN
              WRITE(B,102) I
              READ(B,202) A2
 102          FORMAT(I2)
 202          FORMAT(A2)
              AI4='00'//A2
              RETURN
          END IF
          IF(I.GE.100.AND.I.LT.1000) THEN
              WRITE(B,103) I
              READ(B,203) A3
 103          FORMAT(I3)
 203          FORMAT(A3)
              AI4='0'//A3
              RETURN
          END IF
          IF(I.GE.1000.AND.I.LT.4001) THEN
              WRITE(B,104) I
              READ(B,204) A4
 104          FORMAT(I4)
 204          FORMAT(A4)
              AI4=A4
              RETURN
          END IF

      END
C SUB ITOAAAAAA.FOR
      SUBROUTINE ITOAAAAAA(I,AI6)
C
          IMPLICIT NONE
C
          INTEGER I
C
          CHARACTER B*80,A1*1,A2*2,A3*3,A4*4
          CHARACTER A5*5
          CHARACTER AI6*6,A6*6
          AI6='000000'
          IF(I.LT.10) THEN
              WRITE(B,101) I
              READ(B,201) A1
 101          FORMAT(I1)
 201          FORMAT(A1)
              AI6='00000'//A1
              RETURN
          END IF
          IF(I.LT.100) THEN
              WRITE(B,102) I
              READ(B,202) A2
 102          FORMAT(I2)
 202          FORMAT(A2)
              AI6='0000'//A2
              RETURN
          END IF
          IF(I.LT.1000) THEN
              WRITE(B,103) I
              READ(B,203) A3
 103          FORMAT(I3)
 203          FORMAT(A3)
              AI6='000'//A3
              RETURN
          END IF
          IF(I.LT.10000) THEN
              WRITE(B,104) I
              READ(B,204) A4
 104          FORMAT(I4)
 204          FORMAT(A4)
              AI6='00'//A4
              RETURN
          END IF
          IF(I.LT.100000) THEN
              WRITE(B,105) I
              READ(B,205) A5
 105          FORMAT(I5)
 205          FORMAT(A5)
              AI6='0'//A5
              RETURN
          END IF
          IF(I.LT.1000000) THEN
              WRITE(B,106) I
              READ(B,206) A6
 106          FORMAT(I6)
 206          FORMAT(A6)
              AI6=A6
              RETURN
          END IF
      END
C SUB ITOAAA.FOR
      SUBROUTINE ITOAAA(I,AI4)
C
          IMPLICIT NONE
C
          INTEGER I
C
          CHARACTER B*80,AI4*4,AI3*3,AI2*2,AI1*1
C
          IF(I.GE.0.AND.I.LT.10) THEN
              WRITE(B,101) I
              READ(B,201) AI1
              AI4(1:4)='    '
              AI4(1:1)=AI1(1:1)
 101          FORMAT(I1)
 201          FORMAT(A1)
              RETURN
          END IF
          IF(I.GE.10.AND.I.LT.100) THEN
              WRITE(B,102) I
              READ(B,202) AI2
              AI4(1:4)='    '
              AI4(1:2)=AI2(1:2)
 102          FORMAT(I2)
 202          FORMAT(A2)
              RETURN
          END IF
          IF(I.GE.100.AND.I.LT.999) THEN
              WRITE(B,103) I
              READ(B,203) AI3
              AI4(1:4)='    '
              AI4(1:3)=AI3(1:3)
 103          FORMAT(I3)
 203          FORMAT(A3)
              RETURN
          END IF
          IF(I.GE.1000.AND.I.LT.9999) THEN
              WRITE(B,104) I
              AI4(1:4)='    '
              READ(B,204) AI4
 104          FORMAT(I4)
 204          FORMAT(A4)
              RETURN
          END IF
      END
C SUB I3TOA3.FOR
      SUBROUTINE I3TOA3(I,AI3)
C
          IMPLICIT NONE
C
          INTEGER I
C
          CHARACTER B*80,AI3*3,AI2*2,AI1*1
C
          IF(I.GE.0.AND.I.LT.10) THEN
              WRITE(B,101) I
              READ(B,201) AI1
              AI3(1:2)='  '
              AI3(3:3)=AI1(1:1)
 101          FORMAT(I1)
 201          FORMAT(A1)
              RETURN
          END IF
          IF(I.GE.10.AND.I.LT.100) THEN
              WRITE(B,102) I
              READ(B,202) AI2
              AI3(1:1)=' '
              AI3(2:3)=AI2(1:2)
 102          FORMAT(I2)
 202          FORMAT(A2)
              RETURN
          END IF
          IF(I.GE.100.AND.I.LT.999) THEN
              WRITE(B,103) I
              READ(B,203) AI3
 103          FORMAT(I3)
 203          FORMAT(A3)
              RETURN
          END IF
      END
C SUB ATODCODEV.FOR
      SUBROUTINE ATODCODEV(AVL,VL,CVERROR)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT CHARACTER REPRESENTATION OF
C       A D23.15 NUMERIC WORD TO A REAL*8
C       VALUE. CHARACTER VALUE IS 23 SPACES LONG
C      THIS IS USED IN THE CODE V TO PRG TRANSLATOR CV2PRG.FOR
C
          REAL*8 VL
C
          INTEGER I
C
          LOGICAL CVERROR,NODEC
C
          CHARACTER B*140,AVL*23
C
          CVERROR=.FALSE.
C
C     IS THERE A MISSING DECIMAL
          NODEC=.TRUE.
          DO I=1,23
              IF(AVL(I:I).EQ.'.') NODEC=.FALSE.
          END DO
          IF(NODEC) AVL(1:23)=AVL(2:23)//'.'
C
          WRITE(B,110,ERR=98) AVL
          READ(B,*,ERR=98) VL
! 10             CONTINUE
 110      FORMAT(A23)
          RETURN
  98      CVERROR=.TRUE.
          RETURN
      END
C SUB ATOICODEV.FOR
      SUBROUTINE ATOICODEV(AVL,IVL,CVERROR)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT CHARACTER REPRESENTATION OF
C       AN INTEGER NUMERIC WORD TO AN INTEGER
C       VALUE. CHARACTER VALUE IS 23 SPACES LONG
C      THIS IS USED IN THE CODE V TO PRG TRANSLATOR CV2PRG.FOR
C
          INTEGER IVL
C
          CHARACTER B*140,AVL*23
C
          LOGICAL CVERROR
C
          WRITE(B,110,ERR=98) AVL
          READ(B,*,ERR=98) IVL
! 10             CONTINUE
 110      FORMAT(A23)
          RETURN
  98      CVERROR=.TRUE.
          RETURN
      END
C SUB ATODZMX.FOR
      SUBROUTINE ATODZMX(AVL,VL,ZMXERROR)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT CHARACTER REPRESENTATION OF
C       A D23.15 NUMERIC WORD TO A REAL*8
C       VALUE. CHARACTER VALUE IS 23 SPACES LONG
C      THIS IS USED IN THE ZEMAX TO PRG TRANSLATOR ZMX2PRG.FOR
C
          REAL*8 VL
C
C
          INTEGER I,DEC
          LOGICAL ZMXERROR
C
          CHARACTER B*140,AVL*23
          ZMXERROR=.FALSE.
          DEC=0
          DO I=1,23
              IF(AVL(I:I).EQ.'.') DEC=1
          END DO
          IF(DEC.EQ.0) AVL(1:23)=AVL(2:23)//'.'
C
          WRITE(B,110,ERR=98) AVL
          READ(B,*,ERR=98) VL
! 10             CONTINUE
 110      FORMAT(A23)
          RETURN
  98      ZMXERROR=.TRUE.
          RETURN
      END
C SUB ATOIZMX.FOR
      SUBROUTINE ATOIZMX(AVL,IVL,ZMXERROR)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT CHARACTER REPRESENTATION OF
C       AN INTEGER NUMERIC WORD TO AN INTEGER
C       VALUE. CHARACTER VALUE IS 23 SPACES LONG
C      THIS IS USED IN THE ZEMAX TO PRG TRANSLATOR ZMX2PRG.FOR
C
          INTEGER IVL
C
          CHARACTER B*140,AVL*23
C
          LOGICAL ZMXERROR
C
          INCLUDE 'datmai.inc'
          IF(AVL.EQ.'                       ') THEN
              ZMXERROR=.TRUE.
              RETURN
          END IF
C
          WRITE(B,110,ERR=98) AVL
          READ(B,*,ERR=98) IVL
! 10             CONTINUE
 110      FORMAT(A23)
          RETURN
  98      ZMXERROR=.TRUE.
          RETURN
      END
C SUB GGPREG_SAVE.FOR
C
      SUBROUTINE GGPREG_SAVE
          IMPLICIT NONE

C       THIS SUBROUTINE IS USED FOR OUTPUTING OF GENERAL REGISTER VALUES
C       IN A FORM WHICH CAN BE READ BACK INTO THE PROGRAM
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE='"SAVEREG" OUTPUTS THE VALUES OF THE GENERAL PURPOSE'
              CALL SHOWIT(1)
              OUTLYNE='REGISTERS FROM INT(W1) TO INT(W2) IN A FORMAT WHICH MAY'
              CALL SHOWIT(1)
              OUTLYNE='BE READ BY THE PROGRAM'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SQ.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"SAVEREG" ONLY TAKES NUMERIC WORDS #1 AND #2'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"SAVEREG" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).LT.1) THEN
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #1 MAY NOT BE LESS THAN 1'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W2).GT.MAXREG) THEN
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #2 MAY NOT BE GREATER THAN ',MAXREG
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(INT(W1).GT.INT(W2)) THEN
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORD #1 MAY NOT BE LARGER THAN NUMERIC WORD #2'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          DO I=INT(W1),INT(W2)
              WRITE(OUTLYNE,1000) I,GPREG(I)
              CALL SHOWIT(0)
          END DO
 1000     FORMAT('STO ',I6,',',D23.15)
          RETURN
      END
C SUB GGPREG.FOR
C
      SUBROUTINE GGPREG
          IMPLICIT NONE

C       THIS SUBROUTINE IS USED FOR GENERAL REGISTERS
C
          INTEGER IW,I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
          IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        'GENERAL PURPOSE REGISTER COMMANDS ONLY TAKE QUALIFIER AND'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'NUMERIC WORDS #1 AND (OPTIONALLY) #2 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.NE.'CLGREG'.AND.WC.NE.'CLSTREG') THEN
              IF(DF1.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            'GENERAL PURPOSE REGISTER COMMANDS REQUIRE EXPLICIT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'CLGREG') THEN
C       CLEAR ALL GENERAL REGISTERS
C
              I=MAXREG
              GPREG(0:I)=0.0D0
              AGPREG(0:I)=AA//AA//AA//AA
              CALL REGVAR_FIX
              RETURN
          END IF
          IF(WC.EQ.'CLSTREG')THEN
C       THIS IS USED TO CLEAR GENERAL PURPOSE
C       REGISTERS GPREG(151) TO GPREG(MAXREG) WHICH ARE USED
C       FOR THE STATISTICAL OPREATIONS. THESE SHOULD BE CLEARED
C       BEFORE NEW STATISTICAL OPERATIONS ARE STARTED.
              I=MAXREG
              GPREG(151:I)=0.0D0
              CALL REGVAR_FIX
              RETURN
          END IF
          IF(WC.EQ.'STO') THEN
C       THIS IS USED TO STORE THE NUMBER WHICH IS
C       IN THE ACCUMULATOR INTO ONE OF THE MAXREG GENERAL PURPOSE
C       STORAGE REGISTERS.
C
C       REGISTER MEMORY IS PASSED IN MEM2 COMMON.
C
C       WQ IS USED TO SPECIFY STORAGE REGISTER ARITHMETIC.
C
C       VALID QUALIFIER WORDS ARE: PLUS
C                                  MINUS
C                                  MPY
C                                  DIV
C
C        QUALIFIER                     VALUE TRANSFERED REGISTER
C
C         PLUS                          REGISTER N + ACC
C         MINUS                         REGISTER N - ACC
C         MPY                           REGISTER N * ACC
C         DIV                           REGISTER N / ACC
C        (NONE)                            ONLY      ACC
C
              IW=INT(W1)
              IF(IW.LT.1.OR.IW.GT.MAXREG) THEN
                  WRITE(OUTLYNE,*)'INVALID GENERAL PURPOSE REGISTER NUMBER'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              IF(DF2.EQ.1) THEN
C     USE ACCUMULATOR VALUE)
                  IF(WQ.EQ.'PLUS') THEN
                      GPREG(IW)=GPREG(IW)+REG(9)
                      CALL REGVAR_FIX
                      RETURN
                  END IF
                  IF(WQ.EQ.'MINUS') THEN
                      GPREG(IW)=GPREG(IW)-REG(9)
                      CALL REGVAR_FIX
                      RETURN
                  END IF
                  IF(WQ.EQ.'MPY') THEN
                      GPREG(IW)=GPREG(IW)*REG(9)
                      CALL REGVAR_FIX
                      RETURN
                  END IF
                  IF(WQ.EQ.'DIV') THEN
                      IF(REG(9).EQ.0.0D0) THEN
                          WRITE(OUTLYNE,*)'DIVIDE BY ZERO IS NOT ALLOWED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'VALUE OF REGISTER ',IW,' SET TO ZERO'
                          CALL SHOWIT(1)
                          GPREG(IW)=0.0D0
                          CALL REGVAR_FIX
                          CALL MACFAL
                          RETURN
                      ELSE
                          GPREG(IW)=GPREG(IW)/REG(9)
                          CALL REGVAR_FIX
                          RETURN
                      END IF
                  END IF
                  IF(WQ.EQ.' ') THEN
                      GPREG(IW)=REG(9)
                      CALL REGVAR_FIX
                      RETURN
                  END IF
                  WRITE(OUTLYNE,*)'INVALID GENERAL PURPOSE REGISTER OPERATION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C     USE W2 VALUE
                  IF(WQ.EQ.'PLUS') THEN
                      GPREG(IW)=GPREG(IW)+W2
                      CALL REGVAR_FIX
                      RETURN
                  END IF
                  IF(WQ.EQ.'MINUS') THEN
                      GPREG(IW)=GPREG(IW)-W2
                      CALL REGVAR_FIX
                      RETURN
                  END IF
                  IF(WQ.EQ.'MPY') THEN
                      GPREG(IW)=GPREG(IW)*W2
                      CALL REGVAR_FIX
                      RETURN
                  END IF
                  IF(WQ.EQ.'DIV') THEN
                      IF(W2.EQ.0.0D0) THEN
                          WRITE(OUTLYNE,*)'DIVIDE BY ZERO IS NOT ALLOWED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'VALUE OF REGISTER ',IW,' SET TO ZERO'
                          CALL SHOWIT(1)
                          GPREG(IW)=0.0D0
                          CALL REGVAR_FIX
                          CALL MACFAL
                          RETURN
                      ELSE
                          GPREG(IW)=GPREG(IW)/W2
                          CALL REGVAR_FIX
                          RETURN
                      END IF
                  END IF
                  IF(WQ.EQ.' ') THEN
                      GPREG(IW)=W2
                      CALL REGVAR_FIX
                      RETURN
                  END IF
                  WRITE(OUTLYNE,*)'INVALID GENERAL PURPOSE REGISTER OPERATION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              RETURN
          END IF
          IF(WC.EQ.'RCL') THEN
C       THIS IS USED TO RECALL TO THE ACCUMULATOR
C       THE VALUE IN ONE OF THE SPECIFIC MAXREG GENERAL PURPOSE
C       STORAGE REGISTERS.
C
C       REGISTER MEMORY IS PASSED IN MEM2 COMMON.
C
C       WQ IS USED TO SPECIFY STORAGE REGISTER ARITHMETIC.
C
C       VALID QUALIFIER WORDS ARE: PLUS
C                                  MINUS
C                                  MPY
C                                  DIV
C
C        QUALIFIER                     VALUE TRANSFERED TO ACC
C
C         PLUS                          REGISTER N + ACC
C         MINUS                         REGISTER N - ACC
C         MPY                           REGISTER N * ACC
C         DIV                           REGISTER N / ACC
C        (NONE)                         REGISTER N (ONLY)
C
              IW=INT(W1)
              IF(IW.LT.1.OR.IW.GT.MAXREG) THEN
                  WRITE(OUTLYNE,*)'INVALID GENERAL PURPOSE REGISTER NUMBER'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              REG(40)=REG(9)
              IF(WQ.EQ.'PLUS') THEN
                  REG(9)=GPREG(IW)+REG(9)
              ELSE
                  IF(WQ.EQ.'MINUS') THEN
                      REG(9)=GPREG(IW)-REG(9)
                  ELSE
                      IF(WQ.EQ.'MPY') THEN
                          REG(9)=GPREG(IW)*REG(9)
                      ELSE
                          IF(WQ.EQ.'DIV') THEN
                              IF(REG(9).EQ.0.0) THEN
                                  WRITE(OUTLYNE,*) 'DIVIDE BY ZERO IN ACCUMULATOR NOT ALLOWED'
                                  CALL SHOWIT(1)
                                  WRITE(OUTLYNE,*) 'ACCUMULATOR REMAINS ZERO'
                                  CALL SHOWIT(1)
                                  REG(9)=0.0
                              ELSE
                                  REG(9)=GPREG(IW)/REG(9)
                              END IF
                          ELSE
                              IF(WQ.EQ.' ') THEN
                                  REG(9)=GPREG(IW)
                              ELSE
                                  WRITE(OUTLYNE,*)'INVALID GENERAL PURPOSE REGISTER OPERATION'
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
              RETURN
          END IF
          IF(WC.EQ.'STADD') THEN
C       THIS IS USED ADD A NEW PAIR OF STATISTICAL
C       INPUT VALUES TO THE STATISTICAL REGISTERS.
C
C       THE DISPOSITION OF THE STATISTICAL REGISTERS
C       IS AS FOLLOWS:
C
C               GPREG(151) - NUMBER OF DATA POINTS ACCUMULATED
C               GPREG(152) - SUMMATION OF X-VALUES
C               GPREG(153) - SUMMATION OF SQUARES OF X-VALUES
C               GPREG(154) - SUMMATION OF Y-VALUES
C               GPREG(155) - SUMMATION OF SQUARES OF Y-VALUES
C               GPREG(156) - SUMMATION OF PRODUCT X*Y
C               GPREG(157) - SUMMATION OF PRODUCT (X**2)*Y
C               GPREG(158) - SUMMATION OF PRODUCT X*(Y**2)
C               GPREG(159) _ SUMMATION OF CUBES OF X-VALUES
C               GPREG(160) - SUMMATION OF CUBES OF Y-VALUES
              GPREG(151)=GPREG(151)+1.0D0
              GPREG(152)=GPREG(152)+REG(9)
              GPREG(153)=GPREG(153)+(REG(9)*REG(9))
              GPREG(154)=GPREG(154)+REG(10)
              GPREG(155)=GPREG(155)+(REG(10)*REG(10))
              GPREG(156)=GPREG(156)+(REG(9)*REG(10))
              GPREG(157)=GPREG(157)+(REG(9)*REG(9)*REG(10))
              GPREG(158)=GPREG(158)+(REG(9)*REG(10)*REG(10))
              GPREG(159)=GPREG(159)+(REG(9)*REG(9)*REG(9))
              GPREG(160)=GPREG(160)+(REG(10)*REG(10)*REG(10))
              CALL REGVAR_FIX
              RETURN
          END IF
          IF(WC.EQ.'STSUB') THEN
C       THIS IS USED REMOVE A  PAIR OF STATISTICAL
C       INPUT VALUES FROM THE STATISTICAL REGISTERS.
C
C       THE DISPOSITION OF THE STATISTICAL REGISTERS
C       IS AS FOLLOWS:
C
C               GPREG(151) - NUMBER OF DATA POINTS ACCUMULATED
C               GPREG(152) - SUMMATION OF X-VALUES
C               GPREG(153) - SUMMATION OF SQUARES OF X-VALUES
C               GPREG(154) - SUMMATION OF Y-VALUES
C               GPREG(155) - SUMMATION OF SQUARES OF Y-VALUES
C               GPREG(156) - SUMMATION OF PRODUCT X*Y
C               GPREG(157) - SUMMATION OF PRODUCT (X**2)*Y
C               GPREG(158) - SUMMATION OF PRODUCT X*(Y**2)
C               GPREG(159) _ SUMMATION OF CUBES OF X-VALUES
C               GPREG(160) - SUMMATION OF CUBES OF Y-VALUES
              GPREG(151)=GPREG(151)-1.0D0
              GPREG(152)=GPREG(152)-REG(9)
              GPREG(153)=GPREG(153)-(REG(9)*REG(9))
              GPREG(154)=GPREG(154)-REG(10)
              GPREG(155)=GPREG(155)-(REG(10)*REG(10))
              GPREG(156)=GPREG(156)-(REG(9)*REG(10))
              GPREG(157)=GPREG(157)-(REG(9)*REG(9)*REG(10))
              GPREG(158)=GPREG(158)-(REG(9)*REG(10)*REG(10))
              GPREG(159)=GPREG(159)-(REG(9)*REG(9)*REG(9))
              GPREG(160)=GPREG(160)-(REG(10)*REG(10)*REG(10))
              CALL REGVAR_FIX
              RETURN
          END IF
          IF(WC.EQ.'STDEV') THEN
C       THIS IS USED CALCULATE THE STANDARD DEVIATION
C       OF X AND Y
C       FROM THE CURRENT DATA STORED IN THE STATISTICAL REGISTERS.
C       THE RESULTS ARE STORED IN GENERAL REGISTERS 163 AND 164
C       AND ALSO STORED IN REG(9),THE ACCUMULATOR, AND REG(10).
C       STDEV OF X IN REG(9) AND STDEV OF Y IN REG(10)
C
C       STDEV OF X
C
              IF(GPREG(151).LE.1.0) THEN
                  WRITE(OUTLYNE,*)
     1            'STATISTICAL CALCULATIONS REQUIRE MORE THAN ONE ENTRY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'IN THE STATISTICAL DATA BASE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            INT(GPREG(151)),' ENTRIES EXISTS IN CURRENT DATA BASE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              REG(40)=REG(9)
              GPREG(163)=(GPREG(153)-((GPREG(152)**2)/GPREG(151)))
              GPREG(163)=(GPREG(163)/(GPREG(151)-1.0))
              CALL REGVAR_FIX
              REG(9)=DSQRT(GPREG(163))
C
C       STDEV OF Y
C
              GPREG(164)=(GPREG(155)-((GPREG(154)**2)/GPREG(151)))
              GPREG(164)=(GPREG(164)/(GPREG(151)-1.0))
              CALL REGVAR_FIX
              REG(10)=DSQRT(GPREG(164))
              RETURN
          END IF
          IF(WC.EQ.'MEAN') THEN
C       THIS IS USED CALCULATE THE MEAN OF X AND Y
C       FROM THE CURRENT DATA STORED IN THE STATISTICAL REGISTERS.
C       THE RESULTS ARE STORED IN GENERAL REGISTERS 161 AND 162
C       AND ALSO STORED IN REG(9),THE ACCUMULATOR, AND REG(10).
C       MEAN X IN REG(9) AND MEAN Y IN REG(10)
              IF(GPREG(151).LE.1.0) THEN
                  WRITE(OUTLYNE,*)
     1            'STATISTICAL CALCULATIONS REQUIRE MORE THAN ONE ENTRY'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'IN THE STATISTICAL DATA BASE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            INT(GPREG(151)),' ENTRIES EXISTS IN CURRENT DATA BASE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C       MEAN OF X
C
              GPREG(161)=GPREG(152)/GPREG(151)
              CALL REGVAR_FIX
              REG(40)=REG(9)
              REG(9)=GPREG(161)
C
C       MEAN OF Y
C
              GPREG(162)=GPREG(154)/GPREG(151)
              CALL REGVAR_FIX
              REG(10)=GPREG(162)
              RETURN
          END IF
          CALL REGVAR_FIX
          RETURN
      END
      SUBROUTINE REGVAR_FIX
          IMPLICIT NONE
          INTEGER I
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
          IF(VBCNT.NE.0) THEN
              DO I=1,VBCNT
                  IF(INT(VARABL(I,1)).EQ.150) THEN
                      VARABL(I,4)=GPREG(INT(VARABL(I,3)))
                  END IF
              END DO
          END IF
          RETURN
      END
      SUBROUTINE FIXCURLENS
          IMPLICIT NONE
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datmac.inc'
          INTEGER OLDOUT,OLDSQ
          CHARACTER*8 WCOLD
C       NOW OUTPUT LENS TO LENSTEXT.DAT
C       IF SYSP(20) NOT 0.0D0
          IF(SYSP(20).NE.0.0D0) THEN
C
              OPEN(UNIT=89,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'CURLENS/LENSTEXT.DAT'
     2        ,STATUS='UNKNOWN')
              CALL CLOSE_FILE(89,0)
              OPEN(UNIT=89,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'CURLENS/LENSTEXT.DAT'
     2        ,STATUS='UNKNOWN')
C
              OLDOUT=OUT
              OLDSQ=SQ
              OUT=89
C     SET SQ=0
              SQ=0
              SST=0
              SN=0
              WCOLD=WC
              WC='LENO'
C     CALL LENOUT
              CALL LENOUT
              WC=WCOLD
              OUT=OLDOUT
C
              REWIND(UNIT=89)
              CALL CLOSE_FILE(89,1)
          ELSE
C       NO LENS DATA WAS AVAILABLE TO OUTPUT
          END IF
          RETURN
      END
