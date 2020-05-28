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

C       FOURTH SET OF OPTIMIZATION ROUTINES

C SUB TOLREST.FOR
      SUBROUTINE TOLREST
C
          IMPLICIT NONE
C
          INTEGER I,VTYPE,ALTYPE
C
          CHARACTER AV1*23
C
          REAL*8 V1
C
!      LOGICAL NOP
C

C
          COMMON/CAUX1/V1,AV1
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
C       THIS IS SUBROUTINE TOLRESTO. THIS IS THE SUBROUTINE WHICH
C     DOES A RESTOR DURING TOLERANCING
C
C
          IF(.NOT.SOLEXT.OR.VBCNT.EQ.0.OR.OPCNT.EQ.0) THEN
C     JUST RETURN, NOTHING TO RESTORE
              RETURN
          END IF

C     RESTORE ALL VARIABLE VALUES TO THEIR
C     ORIGINAL VALUES.
          I=VBCNT
          VARABL(1:I,4)=VARABL(1:I,13)
          VARABL(1:I,5)=VARABL(1:I,13)
          VARABL(1:I,6)=0.0D0
C
C     NOW APPLY THE CHANGE VECTOR TO THE LENS
C**********************************************************************
          DO I=1,VBCNT
C     THIS IS A LENS LEVEL VARIABLE CHANGE
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
              VTYPE=INT(VARABL(I,1))
C                          CURVATURE
              IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                  V1=VARABL(I,4)
                  ALENS(1,INT(VARABL(I,3)))=V1
C                       CURVATURE DONE
              END IF
              IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                  V1=VARABL(I,4)
                  ALENS(24,INT(VARABL(I,3)))=V1
C                       TORIC CURVATURE DONE
              END IF
              IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                  IF(VTYPE.EQ.3) ALTYPE=3
                  IF(VTYPE.EQ.4) ALTYPE=2
                  IF(VTYPE.EQ.5) ALTYPE=4
                  IF(VTYPE.EQ.6) ALTYPE=5
                  IF(VTYPE.EQ.7) ALTYPE=6
                  IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                  V1=VARABL(I,4)
                  ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C                       THESE VARIABLES DONE
              END IF
              IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1        124.AND.VTYPE.LE.149) THEN
                  IF(VTYPE.EQ.11) ALTYPE=41
                  IF(VTYPE.EQ.12) ALTYPE=37
                  IF(VTYPE.EQ.13) ALTYPE=38
                  IF(VTYPE.EQ.14) ALTYPE=39
                  IF(VTYPE.EQ.15) ALTYPE=40
                  IF(VTYPE.EQ.16) ALTYPE=118
                  IF(VTYPE.EQ.17) ALTYPE=119
                  IF(VTYPE.EQ.18) ALTYPE=120
                  IF(VTYPE.EQ.19) ALTYPE=114
                  IF(VTYPE.EQ.20) ALTYPE=115
                  IF(VTYPE.EQ.21) ALTYPE=46
                  IF(VTYPE.EQ.22) ALTYPE=47
                  IF(VTYPE.EQ.23) ALTYPE=48
                  IF(VTYPE.EQ.24) ALTYPE=49
                  IF(VTYPE.EQ.25) ALTYPE=50
                  IF(VTYPE.EQ.75) ALTYPE=43
                  IF(VTYPE.EQ.124) ALTYPE=71
                  IF(VTYPE.EQ.125) ALTYPE=72
                  IF(VTYPE.EQ.126) ALTYPE=73
                  IF(VTYPE.EQ.127) ALTYPE=74
                  IF(VTYPE.EQ.128) ALTYPE=75
                  IF(VTYPE.EQ.129) ALTYPE=81
                  IF(VTYPE.EQ.130) ALTYPE=82
                  IF(VTYPE.EQ.131) ALTYPE=83
                  IF(VTYPE.EQ.132) ALTYPE=84
                  IF(VTYPE.EQ.133) ALTYPE=85
                  IF(VTYPE.EQ.134) ALTYPE=116
                  IF(VTYPE.EQ.135) ALTYPE=86
                  IF(VTYPE.EQ.136) ALTYPE=87
                  IF(VTYPE.EQ.137) ALTYPE=78
                  IF(VTYPE.EQ.138) ALTYPE=79
                  IF(VTYPE.EQ.139) ALTYPE=80
                  IF(VTYPE.EQ.140) ALTYPE=89
                  IF(VTYPE.EQ.141) ALTYPE=11
                  IF(VTYPE.EQ.142) ALTYPE=10
                  IF(VTYPE.EQ.143) ALTYPE=90
                  IF(VTYPE.EQ.144) ALTYPE=91
                  IF(VTYPE.EQ.145) ALTYPE=92
                  IF(VTYPE.EQ.146) ALTYPE=93
                  IF(VTYPE.EQ.147) ALTYPE=94
                  IF(VTYPE.EQ.148) ALTYPE=95
                  IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                  V1=VARABL(I,4)
                  ALENS(ALTYPE,INT(VARABL(I,3)))=V1
              END IF
              IF(VTYPE.EQ.150) THEN
                  V1=VARABL(I,4)
                  GPREG(INT(VARABL(I,3)))=V1
              END IF
              IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                  V1=VARABL(I,4)
                  FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
              END IF
              IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                  V1=VARABL(I,4)
                  FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
              END IF
C
C     LOOP TO NEXT VARIABL
          END DO
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
          F6=1
          F1=0
          F22=0
          LNSTYP=3
          CALL LNSEOS
          RETURN
      END
      SUBROUTINE RAYS
          IMPLICIT NONE
          CHARACTER AVAL3*3
          INTEGER I
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          AVAL3(1:3)=WC(2:4)
          CALL ATOII(AVAL3,I)
          IF(STI.EQ.1) THEN
              OUTLYNE='"'//WC//'" SETS A RAY VALUE FOR OPTIMIZATION'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
              OUTLYNE='CURRENT SETTINGS FOR "'//WC//'" ARE:'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,111) RAYY(I)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,112) RAYX(I)
              CALL SHOWIT(1)
              WRITE(OUTLYNE,113) INT(RAYW(I))
              CALL SHOWIT(1)
 111          FORMAT(' Y-HT  = ',D23.15)
 112          FORMAT(' X-HT  = ',D23.15)
 113          FORMAT(' WAV#  = ',I2)
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              OUTLYNE='"'//WC//'" TAKES NO QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              OUTLYNE='"'//WC//'" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF3.EQ.1) W3=SYSTEM1(11)
          IF(DF3.EQ.1) DF3=0
          IF(W3.NE.1.0D0.AND.W3.NE.2.0D0.AND.W3.NE.3.0D0.AND.W3.NE.4.0D0
     1    .AND.W3.NE.5.0D0.AND.W3.NE.6.0D0.AND.W3.NE.7.0D0.AND.W3.NE.
     2    8.0D0.AND.W3.NE.9.0D0.AND.W3.NE.10.0D0) THEN
              OUTLYNE='WAVELENGTH # MUST BE 1,2,3,4,5,6,7,8,9 OR 10'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1) W1=0.0D0
          IF(DF2.EQ.1) W2=0.0D0
          RAYY(I)=W1
          RAYX(I)=W2
          RAYW(I)=W3
          RETURN
      END
      SUBROUTINE QRRYFL
          IMPLICIT NONE
C     QUERRIES FIELDS AND RAYS FOR OPTIMIZATION
          INTEGER I
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'RAYS') OUTLYNE=
     1        '"RAYS" LISTS OPTIMIZATION RAYS'
              IF(WC.EQ.'FIELDS') OUTLYNE=
     1        '"FIELDS" LISTS OPTIMIZATION FIELDS'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              IF(SN.EQ.0.OR.S1.EQ.0.OR.S2.EQ.0) THEN
                  IF(WC.EQ.'RAYS'.AND.SQ.EQ.0) OUTLYNE=
     1            '"RAYS" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                  IF(WC.EQ.'FIELDS'.AND.SQ.EQ.0) OUTLYNE=
     1            '"FIELDS" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SST.EQ.1) THEN
              IF(WC.EQ.'RAYS') OUTLYNE=
     1        '"RAYS" TAKES NO STRING INPUT'
              IF(WC.EQ.'FIELDS') OUTLYNE=
     1        '"FIELDS" TAKES NO STRING INPUT'
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.NE.'RESET') THEN
              IF(WC.EQ.'RAYS') OUTLYNE=
     1        '"RAYS" ONLY TAKES "RESET" AS A QUALIFIER WORD'
              IF(WC.EQ.'FIELDS') OUTLYNE=
     1        '"FIELDS" ONLY TAKES "RESET" AS A QUALIFIER WORD'
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              IF(WC.EQ.'RAYS') OUTLYNE=
     1        '"RAYS" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
              IF(WC.EQ.'FIELDS') OUTLYNE=
     1        '"FIELDS" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WC.EQ.'RAYS') THEN
              IF(WQ.EQ.'RESET') THEN
                  CALL RFRESET
                  RETURN
              END IF
              IF(W1.LT.1.0D0.OR.W2.LT.0.0D0.OR.W1.GT.5000.0D0.OR.
     1        W2.GT.5000.0D0.OR.W1.GE.W2) THEN
                  OUTLYNE='INVALID NUMERIC INPUT USED WITH "RAYS"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'FIELDS') THEN
              IF(WQ.EQ.'RESET') THEN
                  CALL RFRESET
                  RETURN
              END IF
              IF(W1.LT.1.0D0.OR.W2.LT.0.0D0.OR.W1.GT.200.0D0.OR.
     1        W2.GT.200.0D0.OR.W1.GT.W2) THEN
                  OUTLYNE='INVALID NUMERIC INPUT USED WITH "FIELDS"'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
 200      FORMAT('FIELD#',I3,1X,D13.6,1X,D13.6,1X,D13.6,1X,I2)
 210      FORMAT('RAY#',I4,1X,D13.6,1X,D13.6,1X,I2)
          IF(WC.EQ.'RAYS') THEN
              WRITE(OUTLYNE,100)
              CALL SHOWIT(0)
 100          FORMAT(10X,'Y-REF HT     ',1X,'X-REF HT     ',1X,
     1        'WAV#')
              DO I=INT(W1),INT(W2)
                  WRITE(OUTLYNE,210) I,RAYY(I),RAYX(I),INT(RAYW(I))
                  CALL SHOWIT(0)
              END DO
          END IF
          IF(WC.EQ.'FIELDS') THEN
              WRITE(OUTLYNE,110)
              CALL SHOWIT(0)
 110          FORMAT(9X,'  Y-FOB      ',1X,'  X-FOB      ',1X,
     1        '  Z-POS      ',1X,'WAV#')
              DO I=INT(W1),INT(W2)
                  WRITE(OUTLYNE,200) I,FIELDY(I),FIELDX(I),FIELDZ(I),
     1            INT(FIELDW(I))
                  CALL SHOWIT(0)
              END DO
          END IF
          RETURN
      END
      SUBROUTINE RFRESET
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(WC.EQ.'FIELDS') THEN
              OPEN(UNIT=16,ACCESS='SEQUENTIAL',
     1        BLANK='NULL',FORM='FORMATTED',FILE=trim(HOME)//'FIELDS.DAT',
     1        STATUS='UNKNOWN')
              CALL CLOSE_FILE(16,0)
              CALL NEWFIELD
              RETURN
          END IF
          IF(WC.EQ.'RAYS') THEN
              OPEN(UNIT=16,ACCESS='SEQUENTIAL',
     1        BLANK='NULL',FORM='FORMATTED',FILE=trim(HOME)//'RAYS.DAT',
     1        STATUS='UNKNOWN')
              CALL CLOSE_FILE(16,0)
              CALL NEWRAY
              RETURN
          END IF
      END
C SUB MACDMP.FOR
      SUBROUTINE MACDMP
C
          IMPLICIT NONE
C
C     THIS DOES THE MACDMP COMMAND AT THE CMD LEVEL
C     SYNTAX IS MACDMP (MACNAME), 0(DEFAULT) OR 1 OR 2
C     0 SAVES VARIABLES AND MERIT
C     1 SAVES VARIABLES ONLY
C     2 SAVES MERIT ONLY
C
          CHARACTER OPNNMM*5,AI1*1,MNAME*8,STAMP*20,BWORD*8
     1    ,AI2*2,AI3*3,BF1*1,BF2*2,BF3*3
C
          INTEGER OLDMODOP,I,J,M1,M2,M3,NEXTM3,OLDCFGOP,OLDCFGVB
C
          LOGICAL EXISJK
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsub.inc'
C
C
C       THIS IS SUBROUTINE MACDMP. THIS IS THE SUBROUTINE WHICH
C       SAVES VARIABLES AND MERIT DEFINITIONS IN A NAMED MACRO
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'"MACDMP" SAVES VARIABLES AND MERIT DEFINITIONS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO THE MACRO NAMED BY THE QUALIFIER WORD.'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'IF NO NUMERIC WORD #1 INPUT IS PRESENT, THEN ALL OPTIMIZATION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'DEFINITIONS WILL BE SAVED.'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'IF NUMERIC WORD 1 = 1, ONLY VARIABLES ARE SAVED.'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'IF NUMERIC WORD 1 = 2, ONLY MERIT DEFINITIONS ARE SAVED.'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"MACDMP" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"MACDMP" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.0.AND.W1.NE.1.0D0.AND.W1.NE.2.0D0) THEN
              WRITE(OUTLYNE,*)'FOR THE "MACDMP" COMMAND, EXPLICIT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NUMERIC WORD #1 INPUT MUST BE "1" OR "2"'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     DOES THE INDICATED MACRO EXIST ALREADY, IF SO STOP AND
C     ISSUE AN ERROR MESSAGE.
C
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"MACRO FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C       OPEN UNIT 20 FOR I/O
C
C       ***************************************************************
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
          J=0
          M3=0
          DO 1010 I=1,MAXMAC
              READ(UNIT=20,REC=I,ERR=1010) MNAME,M1,M2,NEXTM3,STAMP
              M3=M3+NEXTM3
              IF(WQ.EQ.MNAME) J=J+1
 1010     CONTINUE
C               CLOSE UNIT 20
          CALL CLOSE_FILE(20,1)
          IF(J.GT.0) THEN
C       THERE ALREADY WAS A MACRO OF THE SAME NAME ON FILE
              WRITE(OUTLYNE,*)
     1        'MACRO ',WQ,' ALREADY EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'DELETE MACRO ',WQ,' OR PICK ANOTHER NAME THEN'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK FOR NO ROOM IN THE DIRECTORY
          IF(M3.GE.MAXMAC) THEN
C       NO MORE ROOM
              WRITE(OUTLYNE,*)'MACRO DIRECTORY FULL'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'DELETE A MACRO TO MAKE SOME ROOM'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
          END IF
C
C     DELETE CARDTEXT.DAT IF IT EXISTS
          OPEN(UNIT=8,FILE=trim(HOME)//'CARDTEXT.DAT',STATUS='UNKNOWN')
          CALL CLOSE_FILE(8,0)
C
C     MACRO NAME OK, PROCEED
C
          IF(DF1.EQ.1) THEN
C     DOING BOTH VARIABLES AND MERIT DEFINITIONS
              IF(VBCNT.EQ.0
     1        .AND.OPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO VARIABLES OR MERIT DATA EXISTS TO STORE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO DATA WAS WRITTEN TO THE MACRO'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(VBCNT.EQ.0
     1        .AND.OPCNT.NE.0) THEN
                  WRITE(OUTLYNE,*)'NO VARIABLES DATA EXISTS TO STORE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'ONLY MERIT DATA WILL BE WRITTEN TO THE MACRO'
                  CALL SHOWIT(1)
              END IF
              IF(VBCNT.NE.0
     1        .AND.OPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO MERIT DATA EXISTS TO STORE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'ONLY VARIABLES DATA WILL BE WRITTEN TO THE MACRO'
                  CALL SHOWIT(1)
              END IF
C
C     HERE IS WHERE DATA IS STORED WHEN DF1=1
              OPEN(UNIT=8,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2        ,STATUS='UNKNOWN')
              WRITE(8,*)'MACRO    ',WQ
              IF(VBCNT.NE.0) THEN
C     DO VARAIBLES OUTPUT HERE
                  OLDCFGVB=1
                  WRITE(8,*)'VARIABLE'
                  DO I=1,VBCNT
C     FIRST THE "CFG" COMMAND AS NEEDED
                      IF(INT(VARABL(I,2)).NE.OLDCFGVB) THEN
                          OLDCFGVB=INT(VARABL(I,2))
                          WRITE(8,10) OLDCFGVB
 10                       FORMAT('CFG,',I2)
                      END IF
C
C     NOW VARIABLES
C
                      WRITE(8,11) VARNAM(I),VARABL(I,3),VARABL(I,7),VARABL(I,8)
     1                ,VARABL(I,9),VARABL(I,10)
 11                   FORMAT(A8,',',D15.7,',',D15.7,',',D15.7,',',D15.7,',',D15.7)
                  END DO
                  WRITE(8,*)'EOS'
                  OLDCFGVB=1
              END IF
              IF(OPCNT.NE.0) THEN
C     DO MERIT OUTPUT HERE
C     OUTPUT CMD LEVEL SETUP COMMANDS FIRST
                  IF(OPSPDTYPE.EQ.1) WRITE(8,21)
                  IF(OPSPDTYPE.EQ.2) WRITE(8,22)
                  IF(OPSPDTYPE.EQ.3) WRITE(8,23)
 21               FORMAT('OPSPOT RECT')
 22               FORMAT('OPSPOT RING')
 23               FORMAT('OPSPOT RAND')
                  IF(OPSPDTYPE.EQ.1) THEN
                      WRITE(8,30) OPNRECT
 30                   FORMAT('OPRECT  ,',I8)
                  END IF
                  IF(OPSPDTYPE.EQ.2) THEN
                      WRITE(8,24) OPRINGTOT
 24                   FORMAT('OPRINGS ,',I5)
                      DO I=1,int(OPRINGTOT)
                          WRITE(8,25) I,OPRINGRAD(I),OPRINGPNT(I),OPRINGANG(I)
 25                       FORMAT('OPRING  ,',I3,',',D23.15,',',I3,',',D23.15)
                      END DO
                  END IF
                  IF(OPSPDTYPE.EQ.3) THEN
                      WRITE(8,41) OPRNUMBR
 41                   FORMAT('OPRANNUM,',D23.15)
                  END IF
                  WRITE(8,52) OPNRD
 52               FORMAT('OPNRD   ,',I8)
                  OLDCFGOP=1
                  OLDMODOP=1
                  WRITE(8,*)'MERIT'
                  DO I=1,OPCNT
C     FIRST THE "CFG" COMMAND AS NEEDED
                      IF(INT(OPERND(I,16)).NE.OLDCFGOP) THEN
                          OLDCFGOP=INT(OPERND(I,16))
                          WRITE(8,1000) OLDCFGOP
 1000                     FORMAT('CFG,',I2)
                      END IF
C     NEXT THE MODE COMMAND AS NEEDED
                      IF(INT(OPERND(I,13)).NE.OLDMODOP) THEN
                          OLDMODOP=INT(OPERND(I,13))
                          IF(OLDMODOP.EQ.1) WRITE(8,1001)
 1001                     FORMAT('COR')
                          IF(OLDMODOP.EQ.0) WRITE(8,1002)
 1002                     FORMAT('BYP')
                          IF(OLDMODOP.EQ.-2) WRITE(8,1003)
 1003                     FORMAT('GTE')
                          IF(OLDMODOP.EQ.2) WRITE(8,1004)
 1004                     FORMAT('LTE')
                          IF(OLDMODOP.EQ.10) WRITE(8,1005)
 1005                     FORMAT('HLD')
                      END IF
C     NOW OPERANDS
C     THE OPERAND AND FUNCTION NAME AND NW1 AND NW2
                      IF(INT(OPERND(I,1)).EQ.0) BWORD='FUNC00'
                      IF(INT(OPERND(I,1)).EQ.1) BWORD='FUNC01'
                      IF(INT(OPERND(I,1)).EQ.2) BWORD='FUNC02'
                      IF(INT(OPERND(I,1)).EQ.3) BWORD='FUNC03'
                      IF(INT(OPERND(I,1)).EQ.4) BWORD='FUNC04'
                      IF(INT(OPERND(I,1)).EQ.5) BWORD='FUNC05'
                      IF(INT(OPERND(I,1)).EQ.6) BWORD='FUNC06'
                      IF(INT(OPERND(I,1)).EQ.7) BWORD='FUNC07'
                      IF(INT(OPERND(I,1)).EQ.8) BWORD='FUNC08'
                      IF(INT(OPERND(I,1)).EQ.9) BWORD='FUNC09'
                      IF(INT(OPERND(I,1)).EQ.10) BWORD='FUNC10'
                      IF(BWORD.EQ.'FUNC00') THEN
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,101) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,8)
     1                        ,OPERND(I,9),OPERND(I,10)
                          END IF
 101                      FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1                    ,D23.15)
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,102) OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,9),OPERND(I,10)
                          END IF
 102                      FORMAT(A8,',',D23.15,',',D23.15,',,',D23.15,',',D23.15)
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,103) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,10)
                          END IF
 103                      FORMAT(A8,',',D23.15,',',D23.15,',,,',D23.15)
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,104) OPNAM(I),OPERND(I,2),OPERND(I,7)
                          END IF
 104                      FORMAT(A8,',',D23.15,',',D23.15,',,,,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,105) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,8)
     1                        ,OPERND(I,10)
                          END IF
 105                      FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',,',D23.15)
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,106) OPNAM(I),OPERND(I,2),OPERND(I,7)
     1                        ,OPERND(I,8),OPERND(I,9)
                          END IF
 106                      FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,107) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,8)
                          END IF
 107                      FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',,,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,108) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,9)
                          END IF
 108                      FORMAT(A8,',',D23.15,',',D23.15,',,',D23.15,',,')
                      ELSE
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1101) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,8),OPERND(I,9),OPERND(I,10)
                          END IF
 1101                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,','
     1                    ,D23.15,',',D23.15)
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1102) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,9),OPERND(I,10)
                          END IF
 1102                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,',D23.15,',',D23.15
     1                    )
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1103) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,10)
                          END IF
 1103                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,,',D23.15)
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1104) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7)
                          END IF
 1104                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,,,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1105) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,8),OPERND(I,10)
                          END IF
 1105                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,',,',D23.15)
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1106) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,8),OPERND(I,9)
                          END IF
 1106                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1107) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,8)
                          END IF
 1107                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,',,,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1108) BWORD,OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,9)
                          END IF
 1108                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,',D23.15,',,')
                      END IF
                      IF(OPERDESC(I)(1:8).NE.'        ') THEN
                          IF(I.LT.10)    WRITE(BF1,100) I
                          IF(I.GE.10.AND.I.LT.100)   WRITE(BF2,200) I
                          IF(I.GE.100.AND.I.LT.10000) WRITE(BF3,300) I
                          IF(I.LT.10)    READ(BF1,110) AI1
                          IF(I.GE.10.AND.I.LT.100)   READ(BF2,210) AI2
                          IF(I.GE.100.AND.I.LT.10000) READ(BF3,310) AI3
 100                      FORMAT(I1)
 200                      FORMAT(I2)
 300                      FORMAT(I3)
 110                      FORMAT(A1)
 210                      FORMAT(A2)
 310                      FORMAT(A3)
                          IF(I.LT.10)   OPNNMM='OP'//AI1//'  '
                          IF(I.GE.10.AND.I.LT.100)  OPNNMM='OP'//AI2//' '
                          IF(I.GE.100.AND.I.LT.1000) OPNNMM='OP'//AI3
                          WRITE(8,1701) OPNNMM,OPERDESC(I)(1:69)
                      END IF
 1701                 FORMAT('OP_DESC',1x,A5,1X,A69)
C
                  END DO
                  WRITE(8,*)'EOS'
              END IF
              WRITE(8,*)'EOM'
              CALL CLOSE_FILE(8,1)
C     NOW READ IN THE MACRO FROM CARDTEXT.DAT
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='INPUT CR'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              RETURN
C
          END IF
C
          IF(W1.EQ.1) THEN
C     DOING VARIABLES DEFINITIONS ONLY
              IF(VBCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO VARIABLES DATA EXISTS TO STORE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO DATA WAS WRITTEN TO THE MACRO'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       STORE DATA HERE FOR W1=1
              OPEN(UNIT=8,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2        ,STATUS='UNKNOWN')
              WRITE(8,*)'MACRO    ',WQ
              IF(VBCNT.NE.0) THEN
C     DO VARAIBLES OUTPUT HERE
                  OLDCFGVB=1
                  WRITE(8,*)'VARIABLE'
                  DO I=1,VBCNT
C     FIRST THE "CFG" COMMAND AS NEEDED
                      IF(INT(VARABL(I,2)).NE.OLDCFGVB) THEN
                          OLDCFGVB=INT(VARABL(I,2))
                          WRITE(8,10) OLDCFGVB
                      END IF
C
C     NOW VARIABLES
C
                      WRITE(8,11) VARNAM(I),VARABL(I,3),VARABL(I,7),VARABL(I,8)
     1                ,VARABL(I,9),VARABL(I,10)
C
                  END DO
                  WRITE(8,*)'EOS'
                  WRITE(8,*)'EOM'
                  CALL CLOSE_FILE(8,1)
C     NOW READ IN THE MACRO FROM CARDTEXT.DAT
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='INPUT CR'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  RETURN
C
              END IF
          END IF
          IF(W1.EQ.2) THEN
C     DOING MERIT DEFINITIONS ONLY
              IF(OPCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NO MERIT DATA EXISTS TO STORE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO DATA WAS WRITTEN TO THE MACRO'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C     STORE DATA HERE FOR W1=2
              OPEN(UNIT=8,ACCESS='SEQUENTIAL',BLANK='NULL'
     1        ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2        ,STATUS='UNKNOWN')
              WRITE(8,*)'MACRO    ',WQ
              IF(OPCNT.NE.0) THEN
C     DO MERIT OUTPUT HERE
C     OUTPUT CMD LEVEL SETUP COMMANDS FIRST
                  IF(OPSPDTYPE.EQ.1) WRITE(8,21)
                  IF(OPSPDTYPE.EQ.2) WRITE(8,22)
                  IF(OPSPDTYPE.EQ.3) WRITE(8,23)
                  IF(OPSPDTYPE.EQ.1) THEN
                      WRITE(8,30) OPNRECT
                  END IF
                  IF(OPSPDTYPE.EQ.2) THEN
                      WRITE(8,24) OPRINGTOT
                      DO I=1,int(OPRINGTOT)
                          WRITE(8,25) I,OPRINGRAD(I),OPRINGPNT(I),OPRINGANG(I)
                      END DO
                  END IF
                  IF(OPSPDTYPE.EQ.3) THEN
                      WRITE(8,41) OPRNUMBR
                  END IF
                  WRITE(8,52) OPNRD
                  OLDCFGOP=1
                  WRITE(8,*)'MERIT'
                  DO I=1,OPCNT
C     FIRST THE "CFG" COMMAND AS NEEDED
                      IF(INT(OPERND(I,16)).NE.OLDCFGOP) THEN
                          OLDCFGOP=INT(OPERND(I,16))
                          WRITE(8,1000) OLDCFGOP
                      END IF
C     NEXT THE MODE COMMAND AS NEEDED
                      IF(INT(OPERND(I,13)).NE.OLDMODOP) THEN
                          OLDMODOP=INT(OPERND(I,13))
                          IF(OLDMODOP.EQ.1) WRITE(8,1001)
                          IF(OLDMODOP.EQ.0) WRITE(8,1002)
                          IF(OLDMODOP.EQ.-2) WRITE(8,1003)
                          IF(OLDMODOP.EQ.2) WRITE(8,1004)
                          IF(OLDMODOP.EQ.10) WRITE(8,1005)
                      END IF
C     NOW OPERANDS
C     THE OPERAND AND FUNCTION NAME AND NW1 AND NW2
                      IF(INT(OPERND(I,1)).EQ.0) BWORD='FUNC00'
                      IF(INT(OPERND(I,1)).EQ.1) BWORD='FUNC01'
                      IF(INT(OPERND(I,1)).EQ.2) BWORD='FUNC02'
                      IF(INT(OPERND(I,1)).EQ.3) BWORD='FUNC03'
                      IF(INT(OPERND(I,1)).EQ.4) BWORD='FUNC04'
                      IF(INT(OPERND(I,1)).EQ.5) BWORD='FUNC05'
                      IF(INT(OPERND(I,1)).EQ.6) BWORD='FUNC06'
                      IF(INT(OPERND(I,1)).EQ.7) BWORD='FUNC07'
                      IF(INT(OPERND(I,1)).EQ.8) BWORD='FUNC08'
                      IF(INT(OPERND(I,1)).EQ.9) BWORD='FUNC09'
                      IF(INT(OPERND(I,1)).EQ.10) BWORD='FUNC10'
                      IF(BWORD.EQ.'FUNC00') THEN
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,101) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,8)
     1                        ,OPERND(I,9),OPERND(I,10)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,102) OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,9),OPERND(I,10)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,103) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,10)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,104) OPNAM(I),OPERND(I,2),OPERND(I,7)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,105) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,8)
     1                        ,OPERND(I,10)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,106) OPNAM(I),OPERND(I,2),OPERND(I,7)
     1                        ,OPERND(I,8),OPERND(I,9)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,107) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,8)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,108) OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,9)
                          END IF
                      ELSE
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1101) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,8),OPERND(I,9),OPERND(I,10)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1102) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,9),OPERND(I,10)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1103) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,10)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1104) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1105) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,8),OPERND(I,10)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1106) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,8),OPERND(I,9)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1107) BWORD,OPNAM(I),OPERND(I,2),OPERND(I,7),OPERND(I,8)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1108) BWORD,OPERND(I,2),OPERND(I,7),
     1                        OPERND(I,9)
                          END IF
                      END IF
                      IF(OPERDESC(I)(1:8).NE.'        ') THEN
                          IF(I.LT.10)    WRITE(BF1,100) I
                          IF(I.GE.10.AND.I.LT.100)   WRITE(BF2,200) I
                          IF(I.GE.100.AND.I.LT.10000) WRITE(BF3,300) I
                          IF(I.LT.10)    READ(BF1,110) AI1
                          IF(I.GE.10.AND.I.LT.100)   READ(BF2,210) AI2
                          IF(I.GE.100.AND.I.LT.10000) READ(BF3,310) AI3
                          IF(I.LT.10)   OPNNMM='OP'//AI1//'  '
                          IF(I.GE.10.AND.I.LT.100)  OPNNMM='OP'//AI2//' '
                          IF(I.GE.100.AND.I.LT.1000) OPNNMM='OP'//AI3
                          WRITE(8,1701) OPNNMM,OPERDESC(I)(1:69)
                      END IF
C
                  END DO
                  WRITE(8,*)'EOS'
              END IF
              WRITE(8,*)'EOM'
              CALL CLOSE_FILE(8,1)
C     NOW READ IN THE MACRO FROM CARDTEXT.DAT
              SAVE_KDP(1)=SAVEINPT(1)
              INPUT='INPUT CR'
              CALL PROCES
              REST_KDP(1)=RESTINPT(1)
              RETURN
          END IF
C     ALL DONE
          RETURN
      END

C SUB AUTO.FOR
      SUBROUTINE AUTO
C
          IMPLICIT NONE
C
          LOGICAL EXIS23
          LOGICAL ITERROR
C
          INTEGER I,J
C
          CHARACTER WCOLD*8
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'

C
C       THIS IS SUBROUTINE AUTO. THIS IS THE SUBROUTINE WHICH
C       SAVES AND RELOADS THE VARIABLES AND
C       TVARIABLES SUBFILES INTO FILES NAMED "AUTO.DAT"  AND "AUTO2.DAT"
C
          IF(WQ.NE.'SAVE'.AND.WQ.NE.'SAVE2'.AND.WQ.NE.'RELOAD'.AND.
     1    WQ.NE.'RELOAD2') THEN
              WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)'"AUTO" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(WQ.EQ.'SAVE'.OR.WQ.EQ.'SAVE2') THEN
C       THE CURRENT OPTIMIZATION/TOLERANCE FILES ARE STORED
C       IN DIRECT, BINARY UNFORMATTED FILE "AUTO.DAT"
C       IF 'SAVE' WAS THE QUALIFIER AND
C       "AUTO2.DAT" IF WQ IS "SAVE2"
C       RELOAD AND RELOAD2 ARE THE COMPLIMENTS OF SAVE AND SAVE2.
C
C       RECORD 1 = # OF VARIABLES IN THE VARIABLES SUBFILE
C
              IF(VBCNT.EQ.0
     1        .AND.OPCNT.EQ.0) THEN
                  IF(WQ.EQ.'SAVE') THEN
                      WRITE(OUTLYNE,*)'NO "AUTO" DATA EXISTS TO STORE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO DATA WAS WRITTEN TO THE "AUTO.DAT" FILE '
                      CALL SHOWIT(1)
                  END IF
                  IF(WQ.EQ.'SAVE2') THEN
                      WRITE(OUTLYNE,*)'NO "AUTO" DATA EXISTS TO STORE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO DATA WAS WRITTEN TO THE "AUTO2.DAT" FILE '
                      CALL SHOWIT(1)
                  END IF
                  RETURN
              END IF

              IF(WQ.EQ.'SAVE') THEN
                  OPEN(UNIT=23,ACCESS='DIRECT',FILE=LIBAUT//'AUTO.DAT',
     1            FORM='UNFORMATTED',RECL=(100*NRECL),STATUS='UNKNOWN')
              END IF
              IF(WQ.EQ.'SAVE2') THEN
                  OPEN(UNIT=23,ACCESS='DIRECT',FILE=LIBAUT//'AUTO2.DAT',
     1            FORM='UNFORMATTED',RECL=(100*NRECL),STATUS='UNKNOWN')
              END IF
C
              WRITE(UNIT=23,REC=1) OPCNT
              WRITE(UNIT=23,REC=2) VBCNT
              J=2
              IF(OPCNT.NE.0) THEN
C       NOW START AT RECORD J+1 WITH MERIT DATA
C
                  DO I=1,OPCNT
                      J=J+I
                      WRITE(UNIT=23,REC=J)
     1                OPNAM(I),
     1                OPERND(I,1),OPERND(I,2),OPERND(I,3),OPERND(I,4),OPERND(I,5),
     1                OPERND(I,6),OPERND(I,7),OPERND(I,8),OPERND(I,9),OPERND(I,10),
     1                OPERND(I,11),OPERND(I,12),OPERND(I,13),OPERND(I,14),
     1                OPERND(I,15),OPERND(I,16),OPERND(I,17),OPERND(I,18),
     1                OPERND(I,19),OPERND(I,20)
                      J=J+1
                      WRITE(UNIT=23,REC=J) OPERDESC(I)(1:80)
                  END DO
              END IF
C
              IF(VBCNT.NE.0) THEN
C       NOW START AT RECORD J+1 WITH VARIABLE DATA
C
                  DO I=1,VBCNT
                      J=J+I
                      WRITE(UNIT=23,REC=J) VARNAM(I),
     1                VARABL(I,1),VARABL(I,2),VARABL(I,3),VARABL(I,4),VARABL(I,5),
     1                VARABL(I,6),VARABL(I,7),VARABL(I,8),VARABL(I,9),VARABL(I,10),
     1                VARABL(I,11),VARABL(I,12),VARABL(I,13),VARABL(I,14)
                  END DO
                  CALL CLOSE_FILE(23,1)
C       DUMPING OF OTHER THAN RAYSET, MERIT AND VARIABLE
C       DATA IS NOT YET OPERATIONAL
              END IF
C
              IF(WQ.EQ.'SAVE') THEN
                  WRITE(OUTLYNE,*)
     1            'CURRENT "AUTO" DATA HAS BEEN SAVED TO "AUTO.DAT"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(WQ.EQ.'SAVE2') THEN
                  WRITE(OUTLYNE,*)
     1            'CURRENT "AUTO2" DATA HAS BEEN SAVED TO "AUTO2.DAT"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
C
          IF(WQ.EQ.'RELOAD'.OR.WQ.EQ.'RELOAD2') THEN
C
              IF(WQ.EQ.'RELOAD') THEN
                  INQUIRE(FILE=LIBAUT//'AUTO.DAT',EXIST=EXIS23)
                  IF(.NOT.EXIS23) THEN
                      WRITE(OUTLYNE,*)'NO "AUTO.DAT" FILE EXISTS TO BE READ'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO "AUTO" DATA WAS RELOADED'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
C       INITIALIZE OPERATING CONDITIONS
                  WCOLD=WC
                  WC='OPCON'
                  CALL PM
                  WC=WCOLD
C***********************************************************************
                  FMTFMT=0.0D0
                  FMTFLG=.FALSE.
                  CFCH=.FALSE.
                  VBCNT=0
                  OPCNT=0
                  DERSIZ=0
C       THE CURRENT OPTIMIZATION/TOLERANCE FILES ARE READ
C       IN DIRECT, BINARY UNFORMATTED FORM FROM FILE "AUTO.DAT"
C       OPENED AS UNIT NUMBER 23
C
C       RECORD 1 = # OF OPERANDS IN THE MERIT SUBFIL (OPCNT)
C       RECORD 2 = # OF VARIABLES IN THE VARIABLES SUBFILE
C
                  OPEN(UNIT=23,ACCESS='DIRECT',FILE=LIBAUT//'AUTO.DAT',
     1            FORM='UNFORMATTED',RECL=(100*NRECL),STATUS='UNKNOWN')
              END IF
C
              IF(WQ.EQ.'RELOAD2') THEN
                  INQUIRE(FILE=LIBAUT//'AUTO2.DAT',EXIST=EXIS23)
                  IF(.NOT.EXIS23) THEN
                      WRITE(OUTLYNE,*)'NO "AUTO2.DAT" FILE EXISTS TO BE READ'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO "AUTO" DATA WAS RELOADED'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
C       THE CURRENT OPTIMIZATION/TOLERANCE FILES ARE READ
C       IN DIRECT, BINARY UNFORMATTED FORM FROM FILE "AUTO2.DAT"
C       OPENED AS UNIT NUMBER 23
C
                  OPEN(UNIT=23,ACCESS='DIRECT',FILE=LIBAUT//'AUTO2.DAT',
     1            FORM='UNFORMATTED',RECL=(100*NRECL),STATUS='UNKNOWN')
              END IF
C
              I=MAXOPT
              OPERDESC(1:I)(1:80)=CHAR(32)
C
C       SET THE COUNTER TO THE TOP OF THE MERIT ARRAY STRUCTURE.
C       (OPCNT)
              OPCNT=0
              FMTEXT=.FALSE.
              CORMOD=1
C       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
              CURFIG=1

              DEREXT=.FALSE.
              ITERROR=.FALSE.
              CALL ITER(0,0,ITERROR)
              SOLEXT=.FALSE.
              FMTFLG=.FALSE.
              VBCNT=0
              OPCNT=0
              DERSIZ=0
              READ(UNIT=23,REC=1) OPCNT
              READ(UNIT=23,REC=2) VBCNT
              J=2
C
              IF(OPCNT.NE.0) THEN
C       NOW START AT RECORD J+1 WITH MERIT DATA
C
                  DO I=1,OPCNT
                      J=J+I
                      READ(UNIT=23,REC=J)
     1                OPNAM(I),
     1                OPERND(I,1),OPERND(I,2),OPERND(I,3),OPERND(I,4),OPERND(I,5),
     1                OPERND(I,6),OPERND(I,7),OPERND(I,8),OPERND(I,9),OPERND(I,10),
     1                OPERND(I,11),OPERND(I,12),OPERND(I,13),OPERND(I,14),
     1                OPERND(I,15),OPERND(I,16),OPERND(I,17),OPERND(I,18),
     1                OPERND(I,19),OPERND(I,20)
                      J=J+1
                      READ(UNIT=23,REC=J) OPERDESC(I)(1:80)
                  END DO
              END IF
              IF(VBCNT.NE.0) THEN
C       READ IN THE VARIABLE DATA
                  DO I=1,VBCNT
                      J=J+I
                      READ(UNIT=23,REC=J) VARNAM(I),
     1                VARABL(I,1),VARABL(I,2),VARABL(I,3),VARABL(I,4),VARABL(I,5),
     1                VARABL(I,6),VARABL(I,7),VARABL(I,8),VARABL(I,9),VARABL(I,10),
     1                VARABL(I,11),VARABL(I,12),VARABL(I,13),VARABL(I,14)
                  END DO
              END IF
C     FORCE THE VARIABLES TO AGREE WITH THE LENS
              IF(WQ.EQ.'RELOAD') THEN
                  WRITE(OUTLYNE,*)'THE LAST "AUTO" DATA SAVED HAS BEEN RELOADED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'FROM "AUTO.DAT"'
                  CALL SHOWIT(1)
              END IF
              IF(WQ.EQ.'RELOAD2') THEN
                  WRITE(OUTLYNE,*)'THE LAST "AUTO" DATA SAVED HAS BEEN RELOADED'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'FROM "AUTO2.DAT"'
                  CALL SHOWIT(1)
              END IF
          END IF
C       ALL LOADED, CLOSE FILE
          F27=0
          F29=0
          CALL CLOSE_FILE(23,1)
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='U L'
          CALL PROCES
          INPUT='EOS'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)

          RETURN
      END
C SUB TOLDMP.FOR
      SUBROUTINE TOLDMP
C
          IMPLICIT NONE
C
C     THIS DOES THE TOLDMP COMMAND AT THE CMD LEVEL
C     SYNTAX IS TOLDMP (MACNAME)
C     SAVES THE TOLERANCE ANALYSIS TO A MACRO NAMED (MACNAME)
C
C
          LOGICAL YES
C
          CHARACTER BF1*1,AI1*1,OPNNMM*3,MNAME*8,STAMP*20,BWORD*8
C
          INTEGER II,I,J,M1,M2,M3,NEXTM3
C
          LOGICAL EXISJK
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
C
C       THIS IS SUBROUTINE TOLDMP. THIS IS THE SUBROUTINE WHICH
C       SAVES TOLERANCE DEFINITIONS IN A NAMED MACRO
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'"TOLDMP" SAVES TOLERANCE DEFINITIONS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO THE MACRO NAMED BY THE QUALIFIER WORD.'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"TOLDMP" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              WRITE(OUTLYNE,*)'"TOLDMP" TAKES NO STRING OR NUMERIC INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
C     DOES THE INDICATED MACRO EXIST ALREADY, IF SO STOP AND
C     ISSUE AN ERROR MESSAGE.
C
          EXISJK=.FALSE.
          INQUIRE(FILE=trim(LIBMAC)//'MAC.DAT',EXIST=EXISJK)
          IF(.NOT.EXISJK) THEN
              WRITE(OUTLYNE,*)'"MACRO FILES DO NOT YET EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "IMF" AND "PROCEED"'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C       OPEN UNIT 20 FOR I/O
C
C       ***************************************************************
          OPEN(UNIT=20,ACCESS='DIRECT',FILE=trim(LIBMAC)//'MAC.DAT',FORM=
     1    'UNFORMATTED',RECL=(40*NRECL),STATUS='UNKNOWN')
          J=0
          M3=0
          DO 1010 I=1,MAXMAC
              READ(UNIT=20,REC=I,err=1010) MNAME,M1,M2,NEXTM3,STAMP
              M3=M3+NEXTM3
              IF(WQ.EQ.MNAME) J=J+1
 1010     CONTINUE
C               CLOSE UNIT 20
          CALL CLOSE_FILE(20,1)
          IF(J.GT.0) THEN
C       THERE ALREADY WAS A MACRO OF THE SAME NAME ON FILE
              WRITE(OUTLYNE,*)
     1        'MACRO ',WQ,' ALREADY EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'DELETE MACRO ',WQ,' OR PICK ANOTHER NAME THEN'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK FOR NO ROOM IN THE DIRECTORY
          IF(M3.GE.MAXMAC) THEN
C       NO MORE ROOM
              WRITE(OUTLYNE,*)'MACRO DIRECTORY FULL'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'DELETE A MACRO TO MAKE SOME ROOM'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
          END IF
C
C     DELETE CARDTEXT.DAT IF IT EXISTS
          OPEN(UNIT=8
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2    ,STATUS='UNKNOWN')
          CALL CLOSE_FILE(8,0)
C     NOW OPEN IT FOR OUTPUT
          OPEN(UNIT=8,ACCESS='SEQUENTIAL',BLANK='NULL'
     1    ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2    ,STATUS='UNKNOWN')
C
C     MACRO NAME OK, PROCEED
C     HERE IS WHERE DATA IS STORED
          WRITE(8,*)'MACRO    ',WQ
C
C     COMPENSATOR VARIABLES
          YES=.FALSE.
          DO I=1,MAXCMP
              IF(ISTOP(I)) YES=.TRUE.
              IF(ISCOMP(I)) YES=.TRUE.
              IF(ISCRIT(I)) YES=.TRUE.
          END DO
          IF(YES) THEN
C
C
C     DOING TOLERANCE DEFINITIONS
              IF(OPSPDTYPE.EQ.1) WRITE(8,21)
              IF(OPSPDTYPE.EQ.2) WRITE(8,22)
              IF(OPSPDTYPE.EQ.3) WRITE(8,23)
 21           FORMAT('OPSPOT RECT')
 22           FORMAT('OPSPOT RING')
 23           FORMAT('OPSPOT RAND')
              IF(OPSPDTYPE.EQ.1) THEN
                  WRITE(8,30) OPNRECT
 30               FORMAT('OPRECT  ,',I8)
              END IF
              IF(OPSPDTYPE.EQ.2) THEN
                  WRITE(8,24) OPRINGTOT
 24               FORMAT('OPRINGS ,',I5)
                  DO I=1,INT(OPRINGTOT)
                      WRITE(8,25) I,OPRINGRAD(I),OPRINGPNT(I),OPRINGANG(I)
 25                   FORMAT('OPRING  ,',I3,',',D23.15,',',I3,',',D23.15)
                  END DO
              END IF
              IF(OPSPDTYPE.EQ.3) THEN
                  WRITE(8,41) OPRNUMBR
 41               FORMAT('OPRANNUM,',D23.15)
              END IF
              WRITE(8,52) OPNRD
 52           FORMAT('OPNRD   ,',I8)
              WRITE(8,53) TOLNRD
 53           FORMAT('TOLNRD  ,',I8)
          END IF
C
          YES=.FALSE.
          DO I=1,MAXCMP
              IF(ISCOMP(I)) YES=.TRUE.
          END DO
          IF(YES) THEN
C     DO COMP VARAIBLES OUTPUT HERE
              WRITE(8,*)'COMPVAR'
              DO I=1,MAXCMP
                  IF(ISCOMP(I)) THEN
                      WRITE(8,11) VARNAM(I),DBLE(I),DBLE(INT(VARABL(I,3)))
 11                   FORMAT(A8,',',D23.15,',',D23.15,',,,,')
 12                   FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',,,')
 13                   FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,',,')
 14                   FORMAT(A8,' ','PIVOT',',',D23.15,',',D23.15,',',D23.15,',,,')
                  END IF
              END DO
              WRITE(8,*)'EOS'
          END IF
C
C     TOLERANCE VARIABLES
C
          IF(TVBCNT.NE.0) THEN
C     HERE IS WHERE DATA IS STORED
C     DO TOL VARAIBLES OUTPUT HERE
              WRITE(8,*)'TVAR'
              DO I=1,TVBCNT
                  II=I+MAXCMP
                  IF(VARNAM(II)(1:5).NE.'STILT'.AND.
     1               VARNAM(II)(1:5).NE.'BTILT'.AND.
     1               VARNAM(II)(1:4).NE.'ROLL'.AND.
     1               VARNAM(II)(1:4).NE.'DISP') THEN
                      WRITE(8,11) VARNAM(II),DBLE(INT(VARABL(II,3))),
     1                VARABL(II,8)
                  END IF
                  IF(VARNAM(II)(1:4).EQ.'DISP') THEN
                      WRITE(8,12) VARNAM(II),DBLE(INT(VARABL(II,3))),
     1                DBLE(INT(VARABL(II,7))),
     1                VARABL(II,8)
                  END IF
                  IF(VARNAM(II)(1:4).EQ.'ROLL') THEN
                      WRITE(8,13) VARNAM(II),DBLE(INT(VARABL(II,3))),
     1                DBLE(INT(VARABL(II,7))),
     1                VARABL(II,8),DBLE(INT(VARABL(II,12)))
                  END IF
                  IF(VARNAM(II)(1:5).EQ.'STILT') THEN
C     DO PIVOT
                      IF(VARABL(II,9).NE.0.0D0.OR.VARABL(II,10).NE.0.0D0.OR.
     1                VARABL(II,11).NE.0.0D0)
     2                WRITE(8,14) VARNAM(II),DBLE(INT(VARABL(II,9)))
     3                ,DBLE(INT(VARABL(II,10))),DBLE(INT(VARABL(II,11)))
C     DO STILT
                      WRITE(8,11) VARNAM(II),DBLE(INT(VARABL(II,3))),
     1                VARABL(II,8)
                  END IF
                  IF(VARNAM(II)(1:5).EQ.'BTILT') THEN
C     DO PIVOT
                      IF(VARABL(II,9).NE.0.0D0.OR.VARABL(II,10).NE.0.0D0.OR.
     1                VARABL(II,11).NE.0.0D0)
     2                WRITE(8,14) VARNAM(II),DBLE(INT(VARABL(II,9)))
     3                ,DBLE(INT(VARABL(II,10))),DBLE(INT(VARABL(II,11)))
C     DO BTILT
                      WRITE(8,12) VARNAM(II),DBLE(INT(VARABL(II,3))),
     1                DBLE(INT(VARABL(II,7))),VARABL(II,8)
                  END IF
              END DO
              WRITE(8,*)'EOS'
          END IF
C
          YES=.FALSE.
          DO I=1,MAXFOCRIT
              IF(ISCRIT(I)) YES=.TRUE.
          END DO
C     FOCRITS
          IF(YES) THEN
C     HERE IS WHERE DATA IS STORED
C
C     DO FOCRIT OUTPUT HERE
              WRITE(8,*)'FOCRIT'
              DO I=1,MAXFOCRIT
C     THE OPERAND AND FUNCTION NAME AND NW1 AND NW2
                  IF(INT(OPERND(I,1)).EQ.0) BWORD='FUNC00'
                  IF(INT(OPERND(I,1)).EQ.1) BWORD='FUNC01'
                  IF(INT(OPERND(I,1)).EQ.2) BWORD='FUNC02'
                  IF(INT(OPERND(I,1)).EQ.3) BWORD='FUNC03'
                  IF(INT(OPERND(I,1)).EQ.4) BWORD='FUNC04'
                  IF(INT(OPERND(I,1)).EQ.5) BWORD='FUNC05'
                  IF(INT(OPERND(I,1)).EQ.6) BWORD='FUNC06'
                  IF(INT(OPERND(I,1)).EQ.7) BWORD='FUNC07'
                  IF(INT(OPERND(I,1)).EQ.8) BWORD='FUNC08'
                  IF(INT(OPERND(I,1)).EQ.9) BWORD='FUNC09'
                  IF(INT(OPERND(I,1)).EQ.10) BWORD='FUNC10'
                  IF(ISCRIT(I)) THEN
C
                      IF(BWORD.EQ.'FUNC00') THEN
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,101) OPNAM(I),DBLE(I),OPERND(I,8),OPERND(I,9),
     1                        OPERND(I,10)
                          END IF
 101                      FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,102) OPNAM(I),DBLE(I),OPERND(I,9),
     1                        OPERND(I,10)
                          END IF
 102                      FORMAT(A8,',',D23.15,',,',D23.15,',',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,103) OPNAM(I),DBLE(I),
     1                        OPERND(I,10)
                          END IF
 103                      FORMAT(A8,',',D23.15,',,,',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,104) OPNAM(I),DBLE(I)
                          END IF
 104                      FORMAT(A8,',',D23.15,',,,,,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,105) OPNAM(I),DBLE(I),OPERND(I,8),
     1                        OPERND(I,10)
                          END IF
 105                      FORMAT(A8,',',D23.15,',',D23.15,',,',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,106) OPNAM(I),DBLE(I),OPERND(I,8),OPERND(I,9)
                          END IF
 106                      FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',,,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,107) OPNAM(I),DBLE(I),OPERND(I,8)
                          END IF
 107                      FORMAT(A8,',',D23.15,',',D23.15,',,,,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,108) OPNAM(I),DBLE(I),OPERND(I,9)
                          END IF
 108                      FORMAT(A8,',',D23.15,',,',D23.15,',,,')
                      ELSE
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1101) BWORD,OPNAM(I),DBLE(I),OPERND(I,8),OPERND(I,9),
     1                        OPERND(I,10)
                          END IF
 1101                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,','
     1                    ,D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1102) BWORD,OPNAM(I),DBLE(I),OPERND(I,9),
     1                        OPERND(I,10)
                          END IF
 1102                     FORMAT(A8,' ',A8,',',D23.15,',,',D23.15,',',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1103) BWORD,OPNAM(I),DBLE(I),
     1                        OPERND(I,10)
                          END IF
 1103                     FORMAT(A8,' ',A8,',',D23.15,',,,',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1104) BWORD,OPNAM(I),DBLE(I)
                          END IF
 1104                     FORMAT(A8,' ',A8,',',D23.15,',,,,,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1105) BWORD,OPNAM(I),DBLE(I),OPERND(I,8),
     1                        OPERND(I,10)
                          END IF
 1105                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,',D23.15,',,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1106) BWORD,OPNAM(I),DBLE(I),OPERND(I,8),OPERND(I,9)
                          END IF
 1106                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,',,,')
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1107) BWORD,OPNAM(I),DBLE(I),OPERND(I,8)
                          END IF
 1107                     FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,,,')
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1108) BWORD,OPNAM(I),DBLE(I),OPERND(I,9)
                          END IF
 1108                     FORMAT(A8,' ',A8,',',D23.15,',,',D23.15,',,,')
                      END IF
C
                  END IF
                  IF(OPERDESC(I)(1:8).NE.'        ') THEN
                      WRITE(BF1,100) I
                      READ(BF1,110) AI1
 100                  FORMAT(I1)
 110                  FORMAT(A1)
                      OPNNMM='OP'//AI1
                      WRITE(8,1701) OPNNMM,OPERDESC(I)(1:69)
                  END IF
 1701             FORMAT('OP_DESC',1x,A3,1X,A69)
              END DO
              WRITE(8,*)'EOS'
          END IF
C
C     TOPER
          YES=.FALSE.
          DO II=1,MAXTOP
              I=II+MAXFOCRIT
              IF(ISTOP(II)) YES=.TRUE.
          END DO
          IF(YES) THEN
C     HERE IS WHERE DATA IS STORED
              WRITE(8,*)'TOPER'
              DO II=1,MAXTOP
                  I=II+MAXFOCRIT
C     THE OPERAND AND FUNCTION NAME AND NW1 AND NW2
                  IF(INT(OPERND(I,1)).EQ.0) BWORD='FUNC00'
                  IF(INT(OPERND(I,1)).EQ.1) BWORD='FUNC01'
                  IF(INT(OPERND(I,1)).EQ.2) BWORD='FUNC02'
                  IF(INT(OPERND(I,1)).EQ.3) BWORD='FUNC03'
                  IF(INT(OPERND(I,1)).EQ.4) BWORD='FUNC04'
                  IF(INT(OPERND(I,1)).EQ.5) BWORD='FUNC05'
                  IF(INT(OPERND(I,1)).EQ.6) BWORD='FUNC06'
                  IF(INT(OPERND(I,1)).EQ.7) BWORD='FUNC07'
                  IF(INT(OPERND(I,1)).EQ.8) BWORD='FUNC08'
                  IF(INT(OPERND(I,1)).EQ.9) BWORD='FUNC09'
                  IF(INT(OPERND(I,1)).EQ.10) BWORD='FUNC10'
                  IF(ISTOP(II)) THEN
C
                      IF(BWORD.EQ.'FUNC00') THEN
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,9101) OPNAM(I),DBLE(II),OPERND(I,8),OPERND(I,9),
     1                        OPERND(I,10),OPERND(I,20)
 9101                         FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',',D23.15,
     1                        ',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,9102) OPNAM(I),DBLE(II),OPERND(I,9),
     1                        OPERND(I,10),OPERND(I,20)
 9102                         FORMAT(A8,',',D23.15,',,',D23.15,',',D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,9103) OPNAM(I),DBLE(II),
     1                        OPERND(I,10),OPERND(I,20)
 9103                         FORMAT(A8,',',D23.15,',,,',D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,9104) OPNAM(I),DBLE(II),OPERND(I,20)
 9104                         FORMAT(A8,',',D23.15,',,,,',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,9105) OPNAM(I),DBLE(II),OPERND(I,8),
     1                        OPERND(I,10),OPERND(I,20)
 9105                         FORMAT(A8,',',D23.15,',',D23.15,',,',D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,9106) OPNAM(I),DBLE(II),OPERND(I,8),OPERND(I,9)
     1                        ,OPERND(I,20)
 9106                         FORMAT(A8,',',D23.15,',',D23.15,',',D23.15,',,',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,9107) OPNAM(I),DBLE(II),OPERND(I,8),OPERND(I,20)
 9107                         FORMAT(A8,',',D23.15,',',D23.15,',,,',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,9108) OPNAM(I),DBLE(II),OPERND(I,9),OPERND(I,20)
 9108                         FORMAT(A8,',',D23.15,',,',D23.15,',,',D23.15)
                          END IF
                      ELSE
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1901) BWORD,OPNAM(I),DBLE(II),OPERND(I,8),OPERND(I,9),
     1                        OPERND(I,10),OPERND(I,20)
 1901                         FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,','
     1                        ,D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1902) BWORD,OPNAM(I),DBLE(II),OPERND(I,9),
     1                        OPERND(I,10),OPERND(I,20)
 1902                         FORMAT(A8,' ',A8,',',D23.15,',,',D23.15,',',D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1903) BWORD,OPNAM(I),DBLE(II),
     1                        OPERND(I,10),OPERND(I,20)
 1903                         FORMAT(A8,' ',A8,',',D23.15,',,,',D23.15,',',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1904) BWORD,OPNAM(I),DBLE(II),OPERND(I,20)
 1904                         FORMAT(A8,' ',A8,',',D23.15,',,,,',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.0.0D0) THEN
                              WRITE(8,1905) BWORD,OPNAM(I),DBLE(II),OPERND(I,8),
     1                        OPERND(I,10),OPERND(I,20)
 1905                         FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,',D23.15,','
     1                        ,D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1906) BWORD,OPNAM(I),DBLE(II),OPERND(I,8),OPERND(I,9)
     1                        ,OPERND(I,20)
 1906                         FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',',D23.15,',,'
     1                        ,D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.0.0D0.AND.OPERND(I,11).EQ.1.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1907) BWORD,OPNAM(I),DBLE(II),OPERND(I,8),OPERND(I,20)
 1907                         FORMAT(A8,' ',A8,',',D23.15,',',D23.15,',,,',D23.15)
                          END IF
C
                          IF(OPERND(I,18).EQ.1.0D0.AND.OPERND(I,11).EQ.0.0D0
     1                    .AND.OPERND(I,12).EQ.1.0D0) THEN
                              WRITE(8,1908) BWORD,OPNAM(I),DBLE(II),OPERND(I,9),OPERND(I,20)
 1908                         FORMAT(A8,' ',A8,',',D23.15,',,',D23.15,',,',D23.15)
                          END IF
                      END IF
C
                  END IF
                  IF(OPERDESC(I)(1:8).NE.'        ') THEN
                      WRITE(BF1,100) I
                      READ(BF1,110) AI1
                      OPNNMM='OP'//AI1
                      WRITE(8,1701) OPNNMM,OPERDESC(I)(1:69)
                  END IF
              END DO
              WRITE(8,*)'EOS'
          END IF
          WRITE(8,*)'EOM'
          CALL CLOSE_FILE(8,1)
C     NOW READ IN THE MACRO FROM CARDTEXT.DAT
          SAVE_KDP(1)=SAVEINPT(1)
          INPUT='INPUT CR'
          CALL PROCES
          REST_KDP(1)=RESTINPT(1)
          RETURN
      END

C SUB RECSIVEITER.FOR
      SUBROUTINE RECURSIVEITER(IFUNCTION,ICHK,ITERROR)
C
          IMPLICIT NONE
          INTEGER IFUNCTION,ICHK
          LOGICAL ITERROR
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          CALL ITER(IFUNCTION,ICHK,ITERROR)
          RETURN
      END

C
C SUB RESTOR.FOR
      SUBROUTINE RESTOR(LCODE)
C
          IMPLICIT NONE
C
          INTEGER ISURF,I,VTYPE,ALTYPE,VADD,VCFG
C
          CHARACTER AV1*23
C
          REAL*8 V1,VTEMP
C
          REAL*8 NEWDEFVAL
C
          COMMON/DEFVALCOM/NEWDEFVAL
C
!      DIMENSION VSAVE(:)
C
!      ALLOCATABLE :: VSAVE
C
          LOGICAL NOP,LCODE,ERR1,ERR2
C
          COMMON/CAUX1/V1,AV1
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
C
C       THIS IS SUBROUTINE RESTOR. THIS IS THE SUBROUTINE WHICH
C       HANDLES CMD LEVEL COMMANDS "RESTORE", "RESTORE MIN" AND
C       "RESTORE ORIG"
C
C       "RESTORE" EITHER TAKES NO QUALIFIER OR THE QUALIFIERS
C       "MIN" AND "ORIG"
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*)'"RESTORE" DOES RESTORATION IN OPTIMIZATION'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)'"RESTORE" TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'MIN'.AND.WQ.NE.'ORIG'.AND.WQ.NE.'NP') THEN
                  WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(SQ.EQ.1.AND.S1.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"RESTORE" TAKES NUMERIC WORD #1 OR QUALIFIER WORD INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) THEN
              IF(DF1.EQ.1) W1=0.0D0
              IF(W1.LT.0.0D0.OR.W1.GT.1.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'NUMERIC WORD #1 MUST BE IN THE RANGE 0.0 TO 1.0'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(.NOT.SOLEXT) THEN
              WRITE(OUTLYNE,*)'NO SOLUTION VECTOR EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C       PROCEED WITH ACTION FOR COMMAND
          IF(VBCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'THE VARIABLE SUBFILE IS EMPTY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO VARIABLE DATA VALUES EXISTS TO RESTORE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(OPCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)'THE MERIT SUBFILE IS EMPTY'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO OPERAND DATA VALUES EXISTS'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     THERE IS DATA TO RESTORE, DO THE APPROPRIATE RESTORATIONS
          IF(SQ.EQ.0.OR.SQ.EQ.1.AND.WQ.EQ.'NP') THEN
C     JUST PLAIN RESTORE, RESTORE ALL VARIABLE VALUES TO THEIR
C     PREVIOUS VALUES. THIS IS SIMILAR "RESTORE ORIG" WHICH RESTORES
C     THE VARIABLES TO THE ORIGINAL STARTING POINT VALUES
C     IF W1=1.0 NO RESTORE TAKES PLACE
C     IF W1=0.0 FULL RESTORE TAKES PLACE
              DO I=1,VBCNT
                  VTEMP=0.0D0
C     VTEMP IS THE CURRENT VALUE
                  VTEMP=VARABL(I,4)
C     THE NEW CURRENT VALUE WILL BE:
                  VARABL(I,4)=VTEMP
     1             -((1.0D0-W1)*(VTEMP-VARABL(I,5)))
C     THE NEW PREVIOUS VALUE WILL THE SAME AS IT WAS BEFORE
C     BOUNDS CHECKER
                  IF(VARABL(I,4).LT.VARABL(I,9)) THEN
                      VARABL(I,4)=VARABL(I,9)

                      IF(WQ.NE.'NP') THEN
                          WRITE(OUTLYNE,*)
     1                    'WARNING: '
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
                  IF(VARABL(I,4).GT.VARABL(I,10)) THEN
                      VARABL(I,4)=VARABL(I,10)

                      IF(WQ.NE.'NP') THEN
                          WRITE(OUTLYNE,*)
     1                    'WARNING: '
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)
     1                    'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
                          CALL SHOWIT(1)
                      END IF
                  END IF
C     THE CHANGE WILL BE:
                  VARABL(I,6)=VARABL(I,4)-VTEMP
C
              END DO
C
C     NOW APPLY THE CHANGE VECTOR TO THE LENS
C**********************************************************************
              DO I=1,VBCNT
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
                  IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                      VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                      IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(1,INT(VARABL(I,3)))=V1
C                       CURVATURE DONE
                      END IF
                      IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(24,INT(VARABL(I,3)))=V1
C                       TORIC CURVATURE DONE
                      END IF
                      IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                          IF(VTYPE.EQ.3) ALTYPE=3
                          IF(VTYPE.EQ.4) ALTYPE=2
                          IF(VTYPE.EQ.5) ALTYPE=4
                          IF(VTYPE.EQ.6) ALTYPE=5
                          IF(VTYPE.EQ.7) ALTYPE=6
                          IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C                       THESE VARIABLES DONE
                      END IF
                      IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1                124.AND.VTYPE.LE.149) THEN
                          IF(VTYPE.EQ.11) ALTYPE=41
                          IF(VTYPE.EQ.12) ALTYPE=37
                          IF(VTYPE.EQ.13) ALTYPE=38
                          IF(VTYPE.EQ.14) ALTYPE=39
                          IF(VTYPE.EQ.15) ALTYPE=40
                          IF(VTYPE.EQ.16) ALTYPE=118
                          IF(VTYPE.EQ.17) ALTYPE=119
                          IF(VTYPE.EQ.18) ALTYPE=120
                          IF(VTYPE.EQ.19) ALTYPE=114
                          IF(VTYPE.EQ.20) ALTYPE=115
                          IF(VTYPE.EQ.21) ALTYPE=46
                          IF(VTYPE.EQ.22) ALTYPE=47
                          IF(VTYPE.EQ.23) ALTYPE=48
                          IF(VTYPE.EQ.24) ALTYPE=49
                          IF(VTYPE.EQ.25) ALTYPE=50
                          IF(VTYPE.EQ.75) ALTYPE=43
                          IF(VTYPE.EQ.124) ALTYPE=71
                          IF(VTYPE.EQ.125) ALTYPE=72
                          IF(VTYPE.EQ.126) ALTYPE=73
                          IF(VTYPE.EQ.127) ALTYPE=74
                          IF(VTYPE.EQ.128) ALTYPE=75
                          IF(VTYPE.EQ.129) ALTYPE=81
                          IF(VTYPE.EQ.130) ALTYPE=82
                          IF(VTYPE.EQ.131) ALTYPE=83
                          IF(VTYPE.EQ.132) ALTYPE=84
                          IF(VTYPE.EQ.133) ALTYPE=85
                          IF(VTYPE.EQ.134) ALTYPE=116
                          IF(VTYPE.EQ.135) ALTYPE=86
                          IF(VTYPE.EQ.136) ALTYPE=87
                          IF(VTYPE.EQ.137) ALTYPE=78
                          IF(VTYPE.EQ.138) ALTYPE=79
                          IF(VTYPE.EQ.139) ALTYPE=80
                          IF(VTYPE.EQ.140) ALTYPE=89
                          IF(VTYPE.EQ.141) ALTYPE=11
                          IF(VTYPE.EQ.142) ALTYPE=10
                          IF(VTYPE.EQ.143) ALTYPE=90
                          IF(VTYPE.EQ.144) ALTYPE=91
                          IF(VTYPE.EQ.145) ALTYPE=92
                          IF(VTYPE.EQ.146) ALTYPE=93
                          IF(VTYPE.EQ.147) ALTYPE=94
                          IF(VTYPE.EQ.148) ALTYPE=95
                          IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                      END IF
                      IF(VTYPE.EQ.150) THEN
                          V1=VARABL(I,4)
                          GPREG(INT(VARABL(I,3)))=V1
                      END IF
                      IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                          ALTYPE=VTYPE-249
C     NEW VALUE IS:
                          V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                          ISURF=INT(VARABL(I,3))
                          DEFGR1=ALENS(103,ISURF)
                          DEFGR2=ALENS(104,ISURF)
                          DEFGR3=ALENS(105,ISURF)
                          DEFGR4=ALENS(106,ISURF)
                          DEFGR5=ALENS(107,ISURF)
                          DEFGR6=0.0D0
                          DEFGR7=ALENS(109,ISURF)
                          DEFGR8=0.0D0
                          ACTNUM=ALTYPE
                          NEWDEFVAL=V1
                          ERR1=.FALSE.
                          ERR2=.FALSE.
                          CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                      END IF
                      IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                      END IF
                      IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                      END IF
C
                  ELSE
C     ERROR IS HERE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                      VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                      VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG
C
C     THE NEW VARAIBLE VALUE IS JUST
                      V1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                      VADD=INT(VARABL(I,14))
                      IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                      IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                      CALL AUXNTA
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
                      IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1                CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1                .OR.CFADD(VADD,1).EQ.141) THEN
                          CFVAL(VADD,2)=V1
                          CFCHAR(VADD,2)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                    =AV1(1:23)
                      ELSE
                          CFVAL(VADD,1)=V1
                          CFCHAR(VADD,1)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                    =AV1(1:23)
                      END IF
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C
C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
                  END IF
C     LOOP TO NEXT VARIABL
              END DO
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
              CALL FIXDEFORMFILE
              F6=1
              F1=0
              F22=0
              LNSTYP=2
              CALL LNSEOS
C**********************************************************************
C     AND RE-EVALUATE THE OPERANDS AND DISPLAY
C     THE NEW FIGURE OF MERIT, THE OLD FIGURE OF MERIT  AND ITS CHANGE
              IF(WQ.EQ.'NP') NOP=.TRUE.
              IF(WQ.NE.'NP') NOP=.FALSE.

              SAVE_KDP(4)=SAVEINPT(4)
              WC='FMT'
              F28=1
              MSG=.FALSE.
              OPTMES=.FALSE.
              IF(NOP) WQ='NP'
              IF(NOP) SQ=1
              IF(.NOT.NOP) WQ='        '
              IF(.NOT.NOP) SQ=0
              SN=0
              SST=0
              WS=' '
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              STI=0
              IF(LCODE) CALL FMT2
              IF(.NOT.LCODE) CALL FMT3
              REST_KDP(4)=RESTINPT(4)
              RETURN
          END IF
          IF(SQ.EQ.1.AND.WQ.EQ.'ORIG') THEN
C     RESTORE ALL VARIABLE VALUES TO THEIR
C     ORIGINAL VALUES.
              VARABL(1:VBCNT,4)=VARABL(1:VBCNT,13)
              VARABL(1:VBCNT,5)=VARABL(1:VBCNT,13)
              VARABL(1:VBCNT,6)=0.0D0
C
C     NOW APPLY THE CHANGE VECTOR TO THE LENS
C**********************************************************************
              DO I=1,VBCNT
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
                  IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                      VTYPE=INT(VARABL(I,1))
C                          CURVATURE
                      IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(1,INT(VARABL(I,3)))=V1
C                       CURVATURE DONE
                      END IF
                      IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(24,INT(VARABL(I,3)))=V1
C                       TORIC CURVATURE DONE
                      END IF
                      IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                          IF(VTYPE.EQ.3) ALTYPE=3
                          IF(VTYPE.EQ.4) ALTYPE=2
                          IF(VTYPE.EQ.5) ALTYPE=4
                          IF(VTYPE.EQ.6) ALTYPE=5
                          IF(VTYPE.EQ.7) ALTYPE=6
                          IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C                       THESE VARIABLES DONE
                      END IF
                      IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1                124.AND.VTYPE.LE.149) THEN
                          IF(VTYPE.EQ.11) ALTYPE=41
                          IF(VTYPE.EQ.12) ALTYPE=37
                          IF(VTYPE.EQ.13) ALTYPE=38
                          IF(VTYPE.EQ.14) ALTYPE=39
                          IF(VTYPE.EQ.15) ALTYPE=40
                          IF(VTYPE.EQ.16) ALTYPE=118
                          IF(VTYPE.EQ.17) ALTYPE=119
                          IF(VTYPE.EQ.18) ALTYPE=120
                          IF(VTYPE.EQ.19) ALTYPE=114
                          IF(VTYPE.EQ.20) ALTYPE=115
                          IF(VTYPE.EQ.21) ALTYPE=46
                          IF(VTYPE.EQ.22) ALTYPE=47
                          IF(VTYPE.EQ.23) ALTYPE=48
                          IF(VTYPE.EQ.24) ALTYPE=49
                          IF(VTYPE.EQ.25) ALTYPE=50
                          IF(VTYPE.EQ.75) ALTYPE=43
                          IF(VTYPE.EQ.124) ALTYPE=71
                          IF(VTYPE.EQ.125) ALTYPE=72
                          IF(VTYPE.EQ.126) ALTYPE=73
                          IF(VTYPE.EQ.127) ALTYPE=74
                          IF(VTYPE.EQ.128) ALTYPE=75
                          IF(VTYPE.EQ.129) ALTYPE=81
                          IF(VTYPE.EQ.130) ALTYPE=82
                          IF(VTYPE.EQ.131) ALTYPE=83
                          IF(VTYPE.EQ.132) ALTYPE=84
                          IF(VTYPE.EQ.133) ALTYPE=85
                          IF(VTYPE.EQ.134) ALTYPE=116
                          IF(VTYPE.EQ.135) ALTYPE=86
                          IF(VTYPE.EQ.136) ALTYPE=87
                          IF(VTYPE.EQ.137) ALTYPE=78
                          IF(VTYPE.EQ.138) ALTYPE=79
                          IF(VTYPE.EQ.139) ALTYPE=80
                          IF(VTYPE.EQ.140) ALTYPE=89
                          IF(VTYPE.EQ.141) ALTYPE=11
                          IF(VTYPE.EQ.142) ALTYPE=10
                          IF(VTYPE.EQ.143) ALTYPE=90
                          IF(VTYPE.EQ.144) ALTYPE=91
                          IF(VTYPE.EQ.145) ALTYPE=92
                          IF(VTYPE.EQ.146) ALTYPE=93
                          IF(VTYPE.EQ.147) ALTYPE=94
                          IF(VTYPE.EQ.148) ALTYPE=95
                          IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                      END IF
                      IF(VTYPE.EQ.150) THEN
                          V1=VARABL(I,4)
                          GPREG(INT(VARABL(I,3)))=V1
                      END IF
                      IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                          ALTYPE=VTYPE-249
C     NEW VALUE IS:
                          V1=VARABL(I,4)
C     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                          ISURF=INT(VARABL(I,3))
                          DEFGR1=ALENS(103,ISURF)
                          DEFGR2=ALENS(104,ISURF)
                          DEFGR3=ALENS(105,ISURF)
                          DEFGR4=ALENS(106,ISURF)
                          DEFGR5=ALENS(107,ISURF)
                          DEFGR6=0.0D0
                          DEFGR7=ALENS(109,ISURF)
                          DEFGR8=0.0D0
                          ACTNUM=ALTYPE
                          NEWDEFVAL=V1
                          ERR1=.FALSE.
                          ERR2=.FALSE.
                          CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                      END IF
                      IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                      END IF
                      IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                          V1=VARABL(I,4)
                          FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C                       SPECIAL SURFACE COEFICIENTS DONE
                      END IF
C
                  ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                      VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                      VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG
C
C     THE NEW VARAIBLE VALUE IS JUST
                      V1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                      VADD=INT(VARABL(I,14))
                      IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                      IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                      CALL AUXNTA
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
C
                      IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1                CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1                .OR.CFADD(VADD,1).EQ.141) THEN
                          CFVAL(VADD,2)=V1
                          CFCHAR(VADD,2)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                    =AV1(1:23)
                      ELSE
                          CFVAL(VADD,1)=V1
                          CFCHAR(VADD,1)=AV1
C     NOW UPDATE THE CONFIG ARRAY
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                    =AV1(1:23)
                      END IF
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C
C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
                  END IF
C     LOOP TO NEXT VARIABL
              END DO
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
              CALL FIXDEFORMFILE
              F6=1
              F1=0
              F22=0
              LNSTYP=2
              CALL LNSEOS
C**********************************************************************
C     AND RE-EVALUATE THE OPERANDS AND DISPLAY
C     THE NEW FIGURE OF MERIT, THE OLD FIGURE OF MERIT  AND ITS CHANGE
              SAVE_KDP(4)=SAVEINPT(4)
              F28=1
              MSG=.FALSE.
              OPTMES=.FALSE.
              WC='FMT'
              WQ='        '
              SQ=0
              SN=0
              SST=0
              WS=' '
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              STI=0
              IF(LCODE) CALL FMT2
              IF(.NOT.LCODE) CALL FMT3
              REST_KDP(4)=RESTINPT(4)
              RETURN
          END IF
C
          IF(WQ.EQ.'MIN') THEN
              CALL RESTMIN
              RETURN
          END IF
          RETURN
      END
C SUB ITER.FOR
      SUBROUTINE ITER(IFUNCTION,ICHK,ITERROR)
C
          IMPLICIT NONE
C
          INTEGER ALLOERR,IIID,JJJD,I,J,VCFG,VTYPE,ALTYPE,VADD
C
          INTEGER MAXCNT,IFUNCTION,ICHK,IA,JA
C
          CHARACTER AV1*23,OLDAV1*23,AN1*23
C
          REAL*8 N1,V1,OLDV1,DERVAL,OREG
C
          REAL*8 DERIV
          DIMENSION DERIV(:,:)
          ALLOCATABLE :: DERIV
C
          INTEGER ISURF
C
          REAL*8 NEWDEFVAL
C
          COMMON/DEFVALCOM/NEWDEFVAL
C
          LOGICAL GETTER,PLL,SILENT,ERR1,ERR2
          LOGICAL ITERROR
C
          COMMON/PLLPLL/PLL
C
          COMMON/RETTEG/GETTER,DERVAL
C
          COMMON/CAUX1/N1,AN1
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
C
          SAVE DERIV
C
          IIID=OPCNT
          JJJD=VBCNT
C
C       THIS IS SUBROUTINE ITER. THIS IS THE SUBROUTINE WHICH
C       HANDLES AN OPTIMIZATION, ITERATION CYCLE.
C
C     NOW WE CHECK FOR VALID INPUT
C     ITER CAN TAKE QUALIFIER WORDS "DIRECT" OR "POWL OR "POWELL" OR "FULL"
C     OR IT CAN TAKE NUMERIC WORD #1. IT TAKES NO STRING INPUT AND
C     HAS NO RESPONSE TO "?".
          IF(GETTER) GO TO 9876
C
          IF(ICHK.EQ.1) THEN
              IF(STI.EQ.1) THEN
                  IF(WC.NE.'RSV'.AND.WC.NE.'SV') WRITE(OUTLYNE,*)
     1            '"(ITER OR IT) INITIATES AN OPTIMIZATION CYCLE'
                  IF(WC.EQ.'RSV') WRITE(OUTLYNE,*)
     1            '"RSV" RESOLVE THE CURRENT MATRIX AFTER FIRST DOING A "RESTORE"'
                  IF(WC.EQ.'SV') WRITE(OUTLYNE,*)
     1            '"RSV" RESOLVE THE CURRENT MATRIX WITHOUT DOING A "RESTORE"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(SST.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"ITER", "IT", "SV" AND "RSV" TAKE NO STRING INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
              IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                  IF(WQ.NE.'ADJUST') THEN
                      WRITE(OUTLYNE,*)
     1                '"ITER", "IT", "SV" AND "RSV" TAKE NO '
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #2 THROUGH #5 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
              IF(SQ.EQ.1) THEN
                  IF(WQ.NE.'POWL'.AND.WQ.NE.'DIR'.AND.WQ.NE.'P'.AND.WQ.NE.'F'
     1            .AND.WQ.NE.'MDUMP'.AND.WQ.NE.'MDP'.AND.WQ.NE.'MDUMPA'
     2            .AND.WQ.NE.'POWELL'
     3            .AND.WQ.NE.'MDPA'.AND.WQ.NE.'FULL'.AND.WQ.NE.'D'.AND.WQ.NE.
     4            'A'.AND.WQ.NE.'ADJ'.AND.WQ.NE.'ADJUST') THEN
                      WRITE(OUTLYNE,*) WQ
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'INVALID QUALIFIER WORD USED WITH "ITER" OR "IT"'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
          END IF
C
          IF(WQ.EQ.'MDUMP'.OR.WQ.EQ.'MDP'.OR.WQ.EQ.'MDUMPA'
     1    .OR.WQ.EQ.'MDPA') THEN
              PLL=.FALSE.
              IIID=OPCNT
              JJJD=VBCNT
              IF(IIID.GE.JJJD)MAXCNT=IIID+1
              IF(IIID.LT.JJJD)MAXCNT=JJJD+1
              CALL MDUMP(MAXCNT,MAXCNT,DERIV)
              RETURN
          END IF
          IF(WQ.EQ.'FULL'.OR.WQ.EQ.'F') THEN
              PLL=.FALSE.
              F32=1
              SAVE_KDP(1)=SAVEINPT(1)
              WC='ITER'
              WQ='        '
              WS=' '
              W1=0.0D0
              W2=0.0D0
              W3=0.0D0
              W4=0.0D0
              W5=0.0D0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              SN=0
              STI=0
              SQ=0
              SST=0
              CALL RECURSIVEITER(IFUNCTION,ICHK,ITERROR)
              REST_KDP(1)=RESTINPT(1)
              IF(KILOPT) THEN
                  F28=0
                  CALL MACFAL
                  RETURN
              END IF
              IF(F32.EQ.0) RETURN
              F32=0
              SAVE_KDP(1)=SAVEINPT(1)
              WC='PFIND'
              WQ='        '
              WS=' '
              W1=0.0D0
              W2=0.0D0
              W3=0.0D0
              W4=0.0D0
              W5=0.0D0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              SN=0
              STI=0
              SQ=0
              SST=0
              CALL PFIND
              REST_KDP(1)=RESTINPT(1)
              IF(KILOPT) THEN
                  F28=0
                  CALL MACFAL
                  RETURN
              END IF
              F32=1
              SAVE_KDP(1)=SAVEINPT(1)
              WC='ITER'
              WQ='        '
              WS=' '
              W1=0.0D0
              W2=0.0D0
              W3=0.0D0
              W4=0.0D0
              W5=0.0D0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              SN=0
              STI=0
              SQ=0
              SST=0
              CALL RECURSIVEITER(IFUNCTION,ICHK,ITERROR)
              REST_KDP(1)=RESTINPT(1)
              SAVE_KDP(9)=SAVEINPT(9)
              WC='RESTORE'
              WQ='MIN'
              WS=' '
              W1=0.0D0
              W2=0.0D0
              W3=0.0D0
              W4=0.0D0
              W5=0.0D0
              DF1=1
              DF2=1
              DF3=1
              DF4=1
              DF5=1
              S1=0
              S2=0
              S3=0
              S4=0
              S5=0
              SN=0
              STI=0
              SQ=1
              SST=0
              CALL RESTOR(.TRUE.)
              REST_KDP(9)=RESTINPT(9)
              IF(KILOPT) THEN
                  F28=0
                  CALL MACFAL
                  RETURN
              END IF
              IF(F32.EQ.0) RETURN
              F32=0
              RETURN
          END IF
C       END OF ITER FULL
C
C     ITER ADJUST
C
          IF(WC.EQ.'IT'.OR.WC.EQ.'ITER') THEN
              IF(WQ.EQ.'ADJ'.OR.WQ.EQ.'A'.OR.WQ.EQ.'ADJUST') THEN
                  IF(DF1.EQ.1) THEN
                      IA=0
                  ELSE
                      IA=1
                  END IF
                  IF(DF2.EQ.1) THEN
                      JA=0
                  ELSE
                      JA=1
                  END IF
                  IF(IA.EQ.0.OR.IA.EQ.1.AND..NOT.DEREXT) THEN
C     RECOMPUTE DERIVATIVES USING NEW VARABL(J,8) VALUES
                      DEALLOCATE(DERIV,STAT=ALLOERR)
                      IIID=OPCNT
                      JJJD=VBCNT
                      IF(IIID.GE.JJJD)MAXCNT=IIID+1
                      IF(IIID.LT.JJJD)MAXCNT=JJJD+1
                      ALLOCATE(DERIV(1:MAXCNT,1:MAXCNT),STAT=ALLOERR)
                      DERIV(1:MAXCNT,1:MAXCNT)=0.0D0
                      CALL DERIVATIVES(MAXCNT,DERIV)
                  ELSE
C     USE EXISTING MATRIX
                  END IF
                  CALL ITERADJUST(MAXCNT,MAXCNT,DERIV,JA)
              END IF
          END IF
C
C
          IF(WC.EQ.'ITER'.AND.SQ.EQ.0
     1    .OR.WC.EQ.'IT'.AND.SQ.EQ.0
     2    .OR.WC.EQ.'IT'.AND.WQ.EQ.'P'.AND..NOT.DEREXT
     2    .OR.WC.EQ.'ITER'.AND.WQ.EQ.'POWELL'.AND..NOT.DEREXT
     2    .OR.WC.EQ.'RSV'.AND..NOT.DEREXT.OR.WC.EQ.'SV'.AND.
     3    .NOT.DEREXT
     4    .OR.WC.EQ.'ITER'.AND.WQ.EQ.'D'.OR.WC.EQ.'IT'
     5    .AND.WQ.EQ.'DIR'.OR.WC.EQ.'IT'.AND.WQ.EQ.'D'.OR.
     6    WC.EQ.'ITER'.AND.WQ.EQ.'DIR') THEN
C     THIS IS JUST PLAIN OLD ITER OR RSV OR SV OR ITER DIR
C     WITH NO MATRIX

C
C     THE ARRAY DERIV(I,J) CONTAINS THE PARTIAL DERIVATIVES OF
C     ALL DEFINED OPERANDS WITH RESPECT TO CHANGES IN ALL DEFINED
C     OPERANDS.
              DEALLOCATE(DERIV,STAT=ALLOERR)
              IIID=OPCNT
              JJJD=VBCNT
              IF(IIID.GE.JJJD)MAXCNT=IIID+1
              IF(IIID.LT.JJJD)MAXCNT=JJJD+1
              ALLOCATE(DERIV(1:MAXCNT,1:MAXCNT),STAT=ALLOERR)
C
              DERIV(1:MAXCNT,1:MAXCNT)=0.0D0

              CALL DERIVATIVES(MAXCNT,DERIV)
              GO TO 666
C
C     IF OPCNT=0 OR VBCNT=0, THEN THERE CAN BE NO ITER. DO THIS FIRST.
C
              IF(OPCNT.EQ.0.OR.VBCNT.EQ.0) THEN
                  WRITE(OUTLYNE,*)
     1            '"ITER", "IT", "SV" AND "RSV" REQUIRE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'VARIABLES AND OPERANDS TO EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'BEFORE THEY CAN FUNCTION'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF

C     THE INTEGER VARIABLE DERSIZ IS NOW SET TO THE LARGER
C     OF THE VALUES OPCNT AND VBCNT

              IF(VBCNT.GT.OPCNT) DERSIZ=VBCNT
              IF(VBCNT.LE.OPCNT) DERSIZ=OPCNT
C
              PLL=.FALSE.
C
C     CALCULATE THE VALUES OF ALL OPERANDS AND LOAD THEM INTO THE
C     OPERND ARRAY
C
              OPCALC_TYPE=3
              CALL OPCALC
              IF(F28.EQ.0) RETURN
              CALL OPLOAD
              IF(F28.EQ.0) RETURN
C     REMEMBER THE CURRENT, UN-PERTURBED FMT
              FMTFMT=0.0D0
              FMTFLG=.TRUE.
              FMTEXT=.TRUE.
              DO I=1,OPCNT
                  IF(OPERND(I,19).EQ.0.0D0) FMTFMT=FMTFMT+(OPERND(I,14)**2)
              END DO
              OLDOP(1:OPCNT,1:20)=OPERND(1:OPCNT,1:20)
              IF(DABS(FMTFMT).LE.1.0D-50) THEN
                  WRITE(OUTLYNE,*)
     1            '"FMT=0" AND NO IMPROVEMENT IS POSSIBLE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                  CALL SHOWIT(1)
                  RETURN
              END IF
C
C     NOW FOR EACH VARIABLE IN THE VARIABLES LIST, MAKE THE APPROPRIATE
C     CHANGE TO THE LENS, CALCULATE THE OPERANDS, LOAD THE OPERND ARRAY,
C     FORM THE PARTIAL DERIVATIVES AND LOAD THE APPROPRIATE ROW ENTRIES
C     OF THE COLUMN IN DERIV FOR THAT VARIABLE. PRINT A MESSAGE TO THE
C     EFFECT THAT THE DERIVATIVE MATRIX IS BEING GENERATED.
C     EACH ROW BELONGS TO AN OPERAND
C     EACH COLUMN BELONGS TO AN VARIABLE
C
              DO I=1,VBCNT
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
                  IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE AND DERIVATIVE STUFF
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                      VTYPE=INT(VARABL(I,1))
                      IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                          V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                          ALENS(1,INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                          F6=1
                          F1=0
                          F22=0
                          LNSTYP=2
                          CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                          OPCALC_TYPE=3
                          CALL OPCALC
                          IF(F28.EQ.0) RETURN
                          CALL OPLOAD
                          IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                          DO J=1,OPCNT
                              DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                        (DINMUL*VARABL(I,8))
                              DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                          END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                          ALENS(1,INT(VARABL(I,3)))=VARABL(I,4)
C                       CURVATURE DONE
                      END IF
                      IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                          V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                          ALENS(24,INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                          F6=1
                          F1=0
                          F22=0
                          LNSTYP=2
                          CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                          OPCALC_TYPE=3
                          CALL OPCALC
                          IF(F28.EQ.0) RETURN
                          CALL OPLOAD
                          IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                          DO J=1,OPCNT
                              DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                        (DINMUL*VARABL(I,8))
                              DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                          END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                          ALENS(24,INT(VARABL(I,3)))=VARABL(I,4)
C                       TORIC CURVATURE DONE
                      END IF
                      IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                          IF(VTYPE.EQ.3) ALTYPE=3
                          IF(VTYPE.EQ.4) ALTYPE=2
                          IF(VTYPE.EQ.5) ALTYPE=4
                          IF(VTYPE.EQ.6) ALTYPE=5
                          IF(VTYPE.EQ.7) ALTYPE=6
                          IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                          V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                          ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                          F6=1
                          F1=0
                          F22=0
                          LNSTYP=2
                          CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                          OPCALC_TYPE=3
                          CALL OPCALC
                          IF(F28.EQ.0) RETURN
                          CALL OPLOAD
                          IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                          DO J=1,OPCNT
                              DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                        (DINMUL*VARABL(I,8))
                              DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                          END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                          ALENS(ALTYPE,INT(VARABL(I,3)))=VARABL(I,4)
C                       THESE VARIABLES DONE
                      END IF
                      IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1                124.AND.VTYPE.LE.149) THEN
                          IF(VTYPE.EQ.11) ALTYPE=41
                          IF(VTYPE.EQ.12) ALTYPE=37
                          IF(VTYPE.EQ.13) ALTYPE=38
                          IF(VTYPE.EQ.14) ALTYPE=39
                          IF(VTYPE.EQ.15) ALTYPE=40
                          IF(VTYPE.EQ.16) ALTYPE=118
                          IF(VTYPE.EQ.17) ALTYPE=119
                          IF(VTYPE.EQ.18) ALTYPE=120
                          IF(VTYPE.EQ.19) ALTYPE=114
                          IF(VTYPE.EQ.20) ALTYPE=115
                          IF(VTYPE.EQ.21) ALTYPE=46
                          IF(VTYPE.EQ.22) ALTYPE=47
                          IF(VTYPE.EQ.23) ALTYPE=48
                          IF(VTYPE.EQ.24) ALTYPE=49
                          IF(VTYPE.EQ.25) ALTYPE=50
                          IF(VTYPE.EQ.75) ALTYPE=43
                          IF(VTYPE.EQ.124) ALTYPE=71
                          IF(VTYPE.EQ.125) ALTYPE=72
                          IF(VTYPE.EQ.126) ALTYPE=73
                          IF(VTYPE.EQ.127) ALTYPE=74
                          IF(VTYPE.EQ.128) ALTYPE=75
                          IF(VTYPE.EQ.129) ALTYPE=81
                          IF(VTYPE.EQ.130) ALTYPE=82
                          IF(VTYPE.EQ.131) ALTYPE=83
                          IF(VTYPE.EQ.132) ALTYPE=84
                          IF(VTYPE.EQ.133) ALTYPE=85
                          IF(VTYPE.EQ.134) ALTYPE=116
                          IF(VTYPE.EQ.135) ALTYPE=86
                          IF(VTYPE.EQ.136) ALTYPE=87
                          IF(VTYPE.EQ.137) ALTYPE=78
                          IF(VTYPE.EQ.138) ALTYPE=79
                          IF(VTYPE.EQ.139) ALTYPE=80
                          IF(VTYPE.EQ.140) ALTYPE=89
                          IF(VTYPE.EQ.141) ALTYPE=11
                          IF(VTYPE.EQ.142) ALTYPE=10
                          IF(VTYPE.EQ.143) ALTYPE=90
                          IF(VTYPE.EQ.144) ALTYPE=91
                          IF(VTYPE.EQ.145) ALTYPE=92
                          IF(VTYPE.EQ.146) ALTYPE=93
                          IF(VTYPE.EQ.147) ALTYPE=94
                          IF(VTYPE.EQ.148) ALTYPE=95
                          IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                          V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                          ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                          F6=1
                          F1=0
                          F22=0
                          LNSTYP=2
                          CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                          OPCALC_TYPE=3
                          CALL OPCALC
                          IF(F28.EQ.0) RETURN
                          CALL OPLOAD
                          IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                          DO J=1,OPCNT
                              DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                        (DINMUL*VARABL(I,8))
                              DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                          END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                          ALENS(ALTYPE,INT(VARABL(I,3)))=VARABL(I,4)
                      END IF
                      IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                          ALTYPE=VTYPE-249
C     NEW VALUE IS:
                          V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
C     SET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                          ISURF=INT(VARABL(I,3))
                          DEFGR1=ALENS(103,ISURF)
                          DEFGR2=ALENS(104,ISURF)
                          DEFGR3=ALENS(105,ISURF)
                          DEFGR4=ALENS(106,ISURF)
                          DEFGR5=ALENS(107,ISURF)
                          DEFGR6=0.0D0
                          DEFGR7=ALENS(109,ISURF)
                          DEFGR8=0.0D0
                          ACTNUM=ALTYPE
                          NEWDEFVAL=V1
                          ERR1=.FALSE.
                          ERR2=.FALSE.
                          CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
C     UPDATE THE LENS
                          F6=1
                          F1=0
                          F22=0
                          LNSTYP=2
                          CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                          OPCALC_TYPE=3
                          CALL OPCALC
                          IF(F28.EQ.0) RETURN
                          CALL OPLOAD
                          IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                          DO J=1,OPCNT
                              DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                        (DINMUL*VARABL(I,8))
                              DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                          END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
C     SET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                          ISURF=INT(VARABL(I,3))
                          DEFGR1=ALENS(103,ISURF)
                          DEFGR2=ALENS(104,ISURF)
                          DEFGR3=ALENS(105,ISURF)
                          DEFGR4=ALENS(106,ISURF)
                          DEFGR5=ALENS(107,ISURF)
                          DEFGR6=0.0D0
                          DEFGR7=ALENS(109,ISURF)
                          DEFGR8=0.0D0
                          ACTNUM=ALTYPE
                          NEWDEFVAL=VARABL(I,4)
                          ERR1=.FALSE.
                          ERR2=.FALSE.
                          CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                      END IF
                      IF(VTYPE.EQ.150) THEN
C     NEW VALUE IS:
                          V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
C     SET THE APPRORIATE REGISTER VALUE
                          ISURF=INT(VARABL(I,3))
                          OREG=GPREG(ISURF)
                          GPREG(ISURF)=V1
C     CALCULATE OPERANDS AND LOAD THEM
                          OPCALC_TYPE=3
                          CALL OPCALC
                          IF(F28.EQ.0) RETURN
                          CALL OPLOAD
                          IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                          DO J=1,OPCNT
                              DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                        (DINMUL*VARABL(I,8))
                              DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                          END DO
C     RESTORE THE REGISTER VALUE
                          ISURF=INT(VARABL(I,3))
                          GPREG(ISURF)=OREG
C     LOOP TO NEXT DERIVATIVE
C     DERIVATIVES DONE FOR VARIABLE I
                      END IF
                      IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                          V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                          FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                          F6=1
                          F1=0
                          F22=0
                          LNSTYP=2
                          CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                          OPCALC_TYPE=3
                          CALL OPCALC
                          IF(F28.EQ.0) RETURN
                          CALL OPLOAD
                          IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                          DO J=1,OPCNT
                              DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                        (DINMUL*VARABL(I,8))
                              DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                          END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                          FTFL01((VTYPE-26),INT(VARABL(I,3)))=VARABL(I,4)
C                       SPECIAL SURFACE COEFICIENTS DONE
                      END IF
                      IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                          V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                          FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                          F6=1
                          F1=0
                          F22=0
                          LNSTYP=2
                          CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                          OPCALC_TYPE=3
                          CALL OPCALC
                          IF(F28.EQ.0) RETURN
                          CALL OPLOAD
                          IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                          DO J=1,OPCNT
                              DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                        (DINMUL*VARABL(I,8))
                              DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                          END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                          FTFL01((VTYPE-27),INT(VARABL(I,3)))=VARABL(I,4)
C                       SPECIAL SURFACE COEFICIENTS DONE
                      END IF
                  ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                      VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                      VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG, DO AN EOS AND EVALUATE ALL OPERANDS
C
C     THE OLD VARIABLE VALUE IS:
                      OLDV1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
C     OLDVA1 IS THE VALUE WE WILL USE IN THE RESTORATION PROCESS
C     AFTER THE DERIVATIVE IS CALCULATED
                      VADD=INT(VARABL(I,14))
                      IF(CFADD(VADD,1).EQ.1.AND.OLDV1.NE.0.0D0) OLDV1=1.0D0/OLDV1
                      IF(CFADD(VADD,1).EQ.9.AND.OLDV1.NE.0.0D0) OLDV1=1.0D0/OLDV1
                      N1=OLDV1
                      CALL AUXNTA
                      OLDAV1=AN1
C     THE NEW VARAIBLE VALUE IS JUST
                      V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                      IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                      IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                      N1=V1
                      CALL AUXNTA
                      AV1=AN1
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
                      VADD=INT(VARABL(I,14))
C
                      IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1                CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1                .OR.CFADD(VADD,1).EQ.141) THEN
                          CFVAL(VADD,2)=V1
                          CFCHAR(VADD,2)=AV1
                      ELSE
                          CFVAL(VADD,1)=V1
                          CFCHAR(VADD,1)=AV1
                      END IF
C     NOW UPDATE THE CONFIG ARRAY
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C     NOW UPDATE THE CONFIG ARRAY
                      IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1                CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1                .OR.CFADD(VADD,1).EQ.141) THEN
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                    =AV1(1:23)
                      ELSE
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                    =AV1(1:23)
                      END IF
C
C     NOW EVALUATE THE OPERANDS
                      OPCALC_TYPE=3
                      CALL OPCALC
                      IF(F28.EQ.0) RETURN
                      CALL OPLOAD
                      IF(F28.EQ.0) RETURN
                      DO J=1,OPCNT
C     CALC THE DERIVATIVE FOR OPERAND J AND STORE IT
                          DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                    (DINMUL*VARABL(I,8))
                          DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                      END DO
C     NOW IN THE SAME PLACE WHERE AV1 WAS STUFFED, STUFF OLDAV1
                      VADD=INT(VARABL(I,14))
                      IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1                CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1                .OR.CFADD(VADD,1).EQ.141) THEN
                          CFVAL(VADD,2)=OLDV1
                          CFCHAR(VADD,2)=OLDAV1
C     NOW UPDATE THE CONFIG ARRAY
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                    =OLDAV1(1:23)
                      ELSE
                          CFVAL(VADD,1)=OLDV1
                          CFCHAR(VADD,1)=OLDAV1
C     NOW UPDATE THE CONFIG ARRAY
                          CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                    =OLDAV1(1:23)
                      END IF

C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
                  END IF
C     LOOP TO NEXT VARIABL
              END DO
C     WE JUST DID THE LAST CALCULATION AND WE WANT TO
C     RESTORE THE ORIGINAL OPERANDS
              OPERND(1:OPCNT,1:20)=OLDOP(1:OPCNT,1:20)
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
              F6=1
              F1=0
              F22=0
              LNSTYP=2
              CALL LNSEOS

C     THE DERIVATIVE MATRIX IS FORMED



 666          CONTINUE


              IF(KILOPT) THEN
C     NO SOLUTION IS ATTEMPTED
                  OUTLYNE='SOME OPERANDS ARE NOT CALCULABLE.'
                  CALL SHOWIT(1)
                  OUTLYNE='"ITER" CAN NOT PROCEED.'
                  CALL SHOWIT(1)
                  F28=0
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(WC.EQ.'ITER'.AND.SQ.EQ.0.OR.
     1    WC.EQ.'IT'.AND.SQ.EQ.0.OR.
     1    WC.EQ.'IT'.AND.WQ.EQ.'P'.OR.WC.EQ.'ITER'.AND.
     1    WQ.EQ.'POWELL'.OR.
     1    WC.EQ.'ITER'.AND.WQ.EQ.'DIR'.OR.
     1    WC.EQ.'ITER'.AND.WQ.EQ.'D'.OR.
     1    WC.EQ.'IT'.AND.WQ.EQ.'DIR'.OR.
     1    WC.EQ.'IT'.AND.WQ.EQ.'D') THEN
C     SOLVE THE MATRIX, CREATE THE SOLUTION VECTOR AND APPLY IT
C     TO THE CURRENT LENS WITHOUT DOING A RESTORE
C     CALL SOLVIT
C
              SOLEXT=.FALSE.
              IIID=OPCNT
              JJJD=VBCNT
              IF(IIID.GE.JJJD)MAXCNT=IIID+1
              IF(IIID.LT.JJJD)MAXCNT=JJJD+1
              SILENT=.FALSE.
              IF(IFUNCTION.EQ.2) SILENT=.TRUE.
              IF(WQ.NE.'P'.AND.WQ.NE.'POWELL') THEN
                  CALL SOLVIT(MAXCNT,MAXCNT,DERIV,SILENT)
              ELSE
                  CALL SOLVIT_POWELL(MAXCNT,MAXCNT,DERIV,SILENT)


 10               FORMAT('    NEW FMT = ',G23.15,1X,'CHANGE = ',G23.15)
                  WRITE(OUTLYNE,10) FMTFMT,(FMTFMT-OLDFMT)
                  CALL SHOWIT(0)
              END IF
              SOLEXT=.TRUE.
              RETURN
          ELSE
              IF(WC.EQ.'RSV'.AND.DEREXT) THEN
C     SOLVE THE MATRIX, CREATE THE SOLUTION VECTOR AND APPLY IT
C     TO THE CURRENT LENS AFTER DOING A RESTORE
                  IF(KILOPT) THEN
                      F28=0
                      CALL MACFAL
                      RETURN
                  END IF
                  PLL=.FALSE.
                  FMTFMT=0.0D0
                  DO I=1,OPCNT
                      FMTFMT=FMTFMT+(OPERND(I,14)**2)
                  END DO
                  IF(DABS(FMTFMT).LE.1.0D-50) THEN
                      WRITE(OUTLYNE,*)
     1                '"FMT=0" AND NO IMPROVEMENT IS POSSIBLE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  F28=1
                  MSG=.FALSE.
                  OPTMES=.FALSE.
                  SAVE_KDP(9)=SAVEINPT(9)
                  WC='RESTORE'
                  WQ='  '
                  WS=' '
                  W1=0.0D0
                  W2=0.0D0
                  W3=0.0D0
                  W4=0.0D0
                  W5=0.0D0
                  DF1=1
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  S1=0
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  SN=0
                  STI=0
                  SQ=0
                  SST=0
                  CALL RESTOR(.FALSE.)
                  REST_KDP(9)=RESTINPT(9)
                  IF(KILOPT) THEN
C     NO SOLUTION IS ATTEMPTED
                      OUTLYNE='SOME OPERANDS ARE NOT CALCULABLE.'
                      CALL SHOWIT(1)
                      OUTLYNE='"ITER" CAN NOT PROCEED.'
                      CALL SHOWIT(1)
                      F28=0
                      CALL MACFAL
                      RETURN
                  END IF
                  F28=1
                  MSG=.FALSE.
                  OPTMES=.FALSE.
                  SOLEXT=.FALSE.
                  IIID=OPCNT
                  JJJD=VBCNT
                  IF(IIID.GE.JJJD)MAXCNT=IIID+1
                  IF(IIID.LT.JJJD)MAXCNT=JJJD+1
                  SILENT=.FALSE.
                  IF(IFUNCTION.EQ.2) SILENT=.TRUE.
                  CALL SOLVIT(MAXCNT,MAXCNT,DERIV,SILENT)
                  SOLEXT=.TRUE.
                  RETURN
              END IF
              IF(WC.EQ.'SV'.AND.DEREXT) THEN
C     SOLVE THE MATRIX, CREATE THE SOLUTION VECTOR AND APPLY IT
C     TO THE CURRENT LENS WITHOUT DOING RESTORE
                  IF(KILOPT) THEN
                      F28=0
                      CALL MACFAL
                      RETURN
                  END IF
                  PLL=.FALSE.
                  FMTFMT=0.0D0
                  DO I=1,OPCNT
                      FMTFMT=FMTFMT+(OPERND(I,14)**2)
                  END DO
                  IF(DABS(FMTFMT).LE.1.0D-50) THEN
                      WRITE(OUTLYNE,*)
     1                '"FMT=0" AND NO IMPROVEMENT IS POSSIBLE'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'NO ACTION TAKEN'
                      CALL SHOWIT(1)
                      RETURN
                  END IF
                  F28=1
                  MSG=.FALSE.
                  OPTMES=.FALSE.
                  SOLEXT=.FALSE.
                  IIID=OPCNT
                  JJJD=VBCNT
                  IF(IIID.GE.JJJD)MAXCNT=IIID+1
                  IF(IIID.LT.JJJD)MAXCNT=JJJD+1
                  SILENT=.FALSE.
                  IF(IFUNCTION.EQ.2) SILENT=.TRUE.
                  CALL SOLVIT(MAXCNT,MAXCNT,DERIV,SILENT)
                  SOLEXT=.TRUE.
                  RETURN
              END IF
          END IF
C

C       THIS IS ONLY FOR GETTING A DERIVATIVE VALUE
 9876     CONTINUE
          GETTER=.FALSE.
          IF(ICHK.NE.0) THEN
              IF(DEREXT) DERVAL=DERIV(INT(W2),INT(W1))
              IF(.NOT.DEREXT) DERVAL=0.0D0
          ELSE
              DERVAL=0.0D0
          END IF
          RETURN
      END
C SUB DERIVATIVES.FOR
      SUBROUTINE DERIVATIVES(MAXCNT,DERIV)
C
          IMPLICIT NONE
C
          INTEGER I,J,VCFG,VTYPE,ALTYPE,VADD
C
          INTEGER MAXCNT
C
          CHARACTER AV1*23,OLDAV1*23,AN1*23
C
          REAL*8 N1,V1,OLDV1,DERVAL,OREG
C
          REAL*8 DERIV

          DIMENSION DERIV(1:MAXCNT,1:MAXCNT)
C
          INTEGER ISURF
C
          REAL*8 NEWDEFVAL
C
          COMMON/DEFVALCOM/NEWDEFVAL
C
          LOGICAL GETTER,PLL,ERR1,ERR2
C
          COMMON/PLLPLL/PLL
C
          COMMON/RETTEG/GETTER,DERVAL
C
          COMMON/CAUX1/N1,AN1
C
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
C
C       THIS IS SUBROUTINE DERIVATIVES. IT COMPUTES THE DAMPED LEAST SQUARES
C       DERIVATIVE MATRIX
C
C
C     IF OPCNT=0 OR VBCNT=0, THEN THERE CAN BE NO ITER. DO THIS FIRST.
C
          IF(OPCNT.EQ.0.OR.VBCNT.EQ.0) THEN
              WRITE(OUTLYNE,*)
     1        '"ITER", "IT", "SV" AND "RSV" REQUIRE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'VARIABLES AND OPERANDS TO EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)
     1        'BEFORE THEY CAN FUNCTION'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF

C     THE INTEGER VARIABLE DERSIZ IS NOW SET TO THE LARGER
C     OF THE VALUES OPCNT AND VBCNT

          IF(VBCNT.GT.OPCNT) DERSIZ=VBCNT
          IF(VBCNT.LE.OPCNT) DERSIZ=OPCNT
C
          PLL=.FALSE.
C
C     CALCULATE THE VALUES OF ALL OPERANDS AND LOAD THEM INTO THE
C     OPERND ARRAY
C
          OPCALC_TYPE=3
          CALL OPCALC
          IF(F28.EQ.0) RETURN
          CALL OPLOAD
          IF(F28.EQ.0) RETURN
C     REMEMBER THE CURRENT, UN-PERTURBED FMT
          FMTFMT=0.0D0
          FMTFLG=.TRUE.
          FMTEXT=.TRUE.
          DO I=1,OPCNT
              IF(OPERND(I,19).EQ.0.0D0) FMTFMT=FMTFMT+(OPERND(I,14)**2)
          END DO
          OLDOP(1:OPCNT,1:20)=OPERND(1:OPCNT,1:20)
          IF(DABS(FMTFMT).LE.1.0D-50) THEN
              WRITE(OUTLYNE,*)
     1        '"FMT=0" AND NO IMPROVEMENT IS POSSIBLE'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'NO ACTION TAKEN'
              CALL SHOWIT(1)
              RETURN
          END IF
C
C     NOW FOR EACH VARIABLE IN THE VARIABLES LIST, MAKE THE APPROPRIATE
C     CHANGE TO THE LENS, CALCULATE THE OPERANDS, LOAD THE OPERND ARRAY,
C     FORM THE PARTIAL DERIVATIVES AND LOAD THE APPROPRIATE ROW ENTRIES
C     OF THE COLUMN IN DERIV FOR THAT VARIABLE. PRINT A MESSAGE TO THE
C     EFFECT THAT THE DERIVATIVE MATRIX IS BEING GENERATED.
C     EACH ROW BELONGS TO AN OPERAND
C     EACH COLUMN BELONGS TO AN VARIABLE
C
          DO I=1,VBCNT
C     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
C     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
              IF(VARABL(I,2).EQ.1.0D0) THEN
C     NON-CONFIGS VARIABLE MEANING CONFIG 1
C     THIS IS A LENS LEVEL VARIABLE CHANGE AND DERIVATIVE STUFF
C     GET THE DATA TYPE NUMBER OF THE VARIABLE
                  VTYPE=INT(VARABL(I,1))
                  IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
C     SURFACE CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                      ALENS(1,INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                      F6=1
                      F1=0
                      F22=0
                      LNSTYP=2
                      CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                      OPCALC_TYPE=3
                      CALL OPCALC
                      IF(F28.EQ.0) RETURN
                      CALL OPLOAD
                      IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                      DO J=1,OPCNT
                          DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                    (DINMUL*VARABL(I,8))
                          DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                      END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                      ALENS(1,INT(VARABL(I,3)))=VARABL(I,4)
C                       CURVATURE DONE
                  END IF
                  IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
C     SURFACE TORIC CURVATURE
C     NEW VALUE IS:
                      V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                      ALENS(24,INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                      F6=1
                      F1=0
                      F22=0
                      LNSTYP=2
                      CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                      OPCALC_TYPE=3
                      CALL OPCALC
                      IF(F28.EQ.0) RETURN
                      CALL OPLOAD
                      IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                      DO J=1,OPCNT
                          DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                    (DINMUL*VARABL(I,8))
                          DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                      END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                      ALENS(24,INT(VARABL(I,3)))=VARABL(I,4)
C                       TORIC CURVATURE DONE
                  END IF
                  IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
                      IF(VTYPE.EQ.3) ALTYPE=3
                      IF(VTYPE.EQ.4) ALTYPE=2
                      IF(VTYPE.EQ.5) ALTYPE=4
                      IF(VTYPE.EQ.6) ALTYPE=5
                      IF(VTYPE.EQ.7) ALTYPE=6
                      IF(VTYPE.EQ.8) ALTYPE=7
C     NEW VALUE IS:
                      V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                      F6=1
                      F1=0
                      F22=0
                      LNSTYP=2
                      CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                      OPCALC_TYPE=3
                      CALL OPCALC
                      IF(F28.EQ.0) RETURN
                      CALL OPLOAD
                      IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                      DO J=1,OPCNT
                          DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                    (DINMUL*VARABL(I,8))
                          DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                      END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                      ALENS(ALTYPE,INT(VARABL(I,3)))=VARABL(I,4)
C                       THESE VARIABLES DONE
                  END IF
                  IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE.
     1            124.AND.VTYPE.LE.149) THEN
                      IF(VTYPE.EQ.11) ALTYPE=41
                      IF(VTYPE.EQ.12) ALTYPE=37
                      IF(VTYPE.EQ.13) ALTYPE=38
                      IF(VTYPE.EQ.14) ALTYPE=39
                      IF(VTYPE.EQ.15) ALTYPE=40
                      IF(VTYPE.EQ.16) ALTYPE=118
                      IF(VTYPE.EQ.17) ALTYPE=119
                      IF(VTYPE.EQ.18) ALTYPE=120
                      IF(VTYPE.EQ.19) ALTYPE=114
                      IF(VTYPE.EQ.20) ALTYPE=115
                      IF(VTYPE.EQ.21) ALTYPE=46
                      IF(VTYPE.EQ.22) ALTYPE=47
                      IF(VTYPE.EQ.23) ALTYPE=48
                      IF(VTYPE.EQ.24) ALTYPE=49
                      IF(VTYPE.EQ.25) ALTYPE=50
                      IF(VTYPE.EQ.75) ALTYPE=43
                      IF(VTYPE.EQ.124) ALTYPE=71
                      IF(VTYPE.EQ.125) ALTYPE=72
                      IF(VTYPE.EQ.126) ALTYPE=73
                      IF(VTYPE.EQ.127) ALTYPE=74
                      IF(VTYPE.EQ.128) ALTYPE=75
                      IF(VTYPE.EQ.129) ALTYPE=81
                      IF(VTYPE.EQ.130) ALTYPE=82
                      IF(VTYPE.EQ.131) ALTYPE=83
                      IF(VTYPE.EQ.132) ALTYPE=84
                      IF(VTYPE.EQ.133) ALTYPE=85
                      IF(VTYPE.EQ.134) ALTYPE=116
                      IF(VTYPE.EQ.135) ALTYPE=86
                      IF(VTYPE.EQ.136) ALTYPE=87
                      IF(VTYPE.EQ.137) ALTYPE=78
                      IF(VTYPE.EQ.138) ALTYPE=79
                      IF(VTYPE.EQ.139) ALTYPE=80
                      IF(VTYPE.EQ.140) ALTYPE=89
                      IF(VTYPE.EQ.141) ALTYPE=11
                      IF(VTYPE.EQ.142) ALTYPE=10
                      IF(VTYPE.EQ.143) ALTYPE=90
                      IF(VTYPE.EQ.144) ALTYPE=91
                      IF(VTYPE.EQ.145) ALTYPE=92
                      IF(VTYPE.EQ.146) ALTYPE=93
                      IF(VTYPE.EQ.147) ALTYPE=94
                      IF(VTYPE.EQ.148) ALTYPE=95
                      IF(VTYPE.EQ.149) ALTYPE=98
C     NEW VALUE IS:
                      V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                      F6=1
                      F1=0
                      F22=0
                      LNSTYP=2
                      CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                      OPCALC_TYPE=3
                      CALL OPCALC
                      IF(F28.EQ.0) RETURN
                      CALL OPLOAD
                      IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                      DO J=1,OPCNT
                          DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                    (DINMUL*VARABL(I,8))
                          DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                      END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                      ALENS(ALTYPE,INT(VARABL(I,3)))=VARABL(I,4)
                  END IF
                  IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
                      ALTYPE=VTYPE-249
C     NEW VALUE IS:
                      V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
C     SET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                      ISURF=INT(VARABL(I,3))
                      DEFGR1=ALENS(103,ISURF)
                      DEFGR2=ALENS(104,ISURF)
                      DEFGR3=ALENS(105,ISURF)
                      DEFGR4=ALENS(106,ISURF)
                      DEFGR5=ALENS(107,ISURF)
                      DEFGR6=0.0D0
                      DEFGR7=ALENS(109,ISURF)
                      DEFGR8=0.0D0
                      ACTNUM=ALTYPE
                      NEWDEFVAL=V1
                      ERR1=.FALSE.
                      ERR2=.FALSE.
                      CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
C     UPDATE THE LENS
                      F6=1
                      F1=0
                      F22=0
                      LNSTYP=2
                      CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                      OPCALC_TYPE=3
                      CALL OPCALC
                      IF(F28.EQ.0) RETURN
                      CALL OPLOAD
                      IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                      DO J=1,OPCNT
                          DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                    (DINMUL*VARABL(I,8))
                          DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                      END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
C     SET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
                      ISURF=INT(VARABL(I,3))
                      DEFGR1=ALENS(103,ISURF)
                      DEFGR2=ALENS(104,ISURF)
                      DEFGR3=ALENS(105,ISURF)
                      DEFGR4=ALENS(106,ISURF)
                      DEFGR5=ALENS(107,ISURF)
                      DEFGR6=0.0D0
                      DEFGR7=ALENS(109,ISURF)
                      DEFGR8=0.0D0
                      ACTNUM=ALTYPE
                      NEWDEFVAL=VARABL(I,4)
                      ERR1=.FALSE.
                      ERR2=.FALSE.
                      CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                  END IF
                  IF(VTYPE.EQ.150) THEN
C     NEW VALUE IS:
                      V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
C     SET THE APPRORIATE REGISTER VALUE
                      ISURF=INT(VARABL(I,3))
                      OREG=GPREG(ISURF)
                      GPREG(ISURF)=V1
C     CALCULATE OPERANDS AND LOAD THEM
                      OPCALC_TYPE=3
                      CALL OPCALC
                      IF(F28.EQ.0) RETURN
                      CALL OPLOAD
                      IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                      DO J=1,OPCNT
                          DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                    (DINMUL*VARABL(I,8))
                          DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                      END DO
C     RESTORE THE REGISTER VALUE
                      ISURF=INT(VARABL(I,3))
                      GPREG(ISURF)=OREG
C     LOOP TO NEXT DERIVATIVE
C     DERIVATIVES DONE FOR VARIABLE I
                  END IF
                  IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                      FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                      F6=1
                      F1=0
                      F22=0
                      LNSTYP=2
                      CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                      OPCALC_TYPE=3
                      CALL OPCALC
                      IF(F28.EQ.0) RETURN
                      CALL OPLOAD
                      IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                      DO J=1,OPCNT
                          DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                    (DINMUL*VARABL(I,8))
                          DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                      END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                      FTFL01((VTYPE-26),INT(VARABL(I,3)))=VARABL(I,4)
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
                  IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
C     SPECIAL SURFACE COEFFICIENTS
C     NEW VALUE IS:
                      V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                      FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
C     UPDATE THE LENS
                      F6=1
                      F1=0
                      F22=0
                      LNSTYP=2
                      CALL LNSEOS
C     CALCULATE OPERANDS AND LOAD THEM
                      OPCALC_TYPE=3
                      CALL OPCALC
                      IF(F28.EQ.0) RETURN
                      CALL OPLOAD
                      IF(F28.EQ.0) RETURN
C     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
                      DO J=1,OPCNT
                          DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                    (DINMUL*VARABL(I,8))
                          DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                      END DO
C     DERIVATIVES DONE FOR VARIABLE I
C     RESTORE THE LENS
                      FTFL01((VTYPE-27),INT(VARABL(I,3)))=VARABL(I,4)
C                       SPECIAL SURFACE COEFICIENTS DONE
                  END IF
              ELSE
C     CONFIGS VARIABLE
C     VCFG IS THE CONFIG NUMBER
                  VCFG=INT(VARABL(I,2))
C     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
                  VTYPE=INT(VARABL(I,1))
C
C     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
C     CONFIG, DO AN EOS AND EVALUATE ALL OPERANDS
C
C     THE OLD VARIABLE VALUE IS:
                  OLDV1=VARABL(I,4)
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
C     OLDVA1 IS THE VALUE WE WILL USE IN THE RESTORATION PROCESS
C     AFTER THE DERIVATIVE IS CALCULATED
                  VADD=INT(VARABL(I,14))
                  IF(CFADD(VADD,1).EQ.1.AND.OLDV1.NE.0.0D0) OLDV1=1.0D0/OLDV1
                  IF(CFADD(VADD,1).EQ.9.AND.OLDV1.NE.0.0D0) OLDV1=1.0D0/OLDV1
                  N1=OLDV1
                  CALL AUXNTA
                  OLDAV1=AN1
C     THE NEW VARAIBLE VALUE IS JUST
                  V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
                  IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
                  IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
C     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                  N1=V1
                  CALL AUXNTA
                  AV1=AN1
C
C     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
C     VARIABLE IS FOUND IS:
                  VADD=INT(VARABL(I,14))
C
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CFVAL(VADD,2)=V1
                      CFCHAR(VADD,2)=AV1
                  ELSE
                      CFVAL(VADD,1)=V1
                      CFCHAR(VADD,1)=AV1
                  END IF
C     NOW UPDATE THE CONFIG ARRAY
C     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
C     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
C     STUFF IT THERE.
C     NOW UPDATE THE CONFIG ARRAY
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                =AV1(1:23)
                  ELSE
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                =AV1(1:23)
                  END IF
C
C     NOW EVALUATE THE OPERANDS
                  OPCALC_TYPE=3
                  CALL OPCALC
                  IF(F28.EQ.0) RETURN
                  CALL OPLOAD
                  IF(F28.EQ.0) RETURN
                  DO J=1,OPCNT
C     CALC THE DERIVATIVE FOR OPERAND J AND STORE IT
                      DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
     1                (DINMUL*VARABL(I,8))
                      DEREXT=.TRUE.
C     LOOP TO NEXT DERIVATIVE
                  END DO
C     NOW IN THE SAME PLACE WHERE AV1 WAS STUFFED, STUFF OLDAV1
                  VADD=INT(VARABL(I,14))
                  IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR.
     1            CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123
     1            .OR.CFADD(VADD,1).EQ.141) THEN
                      CFVAL(VADD,2)=OLDV1
                      CFCHAR(VADD,2)=OLDAV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7))
     1                =OLDAV1(1:23)
                  ELSE
                      CFVAL(VADD,1)=OLDV1
                      CFCHAR(VADD,1)=OLDAV1
C     NOW UPDATE THE CONFIG ARRAY
                      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5))
     1                =OLDAV1(1:23)
                  END IF

C     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
C     FINISHED WITH A CONFIG VARIABLE
              END IF
C     LOOP TO NEXT VARIABL
          END DO
C     WE JUST DID THE LAST CALCULATION AND WE WANT TO
C     RESTORE THE ORIGINAL OPERANDS
          OPERND(1:OPCNT,1:20)=OLDOP(1:OPCNT,1:20)
C     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
          F6=1
          F1=0
          F22=0
          LNSTYP=2
          CALL LNSEOS

C     THE DERIVATIVE MATRIX IS FORMED
          RETURN
      END
