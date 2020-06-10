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

C       SEVENTH SET OF UTILTIY ROUTINES GO HERE

C SUB LWRITE.FOR
      SUBROUTINE LWRITE

          USE posix_regex
        
          IMPLICIT NONE

C     THIS SUBROUTINE IS CALLED TO WRITE OUT A LINE OF 5
C     GENERAL PURPOSE NUMERIC STORAGE REGISTERS
C     USING THE FORMAT SPECIFIED BY THE LFORMAT COMMAND

          CHARACTER CSTRING*132,LFORM1*141,LLAB*80,WSSA*150
          INTEGER LL,I,J,K,NVNVAL,NVNVALL,II
          REAL*8 V(1:5)

          INCLUDE 'datmai.inc'

          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,*) 'NO ADDITIONAL INFORMATION AVAILABLE'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*) WC,' TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.0) THEN
              WRITE(OUTLYNE,*) WC,' REQUIRES EXPLICIT NUMERIC INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C     QUALIFIER CHECK
          IF(SQ.EQ.1) THEN
   
C     CHECK FOR VALID QUALIFIER WORDS FOR "LWRITE"
              IF (.NOT. (WQ .REM. "\bA([1-9]|[1-9][0-9]|[123][0-9][0-9]|400)\b")) THEN
                 WRITE(OUTLYNE,*)
     1           'INVALID QUALIFIER WORD USED WITH "LWRITE"'
                 CALL SHOWIT(1)
                 WRITE(OUTLYNE,*)
     1           'RE-ENTER COMMAND'
                 CALL SHOWIT(1)
                 CALL MACFAL
              END IF

              READ ( WQ(2:LEN_TRIM(WQ)),*) I
              
C     CHECK CONTENTS OF ALPHA REGISTER
              DO J=80,1,-1
                  IF(AGPREG(I)(J:J).NE.' ') THEN
                      NVNVAL=J
                      GO TO 21
                  END IF
              END DO
 21           CONTINUE
              IF(NVNVAL.EQ.0) SQ=0
              IF(NVNVAL.GT.0.0D0) LLAB(1:NVNVAL)=AGPREG(I)(1:NVNVAL)
          END IF
          DO I=1,5
              IF(I.EQ.1.AND.INT(W1).LT.1.AND.DF1.EQ.0
     1        .OR.I.EQ.1.AND.INT(W1).GT.400..AND.DF1.EQ.0.OR.
     1        I.EQ.2.AND.INT(W2).LT.1.AND.DF2.EQ.0
     1        .OR.I.EQ.1.AND.INT(W1).GT.400.AND.DF2.EQ.0.OR.
     1        I.EQ.3.AND.INT(W3).LT.1.AND.DF3.EQ.0
     1        .OR.I.EQ.3.AND.INT(W1).GT.400.AND.DF2.EQ.0.OR.
     1        I.EQ.4.AND.INT(W4).LT.1.AND.DF4.EQ.0
     1        .OR.I.EQ.4.AND.INT(W1).GT.400.AND.DF2.EQ.0.OR.
     1        I.EQ.5.AND.INT(W5).LT.1.AND.DF5.EQ.0
     1        .OR.I.EQ.5.AND.INT(W1).GT.400.AND.DF2.EQ.0) THEN
                  WRITE(OUTLYNE,*) 'NUMERIC STORAGE REGISTED DOES NOT EXIST'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END DO
C
          J=0
          DO I=1,5
              IF(I.EQ.1.AND.DF1.EQ.0) THEN
                  J=J+1
                  V(J)=GPREG(INT(W1))
              END IF
              IF(I.EQ.2.AND.DF2.EQ.0) THEN
                  J=J+1
                  V(J)=GPREG(INT(W2))
              END IF
              IF(I.EQ.3.AND.DF3.EQ.0) THEN
                  J=J+1
                  V(J)=GPREG(INT(W3))
              END IF
              IF(I.EQ.4.AND.DF4.EQ.0) THEN
                  J=J+1
                  V(J)=GPREG(INT(W4))
              END IF
              IF(I.EQ.5.AND.DF5.EQ.0) THEN
                  J=J+1
                  V(J)=GPREG(INT(W5))
              END IF
          END DO
          DO I=80,1,-1
              IF(LFORM(I:I).NE.' ') THEN
                  LL=I
                  GO TO 96
              END IF
          END DO
 96       CONTINUE
          LFORM1(1:LL+2)='('//LFORM(1:LL)//')'
          IF(SQ.EQ.0) THEN
              WRITE(UNIT=CSTRING,FMT=LFORM1(1:LL+2),ERR=69) (V(K), K=1,J)
              NVNVALL=132
              DO II=132,1,-1
                  IF(CSTRING(II:II).NE.' ') THEN
                      NVNVALL=II
                      GO TO 93
                  END IF
              END DO
 93           CONTINUE
              IF(NVNVALL.LE.79) WRITE(OUTLYNE,1100) CSTRING(1:79)
              IF(NVNVALL.GT.79) WRITE(OUTLYNE,1250) CSTRING(1:131)
              CALL SHOWIT(0)
 1100         FORMAT(A79)
 1250         FORMAT(A131)
 1200         FORMAT(A149)
              RETURN
          END IF
          IF(SQ.EQ.1) THEN
              WRITE(UNIT=CSTRING,FMT=LFORM1(1:LL+2),ERR=69) (V(K), K=1,J)
              NVNVALL=132
              DO II=132,1,-1
                  IF(CSTRING(II:II).NE.' ') THEN
                      NVNVALL=II
                      GO TO 94
                  END IF
              END DO
 94           CONTINUE
C
              WSSA(1:150)=LLAB(1:NVNVAL)//' '//CSTRING(1:NVNVALL)
              NVNVALL=NVNVAL+NVNVALL+1
              IF(NVNVALL.LE.79) WRITE(OUTLYNE,1100) WSSA(1:79)
              IF(NVNVALL.GT.79) WRITE(OUTLYNE,1200) WSSA(1:149)
              CALL SHOWIT(0)
          END IF
          RETURN
   69     CONTINUE
          WRITE(OUTLYNE,*)
     1    'LINE FORMAT DOES NOT AGREE WITH OUTPUT LIST'
          CALL SHOWIT(1)
          WRITE(OUTLYNE,*)
     1    'RE-ISSUE THE "LFORMAT" OR THE "LWRITE" COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
      END


C SUB LFORMER.FOR
      SUBROUTINE LFORMER
C
          IMPLICIT NONE
C
C     THIS DOES THE "LFORMAT" COMMAND
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              WRITE(OUTLYNE,100)
              CALL SHOWIT(0)
 100          FORMAT('THE CURRENT LINE FORMAT IS : ')
              WRITE(OUTLYNE,200) LFORM
              CALL SHOWIT(0)
 200          FORMAT(A79)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        WC,' ONLY TAKES STRING OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1.AND.S1.EQ.1) THEN
              WRITE(OUTLYNE,*) WC,' TAKES STRING OR NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'BUT NOT BOTH'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(S1.EQ.1.AND.INT(W1).LT.1.OR.S1.EQ.1
     1    .AND.INT(W1).GT.400) THEN
              WRITE(OUTLYNE,*) 'ALPHA NUMERIC STORAGE REGISTED DOES NOT EXIST'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              LFORM=' '
              LFORM=WS
              RETURN
          END IF
          IF(S1.EQ.1) THEN
              LFORM=' '
              LFORM=AGPREG(INT(W1))
              RETURN
          END IF
          RETURN
      END


C SUB MYSYS.FOR
          SUBROUTINE MYSYS

          USE opsys
          USE strings  
            
          IMPLICIT NONE
          CHARACTER WWSS*80
C
          INTEGER N,I
C
          INCLUDE 'datmai.inc'
C
          IF(WC(1:1).EQ.'S') THEN
              IF(STI.EQ.1) THEN
                  OUTLYNE=
     1            '"SYS(TEM)" IS USED EXECUTE AN OPERATING SYSTEM COMMAND'
                  CALL SHOWIT(1)
                  RETURN
              ELSE
              END IF
              IF(SN.EQ.1.OR.SQ.EQ.1) THEN
                  OUTLYNE='"SYS(TEM)" ONLY TAKES STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              N=1
              CALL to_lower(WS)
              DO I=80,1,-1
                  IF(WS(I:I).NE.' ') THEN
                      N=I
                      GO TO 10
                  ELSE
                  END IF
              END DO
 10           CONTINUE
              DO I=1,80
                  WWSS(I:I)=' '
              END DO
              WWSS(1:N)=WS(1:N)
              CALL shell_command( WWSS )
          END IF
           
          RETURN
      END


C SUB NTOA2.FOR
      SUBROUTINE NTOA2(N1,N2,N3,N4,N5,BN1,BN2,BN3,BN4,BN5)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 NUMERIC WORDS
C       TO CHARACTER VARIABLES
C
          REAL*8 N1,N2,N3,N4,N5,N
C
          CHARACTER B*140,BN*10,BN1*10,BN2*10,BN3*10,BN4*10,BN5*10
C
          INTEGER I
C
          DO 10 I=1,5
              IF(I.EQ.1) N=N1
              IF(I.EQ.2) N=N2
              IF(I.EQ.3) N=N3
              IF(I.EQ.4) N=N4
              IF(I.EQ.5) N=N5
              IF(DABS(N).GE.10000.0D0.OR.DABS(N).LT.0.0001D0) THEN
                  WRITE(B,100) N
                  READ(B,300) BN
                  IF(I.EQ.1) BN1=BN
                  IF(I.EQ.2) BN2=BN
                  IF(I.EQ.3) BN3=BN
                  IF(I.EQ.4) BN4=BN
                  IF(I.EQ.5) BN5=BN
              ELSE
              END IF
              IF(DABS(N).GE.1000.0D0.AND.DABS(N).LT.10000.0D0.OR.
     1        DABS(N).LE.0.001D0.AND.DABS(N).GT.0.0001D0) THEN
                  WRITE(B,180) N
                  READ(B,300) BN
                  IF(I.EQ.1) BN1=BN
                  IF(I.EQ.2) BN2=BN
                  IF(I.EQ.3) BN3=BN
                  IF(I.EQ.4) BN4=BN
                  IF(I.EQ.5) BN5=BN
              ELSE
              END IF
              IF(DABS(N).GE.100.0D0.AND.DABS(N).LT.1000.0D0.OR.
     1        DABS(N).LE.0.01D0.AND.DABS(N).GT.0.001D0) THEN
                  WRITE(B,170) N
                  READ(B,300) BN
                  IF(I.EQ.1) BN1=BN
                  IF(I.EQ.2) BN2=BN
                  IF(I.EQ.3) BN3=BN
                  IF(I.EQ.4) BN4=BN
                  IF(I.EQ.5) BN5=BN
              ELSE
              END IF
              IF(DABS(N).GE.10.0D0.AND.DABS(N).LT.100.0D0.OR.
     1        DABS(N).LE.0.1D0.AND.DABS(N).GT.0.01D0) THEN
                  WRITE(B,160) N
                  READ(B,300) BN
                  IF(I.EQ.1) BN1=BN
                  IF(I.EQ.2) BN2=BN
                  IF(I.EQ.3) BN3=BN
                  IF(I.EQ.4) BN4=BN
                  IF(I.EQ.5) BN5=BN
              ELSE
              END IF
              IF(DABS(N).GE.1.0D0.AND.DABS(N).LT.10.0D0.OR.
     1        DABS(N).LE.1.0D0.AND.DABS(N).GT.0.1D0) THEN
                  WRITE(B,150) N
                  READ(B,300) BN
                  IF(I.EQ.1) BN1=BN
                  IF(I.EQ.2) BN2=BN
                  IF(I.EQ.3) BN3=BN
                  IF(I.EQ.4) BN4=BN
                  IF(I.EQ.5) BN5=BN
              ELSE
              END IF
 10       CONTINUE
 100      FORMAT(D10.3)
 150      FORMAT(F10.7)
 160      FORMAT(F10.6)
 170      FORMAT(F10.5)
 180      FORMAT(F10.4)
 300      FORMAT(A10)
          RETURN
      END


C SUB NWTOAW.FOR
      SUBROUTINE NWTOAW(N1,N2,N3,N4,N5,AN1,AN2,AN3,AN4,AN5
     1,BN1,BN2,BN3,BN4,BN5)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 NUMERIC WORDS
C       TO CHARACTER VARIABLES
C
          REAL*8 N1,N2,N3,N4,N5,N
C
          CHARACTER B*140,AN1*23,AN2*23,AN3*23,AN4*23,AN5*23,AN*23
     1    ,C*140,BN*11,BN1*11,BN2*11,BN3*11,BN4*11,BN5*11
C
          INTEGER I
          include 'datmai.inc'
C
          DO 10 I=1,5
              IF(I.EQ.1) N=N1
              IF(I.EQ.2) N=N2
              IF(I.EQ.3) N=N3
              IF(I.EQ.4) N=N4
              IF(I.EQ.5) N=N5
              WRITE(B,100) N
              WRITE(C,250) N
              READ(B,200) AN
              READ(C,300) BN
              IF(N.EQ.0.0D0) AN='                    0.0'
              IF(N.EQ.0.0D0) BN='        0.0'
              IF(I.EQ.1) AN1=AN
              IF(I.EQ.2) AN2=AN
              IF(I.EQ.3) AN3=AN
              IF(I.EQ.4) AN4=AN
              IF(I.EQ.5) AN5=AN
              IF(I.EQ.1) BN1=BN
              IF(I.EQ.2) BN2=BN
              IF(I.EQ.3) BN3=BN
              IF(I.EQ.4) BN4=BN
              IF(I.EQ.5) BN5=BN
 10       CONTINUE
 100      FORMAT(G23.15)
 200      FORMAT(A23)
 250      FORMAT(G11.4)
 300      FORMAT(A11)
          RETURN
      END


C SUB NWTOAWB.FOR
      SUBROUTINE NWTOAWB(N1,N2,N3,N4,N5,AN1,AN2,AN3,AN4,AN5
     1,BN1,BN2,BN3,BN4,BN5)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 NUMERIC WORDS
C       TO CHARACTER VARIABLES
C
          REAL*8 N1,N2,N3,N4,N5,N
C
          CHARACTER B*140,AN1*23,AN2*23,AN3*23,AN4*23,AN5*23,AN*23
     1    ,C*140,BN*11,BN1*11,BN2*11,BN3*11,BN4*11,BN5*11
C
          INTEGER I
          INCLUDE 'datmai.inc'
C
          DO 10 I=1,5
              IF(I.EQ.1) N=N1
              IF(I.EQ.2) N=N2
              IF(I.EQ.3) N=N3
              IF(I.EQ.4) N=N4
              IF(I.EQ.5) N=N5
              WRITE(B,100) N
              WRITE(C,250) N
              READ(B,200) AN
              READ(C,300) BN
              IF(I.EQ.1) AN1=AN
              IF(I.EQ.2) AN2=AN
              IF(I.EQ.3) AN3=AN
              IF(I.EQ.4) AN4=AN
              IF(I.EQ.5) AN5=AN
              IF(I.EQ.1) BN1=BN
              IF(I.EQ.2) BN2=BN
              IF(I.EQ.3) BN3=BN
              IF(I.EQ.4) BN4=BN
              IF(I.EQ.5) BN5=BN
 10       CONTINUE
 100      FORMAT(D23.15)
 200      FORMAT(A23)
 250      FORMAT(D11.4)
 300      FORMAT(A11)
          RETURN
      END


C SUB ALINE1.FOR
      SUBROUTINE ALINE1(I,LINE1,ALN1)
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE USES WRITE AND READS TO AN INTERNAL
C       FILE TO CONVERT REAL*8 NUMERIC WORDS
C       TO CHARACTER VARIABLES
C
          CHARACTER A*3,AI*3,LINE1*80,ALN1*80
C
          INTEGER I
C
          WRITE(A,100) I
          READ(A,300) AI
          ALN1=AI//LINE1(1:77)
 100      FORMAT(I3)
 300      FORMAT(A3)
          RETURN
      END
C SUB MESCOM.FOR
      SUBROUTINE MESCOM
C
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
C
          IF(WC.EQ.'M') THEN
C
              IF(SQ.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"M" TAKES ONLY STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       'M'- CAUSES THE MESSAGE IN THE STRING TO BE PRINTED TO THE
C       DEFAULT OUTPUT DEVICE. REMEMBER THE STRING BEGINS WITH A COLON.
C
C       EXAMPLE:
C
C       M,:THE NEXT INSTRUCTION CALCULATES POINT SPREAD FUNCTION.
C
C       M ,1 PRINTS MESSAGE STORED IN THE AGPREG(1) REGISTER
C
C       PRODUCES AT THE CURRENT OUTPUT DEVICE:
C
C               THE NEXT INSTRUCTION CALCULATES POINT SPREAD FUNCTION.
              WRITE(OUTLYNE,1000) WS
              CALL SHOWIT(0)
 1000         FORMAT(A79)
              RETURN
          ELSE
C       NOT MESAGE
          END IF

          IF(WC.EQ.'C') THEN
              IF(SQ.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE='"C" TAKES ONLY STRING INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
C       BLANK INPUT EQUATES TO :(BLANK)
C
C       'C'- THE COMMENT DOESN'T DO ANYTHING. IT JUST ALLOWS
C       ANNOTATION OF INPUT  OR ANNOTATION
C       OF MACROS. IT WORKS LIKE THE COMMENT LINE IN FORTRAN.
C
              RETURN
          ELSE
C       NOT COMENT
          END IF
      END

      
C SUB FIGURE.FOR
      SUBROUTINE FIGURE
C
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
C
          IF(STI.EQ.1) THEN
              OUTLYNE='THE CURRENT FIGURE HEADING IS:'
              CALL SHOWIT(1)
              OUTLYNE=FIGTITLE
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"FIGURE" TAKES ONLY STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          FIGTITLE=WS
          RETURN
      END

      
C SUB VECTOROP.FOR
      SUBROUTINE VECTOROP
C
          IMPLICIT NONE
C
          DOUBLE PRECISION AX,AY,AZ,BX,BY,BZ,DP,PX,PY,PZ
C
          INCLUDE 'datmai.inc'

C
          IF(STI.EQ.1.OR.DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1) THEN
              IF(WC.EQ.'AVEC') THEN
                  OUTLYNE='CURRENT AVEC IS:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'X=',AXVEC
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'Y=',AYVEC
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'Z=',AZVEC
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(WC.EQ.'BVEC') THEN
                  OUTLYNE='CURRENT BVEC IS:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'X=',BXVEC
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'Y=',BYVEC
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'Z=',BZVEC
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(STI.EQ.1) THEN
              IF(WC.EQ.'DOT') THEN
                  OUTLYNE='DOT RETURNS THE DOT PRODUCT OF AVEC AND BVEC'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(WC.EQ.'CROSS') THEN
                  OUTLYNE='DOT RETURNS THE CROSS PRODUCT OF AVEC AND BVEC'
                  CALL SHOWIT(1)
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'DOT') THEN
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*) '"DOT" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'CROSS') THEN
              IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
                  WRITE(OUTLYNE,*) '"CROSS" TAKES NO ADDITIONAL INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'AVEC') THEN
              IF(S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"AVEC" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              AXVEC=W1
              AYVEC=W2
              AZVEC=W3
              IF(DF1.EQ.1) AXVEC=0.0D0
              IF(DF2.EQ.1) AYVEC=0.0D0
              IF(DF3.EQ.1) AZVEC=0.0D0
          END IF
          IF(WC.EQ.'BVEC') THEN
              IF(S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
                  WRITE(OUTLYNE,*)
     1            '"BVEC" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              BXVEC=W1
              BYVEC=W2
              BZVEC=W3
              IF(DF1.EQ.1) BXVEC=0.0D0
              IF(DF2.EQ.1) BYVEC=0.0D0
              IF(DF3.EQ.1) BZVEC=0.0D0
          END IF
          AX=AXVEC
          AY=AYVEC
          AZ=AZVEC
          BX=BXVEC
          BY=BYVEC
          BZ=BZVEC
          IF(WC.EQ.'DOT') THEN
              CALL DOT_PRODUCT(DP,AX,AY,AZ,BX,BY,BZ)
              REG(40)=REG(9)
              REG(9)=DP
              WRITE(OUTLYNE,*) 'DOT PRODUCT = ',DP
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(WC.EQ.'CROSS') THEN
              CALL CROSS_PRODUCT(PX,PY,PZ,AX,AY,AZ,BX,BY,BZ)
              REG(40)=REG(9)
              REG(11)=PX
              REG(10)=PY
              REG(9)=PZ
              WRITE(OUTLYNE,*) 'CROSS PRODUCT (X-COMPONENT) = ',PX
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'CROSS PRODUCT (Y-COMPONENT) = ',PY
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*) 'CROSS PRODUCT (Z-COMPONENT) = ',PZ
              CALL SHOWIT(1)
              RETURN
          END IF
          RETURN
      END


C SUB LINE_TO_PLANE
      SUBROUTINE LINE_TO_PLANE
     1(X,Y,Z,X0,Y0,Z0,L,M,N,XP,YP,ZP,LP,MP,NP,ERROR)
C
C       RETURNS THE X,Y,Z LOCATION WHEN VECTOR X,Y,Z,L,M,N (STARTING
C       AT X0,Y0,Z0 INTERSECTS THE PLANE WHOSE NORMAL VECTOR IS IN THE
C       LP,MP,NP DIRECTION, THE NORMAL PASSING THROUGH POINT XP,YP,ZP
C       ERROR SET TO TRUE IS NO INTERSECTION IS POSSIBLE
C
          IMPLICIT NONE
C
          REAL*8 X,Y,Z,L,M,N,X0,Y0,Z0
C
          REAL*8 XP,YP,ZP,LP,MP,NP,NU,NOM,DENOM
C
          LOGICAL ERROR
C
          ERROR=.FALSE.
C
          NOM=((X0-XP)*LP)+((Y0-YP)*MP)+((Z0-ZP)*NP)
          DENOM=(L*LP)+(M*MP)+(N*NP)
          IF(DENOM.EQ.0.0D0) THEN
              ERROR=.TRUE.
              RETURN
          ELSE
              NU=NOM/DENOM
              X=X0-(NU*L)
              Y=Y0-(NU*M)
              Z=Z0-(NU*N)
          END IF
          RETURN
      END
