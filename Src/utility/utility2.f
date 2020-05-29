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

C       SECOND SET OF UTILTIY ROUTINES GO HERE

C SUB SETHED.FOR
      SUBROUTINE SETHED
C
          IMPLICIT NONE
C
C       THIS ROUTINE SETS, CLEARS AND INTERROGATES THE HEADINGS
C       FLAG FOR SINGLE LINE PRINTOUT SUCH AS IN RTG,3
C
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              outlyne=
     1        '"HEADINGS"'
              CALL SHOWIT(1)
              outlyne=
     1        'TAKES NO STRING OR NUMERIC WORD INPUT'
              CALL SHOWIT(1)
              outlyne='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          ELSE
          END IF
          IF(SQ.EQ.1) THEN
              IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
                  outlyne='INVALID QUALIFIER WORD'
                  CALL SHOWIT(1)
                  outlyne='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
C       QUALIFIERS VALID PROCEED
              END IF
C       DO QUALIFIERS HERE
C
              IF(WQ.EQ.'ON') THEN
                  HEADIN=.TRUE.
              ELSE
              END IF
C
              IF(WQ.EQ.'OFF') THEN
                  HEADIN=.FALSE.
              ELSE
              END IF
C
              RETURN
          ELSE
C       NO QUALIFIERS PROCEED
          END IF
          IF( HEADIN) THEN
C       HEADINGS IS ON
              outlyne='"HEADINGS" IS CURRENTLY SET TO "ON"'
              CALL SHOWIT(1)
              RETURN
          ELSE
C       HEADINGS IS OFF
              outlyne='"HEADINGS" IS CURRENTLY SET TO "OFF"'
              CALL SHOWIT(1)
          END IF
          RETURN
      END
C SUB SET.FOR
      SUBROUTINE SET
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS USED TO SET A NAMED REGISTER (A THROUGH H)
C       THE ACCUMULATOR (NAMED 'BLANK' OR 'ACC' OR 'X')
C       OR THE 'Y','Z','T','IX','IY','IZ',AND 'IT' REGISTERS, AND
C       THE INDEXING REGISTERS I,ITEST,J AND JTEST.
C       THE INDEXING REGISTERS K,KTEST,L AND LTEST.
C       THE INDEXING REGISTERS M,MTEST,N AND NTEST.
C       TO  SPECIFIC NUMERIC VALUE1S. IF WQ IS BLANK,
C       THE ACCUMULATOR 'X' IS ASSUMED. IF W1 IS BLANK, ZERO IS STORED.
C       REGISTER MEMORY IS PASSED IS MEMORY COMMON.
C
C       WQ IS USED TO CALL THE REGISTER BY NAME.
C
          CHARACTER ACCWRD*8
C
          REAL*8 SVAL
C
          LOGICAL GSTRING
C
          INTEGER ACCSUB,ACCCNT
C
          COMMON/ACCSB/ACCWRD
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          INCLUDE 'datmai.inc'
C
          IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"SET" ONLY TAKES QUALIFIER, STRING AND NUMERIC WORD #1 INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.1) THEN
              S1=0
              SN=0
              W1=0.0D0
              DF1=1
          END IF
          IF(SST.EQ.0) THEN
              SVAL=W1
          END IF
          IF(SST.EQ.1) THEN
              GSTRING=.FALSE.
              IF(WS(1:8).EQ.'A       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'B       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'C       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'D       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'E       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'F       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'G       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'H       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'I       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'J       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'K       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'L       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'M       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'N       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'X       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'ACC     ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'Y       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'Z       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'T       ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'IX      ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'IY      ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'IZ      ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'IT      ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'ITEST   ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'JTEST   ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'KTEST   ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'LTEST   ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'MTEST   ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'NTEST   ') GSTRING=.TRUE.
              IF(WS(1:8).EQ.'A       ') SVAL=REG(1)
              IF(WS(1:8).EQ.'B       ') SVAL=REG(2)
              IF(WS(1:8).EQ.'C       ') SVAL=REG(3)
              IF(WS(1:8).EQ.'D       ') SVAL=REG(4)
              IF(WS(1:8).EQ.'E       ') SVAL=REG(5)
              IF(WS(1:8).EQ.'F       ') SVAL=REG(6)
              IF(WS(1:8).EQ.'G       ') SVAL=REG(7)
              IF(WS(1:8).EQ.'H       ') SVAL=REG(8)
              IF(WS(1:8).EQ.'I       ') SVAL=REG(17)
              IF(WS(1:8).EQ.'J       ') SVAL=REG(19)
              IF(WS(1:8).EQ.'K       ') SVAL=REG(21)
              IF(WS(1:8).EQ.'L       ') SVAL=REG(22)
              IF(WS(1:8).EQ.'M       ') SVAL=REG(23)
              IF(WS(1:8).EQ.'N       ') SVAL=REG(24)
              IF(WS(1:8).EQ.'X       ') SVAL=REG(9)
              IF(WS(1:8).EQ.'ACC     ') SVAL=REG(9)
              IF(WS(1:8).EQ.'Y       ') SVAL=REG(10)
              IF(WS(1:8).EQ.'Z       ') SVAL=REG(11)
              IF(WS(1:8).EQ.'T       ') SVAL=REG(12)
              IF(WS(1:8).EQ.'IX      ') SVAL=REG(13)
              IF(WS(1:8).EQ.'IY      ') SVAL=REG(14)
              IF(WS(1:8).EQ.'IZ      ') SVAL=REG(15)
              IF(WS(1:8).EQ.'IT      ') SVAL=REG(16)
              IF(WS(1:8).EQ.'ITEST   ') SVAL=REG(18)
              IF(WS(1:8).EQ.'JTEST   ') SVAL=REG(20)
              IF(WS(1:8).EQ.'KTEST   ') SVAL=REG(25)
              IF(WS(1:8).EQ.'LTEST   ') SVAL=REG(26)
              IF(WS(1:8).EQ.'MTEST   ') SVAL=REG(27)
              IF(WS(1:8).EQ.'NTEST   ') SVAL=REG(28)
          END IF
          IF(SST.EQ.1.AND..NOT.GSTRING) THEN
              WRITE(OUTLYNE,*)
     1        'INVALID SOURCE REGISTER NAME ENTERED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          W1=SVAL
          IF(DF1.EQ.1.AND..NOT.GSTRING) W1=0.0D0
          IF(ACCSUB.EQ.1) THEN
              IF(WQ.EQ.'ACC'.OR.WQ.EQ.'X'.OR.WQ.EQ.' ') THEN
                  WQ=ACCWRD
                  ACCCNT=ACCCNT-1
                  IF(ACCCNT.EQ.0) ACCSUB=0
              END IF
          END IF
          IF(WQ.EQ.'A') THEN
              REG(1)=W1
          END IF
          IF(WQ.EQ.'B') THEN
              REG(2)=W1
          END IF
          IF(WQ.EQ.'C') THEN
              REG(3)=W1
          END IF
          IF(WQ.EQ.'D') THEN
              REG(4)=W1
          END IF
          IF(WQ.EQ.'E') THEN
              REG(5)=W1
          END IF
          IF(WQ.EQ.'F') THEN
              REG(6)=W1
          END IF
          IF(WQ.EQ.'G') THEN
              REG(7)=W1
          END IF
          IF(WQ.EQ.'H') THEN
              REG(8)=W1
          END IF
          IF(WQ.EQ.'ACC'.OR.WQ.EQ.'        '.OR.WQ.EQ.'X') THEN
              REG(40)=REG(9)
              REG(9)=W1
          END IF
          IF(WQ.EQ.'Y') THEN
              REG(10)=W1
          END IF
          IF(WQ.EQ.'Z') THEN
              REG(11)=W1
          END IF
          IF(WQ.EQ.'T') THEN
              REG(12)=W1
          END IF
          IF(WQ.EQ.'IX') THEN
              REG(30)=REG(13)
              REG(13)=W1
          END IF
          IF(WQ.EQ.'IY') THEN
              REG(14)=W1
          END IF
          IF(WQ.EQ.'IZ') THEN
              REG(15)=W1
          END IF
          IF(WQ.EQ.'IT') THEN
              REG(16)=W1
          END IF
          IF(WQ.EQ.'I') THEN
              REG(17)=W1
          END IF
          IF(WQ.EQ.'ITEST') THEN
              REG(18)=W1
          END IF
          IF(WQ.EQ.'J') THEN
              REG(19)=W1
          END IF
          IF(WQ.EQ.'JTEST') THEN
              REG(20)=W1
          END IF
          IF(WQ.EQ.'K') THEN
              REG(21)=W1
          END IF
          IF(WQ.EQ.'L') THEN
              REG(22)=W1
          END IF
          IF(WQ.EQ.'M') THEN
              REG(23)=W1
          END IF
          IF(WQ.EQ.'N') THEN
              REG(24)=W1
          END IF
          IF(WQ.EQ.'KTEST') THEN
              REG(25)=W1
          END IF
          IF(WQ.EQ.'LTEST') THEN
              REG(26)=W1
          END IF
          IF(WQ.EQ.'MTEST') THEN
              REG(27)=W1
          END IF
          IF(WQ.EQ.'NTEST') THEN
              REG(28)=W1
          END IF
          IF(WQ.NE.'A'.AND.WQ.NE.'B'.AND.WQ.NE.'C'
     1    .AND.WQ.NE.'D'.AND.WQ.NE.'E'.AND.WQ.NE.'F'
     2    .AND.WQ.NE.'G'.AND.WQ.NE.'H'.AND.WQ.NE.'I'
     3    .AND.WQ.NE.'J'.AND.WQ.NE.'ITEST'.AND.WQ.NE.'JTEST'
     4    .AND.WQ.NE.'X'.AND.WQ.NE.'Y'.AND.WQ.NE.'Z'
     5    .AND.WQ.NE.'T'.AND.WQ.NE.'IX'.AND.WQ.NE.'IY'
     6    .AND.WQ.NE.'IZ'.AND.WQ.NE.'IT'
     7    .AND.WQ.NE.'ACC'.AND.
     8    WQ.NE.'        '.AND.WQ.NE.'K'.AND.WQ.NE.'L'.AND.WQ.NE.'M'
     9    .AND.WQ.NE.'N'.AND.WQ.NE.'KTEST'.AND.WQ.NE.'LTEST'.AND.WQ.NE.
     1    'MTEST'.AND.WQ.NE.'NTEST') THEN
              WRITE(OUTLYNE,*)'INVALID TARGET REGISTER NAME ENTERED'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
          ELSE
          END IF
          RETURN
      END


C SUB SAGFLT.FOR
      SUBROUTINE SAGFLT(I,X,Y,SAG)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SAGFLT.FOR. THIS SUBROUTINE IMPLEMENTS
C       FLAT ASPHERIC SURFACE SAG CALCULATIONS.
C       SAG OF SPECIAL SURFACES IS DONE BY A CALL TO SAGSPC.FOR
C
          INTEGER I
C
          REAL*8 Z,X,Y,SAG,XYMAX,XYMIN
          real*8 SAG1,SAG2,SAG3,SAG4,SAG5,SAG6,SAG7,SAG8,SAG9,SAG10
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       SURFACE I IS PLANO AND MAY CONTAIN 2ND, 4TH, 6TH, 8TH AND 10TH ORDER
C       ASPHERIC TERMS AND SPECIAL SURFACE SHAPES.
C
          XYMAX=1.0D+300
          XYMIN=-1.0D+300

          IF(ALENS(133,I).NE.0.0D0) CALL SAGARRAY(I,X,Y)

          IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.1) THEN

              SAG1=ALENS(43,I)*((X**2)+(Y**2))
              if ((X**2)+(Y**2).GT.XYMAX) then
                  SAG1=ALENS(43,I)*XYMAX
              end if
              if ((X**2)+(Y**2).LT.XYMIN) then
                  SAG1=ALENS(43,I)*XYMIN
              end if

              SAG2=ALENS(4,I)*((X**2)+(Y**2))**2
              if (((X**2)+(Y**2))**2.GT.XYMAX) then
                  SAG2=ALENS(4,I)*XYMAX
              end if
              if (((X**2)+(Y**2))**2.LT.XYMIN) then
                  SAG2=ALENS(4,I)*XYMIN
              end if

              SAG3=ALENS(5,I)*((X**2)+(Y**2))**3
              if (((X**2)+(Y**2))**3.GT.XYMAX) then
                  SAG3=ALENS(5,I)*XYMAX
              end if
              if (((X**2)+(Y**2))**3.LT.XYMIN) then
                  SAG3=ALENS(5,I)*XYMIN
              end if

              SAG4=ALENS(6,I)*((X**2)+(Y**2))**4
              if (((X**2)+(Y**2))**4.GT.XYMAX) then
                  SAG4=ALENS(6,I)*XYMAX
              end if
              if (((X**2)+(Y**2))**4.LT.XYMIN) then
                  SAG4=ALENS(6,I)*XYMIN
              end if

              SAG5=ALENS(7,I)*((X**2)+(Y**2))**5
              if (((X**2)+(Y**2))**5.GT.XYMAX) then
                  SAG5=ALENS(7,I)*XYMAX
              end if
              if (((X**2)+(Y**2))**5.LT.XYMIN) then
                  SAG5=ALENS(7,I)*XYMIN
              end if

              SAG6=ALENS(81,I)*((X**2)+(Y**2))**6
              if (((X**2)+(Y**2))**6.GT.XYMAX) then
                  SAG6=ALENS(81,I)*XYMAX
              end if
              if (((X**2)+(Y**2))**6.LT.XYMIN) then
                  SAG6=ALENS(81,I)*XYMIN
              end if

              SAG7=ALENS(82,I)*((X**2)+(Y**2))**7
              if (((X**2)+(Y**2))**7.GT.XYMAX) then
                  SAG7=ALENS(82,I)*XYMAX
              end if
              if (((X**2)+(Y**2))**7.LT.XYMIN) then
                  SAG7=ALENS(82,I)*XYMIN
              end if

              SAG8=ALENS(83,I)*((X**2)+(Y**2))**8
              if (((X**2)+(Y**2))**8.GT.XYMAX) then
                  SAG8=ALENS(83,I)*XYMAX
              end if
              if (((X**2)+(Y**2))**8.LT.XYMIN) then
                  SAG8=ALENS(83,I)*XYMIN
              end if

              SAG9=ALENS(84,I)*((X**2)+(Y**2))**9
              if (((X**2)+(Y**2))**9.GT.XYMAX) then
                  SAG9=ALENS(84,I)*XYMAX
              end if
              if (((X**2)+(Y**2))**9.LT.XYMIN) then
                  SAG9=ALENS(84,I)*XYMIN
              end if

              SAG10=ALENS(85,I)*((X**2)+(Y**2))**10
              if (((X**2)+(Y**2))**10.GT.XYMAX) then
                  SAG10=ALENS(85,I)*XYMAX
              end if
              if (((X**2)+(Y**2))**10.LT.XYMIN) then
                  SAG10=ALENS(85,I)*XYMIN
              end if

              SAG=SAG1+SAG2+SAG3+SAG4+SAG5+SAG6+SAG7+SAG8+SAG9+SAG10

          ELSE
              SAG=0.0D0
          END IF

C       SPECIAL SURFACE ?
          IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.2) THEN
              IF(ALENS(34,I).NE.0.0D0.AND.
     1        ALENS(34,I).NE.6.0D0.AND.ALENS(34,I).NE.7.0D0.AND.
     1        ALENS(34,I).NE.9.0D0.AND.ALENS(34,I).NE.10.0D0.AND.
     1        ALENS(34,I).NE.12.0D0.AND.ALENS(34,I).NE.13.0D0.OR.
     1        ALENS(103,I).EQ.1.0D0) THEN
                  CALL SAGSPC(I,X,Y,Z)
                  IF(ALENS(34,I).EQ.24.0D0.AND.FTFL01(2,I).EQ.-1.0D0) THEN
                      SAG=Z
                  ELSE
                      SAG=SAG+Z
                  END IF
                  IF(ALENS(124,I).EQ.1.0D0) SAG=0.0D0
              END IF
          END IF
          RETURN
      END


C SUB SAGANA.FOR
      SUBROUTINE SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY
     1,SAG,I)
C
          IMPLICIT NONE
C
          REAL*8 CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY
     1    ,SAG,R,R1,R3,R2,R4,Z
C
          INTEGER I
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          IF(ALENS(133,I).NE.0.0D0) CALL SAGARRAY(I,X,Y)
C
          R1=(DY*((((1.0D0-DX)*(X**2))+((1.0D0+DX)*(Y**2)))**2))
          R2=(EY*((((1.0D0-EX)*(X**2))+((1.0D0+EX)*(Y**2)))**3))
          R3=(FY*((((1.0D0-FX)*(X**2))+((1.0D0+FX)*(Y**2)))**4))
          R4=(GY*((((1.0D0-GX)*(X**2))+((1.0D0+GX)*(Y**2)))**5))
          R=1.0D0-((KX+1.0D0)*(CX**2)*(X**2))-((KY+1.0D0)*(CY**2)*(Y**2))
          IF(R.LT.0.0D0) THEN
              SAG=0.0D0
              RETURN
          END IF
          IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.1) THEN
              SAG=((((CX*(X**2))+(CY*(Y**2)))/(1.0D0+DSQRT(R))))
     1        +R1+R2+R3+R4
          ELSE
              SAG=0.0D0
          END IF
          IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.2) THEN
C       SPECIAL SURFACE ?
              IF(ALENS(34,I).NE.0.0D0.AND.
     1        ALENS(34,I).NE.6.0D0.AND.ALENS(34,I).NE.7.0D0.AND.
     1        ALENS(34,I).NE.9.0D0.AND.ALENS(34,I).NE.10.0D0.AND.
     1        ALENS(34,I).NE.12.0D0.AND.ALENS(34,I).NE.13.0D0.OR.
     1        ALENS(103,I).EQ.1.0D0) THEN
                  CALL SAGSPC(I,X,Y,Z)
                  IF(ALENS(34,I).EQ.24.0D0.AND.FTFL01(2,I).EQ.-1.0D0) THEN
                      SAG=Z
                  ELSE
                      SAG=SAG+Z
                  END IF
                  IF(ALENS(124,I).EQ.1.0D0) SAG=0.0D0
              END IF
          END IF
          RETURN
      END
C SUB SAGASP.FOR
      SUBROUTINE SAGASP(I,X,Y,SAG)
C
          IMPLICIT NONE
C
          INTEGER I
C
          REAL*8 X,Y,SAG,C2,RHO2,RHO,Z,R
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          IF(ALENS(133,I).NE.0.0D0) CALL SAGARRAY(I,X,Y)
C
          RHO2=(X**2)+(Y**2)
          RHO=DSQRT(RHO2)
          C2=ALENS(1,I)**2
          R=1.0D0-((ALENS(2,I)+1.0D0)*(C2*RHO2))
          IF(R.LT.0.0D0) THEN
              R=1.0D0-((ALENS(2,I)+1))
              IF(R.LT.0.0D0) R=0.0D0
              SAG=(1.0D0/ALENS(1,I))/(1.0D0+DSQRT(R))
              RETURN
          END IF
          IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.1) THEN
              SAG=(ALENS(1,I)*RHO2)/
     1        (1.0D0+DSQRT(R))
              IF(ALENS(8,I).NE.0.0D0) THEN
                  SAG=SAG+
     2            (ALENS(4,I)*(RHO**4))
     2            +(ALENS(5,I)*(RHO**6))
     2            +(ALENS(6,I)*(RHO**8))
     2            +(ALENS(7,I)*(RHO**10))
     2            +(ALENS(81,I)*(RHO**12))
     2            +(ALENS(82,I)*(RHO**14))
     2            +(ALENS(83,I)*(RHO**16))
     2            +(ALENS(84,I)*(RHO**18))
     2            +(ALENS(85,I)*(RHO**20))
              END IF
          ELSE
              SAG=0.0D0
          END IF
C
C       SPECIAL SURFACE ?
          IF(SAGCODE.EQ.0.OR.SAGCODE.EQ.2) THEN
              IF(ALENS(34,I).NE.0.0D0.AND.
     1        ALENS(34,I).NE.6.0D0.AND.ALENS(34,I).NE.7.0D0.AND.
     1        ALENS(34,I).NE.9.0D0.AND.ALENS(34,I).NE.10.0D0.AND.
     1        ALENS(34,I).NE.12.0D0.AND.ALENS(34,I).NE.13.0D0.OR.
     1        ALENS(103,I).EQ.1.0D0) THEN
                  CALL SAGSPC(I,X,Y,Z)
                  IF(ALENS(34,I).EQ.24.0D0.AND.FTFL01(2,I).EQ.-1.0D0) THEN
                      SAG=Z
                  ELSE
                      SAG=SAG+Z
                  END IF
                  IF(ALENS(124,I).EQ.1.0D0) SAG=0.0D0
              END IF
          END IF
          RETURN
      END
C SUB SAGINT.FOR
      SUBROUTINE SAGINT(I,X,Y,Z,L1,M1,N1)
C       THIS DOES SPECIAL SURFACES NOW
C
          IMPLICIT NONE
C
          REAL*8 C,K,Z,DELTA,ARG
     1    ,ARG1,X,Y,ARG2,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY
     2    ,KX,KY,L1,M1,N1
          REAL*8 X1,X2,Y1,Y2,Z1,Z2,ARGA,ARGB,ARGC,ARGD
C
          INTEGER I
C
          EXTERNAL ARG1,ARG2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          IF(SYSTEM1(6).EQ.1.0D0) DELTA=0.001D0/25.4
          IF(SYSTEM1(6).EQ.2.0D0) DELTA=0.0001D0
          IF(SYSTEM1(6).EQ.3.0D0) DELTA=0.001D0
          IF(SYSTEM1(6).EQ.4.0D0) DELTA=0.000001D0
          X1=X-DELTA
          X2=X+DELTA
          Y1=Y-DELTA
          Y2=Y+DELTA
C
C       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
          IF(ALENS(1,I).EQ.0.0D0
     1    .AND.ALENS(23,I).EQ.0.0D0) THEN
              CALL SAGFLT(I,X,Y,Z)
              CALL SAGFLT(I,X,Y2,Z2)
              CALL SAGFLT(I,X,Y1,Z1)
              M1=-(Z2-Z1)/(2.0D0*DELTA)
              CALL SAGFLT(I,X2,Y,Z2)
              CALL SAGFLT(I,X1,Y,Z1)
              L1=-(Z2-Z1)/(2.0D0*DELTA)
              N1=DSQRT(1.0D0-(M1**2)-(L1**2))
              RETURN
          END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
          IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
              C=ALENS(1,I)
              K=ALENS(2,I)
              ARG= ARG1(C,K,X,Y)
              ARGA= ARG1(C,K,X1,Y)
              ARGB= ARG1(C,K,X2,Y)
              ARGC= ARG1(C,K,X,Y1)
              ARGD= ARG1(C,K,X,Y2)
              IF(ARG.LT.0.0D0.OR.ARGA.LT.0.0D0.OR.ARGB.LT.0.0D0.OR.
     1        ARGC.LT.0.0D0.OR.ARGD.LT.0.0D0) THEN
                  Z=0.0D0
                  L1=0.0D0
                  M1=0.0D0
                  N1=1.0D0
                  Z=0.0D0
                  RETURN
              ELSE
C                       PROCEED
                  CALL SAGASP(I,X,Y,Z)
                  CALL SAGASP(I,X,Y2,Z2)
                  CALL SAGASP(I,X,Y1,Z1)
                  M1=-(Z2-Z1)/(2.0D0*DELTA)
                  CALL SAGASP(I,X2,Y,Z2)
                  CALL SAGASP(I,X1,Y,Z1)
                  L1=-(Z2-Z1)/(2.0D0*DELTA)
                  N1=DSQRT(1.0D0-(M1**2)-(L1**2))
                  RETURN
              END IF
          END IF
          IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
              IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                  CY=ALENS(1,I)
                  KY=ALENS(2,I)
                  DY=ALENS(4,I)
                  EY=ALENS(5,I)
                  FY=ALENS(6,I)
                  GY=ALENS(7,I)
                  CX=ALENS(24,I)
                  KX=ALENS(41,I)
                  DX=ALENS(37,I)
                  EX=ALENS(38,I)
                  FX=ALENS(39,I)
                  GX=ALENS(40,I)
              END IF
              IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                  CX=ALENS(1,I)
                  KX=ALENS(2,I)
                  DX=ALENS(4,I)
                  EX=ALENS(5,I)
                  FX=ALENS(6,I)
                  GX=ALENS(7,I)
                  CY=ALENS(24,I)
                  KY=ALENS(41,I)
                  DY=ALENS(37,I)
                  EY=ALENS(38,I)
                  FY=ALENS(39,I)
                  GY=ALENS(40,I)
              END IF
              ARG=ARG2(CX,CY,KX,KY,X,Y)
              ARGA=ARG2(CX,CY,KX,KY,X1,Y)
              ARGB=ARG2(CX,CY,KX,KY,X2,Y)
              ARGC=ARG2(CX,CY,KX,KY,X,Y1)
              ARGD=ARG2(CX,CY,KX,KY,X,Y2)
              IF(ARG.LT.0.0D0.OR.ARGA.LT.0.0D0.OR.ARGB.LT.0.0D0.OR.
     1        ARGC.LT.0.0D0.OR.ARGD.LT.0.0D0) THEN
                  Z=0.0D0
                  L1=0.0D0
                  M1=0.0D0
                  N1=1.0D0
                  RETURN
              ELSE
C                       PROCEED
                  CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,Z,I)
                  CALL SAGANA(CX,CY,KX,KY,X,Y2,DX,DY,EX,EY,FX,FY,GX,GY,Z2,I)
                  CALL SAGANA(CX,CY,KX,KY,X,Y1,DX,DY,EX,EY,FX,FY,GX,GY,Z1,I)
                  M1=-(Z2-Z1)/(2.0D0*DELTA)
                  CALL SAGANA(CX,CY,KX,KY,X2,Y,DX,DY,EX,EY,FX,FY,GX,GY,Z2,I)
                  CALL SAGANA(CX,CY,KX,KY,X1,Y,DX,DY,EX,EY,FX,FY,GX,GY,Z1,I)
                  L1=-(Z2-Z1)/(2.0D0*DELTA)
                  N1=DSQRT(1.0D0-(M1**2)-(L1**2))
                  RETURN
              END IF
          END IF
      END
C SUB SAGITT.FOR
      SUBROUTINE SAGITT(I,CA,J,SAG,ETERROR)
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SAGITT.
C
          EXTERNAL ARG1,ARG2
C
          LOGICAL ETERROR
C
          REAL*8 C,K,SAG,ARG,CA,KX,KY
     1    ,ARG1,X,Y,ARG2,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY
C
          INTEGER I,J
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          ETERROR=.FALSE.
C
C     I IS THE SURFACE NUMBER
C     CA IS THE HEIGHT FOR THE CALCULATION
C     J=1 FOR YZ PLANE, J=2 FOR XZ PLANE
          IF(J.EQ.1) THEN
              Y=CA
              X=0.0D0
          ELSE
              Y=0.0D0
              X=CA
          END IF
C       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
          IF(ALENS(1,I).EQ.0.0D0
     1    .AND.ALENS(23,I).EQ.0.0D0) THEN
              CALL SAGFLT(I,X,Y,SAG)
              IF(DABS(SAG).LT.1E-15) SAG=0.0D0
              IF(ALENS(124,I).EQ.1.0D0) SAG=0.0D0
              RETURN
          END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
          IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
              C=ALENS(1,I)
              K=ALENS(2,I)
              ARG= ARG1(C,K,X,Y)
              IF(ARG.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WARNING: EDGE THICKNESS CALCULATION ERROR'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'EDGE THICKNESS OPERAND VALUE1 SET TO ZERO'
                  CALL SHOWIT(1)
                  ETERROR=.TRUE.
                  RETURN
              END IF
C                        PROCEED
              CALL SAGASP(I,X,Y,SAG)
              IF(DABS(SAG).LT.1E-15) SAG=0.0D0
              IF(ALENS(124,I).EQ.1.0D0) SAG=0.0D0
              RETURN
          END IF
          IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
              IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                  CY=ALENS(1,I)
                  KY=ALENS(2,I)
                  DY=ALENS(4,I)
                  EY=ALENS(5,I)
                  FY=ALENS(6,I)
                  GY=ALENS(7,I)
                  CX=ALENS(24,I)
                  KX=ALENS(41,I)
                  DX=ALENS(37,I)
                  EX=ALENS(38,I)
                  FX=ALENS(39,I)
                  GX=ALENS(40,I)
              END IF
              IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                  CX=ALENS(1,I)
                  KX=ALENS(2,I)
                  DX=ALENS(4,I)
                  EX=ALENS(5,I)
                  FX=ALENS(6,I)
                  GX=ALENS(7,I)
                  CY=ALENS(24,I)
                  KY=ALENS(41,I)
                  DY=ALENS(37,I)
                  EY=ALENS(38,I)
                  FY=ALENS(39,I)
                  GY=ALENS(40,I)
              END IF
              ARG=ARG2(CX,CY,KX,KY,X,Y)
              IF(ARG.LT.0.0D0) THEN
                  WRITE(OUTLYNE,*)
     1            'WARNING: EDGE THICKNESS CALCULATION ERROR'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'EDGE THICKNESS OPERAND VALUE1 SET TO ZERO'
                  CALL SHOWIT(1)
                  ETERROR=.TRUE.
                  RETURN
              END IF
C                       PROCEED
              CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
              IF(DABS(SAG).LT.1E-15) SAG=0.0D0
              IF(ALENS(124,I).EQ.1.0D0) SAG=0.0D0
              RETURN
          END IF
          IF(ALENS(124,I).EQ.1.0D0) SAG=0.0D0
          RETURN
      END
C SUB SAGPLT.FOR
      SUBROUTINE SAGPLT(I,X,Y,SAG,NO)
C     I IS SURFACE #
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SAGPLT. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE SAG FOR SURFACE PROFILE AND CLAP/COBS PLOTTING.
C
          REAL*8 C,K,SAG,ARG
     1    ,ARG1,X,Y,ARG2,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY
     2    ,KX,KY
C
          INTEGER I,NO
C
          EXTERNAL ARG1,ARG2
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       FLAT SURFACE MAYBE ASPHERICS OR SPECIAL SURFACE STUFF
          IF(ALENS(1,I).EQ.0.0D0
     1    .AND.ALENS(23,I).EQ.0.0D0) THEN
              CALL SAGFLT(I,X,Y,SAG)
              IF(DABS(SAG).LT.1.0D-15) SAG=0.0D0
              IF(ALENS(124,I).EQ.1.0D0) SAG=0.0D0
              NO=0
          ELSE
C       NOT PLANO WITH ASPHERICS
          END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
          IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
              C=ALENS(1,I)
              K=ALENS(2,I)
              ARG= ARG1(C,K,X,Y)
              IF(ARG.LT.0.0D0) THEN
                  NO=1
                  SAG=0.0D0
              ELSE
              END IF
C                       PROCEED
              CALL SAGASP(I,X,Y,SAG)
              IF(DABS(SAG).LT.1D-15) SAG=0.0D0
              IF(ALENS(124,I).EQ.1.0D0) SAG=0.0D0
              NO=0
          ELSE
C       NOT ROTATIONALLY SYMMETRIC ASPHERIC
          END IF
          IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
              IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                  CY=ALENS(1,I)
                  KY=ALENS(2,I)
                  DY=ALENS(4,I)
                  EY=ALENS(5,I)
                  FY=ALENS(6,I)
                  GY=ALENS(7,I)
                  CX=ALENS(24,I)
                  KX=ALENS(41,I)
                  DX=ALENS(37,I)
                  EX=ALENS(38,I)
                  FX=ALENS(39,I)
                  GX=ALENS(40,I)
              ELSE
              END IF
              IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                  CX=ALENS(1,I)
                  KX=ALENS(2,I)
                  DX=ALENS(4,I)
                  EX=ALENS(5,I)
                  FX=ALENS(6,I)
                  GX=ALENS(7,I)
                  CY=ALENS(24,I)
                  KY=ALENS(41,I)
                  DY=ALENS(37,I)
                  EY=ALENS(38,I)
                  FY=ALENS(39,I)
                  GY=ALENS(40,I)
              ELSE
              END IF
              ARG=ARG2(CX,CY,KX,KY,X,Y)
              IF(ARG.LT.0.0D0) THEN
                  NO=1
              ELSE
              END IF
C                       PROCEED
              CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
              IF(DABS(SAG).LT.1D-15) SAG=0.0D0
              IF(ALENS(124,I).EQ.1.0D0) SAG=0.0D0
              NO=0
          ELSE
C       NOT ANAMORPHIC ASPHERIC
          END IF
          IF(ALENS(124,I).EQ.1.0D0) SAG=0.0D0
          RETURN
      END
C SUB SAGSPC.FOR
      SUBROUTINE SAGSPC(I,X,Y,Z)
          USE GLOBALS
C
          IMPLICIT NONE
C
C     THIS IS SUBROUTINE SAGSPC.FOR CALCULATES SAG FOR A SPECIAL SURFACE
C
          EXTERNAL FF2,FF3,FF4,FF5
C
          REAL*8 XPASS,YPASS,ZPASS
C
          COMMON/SAGPAS/XPASS,YPASS,ZPASS
C
          INTEGER KLI,IPASS1

          COMMON/NEFER/KLI
C
          LOGICAL FUN,GERROR,GERROR1,GERROR2,UERROR
C
          COMMON/COMFUN/FUN
C
          EXTERNAL FNZ1
C
          INTEGER I,III
C
          REAL*8 X,Y,Z,THETA,R,FF2,FF3,FF4,XX,YY
     1    ,AAAX,AAAY,FF5,RRRHO,JK_WAVE,AMP1,OMEGA1X,OMEGA1Y
     1    ,AMP2,OMEGA2X,OMEGA2Y
     1    ,AMP3,OMEGA3X,OMEGA3Y
     1    ,AMP4,OMEGA4X,OMEGA4Y
     1    ,AMP5,OMEGA5X,OMEGA5Y
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
C
          INR=ALENS(76,I)
C
C     SPECIAL SURFACE TYPE 1
          Z=0.0D0
          IF(ALENS(34,I).EQ.1.0D0) THEN
              DO III=9,48
                  IF(FTFL01(III,I).EQ.0.0D0.OR.X.EQ.0.0D0.AND.Y.EQ.0.0D0
     1            .AND.(III-9).EQ.0) THEN
                      Z=Z+
     1                FTFL01(III,I)
                  ELSE
                      Z=Z+
     1                (FTFL01(III,I)*(((DSQRT((X**2)+(Y**2)))**(III-9))))
                  END IF
              END DO
              IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
              RETURN
          END IF
C     SPECIAL SURFACE TYPE 4
          IF(ALENS(34,I).EQ.4.0D0) THEN
              IF(INT(SYSTEM1(11)).EQ.1)  JK_WAVE=SYSTEM1(1)
              IF(INT(SYSTEM1(11)).EQ.2)  JK_WAVE=SYSTEM1(2)
              IF(INT(SYSTEM1(11)).EQ.3)  JK_WAVE=SYSTEM1(3)
              IF(INT(SYSTEM1(11)).EQ.4)  JK_WAVE=SYSTEM1(4)
              IF(INT(SYSTEM1(11)).EQ.5)  JK_WAVE=SYSTEM1(5)
              IF(INT(SYSTEM1(11)).EQ.6)  JK_WAVE=SYSTEM1(71)
              IF(INT(SYSTEM1(11)).EQ.7)  JK_WAVE=SYSTEM1(72)
              IF(INT(SYSTEM1(11)).EQ.8)  JK_WAVE=SYSTEM1(73)
              IF(INT(SYSTEM1(11)).EQ.9)  JK_WAVE=SYSTEM1(74)
              IF(INT(SYSTEM1(11)).EQ.10) JK_WAVE=SYSTEM1(75)
              AMP1=DABS(FTFL01(1,I)*JK_WAVE*0.5D0)
              AMP2=DABS(FTFL01(4,I)*JK_WAVE*0.5D0)
              AMP3=DABS(FTFL01(7,I)*JK_WAVE*0.5D0)
              AMP4=DABS(FTFL01(10,I)*JK_WAVE*0.5D0)
              AMP5=DABS(FTFL01(13,I)*JK_WAVE*0.5D0)
              IF(SYSTEM1(6).EQ.1) AMP1=(AMP1*1.0D-4/2.54D0)
              IF(SYSTEM1(6).EQ.2) AMP1=AMP1*1.0D-4
              IF(SYSTEM1(6).EQ.3) AMP1=AMP1*1.0D-3
              IF(SYSTEM1(6).EQ.4) AMP1=AMP1*1.0D-6
              IF(SYSTEM1(6).EQ.1) AMP2=(AMP2*1.0D-4/2.54D0)
              IF(SYSTEM1(6).EQ.2) AMP2=AMP2*1.0D-4
              IF(SYSTEM1(6).EQ.3) AMP2=AMP2*1.0D-3
              IF(SYSTEM1(6).EQ.4) AMP2=AMP2*1.0D-6
              IF(SYSTEM1(6).EQ.1) AMP3=(AMP3*1.0D-4/2.54D0)
              IF(SYSTEM1(6).EQ.2) AMP3=AMP3*1.0D-4
              IF(SYSTEM1(6).EQ.3) AMP3=AMP3*1.0D-3
              IF(SYSTEM1(6).EQ.4) AMP3=AMP3*1.0D-6
              IF(SYSTEM1(6).EQ.1) AMP4=(AMP4*1.0D-4/2.54D0)
              IF(SYSTEM1(6).EQ.2) AMP4=AMP4*1.0D-4
              IF(SYSTEM1(6).EQ.3) AMP4=AMP4*1.0D-3
              IF(SYSTEM1(6).EQ.4) AMP4=AMP4*1.0D-6
              IF(SYSTEM1(6).EQ.1) AMP5=(AMP5*1.0D-4/2.54D0)
              IF(SYSTEM1(6).EQ.2) AMP5=AMP5*1.0D-4
              IF(SYSTEM1(6).EQ.3) AMP5=AMP5*1.0D-3
              IF(SYSTEM1(6).EQ.4) AMP5=AMP5*1.0D-6
              IF(FTFL01(2,I).EQ.0.0D0) THEN
                  OMEGA1X=0.0D0
              ELSE
                  OMEGA1X=TWOPII/DABS(FTFL01(2,I))
              END IF
              IF(FTFL01(3,I).EQ.0.0D0) THEN
                  OMEGA1Y=0.0D0
              ELSE
                  OMEGA1Y=TWOPII/DABS(FTFL01(3,I))
              END IF
              IF(FTFL01(5,I).EQ.0.0D0) THEN
                  OMEGA2X=0.0D0
              ELSE
                  OMEGA2X=TWOPII/DABS(FTFL01(5,I))
              END IF
              IF(FTFL01(6,I).EQ.0.0D0) THEN
                  OMEGA2Y=0.0D0
              ELSE
                  OMEGA2Y=TWOPII/DABS(FTFL01(6,I))
              END IF
              IF(FTFL01(8,I).EQ.0.0D0) THEN
                  OMEGA3X=0.0D0
              ELSE
                  OMEGA3X=TWOPII/DABS(FTFL01(8,I))
              END IF
              IF(FTFL01(9,I).EQ.0.0D0) THEN
                  OMEGA3Y=0.0D0
              ELSE
                  OMEGA3Y=TWOPII/DABS(FTFL01(9,I))
              END IF
              IF(FTFL01(11,I).EQ.0.0D0) THEN
                  OMEGA4X=0.0D0
              ELSE
                  OMEGA4X=TWOPII/DABS(FTFL01(11,I))
              END IF
              IF(FTFL01(12,I).EQ.0.0D0) THEN
                  OMEGA4Y=0.0D0
              ELSE
                  OMEGA4Y=TWOPII/DABS(FTFL01(12,I))
              END IF
              IF(FTFL01(14,I).EQ.0.0D0) THEN
                  OMEGA5X=0.0D0
              ELSE
                  OMEGA5X=TWOPII/DABS(FTFL01(14,I))
              END IF
              IF(FTFL01(15,I).EQ.0.0D0) THEN
                  OMEGA5Y=0.0D0
              ELSE
                  OMEGA5Y=TWOPII/DABS(FTFL01(15,I))
              END IF
              Z=(AMP1*(DCOS(OMEGA1X*X)*(DCOS(OMEGA1Y*Y))))
     1        +(AMP2*(DCOS(OMEGA2X*X)*(DCOS(OMEGA2Y*Y))))
     1        +(AMP3*(DCOS(OMEGA3X*X)*(DCOS(OMEGA3Y*Y))))
     1        +(AMP4*(DCOS(OMEGA4X*X)*(DCOS(OMEGA4Y*Y))))
     1        +(AMP5*(DCOS(OMEGA5X*X)*(DCOS(OMEGA5Y*Y))))
              IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
              RETURN
          END IF
C
C     SPECIAL SURFACE TYPE 16 FRESNEL
          Z=0.0D0
          IF(ALENS(34,I).EQ.16.0D0) THEN
              IF(ALENS(1,I).EQ.0.0D0) THEN
                  Z=0.0D0
                  IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
              ELSE
                  IF(ALENS(23,I).EQ.0.0D0) THEN
                      RRRHO=DSQRT((X**2)+(Y**2))
                      Z=(ALENS(1,I)*(RRRHO**2))/
     1                (1.0D0+DSQRT(1.0D0-(1.0D0+ALENS(2,I))*(ALENS(1,I)**2)*(RRRHO**2)))
                      IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
                  END IF
                  IF(ALENS(23,I).EQ.1.0D0) THEN
                      RRRHO=X
                      Z=(ALENS(24,I)*(RRRHO**2))/
     1                (1.0D0+DSQRT(1.0D0-(1.0D0+ALENS(41,I))*(ALENS(24,I)**2)
     1                *(RRRHO**2)))
                      IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
                  END IF
                  IF(ALENS(23,I).EQ.2.0D0) THEN
                      RRRHO=Y
                      Z=(ALENS(24,I)*(RRRHO**2))/
     1                (1.0D0+DSQRT(1.0D0-(1.0D0+ALENS(41,I))*(ALENS(24,I)**2)
     1                *(RRRHO**2)))
                      IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
                  END IF
              END IF
              RETURN
          END IF
C
C     SPECIAL SURFACE TYPE 2
          IF(ALENS(34,I).EQ.2.0D0) THEN
              AAAX=X/INR
              AAAY=Y/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).LE.1.0D-15.AND.DABS(AAAX).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              IF(R.EQ.0.0D0) THETA=0.0D0
              Z=0.0D0
              DO III=1,66
                  Z=Z+
     1            (FTFL01(III,I)*(FF2(R,THETA,III)))
              END DO
              IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
              RETURN
          END IF
C
C     SPECIAL SURFACE TYPE 3
          IF(ALENS(34,I).EQ.3.0D0) THEN
              AAAX=X/INR
              AAAY=Y/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).LE.1.0D-15.AND.DABS(AAAX).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              IF(R.EQ.0.0D0) THETA=0.0D0
              Z=0.0D0
              DO III=1,37
                  Z=Z+
     1            (FTFL01(III,I)*(FF3(R,THETA,III)))
              END DO
              IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
              RETURN
          END IF
C
C
C     SPECIAL SURFACE TYPE 22
          IF(ALENS(34,I).EQ.22.0D0) THEN
              XPASS=X
              YPASS=Y
              GERROR=.FALSE.
              CALL GRIDS(5,I,GERROR)
              IF(.NOT.GERROR) GRIDSUNLOADED22(I)=.FALSE.
              IF(GERROR) THEN
                  WRITE(OUTLYNE,*)'NO GRID FILE EXISTS FOR THIS SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO SAG VALUE1 COULD BE CALCULATED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              Z=Z+ZPASS
              IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
              RETURN
          END IF
C
C     DEFORM
          IF(ALENS(103,I).EQ.1.0D0) THEN
              XPASS=X
              YPASS=Y
              GERROR1=.FALSE.
              GERROR2=.FALSE.
              CALL DEFGRIDS(5,I,GERROR1,GERROR2)
              IF(GERROR1) THEN
                  WRITE(OUTLYNE,*)'NO FILE EXISTS FOR THIS DEFORMABLE SURFACE'
                  CALL SHOWIT(1)
                  OUTLYNE='NO SAG VALUE1 COULD BE CALCULATED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              IF(GERROR2) THEN
                  WRITE(OUTLYNE,*)'POINT BEYOND DEFINED DEFORMABLE SURFACE BOUNDARY'
                  CALL SHOWIT(1)
                  OUTLYNE='NO SAG VALUE1 COULD BE CALCULATED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              ELSE
              END IF
              Z=Z+ZPASS
              IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
              RETURN
          END IF
C
C
C     SPECIAL SURFACE TYPE 23
          IF(ALENS(34,I).EQ.23.0D0) THEN
              XPASS=X
              YPASS=Y
              IPASS1=3
              CALL SPL23(I,XPASS,YPASS,ZPASS,IPASS1)
              Z=ZPASS
              IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
              RETURN
          END IF
C
C     SPECIAL SURFACE TYPE 14
          IF(ALENS(34,I).EQ.14.0D0) THEN
              AAAX=X/INR
              AAAY=Y/INR
              R=DSQRT((AAAX**2)+(AAAY**2))
              IF(DABS(AAAY).GE.DABS(((1.0D35)*AAAX))) THEN
                  IF(AAAY.GE.0.0D0) THETA=PII/2.0D0
                  IF(AAAY.LT.0.0D0) THETA=(3.0D0*PII)/2.0D0
              ELSE
                  IF(DABS(AAAY).LE.1.0D-15.AND.DABS(AAAX).LE.1.0D-15) THEN
                      THETA=0.0D0
                  ELSE
                      THETA=DATAN2(AAAY,AAAX)
                  END IF
                  IF(THETA.LT.0.0D0) THETA=THETA+(TWOPII)
              END IF
              IF(R.EQ.0.0D0) THETA=0.0D0
              Z=0.0D0
              DO III=1,48
                  Z=Z+
     1            (FTFL01(III,I)*(FF5(R,THETA,III)))
              END DO
              IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
              RETURN
          END IF
C
C     SPECIAL SURFACE TYPE 21
          IF(ALENS(34,I).EQ.21.0D0) THEN
              CALL USERSURF(I,X,Y,Z,UERROR)
              IF(UERROR) Z=0.0D0
              IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
              RETURN
          END IF
C
C     SPECIAL SURFACE TYPE 8
          IF(ALENS(34,I).EQ.8.0D0) THEN
              XX=X
              YY=Y
              Z=0.0D0
              DO III=1,91
                  Z=Z+
     1            (FTFL01(III,I)*(FF4(XX,YY,III)))
              END DO
              IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
              RETURN
          END IF
C
C     SPECIAL SURFACE TYPE 5
C     USER DEFINED SURFACE
          IF(ALENS(34,I).EQ.5.0D0.OR.ALENS(34,I).EQ.17.0D0) THEN
              REG(40)=REG(9)
              REG(9)=X
              REG(10)=Y
C     THIS SURFACE AUTOMATICALLY USES MACRO FUNCTION FUN10
C     FOR THE INITIAL USER FUCTION TO BE CALLED TO EVALUATE
C     THE Z VALUE1 CORRESPONDING TO THE AY AND AX VALUE1S.
C     COEFFICIENTS C1 TO C96 ARE USED BY THE USER IN THE
C     MACRO FUNCTION FUN10 AND ANY MACRO FUNCTION CALLED BY FUN10.
C     THEY ARE STORED INITIALLY IN GENERAL PURPOSE STORAGE REGISTERS
C     NUMBERS 301 TO 396 AND ACCESSED BY THE USER VIA RCL COMMANDS
              GPREG(301)=FTFL01(1,I)
              GPREG(302)=FTFL01(2,I)
              GPREG(303)=FTFL01(3,I)
              GPREG(304)=FTFL01(4,I)
              GPREG(305)=FTFL01(5,I)
              GPREG(306)=FTFL01(6,I)
              GPREG(307)=FTFL01(7,I)
              GPREG(308)=FTFL01(8,I)
              GPREG(309)=FTFL01(9,I)
              GPREG(310)=FTFL01(10,I)
              GPREG(311)=FTFL01(11,I)
              GPREG(312)=FTFL01(12,I)
              GPREG(313)=FTFL01(13,I)
              GPREG(314)=FTFL01(14,I)
              GPREG(315)=FTFL01(15,I)
              GPREG(316)=FTFL01(16,I)
              GPREG(317)=FTFL01(17,I)
              GPREG(318)=FTFL01(18,I)
              GPREG(319)=FTFL01(19,I)
              GPREG(320)=FTFL01(20,I)
              GPREG(321)=FTFL01(21,I)
              GPREG(322)=FTFL01(22,I)
              GPREG(323)=FTFL01(23,I)
              GPREG(324)=FTFL01(24,I)
              GPREG(325)=FTFL01(25,I)
              GPREG(326)=FTFL01(26,I)
              GPREG(327)=FTFL01(27,I)
              GPREG(328)=FTFL01(28,I)
              GPREG(329)=FTFL01(29,I)
              GPREG(330)=FTFL01(30,I)
              GPREG(331)=FTFL01(31,I)
              GPREG(332)=FTFL01(32,I)
              GPREG(333)=FTFL01(33,I)
              GPREG(334)=FTFL01(34,I)
              GPREG(335)=FTFL01(35,I)
              GPREG(336)=FTFL01(36,I)
              GPREG(337)=FTFL01(37,I)
              GPREG(338)=FTFL01(38,I)
              GPREG(339)=FTFL01(39,I)
              GPREG(340)=FTFL01(40,I)
              GPREG(341)=FTFL01(41,I)
              GPREG(342)=FTFL01(42,I)
              GPREG(343)=FTFL01(43,I)
              GPREG(344)=FTFL01(44,I)
              GPREG(345)=FTFL01(45,I)
              GPREG(346)=FTFL01(46,I)
              GPREG(347)=FTFL01(47,I)
              GPREG(348)=FTFL01(48,I)
              GPREG(349)=FTFL01(49,I)
              GPREG(350)=FTFL01(50,I)
              GPREG(351)=FTFL01(51,I)
              GPREG(352)=FTFL01(52,I)
              GPREG(353)=FTFL01(53,I)
              GPREG(354)=FTFL01(54,I)
              GPREG(355)=FTFL01(55,I)
              GPREG(356)=FTFL01(56,I)
              GPREG(357)=FTFL01(57,I)
              GPREG(358)=FTFL01(58,I)
              GPREG(359)=FTFL01(59,I)
              GPREG(360)=FTFL01(60,I)
              GPREG(361)=FTFL01(61,I)
              GPREG(362)=FTFL01(62,I)
              GPREG(363)=FTFL01(63,I)
              GPREG(364)=FTFL01(64,I)
              GPREG(365)=FTFL01(65,I)
              GPREG(366)=FTFL01(66,I)
              GPREG(367)=FTFL01(67,I)
              GPREG(368)=FTFL01(68,I)
              GPREG(369)=FTFL01(69,I)
              GPREG(370)=FTFL01(70,I)
              GPREG(371)=FTFL01(71,I)
              GPREG(372)=FTFL01(72,I)
              GPREG(373)=FTFL01(73,I)
              GPREG(374)=FTFL01(74,I)
              GPREG(375)=FTFL01(75,I)
              GPREG(376)=FTFL01(76,I)
              GPREG(377)=FTFL01(77,I)
              GPREG(378)=FTFL01(78,I)
              GPREG(379)=FTFL01(79,I)
              GPREG(380)=FTFL01(80,I)
              GPREG(381)=FTFL01(81,I)
              GPREG(382)=FTFL01(82,I)
              GPREG(383)=FTFL01(83,I)
              GPREG(384)=FTFL01(84,I)
              GPREG(385)=FTFL01(85,I)
              GPREG(386)=FTFL01(86,I)
              GPREG(387)=FTFL01(87,I)
              GPREG(388)=FTFL01(88,I)
              GPREG(389)=FTFL01(89,I)
              GPREG(390)=FTFL01(90,I)
              GPREG(391)=FTFL01(91,I)
              GPREG(392)=FTFL01(92,I)
              GPREG(393)=FTFL01(93,I)
              GPREG(394)=FTFL01(94,I)
              GPREG(395)=FTFL01(95,I)
              GPREG(396)=FTFL01(96,I)
              IF(.NOT.FUNEXT(10)) THEN
C     NO FUN10 EXISTS, RETURN 0.0D0 AS THE Z CONTRIBUTION
                  Z=0.0D0
              ELSE
C     FUN10 EXISTS, RUN FUN10
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC='FUN10'
                  WQ='        '
                  SQ=0
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
                  FMWQ='        '
                  FMSQ=0
                  FMNW(1)=0.0D0
                  FMNW(2)=0.0D0
                  FMNW(3)=0.0D0
                  FMNW(4)=0.0D0
                  FMNW(5)=0.0D0
                  FMDF(1)=1
                  FMDF(2)=1
                  FMDF(3)=1
                  FMDF(4)=1
                  FMDF(5)=1
                  NESFUN(1:10)=.FALSE.
                  KLI=10
                  FUN=.TRUE.
                  F26=1
                  CALL FUNEXC
                  F26=0
                  REST_KDP(1)=RESTINPT(1)
C     USE WHAT REMAINS IN THE Z-REGISTER
                  IF(ALENS(124,I).EQ.1.0D0) Z=0.0D0
                  Z=REG(11)
              END IF
              RETURN
          END IF
C
          RETURN
      END
C SUB ARG1.FOR
C       ROTATIONALLY SYMMETRIC ASPHERIC
      FUNCTION ARG1(C,K,X,Y)
C
          IMPLICIT NONE
C
          REAL*8 X,Y,C,K,RHO,ARG1
C
          RHO=DSQRT((X**2)+(Y**2))
          ARG1=
     1    1.0D0-((K+1.0D0)*(C**2)*(RHO**2))
          RETURN
      END
C SUB ARG2.FOR
C       ANAMORPHIC ASPHERIC
      FUNCTION ARG2(CX,CY,KX,KY,X,Y)
C
          IMPLICIT NONE
C
          REAL*8 CX,CY,KX,KY,X,Y
     1    ,ARG2,R
C
          R=1.0D0-((KX+1.0D0)*(CX**2)*(X**2))-((KY+1.0D0)*(CY**2)*(Y**2))
          ARG2=R
          RETURN
      END
C SUB SAGRET.FOR
      SUBROUTINE SAGRET(I,X,Y,Z,SAGERR)
C       THIS DOES SPECIAL SURFACES NOW
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SAGRET. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE SAG OF SURFACES IN RAYTRACING. PUT IN TO SUPPORT
C     NON-FLAT OBJECT SURFACES
C
          REAL*8 C,K,SAG,ARG
     1    ,ARG1,X,Y,ARG2,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY
     2    ,KX,KY,Z
C
          LOGICAL SAGERR
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          SAGCODE=0
          SAGERR=.FALSE.
C
C     FIX POSSIBLE OVERFLOWS AT OBJECT AND OTHER SURFACES
C
          IF(DABS(X).GT.1.0D35.OR.DABS(Y).GT.1.0D35) THEN
              SAG=0.0D0
              RETURN
          END IF
C
C       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
          IF(ALENS(1,I).EQ.0.0D0
     1    .AND.ALENS(23,I).EQ.0.0D0) THEN
              CALL SAGFLT(I,X,Y,SAG)
              IF(DABS(SAG).LT.1E-15) SAG=0.0D0
              Z=SAG
              RETURN
          END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
          IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
              C=ALENS(1,I)
              K=ALENS(2,I)
              ARG= ARG1(C,K,X,Y)
              IF(ARG.LT.0.0D0) THEN
                  Z=0.0D0
                  SAGERR=.TRUE.
                  RETURN
              END IF
C                       PROCEED
              CALL SAGASP(I,X,Y,SAG)
              IF(DABS(SAG).LT.1E-15) SAG=0.0D0
              Z=SAG
              RETURN
          END IF
          IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
              IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                  CY=ALENS(1,I)
                  KY=ALENS(2,I)
                  DY=ALENS(4,I)
                  EY=ALENS(5,I)
                  FY=ALENS(6,I)
                  GY=ALENS(7,I)
                  CX=ALENS(24,I)
                  KX=ALENS(41,I)
                  DX=ALENS(37,I)
                  EX=ALENS(38,I)
                  FX=ALENS(39,I)
                  GX=ALENS(40,I)
              END IF
              IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                  CX=ALENS(1,I)
                  KX=ALENS(2,I)
                  DX=ALENS(4,I)
                  EX=ALENS(5,I)
                  FX=ALENS(6,I)
                  GX=ALENS(7,I)
                  CY=ALENS(24,I)
                  KY=ALENS(41,I)
                  DY=ALENS(37,I)
                  EY=ALENS(38,I)
                  FY=ALENS(39,I)
                  GY=ALENS(40,I)
              END IF
              ARG=ARG2(CX,CY,KX,KY,X,Y)
              IF(ARG.LT.0.0D0) THEN
                  SAGERR=.TRUE.
                  Z=0.0D0
                  RETURN
              END IF
C                       PROCEED
              CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
              IF(DABS(SAG).LT.1E-15) SAG=0.0D0
              Z=SAG
              RETURN
          END IF
      END


      SUBROUTINE MAX_CLAP_VAL(I,MAXCLAP)
          IMPLICIT NONE
          REAL*8 MAXCLAP1,MAXCLAP2,MAXCLAP
          INTEGER I
          INCLUDE 'datlen.inc'
          IF(ALENS(9,I).EQ.0.0D0.OR.ALENS(127,I).NE.0.0D0) THEN
              MAXCLAP1=DABS(PXTRAY(5,I))+DABS(PXTRAY(1,I))
              MAXCLAP2=DABS(PXTRAX(5,I))+DABS(PXTRAX(1,I))
              MAXCLAP=MAXCLAP2
              IF(MAXCLAP1.GT.MAXCLAP2) MAXCLAP=MAXCLAP1
              RETURN
          END IF
          IF(ALENS(9,I).EQ.1.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
              MAXCLAP1=DABS(ALENS(10,I))+DABS(ALENS(12,I))
              MAXCLAP2=DABS(ALENS(10,I))+DABS(ALENS(13,I))
              MAXCLAP=MAXCLAP2
              IF(MAXCLAP1.GT.MAXCLAP2) MAXCLAP=MAXCLAP1
              RETURN
          END IF
          IF(ALENS(9,I).EQ.5.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
              MAXCLAP1=DABS(ALENS(10,I))+DABS(ALENS(12,I))
              MAXCLAP2=DABS(ALENS(10,I))+DABS(ALENS(13,I))
              MAXCLAP=MAXCLAP2
              IF(MAXCLAP1.GT.MAXCLAP2) MAXCLAP=MAXCLAP1
              RETURN
          END IF
          IF(ALENS(9,I).EQ.6.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
              MAXCLAP1=DABS(ALENS(11,I))+DABS(ALENS(12,I))
              MAXCLAP2=DABS(ALENS(11,I))+DABS(ALENS(13,I))
              MAXCLAP=MAXCLAP2
              IF(MAXCLAP1.GT.MAXCLAP2) MAXCLAP=MAXCLAP1
              RETURN
          END IF
          IF(ALENS(9,I).GT.1.0D0.AND.ALENS(9,I).LE.4.0D0.AND.
     1    ALENS(127,I).EQ.0.0D0) THEN
              MAXCLAP1=DABS(ALENS(10,I))+DABS(ALENS(12,I))
              MAXCLAP2=DABS(ALENS(11,I))+DABS(ALENS(13,I))
              MAXCLAP=MAXCLAP2
              IF(MAXCLAP1.GT.MAXCLAP2) MAXCLAP=MAXCLAP1
              RETURN
          END IF
      END
C SUB SSAAGG.FOR
      SUBROUTINE SSAAGG
C       THIS DOES SPECIAL SURFACES NOW
C
          IMPLICIT NONE
C
C       THIS IS SUBROUTINE SSAAGG. THIS IS THE SUBROUTINE WHICH
C       HANDLES THE SAG CMD LEVEL COMMAND.
C
          REAL*8 C,K,SAG,DELTA,ARG,MAXCLAP,DELCLAP,SSAG
     1    ,ARG1,X,Y,ARG2,CX,CY,DX,DY,EX,EY,FX,FY,GX,GY
     2    ,KX,KY,S_L,S_M,S_N,DDD
C
          REAL*8 VALUE1
C
          INTEGER ISAG
          REAL*8 SAGMIN,SAGMAX,SAGRMS,SAGMEAN,SAG2MEAN,SAGPTOV
          COMMON/SAGSTUFF/SAGMIN,SAGMAX,SAGRMS,SAGMEAN,ISAG,SAG2MEAN
     1    ,SAGPTOV

          LOGICAL EXIS90,OPEN90,ISITIN
C
          INTEGER I,J,NPOINT,GOO,NUM5,L,M,N
C
          COMMON/GV/VALUE1,NUM5
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
          SAGMAX=-1.0D300
          SAGMIN=1.0D300
          SAGRMS=0.0D0
          SAGMEAN=0.0D0
          SAG2MEAN=0.0D0
C
C       RESTORE TO OLD OBJ,REF AND IMAGE SURFACES
          CALL RESSUR
C
C     GET SAG DONE HERE
C
C       DO GET SAG HERE
          IF(WC.EQ.'GET'.AND.WQ.EQ.'SAG') THEN
C
              IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
                  WRITE(OUTLYNE,*)'FOR "GET SAG"'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)
     1            'SURFACE NUMBER BEYOND LEGAL BOUNDS'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
C
              I=INT(W1)
C       FLAT SURFACE (MAYBE ASPHERICS AND SPECIAL SURFACE STUFF)
              IF(ALENS(1,I).EQ.0.0D0
     1        .AND.ALENS(23,I).EQ.0.0D0) THEN
                  X=W2
                  Y=W3
                  CALL SAGFLT(I,X,Y,SAG)
                  IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                  VALUE1=SAG
                  CALL GETA
                  DDD=SAGDEL
                  CALL SAGFLT(I,X+DDD,Y,SAG)
                  S_L=SAG
                  CALL SAGFLT(I,X-DDD,Y,SAG)
                  S_L=S_L-SAG
                  S_L=-S_L/(2.0D0*DDD)
                  CALL SAGFLT(I,X,Y+DDD,SAG)
                  S_M=SAG
                  CALL SAGFLT(I,X,Y-DDD,SAG)
                  S_M=S_M-SAG
                  S_M=-S_M/(2.0D0*DDD)
                  S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
                  REG(12)=S_N
                  REG(11)=S_M
                  REG(10)=S_L
                  RETURN
              END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
              IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
                  C=ALENS(1,I)
                  K=ALENS(2,I)
                  X=W2
                  Y=W3
                  ARG= ARG1(C,K,X,Y)
                  IF(ARG.LT.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NO REAL SAG VALUE1 EXISTS FOR THE GIVEN X AND Y VALUE1S'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      GOO=1
                  END IF
C                       PROCEED
                  IF(GOO.NE.1) THEN
                      CALL SAGASP(I,X,Y,SAG)
                      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                      VALUE1=SAG
                      CALL GETA
                      DDD=SAGDEL
                      CALL SAGASP(I,X+DDD,Y,SAG)
                      S_L=SAG
                      CALL SAGASP(I,X-DDD,Y,SAG)
                      S_L=S_L-SAG
                      S_L=-S_L/(2.0D0*DDD)
                      CALL SAGASP(I,X,Y+DDD,SAG)
                      S_M=SAG
                      CALL SAGASP(I,X,Y-DDD,SAG)
                      S_M=S_M-SAG
                      S_M=-S_M/(2.0D0*DDD)
                      S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
                      REG(12)=S_N
                      REG(11)=S_M
                      REG(10)=S_L
                  END IF
                  GOO=0
                  RETURN
              END IF
              IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
                  IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                      CY=ALENS(1,I)
                      KY=ALENS(2,I)
                      DY=ALENS(4,I)
                      EY=ALENS(5,I)
                      FY=ALENS(6,I)
                      GY=ALENS(7,I)
                      CX=ALENS(24,I)
                      KX=ALENS(41,I)
                      DX=ALENS(37,I)
                      EX=ALENS(38,I)
                      FX=ALENS(39,I)
                      GX=ALENS(40,I)
                  END IF
                  IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                      CX=ALENS(1,I)
                      KX=ALENS(2,I)
                      DX=ALENS(4,I)
                      EX=ALENS(5,I)
                      FX=ALENS(6,I)
                      GX=ALENS(7,I)
                      CY=ALENS(24,I)
                      KY=ALENS(41,I)
                      DY=ALENS(37,I)
                      EY=ALENS(38,I)
                      FY=ALENS(39,I)
                      GY=ALENS(40,I)
                  END IF
                  NPOINT=INT((W3-W2)/W4)
                  X=W2
                  Y=W3
                  ARG=ARG2(CX,CY,KX,KY,X,Y)
                  IF(ARG.LT.0.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                'NO REAL SAG VALUE1 EXISTS BEYOND THE RANGE ALREADY PRINTED'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'SAG TABLE OUTPUT TERMINATED'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      GOO=1
                  END IF
C                       PROCEED
                  IF(GOO.NE.1) THEN
                      CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                      VALUE1=SAG
                      CALL GETA
                      DDD=SAGDEL
                      CALL SAGANA(CX,CY,KX,KY,X+DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                      S_L=SAG
                      CALL SAGANA(CX,CY,KX,KY,X-DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                      S_L=S_L-SAG
                      S_L=-S_L/(2.0D0*DDD)
                      CALL SAGANA(CX,CY,KX,KY,X,Y+DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                      S_M=SAG
                      CALL SAGANA(CX,CY,KX,KY,X,Y-DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                      S_M=S_M-SAG
                      S_M=-S_M/(2.0D0*DDD)
                      S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
                      REG(12)=S_N
                      REG(11)=S_M
                      REG(10)=S_L
                  END IF
                  GOO=0
                  RETURN
              END IF
              RETURN
          END IF
C
C     THE SAG COMMAND
C
          IF(SST.EQ.1) THEN
              WRITE(OUTLYNE,*)
     1        '"',WC(1:3),'"',
     1        ' TAKES NO STRING INPUT'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SYSTEM1(20).EQ.0.0) THEN
              WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C       CHECK QUALIFIERS
          IF(WQ.EQ.'X'.OR.WQ.EQ.'Y'.OR.WQ.EQ.'PT'.OR.WQ.EQ.'FILE'
     1    .OR.WQ.EQ.'PTACC') THEN
C       VALID QUALIFIERS, PROCEED
C       DO DEFAULTS FOR SAG Y AND SAG X
              IF(WQ.EQ.'X'.OR.WQ.EQ.'Y') THEN
                  IF(DF1.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                '"SAG X" AND "SAG Y" REQUIRE EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF2.EQ.1) W2=0.0D0
                  IF(DF3.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                '"SAG X" AND "SAG Y" REQUIRE EXPLICIT NUMERIC WORD #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF4.EQ.1) W4=(DABS(W3)-DABS(W2))/10.0D0
              END IF
C       DO DEFAULTS FOR SAG PT
              IF(WQ.EQ.'PT'.OR.WQ.EQ.'PTACC') THEN
                  IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
                      IF(WQ.EQ.'PT')WRITE(OUTLYNE,*)
     1                '"SAG PT" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
                      IF(WQ.EQ.'PTACC')WRITE(OUTLYNE,*)
     1                '"SAG PTACC" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF4.EQ.1) W4 = 0.0D0
                  IF(DF5.EQ.1) W5 = 0.0D0
                  IF(S4.EQ.1.OR.S5.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                '"SAG PT" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C       DO DEFAULTS FOR SAG FILE
              IF(WQ.EQ.'FILE') THEN
                  IF(DF1.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                '"SAG FILE" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM1(20)) THEN
                      WRITE(OUTLYNE,*)
     1                'SURFACE NUMBER FOR "SAG FILE" BEYOND LEGAL BOUNDS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF2.EQ.1) W2=25.0D0
                  IF(W2.LT.4.0D0) THEN
                      WRITE(OUTLYNE,*)
     1                '"N" MUST BE AT LEAST 4, FOR "SAG FILE"'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(DF3.EQ.1) W3 = 0.0D0
                  SAGCODE=INT(W3)
                  IF(INT(W3).NE.0.AND.INT(W3).NE.1.AND.INT(W3).NE.2) THEN
                      WRITE(OUTLYNE,*)
     1                '"CODE" MUST BE 0, 1, OR 2, FOR "SAG FILE"'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
                  IF(S4.EQ.1) THEN
                      WRITE(OUTLYNE,*)
     1                '"SAG FILE" TAKES NO NUMERIC WORD #4 INPUT'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
              END IF
C
C       NOW DO SAG X, SAG Y AND SAG PT
              IF(WQ.EQ.'X'.OR.WQ.EQ.'Y') THEN
C
                  IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
                      WRITE(OUTLYNE,*)'FOR "SAG Y" AND "SAG X"'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'SURFACE NUMBER BEYOND LEGAL BOUNDS'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  IF(W3.LE.W2) THEN
                      WRITE(OUTLYNE,*)'FOR "SAG Y" AND "SAG X"'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #3 MUST BE GREATER THAN NUMERIC WORD #2'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  IF((W3-W2).LT.W4) THEN
                      WRITE(OUTLYNE,*)'FOR "SAG Y" AND "SAG X"'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORD #4 MUST BE LESS THAN THE DIFFERENCE BETWEEN'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)
     1                'NUMERIC WORDS #3 AND #2'
                      CALL SHOWIT(1)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(1)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  I=INT(W1)
C       FLAT SURFACE MAYBE ASPHERICS OR SPECIAL
                  IF(ALENS(1,I).EQ.0.0D0
     1            .AND.ALENS(23,I).EQ.0.0D0) THEN
                      NPOINT=INT((W3-W2)/W4)
C       PRINT HEADING
                      X=0.0D0
                      Y=0.0D0
                      SAG=0.0D0
                      WRITE(OUTLYNE,100) I
                      CALL SHOWIT(0)
                      IF(SYSTEM1(6).EQ.1.0) WRITE(OUTLYNE,23)
                      IF(SYSTEM1(6).EQ.2.0) WRITE(OUTLYNE,24)
                      IF(SYSTEM1(6).EQ.3.0) WRITE(OUTLYNE,25)
                      IF(SYSTEM1(6).EQ.4.0) WRITE(OUTLYNE,33)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,11)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,200)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,11)
                      CALL SHOWIT(0)
                      IF(WQ.EQ.'X') THEN
                          Y=W5
                          X=W2
                      END IF
                      IF(WQ.EQ.'Y') THEN
                          X=W5
                          Y=W2
                      END IF
                      DELTA=(W3-W2)/DBLE(NPOINT)
                      DO J=1,NPOINT+1
                          CALL SAGFLT(I,X,Y,SAG)
                          IF(DABS(X).LT.1E-15) X=0.0D0
                          IF(DABS(Y).LT.1E-15) Y=0.0D0
                          IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                          SSAG=SAG
                          DDD=SAGDEL
                          CALL SAGFLT(I,X+DDD,Y,SAG)
                          S_L=SAG
                          CALL SAGFLT(I,X-DDD,Y,SAG)
                          S_L=S_L-SAG
                          S_L=-S_L/(2.0D0*DDD)
                          CALL SAGFLT(I,X,Y+DDD,SAG)
                          S_M=SAG
                          CALL SAGFLT(I,X,Y-DDD,SAG)
                          S_M=S_M-SAG
                          S_M=-S_M/(2.0D0*DDD)
                          S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
                          WRITE(OUTLYNE,300)X,Y,SSAG,S_L,S_M,S_N
                          CALL SHOWIT(0)
                          IF(WQ.EQ.'X') X=X+DELTA
                          IF(WQ.EQ.'Y') Y=Y+DELTA
                      END DO
                      RETURN
                  END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
                  IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
                      C=ALENS(1,I)
                      K=ALENS(2,I)
                      NPOINT=INT((W3-W2)/W4)
                      X=0.0D0
                      Y=0.0D0
                      SAG=0.0D0
                      IF(WQ.EQ.'X') THEN
                          Y=W5
                          X=W2
                      END IF
                      IF(WQ.EQ.'Y') THEN
                          X=W5
                          Y=W2
                      END IF
                      WRITE(OUTLYNE,100) I
                      CALL SHOWIT(0)
                      IF(SYSTEM1(6).EQ.1.0) WRITE(OUTLYNE,23)
                      IF(SYSTEM1(6).EQ.2.0) WRITE(OUTLYNE,24)
                      IF(SYSTEM1(6).EQ.3.0) WRITE(OUTLYNE,25)
                      IF(SYSTEM1(6).EQ.4.0) WRITE(OUTLYNE,33)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,11)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,200)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,11)
                      CALL SHOWIT(0)
C       PRINT HEADING
                      DELTA=(W3-W2)/DBLE(NPOINT)
                      DO J=1,NPOINT+1
                          ARG= ARG1(C,K,X,Y)
                          IF(ARG.LT.0.0D0) THEN
                              WRITE(OUTLYNE,*)
     1                        'NO REAL SAG VALUE1 EXISTS BEYOND THE RANGE ALREADY PRINTED'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SAG TABLE OUTPUT TERMINATED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              GOO=1
                          END IF
C                       PROCEED
                          IF(GOO.NE.1) THEN
                              CALL SAGASP(I,X,Y,SAG)
                              IF(DABS(X).LT.1E-15) X=0.0D0
                              IF(DABS(Y).LT.1E-15) Y=0.0D0
                              IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                              SSAG=SAG
                              DDD=SAGDEL
                              CALL SAGASP(I,X+DDD,Y,SAG)
                              S_L=SAG
                              CALL SAGASP(I,X-DDD,Y,SAG)
                              S_L=S_L-SAG
                              S_L=-S_L/(2.0D0*DDD)
                              CALL SAGASP(I,X,Y+DDD,SAG)
                              S_M=SAG
                              CALL SAGASP(I,X,Y-DDD,SAG)
                              S_M=S_M-SAG
                              S_M=-S_M/(2.0D0*DDD)
                              S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
                              WRITE(OUTLYNE,300)X,Y,SSAG,S_L,S_M,S_N
                              CALL SHOWIT(0)
                              IF(WQ.EQ.'X') X=X+DELTA
                              IF(WQ.EQ.'Y') Y=Y+DELTA
                          END IF
                          GOO=0
                      END DO
                      RETURN
                  END IF
                  IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
                      IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                          CY=ALENS(1,I)
                          KY=ALENS(2,I)
                          DY=ALENS(4,I)
                          EY=ALENS(5,I)
                          FY=ALENS(6,I)
                          GY=ALENS(7,I)
                          CX=ALENS(24,I)
                          KX=ALENS(41,I)
                          DX=ALENS(37,I)
                          EX=ALENS(38,I)
                          FX=ALENS(39,I)
                          GX=ALENS(40,I)
                      END IF
                      IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                          CX=ALENS(1,I)
                          KX=ALENS(2,I)
                          DX=ALENS(4,I)
                          EX=ALENS(5,I)
                          FX=ALENS(6,I)
                          GX=ALENS(7,I)
                          CY=ALENS(24,I)
                          KY=ALENS(41,I)
                          DY=ALENS(37,I)
                          EY=ALENS(38,I)
                          FY=ALENS(39,I)
                          GY=ALENS(40,I)
                      END IF
                      NPOINT=INT((W3-W2)/W4)
                      X=0.0D0
                      Y=0.0D0
                      SAG=0.0D0
                      IF(WQ.EQ.'X') THEN
                          Y=W5
                          X=W2
                      END IF
                      IF(WQ.EQ.'Y') THEN
                          X=W5
                          Y=W2
                      END IF
                      WRITE(OUTLYNE,100) I
                      CALL SHOWIT(0)
                      IF(SYSTEM1(6).EQ.1.0) WRITE(OUTLYNE,23)
                      IF(SYSTEM1(6).EQ.2.0) WRITE(OUTLYNE,24)
                      IF(SYSTEM1(6).EQ.3.0) WRITE(OUTLYNE,25)
                      IF(SYSTEM1(6).EQ.4.0) WRITE(OUTLYNE,33)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,11)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,200)
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,11)
                      CALL SHOWIT(0)
C       PRINT HEADING
                      DELTA=(W3-W2)/DBLE(NPOINT)
                      DO J=1,NPOINT+1
                          ARG=ARG2(CX,CY,KX,KY,X,Y)
                          IF(ARG.LT.0.0D0) THEN
                              WRITE(OUTLYNE,*)
     1                        'NO REAL SAG VALUE1 EXISTS BEYOND THE RANGE ALREADY PRINTED'
                              CALL SHOWIT(1)
                              WRITE(OUTLYNE,*)'SAG TABLE OUTPUT TERMINATED'
                              CALL SHOWIT(1)
                              CALL MACFAL
                              GOO=1
                          END IF
C                       PROCEED
                          IF(GOO.NE.1) THEN
                              CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                              IF(DABS(X).LT.1E-15) X=0.0D0
                              IF(DABS(Y).LT.1E-15) Y=0.0D0
                              IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                              SSAG=SAG
                              DDD=SAGDEL
                              CALL SAGANA(CX,CY,KX,KY,X+DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                              S_L=SAG
                              CALL SAGANA(CX,CY,KX,KY,X-DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                              S_L=S_L-SAG
                              S_L=-S_L/(2.0D0*DDD)
                              CALL SAGANA(CX,CY,KX,KY,X,Y+DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                              S_M=SAG
                              CALL SAGANA(CX,CY,KX,KY,X,Y-DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                              S_M=S_M-SAG
                              S_M=-S_M/(2.0D0*DDD)
                              S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))
                              WRITE(OUTLYNE,300)X,Y,SSAG,S_L,S_M,S_N
                              CALL SHOWIT(0)
                              IF(WQ.EQ.'X') X=X+DELTA
                              IF(WQ.EQ.'Y') Y=Y+DELTA
                          END IF
                          GOO=0
                      END DO
                      RETURN
                  END IF
              END IF
              IF(WQ.EQ.'PT'.OR.WQ.EQ.'PTACC') THEN
C
                  IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
                      WRITE(OUTLYNE,*)'FOR "SAG PT"'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)
     1                'SURFACE NUMBER BEYOND LEGAL BOUNDS'
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                      CALL SHOWIT(0)
                      CALL MACFAL
                      RETURN
                  END IF
C
                  I=INT(W1)
C       FLAT SURFACE MAYBE ASPHERICS AND SPECIAL
                  IF(ALENS(1,I).EQ.0.0D0
     1            .AND.ALENS(23,I).EQ.0.0D0) THEN
                      X=W2
                      Y=W3


                      CALL SAGFLT(I,X,Y,SAG)
                      IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                      DDD=SAGDEL
                      CALL SAGFLT(I,X+DDD,Y,SAG)
                      S_L=SAG
                      CALL SAGFLT(I,X-DDD,Y,SAG)
                      S_L=S_L-SAG
                      S_L=-S_L/(2.0D0*DDD)
                      CALL SAGFLT(I,X,Y+DDD,SAG)
                      S_M=SAG
                      CALL SAGFLT(I,X,Y-DDD,SAG)
                      S_M=S_M-SAG
                      S_M=-S_M/(2.0D0*DDD)
                      S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))

                      IF(WQ.NE.'PTACC') THEN
                          WRITE(OUTLYNE,100) I
                          CALL SHOWIT(0)
                          IF(SYSTEM1(6).EQ.1.0) WRITE(OUTLYNE,23)
                          IF(SYSTEM1(6).EQ.2.0) WRITE(OUTLYNE,24)
                          IF(SYSTEM1(6).EQ.3.0) WRITE(OUTLYNE,25)
                          IF(SYSTEM1(6).EQ.4.0) WRITE(OUTLYNE,33)
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,301)X
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,302)Y
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,303)SAG
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,304)S_L
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,305)S_M
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,306)S_N
                          CALL SHOWIT(0)
                      END IF
                      REG(13)=X
                      REG(14)=Y
                      REG(40)=REG(9)
                      REG(9)=SAG
                      REG(12)=S_N
                      REG(11)=S_M
                      REG(10)=S_L
                      RETURN
                  END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
                  IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
                      C=ALENS(1,I)
                      K=ALENS(2,I)
                      X=W2
                      Y=W3

                      ARG= ARG1(C,K,X,Y)
                      IF(ARG.LT.0.0D0) THEN
                          WRITE(OUTLYNE,*)
     1                    'NO REAL SAG VALUE1 EXISTS FOR THE GIVEN X AND Y VALUE1S'
                          CALL SHOWIT(0)
                          WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                          CALL SHOWIT(0)
                          CALL MACFAL
                          GOO=1
                      END IF
C                       PROCEED
                      IF(GOO.NE.1) THEN
                          CALL SAGASP(I,X,Y,SAG)
                          IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                          DDD=SAGDEL
                          CALL SAGASP(I,X+DDD,Y,SAG)
                          S_L=SAG
                          CALL SAGASP(I,X-DDD,Y,SAG)
                          S_L=S_L-SAG
                          S_L=-S_L/(2.0D0*DDD)
                          CALL SAGASP(I,X,Y+DDD,SAG)
                          S_M=SAG
                          CALL SAGASP(I,X,Y-DDD,SAG)
                          S_M=S_M-SAG
                          S_M=-S_M/(2.0D0*DDD)
                          S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))

                          IF(WQ.NE.'PTACC') THEN
                              WRITE(OUTLYNE,100) I
                              CALL SHOWIT(0)
                              IF(SYSTEM1(6).EQ.1.0) WRITE(OUTLYNE,23)
                              IF(SYSTEM1(6).EQ.2.0) WRITE(OUTLYNE,24)
                              IF(SYSTEM1(6).EQ.3.0) WRITE(OUTLYNE,25)
                              IF(SYSTEM1(6).EQ.4.0) WRITE(OUTLYNE,33)
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,301)X
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,302)Y
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,303)SAG
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,304)S_L
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,305)S_M
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,306)S_N
                              CALL SHOWIT(0)
                          END IF
                          REG(13)=X
                          REG(14)=Y
                          REG(40)=REG(9)
                          REG(9)=SAG
                          REG(12)=S_N
                          REG(11)=S_M
                          REG(10)=S_L
                      END IF
                      GOO=0
                      RETURN
                  END IF
                  IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
                      IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                          CY=ALENS(1,I)
                          KY=ALENS(2,I)
                          DY=ALENS(4,I)
                          EY=ALENS(5,I)
                          FY=ALENS(6,I)
                          GY=ALENS(7,I)
                          CX=ALENS(24,I)
                          KX=ALENS(41,I)
                          DX=ALENS(37,I)
                          EX=ALENS(38,I)
                          FX=ALENS(39,I)
                          GX=ALENS(40,I)
                      END IF
                      IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                          CX=ALENS(1,I)
                          KX=ALENS(2,I)
                          DX=ALENS(4,I)
                          EX=ALENS(5,I)
                          FX=ALENS(6,I)
                          GX=ALENS(7,I)
                          CY=ALENS(24,I)
                          KY=ALENS(41,I)
                          DY=ALENS(37,I)
                          EY=ALENS(38,I)
                          FY=ALENS(39,I)
                          GY=ALENS(40,I)
                      END IF
                      NPOINT=INT((W3-W2)/W4)
                      X=W2
                      Y=W3

C       PRINT HEADING
                      ARG=ARG2(CX,CY,KX,KY,X,Y)
                      IF(ARG.LT.0.0D0) THEN
                          WRITE(OUTLYNE,*)
     1                    'NO REAL SAG VALUE1 EXISTS BEYOND THE RANGE ALREADY PRINTED'
                          CALL SHOWIT(1)
                          WRITE(OUTLYNE,*)'SAG TABLE OUTPUT TERMINATED'
                          CALL SHOWIT(1)
                          CALL MACFAL
                          GOO=1
                      END IF
C                       PROCEED
                      IF(GOO.NE.1) THEN
                          CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                          IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                          DDD=SAGDEL
                          CALL SAGANA(CX,CY,KX,KY,X+DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                          S_L=SAG
                          CALL SAGANA(CX,CY,KX,KY,X-DDD,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                          S_L=S_L-SAG
                          S_L=-S_L/(2.0D0*DDD)
                          CALL SAGANA(CX,CY,KX,KY,X,Y+DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                          S_M=SAG
                          CALL SAGANA(CX,CY,KX,KY,X,Y-DDD,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                          S_M=S_M-SAG
                          S_M=-S_M/(2.0D0*DDD)
                          S_N=DSQRT(1.0D0-(S_L**2)-(S_M**2))

                          IF(WQ.EQ.'PTACC') THEN
                              WRITE(OUTLYNE,100) I
                              CALL SHOWIT(0)
                              IF(SYSTEM1(6).EQ.1.0) WRITE(OUTLYNE,23)
                              IF(SYSTEM1(6).EQ.2.0) WRITE(OUTLYNE,24)
                              IF(SYSTEM1(6).EQ.3.0) WRITE(OUTLYNE,25)
                              IF(SYSTEM1(6).EQ.4.0) WRITE(OUTLYNE,33)
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,301)X
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,302)Y
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,303)SAG
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,304)S_L
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,305)S_M
                              CALL SHOWIT(0)
                              WRITE(OUTLYNE,306)S_N
                              CALL SHOWIT(0)
                          END IF
                          REG(13)=X
                          REG(14)=Y
                          REG(40)=REG(9)
                          REG(9)=SAG
                          REG(12)=S_N
                          REG(11)=S_M
                          REG(10)=S_L
                      END IF
                      GOO=0
                      RETURN
                  END IF
              END IF
              IF(WQ.EQ.'FILE') THEN
                  EXIS90=.FALSE.
                  OPEN90=.FALSE.
                  INQUIRE(FILE=trim(HOME)//'SAG.DAT',EXIST=EXIS90)
                  INQUIRE(FILE=trim(HOME)//'SAG.DAT',OPENED=OPEN90)
                  IF(EXIS90) THEN
                      IF(OPEN90) THEN
                          CALL CLOSE_FILE(90,0)
                      ELSE
                          OPEN(UNIT=90,FILE=trim(HOME)//'SAG.DAT',STATUS='UNKNOWN')
                          CALL CLOSE_FILE(90,0)
                      END IF
                  ELSE
C     NO FILE TO GET RID OF
                  END IF
C     OPEN THE OUTPUT FILE
                  OPEN(UNIT=90,BLANK='NULL'
     1              ,FORM='FORMATTED',FILE=trim(HOME)//'SAG.DAT'
     2              ,STATUS='UNKNOWN')
C
                  I=INT(W1)
                  N=INT(W2)
C     SET UP DO LOOP PARAMETERS AND LOOP
C     DETERMINE MAXIMUM CLEAR APERTURE
                  CALL MAX_CLAP_VAL(I,MAXCLAP)
                  DELCLAP=DABS((2.0D0*MAXCLAP)/DBLE(N-1))
                  WRITE(90,*) I,N*N
                  Y=-MAXCLAP
                  DO M=1,N
                      X=-MAXCLAP
                      DO L=1,N
C       FLAT SURFACE MAYBE ASPHERICS AND SPECIAL
                          IF(ALENS(1,I).EQ.0.0D0
     1                    .AND.ALENS(23,I).EQ.0.0D0) THEN
                              CALL SAGFLT(I,X,Y,SAG)
                              IF(DABS(SAG).LT.1E-15) SAG=0.0D0

                          END IF
C       SPHERICAL, CONIC AND ASPHERIC ROTATIONALLY SYMMETRIC SURFACES
                          IF(ALENS(1,I).NE.0.0D0.AND.ALENS(23,I).EQ.0.0D0) THEN
                              C=ALENS(1,I)
                              K=ALENS(2,I)
                              ARG= ARG1(C,K,X,Y)
                              IF(ARG.LT.0.0D0) THEN
                                  WRITE(OUTLYNE,*)
     1                            'NO REAL SAG VALUE1 EXISTS FOR THE GIVEN X AND Y VALUE1S'
                                  CALL SHOWIT(0)
                                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                                  CALL SHOWIT(0)
                                  CALL MACFAL
                                  GOO=1
                              END IF
C                       PROCEED
                              IF(GOO.NE.1) THEN
                                  CALL SAGASP(I,X,Y,SAG)
                                  IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                              END IF
                              GOO=0
                          END IF
                          IF(ALENS(23,I).NE.0.0D0) THEN
C       SURFACE I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
C       ASPHERIC
                              IF(ALENS(23,I).EQ.1.0D0) THEN
C       Y-TORIC
                                  CY=ALENS(1,I)
                                  KY=ALENS(2,I)
                                  DY=ALENS(4,I)
                                  EY=ALENS(5,I)
                                  FY=ALENS(6,I)
                                  GY=ALENS(7,I)
                                  CX=ALENS(24,I)
                                  KX=ALENS(41,I)
                                  DX=ALENS(37,I)
                                  EX=ALENS(38,I)
                                  FX=ALENS(39,I)
                                  GX=ALENS(40,I)
                              END IF
                              IF(ALENS(23,I).EQ.2.0D0) THEN
C       X-TORIC
                                  CX=ALENS(1,I)
                                  KX=ALENS(2,I)
                                  DX=ALENS(4,I)
                                  EX=ALENS(5,I)
                                  FX=ALENS(6,I)
                                  GX=ALENS(7,I)
                                  CY=ALENS(24,I)
                                  KY=ALENS(41,I)
                                  DY=ALENS(37,I)
                                  EY=ALENS(38,I)
                                  FY=ALENS(39,I)
                                  GY=ALENS(40,I)
                              END IF
                              NPOINT=INT((W3-W2)/W4)
                              ARG=ARG2(CX,CY,KX,KY,X,Y)
                              IF(ARG.LT.0.0D0) THEN
                                  WRITE(OUTLYNE,*)
     1                            'NO REAL SAG VALUE1 EXISTS BEYOND THE RANGE ALREADY PRINTED'
                                  CALL SHOWIT(1)
                                  WRITE(OUTLYNE,*)'SAG TABLE OUTPUT TERMINATED'
                                  CALL SHOWIT(1)
                                  CALL MACFAL
                                  GOO=1
                                  CALL CLOSE_FILE(90,0)
                                  RETURN
                              END IF
C                       PROCEED
                              IF(GOO.NE.1) THEN
                                  CALL SAGANA(CX,CY,KX,KY,X,Y,DX,DY,EX,EY,FX,FY,GX,GY,SAG,I)
                                  IF(DABS(SAG).LT.1E-15) SAG=0.0D0
                              END IF
                              GOO=0
                          END IF
                          CALL SAGCACO(I,X,Y,SAG,ISITIN)
                          WRITE(90,102)X,Y,SAG
                          IF(SAG.GT.SAGMAX) SAGMAX=SAG
                          IF(SAG.LT.SAGMIN) SAGMIN=SAG
                          SAGMEAN=SAGMEAN+SAG
                          SAG2MEAN=SAG2MEAN+(SAG**2)
                          ISAG=ISAG+1
 102                      FORMAT(D23.15,1X,D23.15,1X,D23.15)
                          X=X+DELCLAP
                      END DO
                      Y=Y+DELCLAP
                  END DO
                  CALL CLOSE_FILE(90,1)
                  SAGRMS=DSQRT((SAG2MEAN-((SAGMEAN**2)/DBLE(ISAG)))/(DBLE(ISAG-1)))
                  SAGMEAN=SAGMEAN/DBLE(ISAG)
                  IF(SYSTEM1(6).EQ.1.0D0) THEN
                      SAGMIN=SAGMIN*25.4D0*1.0D3
                      SAGMAX=SAGMAX*25.4D0*1.0D3
                      SAGMEAN=SAGMEAN*25.4D0*1.0D3
                      SAGRMS=SAGRMS*25.4D0*1.0D3
                  END IF
                  IF(SYSTEM1(6).EQ.2.0D0) THEN
                      SAGMIN=SAGMIN*1.0D4
                      SAGMAX=SAGMAX*1.0D4
                      SAGMEAN=SAGMEAN*1.0D4
                      SAGRMS=SAGRMS*1.0D4
                  END IF
                  IF(SYSTEM1(6).EQ.3.0D0) THEN
                      SAGMIN=SAGMIN*1.0D3
                      SAGMAX=SAGMAX*1.0D3
                      SAGMEAN=SAGMEAN*1.0D3
                      SAGRMS=SAGRMS*1.0D3
                  END IF
                  IF(SYSTEM1(6).EQ.4.0D0) THEN
                      SAGMIN=SAGMIN*1.0D6
                      SAGMAX=SAGMAX*1.0D6
                      SAGMEAN=SAGMEAN*1.0D6
                      SAGRMS=SAGRMS*1.0D6
                  END IF
                  SAGPTOV=SAGMAX-SAGMIN
                  IF(DF5.EQ.1) THEN
                      WRITE(OUTLYNE,103) SAGMIN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,104) SAGMAX
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,105) SAGMEAN
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,106) SAGRMS
                      CALL SHOWIT(0)
                      WRITE(OUTLYNE,107) SAGPTOV
                      CALL SHOWIT(0)
                  END IF
                  REG(12)=SAGMEAN
                  REG(11)=SAGRMS
                  REG(10)=SAGPTOV
 103              FORMAT('MINIMUM SAG VALUE1 IN MICROMETER = ',G23.15)
 104              FORMAT('MAXIMUM SAG VALUE1 IN MICROMETER = ',G23.15)
 105              FORMAT('   MEAN SAG VALUE1 IN MICROMETER = ',G23.15)
 106              FORMAT('    RMS SAG VALUE1 IN MICROMETER = ',G23.15)
 107              FORMAT('    P-V SAG VALUE1 IN MICROMETER = ',G23.15)
                  RETURN
              ELSE
C     NOT 'FILE'
              END IF
          ELSE
C       INVALID QUALIFIER USED WITH SAG
              WRITE(OUTLYNE,*)
     1        'INVALID QUALIFIER WORD USED WITH THE "SAG" COMMAND'
              CALL SHOWIT(1)
              WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
 100      FORMAT('SAG DATA FOR SURFACE NUMBER ',I3)
! 101    FORMAT('ALL UNITS ARE ',A3)
 11       FORMAT(1X)
 200      FORMAT(4X,'X',12X,'Y',9X,'SURFACE SAG',5X,'L',12X,'M',12X,'N')
 300      FORMAT(G12.5,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5)
 301      FORMAT('  X = ',G13.6)
 302      FORMAT('  Y = ',G13.6)
 303      FORMAT('SAG = ',G13.6)
 304      FORMAT('  L = ',G13.6)
 305      FORMAT('  M = ',G13.6)
 306      FORMAT('  N = ',G13.6)
 23       FORMAT('UNITS = INCHES')
 24       FORMAT('UNITS = CENTIMETERS')
 25       FORMAT('UNITS = MILIMETERS')
 33       FORMAT('UNITS = METERS')
      END


      SUBROUTINE SAGARRAY(I,X,Y)
          IMPLICIT NONE
          REAL*8 X,Y,DX,DY,XWORKING,YWORKING,N_X,N_Y,SGNX,SGNY
          INTEGER I
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
          DX=ALENS(131,I)
          DY=ALENS(132,I)
          IF(ALENS(133,I).EQ.-1.0D0) THEN
C       ODD
              N_X=DBLE(NINT(X/DX))
              N_Y=DBLE(NINT(Y/DY))
              XWORKING=X-(N_X*DX)
              YWORKING=Y-(N_Y*DY)
              X=XWORKING
              Y=YWORKING
          END IF
          IF(ALENS(133,I).EQ.1.0D0) THEN
C       EVEN
              IF(X.EQ.0.0D0) THEN
                  SGNX=1.0D0
              ELSE
                  SGNX=X/DABS(X)
              END IF
              IF(Y.EQ.0.0D0) THEN
                  SGNY=1.0D0
              ELSE
                  SGNY=Y/DABS(Y)
              END IF
              N_X=(DBLE(INT(X/DX))*2.0D0)+SGNX
              N_Y=(DBLE(INT(Y/DY))*2.0D0)+SGNY
              XWORKING=X-(N_X*DX/2.0D0)
              YWORKING=Y-(N_Y*DY/2.0D0)
              X=XWORKING
              Y=YWORKING
          END IF
          RETURN
      END


C SUB SETBLNI.FOR
      SUBROUTINE SETBLNI
C
          IMPLICIT NONE
C
C       SETS CURRENT INSTRUCTION VALUE1S AND STATUS INDICATORS
C       TO BLANK
C
          CHARACTER COMMWD_JK*8,QUALWD_JK*8,STRING_JK*80,
     1    ANW_JK*140,ANW1_JK*23,ANW2_JK*23,ANW3_JK*23,
     2    ANW4_JK*23,ANW5_JK*23
C
          REAL*8 NW1_JK,NW2_JK,NW3_JK,NW4_JK,
     1    NW5_JK
C
          INTEGER STATNW_JK,STATN1_JK,STATN2_JK,STATN3_JK,
     1    STATN4_JK,STATN5_JK,STATCO_JK,STATC2_JK,
     2    STATBL_JK,STBLK2_JK,STATQL_JK,STATST_JK,SI_JK
C
          COMMON/CBLANK_JK/NW1_JK,NW2_JK,NW3_JK,NW4_JK
     1    ,NW5_JK,STATNW_JK,STATN1_JK,STATN2_JK,
     1    STATN3_JK,STATN4_JK,STATN5_JK,STATCO_JK,STATC2_JK,
     1    STATBL_JK,STBLK2_JK,STATQL_JK,
     2    STATST_JK,SI_JK
C
          COMMON/CCBLAN_JK/COMMWD_JK,QUALWD_JK,STRING_JK,
     1    ANW_JK,ANW1_JK,ANW2_JK,ANW3_JK,ANW4_JK,
     1    ANW5_JK
C
C
          INCLUDE 'datmai.inc'
C
          COMMWD_JK=BB
          QUALWD_JK=BB
          STRING_JK=AA//AA//AA//AA
          ANW_JK=AA//AA//AA//AA//AA//AA//AA
          ANW1_JK=AA//'   '
          ANW2_JK=AA//'   '
          ANW3_JK=AA//'   '
          ANW4_JK=AA//'   '
          ANW5_JK=AA//'   '
          NW1_JK=0.0D0
          NW2_JK=0.0D0
          NW3_JK=0.0D0
          NW4_JK=0.0D0
          NW5_JK=0.0D0
          STATNW_JK=0
          STATN1_JK=0
          STATN2_JK=0
          STATN3_JK=0
          STATN4_JK=0
          STATN5_JK=0
          STATQL_JK=0
          STATBL_JK=1
          STBLK2_JK=0
          STATCO_JK=0
          STATC2_JK=0
          STATST_JK=0
          SI_JK=0
          RETURN
      END
C SUB SETBLN_JK.FOR
      SUBROUTINE SETBLN_JK
C
          IMPLICIT NONE
C
C       SETS CURRENT INSTRUCTION VALUE1S AND STATUS INDICATORS
C       TO BLANK
C
          CHARACTER COMMWD_JK*8,QUALWD_JK*8,STRING_JK*80,
     1    ANW_JK*140,ANW1_JK*23,ANW2_JK*23,ANW3_JK*23,
     2    ANW4_JK*23,ANW5_JK*23
C
          REAL*8 NW1_JK,NW2_JK,NW3_JK,NW4_JK,
     1    NW5_JK
C
          INTEGER STATNW_JK,STATN1_JK,STATN2_JK,STATN3_JK,
     1    STATN4_JK,STATN5_JK,STATCO_JK,STATC2_JK,
     2    STATBL_JK,STBLK2_JK,STATQL_JK,STATST_JK,SI_JK
C
          COMMON/CBLANK_JK/NW1_JK,NW2_JK,NW3_JK,NW4_JK
     1    ,NW5_JK,STATNW_JK,STATN1_JK,STATN2_JK,
     1    STATN3_JK,STATN4_JK,STATN5_JK,STATCO_JK,STATC2_JK,
     1    STATBL_JK,STBLK2_JK,STATQL_JK,
     2    STATST_JK,SI_JK
C
          COMMON/CCBLAN_JK/COMMWD_JK,QUALWD_JK,STRING_JK,
     1    ANW_JK,ANW1_JK,ANW2_JK,ANW3_JK,ANW4_JK,
     1    ANW5_JK
C
          INCLUDE 'datmai.inc'
C
          COMMWD_JK=BB
          QUALWD_JK=BB
          STRING_JK=AA//AA//AA//AA
          ANW_JK=AA//AA//AA//AA//AA//AA//AA
          ANW1_JK=AA//'   '
          ANW2_JK=AA//'   '
          ANW3_JK=AA//'   '
          ANW4_JK=AA//'   '
          ANW5_JK=AA//'   '
          NW1_JK=0.0D0
          NW2_JK=0.0D0
          NW3_JK=0.0D0
          NW4_JK=0.0D0
          NW5_JK=0.0D0
          STATNW_JK=0
          STATN1_JK=0
          STATN2_JK=0
          STATN3_JK=0
          STATN4_JK=0
          STATN5_JK=0
          STATQL_JK=0
          STATBL_JK=1
          STBLK2_JK=0
          STATCO_JK=0
          STATC2_JK=0
          STATST_JK=0
          SI_JK=0
          RETURN
      END



C
      SUBROUTINE ATODFAST(REMAIN)
C**********************************************************************
C     THIS SUBROUTINE IS USED FOR CONVERTING CHARACTER REPRESENTATIONS
C     OF REAL*8 INPUT TO REAL*8 VALUE1S.
C
C     THE INPUT IS THE CHARACTER*140 VARIABLE "JK_ANW"
C
C     THE TOTAL NUMBER OF EXPECTED VALUE1S STORED IN INPUT
C     IS 5
C
C     RETURNED VALUE1S ARE:
C
C               JK_INP(1:5) = CHARACTER REPRESENTATIONS
C     OF EACH OF THE 5 VALUE1S
C
C               NUM(1:5) = REAL*8 VALUE1S
C
C               JK_DF(1:5) = 0 FOR EXPLICIT INPUT VALUE1
C                            1 FOR DEFAULT INPUT VALUE1
C                            2 FOR NO INPUT VALUE1
C
C              JK_FLG1(1:5) = .TRUE. FOR VALID INPUT
C                             .FALSE. FOR INVALID INPUT
C
C              JK_FLG2        = .TRUE. FOR 5 NUMERIC WORDS ONLY
C                             .FALSE. FOR INPUT BEYOND 5 NUMERIC WORDS
C
C     A VALUE1 IS INVALID IF IT DOES NOT REPRESENT A NUMERIC VALUE1
C
          IMPLICIT NONE
C
          INTEGER JK_DF(1:5)
C
          REAL*8 JK_NUM(1:5)
C
          CHARACTER JK_ANW*140,JK_INP(1:5)*80,REMAIN*140
C
          LOGICAL JK_FLG1(1:5),JK_FLG2,JK_ERROR

          COMMON/FAST1/JK_ANW,JK_INP
          CHARACTER RA(0:6)*140
          COMMON/FAST7/RA
          COMMON/FAST2/JK_NUM,JK_DF
          COMMON/FAST3/JK_FLG1,JK_FLG2
          COMMON/FAST4/JK_ERROR
C
          CALL BREAKO2(REMAIN)
C
          CALL ATONFAS
C
          RETURN
      END


C
      SUBROUTINE ATONFAS
C
          IMPLICIT NONE
C
          REAL*8 JK_NUM(1:5)
C
          INTEGER I,JK_DF(1:5)
C
          CHARACTER JK_ANW*140,JK_INP(1:5)*80,B*140
C
          LOGICAL JK_ERROR,JK_FLG2,JK_FLG1(1:5)
C
          COMMON/FAST1/JK_ANW,JK_INP
          CHARACTER RA(0:6)*140
          COMMON/FAST7/RA
C
          COMMON/FAST2/JK_NUM,JK_DF
C
          COMMON/FAST3/JK_FLG1,JK_FLG2
C
          COMMON/FAST4/JK_ERROR
C
          DO 30 I=1,5
              IF(.NOT.JK_FLG1(I)) THEN
C     EARLIER DETERMINATION OF A BAD INPUT VALUE1
                  JK_NUM(I)=0.0D0
              ELSE
C     VALUE1 WAS NOT PREVIOUSLY COUNTED AS BAD
                  IF(JK_DF(I).EQ.0) THEN
C
C     CONVERT TO REAL*8
                      WRITE(B,10) JK_INP(I)(1:80)
10                    FORMAT(A80)
                      READ(B,20,ERR=99999) JK_NUM(I)
20                    FORMAT(D23.15)
                      JK_ERROR=.FALSE.
                      JK_FLG1(I)=.TRUE.
                      GO TO 30
99999                 JK_ERROR=.TRUE.
                      JK_FLG1(I)=.FALSE.
                      JK_NUM(I)=0.0D0
                      JK_INP(I)='0.0'
                  ELSE
C     DEFAULT, SET TO 0
                      JK_NUM(I)=0.0D0
                      JK_FLG1(I)=.TRUE.
                      JK_INP(I)='0.0'
                  END IF
              END IF
30        CONTINUE
          RETURN
      END
C
      SUBROUTINE BREAKO2(REMAIN)
C
C     THIS ROUTINE TAKES A 140 CHARACTER VARIABLE NAMED "JK_ANW"
C     AND BREAKS IT INTO UP TO 5 CHARACTER VARIABLES WITH THE BREAKS
C     INDICATED BY A BLANK SPACE OR A COMMA.
C
C     THE RETURNED VARIABLES ARE RETURNED IN THE CHARACTER*80 ARRAY
C     JK_INP(1:20). IF A VALUE1 WAS RETURNED, THE FLAG JK_FLG(I) IS
C     SET TO .TRUE. ELSE IT IS LEFT AS FALSE.
C
          IMPLICIT NONE
C
          REAL*8 JK_NUM(1:5)
C
          INTEGER I,J,JK_DF(1:5),JL,JK_N
C
          LOGICAL JK_BLANK,JK_FLG1(1:5),JK_FLG2
C
          CHARACTER JK_ANW*140,REMAIN*140,JK_INP(1:5)*80,BLJK*80
C
          COMMON/FAST1/JK_ANW,JK_INP
          CHARACTER RA(0:6)*140
          COMMON/FAST7/RA
C
          COMMON/FAST2/JK_NUM,JK_DF
C
          COMMON/FAST3/JK_FLG1,JK_FLG2

          COMMON/FAST5/JK_BLANK
C
          COMMON/FAST6/JK_N
C
          INCLUDE 'datmai.inc'
C
C     INITIALIZE ALL VALUE1S
          BLJK=AA//AA//AA//AA
          JK_FLG2=.TRUE.
          DO I=1,5
C
C     JK_DF(I)=1 MEANS NO JK_ANW OR DEFAULT INPUT
              JK_DF(I)=1
C
              JK_FLG1(I)=.TRUE.
C
              DO J=1,80
                  JK_INP(I)(J:J)=' '
              END DO
          END DO
C
C     STRIP LEADING BLANKS IF ANY
          CALL STPLB2
          REMAIN(1:140)=JK_ANW(1:140)
          IF(JK_BLANK) GO TO 201
C
C     IF A NON-NUMERIC CHARACTER WAS FOUND, JUMP TO 201
          IF(REMAIN(1:1).NE.'+'.AND.
     1       REMAIN(1:1).NE.'-'.AND.
     1       REMAIN(1:1).NE.'0'.AND.
     1       REMAIN(1:1).NE.'1'.AND.
     1       REMAIN(1:1).NE.'2'.AND.
     1       REMAIN(1:1).NE.'3'.AND.
     1       REMAIN(1:1).NE.'4'.AND.
     1       REMAIN(1:1).NE.'5'.AND.
     1       REMAIN(1:1).NE.'6'.AND.
     1       REMAIN(1:1).NE.'7'.AND.
     1       REMAIN(1:1).NE.'8'.AND.
     1       REMAIN(1:1).NE.'9'.AND.
     1       REMAIN(1:1).NE.','.AND.
     1       REMAIN(1:1).NE.'.') GO TO 201
C
C
C     CASE OF EMBEDDED BLANK AS IN 2D 3 REPRESENTING 2D+3
          CALL INSRTP2
          REMAIN(1:140)=JK_ANW(1:140)
C
C     CASE OF MISSING D OR E IN 2-3 REPRESENTING 2D-3
          CALL INSRTD2
          REMAIN(1:140)=JK_ANW(1:140)
C
C     BREAK OUT VALUE1S
C
          RA(0)=JK_ANW(1:140)
          DO 200 J=1,5
C     STRIP LEADING BLANKS IF ANY
              CALL STPLB2
              REMAIN(1:140)=JK_ANW(1:140)
C
C     IF A NON-NUMERIC CHARACTER WAS FOUND, JUMP TO 201
              IF(REMAIN(1:1).NE.'+'.AND.
     1           REMAIN(1:1).NE.'-'.AND.
     1           REMAIN(1:1).NE.'0'.AND.
     1           REMAIN(1:1).NE.'1'.AND.
     1           REMAIN(1:1).NE.'2'.AND.
     1           REMAIN(1:1).NE.'3'.AND.
     1           REMAIN(1:1).NE.'4'.AND.
     1           REMAIN(1:1).NE.'5'.AND.
     1           REMAIN(1:1).NE.'6'.AND.
     1           REMAIN(1:1).NE.'7'.AND.
     1           REMAIN(1:1).NE.'8'.AND.
     1           REMAIN(1:1).NE.'9'.AND.
     1           REMAIN(1:1).NE.','.AND.
     1           REMAIN(1:1).NE.'.') GO TO 201
C
C     IS THE NEXT VALUE1 A DEFAULT JK_ANW VALUE1
              IF(JK_ANW(1:1).EQ.','.OR.JK_ANW(1:1).EQ.' '.OR.JK_BLANK) THEN
C     THE NEXT VALUE1 IS DEFALUT
                  JK_INP(J)(1:80)=BLJK
                  JK_INP(J)(1:23)=' 0.0000000000000000D+00'
                  JK_DF(J)=1
C     REBUILD JK_ANW WITHOUT COMMA
                  JK_ANW(1:140)=JK_ANW(2:140)
                  REMAIN(1:140)=JK_ANW(1:140)
                  RA(J)=REMAIN(1:140)
                  GO TO 200
              ELSE
C     NEXT VALUE1 IS NOT DEFAULT, BREAK IT OUT
C
                  DO I=2,139
                      IF(JK_ANW(I:I).EQ.' '.OR.JK_ANW(I:I).EQ.',') THEN
                          JK_INP(J)(1:80)=BLJK
                          JK_INP(J)(1:80)=JK_ANW(1:I-1)
                          JK_DF(J)=0
                          REMAIN(1:140)=JK_ANW(I+1:140)
                          JK_ANW(1:140)=JK_ANW(I+1:140)
                          RA(J)=REMAIN(1:140)
                          GO TO 200
                      END IF
                  END DO
C
              END IF
C
200       CONTINUE
C
201       CONTINUE
C
C     ALL 5 NUMERIC WORDS ARE BROKEN OUT. IS THERE ANYTHING LEFT
C     NON-BLANK IN JK_ANW(REMAIN).IF SO,SET JK_FLG2 TO FALSE AND RETURN
C     ANYTHING, THAT IS, BEYOND ONE TRAILING COMMA
          DO JL=1,140
              IF(REMAIN(JL:JL).NE.' ') THEN
                  JK_FLG2=.FALSE.
              END IF
          END DO
C
          DO J=1,5
C     IF A JK_INP VALUE1 IS MISSING A DECIMAL POINT, ADD ONE
              JK_N=J
              CALL ADDDE2
          END DO
          DO J=1,5
              IF(JK_FLG1(J)) THEN
C     IF A JK_INP HAS A "D" OR "E", CHECK THE EXPONENT SIZE
                  JK_N=J
                  CALL DESIZ2
C     NUMBER ALREADY NAN
              END IF
          END DO
C
          DO J=1,5
              IF(JK_FLG1(J)) THEN
                  IF(JK_INP(J)(1:1).NE.'(') THEN
C     JK_INP(J) DOES NOT START WITH A LEFT PARENTHESIS
                      IF(JK_INP(J)(24:24).NE.' ') THEN
C     JK_INP(J) IS TOO LONG
                          JK_FLG1(J)=.FALSE.
                          RETURN
C     LENGTH OK
                      END IF
C     PARENTHESIS IS THERE
                  END IF
              END IF
          END DO
C
          RETURN
      END
      SUBROUTINE STPLB2
C**********************************************************************
C     SRIP LEADING BLANKS ROUTINE
C**********************************************************************
C
          IMPLICIT NONE
C
C     STRIPS OFF LEADING BLANKS FROM INPUT*140
C
          CHARACTER JK_ANW*140,BL*140,JK_INP(1:5)*80
C
          LOGICAL JK_BLANK
C
          INTEGER I
C
          COMMON/FAST1/JK_ANW,JK_INP
          CHARACTER RA(0:6)*140
          COMMON/FAST7/RA
C
          COMMON/FAST5/JK_BLANK
C
          JK_BLANK=.FALSE.
          DO I=1,140
              BL(I:I)=' '
          END DO
C
          I=0
1         IF(ICHAR(JK_ANW(1:1)).EQ.32) THEN
              JK_ANW(1:140)=JK_ANW(2:140)
              IF(JK_ANW(1:140).EQ.BL(1:140)) THEN
                  JK_BLANK=.TRUE.
                  RETURN
C     PROCEED
              END IF
              I=I+1
              IF(I.LT.140) GO TO 1
              IF(I.GE.140) THEN
                  JK_BLANK=.TRUE.
                  RETURN
              END IF
C     ALL LEADING BLANKS REMOVED
          END IF
          RETURN
      END



      SUBROUTINE INSRTP2
C**********************************************************************
C     ADD MISSING PLUS SIGN ROUTINE
C**********************************************************************
C
          IMPLICIT NONE
C
          CHARACTER JK_ANW*140,JK_INP(1:5)*80
C
          COMMON/FAST1/JK_ANW,JK_INP
          CHARACTER RA(0:6)*140
          COMMON/FAST7/RA
C
          INTEGER I
C
C     CASE OF EMBEDDED BLANK AS IN 2D 3 REPRESENTING 2D+3
C     IF FORMS LIKE 2D 3 OR 3E 5, THE SPACE MUST BE REPLACED
C     WITH A PLUS SIGN
          DO I=1,138
              IF(JK_ANW(I:I).EQ.'D'.OR.JK_ANW(I:I).EQ.'E') THEN
                  IF(JK_ANW(I+1:I+1).EQ.' '.AND.JK_ANW(I+2:I+2).NE.' ') THEN
                      JK_ANW(I+1:I+1)='+'
                  END IF
              END IF
          END DO
          RETURN
      END
      SUBROUTINE INSRTD2
C**********************************************************************
C     ADD MISSING "D" ROUTINE
C**********************************************************************
C
          IMPLICIT NONE
C
          CHARACTER JK_ANW*140,JK_INP(1:5)*80
C
          COMMON/FAST1/JK_ANW,JK_INP
          CHARACTER RA(0:6)*140
          COMMON/FAST7/RA
C
          INTEGER I
C
C     CASE OF MISSING "D" AS IN 2-3 REPRESENTING 2D-3
          DO I=1,138
              IF(JK_ANW(I:I).EQ.'0'.OR.JK_ANW(I:I).EQ.'1'.OR.
     1        JK_ANW(I:I).EQ.'2'.OR.JK_ANW(I:I).EQ.'3'.OR.
     1        JK_ANW(I:I).EQ.'4'.OR.JK_ANW(I:I).EQ.'5'.OR.
     1        JK_ANW(I:I).EQ.'6'.OR.JK_ANW(I:I).EQ.'7'.OR.
     1        JK_ANW(I:I).EQ.'8'.OR.JK_ANW(I:I).EQ.'9'.OR.
     1        JK_ANW(I:I).EQ.'10') THEN
                  IF(JK_ANW(I+1:I+1).EQ.'-'.OR.JK_ANW(I+1:I+1).EQ.'+') THEN
                      JK_ANW(1:140)=JK_ANW(1:I)//'D'//JK_ANW(I+1:140)
                  END IF
              END IF
          END DO
          RETURN
      END


      SUBROUTINE ADDDE2
C**********************************************************************
C     DECIMAL ADDIN ROUTINE
C**********************************************************************
C
          IMPLICIT NONE
C
          CHARACTER JK_INP(1:5)*80,JK_ANW*140,BLNK80*80,AA20*20
C
          LOGICAL DEE,JK_FLG1(1:5),JK_FLG2
C
          INTEGER I,DEEPOS,JK_N
C
          COMMON/FAST1/JK_ANW,JK_INP
          CHARACTER RA(0:6)*140
          COMMON/FAST7/RA
C
          COMMON/FAST3/JK_FLG1,JK_FLG2
C
          COMMON/FAST6/JK_N
C
          AA20='                    '
          BLNK80=AA20//AA20//AA20//AA20
C
          DEE=.FALSE.
C
          DO I=1,80
              IF(JK_INP(JK_N)(I:I).EQ.'.') THEN
C     DECIMAL PRESENT,RETURN
                  JK_FLG1(JK_N)=.TRUE.
                  RETURN
C     PROCEED
              END IF
C     DECIMAL NOT PRESENT
          END DO
C
          DO I=1,80
              IF(JK_INP(JK_N)(I:I).EQ.'D'.OR.JK_INP(JK_N)(I:I).EQ.'E') THEN
                  DEE=.TRUE.
                  DEEPOS=I
                  GO TO 20
C     KEEP LOOKING
              END IF
          END DO
20        CONTINUE
C
          IF(DEE) THEN
C     EXPONENT THERE
              IF(DEEPOS.NE.1) THEN
                  JK_INP(JK_N)(1:80)=JK_INP(JK_N)(1:DEEPOS-1)//
     1            '.0'//JK_INP(JK_N)(DEEPOS:78)
                  JK_FLG1(JK_N)=.TRUE.
              ELSE
C     DEEPOS=1
                  JK_INP(JK_N)(1:80)=BLNK80
                  JK_INP(JK_N)(1:3)='0.0'
                  JK_FLG1(JK_N)=.FALSE.
              END IF
              RETURN
C     NO EXPONENT
          END IF
          DO I=1,78
              IF(JK_INP(JK_N)(I:I).EQ.' ') THEN
                  IF(I.EQ.1) THEN
                      JK_INP(JK_N)(1:80)=BLNK80
                      JK_INP(JK_N)(1:3)='0.0'
                  ELSE
C     I NO1 1
                      JK_INP(JK_N)(1:80)=JK_INP(JK_N)(1:I-1)//'.0'
                  END IF
                  GO TO 40
              END IF
          END DO
40        CONTINUE
          RETURN
      END


      SUBROUTINE DESIZ2
C**********************************************************************
C     EXPONENT TOO BIG ROUTINE
C**********************************************************************
C
          IMPLICIT NONE
C
          CHARACTER JK_INP(1:5)*80,JK_ANW*140,CHEXP*80,B*140
C
          LOGICAL DEE,JK_FLG1(1:5),JK_FLG2
C
          REAL*8 NUMEXP
C
          INTEGER I,DEEPOS,JK_N

          COMMON/FAST1/JK_ANW,JK_INP
          CHARACTER RA(0:6)*140
          COMMON/FAST7/RA
C
          COMMON/FAST3/JK_FLG1,JK_FLG2
C
          COMMON/FAST6/JK_N
C
          DEE=.FALSE.
C
          DO I=1,80
              IF(JK_INP(JK_N)(I:I).EQ.'D'.OR.JK_INP(JK_N)(I:I).EQ.'E') THEN
                  DEE=.TRUE.
                  DEEPOS=I
                  GO TO 20
C     KEEP LOOKING
              END IF
          END DO
20        CONTINUE
C
          IF(DEE) THEN
              CHEXP(1:80)=JK_INP(JK_N)(DEEPOS+1:80)
              DO I=2,78
                  IF(CHEXP(I:I).EQ.' ') THEN
                      CHEXP(1:80)=CHEXP(1:I-1)//'.0'
                      GO TO 500
                  END IF
              END DO
500           CONTINUE
              WRITE(B,100) CHEXP
              READ(B,200,ERR=222) NUMEXP
100           FORMAT(A80)
200           FORMAT(D23.15)
              IF(DABS(NUMEXP).GT.300.0D0) THEN
                  JK_INP(JK_N)='0.0'
                  JK_FLG1(JK_N)=.FALSE.
                  RETURN
              ELSE
C     EXPONENT OK
                  JK_FLG1(JK_N)=.TRUE.
              END IF
          ELSE
              JK_FLG1(JK_N)=.TRUE.
C     NO D OR E, RETURN
          END IF
          RETURN
222       CONTINUE
C     CASE OF EXPONENT NOT A NUMBER
          JK_INP(JK_N)='0.0'
          JK_FLG1(JK_N)=.FALSE.
          RETURN
      END



C SUB PRO0.FOR
      SUBROUTINE PRO0
C
          IMPLICIT NONE
C
C       IF A COLON FOLLOWS ANYTHING EXCEPT A BANK OR A COMMA, ADD A COMMA
C     INFRONT OF THE COLON
C
C                       DEFINE VARIABLES
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
          INTEGER I
C
          INCLUDE 'datmai.inc'
C
 10       CONTINUE
          DO I=2,140
              IF(INPUT(I:I).EQ.':') THEN
                  IF(INPUT(I-1:I-1).NE.','.AND.INPUT(I-1:I-1).NE.' ') THEN
C     ADD A COMMA
                      INPUT(1:140)=INPUT(1:I-1)//','//INPUT(I:139)
                      GO TO 10
C     CONTINUE, A COMMA OR SPACE WAS FOUND
                  END IF
C     NOT A COLON, CONTINUE
              END IF
          END DO
C
          RETURN
      END



C SUB PRO4.FOR
      SUBROUTINE PRO4
C
          IMPLICIT NONE
C
C       IF A COLON FOLLOWS ANYTHING EXCEPT A BANK OR A COMMA, ADD A COMMA
C     INFRONT OF THE COLON
C
C                       DEFINE VARIABLES
C
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
          INTEGER I
          INCLUDE 'datmai.inc'
C
 10       CONTINUE
          DO I=2,80
              IF(WS(I:I).EQ.':') THEN
                  IF(WS(I-1:I-1).EQ.',') THEN
C     REMOVE THE COMMA
                      IF(I.EQ.2) THEN
                          WS(1:80)=WS(2:80)
                          GO TO 10
                      ELSE
C     I NOT 2
                          WS(1:80)=WS(1:I-2)//WS(I:80)
                          GO TO 10
                      END IF
C     CONTINUE
                  END IF
C     NOT A COLON, CONTINUE
              END IF
          END DO
C
          RETURN
      END



C SUB SETBLN.FOR
      SUBROUTINE SETBLN
C
          IMPLICIT NONE
C
C       SETS CURRENT INSTRUCTION VALUE1S AND STATUS INDICATORS
C       TO BLANK
C
          CHARACTER COMMWD(1:20)*8,QUALWD(1:20)*8,STRING(1:20)*140,
     1    ANW(1:20)*140,ANW1(1:20)*23,ANW2(1:20)*23,ANW3(1:20)*23,
     2    ANW4(1:20)*23,ANW5(1:20)*23
C
          REAL*8 NW1(1:20),NW2(1:20),NW3(1:20),NW4(1:20),
     1    NW5(1:20)
C
          INTEGER STATNW(1:20),STATN1(1:20),STATN2(1:20),STATN3(1:20),
     1    STATN4(1:20),STATN5(1:20),STATCO(1:20),STATC2(1:20),
     2    STATBL(1:20),STBLK2(1:20),STATQL(1:20),STATST(1:20),SI(1:20),
     3    BI
C
          COMMON/CBLANK/NW1,NW2,NW3,NW4,NW5,STATNW,STATN1,STATN2,
     1    STATN3,STATN4,STATN5,STATCO,STATC2,STATBL,STBLK2,STATQL,
     2    STATST,SI,BI
C
          COMMON/CCBLAN/COMMWD,QUALWD,STRING,ANW,ANW1,ANW2,ANW3,ANW4,
     1    ANW5
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
C
          INCLUDE 'datmai.inc'
C
          COMMWD(BI)=BB
          QUALWD(BI)=BB
          STRING(BI)=AA//AA//AA//AA
          ANW(BI)=AA//AA//AA//AA//AA//AA//AA
          ANW1(BI)=AA//'   '
          ANW2(BI)=AA//'   '
          ANW3(BI)=AA//'   '
          ANW4(BI)=AA//'   '
          ANW5(BI)=AA//'   '
          NW1(BI)=0.0D0
          NW2(BI)=0.0D0
          NW3(BI)=0.0D0
          NW4(BI)=0.0D0
          NW5(BI)=0.0D0
          STATNW(BI)=0
          STATN1(BI)=0
          STATN2(BI)=0
          STATN3(BI)=0
          STATN4(BI)=0
          STATN5(BI)=0
          STATQL(BI)=0
          STATBL(BI)=1
          STBLK2(BI)=0
          STATCO(BI)=0
          STATC2(BI)=0
          STATST(BI)=0
          SI(BI)=0
          RETURN
      END



      SUBROUTINE ATOD(INPUT,INP,NUM,DF,FLG1,INUM,FLG2,REMAIN)
C**********************************************************************
C     THIS SUBROUTINE IS USED FOR CONVERTING CHARACTER REPRESENTATIONS
C     OF REAL*8 INPUT TO REAL*8 VALUE1S.
C
C     THE INPUT IS THE CHARACTER*140 VARIABLE "INPUT"
C
C     THE TOTAL NUMBER OF EXPECTED VALUE1S STORED IN INPUT
C     IS DESIGNATED BY THE INTEGER VALUE1 "INUM".
C
C     RETURNED VALUE1S ARE:
C
C               INP(1:INUM) = CHARACTER REPRESENTATIONS
C     OF EACH OF THE INUM VALUE1S
C
C               NUM(1:INUM) = REAL*8 VALUE1S
C
C               DF(1:INUM) = 0 FOR EXPLICIT INPUT VALUE1
C                            1 FOR DEFAULT INPUT VALUE1
C                            2 FOR NO INPUT VALUE1
C
C               FLG1(1:INUM) = .TRUE. FOR VALID INPUT
C                             .FALSE. FOR INVALID INPUT
C
C               FLG2        = .TRUE. FOR 5 NUMERIC WORDS ONLY
C                             .FALSE. FOR INPUT BEYOND 5 NUMERIC WORDS
C
C     A VALUE1 IS INVALID IF IT DOES NOT REPRESENT A NUMERIC VALUE1
C
          IMPLICIT NONE
C
          INTEGER DF(*),INUM
C
          REAL*8 NUM(*)
C
          CHARACTER INPUT*140,INP(*)*80,REMAIN*140
C
          LOGICAL FLG1(*),FLG2,ERROR
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
C
          CALL BREAKOUT(INPUT,INP,DF,FLG1,INUM,FLG2,REMAIN)
C
          CALL ATON(INP,FLG1,NUM,ERROR,DF,INUM)
C
          RETURN
      END



C
      SUBROUTINE BREAKOUT(INPUT,INP,DF,FLG1,INUM,FLG2,REMAIN)
C
C     THIS ROUTINE TAKES A 140 CHARACTER VARIABLE NAMED "INPUT"
C     AND BREAKS IT INTO UP TO INUM CHARACTER VARIABLES WITH THE BREAKS
C     INDICATED BY A BLANK SPACE OR A COMMA.
C
C     THE RETURNED VARIABLES ARE RETURNED IN THE CHARACTER*80 ARRAY
C     INP(1:20). IF A VALUE1 WAS RETURNED, THE FLAG FLG(I) IS
C     SET TO .TRUE. ELSE IT IS LEFT AS FALSE.
C
          IMPLICIT NONE
C
          INTEGER I,J,DF(*),INUM,JL
C
          LOGICAL BLANK,FLG1(*),FLG2
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
C
          CHARACTER INPUT*140,REMAIN*140,INP(*)*80
C
C     INITIALIZE ALL VALUE1S
          FLG2=.TRUE.
          DO I=1,INUM
C
C     DF(I)=1 MEANS NO INPUT OR DEFAULT INPUT
              DF(I)=1
C
              FLG1(I)=.TRUE.
C
              DO J=1,80
                  INP(I)(J:J)=' '
              END DO
          END DO
C
C     STRIP LEADING BLANKS IF ANY
          CALL STPLBL(INPUT,BLANK)
          REMAIN(1:140)=INPUT(1:140)
          IF(BLANK) GO TO 201
C     IF A NON-NUMERIC CHARACTER WAS FOUND, JUMP TO 201
          IF(REMAIN(1:1).NE.'+'.AND.
     1       REMAIN(1:1).NE.'-'.AND.
     1       REMAIN(1:1).NE.'0'.AND.
     1       REMAIN(1:1).NE.'1'.AND.
     1       REMAIN(1:1).NE.'2'.AND.
     1       REMAIN(1:1).NE.'3'.AND.
     1       REMAIN(1:1).NE.'4'.AND.
     1       REMAIN(1:1).NE.'5'.AND.
     1       REMAIN(1:1).NE.'6'.AND.
     1       REMAIN(1:1).NE.'7'.AND.
     1       REMAIN(1:1).NE.'8'.AND.
     1       REMAIN(1:1).NE.'9'.AND.
     1       REMAIN(1:1).NE.','.AND.
     1       REMAIN(1:1).NE.'.') GO TO 201
C
C     CASE OF EMBEDDED BLANK AS IN 2D 3 REPRESENTING 2D+3
          CALL INSRTP(INPUT)
          REMAIN(1:140)=INPUT(1:140)
C
C     CASE OF MISSING D OR E IN 2-3 REPRESENTING 2D-3
          CALL INSRTD(INPUT)
          REMAIN(1:140)=INPUT(1:140)
C
C     BREAK OUT VALUE1S
C
          RA(0)=INPUT(1:140)
          DO 200 J=1,INUM
C     STRIP LEADING BLANKS IF ANY
              CALL STPLBL(INPUT,BLANK)
              IF(BLANK) GO TO 200
              REMAIN(1:140)=INPUT(1:140)
C
C     IF A NON-NUMERIC CHARACTER WAS FOUND, JUMP TO 201
              IF(REMAIN(1:1).NE.'+'.AND.
     1           REMAIN(1:1).NE.'-'.AND.
     1           REMAIN(1:1).NE.'0'.AND.
     1           REMAIN(1:1).NE.'1'.AND.
     1           REMAIN(1:1).NE.'2'.AND.
     1           REMAIN(1:1).NE.'3'.AND.
     1           REMAIN(1:1).NE.'4'.AND.
     1           REMAIN(1:1).NE.'5'.AND.
     1           REMAIN(1:1).NE.'6'.AND.
     1           REMAIN(1:1).NE.'7'.AND.
     1           REMAIN(1:1).NE.'8'.AND.
     1           REMAIN(1:1).NE.'9'.AND.
     1           REMAIN(1:1).NE.','.AND.
     1           REMAIN(1:1).NE.'.') GO TO 201
C
C     IS THE NEXT VALUE1 A DEFAULT INPUT VALUE1
              IF(INPUT(1:1).EQ.',') THEN
C     THE NEXT VALUE1 IS DEFALUT
                  INP(J)(1:23)='+0.0000000000000000D000'
                  DF(J)=1
C     REBUILD INPUT WITHOUT COMMA
                  INPUT(1:140)=INPUT(2:140)
                  REMAIN(1:140)=INPUT(1:140)
                  RA(J)=REMAIN(1:140)
                  GO TO 200
              ELSE
C     NEXT VALUE1 IS NOT DEFAULT, BREAK IT OUT
C
                  DO I=2,139
                      IF(INPUT(I:I).EQ.' '.OR.INPUT(I:I).EQ.',') THEN
                          INP(J)=INPUT(1:I-1)
                          DF(J)=0
                          REMAIN(1:140)=INPUT(I+1:140)
                          RA(J)=REMAIN(1:140)
                          INPUT(1:140)=INPUT(I+1:140)
                          GO TO 200
                      END IF
                  END DO
              END IF
C
200       CONTINUE
201       CONTINUE
C
C     ALL 5 NUMERIC WORDS ARE BROKEN OUT. IS THERE ANYTHING LEFT
C     NON-BLANK IN INPUT(REMAIN).IF SO,SET FLG2 TO FALSE AND RETURN
C     ANYTHING, THAT IS, BEYOND ONE TRAILING COMMA
          DO JL=1,140
              IF(REMAIN(JL:JL).NE.' ') THEN
                  FLG2=.FALSE.
              END IF
          END DO
C
          DO J=1,INUM
C     IF A INP VALUE1 IS MISSING A DECIMAL POINT, ADD ONE
              CALL ADDDEC(INP(J),FLG1(J))
          END DO
          DO J=1,INUM
              IF(FLG1(J)) THEN
C     IF A INP HAS A "D" OR "E", CHECK THE EXPONENT SIZE
                  CALL DESIZE(INP(J),FLG1(J))
C     NUMBER ALREADY NAN
              END IF
          END DO
C
          DO J=1,INUM
              IF(FLG1(J)) THEN
                  IF(INP(J)(1:1).NE.'(') THEN
C     INP(J) DOES NOT START WITH A LEFT PARENTHESIS
                      IF(INP(J)(24:24).NE.' ') THEN
C     INP(J) IS TOO LONG
                          FLG1(J)=.FALSE.
                          RETURN
C     LENGTH OK
                      END IF
C     PARENTHESIS IS THERE
                  END IF
              END IF
          END DO
C
          RETURN
      END



      SUBROUTINE STPLBL(INPUT,BLANK)
C**********************************************************************
C     SRIP LEADING BLANKS ROUTINE
C**********************************************************************
C
          IMPLICIT NONE
C
C     STRIPS OFF LEADING BLANKS FROM INPUT*140
C
          CHARACTER INPUT*140,BL*140
C
          LOGICAL BLANK
C
          INTEGER I
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
C
          BLANK=.FALSE.
          DO I=1,140
              BL(I:I)=' '
          END DO
C
          I=0
1         IF(ICHAR(INPUT(1:1)).EQ.32) THEN
              INPUT(1:140)=INPUT(2:140)
              IF(INPUT(1:140).EQ.BL(1:140)) THEN
                  BLANK=.TRUE.
                  RETURN
C     PROCEED
              END IF
              I=I+1
              IF(I.LT.140) GO TO 1
              IF(I.GE.140) THEN
                  BLANK=.TRUE.
                  RETURN
              END IF
C     ALL LEADING BLANKS REMOVED
          END IF
          RETURN
      END

      SUBROUTINE STPCOM(INPUT)
C**********************************************************************
C     STRIP LEADING COMMA ROUTINE
C**********************************************************************
C
          IMPLICIT NONE
C
C     STRIPS OFF ONE LEADING COMMA FROM INPUT*140
C     STRIP ONE LEADING COMMA IF PRESENT ROUTINE
C     IF THERE IS ONE, ELSE DOES NOTHING
C
          CHARACTER INPUT*140
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
C
          IF(ICHAR(INPUT(1:1)).EQ.44) THEN
              INPUT(1:140)=INPUT(2:140)
              RETURN
C     NO COMMA TO BE REMOVED
          END IF
          RETURN
      END
      SUBROUTINE INSRTP(INPUT)
C**********************************************************************
C     ADD MISSING PLUS SIGN ROUTINE
C**********************************************************************
C
          IMPLICIT NONE
C
          CHARACTER INPUT*140
C
          INTEGER I
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
C
C     CASE OF EMBEDDED BLANK AS IN 2D 3 REPRESENTING 2D+3
C     IF FORMS LIKE 2D 3 OR 3E 5, THE SPACE MUST BE REPLACED
C     WITH A PLUS SIGN
          DO I=1,138
              IF(INPUT(I:I).EQ.'D'.OR.INPUT(I:I).EQ.'E') THEN
                  IF(INPUT(I+1:I+1).EQ.' '.AND.INPUT(I+2:I+2).NE.' ') THEN
                      INPUT(I+1:I+1)='+'
                  END IF
              END IF
          END DO
          RETURN
      END


      SUBROUTINE INSRTD(INPUT)
C**********************************************************************
C     ADD MISSING "D" ROUTINE
C**********************************************************************
C
          IMPLICIT NONE
C
          CHARACTER INPUT*140
C
          INTEGER I
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
C
C     CASE OF MISSING "D" AS IN 2-3 REPRESENTING 2D-3
          DO I=1,138
              IF(INPUT(I:I).EQ.'0'.OR.INPUT(I:I).EQ.'1'.OR.
     1        INPUT(I:I).EQ.'2'.OR.INPUT(I:I).EQ.'3'.OR.
     1        INPUT(I:I).EQ.'4'.OR.INPUT(I:I).EQ.'5'.OR.
     1        INPUT(I:I).EQ.'6'.OR.INPUT(I:I).EQ.'7'.OR.
     1        INPUT(I:I).EQ.'8'.OR.INPUT(I:I).EQ.'9'.OR.
     1        INPUT(I:I).EQ.'10') THEN
                  IF(INPUT(I+1:I+1).EQ.'-'.OR.INPUT(I+1:I+1).EQ.'+') THEN
                      INPUT(1:140)=INPUT(1:I)//'D'//INPUT(I+1:140)
                  END IF
              END IF
          END DO
          RETURN
      END
      SUBROUTINE DESIZE(INP1,FLG1)
C**********************************************************************
C     EXPONENT TOO BIG ROUTINE
C**********************************************************************
C
          IMPLICIT NONE
C
          CHARACTER INP1*80,CHEXP*80,B*140
C
          LOGICAL DEE,FLG1
C
          REAL*8 NUMEXP
C
          INTEGER I,DEEPOS
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
C
          DEE=.FALSE.
C
          DO I=1,80
              IF(INP1(I:I).EQ.'D'.OR.INP1(I:I).EQ.'E') THEN
                  DEE=.TRUE.
                  DEEPOS=I
                  GO TO 20
C     KEEP LOOKING
              END IF
          END DO
20        CONTINUE
C
          IF(DEE) THEN
              CHEXP(1:80)=INP1(DEEPOS+1:80)
              DO I=2,78
                  IF(CHEXP(I:I).EQ.' ') THEN
                      CHEXP(1:80)=CHEXP(1:I-1)//'.0'
                      GO TO 500
                  END IF
              END DO
500           CONTINUE
              WRITE(B,100) CHEXP
              READ(B,200,ERR=222) NUMEXP
100           FORMAT(A80)
200           FORMAT(D23.15)
              IF(DABS(NUMEXP).GT.300.0D0) THEN
                  INP1='0.0'
                  FLG1=.FALSE.
                  RETURN
              ELSE
C     EXPONENT OK
                  FLG1=.TRUE.
              END IF
          ELSE
              FLG1=.TRUE.
C     NO D OR E, RETURN
          END IF
          RETURN
222       CONTINUE
C     CASE OF EXPONENT NOT A NUMBER
          INP1='0.0'
          FLG1=.FALSE.
          RETURN
      END
      SUBROUTINE ADDDEC(INP1,FLG1)
C**********************************************************************
C     DECIMAL ADDIN ROUTINE
C**********************************************************************
C
          IMPLICIT NONE
C
          CHARACTER INP1*80,BLNK80*80,AA20*20
C
          LOGICAL DEE,FLG1
C
          INTEGER I,DEEPOS
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
C
          AA20='                    '
          BLNK80=AA20//AA20//AA20//AA20
C
          DEE=.FALSE.
C
          DO I=1,80
              IF(INP1(I:I).EQ.'.') THEN
C     DECIMAL PRESENT,RETURN
                  FLG1=.TRUE.
                  RETURN
C     PROCEED
              END IF
C     DECIMAL NOT PRESENT
          END DO
C
          DO I=1,80
              IF(INP1(I:I).EQ.'D'.OR.INP1(I:I).EQ.'E') THEN
                  DEE=.TRUE.
                  DEEPOS=I
                  GO TO 20
C     KEEP LOOKING
              END IF
          END DO
20        CONTINUE
C
          IF(DEE) THEN
C     EXPONENT THERE
              IF(DEEPOS.NE.1) THEN
                  INP1(1:80)=INP1(1:DEEPOS-1)//'.0'//INP1(DEEPOS:78)
                  FLG1=.TRUE.
              ELSE
C     DEEPOS=1
                  INP1(1:80)=BLNK80
                  INP1(1:3)='0.0'
                  FLG1=.FALSE.
              END IF
              RETURN
C     NO EXPONENT
          END IF
          DO I=1,78
              IF(INP1(I:I).EQ.' ') THEN
                  IF(I.EQ.1) THEN
                      INP1(1:80)=BLNK80
                      INP1(1:3)='0.0'
                  ELSE
C     I NO1 1
                      INP1(1:80)=INP1(1:I-1)//'.0'
                  END IF
                  GO TO 40
              END IF
          END DO
40        CONTINUE
          RETURN
      END



      SUBROUTINE ATON(INP,FLG1,NUM,ERROR,DF,INUM)
C
          IMPLICIT NONE
C
          REAL*8 NUM(*)
C
          INTEGER I,DF(*),INUM
C
          CHARACTER INP(*)*80,B*140
C
          LOGICAL ERROR,FLG1(*)
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
C
          DO 30 I=1,INUM
              IF(.NOT.FLG1(I)) THEN
C     EARLIER DETERMINATION OF A BAD INPUT VALE
                  NUM(I)=0.0D0
              ELSE
C     VALUE1 WAS NOT PREVIOUSLY COUNTED AS BAD
                  IF(DF(I).EQ.0) THEN
C
C     CONVERT TO REAL*8
                      WRITE(B,10) INP(I)(1:80)
10                    FORMAT(A80)
                      READ(B,20,ERR=99999) NUM(I)
20                    FORMAT(D23.15)
                      ERROR=.FALSE.
                      FLG1(I)=.TRUE.
                      GO TO 30
99999                 ERROR=.TRUE.
                      FLG1(I)=.FALSE.
                      NUM(I)=0.0D0
                      INP(I)='0.0'
                  ELSE
C     DEFAULT, SET TO 0
                      NUM(I)=0.0D0
                      FLG1(I)=.TRUE.
                      INP(I)='0.0'
                  END IF
              END IF
30        CONTINUE
          RETURN
      END


C SUB PROCES.FOR
      SUBROUTINE PROCES
          USE GLOBALS
C
          IMPLICIT NONE
C
C               THIS SUBROUTINE PROCESSES THE 140 CHARACTER
C               INPUT LINE INTO UP TO 20 PROGRAM COMMANDS
C
C                       DEFINE VARIABLES
C
          CHARACTER INSTRC(1:20)*140,OLYNE*140
C
          INTEGER NSTRUC,J
C
          COMMON/PRO22/INSTRC,NSTRUC

C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
          INCLUDE 'datmai.inc'

          NUMCOM=1
          IF(MULTICOM) NUMCOM=20
C
          LASTCOMWRD=WC
          LASTWASFOB=.FALSE.
          IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
C
C     THIS ALLOWS RTG? TO BE READ AS RTG ?
 101      CONTINUE
          DO J=2,140
              IF(INPUT(J:J).EQ.'?'.AND.INPUT(J-1:J-1).NE.' ') THEN
                  INPUT(1:140)=INPUT(1:J-1)//' '//INPUT(J:139)
                  GO TO 101
              END IF
          END DO
C       PRO0 ADDS A COMMA BETWEEN ANY CHARACTER AND A COLON
C       IF THE CHARACTER IS NOT A BLANK OR A COMMA  2/14/94
          CALL PRO0
C
C     TAKE OUT SPACES INFRONT OF A COMMA
          OLYNE(1:140)=INPUT(1:140)
          CALL NOBLANK(OLYNE)
          INPUT(1:140)=OLYNE(1:140)
C
C       PRO2 DOES VIRTUAL CARRIAGE RETURNS
          CALL PRO2

C       PRO3 FINISHES PROCESSING INPUT INTO PROGRAM INSTRUCTIONS
          CALL PRO3
          RETURN
      END



C SUB PRO2.FOR
      SUBROUTINE PRO2
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE PROCESSES THE STRING VARIABLE "INPUT"
C       BY REMOVING UP TO 20 VIRTUAL CARRIAGE RETURNS AND RETURNING
C       UP TO 20 INSTRUCTION LINES IN THE ARRAY INSTRUC. THE NUMBER
C       NON-BLANK INSTRUCTIONS IS PASSED VIA NSTRUC. THE CHARACTER
C     USED FOR REPRESENTING A VIRTUAL CARRIAGE RETURN IS DESIGNATED
C     BY ITS ASCII VALUE1. THIS VALUE1 IS PASSED BY C_VAL, AN INTEGER.
C     THE DEFAULT VALUE1 FOR C_VAL IS (59), THIS IS THE ASCII VALUE1
C     OF THE SEMICOLON (;). THIS IS THE WAY CODE V DOES IT.
C                       DEFINE VARIABLES
C
          CHARACTER INSTRC(1:20)*140,STRUC*140
C
          CHARACTER RA(0:6)*140
C
          COMMON/FAST7/RA
C
          INTEGER CVAL(1:140),VRETCT,NSTRUC,SUM,VRTRCK(1:140)
     1    ,VRPOS(1:140),I,JJ,K,J,INSS,C_VAL
C
          COMMON/PRO22/INSTRC,NSTRUC
C
          COMMON/JKSTRUC/STRUC
C
          INCLUDE 'datmai.inc'
C
C               NOW WE RESOLVE MULTIPLE PROGRAM INSTRUCTIONS ON THE
C               INPUT INSTRUCTION LINE.
C
C               NOW INITIALIZE AGAIN THE VIRTUAL CARRIAGE RETURN COUNT,
C               ITS POSITION TRACKING ARRAY AND THE CVAL ARRAY.
C               THEN REANALYSE 'INPUT', FILLING THE CVAL AND VRPOS
C               ARRAYS.
C               STRIP 20 LEADING BLANKS
          J=1
          DO I=1,20
              IF(INPUT(J:J).EQ.' ') INPUT(1:140)=INPUT(2:140)
          END DO
C
          C_VAL=59
C
 65       VRETCT = 0
          DO I=1,140
              VRPOS(I)=0
              CVAL(I)=0
          END DO
          DO I=1,140
              CVAL(I)=ICHAR(INPUT(I:I))
          END DO
          DO I=1,140
              IF(CVAL(I).EQ.C_VAL) THEN
                  VRETCT = VRETCT+1
                  VRPOS(I)=I
              END IF
          END DO
C               NOW WE KNOW WHERE THE VIRTUAL RETURNS ARE.
C               THE RESOLUTION RULE FOR VIRTUAL CARRIAGE RETURNS IS:
C
C                       1) THE PROGRAM EXPECTS TO SEE ONLY SINGLE
C                          VIRTUAL CARRIAGE RETURNS.
C                       2) THE PROGRAM DOES NOT WANT TO SEE VIRTUAL
C                          CARRIAGE RETURNS NEXT TO ONE ANOTHER.
C                       3) THE PROGRAM FIRST LOOKS FOR THE CASE OF
C                          MULTIPLE ADJACENT VIRTUAL CARRIAGE RETURNS
C                          AND REMOVES ALL BUT ONE OF THEM. IN THIS
C                          PROCESS THE VARIABLE 'INPUT' IS REBUILT
C                          ONCE AGAIN. ALSO VIRTUAL RETURNS IN POSITION
C                          1 OF 'INPUT' ARE REMOVED.
C
C               ARE THERE ANY VIRTUAL CARRIAGE RETURNS ?
C
          IF(VRETCT.NE.0) THEN
C               IS THE FIRST POSITION A VIRTUAL RETURN ?
              IF(VRPOS(1).NE.0) THEN
C               REBUILD 'INPUT'
                  INPUT = INPUT(2:140)
                  GO TO 65
              END IF
C               ARE THERE ANY ADJACENT VIRTUAL CARRIAGE RETURNS ?
C               IF THERE ARE THEY ARE ONE BY ONE CUT DOWN TO SINGLE
C               NON ADJACENT RETURNS.
              DO 64 I=1,138
                  IF(VRPOS(I).NE.0.AND.VRPOS(I+1).NE.0) THEN
                      INPUT = INPUT(1:I)//INPUT(I+2:140)
                      GO TO 65
                  END IF
 64           CONTINUE
          END IF
C
C               NOW THERE ARE NO VIRTUAL RETURNS IN THE FIRST POSITION
C               AND THERE ARE NO ADJACENT VIRTUAL RETURNS.
C
C               NOW REMOVE ALL TRAILING VIRTUAL RETURNS.
C               AS BEFORE, REINITIALIZE COUNTING VARIABLES AND ARRAYS.
C               NOW PROCEED TO REMOVE ANY TRAILING VIRTUAL CARRIAGE
C               RETURNS.
C
          VRETCT = 0
          DO I=1,140
              VRPOS(I)=0
              CVAL(I)=0
          END DO
C
          DO I=1,140
              CVAL(I)=ICHAR(INPUT(I:I))
          END DO
C
          DO I=1,140
              IF(CVAL(I).EQ.C_VAL) THEN
                  VRETCT = VRETCT+1
                  VRPOS(I)=I
              END IF
          END DO
C
          DO I=1,140
              JJ=I+1
              SUM = 0
              IF(VRPOS(I).NE.0) THEN
C
                  DO K=JJ,140
                      SUM=SUM +(CVAL(K)-32)
                  END DO
C
                  IF(SUM.EQ.0) THEN
                      INPUT = INPUT(1:(JJ-2))
                      GO TO 1000
                  ELSE
                      SUM = 0
                  END IF
              END IF
          END DO
C
 1000     CONTINUE
C
C               NOW ALL VITUAL LINE DELETES,CHARACTER DELETES, AND
C               VIRTUAL CARRIAGE RETURNS HAVE BEEN RESOLVED. IF
C               THERE ARE VIRTUAL CARRIAGE RETURNS,THEN THE
C               INPUT INSTRUCTION LINE CONTAINS MULTIPLE PROGRAM
C               COMMANDS WHICH MUST BE RESOLVED, STORED, AND THEN
C               EXECUTED. THE COMMANDS WILL BE INITIALLY BROKEN UP
C               AND STORED IN THE INSTRC ARRAY. INSTRC IS A CHARACTER
C               ARRAY OF 140 ELEMENTS MAXIMUM. THE ACTUAL NUMBER OF
C               INSTRUCTIONS STORED DURING ANY ONE INPUT CYCLE WILL
C               BE TRACKED BY THE INTEGER VARIABLE NSTRUC. THE MAXIMUM
C               ALLOWABLE VALUE1 FOR NSTRUC IS 20. 20 COMMANDS CAN
C               BE STACKED ON ONE INPUT LINE.
C               EACH ELEMENT OF INSTR ARRAY CAN HAVE 140 CHARACTERS.
C
C               AT THIS POINT INITIALIZE THE INSTRC(I) ARRAY TO HAVE
C               EACH ELEMENT CONTAIN ONLY BLANKS.ALSO INITIALIZE
C               COMMWD(I),QUALWD(I),STRING(I),ANW1(I),ANW2(I),ANW3(I),
C               ANW4(I),AND ANW5(I). THESE WILL BE THE REPOSITORIES
C               OF THE COMMAND,QUALIFIER,STRING,AND ALPHA-NUMERIC
C               REPRESENTATIONS OF THE NUMERIC WORDS.
C
          DO I=1,NUMCOM
              INSTRC(I)=AA//AA//AA//AA//AA//AA//AA
          END DO
C
C               AGAIN  INITALIZE AND ASSIGN VALUE1S TO COUNTERS AND
C               ARRAYS.
          J = 1
          DO I=1,140
              CVAL(I)=0
              VRTRCK(I)=0
          END DO
          DO I=1,140
              CVAL(I) = ICHAR(INPUT(I:I))
          END DO
          DO I=1,140
              IF(CVAL(I).EQ.C_VAL) THEN
                  VRTRCK(J)=I
                  J=J+1
              END IF
          END DO
          IF(J.EQ.1) THEN
C                       THERE IS ONLY ONE INSTRUCTION
              NSTRUC=1
              INSTRC(1)=INPUT
              GO TO 1010
C               THERE IS MORE THAN INSTRUCTION
          END IF
          NSTRUC=J
          INSTRC(1) = INPUT(1:(VRTRCK(1)-1))
          IF(NSTRUC.EQ.2) THEN
              INSTRC(2) = INPUT((VRTRCK(1)+1):140)
          END IF
          IF(NSTRUC.EQ.3) THEN
              INSTRC(2) = INPUT((VRTRCK(1)+1):(VRTRCK(2)-1))
              INSTRC(3) = INPUT((VRTRCK(2)+1):140)
          END IF
          IF(NSTRUC.GT.3) THEN
              DO 1004 I=2,(NSTRUC-1)
                  INSTRC(I)=INPUT((VRTRCK(I-1)+1):(VRTRCK(I)-1))
 1004         CONTINUE
              INSTRC(NSTRUC)=INPUT((VRTRCK(NSTRUC-1)+1):140)
              GO TO 1020
          END IF
 1010     CONTINUE
 1020     CONTINUE
C
C
C       NEW FEATURE ADDED ON 8/5/88, CHANGE ALL LOWER CASE ALPHA
C       CHARACTERS IN INSTRC(I) TO UPPER CASE
C       UNLESS THE INSTRUCTION STARTS WITH:
C                      M(SPACE)
C     OR               M,
C     OR               C(SPACE)
C     OR               C,
C     OR               LIC(SPACE)
C     OR               LIC,
C     OR               LI(SPACE)
C     OR               LI,
C     OR               FIGURE(SPACE)
C     OR               FIGURE,
C     OR               ONAME
C     OR               SNAME
C       DO THIS BY CALLING UPPER.FOR
          DO INSS=1,NSTRUC
              STRUC=INSTRC(INSS)
              IF(STRUC(1:1).EQ.'M'.OR.STRUC(1:1).EQ.'m'.or.
     1        STRUC(1:1).EQ.'C'.OR.STRUC(1:1).EQ.'c') THEN
                  IF(STRUC(2:2).EQ.','.OR.STRUC(2:2).EQ.' ') THEN
                      IF(STRUC(1:1).EQ.'m') STRUC(1:1)='M'
                      IF(STRUC(1:1).EQ.'c') STRUC(1:1)='C'
                      STRUC(1:140)=STRUC(1:2)//STRUC(3:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:3).EQ.'LIC'.OR.STRUC(1:3).EQ.'lic') THEN
                  IF(STRUC(4:4).EQ.','.OR.STRUC(4:4).EQ.' ') THEN
                      STRUC(1:3)='LIC'
                      STRUC(1:140)=STRUC(1:4)//STRUC(5:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:3).EQ.'MFG'.OR.STRUC(1:3).EQ.'mfg') THEN
                  IF(STRUC(4:4).EQ.','.OR.STRUC(4:4).EQ.' ') THEN
                      STRUC(1:3)='MFG'
                      STRUC(1:140)=STRUC(1:4)//STRUC(5:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:6).EQ.'CATNUM'.OR.STRUC(1:6).EQ.'catnum') THEN
                  IF(STRUC(7:7).EQ.','.OR.STRUC(7:7).EQ.' ') THEN
                      STRUC(1:6)='CATNUM'
                      STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:5).EQ.'ONAME'.OR.STRUC(1:5).EQ.'oname') THEN
                  IF(STRUC(6:6).EQ.','.OR.STRUC(6:6).EQ.' ') THEN
                      STRUC(1:5)='ONAME'
                      STRUC(1:140)=STRUC(1:6)//STRUC(7:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:5).EQ.'SNAME'.OR.STRUC(1:5).EQ.'sname') THEN
                  IF(STRUC(6:6).EQ.','.OR.STRUC(6:6).EQ.' ') THEN
                      STRUC(1:5)='SNAME'
                      STRUC(1:140)=STRUC(1:6)//STRUC(7:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:6).EQ.'CV2PRG'.OR.STRUC(1:6).EQ.'cv2prg') THEN
                  IF(STRUC(7:7).EQ.','.OR.STRUC(7:7).EQ.' ') THEN
                      STRUC(1:6)='CV2PRG'
                      STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:7).EQ.'ZMX2PRG'.OR.STRUC(1:7).EQ.'zmx2prg') THEN
                  IF(STRUC(8:8).EQ.','.OR.STRUC(8:8).EQ.' ') THEN
                      STRUC(1:7)='ZMX2PRG'
                      STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:7).EQ.'PFANCAP'.OR.STRUC(1:7).EQ.'pfancap') THEN
                  IF(STRUC(8:8).EQ.','.OR.STRUC(8:8).EQ.' ') THEN
                      STRUC(1:7)='PFANCAP'
                      STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:7).EQ.'PFANLBL'.OR.STRUC(1:7).EQ.'pfanlbl') THEN
                  IF(STRUC(8:8).EQ.','.OR.STRUC(8:8).EQ.' ') THEN
                      STRUC(1:7)='PFANLBL'
                      STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:2).EQ.'LI'.OR.STRUC(1:2).EQ.'li') THEN
                  IF(STRUC(3:3).EQ.','.OR.STRUC(3:3).EQ.' ') THEN
                      STRUC(1:2)='LI'
                      STRUC(1:140)=STRUC(1:3)//STRUC(4:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:3).EQ.'LBL'.OR.STRUC(1:3).EQ.'lbl') THEN
                  IF(STRUC(4:4).EQ.','.OR.STRUC(4:4).EQ.' ') THEN
                      IF(STRUC(1:3).EQ.'lbl') STRUC(1:3)='LBL'
                      STRUC(1:140)=STRUC(1:4)//STRUC(5:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:5).EQ.'LABEL'.OR.STRUC(1:5).EQ.'label') THEN
                  IF(STRUC(6:6).EQ.','.OR.STRUC(6:6).EQ.' ') THEN
                      IF(STRUC(1:5).EQ.'label') STRUC(1:5)='LABEL'
                      STRUC(1:140)=STRUC(1:6)//STRUC(7:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
              IF(STRUC(1:6).EQ.'FIGURE'.OR.STRUC(1:6).EQ.'figure') THEN
                  IF(STRUC(7:7).EQ.','.OR.STRUC(7:7).EQ.' ') THEN
                      IF(STRUC(1:6).EQ.'figure') STRUC(1:6)='FIGURE'
                      STRUC(1:140)=STRUC(1:7)//STRUC(8:139)
                      GO TO 89
                  END IF
              ELSE
              END IF
 89           INSTRC(INSS)=STRUC
          END DO
C       NEW FEATURE COMPLETE
          RETURN
      END



C SUB PRO3.FOR
      SUBROUTINE PRO3
          USE GLOBALS
C
          IMPLICIT NONE
C
C       THIS ROUTINE PROCESSES UP TO NUMCOM INSTRUCTIONS
C       INTO PROGRAM COMMANDS
C
C                       DEFINE VARIABLES
C
          CHARACTER INSTRC(1:20)*140,COMMWD(1:20)*8,
     1    QUALWD(1:20)*8,STRING(1:20)*140,CH*139,
     2    ANW1(1:20)*23,ANW2(1:20)*23,ANW3(1:20)*23,
     3    ANW4(1:20)*23,ANW5(1:20)*23,ANW(1:20)*140,
     4    DM1*140,DM2*140,BLJK*80
     5    ,JK_INP(1:5)*80
C
          CHARACTER RA(0:6)*140,REMAIN*140,AHOLD1*23,AHOLD2*23
     1    ,AHOLD3*23,AHOLD4*23,AHOLD5*23,AHOLDWQ*6
C
          COMMON/FAST7/RA
C
          LOGICAL ALLB,COLREP,JJK_FLG1(1:5),JJK_FLG2
C
          INTEGER IQ,NSTRUC,COMTST,JK_DF(1:5),JK_I,
     3    BLNK,BSUM,BVAL,STATQL(1:20),STATST(1:20),STATN1(1:20)
     4    ,STATN2(1:20),STATN3(1:20),STATN4(1:20),STATN5(1:20)
     5    ,QBVAL,STATCO(1:20),STATBL(1:20),STATNW(1:20)
     6    ,STATC2(1:20),STBLK2(1:20),IJK
     7    ,I,K,J,L,KKI,KI,M,K1,JKII,HOLDSQ
C
          INTEGER QBVL1,DFSTA1(1:20),DFSTA2(1:20)
     1    ,DFSTA3(1:20),DFSTA4(1:20),DFSTA5(1:20)
     2    ,SI(1:20),
     6    QBVL,BI
C
          REAL*8 NW1(1:20),NW2(1:20),NW3(1:20),NW4(1:20)
     1    ,NW5(1:20)
     2    ,JK_NUM(1:5),HOLD1,HOLD2,HOLD3,HOLD4,HOLD5
C
          COMMON/CBLANK/NW1,NW2,NW3,NW4,NW5,STATNW,STATN1,STATN2,
     1    STATN3,STATN4,STATN5,STATCO,STATC2,STATBL,STBLK2,STATQL,
     2    STATST,SI,BI
C
          COMMON/CCBLAN/COMMWD,QUALWD,STRING,ANW,ANW1,ANW2,ANW3,ANW4,
     1    ANW5
C
          COMMON/PRO22/INSTRC,NSTRUC
C
          INCLUDE 'datmai.inc'


C
          DM1=AA//AA//AA//AA//AA//AA//AA
          DM2=AA//AA//AA//AA//AA//AA//AA
C               AT THIS POINT INITIALIZE
C               COMMWD(I),QUALWD(I),STRING(I),ANW1(I),ANW2(I),ANW3(I),
C               ANW4(I),AND ANW5(I). THESE WILL BE THE REPOSITORIES
C               OF THE COMMAND,QUALIFIER,STRING,AND ALPHA-NUMERIC
C               REPRESENTATIONS OF THE NUMERIC WORDS.
C
C
          DO 1036 I=1,NUMCOM
              COMMWD(I)=BB
              QUALWD(I)=BB
              STRING(I)=AA//AA//AA//AA
              ANW(I)=AA//AA//AA//AA//AA//AA//AA
              ANW1(I)=AA//'   '
              ANW2(I)=AA//'   '
              ANW3(I)=AA//'   '
              ANW4(I)=AA//'   '
              ANW5(I)=AA//'   '
              NW1(I)=0.0D0
              NW2(I)=0.0D0
              NW3(I)=0.0D0
              NW4(I)=0.0D0
              NW5(I)=0.0D0
 1036     CONTINUE
C
C               NOW  EACH ENTRY IN THE INSTRC ARRAY MUST BE BROKEN UP
C               INTO A COMMAND WORD, QUALIFIER WORD, NUNERIC WORDS,
C               OR ALPHANUMERIC STRING AS APPROPRIATE.
C
C               FOR EACH ENTRY IN THE INSTRC ARRAY THERE WILL
C               BE A CORRESPONDING SET OF ENTRIES IN STATUS
C               ARRAYS WHICH WILL TRACK THE PRESENCE OF ABSCENCE
C               OF A QUALIFIER WORD, FIVE NUMERIC WORDS AND AN
C               ALPHA-NUMERIC STRING. THE STATUS ARRAY FOR THE
C               PRESENCE OR ABSCENCE OF A QUALIFIER WORD IS
C               STATQL(I) WHERE I IS THE INDEX OF THE INSTRUCTION
C               ORIGINALLY STORED IN THE INSTRC(I) ARRAY.
C               IF THERE IS A QUALIFIER WORD FOR THE COMMAND
C               STORED IN INSTRC(I), THEN STATQL(I) WILL BE SET
C               TO 1. IF THERE IS NO QUALIFIER WORD, STATQL(I)
C               WILL BE SET TO 0(ZERO).
C               SIMILAR ARRAYS TO TRACK THE STATUS OF AN
C               ALPHA-NUMERIC STRING OR THE 5 NUMERIC WORDS ARE
C               STATST(I),STATN1(I),STATN2(I),STATN3(I),STATN4(I)
C               AND STATN5(I).
C               THESE STATUS ARRAYS WILL FOLLOW THE INSTRC(I)
C               INSTRUCTIONS TO THE COMMAND EXECUTION SECTION
C               OF THIS PROGRAM.
C
C               THE FIRST EIGHT NON-BLANK CONSECUTIVE ALPHA-
C               NUMERIC CHARACTERS (NOT COUNTING LEADING BLANKS
C               WHICH ARE DISCARDED), ARE CONSIDERED BY THIS
C               PROGRAM TO BE THE COMMAND WORD OF AN INSTRUCTION
C               LINE. THE FIRST CHARACTER OF A COMMAND WORD IS THE
C               FIRST NON-BLANK CHARACTER IN AN INSTRC ARRAY
C               ENTRY. THE LAST CHARACTER IN A COMMAND WORD IS
C               EITHER THE EIGTH CONSECUTIVE NON-BLANK CHARACTER
C               IN AN INSTRC ARRAY ENTRY OR IT IS THE LAST NON-BLANK
C               CONSECUTIVE CHARACTER IN AN INSTRC ARRAY ENTRY.
C               WHEN THE COMMAND PROCESSOR ENCOUNTERS ONE OR MORE BLANK
C               CHARACTERS WHICH ARE NOT LEADING BLANKS, IT ASSUMES
C               THAT COMMAND WORD ENTRY HAS TERMINATED. THE INSTRUCTION
C               PROCESSOR THEN LOOKS FOR QUALIFIER AND OTHER INPUT.
C
C               THE COMMAND WORD ENTRY FROM ARRAY INSTRC(I) IS THEN
C               STORED IN THE COMMAND WORD ARRAY COMMWD(I).
C
C               NOW BREAK OUT THE COMMAND WORDS FROM THE INSTRC ARRAY.
C
C                       THE VARIABLE NSTRUC IS THE COUNT OF THE
C                       NUMBER OF PENDING ISTRUCTIONS STORED IN
C                       THE INSTRC ARRAY. MAX VALUE1 IS NUMCOM.
C
C
C               HERE SETUP STATUS ARRAYS AND INITIALIZE TO 2
C               WHICH MEANS NOT YET ASSIGNED.
C
          DO 1025 I=1,NUMCOM

C
C       0 MEANS NO, 1 MEANS YES, 2 MEANS NOT YES ASSIGNED STATUS
C
C               EVERYTHING IN AN INSTRUCTION IS BLANK
              STATBL(I)=2
C               QUALIFIER IS BLANK
              STATQL(I)=2
C               STRING IS BLANK
              STATST(I)=2
C               ALL NWS ARE ZERO
              STATNW(I)=2
C               NW1 TO NW5 ARE ZERO
              STATN1(I)=2
              STATN2(I)=2
              STATN3(I)=2
              STATN4(I)=2
              STATN5(I)=2
C               FIRST COMMA PRESENT
              STATCO(I)=2
C               COMMA AFTER QUAL PRESENT
              STATC2(I)=2
C               ALL BLANK EXCEPT COMMAND WORD
              STBLK2(I)=2
C               THE NUMERIC WORDS HAVE THE "DEFAULT" VALUE1
C       YES=1,NO=0,NOT ASSIGNED=2
              DFSTA1(I)=2
              DFSTA2(I)=2
              DFSTA3(I)=2
              DFSTA4(I)=2
              DFSTA5(I)=2
C
C       IS THE INTERROGATOR ? PRESENT. YES=1,NO=0,NOT
C       ASSIGNED =0 STATUS INDICATOR IS SI(I)
              SI(I)=0

 1025     CONTINUE
C
C       PROCESS EACH INSTRC(I) INSTRUCTION NOW (MAIN LOOP)
C
          DO 1015 I=1,NSTRUC
C
C               CHECK FOR AND REMOVE FROM EACH INSTRC(I) ANY
C               AND ALL LEADING BLANKS. (ASCII CHARACTER 32)
C
              K = 1
 1016         COMTST = ICHAR(INSTRC(I)(1:1))
              IF(COMTST.EQ.32) THEN
                  INSTRC(I) = (INSTRC(I)(2:140))
C       A BLANK WAS REMOVED
                  K = K + 1
                  IF(K.GT.50) THEN
C       COMMAND WORD WAS ALL BLANK. IN THIS CASE WE SET THE ENTIRE
C       LINE TO BLANK AS WITHOUT A COMMAND WORD WE CAN NOT HAVE
C       QUALIFIER,STRING OR NUMERIC INPUT.
C
C       HERE TEST FOR NSTRUC=1. IF SO SET FLAG 50 = 1
C       ONLY IF FLAG 50 = 1 AND THE INSTRUCTION LINE WAS
C       INTENTIONALLY SET TO BLANK WILL THIS ALLOW
C       SUBROUTINE BLANK TO PRINT THE PROGRAM NAME IN RESPONSE
C       TO A SINGLE CARRIAGE RETURN.
C
                      IF(NSTRUC.EQ.1) F50=1
                      IF(NSTRUC.NE.1) F50=0
C
C       IF INFACT A BLANK COMMAND WORD WAS INTENDED TO BE ENTERED
C       BY EITHER JUST PRESSING THE RETURN KEY OR BY ENTERING A
C       BLANK SPACE FOLLOWED BY A VIRTUAL CARRIAGE RETURN (;) OR A
C       REAL CARRIAGE RETURN, THE INITIALIZATION PROCESS FOR
C       ARRAY INSTRC(I) LEAVES EACH SPACE FILLED WITH A BLANK THUS
C       THAT ENTRY IN THE INSTRC ARRAY WILL BE BLANK IN ALL 140
C       CHARACTERS. WHEN ONLY ONE BLANK IS STORED IN THE FIRST
C       POSITIONS OF AN INSTRC(I) LINE, ALL 140 CHARACTERS WILL
C       LATER TEST AS BLANK.
C
C       IF 140 CHARACTERS TEST BLANK,THEN CERTAINLY THE FIRST
C       EIGHT WILL.
C
C       CALL SETBLN WHERE ALL VALUE1S SET TO BLANK INSTRUCTION
C       THEN SKIP TO NEXT STACKED INSTRUCTION.
C
                      BI=I
                      CALL SETBLN
                      GO TO 1015
                  ELSE
                      GO TO 1016
                  END IF
              END IF
C       THERE WERE NO LEADING BLANKS
C
C     CHECK FOR SPECIAL NUMEIC INPUT  2/26/96
              IF(IN.EQ.5) THEN
                  IF(INSTRC(I)(1:1).EQ.'+'.OR.INSTRC(I)(1:1).EQ.'-') THEN
                      IF(INSTRC(I)(2:2).NE.' ') THEN
C     SPECIAL INPUT OF X, PROCESS AND PROCEED
                          INSTRC(I)(1:140)='SET X,'//INSTRC(I)(1:34)
                      END IF
                  END IF
                  IF(INSTRC(I)(1:1).EQ.'1'.OR.INSTRC(I)(1:1).EQ.'2'.OR.
     1            INSTRC(I)(1:1).EQ.'3'.OR.INSTRC(I)(1:1).EQ.'4'.OR.
     1            INSTRC(I)(1:1).EQ.'5'.OR.INSTRC(I)(1:1).EQ.'6'.OR.
     1            INSTRC(I)(1:1).EQ.'7'.OR.INSTRC(I)(1:1).EQ.'8'.OR.
     1            INSTRC(I)(1:1).EQ.'0'.OR.INSTRC(I)(1:1).EQ.'.'.OR.
     1            INSTRC(I)(1:1).EQ.'9') THEN
C     SPECIAL INPUT OF X
                      INSTRC(I)(1:140)='SET X,'//INSTRC(I)(1:34)
                  END IF
              END IF
C     NOW PROCEED WITH NORMAL PROCESSING
C
C       CONTINUE PROCESSING.
C
C               NOW MAKE EACH COMMAND WORD TERMINATE IF A BLANK
C               OR COMMA
C               OCCURS WITHIN THE FIRST EIGHT CHARACTERS OF EACH
C               VIRTUAL COMMAND LINE.
C               STRIP OFF THE COMMAND WORD AND FILE IT IN THE
C               COMMWD(I) ARRAY AND STORE THE REMAINDER OF THE
C               COMMAND BACK IN THE INSTRC(I) ARRAY.
C
              DO 1022 J=2,9
                  BLNK = ICHAR(INSTRC(I)(J:J))
                  IF(BLNK.EQ.32.OR.BLNK.EQ.44) THEN
                      COMMWD(I)=(INSTRC(I)(1:(J-1)))
                      INSTRC(I)=(INSTRC(I)(J:140))
                      GO TO 1023
                  END IF
 1022         CONTINUE
              COMMWD(I)=(INSTRC(I)(1:8))
              INSTRC(I)=(INSTRC(I)(9:140))
 1023         CONTINUE
C
C               NOW THE COMMAND WORD AND THE REMAINDER WITHOUT
C               THE COMMAND WORD ARE STORED IN COMMWD(I) AND
C               INSTRC(I).
C
C               NOW WE TAKE THE REMAINING PIECE OF EACH ORIGINAL
C               INSTRUCTION,EXAMINE IT, AND TURN IT INTO
C               QUALIFIERS, NUMERIC WORDS, OR A STRING EXPRESSION.
C
C               IF THE FIRST NON-BLANK CHARACTER ENCOUNTERED IN
C               THE REMAINING INTRUCTION IS A COMMA (ASCII 44)
C               THEN THE PROGRAM EXPECTS EITHER A NUMERIC WORD
C               OR AN ALPHANUMERIC STRING. IF THE NEXT INPUT IS
C               AN ALPHANUMERIC STRING THEN THE NEXT NON-BLANK
C               CHARACTER AFTER THE COMMA MUST BE A COLON
C               (ASCII 58).
C               IF THE NEXT NON-BLANK CHARACTER AFTER THE COMMAND WORD
C               IS NOT A COMMA OR A COMMA FOLLOWED BY A COLON
C               THEN THE PROGRAM WILL INTERPRET THE NEXT NON-BLANK
C               CHARACTERS TO BE A QUALIFIER WORD UNLESS THE FIRST
C               CHARACTER OF THE NEXT NON-BLANK CHARCATER IS EITHER
C               A (,),+,-,1,2,3,4,5,6,7,8,9,0,OR,. IF THESE CHARACTERS
C               ARE FIRST SEEN THEN THE NEXT NON-BLANK INPUT IS
C               ONLY NUMERIC. IN THIS CASE A DISCOVERED LEADING BLANK
C               IS REPLACED WITH A COMMA AND THE PROCESSING CONTINUES.
C               IF THE FIRST NON-BLANK CHARACTER AFTER THE BLANK IS
C               A (:) THEN THE NEXT DATA IS STRING DATA AND THE
C               ONE OR MORE BLANKS WITHOUT A COMMA ARE USED
C               TO SEPARATE THE COMMAND WORD FROM THE QUALIFIER.
C               ONE OR MORE BLANKS WITH OR WITHOUT A COMMA CAN SEPARATE
C               THE SECOND THRU FIFTH NUMERIC WORDS FROM EACH OTHER AND
C               FROM THE FIRST NUMERIC WORD. BLANKS CAN ALWAYS
C               OCCUR WITHIN ALPHANUMERIC STRINGS AND ARE THEREIN
C               TREATED AS PART OF THE STRING. COLONS CAN
C               OCCUR WITHIN ALPHANUMERIC STRINGS. ONLY THE FIRST
C               OCCURENCE OF A COLON STARTING AN ALPHANUMERIC
C               STRING IS LOOKED FOR AND TRACKED.
C
C               THE FIRST TEST IS TO EXAMINE EACH INSTRC(I) ENTRY
C               AND DETERMINE IF IT IS ALL BLANK. IF IT IS,THEN ONLY
C               A COMMAND WORD MAKES UP THE CURRENT PROGRAM
C               INSTRUCTION.
C
C       BEGIN CHECKING FOR ALL BLANKS EXCEPT FOR THE COMMAND WORD.
C
              BSUM = 0
              BVAL = 0
              DO 1027 K=2,140
                  BVAL =(ICHAR(INSTRC(I)(K:K))-32)
                  BSUM = BSUM + BVAL
 1027         CONTINUE
C               IF BSUM IS EQUAL TO ZERO THEN ALL OF INSTRC(I) IS
C               BLANK. REMEMBER, BY NOW THE COMMAND WORDS HAVE BEEN
C               REMOVED FROM INSTRC(I). IF THE COMMAND WORD WAS
C               INTERPRETED AS BLANK THEN THE REST OF THAT
C               INSTRUCTION LINE WAS SET TO BLANK AS WELL.
C               THAT OCCURED EARLIER IN THE CODE.
C
              IF(BSUM.EQ.0) THEN
C
                  STATQL(I)=0
                  STATST(I)=0
                  SI(I)=0
                  STATNW(I)=0
                  STATN1(I)=0
                  STATN2(I)=0
                  STATN3(I)=0
                  STATN4(I)=0
                  STATN5(I)=0
                  STATCO(I)=0
                  STATC2(I)=0
                  STBLK2(I)=1
                  STATBL(I)=0
                  DFSTA1(I)=1
                  DFSTA2(I)=1
                  DFSTA3(I)=1
                  DFSTA4(I)=1
                  DFSTA5(I)=1
C
                  GO TO 1015
C       GO TO THE NEXT INSTRUCTION
C
              ELSE
C
C       REMAINDER INSTC(I) IS NOT ALL BLANK, CONT. PROCESSING.
C
C       HERE IS WHERE THE SEARCH FOR SPECIFIC NON-BLANK INPUT BEGINS.
C
C       FIRST LOOK FOR THE FIRST OCCURENCE OF A COMMA OR BLANK IN EACH
C       INSTRC(I). DISCARD ANY OTHER CHARACTERS WHICH OCCUR BEFORE THE
C       FIRST COMMA(ASCII 44) OR BLANK(ASCII 32). REFILE EACH INSTRC(I)
C       SO THAT IT STARTS WITH A COMMA OR BLANK. WE ALREADY KNOW THAT
C       THE INSTRC(I)S WHICH WE ARE LOOKING AT ARE NOT ALL BLANK.
C
 1032             QBVAL = 0
                  L=1
                  QBVAL = ICHAR(INSTRC(I)(L:L))
                  IF(QBVAL.NE.32.AND.QBVAL.NE.44) THEN
                      INSTRC(I)= (INSTRC(I)((L+1):140))
                      GO TO 1032
                  ELSE
                      GO TO 1033
                  END IF
              END IF
 1033         CONTINUE
C
C       NOW EACH ORIGINAL INSTRC(I) HAS BEEN BROKEN UP INTO
C       A COMMAND WORD COMMWD(I) AND A REMAINDER INSTRC(I).
C       IF THE REMAINDER WAS BLANK THEN ALL THE STATUS ARRAYS
C       ARE SET TO ZERO.
C       IF THE COMMAND WORD WAS BLANK THEN IT IS STORED AS 140
C       CHARCTERS OF BLANK.
C
C       NOW EXAMINE EACH REMAINING INSTRC(I) INSTRUCTION (THEY
C       NOW ALL START WITH EITHER A BLANK OR A COMMA)
C       AND DETERMINE THEIR CONTENTS IN TERMS OF
C       QUALIFIERS,STRINGS,OR NUMERIC WORDS.
C
C       CHECK FOR THE PRESENCE OF A QUALIFIER WORD.
C       IF THERE IS A QUALIFIER WORD THEN THE FIRST NON-BLANK
C       CHARACTER IN INSTRC(I) WILL NOT BE A COMMA(ASCII 44) OR
C       ONE OF THE SPECIAL NUMERIC WORD CHARACTERS +,-,1,2,3,4,5
C       6,7,8,9,0,),(,OR,.
C
C       IF THE FIRST NON-BLANK CHARACTER IS A COMMA
C       THEN THERE IS NO QUALIFIER WORD.
C       ACCORDING TO THE RULES SET DOWN, QUALIFIER WORDS
C       ARE SEPARATED FROM COMMAND WORDS BY ONE OR MORE
C       BLANKS (ASCII 32) AND NEVER BY A COMMA. FURTHERMORE,
C       QUALIFIER WORDS NEVER BEGIN WITH A COLON (:, ASCII
C       58 OR COMMAS ASCII 44).
C       BREAK OFF THE QUALIFIER IN A SIMILAR MANNER TO THE
C       COMMAND WORD SEPARATION DONE EARLIER AND FILE THE REMAINER
C       OF THE INSTRC(I) BACK INTO THE INSTRC(I) ARRAY. IF NO
C       QUALIFIER IS FOUND, SET STATQL(I)=0 AND PROCEED TO
C       LOOK FOR AN ALPHA-NUMERIC STRING OR NUMERIC WORDS.
C
C******************************************************************
C
C       SUPPOSE THAT INSTRC(I) STARTS WITH A BLANK BUT THEN
C       HAS SEVERAL MORE BLANKS BEFORE ENCOUNTERING A COMMA
C       OR ANOTHER NON-BLANK CHARACTER. ALSO SUPPOSE THAT
C       BY NOW INSTRC(I) IS ALL BLANK.
C
C       TEST FOR ALL BLANKS
C
              BSUM=0
              BVAL=0
              DO 7020 KKI=1,140
                  BVAL=(ICHAR(INSTRC(I)(KKI:KKI))-32)
                  BSUM=BSUM+BVAL
 7020         CONTINUE
              IF(BSUM.EQ.0) THEN
C       THE REST OF INSTRC(I) = BLANK
C       SET STATUS INDICATORS ACCORDINGLY AND GO TO NEXT
C       INSTRUCTION.
C
                  STATQL(I)=0
                  STATST(I)=0
                  SI(I)=0
                  STATNW(I)=0
                  STATN1(I)=0
                  STATN2(I)=0
                  STATN3(I)=0
                  STATN4(I)=0
                  STATN5(I)=0
                  STATCO(I)=0
                  STATC2(I)=0
                  STBLK2(I)=1
                  STATBL(I)=0
                  DFSTA1(I)=1
                  DFSTA2(I)=1
                  DFSTA3(I)=1
                  DFSTA4(I)=1
                  DFSTA5(I)=1
                  GO TO 1015
C       CONTINUE TO PROCESS
              END IF
C       THE ENTIRE INSTRUCTION WAS NOT BLANK. REMOVE LEADING
C       BLANKS UNTIL A NON-BLANK IS FOUND.
C
 7029         KI=1
              QBVAL=ICHAR(INSTRC(I)(KI:KI))
              QBVL=ICHAR(INSTRC(I)((KI+1):(KI+1)))
              IF(QBVAL.EQ.44) GO TO 7030
              IF(QBVAL.NE.32.AND.QBVAL.NE.44) THEN
C
                  OUTLYNE='SERIOUS ERROR RESOLVING EXTRA BLANKS, SEE'
                  CALL SHOWIT(1)
                  OUTLYNE='STATEMENT LABLE 7029/7030 IN SUBROUTINE "PROCESS"'
                  CALL SHOWIT(1)
                  RETURN
              END IF
              IF(QBVAL.EQ.32.AND.QBVL.EQ.44.OR.QBVAL.EQ.32.AND.
     1        QBVL.EQ.32) THEN
                  INSTRC(I)=(INSTRC(I)((KI+1):140))
                  GO TO 7029
              ELSE
C       DON'T REMOVE THE BLANK. GET OUT AND CONTINUE
                  GO TO 7030
              END IF
 7030         CONTINUE
C
C*******************************************************************
C
C       HERE WE MUST HANDLE THE CASE OF A COMMA FOLLOWED BY
C       MORE BLANKS BEFORE MORE NON-BLANK CHARACTERS.
C       LOOK NOW IF THE FIRST CHARACTER IS A COMMA. IF THE
C       ANSWER IS YES THE REMOVE ANY BLANKS WHICH FOLLOW IT
C       UP TO BUT NOT INCLUDING THE NEXT NON-BLANK
C       CHARACTER
C
 7010         QBVAL=ICHAR(INSTRC(I)(1:1))
              IF(QBVAL.EQ.44) THEN
                  QBVL1=ICHAR(INSTRC(I)(2:2))
                  IF(QBVL1.EQ.32) THEN
                      DM1(1:1)=(INSTRC(I)(1:1))
                      DM2(3:140)=(INSTRC(I)(3:140))
                      INSTRC(I)=DM1(1:1)//DM2(3:140)
                      GO TO 7010
                  ELSE
C       DON'T REMOVE ANYTHING, PROCEED WITH PROCESSING
                      GO TO 7011
                  END IF
              ELSE
C       DON'T REMOVE ANYTHING, PROCEED WITH PROCESSING
                  GO TO 7011
              END IF
 7011         CONTINUE
C
C               THE ABOVE SMALL LOOP REMOVES UNWANTED BLANKS
C               OF THE FORM:
C
C       COMMAND WORD,    NUMERIC WORD
C
C*******************************************************************
C
C       HERE TEST FOR A BLANK. IF YES THERE IS A QUALIFIER
C       UNLESS THE NEXT NON-BLANK CHARACTER IS A (,+,-,1,2,3
C       4,5,6,7,8,9,.,:,),OR ?).
C       IF NOT THEN THERE IS NO QUALIFIER BUT THERE MAY BE
C       NUMERIC OR STRING DATA
C
              QBVAL=ICHAR(INSTRC(I)(1:1))
              IF(QBVAL.NE.44) THEN
C
C       REMOVE LEADING BLANKS AND CHECK FOR SPECIAL
C       CHARACTERS.
C
C               START THE QUALIFIER SEARCH HERE.
C
                  K=1
                  QBVAL=ICHAR(INSTRC(I)(1:1))
C       FIRST REMOVE THE LEADING BLANKS. (WILL HANDLE 20 OF THEM)
 1050             IF(K.GT.50) GO TO 1052
                  IF(QBVAL.EQ.32) THEN
                      INSTRC(I)=(INSTRC(I)(2:140))
C       REFORM QBVAL
                      QBVAL=ICHAR(INSTRC(I)(1:1))
C               ADD 1 TO K
                      K=K+1
                      GO TO 1050
                  ELSE
                      GO TO 1053
                  END IF
C       IF 20 REMOVALS STILL ONLY SEE BLANKS THEN ALL IS BLANK
 1052             INSTRC(I)=AA//AA//AA//AA//AA//AA//AA
C               INSTRC(I) SET TO BLANKS AS EIGTHY BLANKS
C               WERE FOUND BEFORE A QUALIFIER.
                  QUALWD(I)=BB
                  STATQL(I)=0
                  STATBL(I)=0
                  STBLK2(I)=1
C       ASSUME NO NUMERIC OR STRING VALUE1S PRESENT EITHER.
                  STATST(I)=0
                  SI(I)=0
                  STATNW(I)=0
                  STATCO(I)=0
                  STATC2(I)=0
                  STATN1(I)=0
                  STATN2(I)=0
                  STATN3(I)=0
                  STATN4(I)=0
                  STATN5(I)=0
                  DFSTA1(I)=1
                  DFSTA2(I)=1
                  DFSTA3(I)=1
                  DFSTA4(I)=1
                  DFSTA5(I)=1
                  GO TO 1015
 1053             CONTINUE

C
C       ALL LEADING BLANKS ARE REMOVED
C
C       NEW SPECIAL FEATURE ADDED ON 3/11/89 TO ALLOW 13 CHARACTER
C       GLASS NAMES IF THE COMMAND WORD IS:
                  COLREP=.FALSE.
C               SCHOTT
C               SCH2000
C               PFANLBL
C               PFANCAP
C               OHARA
C               HOYA
C               HIKARI
C               CORNIN
C               CHANCE
C               GLCAT
C               MATL
C               RUSSIAN
C               RADHARD
C               USER
C               C
C               M
C               LI
C               LIC
C               FIGURE
C               RE
C               SYS OR SYSTEM
C               LFORMAT
C               LBL OR LABEL
C               STWORD
C               SURFCOAT
C               TITLE
C               DWGNO
C               SURFMATL
C               SURFQUAL
C               CONAME
C               EDIT
C               MAC_EDIT
C               LENSLOC
C               LENSSAVE
C               LSAVE
C               LENSREST
C               LOADPROF
C
C
                  IF(COMMWD(I).EQ.'SURFCOAT')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'TITLE')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'DWGNO')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SURFMATL')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SURFQUAL')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CONAME')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SCHOTT')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SCH2000') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PFANLBL') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PFANCAP') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'GLA')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'OHARA')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'HOYA')    COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'HIKARI')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CORNIN')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CHANCE')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'GLCAT')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'MATL')    COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'RUSSIAN') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'RADHARD') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'USER')    COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'C')       COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'M')       COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LI')      COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PNOTE')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LENS')    COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'MFG')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CATNAME') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'ONAME   ')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SNAME   ')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PSFLI')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PSFTAG')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LIC')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CV2PRG')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'ZMX2PRG') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'INI')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LTYPE')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'FIGURE')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'RE')      COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SYS')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SYSTEM')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LFORMAT') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LBL')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LABEL')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'STWORD')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'EDIT')    COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SETAX')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PROMPT')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PREAD')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'FORMAT')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'OTOBMP')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'ITOBMP')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'BMPREADR')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'MAC_EDIT')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CAPFNIN') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CAPFNOUT')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LENSLOC') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LENSSAVE')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LSAVE')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LENSREST')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LOADPROF')COLREP=.TRUE.
C
C       IF COMMWD(I) IS ONE OF THESE, THEN THE REMAINDER OF INSTR
C       IS A STRING
                  IF(COLREP) THEN
                      COLREP=.FALSE.
C
                      QUALWD(I)='        '
                      STATQL(I)=0
                      STRING(I)=INSTRC(I)
                      STATST(I)=1
C     STRIP LEADING COMMA
                      DO IJK=1,9
                          IF(STRING(I)(1:1).EQ.',') THEN
                              BLJK=STRING(I)(2:80)
                              STRING(I)=AA//AA//AA//AA
                              STRING(I)=BLJK
                          ELSE
                          END IF
                      END DO
C     STRIP LEADING SPACES UNLESS COMMAND WORD IS SPECIAL
                      IF(COMMWD(I).NE.'M'.AND.COMMWD(I).NE.'C') THEN
                          DO IJK=1,9
                              IF(STRING(I)(1:1).EQ.' ') THEN
                                  BLJK=STRING(I)(2:80)
                                  STRING(I)=AA//AA//AA//AA
                                  STRING(I)=BLJK
                              ELSE
                                  GO TO 8763
                              END IF
                          END DO
                      ELSE
                      END IF
 8763                 CONTINUE
C     STRIP LEADING COLON
                      DO IJK=1,1
                          IF(STRING(I)(1:1).EQ.':') THEN
                              BLJK=STRING(I)(2:80)
                              STRING(I)=AA//AA//AA//AA
                              STRING(I)=BLJK
                          ELSE
                              GO TO 8764
                          END IF
                      END DO
 8764                 CONTINUE
                      IF(STRING(I)(1:1).EQ.'?') THEN
                          STATST(I)=0
                          SI(I)=1
                      ELSE
                          STATST(I)=1
                          SI(I)=0
                      END IF
                      NW1(I)=0.0
                      NW2(I)=0.0
                      NW3(I)=0.0
                      NW4(I)=0.0
                      NW5(I)=0.0
                      STATQL(I)=0
                      STATBL(I)=0
                      STBLK2(I)=0
                      STATNW(I)=0
                      STATN1(I)=0
                      STATN2(I)=0
                      STATN3(I)=0
                      STATN4(I)=0
                      STATN5(I)=0
                      STATCO(I)=0
                      STATC2(I)=0
                      STATN1(I)=0
                      STATN2(I)=0
                      STATN3(I)=0
                      STATN4(I)=0
                      STATN5(I)=0
                      DFSTA1(I)=1
                      DFSTA2(I)=1
                      DFSTA3(I)=1
                      DFSTA4(I)=1
                      DFSTA5(I)=1
                      GO TO 1015
C       CONTINUE PROCESSING INSTRC
                  END IF
C       QUALIFIER WORDS CANNOT BEGIN WITH A ),(,0,1,2,3,4
C       5,6,7,8,9,.,+,-,:,OR ? IF THESE
C       ARE ENCOUNTERED THEN
C       A COMMA IS INSERTED AHEAD OF THE NEXT NON-BLANK CHARACTER
C       AND THE DATA IS PROCESSED AS NUMERIC OR
C       STRING DATA. THIS ALLOWS FOR THE COMMA
C       BETWEEN COMMAND AND NUMERIC OR STRING
C       AND THE COMMA BETWEEN QUAL AND NUMERIC OR
C       STRING TO BE OPTIONAL.
C
C       CHECK FOR SPECIAL CHARACTERS BEGINS HERE.
                  QBVAL = 0
                  QBVAL=ICHAR(INSTRC(I)(1:1))
                  IF(QBVAL.EQ.48.OR.QBVAL.EQ.49.OR.
     1            QBVAL.EQ.50.OR.QBVAL.EQ.51.OR.QBVAL.EQ.
     2            52.OR.QBVAL.EQ.53.OR.QBVAL.EQ.54.OR.
     3            QBVAL.EQ.55.OR.QBVAL.EQ.56.OR.QBVAL.EQ.
     4            57.OR.QBVAL.EQ.46.OR.QBVAL.EQ.43.OR.
     5            QBVAL.EQ.45.OR.QBVAL.EQ.58.OR.QBVAL
     6            .EQ.63.OR.QBVAL.EQ.40.OR.QBVAL.EQ.41) THEN
C       REFORM INSTRC(I) WITH LEADING COMMA.
                      CH= (INSTRC(I)(1:139))
                      INSTRC(I)=','//CH
                      QUALWD(I)=BB
                      STATQL(I)=0
                      STATCO(I)=1
                      STATC2(I)=0
                      STATBL(I)=0
                      STBLK2(I)=0
                  ELSE
C       NO SPECIAL CHARACTERS OCCURRED, THERE MUST BE A QUALIFIER
C       SO PROCESS OUT THE QUALIFIER.
C
C
C               THE QUALIFIER WORD IS ALSO SHORTENED TO A
C               MAXIMUM OF 8 CHARACTERS (BUT THE QUALIFIER
C               ALSO ENDS IF A BLANK OR COMMA IS ENCOUNTERED.
C               THUS FIRST EITHER
C               LOOK IN INSTRC(I) FOR THE FIRST COMMA,THE FIRST
C               BLANK OR THE FIRST EIGHT NON-BLANK/NON-COMMA
C               CHARACTERS, THEN FILE IN THE QUALIFIER
C               WORD QUALWD(I) ARRAY.
C
                      QBVAL = 0
                      DO 1043 M=2,9
                          QBVAL = ICHAR(INSTRC(I)(M:M))
                          IF(QBVAL.EQ.32.OR.QBVAL.EQ.44) THEN
C               THE END OF THE QUALIFIER WAS FOUND BEFORE EIGHT
C               CHARACTERS WERE SEARCHED. MAKE NEW QUALWD(I).
                              QUALWD(I)=(INSTRC(I)(1:(M-1)))
C               REMAINDER IS REFILED INTO INSTRC(I) FOR FUTURE SEARCH
                              INSTRC(I)=(INSTRC(I)(M:140))
                              GO TO 1044
                          END IF
 1043                 CONTINUE
                      QUALWD(I)=(INSTRC(I)(1:8))
                      INSTRC(I)=(INSTRC(I)(9:140))
 1044                 CONTINUE
                      STATQL(I)=1
                      STATCO(I)=0
                      STBLK2(I)=0
                      STATBL(I)=0
                  END IF
C
              ELSE
C
C               THERE IS NO QUAL BUT THERE IS NUMERIC
C               OR STRING DATA SO RESOLVE IT.
C
                  QUALWD(I)=BB
                  STATQL(I)=0
                  STATCO(I)=1
                  STATC2(I)=0
                  STATBL(I)=0
                  STBLK2(I)=0
C
C       NEW SPECIAL FEATURE ADDED ON 3/11/89 TO ALLOW 13 CHARACTER
C       GLASS NAMES IF THE COMMAND WORD IS:
                  COLREP=.FALSE.
C               SCHOTT
C               SCH2000
C               PFANLBL
C               PFANCAP
C               OHARA
C               HOYA
C               HIKARI
C               CORNIN
C               CHANCE
C               GLCAT
C               MATL
C               RUSSIAN
C               RADHARD
C               USER
C               C
C               M
C               LI
C               LIC
C               FIGURE
C               RE
C               SYS OR SYSTEM
C               LFORMAT
C               LBL OR LABEL
C               STWORD
C               SURFCOAT
C               TITLE
C               DWGNO
C               SURFMATL
C               SURFQUAL
C               CONAME
C               EDIT
C               MAC_EDIT
C               LENSLOC
C               LENSSAVE
C               LSAVE
C               LENSREST
C               LOADPROF
C
C
                  IF(COMMWD(I).EQ.'SURFCOAT')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'TITLE')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'DWGNO')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SURFMATL')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SURFQUAL')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CONAME')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SCHOTT')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SCH2000') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PFANLBL') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PFANCAP') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'GLA')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'OHARA')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'HOYA')    COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'HIKARI')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CORNIN')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CHANCE')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'GLCAT')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'RUSSIAN') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'RADHARD') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'USER')    COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'C')       COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'M')       COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LI')      COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PNOTE')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LENS')    COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'MFG')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CATNUM')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'ONAME   ')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SNAME   ')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PSFLI')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PSFTAG')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LIC')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CV2PRG')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'ZMX2PRG') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'INI')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LTYPE')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'FIGURE')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'RE')      COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SYS')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SYSTEM')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LFORMAT') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LBL')     COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LABEL')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'STWORD')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'EDIT')    COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'SETAX')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PROMPT')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'PREAD')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'FORMAT')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'OTOBMP')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'ITOBMP')  COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'BMPREADR')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CAPFNIN') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'CAPFNOUT')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LENSLOC') COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LENSSAVE')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LENSREST')COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LSAVE')   COLREP=.TRUE.
                  IF(COMMWD(I).EQ.'LOADPROF')COLREP=.TRUE.

C
C       IF COMMWD(I) IS ONE OF THESE, THEN THE REMAINDER OF INSTR
C       IS A STRING
                  IF(COLREP) THEN
                      COLREP=.FALSE.
C
                      QUALWD(I)='        '
                      STATQL(I)=0
                      STRING(I)=INSTRC(I)
                      STATST(I)=1
C     STRIP LEADING COMMA
                      DO IJK=1,9
                          IF(STRING(I)(1:1).EQ.',') THEN
                              BLJK=STRING(I)(2:80)
                              STRING(I)=AA//AA//AA//AA
                              STRING(I)=BLJK
                          ELSE
                          END IF
                      END DO
C     STRIP LEADING SPACES UNLESS COMMAND WORD IS SPECIAL
                      IF(COMMWD(I).NE.'M'.AND.COMMWD(I).NE.'C') THEN
                          DO IJK=1,9
                              IF(STRING(I)(1:1).EQ.' ') THEN
                                  BLJK=STRING(I)(2:80)
                                  STRING(I)=AA//AA//AA//AA
                                  STRING(I)=BLJK
                              ELSE
                                  GO TO 9763
                              END IF
                          END DO
                      ELSE
                      END IF
 9763                 CONTINUE
C     STRIP LEADING COLON
                      DO IJK=1,1
                          IF(STRING(I)(1:1).EQ.':') THEN
                              BLJK=STRING(I)(2:80)
                              STRING(I)=AA//AA//AA//AA
                              STRING(I)=BLJK
                          ELSE
                              GO TO 9764
                          END IF
                      END DO
 9764                 CONTINUE
                      IF(STRING(I)(1:1).EQ.'?') THEN
                          STATST(I)=0
                          SI(I)=1
                      ELSE
                          STATST(I)=1
                          SI(I)=0
                      END IF
                      NW1(I)=0.0
                      NW2(I)=0.0
                      NW3(I)=0.0
                      NW4(I)=0.0
                      NW5(I)=0.0
                      STATQL(I)=0
                      STATBL(I)=0
                      STBLK2(I)=0
                      STATNW(I)=0
                      STATN1(I)=0
                      STATN2(I)=0
                      STATN3(I)=0
                      STATN4(I)=0
                      STATN5(I)=0
                      STATCO(I)=0
                      STATC2(I)=0
                      STATN1(I)=0
                      STATN2(I)=0
                      STATN3(I)=0
                      STATN4(I)=0
                      STATN5(I)=0
                      DFSTA1(I)=1
                      DFSTA2(I)=1
                      DFSTA3(I)=1
                      DFSTA4(I)=1
                      DFSTA5(I)=1
                      GO TO 1015
C       CONTINUE PROCESSING INSTRC
                  END IF
C
              END IF
C               NOW RESOLVE THE REMAINDER
C
C       IF THERE WAS A QUALIFIER AND IF IT WAS TYPED IN AS MORE
C       THAN EIGTH CHARACTERS THEN THE REMAINDER IS IN INSTRC(I)
C       CHECK FOR AND REMOVE ALL LEADING NON-BLANK AND NON-COMMA
C       CHARACTERS.
C
 9010         QBVAL=ICHAR(INSTRC(I)(1:1))
              IF(QBVAL.NE.32.AND.QBVAL.NE.44) THEN
                  INSTRC(I)=INSTRC(I)(2:140)
                  GO TO 9010
C       ALL LEADING NON-BLANK AND NON-COMMA CHARACTERS HAVE
C       BEEN REMOVED, PROCEED.
              END IF
C
C       IS THE INSTRC(I) ALL BLANK?
C
              BSUM=0
              BVAL=0
              DO 9011 KI=1,140
                  BVAL=ICHAR(INSTRC(I)(KI:KI))
                  BSUM=BSUM+BVAL
 9011         CONTINUE
              IF(BSUM.EQ.0) THEN
C       THE REMAINDER OF INSTRC(I) IS BLANK. SET
C       DEFAULT INDICATORS AND PROCEED.
                  STATST(I)=0
                  SI(I)=0
                  STATNW(I)=0
                  STATCO(I)=0
                  STATC2(I)=0
                  STATN1(I)=0
                  STATN2(I)=0
                  STATN3(I)=0
                  STATN4(I)=0
                  STATN5(I)=0
                  DFSTA1(I)=1
                  DFSTA2(I)=1
                  DFSTA3(I)=1
                  DFSTA4(I)=1
                  DFSTA5(I)=1
                  GO TO 1015
C       INSTRC(I) IS NOT ALL BLANK AND MUST CONTAIN
C       EITHER STRING OR NUMERIC DATA.
              END IF
C
C       NOW AS WITH THE CASE OF THE COMMAND WORD WITH
C       NO QUALIFIER, WE NEED TO LOOK FOR THE CASE
C       OF EXTRA BLANKS BEFORE AND AFTER A COMMA.
C       REMOVE ALL LEADING BLANKS.
C
              KI=1
 9012         QBVAL=ICHAR(INSTRC(I)(1:1))
              KI=KI+1
              IF(QBVAL.EQ.32) THEN
                  INSTRC(I)=INSTRC(I)(2:140)
                  IF(KI.LE.50) GO TO 9012
                  IF(KI.GT.50) THEN
C       THE REST OF THE LINE WAS BLANK
                      STATST(I)=0
                      SI(I)=0
                      STATNW(I)=0
                      STATCO(I)=0
                      STATC2(I)=0
                      STATN1(I)=0
                      STATN2(I)=0
                      STATN3(I)=0
                      STATN4(I)=0
                      STATN5(I)=0
                      DFSTA1(I)=1
                      DFSTA2(I)=1
                      DFSTA3(I)=1
                      DFSTA4(I)=1
                      DFSTA5(I)=1
                      GO TO 1015
                  END IF
C       ALL LEADING BLANKS REMOVED.
              END IF
C
C       IS THE NEXT CHARACTER A COMMA? IF SO IS
C       IT FOLLOWED BY A BLANK? IF YES REMOVE BLANK.
C
              KI=1
 9013         QBVAL=ICHAR(INSTRC(I)(1:1))
              QBVL=ICHAR(INSTRC(I)(2:2))
              IF(QBVAL.EQ.44.AND.QBVL.EQ.32) THEN
C       REMOVE THE BLANK AND TEST AGAIN
                  DM1(1:1)=INSTRC(I)(1:1)
                  DM2(3:140)=INSTRC(I)(3:140)
                  INSTRC(I)=DM1(1:1)//DM2(3:140)
                  KI=KI+1
                  IF(KI.GT.50) THEN
C
C       THE REST OF THE INSTRC(I) IS BLANK.
C       SET STATUS AND CONTINUE.
                      STATST(I)=0
                      SI(I)=0
                      STATNW(I)=0
                      STATCO(I)=0
                      STATC2(I)=0
                      STATN1(I)=0
                      STATN2(I)=0
                      STATN3(I)=0
                      STATN4(I)=0
                      STATN5(I)=0
                      DFSTA1(I)=1
                      DFSTA2(I)=1
                      DFSTA3(I)=1
                      DFSTA4(I)=1
                      DFSTA5(I)=1
                      GO TO 1015
C       PROCEED
                  END IF
                  GO TO 9013
C       THERE ARE NO BLANKS FOLLOWING THE COMMA
C       PROCEED
              END IF
C       NOW BREAK OUT STRINGS AND NUMERIC WORDS
C
C
C       ALL LEADING BLANKS WERE REMOVED. THEN IF THERE
C       WAS A COMMA, ALL TRAILING BLANKS WERE REMOVED.
C       IF THERE NOW NO COMMA IN POSITION 1, PUT ONE
C       THERE AND THEN PROCEED.
C
              QBVAL=ICHAR(INSTRC(I)(1:1))
              IF(QBVAL.NE.44) THEN
                  CH=(INSTRC(I)(1:139))
                  INSTRC(I)=','//CH
C       THERE WAS A LEADING COMMA
              END IF
C
C       NEXT CHARCATER IS A COMMA NOT FOLLOWED BY BLANKS
C
C
C               CORRECTLY SET THE COMMA STATUS INDICATOR
C               STATC2(I).
C
              IF(STATCO(I).EQ.1) STATC2(I)=0
              IF(STATCO(I).EQ.0) STATC2(I)=1
C
C       A COMMA WAS THE NEXT NON-BLANK CHARACTER ENCOUNTERED. ANY NON-
C       BLANK CHARACTERS WHICH FOLLOW ARE EITHER STRING OR NUMERIC
C       INPUT. IF THE NEXT NON-BLANK CHARACTER FOUND IS A COLON
C       THEN THE REMAINING INPUT IN INSTRC(I) IS AN ALPHANUMERIC STRING
C
C       NOW REMOVE THE COMMA IN THE LEADING PLACE OF INSTRC(I).
C
              INSTRC(I)=(INSTRC(I)(2:140))
C
C       NOW REMOVE ANY LEADING BLANKS AND CHECK IF THE
C       FIRST NON-BLANK CHARACTER ENCOUNTERED IS A
C       COLON OR THE SPECIAL INTERROGATOR CHARACTER ?.
C       IF A COLON IS FOUND THEN THERE IS STRING
C       DATA AND NOT NUMERIC. IF THE SPECIAL INTERROGATOR
C       CHARACTER ? THEN THE (?) BECOMES THE STRING.
C       THERE ARE TWWO WAYS TO MAKE ? THE STRING.
C       THE TRADITIONAL WAY IS TO TYPE :?, THE SHORT CUT WAY
C       IS TO JUST TYPE ? AFTER A COMMAND WORD OR COMMAND WORD/
C       QUALIFIER WORD PAIR.
C
C       FIRST CHECK FOR ALL BLANKS
C
              QBVAL = 0
              K1=0
 1017         QBVAL = ICHAR(INSTRC(I)(1:1))
              K1=K1+1
              IF(QBVAL.EQ.32) THEN
                  INSTRC(I)=(INSTRC(I)(2:140))
                  IF(K1.EQ.140) THEN
C       NO STRING JUST BLANK NUMERIC INPUT
                      STATST(I)=0
                      SI(I)=0
                      STATNW(I)=0
                      STATN1(I)=0
                      STATN2(I)=0
                      STATN3(I)=0
                      STATN4(I)=0
                      STATN5(I)=0
                      DFSTA1(I)=1
                      DFSTA2(I)=1
                      DFSTA3(I)=1
                      DFSTA4(I)=1
                      DFSTA5(I)=1
                      GO TO 1015
                  END IF
                  GO TO 1017
              END IF
C       TEST FOR COLON ASCII(58) OR QUESTION MARK
C       ASCII(63).
C
C     ADDED 1/22/95 SPECIAL STRING INPUT AFTER A QUALIFIER
C
C     IF CW IS WRITE AND THE REST OF THE INSTRUCTION IS NOT BLANK,
C     ADD A COLON
              IF(COMMWD(I).EQ.'WRITE'.OR.COMMWD(I).EQ.'ASTO'.OR.COMMWD(I).EQ.
     1        'ROWHD'.OR.COMMWD(I).EQ.'COLHD'.OR.COMMWD(I).EQ.'ONAME'.OR.
     1        COMMWD(I).EQ.'LI'.OR.COMMWD(I).EQ.'LIC'.OR.COMMWD(I).EQ.'FIGURE'
     1        .OR.COMMWD(I).EQ.'M'.OR.COMMWD(I).EQ.'C'.OR.COMMWD(I).EQ.'PSFLI'
     2        .OR.COMMWD(I).EQ.'ROWHD2'.OR.COMMWD(I).EQ.'COLHD2'.OR.COMMWD(I)
     3        .EQ.'PSFTAG'.OR.COMMWD(I).EQ.'GLASSP'.OR.COMMWD(I).EQ.'LO'.OR.
     4        COMMWD(I).EQ.'QSUB'.AND.QUALWD(I).EQ.'DV'.OR.COMMWD(I).EQ.
     5        'SNAME'.OR.COMMWD(I).EQ.'RENAME'.OR.COMMWD(I).EQ.'DUP'
     6        .OR.COMMWD(I).EQ.'PLOT'.AND.QUALWD(I).EQ.'NAME'
     7        .OR.COMMWD(I).EQ.'INI'.OR.COMMWD(I).EQ.'LTYPE'.OR.
     8        COMMWD(I).EQ.'AWRTSUM'.AND.QUALWD(I).NE.'        '.OR.
     9        COMMWD(I).EQ.'BWRTSUM'.AND.QUALWD(I).NE.'        '.OR.
     9        COMMWD(I).EQ.'MRENAME'.AND.QUALWD(I).NE.'        '.OR.
     9        COMMWD(I).EQ.'MCOPY'.AND.QUALWD(I).NE.'        '
     1        .OR.COMMWD(I).EQ.'OP_DESC'.AND.QUALWD(I).NE.'        '
     2        .OR.COMMWD(I).EQ.'STREAK'.AND.QUALWD(I).EQ.'PLOT'.OR.COMMWD(I)
     3        .EQ.'STREAK'.AND.QUALWD(I).EQ.'WRITE'.OR.COMMWD(I).EQ.'PLOT'.AND.
     4        QUALWD(I).EQ.'VIGSTAT'.OR.COMMWD(I).EQ.'SSUB'.AND.QUALWD(I)
     5        .EQ.'DV'.OR.COMMWD(I).EQ.'LBL'.OR.COMMWD(I).EQ.'LABEL'
     5        .OR.COMMWD(I).EQ.'CV2PRG'.OR.COMMWD(I).EQ.'ZMX2PRG'
     5        .OR.COMMWD(I).EQ.'DXF'.AND.QUALWD(I).EQ.'LAYER'.OR.COMMWD(I).EQ.
     6        'MFG'.OR.COMMWD(I).EQ.'CATNUM'.OR.COMMWD(I).EQ.'OTOBMP'.OR.
     7        COMMWD(I).EQ.'ITOBMP'.OR.COMMWD(I).EQ.'BMPREADR'.OR.COMMWD(I).EQ.
     8        'CAPFNIN'.OR.COMMWD(I).EQ.'CAPFNOUT'.OR.COMMWD(I).EQ.'LENS'.OR.
     9        COMMWD(I).EQ.'PNOTE') THEN
                  DO IQ=1,8
C     STRIP OFF UP TO 8 LEADING COMMAS
                      IF(INSTRC(I)(1:1).EQ.',') INSTRC(I)(1:139)=INSTRC(I)(2:139)
                  END DO
                  ALLB=.TRUE.
                  DO IQ=1,80
                      IF(INSTRC(I)(IQ:IQ).NE.' ') ALLB=.FALSE.
                  END DO
                  IF(COMMWD(I).EQ.'OUTPUT'.AND.QUALWD(I).EQ.'T        '.OR.
     1            COMMWD(I).EQ.'OUT'.AND.QUALWD(I).EQ.'T       ') THEN
                      DO IQ=1,8
C     STRIP OFF UP TO 8 LEADING COMMAS
                          IF(INSTRC(I)(1:1).EQ.',') INSTRC(I)(1:139)=INSTRC(I)(2:139)
                      END DO
                      ALLB=.TRUE.
                      DO IQ=1,80
                          IF(INSTRC(I)(IQ:IQ).NE.' ') ALLB=.FALSE.
                      END DO
                      IF(INSTRC(I)(1:1).EQ.'?') THEN
                          SI(I)=1
                          STATST(I)=0
                      ELSE
                          IF(.NOT.ALLB) INSTRC(I)(1:140)=':'//INSTRC(I)(1:139)
                          SI(I)=0
                          STATST(I)=1
                      END IF
                  END IF
                  IF(INSTRC(I)(1:1).EQ.'?') THEN
                      SI(I)=1
                      STATST(I)=0
                  ELSE
                      IF(.NOT.ALLB) INSTRC(I)(1:140)=':'//INSTRC(I)(1:139)
                      SI(I)=0
                      STATST(I)=1
                  END IF
              END IF
C
              QBVAL = 0
              QBVAL=ICHAR(INSTRC(I)(1:1))
              IF(QBVAL.EQ.58) THEN
C       THERE IS A COLON, THERE IS STRING DATA
                  STRING(I)=(INSTRC(I)(2:140))
                  STATNW(I)=0
                  STATN1(I)=0
                  STATN2(I)=0
                  STATN3(I)=0
                  STATN4(I)=0
                  STATN5(I)=0
                  DFSTA1(I)=1
                  DFSTA2(I)=1
                  DFSTA3(I)=1
                  DFSTA4(I)=1
                  DFSTA5(I)=1
                  GO TO 1015
              ELSE
                  IF(QBVAL.EQ.63) THEN
C       THE INTERROGATOR IS PRESENT.
                      STRING(I)=(INSTRC(I)(1:1))
                      SI(I)=1
                      STATST(I)=0
                      STATNW(I)=0
                      STATN1(I)=0
                      STATN2(I)=0
                      STATN3(I)=0
                      STATN4(I)=0
                      STATN5(I)=0
                      DFSTA1(I)=1
                      DFSTA2(I)=1
                      DFSTA3(I)=1
                      DFSTA4(I)=1
                      DFSTA5(I)=1
                      GO TO 1015
                  END IF
C
C       THERE IS NUMERIC DATA, RESOLVE IT.
C
C       NOW BREAK OUT THE ONE TO FIVE ALPHANUMERIC
C       REPRESENTATIONS OF THE NUMERIC WORDS, THEN
C       TRANSLATE THEM INTO NUMERIC REAL*8
C       VALUE1S. AS OF 1/9/93 AT 5:14PM, NUMERIC WORDS CAN HAVE
C       NESTED COMMAS.
C
                  STATST(I)=0
                  STATNW(I)=1
                  ANW(I)=INSTRC(I)
                  CALL ATOD(ANW(I),JK_INP,JK_NUM,JK_DF,JJK_FLG1,5,JJK_FLG2,REMAIN)
C
                  IF(.NOT.JJK_FLG2) THEN
C     STRING FOLLOWS 5TH NUMERIC WORD
                      STATST(I)=1
                      STRING(I)(1:80)=REMAIN(1:80)
                  END IF
C
                  DO JK_I = 1,5
                      IF(.NOT.JJK_FLG1(JK_I)) THEN
                          DO JKII=JK_I,5
                              IF(JKII.EQ.1) THEN
                                  DFSTA1(I)=1
                                  NW1(I)=0.0D0
                                  STATN1(I)=0
                              END IF
                              IF(JKII.EQ.2) THEN
                                  DFSTA2(I)=1
                                  NW2(I)=0.0D0
                                  STATN2(I)=0
                              END IF
                              IF(JKII.EQ.3) THEN
                                  DFSTA3(I)=1
                                  NW3(I)=0.0D0
                                  STATN3(I)=0
                              END IF
                              IF(JKII.EQ.4) THEN
                                  DFSTA4(I)=1
                                  NW4(I)=0.0D0
                                  STATN4(I)=0
                              END IF
                              IF(JKII.EQ.5) THEN
                                  DFSTA5(I)=1
                                  NW5(I)=0.0D0
                                  STATN5(I)=0
                              END IF
                          END DO
                      END IF
                  END DO
C
                  IF(JK_DF(1).EQ.1) THEN
                      DFSTA1(I)=1
                      NW1(I)=0.0D0
                      STATN1(I)=0
                  ELSE
                      DFSTA1(I)=0
                      NW1(I)=JK_NUM(1)
                      STATN1(I)=1
                  END IF
                  IF(JK_DF(2).EQ.1) THEN
                      DFSTA2(I)=1
                      NW2(I)=0.0D0
                      STATN2(I)=0
                  ELSE
                      DFSTA2(I)=0
                      NW2(I)=JK_NUM(2)
                      STATN2(I)=1
                  END IF
                  IF(JK_DF(3).EQ.1) THEN
                      DFSTA3(I)=1
                      NW3(I)=0.0D0
                      STATN3(I)=0
                  ELSE
                      DFSTA3(I)=0
                      NW3(I)=JK_NUM(3)
                      STATN3(I)=1
                  END IF
                  IF(JK_DF(4).EQ.1) THEN
                      DFSTA4(I)=1
                      NW4(I)=0.0D0
                      STATN4(I)=0
                  ELSE
                      DFSTA4(I)=0
                      NW4(I)=JK_NUM(4)
                      STATN4(I)=1
                  END IF
                  IF(JK_DF(5).EQ.1) THEN
                      DFSTA5(I)=1
                      NW5(I)=0.0D0
                      STATN5(I)=0
                  ELSE
                      DFSTA5(I)=0
                      NW5(I)=JK_NUM(5)
                      STATN5(I)=1
                  END IF
C

              END IF
C
 1015     CONTINUE
C
          DO 1999 I=1,NSTRUC
C
C       CALL THE CONTROL SUBROUTINE TO BRANCH OFF TO
C       SPECIFIC PROGRAM CMDS.
C
C
              IF(COMMWD(I)(1:8).EQ.'WRITE   '.AND.
     1        QUALWD(I)(1:8).EQ.'        ') THEN
                  QUALWD(I)(1:8)='X       '
                  STATQL(I)=1
              END IF
              WC=COMMWD(I)
              WQ=QUALWD(I)
              WS=STRING(I)
              W1=NW1(I)
              W2=NW2(I)
              W3=NW3(I)
              W4=NW4(I)
              W5=NW5(I)
              SB1=STATBL(I)
              SB2=STBLK2(I)
              SC1=STATCO(I)
              SC2=STATC2(I)
              SQ=STATQL(I)
              SST=STATST(I)
              STI=SI(I)
              S1=STATN1(I)
              S2=STATN2(I)
              S3=STATN3(I)
              S4=STATN4(I)
              S5=STATN5(I)
              IF(S1.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1        .OR.S5.EQ.1) THEN
                  STATNW(I)=1
              ELSE
                  STATNW(I)=0
              END IF
              SN=STATNW(I)
              DF1=DFSTA1(I)
              DF2=DFSTA2(I)
              DF3=DFSTA3(I)
              DF4=DFSTA4(I)
              DF5=DFSTA5(I)
              IF(DF1.EQ.2) DF1=1
              IF(DF2.EQ.2) DF2=1
              IF(DF3.EQ.2) DF3=1
              IF(DF4.EQ.2) DF4=1
              IF(DF5.EQ.2) DF5=1
C       CORRECTION MADE ON 2/10/88
C       FOR ANY NUMERIC WORD WITH A DEFAULT VALUE1, THE
C       CORRESPONDING S1 TO S5 SHOULD BE SET TO 0
C       IF ALL 5 WORDS ARE DEFAULT, SN IS ALSO 0
              IF(DF1.EQ.1) S1=0
              IF(DF2.EQ.1) S2=0
              IF(DF3.EQ.1) S3=0
              IF(DF4.EQ.1) S4=0
              IF(DF5.EQ.1) S5=0
              IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1
     1        .AND.DF5.EQ.1) SN=0
C
C       THIS ERROR WAS FOUND ON 2/10/88 WHEN INSTALLING THE
C       CFGCHG.FOR SUBROUTINE
C
              IF(F47.NE.0) THEN
                  CALL MREA
              ELSE
C     REMOVES , INFRONT OF A : USED IN A STRING 2/14/94
                  CALL PRO4
C
C     HEXAGON/ACCOS-V CONFIGS INPUT
C
C     6/23/05 ADDED A SAVE AND RELOAD AND PATCH CODE TO PROCESS HEXAGON CONFIGS
C     COMMANDS
                  IF(F10.EQ.1.AND.F13.EQ.0.OR.F11.EQ.1.AND.F13.EQ.0) THEN
C     CHECK FOR A HEXAGON COMMAND AND THEN REPLACE IT WITH AN ODP COMMAND SEQUENCE
C     CHECK FOR UPDATE LENS TYPES OF HEXAGON COMMANDS AND ISSUE THE CORRECT ODP COMMANDS
C
C     CV
C
                      IF(WC.EQ.'CV'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
 666                      FORMAT(D23.15)
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='CV '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     RD
C
                      IF(WC.EQ.'RD'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='RD '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     CVR
C
                      IF(WC.EQ.'CVR'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='CVTOR '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     RDR
C
                      IF(WC.EQ.'RDR'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='RDTOR '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     CC
C
                      IF(WC.EQ.'CC'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='CC '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     XD
C
                      IF(WC.EQ.'XD'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='XD '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     YD
C
                      IF(WC.EQ.'YD'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='YD '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     ALPHA
C
                      IF(WC.EQ.'ALPHA'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='ALPHA '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     BETA
C
                      IF(WC.EQ.'BETA'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='BETA '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     GAMMA
C
                      IF(WC.EQ.'GAMMA'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='GAMMA '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     AD
C
                      IF(WC.EQ.'AD'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='AD '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     AE
C
                      IF(WC.EQ.'AE'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='AE '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     AF
C
                      IF(WC.EQ.'AF'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='AF '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     AG
C
                      IF(WC.EQ.'AG'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='CV '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     RN
C
                      IF(WC.EQ.'RN'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          GO TO 1999
                      END IF
C
C     DF
C
                      IF(WC.EQ.'DF'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          GO TO 1999
                      END IF
C
C     TH
C
                      IF(WC.EQ.'TH'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          INPUT='TH '//AHOLD2
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     TEMP
C
                      IF(WC.EQ.'TEMP'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          GO TO 1999
                      END IF
C
C     GLALPHA
C
                      IF(WC.EQ.'GLALPHA'.AND.DF1.EQ.0.AND.DF2.EQ.0) THEN
                          GO TO 1999
                      END IF
C
C     SCY
C
                      IF(WC.EQ.'SCY'.AND.DF1.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          AHOLDWQ=WQ(1:6)
                          HOLDSQ=SQ
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          IF(HOLDSQ.EQ.0) THEN
                              INPUT='SCY '//AHOLD2
                          ELSE
                              INPUT='SCY '//AHOLDWQ//' '//AHOLD2
                          END IF
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C     SCX
C
                      IF(WC.EQ.'SCX'.AND.DF1.EQ.0) THEN
                          HOLD1=W1
                          HOLD2=W2
                          HOLD3=W3
                          HOLD4=W4
                          HOLD5=W5
                          AHOLDWQ=WQ(1:6)
                          HOLDSQ=SQ
                          WRITE(AHOLD1,666) HOLD1
                          WRITE(AHOLD2,666) HOLD2
                          WRITE(AHOLD3,666) HOLD3
                          WRITE(AHOLD4,666) HOLD4
                          WRITE(AHOLD5,666) HOLD5
                          SAVE_KDP(31)=SAVEINPT(31)
                          INPUT='U L'
                          CALL PROCES
                          INPUT='CHG '//AHOLD1
                          CALL PROCES
                          IF(HOLDSQ.EQ.0) THEN
                              INPUT='SCX '//AHOLD2
                          ELSE
                              INPUT='SCX '//AHOLDWQ//' '//AHOLD2
                          END IF
                          CALL PROCES
                          INPUT='EOS'
                          CALL PROCES
                          REST_KDP(31)=RESTINPT(31)
                          GO TO 1999
                      END IF
C
C
C
                  END IF
                  CALL CONTRO
              END IF
C
 1999     CONTINUE
          RETURN
C
      END
