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

C       THIRD SET OF UTILTIY ROUTINES GO HERE

C SUB RGMATH.FOR
      SUBROUTINE RGMATH
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO DO SYMBOLIC REGISTER
C       MATHEMATICS FUNCTIONS SUCH AS PLUS, MINUS ETC.
C       ALSO DOES THE CLREG COMMAND
C
          CHARACTER ACCWRD*8
C
          INTEGER ACCSUB,ACCCNT
C
          REAL*8
     1    PLUS,MINUS,MPY,DIV,MOVE,SINE,COSINE,TANN,SINHH,COSHH,
     2    TANHH,SROOT,POW,LLOG10,LLN,EEXP,RECIP,ABSOL,SSGN,
     3    MINTGR,MFRAC,FACT,CHSIGN,DEGRA,ASINE,ACOSIN,ATANN,
     4    RANDMM,V1,V2
C
          EXTERNAL RANDMM
C
          COMMON/ACCSB/ACCWRD
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          INCLUDE 'datmai.inc'
C
          IF(WC.NE.'WRITE') THEN
              IF(SST.EQ.1.OR.SN.EQ.1) THEN
                  OUTLYNE=WC//' ONLY TAKES QUALIFIER INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WC.EQ.'WRITE') THEN
              IF(SN.EQ.1) THEN
                  OUTLYNE=WC//' TAKES NO NUMERIC INPUT'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
C
          IF(WC.EQ.'WRITE'.OR.WC.EQ.'PRIREG') THEN
              CALL WRITE
              RETURN
          END IF
          IF(WS.EQ.'CLREG') THEN
C
              REG(1:50)=0.0D0
              RETURN
          END IF
          IF(WQ.NE.'A'.AND.WQ.NE.'B'.AND.WQ.NE.'C'
     1    .AND.WQ.NE.'D'.AND.WQ.NE.'E'.AND.WQ.NE.'F'.AND.
     2    WQ.NE.'G'.AND.WQ.NE.'H'.AND.WQ.NE.'Y'.AND.WQ.NE.'Z'
     3    .AND.WQ.NE.'T'.AND.WQ.NE.'IX'.AND.WQ.NE.'IY'
     4    .AND.WQ.NE.'IZ'.AND.WQ.NE.'IT'.AND.WQ.NE.'I'.AND.
     5    WQ.NE.'ITEST'.AND.WQ.NE.'J'.AND.WQ.NE.'JTEST'
     6    .AND.WQ.NE.'X'.AND.WQ.NE.' '.AND.WQ.NE.'K'.AND.WQ.NE.'L'
     7    .AND.WQ.NE.'M'.AND.WQ.NE.'N'.AND.WQ.NE.'KTEST'.AND.WQ.NE.'MTEST'
     8    .AND.WQ.NE.'LTEST'.AND.WQ.NE.'NTEST') THEN
              OUTLYNE='INVALID REGISTER NAME'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SN.EQ.1.OR.SST.EQ.1) THEN
              OUTLYNE= WC//' ONLY TAKES QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ACCSUB.EQ.1) THEN
              IF(WQ.EQ.'ACC'.OR.WQ.EQ.'X'.OR.WQ.EQ.' ') THEN
                  WQ=ACCWRD
                  ACCCNT=ACCCNT-1
                  IF(ACCCNT.EQ.0) ACCSUB=0
              END IF
          END IF
          REG(40)=REG(9)
          IF(WC.EQ.'PLUS')   REG(9)=PLUS()
C
          IF(WC.EQ.'MAXVAL') THEN
C       UPDATE LASTX
              REG(40)=REG(9)
              CALL JK_MAXVAL(V1,V2)
              REG(10) = V2
              REG(9) = V1
          END IF
C
          IF(WC.EQ.'MINVAL') THEN
C       UPDATE LASTX
              REG(40)=REG(9)
              CALL JK_MINVAL(V1,V2)
              REG(10) = V2
              REG(9) = V1
          END IF
C
          IF(WC.EQ.'MINUS')  REG(9)=MINUS()
          IF(WC.EQ.'MPY')    REG(9)=MPY()
          IF(WC.EQ.'DIV')    REG(9)=DIV()
          IF(WC.EQ.'MOVE')   REG(9)=MOVE()
          IF(WC.EQ.'SIN')    REG(9)=SINE()
          IF(WC.EQ.'COS')    REG(9)=COSINE()
          IF(WC.EQ.'TAN')    REG(9)=TANN()
          IF(WC.EQ.'SINH')   REG(9)=SINHH()
          IF(WC.EQ.'COSH')   REG(9)=COSHH()
          IF(WC.EQ.'TANH')   REG(9)=TANHH()
          IF(WC.EQ.'SQRT')   REG(9)=SROOT()
          IF(WC.EQ.'EXP')    REG(9)=EEXP()
          IF(WC.EQ.'POW')    REG(9)=POW()
          IF(WC.EQ.'LOG10')  REG(9)=LLOG10()
          IF(WC.EQ.'LN')     REG(9)=LLN()
          IF(WC.EQ.'RECIP')  REG(9)=RECIP()
          IF(WC.EQ.'STORE')  THEN
              CALL STORE
          END IF
          IF(WC.EQ.'SGN')    REG(9)=SSGN()
          IF(WC.EQ.'INTGR')  REG(9)=MINTGR()
          IF(WC.EQ.'FRAC')   REG(9)=MFRAC()
          IF(WC.EQ.'FACT')   REG(9)=FACT()
          IF(WC.EQ.'CHS')    REG(9)=CHSIGN()
          IF(WC.EQ.'RTD')    REG(9)=DEGRA()
          IF(WC.EQ.'DTR')    REG(9)=DEGRA()
          IF(WC.EQ.'ASIN')   REG(9)=ASINE()
          IF(WC.EQ.'ACOS')    REG(9)=ACOSIN()
          IF(WC.EQ.'ATAN')   REG(9)=ATANN()
          IF(WC.EQ.'PI')     THEN
              CALL PI
          END IF
          IF(WC.EQ.'RAND')   REG(9)=RANDMM()
          IF(WC.EQ.'ABS')    REG(9)=ABSOL()
          RETURN
      END
C SUB PLUS.FOR
      FUNCTION PLUS()
          IMPLICIT NONE
          REAL*8 PLUS
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') PLUS=REG(9)+REG(1)
          IF(WQ.EQ.'B') PLUS=REG(9)+REG(2)
          IF(WQ.EQ.'C') PLUS=REG(9)+REG(3)
          IF(WQ.EQ.'D') PLUS=REG(9)+REG(4)
          IF(WQ.EQ.'E') PLUS=REG(9)+REG(5)
          IF(WQ.EQ.'F') PLUS=REG(9)+REG(6)
          IF(WQ.EQ.'G') PLUS=REG(9)+REG(7)
          IF(WQ.EQ.'H') PLUS=REG(9)+REG(8)
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1     PLUS=REG(9)+REG(9)
          IF(WQ.EQ.'Y') PLUS=REG(9)+REG(10)
          IF(WQ.EQ.'Z') PLUS=REG(9)+REG(11)
          IF(WQ.EQ.'T') PLUS=REG(9)+REG(12)
          IF(WQ.EQ.'IX') PLUS=REG(9)+REG(13)
          IF(WQ.EQ.'IY') PLUS=REG(9)+REG(14)
          IF(WQ.EQ.'IZ') PLUS=REG(9)+REG(15)
          IF(WQ.EQ.'IT') PLUS=REG(9)+REG(16)
          IF(WQ.EQ.'I') PLUS=REG(9)+REG(17)
          IF(WQ.EQ.'ITEST') PLUS=REG(9)+REG(18)
          IF(WQ.EQ.'J') PLUS=REG(9)+REG(19)
          IF(WQ.EQ.'JTEST') PLUS=REG(9)+REG(20)
          IF(WQ.EQ.'K') PLUS=REG(9)+REG(21)
          IF(WQ.EQ.'L') PLUS=REG(9)+REG(22)
          IF(WQ.EQ.'M') PLUS=REG(9)+REG(23)
          IF(WQ.EQ.'N') PLUS=REG(9)+REG(24)
          IF(WQ.EQ.'KTEST') PLUS=REG(9)+REG(25)
          IF(WQ.EQ.'LTEST') PLUS=REG(9)+REG(26)
          IF(WQ.EQ.'MTEST') PLUS=REG(9)+REG(27)
          IF(WQ.EQ.'NTEST') PLUS=REG(9)+REG(28)
          RETURN
      END
C SUB MINUS.FOR
      FUNCTION MINUS()
          IMPLICIT NONE
          REAL*8 MINUS
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') MINUS=REG(9)-REG(1)
          IF(WQ.EQ.'B') MINUS=REG(9)-REG(2)
          IF(WQ.EQ.'C') MINUS=REG(9)-REG(3)
          IF(WQ.EQ.'D') MINUS=REG(9)-REG(4)
          IF(WQ.EQ.'E') MINUS=REG(9)-REG(5)
          IF(WQ.EQ.'F') MINUS=REG(9)-REG(6)
          IF(WQ.EQ.'G') MINUS=REG(9)-REG(7)
          IF(WQ.EQ.'H') MINUS=REG(9)-REG(8)
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1     MINUS=REG(9)-REG(9)
          IF(WQ.EQ.'Y') MINUS=REG(9)-REG(10)
          IF(WQ.EQ.'Z') MINUS=REG(9)-REG(11)
          IF(WQ.EQ.'T') MINUS=REG(9)-REG(12)
          IF(WQ.EQ.'IX') MINUS=REG(9)-REG(13)
          IF(WQ.EQ.'IY') MINUS=REG(9)-REG(14)
          IF(WQ.EQ.'IZ') MINUS=REG(9)-REG(15)
          IF(WQ.EQ.'IT') MINUS=REG(9)-REG(16)
          IF(WQ.EQ.'I') MINUS=REG(9)-REG(17)
          IF(WQ.EQ.'ITEST') MINUS=REG(9)-REG(18)
          IF(WQ.EQ.'J') MINUS=REG(9)-REG(19)
          IF(WQ.EQ.'JTEST') MINUS=REG(9)-REG(20)
          IF(WQ.EQ.'K') MINUS=REG(9)-REG(21)
          IF(WQ.EQ.'L') MINUS=REG(9)-REG(22)
          IF(WQ.EQ.'M') MINUS=REG(9)-REG(23)
          IF(WQ.EQ.'N') MINUS=REG(9)-REG(24)
          IF(WQ.EQ.'KTEST') MINUS=REG(9)-REG(25)
          IF(WQ.EQ.'LTEST') MINUS=REG(9)-REG(26)
          IF(WQ.EQ.'MTEST') MINUS=REG(9)-REG(27)
          IF(WQ.EQ.'NTEST') MINUS=REG(9)-REG(28)
          RETURN
      END
C SUB MPY.FOR
      FUNCTION MPY()
          IMPLICIT NONE
          REAL*8 MPY
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') MPY=REG(9)*REG(1)
          IF(WQ.EQ.'B') MPY=REG(9)*REG(2)
          IF(WQ.EQ.'C') MPY=REG(9)*REG(3)
          IF(WQ.EQ.'D') MPY=REG(9)*REG(4)
          IF(WQ.EQ.'E') MPY=REG(9)*REG(5)
          IF(WQ.EQ.'F') MPY=REG(9)*REG(6)
          IF(WQ.EQ.'G') MPY=REG(9)*REG(7)
          IF(WQ.EQ.'H') MPY=REG(9)*REG(8)
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1     MPY=REG(9)*REG(9)
          IF(WQ.EQ.'Y') MPY=REG(9)*REG(10)
          IF(WQ.EQ.'Z') MPY=REG(9)*REG(11)
          IF(WQ.EQ.'T') MPY=REG(9)*REG(12)
          IF(WQ.EQ.'IX') MPY=REG(9)*REG(13)
          IF(WQ.EQ.'IY') MPY=REG(9)*REG(14)
          IF(WQ.EQ.'IZ') MPY=REG(9)*REG(15)
          IF(WQ.EQ.'IT') MPY=REG(9)*REG(16)
          IF(WQ.EQ.'I') MPY=REG(9)*REG(17)
          IF(WQ.EQ.'ITEST') MPY=REG(9)*REG(18)
          IF(WQ.EQ.'J') MPY=REG(9)*REG(19)
          IF(WQ.EQ.'JTEST') MPY=REG(9)*REG(20)
          IF(WQ.EQ.'K') MPY=REG(9)*REG(21)
          IF(WQ.EQ.'L') MPY=REG(9)*REG(22)
          IF(WQ.EQ.'M') MPY=REG(9)*REG(23)
          IF(WQ.EQ.'N') MPY=REG(9)*REG(24)
          IF(WQ.EQ.'KTEST') MPY=REG(9)*REG(25)
          IF(WQ.EQ.'LTEST') MPY=REG(9)*REG(26)
          IF(WQ.EQ.'MTEST') MPY=REG(9)*REG(27)
          IF(WQ.EQ.'NTEST') MPY=REG(9)*REG(28)
          RETURN
      END
C SUB MOVE.FOR
      FUNCTION MOVE()
          IMPLICIT NONE
          REAL*8 MOVE
          INCLUDE 'datmai.inc'
          IF(WQ.NE.'X'.AND.WQ.NE.'Y'.AND.WQ.NE.'Z'.AND.WQ.NE.'T') THEN
              REG(40)=REG(9)
          END IF
          IF(WQ.EQ.'A') MOVE=REG(1)
          IF(WQ.EQ.'B') MOVE=REG(2)
          IF(WQ.EQ.'C') MOVE=REG(3)
          IF(WQ.EQ.'D') MOVE=REG(4)
          IF(WQ.EQ.'E') MOVE=REG(5)
          IF(WQ.EQ.'F') MOVE=REG(6)
          IF(WQ.EQ.'G') MOVE=REG(7)
          IF(WQ.EQ.'H') MOVE=REG(8)
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1     MOVE=REG(9)
          IF(WQ.EQ.'Y') MOVE=REG(10)
          IF(WQ.EQ.'Z') MOVE=REG(11)
          IF(WQ.EQ.'T') MOVE=REG(12)
          IF(WQ.EQ.'IX') MOVE=REG(13)
          IF(WQ.EQ.'IY') MOVE=REG(14)
          IF(WQ.EQ.'IZ') MOVE=REG(15)
          IF(WQ.EQ.'IT') MOVE=REG(16)
          IF(WQ.EQ.'I') MOVE=REG(17)
          IF(WQ.EQ.'ITEST') MOVE=REG(18)
          IF(WQ.EQ.'J') MOVE=REG(19)
          IF(WQ.EQ.'JTEST') MOVE=REG(20)
          IF(WQ.EQ.'K') MOVE=REG(21)
          IF(WQ.EQ.'L') MOVE=REG(22)
          IF(WQ.EQ.'M') MOVE=REG(23)
          IF(WQ.EQ.'N') MOVE=REG(24)
          IF(WQ.EQ.'KTEST') MOVE=REG(25)
          IF(WQ.EQ.'LTEST') MOVE=REG(26)
          IF(WQ.EQ.'MTEST') MOVE=REG(27)
          IF(WQ.EQ.'NTEST') MOVE=REG(28)
          RETURN
      END
C SUB DIV.FOR
      FUNCTION DIV()
          IMPLICIT NONE
          REAL*8 DIV
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') THEN
              IF(REG(1).NE.0.0D0) DIV=REG(9)/REG(1)
              IF(REG(1).EQ.0.0D0) THEN
                  OUTLYNE='REG A =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'B') THEN
              IF(REG(2).NE.0.0D0) DIV=REG(9)/REG(2)
              IF(REG(2).EQ.0.0D0) THEN
                  OUTLYNE='REG B =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'C') THEN
              IF(REG(3).NE.0.0D0) DIV=REG(9)/REG(3)
              IF(REG(3).EQ.0.0D0) THEN
                  OUTLYNE='REG C =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'D') THEN
              IF(REG(4).NE.0.0D0) DIV=REG(9)/REG(4)
              IF(REG(4).EQ.0.0D0) THEN
                  OUTLYNE='REG D =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'E') THEN
              IF(REG(5).NE.0.0D0) DIV=REG(9)/REG(5)
              IF(REG(5).EQ.0.0D0) THEN
                  OUTLYNE='REG E =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'F') THEN
              IF(REG(6).NE.0.0D0) DIV=REG(9)/REG(6)
              IF(REG(6).EQ.0.0D0) THEN
                  OUTLYNE='REG F =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'G') THEN
              IF(REG(7).NE.0.0D0) DIV=REG(9)/REG(7)
              IF(REG(7).EQ.0.0D0) THEN
                  OUTLYNE='REG G =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'H') THEN
              IF(REG(8).NE.0.0D0) DIV=REG(9)/REG(8)
              IF(REG(8).EQ.0.0D0) THEN
                  OUTLYNE='REG H =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X') THEN
              IF(REG(9).NE.0.0D0) DIV=REG(9)/REG(9)
              IF(REG(9).EQ.0.0D0) THEN
                  OUTLYNE='ACCUMULATOR =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'Y') THEN
              IF(REG(10).NE.0.0D0) DIV=REG(9)/REG(10)
              IF(REG(10).EQ.0.0D0) THEN
                  OUTLYNE='Y REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'Z') THEN
              IF(REG(11).NE.0.0D0) DIV=REG(9)/REG(11)
              IF(REG(11).EQ.0.0D0) THEN
                  OUTLYNE='Z REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'T') THEN
              IF(REG(12).NE.0.0D0) DIV=REG(9)/REG(12)
              IF(REG(12).EQ.0.0D0) THEN
                  OUTLYNE='T REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'IX') THEN
              IF(REG(13).NE.0.0D0) DIV=REG(9)/REG(13)
              IF(REG(13).EQ.0.0D0) THEN
                  OUTLYNE='IX REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'IY') THEN
              IF(REG(14).NE.0.0D0) DIV=REG(9)/REG(14)
              IF(REG(14).EQ.0.0D0) THEN
                  OUTLYNE='IY REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'IZ') THEN
              IF(REG(15).NE.0.0D0) DIV=REG(9)/REG(15)
              IF(REG(15).EQ.0.0D0) THEN
                  OUTLYNE='IZ REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'IT') THEN
              IF(REG(16).NE.0.0D0) DIV=REG(9)/REG(16)
              IF(REG(16).EQ.0.0D0) THEN
                  OUTLYNE='IT REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'I') THEN
              IF(REG(17).NE.0.0D0) DIV=REG(9)/REG(17)
              IF(REG(17).EQ.0.0D0) THEN
                  OUTLYNE='I REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'ITEST') THEN
              IF(REG(18).NE.0.0D0) DIV=REG(9)/REG(18)
              IF(REG(18).EQ.0.0D0) THEN
                  OUTLYNE='ITEST REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'J') THEN
              IF(REG(19).NE.0.0D0) DIV=REG(9)/REG(19)
              IF(REG(19).EQ.0.0D0) THEN
                  OUTLYNE='J REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'JTEST') THEN
              IF(REG(20).NE.0.0D0) DIV=REG(9)/REG(20)
              IF(REG(20).EQ.0.0D0) THEN
                  OUTLYNE='JTEST REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'K') THEN
              IF(REG(21).NE.0.0D0) DIV=REG(9)/REG(21)
              IF(REG(21).EQ.0.0D0) THEN
                  OUTLYNE='K REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'L') THEN
              IF(REG(22).NE.0.0D0) DIV=REG(9)/REG(22)
              IF(REG(22).EQ.0.0D0) THEN
                  OUTLYNE='L REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'M') THEN
              IF(REG(23).NE.0.0D0) DIV=REG(9)/REG(23)
              IF(REG(23).EQ.0.0D0) THEN
                  OUTLYNE='M REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'N') THEN
              IF(REG(24).NE.0.0D0) DIV=REG(9)/REG(24)
              IF(REG(24).EQ.0.0D0) THEN
                  OUTLYNE='N REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'KTEST') THEN
              IF(REG(25).NE.0.0D0) DIV=REG(9)/REG(25)
              IF(REG(25).EQ.0.0D0) THEN
                  OUTLYNE='KTEST REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'LTEST') THEN
              IF(REG(26).NE.0.0D0) DIV=REG(9)/REG(26)
              IF(REG(26).EQ.0.0D0) THEN
                  OUTLYNE='LTEST REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'MTEST') THEN
              IF(REG(27).NE.0.0D0) DIV=REG(9)/REG(27)
              IF(REG(27).EQ.0.0D0) THEN
                  OUTLYNE='MTEST REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          IF(WQ.EQ.'NTEST') THEN
              IF(REG(28).NE.0.0D0) DIV=REG(9)/REG(28)
              IF(REG(28).EQ.0.0D0) THEN
                  OUTLYNE='NTEST REG =ZERO, NO DIVISION DONE'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
              END IF
          END IF
          RETURN
      END
C SUB SINE.FOR
      FUNCTION SINE()
          IMPLICIT NONE
          REAL*8 SINE
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') SINE=DSIN(REG(1))
          IF(WQ.EQ.'B') SINE=DSIN(REG(2))
          IF(WQ.EQ.'C') SINE=DSIN(REG(3))
          IF(WQ.EQ.'D') SINE=DSIN(REG(4))
          IF(WQ.EQ.'E') SINE=DSIN(REG(5))
          IF(WQ.EQ.'F') SINE=DSIN(REG(6))
          IF(WQ.EQ.'G') SINE=DSIN(REG(7))
          IF(WQ.EQ.'H') SINE=DSIN(REG(8))
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    SINE=DSIN(REG(9))
          IF(WQ.EQ.'Y') SINE=DSIN(REG(10))
          IF(WQ.EQ.'Z') SINE=DSIN(REG(11))
          IF(WQ.EQ.'T') SINE=DSIN(REG(12))
          IF(WQ.EQ.'IX') SINE=DSIN(REG(13))
          IF(WQ.EQ.'IY') SINE=DSIN(REG(14))
          IF(WQ.EQ.'IZ') SINE=DSIN(REG(15))
          IF(WQ.EQ.'IT') SINE=DSIN(REG(16))
          IF(WQ.EQ.'I') SINE=DSIN(REG(17))
          IF(WQ.EQ.'ITEST') SINE=DSIN(REG(18))
          IF(WQ.EQ.'J') SINE=DSIN(REG(19))
          IF(WQ.EQ.'JTEXT') SINE=DSIN(REG(20))
          IF(WQ.EQ.'K') SINE=DSIN(REG(21))
          IF(WQ.EQ.'L') SINE=DSIN(REG(22))
          IF(WQ.EQ.'M') SINE=DSIN(REG(23))
          IF(WQ.EQ.'N') SINE=DSIN(REG(24))
          IF(WQ.EQ.'KTEXT') SINE=DSIN(REG(25))
          IF(WQ.EQ.'LTEXT') SINE=DSIN(REG(26))
          IF(WQ.EQ.'MTEXT') SINE=DSIN(REG(27))
          IF(WQ.EQ.'NTEXT') SINE=DSIN(REG(20))
          RETURN
      END
C SUB COSINE.FOR
      FUNCTION COSINE()
          IMPLICIT NONE
          REAL*8 COSINE
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') COSINE=DCOS(REG(1))
          IF(WQ.EQ.'B') COSINE=DCOS(REG(2))
          IF(WQ.EQ.'C') COSINE=DCOS(REG(3))
          IF(WQ.EQ.'D') COSINE=DCOS(REG(4))
          IF(WQ.EQ.'E') COSINE=DCOS(REG(5))
          IF(WQ.EQ.'F') COSINE=DCOS(REG(6))
          IF(WQ.EQ.'G') COSINE=DCOS(REG(7))
          IF(WQ.EQ.'H') COSINE=DCOS(REG(8))
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    COSINE=DCOS(REG(9))
          IF(WQ.EQ.'Y') COSINE=DCOS(REG(10))
          IF(WQ.EQ.'Z') COSINE=DCOS(REG(11))
          IF(WQ.EQ.'T') COSINE=DCOS(REG(12))
          IF(WQ.EQ.'IX') COSINE=DCOS(REG(13))
          IF(WQ.EQ.'IY') COSINE=DCOS(REG(14))
          IF(WQ.EQ.'IZ') COSINE=DCOS(REG(15))
          IF(WQ.EQ.'IT') COSINE=DCOS(REG(16))
          IF(WQ.EQ.'I') COSINE=DCOS(REG(17))
          IF(WQ.EQ.'ITEST') COSINE=DCOS(REG(18))
          IF(WQ.EQ.'J') COSINE=DCOS(REG(19))
          IF(WQ.EQ.'JTEXT') COSINE=DCOS(REG(20))
          IF(WQ.EQ.'K') COSINE=DCOS(REG(21))
          IF(WQ.EQ.'L') COSINE=DCOS(REG(22))
          IF(WQ.EQ.'M') COSINE=DCOS(REG(23))
          IF(WQ.EQ.'N') COSINE=DCOS(REG(24))
          IF(WQ.EQ.'KTEXT') COSINE=DCOS(REG(25))
          IF(WQ.EQ.'LTEXT') COSINE=DCOS(REG(26))
          IF(WQ.EQ.'JTEXT') COSINE=DCOS(REG(27))
          IF(WQ.EQ.'NTEXT') COSINE=DCOS(REG(28))
          RETURN
      END
C SUB TANN.FOR
      FUNCTION TANN()
          IMPLICIT NONE
          REAL*8 TANN
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') TANN=DTAN(REG(1))
          IF(WQ.EQ.'B') TANN=DTAN(REG(2))
          IF(WQ.EQ.'C') TANN=DTAN(REG(3))
          IF(WQ.EQ.'D') TANN=DTAN(REG(4))
          IF(WQ.EQ.'E') TANN=DTAN(REG(5))
          IF(WQ.EQ.'F') TANN=DTAN(REG(6))
          IF(WQ.EQ.'G') TANN=DTAN(REG(7))
          IF(WQ.EQ.'H') TANN=DTAN(REG(8))
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    TANN=DTAN(REG(9))
          IF(WQ.EQ.'Y') TANN=DTAN(REG(10))
          IF(WQ.EQ.'Z') TANN=DTAN(REG(11))
          IF(WQ.EQ.'T') TANN=DTAN(REG(12))
          IF(WQ.EQ.'IX') TANN=DTAN(REG(13))
          IF(WQ.EQ.'IY') TANN=DTAN(REG(14))
          IF(WQ.EQ.'IZ') TANN=DTAN(REG(15))
          IF(WQ.EQ.'IT') TANN=DTAN(REG(16))
          IF(WQ.EQ.'I') TANN=DTAN(REG(17))
          IF(WQ.EQ.'ITEST') TANN=DTAN(REG(18))
          IF(WQ.EQ.'J') TANN=DTAN(REG(19))
          IF(WQ.EQ.'JTEXT') TANN=DTAN(REG(20))
          IF(WQ.EQ.'K') TANN=DTAN(REG(21))
          IF(WQ.EQ.'L') TANN=DTAN(REG(22))
          IF(WQ.EQ.'M') TANN=DTAN(REG(23))
          IF(WQ.EQ.'N') TANN=DTAN(REG(24))
          IF(WQ.EQ.'KTEXT') TANN=DTAN(REG(25))
          IF(WQ.EQ.'LTEXT') TANN=DTAN(REG(26))
          IF(WQ.EQ.'MTEXT') TANN=DTAN(REG(27))
          IF(WQ.EQ.'NTEXT') TANN=DTAN(REG(28))
          RETURN
      END
C SUB SINHH.FOR
      FUNCTION SINHH()
          IMPLICIT NONE
          REAL*8 SINHH
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') SINHH=DSINH(REG(1))
          IF(WQ.EQ.'B') SINHH=DSINH(REG(2))
          IF(WQ.EQ.'C') SINHH=DSINH(REG(3))
          IF(WQ.EQ.'D') SINHH=DSINH(REG(4))
          IF(WQ.EQ.'E') SINHH=DSINH(REG(5))
          IF(WQ.EQ.'F') SINHH=DSINH(REG(6))
          IF(WQ.EQ.'G') SINHH=DSINH(REG(7))
          IF(WQ.EQ.'H') SINHH=DSINH(REG(8))
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    SINHH=DSINH(REG(9))
          IF(WQ.EQ.'Y') SINHH=DSINH(REG(10))
          IF(WQ.EQ.'Z') SINHH=DSINH(REG(11))
          IF(WQ.EQ.'T') SINHH=DSINH(REG(12))
          IF(WQ.EQ.'IX') SINHH=DSINH(REG(13))
          IF(WQ.EQ.'IY') SINHH=DSINH(REG(14))
          IF(WQ.EQ.'IZ') SINHH=DSINH(REG(15))
          IF(WQ.EQ.'IT') SINHH=DSINH(REG(16))
          IF(WQ.EQ.'I') SINHH=DSINH(REG(17))
          IF(WQ.EQ.'ITEST') SINHH=DSINH(REG(18))
          IF(WQ.EQ.'J') SINHH=DSINH(REG(19))
          IF(WQ.EQ.'JTEXT') SINHH=DSINH(REG(20))
          IF(WQ.EQ.'K') SINHH=DSINH(REG(21))
          IF(WQ.EQ.'L') SINHH=DSINH(REG(22))
          IF(WQ.EQ.'M') SINHH=DSINH(REG(23))
          IF(WQ.EQ.'N') SINHH=DSINH(REG(24))
          IF(WQ.EQ.'KTEXT') SINHH=DSINH(REG(25))
          IF(WQ.EQ.'JTEXT') SINHH=DSINH(REG(26))
          IF(WQ.EQ.'MTEXT') SINHH=DSINH(REG(27))
          IF(WQ.EQ.'NTEXT') SINHH=DSINH(REG(28))
          RETURN
      END
C SUB COSHH.FOR
      FUNCTION COSHH()
          IMPLICIT NONE
          REAL*8 COSHH
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') COSHH=DCOSH(REG(1))
          IF(WQ.EQ.'B') COSHH=DCOSH(REG(2))
          IF(WQ.EQ.'C') COSHH=DCOSH(REG(3))
          IF(WQ.EQ.'D') COSHH=DCOSH(REG(4))
          IF(WQ.EQ.'E') COSHH=DCOSH(REG(5))
          IF(WQ.EQ.'F') COSHH=DCOSH(REG(6))
          IF(WQ.EQ.'G') COSHH=DCOSH(REG(7))
          IF(WQ.EQ.'H') COSHH=DCOSH(REG(8))
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1                  COSHH=DCOSH(REG(9))
          IF(WQ.EQ.'Y') COSHH=DCOSH(REG(10))
          IF(WQ.EQ.'Z') COSHH=DCOSH(REG(11))
          IF(WQ.EQ.'T') COSHH=DCOSH(REG(12))
          IF(WQ.EQ.'IX')COSHH=DCOSH(REG(13))
          IF(WQ.EQ.'IY')COSHH=DCOSH(REG(14))
          IF(WQ.EQ.'IZ')COSHH=DCOSH(REG(15))
          IF(WQ.EQ.'IT')COSHH=DCOSH(REG(16))
          IF(WQ.EQ.'I') COSHH=DCOSH(REG(17))
          IF(WQ.EQ.'ITEST') COSHH=DCOSH(REG(18))
          IF(WQ.EQ.'J') COSHH=DCOSH(REG(19))
          IF(WQ.EQ.'JTEXT') COSHH=DCOSH(REG(20))
          IF(WQ.EQ.'K') COSHH=DCOSH(REG(21))
          IF(WQ.EQ.'L') COSHH=DCOSH(REG(22))
          IF(WQ.EQ.'M') COSHH=DCOSH(REG(23))
          IF(WQ.EQ.'N') COSHH=DCOSH(REG(24))
          IF(WQ.EQ.'KTEXT') COSHH=DCOSH(REG(25))
          IF(WQ.EQ.'LTEXT') COSHH=DCOSH(REG(26))
          IF(WQ.EQ.'MTEXT') COSHH=DCOSH(REG(27))
          IF(WQ.EQ.'NTEXT') COSHH=DCOSH(REG(28))
          RETURN
      END
C SUB TANHH.FOR
      FUNCTION TANHH()
          IMPLICIT NONE
          REAL*8 TANHH
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') TANHH=DTANH(REG(1))
          IF(WQ.EQ.'B') TANHH=DTANH(REG(2))
          IF(WQ.EQ.'C') TANHH=DTANH(REG(3))
          IF(WQ.EQ.'D') TANHH=DTANH(REG(4))
          IF(WQ.EQ.'E') TANHH=DTANH(REG(5))
          IF(WQ.EQ.'F') TANHH=DTANH(REG(6))
          IF(WQ.EQ.'G') TANHH=DTANH(REG(7))
          IF(WQ.EQ.'H') TANHH=DTANH(REG(8))
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    TANHH=DTANH(REG(9))
          IF(WQ.EQ.'Y') TANHH=DTANH(REG(10))
          IF(WQ.EQ.'Z') TANHH=DTANH(REG(11))
          IF(WQ.EQ.'T') TANHH=DTANH(REG(12))
          IF(WQ.EQ.'IX') TANHH=DTANH(REG(13))
          IF(WQ.EQ.'IY') TANHH=DTANH(REG(14))
          IF(WQ.EQ.'IZ') TANHH=DTANH(REG(15))
          IF(WQ.EQ.'IT') TANHH=DTANH(REG(16))
          IF(WQ.EQ.'I') TANHH=DTANH(REG(17))
          IF(WQ.EQ.'ITEST') TANHH=DTANH(REG(18))
          IF(WQ.EQ.'J') TANHH=DTANH(REG(19))
          IF(WQ.EQ.'JTEXT') TANHH=DTANH(REG(20))
          IF(WQ.EQ.'K') TANHH=DTANH(REG(21))
          IF(WQ.EQ.'L') TANHH=DTANH(REG(22))
          IF(WQ.EQ.'M') TANHH=DTANH(REG(23))
          IF(WQ.EQ.'N') TANHH=DTANH(REG(24))
          IF(WQ.EQ.'KTEXT') TANHH=DTANH(REG(25))
          IF(WQ.EQ.'LTEXT') TANHH=DTANH(REG(26))
          IF(WQ.EQ.'MTEXT') TANHH=DTANH(REG(27))
          IF(WQ.EQ.'NTEXT') TANHH=DTANH(REG(28))
          RETURN
      END
C SUB SROOT.FOR
      FUNCTION SROOT()
          IMPLICIT NONE
          REAL*8 SROOT
          INCLUDE 'datmai.inc'
          GO TO 11
 10       CONTINUE
          OUTLYNE='INVALID NEG. SQRT ARGUMENT'
          CALL SHOWIT(1)
          OUTLYNE='FOR REG '//WC
          CALL SHOWIT(1)
          OUTLYNE='ACCUMULATOR NOT ALTERED'
          CALL SHOWIT(1)
          CALL MACFAL
          SROOT=0.0D0                 !Add by ENDO
          RETURN
 11       CONTINUE
          REG(40)=REG(9)
          IF(WQ.EQ.'A') THEN
              IF(REG(1).GE.0.0D0) SROOT=DSQRT(REG(1))
              IF(REG(1).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'B') THEN
              IF(REG(2).GE.0.0D0) SROOT=DSQRT(REG(2))
              IF(REG(2).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'C') THEN
              IF(REG(3).GE.0.0D0) SROOT=DSQRT(REG(3))
              IF(REG(3).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'D') THEN
              IF(REG(4).GE.0.0D0) SROOT=DSQRT(REG(4))
              IF(REG(4).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'E') THEN
              IF(REG(5).GE.0.0D0) SROOT=DSQRT(REG(5))
              IF(REG(5).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'F') THEN
              IF(REG(6).GE.0.0D0) SROOT=DSQRT(REG(6))
              IF(REG(6).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'G') THEN
              IF(REG(7).GE.0.0D0) SROOT=DSQRT(REG(7))
              IF(REG(7).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'H') THEN
              IF(REG(8).GE.0.0D0) SROOT=DSQRT(REG(8))
              IF(REG(8).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X') THEN
              IF(REG(9).GE.0.0D0) SROOT=DSQRT(REG(9))
              IF(REG(9).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'Y') THEN
              IF(REG(10).GE.0.0D0) SROOT=DSQRT(REG(10))
              IF(REG(10).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'Z') THEN
              IF(REG(11).GE.0.0D0) SROOT=DSQRT(REG(11))
              IF(REG(11).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'T') THEN
              IF(REG(12).GE.0.0D0) SROOT=DSQRT(REG(12))
              IF(REG(12).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'IX') THEN
              IF(REG(13).GE.0.0D0) SROOT=DSQRT(REG(13))
              IF(REG(13).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'IY') THEN
              IF(REG(14).GE.0.0D0) SROOT=DSQRT(REG(14))
              IF(REG(14).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'IZ') THEN
              IF(REG(15).GE.0.0D0) SROOT=DSQRT(REG(15))
              IF(REG(15).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'IT') THEN
              IF(REG(16).GE.0.0D0) SROOT=DSQRT(REG(16))
              IF(REG(16).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'I') THEN
              IF(REG(17).GE.0.0D0) SROOT=DSQRT(REG(17))
              IF(REG(17).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'ITEST') THEN
              IF(REG(18).GE.0.0D0) SROOT=DSQRT(REG(18))
              IF(REG(18).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'J') THEN
              IF(REG(19).GE.0.0D0) SROOT=DSQRT(REG(19))
              IF(REG(19).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'JTEST') THEN
              IF(REG(20).GE.0.0D0) SROOT=DSQRT(REG(20))
              IF(REG(20).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'K') THEN
              IF(REG(21).GE.0.0D0) SROOT=DSQRT(REG(21))
              IF(REG(21).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'L') THEN
              IF(REG(22).GE.0.0D0) SROOT=DSQRT(REG(22))
              IF(REG(22).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'M') THEN
              IF(REG(23).GE.0.0D0) SROOT=DSQRT(REG(23))
              IF(REG(23).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'N') THEN
              IF(REG(24).GE.0.0D0) SROOT=DSQRT(REG(24))
              IF(REG(24).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'KTEST') THEN
              IF(REG(25).GE.0.0D0) SROOT=DSQRT(REG(25))
              IF(REG(25).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'LTEST') THEN
              IF(REG(26).GE.0.0D0) SROOT=DSQRT(REG(26))
              IF(REG(26).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'MTEST') THEN
              IF(REG(27).GE.0.0D0) SROOT=DSQRT(REG(27))
              IF(REG(27).LT.0.0D0) GO TO 10
          END IF
          IF(WQ.EQ.'NTEST') THEN
              IF(REG(28).GE.0.0D0) SROOT=DSQRT(REG(28))
              IF(REG(28).LT.0.0D0) GO TO 10
          END IF
          RETURN
      END
C SUB POW.FOR
      FUNCTION POW()
          IMPLICIT NONE
          REAL*8 POW
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') THEN
              IF(REG(9).EQ.0.0D0.AND.REG(1).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(1)
              END IF
          END IF
          IF(WQ.EQ.'A') THEN
              IF(REG(9).EQ.0.0D0.AND.REG(1).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(1)
              END IF
          END IF
          IF(WQ.EQ.'B') THEN
              IF(REG(9).EQ.0.0D0.AND.REG(2).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(2)
              END IF
          END IF
          IF(WQ.EQ.'C') THEN
              IF(REG(9).EQ.0.0D0.AND.REG(3).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(3)
              END IF
          END IF
          IF(WQ.EQ.'D') THEN
              IF(REG(9).EQ.0.0D0.AND.REG(4).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(4)
              END IF
          END IF
          IF(WQ.EQ.'E') THEN
              IF(REG(9).EQ.0.0D0.AND.REG(5).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(5)
              END IF
          END IF
          IF(WQ.EQ.'F') THEN
              IF(REG(9).EQ.0.0D0.AND.REG(6).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(6)
              END IF
          END IF
          IF(WQ.EQ.'G') THEN
              IF(REG(9).EQ.0.0D0.AND.REG(7).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(7)
              END IF
          END IF
          IF(WQ.EQ.'H') THEN
              IF(REG(9).EQ.0.0D0.AND.REG(8).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(8)
              END IF
          END IF
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X') THEN
              IF(REG(9).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(9)
              END IF
          END IF
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X') THEN
              IF(REG(9).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(9)
              END IF
          END IF
          IF(WQ.EQ.'Y') THEN
              IF(REG(10).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(10)
              END IF
          END IF
          IF(WQ.EQ.'Z') THEN
              IF(REG(11).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(11)
              END IF
          END IF
          IF(WQ.EQ.'T') THEN
              IF(REG(12).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(12)
              END IF
          END IF
          IF(WQ.EQ.'IX') THEN
              IF(REG(13).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(13)
              END IF
          END IF
          IF(WQ.EQ.'IY') THEN
              IF(REG(14).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(14)
              END IF
          END IF
          IF(WQ.EQ.'IZ') THEN
              IF(REG(15).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(15)
              END IF
          END IF
          IF(WQ.EQ.'IT') THEN
              IF(REG(16).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(16)
              END IF
          END IF
          IF(WQ.EQ.'I') THEN
              IF(REG(17).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(17)
              END IF
          END IF
          IF(WQ.EQ.'ITEST') THEN
              IF(REG(18).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(18)
              END IF
          END IF
          IF(WQ.EQ.'J') THEN
              IF(REG(19).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(19)
              END IF
          END IF
          IF(WQ.EQ.'JTEST') THEN
              IF(REG(20).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(20)
              END IF
          END IF
          IF(WQ.EQ.'K') THEN
              IF(REG(21).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(21)
              END IF
          END IF
          IF(WQ.EQ.'L') THEN
              IF(REG(22).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(22)
              END IF
          END IF
          IF(WQ.EQ.'M') THEN
              IF(REG(23).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(23)
              END IF
          END IF
          IF(WQ.EQ.'N') THEN
              IF(REG(24).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(24)
              END IF
          END IF
          IF(WQ.EQ.'KTEST') THEN
              IF(REG(25).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(25)
              END IF
          END IF
          IF(WQ.EQ.'LTEST') THEN
              IF(REG(26).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(26)
              END IF
          END IF
          IF(WQ.EQ.'MTEST') THEN
              IF(REG(27).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(27)
              END IF
          END IF
          IF(WQ.EQ.'NTEST') THEN
              IF(REG(28).EQ.0.0D0) THEN
                  POW=1.0D0
              ELSE
                  POW=REG(9)**REG(28)
              END IF
          END IF
          RETURN
      END
C SUB EEXP.FOR
      FUNCTION EEXP()
          IMPLICIT NONE
          REAL*8 EEXP,REGGY
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') REGGY=REG(1)
          IF(WQ.EQ.'B') REGGY=REG(2)
          IF(WQ.EQ.'C') REGGY=REG(3)
          IF(WQ.EQ.'D') REGGY=REG(4)
          IF(WQ.EQ.'E') REGGY=REG(5)
          IF(WQ.EQ.'F') REGGY=REG(6)
          IF(WQ.EQ.'G') REGGY=REG(7)
          IF(WQ.EQ.'H') REGGY=REG(8)
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    REGGY=REG(9)
          IF(WQ.EQ.'Y') REGGY=REG(10)
          IF(WQ.EQ.'Z') REGGY=REG(11)
          IF(WQ.EQ.'T') REGGY=REG(12)
          IF(WQ.EQ.'IX') REGGY=REG(13)
          IF(WQ.EQ.'IY') REGGY=REG(14)
          IF(WQ.EQ.'IZ') REGGY=REG(15)
          IF(WQ.EQ.'IT') REGGY=REG(16)
          IF(WQ.EQ.'I') REGGY=REG(17)
          IF(WQ.EQ.'ITEST') REGGY=REG(18)
          IF(WQ.EQ.'J') REGGY=REG(19)
          IF(WQ.EQ.'JTEST') REGGY=REG(20)
          IF(WQ.EQ.'K') REGGY=REG(21)
          IF(WQ.EQ.'L') REGGY=REG(22)
          IF(WQ.EQ.'M') REGGY=REG(23)
          IF(WQ.EQ.'N') REGGY=REG(24)
          IF(WQ.EQ.'KTEST') REGGY=REG(25)
          IF(WQ.EQ.'LTEST') REGGY=REG(26)
          IF(WQ.EQ.'MTEST') REGGY=REG(27)
          IF(WQ.EQ.'NTEST') REGGY=REG(20)
          IF(REGGY.GT.88.0) THEN
              REG(9)=1E38
              OUTLYNE='VALUE IN REG '//WQ//' EXCEEDED MAX ALLOWED'
              CALL SHOWIT(1)
              OUTLYNE='MAXIMUM ARGUMENT FOR "EXP" IS +88.0'
              CALL SHOWIT(1)
              OUTLYNE='ACCUMULATOR SET TO 1.0E38'
              CALL SHOWIT(1)
          END IF
          EEXP=DEXP(REGGY)
          RETURN
      END
C SUB LLOG10.FOR
      FUNCTION LLOG10()
          IMPLICIT NONE
          REAL*8 LLOG10
          INCLUDE 'datmai.inc'
          GO TO 201
 200      CONTINUE
          OUTLYNE='ARG FOR "LOG10" IN REG '//WQ//' IS ZERO'
          CALL SHOWIT(1)
          OUTLYNE='ACCULULATOR SET TO ZERO'
          CALL SHOWIT(1)
          REG(9)=0.0D0
          CALL MACFAL
          LLOG10=0.0D0                !Add by ENDO
          RETURN
 201      REG(40)=REG(9)
          IF(WQ.EQ.'A') THEN
              IF(REG(1).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(1))
          END IF
          IF(WQ.EQ.'B') THEN
              IF(REG(2).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(2))
          END IF
          IF(WQ.EQ.'C') THEN
              IF(REG(3).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(3))
          END IF
          IF(WQ.EQ.'D') THEN
              IF(REG(4).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(4))
          END IF
          IF(WQ.EQ.'E') THEN
              IF(REG(5).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(5))
          END IF
          IF(WQ.EQ.'F') THEN
              IF(REG(6).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(6))
          END IF
          IF(WQ.EQ.'G') THEN
              IF(REG(7).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(7))
          END IF
          IF(WQ.EQ.'H') THEN
              IF(REG(8).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(8))
          END IF
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X') THEN
              IF(REG(9).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(9))
          END IF
          IF(WQ.EQ.'Y') THEN
              IF(REG(10).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(10))
          END IF
          IF(WQ.EQ.'Z') THEN
              IF(REG(11).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(11))
          END IF
          IF(WQ.EQ.'T') THEN
              IF(REG(12).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(12))
          END IF
          IF(WQ.EQ.'IX') THEN
              IF(REG(13).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(13))
          END IF
          IF(WQ.EQ.'IY') THEN
              IF(REG(14).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(14))
          END IF
          IF(WQ.EQ.'IZ') THEN
              IF(REG(15).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(15))
          END IF
          IF(WQ.EQ.'IT') THEN
              IF(REG(16).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(16))
          END IF
          IF(WQ.EQ.'I') THEN
              IF(REG(17).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(17))
          END IF
          IF(WQ.EQ.'ITEST') THEN
              IF(REG(18).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(18))
          END IF
          IF(WQ.EQ.'J') THEN
              IF(REG(19).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(19))
          END IF
          IF(WQ.EQ.'JTEST') THEN
              IF(REG(20).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(20))
          END IF
          IF(WQ.EQ.'K') THEN
              IF(REG(21).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(21))
          END IF
          IF(WQ.EQ.'L') THEN
              IF(REG(22).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(22))
          END IF
          IF(WQ.EQ.'M') THEN
              IF(REG(23).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(23))
          END IF
          IF(WQ.EQ.'N') THEN
              IF(REG(24).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(24))
          END IF
          IF(WQ.EQ.'KTEST') THEN
              IF(REG(25).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(25))
          END IF
          IF(WQ.EQ.'LTEST') THEN
              IF(REG(26).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(26))
          END IF
          IF(WQ.EQ.'MTEST') THEN
              IF(REG(27).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(27))
          END IF
          IF(WQ.EQ.'NTEST') THEN
              IF(REG(28).LE.0.0D0) GO TO 200
              LLOG10=DLOG10(REG(28))
          END IF
          RETURN
      END
C SUB LLN.FOR
      FUNCTION LLN()
          IMPLICIT NONE
          REAL*8 LLN
          INCLUDE 'datmai.inc'
          GO TO 201
 200      CONTINUE
          OUTLYNE='ARG FOR "LN" IN REG '//WQ//' IS ZERO'
          CALL SHOWIT(1)
          OUTLYNE='ACCULULATOR SET TO ZERO'
          CALL SHOWIT(1)
          REG(9)=0.0D0
          CALL MACFAL
          LLN=0               !Add by ENDO
          RETURN
 201      REG(40)=REG(9)
          IF(WQ.EQ.'A') THEN
              IF(REG(1).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(1))
          END IF
          IF(WQ.EQ.'B') THEN
              IF(REG(2).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(2))
          END IF
          IF(WQ.EQ.'C') THEN
              IF(REG(3).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(3))
          END IF
          IF(WQ.EQ.'D') THEN
              IF(REG(4).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(4))
          END IF
          IF(WQ.EQ.'E') THEN
              IF(REG(5).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(5))
          END IF
          IF(WQ.EQ.'F') THEN
              IF(REG(6).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(6))
          END IF
          IF(WQ.EQ.'G') THEN
              IF(REG(7).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(7))
          END IF
          IF(WQ.EQ.'H') THEN
              IF(REG(8).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(8))
          END IF
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X') THEN
              IF(REG(9).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(9))
          END IF
          IF(WQ.EQ.'Y') THEN
              IF(REG(10).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(10))
          END IF
          IF(WQ.EQ.'Z') THEN
              IF(REG(11).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(11))
          END IF
          IF(WQ.EQ.'T') THEN
              IF(REG(12).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(12))
          END IF
          IF(WQ.EQ.'IX') THEN
              IF(REG(13).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(13))
          END IF
          IF(WQ.EQ.'IY') THEN
              IF(REG(14).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(14))
          END IF
          IF(WQ.EQ.'IZ') THEN
              IF(REG(15).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(15))
          END IF
          IF(WQ.EQ.'IT') THEN
              IF(REG(16).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(16))
          END IF
          IF(WQ.EQ.'I') THEN
              IF(REG(17).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(17))
          END IF
          IF(WQ.EQ.'ITEST') THEN
              IF(REG(18).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(18))
          END IF
          IF(WQ.EQ.'J') THEN
              IF(REG(19).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(19))
          END IF
          IF(WQ.EQ.'JTEST') THEN
              IF(REG(20).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(20))
          END IF
          IF(WQ.EQ.'K') THEN
              IF(REG(21).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(21))
          END IF
          IF(WQ.EQ.'L') THEN
              IF(REG(22).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(22))
          END IF
          IF(WQ.EQ.'M') THEN
              IF(REG(23).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(23))
          END IF
          IF(WQ.EQ.'N') THEN
              IF(REG(24).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(24))
          END IF
          IF(WQ.EQ.'KTEST') THEN
              IF(REG(25).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(25))
          END IF
          IF(WQ.EQ.'LTEST') THEN
              IF(REG(26).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(26))
          END IF
          IF(WQ.EQ.'MTEST') THEN
              IF(REG(27).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(27))
          END IF
          IF(WQ.EQ.'NTEST') THEN
              IF(REG(28).LE.0.0D0) GO TO 200
              LLN=DLOG(REG(28))
          END IF
          RETURN
      END
C SUB RECIP.FOR
      FUNCTION RECIP()
          IMPLICIT NONE
          REAL*8 RECIP
          INCLUDE 'datmai.inc'
          GO TO 201
 200      CONTINUE
          OUTLYNE='ARG FOR "RECIP" IN REG '//WQ//' IS ZERO'
          CALL SHOWIT(1)
          OUTLYNE='ACCULULATOR SET TO ZERO'
          CALL SHOWIT(1)
          REG(9)=0.0D0
          CALL MACFAL
          RECIP=0                 !Add by ENDO
          RETURN
 201      REG(40)=REG(9)
          IF(WQ.EQ.'A') THEN
              IF(REG(1).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(1))
          END IF
          IF(WQ.EQ.'B') THEN
              IF(REG(2).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(2))
          END IF
          IF(WQ.EQ.'C') THEN
              IF(REG(3).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(3))
          END IF
          IF(WQ.EQ.'D') THEN
              IF(REG(4).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(4))
          END IF
          IF(WQ.EQ.'E') THEN
              IF(REG(5).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(5))
          END IF
          IF(WQ.EQ.'F') THEN
              IF(REG(6).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(6))
          END IF
          IF(WQ.EQ.'G') THEN
              IF(REG(7).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(7))
          END IF
          IF(WQ.EQ.'H') THEN
              IF(REG(8).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(8))
          END IF
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X') THEN
              IF(REG(9).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(9))
          END IF
          IF(WQ.EQ.'Y') THEN
              IF(REG(10).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(10))
          END IF
          IF(WQ.EQ.'Z') THEN
              IF(REG(11).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(11))
          END IF
          IF(WQ.EQ.'T') THEN
              IF(REG(12).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(12))
          END IF
          IF(WQ.EQ.'IX') THEN
              IF(REG(13).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(13))
          END IF
          IF(WQ.EQ.'IY') THEN
              IF(REG(14).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(14))
          END IF
          IF(WQ.EQ.'IZ') THEN
              IF(REG(15).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(15))
          END IF
          IF(WQ.EQ.'IT') THEN
              IF(REG(16).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(16))
          END IF
          IF(WQ.EQ.'I') THEN
              IF(REG(17).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(17))
          END IF
          IF(WQ.EQ.'ITEST') THEN
              IF(REG(18).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(18))
          END IF
          IF(WQ.EQ.'J') THEN
              IF(REG(19).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(19))
          END IF
          IF(WQ.EQ.'JTEST') THEN
              IF(REG(20).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(20))
          END IF
          IF(WQ.EQ.'K') THEN
              IF(REG(21).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(21))
          END IF
          IF(WQ.EQ.'L') THEN
              IF(REG(22).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(22))
          END IF
          IF(WQ.EQ.'M') THEN
              IF(REG(23).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(23))
          END IF
          IF(WQ.EQ.'N') THEN
              IF(REG(24).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(24))
          END IF
          IF(WQ.EQ.'KTEST') THEN
              IF(REG(25).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(25))
          END IF
          IF(WQ.EQ.'LTEST') THEN
              IF(REG(26).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(26))
          END IF
          IF(WQ.EQ.'MTEST') THEN
              IF(REG(27).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(27))
          END IF
          IF(WQ.EQ.'NTEST') THEN
              IF(REG(28).LE.0.0D0) GO TO 200
              RECIP=1.0D0/(REG(28))
          END IF
          RETURN
      END
C SUB ABSOL.FOR
      FUNCTION ABSOL()
          IMPLICIT NONE
          REAL*8 ABSOL
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') ABSOL=DABS(REG(1))
          IF(WQ.EQ.'B') ABSOL=DABS(REG(2))
          IF(WQ.EQ.'C') ABSOL=DABS(REG(3))
          IF(WQ.EQ.'D') ABSOL=DABS(REG(4))
          IF(WQ.EQ.'E') ABSOL=DABS(REG(5))
          IF(WQ.EQ.'F') ABSOL=DABS(REG(6))
          IF(WQ.EQ.'G') ABSOL=DABS(REG(7))
          IF(WQ.EQ.'H') ABSOL=DABS(REG(8))
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1     ABSOL=DABS(REG(9))
          IF(WQ.EQ.'Y') ABSOL=DABS(REG(10))
          IF(WQ.EQ.'Z') ABSOL=DABS(REG(11))
          IF(WQ.EQ.'T') ABSOL=DABS(REG(12))
          IF(WQ.EQ.'IX') ABSOL=DABS(REG(13))
          IF(WQ.EQ.'IY') ABSOL=DABS(REG(14))
          IF(WQ.EQ.'IZ') ABSOL=DABS(REG(15))
          IF(WQ.EQ.'IT') ABSOL=DABS(REG(16))
          IF(WQ.EQ.'I') ABSOL=DABS(REG(17))
          IF(WQ.EQ.'ITEST') ABSOL=DABS(REG(18))
          IF(WQ.EQ.'J') ABSOL=DABS(REG(19))
          IF(WQ.EQ.'JTEST') ABSOL=DABS(REG(20))
          IF(WQ.EQ.'K') ABSOL=DABS(REG(21))
          IF(WQ.EQ.'L') ABSOL=DABS(REG(22))
          IF(WQ.EQ.'M') ABSOL=DABS(REG(23))
          IF(WQ.EQ.'N') ABSOL=DABS(REG(24))
          IF(WQ.EQ.'KTEST') ABSOL=DABS(REG(25))
          IF(WQ.EQ.'LTEST') ABSOL=DABS(REG(26))
          IF(WQ.EQ.'MTEST') ABSOL=DABS(REG(27))
          IF(WQ.EQ.'NTEST') ABSOL=DABS(REG(28))
          RETURN
      END
C SUB STORE.FOR
      SUBROUTINE STORE
          IMPLICIT NONE
C       THIS SUBROUTINE IS USED TO STORE THE CONTENTS OF THE
C       ACCUMULATOR INTO REGISTERS A THROUGH H, X,Y,Z,T,IX,IY
C       IZ,OR IT. THE STORE COMMAND IS THE INVERSE OF THE MOVE
C       COMMAND. ALSO WORKS ON I,J,ITEST AND JTEST
C       ALSO WORKS ON K,L,KTEST AND LTEST
C       ALSO WORKS ON M,N,MTEST AND NTEST
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') REG(1)=REG(9)
          IF(WQ.EQ.'B') REG(2)=REG(9)
          IF(WQ.EQ.'C') REG(3)=REG(9)
          IF(WQ.EQ.'D') REG(4)=REG(9)
          IF(WQ.EQ.'E') REG(5)=REG(9)
          IF(WQ.EQ.'F') REG(6)=REG(9)
          IF(WQ.EQ.'G') REG(7)=REG(9)
          IF(WQ.EQ.'H') REG(8)=REG(9)
          IF(WQ.EQ.' '.OR.WQ.EQ.'X'.OR.WQ.EQ.'ACC')THEN

              OUTLYNE='STORE X NOT DEFINED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'Y') REG(10)=REG(9)
          IF(WQ.EQ.'Z') REG(11)=REG(9)
          IF(WQ.EQ.'T') REG(12)=REG(9)
          IF(WQ.EQ.'IX') THEN
              REG(30)=REG(13)
              REG(13)=REG(9)
          END IF
          IF(WQ.EQ.'IY') REG(14)=REG(9)
          IF(WQ.EQ.'IZ') REG(15)=REG(9)
          IF(WQ.EQ.'IT') REG(16)=REG(9)
          IF(WQ.EQ.'I') REG(17)=REG(9)
          IF(WQ.EQ.'ITEST') REG(18)=REG(9)
          IF(WQ.EQ.'J') REG(19)=REG(9)
          IF(WQ.EQ.'JTEST') REG(20)=REG(9)
          IF(WQ.EQ.'K') REG(21)=REG(9)
          IF(WQ.EQ.'L') REG(22)=REG(9)
          IF(WQ.EQ.'M') REG(23)=REG(9)
          IF(WQ.EQ.'N') REG(24)=REG(9)
          IF(WQ.EQ.'KTEST') REG(25)=REG(9)
          IF(WQ.EQ.'LTEST') REG(26)=REG(9)
          IF(WQ.EQ.'MTEST') REG(27)=REG(9)
          IF(WQ.EQ.'NTEST') REG(28)=REG(9)
          RETURN
      END
C SUB JK_MINVAL.FOR
      SUBROUTINE JK_MINVAL(V1,V2)
          IMPLICIT NONE
C       THIS SUBROUTINE IS USED TO FIND THE MINUMUM VALUE
C       ALL THE VALUES STORED IN THE MAXREG GENERAL PURPOSE REGISTERS
C       THE MINVAL IS RETURNED IN V1
C     V2 STORES THE ADRESS OF THE MINIMUM VALUE
C
          REAL*8 V1,V2
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
          V1=GPREG(1)
          V2=1.0D0
          DO I=1,MAXREG
              IF(GPREG(I).LT.V1) THEN
                  V1=GPREG(I)
                  V2=DBLE(I)
              END IF
          END DO
          RETURN
      END
C SUB JK_MAXVAL.FOR
      SUBROUTINE JK_MAXVAL(V1,V2)
          IMPLICIT NONE
C       THIS SUBROUTINE IS USED TO FIND THE MAXIMUM VALUE
C       ALL THE VALUES STORED IN THE MAXREG GENERAL PURPOSE REGISTERS
C       THE MAXVAL IS RETURNED IN V1
C     V2 STORES THE ADRESS OF THE MAXIMUM VALUE
C
          REAL*8 V1,V2
C
          INTEGER I
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datsub.inc'
C
          V1=GPREG(1)
          V2=1.0D0
          DO I=1,MAXREG
              IF(GPREG(I).GT.V1) THEN
                  V1=GPREG(I)
                  V2=DBLE(I)
              END IF
          END DO
          RETURN
      END



C SUB SSGN.FOR
      FUNCTION SSGN()
          IMPLICIT NONE
          REAL*8 SSGN
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') THEN
              REG(9)=REG(1)
              GO TO 200
          END IF
          IF(WQ.EQ.'B') THEN
              REG(9)=REG(2)
              GO TO 200
          END IF
          IF(WQ.EQ.'C') THEN
              REG(9)=REG(3)
              GO TO 200
          END IF
          IF(WQ.EQ.'D') THEN
              REG(9)=REG(4)
              GO TO 200
          END IF
          IF(WQ.EQ.'E') THEN
              REG(9)=REG(5)
              GO TO 200
          END IF
          IF(WQ.EQ.'F') THEN
              REG(9)=REG(6)
              GO TO 200
          END IF
          IF(WQ.EQ.'G') THEN
              REG(9)=REG(7)
              GO TO 200
          END IF
          IF(WQ.EQ.'H') THEN
              REG(9)=REG(8)
              GO TO 200
          END IF
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X') THEN
              GO TO 200
          END IF
          IF(WQ.EQ.'Y') THEN
              REG(9)=REG(10)
              GO TO 200
          END IF
          IF(WQ.EQ.'Z') THEN
              REG(9)=REG(11)
              GO TO 200
          END IF
          IF(WQ.EQ.'T') THEN
              REG(9)=REG(12)
              GO TO 200
          END IF
          IF(WQ.EQ.'IX') THEN
              REG(9)=REG(13)
              GO TO 200
          END IF
          IF(WQ.EQ.'IY') THEN
              REG(9)=REG(14)
              GO TO 200
          END IF
          IF(WQ.EQ.'IZ') THEN
              REG(9)=REG(15)
              GO TO 200
          END IF
          IF(WQ.EQ.'IT') THEN
              REG(9)=REG(16)
              GO TO 200
          END IF
          IF(WQ.EQ.'I') THEN
              REG(9)=REG(17)
              GO TO 200
          END IF
          IF(WQ.EQ.'ITEST') THEN
              REG(9)=REG(18)
              GO TO 200
          END IF
          IF(WQ.EQ.'J') THEN
              REG(9)=REG(19)
              GO TO 200
          END IF
          IF(WQ.EQ.'JTEST') THEN
              REG(9)=REG(20)
              GO TO 200
          END IF
          IF(WQ.EQ.'K') THEN
              REG(9)=REG(21)
              GO TO 200
          END IF
          IF(WQ.EQ.'L') THEN
              REG(9)=REG(22)
              GO TO 200
          END IF
          IF(WQ.EQ.'M') THEN
              REG(9)=REG(23)
              GO TO 200
          END IF
          IF(WQ.EQ.'N') THEN
              REG(9)=REG(24)
              GO TO 200
          END IF
          IF(WQ.EQ.'KTEST') THEN
              REG(9)=REG(25)
              GO TO 200
          END IF
          IF(WQ.EQ.'LTEST') THEN
              REG(9)=REG(26)
              GO TO 200
          END IF
          IF(WQ.EQ.'MTEST') THEN
              REG(9)=REG(27)
              GO TO 200
          END IF
          IF(WQ.EQ.'NTEST') THEN
              REG(9)=REG(28)
              GO TO 200
          END IF

          OUTLYNE='Error:SSGN Set to ZERO.' !Add by ENDO
          CALL SHOWIT(1)
          CALL MACFAL
          SSGN=0

          RETURN

 200      IF(REG(9).EQ.0.0D0) SSGN=0.0D0
          IF(REG(9).GT.0.0D0) SSGN=1.0D0
          IF(REG(9).LT.0.0D0) SSGN=-1.0D0
          RETURN
      END
C SUB MINTGR.FOR
      FUNCTION MINTGR()
          IMPLICIT NONE
          REAL*8 MINTGR
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'B') MINTGR=INT(REG(2))
          IF(WQ.EQ.'C') MINTGR=INT(REG(3))
          IF(WQ.EQ.'D') MINTGR=INT(REG(4))
          IF(WQ.EQ.'E') MINTGR=INT(REG(5))
          IF(WQ.EQ.'F') MINTGR=INT(REG(6))
          IF(WQ.EQ.'G') MINTGR=INT(REG(7))
          IF(WQ.EQ.'H') MINTGR=INT(REG(8))
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1     MINTGR=INT(REG(9))
          IF(WQ.EQ.'Y') MINTGR=INT(REG(10))
          IF(WQ.EQ.'Z') MINTGR=INT(REG(11))
          IF(WQ.EQ.'T') MINTGR=INT(REG(12))
          IF(WQ.EQ.'IX') MINTGR=INT(REG(13))
          IF(WQ.EQ.'IY') MINTGR=INT(REG(14))
          IF(WQ.EQ.'IZ') MINTGR=INT(REG(15))
          IF(WQ.EQ.'IT') MINTGR=INT(REG(16))
          IF(WQ.EQ.'I') MINTGR=INT(REG(17))
          IF(WQ.EQ.'ITEST') MINTGR=INT(REG(18))
          IF(WQ.EQ.'J') MINTGR=INT(REG(19))
          IF(WQ.EQ.'JTEST') MINTGR=INT(REG(20))
          IF(WQ.EQ.'K') MINTGR=INT(REG(21))
          IF(WQ.EQ.'L') MINTGR=INT(REG(22))
          IF(WQ.EQ.'M') MINTGR=INT(REG(23))
          IF(WQ.EQ.'N') MINTGR=INT(REG(24))
          IF(WQ.EQ.'KTEST') MINTGR=INT(REG(25))
          IF(WQ.EQ.'LTEST') MINTGR=INT(REG(26))
          IF(WQ.EQ.'MTEST') MINTGR=INT(REG(27))
          IF(WQ.EQ.'NTEST') MINTGR=INT(REG(28))
          RETURN
      END
C SUB MFRAC.FOR
      FUNCTION MFRAC()
          IMPLICIT NONE
          REAL*8 MFRAC
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') MFRAC=REG(1)-INT(REG(1))
          IF(WQ.EQ.'B') MFRAC=REG(2)-INT(REG(2))
          IF(WQ.EQ.'C') MFRAC=REG(3)-INT(REG(3))
          IF(WQ.EQ.'D') MFRAC=REG(4)-INT(REG(4))
          IF(WQ.EQ.'E') MFRAC=REG(5)-INT(REG(5))
          IF(WQ.EQ.'F') MFRAC=REG(6)-INT(REG(6))
          IF(WQ.EQ.'G') MFRAC=REG(7)-INT(REG(7))
          IF(WQ.EQ.'H') MFRAC=REG(8)-INT(REG(8))
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1     MFRAC=REG(9)-INT(REG(9))
          IF(WQ.EQ.'Y') MFRAC=REG(10)-INT(REG(10))
          IF(WQ.EQ.'Z') MFRAC=REG(11)-INT(REG(11))
          IF(WQ.EQ.'T') MFRAC=REG(12)-INT(REG(12))
          IF(WQ.EQ.'IX') MFRAC=REG(13)-INT(REG(13))
          IF(WQ.EQ.'IY') MFRAC=REG(14)-INT(REG(14))
          IF(WQ.EQ.'IZ') MFRAC=REG(15)-INT(REG(15))
          IF(WQ.EQ.'IT') MFRAC=REG(16)-INT(REG(16))
          IF(WQ.EQ.'I') MFRAC=REG(17)-INT(REG(17))
          IF(WQ.EQ.'ITEST') MFRAC=REG(18)-INT(REG(18))
          IF(WQ.EQ.'J') MFRAC=REG(19)-INT(REG(19))
          IF(WQ.EQ.'JTEST') MFRAC=REG(20)-INT(REG(20))
          IF(WQ.EQ.'K') MFRAC=REG(21)-INT(REG(21))
          IF(WQ.EQ.'L') MFRAC=REG(22)-INT(REG(22))
          IF(WQ.EQ.'M') MFRAC=REG(23)-INT(REG(23))
          IF(WQ.EQ.'N') MFRAC=REG(24)-INT(REG(24))
          IF(WQ.EQ.'KTEST') MFRAC=REG(25)-INT(REG(25))
          IF(WQ.EQ.'LTEST') MFRAC=REG(26)-INT(REG(26))
          IF(WQ.EQ.'MTEST') MFRAC=REG(27)-INT(REG(27))
          IF(WQ.EQ.'NTEST') MFRAC=REG(28)-INT(REG(28))
          RETURN
      END


C SUB FACT.FOR
      FUNCTION FACT()

          IMPLICIT NONE
          REAL*8 FACT
          REAL*8 DARG,NFACT,REGG
          INTEGER ARG,I
          INCLUDE 'datmai.inc'
          IF(WQ.EQ.'A') REGG=REG(1)
          IF(WQ.EQ.'B') REGG=REG(2)
          IF(WQ.EQ.'C') REGG=REG(3)
          IF(WQ.EQ.'D') REGG=REG(4)
          IF(WQ.EQ.'E') REGG=REG(5)
          IF(WQ.EQ.'F') REGG=REG(6)
          IF(WQ.EQ.'G') REGG=REG(7)
          IF(WQ.EQ.'H') REGG=REG(8)
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1     REGG=REG(9)
          IF(WQ.EQ.'Y') REGG=REG(10)
          IF(WQ.EQ.'Z') REGG=REG(11)
          IF(WQ.EQ.'T') REGG=REG(12)
          IF(WQ.EQ.'IX') REGG=REG(13)
          IF(WQ.EQ.'IY') REGG=REG(14)
          IF(WQ.EQ.'IZ') REGG=REG(15)
          IF(WQ.EQ.'IT') REGG=REG(16)
          IF(WQ.EQ.'I') REGG=REG(17)
          IF(WQ.EQ.'ITEST') REGG=REG(18)
          IF(WQ.EQ.'J') REGG=REG(19)
          IF(WQ.EQ.'JTEST') REGG=REG(20)
          IF(WQ.EQ.'K') REGG=REG(21)
          IF(WQ.EQ.'L') REGG=REG(22)
          IF(WQ.EQ.'M') REGG=REG(23)
          IF(WQ.EQ.'N') REGG=REG(24)
          IF(WQ.EQ.'KTEST') REGG=REG(25)
          IF(WQ.EQ.'LTEST') REGG=REG(26)
          IF(WQ.EQ.'MTEST') REGG=REG(27)
          IF(WQ.EQ.'NTEST') REGG=REG(28)
C
C       NOW TEST FOR NEGATIVE VALUE
          IF(REGG.LT.0.0D0) THEN
              OUTLYNE=
     1        'NEGATIVE FACTORIAL ARGUMENT IN REG '//WQ
              CALL SHOWIT(1)
              OUTLYNE='ACCUMULATOR NOT ALTERED'
              CALL SHOWIT(1)
              CALL MACFAL
              FACT=0              !Add by ENDO
              RETURN
          END IF
C       CONVERT TO INTEGER
          ARG=INT(REGG)
          DARG=DBLE(ARG)
          IF((REGG-DARG).NE.0) THEN
              OUTLYNE='REG '//WQ//':'
              CALL SHOWIT(1)
              OUTLYNE='NON-INTEGER ARGUMENT FOR FACTORIAL NOT ALLOWED'
              CALL SHOWIT(1)
              OUTLYNE='ACCUMULATOR NOT ALTERED'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(ARG.GT.33) THEN
              OUTLYNE='REG '//WQ//':'
              CALL SHOWIT(1)
              OUTLYNE='MAXIMUM FACTORIAL ARGUMENT IS 33'
              CALL SHOWIT(1)
              OUTLYNE='ACCUMULATOR NOT ALTERED'
              CALL SHOWIT(1)
              CALL MACFAL
              FACT=0                  !Add by ENDO
              RETURN
          END IF
C       NOW CALCULATE FACTORIAL
          IF(ARG.EQ.0) FACT=1.0D0
          IF(ARG.EQ.1) FACT=1.0D0
          IF(ARG.GT.1) THEN
              NFACT=1.0D0
              DO 5 I=1,ARG
                  NFACT=NFACT*DBLE(I)
 5            CONTINUE
              FACT=NFACT
          END IF
          REG(40)=REG(9)
          RETURN
      END


C SUB CHSIGN.FOR
      FUNCTION CHSIGN()
          IMPLICIT NONE
          REAL*8 CHSIGN
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') CHSIGN=-REG(1)
          IF(WQ.EQ.'B') CHSIGN=-REG(2)
          IF(WQ.EQ.'C') CHSIGN=-REG(3)
          IF(WQ.EQ.'D') CHSIGN=-REG(4)
          IF(WQ.EQ.'E') CHSIGN=-REG(5)
          IF(WQ.EQ.'F') CHSIGN=-REG(6)
          IF(WQ.EQ.'G') CHSIGN=-REG(7)
          IF(WQ.EQ.'H') CHSIGN=-REG(8)
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    CHSIGN=-REG(9)
          IF(WQ.EQ.'Y') CHSIGN=-REG(10)
          IF(WQ.EQ.'Z') CHSIGN=-REG(11)
          IF(WQ.EQ.'T') CHSIGN=-REG(12)
          IF(WQ.EQ.'IX') CHSIGN=-REG(13)
          IF(WQ.EQ.'IY') CHSIGN=-REG(14)
          IF(WQ.EQ.'IZ') CHSIGN=-REG(15)
          IF(WQ.EQ.'IT') CHSIGN=-REG(16)
          IF(WQ.EQ.'I') CHSIGN=-REG(17)
          IF(WQ.EQ.'ITEST') CHSIGN=-REG(18)
          IF(WQ.EQ.'J') CHSIGN=-REG(19)
          IF(WQ.EQ.'JTEST') CHSIGN=-REG(20)
          IF(WQ.EQ.'K') CHSIGN=-REG(21)
          IF(WQ.EQ.'L') CHSIGN=-REG(22)
          IF(WQ.EQ.'M') CHSIGN=-REG(23)
          IF(WQ.EQ.'N') CHSIGN=-REG(24)
          IF(WQ.EQ.'KTEST') CHSIGN=-REG(25)
          IF(WQ.EQ.'LTEST') CHSIGN=-REG(26)
          IF(WQ.EQ.'MTEST') CHSIGN=-REG(27)
          IF(WQ.EQ.'NTEST') CHSIGN=-REG(28)
          RETURN
      END
C SUB DEGRAD.FOR
      FUNCTION DEGRA()
          IMPLICIT NONE
          REAL*8 PI,DEGRA,AAA
          INCLUDE 'datmai.inc'
          PI=PII
          IF (WC.EQ.'RTD') THEN
              AAA=(180.0D0/PI)
              REG(40)=REG(9)
              IF(WQ.EQ.'A') DEGRA=REG(1)*AAA
              IF(WQ.EQ.'B') DEGRA=REG(2)*AAA
              IF(WQ.EQ.'C') DEGRA=REG(3)*AAA
              IF(WQ.EQ.'D') DEGRA=REG(4)*AAA
              IF(WQ.EQ.'E') DEGRA=REG(5)*AAA
              IF(WQ.EQ.'F') DEGRA=REG(6)*AAA
              IF(WQ.EQ.'G') DEGRA=REG(7)*AAA
              IF(WQ.EQ.'H') DEGRA=REG(8)*AAA
              IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1         DEGRA=REG(9)*AAA
              IF(WQ.EQ.'Y') DEGRA=REG(10)*AAA
              IF(WQ.EQ.'Z') DEGRA=REG(11)*AAA
              IF(WQ.EQ.'T') DEGRA=REG(12)*AAA
              IF(WQ.EQ.'IX') DEGRA=REG(13)*AAA
              IF(WQ.EQ.'IY') DEGRA=REG(14)*AAA
              IF(WQ.EQ.'IZ') DEGRA=REG(15)*AAA
              IF(WQ.EQ.'IT') DEGRA=REG(16)*AAA
              IF(WQ.EQ.'I') DEGRA=REG(17)*AAA
              IF(WQ.EQ.'ITEST') DEGRA=REG(18)*AAA
              IF(WQ.EQ.'J') DEGRA=REG(19)*AAA
              IF(WQ.EQ.'JTEST') DEGRA=REG(20)*AAA
              IF(WQ.EQ.'K') DEGRA=REG(21)*AAA
              IF(WQ.EQ.'L') DEGRA=REG(22)*AAA
              IF(WQ.EQ.'M') DEGRA=REG(23)*AAA
              IF(WQ.EQ.'N') DEGRA=REG(24)*AAA
              IF(WQ.EQ.'KTEST') DEGRA=REG(25)*AAA
              IF(WQ.EQ.'LTEST') DEGRA=REG(26)*AAA
              IF(WQ.EQ.'MTEST') DEGRA=REG(27)*AAA
              IF(WQ.EQ.'NTEST') DEGRA=REG(28)*AAA

          ELSE IF (WC.EQ.'DTR') THEN
              AAA=(PI/180.0D0)
              REG(40)=REG(9)
              IF(WQ.EQ.'A') DEGRA=REG(1)*AAA
              IF(WQ.EQ.'B') DEGRA=REG(2)*AAA
              IF(WQ.EQ.'C') DEGRA=REG(3)*AAA
              IF(WQ.EQ.'D') DEGRA=REG(4)*AAA
              IF(WQ.EQ.'E') DEGRA=REG(5)*AAA
              IF(WQ.EQ.'F') DEGRA=REG(6)*AAA
              IF(WQ.EQ.'G') DEGRA=REG(7)*AAA
              IF(WQ.EQ.'H') DEGRA=REG(8)*AAA
              IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X') DEGRA=REG(9)*AAA
              IF(WQ.EQ.'Y') DEGRA=REG(10)*AAA
              IF(WQ.EQ.'Z') DEGRA=REG(11)*AAA
              IF(WQ.EQ.'T') DEGRA=REG(12)*AAA
              IF(WQ.EQ.'IX') DEGRA=REG(13)*AAA
              IF(WQ.EQ.'IY') DEGRA=REG(14)*AAA
              IF(WQ.EQ.'IZ') DEGRA=REG(15)*AAA
              IF(WQ.EQ.'IT') DEGRA=REG(16)*AAA
              IF(WQ.EQ.'I') DEGRA=REG(17)*AAA
              IF(WQ.EQ.'ITEST') DEGRA=REG(18)*AAA
              IF(WQ.EQ.'J') DEGRA=REG(19)*AAA
              IF(WQ.EQ.'JTEST') DEGRA=REG(20)*AAA
              IF(WQ.EQ.'K') DEGRA=REG(21)*AAA
              IF(WQ.EQ.'L') DEGRA=REG(22)*AAA
              IF(WQ.EQ.'M') DEGRA=REG(23)*AAA
              IF(WQ.EQ.'N') DEGRA=REG(24)*AAA
              IF(WQ.EQ.'KTEST') DEGRA=REG(25)*AAA
              IF(WQ.EQ.'LTEST') DEGRA=REG(26)*AAA
              IF(WQ.EQ.'MTEST') DEGRA=REG(27)*AAA
              IF(WQ.EQ.'NTEST') DEGRA=REG(28)*AAA

           ELSE
              ! it would be an error to get here
              DEGRA = 0.d0 ! make compiler happy (U.G.) 
          END IF
      END FUNCTION DEGRA
C SUB PI.FOR
      SUBROUTINE PI
          IMPLICIT NONE
          REAL*8 RWW1
          INCLUDE 'datmai.inc'
          RWW1=PII
          IF(WQ.EQ.'A') REG(1)=RWW1
          IF(WQ.EQ.'B') REG(2)=RWW1
          IF(WQ.EQ.'C') REG(3)=RWW1
          IF(WQ.EQ.'D') REG(4)=RWW1
          IF(WQ.EQ.'E') REG(5)=RWW1
          IF(WQ.EQ.'F') REG(6)=RWW1
          IF(WQ.EQ.'G') REG(7)=RWW1
          IF(WQ.EQ.'H') REG(8)=RWW1
          IF(WQ.EQ.'ACC'.OR.WQ.EQ.'        '.OR.WQ.EQ.'X') THEN
              REG(40)=REG(9)
              REG(9)=RWW1
          END IF
          IF(WQ.EQ.'Y') REG(10)=RWW1
          IF(WQ.EQ.'Z') REG(11)=RWW1
          IF(WQ.EQ.'T') REG(12)=RWW1
          IF(WQ.EQ.'IX') THEN
              REG(30)=REG(13)
              REG(13)=RWW1
          END IF
          IF(WQ.EQ.'IY') REG(14)=RWW1
          IF(WQ.EQ.'IZ') REG(15)=RWW1
          IF(WQ.EQ.'IT') REG(16)=RWW1
          IF(WQ.EQ.'I') REG(17)=RWW1
          IF(WQ.EQ.'ITEST') REG(18)=RWW1
          IF(WQ.EQ.'J') REG(19)=RWW1
          IF(WQ.EQ.'JTEST') REG(20)=RWW1
          IF(WQ.EQ.'K') REG(21)=RWW1
          IF(WQ.EQ.'L') REG(22)=RWW1
          IF(WQ.EQ.'M') REG(23)=RWW1
          IF(WQ.EQ.'N') REG(24)=RWW1
          IF(WQ.EQ.'KTEST') REG(25)=RWW1
          IF(WQ.EQ.'LTEST') REG(26)=RWW1
          IF(WQ.EQ.'MTEST') REG(27)=RWW1
          IF(WQ.EQ.'NTEST') REG(28)=RWW1
          RETURN
      END
C SUB ATANN.FOR
      FUNCTION ATANN()
          IMPLICIT NONE
          REAL*8 ATANN
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') ATANN=DATAN(REG(1))
          IF(WQ.EQ.'B') ATANN=DATAN(REG(2))
          IF(WQ.EQ.'C') ATANN=DATAN(REG(3))
          IF(WQ.EQ.'D') ATANN=DATAN(REG(4))
          IF(WQ.EQ.'E') ATANN=DATAN(REG(5))
          IF(WQ.EQ.'F') ATANN=DATAN(REG(6))
          IF(WQ.EQ.'G') ATANN=DATAN(REG(7))
          IF(WQ.EQ.'H') ATANN=DATAN(REG(8))
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    ATANN=DATAN(REG(9))
          IF(WQ.EQ.'Y') ATANN=DATAN(REG(10))
          IF(WQ.EQ.'Z') ATANN=DATAN(REG(11))
          IF(WQ.EQ.'T') ATANN=DATAN(REG(12))
          IF(WQ.EQ.'IX') ATANN=DATAN(REG(13))
          IF(WQ.EQ.'IY') ATANN=DATAN(REG(14))
          IF(WQ.EQ.'IZ') ATANN=DATAN(REG(15))
          IF(WQ.EQ.'IT') ATANN=DATAN(REG(16))
          IF(WQ.EQ.'I') ATANN=DATAN(REG(17))
          IF(WQ.EQ.'ITEST') ATANN=DATAN(REG(18))
          IF(WQ.EQ.'J') ATANN=DATAN(REG(19))
          IF(WQ.EQ.'JTEST') ATANN=DATAN(REG(20))
          IF(WQ.EQ.'K') ATANN=DATAN(REG(21))
          IF(WQ.EQ.'L') ATANN=DATAN(REG(22))
          IF(WQ.EQ.'M') ATANN=DATAN(REG(23))
          IF(WQ.EQ.'N') ATANN=DATAN(REG(24))
          IF(WQ.EQ.'KTEST') ATANN=DATAN(REG(25))
          IF(WQ.EQ.'LTEST') ATANN=DATAN(REG(26))
          IF(WQ.EQ.'MTEST') ATANN=DATAN(REG(27))
          IF(WQ.EQ.'NTEST') ATANN=DATAN(REG(28))
          RETURN
      END
C SUB ASINE.FOR
      FUNCTION ASINE()
          IMPLICIT NONE
          REAL*8 ASINE
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') ASINE=(REG(1))
          IF(WQ.EQ.'B') ASINE=(REG(2))
          IF(WQ.EQ.'C') ASINE=(REG(3))
          IF(WQ.EQ.'D') ASINE=(REG(4))
          IF(WQ.EQ.'E') ASINE=(REG(5))
          IF(WQ.EQ.'F') ASINE=(REG(6))
          IF(WQ.EQ.'G') ASINE=(REG(7))
          IF(WQ.EQ.'H') ASINE=(REG(8))
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    ASINE=(REG(9))
          IF(WQ.EQ.'Y') ASINE=(REG(10))
          IF(WQ.EQ.'Z') ASINE=(REG(11))
          IF(WQ.EQ.'T') ASINE=(REG(12))
          IF(WQ.EQ.'IX') ASINE=(REG(13))
          IF(WQ.EQ.'IY') ASINE=(REG(14))
          IF(WQ.EQ.'IZ') ASINE=(REG(15))
          IF(WQ.EQ.'IT') ASINE=(REG(16))
          IF(WQ.EQ.'I') ASINE=(REG(17))
          IF(WQ.EQ.'ITEST') ASINE=(REG(18))
          IF(WQ.EQ.'J') ASINE=(REG(19))
          IF(WQ.EQ.'JTEST') ASINE=(REG(20))
          IF(WQ.EQ.'K') ASINE=(REG(21))
          IF(WQ.EQ.'L') ASINE=(REG(22))
          IF(WQ.EQ.'M') ASINE=(REG(23))
          IF(WQ.EQ.'N') ASINE=(REG(24))
          IF(WQ.EQ.'KTEST') ASINE=(REG(25))
          IF(WQ.EQ.'LTEST') ASINE=(REG(26))
          IF(WQ.EQ.'MTEST') ASINE=(REG(27))
          IF(WQ.EQ.'NTEST') ASINE=(REG(28))
          IF(ASINE.LT.0.0D0) THEN
              OUTLYNE='ARG FOR "ASIN" IN REG '//WQ//' LESS THAN  ZERO'
              CALL SHOWIT(1)
              OUTLYNE='ACCULULATOR SET TO ZERO'
              CALL SHOWIT(1)
              ASINE=0.0D0
              CALL MACFAL
              RETURN
          END IF
          IF(ASINE.LT.0.0D0) THEN
              OUTLYNE='ARG FOR "ASIN" IN REG '//WQ//' GREATER THAN  ONE'
              CALL SHOWIT(1)
              OUTLYNE='ACCULULATOR SET TO ZERO'
              CALL SHOWIT(1)
              ASINE=0.0D0
              CALL MACFAL
              RETURN
          END IF
          ASINE=DASIN(ASINE)
          RETURN
      END
C SUB ACOSIN.FOR
      FUNCTION ACOSIN()
          IMPLICIT NONE
          REAL*8 ACOSIN
          INCLUDE 'datmai.inc'
          REG(40)=REG(9)
          IF(WQ.EQ.'A') ACOSIN=REG(1)
          IF(WQ.EQ.'B') ACOSIN=REG(2)
          IF(WQ.EQ.'C') ACOSIN=REG(3)
          IF(WQ.EQ.'D') ACOSIN=REG(4)
          IF(WQ.EQ.'E') ACOSIN=REG(5)
          IF(WQ.EQ.'F') ACOSIN=REG(6)
          IF(WQ.EQ.'G') ACOSIN=REG(7)
          IF(WQ.EQ.'H') ACOSIN=REG(8)
          IF(WQ.EQ.' '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1    ACOSIN=REG(9)
          IF(WQ.EQ.'Y') ACOSIN=REG(10)
          IF(WQ.EQ.'Z') ACOSIN=REG(11)
          IF(WQ.EQ.'T') ACOSIN=REG(12)
          IF(WQ.EQ.'IX') ACOSIN=REG(13)
          IF(WQ.EQ.'IY') ACOSIN=REG(14)
          IF(WQ.EQ.'IZ') ACOSIN=REG(15)
          IF(WQ.EQ.'IT') ACOSIN=REG(16)
          IF(WQ.EQ.'I') ACOSIN=REG(17)
          IF(WQ.EQ.'ITEST') ACOSIN=REG(18)
          IF(WQ.EQ.'J') ACOSIN=REG(19)
          IF(WQ.EQ.'JTEST') ACOSIN=REG(20)
          IF(WQ.EQ.'K') ACOSIN=REG(21)
          IF(WQ.EQ.'L') ACOSIN=REG(22)
          IF(WQ.EQ.'M') ACOSIN=REG(23)
          IF(WQ.EQ.'N') ACOSIN=REG(24)
          IF(WQ.EQ.'KTEST') ACOSIN=REG(25)
          IF(WQ.EQ.'LTEST') ACOSIN=REG(26)
          IF(WQ.EQ.'MTEST') ACOSIN=REG(27)
          IF(WQ.EQ.'NTEST') ACOSIN=REG(28)
          IF(ACOSIN.LT.0.0D0) THEN
              OUTLYNE='ARG FOR "ACOS" IN REG '//WQ//' LESS THAN  ZERO'
              CALL SHOWIT(1)
              OUTLYNE='ACCULULATOR SET TO ZERO'
              CALL SHOWIT(1)
              ACOSIN=0.0D0
              CALL MACFAL
              RETURN
          END IF
          IF(ACOSIN.LT.0.0D0) THEN
              OUTLYNE='ARG FOR "ACOS" IN REG '//WQ//' GREATER THAN  ONE'
              CALL SHOWIT(1)
              OUTLYNE='ACCULULATOR SET TO ZERO'
              CALL SHOWIT(1)
              ACOSIN=0.0D0
              CALL MACFAL
              RETURN
          END IF
          ACOSIN=DACOS(ACOSIN)
          RETURN
      END
C SUB FORMER.FOR
      SUBROUTINE FORMER
C
          IMPLICIT NONE
C
          INCLUDE 'datmai.inc'
C
          INTEGER I,LL
C
          IF(STI.EQ.1) THEN
              LL=0
              DO I=80,1,-1
                  IF(WFORM(I:I).NE.' ') THEN
                      LL=I
                      GO TO 986
                  END IF
              END DO
 986          CONTINUE
              WRITE(OUTLYNE,*) 'THE CURRENT WRITE FORMAT IS : ',WFORM(2:LL-1)
              CALL SHOWIT(0)
              RETURN
          END IF
          IF(SQ.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE= WC//' ONLY TAKES STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SST.EQ.0) THEN
              OUTLYNE= WC//' REQUIRES EXPLICIT STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          DO I=1,80
              WFORM(I:I)=' '
          END DO
          LL=0
          DO I=80,1,-1
              IF(WS(I:I).NE.' ') THEN
                  LL=I
                  GO TO 987
              END IF
          END DO
 987      CONTINUE
          WFORM(1:LL+2)='('//WS(1:LL)//')'
          RETURN
      END
C SUB REWIND.FOR
      SUBROUTINE REWIND
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO REWIND THE CARDTEXT.DAT FILE
C       UNIT = 8.
C
C       THE ONLY VALID QUALIFIER WORD IS:
C               CP = CARDTEXT.DATA = (UNIT=8)
C
          LOGICAL OPEN8,EXIST8,OPEN67,EXIST67
C
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"REWIND" ONLY TAKES QUALIFIER INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(SQ.EQ.0) WQ='FOE.DAT'
          IF(WQ.NE.'CP'.AND.WQ.NE.'FOE') THEN
              OUTLYNE='ONLY CP (CARDTEXT DATA) AND FOE.DAT CAN BE REWOUND.'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'CP') THEN
C       WQ MUST BE 'CP'
              EXIST8=.FALSE.
              OPEN8=.FALSE.
              INQUIRE(FILE=trim(HOME)//'CARDTEXT.DAT',EXIST=EXIST8)
              INQUIRE(FILE=trim(HOME)//'CARDTEXT.DAT',OPENED=OPEN8)
              IF(EXIST8.OR.OPEN8) THEN
                  REWIND(UNIT=8)
              ELSE
                  IF(APPEND) OPEN(UNIT=8,ACCESS='APPEND',BLANK='NULL'
     1            ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2            ,STATUS='UNKNOWN')
                  IF(.NOT.APPEND) OPEN(UNIT=8,ACCESS='SEQUENTIAL',BLANK='NULL'
     1            ,FORM='FORMATTED',FILE=trim(HOME)//'CARDTEXT.DAT'
     2            ,STATUS='UNKNOWN')
              END IF
          END IF
          IF(WQ.EQ.'FOE') THEN
C       WQ MUST BE 'FOE'
              EXIST8=.FALSE.
              OPEN8=.FALSE.
              INQUIRE(FILE=trim(HOME)//'FOE.DAT',EXIST=EXIST67)
              INQUIRE(FILE=trim(HOME)//'FOE.DAT',OPENED=OPEN67)
              IF(EXIST8.OR.OPEN67) THEN
                  REWIND(UNIT=67)
              ELSE
                  IF(APPEND) OPEN(UNIT=67,ACCESS='APPEND',BLANK='NULL'
     1            ,FORM='FORMATTED',FILE=trim(HOME)//'FOE.DAT'
     2            ,STATUS='UNKNOWN')
                  IF(.NOT.APPEND) OPEN(UNIT=67,ACCESS='SEQUENTIAL',BLANK='NULL'
     1            ,FORM='FORMATTED',FILE=trim(HOME)//'FOE.DAT'
     2            ,STATUS='UNKNOWN')
              END IF
          END IF
          RETURN
      END
C SUB ALLDEF.FOR
      SUBROUTINE ALLDEF
C
          IMPLICIT NONE
C
C     THIS DOES THE "ALL" COMMAND
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(STI.EQ.1) THEN
              IF(ALLSET) OUTLYNE='"ALL" IS SET TO "ON"'
              IF(.NOT.ALLSET) OUTLYNE='"ALL" IS SET TO "OFF"'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"ALL" TAKES NO NUMERIC OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
              OUTLYNE='THE ONLY VALID QUALIFIERS ARE "ON" AND "OFF"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON') ALLSET=.TRUE.
          IF(WQ.EQ.'OFF') ALLSET=.FALSE.
          RETURN
      END
C SUB SETCOAT.FOR
      SUBROUTINE SETCOAT
C
          IMPLICIT NONE
C
C     THIS DOES THE "COATINGS" COMMAND
C
          INCLUDE 'datmai.inc'
          INCLUDE 'datlen.inc'
C
C       CHECK FOR STRING INPUT
          IF(STI.EQ.1) THEN
              IF(COATSET) OUTLYNE='"COATINGS" IS SET TO "ON"'
              IF(.NOT.COATSET) OUTLYNE='"COATINGS" IS SET TO "OFF"'
              CALL SHOWIT(1)
              RETURN
          END IF
          IF(SST.EQ.1.OR.SN.EQ.1) THEN
              OUTLYNE='"COATINGS" TAKES NO NUMERIC OR STRING INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
              OUTLYNE='THE ONLY VALID QUALIFIERS ARE "ON" AND "OFF"'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(WQ.EQ.'ON') COATSET=.TRUE.
          IF(WQ.EQ.'OFF') COATSET=.FALSE.
          RETURN
      END
C SUB ATANN2.FOR
      SUBROUTINE ATANN2
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO CALCULATE THE DATAN2 FUNCTION
C       USING THE ARGUMENT W1 AND W2. IF W2 = 0.0D0 THEN ATANN2
C       RETURNS THE VALUE PI/2.
C
C       THE RESULT IS LEFT IN THE ACCUMULATOR.
C       IN GENERAL THE RESULT IS RETURNED AS AN ANGULAR
C       VALUE IN RADIANS BETWEEN -PI/2 AND +PI/2.
C
          INCLUDE 'datmai.inc'
C
          IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
              OUTLYNE='"ATANN2" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
          IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
              OUTLYNE=
     1        '"ATANN2" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              RETURN
          END IF
C
          IF(W2.EQ.0.0D0) THEN
              REG(40)=REG(9)
              REG(9)=PII/2.0D0
          ELSE
          END IF
          REG(40)=REG(9)
          IF(DABS(W1).GE.DABS(((1.0D35)*W2))) THEN
              IF(W1.GE.0.0D0) REG(9)=PII/2.0D0
              IF(W1.LT.0.0D0) REG(9)=(3.0D0*PII)/2.0D0
          ELSE
              IF(DABS(W1).LE.1.0D-15.AND.DABS(W2).LE.1.0D-15) THEN
                  REG(9)=0.0D0
              ELSE
                  REG(9)=DATAN2(W1,W2)
              END IF
              IF(REG(9).LT.0.0D0) REG(9)=REG(9)+(TWOPII)
          END IF
          RETURN
      END
C SUB WRITE.FOR
      SUBROUTINE WRITE
C
          IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO WRITE OUT THE CONTENTS
C       OF A NAMED STORAGE REGISTER OR THE ACCUMULATOR.
C       OUTPUT CAN BE LABELD OR NOT LABELD. THE LABEL IS
C       TYPED IN AS AN ALPHANUMERIC STRING.
C
          CHARACTER ACCWRD*8,CSTRING*23
C
          REAL*8 RGVAL
C
          INTEGER ACCSUB,ACCCNT,N,i
C
          COMMON/ACCSB/ACCWRD
C
          COMMON/ACCSB2/ACCSUB,ACCCNT
C
          INCLUDE 'datmai.inc'
C
C               TEST TO SEE IF LABEL IS BLANK. IF YES, PRINT
C               REGISTER NAME FOLLOWED BY EQUAL SIGN IF
C               OUTPUT DEVICE IS THE SCREEN (OUT=6).
C               IF OUT NOT EQUAL TO 6 THEN JUST PRINT
C               THE REGISTER VALUE. IF LABEL IS NOT BLANK
C               PRINT FIRST 40 CHARACTERS IN THE
C               STRING FOLLOWED BY AN EQUAL SIGN.
C
          IF(STI.EQ.1) THEN
              OUTLYNE= 'NO ADDITIONAL INFORMATION AVAILABLE'
              CALL SHOWIT(1)
              RETURN
          END IF
C
          IF(WQ.NE.'A'.AND.WQ.NE.'B'.AND.WQ.NE.'C'
     1    .AND.WQ.NE.'D'.AND.WQ.NE.'E'.AND.WQ.NE.'F'
     2    .AND.WQ.NE.'G'.AND.WQ.NE.'H'.AND.WQ.NE.'ACC'
     3    .AND.WQ.NE.'        '.AND.WQ.NE.'ALL'.AND.
     4    WQ.NE.'X'.AND.WQ.NE.'Y'.AND.WQ.NE.'Z'.AND.
     5    WQ.NE.'T'.AND.WQ.NE.'IX'.AND.WQ.NE.'IY'
     6    .AND.WQ.NE.'IZ'.AND.WQ.NE.'IT'.AND.WQ.NE.
     7    'I'.AND.WQ.NE.'ITEST'.AND.WQ.NE.'J'.AND.
     8    WQ.NE.'JTEST'.AND.WQ.NE.'K'.AND.WQ.NE.'L'.AND.WQ.NE.'M'
     9    .AND.WQ.NE.'N'.AND.WQ.NE.'KTEST'.AND.WQ.NE.'LTEST'.AND.WQ.NE.
     1    'MTEST'.AND.WQ.NE.'NTEST') THEN
              OUTLYNE='INVALID REGISTER NAME'
              CALL SHOWIT(1)
              OUTLYNE='RE-ENTER COMMAND'
              CALL SHOWIT(1)
              CALL MACFAL
              GO TO 20
          END IF
          IF(ACCSUB.EQ.1) THEN
              IF(WQ.EQ.'ACC'.OR.WQ.EQ.'X'.OR.WQ.EQ.' ') THEN
                  WQ=ACCWRD
                  ACCCNT=ACCCNT-1
                  IF(ACCCNT.EQ.0) ACCSUB=0
              END IF
          END IF
 10       IF(SST.EQ.0) THEN
              IF(WQ.EQ.'A') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(1)
                      WS='A ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(1)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'B') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(2)
                      WS='B ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(2)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'C') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(3)
                      WS='C ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(3)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'D') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(4)
                      WS='D ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(4)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'E') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(5)
                      WS='E ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(5)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'F') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(6)
                      WS='F ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(6)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'G') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(7)
                      WS='G ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(7)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'H') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(8)
                      WS='H ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(8)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'        '.OR.WQ.EQ.'ACC'
     1        .OR.WQ.EQ.'X') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(9)
                      WS='X ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(9)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'Y') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(10)
                      WS='Y ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(10)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'Z') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(11)
                      WS='Z ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(11)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'T') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(12)
                      WS='T ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(12)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'IX') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(13)
                      WS='IX ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(13)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'IY') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(14)
                      WS='IY ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(14)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'IZ') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(15)
                      WS='IZ ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(15)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'IT') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(16)
                      WS='IT ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(16)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'I') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(17)
                      WS='I ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(17)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'ITEST') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(18)
                      WS='ITEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(18)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'J') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(19)
                      WS='J ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(19)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'JTEST') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(20)
                      WS='JTEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(20)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'LASTX') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(40)
                      WS='LASTX ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(40)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'LASTIX') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(30)
                      WS='LASTIX ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(30)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'K') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(21)
                      WS='K ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(21)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'L') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(22)
                      WS='L ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(22)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'M') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(23)
                      WS='M ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(23)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'N') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(24)
                      WS='N ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(24)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'KTEST') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(25)
                      WS='KTEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(25)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'LTEST') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(26)
                      WS='LTEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(26)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'MTEST') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(27)
                      WS='MTEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(27)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'NTEST') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(28)
                      WS='NTEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      WRITE(OUTLYNE,1001) REG(28)
                      CALL SHOWIT(0)
                  END IF
              END IF
              IF(WQ.EQ.'ALL'.OR.WC.EQ.'PRIREG') THEN
                  IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(1)
                      WS='A ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(2)
                      WS='B ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(3)
                      WS='C ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(4)
                      WS='D ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(5)
                      WS='E ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(6)
                      WS='F ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(7)
                      WS='G ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(8)
                      WS='H ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
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
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(17)
                      WS='I ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(18)
                      WS='ITEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(19)
                      WS='J ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(20)
                      WS='JTEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(21)
                      WS='K ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(25)
                      WS='KTEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(22)
                      WS='L ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(26)
                      WS='LTEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(23)
                      WS='M ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(27)
                      WS='MTEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(24)
                      WS='N ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(28)
                      WS='NTEST ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(40)
                      WS='LASTX ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(30)
                      WS='LASTIX ='//' '//CSTRING(1:23)
                      WRITE(OUTLYNE,1100) WS
                      CALL SHOWIT(0)
                  ELSE
                      OUTLYNE=' WRITE ALL SUPPORTED FOR SCREEN AND PRINTER ONLY'
                      CALL SHOWIT(1)
                      CALL MACFAL
                  END IF
              END IF
C
          ELSE
C
C               THE LABEL IS NOT BLANK
C               IF THE LABEL IS NOT BLANK BUT OUT IS NOT
C               EQUAL TO 6 OR 7, THEN SET THE LABEL TO BLANK
C               AND GO TO 10 AND REPROCESS.
              IF(OUT.NE.6.AND.OUT.NE.7) THEN
                  SST=0
                  GO TO 10
              END IF
C
              IF(WS(1:1).EQ.':') WS(1:80)=WS(2:80)
              DO I=40,1,-1
                  IF(WS(I:I).NE.' ') THEN
                      N=I
                      GO TO 987
                  END IF
              END DO
 987          WS(1:(N+2))=WS(1:N)//' ='
              N=N+2
              IF(WQ.EQ.'A') RGVAL=REG(1)
              IF(WQ.EQ.'B') RGVAL=REG(2)
              IF(WQ.EQ.'C') RGVAL=REG(3)
              IF(WQ.EQ.'D') RGVAL=REG(4)
              IF(WQ.EQ.'E') RGVAL=REG(5)
              IF(WQ.EQ.'F') RGVAL=REG(6)
              IF(WQ.EQ.'G') RGVAL=REG(7)
              IF(WQ.EQ.'H') RGVAL=REG(8)
              IF(WQ.EQ.'        '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1        RGVAL=REG(9)
              IF(WQ.EQ.'Y') RGVAL=REG(10)
              IF(WQ.EQ.'Z') RGVAL=REG(11)
              IF(WQ.EQ.'T') RGVAL=REG(12)
              IF(WQ.EQ.'IX') RGVAL=REG(13)
              IF(WQ.EQ.'IY') RGVAL=REG(14)
              IF(WQ.EQ.'IZ') RGVAL=REG(15)
              IF(WQ.EQ.'IT') RGVAL=REG(16)
              IF(WQ.EQ.'I') RGVAL=REG(17)
              IF(WQ.EQ.'ITEST') RGVAL=REG(18)
              IF(WQ.EQ.'J') RGVAL=REG(19)
              IF(WQ.EQ.'JTEST') RGVAL=REG(20)
              IF(WQ.EQ.'K') RGVAL=REG(21)
              IF(WQ.EQ.'L') RGVAL=REG(22)
              IF(WQ.EQ.'M') RGVAL=REG(23)
              IF(WQ.EQ.'N') RGVAL=REG(24)
              IF(WQ.EQ.'KTEST') RGVAL=REG(25)
              IF(WQ.EQ.'LTEST') RGVAL=REG(26)
              IF(WQ.EQ.'MTEST') RGVAL=REG(27)
              IF(WQ.EQ.'NTEST') RGVAL=REG(28)
              IF(WQ.EQ.'LASTX') RGVAL=REG(40)
              IF(WQ.EQ.'LASTIX') RGVAL=REG(30)
              IF(WQ.EQ.'ALL'.OR.WC.EQ.'PRIREG')THEN
                  OUTLYNE='WRITE ALL NOT FUNCTIONAL WITH LABEL STRING'
                  CALL SHOWIT(1)
                  CALL MACFAL
              END IF
              WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) RGVAL
              WS=WS(1:N)//' '//CSTRING(1:23)
              WRITE(OUTLYNE,1100) WS
              CALL SHOWIT(0)
          END IF
 20       CONTINUE
C
C       THE FOLLOWING ARE THE WRITE FORMAT STATEMENTS
C
 1001     FORMAT(D23.15)
 1100     FORMAT(A79)
          RETURN
   69     CONTINUE
          OUTLYNE=
     1    'INVALID FORMAT SPECIFICATION EXISTS'
          CALL SHOWIT(1)
          OUTLYNE=
     1    'RE-ISSUE THE "FORMAT" COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
      END
