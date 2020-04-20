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

C       USER FILES

      SUBROUTINE USERSURF(I,X,Y,Z,UERROR)
C
          IMPLICIT NONE
          REAL*8 RHO,RHO2,C,C2,X,Y,Z,KAPPA,DEN
          INTEGER I
          LOGICAL UERROR
C     THIS IS THE USER-DEFINED SUBROUTINE SURFACE
C     GIVEN SOME X AND Y, THIS ROUTINE RETURNS Z. IT CAN USE SPECIAL SURFACE
C     COEFS, GENERAL PURPOSE STORAGE REGISTERS AND AND OTHER DATA OR
C     INCLUDED SUBROUTINES. THE MAIN PROGRAM DOES ALL NECESSARY ITERATIVE
C     SURFACE INTERSECTION AND SURFACE NORMAL CALCULATIONS FOR THE USER
C     AND DOES THEM RIGOROUSLY AND CORRECTLY UNLIKE THE BAD ROUTINES
C     USED IN CODE-V WHICH ARE LESS THAN WORTHLESS!
C
C     THE APPROPRIATE Z WHICH IT PASSES BACK TO THE CALLING ROUTINE.
C
C     THIS Z VALUE IS ADDED TO ANY EXISTING Z COMPUTED FOR THINGS LIKE
C     CURVATURE AND ASPHERIC DEFORMATIONS IN THE REGULAR LENS DATABASE.
C
          INCLUDE 'datlen.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datsub.inc'
          INCLUDE 'datmai.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'dathgr.inc'
          UERROR=.FALSE.
C
          RHO2=((X**2)+(Y**2))
          RHO=DSQRT(RHO2)
          C2=C**2
          C=FTFL01(1,I)+(FTFL01(2,I)*RHO)+
     1    (FTFL01(3,I)*(RHO**2))+(FTFL01(4,I)*(RHO**3))+
     1    (FTFL01(5,I)*(RHO**4))+(FTFL01(6,I)*(RHO**5))+
     1    (FTFL01(7,I)*(RHO**6))+(FTFL01(8,I)*(RHO**7))+
     1    (FTFL01(9,I)*(RHO**8))+(FTFL01(10,I)*(RHO**9))+
     1    (FTFL01(11,I)*(RHO**10))+(FTFL01(12,I)*(RHO**11))+
     1    (FTFL01(13,I)*(RHO**12))+(FTFL01(14,I)*(RHO**13))+
     1    (FTFL01(15,I)*(RHO**14))+(FTFL01(16,I)*(RHO**15))+
     1    (FTFL01(17,I)*(RHO**16))+(FTFL01(18,I)*(RHO**17))+
     1    (FTFL01(19,I)*(RHO**18))+(FTFL01(20,I)*(RHO**19))+
     1    (FTFL01(21,I)*(RHO**20))
          KAPPA=FTFL01(31,R_I)+(FTFL01(32,I)*RHO)+
     1    (FTFL01(33,I)*(RHO**2))+(FTFL01(34,I)*(RHO**3))+
     1    (FTFL01(35,I)*(RHO**4))+(FTFL01(36,I)*(RHO**5))+
     1    (FTFL01(37,I)*(RHO**6))+(FTFL01(38,I)*(RHO**7))+
     1    (FTFL01(39,I)*(RHO**8))+(FTFL01(40,I)*(RHO**9))+
     1    (FTFL01(41,I)*(RHO**10))+(FTFL01(42,I)*(RHO**11))+
     1    (FTFL01(43,I)*(RHO**12))+(FTFL01(44,I)*(RHO**13))+
     1    (FTFL01(45,I)*(RHO**14))+(FTFL01(46,I)*(RHO**15))+
     1    (FTFL01(47,I)*(RHO**16))+(FTFL01(48,I)*(RHO**17))+
     1    (FTFL01(49,I)*(RHO**18))+(FTFL01(50,I)*(RHO**19))+
     1    (FTFL01(51,I)*(RHO**20))
          DEN=(1.0D0-((KAPPA+1.0D0)*C2*RHO2))
          UERROR=.FALSE.
          IF(DEN.LT.0.0D0) THEN
              UERROR=.TRUE.
              Z=0.0D0
              RETURN
          END IF
          Z=(C*RHO2)/(1+DSQRT(DEN))
          RETURN
      END
      SUBROUTINE CALL_USERFUNC
          USE GLOBALS
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'dathgr.inc'
          INCLUDE 'datpts.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsub.inc'
          REAL*8 X,Y,Z,T
          INTEGER I
          X=W1
          Y=W2
          Z=W3
          T=W4
          I=INT(W5)
          CALL USERFUNC(X,Y,Z,T,I)
          RETURN
      END
C
      SUBROUTINE USERFUNC(X,Y,Z,T,I)
          USE GLOBALS
          IMPLICIT NONE
          REAL*8 X,Y,Z,T,RESULT,C,K,R
          INTEGER I
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'dathgr.inc'
          INCLUDE 'datpts.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsub.inc'
C     CASE T=1
          IF(INT(T).EQ.1) THEN
C     IF T = 1, WE HAVE AN OPTICAL SURFACE WITH POLYNOMIALS PASSED
C     IN NUMERICAL STORAGE REGISTERS STARTING AT 101
C     THE RESULT RETURNED IS F(r)-z, where r=sqrt((x**2)+(y**2))
C     REG 101 PASSES CURVATURE
C     REG 102 PASSES CONIC
C     REG 103 PASSES COEF FOR 1/R**6
C     REG 104 PASSES COEF FOR 1/R**4
C     REG 105 PASSES COEF FOR 1/R**2
C     REG 106 PASSES COEF FOR CONSTANT TERM
C     REG 107 PASSES COEF FOR R**2
C     REG 108 PASSES COEF FOR R**4
C     REG 109 PASSES COEF FOR R**6
C     REG 110 PASSES COEF FOR R**8
C     REG 111 PASSES COEF FOR R**10
C     REG 112 PASSES COEF FOR R**12
C     REG 113 PASSES COEF FOR R**14
C     REG 114 PASSES COEF FOR R**16
C     REG 115 PASSES COEF FOR R**18
C     REG 116 PASSES COEF FOR R**20
              C=GPREG(101)
              K=GPREG(102)
              R=DSQRT((X**2)+(Y**2))
              RESULT=-Z+
     1        ((C*(R**2))/(1.0D0+DSQRT(1.0D0-((K+1.0D0)*(C**2)*(R**2)))))
              IF(R.NE.0.0D0) THEN
                  RESULT=RESULT
     1            +(GPREG(103)/(R**6))
     2            +(GPREG(104)/(R**4))
     3            +(GPREG(105)/(R**2))
              END IF
              RESULT=RESULT
     1        +GPREG(106)
     2        +(GPREG(107)*(R**2))
     2        +(GPREG(108)*(R**4))
     2        +(GPREG(109)*(R**6))
     2        +(GPREG(110)*(R**8))
     2        +(GPREG(111)*(R**10))
     2        +(GPREG(112)*(R**12))
     2        +(GPREG(113)*(R**14))
     2        +(GPREG(114)*(R**16))
     2        +(GPREG(115)*(R**18))
     2        +(GPREG(116)*(R**20))
              IF(I.EQ.0) THEN
C     PASS RESULT TO ACCUMULATOR WITH NO STACK PUSH
                  REG(9)=RESULT
              ELSE
C     PASS TO GP REGISTER
                  GPREG(I)=RESULT
              END IF
              RETURN
          END IF
          RESULT=PII
          IF(I.EQ.0) THEN
C     PASS RESULT TO ACCUMULATOR WITH NO STACK PUSH
              REG(9)=RESULT
          ELSE
C     PASS TO GP REGISTER
              GPREG(I)=RESULT
          END IF
      END
C
      SUBROUTINE CALL_USERSUBR
          USE GLOBALS
          IMPLICIT NONE
          INCLUDE 'datmai.inc'
          INCLUDE 'datmac.inc'
          INCLUDE 'datcfg.inc'
          INCLUDE 'dathgr.inc'
          INCLUDE 'datpts.inc'
          INCLUDE 'datsp1.inc'
          INCLUDE 'datspd.inc'
          INCLUDE 'datsub.inc'
          WRITE(OUTLYNE,*) 'RUNNING THE USERSUBR.FOR SUBROUTINE'
          CALL SHOWIT(1)
          RETURN
c
      END
